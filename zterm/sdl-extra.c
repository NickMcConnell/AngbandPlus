/*
 *
 * This file contains routines for maiming bitmaps as well as other
 * supplemental routines, all for SDL.
 *
 * Copyright 2001 Gregory Velichansky (hmaon@bumba.net)
 * You may use it under the terms of the standard Angband license (below) or
 * the GNU General Public License (GPL) Version 2 or greater. (see below)
 *
 * The Angband license states:
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * The GNU GPL notice:
   main-sdl.c - SDL (http://libsdl.org) Term implementation for Angband.
	Copyright (C) 2001  Gregory Velichansky (hmaon@bumba.net)

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, please see 
	http://www.gnu.org/copyleft/gpl.html


	Please see the file COPYING for more detail regarding Angband's current
	license situation.
*/


#include "angband.h"

#ifdef USE_SDL


#include "SDL.h"
#ifdef ALLOW_TTF
#include "SDL_ttf.h"
#endif /* allow_ttf */
#include <string.h>
#include <math.h> /* for scaling blits */
#ifndef WIN32
#include <bits/nan.h>
#endif
#include "langband.h"


/*
 *
 * Supplemental SDL bitmap manipulation functions.
 *
 * These could be moved to a separate file. In mai?-x11.c, similar routines
 * are separate from the main display module implementation.
 *
 */


/* The most pedantic-a%& getpixel and putpixel ever, hopefully. */
/* There may still be endianness bugs! These will be fixed after adequte testing. XXX XXX XXX */
inline errr SDL_GetPixel (SDL_Surface *f, Uint32 x, Uint32 y, Uint8 *r, Uint8 *g, Uint8 *b)
{
	/*const Uint32 mask[] = {0x0, 0xff, 0xffff, 0xffffff, 0xffffffff};*/
	Uint32 pixel;

	Uint8 *pp;

	int n; /* general purpose 'n'. */

	if (f == NULL) return -1;

	pp = (Uint8 *) f->pixels;

	if (x >= f->w || y >= f->h) return -1;

	pp += (f->pitch * y);

	pp += (x * f->format->BytesPerPixel);

	/* we do not lock the surface here, it would be inefficient XXX */
	/* this reads the pixel as though it was a big-endian integer XXX */
	/* I'm trying to avoid reading part the end of the pixel data by
	 * using a data-type that's larger than the pixels */
	for (n = 0, pixel = 0; n < f->format->BytesPerPixel; ++n, ++pp)
	{
#if SDL_BYTEORDER == SDL_LIL_ENDIAN
		pixel >>= 8;
		pixel |= *pp << (f->format->BitsPerPixel - 8);
#else
		pixel |= *pp;
		pixel <<= 8;
#endif
	}

	SDL_GetRGB(pixel, f->format, r, g, b);
	return 0;
}

/* This function looks remarkably similar to the one above. Yes, it's cut
 * and paste. */
inline errr SDL_PutPixel (SDL_Surface *f, Uint32 x, Uint32 y, Uint8 r, Uint8 g, Uint8 b)
{
	Uint32 pixel;

	Uint8 *pp;

	int n; 

	if (f == NULL) return -1;

	pp = (Uint8 *) f->pixels;

	if (x >= f->w || y >= f->h) return -1;

	pp += (f->pitch * y);

	pp += (x * f->format->BytesPerPixel);

	pixel = SDL_MapRGB(f->format, r, g, b);

	for (n = 0; n < f->format->BytesPerPixel; ++n, ++pp)
	{
		*pp = (Uint8) (pixel & 0xFF);
		pixel >>= 8;
	}

	return 0;
}


/* This routine performs a scaling blit. It will shrink and magnify. :) */
/* It uses floating point arithmetic (because I am lazy) so it's not too fast
 * but I only intend for it to be used in pre-processing, that is image 
 * processing at load time. It's fast enough for that, at least.
 * Actually on my machine it IS fast enough to scale fonts and bitmaps
 * in real-time. :) 
 * This routine uses a weighted average, the weight being based on overlapping
 * pixel area.
 */
inline errr SDL_ScaleBlit(SDL_Surface *src, SDL_Rect *sr, SDL_Surface *dst, SDL_Rect *dr)
{
	Uint8 r, g, b;

	float rs, gs, bs; /* sums */

	float area;

	float sx, sy; /* current source x and y */
	float dsx, dsy; /* source increment, per increment of 1 in destination */
	
	float wsx, wsy; 
	/* width of source box. Equal to dsx,dsy except when either of then are
	 * smaller than 1. This is a hack for smoother magnification. XXX */


	float x, y; /* x and y in sub-area */

	Uint32 tx, ty; /* "to" x and y */
	Uint32 lx, ly;

	float w, e, n, s; /* some temporary variables, named after orientations */


	if (src == NULL || sr == NULL || dst == NULL || dr == NULL) return -1;

	/* these are meaningless and would cause a divide by zero: */
	if (!dr->w || !dr->h) return -1; 

	wsx = dsx = ((float) sr->w) / dr->w;
	if (wsx < 1) wsx = 1;
	wsy = dsy = ((float) sr->h) / dr->h;
	if (wsy < 1) wsy = 1;

	lx = dr->x + dr->w; 
	ly = dr->y + dr->h;

	area = wsx * wsy;



	for (ty = dr->y, sy = (float)sr->y; ty < ly; ++ty, sy+=dsy)
	{
		for (tx = dr->x, sx = (float)sr->x; tx < lx; ++tx, sx+=dsx)
		{
			rs = gs = bs = 0.0;
			for (y = floor(sy) - 1; ceil(sy + wsy) > y; ++y)
			{
				for (x = floor(sx) - 1; ceil(sx + wsx) > x; ++x)
				{
					w = (x > sx) ? 0 : sx - x;
					n = (y > sy) ? 0 : sy - y;

					e = (sx+wsx >= x+1) ? 1 : sx+wsx - x;
					s = (sy+wsy >= y+1) ? 1 : sy+wsy - y;

					if (w > e || s < n ) continue;

#define gsx ((Uint32)x >= sr->x+sr->w ? sr->x+sr->w-1 : (Uint32)x)
#define gsy ((Uint32)y >= sr->y+sr->h ? sr->y+sr->h-1 : (Uint32)y)
					SDL_GetPixel (src, gsx, gsy, &r, &g, &b);



					rs += (e - w) * (s - n) * (float)r;
					gs += (e - w) * (s - n) * (float)g;
					bs += (e - w) * (s - n) * (float)b;
				}
			}
			rs /= area;
			gs /= area;
			bs /= area;

			if (rs >= 256.0 || gs >= 256.0 || bs > 256.0) 
			{
				ERRORMSG("ScaleBlit: weighted average error, values: %f, %f, %f\n", rs, gs, bs);
				/**((char *)0) = 0;*/
			}
			if (rs > 255.0) rs = 255.0;
			if (gs > 255.0) gs = 255.0;
			if (bs > 255.0) bs = 255.0;

			r = (Uint8)rs;
			g = (Uint8)gs;
			b = (Uint8)bs;

			SDL_PutPixel (dst, tx, ty, r, g, b);
		}
	}

	return 0;
#undef gsx
#undef gsy
}


/* Integer math version of SDL_ScaleBlit().
 * Where necessary, a number uses the 16 high bits for the integer
 * and the 16 low bits for the decimal portion.
 *
 * eg:
 * float a = (float) (b >> 16) + (b & 0xFFFF)/65536.0;
 */

inline Uint32 ifloor(Uint32 i)
{
	return i & 0xFFFF0000;
}

inline Uint32 iceil(Uint32 i)
{
	return (i & 0xFFFF) ? i : ifloor(i) + (1<<16);
}


errr SDL_FastScaleBlit(SDL_Surface *src, SDL_Rect *sr, SDL_Surface *dst, SDL_Rect *dr)
{
	Uint8 r, g, b;
	Uint32 rs, gs, bs; /* sums. */

	/* temp storage for large int multiplies. Uint64 doen't exist anywhere */
	double farea; 
	Uint32 area;

	Uint32 sx, sy;
	Uint32 dsx, dsy;

	Uint32 wsx, wsy;

	Uint32 x, y; /* x and y, for sub-area */

	Uint32 tx, ty; /* normal integers */
	Uint32 lx, ly; /* normal integers */

	Uint32 w, e, n, s; /* temp variables, named after compass directions */

	if (src == NULL || sr == NULL || dst == NULL || dr == NULL) return -1;

	if (!dr->w || !dr->h) return -1;


	/* TODO FIXME check for possible overflows! */

	wsx = dsx = (sr->w << 16) / dr->w;
	if (!(wsx & 0xFFFF0000)) wsx = 1 << 16;
	wsy = dsy = (sr->h << 16) / dr->h;
	if (!(wsy & 0xFFFF0000)) wsy = 1 << 16;

	lx = dr->x + dr->w;
	ly = dr->y + dr->h;

	/* lazy multiplication. Hey, it's only once per blit. :P */
	farea = ((double)wsx) * ((double)wsy);
	farea /= (double)(1 << 16);
	area = (Uint32) farea;

	/* For optimization, those setup routines should be moved into
	 * SDL_ScaleTiledBitmap() for that function.
	 */

	for (ty = dr->y, sy = sr->y << 16; ty < ly; ++ty, sy+=dsy)
	{
		for (tx = dr->x, sx = sr->x << 16; tx < lx; ++tx, sx+=dsx)
		{
			rs = gs = bs = 0;
			for (y = ifloor(sy); iceil(sy + wsy) > y; y += (1<<16))
			{
				for (x = ifloor(sx); iceil(sx + wsx) > x; x += (1<<16))
				{
					w = (x > sx) ? 0 : sx - x;
					n = (y > sy) ? 0 : sy - y;

					e = (sx+wsx >= x+(1<<16)) ? 1<<16 : sx+wsx - x;
					s = (sy+wsy >= y+(1<<16)) ? 1<<16 : sy+wsy - y;

					if (w > e || s < n) continue;

#define gsx ((x >> 16) >= sr->x+sr->w ? sr->x+sr->w-1 : x >> 16)
#define gsy ((y >> 16) >= sr->y+sr->h ? sr->y+sr->h-1 : y >> 16)

					SDL_GetPixel (src, gsx, gsy, &r, &g, &b);

					rs += ((e - w)>>8) * ((s - n)>>8) * r;
					gs += ((e - w)>>8) * ((s - n)>>8) * g;
					bs += ((e - w)>>8) * ((s - n)>>8) * b;
				}
			}
			rs /= area;
			gs /= area;
			bs /= area;

			if (rs >= 256 || gs >= 256 || bs >= 256)
			{
			    ERRORMSG("fixed point weighted average overflow!, values: %d, %d, %d\n", rs, gs, bs);
			}

			r = (Uint8) rs;
			g = (Uint8) gs;
			b = (Uint8) bs;

			SDL_PutPixel (dst, tx, ty, r, g, b);
		}
	}

	return 0;
#undef gsx
#undef gsy
}



#if 0 /* the procedure above is a more generalized version of the one below */

/* The following is a function to perform a Blit while magnifying */
/* Anti-aliasing is performed. :) */
/* It is probably very SLOW on most systems. Use it for pre-processing. XXX */
/* a Blit while shrinking is handled by a different function */
errr SDL_StretchBlit(SDL_Surface *src, SDL_Rect *sr, SDL_Surface *dst, SDL_Rect *dr)
{
	double sx, sy; /* current source x and y */
	Uint32 isx, isy; /* temp. values for convenience in calculation code */
	double dsx, dsy; /* source increment, per increment of 1 in destination */
	double wx, wy; /* temp. weight values for the color mixing calculations */
	double weight; /* temp. weight of pixel */

	/* coordinates to get pixels from: ... */
#undef gsx
#define gsx (isx >= sr->x+sr->w ? sr->x+sr->w-1 : isx)
#undef gsy
#define gsy (isy >= sr->y+sr->h ? sr->y+sr->h-1 : isy)


	Uint32 tx, ty; /* "to" x and y. "dx, dy" would be too confusing. */
	Uint32 lx, ly; /* end x and y in destination, not inclusive */

	double r, g, b; /* temporary values on which we perform calculations */
	/*double s;*/ /* scale factor calculation thing. gross hack. don't ask. */
	Uint8 ir, ig, ib; /* same here. */

	if (src == NULL || sr == NULL || dst == NULL || dr == NULL) return -1;

	/* these are meaningless and would cause a divide by zero: */
	if (!dr->w || !dr->h) return -1; 

	dsx = ((double) sr->w) / dr->w;
	dsy = ((double) sr->h) / dr->h;

	lx = dr->x + dr->w; 
	ly = dr->y + dr->h;

	for (ty = dr->y, sy = (double)sr->y; ty < ly; ++ty, sy+=dsy)
	{
		for (tx = dr->x, sx = (double)sr->x; tx < lx; ++tx, sx+=dsx)
		{
			/* here we must consider four pixels and mix them together */
			/* the further away we are from a pixel, the less weight it has
			 * when we mix in its color. Hence the "1 - hypot(..." etc.
			 * Actually, no. Let's not use hypot().
			 */
			/* 
			 * upper left pixel 
			 */
			wx = ((floor(sx) + 1) - sx);
			wy = ((floor(sy) + 1) - sy);

			isx = (Uint32) floor(sx);
			isy = (Uint32) floor(sy);

			if (SDL_GetPixel(src, gsx, gsy, &ir, &ig, &ib)) return -1;
		
			weight = wx * wy; 
			/* the area of the overlap of our hypothetical and real pixel!!! */ 
			if (weight < 1/1024.0) weight = 0;
			r = weight * (double)ir;
			g = weight * (double)ig;
			b = weight * (double)ib;
			/*s = weight * 255.0;*/

			/* 
			 * upper right pixel 
			 */
			wx = 1 - wx;
			isx += 1;

			if (SDL_GetPixel(src, gsx, gsy, &ir, &ig, &ib)) return -1;
		
			weight = wx * wy;
			if (weight < 1/1024.0) weight = 0;
			r += weight * (double)ir;
			g += weight * (double)ig;
			b += weight * (double)ib;
			/*s += weight * 255.0;*/
	
			/* 
			 * lower right pixel 
			 */
			wy = 1 - wy;
			isy += 1;

			if (SDL_GetPixel(src, gsx, gsy, &ir, &ig, &ib)) return -1;
			
			weight = wx * wy;
			if (weight < 1/1024.0) weight = 0;
			r += weight * (double)ir;
			g += weight * (double)ig;
			b += weight * (double)ib;
			/*s += weight * 255.0;*/

			/*
			 * lower left pixel
			 */
			wx = 1 - wx;
			isx -= 1;

			if (SDL_GetPixel(src, gsx, gsy, &ir, &ig, &ib)) return -1;
			
			weight = wx * wy;
			if (weight < 1/1024.0) weight = 0;
			r += weight * (double)ir;
			g += weight * (double)ig;
			b += weight * (double)ib;
			/*s += weight * 255.0;*/
	

			/*
			r = 255 * (r/s);
			g = 255 * (g/s);
			b = 255 * (b/s);
			*/

			if (r >= 256.0 || g >= 256.0 || b > 256.0) 
			{
			    ERRORMSG("mixing error!, values: %f, %f, %f\n", (double)r, (double)g, (double)b);
				/**((char *)0) = 0;*/
			}
			if (r > 255.0) r = 255.0;
			if (g > 255.0) g = 255.0;
			if (b > 255.0) b = 255.0;
			ir = (Uint8) r;
			ig = (Uint8) g;
			ib = (Uint8) b;

			SDL_PutPixel(dst, tx, ty, ir, ig, ib);
		}
	}

	return 0;
}

#endif




/* This function will take an SDL_Surface, allocate a new surface to hold
 * the resized surface, perform the scaling operation, free the old surface
 * and return the new one. This behaviour is vaguely modeled after C library
 * string functions. Returns NULL on grievous errors! 
 *
 * The scaling operation is performed one or more times to accomodate
 * images comprised by a number of sub-images whose edges must not be blurred
 * with the edges of adjacent sub-images. (Think fonts and tile sets.)
 *
 * If t_oldw and t_oldh are set to src->w and src->h respectively
 *
 * t_oldw, t_oldh are the size of the old tiles
 */
SDL_Surface *SDL_ScaleTiledBitmap (SDL_Surface *src, 
				   Uint32 t_oldw, 
				   Uint32 t_oldh, 
				   Uint32 t_neww,
				   Uint32 t_newh,
				   int dealloc_src)

{
	SDL_Surface *dst;
	SDL_Rect sr, dr;
	Uint32 x, y;
	Uint32 nx, ny;

	if (!t_oldw || !t_oldh || !t_neww || !t_newh || !src) return NULL; /*dummy!*/

	if (t_oldw == t_neww && t_oldh == t_newh) return src; /* OK... */

	/* Get the number of tiles in the image.
	 * Any possible clipped tiles at the edges are ignored.
	 */
	nx = src->w / t_oldw;
	ny = src->h / t_oldh;

	/* Allocate a new SDL_Surface of appropriate size, with settings otherwise
	 * identical to src.
	 */
	dst = SDL_CreateRGBSurface(src->flags, 
				   nx * t_neww, 
				   ny * t_newh, 
				   //src->format->BitsPerPixel,
				   16,
				   src->format->Rmask,
				   src->format->Gmask,
				   src->format->Bmask,
				   src->format->Amask);
	
	
	for (y = 0; y < ny; ++y)
	{
		for (x = 0; x < nx; ++x)
		{
			sr.w = t_oldw; sr.h = t_oldh;
			sr.x = x * t_oldw; sr.y = y * t_oldh;
			
			dr.w = t_neww; dr.h = t_newh;
			dr.x = x * t_neww; dr.y = y * t_newh;

			/*printf("%d,%d -> %d,%d   (%d,%d -> %d, %d)\n", sr.x, sr.y, dr.x, dr.y, sr.w, sr.h, dr.w, dr.h);*/

			/* scale-blit one tile and check for error
			 * although SDl_ScaleBlit() might not have any errors to return.
			 */
			if (SDL_FastScaleBlit(src, &sr, dst, &dr)) return NULL;
		}
	}

	if (dealloc_src) SDL_FreeSurface(src);

	return dst;
}


/* The following function will extract height and width info from a filename
 * such as 16x16.xyz or 8X13.bar or even argle8ook16.foo
 *
 * I realize now that it's also useful for reading integers out of an argument
 * such as --fooscale1=2
 */

errr strtoii(const char *str, Uint32 *w, Uint32 *h)
{
	char buf[1024];
	char *s = buf;
	char *tok;
	char *numeric = "0123456789";

	size_t l; /* length of numeric string */

	if (!str || !w || !h) return -1;

	if (strlen(str) < 3) return -1; /* must have room for at least "1x1" */

	strncpy(buf, str, 1023);
	buf[1023] = '\0';

	tok = strpbrk(buf, numeric);
	if (!tok) return -1;

	l = strspn(tok, numeric);
	if (!l) return -1;

	tok[l] = '\0';

	s = tok + l + 1;

	if(!sscanf(tok, "%d", w)) return -1;

	/* next token */
	tok = strpbrk(s, numeric);
	if (!tok) return -1;

	l = strspn(tok, numeric);
	if (!l) return -1;

	tok[l] = '\0';
	/* no need to set s since this is the last token */

	if(!sscanf(tok, "%d", h)) return -1;

	return 0;

}




char *formatsdlflags(Uint32 flags) {
    char *buffer = malloc(128); // leak, fix later
    sprintf(buffer, "%s%s%s%s%s%s%s%s%s%s (%x)", 
			(flags & SDL_HWSURFACE) ? "SDL_HWSURFACE "  : "",
			(flags & SDL_ANYFORMAT) ? "SDL_ANYFORMAT "  : "",
			(flags & SDL_HWPALETTE) ? "SDL_HWPALETTE "  : "",
			(flags & SDL_DOUBLEBUF) ? "SDL_DOUBLEBUF "  : "", 
			(flags & SDL_FULLSCREEN) ?"SDL_FULLSCREEN " : "",
			(flags & SDL_RESIZABLE) ? "SDL_RESIZABLE "  : "",
			(flags & SDL_HWACCEL) ?   "SDL_HWACCEL "    : "",
			(flags & SDL_SRCCOLORKEY) ? "SDL_SRCCOLRKEY "  : "",
			(flags & SDL_RLEACCEL) ? "SDL_RLEACCEL "   : "",
			(flags & SDL_SRCALPHA) ? "SDL_SRCALPHA "   : "",
			flags);
    return buffer;
};






/* A lot of code for handling keystrokes follow. */


typedef struct sdl_keymapt sdl_keymapt;

struct sdl_keymapt {
	SDLKey k; /* what we get from SDL */
	char *s; /* what we feed to the Term_keypress */
	char *ctrl; /* what if CTRL is pressed? (NULL if the same) */
	char *shift; /* what if SHIFT is pressed? */
};

/* XXX XXX XXX the following keymap sucks. More comments below. */
sdl_keymapt sdl_keymap[] =
{
	/*{SDLK_UP, "[A", "Oa", "Ox"}, 
	{SDLK_DOWN, "[B", "Ob", "Or"},
	{SDLK_RIGHT, "[C", "Oc", "Ot"},
	{SDLK_LEFT, "[D", "Od", "Ov"},
	{SDLK_INSERT, "[2~", "[2^", "Op"},
	{SDLK_HOME, "[1~", "[1^", "Ow"},
	{SDLK_END, "[4~", "[4^", "Oq"},
	{SDLK_PAGEUP, "[5~", "[5^", "Oy"},
	{SDLK_PAGEDOWN, "[6~", "[6^", "Os"},*/
	{SDLK_F1, "[[A", NULL, NULL},
	{SDLK_F2, "[[B", NULL, NULL},
	{SDLK_F3, "[[C", NULL, NULL},
	{SDLK_F4, "[[D", NULL, NULL},
	{SDLK_F5, "[[E", NULL, NULL},
	{SDLK_F6, "[[17~", NULL, NULL},
	{SDLK_F7, "[[18~", NULL, NULL},
	{SDLK_F8, "[[19~", NULL, NULL},
	{SDLK_F9, "[[20~", NULL, NULL},
	{SDLK_F10, "[[21~", NULL, NULL},
	{SDLK_F11, "[[23~", NULL, NULL},
	{SDLK_F12, "[[24~", NULL, NULL},
	/* I have no machines with F13, F14, F15. Is that a Sun thing? */
	{SDLK_F13, "", NULL, NULL}, 
	{SDLK_F14, "", NULL, NULL},
	{SDLK_F15, "", NULL, NULL},
	{SDLK_RSHIFT, "", NULL, NULL}, 
	{SDLK_LSHIFT, "", NULL, NULL},
	{SDLK_RALT, "", NULL, NULL},
	{SDLK_LALT, "", NULL, NULL},
	{SDLK_RCTRL, "", NULL, NULL},
	{SDLK_LCTRL, "", NULL, NULL},
	{SDLK_RMETA, "", NULL, NULL},
	{SDLK_LMETA, "", NULL, NULL},
	{SDLK_NUMLOCK, "", NULL, NULL},
	{SDLK_CAPSLOCK, "", NULL, NULL},
	{SDLK_SCROLLOCK, "", NULL, NULL},
	{SDLK_LSUPER, "", NULL, NULL},
	{SDLK_RSUPER, "", NULL, NULL},
	{SDLK_HELP, "?", NULL, NULL},
	{SDLK_PRINT, "", NULL, NULL},
	{SDLK_SYSREQ, "", NULL, NULL},
	{SDLK_BREAK, "", NULL, NULL},
	{SDLK_MENU, "", NULL, NULL},
	{SDLK_POWER, "", NULL, NULL},
	{SDLK_EURO, "", NULL, NULL},
	{SDLK_0, "0", NULL, ")"}, /* XXX XXX XXX The CTRL-number keys need to be */
	{SDLK_1, "1", NULL, "!"}, /* defined since they represent digging for    */
	{SDLK_2, "2", NULL, "@"}, /* some people!. Really, this whole table      */
	{SDLK_3, "3", NULL, "#"}, /* should be replaced with something cleaner   */
	{SDLK_4, "4", NULL, "$"}, /* and an SDL pref file should be created.     */
	{SDLK_5, "5", NULL, "%"},
	{SDLK_6, "6", NULL, "^"},
	{SDLK_7, "7", NULL, "&"},
	{SDLK_8, "8", NULL, "*"},
	{SDLK_9, "9", NULL, "("},
	{SDLK_SEMICOLON, ";", NULL, ":"},
	{SDLK_COMMA, ",", NULL, "<"},
	{SDLK_PERIOD, ".", NULL, ">"},
	{SDLK_BACKSLASH, "\\", NULL, "|"},
	{SDLK_BACKQUOTE, "`", NULL, "~"},
	{SDLK_LEFTBRACKET, "[", NULL, "{"},
	{SDLK_RIGHTBRACKET, "]", NULL, "}"},
	{SDLK_MINUS, "-", NULL, "_"},
	{SDLK_EQUALS, "=", NULL, "+"},
	{SDLK_SLASH, "/", NULL, "?"},
	{SDLK_UNKNOWN, NULL, NULL, NULL} /* terminator */
};

void Multikeypress(char *k)
{
	while (*k) Term_keypress(*k++);
}

int IsMovement(SDLKey k)
{
	switch (k)
	{
		case SDLK_UP:
		case SDLK_DOWN:
		case SDLK_RIGHT:
		case SDLK_LEFT:
		case SDLK_INSERT:
		case SDLK_HOME:
		case SDLK_END:
		case SDLK_PAGEUP:
		case SDLK_PAGEDOWN:
		case SDLK_KP0:
		case SDLK_KP1:
		case SDLK_KP2:
		case SDLK_KP3:
		case SDLK_KP4:
		case SDLK_KP5:
		case SDLK_KP6:
		case SDLK_KP7:
		case SDLK_KP8:
		case SDLK_KP9:
			return TRUE;

		default:
			return FALSE;
	}
	return 1234567; /* all good children go to heaven */
}


char *SDL_keysymtostr(SDL_keysym *ks)
{
#ifdef bufsize
#error bufsize steps on previous define!
#endif
#define bufsize 64
	int bufused = 0;

	/* I am returning a pointer to the below variable. 
	 * I /think/ this is legal but I am not sure!  XXX XXX XXX
	 * It certainly seems to work fine, at least under GCC.
	 * It can easily be changed to a pointer passed as an argument.
	 */
	static char buf[bufsize]; 
	Uint8 ch;
	/*Uint32 i;*/

	/* cat for strings and app[end] for characters */
#define sdlkcat(a) strncat(buf,(a),bufsize-bufused-1); bufused+=strlen((a)); 
#define sdlkapp(a) if(bufused<bufsize-1) { buf[bufused]=a; buf[bufused+1]='\0'; bufused++; }

	buf[0] = '\0';

#if 0
	for (i = 0; ; ++i)
	{
		if (sdl_keymap[i].k == ks->sym) 
		{
			if (sdl_keymap[i].s && strlen(sdl_keymap[i].s)) 
			{
				if (ks->mod & KMOD_ALT) 
				{
					sdlkapp('');
				}
				if (ks->mod & KMOD_CTRL)
				{
					if(sdl_keymap[i].ctrl) 
					{
						sdlkcat(sdl_keymap[i].ctrl);
						break;
					} 
				} else
				if (ks->mod & KMOD_SHIFT)
				{
					if(sdl_keymap[i].shift)
					{
						sdlkcat(sdl_keymap[i].shift);
						break; 
					} 
				}
				sdlkcat(sdl_keymap[i].s);
			}
			break; /* out of the for() loop */
		} else
		if (sdl_keymap[i].k == SDLK_UNKNOWN)
		{
#endif
		  if (IsMovement(ks->sym)) {
		    // Stig: this was snprintf(), put it back in later!!
				sprintf(buf, "%c%s%s%s%s_%lX%c", 31,
						ks->mod & KMOD_CTRL  ? "N" : "",
						ks->mod & KMOD_SHIFT ? "S" : "",
						"", /* for future expansion. */
						ks->mod & KMOD_ALT   ? "M" : "",
						(unsigned long) ks->sym, 13);
				ch = 0;
			}else
			{
				if (ks->mod & KMOD_ALT) 
				{
					sdlkapp('');
				}
				/*ch = ks->sym; */
				ch = ks->unicode & 0xff;
				/*if (ch <= 'z' && ch >= 'a') {
					if (ks->mod & KMOD_CTRL)
					{
						ch = 1 + ch - 'a';
					} else
					if (ks->mod & KMOD_SHIFT)
					{
						ch += ('A' - 'a');
					}
				}*/
			}

			if (ch) sdlkapp(ch);

#if 0
			break; /* end the for loop; we're at the end of keymap */

		}
	} /* for... */
#endif


	/*puts(buf);*/
	return buf;
#undef bufsize
#undef sdlkcat
#undef sdlkapp

} /* SDL_keystring */



/* Cursor hack, for testing of ideas. XXX XXX XXX */

SDL_Surface *sdl_screen_cursor = NULL;
SDL_Rect sdl_screen_cursor_sr;


/* it must be true that w * h * 4 <= maxUint32 */

errr SDL_init_screen_cursor(Uint32 w, Uint32 h)
{
	Uint32 i;

	sdl_screen_cursor_sr.x = sdl_screen_cursor_sr.y = 0;
	sdl_screen_cursor_sr.w = w;
	sdl_screen_cursor_sr.h = h;

	sdl_screen_cursor = NULL;
	sdl_screen_cursor = SDL_CreateRGBSurface(SDL_SRCALPHA, w, h, 32,
	                                         0xff000000,
						 0x00ff0000,
						 0x0000ff00,
						 0x00000000);

	if (!sdl_screen_cursor) return -1;

	SDL_SetAlpha(sdl_screen_cursor, SDL_SRCALPHA | SDL_RLEACCEL, 0x80);
	for (i = 0; i < w*h*4; ++i)
	{
		((Uint8 *)(sdl_screen_cursor->pixels))[i] = !(i & 2)?0x80 : 0xFF;
	}

	return 0;
}

errr SDL_DrawCursor(SDL_Surface *dst, SDL_Rect *dr)
{
	if (!dst || !dr || !sdl_screen_cursor) return -1;
	if (SDL_BlitSurface(sdl_screen_cursor, &sdl_screen_cursor_sr, dst, dr)) return -1;
	SDL_UpdateRect(dst, dr->x, dr->y, dr->w, dr->h);
	return 0;
}


#define JAI_PutRawPixel8(ptr,pxl) (*(Uint8*)ptr=pxl)
#define JAI_PutRawPixel16(ptr,pxl) (*(Uint16*)ptr=pxl)
#if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
#define JAI_PutRawPixel24(ptr,pxl) {*(Uint8*)ptr=(pxl>>16)&0xff;ptr[1]=(pxl>>8)&0xff;ptr[2]=pxl&0xff;}
#else
#define JAI_PutRawPixel24(ptr,pxl) {ptr[2]=(pxl>>16)&0xff;ptr[1]=(pxl>>8)&0xff;*(Uint8*)ptr=pxl&0xff;}
#endif
#define JAI_PutRawPixel32(ptr,pxl) (*(Uint32*)ptr=pxl)
#define JAI_PutRawPixel(ptr,pxl,bpp) { \
    switch(bpp) { \
        case 1: \
            JAI_PutRawPixel8(ptr,pxl); \
            break; \
        case 2: \
            JAI_PutRawPixel16(ptr,pxl); \
            break; \
        case 3: \
            JAI_PutRawPixel24(ptr,pxl); \
            break; \
        case 4: \
            JAI_PutRawPixel32(ptr,pxl); \
            break; \
    } }

#define JAI_GetRawPixel8(ptr) (*ptr)
#define JAI_GetRawPixel16(ptr) (*(Uint16*)ptr)
#if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
#define JAI_GetRawPixel24(ptr)  (ptr[0] << 16 | ptr[1] << 8 | ptr[2])
#else
#define JAI_GetRawPixel24(ptr)  (ptr[2] << 16 | ptr[1] << 8 | ptr[0])
#endif
#define JAI_GetRawPixel32(ptr) (*(Uint32*)ptr)
#define JAI_GetRawPixel(ptr,bpp) ( \
    (bpp==1)?JAI_GetRawPixel8(ptr):\
	(bpp==2)?JAI_GetRawPixel16(ptr):\
	(bpp==3)?JAI_GetRawPixel24(ptr):\
	(bpp==4)?JAI_GetRawPixel32(ptr):\
	0)


int JAI_BlitSurfaceAlpha(SDL_Surface *src, SDL_Rect *srcrect,
			 SDL_Surface *dst, SDL_Rect *dstrect)
{
	SDL_Rect sr,dr, sr2,dr2;
	int retval;

	if(srcrect)
	{
		memcpy(&sr,srcrect,sizeof(SDL_Rect));
		memcpy(&sr2,srcrect,sizeof(SDL_Rect));
	}
	else
		memcpy(&sr,&src->clip_rect,sizeof(SDL_Rect));
	if(dstrect)
	{
		memcpy(&dr,dstrect,sizeof(SDL_Rect));
	}
	else
		memcpy(&dr,&dst->clip_rect,sizeof(SDL_Rect));
	memcpy(&sr2,&sr,sizeof(SDL_Rect));
	memcpy(&dr2,&dr,sizeof(SDL_Rect));
	retval=SDL_BlitSurface(src, &sr2, dst, &dr2);
	if(retval<0)
		return retval;
	if(dst->flags&SDL_SRCALPHA && src->flags&SDL_SRCALPHA)
	{
		Uint8 as,ad,*ps,*pd,rgb;
		SDL_Color cd;
		int bpps, bppd, x, y;
		Uint32 p;

//		debug("JAI_BlitSurfaceAlpha: before %dx%d+%d+%d to %dx%d+%d+%d\n",
//				sr.w,sr.h,sr.x,sr.y, dr.w,dr.h,dr.x,dr.y);
		if(dr.x<0)
		{
			sr.x-=dr.x;
			sr.w+=dr.x;
			dr.w+=dr.x;
			dr.x=0;
		}
		if(dr.y<0)
		{
			sr.y-=dr.y;
			sr.h+=dr.y;
			dr.h+=dr.y;
			dr.y=0;
		}
//		debug("JAI_BlitSurfaceAlpha: after  %dx%d+%d+%d to %dx%d+%d+%d\n",
//				sr.w,sr.h,sr.x,sr.y, dr.w,dr.h,dr.x,dr.y);
		SDL_LockSurface(src);
		SDL_LockSurface(dst);
		bpps=src->format->BytesPerPixel;
		bppd=dst->format->BytesPerPixel;
		for(y=0; y<sr.h && y+dr.y<dst->clip_rect.h; y++)
		{
			ps=src->pixels + bpps*sr.x + src->pitch*(sr.y+y);
			pd=dst->pixels + bppd*dr.x + dst->pitch*(dr.y+y);
			for(x=0; x<sr.w && x+dr.x<dst->clip_rect.w; x++)
			{
				p=JAI_GetRawPixel(ps,bpps);
				SDL_GetRGBA(p,src->format, &rgb,&rgb,&rgb,&as);
				p=JAI_GetRawPixel(pd,bppd);
				SDL_GetRGBA(p,dst->format, &cd.r,&cd.g,&cd.b,&ad);
				if(as>ad)
					ad=as;
				p=SDL_MapRGBA(dst->format,cd.r,cd.g,cd.b,ad);
				JAI_PutRawPixel(pd,p,bppd);
				ps+=bpps;
				pd+=bppd;
			}
		}
		SDL_UnlockSurface(src);
		SDL_UnlockSurface(dst);
	}
	return retval;
}


/*
 * Load a HEX font.
 * See http://czyborra.com/unifont/
 *
 * XXX Note. Your file should not be all full-width glyphs. At least one
 * half-width glyph must be present for my lame algorithm to work.
 * It is OK to have all half-width glyphs.
 * 
 * This routine will try to use strtoii() to figure out the font's bounding
 * box from the filename. This seems to be an acceptable thing to try, 
 * as seen in main-win.c
 *
 * FIXME
 * BUGS: There is no attempt made at figuring out a righteous bounding box.
 *	      Certain HEX fonts can be *wider* than 16 pixels. They may break.
 *
 *	What we need is a BDF loader. It's not a high priority though.
 *
 */

#define highhextoi(x) (strchr("ABCDEF", (x))? 0xA + ((x)-'A'):0)
#define hexchartoi(x) (strchr("0123456789", (x))? (x)-'0' : highhextoi((x)))

#ifndef MAX_HEX_FONT_LINE
#define MAX_HEX_FONT_LINE 1024
#endif

static errr
load_HEX_font_sdl(FontData *fd, cptr filename, bool justmetrics) {

	FILE *f;

	char buf[1036]; /* 12 (or 11? ;->)extra bytes for good luck. */

	char gs[MAX_HEX_FONT_LINE]; /* glyph string */

	Uint32 i,j;

	errr fail = 0; /* did we fail? */

	Uint32 x; /* current x in fd->face */
	Uint32 y; /* current y in fd->face */

	Uint32 gn; /* current glyph n */

	Uint32 n; /* current nibble or byte or whatever data from file */

	Uint32 pos; /* position in the nasty string */

	Uint32 bytesdone; /* bytes processed */

	Uint32 mw, mh; /* for strtoii() */

	SDL_Surface *face;
	int dw = 0, dh = 0;

	/* check font_data */
	if (fd->width || fd->height || fd->theFont) {
	    INFOMSG("Data for font already allocated, exiting.\n");
	    return 1; /* dealloc it first, dummy. */
	}

	/* Build the filename */
//	sprintf(buf, "%s/%s", ANGBAND_DIR_XTRA, filename);
	sprintf(buf, "%s%s/%s", base_config_path, "fonts", filename);

	f = fopen(buf, "r");

	if (!f) 
	{
	    ERRORMSG("Hexfont: Couldn't open: %s\n", buf);
	    return -1;
	}

	

	/* try hard to figure out the font metrics */

	while (fgets(gs, MAX_HEX_FONT_LINE, f) != NULL)
	{
		i = strlen(gs);

		if (gs[i-1] == '\n') i--;
		if (gs[i-1] == '\r') i--;
		
		i -= 5; /* each line begins with 1234: */

		/* now i is the number of nibbles in the line */

		if (i & 1)
		{
			ERRORMSG("Error in HEX line measurment. Report to stig@langband.org or hmaon@bumba.net.");
			fclose(f);
			fail = -1;
			break;
		}

		i >>= 1; /* i is number of bytes */

		if (!fd->height)
		{
			dw = fd->width = 8; /* a nasty guess. */
			dh = fd->height = i;
			/*if (i & 1) break;*/ /* odd number of bytes. this is the height. */
		} else 
		{
			if (i > fd->height) {
				dw = fd->width = 16; /* an even nastier guess (full-width glyphs here) */
				if(fd -> height / 2 == i / 3)
				{
					/* this sucks. */
					dh = fd->height = i / 3;
					dw = fd->width = 24;
				} else
				if(i != (fd->height)*2) /* check sanity and file integrity */
				{
					ERRORMSG("Error 2 in HEX measurement.");
					/*fail = -1;*/
				}
				break; /* this is a full-width glyph. We have the height. */
			} else
			if (i < fd->height) {
				if (i*2 != fd->height)
				{
					ERRORMSG("Error 3 in HEX measurement.");
					/*fail = -1;*/
				}
				dw = fd->width = 16; /* the same nastier guess. */
				dh = fd->height = i; /* Ah, so this is the height */
			}
			/* they're equal. we can say nothing about the glyph height */
		}
	}

	/* analyze the file name */
	if(!strtoii(filename, &mw, &mh)) {

		/* success! */
		dw = mw;
		dh = mh;
	}
	else {

	    INFOMSG("You may wish to incude the dimensions of a font in its file name. ie \"vga8x16.hex\"");
	}
/*
	if (justmetrics) 
	{
		fclose(f);
		return fail;
	}
*/
	/* Might as well allocate the bitmap here. */
	/* XXX I'm not sure what pixel format to use but the text blitter is 
	 * probably the wrong thing to start optimizing.
	 */
	face = SDL_CreateRGBSurface(SDL_SWSURFACE, fd->width, 256*fd->height,8,0,0,0,0); 
	if (!face) return -1;
	SDL_SetAlpha(face, SDL_RLEACCEL, SDL_ALPHA_OPAQUE); /* use RLE */

	rewind(f);

	while (fgets(gs, MAX_HEX_FONT_LINE, f) != NULL)
	{
#ifdef FONT_LOAD_DEBUGGING
		puts("");
		puts(gs);
#endif
		/* figure out character code (aka index). xxxx:... */
		if (sscanf(gs, "%4x", &gn) != 1)
		{
		    ERRORMSG("Broken HEX file.");
		    fail = -1;
		    break;
		}

#ifdef FONT_LOAD_DEBUGGING
		printf("%4x:\n", gn);
#endif
		if (gn > 255) {
			gn = 255;
		}

		x = 0; 
		y = fd->height * gn;
		
		i = strlen(gs);

		if (gs[i-1] == '\n') {
			i--;
			gs[i] = '\0';
		}
		if (gs[i-1] == '\r') 
		{
			i--;
			gs[i] = '\0';
		}
		
		i -= 5; /* each line begins with 1234: */
		/* now i is the number of nibbles represented in the line */
		i >>= 1; /* now bytes. */

		pos = 5;
		bytesdone = 0; 

		while (gs[pos] != '\0' && pos < strlen(gs)) 
		{
			n  = (hexchartoi(gs[pos])) << 4; pos++; 
			n += (hexchartoi(gs[pos])); pos++;
			/* they're macros. do NOT pass a "pos++" to them! :) :) :) */

			for(j = 0; j < 8; ++j, ++x, n <<= 1)
			{
				if (n & 0x80) 
				{
#ifdef FONT_LOAD_DEBUGGING
					printf("#");
#endif
					((Uint8 *)face->pixels)[x + y*face->pitch] = 0xff;
				} else
				{
#ifdef FONT_LOAD_DEBUGGING
					printf("-");
#endif
					((Uint8 *)face->pixels)[x + y*face->pitch] = 0x00;
				}
			}
			++bytesdone;

			/* processing half-width glyph or just finished even byte */
			if (i == fd->height || ((i == 2*fd->height) && !(bytesdone & 1))) 
			{
				x = 0;
				++y;
#ifdef FONT_LOAD_DEBUGGING
				printf("\n");
#endif
			} else if (i == 2*fd->height)
			{
				/* XXX do nothing? */
			} else 
			{
				/* XXX XXX XXX softer errors since HEX seems to actually permit
				 * this situation
				 */
				/*plog("HEX measurement error, fd->h is not a multiple of i.");*/
				/*fail = -1;*/
			}
		} /* while (gs[pos... */
	} /* while (fgets... */

	//DBGPUT("Val %d %d\n", dw, dh);
	fd->theFont = face;
	
	return fail;
}

FontData *
load_hex_font(cptr filename, bool justmetrics) {
    int retval;
    FontData *fdata = malloc(sizeof(FontData));
    fdata->theFont = NULL;
    fdata->width = 0;
    fdata->height = 0;
    fdata->font_type = FONT_TYPE_HEX;
    retval = load_HEX_font_sdl(fdata, filename, justmetrics);
    //DBGPUT("Val %d %d -> %d\n", fdata->width, fdata->height, retval);
    if (retval != 0) {
	free(fdata);
	return NULL;
    }
    else {
	return fdata;
    }
}

#ifdef ALLOW_TTF

FontData *
load_ttf_font(const char *fname, int ptsize) {

    int renderstyle = TTF_STYLE_NORMAL;
    int max_width = -1;
    char buf[128];
    
    TTF_Font *theFont;
    FontData *fdata = malloc(sizeof(FontData));

    
//    DBGPUT("Opening font %s\n", fname);
    
   
    TTF_Init();
    
    sprintf(buf, "%s%s/%s", base_config_path, "fonts", fname);
    
    theFont = TTF_OpenFont(buf, ptsize);

    if (!theFont) {
	ERRORMSG("Unable to find font '%s'\n", buf);
	return NULL;
    }
    
    TTF_SetFontStyle(theFont, renderstyle);

    // DBGPUT("Font %s loaded", fname);

    fdata->theFont = theFont;
    fdata->font_type = FONT_TYPE_TTF;
        
    {
	int i;
	char buf[10];
	SDL_Color white = { 0xFF, 0xFF, 0xFF, 0 };
	for (i=0; i < 256; i++) {
	    SDL_Surface *text;
	    text = TTF_RenderGlyph_Solid(theFont, i, white);
	    if (text->w > max_width) {
		max_width = text->w;
	    }
	    fdata->letters[i] = text;
	}

	//max_width -= 2; // hack
    }

    fdata->width = max_width;
    fdata->height = TTF_FontHeight(theFont);
    
    return fdata;
}

int
display_char(SDL_Surface *surface, SDL_Rect *dest, FontData *fdata, s16b attr, s16b ch) {

    SDL_Surface *letter = NULL;
    
    if (!fdata->theFont) return -1;
    // DBGPUT("fff %c %d\n", ch, attr & 0xf);

    // This is the socalled fast/blit version
/*
    letter = fdata->letters[ch];  
    {
	SDL_PixelFormat *format = letter->format;
	// DBGPUT( "Ckey %ld depth %d\n", format->colorkey, format->BitsPerPixel);
	format->palette->colors[1] = color_data_sdl[attr & 0xf];
    }
*/  
    // SDL_SetColors(letter, &(color_data_sdl[attr & 0xf]), 0xff, 1);
    // SDL_SetColors(letter, &(color_data_sdl[colour&0xf]), 0xff, 1);
    
    // this is the socalled render version which renders anew every time
    {
	TTF_Font *font = (TTF_Font *)(fdata->theFont);
	letter = TTF_RenderGlyph_Solid(font, ch, color_data_sdl[attr & 0xf]);
	// DBGPUT("managed to render\n");
    }
	
    SDL_BlitSurface(letter, NULL, surface, dest);
    //SDL_UpdateRect(surface, dest->x, dest->y, dest->w, dest->h);
    
    return 0;
}

#endif /* use_ttf */

#endif /* use_sdl */
