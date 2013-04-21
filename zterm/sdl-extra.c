/*
 * DESC: sdl-extra.c - extra functionality for SDL backend
 * Copyright (c) 2002 - Stig Erik Sandø

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * -----
 *
 * Has only been slighttly adapted from Greg Velichansky's SDL visual module for regular
 * Angband:
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


#ifdef USE_SDL

#include "SDL.h"
#ifdef ALLOW_TTF
#include "SDL_ttf.h"
#endif /* allow_ttf */
#include <string.h>
#include <math.h> /* for scaling blits */
#include "langband.h"


/* The following function will extract height and width info from a filename
 * such as 16x16.xyz or 8X13.bar or even argle8ook16.foo
 *
 * I realize now that it's also useful for reading integers out of an argument
 * such as --fooscale1=2
 */

int
strtoii(const char *str, Uint32 *w, Uint32 *h) {

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

static int
load_HEX_font_sdl(FontData *fd, const char *filename, bool justmetrics) {

	FILE *f;

	char buf[1036]; /* 12 (or 11? ;->)extra bytes for good luck. */

	char gs[MAX_HEX_FONT_LINE]; /* glyph string */

	Uint32 i,j;

	int fail = 0; /* did we fail? */

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
load_hex_font(const char *filename, bool justmetrics) {
    
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
	//char buf[10];
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
