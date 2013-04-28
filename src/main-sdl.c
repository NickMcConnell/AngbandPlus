/* File: main-sdl.c */

/*
 * Copyright (c) 2007 Leon Marrick and others
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */


/*
 * This file helps Angband work with at least some computers capable of running
 * SDL, a set of simple and quite popular game development libraries that work
 * on many different operating systems, including Windows, most flavours of
 * UNIX or similar, and Mac OS X.  It requires a 32-bit (or higher) machine
 * capable of displaying at least 640x480 in 256 colors.  A Pentium or better
 * is strongly recommended (for speed reasons treated more fully below).
 *
 * To use this file, use an appropriate "Makefile" or "Project File", install
 * the required libraries (described below), make sure that "USE_SDL" is
 * defined somewhere, and obtain various extra files (described below).  If
 * you are new to all this, read "makefile.sdl".
 *
 * This port uses the following libraries:  SDL (v1.2+), SDL_ttf, and
 * SDL_mixer (only needed if USE_SOUND is defined).  All are available as
 * source code, pre-compiled libs for developers, and libs (or dlls) for
 * players from www.libsdl.org
 *
 *
 * Other files used by this port:
 * - The game must have a collection of bitmap .fon files in /lib/xtra/font.
 *
 * - If "USE_GRAPHICS" is defined, then it also needs some bitmapped (.bmp)
 *   graphics files in /lib/xtra/graf, such as "16x16.bmp" and "16x16m.bmp".
 * - If "USE_SOUND" is defined "*.WAV" sound files must be placed into
 *   "lib/xtra/sound/".
 *
 * - The "lib/pref/pref-sdl.prf" file contains keymaps, macro definitions,
 *   and/or color redefinitions.
 * - The "lib/pref/font-sdl.prf" contains attr/char mappings for use with the
 *   normal "*.fon" font files in the "lib/xtra/font/" directory.
 *
 * - The files "win/angband.rc" and "win/angband.ico" can be included in your
 *   project if using MSVC++.  "win/angband.ico" can also be added if using
 *   DevC++ and probably other Windows compilers.
 *
 *
 * "Term" framework by Ben Harrison (benh@phial.com).
 *
 * Original Sangband SDL port and the "intrface" module by Leon Marrick
 * (www.runegold.org/sangband).
 *
 * Additional helpful ideas by:
 * 2001 Gregory Velichansky <hmaon@bumba.net>, creator of the first Angband SDL
 * port.
 * 2006 Eric Stevens <sdltome@gmail.com>, main author of the TOME SDL port.
 */

/*
 * Comments on using SDL with Angband:
 *
 * The good news:
 * - SDL is cross-platform.  Really.  No joke.  If this port doesn't work on
 *   your system, it probably isn't SDL's fault.
 * - SDL is relatively easy to use, allowing you you to cobble up feature-rich
 *   apps that look half-way decent without too much fuss and bother.  It's
 *   wonderful for prototyping.
 * - SDL does most of what *band developers are likely to need, plus a whole
 *   lot more we haven't realized we want yet.
 * - SDL is a cleanly written open-source API; it is much less painful to get
 *   the straight word then it is with most other libraries.  Also, the SDL
 *   community offers active discussion boards, solid documentation, and
 *   numerous code examples relating to almost any question you might have
 *   occasion to ask.
 *
 * The bad news:
 * - SDL can be tedious to install.  Each individual library is straightforward
 *   enough, but *band development work requires several, the number growing as
 *   you get more sophisticated.
 * - SDL (as a stand-alone lib, without the assistance of OpenGL) can be very
 *   sluggish if you aren't careful.  It is poor at detecting, let alone making
 *   fullest use of, available video hardware, which cripples speed.  So,
 *   getting half-way decent performance in a game like Angband takes some
 *   skill and vast amounts of effort.  Speed - the lack thereof - is this
 *   port's biggest problem.  More comments below.
 * - SDL is not a complete game development library (although the add-ons help
 *   tremendously).  Much-needed functionality - text output, blit stretching,
 *   and video refresh synching being three examples - were either missing
 *   altogether or covered by functions that proved too slow or delicate for
 *   production use.  I ended up having to spend at least as much time, and
 *   write at least as much low-level code, as I did using the Win32 API.
 * - SDL, like Allegro but to a lesser extent, is falling behind current tech-
 *   nology.  Development progresses, but obsolescence looms, especially on
 *   Windows machines.
 */


#ifdef USE_SDL

/* Include the Angband headers, including the interface stuff */
#include "angband.h"
#include "intrface.h"

/* Some standard C headers */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef HAVE_ICONV
#include <iconv.h>
#endif


/* Include the basic and font-rendering SDL libraries (both are required) */
#include "SDL/SDL.h"
#include "SDL/SDL_audio.h"
#include "SDL/SDL_ttf.h"


/* Optional libaries:  SDL_Mixer (needed to play sounds) */
#ifdef USE_SOUND
#  include "SDL/SDL_mixer.h"
#endif /* USE_SOUND */



/*
 * Do not support gamma (yet)  XXX XXX
 */
#undef SUPPORT_GAMMA


/*
 * Pointers to SDL surfaces for the application window, graphics, and mask
 */
static SDL_Surface *AppWin;
static SDL_Surface *GraphSurface;
static SDL_Surface *MaskSurface;



/*
 * User-defined message types.  SDL allocates 12 messages for this purpose.
 */
#define USER_MSG_TIMER      1
#define USER_MSG_SOUND      2
#define USER_MSG_MUSIC      3



/*
 * Hack -- define which keys should be ignored
 */
static bool ignore_key[1024];

/*
 * We ignore all keypresses involving only modifier keys
 */
static int ignore_key_list[] =
{
	SDLK_NUMLOCK, SDLK_CAPSLOCK, SDLK_SCROLLOCK, SDLK_RSHIFT,
	SDLK_LSHIFT, SDLK_RCTRL, SDLK_LCTRL, SDLK_RALT, SDLK_LALT,
	SDLK_RMETA, SDLK_LMETA, SDLK_LSUPER, SDLK_RSUPER, SDLK_MODE,
	SDLK_COMPOSE, 0
};

/*
 * The number pad consists of 10 keys, each with an SDL identifier
 */
#define is_numpad(k) \
  ((k == SDLK_KP0) || (k == SDLK_KP1) || (k == SDLK_KP2) || (k == SDLK_KP3) || \
   (k == SDLK_KP4) || (k == SDLK_KP5) || (k == SDLK_KP6) || \
   (k == SDLK_KP7) || (k == SDLK_KP8) || (k == SDLK_KP9))


/*
 * The "complex" color values
 */
static SDL_Color win_clr[256];

/*
 * Save the background color
 */
static u32b	black_clr;




/*
 * A time reference used to determine how long it has been since the previous mouseclick.
 */
static u32b mouseclick_timer = 0L;



#ifdef USE_SOUND

	/*
	 * "Let there be two channels"
	 *
	 * On my test machine, if this value is set to more than 2,
	 * "Mix_AllocateChannels()" will allocate all the channels, but the
	 * game will crash if any channel other than #0 or #1 is opened.
	 */
	#define NUM_CHANNELS  2

	/*
	 * An array of pointers to "Mix_Chunk" data, one for each possible channel
	 */
	static Mix_Chunk *mix_chunks[NUM_CHANNELS];

	/*
	 * Global pointer to the active music track
	 */
	Mix_Music *music_track;

	/*
	 * Actual number of channels (doesn't work correctly!)
	 */
	static int max_channels = 0;

#endif /* USE_SOUND */



/*
 * Store some update rects before force-flushing them.
 *
 * This value has a major effect on the efficiency of all screen-output code
 * because of its effect on how "SDL_UpdateRects" interacts with screen
 * refreshes.  When too high, the game misses screen updates.  When too low,
 * all sorts of unnecessary calculations are required.
 */
#define MAX_UPDATE_RECTS    64

/*
 * An array of pending update rectangles (for efficiency).
 */
static SDL_Rect update_rects[MAX_UPDATE_RECTS];
static int update_rects_num = 0;



/* Forward declare */
typedef struct stretch_array_type stretch_array_type;

/*
 * A structure for data related to (simple) blit stretching.
 */
struct stretch_array_type
{
	int src_w, src_h;          /* Source width and height, in pixels */
	int dst_w, dst_h;          /* Destination width and height, in pixels */
	int src_pitch, dst_pitch;  /* Pixel width of src and dst, in bytes */
	int src_bpp, dst_bpp;      /* Color depth, in bytes */

	int size;                  /* Size of stretch arrays */

	/* Pixel offsets for source and destination (first byte of color information) */
	int *src_pixel_offset;
	int *dst_pixel_offset;
};

/* The (single) stretch array */
static stretch_array_type *stretch_array;




/*
 * Fill in an SDL_Rect structure.
 */
static SDL_Rect do_sdl_rect(int x, int y, int w, int h)
{
	SDL_Rect rect;

	rect.x = (u16b)x;
	rect.y = (u16b)y;
	rect.w = (u16b)w;
	rect.h = (u16b)h;

	return (rect);
}

/*
 * Validate a file
 */
static void validate_file(cptr s)
{
	int fd = fd_open(s, O_WRONLY);

	if (fd >= 0) (void)fd_close(fd);

	if (fd < 0) quit_fmt("Cannot find required file:\n%s", s);
}



/*
 * Flush the update queue to the application window.
 *
 * This function handles all clipping and error-checking.
 *
 *
 * Right here is where this port bogs down.  SDL does not reliably offer
 * hardware surfaces, so all our drawing must be done in (at least) two steps:
 * Once to the application surface in system memory, and then again to the
 * physical screen. It is this second step that is done in "SDL_UpdateRects()".
 *
 * Writing to to the video card should ideally be called in synchronization
 * with the video refresh.  However, SDL offers no way for you to do this
 * (that job is left in the capable hands of OpenGL).  "SDL_Flip()" can
 * synchronize, but only if you have hardware surfaces.  Also, a flip covers
 * the full screen and updating this way is poorly suited for Angband.
 *
 * If you want to optimize, start here!  XXX XXX XXX
 */
static void flush_update_rects(void)
{
	int i;

	/* For each update rect stored in our queue */
	for (i = 0; i < update_rects_num; i++)
	{
		/* Get this rect */
		SDL_Rect *rect = &update_rects[i];

		/* Enforce left-top edge */
		if (rect->x < 0) rect->x = 0;
		if (rect->y < 0) rect->y = 0;

		/* Constrain size to window */
		if (rect->x + rect->w > AppWin->w) rect->w = AppWin->w - rect->x;
		if (rect->y + rect->h > AppWin->h) rect->h = AppWin->h - rect->y;
	}

	/* Update all the rects */
	SDL_UpdateRects(AppWin, update_rects_num, update_rects);

	/* No rects to update */
	update_rects_num = 0;
}



/*
 * Pre-calculate data used in simple (non-blended) stretching.  -LM-
 *
 * Angband needs to use a lot of stretching.  Fortunately, great efficiency
 * gains are possible.  Firstly, source and destination surfaces and cell size
 * often do not change for long runs of output.  Once we set up an array of
 * pixel locations we can therefore use it many times before having to re-
 * calculate.  Secondly, among the operations we can do beforehand is to set
 * up pixel offset maps for both source and destination, reducing the actual
 * stretch operation to a mere loop and lookup.
 *
 * Note:  We assume that all the data we are fed is valid.  XXX
 */
static errr stretch_recalculate_array_sdl(SDL_Surface *src, int src_w, int src_h,
	SDL_Surface *dst, int dst_w, int dst_h)
{
	int i, v;
	int dst_x, dst_y, src_x, src_y;

	/* Arrays to hold the pixel offset data */
	int *src_pixel_offset_data;
	int *dst_pixel_offset_data;


	/* Point to the stretch array (there must be a stretch array set up) */
	stretch_array_type *stretch_ptr = stretch_array;


	/* Free any stored pixel offsets */
	if (stretch_ptr->src_pixel_offset) FREE(stretch_ptr->src_pixel_offset);
	if (stretch_ptr->dst_pixel_offset) FREE(stretch_ptr->dst_pixel_offset);


	/* Save a bunch of information */
	stretch_ptr->src_w     = src_w;
	stretch_ptr->src_h     = src_h;
	stretch_ptr->src_pitch = src->pitch;
	stretch_ptr->src_bpp   = src->format->BytesPerPixel;

	stretch_ptr->dst_w     = dst_w;
	stretch_ptr->dst_h     = dst_h;
	stretch_ptr->dst_pitch = dst->pitch;
	stretch_ptr->dst_bpp   = dst->format->BytesPerPixel;


	/* Calculate destination size (array length) */
	stretch_ptr->size = dst_h * dst_w;

	/* Make new pixel offset arrays */
	C_MAKE(src_pixel_offset_data, stretch_ptr->size, int);
	C_MAKE(dst_pixel_offset_data, stretch_ptr->size, int);

	/* Point to them */
	stretch_ptr->src_pixel_offset = src_pixel_offset_data;
	stretch_ptr->dst_pixel_offset = dst_pixel_offset_data;


	/* Start at the beginning of the array */
	i = 0;

	/* For every row in the destination cell, */
	for (dst_y = 0; dst_y < dst_h; dst_y++)
	{
		/* Figure out which source row should be used */
		src_y = ((dst_y * src_h) + (dst_h/2)) / dst_h;

		/* For every column in the destination cell, */
		for (dst_x = 0; dst_x < dst_w; dst_x++)
		{
			/* Figure out which source column should be used */
			src_x = ((dst_x * src_w) + (dst_w/2)) / dst_w;

			/* Calculate pixel offset for the source */
			v = (src_y * src->pitch) + (src_x * src->format->BytesPerPixel);

			/* Save it */
			src_pixel_offset_data[i] = v;

			/* Calculate pixel offset for the destination */
			v = (dst_y * dst->pitch) + (dst_x * dst->format->BytesPerPixel);

			/* Save it */
			dst_pixel_offset_data[i] = v;

			/* Advance to the next array position */
			i++;

			/* Add additional blending data here?  Perhaps allow some stretch options? */
		}
	}

	/* Success */
	return (0);
}


/*
 * Helper function that stretches or shrinks pixel data to size and blits it
 * to the destination surface.  -LM-
 *
 * We favor simplicity over beauty, partly due to lack of skill in
 * color combining (especially with multiple possible bit depths).
 *
 * Source and destination surface must be of the same bit depth.  XXX
 *
 * It is strongly recommended that the source surface be in system memory.
 * This function needs to read pixel data, a very slow operation on modern
 * video cards.  If we really want to store our graphics in video memory,
 * then copying the source cell before accessing it might work.
 *
 * To do:  Accept transparency.
 */
static errr sdl_stretch(SDL_Surface *src, SDL_Rect src_rect, SDL_Surface *dst,
	SDL_Rect dst_rect)
{
	int i;
	Uint8 *ps0, *pd0;
	int size;

	int *src_offset;
	int *dst_offset;

	Uint8 src_bpp = src->format->BytesPerPixel;
	Uint8 dst_bpp = dst->format->BytesPerPixel;


	/* Paranoia -- refuse NULL surfaces */
	if (!src || !dst) return (2);

	/* Require identical bit depths  XXX */
	if (src_bpp != dst_bpp)
	{
		plog("sdl_stretch() called using incompatible surfaces.");
		return (1);
	}

	/* Auto-calculate some inputs */
	if (!src_rect.w) src_rect.w = src->w;
	if (!src_rect.h) src_rect.h = src->h;
	if (!dst_rect.w) dst_rect.w = dst->w;
	if (!dst_rect.h) dst_rect.h = dst->h;

	/* Clip rectangles, refuse to do entirely off-surface blits */
	if (src_rect.x + src_rect.w >= src->w)
	{
		if (src_rect.x >= src->w) return (1);
		else src_rect.w = src->w - src_rect.x;
	}
	if (src_rect.y + src_rect.h >= src->h)
	{
		if (src_rect.y >= src->h) return (1);
		else src_rect.h = src->h - src_rect.y;
	}
	if (dst_rect.x + dst_rect.w >= dst->w)
	{
		if (dst_rect.x >= dst->w) return (1);
		else dst_rect.w = dst->w - dst_rect.x;
	}
	if (dst_rect.y + dst_rect.h >= dst->h)
	{
		if (dst_rect.y >= dst->h) return (1);
		else dst_rect.h = dst->h - dst_rect.y;
	}


	/* Either src or dst size doesn't match the pre-calculated stretch array */
	if ((src_rect.w != stretch_array->src_w) || (src_rect.h != stretch_array->src_h) ||
	    (dst_rect.w != stretch_array->dst_w) || (dst_rect.h != stretch_array->dst_h) ||

		/* Or the surfaces are different in pixel width or bit depth */
	    (src->pitch != stretch_array->src_pitch) ||
	    (dst->pitch != stretch_array->dst_pitch) ||
	    (src->format->BytesPerPixel != stretch_array->src_bpp) ||
	    (dst->format->BytesPerPixel != stretch_array->dst_bpp))
	{
		/* Re-calculate the stretch array */
		if (stretch_recalculate_array_sdl(src, src_rect.w, src_rect.h,
			dst, dst_rect.w, dst_rect.h)) return (1);
	}


	/* Point to the left-top corner of the source pixel data */
	ps0 = (Uint8 *)src->pixels + (src_rect.y * src->pitch) + (src_rect.x * src_bpp);

	/* Point to the left-top corner of the destination pixel data */
	pd0 = (Uint8 *)dst->pixels + (dst_rect.y * dst->pitch) + (dst_rect.x * dst_bpp);

	/* Save the total size */
	size = stretch_array->size;

	/* Point directly to the pixel offset arrays */
	src_offset = stretch_array->src_pixel_offset;
	dst_offset = stretch_array->dst_pixel_offset;


	/* Lock the destination surface (if necessary) */
	if (SDL_MUSTLOCK(dst))
	{
		if (SDL_LockSurface(dst) < 0) return (1);
	}

	/* Handle all of the legal bit depths */
	switch (dst_bpp)
	{
		/* 256 colors -- 1 byte per pixel */
		case 1:
		{
			/* We want to transfer only one byte */
			Uint8 *ps1, *pd1;

			/* Stretch and blit the pixels */
			for (i = 0; i < size; i++)
			{
				/* Point to the source pixel */
				ps1 = ps0 + src_offset[i];

				/* Point to the destination pixel */
				pd1 = pd0 + dst_offset[i];

				/* Copy */
				*pd1 = *ps1;
			}
			break;
		}

		/* 15 and 16-bit color */
		case 2:
		{
			/* We want to transfer two bytes */
			Uint16 *ps1, *pd1;

			/* Stretch and blit the pixels */
			for (i = 0; i < size; i++)
			{
				/* Point to the source pixel */
				ps1 = (Uint16 *)(ps0 + src_offset[i]);

				/* Point to the destination pixel */
				pd1 = (Uint16 *)(pd0 + dst_offset[i]);

				/* Copy */
				*pd1 = *ps1;
			}
			break;
		}

		/* 24-bit color (not sure how to handle this efficiently) */
		case 3:
		{
			/* We want to transfer three bytes, but lack a suitable data type */
			Uint8 *ps1, *pd1;

			/* Stretch and blit the pixels */
			for (i = 0; i < size; i++)
			{
				/* Point to the source pixel (first byte) */
				ps1 = ps0 + src_offset[i];

				/* Point to the destination pixel (first byte) */
				pd1 = pd0 + dst_offset[i];

				/* Copy  XXX XXX */
				*pd1     = *ps1;
				*(pd1+1) = *(ps1+1);
				*(pd1+2) = *(ps1+2);
			}
			break;
		}

		/* 32-bit color */
		case 4:
		default:
		{
			/* We want to transfer four bytes */
			Uint32 *ps1, *pd1;

			/* Stretch and blit the pixels */
			for (i = 0; i < size; i++)
			{
				/* Point to the source pixel */
				ps1 = (Uint32 *)(ps0 + src_offset[i]);

				/* Point to the destination pixel */
				pd1 = (Uint32 *)(pd0 + dst_offset[i]);

				/* Copy */
				*pd1 = *ps1;
			}
			break;
		}
	}

	/* Unlock the surface */
	if (SDL_MUSTLOCK(dst)) SDL_UnlockSurface(dst);

	/* Success */
	return (0);
}




/*
 * Release a font and all of its associated data, except for the string
 * "font_want".  -LM-
 */
static void remove_font_sdl(window_type *win_ptr)
{
	/* Free the old pixel access array */
	FREE(win_ptr->font_id);

	/* Forget the pointer */
	win_ptr->font_id = NULL;

	/* Free the old name */
	(void)string_free(win_ptr->font_file);

	/* Forget it */
	win_ptr->font_file = NULL;
}



/*
 * There are 256 glyphs in a character set.  Not all will always be defined.
 */
#define NUM_GLYPHS   256


/*
 * Encode a font into data suitable for efficient text output.  -LM-
 *
 * Because "TTF_RenderText_Solid" is so slow, we cannot simply use it in
 * "Term_text_sdl()".  A better option is to pre-render each character in the
 * font to an individual SDL surface, use palette tweaking to change colors
 * (as done in the original Angband SDL port back in 2001), and blit from them
 * as necessary.  An earlier version of this code did just this.
 *
 * However, although much faster than on-demand rendering, this method is
 * still unacceptably slow, especially if the color depth is not 8-bit.  So
 * we now encode the font into a one-dimensional array.  The first few data
 * points are start and end position for each character; the rest are pre-
 * calculated pixel offsets.
 *
 * "Font" must contain a monospaced font.  It is optional, but highly
 * recommended, that all characters between 0 and 255, inclusive, be defined
 * (they can be blank).  Characters after 256 are ignored.  XXX XXX
 *
 * At present, we assume a bitmapped font (as opposed to TrueType), because I
 * know of no Truetype fonts designed specifically for Roguelikes.
 */
static int *create_glyph_surface(TTF_Font *font, int w, int h)
{
	SDL_Surface *temp_glyph;

	int i, v, x, y;

	/* Create a color for "white" */
	SDL_Color white_clr  = { 255, 255, 255,   0 };

	int *temp_array;
	int *font_array;

	/* The first pixel data is stored immediately after the character indexes */
	/* But we need an extra space to store the total pixel count */
	int num_pixels = NUM_GLYPHS + 1;

	Uint8 *p;


	/* Make a very large temporary holding bin for pixel offset data  XXX */
	C_MAKE(temp_array, NUM_GLYPHS * 1024, int);


	/* Render and encode all the characters */
	for (i = 0; i < NUM_GLYPHS; i++)
	{
		/* Render this glyph, in white, onto an 8-bit surface */
		temp_glyph = TTF_RenderGlyph_Solid(font, i, white_clr);

		/* Remember the current pixel array location */
		temp_array[i] = num_pixels;

		/* If glyph does not exist, no pixels get plotted */
		if (!temp_glyph) continue;

		/* Store this glyph into our temporary array */
		for (x = 0; x < temp_glyph->w; x++)
		{
			for (y = 0; y < temp_glyph->h; y++)
			{
				/* Access this pixel.  "pitch" is the pixel width in bytes of an SDL surface */
				p = (Uint8 *)temp_glyph->pixels + (y * temp_glyph->pitch) +
					(x * temp_glyph->format->BytesPerPixel);

				/* Ignore pixels without color data */
				if (!*p) continue;

				/* Precalculate some of the math needed to access this pixel on screen */
				v = (y * AppWin->pitch) + (x * AppWin->format->BytesPerPixel);

				/* And store it */
				temp_array[num_pixels++] = v;
			}
		}

		/* Erase the temporary surface */
		SDL_FillRect(temp_glyph, NULL, SDL_MapRGB(temp_glyph->format, 0, 0, 0));
	}

	/* Calculate and store access position data for the final character */
	temp_array[NUM_GLYPHS] = num_pixels;

	/* Make a pixel access array for this font */
	C_MAKE(font_array, num_pixels, int);

	/* Save the data gathered in our temporary array */
	for (i = 0; i < num_pixels; i++) font_array[i] = temp_array[i];

	/* Free the temporary glyph surface */
	SDL_FreeSurface(temp_glyph);

	/* Free the temporary pixel data */
	FREE(temp_array);

	/* Return the pixel access array */
	return (font_array);
}


/*
 * Change the font used in a Term window.
 *
 * This function returns zero only if everything succeeds.
 */
static errr change_font_sdl(window_type *win_ptr, char *path)
{
	int wid, hgt;

	char *base;

	TTF_Font *new_font;


	/* No path given */
	if (!path) return (1);

	/* Analyze font path, get width and height */
	base = analyze_font(path, &wid, &hgt);

	/* Get the new font */
	new_font = TTF_OpenFont(path, hgt);

	/* Handle errors */
	if (!new_font) return (1);


	/* We were unable to determine the font size */
	if (!wid || !hgt)
	{
		/* Calculate metrics using the letter "M" (traditional choice) */
		if (TTF_SizeText(new_font, "M", &wid, &hgt)) return (1);
	}

	/* The old glyph surface is active (do this AFTER we verify the new font) */
	if (win_ptr->font_id) SDL_FreeSurface(win_ptr->font_id);

	/* Free the old name */
	(void)string_free(win_ptr->font_file);


	/* Create a new glyph surface, assign a pointer to it */
	win_ptr->font_id = create_glyph_surface(new_font, wid, hgt);

	/* Save new font name */
	win_ptr->font_file = string_make(base);

	/* Close the font */
	TTF_CloseFont(new_font);


	/* Save the font size */
	win_ptr->font_wid = wid;
	win_ptr->font_hgt = hgt;

	/* Usually, also change the tile size */
	if ((win_ptr->Term_idx != TERM_MAP) || (!mode_is_graphical(arg_graphics)))
	{
		win_ptr->tile_wid = wid;
		win_ptr->tile_hgt = hgt;
	}

	/* Success */
	return (0);
}


/* Never request more than 246 colors  XXX XXX */
#define PAL_SIZE    246


/*
 * If the display is paletted (256 colors), we need to update the system
 * palette if colors change.
 *
 * If we are not using graphics, we simply include all the text colors in our
 * requested palette.  If we are using graphics, then we load the first 246
 * (256, less a few to accommodate the system) colors requested in the bitmap
 * palette, and then attempt to match all the text colors as best we can.  -LM-
 *
 * This function is never called before all windows are ready.
 */
static bool new_palette(void)
{
	int i, j;
	int clr_red, clr_green, clr_blue;

	/* A 256-color palette */
	SDL_Color colors[256];

	/* The screen is not paletted -- no need to do anything */
	if (!paletted) return (TRUE);

	/* Wipe our requested palette (make everything black) */
	(void)C_WIPE(colors, 256, SDL_Color);


#ifdef USE_GRAPHICS

	/* Access the bitmap if present */
	if (GraphSurface)
	{
		/* Point to the palette of the bitmap surface */
		SDL_Palette *BMP_Palette = GraphSurface->format->palette;

		/* Store the first PAL_SIZE bitmap palette entries */
		for (i = 0; i < PAL_SIZE; i++) colors[i] = BMP_Palette->colors[i];
	}

#endif /* USE_GRAPHICS */


	/* Save the game colors into the new palette as best we can */
	for (i = 0; i < MAX_COLORS; i++)
	{
		long delta = 1000000L;
		int drv, dgv, dbv;

		int best_match = -1;
		bool black_flag = FALSE;

		/* This text color has no index -- skip */
		if (!color_table[i].index_char) continue;

		/* Get RGB values for the current game color */
		clr_red = color_table[i].rv;
		clr_green = color_table[i].gv;
		clr_blue = color_table[i].bv;


		/* Scan the requested palette */
		for (j = 0; j < PAL_SIZE; j++)
		{
			/* The first black entry must be requested; the others are free for use */
			if ((!colors[i].r) && (!colors[i].g) && (!colors[i].b))
			{
				/* We already skipped a black entry, or are requesting black */
				if ((black_flag) || (i == TERM_DARK))
				{
					/* Accept immediately */
					best_match = j;
					break;
				}

				/* Otherwise, skip the first black */
				else
				{
					black_flag = TRUE;
					continue;
				}
			}

			/* Skip any palette entries already used */
			if (colors[i].unused) continue;

			/* Get difference in RGB values */
			drv = ABS(colors[j].r - clr_red);
			dgv = ABS(colors[j].g - clr_green);
			dbv = ABS(colors[j].b - clr_blue);

			/* If squared RGB difference is less, remember this color */
			if (delta > (long)drv * drv + dgv * dgv + dbv * dbv)
			{
				delta = (long)drv * drv + dgv * dgv + dbv * dbv;
				best_match = j;
			}

			/* Accept exact matches immediately */
			if (!delta) break;
		}

		/* We have a legal match */
		if (best_match >= 0)
		{
			/* Save the game color (overwrite any bitmap palette entry here  XXX) */
			colors[i].r = clr_red;
			colors[i].g = clr_green;
			colors[i].b = clr_blue;

#ifdef SUPPORT_GAMMA

			/* Gamma-correct the colors (this may not work...) */
			if (gamma_correction > 0)
			{
				colors[best_match].r = gamma_table[colors[best_match].r];
				colors[best_match].g = gamma_table[colors[best_match].g];
				colors[best_match].b = gamma_table[colors[best_match].b];
			}

#endif /* SUPPORT_GAMMA */

			/* Hack -- Do not overwrite this color any more */
			colors[best_match].unused = 1;
		}
	}

	/* Change both physical and logical palettes */
	(void)SDL_SetPalette(AppWin, (SDL_LOGPAL | SDL_PHYSPAL), colors, 0, PAL_SIZE);

	/* Assume at lease partial success  XXX */
	return (TRUE);
}



/*
 * Handle the multifarious ways things can go wrong in the below function by
 * setting graphics mode to extended font.
 */
static bool init_graphics_sdl_error(char *msg)
{
	/* Complain */
	plog(msg);

	/* Request extended font display */
	arg_graphics = GRAPHICS_FONT;

	/* Use it */
	return (init_graphics_hook(FALSE));
}

/*
 * Initialize graphics (for the map window)
 *
 * Return TRUE if we have changed the graphics mode, FALSE if not.
 *
 * This code needs some work.  At present, we stupidly use black as a trans-
 * parent color in graphics and white as a transparent color in masks, and
 * then apply both graphics and mask for each (foreground) pict drawn. A much
 * more intelligent method (on screens showing >= 16-bit color) would be to
 * create an alpha channel for the graphics surface, set the alpha of all
 * pixels shown as white in the mask to fully transparent, discard the mask,
 * and then turn on and off alpha processing as necessary.
 *
 * Failing this, we really need a more intelligent choice than black for our
 * transparent color.  A lot of coders use magenta (0xFF, 0, 0xFF).  We could
 * then get rid of that stupid mask.
 */
static bool init_graphics_sdl(bool verify_map)
{
#ifdef USE_GRAPHICS
	char buf[1024];
	int wid, hgt;
	int i;

	u32b black, white;
	bool use_mask = FALSE;

	SDL_Surface *converted;

	char filename[32];
	char name[80];
	char mask[80];

	window_type *win_ptr = &window_settings[use_special_map ? TERM_MAP : TERM_MAIN];


	/* We only allow bitmap graphics if the special map display is available */
	if ((!use_special_map) && (mode_is_graphical(arg_graphics))) return (FALSE);


	/* Free the bitmap surfaces (if any) */
	SDL_FreeSurface(GraphSurface);
	SDL_FreeSurface(MaskSurface);

	/* Clear all pointers to graphical surfaces */
	GraphSurface = NULL;
	MaskSurface = NULL;

	/* We usually use "Term_pict" only for high-bit attr/char pairs */
	if (term_map)
	{
		term_map->always_pict = FALSE;
		term_map->higher_pict = TRUE;
	}


	/* We are requesting a non-graphical mode */
	if (!mode_is_graphical(arg_graphics))
	{
		/* Get the font name */
		(void)my_strcpy(buf, win_ptr->font_file, sizeof(buf));

		/* Use the font to determine cell size */
		(void)analyze_font(buf, &win_ptr->tile_wid, &win_ptr->tile_hgt);

		/* Recalculate the map term window */
		calc_map_display();

		/* Nothing more to do */
		return (TRUE);
	}


	/* Get tile width and height */
	wid = graphics_data[arg_graphics].tile_wid;
	hgt = graphics_data[arg_graphics].tile_hgt;


	/* Verify that enough rows and columns will be present (but only if ready) */
	if ((verify_map) && (wid) && (hgt))
	{
		if ((((win_ptr->window_wid - win_ptr->border_left - win_ptr->border_right) / wid) <
			term_size_min[win_ptr->Term_idx][0]) ||
			(((win_ptr->window_hgt - win_ptr->border_top - win_ptr->border_bottom) / hgt) <
			term_size_min[win_ptr->Term_idx][1]))
		{
			return (init_graphics_sdl_error("Sorry, the graphics tiles requested are too large to fit in the map window."));
		}
	}

	/* Get the graphics file name */
	(void)my_strcpy(filename, graphics_data[arg_graphics].file_name, sizeof(filename));

	/* HACK -- ignore any part of the name starting with a '-'  XXX XXX */
	for (i = 0; filename[i]; i++)
	{
		if (filename[i] == '-')
		{
			filename[i] = '\0';
			break;
		}
	}


	/* Determine if these graphics use a mask */
	use_mask = (mode_is_masked(arg_graphics)) ? TRUE : FALSE;

	/* Build the names of the graphics and (optionally) the mask files */
	strcpy(name, format("%s.bmp", filename));
	if (use_mask) strcpy(mask, format("%sm.bmp", filename));
	else          mask[0] = '\0';

	/* Build the bitmap file path */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_GRAF, name);

	/* Load the graphics bitmap into a new surface */
	GraphSurface = SDL_LoadBMP(buf);

	/* Handle errors */
	if (!GraphSurface)
	{
		return (init_graphics_sdl_error(format("Cannot read bitmap file '%s'", buf)));
	}


	/* These graphics use a mask */
	if (use_mask)
	{
		/* Build the mask file path */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_GRAF, mask);

		/* Load the mask bitmap into a new surface */
		MaskSurface = SDL_LoadBMP(buf);

		/* Handle errors */
		if (!MaskSurface)
		{
			return (init_graphics_sdl_error(format("Cannot read bitmap file '%s'", buf)));
		}
	}

	/* Update palette (if needed) */
	if (!new_palette()) return (init_graphics_sdl_error("Cannot update palette"));


	/*
	 * Convert and add transparency to both the graphics and mask surfaces
	 *
	 * This code is tricky:  Blitting is (even!) slower unless done from two
	 * surfaces of the same color depth.  If we want run-length acceleration
	 * of same-color bands, we must request it *before* conversion or suffer
	 * a serious slowdown when blitting.  If we want to specify a transparent
	 * color, we must request it *after* conversion or the wrong color will be
	 * chosen.
	 */
	if (GraphSurface)
	{
		/* Request run-length acceleration */
		SDL_SetColorKey(GraphSurface, SDL_RLEACCEL, 0);

		/* Convert to video format */
		converted = SDL_DisplayFormat(GraphSurface);

		/* Handle errors */
		if (!converted)	return (init_graphics_sdl_error("Couldn't convert surface"));

		/* Forget the old graphics surface */
		SDL_FreeSurface(GraphSurface);

		/* Point to our converted surface */
		GraphSurface = converted;

		/* Build a color for "black" that matches the pixel depth of this surface */
		black = SDL_MapRGB(GraphSurface->format, 0, 0, 0);

		/* Use it as our transparent color  XXX XXX */
		SDL_SetColorKey(GraphSurface, SDL_SRCCOLORKEY, black);
	}
	if (MaskSurface)
	{
		/* Request run-length acceleration */
		SDL_SetColorKey(MaskSurface, SDL_RLEACCEL, 0);

		/* Convert to video format */
		converted = SDL_DisplayFormat(MaskSurface);

		/* Handle errors */
		if (!converted)	return (init_graphics_sdl_error("Couldn't convert surface"));

		/* Forget the old mask surface */
		SDL_FreeSurface(MaskSurface);

		/* Point to our converted surface */
		MaskSurface = converted;

		/* Build a color for "white" that matches the pixel depth of this surface */
		white = SDL_MapRGB(MaskSurface->format, 255, 255, 255);

		/* Use it as our transparent mask color */
		SDL_SetColorKey(MaskSurface, SDL_SRCCOLORKEY, white);
	}

	/* Save the new sizes in the map window */
	window_settings[TERM_MAP].tile_wid = wid;
	window_settings[TERM_MAP].tile_hgt = hgt;

	/* If the graphics are masked, then the map display should always use "Term_pict". */
	if ((MaskSurface) && (term_map))
	{
		term_map->always_pict = TRUE;
		term_map->higher_pict = FALSE;
	}

	/* Automatically use the largest available font that will fit */
	(void)use_largest_font(win_ptr, wid, hgt);


	/* Recalculate the map term window */
	calc_map_display();


#else /* USE_GRAPHICS */

	/* If we do not have graphics defined, cancel any graphics request */
	if (mode_is_graphical(arg_graphics))
	{
		use_graphics = arg_graphics = GRAPHICS_FONT;
	}

#endif /* USE_GRAPHICS */

	/* Result */
	return (TRUE);
}


/*
 * React to global changes
 */
static errr Term_xtra_sdl_react(void)
{
	int i;

	window_type *win_ptr;
	term *old = Term;

	byte rv, gv, bv;

	bool change = FALSE;


	/* Save the default colors */
	for (i = 0; i < MAX_COLORS; i++)
	{
		/* Extract desired values */
		rv = color_table[i].rv;
		gv = color_table[i].gv;
		bv = color_table[i].bv;

#ifdef SUPPORT_GAMMA

		if (gamma_correction > 0)
		{
			rv = gamma_table[rv];
			gv = gamma_table[gv];
			bv = gamma_table[bv];
		}

#endif /* SUPPORT_GAMMA */

		/* Activate changes */
		if (win_clr[i].r != rv)
		{
			win_clr[i].r = rv;
			change = TRUE;
		}
		if (win_clr[i].g != gv)
		{
			win_clr[i].g = gv;
			change = TRUE;
		}
		if (win_clr[i].b != bv)
		{
			win_clr[i].b = bv;
			change = TRUE;
		}
	}

	/* Activate the palette if needed */
	if (change) (void)new_palette();


#ifdef USE_SOUND

	/* The sound options have changed */
	if (use_sound != arg_sound)
	{
		/* Request sound */
		if ((arg_sound == SOUND_ONLY) || (arg_sound == SOUND_AND_MUSIC))
		{
			/* Already done */
			if (sounds_ready)
			{
				/* No need to do anything */
			}

			/* Load the prefs, activate sound if possible */
			else if (!load_sound_prefs())
			{
				if (arg_sound == SOUND_ONLY) arg_sound = SOUND_NONE;
				if (arg_sound == SOUND_AND_MUSIC) arg_sound = MUSIC_ONLY;
				plog("Cannot initialize sound!");
			}
		}

		/* Request music */
		if ((arg_sound == MUSIC_ONLY) || (arg_sound == SOUND_AND_MUSIC))
		{
			/* Already done */
			if (music_ready)
			{
				/* No need to do anything */
			}

			/* Load the prefs, activate music if possible */
			else if (!load_music_prefs())
			{
				if (arg_sound == MUSIC_ONLY) arg_sound = SOUND_NONE;
				if (arg_sound == SOUND_AND_MUSIC) arg_sound = SOUND_ONLY;
				plog("Cannot initialize music!");
			}
		}

		/* Turn off music */
		if ((arg_sound != MUSIC_ONLY) && (arg_sound != SOUND_AND_MUSIC))
		{
			/* Shut down the current music, if any */
			if (music_track)
			{
				if (use_sound) Mix_FadeOutMusic(0);
				Mix_FreeMusic(music_track);
			}
		}

		/* Change setting */
		use_sound = arg_sound;
	}

#endif /* USE_SOUND */


#ifdef USE_GRAPHICS

	/* Handle "arg_graphics" */
	if (use_graphics != arg_graphics)
	{
		/* Change the graphics mode */
		if (init_graphics_sdl(TRUE)) use_graphics = arg_graphics;

		/* On failure, cancel the request */
		else                         arg_graphics = use_graphics;
	}

#endif /* USE_GRAPHICS */


	/* Clean up sub-windows */
	for (i = TERM_SUBWINDOW; i < TERM_MAX; i++)
	{
		win_ptr = &window_settings[i];

		/* Window must be visible */
		if (!win_ptr->visible) continue;

		/* Update resized windows */
		if ((win_ptr->cols != term_data[win_ptr->Term_idx].cols) ||
		    (win_ptr->rows != term_data[win_ptr->Term_idx].rows))
		{
			/* Activate */
			(void)Term_activate(angband_term[i]);

			/* Resize the term */
			(void)Term_resize(win_ptr->cols, win_ptr->rows);

			/* Redraw the contents */
			(void)Term_redraw();
		}
	}

	/* Point to the standard or tall display */
	win_ptr = &window_settings[use_tall_display ? WINDOW_DISPLAY : TERM_MAIN];

	/* Update resized windows */
	if ((win_ptr->cols != term_data[win_ptr->Term_idx].cols) ||
		(win_ptr->rows != term_data[win_ptr->Term_idx].rows))
	{
		/* Activate */
		(void)Term_activate(term_main);

		/* Resize the term */
		(void)Term_resize(win_ptr->cols, win_ptr->rows);

		/* Redraw the contents */
		(void)Term_redraw();
	}

	/* Re-calculate the special map (always, if present) */
	if (use_special_map)
	{
		/* Re-calculate the special map */
		calc_map_display();

		/* Refresh map display (if the main screen is active) */
		if (!main_screen_inactive)
		{
			/* Activate the map display */
			(void)Term_activate(term_map);

			/* Redraw its contents */
			(void)Term_redraw();
		}
	}

	/* Toggle window active status (all standard windows) */
	for (i = 0; i < TERM_MAX; i++)
	{
		/* No term, or term is unavailable */
		if (!angband_term[i]) continue;

		/* Update its mapped flag based on window visibility */
		angband_term[i]->mapped_flag = (window_settings[i].visible);
	}


	/* Show the cursor only if it does anything */
	(void)SDL_ShowCursor(use_mouse ? SDL_ENABLE : SDL_DISABLE);


	/* Restore old term */
	(void)Term_activate(old);

	/* Success */
	return (0);
}


/*
 * Play WAV-format sounds.  Thanks to SDL_Mixer, this port can play sounds
 * while other parts of the game are running -- even other sounds.
 *
 * Sounds and music are two areas where this set of libraries truly excels.
 */
static void play_sound_sdl(SDL_UserEvent *request)
{
#ifdef USE_SOUND
	int i;
	char buf[1024];
	int channel = -1;

	/* Extract some information */
	int v = request->code;


	/* Sound disabled */
	if (!use_sound || !sounds_ready) return;

	/* Illegal sound */
	if ((v < 0) || (v >= MSG_MAX)) return;

	/* Count the samples for this sound */
	for (i = 0; i < SAMPLE_MAX; i++)
	{
		if (!sound_file[v][i]) break;
	}

	/* No samples available */
	if (i == 0) return;


	/* Get the first available channel */
	for (i = 0; i < max_channels; i++)
	{
		/* If this channel isn't playing something */
		if (!Mix_Playing(i))
		{
			/* Wipe any existing sound data */
			if (mix_chunks[i])
			{
				Mix_FreeChunk(mix_chunks[i]);
				mix_chunks[i] = NULL;
			}

			/* Accept this channel */
			channel = i;
			break;
		}
	}

	/* All the channels are playing -- do nothing  (shouldn't happen) */
	if (channel == -1) return;


	/* Choose a sound and build a path to its file */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_SOUND, sound_file[v][Rand_simple(i)]);

	/* Load the sound into memory, save it in our array */
	mix_chunks[i] = Mix_LoadWAV(buf);

	/* Play the sound once */
	if (mix_chunks[i]) (void)Mix_PlayChannel(channel, mix_chunks[i], 0);
#endif /* USE_SOUND */
}


/*
 * Play music.  Only one music channel can be open at a time.
 *
 * A request->code of -1 stops and clears the current music.
 */
static void play_music_sdl(SDL_UserEvent *request)
{
#ifdef USE_SOUND
	int i;
	char buf[1024];

	/* Extract some information */
	int v = request->code;
	int fade_in = 0;

	/* Shut down the current music, if any */
	if (music_track)
	{
		if (use_sound) Mix_FadeOutMusic(0);
		Mix_FreeMusic(music_track);
	}

	/* Music disabled */
	if (!use_sound || !music_ready) return;

	/* Illegal music, or music not present */
	if ((v < 0) || (v >= MUSIC_MAX)) return;

	/* Count the songs for this theme */
	for (i = 0; i < SAMPLE_MAX; i++)
	{
		if (!music_file[v][i]) break;
	}

	/* No songs available */
	if (i == 0) return;

	/* Choose a song and build a path to its file */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_MUSIC, music_file[v][Rand_simple(i)]);

	/* Load the requested music track */
	music_track = Mix_LoadMUS(buf);

	/* Fade in the requested music (allow fade-in, repeat forever) */
	(void)Mix_FadeInMusic(music_track, -1, fade_in);

	/* Save some variables for the jukebox */
	cur_music_start_time = SDL_GetTicks() / 1000;
	cur_music_theme = v;
	cur_song_length = 120;  /* No public song length info? */


#endif /* USE_SOUND */

	return;
}


/*
 * Handle the screen animation timer.  We can't call any application functions
 * here, due to thread conflicts, but we can post a message to the message
 * queue.
 */
static Uint32 animate_timer_sdl(Uint32 interval, void *param)
{
	SDL_Event ping;

	/* Wipe this event */
	WIPE(&ping, SDL_Event);

	/* This is a user event of type "timer" */
	ping.type = SDL_USEREVENT + USER_MSG_TIMER;

	/* Request the basic timer */
	ping.user.code = 1;

	/* Ping the message queue */
	SDL_PushEvent(&ping);

	/* Keep the timer going */
	return (interval);
}



/*
 * Functions needed by the "Term" code
 */



/*
 * Hack -- make a noise
 */
static errr Term_xtra_sdl_noise(void)
{
	/* We need a public-domain "beep" sound */
	return (0);
}


/*
 * Make a sound.  Sounds are handled through calls to the message queue (this
 * avoids tying up the application while the sound plays).
 */
static errr Term_xtra_sdl_sound(int v)
{
#ifdef USE_SOUND
	SDL_Event ping;

	/* Wipe this event */
	WIPE(&ping, SDL_Event);

	/* This is a user event of type "sound" */
	ping.type = SDL_USEREVENT + USER_MSG_SOUND;

	/* Store the sound index */
	ping.user.code = v;

	/* Ping the message queue */
	SDL_PushEvent(&ping);

	/* Return */
	return (0);

#endif /* USE_SOUND */

	/* Oops */
	return (1);
}


/*
 * Start or stop playing music.
 */
static errr Term_xtra_sdl_music(int v)
{
#ifdef USE_SOUND

	SDL_Event ping;

	int fade_in, fade_out;  /* These are not actually used yet */

	/* Get current time (in seconds) */
	int cur_time = (SDL_GetTicks() / 1000);

	/* Call the jukebox code -- return if nothing to do */
	if (!jukebox(&v, cur_time, &fade_in, &fade_out)) return (0);

	/* Wipe this event */
	WIPE(&ping, SDL_Event);

	/* This is a user event of type "music" */
	ping.type = SDL_USEREVENT + USER_MSG_MUSIC;

	/* Store the music index */
	ping.user.code = v;

	/* No fade in or out yet */
	(void)fade_in;
	(void)fade_out;

	/* Ping the message queue */
	SDL_PushEvent(&ping);

	/* Return */
	return (0);

#endif /* USE_SOUND */

	/* Oops */
	return (1);
}


/*
 * Given a position in the ISO Latin-1 character set, return
 * the correct character on this system.
 */
static byte Term_xchar_sdl(byte c)
{
	/* The SDL port uses the Latin-1 standard */
	return (c);
}


/*
 * Draw a cursor at (col, row), using a bitmapped cursor shape or a thick underline.
 */
static errr Term_curs_sdl(int col, int row)
{
	window_type *win_ptr = (window_type*)(Term->data);

	SDL_Rect dst_rect;

	/* Get left-top corner of window */
	int x0 = win_ptr->window_left + win_ptr->border_left;
	int y0 = win_ptr->window_top + win_ptr->border_top;


	/* We are using masked graphics, this is the map term, and the cursor is graphical */
	if ((MaskSurface) && (win_ptr->Term_idx == TERM_MAP) &&
	    (misc_graphics_info[PICT_CURSOR][0] & 0x80))
	{
		SDL_Rect src_rect;

		/* Size of bitmap (source) cell */
		src_rect.w = graphics_data[use_graphics].tile_wid;
		src_rect.h = graphics_data[use_graphics].tile_hgt;

		/* Size of window (destination) cell */
		dst_rect.w = win_ptr->tile_wid;
		dst_rect.h = win_ptr->tile_hgt;

		/* Get position of cursor graphic within the bitmap */
		src_rect.x = (misc_graphics_info[PICT_CURSOR][1] & 0x7F) * src_rect.w;
		src_rect.y = (misc_graphics_info[PICT_CURSOR][0] & 0x7F) * src_rect.h;

		/* Location of window cell */
		dst_rect.x = x0 + (col * dst_rect.w);
		dst_rect.y = y0 + (row * dst_rect.h);

		/* No need to stretch */
		if ((src_rect.w == dst_rect.w) && (src_rect.h == dst_rect.h))
		{
			/* If using a mask, mask out the tile */
			if (MaskSurface)
			{
				(void)SDL_BlitSurface(MaskSurface, &src_rect, AppWin, &dst_rect);
			}

			/* Copy from the bitmap to the window */
			(void)SDL_BlitSurface(GraphSurface, &src_rect, AppWin, &dst_rect);
		}

		/* Need to stretch */
		else
		{
			/* If using a mask, mask out the tile */
			if (MaskSurface)
			{
				SDL_SoftStretch(MaskSurface, &src_rect, AppWin, &dst_rect);
			}

			/* Copy and stretch from the bitmap to the window */
			SDL_SoftStretch(GraphSurface, &src_rect, AppWin, &dst_rect);
		}

		/* Update the screen */
		SDL_UpdateRect(AppWin, dst_rect.x, dst_rect.y, dst_rect.w, dst_rect.h);
	}

	/* We are using a box cursor (the map term always shows a box cursor) */
	else if ((cursor_shape) || (win_ptr->Term_idx == TERM_MAP))
	{
		/* Get a yellow color */
		u32b yellow = SDL_MapRGB(AppWin->format, win_clr[TERM_YELLOW].r, win_clr[TERM_YELLOW].g, win_clr[TERM_YELLOW].b);


		/* This is very crude code, but it's not worth using SDL_image just for this */

		/* Draw the rectangle */
		dst_rect = do_sdl_rect(x0 + (col * win_ptr->tile_wid),
				y0 + (row * win_ptr->tile_hgt), 1, win_ptr->tile_hgt);
		SDL_FillRect(AppWin, &dst_rect, yellow);

		dst_rect = do_sdl_rect(x0 + (col * win_ptr->tile_wid) + (win_ptr->tile_wid - 1),
				y0 + (row * win_ptr->tile_hgt), 1, win_ptr->tile_hgt);
		SDL_FillRect(AppWin, &dst_rect, yellow);

		dst_rect = do_sdl_rect(x0 + (col * win_ptr->tile_wid),
				y0 + (row * win_ptr->tile_hgt), win_ptr->tile_wid, 1);
		SDL_FillRect(AppWin, &dst_rect, yellow);

		dst_rect = do_sdl_rect(x0 + (col * win_ptr->tile_wid),
				y0 + (row * win_ptr->tile_hgt) + (win_ptr->tile_hgt - 1), win_ptr->tile_wid, 1);
		SDL_FillRect(AppWin, &dst_rect, yellow);


		/* Get the full cursor box */
		dst_rect = do_sdl_rect(x0 + (col * win_ptr->tile_wid),
				y0 + (row * win_ptr->tile_hgt), win_ptr->tile_wid, win_ptr->tile_hgt);

		/* Update the screen */
		SDL_UpdateRect(AppWin, dst_rect.x, dst_rect.y, dst_rect.w, dst_rect.h);
	}

	/* Otherwise, paint a thick underline */
	else
	{
		int thickness = (win_ptr->tile_hgt >= 12) ? 2 : 1;

		/* Get a yellow color */
		u32b yellow = SDL_MapRGB(AppWin->format, win_clr[TERM_YELLOW].r, win_clr[TERM_YELLOW].g, win_clr[TERM_YELLOW].b);

		/* Thick underline */
		dst_rect.x = x0 + (col * win_ptr->tile_wid);
		dst_rect.y = y0 + ((row+1) * win_ptr->tile_hgt) - thickness;
		dst_rect.w = win_ptr->tile_wid;
		dst_rect.h = thickness;

		/* Fill the rectangle */
		SDL_FillRect(AppWin, &dst_rect, yellow);

		/* Update the screen */
		SDL_UpdateRect(AppWin, dst_rect.x, dst_rect.y, dst_rect.w, dst_rect.h);
	}

	/* Success */
	return (0);
}


/*
 * Clear a terminal window
 */
static errr Term_xtra_sdl_clear(void)
{
	window_type *win_ptr = (window_type*)(Term->data);

	SDL_Rect rc;

	/* We do clear non-visible windows */

	/* If window is visible, notify of possible overlap */
	if (win_ptr->visible) overlap_notify(0, 0, 999, 999);

	/* Select the entire terminal window */
	rc.x = win_ptr->window_left + win_ptr->border_left;
	rc.y = win_ptr->window_top + win_ptr->border_top;
	rc.w = win_ptr->window_wid - win_ptr->border_left - win_ptr->border_right;
	rc.h = win_ptr->window_hgt - win_ptr->border_top - win_ptr->border_bottom;


	/* Fill the rectangle */
	SDL_FillRect(AppWin, &rc, black_clr);

	/* Update the screen */
	SDL_UpdateRect(AppWin, rc.x, rc.y, rc.w, rc.h);

	/* Success */
	return (0);
}


/*
 * Erase a "block" of "n" characters starting at (col, row).
 */
static errr Term_wipe_sdl(int col, int row, int n)
{
	window_type *win_ptr = (window_type*)(Term->data);

	SDL_Rect rc;

	int x0 = win_ptr->window_left + win_ptr->border_left;
	int y0 = win_ptr->window_top  + win_ptr->border_top;


	/* Do nothing unless this window is visible */
	if (!win_ptr->visible) return (0);

	/* Store redraw area (if necessary) */
	overlap_notify(col, row, col + n - 1, row);

	/* Rectangle to erase in client coords */
	rc.x = x0 + (col * win_ptr->tile_wid);
	rc.y = y0 + (row * win_ptr->tile_hgt);
	rc.w = (n * win_ptr->tile_wid);
	rc.h = win_ptr->tile_hgt;

	/* Fill the rectangle */
	SDL_FillRect(AppWin, &rc, black_clr);

	/* Add this update rect to our queue */
	COPY(&update_rects[update_rects_num++], &rc, SDL_Rect);

	/* Hack -- flush the update queue when full */
	if (update_rects_num == MAX_UPDATE_RECTS) flush_update_rects();

	/* Success */
	return (0);
}



/* Allow centering and opaquing of text */
static bool text_centered = FALSE;
static bool text_opaque = TRUE;

/*
 * Low level text output.  Assumes valid input.  -LM-
 *
 * Draw several ("n") chars, with an attr, at a given location.  By default,
 * characters are drawn aligned to the left top corner of the cell and with
 * opaque backgrounds.  You may change either of these assumptions for a
 * single call to this function by setting the above variables.
 *
 * At present, we only handle bitmapped fonts.
 *
 * We assume that font size will never be larger than tile size (this should
 * be verified by the font initialization code).
 *
 * This code has gone through dozens of iterations in the quest for acceptable
 * speed and has finally gotten to the point where only the call to
 * "flush_update_rects()" takes much time.  Nevertheless, it remains the
 * slowest implementation of "Term_text" I have ever written.  *frustrated*
 */
static errr Term_text_sdl(int col, int row, int n, byte a, cptr s)
{
	window_type *win_ptr = (window_type*)(Term->data);

	SDL_Rect full_rect;

	int i, j;
	int x, y;
	int start, end;
	int *font = win_ptr->font_id;

	Uint32 text_color;
	Uint8 *pd0;
	Uint8 bytes_per_pixel = AppWin->format->BytesPerPixel;


	/* Do nothing unless this window is visible */
	if (!win_ptr->visible) return (0);

	/* Store redraw area (if necessary) */
	overlap_notify(col, row, col + n - 1, row);


	/* Build a text color in the current pixel format */
	text_color = SDL_MapRGB(AppWin->format, win_clr[a].r, win_clr[a].g, win_clr[a].b);


	/* Destination left-top pixel */
	x = win_ptr->window_left + win_ptr->border_left + (col * win_ptr->tile_wid);
	y = win_ptr->window_top  + win_ptr->border_top  + (row * win_ptr->tile_hgt);

	/* Remember the total area covered */
	full_rect.x = x;
	full_rect.y = y;
	full_rect.w = win_ptr->tile_wid * n;
	full_rect.h = win_ptr->tile_hgt;

	/* Opaque text needs a black background */
	if (text_opaque) SDL_FillRect(AppWin, &full_rect, black_clr);

	/* Allow centering (only works if text is not larger than tile) */
	if (text_centered)
	{
		/* Shift the top-left corner to center the whole string */
		if (win_ptr->tile_wid > win_ptr->font_wid)
			x += (win_ptr->tile_wid - win_ptr->font_wid) / 2;
		if (win_ptr->tile_hgt > win_ptr->font_hgt)
			y += (win_ptr->tile_hgt - win_ptr->font_hgt) / 2;
	}

	/* Lock the application surface (if necessary) */
	if (SDL_MUSTLOCK(AppWin))
	{
		if (SDL_LockSurface(AppWin) < 0) return (1);
	}

	/* Point to the left-top corner of the screen pixel data for this tile */
	pd0 = (Uint8 *)AppWin->pixels + (y * AppWin->pitch) + (x * bytes_per_pixel);

	/* Advance along the string.  Adjust pixel pointer one tile per character. */
	for (i = 0; i < n; i++, pd0 += (win_ptr->tile_wid * bytes_per_pixel))
	{
		/* Translate the character */
		byte c = *(s + i);

		/* Get the first and last pixel locator we need to plot */
		start = font[c];
		end   = font[c + 1];

		/* Handle all of the legal bit depths */
		switch (bytes_per_pixel)
		{
			/* 256 colors -- pixel data is 8-bit */
			case 1:
			{
				/*
				 * Use entries in the pixel location array to jump to and
				 * change pixels to our desired color.
				 */
				for (j = start; j < end; j++) *(Uint8*)(pd0 + font[j]) = (Uint8)text_color;
				break;
			}

			/* 15 and 16-bit color (SDL handles the details) */
			case 2:
			{
				for (j = start; j < end; j++) *(Uint16*)(pd0 + font[j]) = (Uint16)text_color;
				break;
			}

			/* 32-bit color is easy.  24-bit color needs more testing. */
			case 3:
			case 4:
			default:
			{
				for (j = start; j < end; j++) *(Uint32*)(pd0 + font[j]) = text_color;
				break;
			}
		}
	}

	/* Unlock the surface */
	if (SDL_MUSTLOCK(AppWin)) SDL_UnlockSurface(AppWin);

	/* Add this area to our update queue */
	COPY(&update_rects[update_rects_num++], &full_rect, SDL_Rect);

	/* Hack -- flush the update queue when full */
	if (update_rects_num == MAX_UPDATE_RECTS) flush_update_rects();


	/* Cancel centering, assume opaquing */
	text_centered = FALSE;
	text_opaque = TRUE;

	/* Return */
	return (0);
}



/*
 * Helper function that prints a centered character.  Optionally, the character
 * may also be transparent (without background).  -LM-
 */
static errr Term_text_special_sdl(int col, int row, const byte *a, const char *c,
	bool transparent)
{
	/* Build a "string"  XXX XXX */
	char s[2];
	s[0] = *c;
	s[1] = '\0';

	/* Set text options */
	text_centered = TRUE;
	text_opaque = !transparent;

	/* Print the text */
	return (Term_text_sdl(col, row, 1, *a, s));
}



/*
 * Low level graphics.  Assumes valid input.
 *
 * We request graphics by setting the high bit of both "attr" and "char" and
 * then using the "Term_higher_pict".
 * Graphic data is stored in large bitmaps with many rows and columns:  attr
 * (less the high bit) gets us the row, char (less the high bit) gets us the
 * column.
 *
 * This function also handles text output if transparency is on ("Term_always_
 * pict" is set to TRUE).  This allows us to have centered letters that appear
 * on a graphical background.
 *
 * If graphics are not available we simply wipe the given grids.
 *
 * This code needs some efficiency tweaks.  A good place to start would be to
 * lose the mask.   XXX XXX XXX
 */
static errr Term_pict_sdl(int col, int row, int n, const byte *ap, const char *cp,
	const byte *tap, const char *tcp)
{
	window_type *win_ptr = (window_type*)(Term->data);

#ifdef USE_GRAPHICS

	int i;

	SDL_Rect src_rect, dst_rect, full_rect;

	/* Assume no stretching or shrinking */
	bool do_stretch = FALSE;


	/* Do nothing unless this window is visible */
	if (!win_ptr->visible) return (0);

	/* Paranoia -- Require a graphics surface */
	if (!GraphSurface) return (Term_wipe_sdl(col, row, n));


	/* Store redraw area (if necessary) */
	overlap_notify(col, row, col + n - 1, row);


	/* Size of bitmap (source) cell */
	src_rect.w = graphics_data[use_graphics].tile_wid;
	src_rect.h = graphics_data[use_graphics].tile_hgt;

	/* Size of window (destination) cell */
	dst_rect.w = win_ptr->tile_wid;
	dst_rect.h = win_ptr->tile_hgt;

	/* Location of first window cell */
	dst_rect.x = win_ptr->window_left + win_ptr->border_left + (col * dst_rect.w);
	dst_rect.y = win_ptr->window_top  + win_ptr->border_top  + (row * dst_rect.h);

	/* Remember the total area covered */
	full_rect.x = dst_rect.x;
	full_rect.y = dst_rect.y;
	full_rect.w = win_ptr->tile_wid * n;
	full_rect.h = win_ptr->tile_hgt;


	/* We may need to stretch or shrink the graphics */
	if ((dst_rect.w != src_rect.w) || (dst_rect.h != src_rect.h))
	{
		do_stretch = TRUE;
	}

	/* Wipe the grid (shouldn't be necessary)  XXX */
	SDL_FillRect(AppWin, &full_rect, black_clr);


	/* Draw attr/char pairs.  Advance one tile per pair. */
	for (i = 0; i < n; i++, dst_rect.x += dst_rect.w)
	{
		/* If we are using masked graphics, */
		if (MaskSurface)
		{
			/* Handle background text (attr is < 128) */
			if (!(tap[i] & 0x80))
			{
				/* Centered text output with black background */
				if (Term_text_special_sdl(col + i, row, tap + i, tcp + i, FALSE)) return (1);
			}

			/* Handle background graphics (usual case) */
			else
			{
				/* Get the terrain/background graphic */
				src_rect.x = (tcp[i] & 0x7F) * src_rect.w;
				src_rect.y = (tap[i] & 0x7F) * src_rect.h;

				/* No need to stretch */
				if (!do_stretch)
				{
					/* Copy from the bitmap to the window */
					(void)SDL_BlitSurface(GraphSurface, &src_rect, AppWin, &dst_rect);
				}

				/* Need to stretch */
				else
				{
					/* Copy and stretch from the bitmap to the window */
					if (sdl_stretch(GraphSurface, src_rect, AppWin, dst_rect)) return (1);
				}
			}

			/* If foreground is the same as background, we're done */
			if ((tap[i] == ap[i]) && (tcp[i] == cp[i])) continue;
		}

		/* Handle foreground text (attr is <= 127) */
		if (!(ap[i] & 0x80))
		{
			/* Centered text output (transparent if "use_mask" is TRUE) */
			if (Term_text_special_sdl(col + i, row, ap + i, cp + i, (MaskSurface != NULL))) return (1);
		}

		/* Handle foreground graphics */
		else
		{
			/* Get the foreground graphic */
			src_rect.x = (cp[i] & 0x7F) * src_rect.w;
			src_rect.y = (ap[i] & 0x7F) * src_rect.h;

			/* No need to stretch */
			if (!do_stretch)
			{
				/* If using a mask, mask out the tile */
				if (MaskSurface)
				{
					(void)SDL_BlitSurface(MaskSurface, &src_rect, AppWin, &dst_rect);
				}

				/* Copy from the bitmap to the window */
				(void)SDL_BlitSurface(GraphSurface, &src_rect, AppWin, &dst_rect);
			}

			/* Need to stretch */
			else
			{
				/* If using a mask, mask out the tile */
				if (MaskSurface)
				{
					if (sdl_stretch(MaskSurface, src_rect, AppWin, dst_rect)) return (1);
				}

				/* Copy and stretch from the bitmap to the window */
				if (sdl_stretch(GraphSurface, src_rect, AppWin, dst_rect)) return (1);
			}
		}
	}


	/* Add this area to our update queue */
	COPY(&update_rects[update_rects_num++], &full_rect, SDL_Rect);

	/* Hack -- flush the update queue when full */
	if (update_rects_num == MAX_UPDATE_RECTS) flush_update_rects();

	/* Success */
	return (0);


#else /* USE_GRAPHICS */

	/* If graphics are undefined, just erase this grid */
	return (Term_wipe_sdl(col, row, n));

#endif /* USE_GRAPHICS */
}




/*
 * Print the screen to file.  -Gregory Velichansky (hmaon@bumba.net)
 *
 * On multi-user systems, the file ought to be saved in the user's Sangband
 * directory.
 */
static void sdl_print_screen(void)
{
	int i;
	char buf[1024];
	char file[1024];
	FILE *tmp;


	/* If we are on a multi-user system, we must save in the user directory */
#ifdef PRIVATE_USER_PATH
	/* Build the filename (this doesn't work; not sure why) */
	(void)path_build(file, sizeof(file), ANGBAND_DIR_USER, "newshot.bmp");

	/* Stop-gap XXX */
	(void)my_strcpy(file, "newshot.bmp", sizeof(file));
#else
	/* Otherwise, we simply save to the working directory */
	(void)my_strcpy(file, "newshot.bmp", sizeof(file));
#endif

	/* Grab permissions */
	safe_setuid_grab();

	/* Save the application surface to a bitmap */
	if (SDL_SaveBMP(AppWin, file))
	{
		plog(format("You fail to get a screenshot (trying to save to %s)!", file));
		return;
	}

	/* Drop permissions */
	safe_setuid_drop();

	/* Name screen shots in consecutive order */
	for (i = 1; i < 999; i++)
	{
		/* Save this index as a name */
		(void)strnfmt(buf, sizeof(buf), "scrn%03d.bmp", i);

		/* Try this name */
		tmp = fopen(buf, "rb");

		/* File already exists - don't overwrite */
		if (tmp)
		{
			fclose(tmp);
			continue;
		}

		/* Rename the file */
		rename("newshot.bmp", buf);

		/* XXX XXX XXX Make a sound here */

		/* All done */
		break;
	}
}

/*
 * Handle keypresses.
 *
 * We treat left and right modifier keys as equivalent.
 * We ignore any key without a valid SDLK index.
 */
static void sdl_keypress(SDL_keysym keysym)
{
	u16b key_code = keysym.unicode;
	SDLKey key_sym = keysym.sym;

	/* Store the value of various modifier keys */
	bool mc = (bool)((keysym.mod & (KMOD_CTRL)) != KMOD_NONE);
	bool ms = (bool)((keysym.mod & (KMOD_SHIFT)) != KMOD_NONE);
	bool ma = (bool)((keysym.mod & (KMOD_ALT)) != KMOD_NONE);
	bool mm = (bool)((keysym.mod & (KMOD_META)) != KMOD_NONE);


	/* Ignore if main term is not initialized */
	if (!term_main) return;

	/* Handle print screen */
	if (key_sym == SDLK_PRINT)
	{
		sdl_print_screen();
		return;
	}

	/* Ignore various keypress, including pure modifier keys */
	if (ignore_key[key_sym]) return;

	/*
	 * If the keycode is 7-bit ASCII (except numberpad), and ALT and META are not
	 * pressed, send it directly to the application.
	 */
	if ((key_code) && !(key_code & (0xFF80)) && !is_numpad(key_sym) && !ma && !mm)
	{
		(void)Term_keypress(key_code);
	}

	/* Handle all other valid SDL keys */
	else if (key_sym < SDLK_LAST)
	{
		char buf[80];
		int i;

		/* Begin the macro trigger */
		(void)Term_keypress(31);

		/* Send the modifiers */
		if (mc) (void)Term_keypress('C');
		if (ms) (void)Term_keypress('S');
		if (ma) (void)Term_keypress('A');
		if (mm) (void)Term_keypress('M');

		/* Build the SDL key name */
		(void)my_strcpy(buf, format("[%s]", SDL_GetKeyName(key_sym)), 80);

		/* Convert and store key name */
		for (i = 0; buf[i]; i++)
		{
			/* Make lowercase */
			buf[i] = tolower(buf[i]);

			/* Replace spaces with underscores */
			if (buf[i] == ' ') buf[i] = '_';

			/* Add to the keyqueue */
			(void)Term_keypress(buf[i]);
		}
	}
}


/*
 * Handle a single message sent to the application.
 *
 * Functions that are either called from a separate thread or which need to
 * create a separate thread (such as sounds) need to pass messages to this
 * function in order to execute most operations.  See the useage of
 * "SDL_USEREVENT".
 */
static errr sdl_HandleEvent(SDL_Event *event)
{
	/* Handle message */
	switch (event->type)
	{
		/* Keypresses */
		case SDL_KEYDOWN:
		{
			/* Hack -- resume playing music (see below) */
			Mix_ResumeMusic();

			/* Handle keypress */
			sdl_keypress(event->key.keysym);

			break;
		}

		/* Mouse movement and button down */
		case SDL_MOUSEMOTION:
		case SDL_MOUSEBUTTONDOWN:
		{
			/* Wipe the data storage */
			mouseaction_type this_mouse_action = { 0, 0, 0, 0 };

			/* Ignore if not using the mouse */
			if (!use_mouse) break;

			/* Ignore if no active window */
			if (!Term) break;

			/* Right-clicks trigger GUI interaction (if legal) */
			if ((event->type == SDL_MOUSEBUTTONDOWN) && (event->button.button == SDL_BUTTON_RIGHT))
			{
				/* If waiting for a command, activate the GUI options */
				if ((inkey_flag) && (!screen_depth))
				{
					screen_save(FALSE);
					(void)Term_user_gui(0);
					screen_load();

					/* Redraw everything */
					do_cmd_redraw();
				}
				return (0);
			}

			/* In wheel movement all that matters is "up" or "down" */
			if ((event->type == SDL_MOUSEBUTTONDOWN) &&
			    ((event->button.button == SDL_BUTTON_WHEELUP) ||
			     (event->button.button == SDL_BUTTON_WHEELDOWN)))
			{
				/* Require significant movement (windows standard) */
				if (event->button.button == SDL_BUTTON_WHEELUP)
					this_mouse_action.y = 0;
				if (event->button.button == SDL_BUTTON_WHEELDOWN)
					this_mouse_action.y = 255;
			}

			/* Other actions require that we save position in a specific Term */
			else
			{
				int term, col, row;

				/* Extract the mouse coordinates */
				int xPos;
				int yPos;

				/* Handle both mouse movement and clicks */
				if (event->type == SDL_MOUSEMOTION)
				{
					xPos = event->motion.x;
					yPos = event->motion.y;
				}
				else
				{
					xPos = event->button.x;
					yPos = event->button.y;
				}

				/* Get the term and grid position in which these coordinates lie */
				get_term_position(&term, &col, &row, xPos, yPos, TRUE);

				/* Ignore positions not within any term */
				if ((term >= TERM_MAX) && (term < 0)) break;

				/* If this is just a mouse movement, then only save if different */
				if ((event->type == SDL_MOUSEMOTION) &&
				    (col == prev_mouse_action.x) && (row == prev_mouse_action.y))
				{
					break;
				}

				/* Remember the term */
				this_mouse_action.term = term;

				/* Remember the current position (only for these kinds of actions) */
				prev_mouse_action.x = this_mouse_action.x = col;
				prev_mouse_action.y = this_mouse_action.y = row;
			}

			/* We have used the mouse */
			(void)Term_keypress(MOUSEKEY);

			/* Store the action type */
			if (event->type != SDL_MOUSEBUTTONDOWN)
			{
				(void)Term_keypress(MOUSE_MOVEONLY);
			}
			else
			{
				/* Get elapsed time since the last mouse click */
				u32b ticks = SDL_GetTicks() - mouseclick_timer;

				if (event->button.button == SDL_BUTTON_LEFT)
				{
					/* Recognize double-clicks (needs to be made configurable) */
					if ((ticks > 100) && (ticks <= 750))
						(void)Term_keypress(MOUSE_L_DBLCLICK);
					else
						(void)Term_keypress(MOUSE_L_CLICK);

					/* Reset the timer */
					mouseclick_timer = SDL_GetTicks();
				}
				if (event->button.button == SDL_BUTTON_RIGHT)
				{
					/* Recognize double-clicks (needs to be made configurable) */
					if ((ticks > 100) && (ticks <= 750))
						(void)Term_keypress(MOUSE_R_DBLCLICK);
					else
						(void)Term_keypress(MOUSE_R_CLICK);

					/* Reset the timer */
					mouseclick_timer = SDL_GetTicks();
				}
				else if ((event->button.button == SDL_BUTTON_WHEELUP) ||
				         (event->button.button == SDL_BUTTON_WHEELDOWN))
				{
					(void)Term_keypress(MOUSE_WHEEL);
				}
			}

			/* Store the X, Y position */
			(void)Term_keypress(this_mouse_action.x);
			(void)Term_keypress(this_mouse_action.y);

			/* Store the flags */
			(void)Term_keypress(this_mouse_action.term);

			break;
		}

		/* Change application visibility */
		case SDL_ACTIVEEVENT:
		{
#ifdef USE_SOUND
			/*
			 * The input focus and appactive messages are only received sporadically,
			 * so we are left with the choice of never turning off the music, or of
			 * turning it off whenever the mouse leaves the application window.
			 */

			/* Pause and unpause music as needed */
			if (event->active.gain == 0) Mix_PauseMusic();
			else Mix_ResumeMusic();
#endif /* USE_SOUND */
			break;
		}


		/* Shut down the game */
		case SDL_QUIT:
		{
			/* We are playing a game with an active character, and are not using the infinite lives cheat */
			if ((character_generated) && (!beginner_play) && (inkey_flag))
			{
				/* Hack -- Forget messages */
				msg_flag = FALSE;

				/* Save the game */
				do_cmd_save_game(FALSE);
			}
			quit(NULL);
			break;
		}

		/* Resize the application */
		case SDL_VIDEORESIZE:
		{
			/* Resize the application surface */
			(void)SDL_SetVideoMode(event->resize.w, event->resize.h, 0, 0);
			break;
		}

		/* User events */
		case SDL_USEREVENT + USER_MSG_TIMER:
		case SDL_USEREVENT + USER_MSG_SOUND:
		case SDL_USEREVENT + USER_MSG_MUSIC:
		{
			/* A timer has gone off */
			if (event->type == SDL_USEREVENT + USER_MSG_TIMER)
			{
				/* Basic map animation */
				if (event->user.code == 1) map_animate();
			}

			/* We want to make a sound */
			else if (event->type == SDL_USEREVENT + USER_MSG_SOUND)
			{
				/* Feed the user event data to the sound function */
				play_sound_sdl(&(event->user));
			}

			/* Time to play music */
			else if (event->type == SDL_USEREVENT + USER_MSG_MUSIC)
			{
				/* Feed the user event data to the music function */
				play_music_sdl(&(event->user));
			}

			break;
		}

		/* All other possible events */
		default:
		{
			/* Do nothing */
			break;
		}
	}

	return (0);
}


/*
 * Process at least one event
 */
static errr Term_xtra_sdl_event(int v)
{
	SDL_Event event;
	errr error = 0;

	/* Wait for an event */
	if (v)
	{
		/* Wait for an event */
		if (SDL_WaitEvent(&event))
		{
			/* Handle it */
			error = sdl_HandleEvent(&event);
		}
		else return (1);
	}

	/* Check for an event */
	else
	{
		/* Get a single pending event */
		if (SDL_PollEvent(&event))
		{
			/* Handle it */
			error = sdl_HandleEvent(&event);
		}
	}

	/* Note success or failure */
	return (error);
}


/*
 * Process all pending events
 */
static errr Term_xtra_sdl_flush(void)
{
	SDL_Event event;

	/* Get all pending events */
	while (SDL_PollEvent(&event))
	{
		/* Handle them (ignore errors) */
		(void)sdl_HandleEvent(&event);
	}

	/* Done */
	return (0);
}

/*
 * Delay for "x" milliseconds
 */
static errr Term_xtra_sdl_delay(int v)
{
	/* Sleep */
	if (v > 0)
	{
		(void)Term_xtra_sdl_event(0);
		SDL_Delay(v);
	}

	/* Success */
	return (0);
}



/*
 * Do a "special thing"
 */
static errr Term_xtra_sdl(int n, int v)
{
	/* Handle a subset of the legal requests */
	switch (n)
	{
		/* Process an event */
		case TERM_XTRA_EVENT:
		{
			return (Term_xtra_sdl_event(v));
		}

		/* Flush all events */
		case TERM_XTRA_FLUSH:
		{
			return (Term_xtra_sdl_flush());
		}

		/* Clear the screen */
		case TERM_XTRA_CLEAR:
		{
			return (Term_xtra_sdl_clear());
		}

		/* Show or hide the cursor */
		case TERM_XTRA_SHAPE:
		{
			int x, y;

			/* Obtain the cursor */
			(void)Term_locate(&x, &y);

			/* Show or hide the cursor */
			(void)Term_curs_sdl(x, y);
			return (0);
		}

		/* Refresh the screen */
		case TERM_XTRA_FRESH:
		{
			/* Get the window for the active term */
			window_type *win_ptr = (window_type*)(Term->data);

			/* Update the display */
			flush_update_rects();

			/* Do special refresh maintenance */
			if      (win_ptr->Term_idx == TERM_MAIN) return (Term_fresh_gui(v));
			else if (win_ptr->Term_idx == TERM_MAP)  return (Term_fresh_gui(v));
			else                                     return (0);
		}

		/* Make a bell sound */
		case TERM_XTRA_NOISE:
		{
			return (Term_xtra_sdl_noise());
		}

		/* Make a special sound */
		case TERM_XTRA_SOUND:
		{
			return (Term_xtra_sdl_sound(v));
		}

		/* Process random events */
		case TERM_XTRA_BORED:
		{
			return (Term_xtra_sdl_event(0));
		}

		/* React to global changes */
		case TERM_XTRA_REACT:
		{
			return (Term_xtra_sdl_react());
		}

		/* Delay for some milliseconds */
		case TERM_XTRA_DELAY:
		{
			return (Term_xtra_sdl_delay(v));
		}

		/* Remove region covered by overlapping term from area to be refreshed */
		case TERM_XTRA_OVLAP:
		{
			return (Term_xtra_gui_overlap());
		}

		/* Play music */
		case TERM_XTRA_MUSIC:
		{
			return (Term_xtra_sdl_music(v));
		}
	}

	/* Oops */
	return (1);
}




/*
 * Create and initialize the Term contined within this window.
 */
void term_data_link_sdl(window_type *win_ptr)
{
	term *t = &term_data[win_ptr->Term_idx];

	/* Initialize the term */
	(void)term_init(t, win_ptr->cols, win_ptr->rows, win_ptr->keys);

	/* Use a "software" cursor */
	t->soft_cursor = TRUE;

	/*
	 * Usually (but see "init_graphics_sdl") use "Term_pict" for graphic
	 * data and "Term_text" for text
	 */
	t->higher_pict = TRUE;

	/* Erase with "white space" */
	t->attr_blank = TERM_WHITE;
	t->char_blank = ' ';

	/* Never refresh one row */
	t->never_frosh = TRUE;

	/* Ignore the init/nuke hooks */

	/* Prepare the template hooks */
	t->user_hook = Term_user_gui;
	t->xtra_hook = Term_xtra_sdl;
	t->curs_hook = Term_curs_sdl;
	t->wipe_hook = Term_wipe_sdl;
	t->text_hook = Term_text_sdl;
	t->pict_hook = Term_pict_sdl;
	t->xchar_hook = Term_xchar_sdl;

	/* Allow special refresh updates (to handle overlapping terms)  XXX */
	if (win_ptr->Term_idx == TERM_MAIN) t->fresh_hook = Term_fresh_gui;
	if (win_ptr->Term_idx == TERM_MAP)  t->fresh_hook = Term_fresh_gui;

	/* Remember where we came from */
	t->data = win_ptr;
}


/*
 * Create the windows.
 *
 * First, instantiate the "default" values, then read the initialization file
 * to over-ride selected values, then create the windows.
 */
static void init_windows(const SDL_VideoInfo *VideoInfo)
{
	int i, j;
	int cols, rows;
	int tries = 0;
	u32b flags;

	window_type *win_ptr;

	char buf[1024];

	int wid_tmp, hgt_tmp;


	/* Save screen width and height */
	screen_w = VideoInfo->current_w;
	screen_h = VideoInfo->current_h;

	/* Get information we need before loading windows */
	load_prefs(TRUE);

	/* Search defaults for the closest match to our application size */
	seek_app_size_defaults(&wid_tmp, &hgt_tmp, app_wid, app_hgt);


	/* If we don't find our display mode, try, try again */
	resolution_retry:


	/* Get defaults (if any) for each game Term (plus 1 special window) */
	for (i = 0; i < TERM_MAX + 1; i++)
	{
		/* Note:  We use and save default screen sizes, not actual sizes */

		/* Search for a legal window definition */
		for (j = 0; j < NUM_WINDOW_DEFAULTS; j++)
		{
			/* Ignore windows not of our index */
			if (window_defaults[j].Term_idx != i) continue;

			/* Require correct resolution and display mode */
			if ((window_defaults[j].resolution_x == wid_tmp) &&
			    (window_defaults[j].resolution_y == hgt_tmp) &&
			    (window_defaults[j].mode == cur_display_mode))
			{
				/* Copy it to the settings array */
				COPY(&window_settings[i], &window_defaults[j], window_type);
				break;
			}

			/* Allow default windows for this term */
			else if ((window_defaults[j].resolution_x <= 0) &&
			         (window_defaults[j].resolution_y <= 0))
			{
				/* Copy it to the settings array */
				COPY(&window_settings[i], &window_defaults[j], window_type);

				/* Save our monitor size */
				window_settings[i].resolution_x = wid_tmp;
				window_settings[i].resolution_y = hgt_tmp;

				/* Save our display setting */
				window_settings[i].mode = cur_display_mode;

				/* Allow default font requests */
				if ((!window_settings[i].font_want) && (i > TERM_MAIN))
				{
					window_settings[i].font_want = window_settings[TERM_MAIN].font_want;
				}
				break;
			}
		}
	}

	/* Modify various things by reading the initialization file */
	load_prefs(FALSE);


	/* Our main window is undefined!  Help! */
	if (!window_settings[TERM_MAIN].visible)
	{
		/* Increment tries, quit if we can't get any joy */
		if (++tries > 2) quit("No main window definition found!");

		/* Try both map and sub-window display modes */
		if (tries == 1)
		{
			if      (cur_display_mode == INTERFACE_SCREEN_EMPHA_SUB)
				cur_display_mode = INTERFACE_SCREEN_EMPHA_MAP;
			else if (cur_display_mode == INTERFACE_SCREEN_EMPHA_MAP)
				cur_display_mode = INTERFACE_SCREEN_EMPHA_SUB;

			else if (cur_display_mode == INTERFACE_WINDOW_EMPHA_MAP)
				cur_display_mode = INTERFACE_WINDOW_EMPHA_SUB;
			else if (cur_display_mode == INTERFACE_WINDOW_EMPHA_SUB)
				cur_display_mode = INTERFACE_WINDOW_EMPHA_MAP;
		}

		/* If that doesn't work, then try INTERFACE_SCREEN_EMPHA_MAP, ... */
		else
		{
			/* ... which SHOULD be defined for all default monitor sizes. */
			cur_display_mode = INTERFACE_SCREEN_EMPHA_MAP;
		}

		/* Try again */
		goto resolution_retry;
	}

	/* Set "arg_display_mode" */
	arg_display_mode = cur_display_mode;


	/* If we have not defined a display window, copy the main window */
	if (!window_settings[WINDOW_DISPLAY].visible)
	{
		COPY(&window_settings[WINDOW_DISPLAY], &window_settings[TERM_MAIN], window_type);
	}

	/* Use the special map window only if visible */
	use_special_map = window_settings[TERM_MAP].visible;

	/* This port allows special display configurations */
	special_view_hook = special_view_gui;


	/* We want either a bordered or full-screen application window */
	if ((cur_display_mode == INTERFACE_SCREEN_EMPHA_MAP) ||
	    (cur_display_mode == INTERFACE_SCREEN_EMPHA_SUB))
	{
		/*
		 * We use software surfaces because we cannot rely on getting hardware surfaces.
		 */
		flags = SDL_SWSURFACE | SDL_FULLSCREEN | SDL_ANYFORMAT;

		/* Create the application window */
		AppWin = SDL_SetVideoMode(screen_w, screen_h, 0, flags);
	}
	else
	{
		/* In windowed mode, we request only a software surface */
		flags = SDL_SWSURFACE | SDL_ANYFORMAT;

		/*
		 * In order to position windows in SDL, we must set environment variables
		 * (or write system-specific code).
		 * The following might or might not work on all systems:
		 * SDL_putenv("SDL_VIDEO_WINDOW_POS=10,10");
		 * It might have to be inserted before SDL is initialized.
		 */

		/* Request that the window be centered (using environment variables!?). */
		SDL_putenv("SDL_VIDEO_WINDOW_POS=center");
		SDL_putenv("SDL_VIDEO_CENTERED=1");

		/* Create the application window */
		AppWin = SDL_SetVideoMode(app_wid - 8, app_hgt - 34, 0, flags);
	}


	/* Handle failure */
 	if (!AppWin) quit(format("Failed to create %d%d window at %d bpp!",
		VideoInfo->current_h, VideoInfo->current_w, VideoInfo->vfmt->BitsPerPixel));

	/* Activate paletting code if we have just 256 colors */
	if (AppWin->format->BitsPerPixel == 8) paletted = TRUE;

	/* Save application client width and height */
	client_w = AppWin->w;
	client_h = AppWin->h;

	/* Set the window caption */
	SDL_WM_SetCaption(VERSION_NAME, NULL);


	/* Do various things to each term window (plus the 1 special display window) */
	for (i = 0; i < TERM_MAX + 1; i++)
	{
		/* Get the window */
		win_ptr = &window_settings[i];

		/* Ignore non-visible windows */
		if (!win_ptr->visible) continue;

		/* Remember current application size (so this window gets loaded again) */
		win_ptr->resolution_x = app_wid;
		win_ptr->resolution_y = app_hgt;

		/* Access the specified font file */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, win_ptr->font_want);

		/* Use it */
		if (change_font_sdl(win_ptr, buf))
		{
			/* On failure, access the standard font file */
			(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, DEFAULT_GAME_FONT);

			/* Use it */
			if (change_font_sdl(win_ptr, buf))
			{
				quit_fmt("Could not find the default font (looking for \"%s\")", buf);
			}
		}

		/* We know the size of most windows (Hard-coded test  XXX XXX) */
		if (i != TERM_MAP)
		{
			/* Get rows and columns for all windows whose size is now known */
			cols = (win_ptr->window_wid - win_ptr->border_left -
				win_ptr->border_right) / win_ptr->tile_wid;
			rows = (win_ptr->window_hgt - win_ptr->border_top -
				win_ptr->border_bottom) / win_ptr->tile_hgt;

			/* Save rows and cols, ensuring that they are never greater than 255 */
			win_ptr->cols = MIN(cols, 255);
			win_ptr->rows = MIN(rows, 255);

			/* Verify rows and columns */
			if ((win_ptr->cols < term_size_min[i][0]) ||
				(win_ptr->rows < term_size_min[i][1]))
			{
				char buf[1024];

				/* Build a (hopefully informative) error message */
				(void)my_strcpy(buf, format("Window \"%s\" (Term-#%d) requires %d columns and %d rows, but only has %dx%d.\n\nReasons for this error may include not having a small enough font\navailable in \"lib/xtra/font\" or corruption of the %s.ini file.",
						angband_term_name[i], i, term_size_min[i][0], term_size_min[i][1],
						win_ptr->cols, win_ptr->rows, AppName),
						sizeof(buf));

				/* Save the error */
				fprintf(stderr, "%s\n", buf);

				/* Complain and quit */
				quit_fmt(buf);
			}
		}
	}


	/* Initialize graphics (also updates the map term window) */
	if (init_graphics_sdl(FALSE)) use_graphics = arg_graphics;

	/* On failure, cancel the request */
	else                          arg_graphics = use_graphics;


	/* Calculate map display (if present) */
	if (window_settings[TERM_MAP].visible) calc_map_display();


	/* Create and initialize terms */
	for (i = 0; i < TERM_MAX; i++)
	{
		/* Skip windows that are not visible */
		if (!window_settings[i].visible) continue;

		/* Remember which term we are hosting */
		window_settings[i].Term_idx = i;

		/* Create and initialize the term */
		term_data_link_sdl(&window_settings[i]);

		/* Add a pointer to the term to our easy access array */
		angband_term[i] = &term_data[i];
	}


	/* The tall display window uses the main term */
	window_settings[WINDOW_DISPLAY].Term_idx = TERM_MAIN;

	/* Link to the main term */
	(void)Term_activate(term_main);


	/* If screen is larger than our current resolution, maximize the windows. */
	if ((screen_w > wid_tmp) || (screen_h > hgt_tmp)) do_maximize();

#ifdef SUPPORT_GAMMA

	/* Set up the gamma correction table */
	if (gamma_correction > 0) build_gamma_table(gamma_correction);

#endif /* SUPPORT_GAMMA */

	/* New palette XXX XXX XXX */
	(void)new_palette();


	/* Build a list of fonts available to the game */
	get_font_list("fontlist.txt", ".fon");

	/* Hack -- If the graphics are masked, then the map display should always use "Term_pict". */
	if ((MaskSurface) && (term_map))
	{
		term_map->always_pict = TRUE;
		term_map->higher_pict = FALSE;
	}

	/* Build a color for "black" that matches the pixel depth of this surface */
	black_clr = SDL_MapRGB(AppWin->format, win_clr[TERM_DARK].r, win_clr[TERM_DARK].g, win_clr[TERM_DARK].b);

	/* React to global changes (make sure we've updated completely) */
	(void)Term_xtra_sdl_react();

	/* Process pending messages */
	(void)Term_xtra_sdl_flush();
}



/*** Temporary Hooks (for use before the application window is created) ***/


/*
 * Display warning message (see "z-util.c")
 */
static void hack_plog(cptr str)
{
	/* Do nothing (because we don't have a surface yet) */
	(void)str;
}

/*
 * Display error message and quit (see "z-util.c")
 */
static void hack_quit(cptr str)
{
	/* Display nothing (because we don't have a surface yet) */
	(void)str;

	/* Shut down the TTF library */
	TTF_Quit();

	/* Shut down the SDL library */
	SDL_Quit();

	/* Exit */
	exit(0);
}



/*** Various hooks ***/


/*
 * Create a simple popup box to display warnings and errors.
 *
 * Not yet implemented.  XXX XXX
 */
static void SDL_MessageBox(cptr str, cptr title)
{
	/* Wait for it */
	(void)inkey(ALLOW_CLICK);
}



/*
 * Display warning message (see "z-util.c")
 *
 * Hack up a dialog box when we get the chance.
 */
static void hook_plog(cptr str)
{
	/* Warning */
	if (str)
	{
		msg_print(str);
		(void)inkey(ALLOW_CLICK);
	}
}


#ifdef USE_SOUND
/*
 * Shut off the audio system
 */
static void cleanup_audio(void)
{
	int i, j;

	/* Halt playback on all channels */
	Mix_HaltChannel(-1);

	/* Free the chunks */
	for (i = 0; i < NUM_CHANNELS; i++)
	{
		Mix_FreeChunk(mix_chunks[i]);
		mix_chunks[i] = NULL;
	}

	/* Free the channels themselves */
	(void) Mix_AllocateChannels(0);

	/* Shut down the current music, if any */
	if (music_track) Mix_FreeMusic(music_track);

	/* Free the sound names */
	if (sounds_ready)
	{
		for (i = 0; i < MSG_MAX; i++)
		{
			for (j = 0; j < SAMPLE_MAX; j++)
			{
				/* Free the sound names */
				if (sound_file[i][j]) (void)string_free(sound_file[i][j]);
			}
		}
	}

	/* Free the music names */
	if (music_ready)
	{
		for (i = 0; i < MUSIC_MAX; i++)
		{
			for (j = 0; j < SAMPLE_MAX; j++)
			{
				/* Free the song names */
				if (music_file[i][j]) (void)string_free(music_file[i][j]);
			}
		}
	}
}
#endif /* USE_SOUND */




/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(cptr str)
{
	int i;

	/* Give a warning */
 	if (str)
	{
		SDL_MessageBox(str, "Error");
	}

	/* Hack -- Re-calculate application window size (including borders) */
	if ((cur_display_mode == INTERFACE_SCREEN_EMPHA_MAP) ||
	    (cur_display_mode == INTERFACE_SCREEN_EMPHA_SUB))
	{
		app_wid = AppWin->w;
		app_hgt = AppWin->h;
	}
	else
	{
		/* XXX XXX XXX */
		app_wid = AppWin->w + 8;
		app_hgt = AppWin->h + 34;
	}

	/* Save the preferences */
	save_prefs();

	/* Destroy all term windows */
	for (i = TERM_MAX - 1; i >= 0; --i)
	{
		/* Nuke the game term structure */
		(void)term_nuke(angband_term[i]);
	}

	/* Scan all the term windows */
	for (i = 0; i <= TERM_MAX; i++)
	{
		/* Get this term window */
		window_type *win_ptr = &window_settings[i];

		/* Remove all fonts from the system, free resources */
		remove_font_sdl(win_ptr);
		if (win_ptr->font_want) (void)string_free(win_ptr->font_want);
	}

	/* Free the application surface */
	SDL_FreeSurface(AppWin);

#ifdef USE_GRAPHICS
	/* Free the bitmap surfaces */
	SDL_FreeSurface(GraphSurface);

	SDL_FreeSurface(MaskSurface);
#endif /* USE_GRAPHICS */


#ifdef USE_SOUND

	/* Shut off the audio system */
	cleanup_audio();

	/* Shut down the SDL_Mixer library */
	Mix_CloseAudio();

#endif /* USE_SOUND */

	/* Shut down the TTF library */
	TTF_Quit();

	/* Shut down the SDL library */
	SDL_Quit();


	/*** Free some other stuff ***/

	/* Free the stretch array */
	if (stretch_array)
	{
		/* Free any stored pixel offsets */
		if (stretch_array->src_pixel_offset) FREE(stretch_array->src_pixel_offset);
		if (stretch_array->dst_pixel_offset) FREE(stretch_array->dst_pixel_offset);

		/* Free the stretch array itself */
		FREE(stretch_array);
	}

	/* Free directory path strings */
	(void)string_free(ANGBAND_DIR_XTRA_FONT);
	(void)string_free(ANGBAND_DIR_XTRA_GRAF);
	(void)string_free(ANGBAND_DIR_XTRA_SOUND);
	(void)string_free(ANGBAND_DIR_XTRA_MUSIC);

	cleanup_angband();

	exit(0);
}



/*** Initialize ***/



/*
 * Init file paths.  In addition to the basic paths created by
 * "init_file_paths()", we also create paths to fonts, graphics, sounds, and
 * the initialization file.
 *
 * The "lib" directory is "/<<game dir>>/lib/" by default, but (where allowed)
 * this can be set to any legal directory using the <<VERSION_NAME>>_PATH
 * environment variable.
 */
static void init_stuff(void)
{
	int i;
	char path[1024];
	cptr tail = NULL;
	cptr p = PATH_SEP;


	/* Use the working folder as a starting point */
	(void)my_strcpy(path, WorkingFolder, sizeof(path));

	/* If we are allowing changeable paths */
#ifndef FIXED_PATHS
	/* Get the environment variable */
	tail = getenv(format("%s_PATH", VERSION_NAME));
#endif /* FIXED_PATHS */


	/* If a suitable environment variable exists, */
	if (tail)
	{
		/* Use it as the effective working directory */
		(void)my_strcpy(path, tail, sizeof(path));

		/* Add a path separator if needed */
		if (!suffix(path, PATH_SEP)) (void)my_strcat(path, PATH_SEP, sizeof(path));
	}

	/* If we aren't using an environment variable, */
	else
	{
		/*
		 * This code could stand to be smartened up...
		 */

		/* Get the position of the last character in the path */
		i = strlen(path) - 1;

		/* Remove any path separator */
		if (path[i] == p[0]) path[i] = '\0';

		/* Remove everything after the last PATH_SEP */
		while (!suffix(path, PATH_SEP))
		{
			path[--i] = '\0';
			if (i < 2) break;
		}

		/* And then remove the path separator also */
		path[--i] = '\0';

		/* Save the application directory */
		(void)my_strcpy(WorkingFolder, path, sizeof(WorkingFolder));

		/* Add the "lib" directory, plus path separator */
		(void)my_strcat(path, format("%s%s%s", PATH_SEP, "lib", PATH_SEP), sizeof(path));
	}


	/* Initialize file paths */
	init_file_paths(path);


	/*
	 * On single-user systems, the working directory is wherever the game was
	 * launched from (usually the actual game directory).  On multi-user
	 * systems, we are safe writing to the user's personal game directory.
	 */
#ifdef PRIVATE_USER_PATH
	/* Use user's game directory */
	(void)my_strcpy(WorkingFolder, PRIVATE_USER_PATH, sizeof(WorkingFolder));

	/* Create directories for the user's files */
	create_user_dirs();
#endif


	/* Add a path separator if needed */
	if (!suffix(WorkingFolder, PATH_SEP)) (void)my_strcat(WorkingFolder, PATH_SEP, sizeof(path));


	/* Build the news filename */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_FILE, "news.txt");

	/* Hack -- Validate the "news.txt" file */
	validate_file(path);


	/* Build the "font" path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "font");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_FONT = string_make(path);

	/* Build the filename */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA_FONT, DEFAULT_GAME_FONT);

	/* Hack -- Validate the basic font */
	validate_file(path);


#ifdef USE_GRAPHICS

	/* Build the "graf" path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "graf");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_GRAF = string_make(path);


#endif /* USE_GRAPHICS */


#ifdef USE_SOUND

	/* Build the "sound" path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "sound");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_SOUND = string_make(path);

	/* Build the "music" path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "music");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_MUSIC = string_make(path);


	/* Allocate some sound channels */
	max_channels = Mix_AllocateChannels(NUM_CHANNELS);

	/* Wipe the pointers to the sound chunk data */
	for (i = 0; i < NUM_CHANNELS; i++) mix_chunks[i] = NULL;

#endif /* USE_SOUND */

	/* Allocate a (single) stretch array */
	MAKE(stretch_array, stretch_array_type);
}




/*
 * The SDL port's "main()" function.
 */
int main(int argc, char *argv[])
{
	int i;

	const SDL_VideoInfo *VideoInfo;
	SDL_TimerID timer_basic;

	/* Remember the program name (may or may not work) */
	argv0 = argv[0];


#ifdef SET_UID

	/* Get the user id */
	player_uid = getuid();
	player_egid = getegid();

#endif /* SET_UID */

	/* Catch nasty signals */
	signals_init();


	/* Get the current working directory (should be where the application got launched from) */
	strcpy(WorkingFolder, argv[0]);

	/* Initialize SDL:  Timer, video, and audio functions */
	if (SDL_Init(SDL_INIT_TIMER | SDL_INIT_AUDIO | SDL_INIT_VIDEO) < 0)
	{
		fprintf(stderr, "Couldn't initialize SDL: %s\n",SDL_GetError());
		return (2);
	}

	/* Initialize the TTF library */
	if (TTF_Init() < 0)
	{
		fprintf(stderr, "Couldn't initialize TTF: %s\n",SDL_GetError());
		SDL_Quit();
		return (2);
	}

#ifdef USE_SOUND
	if (Mix_OpenAudio(22050, MIX_DEFAULT_FORMAT, 2, 2048) < 0)
	{
		TTF_Quit();
		SDL_Quit();
		return (2);
	}
#endif /* USE_SOUND */


	/* Temporary warning and quit hooks */
	plog_aux = hack_plog;
	quit_aux = hack_quit;


	/* Get information about the video hardware */
	VideoInfo = SDL_GetVideoInfo();

	/* Require at least 256 colors */
	if (VideoInfo->vfmt->BitsPerPixel < 8) quit(format("This %s port requires at least 256 colors.", VERSION_NAME));

	/* Prepare the filepaths */
	init_stuff();


	/* Set up a 1-second timer for screen animation */
	timer_basic = SDL_AddTimer(1000L, animate_timer_sdl, 0);


	/* Initialize the ignored keypresses */
	for (i = 0; ignore_key_list[i]; i++)
	{
		ignore_key[ignore_key_list[i]] = TRUE;
	}

	/* Enable key repeating; use defaults */
	(void)SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

	/* Enable Unicode (so we can read key codes) */
	(void)SDL_EnableUNICODE(1);


	/* Use the maximum possible text colors */
	max_system_colors = MAX_COLORS;

	/* Initialize the colors */
	for (i = 0; i < MAX_COLORS; i++)
	{
		/* Extract desired values */
		win_clr[i].r = color_table[i].rv;
		win_clr[i].g = color_table[i].gv;
		win_clr[i].b = color_table[i].bv;
	}


	/* Prepare the windows */
	init_windows(VideoInfo);

	/* Activate warning and quit hooks */
	plog_aux = hook_plog;
	quit_aux = hook_quit;

	/* Set global function hooks */
	switch_display_hook = switch_display_gui;
	window_change_font_hook = window_change_font_gui;
	change_font_hook = change_font_sdl;
	remove_font_hook = remove_font_sdl;
	init_graphics_hook = init_graphics_sdl;
	term_data_link_hook = term_data_link_sdl;


	/* Set the system suffix */
	ANGBAND_SYS = "sdl";


	/* Deactivate the main view, request a full-screen display */
	display_change(DSP_SAVE | DSP_FULL,
		term_size_min[WINDOW_DISPLAY][0], term_size_min[WINDOW_DISPLAY][1]);

	/* Initialize the game */
	init_angband();

	/* Wait for response unless we have a savefile */
 	if (!savefile[0]) pause_line(Term->rows - 1);

	/* Cancel full-screen view */
	display_change(DSP_FULL | DSP_LOAD, 0, 0);


	/* Play the game */
	play_game(FALSE);

	/* Remove the basic timer */
	(void)SDL_RemoveTimer(timer_basic);

	/* Paranoia */
	quit(NULL);

	/* Paranoia */
	return (0);
}

#endif /* USE_SDL */

