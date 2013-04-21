#ifndef LANGBAND_H
#define LANGBAND_H

/*
 * DESC: langband.h - langband-related defined/includes/etc.
 * Copyright (c) 2000-2002 - Stig Erik Sandø

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */


#ifndef INTERFACE
#if defined(WIN_MAKEDLL)
#  define INTERFACE __declspec(dllexport)
#elif defined(WIN_USEDLL)
#  define INTERFACE __declspec(dllimport)
#else
#  define INTERFACE
#endif
#endif /* iface */



/*
 * Extract the "SUNOS" flag from the compiler
 */
#if defined(sun)
# ifndef SUNOS
#   define SUNOS
# endif
#endif

/*
 * Extract the "ULTRIX" flag from the compiler
 */
#if defined(ultrix) || defined(Pyramid)
# ifndef ULTRIX
#  define ULTRIX
# endif
#endif

/*
 * Extract the "ATARI" flag from the compiler [cjh]
 */
#if defined(__atarist) || defined(__atarist__)
# ifndef ATARI
#  define ATARI
# endif
#endif

/*
 * Extract the "ACORN" flag from the compiler
 */
#ifdef __riscos
# ifndef ACORN
#  define ACORN
# endif
#endif

/*
 * Extract the "SGI" flag from the compiler
 */
#ifdef sgi
# ifndef SGI
#  define SGI
# endif
#endif

/*
 * Extract the "MSDOS" flag from the compiler
 */
#ifdef __MSDOS__
# ifndef MSDOS
#  define MSDOS
# endif
#endif

/*
 * Extract the "WINDOWS" flag from the compiler
 */
#if defined(_Windows) || defined(__WINDOWS__) || \
    defined(__WIN32__) || defined(WIN32) || \
    defined(__WINNT__) || defined(__NT__)
# ifndef WINDOWS
#  define WINDOWS
# endif
#endif

/*
 * Remove the MSDOS flag when using WINDOWS
 */
#ifdef WINDOWS
# ifdef MSDOS
#  undef MSDOS
# endif
#endif

/*
 * Remove the WINDOWS flag when using MACINTOSH
 */
#ifdef MACINTOSH
# ifdef WINDOWS
#  undef WINDOWS
# endif
#endif



/*
 * OPTION: Define "L64" if a "long" is 64-bits.  See "h-types.h".
 * The only such platform that angband is ported to is currently
 * DEC Alpha AXP running OSF/1 (OpenVMS uses 32-bit longs).
 */
#if defined(__alpha) && defined(__osf__)
# define L64
#endif


#include <stdio.h>
#include <ctype.h>

#if defined(NeXT)
# include <libc.h>
#else
# include <stdlib.h>
#endif


#include <time.h>
#include <string.h>
#include <stdarg.h>

/*
 * Basic "types".
 *
 * Note the attempt to make all basic types have 4 letters.
 * This improves readibility and standardizes the code.
 *
 * Likewise, all complex types are at least 4 letters.
 * Thus, almost every 1 to 3 letter word is a legal variable,
 * except for certain reserved words ('for' and 'if' and 'do').
 *
 * Note that the type used in structures for bit flags should be uint.
 * As long as these bit flags are sequential, they will be space smart.
 *
 * Note that on some machines, apparently "signed char" is illegal.
 *
 * A char/byte takes exactly 1 byte
 * A s16b/u16b takes exactly 2 bytes
 * A s32b/u32b takes exactly 4 bytes
 *
 * A sint/uint takes at least 2 bytes
 * A long/huge takes at least 4 bytes
 *
 * Note that some files have already been included by "h-include.h"
 * These include <stdio.h> and <sys/types>, which define some types
 * In particular, "bool", "byte", "uint", and "huge" may be defined
 * already, possibly using "typedefs" of various kinds, and possibly
 * being defined to something other than required by my code.  So we
 * simply redefine them all using a stupid "_hack" suffix.
 *
 * Also, see <limits.h> for min/max values for sint, uint, long, huge
 * (INT_MIN, INT_MAX, 0, UINT_MAX, LONG_MIN, LONG_MAX, 0, ULONG_MAX).
 * These limits should be verified and coded into "h-constant.h", or
 * perhaps not, since those types have "unknown" length by definition.
 */



/*
 * Hack -- prevent problems with non-MACINTOSH
 */
#undef uint
#define uint uint_hack

/*
 * Hack -- prevent problems with MSDOS and WINDOWS
 */
#undef huge
#define huge huge_hack

/*
 * Hack -- prevent problems with AMIGA
 */
#undef byte
#define byte byte_hack

/*
 * Hack -- prevent problems with C++
 */
#undef bool
#define bool bool_hack


/* Note that "signed char" is not always "defined" */
/* So always use "s16b" to hold small signed values */
/* A signed byte of memory */
/* typedef signed char syte; */

/* Note that unsigned values can cause math problems */
/* An unsigned byte of memory */
typedef unsigned char byte;

/* Note that a bool is smaller than a full "int" */
/* Simple True/False type */
typedef char bool;


/* A signed, standard integer (at least 2 bytes) */
typedef int sint;

/* An unsigned, "standard" integer (often pre-defined) */
typedef unsigned int uint;


/* The largest possible signed integer (pre-defined) */
/* typedef long long; */

/* The largest possible unsigned integer */
typedef unsigned long huge;


/* Signed/Unsigned 16 bit value */
typedef signed short s16b;
typedef unsigned short u16b;

/* Signed/Unsigned 32 bit value */
#ifdef L64	/* 64 bit longs */
typedef signed int s32b;
typedef unsigned int u32b;
#else
typedef signed long s32b;
typedef unsigned long u32b;
#endif




/*** Pointers to Functions of special types (for various purposes) ***/

typedef enum {
    LISPSYS_CMUCL     = 0,
    LISPSYS_ACL       = 1,
    LISPSYS_CLISP     = 2,
    LISPSYS_LISPWORKS = 3,
    LISPSYS_SBCL      = 4,
    LISPSYS_CORMAN    = 5,
    LISPSYS_BAD       = 20
} LISP_SYSTEMS;

typedef enum {
    UITYPE_X11        = 0,
    UITYPE_GCU        = 1,
    UITYPE_GTK        = 2,
    UITYPE_WIN        = 3,
    UITYPE_SDL        = 4,
    UITYPE_BAD        = 20
} UITYPES;

/*
 * Define some simple constants
 */


/*
 * Hack -- Define NULL
 */
#ifndef NULL
# ifdef __STDC__
#  define NULL ((void*)0)
# else
#  define NULL ((char*)0)
# endif /* __STDC__ */
#endif /* NULL */



/*
 * The constants "TRUE" and "FALSE"
 */

#undef TRUE
#define TRUE	1

#undef FALSE
#define FALSE	0



/*
 * OPTION: Use "blocking getch() calls" in "main-gcu.c".
 * Hack -- Note that this option will NOT work on many BSD machines
 * Currently used whenever available, if you get a warning about
 * "nodelay()" undefined, then make sure to undefine this.
 */
#if defined(SYS_V) || defined(AMIGA)
# define USE_GETCH
#endif


/*
 * OPTION: Use the "curs_set()" call in "main-gcu.c".
 * Hack -- This option will not work on most BSD machines
 */
#ifdef SYS_V
# define USE_CURS_SET
#endif



/*** Color constants ***/


/*
 * Angband "attributes" (with symbols, and base (R,G,B) codes)
 *
 * The "(R,G,B)" codes are given in "fourths" of the "maximal" value,
 * and should "gamma corrected" on most (non-Macintosh) machines.
 */
#define TERM_DARK		0	/* 'd' */	/* 0,0,0 */
#define TERM_WHITE		1	/* 'w' */	/* 4,4,4 */
#define TERM_SLATE		2	/* 's' */	/* 2,2,2 */
#define TERM_ORANGE		3	/* 'o' */	/* 4,2,0 */
#define TERM_RED		4	/* 'r' */	/* 3,0,0 */
#define TERM_GREEN		5	/* 'g' */	/* 0,2,1 */
#define TERM_BLUE		6	/* 'b' */	/* 0,0,4 */
#define TERM_UMBER		7	/* 'u' */	/* 2,1,0 */
#define TERM_L_DARK		8	/* 'D' */	/* 1,1,1 */
#define TERM_L_WHITE	9	/* 'W' */	/* 3,3,3 */
#define TERM_VIOLET		10	/* 'v' */	/* 4,0,4 */
#define TERM_YELLOW		11	/* 'y' */	/* 4,4,0 */
#define TERM_L_RED		12	/* 'R' */	/* 4,0,0 */
#define TERM_L_GREEN	13	/* 'G' */	/* 0,4,0 */
#define TERM_L_BLUE		14	/* 'B' */	/* 0,4,4 */
#define TERM_L_UMBER	15	/* 'U' */	/* 3,2,1 */


#define NO_FLAGS 0
#define LANGBAND_GRAPHICS 1
#define LANGBAND_SOUND 2

#define LANGBAND_TEXT_END 0x80
#define LANGBAND_GFX_START 0x100

#define IMAGE_ARRAY_SIZE 64

// Gervais values!
#define GFXTILE_WIDTH 32
#define GFXTILE_HEIGHT 32

extern const char *base_source_path;
extern const char *base_config_path;
extern const char *base_gfx_path;

extern void lb_format(FILE *ofile, int priority, const char *fmt, ...);
#ifdef DEBUG
extern void DBGPUT(const char *fmt, ...);
#else
#define DBGPUT if(1){}else printf
#endif

extern void ERRORMSG(const char *fmt, ...);
#define INFOMSG ERRORMSG
#define DEBUGPUT DBGPUT

INTERFACE int init_graphics();
INTERFACE int init_sound_system(int size);

INTERFACE int paint_image(const char *fname, int x, int y);
INTERFACE int load_gfx_image(const char *fname, int idx, unsigned int transcolour);
INTERFACE int paint_gfx_image(const char *fname, const char *name, int x, int y);
INTERFACE int load_texture(int idx, const char*filename, int twid, int thgt, int alpha);
INTERFACE int init_tile_files();
INTERFACE int fill_area(int image_index, int tile_num, int x1, int y1, int x2, int y2);

INTERFACE int get_sound_status();
INTERFACE int load_sound_effect(const char *fname, int idx);
INTERFACE int play_sound_effect(int sound_idx);

/* remove later */
extern void print_image_list();

INTERFACE int load_scaled_image(const char *filename, int image_index, int width,
				int height, unsigned int transcolour);

INTERFACE int get_image_width(int idx);
INTERFACE int get_image_height(int idx);

extern int use_sound;

INTERFACE int current_ui();
INTERFACE void print_coloured_token(int wantedTerm, int colour, int token, int row, int col);
INTERFACE void print_coloured_stat(int wantedTerm, int colour, int stat, int row, int col);
INTERFACE void print_coloured_number(int wantedTerm, int colour, long number, int padding, int row, int col);
INTERFACE int init_c_side(const char *ui, const char *sourcePath,
			  const char *confPath, const char *gfxPath,
			  int extra_flags);
INTERFACE char *load_sound(int msg, char *fname);
int play_game_lisp();
void readjust_screen_lisp(int width, int height);
void mouse_clicked(int button, int x, int y);


/** will we access lisp through callbacks? */
extern int lisp_will_use_callback;
extern LISP_SYSTEMS current_lisp_system;
INTERFACE void set_lisp_system(LISP_SYSTEMS type);

//#ifdef WIN32
//INTERFACE int setHINST(long val);
//#else
INTERFACE void set_lisp_callback(char *name, void *ptr);
//#endif


INTERFACE int exp_complex_blit(short win_num, short x, short y, unsigned int img, int flags);
INTERFACE int exp_transparent_blit(short win_num, short x, short y, unsigned int img, short flags);
INTERFACE int exp_full_blit(short win_num, short x, short y, unsigned int img, short flags);
INTERFACE int exp_clear_coords(short win_num, short x, short y, short w, short h);
INTERFACE int exp_flush_coords(short win_num, short x, short y, short w, short h);
INTERFACE int sdl_getEvent(int option);
INTERFACE int listenForEvent(int option);
INTERFACE int cleanup_c_side(void);

#ifdef USE_X11
int cleanup_X11(void);
#endif

#ifdef USE_GCU
INTERFACE int cleanup_GCU(void);
#endif

#ifdef USE_SDL
INTERFACE int cleanup_SDL(void);
#endif


#ifdef WIN32
INTERFACE int main(int argc, char *argv[]);
#endif

typedef struct sound_bite sound_bite;

struct sound_bite {
    char *filename;
    void *handle; // probably a Mix_Chunk* for SDL
};

extern sound_bite **sound_bites;

#ifdef USE_SDL

#include "SDL.h"

#include "SDL_mixer.h"

#define MAX_IMAGES 64

#define FONT_TYPE_TTF 5
#define FONT_TYPE_HEX 6

struct FontData {
    int width;
    int height;
    
    void *theFont;
    int font_type;
    SDL_Surface *letters[256];
};

struct graf_tiles {

	/*
	 * to find a character:
	 * x = character * w
	 */
	SDL_Surface *face;
	/* 
	 * font metrics.
	 * Obviously, the font system is very minimalist.
	 */
	Uint8 w;
	Uint8 h;

	Uint8 dw; /* width and height of font on destination surface */
	Uint8 dh;

	Uint8 precolorized;
};

struct tile_information {

    SDL_Surface *tiles[MAX_IMAGES];
    char *tile_files[MAX_IMAGES];
    int tile_columns[MAX_IMAGES]; // 
    int num_tiles;

    int tile_width;
    int tile_height;
};


typedef struct FontData FontData; /* must be here to avoid fwd. ref. */
typedef struct graf_tiles graf_tiles; 
typedef struct tile_information TileInformation;

FontData *load_hex_font(const char *filename, bool justmetrics);
//int load_HEX_font_sdl(FontData *fd, const char *filename, bool justmetrics);
int strtoii(const char *str, Uint32 *w, Uint32 *h);

#ifdef ALLOW_TTF
int display_char(SDL_Surface *surface, SDL_Rect *dest, FontData *fdata, s16b attr, s16b ch);
FontData *load_ttf_font(const char *fname, int size);
#endif /* use_ttf */


//int sdl_textureBackground(int term_num, const char *fname, int alpha);

int JAI_BlitSurfaceAlpha(SDL_Surface *src, SDL_Rect *srcrect,
			 SDL_Surface *dst, SDL_Rect *dstrect);

extern int sdl_load_gfx_image(const char *fname, int idx, unsigned int transcolour);
extern int sdl_paint_gfx_image(const char *fname, const char *name, int x, int y);
extern int sdl_load_scaled_image(const char *filename, int image_index, int width,
				 int height, unsigned int transcolour);
extern int sdl_switch_terms(int bigterm);
//extern int sdl_term_gfx_use_p(term *t);
extern int sdl_swap_map();

extern int sdl_getEvent(int option);

extern SDL_Color color_data_sdl[16];

#endif /* use sdl */

#ifdef USE_GCU

extern int gcu_switch_terms(int bigterm);

#endif /* use gcu */

#endif /* langband_h */
