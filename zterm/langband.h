#ifndef LANGBAND_H
#define LANGBAND_H



#ifdef WIN32
/* angband.h included earlier with these defs */
#ifndef INCLUDED_H_TYPE_H
typedef unsigned char byte;
typedef const char *cptr;
typedef int errr;
typedef char bool;
#endif
#endif /* win32 */


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

INTERFACE errr init_graphics();
INTERFACE int init_sound_system(int size);

INTERFACE int paint_image(const char *fname, int x, int y);
INTERFACE int load_gfx_image(const char *fname, const char *type);
INTERFACE int paint_gfx_image(const char *fname, const char *name, int x, int y);
INTERFACE int textureBackground(int term_num, const char *fname, int alpha);
INTERFACE int init_tile_files();
INTERFACE int fill_area(int image_index, int tile_num, int x1, int y1, int x2, int y2);

INTERFACE int get_sound_status();
INTERFACE int load_sound_effect(const char *fname, int idx);

/* remove later */
extern void print_image_list();

INTERFACE int load_scaled_image(const char *filename, int image_index, int width, int height);
INTERFACE int my_get_current_term();
INTERFACE int my_term_activate(int num);

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

FontData *load_hex_font(cptr filename, bool justmetrics);
//errr load_HEX_font_sdl(FontData *fd, cptr filename, bool justmetrics);
errr strtoii(const char *str, Uint32 *w, Uint32 *h);

#ifdef ALLOW_TTF
int display_char(SDL_Surface *surface, SDL_Rect *dest, FontData *fdata, s16b attr, s16b ch);
FontData *load_ttf_font(const char *fname, int size);
#endif /* use_ttf */


int sdl_textureBackground(int term_num, const char *fname, int alpha);

int JAI_BlitSurfaceAlpha(SDL_Surface *src, SDL_Rect *srcrect,
			 SDL_Surface *dst, SDL_Rect *dstrect);

extern int sdl_load_gfx_image(const char *fname, const char *type);
extern int sdl_paint_gfx_image(const char *fname, const char *name, int x, int y);
extern int sdl_load_scaled_image(const char *filename, int image_index, int width, int height);
extern int sdl_switch_terms(int bigterm);
extern int sdl_term_gfx_use_p(term *t);
extern int sdl_swap_map();

extern SDL_Color color_data_sdl[16];

#endif /* use sdl */

#ifdef USE_GCU

extern int gcu_switch_terms(int bigterm);

#endif /* use gcu */

#endif /* langband_h */
