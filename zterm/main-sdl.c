/* File: main-sdl.c */

/* Purpose: SDL visual module for Angband 2.7.9 through 2.9.2 */

/* Most of the code in this file (all not appearing in main-xxx.c) is
 * Copyright 2001 Gregory Velichansky (hmaon@bumba.net)
 * You may use it under the terms of the standard Angband license (below) or
 * the GNU General Public License (GPL) Version 2 or greater. (see below)
 * Ben Harrison's copyright appears in the file COPYING and at last
 * check allows you to use his code under the GPL as well.
 *
 * The Angband license states:
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * The GNU GPL notice:
   main-sdl.c - SDL (http://libsdl.org) display module for Angband.
	Copyright (C) 2001  Gregory Velichansky (hmaon@bumba.net)
	Portions Copyright (C) 1997 Ben Harrison
	(see the file COPYING in the latest distribution of the Angband source code)

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

*/



/*
 * This file originally written by "Ben Harrison (benh@voicenet.com)".
 *
 */


#include "angband.h"
#include "langband.h"
#include "lbwindows.h"

#ifdef USE_SDL

/* 
 *
 * Pre-processor configuration, data structure definitions, global variables,
 * and #includes.
 *
 * The #defines could be moved into config.h if this module was a standard
 * feature of Angband, if people find that helpful.
 *
 * If certain parts of this file are moved to a separate file, the data
 * structures would possibly need to be moved to a header file. That's probably
 * not a good idea since no other display module works that way.
 *
 */

#include "SDL_image.h"
#include "SDL_mixer.h"
#include <string.h>

struct sdl_winconnection {

    SDL_Surface *face; // pointer to the paint-frame
    FontData *font_data; // pointer to the font-information for this frame
    graf_tiles *gt; // pointer to the graphical tiles this frame should use
    SDL_Surface *background; // pointer to the background surface

    Sint32 cx, cy; /* last known cursor coordinates */
    
    bool prefer_fresh; /* in case we don't implement FROSH in a graphics engine */
    bool cursor_on;
    
    bool cursor_magic; /* experimental cursor effects */

};

typedef struct sdl_winconnection sdl_winconnection;

/* this stuff was moved to sdl-maim.c */
extern errr SDL_GetPixel (SDL_Surface *f, Uint32 x, Uint32 y, Uint8 *r, Uint8 *g, Uint8 *b);
extern errr SDL_PutPixel (SDL_Surface *f, Uint32 x, Uint32 y, Uint8 r, Uint8 g, Uint8 b);
extern errr SDL_ScaleBlit(SDL_Surface *src, SDL_Rect *sr, SDL_Surface *dst, SDL_Rect *dr);
extern errr SDL_FastScaleBlit(SDL_Surface *src, SDL_Rect *sr, SDL_Surface *dst, SDL_Rect *dr);
extern SDL_Surface *SDL_ScaleTiledBitmap (SDL_Surface *src, Uint32 t_oldw, Uint32 t_oldh,
					  Uint32 t_neww, Uint32 t_newh, int dealloc_src);

extern errr strtoii(const char *str, Uint32 *w, Uint32 *h);
extern char *formatsdlflags(Uint32 flags);

extern void Multikeypress(char *k);
extern int IsMovement(SDLKey k);
extern char *SDL_keysymtostr(SDL_keysym *ks); /* this is the important one. */

extern errr SDL_init_screen_cursor(Uint32 w, Uint32 h);
extern errr SDL_DrawCursor(SDL_Surface *dst, SDL_Rect *dr);
static errr Term_text_sdl(int x, int y, int n, s16b a, const s16b *s);


#ifndef SDL_DISABLE
#define SDL_DISABLE 0
#endif


#define CURS_MAG_X 0
#define CURS_MAG_Y 21


/*
 * Extra data to associate with each "window"
 *
 * Each "window" is represented by a "term_data" structure, which
 * contains a "term" structure, which contains a pointer (t->data)
 * back to the term_data structure.
 *
 * A font_data struct keeps the SDL_Surface and other info for tile graphics
 * which include fonts.
 */

errr Term_xtra_sdl(int n, int v);

SDL_Surface *theWindow = NULL;

TileInformation *tileInfo = NULL;

//FontData *screen_font = NULL;
graf_tiles *screen_tiles = NULL;


static const char *david_gervais_tile_files[] = {
/* 0 */ "", 
        "", 
        "", 
        "dg_armor32.bmp", 
        "dg_effects32.bmp", 
/* 5 */ "dg_food32.bmp", 
        "dg_classm32.bmp", 
        "dg_humans32.bmp", 
        "dg_jewls32.bmp", 
        "dg_magic32.bmp", 
/* 10 */"dg_misc32.bmp", 
        "dg_potions32.bmp", 
        "dg_wands32.bmp", 
        "dg_weapons32.bmp", 
        "dg_people32.bmp", 
/* 15 */"dg_dragon32.bmp", 
        "dg_monster132.bmp", 
        "dg_monster232.bmp", 
        "dg_monster332.bmp", 
        "dg_monster432.bmp", 
/* 20 */"dg_monster532.bmp", 
        "dg_monster632.bmp", 
        "dg_monster732.bmp", 
        "dg_undead32.bmp", 
        "dg_uniques32.bmp", 
/* 25 */"dg_dungeon32.bmp", 
        "dg_grounds32.bmp", 
        "dg_extra132.bmp", 
        "dg_town032.bmp", 
        "dg_town132.bmp", 
/* 30 */"dg_town232.bmp", 
        "dg_town332.bmp", 
        "dg_town432.bmp", 
        "dg_town532.bmp", 
        "dg_town632.bmp", 
/* 35 */"dg_town732.bmp", 
        "dg_town832.bmp", 
        "dg_town932.bmp",
	"button.bmp",
	"button2.bmp",
        NULL
};

/* color data copied straight from main-xxx.c */
SDL_Color color_data_sdl[16] =
{
/* 0 */	{0, 0, 0, 0}, 
	{4, 4, 4, 0}, 
	{2, 2, 2, 0}, 
	{4, 2, 0, 0}, 
	{3, 0, 0, 0}, 
/* 5 */	{0, 2, 1, 0}, 
	{0, 0, 4, 0}, 
	{2, 1, 0, 0}, 
	{1, 1, 1, 0}, 
	{3, 3, 3, 0}, 
/* 10 */{4, 0, 4, 0}, 
	{4, 4, 0, 0}, 
	{4, 0, 0, 0}, 
	{0, 4, 0, 0}, 
	{0, 4, 4, 0}, 
	{3, 2, 1, 0}
};

int
sdl_term_gfx_use_p(angband_zterm *t) {
    if (t) {
	LangbandFrame *lf = (LangbandFrame*)(t->data);
	if (lf->use_gfx_tiles)
	    return 1;
    }
    return 0;
}



void
init_tile_information(TileInformation *ti) {
    int i = 0;
    
    if (!ti) return;
    
    ti->num_tiles = 0;
    ti->tile_width = 0;
    ti->tile_height = 0;
    
    for (i=0; i < MAX_IMAGES; i++) {
	ti->tiles[i] = NULL;
	ti->tile_files[i] = NULL;
	ti->tile_columns[i] = 0;
    }
}

void
read_tiles(TileInformation *ti, const char *tilefiles[]) {

    int i = 0;
    int wid;
    char filename[1024];
    
    for (i=0; i < MAX_IMAGES; i++) {
	
	const char *fname = tilefiles[i];
	SDL_Surface *face = NULL;
	
	if (fname == NULL) return;

	ti->num_tiles++;
	
	if (strlen(fname) == 0) continue;
	
	sprintf(filename,"%s%s/%s", base_gfx_path, "tiles", fname);
	
	ti->tile_files[i] = malloc(strlen(filename) + 1);
	strcpy(ti->tile_files[i], filename);

	//face = SDL_LoadBMP(filename);
	face = IMG_Load(filename);

	{
	    Uint32 trans = 0;
	    //LangbandFrame *lf = (LangbandFrame *)(Term->data);
	    wid = face->w / GFXTILE_WIDTH;
//	    int hgt = face->h / GFXTILE_HEIGHT;

	    
//	    int new_wid = wid * 9*2;
//	    int new_hgt = hgt * 15;

//	    DBGPUT("Wanted tile-size %d %d %d %d\n", screen_tiles.w, screen_tiles.h,
//		    screen_tiles.dw, screen_tiles.dh);
	    //DBGPUT("Rescaling %d,%d to %d,%d and %d\n", face->w, face->h, screen_tiles.dw, screen_tiles.dh,
	    //    face->format->BitsPerPixel);
	    trans = SDL_MapRGB(face->format, 255, 255, 255);
	    //SDL_SetColorKey(face, SDL_SRCCOLORKEY, trans);
	    //face = SDL_ScaleTiledBitmap(face, screen_tiles->w, screen_tiles->h,
	    //screen_tiles->dw, screen_tiles->dh, TRUE);
	    //trans = SDL_MapRGB(face->format, 255, 255, 255);
	    SDL_SetColorKey(face, SDL_SRCCOLORKEY, trans);
	    //DBGPUT("Success %d %d %d!\n", trans, face->format->BitsPerPixel, td->face->format->BitsPerPixel);
	}
	
	if (!face) {
	    ERRORMSG("Could not find  %s\n", filename);
	}
	else {
	    //DBGPUT("Read %s\n", filename);

	    ti->tiles[i] = face;
	    //update
//	    ti->tile_columns[i] = face->w / ti->tile_width; //right?
	    ti->tile_columns[i] = wid;
	}
    }
    
}

int
find_image(const char *fname) {
    int i = 0;
    //term_data *td = &loc_terms[i];
    
    for (i=0; i < tileInfo->num_tiles; i++) {
	//DBGPUT("Checking %s vs %s\n", fname, tileInfo->tile_files[i]);
	if (tileInfo->tile_files[i] &&
	    !strcmp(tileInfo->tile_files[i], fname)) {
	    return i;
	}
    }
    return -1;
}




static int
load_image_data(const char *filename, int image_index, int tiled, int tile_width, int tile_height) {
    SDL_Surface *surf = NULL;
    surf = IMG_Load(filename);

    // to avoid warnings
    tiled = tile_width = tile_height = 0;
    
    if (surf) {
        int idx = image_index;

	// small hack if you wish to test transparency on white colour
//	Uint32 trans = SDL_MapRGB(surf->format, 255, 255, 255);
//	SDL_SetColorKey(surf, SDL_SRCCOLORKEY, trans);
	
	tileInfo->tiles[idx] = surf;
	tileInfo->tile_files[idx] = malloc(strlen(filename) + 1);
	strcpy(tileInfo->tile_files[idx], filename); // fix!
	//DBGPUT("Returning idx %d for %s\n", idx, filename);
	return idx;
    }
    else {
	ERRORMSG("Was unable to load image %s\n", filename);
	return -1;
    }
}

int
load_tiled_image(const char *filename, int image_index, int tile_width, int tile_height) {
    return load_image_data(filename,image_index, 1, tile_width, tile_height);
}

int
load_plain_image(const char *filename, int image_index) {
    return load_image_data(filename,image_index, 0, -1, -1);
}

int
sdl_load_scaled_image(const char *filename, int image_index, int width, int height) {
    if (image_index == -1) {
	image_index = find_image(filename);
	if (image_index < 0) {
	    image_index = tileInfo->num_tiles++;
	}
	else {
	    return image_index;
	}

    }
    return load_image_data(filename, image_index, 0, width, height);
}

int
paint_image(const char *fname, int x, int y) {

    int j = 0;
    int i = 0;
 
    int idx = find_image(fname);

    if (idx < 0) {
	//DBGPUT("paint didn't find %s\n", fname);
	idx = load_plain_image(fname, tileInfo->num_tiles);
	if (idx > 0) {
	    tileInfo->num_tiles++;
	}
	else {
	    return -1;
	}
    }

/*
  DBGPUT("col %d, wid %d, hgt %d, fn-twid %d, fn-wid %d, fn-hgt %d\n",
	     td->tiles_col[idx], td->tiles[idx]->width, td->tiles[idx]->height,
	     td->fnt->twid, td->fnt->wid, td->fnt->hgt);
*/
    {
	LangbandFrame *lf = (LangbandFrame *)(Term->data);
	int attr = idx + LANGBAND_GFX_START;
	int counter = LANGBAND_GFX_START;	
	int wid = tileInfo->tiles[idx]->w / lf->tile_width;
	int hgt = tileInfo->tiles[idx]->h / lf->tile_height;
	//int wid = 5;
	//int hgt = 5;
	int xstart = x;// * 2;

	if (tileInfo->tiles[idx]->w % lf->tile_width)
	    wid++;
	
	//if (td->tiles[idx]->height % td->fnt->hgt)
	//    hgt++;
	
/*	
	DBGPUT( "attr %d, col %d, wid %d, hgt %d, img-wid %d, img-hgt %d, fn-twid %d, fn-wid %d, fn-hgt %d\n",
		 attr, td->tiles_col[idx],
		 wid,
		 hgt,
		 td->tiles[idx]->width, td->tiles[idx]->height,
		 td->fnt->twid, td->fnt->wid, td->fnt->hgt);
*/

		
	for (j=0; j < hgt; j++) {
	    for (i=0; i < wid; i++) {
		/* time to paint it */
		int spot = xstart + i;//*2;
//		int c = (i + (j * wid)) | 0x80;
//		DBGPUT("Printing %d %d at %d,%d\n", attr, counter, spot, y+j);
//		if (j==0 && i==0) {
//		    DBGPUT("Printing %d %d at %d,%d\n", attr, counter, spot, y+j);
//		}
		Term_queue_char(spot,   y+j, (s16b)attr, (s16b)counter++, 0, 0);
		//DBGPUT("go\n");
		//Term_queue_char(spot+1, y+j, -1, -1, 0, 0);
	    }
	}
    }
    
//    DBGPUT("done printing\n");
//    print_image_list();
    
    Term_fresh();
    
//    DBGPUT("all done printing\n");

    return 0;
}


int
sdl_paint_gfx_image(const char *fname, const char *type, int x, int y) {

    char filename[1024];
    
    sprintf(filename, "%s%s/%s", base_gfx_path, type, fname);
    // DBGPUT("Painting %s\n", filename);

    return paint_image(filename, x, y);
}

    
int
sdl_load_gfx_image(const char *fname, const char *type) {

    int idx = -1;
    char filename[1024];
    
    sprintf(filename, "%s%s/%s", base_gfx_path, type , fname);

    idx = find_image(filename);
    // DBGPUT("Searched for and found %s at %d\n", filename, idx);
    
    if (idx < 0) {
        return load_plain_image(filename, tileInfo->num_tiles++);
    }
    else {
	return idx;
    }
}



int
fill_area(int image_index, int tile_num, int x1, int y1, int x2, int y2) {
    
    int j = 0;
    int i = 0;
    
//    term_data *td = sdl_subwindows[BIG_TERM_IDX]; // fix
    TileInformation *ti = tileInfo;
    

    if (image_index < 0 || image_index >= IMAGE_ARRAY_SIZE ||
	!ti->tiles[image_index]) {
	ERRORMSG("Image with idx %d not legal/found.\n", image_index);
	return -1;
    }

    if (tile_num < 0) {
	ERRORMSG("Tile-num %d not legal/found.\n", tile_num);
	return -1;
    }

    {
	s16b attr = image_index + LANGBAND_GFX_START;
	s16b tile = tile_num + LANGBAND_GFX_START;

	int xstart = x1 * 2;
	int xrange = x2 - x1;
    
	for (i = 0; i < xrange; i++) {
	    for (j = y1; j < y2; j++) {
		int spot = xstart + i;
		//DBGPUT("Filling %d,%d at %d,%d\n", attr, tile, spot, j);
		Term_queue_char(spot, j, attr, tile, 0, 0);
		//Term_queue_char(spot+1, j, -1, -1, 0, 0);
	    }
	}
    }


    return 0;
}



/*#define SCALETOCOLOR(x) (x=((x)*63+((x)-1)))*/
#define ScaleToColor(x) ((x)=((x)*60)+15)
/*#define ScaleToColor(x) ((x)=((x)*63))*/
void init_color_data_sdl() {
    Uint8 i;
    
    for (i = 0; i < 16; ++i) {
	color_data_sdl[i].unused = 255; /* no reason. */
	if(!color_data_sdl[i].r && !color_data_sdl[i].g && !color_data_sdl[i].b)
	    continue;
	ScaleToColor(color_data_sdl[i].r);
	ScaleToColor(color_data_sdl[i].g);
	ScaleToColor(color_data_sdl[i].b);
    }
}




/*
 *
 * Function hooks needed by "Term"
 *
 * This is really the meat of the display module.
 * If you implement a new graphics engine, you implement several of these
 * hooks (at least pict_hook, aka Term_pict_???()), and replace mine with
 * them in term_data_link(). Ask me if you have any questions.
 * Perhaps by the time you read this, a short hacker's guide is already
 * available.
 *
 * I think that it is important to make sure that new modules using main-sdl.c 
 * remain compatible with future revisions of main-sdl.c. I don't want other
 * modules falling behind when I make important changes. I also think that it
 * is an easy thing to avoid. -- Greg V.
 *
 */


/*
 * XXX XXX XXX Nuke an old "term"
 *
 * This function is called when an old "term" is no longer needed.  It should
 * do whatever is needed to clean up before the program exits, such as wiping
 * the screen, restoring the cursor, fixing the font, etc.  Often this function
 * does nothing and lets the operating system clean up when the program quits.
 */

// This is broken!
static void Term_nuke_sdl(angband_zterm *t) {

    LangbandFrame *lf = (LangbandFrame *)(t->data);
    ERRORMSG("NUKE\n");

    if (lf->visible) {
	sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;
	if (wc->face) {
	    SDL_FreeSurface(wc->face); /* what happen! someone set up us the bomb! */
	    wc->face = NULL;
	}
    }
}



/*
 * XXX XXX XXX Do a "user action" on the current "term"
 *
 * This function allows the visual module to do things.
 *
 * This function is currently unused, but has access to the "info"
 * field of the "term" to hold an extra argument.
 *
 * In general, this function should return zero if the action is successfully
 * handled, and non-zero if the action is unknown or incorrectly handled.
 */
static errr Term_user_sdl(int n)
{
    // to avoid warning
    n = 0;

	/* XXX XXX XXX Handle the request */

	/* TODO What? Huh? */

	/* Unknown */
	return (1);
}



/*
 * XXX XXX XXX Do a "special thing" to the current "term"
 *
 * This function must react to a large number of possible arguments, each
 * corresponding to a different "action request" by the "term.c" package.
 *
 * The "action type" is specified by the first argument, which must be a
 * constant of the form "TERM_XTRA_*" as given in "term.h", and the second
 * argument specifies the "information" for that argument, if any, and will
 * vary according to the first argument.
 *
 * In general, this function should return zero if the action is successfully
 * handled, and non-zero if the action is unknown or incorrectly handled.
 */
errr Term_xtra_sdl(int n, int v)
{

    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;
    SDL_Event event; /* just a temporary place to hold an event */

    /* Analyze */
    switch (n)
    {
    case TERM_XTRA_EVENT:
	
	do {
	    if (v) {
		if (!SDL_WaitEvent(&event)) return(0); /* TODO handle errors */
		v = 0;
	    } else 
	    {
		if(!SDL_PollEvent(&event)) return(0);
	    }
	    
	    if (event.type == SDL_QUIT) 
	    {
		z_quit("Goodbye.");
	    }
	    else if (event.type == SDL_MOUSEBUTTONDOWN) {
//		DBGPUT("Mouse button %d pressed at (%d,%d)\n",
//			event.button.button, event.button.x, event.button.y);
		mouse_clicked(event.button.button, event.button.x, event.button.y);
	    }

	    else if (event.type == SDL_KEYDOWN) {
		
		if (event.key.state == SDL_PRESSED) {
		    
		    /* Various frivolous hacks. */
		    switch(event.key.keysym.sym)
		    {
//			char buf[1024];
//			FILE *tmp;
//			int i;
#if ALLOW_BAD_GFX_CHANGE
			/* Try to toggle graphics. */
		    case SDLK_SCROLLOCK:
			use_graphics = !use_graphics;
			data[0].t.higher_pict = !data[0].t.higher_pict;
			    
			/*sprintf(buf, "%s-%s.prf", (use_graphics ? "graf" : "font"), ANGBAND_SYS);*/
			break;
#endif

#if 0
			/* Try to save a screenshot. */
		    case SDLK_PRINT:
			if (SDL_SaveBMP(sdl_subwindows[0]->face, "newshot.bmp")) 
			{
			    INFOMSG("You fail to get the screenshot off!");
			    break;
			}
			for (i = 0; i < 999; ++i) {
			    snprintf(buf, 1024, "%03d.bmp", i);
			    if ((tmp = fopen(buf, "rb")) != NULL)
			    {
				fclose(tmp);
				continue;
			    }
			    rename("newshot.bmp", buf);
			}
			INFOMSG("*click*");
			break;
#endif
		    default:
			break;
		    } /* switch */
			
		    Multikeypress(SDL_keysymtostr(&event.key.keysym));
			
		} /* SDL_PRESSED */ 
	    } 
	    
	} while (SDL_PollEvent(NULL));
	
	return (0);
    
    case TERM_XTRA_FLUSH:
	
	/* XXX XXX XXX Flush all pending events */
	/* This action should handle all events waiting on the */
	/* queue, optionally discarding all "keypress" events, */
	/* since they will be discarded anyway in "term.c". */
	/* This action is required, but is often not "essential". */
	
	/*DBGPUT("TERM_XTRA_FLUSH\n");*/
	
	while (SDL_PollEvent(&event)) /* do nothing */ ;
	
	return (0);
	
    case TERM_XTRA_CLEAR:
	
	/* XXX XXX XXX Clear the entire window */
	/* This action should clear the entire window, and redraw */
	/* any "borders" or other "graphic" aspects of the window. */
	/* This action is required. */
	
	if (!(wc->face)) return 1;
	if (!(lf->visible)) return 0;

	/* a NULL dstrect fills the entire window */
	/* XXX the color 0 will not necessarily work for 8-bit modes */
	{
	    SDL_Rect dr;
	    int retval;
	    dr.x = lf->xoffset;
	    dr.y = lf->yoffset;
//	    dr.w = lf->frame_width;
//	    dr.h = lf->frame_height;
	    dr.w = lf->allowed_width;
	    dr.h = lf->allowed_height;
	    //DBGPUT("Doing clear of %s with background %p.\n", lf->name, wc->background);
	    if (wc->background) {
		retval = SDL_BlitSurface(wc->background, 0, wc->face, &dr);
	    }
	    else {
		retval = SDL_FillRect(wc->face, &dr, 0);
	    }
	    
	    SDL_UpdateRect(wc->face, dr.x, dr.y, dr.w, dr.h);

	    //SDL_UpdateRect(td->face, dr.x, dr.y, dr.w, dr.h);
//	    DBGPUT("Clear on window %p [%d,%d,%d] [%d,%d,%d,%d] gave %d\n",
//		   td, td->face->format->BitsPerPixel, td->face->w, td->face->h,
//		    dr.x, dr.y, dr.w, dr.h, retval);
/*
  dr.x = 200;
  dr.y = 200;
  dr.w = 200;
  dr.h = 200;
  SDL_FillRect(td->face, &dr, 1400);
*/
	}
	
	return 0;

    case TERM_XTRA_SHAPE:

	/* XXX XXX XXX Set the cursor visibility (optional) */
	/* This action should change the visibility of the cursor, */
	/* if possible, to the requested value (0=off, 1=on) */
	/* This action is optional, but can improve both the */
	/* efficiency (and attractiveness) of the program. */
	if (!(lf->visible)) return 0;
	wc->cursor_on = n ? TRUE : FALSE;

	return (0);

    case TERM_XTRA_FROSH:

	/* XXX XXX XXX Flush a row of output (optional) */
	/* This action should make sure that row "v" of the "output" */
	/* to the window will actually appear on the window. */
	/* This action is optional on most systems. */

	if (!wc->face) return -1;
	if (wc->prefer_fresh) return 0;
	if (!(lf->visible)) return 0;
	//DBGPUT("Frosh on window %p\n", td);
	SDL_UpdateRect(wc->face, 0 + lf->xoffset, v * lf->tile_height + lf->yoffset,
		       lf->frame_width, lf->tile_height); 

	return (0);

    case TERM_XTRA_FRESH:

	/* XXX XXX XXX Flush output (optional) */
	/* This action should make sure that all "output" to the */
	/* window will actually appear on the window. */
	/* This action is optional if all "output" will eventually */
	/* show up on its own, or when actually requested. */

	if (!(lf->visible)) return 0;
	if (!wc->face) return -1;
	if ((wc->face->flags & SDL_HWSURFACE && wc->face->flags & SDL_DOUBLEBUF)
	    || wc->prefer_fresh) 
	{
	    //DBGPUT("Fresh on window %p\n", td);
	    SDL_Flip(wc->face);
	}

	return (0);

    case TERM_XTRA_NOISE:

	/* XXX XXX XXX Make a noise (optional) */
	/* This action should produce a "beep" noise. */
	/* This action is optional, but nice. */

	return (0);

    case TERM_XTRA_SOUND:

	if (use_sound) {
	    int soundChannel;
	    //DBGPUT("Playing sound %d\n", v);
	    Mix_Chunk *ptr = (Mix_Chunk *)sound_bites[v]->handle;
	    soundChannel = Mix_PlayChannel(-1, ptr, 0);
	}

	return (0);

    case TERM_XTRA_BORED:

	/* XXX XXX XXX Handle random events when bored (optional) */
	/* This action is optional, and not important */

	/* TODO add nifty effects here, I guess? */


	return (0);

    case TERM_XTRA_REACT:

	/* XXX XXX XXX React to global changes (optional) */
	/* For example, this action can be used to react to */
	/* changes in the global "color_table[256][4]" array. */
	/* This action is optional, but can be very useful */
	if (!(lf->visible)) return 0;
	SDL_Flip(wc->face); /* I guess... XXX XXX XXX */ 

	return (0);

    case TERM_XTRA_ALIVE:

	/* XXX XXX XXX Change the "hard" level (optional) */
	/* This action is used if the program changes "aliveness" */
	/* by being either "suspended" (v=0) or "resumed" (v=1) */
	/* This action is optional, unless the computer uses the */
	/* same "physical screen" for multiple programs, in which */
	/* case this action should clean up to let other programs */
	/* use the screen, or resume from such a cleaned up state. */
	/* This action is currently only used on UNIX machines */

	/* TODO this should probably do something... no, nevermind. */

	return (0);

    case TERM_XTRA_LEVEL:

	/* XXX XXX XXX Change the "soft" level (optional) */
	/* This action is used when the term window changes "activation" */
	/* either by becoming "inactive" (v=0) or "active" (v=1) */
	/* This action is optional but can be used to do things like */
	/* activate the proper font / drawing mode for the newly active */
	/* term window.  This action should NOT change which window has */
	/* the "focus", which window is "raised", or anything like that. */
	/* This action is optional if all the other things which depend */
	/* on what term is active handle activation themself. */

	return (0);

    case TERM_XTRA_DELAY:

	/* XXX XXX XXX Delay for some milliseconds (optional) */
	/* This action is important for certain visual effects, and also */
	/* for the parsing of macro sequences on certain machines. */
	if (!(lf->visible)) return 0;
	SDL_Delay(v);

	return (0);
    }

    /* Unknown or Unhandled action */
    return 1;
}


/*
 * XXX XXX XXX Erase some characters
 *
 * This function should erase "n" characters starting at (x,y).
 *
 * You may assume "valid" input if the window is properly sized.
 */
static errr Term_wipe_sdl(int x, int y, int n)
{
    SDL_Rect dr;
    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;

    //term_data *td = (term_data*)(Term->data);

    if (!(lf->visible)) return 0;
    //printf("wipe[%p] %d, %d, %d\n", td, x, y, n);

    //if (!td->gt || !td->font_data) return 0;
    if (!wc->font_data) return 0;

    if (lf->azt->higher_pict || (wc->cursor_magic && (wc->cx == x && wc->cy == y)))
    {
	dr.w = n * lf->tile_width;
	dr.h = lf->tile_height;
	dr.x = lf->xoffset + lf->tile_width * x;
	dr.y = lf->yoffset + lf->tile_height * y;
	//DBGPUT("Clean area %d,%d,%d,%d on %p\n", dr.w, dr.h, dr.x, dr.y, td->face);
	if (wc->background) {
	    SDL_Rect sr;
	    sr.x = lf->tile_width * x;
	    sr.y = lf->tile_height * y;
	    sr.w = n * lf->tile_width;
	    sr.h = lf->tile_height;
	    SDL_BlitSurface(wc->background, &sr, wc->face, &dr);
	}
	else {
	    SDL_FillRect(wc->face, &dr, 0);
	}
	//DBGPUT("galpe");
	if (wc->cx == x && wc->cy == y)
	{
	    SDL_UpdateRect(wc->face, dr.x, dr.y, dr.w, dr.h);
			
	    if (wc->cursor_magic && lf->azt->higher_pict)
	    {
		dr.x = lf->xoffset + CURS_MAG_X * lf->tile_width;
		dr.y = lf->yoffset + CURS_MAG_Y * lf->tile_height;
		dr.w = 2 * lf->tile_width;
		dr.h = 2 * lf->tile_height;
		SDL_FillRect(wc->face, &dr, 0);
		SDL_UpdateRect(wc->face, dr.x, dr.y, dr.w, dr.h);
	    }
	}
    }

    //DBGPUT("leave wipe");
    /* Success */
    return 0;
}


/*
 * XXX XXX XXX Display the cursor
 *
 * This routine should display the cursor at the given location
 * (x,y) in some manner.  On some machines this involves actually
 * moving the physical cursor, on others it involves drawing a fake
 * cursor in some form of graphics mode.  Note the "soft_cursor"
 * flag which tells "term.c" to treat the "cursor" as a "visual"
 * thing and not as a "hardware" cursor.
 *
 * You may assume "valid" input if the window is properly sized.
 *
 * You may use the "Term_grab(x, y, &a, &c)" function, if needed,
 * to determine what attr/char should be "under" the new cursor,
 * for "inverting" purposes or whatever.
 */
static errr Term_curs_sdl(int x, int y) {

    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;

//	term_data *td = (term_data*)(Term->data);
    SDL_Rect dr, mr, gr; /* cursor destination, magic, and graphic tile loc. */
    Uint8 a, c;

    return 0;
	
    if (wc->cursor_on) 
    {
	wc->cx = x;
	wc->cy = y;
	dr.x = x * lf->tile_width;
	dr.y = y * lf->tile_height;
	dr.w = lf->tile_width;
	dr.h = lf->tile_height;
		
	if (wc->cursor_magic && lf->azt->higher_pict && wc->gt) {

	    mr.x = CURS_MAG_X * lf->tile_width;
	    mr.y = CURS_MAG_Y * lf->tile_height;
	    mr.w = lf->tile_width  * 2;
	    mr.h = lf->tile_height * 2;
#if 1 /* the following code assumes knowledge of the term struct */
	    /* it also allows one to view the magnified tile with all detail
	     * even if tiles in the display are scaled down, thus losing detail */
	    a = Term->scr->a[y][x];
	    c = Term->scr->c[y][x];
	    if (a & 0x80 && c & 0x80) /* don't bother magnifying letters */
	    {
		a &= 0x7F;
		c &= 0x7F;
		gr.x = c * wc->gt->w;
		gr.y = a * wc->gt->h;
		gr.w = wc->gt->w;
		gr.h = wc->gt->h;
		SDL_FastScaleBlit(wc->gt->face, &gr, wc->face, &mr);
		SDL_UpdateRect(wc->face, mr.x, mr.y, mr.w, mr.h);
	    }
#else
	    SDL_FastScaleBlit(wc->face, &dr, wc->face, &mr);
	    SDL_UpdateRect(wc->face, mr.x, mr.y, mr.w, mr.h);
#endif
	}
	SDL_DrawCursor(wc->face, &dr);
    }


    /* Success */
    return 0;
}

inline errr SDL_DrawChar (SDL_Surface *f, SDL_Rect *dr, FontData *fd, s16b a, s16b c)
{
    SDL_Rect sr;
    
    if (!f || !dr || !fd) return -1;
    //DBGPUT("type of %c is %d\n", c, fd->font_type);

    if (FALSE) { }
#ifdef ALLOW_TTF
    else if (fd->font_type == FONT_TYPE_TTF) {
	display_char(f, dr, fd, a, c);
    }
#endif
    else if (fd->font_type == FONT_TYPE_HEX) {

	sr.w = fd->width;
	sr.h = fd->height;
	
	sr.x = 0;
	sr.y = c * fd->height;
	
	/* XXX Force SDL, or whatever it wraps, to make the text the color we want
	 * by tweaking the palette. This really is slower than blits between
	 * surfaces with identical color formats but it's so easy and convenient!
	 * Anyway, it seems to be fast enough on my machine. */
	
	/* Assumes 16 colors. (hence the 0xf.) Currently that's all I have
	 * defined in my color table. XXX XXX XXX */

	SDL_SetColors(fd->theFont, &(color_data_sdl[a&0xf]), 0xff, 1); 
//	SDL_SetColors(fd->face, &(color_data_sdl[a&0xf]), 0xff, 1); 
	SDL_SetColorKey(fd->theFont, SDL_SRCCOLORKEY, 0);

	//DBGPUT("blit %c\n", c);
	/*if(SDL_MUSTLOCK(f)) SDL_LockSurface(f);*/
	SDL_BlitSurface(fd->theFont, &sr, f, dr);
	/*if(SDL_MUSTLOCK(f)) SDL_UnlockSurface(f);*/
    }
    else {
	ERRORMSG("Unknown/unsupported font-type %d\n",  fd->font_type);
	return -1;
    }
    
    return 0; /* This'll never fail... right? */
}

// nasty nasty hack
static int charfun_wipe = 1;

/* The following draws one character to the Term, using font_data. */
inline static errr Term_char_sdl (int x, int y, s16b a, s16b c) {
    /*SDL_Rect sr, dr;*/

    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;
	
    int xadj, yadj;
    SDL_Rect dr;

    if (!(lf->visible)) return 0;
	
//	if (td != &(local_terms[0])) {
//	    DBGPUT("Draw %c %d,%d (%d)\n", c, x, y, charfun_wipe);
//	}
	
    if (!wc->face) {
	ERRORMSG("Drawing to invalid term %s\n", lf->name);
	return -1;
    }
	
    if (charfun_wipe) 
	Term_wipe_sdl(x, y, 1);
    
    /* try to center chars. */
    xadj = (lf->tile_width > wc->font_data->width) ? (lf->tile_width - wc->font_data->width) : 0; 
    xadj >>= 1;
    yadj = (lf->tile_height > wc->font_data->height) ? (lf->tile_height - wc->font_data->height) : 0;
    yadj >>= 1;

	
    dr.x = (x * lf->tile_width)  + xadj + lf->xoffset;
    dr.y = (y * lf->tile_height) + yadj + lf->yoffset;
    dr.w = lf->tile_width;
    dr.h = lf->tile_height;

    if (SDL_DrawChar(wc->face, &dr, wc->font_data, a, c)) 
    {
	DBGPUT("Early return\n");
	return -1;
    }
	
    if (wc->cursor_on && wc->cx == x && wc->cy == y) {

	SDL_UpdateRect(wc->face, x * lf->tile_width,
		       y * lf->tile_height,
		       lf->tile_width, lf->tile_height);
	wc->cx = wc->cy = -1;
    }
	
	
    /* Success */
    return 0;
}

/*
 * XXX XXX XXX Draw a "picture" on the screen
 *
 * This routine should display the given attr/char pair at the
 * given location (x,y).  This function is only used if one of
 * the flags "always_pict" or "higher_pict" is defined.
 *
 * You must be sure that the attr/char pair, when displayed, will
 * erase anything (including any visual cursor) that used to be
 * at the given location.  On many machines this is automatic, on
 * others, you must first call "Term_wipe_xxx(x, y, 1)".
 *
 * With the "higher_pict" flag, this function can be used to allow
 * the display of "pseudo-graphic" pictures, for example, by using
 * "(a&0x7F)" as a "row" and "(c&0x7F)" as a "column" to index into
 * a special auxiliary pixmap of special pictures.
 *
 * With the "always_pict" flag, this function can be used to force
 * every attr/char pair to be drawn one at a time, instead of trying
 * to "collect" the attr/char pairs into "strips" with similar "attr"
 * codes, which would be sent to "Term_text_xxx()".
 *
 * This is the implementation of the text and 2D tile graphics display.
 * TODO Implement and wrap other graphics engines. They should replace
 * Term_pict_sdl as the hook in term, probably.
 *
 */


errr Term_pict_sdl(int x, int y, int n, const s16b *ap, const s16b *cp, const s16b *tap, const s16b *tcp) {

    s16b a, c, ta, tc;
    
    SDL_Rect sr, dr, trans_sr;
    
    int grp1 = -1;
    int grp2 = -1;
    int i;
    
    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;
	
    if (!(lf->visible)) return 0;
    
    //DBGPUT("*Doing tile (%d,%d) (%d,%d,%d) (%p,%p)\n", x, y, n, *ap, *cp,
//	    td->gt, td->gt->face);

    if (!wc->gt || !wc->face) {
	DBGPUT("SCREAM at %d,%d\n", x, y);
	return -1;
	
//	Term_text_sdl(x, y, n, *ap, cp);
    }

    dr.x = x * lf->tile_width  + lf->xoffset;
    dr.y = y * lf->tile_height + lf->yoffset;    

	
    for (i = 0; i < n; ++i) {
	
	a = *ap++;
	c = *cp++;

	if ((a == -1) && (c == -1)) return 0; // hack

	ta = *tap++;
	tc = *tcp++;
	    
	
        // always gervais
	if (a >= 0) {
	    grp1 = a - LANGBAND_GFX_START;
	}
	else {
	    DBGPUT("negative creep\n");
	}
	if (ta >= 0)
	    grp2 = ta - LANGBAND_GFX_START;

	if (grp1 >= 0 && tileInfo->tiles[grp1]) {
	    //int grp1_tilecols = tileInfo->tile_columns[grp1];
	    int grp1_tilecols = tileInfo->tiles[grp1]->w / lf->tile_width;

	    sr.x = ((c - LANGBAND_GFX_START) % grp1_tilecols) * lf->tile_width; //td->gt->dw; //td->fnt->twid;
	    sr.y = ((c - LANGBAND_GFX_START) / grp1_tilecols) * lf->tile_height; //td->gt->dh; //td->fnt->hgt;	    
	}

	if (grp2 < 0 || !tileInfo->tiles[grp2] ) {
	    grp2 = grp1;
	    trans_sr.x = sr.x;
	    trans_sr.y = sr.y;
	}
	else {
	    //int grp2_tilecols = tileInfo->tile_columns[grp2];
	    int grp2_tilecols = tileInfo->tiles[grp2]->w / lf->tile_width;
	    trans_sr.x = ((tc - LANGBAND_GFX_START) % grp2_tilecols) * lf->tile_width; //td->gt->dw; //td->fnt->twid;
	    trans_sr.y = ((tc - LANGBAND_GFX_START) / grp2_tilecols) * lf->tile_height; //td->gt->dw; //td->fnt->hgt;   
	}

	dr.w = lf->tile_width;// * 2;
	dr.h = lf->tile_height;

	sr.w = dr.w;
	sr.h = dr.h;
	trans_sr.w = dr.w;
	trans_sr.h = dr.h;

//	DBGPUT("-Doing tile (%d, %s) (%d,%d,%d,%d) (%d,%d,%d,%d)\n", grp1, tileInfo->tile_files[grp1],
//		sr.x, sr.y, sr.w, sr.h, dr.x, dr.y, dr.w, dr.h);

	
	if (grp1 >= 0  && !tileInfo->tiles[grp1]) {
	    SDL_FillRect(wc->face, &dr, 0);
//		DBGPUT("Ignoring group %d\n", grp1);
	}
	// most common case
	else if (((sr.x == trans_sr.x) && (sr.y == trans_sr.y) && (grp1 == grp2)) ||
		 !((ta >= LANGBAND_GFX_START) && (tc >= LANGBAND_GFX_START)))  {// ever happen?
	    // paint blank pixel, then blit

	    if (!(ta == -1 && tc == -1)) {
		SDL_FillRect(wc->face, &dr, 0); // whould this be background too?
	    }
	    else if (wc->background) {
		SDL_Rect sr;
		sr.x = lf->tile_width * x;
		sr.y = lf->tile_height * y;
		sr.w = n * lf->tile_width;
		sr.h = lf->tile_height;
		SDL_BlitSurface(wc->background, &sr, wc->face, &dr);
	    }
	    //DBGPUT("going blit\n");
	    SDL_BlitSurface(tileInfo->tiles[grp1], &sr, wc->face, &dr);
	}
	// we have a letter on a background, transparent
	else if (grp1 < 0 && grp2 >= 0 && tileInfo->tiles[grp2]) {
	    //DBGPUT("Got a %d letter %c on background\n", a, c);
	    SDL_FillRect(wc->face, &dr, 0);
	    SDL_BlitSurface(tileInfo->tiles[grp2], &trans_sr, wc->face, &dr);
	    charfun_wipe = 0;
	    Term_char_sdl(x, y, a, c);
	    charfun_wipe = 1;
	}
	else { // transparent stuff
	    SDL_FillRect(wc->face, &dr, 0);
	    SDL_BlitSurface(tileInfo->tiles[grp2], &trans_sr, wc->face, &dr);
	    SDL_BlitSurface(tileInfo->tiles[grp1], &sr, wc->face, &dr);
	}
	//DBGPUT("after blit\n");
	SDL_UpdateRect(wc->face, dr.x, dr.y, dr.w, dr.h);



//	SDL_BlitSurface(tileInfo->tiles[grp2], &trans_sr, td->face, &dr);
//	SDL_BlitSurface(tileInfo->tiles[grp1], &sr, td->face, &dr);

	/*
	  
	if (sr.x > td->gt->face->w)
	{
		SDL_FillRect(td->face, &dr, SDL_MapRGB(td->face->format, 255, 64, 64));
		DBGPUT("OOBound (%d, %d) (%d, %d bitmap)", c&0x7f, a&0x7f, sr.x, sr.y); 
	} else
	{
		SDL_BlitSurface(td->gt->face, &sr, td->face, &dr);
	}
	if (td->cx == x && td->cy == y)
	{
		SDL_UpdateRect(td->face, dr.x, dr.y, dr.w, dr.h);
	}
	*/

    }
    
    /* Success */
    return 0;
	
}




/*
 * XXX XXX XXX Display some text on the screen
 *
 * This function should actually display a string of characters
 * starting at the given location, using the given "attribute",
 * and using the given string of characters, which is terminated
 * with a nul character and which has exactly "n" characters.
 *
 * You may assume "valid" input if the window is properly sized.
 *
 * You must be sure that the string, when written, erases anything
 * (including any visual cursor) that used to be where the text is
 * drawn.  On many machines this happens automatically, on others,
 * you must first call "Term_wipe_xxx()" to clear the area.
 *
 * You may ignore the "color" parameter if you are only supporting
 * a monochrome environment, unless you have set the "draw_blanks"
 * flag, since this routine is normally never called to display
 * "black" (invisible) text, and all other colors should be drawn
 * in the "normal" color in a monochrome environment.
 *
 * Note that this function must correctly handle "black" text if
 * the "always_text" flag is set, if this flag is not set, all the
 * "black" text will be handled by the "Term_wipe_xxx()" hook.
 */
static errr Term_text_sdl(int x, int y, int n, s16b a, const s16b *s) {

    LangbandFrame *lf = (LangbandFrame *)(Term->data);

    if (!(lf->visible)) return 0;

    while(n > 0) {
	if (a == -1 && *s == -1) {
	}
	else {
	    Term_char_sdl(x, y, a, *s);
	}
	++x; --n; ++s;
    }

    
    /* Success, we hope. */
    return (0);
}




/*
 * XXX XXX XXX Instantiate a "term_data" structure
 *
 * This is one way to prepare the "term_data" structures and to
 * "link" the various informational pieces together.
 *
 * This function assumes that every window should be 80x24 in size
 * (the standard size) and should be able to queue 256 characters.
 * Technically, only the "main screen window" needs to queue any
 * characters, but this method is simple.
 *
 * Note that "activation" calls the "Term_init_xxx()" hook for
 * the "term" structure, if needed.
 */
static void term_data_link(LangbandFrame *lf, int cols, int rows) {
    
    angband_zterm *t = lf->azt;
    
    //DEBUGPUT("link %p %p\n", td, t);


    term_init(t, cols, rows, 256);
    //DEBUGPUT("after init\n");
    lf->columns = cols;
    lf->rows = rows;
	

    lf->frame_width  = lf->columns * lf->tile_width;
    lf->frame_height = lf->rows * lf->tile_height;
	
    /* XXX XXX XXX Choose "soft" or "hard" cursor */
    /* A "soft" cursor must be explicitly "drawn" by the program */
    /* while a "hard" cursor has some "physical" existance and is */
    /* moved whenever text is drawn on the screen.  See "term.c". */
    t->soft_cursor = TRUE;

    /* XXX XXX XXX Avoid the "corner" of the window */
    /* t->icky_corner = TRUE; */

    /* XXX XXX XXX Use "Term_pict()" for all data */
    /* See the "Term_pict_xxx()" function above. */
    /* t->always_pict = TRUE; */

    /* XXX XXX XXX Use "Term_pict()" for "special" data */
    /* See the "Term_pict_xxx()" function above. */
    t->higher_pict = TRUE; /* XXX Should this be set or not? */

    /* XXX XXX XXX Use "Term_text()" for all data */
    /* See the "Term_text_xxx()" function above. */
    /* t->always_text = TRUE; */

    /* XXX XXX XXX Ignore the "TERM_XTRA_BORED" action */
    /* This may make things slightly more efficient. */
    t->never_bored = TRUE;

    /* XXX XXX XXX Ignore the "TERM_XTRA_FROSH" action */
    /* This may make things slightly more efficient. */
    /* t->never_frosh = TRUE; */

    /* Erase with "white space" */
    t->attr_blank = TERM_WHITE;
    t->char_blank = ' ';

    /* Prepare the init/nuke hooks */
    //td->init_hook = t->init_hook = Term_init_sdl;
    lf->nuke_hook = t->nuke_hook = Term_nuke_sdl;

    /* Prepare the template hooks */
    lf->user_hook = t->user_hook = Term_user_sdl;
    lf->xtra_hook = t->xtra_hook = Term_xtra_sdl;
    lf->wipe_hook = t->wipe_hook = Term_wipe_sdl;
    lf->curs_hook = t->curs_hook = Term_curs_sdl;
    lf->text_hook = t->text_hook = Term_text_sdl;


    lf->pict_hook = Term_pict_sdl;
    t->pict_hook = lf->pict_hook;

    //DEBUGPUT("First Hook for %p is %p %p\n", t, t->xtra_hook, t->init_hook);

    /* Remember where we came from */
    t->data = (vptr)(lf);
	
    //DBGPUT("Second Hook for %p is %p %p\n", t, t->xtra_hook, t->init_hook);
	
    /* Activate it */
    Term_activate(t);
	
    //DBGPUT("Last Hook for %p is %p %p\n", t, t->xtra_hook, t->init_hook);
}

FontData *
sdl_load_font(const char *fname, int ptsize) {

    FontData *fd = NULL;
    const char *extension = strrchr(fname, '.');

    if (!extension) {
	ERRORMSG("Don't know font-type for file %s.\n",
		 fname);
	return NULL;
    }

    extension++;

    if (FALSE) { }
    else if (!strcmp(extension, "hex")) {
	fd = load_hex_font(fname, TRUE);
    }
#ifdef ALLOW_TTF
    else if (!strcmp(extension, "ttf")) {
	fd = load_ttf_font(fname, ptsize);
    }
#endif
    else {
	ERRORMSG("Don't know how to handle font-type '%s' for file %s\n",
		 extension, fname);
    }

    return fd;
}

LangbandFrame *
sdlify_frame(LangbandFrame *lf) {

    sdl_winconnection *wc = NULL;
    int max_col = 0, max_row = 0;
    FontData *fd = NULL;

    if (!lf) {
	ERRORMSG("Illegal frame given to sdlify_term_frame().\n");
	return NULL;
    }

    wc = malloc(sizeof(sdl_winconnection));
    WIPE(wc, sdl_winconnection);

    lf->azt = malloc(sizeof(angband_zterm));
    WIPE(lf->azt, angband_zterm);

    lf->ui_connection = wc;

    // DBGPUT("Making window %d with tw %d and th %d\n", num, tile_width, tile_height);

    if (!(lf->fontname)) {
	ERRORMSG("No legal fontname provided for frame '%s'.", lf->name);
	return NULL;
    }
    
    fd = sdl_load_font(lf->fontname, 16);

    if (!fd) {
	ERRORMSG("Something screwed up with font-loading of '%s'\n", lf->fontname);
	return NULL;
    }
    
    wc->font_data = fd;
	
    if (lf->tile_width < wc->font_data->width)
	lf->tile_width = wc->font_data->width; 
    if (lf->tile_height < wc->font_data->height)
	lf->tile_height = wc->font_data->height; 

    wc->cursor_on = TRUE;
    wc->cursor_magic = TRUE;
    wc->background = NULL;

    if (lf->allowed_width < 1) {
	lf->allowed_width = theWindow->w - lf->xoffset;
    }
    if (lf->allowed_height < 1) {
	lf->allowed_height = theWindow->h - lf->yoffset;
    }

    if (lf->tile_width == 0 || lf->tile_height == 0) {
	ERRORMSG("Somehow tilewidth is %d and tileheight is %d, this is illegal.\n",
		 lf->tile_width, lf->tile_width);
	return NULL;
    }
    else {
        max_col = lf->allowed_width / lf->tile_width;
	max_row = lf->allowed_height / lf->tile_height;
    }

    //DBGPUT("gah %p %p\n", Term, lf->azt);
    term_data_link(lf, max_col, max_row);

    
    wc->face = theWindow;

    //DBGPUT("returning %p\n", lf);
    
    return lf;
}

/* Background/Texture related stuff */
int
loadBackground(LangbandFrame *lf, const char *fname) {

    int i, j;
    SDL_Surface *bg = NULL;
    SDL_Surface *texture = NULL;
    sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;

    // clean up..
    if (wc->background) {
	SDL_FreeSurface(wc->background);
	wc->background = NULL;
    }
    // how do we treat other backgrounds and default bacgroundfiles?
    
    if (!fname || strlen(fname) < 2) {
	// this means we're mostly called to clear the background, no warning needed
	DBGPUT("Fell out with %s.\n", fname);
	return -2;
    }
    
    texture = IMG_Load(fname);
    
    if (!texture) {
	ERRORMSG("Unable to find background '%s'\n", fname);
	return -1;
    }

//    SDL_SetAlpha(texture, SDL_SRCALPHA,SDL_ALPHA_TRANSPARENT);
    
    bg = SDL_CreateRGBSurface(SDL_SWSURFACE|SDL_SRCALPHA,
			      lf->frame_width, lf->frame_height, 32,
			      texture->format->Rmask,
			      texture->format->Gmask,
			      texture->format->Bmask,
			      texture->format->Amask);
    /*
    SDL_SetColorKey(texture,SDL_SRCCOLORKEY,0);

    SDL_SetColorKey(bg,SDL_SRCCOLORKEY,0);
    */
    
//    SDL_SetAlpha(bg, SDL_SRCALPHA,SDL_ALPHA_TRANSPARENT);
    

//    bg->format->alpha = texture->format->alpha;
    
    //bg->format = texture->format;
    // do this in the y-direction too!
    for (j=0; j < lf->frame_height; j += texture->h) {
	for (i=0; i < lf->frame_width; i += texture->w) {
	    SDL_Rect dr;
	    dr.x = i;
	    dr.y = j;
	    dr.w = texture->w;
	    dr.h = texture->h;
	    //DBGPUT("Pasting to %d,%d\n", dr.x, dr.y);
	    JAI_BlitSurfaceAlpha(texture, 0, bg, &dr);
	}
    }
/*
  DBGPUT("texture is %d %d %d, bg is %d %d %d\n",
	    texture->format->BitsPerPixel, texture->w, texture->h,
	    bg->format->BitsPerPixel, bg->w, bg->h);
*/
    bg = SDL_DisplayFormatAlpha(bg);

    wc->background = bg;
    /*
    {
	SDL_Rect dr;
	dr.x = misc->xoffset; dr.y = misc->yoffset;
	dr.w = misc->term_width; dr.h = misc->term_height;
	SDL_BlitSurface(bg, 0, misc->face, &dr);
	SDL_UpdateRect(misc->face, dr.x, dr.y, dr.w, dr.h);
    }
    */
    SDL_FreeSurface(texture);
    
    return 0;
    
}

int
sdl_textureBackground(int term_num, const char *fname, int alpha) {

    LangbandFrame *lf = predefinedFrames[term_num];
    sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;

    char filename[1024];
    int retval = 0;

    if (!lf) {
	ERRORMSG("No legal frame found for key '%d'.\n", term_num);
	return -3;
    }
    
    if (fname && strlen(fname)>1) {
	sprintf(filename,"%s%s/%s", base_gfx_path, "textures", fname);

	retval = loadBackground(lf, filename);
    }
    else {
	retval = loadBackground(lf, NULL);
    }
    
    if (wc->background && alpha >= 0) {
	SDL_SetAlpha(wc->background, 0, alpha);
    }


    return retval;
}



/*
 * A "normal" system uses "main.c" for the "main()" function, and
 * simply adds a call to "init_xxx()" to that function, conditional
 * on some form of "USE_XXX" define.
 */

/*
 * XXX XXX XXX Initialization function
 */
errr init_sdl(int oargc, char **oargv) {

//    int argc = oargc;
//    char **argv = oargv;

    Uint32 initflags = SDL_INIT_VIDEO; /* What's the point, if not video? */
    //int fullscreen = 0;

    int i = 0;
    int sdl_window_flags = 0;
    //Uint8 bpp = 0;

    TileInformation *ti = NULL; // maybe update this later?
    
    // to avoid dumb warnings
    if (oargc || oargv) oargc = 0;
    
    ANGBAND_GRAF = "new"; /* not necessarily right.. set again below. XXX */

    use_graphics = TRUE; // fix?

    initflags |= SDL_INIT_AUDIO;
    /* TODO perhaps use SDL_InitSubSystem() instead. */

    /* I don't think you'd want the following (except for core dump): */
//#ifdef SDL_NOPARACHUTE
    initflags |= SDL_INIT_NOPARACHUTE;
//#endif

    /* This isn't supposed to do anything on Windows but it may break things!
     * XXX XXX XXX */
    /*initflags |= SDL_INIT_EVENTTHREAD;*/


    if (SDL_Init(initflags) != 0) {
	return -1;
    }

    // postponed for WIN32 until later

    if (use_sound) {
	
	int audio_rate = 22050;
	Uint16 audio_format = AUDIO_S16; 
	int audio_channels = 2;
	int audio_buffers = 512;
	
	
	char buffer[1024];
	char *which = SDL_AudioDriverName(buffer, 1000);
	
	if (which) {
	    INFOMSG("LAngband: Audio-driver used: %s\n", which);
	}
	else {
	    INFOMSG("No Audio-driver.\n");
	}
	
	
	INFOMSG("If possible, try to use SDL compiled for OSS, and kill esd before starting.\n");
	INFOMSG("If you have esd running your computer may lock up or sound may get a long delay.\n");

	if (Mix_OpenAudio(audio_rate, audio_format, audio_channels, audio_buffers)) {
	    ERRORMSG("Langband is unable to open audio!\n");
	    if (which) {
		INFOMSG("Langband/SDL tried to open sound-system of type '%s'\n", which);
	    }
	    if (!which || !strcmp(which, "dsp")) {
		ERRORMSG("Possible reason can be that /dev/dsp is being blocked by another program.\n"\
			 "Please see if you have any sound-daemons running (esd, arts, ..) that can be shut down.\n");
	    }

	    // the game can go on, but no sound!
	    use_sound = FALSE;
	    
	    //exit(1);
	}
	else {
	    DBGPUT("We managed to init the sound-system, good!\n");
	}
	
    }
    
    DBGPUT("opened audio.\n");
    
    init_color_data_sdl();

    // problems on Win
    //	if (fullscreen) {
    // sdl_window_flags |= SDL_FULLSCREEN | SDL_HWSURFACE | SDL_DOUBLEBUF;
    //	}

    // DBGPUT("going");
    // Let us create the base window first of all!

    theWindow = SDL_SetVideoMode(800, 600, 0, sdl_window_flags);
    if (theWindow == NULL) {
	ERRORMSG("SDL could not initialize video mode.");
	return -1;
    }

	
    SDL_WM_SetCaption("Langband", "Langband Main Screen");
    SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
//	SDL_ShowCursor(SDL_DISABLE);
    SDL_EnableUNICODE((1 == 1));

    // DBGPUT("vinduer");
    // mainwindow->t.higher_pict = TRUE;
    ANGBAND_SYS = "sdl";

    // init subwindows
//    sdl_subwindows = malloc(sizeof(term_data *) * 10);

    for (i = 0; i < num_predefinedFrames; i++) {
	LangbandFrame *lf = get_frame(i, PREDEFINED);
	const char *frameName = NULL;
	//DBGPUT("Checking sub %d\n", i);
	if (!lf) {
	    DBGPUT("Did not find frame %d.\n", i);
	    continue;
	}
	frameName = lf->name;
	lf = sdlify_frame(lf);
	
	//DBGPUT("did sdlify %p\n", lf);
	
	if (!lf) {
	    ERRORMSG("Problems creating frame '%s'\n", frameName);
	    return -1;
	}
	if (lf->backgroundfile) {
	    sdl_textureBackground(i, lf->backgroundfile, -1);
	}
	//DBGPUT("end-loop\n");
    }

    DBGPUT("tile init\n");

    // Initing tiles
    ti = malloc(sizeof(TileInformation));
	
    screen_tiles = malloc(sizeof(graf_tiles));
	
    screen_tiles->w = GFXTILE_WIDTH;
    screen_tiles->h = GFXTILE_HEIGHT;
	
    // FIX this to something else later!
    //   screen_tiles->dw = map->font_data->width;
    //screen_tiles->dh = map->font_data->height;
    screen_tiles->dw = GFXTILE_WIDTH; //map->font_data->width;
    screen_tiles->dh = GFXTILE_HEIGHT; //map->font_data->height;
    screen_tiles->face = NULL;

    DBGPUT("read tile\n");
    init_tile_information(ti);
    read_tiles(ti, david_gervais_tile_files);
    tileInfo = ti;

    DBGPUT("assign\n");

    // There is a chance something is done really wrong here!
    for (i = 0; i < max_predefinedFrames; i++) {
	LangbandFrame *lf = predefinedFrames[i];
	//DBGPUT("doing subwin %d %p\n", i, ted);
	if (lf) {
	    sdl_winconnection *wc = (sdl_winconnection*)lf->ui_connection;
	    wc->gt = screen_tiles; // improve later
	    lf->visible = FALSE;
	    //angband_term[i] = t;
	}
	else {
	    //angband_term[i] = NULL;
	}
	//DBGPUT("done subwin %d %p %p\n", i, ted, angband_term[i]);
    }

    
    DBGPUT("make other windows\n");
#if 0
    {
	tile_map = sdl_subwindows[MAP_TERM_IDX];
	ascii_map = make_term_window(MAP_TERM_IDX, "Map ASCII",
				     tile_map->xoffset, tile_map->yoffset,
				     tile_map->term_width, tile_map->term_height,
				     "vga8x16.hex", -1, -1);
	//ascii_map->shown = FALSE;
	ascii_map->gt = NULL;
	//angband_term[MAP_TERM_IDX] = &(ascii_map->t);
	//sdl_subwindows[MAP_TERM_IDX] = ascii_map;
    }
#endif    
    
    DBGPUT("after assign and Term is %p\n", Term);
    // big-term starts out as shown!

    activate_frame(FULL_TERM_IDX);
    Term = activeFrames[FULL_TERM_IDX]->azt;

    //SDL_init_screen_cursor(mainwindow->tile_width, mainwindow->tile_height);

    DBGPUT("return to sender with term being %p, and first active frame is %p\n", Term, activeFrames[0]);
    
    return 0;
}

errr
cleanup_SDL(void) {

    DBGPUT("closing\n");
    if (theWindow) {
	SDL_FreeSurface(theWindow);
	theWindow = NULL;
    }
    if (tileInfo) {
	free(tileInfo); // may need to do subpointers too. 
	tileInfo = NULL;
    }

    if (screen_tiles) {
	free(screen_tiles);
	screen_tiles = NULL;
    }
    if (use_sound) {
	Mix_CloseAudio();
    }
    SDL_Quit();
    DBGPUT("closed\n");
    
    return 0;
}


#endif /* USE_SDL */
