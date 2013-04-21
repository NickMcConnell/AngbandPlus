#ifndef LB_WINDOWS_H
#define LB_WINDOWS_H

/*
 * DESC: lbwindows.h - the windowing system for langband on the C-side
 * Copyright (c) 2000-2002 - Stig Erik Sandø

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "langband.h"

// authority on these is constants.lisp and this list
// should match the lisp-one

#define FULL_FRAME_IDX     0
#define MESSAGE_FRAME_IDX  1
#define CHARINFO_FRAME_IDX 2
#define MISC_FRAME_IDX     3
#define GFXMAP_FRAME_IDX   4
#define ASCIIMAP_FRAME_IDX 5
#define INV_FRAME_IDX      6
#define DIALOGUE_FRAME_IDX 7

// deprecated
#define FULL_TERM_IDX     0
#define GFXMAP_TERM_IDX   4
#define ASCIIMAP_TERM_IDX 5


typedef enum { ACTIVE = 0, PREDEFINED = 1} FrameType;

//typedef term angband_zterm;


struct LangbandFrame {

//    angband_zterm *azt;
        
    char *name; // name of frame
    int key; // the key (for othe rlookups) for the frame
    
    int xoffset; // x-offset compared to the main window
    int yoffset; // y-offset compared to the main window
    int columns; // how many columns in the frame
    int rows;    // how many rows in the frame
    int tile_width; // what is the width of an individual tile
    int tile_height; // what is the height of an individual tile
    int frame_width; // what is the actual width of the frame (columns*tile_width)
    int frame_height; // what is the actual height of the frame (rows*tile_height)
    int allowed_width; // what is the maximum width the frame can occupy
    int allowed_height; // what is the maximum height the frame can occupy;
    int bpp; // where applicable, the depth of the window colours

    int flags; // any flags;

    int visible;
    int should_be_hidden;
    int use_gfx_tiles;

    int ui_type; // an integer specifying what kind of UI this frame will work with
    void *ui_connection; // pointer to a struct with information for an ui

    char *fontname; /* maybe only used by some UIs */
    //char *backgroundfile;

    int background;


};

typedef struct LangbandFrame LangbandFrame;

extern int num_predefinedFrames;
extern int max_predefinedFrames;
extern LangbandFrame **predefinedFrames;
extern int num_activeFrames;
extern int max_activeFrames;
extern LangbandFrame **activeFrames;

INTERFACE int init_frame_system(int active_size, int predefined_size);
INTERFACE int legal_frame_key_p(int key, FrameType ft);
LangbandFrame *get_frame(int key, FrameType ft);
INTERFACE int has_frame(int key, FrameType ft);


INTERFACE int add_frame(int key, const char *name);
INTERFACE int add_frame_coords(int key, int x, int y, int w, int h);
INTERFACE int add_frame_tileinfo(int key, int tw, int th, const char *font);
INTERFACE int add_frame_gfxinfo(int key, int use_tiles);
INTERFACE int add_frame_bg(int key, int img_idx);
INTERFACE int activate_frame(int key);
INTERFACE int deactivate_frame(int key);
INTERFACE int clean_frame(int key);
INTERFACE int wipe_frame(int key);


INTERFACE int get_frame_columns(int key, FrameType ft);
INTERFACE int get_frame_rows(int key, FrameType ft);
INTERFACE int get_frame_tile_width(int key, FrameType ft);
INTERFACE int get_frame_tile_height(int key, FrameType ft);
INTERFACE int get_frame_gfx_tiles(int key, FrameType ft);


#endif /* lb_windows_h */
