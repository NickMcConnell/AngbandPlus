/* File: interface.c */

/*
 * Interface and common code used by more than one port, or of use to more
 * than one.  Includes window-handling, interactive control, font, sound,
 * music, and config file code.
 *
 * Copyright (c) 2007 Leon Marrick
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"
#include "intrface.h"



/*
 * This file is the back-end for a cross-platform graphical display engine
 * built on top of the Term package (z-term.c) and based originally on a
 * heavily revised version of the Angband Windows port.  The core objective is
 * to serve up a game that looks good out of the box - worthy of comparison
 * with AngbandTK - on multiple OSs.  Forcing the user to type in environment
 * variables, write scripts, fiddle with config files, or tweak half a dozen
 * settings before the game stops playing dumb terminal is out.
 *
 * This code remains, however, a work in progress.
 *
 * The bad:
 * - No unified way to specify single-user or multi-user behavior.  At present,
 *   each port does things its own way:  main-win is set up for single user,
 *   main-sdl for multi user.  Ideally the installer of the game (assuming he
 *   has necessary permissions) should be able to choose between the ease of
 *   use and ready access of the first and the security and extensibility of
 *   the second.
 * - There should be a clear dividing line between module code (what's here)
 *   and port-specific code (what's in the main-xxx files).  At present, the
 *   distinction is rather messy, which means that this code is not as clean
 *   or easy to use as Ben Harrison's Term package (the gold standard as far
 *   as I'm concerned).
 * - The overlapping window code is fairly messy, is too difficult to use
 *   correctly, and allows too much screen flicker.  It will have to be
 *   rewritten ... once we figure out how.  :-/
 * - There is no way to fine-tune window position within the game.  This is the
 *   one important feature of the Windows port that has gone missing.  Although
 *   I'm perfectly satisfied with static term windows and much prefer a simple
 *   choice of "more subwindows" or "more map", not everyone will agree with
 *   me.  If you don't, I refer the viewport-adjustment code in the Tome SDL
 *   port to your attention.
 *
 * The missing:
 * - This is not a true graphical user interface.  For example, when the game
 *   displays one of its multifarious lists (such as inventory), there is no
 *   easy way for it to request from the port a suitable pop-up window or
 *   surface, and no easy way for the port to report back such things as "which
 *   item did the user click on?".  Both UnAngband and FAAngband have done
 *   solid work on the second problem.
 */


/*
 * HOOKS
 *
 * There are an excessive number of these.  XXX XXX XXX
 */
void (*window_change_font_hook)(int idx, int cursor_y) = NULL;
errr (*change_font_hook)(window_type *win_ptr, char *path) = NULL;
void (*remove_font_hook)(window_type *win_ptr) = NULL;
bool (*init_graphics_hook)(bool verify_map) = NULL;
void (*term_data_link_hook)(window_type *win_ptr) = NULL;



/*
 * GLOBAL VARIABLES  (see intrface.h)
 */

bool paletted;

cptr AppName = VERSION_NAME;

char WorkingFolder[1024];

cptr ANGBAND_DIR_XTRA_FONT;
cptr ANGBAND_DIR_XTRA_GRAF;
cptr ANGBAND_DIR_XTRA_SOUND;
cptr ANGBAND_DIR_XTRA_MUSIC;

int screen_w;
int screen_h;

int app_left;
int app_top;
int app_wid;
int app_hgt;

int client_w;
int client_h;

int cur_display_mode;
int arg_display_mode;

term term_data[TERM_MAX];

#ifdef USE_SOUND
cptr sound_file[MSG_MAX][SAMPLE_MAX];
cptr music_file[MUSIC_MAX][SAMPLE_MAX];

bool sounds_ready = FALSE;
bool music_ready = FALSE;

/* Time (in seconds) that the current music started */
u32b cur_music_start_time = 0L;
int cur_music_theme = -1;
int cur_song_length = 120;

#endif /* USE_SOUND */

int media_quality_level;

#ifdef SUPPORT_GAMMA
int gamma_correction;
#endif /* SUPPORT_GAMMA */

window_type window_settings[TERM_MAX + 1];




/*
 * Standard window settings (before any preferences are read).
 *
 * For each default resolution, we list all the window layouts available.
 * Care should be taken that they actually show the required number of grids
 * (watch out for internal and system-defined borders).
 *
 * Notes:
 * - For each resolution, at least the "INTERFACE_SCREEN_EMPHA_MAP" display mode
 * must be defined.
 *
 * - For each resolution and display mode, at least two windows must be defined:
 * the main term screen (must display at least 80x24) and the tall display
 * screen (must display at least 80x46).
 *
 * - Window size and positioning refer to the area WITHIN the application
 * window borders.  In windowed mode, the system will add a title bar on the
 * top, resizing borders on the other three sides, and may also require that
 * the total application window size (including these borders) not be larger
 * than the screen.  Also in windowed mode, we should allocate one more pixel
 * as an inner edge to stop the text running into the outside border.  On
 * MS Windows, that's 36 pixels lost vertically and 10 horizontally.
 *
 * - The map window's size and position depends entirely on that of the main
 * game window, not on values here or in the preferences.  If the map window is
 * not defined, the game will not use a special map display.  If it is, then
 * size and position is auto-calculated and the font can (if not specified)
 * be shared with the main window.
 *
 * - Sub-windows TERM_SUBWINDOW can be smaller than 80x24; 75x24 or even less
 * will work for most displays (the character screen being an exception), and
 * 79x24 will work for all.  Heights of as little as 5 rows are perfectly
 * acceptable for displays like previous messages or nearest monsters/objects.
 */
window_type window_defaults[NUM_WINDOW_DEFAULTS] =
{
	/* Map window placeholder -- this window is auto-calculated */
	{
		0, 0, 0, TERM_MAP, 0, 0,
		8,     0, 0, 0, 0,     0, 0, 0, 0,
		NULL, NULL, NULL,    0, 0, 0, 0,   1, 0
	},  /* Font will be that of the main window */


	/* 640x480 resolution (should always work on any legal monitor) */
	{
		640, 480, INTERFACE_SCREEN_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 640, 480,     0, 0, 0, 0,
		"8x12x.fon", NULL, NULL,    0, 0, 0, 0,   1, 0
	},
	{
		640, 480, INTERFACE_SCREEN_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 640, 480,     0, 0, 0, 0,
		"8x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		/* In windowed mode we have 632x446 client space available */
		640, 480, INTERFACE_WINDOW_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 632, 446,     1, 1, 1, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		640, 480, INTERFACE_WINDOW_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 632, 446,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},


	/* 800x600 resolution:  2 sub-windows */
	{
		800, 600, INTERFACE_SCREEN_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 800, 407,     0, 0, 0, 1,
		"10x14x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		800, 600, INTERFACE_SCREEN_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 800, 600,     0, 0, 0, 0,
		"8x12x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		800, 600, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW, 0, 0,
		8,     0, 407, 401, 192,     0, 1, 1, 0,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{ /* We have to have inner borders, so squeeze me to 79 columns */
		800, 600, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW + 1, 0, 0,
		8,     401, 407, 399, 192,     1, 1, 0, 0,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 1,   1, 0
	},
/* mode */
	{
		800, 600, INTERFACE_WINDOW_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 792, 372,     1, 1, 1, 1,
		"8x12x.fon", NULL, NULL,     0, 0, 0, 1,   1, 0
	},
	{
		800, 600, INTERFACE_WINDOW_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 792, 372,     1, 1, 1, 1,
		"8x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{ /* We have to squeeze both sub-windows to 79 columns */
		800, 600, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW, 0, 0,
		8,     0, 372, 396, 194,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		800, 600, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW + 1, 0, 0,
		8,     396, 372, 396, 194,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},


	/* 1024x768 resolution:  2 or 5 sub-windows */
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 1024, 528,     0, 0, 0, 1,
		"12x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1024, 768,     0, 0, 0, 0,
		"12x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW, 0, 0,
		8,     0, 528, 512, 240,     0, 0, 1, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW + 1, 0, 0,
		8,     512, 528, 512, 240,     1, 0, 0, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_SUB, TERM_MAIN, 0, 0,
		1024,     0, 0, 1024, 439,     0, 0, 0, 1,
		"12x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_SUB, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1024, 768,     0, 0, 0, 0,
		"12x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{  /* A trio of stubby sub-windows, for messages or nearest monster/objects */
		1024, 768, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW, 0, 0,
		8,     0, 439, 391, 89,     0, 0, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 1, 0, 0,
		8,     391, 439, 242, 89,     0, 0, 0, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 2, 0, 0,
		8,     633, 439, 391, 89,     1, 0, 0, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{   /* Two larger, general-purpose sub-windows */
		1024, 768, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 3, 0, 0,
		8,     0, 528, 512, 240,     0, 0, 1, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 4, 0, 0,
		8,     512, 528, 512, 240,     1, 0, 0, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 1016, 492,     1, 1, 1, 1,
		"12x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1016, 734,        1, 1, 1, 1,
		"10x14x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW, 0, 0,
		8,     0, 492, 508, 242,     1, 1, 1, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW + 1, 0, 0,
		8,     508, 492, 508, 242,     1, 1, 1, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_SUB, TERM_MAIN, 0, 0,
		1024,     0, 0, 1016, 406,     1, 1, 1, 1,
		"12x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_SUB, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1016, 734,     1, 1, 1, 1,
		"10x14x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW, 0, 0,
		8,     0, 406, 392, 89,     1, 0, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 1, 0, 0,
		8,     392, 406, 232, 89,     1, 0, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 2, 0, 0,
		8,     624, 406, 392, 89,     1, 0, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 3, 0, 0,
		8,     0, 495, 508, 241,     1, 0, 1, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1024, 768, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 4, 0, 0,
		8,     508, 495, 512, 241,     1, 0, 1, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},


	/* 1280x1024 resolution:  3 or 7 sub-windows */
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 1280, 831,     0, 0, 0, 1,  /* 1280, 831 */
		"12x18xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1280, 831,     0, 0, 0, 1,   /* We get 46 rows in either case */
		"12x18xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW, 0, 0,
		8,     0, 831, 401, 193,     0, 1, 1, 0,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW + 1, 0, 0,
		8,     401, 831, 402, 193,     1, 1, 1, 0,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW + 2, 0, 0,
		8,     803, 831, 477, 193,     1, 1, 0, 0,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, TERM_MAIN, 0, 0,
		1024,     0, 0, 881, 831,     0, 0, 1, 1,
		"11x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 881, 831,     0, 0, 1, 0,
		"11x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW, 0, 0,
		8,     881, 0, 399, 207,     1, 0, 0, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 1, 0, 0,
		8,     881, 207, 399, 208,     1, 1, 0, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 2, 0, 0,
		8,     881, 415, 399, 208,     1, 1, 0, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 3, 0, 0,
		8,     881, 623, 399, 208,     1, 1, 0, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 4, 0, 0,
		8,     0, 831, 401, 193,     0, 1, 1, 0,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 5, 0, 0,
		8,     401, 831, 478, 193,     1, 1, 1, 0,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 6, 0, 0,
		8,     879, 831, 401, 193,     1, 1, 0, 0,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 1272, 796,     1, 1, 1, 1,
		"12x18xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1272, 796,     1, 1, 1, 1,
		"12x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW, 0, 0,
		8,     0, 796, 402, 194,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW + 1, 0, 0,
		8,     402, 796, 468, 194,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW + 2, 0, 0,
		8,     870, 796, 402, 194,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, TERM_MAIN, 0, 0,
		1024,     0, 0, 882, 796,     1, 1, 1, 1,
		"11x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 882, 796,     1, 1, 1, 1,
		"11x16xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW, 0, 0,
		8,     882, 0, 390, 199,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 1, 0, 0,
		8,     882, 199, 390, 199,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 2, 0, 0,
		8,     882, 398, 390, 199,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 3, 0, 0,
		8,     882, 597, 390, 199,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 4, 0, 0,
		8,     0, 796, 402, 194,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 5, 0, 0,
		8,     402, 796, 468, 194,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1280, 1024, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 6, 0, 0,
		8,     870, 796, 402, 194,     1, 1, 1, 1,
		"5x8x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},


	/* 1600x1200 resolution:  3 or 7 sub-windows */
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 1600, 958,     0, 0, 0, 1,
		"16x24x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1600, 1200,     0, 0, 0, 0,
		"16x24x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW, 0, 0,
		8,     0, 958, 533, 242,     2, 2, 2, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW + 1, 0, 0,
		8,     533, 958, 534, 242,     2, 2, 2, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_MAP, TERM_SUBWINDOW + 2, 0, 0,
		8,     1067, 958, 533, 242,     2, 2, 2, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, TERM_MAIN, 0, 0,
		1024,     0, 0, 1121, 959,     0, 0, 1, 1,
		"14x20xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1121, 959,     0, 0, 1, 1,  /* 46+ rows in either case */
		"14x20xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW, 0, 0,
		8,     1121, 0, 479, 241,     1, 0, 0, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 1, 0, 0,
		8,     1121, 241, 479, 242,     1, 1, 0, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 2, 0, 0,
		8,     1121, 483, 479, 242,     1, 1, 0, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 3, 0, 0,
		8,     1121, 725, 479, 234,     1, 1, 0, 1,  /* 23 rows */
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 4, 0, 0,
		8,     0, 959, 481, 241,     0, 1, 1, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 5, 0, 0,
		8,     481, 959, 638, 241,     1, 1, 1, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_SCREEN_EMPHA_SUB, TERM_SUBWINDOW + 6, 0, 0,
		8,     1121, 959, 479, 241,     1, 1, 0, 0,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_MAP, TERM_MAIN, 0, 0,
		1024,     0, 0, 1592, 924,     1, 1, 1, 1,
		"16x24x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_MAP, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1592, 924,     1, 1, 1, 1,
		"14x20xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW, 0, 0,
		8,     0, 924, 531, 242,     2, 1, 2, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW + 1, 0, 0,
		8,     531, 924, 530, 242,     2, 1, 2, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_MAP, TERM_SUBWINDOW + 2, 0, 0,
		8,     1061, 924, 531, 242,     2, 1, 2, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
/* mode */
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, TERM_MAIN, 0, 0,
		1024,     0, 0, 1122, 924,     1, 1, 1, 1,
		"14x20xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, WINDOW_DISPLAY, 0, 0,
		8,     0, 0, 1122, 924,     1, 1, 1, 1,  /* 46+ rows in either case */
		"14x20xrb.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW, 0, 0,
		8,     1122, 0, 470, 242,     1, 1, 1, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 1, 0, 0,
		8,     1122, 242, 470, 242,     1, 1, 1, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 2, 0, 0,
		8,     1122, 484, 470, 242,     1, 1, 1, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 3, 0, 0,
		8,     1122, 726, 470, 198,     1, 1, 1, 1,  /* 19 rows */
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 4, 0, 0,
		8,     0, 924, 531, 242,     2, 1, 2, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 5, 0, 0,
		8,     531, 924, 530, 242,     2, 1, 2, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	},
	{
		1600, 1200, INTERFACE_WINDOW_EMPHA_SUB, TERM_SUBWINDOW + 6, 0, 0,
		8,     1061, 924, 531, 242,     2, 1, 2, 1,
		"6x10x.fon", NULL, NULL,     0, 0, 0, 0,   1, 0
	}
};





/************************************************************************
 *                                                                      *
 *                                Windows                               *
 *                                                                      *
 ************************************************************************/



/*
 * Determine if the active Term is overlapped.  If it is, remove the overlapped
 * area from the region scheduled to be refreshed.
 */
errr Term_xtra_gui_overlap(void)
{
	srect rc;
	int i, j;
	int y, x, y1, y2, x1, x2, y1a, y2a, x1a, x2a;

	byte *scr_aa, *old_aa;
	char *scr_cc, *old_cc;
	byte *scr_taa, *old_taa;
	char *scr_tcc, *old_tcc;


	/* Get this term's display window */
	window_type *win_ptr = (window_type*)(Term->data);

	/* Get the index of the active Term */
	int term_idx = win_ptr->Term_idx;


	/* Paranoia - ignore non-visible windows */
	if (!win_ptr->visible) return (0);

	/* Ignore non-mapped Terms (they should never be calling this function) */
	if (!Term->mapped_flag) return (-1);


	/* Get the outer corners of the area(s) to be refreshed */
	y1 = Term->y1;
	y2 = Term->y2;

	for (x1 = 999, x2 = 0, i = y1; i <= y2; i++)
	{
		if (x1 > Term->x1[i]) x1 = Term->x1[i];
		if (x2 < Term->x2[i]) x2 = Term->x2[i];
	}

	/* Calculate the area this term is redrawing in pixel coordinates */
	rc.left   = win_ptr->window_left + win_ptr->border_left + (x1 * win_ptr->tile_wid);
	rc.top    = win_ptr->window_top  + win_ptr->border_top  + (y1 * win_ptr->tile_hgt);
	rc.right  = rc.left + ((x2 - x1 + 1) * win_ptr->tile_wid) - 1;
	rc.bottom = rc.top  + ((y2 - y1 + 1) * win_ptr->tile_hgt) - 1;


	/* Otherwise, look at all Terms with a higher z-priority, in order */
	for (i = TERM_MAX - 1; i > term_z_order[term_idx]; i--)
	{
		window_type *win2_ptr;

		/* Look for the Term with this z-order */
		for (j = 0; j < TERM_MAX; j++)
		{
			if (term_z_order[j] == i) break;
		}

		/* Special case -- use the tall display if active */
		if ((j == TERM_MAIN) && (use_tall_display)) j = WINDOW_DISPLAY;

		/* Access the chosen Term window (not the term - it may not exist) */
		win2_ptr = &window_settings[j];

		/* Ignore non-visible windows */
		if (!win2_ptr->visible) continue;

		/* Efficiency -- if this isn't the main term, we can ignore the map */
		if ((Term != angband_term[TERM_MAIN]) && (win2_ptr->Term_idx == TERM_MAP))
			continue;

		/* Ignore if it shares no space with the redraw rectangle */
		if ((long)(win2_ptr->window_left) > rc.right) continue;
		if ((long)(win2_ptr->window_top)  > rc.bottom) continue;
		if ((long)(win2_ptr->window_left + win2_ptr->window_wid) < rc.left) continue;
		if ((long)(win2_ptr->window_top  + win2_ptr->window_hgt) < rc.top) continue;


		/* Calculate the leftmost overlapped grid */
		x1a = ((win2_ptr->window_left) - (win_ptr->window_left +
			win_ptr->border_left)) / win_ptr->tile_wid;

		/* Calculate the rightmost overlapped grid */
		x2a = ((win2_ptr->window_left + win2_ptr->window_wid - 1) -
			(win_ptr->window_left + win_ptr->border_left)) / win_ptr->tile_wid;

		/* Calculate the topmost overlapped grid */
		y1a = ((win2_ptr->window_top) - (win_ptr->window_top +
			win_ptr->border_top)) / win_ptr->tile_hgt;

		/* Calculate the bottommost overlapped grid */
		y2a = ((win2_ptr->window_top + win2_ptr->window_hgt - 1) -
			(win_ptr->window_top + win_ptr->border_top)) / win_ptr->tile_hgt;

		/*
		 * There should always be some overlap now.
		 *
		 * Do not adjust these numbers for legality, or refreshes for a screen
		 * of 256 width or 256 height will not be suppressed properly.
		 */


		/*
		 * Efficiency -- handle overlaps that cover an entire side by adjusting
		 * the rows and columns to update.
		 */

		/* The full width of the current Term is overlapped */
		if ((x1a <= x1) && (x2a >= x2))
		{
			/* Completely covered; cancel the refresh */
			if ((y1a <= y1) && (y2a >= y2))
			{
				Term->y1 = 255;
				Term->y2 = 0;
				return (0);
			}

			/* Covered only on the top */
			else if (y1a <= y1)
			{
				/* Adjust for legality AFTER all tests */
				if (y2a > 254) y2a = 254;
				if (y2a <  -1) y2a =  -1;

				/* Refresh starting just below the last covered row */
				Term->y1 = y2a + 1;
			}

			/* Covered only on the bottom */
			else if (y2a >= y2)
			{
				if (y1a > 256) y1a = 256;
				if (y1a <   1) y1a =   1;

				Term->y2 = y1a - 1;
			}
		}

		/* The full height of the current Term is overlapped */
		else if ((y1a <= y1) && (y2a >= y2))
		{
			/* Total cancel already handled */

			/* Covered only on the left */
			if (x1a <= x1)
			{
				if (x2a > 254) x2a = 254;
				if (x2a <  -1) x2a =  -1;

				/* Shorten all rows */
				for (y = y1; y <= y2; y++)
					if (Term->x1[y] < x2a) Term->x1[y] = x2a + 1;
			}

			/* Covered only on the right */
			else if (x2a >= x2)
			{
				if (x1a > 256) x1a = 256;
				if (x1a <   1) x1a =   1;

				for (y = y1; y <= y2; y++)
					if (Term->x2[y] < x1a) Term->x2[y] = x1a - 1;
			}
		}

		/*
		 * If the overlapped region does not cover a full side (ex. the map
		 * window over the main window), then we must explicitly cancel each
		 * covered grid.  This is inefficient.
		 */
		else
		{
			for (y = y1; y <= y2; y++)
			{
				/* Row is above or below the overlapped area */
				if ((y < y1a) || (y > y2a)) continue;

				/* Point to what we currently have stored for this row */
				scr_aa = Term->scr->a[y];
				scr_cc = Term->scr->c[y];
				old_aa = Term->old->a[y];
				old_cc = Term->old->c[y];

				scr_taa = Term->scr->ta[y];
				scr_tcc = Term->scr->tc[y];
				old_taa = Term->old->ta[y];
				old_tcc = Term->old->tc[y];

				/* Scan the row */
				for (x = x1; x <= x2; x++)
				{
					/* Grid is left or right of the overlapped area */
					if ((x < x1a) || (x > x2a)) continue;

					/* Pretend that what we have queued is already on the screen */
					old_aa[x] = scr_aa[x];
					old_cc[x] = scr_cc[x];
					old_taa[x] = scr_taa[x];
					old_tcc[x] = scr_tcc[x];
				}
			}
		}
	}

	/* Success */
	return (0);
}




/*
 * Scan for application sizes with defined window layouts.
 */
void seek_app_size_defaults(int *wid_tmp, int *hgt_tmp,
	int app_wid, int app_hgt)
{
	int i;

	int largest_legal_wid = 0;
	int largest_legal_hgt = 0;


	/* Search all the windows defaults */
	for (i = 0; i < NUM_WINDOW_DEFAULTS; i++)
	{
		/* Point to this window */
		const window_type *win_ptr = &window_defaults[i];

		/* This size is not larger than our application */
		if ((app_wid >= win_ptr->resolution_x) &&
			(app_hgt >= win_ptr->resolution_y))
		{
			/* And it is larger than the previous best resolution */
			if ((largest_legal_wid < win_ptr->resolution_x) ||
				(largest_legal_hgt < win_ptr->resolution_y))
			{
				/* Save this value */
				largest_legal_wid = win_ptr->resolution_x;
				largest_legal_hgt = win_ptr->resolution_y;
			}
		}
	}

	/* Set game (but not screen) resolution to this value */
	*wid_tmp = largest_legal_wid;
	*hgt_tmp = largest_legal_hgt;
}


/*
 * Calculate various things for the map display.
 *
 * This function must work correctly whether the map display term is present
 * or not, because we can't initialize a term until we know how big it needs
 * to be.
 */
void calc_map_display(void)
{
	int bottom, cols, rows;

	term *old = Term;

	/* Get the map window */
	window_type *map_win_ptr = &window_settings[TERM_MAP];

	/* Get the main window */
	window_type *win_ptr = &window_settings[use_tall_display ? WINDOW_DISPLAY : TERM_MAIN];

	/* Require that basic stuff be initialized */
	if (!Term) return;

	/* If not using a special map, just calculate the map size and leave */
	if (!use_special_map)
	{
		/* The map is displayed in the main term */
		calc_map_size(win_ptr->cols - COL_MAP, win_ptr->rows - ROW_MAP - 1);
		return;
	}


	/* Calculate left and top edges */
	map_win_ptr->window_left = win_ptr->window_left + win_ptr->border_left;
	map_win_ptr->window_left += win_ptr->tile_wid * COL_MAP;

	map_win_ptr->window_top = win_ptr->window_top + win_ptr->border_top;
	map_win_ptr->window_top += win_ptr->tile_hgt * ROW_MAP;

	/* Calculate width (main window width, less borders on both sides) */
	map_win_ptr->window_wid = win_ptr->window_wid -
		(map_win_ptr->window_left - win_ptr->window_left) - win_ptr->border_right;

	/* Get lower edge of last available line of text in main window */
	bottom = win_ptr->window_top + win_ptr->border_top + win_ptr->rows * win_ptr->tile_hgt;

	/* Deduct for status bar (hard-coding XXX XXX) */
	bottom -= win_ptr->tile_hgt;

	/* Calculate height */
	map_win_ptr->window_hgt = bottom - map_win_ptr->window_top;

	/* Calculate map window rows and columns */
	cols = (map_win_ptr->window_wid - map_win_ptr->border_left -
		map_win_ptr->border_right) / map_win_ptr->tile_wid;
	rows = (map_win_ptr->window_hgt - map_win_ptr->border_top -
		map_win_ptr->border_bottom) / map_win_ptr->tile_hgt;

	/* Save rows and cols, ensuring that they are never greater than 255 */
	map_win_ptr->cols = MIN(cols, 255);
	map_win_ptr->rows = MIN(rows, 255);


	/* Hack -- update the size of the map display (which has special rules) */
	calc_map_size(map_win_ptr->cols, map_win_ptr->rows);


	/* If the map term exists, */
	if (term_map)
	{
		/* Activate the map term */
		(void)Term_activate(term_map);

		/* Resize the map term */
		(void)Term_resize(map_win_ptr->cols, map_win_ptr->rows);

		/* Restore previous term */
		(void)Term_activate(old);
	}
}



/*
 * Given a point in client coordinates, return the term and grid position
 * in which that point lies.
 */
void get_term_position(int *term, int *col, int *row, int x, int y, bool ignore_popups)
{
	int i;
	window_type *win_ptr;
	int term_idx;
	bool popup_hack_flag = FALSE;


	/* Hack -- Optionally, ignore pop-up windows that cover the map view  XXX */
	if ((ignore_popups) && (use_special_map) && (angband_term[TERM_MAIN]->popup_hack_flag))
	{
		swap_term_z_order(TERM_MAIN, TERM_MAP);
		popup_hack_flag = TRUE;
	}

	/* For each possible Term (inward towards the screen) */
	for (i = TERM_MAX - 1; i >= 0; i--)
	{
		/* Look for the Term with this z-order */
		for (term_idx = 0; term_idx < TERM_MAX; term_idx++)
		{
			if (term_z_order[term_idx] == i) break;
		}

		/* Special case -- use the tall display if active */
		if ((term_idx == TERM_MAIN) && (use_tall_display))
			term_idx = WINDOW_DISPLAY;


		/* Ignore non-initialized terms */
		if (!angband_term[term_idx]) continue;

		/* Get this term's display window */
		win_ptr = (window_type *)(angband_term[term_idx]->data);

		/* Ignore non-visible windows */
		if (!win_ptr->visible) continue;

		/* Ignore unless it contains this point */
		if (win_ptr->window_left > x) continue;
		if (win_ptr->window_top  > y) continue;
		if ((win_ptr->window_left + win_ptr->window_wid +
		     win_ptr->border_left + win_ptr->border_right) < x) continue;
		if ((win_ptr->window_top  + win_ptr->window_hgt +
		     win_ptr->border_left + win_ptr->border_right) < y) continue;

		/* Calculate grid position */
		*col = (x - win_ptr->window_left - win_ptr->border_left) / win_ptr->tile_wid;
		*row = (y - win_ptr->window_top  - win_ptr->border_top)  / win_ptr->tile_hgt;

		/* Remember term index */
		*term = term_idx;

		/* Found it */
		break;
	}

	/* Undo the ignore popup hack */
	if (popup_hack_flag)
	{
		swap_term_z_order(TERM_MAIN, TERM_MAP);
	}
}


/*
 * When a term window is drawn on, any term window overlapping it must be
 * redrawn afterwards.  The basic display engine was never designed for such a
 * case and so we must write our own code.  -LM-
 *
 * Since we don't want to redraw everything if we can easily avoid it, we save
 * redraw rectangles, translating the provided rows and columns into pixel
 * coordinates.
 *
 * At present, this code only handles the case where the main term is both
 * overlapped by the map term (as is the case if "screen_depth" is 0) and
 * drawing in its area.  A better solution would be to stop the main term
 * drawing over the map term in the first place, but I haven't figured out how
 * to do this cleanly (without hacking up the term code too badly) and
 * efficently.
 */
void overlap_notify(int x1, int y1, int x2, int y2)
{
	srect rc;
	int i;

	window_type *win_ptr;
	term *old = Term;

	int old_term_idx;


	/* Do nothing if term structures are not prepared yet */
	if (!Term) return;

	/* At present, only the main term is allowed to overlap anything */
	if (term_z_order[TERM_MAP] < term_z_order[TERM_MAIN])
	{
		if (Term != term_main) return;
	}
	else
	{
		return;
	}

	/* Get the window for the active term */
	win_ptr = (window_type*)(Term->data);

	/* Get its index */
	old_term_idx = win_ptr->Term_idx;


	/* Calculate the area this term is redrawing in pixel coordinates */
	rc.left   = win_ptr->window_left + win_ptr->border_left + (x1 * win_ptr->tile_wid);
	rc.top    = win_ptr->window_top  + win_ptr->border_top  + (y1 * win_ptr->tile_hgt);
	rc.right  = rc.left + ((x2 - x1 + 1) * win_ptr->tile_wid) - 1;
	rc.bottom = rc.top  + ((y2 - y1 + 1) * win_ptr->tile_hgt) - 1;


	/*  Find the topmost standard window that is overlain by this update */
	for (i = TERM_MAX - 1; i >= 0; i--)
	{
		/* Search in z-order, topmost first */
		int term_idx = term_z_order[i];

		/* Ignore the origin term */
		if (old_term_idx == term_idx) continue;

		/* Get this term's display window */
		win_ptr = &window_settings[term_idx];

		/* Ignore non-visible windows */
		if (!win_ptr->visible) continue;

		/* Ignore windows that will already be entirely erased */
		if (angband_term[term_idx]->total_erase) continue;

		/* Ignore if it shares no space with the redraw rectangle */
		if ((long)(win_ptr->window_left) > rc.right) continue;
		if ((long)(win_ptr->window_top)  > rc.bottom) continue;
		if ((long)(win_ptr->window_left + win_ptr->window_wid) < rc.left) continue;
		if ((long)(win_ptr->window_top  + win_ptr->window_hgt) < rc.top) continue;


		/* Calculate redraw area */
		x1 = (rc.left - win_ptr->window_left - win_ptr->border_left) / win_ptr->tile_wid;
		y1 = (rc.top - win_ptr->window_top - win_ptr->border_top)  / win_ptr->tile_hgt;

		x2 = ((rc.right - win_ptr->window_left - win_ptr->border_left) / win_ptr->tile_wid);
		y2 = ((rc.bottom - win_ptr->window_top - win_ptr->border_top)  / win_ptr->tile_hgt);


		/* Activate this term */
		(void)Term_activate(angband_term[term_idx]);

		/* Redraw the screen display here, refresh later */
		(void)Term_redraw_section_nofresh(x1, y1, x2, y2);
	}

	/* Restore previous term */
	(void)Term_activate(old);
}


/*
 * Handle special redraws which may become necessary in certain situations.  -LM-
 *
 * In our case, we want to redraw windows that have been temporarily overlapped.
 */
errr Term_fresh_gui(bool total_erase)
{
	errr error = 0;
	term *old = Term;

	/* Hack -- no need for this unless we're using a special map display */
	if (!use_special_map) return (0);

	/* Hack! -- Do not do anything if the map is not in use */
	if (!character_dungeon || !character_generated) return (0);


	/* If we have just refreshed the main term, also refresh the map if it overlaps */
	if ((Term == term_main) && (term_z_order[TERM_MAP] > term_z_order[TERM_MAIN]))
	{
		/* Activate the map term */
		(void)Term_activate(term_map);

		/* If the main term was totally erased, do the same to the map display */
		if (total_erase) Term->total_erase = TRUE;

		/* Refresh the map term, report any errors */
		error = Term_fresh();
	}

	/* If the map was refreshed when the main term is on top, refresh the main term now */
	else if ((Term == term_map) && (term_z_order[TERM_MAP] < term_z_order[TERM_MAIN]))
	{
		/* Activate the main term */
		(void)Term_activate(term_main);

		/* If the map term was totally erased, do the same to the main display */
		if (total_erase) Term->total_erase = TRUE;

		/* Refresh the main term, report any errors */
		error = Term_fresh();
	}


	/* Restore previous term (if different) */
	(void)Term_activate(old);

	return (error);
}




/*
 * Start or stop using the special large display (46+ character rows).
 */
errr switch_display_gui(int display)
{
	window_type *old_win_ptr;
	window_type *win_ptr;

	int diff_left, diff_top, diff_right, diff_bottom;


	/* Get the previous window data */
	old_win_ptr = (window_type *)term_main->data;

	/* Activate the correct term window display */
	if (display)
	{
		/* Switch over to use of the special term display window */
		term_main->data = &window_settings[WINDOW_DISPLAY];
	}
	else
	{
		/* Return to using the usual main term window */
		term_main->data = &window_settings[TERM_MAIN];
	}

	/* Get the current window data */
	win_ptr = (window_type *)term_main->data;


	/* Determine if any screen space is covered or uncovered */
	diff_left = win_ptr->window_left - old_win_ptr->window_left;
	diff_top = win_ptr->window_top - old_win_ptr->window_top;
	diff_right = (win_ptr->window_left + win_ptr->window_wid) -
	             (old_win_ptr->window_left + old_win_ptr->window_wid);
	diff_bottom = (win_ptr->window_top + win_ptr->window_hgt) -
	              (old_win_ptr->window_top + old_win_ptr->window_hgt);

	/* If screen space was uncovered, */
	if ((diff_left > 0) || (diff_top > 0) || (diff_right < 0) || (diff_bottom < 0))
	{
		/*
		 * The correct response would be to request updates for only the affected
		 * sub-windows.  This would involve a lot of code, so we'll request a
		 * full set of window redraws.
		 */

		/* Refresh everything (later) */
		p_ptr->window = 0xFFFFFFFF;
	}

	/* React, note response */
	return (Term_xtra(TERM_XTRA_REACT, 0));
}



/*
 * A temporary string to hold a font name when a window_type is saved.
 */
static cptr temp_font_file = NULL;

/*
 * A temporary display window data structure.
 */
static window_type *window_temp = NULL;

static int temp_window_idx = -1;


/*
 * Allow special, game-configurable displays that - at least ideally - show the
 * requested cols and rows using as much of the screen as possible.  -LM-
 *
 * The calling code decides whether to use the standard or tall display.
 */
void special_view_gui(int cols, int rows, bool activate)
{
	term *old = Term;
	window_type *win_ptr;

	int wid, hgt, Wrows, Wcols;


	/* Activate the main term */
	(void)Term_activate(term_main);

	/* We are turning on the special display */
	if ((activate) && (temp_window_idx < 0))
	{
		int cell_wid, cell_hgt;

		/* Paranoia -- insist upon showing 1 row and 1 column */
		if (cols < 1) cols = 1;
		if (rows < 1) rows = 1;

		/* Get the index for either the standard or tall display window */
		temp_window_idx = use_tall_display ? WINDOW_DISPLAY : TERM_MAIN;

		/* Point to the correct display window */
		win_ptr = &window_settings[temp_window_idx];

		/* Make a temporary window structure */
		MAKE(window_temp, window_type);

		/* Save the font name (as otherwise it will be destroyed) */
		temp_font_file = string_make(win_ptr->font_file);

		/* Remove old font from the system, free system resources */
		remove_font_hook(win_ptr);

		/* Save the display window's existing data */
		COPY(window_temp, win_ptr, window_type);


		/* Expand display window to maximum size */
		win_ptr->window_wid = client_w;
		win_ptr->window_hgt = client_h;

		/* Move it to the left-top corner */
		win_ptr->window_left = 0;
		win_ptr->window_top  = 0;

		/* Set consistent margins */
		win_ptr->border_left = MIN(win_ptr->border_left, win_ptr->border_right);
		win_ptr->border_right = win_ptr->border_left;
		win_ptr->border_top = MIN(win_ptr->border_top, win_ptr->border_bottom);
		win_ptr->border_bottom = win_ptr->border_top;

		/* Determine useable space (preserve borders) */
		wid = win_ptr->window_wid - win_ptr->border_left - win_ptr->border_right;
		hgt = win_ptr->window_hgt - win_ptr->border_top  - win_ptr->border_bottom;

		/* Determine what font size will give us the requested rows and cols */
		cell_wid = wid / cols;
		cell_hgt = hgt / rows;

		/* Find and use the largest font that will fit */
		if (!use_largest_font(win_ptr, cell_wid, cell_hgt))
		{
			/* If we failed here, restore the old window completely */
			special_view_hook(0, 0, FALSE);

			/* And leave */
			return;
		}

		/* If we are using graphics ... */
		if (mode_is_graphical(use_graphics))
		{
			/* Maximize tile size */
			if (win_ptr->tile_wid < wid / cols) win_ptr->tile_wid = wid / cols;
			if (win_ptr->tile_hgt < hgt / rows) win_ptr->tile_hgt = hgt / rows;

			/* Always use "Term_pict" */
			Term->always_pict = TRUE;
			Term->higher_pict = FALSE;
		}

		/* Calculate window rows and columns */
		Wcols = wid / win_ptr->tile_wid;
		Wrows = hgt / win_ptr->tile_hgt;

		/* Save rows and cols, ensuring that they are never greater than 255 */
		win_ptr->cols = MIN(Wcols, 255);
		win_ptr->rows = MIN(Wrows, 255);

		/* Resize the Term */
		if (Term_resize(win_ptr->cols, win_ptr->rows))
		{
			/* If we failed here, restore the old window completely */
			special_view_hook(0, 0, FALSE);

			/* And leave */
			return;
		}

		/* We are now using the full-screen view */
		use_fullscreen_view = TRUE;
	}


	/* Restore the standard dimensions for this window */
	else if (temp_window_idx >= 0)
	{
		char path[1024];

		/* Do nothing if we don't have saved data to use */
		if (!window_temp) return;

		/* Point to the correct main term window */
		win_ptr = &window_settings[temp_window_idx];

		/* Restore previous settings */
		COPY(win_ptr, window_temp, window_type);

		/* Delete the temporary saved window settings */
		KILL(window_temp, window_type);

		/* Invalidate the display index */
		temp_window_idx = -1;

		/* Build a path to our saved font file */
		(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA_FONT, temp_font_file);

		/* Restore the old font (it had better be available...) */
		(void)change_font_hook(win_ptr, path);

		/* Free the temporary font name holder */
		(void)string_free(temp_font_file);

		/* Restore previous Term size */
		(void)Term_resize(win_ptr->cols, win_ptr->rows);

		/* We are no longer using the full-screen view */
		use_fullscreen_view = FALSE;
	}

	/* React to changes */
	(void)Term_xtra(TERM_XTRA_REACT, 0);


	/* Restore previous term */
	(void)Term_activate(old);
}



/*
 * Turn on and off the special map display.  -LM-
 */
static void toggle_special_map(bool use_map)
{
	char buf[1024];

	/* Ignore non-changes */
	if (use_map == use_special_map) return;

	/* Turn off the map */
	if (!use_map)
	{
		/* Cancel the special map rules */
		use_special_map = FALSE;

		/* Hide the dedicated map window */
		window_settings[TERM_MAP].visible = FALSE;

		/*
		 * Turn off bitmap graphics (we don't have the code to make graphics
		 * look acceptable without a flexible map view)
		 */
		if (mode_is_graphical(use_graphics))
		{
			use_graphics = arg_graphics = GRAPHICS_FONT;
			(void)init_graphics_hook(TRUE);

			/* Reset visuals */
			reset_visuals();
		}
	}

	/* Turn on the map */
	else
	{
		/* Point to the map term's window settings */
		window_type *win_ptr = &window_settings[TERM_MAP];


		/* We lack a map term (probable case) */
		if ((!angband_term[TERM_MAP]) || (!angband_term[TERM_MAP]->mapped_flag))
		{
			/* Remember which term we are hosting */
			win_ptr->Term_idx = TERM_MAP;

			/* Create and initialize the term */
			term_data_link_hook(&window_settings[TERM_MAP]);

			/* Add a pointer to the term to our easy access array */
			angband_term[TERM_MAP] = &term_data[TERM_MAP];

			/* Free the old "font_want" string (f any) */
			(void)string_free(win_ptr->font_want);

			/* Use the main term's font for the map term by default */
			win_ptr->font_want = string_make(window_settings[TERM_MAIN].font_file);
		}

		/* Access our desired font file */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, win_ptr->font_want);

		/* Use it */
		if (change_font_hook(win_ptr, buf))
		{
			/* On failure, access the standard font file */
			(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, DEFAULT_GAME_FONT);

			/* Use it */
			if (change_font_hook(win_ptr, buf))
			{
				/* Stop on error */
				return;
			}
		}


		/* Remember current application size (so this window gets loaded again) */
		win_ptr->resolution_x = app_wid;
		win_ptr->resolution_y = app_hgt;

		/* Show the dedicated map window */
		win_ptr->visible = TRUE;


		/* Cancel any graphics requests (we may not have the space) */
		if (arg_graphics != GRAPHICS_NONE) arg_graphics = GRAPHICS_FONT;

		/* Initialize graphics, calculate size, position, and rows/cols */
		if (!init_graphics_hook(FALSE))
		{
			/* If we fail THIS test, we really have a problem */
			plog("Error in map initialization!");
			return;
		}

		/* Set graphics to what we had before */
		arg_graphics = use_graphics;

		/* Try again.  Handle errors by staying with text/extended text */
		if (!init_graphics_hook(TRUE))
		{
			use_graphics = arg_graphics = GRAPHICS_FONT;

			plog("Error in map graphics initialization!");
		}

		/* Calculate map term window stuff */
		calc_map_display();
	}

	/* Set map display */
	use_special_map = use_map;

	/* Update graphics */
	init_graphics_hook(FALSE);
}


/*
 * Maximize windows.  We do this by enlarging the map and shoving the
 * sub-windows down and right.  -LM-
 */
void do_maximize(void)
{
	term *old = Term;

	int clear_left, clear_top, clear_right, clear_bottom;
	int i, cols, rows;
	srect rc_max;
	srect rc_cur;

	window_type *win_ptr;
	window_type *main_win_ptr;


	/* Default maximal outer dimensions is the application window */
	rc_max.left   = 0;
	rc_max.top    = 0;
	rc_max.right  = app_wid;
	rc_max.bottom = app_hgt;

	/* Handle windowed mode */
	if ((cur_display_mode == INTERFACE_WINDOW_EMPHA_MAP) ||
	    (cur_display_mode == INTERFACE_WINDOW_EMPHA_SUB))
	{
		rc_max.right  = client_w;
		rc_max.bottom = client_h;
	}

	/* Default current size is maximally negative */
	rc_cur.left = 9999;  rc_cur.top = 9999;  rc_cur.right = 0;  rc_cur.bottom = 0;

	/* Calculate window dimensions by examining visible term windows (& the tall display) */
	for (i = 0; i < TERM_MAX + 1; i++)
	{
		/* Get this window */
		window_type *win_ptr = &window_settings[i];

		/* Skip all non-visible windows */
		if (!win_ptr->visible) continue;

		/* Find the nearest left-top corner of any window */
		if (rc_cur.left > (long)(win_ptr->window_left))
		    rc_cur.left = win_ptr->window_left;
		if (rc_cur.top > (long)(win_ptr->window_top))
		    rc_cur.top = win_ptr->window_top;

		/* Find the furthest right-bottom corner of any window */
		if (rc_cur.right < (long)(win_ptr->window_left + win_ptr->window_wid))
		    rc_cur.right = win_ptr->window_left + win_ptr->window_wid;
		if (rc_cur.bottom < (long)(win_ptr->window_top + win_ptr->window_hgt))
		    rc_cur.bottom = win_ptr->window_top + win_ptr->window_hgt;
	}


	/* Remember the clear space we have on all sides */
	clear_left = rc_cur.left - rc_max.left;
	clear_top = rc_cur.top - rc_max.top;
	clear_right = rc_max.right - rc_cur.right;
	clear_bottom = rc_max.bottom - rc_cur.bottom;

	/* If there is no clear space, do nothing (we don't correct overlap) */
	if ((clear_left <= 0) && (clear_top <= 0) && (clear_right <= 0) && (clear_bottom <= 0))
	{
		return;
	}

	/* Make sure the standard display is active */
	display_change(DSP_NORM, 0, 0);


	/* Align all the windows to the top-left corner */
	for (i = TERM_SUBWINDOW; i < TERM_MAX + 1; i++)
	{
		/* Get this window */
		window_type *win_ptr = &window_settings[i];

		/* Skip all non-visible windows */
		if (!win_ptr->visible) continue;

		/* Align leftwards */
		win_ptr->window_left -= clear_left;

		/* Align upwards */
		win_ptr->window_top -= clear_top;
	}

	/* Add any freed space (usually none) to what we can use */
	clear_right += clear_left;
	clear_bottom += clear_top;


	/* Point to the main term data (standard display) */
	main_win_ptr = &window_settings[TERM_MAIN];

	/* Scan all terms (not the tall display) */
	for (i = TERM_MAX - 1; i >= 0; i--)
	{
		/* Get this window */
		win_ptr = &window_settings[i];

		/* Skip all non-visible windows */
		if (!win_ptr->visible) continue;

		/* This is a sub-window */
		if ((i >= TERM_SUBWINDOW) && (i < TERM_MAX))
		{
			/* Shove down or right, with down taking priority */

			/* This window is below the main term (allow some error) */
			if (win_ptr->window_top >= main_win_ptr->window_top + main_win_ptr->window_hgt - 5)
			{
				/* Shove it down */
				win_ptr->window_top += clear_bottom;
			}

			/* This window is to the right of the main term (allow some error) */
			else if (win_ptr->window_left >= main_win_ptr->window_left + main_win_ptr->window_wid - 5)
			{
				/* Shove it rightwards */
				win_ptr->window_left += clear_right;
			}
		}

		/* This is not a sub-window */
		else
		{
			/* Enlarge */
			win_ptr->window_wid += clear_right;
			win_ptr->window_hgt += clear_bottom;

			/* Calculate rows and columns */
			cols = (win_ptr->window_wid - win_ptr->border_left -
				win_ptr->border_right) / win_ptr->tile_wid;
			rows = (win_ptr->window_hgt - win_ptr->border_top -
				win_ptr->border_bottom) / win_ptr->tile_hgt;

			/* Save rows and cols, ensuring that they are never greater than 255 */
			win_ptr->cols = MIN(cols, 255);
			win_ptr->rows = MIN(rows, 255);

			/* Activate this Term */
			(void)Term_activate(angband_term[i]);

			/* Resize it */
			(void)Term_resize(win_ptr->cols, win_ptr->rows);
		}
	}

	/* Point to the main term data (tall display) */
	win_ptr = &window_settings[WINDOW_DISPLAY];

	/* Resize the tall display */
	if (win_ptr->window_wid < main_win_ptr->window_wid)
		win_ptr->window_wid = main_win_ptr->window_wid;
	if (win_ptr->window_hgt < main_win_ptr->window_hgt)
		win_ptr->window_hgt = main_win_ptr->window_hgt;

	/* Calculate rows and columns */
	cols = (win_ptr->window_wid - win_ptr->border_left -
		win_ptr->border_right) / win_ptr->tile_wid;
	rows = (win_ptr->window_hgt - win_ptr->border_top -
		win_ptr->border_bottom) / win_ptr->tile_hgt;

	/* Save rows and cols, ensuring that they are never greater than 255 */
	win_ptr->cols = MIN(cols, 255);
	win_ptr->rows = MIN(rows, 255);

	/* Activate the main Term */
	(void)Term_activate(term_main);

	/* Resize it to accomodate the (possibly enlarged) tall display */
	(void)Term_resize(win_ptr->cols, win_ptr->rows);


	/* Restore the previous active Term */
	(void)Term_activate(old);
}


/*
 * Interact with the user:  Interface options screen.  -LM-
 *
 * A dialog box would be nice...
 *
 * The updating and refreshing code is excessively complex.  XXX XXX
 */
errr Term_user_gui(int n)
{
	term *old = Term;

	bool maximize_flag = FALSE;
	bool special_map_flag = use_special_map;

	bool adjust_map = FALSE;
	bool dummy;

	char tmp[DESC_LEN];

	cptr s, t;

	char ch;

	/* There are eight global options */
	int opts = 8;

	int i, k = 0;

	/* Cursor position for help and notes */
	int cursor_y = ((opts+1) / 2) + ((TERM_MAX+1) / 2) + 6;

	char buf[DESC_LEN];


	/*
	 * Normal display, cleared, without centering (because we may be switching
	 * terms).
	 */
	display_change(DSP_REMEMBER | DSP_NORM | DSP_CLEAR | DSP_CX, 0, 0);


	/* Six global options/triggers, plus TERM_MAX windows, plus tall display */
	n = opts + TERM_MAX + 1;


	/* Prompt */
	(void)strnfmt(buf, sizeof(buf),
		"User Interface Options (8/2 move, 4/6 set, '?' for help, ESC to accept) ");
	prt(buf, 0, 0);

	/* Interact with the player */
	while (TRUE)
	{
		/* Clear top of screen */
		for (i = 1; i < cursor_y; i++) clear_row(i);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k) a = TERM_L_BLUE;

			/* Get the option text */
			if      (i == 0) s = "Display mode";
			else if (i == 1) s = "Graphics";
			else if (i == 2) s = "Sound";
			else if (i == 3) s = "Mouse";
			else if (i == 4) s = "Special map";
			else if (i == 5) s = "Maximize";
			else if (i == 6) s = "Cursor shape";
			else if (i == 7) s = "Optimization";
			else             s = angband_term_name[i - opts];

			/* Clear 't' */
			t = NULL;

			/* Get the current preference text */
			if (i == 0)
			{
				if      (arg_display_mode == INTERFACE_SCREEN_EMPHA_MAP)
					t = "Fullscreen, large map";
				else if (arg_display_mode == INTERFACE_SCREEN_EMPHA_SUB)
					t = "Fullscreen, subwindows";
				else if (arg_display_mode == INTERFACE_WINDOW_EMPHA_MAP)
					t = "Windowed, large map";
				else if (arg_display_mode == INTERFACE_WINDOW_EMPHA_SUB)
					t = "Windowed, subwindows";
			}

			else if (i == 1)
			{
				t = graphics_data[arg_graphics].full_name;
			}
			else if (i == 2)
			{
				if      (arg_sound == SOUND_NONE) t = "Off";
				else if (arg_sound == SOUND_ONLY) t = "Sound On";
				else if (arg_sound == MUSIC_ONLY) t = "Music On";
				else if (arg_sound == SOUND_AND_MUSIC) t = "Sound and Music";
			}
			else if (i == 3)
			{
				if      (use_mouse) t = "On";
				else                t = "Off";
			}
			else if (i == 4)
			{
				if (special_map_flag != use_special_map)
				{
					if (special_map_flag) t = "(activation pending)";
					else                  t = "(deactivation pending)";
				}
				else
				{
					if (use_special_map)  t = "On";
					else                  t = "Off";
				}
			}
			else if (i == 5)
			{
				if (maximize_flag) t = "(update pending)";
				else               t = "(no change)";
			}
			else if (i == 6)
			{
				if (cursor_shape) t = "Box cursor";
				else              t = "Underscore cursor";
			}
			else if (i == 7)
			{
				if      (media_quality_level == QUALITY_HIGH)   t = "High quality / Slow speed";
				else if (media_quality_level == QUALITY_MEDIUM) t = "Medium quality / Speed";
				else if (media_quality_level == QUALITY_LOW)    t = "Low quality / High speed";
				else                                            t = "(unknown)";
			}
			else if (i >= opts)
			{
				/* Get window */
				window_type *win_ptr = &window_settings[i - opts];

				/* If window is visible */
				if (window_settings[i - opts].visible)
				{
					/* Build an information string */
					(void)strnfmt(tmp, sizeof(tmp), "%8s", win_ptr->font_file);

					/* Point to it */
					t = tmp;
				}

				/* Indicate a hidden window */
				else
				{
					t = "(not visible)";
				}
			}

			/* Build the option text */
			(void)strnfmt(buf, sizeof(buf), "%c) %-14s%-23s", 'a' + i, s, t);

			/* Position the options (two columns & two blocks */
			if      (i <  (opts+1)/2)              c_prt(a, buf, i + 2, 0);
			else if (i <  opts)                    c_prt(a, buf, i - 2, 40);
			else if (i <  opts + 1 + TERM_MAX / 2) c_prt(a, buf, i, 0);
			else                                   c_prt(a, buf, i - 1 - TERM_MAX / 2, 40);
		}

		/* Output special notes */
		if (k == 1)
		{
			/* Display information about this graphics mode */
			(void)Term_gotoxy(7, cursor_y);
			c_roff(TERM_L_BLUE, format("%s", graphics_data[arg_graphics].desc), 2, 78);
		}
		else if ((k >= opts) && (window_settings[k - opts].visible))
		{
			/* Get window */
			window_type *win_ptr = &window_settings[k - opts];

			/* Move cursor */
			(void)Term_gotoxy(7, cursor_y);

			/* Build up term display notes */
			(void)strnfmt(buf, sizeof(buf), "Rows = %d, Cols = %d (minimum %dx%d)   Tile size can be up to %dx%d.",
				win_ptr->cols, win_ptr->rows,
				term_size_min[k - opts][0], term_size_min[k - opts][1],
				(win_ptr->window_wid - win_ptr->border_left - win_ptr->border_right) / term_size_min[k - opts][0],
				(win_ptr->window_hgt - win_ptr->border_top - win_ptr->border_bottom) / term_size_min[k - opts][1]);

			/* Display them */
			c_roff(TERM_L_BLUE, buf, 2, 78);
		}


		/* Position the cursor */
		if      (k < (opts+1)/2)              move_cursor(k + 2, 0);
		else if (k < opts)                    move_cursor(k - 2, 40);
		else if (k < opts + 1 + TERM_MAX / 2) move_cursor(k, 0);
		else                                  move_cursor(k - 1 - TERM_MAX / 2, 40);


		/* Get a key (no mouse?!?) */
		ch = inkey(FALSE);

		/* Allow direction keys */
		get_ui_direction(&ch, UI_NOROGUE, &dummy);

		/* Clear previous error or status messages */
		for (i = cursor_y; i < cursor_y + 8; i++) clear_row(i);


		/* Handle request to jump to a specific option */
		if (isalpha(ch))
		{
			/* Make lowercase */
			ch = tolower(ch);

			/* Get the option */
			i = ch - 'a';

			/* Must be legal */
			if ((i >= n) || (i < 0))
			{
				bell("Illegal display/multimedia option!");
				continue;
			}

			/* Jump to the option */
			k = i;

			/* Switch between standard and tall displays as needed */
			if      (k == TERM_MAIN + opts)      display_change(DSP_NORM, 0, 0);
			else if (k == WINDOW_DISPLAY + opts) display_change(DSP_TALL, 0, 0);

			continue;
		}

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				break;
			}

			case '?':
			{
				/* Move cursor */
				if (k < opts) (void)Term_gotoxy(7, cursor_y);
				else          (void)Term_gotoxy(7, cursor_y + 2);

				/* Helpful notes */
				if      (k == 0) c_roff(TERM_WHITE, "If you change the basic display mode, the game will use your new settings the next time it runs.", 2, 78);
				else if (k == 1) break; /* Help text already printed */
				else if (k == 2) c_roff(TERM_L_BLUE, "The game can play a variety of sounds.  If you are using certain ports (but not all) the game can also play music.  Sounds slow down the game only slightly, but a fairly modern computer is required for music.", 2, 78);
				else if (k == 3) c_roff(TERM_L_BLUE, "You can activate the mouse.  A left double-click lets you look around.  When looking, movement and clicks display information about the grid under the cursor and left double-click targets it.", 2, 78);
				else if (k == 4) c_roff(TERM_L_BLUE, "The flexible map view makes it possible to combine any text font with any map graphics.  You can also use a fancy font for the map and a plain one for basic text.  The special map view must be ON to display bitmapped graphics.", 2, 78);
				else if (k == 5) c_roff(TERM_L_BLUE, "If you see blank space between all of your windows and the edge of the display, try maximizing.", 2, 78);
				else if (k == 6) c_roff(TERM_L_BLUE, "Cursors are usually either a thick underscore or a box; you may pick either.  The bitmapped graphics modes use special cursors for the map window.", 2, 78);
				else if (k == 7) c_roff(TERM_L_BLUE, "Lots of pretty graphics slow down the game.  While most machines should not have a problem, if you find things bogging down, try optimizing for speed.  Implementation of this option is a work in progress, so consider turning off graphics, music, or other CPU-intensive features if you don't see enough of an improvement.", 2, 78);

				break;
			}

			case '8':
			{
				k = (n + k - 1) % n;

				/* Switch between standard and tall displays as needed */
				if      (k == TERM_MAIN + opts)      display_change(DSP_NORM, 0, 0);
				else if (k == WINDOW_DISPLAY + opts) display_change(DSP_TALL, 0, 0);

				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '\t':
			case '2':
			{
				k = (k + 1) % n;

				/* Switch between standard and tall displays as needed */
				if      (k == TERM_MAIN + opts)      display_change(DSP_NORM, 0, 0);
				else if (k == WINDOW_DISPLAY + opts) display_change(DSP_TALL, 0, 0);

				break;
			}

			case '=':
			case '+':
			case 'y':
			case '6':
			{
				if (k == 0)
				{
					/* Increment display mode, wrap around */
					arg_display_mode++;
					if (arg_display_mode >= 4) arg_display_mode = 0;

					/* Move cursor */
					(void)Term_gotoxy(7, cursor_y);

					/* Notes */
					c_roff(TERM_WHITE, "This setting will be applied upon game restart.  You should do this before setting any window options.", 2, 78);
				}
				else if (k == 1)
				{
					/* Increment graphics mode, skip unused modes, wrap around */
					arg_graphics++;
					while (strlen(graphics_data[arg_graphics].full_name) < 1) arg_graphics++;
					if (arg_graphics >= GRAPHICS_MAX) arg_graphics = GRAPHICS_NONE;

					/* Move cursor */
					(void)Term_gotoxy(7, cursor_y);

					/* Helpful notes */
					if (mode_is_graphical(arg_graphics) && !use_special_map)
						c_roff(TERM_YELLOW, "In order to show graphical images, the special map needs to be turned ON.", 2, 78);
					else
						c_roff(TERM_L_BLUE, "Graphics will be updated (if enough space exists) when you accept changes.", 2, 78);
				}
				else if (k == 2)
				{
					if (++arg_sound > SOUND_MODES_MAX) arg_sound = SOUND_NONE;

					/* Move cursor */
					(void)Term_gotoxy(7, cursor_y);

					/* Helpful notes */
					if ((arg_sound == MUSIC_ONLY) || (arg_sound == SOUND_AND_MUSIC))
						c_roff(TERM_YELLOW, format("Warning:  %s uses several different music formats, .it and .mid being two.  Not all sound cards can handle both.  If the game starts crashing on you, read \"/lib/xtra/music/jukebox.cfg\" for some possible fixes.", VERSION_NAME), 2, 78);
				}
				else if (k == 3)
				{
					use_mouse = TRUE;
				}
				else if (k == 4)
				{
					special_map_flag = TRUE;

					/* Notice changes */
					if (!use_special_map)
					{
						/* Move cursor */
						(void)Term_gotoxy(7, cursor_y);

						/* Helpful notes */
						c_roff(TERM_L_BLUE, "Note:  In order for the map display to update, you now need to accept changes and leave.", 2, 78);
					}
				}
				else if (k == 5)
				{
					maximize_flag = TRUE;

					/* Move cursor */
					(void)Term_gotoxy(7, cursor_y);

					/* Helpful notes */
					c_roff(TERM_L_BLUE, "Note:  In order for the display to update, you now need to accept changes and leave.  It is a good idea to do this before setting any fonts.", 2, 78);
				}
				else if (k == 6)
				{
					/* Toggle cursor shape */
					cursor_shape = !cursor_shape;
				}
				else if (k == 7)
				{
					if      (media_quality_level == QUALITY_LOW)
						media_quality_level = QUALITY_MEDIUM;
					else if (media_quality_level == QUALITY_MEDIUM)
						media_quality_level = QUALITY_HIGH;
					else if (media_quality_level == QUALITY_HIGH)
						media_quality_level = QUALITY_LOW;
				}
				else
				{
					/* If window is visible */
					if (window_settings[k - opts].visible)
					{
						/* Change the font, check legality, update rows/cols */
						window_change_font_hook(k - opts, cursor_y);

						/* Clear the window */
						(void)Term_clear();

						adjust_map = TRUE;
					}
				}

				break;
			}

			case '-':
			case '_':
			case 'n':
			case '4':
			{
				if (k == 0)
				{
					/* Decrement display mode, wrap around */
					arg_display_mode--;
					if (arg_display_mode < 0) arg_display_mode = 3;

					/* Move cursor */
					(void)Term_gotoxy(7, cursor_y);

					/* Notes */
					c_roff(TERM_WHITE, "This setting will be applied upon game restart.  You should do this before setting any window options.", 2, 78);
				}
				else if (k == 1)
				{
					/* Decrement graphics mode, wrap around */
					if (--arg_graphics < GRAPHICS_NONE) arg_graphics = GRAPHICS_MAX - 1;

					/* Skip past unused graphics (this is safe as long as GRAPHICS_NONE has a name) */
					while (strlen(graphics_data[arg_graphics].full_name) < 1) arg_graphics--;

					/* Move cursor */
					(void)Term_gotoxy(7, cursor_y);

					/* Helpful notes */
					if (mode_is_graphical(arg_graphics) && !use_special_map)
						c_roff(TERM_YELLOW, "In order to show graphical images, the special map needs to be turned ON.", 2, 78);
					else
						c_roff(TERM_L_BLUE, "Graphics will be updated (if enough space exists) when you accept changes.", 2, 78);
				}
				else if (k == 2)
				{
					if (--arg_sound < SOUND_NONE) arg_sound = SOUND_MODES_MAX;

					/* Move cursor */
					(void)Term_gotoxy(7, cursor_y);

					/* Helpful notes */
					if ((arg_sound == MUSIC_ONLY) || (arg_sound == SOUND_AND_MUSIC))
						c_roff(TERM_YELLOW, format("Warning:  %s uses several different music formats, .it and .mid being two.  Not all sound cards can handle both.  If the game starts crashing on you, read \"/lib/xtra/music/jukebox.cfg\" for some possible fixes.", VERSION_NAME), 2, 78);
				}
				else if (k == 3)
				{
					use_mouse = FALSE;
				}
				else if (k == 4)
				{
					special_map_flag = FALSE;

					/* Refuse to turn off if graphics are active */
					if (mode_is_graphical(arg_graphics))
					{
						/* Turn it on again */
						special_map_flag = TRUE;

						/* Move cursor */
						(void)Term_gotoxy(7, cursor_y);

						/* Helpful notes */
						c_roff(TERM_YELLOW, "You cannot turn off the special map display when using graphics.", 2, 78);
					}
				}
				else if (k == 5)
				{
					maximize_flag = FALSE;

					/* Move cursor */
					(void)Term_gotoxy(7, cursor_y);

					/* Helpful notes */
					c_roff(TERM_L_BLUE, "The display will no longer be fitted to screen when you leave.", 2, 78);
				}
				else if (k == 6)
				{
					/* Toggle cursor shape */
					cursor_shape = !cursor_shape;
				}
				else if (k == 7)
				{
					if      (media_quality_level == QUALITY_HIGH)
						media_quality_level = QUALITY_MEDIUM;
					else if (media_quality_level == QUALITY_MEDIUM)
						media_quality_level = QUALITY_LOW;
					else if (media_quality_level == QUALITY_LOW)
					media_quality_level = QUALITY_HIGH;
				}

				else
				{
					/* If window is visible */
					if (window_settings[k - opts].visible)
					{
						/* Change the font, check legality, update rows/cols */
						window_change_font_hook(k - opts, cursor_y);

						/* Clear the window */
						(void)Term_clear();

						adjust_map = TRUE;
					}
				}

				break;
			}

			default:
			{
				bell("Illegal command for interface preferences!");
				break;
			}
		}

		/* Change graphics (if defined) */
		if (arg_graphics != use_graphics)
		{
			/* Initialize graphics */
			if (init_graphics_hook(FALSE))
			{
				/* Remember the settings */
				use_graphics = arg_graphics;

				/* Calculate map display */
				calc_map_display();

				/* Recenter the map */
				verify_panel(0, FALSE);

				/* Reload all the visual prefs */
				reset_visuals();

				/* Clear the map window (later) */
				adjust_map = TRUE;
			}

			/* On failure, cancel the request */
			else arg_graphics = use_graphics;
		}

		/* Cancel */
		if (ch == ESCAPE) break;
	}

	/* Maximize windows if requested */
	if (maximize_flag) do_maximize();

	/* Various things about the map term need adjusting */
	/* This code needs a re-think */
	if (adjust_map)
	{
		/* Activate the map display Term */
		(void)Term_activate(use_special_map ? term_map : term_main);

		/* Wipe the map screen (to avoid visual artifacts) */
		(void)Term_clear();
		(void)Term_fresh();

		/* Recenter the map around the player */
		verify_panel(p_ptr->move_dir, TRUE);

		/* Calculate map term window stuff */
		calc_map_display();

		/* Wipe the map screen (to avoid visual artifacts) */
		(void)Term_clear();
		(void)Term_fresh();
	}

	/* Restore the previous active Term */
	(void)Term_activate(old);

	/* Change the map display if requested */
	toggle_special_map(special_map_flag);

	/* Do a complete redraw */
	do_cmd_redraw();


	/* Restore previous display */
	display_change(DSP_RESTORE, 0, 0);

	/*
	 * We cannot (easily) restore the previous centering because the term size
	 * may have changed.
	 */

	/* Recenter the map around the player */
	if (adjust_map) verify_panel(0, FALSE);

	/* Success */
	return (0);
}




/************************************************************************
 *                                                                      *
 *                                Fonts                                 *
 *                                                                      *
 ************************************************************************/


/* Number of possible font records */
#define MAX_FONT_RECORDS   64


/* Array of font records */
static font_record_type font_record[MAX_FONT_RECORDS];



/*
 * Hack -- given a simple filename, extract the "font size" info
 *
 * Return a pointer to a static buffer holding the capitalized base name.
 */
char *analyze_font(char *path, int *w, int *h)
{
	char *s, *p;

	/* Start at the end */
	p = path + strlen(path) - 1;

	/* Back up to divider */
	while ((p >= path) && (*p != ':') && (*p != '\\') && (!strchr(PATH_SEP, *p))) --p;

	/* Advance to file name */
	++p;

	/* Find first 'X' or 'x' */
	s = strchr(p, 'X');
	if (!s) s = strchr(p, 'x');

	/* Extract font width */
	*w = atoi(p);

	/* Extract height */
	*h = s ? atoi(s+1) : 0;

	/* Result */
	return (p);
}



/*
 * Build a list of all font files available to the game.  Lacking a platform-
 * independent way to list files within a directory, we must beg for outside
 * help in the form of a list file.
 */
void get_font_list(char *filename, char *suffix)
{
	int i = 0;
	int wid, hgt;

	char path[1024];
	char buf[1024];
	FILE *fff;


	/* Build a path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA_FONT, filename);

	/* Try to open the file */
	fff = my_fopen(path, "r");

	/* Handle missing file */
	if (!fff) plog("Was not able to find any listing of fonts.");

	/* Process the input file */
	while (0 == my_fgets(fff, buf, sizeof(buf)))
	{
		/* Ignore blank lines */
		if (!buf[0]) continue;

		/* Ignore comments */
		if (buf[0] == '#') continue;

		/* Require the correct file suffix (optionally) */
		if ((suffix) && (!strstr(buf, suffix))) continue;

		/* Analyze the font name to determine font width and height */
		(void)analyze_font(buf, &wid, &hgt);

		/* If we got no useable information, ignore this font */
		if (!wid || !hgt) continue;

		/* Save this font */
		font_record[i].wid = wid;
		font_record[i].hgt = hgt;
		(void)my_strcpy(font_record[i].name, buf, 40);

		/* Increment list number, stop before overflowing the array */
		if (++i >= MAX_FONT_RECORDS) break;
	}

	/* Close the listing file */
	(void)my_fclose(fff);
}



/*
 * Change a window font, verify dimensions.
 *
 * This function is used for display modules that don't have a special interface
 * for choosing fonts.
 */
void window_change_font_gui(int idx, int cursor_y)
{
	/* Get window */
	window_type *win_ptr = &window_settings[idx];

	char str[DESC_LEN];
	char buf[1024];
	int i, k, num, cols, rows;
	byte attr;
	byte index_valid[MAX_FONT_RECORDS];

	/* Calculate available space */
	int window_wid = (win_ptr->window_wid - win_ptr->border_left - win_ptr->border_right);
	int window_hgt = (win_ptr->window_hgt - win_ptr->border_top - win_ptr->border_bottom);

	/* Unused variable */
	(void)cursor_y;

	/* Clear the screen */
	(void)Term_clear();

	/* Wipe our array of valid indices */
	C_WIPE(index_valid, sizeof(index_valid), byte);


	/* List the available fonts */
	for (i = 0, k = 0; i < MAX_FONT_RECORDS; i++)
	{
		/* Font has no size */
		if (!(font_record[i].wid) || !(font_record[i].hgt)) continue;

		/* Font is too large for this window */
		if (term_size_min[idx][0] * font_record[i].wid > window_wid) continue;
		if (term_size_min[idx][1] * font_record[i].hgt > window_hgt) continue;


		/* Build a listing */
		(void)my_strcpy(str, format("%c) %-37s", index_chars[k], font_record[i].name), sizeof(str));

		/* Colorize (note current font) */
		if (streq(win_ptr->font_file, font_record[i].name)) attr = TERM_L_BLUE;
		else                                                attr = TERM_WHITE;

		/* List the font (two columns) */
		if (k < Term->rows - 4) c_prt(attr, str, k + 2,                     0);
		else                    c_prt(attr, str, k + 2 - (Term->rows - 4), 40);

		/* Note that this is a valid index */
		index_valid[i] = TRUE;

		/* Increment listing */
		k++;
	}

	/* Remember how many fonts are available */
	num = k;

	/* Handle total lack of fonts */
	if (num == 0) return;


	/* Title */
	c_prt(TERM_L_BLUE, format("Window #%d:  Font selection", idx), 0, 0);


	/* Interact */
	while (TRUE)
	{
		char ch;

		/* Prompt */
		prt(format("Choose which font? (Fonts %c-%c, ESC to exit) ",
			index_chars[0], index_chars[num]), Term->rows - 1, 0);

		/* Query */
		ch = inkey(FALSE);

		/* Handle escape */
		if (ch == ESCAPE) return;

		/* Handle mouse and direction keys at some point */

		/* Scan all possible indices */
		for (i = 0, k = 0; i < MAX_FONT_RECORDS; i++)
		{
			/* Skip invalid */
			if (!index_valid[i]) continue;

			/* Match the index */
			if (ch == index_chars[k++]) break;
		}

		/* We chose a valid index */
		if (i < MAX_FONT_RECORDS) break;

		/* Try, try again  XXX */
		prt("Sorry, the index chosen was not recognized.  Press any key to continue.", Term->rows - 1, 0);
		(void)inkey(FALSE);
	}

	/* Build the font file path */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, font_record[i].name);

	/* Change the font */
	(void)change_font_hook(win_ptr, buf);

	/* Get rows and columns */
	cols = window_wid / win_ptr->tile_wid;
	rows = window_hgt / win_ptr->tile_hgt;

	/* Save rows and cols, ensuring that they are never greater than 255 */
	win_ptr->cols = MIN(cols, 255);
	win_ptr->rows = MIN(rows, 255);
}


/*
 * Find and use the largest font that will fit in the given cell size.
 *
 * Note:  The font record listing assumes we will scan from smallest to
 * largest.
 */
bool use_largest_font(window_type *win_ptr, int cell_wid, int cell_hgt)
{
	char path[1024];

	/* Keep looking for the bext available font */
	while (TRUE)
	{
		int i, font_idx = -1;
		int font_wid = 0;
		int font_hgt = 0;

		/* Scan our array of available font sizes */
		for (i = 0; i < MAX_FONT_RECORDS; i++)
		{
			/* Ignore fonts unless larger than our best yet */
			if ((font_record[i].wid <= font_wid) &&
				(font_record[i].hgt <= font_hgt)) continue;

			/* Ignore fonts too large for the window */
			if ((font_record[i].wid > cell_wid) ||
				(font_record[i].hgt > cell_hgt)) continue;

			/* Ignore unless cell area is larger */
			if ((font_record[i].wid * font_record[i].hgt) <=
			    (font_wid * font_hgt)) continue;

			/* Save as a new best available font size */
			font_wid = font_record[i].wid;
			font_hgt = font_record[i].hgt;
			font_idx = i;
		}

		/* If we found no fonts, return an error */
		if (font_idx < 0) return (FALSE);

		/* Build a path name */
		(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA_FONT, font_record[font_idx].name);

		/* Try to use this font (changes tile size) */
		if (change_font_hook(win_ptr, path))
		{
			/* If we failed, then delete this font record and try again */
			font_record[font_idx].hgt = 0;
			font_record[font_idx].wid = 0;
			font_record[font_idx].name[0] = '\0';
			continue;
		}

		break;
	}

	/* Success */
	return (TRUE);
}





/************************************************************************
 *                                                                      *
 *                                Sounds                                *
 *                                                                      *
 ************************************************************************/


#ifdef USE_SOUND

/*
 * XXX - Hacked from "tokenize()" in files.c.
 *
 * Extract "tokens" from a buffer
 *
 * This function uses "whitespace" as delimiters, and treats any amount of
 * whitespace as a single delimiter.  We will never return any empty tokens.
 * When given an empty buffer, or a buffer containing only "whitespace", we
 * will return no tokens.  We will never extract more than "num" tokens.
 *
 * By running a token through the "text_to_ascii()" function, you can allow
 * that token to include (encoded) whitespace, using "\s" to encode spaces.
 *
 * We save pointers to the tokens in "tokens", and return the number found.
 */
static s16b tokenize_whitespace(char *buf, s16b num, char **tokens)
{
	int k = 0;

	char *s = buf;


	/* Process */
	while (k < num)
	{
		char *t;

		/* Skip leading whitespace */
		for ( ; *s && my_isspace((unsigned char)*s); ++s) /* loop */;

		/* All done */
		if (!*s) break;

		/* Find next whitespace, if any */
		for (t = s; *t && !my_isspace((unsigned char)*t); ++t) /* loop */;

		/* Nuke and advance (if necessary) */
		if (*t) *t++ = '\0';

		/* Save the token */
		tokens[k++] = s;

		/* Advance */
		s = t;
	}

	/* Count */
	return (k);
}

/*
 * Save file names for the sounds used by the game.
 *
 * This function assumes the presence of 1) of a configuration file and 2)
 * various .wav-format soundfiles in in the sound directory (usually
 * lib/xtra/sound/).
 */
bool load_sound_prefs(void)
{
	int i, j, num;
	bool sounds_flag = FALSE;

	char tmp[1024];
	char path[1024];
	char wav_path[1024];

	char *zz[SAMPLE_MAX];

	FILE *fff;
	int fd;


	/* Already done */
	if (sounds_ready) return (TRUE);

	/* Locate the sound configuration file */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA_SOUND, "sound.cfg");

	/* Open it */
	fff = my_fopen(path, "r");

	/* File can't be found - return */
	if (!fff) return (FALSE);

	/* For each possible sound */
	for (i = 0; i < MSG_MAX; i++)
	{
		/* Ignore empty sound strings */
		if (!angband_sound_name[i][0]) continue;

		/* Load the list of possible sounds for this event */
		rd_config_str(fff, "Sound", angband_sound_name[i], tmp, sizeof(tmp), "");

		/* Strip whitespace, extract each sound name */
		num = tokenize_whitespace(tmp, SAMPLE_MAX, zz);

		/* We can only store SAMPLE_MAX sounds */
		if (num > SAMPLE_MAX) num = SAMPLE_MAX;

		/* For each sound, */
		for (j = 0; j < num; j++)
		{
			/* Get the sound */
			(void)path_build(wav_path, sizeof(wav_path), ANGBAND_DIR_XTRA_SOUND, zz[j]);

			/* Try to open this file */
			fd = fd_open(wav_path, O_WRONLY);

			/* File exists */
			if (fd >= 0)
			{
				/* Close it */
				(void)fd_close(fd);

				/* Save the sound name, take note */
				sound_file[i][j] = string_make(zz[j]);
				sounds_flag = TRUE;
			}
		}
	}

	/* Free the sound config file index */
	free_file_index();

	/* The sounds are ready */
	if (sounds_flag) sounds_ready = TRUE;

	/* Return "we have at least one legal sound" */
	return (sounds_flag);
}



/************************************************************************
 *                                                                      *
 *                                Music                                 *
 *                                                                      *
 ************************************************************************/




/*
 * Save file names for the music used by the game.
 *
 * This function assumes the presence of 1) of a configuration file and 2)
 * various readable files in in the music directory (usuallly lib/xtra/music/).
 */
bool load_music_prefs(void)
{
	int i, j, num;
	bool music_flag = FALSE;

	char tmp[1024];
	char path[1024];
	char wav_path[1024];

	char *zz[SAMPLE_MAX];

	FILE *fff;
	int fd;


	/* Already done */
	if (music_ready) return (TRUE);

	/* Locate the music configuration file */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA_MUSIC, "jukebox.cfg");

	/* Open it */
	fff = my_fopen(path, "r");

	/* File can't be found - return */
	if (!fff) return (FALSE);


	/* For each possible music */
	for (i = 0; i < MUSIC_MAX; i++)
	{
		/* Ignore empty music strings */
		if (!angband_music_name[i][0]) continue;

		/* Load the list of possible songs for this theme */
		rd_config_str(fff, "Music", angband_music_name[i], tmp, sizeof(tmp), "");

		/* Strip whitespace, extract each song name */
		num = tokenize_whitespace(tmp, SAMPLE_MAX, zz);

		/* We can only store SAMPLE_MAX songs */
		if (num > SAMPLE_MAX) num = SAMPLE_MAX;

		/* For each song, */
		for (j = 0; j < num; j++)
		{
			/* Get the music */
			(void)path_build(wav_path, sizeof(wav_path), ANGBAND_DIR_XTRA_MUSIC, zz[j]);

			/* Try to open this file */
			fd = fd_open(wav_path, O_WRONLY);

			/* File exists */
			if (fd >= 0)
			{
				/* Close it */
				(void)fd_close(fd);

				/* Save the song name, take note */
				music_file[i][j] = string_make(zz[j]);
				music_flag = TRUE;
			}
		}
	}

	/* Free the music config file index */
	free_file_index();

	/* The music is ready */
	if (music_flag) music_ready = TRUE;

	/* Return "we have at least one legal song" */
	return (music_flag);
}


/*
 * Given an estimated level of danger/excitement (or other information), play
 * appropriate music on the jukebox.  -LM-
 *
 * If the music doesn't have to be adjusted immediately, then we balance our
 * desire for music appropriate to the situation with our desire to play a
 * given song long enough to do it justice and not long enough to be boring.
 * We are quicker on the draw when high-intensity music is requested, and
 * also when the danger/excitement level has changed greatly.
 *
 * Note that all play lengths are in seconds (not milliseconds).
 */
bool jukebox(int *v, int time, int *fade_in, int *fade_out)
{
	int theme;

	/* Decode requests for immediate change to suit the given theme  XXX XXX */
	bool adjust_now = ((*v >= 100) || (cur_music_theme < 0));

	/* Extract theme */
	if (*v >= 100) *v -= 100;
	theme = *v;


	/* Calculate fade-in and fade-out time for the danger/excitement themes */
	if ((theme >= 0) && (theme <= 5))
	{
		/* Be more abrupt when things are dangerous */
		*fade_in  = 1000 - (theme * 180);
		*fade_out =  500 - (theme * 80);
	}

	/* Otherwise, choose plausible values */
	else
	{
		*fade_in  = 800;
		*fade_out = 400;
	}


	/* We need to adjust to the current theme right now */
	if ((adjust_now) && (theme != cur_music_theme)) return (TRUE);


	/* Get length of time (in seconds) since the current song began */
	time -= cur_music_start_time;

	/* Refuse to change songs more than once every 10 seconds */
	if (time < 10) return (FALSE);

	/* The theme is one of the danger/excitement themes */
	if ((theme >= 0) && (theme <= 5))
	{
		/* Get signed difference in theme intensity */
		int diff = theme - cur_music_theme;

		/* Handle various possible cases */
		if      (diff >=  4)
		{
			if (time >= 10) return (TRUE);
		}
		else if (diff >=  3)
		{
			if (time >= 20) return (TRUE);
		}
		else if (diff >=  2)
		{
			if (time >= 30) return (TRUE);
		}
		else if (diff >=  1)
		{
			if (time >= MAX(cur_song_length, 50)) return (TRUE);
		}
		else if (diff >=  0)
		{
			if (time >= MAX(cur_song_length, 120)) return (TRUE);
		}
		else if (diff >= -1)
		{
			if (time >= MAX(cur_song_length, 60)) return (TRUE);
		}
		else if (diff >= -2)
		{
			if (time >= 40) return (TRUE);
		}
		else if (diff >= -3)
		{
			if (time >= 30) return (TRUE);
		}
	}

	/* The theme is an unknown case */
	else
	{
		if (time >= MAX(cur_song_length, 60)) return (TRUE);
	}

	/* Do nothing */
	return (FALSE);
}


#endif /* USE_SOUND */








/************************************************************************
 *                                                                      *
 *                             Config Files                             *
 *                                                                      *
 ************************************************************************/


static bool file_index_ready = FALSE;

/* Allow <<pick a number arbitrarily>> 256 sections */
#define MAX_FILE_SECTIONS   256


/*
 * An array of section headings  (we /could/ allocate these dynamically...)
 */
static file_section_type file_section_data[MAX_FILE_SECTIONS];


/*
 * Build a config file index by finding all the section headings and listing
 * them in an array, together with their position in the file (offset from
 * file start).
 *
 * You are responsible for freeing memory allocated in this function; call
 * "free_file_index()".
 *
 * Return 0 if no errors happened.
 */
static errr make_file_index(FILE *fff, int max_sections)
{
	char buf[1024];
	int i = 0;
	long pos;


	/* Parse the file */
	while (TRUE)
	{
		/* Read a line or stop */
		if (my_fgets(fff, buf, sizeof(buf))) break;

		/* Ignore if first character is not a '[' */
		if (buf[0] != '[') continue;

		/* Save header text */
		file_section_data[i].text = string_make(buf);

		/* Get current read position within file */
		pos = ftell(fff);

		/* Handle errors */
		if (pos < 0) return (1);

		/* Save header offset */
		file_section_data[i].offset = pos;

		/* Do not overflow the array */
		if (++i >= max_sections) break;
	}

	/* Add a sentinel */
	if (i < max_sections)
	{
		file_section_data[i].offset = -1;
	}

	/* File index is ready */
	file_index_ready = TRUE;

	/* Success */
	return (0);
}


/*
 * Read data from a config file.  If "is_number" is TRUE, "data" and "v" must
 * be ints.  If FALSE, they must be strings.
 *
 * This code is a cross-platform re-implementation of (part of) Microsoft's
 * .ini file API.
 *
 * The config file need not exist.  If it doesn't, or anything else prevents a
 * read of the requested data, we use the default values.
 */
void rd_config_data_aux(FILE *fff, cptr section, cptr line, char *data, int data_size)
{
	int i, j;
	char buf[1024];
	char tmp[1024];
	char *s;


	/* Assume no data */
	data[0] = '\0';

	/* No config file available */
	if (!fff) return;


	/* The file section index is not ready */
	if (!file_index_ready)
	{
		/* Make it so (return on error) */
		if (make_file_index(fff, MAX_FILE_SECTIONS)) return;
	}


	/* Scan the file index */
	for (i = 0; i < MAX_FILE_SECTIONS; i++)
	{
		/* If we hit the sentinel, return */
		if (file_section_data[i].offset < 0) return;

		/* If the section headers match, accept */
		if (streq(file_section_data[i].text, format("[%s]", section)))
		{
			/* Jump to the stored offset from file start */
			fseek(fff, file_section_data[i].offset, SEEK_SET);

			/* Accept */
			break;
		}
	}

	/* Header not found */
	if (i >= MAX_FILE_SECTIONS) return;

	/* Scan this section, starting at the current pointer position */
	while (TRUE)
	{
		/* Read a line or stop */
		if (my_fgets(fff, buf, sizeof(buf))) break;

		/* Ignore comments and empty lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Stop when we hit a section header */
		if (buf[0] == '[') break;

		/* Local copy */
		strcpy(tmp, buf);

		/* Save only that portion of tmp before any '=' */
		for (i = 0; tmp[i]; i++)
		{
			if (tmp[i] == '=')
			{
				/* Remove the '=' and any blank space immediately preceeding */
				for (j = i; ((tmp[j] == '=') || (tmp[j] == ' ')); j--) tmp[j] = '\0';

				/* Save 'i' as the position just after the '=' */
				i++;
				break;
			}
		}

		/* This is our line */
		if (streq(line, tmp))
		{
			/* Move forward from the '=' until we hit a non-space */
			for (s = buf + i; *s; s++)
			{
				if (*s != ' ') break;
			}

			/* Copy into our data string */
			(void)my_strcpy(data, s, data_size);

			/* Done */
			return;
		}
	}
}

/*
 * Read information as a number
 */
int rd_config_int(FILE *fff, cptr section, cptr line, const int v)
{
	char tmp[DESC_LEN];

	/* Get the data */
	rd_config_data_aux(fff, section, line, tmp, sizeof(tmp));

	/* Get numerical data */
	if (tmp[0] && isdigit(tmp[0]))
	{
		return (atoi(tmp));
	}

	/* Use the default if no number found */
	return (v);
}

/*
 * Read information as a string
 */
void rd_config_str(FILE *fff, cptr section, cptr line, char *data, int data_size, cptr v)
{
	char tmp[1024];

	/* Get the data */
	rd_config_data_aux(fff, section, line, tmp, sizeof(tmp));

	/* Note lack of information */
	if (tmp[0] == '\0')
	{
		(void)my_strcpy(data, v, data_size);
	}

	/* Handle string data */
	else
	{
		(void)my_strcpy(data, tmp, data_size);
	}
}


/*
 * Free a file index array.
 *
 * This should always be done after using a file section array to seek through
 * a file or memory leaks will occur.
 */
void free_file_index(void)
{
	int i;

	/* Scan down the array */
	for (i = 0; i < MAX_FILE_SECTIONS; i++)
	{
		/* Stop at the sentinel */
		if (file_section_data[i].offset < 0) break;

		/* Free all strings */
		(void)string_free(file_section_data[i].text);
	}

	/* File index is no longer ready */
	file_index_ready = FALSE;
}



/*
 * Replace a section with the given text
 * Adopted from "remove_old_dump()" by -Mogami-
 */
void replace_section(cptr orig_file, char *section, char *data)
{
	FILE *tmp_fff = NULL, *orig_fff = NULL;

	char tmp_file[1024];
	char buf[1024];
	bool copy = TRUE;
	bool changed = FALSE;


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the original file in read-only mode */
	orig_fff = my_fopen(orig_file, "r");

	/* If original file does not exist, */
	if (!orig_fff)
	{
		/* Create the original file */
		orig_fff = my_fopen(orig_file, "w");
	}

	/* Open a new temporary file */
	tmp_fff = my_fopen_temp(tmp_file, sizeof(tmp_file));

	/* Handle error */
	if (!tmp_fff)
	{
		(void)my_fclose(orig_fff);
		return;
	}


	/* Loop for every line */
	while (TRUE)
	{
		/* Read a line */
		if (my_fgets(orig_fff, buf, sizeof(buf))) break;

		/* This line is a section heading */
		if (buf[0] == '[')
		{
			/* If we were within our section, we are no longer.  Resume copying. */
			if (!copy)
			{
				copy = TRUE;
			}

			/* We have found the section requested */
			else if (streq(buf, format("[%s]", section)))
			{
				/* Replace this section; note change in files */
				copy = FALSE;
				changed = TRUE;

				/* Insert "data" (if defined) */
				if (data)
				{
					/* Re-write the header */
					fprintf(tmp_fff, format("%s\n", buf));

					/* Insert the new data */
					fprintf(tmp_fff, data);

					/* Insert a spacer line */
					fprintf(tmp_fff, "\n");
				}
			}
		}

		/* Copy unless directed otherwise */
		if (copy) fprintf(tmp_fff, "%s\n", buf);
	}

	/* If we didn't find the section we were looking for, append the data */
	if (!changed && data)
	{
		/* Write the header */
		fprintf(tmp_fff, format("[%s]\n", section));

		/* Write the data */
		fprintf(tmp_fff, data);

		/* Insert a spacer line */
		fprintf(tmp_fff, "\n");

		/* Note change */
		changed = TRUE;
	}

	/* Close files */
	(void)my_fclose(orig_fff);
	(void)my_fclose(tmp_fff);

	/* If there are changes, overwrite the original file with the new one */
	if (changed)
	{
		/* Copy contents of temporary file */
		tmp_fff = my_fopen(tmp_file, "r");
		orig_fff = my_fopen(orig_file, "w");

		while (!my_fgets(tmp_fff, buf, sizeof(buf)))
		{
			fprintf(orig_fff, "%s\n", buf);
		}

		(void)my_fclose(orig_fff);
		(void)my_fclose(tmp_fff);
	}

	/* Kill the temporary file */
	(void)fd_kill(tmp_file);
}



/*
 * Save options for a single window
 */
static void save_prefs_aux(char *ini_file, window_type *win_ptr, char *sec_name)
{
	char buf[4096];

	/* Start with no information */
	buf[0] = '\0';

	/* Visible */
	(void)my_strcat(buf, format("Visible       = %d\n", win_ptr->visible), sizeof(buf));

	/* Window position */
	(void)my_strcat(buf, format("WindowLeft    = %d\n", win_ptr->window_left), sizeof(buf));
	(void)my_strcat(buf, format("WindowTop     = %d\n", win_ptr->window_top), sizeof(buf));

	/* Window size */
	(void)my_strcat(buf, format("WindowWid     = %d\n", win_ptr->window_wid), sizeof(buf));
	(void)my_strcat(buf, format("WindowHgt     = %d\n", win_ptr->window_hgt), sizeof(buf));

	/* Inner border */
	(void)my_strcat(buf, format("BorderLeft    = %d\n", win_ptr->border_left), sizeof(buf));
	(void)my_strcat(buf, format("BorderTop     = %d\n", win_ptr->border_top), sizeof(buf));
	(void)my_strcat(buf, format("BorderRight   = %d\n", win_ptr->border_right), sizeof(buf));
	(void)my_strcat(buf, format("BorderBottom  = %d\n", win_ptr->border_bottom), sizeof(buf));

	/* Font */
	(void)my_strcat(buf, format("Font          = %s\n",
		(win_ptr->font_file ? win_ptr->font_file : DEFAULT_GAME_FONT)), sizeof(buf));

	/* Tile size (merely for reference; actual values are auto-calculated) */
	(void)my_strcat(buf, format("TileWid       = %d\n", win_ptr->tile_wid), sizeof(buf));
	(void)my_strcat(buf, format("TileHgt       = %d\n", win_ptr->tile_hgt), sizeof(buf));

	/* Keys stored */
	(void)my_strcat(buf, format("Keys          = %d\n", win_ptr->keys), sizeof(buf));

	/* Save the data, replacing anything currently in this section */
	replace_section(ini_file, sec_name, buf);
}

/*
 * Save window and game interface options
 *
 * We assume that the windows have all been initialized.
 */
void save_prefs(void)
{
	int i;

	char buf[4096];
	char path[1024];
	char name[DESC_LEN];


	/* Store preferences in the working folder */
	(void)path_build(path, sizeof(path), WorkingFolder, "config.txt");

	/* Build an application name */
	(void)my_strcpy(name, format("%s", VERSION_NAME), sizeof(name));


	/* Start with no information */
	buf[0] = '\0';

	/* Save the graphics setting */
	(void)my_strcat(buf, format("Graphics      = %s\n", graphics_data[arg_graphics].file_name), sizeof(buf));

	/* Save the "use_sound" flag */
	(void)my_strcat(buf, format("Sound         = %d\n", use_sound), sizeof(buf));

	/* Save the "use_mouse" flag */
	(void)my_strcat(buf, format("Mouse         = %d\n", (use_mouse ? 1 : 0)), sizeof(buf));

	/* Save the requested display mode */
	(void)my_strcat(buf, format("Display mode  = %d\n", arg_display_mode), sizeof(buf));

	/* Save the optimization level */
	(void)my_strcat(buf, format("Media quality = %d\n", media_quality_level), sizeof(buf));

	/* Save the cursor shape */
	(void)my_strcat(buf, format("Cursor shape  = %d\n", (cursor_shape ? 1 : 0)), sizeof(buf));

	/* Save the application window size */
	(void)my_strcat(buf, format("\nScreenLeft    = %d\n", app_left), sizeof(buf));
	(void)my_strcat(buf, format("ScreenTop     = %d\n", app_top), sizeof(buf));
	(void)my_strcat(buf, format("ScreenWid     = %d\n", app_wid), sizeof(buf));
	(void)my_strcat(buf, format("ScreenHgt     = %d\n\n", app_hgt), sizeof(buf));


	/* Save the data, replacing anything currently in this section */
	replace_section(path, name, buf);

	/* Save window options (terms, plus the 1 special window) */
	for (i = 0; i < TERM_MAX + 1; i++)
	{
		(void)strnfmt(buf, sizeof(buf), "%dx%d screen:  Display mode %d, Term-%d",
			app_wid, app_hgt, cur_display_mode, i);

		save_prefs_aux(path, &window_settings[i], buf);
	}
}


/*
 * Load options for a single window
 */
static void load_prefs_aux(FILE *fff, window_type *win_ptr, char *sec_name)
{
	char tmp[256];

	/* Visible */
	win_ptr->visible = (rd_config_int(fff, sec_name, "Visible", win_ptr->visible) != 0);

	/* Window position */
	win_ptr->window_left = rd_config_int(fff, sec_name, "WindowLeft", win_ptr->window_left);
	win_ptr->window_top  = rd_config_int(fff, sec_name, "WindowTop",  win_ptr->window_top);

	/* Window size */
	win_ptr->window_wid = rd_config_int(fff, sec_name, "WindowWid", win_ptr->window_wid);
	win_ptr->window_hgt = rd_config_int(fff, sec_name, "WindowHgt", win_ptr->window_hgt);

	/* Inner border */
	win_ptr->border_left   = rd_config_int(fff, sec_name, "BorderLeft",   win_ptr->border_left);
	win_ptr->border_top    = rd_config_int(fff, sec_name, "BorderTop",    win_ptr->border_top);
	win_ptr->border_right  = rd_config_int(fff, sec_name, "BorderRight",  win_ptr->border_right);
	win_ptr->border_bottom = rd_config_int(fff, sec_name, "BorderBottom", win_ptr->border_bottom);

	/* Desired font */
	if (win_ptr->font_want)
		rd_config_str(fff, sec_name, "Font", tmp, sizeof(tmp), win_ptr->font_want);
	else
		rd_config_str(fff, sec_name, "Font", tmp, sizeof(tmp), DEFAULT_GAME_FONT);
	win_ptr->font_want = string_make(tmp);

	/* Keypress storage size */
	win_ptr->keys = rd_config_int(fff, sec_name, "Keys", win_ptr->keys);
}


/*
 * Load window and game interface options
 */
void load_prefs(bool first_pass)
{
	int i;
	int tmp_wid, tmp_hgt;

	char path[1024];
	char tmp[256];

	FILE *fff;


	/* The config file lives in the working folder (either app dir or user dir) */
	(void)path_build(path, sizeof(path), WorkingFolder, "config.txt");

	/* Open the config file */
	fff = my_fopen(path, "r");

	/* If not found, we can safely load prefs anyway in order to set defaults */


	/* Hack -- get information we need immediately */
	if (first_pass)
	{
		/* Load the display mode */
		arg_display_mode = cur_display_mode =
			rd_config_int(fff, VERSION_NAME, "Display mode",
				INTERFACE_SCREEN_EMPHA_MAP);

		/* Load the application window size and position */
		app_left = rd_config_int(fff, VERSION_NAME, "ScreenLeft", 0);
		app_top  = rd_config_int(fff, VERSION_NAME, "ScreenTop", 0);
		app_wid  = rd_config_int(fff, VERSION_NAME, "ScreenWid", screen_w);
		app_hgt  = rd_config_int(fff, VERSION_NAME, "ScreenHgt", screen_h);

		/* Requested application size is illegal */
		if ((app_wid > screen_w) ||
			(app_hgt > screen_h) ||
			(app_wid < 640) ||
			(app_hgt < 480))
		{
			/* Make it the size of the screen */
			app_wid = screen_w;
			app_hgt = screen_h;
		}

		/* We accept any position XXX */

		(void)my_fclose(fff);
		return;
	}


	/* Get the requested graphics name (default to "font") */
	rd_config_str(fff, VERSION_NAME, "Graphics", tmp, sizeof(tmp),
		graphics_data[GRAPHICS_FONT].file_name);

	/* Look for the requested graphics setting */
	for (i = 0; i < GRAPHICS_MAX; i++)
	{
		/* Accept exact matches */
		if (streq(tmp, graphics_data[i].file_name))
		{
			arg_graphics = i;
			break;
		}
	}

	/* Display mode */
	arg_display_mode = cur_display_mode =
		rd_config_int(fff, VERSION_NAME, "Display mode",
			INTERFACE_SCREEN_EMPHA_MAP);

	/* Optimization level */
	media_quality_level = rd_config_int(fff, VERSION_NAME, "Media quality", QUALITY_HIGH);

	/* Sound */
	arg_sound = rd_config_int(fff, VERSION_NAME, "Sound", 0);

	/* Mouse */
	use_mouse = (rd_config_int(fff, VERSION_NAME, "Mouse", 1) != 0);

	/* Cursor shape */
	cursor_shape = (rd_config_int(fff, VERSION_NAME, "Cursor shape", 0) != 0);

#ifdef SUPPORT_GAMMA
	/* Extract the gamma correction */
	gamma_correction = rd_config_int(fff, VERSION_NAME, "Gamma", 0);
#endif /* SUPPORT_GAMMA */

	/* Fiddle mode (debug savefile loading) */
	arg_fiddle = (rd_config_int(fff, VERSION_NAME, "Fiddle", 0) != 0);

	/* Wizard mode */
	arg_wizard = (rd_config_int(fff, VERSION_NAME, "Wizard", 0) != 0);

	/* Original keyset (deprecated) */
	arg_force_original = (rd_config_int(fff, VERSION_NAME, "Original keyset", 0) != 0);

	/* Roguelike keyset (deprecated) */
	arg_force_roguelike = (rd_config_int(fff, VERSION_NAME, "Roguelike keyset", 0) != 0);


	/* Assume application size has useful information about it */
	tmp_wid = app_wid;
	tmp_hgt = app_hgt;

	/* Build a section header for our ideal resolution and current display mode */
	(void)strnfmt(tmp, sizeof(tmp), "%dx%d screen:  Display mode %d, Term-0",
		app_wid, app_hgt, cur_display_mode);

	/* Try to find information in that section */
	i = rd_config_int(fff, tmp, "Visible", 999);

	/* Test failed - we have to use a default resolution */
	if (i == 999)
	{
		/* Search defaults for the closest match to our screen size */
		seek_app_size_defaults(&tmp_wid, &tmp_hgt, app_wid, app_hgt);

		/*
		 * Do not reset the user's screen to this size.
		 * We accept the loss of some prettiness to give the user greater
		 * ability to optimize for his particular layout.  XXX XXX
		 */
	}

	/* Load window prefs using the (possibly adjusted) resolution */
	for (i = 0; i < TERM_MAX + 1; i++)
	{
		(void)strnfmt(tmp, sizeof(tmp), "%dx%d screen:  Display mode %d, Term-%d",
			tmp_wid, tmp_hgt, cur_display_mode, i);

		load_prefs_aux(fff, &window_settings[i], tmp);
	}

	/* Free resources used to store the config file index */
	free_file_index();

	/* Close the file */
	(void)my_fclose(fff);
}



