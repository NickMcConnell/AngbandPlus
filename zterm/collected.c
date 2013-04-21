/*
 * DESC: collected.h - a kitchen sink of all non-system specific C-code
 * Copyright (c) 2000-2002 - Stig Erik Sandø

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "autoconf.h"
#include "langband.h"
#include "lbwindows.h"
#include "lbsound.h"
#include "lbtools.h"

#if defined(USE_GCU)
int lbui_init_gcu(int flags);
#endif

#if defined(USE_SDL)
int lbui_init_sdl(int flags);
#endif

extern int lbui_cleanup_callbacks();
extern int lbui_cleanup_frame_system();

/** defaults to false, since we don't need callbacks anymore */
int lbui_will_use_callback = 0;

/** set default illegal value. */
LISP_SYSTEMS lbui_current_lisp_system = LISPSYS_BAD;

/** the base path for source files */
const char *lbui_base_source_path = "./";
/** the base path for config files */
const char *lbui_base_config_path = "./config/";
/** the base path for gfx files */
const char *lbui_base_data_path = "./data/";

int lbui_which_ui_used = -1;

int lbui_current_ui() { return lbui_which_ui_used; }

int
lbui_init_c_side(const char *ui, const char *sourcePath, const char *configPath,
		 const char *dataPath, int extra_flags) {

    int possible_to_go_X = 0;
    int default_mode = 0; // 0 is x, 1 is gcu, 2 is gtk.. hackish
    int wanted_ui = 0;
    int graphical = (extra_flags & LANGBAND_GRAPHICS);
    int init_retval = -666;
    

//#ifdef USE_SDL
    lbui_set_sound_status(extra_flags & LANGBAND_SOUND);
//#else
//    lbui_set_sound_status(0);
//#endif

    if (lbui_get_sound_status()) {
#ifdef USE_SDL_MIXER
	lbui_set_soundsystem(SOUNDSYSTEM_SDL_MIXER);
#endif
	
#ifdef USE_OPENAL
	lbui_set_soundsystem(SOUNDSYSTEM_OPENAL);
#endif
#ifdef USE_EXTERNAL_SOUND
	lbui_set_soundsystem(SOUNDSYSTEM_EXTERNAL);
#endif
    }
    
    if (!ui) {
	ui = "SDL"; // default
    }

    if (sourcePath && (strlen(sourcePath)>0)) {
	char *str = malloc(strlen(sourcePath) +2);
	strcpy(str, sourcePath);
	lbui_base_source_path = str;
    }
    
    if (configPath && (strlen(configPath)>0)) {
	char *str = malloc(strlen(configPath) +2);
	strcpy(str, configPath);
	lbui_base_config_path = str;
    }
    if (dataPath && (strlen(dataPath)>0)) {
	char *str = malloc(strlen(dataPath) +2);
	strcpy(str, dataPath);
	lbui_base_data_path = str;
    }

    /* verify that we have decent value */
    if (!strcmp(ui, "DEFAULT") ||
	!strcmp(ui, "default")) {
	wanted_ui = default_mode;
    }
    else if (!strcmp(ui, "X11") ||
	     !strcmp(ui, "x11") ||
	     !strcmp(ui, "X")) {
	wanted_ui = UITYPE_X11;
    }
    else if (!strcmp(ui, "gcu") ||
	     !strcmp(ui, "curses") ||
	     !strcmp(ui, "GCU")) {
	wanted_ui = UITYPE_GCU;
	graphical = 0; // override
    }
    else if (!strcmp(ui, "gtk") ||
	     !strcmp(ui, "gtk+") ||
	     !strcmp(ui, "GTK")) {
	wanted_ui = UITYPE_GTK;
	graphical = 0; // override
    }
    else if (!strcmp(ui, "win") ||
	     !strcmp(ui, "Win") ||
	     !strcmp(ui, "WIN")) {
	wanted_ui = UITYPE_WIN;
	graphical = 0; // override
    }
    else if (!strcmp(ui, "sdl") ||
	     !strcmp(ui, "SDL")) {
	wanted_ui = UITYPE_SDL;
	graphical = 1; // override
    }

				     
    else {
	ERRORMSG("Unable to find compatible UI with spec '%s'\n", ui);
	return -1;
    }

    if (wanted_ui >= UITYPE_X11 && wanted_ui <= UITYPE_SDL) {

    }
    else {
	ERRORMSG("The UI-value is set to an illegal value: %d\n", wanted_ui);
	return -2;
    }

#ifdef USE_GCU
//    ERRORMSG("Yes, we have compiled in gcu.. \n");
#endif


    /* let us check if we can go X */
    
#if defined(USE_GTK) || defined(USE_X11)    
    {
	char *val = getenv("DISPLAY");
	if (val && strlen(val)) {
	    possible_to_go_X = 1;
	}
    }
#endif
    

    if (1==0) { }

#if defined (USE_SDL)
    else if (wanted_ui == UITYPE_SDL) {
	lbui_which_ui_used = UITYPE_SDL;
	init_retval = lbui_init_sdl(extra_flags);
    }
#endif
    
#if defined(USE_X11)
    else if (possible_to_go_X && wanted_ui == UITYPE_X11) {
	lbui_which_ui_used = UITYPE_X11;
	init_retval = init_x11(argc,argv); /* proper value set */
    }
#endif
    
#if defined(USE_GTK)
    /* a fallback if X11 doesn't exist */
    else if (possible_to_go_X && (wanted_ui == UITYPE_GTK || wanted_ui == UITYPE_X11)) {
	lbui_which_ui_used = UITYPE_GTK;
	init_retval = init_gtk(argc,argv); /* proper value set */
    }
#endif
    
#if defined (USE_GCU)
    else if (wanted_ui == UITYPE_GCU) {
	lbui_which_ui_used = UITYPE_GCU;
	init_retval = lbui_init_gcu(extra_flags);
    }
#endif

    else {
	if (!possible_to_go_X && (wanted_ui == UITYPE_X11 || wanted_ui == UITYPE_GTK)) {
	    ERRORMSG("Wanted an X-dependent UI, but unable to find X (no DISPLAY env).\n");
	}
	else {
	    ERRORMSG("Unable to find a suitable UI to use [%s,%d].\n", ui, wanted_ui);
	}
	return -10 - wanted_ui;
    }

    if (init_retval != 0) {
	ERRORMSG("Init of UI screwed up miserably (retval = %d), exiting.\n", init_retval);
	return init_retval;
    }

    /* Verify main term */
    if (lbui_has_frame(0, ACTIVE) == 0) {
	ERRORMSG("main window does not exist\n");
	return -2;
    }

    /* Make sure main term is active */
    //Term_activate(activeFrames[0]->azt);

    /* Verify minimum size */
    /* FIX!!! */

#if defined(USE_X11) || defined(USE_GCU) || defined(USE_SDL)
    //DBGPUT("lisp-sys %d, callback %d\n", lbui_current_lisp_system, lbui_will_use_callback);
    if (lbui_will_use_callback) {
	// this is a callback
	//DBGPUT("going back");
	return lbui_play_game_lisp();
    }
#endif
    
    return -1; // never really started the ball.
}


int
lbui_cleanup_c_side(void) {

    int cur_ui = lbui_current_ui();
    int retval = -1;
    
    lbui_cleanup_callbacks();
    lbui_cleanup_frame_system();
    lbui_close_sound_system();

    if (0) { }
    
#ifdef USE_X11
    else if (cur_ui == UITYPE_X11) {
	retval = cleanup_X11();
    }
#endif

#ifdef USE_GCU
    else if (cur_ui == UITYPE_GCU) {
	retval = gcu_cleanup();
    }
#endif

#ifdef USE_SDL
    else if (cur_ui == UITYPE_SDL) {
	retval = sdl_cleanup();
    }
#endif

    lbui_current_lisp_system = LISPSYS_BAD;
    lbui_which_ui_used = -1;
    lbui_set_sound_status(0);

    return retval;
}

int
lbui_load_gfx_image(const char *fname, int idx, unsigned int transcolour) {

    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_load_gfx_image(fname, idx, transcolour);
    }
#endif
    else {
	return -1;
    }
    
}

int
lbui_load_texture(int idx, const char *filename, int target_width, int target_height, int alpha) {
    
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_load_texture(idx, filename, target_width, target_height, alpha);
    }
#endif /*sdl */
    else {
	return -1;
    }
}

int
lbui_listen_for_event(int option) {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_get_event(option);
    }
#endif /* sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return gcu_get_event(option);
    }
#endif /* gcu */
    else {
	return -1;
    }
}

int
lbui_get_image_width(int idx) {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_get_image_width(idx);
    }
#endif /*sdl */
    else {
	return -1;
    }
}

int
lbui_get_image_height(int idx) {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_get_image_height(idx);
    }
#endif /*sdl */
    else {
	return -1;
    }
}

int
lbui_get_window_width() {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_get_window_width();
    }
#endif /*sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return gcu_get_window_width();
    }
#endif /* gcu */
    else {
	return -1;
    }
}

int
lbui_get_window_height() {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_get_window_height();
    }
#endif /*sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return gcu_get_window_height();
    }
#endif /* gcu */
    else {
	return -1;
    }
}

int
lbui_full_blit(short win_num, short x, short y, unsigned int img, short flags) {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_full_blit(win_num, x, y, img, flags);
    }
#endif /*sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return gcu_full_blit(win_num, x, y, img, flags);
    }
#endif /* gcu */
    else {
	return -1;
    }
}

int
lbui_transparent_blit(short win_num, short x, short y, unsigned int img, short flags) {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_transparent_blit(win_num, x, y, img, flags);
    }
#endif /*sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return gcu_transparent_blit(win_num, x, y, img, flags);
    }
#endif /* gcu */
    else {
	return -1;
    }
}

int
lbui_clear_coords(short win_num, short x, short y, short w, short h) {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_clear_coords(win_num, x, y, w, h);
    }
#endif /*sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return gcu_clear_coords(win_num, x, y, w, h);
    }
#endif /* gcu */
    else {
	return -1;
    }
}

int
lbui_flush_coords(short win_num, short x, short y, short w, short h) {
    if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_flush_coords(win_num, x, y, w, h);
    }
#endif /*sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return gcu_flush_coords(win_num, x, y, w, h);
    }
#endif /* gcu */
    else {
	return -1;
    }
}

int
lbui_recalculate_frame_placements(int arg) {
   if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	return sdl_recalculate_frame_placements(arg);
    }
#endif /*sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return gcu_recalculate_frame_placements(arg);
    }
#endif /* gcu */
    else {
	return -1;
    }

}

int
lbui_install_font_in_frame(int win_num, const char *font, int ptsize, int style) {
    
    int install_retval = 0;

    //DBGPUT("Adding to frame\n");

    install_retval = lbui_add_frame_fontinfo(win_num, font, ptsize, style);

    //DBGPUT("Done add frame\n");
	
    if (install_retval) {
	DBGPUT("Returning from add_frame with %d from %d\n", install_retval, win_num);
	return install_retval;
    }
    
   if (0) { return -1; }
#ifdef USE_SDL
    else if (lbui_which_ui_used == UITYPE_SDL) {
	LangbandFrame *lf = lbui_predefinedFrames[win_num];
	if (lf) {
	    lf = sdl_install_font_in_frame(lf);
	}
	if (lf)
	    return 0;
	else
	    return -3;
    }
#endif /*sdl */
#ifdef USE_GCU
    else if (lbui_which_ui_used == UITYPE_GCU) {
	return 0;
    }
#endif /* gcu */
    else {
	return -1;
    }

}
