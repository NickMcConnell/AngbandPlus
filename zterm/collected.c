/*
 * DESC: collected.h - a kitchen sink of all non-system specific C-code
 * Copyright (c) 2000-2002 - Stig Erik Sandø

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "langband.h"
#include "lbwindows.h"

#if defined(USE_GTK)
int init_gtk(int argc, char **argv);
#endif

#if defined(USE_X11)
int init_x11(int argc, char **argv);
#endif

#if defined(USE_GCU)
int init_gcu(int argc, char **argv);
#endif

#if defined(USE_SDL)
int init_sdl(int flags);
#endif


extern int cleanup_callbacks();
extern int cleanup_frame_system();

/** defaults to true as cmucl is default */
int lisp_will_use_callback = 0;

/** set default illegal value. */
LISP_SYSTEMS current_lisp_system = LISPSYS_BAD;

/** the base path for source files */
const char *base_source_path = "./";
/** the base path for config files */
const char *base_config_path = "./config/";
/** the base path for gfx files */
const char *base_data_path = "./data/";

int which_ui_used = -1;
int which_soundsystem_used = SOUNDSYSTEM_NONE;
int use_sound = 0;

/* Sound-related stuff */
int max_sound_effects = -1;
sound_effect **sound_effects = NULL;
int max_music_handles = -1;
sound_effect **music_handles = NULL;


int current_ui() { return which_ui_used; }
int current_soundsystem() { return which_soundsystem_used; }

int
init_c_side(const char *ui, const char *sourcePath, const char *configPath,
	    const char *dataPath, int extra_flags) {

    int possible_to_go_X = 0;
    int default_mode = 0; // 0 is x, 1 is gcu, 2 is gtk.. hackish
    int wanted_ui = 0;
    int graphical = (extra_flags & LANGBAND_GRAPHICS);
    int init_retval = -666;
    

#ifdef USE_SDL
    use_sound = (extra_flags & LANGBAND_SOUND);
#else
    use_sound = FALSE;
#endif

    if (use_sound) {
#ifdef USE_SDL_MIXER
	which_soundsystem_used = SOUNDSYSTEM_SDL_MIXER;
#endif
	
#ifdef USE_OPENAL
	which_soundsystem_used = SOUNDSYSTEM_OPENAL;
#endif
    }
    
    if (!ui) {
	ui = "SDL"; // default
    }

    if (sourcePath && (strlen(sourcePath)>0)) {
	char *str = malloc(strlen(sourcePath) +2);
	strcpy(str, sourcePath);
	base_source_path = str;
    }
    
    if (configPath && (strlen(configPath)>0)) {
	char *str = malloc(strlen(configPath) +2);
	strcpy(str, configPath);
	base_config_path = str;
    }
    if (dataPath && (strlen(dataPath)>0)) {
	char *str = malloc(strlen(dataPath) +2);
	strcpy(str, dataPath);
	base_data_path = str;
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
	which_ui_used = UITYPE_SDL;
	init_retval = init_sdl(extra_flags);
    }
#endif
    
#if defined(USE_X11)
    else if (possible_to_go_X && wanted_ui == UITYPE_X11) {
	which_ui_used = UITYPE_X11;
	init_retval = init_x11(argc,argv); /* proper value set */
    }
#endif
    
#if defined(USE_GTK)
    /* a fallback if X11 doesn't exist */
    else if (possible_to_go_X && (wanted_ui == UITYPE_GTK || wanted_ui == UITYPE_X11)) {
	which_ui_used = UITYPE_GTK;
	init_retval = init_gtk(argc,argv); /* proper value set */
    }
#endif
    
#if defined (USE_GCU)
    else if (wanted_ui == UITYPE_GCU || !possible_to_go_X) {
	which_ui_used = UITYPE_GCU;
	init_retval = init_gcu(argc, argv);
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
    if (has_frame(0, ACTIVE) == 0) {
	ERRORMSG("main window does not exist\n");
	return -2;
    }

    /* Make sure main term is active */
    //Term_activate(activeFrames[0]->azt);

    /* Verify minimum size */
    /* FIX!!! */

#if defined(USE_X11) || defined(USE_GCU) || defined(USE_SDL)
    //DBGPUT("lisp-sys %d, callback %d\n", current_lisp_system, lisp_will_use_callback);
    if (lisp_will_use_callback) {
	// this is a callback
	//DBGPUT("going back");
	return play_game_lisp();
    }
#endif
    
    return -1; // never really started the ball.
}

int
cleanup_c_side(void) {

    int cur_ui = current_ui();
    int retval = -1;
    
    cleanup_callbacks();
    cleanup_frame_system();

    if (0) { }
    
#ifdef USE_X11
    else if (cur_ui == UITYPE_X11) {
	retval = cleanup_X11();
    }
#endif

#ifdef USE_GCU
    else if (cur_ui == UITYPE_GCU) {
	retval = cleanup_GCU();
    }
#endif

#ifdef USE_SDL
    else if (cur_ui == UITYPE_SDL) {
	retval = cleanup_SDL();
    }
#endif

    current_lisp_system = LISPSYS_BAD;
    which_ui_used = -1;
    use_sound = 0;
    
    return retval;
}

int
get_sound_status() {
    return use_sound;
}



#ifdef USE_SDL_MIXER
extern int
sdl_load_sound_effect(const char *fname, sound_effect *handle);
extern int
sdl_load_music_file(const char *fname, music_handle *handle);
#endif

#ifdef USE_OPENAL
extern int
al_load_sound_effect(const char *fname, sound_effect *handle);
extern int
al_load_music_file(const char *fname, music_handle *handle);
#endif

int
init_sound_system(int size) {

    if (size < 1) {
	ERRORMSG("Illegal size %d given for sound-caching.\n", size);
	return -1;
    }
    else {
#ifdef USE_SDL
	int i = 0;
	sound_effects = malloc(size * sizeof(sound_effect*));
	for (i=0; i < size; i++) {
	    sound_effects[i] = NULL;
	}
	max_sound_effects = size;

	music_handles = malloc(size * sizeof(music_handle*));
	for (i=0; i < size; i++) {
	    music_handles[i] = NULL;
	}
	max_music_handles = size;
#endif
	return 0;
    }
    
}

int
load_sound_effect(const char *fname, int idx) {

    char *ptr = NULL;
    sound_effect *sb = NULL;
    int retval = -20;
    
    if (idx >= max_sound_effects) {
	ERRORMSG("Illegal index %d given for sound-effect %s.\n", idx, fname);
	return -1;
    }
    // we should add it to the list
    if (idx < 0) {
	int i = 0;
	for (i=0; i < max_sound_effects; i++) {
	    if (sound_effects[i] == NULL) {
		idx = i;
		break;
	    }
		
	}
	if (idx < 0) {
	    ERRORMSG("Sound-cache filled already, cannot add more sound-effects.\n");
	    return -3;
	}
    }

    if (!fname || strlen(fname) < 2) {
	ERRORMSG("The filename given for sound-effect %d is not legal.\n", idx);
	return -2;
    }

    sb = malloc(sizeof(sound_effect));

    if (FALSE) {}
#ifdef USE_OPENAL
    else if (which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	retval = al_load_sound_effect(fname, sb);
	if (retval != 0) {
	    ERRORMSG("Langband/OpenAL: Unable (%d) to load soundeffect %s.\n", retval, fname);
	    // handle error
	    free(sb);
	    return -6;
	}
    }
#endif
#ifdef USE_SDL_MIXER
    else if (which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	retval = sdl_load_sound_effect(fname, sb);
	if (!sb->handle) {
	    ERRORMSG("Langband/SDL-mixer: Unable (%d) to load soundeffect %s.\n", retval, fname);
	    // handle error
	    free(sb);
	    return -6;
	}
    }
#endif
    
    ptr = malloc(strlen(fname)+1);
    strcpy(ptr, fname);
    
    sb->filename = ptr;

    if (sound_effects[idx]) {
	free(sound_effects[idx]);
	sound_effects[idx] = NULL;
    }
    
    sound_effects[idx] = sb;

    return idx;
}

int
load_music_file(const char *fname, int idx) {

    char *ptr = NULL;
    music_handle *sb = NULL;
    int retval = -20;
    
    if (idx >= max_music_handles) {
	ERRORMSG("Illegal index %d given for music-handle %s.\n", idx, fname);
	return -1;
    }
    // we should add it to the list
    if (idx < 0) {
	int i = 0;
	for (i=0; i < max_music_handles; i++) {
	    if (music_handles[i] == NULL) {
		idx = i;
		break;
	    }
		
	}
	if (idx < 0) {
	    ERRORMSG("Music-cache filled already, cannot add more music-files.\n");
	    return -3;
	}
    }

    if (!fname || strlen(fname) < 2) {
	ERRORMSG("The filename given for music-file %d is not legal.\n", idx);
	return -2;
    }

    sb = malloc(sizeof(music_handle));

    if (FALSE) {}
#ifdef USE_OPENAL
    else if (which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	retval = al_load_music_file(fname, sb);
	if (retval != 0) {
	    ERRORMSG("Langband/OpenAL: Unable (%d) to load musicfile %s.\n", retval, fname);
	    // handle error
	    free(sb);
	    return -6;
	}
    }
#endif
#ifdef USE_SDL_MIXER
    else if (which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	retval = sdl_load_music_file(fname, sb);
	if (!sb->handle) {
	    ERRORMSG("Langband/SDL-mixer: Unable (%d) to load musicfile %s.\n", retval, fname);
	    // handle error
	    free(sb);
	    return -6;
	}
    }
#endif

    ptr = malloc(strlen(fname)+1);
    strcpy(ptr, fname);
    
    sb->filename = ptr;

    if (music_handles[idx]) {
	free(music_handles[idx]);
	music_handles[idx] = NULL;
    }
    
    music_handles[idx] = sb;
    //INFOMSG("Installed music %s in %d\n", fname, idx);
    return idx;
}

#ifdef USE_SDL_MIXER
extern int sdl_play_sound_effect(int idx, float where);
extern int sdl_play_music_file(int idx, float where);
#endif

#ifdef USE_OPENAL
extern int al_play_sound_effect(int idx, float where);
extern int al_play_music_file(int idx, float where);
#endif


int
play_sound_effect(int sound_idx) {

    float where = 0.0;
    //INFOMSG("play effect %d to %d\n", sound_idx, which_soundsystem_used);

    if (!use_sound) {
	return -1;
    }
    
    if (sound_idx < 0 || sound_idx >= max_sound_effects || !sound_effects[sound_idx]) {
	ERRORMSG("Invalid sound-index %d provided for sound-effect\n.", sound_idx);
	return -12;
    }
    
    if (FALSE) { return -1;}
#ifdef USE_OPENAL
    else if (which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	return al_play_sound_effect(sound_idx, where);
    }
#endif
#ifdef USE_SDL_MIXER
    else if (which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	return sdl_play_sound_effect(sound_idx, where);
    }
#endif
    else {
	return -1;
    }
    
}

int
play_music_file(int sound_idx) {
    
    float where = 0.0;
    
    if (!use_sound) {
	return -1;
    }
    
    if (sound_idx < 0 || sound_idx >= max_music_handles || !music_handles[sound_idx]) {
	ERRORMSG("Invalid sound-index %d provided for music file\n.", sound_idx);
	return -12;
    }

    DBGPUT("Play music %d.\n", sound_idx);
    
    if (FALSE) { return -1;}
#ifdef USE_OPENAL
    else if (which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	return al_play_music_file(sound_idx, where);
    }
#endif
#ifdef USE_SDL_MIXER
    else if (which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	return sdl_play_music_file(sound_idx, where);
    }
#endif
    else {
	return -1;
    }
    
}

int
load_gfx_image(const char *fname, int idx, unsigned int transcolour) {

    if (FALSE) { return -1; }
#ifdef USE_SDL
    else if (which_ui_used == UITYPE_SDL) {
	return sdl_load_gfx_image(fname, idx, transcolour);
    }
#endif
    else {
	return -1;
    }
    
}

void
lb_format(FILE *ofile, int priority, const char *fmt, ...) {
    if (!ofile) {

    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	if (priority)
	    fflush(ofile);
    }
}

FILE *dbgout = NULL;

#if defined(USE_GCU) || defined(WIN32)
static void
open_dbg_file () {
    dbgout = fopen("dbgout.txt","a");
    fprintf(dbgout, "---------------------------------------\n");
}
#endif

#ifdef DEBUG
void
DBGPUT(const char *fmt, ...) {
    FILE *ofile = NULL;

#if defined(USE_GCU) || defined(WIN32)
    if (!dbgout) open_dbg_file();
    ofile = dbgout;
#else
    ofile = stderr;
#endif

    if (!ofile) {
	    
    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	fflush(ofile);
    }
}
#endif /* debug */

void
ERRORMSG(const char *fmt, ...) {

    FILE *ofile = NULL;

#if defined(USE_GCU) || defined(WIN32)
    if (!dbgout) open_dbg_file();
    ofile = dbgout;
#else
    ofile = stderr;
#endif
    
    if (!ofile) {
	    
    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	fflush(ofile);
    }

}

int
listenForEvent(int option) {
    return sdl_getEvent(option);
}
