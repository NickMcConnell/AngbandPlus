#include "lbsound.h"
//#include "langband.h"
#include "lbtools.h"
#include <string.h>
#include <stdlib.h>

#ifdef USE_SDL_MIXER
extern int sdl_init_mixer();
extern int sdl_close_mixer();
extern int sdl_load_sound_effect(const char *fname, int idx);
extern int sdl_load_music_file(const char *fname, int idx);
extern int sdl_halt_music(void);
extern int sdl_halt_sound_effects(short channel);
extern int sdl_play_sound_effect(int idx, float where, short channel, short loops);
extern int sdl_play_music_file(int idx, float where, short loops);
#endif

#ifdef USE_OPENAL
extern int al_init_mixer();
extern int al_close_mixer();
extern int al_load_sound_effect(const char *fname, int idx);
extern int al_load_music_file(const char *fname, int idx);
extern int al_halt_music(void);
extern int al_halt_sound_effects(short channel);
extern int al_play_sound_effect(int idx, float where, short channel, short loops);
extern int al_play_music_file(int idx, float where, short loops);
#endif

#ifdef USE_EXTERNAL_SOUND
extern int lbext_init_mixer();
extern int lbext_close_mixer();
extern int lbext_load_sound_effect(const char *fname, int idx);
extern int lbext_load_music_file(const char *fname, int idx);
extern int lbext_halt_music(void);
extern int lbext_halt_sound_effects(short channel);
extern int lbext_play_sound_effect(int idx, float where, short channel, short loops);
extern int lbext_play_music_file(int idx, float where, short loops);
#endif


/* Sound-related stuff */
int lbui_max_sound_effects = -1;
sound_effect **lbui_sound_effects = NULL;
int lbui_max_music_handles = -1;
sound_effect **lbui_music_handles = NULL;

int lbui_which_soundsystem_used = SOUNDSYSTEM_NONE;
int lbui_use_sound = 0;


int
lbui_current_soundsystem(void) {
    return lbui_which_soundsystem_used;
}

int
lbui_set_soundsystem(SOUNDSYSTEM sys) {
    lbui_which_soundsystem_used = sys;
    return sys;
}

int
lbui_set_sound_status(int val) {
    lbui_use_sound = val;
    return val;
}

int
lbui_get_sound_status(void) {
    return lbui_use_sound;
}

int
lbui_get_max_effects(void) {
    return lbui_max_sound_effects;
}

int
lbui_get_max_music(void) {
    return lbui_max_music_handles;
}


int
lbui_init_sound_system(int size) {

    //DBGPUT("LBUI(%d): init with size %d.\n",  lbui_current_soundsystem(), size);
    
    if (size < 1) {
	ERRORMSG("LBUI(%d): Illegal size %d given for sound-caching.\n",
		 lbui_current_soundsystem(), size);
	return -1;
    }
    else {
//#ifdef USE_SDL
	int i = 0;
	lbui_sound_effects = malloc(size * sizeof(sound_effect*));
	for (i=0; i < size; i++) {
	    lbui_sound_effects[i] = NULL;
	}
	lbui_max_sound_effects = size;

	lbui_music_handles = malloc(size * sizeof(music_handle*));
	for (i=0; i < size; i++) {
	    lbui_music_handles[i] = NULL;
	}
	lbui_max_music_handles = size;
//#endif
	
	// DBGPUT("opened audio.\n");
	return 0;
    }

    
    
}

int
lbui_activate_sound_system(void) {
    int retval = -1;
    
    //DBGPUT("LBUI Sound: Activate %d\n", lbui_current_soundsystem());
    
    if (lbui_get_sound_status()) {

	if (0) { return -1;}
#ifdef USE_OPENAL
	else if (lbui_current_soundsystem() == SOUNDSYSTEM_OPENAL) {
	    retval = al_init_mixer();
	}
#endif
#ifdef USE_SDL_MIXER
	else if (lbui_current_soundsystem() == SOUNDSYSTEM_SDL_MIXER) {
	    retval = sdl_init_mixer();
	}
#endif
#ifdef USE_EXTERNAL_SOUND
	else if (lbui_current_soundsystem() == SOUNDSYSTEM_EXTERNAL) {
	    retval = lbext_init_mixer();
	}
#endif
	if (retval != 0) {
	    // the game can go on, but no sound!
	    lbui_set_sound_status(0);
	}

    }

    return retval;
}

int
lbui_close_sound_system(void) {

    if (lbui_get_sound_status()) {
	if (0) { return -1;}
#ifdef USE_OPENAL
	else if (lbui_current_soundsystem() == SOUNDSYSTEM_OPENAL) {
	    al_close_mixer();
	}
#endif
#ifdef USE_SDL_MIXER
	else if (lbui_current_soundsystem() == SOUNDSYSTEM_SDL_MIXER) {
	    sdl_close_mixer();
	}
#endif
#ifdef USE_EXTERNAL_SOUND
	else if (lbui_current_soundsystem() == SOUNDSYSTEM_EXTERNAL) {
	    lbext_close_mixer();
	}
#endif

    }

    
    return 0;
}

int
find_free_effect_spot() {
    int idx = -1;
    int i = 0;
    for (i=0; i < lbui_max_sound_effects; i++) {
	if (lbui_sound_effects[i] == NULL) {
	    idx = i;
	    break;
	}
	
    }
    if (idx < 0) {
	ERRORMSG("Sound-cache filled already, cannot add more sound-effects.\n");
	return -3;
    }
    
    return idx;
}

int
find_free_music_spot() {
    int idx = -1;
    int i = 0;
    for (i=0; i < lbui_max_music_handles; i++) {
	if (lbui_music_handles[i] == NULL) {
	    idx = i;
	    break;
	}
	
    }
    if (idx < 0) {
	ERRORMSG("Music-cache filled already, cannot add more music-files.\n");
	return -3;
    }
    return idx;
}

int
lbui_load_sound_effect(const char *fname, int idx) {

    //DBGPUT("LBUI(%d): try to load sfx %s at %d\n", lbui_which_soundsystem_used, fname, idx);
    
    if (idx >= lbui_max_sound_effects) {
	ERRORMSG("LBUI(%d): Illegal index %d given for sound-effect %s.\n",
		 lbui_current_soundsystem(), idx, fname);
	return -1;
    }

    if (!fname || strlen(fname) < 2) {
	ERRORMSG("LBUI(%d): The filename given for sound-effect %d is not legal.\n",
		 lbui_current_soundsystem(), idx);
	return -2;
    }

    

    if (0) {}
#ifdef USE_OPENAL
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	idx = al_load_sound_effect(fname, idx);
	if (idx < 0) {
	    ERRORMSG("Langband/OpenAL: Unable (%d) to load soundeffect %s.\n", idx, fname);
	    // handle error
	    return -6;
	}
    }
#endif
#ifdef USE_SDL_MIXER
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	idx = sdl_load_sound_effect(fname, idx);
	if (idx < 0) {
	    ERRORMSG("Langband/SDL-mixer: Unable (%d) to load soundeffect %s.\n", idx, fname);
	    // handle error
	    
	    return -6;
	}
    }
#endif
#ifdef USE_EXTERNAL_SOUND
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_EXTERNAL) {
	idx = lbext_load_sound_effect(fname, idx);
	if (idx < 0) {
	    ERRORMSG("Langband/external: Unable (%d) to load soundeffect %s.\n", idx, fname);
	    // handle error
	    return -6;
	}

    }
#endif
    //DBGPUT("LBUI(%d): return load sfx %s at %d\n", lbui_which_soundsystem_used, fname, idx);
    
    return idx;
}

int
lbui_load_music_file(const char *fname, int idx) {
    
    if (idx >= lbui_max_music_handles) {
	ERRORMSG("LBUI (%d): Illegal index %d given for music-handle %s.\n",
		 lbui_current_soundsystem(), idx, fname);
	return -1;
    }
    // we should add it to the list
    
    if (!fname || strlen(fname) < 2) {
	ERRORMSG("LBUI(%d): The filename given for music-file %d is not legal.\n",
		 lbui_current_soundsystem(), idx);
	return -2;
    }
    

    if (0) {}
#ifdef USE_OPENAL
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	idx = al_load_music_file(fname, idx);
	if (idx < 0) {
	    ERRORMSG("Langband/OpenAL: Unable (%d) to load musicfile %s.\n", idx, fname);
	    
	    return -6;
	}
    }
#endif
#ifdef USE_SDL_MIXER
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	idx = sdl_load_music_file(fname, idx);
	if (idx < 0) {
	    ERRORMSG("Langband/SDL-mixer: Unable (%d) to load musicfile %s.\n", idx, fname);
	    return -6;
	}
    }
#endif
#ifdef USE_EXTERNAL_SOUND
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_EXTERNAL) {
	idx = lbext_load_music_file(fname, idx);
	if (idx < 0) {
	    ERRORMSG("Langband/external: Unable (%d) to load musicfile %s.\n", idx, fname);
	    return -6;
	}
    }
#endif
   
    //INFOMSG("Installed music %s in %d\n", fname, idx);
    return idx;
}

int
lbui_play_sound_effect(int sound_idx, short channel, short loops) {

    //INFOMSG("play effect %d to %d\n", sound_idx, which_soundsystem_used);
    float where = 0.0;

    if (!lbui_use_sound) {
	return -1;
    }
    
    if (sound_idx < 0 || sound_idx >= lbui_max_sound_effects) { 
	ERRORMSG("LBUI(%d): Invalid sound-index %d provided for sound-effect.\n",
		 lbui_current_soundsystem(), sound_idx);
	return -12;
    }
    
    if (0) { return -1;}
#ifdef USE_OPENAL
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	return al_play_sound_effect(sound_idx, where, channel, loops);
    }
#endif
#ifdef USE_SDL_MIXER
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	return sdl_play_sound_effect(sound_idx, where, channel, loops);
    }
#endif
#ifdef USE_EXTERNAL_SOUND
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_EXTERNAL) {
	return lbext_play_sound_effect(sound_idx, where, channel, loops);
    }
#endif
    else {
	where = 0.0; // to avoid warning
	channel = loops; // to avoid warning
	return -1;
    }
    
}

int
lbui_play_music_file(int sound_idx, short loops) {
    
    float where = 0.0;
    
    if (!lbui_use_sound) {
	return -1;
    }
    
    if (sound_idx < 0 || sound_idx >= lbui_max_music_handles) {
	ERRORMSG("Invalid sound-index %d provided for music file\n.", sound_idx);
	return -12;
    }

    //DBGPUT("LBUI(%d): Play music %d.\n", lbui_current_soundsystem(), sound_idx);
    
    if (0) { return -1;}
#ifdef USE_OPENAL
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	return al_play_music_file(sound_idx, where, loops);
    }
#endif
#ifdef USE_SDL_MIXER
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	return sdl_play_music_file(sound_idx, where, loops);
    }
#endif
#ifdef USE_EXTERNAL_SOUND
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_EXTERNAL) {
	return lbext_play_music_file(sound_idx, where, loops);
    }
#endif
    else {
	where = 0.0; // to avoid warning
	loops = 0; // to avoid warnings
	return -1;
    }
    
}

int
lbui_halt_sound_effects(short channel) {
    
    if (!lbui_use_sound) {
	return -1;
    }

    //DBGPUT("Halt effects at channel %d.\n", channel);
    
    if (0) { return -1;}
#ifdef USE_OPENAL
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	return al_halt_sound_effects(channel);
    }
#endif
#ifdef USE_SDL_MIXER
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	return sdl_halt_sound_effects(channel);
    }
#endif
#ifdef USE_EXTERNAL_SOUND
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_EXTERNAL) {
	return lbext_halt_sound_effects(channel);
    }
#endif

    else {
	channel = 0; // to avoid warning
	return -1;
    }
    
}


int
lbui_halt_music(void) {
    
    if (!lbui_use_sound) {
	return -1;
    }

    //DBGPUT("Halt music.\n");
    
    if (0) { return -1;}
#ifdef USE_OPENAL
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_OPENAL) {
	return al_halt_music();
    }
#endif
#ifdef USE_SDL_MIXER
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_SDL_MIXER) {
	return sdl_halt_music();
    }
#endif
#ifdef USE_EXTERNAL_SOUND
    else if (lbui_which_soundsystem_used == SOUNDSYSTEM_EXTERNAL) {
	return lbext_halt_music();
    }
#endif
    else {
	return -1;
    }
    
}
