#ifndef LB_SOUND_H
#define LB_SOUND_H

/*
 * DESC: lbsound.h - the sound system
 * Copyright (c) 2003 - Stig Erik Sandø

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "autoconf.h"

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
 * Hack -- Define NULL
 */
#ifndef NULL
# ifdef __STDC__
#  define NULL ((void*)0)
# else
#  define NULL ((char*)0)
# endif /* __STDC__ */
#endif /* NULL */

typedef enum {
    SOUNDSYSTEM_NONE      = 0,
    SOUNDSYSTEM_SDL_MIXER = 1,
    SOUNDSYSTEM_OPENAL    = 2,
    SOUNDSYSTEM_EXTERNAL  = 3
} SOUNDSYSTEM;

typedef struct sound_handle sound_effect;
typedef struct sound_handle music_handle;

struct sound_handle {
    char *filename;
    int buffer_idx;
    void *handle; // probably a Mix_Chunk* for SDL
};

extern sound_effect **lbui_sound_effects;
extern music_handle **lbui_music_handles;



INTERFACE int lbui_init_sound_system(int size);
INTERFACE int lbui_activate_sound_system();
INTERFACE int lbui_close_sound_system();

INTERFACE int lbui_get_sound_status(void);
INTERFACE int lbui_load_sound_effect(const char *fname, int idx);
INTERFACE int lbui_play_sound_effect(int sound_idx, short channel, short loops);
INTERFACE int lbui_halt_sound_effects(short channel);
INTERFACE int lbui_load_music_file(const char *fname, int idx);
INTERFACE int lbui_play_music_file(int sound_idx, short loops);
INTERFACE int lbui_halt_music(void);

extern int lbui_set_soundsystem(SOUNDSYSTEM sys);
extern int lbui_current_soundsystem(void);
extern int lbui_set_sound_status(int val);
extern int lbui_get_sound_status(void);
extern int find_free_effect_spot(void);
extern int find_free_music_spot(void);
extern int lbui_get_max_effects(void);
extern int lbui_get_max_music(void);



#endif /* lb_windows_h */
