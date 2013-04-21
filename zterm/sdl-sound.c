#include "langband.h"

#ifdef USE_SDL

#include "SDL_mixer.h"

int
sdl_init_mixer() {
	
    int audio_rate = 22050;
    Uint16 audio_format = AUDIO_S16; 
    int audio_channels = 2;
    int audio_buffers = 2048;
    
    
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
	
	//exit(1);
	return -1;
    }
    else {
	DBGPUT("We managed to init the sound-system, good!\n");
	Mix_VolumeMusic(MIX_MAX_VOLUME);
	return 0;
    }
    
}

int
sdl_close_mixer() {
    Mix_CloseAudio();
    return 0;
}

int
sdl_load_sound_effect(const char *fname, sound_effect *handle) {

    handle->handle = Mix_LoadWAV(fname);

    if (!handle->handle) {
	ERRORMSG("Mix_LoadWAV: Error '%s' when loading %s\n", Mix_GetError(), fname);
	return -1;
    }

    
    return 0;
}

int
sdl_load_music_file(const char *fname, music_handle *handle) {

    handle->handle = Mix_LoadMUS(fname);

    if (!handle->handle) {
	ERRORMSG("Mix_LoadMUS: Error '%s' when loading %s\n", Mix_GetError(), fname);
	return -1;
    }

    return 0;
}

int
sdl_play_sound_effect(int num, float placement) {
    if (use_sound) {
	int soundChannel;
	Mix_Chunk *ptr = NULL;
	//DBGPUT("Playing sound %d\n", num);
	ptr = (Mix_Chunk *)sound_effects[num]->handle;
	soundChannel = Mix_PlayChannel(-1, ptr, 0);
	return 0;
    }
    else {
	return -1;
    }
}

int
sdl_play_music_file(int num, float placement) {
    if (use_sound) {
	int soundChannel;
	Mix_Music *ptr = NULL;
	//DBGPUT("Playing sound %d\n", num);
	ptr = (Mix_Music *)music_handles[num]->handle;
	soundChannel = Mix_PlayMusic(ptr, 0);
	return 0;
    }
    else {
	return -1;
    }
}

#endif /* use_sdl */
