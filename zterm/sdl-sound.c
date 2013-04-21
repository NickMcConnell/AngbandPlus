#include "langband.h"
#include "lbsound.h"
#include "lbtools.h"

#ifdef USE_SDL_MIXER

#include "SDL_mixer.h"

int
sdl_init_mixer() {

    // int audio_rate = 22050;
    int audio_rate = 44100;
    // Uint16 audio_format = AUDIO_S16;
    Uint16 audio_format = MIX_DEFAULT_FORMAT;
    int audio_channels = 2;
    int audio_buffers = 1024; //2048;
    
    
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
	Mix_Volume(-1, MIX_MAX_VOLUME);
	Mix_VolumeMusic(MIX_MAX_VOLUME);

	Mix_AllocateChannels(4); // 2 and 3 are for random sounds
	Mix_ReserveChannels(2); // 0 and 1 is controllable by lisp
	
	DBGPUT("We have %d sound-channels.\n", Mix_AllocateChannels(-1));
	return 0;
    }
    
}

int
sdl_close_mixer() {
    Mix_HaltMusic();
    Mix_HaltChannel(-1);
    Mix_CloseAudio();
    return 0;
}

int
sdl_load_sound_effect(const char *fname, int idx) {
    int real_idx = idx;
    sound_effect *handle = NULL;

    if (real_idx < 0) {
	real_idx = find_free_effect_spot();
    }
    if (real_idx < 0) {
	ERRORMSG("No free effect-spots.\n");
	return -3;
    }

    if (lbui_sound_effects[real_idx]) {
	free(lbui_sound_effects[real_idx]);
	lbui_sound_effects[real_idx] = NULL;
    }

    
    handle = malloc(sizeof(sound_effect));

    handle->handle = Mix_LoadWAV(fname);

    if (!handle->handle) {
	ERRORMSG("Mix_LoadWAV: Error '%s' when loading %s\n", Mix_GetError(), fname);
	free(handle);
	return -1;
    }
    else {
	char *ptr = malloc(strlen(fname)+1);
	strcpy(ptr, fname);
	handle->filename = ptr;
    
	lbui_sound_effects[real_idx] = handle;
	return real_idx;
    }
}

int
sdl_load_music_file(const char *fname, int idx) {
    char *ptr = NULL;
    music_handle *handle = NULL;
    int real_idx = idx;
    
    if (real_idx < 0) {
	real_idx = find_free_music_spot();
    }
    if (real_idx < 0) {
	ERRORMSG("No free music-spots.\n");
	return -3;
    }
    
    if (lbui_music_handles[real_idx]) {
	free(lbui_music_handles[real_idx]);
	lbui_music_handles[real_idx] = NULL;
    }

    INFOMSG("Loading music-file '%s'\n", fname);
    
    handle = malloc(sizeof(music_handle));
    
    handle->handle = Mix_LoadMUS(fname);

    if (!handle->handle) {
	ERRORMSG("Mix_LoadMUS: Error '%s' when loading %s\n", Mix_GetError(), fname);
	free(handle);
	return -1;
    }

    ptr = malloc(strlen(fname)+1);
    strcpy(ptr, fname);
    
    handle->filename = ptr;
    
    lbui_music_handles[real_idx] = handle;
    
    return real_idx;
}

static void
channelDone(int channel) {

    //DBGPUT("Channel %d finished.\n", channel);
}
    

int
sdl_play_sound_effect(int num, float placement, short channel, short loops) {
    if (!lbui_sound_effects[num]) {
	return -6;
    }
    if (lbui_get_sound_status()) {
	int soundChannel;
	Mix_Chunk *ptr = NULL;
	INFOMSG("Playing sound %d\n", num);
	//Mix_ChannelFinished(channelDone);
	ptr = (Mix_Chunk *)lbui_sound_effects[num]->handle;
	soundChannel = Mix_PlayChannel(channel, ptr, loops);
	return 0;
    }
    else {
	return -1;
    }
}

int
sdl_halt_sound_effects(short channel) {
    //DBGPUT("Halt channel %d.\n", channel);
    //Mix_HaltChannel(channel);
    Mix_FadeOutChannel(channel, 75);
    return 0;
}

static Mix_Music *music_ptr = NULL;

static void
musicFinished() {
    
    //DBGPUT("Music stopped.\n");
    
    //Mix_SetMusicPosition(0);
    //Mix_RewindMusic();
    Mix_PlayMusic(music_ptr, 0);
    //DBGPUT("Music restarted for a new spin.\n");
}


int
sdl_play_music_file(int num, float placement, short loops) {
    if (!lbui_music_handles[num]) {
	return -6;
    }
    if (lbui_get_sound_status()) {
	int soundChannel;
	DBGPUT("SDL-mixer: playing music %d %d\n", num, loops);
	Mix_HookMusicFinished(musicFinished);
	music_ptr = (Mix_Music *)lbui_music_handles[num]->handle;
	soundChannel = Mix_PlayMusic(music_ptr, loops);
	return 0;
    }
    else {
	return -1;
    }
}



int
sdl_halt_music(void) {
    if (lbui_get_sound_status()) {
	music_ptr = NULL;
	Mix_HaltMusic();
	/*
	int soundChannel;
	Mix_Music *ptr = NULL;
	//DBGPUT("Playing sound %d\n", num);
	ptr = (Mix_Music *)music_handles[num]->handle;
	soundChannel = Mix_PlayMusic(ptr, 0);
	*/
	return 0;
    }
    else {
	return -1;
    }
}


#endif /* use_sdl_mixer */
