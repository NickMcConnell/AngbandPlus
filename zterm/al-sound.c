#include <AL/al.h>
#include <AL/alc.h>
#include <AL/alut.h>
#include "lbsound.h"
#include "lbtools.h"
//#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//static ALfloat left_pos[3] = { -4.0, 0.0, 0.0 }; // x, y, z I think.. x negative means left (sound is up)
//static ALfloat right_pos[3] = { 4.0, 0.0, 0.0 }; // x, y, z I think.. x positive means right (sound is up)
static ALfloat zeroes[] = { 0.0f, 0.0f, 0.0f }; // at the 0,0,0 coordinate
static ALfloat front[] = { 0.0f, 0.0f, -1.0f, 0.0f, 1.0f, 0.0f };
static ALfloat back[] = { 0.0f, 0.0f, 1.0f, 0.0f, 1.0f, 0.0f };
#define VORBIS_FUNC    "alutLoadVorbis_LOKI"
/* our vorbis extension */
typedef ALboolean (vorbisLoader)(ALuint, ALvoid *, ALint);
vorbisLoader *alutLoadVorbisp = NULL;
void *my_context_id = NULL;

int
al_init_mixer() {
    
    ALCdevice *dev = NULL;
    
    alutLoadVorbisp = NULL;
    my_context_id = NULL;
    //DBGPUT("OpenAL: Init mixer %u\n", getpid());
    
    dev = alcOpenDevice(  (const ALubyte *) "'((native-use-select #f))" );

    if ( dev == NULL ) {
	ERRORMSG("Invalid sound-device.\n");
	//_alcSetError( ALC_INVALID_DEVICE );
	return -1;
    }
    
    my_context_id = alcCreateContext( dev, NULL );
    
    if (my_context_id == NULL) {
	ERRORMSG("Invalid sound-device.\n");
	return -2;
	//_alcSetError( ALC_INVALID_DEVICE );
    }
    
    alcMakeContextCurrent( my_context_id );
    

    // this places the listener in the middle I think. 
    alListenerfv( AL_POSITION, zeroes );
    alListenerfv( AL_VELOCITY, zeroes );
    alListenerfv( AL_ORIENTATION, front );

    //DBGMSG("Returning from init.\n"); 
    return 0;
}

int
al_close_mixer() {
    
    //DBGPUT("Langband/OpenAL: Closing mixer.\n");
    
    alutLoadVorbisp = NULL;
    
    if (my_context_id == NULL) {
	//_alcSetError(ALC_INVALID_CONTEXT);
	ERRORMSG("Invalid AL context\n");
	return -2;
    }

    alcDestroyContext(my_context_id);

    //DBGPUT("Langband/OpenAL: Mixer closed.\n");
    return 0;
}


int
al_load_sound_effect(const char *fname, int idx) {

    struct stat sbuf;
    int size;
    int real_idx = idx;
    void *data;
    FILE *fh;
    char *ptr = NULL;
    sound_effect *handle = NULL;

    //DBGPUT("Langband/OpenAL: Trying to load sound %s.\n", fname);

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
    
    if (stat(fname, &sbuf) == -1) {
	perror(fname);
	return errno;
    }
    
    size = sbuf.st_size;
    data = malloc(size);
	
    if (data == NULL) {
	return 1;
    }
    
    fh = fopen(fname, "rb");
    if (fh == NULL) {
	ERRORMSG("Langband/OpenAL: Could not open %s.\n", fname);
	
	free(data);
	return -5;
    }

    handle = malloc(sizeof(sound_effect));

    alGenBuffers( 1, &(handle->buffer_idx));

    fread(data, size, 1, fh);

    if (!alutLoadVorbisp) {
	alutLoadVorbisp = (vorbisLoader *) alGetProcAddress((ALubyte *) VORBIS_FUNC);
	if (alutLoadVorbisp == NULL) {
	    free(data);
	    free(handle);
	    
	    ERRORMSG("Langband/OpenAL: Could not GetProc %s.\n", (ALubyte *) VORBIS_FUNC);
	    return -4;
	}
    }
    
    if (alutLoadVorbisp(handle->buffer_idx, data, size) != AL_TRUE) {
	ERRORMSG("Langband/OpenAL: alutLoadVorbis failed.\n");
	free(data);
	free(handle);
	return -2;
    }

    ptr = malloc(strlen(fname)+1);
    strcpy(ptr, fname);
    
    handle->filename = ptr;
    
    lbui_sound_effects[real_idx] = handle;
    
    //INFOMSG("Langband/OpenAL: Loaded music file %s of size %ld in index %ld.\n", fname, size, handle->buffer_idx);
    
    //free(data);
    
    return real_idx;

}



//static ALuint vorbsource = (ALuint ) -1;

int
al_load_music_file(const char *fname, int idx) {
    struct stat sbuf;
    int size;
    int real_idx = idx;
    void *data;
    FILE *fh;
    char *ptr = NULL;
    music_handle *handle = NULL;

    //DBGPUT("Langband/OpenAL: Trying to load music %s.\n", fname);

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
    
    if (stat(fname, &sbuf) == -1) {
	perror(fname);
	return errno;
    }
    
    size = sbuf.st_size;
    data = malloc(size);
	
    if (data == NULL) {
	return 1;
    }
    
    fh = fopen(fname, "rb");
    if (fh == NULL) {
	ERRORMSG("Langband/OpenAL: Could not open %s.\n", fname);
	
	free(data);
	return -5;
    }

    handle = malloc(sizeof(music_handle));

    alGenBuffers( 1, &(handle->buffer_idx));

    fread(data, size, 1, fh);

    if (!alutLoadVorbisp) {
	alutLoadVorbisp = (vorbisLoader *) alGetProcAddress((ALubyte *) VORBIS_FUNC);
	if (alutLoadVorbisp == NULL) {
	    free(data);
	    free(handle);
	    
	    ERRORMSG("Langband/OpenAL: Could not GetProc %s.\n", (ALubyte *) VORBIS_FUNC);
	    return -4;
	}
    }
    
    if (alutLoadVorbisp(handle->buffer_idx, data, size) != AL_TRUE) {
	ERRORMSG("Langband/OpenAL: alutLoadVorbis failed.\n");
	free(data);
	free(handle);
	return -2;
    }

    ptr = malloc(strlen(fname)+1);
    strcpy(ptr, fname);
    
    handle->filename = ptr;
    
    lbui_music_handles[real_idx] = handle;
    
    //INFOMSG("Langband/OpenAL: Loaded music file %s of size %ld in index %ld.\n", fname, size, handle->buffer_idx);
    
    //free(data);
    
    return real_idx;
}

int
al_play_sound_effect(int num, float where, short channel, short loops) {
    
    ALfloat pos[3] = { 0.0, 0.0, 0.0 }; 
    ALuint boom;
    alGenSources( 1, &boom );
    
    if (!lbui_sound_effects[num]) {
	ERRORMSG("OpenAL: Unable to find wanted sound-effect at idx %d\n", num);
	return -6;
    }
    
    pos[0] = 4.0 * where;
    
    // set info about the source on the right
    alSourcefv( boom, AL_POSITION, pos );
    alSourcefv( boom, AL_VELOCITY, zeroes );
    alSourcefv( boom, AL_ORIENTATION, back );
    alSourcei( boom, AL_BUFFER, lbui_sound_effects[num]->buffer_idx );

    //INFOMSG("play %d\n", num);
    alSourcePlay(boom);

    channel = loops = 0; // to avoid warnings
    
    return 0;
}

int
al_play_music_file(int num, float where, short loops) {

    
    ALfloat pos[3] = { 0.0, 0.0, 0.0 }; 
    ALuint boom;
    alGenSources( 1, &boom );

    if (!lbui_music_handles[num]) {
	return -6;
    }
    
    //INFOMSG("Play music.. %d\n", music_handles[num]->buffer_idx);
    pos[0] = 4.0 * where;
    
    // set info about the source on the right
    alSourcei( boom, AL_BUFFER, lbui_music_handles[num]->buffer_idx);
    //alSourcei( boom, AL_LOOPING, AL_TRUE); 
    alSourcefv( boom, AL_POSITION, pos );
    // alSourcefv( boom, AL_VELOCITY, zeroes );
    // alSourcefv( boom, AL_ORIENTATION, back );
    // alSourcei( boom, AL_BUFFER, sound_effects[num]->buffer_idx );

    //INFOMSG("play %d\n", num);
    alSourcePlay(boom);

    //SDL_Delay(5000);
    loops = 0; // to avoid warnings
    
    return 0;
}


int
al_halt_music(void) {
    
    return 0;
}


int
al_halt_sound_effects(short channel) {

    channel = 0; // to avoid warnings
    return 0;
}


#ifdef USE_AL_MAIN
int
main(int argc, char* argv[]) {

    int horse = 0;

    al_init_mixer();
    
    horse = al_load_source("Horse.wav");
    
    al_play_sound(horse,-1.0);
    //alSourcePlay( left_sid );
    SDL_Delay(4000);
    al_play_sound(horse,1.0);
    //alSourcePlay( right_sid );
    SDL_Delay(4000);
    
    //alSourcePlay( left_sid );
    //alSourcePlay( right_sid );
	
    return 0;
}

#endif

#ifdef OUTDATED_WAV_READER

int
al_load_sound_effect(const char *fname, int idx) {

    // int where = source_count;
    
    ALsizei size;
    ALsizei bits;
    ALsizei freq;
    ALsizei format;
    int retval = -1;
    int real_idx = idx;
    sound_effect *handle = NULL;
    // struct SourceInfo *ptr = &sound_sources[where];
    // ERRORMSG("boom is %ld\n", handle->buffer_idx);

        // we should add it to the list
    if (real_idx < 0) {
	real_idx = find_free_effect_spot();
    }
    if (real_idx < 0) {
	ERRORMSG("No free effect-spots.\n");
	return -3;
    }

    if (lbui_sound_effects[real_idx]) {
	//DBGPUT("Al: Free sfx handle %d\n", real_idx);
	free(lbui_sound_effects[real_idx]);
	lbui_sound_effects[real_idx] = NULL;
    }
    
    handle = malloc(sizeof(sound_effect));
    
    alGenBuffers( 1, &(handle->buffer_idx));
    
    if (alGetError() != AL_NO_ERROR) {
	ERRORMSG("Langband/OpenAL: Unable to generate buffer.\n" );
	free(handle);
	return -1;
    }

    //ERRORMSG("boom %s is %ld\n", fname, handle->buffer_idx);
    
    // load a wav, and get the info into some vars
    retval = alutLoadWAV( fname, &(handle->handle), &format, &size, &bits, &freq );
    if (retval != AL_TRUE) {
	ERRORMSG("Langband/OpenAL: Problems loading WAV-file %s.\n", fname);
	free(handle);
	return -2;
    }
    // inform the buffer about these data
    alBufferData( handle->buffer_idx, format, handle->handle, size, freq );

    // clear errors
    if (alGetError() != AL_NO_ERROR) {
	ERRORMSG("Langband/OpenAL: Unable to read soundfile %s.\n", fname );
	free(handle);
	return -3;
    }
    else {
	char *ptr = malloc(strlen(fname)+1);
	strcpy(ptr, fname);
	handle->filename = ptr;
	lbui_sound_effects[real_idx] = handle;
	//DBGPUT("AL: Assign %s to idx %d.\n", fname, real_idx);
	return real_idx;
    }
}

#endif
