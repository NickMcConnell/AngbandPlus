#include <AL/al.h>
#include <AL/alc.h>
#include <AL/alut.h>
#include "langband.h"
//#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>

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

    return 0;
}

int
al_close_mixer() {
    
    DBGPUT("Langband/OpenAL: Closing mixer.\n");
    
    alutLoadVorbisp = NULL;
    
    if (my_context_id == NULL) {
	//_alcSetError(ALC_INVALID_CONTEXT);
	ERRORMSG("Invalid AL context\n");
	return -2;
    }

    alcDestroyContext(my_context_id);

    DBGPUT("Langband/OpenAL: Mixer closed.\n");
    return 0;
}


int
al_load_sound_effect(const char *fname, sound_effect *handle) {

//    int where = source_count;
    
    ALsizei size;
    ALsizei bits;
    ALsizei freq;
    ALsizei format;
    int retval = -1;
    
//    struct SourceInfo *ptr = &sound_sources[where];
//    ERRORMSG("boom is %ld\n", handle->buffer_idx);
    
    alGenBuffers( 1, &(handle->buffer_idx));
    
    if(alGetError() != AL_NO_ERROR) {
	ERRORMSG("Langband/OpenAL: Unable to generate buffer.\n" );
	return -1;
    }

    //ERRORMSG("boom %s is %ld\n", fname, handle->buffer_idx);
    
    // load a wav, and get the info into some vars
    retval = alutLoadWAV( fname, &(handle->handle), &format, &size, &bits, &freq );
    if (retval != AL_TRUE) {
	ERRORMSG("Langband/OpenAL: Problems loading WAV-file %s.\n", fname);
	return -2;
    }
    // inform the buffer about these data
    alBufferData( handle->buffer_idx, format, handle->handle, size, freq );

    // clear errors
    if (alGetError() != AL_NO_ERROR) {
	ERRORMSG("Langband/OpenAL: Unable to read soundfile %s.\n", fname );
	return -3;
    }
    else {
	return 0;
    }
}



//static ALuint vorbsource = (ALuint ) -1;

int
al_load_music_file(const char *fname, music_handle *handle) {
    struct stat sbuf;
    int size;
    void *data;
    FILE *fh;

    INFOMSG("Langband/OpenAL: Trying to load music %s.\n", fname);
    
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
	return 1;
    }
    
    alGenBuffers( 1, &(handle->buffer_idx));

    fread(data, size, 1, fh);

    if (!alutLoadVorbisp) {
	alutLoadVorbisp = (vorbisLoader *) alGetProcAddress((ALubyte *) VORBIS_FUNC);
	if (alutLoadVorbisp == NULL) {
	    free(data);
	    
	    ERRORMSG("Langband/OpenAL: Could not GetProc %s.\n", (ALubyte *) VORBIS_FUNC);
	    return -4;
	}
    }
    
    if (alutLoadVorbisp(handle->buffer_idx, data, size) != AL_TRUE) {
	ERRORMSG("Langband/OpenAL: alutLoadVorbis failed.\n");
	return -2;
    }

    INFOMSG("Langband/OpenAL: Loaded music file %s of size %ld in index %ld.\n", fname, size, handle->buffer_idx);
    
    //free(data);
    
    return 0;
}

int
al_play_sound_effect(int num, float where) {
    
    ALfloat pos[3] = { 0.0, 0.0, 0.0 }; 
    ALuint boom;
    alGenSources( 1, &boom );
    
    pos[0] = 4.0 * where;
    
    // set info about the source on the right
    alSourcefv( boom, AL_POSITION, pos );
    alSourcefv( boom, AL_VELOCITY, zeroes );
    alSourcefv( boom, AL_ORIENTATION, back );
    alSourcei( boom, AL_BUFFER, sound_effects[num]->buffer_idx );

    //INFOMSG("play %d\n", num);
    alSourcePlay(boom);
    return 0;
}

int
al_play_music_file(int num, float where) {

    
    ALfloat pos[3] = { 0.0, 0.0, 0.0 }; 
    ALuint boom;
    alGenSources( 1, &boom );

    //INFOMSG("Play music.. %d\n", music_handles[num]->buffer_idx);
    pos[0] = 4.0 * where;
    
    // set info about the source on the right
    alSourcei( boom, AL_BUFFER, music_handles[num]->buffer_idx);
    //alSourcei( boom, AL_LOOPING, AL_TRUE); 
    alSourcefv( boom, AL_POSITION, pos );
//    alSourcefv( boom, AL_VELOCITY, zeroes );
//    alSourcefv( boom, AL_ORIENTATION, back );
//    alSourcei( boom, AL_BUFFER, sound_effects[num]->buffer_idx );

    //INFOMSG("play %d\n", num);
    alSourcePlay(boom);

    //SDL_Delay(5000);
    
    return 0;
}


#ifndef USE_SDL
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
