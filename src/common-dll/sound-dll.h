/* File: sound-dll.h */

/* Purpose: public interface to sound.dll */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef INCLUDED_SOUNDDLL_H
#define INCLUDED_SOUNDDLL_H

#include "storage.h"

#ifndef INCLUDED_SOUND_H
typedef char *SoundId;
typedef void *SoundData;
#endif

#define MAX_SND_CHANNELS 16

typedef struct SoundStubs SoundStubs;
struct SoundStubs
{
	void (*activate)(int active);
	void (*exit)(void);
	void (*free_channel)(int chan);
	void (*free_data)(SoundData snd_data, int chan);
	SoundData (*load)(char *path, int chan);
	void (*play)(SoundData snd_data, int chan);
	void (*purge)(void);
	void (*stop)(SoundData snd_data, int chan);
#ifdef PLATFORM_WIN
	void (*window)(void *hWnd);
#endif /* PLATFORM_WIN */
#ifdef SOUND3D
	void (*position)(int chan, int x, int y);
#endif
	void (*volume)(int volume);
	int shareData;
};

#ifndef BUILD_sound_dll

#define sound_hook_exit \
	(soundStubsPtr->exit)
#define sound_hook_activate \
	(soundStubsPtr->activate)
#define sound_hook_free_channel \
	(soundStubsPtr->free_channel)
#define sound_hook_free_data \
	(soundStubsPtr->free_data)
#define sound_hook_load \
	(soundStubsPtr->load)
#define sound_hook_play \
	(soundStubsPtr->play)
#define sound_hook_purge \
	(soundStubsPtr->purge)
#define sound_hook_stop \
	(soundStubsPtr->stop)
#define sound_hook_window \
	(soundStubsPtr->window)
#define sound_hook_volume \
	(soundStubsPtr->volume)

#endif /* BUILD_sound_dll */

typedef void (*SoundDoneProc)(int chan);
DLL_EXTERN int Sound_Init(SoundStubs **stubsPtrPtr, SoundDoneProc doneProc);

#endif /* INCLUDED_SOUNDDLL_H */
