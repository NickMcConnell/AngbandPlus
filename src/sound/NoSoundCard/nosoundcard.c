/* File: nosoundcard.c */

/* Purpose: Stub sound back-end for AngbandTk */

/*
 * Copyright (c) 1997-2005 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifdef PLATFORM_WIN
#include <windows.h>
#endif /* PLATFORM_WIN */
#include "sound-dll.h"

/* #define DEBUG */

#ifdef DEBUG
#include "dbwin.h"
#endif /* DEBUG */

#ifndef TRUE
#define TRUE 1
#endif

/* hook_exit -- Free sound system resources */
void sound_hook_exit(void)
{
#ifdef DEBUG
	dbwin("sound_hook_exit\n");
#endif /* DEBUG */
}

/* Free resources on channels that have finished playing */
void sound_hook_purge(void)
{
#ifdef DEBUG
	dbwin("sound_hook_purge\n");
#endif /* DEBUG */
}

/* hook_load -- load sound data */
SoundData sound_hook_load(char *path, int chan)
{
#ifdef DEBUG
	dbwin("sound_hook_load: path=%s chan=%d\n", path, chan);
#endif /* DEBUG */

	return 0;
}

/* hook_play -- Play sound data */
void sound_hook_play(SoundData snd_data, int chan)
{
#ifdef DEBUG
	dbwin("sound_hook_play chan=%d\n", chan);
#endif /* DEBUG */
}

/* hook_free_data -- Free sound data */
void sound_hook_free_data(SoundData snd_data, int chan)
{
#ifdef DEBUG
	dbwin("sound_hook_free_data chan=%d\n", chan);
#endif /* DEBUG */
}

/* hook_free_channel -- Mark a channel as free */
void sound_hook_free_channel(int chan)
{
#ifdef DEBUG
	dbwin("sound_hook_free_channel chan=%d\n", chan);
#endif /* DEBUG */
}

/* hook_stop -- Stop sound playing */
void sound_hook_stop(SoundData snd_data, int chan)
{
#ifdef DEBUG
	dbwin("sound_hook_stop chan=%d\n", chan);
#endif /* DEBUG */
}

/* hook_activate -- Claim control of the sound system */
void sound_hook_activate(int active)
{
#ifdef DEBUG
	dbwin("sound_hook_activate\n");
#endif /* DEBUG */
}

/* hook_volume -- Change the sound volume, 0-100 */
void sound_hook_volume(int volume)
{
#ifdef DEBUG
	dbwin("sound_hook_volume\n");
#endif /* DEBUG */
}

#ifdef PLATFORM_WIN

/* hook_window -- Allow window to play sounds */
void sound_hook_window(void *hWnd)
{
#ifdef DEBUG
	dbwin("sound_hook_window\n");
#endif /* DEBUG */
}

#endif /* PLATFORM_WIN */

SoundStubs soundStubs = {
	sound_hook_activate,
	sound_hook_exit,
	sound_hook_free_channel,
	sound_hook_free_data,
	sound_hook_load,
	sound_hook_play,
	sound_hook_purge,
	sound_hook_stop,
#ifdef PLATFORM_WIN
	sound_hook_window,
#endif /* PLATFORM_WIN */
#ifdef SOUND3D
	sound_hook_position,
#endif
	sound_hook_volume,
	TRUE
};

/* hook_init -- Initialize sound channels */
int Sound_Init(SoundStubs **stubsPtrPtr, SoundDoneProc doneProc)
{
#ifdef DEBUG
	dbwin("sound_hook_init\n");
#endif /* DEBUG */

	(*stubsPtrPtr) = &soundStubs;

	return 0;
}

