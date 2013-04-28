/* File: bass.c */

/* Purpose: BASS sound back-end for AngbandTk */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifdef __LCC__
#include <mmsystem.h>
#endif

#include <stdio.h>
#include "bass.h"
#include "sound-dll.h"
#include "dbwin.h"

#define BASS24

static HSTREAM g_stream[MAX_SND_CHANNELS];
static int g_init = 0, g_self_init = 0;
static int g_start = 0;
static int debug_sound = 0;
static int g_volume = 100;

/* Dumb: Call back to the application */
static SoundDoneProc sound_done_hook;

void BASS_Error(char *f)
{
	dbwin("%s error %d\n", f, BASS_ErrorGetCode());
}

void sound_hook_exit(void)
{
	if (debug_sound) dbwin("sound_hook_exit\n");

	if (g_init && g_self_init)
		BASS_Free();
}

/* Free resources on channels that have finished playing */
void sound_hook_purge(void)
{
	int i;

	if (debug_sound) dbwin("sound_hook_purge\n");

	for (i = 0; i < MAX_SND_CHANNELS; i++)
	{
		/* Channel is free */
		if (g_stream[i] == 0) continue;

		/* Sound is playing */
		if (BASS_ChannelIsActive(g_stream[i]) == BASS_ACTIVE_PLAYING) continue;

		/* Sound is finished */
		(*sound_done_hook)(i);
	}
}

/* hook_load -- load sound data */
SoundData sound_hook_load(char *path, int chan)
{
	/* Abort if sound was not initialized */
	if (!g_init) return 0L;

	if (debug_sound) dbwin("sound_hook_load: path=%s chan=%d\n", path, chan);

#ifdef SOUND3D
	g_stream[chan] = BASS_StreamCreateFile(FALSE, path, 0, 0, BASS_SAMPLE_MONO | BASS_SAMPLE_3D);
#else
	g_stream[chan] = BASS_StreamCreateFile(FALSE, path, 0, 0, 0);
#endif

	if (g_stream[chan] != 0)
	{
		(void) BASS_ChannelSetAttribute(g_stream[chan],
			BASS_ATTRIB_VOL, (g_volume/100.0f));
	}
	return (SoundData) g_stream[chan];
}

/* hook_play -- Play sound data */
void sound_hook_play(SoundData snd_data, int chan)
{
	if (debug_sound) dbwin("sound_hook_play chan=%d\n", chan);
#if 0
	/* Work around a bug */
	if (!g_start)
	{
		BASS_Start();
		g_start = 1;
	}
#endif

#ifdef BASS20
	if (!BASS_StreamPlay((HSTREAM) snd_data, FALSE, 0))
#endif /* BASS20 */
#ifdef BASS24
	if (!BASS_ChannelPlay((HSTREAM) snd_data, TRUE))
#endif /* BASS24 */
	{
		BASS_Error("BASS_SamplePlay");
		(*sound_done_hook)(chan);
	}
}

/* hook_free -- Free sound data */
void sound_hook_free_data(SoundData snd_data, int chan)
{
	if (debug_sound) dbwin("sound_hook_free_data chan=%d\n", chan);

	(void) BASS_ChannelStop((HSTREAM) snd_data);
	BASS_StreamFree((HSTREAM) snd_data);
	g_stream[chan] = 0;
}

/* hook_free_channel -- Mark a channel as free */
void sound_hook_free_channel(int chan)
{
	if (debug_sound) dbwin("sound_hook_free_channel chan=%d\n", chan);
}

/* hook_stop -- Stop sound playing */
void sound_hook_stop(SoundData snd_data, int chan)
{
	if (debug_sound) dbwin("sound_hook_stop chan=%d\n", chan);

	(void) BASS_ChannelStop(g_stream[chan]);
	(*sound_done_hook)(chan);
}

/* hook_activate -- Claim control of the sound system */
void sound_hook_activate(int active)
{
	/* BASS_Pause/Stop/Start ??? */
}

void sound_hook_volume(int volume)
{
	int i;

	g_volume = volume;

	for (i = 0; i < MAX_SND_CHANNELS; i++)
	{
		/* Channel is free */
		if (g_stream[i] == 0) continue;

		(void) BASS_ChannelSetAttribute(g_stream[i],
			BASS_ATTRIB_VOL, (volume/100.0f));
	}
}

void sound_hook_window(void *hWnd)
{
}

#ifdef SOUND3D
void sound_hook_position(int chan, int x, int y)
{
	BASS_3DVECTOR pos;

/*	if (debug_sound) dbwin("sound_hook_position: chan %d x,y %d,%d\n", chan, x, y); */

	pos.x = x * 5;
	pos.y = y * 5;
	pos.z = 0;
	if (chan == -1)
		BASS_Set3DPosition(&pos, NULL, NULL, NULL);
	else if (BASS_ChannelIsActive(g_stream[chan]) == BASS_ACTIVE_PLAYING)
	{
		if (x == -1 && y == -1)
			BASS_ChannelSet3DAttributes(g_stream[chan], BASS_3DMODE_OFF, -1, -1, -1, -1, -1);
		else
			BASS_ChannelSet3DPosition(g_stream[chan], &pos, NULL, NULL);
	}
	BASS_Apply3D();
}
#endif

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
#endif
#ifdef SOUND3D
	sound_hook_position,
#endif
	sound_hook_volume,
	FALSE /* sound data can't be shared by channels */
};

static HINSTANCE ghModule;

BOOL WINAPI DllMain(HINSTANCE hModule, ULONG Reason, LPVOID pv)
{
	switch (Reason)
	{
		case DLL_PROCESS_ATTACH:
			ghModule = hModule;
			break;
	}

	return TRUE;
}

static HWND init_window(void)
{
	HWND hWnd;
	WNDCLASS wc;

	wc.hCursor        = LoadCursor(NULL, IDC_ARROW);
	wc.hIcon          = NULL;
	wc.lpszMenuName   = NULL;
	wc.lpszClassName  = "BASSSoundWindow";
	wc.hbrBackground  = GetStockObject(LTGRAY_BRUSH);
	wc.hInstance      = ghModule;
	wc.style          = 0;
	wc.lpfnWndProc    = DefWindowProc;
	wc.cbWndExtra     = 0;
	wc.cbClsExtra     = 0;

	if (!RegisterClass(&wc))
	{
		MessageBox(NULL, "Could not register window class.", "Sound Error",
			MB_OK);
		return NULL;
	}

	/*
	 * You can only play sounds if the active window has a DirectSound
	 * buffer associated with it. Because I don't have any windows yet,
	 * I will create an invisible one here.
	 */
	hWnd = CreateWindow("BASSSoundWindow", "" , WS_DISABLED, 0, 0, 0, 0,
		NULL, NULL, ghModule, NULL);
	if (!hWnd)
	{
		MessageBox(NULL, "Could not create sound window.", "Sound Error",
			MB_OK);
		return NULL;
	}
	return hWnd;
}

/* hook_init -- Initialize sound channels */
int Sound_Init(SoundStubs **stubsPtrPtr, SoundDoneProc doneProc)
{
	HWND hWnd;
	int i;
	DWORD version = BASS_GetVersion();
	char buf[128];

#ifdef BASS20
	if (version != MAKELONG(2,0))
	{
		MessageBox(NULL, "BASS version 2.0 required", "Sound Error", MB_OK);
		return 1;
	}
#endif
#ifdef BASS24
	if (HIWORD(version) != MAKEWORD(4,2))
	{
		(void) sprintf(buf, "BASS version 2.4 required, version %x loaded.",
			(unsigned int)version);
		MessageBox(NULL, buf, "Sound Error", MB_OK);
		return 1;
	}
#endif

	if ((hWnd = init_window()) == NULL)
	{
		return 1;
	}

#ifdef SOUND3D
	if (!BASS_Init(1, 44100, BASS_DEVICE_3D, hWnd, NULL))
#else
	if (!BASS_Init(1, 44100, 0, hWnd, NULL))
#endif
	{
		DWORD error = BASS_ErrorGetCode();

		if (error != BASS_ERROR_ALREADY)
		{
			(void) sprintf(buf, "Could not initialize sound.\nBASS_Init() failed with error %ld", error);
			MessageBox(hWnd, buf, "Sound Error", MB_OK);
			return 1;
		}
		g_self_init = 0;
	}
	else
	{
		g_self_init = 1;
	}
	g_init = 1;

#ifdef SOUND3D
/*	BASS_Set3DFactors(1.0, 5.0, 0.0);*/
	/* Turn EAX off */
	BASS_SetEAXParameters(-1,0.0,-1.0,-1.0);
#endif

	for (i = 0; i < MAX_SND_CHANNELS; i++)
	{
		g_stream[i] = 0;
	}

	(*stubsPtrPtr) = &soundStubs;

	/* Hack -- Call back to the application */
	sound_done_hook = doneProc;

	return 0;
}

