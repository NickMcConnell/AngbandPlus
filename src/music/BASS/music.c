/* File: music.c */

/* Purpose: BASS music package for AngbandTk */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <windows.h>
#include <tcl.h>
#include "bass.h"

#define BASS24

DLLEXPORT int Music_Init _ANSI_ARGS_((Tcl_Interp *interp));

static HMUSIC gMusic = 0;
static HSTREAM gStream = 0;
static HSYNC gSync = 0;
static int g_volume = 100;
static int g_mute = FALSE;
static char *gNotifyCmd = NULL;
static Tcl_Interp *g_interp;
static BOOL gStarted = FALSE;
static BOOL gDoFree = FALSE;

static int stop_music(void)
{
	if (gMusic != 0)
	{
		if (!BASS_ChannelStop(gMusic)) return TCL_ERROR;
/* BASS_ChannelRemoveSync() ? */
		BASS_MusicFree(gMusic);
		gMusic = 0;
	}
	if (gStream != 0)
	{
		if (!BASS_ChannelStop(gStream)) return TCL_ERROR;
		BASS_StreamFree(gStream);
		gStream = 0;
	}
	return TCL_OK;
}

/*
 * Tcl_DoWhenIdle() callback. Evaluate the script which was passed to
 * "music notify". This lets Tcl know that a song has ended.
 */
void NotifyIdleProc(ClientData clientData)
{
	if (gNotifyCmd != NULL)
		Tcl_GlobalEval(g_interp, gNotifyCmd);
}

/* Called by BASS when a non-looping song ends */
#ifdef BASS20
void CALLBACK SyncProc(HSYNC handle, DWORD channel, DWORD data, DWORD user)
#endif
#ifdef BASS24
void CALLBACK SyncProc(HSYNC handle, DWORD channel, DWORD data, void *user)
#endif
{
	(void) stop_music();
	Tcl_DoWhenIdle(NotifyIdleProc, NULL);
}

char *types_music[] = { "MO3", "IT", "XM", "S3M", "MTM", "MOD", "UMX", NULL };
char *types_stream[] = { "MP3", "MP2", "MP1", "OGG", "WAV", NULL };

#define TYPE_UNKNOWN 0
#define TYPE_MUSIC 1
#define TYPE_STREAM 2

static int get_type(char *path)
{
	char *p;
	int i = 0;

	p = strrchr(path, '.');
	if (p != NULL)
	{
		p++;
/*		if (strlen(p) != 3)
			return TYPE_UNKNOWN;*/
		for (i = 0; types_music[i] != NULL; i++)
		{
			if (stricmp(types_music[i], p) == 0)
				return TYPE_MUSIC;
		}
		for (i = 0; types_stream[i] != NULL; i++)
		{
			if (stricmp(types_stream[i], p) == 0)
				return TYPE_STREAM;
		}
	}

	return TYPE_UNKNOWN;
}

int objcmd_music(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *options[] = {"play", "stop", "pause", "resume",
		"notify", "length", "position", "state", "issong", "volume", "mute",
		NULL};
	enum {IDX_PLAY, IDX_STOP, IDX_PAUSE, IDX_RESUME, IDX_NOTIFY,
		IDX_LENGTH, IDX_POSITION, IDX_STATE, IDX_ISSONG, IDX_VOLUME,
		IDX_MUTE} option;
	char *t, buf[128];
	DWORD error;
	Tcl_DString temp;
	int type;

	if (objc < 2)
	{
		Tcl_WrongNumArgs(interp, 1, objv, "string");
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objv[1], options, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_PLAY:
			if (stop_music() != TCL_OK) goto error;
			if (!gStarted)
			{
				BASS_Start();
				gStarted = TRUE;
			}
			t = Tcl_GetStringFromObj(objv[2], NULL);
			Tcl_DStringInit(&temp);
			t = Tcl_TranslateFileName(interp, t, &temp);
			if (t == NULL) return TCL_ERROR;
			type = get_type(t);
			if (type == TYPE_UNKNOWN)
			{
				Tcl_AppendResult(interp, "unsupported file extension on \"",
					t, "\"", NULL);
				Tcl_DStringFree(&temp);
				return TCL_ERROR;
			}
			if (type == TYPE_MUSIC)
			{
				gMusic = BASS_MusicLoad(FALSE, t, 0, 0, BASS_MUSIC_RAMP, 0);
				Tcl_DStringFree(&temp);
				if (gMusic == 0) goto error;
				gSync = BASS_ChannelSetSync(gMusic, BASS_SYNC_END, 0, SyncProc, 0);
				if (!gSync) goto error;
#ifdef BASS20
				if (!BASS_ChannelSetAttributes(gMusic, -1, g_mute ? 0 : g_volume, -101))
#endif
#ifdef BASS24
				if (!BASS_ChannelSetAttribute(gMusic, BASS_ATTRIB_VOL,
					g_mute ? 0 : (g_volume/100.0f)))
#endif
				{
					BASS_MusicFree(gMusic);
					gMusic = 0;
					goto error;
				}
#ifdef BASS20
				if (!BASS_MusicPlay(gMusic))
#endif
#ifdef BASS24
				if (!BASS_ChannelPlay(gMusic, TRUE))
#endif
				{
					BASS_MusicFree(gMusic);
					gMusic = 0;
					goto error;
				}
			}
			if (type == TYPE_STREAM)
			{
				gStream = BASS_StreamCreateFile(FALSE, t, 0, 0, 0);
				Tcl_DStringFree(&temp);
				if (gStream == 0) goto error;
				gSync = BASS_ChannelSetSync(gStream, BASS_SYNC_END, 0, SyncProc, 0);
				if (!gSync) goto error;
#ifdef BASS20
				if (!BASS_ChannelSetAttributes(gStream, -1, g_mute ? 0 : g_volume, -101))
#endif
#ifdef BASS24
				if (!BASS_ChannelSetAttribute(gStream, BASS_ATTRIB_VOL,
					g_mute ? 0 : (g_volume/100.0f)))
#endif
				{
					BASS_StreamFree(gStream);
					gStream = 0;
					goto error;
				}
#ifdef BASS20
				if (!BASS_StreamPlay(gStream, FALSE, 0))
#endif
#ifdef BASS24
				if (!BASS_ChannelPlay(gStream, FALSE))
#endif
				{
					BASS_StreamFree(gStream);
					gStream = 0;
					goto error;
				}
			}
			break;

		case IDX_STOP:
			if (stop_music() != TCL_OK) goto error;
			break;

		case IDX_PAUSE:
		{
			DWORD handle = gMusic + gStream;
			if (handle == 0)
			{
				Tcl_SetResult(interp, "no current song", TCL_STATIC);
				return TCL_ERROR;
			}
			if (BASS_ChannelIsActive(handle) != BASS_ACTIVE_PLAYING)
			{
				Tcl_SetResult(interp, "music is not playing", TCL_STATIC);
				return TCL_ERROR;
			}
			if (!BASS_ChannelPause(handle)) goto error;
			break;
		}

		case IDX_RESUME:
		{
			DWORD handle = gMusic + gStream;
			if (handle == 0)
			{
				Tcl_SetResult(interp, "no current song", TCL_STATIC);
				return TCL_ERROR;
			}
			if (BASS_ChannelIsActive(handle) != BASS_ACTIVE_PAUSED)
			{
				Tcl_SetResult(interp, "music is not paused", TCL_STATIC);
				return TCL_ERROR;
			}
#ifdef BASS20
			if (!BASS_ChannelResume(handle)) goto error;
#endif
#ifdef BASS24
			if (!BASS_ChannelPlay(handle, FALSE)) goto error;
#endif
			break;
		}

		case IDX_NOTIFY:
		{
			int length;
			if (gNotifyCmd) Tcl_Free(gNotifyCmd);
			t = Tcl_GetStringFromObj(objv[2], &length);
			gNotifyCmd = Tcl_Alloc(length + 1);
			(void) strcpy(gNotifyCmd, t);
			break;
		}

		case IDX_LENGTH:
		{
			DWORD len = 0;
			if (gMusic == 0 && gStream == 0)
			{
				Tcl_SetResult(interp, "no current song", TCL_STATIC);
				return TCL_ERROR;
			}
			if (gMusic != 0)
			{
				/* Length is in "orders" */
#ifdef BASS20
				len = BASS_MusicGetLength(gMusic, FALSE);
#endif
#ifdef BASS24
				len = BASS_ChannelGetLength(gMusic, BASS_POS_MUSIC_ORDER);
#endif
				if (len == -1) goto error;
			}
			if (gStream != 0)
			{
#ifdef BASS20
				QWORD bytes = BASS_StreamGetLength(gStream);
#endif
#ifdef BASS24
				QWORD bytes = BASS_ChannelGetLength(gStream, BASS_POS_BYTE);
#endif
				float seconds = BASS_ChannelBytes2Seconds(gStream, bytes);
				len = (int) seconds;
			}
			Tcl_SetObjResult(interp, Tcl_NewLongObj(len));
			break;
		}

		case IDX_POSITION:
		{
			DWORD pos;
			if (gMusic == 0 && gStream == 0)
			{
				Tcl_SetResult(interp, "no current song", TCL_STATIC);
				return TCL_ERROR;
			}
			if (objc == 3)
			{
				long position;
				DWORD len = 0;
				if (Tcl_GetLongFromObj(interp, objv[2], &position) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (gMusic != 0)
				{
#ifdef BASS20
					len = BASS_MusicGetLength(gMusic, FALSE);
#endif
#ifdef BASS24
					len = BASS_ChannelGetLength(gMusic, BASS_POS_MUSIC_ORDER);
#endif
				}
				if (gStream != 0)
				{
#ifdef BASS20
					QWORD bytes = BASS_StreamGetLength(gStream);
#endif
#ifdef BASS24
					QWORD bytes = BASS_ChannelGetLength(gStream, BASS_POS_BYTE);
#endif
					float seconds = BASS_ChannelBytes2Seconds(gStream, bytes);
					len = (int) seconds;
				}
				if ((position < 0) || (position >= len))
				{
					char errorMsg[512];
					(void) sprintf(errorMsg,
						"position \"%ld\" out of bounds: must be from 0 to %ld",
						position, (long) (len - 1));
					Tcl_SetResult(interp, errorMsg, TCL_VOLATILE);
					return TCL_ERROR;
				}
				if (gMusic != 0)
				{
#ifdef BASS20
#if 1
					/* FIXME: set "row" in the "order" */
					/* Want to stop current notes playing */
					if (!BASS_MusicPlayEx(gMusic, MAKELONG(position,0), -1, TRUE))
						goto error;
#else
					if (!BASS_ChannelSetPosition(gMusic, MAKELONG(position,0))) goto error;
#endif
#endif
#ifdef BASS24
					if (!BASS_ChannelSetPosition(gStream, MAKELONG(position,0),
						BASS_POS_MUSIC_ORDER | BASS_MUSIC_POSRESET)) goto error;
#endif
				}
				if (gStream != 0)
				{
					QWORD bytes;
					bytes = BASS_ChannelSeconds2Bytes(gStream, position);
					if (bytes == -1)
						goto error;
#ifdef BASS20
					if (!BASS_ChannelSetPosition(gStream, bytes)) goto error;
#endif
#ifdef BASS24
					if (!BASS_ChannelSetPosition(gStream, bytes, BASS_POS_BYTE)) goto error;
#endif
				}
				break;
			}
			if (gMusic != 0)
			{
#ifdef BASS20
				if ((pos = BASS_ChannelGetPosition(gMusic)) == -1) goto error;
#endif
#ifdef BASS24
				if ((pos = BASS_ChannelGetPosition(gMusic, BASS_POS_MUSIC_ORDER)) == -1) goto error;
#endif
				Tcl_SetObjResult(interp, Tcl_NewLongObj(LOWORD(pos)));
			}
			if (gStream != 0)
			{
#ifdef BASS20
				QWORD bytes = BASS_StreamGetLength(gStream);
#endif
#ifdef BASS24
				QWORD bytes = BASS_ChannelGetLength(gStream, BASS_POS_BYTE);
#endif
				float seconds = BASS_ChannelBytes2Seconds(gStream, bytes);
				Tcl_SetObjResult(interp, Tcl_NewLongObj(seconds));
			}
			break;
		}

		case IDX_STATE:
		{
			char *state = "none";
			if (gMusic != 0 || gStream != 0)
			{
				switch (BASS_ChannelIsActive(gMusic + gStream))
				{
					case BASS_ACTIVE_PLAYING:
						state = "playing";
						break;
					case BASS_ACTIVE_PAUSED:
						state = "paused";
						break;
				}
			}
			Tcl_SetObjResult(interp, Tcl_NewStringObj(state, -1));
			break;
		}

		case IDX_ISSONG:
		{
			Tcl_SetObjResult(interp, Tcl_NewBooleanObj(gMusic != 0 || gStream != 0));
			break;
		}

		/* Volume of music system, whether or not a song is playing */
		case IDX_VOLUME:
		{
			int volume;
			if (objc == 3)
			{
				if (Tcl_GetIntFromObj(interp, objv[2], &volume) != TCL_OK)
					return TCL_ERROR;
				if ((volume < 0) || (volume > 100)) break; /* ERROR */
				g_volume = volume;
				if (gMusic || gStream)
				{
#ifdef BASS20
					if (!BASS_ChannelSetAttributes(gMusic + gStream, -1, g_volume, -101))
#endif
#ifdef BASS24
					if (!BASS_ChannelSetAttribute(gMusic + gStream,
						BASS_ATTRIB_VOL, (g_volume/100.0f)))
#endif
						goto error;
				}
				break;
			}
			Tcl_SetObjResult(interp, Tcl_NewIntObj(g_volume));
			break;
		}

		case IDX_MUTE:
		{
			if (objc == 3)
			{
				if (Tcl_GetBooleanFromObj(interp, objv[2], &g_mute) != TCL_OK)
					return TCL_ERROR;
				if (gMusic || gStream)
				{
#ifdef BASS20
					if (!BASS_ChannelSetAttributes(gMusic + gStream, -1,
						g_mute ? 0 : g_volume, -101))
#endif
#ifdef BASS24
					if (!BASS_ChannelSetAttribute(gMusic + gStream,
						BASS_ATTRIB_VOL, g_mute ? 0 : (g_volume/100.0f)))
#endif
						goto error;
				}
			}
			Tcl_SetObjResult(interp, Tcl_NewBooleanObj(g_mute));
			break;
		}
	}

	/* Success */
	return TCL_OK;

error:
	error = BASS_ErrorGetCode();
	(void) sprintf(buf, "BASS error %ld", error);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);

	/* Failure */
	return TCL_ERROR;
}

static HINSTANCE ghModule;

BOOL WINAPI DllMain(HINSTANCE hModule, ULONG Reason, LPVOID pv)
{
	switch (Reason)
	{
		case DLL_PROCESS_ATTACH:
			ghModule = hModule;
			break;
		case DLL_PROCESS_DETACH:
//			if (gDoFree)
//				(void) BASS_Free();
			break;
	}

	return TRUE;
}

/*
 * BASS requires a window, so create a hidden one.
 */
static HWND init_window(void)
{
	HWND hWnd;
	WNDCLASS wc;

	wc.hCursor        = LoadCursor(NULL, IDC_ARROW);
	wc.hIcon          = NULL;
	wc.lpszMenuName   = NULL;
	wc.lpszClassName  = "BASSWindow";
	wc.hbrBackground  = GetStockObject(LTGRAY_BRUSH);
	wc.hInstance      = ghModule;
	wc.style          = 0;
	wc.lpfnWndProc    = DefWindowProc;
	wc.cbWndExtra     = 0;
	wc.cbClsExtra     = 0;

	if (!RegisterClass(&wc))
	{
		return NULL;
	}

	hWnd = CreateWindow("BASSWindow", "" , WS_DISABLED, 0, 0, 0, 0, NULL, NULL,
		ghModule, NULL);
	return hWnd;
}

#if 0
static void deviceVolume(DWORD *vol, BOOL set)
{
	if (set)
	{
		(void) waveOutSetVolume(0, *vol);
	}
	else
	{
		/* FIXME: What is the "default" WaveOut device? */
		(void) waveOutGetVolume(0, vol);
	}
}
#endif

static int init_BASS(void)
{
	HWND hWnd;
#if 0
	DWORD vol;
#endif
	DWORD version = BASS_GetVersion();
	char buf[128];

#ifdef BASS20
	if (version != MAKELONG(2,0))
	{
		Tcl_AppendResult(g_interp, "BASS version 2.0 required", NULL);
		return TCL_ERROR;
	}
#endif
#ifdef BASS24
	if (HIWORD(version) != MAKEWORD(4,2))
	{
		(void) sprintf(buf, "BASS version 2.4 required, version %x loaded.",
			(unsigned int)version);
		Tcl_AppendResult(g_interp, buf, NULL);
		return TCL_ERROR;
	}
#endif

	if ((hWnd = init_window()) == NULL)
	{
		char buf[128];
		(void) sprintf(buf, "couldn't create BASS window");
		Tcl_SetResult(g_interp, buf, TCL_VOLATILE);
		return TCL_ERROR;
	}

#if 0
	deviceVolume(&vol, 0);
#endif

	if (!BASS_Init(1, 44100, 0, hWnd, NULL))
	{
		DWORD error = BASS_ErrorGetCode();

		/* Hack -- sound-bass.dll may init before us */
		if (error != BASS_ERROR_ALREADY)
		{
			char buf[128];
			(void) sprintf(buf, "BASS error %ld", error);
			Tcl_SetResult(g_interp, buf, TCL_VOLATILE);
			return TCL_ERROR;
		}
	}
	else
	{
		gDoFree = TRUE;
	}

#if 0
	deviceVolume(&vol, 1);
#endif

	return TCL_OK;
}

int Music_Init(Tcl_Interp *interp)
{
#ifdef USE_TCL_STUBS
	if (Tcl_InitStubs(interp, "8.0", 0) == NULL)
	{
		return TCL_ERROR;
	}
#endif

	g_interp = interp;

	if (init_BASS() != TCL_OK) return TCL_ERROR;

	Tcl_CreateObjCommand(interp, "music", objcmd_music, NULL, NULL);

	return Tcl_PkgProvide(interp, "music", "1.0");
}
