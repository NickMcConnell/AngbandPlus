/* File: main-win.c */

/*
 * Copyright (c) 2007
 * Leon Marrick, Skirmantas Kligys, Ben Harrison
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */


/*
 * This file helps Angband work with Windows computers.  It requires a 32-bit
 * (or higher) machine, capable of displaying at least 640x480 in 16 colors,
 * running Windows 95 or NT 4.0 or better.  Most Windows emulators should
 * also work.
 *
 * To use this file, use an appropriate "Makefile" or "Project File", make
 * sure that "WINDOWS" and/or "WIN32" are defined somewhere, and obtain various
 * extra files as described below.  The Windows version has been tested to
 * compile with Visual C++ 5.0 and 6.0, Borland C++ 5.5 free command line
 * tools, Dev C++, and (probably) lcc-win32.
 *
 * The core of this port (everything except special multimedia add-ons) is
 * written in ANSI C and relies solely on WIN32 API calls available on every
 * 32-bit version of Windows.
 *
 * If you have certain external libraries, you can define USE_SDL to get
 * multiple sound channels and music.  See below for more details.
 *
 * Other files used by this port:
 * - The files "win/angband.rc" and "win/angband.ico" need to be included in your
 *   project.
 * - The game must have a collection of bitmap .fon files in /lib/xtra/font.
 *
 * - If "USE_GRAPHICS" is defined, then it also needs some bitmapped (.bmp)
 *   graphics files in /lib/xtra/graf, such as "16x16.bmp" and "16x16m.bmp".
 * - If "USE_SOUND" is defined, then some additional libraries (for example,
 *   "winmm.lib") may need to be linked in, and "*.WAV" sound files must be
 *   placed into "lib/xtra/sound/".
 *
 * - The "lib/pref/pref-win.prf" file contains keymaps, macro definitions,
 *   and/or color redefinitions.
 * - The "lib/pref/font-win.prf" contains attr/char mappings for use with the
 *   font files in "lib/xtra/font".
 *
 *
 * "Term" framework by Ben Harrison (benh@phial.com).
 *
 * "intrface" module and most code by Leon Marrick.
 *
 * Based on the Angband 3.0.6 Windows port, originally written by Skirmantas
 * Kligys (kligys@scf.usc.edu), and containing code by:
 * Ross E Becker (beckerr@cis.ohio-state.edu)
 * Chris R. Martin (crm7479@tam2000.tamu.edu)
 * Robert Ruehlmann (rr9@thangorodrim.net)
 */


/*
 * Comments on using the Win32 API with Angband:
 *
 * Basics:
 *   The Win32 API runs on any 32-bit version of Windows, which means that this
 *   port taps into what is vastly the largest potential player base for
 *   Angband (also the largest actual one according to the most recent survey,
 *   but not by so great a margin).
 *
 * - In general, the Win32 API offers very solid support for 2D applications
 *   that do not require extensive multimedia.  It's not as easy to use as more
 *   modern and/or specialized libraries, but it can and will do almost
 *   anything.  Kinda like C.  :-)
 * - Documentation is complete - fortunately, as you can't read the source! -
 *   and widely available, but getting the specific answer you need can be
 *   quite difficult due to poor searching tools in Visual C++ and the
 *   Microsoft website.  Getting a printed book or downloading several ebooks
 *   or help documents is strongly recommended.
 * - Code examples abound.  If you want to do something, you are pretty well
 *   guaranteed to find helpful code and comments.  A particularly useful
 *   website is the Code Project "www.codeproject.com".
 *
 * Efficiency:
 * - The Win32 API is highly efficient at text output, but is not so hot at
 *   graphical display.  However, the Windows port remains faster at both on
 *   Windows machines than the SDL port is on any tested system.
 * - The fancier the things you want to do, the more need you will have of
 *   additional libraries.  At present, however, *bands are a long way from
 *   needing anything like DirectX.
 *
 * Usage:
 * - That said, we are beginning to run into some limitations, ones that only
 *   using additional libaries will solve.  For example, playing music in the
 *   Win32 API is such a PITA that we use SDL instead.
 */


#include "angband.h"
#include "intrface.h"


#ifdef WINDOWS


/*
 * Include various library and support files
 */
#include <windows.h>
#include <windowsx.h> /* Defines translations, etc. */
#include <commdlg.h>
#include <shellapi.h>
#include <time.h>     /* Windows time functions */
#include <winuser.h>  /* Windows user interface functions */

/*
 * Extract the "WIN32" flag from the compiler
 */
#if defined(__WIN32__) || defined(__WINNT__) || defined(__NT__)
# ifndef WIN32
#  define WIN32
# endif
#endif



/*
 * This may need to be removed for some compilers XXX XXX XXX
 */
#ifndef _MBCS
#ifndef STRICT
#define STRICT
#endif
#endif


/*
 * Exclude parts of WINDOWS.H that are not needed
 */
#define NOCOMM            /* Comm driver APIs and definitions */
#define NOLOGERROR        /* LogError() and related definitions */
#define NOPROFILER        /* Profiler APIs */
#define NOLFILEIO         /* _l* file I/O routines */
#define NOOPENFILE        /* OpenFile and related definitions */
#define NORESOURCE        /* Resource management */
#define NOATOM            /* Atom management */
#define NOLANGUAGE        /* Character test routines */
#define NOLSTRING         /* lstr* string management routines */
#define NODBCS            /* Double-byte character set routines */
#define NOKEYBOARDINFO    /* Keyboard driver routines */
#define NOCOLOR           /* COLOR_* color values */
#define NODRAWTEXT        /* DrawText() and related definitions */
#define NOSCALABLEFONT    /* Truetype scalable font support */
#define NOMETAFILE        /* Metafile support */
#define NOSYSTEMPARAMSINFO /* SystemParametersInfo() and SPI_* definitions */
#define NODEFERWINDOWPOS  /* DeferWindowPos and related definitions */
#define NOKEYSTATES       /* MK_* message key state flags */
#define NOWH              /* SetWindowsHook and related WH_* definitions */
#define NOCLIPBOARD       /* Clipboard APIs and definitions */
#define NOICONS           /* IDI_* icon IDs */
#define NOMDI             /* MDI support */
#define NOHELP            /* Help support */

/* Not defined since it breaks Borland C++ 5.5 */
/* #define NOCTLMGR */    /* Control management and controls */

/*
 * Exclude parts of WINDOWS.H that are not needed (Win32)
 */
#define WIN32_LEAN_AND_MEAN
#define NONLS             /* All NLS defines and routines */
#define NOSERVICE         /* All Service Controller routines, SERVICE_ equates, etc. */
#define NOKANJI           /* Kanji support stuff. */
#define NOMCX             /* Modem Configuration Extensions */



#ifdef USE_SOUND

/*
 * Exclude parts of MMSYSTEM.H that are not needed
 */
#define MMNODRV          /* Installable driver support */
#define MMNOWAVE         /* Waveform support */
#define MMNOMIDI         /* MIDI support */
#define MMNOAUX          /* Auxiliary audio support */
#define MMNOTIMER        /* Timer support */
#define MMNOJOY          /* Joystick support */
#define MMNOMCI          /* MCI support */
#define MMNOMMIO         /* Multimedia file I/O support */
#define MMNOMMSYSTEM     /* General MMSYSTEM functions */

#include <mmsystem.h>

#endif /* USE_SOUND */




/*
 * "GetFileAttributes()" returns 0xFFFFFFFF on failure.
 */
#define INVALID_FILE_NAME (DWORD)0xFFFFFFFF




/*
 * If you have the development versions of SDL (Simple DirectMedia Library)
 * and SDL_mixer installed, then you can compile with multiple sound channels
 * and music support by defining both "USE_SOUND" and "USE_SDL".
 */
#ifdef USE_SDL

	/*
	 * Include the basic SDL library
	 */
	#include "SDL.h"

	/* What follows applies only if we compile with sound support */
	#ifdef USE_SOUND

		/* The mixer library is required */
		#include "SDL_mixer.h"

		/* We are now using the SDL sound/music package */
		#define USE_SDL_SOUND


		/*
		 * "Let there be two channels"
		 *
		 * On my test machine, if this value is set to more than 2,
		 * "Mix_AllocateChannels()" will allocate all the channels, but the
		 * game will crash if any channel other than #0 or #1 is opened.
		 */
		#define NUM_CHANNELS  2

		/*
		 * An array of pointers to "Mix_Chunk" data, one for each possible channel
		 */
		static Mix_Chunk *mix_chunks[NUM_CHANNELS];

		/*
		 * Global pointer to the active music track
		 */
		Mix_Music *music_track;

		/*
		 * Actual number of channels (doesn't work correctly!)
		 */
		static int max_channels = 0;

	#endif /* USE_SOUND */

#endif /* SDL_XTRAS */



/* Global handle to the (single) application window */
static HWND AppWin;


/*
 * 16 colors screen, don't use RGB()
 */
static bool colors16 = FALSE;

/*
 * Saved instance handle
 */
static HINSTANCE hInstance;



/*
 * Yellow brush for the cursor
 */
static HBRUSH hbrYellowBrush;

/*
 * An icon
 */
static HICON hIcon;

/*
 * A palette
 */
static HPALETTE hPal;

/*
 * A cursor
 */
static HCURSOR AppCursor;


/*
 * The "complex" color values
 */
static COLORREF win_clr[256];


/*
 * The "simple" color values
 *
 * See "main-ibm.c" for original table information
 *
 * The entries below are taken from the "color bits" defined above.
 *
 * Note that many of the choices below suck, but so do crappy monitors.
 */
static byte win_pal[256] =
{
	0x00,       	/* Dark */
	0x07,       	/* White */
	0x03,       	/* Slate XXX */
	0x04 | 0x08,	/* Orange XXX */
	0x04,       	/* Red */
	0x02,       	/* Green */
	0x01,       	/* Blue */
	0x06,       	/* Umber XXX */
	0x00 | 0x08,	/* Light Dark */
	0x03 | 0x08,	/* Light Slate XXX */
	0x05,       	/* Violet XXX */
	0x06 | 0x08,	/* Yellow */
	0x05 | 0x08,	/* Light Red XXX */
	0x02 | 0x08,	/* Light Green */
	0x01 | 0x08,	/* Light Blue */
	0x06        	/* Light Umber XXX */
};




#ifdef SUPPORT_GAMMA
int gamma_correction;
#endif /* SUPPORT_GAMMA */



/*
 * Hack -- define which keys are "special" and which should be ignored
 */
static bool special_key[256];
static bool ignore_key[256];


/*
 * Hack -- initialization list for "special_key"
 */
static byte special_key_list[] =
{
	VK_CLEAR, VK_PAUSE, VK_CAPITAL, VK_KANA, VK_JUNJA, VK_FINAL, VK_KANJI,
	VK_CONVERT, VK_NONCONVERT, VK_ACCEPT, VK_MODECHANGE,
	VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN,
	VK_SELECT, VK_PRINT, VK_EXECUTE, VK_SNAPSHOT, VK_INSERT, VK_DELETE,
	VK_HELP, VK_APPS,
	VK_F1, VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8, VK_F9, VK_F10,
	VK_F11, VK_F12, VK_F13, VK_F14, VK_F15, VK_F16, VK_F17, VK_F18, VK_F19, VK_F20,
	VK_F21, VK_F22, VK_F23, VK_F24, VK_NUMLOCK, VK_SCROLL,
	VK_ATTN, VK_CRSEL, VK_EXSEL, VK_EREOF, VK_PLAY, VK_ZOOM, VK_NONAME,
	VK_PA1, 0
};

static byte ignore_key_list[] =
{
	VK_ESCAPE, VK_TAB, VK_SPACE,
	VK_SHIFT, VK_CONTROL, VK_MENU, VK_LWIN, VK_RWIN,
	VK_LSHIFT, VK_RSHIFT, VK_LCONTROL, VK_RCONTROL, VK_LMENU, VK_RMENU,
	MOUSEKEY, 0
};


/*
 * Include the support for loading bitmaps
 */
#ifdef USE_GRAPHICS
# include "win/readdib.h"

/*
 * The global bitmap
 */
static DIBINIT infGraph;

/*
 * The global bitmap mask
 */
static DIBINIT infMask;

/*
 * Handles to memory device contexts that store the graphics and mask bitmaps.
 * Created and destroyed in "init_graphics()".
 */
static HDC hdcMemGraphics;
static HDC hdcMemMask;


#endif /* USE_GRAPHICS */

#if 0
/*
 * This function is used for debugging.  It should be called immediately
 * after the Windows API call you wish to test.
 */
static void show_win_error(LPTSTR s)
{
	TCHAR error_str[160];
	TCHAR buf[160];
	DWORD last_error = GetLastError();

	/* We have an non-zero error */
	if (last_error)
	{
		FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
					  NULL, last_error,
					  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
					  error_str, 0, NULL);

		wsprintf(buf, "%s failed with error %d: %s", s, last_error, error_str);

		MessageBox(NULL, buf, "Error", MB_OK | MB_ICONINFORMATION);
	}
}
#endif

/*
 * Check for existance of a file.  We must supply the full path.
 */
static bool check_file(cptr s)
{
	char path[1024];

	DWORD attrib;

	/* Copy it */
	(void)my_strcpy(path, s, sizeof(path));

	/* Examine */
	attrib = GetFileAttributes(path);

	/* Require valid filename */
	if (attrib == INVALID_FILE_NAME) return (FALSE);

	/* Prohibit directory */
	if (attrib & FILE_ATTRIBUTE_DIRECTORY) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Check for existance of a directory
 */
static bool check_dir(cptr s)
{
	int i;

	char path[1024];

	DWORD attrib;

	/* Copy it */
	(void)my_strcpy(path, s, sizeof(path));

	/* Check length */
	i = strlen(path);

	/* Remove trailing backslash */
	if (i && (path[i-1] == '\\')) path[--i] = '\0';

	/* Examine */
	attrib = GetFileAttributes(path);

	/* Require valid filename */
	if (attrib == INVALID_FILE_NAME) return (FALSE);

	/* Require directory */
	if (!(attrib & FILE_ATTRIBUTE_DIRECTORY)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Validate a file
 */
static void validate_file(cptr s)
{
	/* Verify or fail */
	if (!check_file(s))
	{
		quit_fmt("Cannot find required file:\n%s", s);
	}
}


/*
 * Validate a directory
 */
static void validate_dir(cptr s)
{
	/* Verify or fail */
	if (!check_dir(s))
	{
		quit_fmt("Cannot find required directory:\n%s", s);
	}
}



/*
 * Release a font and all of its associated data, except for the string
 * "font_want".  -LM-
 */
static void remove_font_win(window_type *win_ptr)
{
	char buf[1024];

	/* Build the path name */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, win_ptr->font_file);

	/* Remove the font from the system */
	(void)RemoveFontResource(buf);

	/* Free the font from memory */
	DeleteObject(win_ptr->font_id);

	/* Free the old name */
	(void)string_free(win_ptr->font_file);

	/* Forget it */
	win_ptr->font_file = NULL;

	/* Notify other applications that a font is no longer available  */
	PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
}


/*
 * Change the font used in a Term window.
 *
 * This function returns zero only if everything succeeds.
 */
static errr change_font_win(window_type *win_ptr, char *path)
{
	int wid, hgt;

	char *base;


	/* No path given */
	if (!path) return (1);

	/* Analyze font path, get width and height */
	base = analyze_font(path, &wid, &hgt);

	/* Verify file */
	if (!check_file(path)) return (1);


	/* The old font is active */
	if (win_ptr->font_file)
	{
		/* Remove old font from the system, free system resources */
		remove_font_win(win_ptr);
	}

	/* Load the new font */
	if (!AddFontResource(path)) return (1);

	/* Save new font name */
	win_ptr->font_file = string_make(base);

	/* Remove the "suffix" */
	base[strlen(base)-4] = '\0';

	/* Create the font (using the base name) */
	win_ptr->font_id = CreateFont(hgt, wid, 0, 0, FW_DONTCARE, 0, 0, 0,
	                              ANSI_CHARSET, OUT_DEFAULT_PRECIS,
	                              CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
	                              FIXED_PITCH | FF_DONTCARE, base);


	/* We were unable to determine the font size */
	if (!wid || !hgt)
	{
		HDC hdc;
		HFONT hfOld;
		TEXTMETRIC tm;

		/* Access the application window device context */
		hdc = GetDC(AppWin);

		/* Replace old font with our new font, save old */
		hfOld = (HFONT) SelectObject(hdc, win_ptr->font_id);

		/* Get data on our font */
		GetTextMetrics(hdc, &tm);

		/* Restore the old font */
		SelectObject(hdc, hfOld);

		/* Release the device context */
		ReleaseDC(AppWin, hdc);

		/* Store the font width and height */
		wid = tm.tmAveCharWidth;
		hgt = tm.tmHeight;
	}

	/* Save the font size */
	win_ptr->font_wid = wid;
	win_ptr->font_hgt = hgt;

	/* Usually, also change the tile size */
	if ((win_ptr->Term_idx != TERM_MAP) || (!mode_is_graphical(arg_graphics)))
	{
		win_ptr->tile_wid = wid;
		win_ptr->tile_hgt = hgt;
	}

	/* If we are the main term window, recalculate the map term window */
	if (win_ptr->Term_idx == TERM_MAIN) calc_map_display();

	/* Success */
	return (0);
}

/*
 * Allow the user to change the font for this window.
 */
static void window_change_font_aux(window_type *win_ptr)
{
	OPENFILENAME ofn;

	char tmp[1024] = "";

	/* Extract a default if possible */
	if (win_ptr->font_file) strcpy(tmp, win_ptr->font_file);

	/* Ask for a choice */
	memset(&ofn, 0, sizeof(ofn));
	ofn.lStructSize = sizeof(ofn);
	ofn.hwndOwner = AppWin;
	ofn.lpstrFilter = "Font Files (*.fon)\0*.fon\0\0";
	ofn.nFilterIndex = 1;
	ofn.lpstrFile = tmp;
	ofn.nMaxFile = 1024;
	ofn.lpstrInitialDir = ANGBAND_DIR_XTRA_FONT;
	ofn.Flags = OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;
	ofn.lpstrDefExt = "fon";

	/* Force choice if legal */
	if (GetOpenFileName(&ofn))
	{
		/* Use the font */
		if (change_font_win(win_ptr, tmp))
		{
			/* On failure, access the standard font file */
			(void)path_build(tmp, sizeof(tmp), ANGBAND_DIR_XTRA_FONT, DEFAULT_GAME_FONT);

			/* Use the standard font */
			(void)change_font_win(win_ptr, tmp);
		}
	}
}

/*
 * Change a window font, verify dimensions.
 */
static void window_change_font_win(int idx, int cursor_y)
{
	/* Get window */
	window_type *win_ptr = &window_settings[idx];

	char old_font[80];
	int cols, rows;

	/* Calculate available space */
	int window_wid = (win_ptr->window_wid - win_ptr->border_left - win_ptr->border_right);
	int window_hgt = (win_ptr->window_hgt - win_ptr->border_top - win_ptr->border_bottom);


	/* Save the current font */
	(void)my_strcpy(old_font, win_ptr->font_file, sizeof(old_font));

	/* Change the font (using the system interface */
	window_change_font_aux(win_ptr);

	/* Get rows and columns */
	cols = window_wid / win_ptr->font_wid;
	rows = window_hgt / win_ptr->font_hgt;

	/* Save rows and cols, ensuring that they are never greater than 255 */
	win_ptr->cols = MIN(cols, 255);
	win_ptr->rows = MIN(rows, 255);

	/* Verify rows and columns */
	if ((win_ptr->cols < term_size_min[idx][0]) ||
		(win_ptr->rows < term_size_min[idx][1]))
	{
		char path[1024];

		/* Move cursor */
		(void)Term_gotoxy(7, cursor_y);

		/* Notice problem */
		c_roff(TERM_YELLOW, format("ERROR:  The chosen font is too large for this window and not enough rows and/or columns can be displayed.  (showing %d x %d, need %d x %d).  Previous font restored.",
			win_ptr->cols, win_ptr->rows, term_size_min[idx][0], term_size_min[idx][1]), 2, 78);

		(void)inkey(ALLOW_CLICK);

		/* Access the previous font file */
		(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA_FONT, old_font);

		/* Use it */
		(void)change_font_win(win_ptr, path);

		/* Get rows and columns */
		cols = window_wid / win_ptr->tile_wid;
		rows = window_hgt / win_ptr->tile_hgt;

		/* Save rows and cols, ensuring that they are never greater than 255 */
		win_ptr->cols = MIN(cols, 255);
		win_ptr->rows = MIN(rows, 255);
	}
}




/* Never request more than 236 colors */
#define PAL_SIZE    236


/*
 * If the display is paletted (256 colors), we need to update the system
 * palette if colors change.
 *
 * If we are not using graphics, we simply include all the text colors in our
 * requested palette.  If we are using graphics, we load the first 236
 * (256, less 20 required by the system) colors requested in the bitmap
 * palette, and then attempt to match all the text colors as best we can.  -LM-
 *
 * ERROR:  We don't know how to make masked graphics work in 256 color mode,
 * and so therefore delete any mask.  XXX XXX XXX
 *
 * This function is never called before all windows are ready.
 */
static int new_palette(void)
{
#ifdef USE_GRAPHICS
	HPALETTE hBmPal;
#endif /* USE_GRAPHICS */
	HPALETTE hNewPal;
	HDC hdc;
	int i, j;
	int pLogPalSize;
	LPLOGPALETTE pLogPal;
	LPPALETTEENTRY p;
	int clr_red, clr_green, clr_blue;


	/* The screen is not paletted -- no need to do anything */
	if (!paletted) return (TRUE);

	/* Size of palette */
	pLogPalSize = sizeof(LOGPALETTE) + PAL_SIZE * sizeof(PALETTEENTRY);

	/* Allocate palette */
	pLogPal = (LPLOGPALETTE)ralloc(pLogPalSize);

	/* Wipe palette */
	for (i = 0; i < PAL_SIZE; i++) WIPE(&pLogPal->palPalEntry[i], PALETTEENTRY);

	/* Version  XXX XXX */
	pLogPal->palVersion = 0x300;

	/* Note palette size */
	pLogPal->palNumEntries = PAL_SIZE;


#ifdef USE_GRAPHICS

	/* Check the bitmap palette */
	hBmPal = infGraph.hPalette;

	/* Use the bitmap */
	if (hBmPal)
	{
		/* Allocate an array of palette entries */
		LPPALETTEENTRY lppe = ralloc(PAL_SIZE * sizeof(PALETTEENTRY));

		/* Grab the first 236 entries from the current graphics palette */
		int nEntries = GetPaletteEntries(hBmPal, 0, PAL_SIZE, lppe);

		/* Save them into the requested palette */
		for (i = 0; i < nEntries; i++)
		{
			pLogPal->palPalEntry[i] = lppe[i];
			pLogPal->palPalEntry[i].peFlags = 0;
		}

		/* Free the palette entry */
		free(lppe);
	}

	/* ERROR -- We don't know how to make masked graphics work in 256 color mode  XXX XXX XXX */
	if (hdcMemMask)
	{
		DeleteDC(hdcMemMask);
		hdcMemMask = NULL;
	}


#endif /* USE_GRAPHICS */


	/* Save the game colors into the new palette as best we can */
	for (i = 0; i < MAX_COLORS; i++)
	{
		long delta = 1000000L;
		int drv, dgv, dbv;

		int best_match = -1;
		bool black_flag = FALSE;

		/* This text color has no index -- skip */
		if (!color_table[i].index_char) continue;

		/* Get RGB values for the current game color */
		clr_red = color_table[i].rv;
		clr_green = color_table[i].gv;
		clr_blue = color_table[i].bv;


		/* Scan the requested palette */
		for (j = 0; j < PAL_SIZE; j++)
		{
			/* Point to this palette entry */
			p = &(pLogPal->palPalEntry[j]);

			/* The first black entry must be requested; the others are free for use */
			if ((!p->peRed) && (!p->peGreen) && (!p->peBlue))
			{
				/* We already skipped a black entry, or are requesting black */
				if ((black_flag) || (i == TERM_DARK))
				{
					/* Accept immediately */
					best_match = j;
					break;
				}

				/* Otherwise, skip the first black */
				else
				{
					black_flag = TRUE;
					continue;
				}
			}

			/* Skip any palette entries already used */
			if (p->peFlags & (PC_NOCOLLAPSE)) continue;

			/* Get difference in RGB values */
			drv = ABS(p->peRed - clr_red);
			dgv = ABS(p->peGreen - clr_green);
			dbv = ABS(p->peBlue - clr_blue);

			/* If squared RGB difference is less, remember this color */
			if (delta > (long)drv * drv + dgv * dgv + dbv * dbv)
			{
				delta = (long)drv * drv + dgv * dgv + dbv * dbv;
				best_match = j;
			}

			/* Accept exact matches immediately */
			if (!delta) break;
		}

		/* We have a legal match */
		if (best_match >= 0)
		{
			/* Point to this palette entry */
			p = &(pLogPal->palPalEntry[best_match]);

			/* Save the game color (overwrite any bitmap palette entry here  XXX) */
			p->peRed = clr_red;
			p->peGreen = clr_green;
			p->peBlue = clr_blue;

#ifdef SUPPORT_GAMMA

			/* Gamma-correct the colors (this may not work...) */
			if (gamma_correction > 0)
			{
				p->peRed = gamma_table[p->peRed];
				p->peGreen = gamma_table[p->peGreen];
				p->peBlue = gamma_table[p->peBlue];
			}

#endif /* SUPPORT_GAMMA */

			/* Do not overwrite this color any more */
			p->peFlags = PC_NOCOLLAPSE;
		}
	}

	/* Do not let the system switch or substitute any of the colors */
	for (i = 0; i < PAL_SIZE; i++) pLogPal->palPalEntry[i].peFlags = PC_NOCOLLAPSE;


	/* Create a new palette, or fail */
	hNewPal = CreatePalette(pLogPal);
	if (!hNewPal) quit("Cannot create palette!");

	/* Free the temporary data */
	free(pLogPal);


	/* Realize the palette */
	hdc = GetDC(AppWin);
	SelectPalette(hdc, hNewPal, 0);
	i = RealizePalette(hdc);
	ReleaseDC(AppWin, hdc);
	if (i == 0) quit("Cannot realize palette!");

	/* Update application colors */
	(void)UpdateColors(hdc);

	/* Delete old palette */
	if (hPal) DeleteObject(hPal);

	/* Save new palette */
	hPal = hNewPal;

	/* Success */
	return (TRUE);
}


/*
 * Initialize graphics (for the map window)
 *
 * Return TRUE if we have changed the graphics mode, FALSE if not.
 */
static bool init_graphics_win(bool verify_map)
{
#ifdef USE_GRAPHICS
	char buf[1024];
	int wid, hgt;
	int i;
	bool use_mask;

	char tmp[40];
	char name[80];
	char mask[80];

	HDC hdc;

	window_type *win_ptr = &window_settings[use_special_map ? TERM_MAP : TERM_MAIN];

	/* Save the old graphics and mask (if any) */
	DIBINIT infGraph_old = infGraph;
	DIBINIT infMask_old = infMask;


	/* We only allow bitmap graphics if the special map display is available */
	if ((!use_special_map) && (mode_is_graphical(arg_graphics))) return (FALSE);


	/* Release the memnory device context that contains the graphics bitmap (if any) */
	if (hdcMemGraphics)
	{
		DeleteDC(hdcMemGraphics);
		hdcMemGraphics = NULL;
	}

	/* Release the mask (if any) */
	if (hdcMemMask)
	{
		DeleteDC(hdcMemMask);
		hdcMemMask = NULL;
	}

	/* Free the bitmap stuff */
	FreeDIB(&infGraph);
	FreeDIB(&infMask);

	/* We usually use "Term_pict" only for high-bit attr/char pairs */
	if (term_map)
	{
		term_map->always_pict = FALSE;
		term_map->higher_pict = TRUE;
	}


	/* We are requesting a non-graphical mode */
	if (!mode_is_graphical(arg_graphics))
	{
		/* Get the font name */
		(void)my_strcpy(buf, win_ptr->font_file, sizeof(buf));

		/* Use the font to determine cell size */
		(void)analyze_font(buf, &win_ptr->tile_wid, &win_ptr->tile_hgt);

		/* Nothing more to do */
		return (TRUE);
	}


	/* Get tile width and height */
	wid = graphics_data[arg_graphics].tile_wid;
	hgt = graphics_data[arg_graphics].tile_hgt;

	/* Verify that enough rows and columns will be present (but only if ready) */
	if ((verify_map) && (wid) && (hgt))
	{
		if ((((win_ptr->window_wid - win_ptr->border_left - win_ptr->border_right) / wid) <
			term_size_min[win_ptr->Term_idx][0]) ||
			(((win_ptr->window_hgt - win_ptr->border_top - win_ptr->border_bottom) / hgt) <
			term_size_min[win_ptr->Term_idx][1]))
		{
			/* Complain */
			plog("Sorry, the graphics tiles requested are to large to fit in the map window.");

			/* Cancel if not enough space */
			infGraph = infGraph_old;
			infMask = infMask_old;

			return (FALSE);
		}
	}

	/* Get the graphics file name */
	(void)my_strcpy(tmp, graphics_data[arg_graphics].file_name, 40);

	/* HACK -- ignore any part of the name starting with a '-'  XXX XXX */
	for (i = 0; i < 40; i++)
	{
		if (!tmp[i]) break;
		if (tmp[i] == '-')
		{
			tmp[i] = '\0';
			break;
		}
	}

	/* Determine if these graphics use a mask */
	use_mask = (mode_is_masked(arg_graphics)) ? TRUE : FALSE;


	/* Build the names of the graphics and (optionally) the mask files */
	strcpy(name, format("%s.bmp", tmp));
	if (use_mask) strcpy(mask, format("%sm.bmp", tmp));
	else          mask[0] = '\0';

	/* Build the bitmap file path */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_GRAF, name);

	/* Load the bitmap */
	if (!ReadDIB(AppWin, buf, &infGraph))
	{
		/* Complain, use old graphics */
		plog_fmt("Cannot read bitmap file '%s'", buf);

		infGraph = infGraph_old;
		infMask = infMask_old;

		return (FALSE);
	}

	/* These graphics use a mask */
	if (use_mask)
	{
		/* Build the mask file path */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_GRAF, mask);

		/* Load the mask */
		if (!ReadDIB(AppWin, buf, &infMask))
		{
			/* Free the graphics */
			FreeDIB(&infGraph);

			/* Complain, use old graphics */
			plog_fmt("Cannot read mask file '%s'", buf);

			infGraph = infGraph_old;
			infMask = infMask_old;

			return (FALSE);
		}
	}

	/* Activate a palette */
	if (!new_palette())
	{
		/* Free both graphics and mask */
		FreeDIB(&infGraph);
		FreeDIB(&infMask);

		/* Complain, use old graphics */
		plog("Cannot activate palette!");

		infGraph = infGraph_old;
		infMask = infMask_old;

		return (FALSE);
	}


	/* Access the application window device context */
	hdc = GetDC(AppWin);


	/* Get a memory device context */
	hdcMemGraphics = CreateCompatibleDC(hdc);

	/* Select bitmap into it */
	(void)SelectObject(hdcMemGraphics, infGraph.hBitmap);

	/* If we are using a mask, do the same thing for the mask bitmap */
	if (use_mask)
	{
		hdcMemMask = CreateCompatibleDC(hdc);
		(void)SelectObject(hdcMemMask, infMask.hBitmap);
	}

	/* Set the correct mode for stretching the tiles */
	if (colors16 || (media_quality_level != QUALITY_HIGH))
	{
		/* Simple method -- overwrite lines we don't have space for */
		(void)SetStretchBltMode(hdc, COLORONCOLOR);
	}
	else
	{
		/* Complex method -- average out adjacent lines */
		(void)SetStretchBltMode(hdc, HALFTONE);  /* This is a much slower method */
		(void)SetBrushOrgEx(hdc, 0, 0, NULL);
	}

	/* Release the device context */
	ReleaseDC(AppWin, hdc);


	/* Save the new sizes in both the global bitmap and the map window */
	window_settings[TERM_MAP].tile_wid = infGraph.CellWidth  = wid;
	window_settings[TERM_MAP].tile_hgt = infGraph.CellHeight = hgt;

	/* If the graphics are masked, then the map display should always use "Term_pict". */
	if ((hdcMemMask) && (term_map))
	{
		term_map->always_pict = TRUE;
		term_map->higher_pict = FALSE;
	}

	/* Automatically use the largest available font that will fit */
	(void)use_largest_font(win_ptr, wid, hgt);


#else /* USE_GRAPHICS */

	/* If we do not have graphics defined, cancel any graphics request */
	if (mode_is_graphical(arg_graphics))
	{
		use_graphics = arg_graphics = GRAPHICS_FONT;
	}

#endif /* USE_GRAPHICS */

	/* Result */
	return (TRUE);
}


/*
 * React to global changes
 */
static errr Term_xtra_win_react(void)
{
	int i;

	window_type *win_ptr;
	term *old = Term;


	/* Simple color */
	if (colors16)
	{
		/* Save the default colors */
		for (i = 0; i < MAX_COLORS; i++)
		{
			/* Simply accept the desired colors */
			win_pal[i] = color_table[i].kv;
		}
	}

	/* Complex color */
	else
	{
		COLORREF code;

		byte rv, gv, bv;

		bool change = FALSE;

		/* Save the default colors */
		for (i = 0; i < MAX_COLORS; i++)
		{
			/* Extract desired values */
			rv = color_table[i].rv;
			gv = color_table[i].gv;
			bv = color_table[i].bv;

#ifdef SUPPORT_GAMMA

			if (gamma_correction > 0)
			{
				rv = gamma_table[rv];
				gv = gamma_table[gv];
				bv = gamma_table[bv];
			}

#endif /* SUPPORT_GAMMA */

			/* Extract a full color code */
			code = PALETTERGB(rv, gv, bv);

			/* Activate changes */
			if (win_clr[i] != code)
			{
				/* Note the change */
				change = TRUE;

				/* Apply the desired color */
				win_clr[i] = code;
			}
		}

		/* Activate the palette if needed */
		if (change) (void)new_palette();
	}


#ifdef USE_SOUND

	/* The sound options have changed */
	if (use_sound != arg_sound)
	{
		/* Request sound */
		if ((arg_sound == SOUND_ONLY) || (arg_sound == SOUND_AND_MUSIC))
		{
			/* Already done */
			if (sounds_ready)
			{
				/* No need to do anything */
			}

			/* Load the prefs, activate sound if possible */
			else if (!load_sound_prefs())
			{
				if (arg_sound == SOUND_ONLY) arg_sound = SOUND_NONE;
				if (arg_sound == SOUND_AND_MUSIC) arg_sound = MUSIC_ONLY;
				plog("Cannot initialize sound!");
			}
		}

		/* Request music */
		if ((arg_sound == MUSIC_ONLY) || (arg_sound == SOUND_AND_MUSIC))
		{
			/* Already done */
			if (music_ready)
			{
				/* No need to do anything */
			}

			/* Load the prefs, activate music if possible */
			else if (!load_music_prefs())
			{
				if (arg_sound == MUSIC_ONLY) arg_sound = SOUND_NONE;
				if (arg_sound == SOUND_AND_MUSIC) arg_sound = SOUND_ONLY;
				plog("Cannot initialize music!");
			}
		}
#if defined(USE_SOUND) && defined(USE_SDL)
		/* Turn off music */
		if ((arg_sound != MUSIC_ONLY) && (arg_sound != SOUND_AND_MUSIC))
		{
			/* Shut down the current music, if any */
			if (music_track)
			{
				if (use_sound) Mix_FadeOutMusic(0);
				Mix_FreeMusic(music_track);
			}
		}
#endif
		/* Change setting */
		use_sound = arg_sound;
	}

#endif /* USE_SOUND */


#ifdef USE_GRAPHICS

	/* Handle "arg_graphics" */
	if (use_graphics != arg_graphics)
	{
		/* Change the graphics mode */
		if (init_graphics_win(TRUE)) use_graphics = arg_graphics;

		/* On failure, cancel the request */
		else                         arg_graphics = use_graphics;
	}

#endif /* USE_GRAPHICS */


	/* Clean up sub-windows */
	for (i = TERM_SUBWINDOW; i < TERM_MAX; i++)
	{
		win_ptr = &window_settings[i];

		/* Window must be visible */
		if (!win_ptr->visible) continue;

		/* Update resized windows */
		if ((win_ptr->cols != term_data[win_ptr->Term_idx].cols) ||
		    (win_ptr->rows != term_data[win_ptr->Term_idx].rows))
		{
			/* Activate */
			(void)Term_activate(angband_term[i]);

			/* Resize the term */
			(void)Term_resize(win_ptr->cols, win_ptr->rows);

			/* Redraw the contents */
			(void)Term_redraw();
		}
	}

	/* Point to the standard or tall display */
	win_ptr = &window_settings[use_tall_display ? WINDOW_DISPLAY : TERM_MAIN];

	/* Update resized windows */
	if ((win_ptr->cols != term_data[win_ptr->Term_idx].cols) ||
		(win_ptr->rows != term_data[win_ptr->Term_idx].rows))
	{
		/* Activate */
		(void)Term_activate(term_main);

		/* Resize the term */
		(void)Term_resize(win_ptr->cols, win_ptr->rows);

		/* Redraw the contents */
		(void)Term_redraw();
	}

	/* Re-calculate the special map (always, if present) */
	if (use_special_map)
	{
		/* Re-calculate the special map */
		calc_map_display();

		/* Refresh map display (if the main screen is active) */
		if (!main_screen_inactive)
		{
			/* Activate the map display */
			(void)Term_activate(term_map);

			/* Redraw its contents */
			(void)Term_redraw();
		}
	}

	/* Toggle window active status (all standard windows) */
	for (i = 0; i < TERM_MAX; i++)
	{
		/* No term, or term is unavailable */
		if (!angband_term[i]) continue;

		/* Update its mapped flag based on window visiblity */
		angband_term[i]->mapped_flag = (window_settings[i].visible);
	}

	/* Show the cursor only if it does anything */
	SetCursor(use_mouse ? AppCursor : NULL);

	/* Restore old term */
	(void)Term_activate(old);

	/* Success */
	return (0);
}







/*
 * Functions needed by the "Term" code
 */



/*
 * Given a position in the ISO Latin-1 character set, return
 * the correct character on this system.
 */
static byte Term_xchar_win(byte c)
{
	/* The Windows port uses the Latin-1 standard */
	return (c);
}


/*
 * Draw a cursor at (col, row), using a bitmapped cursor shape, a yellow
 * box, or a thick underline.
 */
static errr Term_curs_win(int col, int row)
{
	window_type *win_ptr = (window_type*)(Term->data);

	srect rc;
	HDC hdc;

	/* Get left-top corner of window */
	int x0 = win_ptr->window_left + win_ptr->border_left;
	int y0 = win_ptr->window_top + win_ptr->border_top;


	/* Access the (whole) application window */
	hdc = GetDC(AppWin);

	/* We are using masked graphics, this is the map term, and the cursor is graphical */
	if ((hdcMemMask) && (win_ptr->Term_idx == TERM_MAP) &&
	    (misc_graphics_info[PICT_CURSOR][0] & 0x80))
	{
		/* Get bitmap cell size */
		int cell_wid = graphics_data[use_graphics].tile_wid;
		int cell_hgt = graphics_data[use_graphics].tile_hgt;

		/* Get position of cursor graphic within the bitmap */
		int graphic_x = (misc_graphics_info[PICT_CURSOR][1] & 0x7F) * cell_wid;
		int graphic_y = (misc_graphics_info[PICT_CURSOR][0] & 0x7F) * cell_hgt;

		/* Get cursor grid left-top corner */
		rc.left = x0 + (col * win_ptr->tile_wid);
		rc.top  = y0 + (row * win_ptr->tile_hgt);

		/* No need to stretch */
		if ((win_ptr->tile_wid == cell_wid) && (win_ptr->tile_hgt == cell_hgt))
		{
			/* If using a mask, mask out the tile */
			if (hdcMemMask)
			{
				(void)BitBlt(hdc, rc.left, rc.top, win_ptr->tile_wid, win_ptr->tile_hgt,
					hdcMemMask, graphic_x, graphic_y, SRCAND);
			}

			/* Copy from the bitmap to the window */
			(void)BitBlt(hdc, rc.left, rc.top, win_ptr->tile_wid, win_ptr->tile_hgt,
				hdcMemGraphics, graphic_x, graphic_y, SRCPAINT);
		}

		/* Need to stretch */
		else
		{
			/* If using a mask, mask out the tile */
			if (hdcMemGraphics)
			{
				(void)StretchBlt(hdc, rc.left, rc.top, win_ptr->tile_wid, win_ptr->tile_hgt,
					hdcMemMask, graphic_x, graphic_y, cell_wid, cell_hgt, SRCAND);
			}

			/* Copy and stretch from the bitmap to the window */
			(void)StretchBlt(hdc, rc.left, rc.top, win_ptr->tile_wid, win_ptr->tile_hgt,
				hdcMemGraphics, graphic_x, graphic_y, cell_wid, cell_hgt, SRCCOPY);
		}

		/* Release the application window device context */
		ReleaseDC(AppWin, hdc);

		/* Done */
		return (0);
	}


	/* Otherwise, paint either a box or a thick underline */
	if ((cursor_shape) || (win_ptr->Term_idx == TERM_MAP))
	{
		/* Frame the grid */
		rc.left   = x0 + (col * win_ptr->tile_wid);
		rc.top    = y0 + (row * win_ptr->tile_hgt);
		rc.right  = rc.left + win_ptr->tile_wid;
		rc.bottom = rc.top + win_ptr->tile_hgt;
	}
	else
	{
		int thickness = (win_ptr->tile_hgt >= 12) ? 2 : 1;

		/* Thick underline */
		rc.left   = x0 + (col * win_ptr->tile_wid);
		rc.top    = y0 + ((row+1) * win_ptr->tile_hgt) - thickness;
		rc.right  = rc.left + win_ptr->tile_wid;
		rc.bottom = rc.top + thickness;
	}

	/* Draw a rectangular border */
	FrameRect(hdc, (RECT *)&rc, hbrYellowBrush);

	/* Release the application window device context */
	ReleaseDC(AppWin, hdc);

	/* Success */
	return (0);
}


/*
 * Clear a terminal window
 */
static errr Term_xtra_win_clear(void)
{
	window_type *win_ptr = (window_type*)(Term->data);

	HDC hdc;
	RECT rc;

	/* We do clear non-visible windows */

	/* If window is visible, notify of possible overlap */
	if (win_ptr->visible) overlap_notify(0, 0, 999, 999);

	/* Select the entire terminal window */
	rc.left   = win_ptr->window_left + win_ptr->border_left;
	rc.top    = win_ptr->window_top + win_ptr->border_top;
	rc.right  = rc.left + win_ptr->window_wid - win_ptr->border_left - win_ptr->border_right;
	rc.bottom = rc.top  + win_ptr->window_hgt - win_ptr->border_top - win_ptr->border_bottom;

	/* Get the application window drawing surface (client area only) */
	hdc = GetDC(AppWin);

	/* Black background */
	SetBkColor(hdc, RGB(0, 0, 0));

	/* Select the current font */
	SelectObject(hdc, win_ptr->font_id);

	/* Print a huge black space (efficiently) */
	ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);

	/* Release the application window device context */
	ReleaseDC(AppWin, hdc);

	/* Success */
	return (0);
}


/*
 * Erase a "block" of "n" characters starting at (col, row).
 */
static errr Term_wipe_win(int col, int row, int n)
{
	window_type *win_ptr = (window_type*)(Term->data);

	HDC hdc;
	RECT rc;

	int x0 = win_ptr->window_left + win_ptr->border_left;
	int y0 = win_ptr->window_top  + win_ptr->border_top;


	/* Do nothing unless this window is visible */
	if (!win_ptr->visible) return (0);

	/* Store redraw area (if necessary) */
	overlap_notify(col, row, col + n - 1, row);

	/* Rectangle to erase in client coords */
	rc.left   = x0 + (col * win_ptr->tile_wid);
	rc.top    = y0 + (row * win_ptr->tile_hgt);
	rc.right  = rc.left + (n * win_ptr->tile_wid);
	rc.bottom = rc.top + win_ptr->tile_hgt;

	/* Get the application window drawing surface (client area only) */
	hdc = GetDC(AppWin);

	/* Black background */
	SetBkColor(hdc, RGB(0, 0, 0));

	/* Select the current font */
	SelectObject(hdc, win_ptr->font_id);

	/* Print out some blank spaces (efficiently) */
	ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);

	/* Release the application window device context */
	ReleaseDC(AppWin, hdc);

	/* Success */
	return (0);
}


/*
 * Low level text output.  Assumes valid input.
 *
 * Draw several ("n") chars, with an attr, at a given location.  Characters
 * are drawn using the current text alignment (always left bottom) and with
 * opaque backgrounds.  -LM-
 */
static errr Term_text_win(int col, int row, int n, byte a, cptr s)
{
	window_type *win_ptr = (window_type*)(Term->data);
	HDC hdc;
	int x0, y0;

	/* Remember errors */
	errr error;


	/* Do nothing unless this window is visible */
	if (!win_ptr->visible) return (0);

	/* Store redraw area (if necessary) */
	overlap_notify(col, row, col + n - 1, row);

	/* Get the application window drawing surface (client area only) */
	hdc = GetDC(AppWin);

	/* Get the left-top corner */
	x0 = win_ptr->window_left + win_ptr->border_left + (col * win_ptr->tile_wid);
	y0 = win_ptr->window_top  + win_ptr->border_top  + (row * win_ptr->tile_hgt);

	/* Foreground color: 16, 256 (paletted), or full */
	if (colors16) SetTextColor(hdc, PALETTEINDEX(win_pal[a]));
	else          SetTextColor(hdc, win_clr[a]);

	/* Use this term window's font */
	SelectObject(hdc, win_ptr->font_id);

	/* Background mode - opaque by default */
	SetBkMode(hdc, OPAQUE);

	/* Background color - always black */
	SetBkColor(hdc, RGB(0, 0, 0));


	/* Our font size matches our tile size */
	if ((win_ptr->font_wid == win_ptr->tile_wid) && (win_ptr->font_hgt == win_ptr->tile_hgt))
	{
		/* Print the text, note errors */
		error = (TextOut(hdc, x0, y0, s, n) == 0) ? 1 : 0;
	}

	/* The two do not match; we need to get more rigorous (and less efficient) */
	else
	{
		UINT fuOptions = 0;
		INT *lpDx;
		RECT rc;
		int i;

		/* Fill in the clipping rectangle */
		rc.left   = x0;
		rc.top    = y0;
		rc.right  = x0 + win_ptr->tile_wid;
		rc.bottom = y0 + win_ptr->tile_hgt;

		/* If the font size exceeds the tile size, we use clipping */
		if ((win_ptr->font_wid > win_ptr->tile_wid) || (win_ptr->font_hgt > win_ptr->tile_hgt))
			fuOptions |= (ETO_CLIPPED);

		/* Create a spacing array */
		C_MAKE(lpDx, n, INT);

		/* Characters are spaced one tile width apart */
		for (i = 0; i < n; i++) lpDx[i] = win_ptr->tile_wid;

		/* Print the text, note errors */
		error = ExtTextOut(hdc, x0, y0, fuOptions, &rc, s, n, lpDx);

		/* Kill the spacing array */
		C_KILL(lpDx, n, INT);
	}


	/* Release application window device context */
	ReleaseDC(AppWin, hdc);

	/* Note success or failure */
	return (error);
}


/*
 * Helper function that prints a centered character.  Optionally, the character
 * may also be transparent (without background).  -LM-
 */
static errr Term_text_special_win(HDC hdc, int x0, int y0,
	const byte *a, const char *c, bool transparent)
{
	window_type *win_ptr = (window_type*)(Term->data);
	srect rc;
	UINT fuOptions = 0;

	/* Bounding rectangle -- tile size equals font size */
	if ((win_ptr->font_wid == win_ptr->tile_wid) && (win_ptr->font_hgt == win_ptr->tile_hgt))
	{
		rc.left   = x0;
		rc.top    = y0;
		rc.right  = x0 + win_ptr->tile_wid;
		rc.bottom = y0 + win_ptr->tile_hgt;
	}

	/* Font size does not equal tile size -- use centering and clipping as needed */
	else
	{
		/* Center or clip horizontally */
		if (win_ptr->font_wid <= win_ptr->tile_wid)
		{
			rc.left   = x0 + (win_ptr->tile_wid - win_ptr->font_wid) / 2;
			rc.right  = rc.left + win_ptr->font_wid;
		}
		else
		{
			rc.left   = x0;
			rc.right  = x0 + win_ptr->tile_wid;
			fuOptions |= (ETO_CLIPPED);
		}

		/* Center or clip vertically */
		if (win_ptr->font_hgt <= win_ptr->tile_hgt)
		{
			rc.top    = y0 + (win_ptr->tile_hgt - win_ptr->font_hgt) / 2;
			rc.bottom = rc.top + win_ptr->font_hgt;
		}
		else
		{
			rc.top    = y0;
			rc.right  = x0 + win_ptr->tile_wid;
			fuOptions |= (ETO_CLIPPED);
		}
	}

	/* Foreground color: 16, 256 (paletted), or full */
	if (colors16) SetTextColor(hdc, PALETTEINDEX(win_pal[*a]));
	else          SetTextColor(hdc, win_clr[*a]);

	/* Use this term window's font */
	SelectObject(hdc, win_ptr->font_id);


	/* Background mode can be either opaque or transparent */
	SetBkMode(hdc, transparent ? TRANSPARENT : OPAQUE);

	/* Background color - always black */
	SetBkColor(hdc, RGB(0, 0, 0));

	/* Dump the text */
	if (ExtTextOut(hdc, rc.left, rc.top, fuOptions, (RECT *)&rc, c, 1, NULL) == 0) return (1);

	/* Restore default background mode */
	SetBkMode(hdc, OPAQUE);

	return (0);
}



/*
 * Low level graphics.  Assumes valid input.
 *
 * We request graphics by setting the high bit of both "attr" and "char" and
 * then using the "Term_higher_pict".
 * Graphic data is stored in large bitmaps with many rows and columns:  attr
 * (less the high bit) gets us the row, char (less the high bit) gets us the
 * column.
 *
 * This function also handles text output if transparency is on ("Term_always_
 * pict" is set to TRUE).  This allows us to have centered letters that appear
 * on a graphical background.  -LM-
 *
 * If graphics are not available we simply wipe the given grids.
 */
static errr Term_pict_win(int col, int row, int n, const byte *ap, const char *cp,
	const byte *tap, const char *tcp)
{
	window_type *win_ptr = (window_type*)(Term->data);

#ifdef USE_GRAPHICS

	int i;

	int cell_wid, cell_hgt, tile_wid, tile_hgt;

	int x, y, graphic_x, graphic_y;

	/* Get the application window drawing surface (client area only) */
	HDC hdc = GetDC(AppWin);

	/* Assume no stretching or shrinking */
	bool do_stretch = FALSE;


	/* Do nothing unless this window is visible */
	if (!win_ptr->visible) return (0);

	/* Paranoia -- Require that the graphics device context be enabled */
	if (!hdcMemGraphics) return (Term_wipe_win(col, row, n));


	/* Store redraw area (if necessary) */
	overlap_notify(col, row, col + n - 1, row);


	/* Size of bitmap cell */
	cell_wid = infGraph.CellWidth;
	cell_hgt = infGraph.CellHeight;

	/* Size of window cell */
	tile_wid = win_ptr->tile_wid;
	tile_hgt = win_ptr->tile_hgt;

	/* Location of first window cell */
	x = win_ptr->window_left + win_ptr->border_left + (col * tile_wid);
	y = win_ptr->window_top  + win_ptr->border_top  + (row * tile_hgt);


	/* We will need to stretch or shrink the graphics */
	if ((tile_wid != cell_wid) || (tile_hgt != cell_hgt)) do_stretch = TRUE;


	/* Draw attr/char pairs.  Advance one tile per pair. */
	for (i = 0; i < n; i++, x += tile_wid)
	{
		/* If we are using masked graphics, */
		if (hdcMemMask)
		{
			/* Handle background text (attr is < 128) */
			if (!(tap[i] & 0x80))
			{
				/* Centered text output with black background */
				if (Term_text_special_win(hdc, x, y, tap + i, tcp + i, FALSE)) return (1);
			}

			/* Handle background graphics (usual case) */
			else
			{
				/* Get the terrain/background graphic */
				graphic_x = (tcp[i] & 0x7F) * cell_wid;
				graphic_y = (tap[i] & 0x7F) * cell_hgt;

				/* No need to stretch */
				if (!do_stretch)
				{
					/* Copy from the bitmap to the window */
					(void)BitBlt(hdc, x, y, tile_wid, tile_hgt, hdcMemGraphics, graphic_x, graphic_y, SRCCOPY);
				}

				/* Need to stretch */
				else
				{
					/* Copy and stretch from the bitmap to the window */
					(void)StretchBlt(hdc, x, y, tile_wid, tile_hgt, hdcMemGraphics,
						graphic_x, graphic_y, cell_wid, cell_hgt, SRCCOPY);
				}
			}

			/* If foreground is the same as background, we're done */
			if ((tap[i] == ap[i]) && (tcp[i] == cp[i])) continue;
		}


		/* Handle foreground text (attr is <= 127) */
		if (!(ap[i] & 0x80))
		{
			/* Centered text output (transparent if a mask is available) */
			if (Term_text_special_win(hdc, x, y, ap + i, cp + i, (hdcMemMask != NULL))) return (1);
		}

		/* Handle foreground graphics */
		else
		{
			/* Get the foreground graphic */
			graphic_x = (cp[i] & 0x7F) * cell_wid;
			graphic_y = (ap[i] & 0x7F) * cell_hgt;

			/* No need to stretch */
			if (!do_stretch)
			{
				/* If using a mask, mask out the tile */
				if (hdcMemMask)
				{
					(void)BitBlt(hdc, x, y, tile_wid, tile_hgt, hdcMemMask, graphic_x, graphic_y, SRCAND);
				}

				/* Copy from the bitmap to the window */
				(void)BitBlt(hdc, x, y, tile_wid, tile_hgt, hdcMemGraphics, graphic_x, graphic_y, SRCPAINT);
			}

			/* Need to stretch */
			else
			{
				/* If using a mask, mask out the tile */
				if (hdcMemMask)
				{
					(void)StretchBlt(hdc, x, y, tile_wid, tile_hgt, hdcMemMask,
						graphic_x, graphic_y, cell_wid, cell_hgt, SRCAND);
				}

				/* Copy and stretch from the bitmap to the window */
				(void)StretchBlt(hdc, x, y, tile_wid, tile_hgt, hdcMemGraphics,
					graphic_x, graphic_y, cell_wid, cell_hgt, SRCPAINT);
			}
		}
	}

	/* Release the application window device context */
	ReleaseDC(AppWin, hdc);

#else /* USE_GRAPHICS */

	/* If graphics are undefined, just erase this grid */
	return (Term_wipe_win(col, row, n));

#endif /* USE_GRAPHICS */

	/* Success */
	return (0);
}




/* We are using SDL to play sounds and music */
#if defined(USE_SOUND) && defined(USE_SDL)

/*
 * Play WAV-format sounds.
 */
static void play_sound_sdl(int v)
{
	int i;
	char buf[1024];
	int channel = -1;


	/* Sound disabled */
	if (!use_sound || !sounds_ready) return;

	/* Illegal sound */
	if ((v < 0) || (v >= MSG_MAX)) return;

	/* Count the samples for this sound */
	for (i = 0; i < SAMPLE_MAX; i++)
	{
		if (!sound_file[v][i]) break;
	}

	/* No samples available */
	if (i == 0) return;


	/* Get the first available channel */
	for (i = 0; i < max_channels; i++)
	{
		/* If this channel isn't playing something */
		if (!Mix_Playing(i))
		{
			/* Wipe any existing sound data */
			if (mix_chunks[i])
			{
				Mix_FreeChunk(mix_chunks[i]);
				mix_chunks[i] = NULL;
			}

			/* Accept this channel */
			channel = i;
			break;
		}
	}

	/* All the channels are playing -- do nothing */
	if (channel == -1) return;


	/* Choose a sound and build a path to its file */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_SOUND, sound_file[v][Rand_simple(i)]);

	/* Load the sound into memory, save it in our array */
	mix_chunks[i] = Mix_LoadWAV(buf);

	/* Play the sound once */
	if (mix_chunks[i]) (void)Mix_PlayChannel(channel, mix_chunks[i], 0);
}


/*
 * Play music.  Only one music channel can be open at a time.
 *
 * A request->code of -1 stops and clears the current music.
 */
static void play_music_sdl(int v)
{
	int i;
	char buf[1024];

	/* Extract some information */
	int fade_in = 0;

	/* Shut down the current music, if any */
	if (music_track)
	{
		if (use_sound) Mix_FadeOutMusic(0);
		Mix_FreeMusic(music_track);
	}

	/* Music disabled */
	if (!use_sound || !music_ready) return;

	/* Illegal music, or music not present */
	if ((v < 0) || (v >= MUSIC_MAX)) return;

	/* Count the songs for this theme */
	for (i = 0; i < SAMPLE_MAX; i++)
	{
		if (!music_file[v][i]) break;
	}

	/* No songs available */
	if (i == 0) return;

	/* Choose a song and build a path to its file */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_MUSIC, music_file[v][Rand_simple(i)]);

	/* Load the requested music track */
	music_track = Mix_LoadMUS(buf);

	/* Fade in the requested music (allow fade-in, repeat forever) */
	(void)Mix_FadeInMusic(music_track, -1, fade_in);

	/* Save some variables for the jukebox */
	cur_music_start_time = GetTickCount() / 1000;
	cur_music_theme = v;
	cur_song_length = 120;  /* No public length info? */
}


#endif /* USE_SOUND and USE_SDL */




/*
 * Hack -- make a noise
 */
static errr Term_xtra_win_noise(void)
{
	MessageBeep(MB_ICONASTERISK);
	return (0);
}


/*
 * Hack -- make a sound (asynchronously)
 *
 * This is fairly inefficient code (a file is loaded on every sound request),
 * but very simple.
 *
 * Due to the limits of the Win32 API in multimedia, only one sound can play
 * at a time.  The "PlaySound()" function automagically handles all threading
 * issues for us, so we don't have to set up any event-driven callbacks.
 *
 * Using SDL makes it practicable to play several sounds at once, a common case
 * when playing quickly.
 */
static errr Term_xtra_win_sound(int v)
{
#ifdef USE_SOUND

/* We are using the native API to play sounds */
#ifndef USE_SDL
	int i;
	char buf[1024];


	/* Sound disabled */
	if (!use_sound) return (1);

	/* Illegal sound */
	if ((v < 0) || (v >= MSG_MAX)) return (1);

	/* Count the samples for this sound */
	for (i = 0; i < SAMPLE_MAX; i++)
	{
		if (!sound_file[v][i]) break;
	}

	/* No samples available */
	if (i == 0) return (1);

	/* Build the path, choose a random sample */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_SOUND, sound_file[v][Rand_simple(i)]);

	/* Play the sound  (Error here?  Include "Winmm.lib") */
	return (!PlaySound(buf, 0, SND_FILENAME | SND_ASYNC));


/* We are using SDL to play sounds */
#else

	/* Post a message to the application */
	(void)PostMessage(AppWin, WM_USER, 1, v);

	/* Done */
	return (0);

#endif /* USE_SDL */

#endif /* USE_SOUND */


	/* Oops */
	return (1);
}


/*
 * Start or stop playing music.
 */
static errr Term_xtra_win_music(int v)
{
#ifdef USE_SOUND

/* We are using the native API to play music */
#ifndef USE_SDL

	/* Not implemented */
	return (1);


/* We are using SDL to play music */
#else

	int fade_in, fade_out;  /* These are not actually used yet */

	/* Get current time (in seconds) */
	int cur_time = (GetTickCount() / 1000);

	/* Call the jukebox code -- return if nothing to do */
	if (!jukebox(&v, cur_time, &fade_in, &fade_out)) return (0);

	/* Post a message to the application */
	(void)PostMessage(AppWin, WM_USER, 2, v);

	/* Done */
	return (0);

#endif /* USE_SDL */

#endif /* USE_SOUND */

	/* Unused */
	(void)v;

	/* Oops */
	return (1);
}


/*
 * Process at least one event
 */
static errr Term_xtra_win_event(int v)
{
	MSG msg;

	/* Wait for an event */
	if (v)
	{
		/* Block */
		if (GetMessage(&msg, NULL, 0, 0))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	/* Check for an event */
	else
	{
		/* Check */
		if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	/* Success */
	return (0);
}


/*
 * Process all pending events
 */
static errr Term_xtra_win_flush(void)
{
	MSG msg;

	/* Process all pending events */
	while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	/* Success */
	return (0);
}


/*
 * Delay for "x" milliseconds
 */
static errr Term_xtra_win_delay(int v)
{
	/* Sleep */
	if (v > 0)
	{
		Term_xtra_win_event(0);
		Sleep(v);
	}

	/* Success */
	return (0);
}


/*
 * Do a "special thing"
 */
static errr Term_xtra_win(int n, int v)
{
	/* Handle a subset of the legal requests */
	switch (n)
	{
		/* Process an event */
		case TERM_XTRA_EVENT:
		{
			return (Term_xtra_win_event(v));
		}

		/* Flush all events */
		case TERM_XTRA_FLUSH:
		{
			return (Term_xtra_win_flush());
		}

		/* Clear the screen */
		case TERM_XTRA_CLEAR:
		{
			return (Term_xtra_win_clear());
		}

		/* Show or hide the cursor */
		case TERM_XTRA_SHAPE:
		{
			int x, y;

			/* Obtain the cursor */
			(void)Term_locate(&x, &y);

			/* Show or hide the cursor */
			Term_curs_win(x, y);
			return (0);
		}

		/* Refresh the screen */
		case TERM_XTRA_FRESH:
		{
			/* Get the window for the active term */
			window_type *win_ptr = (window_type*)(Term->data);

			/* Do special refresh maintainence */
			if      (win_ptr->Term_idx == TERM_MAIN) return (Term_fresh_gui(v));
			else if (win_ptr->Term_idx == TERM_MAP)  return (Term_fresh_gui(v));
			else                                     return (0);
		}

		/* Make a bell sound */
		case TERM_XTRA_NOISE:
		{
			return (Term_xtra_win_noise());
		}

		/* Make a special sound */
		case TERM_XTRA_SOUND:
		{
			return (Term_xtra_win_sound(v));
		}

		/* Process random events */
		case TERM_XTRA_BORED:
		{
			return (Term_xtra_win_event(0));
		}

		/* React to global changes */
		case TERM_XTRA_REACT:
		{
			return (Term_xtra_win_react());
		}

		/* Delay for some milliseconds */
		case TERM_XTRA_DELAY:
		{
			return (Term_xtra_win_delay(v));
		}

		/* Remove region covered by overlapping term from area to be refreshed */
		case TERM_XTRA_OVLAP:
		{
			return (Term_xtra_gui_overlap());
		}

		/* Play music */
		case TERM_XTRA_MUSIC:
		{
			return (Term_xtra_win_music(v));
		}
	}

	/* Oops */
	return (1);
}




/*
 * Check for double clicked (or dragged) savefile
 *
 * Apparently, Windows copies the entire filename into the first
 * piece of the "command line string".  Perhaps we should extract
 * the "basename" of that filename and append it to the "save" dir.
 */
static void check_for_save_file(LPSTR cmd_line)
{
	char *s, *p;

	/* First arg */
	s = cmd_line;

	/* No args */
	if (!s || !*s) return;

	/* Next arg */
	p = strchr(s, ' ');

	/* Tokenize */
	if (p) *p = '\0';

	/* Extract filename */
	strncat(savefile, s, sizeof(savefile) - 1);

	/* Validate the file */
	validate_file(savefile);

	(void)Term_fresh();

	/* Play game */
	play_game(FALSE);

	/* Quit */
	quit(NULL);
}


/*
 * Redraw a section of the application window
 */
static void handle_wm_paint(HWND hWnd)
{
	int i, term_idx;
	int x1, y1, x2, y2;

	PAINTSTRUCT ps;
	window_type *win_ptr;

	/* Remember the current term */
	term *old = Term;

	/* If the terms are not ready, do nothing */
	if (!term_main) return;

	/* Prepare the window for painting, define area to be redrawn */
	BeginPaint(hWnd, &ps);


	/* For each possible z-order (outward from the screen) */
	for (i = 0; i < TERM_MAX; i++)
	{
		/* Find the term with this z-order */
		for (term_idx = 0; term_idx < TERM_MAX; term_idx++)
		{
			if (term_z_order[term_idx] == i) break;
		}

		/* Ignore non-initialized terms */
		if (!angband_term[term_idx]) continue;

		/* Get this term's display window */
		win_ptr = (window_type *)(angband_term[term_idx]->data);

		/* Ignore non-visible windows */
		if (!win_ptr->visible) continue;

		/* Ignore if not within area to be redrawn */
		if ((long)(win_ptr->window_left) > ps.rcPaint.right) continue;
		if ((long)(win_ptr->window_top)  > ps.rcPaint.bottom) continue;
		if ((long)(win_ptr->window_left + win_ptr->window_wid) < ps.rcPaint.left) continue;
		if ((long)(win_ptr->window_top  + win_ptr->window_hgt) < ps.rcPaint.top) continue;

		/* Calculate redraw area */
		x1 = (ps.rcPaint.left - win_ptr->window_left - win_ptr->border_left) / win_ptr->tile_wid;
		y1 = (ps.rcPaint.top - win_ptr->window_top - win_ptr->border_top)  / win_ptr->tile_hgt;

		x2 = ((ps.rcPaint.right - win_ptr->window_left - win_ptr->border_left) / win_ptr->tile_wid) + 1;
		y2 = ((ps.rcPaint.bottom - win_ptr->window_top - win_ptr->border_top)  / win_ptr->tile_hgt) + 1;

		/* Activate this term */
		(void)Term_activate(angband_term[term_idx]);

		/* Redraw the area */
		(void)Term_redraw_section(x1, y1, x2, y2);
	}

	/* Restore the previous term */
	(void)Term_activate(old);

	/* Stop painting */
	EndPaint(hWnd, &ps);
}



/*
 * Create and initialize the Term contined within this window.
 */
void term_data_link_win(window_type *win_ptr)
{
	term *t = &term_data[win_ptr->Term_idx];

	/* Initialize the term */
	(void)term_init(t, win_ptr->cols, win_ptr->rows, win_ptr->keys);

	/* Use a "software" cursor */
	t->soft_cursor = TRUE;

	/*
	 * Usually (but see "init_graphics_win") use "Term_pict" for graphic
	 * data and "Term_text" for text.
	 */
	t->higher_pict = TRUE;

	/* Erase with "white space" */
	t->attr_blank = TERM_WHITE;
	t->char_blank = ' ';

	/* Never refresh one row */
	t->never_frosh = TRUE;


	/* Ignore the init/nuke hooks */

	/* Prepare the template hooks */
	t->user_hook = Term_user_gui;
	t->xtra_hook = Term_xtra_win;
	t->curs_hook = Term_curs_win;
	t->wipe_hook = Term_wipe_win;
	t->text_hook = Term_text_win;
	t->pict_hook = Term_pict_win;
	t->xchar_hook = Term_xchar_win;

	/* Allow special refresh updates (to handle overlapping terms)  XXX */
	if (win_ptr->Term_idx == TERM_MAIN) t->fresh_hook = Term_fresh_gui;
	if (win_ptr->Term_idx == TERM_MAP)  t->fresh_hook = Term_fresh_gui;

	/* Remember where we came from */
	t->data = win_ptr;
}


/*
 * Create the windows.
 *
 * First, instantiate the "default" values, then read the initialization file
 * to over-ride selected values, then create the windows.
 */
static void init_windows(void)
{
	int i, j;
	int cols, rows;
	int tries = 0;

	window_type *win_ptr;
	DWORD dwStyle;
	RECT rc;

	char buf[1024];

	int wid_tmp, hgt_tmp;


	/* Save screen width and height */
	screen_w = GetSystemMetrics(SM_CXSCREEN);
	screen_h = GetSystemMetrics(SM_CYSCREEN);

	/* Get information we need before loading windows */
	load_prefs(TRUE);

	/* Search defaults for the closest match to our application size */
	seek_app_size_defaults(&wid_tmp, &hgt_tmp, app_wid, app_hgt);


	/* If we don't find our display mode, try, try again */
	resolution_retry:


	/* Get defaults (if any) for each game Term (plus 1 special window) */
	for (i = 0; i < TERM_MAX + 1; i++)
	{
		/* Note:  We use and save default screen sizes, not actual sizes */

		/* Search for a legal window definition */
		for (j = 0; j < NUM_WINDOW_DEFAULTS; j++)
		{
			/* Ignore windows not of our index */
			if (window_defaults[j].Term_idx != i) continue;

			/* Require correct resolution and display mode */
			if ((window_defaults[j].resolution_x == wid_tmp) &&
			    (window_defaults[j].resolution_y == hgt_tmp) &&
			    (window_defaults[j].mode == cur_display_mode))
			{
				/* Copy it to the settings array */
				COPY(&window_settings[i], &window_defaults[j], window_type);
				break;
			}

			/* Allow default windows for this term */
			else if ((window_defaults[j].resolution_x <= 0) &&
			         (window_defaults[j].resolution_y <= 0))
			{
				/* Copy it to the settings array */
				COPY(&window_settings[i], &window_defaults[j], window_type);

				/* Save our monitor size */
				window_settings[i].resolution_x = wid_tmp;
				window_settings[i].resolution_y = hgt_tmp;

				/* Save our display setting */
				window_settings[i].mode = cur_display_mode;

				/* Allow default font requests */
				if ((!window_settings[i].font_want) && (i > TERM_MAIN))
				{
					window_settings[i].font_want = string_make(window_settings[TERM_MAIN].font_want);
				}
				break;
			}
		}
	}

	/* Modify various things by reading the initialization file */
	load_prefs(FALSE);


	/* Our main window is undefined!  Help! */
	if (!window_settings[TERM_MAIN].visible)
	{
		/* Increment tries, quit if we can't get any joy */
		if (++tries > 2) quit("No main window definition found!");

		/* Try both map and sub-window display modes */
		if (tries == 1)
		{
			if      (cur_display_mode == INTERFACE_SCREEN_EMPHA_SUB)
				cur_display_mode = INTERFACE_SCREEN_EMPHA_MAP;
			else if (cur_display_mode == INTERFACE_SCREEN_EMPHA_MAP)
				cur_display_mode = INTERFACE_SCREEN_EMPHA_SUB;

			else if (cur_display_mode == INTERFACE_WINDOW_EMPHA_MAP)
				cur_display_mode = INTERFACE_WINDOW_EMPHA_SUB;
			else if (cur_display_mode == INTERFACE_WINDOW_EMPHA_SUB)
				cur_display_mode = INTERFACE_WINDOW_EMPHA_MAP;
		}

		/* If that doesn't work, then try INTERFACE_SCREEN_EMPHA_MAP, ... */
		else
		{
			/* ... which SHOULD be defined for all default monitor sizes. */
			cur_display_mode = INTERFACE_SCREEN_EMPHA_MAP;
		}

		/* Try again */
		goto resolution_retry;
	}

	/* Set "arg_display_mode" */
	arg_display_mode = cur_display_mode;


	/* If we have not defined a display window, copy the main window */
	if (!window_settings[WINDOW_DISPLAY].visible)
	{
		COPY(&window_settings[WINDOW_DISPLAY], &window_settings[TERM_MAIN], window_type);
	}


	/* Use the special map window only if visible */
	use_special_map = window_settings[TERM_MAP].visible;

	/* This port allows special display configurations */
	special_view_hook = special_view_gui;


	/* We want either a bordered or full-screen application window */
	if ((cur_display_mode == INTERFACE_SCREEN_EMPHA_MAP) ||
	    (cur_display_mode == INTERFACE_SCREEN_EMPHA_SUB))
	{
		dwStyle = (WS_POPUP | WS_VISIBLE);

		/* Create the application window */
		AppWin = CreateWindowEx(0, AppName, AppName, dwStyle,
			0, 0, screen_w, screen_h, HWND_DESKTOP, NULL, 0, NULL);
	}
	else
	{
		dwStyle = (WS_THICKFRAME | WS_VISIBLE);

		/* Create the application window */
		AppWin = CreateWindowEx(0, AppName, AppName, dwStyle,
			app_left, app_top, app_wid, app_hgt,
			HWND_DESKTOP, NULL, 0, NULL);
	}

	/* Complain and quit on failure */
	if (!AppWin) quit_fmt("Failed to create %s window", VERSION_NAME);

	/* Get client area of window */
	(void)GetClientRect(AppWin, &rc);

	/* Application client size is the full window less any borders */
	client_w = rc.right - rc.left;
	client_h = rc.bottom - rc.top;


	/* Do various things to each term window (plus the 1 special display window) */
	for (i = 0; i < TERM_MAX + 1; i++)
	{
		/* Get the window */
		win_ptr = &window_settings[i];

		/* Ignore non-visible windows */
		if (!win_ptr->visible) continue;

		/* Remember current application size (so this window gets loaded again) */
		win_ptr->resolution_x = app_wid;
		win_ptr->resolution_y = app_hgt;

		/* Access the specified font file */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, win_ptr->font_want);

		/* Use it */
		if (change_font_win(win_ptr, buf))
		{
			/* On failure, access the standard font file */
			(void)path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, DEFAULT_GAME_FONT);

			/* Use it */
			if (change_font_win(win_ptr, buf))
			{
				quit_fmt("Could not find the default font (looking for \"%s\")", buf);
			}
		}

		/* We know the size of most windows (Hard-coded test  XXX XXX) */
		if (i != TERM_MAP)
		{
			/* Get rows and columns for all windows whose size is now known */
			cols = (win_ptr->window_wid - win_ptr->border_left -
				win_ptr->border_right) / win_ptr->tile_wid;
			rows = (win_ptr->window_hgt - win_ptr->border_top -
				win_ptr->border_bottom) / win_ptr->tile_hgt;

			/* Save rows and cols, ensuring that they are never greater than 255 */
			win_ptr->cols = MIN(cols, 255);
			win_ptr->rows = MIN(rows, 255);

			/* Verify rows and columns */
			if ((win_ptr->cols < term_size_min[i][0]) ||
				(win_ptr->rows < term_size_min[i][1]))
			{
				char buf[1024];

				/* Build a (hopefully informative) error message */
				(void)my_strcpy(buf, format("Window \"%s\" (Term-#%d) requires %d columns and %d rows, but only has %dx%d.\n\nReasons for this error may include not having a small enough font\navailable in \"lib/xtra/font\" or corruption of the %s.ini file.",
						angband_term_name[i], i, term_size_min[i][0], term_size_min[i][1],
						win_ptr->cols, win_ptr->rows, AppName),
						sizeof(buf));

				/* Complain and quit */
				if (MessageBox(NULL, buf, "Critical Error", MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP))
				{
					quit("Window too small");
				}
			}
		}
	}


	/* Initialize graphics */
	if (init_graphics_win(FALSE)) use_graphics = arg_graphics;

	/* On failure, cancel the request */
	else                          arg_graphics = use_graphics;


	/* Calculate map display (if present) */
	if (window_settings[TERM_MAP].visible) calc_map_display();


	/* Create and initialize terms */
	for (i = 0; i < TERM_MAX; i++)
	{
		/* Skip windows that are not visible */
		if (!window_settings[i].visible) continue;

		/* Remember which term we are hosting */
		window_settings[i].Term_idx = i;

		/* Create and initialize the term */
		term_data_link_win(&window_settings[i]);

		/* Add a pointer to the term to our easy access array */
		angband_term[i] = &term_data[i];
	}


	/* Hack -- The special display window uses the main term */
	window_settings[WINDOW_DISPLAY].Term_idx = TERM_MAIN;

	/* Link to the main term */
	(void)Term_activate(term_main);


	/* If screen is larger than our current resolution, maximize the windows. */
	if ((screen_w > wid_tmp) || (screen_h > hgt_tmp)) do_maximize();

#ifdef SUPPORT_GAMMA

	/* Set up the gamma correction table */
	if (gamma_correction > 0) build_gamma_table(gamma_correction);

#endif /* SUPPORT_GAMMA */

	/* New palette XXX XXX XXX */
	(void)new_palette();

	/* Build a list of font sizes available to the game */
	get_font_list("fontlist.txt", ".fon");

	/* Hack -- If the graphics are masked, then the map display should always use "Term_pict". */
	if ((hdcMemMask) && (term_map))
	{
		term_map->always_pict = TRUE;
		term_map->higher_pict = FALSE;
	}

	/* React to global changes (make sure we've updated completely) */
	(void)Term_xtra_win_react();

	/* Process pending messages */
	(void)Term_xtra_win_flush();
}

/*
 * State of the numlock key when the application started up
 */
static bool old_numlock = FALSE;

/*
 * Originally from "support.microsoft.com/kb/127190/EN-US/"
 *
 * On startup, remember the current state of the numlock key and then turn it
 * off.  On shutdown, restore the old numlock setting.
 */
static void SetNumLock(BOOL startup)
{
	bool flag = FALSE;
	BYTE keyState[256];

	/* Get the keyboard state */
	GetKeyboardState((LPBYTE)&keyState);

	/* Starting the game */
	if (startup)
	{
		/* Remember the current numlock setting */
		old_numlock = (keyState[VK_NUMLOCK] & 1);

		/* Turn numlock off, if necessary */
		if (old_numlock) flag = TRUE;
	}

	/* Closing the game */
	else
	{
		/* Restore old numlock setting */
		if (( old_numlock && !(keyState[VK_NUMLOCK] & 1)) ||
			(!old_numlock &&  (keyState[VK_NUMLOCK] & 1)))
		{
			flag = TRUE;
		}
	}

	/* Toggle numlock */
	if (flag)
	{
		/* Simulate a key press */
		keybd_event(VK_NUMLOCK, 0x45,
			KEYEVENTF_EXTENDEDKEY | 0, 0);

		/* Simulate a key release */
		keybd_event(VK_NUMLOCK, 0x45,
			KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
	}
}


/*
 * Handle messages sent to the main application window.
 */
static LRESULT CALLBACK my_WindowProc(HWND hWnd, UINT uMsg,
                                      WPARAM wParam, LPARAM lParam)
{
	HDC hdc;
	int i;


	/* Handle message */
	switch (uMsg)
	{
		/* Determine minimum and maximum window sizes (constrain resizing) */
		case WM_GETMINMAXINFO:
		{
			MINMAXINFO FAR *lpmmi;
			RECT rc;
			lpmmi = (MINMAXINFO FAR *)lParam;

			/* Ignore if application window not set up */
			if (!AppWin) return (1);

			/* Ignore if we don't have any term windows */
			if (!angband_term[TERM_MAIN]) return (1);


			/* We may have either a bordered or full-screen application window */
			if ((cur_display_mode == INTERFACE_SCREEN_EMPHA_MAP) ||
				(cur_display_mode == INTERFACE_SCREEN_EMPHA_SUB))
			{
				/* Window dimensions are the full screen */
				rc.left   = 0;
				rc.top    = 0;
				rc.right  = GetSystemMetrics(SM_CXSCREEN);
				rc.bottom = GetSystemMetrics(SM_CYSCREEN);
			}
			else
			{
				/* Get the properties of the application window */
				DWORD dwStyle = GetWindowLong(AppWin, GWL_STYLE);

				/* Get the application window size, including borders */
				rc.left   = 0;
				rc.top    = 0;
				rc.right  = app_wid;
				rc.bottom = app_hgt;
			}

			/* Save minimum size */
			lpmmi->ptMinTrackSize.x = rc.right - rc.left;
			lpmmi->ptMinTrackSize.y = rc.bottom - rc.top;

			/* The maximum size is always the screen dimensions */
			lpmmi->ptMaxTrackSize.x = GetSystemMetrics(SM_CXSCREEN);
			lpmmi->ptMaxTrackSize.y = GetSystemMetrics(SM_CYSCREEN);

			/* We have processed this message */
			return (0);
		}

		/* Request to redraw a portion of the application window */
		case WM_PAINT:
		{
			handle_wm_paint(hWnd);

			return (0);
		}

		/* Keypresses */
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
		{
			bool mc = FALSE;
			bool ms = FALSE;
			bool ma = FALSE;

			/* Pressing either windows key minimizes the application */
			if ((wParam == VK_LWIN) || (wParam == VK_RWIN))
			{
				(void)ShowWindow(AppWin, SW_MINIMIZE);
				return (0);
			}

			/* Extract the modifiers */
			if (GetKeyState(VK_CONTROL) & 0x8000) mc = TRUE;
			if (GetKeyState(VK_SHIFT)   & 0x8000) ms = TRUE;
			if (GetKeyState(VK_MENU)    & 0x8000) ma = TRUE;

			/*
			 * Handle "special" keys, reject ignored keys
			 *
			 * Hack -- Because the AltGr key is handled as Control+Alt, we
			 * must ignore this combination.  XXX XXX
			 */
			if ((special_key[(byte)(wParam)]) ||
			    (ma && !mc && !ignore_key[(byte)(wParam)]))
			{
				/* Begin the macro trigger */
				Term_keypress(31);

				/* Send the modifiers */
				if (mc) (void)Term_keypress('C');
				if (ms) (void)Term_keypress('S');
				if (ma) (void)Term_keypress('A');

				/* Extract "scan code" */
				i = LOBYTE(HIWORD(lParam));

				/* Introduce the scan code */
				Term_keypress('x');

				/* Encode the hexidecimal scan code */
				Term_keypress(hexsym[i / 16]);
				Term_keypress(hexsym[i % 16]);

				return (0);
			}

			break;
		}

		/* Translated keypresses */
		case WM_CHAR:
		{
			if (wParam == 0) return (0);
			if (term_main) (void)Term_keypress(wParam);

			return (0);
		}

		/* Close window */
		case WM_CLOSE:
		{
			/* We are playing a game with an active character, and are not using the infinite lives cheat */
			if ((character_generated) && (!beginner_play))
			{
				/* Commands are not allowed now */
				if (!inkey_flag)
				{
					plog("You may not do that right now.");
					return (0);
				}

				/* Hack -- Forget messages */
				msg_flag = FALSE;

				/* Save the game */
				do_cmd_save_game(FALSE);
			}
			quit(NULL);
			return (0);
		}

		/* Shut down the application thread */
		case WM_QUIT:
		{
			quit(NULL);
			return (0);
		}

		case WM_PALETTECHANGED:
		{
			/* Ignore if palette change caused by itself */
			if ((HWND)wParam == hWnd) return (0);

			/* Fall through... */
		}

		case WM_QUERYNEWPALETTE:
		{
			/* Not needed unless colors are paletted */
			if (!paletted) return (0);

			/* Access the (whole) application window */
			hdc = GetDC(hWnd);

			/* Select the palette */
			SelectPalette(hdc, hPal, FALSE);

			/* Use this palette as the system palette */
			i = RealizePalette(hdc);

			/* If any palette entries changed, repaint the window. */
			if (i) InvalidateRect(hWnd, NULL, TRUE);

			/* Release device context focus */
			ReleaseDC(hWnd, hdc);

			return (0);
		}

		/* Activate or deactivate this window */
		case WM_ACTIVATE:
		{
			/* Unless this window is minimized or being deactivated */
			if (lParam && !HIWORD(wParam))
			{
				/* Focus on it */
				SetFocus(hWnd);

				return (0);
			}

			break;
		}

		/* Create the application window */
		case WM_CREATE:
		{
			/* Set a 1-second timer */
			SetTimer(hWnd, TIMER_BASIC, 1000, NULL);

			/* Turn off numlock */
			SetNumLock(TRUE);

			/* Pass message to the system */
			return (0);
		}

		/* Destroy the application window */
		case WM_DESTROY:
		{
			/* Kill the timer */
			KillTimer(hWnd, TIMER_BASIC);

			/* Restore previous numlock setting */
			SetNumLock(FALSE);

			break;
		}

		/* A timer has pinged us */
		case WM_TIMER:
		{
			/* If it's the basic timer, animate some stuff */
			if (wParam == TIMER_BASIC) map_animate();

			return (0);
		}

		/* A user-defined event has triggered */
		case WM_USER:
		{
/* We are using SDL for sounds/music */
#if defined(USE_SOUND) && defined(USE_SDL)

			/* Play a sound */
			if (wParam == 1) play_sound_sdl(lParam);

			/* Play music */
			else if (wParam == 2) play_music_sdl(lParam);

#endif
			return (0);
		}

		/* Handle mouse actions  -LM- */
		case WM_LBUTTONDBLCLK:
		case WM_LBUTTONDOWN:
		case WM_RBUTTONDOWN:
		case WM_MOUSEMOVE:
		case 0x020A:  /* The mouse wheel (Win 98 and later) */
		{
			/* Wipe the data storage */
			mouseaction_type this_mouse_action = { 0, 0, 0, 0 };

			/* Ignore if not using the mouse */
			if (!use_mouse) break;

			/* Ignore if no active window, or the game is not initialized */
			if (!Term) break;

			/* Right-clicks trigger GUI interaction (if legal) */
			if (uMsg == WM_RBUTTONDOWN)
			{
				/* If waiting for a command, activate the GUI options */
				if ((inkey_flag) && (!screen_depth))
				{
					screen_save(FALSE);
					(void)Term_user_gui(0);
					screen_load();

					/* Redraw everything */
					do_cmd_redraw();
				}
				return (0);
			}

			/* In wheel movement all that matters is "up" or "down" */
			if (uMsg == 0x020A)
			{
				short zDelta = (short) HIWORD(wParam);

				/* Require significant movement (windows standard) */
				if      (zDelta / 120 > 0) this_mouse_action.y = 0;
				else if (zDelta / 120 < 0) this_mouse_action.y = 255;

				/* Otherwise do nothing */
				else break;
			}

			/* Other actions require that we save position in a specific Term */
			else
			{
				int term, col, row;

				/* Extract the mouse coordinates */
				int xPos = GET_X_LPARAM(lParam);
				int yPos = GET_Y_LPARAM(lParam);

				/* Get the term and grid position in which these coordinates lie */
				get_term_position(&term, &col, &row, xPos, yPos, TRUE);

				/* Ignore positions not within any term */
				if ((term >= TERM_MAX) && (term < 0)) break;

				/* If this is just a mouse movement, then only save if different */
				if ((uMsg == WM_MOUSEMOVE) &&
				    (col == prev_mouse_action.x) && (row == prev_mouse_action.y))
				{
					break;
				}

				/* Remember the term */
				this_mouse_action.term = term;

				/* Remember the current position (only for these kinds of actions) */
				prev_mouse_action.x = this_mouse_action.x = col;
				prev_mouse_action.y = this_mouse_action.y = row;
			}

			/* We have used the mouse */
			(void)Term_keypress(MOUSEKEY);

			/* Store the action type */
			if      (uMsg == WM_LBUTTONDOWN)   (void)Term_keypress(MOUSE_L_CLICK);
			else if (uMsg == WM_LBUTTONDBLCLK) (void)Term_keypress(MOUSE_L_DBLCLICK);
			else if (uMsg == 0x020A)           (void)Term_keypress(MOUSE_WHEEL);
			else                               (void)Term_keypress(MOUSE_MOVEONLY);

			/* Store the X, Y position */
			(void)Term_keypress(this_mouse_action.x);
			(void)Term_keypress(this_mouse_action.y);

			/* Store the flags */
			(void)Term_keypress(this_mouse_action.term);

			/* Message handled */
			return (0);
		}

#if defined(USE_SOUND) && defined(USE_SDL)
		/* Gain or lose focus */
		case WM_SETFOCUS:
		{
			/* Pause and unpause music as needed */
			Mix_ResumeMusic();
			break;
		}
		case WM_KILLFOCUS:
		{
			/* Pause and unpause music as needed */
			Mix_PauseMusic();
			break;
		}
#endif

		/* We need to set the cursor */
		case WM_SETCURSOR:
		{
			/* Only do something if hot spot is within client area */
			if (LOWORD(lParam) == HTCLIENT)
			{
				SetCursor(use_mouse ? AppCursor : NULL);
				return (1);
			}
			return (0);
		}
	}

	/* Pass the message to the system */
	return (DefWindowProc(hWnd, uMsg, wParam, lParam));
}



/*** Temporary Hooks (for use before the application window is created) ***/



/*
 * Display warning message (see "z-util.c")
 */
static void hack_plog(cptr str)
{
	/* Give a warning */
	if (str)
	{
		MessageBox(NULL, str, "Warning",
		           MB_ICONEXCLAMATION | MB_OK);
	}
}

/*
 * Display error message and quit (see "z-util.c")
 */
static void hack_quit(cptr str)
{
	/* Give a warning */
	if (str)
	{
		MessageBox(NULL, str, "Error",
		           MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);
	}

	/* Unregister the classes */
	UnregisterClass(AppName, hInstance);

	/* Destroy the icon */
	if (hIcon) DestroyIcon(hIcon);

	/* Exit */
	exit(0);
}


/*** Various hooks ***/

/*
 * Display warning message (see "z-util.c")
 */
static void hook_plog(cptr str)
{
	/* Warning */
	if (str)
	{
		MessageBox(AppWin, str, "Warning",
		           MB_ICONEXCLAMATION | MB_OK);
	}
}



/* We are using SDL for sounds/music */
#if defined(USE_SOUND) && defined(USE_SDL)
/*
 * Shut off the audio system
 */
static void cleanup_audio(void)
{
	int i, j;

	/* Halt playback on all channels */
	Mix_HaltChannel(-1);

	/* Free the chunks */
	for (i = 0; i < NUM_CHANNELS; i++)
	{
		Mix_FreeChunk(mix_chunks[i]);
		mix_chunks[i] = NULL;
	}

	/* Free the channels themselves */
	(void) Mix_AllocateChannels(0);

	/* Shut down the current music, if any */
	if (music_track) Mix_FreeMusic(music_track);

	/* Free the sound names -- already done */

	/* Free the music names */
	if (music_ready)
	{
		for (i = 0; i < MUSIC_MAX; i++)
		{
			for (j = 0; j < SAMPLE_MAX; j++)
			{
				/* Free the song names */
				if (music_file[i][j]) (void)string_free(music_file[i][j]);
			}
		}
	}
}
#endif /* USE_SOUND and USE_SDL */


/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(cptr str)
{
	int i;
	RECT rc;


	/* Give a warning */
	if (str)
	{
		MessageBox(AppWin, str, "Error",
			MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);
	}

	/* Get current location and size of application window (including borders) */
	GetWindowRect(AppWin, &rc);

	/* Save the position and size */
	app_left = rc.left;
	app_top  = rc.top;
	app_wid  = rc.right - rc.left;
	app_hgt  = rc.bottom - rc.top;

	/* Save the preferences */
	save_prefs();

	/* Destroy all term windows */
	for (i = TERM_MAX - 1; i >= 0; --i)
	{
		/* Nuke the game term structure */
		(void)term_nuke(angband_term[i]);
	}

	/* Scan all the term windows */
	for (i = 0; i <= TERM_MAX; i++)
	{
		/* Get this term window */
		window_type *win_ptr = &window_settings[i];

		/* Remove all fonts from the system, free resources */
		if (win_ptr->font_file)
		{
			/* Remove font from the system */
			remove_font_win(win_ptr);
		}
		if (win_ptr->font_want) (void)string_free(win_ptr->font_want);
	}

	/* Kill the application window */
	if (AppWin) DestroyWindow(AppWin);
	AppWin = 0;


#ifdef USE_GRAPHICS
	/* Free the bitmap stuff */
	FreeDIB(&infGraph);
	FreeDIB(&infMask);

	/* Release the memnory device context that contains the graphics bitmap (if any) */
	if (hdcMemGraphics) DeleteDC(hdcMemGraphics);

	/* Release the mask (if any) */
	if (hdcMemMask) DeleteDC(hdcMemMask);
#endif /* USE_GRAPHICS */

#ifdef USE_SOUND
	/* Free the sound names */
	if (sounds_ready)
	{
		int j;

		for (i = 0; i < MSG_MAX; i++)
		{
			for (j = 0; j < SAMPLE_MAX; j++)
			{
				/* Free the sound names */
				if (sound_file[i][j]) (void)string_free(sound_file[i][j]);
			}
		}
	}
#endif /* USE_SOUND */



	/* We compiled with SDL support */
#ifdef USE_SDL

	/* We used SDL for sounds/music */
#ifdef USE_SOUND

	/* Shut off the audio system */
	cleanup_audio();

	/* Shut down the SDL_Mixer library */
    Mix_CloseAudio();

#endif /* USE_SOUND (SDL-extended) */

	/* Shut down the SDL library */
	SDL_Quit();

#endif /* USE_SDL */


	/*** Free some other stuff ***/

	DeleteObject(hbrYellowBrush);

	if (hPal) DeleteObject(hPal);

	UnregisterClass(AppName, hInstance);

	if (hIcon) DestroyIcon(hIcon);

	/* Free strings */
	(void)string_free(argv0);
	(void)string_free(ANGBAND_DIR_XTRA_FONT);
	(void)string_free(ANGBAND_DIR_XTRA_GRAF);
	(void)string_free(ANGBAND_DIR_XTRA_SOUND);

	cleanup_angband();

	exit(0);
}



/*** Initialize ***/



/*
 * Init some stuff
 */
static void init_stuff(void)
{
	int i;

	char path[1024];

	/* Get program name with full path */
	if (GetModuleFileName(hInstance, path, sizeof(path)) == 0) quit_fmt("Could not find executable (%s)!", path);


	/* Paranoia -- need space to store /lib dirs */
	path[900] = '\0';


	/* Save the "program name" */
	argv0 = string_make(path);

	/* Start at beginning of string */
	i = strlen(path);

	/* Remove everything after the last PATH_SEP */
	while (!suffix(path, PATH_SEP))
	{
		path[--i] = '\0';
		if (i < 2) break;
	}

	/* And then remove the path separator also */
	path[--i] = '\0';

	/*
	 * On single-user systems, the working directory is the application
	 * directory.  On multi-user systems, we are safe writing to the user's
	 * personal game directory.
	 */
#ifdef PRIVATE_USER_PATH
	/* Use user's game directory */
	(void)my_strcpy(WorkingFolder, PRIVATE_USER_PATH, sizeof(WorkingFolder));

	/* Create directories for the user's files */
	create_user_dirs();
#else
	/* OR, use application directory */
	(void)my_strcpy(WorkingFolder, path, sizeof(WorkingFolder));
#endif

	/* Add "\lib\" to the path */
	strcat(path, "\\lib\\");

	/* Validate the path */
	validate_dir(path);

	/* Init the file paths */
	init_file_paths(path);

	/* Hack -- Validate the paths */
	validate_dir(ANGBAND_DIR_APEX);
	validate_dir(ANGBAND_DIR_BONE);
	validate_dir(ANGBAND_DIR_DATA);
	validate_dir(ANGBAND_DIR_EDIT);

	validate_dir(ANGBAND_DIR_FILE);
	validate_dir(ANGBAND_DIR_HELP);
	validate_dir(ANGBAND_DIR_INFO);
	validate_dir(ANGBAND_DIR_PREF);
	validate_dir(ANGBAND_DIR_SAVE);
	validate_dir(ANGBAND_DIR_USER);
	validate_dir(ANGBAND_DIR_XTRA);

	/* Build the filename */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_FILE, "news.txt");

	/* Hack -- Validate the "news.txt" file */
	validate_file(path);


	/* Build the "font" path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "font");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_FONT = string_make(path);

	/* Validate the "font" directory */
	validate_dir(ANGBAND_DIR_XTRA_FONT);

	/* Build the filename */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA_FONT, DEFAULT_GAME_FONT);

	/* Hack -- Validate the basic font */
	validate_file(path);


#ifdef USE_GRAPHICS

	/* Build the "graf" path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "graf");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_GRAF = string_make(path);

	/* Check the "graf" directory */
	if (!check_dir(ANGBAND_DIR_XTRA_GRAF)) arg_graphics = GRAPHICS_FONT;

#endif /* USE_GRAPHICS */


#ifdef USE_SOUND

	/* Build the "sound" path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "sound");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_SOUND = string_make(path);

	/* Check the "sound" directory */
	if (!check_dir(ANGBAND_DIR_XTRA_SOUND)) arg_sound = SOUND_NONE;


	/* Build the "music" path */
	(void)path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "music");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_MUSIC = string_make(path);

	/* Do not check the music directory */

#endif /* USE_SOUND */


	/* We compiled with SDL support */
#ifdef USE_SDL

	/* Fire up the audio portion of the SDL library */
	SDL_Init(SDL_INIT_AUDIO);

	/* We are using SDL for sounds/music */
#ifdef USE_SOUND

	/* Fire up SDL_Mixer */
	if (Mix_OpenAudio(22050, MIX_DEFAULT_FORMAT, 2, 2048) >= 0)
	{
		/* Allocate some sound channels */
		max_channels = Mix_AllocateChannels(NUM_CHANNELS);

		/* Wipe the pointers to the sound chunk data */
		for (i = 0; i < NUM_CHANNELS; i++) mix_chunks[i] = NULL;
	}
	else
	{
		arg_sound = SOUND_NONE;  /* XXX */
	}

#endif /* USE_SOUND (SDL-extended) */

#endif /* USE_SDL */

}


/*
 * The windows port's "main()" function.
 */
int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst,
                       LPSTR lpCmdLine, int nCmdShow)
{
	int i;

	WNDCLASS wc;
	HDC hdc;

	/* Unused parameter */
	(void)nCmdShow;

	/* Set up an application window class */
	if (hPrevInst == NULL)
	{
		/* Share the same device context for all windows */
		wc.style         = CS_CLASSDC | CS_DBLCLKS;

		/* Set up a message-handling function */
		wc.lpfnWndProc   = my_WindowProc;

		/* Allocate space for a 4-bit pointer after the window instance */
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 4;

		/* Note application process, save globally */
		wc.hInstance     = hInstance = hInst;

		/* Get the icon */
		wc.hIcon         = hIcon = LoadIcon(hInst, "sangband.ico");

		/* Use a standard arrow cursor */
		wc.hCursor       = AppCursor = LoadCursor(NULL, IDC_ARROW);

		/* Black background */
		wc.hbrBackground = (HBRUSH) GetStockObject(BLACK_BRUSH);

		/* No default menu */
		wc.lpszMenuName  = NULL;

		/* Give a name to this window class */
		wc.lpszClassName = AppName;

		/* Register this class, quitting on failure */
		if (!RegisterClass(&wc)) exit(1);
	}

	/* Temporary warning and quit hooks */
	plog_aux = hack_plog;
	quit_aux = hack_quit;

	/* Prepare the filepaths */
	init_stuff();

	/* Initialize the keypress analyzer */
	for (i = 0; special_key_list[i]; i++)
	{
		special_key[special_key_list[i]] = TRUE;
	}

	/* Initialize the ignored keypresses */
	for (i = 0; ignore_key_list[i]; ++i)
	{
		ignore_key[ignore_key_list[i]] = TRUE;
	}


	/* Determine if display is 16/256/true color */
	hdc = GetDC(NULL);
	colors16 = (GetDeviceCaps(hdc, BITSPIXEL) == 4);
	paletted = ((GetDeviceCaps(hdc, RASTERCAPS) & RC_PALETTE) ? TRUE : FALSE);
	ReleaseDC(NULL, hdc);

	/* Save the maximum system colors */
	if (colors16) max_system_colors = 16;
	else          max_system_colors = MAX_COLORS;

	/* Initialize the colors */
	for (i = 0; i < MAX_COLORS; i++)
	{
		byte rv, gv, bv;

		/* Extract desired values */
		rv = color_table[i].rv;
		gv = color_table[i].gv;
		bv = color_table[i].bv;

		/* Extract the "complex" code */
		win_clr[i] = PALETTERGB(rv, gv, bv);

		/* Save the "simple" code */
		color_table[i].kv = win_pal[i];
	}

	/* Create the basic brushes */
	hbrYellowBrush = CreateSolidBrush(win_clr[TERM_YELLOW]);


	/* Prepare the windows */
	init_windows();

	/* Activate warning and quit hooks */
	plog_aux = hook_plog;
	quit_aux = hook_quit;

	/* Set global function hooks */
	switch_display_hook = switch_display_gui;
	window_change_font_hook = window_change_font_win;
	change_font_hook = change_font_win;
	remove_font_hook = remove_font_win;
	init_graphics_hook = init_graphics_win;
	term_data_link_hook = term_data_link_win;


	/* Set the system suffix */
	ANGBAND_SYS = "win";


	/* Deactivate the main view, request a full-screen display */
	display_change(DSP_SAVE | DSP_FULL,
		term_size_min[WINDOW_DISPLAY][0], term_size_min[WINDOW_DISPLAY][1]);

	/* Initialize the game */
	init_angband();


	/* Paranoia -- erase the savefile name */
	*savefile = '\0';

	/* Did the user double click on a save file? */
	check_for_save_file(lpCmdLine);

	/* Wait for response unless we have a savefile */
	if (!savefile[0]) pause_line(Term->rows - 1);

	/* Cancel full-screen view */
	display_change(DSP_FULL | DSP_LOAD, 0, 0);


	/* Play the game */
	play_game(FALSE);


	/* Paranoia */
	quit(NULL);

	/* Paranoia */
	return (0);
}


#endif /* WINDOWS */
