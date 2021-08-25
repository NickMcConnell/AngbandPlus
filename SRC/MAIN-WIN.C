/* File: main-win.c */

/*
 * Copyright (c) 1997 Ben Harrison, Skirmantas Kligys, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */


/*
 * This file helps Angband work with Windows computers.
 *
 * To use this file, use an appropriate "Makefile" or "Project File",
 * make sure that "WINDOWS" and/or "WIN32" are defined somewhere, and
 * make sure to obtain various extra files as described below.
 *
 * The official compilation uses the CodeWarrior Pro compiler, which
 * includes a special project file and precompilable header file.
 *
 *
 * See also "main-dos.c" and "main-ibm.c".
 *
 *
 * The "lib/user/pref-win.prf" file contains keymaps, macro definitions,
 * and/or color redefinitions.
 *
 * The "lib/user/font-win.prf" contains attr/char mappings for use with the
 * normal "lib/xtra/font/xxx.fon" font files.
 *
 * The "lib/user/graf-win.prf" contains attr/char mappings for use with the
 * special "lib/xtra/graf/xxx.bmp" bitmap files, which are activated by a menu
 * item.
 *
 *
 * Compiling this file, and using the resulting executable, requires
 * several extra files not distributed with the standard Angband code.
 * If "USE_GRAPHICS" is defined, then "readdib.h" and "readdib.c" must
 * be placed into "src/", and the "8X8.BMP" bitmap file must be placed
 * into "lib/xtra/graf".  In any case, some "*.fon" files (including
 * "8X13.FON" if nothing else) must be placed into "lib/xtra/font/".
 * If "USE_SOUND" is defined, then some special library (for example,
 * "winmm.lib") may need to be linked in, and desired "*.WAV" sound
 * files must be placed into "lib/xtra/sound/".  All of these extra
 * files can be found in the "ext-win" archive.
 *
 *
 * The "Term_xtra_win_clear()" function should probably do a low-level
 * clear of the current window, and redraw the borders and other things,
 * if only for efficiency.  XXX XXX XXX
 *
 * A simpler method is needed for selecting the "tile size" for windows.
 * XXX XXX XXX
 *
 * The various "warning" messages assume the existance of the "screen.w"
 * window, I think, and only a few calls actually check for its existance,
 * this may be okay since "NULL" means "on top of all windows". (?)  The
 * user must never be allowed to "hide" the main window, or the "menubar"
 * will disappear.  XXX XXX XXX
 *
 * Special "Windows Help Files" can be placed into "lib/xtra/help/" for
 * use with the "winhelp.exe" program.  These files *may* be available
 * at the ftp site somewhere, but I have not seen them.  XXX XXX XXX
 *
 *
 * Initial framework (and most code) by Ben Harrison (benh@phial.com).
 *
 * Original code by Skirmantas Kligys (kligys@scf.usc.edu).
 *
 * Additional code by Ross E Becker (beckerr@cis.ohio-state.edu),
 * and Chris R. Martin (crm7479@tam2000.tamu.edu).
 */


#include "angband.h"


#ifdef WINDOWS


/*
 * Extract the "WIN32" flag from the compiler
 */
#if defined(__WIN32__) || defined(__WINNT__) || defined(__NT__)
# ifndef WIN32
#  define WIN32
# endif
#endif


/*
 * Hack -- allow use of "screen saver" mode
 */
#define USE_SAVER


/*
 * Allow use of installed fonts
 */
#define USE_SYS_FONT


/*
 * Save the HDC for performance
 */
#define SAVE_DC


/*
 * Apply gamma correction to colors
 */
#define USE_GAMMA_CORRECTION


/*
 * Use a lot of game menus
 */
#define LOTSA_MENUS


/*
 * Menu constants -- see "ANGBAND.RC"
 */

#define IDM_FILE_NEW			100
#define IDM_FILE_OPEN			101
#define IDM_FILE_EXIT			121

#define IDM_WINDOW_VIS_0		200
#define IDM_WINDOW_VIS_1		201
#define IDM_WINDOW_VIS_2		202
#define IDM_WINDOW_VIS_3		203
#define IDM_WINDOW_VIS_4		204
#define IDM_WINDOW_VIS_5		205
#define IDM_WINDOW_VIS_6		206
#define IDM_WINDOW_VIS_7		207

#define IDM_WINDOW_FONT_0		210
#define IDM_WINDOW_FONT_1		211
#define IDM_WINDOW_FONT_2		212
#define IDM_WINDOW_FONT_3		213
#define IDM_WINDOW_FONT_4		214
#define IDM_WINDOW_FONT_5		215
#define IDM_WINDOW_FONT_6		216
#define IDM_WINDOW_FONT_7		217

#define IDM_WINDOW_BIZ_0		230
#define IDM_WINDOW_BIZ_1		231
#define IDM_WINDOW_BIZ_2		232
#define IDM_WINDOW_BIZ_3		233
#define IDM_WINDOW_BIZ_4		234
#define IDM_WINDOW_BIZ_5		235
#define IDM_WINDOW_BIZ_6		236
#define IDM_WINDOW_BIZ_7		237

#define IDM_WINDOW_I_WID_0		240
#define IDM_WINDOW_I_WID_1		241
#define IDM_WINDOW_I_WID_2		242
#define IDM_WINDOW_I_WID_3		243
#define IDM_WINDOW_I_WID_4		244
#define IDM_WINDOW_I_WID_5		245
#define IDM_WINDOW_I_WID_6		246
#define IDM_WINDOW_I_WID_7		247

#define IDM_WINDOW_D_WID_0		250
#define IDM_WINDOW_D_WID_1		251
#define IDM_WINDOW_D_WID_2		252
#define IDM_WINDOW_D_WID_3		253
#define IDM_WINDOW_D_WID_4		254
#define IDM_WINDOW_D_WID_5		255
#define IDM_WINDOW_D_WID_6		256
#define IDM_WINDOW_D_WID_7		257

#define IDM_WINDOW_I_HGT_0		260
#define IDM_WINDOW_I_HGT_1		261
#define IDM_WINDOW_I_HGT_2		262
#define IDM_WINDOW_I_HGT_3		263
#define IDM_WINDOW_I_HGT_4		264
#define IDM_WINDOW_I_HGT_5		265
#define IDM_WINDOW_I_HGT_6		266
#define IDM_WINDOW_I_HGT_7		267

#define IDM_WINDOW_D_HGT_0		270
#define IDM_WINDOW_D_HGT_1		271
#define IDM_WINDOW_D_HGT_2		272
#define IDM_WINDOW_D_HGT_3		273
#define IDM_WINDOW_D_HGT_4		274
#define IDM_WINDOW_D_HGT_5		275
#define IDM_WINDOW_D_HGT_6		276
#define IDM_WINDOW_D_HGT_7		277

#ifdef USE_SYS_FONT

#define IDM_WINDOW_SYSFONT_0	310
#define IDM_WINDOW_SYSFONT_1	311
#define IDM_WINDOW_SYSFONT_2	312
#define IDM_WINDOW_SYSFONT_3	313
#define IDM_WINDOW_SYSFONT_4	314
#define IDM_WINDOW_SYSFONT_5	315
#define IDM_WINDOW_SYSFONT_6	316
#define IDM_WINDOW_SYSFONT_7	317

#endif /* USE_SYS_FONT */

#define IDM_OPTIONS_GRAPHICS	400
#define IDM_OPTIONS_SOUND		401
#define IDM_OPTIONS_SAVER		411

#define IDM_HELP_GENERAL		901
#define IDM_HELP_SPOILERS		902


/*
 * This may need to be removed for some compilers XXX XXX XXX
 */
#define STRICT

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
#define NOCTLMGR          /* Control management and controls */
#define NOHELP            /* Help support */

/*
 * Exclude parts of WINDOWS.H that are not needed (Win32)
 */
#define WIN32_LEAN_AND_MEAN
#define NONLS             /* All NLS defines and routines */
#define NOSERVICE         /* All Service Controller routines, SERVICE_ equates, etc. */
#define NOKANJI           /* Kanji support stuff. */
#define NOMCX             /* Modem Configuration Extensions */

/*
 * Include the "windows" support file
 */
#include <windows.h>

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

/*
 * Include some more files
 */
#include <mmsystem.h>
#include <commdlg.h>

/*
 * Include the support for loading bitmaps
 */
#ifdef USE_GRAPHICS
# include "readdib.h"
#endif

/*
 * Hack -- Fake declarations from "dos.h" XXX XXX XXX
 */
#ifdef WIN32
#define INVALID_FILE_NAME (DWORD)0xFFFFFFFF
#else /* WIN32 */
#define FA_LABEL    0x08        /* Volume label */
#define FA_DIREC    0x10        /* Directory */
unsigned _cdecl _dos_getfileattr(const char *, unsigned *);
#endif /* WIN32 */

/*
 * Silliness in WIN32 drawing routine
 */
#ifdef WIN32
# define MoveTo(H,X,Y) MoveToEx(H, X, Y, NULL)
#endif /* WIN32 */

/*
 * Silliness for Windows 95
 */
#ifndef WS_EX_TOOLWINDOW
# define WS_EX_TOOLWINDOW 0
#endif

/*
 * Foreground color bits (hard-coded by DOS)
 */
#define VID_BLACK	0x00
#define VID_BLUE	0x01
#define VID_GREEN	0x02
#define VID_CYAN	0x03
#define VID_RED		0x04
#define VID_MAGENTA	0x05
#define VID_YELLOW	0x06
#define VID_WHITE	0x07

/*
 * Bright text (hard-coded by DOS)
 */
#define VID_BRIGHT	0x08

/*
 * Background color bits (hard-coded by DOS)
 */
#define VUD_BLACK	0x00
#define VUD_BLUE	0x10
#define VUD_GREEN	0x20
#define VUD_CYAN	0x30
#define VUD_RED		0x40
#define VUD_MAGENTA	0x50
#define VUD_YELLOW	0x60
#define VUD_WHITE	0x70

/*
 * Blinking text (hard-coded by DOS)
 */
#define VUD_BRIGHT	0x80


/*
 * Forward declare
 */
typedef struct _term_data term_data;

/*
 * Extra "term" data
 *
 * Note the use of "font_want" for the names of the font file requested by
 * the user, and the use of "font_file" for the currently active font file.
 *
 * The "font_file" is uppercased, and takes the form "8X13.FON", while
 * "font_want" can be in almost any form as long as it could be construed
 * as attempting to represent the name of a font.
 */
struct _term_data
{
	term t;

	cptr s;

	HWND w;

	DWORD dwStyle;
	DWORD dwExStyle;

	uint keys;

	uint rows;
	uint cols;

	uint pos_x;
	uint pos_y;
	uint size_wid;
	uint size_hgt;
	uint size_ow1;
	uint size_oh1;
	uint size_ow2;
	uint size_oh2;

	bool size_hack;

	bool xtra_hack;

	bool visible;

	bool bizarre;

	cptr font_want;

	cptr font_file;

	HFONT font_id;

#ifdef USE_SYS_FONT

	bool font_sys; /* TRUE if system font desired */
	cptr font_sys_face; /* Font face string_make() */
	uint font_sys_size; /* Font size */
	bool font_sys_bold; /* 1 for bold, 0 for normal */

#endif /* USE_SYS_FONT */

	uint font_wid;
	uint font_hgt;

	uint tile_wid;
	uint tile_hgt;

#ifdef SAVE_DC

	HDC dc;

#endif /* SAVE_DC */
};


/*
 * Maximum number of windows XXX XXX XXX
 */
#define MAX_TERM_DATA 8

/*
 * An array of term_data's
 */
static term_data data[MAX_TERM_DATA];

/*
 * Hack -- global "window creation" pointer
 */
static term_data *my_td;

/*
 * game in progress
 */
bool game_in_progress = FALSE;

/*
 * note when "open"/"new" become valid
 */
bool initialized = FALSE;

/*
 * screen paletted, i.e. 256 colors
 */
bool paletted = FALSE;

/*
 * 16 colors screen, don't use RGB()
 */
bool colors16 = FALSE;

/*
 * Saved instance handle
 */
static HINSTANCE hInstance;

/*
 * Yellow brush for the cursor
 */
static HBRUSH hbrYellow;

/*
 * An icon
 */
static HICON hIcon;

/*
 * A palette
 */
static HPALETTE hPal;


#ifdef USE_SAVER

/*
 * The screen saver window
 */
static HWND hwndSaver;

#endif /* USE_SAVER */


#ifdef USE_GRAPHICS

/*
 * Flag set once "graphics" has been initialized
 */
static bool can_use_graphics = FALSE;

/*
 * The global bitmap
 */
static DIBINIT infGraph;

#endif


#ifdef USE_SOUND

/*
 * Flag set once "sound" has been initialized
 */
static bool can_use_sound = FALSE;

/*
 * An array of sound file names
 */
static cptr sound_file[SOUND_MAX];

#endif /* USE_SOUND */


/*
 * Full path to ANGBAND.INI
 */
static cptr ini_file = NULL;

/*
 * Name of application
 */
static cptr AppName  = "ANGBAND";

/*
 * Name of sub-window type
 */
static cptr AngList  = "AngList";

/*
 * Directory names
 */
static cptr ANGBAND_DIR_XTRA_FONT;
#ifdef USE_GRAPHICS
static cptr ANGBAND_DIR_XTRA_GRAF;
#endif /* USE_GRAPHICS */
static cptr ANGBAND_DIR_XTRA_SOUND;
static cptr ANGBAND_DIR_XTRA_HELP;


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
static BYTE win_pal[256] =
{
	VID_BLACK,					/* Dark */
	VID_WHITE,					/* White */
	VID_CYAN,					/* Slate XXX */
	VID_RED | VID_BRIGHT,		/* Orange XXX */
	VID_RED,					/* Red */
	VID_GREEN,					/* Green */
	VID_BLUE,					/* Blue */
	VID_YELLOW,					/* Umber XXX */
	VID_BLACK | VID_BRIGHT,		/* Light Dark */
	VID_CYAN | VID_BRIGHT,		/* Light Slate XXX */
	VID_MAGENTA,				/* Violet XXX */
	VID_YELLOW | VID_BRIGHT,	/* Yellow */
	VID_MAGENTA | VID_BRIGHT,	/* Light Red XXX */
	VID_GREEN | VID_BRIGHT,		/* Light Green */
	VID_BLUE | VID_BRIGHT,		/* Light Blue */
	VID_YELLOW					/* Light Umber XXX */
};


/*
 * Hack -- define which keys are "special"
 */
static bool special_key[256];

/*
 * Hack -- initialization list for "special_key"
 *
 * We ignore the modifier keys (shift, control, alt, num lock, scroll lock),
 * and the normal keys (escape, tab, return, letters, numbers, etc), but we
 * catch the keypad keys (with and without numlock set, including keypad 5),
 * the function keys (including the "menu" key which maps to F10), and the
 * "pause" key (between scroll lock and numlock).  We also catch a few odd
 * keys which I do not recognize, but which are listed among keys which we
 * do catch, so they should be harmless to catch.
 */
static byte special_key_list[] =
{
	VK_CLEAR,		/* 0x0C (KP<5>) */

	VK_PAUSE,		/* 0x13 (pause) */

	VK_PRIOR,		/* 0x21 (KP<9>) */
	VK_NEXT,		/* 0x22 (KP<3>) */
	VK_END,			/* 0x23 (KP<1>) */
	VK_HOME,		/* 0x24 (KP<7>) */
	VK_LEFT,		/* 0x25 (KP<4>) */
	VK_UP,			/* 0x26 (KP<8>) */
	VK_RIGHT,		/* 0x27 (KP<6>) */
	VK_DOWN,		/* 0x28 (KP<2>) */
	VK_SELECT,		/* 0x29 (?????) */
	VK_PRINT,		/* 0x2A (?????) */
	VK_EXECUTE,		/* 0x2B (?????) */
	VK_SNAPSHOT,	/* 0x2C (?????) */
	VK_INSERT,		/* 0x2D (KP<0>) */
	VK_DELETE,		/* 0x2E (KP<.>) */
	VK_HELP,		/* 0x2F (?????) */

	VK_NUMPAD0,		/* 0x60 (KP<0>) */
	VK_NUMPAD1,		/* 0x61 (KP<1>) */
	VK_NUMPAD2,		/* 0x62 (KP<2>) */
	VK_NUMPAD3,		/* 0x63 (KP<3>) */
	VK_NUMPAD4,		/* 0x64 (KP<4>) */
	VK_NUMPAD5,		/* 0x65 (KP<5>) */
	VK_NUMPAD6,		/* 0x66 (KP<6>) */
	VK_NUMPAD7,		/* 0x67 (KP<7>) */
	VK_NUMPAD8,		/* 0x68 (KP<8>) */
	VK_NUMPAD9,		/* 0x69 (KP<9>) */
	VK_MULTIPLY,	/* 0x6A (KP<*>) */
	VK_ADD,			/* 0x6B (KP<+>) */
	VK_SEPARATOR,	/* 0x6C (?????) */
	VK_SUBTRACT,	/* 0x6D (KP<->) */
	VK_DECIMAL,		/* 0x6E (KP<.>) */
	VK_DIVIDE,		/* 0x6F (KP</>) */

	VK_F1,			/* 0x70 */
	VK_F2,			/* 0x71 */
	VK_F3,			/* 0x72 */
	VK_F4,			/* 0x73 */
	VK_F5,			/* 0x74 */
	VK_F6,			/* 0x75 */
	VK_F7,			/* 0x76 */
	VK_F8,			/* 0x77 */
	VK_F9,			/* 0x78 */
	VK_F10,			/* 0x79 */
	VK_F11,			/* 0x7A */
	VK_F12,			/* 0x7B */
	VK_F13,			/* 0x7C */
	VK_F14,			/* 0x7D */
	VK_F15,			/* 0x7E */
	VK_F16,			/* 0x7F */
	VK_F17,			/* 0x80 */
	VK_F18,			/* 0x81 */
	VK_F19,			/* 0x82 */
	VK_F20,			/* 0x83 */
	VK_F21,			/* 0x84 */
	VK_F22,			/* 0x85 */
	VK_F23,			/* 0x86 */
	VK_F24,			/* 0x87 */

	0
};

#if 0 /* UNUSED */

/*
 * Hack -- given a pathname, point at the filename
 */
static cptr extract_file_name(cptr s)
{
	cptr p;

	/* Start at the end */
	p = s + strlen(s) - 1;

	/* Back up to divider */
	while ((p >= s) && (*p != ':') && (*p != '\\')) p--;

	/* Return file name */
	return (p+1);
}

#endif /* UNUSED */

/*
 * Hack -- given a simple filename, extract the "font size" info
 *
 * Return a pointer to a static buffer holding the capitalized base name.
 */
static char *analyze_font(char *path, int *wp, int *hp)
{
	int wid, hgt;

	char *s, *p;

	/* Start at the end */
	p = path + strlen(path) - 1;

	/* Back up to divider */
	while ((p >= path) && (*p != ':') && (*p != '\\')) --p;

	/* Advance to file name */
	++p;

	/* Capitalize */
	for (s = p; *s; ++s)
	{
		/* Capitalize (be paranoid) */
		if (islower(*s)) *s = toupper(*s);
	}

	/* Find first 'X' */
	s = strchr(p, 'X');

	/* Extract font width */
	wid = atoi(p);

	/* Extract height */
	hgt = s ? atoi(s+1) : 0;

	/* Save results */
	(*wp) = wid;
	(*hp) = hgt;

	/* Result */
	return (p);
}


/*
 * Check for existance of a file
 */
static bool check_file(cptr s)
{
	char path[1024];

#ifdef WIN32

	DWORD attrib;

#else /* WIN32 */

	unsigned int attrib;

#endif /* WIN32 */

	/* Copy it */
	strcpy(path, s);

#ifdef WIN32

	/* Examine */
	attrib = GetFileAttributes(path);

	/* Require valid filename */
	if (attrib == INVALID_FILE_NAME) return (FALSE);

	/* Prohibit directory */
	if (attrib & FILE_ATTRIBUTE_DIRECTORY) return (FALSE);

#else /* WIN32 */

	/* Examine and verify */
	if (_dos_getfileattr(path, &attrib)) return (FALSE);

	/* Prohibit something */
	if (attrib & FA_LABEL) return (FALSE);

	/* Prohibit directory */
	if (attrib & FA_DIREC) return (FALSE);

#endif /* WIN32 */

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

#ifdef WIN32

	DWORD attrib;

#else /* WIN32 */

	unsigned int attrib;

#endif /* WIN32 */

	/* Copy it */
	strcpy(path, s);

	/* Check length */
	i = strlen(path);

	/* Remove trailing backslash */
	if (i && (path[i-1] == '\\')) path[--i] = '\0';

#ifdef WIN32

	/* Examine */
	attrib = GetFileAttributes(path);

	/* Require valid filename */
	if (attrib == INVALID_FILE_NAME) return (FALSE);

	/* Require directory */
	if (!(attrib & FILE_ATTRIBUTE_DIRECTORY)) return (FALSE);

#else /* WIN32 */

	/* Examine and verify */
	if (_dos_getfileattr(path, &attrib)) return (FALSE);

	/* Prohibit something */
	if (attrib & FA_LABEL) return (FALSE);

	/* Require directory */
	if (!(attrib & FA_DIREC)) return (FALSE);

#endif /* WIN32 */

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
 * Get the "size" for a window
 */
static void term_getsize(term_data *td)
{
	RECT rc;

	int wid, hgt;

	/* Paranoia */
	if (td->cols < 1) td->cols = 1;
	if (td->rows < 1) td->rows = 1;

	/* Paranoia */
	if (td != &data[0])
	{
		if (td->cols > 80) td->cols = 80;
		if (td->rows > 24) td->rows = 24;
	}

	/* Window sizes */
	wid = td->cols * td->tile_wid + td->size_ow1 + td->size_ow2;
	hgt = td->rows * td->tile_hgt + td->size_oh1 + td->size_oh2;

	/* Fake window size */
	rc.left = 0;
	rc.right = rc.left + wid;
	rc.top = 0;
	rc.bottom = rc.top + hgt;

	/* XXX XXX XXX */
	/* rc.right += 1; */
	/* rc.bottom += 1; */

	/* Adjust */
	AdjustWindowRectEx(&rc, td->dwStyle, TRUE, td->dwExStyle);

	/* Total size */
	td->size_wid = rc.right - rc.left;
	td->size_hgt = rc.bottom - rc.top;

	/* See CreateWindowEx */
	if (!td->w) return;

	/* Extract actual location */
	GetWindowRect(td->w, &rc);

	/* Save the location */
	td->pos_x = rc.left;
	td->pos_y = rc.top;
}


/*
 * Write the "prefs" for a single term
 */
static void save_prefs_aux(term_data *td, cptr sec_name)
{
	char buf[1024];

	RECT rc;

	/* Paranoia */
	if (!td->w) return;

	/* Visible */
	strcpy(buf, td->visible ? "1" : "0");
	WritePrivateProfileString(sec_name, "Visible", buf, ini_file);

	/* Font */
	strcpy(buf, td->font_file ? td->font_file : "8X13.FON");
	WritePrivateProfileString(sec_name, "Font", buf, ini_file);

	/* Bizarre */
	strcpy(buf, td->bizarre ? "1" : "0");
	WritePrivateProfileString(sec_name, "Bizarre", buf, ini_file);

	/* Tile size (x) */
	wsprintf(buf, "%d", td->tile_wid);
	WritePrivateProfileString(sec_name, "TileWid", buf, ini_file);

	/* Tile size (y) */
	wsprintf(buf, "%d", td->tile_hgt);
	WritePrivateProfileString(sec_name, "TileHgt", buf, ini_file);

	/* Window size (x) */
	wsprintf(buf, "%d", td->cols);
	WritePrivateProfileString(sec_name, "NumCols", buf, ini_file);

	/* Window size (y) */
	wsprintf(buf, "%d", td->rows);
	WritePrivateProfileString(sec_name, "NumRows", buf, ini_file);

	/* Acquire position */
	GetWindowRect(td->w, &rc);

	/* Window position (x) */
	wsprintf(buf, "%d", rc.left);
	WritePrivateProfileString(sec_name, "PositionX", buf, ini_file);

	/* Window position (y) */
	wsprintf(buf, "%d", rc.top);
	WritePrivateProfileString(sec_name, "PositionY", buf, ini_file);

#ifdef USE_SYS_FONT

	/* System font */
	strcpy(buf, td->font_sys ? "1" : "0");
	WritePrivateProfileString(sec_name, "FontSys", buf, ini_file);

	/* This window is using a system font */
	if (td->font_sys)
	{
		/* System font: face */
		strcpy(buf, td->font_sys_face ? td->font_sys_face : "Courier");
		WritePrivateProfileString(sec_name, "FontFace", buf, ini_file);

		/* System font: size */
		wsprintf(buf, "%d", td->font_sys_size);
		WritePrivateProfileString(sec_name, "FontSize", buf, ini_file);

		/* System font: bold */
		strcpy(buf, td->font_sys_bold ? "1" : "0");
		WritePrivateProfileString(sec_name, "FontBold", buf, ini_file);
	}

	/* No system font */
	else
	{
		/* Delete these keys */
		WritePrivateProfileString(sec_name, "FontFace", NULL, ini_file);
		WritePrivateProfileString(sec_name, "FontSize", NULL, ini_file);
		WritePrivateProfileString(sec_name, "FontBold", NULL, ini_file);
	}

#endif /* USE_SYS_FONT */
}


/*
 * Write the "prefs"
 *
 * We assume that the windows have all been initialized
 */
static void save_prefs(void)
{
	int i;

	char buf[128];

	/* Save the "arg_graphics" flag */
	strcpy(buf, arg_graphics ? "1" : "0");
	WritePrivateProfileString("Angband", "Graphics", buf, ini_file);

	/* Save the "arg_sound" flag */
	strcpy(buf, arg_sound ? "1" : "0");
	WritePrivateProfileString("Angband", "Sound", buf, ini_file);

	/* Save window prefs */
	for (i = 0; i < MAX_TERM_DATA; ++i)
	{
		term_data *td = &data[i];

		sprintf(buf, "Term-%d", i);
		
		save_prefs_aux(td, buf);
	}
}


/*
 * Load the "prefs" for a single term
 */
static void load_prefs_aux(term_data *td, cptr sec_name)
{
	char tmp[1024];

	int wid, hgt;

	/* Visible */
	td->visible = (GetPrivateProfileInt(sec_name, "Visible", td->visible, ini_file) != 0);

	/* Desired font, with default */
	GetPrivateProfileString(sec_name, "Font", "8X13.FON", tmp, 127, ini_file);

	/* Bizarre */
	td->bizarre = (GetPrivateProfileInt(sec_name, "Bizarre", td->bizarre, ini_file) != 0);

	/* Analyze font, save desired font name */
	td->font_want = string_make(analyze_font(tmp, &wid, &hgt));

	/* Tile size */	
	td->tile_wid = GetPrivateProfileInt(sec_name, "TileWid", wid, ini_file);
	td->tile_hgt = GetPrivateProfileInt(sec_name, "TileHgt", hgt, ini_file);

	/* Window size */
	td->cols = GetPrivateProfileInt(sec_name, "NumCols", td->cols, ini_file);
	td->rows = GetPrivateProfileInt(sec_name, "NumRows", td->rows, ini_file);

	/* Window position */
	td->pos_x = GetPrivateProfileInt(sec_name, "PositionX", td->pos_x, ini_file);
	td->pos_y = GetPrivateProfileInt(sec_name, "PositionY", td->pos_y, ini_file);

#ifdef USE_SYS_FONT

	/* System font */
	td->font_sys = (GetPrivateProfileInt(sec_name, "FontSys", 0, ini_file) != 0);

	if (td->font_sys)
	{
		/* System font: face */
		GetPrivateProfileString(sec_name, "FontFace", "Courier", tmp, 127, ini_file);
		td->font_sys_face = string_make(tmp);

		/* System font: size */
		td->font_sys_size = GetPrivateProfileInt(sec_name, "FontSize", 9, ini_file);

		/* System font: bold */
		td->font_sys_bold = (GetPrivateProfileInt(sec_name, "FontBold", 0, ini_file) != 0);
	}

#endif /* USE_SYS_FONT */

	/* Update bigscreen variables */
	if (td == &data[0])
	{
		/* Sanity */
		if (td->rows < 24) td->rows = 24;
		if (td->rows > (DUNGEON_HGT + 2)) td->rows = DUNGEON_HGT + 2;

		/* Sanity */
		if (td->cols < 80) td->cols = 80;
		if (td->cols > (DUNGEON_WID + 2)) td->cols = DUNGEON_WID + 2;

		screen_y = td->rows;
		screen_x = td->cols;

		SCREEN_HGT = screen_y - 2;
		SCREEN_WID = screen_x - (COL_MAP + 1);
	}
}


/*
 * Load the "prefs"
 */
static void load_prefs(void)
{
	int i;

	char buf[1024];
	
	/* Extract the "arg_graphics" flag */
	arg_graphics = (GetPrivateProfileInt("Angband", "Graphics", 0, ini_file) != 0);

	/* Extract the "arg_sound" flag */
	arg_sound = (GetPrivateProfileInt("Angband", "Sound", 0, ini_file) != 0);

	/* Load window prefs */
	for (i = 0; i < MAX_TERM_DATA; ++i)
	{
		term_data *td = &data[i];

		sprintf(buf, "Term-%d", i);
		
		load_prefs_aux(td, buf);
	}
}


/*
 * Create the new global palette based on the bitmap palette
 * (if any), and the standard 16 entry palette derived from
 * "win_clr[]" which is used for the basic 16 Angband colors.
 *
 * This function is never called before all windows are ready.
 *
 * This function returns FALSE if the new palette could not be
 * prepared, which should normally be a fatal error.  XXX XXX
 *
 * Note that only some machines actually use a "palette".
 */
static int new_palette(void)
{
	HPALETTE hBmPal;
	HPALETTE hNewPal;
	HDC hdc;
	int i, nEntries;
	int pLogPalSize;
	int lppeSize;
	LPLOGPALETTE pLogPal;
	LPPALETTEENTRY lppe;

	term_data *td;


	/* This makes no sense */
	if (!paletted) return (TRUE);


	/* No palette */
	hBmPal = NULL;

	/* No bitmap */
	lppeSize = 0;
	lppe = NULL;
	nEntries = 0;

#ifdef USE_GRAPHICS

	/* Check the bitmap palette */
	hBmPal = infGraph.hPalette;

	/* Use the bitmap */
	if (hBmPal)
	{
		lppeSize = 256 * sizeof(PALETTEENTRY);
		lppe = (LPPALETTEENTRY)ralloc(lppeSize);
		nEntries = GetPaletteEntries(hBmPal, 0, 255, lppe);
		if ((nEntries == 0) || (nEntries > 220))
		{
			/* Warn the user */
			plog_fmt("Unusable bitmap palette (%d entries)", nEntries);

			/* Cleanup */
			rnfree(lppe, lppeSize);

			/* Fail */
			return (FALSE);
		}
	}

#endif

	/* Size of palette */
	pLogPalSize = sizeof(LOGPALETTE) + (nEntries + 16) * sizeof(PALETTEENTRY);

	/* Allocate palette */
	pLogPal = (LPLOGPALETTE)ralloc(pLogPalSize);

	/* Version */
	pLogPal->palVersion = 0x300;

	/* Make room for bitmap and normal data */
	pLogPal->palNumEntries = nEntries + 16;

	/* Save the bitmap data */
	for (i = 0; i < nEntries; i++)
	{
		pLogPal->palPalEntry[i] = lppe[i];
	}

	/* Save the normal data */
	for (i = 0; i < 16; i++)
	{
		LPPALETTEENTRY p;

		/* Access the entry */
		p = &(pLogPal->palPalEntry[i+nEntries]);

		/* Save the colors */
		p->peRed = GetRValue(win_clr[i]);
		p->peGreen = GetGValue(win_clr[i]);
		p->peBlue = GetBValue(win_clr[i]);

		/* Save the flags */
		p->peFlags = PC_NOCOLLAPSE;
	}

	/* Free something */
	if (lppe) rnfree(lppe, lppeSize);

	/* Create a new palette, or fail */
	hNewPal = CreatePalette(pLogPal);
	if (!hNewPal) quit("Cannot create palette!");

	/* Free the palette */
	rnfree(pLogPal, pLogPalSize);

	/* Main window */
	td = &data[0];

#ifdef SAVE_DC

	/* Realize the palette */
	hdc = td->dc;
	SelectPalette(hdc, hNewPal, 0);
	i = RealizePalette(hdc);
	if (i == 0) quit("Cannot realize palette!");

#else /* not SAVE_DC */
	
	/* Realize the palette */
	hdc = GetDC(td->w);
	SelectPalette(hdc, hNewPal, 0);
	i = RealizePalette(hdc);
	ReleaseDC(td->w, hdc);
	if (i == 0) quit("Cannot realize palette!");

#endif /* not SAVE_DC */

	/* Sub-windows */
	for (i = 1; i < MAX_TERM_DATA; i++)
	{
		td = &data[i];
		
#ifdef SAVE_DC

		hdc = td->dc;
		SelectPalette(hdc, hNewPal, 0);

#else /* not SAVE_DC */

		hdc = GetDC(td->w);
		SelectPalette(hdc, hNewPal, 0);
		ReleaseDC(td->w, hdc);

#endif /* not SAVE_DC */
	}

	/* Delete old palette */
	if (hPal) DeleteObject(hPal);

	/* Save new palette */
	hPal = hNewPal;

	/* Success */
	return (TRUE);
}

#ifdef USE_GRAPHICS

/*
 * Initialize graphics
 */
static bool init_graphics()
{
	/* Initialize once */
	if (!can_use_graphics)
	{
		int wid = 8;
		int hgt = 8;

		cptr name = "8X8.BMP";

		char buf[1024];

		/* Access the bitmap file */
		path_build(buf, 1024, ANGBAND_DIR_XTRA_GRAF, name);

		/* Load the bitmap or quit */
		if (!ReadDIB(data[0].w, buf, &infGraph))
		{
			plog_fmt("Cannot read bitmap file '%s'", name);
			return (FALSE);
		}

		/* Save the new sizes */
		infGraph.CellWidth = wid;
		infGraph.CellHeight = hgt;

		/* Activate a palette */
		if (!new_palette())
		{
			/* Free bitmap XXX XXX XXX */

			/* Oops */
			plog("Cannot activate palette!");
			return (FALSE);
		}

		/* Graphics available */
		can_use_graphics = TRUE;
	}

	/* Result */
	return (can_use_graphics);
}

#endif /* USE_GRAPHICS */

/*
 * Initialize sound
 */
static bool init_sound()
{
	/* Initialize once */
	if (!can_use_sound)
	{
		int i;

		char wav[128];
		char buf[1024];

		/* Prepare the sounds */
		for (i = 1; i < SOUND_MAX; i++)
		{
			/* Extract name of sound file */
			sprintf(wav, "%s.wav", angband_sound_name[i]);

			/* Access the sound */
			path_build(buf, 1024, ANGBAND_DIR_XTRA_SOUND, wav);

			/* Save the sound filename, if it exists */
			if (check_file(buf)) sound_file[i] = string_make(buf);
		}

		/* Sound available */
		can_use_sound = TRUE;
	}
	
	/* Result */
	return (can_use_sound);
}



/*
 * Resize a window
 */
static void term_window_resize(term_data *td)
{
	/* Require window */
	if (!td->w) return;

	/* Resize the window */
	SetWindowPos(td->w, 0, 0, 0,
	             td->size_wid, td->size_hgt,
	             SWP_NOMOVE | SWP_NOZORDER);

	/* Redraw later */
	InvalidateRect(td->w, NULL, TRUE);
}


/*
 * Force the use of a new "font file" for a term_data
 *
 * This function may be called before the "window" is ready
 *
 * This function returns zero only if everything succeeds.
 *
 * Note that the "font name" must be capitalized!!!
 */
static errr term_force_font(term_data *td, cptr path)
{
	int i;

	int wid, hgt;

	char *base;

	char buf[1024];


	/* Forget the old font (if needed) */
	if (td->font_id) DeleteObject(td->font_id);

	/* Forget old font */
	if (td->font_file)
	{
		bool used = FALSE;

		/* Scan windows */
		for (i = 0; i < MAX_TERM_DATA; i++)
		{
			/* Check "screen" */
			if ((td != &data[i]) &&
			    (data[i].font_file) &&
			    (streq(data[i].font_file, td->font_file)))
			{
				used = TRUE;
			}
		}

#ifdef SAVE_DC
		SelectObject(td->dc, GetStockObject(ANSI_FIXED_FONT));
#endif /* SAVE_DC */

		/* Remove unused font resources */
		if (!used) RemoveFontResource(td->font_file);

		/* Free the old name */
		string_free(td->font_file);

		/* Forget it */
		td->font_file = NULL;
	}


	/* No path given */
	if (!path) return (1);


	/* Local copy */
	strcpy(buf, path);

	/* Analyze font path */
	base = analyze_font(buf, &wid, &hgt);

	/* Verify suffix */
	if (!suffix(base, ".FON")) return (1);

	/* Verify file */
	if (!check_file(buf)) return (1);

	/* Load the new font */
	if (!AddFontResource(buf)) return (1);

	/* Save new font name */
	td->font_file = string_make(base);

	/* Remove the "suffix" */
	base[strlen(base)-4] = '\0';

	/* Create the font (using the 'base' of the font file name!) */
	td->font_id = CreateFont(hgt, wid, 0, 0, FW_DONTCARE, 0, 0, 0,
	                         ANSI_CHARSET, OUT_DEFAULT_PRECIS,
	                         CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
	                         FIXED_PITCH | FF_DONTCARE, base);

	/* Hack -- Unknown size */
	if (!wid || !hgt)
	{
		HDC hdcDesktop;
		HFONT hfOld;
		TEXTMETRIC tm;

		/* all this trouble to get the cell size */
		hdcDesktop = GetDC(HWND_DESKTOP);
		hfOld = SelectObject(hdcDesktop, td->font_id);
		GetTextMetrics(hdcDesktop, &tm);
		SelectObject(hdcDesktop, hfOld);
		ReleaseDC(HWND_DESKTOP, hdcDesktop);

		/* Font size info */
		wid = tm.tmAveCharWidth;
		hgt = tm.tmHeight;
	}

	/* Save the size info */
	td->font_wid = wid;
	td->font_hgt = hgt;

	/* Success */
	return (0);
}


#ifdef USE_SYS_FONT

static errr term_init_font_sys(term_data *td)
{
    LOGFONT lf;
	HDC hdcDesktop;
	HFONT hfOld;
	TEXTMETRIC tm;

	if (!td->font_sys) return (1);

	hdcDesktop = GetDC(HWND_DESKTOP);
	lf.lfHeight = -MulDiv(td->font_sys_size,
		GetDeviceCaps(hdcDesktop, LOGPIXELSY), 72);
	ReleaseDC(HWND_DESKTOP, hdcDesktop);

	lf.lfWidth			= 0;
	lf.lfEscapement		= 0;
	lf.lfOrientation	= 0;
	lf.lfWeight			= (td->font_sys_bold) ? FW_BOLD : FW_NORMAL;
	lf.lfItalic			= 0;
	lf.lfUnderline		= 0;
	lf.lfStrikeOut		= 0;
	lf.lfCharSet		= DEFAULT_CHARSET;
	lf.lfOutPrecision	= OUT_TT_PRECIS;
	lf.lfClipPrecision	= CLIP_DEFAULT_PRECIS;
	lf.lfQuality		= DEFAULT_QUALITY;
	lf.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
	(void) strncpy(lf.lfFaceName, td->font_sys_face, 31);

	td->font_id = CreateFontIndirect(&lf);
	if (td->font_id == NULL) return (1);

	/* All this trouble to get the cell size */
	hdcDesktop = GetDC(HWND_DESKTOP);
	hfOld = SelectObject(hdcDesktop, td->font_id);
	GetTextMetrics(hdcDesktop, &tm);
	SelectObject(hdcDesktop, hfOld);
	ReleaseDC(HWND_DESKTOP, hdcDesktop);

	/* Font size info */
	td->font_wid = tm.tmAveCharWidth;
	td->font_hgt = tm.tmHeight;

	/* Success */
	return (0);
}

static BOOL choose_font(term_data *td, LOGFONT *lf, int *pointSize, BOOL setDefault)
{
	CHOOSEFONT cf;
	
    /* Initialize members of the CHOOSEFONT structure. */ 
 
    cf.lStructSize = sizeof(CHOOSEFONT);
    cf.hwndOwner = (HWND)td->w;
    cf.hDC = (HDC)NULL;
    cf.lpLogFont = lf;
    cf.iPointSize = 0;
    cf.Flags = CF_SCREENFONTS;
    cf.rgbColors = RGB(0,0,0);
    cf.lCustData = 0L;
    cf.lpfnHook = (LPCFHOOKPROC)NULL;
    cf.lpTemplateName = (LPSTR)NULL;

    cf.hInstance = (HINSTANCE) NULL;
    cf.lpszStyle = (LPSTR)NULL;
    cf.nFontType = SCREEN_FONTTYPE;
    cf.nSizeMin = 0;
    cf.nSizeMax = 0;

	/* Require fixed width */
	cf.Flags |= CF_FIXEDPITCHONLY;

	/* Warn user about non-existent font/style */
	cf.Flags |= CF_FORCEFONTEXIST;

	/* Use given LOGFONT */
	if (setDefault) cf.Flags |= CF_INITTOLOGFONTSTRUCT;

#if 0
	/* Allow user to edit effects */
	lf->lfUnderline = 0;
	lf->lfStrikeOut = 0;
	cf.Flags |= CF_EFFECTS;
#endif

	/* Interact with the user */
	if (ChooseFont(&cf))
	{
		(*pointSize) = cf.iPointSize / 10;
		return TRUE;
	}

	return FALSE;
}

/*
 * Allow the user to change the font for this window.
 */
static void term_change_font_sys(term_data *td)
{
	HDC hdc;
	LOGFONT lf;
	int pointSize;

	/* Set default values to pass to ChooseFont() */
	if (td->font_sys)
	{
		hdc = GetDC(HWND_DESKTOP);
		lf.lfHeight = -MulDiv(td->font_sys_size, GetDeviceCaps(hdc, LOGPIXELSY), 72);
		ReleaseDC(HWND_DESKTOP, hdc);
	
		lf.lfWidth			= 0;
		lf.lfEscapement		= 0;
		lf.lfOrientation	= 0;
		lf.lfWeight			= (td->font_sys_bold) ? FW_BOLD : FW_NORMAL;
		lf.lfItalic			= 0;
		lf.lfUnderline		= 0;
		lf.lfStrikeOut		= 0;
		lf.lfCharSet		= DEFAULT_CHARSET;
		lf.lfOutPrecision	= OUT_TT_PRECIS;
		lf.lfClipPrecision	= CLIP_DEFAULT_PRECIS;
		lf.lfQuality		= DEFAULT_QUALITY;
		lf.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
		(void) strncpy(lf.lfFaceName, td->font_sys_face, 31);
	}

	/* Ask for a choice */
	if (choose_font(td, &lf, &pointSize, td->font_sys != 0))
	{
		/* Force the font */
		term_force_font(td, NULL);

		string_free(td->font_sys_face);

		/* Create the font object */
		td->font_id = CreateFontIndirect(&lf);

		/* Note system font */
		td->font_sys = 1;
		td->font_sys_face = string_make(lf.lfFaceName);
		td->font_sys_bold = lf.lfWeight > FW_MEDIUM;
		td->font_sys_size = pointSize;

#ifdef SAVE_DC
		SelectObject(td->dc, td->font_id);
#endif /* SAVE_DC */

		{
			HDC hdcDesktop;
			HFONT hfOld;
			TEXTMETRIC tm;
	
			/* all this trouble to get the cell size */
			hdcDesktop = GetDC(HWND_DESKTOP);
			hfOld = SelectObject(hdcDesktop, td->font_id);
			GetTextMetrics(hdcDesktop, &tm);
			SelectObject(hdcDesktop, hfOld);
			ReleaseDC(HWND_DESKTOP, hdcDesktop);
	
			/* Save the size info */
			td->font_wid = tm.tmMaxCharWidth;
			td->font_hgt = tm.tmHeight;
		}

		/* Assume not bizarre */
		td->bizarre = FALSE;

		/* Reset the tile info */
		td->tile_wid = td->font_wid;
		td->tile_hgt = td->font_hgt;

		/* Analyze the font */
		term_getsize(td);

		/* Resize the window */
		term_window_resize(td);
	}
}

#endif /* USE_SYS_FONT */


/*
 * Allow the user to change the font for this window.
 */
static void term_change_font(term_data *td)
{
	OPENFILENAME ofn;

	char tmp[1024] = "";

	/* Extract a default if possible */
	if (td->font_file) strcpy(tmp, td->font_file);

	/* Ask for a choice */
	memset(&ofn, 0, sizeof(ofn));
	ofn.lStructSize = sizeof(ofn);
	ofn.hwndOwner = data[0].w;
	ofn.lpstrFilter = "Angband Font Files (*.fon)\0*.fon\0";
	ofn.nFilterIndex = 1;
	ofn.lpstrFile = tmp;
	ofn.nMaxFile = 128;
	ofn.lpstrInitialDir = ANGBAND_DIR_XTRA_FONT;
	ofn.Flags = OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;
	ofn.lpstrDefExt = "fon";

	/* Force choice if legal */
	if (GetOpenFileName(&ofn))
	{
		/* Force the font */
		if (term_force_font(td, tmp))
		{
			/* Access the standard font file */
			path_build(tmp, 1024, ANGBAND_DIR_XTRA_FONT, "8X13.FON");
			
			/* Force the use of that font */
			(void)term_force_font(td, tmp);
		}

		/* Assume not bizarre */
		td->bizarre = FALSE;

#ifdef USE_SYS_FONT
		/* Note system font */
		td->font_sys = 0;
		string_free(td->font_sys_face);
#endif /* USE_SYS_FONT */

#ifdef SAVE_DC
		SelectObject(td->dc, td->font_id);
#endif /* SAVE_DC */

		/* Reset the tile info */
		td->tile_wid = td->font_wid;
		td->tile_hgt = td->font_hgt;

		/* Analyze the font */
		term_getsize(td);

		/* Resize the window */
		term_window_resize(td);
	}
}



/*
 * Hack -- redraw a term_data
 */
static void term_data_redraw(term_data *td)
{
	/* Activate the term */
	Term_activate(&td->t);

	/* Redraw the contents */
	Term_redraw();

	/* Restore the term */
	Term_activate(term_screen);
}





/*** Function hooks needed by "Term" ***/


#if 0

/*
 * Initialize a new Term
 */
static void Term_init_win(term *t)
{
	/* XXX Unused */
}


/*
 * Nuke an old Term
 */
static void Term_nuke_win(term *t)
{
	/* XXX Unused */
}

#endif


/*
 * Interact with the User
 */
static errr Term_user_win(int n)
{
	/* Success */
	return (0);
}


/*
 * React to global changes
 */
static errr Term_xtra_win_react(void)
{
	int i;


	/* Simple color */
	if (colors16)
	{
		/* Save the default colors */
		for (i = 0; i < 256; i++)
		{
			/* Simply accept the desired colors */
			win_pal[i] = angband_color_table[i][0];
		}
	}

	/* Complex color */
	else
	{
		COLORREF code;

		byte rv, gv, bv;

		bool change = FALSE;

		/* Save the default colors */
		for (i = 0; i < 256; i++)
		{
			/* Extract desired values */
			rv = angband_color_table[i][1];
			gv = angband_color_table[i][2];
			bv = angband_color_table[i][3];

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

	/* Handle "arg_sound" */
	if (use_sound != arg_sound)
	{
		/* Initialize (if needed) */
		if (arg_sound && !init_sound())
		{
			/* Warning */
			plog("Cannot initialize sound!");
			
			/* Cannot enable */
			arg_sound = FALSE;
		}

		/* Change setting */
		use_sound = arg_sound;
	}

#endif


#ifdef USE_GRAPHICS

	/* Handle "arg_graphics" */
	if (use_graphics != arg_graphics)
	{
		/* Initialize (if needed) */
		if (arg_graphics && !init_graphics())
		{
			/* Warning */
			plog("Cannot initialize graphics!");

			/* Cannot enable */
			arg_graphics = FALSE;
		}

		/* Change setting */
		use_graphics = arg_graphics;

		/* Reset visuals */
		reset_visuals(TRUE);
	}

#endif /* USE_GRAPHICS */


	/* Clean up windows */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		term *old = Term;

		term_data *td = &data[i];

		/* Update resized windows */
		if ((td->cols != td->t.wid) || (td->rows != td->t.hgt))
		{
			/* Activate */
			Term_activate(&td->t);

			/* Hack -- Resize the term */
			Term_resize(td->cols, td->rows);

			/* Redraw the contents */
			Term_redraw();

			/* Restore */
			Term_activate(old);
		}
	}

	/* Success */
	return (0);
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
	return 0;
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
 * Hack -- clear the screen
 *
 * Make this more efficient XXX XXX XXX
 */
static errr Term_xtra_win_clear(void)
{
	term_data *td = (term_data*)(Term->data);

	HDC hdc;
	RECT rc;

	/* Rectangle to erase */
	rc.left = td->size_ow1;
	rc.right = rc.left + td->cols * td->tile_wid;
	rc.top = td->size_oh1;
	rc.bottom = rc.top + td->rows * td->tile_hgt;

#ifdef SAVE_DC

	/* Erase it */
	hdc = td->dc;
//	SetBkColor(hdc, RGB(0, 0, 0));
	ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);

#else /* not SAVE_DC */

	/* Erase it */
	hdc = GetDC(td->w);
	SetBkColor(hdc, RGB(0, 0, 0));
	SelectObject(hdc, td->font_id);
	ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);
	ReleaseDC(td->w, hdc);

#endif /* not SAVE_DC */

	/* Success */
	return 0;
}


/*
 * Hack -- make a noise
 */
static errr Term_xtra_win_noise(void)
{
	MessageBeep(MB_ICONASTERISK);
	return (0);
}


/*
 * Hack -- make a sound
 */
static errr Term_xtra_win_sound(int v)
{
	/* Sound disabled */
	if (!use_sound) return (1);

	/* Illegal sound */
	if ((v < 0) || (v >= SOUND_MAX)) return (1);

	/* Unknown sound */
	if (!sound_file[v]) return (1);

#ifdef USE_SOUND

#ifdef WIN32

	/* Play the sound, catch errors */
	return (PlaySound(sound_file[v], 0, SND_FILENAME | SND_ASYNC));

#else /* WIN32 */

	/* Play the sound, catch errors */
	return (sndPlaySound(sound_file[v], SND_ASYNC));

#endif /* WIN32 */

#endif /* USE_SOUND */

	/* Oops */
	return (1);
}


/*
 * Delay for "x" milliseconds
 */
static int Term_xtra_win_delay(int v)
{

#ifdef WIN32

	/* Sleep */
	Sleep(v);

#else /* WIN32 */

	DWORD t;
	MSG msg;

	/* Final count */
	t = GetTickCount() + v;

	/* Wait for it */
	while (GetTickCount() < t)
	{
		/* Handle messages */
		if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

#endif /* WIN32 */

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
	}

	/* Oops */
	return 1;
}



/*
 * Low level graphics (Assumes valid input).
 *
 * Draw a "cursor" at (x,y), using a "yellow box".
 */
static errr Term_curs_win(int x, int y)
{
	term_data *td = (term_data*)(Term->data);

	RECT rc;
	HDC hdc;

	/* Frame the grid */
	rc.left = x * td->tile_wid + td->size_ow1;
	rc.right = rc.left + td->tile_wid;
	rc.top = y * td->tile_hgt + td->size_oh1;
	rc.bottom = rc.top + td->tile_hgt;

#ifdef SAVE_DC

	/* Cursor is done as a yellow "box" */
	hdc = td->dc;
	FrameRect(hdc, &rc, hbrYellow);

#else /* not SAVE_DC */

	/* Cursor is done as a yellow "box" */
	hdc = GetDC(data[0].w);
	FrameRect(hdc, &rc, hbrYellow);
	ReleaseDC(data[0].w, hdc);

#endif /* not SAVE_DC */

	/* Success */
	return 0;
}


/*
 * Low level graphics (Assumes valid input).
 *
 * Erase a "block" of "n" characters starting at (x,y).
 */
static errr Term_wipe_win(int x, int y, int n)
{
	term_data *td = (term_data*)(Term->data);

	HDC hdc;
	RECT rc;

	/* Rectangle to erase in client coords */
	rc.left = x * td->tile_wid + td->size_ow1;
	rc.right = rc.left + n * td->tile_wid;
	rc.top = y * td->tile_hgt + td->size_oh1;
	rc.bottom = rc.top + td->tile_hgt;

#ifdef SAVE_DC

	hdc = td->dc;
//	SetBkColor(hdc, RGB(0, 0, 0));
	ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);

#else /* not SAVE_DC */

	hdc = GetDC(td->w);
	SetBkColor(hdc, RGB(0, 0, 0));
	SelectObject(hdc, td->font_id);
	ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);
	ReleaseDC(td->w, hdc);

#endif /* not SAVE_DC */

	/* Success */
	return 0;
}


/*
 * Low level graphics.  Assumes valid input.
 *
 * Draw several ("n") chars, with an attr, at a given location.
 *
 * All "graphic" data is handled by "Term_pict_win()", below.
 *
 * One would think there is a more efficient method for telling a window
 * what color it should be using to draw with, but perhaps simply changing
 * it every time is not too inefficient.  XXX XXX XXX
 */
static errr Term_text_win(int x, int y, int n, byte a, const char *s)
{
	term_data *td = (term_data*)(Term->data);
	RECT rc;
	HDC hdc;


	/* Total rectangle */
	rc.left = x * td->tile_wid + td->size_ow1;
	rc.right = rc.left + n * td->tile_wid;
	rc.top = y * td->tile_hgt + td->size_oh1;
	rc.bottom = rc.top + td->tile_hgt;

#ifdef SAVE_DC

	/* Acquire DC */
	hdc = td->dc;

#else /* not SAVE_DC */

	/* Acquire DC */
	hdc = GetDC(td->w);

	/* Background color */
	SetBkColor(hdc, RGB(0, 0, 0));

#endif /* not SAVE_DC */

	/* Foreground color */
	if (colors16)
	{
		SetTextColor(hdc, PALETTEINDEX(win_pal[a]));
	}
	else if (paletted)
	{
		SetTextColor(hdc, win_clr[a&0x0F]);
	}
	else
	{
		SetTextColor(hdc, win_clr[a]);
	}

#ifndef SAVE_DC

	/* Use the font */
	SelectObject(hdc, td->font_id);

#endif /* not SAVE_DC */

	/* Bizarre size */
	if (td->bizarre ||
	    (td->tile_hgt != td->font_hgt) ||
	    (td->tile_wid != td->font_wid))
	{
		int i;

		/* Erase complete rectangle */
		ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);

		/* New rectangle */
		rc.left += ((td->tile_wid - td->font_wid) / 2);
		rc.right = rc.left + td->font_wid;
		rc.top += ((td->tile_hgt - td->font_hgt) / 2);
		rc.bottom = rc.top + td->font_hgt;

		/* Dump each character */
		for (i = 0; i < n; i++)
		{
			/* Dump the text */
			ExtTextOut(hdc, rc.left, rc.top, 0, &rc,
	    		       s+i, 1, NULL);

			/* Advance */
			rc.left += td->tile_wid;
			rc.right += td->tile_wid;
		}
	}

	/* Normal size */
	else
	{
		/* Dump the text */
		ExtTextOut(hdc, rc.left, rc.top, ETO_OPAQUE | ETO_CLIPPED, &rc,
	    	       s, n, NULL);
	}

#ifndef SAVE_DC

	/* Release DC */
	ReleaseDC(td->w, hdc);

#endif /* not SAVE_DC */

	/* Success */
	return 0;
}


/*
 * Low level graphics.  Assumes valid input.
 *
 * Draw an array of "special" attr/char pairs at the given location.
 *
 * We use the "Term_pict_win()" function for "graphic" data, which are
 * encoded by setting the "high-bits" of both the "attr" and the "char"
 * data.  We use the "attr" to represent the "row" of the main bitmap,
 * and the "char" to represent the "col" of the main bitmap.  The use
 * of this function is induced by the "higher_pict" flag.
 *
 * If "graphics" is not available, we simply "wipe" the given grids.
 */
static errr Term_pict_win(int x, int y, int n, const byte *ap, const char *cp)
{
#ifdef USE_GRAPHICS

	term_data *td = (term_data*)(Term->data);

	int i;
	int x1, y1, w1, h1;
	int x2, y2, w2, h2;

	HDC hdc;
	HDC hdcSrc;
	HBITMAP hbmSrcOld;

	/* Paranoia */
	if (!use_graphics)
	{
		/* Erase the grids */
		return (Term_wipe_win(x, y, n));
	}

	/* Size of bitmap cell */
	w1 = infGraph.CellWidth;
	h1 = infGraph.CellHeight;

	/* Size of window cell */
	w2 = td->tile_wid;
	h2 = td->tile_hgt;

	/* Location of window cell */
	x2 = x * w2 + td->size_ow1;
	y2 = y * h2 + td->size_oh1;

	/* Info */
	hdc = GetDC(td->w);

	/* More info */
	hdcSrc = CreateCompatibleDC(hdc);
	hbmSrcOld = SelectObject(hdcSrc, infGraph.hBitmap);

	/* Draw attr/char pairs */
	for (i = 0; i < n; i++, x2 += w2)
	{
		byte a = ap[i];
		char c = cp[i];

		/* Extract picture */
		int row = (a & 0x1F);
		int col = (c & 0x1F);

		/* Location of bitmap cell */
		x1 = col * w1;
		y1 = row * h1;

		/* Perfect size */
		if ((w1 == w2) && (h1 == h2))
		{
			/* Copy the picture from the bitmap to the window */
			BitBlt(hdc, x2, y2, w2, h2, hdcSrc, x1, y1, SRCCOPY);
		}

		/* Need to stretch */
		else
		{
			/* Copy the picture from the bitmap to the window */
			StretchBlt(hdc, x2, y2, w2, h2, hdcSrc, x1, y1, w1, h1, SRCCOPY);
		}
	}

	/* Release */
	SelectObject(hdcSrc, hbmSrcOld);
	DeleteDC(hdcSrc);

	/* Release */
	ReleaseDC(td->w, hdc);

#else

	/* Just erase this grid */
	return (Term_wipe_win(x, y, n));

#endif

	/* Success */
	return 0;
}


/*** Other routines ***/


/*
 * Create and initialize a "term_data" given a title
 */
static void term_data_link(term_data *td)
{
	term *t = &td->t;

	/* Initialize the term */
	term_init(t, td->cols, td->rows, td->keys);

	/* Use a "software" cursor */
	t->soft_cursor = TRUE;

	/* Use "Term_pict" for "graphic" data */
	t->higher_pict = TRUE;

	/* Erase with "white space" */
	t->attr_blank = TERM_WHITE;
	t->char_blank = ' ';

#if 0
	/* Prepare the init/nuke hooks */
	t->init_hook = Term_init_win;
	t->nuke_hook = Term_nuke_win;
#endif

	/* Prepare the template hooks */
	t->user_hook = Term_user_win;
	t->xtra_hook = Term_xtra_win;
	t->curs_hook = Term_curs_win;
	t->wipe_hook = Term_wipe_win;
	t->text_hook = Term_text_win;
	t->pict_hook = Term_pict_win;

	/* Remember where we came from */
	t->data = (vptr)(td);
}


/*
 * Create the windows
 *
 * First, instantiate the "default" values, then read the "ini_file"
 * to over-ride selected values, then create the windows, and fonts.
 *
 * Must use SW_SHOW not SW_SHOWNA, since on 256 color display
 * must make active to realize the palette.  XXX XXX XXX
 */
static void init_windows(void)
{
	int i;

	term_data *td;

	char buf[1024];


	/* Main window */
	td = &data[0];
	WIPE(td, term_data);
	td->s = angband_term_name[0];
	td->keys = 1024;
	td->rows = screen_y;
	td->cols = screen_x;
	td->visible = TRUE;
	td->size_ow1 = 2;
	td->size_ow2 = 2;
	td->size_oh1 = 2;
	td->size_oh2 = 2;
	td->pos_x = 7 * 30;
	td->pos_y = 7 * 20;
#ifdef USE_SYS_FONT
	td->font_sys = 0;
	td->font_sys_face = NULL;
	td->font_sys_size = 0;
	td->font_sys_bold = 0;
#endif /* USE_SYS_FONT */
#ifdef SAVE_DC
	td->dc = NULL;
#endif /*SAVE_DC */

	/* Sub windows */
	for (i = 1; i < MAX_TERM_DATA; i++)
	{
		td = &data[i];
		WIPE(td, term_data);
		td->s = angband_term_name[i];
		td->keys = 16;
		td->rows = 24;
		td->cols = 80;
		td->visible = FALSE;
		td->size_ow1 = 1;
		td->size_ow2 = 1;
		td->size_oh1 = 1;
		td->size_oh2 = 1;
		td->pos_x = (7 - i) * 30;
		td->pos_y = (7 - i) * 20;
#ifdef USE_SYS_FONT
		td->font_sys = 0;
		td->font_sys_face = NULL;
		td->font_sys_size = 0;
		td->font_sys_bold = 0;
#endif /* USE_SYS_FONT */
#ifdef SAVE_DC
		td->dc = NULL;
#endif /*SAVE_DC */
	}


	/* Load prefs */
	load_prefs();


	/* Main window (need these before term_getsize gets called) */
	td = &data[0];
	td->dwStyle = (WS_OVERLAPPED | WS_THICKFRAME | WS_SYSMENU |
	               WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_CAPTION |
	               WS_VISIBLE);
	td->dwExStyle = 0;
	td->visible = TRUE;

	/* Sub windows (need these before term_getsize gets called) */
	for (i = 1; i < MAX_TERM_DATA; i++)
	{
		td = &data[i];
		td->dwStyle = (WS_OVERLAPPED | WS_THICKFRAME | WS_SYSMENU);
		td->dwExStyle = (WS_EX_TOOLWINDOW);
	}


	/* All windows */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		td = &data[i];

#ifdef USE_SYS_FONT

		if (term_init_font_sys(td))
		{
			td->font_sys = 0;
			string_free(td->font_sys_face);
			td->font_sys_face = NULL;

#endif /* not USE_SYS_FONT */

		/* Access the standard font file */
		path_build(buf, 1024, ANGBAND_DIR_XTRA_FONT, td->font_want);
			
		/* Activate the chosen font */
		if (term_force_font(td, buf))
		{
			/* Access the standard font file */
			path_build(buf, 1024, ANGBAND_DIR_XTRA_FONT, "8X13.FON");
			
			/* Force the use of that font */
			(void)term_force_font(td, buf);

			/* Oops */
			td->tile_wid = 8;
			td->tile_hgt = 13;

			/* Assume not bizarre */
			td->bizarre = FALSE;
		}

#ifdef USE_SYS_FONT
		}
#endif /* not USE_SYS_FONT */

		/* Analyze the font */
		term_getsize(td);

		/* Resize the window */
		term_window_resize(td);
	}


	/* Sub windows (reverse order) */
	for (i = MAX_TERM_DATA - 1; i >= 1; --i)
	{
		td = &data[i];

		my_td = td;
		td->w = CreateWindowEx(td->dwExStyle, AngList,
		                       td->s, td->dwStyle,
		                       td->pos_x, td->pos_y,
		                       td->size_wid, td->size_hgt,
		                       HWND_DESKTOP, NULL, hInstance, NULL);
		my_td = NULL;
		if (!td->w) quit("Failed to create sub-window");

#ifdef SAVE_DC

		/* This is the only time we need to get the device context */
		td->dc = GetDC(td->w);

		/* Set the font */
		SelectObject(td->dc, td->font_id);

		/* Background color */
		SetBkColor(td->dc, RGB(0, 0, 0));

#endif /* SAVE_DC */

		if (td->visible)
		{
			td->size_hack = TRUE;
			ShowWindow(td->w, SW_SHOW);
			td->size_hack = FALSE;
		}

		term_data_link(td);
		angband_term[i] = &td->t;

		if (td->visible)
		{
			/* Activate the window */
			SetActiveWindow(td->w);

			/* Bring window to top */
			SetWindowPos(td->w, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
		}
	}


	/* Main window */
	td = &data[0];

	/* Main window */
	my_td = td;
	td->w = CreateWindowEx(td->dwExStyle, AppName,
	                       td->s, td->dwStyle,
	                       td->pos_x, td->pos_y,
	                       td->size_wid, td->size_hgt,
	                       HWND_DESKTOP, NULL, hInstance, NULL);
	my_td = NULL;
	if (!td->w) quit("Failed to create Angband window");

#ifdef SAVE_DC

	/* This is the only time we need to get the device context */
	td->dc = GetDC(td->w);

	/* Set the font */
	SelectObject(td->dc, td->font_id);

	/* Background color */
	SetBkColor(td->dc, RGB(0, 0, 0));

#endif /* SAVE_DC */

	term_data_link(td);
	angband_term[0] = &td->t;

	/* Activate the main window */
	SetActiveWindow(td->w);

	/* Bring main window back to top */
	SetWindowPos(td->w, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);


	/* New palette XXX XXX XXX */
	(void)new_palette();


	/* Create a "brush" for drawing the "cursor" */
	hbrYellow = CreateSolidBrush(win_clr[TERM_YELLOW]);


	/* Process pending messages */
	(void)Term_xtra_win_flush();
}

#ifdef LOTSA_MENUS

static bool old_rogue_like_commands = -1;
static bool old_always_pickup = -1;
static bool old_total_winner = -1;

bool angband_keymap_find(cptr str, char *out)
{
	int i, j, ch, n, mode;
	int match, max_len;
	bool cntrl = FALSE;
	char buf[80];


	/* Assume no match */
	out[0] = '\0';

	/* Roguelike */
	if (rogue_like_commands)
	{
		mode = KEYMAP_MODE_ROGUE;
	}

	/* Original */
	else
	{
		mode = KEYMAP_MODE_ORIG;
	}

	ch = str[0];
	n = strlen(str);

	/* XXX Hack -- Convert control sequence */
	if ((n >= 2) && (ch == '^'))
	{
		ch = KTRL(str[1]);
		cntrl = TRUE;
	}

	/*
	 * If the given underlying sequence is a single character,
	 * and no keymap exists for that character, then return the
	 * given character. For example, "w" maps to "w" under the
	 * original keyset.
	 */
	if (((n == 1) || cntrl) && (keymap_act[mode][(byte)ch] == NULL))
	{
		if (cntrl)
		{
			sprintf(out, "Ctrl+%c", toupper(str[1]));
		}
		else
		{
			strcpy(out, str);
		}
		return TRUE;
	}

	/* Printable --> Ascii */
	text_to_ascii(buf, str);

	/* Look for the shortest matching action */
	match = 0;
	max_len = 128;
	for (i = 0; i < 256; i++)
	{
		cptr action = keymap_act[mode][i];

		if (!action) continue;
		for (j = 0; j < n; j++) if (action[j] != buf[j]) break;
		if (j < n) continue;
		while (action[j]) j++;
		if (j < (size_t) max_len)
		{
			max_len = j;
			match = i;
		}
	}

	if (match)
	{
		if (iscntrl(match))
		{
			sprintf(out, "Ctrl+%c", (char) (match + 64));
		}
		else
		{
			sprintf(out, "%c", (char) match);
		}
	}

	return match;
}

/*
 * Set the accelerator for a menu item.
 */
static void set_menu_item(HMENU hm, UINT id, char *text, char *accel)
{
	char buf[128], *p;
	MENUITEMINFO mii;

	if (GetMenuString(hm, id, buf, 127, MF_BYCOMMAND))
	{
		/* Sometimes change the text as well */
		if (text)
		{
			strcpy(buf, text);
		}
		else
		{
			/* Look for and nuke any TAB */
			p = strchr(buf, '\t');
			if (p) *p = '\0';
		}

		/* Sometimes change the accelerator */
		if (accel)
		{
			/* Append TAB and accelerator */
			(void) strcat(buf, "\t");
			(void) strcat(buf, accel);
		}

		/* Update the menu item */
		mii.cbSize = sizeof(mii);
		mii.dwTypeData = buf;
		mii.fType = MFT_STRING;
		mii.fMask = MIIM_TYPE;
		(void) SetMenuItemInfo(hm, id, FALSE, &mii);
	}
}

/*
 * Disable all menu items recursively.
 */
static void disable_all_items(HMENU hm)
{
	int i, n = GetMenuItemCount(hm);

	for (i = 0; i < n; i++)
	{
		MENUITEMINFO mii;

		mii.cbSize = sizeof(MENUITEMINFO);
		mii.fMask = MIIM_SUBMENU;
		mii.hSubMenu = NULL;
		if (GetMenuItemInfo(hm, i, TRUE, &mii) && mii.hSubMenu)
		{
			disable_all_items(mii.hSubMenu);
		}
		else
		{
			EnableMenuItem(hm, i, MF_BYPOSITION | MF_GRAYED);
		}
	}
}

#endif /* LOTSA_MENUS */

/*
 * Prepare the menus
 */
static void setup_menus(void)
{
	int i;

	HMENU hm = GetMenu(data[0].w);

	/* Disable every menu item recursively */
	disable_all_items(hm);

	/* No character available */
	if (!character_generated)
	{
		/* Menu "File", Item "New" */
		EnableMenuItem(hm, IDM_FILE_NEW, MF_BYCOMMAND | MF_ENABLED);

		/* Menu "File", Item "Open" */
		EnableMenuItem(hm, IDM_FILE_OPEN, MF_BYCOMMAND | MF_ENABLED);
	}

	/* A character available */
	if (character_generated && inkey_flag)
	{
		/* Q, retire/suicide */
		EnableMenuItem(hm, 1000 + 'Q', MF_BYCOMMAND | MF_ENABLED);

		/* Control-S, save without quit */
		EnableMenuItem(hm, 1200 + 'S', MF_BYCOMMAND | MF_ENABLED);

		/* Control-X, save and quit */
		EnableMenuItem(hm, 1200 + 'X', MF_BYCOMMAND | MF_ENABLED);
	}

	/* Menu "File", Item "Exit" */
	EnableMenuItem(hm, IDM_FILE_EXIT, MF_BYCOMMAND | MF_ENABLED);

	/* Menu "Window::Visibility" */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		CheckMenuItem(hm, IDM_WINDOW_VIS_0 + i,
			(data[i].visible ? MF_CHECKED : MF_UNCHECKED));

		EnableMenuItem(hm, IDM_WINDOW_VIS_0 + i,
			MF_BYCOMMAND | MF_ENABLED);
	}

	/* Menu "Window::Font" */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		if (data[i].visible)
		{
			EnableMenuItem(hm, IDM_WINDOW_FONT_0 + i,
				MF_BYCOMMAND | MF_ENABLED);
		}
	}

	/* Menu "Window::System Font" */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		if (data[i].visible)
		{
			EnableMenuItem(hm, IDM_WINDOW_SYSFONT_0 + i,
				MF_BYCOMMAND | MF_ENABLED);
		}
	}

#ifdef USE_GRAPHICS
	/* Menu "Options", Item "Graphics" */
	EnableMenuItem(hm, IDM_OPTIONS_GRAPHICS, MF_ENABLED);
#endif

#ifdef USE_SOUND
	/* Menu "Options", Item "Sound" */
	CheckMenuItem(hm, IDM_OPTIONS_SOUND,
		(arg_sound ? MF_CHECKED : MF_UNCHECKED));

	if (game_in_progress && inkey_flag)
	{
		/* Menu "Options", Item "Sound" */
		EnableMenuItem(hm, IDM_OPTIONS_SOUND, MF_ENABLED);
	}
#endif

#ifdef USE_SAVER
	/* Menu "Options", Item "ScreenSaver" */
	EnableMenuItem(hm, IDM_OPTIONS_SAVER, MF_BYCOMMAND | MF_ENABLED);
#endif

#ifdef LOTSA_MENUS

	/* Suicide is either "Retire" or "Kill Character */
	if (old_total_winner != p_ptr->total_winner)
	{
		char *t = "Q", out[80];
		(void) angband_keymap_find(t, out);
		set_menu_item(hm, 1000 + 'Q',
			p_ptr->total_winner ? "Retire" : "Kill Character",
			out[0] ? out : NULL);
		old_total_winner = p_ptr->total_winner;
	}

	/* Walk/Stay respects the "always_pickup" option */
	if (old_always_pickup != always_pickup)
	{
		old_always_pickup = always_pickup;
		angband_keymap_flag = TRUE;

		if (always_pickup)
		{
			set_menu_item(hm, 1000 + ';', "Walk (With Pickup)", NULL);
			set_menu_item(hm, 1000 + '-', "Walk (No Pickup)", NULL);
			set_menu_item(hm, 1000 + ',', "Stay (With Pickup)", NULL);
		}
		else
		{
			set_menu_item(hm, 1000 + ';', "Walk (No Pickup)", NULL);
			set_menu_item(hm, 1000 + '-', "Walk (With Pickup)", NULL);
			set_menu_item(hm, 1000 + ',', "Stay (No Pickup)", NULL);
		}
	}

	/* If keymap changes, update the menus */
	if (old_rogue_like_commands != rogue_like_commands)
	{
		old_rogue_like_commands = rogue_like_commands;
		angband_keymap_flag = TRUE;
	}

	/*
	 * Note: Control-key keymaps have menu item ids >= 1200.
	 * Other keymaps are >= 1000. So to get the menu item id for ^R you
	 * have 1200 + 'R'.
	 */

	/* Keymap changed, update menus */
	if (angband_keymap_flag)
	{
		char cmdChars[] =
			"abcdefgijklnoqrstuvwz"
			"ABCDEFGILMQRSTV"
			"@%^&*(){}]-+=;:\",<.>\\/~?";
		int i;

		for (i = 0; cmdChars[i]; i++)
		{
			UINT id = 1000 + cmdChars[i];
			char buf[2], out[80];
			(void)sprintf(buf, "%c", cmdChars[i]);
			if (angband_keymap_find(buf, out))
				set_menu_item(hm, id, NULL, out);
		}
	}

	/* Keymap changed, update menus */
	if (angband_keymap_flag)
	{
		char cmdChars[] = "FPRSTX";
		int i;

		for (i = 0; cmdChars[i]; i++)
		{
			UINT id = 1200 + cmdChars[i];
			char buf[3], out[80];
			(void)sprintf(buf, "^%c", cmdChars[i]);
			if (angband_keymap_find(buf, out))
				set_menu_item(hm, id, NULL, out);
		}
	}

	/* We saw the keymap change */
	angband_keymap_flag = FALSE;

	/* Enable commands if allowed */
	if (game_in_progress && inkey_flag)
	{
		char cmdChars[] =
			"abcdefgijklnoqrstuvwz"
			"ABCDEFGILMQRSTV"
			"@%^&*(){}]-+=;:\",<.>\\/~?";
		int i;

		for (i = 0; cmdChars[i]; i++)
		{
			UINT id = 1000 + cmdChars[i];
			EnableMenuItem(hm, id, MF_BYCOMMAND | MF_ENABLED);
		}
	}

	/* Enable commands if allowed */
	if (game_in_progress && inkey_flag)
	{
		char cmdChars[] = "FPRSTX";
		int i;

		for (i = 0; cmdChars[i]; i++)
		{
			UINT id = 1200 + cmdChars[i];
			EnableMenuItem(hm, id, MF_BYCOMMAND | MF_ENABLED);
		}
	}

#endif /* LOTSA_MENUS */
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

	/* Second arg */
	p = strchr(s, ' ');

	/* Tokenize, advance */
	if (p) *p++ = '\0';

	/* No args */
	if (!*s) return;

	/* Extract filename */
	strcat(savefile, s);

	/* Validate the file */
	validate_file(savefile);

	/* Game in progress */
	game_in_progress = TRUE;

	/* Play game */
	play_game(FALSE);
}


/*
 * Process a menu command
 */
static void process_menus(WORD wCmd)
{
	int i;

	term_data *td;

	OPENFILENAME ofn;

	/* Analyze */
	switch (wCmd)
	{
		/* New game */
		case IDM_FILE_NEW:
		{
			if (!initialized)
			{
				plog("You cannot do that yet...");
			}
			else if (game_in_progress)
			{
				plog("You can't start a new game while you're still playing!");
			}
			else
			{
				game_in_progress = TRUE;
				Term_flush();
				play_game(TRUE);
				quit(NULL);
			}
			break;
		}

		/* Open game */
		case IDM_FILE_OPEN:
		{
			if (!initialized)
			{
				plog("You cannot do that yet...");
			}
			else if (game_in_progress)
			{
				plog("You can't open a new game while you're still playing!");
			}
			else
			{
				memset(&ofn, 0, sizeof(ofn));
				ofn.lStructSize = sizeof(ofn);
				ofn.hwndOwner = data[0].w;
				ofn.lpstrFilter = "Save Files (*.)\0*\0";
				ofn.nFilterIndex = 1;
				ofn.lpstrFile = savefile;
				ofn.nMaxFile = 1024;
				ofn.lpstrInitialDir = ANGBAND_DIR_SAVE;
				ofn.Flags = OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;

				if (GetOpenFileName(&ofn))
				{
					/* Load 'savefile' */
					validate_file(savefile);
					game_in_progress = TRUE;
					Term_flush();
					play_game(FALSE);
					quit(NULL);
				}
			}
			break;
		}

		/* Exit */
		case IDM_FILE_EXIT:
		{
			if (game_in_progress && character_generated)
			{
				/* XXX XXX XXX */
				if (MessageBox(data[0].w,
					"Your character will be not saved!", "Warning",
					MB_ICONEXCLAMATION | MB_OKCANCEL) == IDCANCEL)
				{
					break;
				}
			}
			quit(NULL);
			break;
		}

		case IDM_WINDOW_VIS_0:
		{
			plog("You are not allowed to do that!");

			break;
		}

		/* Window visibility */
		case IDM_WINDOW_VIS_1:
		case IDM_WINDOW_VIS_2:
		case IDM_WINDOW_VIS_3:
		case IDM_WINDOW_VIS_4:
		case IDM_WINDOW_VIS_5:
		case IDM_WINDOW_VIS_6:
		case IDM_WINDOW_VIS_7:
		{
			i = wCmd - IDM_WINDOW_VIS_0;

			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
			
			if (!td->visible)
			{
				td->visible = TRUE;
				ShowWindow(td->w, SW_SHOW);
				term_data_redraw(td);
			}
			else
			{
				td->visible = FALSE;
				ShowWindow(td->w, SW_HIDE);
			}

			break;
		}

		/* Window fonts */
		case IDM_WINDOW_FONT_0:
		case IDM_WINDOW_FONT_1:
		case IDM_WINDOW_FONT_2:
		case IDM_WINDOW_FONT_3:
		case IDM_WINDOW_FONT_4:
		case IDM_WINDOW_FONT_5:
		case IDM_WINDOW_FONT_6:
		case IDM_WINDOW_FONT_7:
		{
			i = wCmd - IDM_WINDOW_FONT_0;

			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
			
			term_change_font(td);

			break;
		}

#ifdef USE_SYS_FONT

		/* Window fonts */
		case IDM_WINDOW_SYSFONT_0:
		case IDM_WINDOW_SYSFONT_1:
		case IDM_WINDOW_SYSFONT_2:
		case IDM_WINDOW_SYSFONT_3:
		case IDM_WINDOW_SYSFONT_4:
		case IDM_WINDOW_SYSFONT_5:
		case IDM_WINDOW_SYSFONT_6:
		case IDM_WINDOW_SYSFONT_7:
		{
			i = wCmd - IDM_WINDOW_SYSFONT_0;

			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
			
			term_change_font_sys(td);

			break;
		}

#endif /* USE_SYS_FONT */

		/* Bizarre Display */
		case IDM_WINDOW_BIZ_0:
		case IDM_WINDOW_BIZ_1:
		case IDM_WINDOW_BIZ_2:
		case IDM_WINDOW_BIZ_3:
		case IDM_WINDOW_BIZ_4:
		case IDM_WINDOW_BIZ_5:
		case IDM_WINDOW_BIZ_6:
		case IDM_WINDOW_BIZ_7:
		{
			i = wCmd - IDM_WINDOW_BIZ_0;

			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
			
			td->bizarre = !td->bizarre;

			term_getsize(td);

			term_window_resize(td);

			break;
		}

		/* Increase Tile Width */
		case IDM_WINDOW_I_WID_0:
		case IDM_WINDOW_I_WID_1:
		case IDM_WINDOW_I_WID_2:
		case IDM_WINDOW_I_WID_3:
		case IDM_WINDOW_I_WID_4:
		case IDM_WINDOW_I_WID_5:
		case IDM_WINDOW_I_WID_6:
		case IDM_WINDOW_I_WID_7:
		{
			i = wCmd - IDM_WINDOW_I_WID_0;

			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
			
			td->tile_wid += 1;
			
			term_getsize(td);

			term_window_resize(td);

			break;
		}

		/* Decrease Tile Height */
		case IDM_WINDOW_D_WID_0:
		case IDM_WINDOW_D_WID_1:
		case IDM_WINDOW_D_WID_2:
		case IDM_WINDOW_D_WID_3:
		case IDM_WINDOW_D_WID_4:
		case IDM_WINDOW_D_WID_5:
		case IDM_WINDOW_D_WID_6:
		case IDM_WINDOW_D_WID_7:
		{
			i = wCmd - IDM_WINDOW_D_WID_0;

			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
			
			td->tile_wid -= 1;
			
			term_getsize(td);

			term_window_resize(td);

			break;
		}

		/* Increase Tile Height */
		case IDM_WINDOW_I_HGT_0:
		case IDM_WINDOW_I_HGT_1:
		case IDM_WINDOW_I_HGT_2:
		case IDM_WINDOW_I_HGT_3:
		case IDM_WINDOW_I_HGT_4:
		case IDM_WINDOW_I_HGT_5:
		case IDM_WINDOW_I_HGT_6:
		case IDM_WINDOW_I_HGT_7:
		{
			i = wCmd - IDM_WINDOW_I_HGT_0;

			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
			
			td->tile_hgt += 1;
			
			term_getsize(td);

			term_window_resize(td);

			break;
		}

		/* Decrease Tile Height */
		case IDM_WINDOW_D_HGT_0:
		case IDM_WINDOW_D_HGT_1:
		case IDM_WINDOW_D_HGT_2:
		case IDM_WINDOW_D_HGT_3:
		case IDM_WINDOW_D_HGT_4:
		case IDM_WINDOW_D_HGT_5:
		case IDM_WINDOW_D_HGT_6:
		case IDM_WINDOW_D_HGT_7:
		{
			i = wCmd - IDM_WINDOW_D_HGT_0;

			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
			
			td->tile_hgt -= 1;
			
			term_getsize(td);

			term_window_resize(td);

			break;
		}

		case IDM_OPTIONS_GRAPHICS:
		{
			/* Paranoia */
			if (!inkey_flag)
			{
				plog("You may not do that right now.");
				break;
			}

			/* Toggle "arg_graphics" */
			arg_graphics = !arg_graphics;

			/* React to changes */
			Term_xtra_win_react();

			/* Hack -- Force redraw */
			Term_key_push(KTRL('R'));

			break;
		}

		case IDM_OPTIONS_SOUND:
		{
			/* Paranoia */
			if (!inkey_flag)
			{
				plog("You may not do that right now.");
				break;
			}

			/* Toggle "arg_sound" */
			arg_sound = !arg_sound;

			/* React to changes */
			Term_xtra_win_react();

			/* Hack -- Force redraw */
			Term_key_push(KTRL('R'));

			break;
		}

#ifdef USE_SAVER

		case IDM_OPTIONS_SAVER:
		{
			if (hwndSaver)
			{
				DestroyWindow(hwndSaver);
				hwndSaver = NULL;
			}
			else
			{
				/* Create a screen scaver window */
				hwndSaver = CreateWindowEx(WS_EX_TOPMOST, "WindowsScreenSaverClass",
				                           "Angband Screensaver",
				                           WS_POPUP | WS_MAXIMIZE | WS_VISIBLE,
				                           0, 0, GetSystemMetrics(SM_CXSCREEN),
				                           GetSystemMetrics(SM_CYSCREEN),
				                           NULL, NULL, hInstance, NULL);

				if (hwndSaver)
				{
					/* Push the window to the bottom XXX XXX XXX */
					SetWindowPos(hwndSaver, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
				}
				else
				{
					plog("Failed to create saver window");
				}
			}
			break;
		}

#endif

		case IDM_HELP_GENERAL:
		{
			char buf[1024];
			char tmp[1024];
			path_build(tmp, 1024, ANGBAND_DIR_XTRA_HELP, "angband.hlp");
			if (check_file(tmp))
			{
				sprintf(buf, "winhelp.exe %s", tmp);
				WinExec(buf, SW_NORMAL);
			}
			else
			{
				plog_fmt("Cannot find help file: %s", tmp);
				plog("Use the online help files instead.");
			}
			break;
		}

		case IDM_HELP_SPOILERS:
		{
			char buf[1024];
			char tmp[1024];
			path_build(tmp, 1024, ANGBAND_DIR_XTRA_HELP, "spoilers.hlp");
			if (check_file(tmp))
			{
				sprintf(buf, "winhelp.exe %s", tmp);
				WinExec(buf, SW_NORMAL);
			}
			else
			{
				plog_fmt("Cannot find help file: %s", tmp);
				plog("Use the online help files instead.");
			}
			break;
		}

		default:
		{
			/* Bypass keymap */
			Term_keypress('\\');

			/* XXX Hack -- Bypass keymap */
			if (wCmd == 1000 + '\\') break;

			if (wCmd >= 1200)
			{
				/* Control key */
				Term_keypress(KTRL(wCmd - 1200));
			}
			else if (wCmd >= 1000)
			{
				/* Regular key */
				Term_keypress(wCmd - 1000);
			}
			break;
		}
	}
}


static LPARAM special_param = 0;

#ifdef __MWERKS__
LRESULT FAR PASCAL AngbandWndProc(HWND hWnd, UINT uMsg,
                                  WPARAM wParam, LPARAM lParam);
LRESULT FAR PASCAL AngbandWndProc(HWND hWnd, UINT uMsg,
                                  WPARAM wParam, LPARAM lParam)
#else /* __MWERKS__ */
LRESULT FAR PASCAL _export AngbandWndProc(HWND hWnd, UINT uMsg,
                                          WPARAM wParam, LPARAM lParam)
#endif /* __MWERKS__ */
{
	PAINTSTRUCT ps;
	HDC hdc;
	term_data *td;
	MINMAXINFO FAR *lpmmi;
	RECT rc;
	int i;


	/* Acquire proper "term_data" info */
	td = (term_data *)GetWindowLong(hWnd, 0);

	/* Handle message */
	switch (uMsg)
	{
		/* XXX XXX XXX */
		case WM_NCCREATE:
		{
			SetWindowLong(hWnd, 0, (LONG)(my_td));
			break;
		}

		/* XXX XXX XXX */
		case WM_CREATE:
		{
			return 0;
		}

		case WM_GETMINMAXINFO:
		{
			lpmmi = (MINMAXINFO FAR *)lParam;

			/* this message was sent before WM_NCCREATE */
			if (!td) return 1;

			/* Minimum window size is 8x2 */
			rc.left = rc.top = 0;
			rc.right = rc.left + 8 * td->tile_wid + td->size_ow1 + td->size_ow2;
			rc.bottom = rc.top + 2 * td->tile_hgt + td->size_oh1 + td->size_oh2 + 1;

			/* Adjust */
			AdjustWindowRectEx(&rc, td->dwStyle, TRUE, td->dwExStyle);

			/* Save minimum size */
			lpmmi->ptMinTrackSize.x = rc.right - rc.left;
			lpmmi->ptMinTrackSize.y = rc.bottom - rc.top;

			/* Maximum window size */
			rc.left = rc.top = 0;
			rc.right = rc.left + screen_x * td->tile_wid + td->size_ow1 + td->size_ow2;
			rc.bottom = rc.top + screen_y * td->tile_hgt + td->size_oh1 + td->size_oh2;

			/* Paranoia */
			rc.right  += (td->tile_wid - 1);
			rc.bottom += (td->tile_hgt - 1);

			/* Adjust */
			AdjustWindowRectEx(&rc, td->dwStyle, TRUE, td->dwExStyle);

			/* Save maximum size */
			lpmmi->ptMaxSize.x = rc.right - rc.left;
			lpmmi->ptMaxSize.y = rc.bottom - rc.top;

			/* Save maximum size */
			lpmmi->ptMaxTrackSize.x = rc.right - rc.left;
			lpmmi->ptMaxTrackSize.y = rc.bottom - rc.top;

			return 0;
		}

		case WM_PAINT:
		{
			BeginPaint(hWnd, &ps);
			if (td) term_data_redraw(td);
			EndPaint(hWnd, &ps);
			ValidateRect(hWnd, NULL);
			return 0;
		}

		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
		{
			bool mc = FALSE;
			bool ms = FALSE;
			bool ma = FALSE;

			/* Extract the modifiers */
			if (GetKeyState(VK_CONTROL) & 0x8000) mc = TRUE;
			if (GetKeyState(VK_SHIFT)   & 0x8000) ms = TRUE;
			if (GetKeyState(VK_MENU)    & 0x8000) ma = TRUE;

			/* Handle "special" keys */
			if (special_key[(byte)(wParam)])
			{
				/* Begin the macro trigger */
				Term_keypress(31);

				/* Send the modifiers */
				if (mc) Term_keypress('C');
				if (ms) Term_keypress('S');
				if (ma) Term_keypress('A');

				/* Extract "scan code" */
				i = LOBYTE(HIWORD(lParam));

				/* Introduce the scan code */
				Term_keypress('x');

				/* Encode the hexidecimal scan code */
				Term_keypress(hexsym[i/16]);
				Term_keypress(hexsym[i%16]);

				/* End the macro trigger */
				Term_keypress(13);

				special_param = lParam;

				return 0;
			}
			break;
		}

		case WM_CHAR:
		{
			if (special_param != lParam)
			{
				Term_keypress(wParam);
			}
			special_param = 0;
			return 0;
		}

		case WM_INITMENU:
		{
			setup_menus();
			return 0;
		}

		case WM_CLOSE:
		{
			if (game_in_progress && character_generated)
			{
				/* Hack -- Forget messages */
				msg_flag = FALSE;

				/* Save the game */
				do_cmd_save_game(FALSE);
			}
			quit(NULL);
			return 0;
		}

		case WM_QUIT:
		{
			quit(NULL);
			return 0;
		}

		case WM_COMMAND:
		{
			process_menus(LOWORD(wParam));
			return 0;
		}

		case WM_SIZE:
		{
			/* this message was sent before WM_NCCREATE */
			if (!td) return 1;

			/* it was sent from inside CreateWindowEx */
			if (!td->w) return 1;

			/* was sent from WM_SIZE */
			if (td->size_hack) return 1;

			switch (wParam)
			{
				case SIZE_MINIMIZED:
				{
					/* Hide sub-windows */
					for (i = 1; i < MAX_TERM_DATA; i++)
					{
						if (data[i].visible) ShowWindow(data[i].w, SW_HIDE);
					}
					return 0;
				}

				case SIZE_MAXIMIZED:
				{
					/* fall through XXX XXX XXX */
				}

				case SIZE_RESTORED:
				{
					td->size_hack = TRUE;

					td->cols = (LOWORD(lParam) - td->size_ow1 - td->size_ow2) / td->tile_wid;
					td->rows = (HIWORD(lParam) - td->size_oh1 - td->size_oh2) / td->tile_hgt;

					term_getsize(td);

					MoveWindow(hWnd, td->pos_x, td->pos_y, td->size_wid, td->size_hgt, TRUE);

					td->size_hack = FALSE;

					/* Restore sub-windows */
					for (i = 1; i < MAX_TERM_DATA; i++)
					{
						if (data[i].visible) ShowWindow(data[i].w, SW_SHOWNOACTIVATE);
					}

					return 0;
				}
			}
			break;
		}

		case WM_PALETTECHANGED:
		{
			/* Ignore if palette change caused by itself */
			if ((HWND)wParam == hWnd) return 0;

			/* Fall through... */
		}

		case WM_QUERYNEWPALETTE:
		{
			if (!paletted) return 0;

#ifdef SAVE_DC

			hdc = td->dc;
			SelectPalette(hdc, hPal, FALSE);
			i = RealizePalette(hdc);

			/* if any palette entries changed, repaint the window. */
			if (i) InvalidateRect(hWnd, NULL, TRUE);

#else /* not SAVE_DC */

			hdc = GetDC(hWnd);

			SelectPalette(hdc, hPal, FALSE);

			i = RealizePalette(hdc);

			/* if any palette entries changed, repaint the window. */
			if (i) InvalidateRect(hWnd, NULL, TRUE);

			ReleaseDC(hWnd, hdc);

#endif /* not SAVE_DC */

			return 0;
		}

		case WM_ACTIVATE:
		{
			if (wParam && !HIWORD(lParam))
			{
				/* Do something to sub-windows */
				for (i = 1; i < MAX_TERM_DATA; i++)
				{
					SetWindowPos(data[i].w, hWnd, 0, 0, 0, 0,
					             SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE);
				}

				/* Focus on main window */
				SetFocus(hWnd);

				return 0;
			}

			break;
		}
	}

	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}


#ifdef __MWERKS__
LRESULT FAR PASCAL AngbandListProc(HWND hWnd, UINT uMsg,
                                           WPARAM wParam, LPARAM lParam);
LRESULT FAR PASCAL AngbandListProc(HWND hWnd, UINT uMsg,
                                           WPARAM wParam, LPARAM lParam)
#else /* __MWERKS__ */
LRESULT FAR PASCAL _export AngbandListProc(HWND hWnd, UINT uMsg,
                                           WPARAM wParam, LPARAM lParam)
#endif /* __MWERKS__ */
{
	term_data *td;
	MINMAXINFO FAR *lpmmi;
	RECT rc;
	PAINTSTRUCT ps;
	HDC hdc;
	int i;


	/* Acquire proper "term_data" info */
	td = (term_data *)GetWindowLong(hWnd, 0);

	/* Process message */
	switch (uMsg)
	{
		/* XXX XXX XXX */
		case WM_NCCREATE:
		{
			SetWindowLong(hWnd, 0, (LONG)(my_td));
			break;
		}

		/* XXX XXX XXX */
		case WM_CREATE:
		{
			return 0;
		}

		case WM_GETMINMAXINFO:
		{
			/* this message was sent before WM_NCCREATE */
			if (!td) return 1;

			lpmmi = (MINMAXINFO FAR *)lParam;

			/* Minimum size */
			rc.left = rc.top = 0;
			rc.right = rc.left + 8 * td->tile_wid + td->size_ow1 + td->size_ow2;
			rc.bottom = rc.top + 2 * td->tile_hgt + td->size_oh1 + td->size_oh2;

			/* Adjust */
			AdjustWindowRectEx(&rc, td->dwStyle, TRUE, td->dwExStyle);

			/* Save the minimum size */
			lpmmi->ptMinTrackSize.x = rc.right - rc.left;
			lpmmi->ptMinTrackSize.y = rc.bottom - rc.top;

			/* Maximum window size */
			rc.left = rc.top = 0;
			rc.right = rc.left + 80 * td->tile_wid + td->size_ow1 + td->size_ow2;
			rc.bottom = rc.top + 24 * td->tile_hgt + td->size_oh1 + td->size_oh2;

			/* Paranoia */
			rc.right += (td->tile_wid - 1);
			rc.bottom += (td->tile_hgt - 1);

			/* Adjust */
			AdjustWindowRectEx(&rc, td->dwStyle, TRUE, td->dwExStyle);

			/* Save maximum size */
			lpmmi->ptMaxSize.x = rc.right - rc.left;
			lpmmi->ptMaxSize.y = rc.bottom - rc.top;

			/* Save the maximum size */
			lpmmi->ptMaxTrackSize.x = rc.right - rc.left;
			lpmmi->ptMaxTrackSize.y = rc.bottom - rc.top;

			return 0;
		}

		case WM_SIZE:
		{
			/* this message was sent before WM_NCCREATE */
			if (!td) return 1;

			/* it was sent from inside CreateWindowEx */
			if (!td->w) return 1;

			/* was sent from inside WM_SIZE */
			if (td->size_hack) return 1;

			td->size_hack = TRUE;

			td->cols = (LOWORD(lParam) - td->size_ow1 - td->size_ow2) / td->tile_wid;
			td->rows = (HIWORD(lParam) - td->size_oh1 - td->size_oh2) / td->tile_hgt;

			term_getsize(td);

			MoveWindow(hWnd, td->pos_x, td->pos_y, td->size_wid, td->size_hgt, TRUE);

			td->size_hack = FALSE;

			return 0;
		}

		case WM_PAINT:
		{
			BeginPaint(hWnd, &ps);
			if (td) term_data_redraw(td);
			EndPaint(hWnd, &ps);
			return 0;
		}

		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
		{
			bool mc = FALSE;
			bool ms = FALSE;
			bool ma = FALSE;

			/* Extract the modifiers */
			if (GetKeyState(VK_CONTROL) & 0x8000) mc = TRUE;
			if (GetKeyState(VK_SHIFT)   & 0x8000) ms = TRUE;
			if (GetKeyState(VK_MENU)    & 0x8000) ma = TRUE;

			/* Handle "special" keys */
			if (special_key[(byte)(wParam)])
			{
				/* Begin the macro trigger */
				Term_keypress(31);

				/* Send the modifiers */
				if (mc) Term_keypress('C');
				if (ms) Term_keypress('S');
				if (ma) Term_keypress('A');

				/* Extract "scan code" */
				i = LOBYTE(HIWORD(lParam));

				/* Introduce the scan code */
				Term_keypress('x');

				/* Encode the hexidecimal scan code */
				Term_keypress(hexsym[i/16]);
				Term_keypress(hexsym[i%16]);

				/* End the macro trigger */
				Term_keypress(13);

				special_param = lParam;

				return 0;
			}

			break;
		}

		case WM_CHAR:
		{
			if (special_param != lParam) {
				Term_keypress(wParam);
			}
			special_param = 0;
			return 0;
		}

		case WM_PALETTECHANGED:
		{
			/* ignore if palette change caused by itself */
			if ((HWND)wParam == hWnd) return FALSE;
			/* otherwise, fall through!!! */
		}

		case WM_QUERYNEWPALETTE:
		{
			if (!paletted) return 0;

#ifdef SAVE_DC

			hdc = td->dc;
			SelectPalette(hdc, hPal, FALSE);
			i = RealizePalette(hdc);

			/* if any palette entries changed, repaint the window. */
			if (i) InvalidateRect(hWnd, NULL, TRUE);

#else /* not SAVE_DC */

			hdc = GetDC(hWnd);
			SelectPalette(hdc, hPal, FALSE);
			i = RealizePalette(hdc);
			/* if any palette entries changed, repaint the window. */
			if (i) InvalidateRect(hWnd, NULL, TRUE);
			ReleaseDC(hWnd, hdc);

#endif /* not SAVE_DC */

			return 0;
		}

		case WM_NCLBUTTONDOWN:
		{

#ifdef HTCLOSE
			if (wParam == HTCLOSE) wParam = HTSYSMENU;
#endif /* HTCLOSE */

			if (wParam == HTSYSMENU)
			{
				if (td->visible)
				{
					td->visible = FALSE;
					ShowWindow(td->w, SW_HIDE);
				}

				return 0;
			}
			
			break;
		}
	}

	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}


#ifdef USE_SAVER

#define MOUSE_SENS 40

#ifdef __MWERKS__
LRESULT FAR PASCAL AngbandSaverProc(HWND hWnd, UINT uMsg,
                                    WPARAM wParam, LPARAM lParam);
LRESULT FAR PASCAL AngbandSaverProc(HWND hWnd, UINT uMsg,
                                    WPARAM wParam, LPARAM lParam)
#else /* __MWERKS__ */
LRESULT FAR PASCAL _export AngbandSaverProc(HWND hWnd, UINT uMsg,
                                            WPARAM wParam, LPARAM lParam)
#endif /* __MWERKS__ */
{
	static int iMouse = 0;
	static WORD xMouse = 0;
	static WORD yMouse = 0;

	int dx, dy;


	/* Process */
	switch (uMsg)
	{
		/* XXX XXX XXX */
		case WM_NCCREATE:
		{
			break;
		}

		case WM_SETCURSOR:
		{
			SetCursor(NULL);
			return 0;
		}

#if 0
		case WM_ACTIVATE:
		{
			if (LOWORD(wParam) == WA_INACTIVE) break;

			/* else fall through */
		}
#endif

		case WM_LBUTTONDOWN:
		case WM_MBUTTONDOWN:
		case WM_RBUTTONDOWN:
		case WM_KEYDOWN:
		{
			SendMessage(hWnd, WM_CLOSE, 0, 0);
			return 0;
		}

		case WM_MOUSEMOVE:
		{
			if (iMouse)
			{
				dx = LOWORD(lParam) - xMouse;
				dy = HIWORD(lParam) - yMouse;

				if (dx < 0) dx = -dx;
				if (dy < 0) dy = -dy;

				if ((dx > MOUSE_SENS) || (dy > MOUSE_SENS))
				{
					SendMessage(hWnd, WM_CLOSE, 0, 0);
				}
			}

			/* Save last location */
			iMouse = 1;
			xMouse = LOWORD(lParam);
			yMouse = HIWORD(lParam);

			return 0;
		}

		case WM_CLOSE:
		{
			DestroyWindow(hwndSaver);
			hwndSaver = NULL;
			return 0;
		}
	}

	/* Oops */
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

#endif /* USE_SAVER */





/*** Temporary Hooks ***/


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
		MessageBox(data[0].w, str, "Warning",
		           MB_ICONEXCLAMATION | MB_OK);
	}
}


/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(cptr str)
{
	int i;


	/* Give a warning */
	if (str)
	{
		MessageBox(data[0].w, str, "Error",
		           MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);
	}


	/* Save the preferences */
	save_prefs();


	/*** Could use 'Term_nuke_win()' XXX XXX XXX */

	/* Destroy all windows */
	for (i = MAX_TERM_DATA - 1; i >= 0; --i)
	{
		term_force_font(&data[i], NULL);
		if (data[i].font_want) string_free(data[i].font_want);
		if (data[i].w) DestroyWindow(data[i].w);
		data[i].w = 0;
	}


	/*** Free some other stuff ***/

	DeleteObject(hbrYellow);

	if (hPal) DeleteObject(hPal);

	UnregisterClass(AppName, hInstance);

	if (hIcon) DestroyIcon(hIcon);

	exit(0);
}



/*** Initialize ***/


/*
 * Init some stuff
 */
static void init_stuff(void)
{
	char path[1024];
	char *p;

	/* Get the application path */
	GetModuleFileName(hInstance, path, 512);

	/* Strip off the application name */
	p = strrchr(path, '\\');
	*p = '\0';

	/* Access "ANGBAND.INI" */
	(void) strcpy(p, "\\ANGBAND.INI");

	/* Save "ANGBAND.INI" */
	ini_file = string_make(path);

	/* Validate "ANGBAND.INI" */
	validate_file(ini_file);

	/* Append "lib" directory to pathname */
	(void) strcpy(p, "\\lib\\");

	/* Validate the path */
	validate_dir(path);


	/* Init the file paths */
	init_file_paths(path);

#if 0
	/* Mega-Hack XXX XXX XXX */
	if (!check_dir(ANGBAND_DIR_APEX))
	{
		mkdir(ANGBAND_DIR_APEX);
	}
#endif

	/* Hack -- Validate the paths */
	validate_dir(ANGBAND_DIR_APEX);
	validate_dir(ANGBAND_DIR_BONE);
	validate_dir(ANGBAND_DIR_DATA);
	validate_dir(ANGBAND_DIR_EDIT);
	validate_dir(ANGBAND_DIR_FILE);
	validate_dir(ANGBAND_DIR_HELP);
	validate_dir(ANGBAND_DIR_INFO);
	validate_dir(ANGBAND_DIR_SAVE);
	validate_dir(ANGBAND_DIR_USER);
	validate_dir(ANGBAND_DIR_XTRA);

	/* Build the filename */
	path_build(path, 1024, ANGBAND_DIR_FILE, "news.txt");

	/* Hack -- Validate the "news.txt" file */
	validate_file(path);


	/* Build the "font" path */
	path_build(path, 1024, ANGBAND_DIR_XTRA, "font");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_FONT = string_make(path);

	/* Validate the "font" directory */
	validate_dir(ANGBAND_DIR_XTRA_FONT);

	/* Build the filename */
	path_build(path, 1024, ANGBAND_DIR_XTRA_FONT, "8X13.FON");

	/* Hack -- Validate the basic font */
	validate_file(path);


#ifdef USE_GRAPHICS

	/* Build the "graf" path */
	path_build(path, 1024, ANGBAND_DIR_XTRA, "graf");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_GRAF = string_make(path);

	/* Validate the "graf" directory */
	validate_dir(ANGBAND_DIR_XTRA_GRAF);

	/* Build the filename */
	path_build(path, 1024, ANGBAND_DIR_XTRA_GRAF, "8X8.BMP");

	/* Hack -- Validate the basic graf */
	validate_file(path);

#endif


#ifdef USE_SOUND

	/* Build the "sound" path */
	path_build(path, 1024, ANGBAND_DIR_XTRA, "sound");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_SOUND = string_make(path);

	/* Validate the "sound" directory */
	validate_dir(ANGBAND_DIR_XTRA_SOUND);

#endif


	/* Build the "help" path */
	path_build(path, 1024, ANGBAND_DIR_XTRA, "help");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_HELP = string_make(path);

	/* Validate the "help" directory */
	/* validate_dir(ANGBAND_DIR_XTRA_HELP); */
}

#ifdef USE_GAMMA_CORRECTION

#include <math.h>

static int gamma_correct(int value, double gamma)
{
	double ind;
	double inverse;

	if (gamma)
	{
		inverse = 1.0 / gamma;
	}
	else
	{
		inverse = 1.0;
	}
	ind = (double) value / 256.0;
	return (int) (256 * pow(ind, inverse));
}

static void gamma_correct_colors(void)
{
	int i;

	double gamma = 1.3;

	for (i = 0; i < 256; i++)
	{
		byte rv, gv, bv;
	
		/* Extract desired values */
		rv = angband_color_table[i][1];
		gv = angband_color_table[i][2];
		bv = angband_color_table[i][3];

		/* Apply gamma correction */
		rv = gamma_correct(rv, gamma);
		gv = gamma_correct(gv, gamma);
		bv = gamma_correct(bv, gamma);
	
		/* Save corrected values */
		angband_color_table[i][1] = rv;
		angband_color_table[i][2] = gv;
		angband_color_table[i][3] = bv;
	}
}

#endif /* USE_GAMMA_CORRECTION */


int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst,
                       LPSTR lpCmdLine, int nCmdShow)
{
	int i;

	WNDCLASS wc;
	HDC hdc;
	MSG msg;

	/* Save globally */
	hInstance = hInst;

	/* Initialize */
	if (hPrevInst == NULL)
	{
#ifdef SAVE_DC
		wc.style         = CS_OWNDC;
#else /* not SAVE_DC */
		wc.style         = CS_CLASSDC;
#endif /* not SAVE_DC */
		wc.lpfnWndProc   = AngbandWndProc;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 4; /* one long pointer to term_data */
		wc.hInstance     = hInst;
		wc.hIcon         = hIcon = LoadIcon(hInst, AppName);
		wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
		wc.hbrBackground = GetStockObject(BLACK_BRUSH);
		wc.lpszMenuName  = AppName;
		wc.lpszClassName = AppName;

		if (!RegisterClass(&wc)) exit(1);

		wc.lpfnWndProc   = AngbandListProc;
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = AngList;

		if (!RegisterClass(&wc)) exit(2);

#ifdef USE_SAVER

		wc.style          = CS_VREDRAW | CS_HREDRAW | CS_SAVEBITS | CS_DBLCLKS;
		wc.lpfnWndProc    = AngbandSaverProc;
		wc.hCursor        = NULL;
		wc.lpszMenuName   = NULL;
		wc.lpszClassName  = "WindowsScreenSaverClass";

		if (!RegisterClass(&wc)) exit(3);

#endif

	}

	/* Temporary hooks */
	plog_aux = hack_plog;
	quit_aux = hack_quit;
	core_aux = hack_quit;

	/* Prepare the filepaths */
	init_stuff();

	/* Initialize the keypress analyzer */
	for (i = 0; special_key_list[i]; ++i)
	{
		special_key[special_key_list[i]] = TRUE;
	}
	
	/* Determine if display is 16/256/true color */
	hdc = GetDC(NULL);
	colors16 = (GetDeviceCaps(hdc, BITSPIXEL) == 4);
	paletted = ((GetDeviceCaps(hdc, RASTERCAPS) & RC_PALETTE) ? TRUE : FALSE);
	ReleaseDC(NULL, hdc);

#ifdef USE_GAMMA_CORRECTION

	gamma_correct_colors();

#endif /* USE_GAMMA_CORRECTION */

	/* Initialize the colors */
	for (i = 0; i < 256; i++)
	{
		byte rv, gv, bv;

		/* Extract desired values */
		rv = angband_color_table[i][1];
		gv = angband_color_table[i][2];
		bv = angband_color_table[i][3];

		/* Extract the "complex" code */
		win_clr[i] = PALETTERGB(rv, gv, bv);

		/* Save the "simple" code */
		angband_color_table[i][0] = win_pal[i];
	}

	/* Prepare the windows */
	init_windows();

	/* Activate hooks */
	plog_aux = hook_plog;
	quit_aux = hook_quit;
	core_aux = hook_quit;

	/* Set the system suffix */
	ANGBAND_SYS = "win";

	/* Initialize */
	init_angband();

	/* We are now initialized */
	initialized = TRUE;

	/* Did the user double click on a save file? */
	check_for_save_file(lpCmdLine);

	/* Prompt the user */
	prt_center("[Choose 'New' or 'Open' from the 'File' menu]", screen_y - 1);
	Term_fresh();

	/* Process messages forever */
	while (GetMessage(&msg, NULL, 0, 0))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	/* Paranoia */
	quit(NULL);

	/* Paranoia */
	return (0);
}


#endif /* WINDOWS */


