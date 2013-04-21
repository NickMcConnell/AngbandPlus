/* File: main-win.c */

/*
 * Copyright (c) 1997 Ben Harrison, Skirmantas Kligys, Robert Ruehlmann,
 * and others
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
 * The Windows version has been tested to compile with Visual C++ 5.0
 * and 6.0, Cygwin 1.0, Borland C++ 5.5 command line tools, and lcc-win32.
 *
 *
 * See also "main-dos.c" and "main-ibm.c".
 *
 *
 * The "lib/user/pref-win.prf" file contains keymaps, macro definitions,
 * and/or color redefinitions.
 *
 * The "lib/user/font-win.prf" contains attr/char mappings for use with the
 * normal "*.fon" font files in the "lib/xtra/font/" directory.
 *
 * The "lib/user/graf-win.prf" contains attr/char mappings for use with the
 * special "*.bmp" bitmap files in the "lib/xtra/graf/" directory, which
 * are activated by a menu item.
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
 * Special "Windows Help Files" can be placed into "lib/xtra/help/" for
 * use with the "winhelp.exe" program.  These files *may* be available
 * at the ftp site somewhere, but I have not seen them.  XXX XXX XXX
 *
 * ToDo: The screensaver mode should implement ScreenSaverConfigureDialog,
 * DefScreenSaverProc, and ScreenSaverProc.
 *
 * Initial framework (and most code) by Ben Harrison (benh@phial.com).
 *
 * Original code by Skirmantas Kligys (kligys@scf.usc.edu).
 *
 * Additional code by Ross E Becker (beckerr@cis.ohio-state.edu),
 * and Chris R. Martin (crm7479@tam2000.tamu.edu).
 *
 * Additional code by Robert Ruehlmann <rr9@angband.org>.
 */

#include "angband.h"


#ifdef WINDOWS


/*
 * Use HTML-Help.
 */
/* #define HTML_HELP */

#ifdef HTML_HELP
# define HELP_GENERAL "angband.chm"
# define HELP_SPOILERS "angband.chm"
#else /* HTML_HELP */
# define HELP_GENERAL "angband.hlp"
# define HELP_SPOILERS "spoilers.hlp"
#endif /* HTML_HELP */


/*
 * Extract the "WIN32" flag from the compiler
 */
#if defined(__WIN32__) || defined(__WINNT__) || defined(__NT__)
# ifndef WIN32
#  define WIN32
# endif
#endif



/*
 * Menu constants -- see "ANGBAND.RC"
 */

#define IDM_FILE_NEW			100
#define IDM_FILE_OPEN			101
#define IDM_FILE_SAVE			110
#define IDM_FILE_SCORE			120
#define IDM_FILE_EXIT			130

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

#define IDM_OPTIONS_NO_GRAPHICS     400
#define IDM_OPTIONS_OLD_GRAPHICS    401
#define IDM_OPTIONS_NEW_GRAPHICS    402
#define IDM_OPTIONS_SOUND           410
#define IDM_OPTIONS_LOW_PRIORITY    420
#define IDM_OPTIONS_SAVER           430
#define IDM_OPTIONS_MAP             440

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

/*
 * Include the "windows" support file
 */
#include <windows.h>

#include <commdlg.h>

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
#endif /* WS_EX_TOOLWINDOW */

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

#ifdef WIN32
static HINSTANCE theHinstance = 0;
static HWND theParentWnd = 0;
int setHINST(long val) {
    theParentWnd = (HWND)val;

    if (!theParentWnd || !IsWindow(theParentWnd)) {
	hack_plog("Did not pass a proper window.");
    }
    
    theHinstance = (HINSTANCE)GetWindowLong (theParentWnd, GWL_HINSTANCE);
    
    return 0;
}
HINSTANCE getHINST(void) {
    return theHinstance;
}
HWND getParentWnd(void) {
    return theParentWnd;
}
#endif


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

    byte rows;
    byte cols;

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
    bool maximized;

    bool bizarre;

    cptr font_want;

    cptr font_file;

    HFONT font_id;

    uint font_wid;
    uint font_hgt;

    uint tile_wid;
    uint tile_hgt;

    uint map_tile_wid;
    uint map_tile_hgt;

    bool map_active;
};


/*
 * Maximum number of windows XXX XXX XXX
 */
#define MAX_TERM_DATA 1


/*
 * An array of term_data's
 */
static term_data *loc_terms;

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

static bool low_priority = FALSE;

/*
 * Saved instance handle
 */
//static HINSTANCE hInstance;

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


/*
 * Full path to ANGBAND.INI
 */
static cptr ini_file = NULL;

/*
 * Name of application
 */
static cptr AppName = "langband";

/*
 * Name of sub-window type
 */
static cptr AngList = "AngList";

/*
 * Directory names
 */
static cptr ANGBAND_DIR_XTRA_FONT;
static cptr ANGBAND_DIR_XTRA_GRAF;
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
static byte win_pal[256] =
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
static const byte special_key_list[] =
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

static void
show_win_error(void)
{
    LPVOID lpMsgBuf;

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		  NULL, GetLastError(),
		  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		  (LPTSTR) &lpMsgBuf, 0, NULL);

    MessageBox(NULL, lpMsgBuf, "Error", MB_OK | MB_ICONINFORMATION);

    LocalFree(lpMsgBuf);
}


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
 * Get the "size" for a window
 */
static void term_getsize(term_data *td)
{
    RECT rc;

    int wid, hgt;

    /* Paranoia */
    if (td->cols < 1) td->cols = 1;
    if (td->rows < 1) td->rows = 1;

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

    /* No bitmap */
    lppeSize = 0;
    lppe = NULL;
    nEntries = 0;


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
    if (!hNewPal) {
	quit("Cannot create palette!");
    }

    /* Free the palette */
    rnfree(pLogPal, pLogPalSize);

    /* Main window */
    td = &loc_terms[0];

    /* Realize the palette */
    hdc = GetDC(td->w);
    SelectPalette(hdc, hNewPal, 0);
    i = RealizePalette(hdc);
    ReleaseDC(td->w, hdc);
    if (i == 0) {
	quit("Cannot realize palette!");
    }

    /* Sub-windows */
    for (i = 1; i < MAX_TERM_DATA; i++)
    {
	td = &loc_terms[i];

	hdc = GetDC(td->w);
	SelectPalette(hdc, hNewPal, 0);
	ReleaseDC(td->w, hdc);
    }

    /* Delete old palette */
    if (hPal) DeleteObject(hPal);

    /* Save new palette */
    hPal = hNewPal;

    /* Success */
    return (TRUE);
}



/*
 * Resize a window
 */
static void term_window_resize(const term_data *td)
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
	    /* Don't check when closing the application */
	    if (!path) break;

	    /* Check "screen" */
	    if ((td != &loc_terms[i]) &&
		(loc_terms[i].font_file) &&
		(streq(loc_terms[i].font_file, td->font_file)))
	    {
		used = TRUE;
	    }
	}

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
    if (!suffix(base, ".FON")) return (2);

    /* Verify file */
    if (!check_file(buf)) return (3);

    /* Load the new font */
    if (!AddFontResource(buf)) return (4);

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


static void windows_map_aux(void);


/*
 * Hack -- redraw a term_data
 */
static void term_data_redraw(term_data *td)
{
    if (td->map_active)
    {
	/* Redraw the map */
	windows_map_aux();
    }
    else
    {
	/* Activate the term */
	Term_activate(&td->t);

	/* Redraw the contents */
	Term_redraw();

	/* Restore the term */
	Term_activate(term_screen);
    }
}


/*
 * Hack -- redraw a term_data
 */
static void term_data_redraw_section(term_data *td, int x1, int y1, 
				     int x2, int y2) {

    int a = x1+x2+y1+y2;
    /* Activate the term */
    Term_activate(&td->t);


    a+=5;

	/* Redraw the area */
	// put back later
//	Term_redraw_section(x1, y1, x2, y2);

	/* Restore the term */
    Term_activate(term_screen);
}



/*** Function hooks needed by "Term" ***/


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


    /* Clean up windows */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	term *old = Term;

	term_data *td = &loc_terms[i];

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

    /* Erase it */
    hdc = GetDC(td->w);
    SetBkColor(hdc, RGB(0, 0, 0));
    SelectObject(hdc, td->font_id);
    ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);
    ReleaseDC(td->w, hdc);

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

    int tile_wid, tile_hgt;

    if (td->map_active)
    {
	tile_wid = td->map_tile_wid;
	tile_hgt = td->map_tile_hgt;
    }
    else
    {
	tile_wid = td->tile_wid;
	tile_hgt = td->tile_hgt;
    }

    /* Frame the grid */
    rc.left = x * tile_wid + td->size_ow1;
    rc.right = rc.left + tile_wid;
    rc.top = y * tile_hgt + td->size_oh1;
    rc.bottom = rc.top + tile_hgt;

    /* Cursor is done as a yellow "box" */
    hdc = GetDC(td->w);
    FrameRect(hdc, &rc, hbrYellow);
    ReleaseDC(td->w, hdc);

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

    hdc = GetDC(td->w);
    SetBkColor(hdc, RGB(0, 0, 0));
    SelectObject(hdc, td->font_id);
    ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);
    ReleaseDC(td->w, hdc);

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
static errr Term_text_win(int x, int y, int n, byte a, cptr s)
{
    term_data *td = (term_data*)(Term->data);
    RECT rc;
    HDC hdc;


    /* Total rectangle */
    rc.left = x * td->tile_wid + td->size_ow1;
    rc.right = rc.left + n * td->tile_wid;
    rc.top = y * td->tile_hgt + td->size_oh1;
    rc.bottom = rc.top + td->tile_hgt;

    /* Acquire DC */
    hdc = GetDC(td->w);

    /* Background color */
    SetBkColor(hdc, RGB(0, 0, 0));

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

    /* Use the font */
    SelectObject(hdc, td->font_id);

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

    /* Release DC */
    ReleaseDC(td->w, hdc);

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
# ifdef USE_TRANSPARENCY
static errr Term_pict_win(int x, int y, int n, const byte *ap, const char *cp, const byte *tap, const char *tcp)
# else /* USE_TRANSPARENCY */
    static errr Term_pict_win(int x, int y, int n, const byte *ap, const char *cp)
# endif /* USE_TRANSPARENCY */
{
    term_data *td = (term_data*)(Term->data);

    /* Just erase this grid */
    return (Term_wipe_win(x, y, n));

    /* Success */
    return 0;
}


static void windows_map_aux(void)
{

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


static errr
read_font_file(term_data *td, const char *fname) {

    char buf[1024];
    int retval = 0;
    
    sprintf(buf, "%s%s", base_config_dir, fname);
    retval = term_force_font(td, buf);
    if (retval) {
	char buff[1024];
	sprintf(buff, "cock-up with fonts %d, file: %s", retval, buf);
	hack_plog(buff);
    }
    return retval;
}

static void init_windows(HWND parentWnd) {
    
    term_data *td;

//    hack_plog("entry win");
    td = &loc_terms[0];
    WIPE(td, term_data);
    
    td->s = "langband";
    td->keys = 1024;
    td->rows = 24;
    td->cols = 80;
    td->visible = TRUE;
    td->size_ow1 = 2;
    td->size_ow2 = 2;
    td->size_oh1 = 2;
    td->size_oh2 = 2;
    td->pos_x = 30;
    td->pos_y = 20;

    td->dwStyle = (WS_OVERLAPPED | WS_BORDER | WS_SYSMENU | WS_CAPTION);
//    if (td->maximized) td->dwStyle |= WS_MAXIMIZE;
    td->dwExStyle = (WS_EX_TOOLWINDOW);
    td->visible = TRUE;


    {
	/* Force the use of that font */
	read_font_file(td, "8x13.fon");
	
	/* Oops */
	td->tile_wid = 8;
	td->tile_hgt = 13;
	/* Assume not bizarre */
	td->bizarre = FALSE;
	/* Analyze the font */
	term_getsize(td);
    
	/* Resize the window */
	term_window_resize(td);
    }
    
    /* Main window */
//    td = &data[0];
    
/*
  {
  char buf[1024];
  sprintf(buf,"foing win %d %d %d %d %p %p",
  td->pos_x, td->pos_y, td->size_wid, td->size_hgt,
  parentWnd, getHINST());
  hack_plog(buf);
  }
*/  
    
    /* Main window */
    my_td = td;
    td->w = CreateWindowEx(td->dwExStyle, AppName,
			   td->s, td->dwStyle,
			   td->pos_x, td->pos_y,
			   td->size_wid, td->size_hgt,
			   parentWnd, NULL, getHINST(), NULL);
    my_td = NULL;

    if (!td->w) {
	quit("Failed to create langband window");
    }
//    hack_plog("made win0");

//    show_win_error();
//    hack_plog("made win");
	
    term_data_link(td);
    term_screen = &td->t;
    
    
    /* Activate the main window */
    SetActiveWindow(td->w);
    
    /* Bring main window back to top */
    SetWindowPos(td->w, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);

//    show_win_error();
//    hack_plog("made win2");
    
    /* New palette XXX XXX XXX */
    (void)new_palette();
    
    
    /* Create a "brush" for drawing the "cursor" */
    hbrYellow = CreateSolidBrush(win_clr[TERM_YELLOW]);

//    show_win_error();
//    hack_plog("made win3");
    
    ShowWindow(td->w, SW_SHOW);
//    hack_plog("made win4");
//    show_win_error();
    
    /* Process pending messages */
    (void)Term_xtra_win_flush();
}
    
#if 0
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
    td->rows = 24;
    td->cols = 80;
    td->visible = TRUE;
    td->size_ow1 = 2;
    td->size_ow2 = 2;
    td->size_oh1 = 2;
    td->size_oh2 = 2;
    td->pos_x = 30;
    td->pos_y = 20;

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
    }


    /* Load prefs */
//	load_prefs();


    /* Main window (need these before term_getsize gets called) */
    td = &data[0];
    td->dwStyle = (WS_OVERLAPPED | WS_THICKFRAME | WS_SYSMENU |
		   WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_CAPTION |
		   WS_VISIBLE);
    if (td->maximized) td->dwStyle |= WS_MAXIMIZE;
    td->dwExStyle = 0;
    td->visible = TRUE;

    /* Sub windows (need these before term_getsize gets called) */
    for (i = 1; i < MAX_TERM_DATA; i++)
    {
	td = &data[i];
	td->dwStyle = (WS_OVERLAPPED | WS_THICKFRAME | WS_SYSMENU | WS_CAPTION);
	td->dwExStyle = (WS_EX_TOOLWINDOW);
    }


    /* All windows */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	td = &data[i];

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
	if (!td->w) {
	    quit("Failed to create sub-window");
	}

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
    if (!td->w) quit_fmt("Failed to create %s window", "langband");

    term_data_link(td);
    term_screen = &td->t;



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
#endif /* 0 */


/*
 * Prepare the menus
 */
static void setup_menus(void)
{
    int i;

    HMENU hm = GetMenu(loc_terms[0].w);


    /* Menu "File", Disable all */
    EnableMenuItem(hm, IDM_FILE_NEW,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_FILE_OPEN,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_FILE_SAVE,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_FILE_EXIT,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_FILE_SCORE,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);


    /* No character available */
    if (!character_generated)
    {
	/* Menu "File", Item "New" */
	EnableMenuItem(hm, IDM_FILE_NEW, MF_BYCOMMAND | MF_ENABLED);

	/* Menu "File", Item "Open" */
	EnableMenuItem(hm, IDM_FILE_OPEN, MF_BYCOMMAND | MF_ENABLED);
    }

    /* A character available */
    if (game_in_progress && character_generated && inkey_flag && can_save)
    {
	/* Menu "File", Item "Save" */
	EnableMenuItem(hm, IDM_FILE_SAVE, MF_BYCOMMAND | MF_ENABLED);
    }

    if (!game_in_progress || !character_generated ||
	(inkey_flag && can_save))
    {
	/* Menu "File", Item "Exit" */
	EnableMenuItem(hm, IDM_FILE_EXIT, MF_BYCOMMAND | MF_ENABLED);
    }

    if (initialized)
    {
	/* Menu "File", Item "Show Scores" */
	EnableMenuItem(hm, IDM_FILE_SCORE, MF_BYCOMMAND | MF_ENABLED);
    }


    /* Menu "Window::Visibility" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	EnableMenuItem(hm, IDM_WINDOW_VIS_0 + i,
		       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

	CheckMenuItem(hm, IDM_WINDOW_VIS_0 + i,
		      (loc_terms[i].visible ? MF_CHECKED : MF_UNCHECKED));

	EnableMenuItem(hm, IDM_WINDOW_VIS_0 + i,
		       MF_BYCOMMAND | MF_ENABLED);
    }

    /* Menu "Window::Font" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	EnableMenuItem(hm, IDM_WINDOW_FONT_0 + i,
		       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

	if (loc_terms[i].visible)
	{
	    EnableMenuItem(hm, IDM_WINDOW_FONT_0 + i,
			   MF_BYCOMMAND | MF_ENABLED);
	}
    }

    /* Menu "Window::Bizarre Display" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	EnableMenuItem(hm, IDM_WINDOW_BIZ_0 + i,
		       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

	CheckMenuItem(hm, IDM_WINDOW_BIZ_0 + i,
		      (loc_terms[i].bizarre ? MF_CHECKED : MF_UNCHECKED));

	if (loc_terms[i].visible)
	{
	    EnableMenuItem(hm, IDM_WINDOW_BIZ_0 + i,
			   MF_BYCOMMAND | MF_ENABLED);

	}
    }

    /* Menu "Window::Increase Tile Width" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	EnableMenuItem(hm, IDM_WINDOW_I_WID_0 + i,
		       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

	if (loc_terms[i].visible)
	{
	    EnableMenuItem(hm, IDM_WINDOW_I_WID_0 + i,
			   MF_BYCOMMAND | MF_ENABLED);

	}
    }

    /* Menu "Window::Decrease Tile Width" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	EnableMenuItem(hm, IDM_WINDOW_D_WID_0 + i,
		       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

	if (loc_terms[i].visible)
	{
	    EnableMenuItem(hm, IDM_WINDOW_D_WID_0 + i,
			   MF_BYCOMMAND | MF_ENABLED);

	}
    }

    /* Menu "Window::Increase Tile Height" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	EnableMenuItem(hm, IDM_WINDOW_I_HGT_0 + i,
		       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

	if (loc_terms[i].visible)
	{
	    EnableMenuItem(hm, IDM_WINDOW_I_HGT_0 + i,
			   MF_BYCOMMAND | MF_ENABLED);

	}
    }

    /* Menu "Window::Decrease Tile Height" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
	EnableMenuItem(hm, IDM_WINDOW_D_HGT_0 + i,
		       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

	if (loc_terms[i].visible)
	{
	    EnableMenuItem(hm, IDM_WINDOW_D_HGT_0 + i,
			   MF_BYCOMMAND | MF_ENABLED);

	}
    }

    /* Menu "Options", disable all */
    EnableMenuItem(hm, IDM_OPTIONS_NO_GRAPHICS,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_OPTIONS_OLD_GRAPHICS,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_OPTIONS_NEW_GRAPHICS,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_OPTIONS_SOUND,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_OPTIONS_SAVER,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_OPTIONS_LOW_PRIORITY,
		   MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

    /* Menu "Options", Item "Map" */
    if (inkey_flag && initialized && (use_graphics != GRAPHICS_NONE))
	EnableMenuItem(GetMenu(loc_terms[0].w), IDM_OPTIONS_MAP, MF_BYCOMMAND | MF_ENABLED);
    else {
	EnableMenuItem(GetMenu(loc_terms[0].w), IDM_OPTIONS_MAP,
		       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    }

    /* Menu "Options", update all */
    CheckMenuItem(hm, IDM_OPTIONS_NO_GRAPHICS,
		  (arg_graphics == GRAPHICS_NONE ? MF_CHECKED : MF_UNCHECKED));
    CheckMenuItem(hm, IDM_OPTIONS_OLD_GRAPHICS,
		  (arg_graphics == GRAPHICS_ORIGINAL ? MF_CHECKED : MF_UNCHECKED));
    CheckMenuItem(hm, IDM_OPTIONS_NEW_GRAPHICS,
		  (arg_graphics == GRAPHICS_ADAM_BOLT ? MF_CHECKED : MF_UNCHECKED));
    CheckMenuItem(hm, IDM_OPTIONS_SOUND,
		  (arg_sound ? MF_CHECKED : MF_UNCHECKED));

    CheckMenuItem(hm, IDM_OPTIONS_LOW_PRIORITY,
		  (low_priority ? MF_CHECKED : MF_UNCHECKED));


    EnableMenuItem(hm, IDM_OPTIONS_LOW_PRIORITY,
		   MF_BYCOMMAND | MF_ENABLED);
}


/*
 * Check for double clicked (or dragged) savefile
 *
 * Apparently, Windows copies the entire filename into the first
 * piece of the "command line string".  Perhaps we should extract
 * the "basename" of that filename and append it to the "save" dir.
 */


/*
 * Display a help file
 */
static void display_help(cptr filename)
{

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
	    //quit(NULL);
	}
	break;
    }

    /* Open game */
    case IDM_FILE_OPEN:
    {

	break;
    }

    /* Save game */
    case IDM_FILE_SAVE:
    {

	break;
    }

    /* Show scores */
    case IDM_FILE_SCORE:
    {

	break;
    }

    /* Exit */
    case IDM_FILE_EXIT:
    {
	if (game_in_progress && character_generated)
	{
				/* Paranoia */
	    if (!inkey_flag || !can_save)
	    {
		plog("You may not do that right now.");
		break;
	    }

				/* Hack -- Forget messages */
	    msg_flag = FALSE;


	}
	//quit(NULL);
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

	td = &loc_terms[i];

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

	td = &loc_terms[i];

	plog("Tried to change font, disabled.");
//			term_change_font(td);

	break;
    }

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

	td = &loc_terms[i];

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

	td = &loc_terms[i];

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

	td = &loc_terms[i];

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

	td = &loc_terms[i];

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

	td = &loc_terms[i];

	td->tile_hgt -= 1;

	term_getsize(td);

	term_window_resize(td);

	break;
    }

    case IDM_OPTIONS_NO_GRAPHICS:
    {
	/* Paranoia */
	if (!inkey_flag || !initialized)
	{
	    plog("You may not do that right now.");
	    break;
	}

	/* Toggle "arg_graphics" */
	if (arg_graphics != GRAPHICS_NONE)
	{
	    arg_graphics = GRAPHICS_NONE;

				/* React to changes */
	    Term_xtra_win_react();

				/* Hack -- Force redraw */
	    Term_key_push(KTRL('R'));
	}

	break;
    }

    case IDM_OPTIONS_OLD_GRAPHICS:
    {
	/* Paranoia */
	if (!inkey_flag || !initialized)
	{
	    plog("You may not do that right now.");
	    break;
	}

	/* Toggle "arg_graphics" */
	if (arg_graphics != GRAPHICS_ORIGINAL)
	{
	    arg_graphics = GRAPHICS_ORIGINAL;

				/* React to changes */
	    Term_xtra_win_react();

				/* Hack -- Force redraw */
	    Term_key_push(KTRL('R'));
	}

	break;
    }

    case IDM_OPTIONS_NEW_GRAPHICS:
    {
	/* Paranoia */
	if (!inkey_flag || !initialized)
	{
	    plog("You may not do that right now.");
	    break;
	}

	/* Toggle "arg_graphics" */
	if (arg_graphics != GRAPHICS_ADAM_BOLT)
	{
	    arg_graphics = GRAPHICS_ADAM_BOLT;

				/* React to changes */
	    Term_xtra_win_react();

				/* Hack -- Force redraw */
	    Term_key_push(KTRL('R'));
	}

	break;
    }

    case IDM_OPTIONS_SOUND:
    {
	/* Paranoia */
	if (!inkey_flag || !initialized)
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


    case IDM_OPTIONS_LOW_PRIORITY:
    {
	/* Lower or reset the priority of the current process */
	if (low_priority)
	    SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
	else
	    SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);

	/* Toggle priority */
	low_priority = !low_priority;

	break;
    }

    case IDM_OPTIONS_MAP:
    {
			
	break;
    }

    case IDM_HELP_GENERAL:
    {
	display_help(HELP_GENERAL);
	break;
    }

    case IDM_HELP_SPOILERS:
    {
	display_help(HELP_SPOILERS);
	break;
    }
    }
}


/*
 * Redraw a section of a window
 */
static void handle_wm_paint(HWND hWnd)
{
    int x1, y1, x2, y2;
    PAINTSTRUCT ps;
    term_data *td;

    /* Acquire proper "term_data" info */
    td = (term_data *)GetWindowLong(hWnd, 0);

    BeginPaint(hWnd, &ps);

    if (td->map_active)
    {
	/* Redraw the map */
	/* ToDo: Only redraw the necessary parts */
	windows_map_aux();
    }
    else
    {
	/* Get the area that should be updated (rounding up/down) */
	/* ToDo: Take the window borders into account */
	x1 = (ps.rcPaint.left / td->tile_wid) - 1;
	x2 = (ps.rcPaint.right / td->tile_wid) + 1;
	y1 = (ps.rcPaint.top / td->tile_hgt) - 1;
	y2 = (ps.rcPaint.bottom / td->tile_hgt) + 1;

	/* Redraw */
	if (td) term_data_redraw_section(td, x1, y1, x2, y2);
    }

    EndPaint(hWnd, &ps);
}

/*
 * Global array for converting numbers to uppercase hecidecimal digit
 * This array can also be used to convert a number to an octal digit
 */
static char hexsym[16] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
};



static LRESULT FAR PASCAL AngbandWndProc(HWND hWnd, UINT uMsg,
					 WPARAM wParam, LPARAM lParam)
{
    HDC hdc;
    term_data *td;
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
	MINMAXINFO FAR *lpmmi;
	RECT rc;

	lpmmi = (MINMAXINFO FAR *)lParam;

	/* this message was sent before WM_NCCREATE */
	if (!td) return 1;

	/* Minimum window size is 80x24 */
	rc.left = rc.top = 0;
	rc.right = rc.left + 80 * td->tile_wid + td->size_ow1 + td->size_ow2;
	rc.bottom = rc.top + 24 * td->tile_hgt + td->size_oh1 + td->size_oh2 + 1;

	/* Adjust */
	AdjustWindowRectEx(&rc, td->dwStyle, TRUE, td->dwExStyle);

	/* Save minimum size */
	lpmmi->ptMinTrackSize.x = rc.right - rc.left;
	lpmmi->ptMinTrackSize.y = rc.bottom - rc.top;

	return 0;
    }

    case WM_PAINT:
    {
	handle_wm_paint(hWnd);

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

	    return 0;
	}

	break;
    }

    case WM_CHAR:
    {
	Term_keypress(wParam);
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
	    if (!inkey_flag || !can_save)
	    {
		plog("You may not do that right now.");
		return 0;
	    }

				/* Hack -- Forget messages */
	    msg_flag = FALSE;

				/* Save the game */
	}
	
	//quit(NULL);
	return 0;
    }

    case WM_QUIT:
    {
	//quit(NULL);
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
		if (loc_terms[i].visible) 
		  ShowWindow(loc_terms[i].w, SW_HIDE);
	    }
	    return 0;
	}

	case SIZE_MAXIMIZED:
	{
	    /* fall through XXX XXX XXX */
	}

	case SIZE_RESTORED:
	{
	    int cols = (LOWORD(lParam) - td->size_ow1) / td->tile_wid;
	    int rows = (HIWORD(lParam) - td->size_oh1) / td->tile_hgt;

	    /* New size */
	    if ((td->cols != cols) || (td->rows != rows))
	    {
		/* Save the new size */
		td->cols = cols;
		td->rows = rows;

		/* Activate */
		Term_activate(&td->t);

		/* Resize the term */
		Term_resize(td->cols, td->rows);

		/* Redraw later */
		InvalidateRect(td->w, NULL, TRUE);
	    }

	    td->size_hack = TRUE;

	    /* Show sub-windows */
	    for (i = 1; i < MAX_TERM_DATA; i++)
	    {
		if (loc_terms[i].visible) 
		  ShowWindow(loc_terms[i].w, SW_SHOW);
	    }

	    td->size_hack = FALSE;

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

	hdc = GetDC(hWnd);

	SelectPalette(hdc, hPal, FALSE);

	i = RealizePalette(hdc);

	/* if any palette entries changed, repaint the window. */
	if (i) InvalidateRect(hWnd, NULL, TRUE);

	ReleaseDC(hWnd, hdc);

	return 0;
    }

    case WM_ACTIVATE:
    {
	if (wParam && !HIWORD(lParam))
	{
				/* Do something to sub-windows */
	    for (i = 1; i < MAX_TERM_DATA; i++)
	    {
		SetWindowPos(loc_terms[i].w, hWnd, 0, 0, 0, 0,
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


static LRESULT FAR PASCAL AngbandListProc(HWND hWnd, UINT uMsg,
					  WPARAM wParam, LPARAM lParam)
{
    term_data *td;
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

	return 0;
    }

    case WM_SIZE:
    {
	int cols;
	int rows;

	/* this message was sent before WM_NCCREATE */
	if (!td) return 1;

	/* it was sent from inside CreateWindowEx */
	if (!td->w) return 1;

	/* was sent from inside WM_SIZE */
	if (td->size_hack) return 1;

	td->size_hack = TRUE;

	cols = (LOWORD(lParam) - td->size_ow1) / td->tile_wid;
	rows = (HIWORD(lParam) - td->size_oh1) / td->tile_hgt;

	/* New size */
	if ((td->cols != cols) || (td->rows != rows))
	{
	    /* Save old term */
	    term *old_term = Term;

	    /* Save the new size */
	    td->cols = cols;
	    td->rows = rows;

	    /* Activate */
	    Term_activate(&td->t);

	    /* Resize the term */
	    Term_resize(td->cols, td->rows);

	    /* Activate */
	    Term_activate(old_term);

	    /* Redraw later */
	    InvalidateRect(td->w, NULL, TRUE);

	    /* HACK - Redraw all windows */
	    
	    //p_ptr->window = 0xFFFFFFFF;
	    window_stuff();
	}

	td->size_hack = FALSE;

	return 0;
    }

    case WM_PAINT:
    {
	handle_wm_paint(hWnd);

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

	    return 0;
	}

	break;
    }

    case WM_CHAR:
    {
	Term_keypress(wParam);
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
	hdc = GetDC(hWnd);
	SelectPalette(hdc, hPal, FALSE);
	i = RealizePalette(hdc);
	/* if any palette entries changed, repaint the window. */
	if (i) InvalidateRect(hWnd, NULL, TRUE);
	ReleaseDC(hWnd, hdc);
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



/*** Temporary Hooks ***/



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
//	UnregisterClass(AppName, hInstance);

    /* Destroy the icon */
    if (hIcon) DestroyIcon(hIcon);

    /* Exit */
//    exit(0);
}



/*** Various hooks ***/


/*
 * Display warning message (see "z-util.c")
 */
static void hook_plog(cptr str)
{
    /* Warning */
    if (str && loc_terms)
    {
	MessageBox(loc_terms[0].w, str, "Warning",
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
    if (str && loc_terms)
    {
	MessageBox(loc_terms[0].w, str, "Error",
		   MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);
    }


    /*** Could use 'Term_nuke_win()' XXX XXX XXX */

    /* Destroy all windows */
    for (i = MAX_TERM_DATA - 1; i >= 0; --i)
    {
	term_force_font(&loc_terms[i], NULL);
	if (loc_terms[i].font_want) string_free(loc_terms[i].font_want);
	if (loc_terms[i].w) DestroyWindow(loc_terms[i].w);
	loc_terms[i].w = NULL;

	term_nuke(&loc_terms[i].t);
    }

    /* Free the bitmap stuff */

    /*** Free some other stuff ***/

    DeleteObject(hbrYellow);

    if (hPal) DeleteObject(hPal);

//	UnregisterClass(AppName, hInstance);

    if (hIcon) DestroyIcon(hIcon);

    /* Free strings */
    string_free(ini_file);
    string_free(argv0);
    string_free(ANGBAND_DIR_XTRA_FONT);
    string_free(ANGBAND_DIR_XTRA_GRAF);
    string_free(ANGBAND_DIR_XTRA_SOUND);
    string_free(ANGBAND_DIR_XTRA_HELP);

#ifdef HAS_CLEANUP
    cleanup_angband();
#endif /* HAS_CLEANUP */

    //    exit(0);
}



/*** Initialize ***/



//int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst,
//                       LPSTR lpCmdLine, int nCmdShow)
errr
init_win(void)
{
    int i;

    WNDCLASS wc;
    HDC hdc;
    MSG msg;
    HINSTANCE hPrevInst = NULL;
    HINSTANCE hInst = getHINST();
	
    /* Initialize */
    if (hPrevInst == NULL)
    {
	wc.style         = CS_CLASSDC;
	wc.lpfnWndProc   = AngbandWndProc;
	wc.cbClsExtra    = 0;
	wc.cbWndExtra    = 4; // one long pointer to term_data 
	wc.hInstance     = hInst;
	wc.hIcon         = hIcon = LoadIcon(hInst, "LANGBAND");
	wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = GetStockObject(BLACK_BRUSH);
	wc.lpszMenuName  = "LANGBAND";
	wc.lpszClassName = AppName;

	if (!RegisterClass(&wc)) exit(1);

	wc.lpfnWndProc   = AngbandListProc;
	wc.lpszMenuName  = NULL;
	wc.lpszClassName = AngList;

	if (!RegisterClass(&wc)) exit(2);


    }

//	hack_plog("Init..");
    /* Temporary hooks */
    plog_aux = hack_plog;
    quit_aux = hack_quit;
    core_aux = hack_quit;


    /* Initialize the keypress analyzer */
    for (i = 0; special_key_list[i]; i++)
    {
	special_key[special_key_list[i]] = TRUE;
    }

//	hack_plog("Init.. col");
    /* Determine if display is 16/256/true color */
    hdc = GetDC(NULL);
    colors16 = (GetDeviceCaps(hdc, BITSPIXEL) == 4);
    paletted = ((GetDeviceCaps(hdc, RASTERCAPS) & RC_PALETTE) ? TRUE : FALSE);
    ReleaseDC(NULL, hdc);

//	hack_plog("Init.. colarr");
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

    //hack_plog("Init.. win");
    C_MAKE(loc_terms,MAX_TERM_DATA,term_data);
    
    /* Prepare the windows */
    init_windows(getParentWnd());
//	hack_plog("fish..");
	
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

    pause_line(23);
    play_game(TRUE);
    /* Prompt the user */
//	c_prt(TERM_WHITE,"[Choose 'New' or 'Open' from the 'File' menu]", 23, 17);
    Term_fresh();

//	hack_plog("sheesh..");
    /* Process messages forever */
/*	while (GetMessage(&msg, NULL, 0, 0))
	{
	TranslateMessage(&msg);
	DispatchMessage(&msg);
	}
*/
    /* Paranoia */
//	quit(NULL);

    /* Paranoia */
    return (0);
}

#endif /* WINDOWS */
