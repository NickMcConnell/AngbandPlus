/* File: main-wce.c */

/*
 * Copyright (c) 1997 Ben Harrison, Skirmantas Kligys, Robert Ruehlmann,
 * and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */


/*
 * This file helps Angband work with WindowsCE handhelds.
 *
 * The WinCE version has been tested to compile with eMbedded Visual C++ 4.0
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

#define HAS_CLEANUP


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
 * Switch on for compiling ZAngband with the new wilderness
 * (version 2.5.0 and later).
 */
#define ZANGBAND_WILDERNESS


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
#ifdef _WIN32_WCE
#else
#define USE_SAVER
#endif

/*
 * Menu constants -- see "ANGBAND.RC"
 */
 
#ifdef _WIN32_WCE
#define NUM_MACRO_BUTTONS		18
#define MACRO_BUTTON			50000
// SJG These are defined in the resource.h file
#else
#define IDM_FILE_NEW                    100
#define IDM_FILE_OPEN                   101
#define IDM_FILE_SAVE                   110
#define IDM_FILE_SCORE                  120
#define IDM_FILE_EXIT                   130

#define IDM_WINDOW_VIS_0                200
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
#define IDM_WINDOW_D_HGT_6              276
#define IDM_WINDOW_D_HGT_7              277

#define IDM_OPTIONS_NO_GRAPHICS  400
#define IDM_OPTIONS_OLD_GRAPHICS 401
#define IDM_OPTIONS_NEW_GRAPHICS 402
#define IDM_OPTIONS_GRAPHICS_DAVID  403
#define IDM_OPTIONS_GRAPHICS_NICE   405
#define IDM_OPTIONS_TRPTILE         407
#define IDM_OPTIONS_DBLTILE         408
#define IDM_OPTIONS_BIGTILE         409
#define IDM_OPTIONS_SOUND               410
#define IDM_OPTIONS_SAVER               430
#define IDM_OPTIONS_MAP                 440
#define IDM_OPTIONS_SMALL_SCREEN        450

#define IDM_HELP_GENERAL                901
#define IDM_HELP_SPOILERS               902
#endif /* _WIN32_WCE */
/*
 * Define Graphics
 */
#define GRAPHICS_NONE           0
#define GRAPHICS_ORIGINAL       1
#define GRAPHICS_ADAM_BOLT      2


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
#ifdef _WIN32_WCE
#else
#define NORESOURCE        /* Resource management */
#endif
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
#include <windowsx.h>

#ifdef _WIN32_WCE

#include "resource.h"

#include <commctrl.h>
#include <aygshell.h>
#include <sipapi.h>

#define MAX_LOADSTRING 100

static SHACTIVATEINFO s_sai;

//#include "gx.h"

//GXDisplayProperties g_gxdp;    // GX struct
//GXKeyList g_gxkl;              // GX struct

// Forward declarations of functions included in this code module:
ATOM				MyRegisterClass	(HINSTANCE, LPTSTR);
BOOL				InitInstance	(HINSTANCE, int);
HWND				CreateRpCommandBar(HWND);
HWND				CreateMacroBar(HWND);
int add_button_win(char *label, unsigned char keypress);
int kill_button_win(unsigned char keypress);


#define SHGetSubMenu(hWndMB,ID_MENU) (HMENU)SendMessage((hWndMB), SHCMBM_GETSUBMENU, (WPARAM)0, (LPARAM)ID_MENU);

/* This is because the verison of gx.h I have is not C friendly.
So I will just prototype the functions I need here. */

typedef int (*pFuncPlgn_GXStuff)();
typedef struct GXKeyList (*pGXGetDefaultKeys)();

/* static cptr cecommand[18] = {"1","2","3","4","5","6","7","8","9","0","l","g","w","a","u","z","q","r"};
static int cecommandnum[18] = {49,50,51,52,53,54,55,56,57,48,108,103,119,97,117,122,113,114}; 
static cptr cecommand[18]    = {" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "}; 
static cptr cecommand[18]    = {"","","","","","","","","","","","","","","","","",""};
static int  cecommandnum[18] = {' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '}; */

#endif /* _WIN32_WCE */

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

#include <commdlg.h>
#include <shellapi.h>

/*
 * HTML-Help requires htmlhelp.h and htmlhelp.lib from Microsoft's
 * HTML Workshop < http://msdn.microsoft.com/workshop/author/htmlhelp/ >.
 */
#ifdef HTML_HELP
#include <htmlhelp.h>
#endif /* HTML_HELP */

/*
 * Include the support for loading bitmaps
 */
#ifdef USE_GRAPHICS
# include "readdib.h"
#endif /* USE_GRAPHICS */

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
#define VUD_BRIGHT      0x80


/*
 * Forward declare
 */
typedef struct _term_data term_data;

//static int yOldPos = 0;
//static int xOldPos = 0;
static bool term_initialised = FALSE;

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

        int grid_display;
  /*    bool map_active;*/
};


/*
 * Maximum number of windows XXX XXX XXX
 */
#ifdef _WIN32_WCE
#define MAX_TERM_DATA 1
#else
#define MAX_TERM_DATA TERM_WIN_MAX
#endif

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

#ifdef _WIN32_WCE
static HPEN hPenYellow;
#else
static HBRUSH hbrYellow;
#endif

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
 * Flag set when switching tilesizes
 */
static bool change_tilesize = FALSE;

/*
 * The global bitmap
 */
static DIBINIT infGraph;

/*
 * The global bitmap mask
 */
static DIBINIT infMask;

#endif /* USE_GRAPHICS */


#ifdef USE_SOUND

/*
 * Flag set once "sound" has been initialized
 */
static bool can_use_sound = FALSE;

#define SAMPLE_MAX 8

/*
 * An array of sound file names
 */
static cptr sound_file[SOUND_MAX][SAMPLE_MAX];

#endif /* USE_SOUND */


/*
 * Full path to ANGBAND.INI
 */
static cptr ini_file = NULL;

/*
 * Name of application
 */
static cptr AppName = "ANGBAND";

/*
 * Name of sub-window type
 */
static cptr AngList = "AngList";

/*
 * Directory names
 */
static cptr ANGBAND_DIR_XTRA_FONT;
#ifdef _WIN32_WCE
cptr ANGBAND_DIR_XTRA_GRAF;
#else
static cptr ANGBAND_DIR_XTRA_GRAF;
#endif
static cptr ANGBAND_DIR_XTRA_SOUND;
static cptr ANGBAND_DIR_XTRA_HELP;
#if 0
static cptr ANGBAND_DIR_XTRA_MUSIC;
#endif /* 0 */

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
        VID_BLACK,                                      /* Dark */
        VID_WHITE,                                      /* White */
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


#ifdef SUPPORT_GAMMA
static int gamma_correction;
#endif /* SUPPORT_GAMMA */


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
        VK_CLEAR,               /* 0x0C (KP<5>) */

	VK_PAUSE,		/* 0x13 (pause) */

	VK_PRIOR,		/* 0x21 (KP<9>) */
	VK_NEXT,		/* 0x22 (KP<3>) */
	VK_END,			/* 0x23 (KP<1>) */
	VK_HOME,		/* 0x24 (KP<7>) */
	VK_LEFT,		/* 0x25 (KP<4>) */
        VK_UP,                  /* 0x26 (KP<8>) */
        VK_RIGHT,               /* 0x27 (KP<6>) */
        VK_DOWN,                /* 0x28 (KP<2>) */
        VK_SELECT,              /* 0x29 (?????) */
        VK_PRINT,               /* 0x2A (?????) */
        VK_EXECUTE,             /* 0x2B (?????) */
        VK_SNAPSHOT,    /* 0x2C (?????) */
        VK_INSERT,              /* 0x2D (KP<0>) */
        VK_DELETE,              /* 0x2E (KP<.>) */
        VK_HELP,                /* 0x2F (?????) */

#if 0
        VK_NUMPAD0,             /* 0x60 (KP<0>) */
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
#endif /* 0 */

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

#ifdef _WIN32_WCE

#include "angbandcw.h"
WINCEGLOBALS g_g;

#endif /* _WIN32_WCE */

#if 0
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
#endif /* 0 */

#if 0 /* SJG can't see that this is called */
static void show_win_error(void)
{
	LPVOID lpMsgBuf;

	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
	              NULL, GetLastError(),
	              MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
	              (LPTSTR) &lpMsgBuf, 0, NULL);

	MessageBox(NULL, lpMsgBuf, "Error", MB_OK | MB_ICONINFORMATION);

	LocalFree(lpMsgBuf);
}
#endif /* 0 */

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
#ifdef _WIN32_WCE
	{
		TCHAR wcpath[1024];
		mbstowcs( wcpath, path, 1024);
		attrib = GetFileAttributes(wcpath);
	}
#else
        attrib = GetFileAttributes(path);
#endif

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
#ifdef _WIN32_WCE
	{
		TCHAR wcpath[1024];
		mbstowcs( wcpath, path, 1024);
		attrib = GetFileAttributes(wcpath);
	}
#else
        attrib = GetFileAttributes(path);
#endif

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
        if (td->cols > 255) td->cols = 255;
        if (td->rows > 255) td->rows = 255;
#ifdef _WIN32_WCE
	// breaks WCE version!
#else
        if (use_graphics_nice)
          {
            switch (use_graphics)
              {
              case GRAPHICS_DAVID_GERVAIS:
                {
                  /* Reset the tile info */
                  td->tile_wid = 32;
                  td->tile_hgt = 32;
                  break;
                }
                
              case GRAPHICS_ADAM_BOLT:
                {
                  /* Reset the tile info */
                  td->tile_wid = 16;
                  td->tile_hgt = 16;
                  break;
                }
                
              case GRAPHICS_ORIGINAL:
                {
                  /* Reset the tile info */
                  td->tile_wid = 8;
                  td->tile_hgt = 8;
                  break;
                }
                
              case GRAPHICS_NONE:
                {
                  /* Reset the tile info */
                  td->tile_wid = td->font_wid;
                  td->tile_hgt = td->font_hgt;
                  break;
                }
                
              }
            
            use_trptile = FALSE;
            use_dbltile = FALSE;
            use_bigtile = FALSE;
            
            if ((td->tile_hgt >= td->font_hgt * 3) && 
                (td->tile_wid >= td->font_wid * 3))
              {
                use_trptile = TRUE;
                td->tile_wid /= 3;
                td->tile_hgt /= 3;
              }
            else if ((td->tile_hgt >= td->font_hgt * 2) && 
                     (td->tile_wid >= td->font_wid * 2))
              {
                use_dbltile = TRUE;
                td->tile_wid /= 2;
                td->tile_hgt /= 2;
              }
            
            if (td->tile_wid >= td->font_wid * 2)
              {
                use_bigtile = TRUE;
                td->tile_wid /= 2;
              }
            
            if (td->tile_wid < td->font_wid) td->tile_wid = td->font_wid;
            if (td->tile_hgt < td->font_hgt) td->tile_hgt = td->font_hgt;
          }
#endif
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

#ifdef _WIN32_WCE
#else
	RECT rc;

	WINDOWPLACEMENT lpwndpl;
#endif

	/* Paranoia */
	if (!td->w) return;

#ifdef _WIN32_WCE
	/* Visible */
	strcpy(buf, td->visible ? "1" : "0");
	WritePrivateProfileString_Reg(sec_name, "Visible", buf, ini_file);

	/* Font */
	strcpy(buf, td->font_file ? td->font_file : "8X13.FON");
	WritePrivateProfileString_Reg(sec_name, "Font", buf, ini_file);

	sprintf(buf, "%d", g_g.m_fakeFont16ColorHack);
	WritePrivateProfileString_Reg(sec_name, "16Colors", buf, ini_file);

	/* Title Bar */
	sprintf(buf, "%d", g_g.m_bTitleBarShown);
	WritePrivateProfileString_Reg(sec_name, "TitleBar", buf, ini_file);

	/* Macro Bar */
	sprintf(buf, "%d", g_g.m_bMacroBarShown);
	WritePrivateProfileString_Reg(sec_name, "MacroBar", buf, ini_file);

	/* Macro Bar */
	sprintf(buf, "%d", small_screen);
	WritePrivateProfileString_Reg(sec_name, "SmallScreen", buf, ini_file);

	/* Bizarre */
	strcpy(buf, td->bizarre ? "1" : "0");
	WritePrivateProfileString_Reg(sec_name, "Bizarre", buf, ini_file);

	/* Tile size (x) */
	sprintf(buf, "%d", td->tile_wid);
	WritePrivateProfileString_Reg(sec_name, "TileWid", buf, ini_file);

	/* Tile size (y) */
	sprintf(buf, "%d", td->tile_hgt);
	WritePrivateProfileString_Reg(sec_name, "TileHgt", buf, ini_file);

#else
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

	/* Get window placement and dimensions */
	lpwndpl.length = sizeof(WINDOWPLACEMENT);
	GetWindowPlacement(td->w, &lpwndpl);

	/* Acquire position in *normal* mode (not minimized) */
	rc = lpwndpl.rcNormalPosition;

	/* Get information about the placement of the window */
	if (lpwndpl.flags & SW_SHOWMAXIMIZED)
		td->maximized = TRUE;
	else
		td->maximized = FALSE;

	/* Window position (x) */
	wsprintf(buf, "%d", rc.left);
	WritePrivateProfileString(sec_name, "PositionX", buf, ini_file);

	/* Window position (y) */
	wsprintf(buf, "%d", rc.top);
	WritePrivateProfileString(sec_name, "PositionY", buf, ini_file);

	/* Maximized */
	strcpy(buf, td->maximized ? "1" : "0");
	WritePrivateProfileString(sec_name, "Maximized", buf, ini_file);
#endif
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

#ifdef _WIN32_WCE
	/* Save the "arg_graphics" flag */
	sprintf(buf, "%d", arg_graphics);
	WritePrivateProfileString_Reg("Angband", "Graphics", buf, ini_file);

	/* Save the "use_trptile" flag */
	strcpy(buf, use_trptile ? "1" : "0");
	WritePrivateProfileString_Reg("Angband", "Trptile", buf, ini_file);

	/* Save the "use_dbltile" flag */
	strcpy(buf, use_dbltile ? "1" : "0");
	WritePrivateProfileString_Reg("Angband", "Dbltile", buf, ini_file);

	/* Save the "use_bigtile" flag */
	strcpy(buf, use_bigtile ? "1" : "0");
	WritePrivateProfileString_Reg("Angband", "Bigtile", buf, ini_file);
#else
	/* Save the "arg_graphics" flag */
        sprintf(buf, "%d", arg_graphics);
        WritePrivateProfileString("Angband", "Graphics", buf, ini_file);

        /* Save the "use_graphics_nice" flag */
        strcpy(buf, arg_graphics_nice ? "1" : "0");
        WritePrivateProfileString("Angband", "Graphics_Nice", buf, ini_file);

        /* Save the "use_trptile" flag */
        strcpy(buf, use_trptile ? "1" : "0");
        WritePrivateProfileString("Angband", "Trptile", buf, ini_file);

        /* Save the "use_dbltile" flag */
        strcpy(buf, use_dbltile ? "1" : "0");
        WritePrivateProfileString("Angband", "Dbltile", buf, ini_file);

        /* Save the "use_bigtile" flag */
        strcpy(buf, use_bigtile ? "1" : "0");
        WritePrivateProfileString("Angband", "Bigtile", buf, ini_file);

        /* Save the "arg_sound" flag */
        strcpy(buf, arg_sound ? "1" : "0");
        WritePrivateProfileString("Angband", "Sound", buf, ini_file);

        /* Save the "small_screen" flag */
        strcpy(buf, small_screen ? "1" : "0");
        WritePrivateProfileString("Angband", "Small_Screen", buf, ini_file);
#endif

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

#ifdef _WIN32_WCE
	/* Visible */
	td->visible = (GetPrivateProfileInt_Reg(sec_name, "Visible", td->visible, ini_file) != 0);

	/* Maximized */
	td->maximized = (GetPrivateProfileInt_Reg(sec_name, "Maximized", td->maximized, ini_file) != 0);

	GetPrivateProfileString_Reg(sec_name, "Font", "5X8.FON", tmp, 127, ini_file);

	/* Title Bar */
	g_g.m_bTitleBarShown = GetPrivateProfileInt_Reg(sec_name, "TitleBar", 1, ini_file);

	g_g.m_bMacroBarShown = GetPrivateProfileInt_Reg(sec_name, "MacroBar", 1, ini_file);

	small_screen = GetPrivateProfileInt_Reg(sec_name, "SmallScreen", 1, ini_file);

	g_g.m_fakeFont16ColorHack = GetPrivateProfileInt_Reg(sec_name, "16Colors", 0, ini_file);

	/* Bizarre */
	td->bizarre = (GetPrivateProfileInt_Reg(sec_name, "Bizarre", td->bizarre, ini_file) != 0);

	/* Analyze font, save desired font name */
	td->font_want = string_make(analyze_font(tmp, &wid, &hgt));

	/* Tile size */
	td->tile_wid = GetPrivateProfileInt_Reg(sec_name, "TileWid", wid, ini_file);
	td->tile_hgt = GetPrivateProfileInt_Reg(sec_name, "TileHgt", hgt, ini_file);

	g_g.g_useWinCEFakeFont = 1;
	g_g.g_mw_fakeFontWidth = td->tile_wid;
	g_g.g_mw_fakeFontHeight = td->tile_hgt;

	/* Window position */
	td->pos_x = GetPrivateProfileInt_Reg(sec_name, "PositionX", td->pos_x, ini_file);
	td->pos_y = GetPrivateProfileInt_Reg(sec_name, "PositionY", td->pos_y, ini_file);

	TestWinCEFakeFont();
#else
	/* Visible */
	td->visible = (GetPrivateProfileInt(sec_name, "Visible", td->visible, ini_file) != 0);

	/* Maximized */
        td->maximized = (GetPrivateProfileInt(sec_name, "Maximized", td->maximized, ini_file) != 0);

        /* Desired font, with default */
        GetPrivateProfileString(sec_name, "Font", "8X13.FON", tmp, 127, ini_file);

        /* Bizarre */
        td->bizarre = (GetPrivateProfileInt(sec_name, "Bizarre", TRUE, ini_file) != 0);

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
#endif
}


/*
 * Load the "prefs"
 */
static void load_prefs(void)
{
	int i;

	char buf[1024];

#ifdef _WIN32_WCE
	/* Extract the "arg_graphics" flag */
	arg_graphics = GetPrivateProfileInt_Reg("Angband", "Graphics", GRAPHICS_NONE, ini_file);

	/* Extract the "use_trptile" flag */
	use_trptile = GetPrivateProfileInt_Reg("Angband", "Trptile", FALSE, ini_file);

	/* Extract the "use_dbltile" flag */
	use_dbltile = GetPrivateProfileInt_Reg("Angband", "Dbltile", FALSE, ini_file);

	/* Extract the "use_bigtile" flag */
	use_bigtile = GetPrivateProfileInt_Reg("Angband", "Bigtile", FALSE, ini_file);
#else
        /* Extract the "arg_graphics" flag */
        arg_graphics = GetPrivateProfileInt("Angband", "Graphics", GRAPHICS_NONE, ini_file);

        /* Extract the "arg_graphics" flag */
        arg_graphics_nice = GetPrivateProfileInt("Angband", "Graphics_Nice", TRUE, ini_file);

        /* Extract the "use_trptile" flag */
        use_trptile = GetPrivateProfileInt("Angband", "Trptile", FALSE, ini_file);

        /* Extract the "use_dbltile" flag */
        use_dbltile = GetPrivateProfileInt("Angband", "Dbltile", FALSE, ini_file);

        /* Extract the "use_bigtile" flag */
        use_bigtile = GetPrivateProfileInt("Angband", "Bigtile", FALSE, ini_file);

        /* Extract the "arg_sound" flag */
        arg_sound = (GetPrivateProfileInt("Angband", "Sound", 0, ini_file) != 0);
        /* Extract the "small_screen" flag */
        small_screen = (GetPrivateProfileInt("Angband", "Small_Screen", 0, ini_file) != 0);
#endif
#ifdef SUPPORT_GAMMA

	/* Extract the gamma correction */
#ifdef _WIN32_WCE
	/* Extract the gamma correction */
	gamma_correction = GetPrivateProfileInt_Reg("Angband", "Gamma", 0, ini_file);
#else
	gamma_correction = GetPrivateProfileInt("Angband", "Gamma", 0, ini_file);
#endif

#endif /* SUPPORT_GAMMA */

        /* Load window prefs */
        for (i = 0; i < MAX_TERM_DATA; ++i)
        {
                term_data *td = &data[i];

		sprintf(buf, "Term-%d", i);

                load_prefs_aux(td, buf);
        }
}


#ifdef USE_SOUND

/*
 * XXX XXX XXX - Taken from files.c.
 *
 * Extract "tokens" from a buffer
 *
 * This function uses "whitespace" as delimiters, and treats any amount of
 * whitespace as a single delimiter.  We will never return any empty tokens.
 * When given an empty buffer, or a buffer containing only "whitespace", we
 * will return no tokens.  We will never extract more than "num" tokens.
 *
 * By running a token through the "text_to_ascii()" function, you can allow
 * that token to include (encoded) whitespace, using "\s" to encode spaces.
 *
 * We save pointers to the tokens in "tokens", and return the number found.
 */
static s16b tokenize_whitespace(char *buf, s16b num, char **tokens)
{
	int k = 0;

	char *s = buf;


	/* Process */
	while (k < num)
	{
                char *t;

                /* Skip leading whitespace */
                for ( ; *s && isspace(*s); ++s) /* loop */;

                /* All done */
                if (!*s) break;

                /* Find next whitespace, if any */
                for (t = s; *t && !isspace(*t); ++t) /* loop */;

                /* Nuke and advance (if necessary) */
                if (*t) *t++ = '\0';

		/* Save the token */
		tokens[k++] = s;

		/* Advance */
		s = t;
	}

	/* Count */
	return (k);
}


static void load_sound_prefs(void)
{
	int i, j, num;
	char tmp[1024];
	char ini_path[1024];
	char wav_path[1024];
        char *zz[SAMPLE_MAX];

        /* Access the sound.cfg */
        path_build(ini_path, 1024, ANGBAND_DIR_XTRA_SOUND, "sound.cfg");

        for (i = 0; i < SOUND_MAX; i++)
        {
                GetPrivateProfileString("Sound", angband_sound_name[i], "", tmp, 1024, ini_path);

                num = tokenize_whitespace(tmp, SAMPLE_MAX, zz);

                for (j = 0; j < num; j++)
                {
                        /* Access the sound */
                        path_build(wav_path, 1024, ANGBAND_DIR_XTRA_SOUND, zz[j]);

                        /* Save the sound filename, if it exists */
                        if (check_file(wav_path))
                                sound_file[i][j] = string_make(zz[j]);
                }
        }
}

#endif /* USE_SOUND */


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
#ifdef USE_GRAPHICS
	HPALETTE hBmPal;
#endif /* USE_GRAPHICS */
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
        //lppeSize = 0;
        lppe = NULL;
        nEntries = 0;

#ifdef USE_GRAPHICS

	/* Check the bitmap palette */
	hBmPal = infGraph.hPalette;

        /* Use the bitmap */
        if (hBmPal)
        {
          //lppeSize = 256 * sizeof(PALETTEENTRY);
          //lppe = (LPPALETTEENTRY)ralloc(lppeSize);
          //lppe = mem_alloc(256 * sizeof(PALETTEENTRY));
          lppe = ralloc(256 * sizeof(PALETTEENTRY));
                nEntries = GetPaletteEntries(hBmPal, 0, 255, lppe);
                if ((nEntries == 0) || (nEntries > 220))
                {
			/* Warn the user */
                        plog("Please switch to high- or true-color mode.");

                        /* Cleanup */
                        //rnfree(lppe, lppeSize);
                        //mem_free(lppe);
                        free(lppe);

                        /* Fail */
                        return (FALSE);
		}
	}

#endif /* USE_GRAPHICS */

	/* Size of palette */
        pLogPalSize = sizeof(LOGPALETTE) + (nEntries + 16) * sizeof(PALETTEENTRY);

        /* Allocate palette */
        //pLogPal = (LPLOGPALETTE)mem_alloc(pLogPalSize);
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

#ifdef SUPPORT_GAMMA

		if (gamma_correction > 0)
		{
			p->peRed = gamma_table[p->peRed];
			p->peGreen = gamma_table[p->peGreen];
			p->peBlue = gamma_table[p->peBlue];
		}

#endif /* SUPPORT_GAMMA */

		/* Save the flags */
#ifdef _WIN32_WCE
		p->peFlags = 0;
#else
		p->peFlags = PC_NOCOLLAPSE;
#endif
	}

        /* Free something */
        //if (lppe) rnfree(lppe, lppeSize);
        //if (lppe) mem_free(lppe);
        if (lppe) free(lppe);

        /* Create a new palette, or fail */
        hNewPal = CreatePalette(pLogPal);
        if (!hNewPal) quit("Cannot create palette!");

        /* Free the palette */
        //rnfree(pLogPal, pLogPalSize);
        //mem_free(pLogPal);
        free(pLogPal);

        /* Main window */
        td = &data[0];

	/* Realize the palette */
	hdc = GetDC(td->w);
	SelectPalette(hdc, hNewPal, 0);
	i = RealizePalette(hdc);
	ReleaseDC(td->w, hdc);
	if (i == 0) quit("Cannot realize palette!");

#ifdef _WIN32_WCE
#else
	/* Sub-windows */
	for (i = 1; i < MAX_TERM_DATA; i++)
	{
		td = &data[i];

		hdc = GetDC(td->w);
		SelectPalette(hdc, hNewPal, 0);
		ReleaseDC(td->w, hdc);
	}
#endif

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
static bool init_graphics(void)
{
	/* Initialize once */
	/* if (can_use_graphics != arg_graphics) */
	{
                char buf[1024];
                int wid, hgt;
                cptr name;

                if (arg_graphics == GRAPHICS_DAVID_GERVAIS)
                {
			wid = 32;
                        hgt = 32;

                        name = "32x32.bmp";

                        ANGBAND_GRAF = "david";

                }
                else if (arg_graphics == GRAPHICS_ADAM_BOLT)
                {
			wid = 16;
                        hgt = 16;

                        name = "16X16.BMP";

                        ANGBAND_GRAF = "new";

                }
                else
                {
			wid = 8;
			hgt = 8;

			name = "8X8.BMP";
			ANGBAND_GRAF = "old";
                }

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

                if (arg_graphics == GRAPHICS_DAVID_GERVAIS)
                {
                        /* Access the mask file */
                        path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_GRAF, "mask32.bmp");

                        /* Load the bitmap or quit */
                        if (!ReadDIB(data[0].w, buf, &infMask))
                        {
                                plog_fmt("Cannot read bitmap file '%s'", buf);
                                return (FALSE);
                        }
                }
                else if (arg_graphics == GRAPHICS_ADAM_BOLT)
                {
                        /* Access the mask file */
                        path_build(buf, 1024, ANGBAND_DIR_XTRA_GRAF, "mask.bmp");

                        /* Load the bitmap or quit */
                        if (!ReadDIB(data[0].w, buf, &infMask))
			{
				plog_fmt("Cannot read bitmap file '%s'", buf);
				return (FALSE);
			}
		}

		/* Activate a palette */
		if (!new_palette())
		{
			/* Free bitmap XXX XXX XXX */

			/* Oops */
			plog("Cannot activate palette!");
			return (FALSE);
		}

		/* Graphics available */
		can_use_graphics = arg_graphics;
	}

	/* Result */
	return (can_use_graphics);
}
#endif /* USE_GRAPHICS */


#ifdef USE_SOUND
/*
 * Initialize sound
 */
static bool init_sound(void)
{
	/* Initialize once */
	if (!can_use_sound)
	{
		/* Load the prefs */
		load_sound_prefs();

		/* Sound available */
		can_use_sound = TRUE;
	}

	/* Result */
	return (can_use_sound);
}
#endif /* USE_SOUND */


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
                        /* Don't check when closing the application */
                        if (!path) break;

                        /* Check "screen" */
                        if ((td != &data[i]) &&
                            (data[i].font_file) &&
			    (streq(data[i].font_file, td->font_file)))
			{
				used = TRUE;
			}
                }

		/* Remove unused font resources */
#ifdef _WIN32_WCE
		{
			TCHAR wcfont_file[1024];
			mbstowcs( wcfont_file, td->font_file, 1024);
			if (!used) RemoveFontResource(wcfont_file);
		}
#else
		if (!used) RemoveFontResource(td->font_file);
#endif

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
#ifdef _WIN32_WCE
#else
	if (!suffix(base, ".FON")) return (1);
#endif

        /* Verify file */
        if (!check_file(buf)) return (1);

	/* Load the new font */
#ifdef _WIN32_WCE
	{
		TCHAR wcbuf[1024];
		mbstowcs( wcbuf, buf, 1024);

#define DONT_ADD_FON_RESOURCE 1
#ifdef DONT_ADD_FON_RESOURCE
#else
		// This does not work on the iPaq!!!
		if (!AddFontResource(wcbuf)) return (1);
#endif
	}
#else
	if (!AddFontResource(buf)) return (1);
#endif

        /* Save new font name */
        td->font_file = string_make(base);

	/* Remove the "suffix" */
	base[strlen(base)-4] = '\0';

#ifdef _WIN32_WCE
	{
		LOGFONT lFont;
		
		lFont.lfHeight			= hgt; 
		lFont.lfWidth			= wid; 
		lFont.lfEscapement		= 0; 
		lFont.lfOrientation		= 0; 
		lFont.lfWeight			= FW_DONTCARE; 
		lFont.lfItalic			= 0; 
		lFont.lfUnderline			= 0; 
		lFont.lfStrikeOut			= 0; 
		lFont.lfCharSet			= ANSI_CHARSET; 
		lFont.lfOutPrecision		= OUT_DEFAULT_PRECIS; 
		lFont.lfClipPrecision		= CLIP_DEFAULT_PRECIS; 
		lFont.lfQuality			= DEFAULT_QUALITY; 
		lFont.lfPitchAndFamily		= FIXED_PITCH | FF_DONTCARE; 


#ifdef DONT_ADD_FON_RESOURCE
		// DEBUG use the TT font I made
		mbstowcs( lFont.lfFaceName, "5x8try", LF_FACESIZE);
#else
		// FWG 3-01-2001 Load a .FON font file does not work on the iPaq.
		mbstowcs( lFont.lfFaceName, base, LF_FACESIZE);
#endif
		
		td->font_id = CreateFontIndirect(&lFont); 
	}
#else
	/* Create the font (using the 'base' of the font file name!) */
	td->font_id = CreateFont(hgt, wid, 0, 0, FW_DONTCARE, 0, 0, 0,
	                         ANSI_CHARSET, OUT_DEFAULT_PRECIS,
	                         CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
	                         FIXED_PITCH | FF_DONTCARE, base);
#endif

	/* Hack -- Unknown size */
	if (!wid || !hgt)
	{
		HDC hdcDesktop;
		HFONT hfOld;
		TEXTMETRIC tm;

                /* all this trouble to get the cell size */
                hdcDesktop = GetDC(HWND_DESKTOP);
                hfOld = (HFONT)SelectObject(hdcDesktop, td->font_id);
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



/*
 * Allow the user to change the font for this window.
 */
static void term_change_font(term_data *td)
{
#ifdef _WIN32_WCE
	OPENFILENAME ofn;
	TCHAR wctmp[1024];
	TCHAR wcxtra_font[1024];

	char tmp[1024] = "";

	/* Extract a default if possible */
	if (td->font_file) strcpy(tmp, td->font_file);

	/* Ask for a choice */
	memset(&ofn, 0, sizeof(ofn));
	ofn.lStructSize = sizeof(ofn);
	ofn.hwndOwner = data[0].w;
	ofn.lpstrFilter = _T("Angband Font Files (*.fon)\0*.fon\0");
	ofn.nFilterIndex = 1;

	mbstowcs( wctmp, tmp, 1024);
	ofn.lpstrFile = wctmp;

	ofn.nMaxFile = 128;

	mbstowcs( wcxtra_font, ANGBAND_DIR_XTRA_FONT, 1024);
	ofn.lpstrInitialDir = wcxtra_font;

	ofn.Flags = OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;
	ofn.lpstrDefExt = _T("fon");

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

		/* Reset the tile info */
		td->tile_wid = td->font_wid;
		td->tile_hgt = td->font_hgt;

		/* Analyze the font */
		term_getsize(td);

		/* Resize the window */
		term_window_resize(td);
	}
#else
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

                /* Assume bizarre */
                td->bizarre = TRUE;

                /* Reset the tile info */
		td->tile_wid = td->font_wid;
		td->tile_hgt = td->font_hgt;

		/* Analyze the font */
		term_getsize(td);

		/* Resize the window */
		term_window_resize(td);
	}
#endif
}

#ifdef _WIN32_WCE
#else
static void windows_map_aux(void);
#endif

/*
 * Hack -- redraw a term_data
 */
static void term_data_redraw(term_data *td)
{
	if (td->grid_display == 3)
	{
		/* Redraw the map */
#ifdef _WIN32_WCE
#else
		windows_map_aux();
#endif
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
static void term_data_redraw_section(term_data *td, int x1, int y1, int x2, int y2)
{
	/* Activate the term */
	Term_activate(&td->t);

	/* Redraw the area */
	Term_redraw_section(x1, y1, x2, y2);

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

#endif /* 0 */


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

        /* Get the main window */
        term_data *td = &data[0];

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

#endif /* USE_SOUND */


#ifdef USE_GRAPHICS

#ifdef _WIN32_WCE
#else
	/* Handle "arg_graphics_nice" */
	if (use_graphics_nice != arg_graphics_nice)
	{
		/* Change setting */
		use_graphics_nice = arg_graphics_nice;

                /* HACK - Assume bizarre */
                td->bizarre = TRUE;

                /* Analyze the font */
                term_getsize(td);

		/* Resize the window */
		term_window_resize(td);
	}
#endif

        /* Handle "arg_graphics" */
        if (use_graphics != arg_graphics)
        {
                /* Free the bitmap stuff */
                FreeDIB(&infGraph);
                FreeDIB(&infMask);

		/* Initialize (if needed) */
		if (arg_graphics && !init_graphics())
		{
			/* Warning */
			plog("Cannot initialize graphics!");

			/* Cannot enable */
			arg_graphics = GRAPHICS_NONE;
		}

		/* Change setting */
		use_graphics = arg_graphics;

		//Term->always_draw = FALSE;
#ifdef _WIN32_WCE
#else
		if (use_graphics_nice)
		{
		  /* HACK - Assume bizarre */
		  td->bizarre = TRUE;
		  
		  /* Analyze the font */
		  term_getsize(td);
		  
		  /* Resize the window */
		  term_window_resize(td);
		}
#endif
		/* Reset visuals */
#ifdef ANGBAND_2_8_1
                reset_visuals();
#else /* ANGBAND_2_8_1 */
                reset_visuals(TRUE);
#endif /* ANGBAND_2_8_1 */

                /* Reset the panel */
                verify_panel();
        }

        /* Handle "change_tilesize" */
        if (change_tilesize)
        {
                /* Reset visuals */
                reset_visuals(TRUE);

                /* Reset the panel */
                verify_panel();

                /* Reset the flag */
                change_tilesize = FALSE;
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
        int i;
        char buf[1024];

        /* Sound disabled */
        if (!use_sound) return (1);

        /* Illegal sound */
        if ((v < 0) || (v >= SOUND_MAX)) return (1);

#ifdef USE_SOUND

	/* Count the samples */
	for (i = 0; i < SAMPLE_MAX; i++)
	{
		if (!sound_file[v][i])
			break;
        }

        /* No sample */
        if (i == 0) return (1);

        /* Build the path */
        path_build(buf, 1024, ANGBAND_DIR_XTRA_SOUND, sound_file[v][rand_int(i)]);

#ifdef WIN32

        /* Play the sound, catch errors */
        return (PlaySound(buf, 0, SND_FILENAME | SND_ASYNC));

#else /* WIN32 */

        /* Play the sound, catch errors */
        return (sndPlaySound(buf, SND_ASYNC));

#endif /* WIN32 */

#else /* USE_SOUND */

        /* Oops */
        return (1);

#endif /* USE_SOUND */
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
 * Notice grid display changing. Set grid_display to new value.
 */
static int Term_xtra_win_grids(int v)
{
        term_data *td = (term_data*)(Term->data);

        td->grid_display = v;

        //Term->always_draw = FALSE;
        

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

                /* Notice screen save / load */
                //case TERM_XTRA_GRIDS:
                //{
                //return (Term_xtra_win_grids(v));
                //}
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

#ifdef _WIN32_WCE
	HPEN hOldPen;
	POINT lines[5];
#endif

	int tile_wid, tile_hgt;

        if (td->grid_display == 3)
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
#ifdef _WIN32_WCE
	//SJG
	hOldPen = (HPEN)SelectObject(hdc, hPenYellow);
	lines[0].x = rc.left;
	lines[0].y = rc.top;
	lines[1].x = rc.right - 1;
	lines[1].y = rc.top;
	lines[2].x = rc.right - 1;
	lines[2].y = rc.bottom - 1;
	lines[3].x = rc.left;
	lines[3].y = rc.bottom - 1;
	lines[4].x = rc.left;
	lines[4].y = rc.top;
	Polyline(hdc, lines, 5);
	SelectObject(hdc, hOldPen);
#else
	FrameRect(hdc, &rc, hbrYellow);
#endif
	ReleaseDC(td->w, hdc);

	/* Success */
	return 0;
}


/*
 * Low level graphics (Assumes valid input).
 *
 * Draw a "cursor" at (x,y), using a "yellow box".
 */
static errr Term_bigcurs_win(int x, int y)
{
	term_data *td = (term_data*)(Term->data);

	RECT rc;
	HDC hdc;

#ifdef _WIN32_WCE
	HPEN hOldPen;
	POINT lines[5];
#endif

        int tile_wid, tile_hgt;

        if (td->grid_display == 3)
        {
                /* Normal cursor in map window */
                Term_curs_win(x, y);
		return 0;
	}
	else
	{
		tile_wid = td->tile_wid;
		tile_hgt = td->tile_hgt;
	}

        /* Frame the grid */
        rc.left = x * tile_wid + td->size_ow1;
        rc.right = rc.left + ((use_trptile && use_bigtile) ? 6 : (use_trptile ? 3 : ((use_dbltile && use_bigtile) ? 4 : 2))) * tile_wid;
        rc.top = y * tile_hgt + td->size_oh1;
        rc.bottom = rc.top + (use_trptile ? 3 : (use_dbltile ? 2 : 1)) * tile_hgt;

	/* Cursor is done as a yellow "box" */
	hdc = GetDC(td->w);
#ifdef _WIN32_WCE
	//SJG
	hOldPen = (HPEN)SelectObject(hdc, hPenYellow);
	lines[0].x = rc.left;
	lines[0].y = rc.top;
	lines[1].x = rc.right - 1;
	lines[1].y = rc.top;
	lines[2].x = rc.right - 1;
	lines[2].y = rc.bottom - 1;
	lines[3].x = rc.left;
	lines[3].y = rc.bottom - 1;
	lines[4].x = rc.left;
	lines[4].y = rc.top;
	Polyline(hdc, lines, 5);
	SelectObject(hdc, hOldPen);
#else
	FrameRect(hdc, &rc, hbrYellow);
#endif
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
static errr Term_text_win(int x, int y, int n, byte a, const char *s)
{
	term_data *td = (term_data*)(Term->data);
#ifdef _WIN32_WCE
	HDC hdc;
	int i;
	int x1, y1, w1, h1;
	int x2, y2, w2, h2;
	int x3, y3, w3, h3;

	int localW2;
	int localH2;

	HDC hdcSrc;
	HBITMAP hbmSrcOld;
	HDC hdcMask;
	HBITMAP hbmSrcMaskOld;

	w1 = w2 = w3 = td->tile_wid;
	h1 = h2 = h3 = td->tile_hgt;
	localW2 = w2;
	localH2 = h2;

	/* Location of window cell */
	x2 = x * td->tile_wid + td->size_ow1;
	y2 = y * td->tile_hgt + td->size_oh1;

	/* Acquire DC */
	hdc = GetDC(td->w);

	/* More info */
	hdcSrc = CreateCompatibleDC(hdc);
	hbmSrcOld = SelectObject(hdcSrc, g_g.g_maskedLetterColors.mImage.hBitmap);

	hdcMask = CreateCompatibleDC(hdc);
	hbmSrcMaskOld = SelectObject(hdcMask, g_g.g_maskedLetters.mImage.hBitmap);

	/* Foreground color */
	if (colors16)
	{
#ifdef _WIN32_WCE
		// I don't know what PALETTEINDEX is but I won't have to worry about
		// it to much unless I support anything besides 12 bit or less.
#else
		SetTextColor(hdc, PALETTEINDEX(win_pal[a]));
#endif
	}
	else if (paletted)
	{
		//SetTextColor(hdc, win_clr[a&0x0F]);
	}
	else
	{
		y3 = 0;
		x3 = a * w3;
	}

	/* Draw attr/char pairs */
	for (i = 0; i < n; i++)
	{
		byte newA = a;
		char newC = s[i];

		/* Extract picture */
/*
		int row = (newA & 0x7F);
		int col = (newC & 0x7F);
*/
		/* Location of bitmap cell */
		x1 = (newC - 31) * w1;

		if (g_g.m_fakeFont16ColorHack)
		{
			y1 = newA * h1;
		}
		else
		{
			y1 = 0;
		}

		/* Perfect size */
		if ((w1 == w2) && (h1 == h2))
		{
			BOOL b;
			
			if (g_g.m_fakeFont16ColorHack)
			{
				/* Copy the terrain picture from the bitmap to the window */
				/* In this mode the mask is actually the src. */
				b = BitBlt(hdc, x2, y2, localW2, localH2, hdcMask, x1, y1, SRCCOPY);
			}
			else
			{
				if (newC != ' ')
				{
					/* Copy the terrain picture from the bitmap to the window */
					b = BitBlt(hdc, x2, y2, localW2, localH2, hdcSrc, x3, y3, SRCCOPY);
					
					
					/* Mask out the tile */
					b = BitBlt(hdc, x2, y2, localW2, localH2, hdcMask, x1, y1, SRCAND);
					
					/* Draw the tile */
					b = BitBlt(hdc, x2, y2, localW2, localH2, hdcSrc, x3, y3, SRCINVERT);
				}
				else
				{
					/* Mask out the tile */
					b = BitBlt(hdc, x2, y2, localW2, localH2, hdcMask, x1, y1, BLACKNESS);
				}
			}
		}
		
		/* Need to stretch */
		else
		{		
			/* Copy the picture from the bitmap to the window */
			StretchBlt(hdc, x2, y2, w2, h2, hdcSrc, x1, y1, w1, h1, SRCCOPY);
		}

		// Part of this used to be handled in the for loop.
		x2 += w2;
	}

	/* Release */
	SelectObject(hdcSrc, hbmSrcOld);
	DeleteDC(hdcSrc);

	/* Release */
	SelectObject(hdcMask, hbmSrcMaskOld);
	DeleteDC(hdcMask);
#else
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
#endif

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
static errr Term_pict_win(int x, int y, int n, const byte *ap, const char *cp, const byte *tap, const char *tcp)
{
	term_data *td = (term_data*)(Term->data);

#ifdef USE_GRAPHICS

        int i;
        int x1, y1, w1, h1;
        int x2, y2, w2, h2, tw2, th2;

        int x3, y3;

        HDC hdcMask;
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
        if (td->grid_display == 3)
        {
                w2 = td->map_tile_wid;
                h2 = td->map_tile_hgt;
                tw2 = w2;
                th2 = h2;
        }
        else
        {
                w2 = td->tile_wid;
                h2 = td->tile_hgt;

                /* Triple tile mode */
                if (use_trptile)
                        th2 = 3 * h2;
                else if (use_dbltile)
                        th2 = 2 * h2;
                else
                        th2 = h2;

                /* Triple tile mode */
                if (use_trptile)
                        tw2 = (use_bigtile ? 6 : 3) * w2;
                else if (use_dbltile)
                        tw2 = (use_bigtile ? 4 : 2) * w2;
                /* big tile mode */
                else if (use_bigtile)
                        tw2 = 2 * w2;
                else
                        tw2 = w2;
	}

	/* Location of window cell */
	x2 = x * w2 + td->size_ow1;
	y2 = y * h2 + td->size_oh1;

	/* Info */
	hdc = GetDC(td->w);

        /* More info */
        hdcSrc = CreateCompatibleDC(hdc);
        hbmSrcOld = (HBITMAP)SelectObject(hdcSrc, infGraph.hBitmap);

        if ((arg_graphics == GRAPHICS_ADAM_BOLT)|| 
            (arg_graphics == GRAPHICS_DAVID_GERVAIS))
	{
		hdcMask = CreateCompatibleDC(hdc);
		SelectObject(hdcMask, infMask.hBitmap);
	}
	else
	{
                hdcMask = NULL;
        }


        /* Draw attr/char pairs */
#if 0
        for (i = 0; i < n; i++, x2 += w2)
#endif
        for (i = n-1; i >= 0; i--, x2 -= w2)
        {
                byte a = ap[i];
                char c = cp[i];

		/* Extract picture */
		int row = (a & 0x7F);
		int col = (c & 0x7F);

		/* Location of bitmap cell */
		x1 = col * w1;
		y1 = row * h1;

		if ((arg_graphics == GRAPHICS_ADAM_BOLT) ||
		    (arg_graphics == GRAPHICS_DAVID_GERVAIS))
		{
			x3 = (tcp[i] & 0x7F) * w1;
                        y3 = (tap[i] & 0x7F) * h1;

                        /* Perfect size */
                        if ((w1 == tw2) && (h1 == th2))
                        {
                                /* Copy the terrain picture from the bitmap to the window */
                                BitBlt(hdc, x2, y2, tw2, th2, hdcSrc, x3, y3, SRCCOPY);
                        

                                /* Only draw if terrain and overlay are different */
                                if ((x1 != x3) || (y1 != y3))
                                  {
                                    /* Mask out the tile */
                                    BitBlt(hdc, x2, y2, tw2, th2, hdcMask, x1, y1, SRCAND);
                                    
                                    /* Draw the tile */
                                    BitBlt(hdc, x2, y2, tw2, th2, hdcSrc, x1, y1, SRCPAINT);
                                  }
                        }

                        /* Need to stretch */
			else
			{
#ifdef _WIN32_WCE
#else
				/* Set the correct mode for stretching the tiles */
                                SetStretchBltMode(hdc, COLORONCOLOR);
#endif

                                /* Copy the terrain picture from the bitmap to the window */
                                StretchBlt(hdc, x2, y2, tw2, th2, hdcSrc, x3, y3, w1, h1, SRCCOPY);

                                /* Only draw if terrain and overlay are different */
                                if ((x1 != x3) || (y1 != y3))
                                {
                                        /* Mask out the tile */
                                        StretchBlt(hdc, x2, y2, tw2, th2, hdcMask, x1, y1, w1, h1, SRCAND);

                                        /* Draw the tile */
                                        StretchBlt(hdc, x2, y2, tw2, th2, hdcSrc, x1, y1, w1, h1, SRCPAINT);
                                }
                        }
                }
                else
                {
                        /* Perfect size */
                        if ((w1 == tw2) && (h1 == th2))
                        {
                                /* Copy the picture from the bitmap to the window */
                                BitBlt(hdc, x2, y2, tw2, th2, hdcSrc, x1, y1, SRCCOPY);
                        }

                        /* Need to stretch */
			else
			{
#ifdef _WIN32_WCE
#else
				/* Set the correct mode for stretching the tiles */
				SetStretchBltMode(hdc, COLORONCOLOR);
#endif

                                /* Copy the picture from the bitmap to the window */
                                StretchBlt(hdc, x2, y2, tw2, th2, hdcSrc, x1, y1, w1, h1, SRCCOPY);
                        }
                }
        }

	/* Release */
	SelectObject(hdcSrc, hbmSrcOld);
	DeleteDC(hdcSrc);

	if ((arg_graphics == GRAPHICS_ADAM_BOLT) ||
	    (arg_graphics == GRAPHICS_DAVID_GERVAIS))
	{
		/* Release */
		SelectObject(hdcMask, hbmSrcOld);
		DeleteDC(hdcMask);
	}

	/* Release */
	ReleaseDC(td->w, hdc);

#else /* USE_GRAPHICS */

	/* Just erase this grid */
	return (Term_wipe_win(x, y, n));

#endif /* USE_GRAPHICS */

	/* Success */
	return 0;
}

#ifdef _WIN32_WCE
#else
static void windows_map_aux(void)
{
	term_data *td = &data[0];
	byte a;
	char c;
	int x, min_x, max_x;
	int y, min_y, max_y;
        byte ta;
        char tc;

        s16b py = p_ptr->py;
        s16b px = p_ptr->px;

#ifdef ZANGBAND

        td->map_tile_wid = (td->tile_wid * td->cols) / MAX_WID;
        td->map_tile_hgt = (td->tile_hgt * td->rows) / MAX_HGT;

#ifdef ZANGBAND_WILDERNESS
        
        min_x = min_wid;
        min_y = min_hgt;
        max_x = max_wid;
        max_y = max_hgt;

#else /* ZANGBAND_WILDERNESS */

        min_x = 0;
        min_y = 0;
        max_x = cur_wid;
        max_y = cur_hgt;

#endif /* ZANGBAND_WILDERNESS */

#else /* ZANGBAND */

        td->map_tile_wid = (td->tile_wid * td->cols) / DUNGEON_WID;
        td->map_tile_hgt = (td->tile_hgt * td->rows) / DUNGEON_HGT;

	min_x = 0;
	min_y = 0;
        max_x = DUNGEON_WID;
        max_y = DUNGEON_HGT;

#endif /* ZANGBAND */

        /* Draw the map */
        for (x = min_x; x < max_x; x++)
        {
                for (y = min_y; y < max_y; y++)
                {
                        map_info(y, x, &a, &c, &ta, &tc);

                        /* Ignore non-graphics */
                        if ((a & 0x80) && (c & 0x80))
			{
				Term_pict_win(x - min_x, y - min_y, 1, &a, &c, &ta, &tc);
			}
		}
        }

        /* Hilite the player */
        Term_curs_win(px - min_x, py - min_y);
}


/*
 * MEGA_HACK - Display a graphical map of the dungeon.
 */
static void windows_map(void)
{
        term_data *td = &data[0];
        event_type ke;

        int old_display = td->grid_display;

        /* Only in graphics mode since the fonts can't be scaled */
        if (!use_graphics) return;

	/* Prevent various menu-actions from working */
	initialized = FALSE;

        /* Clear screen */
        Term_xtra_win_clear();

        td->grid_display = 3;

        /* Draw the map */
        windows_map_aux();

        /* Wait for a keypress, flush key buffer */
        Term_inkey(&ke, TRUE, TRUE);
        Term_flush();

        /* Switch off the map display */
        td->grid_display = old_display;

        /* Restore screen */
        Term_xtra_win_clear();
	Term_redraw();

	/* We are ready again */
	initialized = TRUE;
}

#endif
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
#endif /* 0 */

	/* Prepare the template hooks */
	t->user_hook = Term_user_win;
	t->xtra_hook = Term_xtra_win;
	t->curs_hook = Term_curs_win;
	t->bigcurs_hook = Term_bigcurs_win;
	t->wipe_hook = Term_wipe_win;
        t->text_hook = Term_text_win;
        t->pict_hook = Term_pict_win;

        /* Notice when grid display changes */
        //t->notice_grid = TRUE;

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
	td->rows = 24;
	td->cols = 80;
	td->visible = TRUE;
#ifdef _WIN32_WCE
	td->size_ow1 = 0;
	td->size_ow2 = 0;
	td->size_oh1 = 0;
	td->size_oh2 = 0;
	td->pos_x = 0;
	td->pos_y = 0;
#else
	td->size_ow1 = 2;
        td->size_ow2 = 2;
        td->size_oh1 = 2;
        td->size_oh2 = 2;
        td->pos_x = 7 * 30;
        td->pos_y = 7 * 20;
#endif
        td->grid_display = 1;
	
#ifdef _WIN32_WCE
#else
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
                td->grid_display = 0;
        }
#endif

	/* Load prefs */
	load_prefs();


	/* Main window (need these before term_getsize gets called) */
	td = &data[0];
#ifdef _WIN32_WCE
	td->dwStyle = (WS_VISIBLE);
#else
	td->dwStyle = (WS_OVERLAPPED | WS_THICKFRAME | WS_SYSMENU |
	               WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_CAPTION |
	               WS_VISIBLE);
	if (td->maximized) td->dwStyle |= WS_MAXIMIZE;
#endif
	td->dwExStyle = 0;
	td->visible = TRUE;

#ifdef _WIN32_WCE
#else
	/* Sub windows (need these before term_getsize gets called) */
        for (i = 1; i < MAX_TERM_DATA; i++)
        {
                td = &data[i];
                td->dwStyle = (WS_OVERLAPPED | WS_THICKFRAME | WS_SYSMENU);
                if (td->maximized) td->dwStyle |= WS_MAXIMIZE;
                td->dwExStyle = (WS_EX_TOOLWINDOW);
        }
#endif

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

                        /* Assume bizarre */
                        td->bizarre = TRUE;
                }

		/* Analyze the font */
		term_getsize(td);

		/* Resize the window */
		term_window_resize(td);
	}

#ifdef _WIN32_WCE
#else
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
#endif

	/* Main window */
	td = &data[0];

	/* Main window */
	my_td = td;
#ifdef _WIN32_WCE
	{
		TCHAR wcAppName[1024];
		TCHAR wc_tds[1024];
		
		mbstowcs( wcAppName, AppName, 1024);
		mbstowcs( wc_tds, td->s, 1024);

		// take in account for menusize
		CalculateWindowExtents();
		
		td->w = CreateWindowEx(
			td->dwExStyle, 
			wcAppName,      
			wc_tds,
			WS_VISIBLE,
	        g_g.m_winX,
			g_g.m_winY,                    
			g_g.m_winW, 
			g_g.m_winH,      
			NULL, 
			NULL,
			hInstance, 
			NULL);
		g_g.g_mainhWnd = td->w;

	}
#else
	td->w = CreateWindowEx(td->dwExStyle, AppName,
	                       td->s, td->dwStyle,
	                       td->pos_x, td->pos_y,
                               td->size_wid, td->size_hgt,
                               HWND_DESKTOP, NULL, hInstance, NULL);
#endif

        my_td = NULL;
        if (!td->w) quit("Failed to create Angband window");

#ifdef _WIN32_WCE
	if (!g_g.m_bTitleBarShown) DoSHFullScreen(g_g.g_mainhWnd, SHFS_HIDETASKBAR);
/*	if (g_g.m_bMacroBarShown) g_g.g_TBhWnd = CreateMacroBar(g_g.g_mainhWnd); */

	td->cols = g_g.m_winW / td->tile_wid;
/*	td->rows = (g_g.m_winH - ((g_g.m_bMacroBarShown) ? g_g.m_MacroBarHeight : 0)) / td->tile_hgt; */
	td->rows = g_g.m_winH / td->tile_hgt;
#endif

        term_data_link(td);
        angband_term[0] = &td->t;


        /*
         * Reset map size if required
         */

        /* Mega-Hack -- no panel yet */
        panel_row_min = 0;
        panel_row_max = 0;
        panel_col_min = 0;
        panel_col_max = 0;


        /* Activate the main window */
        SetActiveWindow(td->w);

	/* Bring main window back to top */
	SetWindowPos(td->w, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);

#ifdef SUPPORT_GAMMA

	if (gamma_correction > 0)
		build_gamma_table(gamma_correction);

#endif /* SUPPORT_GAMMA */

	/* New palette XXX XXX XXX */
	(void)new_palette();


	/* Create a "brush" for drawing the "cursor" */
#ifdef _WIN32_WCE
	hPenYellow = CreatePen(PS_SOLID, 1, win_clr[TERM_YELLOW]);
#else
	hbrYellow = CreateSolidBrush(win_clr[TERM_YELLOW]);
#endif

        /* Process pending messages */
        (void)Term_xtra_win_flush();

        /* Initialised */
        term_initialised = TRUE;

}



/*
 * Prepare the menus
 */
static void setup_menus(void)
{
#ifdef _WIN32_WCE
	// Done elsewhere
#else
	int i;

        HMENU hm = GetMenu(data[0].w);


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
        if (character_generated)
        {
                /* Menu "File", Item "Save" */
                EnableMenuItem(hm, IDM_FILE_SAVE, MF_BYCOMMAND | MF_ENABLED);
        }

        /* Menu "File", Item "Exit" */
        EnableMenuItem(hm, IDM_FILE_EXIT, MF_BYCOMMAND | MF_ENABLED);

        /* Menu "File", Item "Show Scores" */
        EnableMenuItem(hm, IDM_FILE_SCORE, MF_BYCOMMAND | MF_ENABLED);


        /* Menu "Window::Visibility" */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		EnableMenuItem(hm, IDM_WINDOW_VIS_0 + i,
		               MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

		CheckMenuItem(hm, IDM_WINDOW_VIS_0 + i,
		              (data[i].visible ? MF_CHECKED : MF_UNCHECKED));

		EnableMenuItem(hm, IDM_WINDOW_VIS_0 + i,
		               MF_BYCOMMAND | MF_ENABLED);
	}

	/* Menu "Window::Font" */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		EnableMenuItem(hm, IDM_WINDOW_FONT_0 + i,
		               MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

		if (data[i].visible)
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
		              (data[i].bizarre ? MF_CHECKED : MF_UNCHECKED));

		if (data[i].visible)
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

		if (data[i].visible)
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

		if (data[i].visible)
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

		if (data[i].visible)
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

		if (data[i].visible)
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
        EnableMenuItem(hm, IDM_OPTIONS_GRAPHICS_DAVID,
                       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        EnableMenuItem(hm, IDM_OPTIONS_GRAPHICS_NICE,
                       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        EnableMenuItem(hm, IDM_OPTIONS_TRPTILE,
                       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        EnableMenuItem(hm, IDM_OPTIONS_DBLTILE,
                       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        EnableMenuItem(hm, IDM_OPTIONS_BIGTILE,
                       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        EnableMenuItem(hm, IDM_OPTIONS_SOUND,
                       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        EnableMenuItem(hm, IDM_OPTIONS_SMALL_SCREEN,
                       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        EnableMenuItem(hm, IDM_OPTIONS_SAVER,
                       MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

        /* Menu "Options", Item "Map" */
        if (use_graphics != GRAPHICS_NONE)
                EnableMenuItem(GetMenu(data[0].w), IDM_OPTIONS_MAP, MF_BYCOMMAND | MF_ENABLED);
        else
                EnableMenuItem(GetMenu(data[0].w), IDM_OPTIONS_MAP,
                               MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

        /* Menu "Options", update all */
        CheckMenuItem(hm, IDM_OPTIONS_NO_GRAPHICS,
                      (arg_graphics == GRAPHICS_NONE ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_OLD_GRAPHICS,
                      (arg_graphics == GRAPHICS_ORIGINAL ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_NEW_GRAPHICS,
                      (arg_graphics == GRAPHICS_ADAM_BOLT ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_GRAPHICS_DAVID,
                      (arg_graphics == GRAPHICS_DAVID_GERVAIS ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_GRAPHICS_NICE,
                      (arg_graphics_nice ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_TRPTILE,
                      (use_trptile ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_DBLTILE,
                      (use_dbltile ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_BIGTILE,
                      (use_bigtile ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_SOUND,
                      (arg_sound ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_SMALL_SCREEN,
                      (small_screen ? MF_CHECKED : MF_UNCHECKED));
        CheckMenuItem(hm, IDM_OPTIONS_SAVER,
                      (hwndSaver ? MF_CHECKED : MF_UNCHECKED));

#ifdef USE_GRAPHICS
        /* Menu "Options", Item "Graphics" */
        EnableMenuItem(hm, IDM_OPTIONS_NO_GRAPHICS, MF_ENABLED);
        EnableMenuItem(hm, IDM_OPTIONS_OLD_GRAPHICS, MF_ENABLED);
        EnableMenuItem(hm, IDM_OPTIONS_NEW_GRAPHICS, MF_ENABLED);
        EnableMenuItem(hm, IDM_OPTIONS_GRAPHICS_DAVID, MF_ENABLED);
        EnableMenuItem(hm, IDM_OPTIONS_GRAPHICS_NICE, MF_ENABLED);
        EnableMenuItem(hm, IDM_OPTIONS_TRPTILE, MF_ENABLED);
        EnableMenuItem(hm, IDM_OPTIONS_DBLTILE, MF_ENABLED);
        EnableMenuItem(hm, IDM_OPTIONS_BIGTILE, MF_ENABLED);
#endif /* USE_GRAPHICS */

#ifdef USE_SOUND
        /* Menu "Options", Item "Sound" */
        EnableMenuItem(hm, IDM_OPTIONS_SOUND, MF_ENABLED);
#endif /* USE_SOUND */
        EnableMenuItem(hm, IDM_OPTIONS_SMALL_SCREEN, MF_ENABLED);

#ifdef USE_SAVER
        /* Menu "Options", Item "ScreenSaver" */
        EnableMenuItem(hm, IDM_OPTIONS_SAVER,
                       MF_BYCOMMAND | MF_ENABLED);
#endif /* USE_SAVER */

#endif /* _WIN32_WCE */
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

#ifdef _WIN32_WCE
#else
        /* Second arg */
        p = strchr(s, ' ');

        /* Tokenize, advance */
        if (p) *p++ = '\0';
#endif

        /* No args */
        if (!*s) return;

        /* Extract filename */
        strcat(savefile, s);

        /* Validate the file */
        validate_file(savefile);

        /* Game in progress */
        game_in_progress = TRUE;

#ifdef _WIN32_WCE
    /* Create the toolbar */
    if (g_g.m_bMacroBarShown)
	{
		g_g.g_TBhWnd = CreateMacroBar(g_g.g_mainhWnd);
	    MoveWindow(g_g.g_mainhWnd, g_g.m_winX, g_g.m_winY, g_g.m_winW, g_g.m_winH, TRUE);
	}
#endif

        /* Play game */
        play_game(FALSE);
}


/*
 * Display a help file
 */
static void display_help(cptr filename)
{
        char tmp[1024];

        path_build(tmp, 1024, ANGBAND_DIR_XTRA_HELP, filename);

        if (check_file(tmp))
        {
#ifdef HTML_HELP

                HtmlHelp(data[0].w, tmp, HH_DISPLAY_TOPIC, 0);

#else /* HTML_HELP */

#ifdef _WIN32_WCE
		// I don't know what WinExec is. I have to figure it out.
#else
                char buf[1024];

                sprintf(buf, "winhelp.exe %s", tmp);
                WinExec(buf, SW_NORMAL);
#endif

#endif /* HTML_HELP */

        }
        else
        {
		plog_fmt("Cannot find help file: %s", tmp);
		plog("Use the online help files instead.");
	}
}

#ifdef _WIN32_WCE
//DoSHFullScreen
// demonstrates how to show app fullscreen
BOOL DoSHFullScreen(HWND hwndRequester, DWORD dwState)
{
  // Valid states
  //#define SHFS_SHOWTASKBAR            0x0001
  //#define SHFS_HIDETASKBAR            0x0002
  //#define SHFS_SHOWSIPBUTTON          0x0004
  //#define SHFS_HIDESIPBUTTON          0x0008
  //#define SHFS_SHOWSTARTICON          0x0010
  //#define SHFS_HIDESTARTICON          0x0020
  
  return SHFullScreen(hwndRequester, dwState);
}

// function prototype
void handle_fakefont(HWND hWnd, term_data *td, int width, int height, struct CONTROLBITMAPS* pMask, struct CONTROLBITMAPS* pColor, BOOL bMove);

#endif
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
#ifdef _WIN32_WCE

	case ID_OPTIONS_MAINWINDOW_TITLEBAR:
		{
			DWORD dwRet;
			HMENU hSetupMenu;

			if (g_g.m_bTitleBarShown)
			{
				DoSHFullScreen(data[0].w, SHFS_HIDETASKBAR);

				g_g.m_winY -= g_g.m_MenuHeight;
				g_g.m_winH += g_g.m_MenuHeight;

				MoveWindow(data[0].w, g_g.m_winX, g_g.m_winY, g_g.m_winW, g_g.m_winH, TRUE);

				if (g_g.g_TBhWnd)
				{
					MoveWindow (g_g.g_TBhWnd, 
									g_g.m_winX, 
									g_g.g_cy - g_g.m_MenuHeight - g_g.m_MacroBarHeight, 
									g_g.m_winW, 
									g_g.m_MacroBarHeight,
									TRUE);
				}
			}
			else
			{
				DoSHFullScreen(data[0].w, SHFS_SHOWTASKBAR);

				g_g.m_winY += g_g.m_MenuHeight;
				g_g.m_winH -= g_g.m_MenuHeight;

				MoveWindow(data[0].w, g_g.m_winX, g_g.m_winY, g_g.m_winW, g_g.m_winH, TRUE);

				if (g_g.g_TBhWnd)
				{
					MoveWindow (g_g.g_TBhWnd, 
									g_g.m_winX, 
									g_g.g_cy - (2 * g_g.m_MenuHeight) - g_g.m_MacroBarHeight, 
									g_g.m_winW, 
									g_g.m_MacroBarHeight,
									TRUE);
				}
			}

			g_g.m_bTitleBarShown = !g_g.m_bTitleBarShown;
			
			hSetupMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
			
			dwRet = CheckMenuItem(hSetupMenu
				, ID_OPTIONS_MAINWINDOW_TITLEBAR 
				, (g_g.m_bTitleBarShown == 0) 
				? (MF_UNCHECKED | MF_BYCOMMAND) 
				: (MF_CHECKED | MF_BYCOMMAND) ); 
		}
		break;

	/**********************************************************************\
		Setup Stuff
	\**********************************************************************/
	case ID_OPTIONS_MAINWINDOW_FONT_16COLORHACKFAST:
		td = &data[0];

		g_g.m_fakeFont16ColorHack = !g_g.m_fakeFont16ColorHack;

		handle_fakefont(data[0].w
				, td
				, g_g.g_mw_fakeFontWidth
				, g_g.g_mw_fakeFontHeight
				, &g_g.g_maskedLetters
				, &g_g.g_maskedLetterColors
				, FALSE);

		UpdateFontMenuChoices(g_g.g_hwndCB);

		/* Debug I don't think we need this. */
		InvalidateRect(g_g.g_mainhWnd, NULL, FALSE);
		break;


	case ID_OPTIONS_MAINWINDOW_FONT_FONT0:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT1:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT2:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT3:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT4:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT5:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT6:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT7:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT8:
	case ID_OPTIONS_MAINWINDOW_FONT_FONT9:

		{
			int rv;
			short offset;

			td = &data[0];

			offset = wCmd - ID_OPTIONS_MAINWINDOW_FONT_FONT0;
			
			rv = HandleFontMenuChoices(g_g.g_hwndCB, offset, td->rows, td->cols);
			if (rv >= 0)
			{
				rv = CalculateWindowExtents();

				handle_fakefont(g_g.g_mainhWnd
					, td
					, g_g.g_mw_fakeFontWidth
					, g_g.g_mw_fakeFontHeight
					, &g_g.g_maskedLetters
					, &g_g.g_maskedLetterColors
					, TRUE);
				
			}
			
			UpdateFontMenuChoices(g_g.g_hwndCB);

			InvalidateRect(g_g.g_mainhWnd, NULL, FALSE);
		}
		break;

/*	case IDM_REPEAT:
		{
			Term_keypress('n');
		}
		break;

	case IDM_REST:
		{
//Ought to flush commands first here. Is there a routine or should I just use escapes?
			Term_keypress('`');
			Term_keypress('`');
			Term_keypress('`');
			Term_keypress('R');
			Term_keypress('&');
			Term_keypress(10);
		}
		break;

	case IDM_ESCAPE:
		{
			Term_keypress('`');
		}
		break;
*/
	case IDM_OPTIONS_MACRO_BAR:
		{
			DWORD dwRet;
			HMENU hSetupMenu;

			/* Paranoia */
			if (!game_in_progress)
			{
				plog("You may not do that right now.");
				break;
			}

			if (!g_g.g_TBhWnd) g_g.g_TBhWnd = CreateMacroBar(g_g.g_mainhWnd);
			else
			{
				if (g_g.m_bMacroBarShown) ShowWindow(g_g.g_TBhWnd, SW_HIDE);
				else ShowWindow(g_g.g_TBhWnd, SW_SHOW);
				g_g.m_bMacroBarShown = !g_g.m_bMacroBarShown;
			}
			MoveWindow(g_g.g_mainhWnd, g_g.m_winX, g_g.m_winY, g_g.m_winW, g_g.m_winH, TRUE);

			hSetupMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
			
			dwRet = CheckMenuItem(hSetupMenu
				, IDM_OPTIONS_MACRO_BAR 
				, (g_g.m_bMacroBarShown == 0) 
				? (MF_UNCHECKED | MF_BYCOMMAND) 
				: (MF_CHECKED | MF_BYCOMMAND) );
		}
		break;

	case MACRO_BUTTON:
		{
			//g_g.m_bMacroButton = !g_g.m_bMacroButton;

        	TBBUTTONINFO tbbi;
         	TCHAR tbbbuf[MAX_MOUSE_LABEL + 9];
        	TCHAR wccommand[MAX_MOUSE_LABEL];
/*			TBBUTTONINFO tbbi;
			TCHAR tbbbuf[10];
			TCHAR wccommand[1]; */

			g_g.m_bMacroButton = !g_g.m_bMacroButton;

			if (!g_g.m_bMacroButton)
           
				for (i = 0; i < 18; i++)
				{
					mbstowcs(wccommand, mse_button[i].label, 1024);
					wsprintf(tbbbuf, L"%s", wccommand);

					memset(&tbbi,0,sizeof(tbbi));
					tbbi.cbSize = sizeof(tbbi);
					tbbi.dwMask = TBIF_TEXT;
					tbbi.pszText = tbbbuf;
					SendMessage(g_g.g_TBhWnd,TB_SETBUTTONINFO,MACRO_BUTTON + 1 + i,(LPARAM)&tbbi);
				}
			
/*				for (i = 0; i < 18; i++)
				{
					mbstowcs(wccommand, cecommand[i], 1024);
					wsprintf(tbbbuf, L"%s", wccommand);

					memset(&tbbi,0,sizeof(tbbi));
					tbbi.cbSize = sizeof(tbbi);
					tbbi.dwMask = TBIF_TEXT;
					tbbi.pszText = tbbbuf;
					SendMessage(g_g.g_TBhWnd,TB_SETBUTTONINFO,MACRO_BUTTON + 1 + i,(LPARAM)&tbbi);
				} */
			else
				for (i = 0; i < 18; i++)
				{
					wsprintf(tbbbuf, L"%d", i + 1);

					memset(&tbbi,0,sizeof(tbbi));
					tbbi.cbSize = sizeof(tbbi);
					tbbi.dwMask = TBIF_TEXT;
					tbbi.pszText = tbbbuf;
					SendMessage(g_g.g_TBhWnd,TB_SETBUTTONINFO,MACRO_BUTTON + 1 + i,(LPARAM)&tbbi);
				}
		}
		break;

	case MACRO_BUTTON + 1:
	case MACRO_BUTTON + 2:
	case MACRO_BUTTON + 3:
	case MACRO_BUTTON + 4:
	case MACRO_BUTTON + 5:
	case MACRO_BUTTON + 6:
	case MACRO_BUTTON + 7:
	case MACRO_BUTTON + 8:
	case MACRO_BUTTON + 9:
	case MACRO_BUTTON + 10:
	case MACRO_BUTTON + 11:
	case MACRO_BUTTON + 12:
	case MACRO_BUTTON + 13:
	case MACRO_BUTTON + 14:
	case MACRO_BUTTON + 15:
	case MACRO_BUTTON + 16:
	case MACRO_BUTTON + 17:
	case MACRO_BUTTON + 18:
		{
			short offset;

			offset = wCmd - MACRO_BUTTON;

			if (g_g.m_bMacroButton) Term_keypress(127 + offset);
			else Term_keypress(mse_button[offset-1].key);
		
		}
		break;

#endif

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
#ifdef _WIN32_WCE
                /* Create the toolbar */
                if (g_g.m_bMacroBarShown)
				{
					g_g.g_TBhWnd = CreateMacroBar(g_g.g_mainhWnd);
				    MoveWindow(g_g.g_mainhWnd, g_g.m_winX, g_g.m_winY, g_g.m_winW, g_g.m_winH, TRUE);
				}
#endif				
                                game_in_progress = TRUE;
                                Term_flush();
                                play_game(TRUE);
                                quit(NULL);
                        }
                        break;
                }

		/* Open game */
		case IDM_FILE_OPEN:
#ifdef _WIN32_WCE
		{
			TCHAR wcSaveFile[1024];
			TCHAR wcDirSave[1024];

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
				mbstowcs( wcSaveFile, savefile, 1024);
				//mbstowcs( wcDirSave, ANGBAND_DIR_SAVE, 1024);
				mbstowcs( wcDirSave, "angband\\lib\\save", 1024);

				memset(&ofn, 0, sizeof(ofn));
				ofn.lStructSize = sizeof(ofn);
				ofn.hwndOwner = data[0].w;
				ofn.lpstrFilter = _T("Save Files (*.oa)\0*.oa\0");
				ofn.nFilterIndex = 1;
				ofn.lpstrFile = wcSaveFile;
				ofn.nMaxFile = 1024;
				ofn.lpstrInitialDir = wcDirSave;
				ofn.Flags = OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;

				if (GetOpenFileName(&ofn))
				{
                    /* Create the toolbar */
                    if (g_g.m_bMacroBarShown)
					{
				    	g_g.g_TBhWnd = CreateMacroBar(g_g.g_mainhWnd);
				        MoveWindow(g_g.g_mainhWnd, g_g.m_winX, g_g.m_winY, g_g.m_winW, g_g.m_winH, TRUE);
					}

					/* Load 'savefile' */
					wcstombs(savefile, wcSaveFile, 1024);
					validate_file(savefile);
					game_in_progress = TRUE;
					Term_flush();

					play_game(FALSE);
					quit(NULL);
				}
			}
			break;
		}
#else
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
#endif

                /* Save game */
                case IDM_FILE_SAVE:
                {
                        if (game_in_progress && character_generated)
                        {
                                /* Paranoia */
                                if (!inkey_flag)
                                {
                                        plog("You may not do that right now.");
                                        break;
                                }

                                /* Hack -- Forget messages */
                                msg_flag = FALSE;

				/* Save the game */
				do_cmd_save_game();
                        }
                        else
                        {
                                plog("You may not do that right now.");
                        }
                        break;
                }

                /* Show scores */
                case IDM_FILE_SCORE:
                {
                        char buf[1024];

                        /* Build the filename */
                        path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

                        /* Open the binary high score file, for reading */
                        highscore_fd = fd_open(buf, O_RDONLY);

			/* Paranoia -- No score file */
#ifdef _WIN32_WCE
			if (highscore_fd == INVALID_HANDLE_VALUE)
#else
			if (highscore_fd < 0)
#endif
			{
				msg_print("Score file unavailable.");
			}
			else
			{
				/* Save Screen */
				screen_save();

                                /* Clear screen */
                                Term_clear();

                                /* Display the scores */
                                if (game_in_progress && character_generated)
                                        predict_score();
                                else
                                        display_scores_aux(0, MAX_HISCORES, -1, NULL);

                                /* Shut the high score file */
                                (void)fd_close(highscore_fd);

                                /* Forget the high score fd */
                                highscore_fd = -1;

                                /* Load screen */
                                screen_load();

                                /* Hack - Flush it */
                                Term_fresh();
                        }

                        break;
                }

                /* Exit */
                case IDM_FILE_EXIT:
                {
			if (game_in_progress && character_generated)
			{
				/* Paranoia */
				if (!inkey_flag)
				{
					plog("You may not do that right now.");
					break;
				}

				/* Hack -- Forget messages */
				msg_flag = FALSE;

				/* Save the game */
				do_cmd_save_game();
			}
			quit(NULL);
			break;
		}
#ifdef _WIN32_WCE
#else
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
                  if ((use_graphics_nice) && (!inkey_flag || !initialized))
                    {
                      plog("You may not do that right now.");
                      break;
                    }
                  
                  i = wCmd - IDM_WINDOW_FONT_0;
                  
                  if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			td = &data[i];
                  
                  term_change_font(td);
                  
                  if (use_graphics_nice)
                    {
                      /* Hack -- Force redraw */
                      Term_key_push(KTRL('R'));
                    }

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
#endif
                case IDM_OPTIONS_NO_GRAPHICS:
                {
                        /* Paranoia */
                        if (!inkey_flag)
                        {
                                plog("You may not do that right now.");
                                break;
			}

			/* Toggle "arg_graphics" */
			if (arg_graphics != GRAPHICS_NONE)
			{
#ifdef _WIN32_WCE
				{
					DWORD dwRet;
					HMENU hOptionsMenu;
					
					hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
					
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_NO_GRAPHICS 
						, (MF_CHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_OLD_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_NEW_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_GRAPHICS_DAVID 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
				}
#endif /* _WIN32_WCE */
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
                        if (!inkey_flag)
                        {
                                plog("You may not do that right now.");
                                break;
			}

			/* Toggle "arg_graphics" */
			if (arg_graphics != GRAPHICS_ORIGINAL)
			{
#ifdef _WIN32_WCE
				{
					DWORD dwRet;
					HMENU hOptionsMenu;
					
					hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
					
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_NO_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_OLD_GRAPHICS 
						, (MF_CHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_NEW_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_GRAPHICS_DAVID 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
				}
#endif /* _WIN32_WCE */
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
                        if (!inkey_flag)
                        {
                                plog("You may not do that right now.");
                                break;
			}

			/* Toggle "arg_graphics" */
			if (arg_graphics != GRAPHICS_ADAM_BOLT)
			{
#ifdef _WIN32_WCE
				{
					DWORD dwRet;
					HMENU hOptionsMenu;
					
					hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
					
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_NO_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_OLD_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_NEW_GRAPHICS 
						, (MF_CHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_GRAPHICS_DAVID 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
				}
#endif /* _WIN32_WCE */
				arg_graphics = GRAPHICS_ADAM_BOLT;

				/* React to changes */
				Term_xtra_win_react();

				/* Hack -- Force redraw */
				Term_key_push(KTRL('R'));
			}

			break;
		}

		case IDM_OPTIONS_GRAPHICS_DAVID:
		{
			/* Paranoia */
			if (!inkey_flag || !initialized)
			{
				plog("You may not do that right now.");
				break;
			}

			/* Toggle "arg_graphics" */
			if (arg_graphics != GRAPHICS_DAVID_GERVAIS)
			{
#ifdef _WIN32_WCE
				{
					DWORD dwRet;
					HMENU hOptionsMenu;
					
					hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
					
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_NO_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_OLD_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_NEW_GRAPHICS 
						, (MF_UNCHECKED | MF_BYCOMMAND) );
					dwRet = CheckMenuItem(hOptionsMenu
						, IDM_OPTIONS_GRAPHICS_DAVID 
						, (MF_CHECKED | MF_BYCOMMAND) );
				}
#endif /* _WIN32_WCE */
				arg_graphics = GRAPHICS_DAVID_GERVAIS;

				/* React to changes */
				Term_xtra_win_react();

				/* Hack -- Force redraw */
				Term_key_push(KTRL('R'));
			}

			break;
		}
#ifdef _WIN32_WCE
#else
		case IDM_OPTIONS_GRAPHICS_NICE:
		{
			/* Paranoia */
			if (!inkey_flag || !initialized)
			{
				plog("You may not do that right now.");
                                break;
                        }

                        /* Toggle "arg_graphics_nice" */
                        arg_graphics_nice = !arg_graphics_nice;

                        /* React to changes */
                        Term_xtra_win_react();

			/* Hack -- Force redraw */
			Term_key_push(KTRL('R'));

			break;
		}
#endif
		case IDM_OPTIONS_TRPTILE:
		{
			/* Paranoia */
			if (!inkey_flag || !initialized)
			{
				plog("You may not do that right now.");
				break;
			}

			/* Toggle "use_trptile" */
			use_trptile = !use_trptile;

			/* Cancel "use_dbltile" */
			use_dbltile = FALSE;
#ifdef _WIN32_WCE
			{
				DWORD dwRet;
				HMENU hOptionsMenu;
				
				hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
				
				dwRet = CheckMenuItem(hOptionsMenu
					, IDM_OPTIONS_TRPTILE 
					, (use_trptile)
					? (MF_CHECKED | MF_BYCOMMAND) 
					: (MF_UNCHECKED | MF_BYCOMMAND) );
				dwRet = CheckMenuItem(hOptionsMenu
					, IDM_OPTIONS_DBLTILE 
					, (MF_UNCHECKED | MF_BYCOMMAND) );
			}
#endif
			/* Set flag */
			change_tilesize = TRUE;

			/* React to changes */
			Term_xtra_win_react();

			/* Hack -- Force redraw */
			Term_key_push(KTRL('R'));

			break;
		}

		case IDM_OPTIONS_DBLTILE:
		{
			/* Paranoia */
			if (!inkey_flag || !initialized)
			{
				plog("You may not do that right now.");
				break;
			}

			/* Toggle "use_dbltile" */
			use_dbltile = !use_dbltile;

			/* Cancel "use_trptile" */
			use_trptile = FALSE;
#ifdef _WIN32_WCE
			{
				DWORD dwRet;
				HMENU hOptionsMenu;
				
				hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
				
				dwRet = CheckMenuItem(hOptionsMenu
					, IDM_OPTIONS_DBLTILE 
					, (use_dbltile)
					? (MF_CHECKED | MF_BYCOMMAND) 
					: (MF_UNCHECKED | MF_BYCOMMAND) );
				dwRet = CheckMenuItem(hOptionsMenu
					, IDM_OPTIONS_TRPTILE 
					, (MF_UNCHECKED | MF_BYCOMMAND) );
			}
#endif
			/* Set flag */
			change_tilesize = TRUE;

			/* React to changes */
			Term_xtra_win_react();

			/* Hack -- Force redraw */
			Term_key_push(KTRL('R'));

			break;
		}

		case IDM_OPTIONS_BIGTILE:
		{
			/* Paranoia */
			if (!inkey_flag || !initialized)
			{
				plog("You may not do that right now.");
				break;
			}

			/* Toggle "use_bigtile" */
			use_bigtile = !use_bigtile;
#ifdef _WIN32_WCE
			{
				DWORD dwRet;
				HMENU hOptionsMenu;
				
				hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
				
				dwRet = CheckMenuItem(hOptionsMenu
					, IDM_OPTIONS_BIGTILE 
					, (use_bigtile)
					? (MF_CHECKED | MF_BYCOMMAND) 
					: (MF_UNCHECKED | MF_BYCOMMAND) );
			}
#endif
			/* Set flag */
			change_tilesize = TRUE;

			/* React to changes */
			Term_xtra_win_react();

			/* Hack -- Force redraw */
			Term_key_push(KTRL('R'));

			break;
		}

#ifdef _WIN32_WCE
#else
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

#ifdef _WIN32_WCE
			{
				DWORD dwRet;
				HMENU hOptionsMenu;
				
				hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
				
				dwRet = CheckMenuItem(hOptionsMenu
					, IDM_OPTIONS_SOUND 
					, (arg_sound)
					? (MF_CHECKED | MF_BYCOMMAND) 
					: (MF_UNCHECKED | MF_BYCOMMAND) );
			}
#endif

			/* React to changes */
			Term_xtra_win_react();

			/* Hack -- Force redraw */
			Term_key_push(KTRL('R'));

			break;
		}
#endif

		case IDM_OPTIONS_SMALL_SCREEN:
		{
			/* Toggle "arg_sound" */
			small_screen = !small_screen;
#ifdef _WIN32_WCE
			{
				DWORD dwRet;
				HMENU hOptionsMenu;
				
				hOptionsMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
				
				dwRet = CheckMenuItem(hOptionsMenu
					, IDM_OPTIONS_SMALL_SCREEN 
					, (small_screen)
					? (MF_CHECKED | MF_BYCOMMAND) 
					: (MF_UNCHECKED | MF_BYCOMMAND) );
			}
#endif
			/* React to changes */
			Term_xtra_win_react();

			/* Hack -- Force redraw */
			if (initialized) Term_key_push(KTRL('R'));

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

#endif /* USE_SAVER */

#ifdef _WIN32_WCE
#else
		case IDM_OPTIONS_MAP:
		{
			windows_map();
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
#endif
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

	if (td->grid_display == 3)
	{
#ifdef _WIN32_WCE
#else
		/* Redraw the map */
		/* ToDo: Only redraw the necessary parts */
		windows_map_aux();
#endif
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

#ifdef _WIN32_WCE
void handle_fakefont(HWND hWnd, term_data *td, int width, int height, struct CONTROLBITMAPS* pMask, struct CONTROLBITMAPS* pColor, BOOL bMove)
{
	char buf[1024];
	char fakeFontName[100];
	
	/* Main window */
	if (g_g.m_fakeFont16ColorHack)
	{
		sprintf(fakeFontName, "FONT_%dX%d_16.BMP", width, height);
	}
	else
	{
		sprintf(fakeFontName, "FONT_%dX%d.BMP", width, height);
	}

	/* Access the bitmap file */
	path_build(buf, 1024, ANGBAND_DIR_XTRA_GRAF, fakeFontName);
	
	/* This should probably get changed to where I do my font loading */
	/* Load the bitmap or quit */
	if (!ReadDIB(hWnd, buf, &pMask->mImage))
	{
		plog_fmt("Cannot read bitmap file '%s'", fakeFontName);
		return;
	}
	if (!g_g.m_fakeFont16ColorHack)
	{
		sprintf(fakeFontName, "COLORS_%dX%d.BMP", width, height);
		
		/* Access the bitmap file */
		path_build(buf, 1024, ANGBAND_DIR_XTRA_GRAF, fakeFontName);
		
		/* This should probably get changed to where I do my font loading */
		/* Load the bitmap or quit */
		if (!ReadDIB(hWnd, buf, &pColor->mImage))
		{
			plog_fmt("Cannot read bitmap file '%s'", fakeFontName);
			return;
		}
	}
	
	g_g.g_useWinCEFakeFont = 1;
	
	td->font_wid = width; 
	td->font_hgt = height; 
	td->tile_wid = width;
	td->tile_hgt = height;

	g_g.g_xClientMax = td->font_wid * td->cols; 
	g_g.g_yClientMax = td->font_hgt * td->rows;

	if (bMove)
	{
		RECT r;
		int winWidth;
		int winHeight;

		GetWindowRect(hWnd, &r);

		winWidth = g_g.g_xClientMax;
		winHeight = g_g.g_yClientMax;

		if (winWidth > g_g.m_winW)
		{
			winWidth = g_g.m_winW;
		}
		if (winHeight > g_g.m_winH)
		{
			winHeight = g_g.m_winH;
		}

		/* Always have the main window fill out the screen. */
		if (winWidth < g_g.m_winW)
		{
			winWidth = g_g.m_winW;
		}
		if (winHeight < g_g.m_winH)
		{
			winHeight = g_g.m_winH;
		}

		MoveWindow(hWnd, r.left, r.top, winWidth, winHeight, FALSE);
	}
}

void HandleWinMainCreate(HWND hWnd)
{
	
	HMENU hMenu;

	SetWindowLong(hWnd, 0, (LONG)(my_td));
	
	g_g.g_hwndCB = CreateRpCommandBar(hWnd);
	
	hMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);
	
	SetDefaultAngbandCEMenuItems(g_g.g_hwndCB);
	
	hMenu = SHGetSubMenu(g_g.g_hwndCB, ID_OPTIONS);

	handle_fakefont(data[0].w, my_td, g_g.g_mw_fakeFontWidth, g_g.g_mw_fakeFontHeight, &g_g.g_maskedLetters, &g_g.g_maskedLetterColors, FALSE);
	
	g_g.g_xClientMax = my_td->font_wid * my_td->cols; 
	g_g.g_yClientMax = my_td->font_hgt * my_td->rows;
	
	g_g.m_gxDll = (unsigned long) LoadLibrary(_T("gx.dll")); 
	
	if (g_g.m_gxDll)
	{
		pFuncPlgn_GXStuff pInput;
		pGXGetDefaultKeys pDefKeys;

		pInput = (pFuncPlgn_GXStuff)GetProcAddress(g_g.m_gxDll, _T("GXOpenInput"));

		pInput = (FARPROC)GetProcAddress(g_g.m_gxDll, _T("?GXOpenInput@@YAHXZ"));

		if (pInput)
		{
			// Initialize the hardware buttons
			pInput();
			
			//Get info about the hardware keys and fill in the g_gxkl struct
			
			g_g.m_bHardwareButtonsOpen = TRUE;
		}

		pDefKeys = (pGXGetDefaultKeys)GetProcAddress(g_g.m_gxDll, _T("?GXGetDefaultKeys@@YA?AUGXKeyList@@H@Z")); 

   		//g_g.g_gxkl = pDefKeys(2);
	}
}

#endif


#ifdef __MWERKS__
LRESULT FAR PASCAL AngbandWndProc(HWND hWnd, UINT uMsg,
                                  WPARAM wParam, LPARAM lParam);
LRESULT FAR PASCAL AngbandWndProc(HWND hWnd, UINT uMsg,
                                  WPARAM wParam, LPARAM lParam)
#else /* __MWERKS__ */
LRESULT FAR PASCAL AngbandWndProc(HWND hWnd, UINT uMsg,
                                          WPARAM wParam, LPARAM lParam)
#endif /* __MWERKS__ */
{
        HDC hdc;
        term_data *td;
        int i;


        int xPos, yPos, button;

        /* Acquire proper "term_data" info */
        td = (term_data *)GetWindowLong(hWnd, 0);

	/* Handle message */
	switch (uMsg)
	{
		/* XXX XXX XXX */
#ifdef _WIN32_WCE
#else
		case WM_NCCREATE:
		{
			SetWindowLong(hWnd, 0, (LONG)(my_td));
			break;
		}

		/* XXX XXX XXX */
#endif
		case WM_CREATE:
		{
#ifdef _WIN32_WCE
			HandleWinMainCreate(hWnd);
#endif
			return 0;
		}
#ifdef _WIN32_WCE
#else
		case WM_GETMINMAXINFO:
		{
			MINMAXINFO FAR *lpmmi;
			RECT rc;

			lpmmi = (MINMAXINFO FAR *)lParam;

                        /* this message was sent before WM_NCCREATE */
                        if (!td) return 1;

                        /* Minimum window size is 80x24 (or 48x24 small screen) */
                        rc.left = rc.top = 0;
                        rc.right = rc.left + (small_screen ? 48 : 80) * td->tile_wid + td->size_ow1 + td->size_ow2;
                        rc.bottom = rc.top + 24 * td->tile_hgt + td->size_oh1 + td->size_oh2 + 1;

                        /* Adjust */
			AdjustWindowRectEx(&rc, td->dwStyle, TRUE, td->dwExStyle);

			/* Save minimum size */
			lpmmi->ptMinTrackSize.x = rc.right - rc.left;
			lpmmi->ptMinTrackSize.y = rc.bottom - rc.top;

			return 0;
		}
#endif

		case WM_PAINT:
		{
			handle_wm_paint(hWnd);

			return 0;
		}
#ifdef _WIN32_WCE
		case WM_KEYUP:
			{
				/* These Should be the hardware button from
				the pda's */
				short vkKey = (short)wParam;
				
				switch(vkKey)
				{
				case 38: /* g_g.g_gxkl.vkUp: /* 38 up */
				case 40: /* g_g.g_gxkl.vkDown: /* 40 down */
				case 37: /* g_g.g_gxkl.vkLeft: /* 37 left */
				case 39: /* g_g.g_gxkl.vkRight: /* 39 right */
					{
						return 0;
					}
				case 204: /* iPAQ */
				case 205: /* iPAQ */
				case 198: /* iPAQ */
				case 196: /* A */
				case 197: /* B */
				case 195: /* C */
				case 194: /* Start */
				case 192: /* Aux1 */
				case 193: /* Aux2 */
					{
						int rv;
						
						rv = ButtonUp(vkKey);
						if (rv >= 0)
						{
							return 0;
						}
					}
					break;
				}
			}
			break;
#endif

		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
#ifdef _WIN32_WCE
			{
				/* These Should be the hardware button from
				the pda's */
				short vkKey = (short)wParam;

				switch(vkKey)
				{
				case 38: /* g_g.g_gxkl.vkUp: /* 38 up */
					{
						Term_keypress(56);
                      //  plog_fmt("Key:%d", g_g.g_gxkl.vkUp);
					  //  plog_fmt("Key:%d", g_g.g_gxkl.vkA);
					//	  plog_fmt("Key:%d", g_g.g_gxkl.vkB);
					//	    plog_fmt("Key:%d", g_g.g_gxkl.vkC);
					//		  plog_fmt("Key:%d", g_g.g_gxkl.vkStart);
						return 0;
					}
				case 40: /*g_g.g_gxkl.vkDown: /* 40 down */
					{
						Term_keypress(50);
						return 0;
					}
				case 37: /*g_g.g_gxkl.vkLeft: /* 37 left */
					{
						Term_keypress(52);
						return 0;
					}
				case 39: /*g_g.g_gxkl.vkRight: /* 39 right */
					{
						Term_keypress(54);
						return 0;
					}
				case 204: /* iPAQ */
				case 205: /* iPAQ */
				case 198: /* iPAQ */
				case 196: /* A */
				case 197: /* B */
				case 195: /* C */
				case 194: /* Start */
				case 192: /* Aux1 */
				case 193: /* Aux2 */
					{
						int rv;
						
						rv = ButtonPress(vkKey);
						if (rv >= 0)
						{
							return 0;
						}
					}
					break;
				}
				
			}
#endif
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

		case WM_MBUTTONDOWN:
                case WM_RBUTTONDOWN:
                case WM_LBUTTONDOWN:
                {
                        if ((term_initialised) && (td->tile_wid) && (td->tile_hgt))
                        {
                                /* Get the text grid */
                                xPos = GET_X_LPARAM(lParam);
                                yPos = GET_Y_LPARAM(lParam);
                                xPos /= td->tile_wid;
                                yPos /= td->tile_hgt;

                                /* XXX TODO Translate iso-coords back to normal if required */

                                if (uMsg == WM_LBUTTONDOWN)
                                        button = 1;
                                else if (uMsg == WM_RBUTTONDOWN)
                                        button = 2;
                                else
                                        button = 3;
                                Term_mousepress(xPos,yPos,button);
                        }

                        break;
                }

                /* Take this out for now 
                case WM_MOUSEMOVE:
                {

                        if ((term_initialised) && (td->tile_wid) && (td->tile_hgt))
                        {
                                 Get the text grid 
                                xPos = GET_X_LPARAM(lParam);
                                yPos = GET_Y_LPARAM(lParam);
                                xPos /= td->tile_wid;
                                yPos /= td->tile_hgt;

                                 Have we changed grid? 
                                if ((xPos != xOldPos) ||
                                        (xPos != yOldPos))
                                {
                                        Term_mousepress(xPos,yPos,0);
                                }

                                 Save last location 
                                xOldPos = xPos;
                                yOldPos = yPos;
                        }
                }
                */

#ifdef _WIN32_WCE
#else
		case WM_INITMENU:
		{
			setup_menus();
			return 0;
		}
#endif

		case WM_CLOSE:
		{
#ifdef _WIN32_WCE
			if (g_g.m_gxDll)
			{
				if (g_g.m_bHardwareButtonsOpen)
				{
					pFuncPlgn_GXStuff pCloseInput;

					g_g.m_bHardwareButtonsOpen = FALSE;
					
					pCloseInput = (pFuncPlgn_GXStuff)GetProcAddress(g_g.m_gxDll, _T("GXCloseInput"));
					
					pCloseInput =(pFuncPlgn_GXStuff)GetProcAddress(g_g.m_gxDll, _T("?GXCloseInput@@YAHXZ"));

					if (pCloseInput)
					{
						// Initialize the hardware buttons
						pCloseInput();
						
						//Get info about the hardware keys and fill in the g_gxkl struct
						g_g.m_bHardwareButtonsOpen = FALSE;
					}
				}

				FreeLibrary(g_g.m_gxDll);
				g_g.m_gxDll = 0;
			}
#endif

			if (game_in_progress && character_generated)
			{
				if (!inkey_flag)
				{
					plog("You may not do that right now.");
					return 0;
				}

				/* Hack -- Forget messages */
				msg_flag = FALSE;

				/* Save the game */
				do_cmd_save_game();
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
#ifdef _WIN32_WCE
#else
					/* Hide sub-windows */
					for (i = 1; i < MAX_TERM_DATA; i++)
					{
						if (data[i].visible) ShowWindow(data[i].w, SW_HIDE);
					}
					return 0;
#endif
				}

				case SIZE_MAXIMIZED:
				{
					/* fall through XXX XXX XXX */
				}

				case SIZE_RESTORED:
				{
					uint cols = (LOWORD(lParam) - td->size_ow1) / td->tile_wid;
#ifdef _WIN32_WCE
					uint rows = (HIWORD(lParam) - td->size_oh1 - ((g_g.m_bMacroBarShown) ? g_g.m_MacroBarHeight : 0)) / td->tile_hgt;
#else
					uint rows = (HIWORD(lParam) - td->size_oh1) / td->tile_hgt;
#endif
					
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

#ifdef _WIN32_WCE
#else
					td->size_hack = TRUE;

					/* Show sub-windows */
					for (i = 1; i < MAX_TERM_DATA; i++)
					{
						if (data[i].visible) ShowWindow(data[i].w, SW_SHOW);
					}

					td->size_hack = FALSE;
#endif

					return 0;
				}
			}
			break;
		}

#ifdef _WIN32_WCE
		case WM_TIMER:
			{
				unsigned int wTimerID;

				wTimerID = wParam;

				switch(wTimerID)
				{
				case MAIN_KEY_DOWN_TIMER:
					HandleKeyDownTimer();
					break;

				default:
					KillTimer(hWnd, wTimerID);
					break;
				}
			}
			break;
#endif

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
#ifdef _WIN32_WCE
#else
				/* Do something to sub-windows */
				for (i = 1; i < MAX_TERM_DATA; i++)
				{
					SetWindowPos(data[i].w, hWnd, 0, 0, 0, 0,
					             SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE);
				}
#endif

				/* Focus on main window */
				SetFocus(hWnd);

				return 0;
			}

			break;
		}
	}

	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

#ifdef _WIN32_WCE
#else
#ifdef __MWERKS__
LRESULT FAR PASCAL AngbandListProc(HWND hWnd, UINT uMsg,
                                           WPARAM wParam, LPARAM lParam);
LRESULT FAR PASCAL AngbandListProc(HWND hWnd, UINT uMsg,
                                           WPARAM wParam, LPARAM lParam)
#else /* __MWERKS__ */
LRESULT FAR PASCAL AngbandListProc(HWND hWnd, UINT uMsg,
                                           WPARAM wParam, LPARAM lParam)
#endif /* __MWERKS__ */
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
#if 0
			MINMAXINFO FAR *lpmmi;
			RECT rc;

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
#endif /* 0 */
			return 0;
		}

                case WM_SIZE:
                {
                        uint cols;
                        uint rows;
                        
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
                                p_ptr->window = 0xFFFFFFFF;
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
#endif /* _WIN32_WCE */


#ifdef USE_SAVER

#define MOUSE_SENS 40

#endif /* USE_SAVER */

#ifdef _WIN32_WCE
#else

#ifdef __MWERKS__
LRESULT FAR PASCAL AngbandSaverProc(HWND hWnd, UINT uMsg,
                                    WPARAM wParam, LPARAM lParam);
LRESULT FAR PASCAL AngbandSaverProc(HWND hWnd, UINT uMsg,
                                    WPARAM wParam, LPARAM lParam)
#else /* __MWERKS__ */
LRESULT FAR PASCAL AngbandSaverProc(HWND hWnd, UINT uMsg,
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
#endif /* 0 */

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

#endif



/*** Temporary Hooks ***/


/*
 * Display warning message (see "z-util.c")
 */
static void hack_plog(cptr str)
{
	/* Give a warning */
	if (str)
	{
#ifdef _WIN32_WCE
		TCHAR wcStr[1024];
		mbstowcs( wcStr, str, 1024);

		MessageBox(NULL, wcStr, _T("Warning"),
		           MB_ICONEXCLAMATION | MB_OK);
#else
		MessageBox(NULL, str, "Warning",
		           MB_ICONEXCLAMATION | MB_OK);
#endif
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
#ifdef _WIN32_WCE
		TCHAR wcStr[1024];
		mbstowcs( wcStr, str, 1024);

		MessageBox(NULL, wcStr, _T("Error"),
		           MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);
#else
		MessageBox(NULL, str, "Error",
		           MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);
#endif
	}

	/* Unregister the classes */
#ifdef _WIN32_WCE
	{
		TCHAR wcAppName[1024];
		mbstowcs( wcAppName, AppName, 1024);

		UnregisterClass(wcAppName, hInstance);
	}
#else
	UnregisterClass(AppName, hInstance);
#endif

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
#ifdef _WIN32_WCE
		TCHAR wcStr[1024];
		mbstowcs( wcStr, str, 1024);

		MessageBox(data[0].w, wcStr, _T("Warning"),
		           MB_ICONEXCLAMATION | MB_OK);
#else
		MessageBox(data[0].w, str, "Warning",
		           MB_ICONEXCLAMATION | MB_OK);
#endif
	}
}


/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(cptr str)
{
        int i, j;

	/* Give a warning */
	if (str)
	{
#ifdef _WIN32_WCE
		TCHAR wcStr[1024];
		mbstowcs( wcStr, str, 1024);

		MessageBox(data[0].w, wcStr, _T("Error"),
		           MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);
#else
		MessageBox(data[0].w, str, "Error",
		           MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);
#endif
	}


        /* Save the preferences */
        save_prefs();


        /*** Could use 'Term_nuke_win()' XXX XXX XXX */

        /* Destroy all windows */
        for (i = MAX_TERM_DATA - 1; i >= 0; --i)
        {
		term_force_font(&data[i], NULL);
                if (data[i].font_want) string_free(data[i].font_want);

                /* Kill the window */
                if (data[i].w) DestroyWindow(data[i].w);
                data[i].w = 0;
                term_nuke(&data[i].t);
        }

        /* Free the bitmap stuff */
#ifdef USE_GRAPHICS
        if (infGraph.hPalette) DeleteObject(infGraph.hPalette);
        if (infGraph.hBitmap) DeleteObject(infGraph.hBitmap);

        if (infMask.hPalette) DeleteObject(infMask.hPalette);
        if (infMask.hBitmap) DeleteObject(infMask.hBitmap);

#endif /* USE_GRAPHICS */

#ifdef USE_SOUND
	/* Free the sound names */
	for (i = 0; i < SOUND_MAX; i++)
	{
		for (j = 0; j < SAMPLE_MAX; j++)
		{
			if (!sound_file[i][j]) break;

			string_free(sound_file[i][j]);
		}
	}
#endif /* USE_SOUND */

	/*** Free some other stuff ***/

#ifdef _WIN32_WCE
	DeleteObject(hPenYellow);
#else
	DeleteObject(hbrYellow);
#endif

	if (hPal) DeleteObject(hPal);

#ifdef _WIN32_WCE
	{
		TCHAR wcAppName[1024];
		mbstowcs( wcAppName, AppName, 1024);

		// Should I be doing this here or later????
		if (g_g.g_maskedLetters.mImage.hPalette) 
		{
			DeleteObject(g_g.g_maskedLetters.mImage.hPalette);
		}
		if (g_g.g_maskedLetters.mImage.hBitmap) 
		{
			DeleteObject(g_g.g_maskedLetters.mImage.hBitmap);
		}
		if (g_g.g_maskedLetterColors.mImage.hPalette) 
		{
			DeleteObject(g_g.g_maskedLetterColors.mImage.hPalette);
		}
		if (g_g.g_maskedLetterColors.mImage.hBitmap) 
		{
			DeleteObject(g_g.g_maskedLetterColors.mImage.hBitmap);
		}

		UnregisterClass(wcAppName, hInstance);
	}
#else
	UnregisterClass(AppName, hInstance);
#endif

	if (hIcon) DestroyIcon(hIcon);

	/* Free strings */
	string_free(ini_file);
	//string_free(argv0);
	string_free(ANGBAND_DIR_XTRA_FONT);
	string_free(ANGBAND_DIR_XTRA_GRAF);
	string_free(ANGBAND_DIR_XTRA_SOUND);
	string_free(ANGBAND_DIR_XTRA_HELP);

#ifdef HAS_CLEANUP
	cleanup_angband();
#endif /* HAS_CLEANUP */

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
#ifdef _WIN32_WCE
	{
		TCHAR wcPath[1024];
		GetModuleFileName(hInstance, wcPath, 1024);
		wcstombs(path, wcPath, 1024);
	}
#else
	GetModuleFileName(hInstance, path, 512);
#endif

        /* Save the "program name" XXX XXX XXX */
        argv0 = path;

        /* Get the name of the "*.ini" file */
        strcpy(path + strlen(path) - 4, ".INI");

        /* Save the the name of the ini-file */
        ini_file = string_make(path);

	/* Analyze the path */
	i = strlen(path);

	/* Get the path */
	for (; i > 0; i--)
	{
		if (path[i] == '\\')
		{
			/* End of path */
			break;
		}
	}

	/* Add "lib" to the path */
	strcpy(path + i + 1, "lib\\");

	/* Validate the path */
	validate_dir(path);

	/* Init the file paths */
	init_file_paths(path);

	/* Hack -- Validate the paths */
	validate_dir(ANGBAND_DIR_APEX);
        validate_dir(ANGBAND_DIR_BONE);
        validate_dir(ANGBAND_DIR_DATA);
        validate_dir(ANGBAND_DIR_EDIT);

#ifdef USE_SCRIPT
        validate_dir(ANGBAND_DIR_SCRIPT);
#endif /* USE_SCRIPT */

        validate_dir(ANGBAND_DIR_FILE);
        validate_dir(ANGBAND_DIR_HELP);
        validate_dir(ANGBAND_DIR_INFO);
	validate_dir(ANGBAND_DIR_PREF);
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

#ifdef _WIN32_WCE
// We do not use the basic font at present
#else
	/* Build the filename */
	path_build(path, 1024, ANGBAND_DIR_XTRA_FONT, "8X13.FON");

	/* Hack -- Validate the basic font */
	validate_file(path);
#endif

#ifdef USE_GRAPHICS

        /* Build the "graf" path */
        path_build(path, 1024, ANGBAND_DIR_XTRA, "graf");

        /* Allocate the path */
        ANGBAND_DIR_XTRA_GRAF = string_make(path);

	/* Validate the "graf" directory */
	validate_dir(ANGBAND_DIR_XTRA_GRAF);

#ifdef _WIN32_WCE
	strcat(path, "\\");
	ScanForFakeFontFiles(g_g.g_mainhWnd, path);
#endif /* _WIN32_WCE */

#endif /* USE_GRAPHICS */


#ifdef USE_SOUND

        /* Build the "sound" path */
        path_build(path, 1024, ANGBAND_DIR_XTRA, "sound");

        /* Allocate the path */
        ANGBAND_DIR_XTRA_SOUND = string_make(path);

	/* Validate the "sound" directory */
	validate_dir(ANGBAND_DIR_XTRA_SOUND);

#endif /* USE_SOUND */

#ifdef USE_MUSIC

        /* Build the "music" path */
        path_build(path, 1024, ANGBAND_DIR_XTRA, "music");

        /* Allocate the path */
        ANGBAND_DIR_XTRA_MUSIC = string_make(path);

        /* Validate the "music" directory */
        validate_dir(ANGBAND_DIR_XTRA_MUSIC);

#endif /* USE_MUSIC */

        /* Build the "help" path */
        path_build(path, 1024, ANGBAND_DIR_XTRA, "help");

        /* Allocate the path */
        ANGBAND_DIR_XTRA_HELP = string_make(path);

        /* Validate the "help" directory */
        /* validate_dir(ANGBAND_DIR_XTRA_HELP); */
}

#ifdef _WIN32_WCE
//
//  FUNCTION: MyRegisterClass()
//
//  PURPOSE: Registers the window class.
//
//  COMMENTS:
//
//    It is important to call this function so that the application 
//    will get 'well formed' small icons associated with it.
//
ATOM MyRegisterClass(HINSTANCE hInstance, LPTSTR szWindowClass)
{
	WNDCLASS	wc;

    wc.style			= CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc		= (WNDPROC) AngbandWndProc;
    wc.cbClsExtra		= 0;
    wc.cbWndExtra		= 0;
    wc.hInstance		= hInstance;
    wc.hIcon			= LoadIcon(hInstance, _T("ANGBAND"));
    wc.hCursor			= 0;
    wc.hbrBackground	= (HBRUSH) GetStockObject(WHITE_BRUSH);
    wc.lpszMenuName		= 0;
    wc.lpszClassName	= szWindowClass;

	return RegisterClass(&wc);
}

//
//  FUNCTION: InitInstance(HANDLE, int)
//
//  PURPOSE: Saves instance handle and creates main window
//
//  COMMENTS:
//
//    In this function, we save the instance handle in a global variable and
//    create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInst, int nCmdShow)
{
	HWND	hWnd = NULL;
	TCHAR	szTitle[1024];			// The title bar text
	TCHAR	szWindowClass[1024];		// The window class name
	
	hInstance = hInst;		// Store instance handle in our global variable
	// Initialize global strings
	wsprintf(szWindowClass, L"%s", L"OANGBAND");
	wsprintf(szTitle, L"%s", L"oangband");

	//If it is already running, then focus on the window
	hWnd = FindWindow(szWindowClass, szTitle);	
	if (hWnd) 
	{
		SetForegroundWindow ((HWND) (((DWORD)hWnd) | 0x01));    
		return 0;
	} 

	{
		WNDCLASS	wc;

		TCHAR wcAngList[1024];
		TCHAR wcAppName[1024];

		mbstowcs( wcAppName, AppName, 1024);
		mbstowcs( wcAngList, AngList, 1024);

		wc.style		= CS_HREDRAW | CS_VREDRAW;
		wc.lpfnWndProc	= AngbandWndProc;
		wc.cbClsExtra	= 0;
		wc.cbWndExtra	= 4; /* one long pointer to term_data */
		wc.hInstance	= hInstance;
		wc.hIcon         = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_OANGBAND));
		wc.hCursor       = 0;
		wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
		wc.lpszMenuName  = 0;
		wc.lpszClassName = wcAppName;

		if (!RegisterClass(&wc))
		{
			//exit(1);
			return FALSE;
		}

	}
	
	return TRUE;
}

#endif
#ifdef _WIN32_WCE
int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst,
                       LPTSTR lpwCmdLine, int nCmdShow)
#else
int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst,
                       LPSTR lpCmdLine, int nCmdShow)
#endif
{
	int i;

#ifdef _WIN32_WCE
#else
	WNDCLASS wc;
#endif
	HDC hdc;
	MSG msg;

#ifdef _WIN32_WCE
	char lpCmdLine[1024];

	InitWinCEGlobalData(&g_g);	

	wcstombs(lpCmdLine, lpwCmdLine, 1024);

		/* Create A save dir if one is not there. 
	This should fix the problem of not bieng able to 
	create one through the zip files. */
	{
		HANDLE hFirstFile;
		WIN32_FIND_DATA findFileData;
		
		TCHAR saveDir[1024];

		_tcscpy(saveDir, _T("\\My Documents\\O"));
		
		hFirstFile = FindFirstFile(saveDir, &findFileData); 
		if (hFirstFile == INVALID_HANDLE_VALUE)
		{
			BOOL b;
			
			b = CreateDirectory(saveDir, NULL); 	
		}
	}
#endif

        /* Save globally */
        hInstance = hInst;

#ifdef _WIN32_WCE
	// Perform application initialization:
	if (!InitInstance (hInst, nCmdShow)) 
	{
		return FALSE;
	}
#else
	/* Initialize */
	if (hPrevInst == NULL)
	{
		wc.style         = CS_CLASSDC;
		wc.lpfnWndProc   = AngbandWndProc;
                wc.cbClsExtra    = 0;
                wc.cbWndExtra    = 4; /* one long pointer to term_data */
                wc.hInstance     = hInst;
                wc.hIcon         = hIcon = LoadIcon(hInst, AppName);
                wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
                wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
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

#endif /* USE_SAVER */

	}
#endif

        /* Temporary hooks */
        plog_aux = hack_plog;
        quit_aux = hack_quit;

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
#ifdef _WIN32_WCE
	paletted = 0;
#else
	paletted = ((GetDeviceCaps(hdc, RASTERCAPS) & RC_PALETTE) ? TRUE : FALSE);
#endif
	ReleaseDC(NULL, hdc);

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
	//core_aux = hook_quit;

	/* Set the system suffix */
	ANGBAND_SYS = "win";
	/* Initialize */
	init_angband();

#ifdef _WIN32_WCE
    /* Re-initialise the hooks */
    add_button_hook = add_button_win;
    kill_button_hook = kill_button_win;
#endif

	/* We are now initialized */
	initialized = TRUE;

#ifdef _WIN32_WCE

	if (strcmp(lpCmdLine, ""))
	{
		
		/* we have a save file */
		char temp[1024];

		strcpy(temp, lpCmdLine);
		strcpy(lpCmdLine, "\\My Documents\\O\\");
		strcat(lpCmdLine, temp);
	}
#endif

	/* Did the user double click on a save file? */
	check_for_save_file(lpCmdLine);

	/* Prompt the user */
	prt("[Choose 'New' or 'Open' from the 'File' menu]", 23, 17);
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

#ifdef _WIN32_WCE
HWND CreateRpCommandBar(HWND hwnd)
{
	SHMENUBARINFO mbi;

	memset(&mbi, 0, sizeof(SHMENUBARINFO));
	mbi.cbSize     = sizeof(SHMENUBARINFO);
	mbi.hwndParent = hwnd;
	mbi.nToolBarId = IDM_MENU;
	mbi.hInstRes   = hInstance;
	mbi.nBmpId     = 0;
	mbi.cBmpImages = 0;

	if (!SHCreateMenuBar(&mbi)) 
		return NULL;

	return mbi.hwndMB;
}

HWND WINAPI CreateMacroBar(HWND hwnd)
{
  HWND hwndTB = NULL;
  INITCOMMONCONTROLSEX iccex;    // The INITCOMMONCONTROLSEX structure
  TCHAR tbbbuf[MAX_MOUSE_LABEL + 9];
  TBBUTTON tbb[NUM_MACRO_BUTTONS + 1];
  TCHAR wccommand[MAX_MOUSE_LABEL];
  int i;
  RECT rectTB;
  
  iccex.dwSize = sizeof (INITCOMMONCONTROLSEX);
  iccex.dwICC = ICC_BAR_CLASSES;
  
  // Register toolbar control classes from the DLL for the common control.
  InitCommonControlsEx (&iccex);
  
  // Create the toolbar control.
  hwndTB = CreateWindowEx(0,
			  TOOLBARCLASSNAME,
			  NULL,
			  WS_CHILD | WS_VISIBLE | TBSTYLE_LIST |
			  CCS_NODIVIDER | CCS_NOPARENTALIGN,
			  0,
			  0,
			  100,
			  30,
			  hwnd,
			  NULL,
			  hInstance,
			  NULL);
  
  if (hwndTB)
    {
      SendMessage(hwndTB,TB_BUTTONSTRUCTSIZE,(WPARAM)sizeof(TBBUTTON),0);
      
      // Create the buttons.
      
      wcscpy(tbbbuf, L"#\0");
      tbb[0].iBitmap = I_IMAGENONE;
      tbb[0].idCommand=MACRO_BUTTON;
      tbb[0].fsState=TBSTATE_ENABLED;
      tbb[0].fsStyle=TBSTYLE_AUTOSIZE | TBSTYLE_CHECK;
      tbb[0].dwData=0;
      tbb[0].iString=SendMessage(hwndTB,TB_ADDSTRING,0,(LPARAM)tbbbuf);
      
      g_g.m_bMacroButton = 0;
      
      for (i = 1; i <= NUM_MACRO_BUTTONS; i++)
	{
/*    wcscpy(tbbbuf, L"\0"); */
      mbstowcs(wccommand, mse_button[i].label, 1024);
      wsprintf(tbbbuf, L"%s", wccommand);
/*	  wsprintf(tbbbuf, L"%d\0", i); */
	  tbb[i].iBitmap = I_IMAGENONE;
	  tbb[i].idCommand=MACRO_BUTTON + i;
	  tbb[i].fsState=TBSTATE_ENABLED | TBSTATE_HIGHLIGHTED;
	  tbb[i].fsStyle=TBSTYLE_AUTOSIZE;
	  tbb[i].dwData=0;
	  tbb[i].iString=SendMessage(hwndTB,TB_ADDSTRING,0,(LPARAM)tbbbuf);
	}
      
      SendMessage(hwndTB, TB_ADDBUTTONS, (WPARAM) NUM_MACRO_BUTTONS + 1, (LPARAM) (LPTBBUTTON) &tbb);
      
      SendMessage(hwndTB, TB_AUTOSIZE, 0, 0); 
      
      // Reposition the toolbar.
      GetWindowRect (hwndTB, &rectTB);
      
      g_g.m_MacroBarHeight = rectTB.bottom - rectTB.top;
      
      if (g_g.m_bTitleBarShown)	MoveWindow (hwndTB, 
					    g_g.m_winX, 
					    g_g.g_cy - (2 * g_g.m_MenuHeight) - g_g.m_MacroBarHeight, 
					    g_g.m_winW, 
					    g_g.m_MacroBarHeight,
					    TRUE);
      else						MoveWindow (hwndTB, 
								    g_g.m_winX, 
								    g_g.g_cy - g_g.m_MenuHeight - g_g.m_MacroBarHeight, 
								    g_g.m_winW, 
								    g_g.m_MacroBarHeight,
								    TRUE);
      
      g_g.m_bMacroBarShown = 1;
    }
  
  return hwndTB;
}

/*
 * Add a button 
 */
int add_button_win(char *label, unsigned char keypress)
{
  int i;
  int length = strlen(label) + 2;
  int button_start = (normal_screen ? status_end : prompt_end);
  int button_end = (normal_screen ? depth_start : Term->wid - 2);

  /* Check the label length */
  if (length > MAX_MOUSE_LABEL) 
    {
      bell("Label too long - button abandoned!");
      return 0;
    }

  /* Check we haven't already got a button for this keypress */
  for (i = 0; i < num_buttons; i++)
    if (mse_button[i].key == keypress)
      return 0;

  /* Check we haven't run out of room */
  button_length += length;
  if (button_length + button_start > button_end) 
    {
      bell("No more room for buttons!");
      button_length -= length;
      return 0;
    }

  /* Make the button */
  strncpy(mse_button[num_buttons].label, label, MAX_MOUSE_LABEL);
  mse_button[num_buttons].left  = button_length;
  mse_button[num_buttons].right = button_length - length + 1;

  if ((g_g.g_TBhWnd) && (!g_g.m_bMacroButton))
  {
	TBBUTTONINFO tbbi;
	TCHAR tbbbuf[MAX_MOUSE_LABEL + 9];
	TCHAR wccommand[MAX_MOUSE_LABEL];

	mbstowcs(wccommand, mse_button[num_buttons].label, 1024);
	wsprintf(tbbbuf, L"%s", wccommand);
	memset(&tbbi,0,sizeof(tbbi));
	tbbi.cbSize = sizeof(tbbi);
	tbbi.dwMask = TBIF_TEXT;
	tbbi.pszText = tbbbuf;
	SendMessage(g_g.g_TBhWnd,TB_SETBUTTONINFO,MACRO_BUTTON + 1 + num_buttons,(LPARAM)&tbbi);
//    plog_fmt("Add label:%s", mse_button[num_buttons].label);
  }

  mse_button[num_buttons++].key = keypress;

  /* Redraw */
  p_ptr->redraw |= (PR_BUTTONS | PR_DEPTH);
  redraw_stuff();

  /* Return the size of the button */
  return (length);
}

/* 
 * Remove a button
 */
int kill_button_win(unsigned char keypress)
{
  int i, j, length;
  TBBUTTONINFO tbbi;
  TCHAR tbbbuf[MAX_MOUSE_LABEL + 9];
  TCHAR wccommand[MAX_MOUSE_LABEL];

//plog_fmt("Remove key:%d", keypress);
  /* Find the button */
  for (i = 0; i < num_buttons; i++)
    if (mse_button[i].key == keypress) break;

  /* No such button */
  if (i == num_buttons)
    {
      //bell("Button kill failed");
      return 0;
    }

  /* Find the length */
  length = mse_button[i].left - mse_button[i].right + 1;
  button_length -= length;

  /* Move each button up one */
  for (j = i; j < num_buttons - 1; j++)
    {
      mse_button[j] = mse_button[j+1];

      /* Adjust length */
      mse_button[j].left -= length;
      mse_button[j].right -= length;
    }

  if ((g_g.g_TBhWnd) && (!g_g.m_bMacroButton))
  {
//  plog_fmt("Remove key, label, pos: %d, %s, %d", keypress, mse_button[i].label, i);
	for (j = i; j < num_buttons - 1; j++)
	{
      mbstowcs(wccommand, mse_button[j].label, 1024);
	  wsprintf(tbbbuf, L"%s", wccommand);
	  memset(&tbbi,0,sizeof(tbbi));
	  tbbi.cbSize = sizeof(tbbi);
	  tbbi.dwMask = TBIF_TEXT;
	  tbbi.pszText = tbbbuf;
	  SendMessage(g_g.g_TBhWnd,TB_SETBUTTONINFO,MACRO_BUTTON + 1 + j,(LPARAM)&tbbi);
//	  plog_fmt("Move label:%s", mse_button[j+1].label);
	}
  }

  /* Wipe the data */
  mse_button[num_buttons - 1].label[0] = '\0';
  mse_button[num_buttons - 1].left  = 0;
  mse_button[num_buttons - 1].right = 0;

  if ((g_g.g_TBhWnd) && (!g_g.m_bMacroButton))
  {
	mbstowcs(wccommand, mse_button[num_buttons - 1].label, 1024);
	wsprintf(tbbbuf, L"%s", wccommand);
	memset(&tbbi,0,sizeof(tbbi));
	tbbi.cbSize = sizeof(tbbi);
	tbbi.dwMask = TBIF_TEXT;
	tbbi.pszText = tbbbuf;
	SendMessage(g_g.g_TBhWnd,TB_SETBUTTONINFO,MACRO_BUTTON + num_buttons,(LPARAM)&tbbi);
  }

  mse_button[num_buttons - 1].key = 0;
  num_buttons--;

  /* Redraw */
  p_ptr->redraw |= (PR_BUTTONS | PR_DEPTH);
  redraw_stuff();

  /* Return the size of the button */
  return (length);
}

#endif

#endif /* WINDOWS */
