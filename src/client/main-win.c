/*
 * File: main-win.c
 * Purpose: Support for Windows Angband
 *
 * Copyright (c) 1997 Ben Harrison, Skirmantas Kligys, Robert Ruehlmann,
 * and others
 * Copyright (c) 2019 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

/* Make sure the WINVER allows the AlphaBlend function */
#if (WINVER < 0x0500)
#define WINVER 0x0500
#endif

#include "c-angband.h"
#include "../win/win-menu.h"
#include "../win/win-term.h"
#include <wingdi.h>

#define HELP_GENERAL "angband.hlp"
#define HELP_SPOILERS "spoilers.hlp"

#ifndef GetWindowLongPtr
#define GetWindowLongPtr GetWindowLong
#endif
#ifndef SetWindowLongPtr
#define SetWindowLongPtr SetWindowLong
#endif
#ifndef GWLP_USERDATA
#define GWLP_USERDATA GWL_USERDATA
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

/*
 * Exclude parts of WINDOWS.H that are not needed (Win32)
 */
#define WIN32_LEAN_AND_MEAN
#define NOSERVICE         /* All Service Controller routines, SERVICE_ equates, etc. */
#define NOKANJI           /* Kanji support stuff. */
#define NOMCX             /* Modem Configuration Extensions */

/*
 * Exclude parts of MMSYSTEM.H that are not needed
 */
#define MMNODRV          /* Installable driver support */
#define MMNOWAVE         /* Waveform support */
#define MMNOMIDI         /* MIDI support */
#define MMNOAUX          /* Auxiliary audio support */
#define MMNOTIMER        /* Timer support */
#define MMNOJOY          /* Joystick support */
#define MMNOMMIO         /* Multimedia file I/O support */
#define MMNOMMSYSTEM     /* General MMSYSTEM functions */

#include <mmsystem.h>

#include <commdlg.h>
#include <shellapi.h>

/*
 * Include the support for loading bitmaps
 */
#include "..\win\readdib.h"

/*
 * Hack -- fake declarations from "dos.h" XXX XXX XXX
 */
#define INVALID_FILE_NAME (DWORD)0xFFFFFFFF

/*
 * Silliness in WIN32 drawing routine
 */
#define MoveTo(H, X, Y) MoveToEx(H, X, Y, NULL)

/*
 * Silliness for Windows 95
 */
#ifndef WS_EX_TOOLWINDOW
# define WS_EX_TOOLWINDOW 0
#endif /* WS_EX_TOOLWINDOW */

/*
 * Foreground color bits (hard-coded by DOS)
 */
#define VID_BLACK   0x00
#define VID_BLUE    0x01
#define VID_GREEN   0x02
#define VID_CYAN    0x03
#define VID_RED     0x04
#define VID_MAGENTA 0x05
#define VID_YELLOW  0x06
#define VID_WHITE   0x07

/*
 * Bright text (hard-coded by DOS)
 */
#define VID_BRIGHT  0x08

/*
 * Background color bits (hard-coded by DOS)
 */
#define VUD_BLACK   0x00
#define VUD_BLUE    0x10
#define VUD_GREEN   0x20
#define VUD_CYAN    0x30
#define VUD_RED     0x40
#define VUD_MAGENTA 0x50
#define VUD_YELLOW  0x60
#define VUD_WHITE   0x70

/*
 * Blinking text (hard-coded by DOS)
 */
#define VUD_BRIGHT  0x80

/*
 * Font settings
 */
#define DEFAULT_FONT	"8X12x.FON"

/*
 * Hack -- resizing
 */
static bool resizing;

/*
 * An array of term_data's
 */
static term_data data[MAX_TERM_DATA];

/*
 * Hack -- global "window creation" pointer
 */
static term_data *my_td;

/*
 * Hack -- global edit control handle
 */
static HWND editmsg;
static HWND old_focus = NULL;
static LRESULT APIENTRY SubClassFunc(HWND hWnd, UINT Message, WPARAM wParam, LPARAM lParam);
static WNDPROC lpfnOldWndProc;

/*
 * Note when "open"/"new" become valid
 */
static bool initialized = false;

/*
 * Screen paletted, i.e. 256 colors
 */
static bool paletted = false;

/*
 * 16 colors screen, don't use RGB()
 */
static bool colors16 = false;

static bool low_priority = false;

/*
 * Saved instance handle
 */
static HINSTANCE hInstance;

/*
 * Yellow brush for the cursor
 */
static HBRUSH hbrYellow;

/*
 * Black brush for the chat window edit control
 */
static HBRUSH hbrBlack;

/*
 * An icon
 */
static HICON hIcon;

/*
 * A palette
 */
static HPALETTE hPal;

/*
 * The global bitmap
 */
static DIBINIT infGraph;

/*
 * The global bitmap mask
 */
static DIBINIT infMask;

static int overdraw = 0;
static int overdrawmax = -1;

static int alphablend = 0;
static BLENDFUNCTION blendfn;

/*
 * Name of application
 */
static const char *AppName = "ANGBAND";

/*
 * Name of sub-window type
 */
static const char *AngList = "AngList";

/*
 * The "complex" color values
 */
static COLORREF win_clr[MAX_COLORS];

/*
 * The "simple" color values
 *
 * See "main-ibm.c" for original table information
 *
 * The entries below are taken from the "color bits" defined above.
 *
 * Note that many of the choices below suck, but so do crappy monitors.
 */
static byte win_pal[MAX_COLORS] =
{
    VID_BLACK,                  /* Dark */
    VID_WHITE,                  /* White */
    VID_CYAN,                   /* Slate XXX */
    VID_RED | VID_BRIGHT,       /* Orange XXX */
    VID_RED,                    /* Red */
    VID_GREEN,                  /* Green */
    VID_BLUE,                   /* Blue */
    VID_YELLOW,                 /* Umber XXX */
    VID_BLACK | VID_BRIGHT,     /* Light Dark */
    VID_CYAN | VID_BRIGHT,      /* Light Slate XXX */
    VID_MAGENTA,                /* Violet XXX */
    VID_YELLOW | VID_BRIGHT,    /* Yellow */
    VID_MAGENTA | VID_BRIGHT,   /* Light Red XXX */
    VID_GREEN | VID_BRIGHT,     /* Light Green */
    VID_BLUE | VID_BRIGHT,      /* Light Blue */
    VID_YELLOW                  /* Light Umber XXX */
};


static int gamma_correction;


static void show_win_error(void)
{
    LPVOID lpMsgBuf;

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(),
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&lpMsgBuf, 0, NULL);

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
        if (islower((unsigned char)*s)) *s = toupper((unsigned char)*s);
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
 * Check for existence of a directory
 */
static bool check_dir(const char *s)
{
    int i;
    char path[MSG_LEN];
    DWORD attrib;

    /* Copy it */
    my_strcpy(path, s, sizeof(path));

    /* Check length */
    i = strlen(path);

    /* Remove trailing backslash */
    if (i && (path[i-1] == '\\')) path[--i] = '\0';

    /* Examine */
    attrib = GetFileAttributes(path);

    /* Require valid filename */
    if (attrib == INVALID_FILE_NAME) return false;

    /* Require directory */
    if (!(attrib & FILE_ATTRIBUTE_DIRECTORY)) return false;

    /* Success */
    return true;
}


/*
 * Validate a file
 */
static void validate_file(const char *s)
{
    /* Verify or fail */
    if (!file_exists(s)) quit_fmt("Cannot find required file:\n%s", s);
}


/*
 * Validate a directory
 */
static void validate_dir(const char *s)
{
    /* Verify or fail */
    if (!check_dir(s)) quit_fmt("Cannot find required directory:\n%s", s);
}


/* Window size */
static void window_size_wh(term_data *td, uint cols, uint rows, uint *wid, uint *hgt)
{
    *wid = cols * td->tile_wid + td->size_ow1 + td->size_ow2;
    *hgt = rows * td->tile_hgt + td->size_oh1 + td->size_oh2;
}


/* Window size */
static void window_size_cr(term_data *td, uint *cols, uint *rows, uint wid, uint hgt)
{
    *cols = (wid - td->size_ow1 - td->size_ow2) / td->tile_wid;
    *rows = (hgt - td->size_oh1 - td->size_oh2) / td->tile_hgt;
}


/* Window size bounds checking */
static void check_window_size(term_data *td)
{
    int cols = td->cols, rows = td->rows;

    check_term_resize((td == &data[0]), &cols, &rows);

    td->cols = cols;
    td->rows = rows;
}


static void stretch_chat_ctrl_win(uint chat_wid, uint chat_hgt)
{
    /* Resize the edit control */
    SetWindowPos(editmsg, 0, 2, chat_hgt - 21, chat_wid - 6, 20, SWP_NOZORDER);
}


/*
 * Get the "size" for a window
 */
static void term_getsize(term_data *td)
{
    RECT rc;
    uint wid, hgt;

    /* Paranoia */
    if (td->cols < 1) td->cols = 1;
	if (td->rows < 1) td->rows = 1;

    if (arg_graphics_nice && (td == &data[0]))
    {
        graphics_mode *mode = get_graphics_mode(use_graphics, true);

        if (mode && mode->grafID)
        {
            if (mode->file[0])
            {
                char *end;

                td->tile_wid = strtol(mode->file, &end, 10);
                td->tile_hgt = strtol(end + 1, NULL, 10);
            }
            else
            {
                td->tile_wid = mode->cell_width;
                td->tile_hgt = mode->cell_height;
            }
            if ((td->tile_wid == 0) || (td->tile_hgt == 0))
            {
                td->tile_wid = mode->cell_width;
                td->tile_hgt = mode->cell_height;
            }
            if ((td->tile_wid == 0) || (td->tile_hgt == 0))
            {
                td->tile_wid = td->font_wid;
                td->tile_hgt = td->font_hgt;
            }
        }
        else
        {
            /* Reset the tile info */
            td->tile_wid = td->font_wid;
            td->tile_hgt = td->font_hgt;
        }

        tile_width = 1;
        tile_height = 1;

        if ((td->tile_hgt >= td->font_hgt * 3) && (td->tile_wid >= td->font_wid * 3))
        {
            tile_width = 3;
            tile_height = 3;
            td->tile_wid /= 3;
            td->tile_hgt /= 3;
        }
        else if ((td->tile_hgt >= td->font_hgt * 2) && (td->tile_wid >= td->font_wid * 2))
        {
            tile_width = 2;
            tile_height = 2;
            td->tile_wid /= 2;
            td->tile_hgt /= 2;
        }

        if (td->tile_wid >= td->font_wid * 2)
        {
            tile_width *= 2;
            td->tile_wid /= 2;
        }

        if (td->tile_wid < td->font_wid) td->tile_wid = td->font_wid;
        if (td->tile_hgt < td->font_hgt) td->tile_hgt = td->font_hgt;

        tile_distorted = is_tile_distorted(use_graphics, tile_width, tile_height);
    }

    /* Paranoia */
    check_window_size(td);

    /* Window sizes */
    window_size_wh(td, td->cols, td->rows, &wid, &hgt);

    /* Window sizes */
    if (td == &data[PMSG_TERM]) stretch_chat_ctrl_win(wid, hgt);

    /* Client window size */
    rc.left = 0;
    rc.right = rc.left + wid;
    rc.top = 0;
    rc.bottom = rc.top + hgt;

    /* Get total window size (without menu for sub-windows) */
    AdjustWindowRectEx(&rc, td->dwStyle, (td == &data[0])? true: false, td->dwExStyle);

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
static void save_prefs_aux(term_data *td, const char *sec_name)
{
    RECT rc;
    WINDOWPLACEMENT lpwndpl;

    /* Paranoia */
    if (!td->w) return;

    /* Visible */
    conf_set_int(sec_name, "Visible", td->visible);

    /* Font */
    conf_set_string(sec_name, "Font", (td->font_file? td->font_file: DEFAULT_FONT));

    if (td == &data[0])
    {
        /* Bizarre */
        conf_set_int(sec_name, "Bizarre", td->bizarre);

        /* Tile size (x) */
        conf_set_int(sec_name, "TileWid", td->tile_wid);

        /* Tile size (y) */
        conf_set_int(sec_name, "TileHgt", td->tile_hgt);
    }

    /* Window size (x) */
    conf_set_int(sec_name, "NumCols", td->cols);

    /* Window size (y) */
    conf_set_int(sec_name, "NumRows", td->rows);

    /* Get window placement and dimensions */
    lpwndpl.length = sizeof(WINDOWPLACEMENT);
    GetWindowPlacement(td->w, &lpwndpl);

    /* Acquire position in *normal* mode (not minimized) */
    rc = lpwndpl.rcNormalPosition;

    /*
     * Sometimes the RECT has negative coordinates (-32000)
     * Make a check here so that you don't have to open the INI file
     * to correct them if it happens...
     */
    if (rc.left < 0) rc.left = 0;
    if (rc.top < 0) rc.top = 0;

    /* Get information about the placement of the window */
    if (lpwndpl.flags & SW_SHOWMAXIMIZED)
        td->maximized = true;
    else
        td->maximized = false;

    /* Window position (x) */
    conf_set_int(sec_name, "PositionX", rc.left);

    /* Window position (y) */
    conf_set_int(sec_name, "PositionY", rc.top);

    /* Maximized */
    if (td == &data[0]) conf_set_int(sec_name, "Maximized", td->maximized);
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

    /* Save the "use_graphics" flag */
    conf_set_int("Angband", "Graphics", use_graphics);

    /* Save the "arg_graphics_nice" flag */
    conf_set_int("Angband", "Graphics_Nice", arg_graphics_nice);

    /* Save the tile width */
    conf_set_int("Angband", "TileWidth", tile_width);

    /* Save the tile height */
    conf_set_int("Angband", "TileHeight", tile_height);

    /* Save window prefs */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
        term_data *td = &data[i];

        strnfmt(buf, sizeof(buf), "Term-%d", i);

        save_prefs_aux(td, buf);
    }
}


/*
 * Load the "prefs" for a single term
 */
static void load_prefs_aux(term_data *td, const char *sec_name)
{
    char tmp[MSG_LEN];
    int wid, hgt;

    /* Visible */
    td->visible = (conf_get_int(sec_name, "Visible", td->visible) != 0);

    /* Desired font, with default */
    my_strcpy(tmp, conf_get_string(sec_name, "Font", DEFAULT_FONT), sizeof(tmp));

    /* Analyze font, save desired font name */
    td->font_want = string_make(analyze_font(tmp, &wid, &hgt));

    if (td == &data[0])
    {
        /* Bizarre */
        td->bizarre = (conf_get_int(sec_name, "Bizarre", true) != 0);

        /* Tile size */
        td->tile_wid = conf_get_int(sec_name, "TileWid", wid);
        td->tile_hgt = conf_get_int(sec_name, "TileHgt", hgt);
    }

    /* Window size */
    td->cols = conf_get_int(sec_name, "NumCols", td->cols);
    td->rows = conf_get_int(sec_name, "NumRows", td->rows);

    /* Window position */
    td->pos_x = conf_get_int(sec_name, "PositionX", td->pos_x);
    td->pos_y = conf_get_int(sec_name, "PositionY", td->pos_y);

    /* Maximized */
    if (td == &data[0])
        td->maximized = (conf_get_int(sec_name, "Maximized", td->maximized) != 0);
}


static errr Term_xtra_win_react(int v);


/*
 * Load the "prefs"
 */
static void load_prefs(void)
{
    int i;
    char buf[MSG_LEN];
    bool first_start;

    if (conf_exists())
        first_start = false;
    else
        first_start = true;

    /* Extract the "arg_graphics" flag */
    arg_graphics = conf_get_int("Angband", "Graphics", GRAPHICS_NONE);

    /* Extract the "arg_graphics_nice" flag */
    arg_graphics_nice = conf_get_int("Angband", "Graphics_Nice", true);

    /* Extract the tile width */
    tile_width = conf_get_int("Angband", "TileWidth", 1);

    /* Extract the tile height */
    tile_height = conf_get_int("Angband", "TileHeight", 1);

    /* Extract the gamma correction */
    gamma_correction = conf_get_int("Angband", "Gamma", 0);

    /* Load window prefs */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
        term_data *td = &data[i];

        strnfmt(buf, sizeof(buf), "Term-%d", i);

        load_prefs_aux(td, buf);
    }

    if (first_start) default_layout_win(data, MAX_TERM_DATA);

    /* Pull nick/pass */
    my_strcpy(nick, conf_get_string("MAngband", "nick", "PLAYER"), sizeof(nick));
    my_strcpy(pass, conf_get_string("MAngband", "pass", "passwd"), sizeof(pass));
    my_strcpy(server_name, conf_get_string("MAngband", "host", ""), sizeof(server_name));

    my_strcpy(meta_address, conf_get_string("MAngband", "meta_address", "mangband.org"),
        sizeof(meta_address));
    meta_port = conf_get_int("MAngband", "meta_port", 8802);

    /* XXX Default real name */
    my_strcpy(real_name, "PLAYER", sizeof(real_name));

    /* Toggle "use_graphics" */
    Term_xtra_win_react(arg_graphics);
}


/*
 * Create the new global palette based on the bitmap palette
 * (if any), and the standard 16 entry palette derived from
 * "win_clr[]" which is used for the basic 16 Angband colors.
 *
 * This function is never called before all windows are ready.
 *
 * This function returns false if the new palette could not be
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
    LPLOGPALETTE pLogPal;
    LPPALETTEENTRY lppe;
    term_data *td;

    /* This makes no sense */
    if (!paletted) return true;

    /* No bitmap */
    lppe = NULL;
    nEntries = 0;

    /* Check the bitmap palette */
    hBmPal = infGraph.hPalette;

    /* Use the bitmap */
    if (hBmPal)
    {
        lppe = mem_alloc(256 * sizeof(PALETTEENTRY));
        nEntries = GetPaletteEntries(hBmPal, 0, 255, lppe);
        if ((nEntries == 0) || (nEntries > 220))
        {
            /* Warn the user */
            plog("Please switch to high- or true-color mode.");

            /* Cleanup */
            mem_free(lppe);

            /* Fail */
            return false;
        }
    }

    /* Size of palette */
    pLogPalSize = sizeof(LOGPALETTE) + (nEntries + 16) * sizeof(PALETTEENTRY);

    /* Allocate palette */
    pLogPal = (LPLOGPALETTE)mem_alloc(pLogPalSize);

    /* Version */
    pLogPal->palVersion = 0x300;

    /* Make room for bitmap and normal data */
    pLogPal->palNumEntries = nEntries + 16;

    /* Save the bitmap data */
    for (i = 0; i < nEntries; i++) pLogPal->palPalEntry[i] = lppe[i];

    /* Save the normal data */
    for (i = 0; i < BASIC_COLORS; i++)
    {
        LPPALETTEENTRY p;

        /* Access the entry */
        p = &(pLogPal->palPalEntry[i+nEntries]);

        /* Save the colors */
        p->peRed = GetRValue(win_clr[i]);
        p->peGreen = GetGValue(win_clr[i]);
        p->peBlue = GetBValue(win_clr[i]);

        if (gamma_correction > 0)
        {
            p->peRed = gamma_table[p->peRed];
            p->peGreen = gamma_table[p->peGreen];
            p->peBlue = gamma_table[p->peBlue];
        }

        /* Save the flags */
        p->peFlags = PC_NOCOLLAPSE;
    }

    /* Free something */
    mem_free(lppe);

    /* Create a new palette, or fail */
    hNewPal = CreatePalette(pLogPal);
    if (!hNewPal) quit("Cannot create palette!");

    /* Free the palette */
    mem_free(pLogPal);

    /* Main window */
    td = &data[0];

    /* Realize the palette */
    hdc = GetDC(td->w);
    SelectPalette(hdc, hNewPal, 0);
    i = RealizePalette(hdc);
    ReleaseDC(td->w, hdc);
    if (i == 0) quit("Cannot realize palette!");

    /* Sub-windows */
    for (i = 1; i < MAX_TERM_DATA; i++)
    {
        td = &data[i];

        hdc = GetDC(td->w);
        SelectPalette(hdc, hNewPal, 0);
        ReleaseDC(td->w, hdc);
    }

    /* Delete old palette */
    if (hPal) DeleteObject(hPal);

    /* Save new palette */
    hPal = hNewPal;

    /* Success */
    return true;
}


/*
 * Initialize graphics
 */
static bool init_graphics(int v)
{
    char buf[MSG_LEN];
    int wid, hgt;
    const char *name;
    const char *path;
    graphics_mode *mode = NULL;

    if (v)
        mode = get_graphics_mode(v, false);
    if (mode)
    {
        if (!mode->pref[0])
        {
            plog_fmt("Invalid tile prefname '%s'", mode->menuname);
            return false;
        }
        wid = mode->cell_width;
        hgt = mode->cell_height;
        if ((wid < 2) || (hgt < 2))
        {
            plog_fmt("Invalid tile dimensions in tileset: '%s'", mode->menuname);
            return false;
        }

        path = mode->path;
        name = mode->file;

        overdraw = mode->overdrawRow;
        overdrawmax = mode->overdrawMax;
        alphablend = mode->alphablend;
    }
    else
    {
        plog("Could not find graphics mode");
        return false;
    }

    /* Access the bitmap file */
    path_build(buf, sizeof(buf), path, name);

    /* Load the image or quit */
    if (alphablend)
    {
        /* See if the given file is already premultiplied */
        if (strstr(buf, "_pre"))
        {
            /* If so, just load it */
            if (!ReadDIB2_PNG(data[0].w, buf, &infGraph, NULL, false))
            {
                plog_fmt("Cannot read file '%s'", buf);
                return false;
            }
        }
        else
        {
            /* If not, see if there is already a premultiplied tileset associated to the file */
            char *ext;
            char modname[MSG_LEN];

            ext = strstr(buf, ".png");

            /* Make sure we have enough space to make the desired name */
            if (ext)
            {
                my_strcpy(modname, buf, 1 + ext - buf);
                my_strcat(modname, "_pre.png", MSG_LEN);

                /* If the file does not exist, mark that we need to create it */
                if (!file_exists(modname)) ext = NULL;
            }

            /* At this point we know the file exists, so load it */
            if (ext)
            {
                if (!ReadDIB2_PNG(data[0].w, modname, &infGraph, NULL, false))
                {
                    plog_fmt("Cannot read premultiplied version of file '%s'", buf);
                    return false;
                }
            }

            /* If not, load the base file and premultiply it */
            else
            {
                if (!ReadDIB2_PNG(data[0].w, buf, &infGraph, NULL, true))
                {
                    plog_fmt("Cannot read file '%s'", buf);
                    return false;
                }

                /* Save the premultiplied file (TODO) */
            }
        }
    }
    else if (!ReadDIB2_PNG(data[0].w, buf, &infGraph, &infMask, false))
    {
        plog_fmt("Cannot read file '%s'", buf);
        return false;
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
        return false;
    }

    /* Result */
    return true;
}


/*
 * Resize a window
 */
static void term_window_resize(const term_data *td)
{
    /* Require window */
    if (!td->w) return;

    /* Resize the window */
    SetWindowPos(td->w, 0, 0, 0, td->size_wid, td->size_hgt, SWP_NOMOVE | SWP_NOZORDER);

    /* Redraw later */
    InvalidateRect(td->w, NULL, true);
}


/*
 * Remove a font, given its filename.
 */
static void term_remove_font(const char *name)
{
    char buf[MSG_LEN];

    /* Build path to the file */
    my_strcpy(buf, ANGBAND_DIR_FONTS, sizeof(buf));
    my_strcat(buf, "\\", sizeof(buf));
    my_strcat(buf, name, sizeof(buf));

    /* Remove it */
    RemoveFontResource(buf);

    /* Notify other applications of the change  XXX */
    PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
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
static errr term_force_font(term_data *td, const char *path)
{
    int i;
    int wid, hgt;
    char *base;
    char buf[MSG_LEN];

    /* Check we have a path */
    if (!path) return (1);

    /* Forget the old font (if needed) */
    if (td->font_id) DeleteObject(td->font_id);

    /* Forget old font */
    if (td->font_file)
    {
        bool used = false;

        /* Scan windows */
        for (i = 0; i < MAX_TERM_DATA; i++)
        {
            /* Check "screen" */
            if ((td != &data[i]) && data[i].font_file && streq(data[i].font_file, td->font_file))
                used = true;
        }

        /* Remove unused font resources */
        if (!used) term_remove_font(td->font_file);

        /* Free the old name */
        string_free(td->font_file);
    }

    /* Local copy */
    my_strcpy(buf, path, sizeof(buf));

    /* Analyze font path */
    base = analyze_font(buf, &wid, &hgt);

    /* Verify suffix */
    if (!suffix(base, ".FON")) return (1);

    /* Verify file */
    if (!file_exists(buf)) return (1);

    /* Load the new font */
    if (!AddFontResourceEx(buf, FR_PRIVATE, 0)) return (1);

    /* Save new font name */
    td->font_file = string_make(base);

    /* Remove the "suffix" */
    base[strlen(base) - 4] = '\0';

    /* Create the font (using the 'base' of the font file name!) */
    td->font_id = CreateFont(hgt, wid, 0, 0, FW_DONTCARE, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS,
        CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FIXED_PITCH | FF_DONTCARE, base);

    /* Hack -- unknown size */
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


static void force_font(term_data *td, char *tmp, int len)
{
    /* Force the font */
    if (term_force_font(td, tmp))
    {
        /* Access the standard font file */
        path_build(tmp, len, ANGBAND_DIR_FONTS, DEFAULT_FONT);

        /* Force the use of that font */
        term_force_font(td, tmp);

        /* Reset the tile info */
        td->tile_wid = td->font_wid;
        td->tile_hgt = td->font_hgt;

        /* Hack -- assume bizarre */
        td->bizarre = true;
    }

    /* Reset the tile info */
    if ((td != &data[0]) || !td->tile_wid || !td->tile_hgt)
    {
        td->tile_wid = td->font_wid;
        td->tile_hgt = td->font_hgt;
    }

    /* Analyze the font */
    term_getsize(td);

    /* Resize the window */
    term_window_resize(td);
}


/*
 * Allow the user to change the font for this window.
 */
static void term_change_font(term_data *td)
{
    OPENFILENAME ofn;
    char tmp[MSG_LEN] = "";

    /* Extract a default if possible */
    if (td->font_file) my_strcpy(tmp, td->font_file, sizeof(tmp));

    /* Ask for a choice */
    memset(&ofn, 0, sizeof(ofn));
    ofn.lStructSize = sizeof(ofn);
    ofn.hwndOwner = data[0].w;
    ofn.lpstrFilter = "Angband Font Files (*.fon)\0*.fon\0";
    ofn.nFilterIndex = 1;
    ofn.lpstrFile = tmp;
    ofn.nMaxFile = 128;
    ofn.lpstrInitialDir = ANGBAND_DIR_FONTS;
    ofn.Flags = OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;
    ofn.lpstrDefExt = "fon";

    /* Force choice if legal */
    if (GetOpenFileName(&ofn)) force_font(td, tmp, sizeof(tmp));
}


static void windows_map_aux(void);


/*
 * Hack -- redraw a term_data
 */
static void term_data_redraw(term_data *td)
{
    if (map_active)
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
static void term_data_redraw_section(term_data *td, int x1, int y1, int x2, int y2)
{
    /* Activate the term */
    Term_activate(&td->t);

    /* Redraw the area */
    Term_redraw_section(x1, y1, x2, y2);

    /* Restore the term */
    Term_activate(term_screen);
}


/*
 * Function hooks needed by "Term"
 */


static errr Term_pict_win(int x, int y, int n, const u16b *ap, const char *cp, const u16b *tap,
    const char *tcp);


/*
 * React to global changes
 */
static errr Term_xtra_win_react(int v)
{
    int i;

    /* Get the main window */
    term_data *td = &data[0];

    /* Simple or complex color */
    if (colors16)
    {
        /* Save the default colors */
        for (i = 0; i < MAX_COLORS; i++)
        {
            /* Simply accept the desired colors */
            win_pal[i] = angband_color_table[i][0];
        }
    }
    else
    {
        COLORREF code;
        byte rv, gv, bv;
        bool change = false;

        /* Save the default colors */
        for (i = 0; i < MAX_COLORS; i++)
        {
            /* Extract desired values */
            rv = angband_color_table[i][1];
            gv = angband_color_table[i][2];
            bv = angband_color_table[i][3];

            if (gamma_correction > 0)
            {
                rv = gamma_table[rv];
                gv = gamma_table[gv];
                bv = gamma_table[bv];
            }

            /* Extract a full color code */
            code = PALETTERGB(rv, gv, bv);

            /* Activate changes */
            if (win_clr[i] != code)
            {
                /* Note the change */
                change = true;

                /* Apply the desired color */
                win_clr[i] = code;
            }
        }

        /* Activate the palette if needed */
        if (change) new_palette();
    }

    /* Handle "use_graphics" */
    if (use_graphics != v)
    {
        /* Free the bitmap stuff */
        FreeDIB(&infGraph);
        FreeDIB(&infMask);

        /* Initialize (if needed) */
        if (v && !init_graphics(v))
        {
            /* Warning */
            plog("Cannot initialize graphics!");

            /* Cannot enable */
            v = GRAPHICS_NONE;
        }
        else td->t.pict_hook = Term_pict_win;

        /* Change setting */
        use_graphics = v;
        tile_distorted = is_tile_distorted(use_graphics, tile_width, tile_height);

        if (arg_graphics_nice)
        {
            /* Hack -- assume bizarre */
            td->bizarre = true;

			/* Analyze the font */
			term_getsize(td);

			/* Resize the window */
			term_window_resize(td);
        }

		/* Reset visuals */
		reset_visuals(true);
    }

    /* Hack -- loading prefs */
    if (!initialized) return (0);

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

            /* Hack -- resize the term */
            Term_resize(td->cols, td->rows, td->rows);

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

    /* Wait or check for an event */
    if (v)
    {
        /* Block */
        if (GetMessage(&msg, NULL, 0, 0))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }
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

    /* Hack -- resize the main window here */
    if (resizing)
    {
        term_data *td = &data[0];
        int i;

        /* Just in case a key is pending */
        if (Setup.initialized) Term_key_push(ESCAPE);

        term_getsize(td);
        term_window_resize(td);

        /* Activate */
        Term_activate(&td->t);

        /* Resize the term */
        Term_resize(td->cols, td->rows, td->rows);

        /* Redraw later */
        InvalidateRect(td->w, NULL, true);

        td->size_hack = true;

        /* Show sub-windows */
        for (i = 1; i < MAX_TERM_DATA; i++)
        {
            if (data[i].visible) ShowWindow(data[i].w, SW_SHOW);
        }

        td->size_hack = false;

        /* Dungeon size */
        if (Setup.initialized)
        {
            net_term_resize(td->cols, td->rows, td->rows);
            do_cmd_redraw();
        }

        resizing = false;
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
 * Delay for "x" milliseconds
 */
static int Term_xtra_win_delay(int v)
{
    /* Sleep */
    if (v > 0) Sleep(v);

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
        /* Process random events */
        case TERM_XTRA_BORED: return (Term_xtra_win_event(0));

        /* Process an event */
        case TERM_XTRA_EVENT: return (Term_xtra_win_event(v));

        /* Flush all events */
        case TERM_XTRA_FLUSH: return (Term_xtra_win_flush());

        /* Clear the screen */
        case TERM_XTRA_CLEAR: return (Term_xtra_win_clear());

        /* React to global changes */
        case TERM_XTRA_REACT: return (Term_xtra_win_react(v));

        /* Delay for some milliseconds */
        case TERM_XTRA_DELAY: return (Term_xtra_win_delay(v));
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

    if (map_active)
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
 * Draw a "cursor" at (x,y), using a "yellow box".
 */
static errr Term_bigcurs_win(int x, int y)
{
    term_data *td = (term_data*)(Term->data);
    RECT rc;
    HDC hdc;
    int tile_wid, tile_hgt;

    if (map_active || Term->minimap_active)
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
    rc.right = rc.left + tile_width * tile_wid;
    rc.top = y * tile_hgt + td->size_oh1;
    rc.bottom = rc.top + tile_height * tile_hgt;

    /* If we are using overdraw, draw a double height cursor */
    if (overdraw && Term->double_cursor)
    {
        u16b a, ta;
        char c, tc;
        int j = 0;

        if (!Term_info(x, y, &a, &c, &ta, &tc)) j = (a & 0x7F);
        if ((j > 2) && (j >= overdraw) && (j <= overdrawmax))
        {
            rc.top -= tile_height * tile_hgt;
            rc.bottom = rc.top + ((tile_height * tile_hgt) << 1);
        }
    }

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
    int tile_wid, tile_hgt;

    if (map_active)
    {
        tile_wid = td->map_tile_wid;
        tile_hgt = td->map_tile_hgt;
    }
    else
    {
        tile_wid = td->tile_wid;
        tile_hgt = td->tile_hgt;
    }

    /* Rectangle to erase in client coords */
    rc.left = x * tile_wid + td->size_ow1;
    rc.right = rc.left + n * tile_wid;
    rc.top = y * tile_hgt + td->size_oh1;
    rc.bottom = rc.top + tile_hgt;

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
static void Term_text_win_aux(int x, int y, int n, u16b a, const char *s)
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
        SetTextColor(hdc, PALETTEINDEX(win_pal[a % MAX_COLORS]));
    else
    {
        if (paletted)
            SetTextColor(hdc, win_clr[(a % MAX_COLORS) & 0x1F]);
        else
            SetTextColor(hdc, win_clr[a % MAX_COLORS]);

        /* Handle background */
        switch (a / MAX_COLORS)
        {
            /* Default Background */
            case BG_BLACK: SetBkColor(hdc, win_clr[0]); break;

            /* Background same as foreground */
            case BG_SAME: SetBkColor(hdc, win_clr[a % MAX_COLORS]); break;

            /* Highlight Background */
            case BG_DARK: SetBkColor(hdc, win_clr[COLOUR_SHADE]); break;
        }
    }

    /* Use the font */
    SelectObject(hdc, td->font_id);

    /* Bizarre or normal size */
    if (td->bizarre || (td->tile_hgt != td->font_hgt) || (td->tile_wid != td->font_wid))
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
            ExtTextOut(hdc, rc.left, rc.top, 0, &rc, s + i, 1, NULL);

            /* Advance */
            rc.left += td->tile_wid;
            rc.right += td->tile_wid;
        }
    }
    else
    {
        /* Dump the text */
        ExtTextOut(hdc, rc.left, rc.top, ETO_OPAQUE | ETO_CLIPPED, &rc, s, n, NULL);
    }

    /* Release DC */
    ReleaseDC(td->w, hdc);
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
static void Term_pict_win_aux(int x, int y, int n, const u16b *ap, const char *cp, const u16b *tap,
    const char *tcp)
{
    term_data *td = (term_data*)(Term->data);
    int i;
    int x1, y1, w1, h1;
	int x2, y2, w2, h2, tw2, th2;
    int x3, y3;
    HDC hdcMask;
    HDC hdc;
    HDC hdcSrc;
    HBITMAP hbmSrcOld;
    int tx, ty;
    int tile_wid = 1, tile_hgt = 1;

    /* Large tile mode */
    if (!map_active && !Term->minimap_active)
    {
        tile_wid = tile_width;
        tile_hgt = tile_height;
    }

    /* Erase the grids */
    for (tx = x; tx < x + n * tile_wid; tx++)
        for (ty = y; ty < y + tile_hgt; ty++)
            Term_wipe_win(tx, ty, 1);

    /* Size of bitmap cell */
    w1 = infGraph.CellWidth;
    h1 = infGraph.CellHeight;

    /* Size of window cell */
    if (map_active)
    {
        w2 = td->map_tile_wid;
        h2 = td->map_tile_hgt;
    }
    else
    {
        w2 = td->tile_wid;
        h2 = td->tile_hgt;
    }
    tw2 = tile_wid * w2;
    th2 = tile_hgt * h2;

    /* Location of window cell */
    x2 = x * w2 + td->size_ow1;
    y2 = y * h2 + td->size_oh1;

    /* Info */
    hdc = GetDC(td->w);

    /* More info */
    hdcSrc = CreateCompatibleDC(hdc);
    hbmSrcOld = SelectObject(hdcSrc, infGraph.hBitmap);

    if (infMask.hBitmap && !alphablend && !overdraw)
    {
        hdcMask = CreateCompatibleDC(hdc);
        SelectObject(hdcMask, infMask.hBitmap);
    }
    else
        hdcMask = NULL;

    /* Draw attr/char pairs */
    for (i = n - 1; i >= 0; i--, x2 -= w2)
    {
        u16b a = ap[i];
        char c = cp[i];

        /* Extract picture */
        int row = (a & 0x7F);
        int col = (c & 0x7F);

        /* Location of bitmap cell */
        x1 = col * w1;
        y1 = row * h1;

        if (hdcMask)
        {
            /* Default background to darkness */
            x3 = y3 = 0;

            /* Use the terrain picture only if mapped */
            if (tap[i] & 0x80)
            {
                x3 = (tcp[i] & 0x7F) * w1;
                y3 = (tap[i] & 0x7F) * h1;
            }

            /* Perfect size */
            if ((w1 == tw2) && (h1 == th2))
            {
                /* Efficiency */
                if (x3 || y3)
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
                else if ((x1 != x3) || (y1 != y3))
                    BitBlt(hdc, x2, y2, tw2, th2, hdcSrc, x1, y1, SRCCOPY);
            }

            /* Need to stretch */
            else
            {
                /* Set the correct mode for stretching the tiles */
                SetStretchBltMode(hdc, COLORONCOLOR);

                /* Efficiency */
                if (x3 || y3)
                {
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
                else if ((x1 != x3) || (y1 != y3))
                    StretchBlt(hdc, x2, y2, tw2, th2, hdcSrc, x1, y1, w1, h1, SRCCOPY);
            }
        }
        else if (alphablend || overdraw)
        {
            int trow = (tap[i] & 0x7F);

            /* Default background to darkness */
            x3 = y3 = 0;

            /* Use the terrain picture only if mapped */
            if (tap[i] & 0x80)
            {
                x3 = (tcp[i] & 0x7F) * w1;
                y3 = trow * h1;
            }

            /* Set the correct mode for stretching the tiles */
            SetStretchBltMode(hdc, COLORONCOLOR);

            /* Perfect size */
            if ((w1 == tw2) && (h1 == th2))
            {
                /* Copy the terrain picture from the bitmap to the window */
                BitBlt(hdc, x2, y2, tw2, th2, hdcSrc, x3, y3, SRCCOPY);
            }

            /* Need to stretch */
            else
            {
                /* Copy the terrain picture from the bitmap to the window */
                StretchBlt(hdc, x2, y2, tw2, th2, hdcSrc, x3, y3, w1, h1, SRCCOPY);
            }

            if (overdraw && (trow >= overdraw) && (y > 2) && (trow <= overdrawmax))
                AlphaBlend(hdc, x2, y2 - th2, tw2, th2, hdcSrc, x3, y3 - h1, w1, h1, blendfn);

            /* Only draw if terrain and overlay are different */
            if ((x1 != x3) || (y1 != y3))
            {
                /* Copy the picture from the bitmap to the window */
                if (overdraw && (row >= overdraw) && (y > 2) && (row <= overdrawmax))
                {
                    AlphaBlend(hdc, x2, y2 - th2, tw2, th2 * 2, hdcSrc, x1, y1 - h1, w1, h1 * 2,
                        blendfn);
                }
                else
                    AlphaBlend(hdc, x2, y2, tw2, th2, hdcSrc, x1, y1, w1, h1, blendfn);
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
                /* Set the correct mode for stretching the tiles */
                SetStretchBltMode(hdc, COLORONCOLOR);

                /* Copy the picture from the bitmap to the window */
				StretchBlt(hdc, x2, y2, tw2, th2, hdcSrc, x1, y1, w1, h1, SRCCOPY);
            }
        }
    }

    /* Release */
    SelectObject(hdcSrc, hbmSrcOld);
    DeleteDC(hdcSrc);

    if (hdcMask)
    {
        /* Release */
        SelectObject(hdcMask, hbmSrcOld);
        DeleteDC(hdcMask);
    }

    /* Release */
    ReleaseDC(td->w, hdc);
}


/*
 * Draw several ("n") chars, with an attr, at a given location.
 *
 * For double-height tiles, we redraw all double-height tiles below.
 */
static errr Term_text_win(int x, int y, int n, u16b a, const char *s)
{
    int i;
    u16b fa, ta;
    char fc, tc;
    int tile_wid = 1, tile_hgt = 1;

    /* Large tile mode */
    if (!map_active && !Term->minimap_active)
    {
        tile_wid = tile_width;
        tile_hgt = tile_height;
    }

    /* Redraw the current text */
    Term_text_win_aux(x, y, n, a, s);

    /* Redraw the bottom tiles (recursively) */
    for (i = 0; i < n; i++)
    {
        int j = 1, tilex, tiley;

        while (j)
        {
            /* Get the position of the jth tile below the ith character */
            tilex = COL_MAP + ((x - COL_MAP + i) / tile_wid) * tile_wid;
            tiley = ROW_MAP + ((y - ROW_MAP) / tile_hgt + j) * tile_hgt;

            if (overdraw && (tiley > 2) && !Term_info(tilex, tiley, &fa, &fc, &ta, &tc))
            {
                int row = (fa & 0x7F);
                int trow = (ta & 0x7F);

                if (((trow >= overdraw) && (trow <= overdrawmax)) ||
                    ((row >= overdraw) && (row <= overdrawmax)))
                {
                    Term_pict_win_aux(tilex, tiley, 1, &fa, &fc, &ta, &tc);
                    j++;
                }
                else j = 0;
            }
            else j = 0;
        }
    }

    /* Success */
    return 0;
}


/*
 * Draw an array of "special" attr/char pairs at the given location.
 *
 * Called with n > 1 only if always_pict is true, which is never the case.
 *
 * For double-height tiles, we redraw the tile just above and all double-height tiles below.
 */
static errr Term_pict_win(int x, int y, int n, const u16b *ap, const char *cp, const u16b *tap,
    const char *tcp)
{
    int i;
    u16b a, ta;
    char c, tc;
    int tile_wid = 1, tile_hgt = 1;

    /* Large tile mode */
    if (!map_active && !Term->minimap_active)
    {
        tile_wid = tile_width;
        tile_hgt = tile_height;
    }

    /* Redraw the top tiles */
    for (i = 0; i < n; i++)
    {
        if ((alphablend || overdraw) &&
            !Term_info(x + i * tile_wid, y - tile_hgt, &a, &c, &ta, &tc))
        {
            if (a & 0x80)
                Term_pict_win_aux(x + i * tile_wid, y - tile_hgt, 1, &a, &c, &ta, &tc);
            else if (!map_active)
            {
                int tx, ty;

                for (tx = x + i * tile_wid; tx < x + (i + 1) * tile_wid; tx++)
                {
                    for (ty = y - tile_hgt; ty < y; ty++)
                    {
                        Term_info(tx, ty, &a, &c, &ta, &tc);
                        Term_text_win_aux(tx, ty, 1, a, &c);
                    }
                }
            }
        }
    }

    /* Redraw the current tiles */
    Term_pict_win_aux(x, y, n, ap, cp, tap, tcp);

    /* Redraw the bottom tiles (recursively) */
    for (i = 0; i < n; i++)
    {
        int j = 1;

        while (j)
        {
            if (overdraw && (y + j * tile_hgt > 2) &&
                !Term_info(x + i * tile_wid, y + j * tile_hgt, &a, &c, &ta, &tc))
            {
                int row = (a & 0x7F);
                int trow = (ta & 0x7F);

                if (((trow >= overdraw) && (trow <= overdrawmax)) ||
                    ((row >= overdraw) && (row <= overdrawmax)))
                {
                    Term_pict_win_aux(x + i * tile_wid, y + j * tile_hgt, 1, &a, &c, &ta, &tc);
                    j++;
                }
                else j = 0;
            }
            else j = 0;
        }
    }

    /* Success */
    return 0;
}


static void map_info(int y, int x, u16b *ap, char *cp, u16b *tap, char *tcp)
{
    *ap = player->scr_info[y][x].a;
    *cp = player->scr_info[y][x].c;
    *tap = player->trn_info[y][x].a;
    *tcp = player->trn_info[y][x].c;
}


static void windows_map_aux(void)
{
    term_data *td = &data[0];
    u16b a;
    char c;
    int x, min_x, max_x;
    int y, min_y, max_y;
    u16b ta;
    char tc;

    /* Paranoia */
    if (!Setup.initialized) return;

    td->map_tile_wid = (td->tile_wid * td->cols) / Setup.max_col;
    td->map_tile_hgt = (td->tile_hgt * td->rows) / Setup.max_row;

    min_x = 0;
    min_y = 0;
    max_x = Setup.max_col;
    max_y = Setup.max_row;

    /* Draw the map */
    for (x = min_x; x < max_x; x++)
    {
        for (y = min_y; y < max_y; y++)
        {
            map_info(y, x, &a, &c, &ta, &tc);

            /* Ignore non-graphics */
            if (a & 0x80) Term_pict_win(x - min_x, y - min_y, 1, &a, &c, &ta, &tc);
        }
    }

    /* Highlight the player */
    Term_curs_win(player->px - min_x, player->py - min_y);
}


/* Loop callback */
static void map_callback_begin(ui_event *cp)
{
    /* Wait until we get the whole thing */
    if (last_line_info == -1) cp->type = EVT_DONE;
}


/*
 * Display a graphical (full) map of the dungeon.
 */
static void windows_map(void)
{
    ui_event ke = EVENT_EMPTY;

    /* Only in graphics mode since the fonts can't be scaled */
    if (!use_graphics) return;

    /* Prevent various menu-actions from working */
    initialized = false;

    /* Enter "icky" mode */
    topline_icky = true;

    /* Save screen */
    screen_save();

    /* Clear screen */
    Term_clear();

    map_active = true;

    /* Reset the line counter */
    last_line_info = -2;

    /* Send the request */
    Send_fullmap();

    /* Wait until we get the whole thing */
    while (last_line_info != -1)
    {
        /* Wait for net input, or a key */
        ke = Net_loop(Term_inkey, map_callback_begin, NULL, SCAN_OFF);

        /* Check for user abort */
        if (is_exit(ke)) break;
    }

    /* Draw the map */
    if (!is_exit(ke))
    {
        windows_map_aux();
        flush_hack();

        /* Wait for a keypress, flush key buffer */
        inkey_ex();
        Term_flush();
    }

    /* Switch off the map display */
    map_active = false;

    /* Restore the screen */
    screen_load(true);

    /* We are ready again */
    initialized = true;

    /* Leave "icky" mode */
    topline_icky = false;
}


/*
 * Other routines
 */


/*
 * Create and initialize a "term_data" given a title
 */
static void term_data_link(term_data *td)
{
    term *t = &td->t;

    /* Initialize the term */
    term_init(t, td->cols, td->rows, td->rows, td->keys);

    /* Use a "software" cursor */
    t->soft_cursor = true;

    /* Use "Term_pict" for "graphic" data */
    t->higher_pict = true;

    /* Erase with "white space" */
    t->attr_blank = COLOUR_WHITE;
    t->char_blank = ' ';

    /* Differentiate between BS/^h, Tab/^i, etc. */
    t->complex_input = true;

    /* Prepare the template hooks */
    t->xtra_hook = Term_xtra_win;
    t->curs_hook = Term_curs_win;
    t->bigcurs_hook = Term_bigcurs_win;
    t->wipe_hook = Term_wipe_win;
    t->text_hook = Term_text_win;
    t->pict_hook = Term_pict_win;

    /* Remember where we came from */
    t->data = td;
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
    char buf[MSG_LEN];
    HFONT editfont;
    MENUITEMINFO mii;
    HMENU hm;
    graphics_mode *mode;
    uint wid, hgt;

    /* Main window */
    td = &data[0];
    memset(td, 0, sizeof(term_data));
    td->s = version_build(VB_NAME | VB_BUILD);
    td->keys = MSG_LEN;
    td->rows = NORMAL_HGT;
    td->cols = NORMAL_WID;
    td->visible = true;
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
        memset(td, 0, sizeof(term_data));
        td->s = angband_term_name[i];
        td->keys = 16;
        td->rows = NORMAL_HGT;
        td->cols = NORMAL_WID;
        td->visible = false;
        td->size_ow1 = 1;
        td->size_ow2 = 1;
        td->size_oh1 = 1;
        td->size_oh2 = 1;
        td->pos_x = (7 - i) * 30;
        td->pos_y = (7 - i) * 20;
    }

    /* Chat window */
    data[PMSG_TERM].size_oh2 = 30;

    /* Load prefs */
    load_prefs();

    /* Main window (need these before term_getsize gets called) */
    td = &data[0];
    td->dwStyle = (WS_OVERLAPPED | WS_THICKFRAME | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX |
        WS_CAPTION | WS_VISIBLE);
    if (td->maximized) td->dwStyle |= WS_MAXIMIZE;
    td->dwExStyle = 0;
    td->visible = true;

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
        path_build(buf, sizeof(buf), ANGBAND_DIR_FONTS, td->font_want);

        /* Activate the chosen font */
        force_font(td, buf, sizeof(buf));
    }

    /* Sub windows (reverse order) */
    for (i = MAX_TERM_DATA - 1; i >= 1; --i)
    {
        td = &data[i];

        my_td = td;
        td->w = CreateWindowEx(td->dwExStyle, AngList, td->s, td->dwStyle, td->pos_x, td->pos_y,
            td->size_wid, td->size_hgt, HWND_DESKTOP, NULL, hInstance, NULL);
        my_td = NULL;
        if (!td->w) quit("Failed to create sub-window");

        if (td->visible)
        {
            td->size_hack = true;
            ShowWindow(td->w, SW_SHOW);
            td->size_hack = false;
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

    /* Chat window */
    td = &data[PMSG_TERM];
    window_size_wh(td, td->cols, td->rows, &wid, &hgt);
    editmsg = CreateWindowEx(WS_EX_STATICEDGE, "EDIT", NULL,
        WS_CHILD | ES_AUTOHSCROLL | ES_OEMCONVERT | WS_VISIBLE, 2, hgt - 24, wid - 8, 20,
        data[PMSG_TERM].w, NULL, hInstance, NULL);
    editfont = CreateFont(16, 0, 0, 0, FW_NORMAL, false, false, false, ANSI_CHARSET,
        OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY, DEFAULT_PITCH, "Arial");
    SendMessage(editmsg, WM_SETFONT, (int)editfont, (int)NULL);
    stretch_chat_ctrl_win(wid, hgt);
    SendMessage(editmsg, EM_LIMITTEXT, 590, 0L);
    lpfnOldWndProc = (WNDPROC)SetWindowLongPtr(editmsg, GWLP_WNDPROC, (DWORD)(SubClassFunc));

    /* Main window */
    td = &data[0];

    /* Main window */
    my_td = td;
    td->w = CreateWindowEx(td->dwExStyle, AppName, td->s, td->dwStyle, td->pos_x, td->pos_y,
        td->size_wid, td->size_hgt, HWND_DESKTOP, NULL, hInstance, NULL);
    my_td = NULL;
    if (!td->w) quit("Failed to create Angband window");

    term_data_link(td);
    term_screen = &td->t;

    /* Activate the main window */
    SetActiveWindow(td->w);

    /* Bring main window back to top */
    SetWindowPos(td->w, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);

    if (gamma_correction > 0)
        build_gamma_table(gamma_correction);

    /* New palette XXX XXX XXX */
    new_palette();

    /* Create a "brush" for drawing the "cursor" */
    hbrYellow = CreateSolidBrush(win_clr[COLOUR_YELLOW]);

    /* Create a "brush" for drawing the chat window edit control background */
    hbrBlack = CreateSolidBrush(0);

    /* Populate the graphic options sub menu with the graphics modes */
    hm = GetMenu(data[0].w);
    mii.cbSize = sizeof(MENUITEMINFO);
    mii.fMask = MIIM_ID | MIIM_TYPE;
    mii.fType = MFT_STRING;
    mode = graphics_modes;
    while (mode)
    {
        if (mode->grafID != GRAPHICS_NONE)
        {
            mii.wID = mode->grafID + IDM_OPTIONS_GRAPHICS_NONE;
            mii.dwTypeData = mode->menuname;
            mii.cch = strlen(mode->menuname);
            InsertMenuItem(hm, IDM_OPTIONS_GRAPHICS_NICE, false, &mii);
        }
        mode = mode->pNext;
    }
    mii.fMask = MIIM_TYPE;
    mii.fType = MFT_SEPARATOR;
    mii.wID = 399;
    mii.dwTypeData = 0;
    mii.cch = 0;
    InsertMenuItem(hm, IDM_OPTIONS_GRAPHICS_NICE, false, &mii);

    /* Setup the alpha blending function */
    blendfn.BlendOp = AC_SRC_OVER;
    blendfn.BlendFlags = 0;
    blendfn.AlphaFormat = AC_SRC_ALPHA;
    blendfn.SourceConstantAlpha = 255;

    /* Process pending messages */
    Term_xtra_win_flush();

    /* Chat window */
    term_chat->user = (void*)0;
    if (data[PMSG_TERM].visible) term_chat->user = (void*)1;
}


struct idx_menu
{
    int idm;
    int tw;
    int th;
};


static struct idx_menu idm_options_tile[] =
{
    {IDM_OPTIONS_TILE_1x1, 1, 1},
    {IDM_OPTIONS_TILE_2x1, 2, 1},
    {IDM_OPTIONS_TILE_2x2, 2, 2},
    {IDM_OPTIONS_TILE_3x1, 3, 1},
    {IDM_OPTIONS_TILE_3x3, 3, 3},
    {IDM_OPTIONS_TILE_4x2, 4, 2},
    {IDM_OPTIONS_TILE_4x4, 4, 4},
    {IDM_OPTIONS_TILE_6x3, 6, 3},
    {IDM_OPTIONS_TILE_6x6, 6, 6},
    {IDM_OPTIONS_TILE_8x4, 8, 4},
    {IDM_OPTIONS_TILE_8x8, 8, 8},
    {IDM_OPTIONS_TILE_16x8, 16, 8},
    {IDM_OPTIONS_TILE_16x16, 16, 16}
};


static struct idx_menu idm_tile_font[] =
{
    {IDM_TILE_08X08, 8, 8},
    {IDM_TILE_16X16, 16, 16},
    {IDM_TILE_32X32, 32, 32},
    {IDM_TILE_08X16, 8, 16},
    {IDM_TILE_10X20, 10, 20},
    {IDM_TILE_16X32, 16, 32},
    {IDM_TILE_08X13, 8, 13},
    {IDM_TILE_10X17, 10, 17},
    {IDM_TILE_12X13, 12, 13},
    {IDM_TILE_12X20, 12, 20},
    {IDM_TILE_16X25, 16, 25}
};


/*
 * Prepare the menus
 */
static void setup_menus(void)
{
    size_t i;
    graphics_mode *mode;
    HMENU hm = GetMenu(data[0].w);

    /* Menu "File" */
    EnableMenuItem(hm, IDM_FILE_NEW, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_FILE_OPEN, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_FILE_SAVE, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_FILE_EXIT, MF_BYCOMMAND | MF_ENABLED);
    EnableMenuItem(hm, IDM_WINDOW_OPT, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_WINDOW_RESET, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

    /* Allow accessing the window options */
    if (initialized && Setup.initialized)
        EnableMenuItem(hm, IDM_WINDOW_OPT, MF_BYCOMMAND | MF_ENABLED);

    if (initialized && !Setup.initialized)
        EnableMenuItem(hm, IDM_WINDOW_RESET, MF_BYCOMMAND | MF_ENABLED);

    /* Menu "Window::Visibility" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
        EnableMenuItem(hm, IDM_WINDOW_VIS_0 + i, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        CheckMenuItem(hm, IDM_WINDOW_VIS_0 + i, (data[i].visible? MF_CHECKED: MF_UNCHECKED));
        if (i > 0) EnableMenuItem(hm, IDM_WINDOW_VIS_0 + i, MF_BYCOMMAND | MF_ENABLED);
    }

    /* Menu "Window::Font" */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
        EnableMenuItem(hm, IDM_WINDOW_FONT_0 + i, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        if (data[i].visible)
            EnableMenuItem(hm, IDM_WINDOW_FONT_0 + i, MF_BYCOMMAND | MF_ENABLED);
    }

    /* Menu "Window::Bizarre Display" */
    EnableMenuItem(hm, IDM_WINDOW_BIZ, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    CheckMenuItem(hm, IDM_WINDOW_BIZ, (data[0].bizarre ? MF_CHECKED : MF_UNCHECKED));
    if (data[0].visible && use_graphics && !arg_graphics_nice && !Setup.initialized)
        EnableMenuItem(hm, IDM_WINDOW_BIZ, MF_BYCOMMAND | MF_ENABLED);

    /* Menu "Window::Increase Tile Width" */
    EnableMenuItem(hm, IDM_WINDOW_I_WID, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    if (data[0].visible && use_graphics && !arg_graphics_nice && !Setup.initialized)
        EnableMenuItem(hm, IDM_WINDOW_I_WID, MF_BYCOMMAND | MF_ENABLED);

    /* Menu "Window::Decrease Tile Width" */
    EnableMenuItem(hm, IDM_WINDOW_D_WID, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    if (data[0].visible && use_graphics && !arg_graphics_nice && !Setup.initialized)
        EnableMenuItem(hm, IDM_WINDOW_D_WID, MF_BYCOMMAND | MF_ENABLED);

    /* Menu "Window::Increase Tile Height" */
    EnableMenuItem(hm, IDM_WINDOW_I_HGT, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    if (data[0].visible && use_graphics && !arg_graphics_nice && !Setup.initialized)
        EnableMenuItem(hm, IDM_WINDOW_I_HGT, MF_BYCOMMAND | MF_ENABLED);

    /* Menu "Window::Decrease Tile Height" */
    EnableMenuItem(hm, IDM_WINDOW_D_HGT, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    if (data[0].visible && use_graphics && !arg_graphics_nice && !Setup.initialized)
        EnableMenuItem(hm, IDM_WINDOW_D_HGT, MF_BYCOMMAND | MF_ENABLED);

    /* Menu "Options", disable all */
    mode = graphics_modes;
    while (mode)
    {
        EnableMenuItem(hm, mode->grafID + IDM_OPTIONS_GRAPHICS_NONE,
            MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
        mode = mode->pNext;
    }

    EnableMenuItem(hm, IDM_OPTIONS_GRAPHICS_NICE, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

    for (i = IDM_OPTIONS_TILE_1x1; i <= IDM_OPTIONS_TILE_16x16; i++)
        EnableMenuItem(hm, i, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

    for (i = IDM_TILE_FONT; i <= IDM_TILE_12X13; i++)
        EnableMenuItem(hm, i, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

    EnableMenuItem(hm, IDM_TILE_12X20, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_TILE_16X25, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

    EnableMenuItem(hm, IDM_OPTIONS_SAVER, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_OPTIONS_LOW_PRIORITY, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

    /* Menu "Options", Item "Map" */
    if (initialized && use_graphics)
        EnableMenuItem(hm, IDM_OPTIONS_MAP, MF_BYCOMMAND | MF_ENABLED);
    else
        EnableMenuItem(hm, IDM_OPTIONS_MAP, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);

    /* Menu "Options", update all */
    mode = graphics_modes;
    while (mode)
    {
        CheckMenuItem(hm, mode->grafID + IDM_OPTIONS_GRAPHICS_NONE,
            ((use_graphics == mode->grafID)? MF_CHECKED: MF_UNCHECKED));
        mode = mode->pNext;
    }

    CheckMenuItem(hm, IDM_OPTIONS_GRAPHICS_NICE, (arg_graphics_nice? MF_CHECKED: MF_UNCHECKED));

    for (i = 0; i < N_ELEMENTS(idm_options_tile); i++)
    {
        if ((tile_width == idm_options_tile[i].tw) && (tile_height == idm_options_tile[i].th))
            CheckMenuItem(hm, idm_options_tile[i].idm, MF_CHECKED);
        else
            CheckMenuItem(hm, idm_options_tile[i].idm, MF_UNCHECKED);
    }

    if ((data[0].tile_wid == data[0].font_wid) && (data[0].tile_hgt == data[0].font_hgt))
        CheckMenuItem(hm, IDM_TILE_FONT, MF_CHECKED);
    else
        CheckMenuItem(hm, IDM_TILE_FONT, MF_UNCHECKED);

    for (i = 0; i < N_ELEMENTS(idm_tile_font); i++)
    {
        if (((int)data[0].tile_wid == idm_tile_font[i].tw) &&
            ((int)data[0].tile_hgt == idm_tile_font[i].th))
        {
            CheckMenuItem(hm, idm_tile_font[i].idm, MF_CHECKED);
        }
        else
            CheckMenuItem(hm, idm_tile_font[i].idm, MF_UNCHECKED);
    }

    CheckMenuItem(hm, IDM_OPTIONS_LOW_PRIORITY, (low_priority? MF_CHECKED: MF_UNCHECKED));

    /* Menu "Options", Item "Graphics" */
    if (initialized && !Setup.initialized)
    {
        mode = graphics_modes;
        while (mode)
        {
            /* Disable pseudo-3D tiles */
            if (!mode->distorted)
                EnableMenuItem(hm, mode->grafID + IDM_OPTIONS_GRAPHICS_NONE, MF_ENABLED);
            mode = mode->pNext;
        }

		EnableMenuItem(hm, IDM_OPTIONS_GRAPHICS_NICE, MF_ENABLED);
        if (use_graphics && !arg_graphics_nice)
        {
            for (i = IDM_OPTIONS_TILE_1x1; i <= IDM_OPTIONS_TILE_16x16; i++)
                EnableMenuItem(hm, i, MF_ENABLED);

            for (i = IDM_TILE_FONT; i <= IDM_TILE_12X13; i++)
                EnableMenuItem(hm, i, MF_ENABLED);

            EnableMenuItem(hm, IDM_TILE_12X20, MF_ENABLED);
            EnableMenuItem(hm, IDM_TILE_16X25, MF_ENABLED);
        }
    }

    EnableMenuItem(hm, IDM_OPTIONS_LOW_PRIORITY, MF_BYCOMMAND | MF_ENABLED);

    /* Menu "Help" */
    EnableMenuItem(hm, IDM_HELP_GENERAL, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    EnableMenuItem(hm, IDM_HELP_SPOILERS, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    if (initialized && Setup.initialized)
    {
        EnableMenuItem(hm, IDM_HELP_GENERAL, MF_BYCOMMAND | MF_ENABLED);
        EnableMenuItem(hm, IDM_HELP_SPOILERS, MF_BYCOMMAND | MF_ENABLED);
    }
}


/*
 * Display a help file
 */
static void display_help(const char *filename)
{
    Term_keypress('?', 0);
}


static BOOL CALLBACK dDialogProc(
  HWND hwndDlg,  /* handle to dialog box */
  UINT uMsg,     /* message */
  WPARAM wParam, /* first message parameter */
  LPARAM lParam  /* second message parameter */
)
{
    switch (uMsg)
    {
        case WM_COMMAND:
            EndDialog(hwndDlg,0);
    }
    return 0;
}


/*
 * Process a menu command
 */
static void process_menus(WORD wCmd)
{
    int i;
    term_data *td;

    /* Analyze */
    switch (wCmd)
    {
        /* New game */
        case IDM_FILE_NEW:
        {
            /* Oops */
            plog("You are not allowed to do that!");
            break;
        }

        /* Open game */
        case IDM_FILE_OPEN:
        {
            /* Oops */
            plog("You are not allowed to do that!");
            break;
        }

        /* Save game */
        case IDM_FILE_SAVE:
        {
            /* Oops */
            plog("You are not allowed to do that!");
            break;
        }

        /* Exit */
        case IDM_FILE_EXIT:
        {
            quit(NULL);
            break;
        }

        case IDM_WINDOW_VIS_0:
        {
            /* Oops */
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
                td->visible = true;
                ShowWindow(td->w, SW_SHOW);
                term_data_redraw(td);

                /* Chat window */
                if (i == PMSG_TERM) term_chat->user = (void*)1;
            }
            else
            {
                td->visible = false;
                ShowWindow(td->w, SW_HIDE);

                /* Chat window */
                if (i == PMSG_TERM) term_chat->user = (void*)0;
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
            /* Paranoia */
            if (arg_graphics_nice && !initialized)
            {
                plog("You may not do that right now.");
                break;
            }
                  
            i = wCmd - IDM_WINDOW_FONT_0;

            if ((i < 0) || (i >= MAX_TERM_DATA)) break;

            td = &data[i];

            term_change_font(td);

			if (arg_graphics_nice)
			{
                /* Hack -- force redraw */
                Term_key_push(KTRL('R'));
			}

            break;
        }

        /* Bizarre Display */
        case IDM_WINDOW_BIZ:
        {
            td = &data[0];

            td->bizarre = !td->bizarre;

            term_getsize(td);

            term_window_resize(td);

            break;
        }

        /* Increase Tile Width */
        case IDM_WINDOW_I_WID:
        {
            td = &data[0];

            td->tile_wid += 1;

            term_getsize(td);

            term_window_resize(td);

            break;
        }

        /* Decrease Tile Height */
        case IDM_WINDOW_D_WID:
        {
            td = &data[0];

            td->tile_wid -= 1;

            term_getsize(td);

            term_window_resize(td);

            break;
        }

        /* Increase Tile Height */
        case IDM_WINDOW_I_HGT:
        {
            td = &data[0];

            td->tile_hgt += 1;

            term_getsize(td);

            term_window_resize(td);

            break;
        }

        /* Decrease Tile Height */
        case IDM_WINDOW_D_HGT:
        {
            td = &data[0];

            td->tile_hgt -= 1;

            term_getsize(td);

            term_window_resize(td);

            break;
        }

        case IDM_WINDOW_OPT:
        {
            /* Paranoia */
            if (initialized && Setup.initialized)
            {
                Term_keypress('=', 0);
                Term_keypress('w', 0);
            }

            break;
        }

        case IDM_WINDOW_RESET:
        {
            /* Paranoia */
            if (!initialized)
            {
                plog("You may not do that right now.");
                break;
            }

            if (MessageBox(NULL,
                "This will reset the size and layout of the angband windows\n based on your screen size. Do you want to continue?",
                "Warning", MB_YESNO | MB_ICONWARNING) == IDYES)
            {
                char buf[MSG_LEN];

                default_layout_win(data, MAX_TERM_DATA);

                /* React to changes */
                Term_xtra_win_react(arg_graphics);

                /* Reposition */
                for (i = MAX_TERM_DATA - 1; i >= 0; i--)
                {
                    td = &data[i];

                    SetWindowPos(td->w, ((i > 0)? data[0].w: 0), td->pos_x, td->pos_y, 0, 0, SWP_NOSIZE);
                }

                /* All windows */
                for (i = 0; i < MAX_TERM_DATA; i++)
                {
                    td = &data[i];

                    /* Access the standard font file */
                    path_build(buf, sizeof(buf), ANGBAND_DIR_FONTS, td->font_want);

                    /* Activate the chosen font */
                    force_font(td, buf, sizeof(buf));
                }

                /* Sub windows (reverse order) */
                for (i = MAX_TERM_DATA - 1; i >= 1; i--)
                {
                    td = &data[i];

                    if (td->visible)
                    {
                        ShowWindow(td->w, SW_SHOW);
                        term_data_redraw(td);

                        /* Chat window */
                        if (i == PMSG_TERM) term_chat->user = (void*)1;
                    }
                    else
                    {
                        ShowWindow(td->w, SW_HIDE);

                        /* Chat window */
                        if (i == PMSG_TERM) term_chat->user = (void*)0;
                    }
                }

                /* Focus on main window */
                SetFocus(data[0].w);
            }

            break;
        }

        case IDM_OPTIONS_GRAPHICS_NICE:
        {
            /* Paranoia */
            if (!initialized)
            {
                plog("You may not do that right now.");
                break;
            }

            /* Toggle "arg_graphics_nice" */
            arg_graphics_nice = !arg_graphics_nice;

            td = &data[0];

            /* Hack -- assume bizarre */
            td->bizarre = true;

            /* Analyze the font */
            term_getsize(td);

            /* Resize the window */
            term_window_resize(td);

            /* React to changes */
            Term_xtra_win_react(use_graphics);

            break;
        }

        case IDM_OPTIONS_TILE_1x1:
        case IDM_OPTIONS_TILE_2x1:
        case IDM_OPTIONS_TILE_2x2:
        case IDM_OPTIONS_TILE_3x1:
        case IDM_OPTIONS_TILE_3x3:
        case IDM_OPTIONS_TILE_4x2:
        case IDM_OPTIONS_TILE_4x4:
        case IDM_OPTIONS_TILE_6x3:
        case IDM_OPTIONS_TILE_6x6:
        case IDM_OPTIONS_TILE_8x4:
        case IDM_OPTIONS_TILE_8x8:
        case IDM_OPTIONS_TILE_16x8:
        case IDM_OPTIONS_TILE_16x16:
        {
            /* Paranoia */
            if (!initialized)
            {
                plog("You may not do that right now.");
                break;
            }

            for (i = 0; i < N_ELEMENTS(idm_options_tile); i++)
            {
                if (idm_options_tile[i].idm == wCmd) break;
            }

            tile_width = idm_options_tile[i].tw;
            tile_height = idm_options_tile[i].th;
            tile_distorted = is_tile_distorted(use_graphics, tile_width, tile_height);

            td = &data[0];
            term_getsize(td);
            term_window_resize(td);

            /* React to changes */
            Term_xtra_win_react(use_graphics);

            break;
        }

        case IDM_TILE_FONT:
        case IDM_TILE_08X08:
        case IDM_TILE_16X16:
        case IDM_TILE_32X32:
        case IDM_TILE_08X16:
        case IDM_TILE_10X20:
        case IDM_TILE_16X32:
        case IDM_TILE_08X13:
        case IDM_TILE_10X17:
        case IDM_TILE_12X13:
        case IDM_TILE_12X20:
        case IDM_TILE_16X25:
        {
            /* Paranoia */
            if (arg_graphics_nice && !initialized)
            {
                plog("You may not do that right now.");
                break;
            }
                  
            for (i = 0; i < N_ELEMENTS(idm_tile_font); i++)
            {
                if (idm_tile_font[i].idm == wCmd) break;
            }

            td = &data[0];

            if (i == N_ELEMENTS(idm_tile_font))
            {
                td->tile_wid = td->font_wid;
                td->tile_hgt = td->font_hgt;
            }
            else
            {
                td->tile_wid = idm_tile_font[i].tw;
                td->tile_hgt = idm_tile_font[i].th;
            }

            term_getsize(td);
            term_window_resize(td);

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
            /* Paranoia */
            if (!initialized)
            {
                plog("You may not do that right now.");
                break;
            }

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

        case IDM_OPTIONS_SCREENSHOT:
        {
            char filename[MSG_LEN];
            char path[MSG_LEN];
            time_t ltime;
            struct tm *today;
            int len;

            time(&ltime);
            today = localtime(&ltime);
            strnfmt(filename, sizeof(filename), "%s", nick);
            len = strlen(filename);
            strftime(filename + len, sizeof(filename) - len, "_%Y%b%d_%H%M%S.png", today);

            /* Get the system-specific path */
            path_build(path, sizeof(path), ANGBAND_DIR_USER, filename);

            td = &data[0];
            if (!SaveWindow_PNG(td->w, path)) plog("Screenshot Save Failed.");

            break;
        }

        case IDM_HELP_ABOUT:
        {
            DialogBox(hInstance, "ABOUT", data[0].w, dDialogProc);
            break;
        }

        default:
        {
            if ((wCmd >= IDM_OPTIONS_GRAPHICS_NONE) &&
                (wCmd <= IDM_OPTIONS_GRAPHICS_NONE + graphics_mode_high_id))
            {
                int selected_mode = 0;
                int desired_mode = wCmd - IDM_OPTIONS_GRAPHICS_NONE;
                graphics_mode *mode = graphics_modes;

                /* Paranoia */
                if (!initialized)
                {
                    plog("You may not do that right now.");
                    break;
                }

                while (mode)
                {
                    if (mode->grafID == desired_mode)
                    {
                        selected_mode = desired_mode;
                        break;
                    }
                    mode = mode->pNext;
                }

                /* Toggle "use_graphics" */
                if (use_graphics != selected_mode)
                {
                    if (selected_mode == GRAPHICS_NONE)
                    {
                        reset_tile_params();

                        /* Hard code some values when switching to text mode */
                        if (!arg_graphics_nice)
                        {
                            td = &data[0];
                            td->tile_wid = td->font_wid;
                            td->tile_hgt = td->font_hgt;

                            /* React to changes */
                            term_getsize(td);
                            term_window_resize(td);
                        }
                    }
                    Term_xtra_win_react(selected_mode);
                }
            }
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
    td = (term_data *)GetWindowLongPtr(hWnd, GWLP_USERDATA);

    BeginPaint(hWnd, &ps);

    if (map_active)
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


static int extract_modifiers(keycode_t ch, bool kp)
{
    bool mc = false;
    bool ms = false;
    bool ma = false;

    /* Extract the modifiers */
    if (GetKeyState(VK_CONTROL) & 0x8000) mc = true;
    if (GetKeyState(VK_SHIFT)   & 0x8000) ms = true;
    if (GetKeyState(VK_MENU)    & 0x8000) ma = true;

    return ((mc && (kp || MODS_INCLUDE_CONTROL(ch))? KC_MOD_CONTROL: 0) |
        (ms && (kp || MODS_INCLUDE_SHIFT(ch))? KC_MOD_SHIFT: 0) |
        (ma? KC_MOD_ALT: 0) | (kp? KC_MOD_KEYPAD: 0));
}


/*
 * We ignore the modifier keys (shift, control, alt, num lock, scroll lock),
 * and the normal keys (escape, tab, return, letters, numbers, etc), but we
 * catch the keypad keys (with and without numlock set, including keypad 5),
 * the function keys (including the "menu" key which maps to F10), and the
 * "pause" key (between scroll lock and numlock).  We also catch a few odd
 * keys which I do not recognize, but which are listed among keys which we
 * do catch, so they should be harmless to catch.
 *
 * Returns true if the key was handled, false otherwise.
 */
static bool handle_keydown(WPARAM wParam, LPARAM lParam)
{
    keycode_t ch = 0;
    bool kp = false;

    switch (wParam)
    {
        case VK_F1: ch = KC_F1; break;
        case VK_F2: ch = KC_F2; break;
        case VK_F3: ch = KC_F3; break;
        case VK_F4: ch = KC_F4; break;
        case VK_F5: ch = KC_F5; break;
        case VK_F6: ch = KC_F6; break;
        case VK_F7: ch = KC_F7; break;
        case VK_F8: ch = KC_F8; break;
        case VK_F9: ch = KC_F9; break;
        case VK_F10: ch = KC_F10; break;
        case VK_F11: ch = KC_F11; break;
        case VK_F12: ch = KC_F12; break;
        case VK_F13: ch = KC_F13; break;
        case VK_F14: ch = KC_F14; break;
        case VK_F15: ch = KC_F15; break;

        case VK_INSERT: ch = KC_INSERT; break;
        case VK_DELETE: ch = KC_DELETE; break;
        case VK_BACK: break;
        case VK_TAB: break;

        case VK_PRIOR: ch = KC_PGUP; break;
        case VK_NEXT: ch = KC_PGDOWN; break;
        case VK_END: ch = KC_END; break;
        case VK_HOME: ch = KC_HOME; break;
        case VK_LEFT: ch = ARROW_LEFT; break;
        case VK_RIGHT: ch = ARROW_RIGHT; break;
        case VK_UP: ch = ARROW_UP; break;
        case VK_DOWN: ch = ARROW_DOWN; break;

        case VK_CLEAR: ch = '5'; kp = true; break;
        case VK_PAUSE: ch = KC_PAUSE; break;
    }

    if (ch)
    {
        int mods = extract_modifiers(ch, kp);

        Term_keypress(ch, mods);
        return true;
    }

    return false;
}


static LRESULT FAR PASCAL AngbandWndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    HDC hdc;
    term_data *td;
    int i;
    int vsc, vk, mods;
    bool kp = false, extended_key;
    keycode_t ch;

    /* Acquire proper "term_data" info */
    td = (term_data *)GetWindowLongPtr(hWnd, GWLP_USERDATA);

    /* Handle message */
    switch (uMsg)
    {
        /* XXX XXX XXX */
        case WM_NCCREATE:
        {
            SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG)(my_td));
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
            uint wid, hgt;

            /* This message was sent before WM_NCCREATE */
            if (!td) return 1;

            lpmmi = (MINMAXINFO FAR *)lParam;

            /* Minimum window size is 80x24 */
            rc.left = rc.top = 0;
            window_size_wh(td, NORMAL_WID, NORMAL_HGT, &wid, &hgt);
            rc.right = rc.left + wid;
            rc.bottom = rc.top + hgt;

            /* Adjust */
            AdjustWindowRectEx(&rc, td->dwStyle, true, td->dwExStyle);

            /* Save minimum size (preserve minimum window size) */
            if (rc.right - rc.left > lpmmi->ptMinTrackSize.x)
                lpmmi->ptMinTrackSize.x = rc.right - rc.left;
            if (rc.bottom - rc.top > lpmmi->ptMinTrackSize.y)
                lpmmi->ptMinTrackSize.y = rc.bottom - rc.top;

            /* Limit to server maximum while playing */
            if (Setup.initialized)
            {
                rc.left = rc.top = 0;
                window_size_wh(td, Setup.max_col * tile_width + COL_MAP + 1,
                    Setup.max_col * tile_height + ROW_MAP + 1, &wid, &hgt);
                rc.right = rc.left + wid;
                rc.bottom = rc.top + hgt;

                /* Paranoia */
                rc.right  += (td->tile_wid - 1);
                rc.bottom += (td->tile_hgt - 1);

                /* Adjust */
                AdjustWindowRectEx(&rc, td->dwStyle, true, td->dwExStyle);

                /* Save maximum size (preserve maximum window size) */
                if (rc.right - rc.left < lpmmi->ptMaxSize.x)
                    lpmmi->ptMaxSize.x = rc.right - rc.left;
                if (rc.bottom - rc.top < lpmmi->ptMaxSize.y)
                    lpmmi->ptMaxSize.y = rc.bottom - rc.top;

                /* Save maximum size (preserve maximum window size) */
                if (rc.right - rc.left < lpmmi->ptMaxTrackSize.x)
                    lpmmi->ptMaxTrackSize.x = rc.right - rc.left;
                if (rc.bottom - rc.top < lpmmi->ptMaxTrackSize.y)
                    lpmmi->ptMaxTrackSize.y = rc.bottom - rc.top;
            }

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
            if (handle_keydown(wParam, lParam)) return 0;
            break;
        }

        case WM_CHAR:
        {
            vsc = LOBYTE(HIWORD(lParam));
            extended_key = ((HIBYTE(HIWORD(lParam)) & 0x1)? true: false);
            vk = MapVirtualKey(vsc, 1);

            if (!Setup.initialized && (wParam == KTRL('X')))
            {
                process_menus(IDM_FILE_EXIT);
                return 0;
            }

            /*
             * We don't want to translate some keys to their ascii values
             * so we have to intercept them here
             */
            switch (vk)
            {
                /* Fix backspace */
                case 8:
                    ch = KC_BACKSPACE;
                    break;

                /* Fix tab */
                case 9:
                    ch = KC_TAB;
                    break;

                /* Fix enter */
                case 13:
                    ch = KC_ENTER;
                    if (extended_key) kp = true;
                    break;

                /* Fix escape */
                case 27:
                    ch = ESCAPE;
                    break;

                default:
                    Term_keypress(wParam, 0);
                    return 0;
            }

            mods = extract_modifiers(ch, kp);
            Term_keypress(ch, mods);

            return 0;
        }

        case WM_INITMENU:
        {
            setup_menus();
            return 0;
        }

#ifndef WM_QUERYENDSESSION
#define WM_QUERYENDSESSION 0x0011
#endif

        case WM_QUERYENDSESSION:
        case WM_QUIT:
        {
            quit(NULL);
            return 1;
        }

        case WM_CLOSE:
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
            /* This message was sent before WM_NCCREATE */
            if (!td) return 1;

            /* It was sent from inside CreateWindowEx */
            if (!td->w) return 1;

            /* Was sent from WM_SIZE */
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
                    uint cols, rows;

                    window_size_cr(td, &cols, &rows, LOWORD(lParam), HIWORD(lParam));

                    /* New size */
                    if (((uint)(td->cols) != cols) || ((uint)(td->rows) != rows))
                    {
                        /* Don't overflow */
                        if (cols > 255) cols = 255;
                        if (rows > 255) rows = 255;

                        /* Save the new size */
                        td->cols = cols;
                        td->rows = rows;

                        /* Hack -- resize the main window later */
                        if (td == &data[0])
                        {
                            resizing = true;
                            return 0;
                        }

                        /* Activate */
                        Term_activate(&td->t);

                        /* Resize the term */
                        Term_resize(td->cols, td->rows, td->rows);

                        /* Redraw later */
                        InvalidateRect(td->w, NULL, true);
                    }

                    td->size_hack = true;

                    /* Show sub-windows */
                    for (i = 1; i < MAX_TERM_DATA; i++)
                    {
                        if (data[i].visible) ShowWindow(data[i].w, SW_SHOW);
                    }

                    td->size_hack = false;

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

            SelectPalette(hdc, hPal, false);

            i = RealizePalette(hdc);

            /* if any palette entries changed, repaint the window. */
            if (i) InvalidateRect(hWnd, NULL, true);

            ReleaseDC(hWnd, hdc);

            return 0;
        }

        case WM_ACTIVATE:
        {
            if (wParam && !HIWORD(lParam))
            {
                /* Do something to sub-windows */
                for (i = MAX_TERM_DATA - 1; i >= 0; i--)
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


static LRESULT FAR PASCAL AngbandListProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    term_data *td;
    HDC hdc;
    int i;
    char pmsg[60];

    /* Acquire proper "term_data" info */
    td = (term_data *)GetWindowLongPtr(hWnd, GWLP_USERDATA);

    /* Process message */
    switch (uMsg)
    {
        /* XXX XXX XXX */
        case WM_NCCREATE:
        {
            SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG)(my_td));
            break;
        }

        /* XXX XXX XXX */
        case WM_CREATE:
        {
            return 0;
        }

        case 0x133: /* WM_CTLCOLOREDIT */
        {
            SetTextColor((HDC)wParam, 0x00ffffff);
            SetBkColor((HDC)wParam, 0);
            return (int)hbrBlack;
        }

        case WM_CLOSE:
        {
            /* Which term is closing ? */
            for (i = 0; i < MAX_TERM_DATA; i++)
            {
                if (&data[i] == td)
                {
                    /* Click its menu entry */
                    process_menus(IDM_WINDOW_VIS_0 + i);
                    break;
                }
            }
            return 0;
        }

        case WM_GETMINMAXINFO:
        {
            MINMAXINFO FAR *lpmmi;
            RECT rc;
            uint wid, hgt;

            /* This message was sent before WM_NCCREATE */
            if (!td) return 1;

            lpmmi = (MINMAXINFO FAR *)lParam;

            /* Minimum window size is 16x4 */
            rc.left = rc.top = 0;
            window_size_wh(td, NORMAL_WID / 5, NORMAL_HGT / 5, &wid, &hgt);
            rc.right = rc.left + wid;
            rc.bottom = rc.top + hgt;

            /* Adjust */
            AdjustWindowRectEx(&rc, td->dwStyle, true, td->dwExStyle);

            /* Save minimum size */
            lpmmi->ptMinTrackSize.x = rc.right - rc.left;
            lpmmi->ptMinTrackSize.y = rc.bottom - rc.top;

            /* Maximum window size is 80x24 */
            rc.left = rc.top = 0;
            window_size_wh(td, NORMAL_WID, NORMAL_HGT, &wid, &hgt);
            rc.right = rc.left + wid;
            rc.bottom = rc.top + hgt;

            /* Paranoia */
            rc.right  += (td->tile_wid - 1);
            rc.bottom += (td->tile_hgt - 1);

            /* Adjust */
            AdjustWindowRectEx(&rc, td->dwStyle, true, td->dwExStyle);

            /* Save maximum size */
            lpmmi->ptMaxSize.x = rc.right - rc.left;
            lpmmi->ptMaxSize.y = rc.bottom - rc.top;

            /* Save maximum size */
            lpmmi->ptMaxTrackSize.x = rc.right - rc.left;
            lpmmi->ptMaxTrackSize.y = rc.bottom - rc.top;

            return 0;
        }

        case WM_EXITSIZEMOVE:
        {
            return 0;
        }

        case WM_SIZE:
        {
            uint cols, rows;

            /* This message was sent before WM_NCCREATE */
            if (!td) return 1;

            /* It was sent from inside CreateWindowEx */
            if (!td->w) return 1;

            /* Was sent from WM_SIZE */
            if (td->size_hack) return 1;

            td->size_hack = true;

            window_size_cr(td, &cols, &rows, LOWORD(lParam), HIWORD(lParam));

            /* New size */
            if (((uint)(td->cols) != cols) || ((uint)(td->rows) != rows))
            {
                /* Save old term */
                term *old_term = Term;

                /* Save the new size */
                td->cols = cols;
                td->rows = rows;

                /* Activate */
                Term_activate(&td->t);

                /* Resize the term */
                Term_resize(td->cols, td->rows, td->rows);

                /* Activate */
                Term_activate(old_term);

                /* Redraw later */
                InvalidateRect(td->w, NULL, true);

                if (td == &data[PMSG_TERM])
                {
                    uint wid, hgt;

                    window_size_wh(td, td->cols, td->rows, &wid, &hgt);
                    stretch_chat_ctrl_win(wid, hgt);
                }

                /* Hack -- redraw all windows */
                if (Setup.initialized) do_cmd_redraw();
            }

            td->size_hack = false;

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
            /* If this is term-7 we are sending a player message */
            if (td == &data[7])
            {
                /* Is this RETURN ? */
                if (wParam == 13)
                {
                    /* Get the controls text and send it */
                    GetWindowText(editmsg, pmsg, 59);
                    Send_msg(pmsg);
                }

                /* If not return, ignore key */
                return 0;
            }

            if (handle_keydown(wParam, lParam)) return 0;
            break;
        }

        case WM_CHAR:
        {
            Term_keypress(wParam, 0);
            return 0;
        }

        case WM_PALETTECHANGED:
        {
            /* Ignore if palette change caused by itself */
            if ((HWND)wParam == hWnd) return false;

            /* Fall through... */
        }

        case WM_QUERYNEWPALETTE:
        {
            if (!paletted) return 0;

            hdc = GetDC(hWnd);
            SelectPalette(hdc, hPal, false);
            i = RealizePalette(hdc);

            /* If any palette entries changed, repaint the window. */
            if (i) InvalidateRect(hWnd, NULL, true);
            ReleaseDC(hWnd, hdc);

            return 0;
        }

        case WM_NCLBUTTONDOWN:
        {
            if (wParam == HTSYSMENU)
            {
                /* Hide sub-windows */
                if ((td->visible) && (td != &data[0]))
                {
                    td->visible = false;
                    ShowWindow(td->w, SW_HIDE);

                    /* Chat window */
                    if (td == &data[PMSG_TERM]) term_chat->user = (void*)0;
                }

                return 0;
            }

            break;
        }
    }

    return DefWindowProc(hWnd, uMsg, wParam, lParam);
}


/*
 * Temporary Hooks
 */


/*
 * Display warning message (see "z-util.c")
 */
static void hack_plog(const char *str)
{
    /* Give a warning */
    if (str) MessageBox(NULL, str, "Warning", MB_ICONEXCLAMATION | MB_OK);
}


/*
 * Display error message and quit (see "z-util.c")
 */
static void hack_quit(const char *str)
{
    /* Give a warning */
    if (str)
        MessageBox(NULL, str, "Error", MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);

    /* Unregister the classes */
    UnregisterClass(AppName, hInstance);

    /* Destroy the icon */
    if (hIcon) DestroyIcon(hIcon);

    /* Free strings */
    string_free(argv0);

    textui_cleanup();
    cleanup_angband();

    /* Cleanup WinSock */
    WSACleanup();

    /* Exit */
    exit(0);
}


/*
 * Various hooks
 */


/*
 * Display warning message (see "z-util.c")
 */
static void hook_plog(const char *str)
{
    /* Warning */
    if (str) MessageBox(data[0].w, str, "Warning", MB_ICONEXCLAMATION | MB_OK);
}


/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(const char *str)
{
    int i;

    /* Give a warning */
    if (str)
        MessageBox(data[0].w, str, "Error", MB_ICONEXCLAMATION | MB_OK | MB_ICONSTOP);

    /* Save the preferences */
    save_prefs();

    /*** Could use 'Term_nuke_win()' XXX XXX XXX */

    /* Destroy all windows */
    for (i = MAX_TERM_DATA - 1; i >= 0; --i)
    {
        /* Remove all fonts from the system, free resources */
        if (data[i].font_file) term_remove_font(data[i].font_file);
        if (data[i].font_id) DeleteObject(data[i].font_id);
        string_free(data[i].font_want);

        /* Kill the window */
        if (data[i].w) DestroyWindow(data[i].w);
        data[i].w = 0;
        term_nuke(&data[i].t);
    }

    /* Free the bitmap stuff */
    FreeDIB(&infGraph);
    FreeDIB(&infMask);

    close_graphics_modes();

    /*** Free some other stuff ***/

    DeleteObject(hbrYellow);

    if (hPal) DeleteObject(hPal);

    UnregisterClass(AppName, hInstance);

    if (hIcon) DestroyIcon(hIcon);

    /* Free strings */
    string_free(argv0);

    textui_cleanup();
    cleanup_angband();

    /* Cleanup network stuff */
    Net_cleanup();

    /* Cleanup WinSock */
    WSACleanup();

    exit(0);
}


/*
 *  Initialize
 */


/*
 * Init some stuff
 */
static void init_stuff_win(void)
{
    int i;
    char path[MSG_LEN];

    /* Get program name with full path */
    if (GetModuleFileName(hInstance, path, sizeof(path)) == 0)
        show_win_error();

    /* Paranoia */
    path[sizeof(path) - 1] = '\0';

    /* Save the "program name" */
    argv0 = string_make(path);

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
    my_strcpy(path + i + 1, "lib\\", sizeof(path));

    /* Validate the path */
    validate_dir(path);

    /* Init the file paths */
    init_file_paths(path, path, path);

    /* Hack -- validate the paths */
    validate_dir(ANGBAND_DIR_CUSTOMIZE);
    validate_dir(ANGBAND_DIR_SCREENS);
    validate_dir(ANGBAND_DIR_FONTS);
    validate_dir(ANGBAND_DIR_TILES);
    validate_dir(ANGBAND_DIR_SOUNDS);
    validate_dir(ANGBAND_DIR_ICONS);
    validate_dir(ANGBAND_DIR_USER);

    /* Build the filename */
    path_build(path, sizeof(path), ANGBAND_DIR_FONTS, DEFAULT_FONT);

    /* Hack -- validate the basic font */
    validate_file(path);
}


int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR lpCmdLine, int nCmdShow)
{
    int i;
    WNDCLASS wc;
    HDC hdc;
    WSADATA wsadata;

    /* Initialize */
    if (hPrevInst == NULL)
    {
        wc.style         = CS_CLASSDC;
        wc.lpfnWndProc   = AngbandWndProc;
        wc.cbClsExtra    = 0;
        wc.cbWndExtra    = 4; /* one long pointer to term_data */
        wc.hInstance     = hInst;
        wc.hIcon         = hIcon = LoadIcon(hInst, "ANGBAND");
        wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
        wc.hbrBackground = GetStockObject(BLACK_BRUSH);
        wc.lpszMenuName  = "ANGBAND";
        wc.lpszClassName = AppName;

        if (!RegisterClass(&wc)) exit(1);

        wc.lpfnWndProc   = AngbandListProc;
        wc.lpszMenuName  = NULL;
        wc.lpszClassName = AngList;

        if (!RegisterClass(&wc)) exit(2);
    }

    /* Save globally */
    hInstance = hInst;

    /* Initialize WinSock */
    WSAStartup(MAKEWORD(1, 1), &wsadata);

    memset(&Setup, 0, sizeof(Setup));

    /* Temporary hooks */
    plog_aux = hack_plog;
    quit_aux = hack_quit;

    /* Global client config */
    conf_init(hInstance);

    /* Prepare the filepaths */
    init_stuff_win();

    /* Determine if display is 16/256/true color */
    hdc = GetDC(NULL);
    colors16 = (GetDeviceCaps(hdc, BITSPIXEL) == 4);
    paletted = ((GetDeviceCaps(hdc, RASTERCAPS) & RC_PALETTE)? true: false);
    ReleaseDC(NULL, hdc);

    /* Initialize the colors */
    for (i = 0; i < MAX_COLORS; i++)
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

    /* Load the possible graphics modes */
    if (!init_graphics_modes())
        plog_fmt("Graphics list load failed");

    /* Prepare the windows */
    init_windows();

    /* Activate hooks */
    plog_aux = hook_plog;
    quit_aux = hook_quit;

    /* Set the system suffix */
    ANGBAND_SYS = "win";

    /* We are now initialized */
    initialized = true;

    turn_off_numlock();

    /* Initialize everything, contact the server, and start the loop */
    client_init();

    /* Paranoia */
    quit(NULL);

    /* Paranoia */
    return (0);
}


/* Hack -- edit control subclass */
static LRESULT APIENTRY SubClassFunc(HWND hWnd, UINT Message, WPARAM wParam, LPARAM lParam)
{
    char pmsgbuf[1000]; /* overkill */
    char pmsg[60];
    char nickbuf[30];

    /* Allow ESCAPE to return focus to main window. */
    if ((Message == WM_KEYDOWN) && (wParam == VK_ESCAPE))
    {
        unset_chat_focus();
        return 0;
    }

    if (Message == WM_CHAR)
    {
        /* Is this RETURN ? */
        if (wParam == 13 || wParam == 10000)
        {
            int msglen = 0;
            memset(nickbuf, 0, sizeof(nickbuf));

            /* Get the controls text and send it */
            msglen = GetWindowText(editmsg, pmsgbuf, 999);

            /* Send the text in chunks of 58 characters, or nearest break before 58 chars */
            if (msglen == 0)
            {
                unset_chat_focus();
                return 0;
            }

            if (msglen < 58)
                Send_msg(pmsgbuf);
            else
            {
                int offset = 0, breakpoint, nicklen;
                char *startmsg;

                /* See if this was a privmsg, if so, pull off the nick */
                for (startmsg = pmsgbuf; *startmsg; startmsg++)
                {
                    if (*startmsg == ':') break;
                }
                if (*startmsg && (startmsg - pmsgbuf < 29))
                {
                    my_strcpy(nickbuf, pmsgbuf, (startmsg - pmsgbuf) + 2);
                    nicklen = strlen(nickbuf);
                    startmsg += 2;
                }
                else
                {
                    startmsg = pmsgbuf;
                    nicklen = 0;
                }

                /* Now deal with what's left */
                while (msglen > 0)
                {
                    memset(pmsg, 0, sizeof(pmsg));

                    if (msglen < (58 - nicklen))
                        breakpoint = msglen;
                    else
                    {
                        /* Try to find a breaking char */
                        for (breakpoint = 58 - nicklen; breakpoint > 0; breakpoint--)
                        {
                            if (startmsg[offset + breakpoint] == ' ') break;
                            if (startmsg[offset + breakpoint] == ',') break;
                            if (startmsg[offset + breakpoint] == '.') break;
                            if (startmsg[offset + breakpoint] == ';') break;
                        }
                        if (!breakpoint) breakpoint = 58 - nicklen; /* nope */
                    }

                    /* If we pulled off a nick above, prepend it. */
                    if (nicklen) my_strcpy(pmsg, nickbuf, nicklen + 1);

                    /* Stash in this part of the msg */
                    strncat(pmsg, startmsg + offset, breakpoint);
                    msglen -= breakpoint;
                    offset += breakpoint;
                    Send_msg(pmsg);
                    Net_flush();
                }
            }

            /* Clear the message box */
            pmsgbuf[0] = 0;
            SetWindowText(editmsg, pmsgbuf);
            unset_chat_focus();
            return 0;
        }
    }
    return CallWindowProc(lpfnOldWndProc, hWnd, Message, wParam, lParam);
}


/*** Externs ***/


/* Hack -- set focus to chat message control */
void set_chat_focus(void)
{
    old_focus = GetFocus();
    SetFocus(editmsg);
}


void unset_chat_focus(void)
{
    /* Set focus back to original window */
    if (old_focus) SetFocus(old_focus);
}
