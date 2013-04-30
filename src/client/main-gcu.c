/*
 * File: main-gcu.c
 * Purpose: Support for "curses" systems
 *
 * Copyright (c) 1997 Ben Harrison, and others
 * Copyright (c) 2009-2011 Erik Osheim
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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

#include "c-angband.h"
#include "netclient.h"

#ifdef USE_GCU
#include "main.h"

#undef MOUSE_MOVED

/*
 * Include the proper "header" file
 */
#include <curses.h>
#include <errno.h>

/*
 * The TERM environment variable; used for terminal capabilities.
 */
static char *termtype;
static bool loaded_terminfo;

/*
 * Information about a term
 */
typedef struct term_data
{
    term t;         /* All term info */
    WINDOW *win;    /* Pointer to the curses window */
} term_data;

/* Max number of windows on screen */
#define MAX_TERM_DATA 4

/* Information about our windows */
static term_data data[MAX_TERM_DATA];

/* Number of initialized "term" structures */
static int active = 0;

#ifdef A_COLOR

/*
 * Hack -- Define "A_BRIGHT" to be "A_BOLD", because on many
 * machines, "A_BRIGHT" produces ugly "inverse" video.
 */
#ifndef A_BRIGHT
# define A_BRIGHT A_BOLD
#endif

/*
 * Software flag -- We are allowed to use color
 */
static int can_use_color = FALSE;

/*
 * Simple Angband to Curses color conversion table
 */
static int colortable[BASIC_COLORS];

/* Screen info: use one big Term 0, or other subwindows? */
static bool use_big_screen = FALSE;
static bool bold_extended = FALSE;

/*
 * Background color we should draw with; either BLACK or DEFAULT
 */
static int bg_color = COLOR_BLACK;

#define PAIR_WHITE 0
#define PAIR_RED 1
#define PAIR_GREEN 2
#define PAIR_YELLOW 3
#define PAIR_BLUE 4
#define PAIR_MAGENTA 5
#define PAIR_CYAN 6
#define PAIR_BLACK 7

#endif


/*
 * Move the cursor to (y, x)
 */
static void gcu_move_cursor(term_data *td, int y, int x)
{
    /* Move the cursor */
    wmove(td->win, y, x);

    /* Move the physical cursor */
    if (td == &data[0]) wmove(stdscr, y, x);
}


/*
 * Init the "curses" system
 */
static void Term_init_gcu(term *t)
{
    term_data *td = (term_data *)(t->data);

    /*
     * This is necessary to keep the first call to getch()
     * from clearing the screen
     */
    wrefresh(stdscr);

    /* Count init's, handle first */
    if (active++ != 0) return;

    /* Erase the window */
    wclear(td->win);

    /* Reset the cursor */
    gcu_move_cursor(td, 0, 0);

    /* Flush changes */
    wrefresh(td->win);
}


/*
 * Nuke the "curses" system
 */
static void Term_nuke_gcu(term *t)
{
    int x, y;
    term_data *td = (term_data *)(t->data);

    /* Delete this window */
    delwin(td->win);

    /* Count nuke's, handle last */
    if (--active != 0) return;

    /* Hack -- Make sure the cursor is visible */
    Term_xtra(TERM_XTRA_SHAPE, 1);

#ifdef A_COLOR
    /* Reset colors to defaults */
    start_color();
#endif

    /* Get current cursor position */
    getyx(stdscr, y, x);

    /* Move the cursor to bottom right corner */
    mvcur(y, x, LINES - 1, 0);

    /* Flush the curses buffer */
    refresh();

    /* Exit curses */
    endwin();

    /* Flush the output */
    fflush(stdout);
}


/*
 * For a given term number (i) set the upper left corner (x, y) and the
 * correct dimensions. Terminal layout: 0|2
 *                                      1|3
 */
static void get_gcu_term_size(int i, int *rows, int *cols, int *y, int *x)
{
    if (use_big_screen && i == 0)
    {
        *rows = LINES;
        *cols = COLS;
        *y = *x = 0;
    }
    else if (use_big_screen)
        *rows = *cols = *y = *x = 0;
    else if (i == 0)
    {
        *rows = NORMAL_HGT;
        *cols = NORMAL_WID;
        *y = *x = 0;
    }
    else if (i == 1)
    {
        *rows = LINES - NORMAL_HGT - 1;
        *cols = NORMAL_WID;
        *y = NORMAL_HGT + 1;
        *x = 0;
    }
    else if (i == 2)
    {
        *rows = NORMAL_HGT;
        *cols = COLS - NORMAL_WID - 1;
        *y = 0;
        *x = NORMAL_WID + 1;
    }
    else if (i == 3)
    {
        *rows = LINES - NORMAL_HGT - 1;
        *cols = COLS - NORMAL_WID - 1;
        *y = NORMAL_HGT + 1;
        *x = NORMAL_WID + 1;
    }
    else
        *rows = *cols = *y = *x = 0;
}

#ifdef USE_NCURSES
/*
 * Query ncurses for new screen size and try to resize the GCU terms.
 */
static void do_gcu_resize(void)
{
    int i, rows, cols, y, x;
    term *old_t = Term;

    for (i = 0; i < MAX_TERM_DATA; i++)
    {
        /* If we're using a big screen, we only care about Term-0 */
        if (use_big_screen && i > 0) break;
        
        /* Activate the current Term */
        Term_activate(&data[i].t);

        /* If we can resize the curses window, then resize the Term */
        get_gcu_term_size(i, &rows, &cols, &y, &x);
        if (wresize(data[i].win, rows, cols) == OK)
            Term_resize(cols, rows, rows);

        /* Activate the old term */
        Term_activate(old_t);
    }
    do_cmd_redraw();
}
#endif

/*
 * Handle keypresses.
 */
static void gcu_keypress(int i)
{
    int j, k, mods = 0;

    /* Not sure if this is portable to non-ncurses platforms */
    #ifdef USE_NCURSES
    if (i == KEY_RESIZE)
    {
        /*
         * Wait until we go one second (10 deci-seconds) before actually
         * doing the resizing. users often end up triggering multiple
         * KEY_RESIZE events while changing window size.
         */
        halfdelay(10);
        do
        {
            i = getch(); //mvwgetch
        }
        while (i == KEY_RESIZE);
        nocbreak();
        cbreak();
        do_gcu_resize();
        if (i == ERR) return;
    }
    #endif

    /* uncomment to debug keycode issues */
    #if 0
    printw("key %d", i);
    wrefresh(stdscr);
    #endif

    /*
     * This might be a bad idea, but...
     *
     * Here we try to second-guess ncurses. In some cases, keypad() mode will
     * fail to translate multi-byte escape sequences into things like number-
     * pad actions, function keys, etc. So we can hardcode a small list of some
     * of the most common sequences here, just in case.
     *
     * Notice that we turn nodelay() on. This means, that we won't accidentally
     * interpret sequences as valid unless all the bytes are immediately
     * available; this seems like an acceptable risk to fix problems associated
     * with various terminal emulators (I'm looking at you PuTTY).
     */
    if (i == 27)
    {
        /* ESC */
        nodelay(stdscr, TRUE);
        j = getch();
        switch (j)
        {
            case 'O':
            {
                k = getch();
                switch (k)
                {
                    /* PuTTY number pad */
                    case 'q': i = '1'; break;
                    case 'r': i = '2'; break;
                    case 's': i = '3'; break;
                    case 't': i = '4'; break;
                    case 'u': i = '5'; break;
                    case 'v': i = '6'; break;
                    case 'w': i = '7'; break;
                    case 'x': i = '8'; break;
                    case 'y': i = '9'; break;

                    /* no match */
                    case ERR: break;
                    default: ungetch(k); ungetch(j);
                }
                break;
            }

            /* no match */
            case ERR: break;
            default: ungetch(j);
        }
        nodelay(stdscr, FALSE);
    }

#ifdef KEY_DOWN
    /* Handle special keys */
    switch (i)
    {
        /* Control keys */
        case 8: i = KC_BACKSPACE; break;
        case 9: i = KC_TAB; break;
        case 13: i = KC_ENTER; break;
        case 27: i = ESCAPE; break;

        /* Keypad keys */
        case 0xFC: i = '0'; break;
        case 0xFD: i = '.'; break;
        case 0xC0: i = '\b'; break;
        case 0xDF: i = '1'; break;
        case 0xF5: i = '3'; break;
        case 0xE9: i = '5'; break;
        case 0xC1: i = '7'; break;
        case 0xF4: i = '9'; break;

        /* Use these keys to handle shift-dir when NUMLOCK is off */
        case 0x31: i = '1'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;
        case 0x32: i = '2'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;
        case 0x33: i = '3'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;
        case 0x34: i = '4'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;
        case 0x35: i = '5'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;
        case 0x36: i = '6'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;
        case 0x37: i = '7'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;
        case 0x38: i = '8'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;
        case 0x39: i = '9'; mods |= (KC_MOD_SHIFT | KC_MOD_KEYPAD); break;

        /* Key definitions, part 1 */
        case KEY_DOWN: i = ARROW_DOWN; break;
        case KEY_UP: i = ARROW_UP; break;
        case KEY_LEFT: i = ARROW_LEFT; break;
        case KEY_RIGHT: i = ARROW_RIGHT; break;
        case KEY_HOME: i = KC_HOME; break;
        case KEY_BACKSPACE: i = KC_BACKSPACE; break;

        /* Key definitions, part 2 */
        case KEY_DC: i = KC_DELETE; break;
        case KEY_IC: i = KC_INSERT; break;
        case KEY_NPAGE: i = KC_PGDOWN; break;
        case KEY_PPAGE: i = KC_PGUP; break;
        case KEY_ENTER: i = KC_ENTER; mods |= KC_MOD_KEYPAD; break;
        case KEY_END: i = KC_END; break;
        case KEY_SDC: i = KC_DELETE; mods |= KC_MOD_SHIFT; break;
        case KEY_SEND: i = KC_END; mods |= KC_MOD_SHIFT; break;
        case KEY_SHOME: i = KC_HOME; mods |= KC_MOD_SHIFT; break;
        case KEY_SIC: i = KC_INSERT; mods |= KC_MOD_SHIFT; break;

        /* Key definitions, part 3 */
        case KEY_SLEFT: i = ARROW_LEFT; mods |= KC_MOD_SHIFT; break;
        case KEY_SNEXT: i = KC_PGDOWN; mods |= KC_MOD_SHIFT; break;
        case KEY_SPREVIOUS: i = KC_PGUP; mods |= KC_MOD_SHIFT; break;
        case KEY_SRIGHT: i = ARROW_RIGHT; mods |= KC_MOD_SHIFT; break;

        /* Key definitions, part 4 */
        /*case CTL_LEFT: i = ARROW_LEFT; mods |= KC_MOD_CONTROL; break;
        case CTL_RIGHT: i = ARROW_RIGHT; mods |= KC_MOD_CONTROL; break;
        case CTL_PGUP: i = KC_PGUP; mods |= KC_MOD_CONTROL; break;
        case CTL_PGDN: i = KC_PGDOWN; mods |= KC_MOD_CONTROL; break;
        case CTL_HOME: i = KC_HOME; mods |= KC_MOD_CONTROL; break;
        case CTL_END: i = KC_END; mods |= KC_MOD_CONTROL; break;*/

        /* Virtual keypad keys (NUMLOCK off) */
        /*case KEY_A1: i = '7'; mods |= KC_MOD_KEYPAD; break;
        case KEY_A2: i = '8'; mods |= KC_MOD_KEYPAD; break;
        case KEY_A3: i = '9'; mods |= KC_MOD_KEYPAD; break;
        case KEY_B1: i = '4'; mods |= KC_MOD_KEYPAD; break;
        case KEY_B2: i = '5'; mods |= KC_MOD_KEYPAD; break;
        case KEY_B3: i = '6'; mods |= KC_MOD_KEYPAD; break;
        case KEY_C1: i = '1'; mods |= KC_MOD_KEYPAD; break;
        case KEY_C2: i = '2'; mods |= KC_MOD_KEYPAD; break;
        case KEY_C3: i = '3'; mods |= KC_MOD_KEYPAD; break;*/

        /* Keypad keys */
        /*case PADSLASH: i = '/'; mods |= KC_MOD_KEYPAD; break;
        case PADENTER: i = KC_ENTER; mods |= KC_MOD_KEYPAD; break;
        case PADSTOP: i = '.'; mods |= KC_MOD_KEYPAD; break;
        case PADSTAR: i = '*'; mods |= KC_MOD_KEYPAD; break;
        case PADMINUS: i = '-'; mods |= KC_MOD_KEYPAD; break;
        case PADPLUS: i = '+'; mods |= KC_MOD_KEYPAD; break;*/

        /* Key definitions, part 5 */
        /*case CTL_UP: i = ARROW_UP; mods |= KC_MOD_CONTROL; break;
        case CTL_DOWN: i = ARROW_DOWN; mods |= KC_MOD_CONTROL; break;
        case PAD0: i = '0'; mods |= KC_MOD_KEYPAD; break;*/

        /* Key definitions, part 6 */
        /*case CTL_PAD0: i = '0'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD1: i = '1'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD2: i = '2'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD3: i = '3'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD4: i = '4'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD5: i = '5'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD6: i = '6'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD7: i = '7'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD8: i = '8'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;
        case CTL_PAD9: i = '9'; mods |= (KC_MOD_CONTROL | KC_MOD_KEYPAD); break;*/

        /* Key definitions, part 7 */
        /*case KEY_SUP: i = ARROW_UP; mods |= KC_MOD_SHIFT; break;
        case KEY_SDOWN: i = ARROW_DOWN; mods |= KC_MOD_SHIFT; break;*/

        default:
        {
            if (i < KEY_MIN) break;

            /* Mega-Hack -- Fold, spindle, and mutilate the keys to fit in 7 bits. */
            if (i >= 252) i = KEY_F(63) - (i - 252);
            if (i >= ARROW_DOWN) i += 4;

            i = 128 + (i & 127);
            break;
        }
    }
#endif

    /* Enqueue the keypress */
    Term_keypress(i, mods);
}


/*
 * Waits indefinitely for the next available event
 */
static errr gcu_WaitEvent(int *pk)
{
    /*
     * Wait for a keypress; use halfdelay(1) so if the user takes more
     * than 0.2 seconds we get a chance to do updates.
     */
    halfdelay(2);
    *pk = getch();
    while (*pk == ERR) *pk = getch();
    nocbreak();
    cbreak();
    return (1);
}


/*
 * Polls for currently pending events
 *
 * Returns 1 if there are any pending events, or 0 if there are none available.
 */
static errr gcu_PollEvent(int *pk)
{
    /* Do not wait for it */
    nodelay(stdscr, TRUE);

    /* Check for keypresses */
    *pk = getch();

    /* Wait for it next time */
    nodelay(stdscr, FALSE);

    /* None ready */
    if (*pk == ERR) return (0);
    if (*pk == EOF) return (0);

    return (1);
}


/*
 * Process events, with optional wait
 */
static errr Term_xtra_gcu_event(int v)
{
    int key;

    /* Wait for an event */
    if (v)
    {
        /* Wait for an event */
        if (gcu_WaitEvent(&key))
        {
            /* Handle it */
            gcu_keypress(key);
        }
        else return (1);
    }
    else
    {
        /* Wait for an event */
        if (gcu_PollEvent(&key))
        {
            /* Handle it */
            gcu_keypress(key);
        }
    }

    /* Success */
    return (0);
}


static errr Term_xtra_gcu_flush(void)
{
    int key;

    /* Get all pending events */
    while (gcu_PollEvent(&key))
    {
        /* Handle them (ignore errors) */
        gcu_keypress(key);
    }

    /* Done */
    return (0);
}


static int scale_color(int i, int j, int scale)
{
    return (angband_color_table[i][j] * (scale - 1) + 127) / 255;
}


static int create_color(int i, int scale)
{
    int r = scale_color(i, 1, scale);
    int g = scale_color(i, 2, scale);
    int b = scale_color(i, 3, scale);
    int rgb = 16 + scale * scale * r + scale * g + b;

    /* In the case of white and black we need to use the ANSI colors */
    if (r == g && g == b)
    {
        if (b == 0) rgb = 0;
        if (b == scale) rgb = 15;
    }

    return rgb;
}


/*
 * React to changes
 */
static errr Term_xtra_gcu_react(void)
{
#ifdef A_COLOR
    if (COLORS == 256 || COLORS == 88)
    {
        /*
         * If we have more than 16 colors, find the best matches. These numbers
         * correspond to xterm/rxvt's builtin color numbers--they do not
         * correspond to curses' constants OR with curses' color pairs.
         *
         * XTerm has 216 (6*6*6) RGB colors, with each RGB setting 0-5.
         * RXVT has 64 (4*4*4) RGB colors, with each RGB setting 0-3.
         *
         * Both also have the basic 16 ANSI colors, plus some extra grayscale
         * colors which we do not use.
         */
        int i;
        int scale = ((COLORS == 256)? 6: 4);

        for (i = 0; i < BASIC_COLORS; i++)
        {
            int fg = create_color(i, scale);
            int isbold = (bold_extended? A_BRIGHT: A_NORMAL);

            init_pair(i + 1, fg, bg_color);
            colortable[i] = COLOR_PAIR(i + 1) | isbold;
        }
    }
#endif

    return 0;
}


/*
 * Handle a "special request"
 */
static errr Term_xtra_gcu(int n, int v)
{
    term_data *td = (term_data *)(Term->data);

    /* Analyze the request */
    switch (n)
    {
        /* Clear screen */
        case TERM_XTRA_CLEAR: touchwin(td->win); wclear(td->win); return 0;

        /* Flush the Curses buffer */
        case TERM_XTRA_FRESH: wrefresh(td->win); return 0;

        /* Change the cursor visibility */
        case TERM_XTRA_SHAPE: curs_set(v); return 0;

        /* Process events */
        case TERM_XTRA_EVENT: return Term_xtra_gcu_event(v);

        /* Flush events */
        case TERM_XTRA_FLUSH: return Term_xtra_gcu_flush();

        /* Delay */
        case TERM_XTRA_DELAY: if (v > 0) Sleep(v); return 0;

        /* React to events */
        case TERM_XTRA_REACT: Term_xtra_gcu_react(); return 0;
    }

    /* Unknown event */
    return 1;
}


/*
 * Actually MOVE the hardware cursor
 */
static errr Term_curs_gcu(int x, int y)
{
    term_data *td = (term_data *)(Term->data);

    gcu_move_cursor(td, y, x);
    return 0;
}


/*
 * Erase a grid of space
 * Hack -- Try to be "semi-efficient".
 */
static errr Term_wipe_gcu(int x, int y, int n)
{
    term_data *td = (term_data *)(Term->data);

    gcu_move_cursor(td, y, x);

    /* Clear to end of line */
    if (x + n >= td->t.wid) wclrtoeol(td->win);

    /* Clear some characters */
    else whline(td->win, ' ', n);

    return 0;
}


/*
 * Place some text on the screen using an attribute
 */
static errr Term_text_gcu(int x, int y, int n, byte a, const char *s)
{
    term_data *td = (term_data *)(Term->data);
    wchar_t buf[NORMAL_WID + 1];
    int i;

    mbstowcs(buf, s, n);

    /* Hack -- Replace magma/quartz by semi-solid blocks */
    for (i = 0; i < n; i++)
    {
        if (buf[i] == 0xAE) buf[i] = 0x2591;
    }

    /* Hack -- Full icky screen */
    if (full_icky_screen)
    {
        for (i = 0; i <= n; i++)
        {
            int px = x + i + p_ptr->offset_x - COL_MAP, py = y + p_ptr->offset_y - ROW_MAP;

            /*
             * Hack -- For the minimap, the @ may not be displayed with this function so check the
             * position instead and add a "white smiling face" at player location
             */
            if (i == n)
            {
                if (Term->minimap_active && (px == p_ptr->px) && (py == p_ptr->py))
                    buf[n++] = 0x263A;
            }

            /* Hack -- Display the @ as a "white smiling face" on icky screens */
            else if (p_ptr->screen_icky)
            {
                if (buf[i] == '@') buf[i] = 0x263A;
            }

            /* Hack -- Always display the player as a @ except when looking/targeting */
            else if ((px == p_ptr->px) && (py == p_ptr->py) && !target_icky_screen)
                buf[i] = '@';
        }
    }

#ifdef A_COLOR
    if (can_use_color)
    {
        /* The lower 7 bits of the attribute indicate the fg/bg */
        int attr = a & 127;

        /* The high bit of the attribute indicates a reversed fg/bg */
        int flip = ((a > 127)? A_REVERSE: A_NORMAL);

        wattrset(td->win, colortable[attr] | flip);
        mvwaddnwstr(td->win, y, x, buf, n);
        wattrset(td->win, A_NORMAL);

        return 0;
    }
#endif

    mvwaddnwstr(td->win, y, x, buf, n);
    return 0;
}


/*
 * Create a window for the given "term_data" argument.
 *
 * Assumes legal arguments.
 */
static errr term_data_init_gcu(term_data *td, int rows, int cols, int y, int x)
{
    term *t = &td->t;

    /* Create new window */
    td->win = newwin(rows, cols, y, x);

    /* Check for failure */
    if (!td->win) quit("Failed to setup curses window.");

    /* Initialize the term */
    term_init(t, cols, rows, rows, 256);

    /* Avoid bottom right corner */
    t->icky_corner = TRUE;

    /* Erase with "white space" */
    t->attr_blank = TERM_WHITE;
    t->char_blank = ' ';

    /* Differentiate between BS/^h, Tab/^i, etc. */
    t->complex_input = TRUE;

    /* Set some hooks */
    t->init_hook = Term_init_gcu;
    t->nuke_hook = Term_nuke_gcu;

    /* Set some more hooks */
    t->text_hook = Term_text_gcu;
    t->wipe_hook = Term_wipe_gcu;
    t->curs_hook = Term_curs_gcu;
    t->xtra_hook = Term_xtra_gcu;

    /* Save the data */
    t->data = td;

    /* Activate it */
    Term_activate(t);

    /* Success */
    return (0);
}


/*
 * Display warning message (see "z-util.c")
 */
static void hook_plog(const char *str)
{
    /* Warning */
    if (str) MessageBox(NULL, str, "Warning", MB_ICONEXCLAMATION | MB_OK);
}


static void hook_quit(const char *str)
{
    static bool quitting = FALSE;

    /* Don't re-enter if already quitting */
    if (quitting) return;
    quitting = TRUE;

    endwin();

    /* Free resources */
    cleanup_angband();

    /* Cleanup network stuff */
    Net_cleanup();

    /* Cleanup WinSock */
    WSACleanup();
}


static BOOL CtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType)
    {
        case CTRL_CLOSE_EVENT:
            quit(NULL);
            return FALSE;
        default:
            return FALSE;
    }
}


/*
 * Prepare "curses" for use by the file "z-term.c"
 *
 * Installs the "hook" functions defined above, and then activates
 * the main screen "term", which clears the screen and such things.
 *
 * Someone should really check the semantics of "initscr()"
 */
errr init_gcu(void)
{
    int i;
    int rows, cols, y, x;
    int next_win = 0;

    /* Initialize info about terminal capabilities */
    termtype = getenv("TERM");
    loaded_terminfo = termtype && tgetent(0, termtype) == 1;

    /* We do it like this to prevent a link error with curseses that lack ESCDELAY. */
    if (!getenv("ESCDELAY")) putenv("ESCDELAY=20");

    /* Initialize */
    if (initscr() == NULL) return (-1);

    /* Activate hooks */
    plog_aux = hook_plog;
    quit_aux = hook_quit;

    /* Register a control handler */
    if (!SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE))
        quit("Could not set control handler");

    /* Require standard size screen */
    if (LINES < NORMAL_HGT || COLS < NORMAL_WID)
        quit("Angband needs at least an 80x24 'curses' screen");

#ifdef A_COLOR
    /* Do we have color, and enough color, available? */
    can_use_color = (has_colors() && (start_color() != ERR) && (COLORS >= 8) && (COLOR_PAIRS >= 8));

#ifdef HAVE_USE_DEFAULT_COLORS
    /* Should we use curses' "default color" */
    if (use_default_colors() == OK) bg_color = -1;
#endif

    /* Attempt to use colors */
    if (can_use_color)
    {
        /* Prepare the color pairs */
        /* PAIR_WHITE (pair 0) is *always* WHITE on BLACK */
        init_pair(PAIR_RED, COLOR_RED, bg_color);
        init_pair(PAIR_GREEN, COLOR_GREEN, bg_color);
        init_pair(PAIR_YELLOW, COLOR_YELLOW, bg_color);
        init_pair(PAIR_BLUE, COLOR_BLUE, bg_color);
        init_pair(PAIR_MAGENTA, COLOR_MAGENTA, bg_color);
        init_pair(PAIR_CYAN, COLOR_CYAN, bg_color);
        init_pair(PAIR_BLACK, COLOR_BLACK, bg_color);

        /* Prepare the colors */
        colortable[TERM_DARK]     = (COLOR_PAIR(PAIR_BLACK));
        colortable[TERM_WHITE]    = (COLOR_PAIR(PAIR_WHITE) | A_BRIGHT);
        colortable[TERM_SLATE]    = (COLOR_PAIR(PAIR_WHITE));
        colortable[TERM_ORANGE]   = (COLOR_PAIR(PAIR_YELLOW));
        colortable[TERM_RED]      = (COLOR_PAIR(PAIR_RED));
        colortable[TERM_GREEN]    = (COLOR_PAIR(PAIR_GREEN));
        colortable[TERM_BLUE]     = (COLOR_PAIR(PAIR_BLUE));
        colortable[TERM_UMBER]    = (COLOR_PAIR(PAIR_YELLOW));
        colortable[TERM_L_DARK]   = (COLOR_PAIR(PAIR_BLACK) | A_BRIGHT);
        colortable[TERM_L_WHITE]  = (COLOR_PAIR(PAIR_WHITE));
        colortable[TERM_L_PURPLE] = (COLOR_PAIR(PAIR_MAGENTA) | A_BRIGHT);
        colortable[TERM_YELLOW]   = (COLOR_PAIR(PAIR_YELLOW) | A_BRIGHT);
        colortable[TERM_L_RED]    = (COLOR_PAIR(PAIR_RED) | A_BRIGHT);
        colortable[TERM_L_GREEN]  = (COLOR_PAIR(PAIR_GREEN) | A_BRIGHT);
        colortable[TERM_L_BLUE]   = (COLOR_PAIR(PAIR_BLUE) | A_BRIGHT);
        colortable[TERM_L_UMBER]  = (COLOR_PAIR(PAIR_YELLOW));

        colortable[TERM_PURPLE]      = (COLOR_PAIR(PAIR_MAGENTA));
        colortable[TERM_VIOLET]      = (COLOR_PAIR(PAIR_MAGENTA));
        colortable[TERM_TEAL]        = (COLOR_PAIR(PAIR_CYAN));
        colortable[TERM_MUD]         = (COLOR_PAIR(PAIR_YELLOW));
        colortable[TERM_L_YELLOW]    = (COLOR_PAIR(PAIR_YELLOW) | A_BRIGHT);
        colortable[TERM_MAGENTA]     = (COLOR_PAIR(PAIR_MAGENTA));
        colortable[TERM_L_TEAL]      = (COLOR_PAIR(PAIR_CYAN) | A_BRIGHT);
        colortable[TERM_L_VIOLET]    = (COLOR_PAIR(PAIR_MAGENTA) | A_BRIGHT);
        colortable[TERM_L_PINK]      = (COLOR_PAIR(PAIR_MAGENTA) | A_BRIGHT);
        colortable[TERM_MUSTARD]     = (COLOR_PAIR(PAIR_YELLOW) | A_BRIGHT);
        colortable[TERM_BLUE_SLATE]  = (COLOR_PAIR(PAIR_CYAN) | A_BRIGHT);
        colortable[TERM_DEEP_L_BLUE] = (COLOR_PAIR(PAIR_CYAN));
    }
#endif

    /* Paranoia -- Assume no waiting */
    nodelay(stdscr, FALSE);

    /* Prepare */
    cbreak();
    noecho();
    nonl();

    /* Tell curses to rewrite escape sequences to KEY_UP and friends */
    keypad(stdscr, TRUE);

    /* Now prepare the term(s) */
    for (i = 0; i < MAX_TERM_DATA; i++)
    {
        if (use_big_screen && i > 0) break;

        /*
         * Get the terminal dimensions; if the user asked for a big screen
         * then we'll put the whole screen in term 0; otherwise we'll divide
         * it amongst the available terms
         */
        get_gcu_term_size(i, &rows, &cols, &y, &x);

        /* Skip non-existant windows */
        if (rows <= 0 || cols <= 0) continue;

        /* Create a term */
        term_data_init_gcu(&data[next_win], rows, cols, y, x);
        
        /* Remember the term */
        angband_term[next_win] = &data[next_win].t;
        
        /* One more window */
        next_win++;
    }

    /* Activate the "Angband" window screen */
    Term_activate(&data[0].t);

    /* Remember the active screen */
    term_screen = &data[0].t;

    /* Success */
    return (0);
}

#endif /* USE_GCU */


