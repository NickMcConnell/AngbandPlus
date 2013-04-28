/* File: main-gcu.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */


/*
 * This file is updated for Sangband by Christer Nyfalt.
 */

/*
 * This file helps Angband run on Unix/Curses machines.
 *
 *
 * To use this file, you must define "USE_GCU" in the Makefile.
 *
 *
 * Hack -- note that "angband.h" is included AFTER the #ifdef test.
 * This was necessary because of annoying "curses.h" silliness.
 *
 * Note that this file is not "intended" to support non-Unix machines,
 * nor is it intended to support VMS or other bizarre setups.
 *
 * Also, this package assumes that the underlying "curses" handles both
 * the "nonl()" and "cbreak()" commands correctly, see the "OPTION" below.
 *
 * This code should work with most versions of "curses" or "ncurses",
 * and the "main-ncu.c" file (and USE_NCU define) are no longer used.
 *
 * This file provides up to 4 term windows.
 *
 * This file will attempt to redefine the screen colors to conform to
 * standard Angband colors.  It will only do so if the terminal type
 * indicates that it can do so.
 *
 * Consider the use of "savetty()" and "resetty()".  XXX XXX XXX
 *
 * This file now requires POSIX.1 termios support from Unix systems.
 * If your system is not yet POSIX compatible, upgrade your OS!
 * There is no excuse to run an OS that is 15 years outdated!
 */


#include "angband.h"


#ifdef USE_GCU

/*
 * Hack -- play games with "bool"
 */
#undef bool

/*
 * Include the proper "header" file
 */
#ifdef USE_NCURSES
# include <ncurses.h>
#else
# include <curses.h>
#endif

/*
 * Try redefining the colors at startup.
 */
#define REDEFINE_COLORS


/*
 * Use POSIX terminal I/O
 */
#define USE_TPOSIX

/*
 * Hack -- Amiga uses "fake curses" and cannot do any of this stuff
 */
#if defined(AMIGA)
# undef USE_TPOSIX
#endif

/*
 * Hack -- Windows Console mode uses PDCURSES and cannot do any terminal stuff
 * Hack -- Windows needs Sleep(), and I really don't want to pull in all
 *         the Win32 headers for this one function
 */
#if defined(WIN32_CONSOLE_MODE)
# undef USE_TPOSIX
_stdcall void Sleep(int);
#define usleep(v) Sleep(v / 1000)
#endif

/*
 * POSIX stuff
 */
#ifdef USE_TPOSIX
# include <termios.h>
#endif

/*
 * OPTION: Use old BSD curses.
 *
 * Some versions of curses does things a bit differently.
 *
 * If you have an old BSD 4.4 version of curses that isn't
 * XSI Curses standard compatible enable this.
 */
/* #define USE_OLD_CURSES */

/*
 * Turn on options that are available in modern curses.
 */
#if defined (USE_OLD_CURSES)
/* Nothing */
#else
# define USE_GETCH
# define USE_CURS_SET
#endif

/*
 * You might want to turn on some of the options below this line
 * if you had to turn on USE_OLD_CURSES
 */


/*
 * OPTION: Use "blocking getch() calls" in "main-gcu.c".
 */
/* #define USE_GETCH */


/*
 * OPTION: Use the "curs_set()" call in "main-gcu.c".
 */
/* #define USE_CURS_SET */


/*
 * OPTION: some machines lack "cbreak()"
 * On these machines, we use an older definition
 */
/* #define cbreak() crmode() */


/*
 * OPTION: some machines cannot handle "nonl()" and "nl()"
 * On these machines, we can simply ignore those commands.
 */
/* #define nonl() */
/* #define nl() */


/*
 * Save the "normal" and "angband" terminal settings
 */

#ifdef USE_TPOSIX

static struct termios  norm_termios;

static struct termios  game_termios;

#endif


static bool           split_window=TRUE;


/*
 * Information about a term
 */
typedef struct term_data term_data;

struct term_data
{
	term t;                 /* All term info */

	WINDOW *win;            /* Pointer to the curses window */

	bool active;            /* The window is active */
};

/* Max number of windows on screen, 2x2 + 1 unused for map */
#define MAX_TERM_DATA 5

/* minimum dimensions to split into multiple windows */
/* add 1 for spacing between the windows */
#define MIN_ROWS_SPLIT (term_size_min[WINDOW_DISPLAY][1] + term_size_min[TERM_SUBWINDOW][1] + 1)
#define MIN_COLS_SPLIT (term_size_min[WINDOW_DISPLAY][0] + term_size_min[TERM_SUBWINDOW][0] + 1)

/* Information about our windows */
static term_data data[MAX_TERM_DATA];

/*
 * Hack -- the old screen maximum size
 */
static int y_max, x_max;

/*
 * Hack -- it's safe to run the resize code.
 */
static bool resize_safe = FALSE;

/*
 * Hack -- Number of initialized "term" structures
 */
static int active = 0;


#ifdef A_COLOR

/*
 * Hack -- define "A_BRIGHT" to be "A_BOLD", because on many
 * machines, "A_BRIGHT" produces ugly "inverse" video.
 */
#ifndef A_BRIGHT
# define A_BRIGHT A_BOLD
#endif

/*
 * Software flag -- we are allowed to use color
 */
static int can_use_color = FALSE;

/*
 * Software flag -- we are allowed to change the colors
 */
static int can_fix_color = FALSE;

/*
 * Simple Angband to Curses color conversion table
 */
static int colortable[16];

#endif /* A_COLOR */


/*
 * Place the "keymap" into its "normal" state
 */
static void keymap_norm(void)
{

#ifdef USE_TPOSIX

	/* restore the saved values of the special chars */
	(void)tcsetattr(0, TCSAFLUSH, &norm_termios);

#endif

}


/*
 * Place the "keymap" into the "game" state
 */
static void keymap_game(void)
{

#ifdef USE_TPOSIX

	/* restore the saved values of the special chars */
	(void)tcsetattr(0, TCSAFLUSH, &game_termios);

#endif

}


/*
 * Save the normal keymap
 */
static void keymap_norm_prepare(void)
{

#ifdef USE_TPOSIX

	/* Get the normal keymap */
	tcgetattr(0, &norm_termios);

#endif

}


/*
 * Save the keymaps (normal and game)
 */
static void keymap_game_prepare(void)
{

#ifdef USE_TPOSIX

	/* Acquire the current mapping */
	tcgetattr(0, &game_termios);

	/* Force "Ctrl-C" to interupt */
	game_termios.c_cc[VINTR] = (char)3;

	/* Force "Ctrl-Z" to suspend */
	game_termios.c_cc[VSUSP] = (char)26;

	/* Hack -- Leave "VSTART/VSTOP" alone */

	/* Disable the standard control characters */
	game_termios.c_cc[VQUIT] = (char)-1;
	game_termios.c_cc[VERASE] = (char)-1;
	game_termios.c_cc[VKILL] = (char)-1;
	game_termios.c_cc[VEOF] = (char)-1;
	game_termios.c_cc[VEOL] = (char)-1;

	/* Normally, block until a character is read */
	game_termios.c_cc[VMIN] = 1;
	game_termios.c_cc[VTIME] = 0;

#endif

}




/*
 * Suspend/Resume
 */
static errr Term_xtra_gcu_alive(int v)
{
	int x, y;


	/* Suspend */
	if (!v)
	{
		/* Go to normal keymap mode */
		keymap_norm();

		/* Restore modes */
		nocbreak();
		echo();
		nl();

		/* Hack -- make sure the cursor is visible */
		(void)Term_xtra(TERM_XTRA_SHAPE, 1);

		/* Flush the curses buffer */
		(void)refresh();

		/* Get current cursor position */
		getyx(curscr, y, x);

		/* Move the cursor to bottom right corner */
		mvcur(y, x, LINES - 1, 0);

		/* Exit curses */
		endwin();

		/* Flush the output */
		(void)fflush(stdout);
	}

	/* Resume */
	else
	{
		/* Refresh */
		/* (void)touchwin(curscr); */
		/* (void)wrefresh(curscr); */

		/* Restore the settings */
		cbreak();
		noecho();
		nonl();

		/* Go to angband keymap mode */
		keymap_game();
	}

	/* Success */
	return (0);
}


const char help_gcu[] = "Curses Visual Display Support";

/*
 * Init the "curses" system
 */
static void Term_init_gcu(term *t)
{
	term_data *td = (term_data *)(t->data);

#ifdef USE_GETCH
	/*
	 * This is necessary to keep the first call to getch()
	 * from clearing the screen
	 */
	wrefresh(stdscr);
#endif /* USE_GETCH */

	/* Count init's, handle first */
	if (active++ != 0) return;

	/* Erase the window */
	(void)wclear(td->win);

	/* Reset the cursor */
	(void)wmove(td->win, 0, 0);

	/* Flush changes */
	(void)wrefresh(td->win);

	/* Game keymap */
	keymap_game();
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

	/* Hack -- make sure the cursor is visible */
	(void)Term_xtra(TERM_XTRA_SHAPE, 1);

#ifdef A_COLOR
	/* Reset colors to defaults */
	start_color();
#endif

	/* Get current cursor position */
	getyx(curscr, y, x);

	/* Move the cursor to bottom right corner */
	mvcur(y, x, LINES - 1, 0);

	/* Flush the curses buffer */
	(void)refresh();

	/* Exit curses */
	endwin();

	/* Flush the output */
	(void)fflush(stdout);

	/* Normal keymap */
	keymap_norm();
}




#ifdef USE_GETCH

/*
 * Process events, with optional wait
 */
static errr Term_xtra_gcu_event(int v)
{
	int i, k;

	/* Wait */
	if (v)
	{
		/* Paranoia -- Wait for it */
		nodelay(stdscr, FALSE);

		/* Get a keypress */
		i = getch();

		/* Mega-Hack -- allow graceful "suspend" */
		for (k = 0; (k < 10) && (i == ERR); k++) i = getch();

		/* Broken input is special */
		if (i == ERR) exit_game_panic();
		if (i == EOF) exit_game_panic();
	}

	/* Do not wait */
	else
	{
		/* Do not wait for it */
		nodelay(stdscr, TRUE);

		/* Check for keypresses */
		i = getch();

		/* Wait for it next time */
		nodelay(stdscr, FALSE);

		/* None ready */
		if (i == ERR) return (1);
		if (i == EOF) return (1);
	}

	/* Enqueue the keypress */
	(void)Term_keypress(i);

	/* Success */
	return (0);
}

#else	/* USE_GETCH */

/*
 * Process events (with optional wait)
 */
static errr Term_xtra_gcu_event(int v)
{
	int i, k;

	char buf[2];

	/* Wait */
	if (v)
	{
		/* Wait for one byte */
		i = read(0, buf, 1);

		/* Hack -- Handle bizarre "errors" */
		if ((i <= 0) && (errno != EINTR)) exit_game_panic();
	}

	/* Do not wait */
	else
	{
		/* Get the current flags for stdin */
		k = fcntl(0, F_GETFL, 0);

		/* Oops */
		if (k < 0) return (1);

		/* Tell stdin not to block */
		if (fcntl(0, F_SETFL, k | O_NONBLOCK) < 0) return (1);

		/* Read one byte, if possible */
		i = read(0, buf, 1);

		/* Replace the flags for stdin */
		if (fcntl(0, F_SETFL, k)) return (1);
	}

	/* Ignore "invalid" keys */
	if ((i != 1) || (!buf[0])) return (1);

	/* Enqueue the keypress */
	Term_keypress(buf[0]);

	/* Success */
	return (0);
}

#endif	/* USE_GETCH */

/*
 * React to changes
 */
static errr Term_xtra_gcu_react(void)
{

#ifdef A_COLOR

	int i;

	/* Cannot handle color redefinition */
	if (!can_fix_color) return (0);

	/* Set the colors */
	for (i = 0; i < 16; i++)
	{
		/* Set one color (note scaling) */
		init_color(i, color_table[i].rv * 1000 / 255,
		              color_table[i].gv * 1000 / 255,
		              color_table[i].bv * 1000 / 255);
	}

#endif /* A_COLOR */

	/* Success */
	return (0);
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
		case TERM_XTRA_CLEAR:
		touchwin(td->win);
		(void)wclear(td->win);
		return (0);

		/* Make a noise */
		case TERM_XTRA_NOISE:
		(void)write(1, "\007", 1);
		return (0);

		/* Flush the Curses buffer */
		case TERM_XTRA_FRESH:
		(void)wrefresh(td->win);
		return (0);

#ifdef USE_CURS_SET

		/* Change the cursor visibility */
		case TERM_XTRA_SHAPE:
		curs_set(v);
		return (0);

#endif

		/* Suspend/Resume curses */
		case TERM_XTRA_ALIVE:
		return (Term_xtra_gcu_alive(v));

		/* Process events */
		case TERM_XTRA_EVENT:
		return (Term_xtra_gcu_event(v));

		/* Flush events */
		case TERM_XTRA_FLUSH:
		while (!Term_xtra_gcu_event(FALSE));
		return (0);

		/* Delay */
		case TERM_XTRA_DELAY:
		if (v <= 0) return (0);
		usleep(1000 * v);
		return (0);

		/* React to events */
		case TERM_XTRA_REACT:
			(void)Term_xtra_gcu_react();
		return (0);
	}

	/* Unknown */
	return (1);
}


/*
 * Actually MOVE the hardware cursor
 */
static errr Term_curs_gcu(int x, int y)
{
	term_data *td = (term_data *)(Term->data);

	/* Literally move the cursor */
	wmove(td->win, y, x);

	/* Success */
	return (0);
}


/*
 * Erase a grid of space
 * Hack -- try to be "semi-efficient".
 */
static errr Term_wipe_gcu(int x, int y, int n)
{
	term_data *td = (term_data *)(Term->data);
	char buf[1024];

	/* Place cursor */
	wmove(td->win, y, x);

	/* Clear to end of line */
	if (x + n >= td->t.cols)
	{
		wclrtoeol(td->win);
	}

	/* Clear some characters */
	else
	{
		/* Format a buffer */
		(void)strnfmt(buf, sizeof(buf), "%*c", n, ' ');

		/* Output */
		waddstr(td->win, buf);
	}

	/* Success */
	return (0);
}


/*
 * Place some text on the screen using an attribute
 */
static errr Term_text_gcu(int x, int y, int n, byte a, cptr s)
{
	term_data *td = (term_data *)(Term->data);

	char buf[1024];


#ifdef A_COLOR
	/* Set the color */
	if (can_use_color) wattrset(td->win, colortable[a & 0x0F]);
#endif

	/* Move the cursor */
	wmove(td->win, y, x);

	/* Format to appropriate size */
	(void)strnfmt(buf, sizeof(buf), "%.*s", n, s);

	/* Write to screen */
	waddstr(td->win, buf);

#ifdef A_COLOR
	/* Unset the color */
	if (can_use_color) wattrset(td->win, 0);
#endif

	/* Success */
	return (0);
}


/*
 * User interaction  -Christer-
 *
 * Only handles suspend ! + Ctrl-Z for now.
 */
static errr Term_user_gcu(int n)
{
	char key;

	/* Unused parameter */
	(void)n;

	/* Prompt */
	prt("Awaiting system command: ", 0, 0);

	/* Wait for, get and remove a keypress */
	(void)Term_inkey(&key, TRUE, TRUE);

	switch (key)
	{

#ifdef HANDLE_SIGNALS

#include <signal.h>

		/* Suspend signal */
		case KTRL('Z'):
		{
#ifdef SIGTSTP
			/* Send SIGTSTP = suspend to myself */
			(void)raise(SIGTSTP);
#endif
			break;
		}

#endif /* HANDLE_SIGNALS */

	}

	/* Clear prompt */
	(void)Term_erase(0, 0, 255);

	/* Success */
	return (0);
}

/*
 * Change the display.
 *
 * At present, we do not change Term size when switching between standard
 * and tall displays; we use a terminal window large enough for both.
 */
static errr switch_display_gcu(int display)
{
	/* Nothing changes */
	(void)display;

	/* Assume success */
	return (0);
}



/*
 * Create a window for the given "term_data" argument.
 */
static errr term_data_init_gcu(term_data *td, int rows, int cols, int y, int x, int i)
{

	term *t = &td->t;

	/* Check window size */
	if (rows <= 0 || cols <= 0) return (0);

	/* Create new window */
	td->win = newwin(rows, cols, y, x);

	/* Check for failure */
	if (!td->win)
	{
		/* Error */
		quit("Failed to setup curses window.");
	}

	/* Initialize the term */
	(void)term_init(t, cols, rows, 256);

	/* Avoid bottom right corner */
	t->icky_corner = TRUE;

	/* Erase with "white space" */
	t->attr_blank = TERM_WHITE;
	t->char_blank = ' ';

	/* Set some hooks */
	t->init_hook = Term_init_gcu;
	t->nuke_hook = Term_nuke_gcu;

	/* Set some more hooks */
	t->text_hook = Term_text_gcu;
	t->wipe_hook = Term_wipe_gcu;
	t->curs_hook = Term_curs_gcu;
	t->xtra_hook = Term_xtra_gcu;
	t->user_hook = Term_user_gcu;
	switch_display_hook = switch_display_gcu;


	/* Flag as active */
	td->active = TRUE;

	/* Save the data */
	t->data = td;

	/* Activate it */
	(void)Term_activate(t);

	/* Success */
	return (0);
}

/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(cptr str)
{
	int i;

	/* Unused parameter XXX XXX */
	(void)str;

	/* Nuke all possible terms (if defined) */
	for (i = 0; i < TERM_MAX; i++)
	{
		(void)term_nuke(angband_term[i]);
	}

	/* Exit curses */
	endwin();
}

/*
 * Prepare "curses" for use by the file "term.c"
 *
 * Installs the "hook" functions defined above, and then activates
 * the main screen "term", which clears the screen and such things.
 *
 * Someone should really check the semantics of "initscr()"
 */
errr init_gcu(int argc, char *argv[])
{
	int i;

	int num_term = MAX_TERM_DATA, next_win = 0;

	/* Parse args */
	for (i = 1; i < argc; i++)
	{
		if (prefix(argv[i], "-x"))
		{
			split_window = FALSE;
			continue;
		}

		plog_fmt("Ignoring option: %s", argv[i]);
	}


	/* Extract the normal keymap */
	keymap_norm_prepare();


#if defined(USE_OLD_CURSES)
	/* Initialize for old BSD 4.4 curses */
	if (initscr() == (WINDOW*)ERR) return (-1);
#else
	/* Initialize for standard curses */
	if (initscr() == NULL) return (-1);
#endif

 	quit_aux = hook_quit;

	/* Hack -- Require large screen, or Quit with message */
	if ((LINES < term_size_min[WINDOW_DISPLAY][1]) ||
	    (COLS < term_size_min[WINDOW_DISPLAY][0]))
	{
		quit(format("Sangband needs at least an %dx%d 'curses' screen",
			    term_size_min[WINDOW_DISPLAY][0],
			    term_size_min[WINDOW_DISPLAY][1]));
	}

	/* Remember screen size */
	y_max = COLS;
	x_max = LINES;

#ifdef A_COLOR

	/*** Init the Color-pairs and set up a translation table ***/

	/* Do we have color, and enough color, available? */
	can_use_color = ((start_color() != ERR) && has_colors() &&
	                 (COLORS >= 8) && (COLOR_PAIRS >= 8));

#ifdef REDEFINE_COLORS
	/* Can we change colors? */
	can_fix_color = (can_use_color && can_change_color() &&
	                 (COLORS >= 16) && (COLOR_PAIRS > 8));
#endif

	/* Attempt to use customized colors */
	if (can_fix_color)
	{
		/* Prepare the color pairs */
		for (i = 1; i <= 8; i++)
		{
			/* Reset the color */
			if (init_pair(i, i - 1, 0) == ERR)
			{
				quit("Color pair init failed");
			}

			/* Set up the colormap */
			colortable[i - 1] = (COLOR_PAIR(i) | A_NORMAL);
			colortable[i + 7] = (COLOR_PAIR(i) | A_BRIGHT);
		}

		/* XXX XXX XXX Take account of "gamma correction" */

		/* Prepare the "Angband Colors" */
		(void)Term_xtra_gcu_react();
	}

	/* Attempt to use colors */
	else if (can_use_color)
	{
		/* Color-pair 0 is *always* WHITE on BLACK */

		/* Prepare the color pairs */
		init_pair(1, COLOR_RED,     COLOR_BLACK);
		init_pair(2, COLOR_GREEN,   COLOR_BLACK);
		init_pair(3, COLOR_YELLOW,  COLOR_BLACK);
		init_pair(4, COLOR_BLUE,    COLOR_BLACK);
		init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
		init_pair(6, COLOR_CYAN,    COLOR_BLACK);
		init_pair(7, COLOR_BLACK,   COLOR_BLACK);

		/* Prepare the "Angband Colors" -- Bright white is too bright */
		colortable[0] = (COLOR_PAIR(7) | A_NORMAL);	/* Black */
		colortable[1] = (COLOR_PAIR(0) | A_NORMAL);	/* White */
		colortable[2] = (COLOR_PAIR(6) | A_NORMAL);	/* Grey XXX */
		colortable[3] = (COLOR_PAIR(1) | A_BRIGHT);	/* Orange XXX */
		colortable[4] = (COLOR_PAIR(1) | A_NORMAL);	/* Red */
		colortable[5] = (COLOR_PAIR(2) | A_NORMAL);	/* Green */
		colortable[6] = (COLOR_PAIR(4) | A_NORMAL);	/* Blue */
		colortable[7] = (COLOR_PAIR(3) | A_NORMAL);	/* Umber */
		colortable[8] = (COLOR_PAIR(7) | A_BRIGHT);	/* Dark-grey XXX */
		colortable[9] = (COLOR_PAIR(6) | A_BRIGHT);	/* Light-grey XXX */
		colortable[10] = (COLOR_PAIR(5) | A_NORMAL);	/* Purple */
		colortable[11] = (COLOR_PAIR(3) | A_BRIGHT);	/* Yellow */
		colortable[12] = (COLOR_PAIR(5) | A_BRIGHT);	/* Light Red XXX */
		colortable[13] = (COLOR_PAIR(2) | A_BRIGHT);	/* Light Green */
		colortable[14] = (COLOR_PAIR(4) | A_BRIGHT);	/* Light Blue */
		colortable[15] = (COLOR_PAIR(3) | A_NORMAL);	/* Light Umber XXX */
	}

#endif /* A_COLOR */


	/*** Low level preparation ***/

#ifdef USE_GETCH

	/* Paranoia -- Assume no waiting */
	nodelay(stdscr, FALSE);

#endif

	/* Prepare */
	cbreak();
	noecho();
	nonl();

	/* Extract the game keymap */
	keymap_game_prepare();


	/*** Now prepare the term(s) ***/

	if (!split_window) num_term = 1;

	/* Create one or several terms */
	for (i = 0; i < num_term; i++)
	{
		/*
		 * Variables rows & cols are first used to contain the main
		 * window size, until they are reassigned to contain the
		 * current window size in the case statments.
		 */
		int rows, cols, y, x;

		/* Flag as inactive */
		data[next_win].active = FALSE;

		if (split_window)
		{
			/*
			 * Sub windows must show least 10 columns, 1 row.  Excess
			 * space goes to the sub windows.
			 *
			 * We could add code that would add excess space to
			 * the main window instead if we have more than
			 * 161x71 space.  XXX XXX
			 */
			if (COLS > MIN_COLS_SPLIT)
				cols = term_size_min[WINDOW_DISPLAY][0];
			else
				cols = COLS;
			if (LINES > MIN_ROWS_SPLIT)
				rows = term_size_min[WINDOW_DISPLAY][1];
			else
				rows = LINES;
		}
		else
		{
			cols = term_size_min[WINDOW_DISPLAY][0];
			rows = term_size_min[WINDOW_DISPLAY][1];
		}

		/* Decide on size and position */
		switch (i)
		{
			/* Upper left */
			case 0:
				y = x = 0;
				break;

			/* Hack - Ignore Map */
			case 1:
				/* Code added to assign space & pos, even if unused */
				y = x = 0;
				cols = term_size_min[TERM_MAP][0];
				rows = term_size_min[TERM_MAP][1];

				next_win++;
				continue;

			/* Lower left */
			case 2:
				y = rows + 1;
				x = 0;
				rows = LINES - (rows + 1);
				break;

			/* Upper right */
			case 3:
				y = 0;
				x = cols + 1;
				cols = COLS - (cols + 1);
				break;

			/* Lower right */
			case 4:
				y = rows + 1;
				x = cols + 1;
				rows = LINES - (rows + 1);
				cols = COLS - (cols + 1);
				break;

			/* XXX */
			default:
				rows = cols = y = x = 0;
				break;
		}

		/* Skip non-existant windows */
		if (rows <= 0 || cols <= 0) continue;

		/* Create a term */
		(void)term_data_init_gcu(&data[next_win], rows, cols, y, x, i);

		/* Remember the term */
		angband_term[next_win] = &data[next_win].t;

		/* One more window */
		next_win++;
	}

	/* Activate the "Angband" window screen */
	(void)Term_activate(&data[0].t);

	/* Resizing is safe */
	resize_safe = TRUE;

	/* Success */
	return (0);
}


#endif /* USE_GCU */


