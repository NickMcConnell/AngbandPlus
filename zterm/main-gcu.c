/* File: main-gcu.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
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
 * See also "USE_CAP" and "main-cap.c" for code that bypasses "curses"
 * and uses the "termcap" information directly, or even bypasses the
 * "termcap" information and sends direct vt100 escape sequences.
 *
 * This file provides up to 4 term windows.
 *
 * This file will attempt to redefine the screen colors to conform to
 * standard Angband colors.  It will only do so if the terminal type
 * indicates that it can do so.  See the page:
 * 
 *     http://www.umr.edu/~keldon/ang-patch/ncurses_color.html
 *
 * for information on this.
 *
 * Consider the use of "savetty()" and "resetty()".  XXX XXX XXX
 */

#include "angband.h"
#include "langband.h"
#include "lbwindows.h"
#include <errno.h>

#ifdef USE_GCU

/*
 * Hack -- play games with "bool" and "term"
 */
#undef bool

/* Avoid 'struct term' name conflict with <curses.h> (via <term.h>) on AIX */
#define term System_term

/*
 * Include the proper "header" file
 */
#ifdef USE_NCURSES
# include <ncurses.h>
#else
# include <curses.h>
#endif

#undef term

/*
 * Try redefining the colors at startup.
 */
#define REDEFINE_COLORS


/*
 * Hack -- try to guess which systems use what commands
 * Hack -- allow one of the "USE_Txxxxx" flags to be pre-set.
 * Mega-Hack -- try to guess when "POSIX" is available.
 * If the user defines two of these, we will probably crash.
 */
#if !defined(USE_TPOSIX)
# if !defined(USE_TERMIO) && !defined(USE_TCHARS)
#  if defined(_POSIX_VERSION)
#   define USE_TPOSIX
#  else
#   if defined(USG) || defined(linux) || defined(SOLARIS)
#    define USE_TERMIO
#   else
#    define USE_TCHARS
#   endif
#  endif
# endif
#endif

/*
 * Hack -- Amiga uses "fake curses" and cannot do any of this stuff
 */
#if defined(AMIGA)
# undef USE_TPOSIX
# undef USE_TERMIO
# undef USE_TCHARS
#endif




/*
 * POSIX stuff
 */
#ifdef USE_TPOSIX
# include <sys/ioctl.h>
# include <termios.h>
#endif

/*
 * One version needs these files
 */
#ifdef USE_TERMIO
# include <sys/ioctl.h>
# include <termio.h>
#endif

/*
 * The other needs these files
 */
#ifdef USE_TCHARS
# include <sys/ioctl.h>
# include <sys/resource.h>
# include <sys/param.h>
# include <sys/file.h>
# include <sys/types.h>
#endif


/*
 * XXX XXX Hack -- POSIX uses "O_NONBLOCK" instead of "O_NDELAY"
 *
 * They should both work due to the "(i != 1)" test below.
 */
#ifndef O_NDELAY
# define O_NDELAY O_NONBLOCK
#endif


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

#ifdef USE_TERMIO

static struct termio  norm_termio;

static struct termio  game_termio;

#endif

#ifdef USE_TCHARS

static struct ltchars norm_special_chars;
static struct sgttyb  norm_ttyb;
static struct tchars  norm_tchars;
static int            norm_local_chars;

static struct ltchars game_special_chars;
static struct sgttyb  game_ttyb;
static struct tchars  game_tchars;
static int            game_local_chars;

#endif

struct gcu_winconnection {
    WINDOW *win;            /* Pointer to the curses window */
};

typedef struct gcu_winconnection gcu_winconnection;

/* Max number of windows on screen */
//#define MAX_TERM_DATA 5

/* Information about our windows */
//static term_data data[MAX_TERM_DATA];

//term_data **gcu_subwindows = NULL;

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

#endif



/*
 * Place the "keymap" into its "normal" state
 */
static void keymap_norm(void)
{

#ifdef USE_TPOSIX

	/* restore the saved values of the special chars */
	(void)tcsetattr(0, TCSAFLUSH, &norm_termios);

#endif

#ifdef USE_TERMIO

	/* restore the saved values of the special chars */
	(void)ioctl(0, TCSETA, (char *)&norm_termio);

#endif

#ifdef USE_TCHARS

	/* restore the saved values of the special chars */
	(void)ioctl(0, TIOCSLTC, (char *)&norm_special_chars);
	(void)ioctl(0, TIOCSETP, (char *)&norm_ttyb);
	(void)ioctl(0, TIOCSETC, (char *)&norm_tchars);
	(void)ioctl(0, TIOCLSET, (char *)&norm_local_chars);

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

#ifdef USE_TERMIO

	/* restore the saved values of the special chars */
	(void)ioctl(0, TCSETA, (char *)&game_termio);

#endif

#ifdef USE_TCHARS

	/* restore the saved values of the special chars */
	(void)ioctl(0, TIOCSLTC, (char *)&game_special_chars);
	(void)ioctl(0, TIOCSETP, (char *)&game_ttyb);
	(void)ioctl(0, TIOCSETC, (char *)&game_tchars);
	(void)ioctl(0, TIOCLSET, (char *)&game_local_chars);

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

#ifdef USE_TERMIO

	/* Get the normal keymap */
	(void)ioctl(0, TCGETA, (char *)&norm_termio);

#endif

#ifdef USE_TCHARS

	/* Get the normal keymap */
	(void)ioctl(0, TIOCGETP, (char *)&norm_ttyb);
	(void)ioctl(0, TIOCGLTC, (char *)&norm_special_chars);
	(void)ioctl(0, TIOCGETC, (char *)&norm_tchars);
	(void)ioctl(0, TIOCLGET, (char *)&norm_local_chars);

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

#ifdef USE_TERMIO

	/* Acquire the current mapping */
	(void)ioctl(0, TCGETA, (char *)&game_termio);

	/* Force "Ctrl-C" to interupt */
	game_termio.c_cc[VINTR] = (char)3;

	/* Force "Ctrl-Z" to suspend */
	game_termio.c_cc[VSUSP] = (char)26;

	/* Hack -- Leave "VSTART/VSTOP" alone */

	/* Disable the standard control characters */
	game_termio.c_cc[VQUIT] = (char)-1;
	game_termio.c_cc[VERASE] = (char)-1;
	game_termio.c_cc[VKILL] = (char)-1;
	game_termio.c_cc[VEOF] = (char)-1;
	game_termio.c_cc[VEOL] = (char)-1;

#if 0
	/* Disable the non-posix control characters */
	game_termio.c_cc[VEOL2] = (char)-1;
	game_termio.c_cc[VSWTCH] = (char)-1;
	game_termio.c_cc[VDSUSP] = (char)-1;
	game_termio.c_cc[VREPRINT] = (char)-1;
	game_termio.c_cc[VDISCARD] = (char)-1;
	game_termio.c_cc[VWERASE] = (char)-1;
	game_termio.c_cc[VLNEXT] = (char)-1;
	game_termio.c_cc[VSTATUS] = (char)-1;
#endif

	/* Normally, block until a character is read */
	game_termio.c_cc[VMIN] = 1;
	game_termio.c_cc[VTIME] = 0;

#endif

#ifdef USE_TCHARS

	/* Get the default game characters */
	(void)ioctl(0, TIOCGETP, (char *)&game_ttyb);
	(void)ioctl(0, TIOCGLTC, (char *)&game_special_chars);
	(void)ioctl(0, TIOCGETC, (char *)&game_tchars);
	(void)ioctl(0, TIOCLGET, (char *)&game_local_chars);

	/* Force suspend (^Z) */
	game_special_chars.t_suspc = (char)26;

	/* Cancel some things */
	game_special_chars.t_dsuspc = (char)-1;
	game_special_chars.t_rprntc = (char)-1;
	game_special_chars.t_flushc = (char)-1;
	game_special_chars.t_werasc = (char)-1;
	game_special_chars.t_lnextc = (char)-1;

	/* Force interupt (^C) */
	game_tchars.t_intrc = (char)3;

	/* Force start/stop (^Q, ^S) */
	game_tchars.t_startc = (char)17;
	game_tchars.t_stopc = (char)19;

	/* Cancel some things */
	game_tchars.t_quitc = (char)-1;
	game_tchars.t_eofc = (char)-1;
	game_tchars.t_brkc = (char)-1;

#endif

}




/*
 * Suspend/Resume
 */
static errr Term_xtra_gcu_alive(int v)
{
        int x, y;
	DEBUGPUT("EXTRA\n");
	
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
		Term_xtra(TERM_XTRA_SHAPE, 1);

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

#ifdef USE_NCURSES
const char help_gcu[] = "NCurses, for terminal console, subopts -b(ig screen)";
#else /* USE_NCURSES */
const char help_gcu[] = "Curses, for terminal console, subopts -b(ig screen)";
#endif /* USE_NCURSES */

/*
 * Init the "curses" system
 */
static void Term_init_gcu(angband_zterm *t) {

    LangbandFrame *lf = (LangbandFrame *)(t->data);
    gcu_winconnection *wc = (gcu_winconnection*)lf->ui_connection;

    /* Count init's, handle first */
    if (active++ != 0) return;

    /* Erase the window */
    (void)wclear(wc->win);

    /* Reset the cursor */
    (void)wmove(wc->win, 0, 0);

    /* Flush changes */
    (void)wrefresh(wc->win);

    /* Game keymap */
    keymap_game();
}


/*
 * Nuke the "curses" system
 */
static void Term_nuke_gcu(angband_zterm *t)
{
    int x, y;
    LangbandFrame *lf = (LangbandFrame *)(t->data);
    gcu_winconnection *wc = (gcu_winconnection*)lf->ui_connection;

    /* Delete this window */
    delwin(wc->win);

    /* Count nuke's, handle last */
    if (--active != 0) return;

    /* Hack -- make sure the cursor is visible */
    Term_xtra(TERM_XTRA_SHAPE, 1);

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
    Term_keypress(i);

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
	if (fcntl(0, F_SETFL, k | O_NDELAY) < 0) return (1);

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
	init_color(i,
		   angband_color_table[i][1] * 1000 / 255,
		   angband_color_table[i][2] * 1000 / 255,
		   angband_color_table[i][3] * 1000 / 255);
    }

#endif

    /* Success */
    return (0);
}


/*
 * Handle a "special request"
 */
errr
Term_xtra_gcu(int n, int v) {

    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    gcu_winconnection *wc = (gcu_winconnection*)lf->ui_connection;

    /* Analyze the request */
    switch (n)
    {
	/* Clear screen */
    case TERM_XTRA_CLEAR:
	touchwin(wc->win);
	(void)wclear(wc->win);
	return (0);

	/* Make a noise */
    case TERM_XTRA_NOISE:
	(void)write(1, "\007", 1);
	return (0);

	/* Flush the Curses buffer */
    case TERM_XTRA_FRESH:
	(void)wrefresh(wc->win);
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
	usleep(1000 * v);
	return (0);

	/* React to events */
    case TERM_XTRA_REACT:
	Term_xtra_gcu_react();
	return (0);
    }

    /* Unknown */
    return (1);
}


/*
 * Actually MOVE the hardware cursor
 */
static errr Term_curs_gcu(int x, int y) {
    
    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    gcu_winconnection *wc = (gcu_winconnection*)lf->ui_connection;

    /* Literally move the cursor */
    wmove(wc->win, y, x);

    /* Success */
    return (0);
}


/*
 * Erase a grid of space
 * Hack -- try to be "semi-efficient".
 */
static errr Term_wipe_gcu(int x, int y, int n) {

    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    gcu_winconnection *wc = (gcu_winconnection*)lf->ui_connection;

    /* Place cursor */
    wmove(wc->win, y, x);

    /* Clear to end of line */
    if (x + n >= lf->azt->wid)
    {
	wclrtoeol(wc->win);
    }

    /* Clear some characters */
    else
    {
	while (n-- > 0) waddch(wc->win, ' ');
    }

    /* Success */
    return (0);
}


/*
 * Place some text on the screen using an attribute
 */
static errr Term_text_gcu(int x, int y, int n, s16b a, const s16b *s) {

    LangbandFrame *lf = (LangbandFrame *)(Term->data);
    gcu_winconnection *wc = (gcu_winconnection*)lf->ui_connection;

    int i, pic;
	
#ifdef A_COLOR
    /* Set the color */
    if (can_use_color) wattrset(wc->win, colortable[a & 0x0F]);
#endif

    /* Move the cursor */
    wmove(wc->win, y, x);

    /* Draw each character */
    for (i = 0; i < n; i++)
    {
#ifdef USE_GRAPHICS
	/* Special character */
	if (use_graphics && (s[i] >= LANGBAND_GFX_START))
	{
	    /* Determine picture to use */
	    switch (s[i] - LANGBAND_GFX_START)
	    {
#ifdef ACS_CKBOARD
		/* Wall */
	    case '#':
		pic = ACS_CKBOARD;
		break;
#endif /* ACS_CKBOARD */

#ifdef ACS_BOARD
		/* Mineral vein */
	    case '%':
		pic = ACS_BOARD;
		break;
#endif /* ACS_BOARD */

		/* XXX */
	    default:
		pic = '?';
		break;
	    }

	    /* Draw the picture */
	    waddch(wc->win, pic);

	    /* Next character */
	    continue;
	}
#endif

	/* Draw a normal character */
	waddch(wc->win, (byte)s[i]);
    }

    /* Success */
    return (0);
}


/*
 * Create a window for the given "term_data" argument.
 *
 * Assumes legal arguments.
 */
static LangbandFrame *
cursify_frame(LangbandFrame *lf, int max_cols, int max_rows) {
    
    angband_zterm *t = NULL;
    gcu_winconnection *wc = NULL;
	
    wc = malloc(sizeof(gcu_winconnection));
    WIPE(wc, gcu_winconnection);

    lf->azt = malloc(sizeof(angband_zterm));
    WIPE(lf->azt, angband_zterm);

    lf->ui_connection = wc;

    // DEBUGPUT("Cursify %s.\n", lf->name);
    
    {
	int cols = lf->allowed_width;
	int rows = lf->allowed_height;

	if (cols <= 0 || rows <= 0) {
	    ERRORMSG("Illegal values for window '%s': cols %d, rows %d. (%d,%d)\n",
		     lf->name, cols, rows, max_cols, max_rows);
	}
	
	//DBGPUT("New window  %d, %d, %d, %d\n", cols, rows, lf->xoffset, lf->yoffset);
	
	/* Create new window */
	wc->win = newwin(rows, cols, lf->yoffset, lf->xoffset);
	
	/* Check for failure */
	if (!wc->win) {
	    /* Error */
	    z_quit("Failed to setup curses window.");
	}
	//DBGPUT("going init on %p with %d %d\n", lf->azt, cols, rows);
	
	/* Initialize the term */
	term_init(lf->azt, cols, rows, 256);
    }

    // time to assign some important values
    lf->columns = lf->frame_width = lf->allowed_width;
    lf->rows = lf->frame_width = lf->allowed_height;

    lf->tile_width = lf->tile_height = 1;
    
    t = lf->azt;
    
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

    /* Save the data */
    t->data = lf;
    //DEBUGPUT("ACT\n");
    /* Activate it */
    Term_activate(t);

    //DEBUGPUT("Finished cursify of %s.\n", lf->name);
    
    /* Success */
    return lf;
}


/*
 * Prepare "curses" for use by the file "z-term.c"
 *
 * Installs the "hook" functions defined above, and then activates
 * the main screen "term", which clears the screen and such things.
 *
 * Someone should really check the semantics of "initscr()"
 */
errr init_gcu(int argc, char **argv)
{
    int i;


//    bool use_big_screen = TRUE;
    bool use_big_screen = FALSE;

    /* Extract the normal keymap */
    keymap_norm_prepare();

    /* Initialize */
    if (initscr() == NULL) return (-1);

    
    /* Require standard size screen */
    if ((LINES < 24) || (COLS < 80)) {
	z_quit("Angband needs at least an 80x24 'curses' screen");
    }

    if (use_big_screen) {
	DBGPUT("Big screen %d,%d\n", LINES, COLS);
    }

#ifdef USE_GRAPHICS

    /* Set graphics flag */
    use_graphics = arg_graphics;

#endif

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
		z_quit("Color pair init failed");
	    }

	    /* Set up the colormap */
	    colortable[i - 1] = (COLOR_PAIR(i) | A_NORMAL);
	    colortable[i + 7] = (COLOR_PAIR(i) | A_BRIGHT);
	}

	/* Take account of "gamma correction" XXX XXX XXX */

	/* Prepare the "Angband Colors" */
	Term_xtra_gcu_react();
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

#endif


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

    for (i = 0; i < num_predefinedFrames; i++) {
	LangbandFrame *lf = get_frame(i, PREDEFINED);
	const char *frameName = NULL;
	//DBGPUT("Checking sub %d\n", i);
	if (!lf) {
	    DBGPUT("Did not find frame %d.\n", i);
	    continue;
	}
	frameName = lf->name;
	lf = cursify_frame(lf, COLS, LINES);
	
	//DBGPUT("did cursify %p\n", lf);
	
	if (!lf) {
	    ERRORMSG("Problems creating frame '%s'\n", frameName);
	    return -1;
	}

	lf->visible = FALSE;
	//DBGPUT("end-loop\n");
    }

    activate_frame(FULL_TERM_IDX);
    Term = activeFrames[FULL_TERM_IDX]->azt;

    DEBUGPUT("Return to london\n");
    
    return 0;
    
#if 0
    gcu_subwindows = malloc(NUMBER_OF_TERMS * sizeof(term_data*));
    for (i=0; i< NUMBER_OF_TERMS; i++) {
	gcu_subwindows[i] = NULL;
    }
	
    
    /* Big screen -- one big term */
    if (use_big_screen)
    {
	/* Create a term */
	term_data *big = malloc(sizeof(term_data));
	WIPE(big,term_data);
	
	term_data_init_gcu(big, 0, 0, COLS, LINES);

	/* Remember the term */
	angband_term[BIG_TERM_IDX] = &(big->t);
	gcu_subwindows[BIG_TERM_IDX] = big;
    }

    /* No big screen -- create as many term windows as possible */
    else
    {

	for (i = 0; i < NUMBER_OF_TERMS; i++) {
	    /* Create a term */
	    term_data *big = malloc(sizeof(term_data));
	    SubWindow *s = get_subwindow(i);
	    if (!s) {
		DBGPUT("Did not find subwindow %d.\n", i);
		return -1;
	    }
		
	    WIPE(big,term_data);
	    
	    term_data_init_gcu(big,   s->x, s->y, s->width, s->height);
	    gcu_subwindows[i] = big;
	    angband_term[i] = &(big->t);

	}

    }


    
    /* Activate the "Angband" window screen */
    Term_activate(&(gcu_subwindows[BIG_TERM_IDX]->t));

    /* Remember the active screen */
    term_screen = &(gcu_subwindows[BIG_TERM_IDX]->t);

    /* Success */
    return (0);
#endif
}

errr
cleanup_GCU(void) {

    Term_xtra_gcu_alive(0);
    return 0;
}


#endif /* USE_GCU */
