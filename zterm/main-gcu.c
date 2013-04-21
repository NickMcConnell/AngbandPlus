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


#ifdef USE_GCU

#include "langband.h"
#include "lbwindows.h"
#include <errno.h>

/*
 * Include the proper "header" file
 */
#ifdef USE_NCURSES
# include <ncurses.h>
#else
# include <curses.h>
#endif


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



struct gcu_winconnection {
    WINDOW *win;            /* Pointer to the curses window */
};

typedef struct gcu_winconnection gcu_winconnection;

extern void gcu_keymap_norm(void);
extern int gcu_setup_colours(void);
extern void gcu_keymap_game_prepare(void);
extern void gcu_keymap_norm_prepare(void);
extern int gcu_can_use_color;
extern int gcu_colortable[16];

/*
 * Create a window for the given "term_data" argument.
 *
 * Assumes legal arguments.
 */
static LangbandFrame *
cursify_frame(LangbandFrame *lf, int max_cols, int max_rows) {
    
    //angband_zterm *t = NULL;
    gcu_winconnection *wc = NULL;
	
    wc = malloc(sizeof(gcu_winconnection));
    memset(wc, 0, sizeof(gcu_winconnection));

    lf->ui_connection = wc;

    DEBUGPUT("Cursify %s.\n", lf->name);
    
    {
	int cols = lf->allowed_width;
	int rows = lf->allowed_height;

	if (cols <= 0 || rows <= 0) {
	    ERRORMSG("Illegal values for window '%s': cols %d, rows %d. (%d,%d)\n",
		     lf->name, cols, rows, max_cols, max_rows);
	}
	
	DBGPUT("New window %s %d, %d, %d, %d\n", lf->name, cols, rows, lf->xoffset, lf->yoffset);
	
	/* Create new window */
	wc->win = newwin(rows, cols, lf->yoffset, lf->xoffset);
	
	/* Check for failure */
	if (!wc->win) {
	    /* Error */
	    ERRORMSG("Failed to setup curses window.");
	    return NULL;
	}
	//DBGPUT("going init on %p with %d %d\n", lf->azt, cols, rows);
	
	/* Initialize the term */
	//term_init(lf->azt, cols, rows, 256);
    }

    // time to assign some important values
    lf->columns = lf->frame_width = lf->allowed_width;
    lf->rows = lf->frame_width = lf->allowed_height;

    lf->tile_width = lf->tile_height = 1;

    DEBUGPUT("Finished cursify of %s.\n", lf->name);
    
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
int
init_gcu(int initflags) {

    int i;
    int use_big_screen = 0;

    /* Extract the normal keymap */
    gcu_keymap_norm_prepare();

    /* Initialize */
    if (initscr() == NULL) return (-1);

    
    /* Require standard size screen */
    if ((LINES < 24) || (COLS < 80)) {
	ERRORMSG("Angband needs at least an 80x24 'curses' screen");
	return -1;
    }

    if (use_big_screen) {
	DBGPUT("Big screen %d,%d\n", LINES, COLS);
    }

    gcu_setup_colours();

    /*** Low level preparation ***/

#ifdef USE_GETCH

    /* Paranoia -- Assume no waiting */
    nodelay(stdscr, 0);

#endif

    /* Prepare */
    cbreak();
    noecho();
    nonl();

    /* Extract the game keymap */
    gcu_keymap_game_prepare();

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
	
	//DBGPUT("did sdlify %p\n", lf);
	
	if (!lf) {
	    ERRORMSG("Problems creating frame '%s'\n", frameName);
	    return -1;
	}
	// tweaking
	{
	    gcu_winconnection *wc = (gcu_winconnection*)lf->ui_connection;
	    //wc->gt = screen_tiles; // improve later
	    lf->visible = 0;
	}
	//DBGPUT("end-loop\n");
    }

#ifdef USE_CURS_SET    
    curs_set(0);
    keypad(stdscr, 1); // gamble on these being available too when curs_set is there
    ESCDELAY = 0;
#endif
    
    activate_frame(FULL_TERM_IDX);    
    DEBUGPUT("Return to london\n");
    
    return 0;
 
}

int
cleanup_GCU(void) {
    int x, y;
    
    DBGPUT("CLEANUP\n");

    /* Go to normal keymap mode */
    gcu_keymap_norm();
    
    /* Restore modes */
    nocbreak();
    echo();
    nl();
    
    /* Hack -- make sure the cursor is visible */
#ifdef USE_CURS_SET    
    curs_set(1);
#endif

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

    //Term_xtra_gcu_alive(0);
    return 0;
}

#define ALSO_CLEAR_BG 0x01
#define DONT_PAINT 0x02

static int
gcu_putChar(WINDOW *win, short x, short y, int attr, int thechar) {
    if (!win) return -1;
#ifdef A_COLOR
    /* Set the color */
    if (gcu_can_use_color) wattrset(win, gcu_colortable[attr & 0x0F]);
#endif
    //DBGPUT("Output char %c at %d,%d\n", thechar, x, y); 
    wmove(win, y, x);
    waddch(win, thechar);
    //(void)wrefresh(win); // remove later
    return 0;
}

int
gcu_transparentBlit(short win_num, short x, short y, unsigned int img, short flags) {
    // also flushes

    LangbandFrame *lf = predefinedFrames[win_num];
    gcu_winconnection *wc = NULL;
    
    if (lf) {
	wc = (gcu_winconnection*)lf->ui_connection;
    }
    else {
	return 2;
    }
    
    //DBGPUT("tran blitting %ld %d to %d,%d\n", img, flag, x, y);
 
    if (img == 0) {
	gcu_putChar(wc->win, x, y, 1, ' ');
    }
    else if (img < 65536) { // character
	unsigned int thechar = (img & 0x000000FF); // bits 1-8
	unsigned int attr = (img & 0x0000FF00) >> 8; // bits 9-16
	gcu_putChar(wc->win, x, y, attr, thechar);
    }
    else {
    }

    if (!(flags & DONT_PAINT)) {
	// improve later
	(void)wrefresh(wc->win);
    }

    
    return -1; //exp_complex_blit(win_num, x, y, img, ALSO_CLEAR_BG | flag);
}

int
gcu_fullBlit(short win_num, short x, short y, unsigned int img, short flags) {
    // also flushes
    //DBGPUT("full blitting %ld %d to %d,%d\n", img, flag, x, y);
    
    return gcu_transparentBlit(win_num, x, y, img, flags);
}

int
gcu_flushCoords(short win_num, short x, short y, short w, short h) {
    
    LangbandFrame *lf = predefinedFrames[win_num];
    gcu_winconnection *wc = NULL;

    
    if (lf) {
	wc = (gcu_winconnection*)lf->ui_connection;
    }
    else {
	return 2;
    }
    // improve later
    (void)wrefresh(wc->win);
    
    return x=y=w=h=0;
}

int
gcu_clearCoords(short win_num, short x, short y, short w, short h) {
    // also flushes
    LangbandFrame *lf = predefinedFrames[win_num];
    gcu_winconnection *wc = NULL;
    int i, j;
    
    if (lf) {
	wc = (gcu_winconnection*)lf->ui_connection;
    }
    else {
	return 2;
    }
    
    for (i=0; i < h; i++) {
	for (j=0; j < w; j++) {
	    gcu_putChar(wc->win, x+j, y+i, 1, ' ');
	}
    }

    // improve later
    (void)wrefresh(wc->win);

    return 0;
}
	
    
#define ONLY_POLL 1

#define KBD_EVT 0x00
#define MOUSE_EVT 0x01

#define CTRL_KEY 0x02
#define ALT_KEY 0x04
#define SHIFT_KEY 0x08

#define L_M_BT 0x02
#define R_M_BT 0x04
#define M_M_BT 0x08

static int
getKey(int wait) {

    int i, k;
    
#ifdef USE_GETCH
    
    /* Wait */
    if (wait)
    {
	/* Paranoia -- Wait for it */
	nodelay(stdscr, 0);

	/* Get a keypress */
	i = getch();

	/* Mega-Hack -- allow graceful "suspend" */
	for (k = 0; (k < 10) && (i == ERR); k++) i = getch();

	/* Broken input is special */
	//if (i == ERR) exit_game_panic();
	//if (i == EOF) exit_game_panic();
    }

    /* Do not wait */
    else
    {
	/* Do not wait for it */
	nodelay(stdscr, 1);

	/* Check for keypresses */
	i = getch();

	/* Wait for it next time */
	nodelay(stdscr, 0);

	/* None ready */
	if (i == ERR) return -1;
	if (i == EOF) return -1;
    }
#else /* use_getch */
    
    {
     char buf[2];
    /* Wait */
    if (wait) {
	
	/* Wait for one byte */
	i = read(0, buf, 1);
	
	/* Hack -- Handle bizarre "errors" */
	if ((i <= 0) && (errno != EINTR)) {
	    ERRORMSG("HELL!\n");
	    exit(-890);
	    //exit_game_panic();
	}
    }
    
    /* Do not wait */
    else {

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
    i = buf[0];
    }
#endif

    /* Enqueue the keypress */
    return i;
}

int
gcu_getEvent(int option) {


    //SDL_Event event; /* just a temporary place to hold an event */
    int retval = 0;
    int eventcode = 0;
    // first bit, is this a keyboard event 0, mouse event 1
    // if keyboard:
    // second bit is if ctrl was pressed
    // third bit is if alt was pressed
    // fourth bit is if shift was pressed
    // ninth bit and out is the actual code.
    // if mouse:
    // second bit is if it was left button
    // third bit is if it was right button
    // fourth bit is if it was middle button
    // bits 6-17 is x coord
    // bits 18-29 is y coord

    //DBGPUT("Asked for event %d\n", option);
    if (option & ONLY_POLL) {
	retval = getKey(0);
	if (retval < 0)
	    return 0; // fix
    }
    else {
	retval = getKey(1);
	if (retval < 0)
	    return 0;
    }

    //INFOMSG("Read %d %o\n", retval, retval);
    
    eventcode |= KBD_EVT;
    eventcode |= retval << 8;
    
    return eventcode;
    
}


#endif /* USE_GCU */
