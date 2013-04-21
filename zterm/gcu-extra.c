
#ifdef USE_GCU

#include "langband.h"


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

/*
 * Try redefining the colors at startup.
 */
#define REDEFINE_COLORS


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
int gcu_can_use_color = 0;

/*
 * Software flag -- we are allowed to change the colors
 */
static int gcu_can_fix_color = 0;

/*
 * Simple Angband to Curses color conversion table
 */
int gcu_colortable[16];

#endif


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



/*
 * Place the "keymap" into its "normal" state
 */
void
gcu_keymap_norm(void) {


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
void
gcu_keymap_game(void) {

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

void
gcu_keymap_norm_prepare(void) {

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
void
gcu_keymap_game_prepare(void) {

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
 * Global table of color definitions (mostly zeros)
 */
static unsigned char angband_color_table[256][4] = {

        {0x00, 0x00, 0x00, 0x00},       /* TERM_DARK */
        {0x00, 0xFF, 0xFF, 0xFF},       /* TERM_WHITE */
        {0x00, 0x80, 0x80, 0x80},       /* TERM_SLATE */
        {0x00, 0xFF, 0x80, 0x00},       /* TERM_ORANGE */
        {0x00, 0xC0, 0x00, 0x00},       /* TERM_RED */
        {0x00, 0x00, 0x80, 0x40},       /* TERM_GREEN */
        {0x00, 0x00, 0x40, 0xFF},       /* TERM_BLUE */
        {0x00, 0x80, 0x40, 0x00},       /* TERM_UMBER */
        {0x00, 0x60, 0x60, 0x60},       /* TERM_L_DARK */
        {0x00, 0xC0, 0xC0, 0xC0},       /* TERM_L_WHITE */
        {0x00, 0xFF, 0x00, 0xFF},       /* TERM_VIOLET */
        {0x00, 0xFF, 0xFF, 0x00},       /* TERM_YELLOW */
        {0x00, 0xFF, 0x40, 0x40},       /* TERM_L_RED */
        {0x00, 0x00, 0xFF, 0x00},       /* TERM_L_GREEN */
        {0x00, 0x00, 0xFF, 0xFF},       /* TERM_L_BLUE */
        {0x00, 0xC0, 0x80, 0x40}        /* TERM_L_UMBER */
};

/*
 * React to changes
 */
static int
gcu_react_changes(void) {


#ifdef A_COLOR

    int i;

    /* Cannot handle color redefinition */
    if (!gcu_can_fix_color) return (0);

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

int
gcu_setup_colours(void) {
    
    int i = 0;
    
#ifdef A_COLOR

    /*** Init the Color-pairs and set up a translation table ***/

    /* Do we have color, and enough color, available? */
    gcu_can_use_color = ((start_color() != ERR) && has_colors() &&
			 (COLORS >= 8) && (COLOR_PAIRS >= 8));

#ifdef REDEFINE_COLORS

    /* Can we change colors? */
    gcu_can_fix_color = (gcu_can_use_color && can_change_color() &&
			 (COLORS >= 16) && (COLOR_PAIRS > 8));

#endif


    /* Attempt to use customized colors */
    if (gcu_can_fix_color)
    {
	/* Prepare the color pairs */
	for (i = 1; i <= 8; i++)
	{
	    /* Reset the color */
	    if (init_pair(i, i - 1, 0) == ERR) {
		ERRORMSG("Color pair init failed");
		return -2;
	    }

	    /* Set up the colormap */
	    gcu_colortable[i - 1] = (COLOR_PAIR(i) | A_NORMAL);
	    gcu_colortable[i + 7] = (COLOR_PAIR(i) | A_BRIGHT);
	}

	/* Take account of "gamma correction" XXX XXX XXX */

	/* Prepare the "Angband Colors" */
	gcu_react_changes();
    }

    /* Attempt to use colors */
    else if (gcu_can_use_color) {

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
	gcu_colortable[0] = (COLOR_PAIR(7) | A_NORMAL);	/* Black */
	gcu_colortable[1] = (COLOR_PAIR(0) | A_NORMAL);	/* White */
	gcu_colortable[2] = (COLOR_PAIR(6) | A_NORMAL);	/* Grey XXX */
	gcu_colortable[3] = (COLOR_PAIR(1) | A_BRIGHT);	/* Orange XXX */
	gcu_colortable[4] = (COLOR_PAIR(1) | A_NORMAL);	/* Red */
	gcu_colortable[5] = (COLOR_PAIR(2) | A_NORMAL);	/* Green */
	gcu_colortable[6] = (COLOR_PAIR(4) | A_NORMAL);	/* Blue */
	gcu_colortable[7] = (COLOR_PAIR(3) | A_NORMAL);	/* Umber */
	gcu_colortable[8] = (COLOR_PAIR(7) | A_BRIGHT);	/* Dark-grey XXX */
	gcu_colortable[9] = (COLOR_PAIR(6) | A_BRIGHT);	/* Light-grey XXX */
	gcu_colortable[10] = (COLOR_PAIR(5) | A_NORMAL);	/* Purple */
	gcu_colortable[11] = (COLOR_PAIR(3) | A_BRIGHT);	/* Yellow */
	gcu_colortable[12] = (COLOR_PAIR(5) | A_BRIGHT);	/* Light Red XXX */
	gcu_colortable[13] = (COLOR_PAIR(2) | A_BRIGHT);	/* Light Green */
	gcu_colortable[14] = (COLOR_PAIR(4) | A_BRIGHT);	/* Light Blue */
	gcu_colortable[15] = (COLOR_PAIR(3) | A_NORMAL);	/* Light Umber XXX */
    }

#endif
    return 0;
}

#endif /* use gcu */
