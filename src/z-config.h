/* File: z-config.h */

/* Purpose: Angband specific configuration stuff */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Allow debug commands */
#define USE_DEBUG

/* Allow various special stuff (sound, graphics, etc.) */
#define USE_SPECIAL


/*
 * Look through the following lines, and where a comment includes the
 * tag "OPTION:", examine the associated "#define" statements, and decide
 * whether you wish to keep, comment, or uncomment them.  You should not
 * have to modify any lines not indicated by "OPTION".
 *
 * Note: Also examine the "system" configuration file "h-config.h".
 *
 * And finally, remember that the "Makefile" will specify some rather
 * important compile time options, like what visual module to use.
 */


/*
 * OPTION: See the Makefile(s), where several options may be declared.
 *
 * Some popular options include "USE_GCU" (allow use with Unix "curses"),
 * "USE_X11" (allow basic use with Unix X11), "USE_XAW" (allow use with
 * Unix X11 plus the Athena Widget set), and "USE_CAP" (allow use with
 * the "termcap" library, or with hard-coded vt100 terminals).
 *
 * The old "USE_NCU" option has been replaced with "USE_GCU".
 *
 * Several other such options are available for non-unix machines,
 * such as "MACINTOSH", "WINDOWS", "USE_IBM", "USE_EMX".
 *
 * You may also need to specify the "system", using defines such as
 * "SOLARIS" (for Solaris), etc, see "h-config.h" for more info.
 */


/*
 * OPTION: Use the POSIX "termios" methods in "main-gcu.c"
 */
/* #define USE_TPOSIX */

/*
 * OPTION: Use the "termio" methods in "main-gcu.c"
 */
/* #define USE_TERMIO */

/*
 * OPTION: Use the icky BSD "tchars" methods in "main-gcu.c"
 */
/* #define USE_TCHARS */


/*
 * OPTION: Use "blocking getch() calls" in "main-gcu.c".
 * Hack -- Note that this option will NOT work on many BSD machines
 * Currently used whenever available, if you get a warning about
 * "nodelay()" undefined, then make sure to undefine this.
 */
#if defined(SYS_V) || defined(AMIGA)
# define USE_GETCH
#endif


/*
 * OPTION: Use the "curs_set()" call in "main-gcu.c".
 * Hack -- This option will not work on most BSD machines
 */
#ifdef SYS_V
# define USE_CURS_SET
#endif


/*
 * OPTION: Include "ncurses.h" instead of "curses.h" in "main-gcu.c"
 */
/* #define USE_NCURSES */


/*
 * OPTION: for multi-user machines running the game setuid to some other
 * user (like 'games') this SAFE_SETUID option allows the program to drop
 * its privileges when saving files that allow for user specified pathnames.
 * This lets the game be installed system wide without major security
 * concerns.  There should not be any side effects on any machines.
 *
 * This will handle "gids" correctly once the permissions are set right.
 */
#define SAFE_SETUID


/*
 * This flag enables the "POSIX" methods for "SAFE_SETUID".
 */
#ifdef _POSIX_SAVED_IDS
# define SAFE_SETUID_POSIX
#endif


/*
 * Prevent problems on (non-Solaris) Suns using "SAFE_SETUID".
 * The SAFE_SETUID code is weird, use it at your own risk...
 */
#if defined(SUNOS) && !defined(SOLARIS)
# undef SAFE_SETUID_POSIX
#endif


/* Debug mode options */
#ifdef USE_DEBUG

/*
 * OPTION: Hack -- Compile in support for "Wizard Commands"
 */
#define ALLOW_WIZARD

/*
 * OPTION: Hack -- Compile in support for "Spoiler Generation"
 */
#define ALLOW_SPOILERS

#endif /* USE_DEBUG */

/*
 * OPTION: Hack -- Compile in support for "Borg mode"
 */
#define ALLOW_BORG

/*
 * OPTION: Allow "do_cmd_colors" at run-time
 */
#define ALLOW_COLORS

/*
 * OPTION: Allow "do_cmd_visuals" at run-time
 */
#define ALLOW_VISUALS

/*
 * OPTION: Allow "do_cmd_macros" at run-time
 */
#define ALLOW_MACROS

/*
 * OPTION: Allow characteres to be "auto-rolled"
 */
#define ALLOW_AUTOROLLER


/*
 * OPTION: Allow parsing of the ascii template files in "init.c".
 * This must be defined if you do not have valid binary image files.
 * It should be usually be defined anyway to allow easy "updating".
 */
#define ALLOW_TEMPLATES

/*
 * OPTION: Delay the loading of the "f_text" array until it is actually
 * needed, saving ~1K, since "feature" descriptions are unused.
 */
#define DELAY_LOAD_F_TEXT

/*
 * OPTION: Delay the loading of the "k_text" array until it is actually
 * needed, saving ~1K, since "object" descriptions are unused.
 */
#define DELAY_LOAD_K_TEXT

/*
 * OPTION: Delay the loading of the "a_text" array until it is actually
 * needed, saving ~1K, since "artifact" descriptions are unused.
 */
#define DELAY_LOAD_A_TEXT

/*
 * OPTION: Delay the loading of the "e_text" array until it is actually
 * needed, saving ~1K, since "ego-item" descriptions are unused.
 */
#define DELAY_LOAD_E_TEXT

/*
 * OPTION: Delay the loading of the "r_text" array until it is actually
 * needed, saving ~60K, but "simplifying" the "monster" descriptions.
 */
/* #define DELAY_LOAD_R_TEXT */

/*
 * OPTION: Delay the loading of the "v_text" array until it is actually
 * needed, saving ~1K, but "destroying" the "vault" generation.
 */
/* #define DELAY_LOAD_V_TEXT */


/*
 * OPTION: Handle signals
 */
#define HANDLE_SIGNALS


/*
 * OPTION: Allow "Wizards" to yield "high scores"
 */
/* #define SCORE_WIZARDS */

/*
 * OPTION: Allow "Borgs" to yield "high scores"
 */
/* #define SCORE_BORGS */

/*
 * OPTION: Allow "Cheaters" to yield "high scores"
 */
/* #define SCORE_CHEATERS */


/*
 * OPTION: Gamma correct colours (with X11 / windows)
 */
#define SUPPORT_GAMMA


/*
 * OPTION: Check the modification time of *_info.raw files
 */
#define CHECK_MODIFICATION_TIME


#ifdef USE_SPECIAL

/*
 * OPTION: Allow the use of "sound" in various places.
 */
#define USE_SOUND

/*
 * OPTION: Allow the use of "graphics" in various places
 */
#define USE_GRAPHICS

/*
 * OPTION: Allow the use of "music" in various places
 */
/* #define USE_MUSIC */

#endif /* USE_SPECIAL */

/*
 * Hack -- Macintosh stuff
 */
#ifdef MACINTOSH

/* Do not handle signals */
# undef HANDLE_SIGNALS

#endif


/*
 * Hack -- Windows stuff
 */
#ifdef WINDOWS

/* Do not handle signals */
# undef HANDLE_SIGNALS

#endif


/*
 * Hack -- EMX stuff
 */
#ifdef USE_EMX

/* Do not handle signals */
# undef HANDLE_SIGNALS

#endif


/*
 * OPTION: Set the "default" path to the angband "lib" directory.
 *
 * See "main.c" for usage, and note that this value is only used on
 * certain machines, primarily Unix machines.
 *
 * The configure script overrides this value.  Check the "--prefix=<dir>"
 * option of the configure script.
 *
 * This value will be over-ridden by the "ANGBAND_PATH" environment
 * variable, if that variable is defined and accessable.  The final
 * "slash" is required if the value supplied is in fact a directory.
 *
 * Using the value "./lib/" below tells Angband that, by default,
 * the user will run "angband" from the same directory that contains
 * the "lib" directory.  This is a reasonable (but imperfect) default.
 *
 * If at all possible, you should change this value to refer to the
 * actual location of the "lib" folder, for example, "/tmp/angband/lib/"
 * or "/usr/games/lib/angband/", or "/pkg/angband/lib".
 */
#ifndef DEFAULT_PATH
# define DEFAULT_PATH "./lib/"
#endif /* DEFAULT_PATH */



/*
 * OPTION: For some brain-dead computers with no command line interface,
 * namely Macintosh, there has to be some way of "naming" your savefiles.
 * The current "Macintosh" hack is to make it so whenever the character
 * name changes, the savefile is renamed accordingly.  But on normal
 * machines, once you manage to "load" a savefile, it stays that way.
 * Macintosh is particularly weird because you can load savefiles that
 * are not contained in the "lib:save:" folder, and if you change the
 * player's name, it will then save the savefile elsewhere.
 */
#if defined(MACINTOSH) || defined(WINDOWS) || defined(AMIGA)
# define SAVEFILE_MUTABLE
#endif


/*
 * OPTION: Prevent usage of the "ANGBAND_PATH" environment variable and
 * the '-d<what>=<path>' command line option (except for '-du=<path>').
 *
 * This prevents cheating in multi-user installs as well as possible
 * security problems when running setgid.
 */
#ifdef SET_UID
#define FIXED_PATHS
#endif /* SET_UID */


/*
 * OPTION: Capitalize the "user_name" (for "default" player name)
 * This option is only relevant on SET_UID machines.
 */
#define CAPITALIZE_USER_NAME

/*
 * OPTION: Person to bother if something goes wrong.
 */
#define MAINTAINER	"sfuerst@zangband.org"


/*
 * OPTION: Default font (when using X11).
 */
#define DEFAULT_X11_FONT		"fixed"

/*
 * OPTION: Default fonts (when using X11)
 */
#define DEFAULT_X11_FONT_0		"10x20"
#define DEFAULT_X11_FONT_1		"9x15"
#define DEFAULT_X11_FONT_2		"9x15"
#define DEFAULT_X11_FONT_3		"5x8"
#define DEFAULT_X11_FONT_4		"5x8"
#define DEFAULT_X11_FONT_5		"5x8"
#define DEFAULT_X11_FONT_6		"5x8"
#define DEFAULT_X11_FONT_7		"5x8"

/*
 * OPTION: Attempt to prevent all "cheating"
 */
/* #define VERIFY_HONOR */


/* Do we want different characters for different races? */
/*
 * Too slow for general use - note that the 16x16 tiles use a
 * much faster version.
 */
/* # define VARIABLE_PLAYER_GRAPH */

/* For longer martial arts descriptions */
# define VERBOSE_MARTIAL_ARTS

/* Allow hordes of 'similar' monsters */
# define MONSTER_HORDES

/* Allow Klackon- and Sprite-Monks to get extra speed
 *
 * undefined by default because
 * Klackons and Sprites are not *supposed* to be
 * playing monks in the first place
 */
/* #define MONK_HACK */

/*
 * Add pillar tunnels (Annoying)
 */
/* #define PILLAR_TUNNELS */

/*
 * Optional use of 64bit type
 */
/* #define USE_64B */

/*
 * Allow execution of arbitrary lua scripts using
 * the '@' debug command.  (Insecure)
 */
/* #define DEBUG_SCRIPTS */

