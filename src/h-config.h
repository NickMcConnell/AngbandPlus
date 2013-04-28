/*
 * File: h-config.h
 *
 * All system-specific options go here.
 *
 * Choose the hardware, operating system, compiler, and (in UNIX/LINUX)
 * various libraries.  Choose system-specific path separators, handle
 * multi-user machines.
 *
 * Also, choose various "system level" compilation options.
 * A lot of these definitions take effect in "h-system.h"
 *
 * Note that most of these "options" are defined by the compiler,
 * the "Makefile", the "project file", system libraries or something
 * similar, and should not be defined by the user.
 */

#ifndef INCLUDED_H_CONFIG_H
#define INCLUDED_H_CONFIG_H

#include <limits.h>

/*
 * OPTION: Compile on a Macintosh machine
 */
#ifndef MACINTOSH
/* #define MACINTOSH */
#endif

/*
 * OPTION: Compile on a Windows machine
 */
#ifndef WINDOWS
/* #define WINDOWS */
#endif

/*
 * OPTION: Compile on an MSDOS machine
 */
#ifndef MSDOS
/* #define MSDOS */
#endif

/*
 * OPTION: Compile on a SYS III version of UNIX
 */
#ifndef SYS_III
/* #define SYS_III */
#endif

/*
 * OPTION: Compile on a SYS V version of UNIX
 */
#ifndef SYS_V
/* #define SYS_V */
#endif

/*
 * OPTION: Compile on a HPUX version of UNIX
 */
#ifndef HPUX
/* #define HPUX */
#endif

/*
 * OPTION: Compile on an SGI running IRIX
 */
#ifndef SGI
/* #define SGI */
#endif

/*
 * OPTION: Compile on a SunOS machine
 */
#ifndef SUNOS
/* #define SUNOS */
#endif

/*
 * OPTION: Compile on a Solaris machine
 */
#ifndef SOLARIS
/* #define SOLARIS */
#endif

/*
 * OPTION: Compile on an ultrix/4.2BSD/Dynix/etc. version of UNIX,
 * Do not define this if you are on any kind of SunOS.
 */
#ifndef ULTRIX
/* #define ULTRIX */
#endif



/*
 * Extract the "SUNOS" flag from the compiler
 */
#if defined(sun)
# ifndef SUNOS
#   define SUNOS
# endif
#endif

/*
 * Extract the "ULTRIX" flag from the compiler
 */
#if defined(ultrix) || defined(Pyramid)
# ifndef ULTRIX
#  define ULTRIX
# endif
#endif

/*
 * Extract the "ATARI" flag from the compiler [cjh]
 */
#if defined(__atarist) || defined(__atarist__)
# ifndef ATARI
#  define ATARI
# endif
#endif

/*
 * Extract the "RISCOS" flag from the compiler
 */
#ifdef __riscos
# ifndef RISCOS
#  define RISCOS
# endif
#endif

/*
 * Extract the "SGI" flag from the compiler
 */
#ifdef sgi
# ifndef SGI
#  define SGI
# endif
#endif

/*
 * Extract the "MSDOS" flag from the compiler
 */
#ifdef __MSDOS__
# ifndef MSDOS
#  define MSDOS
# endif
#endif

/*
 * Extract the "WINDOWS" flag from the compiler
 */
#if defined(_Windows) || defined(__WINDOWS__) || \
    defined(__WIN32__) || defined(WIN32) || \
    defined(__WINNT__) || defined(__NT__)
# ifndef WINDOWS
#  define WINDOWS
# endif
#endif

/*
 * Remove the MSDOS flag when using WINDOWS
 */
#ifdef WINDOWS
# ifdef MSDOS
#  undef MSDOS
# endif
#endif

/*
 * Remove the WINDOWS flag when using MACINTOSH
 */
#ifdef MACINTOSH
# ifdef WINDOWS
#  undef WINDOWS
# endif
#endif



/*
 * OPTION: Define "L64" if a "long" is 64-bits.  See "h-types.h".
 * The only such platform that angband is ported to is currently
 * DEC Alpha AXP running OSF/1 (OpenVMS uses 32-bit longs).
 *
 * Try to use __WORDSIZE to test for 64-bit platforms.
 * I don't know how portable this is.
 * -CJN-
 */
#ifdef __WORDSIZE
# if __WORDSIZE == 64
#  define L64
# endif
#endif

#if defined(__alpha) && defined(__osf__) && !defined(L64)
# define L64
#endif



/*
 * OPTION: set "SET_UID" if the machine is a "multi-user" machine.
 * This option is used to verify the use of "uids" and "gids" for
 * various "UNIX" calls, and of "pids" for getting a random seed,
 * and of the "umask()" call for various reasons, and to guess if
 * the "kill()" function is available, and for permission to use
 * functions to extract user names and expand "tildes" in filenames.
 * It is also used for "locking" and "unlocking" the score file.
 * Basically, SET_UID should *only* be set for "UNIX" machines,
 * or for the "Atari" platform which is Unix-like, apparently.
 */
#if !defined(MACINTOSH) && !defined(WINDOWS) && \
    !defined(MSDOS) && !defined(USE_EMX) && \
    !defined(AMIGA) && !defined(RISCOS)
# define SET_UID
#endif


/*
 * Every system seems to use its own symbol as a path separator.
 * Default to the standard UNIX slash, but attempt to change this
 * for various other systems.  Note that any system that uses the
 * "period" as a separator (i.e. RISCOS) will have to pretend that
 * it uses the slash, and do its own mapping of period <-> slash.
 */
#undef PATH_SEP
#define PATH_SEP "/"
#ifdef MACINTOSH
# undef PATH_SEP
# define PATH_SEP ":"
#endif
#if defined(WINDOWS) || defined(WINNT)
# undef PATH_SEP
# define PATH_SEP "\\"
#endif
#if defined(MSDOS) || defined(OS2) || defined(USE_EMX)
# undef PATH_SEP
# define PATH_SEP "\\"
#endif
#ifdef AMIGA
# undef PATH_SEP
# define PATH_SEP "/"
#endif
#ifdef __GO32__
# undef PATH_SEP
# define PATH_SEP "/"
#endif



/*
 * The Macintosh allows the use of a "file type" when creating a file
 */
#if defined(MACINTOSH) || defined(MACH_O_CARBON)
# define FILE_TYPE_TEXT 'TEXT'
# define FILE_TYPE_DATA 'DATA'
# define FILE_TYPE_SAVE 'SAVE'
# define FILE_TYPE(X) (_ftype = (X))
#else
# define FILE_TYPE(X) ((void)0)
#endif


/*
 * OPTION: See the Makefile(s), where several options may be declared.
 *
 * Options for unix machines include:
 * "USE_GCU" (allow use with UNIX "curses"),
 * "USE_X11" (allow basic use with UNIX X11),
 * "USE_GTK" (allow use with the GTK widget set library)
 *
 * You may also need to specify the "system", using defines such as
 * "SOLARIS" (for Solaris), etc, see "h-config.h" for more info.
 */


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


/*
 * OPTION: Create and use a hidden directory in the user's home directory
 * for storing pref-files and character-dumps.
 */
#ifdef SET_UID
#define PRIVATE_USER_PATH "~/.sangband"
#endif /* SET_UID */


/*
 * On multiuser systems, add the "uid" to savefile names
 */
#ifdef SET_UID
# define SAVEFILE_USE_UID
#endif /* SET_UID */


/*
 * Hack -- Mach-O (native binary format of OS X) is basically a Un*x
 * but has Mac OS/Windows-like user interface
 */
#ifdef MACH_O_CARBON
# ifdef SAVEFILE_USE_UID
#  undef SAVEFILE_USE_UID
# endif
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
 * OPTION: Handle signals
 */
#define HANDLE_SIGNALS


/*
 * Hack -- Macintosh stuff
 */
#ifdef MACINTOSH

/* Do not handle signals */
# undef HANDLE_SIGNALS

#endif

#if 0 /* We're testing signals in Windows */
/*
 * Hack -- Windows stuff
 */
#ifdef WINDOWS

/* Do not handle signals */
# undef HANDLE_SIGNALS

#endif
#endif

/*
 * Hack -- EMX stuff
 */
#ifdef USE_EMX

/* Do not handle signals */
# undef HANDLE_SIGNALS

#endif




/*
 * OPTION: Default font (when using X11).
 */
#define DEFAULT_X11_FONT		"-angband-8x12x-*-iso8859-1"

/*
 * OPTION: Default fonts (when using X11)
 */
#define DEFAULT_X11_FONT_0		"-angband-8x12x-*-iso8859-1"
#define DEFAULT_X11_FONT_1		"-angband-6x10x-*-iso8859-1"
#define DEFAULT_X11_FONT_2		"-angband-6x10x-*-iso8859-1"
#define DEFAULT_X11_FONT_3		"-angband-6x10x-*-iso8859-1"
#define DEFAULT_X11_FONT_4		"-angband-6x10x-*-iso8859-1"
#define DEFAULT_X11_FONT_5		"-angband-6x10x-*-iso8859-1"
#define DEFAULT_X11_FONT_6		"-angband-6x10x-*-iso8859-1"
#define DEFAULT_X11_FONT_7		"-angband-6x10x-*-iso8859-1"


/*
 * OPTION: Default small font (when using X11) (only for main window in 50-line mode).
 */
#define DEFAULT_X11_SFONT_0		"-angband-8x8x-*-iso8859-1"


/*
 * OPTION: Define "HAVE_USLEEP" only if "usleep()" exists.
 *
 * Note that this is only relevant for "SET_UID" machines.
 * Note that new "SOLARIS" and "SGI" machines have "usleep()".
 */
#ifdef SET_UID
# if !defined(ULTRIX) && !defined(ISC)
#  define HAVE_USLEEP
# endif
#endif


/*
 * On ports that use the "curses" library, hitting the escape key causes
 * up to a one second delay.  On such machines, it is very helpful to
 * provide an alternative.
 */
#ifdef USE_GCU
# define USE_BACKQUOTE_AS_ESCAPE
#endif /* USE_GCU */

/*
 * OPTION: Check the modification time of *.raw files
 */
#define CHECK_MODIFICATION_TIME


/*
 * OPTION: Gamma correct colours (with X11)
 */
#define SUPPORT_GAMMA


/* Ensure that NeXT can use fat binaries by default */
#ifdef NeXT

# if defined(m68k)
#  define FAT_SUFFIX_DEFAULT   "m68k"
# endif

# if defined(i386)
#  define FAT_SUFFIX_DEFAULT   "i386"
# endif

# if defined(sparc)
#  define FAT_SUFFIX_DEFAULT   "sparc"
# endif

# if defined(hppa)
#  define FAT_SUFFIX_DEFAULT   "hppa"
# endif

#endif


#endif  /* INCLUDED_H_CONFIG_H */
