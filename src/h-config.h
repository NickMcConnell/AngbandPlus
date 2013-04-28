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


/*
 * OPTION: Compile on a Macintosh machine
 */
#ifndef MACINTOSH
/* #define MACINTOSH */
#endif

/*
 * OPTION: Compile on a Windows machine (if the compiler doesn't detect; see below)
 */
#ifndef WINDOWS
/* #define WINDOWS */
#endif

/*
 * OPTION: Compile on an MSDOS machine (if the compiler doesn't detect; see below)
 */
#ifndef MSDOS
/* #define MSDOS */
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
    !defined(MSDOS) && !defined(RISCOS)
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
#if defined(WINDOWS) || defined(WINNT) || defined(WIN32) || defined(MSDOS)
# undef PATH_SEP
# define PATH_SEP "\\"
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
 * "USE_SDL" (allow use with the SDL library)
 */


/*
 * OPTION: Include "ncurses.h" instead of "curses.h" in "main-gcu.c"
 */
/* #define USE_NCURSES */


/*
 * OPTION: Assume that setegid is available,
 * if not undefine to use setgid instead.
 */
#define HAVE_SETEGID


/*
 * OPTION: Create and use a hidden directory in the user's home directory
 * for storing pref-files and character-dumps.
 */
#ifdef SET_UID
#define PRIVATE_USER_PATH "~/.angband"
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
 * OPTION: Prevent usage of the "SANGBAND_PATH" environment variable and
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

/*
 * Hack -- Windows stuff
 */
#ifdef WINDOWS

/* Do not handle signals */
# undef HANDLE_SIGNALS

#endif



/*
 * OPTION: Check the modification time of *.raw files
 */
#define CHECK_MODIFICATION_TIME


/*
 * OPTION: Define "HAVE_USLEEP" only if "usleep()" exists.
 *
 * Note that this is only relevant for "SET_UID" machines.
 * Note that new "SOLARIS" and "SGI" machines have "usleep()".
 */
#ifdef SET_UID
#  define HAVE_USLEEP
#endif


/*
 * On ports that use the "curses" library, hitting the escape key causes
 * up to a one second delay.  On such machines, it is very helpful to
 * provide an alternative.
 */
#ifdef USE_GCU
# define USE_BACKQUOTE_AS_ESCAPE
#endif /* USE_GCU */



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



/*
 * Compiler-specific stuff below...
 */

/* Hack -- Warning suppression for Visual C++ */
#ifdef _MBCS
  #pragma warning(disable : 4244  4761)
  /* 4244:  conversion from 'type1' to 'type2', possible loss of data */
  /* 4761:  integral size mismatch in argument; conversion supplied */
#endif



#endif  /* INCLUDED_H_CONFIG_H */
