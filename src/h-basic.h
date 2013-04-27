/*
 * File: h-basic.h
 *
 * The most basic "include" file.
 */

#ifndef INCLUDED_H_BASIC_H
#define INCLUDED_H_BASIC_H

#ifdef HAVE_CONFIG_H 
# include "autoconf.h" 
#else 
/*
 * OPTION: Compile on a Macintosh machine
 */
/* #define MACINTOSH */

/*
 * OPTION: Compile on a Windows machine
 */
/* #define WINDOWS */

/*
 * OPTION: Compile on an MSDOS machine
 */
/* #define MSDOS */

/*
 * OPTION: Compile on a SYS III version of UNIX
 */
/* #define SYS_III */

/*
 * OPTION: Compile on a SYS V version of UNIX
 */
/* #define SYS_V */

/*
 * OPTION: Compile on a HPUX version of UNIX
 */
/* #define HPUX */

/*
 * OPTION: Compile on an SGI running IRIX
 */
/* #define SGI */

/*
 * OPTION: Compile on a SunOS machine
 */
/* #define SUNOS */

/*
 * OPTION: Compile on a Solaris machine
 */
/* #define SOLARIS */

/*
 * OPTION: Compile on an ultrix/4.2BSD/Dynix/etc. version of UNIX,
 * Do not define this if you are on any kind of SunOS.
 */
/* #define ULTRIX */



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
 * Extract the "WINDOWS" flag from the compiler 
 */ 
# if defined(_Windows) || defined(__WINDOWS__) || \
	defined(__WIN32__) || defined(WIN32) || \
	defined(__WINNT__) || defined(__NT__) 
#  ifndef WINDOWS 
#   define WINDOWS 
#  endif 
# endif 

#ifdef __cplusplus
/* in C++, assume we have stdint but not stdbool */
#  define HAVE_STDINT_H 
#else
/* 
 * Using C99, assume we have stdint and stdbool 
 */ 
# if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L 
#  define HAVE_STDINT_H 
#  define HAVE_STDBOOL_H 
# endif 
#endif

/* 
 * Everyone except RISC OS has fcntl.h and sys/stat.h
 */ 
#ifndef RISCOS 
# define HAVE_FCNTL_H 
# define HAVE_STAT
#endif 

#endif /* HAVE_CONFIG_H */ 

/*
 * Choose the hardware, operating system, and compiler.
 * Also, choose various "system level" compilation options.
 * A lot of these definitions take effect in "h-basic.h"
 *
 * Note that most of these "options" are defined by the compiler,
 * the "Makefile", the "project file", or something similar, and
 * should not be defined by the user.
 */

/*
 * OPTION: use the Boost libraries
 */
/* #define HAVE_BOOST */


/*
 * OPTION: set "SET_UID" if the machine is a "multi-user" machine.
 * This option is used to verify the use of "uids" and "gids" for
 * various "Unix" calls, and of "pids" for getting a random seed,
 * and of the "umask()" call for various reasons, and to guess if
 * the "kill()" function is available, and for permission to use
 * functions to extract user names and expand "tildes" in filenames.
 * It is also used for "locking" and "unlocking" the score file.
 * Basically, SET_UID should *only* be set for "Unix" machines,
 * or for the "Atari" platform which is Unix-like, apparently
 */
#if !defined(MACINTOSH) && !defined(MACH_O_CARBON) && !defined(WINDOWS) && \
    !defined(MSDOS) && !defined(USE_EMX) && \
    !defined(AMIGA) && !defined(RISCOS) && !defined(VM)
# define SET_UID

/* Without autoconf, turn on some things */ 
# ifndef HAVE_CONFIG_H 
#  define HAVE_DIRENT_H 
#  define HAVE_SETEGID 
#  if defined(linux) 
#   define HAVE_SETRESGID 
#  endif 
# endif 
#endif

/*
 * OPTION: Set "USG" for "System V" versions of Unix
 * This is used to choose a "lock()" function, and to choose
 * which header files ("string.h" vs "strings.h") to include.
 * It is also used to allow certain other options, such as options
 * involving userid's, or multiple users on a single machine, etc.
 */
#ifdef SET_UID
# if defined(SYS_III) || defined(SYS_V) || defined(SOLARIS) || \
     defined(HPUX) || defined(SGI) || defined(ATARI)
#  ifndef USG
#   define USG
#  endif
# endif
#endif


/*
 * Every system seems to use its own symbol as a path separator.
 * Default to the standard Unix slash, but attempt to change this
 * for various other systems.  Note that any system that uses the
 * "period" as a separator (i.e. RISCOS) will have to pretend that
 * it uses the slash, and do its own mapping of period <-> slash.
 * Note that the VM system uses a "flat" directory, and thus uses
 * the empty string for "PATH_SEP".
 */
#undef PATH_SEP
#define PATH_SEP "/"

#ifdef MACINTOSH
# undef PATH_SEP
# define PATH_SEP ":"
#endif

#if defined(WINDOWS) || defined(WINNT) ||  defined(MSDOS) || defined(OS2) || defined(USE_EMX)
# undef PATH_SEP
# define PATH_SEP "\\"
#endif

#if defined(AMIGA) || defined(__GO32__)
# undef PATH_SEP
# define PATH_SEP "/"
#endif

#ifdef VM
# undef PATH_SEP
# define PATH_SEP ""
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

/* Mac OS X has usleep(). */
#ifdef MACH_O_CARBON
# define HAVE_USLEEP
#endif

/*
 * OPTION: Define "HAVE_USLEEP" only if "usleep()" exists.
 *
 * Note that this is only relevant for "SET_UID" machines.
 *
 * (Set in autoconf.h when HAVE_CONFIG_H -- i.e. when configure is used.)
 */
#if defined(SET_UID) && !defined(HAVE_CONFIG_H)
# if !defined(HPUX) && !defined(ULTRIX) && !defined(ISC)
#  define HAVE_USLEEP
# endif
#endif

/*** Include the library header files ***/

/* Use various POSIX functions if available */
#undef _GNU_SOURCE
#define _GNU_SOURCE

/*** ANSI C headers ***/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <assert.h>

#include <stdarg.h>
#include <stdio.h>
#if defined(NeXT)  /* BAD - NeXT uses non-standard headers!  XXX XXX */
# include <libc.h>
#else
# include <stdlib.h>
#endif
#include <string.h>
#include <time.h>

/* memory.h and mem.h should map to string.h */

/*** POSIX headers ***/

#if defined(SET_UID) || defined(MACH_O_CARBON)
# include <pwd.h>
# include <sys/stat.h>
# include <unistd.h>
#endif

#if defined(__DJGPP__) || defined(__MWERKS__)
#include <unistd.h>
#endif /* __DJGPP__ || __MWERKS__ */


/*** Other headers ***/
#include "static_assert.h"

#if defined(MACINTOSH) && defined(__MWERKS__)
# include <unix.h>
#endif

#if defined(WINDOWS) || defined(MSDOS) || defined(USE_EMX)
# include <io.h>
#endif

#ifdef SET_UID

# ifndef USG
#  include <sys/param.h>
#  include <sys/file.h>
# endif

# ifdef linux
#  include <sys/file.h>
# endif

# if defined(SOLARIS)
#  include <netdb.h>
# endif

#endif /* SET_UID */

/*** Define the basic game types ***/

/*
 * errr is an error code
 *
 * A "byte" is an unsigned byte of memory.
 * s16b/u16b are exactly 2 bytes (where possible)
 * s32b/u32b are exactly 4 bytes (where possible)
 * C++ comes with its own bool type...assume C99 stdbool header otherwise.
 */

typedef int errr;
typedef unsigned char byte;

/* while C99 and C++/97 guarantee that an unsigned char is a byte (1==sizeof(unsigned char)), they
 * don't guarantee that a byte is 8 bits.  Porting the savefile to platforms where a byte isn't 8 bits requires work, so
 * warn. */
#if CHAR_BIT!=8
#warning "Zaiband savefile format and internals assume 8-bit byte; severe porting work needed."
#endif

#ifndef __cplusplus
/* if this doesn't work, use the one in the freezer */
#include <stdbool.h>
#endif

/* C99/stdint.h provide guaranteed-size ints */
#ifdef HAVE_STDINT_H

  /* Use guaranteed-size types */
  #include <stdint.h>

  typedef uint16_t u16b;
  typedef int16_t s16b;

  typedef uint32_t u32b;
  typedef int32_t s32b;

#else /* __STDC__ */

  /* Try hacks instead (not guaranteed to work) */
  typedef signed short s16b;
  typedef unsigned short u16b;

  /* Detect >32-bit longs */ 
  #if (UINT_MAX == 0xFFFFFFFFUL) && (ULONG_MAX > 0xFFFFFFFFUL)
    typedef signed int s32b;
    typedef unsigned int u32b;
  #else
    typedef signed long s32b;
    typedef unsigned long u32b;
  #endif

#endif /* __STDC__ */

#define MAX_S16B	((s16b)((u16b)(-1)/2))
#define MAX_S32B	((s32b)((u32b)(-1)/2))

/*** Simple constants ***/

/* Define NULL, for pre-C89 compilers */
#ifndef NULL
# define NULL ((void*)0)
#endif

/* Define "TRUE" and "FALSE" */
#undef TRUE
#undef FALSE

#define TRUE	true
#define FALSE	false

/** Debugging macros ***/ 
#define DSTRINGIFY(x) #x 
#define DSTRING(x)    DSTRINGIFY(x) 
#define DHERE         __FILE__ ":" DSTRING(__LINE__) ": "

/*** Basic math macros ***/

#undef MIN
#undef MAX
#undef ABS
#undef SGN

#define MIN(a,b)	(((a) > (b)) ? (b)  : (a))
#define MAX(a,b)	(((a) < (b)) ? (b)  : (a))
#define ABS(a)		(((a) < 0)   ? (-(a)) : (a))
#define SGN(a)		(((a) < 0)   ? (-1) : ((a) != 0))

/*
 * Given an array, determine how many elements are in the array.
 */
#define N_ELEMENTS(a) (sizeof(a) / sizeof((a)[0]))

/*
 * Return "s" (or not) depending on whether n is singular.
 */
#define PLURAL(n)		((n) == 1 ? "" : "s")

/*** Some hackish character manipulation ***/

/*
 * Note that all "index" values must be "lowercase letters", while
 * all "digits" must be "digits".  Control characters can be made
 * from any legal characters.  XXX XXX XXX
 */
#ifdef VM
#  define A2I(X)	alphatoindex(X)
#  define I2A(X)	indextoalpha(X)
#  define D2I(X)	((X) - '0')
#  define I2D(X)	((X) + '0')
#  define KTRL(X)	((X) & 0x1F)
#  define UN_KTRL(X)	((X) + 64)
#  define ESCAPE	'\033'

/* XXX figure out what these definitions are for VM XXX */
#  define ARROW_DOWN     '\x8A'
#  define ARROW_LEFT     '\x8B'
#  define ARROW_RIGHT    '\x8C'
#  define ARROW_UP       '\x8D'

#  define isarrow(c)     (FALSE)
/* #  define isarrow(c)     ((c >= ARROW_DOWN) && (c <= ARROW_UP)) */
#else
#  define A2I(X)	((X) - 'a')
#  define I2A(X)	((X) + 'a')
#  define D2I(X)	((X) - '0')
#  define I2D(X)	((X) + '0')
#  define KTRL(X)	((X) & 0x1F)
#  define UN_KTRL(X)	((X) + 64)
#  define ESCAPE	'\033'

#  define ARROW_DOWN     '\x8A'
#  define ARROW_LEFT     '\x8B'
#  define ARROW_RIGHT    '\x8C'
#  define ARROW_UP       '\x8D'

#  define isarrow(c)     ((c >= ARROW_DOWN) && (c <= ARROW_UP))
#endif

/* KB: redefine some macros from the Zaimoni.STL for Zaiband */
#ifndef NDEBUG
# ifdef __GNUC__
#  define DEBUG_FAIL_OR_LEAVE(A,B) if (A) {plog(__PRETTY_FUNCTION__); quit(#A);}
#  define DEBUG_DIE_OR_LEAVE(A,B) { plog(__PRETTY_FUNCTION__); quit(A); B; }
# else
#  define DEBUG_FAIL_OR_LEAVE(A,B) if (A) quit(#A)
#  define DEBUG_DIE_OR_LEAVE(A,B) { quit(A); B; }
# endif
# define AUDIT_STATEMENT(A) { plog(#A); A; }
#else
# define DEBUG_FAIL_OR_LEAVE(A,B) if (A) B
# define DEBUG_DIE_OR_LEAVE(A,B) B
# define AUDIT_STATEMENT(A) A
#endif

/* delayed concatenation */
#define ANG_CONCAT(A,B) A##B
#define ANG_EVAL(A) A

/* KB: very temporary hack to toggle on/off whether the new command-loop interface is configured */
/* currently ok for Windows */
/* remove when all backends configured */
#ifdef WINDOWS
#ifndef WIN32_CONSOLE_MODE
#define ZAIBAND_NEW_COMMAND_LOOP 1
#endif
#endif

#endif


