/* File: h-basic.h */

#ifndef INCLUDED_H_BASIC_H
#define INCLUDED_H_BASIC_H

/*
 * The most basic "include" file.
 *
 * This file includes other low level header files.
 */

#ifdef HAVE_CONFIG_H
#include "autoconf.h"
#endif /* HAVE_CONFIG_H */

/* System Configuration */
#include "h-config.h"



/*** Include the library header files ***/

/** ANSI C headers **/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


/** POSIX headers **/

#if !defined(NeXT) && !defined(RISCOS)
# include <fcntl.h>
#endif

#ifdef SET_UID
# include <pwd.h>
# include <sys/stat.h>
# include <sys/types.h>
# include <unistd.h>
#endif

#if defined(__DJGPP__) || defined(__MWERKS__)
#include <unistd.h>
#endif /* __DJGPP__ || __MWERKS__ */


/*** Other headers ***/

#ifdef MACINTOSH
# include <unix.h>
#endif

#if defined(WINDOWS) || defined(MSDOS)
# include <io.h>
#endif



/*** Define the basic game types ***/

/*
 * cptr is a shortcut type for "const char *".  XXX
 * errr is an error code
 *
 * A "byte" is an unsigned byte of memory.
 * s16b/u16b are exactly 2 bytes (where possible)
 * s32b/u32b are exactly 4 bytes (where possible)
 */

/* C++ defines its own bool type, so we hack around it */
#undef bool
#define bool bool_hack


typedef const char *cptr;
typedef int errr;


/* Simple True/False type */
typedef char bool;

/*
 * The constants "TRUE" and "FALSE"
 */

#undef TRUE
#define TRUE	1

#undef FALSE
#define FALSE	0


/* An unsigned byte of memory */
typedef unsigned char byte;

/* Signed/Unsigned 16 bit value */
typedef signed short s16b;
typedef unsigned short u16b;

/* Signed/Unsigned 32 bit value */

/* Detect >32-bit longs */
#if (UINT_MAX == 0xFFFFFFFFUL) && (ULONG_MAX > 0xFFFFFFFFUL)
typedef signed int s32b;
typedef unsigned int u32b;
#else
typedef signed long s32b;
typedef unsigned long u32b;
#endif



/*** Simple constants ***/


/* Hack -- Define NULL */
#ifndef NULL
# define NULL ((void*)0)
#endif /* NULL */



/*** Basic math macros ***/


/*
 * Non-typed minimum value macro
 *
 * Do not use any of these with values that can vary between one call of
 * "a" or "b" and another.  Pre-calculate "a" and "b" if in any doubt.
 */
#undef MIN
#define MIN(a,b)	(((a) > (b)) ? (b)  : (a))

/*
 * Non-typed maximum value macro
 */
#undef MAX
#define MAX(a,b)	(((a) < (b)) ? (b)  : (a))

/*
 * Non-typed absolute value macro
 */
#undef ABS
#define ABS(a)		(((a) < 0)   ? (-(a)) : (a))

/*
 * Non-typed sign extractor macro
 */
#undef SGN
#define SGN(a)		(((a) < 0)   ? (-1) : ((a) != 0))


/*** Useful array length macro ***/

/*
 * Given an array, determine how many elements are in the array.
 */
#define N_ELEMENTS(a) (int)(sizeof(a) / sizeof((a)[0]))


/*** Some hackish character manipulation ***/

/*
 * Note that all "index" values must be "lowercase letters", while
 * all "digits" must be "digits".  Control characters can be made
 * from any legal characters.  XXX XXX XXX
 */
#define A2I(X)	((X) - 'a')
#define I2A(X)	((X) + 'a')
#define D2I(X)	((X) - '0')
#define I2D(X)	((X) + '0')
#define KTRL(X)	((X) & 0x1F)
#define UN_KTRL(X)	((X) + 64)
#define ESCAPE	'\033'

/*
 * System-independent definitions for the arrow keys.
 */
#define ARROW_DOWN	'\x8A'
#define ARROW_LEFT	'\x8B'
#define ARROW_RIGHT	'\x8C'
#define ARROW_UP	'\x8D'



#endif /* INCLUDED_H_BASIC_H */


