/*
 * File: h-basic.h
 * Purpose: The most basic "include" file
 */

#ifndef INCLUDED_H_BASIC_H
#define INCLUDED_H_BASIC_H

/*** Autodetect platform ***/

/*
 * Using C99, assume we have stdint and stdbool
 */
#define HAVE_STDINT_H
/*#define HAVE_STDBOOL_H*/

/*
 * Every system seems to use its own symbol as a path separator.
 */
#undef PATH_SEP
#undef PATH_SEPC
#define PATH_SEP "\\"
#define PATH_SEPC '\\'

#define strcasecmp stricmp

#define EWOULDBLOCK WSAEWOULDBLOCK
#define ECONNRESET WSAECONNRESET

/*** Include the library header files ***/

/** ANSI C headers **/

#include <ctype.h>
#include <errno.h>
#include <assert.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <limits.h>

/** Other headers **/

#include <io.h>
#define strncasecmp strnicmp

#include <fcntl.h>

/* Basic networking stuff */
#include "h-net.h"

/*** Define the basic game types ***/

/* C++ defines its own bool type, so we hack around it */
#undef bool
#define bool bool_hack

typedef int errr;

/*
 * Use a real bool type where possible
 */
#undef TRUE
#undef FALSE
#ifdef HAVE_STDBOOL_H
    #include <stdbool.h>

    #define TRUE  true
    #define FALSE false
#else
    /* Use a char otherwise */
    typedef char bool;

    #define TRUE    1
    #define FALSE   0
#endif

/*
 * Use guaranteed-size ints where possible
 */
#ifdef HAVE_STDINT_H
    /* Use guaranteed-size types */
    #include <stdint.h>

    typedef uint8_t byte;

    typedef uint16_t u16b;
    typedef int16_t s16b;

    typedef uint32_t u32b;
    typedef int32_t s32b;

    typedef uint64_t u64b;
    typedef int64_t s64b;

    #define MAX_UCHAR   UINT8_MAX
    #define MAX_SHORT   INT16_MAX
#else
    /* Try hacks instead (not guaranteed to work) */
    typedef unsigned char byte;
    typedef signed short s16b;
    typedef unsigned short u16b;

    #define MAX_UCHAR   UCHAR_MAX
    #define MAX_SHORT   SHRT_MAX

    /* Detect >32-bit longs */
    #if (UINT_MAX == 0xFFFFFFFFUL) && (ULONG_MAX > 0xFFFFFFFFUL)
        typedef signed int s32b;
        typedef unsigned int u32b;
    #else
        typedef signed long s32b;
        typedef unsigned long u32b;
    #endif
#endif

/* MAngband hacks */
#if (UINT_MAX == 0xFFFFFFFFUL) && (ULONG_MAX > 0xFFFFFFFFUL)
    #define PRId32 "d"
#else
    #define PRId32 "ld"
#endif

/* MAngband types */
typedef int sint;
typedef unsigned int uint;
typedef byte *byte_ptr;

/* Turn counter type "huge turn" (largest number ever) */
#define HTURN_ERA_FLIP 1000000
#define HTURN_ERA_MAX_DIV 1000

typedef struct
{
    u32b era;
    u32b turn;
} hturn;

/*** Basic math macros ***/

#undef MIN
#undef MAX
#undef ABS
#undef SGN
#undef CMP

#define MIN(a,b)    (((a) > (b))? (b)   : (a))
#define MAX(a,b)    (((a) < (b))? (b)   : (a))
#define ABS(a)      (((a) < 0)  ? (-(a)): (a))
#define SGN(a)      (((a) < 0)  ? (-1)  : ((a) != 0))
#define CMP(a,b)    (((a) < (b))? (-1)  : (((b) < (a))? 1: 0))

/*** Useful fairly generic macros ***/

/*
 * Given an array, determine how many elements are in it.
 */
#define N_ELEMENTS(a) (sizeof(a) / sizeof((a)[0]))

/*
 * Return "s" (or not) depending on whether n is singular.
 */
#define PLURAL(n) (((n) == 1)? "": "s")
#define SINGULAR(n) (((n) == 1)? "s": "")

/*** Some hackish character manipulation ***/

/*
 * Note that all "index" values must be "lowercase letters", while
 * all "digits" must be "digits".  Control characters can be made
 * from any legal characters.  XXX XXX XXX
 */
#define A2I(X)      ((X) - 'a')
#define I2A(X)      ((X) + 'a')
#define D2I(X)      ((X) - '0')
#define I2D(X)      ((X) + '0')

/*
 * Force a character to uppercase
 */
#define FORCEUPPER(A)  ((islower((A)))? toupper((A)): (A))

#endif
