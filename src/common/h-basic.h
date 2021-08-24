/*
 * File: h-basic.h
 * Purpose: The most basic "include" file
 */

#ifndef INCLUDED_H_BASIC_H
#define INCLUDED_H_BASIC_H

/*** Autodetect platform ***/

/*
 * Using C99, assume we have stdint and stdbool
 *
 * Note: I build PWMAngband with C++ Builder, which DOES NOT have stdbool
 */

/*
 * Every system seems to use its own symbol as a path separator.
 */
#undef PATH_SEP
#undef PATH_SEPC
#define PATH_SEP "\\"
#define PATH_SEPC '\\'

#define EWOULDBLOCK WSAEWOULDBLOCK
#define ECONNRESET WSAECONNRESET

/*
 * Include the library header files
 */

/** ANSI C headers **/

#include <ctype.h>
#include <stdint.h>
/*#include <stdbool.h>*/
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
#include <fcntl.h>

/* Basic networking stuff */
#include "h-net.h"

/*
 * Define the basic game types
 */

typedef int errr;

/* Use a char otherwise */
typedef char bool;

#define true    1
#define false   0

/* Use guaranteed-size types */
typedef uint8_t byte;

typedef uint16_t u16b;
typedef int16_t s16b;

typedef uint32_t u32b;
typedef int32_t s32b;

typedef uint64_t u64b;
typedef int64_t s64b;

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

/*
 * Basic math macros
 */

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

/*
 * Useful fairly generic macros
 */

/*
 * Given an array, determine how many elements are in it.
 */
#define N_ELEMENTS(a) (sizeof(a) / sizeof((a)[0]))

/*
 * Some hackish character manipulation
 */

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
