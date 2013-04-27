/* File: h-type.h */

#ifndef INCLUDED_H_TYPE_H
#define INCLUDED_H_TYPE_H


/***** Name overrides *****/

#define bool bool_hack
#define sint sint_hack
#define uint uint_hack
#define huge huge_hack


/***** Integer types *****/


/*** 8-bit types ***/

/* Signed byte (possibly broken on certain systems) */
typedef
#ifdef __int8_t_defined
	 int8_t
#else
	 signed char
#endif
	 syte;

/* Unsigned byte */
typedef
#ifdef __int8_t_defined
	 u_int8_t
#else
	 unsigned char
#endif
	 byte;


/*** 16-bit types ***/

/* Signed short */
typedef
#ifdef __int8_t_defined
	 int16_t
#else
	 signed short
#endif
	 s16b;

/* Unsigned short */
typedef
#ifdef __int8_t_defined
	 u_int16_t
#else
	 unsigned short
#endif
	 u16b;


/*** 32-bit types ***/

/* Signed long */
typedef
#ifdef __int8_t_defined
	 int32_t
#elif defined L64
	 signed int
#else
	 signed long
#endif
	 s32b;

/* Unsigned long */
typedef
#ifdef __int8_t_defined
	 u_int32_t
#elif defined L64
	 unsigned int
#else
	 unsigned long
#endif
	 u32b;


#ifdef NO_64_BIT_INT
/*** 64-bit types (unsupported on obsolete compilers) ***/

/* Signed long long */
typedef
#ifdef __int8_t_defined
	 int64_t
#elif defined L64
	 signed long
#else
	 signed long long
#endif
	 s64b;

/* Unsigned long */
typedef
#ifdef __int8_t_defined
	 u_int64_t
#elif defined L64
	 unsigned long
#else
	 unsigned long long
#endif
	 u64b;

typedef u64b huge;

#else /* NO_64_BIT_INT */

typedef u32b huge;

#endif /* NO_64_BIT_INT */


/*** Efficient types (if size is irrelevant) ***/

/* Signed int */
typedef signed int sint;

/* Unsigned int */
typedef unsigned int uint;


/***** Floating point types *****/

/* 16-bit float */
typedef float f16b;

/* 32-bit double */
typedef double f32b;

/* Long double */
typedef long double real;


/***** Character types *****/

/* Unicode char */
typedef wchar_t uchr;

/* String constant */
typedef const char *cptr;

/* String variable */
typedef char *sptr;


/***** Pointers *****/

/* Generic pointer */
typedef void *vptr;


/***** Special types *****/

/* Boolean */
typedef byte bool;

/* Error */
typedef sint errr;

/* Encoded cave location */
typedef u16b grid;

/* Encoded picture information */
typedef u16b pict;

/* Encoded visual information */
typedef u32b visu;

#endif

