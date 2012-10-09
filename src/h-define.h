/* File: h-define.h */

#ifndef INCLUDED_H_DEFINE_H
#define INCLUDED_H_DEFINE_H

/*
 * Define some simple constants
 */


/*
 * Hack -- Define NULL
 */
#ifndef NULL
# ifdef __STDC__
#  define NULL ((void*)0)
# else
#  define NULL ((char*)0)
# endif /* __STDC__ */
#endif /* NULL */


/*
 * Hack -- assist "main-acn.c" XXX XXX XXX
 */
#ifdef ACORN
# define O_RDONLY 0
# define O_WRONLY 1
# define O_RDWR      2
#endif


/*
 * Hack -- force definitions -- see fd_seek()
 */
#ifndef SEEK_SET
# define SEEK_SET 0
#endif
#ifndef SEEK_CUR
# define SEEK_CUR 1
#endif
#ifndef SEEK_END
# define SEEK_END 2
#endif

/*
 * Hack -- force definitions -- see fd_lock()  XXX XXX XXX
 */
#ifndef F_UNLCK
# define F_UNLCK  0
#endif
#ifndef F_RDLCK
# define F_RDLCK  1
#endif
#ifndef F_WRLCK
# define F_WRLCK  2
#endif


/*
 * The constants "TRUE" and "FALSE"
 */
#define ANGBAND_TRUE  ((bool_angband_hack)(1))
#define ANGBAND_FALSE ((bool_angband_hack)(0))

#ifndef USE_NCURSES
 #undef TRUE
 #define TRUE   ANGBAND_TRUE

 #undef FALSE
 #define FALSE  ANGBAND_FALSE
#endif

/**** Simple "Macros" ****/

/*
 * Force a character to lowercase/uppercase
 */
#define FORCELOWER(A)  ((ang_isupper((A))) ? tolower((A)) : (A))
#define FORCEUPPER(A)  ((ang_islower((A))) ? toupper((A)) : (A))


/*
 * Non-typed minimum value macro
 */
#undef MIN
#define MIN(a,b)  (((a) > (b)) ? (b)  : (a))

/*
 * Non-typed maximum value macro
 */
#undef MAX
#define MAX(a,b)  (((a) < (b)) ? (b)  : (a))

/*
 * Non-typed absolute value macro
 */
#undef ABS
#define ABS(a)    (((a) < 0)   ? (-(a)) : (a))

/*
 * Non-typed sign extractor macro
 */
#undef SGN
#define SGN(a)    (((a) < 0)   ? (-1) : ((a) != 0))


/*
 * Hack -- allow use of "ASCII" and "EBCDIC" for "indexes", "digits",
 * and "Control-Characters".
 *
 * Note that all "index" values must be "lowercase letters", while
 * all "digits" must be "digits".  Control characters can be made
 * from any legal characters.  XXX XXX XXX
 */
#ifdef VM
#  define A2I(X)  alphatoindex(X)
#  define I2A(X)  indextoalpha(X)
#  define D2I(X)  ((char)(X) - '0')
#  define I2D(X)  ((char)(X) + '0')
#  define KTRL(X) ((X) & 0x1F)
#  define ESCAPE  '\033'
#else
#  define A2I(X)  ((int)((char)(X) - 'a'))
#  define I2A(X)  ((char)(X) + 'a')
#  define D2I(X)  ((int)((char)(X) - '0'))
#  define I2D(X)  ((char)(X) + '0')
#  define KTRL(X) ((X) & 0x1F)
#  define ESCAPE  '\033'
#endif


#endif

