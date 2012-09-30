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
# endif	/* __STDC__ */
#endif /* NULL */


/*
 * Hack -- assist "main-acn.c" XXX XXX XXX
 */
#ifdef ACORN
# define O_RDONLY	0
# define O_WRONLY	1
# define O_RDWR		2
#endif


/*
 * Hack -- force definitions -- see fd_seek()
 */
#ifndef SEEK_SET
# define SEEK_SET	0
#endif
#ifndef SEEK_CUR
# define SEEK_CUR	1
#endif
#ifndef SEEK_END
# define SEEK_END	2
#endif

/*
 * Hack -- force definitions -- see fd_lock()  XXX XXX XXX
 */
#ifndef F_UNLCK
# define F_UNLCK	0
#endif
#ifndef F_RDLCK
# define F_RDLCK	1
#endif
#ifndef F_WRLCK
# define F_WRLCK	2
#endif


/*
 * The constants "TRUE" and "FALSE"
 */

#undef TRUE
#define TRUE	1

#undef FALSE
#define FALSE	0




/**** Simple "Macros" ****/

/*
 * Force a character to lowercase/uppercase
 */
#define FORCELOWER(A)  ((isupper((A))) ? tolower((A)) : (A))
#define FORCEUPPER(A)  ((islower((A))) ? toupper((A)) : (A))


/*
 * Non-typed minimum value macro
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

/*
 * Turn on aborts for the assert macro for now
 */
#define DEBUG_ABORT


/*
 * An assertion macro
 */
#undef assert

/* Pick which type of output to use */
#ifdef DEBUG_CORE
# define ANG__assert_fmt core_fmt
#else /* DEBUG_CORE */
# define ANG__assert_fmt quit_fmt
#endif /* DEBUG_CORE */

/* Pick whether to save the game before aborting */
#ifdef DEBUG_ABORT
# define ANG__assert_save ((void) 0)
#else
# define ANG__assert_save save_player()
#endif


/* Possibly save the game, and then abort. */
#define assert(expr)\
	do\
	{\
		if (!(expr))\
		{\
			signals_ignore_tstp();\
			ANG__assert_save;\
			ANG__assert_fmt("\n%s%s\n%s%s\n%s%d\n\n",\
			"Assertion failed: ", #expr,\
			"in file ", __FILE__,\
			"on line ", __LINE__);\
		}\
	}\
while (FALSE)

/* An assert that can be used in an expression */
#define assert_exp(expr)\
	assert_helper(#expr, __FILE__, __LINE__, (expr))


/*
 * Hack -- allow use of "ASCII" and "EBCDIC" for "indexes", "digits",
 * and "Control-Characters".
 *
 * Note that all "index" values must be "lowercase letters", while
 * all "digits" must be "digits".  Control characters can be made
 * from any legal characters.  XXX XXX XXX
 */
#ifdef VM
#  define A2I(X)	alphatoindex(X)
#  define I2A(X)	indextoalpha(X)
#  define D2I(X)	((X) - '0')
#  define I2D(X)	((char) ((X) + '0'))
#  define KTRL(X)	((X) & 0x1F)
#  define ESCAPE	'\033'
#else
#  define A2I(X)	((X) - 'a')
#  define I2A(X)	((char) ((X) + 'a'))
#  define D2I(X)	((X) - '0')
#  define I2D(X)	((char) ((X) + '0'))
#  define KTRL(X)	((X) & 0x1F)
#  define ESCAPE	'\033'
#endif


/*
 * Hack - a useful "get array index from pointers" macro.
 *
 * We assume P is a pointer to something in array A.
 */
#define GET_ARRAY_INDEX(A,P) \
	((P)-(A))

/*
 * Get number of elements in an array
 */
#define NUM_ELEMENTS(A) \
	(sizeof(A) / sizeof ((A)[0]))

#endif
