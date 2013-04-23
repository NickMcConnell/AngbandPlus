#define H_DEFINE_H
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
# define O_RDWR 2
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
# define F_UNLCK 0
#endif
#ifndef F_RDLCK
# define F_RDLCK 1
#endif
#ifndef F_WRLCK
# define F_WRLCK 2
#endif


/*
 * The constants "TRUE" and "FALSE"
 */

#undef TRUE
#define TRUE 1

#undef FALSE
#define FALSE 0




/**** Simple "Macros" ****/

/*
 * Allow various ctype.h functions to be called as (bool)func(char c)
 */
#define ISALNUM(A) (isalnum((byte)(A)) != 0)
#define ISALPHA(A) (isalpha((byte)(A)) != 0)
/* #define ISBLANK(A) (isblank((byte)(A)) != 0) */
#define ISCNTRL(A) (iscntrl((byte)(A)) != 0)
#define ISDIGIT(A) (isdigit((byte)(A)) != 0)
#define ISGRAPH(A) (isgraph((byte)(A)) != 0)
#define ISLOWER(A) (islower((byte)(A)) != 0)
#define ISPRINT(A) (isprint((byte)(A)) != 0)
/* #define ISPUNCT(A) (ispunct((byte)(A)) != 0) */
#define ISSPACE(A) (isspace((byte)(A)) != 0)
#define ISUPPER(A) (isupper((byte)(A)) != 0)
/* #define ISXDIGIT(A) (isxdigit((byte)(A)) != 0) */

#define TOLOWER(A) tolower((byte)(A))
#define TOUPPER(A) toupper((byte)(A))

/*
 * Force a character to lowercase/uppercase
 */
#define FORCELOWER(A)  ((ISUPPER((A))) ? TOLOWER((A)) : (A))
#define FORCEUPPER(A)  ((ISLOWER((A))) ? TOUPPER((A)) : (A))


/*
 * Non-typed minimum value macro
 */
#undef MIN
#define MIN(a,b) (((a) > (b)) ? (b)  : (a))

/*
 * Non-typed maximum value macro
 */
#undef MAX
#define MAX(a,b) (((a) < (b)) ? (b)  : (a))

/*
 * Non-typed absolute value macro
 */
#undef ABS
#define ABS(a) (((a) < 0)   ? (-(a)) : (a))

/*
 * Non-typed sign extractor macro
 */
#undef SGN
#define SGN(a) (((a) < 0)   ? (-1) : ((a) != 0))

/*
 * An assertion macro
 */
#undef assert

#ifdef NDEBUG
# define assert(ignore) ((void) 0)
#else /* NDEBUG */

/* Dump the core on an assert() failure rather than simply quitting. */
/* #define DEBUG_ASSERT_CORE */

/* Pick whether to save the game before aborting */
#define DEBUG_ASSERT_SAVE

/* Possibly save the game, and then abort. */
# define assert(expr)\
		if (!(expr)) assert_fail(#expr, __FILE__, __LINE__)
#endif /* NDEBUG */



/*
 * Given an array, determine how many elements are in the array.
 */
#define N_ELEMENTS(a) (sizeof(a) / sizeof((a)[0]))

/*
 * Given an array, find the pointer at the end of it.
 */
#define END_PTR(X) ((X) + N_ELEMENTS(X))

/*
 * Given an array with known size, set a pointer to each element in turn with
 * a for loop.
 */
#define FOR_ALL_IN(ARRAY, PTR) \
	for ((PTR) = (ARRAY); (PTR) < END_PTR(ARRAY); (PTR)++)

/* Try to mark unused variables as such in a way the compiler understands. */
#if defined(__GNUC__) || defined(__TINYC__)
#define UNUSED __attribute__((__unused__))
#else
#define UNUSED
#endif

#define GNUC_VERSION (__GNUC__*100 + __GNUC_MINOR__*10 + __GNUC_PATCHLEVEL__)

/* Indicate functions which have no side-effects if possible. */
#if defined(__GNUC__) && (296 <= GNUC_VERSION)
#define PURE __attribute__((__pure__))
#else
#define PURE
#endif

/* Avoid variable-length arrays unless allowed and supported. */
#ifndef NO_VARIABLE_ARRAYS
#if defined(__GNUC__) || \
	(defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L))
#define VARIABLE_ARRAYS
# endif
#endif

/* Allow real types to be defined (note that "real" is a type for MSVC). */
/* #define USE_FLOAT */


/*
 * Hack -- allow use of "ASCII" and "EBCDIC" for "indexes", "digits",
 * and "Control-Characters".
 *
 * Note that all "index" values must be "lowercase letters", while
 * all "digits" must be "digits".  Control characters can be made
 * from any legal characters.  XXX XXX XXX
 */
#ifdef VM
#  define A2I(X) alphatoindex(X)
#  define I2A(X) indextoalpha(X)
#  define D2I(X) ((X) - '0')
#  define I2D(X) ((X) + '0')
#  define KTRL(X) ((X) & 0x1F)
#  define ESCAPE '\033'
#else
#  define A2I(X) ((X) - 'a')
#  define I2A(X) ((X) + 'a')
#  define D2I(X) ((X) - '0')
#  define I2D(X) ((X) + '0')
#  define KTRL(X) ((X) & 0x1F)
#  define ESCAPE '\033'
#endif


#endif

/* A default value for things which don't really need one, provided to
 * suppress the compile-time errors they generate.
 * Use as (e.g.) "int UNREAD(i);".
 * Defining DEBUG inhibits this, as attempts to use uninitialised variables
 * can then be recognised by debugging tools.
 */
#ifdef DEBUG
#define UNREAD(VAR) VAR
#else /* DEBUG */
#define UNREAD(VAR) VAR = 0
#endif /* DEBUG */

