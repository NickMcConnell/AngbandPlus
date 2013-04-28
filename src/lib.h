/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* lib.h: service routines, not directly related to the game  */

#ifndef LIB_H_INCLUDED
#define LIB_H_INCLUDED

/* This file merges all old z-*.h files. */

/**** Low-level stuff ****/

#include "system.h"


/*
 * Extremely basic stuff, like global temp and constant variables.
 * Also, some very useful low level functions, such as "streq()".
 * All variables and functions here are "addressable".
 */

/**** Emergency exit ****/

/* A cptr to the name of the program */
extern cptr argv0;

/* Aux functions */
extern void (*plog_aux)(cptr);
extern void (*quit_aux)(cptr);
extern void (*core_aux)(cptr);

/* Print an error message */
void plog(cptr str);

/* Exit, with optional message */
void quit(cptr str);

/* Dump core, with optional message */
void core(cptr str);


/**** String functions ****/

/* Case insensitive comparison between two strings */
int my_stricmp(const char *s1, const char *s2);
int my_strnicmp(cptr a, cptr b, int n);

/* Copy a string */
size_t my_strcpy(char *buf, const char *src, size_t bufsize);

/* Concatenate two strings */
size_t my_strcat(char *buf, const char *src, size_t bufsize);

/* Test equality, prefix, suffix */
bool streq(cptr s, cptr t);
bool prefix(cptr s, cptr t);
bool suffix(cptr s, cptr t);


/*
 * Memory management routines.
 *
 * Set ralloc_aux to modify the memory allocation routine.
 * Set rnfree_aux to modify the memory de-allocation routine.
 * Set rpanic_aux to let the program react to memory failures.
 *
 * These routines work best as a *replacement* for malloc/free.
 *
 * The string_make() and string_free() routines handle dynamic strings.
 * A dynamic string is a string allocated at run-time, which should not
 * be modified once it has been created.
 *
 * Note the macros below which simplify the details of allocation,
 * deallocation, setting, clearing, casting, size extraction, etc.
 *
 * The macros MAKE/C_MAKE and KILL have a "procedural" metaphor,
 * and they actually modify their arguments.
 *
 * Note that, for some reason, some allocation macros may disallow
 * "stars" in type names, but you can use typedefs to circumvent
 * this.  For example, instead of "type **p; MAKE(p,type*);" you
 * can use "typedef type *type_ptr; type_ptr *p; MAKE(p,type_ptr)".
 *
 * Note that it is assumed that "memset()" will function correctly,
 * in particular, that it returns its first argument.
 */

/**** Service macros ****/

/* Size of 'N' things of type 'T' */
#define C_SIZE(N,T) \
	((N)*(sizeof(T)))

/* Size of one thing of type 'T' */
#define SIZE(T) \
	(sizeof(T))


/* Compare two arrays of type T[N], at locations P1 and P2 */
#define C_DIFF(P1,P2,N,T) \
	(memcmp((P1),(P2),C_SIZE(N,T)))

/* Compare two things of type T, at locations P1 and P2 */
#define DIFF(P1,P2,T) \
	(memcmp((P1),(P2),SIZE(T)))


/* Set every byte in an array of type T[N], at location P, to V, and return P */
#define C_BSET(P,V,N,T) \
	(memset((P),(V),C_SIZE(N,T)))

/* Set every byte in a thing of type T, at location P, to V, and return P */
#define BSET(P,V,T) \
	(memset((P),(V),SIZE(T)))


/* Wipe an array of type T[N], at location P, and return P */
#define C_WIPE(P,N,T) \
	(memset((P),0,C_SIZE(N,T)))

/* Wipe a thing of type T, at location P, and return P */
#define WIPE(P,T) \
	(memset((P),0,SIZE(T)))


/* Load an array of type T[N], at location P1, from another, at location P2 */
#define C_COPY(P1,P2,N,T) \
	(memcpy((P1),(P2),C_SIZE(N,T)))

/* Load a thing of type T, at location P1, from another, at location P2 */
#define COPY(P1,P2,T) \
	(memcpy((P1),(P2),SIZE(T)))


/* Allocate, and return, an array of type T[N] */
#define C_RNEW(N,T) \
	(ralloc(C_SIZE(N,T)))

/* Allocate, and return, a thing of type T */
#define RNEW(T) \
	(ralloc(SIZE(T)))


/* Allocate, wipe, and return an array of type T[N] */
#define C_ZNEW(N,T) \
	(C_WIPE(C_RNEW(N,T),N,T))

/* Allocate, wipe, and return a thing of type T */
#define ZNEW(T) \
	(WIPE(RNEW(T),T))


/* Allocate a wiped array of type T[N], assign to pointer P */
#define C_MAKE(P,N,T) \
	((P)=C_ZNEW(N,T))

/* Allocate a wiped thing of type T, assign to pointer P */
#define MAKE(P,T) \
	((P)=ZNEW(T))


/* Free one thing at P, return NULL */
#define FREE(P) \
	(rnfree(P))

/* Free a thing at location P and set P to NULL */
#define KILL(P) \
	((P)=FREE(P))

/* Hooks */

/* Replacement hook for "rnfree()" */
extern void* (*rnfree_aux)(void*);

/* Replacement hook for "rpanic()" */
extern void* (*rpanic_aux)(size_t);

/* Replacement hook for "ralloc()" */
extern void* (*ralloc_aux)(size_t);

/**** Memory allocation routines ****/

/* De-allocate memory */
void* rnfree(void *p);

/* Panic, attempt to allocate 'len' bytes */
void* rpanic(size_t len);

/* Allocate (and return) 'len', or dump core */
void* ralloc(size_t len);

/* Create a "dynamic string" */
cptr string_make(cptr str);

/* Free a string allocated with "string_make()" */
errr string_free(cptr str);


/* String formatting */

/*
 * Provides functions very similar to "sprintf()", but which
 * not only parse some additional "format sequences", but also enforce
 * bounds checking, and allow repeated "appends" to the same buffer.
 *
 * See "z-form.c" for more detailed information about the routines,
 * including a list of the legal "format sequences".
 *
 * This file makes use of both "z-util.c" and "z-virt.c"
 */

/* Format arguments into given bounded-length buffer */
size_t vstrnfmt(char *buf, size_t max, cptr fmt, va_list vp);

/* Simple interface to "vstrnfmt()" */
size_t strnfmt(char *buf, size_t max, cptr fmt, ...);

/* Format arguments into a static resizing buffer */
char *vformat(cptr fmt, va_list vp);

/* Free the memory allocated for the format buffer */
void vformat_kill(void);

/* Append a formatted string to another string */
void strnfcat(char *str, size_t max, size_t *end, cptr fmt, ...);

/* Simple interface to "vformat()" */
char *format(cptr fmt, ...);

/* Vararg interface to "plog()", using "format()" */
void plog_fmt(cptr fmt, ...);

/* Vararg interface to "quit()", using "format()" */
void quit_fmt(cptr fmt, ...);

/* Vararg interface to "core()", using "format()" */
void core_fmt(cptr fmt, ...);


/* The almightly Random Number God itself... */

/*
 * The "degree" of the "complex" Random Number Generator.
 * This value is hard-coded at 63 for a wide variety of reasons.
 */
#define RAND_DEG 63

/* Using these macros is suggested instead of Rand_xxx functions. */

/*
 * Generates a random long integer X where O<=X<M.
 * The integer X falls along a uniform distribution.
 * For example, if M is 100, you get "percentile dice"
 */
#define rand_int(M) \
	((s32b)(Rand_div(M)))


/*
 * Generates a random long integer X where 1<=X<=M.
 *
 * Note that the behaviour for M < 1 is undefined.
 */
#define randint(M) \
	(rand_int(M) + 1)

/*
 * Generates a random long integer X where 1<=X<=M.
 *
 * Note that the behaviour for M < 1 is undefined.
 */
#define rand_die(M) \
	(rand_int(M) + 1)


/*
 * Generates a random long integer X where A<=X<=B
 * The integer X falls along a uniform distribution.
 * Note: rand_range(0,N-1) == rand_int(N)
 */
#define rand_range(A,B) \
	((A) + (rand_int(1+(B)-(A))))

/*
 * Generate a random long integer X where A-D<=X<=A+D
 * The integer X falls along a uniform distribution.
 * Note: rand_spread(A,D) == rand_range(A-D,A+D)
 */
#define rand_spread(A,D) \
	((A) + (rand_int(1+(D)+(D))) - (D))

/*
 * An alternative method of calling "rand_int()".
 * From Zangband.
 * note, function could crash if (X < 1), hence the check
 */
#define one_in_(X) \
	(rand_int(X > 0 ? X : 1) == 0)


/* RNG state */
extern bool Rand_quick;
extern u32b Rand_value;
extern u16b Rand_place;
extern u32b Rand_state[RAND_DEG];

/* RNG functions */
void Rand_state_init(u32b seed);
u32b Rand_div(u32b m);
s16b Rand_normal(int mean, int stand);
u32b Rand_simple(u32b m);
s32b div_round(s32b n, s32b d);


/* Platform-independent terminal support */

/*
 * A term_win is a "window" for a Term
 *
 *	- Cursor Useless/Visible codes
 *	- Cursor Location (see "Useless")
 *
 *	- Array[h] -- Access to the attribute array
 *	- Array[h] -- Access to the character array
 *
 *	- Array[h*w] -- Attribute array
 *	- Array[h*w] -- Character array
 *
 * Note that the attr/char pair at (x,y) is a[y][x]/c[y][x]
 * and that the row of attr/chars at (0,y) is a[y]/c[y]
 */

typedef struct term_win term_win;

struct term_win
{
	bool cu, cv;
	byte cx, cy;

	byte **a;
	char **c;

	byte *va;
	char *vc;

	byte **ta;
	char **tc;

	byte *vta;
	char *vtc;
};

/*
 * An actual "term" structure
 *
 *	- Extra "user" info (used by application)
 *
 *	- Extra "data" info (used by implementation)
 *
 *
 *	- Flag "user_flag"
 *	  An extra "user" flag (used by application)
 *
 *
 *	- Flag "data_flag"
 *	  An extra "data" flag (used by implementation)
 *
 *
 *	- Flag "active_flag"
 *	  This "term" is "active"
 *
 *	- Flag "mapped_flag"
 *	  This "term" is "mapped"
 *
 *	- Flag "total_erase"
 *	  This "term" should be fully erased
 *
 *	- Flag "fixed_shape"
 *	  This "term" is not allowed to resize
 *
 *	- Flag "icky_corner"
 *	  This "term" has an "icky" corner grid
 *
 *	- Flag "soft_cursor"
 *	  This "term" uses a "software" cursor
 *
 *	- Flag "always_pict"
 *	  Use the "Term_pict()" routine for all text
 *
 *	- Flag "higher_pict"
 *	  Use the "Term_pict()" routine for special text
 *
 *	- Flag "always_text"
 *	  Use the "Term_text()" routine for invisible text
 *
 *	- Flag "unused_flag"
 *	  Reserved for future use
 *
 *	- Flag "never_bored"
 *	  Never call the "TERM_XTRA_BORED" action
 *
 *	- Flag "never_frosh"
 *	  Never call the "TERM_XTRA_FROSH" action
 *
 *
 *	- Value "attr_blank"
 *	  Use this "attr" value for "blank" grids
 *
 *	- Value "char_blank"
 *	  Use this "char" value for "blank" grids
 *
 *
 *	- Ignore this pointer
 *
 *	- Keypress Queue -- various data
 *
 *	- Keypress Queue -- pending keys
 *
 *
 *	- Window Width (max 255)
 *	- Window Height (max 255)
 *
 *	- Minimum modified row
 *	- Maximum modified row
 *
 *	- Minimum modified column (per row)
 *	- Maximum modified column (per row)
 *
 *
 *	- Displayed screen image
 *	- Requested screen image
 *
 *	- Temporary screen image
 *	- Memorized screen image
 *
 *
 *	- Hook for init-ing the term
 *	- Hook for nuke-ing the term
 *
 *	- Hook for user actions
 *
 *	- Hook for extra actions
 *
 *	- Hook for placing the cursor
 *
 *	- Hook for drawing some blank spaces
 *
 *	- Hook for drawing a string of chars using an attr
 *
 *	- Hook for drawing a sequence of special attr/char pairs
 */

typedef struct term term;

struct term
{
	void *user;

	void *data;

	bool user_flag;

	bool data_flag;

	bool active_flag;
	bool mapped_flag;
	bool total_erase;
	bool fixed_shape;
	bool icky_corner;
	bool soft_cursor;
	bool always_pict;
	bool higher_pict;
	bool always_text;
	bool unused_flag;
	bool never_bored;
	bool never_frosh;

	byte attr_blank;
	char char_blank;

	char *key_queue;

	u16b key_head;
	u16b key_tail;
	u16b key_xtra;
	u16b key_size;

	byte wid;
	byte hgt;

	byte y1;
	byte y2;

	byte *x1;
	byte *x2;

	term_win *old;
	term_win *scr;

	term_win *tmp;
	term_win *mem;

	void (*init_hook)(term *t);
	void (*nuke_hook)(term *t);

	errr (*user_hook)(int n);

	errr (*xtra_hook)(int n, int v);

	errr (*curs_hook)(int x, int y);

	errr (*bigcurs_hook)(int x, int y);

	errr (*wipe_hook)(int x, int y, int n);

	errr (*text_hook)(int x, int y, int n, byte a, cptr s);

	errr (*pict_hook)(int x, int y, int n, const byte *ap, const char *cp, const byte *tap, const char *tcp);
};

/*
 * Definitions for the "actions" of "Term_xtra()"
 *
 * These values may be used as the first parameter of "Term_xtra()",
 * with the second parameter depending on the "action" itself.  Many
 * of the actions shown below are optional on at least one platform.
 *
 * The "TERM_XTRA_EVENT" action uses "v" to "wait" for an event
 * The "TERM_XTRA_SHAPE" action uses "v" to "show" the cursor
 * The "TERM_XTRA_FROSH" action uses "v" for the index of the row
 * The "TERM_XTRA_SOUND" action uses "v" for the index of a sound
 * The "TERM_XTRA_ALIVE" action uses "v" to "activate" (or "close")
 * The "TERM_XTRA_LEVEL" action uses "v" to "resume" (or "suspend")
 * The "TERM_XTRA_DELAY" action uses "v" as a "millisecond" value
 *
 * The other actions do not need a "v" code, so "zero" is used.
 */
#define TERM_XTRA_EVENT	1	/* Process some pending events */
#define TERM_XTRA_FLUSH 2	/* Flush all pending events */
#define TERM_XTRA_CLEAR 3	/* Clear the entire window */
#define TERM_XTRA_SHAPE 4	/* Set cursor shape (optional) */
#define TERM_XTRA_FROSH 5	/* Flush one row (optional) */
#define TERM_XTRA_FRESH 6	/* Flush all rows (optional) */
#define TERM_XTRA_NOISE 7	/* Make a noise (optional) */
#define TERM_XTRA_SOUND 8	/* Make a sound (optional) */
#define TERM_XTRA_BORED 9	/* Handle stuff when bored (optional) */
#define TERM_XTRA_REACT 10	/* React to global changes (optional) */
#define TERM_XTRA_ALIVE 11	/* Change the "hard" level (optional) */
#define TERM_XTRA_LEVEL 12	/* Change the "soft" level (optional) */
#define TERM_XTRA_DELAY 13	/* Delay some milliseconds (optional) */

/* The current terminal */
extern term *Term;

errr Term_user(int n);
errr Term_xtra(int n, int v);

void Term_queue_char(int x, int y, byte a, char c, byte ta, char tc);
void Term_queue_chars(int x, int y, int n, byte a, cptr s);

errr Term_fresh(void);
errr Term_set_cursor(bool v);
errr Term_gotoxy(int x, int y);
errr Term_draw(int x, int y, byte a, char c);
errr Term_addch(byte a, char c);
errr Term_addstr(int n, byte a, cptr s);
errr Term_putch(int x, int y, byte a, char c);
errr Term_putstr(int x, int y, int n, byte a, cptr s);
errr Term_erase(int x, int y, int n);
errr Term_clear(void);
errr Term_redraw(void);
errr Term_redraw_section(int x1, int y1, int x2, int y2);

errr Term_get_cursor(bool *v);
errr Term_get_size(int *w, int *h);
errr Term_locate(int *x, int *y);
errr Term_what(int x, int y, byte *a, char *c);

errr Term_flush(void);
errr Term_keypress(int k);
errr Term_key_push(int k);
errr Term_inkey(char *ch, bool wait, bool take);

errr Term_save(void);
errr Term_load(void);

errr Term_exchange(void);

errr Term_resize(int w, int h);

errr Term_activate(term *t);

errr term_nuke(term *t);
errr term_init(term *t, int w, int h, int k);

#endif
