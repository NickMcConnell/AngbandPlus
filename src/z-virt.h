#define Z_VIRT_H
/* File: z-virt.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#ifndef INCLUDED_Z_VIRT_H
#define INCLUDED_Z_VIRT_H

#include "h-basic.h"

/*
 * Memory management routines.
 *
 * Set ralloc_aux to modify the memory allocation routine.
 * Set _aux to modify the memory de-allocation routine.
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
 * The macros MAKE/C_MAKE and KILL/C_KILL have a "procedural" metaphor,
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



/**** Available macros ****/


/* Size of 'N' things of type 'T' */
#define C_SIZE(N,T) \
		((huge)((N)*(sizeof(T))))

/* Size of one thing of type 'T' */
#define SIZE(T) \
		((huge)(sizeof(T)))


/* Compare two arrays of type T[N], at locations P1 and P2 */
#define C_DIFF(P1,P2,N,T) \
	(memcmp((char*)(P1),(char*)(P2),C_SIZE(N,T)))

/* Compare two things of type T, at locations P1 and P2 */
#define DIFF(P1,P2,T) \
	(memcmp((char*)(P1),(char*)(P2),SIZE(T)))


/* Set every byte in an array of type T[N], at location P, to V, and return P */
#define C_BSET(P,V,N,T) \
	(memset((char*)(P),(V),C_SIZE(N,T)))

/* Set every byte in a thing of type T, at location P, to V, and return P */
#define BSET(P,V,T) \
	(memset((char*)(P),(V),SIZE(T)))


/* Wipe an array of type T[N], at location P, and return P */
#define C_WIPE(P,N,T) \
	(memset((char*)(P),0,C_SIZE(N,T)))

/* Wipe a thing of type T, at location P, and return P */
#define WIPE(P,T) \
	(memset((char*)(P),0,SIZE(T)))


/* Load an array of type T[N], at location P1, from another, at location P2 */
#define C_COPY(P1,P2,N,T) \
	(memcpy((char*)(P1),(char*)(P2),C_SIZE(N,T)))

/* Load a thing of type T, at location P1, from another, at location P2 */
#define COPY(P1,P2,T) \
	(memcpy((char*)(P1),(char*)(P2),SIZE(T)))


/* Free an array of N things of type T at P, return NULL */
#define C_FREE(P,N,T) \
	(rnfree(P,C_SIZE(N,T)))

/* Free one thing of type T at P, return NULL */
#define FREE(P) \
	(rnfree((vptr)P))


/* Allocate, and return, an array of type T[N] */
#define C_RNEW(N,T) \
	((T*)ralloc(C_SIZE(N,T)))

/* Allocate, and return, a thing of type T */
#define RNEW(T) \
	((T*)ralloc(SIZE(T)))



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


/* Free an array of type T[N], at location P, and set P to NULL */
#define C_KILL(P,N,T) \
	((P)=C_FREE(P,N,T))

/* Free a thing of type T, at location P, and set P to NULL */
#define KILL(P) \
	((P)=FREE(P))


/*
 * C_TNEW() and TKILL() declare an array which is only used in the local
 * scope. If the compiler is known to support it, this is represented as an
 * array with a variable size for efficiency.
 *
 * (insert constraints on P here...)
 */

#ifdef VARIABLE_ARRAYS

/* Allocate and declare a local array of N things of type T at P. */
#define C_TNEW(P,N,T) \
	T P[N]

/* Free a local array at P (automatic). */
#define TFREE(P) (void)(P)

#else /* VARIABLE_ARRAYS */

/* Allocate and declare a local array of N things of type T at P. */
#define C_TNEW(P, N, T) \
	T *P = C_RNEW(N, T)

/* Free a local array at P. */
#define TFREE(P) \
	FREE(P)

#endif /* VARIABLE_ARRAYS */


/**** Available variables ****/

/* Replacement hook for "rnfree()" */
extern vptr (*rnfree_aux)(vptr);

/* Replacement hook for "rpanic()" */
extern vptr (*rpanic_aux)(huge);

/* Replacement hook for "ralloc()" */
extern vptr (*ralloc_aux)(huge);


/**** Available functions ****/

/* De-allocate a given amount of memory */
extern vptr rnfree(vptr p /*, huge len */);

/* Panic, attempt to Allocate 'len' bytes */
extern vptr rpanic(huge len);

/* Allocate (and return) 'len', or dump core */
extern vptr ralloc(huge len);

/* Create a "dynamic string" */
extern char *string_make(cptr str);

/* Free a string allocated with "string_make()" */
extern errr string_free(cptr str);

#endif

