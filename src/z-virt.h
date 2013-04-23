#define Z_VIRT_H
/* File: z-virt.h */

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
 * These routines will not work as well if the program calls malloc/free.
 *
 * The string_make() routine allocates dynamic strings.
 * A dynamic string is a string allocated at run-time, but not really
 * intended to be modified once allocated.
 *
 * Note the macros below which simplify the details of allocation,
 * clearing, casting, and size extraction.
 *
 * The macros MAKE/C_MAKE and KILL/C_KILL have a "procedural" metaphor.
 *
 * NOTE: For some reason, many allocation macros disallow "stars"
 * in type names, but you can use typedefs to circumvent this.
 * For example, instead of "type **p; MAKE(p,type*);" you can use
 * "typedef type *type_ptr; type_ptr *p; MAKE(p,type_ptr)".
 *
 * Note that it is assumed that "memset()" will function correctly,
 * in particular, that it returns its first argument.
 *
 * The 'GROW' macro is very tentative.
 */


/**** Memory Macros ****/


/* Size of 'N' things of type 'T' */
#define C_SIZE(N,T) \
		((huge)((N)*(sizeof(T))))

/* Size of one thing of type 'T' */
#define SIZE(T) \
		((huge)(sizeof(T)))


/* Set every byte in an array of type T[N], at location P, to V, and return P */
#define C_BSET(P,V,N,T) \
	(T*)(memset((char*)(P),(V),C_SIZE(N,T)))

/* Set every byte in a thing of type T, at location P, to V, and return P */
#define BSET(P,V,T) \
	(T*)(memset((char*)(P),(V),SIZE(T)))

/* Wipe an array of N things of type T at location P, return T */
#define C_WIPE(P,N,T) \
		memset((char*)(P),0,C_SIZE(N,T))

/* Wipe a single thing of type T at location P, return T */
#define WIPE(P,T) \
		memset((char*)(P),0,SIZE(T))


/* When P1 and P2 both point to N T's, Load P1 from P2 explicitly */
#define C_COPY(P1,P2,N,T) \
		memcpy((char*)(P1),(char*)(P2),C_SIZE(N,T))

/* When P1 and P2 both point to T's, Load P1 from P2 explicitly */
#define COPY(P1,P2,T) \
		memcpy((char*)(P1),(char*)(P2),SIZE(T))



/* Allocate and return an array of N things of type T */
#define C_NEW(N,T) \
		((T*)(ralloc(C_SIZE(N,T))))

/* Allocate and return one thing of type T */
#define NEW(T) \
		((T*)(ralloc(SIZE(T))))


/* Allocate, wipe, and return an array of N things of type T */
#define C_ZNEW(N,T) \
		((T*)(C_WIPE(ralloc(C_SIZE(N,T)),N,T)))

/* Allocate, wipe, and return one thing of type T */
#define ZNEW(T) \
		((T*)(WIPE(ralloc(SIZE(T)),T)))


/* Allocate a wiped array of N things of type T, let P point at them */
#define C_MAKE(P,N,T) \
		((P)=C_ZNEW(N,T))

/* Allocate a wiped thing of type T, let P point at it */
#define MAKE(P,T) \
	((P)=ZNEW(T))

/* Free something at P, return NULL */
#define FREE(P) \
	(rnfree((vptr)P))



/* Free a thing at location P and set P to NULL */
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
	T *P = C_NEW(N, T)

/* Free a local array at P. */
#define TFREE(P) \
	FREE(P)

#endif /* VARIABLE_ARRAYS */

/* Mega-Hack -- Cleanly "grow" 'P' from N1 T's to N2 T's */
#define GROW(P,N1,N2,T) \
		(C_MAKE(vptr_tmp,N2,T), C_COPY(vptr_tmp,P,MIN(N1,N2),T), \
		FREE(P), (P)=vptr_tmp)



#endif

