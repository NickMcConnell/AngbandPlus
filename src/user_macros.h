#ifndef USER_MACROS_H
#define USER_MACROS_H


/**** Available macros ****/

/* Wipe an array of type T[N], at location P, and return P */
// Should not be used on any class or structure with methods.
#define C_WIPE(P, N, T) \
    (memset((P), 0, (N) * sizeof(T)))

/* Wipe a thing of type T, at location P, and return P */
// Should not be used on any class or structure with methods.
#define WIPE(P, T) \
    (memset((P), 0, sizeof(T)))

/* Allocate, wipe, and return an array of type T[N] */
#define C_ZNEW(N, T) (new T[N]())

/* Allocate, wipe, and return a thing of type T */
#define ZNEW(T) (new T())

/* Free one thing at P, return NULL */
#define FREE(P) (delete(P), P = NULL)

/* Free one thing at P, return NULL */
#define FREE_ARRAY(P) (delete [] (P), P = NULL)

#define MIN(a,b)	(((a) > (b)) ? (b)  : (a))
#define MAX(a,b)	(((a) < (b)) ? (b)  : (a))
#define ABS(a)		(((a) < 0)   ? (-(a)) : (a))

/* Load a thing of type T, at location P1, from another, at location P2 */
#define COPY(P1, P2, T) \
    (memcpy((P1), (P2), sizeof(T)))

/*
 * Given an array, determine how many elements are in the array.
 */
#define N_ELEMENTS(a) (sizeof(a) / sizeof((a)[0]))

/*Square a number*/
#define GET_SQUARE(X) 	((X) * (X))


#endif // USER_MACROS_H
