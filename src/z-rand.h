/* File: z-rand.h */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */


#ifndef INCLUDED_Z_RAND_H
#define INCLUDED_Z_RAND_H

#include "h-basic.h"



/**** Available constants ****/


/*
 * Random Number Generator -- Degree of "complex" RNG -- see "misc.c"
 * This value is hard-coded at 63 for a wide variety of reasons.
 */
#define RAND_DEG 63




/**** Available macros ****/


/*
 * Generates a random long integer X where O<=X<M.
 * The integer X falls along a uniform distribution.
 * For example, if M is 100, you get "percentile dice"
 */
#define randint0(M) \
    ((s32b)Rand_div(M))

/*
 * Generates a random long integer X where A<=X<=B
 * The integer X falls along a uniform distribution.
 * Note: rand_range(0,N-1) == randint0(N)
 */
#define rand_range(A,B) \
    ((A) + (randint0(1+(B)-(A))))

/*
 * Generate a random long integer X where A-D<=X<=A+D
 * The integer X falls along a uniform distribution.
 * Note: rand_spread(A,D) == rand_range(A-D,A+D)
 */
#define rand_spread(A,D) \
    ((A) + (randint0(1+(D)+(D))) - (D))


/*
 * Generate a random long integer X where 1<=X<=M
 * Also, "correctly" handle the case of M<=1
 */
#define randint1(M) \
    (randint0(M) + 1)

#define _1d(M) (damroll(1, (M)))
#define _2d(M) (damroll(2, (M)))
#define _3d(M) (damroll(3, (M)))
#define _4d(M) (damroll(4, (M)))
#define _5d(M) (damroll(5, (M)))
#define _6d(M) (damroll(6, (M)))
#define _7d(M) (damroll(7, (M)))
#define _8d(M) (damroll(8, (M)))
#define _9d(M) (damroll(9, (M)))
#define _10d(M) (damroll(10, (M)))
#define _11d(M) (damroll(11, (M)))
#define _12d(M) (damroll(12, (M)))
#define _13d(M) (damroll(13, (M)))
#define _14d(M) (damroll(14, (M)))
#define _15d(M) (damroll(15, (M)))

/*
 * Evaluate to TRUE "P" percent of the time
 */
#define magik(P) \
    ((P) <= 0 || randint0(100) < (P))


#define one_in_(X) \
    ((X) <= 0 || randint0((X)) == 0)

/*
 * Evaluate to TRUE "S" percent of the time
 */
#define saving_throw(S) \
    (randint0(100) < (S))


/**** Available Variables ****/

extern bool Rand_quick;
extern u32b Rand_value;
extern u16b Rand_place;
extern u32b Rand_state[RAND_DEG];


/**** Available Functions ****/

extern void Rand_state_init(u32b seed);
extern s32b Rand_div(u32b m);
extern s16b randnor(int mean, int stand);
extern s16b damroll(int num, int sides);
extern s16b maxroll(int num, int sides);
extern s32b div_round(s32b n, s32b d);
extern u32b Rand_simple(u32b m);


#endif


