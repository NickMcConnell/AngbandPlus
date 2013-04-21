/* File: z-rand.h */

/*
 * Copyright (c) 1997 Ben Harrison and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * Ben Harrison and others released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
 * or under the terms of the traditional Angband license. 
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 * or under the terms of the traditional Angband license. 
 */

#ifndef INCLUDED_Z_RAND_H
#define INCLUDED_Z_RAND_H

#include "h-basic.h"



/**** Available constants ****/


/*
 * The "degree" of the "complex" Random Number Generator.
 * This value is hard-coded at 63 for a wide variety of reasons.
 */
#define RAND_DEG 63




/**** Available macros ****/


/*
 * Generates a random long integer X where O<=X<M.
 * The integer X falls along a uniform distribution.
 * For example, if M is 100, you get "percentile dice"
 */
#define rand_int(M) \
	((rand_unbiased) ? ((s32b)(Rand_num(M))) : ((s32b)(Rand_div(M))))


/*
 * Generates a random long integer X where 1<=X<=M.
 *
 * Note that the behaviour for M < 1 is undefined.
 */
#define randint(M) \
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
 * Evaluate to TRUE "P" percent of the time
 */
#define magik(P) \
	(rand_int(100) < (P))

/*
 * Compat with Hellband
 */
#define randnor		Rand_normal


/**** Available Variables ****/


extern bool rand_unbiased;
extern bool Rand_quick;
extern u32b Rand_value;
extern u16b Rand_place;
extern u32b Rand_state[RAND_DEG];


/**** Available Functions ****/


extern void Rand_state_init(u32b seed);
extern u32b Rand_div(u32b m);
extern u32b Rand_num(u32b m);
extern s16b Rand_normal(int mean, int stand);
extern u32b Rand_simple(u32b m);
extern s16b damroll(int num, int sides);
extern s16b maxroll(int num, int sides);

#endif /* INCLUDED_Z_RAND_H */
