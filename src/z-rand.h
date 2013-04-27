/* File: z-rand.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#ifndef INCLUDED_Z_RAND_H
#define INCLUDED_Z_RAND_H

#include "h-basic.h"



/**** Available constants ****/


/**
 * The "degree" of the "complex" Random Number Generator.
 * This value is hard-coded at 63 for a wide variety of reasons.
 */
#define RAND_DEG 63

/**
 * The maximum safe M for Rand_div
 */
#define RAND_DIV_MAX_M (1UL << 28)


/**** Available macros ****/


/**
 * Generates a random long integer X where O<=X<M.
 * The integer X falls along a uniform distribution.
 * For example, if M is 100, you get "percentile dice"
 */
#define rand_int(M) \
	((s32b)(Rand_div(M)))


/**
 * Generates a random long integer X where 1<=X<=M.
 *
 * \warning the behaviour for M < 1 is undefined.
 */
#define randint(M) \
	(rand_int(M) + 1)


/**
 * Generate a random long integer X where A-D<=X<=A+D
 * The integer X falls along a uniform distribution.
 * \note ::rand_spread(A,D) == ::rand_range(A-D,A+D)
 */
#define rand_spread(A,D) \
	((A) + (rand_int(1+(D)+(D))) - (D))

/** 
  * Return TRUE one time in `x`. 
  */ 
#define one_in_(x)              (!rand_int(x))

/**** Available Variables ****/


extern bool Rand_quick;
extern u32b Rand_value;
extern u16b Rand_place;
extern u32b Rand_state[RAND_DEG];


/**** Available Functions ****/


extern void Rand_state_init(u32b seed);
extern u32b Rand_div(u32b m);
extern s16b Rand_normal(int mean, int stand);
extern u32b Rand_simple(u32b m);

/** 
 * Generates a random signed long integer X where "A <= X <= B" 
 * Note that "rand_range(0, N-1)" == "rand_int(N)" 
 * 
 * The integer X falls along a uniform distribution. 
 */ 
extern int rand_range(int A, int B);

#endif /* INCLUDED_Z_RAND_H */
