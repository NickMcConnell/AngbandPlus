/* File: z-rand.h */

/*
 * Copyright (c) 1997-1999 Greg Wooledge, Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* This library is free software; you can redistribute it and/or   */
/* modify it under the terms of the GNU Library General Public     */
/* License as published by the Free Software Foundation; either    */
/* version 2 of the License, or (at your option) any later         */
/* version.                                                        */
/* This library is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of  */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            */
/* See the GNU Library General Public License for more details.    */
/* You should have received a copy of the GNU Library General      */
/* Public License along with this library; if not, write to the    */
/* Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA   */
/* 02111-1307  USA                                                 */

/* Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.       */
/* When you use this, send an email to: matumoto@math.keio.ac.jp   */
/* with an appropriate reference to your work.                     */

#ifndef INCLUDED_Z_RAND_H
#define INCLUDED_Z_RAND_H

#include "h-basic.h"



/**** Available constants ****/

#define MT_DEF_SEED     4357		/* should never be used */

/* Period parameters */
#define MT_N            624
#define MT_M            397
#define MT_MATRIX_A     0x9908b0df      /* constant vector a */
#define MT_UPPER_MASK   0x80000000      /* most significant w-r bits */
#define MT_LOWER_MASK   0x7fffffff      /* least significant r bits */

/* Tempering parameters */
#define MT_TEMPERING_MASK_B     0x9d2c5680
#define MT_TEMPERING_MASK_C     0xefc60000
#define MT_TEMPERING_SHIFT_U(y) ((y) >> 11)
#define MT_TEMPERING_SHIFT_S(y) ((y) << 7)
#define MT_TEMPERING_SHIFT_T(y) ((y) << 15)
#define MT_TEMPERING_SHIFT_L(y) ((y) >> 18)


/**** Available macros ****/


/*
 * Generates a random long integer X where O<=X<M.
 * The integer X falls along a uniform distribution.
 * For example, if M is 100, you get "percentile dice"
 */
#define rand_int(M) \
	((s32b)(Rand_div(M)))

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



/**** Available Variables ****/


extern bool Rand_quick;
extern u32b Rand_value;
extern u16b Rand_place;


/**** Available Functions ****/


extern void Rand_state_init(u32b seed);
extern u32b Rand_div(u32b m);
extern s16b Rand_normal(int mean, int stand);


#endif
