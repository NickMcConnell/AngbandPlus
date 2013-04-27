/* File: z-rand.c */

/*
 * Copyright (c) 2000 Eric Bock
 *
 * This software may be distributed and modified freely provided this notice
 * appears in its entirety in all copies and deriviative works.
 */

#include "z-rand.h"


/* Current value of the u32b RNG */
u32b _rand_u32b_val;

/* Current value of the bool RNG */
u32b _rand_bool_val;

/* Current position of the bool RNG */
int _rand_bool_pos = -1;


/*
 * Generate a random 32-bit integer by splitting [0, 2^32-1] into M partitions,
 * with an extra partition of unknown length (which is ignored).  This has no
 * bias.
 */
u32b _rand_uniform(u32b n)
{
	/* Partition size */
	 u32b s;

	 if (n < 2) return (0);

	 s = 0xFFFFFFFFL / n;

	 while (1)
	 {
		  /* Drop the current value into a partition */
		  u32b r = random / s;

		  /* Done if 0 <= r < m */
		  if (r < n) return (r);
	 }
}


/*
 * Generate a random boolean value by shifting bits off of a random u32b.
 */
bool _rand_bool(void)
{
	 /* Advance */
	 _rand_bool_pos = (_rand_bool_pos + 1) % 32;

	 /* No bits left, refresh */
	 if (!_rand_bool_pos) _rand_bool_val = random;

	 /* Convert this bit to a bool */
	 return (!((_rand_bool_val >> _rand_bool_pos) & 1));
}


/*
 * Generate a random integer by approximating a normal distribution.
 *
 * Note the the sum of 32 independent random variables is very nearly
 * normally distributed, especially for large numbers of possibilities.
 */
s32b _rand_normal(f32b m, f32b s)
{
	 int i;
	 f32b n = 0;

	 for (i = 0; i < 32; i++) n += random / 10.0;

	 /* Well, it had to happen sometime... */
	 return ((n / 13743895344.0 - 0.5) * 16 * s) + m;
}


/*
 * Generate an XdY dice roll
 */
u32b damroll(u32b x, u32b y)
{
	u32b r = x;

	while(x--) r += rand_int(y);

	return r;
}


/*
 * Contest of skills
 *
 * Returns amount a succeeds by
 */
s32b contest(s32b a, s32b b)
{
	s32b powa = conroll;
	s32b powb = conroll;

	/* Critical success/failure */
	if (ABS(powa) > CRITICAL) powa *= 2;
	if (ABS(powb) > CRITICAL) powb *= 2;

	/* Return comparative strength */
	return ((powa + a) - (powb + b));
}


