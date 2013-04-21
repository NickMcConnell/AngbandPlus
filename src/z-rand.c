/* File: z-rand.c */

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

/*
 * This file provides an optimized random number generator.
 *
 * This code provides both a "quick" random number generator (4 bytes of
 * state), and a "decent" random number generator.
 *
 * Note the use of the "simple" RNG, first you activate it via
 * "Rand_quick = TRUE" and "Rand_value = seed" and then it is used
 * automatically used instead of the "complex" RNG, and when you are
 * done, you de-activate it via "Rand_quick = FALSE" or choose a new
 * seed via "Rand_value = seed".
 *
 * This (optimized) random number generator is based loosely on the old
 * "random.c" file from Berkeley but with some major optimizations and
 * algorithm changes.  The "complex" random number generator has been
 * replaced by the "Mersenne Twister" algorithm (see below).
 *
 * Some code by Ben Harrison (benh@phial.com).
 *
 * Some code by Randy (randy@stat.tamu.edu).
 *
 * Some code by Greg Wooledge (wooledge@kellnet.com).
 */



#include "z-rand.h"

/* Internally used functions (the Mersenne Twister algorithm). */
static void sgenrand(u32b seed);
static u32b genrand(void);


/*
 * Random Number Generator -- Linear Congruent RNG
 */
#define LCRNG(X)        ((X) * 1103515245 + 12345)



/*
 * Use the "simple" LCRNG
 */
bool Rand_quick = TRUE;


/*
 * Current "value" of the "simple" RNG
 */
u32b Rand_value;


/*
 * Initialize the "complex" RNG using a new seed
 */
void Rand_state_init(u32b seed)
{
	/* Invoke the Mersenne Twister initialization. */
	if (seed)
	{
		sgenrand(seed);
	}
	else
	{
		sgenrand(MT_DEF_SEED);
	}
}


/*
 * Extract a "random" number from 0 to m-1.
 *
 * The "simple" RNG selects "random" 28-bit numbers, and then uses
 * division to drop those numbers into "m" different partitions,
 * plus a small non-partition to reduce bias, taking as the final
 * value the first "good" partition that a number falls into.
 *
 * This method has no bias, and is much less affected by patterns
 * in the "low" bits of the underlying RNG's.
 *
 * The "complex" version uses the Mersenne Twister.  From the MT FAQ:
 *
 *   Want a uniform discrete distribution among the integers [0..N-1], e.g.
 *   a dice in the case of N=6. 
 *
 *   If the application is not sensitive to the rounding off error, then please
 *   multiply N and take the integer part (sufficient for most applications).
 *   In the very rare case that the rounding off error of real numbers does
 *   matter, then generate random integers, take the minimum integer with
 *   N<=2^n, take the least significant n bits and discard the numbers greater
 *   than or equal to N.
 *
 * Since we're not using real (float/double) numbers here, we'll take the
 * second approach.  Thus the "_div" name is no longer accurate for the
 * "complex" RNG. -GJW
 */
u32b Rand_div(u32b m)
{
	u32b r;

	/* Hack -- simple case */
	if (m <= 1) return (0);

	/* Use a simple RNG */
	if (Rand_quick)
	{
		/* Partition size */
		u32b n = (0x10000000 / m);

		/* Wait for it */
		while (1)
		{
			/* Cycle the generator */
			r = (Rand_value = LCRNG(Rand_value));

			/* Mutate a 28-bit "random" number */
			r = (r >> 4) / n;

			/* Done */
			if (r < m) break;
		}
	}

	/* Use a complex RNG */
	else
	{
		int i;
		u32b m_tmp;

		/*
		 * Find the minimum power of 2 which is >= m.  We do this
		 * by right-shifting m, 1 step at a time until it gets down
		 * to 1; then left-shifting an equal number of times.  If
		 * we get our original m back, then it's a power of 2.
		 * Otherwise left-shift once more. -GJW
		 */
		m_tmp = m;
		i = 1;

		/* Right-shift several times. */
		while (m_tmp > 1)
		{
			m_tmp >>= 1;
			i++;
		}

		/* Left-shift an equal number (i) of times. */
		m_tmp <<= i;

		/*
		 * If m_tmp = m, then m is a power of 2, so stop.  Otherwise,
		 * shift one more time.
		 */
		if (m_tmp != m) m_tmp <<= 1;

		/* Our bitmask is 1 less than m_tmp. */
		m_tmp--;

		/* Wait for it */
		while (1)
		{
			/* Get a random 32-bit value from MT, masked. */
			r = genrand() & m_tmp;

			/* Done */
			if (r < m) break;
		}
	}

	/* Use the value */
	return (r);
}




/*
 * The number of entries in the "Rand_normal_table"
 */
#define RANDNOR_NUM	256

/*
 * The standard deviation of the "Rand_normal_table"
 */
#define RANDNOR_STD	64

/*
 * The normal distribution table for the "Rand_normal()" function (below)
 */
static s16b Rand_normal_table[RANDNOR_NUM] =
{
	206,     613,    1022,    1430,		1838,	 2245,	  2652,	   3058,
	3463,    3867,    4271,    4673,	5075,	 5475,	  5874,	   6271,
	6667,    7061,    7454,    7845,	8234,	 8621,	  9006,	   9389,
	9770,   10148,   10524,   10898,   11269,	11638,	 12004,	  12367,
	12727,   13085,   13440,   13792,   14140,	14486,	 14828,	  15168,
	15504,   15836,   16166,   16492,   16814,	17133,	 17449,	  17761,
	18069,   18374,   18675,   18972,   19266,	19556,	 19842,	  20124,
	20403,   20678,   20949,   21216,   21479,	21738,	 21994,	  22245,

	22493,   22737,   22977,   23213,   23446,	23674,	 23899,	  24120,
	24336,   24550,   24759,   24965,   25166,	25365,	 25559,	  25750,
	25937,   26120,   26300,   26476,   26649,	26818,	 26983,	  27146,
	27304,   27460,   27612,   27760,   27906,	28048,	 28187,	  28323,
	28455,   28585,   28711,   28835,   28955,	29073,	 29188,	  29299,
	29409,   29515,   29619,   29720,   29818,	29914,	 30007,	  30098,
	30186,   30272,   30356,   30437,   30516,	30593,	 30668,	  30740,
	30810,   30879,   30945,   31010,   31072,	31133,	 31192,	  31249,

	31304,   31358,   31410,   31460,   31509,	31556,	 31601,	  31646,
	31688,   31730,   31770,   31808,   31846,	31882,	 31917,	  31950,
	31983,   32014,   32044,   32074,   32102,	32129,	 32155,	  32180,
	32205,   32228,   32251,   32273,   32294,	32314,	 32333,	  32352,
	32370,   32387,   32404,   32420,   32435,	32450,	 32464,	  32477,
	32490,   32503,   32515,   32526,   32537,	32548,	 32558,	  32568,
	32577,   32586,   32595,   32603,   32611,	32618,	 32625,	  32632,
	32639,   32645,   32651,   32657,   32662,	32667,	 32672,	  32677,

	32682,   32686,   32690,   32694,   32698,	32702,	 32705,	  32708,
	32711,   32714,   32717,   32720,   32722,	32725,	 32727,	  32729,
	32731,   32733,   32735,   32737,   32739,	32740,	 32742,	  32743,
	32745,   32746,   32747,   32748,   32749,	32750,	 32751,	  32752,
	32753,   32754,   32755,   32756,   32757,	32757,	 32758,	  32758,
	32759,   32760,   32760,   32761,   32761,	32761,	 32762,	  32762,
	32763,   32763,   32763,   32764,   32764,	32764,	 32764,	  32765,
	32765,   32765,   32765,   32766,   32766,	32766,	 32766,	  32767,
};



/*
 * Generate a random integer number of NORMAL distribution
 *
 * The table above is used to generate a psuedo-normal distribution,
 * in a manner which is much faster than calling a transcendental
 * function to calculate a true normal distribution.
 *
 * Basically, entry 64*N in the table above represents the number of
 * times out of 32767 that a random variable with normal distribution
 * will fall within N standard deviations of the mean.  That is, about
 * 68 percent of the time for N=1 and 95 percent of the time for N=2.
 *
 * The table above contains a "faked" final entry which allows us to
 * pretend that all values in a normal distribution are strictly less
 * than four standard deviations away from the mean.  This results in
 * "conservative" distribution of approximately 1/32768 values.
 *
 * Note that the binary search takes up to 16 quick iterations.
 */
s16b Rand_normal(int mean, int stand)
{
	s16b tmp;
	s16b offset;

	s16b low = 0;
	s16b high = RANDNOR_NUM;

	/* Paranoia */
	if (stand < 1) return (mean);

	/* Roll for probability */
	tmp = rand_int(32768);

	/* Binary Search */
	while (low < high)
	{
		int mid = (low + high) >> 1;

		/* Move right if forced */
		if (Rand_normal_table[mid] < tmp)
		{
			low = mid + 1;
		}

		/* Move left otherwise */
		else
		{
			high = mid;
		}
	}

	/* Convert the index into an offset */
	offset = (long)stand * (long)low / RANDNOR_STD;

	/* One half should be negative */
	if (rand_int(100) < 50) return (mean - offset);

	/* One half should be positive */
	return (mean + offset);
}


/*
 * The following is the Mersenne Twister algorithm, from
 *
 * http://www.math.keio.ac.jp/~matumoto/emt.html
 *
 * I have reformatted the code slightly to enhance readability. -GJW
 */

/* A C-program for MT19937: Integer     version                   */
/*  genrand() generates one pseudorandom unsigned integer (32bit) */
/* which is uniformly distributed among 0 to 2^32-1  for each     */
/* call. sgenrand(seed) set initial values to the working area    */
/* of 624 words. Before genrand(), sgenrand(seed) must be         */
/* called once. (seed is any 32-bit integer except for 0).        */
/*   Coded by Takuji Nishimura, considering the suggestions by    */
/* Topher Cooper and Marc Rieffel in July-Aug. 1997.              */

static u32b mt[MT_N];	/* the array for the state vector  */
static int mti = MT_N + 1;	/* mti==N+1 means mt[N] is not initialized */

/* initializing the array with a NONZERO seed */
void sgenrand(u32b seed)
{
    /* setting initial seeds to mt[N] using         */
    /* the generator Line 25 of Table 1 in          */
    /* [KNUTH 1981, The Art of Computer Programming */
    /*    Vol. 2 (2nd Ed.), pp102]                  */
    mt[0] = seed & 0xffffffff;
    for (mti = 1; mti < MT_N; mti++)
        mt[mti] = (69069 * mt[mti - 1]) & 0xffffffff;
}

u32b genrand(void)
{
    u32b y;
    static u32b mag01[2] = {0x0, MT_MATRIX_A};
    /* mag01[x] = x * MT_MATRIX_A  for x=0,1 */

    if (mti >= MT_N) { /* generate N words at one time */
        int kk;

        if (mti == MT_N + 1)		/* if sgenrand() has not been called, */
            sgenrand(MT_DEF_SEED);	/* a default initial seed is used */

        for (kk = 0; kk < MT_N - MT_M; kk++) {
            y = (mt[kk] & MT_UPPER_MASK) | (mt[kk + 1] & MT_LOWER_MASK);
            mt[kk] = mt[kk + MT_M] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        for ( ; kk < MT_N - 1; kk++) {
            y = (mt[kk] & MT_UPPER_MASK) | (mt[kk + 1] & MT_LOWER_MASK);
            mt[kk] = mt[kk + (MT_M - MT_N)] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        y = (mt[MT_N - 1] & MT_UPPER_MASK) | (mt[0] & MT_LOWER_MASK);
        mt[MT_N - 1] = mt[MT_M - 1] ^ (y >> 1) ^ mag01[y & 0x1];

        mti = 0;
    }
  
    y = mt[mti++];
    y ^= MT_TEMPERING_SHIFT_U(y);
    y ^= MT_TEMPERING_SHIFT_S(y) & MT_TEMPERING_MASK_B;
    y ^= MT_TEMPERING_SHIFT_T(y) & MT_TEMPERING_MASK_C;
    y ^= MT_TEMPERING_SHIFT_L(y);

    return y; 
}
