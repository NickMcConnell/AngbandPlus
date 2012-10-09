/* File: z-rand.c */

/* Purpose: a simple random number generator -BEN- */

#include "z-rand.h"




/*
 * Angband 2.7.9 introduced a new (optimized) random number generator,
 * based loosely on the old "random.c" from Berkeley but with some major
 * optimizations and algorithm changes.  See below for more details.
 *
 * Code by myself (benh@voicenet.com) and Randy (randy@stat.tamu.edu).
 *
 * This code provides (1) a "decent" RNG, based on the "BSD-degree-63-RNG"
 * used in Angband 2.7.8, but rather optimized, and (2) a "simple" RNG,
 * based on the simple "LCRNG" currently used in Angband, but "corrected"
 * to give slightly better values.  Both of these are available in two
 * flavors, first, the simple "mod" flavor, which is fast, but slightly
 * biased at high values, and second, the simple "div" flavor, which is
 * less fast (and potentially non-terminating) but which is not biased
 * and is much less subject to low-bit-non-randomness problems.
 *
 * You can select your favorite flavor by proper definition of the
 * "rand_int()" macro in the "defines.h" file.
 *
 * Note that, in Angband 2.8.0, the "state" table will be saved in the
 * savefile, so a special "initialization" will be necessary.
 */

/*
 * Random Number Generator -- Linear Congruent RNG
 */
#define LCRNG(X)     ((X) * 1103515245 + 12345)



/*
 * Use the "simple" LCRNG
 */
bool Rand_quick = TRUE;


/*
 * Current "value" of the "simple" RNG
 */
u32b Rand_value;


/*
 * Current "index" for the "complex" RNG
 */
u16b Rand_place;

/*
 * Current "state" table for the "complex" RNG
 */
u32b Rand_state[RAND_DEG];



/*
 * Initialize the "complex" RNG using a new seed
 */
void Rand_seed_init_normal(u32b seed)
{
   int i, j;

   /* Seed the table */
   Rand_state[0] = seed;

   /* Propagate the seed */
   for (i = 1; i < RAND_DEG; i++) Rand_state[i] = LCRNG(Rand_state[i-1]);

   /* Cycle the table ten times per degree */
   for (i = 0; i < RAND_DEG * 10; i++)
   {
     /* Acquire the next index */
     j = Rand_place + 1;
     if (j == RAND_DEG) j = 0;

     /* Update the table, extract an entry */
     Rand_state[j] += Rand_state[Rand_place];

     /* Advance the index */
     Rand_place = j;
   }
}


/*
 * Extract a "random" number from 0 to m-1, via "modulus"
 *
 * Note that "m" should probably be less than 500000, or the
 * results may be rather biased towards low values.
 */
s32b Rand_mod(s32b m)
{
   int j;
   u32b r;

   /* Hack -- simple case */
   if (m <= 1) return (0);

   /* Acquire the next index */
   j = Rand_place + 1;
   if (j == RAND_DEG) j = 0;

   /* Update the table, extract an entry */
   r = (Rand_state[j] += Rand_state[Rand_place]);

   /* Advance the index */
   Rand_place = j;

   /* Extract a "random" number */
   r = ((r >> 4) % m);

   /* Use the value */
   return (r);
}

/* A C-program for MT19937: Integer    version             */
/* genrand() generates one pseudorandom unsigned integer (32bit) */
/* which is uniformly distributed among 0 to 2^32-1  for each    */
/* call. sgenrand(seed) set initial values to the working area   */
/* of 624 words. Before genrand(), sgenrand(seed) must be        */
/* called once. (seed is any 32-bit integer except for 0).       */
/*  Coded by Takuji Nishimura, considering the suggestions by    */
/* Topher Cooper and Marc Rieffel in July-Aug. 1997.          */

/* This library is free software; you can redistribute it and/or   */
/* modify it under the terms of the GNU Library General Public    */
/* License as published by the Free Software Foundation; either    */
/* version 2 of the License, or (at your option) any later        */
/* version.                                           */
/* This library is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of  */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        */
/* See the GNU Library General Public License for more details.    */
/* You should have received a copy of the GNU Library General     */
/* Public License along with this library; if not, write to the    */
/* Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA   */
/* 02111-1307  USA                                       */

/* Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.      */
/* When you use this, send an email to: matumoto@math.keio.ac.jp   */
/* with an appropriate reference to your work.                 */

/* Period parameters */
#define RAND_MT_N 624
#define RAND_MT_M 397
#define RAND_MT_MATRIX_A 0x9908b0df   /* constant vector a */
#define RAND_MT_UPPER_MASK 0x80000000 /* most significant w-r bits */
#define RAND_MT_LOWER_MASK 0x7fffffff /* least significant r bits */

/* Tempering parameters */
#define RAND_MT_TEMPERING_MASK_B 0x9d2c5680
#define RAND_MT_TEMPERING_MASK_C 0xefc60000
#define RAND_MT_TEMPERING_SHIFT_U(y)  (y >> 11)
#define RAND_MT_TEMPERING_SHIFT_S(y)  (y << 7)
#define RAND_MT_TEMPERING_SHIFT_T(y)  (y << 15)
#define RAND_MT_TEMPERING_SHIFT_L(y)  (y >> 18)

static u32b mt[RAND_MT_N]; /* the array for the state vector  */
static s16b mti=RAND_MT_N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializing the array with a NONZERO seed */
void Rand_seed_init_mt(u32b seed)
{
   /* jk - even seed should be avoided */
   if (!(seed % 2)) seed++;

   /* setting initial seeds to mt[N] using         */
   /* the generator Line 25 of Table 1 in       */
   /* [KNUTH 1981, The Art of Computer Programming */
   /*  Vol. 2 (2nd Ed.), pp102]              */
   mt[0]= seed & 0xffffffff;
   for (mti=1; mti<RAND_MT_N; mti++)
   {
      mt[mti] = (69069 * mt[mti-1]) & 0xffffffff;
   }
fprintf(stderr,"z-rand.c: Rand_seed_init_mt: initialized, seed %lu\n", seed);
}

s32b Rand_div_mt(s32b m)
{
   u32b y;
   static u32b mag01[2]={0x0, RAND_MT_MATRIX_A};

   /* mag01[x] = x * RAND_MT_MATRIX_A  for x=0,1 */

   if (mti >= RAND_MT_N) /* generate RAND_MT_N words at one time */
   {
      s16b kk;

      if (mti == RAND_MT_N+1)  /* if sgenrand() has not been called, */
         Rand_seed_init_mt(4357); /* a default initial seed is used   */

      for (kk=0;kk<RAND_MT_N-RAND_MT_M;kk++) {
         y = (mt[kk]&RAND_MT_UPPER_MASK)|(mt[kk+1]&RAND_MT_LOWER_MASK);
         mt[kk] = mt[kk+RAND_MT_M] ^ (y >> 1) ^ mag01[y & 0x1];
      }
      for (;kk<RAND_MT_N-1;kk++) {
         y = (mt[kk]&RAND_MT_UPPER_MASK)|(mt[kk+1]&RAND_MT_LOWER_MASK);
         mt[kk] = mt[kk+(RAND_MT_M-RAND_MT_N)] ^ (y >> 1) ^ mag01[y & 0x1];
      }
      y = (mt[RAND_MT_N-1]&RAND_MT_UPPER_MASK)|(mt[0]&RAND_MT_LOWER_MASK);
      mt[RAND_MT_N-1] = mt[RAND_MT_M-1] ^ (y >> 1) ^ mag01[y & 0x1];

      mti = 0;
   }

   y = mt[mti++];
   y ^= RAND_MT_TEMPERING_SHIFT_U(y);
   y ^= RAND_MT_TEMPERING_SHIFT_S(y) & RAND_MT_TEMPERING_MASK_B;
   y ^= RAND_MT_TEMPERING_SHIFT_T(y) & RAND_MT_TEMPERING_MASK_C;
   y ^= RAND_MT_TEMPERING_SHIFT_L(y);

   /* this is added by jk - and will probably break this algorithm in hideous ways */

   return (s32b)(y & (u32b)m);
}

/*
 * Extract a "random" number from 0 to m-1, via "division"
 *
 * This method selects "random" 28-bit numbers, and then uses
 * division to drop those numbers into "m" different partitions,
 * plus a small non-partition to reduce bias, taking as the final
 * value the first "good" partition that a number falls into.
 *
 * This method has no bias, and is much less affected by patterns
 * in the "low" bits of the underlying RNG's.
 */
s32b Rand_div_normal(s32b m)
{
   u32b r, n;

   /* Hack -- simple case */
   if (m <= 1) return (0);

   /* Partition size */
   n = (0x10000000 / m);

   /* Use a complex RNG */
   /* Wait for it */
   while (1)
   {
     int j;

     /* Acquire the next index */
     j = Rand_place + 1;
     if (j == RAND_DEG) j = 0;

     /* Update the table, extract an entry */
     r = (Rand_state[j] += Rand_state[Rand_place]);

     /* Hack -- extract a 28-bit "random" number */
     r = (r >> 4) / n;

     /* Advance the index */
     Rand_place = j;

     /* Done */
     if (r < m) break;
   }

   /* Use the value */
   return (r);
}

/*
 * The number of entries in the "randnor_table"
 */
#define RANDNOR_NUM  256

/*
 * The standard deviation of the "randnor_table"
 */
#define RANDNOR_STD  64

/*
 * The normal distribution table for the "randnor()" function (below)
 */
static int randnor_table[RANDNOR_NUM] = {

    206,   613,     1022,    1430,  1838,  2245,     2652,    3058,
   3463,  3867,     4271,    4673,  5075,  5475,     5874,    6271,
   6667,  7061,     7454,    7845,  8234,  8621,     9006,    9389,
   9770, 10148,    10524,   10898,   11269,  11638,    12004,   12367,
   12727,   13085,    13440,   13792,   14140,  14486,    14828,   15168,
   15504,   15836,    16166,   16492,   16814,  17133,    17449,   17761,
   18069,   18374,    18675,   18972,   19266,  19556,    19842,   20124,
   20403,   20678,    20949,   21216,   21479,  21738,    21994,   22245,

   22493,   22737,    22977,   23213,   23446,  23674,    23899,   24120,
   24336,   24550,    24759,   24965,   25166,  25365,    25559,   25750,
   25937,   26120,    26300,   26476,   26649,  26818,    26983,   27146,
   27304,   27460,    27612,   27760,   27906,  28048,    28187,   28323,
   28455,   28585,    28711,   28835,   28955,  29073,    29188,   29299,
   29409,   29515,    29619,   29720,   29818,  29914,    30007,   30098,
   30186,   30272,    30356,   30437,   30516,  30593,    30668,   30740,
   30810,   30879,    30945,   31010,   31072,  31133,    31192,   31249,

   31304,   31358,    31410,   31460,   31509,  31556,    31601,   31646,
   31688,   31730,    31770,   31808,   31846,  31882,    31917,   31950,
   31983,   32014,    32044,   32074,   32102,  32129,    32155,   32180,
   32205,   32228,    32251,   32273,   32294,  32314,    32333,   32352,
   32370,   32387,    32404,   32420,   32435,  32450,    32464,   32477,
   32490,   32503,    32515,   32526,   32537,  32548,    32558,   32568,
   32577,   32586,    32595,   32603,   32611,  32618,    32625,   32632,
   32639,   32645,    32651,   32657,   32662,  32667,    32672,   32677,

   32682,   32686,    32690,   32694,   32698,  32702,    32705,   32708,
   32711,   32714,    32717,   32720,   32722,  32725,    32727,   32729,
   32731,   32733,    32735,   32737,   32739,  32740,    32742,   32743,
   32745,   32746,    32747,   32748,   32749,  32750,    32751,   32752,
   32753,   32754,    32755,   32756,   32757,  32757,    32758,   32758,
   32759,   32760,    32760,   32761,   32761,  32761,    32762,   32762,
   32763,   32763,    32763,   32764,   32764,  32764,    32764,   32765,
   32765,   32765,    32765,   32766,   32766,  32766,    32766,   32767,
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
 * will fall within N standard deviations of the mean.   That is, about
 * 68 percent of the time for N=1 and 95 percent of the time for N=2.
 *
 * The table above contains a "faked" final entry which allows us to
 * pretend that all values in a normal distribution are strictly less
 * than four standard deviations away from the mean.  This results in
 * "conservative" distribution of approximately 1/32768 values.
 *
 * Note that the binary search takes up to 16 quick iterations.
 */
s16b randnor(int mean, int stand)
{
   int tmp;
   int offset;

   int low = 0;
   int high = RANDNOR_NUM;

   /* Paranoia */
   if (stand < 1) return (mean);

   /* Roll for probability */
   tmp = rand_int(32768);

   /* Binary Search */
   while (low < high)
   {
      int mid = (low + high) >> 1;

      /* Move right if forced */
      if (randnor_table[mid] < tmp)
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
 * Generates damage for "2d6" style dice rolls
 */
s16b damroll(int num, int sides)
{
   int i, sum = 0;
   for (i = 0; i < num; i++) sum += randint(sides);
   return (sum);
}


/*
 * Same as above, but always maximal
 */
s16b maxroll(int num, int sides)
{
   return (num * sides);
}



