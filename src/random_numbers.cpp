/* File: z-rand.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


/*
 * This file provides an optimized random number generator.
 *
 *
 * This code provides both a "quick" random number generator (4 bytes of
 * state), and a "decent" random number generator (256 bytes of state),
 * both available in two flavors, first, the simple "mod" flavor, which
 * is fast, but slightly biased at high values, and second, the simple
 * "div" flavor, which is less fast (and potentially non-terminating)
 * but which is not biased and is much less subject to non-randomness
 * problems in the low bits.  Note the "rand_int()" macro in "z-rand.h",
 * which must specify a "default" flavor.
 *
 * Note the use of the "simple" RNG, first you activate it via
 * "Rand_quick = TRUE" and "Rand_value = seed" and then it is used
 * automatically used instead of the "complex" RNG, and when you are
 * done, you de-activate it via "Rand_quick = FALSE" or choose a new
 * seed via "Rand_value = seed".
 *
 *
 * This (optimized) random number generator is based loosely on the old
 * "random.c" file from Berkeley but with some major optimizations and
 * algorithm changes.  See below for more details.
 *
 * Some code by Ben Harrison (benh@phial.com).
 *
 * Some code by Randy (randy@stat.tamu.edu).
 */


#include "npp.h"
#include "src/random_numbers.h"
#include <time.h>
#include <assert.h>


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
void Rand_state_init(u32b seed)
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
 * Extract a "random" number from 0 to m-1, via "division"
 *
 * This method selects "random" 28-bit numbers, and then uses
 * division to drop those numbers into "m" different partitions,
 * plus a small non-partition to reduce bias, taking as the final
 * value the first "good" partition that a number falls into.
 *
 * This method has no bias, and is much less affected by patterns
 * in the "low" bits of the underlying RNG's.
 *
 * Note that "m" must not be greater than 0x1000000, or division
 * by zero will result.
 */
u32b Rand_div(u32b m)
{
    u32b r, n;

    /* Hack -- simple case */
    if (m <= 1) return (0);

    /* Partition size */
    n = (0x10000000 / m);

    /* Use a simple RNG */
    if (Rand_quick)
    {
        /* Wait for it */
        while (1)
        {
            /* Cycle the generator */
            r = (Rand_value = LCRNG(Rand_value));

            /* Mutate a 28-bit "random" number */
            r = ((r >> 4) & 0x0FFFFFFF) / n;

            /* Done */
            if (r < m) break;
        }
    }

    /* Use a complex RNG */
    else
    {
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
            r = ((r >> 4) & 0x0FFFFFFF) / n;

            /* Advance the index */
            Rand_place = j;

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
    tmp = (s16b)rand_int(32768);

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
    if (one_in_(2)) return (mean - offset);

    /* One half should be positive */
    return (mean + offset);
}





/*
 * Generates damage for "2d6" style dice rolls
 */
int damroll(int num, int sides)
{
    int i;
    int sum = 0;

/*	assert(sides > 0); */
    if (sides <= 0) return (0);

    for (i = 0; i < num; i++)
        sum += randint1(sides);

    return (sum);
}

/*
 * Generates damage for "2d6" style dice rolls
 */
int max_damroll(int num, int sides, int tries)
{
    int best_sum = 1;
    int i;

    for (i = 0; i < tries; i++)
    {
        int sum = damroll(num, sides);

        if (sum > best_sum) best_sum = sum;
    }

    return (best_sum);
}


/*
 * Calculation helper function for randcalc
 */
int damcalc(int num, int sides, aspect dam_aspect)
{
    int val = 0;

    switch (dam_aspect)
    {
        case MAXIMISE:
        case EXTREMIFY:
        {
            val = num * sides;
            break;
        }
        case RANDOMISE:
        {
            val = damroll(num, sides);
            break;
        }
        case MINIMISE:
        {
            val = num;
            break;
        }
        case AVERAGE:
        {
            val = num * (sides + 1) / 2;
            break;
        }
    }

    return (val);
}


/**
 * Generates a random signed long integer X where `A` <= X <= `B`.
 * The integer X falls along a uniform distribution.
 *
 * Note that "rand_range(0, N-1)" == "randint0(N)".
 */
int rand_range(int A, int B)
{
    if (A == B) return A;
    assert(A < B);

    return A + (s32b)Rand_div(1 + B - A);
}





/*
 * Given a numerator and a denominator, supply a properly rounded result,
 * using the RNG to smooth out remainders.  -LM-
 */
s32b div_round(s32b n, s32b d)
{
    s32b tmp;

    /* Refuse to divide by zero */
    if (!d) return (n);

    /* Division */
    tmp = n / d;

    /* Rounding */
    if ((ABS(n) % ABS(d)) > rand_int(ABS(d)))
    {
        /* Increase the absolute value */
        if (n * d > 0L) tmp += 1L;
        else            tmp -= 1L;
    }

    /* Return */
    return (tmp);
}



/*
 * Help determine an "enchantment bonus" for an object.
 *
 * To avoid floating point but still provide a smooth distribution of bonuses,
 * we simply round the results of division in such a way as to "average" the
 * correct floating point value.
 *
 * This function has been changed.  It uses "Rand_normal()" to choose values
 * from a normal distribution, whose mean moves from zero towards the max as
 * the level increases, and whose standard deviation is equal to 1/4 of the
 * max, and whose values are forced to lie between zero and the max, inclusive.
 *
 * Since the "level" rarely passes 100 before Morgoth is dead, it is very
 * rare to get the "full" enchantment on an object, even a deep levels.
 *
 * It is always possible (albeit unlikely) to get the "full" enchantment.
 *
 * A sample distribution of values from "m_bonus(10, N)" is shown below:
 *
 *   N       0     1     2     3     4     5     6     7     8     9    10
 * ---    ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
 *   0   66.37 13.01  9.73  5.47  2.89  1.31  0.72  0.26  0.12  0.09  0.03
 *   8   46.85 24.66 12.13  8.13  4.20  2.30  1.05  0.36  0.19  0.08  0.05
 *  16   30.12 27.62 18.52 10.52  6.34  3.52  1.95  0.90  0.31  0.15  0.05
 *  24   22.44 15.62 30.14 12.92  8.55  5.30  2.39  1.63  0.62  0.28  0.11
 *  32   16.23 11.43 23.01 22.31 11.19  7.18  4.46  2.13  1.20  0.45  0.41
 *  40   10.76  8.91 12.80 29.51 16.00  9.69  5.90  3.43  1.47  0.88  0.65
 *  48    7.28  6.81 10.51 18.27 27.57 11.76  7.85  4.99  2.80  1.22  0.94
 *  56    4.41  4.73  8.52 11.96 24.94 19.78 11.06  7.18  3.68  1.96  1.78
 *  64    2.81  3.07  5.65  9.17 13.01 31.57 13.70  9.30  6.04  3.04  2.64
 *  72    1.87  1.99  3.68  7.15 10.56 20.24 25.78 12.17  7.52  4.42  4.62
 *  80    1.02  1.23  2.78  4.75  8.37 12.04 27.61 18.07 10.28  6.52  7.33
 *  88    0.70  0.57  1.56  3.12  6.34 10.06 15.76 30.46 12.58  8.47 10.38
 *  96    0.27  0.60  1.25  2.28  4.30  7.60 10.77 22.52 22.51 11.37 16.53
 * 104    0.22  0.42  0.77  1.36  2.62  5.33  8.93 13.05 29.54 15.23 22.53
 * 112    0.15  0.20  0.56  0.87  2.00  3.83  6.86 10.06 17.89 27.31 30.27
 * 120    0.03  0.11  0.31  0.46  1.31  2.48  4.60  7.78 11.67 25.53 45.72
 * 128    0.02  0.01  0.13  0.33  0.83  1.41  3.24  6.17  9.57 14.22 64.07
 */
s16b m_bonus(int max, int level)
{
    int bonus, stand, extra, value;

    /* Paranoia -- enforce maximal "level" */
    if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;

    /* The "bonus" moves towards the max */
    bonus = ((max * level) / MAX_DEPTH);

    /* Hack -- determine fraction of error */
    extra = ((max * level) % MAX_DEPTH);

    /* Hack -- simulate floating point computations */
    if (randint0(MAX_DEPTH) < extra) bonus++;


    /* The "stand" is equal to one quarter of the max */
    stand = (max / 4);

    /* Hack -- determine fraction of error */
    extra = (max % 4);

    /* Hack -- simulate floating point computations */
    if (randint0(4) < extra) stand++;


    /* Choose an "interesting" value */
    value = Rand_normal(bonus, stand);

    /* Enforce the minimum value */
    if (value < 0) return (0);

    /* Enforce the maximum value */
    if (value > max) return (max);

    /* Result */
    return (value);
}

/*
 * Calculation helper function for m_bonus
 */
s16b m_bonus_calc(int max, int level, aspect bonus_aspect)
{
    int val = 0;

    switch (bonus_aspect)
    {
        case EXTREMIFY:
        case MAXIMISE:
        {
            val = max;
            break;
        }
        case RANDOMISE:
        {
            val = m_bonus(max, level);
            break;
        }
        case MINIMISE:
        {
            val = 0;
            break;
        }
        case AVERAGE:
        {
            val = max * level / MAX_DEPTH;
            break;
        }
    }

    return (val);
}



/*
 * Calculation helper function for random_value structs
 */
int randcalc(random_value v, int level, aspect rand_aspect)
{
    s16b total;

    if (rand_aspect == EXTREMIFY)
    {
        int min, max;

        min = randcalc(v, level, MINIMISE);
        max = randcalc(v, level, MAXIMISE);

        if (abs(min) > abs(max))
            total = min;
        else
            total = max;
    }
    else
    {
        total = v.base +
                damcalc(v.dice, v.sides, rand_aspect) +
                m_bonus_calc(v.m_bonus, level, rand_aspect);
    }

    return total;
}

/*
 * Test to see if a value is within a random_value's range
 */
bool randcalc_valid(random_value v, int test)
{
    if (test < randcalc(v, 0, MINIMISE))
        return FALSE;

    if (test > randcalc(v, 0, MAXIMISE))
        return FALSE;

    return TRUE;
}

/*
 * Test to see if a random_value actually varies
 */
bool randcalc_varies(random_value v)
{
    if (randcalc(v, 0, MINIMISE) == randcalc(v, 0, MAXIMISE))
        return FALSE;

    return TRUE;
}


/*
 * Extract a "random" number from 0 to m-1, using the "simple" RNG.
 *
 * This function should be used when generating random numbers in
 * "external" program parts like the main-*.c files.  It preserves
 * the current RNG state to prevent influences on game-play.
 */
u32b Rand_simple(u32b m)
{
    /* Use the value */
    return (qrand() % m);
}
