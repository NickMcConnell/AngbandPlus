/*
 * File: z-rand.h
 * Purpose: A Random Number Generator for Angband
 */

#ifndef INCLUDED_Z_RAND_H
#define INCLUDED_Z_RAND_H

/*
 * Assumed maximum dungeon level. This value is used for various
 * calculations involving object and monster creation. It must be at least
 * 100. Setting it below 128 may prevent the creation of some objects.
 */
#define MAX_RAND_DEPTH 128

/*
 * A struct representing a strategy for making a dice roll.
 *
 * The result will be base + XdY + BONUS, where m_bonus is used in a
 * tricky way to determine BONUS.
 */
typedef struct random
{
    int base;
    int dice;
    int sides;
    int m_bonus;
} random_value;

/*
 * The number of 32-bit integers worth of seed state.
 */
#define RAND_DEG 32

/*
 * Random aspects used by damcalc, m_bonus_calc, and ranvals
 */
typedef enum
{
    MAXIMISE,
    RANDOMISE,
    MINIMISE,
    AVERAGE
} aspect;

/*
 * Generates a random signed long integer X where "0 <= X < M" holds.
 *
 * The integer X falls along a uniform distribution.
 */
#define randint0(M) ((s32b)Rand_div(M))

/*
 * Generates a random signed long integer X where "1 <= X <= M" holds.
 *
 * The integer X falls along a uniform distribution.
 */
#define randint1(M) ((s32b)Rand_div(M) + 1)

/*
 * Generate a random signed long integer X where "A - D <= X <= A + D" holds.
 * Note that "rand_spread(A, D)" == "rand_range(A - D, A + D)"
 *
 * The integer X falls along a uniform distribution.
 */
#define rand_spread(A, D) ((A) + (randint0(1 + (D) + (D))) - (D))

/*
 * Return true one time in `x`.
 */
#define one_in_(x)  (!randint0(x))

/*
 * Evaluate to true "P" percent of the time
 */
#define magik(P) (randint0(100) < (P))

/*
 * Evaluate to true A times out of B
 */
#define CHANCE(A, B) (randint0(B) < (A))

/*
 * Whether we are currently using the "quick" method or not.
 */
extern bool Rand_quick;

/*
 * The state used by the "quick" RNG.
 */
extern u32b Rand_value;

/*
 * The state used by the "complex" RNG.
 */
extern u32b state_i;
extern u32b STATE[RAND_DEG];
extern u32b z0;
extern u32b z1;
extern u32b z2;

/*
 * Initialize the RNG state with the given seed.
 */
extern void Rand_state_init(u32b seed);

/*
 * Initialize the RNG
 */
extern void Rand_init(void);

/*
 * Generates a random unsigned long integer X where "0 <= X < M" holds.
 *
 * The integer X falls along a uniform distribution.
 */
extern u32b Rand_div(u32b m);

/*
 * Generate a signed random integer within `stand` standard deviations of
 * `mean`, following a normal distribution.
 */
extern s16b Rand_normal(int mean, int stand);

/*
 * Generate a semi-random number from 0 to m-1, in a way that doesn't affect
 * gameplay.  This is intended for use by external program parts like the
 * main-*.c files.
 */
extern u32b Rand_simple(u32b m);

/*
 * Emulate a number `num` of dice rolls of dice with `sides` sides.
 */
extern int damroll(int num, int sides);

/*
 * Calculation helper function for damroll
 */
extern int damcalc(int num, int sides, aspect dam_aspect);

/*
 * Generates a random signed long integer X where "A <= X <= B"
 * Note that "rand_range(0, N-1)" == "randint0(N)"
 *
 * The integer X falls along a uniform distribution.
 */
extern int rand_range(int A, int B);

/*
 * Function used to determine enchantment bonuses, see function header for
 * a more complete description.
 */
extern s16b m_bonus(int max, int level);

/*
 * Calculation helper function for m_bonus.
 */
extern s16b m_bonus_calc(int max, int level, aspect bonus_aspect);

/*
 * Calculation helper function for random_value structs
 */
extern int randcalc(random_value v, int level, aspect rand_aspect);

/*
 * Test to see if a value is within a random_value's range
 */
extern bool randcalc_valid(random_value v, int test);

/*
 * Test to see if a random_value actually varies
 */
extern bool randcalc_varies(random_value v);

/*
 * Generates a random unsigned long integer X where "0 <= X < M" holds.
 *
 * The integer X falls along a uniform distribution.
 */
extern u32b Rand_mod(u32b m);

/*
 * Test the integrity of the RNG
 */
extern u32b Rand_test(u32b seed);

#endif
