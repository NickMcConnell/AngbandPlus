/* File: z-rand.h */

#ifndef INCLUDED_Z_RAND_H
#define INCLUDED_Z_RAND_H

#include "h-basic.h"


/*** Random number generator ***/

/* Linear congruent generator -- 'Super-Duper' */
#define LCG(X) \
	 ((u32b)((u32b)(X) * 69069 + 1))

/* Generate a random number */
#define random \
	 (_rand_u32b_val = LCG(_rand_u32b_val))

/* Seed the generator with X */
#define rand_seed(X) \
	 (_rand_u32b_val = (X))

/* Seed the generator with the time */
#ifndef SET_UID

# define rand_init() rand_seed(time(NULL))

#else

# define rand_init() (rand_seed((time(NULL) >> 3) * (getpid() << 1)))

#endif


/*** Uniform distribution ***/

/*
 * Generate a uniformly distributed u32b X where
 * 0 <= X < N
 */
#define rand_int(N) \
	 (_rand_uniform(N))

/*
 * Generate a uniformly distributed u32b X where
 * 1 <= X <= N
 */
#define randint(N) \
	 (rand_int(N) + 1)

/*
 * Generate a uniformly distributed u32b X where
 * A <= X <= B
 */
#define rand_range(A,B) \
	 ((A) + (rand_int((B) - (A) + 1)))

/*
 * Generate a uniformly distributed u32b X where
 * |X-A| <= D
 */
#define rand_spread(A,D) \
	 ((A) + (rand_int(2 * (D) + 1)) - (D))


/*** Normal distribution ***/

/*
 * Generate a normally distributed s32b X with mean M and standard deviation S where
 * |X-M| <= 6.75*S
 */
#define rand_nor(M,S) \
	 (_rand_normal(M, S))

/*
 * Generate a normally distributed s32b X with mean 0 where
 * -D <= X <= D
 */
#define rand_nor0(D) \
	 (rand_nor(0, D / 2.82842712475))

/*
 * Generate a normally distributed s32b X with mean M where
 * 0 <= X <= 2*M
 */
#define rand_norm(M) \
	 ((M) + rand_nor0(M))


/*** Special ***/

/* A random boolean value */
#define rand_bool \
	 (_rand_bool())

/* Return TRUE P percent of the time */
#define percent(P) \
	(rand_int(100) < P)

/* Maximal value of XdY */
#define maxroll(X, Y) \
	((X) * (Y))

/* Average value of XdY */
#define averoll(X, Y) \
	((X) * ((Y) + 1) / 2)

/* Contest roll - (9d9 - 45) */
#define conroll \
	(damroll(9, 9) - 45)


/* Critical success/failure */
#define CRITICAL  36


/*** Global variables ***/

extern u32b _rand_u32b_val;
extern u32b _rand_bool_val;
extern int _rand_bool_pos;


/*** Global functions ***/

extern u32b _rand_uniform(u32b m);
extern bool _rand_bool(void);
extern s32b _rand_normal(f32b m, f32b s);
extern u32b damroll(u32b x, u32b y);
extern s32b contest(s32b a, s32b b);


#endif

