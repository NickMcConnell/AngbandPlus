/* File: z-probability.c */

/*
 * Copyright (c) 2008 Kenneth Boyd.  This file is subject to the Boost License V1.0.
 */

#include "z-probability.h"
#include "z-rand.h"

void
PRNG_span::bootstrap()
{
	actual = rand_int(RAND_DIV_MAX_M);
	strict_UB = RAND_DIV_MAX_M;
}

unsigned long
PRNG_span::RNG(unsigned long N)
{
	assert(N<=RAND_DIV_MAX_M);
	while(!in_range(N)) bootstrap();
	{
	unsigned long result = actual/(strict_UB/N);
	strict_UB /= N;
	return result;
	}
}

static int safe_NdS(int N, int S)
{
	PRNG_span tmp;
	int sum = 0;

	while(0<N)
	{
		sum += tmp.RNG(S) + 1;
		--N;
	};
	return sum;
}

int NdS(int N, int S)
{
	/* deal with corner cases */
	if (S <= 0) return 0;	/* undefined behavior later on, avoid */
	if (N <= 0) return 0;
	assert((u32b)S<=RAND_DIV_MAX_M);

	return safe_NdS(N,S);
}
