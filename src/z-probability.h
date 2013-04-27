/* File: z-probability.h */

/*
 * Copyright (c) 2008 Kenneth Boyd.  This file is subject to the Boost license.
 */

#ifndef INCLUDED_Z_PROBABILITY_H
#define INCLUDED_Z_PROBABILITY_H 1

/*
 * A somewhat more efficient user of the RNG.
 */
class PRNG_span
{
private:
	unsigned long actual;
	unsigned long strict_UB;
public:
	PRNG_span() : actual(0),strict_UB(1) {};

	bool in_range(unsigned long N) const {return N<=strict_UB && actual<=(strict_UB/N)*N;};
	bool used_up() const {return 1>=strict_UB;};
	void bootstrap();
	unsigned long RNG(unsigned long N);
};

/*
 * sum of N S-sided dice
 */
extern int NdS(int N, int S);

#endif
