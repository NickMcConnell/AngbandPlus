/* File: bitflag.h */

/*
 * Copyright (c) 2007 Kenneth Boyd.  This file is subject to the Boost license.
 */

/*
 * alternate flag approach for 32-bit bitvectors
 */
#define FLAG_FROM_INDEX(F) (1UL<<((F)%32))
#define OFFSET_FROM_INDEX(F) ((F)/32)
#define TEST_FLAG(A,F) (A[OFFSET_FROM_INDEX(F)] & FLAG_FROM_INDEX(F))
#define SET_FLAG(A,F) A[OFFSET_FROM_INDEX(F)] |= FLAG_FROM_INDEX(F)
#define RESET_FLAG(A,F) A[OFFSET_FROM_INDEX(F)] &= ~(FLAG_FROM_INDEX(F))

