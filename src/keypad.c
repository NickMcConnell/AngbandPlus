/* File: keypad.c */

#include "keypad.h"

#ifdef __cplusplus
#include <cstddef>
#include <cassert>
#else
#include <stddef.h>
#include <assert.h>
#endif

/*
 * Copyright (c) 2007 Kenneth Boyd.   This file is under the Boost License V1.0.
 */

/*
 * Rationale:
 * A typical numeric keypad has the following layout:
 * 789
 * 456
 * 123
 *
 * Traditionally, 5 has been used as the no-move key, and the other 8 mapped to elementary moves as follows:
 * NW N NE
 *  W   E
 * SW S SE
 *
 * The specification pretty much dictates the abstract representation.
 */

const unsigned char ddd[KEYPAD_DIR_MAX+1] =
{ 2, 8, 6, 4, 3, 1, 9, 7, 5 };

const signed char ddx[10] =
{ 0, KEYPAD_WEST_DELTA, 0, KEYPAD_EAST_DELTA, KEYPAD_WEST_DELTA, 0, KEYPAD_EAST_DELTA, KEYPAD_WEST_DELTA, 0, KEYPAD_EAST_DELTA };

const signed char ddy[10] =
{ 0, KEYPAD_SOUTH_DELTA, KEYPAD_SOUTH_DELTA, KEYPAD_SOUTH_DELTA, 0, 0, 0, KEYPAD_NORTH_DELTA, KEYPAD_NORTH_DELTA, KEYPAD_NORTH_DELTA };

const coord_delta dd_coord[10]	=	{
									coord_delta(0,					0),
									coord_delta(KEYPAD_WEST_DELTA,	KEYPAD_SOUTH_DELTA),
									coord_delta(0,					KEYPAD_SOUTH_DELTA),
									coord_delta(KEYPAD_EAST_DELTA,	KEYPAD_SOUTH_DELTA),
									coord_delta(KEYPAD_WEST_DELTA,	0),
									coord_delta(0,					0),
									coord_delta(KEYPAD_EAST_DELTA,	0),
									coord_delta(KEYPAD_WEST_DELTA,	KEYPAD_NORTH_DELTA),
									coord_delta(0,					KEYPAD_NORTH_DELTA),
									coord_delta(KEYPAD_EAST_DELTA,	KEYPAD_NORTH_DELTA)
									};

const signed char ddx_ddd[KEYPAD_DIR_MAX+1] =
{ 0, 0, KEYPAD_EAST_DELTA, KEYPAD_WEST_DELTA, KEYPAD_EAST_DELTA, KEYPAD_WEST_DELTA, KEYPAD_EAST_DELTA, KEYPAD_WEST_DELTA, 0 };

const signed char ddy_ddd[KEYPAD_DIR_MAX+1] =
{ KEYPAD_SOUTH_DELTA, KEYPAD_NORTH_DELTA, 0, 0, KEYPAD_SOUTH_DELTA, KEYPAD_SOUTH_DELTA, KEYPAD_NORTH_DELTA, KEYPAD_NORTH_DELTA, 0 };

const coord_delta dd_coord_ddd[KEYPAD_DIR_MAX+1]	=	{
										coord_delta(0,					KEYPAD_SOUTH_DELTA),
										coord_delta(0,					KEYPAD_NORTH_DELTA),
										coord_delta(KEYPAD_EAST_DELTA,	0),
										coord_delta(KEYPAD_WEST_DELTA,	0),
										coord_delta(KEYPAD_EAST_DELTA,	KEYPAD_SOUTH_DELTA),
										coord_delta(KEYPAD_WEST_DELTA,	KEYPAD_SOUTH_DELTA),
										coord_delta(KEYPAD_EAST_DELTA,	KEYPAD_NORTH_DELTA),
										coord_delta(KEYPAD_WEST_DELTA,	KEYPAD_NORTH_DELTA),
										coord_delta(0,					0)
										};

const unsigned char cycle[2*KEYPAD_DIR_MAX+1] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

const unsigned char chome[10] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };

/**
 * moves squares flagged by move_legal as bad to the end of the array; returns number of good squares.
 *
 * \ret number of squares that move_legal returns true for
 *
 * \pre dir_array contains at least two valid, distinct directions.
 */
int bad_squares_last(int* const dir_array, coord_action* move_legal, int bad_LB, coord origin)
{
	int goodUB = 0;
	coord temp_g_LB(origin);
	coord temp_g_UB(origin);

	assert(NULL != move_legal);
	assert(2 <= bad_LB);
	assert(10 >= bad_LB);

#ifndef NDEBUG
	{
	int i;
	for(i = 0; i < bad_LB; ++i) assert((0 <= dir_array[i]) && (9 >= dir_array[i]));
	}
#endif

	temp_g_LB += dd_coord[dir_array[goodUB]];
	temp_g_UB += dd_coord[dir_array[--bad_LB]];

	while(goodUB<bad_LB)
	{
		if (move_legal(temp_g_LB))
		{
			temp_g_LB = origin + dd_coord[dir_array[++goodUB]];
		}
		else if (!move_legal(temp_g_UB))
		{
			temp_g_UB = origin + dd_coord[dir_array[--bad_LB]];
		}
		else
		{
			const int tmp = dir_array[goodUB];
			const coord tmp2 = temp_g_LB;
			dir_array[goodUB] = dir_array[bad_LB];
			dir_array[bad_LB] = tmp;
			temp_g_LB = temp_g_UB;
			temp_g_UB = tmp2;
		}
	}
	if (move_legal(temp_g_LB)) ++goodUB;

	return goodUB;
}
