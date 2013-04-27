/* File: keypad.c */

#include "keypad.h"

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

