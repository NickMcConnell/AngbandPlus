/* File: keypad.h */

#ifndef KEYPAD_H

#include "types2.h"

/*
 * Copyright (c) 2007 Kenneth Boyd.  This file is subject to the Boost license V1.0.
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
 * The specification pretty much dictates the abstract representation, up to orientation.
 */

/* count of valid keypad directions, and cardinal directions N/S/E/W */
#define KEYPAD_DIR_MAX 8
#define KEYPAD_CARDINAL_DIR_MAX 4

/* adjust these and rebuild if you need a different convention */
#define KEYPAD_NORTH_DELTA -1
#define KEYPAD_SOUTH_DELTA 1
#define KEYPAD_WEST_DELTA -1
#define KEYPAD_EAST_DELTA 1

extern const unsigned char ddd[KEYPAD_DIR_MAX+1];	/* indexes valid keypad directions; 5 is last */

extern const signed char ddx[10];		/* x-offset of keypad direction */
extern const signed char ddy[10];		/* y-offset of keypad direction */
extern const coord_delta dd_coord[10];	/* keypad direction as a coord_delta */

extern const signed char ddx_ddd[KEYPAD_DIR_MAX+1];		/* optimize ddx[ddd[i]] */
extern const signed char ddy_ddd[KEYPAD_DIR_MAX+1];		/* optimize ddy[ddd[i]] */
extern const coord_delta dd_coord_ddd[KEYPAD_DIR_MAX+1];	/* optimize dd_coord[ddd[i]] */

/*
 * two counter-clockwise iterations of the directions, starting from SW/1
 */
extern const unsigned char cycle[2*KEYPAD_DIR_MAX+1];

/*
 * map all valid directions into the above array.  Guarantee safety of +4 or -4 indexing in all cases.
 * invalid directions will have index 0.
 */
extern const unsigned char chome[10];

/**
 *	Sorts directions array according to move_legal function, which is more of a preference function
 *  \param bad_LB is passed in as a strict upper bound on the array
 */
extern int bad_squares_last(int* const dir_array, coord_action* move_legal, int bad_LB, coord origin);



#endif

