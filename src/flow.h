/*
 * flow.h
 *
 * Copyright 2007, Kenneth Boyd.
 *
 * Generalized flowing/distance/progagation etc. code header
 *
 * This file is under the Boost License V1.0.
 */

#include <stdlib.h>
#include "types2.h"

/**
 * \param raw indexes a "distance square" with sides 2*range+1.  Logical 0 is the midpoint.
 * \param range is the range to be flowed.  Should be between 1 and 127.
 * \param g is the starting coordinate in the actual map.
 * \param test is a terrain-approval function.
 * \param goal_cond is a goal condition function.  It may be NULL.
 * \param goal_target is a target identification function.  It may be NULL.
 * 
 * 0 is unreachable.  Other values are effectively steps+1 to reach.
 */
extern void flow_from(unsigned char* raw,unsigned char range,coord g,coord_action* test,coord_action* goal_cond,const coord* goal_target);

inline size_t distance_square_idx(coord_delta tmp,int range)
{	return (tmp.y+range)*(2*range+1)+(tmp.x+range);	}

/*
 * A "view octagon" is composed of all squares within range of the source, inclusive.  Range is that of the classical 
 * V Angband distance function, although the implementation doesn't use this explicitly.
 */
extern size_t squares_in_view_octagon(int range);
extern void check_these_for_view(coord* coord_list,coord src, int range, size_t& StrictUB, int height, int width);

extern int project_path(coord *gp, int range, coord g1, coord g2, bool stop_dest, coord_action* stop_terrain);

