/*
 * flow.c
 *
 * Copyright 2007, Kenneth Boyd.
 *
 * Generalized flowing/distance/progagation etc. code
 *
 * This file is under the Boost License V1.0.
 */

#include "flow.h"
#include "keypad.h"
#include <string.h>
#include <assert.h>

/* default to V distance */
#ifndef distance
#define distance V_distance
#endif

/*
 * raw indexes a "distance square" with sides 2*range+1.  Logical 0 is the midpoint.
 * range is the range to be flowed.  Should be between 1 and 127.
 * start is the starting coordinate in the actual map.
 * test is a terrain-approval function.
 * 
 * This is essentially a Djikstra implementation, with some enhancements.
 *
 * 0 is unreachable.  Other values are effectively steps+1 to reach.
 */

#define WRAPQUEUE_LEN 4*254+1

void flow_from(unsigned char* raw,unsigned char range,coord g,coord_action* test,coord_action* goal_cond,const coord* goal_target)
{
	coord_delta wrap_queue[WRAPQUEUE_LEN];
	unsigned char depth_queue[WRAPQUEUE_LEN];
	size_t trailing_edge = 0;
	size_t leading_edge = 0;

	const int side_length = 2*range+1;

	coord_delta iter_coord;
	size_t i;
	unsigned char goal_cond_dis = 0;
	unsigned char target_dis = 0;
	coord_delta target_at;

	assert(NULL != test);								/* this had better be real, we delegate out-of-bounds calculations to it */
	assert(NULL == goal_cond || NULL == goal_target);	/* not a good idea to both have goal and a target */

	memset(raw,0,side_length*side_length*sizeof(unsigned char));

	/* use 255 as a temporary flag for unreachable */

	/* start process */
	wrap_queue[leading_edge] = coord_delta(0,0); 
	raw[distance_square_idx(wrap_queue[leading_edge],range)] = 1;
	depth_queue[leading_edge++] = 1;

	do	{
		const size_t old_trailing_edge = trailing_edge;
		const coord tmp2(g+wrap_queue[trailing_edge]);
		const unsigned char next_depth = depth_queue[trailing_edge]+1;

		/* don't exceed flowing range */
		if (range<next_depth)	break;

		trailing_edge = (trailing_edge+1)%WRAPQUEUE_LEN;

		/* if looking for a specific coordinate and found it, stop */
		if (goal_target && tmp2==*goal_target)
		{
			target_dis = depth_queue[old_trailing_edge];
			target_at = wrap_queue[old_trailing_edge];
			break;
		}

		/* if looking for a specific condition and found it, stop later depths */
		if (goal_cond)
		{
			if (0<goal_cond_dis)	continue;
			if (goal_cond(tmp2))
			{
				goal_cond_dis = depth_queue[old_trailing_edge];
				continue;
			}
		}

		i = 0;
		do	{
			coord_delta tmp(wrap_queue[old_trailing_edge] + dd_coord_ddd[i]);

			if (0 != raw[distance_square_idx(tmp,range)]) continue;
			if (test && !test(coord(g+tmp)))
			{
				raw[distance_square_idx(tmp,range)] = 255;
				continue;
			}
			leading_edge = (leading_edge+1)%WRAPQUEUE_LEN;
			assert(leading_edge!=old_trailing_edge);
			wrap_queue[leading_edge] = tmp;
			raw[distance_square_idx(tmp,range)] = next_depth;
			depth_queue[leading_edge] = next_depth;
			}
		while(++i < 8);
		}
	while(leading_edge!=trailing_edge);

	/* cleanup: rewrite 255 to 0 */
	for(iter_coord.y = -range; iter_coord.y<=range; ++iter_coord.y)
	{
		for(iter_coord.x = -range; iter_coord.x<=range; ++iter_coord.x)
		{
			if (255 == raw[distance_square_idx(iter_coord,range)])
				 raw[distance_square_idx(iter_coord,range)] = 0;
		}
	}

	/* if we found the target, remove all other marks at that distance */
	if (0<target_dis)
	{
		for(iter_coord.y = -range; iter_coord.y<=range; ++iter_coord.y)
		{
			for(iter_coord.x = -range; iter_coord.x<=range; ++iter_coord.x)
			{
				if (   target_dis == raw[distance_square_idx(iter_coord,range)]
					&& target_at!=iter_coord)
					raw[distance_square_idx(iter_coord,range)] = 0;
			}
		}
	}

	/* same idea if we found the goal */
	if (0<goal_cond_dis)
	{
		for(iter_coord.y = -range; iter_coord.y<=range; ++iter_coord.y)
		{
			for(iter_coord.x = -range; iter_coord.x<=range; ++iter_coord.x)
			{
				if (goal_cond_dis == raw[distance_square_idx(iter_coord,range)])
				{
					const coord tmp2(wrap_queue[trailing_edge].x+g.x,wrap_queue[trailing_edge].y+g.y);
					if (!goal_cond(tmp2))
						raw[distance_square_idx(iter_coord,range)] = 0;
				}
			}
		}
	}

	/* if we have either a goal or a target, prune useless moves */
	if (NULL!=goal_cond || NULL!=goal_target)
	{
		size_t target_depth = range;
		if (0<goal_cond_dis) target_depth = goal_cond_dis;
		if (0<target_dis) target_depth = target_dis;
		while(2<target_depth)
		{
			--target_depth;
			/* to stay alive, a grid must be able to flow further away */
			for(iter_coord.y = -range; iter_coord.y<=range; ++iter_coord.y)
			{
				for(iter_coord.x = -range; iter_coord.x<=range; ++iter_coord.x)
				{
					if (target_depth == raw[distance_square_idx(iter_coord,range)])
					{
						bool alive = false;

						for(i = 0; i < 8; ++i)
						{
							coord_delta tmp = iter_coord + dd_coord_ddd[i];
							if (target_depth+1 == raw[distance_square_idx(tmp,range)])
							{
								alive = true;
								break;
							}
						}

						if (!alive)
							raw[distance_square_idx(iter_coord,range)] = 0;
					}
				}
			}
		}
	}
}

size_t squares_in_view_octagon(int range)
{
	assert(0<=range);

	size_t count = 1;
	int radius = 2;
	if (1<=range) count += 8;

	while(radius<=range)
	{
		int spread = 1;
		int step_back = radius;

		/* primaries */
		count += 4;

		while(spread<step_back)
		{
			count += 8;
			if (0 == ++spread%2) --step_back;
		}
		if (spread==step_back)	count += 4;
		
		++radius;
	}
	return count;
}

void check_these_for_view(coord* coord_list,coord src, int range, size_t& StrictUB, int height, int width)
{
	assert(NULL!=coord_list);
	assert(0<range);
	/* can't check that coord_list is large enough at runtime from here */

	int radius = 2;
	int i;
	int clipped[4] = {range, range, range, range};
	int y_extent = range+(int)src.y;
	int x_extent = range+(int)src.x;

	/* preview for in-bounds */
	/* note the unintuitive ordering, these hard constants 0..3 should be macroed just-in-case */
	if (y_extent>height-1) clipped[0] -= (y_extent-(height-1));	/* due east */
	if (range>(int)src.y) clipped[1] = (int)src.y;				/* due west */
	if (x_extent>width-1) clipped[2] -= (x_extent-(width-1));	/* due south */
	if (range>(int)src.x) clipped[3] = (int)src.x;				/* due north */

	/* radius 0: src */
	StrictUB = 0;
	coord_list[StrictUB++] = src;

	/* radius 1 is special */
	for(i=0; i<KEYPAD_DIR_MAX; ++i)
	{
		coord_list[StrictUB++] = src+dd_coord_ddd[i];
	}

	while(radius<=range)
	{
		int spread = 1;
		int step_back = radius;

		/* primaries */
		for(i=0; i<KEYPAD_CARDINAL_DIR_MAX; ++i)
		{
			/* XXX should have proper * operator for coord_delta XXX */
			if (step_back<=clipped[i]) coord_list[StrictUB++] = src+coord_delta(radius*dd_coord_ddd[i].x,radius*dd_coord_ddd[i].y);
		}

		while(spread<step_back)
		{
			if (step_back<=clipped[2] && spread<=clipped[0]) coord_list[StrictUB++] = src+coord_delta(step_back,spread);
			if (step_back<=clipped[2] && spread<=clipped[1]) coord_list[StrictUB++] = src+coord_delta(step_back,-spread);
			if (step_back<=clipped[3] && spread<=clipped[0]) coord_list[StrictUB++] = src+coord_delta(-step_back,spread);
			if (step_back<=clipped[3] && spread<=clipped[1]) coord_list[StrictUB++] = src+coord_delta(-step_back,-spread);

			if (spread<=clipped[2] && step_back<=clipped[0]) coord_list[StrictUB++] = src+coord_delta(spread,step_back);
			if (spread<=clipped[2] && step_back<=clipped[1]) coord_list[StrictUB++] = src+coord_delta(spread,-step_back);
			if (spread<=clipped[3] && step_back<=clipped[0]) coord_list[StrictUB++] = src+coord_delta(-spread,step_back);
			if (spread<=clipped[3] && step_back<=clipped[1]) coord_list[StrictUB++] = src+coord_delta(-spread,-step_back);

			if (0 == ++spread%2) --step_back;
		}
		if (spread==step_back)
		{	/* diagonal */
			if (spread<=clipped[2] && spread<=clipped[0]) coord_list[StrictUB++] = src+coord_delta(spread,spread);
			if (spread<=clipped[2] && spread<=clipped[1]) coord_list[StrictUB++] = src+coord_delta(spread,-spread);
			if (spread<=clipped[3] && spread<=clipped[0]) coord_list[StrictUB++] = src+coord_delta(-spread,spread);
			if (spread<=clipped[3] && spread<=clipped[1]) coord_list[StrictUB++] = src+coord_delta(-spread,-spread);
		}
		
		++radius;
	}
}

static int project_path_aux(coord* gp, const coord g1, int ax, int ay, int sx, int sy, int range, const coord g2, bool stop_dest, coord_action* stop_terrain)
{
	int i;
	int n = 0;

	bool x_init = false;
	bool y_init = false;
	bool hit_target = false;

	/* defaults */
	if (ax >= ay)
	{
		i = 0;
		gp[0].x = g1.x+sx;
		while(++i<range)
		{
			gp[i].x = gp[i-1].x + sx;
		}
		x_init = true;
	}
	if (ay >= ax)
	{
		i = 0;
		gp[0].y = g1.y+sy;
		while(++i<range)
		{
			gp[i].y = gp[i-1].y + sy;
		}		
		y_init = true;
	}
	if (0 == ax)
	{
		i = 0;
		do	{
			gp[i].x = g1.x;
			}
		while(++i<range);
		x_init = true;
	}
	if (0 == ay)
	{
		i = 0;
		do	{
			gp[i].y = g1.y;
			}
		while(++i<range);
		y_init = true;
	}
Retry_path:
	if (!x_init)
	{
		int frac = 0;
		i = 0;
		gp[0].x = g1.x;
		frac += ax;
		if (2*frac>ay)
		{
			gp[i].x += sx;
			frac -= ay;
		}
		while(++i<range)
		{
			gp[i].x = gp[i-1].x;
			frac += ax;
			if (2*frac>ay)
			{
				gp[i].x += sx;
				frac -= ay;
			}
		}
	}
	if (!y_init)
	{
		int frac = 0;
		i = 0;
		gp[0].y = g1.y;
		frac += ay;
		if (2*frac>ax)
		{
			gp[i].y += sy;
			frac -= ax;
		}
		while(++i<range)
		{
			gp[i].y = gp[i-1].y;
			frac += ay;
			if (2*frac>ax)
			{
				gp[i].y += sy;
				frac -= ax;
			}
		}
	}

	/* now have default path, determine whether it works */
	n = 0;
	do	{
		/* Sometimes stop at destination grid */
		if (g2==gp[n])
		{
			hit_target = true;
			if (stop_dest) {++n; break;}
		}

		/* stop on blocking terrain */
		if (stop_terrain(gp[n])) {++n; break;};
		}
	while(++n<range && distance(gp[n],g1)<range);

	if (!hit_target && 0 != ax && 0 != ay)
	{	/* try to fix this */
		--n;
		if (1 == ay && 3 <= ax && g1.y!=gp[n].y)
		{	/* horizontal trick shot, fixable */
			ax = 2*n+3;
			goto Retry_path;	
		}
		if (1 == ax && 3 <= ay && g1.x!=gp[n].x)
		{	/* vertical trick shot, fixable */
			ay = 2*n+3;
			goto Retry_path;	
		}
		if (ax < ay && 1 == ay-ax && abs((int)gp[n].x-g1.x) != abs((int)gp[n].y-g1.y))
		{	/* diagonal trick shot, includes reversing knight-move */
			ay = 2*n+3;
			ax = ay-1;
			goto Retry_path;
		}
		if (ay < ax && 1 == ax-ay && abs((int)gp[n].x-g1.x) != abs((int)gp[n].y-g1.y))
		{	/* diagonal trick shot #2,  includes reversing knight-move*/
			ax = 2*n+3;
			ay = ax-1;
			goto Retry_path;
		}
		++n;
	}
	return n;
}

/*
 * Find the path taken by a projection, starting from g1 and intending to
 * pass through g2.  Enter one grid per unit of distance on major axis.  Stop 
 * on destination grid if stop_dest is true; stop on whatever terrain is indicated as impassable
 * by stop_terrain.  Path grids are saved into the grid array gp; memory allocation is the caller's problem.
 *
 * stop_dest indicates whether the projection should stop on reaching its destination.
 * stop_terrain is a non-NULL function pointer evaluating whether the terrain in the grid stops the projection.
 * The return value is the number of grids in the path.  The path has zero length if and only if g1==g2.
 *
 * This function was designed to prevent the classic hockey puck corridor exploit.
 */
int project_path(coord *gp, int range, coord g1, coord g2, bool stop_dest, coord_action* stop_terrain)
{
	assert(NULL!=stop_terrain);
	assert(NULL!=gp);

	/* No path necessary (or allowed) */
	if (g1==g2) return (0);

	{ /* C-ish blocking brace */
	/* Absolute */
	int ay = 0;
	int ax = 0;

	/* Offsets */
	int sy = 0;
	int sx = 0;

	/* Analyze "dy" */
	if (g2.y < g1.y)
	{
		ay = (g1.y - g2.y);
		sy = -1;
	}
	else if (g2.y > g1.y)
	{
		ay = (g2.y - g1.y);
		sy = 1;
	}

	/* Analyze "dx" */
	if (g2.x < g1.x)
	{
		ax = (g1.x - g2.x);
		sx = -1;
	}
	else if (g2.x > g1.x)
	{
		ax = (g2.x - g1.x);
		sx = 1;
	}

	/* Length */
	return project_path_aux(gp,g1,ax,ay,sx,sy,range,g2,stop_dest,stop_terrain);
	} /* end blocking brace */
}

