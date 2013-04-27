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

#include "rational.hpp"
using zaiband::rational;

#define WRAPQUEUE_LEN 4*254+1

/*
 * This is essentially a Djikstra implementation, with some enhancements.
 * goal_cond and goal_target both dominate test
 */
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

/** initialize a coordinate list for a view octagon
 *
 * \param coord_list initialize coordinate list here.  Memory allocation is the caller's problem.
 * \param src
 * \param range maximum distance of a square to consider
 * \param[out] StrictUB how many squares we actually initialized
 * \param height map height
 * \param width map width
 */
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

static void project_path_aux_pathgen(coord* gp, const coord g1, int range, coord_scan a, coord_delta s, bool x_init, bool y_init)
{
	if (!x_init)
	{
		int frac = 0;
		int i = 0;
		gp[0].x = g1.x;
		frac += a.x;
		if (2*frac>a.y)
		{
			gp[i].x += s.x;
			frac -= a.y;
		}
		while(++i<range)
		{
			gp[i].x = gp[i-1].x;
			frac += a.x;
			if (2*frac>a.y)
			{
				gp[i].x += s.x;
				frac -= a.y;
			}
		}
	}
	if (!y_init)
	{
		int frac = 0;
		int i = 0;
		gp[0].y = g1.y;
		frac += a.y;
		if (2*frac>a.x)
		{
			gp[i].y += s.y;
			frac -= a.x;
		}
		while(++i<range)
		{
			gp[i].y = gp[i-1].y;
			frac += a.y;
			if (2*frac>a.x)
			{
				gp[i].y += s.y;
				frac -= a.x;
			}
		}
	}
}

static int project_path_aux_pathtest(const coord* gp, const coord g1, const coord g2, int range, bool stop_dest, coord_action* stop_terrain)
{
	int n = 0;
	/* XXX suboptimal, could lift stop_dest test out and fork loops XXX */
	do	{
		/* Sometimes stop at destination grid */
		if (g2==gp[n])
		{
			if (stop_dest) {++n; break;}
		}

		/* stop on blocking terrain */
		if (stop_terrain(gp[n])) {++n; break;};
		}
	while(++n<range && (int)distance(gp[n],g1)<range);
	return n;
}

static int project_path_aux(coord* gp, const coord g1, coord_delta s, int range, const coord g2, bool stop_dest, coord_action* stop_terrain)
{
	int i;
	int n = 0;
	const coord_scan ref_a(abs((int)(g2.x)-(int)(g1.x)),abs((int)(g2.y)-(int)(g1.y)));
	coord_scan a(ref_a);
	const int g2_offset = ((a.x > a.y) ? a.x : a.y)-1;	/* XXX would like std::max or related XXX */

	bool x_init = false;
	bool y_init = false;
	const bool x_large = (a.x > a.y);

	/* defaults */
	if (a.x >= a.y)
	{
		i = 0;
		gp[0].x = g1.x+s.x;
		while(++i<range)
		{
			gp[i].x = gp[i-1].x + s.x;
		}
		x_init = true;
	}
	if (a.y >= a.x)
	{
		i = 0;
		gp[0].y = g1.y+s.y;
		while(++i<range)
		{
			gp[i].y = gp[i-1].y + s.y;
		}		
		y_init = true;
	}
	if (0 == a.x)
	{
		i = 0;
		do	{
			gp[i].x = g1.x;
			}
		while(++i<range);
		x_init = true;
	}
	if (0 == a.y)
	{
		i = 0;
		do	{
			gp[i].y = g1.y;
			}
		while(++i<range);
		y_init = true;
	}
	project_path_aux_pathgen(gp, g1, range, a, s, x_init, y_init);

	/* g2 should be at the obvious location of the path */
	assert(g2==gp[g2_offset]);
	if (g2!=gp[g2_offset]) return 1;

	/* now have default path, determine whether it works */
	n = project_path_aux_pathtest(gp, g1, g2, range, stop_dest, stop_terrain);

	/* do not try to fix uncorrectable compass-direction paths */
	/* if we hit the target, return now */
	if (0==a.x || 0==a.y || a.x==a.y || g2_offset<n) return n;

	/* try to fix this */
	if (1 == a.y && 3 <= a.x)
	{	/* horizontal trick shot, fixable */
		while(g1.y!=gp[n-1].y)
		{
			a.x = 2*n+1;
			project_path_aux_pathgen(gp, g1, range, a, s, x_init, y_init);

			/* g2 should be at the obvious location of the path */
			assert(g2==gp[g2_offset]);
			if (g2!=gp[g2_offset]) return 1;

			n = project_path_aux_pathtest(gp, g1, g2, range, stop_dest, stop_terrain);
			if (g2_offset<n) return n;
		}
	}
	if (1 == a.x && 3 <= a.y)
	{	/* vertical trick shot, fixable */
		while(g1.x!=gp[n-1].x)
		{
			a.y = 2*n+1;
			project_path_aux_pathgen(gp, g1, range, a, s, x_init, y_init);

			/* g2 should be at the obvious location of the path */
			assert(g2==gp[g2_offset]);
			if (g2!=gp[g2_offset]) return 1;

			n = project_path_aux_pathtest(gp, g1, g2, range, stop_dest, stop_terrain);
			if (g2_offset<n) return n;
		}
	}
	if (a.x < a.y && 1 == a.y-a.x)
	{	/* diagonal trick shot, includes reversing knight-move */
		while(abs((int)gp[n-1].x-g1.x) != abs((int)gp[n-1].y-g1.y))
		{
			a.y = 2*n+1;
			a.x = a.y-1;
			project_path_aux_pathgen(gp, g1, range, a, s, x_init, y_init);

			/* g2 should be at the obvious location of the path */
			assert(g2==gp[g2_offset]);
			if (g2!=gp[g2_offset]) return 1;

			n = project_path_aux_pathtest(gp, g1, g2, range, stop_dest, stop_terrain);
			if (g2_offset<n) return n;
		}
	}
	if (a.y < a.x && 1 == a.x-a.y)
	{	/* diagonal trick shot #2,  includes reversing knight-move*/
		while(abs((int)gp[n-1].x-g1.x) != abs((int)gp[n-1].y-g1.y))
		{
			a.x = 2*n+1;
			a.y = a.x-1;
			project_path_aux_pathgen(gp, g1, range, a, s, x_init, y_init);

			/* g2 should be at the obvious location of the path */
			assert(g2==gp[g2_offset]);
			if (g2!=gp[g2_offset]) return 1;

			n = project_path_aux_pathtest(gp, g1, g2, range, stop_dest, stop_terrain);
			if (g2_offset<n) return n;
		}
	}

	assert(1<=g2_offset);

	{
	/* 
     * Adapt some ideas from Tyrecius' permissive field of view.
     */

	/*
     * We want ax, ay such that the number of side-steps taken at g2 is target.
     * tripwire expression is 2*(g2_offset+1)*(small)/(large)
     * C99 guarantees int supports at least 16 bits
     */

	/* Set up the incoming points */
	const coord_scan sorted_ref_a((x_large) ? ref_a.x : ref_a.y ,(x_large) ? ref_a.y : ref_a.x);
	coord_scan sorted_ref_a1(sorted_ref_a);
	coord_scan sorted_ref_a2(sorted_ref_a);
	coord g2_alt_1(g2);	/* favor basis */
	coord g2_alt_2(g2);	/* favor diagonal */
	rational<int> g2_tan_LB(2*sorted_ref_a.y-1,2*sorted_ref_a.x);	/* normally strict */
	rational<int> g2_tan_UB(2*sorted_ref_a.y+1,2*sorted_ref_a.x);	/* normally non-strict */
	const int max_safe_denominator = (32767/2)/sorted_ref_a.x;

	if (max_safe_denominator <= sorted_ref_a.x) return n;	/* failed */

	do	{
		/* step the bounds back */
		--sorted_ref_a1.x;
		--sorted_ref_a2.x;
		--sorted_ref_a2.y;

		g2_alt_2 -= s;
		if (x_large)
		{
			g2_alt_1.x -= s.x;		
		}
		else
		{
			g2_alt_1.y -= s.y;		
		};
	
		/* reality checks */
		/* tangents and terrain for g2_alt_1 */
		{
		rational<int> new_tan_LB(2*sorted_ref_a1.y-1,2*sorted_ref_a1.x);
		rational<int> new_tan_UB(2*sorted_ref_a1.y+1,2*sorted_ref_a1.x);

		while(new_tan_LB > g2_tan_UB || stop_terrain(g2_alt_1))
		{
			if (g2_alt_1==g2_alt_2) return n;	/* failed */

			/* converge */
			--sorted_ref_a1.y;
			if (x_large)	
			{
				g2_alt_1.y -= s.y;		
			}
			else
			{
				g2_alt_1.x -= s.x;		
			};		

			new_tan_LB.assign(2*sorted_ref_a1.y-1,2*sorted_ref_a1.x);
			new_tan_UB.assign(2*sorted_ref_a1.y+1,2*sorted_ref_a1.x);
		}

		if (new_tan_UB <= g2_tan_LB) return n;	/* failed */
		if (g2_tan_UB > new_tan_UB) g2_tan_UB = new_tan_UB;
		}

		/* tangents and terrain for g2_alt_2 */
		{
		rational<int> new_tan_LB(2*sorted_ref_a2.y-1,2*sorted_ref_a2.x);
		rational<int> new_tan_UB(2*sorted_ref_a2.y+1,2*sorted_ref_a2.x);

		while(new_tan_UB <= g2_tan_LB || stop_terrain(g2_alt_2))
		{
			if (g2_alt_1==g2_alt_2) return n;	/* failed */

			/* converge */
			++sorted_ref_a2.y;
			if (x_large)	
			{
				g2_alt_2.y += s.y;		
			}
			else
			{
				g2_alt_2.x += s.x;		
			};		

			new_tan_LB.assign(2*sorted_ref_a2.y-1,2*sorted_ref_a2.x);
			new_tan_UB.assign(2*sorted_ref_a2.y+1,2*sorted_ref_a2.x);
		}

		if (new_tan_LB >= g2_tan_UB) return n;	/* failed */
		if (g2_tan_LB < new_tan_LB) g2_tan_LB = new_tan_LB;
		}
		}
	while(1<sorted_ref_a2.x);

	/* now, find a suitable rational number with a safe denominator */
	if (max_safe_denominator >= g2_tan_UB.denominator())
	{	/* upper bound should work */
		if (x_large)
		{
			a.x = g2_tan_UB.denominator();
			a.y = g2_tan_UB.numerator();
		}
		else
		{
			a.x = g2_tan_UB.numerator();
			a.y = g2_tan_UB.denominator();
		}
		project_path_aux_pathgen(gp, g1, range, a, s, x_init, y_init);

		/* g2 should be at the obvious location of the path */
		assert(g2==gp[g2_offset]);
		if (g2!=gp[g2_offset]) return 1;

		n = project_path_aux_pathtest(gp, g1, g2, range, stop_dest, stop_terrain);
//		if (g2_offset<n) return n;
	}
	}

	return n;
}

/**
 * Find the path taken by a projection.
 *
 * \param[out] gp save path grids here.  Memory allocation is the caller's problem. 
 * \param range maximum range of path.  The distance function is a compile-time option.
 * \param g1 starting coordinate
 * \param g2 intend to pass through this coordinate
 * \param stop_dest indicates whether the projection should stop on reaching its destination.
 * \param stop_terrain is a non-NULL function pointer evaluating whether the terrain in the grid stops the projection.
 *
 * \return the number of grids in the path.  The path has zero length if and only if g1==g2.
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
	/* Offsets */
	coord_delta s(0,0);

	/* Analyze "dy" */
	if (g2.y < g1.y)
	{
		s.y = -1;
	}
	else if (g2.y > g1.y)
	{
		s.y = 1;
	}

	/* Analyze "dx" */
	if (g2.x < g1.x)
	{
		s.x = -1;
	}
	else if (g2.x > g1.x)
	{
		s.x = 1;
	}

	/* Length */
	return project_path_aux(gp,g1,s,range,g2,stop_dest,stop_terrain);
	} /* end blocking brace */
}

