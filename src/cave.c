/* File: cave.c */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/* Purpose: low level dungeon routines -BEN- */


#include "angband.h"


/*
 * Support for Adam Bolt's tileset, lighting and transparency effects
 * by Robert Ruehlmann (rr9@angband.org)
 */

/*
 * Maximum number of slopes in a single octant
 */
#define VINFO_MAX_SLOPES 135


/*
 * Table of data used to calculate projections / los / shots.
 */
static project_type *project_data[VINFO_MAX_SLOPES];

/* Number of squares per slope */
static int slope_count[VINFO_MAX_SLOPES];

/* The min and max slopes for each square in sight */
static int p_slope_min[MAX_SIGHT + 1][MAX_SIGHT + 1];
static int p_slope_max[MAX_SIGHT + 1][MAX_SIGHT + 1];


/*
 * Distance between two points via Newton-Raphson technique
 */
int distance(int x1, int y1, int x2, int y2)
{
	int dy = ABS(y2 - y1);
	int dx = ABS(x2 - x1);

	/* Simple case */
	if (!dy) return dx;
	if (!dx) return dy;

	/* Group */
	{
		/* Squared distance */
		int target = (dy * dy) + (dx * dx);

		/* Approximate distance: hypot(dy,dx) ~= max(dy,dx) + min(dy,dx) / 2 */
		int d = (dy > dx) ? (dy + dx / 2) : (dx + dy / 2);

		while (1)
		{
			/* Approximate error */
			int err = (target - d * d) / (2 * d);

			/* No error - we are done */
			if (!err) break;

			/* Adjust distance */
			d += err;
		}

		/* Return the distance */
		return d;
	}
}


/*
 * Return TRUE if the given square contains a trap
 */
bool is_trap(const cave_type *c_ptr)
{
	/* We assume field_is_type does not alter the data in c_ptr */
	return (*field_is_type((s16b *)&c_ptr->fld_idx, FTYPE_TRAP) != 0);
}


/*
 * Return TRUE if the given square contains a known trap
 */
bool is_visible_trap(const cave_type *c_ptr)
{
	/* We assume field_first_known does not alter the data in c_ptr */
	return (*field_first_known((s16b *)&c_ptr->fld_idx, FTYPE_TRAP) != 0);
}


/*
 * This is a version of the los function that uses a
 * tester function to determine whether or not to stop.
 *
 * Use this function instead of cut+pasting los() everywhere
 * with only tiny changes made to it.
 *
 * This works by following a "ray" that is one of those used
 * in the update_view() routine.
 *
 * We pick the minimal sloped ray that passes through the
 * required square.  We then follow that ray, looking at each
 * grid along it.  If the grid passes c_hook() then we keep
 * going.  If the grid does not, then we go to the minimal
 * slope that does not pass through this blocking grid.
 * We go to the first unchecked square along that ray - and
 * then continue following it.
 *
 * If the new ray does not pass through the target square, then
 * its slope will be greater than the maximal slope of the
 * target.
 *
 * This routine will over-check some squares in a worst-case
 * scenario - but it is fairly efficient.  Most of the required
 * information has been pre-calculated in the code that also
 * works out the data used by update_view()
 *
 * Unlike the old los() routine, this will give exactly the
 * same results as testing cave_view_grid() after using
 * update_view().
 */
static bool los_general(int x1, int y1, int x2, int y2, cave_hook_type c_hook)
{
	int i, j, temp, dist;

	int x, y;

	int dx, dy, ax, ay, sx, sy;

	cave_type *c_ptr;

	dist = distance(x1, y1, x2, y2);

	/* If (x1,y1) == (x2, y2) we know we can see ourselves */
	if (dist == 0) return (TRUE);

	/* We only work for points that are less than MAX_SIGHT appart. */
	if (dist > MAX_SIGHT) return (FALSE);

	/* Extract the offset */
	dy = y2 - y1;
	dx = x2 - x1;

	/* Extract the absolute offset */
	ay = ABS(dy);
	ax = ABS(dx);

	/*
	 * Start at the first square in the list.
	 * This is a square adjacent to (x1,y1)
	 */
	j = 0;

	/* Extract some signs */
	sx = (dx < 0) ? -1 : 1;
	sy = (dy < 0) ? -1 : 1;

	/* Hack - we need to stick to one octant */
	if (ay < ax)
	{
		/* Look up the slope to use */
		i = p_slope_min[ax][ay];

		while (i <= p_slope_max[ax][ay])
		{
			x = x1 + sx * project_data[i][j].x;
			y = y1 + sy * project_data[i][j].y;

			/* Done? */
			if ((x == x2) && (y == y2)) return (TRUE);

			/* Stop if out of bounds */
			if (!in_bounds(x, y)) return (FALSE);

			c_ptr = area(x, y);

			if ((*c_hook) (c_ptr))
			{
				/* Blocked: go to the best position we have not looked at yet */
				temp = project_data[i][j].slope;
				j = project_data[i][j].square;
				i = temp;
			}
			else
			{
				/* Advance along ray */
				j++;
			}
		}
	}
	else
	{
		/* Look up the slope to use */
		i = p_slope_min[ay][ax];

		while (i <= p_slope_max[ay][ax])
		{
			/* Note that the data offsets have x,y swapped */
			x = x1 + sx * project_data[i][j].y;
			y = y1 + sy * project_data[i][j].x;

			/* Done? */
			if ((x == x2) && (y == y2)) return (TRUE);

			/* Stop if out of bounds */
			if (!in_bounds(x, y)) return (FALSE);

			c_ptr = area(x, y);

			if ((*c_hook) (c_ptr))
			{
				/* Blocked: go to the best position we have not looked at yet */
				temp = project_data[i][j].slope;
				j = project_data[i][j].square;
				i = temp;
			}
			else
			{
				/* Advance along ray */
				j++;
			}
		}
	}

	/* No path */
	return (FALSE);
}

/*
 * Hack - a function to pass to los_general() used
 * to simulate the old los()
 */
static bool cave_stop_wall(const cave_type *c_ptr)
{
	/* Is it passable? */
	if (cave_los_grid(c_ptr)) return (FALSE);

	/* Seems ok */
	return (TRUE);
}

/*
 * Slow, but simple LOS routine.  This works in the same way as
 * the view code, so that if something is in view, los() behaves
 * as expected.
 *
 * The old routine was fast, but did not behave in the right way.
 * This new routine does not need to be fast, because it isn't
 * called in time-critical code.
 *
 *
 * It works by trying all slopes that connect (x1,y1) with (x2,y2)
 * If a wall is found, then it back-tracks to the 'best' square
 * to check next.  There may be cases where it checks the same
 * square multiple times, but a simple algorithm is much cleaner.
 *
 * Note that "line of sight" is not "reflexive" in all cases.
 *
 * Use the "projectable()" routine to test "spell/missile line of sight".
 *
 * Use the "update_view()" function to determine player line-of-sight.
 */
bool los(int x1, int y1, int x2, int y2)
{
	return (los_general(x1, y1, x2, y2, cave_stop_wall));
}




/* Slope and square used by mmove2 */
static int mmove_slope;
static int mmove_sq;

/* Direction to move in */
static int mmove_dx;
static int mmove_dy;


/*
 * This is a (slow) function that can be used
 * to test if there is a direct projection
 * between two squares.
 *
 * "Direct" projections tend to look much straighter
 * on the screen than normal ones.
 */
static bool is_direct_projectable(int x1, int y1)
{
	int xx, yy;

	int ax, ay, sx, sy;

	int slope = 0, sq = 0;

	cave_type *c_ptr;

	/* Extract the absolute offset */
	ay = ABS(mmove_dy);
	ax = ABS(mmove_dx);

	/* Extract some signs */
	sx = (mmove_dx < 0) ? -1 : 1;
	sy = (mmove_dy < 0) ? -1 : 1;


	/*
	 * Start at the first square in the list.
	 * This is a square adjacent to (x1,y1)
	 */

	/* Hack - we need to stick to one octant */
	if (ay < ax)
	{
		/* Look up the slope to use */
		slope = (p_slope_min[ax][ay] + p_slope_max[ax][ay]) / 2;

		while (TRUE)
		{
			xx = x1 + sx * project_data[slope][sq].x;
			yy = y1 + sy * project_data[slope][sq].y;

			/* Done? */
			if ((xx == x1 + mmove_dx) && (yy == y1 + mmove_dy)) return (TRUE);

			c_ptr = area(xx, yy);

			/* Is the square not occupied by a monster, and passable? */
			if (!cave_los_grid(c_ptr) || c_ptr->m_idx)
			{
				return (FALSE);
			}
			else
			{
				/* Advance along ray */
				sq++;
			}
		}
	}
	else
	{
		/* Look up the slope to use */
		slope = (p_slope_min[ay][ax] + p_slope_max[ay][ax]) / 2;

		while (TRUE)
		{
			/* Note that the data offsets have x,y swapped */
			xx = x1 + sx * project_data[slope][sq].y;
			yy = y1 + sy * project_data[slope][sq].x;

			/* Done? */
			if ((xx == x1 + mmove_dx) && (yy == y1 + mmove_dy)) return (TRUE);

			c_ptr = area(xx, yy);

			/* Is the square not occupied by a monster, and passable? */
			if (!cave_los_grid(c_ptr) || c_ptr->m_idx)
			{
				return (FALSE);
			}
			else
			{
				/* Advance along ray */
				sq++;
			}
		}
	}
}


/*
 * Calculate the slope and square information used by
 * a following mmove2
 */
void mmove_init(int x1, int y1, int x2, int y2)
{
	int temp;

	int xx, yy;

	int dx, dy, ax, ay, sx, sy, dist;

	cave_type *c_ptr;

	bool is_projectable;

	/* Clear slope and square */
	mmove_slope = 0;
	mmove_sq = 0;

	/* Clear direction */
	mmove_dx = 0;
	mmove_dy = 0;

	/* Paranoia - degenerate case */
	if ((x1 == x2) && (y1 == y2)) return;

	/* Extract the offset */
	dy = y2 - y1;
	dx = x2 - x1;

	/*
	 * We only work for points that are less than MAX_SIGHT appart.
	 * Note that MAX_RANGE < MAX_SIGHT
	 */
	dist = distance(x1, y1, x2, y2);

	if (dist > MAX_SIGHT)
	{
		/* Rescale */
		dx = (dx * MAX_SIGHT) / dist;
		dy = (dy * MAX_SIGHT) / dist;
	}

	/* Save direction */
	mmove_dx = dx;
	mmove_dy = dy;

	/* Extract the absolute offset */
	ay = ABS(dy);
	ax = ABS(dx);

	/* Extract some signs */
	sx = (dx < 0) ? -1 : 1;
	sy = (dy < 0) ? -1 : 1;


	/* Is the square projectable from here? */
	is_projectable = projectable(x1, y1, x2, y2);

	/*
	 * Start at the first square in the list.
	 * This is a square adjacent to (x1,y1)
	 */

	/* Hack - we need to stick to one octant */
	if (ay < ax)
	{
		/* Is there a direct line to the target? */
		if (is_direct_projectable(x1, y1))
		{
			/* Set the direct route */
			mmove_slope = (p_slope_min[ax][ay] + p_slope_max[ax][ay]) / 2;
			mmove_sq = 0;

			/* Done */
			return;
		}

		/* Look up the slope to use */
		mmove_slope = p_slope_min[ax][ay];

		while (mmove_slope <= p_slope_max[ax][ay])
		{
			xx = x1 + sx * project_data[mmove_slope][mmove_sq].x;
			yy = y1 + sy * project_data[mmove_slope][mmove_sq].y;

			/* Done? */
			if ((xx == x1 + dx) && (yy == y1 + dy)) break;

			c_ptr = area(xx, yy);

			/* Do we want to stop early? */
			if (!is_projectable && c_ptr->m_idx) break;

			/* Is the square not occupied by a monster, and passable? */
			if (!cave_los_grid(c_ptr) || c_ptr->m_idx)
			{
				/* Advance to the best position we have not looked at yet */
				temp = project_data[mmove_slope][mmove_sq].slope;
				mmove_sq = project_data[mmove_slope][mmove_sq].square;
				mmove_slope = temp;
			}
			else
			{
				/* Advance along ray */
				mmove_sq++;
			}
		}

		/* No match? */
		if (mmove_slope > p_slope_max[ax][ay])
		{
			mmove_slope = (p_slope_min[ax][ay] + p_slope_max[ax][ay]) / 2;
		}
	}
	else
	{
		/* Is there a direct line to the target? */
		if (is_direct_projectable(x1, y1))
		{
			/* Set the direct route */
			mmove_slope = (p_slope_min[ay][ax] + p_slope_max[ay][ax]) / 2;
			mmove_sq = 0;

			/* Done */
			return;
		}

		/* Look up the slope to use */
		mmove_slope = p_slope_min[ay][ax];

		while (mmove_slope <= p_slope_max[ay][ax])
		{
			/* Note that the data offsets have x,y swapped */
			xx = x1 + sx * project_data[mmove_slope][mmove_sq].y;
			yy = y1 + sy * project_data[mmove_slope][mmove_sq].x;

			/* Done? */
			if ((xx == x1 + dx) && (yy == y1 + dy)) break;

			c_ptr = area(xx, yy);

			/* Do we want to stop early? */
			if (!is_projectable && c_ptr->m_idx) break;

			/* Is the square not occupied by a monster, and passable? */
			if (!cave_los_grid(c_ptr) || c_ptr->m_idx)
			{
				/* Advance to the best position we have not looked at yet */
				temp = project_data[mmove_slope][mmove_sq].slope;
				mmove_sq = project_data[mmove_slope][mmove_sq].square;
				mmove_slope = temp;
			}
			else
			{
				/* Advance along ray */
				mmove_sq++;
			}
		}

		/* No match? */
		if (mmove_slope > p_slope_max[ay][ax])
		{
			mmove_slope = (p_slope_min[ay][ax] + p_slope_max[ay][ax]) / 2;
		}
	}


	/*
	 * Reset to start.
	 *
	 * Square zero is the the first square along the path.
	 * It is not the starting square
	 */
	mmove_sq = 0;
}


/*
 * Calculate incremental motion
 *
 * The current position is updated.
 *
 * (x,y) encodes the current location.
 *
 * This routine is very similar to los() except that we can use it
 * to return partial results.
 */
void mmove(int *x, int *y, int x1, int y1)
{
	int ax, ay, sx, sy;

	/* Extract the absolute offset */
	ay = ABS(mmove_dy);
	ax = ABS(mmove_dx);

	/* Extract some signs */
	sx = (mmove_dx < 0) ? -1 : 1;
	sy = (mmove_dy < 0) ? -1 : 1;

	/* Paranoia - square number is too large */
	if (mmove_sq >= slope_count[mmove_slope])
	{
		mmove_sq = slope_count[mmove_slope] - 1;
	}

	if (ay < ax)
	{
		/* Work out square to return */
		*x = x1 + sx * project_data[mmove_slope][mmove_sq].x;
		*y = y1 + sy * project_data[mmove_slope][mmove_sq].y;
	}
	else
	{
		/* Work out square to return */
		*x = x1 + sx * project_data[mmove_slope][mmove_sq].y;
		*y = y1 + sy * project_data[mmove_slope][mmove_sq].x;
	}

	/* Next square, next time. */
	mmove_sq++;
}


/* Does this square stop the projection? */
static bool project_stop(const cave_type *c_ptr, u16b flg)
{
	if (cave_los_grid(c_ptr))
	{
		/* Require fields do not block magic */
		if (fields_have_flags(c_ptr->fld_idx, FIELD_INFO_NO_MAGIC))
		{
			return (TRUE);
		}

		/* Is the square occupied by a monster? */
		if (c_ptr->m_idx != 0)
		{
			if (flg & (PROJECT_STOP))
			{
				/* Bolt spell is stopped by a monster */
				return (TRUE);
			}

			if ((flg & (PROJECT_FRND)) && is_pet(&m_list[c_ptr->m_idx]))
			{
				/* Try not to affect friendly monsters */
				return (TRUE);
			}
		}

		/* Seems ok */
		return (FALSE);
	}

	/* Blocked */
	return (TRUE);
}

/*
 * Hack - a function to pass to los_general() used
 * to do projectable().  Assume everything blocks projections.
 */
static bool cave_stop_project(const cave_type *c_ptr)
{
	/* Is it passable? */
	return (project_stop(c_ptr, 0));
}


/*
 * Determine if a bolt spell cast from (x1,y1) to (x2,y2) will arrive
 * at the final destination, assuming no monster gets in the way.
 *
 * This needs to be as fast as possible - so do not use the los_general()
 * function.
 *
 * XXX XXX Should we use a version of los(), but choose the average slope?
 *
 * XXX XXX Should we use los_general() anyway?
 *
 * XXX XXX Should there be two slightly different versions of this function
 *         a 'smart' one, and a 'dumb' but fast one?
 */
bool projectable(int x1, int y1, int x2, int y2)
{
	/* Are we projectable? */
	return (los_general(x1, y1, x2, y2, cave_stop_project));
}


/*
 * Determine the path taken by a projection.
 *
 * The path grids are saved into the grid array pointed to by "gp", and
 * there should be room for at least "range" grids in "gp".  Note that
 * due to the way in which distance is calculated, this function normally
 * uses fewer than "range" grids for the projection path, so the result
 * of this function should never be compared directly to "range".  Note
 * that the initial grid (x1,y1) is never saved into the grid array, not
 * even if the initial grid is also the final grid.  XXX XXX XXX
 *
 * The "flg" flags can be used to modify the behavior of this function.
 *
 * In particular, the "PROJECT_STOP" and "PROJECT_THRU" flags have the same
 * semantics as they do for the "project" function, namely, that the path
 * will stop as soon as it hits a monster, or that the path will continue
 * through the destination grid, respectively.
 *
 * This function returns the number of grids (if any) in the path.  This
 * function will return zero if and only if (x1,y1) and (x2,y2) are equal.
 *
 * This algorithm is the same as that used by "los_general()", however
 * the grids are saved along the path.
 *
 * This means that mmove2() will now follow exactly the same path as used
 * by los() and update_view().  You can only hit things you can see - and
 * everything you can see, you can hit.  The paths from this function
 * tend to look slightly "curved" - but that is a small price to pay for
 * simplicity.
 *
 * XXX XXX XXX You could make the paths straighter by using the median
 * ray connecting the two grids, and then going left and right to try and
 * find a free shot.  That would require more information to be saved
 * though.  It also probably would be slower than the current code.
 *
 * XXX XXX XXX This routine must be fairly fast - we use it in
 * projectable(), which is called alot in the AI code.
 */
sint project_path(coord *gp, int x1, int y1, int x2, int y2, u16b flg)
{
	int y, x, sx, sy, dx, dy;

	int sq, sl;

	/* Absolute */
	int ay, ax;

	int dist, temp;

	cave_type *c_ptr;

	/* No path necessary (or allowed) */
	if ((x1 == x2) && (y1 == y2)) return (0);

	/* Extract the offset */
	dy = y2 - y1;
	dx = x2 - x1;

	/*
	 * We only work for points that are less than MAX_SIGHT appart.
	 * Note that MAX_RANGE < MAX_SIGHT
	 */
	dist = distance(x1, y1, x2, y2);

	if (dist > MAX_SIGHT)
	{
		/* Rescale */
		dx = (dx * MAX_SIGHT) / dist;
		dy = (dy * MAX_SIGHT) / dist;
	}

	/* Extract the absolute offset */
	ay = ABS(dy);
	ax = ABS(dx);

	/* Extract some signs */
	sx = (dx < 0) ? -1 : 1;
	sy = (dy < 0) ? -1 : 1;


	/*
	 * Start at the first square in the list.
	 * This is a square adjacent to (x1,y1)
	 */
	sq = 0;

	/* Hack - we need to stick to one octant */
	if (ay < ax)
	{
		/* Look up the slope to use */
		sl = p_slope_min[ax][ay];

		while (sl <= p_slope_max[ax][ay])
		{
			x = x1 + sx * project_data[sl][sq].x;
			y = y1 + sy * project_data[sl][sq].y;

			/* Done? */
			if ((x == x1 + dx) && (y == y1 + dy)) break;

			/* Stop if out of bounds */
			if (!in_bounds(x, y)) break;

			c_ptr = area(x, y);

			if (project_stop(c_ptr, flg))
			{
				/* Advance to the best position we have not looked at yet */
				temp = project_data[sl][sq].slope;
				sq = project_data[sl][sq].square;
				sl = temp;
			}
			else
			{
				/* Advance along ray */
				sq++;
			}
		}

		/* No match? */
		if (sl > p_slope_max[ax][ay])
		{
			sl = (p_slope_min[ax][ay] + p_slope_max[ax][ay]) / 2;
		}
	}
	else
	{
		/* Look up the slope to use */
		sl = p_slope_min[ay][ax];

		while (sl <= p_slope_max[ay][ax])
		{
			/* Note that the data offsets have x,y swapped */
			x = x1 + sx * project_data[sl][sq].y;
			y = y1 + sy * project_data[sl][sq].x;

			/* Done? */
			if ((x == x1 + dx) && (y == y1 + dy)) break;

			/* Stop if out of bounds */
			if (!in_bounds(x, y)) break;

			c_ptr = area(x, y);

			if (project_stop(c_ptr, flg))
			{
				/* Advance to the best position we have not looked at yet */
				temp = project_data[sl][sq].slope;
				sq = project_data[sl][sq].square;
				sl = temp;
			}
			else
			{
				/* Advance along ray */
				sq++;
			}
		}

		/* No match? */
		if (sl > p_slope_max[ay][ax])
		{
			sl = (p_slope_min[ay][ax] + p_slope_max[ay][ax]) / 2;
		}
	}

	/* Scan over squares along path */
	for (sq = 0; (sq < MAX_RANGE) && (sq < slope_count[sl]); sq++)
	{
		if (ay < ax)
		{
			/* Work out square to use */
			x = x1 + sx * project_data[sl][sq].x;
			y = y1 + sy * project_data[sl][sq].y;
		}
		else
		{
			/* Work out square to use */
			x = x1 + sx * project_data[sl][sq].y;
			y = y1 + sy * project_data[sl][sq].x;
		}

		/* Stop if out of bounds */
		if (!in_bounds(x, y))
		{
			sq--;
			break;
		}

		/* Save the square */
		gp[sq].x = x;
		gp[sq].y = y;

		/* Sometimes stop at destination grid */
		if (!(flg & (PROJECT_THRU)))
		{
			if ((x == x2) && (y == y2)) break;
		}

		c_ptr = area(x, y);

		/* Does the grid stop projection? */
		if (project_stop(c_ptr, flg)) break;
	}

	/* Include the last square */
	sq++;

	/* Paranoia */
	if (sq > MAX_RANGE) sq = MAX_RANGE;
	if (sq >= slope_count[sl]) sq = slope_count[sl] - 1;

	/* Length */
	return (sq);
}


/* Will this square stop a ball spell? */
static bool cave_stop_ball(const cave_type *c_ptr)
{
	/* Walls block spells */
	if (!cave_los_grid(c_ptr)) return (TRUE);

	/* Fields can block magic */
	if (fields_have_flags(c_ptr->fld_idx, FIELD_INFO_NO_MAGIC)) return (TRUE);

	/* Seems ok */
	return (FALSE);
}

/*
 * Use Modified version of los() for calculation of balls.
 * Balls are stopped by walls, and by fields.
 */
bool in_ball_range(int x1, int y1, int x2, int y2)
{
	return (los_general(x1, y1, x2, y2, cave_stop_ball));
}


/*
 * Does the grid stop disintegration?
 */
static bool cave_stop_disintegration(const cave_type *c_ptr)
{
	/* Some terrain types block disintegration */
	if (cave_wall_grid(c_ptr) && cave_perma_grid(c_ptr))
	{
		return (TRUE);
	}

	/* Fields can block disintegration to */
	if (fields_have_flags(c_ptr->fld_idx, FIELD_INFO_PERM)) return (TRUE);

	/* Seems ok */
	return (FALSE);
}

/*
 * Use modified version of los() for calculation of disintegration balls.
 * Disintegration effects are stopped by permanent walls and fields.
 */
bool in_disintegration_range(int x1, int y1, int x2, int y2)
{
	return (los_general(x1, y1, x2, y2, cave_stop_disintegration));
}


/*
 * Standard "find me a location" function
 *
 * Obtains a legal location within the given distance of the initial
 * location, and with "los()" from the source to destination location.
 *
 * This function is often called from inside a loop which searches for
 * locations while increasing the "d" distance.
 */
void scatter(int *xp, int *yp, int x, int y, int d)
{
	int nx = 0, ny = 0;

	int c = 0;

	/* Pick a location */
	while (c++ < 1000)
	{
		/* Pick a new location */
		ny = rand_spread(y, d);
		nx = rand_spread(x, d);

		/* Ignore annoying locations */
		if (!in_bounds(nx, ny)) continue;

		/* Ignore excessively distant locations */
		if ((d > 1) && (distance(x, y, nx, ny) > d)) continue;

		/* Require line of sight */
		if (los(x, y, nx, ny)) break;
	}

	if (c > 999)
	{
		ny = y;
		nx = x;
	}

	/* Save the location */
	(*yp) = ny;
	(*xp) = nx;
}


/*
 * Can the player "see" the given grid in detail?
 *
 * He must have vision, illumination, and line of sight.
 *
 * Note -- "GRID_LITE" is only set if the "torch" has "los()".
 * So, given "GRID_LITE", we know that the grid is "fully visible".
 *
 * Note that "CAVE_GLOW" makes little sense for a wall, since it would mean
 * that a wall is visible from any direction.  That would be odd.  Except
 * under wizard light, which might make sense.  Thus, for walls, we require
 * not only that they be "CAVE_GLOW", but also, that they be adjacent to a
 * grid which is not only "CAVE_GLOW", but which is a non-wall, and which is
 * in line of sight of the player.
 *
 * This extra check is expensive, but it provides a more "correct" semantics.
 *
 * Note that "glowing walls" are only considered to be "illuminated" if the
 * grid which is next to the wall in the direction of the player is also a
 * "glowing" grid.  This prevents the player from being able to "see" the
 * walls of illuminated rooms from a corridor outside the room.
 */
bool player_can_see_bold(int x, int y)
{
	pcave_type *pc_ptr;

	/* Needs to be in bounds */
	if (!in_boundsp(x, y)) return (FALSE);

	/* Access the cave grid */
	pc_ptr = parea(x, y);

	/* Require line of sight to the grid + lit grid */
	return (player_can_see_grid(pc_ptr));
}


/*
 * Returns true if the player's grid is dark
 */
bool no_lite(void)
{
	return (!player_can_see_bold(p_ptr->px, p_ptr->py));
}


/*
 * Determine if a given location may be "destroyed"
 *
 * Used by destruction spells, and for placing stairs, etc.
 */
bool cave_valid_grid(const cave_type *c_ptr)
{
	object_type *o_ptr;

	/* Forbid perma-grids */
	if (cave_perma_grid(c_ptr)) return (FALSE);

	/* Check objects */
	OBJ_ITT_START (c_ptr->o_idx, o_ptr)
	{
		/* Forbid artifact grids */
		if (o_ptr->flags3 & TR3_INSTA_ART) return (FALSE);
	}
	OBJ_ITT_END;

	/* Accept */
	return (TRUE);
}




/*
 * Hack -- Legal monster codes
 */
static cptr image_monster_hack =
	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";


/*
 * Mega-Hack -- Hallucinatory monster
 */
static void image_monster(byte *ap, char *cp)
{
	int n = strlen(image_monster_hack);

	/* Random symbol from set above */
	if (use_graphics)
	{
		(*cp) = r_info[randint1(z_info->r_max - 1)].x_char;
		(*ap) = r_info[randint1(z_info->r_max - 1)].x_attr;
	}
	else
		/* Text mode */
	{
		(*cp) = (image_monster_hack[randint0(n)]);

		/* Random color */
		(*ap) = randint1(15);
	}
}



/*
 * Hack -- Legal object codes
 */
static cptr image_object_hack = "?/|\\\"!$()_-=[]{},~";


/*
 * Mega-Hack -- Hallucinatory object
 */
static void image_object(byte *ap, char *cp)
{
	int n = strlen(image_object_hack);

	if (use_graphics)
	{
		(*cp) = k_info[randint1(z_info->k_max - 1)].x_char;
		(*ap) = k_info[randint1(z_info->k_max - 1)].x_attr;
	}
	else
	{
		(*cp) = (image_object_hack[randint0(n)]);

		/* Random color */
		(*ap) = randint1(15);
	}
}



/*
 * Hack -- Random hallucination
 */
static void image_random(byte *ap, char *cp)
{
	/* Normally, assume monsters */
	if (randint0(100) < 75)
	{
		image_monster(ap, cp);
	}

	/* Otherwise, assume objects */
	else
	{
		image_object(ap, cp);
	}
}

/*
 * Table of the GF type for each breath
 */
static int breath_gf[32] =
{
	GF_NONE,	/* RF4_SHRIEK */
	GF_NONE,	/* RF4_ELDRITCH_HORROR */
	GF_NONE,	/* RF4_XXX3X4 */
	GF_NONE,	/* RF4_ROCKET */
	GF_NONE,	/* RF4_ARROW_1 */
	GF_NONE,	/* RF4_ARROW_2 */
	GF_NONE,	/* RF4_ARROW_3 */
	GF_NONE,	/* RF4_ARROW_4 */
	GF_ACID,	/* RF4_BR_ACID */
	GF_ELEC,	/* RF4_BR_ELEC */
	GF_FIRE,	/* RF4_BR_FIRE */
	GF_COLD,	/* RF4_BR_COLD */
	GF_POIS,	/* RF4_BR_POIS */
	GF_NETHER,	/* RF4_BR_NETH */
	GF_LITE,	/* RF4_BR_LITE */
	GF_DARK,	/* RF4_BR_DARK */
	GF_CONFUSION,	/* RF4_BR_CONF */
	GF_SOUND,	/* RF4_BR_SOUN */
	GF_CHAOS,	/* RF4_BR_CHAO */
	GF_DISENCHANT,	/* RF4_BR_DISE */
	GF_NEXUS,	/* RF4_BR_NEXU */
	GF_TIME,	/* RF4_BR_TIME */
	GF_INERTIA,	/* RF4_BR_INER */
	GF_GRAVITY,	/* RF4_BR_GRAV */
	GF_SHARDS,	/* RF4_BR_SHAR */
	GF_PLASMA,	/* RF4_BR_PLAS */
	GF_FORCE,	/* RF4_BR_WALL */
	GF_MANA,	/* RF4_BR_MANA */
	GF_NONE,	/* RF4_BA_NUKE */
	GF_NUKE,	/* RF4_BR_NUKE */
	GF_NONE,	/* RF4_BA_CHAO */
	GF_DISINTEGRATE	/* RF4_BR_DISI */
};


/*
 * Hack -- Get colour based on breaths of monster
 *
 * (This may be a little slow....
 */
static byte breath_attr(const monster_race *r_ptr)
{
	/* Mask out the breath flags */
	u32b flags = r_ptr->flags4 & RF4_BREATHS;
	u32b mask;

	/* See if we breathe anything at all */
	if (flags)
	{
		byte a;
		char c;

		cptr s;

		int i;
		int prob = 1;
		int choice = 0;

		/* Pick breath */
		for (i = 8, mask = 256; i < 32; i++, mask += mask)
		{
			if (flags & mask)
			{
				/* See if we choose this spell */
				if (one_in_(prob)) choice = i;

				/* Decrease probability of picking next 'spell' */
				prob++;
			}
		}

		/* Paranoia */
		if (choice)
		{
			/* Lookup the default colors for this type */
			s = gf_color[breath_gf[choice]];

			/* Oops */
			if (!s) return (TERM_WHITE);

			/* Pick a random color */
			c = s[randint0(strlen(s))];

			/* Lookup this color */
			a = strchr(color_char, c) - color_char;

			/*
			 * Invalid color (note check for < 0 removed, gave a silly
			 * warning because bytes are always >= 0 -- RG)
			 */
			if (a > 15) return (TERM_WHITE);

			/* Use this color */
			return (a);
		}
	}

	/* Just do any of 7 colours */
	switch (randint1(7))
	{
		case 1: return (TERM_RED);
		case 2: return (TERM_L_RED);
		case 3: return (TERM_WHITE);
		case 4: return (TERM_L_GREEN);
		case 5: return (TERM_BLUE);
		case 6: return (TERM_L_DARK);
		case 7: return (TERM_GREEN);
	}

	/* For the compilers... */
	return (TERM_WHITE);
}


/*
 * Two arrays listing the effects of "brightness"
 * and "darkness" on various "base" colours.
 *
 * This is used to do dynamic lighting effects in ascii :-)
 */

static const byte lighting_colours[16] =
{
	/* TERM_DARK */
	TERM_L_DARK,

	/* TERM_WHITE */
	TERM_YELLOW,

	/* TERM_SLATE */
	TERM_WHITE,

	/* TERM_ORANGE */
	TERM_YELLOW,

	/* TERM_RED */
	TERM_RED,

	/* TERM_GREEN */
	TERM_L_GREEN,

	/* TERM_BLUE */
	TERM_BLUE,

	/* TERM_UMBER */
	TERM_L_UMBER,

	/* TERM_L_DARK */
	TERM_SLATE,

	/* TERM_L_WHITE */
	TERM_WHITE,

	/* TERM_VIOLET */
	TERM_L_RED,

	/* TERM_YELLOW */
	TERM_YELLOW,

	/* TERM_L_RED */
	TERM_L_RED,

	/* TERM_L_GREEN */
	TERM_YELLOW,

	/* TERM_L_BLUE */
	TERM_L_BLUE,

	/* TERM_L_UMBER */
	TERM_L_UMBER,
};

static const byte darking_colours[16] =
{
	/* TERM_DARK */
	TERM_DARK,

	/* TERM_WHITE */
	TERM_SLATE,

	/* TERM_SLATE */
	TERM_L_DARK,

	/* TERM_ORANGE */
	TERM_UMBER,

	/* TERM_RED */
	TERM_RED,

	/* TERM_GREEN */
	TERM_GREEN,

	/* TERM_BLUE */
	TERM_BLUE,

	/* TERM_UMBER */
	TERM_RED,

	/* TERM_L_DARK */
	TERM_L_DARK,

	/* TERM_L_WHITE */
	TERM_SLATE,

	/* TERM_VIOLET */
	TERM_BLUE,

	/* TERM_YELLOW */
	TERM_ORANGE,

	/* TERM_L_RED */
	TERM_L_RED,

	/* TERM_L_GREEN */
	TERM_GREEN,

	/* TERM_L_BLUE */
	TERM_L_BLUE,

	/* TERM_L_UMBER */
	TERM_UMBER
};



#ifdef VARIABLE_PLAYER_GRAPH

/* Magic numbers */
#define BMP_FIRST_PC_CLASS		164
#define BMP_FIRST_PC_RACE		128

static void variable_player_graph(byte *a, char *c)
{
	if (use_graphics != GRAPHICS_ADAM_BOLT)
	{
		if (!streq(ANGBAND_SYS, "ibm"))
		{
			if (use_graphics)
			{
				*a = BMP_FIRST_PC_CLASS + p_ptr->pclass;
				*c = BMP_FIRST_PC_RACE + p_ptr->prace;
			}
		}
		else
		{
			if (use_graphics)
			{
				if (p_ptr->psex == SEX_FEMALE) *c = (char)242;
				switch (p_ptr->pclass)
				{
					case CLASS_PALADIN:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_WHITE;
						else
							*a = TERM_WHITE;
						*c = 253;
						break;
					}
					case CLASS_WARRIOR_MAGE:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_RED;
						else
							*a = TERM_VIOLET;
						break;
					}
					case CLASS_CHAOS_WARRIOR:
					{
						*a = randint1(14);
						break;
					}
					case CLASS_MAGE:
					case CLASS_HIGH_MAGE:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_RED;
						else
							*a = TERM_RED;
						*c = 248;
						break;
					}
					case CLASS_PRIEST:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_BLUE;
						else
							*a = TERM_BLUE;
						*c = 248;
						break;
					}
					case CLASS_RANGER:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_GREEN;
						else
							*a = TERM_GREEN;
						break;
					}
					case CLASS_ROGUE:
					{
						if (p_ptr->lev < 20)
							*a = TERM_SLATE;
						else
							*a = TERM_L_DARK;
						break;
					}
					case CLASS_WARRIOR:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_UMBER;
						else
							*a = TERM_UMBER;
						break;
					}
					case CLASS_MONK:
					case CLASS_MINDCRAFTER:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_UMBER;
						else
							*a = TERM_UMBER;
						*c = 248;
						break;
					}
					default:
					{
						/* Unknown */
						*a = TERM_WHITE;
					}
				}

				switch (p_ptr->prace)
				{
					case RACE_GNOME:
					case RACE_HOBBIT:
					{
						*c = 144;
						break;
					}
					case RACE_DWARF:
					{
						*c = 236;
						break;
					}
					case RACE_HALF_ORC:
					{
						*c = 243;
						break;
					}
					case RACE_HALF_TROLL:
					{
						*c = 184;
						break;
					}
					case RACE_ELF:
					case RACE_HALF_ELF:
					case RACE_HIGH_ELF:
					{
						*c = 223;
						break;
					}
					case RACE_HALF_OGRE:
					{
						*c = 168;
						break;
					}
					case RACE_HALF_GIANT:
					case RACE_HALF_TITAN:
					case RACE_CYCLOPS:
					{
						*c = 145;
						break;
					}
					case RACE_YEEK:
					{
						*c = 209;
						break;
					}
					case RACE_KLACKON:
					{
						*c = 229;
						break;
					}
					case RACE_KOBOLD:
					{
						*c = 204;
						break;
					}
					case RACE_NIBELUNG:
					{
						*c = 144;
						break;
					}
					case RACE_DARK_ELF:
					{
						*c = 223;
						break;
					}
					case RACE_DRACONIAN:
					{
						if (p_ptr->lev < 20)
							*c = 240;
						else if (p_ptr->lev < 40)
							*c = 22;
						else
							*c = 137;
						break;
					}
					case RACE_MIND_FLAYER:
					{
						*c = 236;
						break;
					}
					case RACE_IMP:
					{
						*c = 142;
						break;
					}
					case RACE_GOLEM:
					{
						*c = 6;
						break;
					}
					case RACE_SKELETON:
					{
						if (p_ptr->pclass == CLASS_MAGE ||
							p_ptr->pclass == CLASS_PRIEST ||
							p_ptr->pclass == CLASS_HIGH_MAGE ||
							p_ptr->pclass == CLASS_MONK ||
							p_ptr->pclass == CLASS_MINDCRAFTER)
							*c = 159;
						else
							*c = 181;
						break;
					}
					case RACE_ZOMBIE:
					case RACE_GHOUL:
					{
						*c = 221;
						break;
					}
					case RACE_VAMPIRE:
					{
						*c = 217;
						break;
					}
					case RACE_SPECTRE:
					{
						*c = 241;
						break;
					}
					case RACE_SPRITE:
					{
						*c = 244;
						break;
					}
					case RACE_BEASTMAN:
					{
						*c = 154;
						break;
					}
				}
			}
		}
	}
}
#endif /* VARIABLE_PLAYER_GRAPH */

/*
 * Extract the attr/char to display at the given (legal) map location
 *
 * Basically, we "paint" the chosen attr/char in several passes, starting
 * with any known "terrain features" (defaulting to darkness), then adding
 * any known "objects", and finally, adding any known "monsters".  This
 * is not the fastest method but since most of the calls to this function
 * are made for grids with no monsters or objects, it is fast enough.
 *
 * Note that the "zero" entry in the feature/object/monster arrays are
 * used to provide "special" attr/char codes, with "monster zero" being
 * used for the player attr/char, "object zero" being used for the "stack"
 * attr/char, and "feature zero" being used for the "nothing" attr/char,
 * though this function makes use of only "feature zero".
 *
 * Note that monsters can have some "special" flags, including "ATTR_MULTI",
 * which means their color changes, and "ATTR_CLEAR", which means they take
 * the color of whatever is under them, and "CHAR_CLEAR", which means that
 * they take the symbol of whatever is under them.  Technically, the flag
 * "CHAR_MIMIC" is supposed to indicate that a monster looks strange when
 * examined, but this flag is currently ignored.
 *
 * Note the effects of hallucination.  Objects always appear as random
 * "objects", monsters as random "monsters", and normal grids occasionally
 * appear as random "monsters" or "objects", but note that these random
 * "monsters" and "objects" are really just "colored ascii symbols".
 *
 * Note the use of the new "terrain feature" information.  Note that the
 * assumption that all interesting "objects" and "terrain features" are
 * memorized allows extremely optimized processing below.  Note the use
 * of separate flags on objects to mark them as memorized allows a grid
 * to have memorized "terrain" without granting knowledge of any object
 * which may appear in that grid.
 *
 * We use the players memorised information to pick the terrain feature
 * to use.  This allows massive simplification - getting rid of all the
 * checks to tha old CAVE_MARK flag, and allowing the player is blind case
 * to be merged into the main line code.  The section picking the terrain
 * attr/feat is now less than a page long - which is important because
 * this routine is a major bottleneck.
 *
 * Note the "special lighting effects" which can be activated for floor
 * grids using the "view_special_lite" option causing certain grids to be
 * displayed using special colors.
 *
 * Note the "special lighting effects" which can be activated for wall
 * grids using the "view_granite_lite" option causing certain grids to be
 * displayed using special colors.
 *
 * The lighting and darkening of colours is handled by two arrays.  We can
 * also lighten and darken some terrains in the 16x16 tileset.
 *
 * Note that bizarre things must be done when the "attr" and/or "char"
 * codes have the "high-bit" set, since these values are used to encode
 * various "special" pictures in some versions, and certain situations,
 * such as "multi-hued" or "clear" monsters, cause the attr/char codes
 * to be "scrambled" in various ways.
 */
static void map_info(const cave_type *c_ptr, const pcave_type *pc_ptr,
                     byte *ap, char *cp, byte *tap, char *tcp)
{
	feature_type *f_ptr;

	object_type *o_ptr;

	monster_type *m_ptr;

	field_type *fld_ptr;

	s16b this_f_idx, next_f_idx;

	byte feat;
	byte player;

	byte a;
	char c;

	byte feat_not_ascii;
	s16b halluc = p_ptr->image;

	/* Info flags */
	player = pc_ptr->player;

	/* Get the memorized feature */
	feat = pc_ptr->feat;

	/* Pointer to the feature */
	f_ptr = &f_info[feat];

	/* The feats attr */
	a = f_ptr->x_attr;

	/* The feats char */
	c = f_ptr->x_char;

	/*
	 * Look for lighting effects.
	 *
	 * Need to have lighting on and the player is not blind.
	 * We then need to have a grid that is allowed to be lit.
	 */
	if (view_bright_lite && !p_ptr->blind
		&& (!(f_ptr->flags & FF_BLOCK)
			|| (view_granite_lite && !view_torch_grids)))
	{
		/* It's not in view or no lighting effects? */
		if (((!(player & (GRID_VIEW))) && view_special_lite)
			|| !(player & (GRID_SEEN)))
		{
			/* If is ascii graphics */
			if (a < 16)
			{
				/* Use darkened colour */
				a = darking_colours[a];
			}
			else if ((use_graphics == GRAPHICS_ADAM_BOLT)
					 && (f_ptr->flags & FF_USE_TRANS))
			{
				/* Use a dark tile */
				c++;
			}
		}
		else if (((c_ptr->info & (CAVE_MNLT)) || (player & (GRID_LITE)))
				 && view_yellow_lite)
		{
			/* Use the torch effect */
			if (a < 16)
			{
				/* Use bright colour */
				a = lighting_colours[a];
			}
			else if ((use_graphics == GRAPHICS_ADAM_BOLT)
					 && (f_ptr->flags & FF_USE_TRANS))
			{
				/* Use a light tile */
				c += 2;
			}
		}
	}

	/* Hack -- rare random hallucination, except on outer dungeon walls */
	if (halluc && !p_ptr->blind && (feat != FEAT_PERM_SOLID) && one_in_(256))
	{
		/* Hallucinate */
		image_random(&a, &c);
	}

	/* Save the terrain info for the transparency effects */

	/* Does the feature have "extended terrain" information? */
	if (f_ptr->w_attr)
	{
		/*
		 * Store extended terrain information. 
		 * Note hack to get lighting right.
		 */
		(*tap) = f_ptr->w_attr + a - f_ptr->x_attr;
		(*tcp) = f_ptr->w_char + c - f_ptr->x_char;
	}
	else
	{
		(*tap) = a;
		(*tcp) = c;
	}

	/* Handle "player" */
	if (character_dungeon && (c_ptr == area(p_ptr->px, p_ptr->py)))
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		*ap = r_ptr->x_attr;

		/* Get the "player" char */
		*cp = r_ptr->x_char;
#ifdef VARIABLE_PLAYER_GRAPH

		variable_player_graph(ap, cp)
#endif /* VARIABLE_PLAYER_GRAPH */
			/* Done */
			return;
	}

	/* Handle monsters */
	if (c_ptr->m_idx)
	{
		m_ptr = &m_list[c_ptr->m_idx];

		/* Visible monster */
		if (m_ptr->ml)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			feat_not_ascii = (a & 0x80);

			/* Desired attr */
			if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR)) || feat_not_ascii)
			{
				a = r_ptr->x_attr;
			}

			/* Desired char */
			if (!(r_ptr->flags1 & (RF1_CHAR_CLEAR)) || feat_not_ascii)
			{
				c = r_ptr->x_char;
			}

			/* Ignore weird codes + graphics */
			if (a & 0x80)
			{
				/* Do nothing */
			}

			/* Multi-hued monster */
			else if (r_ptr->flags1 & (RF1_ATTR_MULTI))
			{
				/* Is it a shapechanger? */
				if (r_ptr->flags2 & (RF2_SHAPECHANGER))
				{
					if (use_graphics)
					{
						c = r_info[randint1(z_info->r_max - 1)].x_char;
						a = r_info[randint1(z_info->r_max - 1)].x_attr;
					}
					else
					{
						c = (one_in_(25) ?
							 image_object_hack[randint0
											   (strlen(image_object_hack))] :
							 image_monster_hack[randint0
												(strlen(image_monster_hack))]);
					}
				}

				/* Multi-hued attr */
				if (r_ptr->flags2 & RF2_ATTR_ANY)
					a = randint1(15);
				else
				{
					/* Pick colour based on breaths */
					a = breath_attr(r_ptr);
				}
			}
			/* Mimics' colors vary */
			else if (((c == '\"') || (c == '!') || (c == '='))
					 && !(r_ptr->flags1 & RF1_UNIQUE))
			{
				/* Use char */ ;

				/* Use semi-random attr */
				a = c_ptr->m_idx % 15 + 1;
			}

			/* Hack -- hallucination */
			if (halluc)
			{
				/* Hallucinatory monster */
				image_monster(&a, &c);
			}


			/* Hack -- fake monochrome */
			if (fake_monochrome)
			{
				if (p_ptr->invuln || !use_color || ironman_moria)
				{
					a = TERM_WHITE;
				}
				else if (p_ptr->wraith_form) a = TERM_L_DARK;
			}

			/* Save the info */
			(*ap) = a;
			(*cp) = c;

			/* Done */
			return;
		}
	}

	/* Fields */
	for (this_f_idx = c_ptr->fld_idx; this_f_idx; this_f_idx = next_f_idx)
	{
		/* Acquire field */
		fld_ptr = &fld_list[this_f_idx];

		/* Acquire next field */
		next_f_idx = fld_ptr->next_f_idx;

		/* Memorized, visible fields */
		if ((fld_ptr->info & (FIELD_INFO_MARK | FIELD_INFO_VIS)) ==
			(FIELD_INFO_MARK | FIELD_INFO_VIS))
		{
			/* Which display level to use? */
			if (fld_ptr->info & FIELD_INFO_FEAT)
			{
				/* Terrain level */
				if ((use_graphics == GRAPHICS_ADAM_BOLT)
					&& (fld_ptr->info & (FIELD_INFO_TRANS)))
				{
					/* Take into account dynamic lighting. */
					c += fld_ptr->f_char - f_ptr->x_char;
				}
				else
				{
					/* Normal char */
					c = fld_ptr->f_char;
				}

				/* Normal attr */
				a = fld_ptr->f_attr;

				/* Save the terrain info for the transparency effects */
				(*tap) = a;
				(*tcp) = c;
			}
			else
			{
				/* Hack - no monochrome effects.  Add them later? */

				/* Do we need to look at objects? */
				if (fld_ptr->info & (FIELD_INFO_IGNORE))
				{
					c = fld_ptr->f_char;
					a = fld_ptr->f_attr;
					break;
				}


				/* Above objects */

				/* Normal char */
				(*cp) = fld_ptr->f_char;

				/* Normal attr */
				(*ap) = fld_ptr->f_attr;

				/* Done */
				return;
			}

			/* Done */
			break;
		}
	}

	/* Objects */
	OBJ_ITT_START (c_ptr->o_idx, o_ptr)
	{
		/* Memorized objects */
		if (o_ptr->info & (OB_SEEN))
		{
			/* Normal char */
			c = object_char(o_ptr);

			/* Normal attr */
			a = object_attr(o_ptr);

			/* Hack -- hallucination */
			if (halluc) image_object(&a, &c);

			/* Done */
			break;
		}
	}
	OBJ_ITT_END;

	/* Hack -- fake monochrome */
	if (fake_monochrome)
	{
		if (p_ptr->invuln || !use_color || ironman_moria) a = TERM_WHITE;
		else if (p_ptr->wraith_form) a = TERM_L_DARK;
	}

	/* Save the info */
	(*ap) = a;
	(*cp) = c;
}



/*
 * Moves the cursor to a given MAP (x, y) location
 */
void move_cursor_relative(int col, int row)
{
	/* Real coordinates convert to screen positions */
	row -= panel_row_prt;
	col -= panel_col_prt;

	/* Go there */
	Term_gotoxy(col, row);
}



/*
 * Place an attr/char pair at the given map coordinate, if legal.
 */
void print_rel(char c, byte a, int x, int y)
{
	/* Only do "legal" locations */
	if (panel_contains(x, y))
	{
		/* Hack -- fake monochrome */
		if (fake_monochrome)
		{
			if (p_ptr->invuln || !use_color || ironman_moria) a = TERM_WHITE;
			else if (p_ptr->wraith_form) a = TERM_L_DARK;
		}

		/* Draw the char using the attr */
		Term_draw(x - panel_col_prt, y - panel_row_prt, a, c);
	}
}


/*
 * Memorize interesting viewable object/features in the given grid
 *
 * This function should only be called on "legal" grids.
 *
 * This function will memorize the object and/or feature in the given
 * grid, if they are (1) viewable and (2) interesting.  Note that all
 * objects are interesting, all terrain features except floors (and
 * invisible traps) are interesting, and floors (and invisible traps)
 * are interesting sometimes (depending on various options involving
 * the illumination of floor grids).
 *
 * Note that the memorization of objects is completely separate from
 * the memorization of terrain features, preventing annoying floor
 * memorization when a detected object is picked up from a dark floor,
 * and object memorization when an object is dropped into a floor grid
 * which is memorized but out-of-sight.
 *
 * This function should be called every time the "memorization" of
 * a grid (or the object in a grid) is called into question, such
 * as when an object is created in a grid, when a terrain feature
 * changes, and when any grid becomes "illuminated" or "viewable".
 *
 * Note the relatively efficient use of this function by the various
 * "update_view()" and "update_lite()" calls, to allow objects and
 * terrain features to be memorized (and drawn) whenever they become
 * viewable or illuminated in any way, but not when they "maintain"
 * or "lose" their previous viewability or illumination.
 *
 * Note the butchered "internal" version of the old "player_can_see_bold()"
 * that is used internal to this function.  (Using the GRID_SEEN
 * flag then allows us to quickly test visibility by the player.)
 */
void note_spot(int x, int y)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	object_type *o_ptr;

	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);

	s16b this_f_idx, next_f_idx = 0;

	/* Is it lit + in view + player is not blind? */
	if (((c_ptr->info & (CAVE_GLOW | CAVE_MNLT))
		 || (pc_ptr->player & (GRID_LITE)))
		&& player_has_los_grid(pc_ptr) && !p_ptr->blind)
	{
		/* Memorize certain non-torch-lit wall grids */
		if (!cave_floor_grid(c_ptr) && !(pc_ptr->player & (GRID_LITE)))
		{
			int yy, xx;

			/* Hack -- move one grid towards player */
			yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
			xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

			/* Check for "local" illumination */
			if (!(area(xx, yy)->info & (CAVE_GLOW)))
			{
				/* Hack - lite the spot any way */
				lite_spot(x, y);

				/* Done */
				return;
			}

			/* We can't see the square */
			pc_ptr->player &= ~(GRID_SEEN);
		}

		/* We can see the square */
		pc_ptr->player |= (GRID_SEEN);

		/* Memorize feature */
		remember_grid(c_ptr, pc_ptr);

		/* Hack -- memorize objects */
		OBJ_ITT_START (c_ptr->o_idx, o_ptr)
		{
			/* Memorize objects */
			o_ptr->info |= OB_SEEN;
		}
		OBJ_ITT_END;

		/* Hack -- memorize fields */
		for (this_f_idx = c_ptr->fld_idx; this_f_idx; this_f_idx = next_f_idx)
		{
			field_type *f_ptr = &fld_list[this_f_idx];

			/* Acquire next field */
			next_f_idx = f_ptr->next_f_idx;

			/* Memorize fields */
			f_ptr->info |= FIELD_INFO_MARK;
		}
	}

	/* Light the spot, now that we have noticed the changes. */
	lite_spot(x, y);
}


void display_dungeon(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	pcave_type *pc_ptr;
	cave_type *c_ptr;

	int x, y;
	byte a;
	char c;

	int wid = Term->wid / 2, hgt = Term->hgt / 2;

	byte ta;
	char tc;

	for (x = px - wid + 1; x <= px + wid; x++)
	{
		for (y = py - hgt + 1; y <= py + hgt; y++)
		{
			if (in_boundsp(x, y))
			{
				c_ptr = area(x, y);
				pc_ptr = parea(x, y);

				/* Tell the world about this square */
				Term_write_map(x, y, c_ptr, pc_ptr);

				/* Examine the grid */
				map_info(c_ptr, pc_ptr, &a, &c, &ta, &tc);

				/* Hack -- Queue it */
				Term_queue_char(x - px + wid - 1, y - py + hgt - 1, a, c, ta,
								tc);
			}
			else
			{
				/* Clear out-of-bound tiles */

				/* Access darkness */
				feature_type *f_ptr = &f_info[FEAT_NONE];

				/* Normal attr */
				a = f_ptr->x_attr;

				/* Normal char */
				c = f_ptr->x_char;

				/* Hack -- Queue it */
				Term_queue_char(x - px + wid - 1, y - py + hgt - 1, a, c, ta,
								tc);
			}
		}
	}
}


/*
 * Redraw (on the screen) a given MAP location
 *
 * This function should only be called on "legal" grids
 */
void lite_spot(int x, int y)
{
	cave_type *c_ptr;
	pcave_type *pc_ptr;

	/* Paranoia */
	if (!character_dungeon) return;

	if (in_boundsp(x, y))
	{
		/* Get location */
		c_ptr = area(x, y);
		pc_ptr = parea(x, y);

		/* Tell the world about this square */
		Term_write_map(x, y, c_ptr, pc_ptr);

		/* Redraw if on screen */
		if (panel_contains(x, y))
		{
			byte a;
			char c;

			byte ta;
			char tc;

			/* Examine the grid */
			map_info(c_ptr, pc_ptr, &a, &c, &ta, &tc);

			/* Hack -- Queue it */
			Term_queue_char(x - panel_col_prt, y - panel_row_prt, a, c, ta, tc);
		}
	}
}


/*
 * Prints the map of the dungeon
 *
 * Note that, for efficiency, we contain an "optimized" version
 * of both "lite_spot()" and "print_rel()", and that we use the
 * "lite_spot()" function to display the player grid, if needed.
 */
void prt_map(void)
{
	int x, y;
	int v, n;

	/* map bounds */
	s16b xmin, xmax, ymin, ymax;

	int wid, hgt;

	/* Temp variables to speed up deletion loops */
	s16b l1, l2, l3;

	cave_type *c_ptr;
	pcave_type *pc_ptr;

	byte *pa;
	char *pc;

	byte *pta;
	char *ptc;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Remove map offset */
	wid -= COL_MAP + 1;
	hgt -= ROW_MAP + 1;

	/* Access the cursor state */
	(void)Term_get_cursor(&v);

	/* Hide the cursor */
	(void)Term_set_cursor(0);


	/* Get bounds */
	xmin = (p_ptr->min_wid < panel_col_min) ? panel_col_min : p_ptr->min_wid;
	xmax = (p_ptr->max_wid - 1 > panel_col_max) ?
		panel_col_max : p_ptr->max_wid - 1;
	ymin = (p_ptr->min_hgt < panel_row_min) ? panel_row_min : p_ptr->min_hgt;
	ymax = (p_ptr->max_hgt - 1 > panel_row_max) ?
		panel_row_max : p_ptr->max_hgt - 1;

	/* Bottom section of screen */
	for (y = 1; y <= ymin - panel_row_prt; y++)
	{
		/* Erase the section */
		Term_erase(COL_MAP, y, wid);
	}

	/* Top section of screen */
	for (y = ymax - panel_row_prt; y <= hgt; y++)
	{
		/* Erase the section */
		Term_erase(COL_MAP, y, wid);
	}

	/* Sides of screen */
	/* Left side */
	l1 = xmin - panel_col_min;

	/* Right side */
	l2 = xmax - panel_col_prt;
	l3 = Term->wid - l2;

	for (y = ymin - panel_row_prt; y <= ymax - panel_row_prt; y++)
	{
		/* Erase the sections */
		Term_erase(COL_MAP, y, l1);
		Term_erase(l2, y, l3);
	}

	/* Pointers to current position in the string */
	pa = mp_a;
	pc = mp_c;

	pta = mp_ta;
	ptc = mp_tc;

	/* Dump the map */
	for (y = ymin; y <= ymax; y++)
	{
		/* No characters yet */
		n = 0;

		/* Scan the columns of row "y" */
		for (x = xmin; x <= xmax; x++)
		{
			c_ptr = area(x, y);

			pc_ptr = parea(x, y);

			/* Tell the world about this square */
			Term_write_map(x, y, c_ptr, pc_ptr);

			/* Determine what is there */
			map_info(c_ptr, pc_ptr, pa++, pc++, pta++, ptc++);
		}


		/* Point to start of line */
		pa = mp_a;
		pc = mp_c;
		pta = mp_ta;
		ptc = mp_tc;

		/* Efficiency -- Redraw that row of the map */
		Term_queue_line(xmin - panel_col_prt, y - panel_row_prt,
						xmax - xmin + 1, pa, pc, pta, ptc);
	}

	/* Restore the cursor */
	(void)Term_set_cursor(v);
}





/*
 * Hack -- priority array (see below)
 *
 * Note that all "walls" always look like "secret doors" (see "map_info()").
 *
 * This really needs to be done a better way.
 */
static const byte priority_table[][2] =
{
	/* Dark */
	{FEAT_NONE, 2},

	/* Floors */
	{FEAT_FLOOR, 5},

	/* Walls */
	{FEAT_SECRET, 10},
	{FEAT_WALL_EXTRA, 10},
	{FEAT_WALL_INNER, 10},
	{FEAT_WALL_OUTER, 10},
	{FEAT_WALL_SOLID, 10},


	/* Perm Walls */
	{FEAT_PERM_EXTRA, 10},
	{FEAT_PERM_INNER, 10},
	{FEAT_PERM_OUTER, 10},
	{FEAT_PERM_SOLID, 10},

	/* Quartz */
	{FEAT_QUARTZ, 11},

	/* Magma */
	{FEAT_MAGMA, 12},

	/* Rubble */
	{FEAT_RUBBLE, 13},

	/* Open doors */
	{FEAT_OPEN, 15},
	{FEAT_BROKEN, 15},

	/* Closed doors */
	{FEAT_CLOSED, 17},

	/* Hidden gold */
	{FEAT_QUARTZ_K, 19},
	{FEAT_MAGMA_K, 19},

	/* water, lava, & trees */
	{FEAT_DEEP_WATER, 20},
	{FEAT_SHAL_WATER, 20},
	{FEAT_DEEP_LAVA, 20},
	{FEAT_SHAL_LAVA, 20},
	{FEAT_DIRT, 6},
	{FEAT_GRASS, 6},
	{FEAT_TREES, 6},
	{FEAT_MOUNTAIN, 20},

	/* Stairs */
	{FEAT_LESS, 25},
	{FEAT_MORE, 25},

	/* End */
	{0, 0}
};


/*
 * Hack -- a priority function (see below)
 */
static byte priority(byte feat)
{
	int i = 0;

	/* Scan the table */
	while (priority_table[i][1])
	{
		/* Does the feature match? */
		if (priority_table[i][0] == feat)
		{
			return (priority_table[i][1]);
		}

		/* Next entry */
		i++;
	}

	/* Default  (The player /objects/fields?) */
	return (20);
}

/*
 * Equivalent function to map_info, but for displaying
 * the reduced-size dungeon map.
 *
 * We need to calculate priority as well as the symbols to display.
 */
static int display_map_info(int x, int y, char *c, byte *a, char *tc, byte *ta)
{
	int tp;

	byte feat;
	feature_type *f_ptr;

	object_type *o_ptr;
	field_type *fld_ptr;
	monster_type *m_ptr;

	s16b this_f_idx, next_f_idx;

	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);

	/* Get terrain feature */
	feat = pc_ptr->feat;
	f_ptr = &f_info[feat];

	/* Extract the priority of that attr/char */
	tp = priority(feat);

	*a = f_ptr->x_attr;
	*c = f_ptr->x_char;

	if (f_ptr->w_attr)
	{
		/*
		 * Store extended terrain information. 
		 */
		*ta = f_ptr->w_attr;
		*tc = f_ptr->w_char;
	}
	else
	{
		*ta = *a;
		*tc = *c;
	}

	/* Objects */
	OBJ_ITT_START (c_ptr->o_idx, o_ptr)
	{
		/* Memorized objects */
		if (o_ptr->info & (OB_SEEN))
		{
			/* Normal char */
			*c = object_char(o_ptr);

			/* Normal attr */
			*a = object_attr(o_ptr);

			/* Hack -- hallucination */
			if (p_ptr->image) image_object(a, c);

			/* High priority */
			tp = 30;

			/* Done */
			break;
		}
	}
	OBJ_ITT_END;

	/* Do we have a known field here? */
	for (this_f_idx = c_ptr->fld_idx; this_f_idx; this_f_idx = next_f_idx)
	{
		/* Acquire field */
		fld_ptr = &fld_list[this_f_idx];

		/* Acquire next field */
		next_f_idx = fld_ptr->next_f_idx;

		/* Memorized, visible fields */
		if ((fld_ptr->info & (FIELD_INFO_MARK | FIELD_INFO_VIS)) ==
			(FIELD_INFO_MARK | FIELD_INFO_VIS))
		{
			/* Which display level to use? */
			if (fld_ptr->info & FIELD_INFO_FEAT)
			{
				if (tp < 30)
				{
					*c = fld_ptr->f_char;
					*a = fld_ptr->f_attr;
				}

				/* Save the terrain info for the transparency effects */
				*ta = *a;
				*tc = *c;
			}
			else
			{
				/* Do we need to look at objects? */
				if (fld_ptr->info & (FIELD_INFO_IGNORE))
				{
					if (tp < 30)
					{
						*c = fld_ptr->f_char;
						*a = fld_ptr->f_attr;
					}
				}
				else
				{
					*c = fld_ptr->f_char;
					*a = fld_ptr->f_attr;
				}
			}

			/* Set high priority */
			tp = 30;
			break;
		}
	}

	/* Do we have a known monster here? */
	if (c_ptr->m_idx)
	{
		m_ptr = &m_list[c_ptr->m_idx];

		/* Visible monster */
		if (m_ptr->ml)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			byte feat_not_ascii = (*a & 0x80);

			/* Desired attr */
			if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR)) || feat_not_ascii)
			{
				*a = r_ptr->x_attr;
			}

			/* Desired char */
			if (!(r_ptr->flags1 & (RF1_CHAR_CLEAR)) || feat_not_ascii)
			{
				*c = r_ptr->x_char;
			}

			/* High priority */
			tp = 40;
		}
	}

	/* Finally - use the player */
	if ((x == p_ptr->px) && (y == p_ptr->py))
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		*a = r_ptr->x_attr;

		/* Get the "player" char */
		*c = r_ptr->x_char;
#ifdef VARIABLE_PLAYER_GRAPH

		variable_player_graph(a, c);
#endif /* VARIABLE_PLAYER_GRAPH */
		/* Highest priority */
		tp = 50;
	}

	/* Return priority */
	return (tp);
}



/*
 * Display a "small-scale" map of the dungeon in the active Term
 *
 * Note that the "map_info()" function must return fully colorized
 * data or this function will not work correctly.
 *
 * Note that this function must "disable" the special lighting
 * effects so that the "priority" function will work.
 *
 * Note the use of a specialized "priority" function to allow this
 * function to work with any graphic attr/char mappings, and the
 * attempts to optimize this function where possible.
 *
 * cx and cy are offsets from the position of the player.  This
 * allows the map to be shifted around - but only works in the
 * wilderness.  cx and cy return the position of the player on the
 * possibly shifted map.
 */
void display_map(int *cx, int *cy)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, j, x, y;

	byte feat;

	byte ta;
	char tc;

	byte tta;
	char ttc;

	byte tp;

	bool road;

	u16b w_type, w_info, twn;

	byte **ma;
	char **mc;

	byte **mp;

	byte **mta;
	char **mtc;

	bool old_view_special_lite = view_special_lite;
	bool old_view_granite_lite = view_granite_lite;

	int hgt, wid, yrat, xrat, xfactor, yfactor;

	/* Get size */
	Term_get_size(&wid, &hgt);
	hgt -= 2;
	wid -= 14;

	/* Paranoia */
	if ((hgt < 3) || (wid < 3))
	{
		/*
		 * Need to place the player...
		 * This is wrong, but the map is too small anyway.
		 */
		(*cy) = ROW_MAP;
		(*cx) = COL_MAP;
		return;
	}

	/* Disable lighting effects */
	view_special_lite = FALSE;
	view_granite_lite = FALSE;

	/* Allocate the maps */
	C_MAKE(ma, (hgt + 2), byte *);
	C_MAKE(mc, (hgt + 2), char *);
	C_MAKE(mp, (hgt + 2), byte *);

	C_MAKE(mta, (hgt + 2), byte *);
	C_MAKE(mtc, (hgt + 2), char *);

	/* Allocate and wipe each line map */
	for (i = 0; i < (hgt + 2); i++)
	{
		/* Allocate one row each array */
		C_MAKE(ma[i], (wid + 2), byte);
		C_MAKE(mc[i], (wid + 2), char);
		C_MAKE(mp[i], (wid + 2), byte);

		C_MAKE(mta[i], (wid + 2), byte);
		C_MAKE(mtc[i], (wid + 2), char);
	}

	/* Clear the chars and attributes */
	for (y = 0; y < hgt + 2; ++y)
	{
		for (x = 0; x < wid + 2; ++x)
		{
			/* Nothing here */
			ma[y][x] = TERM_WHITE;
			mc[y][x] = ' ';

			mta[y][x] = TERM_WHITE;
			mtc[y][x] = ' ';

			/* No priority */
			mp[y][x] = 0;
		}
	}

	if (!p_ptr->depth)
	{
		/* Plot wilderness */

		/* work out coords of player in wilderness */
		x = px / 16 + *cx;
		y = py / 16 + *cy;

		/* recenter */
		x = x - wid / 2;
		if (x + wid >= max_wild) x = max_wild - wid - 1;
		if (x < 0) x = 0;

		y = y - hgt / 2;
		if (y + hgt >= max_wild) y = max_wild - hgt - 1;
		if (y < 0) y = 0;

		/* Player location in wilderness */
		(*cy) += py / 16 - y + ROW_MAP;
		(*cx) += px / 16 - x + COL_MAP;

		/* Fill in the map */
		for (i = 0; i < wid; ++i)
		{
			for (j = 0; j < hgt; ++j)
			{
				/* Only draw blocks inside map */
				if (((x + i + 1) >= max_wild)
					|| ((y + j + 1) >= max_wild)) continue;

				/* Only draw blocks that have been seen */
				if (!(wild[j + y][i + x].done.info & WILD_INFO_SEEN)) continue;

				w_type = wild[j + y][i + x].done.wild;
				w_info = wild[j + y][i + x].done.info;

				if (w_type < WILD_SEA)
				{
					/* Normal terrain */
					feat = wild_gen_data[w_type].feat;

					/* Allow roads to be drawn */
					road = TRUE;
				}
				else
				{
					feat = FEAT_DEEP_WATER;

					/* No roads please */
					road = FALSE;
				}

				/* Add in effect of other specials */
				if (w_info & (WILD_INFO_WATER))
				{
					feat = FEAT_DEEP_WATER;
				}
				else if (w_info & (WILD_INFO_ACID))
				{
					feat = FEAT_DEEP_ACID;
				}
				else if (w_info & (WILD_INFO_LAVA))
				{
					feat = FEAT_DEEP_LAVA;
				}

				/* This is a nasty hack */

				/* Add in effects of roads */
				if ((w_info & (WILD_INFO_ROAD)) && road)
				{
					ma[j + 1][i + 1] = TERM_UMBER;
					mc[j + 1][i + 1] = '+';
					feat = FEAT_NONE;
				}
				else if ((w_info & (WILD_INFO_TRACK)) && road)
				{
					ma[j + 1][i + 1] = TERM_L_UMBER;
					mc[j + 1][i + 1] = '+';
					feat = FEAT_NONE;
				}

				/* Hack - draw places */
				/* Eventually will get attr,char from place data structure. */

				twn = wild[j + y][i + x].done.place;

				/* If there is a place... */
				if (twn)
				{
					/* Hack make a char /attr */
					if (place[twn].quest_num)
					{
						place_type *pl_ptr = &place[twn];

						wild_type *w_ptr = &wild[pl_ptr->y][pl_ptr->x];

						int depth = (w_ptr->done.mon_gen + 9) / 10;

						if (depth > 9) depth = 9;

						/* Quests are red */
						ma[j + 1][i + 1] = TERM_RED;
						mc[j + 1][i + 1] = '0' + depth;
						feat = FEAT_NONE;
					}
					else
					{
						/* Towns are white */
						ma[j + 1][i + 1] = TERM_WHITE;
						mc[j + 1][i + 1] = place[twn].name[0];
						feat = FEAT_NONE;
					}
				}

				/* Finally show position of player */
				if ((i + x == px / 16) && (j + y == py / 16))
				{
					ma[j + 1][i + 1] = TERM_WHITE;
					mc[j + 1][i + 1] = '@';
					feat = FEAT_NONE;
				}

				if (feat)
				{
					/* Get attr / char pair for wilderness block type */
					ma[j + 1][i + 1] = f_info[feat].x_attr;
					mc[j + 1][i + 1] = f_info[feat].x_char;

					if (f_info[feat].w_attr)
					{
						mta[j + 1][i + 1] = f_info[feat].w_attr;
						mtc[j + 1][i + 1] = f_info[feat].w_char;
					}
					else
					{
						mta[j + 1][i + 1] = ma[j + 1][i + 1];
						mtc[j + 1][i + 1] = mc[j + 1][i + 1];
					}
				}
			}
		}
	}
	else
	{
		yrat = p_ptr->max_hgt - p_ptr->min_hgt;
		xrat = p_ptr->max_wid - p_ptr->min_wid;

		/* Get scaling factors */
		yfactor = ((yrat / hgt < 4) && (yrat > hgt)) ? 10 : 1;
		xfactor = ((xrat / wid < 4) && (xrat > wid)) ? 10 : 1;

		yrat = (yrat * yfactor + hgt - 1) / hgt;
		xrat = (xrat * xfactor + wid - 1) / wid;

		/* Player location in dungeon */
		(*cy) = py * yfactor / yrat + ROW_MAP;
		(*cx) = px * xfactor / xrat + COL_MAP;

		/* Fill in the map of dungeon */
		for (i = p_ptr->min_wid; i < p_ptr->max_wid; ++i)
		{
			for (j = p_ptr->min_hgt; j < p_ptr->max_hgt; ++j)
			{
				/* Location */
				x = i * xfactor / xrat + 1;
				y = j * yfactor / yrat + 1;

				/* Get priority and symbol */
				tp = display_map_info(i, j, &tc, &ta, &ttc, &tta);

				/* Save "best" */
				if (mp[y][x] < tp)
				{
					/* Save the char */
					mc[y][x] = tc;

					/* Save the attr */
					ma[y][x] = ta;

					/* Save the transparency graphic */
					mtc[y][x] = ttc;
					mta[y][x] = tta;

					/* Save priority */
					mp[y][x] = tp;
				}
			}
		}
	}

	/* Corners */
	i = wid + 1;
	j = hgt + 1;

	/* Draw the corners */
	mc[0][0] = '+';
	mc[0][i] = '+';
	mc[j][0] = '+';
	mc[j][i] = '+';

	/* Draw the horizontal edges */
	for (i = 1; i <= wid; i++)
	{
		mc[0][i] = '-';
		mc[j][i] = '-';
	}

	/* Draw the vertical edges */
	for (j = 1; j <= hgt; j++)
	{
		mc[j][0] = '|';
		mc[j][i] = '|';
	}

	/* Display each map line in order */
	for (j = 0; j < hgt + 2; ++j)
	{
		/* Display the line */
		for (i = 0; i < wid + 2; ++i)
		{
			ta = ma[j][i];
			tc = mc[j][i];

			tta = mta[j][i];
			ttc = mtc[j][i];

			/* Hack -- Queue it */
			Term_queue_char(COL_MAP + i - 1, j, ta, tc, tta, ttc);
		}
	}

	/* Restore lighting effects */
	view_special_lite = old_view_special_lite;
	view_granite_lite = old_view_granite_lite;

	/* Free each line map */
	for (i = 0; i < (hgt + 2); i++)
	{
		/* Free one row each array */
		FREE(ma[i]);
		FREE(mc[i]);
		FREE(mta[i]);
		FREE(mtc[i]);
		FREE(mp[i]);
	}

	/* Free the maps */
	FREE(ma);
	FREE(mc);
	FREE(mta);
	FREE(mtc);
	FREE(mp);
}


/*
 * Display a "small-scale" map of the dungeon for the player
 *
 * Currently, the "player" is displayed on the map.  XXX XXX XXX
 */
void do_cmd_view_map(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int cy, cx;
	int wid, hgt;

	/* No overhead map in vanilla town mode. */
	if (!p_ptr->depth && vanilla_town) return;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Save the screen */
	screen_save();

	/* Note */
	prt("Please wait...", 0, 0);

	/* Flush */
	Term_fresh();

	/* Clear the screen */
	Term_clear();

	if (p_ptr->depth)
	{
		/* In the dungeon - All we have to do is display the map */

		/* No offset from player */
		cx = 0;
		cy = 0;

		/* Display the map */
		display_map(&cx, &cy);

		/* Wait for it */
		put_str("Hit any key to continue", (wid - COL_MAP) / 2, hgt - 1);

		/* Hilite the player */
		Term_gotoxy(cx, cy);

		/* Get any key */
		(void)inkey();
	}
	else
	{
		/* Offset from player */
		int x, y;

		/* Direction */
		int d;

		/* No offset yet */
		x = 0;
		y = 0;

		/* In the wilderness - Display the map + move it around */

		while (TRUE)
		{
			/* Reset offset of map */
			cx = x;
			cy = y;

			display_map(&cx, &cy);

			/* Wait for it */
			put_str("Move around, or hit any other key to continue.",
					COL_MAP - 23 + (wid - COL_MAP) / 2, hgt - 1);

			/* Hilite the player */
			Term_gotoxy(cx, cy);

			/* Draw it */
			Term_fresh();

			/* Get a response */
			d = get_keymap_dir(inkey());

			/* Done if not a direction */
			if (!d) break;

			x += ddx[d];
			y += ddy[d];

			/* Bounds checking */
			if (x + px / WILD_BLOCK_SIZE < 0)
			{
				x = -px / WILD_BLOCK_SIZE;
			}
			if (y + py / WILD_BLOCK_SIZE < 0)
			{
				y = -py / WILD_BLOCK_SIZE;
			}
			if (x + px / WILD_BLOCK_SIZE > max_wild - 2)
			{
				x = max_wild - px / WILD_BLOCK_SIZE - 2;
			}
			if (y + py / WILD_BLOCK_SIZE > max_wild - 2)
			{
				y = max_wild - py / WILD_BLOCK_SIZE - 2;
			}
		}
	}

	/* Restore the screen */
	screen_load();
}





/*
 * Some comments on the cave grid flags.  -BEN-
 *
 *
 * One of the major bottlenecks in previous versions of Angband was in
 * the calculation of "line of sight" from the player to various grids,
 * such as monsters.  This was such a nasty bottleneck that a lot of
 * silly things were done to reduce the dependancy on "line of sight",
 * for example, you could not "see" any grids in a lit room until you
 * actually entered the room, and there were all kinds of bizarre grid
 * flags to enable this behavior.  This is also why the "call light"
 * spells always lit an entire room.
 *
 * The code below provides functions to calculate the "field of view"
 * for the player, which, once calculated, provides extremely fast
 * calculation of "line of sight from the player", and to calculate
 * the "field of torch lite", which, again, once calculated, provides
 * extremely fast calculation of "which grids are lit by the player's
 * lite source".  In addition to marking grids as "GRID_VIEW" and/or
 * "GRID_LITE", as appropriate, these functions maintain an array
 * containing the locations of all of the grids marked with the
 * GRID_VIEW flag, which can be used to very quickly scan through all
 * of those grids.
 *
 * To allow more "semantically valid" field of view semantics, whenever
 * the field of view (or the set of torch lit grids) changes, all of the
 * grids in the field of view (or the set of torch lit grids) are "drawn"
 * so that changes in the world will become apparent as soon as possible.
 * This has been optimized so that only grids which actually "change" are
 * redrawn, using the "temp" array and the "CAVE_TEMP" flag to keep track
 * of the grids which are entering or leaving the relevent set of grids.
 *
 * These new methods are so efficient that the old nasty code was removed.
 *
 * Note that there is no reason to "update" the "viewable space" unless
 * the player "moves", or walls/doors are created/destroyed, and there
 * is no reason to "update" the "torch lit grids" unless the field of
 * view changes, or the "light radius" changes, or HACK: if the player
 * becomes blind.  This means that when the player is resting, or digging,
 * or doing anything that does not involve movement or changing the state
 * of the dungeon, there is no need to update the "view" or the "lite"
 * regions, which is nice.
 *
 * Note that the calls to the nasty "los()" function have been removed with
 * regards to the view/lite code.  This results in a huge speed increase.
 *
 *
 * Note that every "GRID_LITE" grid is also a "GRID_VIEW" grid, and in
 * fact, the player can always "see" all grids which are marked as
 * "GRID_LITE", unless they are "off screen".
 *
 * Every lit grid that is "GRID_VIEW" and lit in some way is "GRID_SEEN"
 * allowing a fast way to tell if the player can see a grid or not.
 *
 * The "update_view()" function maintains the "GRID_VIEW" flag for each
 * grid and maintains an array of all "GRID_VIEW" grids.  It also looks
 * after the "GRID_LITE" flag, and the memorization of new map sqaures.
 * The "GRID_SEEN" status is also affected - however, the monster lighting
 * routine also affects this flag.
 *
 *
 * The current "update_view()" algorithm uses the "CAVE_XTRA" flag as a
 * temporary internal flag to mark those grids which have been previously
 * memorized.  This is to prevent blind players from gaining information
 * about their surroundings.  This flag is always cleared when we are done.
 *
 *
 * The current "update_view()" algorithm uses the "CAVE_TEMP" flag, and the
 * array of grids which are marked as "CAVE_TEMP", to keep track of which
 * grids were previously marked as "GRID_VIEW", which allows us to optimize
 * the "screen updates".  We only draw the squares that change on the screen.
 *
 * The "CAVE_TEMP" flag, and the array of "CAVE_TEMP" grids, is also used
 * for various other purposes, such as spreading lite or darkness during
 * "lite_room()" / "unlite_room()", for calculating monster flow, and filling
 * the fractal caves.
 *
 *
 * Any grid can be marked as "CAVE_GLOW" which means that the grid itself is
 * in some way permanently lit.  However, for the player to "see" anything
 * in the grid, as determined by "player_can_see()", the player must not be
 * blind, the grid must be marked as "GRID_VIEW", and, in addition, "wall"
 * grids, even if marked as "perma lit", are only illuminated if they touch
 * a grid which is not a wall and is marked both "CAVE_GLOW" and "GRID_VIEW".
 *
 *
 * The old "CAVE_MARK" flag has been removed - replaced by the terrain type
 * of the memorised grid, in the player information: pcave_type.  This is
 * FEAT_NONE when the player doesn't know anything about the square.
 *
 * Objects are "memorized" in a different way, using a special "OB_SEEN" flag
 * on the object itself, which is set when an object is observed or detected.
 *
 *
 * A grid may be marked as "CAVE_ROOM" which means that it is part of a "room".
 * This is used only in dungeon generation.  Perhaps this flag can be used in
 * other code if required.  This was an alias for "CAVE_MNLT" which is set if
 * the square is lit by a monsters light source.
 *
 *
 * A grid may be marked as "CAVE_ICKY" which means it is part of a "vault",
 * and should be unavailable for "teleportation" destinations.
 *
 *
 * The "view_perma_grids" allows the player to "memorize" every perma-lit grid
 * which is observed, and the "view_torch_grids" allows the player to memorize
 * every torch-lit grid.  The player will always memorize important walls,
 * doors, stairs, and other terrain features, as well as any "detected" grids.
 *
 *
 * Note that the new "update_view()" method allows, among other things, a room
 * to be "partially" seen as the player approaches it, with a growing cone of
 * floor appearing as the player gets closer to the door.
 *
 * And my favorite "plus" is that you can now use a special option to draw the
 * "floors" in the "viewable region" brightly (actually, to draw the *other*
 * grids dimly), providing a "pretty" effect as the player runs around, and
 * to efficiently display the "torch lite" in a special color.
 *
 * Here are some pictures of the legal "light source" radius values, in
 * which the numbers indicate the "order" in which the grids could have
 * been calculated, if desired.  Larger radii are possible...
 *
 *
 *       Rad=0     Rad=1      Rad=2        Rad=3
 *      No-Lite  Torch,etc   Lantern     Artifacts
 *
 *                                          333
 *                             333         43334
 *                  212       32123       3321233
 *         @        1@1       31@13       331@133
 *                  212       32123       3321233
 *                             333         43334
 *                                          333
 *
 *
 * Here is an illustration of the two different "update_view()" algorithms,
 * in which the grids marked "%" are pillars, and the grids marked "?" are
 * not in line of sight of the player.
 *
 *
 *                    Sample situation
 *
 *                  #####################
 *                  ############.%.%.%.%#
 *                  #...@..#####........#
 *                  #............%.%.%.%#
 *                  #......#####........#
 *                  ############........#
 *                  #####################
 *
 *
 *          New Algorithm             Old Algorithm
 *
 *      ########?????????????    ########?????????????
 *      #...@..#?????????????    #...@..#?????????????
 *      #...........?????????    #.........???????????
 *      #......#####.....????    #......####??????????
 *      ########?????????...#    ########?????????????
 *
 *      ########?????????????    ########?????????????
 *      #.@....#?????????????    #.@....#?????????????
 *      #............%???????    #...........?????????
 *      #......#####........?    #......#####?????????
 *      ########??????????..#    ########?????????????
 *
 *      ########?????????????    ########?????%???????
 *      #......#####........#    #......#####..???????
 *      #.@..........%???????    #.@..........%???????
 *      #......#####........#    #......#####..???????
 *      ########?????????????    ########?????????????
 *
 *      ########??????????..#    ########?????????????
 *      #......#####........?    #......#####?????????
 *      #............%???????    #...........?????????
 *      #.@....#?????????????    #.@....#?????????????
 *      ########?????????????    ########?????????????
 *
 *      ########?????????%???    ########?????????????
 *      #......#####.....????    #......####??????????
 *      #...........?????????    #.........???????????
 *      #...@..#?????????????    #...@..#?????????????
 *      ########?????????????    ########?????????????
 */




/*
 * Clear the viewable space
 */
void forget_view(void)
{
	int i, x, y;

	pcave_type *pc_ptr;

	/* None to forget */
	if (!view_n) return;

	/* Clear them all */
	for (i = 0; i < view_n; i++)
	{
		y = view_y[i];
		x = view_x[i];

		/* Access the grid */
		pc_ptr = parea(x, y);

		/* Forget that the grid is viewable or lit */
		pc_ptr->player &= ~(GRID_LITE | GRID_VIEW | GRID_SEEN);

		/* Only lite the spot if is on the panel (can change due to resizing */
		if (!panel_contains(x, y)) continue;

		/* Update the screen */
		lite_spot(x, y);
	}

	/* None left */
	view_n = 0;
}


/*
 * Maximum number of grids in a single octant
 */
#define VINFO_MAX_GRIDS 175


/*
 * Mask of bits used in a single octant
 */
#define VINFO_BITS_4 0x0000007FL
#define VINFO_BITS_3 0xFFFFFFFFL
#define VINFO_BITS_2 0xFFFFFFFFL
#define VINFO_BITS_1 0xFFFFFFFFL
#define VINFO_BITS_0 0xFFFFFFFFL


/*
 * Forward declare
 */
typedef struct vinfo_type vinfo_type;


/*
 * The 'vinfo_type' structure
 */
struct vinfo_type
{
	s16b grid_x[8];
	s16b grid_y[8];

	u32b bits[5];

	vinfo_type *next_0;
	vinfo_type *next_1;

	byte y;
	byte x;
	byte d;
	byte r;
};



/*
 * The array of "vinfo" objects, initialized by "vinfo_init()"
 */
static vinfo_type vinfo[VINFO_MAX_GRIDS];




/*
 * Slope scale factor
 */
#define SCALE 100000L


/*
 * Forward declare
 */
typedef struct vinfo_hack vinfo_hack;


/*
 * Temporary data used by "vinfo_init()"
 *
 *	- Number of slopes
 *
 *	- Slope values
 *
 *  - Slope min and max for each square
 */
struct vinfo_hack
{
	int num_slopes;

	s32b slopes[VINFO_MAX_SLOPES];

	s32b slopes_min[MAX_SIGHT + 1][MAX_SIGHT + 1];
	s32b slopes_max[MAX_SIGHT + 1][MAX_SIGHT + 1];
};



/*
 * Sorting hook -- comp function -- array of s32b (see below)
 *
 * We use "u" to point to an array of s32b.
 */
static bool ang_sort_comp_hook_s32b(const vptr u, const vptr v, int a, int b)
{
	s32b *x = (s32b *)(u);

	/* Hack - ignore v */
	(void)v;

	return (x[a] <= x[b]);
}


/*
 * Sorting hook -- swap function -- array of s32b (see below)
 *
 * We use "u" to point to an array of s32b.
 */
static void ang_sort_swap_hook_s32b(const vptr u, const vptr v, int a, int b)
{
	s32b *x = (s32b *)(u);

	s32b temp;

	/* Hack - ignore v */
	(void)v;

	/* Swap */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;
}



/*
 * Save a slope
 */
static void vinfo_init_aux(vinfo_hack *hack, int x, int y, s32b m)
{
	int i;

	/* Handle "legal" slopes */
	if ((m > 0) && (m <= SCALE))
	{
		/* Look for that slope */
		for (i = 0; i < hack->num_slopes; i++)
		{
			if (hack->slopes[i] == m) break;
		}

		/* New slope */
		if (i == hack->num_slopes)
		{
			/* Paranoia */
			if (hack->num_slopes >= VINFO_MAX_SLOPES)
			{
				quit_fmt("Too many slopes (%d)!", VINFO_MAX_SLOPES);
			}

			/* Save the slope, and advance */
			hack->slopes[hack->num_slopes++] = m;
		}
	}

	/* Track slope range */
	if (hack->slopes_min[y][x] > m) hack->slopes_min[y][x] = m;
	if (hack->slopes_max[y][x] < m) hack->slopes_max[y][x] = m;
}


/*
 * Initialize the "vinfo" and "project_data" arrays
 *
 * Full Octagon (radius 20), Grids=1149
 *
 * Quadrant (south east), Grids=308, Slopes=251
 *
 * Octant (east then south), Grids=161, Slopes=126
 *
 * This function assumes that VINFO_MAX_GRIDS and VINFO_MAX_SLOPES
 * have the correct values, which can be derived by setting them to
 * a number which is too high, running this function, and using the
 * error messages to obtain the correct values.
 */
errr vinfo_init(void)
{
	int i, j;
	int y, x;

	s32b m;

	vinfo_hack *hack;

	int num_grids = 0;

	int queue_head = 0;
	int queue_tail = 0;
	vinfo_type *queue[VINFO_MAX_GRIDS * 2];


	/* Make hack */
	MAKE(hack, vinfo_hack);


	/* Analyze grids */
	for (y = 0; y <= MAX_SIGHT; ++y)
	{
		for (x = y; x <= MAX_SIGHT; ++x)
		{
			/* Skip grids which are out of sight range */
			if (distance(0, 0, x, y) > MAX_SIGHT) continue;

			/* Default slope range */
			hack->slopes_min[y][x] = 999999999;
			hack->slopes_max[y][x] = 0;

			/* Clear the p_slope_min and max arrays */
			p_slope_min[x][y] = VINFO_MAX_SLOPES;
			p_slope_max[x][y] = 0;
			p_slope_min[y][x] = VINFO_MAX_SLOPES;
			p_slope_max[y][x] = 0;

			/* Paranoia */
			if (num_grids >= VINFO_MAX_GRIDS)
			{
				quit_fmt("Too many grids (%d >= %d)!",
						 num_grids, VINFO_MAX_GRIDS);
			}

			/* Count grids */
			num_grids++;

			/* Slope to the top right corner */
			m = SCALE * (1000L * y - 500) / (1000L * x + 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, x, y, m);

			/* Slope to top left corner */
			m = SCALE * (1000L * y - 500) / (1000L * x - 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, x, y, m);

			/* Slope to bottom right corner */
			m = SCALE * (1000L * y + 500) / (1000L * x + 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, x, y, m);

			/* Slope to bottom left corner */
			m = SCALE * (1000L * y + 500) / (1000L * x - 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, x, y, m);
		}
	}


	/* Enforce maximal efficiency */
	if (num_grids < VINFO_MAX_GRIDS)
	{
		quit_fmt("Too few grids (%d < %d)!", num_grids, VINFO_MAX_GRIDS);
	}

	/* Enforce maximal efficiency */
	if (hack->num_slopes < VINFO_MAX_SLOPES)
	{
		quit_fmt("Too few slopes (%d < %d)!",
				 hack->num_slopes, VINFO_MAX_SLOPES);
	}


	/* Sort slopes numerically */
	ang_sort_comp = ang_sort_comp_hook_s32b;

	/* Sort slopes numerically */
	ang_sort_swap = ang_sort_swap_hook_s32b;

	/* Sort the (unique) slopes */
	ang_sort(hack->slopes, NULL, hack->num_slopes);


	/* Clear the counters for each slope */
	(void)C_WIPE(slope_count, VINFO_MAX_SLOPES, int);

	/* Enqueue player grid */
	queue[queue_tail++] = &vinfo[0];

	/* Process queue */
	while (queue_head < queue_tail)
	{
		int e;

		vinfo_type *p;


		/* Index */
		e = queue_head;

		/* Dequeue next grid */
		p = queue[queue_head++];

		/* Location */
		y = vinfo[e].grid_y[0];
		x = vinfo[e].grid_x[0];


		/* Compute grid offsets */
		vinfo[e].grid_x[0] = x;
		vinfo[e].grid_x[1] = y;
		vinfo[e].grid_x[2] = -y;
		vinfo[e].grid_x[3] = -x;
		vinfo[e].grid_x[4] = -x;
		vinfo[e].grid_x[5] = -y;
		vinfo[e].grid_x[6] = y;
		vinfo[e].grid_x[7] = x;

		vinfo[e].grid_y[0] = y;
		vinfo[e].grid_y[1] = x;
		vinfo[e].grid_y[2] = x;
		vinfo[e].grid_y[3] = y;
		vinfo[e].grid_y[4] = -y;
		vinfo[e].grid_y[5] = -x;
		vinfo[e].grid_y[6] = -x;
		vinfo[e].grid_y[7] = -y;


		/* Analyze slopes */
		for (i = 0; i < hack->num_slopes; ++i)
		{
			m = hack->slopes[i];

			/* Memorize intersection slopes (for non-player-grids) */
			if ((e > 0) &&
				(hack->slopes_min[y][x] < m) && (m < hack->slopes_max[y][x]))
			{
				/* We use this slope */
				slope_count[i]++;

				/* Save the bit that stands for this slope */
				vinfo[e].bits[i / 32] |= (1L << (i % 32));
			}
		}


		/* Default */
		vinfo[e].next_0 = &vinfo[0];

		/* Grid next child */
		if (distance(0, 0, x + 1, y) <= MAX_SIGHT)
		{
			if (!((queue[queue_tail - 1]->grid_x[0] == x + 1) &&
				  (queue[queue_tail - 1]->grid_y[0] == y)))
			{
				vinfo[queue_tail].grid_x[0] = x + 1;
				vinfo[queue_tail].grid_y[0] = y;
				queue[queue_tail] = &vinfo[queue_tail];
				queue_tail++;
			}

			vinfo[e].next_0 = &vinfo[queue_tail - 1];
		}


		/* Default */
		vinfo[e].next_1 = &vinfo[0];

		/* Grid diag child */
		if (distance(0, 0, x + 1, y + 1) <= MAX_SIGHT)
		{
			if (!((queue[queue_tail - 1]->grid_x[0] == x + 1) &&
				  (queue[queue_tail - 1]->grid_y[0] == y + 1)))
			{
				vinfo[queue_tail].grid_x[0] = x + 1;
				vinfo[queue_tail].grid_y[0] = y + 1;
				queue[queue_tail] = &vinfo[queue_tail];
				queue_tail++;
			}

			vinfo[e].next_1 = &vinfo[queue_tail - 1];
		}


		/* Hack -- main diagonal has special children */
		if (y == x) vinfo[e].next_0 = vinfo[e].next_1;


		/* Extra values */
		vinfo[e].y = y;
		vinfo[e].x = x;
		vinfo[e].d = ((y > x) ? (y + x / 2) : (x + y / 2));
		vinfo[e].r = ((!y) ? x : (!x) ? y : (y == x) ? y : 0);
	}


	/* Verify maximal bits XXX XXX XXX */
	if (((vinfo[1].bits[4] | vinfo[2].bits[4]) != VINFO_BITS_4) ||
		((vinfo[1].bits[3] | vinfo[2].bits[3]) != VINFO_BITS_3) ||
		((vinfo[1].bits[2] | vinfo[2].bits[2]) != VINFO_BITS_2) ||
		((vinfo[1].bits[1] | vinfo[2].bits[1]) != VINFO_BITS_1) ||
		((vinfo[1].bits[0] | vinfo[2].bits[0]) != VINFO_BITS_0))
	{
		quit("Incorrect bit masks!");
	}

	/* Create the project_data array */
	for (i = 0; i < VINFO_MAX_SLOPES; i++)
	{
		/* Create the list of squares intersected by this slope */
		C_MAKE(project_data[i], slope_count[i], project_type);

		j = 0;

		for (y = 0; y <= MAX_SIGHT; ++y)
		{
			for (x = y; x <= MAX_SIGHT; ++x)
			{
				/* Only if in range */
				if (distance(0, 0, x, y) > MAX_SIGHT) continue;

				/* Hack - ignore the origin */
				if (!x && !y) continue;

				m = hack->slopes[i];

				/* Does this square intersect the line? */
				if ((hack->slopes_min[y][x] < m) &&
					(m < hack->slopes_max[y][x]))
				{
					/* Save the square */
					project_data[i][j].x = x;
					project_data[i][j].y = y;

					/* Add in the slopes information */
					if (p_slope_min[x][y] > i) p_slope_min[x][y] = i;
					if (p_slope_max[x][y] < i) p_slope_max[x][y] = i;

					/* Next square... */
					j++;
				}
			}
		}
	}


	/* 
	 * Add in the final information in the projection table.
	 *
	 * We need to know where to go to if the current square
	 * is blocked.
	 *
	 * This is calculated in the following way:
	 *
	 * First, we need to find the first slope that does not
	 * include the current square.
	 *
	 *  This will be the first slope that does
	 * not contain this square.  The position along that slope
	 * will be the first square that is not already scanned
	 * by the current slope.
	 *
	 * This means that we may end up scanning squares twice,
	 * but the simplification of the algorithm is worth it. 
	 */
	for (i = 0; i < VINFO_MAX_SLOPES; i++)
	{
		for (j = 0; j < slope_count[i]; j++)
		{
			/* Set default case. */
			project_data[i][j].slope = VINFO_MAX_SLOPES;
			project_data[i][j].square = 0;

			/* Find first slope without this square */
			for (x = i + 1; x < VINFO_MAX_SLOPES; x++)
			{
				bool found = FALSE;

				for (y = 0; y < slope_count[x]; y++)
				{
					if ((project_data[x][y].x == project_data[i][j].x) &&
						(project_data[x][y].y == project_data[i][j].y))
					{
						found = TRUE;
						break;
					}
				}

				/* Did we find the blocking square? */
				if (found) continue;

				/* Do we already have an answer? */
				if (project_data[i][j].slope != VINFO_MAX_SLOPES) break;

				/* We did not find the square - save the row */
				project_data[i][j].slope = x;

				/* Paranoia */
				project_data[i][j].square = 0;

				/* Find the first non-matching square */
				for (y = 0; y < slope_count[x]; y++)
				{
					if ((project_data[x][y].x != project_data[i][y].x) ||
						(project_data[x][y].y != project_data[i][y].y))
					{
						/* Not a match */
						project_data[i][j].square = y;
						break;
					}
				}
			}
		}
	}

	/* Kill hack */
	FREE(hack);

	/* Success */
	return (0);
}

/*
 * Calculate the complete field of view using a new algorithm
 *
 *
 * Normally, vision along the major axes is more likely than vision
 * along the diagonal axes, so we check the bits corresponding to
 * the lines of sight near the major axes first.
 *
 * We use the "temp_x/y" arrays (and the "CAVE_TEMP" flag) to keep track of
 * which grids were previously marked "GRID_VIEW", since only those grids
 * whose "GRID_VIEW" value changes during this routine must be redrawn.
 *
 * This function is now responsible for maintaining the "GRID_LITE"
 * flags as well as the "GRID_VIEW" flags, which is good, because
 * the only grids which normally need to be memorized and/or redrawn
 * are the ones whose "GRID_VIEW" flag changes during this routine.
 *
 * Basically, this function divides the "octagon of view" into octants of
 * grids (where grids on the main axes and diagonal axes are "shared" by
 * two octants), and processes each octant one at a time, processing each
 * octant one grid at a time, processing only those grids which "might" be
 * viewable, and setting the "GRID_VIEW" flag for each grid for which there
 * is an (unobstructed) line of sight from the center of the player grid to
 * any internal point in the grid (and collecting these "GRID_VIEW" grids
 * into the "view_g" array), and setting the "GRID_LITE" flag for the grid
 * if, in addition, the grid is "illuminated" in some way (by a torch).
 *
 * This function relies on a theorem (suggested and proven by Mat Hostetter)
 * which states that in each octant of a field of view, a given grid will
 * be "intersected" by one or more unobstructed "lines of sight" from the
 * center of the player grid if and only if it is "intersected" by at least
 * one such unobstructed "line of sight" which passes directly through some
 * corner of some grid in the octant which is not shared by any other octant.
 * The proof is based on the fact that there are at least three significant
 * lines of sight involving any non-shared grid in any octant, one which
 * intersects the grid and passes though the corner of the grid closest to
 * the player, and two which "brush" the grid, passing through the "outer"
 * corners of the grid, and that any line of sight which intersects a grid
 * without passing through the corner of a grid in the octant can be "slid"
 * slowly towards the corner of the grid closest to the player, until it
 * either reaches it or until it brushes the corner of another grid which
 * is closer to the player, and in either case, the existance of a suitable
 * line of sight is thus demonstrated.
 *
 * It turns out that in each octant of the radius 20 "octagon of view",
 * there are 161 grids (with 128 not shared by any other octant), and there
 * are exactly 126 distinct "lines of sight" passing from the center of the
 * player grid through any corner of any non-shared grid in the octant.  To
 * determine if a grid is "viewable" by the player, therefore, you need to
 * simply show that one of these 126 lines of sight intersects the grid but
 * does not intersect any wall grid closer to the player.  So we simply use
 * a bit vector with 126 bits to represent the set of interesting lines of
 * sight which have not yet been obstructed by wall grids, and then we scan
 * all the grids in the octant, moving outwards from the player grid.  For
 * each grid, if any of the lines of sight which intersect that grid have not
 * yet been obstructed, then the grid is viewable.  Furthermore, if the grid
 * is a wall grid, then all of the lines of sight which intersect the grid
 * should be marked as obstructed for future reference.  Also, we only need
 * to check those grids for whom at least one of the "parents" was a viewable
 * non-wall grid, where the parents include the two grids touching the grid
 * but closer to the player grid (one adjacent, and one diagonal).  For the
 * bit vector, we simply use 4 32-bit integers.  All of the static values
 * which are needed by this function are stored in the large "vinfo" array
 * (above), which is initialised at startup.
 *
 * This has been changed to allow a more circular view, due to the more
 * advanced distance() function in Zangband.  There are now 135 lines of
 * sight and one more 32 bit flag to hold the data.
 *
 * Hack -- The queue must be able to hold more than VINFO_MAX_GRIDS grids
 * because the grids at the edge of the field of view use "grid zero" as
 * their children, and the queue must be able to hold several of these
 * special grids.  Because the actual number of required grids is bizarre,
 * we simply allocate twice as many as we would normally need.  XXX XXX XXX
 */
void update_view(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *o_ptr;

	cave_type *c_ptr;
	pcave_type *pc_ptr;

	byte info, player;

	int x, y, i, o2;

	s16b this_f_idx, next_f_idx = 0;

	/* Light radius */
	s16b radius = p_ptr->cur_lite;


	/*** Save the old "lite" grids for later ***/

	/* Save the old "view" grids for later */
	for (i = 0; i < view_n; i++)
	{
		y = view_y[i];
		x = view_x[i];

		if (!in_boundsp(x, y)) continue;

		c_ptr = area(x, y);
		pc_ptr = parea(x, y);

		/* Save "GRID_VIEW" grids */
		if (player_has_los_grid(pc_ptr))
		{
			/* Set "CAVE_TEMP" flag */
			c_ptr->info |= (CAVE_TEMP);

			/* Save grid for later */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}

		/* Clear "GRID_VIEW" and "GRID_SEEN" flags */
		pc_ptr->player &= ~(GRID_VIEW | GRID_SEEN);
	}

	/* empty the viewable list */
	view_n = 0;

	/*** Step 1 -- player grid ***/

	/* Player grid */

	/* Get grid info */
	c_ptr = area(px, py);
	pc_ptr = parea(px, py);
	info = c_ptr->info;
	player = pc_ptr->player;

	/* Assume viewable */
	player |= (GRID_VIEW);

	/* Remember square under player */
	remember_grid(c_ptr, pc_ptr);

	/* Torch-lit grid */
	if (0 < radius)
	{
		/* Mark as "GRID_LITE" */
		player |= (GRID_LITE);
	}

	/* Save the fact that we used to know this square */
	if (pc_ptr->feat)
	{
		info |= CAVE_XTRA;
	}

	/* Save cave info */
	c_ptr->info = info;
	pc_ptr->player = player;

	/* Redraw player */
	/*lite_spot(px, py); */

	/* Save in array */

	view_y[view_n] = py;
	view_x[view_n] = px;
	view_n++;

	/*** Step 2 -- octants ***/

	/* Scan each octant */
	for (o2 = 0; o2 < 8; o2 += 1)
	{
		vinfo_type *p;

		/* Last added */
		vinfo_type *last = &vinfo[0];

		/* Grid queue */
		int queue_head = 0;
		int queue_tail = 0;
		vinfo_type *queue[VINFO_MAX_GRIDS * 2];

		/* Slope bit vector */
		u32b bits0 = VINFO_BITS_0;
		u32b bits1 = VINFO_BITS_1;
		u32b bits2 = VINFO_BITS_2;
		u32b bits3 = VINFO_BITS_3;
		u32b bits4 = VINFO_BITS_4;

		/* Reset queue */
		queue_head = queue_tail = 0;

		/* Initial grids */
		queue[queue_tail++] = &vinfo[1];
		queue[queue_tail++] = &vinfo[2];

		/* Process queue */
		while (queue_head < queue_tail)
		{
			/* Dequeue next grid */
			p = queue[queue_head++];

			/* Check bits */
			if ((bits0 & (p->bits[0])) ||
				(bits1 & (p->bits[1])) ||
				(bits2 & (p->bits[2])) ||
				(bits3 & (p->bits[3])) || (bits4 & (p->bits[4])))
			{
				/* Get location */
				x = p->grid_x[o2] + px;
				y = p->grid_y[o2] + py;

				/* Is it in bounds? */
				if (!in_boundsp(x, y))
				{
					/* Clear bits */
					bits0 &= ~(p->bits[0]);
					bits1 &= ~(p->bits[1]);
					bits2 &= ~(p->bits[2]);
					bits3 &= ~(p->bits[3]);
					bits4 &= ~(p->bits[4]);

					continue;
				}

				/* Point to the location on the map */
				c_ptr = area(x, y);
				pc_ptr = parea(x, y);

				/* Get current info flags for the square */
				info = c_ptr->info;
				player = pc_ptr->player;

				/* Save the fact that we used to know this square */
				if (pc_ptr->feat)
				{
					info |= CAVE_XTRA;
				}

				if (cave_los_grid(c_ptr))
				{
					/* Floor or semi-blocking terrain like trees */

					/* Enqueue child */
					if (last != p->next_0)
					{
						queue[queue_tail++] = last = p->next_0;
					}

					/* Enqueue child */
					if (last != p->next_1)
					{
						queue[queue_tail++] = last = p->next_1;
					}
				}
				/* Handle wall */
				else
				{
					/* Clear bits */
					bits0 &= ~(p->bits[0]);
					bits1 &= ~(p->bits[1]);
					bits2 &= ~(p->bits[2]);
					bits3 &= ~(p->bits[3]);
					bits4 &= ~(p->bits[4]);
				}

				/* All ready seen.  Next... */
				if (player & GRID_VIEW) continue;

				/* Mark as viewable */
				player |= (GRID_VIEW);

				/* Torch-lit grids */
				if (p->d <= radius)
				{
					if (!(player & GRID_LITE))
					{
						/* Mark as "GRID_LITE" */
						player |= (GRID_LITE);

						/* Clear the 'do not update flag' */
						info &= ~(CAVE_TEMP);
					}
				}
				else
				{
					if (player & GRID_LITE)
					{
						/* Clear the flag, and then redraw */
						info &= ~(CAVE_TEMP);
						player &= ~(GRID_LITE);
					}
				}

				/* Save cave info */
				c_ptr->info = info;
				pc_ptr->player = player;

				/* Save in array */
				view_y[view_n] = y;
				view_x[view_n] = x;
				view_n++;
			}
		}
	}

	/*** Step 3 -- Complete the algorithm ***/

	/* Process "new" grids */
	for (i = 0; i < view_n; i++)
	{
		/* Grid */
		x = view_x[i];
		y = view_y[i];

		c_ptr = area(x, y);
		pc_ptr = parea(x, y);

		/* Get grid info */
		info = c_ptr->info;

		/* Handle blindness */
		if ((p_ptr->blind) && !(info & CAVE_XTRA))
		{
			/* Grid cannot be memorised (wasn't before) */
			forget_grid(pc_ptr);

			/* Don't do anything else */
			continue;
		}

		/*
		 * We know we have LOS, but is it visible?
		 */
		if ((info & (CAVE_GLOW | CAVE_MNLT)) || (pc_ptr->player & (GRID_LITE)))
		{
			/* Walls are special */
			if (!cave_floor_grid(c_ptr) && !(pc_ptr->player & (GRID_LITE)))
			{
				/* This is part of note_spot() */
				int yy, xx;

				/* Hack -- move towards player */
				yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
				xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

				/* Check for "local" illumination */
				if (!(area(xx, yy)->info & (CAVE_GLOW | CAVE_MNLT)))
				{
					/* Assume the wall isn't illuminated */
					continue;
				}
			}

			/* We can see it... */
			pc_ptr->player |= GRID_SEEN;


			/* Show the objects */
			OBJ_ITT_START (c_ptr->o_idx, o_ptr)
			{
				/* Memorize objects */
				o_ptr->info |= OB_SEEN;
			}
			OBJ_ITT_END;

			/* Show the fields */
			for (this_f_idx = c_ptr->fld_idx; this_f_idx;
				 this_f_idx = next_f_idx)
			{
				field_type *f_ptr = &fld_list[this_f_idx];

				/* Acquire next field */
				next_f_idx = f_ptr->next_f_idx;

				/* Memorize fields */
				f_ptr->info |= FIELD_INFO_MARK;
			}

			/* Memorise grid */
			remember_grid(c_ptr, pc_ptr);

			/* Must note the new information on the screen */
			if (!(info & CAVE_TEMP))
			{
				/* Redraw */
				lite_spot(x, y);
			}
		}
	}

	/* Process "old" grids */
	for (i = 0; i < temp_n; i++)
	{
		/* Grid */
		x = temp_x[i];
		y = temp_y[i];

		c_ptr = area(x, y);
		pc_ptr = parea(x, y);

		/* Get grid info */
		info = c_ptr->info;
		player = pc_ptr->player;

		/* Clear "CAVE_TEMP" and "CAVE_XTRA" flags */
		info &= ~(CAVE_TEMP | CAVE_XTRA);

		/* Was "GRID_SEEN", is now not in view */
		if (!(player & (GRID_SEEN)))
		{
			/* Forget memorized floor grids from view_torch_grids */
			if (!(info & (CAVE_GLOW)) && !view_torch_grids
				&& (c_ptr->feat == FEAT_FLOOR))
			{
				forget_grid(pc_ptr);
			}

			/* Clear the GRID_LITE flag */
			player &= ~(GRID_LITE);

			/* Save cave info */
			c_ptr->info = info;
			pc_ptr->player = player;

			/* Redraw */
			lite_spot(x, y);
		}
		else
		{
			/* Save cave info */
			c_ptr->info = info;
		}
	}

	/* None left */
	temp_n = 0;
}

/* Monster location */
static int mon_lite_mx, mon_lite_my;

/*
 * Add a square to the changes array
 */
static void mon_lite_hack(int x, int y)
{
	cave_type *c_ptr;
	pcave_type *pc_ptr;

	int dx1, dy1, dx2, dy2;

	/* Out of bounds */
	if (!in_boundsp(x, y)) return;

	c_ptr = area(x, y);
	pc_ptr = parea(x, y);

	/* Want an unlit square */
	if (c_ptr->info & (CAVE_MNLT)) return;

	/* Get vectors */
	dx1 = p_ptr->px - x;
	dy1 = p_ptr->py - y;
	dx2 = mon_lite_mx - x;
	dy2 = mon_lite_my - y;

	/*
	 * Use a dot product to determine angle of illumination
	 *
	 * Only illuminate the correct sides of walls.  (We don't
	 * need to worry about floor - we won't illuminate that
	 * if we cannot see it.)
	 */
	if (!cave_los_grid(c_ptr) && ((dx1 * dx2 + dy1 * dy2) < 0)) return;

	/* Save this square */
	if (temp_n < TEMP_MAX)
	{
		temp_x[temp_n] = x;
		temp_y[temp_n] = y;
		temp_n++;
	}

	/* Light it */
	c_ptr->info |= CAVE_MNLT;

	/* We can see it? */
	if (player_has_los_grid(pc_ptr))
	{
		pc_ptr->player |= GRID_SEEN;

		/* Remember it if view_monster_grids is set. */
		if (view_monster_grids) remember_grid(c_ptr, pc_ptr);
	}
}




/*
 * Update squares illuminated by monsters.
 *
 * Use the CAVE_MNLT flag to denote squares illuminated by monsters.
 *
 * The CAVE_TEMP flag is used to store the state during the
 * updating.  Only squares in view of the player, whose state
 * changes are drawn via lite_spot().
 */
void update_mon_lite(void)
{
	int i, rad;
	cave_type *c_ptr;
	pcave_type *pc_ptr;

	s16b fx, fy;

	/* Blindness check */
	if (p_ptr->blind)
	{
		/* Clear all the monster lit squares */
		for (i = 0; i < lite_n; i++)
		{
			fx = lite_x[i];
			fy = lite_y[i];

			/* Point to grid */
			c_ptr = area(fx, fy);
			pc_ptr = parea(fx, fy);

			/* Clear monster illumination flag */
			c_ptr->info &= ~(CAVE_MNLT);

			/* XXX XXX Square is invisible */
			pc_ptr->player &= ~(GRID_SEEN);

			/* It is now unlit */
			note_spot(fx, fy);
		}

		/* Clear the lit list */
		lite_n = 0;

		/* Done */
		return;
	}

	/* Clear all monster lit squares */
	for (i = 0; i < lite_n; i++)
	{
		/* Point to grid */
		c_ptr = area(lite_x[i], lite_y[i]);
		pc_ptr = parea(lite_x[i], lite_y[i]);

		/* Set temp flag */
		c_ptr->info |= (CAVE_TEMP);

		/* Clear monster illumination flag */
		c_ptr->info &= ~(CAVE_MNLT);

		/* See if the square is still lit */
		if (!((c_ptr->info & (CAVE_GLOW)) || pc_ptr->player & (GRID_LITE)))
		{
			/* Not lit any more */
			pc_ptr->player &= ~(GRID_SEEN);
		}
	}

	/* Empty temp list of new squares to lite up */
	temp_n = 0;

	/* Loop through monsters, adding newly lit squares to changes list */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Is it too far away? */
		if (m_ptr->cdis > MAX_SIGHT + 3) continue;

		/* Get lite radius */
		rad = 0;

		/* Note the radii are cumulative */
		if (r_ptr->flags7 & (RF7_LITE_1)) rad++;
		if (r_ptr->flags7 & (RF7_LITE_2)) rad += 2;

		/* Exit if has no light */
		if (!rad) continue;

		/* Access the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;

		/* Save information */
		mon_lite_mx = fx;
		mon_lite_my = fy;

		/* The square it is on */
		mon_lite_hack(fx, fy);

		/* Adjacent squares */
		mon_lite_hack(fx + 1, fy);
		mon_lite_hack(fx - 1, fy);
		mon_lite_hack(fx, fy + 1);
		mon_lite_hack(fx, fy - 1);
		mon_lite_hack(fx + 1, fy + 1);
		mon_lite_hack(fx + 1, fy - 1);
		mon_lite_hack(fx - 1, fy + 1);
		mon_lite_hack(fx - 1, fy - 1);

		/* Radius 2 */
		if (rad >= 2)
		{
			/* South of the monster */
			if (in_boundsp(fx, fy + 1) && cave_floor_grid(area(fx, fy + 1)))
			{
				mon_lite_hack(fx + 1, fy + 2);
				mon_lite_hack(fx, fy + 2);
				mon_lite_hack(fx - 1, fy + 2);

				if (in_boundsp(fx, fy + 2))
				{
					c_ptr = area(fx, fy + 2);

					/* Radius 3 */
					if ((rad == 3) && cave_floor_grid(c_ptr))
					{
						mon_lite_hack(fx + 1, fy + 3);
						mon_lite_hack(fx, fy + 3);
						mon_lite_hack(fx - 1, fy + 3);
					}
				}
			}

			/* North of the monster */
			if (in_boundsp(fx, fy - 1) && cave_floor_grid(area(fx, fy - 1)))
			{
				mon_lite_hack(fx + 1, fy - 2);
				mon_lite_hack(fx, fy - 2);
				mon_lite_hack(fx - 1, fy - 2);

				if (in_boundsp(fx, fy - 2))
				{
					c_ptr = area(fx, fy - 2);

					/* Radius 3 */
					if ((rad == 3) && cave_floor_grid(c_ptr))
					{
						mon_lite_hack(fx + 1, fy - 3);
						mon_lite_hack(fx, fy - 3);
						mon_lite_hack(fx - 1, fy - 3);
					}
				}
			}

			/* East of the monster */
			if (in_boundsp(fx + 1, fy) && cave_floor_grid(area(fx + 1, fy)))
			{
				mon_lite_hack(fx + 2, fy + 1);
				mon_lite_hack(fx + 2, fy);
				mon_lite_hack(fx + 2, fy - 1);

				if (in_boundsp(fx + 2, fy))
				{
					c_ptr = area(fx + 2, fy);

					/* Radius 3 */
					if ((rad == 3) && cave_floor_grid(c_ptr))
					{
						mon_lite_hack(fx + 3, fy + 1);
						mon_lite_hack(fx + 3, fy);
						mon_lite_hack(fx + 3, fy - 1);
					}
				}
			}

			/* West of the monster */
			if (in_boundsp(fx - 1, fy) && cave_floor_grid(area(fx - 1, fy)))
			{
				mon_lite_hack(fx - 2, fy + 1);
				mon_lite_hack(fx - 2, fy);
				mon_lite_hack(fx - 2, fy - 1);

				if (in_boundsp(fx - 2, fy))
				{
					c_ptr = area(fx - 2, fy);

					/* Radius 3 */
					if ((rad == 3) && cave_floor_grid(c_ptr))
					{
						mon_lite_hack(fx - 3, fy + 1);
						mon_lite_hack(fx - 3, fy);
						mon_lite_hack(fx - 3, fy - 1);
					}
				}
			}
		}

		/* Radius 3 */
		if (rad == 3)
		{
			/* South-East of the monster */
			if (in_boundsp(fx + 1, fy + 1) &&
				cave_floor_grid(area(fx + 1, fy + 1)))
			{
				mon_lite_hack(fx + 2, fy + 2);
			}

			/* South-West of the monster */
			if (in_boundsp(fx - 1, fy + 1) &&
				cave_floor_grid(area(fx - 1, fy + 1)))
			{
				mon_lite_hack(fx - 2, fy + 2);
			}

			/* North-East of the monster */
			if (in_boundsp(fx + 1, fy - 1) &&
				cave_floor_grid(area(fx + 1, fy - 1)))
			{
				mon_lite_hack(fx + 2, fy - 2);
			}

			/* North-West of the monster */
			if (in_boundsp(fx - 1, fy - 1) &&
				cave_floor_grid(area(fx - 1, fy - 1)))
			{
				mon_lite_hack(fx - 2, fy - 2);
			}
		}
	}

	/*
	 * Look at old set flags to see if there are any changes.
	 */
	for (i = 0; i < lite_n; i++)
	{
		fx = lite_x[i];
		fy = lite_y[i];

		if (!in_boundsp(fx, fy)) continue;

		/* Point to grid */
		c_ptr = area(fx, fy);

		/* It it no longer lit? */
		if (!(c_ptr->info & CAVE_MNLT))
		{
			/* Clear the temp flag for the old lit grids */
			c_ptr->info &= ~(CAVE_TEMP);

			if (player_has_los_grid(parea(fx, fy)))
			{
				/* Do we have a monster on this square? */
				if (c_ptr->m_idx)
				{
					/* Update the monster */
					update_mon(c_ptr->m_idx, FALSE);
				}

				/* It is now unlit */
				note_spot(fx, fy);
			}
		}
	}

	/* Clear the lite array */
	lite_n = 0;

	/* Copy the temp array into the lit array lighting the new squares. */
	for (i = 0; i < temp_n; i++)
	{
		fx = temp_x[i];
		fy = temp_y[i];

		if (!in_boundsp(fx, fy)) continue;

		/* Point to grid */
		c_ptr = area(fx, fy);

		if (c_ptr->info & CAVE_TEMP)
		{
			/* Clear the temp flag for the old lit grids that are still lit */
			c_ptr->info &= ~(CAVE_TEMP);
		}

		/* Is the square newly lit and visible? */
		else if (player_has_los_grid(parea(fx, fy)))
		{
			/* Do we have a monster on this square? */
			if (c_ptr->m_idx)
			{
				/* Update the monster */
				update_mon(c_ptr->m_idx, FALSE);
			}

			/* It is now lit */
			note_spot(fx, fy);
		}

		/* Save in the monster lit array */
		lite_x[lite_n] = fx;
		lite_y[lite_n] = fy;
		lite_n++;
	}

	/* Finished with temp_n */
	temp_n = 0;
}


void clear_mon_lite(void)
{
	int i;
	cave_type *c_ptr;

	/* Clear all monster lit squares */
	for (i = 0; i < lite_n; i++)
	{
		/* Point to grid */
		c_ptr = area(lite_x[i], lite_y[i]);

		/* Clear monster illumination flag */
		c_ptr->info &= ~(CAVE_MNLT);
	}

	/* Empty the array */
	lite_n = 0;
}


/*
 * Hack -- provide some "speed" for the "flow" code
 * This entry is the "current index" for the "when" field
 * Note that a "when" value of "zero" means "not used".
 *
 * Note that the "cost" indexes from 1 to 127 are for
 * "old" data, and from 128 to 255 are for "new" data.
 *
 * This means that as long as the player does not "teleport",
 * then any monster up to 128 + MONSTER_FLOW_DEPTH will be
 * able to track down the player, and in general, will be
 * able to track down either the player or a position recently
 * occupied by the player.
 */
static int flow_n = 0;


/*
 * Hack -- forget the "flow" information
 */
void forget_flow(void)
{
	int x, y;

	/* Nothing to forget */
	if (!flow_n) return;

	/* Check the entire dungeon */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
		{
			/* Forget the old data */
			area(x, y)->cost = 0;
			area(x, y)->when = 0;
		}
	}

	/* Start over */
	flow_n = 0;
}


/*
 * Hack - speed up the update_flow algorithm by only doing
 * it everytime the player moves out of LOS of the last
 * "way-point".
 */
static u16b flow_x = 0;
static u16b flow_y = 0;

/*
 * Hack -- fill in the "cost" field of every grid that the player
 * can "reach" with the number of steps needed to reach that grid.
 * This also yields the "distance" of the player from every grid.
 *
 * In addition, mark the "when" of the grids that can reach
 * the player with the incremented value of "flow_n".
 *
 * Hack -- use the "seen" array as a "circular queue".
 *
 * We do not need a priority queue because the cost from grid
 * to grid is always "one" and we process them in order.
 */
void update_flow(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int x, y, d, w, n;
	int ty, tx;

	int flow_tail = 1;
	int flow_head = 0;

	cave_type *c_ptr;
	byte feat;

	/* Paranoia -- make sure the array is empty */
	if (temp_n) return;

	/* The last way-point is on the map */
	if (in_boundsp(flow_x, flow_y))
	{
		/* Check to see if the player is too close and in los */
		if ((distance(px, py, flow_x, flow_y) < FLOW_DIST_MAX)
			&& player_has_los_grid(parea(flow_x, flow_y))
			&& (p_ptr->noise_level < MONSTER_FLOW_DEPTH / 4)) return;
	}

	/* Save player position */
	flow_y = py;
	flow_x = px;

	/* Cycle the old entries (once per 128 updates) */
	if (flow_n++ == 255)
	{
		/* Rotate the time-stamps */
		for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
		{
			for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
			{
				c_ptr = area(x, y);
				w = c_ptr->when;
				c_ptr->when = (w > 128) ? (w - 128) : 0;
			}
		}

		/* Restart */
		flow_n = 128;
	}


	/*** Player Grid ***/
	c_ptr = area(px, py);

	/* Save the time-stamp */
	c_ptr->when = flow_n;

	/* Save the flow cost */
	c_ptr->cost = p_ptr->noise_level;
	p_ptr->noise_level = 0;

	/* Paranoia - not too much noise */
	if (c_ptr->cost > MONSTER_FLOW_DEPTH)
	{
		c_ptr->cost = MONSTER_FLOW_DEPTH;
	}

	/* Enqueue that entry */
	temp_y[0] = py;
	temp_x[0] = px;


	/* Now process the queue */
	while (flow_head != flow_tail)
	{
		/* Extract the next entry */
		ty = temp_y[flow_head];
		tx = temp_x[flow_head];

		/* Forget that entry */
		if (++flow_head == TEMP_MAX) flow_head = 0;

		/* Child cost */
		n = area(tx, ty)->cost - 1;

		/* Hack -- Limit flow depth to noise level */
		if (n == 0) continue;

		/* Add the "children" */
		for (d = 0; d < 8; d++)
		{
			int old_head = flow_tail;

			/* Child location */
			y = ty + ddy_ddd[d];
			x = tx + ddx_ddd[d];

			if (!in_bounds2(x, y)) continue;

			c_ptr = area(x, y);

			feat = c_ptr->feat;

			/* Ignore "pre-stamped" entries */
			if (c_ptr->when == flow_n) continue;

			/* Ignore all "walls" except doors + terrain */
			if (cave_wall_grid(c_ptr) && (feat != FEAT_CLOSED))
			{
				continue;
			}

#if 0
			/*
			 * Hack - do not overwrite loud sounds with quiet ones,
			 * unless some time has passed
			 */
			if (c_ptr->cost + c_ptr->when > n + flow_n) continue;
#endif

			/* Save the time-stamp */
			c_ptr->when = flow_n;

			/* Save the flow cost */
			c_ptr->cost = n;

			/* Enqueue that entry */
			temp_y[flow_tail] = y;
			temp_x[flow_tail] = x;

			/* Advance the queue */
			if (++flow_tail == TEMP_MAX) flow_tail = 0;

			/* Hack -- Overflow by forgetting new entry */
			if (flow_tail == flow_head) flow_tail = old_head;
		}
	}
}



/*
 * Hack -- map a region ala "magic mapping"
 */
void map_area(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, x, y, y1, y2, x1, x2;

	cave_type *c_ptr;
	pcave_type *pc_ptr;

	/* Pick an area to map */
	y1 = py - MAX_DETECT - randint1(10);
	y2 = py + MAX_DETECT + randint1(10);
	x1 = px - MAX_DETECT - randint1(20);
	x2 = px + MAX_DETECT + randint1(20);

	/* Speed -- shrink to fit legal bounds */
	if (y1 < p_ptr->min_hgt + 1) y1 = p_ptr->min_hgt + 1;
	if (y2 > p_ptr->max_hgt - 2) y2 = p_ptr->max_hgt - 2;
	if (x1 < p_ptr->min_wid + 1) x1 = p_ptr->min_wid + 1;
	if (x2 > p_ptr->max_wid - 2) x2 = p_ptr->max_wid - 2;

	/* Scan that area */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			c_ptr = area(x, y);
			pc_ptr = parea(x, y);

			/* All non-walls are "checked" */
			if (!((c_ptr->feat >= FEAT_MAGMA) &&
				  (c_ptr->feat <= FEAT_PERM_SOLID)))
			{
				/* Memorize normal features */
				if (c_ptr->feat != FEAT_FLOOR)
				{
					/* Memorize the grid */
					remember_grid(c_ptr, pc_ptr);
				}

				/* Memorize known walls */
				for (i = 0; i < 8; i++)
				{
					c_ptr = area(x + ddx_ddd[i], y + ddy_ddd[i]);
					pc_ptr = parea(x + ddx_ddd[i], y + ddy_ddd[i]);

					/* Memorize walls */
					if ((c_ptr->feat >= FEAT_MAGMA) &&
						(c_ptr->feat <= FEAT_PERM_SOLID))
					{
						/* Memorize the walls */
						remember_grid(c_ptr, pc_ptr);
					}
				}
			}
		}
	}

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}



/*
 * Light up the dungeon using "clairvoyance"
 *
 * This function "illuminates" every grid in the dungeon, memorizes all
 * "objects", memorizes all grids as with magic mapping, and, under the
 * standard option settings (view_perma_grids but not view_torch_grids)
 * memorizes all floor grids too.
 *
 * Note that if "view_perma_grids" is not set, we do not memorize floor
 * grids, since this would defeat the purpose of "view_perma_grids", not
 * that anyone seems to play without this option.
 *
 * Note that if "view_torch_grids" is set, we do not memorize floor grids,
 * since this would prevent the use of "view_torch_grids" as a method to
 * keep track of what grids have been observed directly.
 */
void wiz_lite(void)
{
	int i, y, x;

	object_type *o_ptr;

	chg_virtue(V_KNOWLEDGE, 1);
	chg_virtue(V_ENLIGHTEN, 1);

	/* Detect monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Repair visibility later */
		repair_monsters = TRUE;

		/* Hack -- Detect monster */
		m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(i, FALSE);
	}

	/* Scan all normal grids */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
		{
			cave_type *c_ptr = area(x, y);
			pcave_type *pc_ptr = parea(x, y);

			/* Memorize normal features */
			if (c_ptr->feat != FEAT_FLOOR)
			{
				/* Memorize the grid */
				remember_grid(c_ptr, pc_ptr);
			}

			/* Remember items on the grid */
			OBJ_ITT_START (c_ptr->o_idx, o_ptr)
			{
				/* Memorize */
				o_ptr->info |= OB_SEEN;
			}
			OBJ_ITT_END;
		}
	}


	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void wiz_dark(void)
{
	int i, y, x;

	/* Forget every grid */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
		{
			cave_type *c_ptr = area(x, y);
			pcave_type *pc_ptr = parea(x, y);

			object_type *o_ptr;

			/* Process the grid */
			forget_grid(pc_ptr);

			/* Forget items on the grid */
			OBJ_ITT_START (c_ptr->o_idx, o_ptr)
			{
				/* Forget the object */
				o_ptr->info &= ~(OB_SEEN);
			}
			OBJ_ITT_END;

		}
	}

	/* Forget all fields */
	for (i = 1; i < fld_max; i++)
	{
		field_type *f_ptr = &fld_list[i];

		/* Skip dead fields */
		if (!f_ptr->t_idx) continue;

		/* Forget the object */
		f_ptr->info &= (~FIELD_INFO_MARK);
	}

	/* Update the view */
	p_ptr->update |= (PU_VIEW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}


/*
 * Change the "feat" flag for a grid, and notice/redraw the grid
 */
void cave_set_feat(int x, int y, int feat)
{
	cave_type *c_ptr = area(x, y);

	/* Does los change? */
	if (cave_floor_grid(c_ptr))
	{
		/* Is new eat a wall grid? */
		if (f_info[feat].flags & FF_BLOCK)
		{
			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
		}
	}
	else
	{
		/* Is new feat a floor grid? */
		if (!(f_info[feat].flags & FF_BLOCK))
		{
			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
		}
	}

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Change the feature */
	c_ptr->feat = feat;

	/* Notice + Redraw */
	if (character_dungeon) note_spot(x, y);

	/* Hack - if under player, notice change */
	if ((x == p_ptr->px) && (y == p_ptr->py))
	{
		parea(x, y)->feat = feat;

		/* Draw change */
		lite_spot(x, y);
	}
}



/*
 * Track a new monster
 */
void health_track(int m_idx)
{
	/* Track a new guy */
	p_ptr->health_who = m_idx;

	/* Redraw (later) */
	p_ptr->redraw |= (PR_HEALTH);
}



/*
 * Hack -- track the given monster race
 */
void monster_race_track(int r_idx)
{
	/* Save this monster ID */
	p_ptr->monster_race_idx = r_idx;

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);
}



/*
 * Hack -- track the given object kind
 */
void object_kind_track(int k_idx)
{
	/* Save this monster ID */
	p_ptr->object_kind_idx = k_idx;

	/* Window stuff */
	p_ptr->window |= (PW_OBJECT);
}



/*
 * Something has happened to disturb the player.
 *
 * The arg indicates a major disturbance, which affects search.
 *
 * All disturbance cancels repeated commands, resting, and running.
 */
void disturb(bool stop_search)
{
	/* Cancel repeated commands */
	if (p_ptr->command_rep)
	{
		/* Cancel */
		p_ptr->command_rep = 0;

		/* Redraw the state (later) */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel Resting */
	if (p_ptr->resting)
	{
		/* Cancel */
		p_ptr->resting = 0;

		/* Redraw the state (later) */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel running */
	if (p_ptr->running)
	{
		/* Cancel */
		p_ptr->running = 0;

		/* Check for new panel if appropriate */
		if (center_player && avoid_center) verify_panel();

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* Cancel searching if requested */
	if (stop_search && p_ptr->searching)
	{
		/* Cancel */
		p_ptr->searching = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Flush the input if requested */
	if (flush_disturb) flush();
}
