/* File: zborg2.c */
/* Purpose: Low level dungeon mapping skills -BEN- */

#include "angband.h"


#ifdef ALLOW_BORG

#include "zborg1.h"
#include "zborg2.h"


/*
 * This file helps the Borg understand mapping the dungeon.
 *
 * Currently, this includes general routines involving dungeon grids,
 * including calculating "flow" values from place to place, determining
 * "line of sight", plus "field of view" and "torch-lit grids", setting
 * the target to a given location, and extracting the optimal direction
 * for "movement" from place to place.
 *
 * Note that the dungeon is assumed smaller than 256 by 256.
 *
 * This file also supplies the (compiled out) support for "automatic
 * room extraction".  This code will automatically group regions of
 * the dungeon into rooms, and do the "flow" navigation on those rooms
 * instead of on grids.  Often, this takes less space, and is faster,
 * howver, it is more complicated, and does not allow "specialized"
 * flow calculations that penalize grids by variable amounts.
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
static errr borg_vinfo_init(void)
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
void borg_update_view(void)
{
	map_block *mb_ptr;

	byte info;

	int x, y, i, o2;

	/* Clear the old "view" grids */
	for (i = 0; i < view_n; i++)
	{
		y = borg_view_y[i];
		x = borg_view_x[i];

		if (!map_in_bounds(x, y)) continue;

		mb_ptr = map_loc(x, y);

		/* Clear "BORG_VIEW" flag */
		mb_ptr->info &= ~(BORG_MAP_VIEW);
	}

	/* empty the viewable list */
	borg_view_n = 0;

	/*** Step 1 -- player grid ***/

	/* Player grid */

	/* Get grid info */
	mb_ptr = map_loc(c_x, c_y);

	/* Assume viewable */
	mb_ptr->info |= (BORG_MAP_VIEW);

	/* Save in array */
	borg_view_y[view_n] = c_y;
	borg_view_x[view_n] = c_x;
	borg_view_n++;

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
				(bits3 & (p->bits[3])) ||
				(bits4 & (p->bits[4])))
			{
				/* Get location */
				x = p->grid_x[o2] + c_x;
				y = p->grid_y[o2] + c_y;

				/* Is it in bounds? */
				if (!map_in_bounds(x, y))
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
				mb_ptr = map_loc(x, y);

				/* Get current info flags for the square */
				info = mb_ptr->info;

				if (borg_cave_los_grid(mb_ptr))
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
				if (info & BORG_MAP_VIEW) continue;

				/* Mark as viewable */
				info |= (BORG_MAP_VIEW);

				/* Save cave info */
				mb_ptr->info = info;

				/* Save in array */
				borg_view_y[view_n] = y;
				borg_view_x[view_n] = x;
				borg_view_n++;
			}
		}
	}
}


/*
 * Clear the viewable space
 */
void borg_forget_view(void)
{
	int i;

	map_block *mb_ptr;

	/* None to forget */
	if (!borg_view_n) return;

	/* Clear them all */
	for (i = 0; i < borg_view_n; i++)
	{
		int y = borg_view_y[i];
		int x = borg_view_x[i];

		/* Bounds checking */
		if (!map_in_bounds(x, y)) continue;

		/* Access the grid */
		mb_ptr = map_loc(x, y);

		/* Forget that the grid is viewable */
		mb_ptr->info &= ~BORG_MAP_VIEW;
	}

	/* None left */
	borg_view_n = 0;
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
static bool los_general(int x1, int y1, int x2, int y2, map_hook_type mb_hook)
{
	int i, j, temp, dist;

	int x, y;

	int dx, dy, ax, ay, sx, sy;

	map_block *mb_ptr;

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
			if (!map_in_bounds(x, y)) return (FALSE);

			mb_ptr = map_loc(x, y);

			if ((*mb_hook) (mb_ptr))
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
			if (!map_in_bounds(x, y)) return (FALSE);

			mb_ptr = map_loc(x, y);

			if ((*mb_hook) (mb_ptr))
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
 * A function to pass to los_general()
 * to do borg_los().
 * we stop at walls
 */
static bool map_stop_wall(map_block *mb_ptr)
{
	/* Is it passable? */
	if (borg_cave_los_grid(mb_ptr)) return (FALSE);

	/* Seems ok */
	return (TRUE);
}

/*
 * a function to pass to los_general().
 * to do borg_bolt_los().
 * we stop at unknown grids and walls
 */
static bool map_stop_wall_pure(map_block *mb_ptr)
{
	/* Do not allow unknown grids */
	if (!mb_ptr->feat) return (FALSE);

	/* Is it passable? */
	if (borg_cave_los_grid(mb_ptr)) return (FALSE);

	/* Seems ok */
	return (TRUE);
}
/*
 * Hack - a function to pass to los_general() used
 * to do borg_bolt_los().
 * we stop at known monsters and walls
 */
static bool map_stop_bolt(map_block *mb_ptr)
{
	/* Walls block projections */
	if (borg_cave_wall_grid(mb_ptr)) return (TRUE);

	/* Stop at monsters */
	if (mb_ptr->monster) return (TRUE);

	/* Seems ok */
	return (FALSE);
}

/*
 * Hack - a function to pass to los_general() used
 * to do borg_bolt_los_pure().
 * we stop at known monsters, walls and unknown grids
 */
static bool map_stop_bolt_pure(map_block *mb_ptr)
{
	/* Unknown area is probably a wall */
	if (!mb_ptr->feat) return (TRUE);

	/* Walls block projections */
	if (borg_cave_wall_grid(mb_ptr)) return (TRUE);

	/* Stop at monsters */
	if (mb_ptr->monster) return (TRUE);

	/* Seems ok */
	return (FALSE);
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
bool borg_los(int x1, int y1, int x2, int y2)
{
	return (los_general(x1, y1, x2, y2, map_stop_wall));
}


bool borg_los_pure(int x1, int y1, int x2, int y2)
{
	return (los_general(x1, y1, x2, y2, map_stop_wall_pure));
}


bool borg_bolt_los(int x1, int y1, int x2, int y2)
{
	return (los_general(x1, y1, x2, y2, map_stop_bolt));
}


bool borg_bolt_los_pure(int x1, int y1, int x2, int y2)
{
	return (los_general(x1, y1, x2, y2, map_stop_bolt_pure));
}



/* Slope and square used by mmove2 */
static int mmove_slope;
static int mmove_sq;

/* Direction to move in */
static int mmove_dx;
static int mmove_dy;


/*
 * Calculate the slope and square information used by
 * a following mmove2
 */
void borg_mmove_init(int x1, int y1, int x2, int y2)
{
	int temp;

	int xx, yy;

	int dx, dy, ax, ay, sx, sy, dist;

	map_block *mb_ptr;

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
		/* Look up the slope to use */
		mmove_slope = p_slope_min[ax][ay];

		while (mmove_slope <= p_slope_max[ax][ay])
		{
			xx = x1 + sx * project_data[mmove_slope][mmove_sq].x;
			yy = y1 + sy * project_data[mmove_slope][mmove_sq].y;

			/* Done? */
			if ((xx == x1 + dx) && (yy == y1 + dy)) break;

			/* Paranoia */
			if (!map_in_bounds(xx, yy)) break;

			mb_ptr = map_loc(xx, yy);

			/* Do we want to stop early? */
			if (!is_projectable && mb_ptr->monster) break;

			/* Is the square not occupied by a monster, and passable? */
			if (!borg_cave_los_grid(mb_ptr) || mb_ptr->monster)
			{
				/* Advance to the best position we have not looked at yet */
				temp = project_data[mmove_slope][mmove_sq].slope;
				mmove_sq = project_data[mmove_slope][mmove_sq].square;
				mmove_slope = temp;
			}
			else
			{
				/* Advance along ray */
				(mmove_sq)++;
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
		/* Look up the slope to use */
		mmove_slope = p_slope_min[ay][ax];

		while (mmove_slope <= p_slope_max[ay][ax])
		{
			/* Note that the data offsets have x,y swapped */
			xx = x1 + sx * project_data[mmove_slope][mmove_sq].y;
			yy = y1 + sy * project_data[mmove_slope][mmove_sq].x;

			/* Done? */
			if ((xx == x1 + dx) && (yy == y1 + dy)) break;

			/* Paranoia */
			if (!map_in_bounds(xx, yy)) break;

			mb_ptr = map_loc(xx, yy);

			/* Do we want to stop early? */
			if (!is_projectable && mb_ptr->monster) break;

			/* Is the square not occupied by a monster, and passable? */
			if (!cave_los_grid(mb_ptr) || mb_ptr->monster)
			{
				/* Advance to the best position we have not looked at yet */
				temp = project_data[mmove_slope][mmove_sq].slope;
				mmove_sq = project_data[mmove_slope][mmove_sq].square;
				mmove_slope = temp;
			}
			else
			{
				/* Advance along ray */
				(mmove_sq)++;
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
void borg_mmove(int *x, int *y, int x1, int y1)
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


/*
 * Hack - a function to pass to los_general() used
 * to do borg_projectable().
 * Hack -- we refuse to assume that unknown grids are floors
 */
static bool map_stop_project(map_block *mb_ptr)
{
	/* Unknown area is probably a wall */
	/* if (!mb_ptr->feat) return (TRUE); */

	/* Walls block projections */
	if (borg_cave_wall_grid(mb_ptr)) return (TRUE);

	/* Seems ok */
	return (FALSE);
}




/*
 * Check the projection from (x1,y1) to (x2,y2).
 * Assume that there is no monster in the way.
 * Hack -- we assume that unknown grids are floors
 * Adapted from "projectable()" in "spells1.c".
 */
bool borg_projectable(int x1, int y1, int x2, int y2)
{
	/* Are we projectable? */
	return (los_general(x1, y1, x2, y2, map_stop_project));
}


/*
 * This file is responsible for the "borg_update" routine, which is used
 * to notice changes in the world, and to keep track of terrain features,
 * objects, monsters, both from visual changes, and from world messages.
 *
 * One big thing this file does is "object/monster tracking", which
 * attempts to gather information about the objects and monsters in
 * the dungeon, including their identity, location, and state, and
 * to "follow" them if they "move", and to delete them when they die.
 *
 * Information about terrain is used to help plan "flow" paths.  Info
 * about objects and monsters is used to optimize planning paths to
 * those objects and monsters.  Info about monsters is also used for
 * the "danger" functions, which help avoid dangerous situations.
 *
 * Notes:
 *   We assume that monsters/objects can never appear in walls/doors
 *   We count the occurance of invisible or offscreen monsters
 *   We treat "mimics" and "trappers" as "invisible" monsters
 *
 * To Do:
 *
 * Bugs:
 *   The timestamps are not quite in sync properly (?)
 */

/*
 * Strategy flags -- recalculate things
 */

bool borg_danger_wipe = FALSE;	/* Recalculate danger */

/*
 * Hack -- message memory
 */

static s16b borg_msg_len;

static s16b borg_msg_siz;

static char *borg_msg_buf;

static s16b borg_msg_num;

static s16b borg_msg_max;

static s16b *borg_msg_pos;

static s16b *borg_msg_use;


/*
 * Hack -- help identify "unique" monster names
 */

static int borg_unique_size;	/* Number of uniques */
static s16b *borg_unique_what;	/* Indexes of uniques */
static cptr *borg_unique_text;	/* Names of uniques */

/*
 * Hack -- help identify "normal" monster names
 */

static int borg_normal_size;	/* Number of normals */
static s16b *borg_normal_what;	/* Indexes of normals */
static cptr *borg_normal_text;	/* Names of normals */


/*
 * Attempt to convert a monster name into a race index
 *
 * First we check for all possible "unique" monsters, including
 * ones we have killed, and even if the monster name is "prefixed"
 * (as in "The Tarrasque" and "The Lernean Hydra").  Since we use
 * a fast binary search, this is acceptable.
 *
 * Otherwise, if the monster is NOT named "The xxx", we assume it
 * must be a "player ghost" (which is impossible).
 *
 * Then, we do a binary search on all "normal" monster names, using
 * a search which is known to find the last matching entry, if one
 * exists, and otherwise to find an entry which would follow the
 * matching entry if there was one, unless the matching entry would
 * follow all the existing entries, in which case it will find the
 * final entry in the list.  Thus, we can search *backwards* from
 * the result of the search, and know that we will access all of
 * the matching entries, as long as we stop once we find an entry
 * which does not match, since this will catch all cases above.
 *
 * Finally, we assume the monster must be a "player ghost" (which
 * as noted above is impossible), which is a hack, but may prevent
 * crashes, even if it does induce strange behavior.
 */
static int borg_guess_race_name(cptr who)
{
	int k, m, n;

	int i, b_i = 0;
	int s, b_s = 0;

	monster_race *r_ptr;

	char partial[160];

	int suflen = 0, len = strlen(who);


	/* If the borg is hallucinating */
	if (bp_ptr->status.image)
	{
		/* Say so */
		borg_note("# Seeing a monster while hallucinating (%s)", who);

		/* The borg can't recognize monster when it is seeing funny things */
		return (0);
	}

	/* Hack -- handle "offscreen" */
	if (suffix(who, " (offscreen)")) suflen = 12;

	/* Handle mutations */
	if (bp_ptr->muta2 & MUT2_SCOR_TAIL &&
		suffix(who, " with your tail")) suflen = 16;
	else if (bp_ptr->muta2 & MUT2_HORNS &&
		suffix(who, " with your horns")) suflen = 17;
	else if (bp_ptr->muta2 & MUT2_BEAK &&
		suffix(who, " with your beak")) suflen = 16;
	else if (bp_ptr->muta2 & MUT2_TRUNK &&
		suffix(who, " with your trunk")) suflen = 17;
	else if (bp_ptr->muta2 & MUT2_TENTACLES &&
		suffix(who, " with your tentacles")) suflen = 21;

	/* Handle monk suffices */
	if (borg_class == CLASS_MONK)
	{
		if (suffix(who, " with your knee"))
		{
			if (suffix(who, " in the groin with your knee"))
				suflen = 28;
			else
				suflen = 15;
		}
		else if (suffix(who, " with your elbow")) suflen = 16;
		else if (suffix(who, " in the ankle")) suflen = 13;
		else if (suffix(who, " with a Cat's Claw")) suflen = 18;
		else if (suffix(who, " with a jump kick")) suflen = 17;
		else if (suffix(who, " with an Eagle's Claw")) suflen = 21;
		else if (suffix(who, " with a circle kick")) suflen = 19;
		else if (suffix(who, " with an Iron Fist")) suflen = 18;
		else if (suffix(who, " with a flying kick")) suflen = 19;
		else if (suffix(who, " with a Dragon Fist")) suflen = 19;
		else if (suffix(who, " with a Crushing Blow")) suflen = 21;
	}

	if (suflen)
	{
		/* Remove the suffix */
		strcpy(partial, who);
		partial[len - suflen] = '\0';
		who = partial;
	}

	/* Start the search */
	m = 0;
	n = borg_unique_size;

	/* Binary search */
	while (m < n - 1)
	{
		/* Pick a "middle" entry */
		i = (m + n) / 2;

		/* Search to the right (or here) */
		if (strcmp(borg_unique_text[i], who) <= 0)
		{
			m = i;
		}

		/* Search to the left */
		else
		{
			n = i;
		}
	}

	/* Check for equality */
	if (streq(who, borg_unique_text[m]))
	{
		/* Use this monster */
		return (borg_unique_what[m]);
	}

	/* Skip the prefix */
	if (prefix(who, "The ")) who += 4;
	else if (prefix(who, "Your ")) who += 5;

	/* Start the search */
	m = 0;
	n = borg_normal_size;

	/* Binary search */
	while (m < n - 1)
	{
		/* Pick a "middle" entry */
		i = (m + n) / 2;
		/* Search to the right (or here) */
		if (strcmp(borg_normal_text[i], who) <= 0)
		{
			m = i;
		}

		/* Search to the left */
		else
		{
			n = i;
		}
	}

	/* Scan possibilities */
	for (k = m; k >= 0; k--)
	{
		/* Stop when done */
		if (!streq(who, borg_normal_text[k])) break;

		/* Extract the monster */
		i = borg_normal_what[k];

		/* Access the monster */
		r_ptr = &r_info[i];

		/* Basic score */
		s = 1000;

		/* Penalize "depth miss" */
		s = s - ABS(r_ptr->level - bp_ptr->depth);

		/* Track best */
		if (b_i && (s < b_s)) continue;

		/* Track it */
		b_i = i;
		b_s = s;
	}

	/* Success */


	if (b_i) return (b_i);

	borg_oops("# Assuming unknown (%s)", who);
 
	/* Oops */
	return (0);
}


/*
 * Delete an old "object" record
 */
static void borg_delete_take(int i)
{
	borg_take *take = &borg_takes[i];

	map_block *mb_ptr;

	/* Paranoia -- Already wiped */
	if (!take->k_idx)
	{
		borg_oops("Deleting already gone object!");
		return;
	}

	/* Bounds checking */
	if (map_in_bounds(take->x, take->y))
	{
		mb_ptr = map_loc(take->x, take->y);

		/* Delete the 'take' value on grid. */
		mb_ptr->take = 0;
	}

	/* Note */
	borg_note("# Forgetting an object '%s' at (%d,%d)",
				  (k_name + k_info[take->k_idx].name), take->x, take->y);

	/* Kill the object */
	(void)WIPE(take, borg_take);

	/* One less object */
	borg_takes_cnt--;

	/* Wipe goals */
	goal = GOAL_NONE;
}


/*
 * Guess the kidx for an unknown item.
 */
static int borg_guess_kidx(char unknown)
{
	int i, b_i = -1;

	int s, b_s = 0;

	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip "empty" items */
		if (!k_ptr->name) continue;

		/* Skip identified items */
		if (k_ptr->aware) continue;

		/* Skip items with the wrong symbol */
		if (unknown != k_ptr->d_char) continue;

		/* Valueless items are boring */
		if (k_ptr->cost <= 0) continue;

		/* Base score */
		s = 10000;

		/* Hack -- penalize "extremely" out of depth */
		if (k_ptr->level > bp_ptr->depth + 50) s = s - 500;

		/* Hack -- penalize "very" out of depth */
		if (k_ptr->level > bp_ptr->depth + 15) s = s - 100;

		/* Hack -- penalize "rather" out of depth */
		if (k_ptr->level > bp_ptr->depth + 5) s = s - 50;

		/* Hack -- penalize "somewhat" out of depth */
		if (k_ptr->level > bp_ptr->depth) s = s - 10;

		/* Hack -- Penalize "depth miss" */
		s = s - ABS(k_ptr->level - bp_ptr->depth);

		/* Hack -- Penalize INSTA_ART items */
		if (FLAG(k_ptr, TR_INSTA_ART)) s = s - 1000;

		/* Desire "best" possible score */
		if (b_i && (s < b_s)) continue;

		/* Track it */
		b_i = i;
		b_s = s;
	}

	/* Found a match? */
	if (b_i != -1) return (b_i);

	/* Didn't find anything */
	borg_note("# Cannot guess object '%c'", unknown);

	return (1);
}


/*
 * Obtain a new "take" index
 */
static int borg_new_take(int k_idx, char unknown, int x, int y)
{
	int i, n = 0;

	borg_take *take;

	/* Handle unknown items */
	if (unknown) k_idx = borg_guess_kidx(unknown);

	/* Paranoia */
	if (!k_idx)
	{
		borg_oops("Cannot find object!");
		return (0);
	}

	/* Look for a "dead" object */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		/* Reuse "dead" objects */
		if (!borg_takes[i].k_idx)
		{
			n = i;
			break;
		}
	}

	/* Allocate a new object */
	if ((n == 0) && (borg_takes_nxt < BORG_TAKES_MAX))
	{
		/* Acquire the entry, advance */
		n = borg_takes_nxt++;
	}

	/* Hack -- steal an old object */
	if (n == 0)
	{
		/* Note */
		borg_note("# Too many objects");

		/* Hack -- Pick a random object */
		n = randint1(borg_takes_nxt - 1);

		/* Delete it */
		borg_delete_take(n);
	}

	/* Count new object */
	borg_takes_cnt++;

	/* Obtain the object */
	take = &borg_takes[n];

	/* Save the kind */
	take->k_idx = k_idx;
	take->unknown = unknown;

	/* Save the location */
	take->x = x;
	take->y = y;

	/* Note */
	borg_note("# Creating an object '%s' at (%d,%d)",
				  (k_name + k_info[take->k_idx].name), x, y);

	/* Wipe goals */
	goal = GOAL_NONE;

	/* Result */
	return (n);
}

/*
 * Remove objects that are out of bounds
 */
static void delete_dead_objects(void)
{
	int i;

	borg_take *bt_ptr;

	/* Look for a "dead" object */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		bt_ptr = &borg_takes[i];

		/* Is the object "alive"? */
		if (bt_ptr->k_idx)
		{
			/* Is it out of bounds? */
			if (!map_in_bounds(bt_ptr->x, bt_ptr->y))
			{
				/* Delete it */
				borg_delete_take(i);
			}
		}
	}
}


/*
 * Delete an old "kill" record
 */
void borg_delete_kill(int who, cptr reason)
{
	borg_kill *kill = &borg_kills[who];

	/* Paranoia -- Already wiped */
	if (!kill->r_idx) return;

	if (map_in_bounds(kill->x, kill->y))
	{
		map_block *mb_ptr = map_loc(kill->x, kill->y);

		if (mb_ptr->kill == who) mb_ptr->kill = 0;
	}

	/* Note */
	borg_note("# Removing a monster '%s' (%i) at (%d,%d) [%s]",
				  mon_race_name(&r_info[kill->r_idx]), who,
				  kill->x, kill->y, reason);

	/* Kill the monster */
	(void)WIPE(kill, borg_kill);

	/* One less monster */
	borg_kills_cnt--;

	/* Recalculate danger */
	borg_danger_wipe = TRUE;

	/* Wipe goals */
	goal = GOAL_NONE;
}

static int get_blank_kill(void)
{
	int i;

	/* Count the monsters */
	borg_kills_cnt++;

	/* Look for a "dead" monster */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		/* Find empty entries */
		if (!borg_kills[i].r_idx) return (i);
	}

	/* Allocate a new monster */
	if (borg_kills_nxt < BORG_KILLS_MAX)
	{
		/* Acquire the entry, advance */
		return (borg_kills_nxt++);
	}

	/* Hack -- steal an old monster */
	borg_note("# Too many monsters");

	/* Hack -- Pick a random monster */
	i = randint1(borg_kills_nxt - 1);

	/* Kill it */
	borg_delete_kill(i, "overflow");

	return (i);
}

/*
 * Merge an old "kill" record
 */
static void borg_merge_kill(int who)
{
	borg_kill *kill = &borg_kills[who];

	/* Paranoia -- Already wiped */
	if (!kill->r_idx) return;

	if (map_in_bounds(kill->x, kill->y))
	{
		map_block *mb_ptr = map_loc(kill->x, kill->y);

		if (mb_ptr->kill == who) mb_ptr->kill = 0;
	}

	/* Kill the monster */
	(void)WIPE(kill, borg_kill);

	/* One less monster */
	borg_kills_cnt--;
}


/*
 * Remove all monsters of a given type
 */
static void borg_wipe_mon(byte type)
{
	int i;

	borg_kill *kill;

	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];

		if (kill->type == type)
		{
			borg_note("Destroying kill %d in MOVED list in wipe_mon", i);

			borg_merge_kill(i);
		}
	}
}


/*
 * Change all the kills of type two
 * to be type one.
 */
static void borg_append_mon_list(byte type1, byte type2)
{
	int i;

	borg_kill *kill;

	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];

		/* Paranoia */
		if (!kill->r_idx) continue;

		/* Add kills of type2 to type1 */
		if (kill->type == type2)
		{
			kill->type = type1;
		}
	}
}


/*
 * Hack -- Update a "new" monster
 */
static void borg_update_kill(int i)
{
	int t, e;

	borg_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];

	map_block *mb_ptr;

	/* Player energy per game turn */
	e = extract_energy[bp_ptr->speed];

	/* Game turns per player move */
	t = (100 + (e - 1)) / e;

	/* Monster energy per game turn */
	e = extract_energy[r_ptr->speed];

	/* Assume no ranged attacks */
	kill->ranged_attack = FALSE;

	/* Can it attack from a distance? */
	if (r_ptr->flags[3] || r_ptr->flags[4] || r_ptr->flags[5])
	{
		kill->ranged_attack = TRUE;
	}

	/* Get the default power */
	kill->power = r_ptr->hdice * r_ptr->hside;

	if (map_in_bounds(kill->x, kill->y))
	{
		/* Get grid */
		mb_ptr = map_loc(kill->x, kill->y);

		/* Do we know about this monster? */
		if (mb_ptr->monster == kill->r_idx)
		{
			/* Use the known hp */
			kill->power = kill->power * mb_ptr->m_hp / 10;

			/* Save the flags */
			kill->m_flags = mb_ptr->m_flags;
		}
	}
}


/*
 * Obtain a new "kill" index
 */
static void borg_new_kill(int r_idx, int n, int x, int y)
{
	borg_kill *kill = &borg_kills[n];

	/* Save the race */
	kill->r_idx = r_idx;

	/* Location */
	kill->x = x;
	kill->y = y;

	/* Timestamp */
	kill->when = borg_t;

	/* Update the monster */
	borg_update_kill(n);

	/* Note */
	borg_note("# Creating a monster '%s' (%d) at (%d, %d)",
				  mon_race_name(&r_info[kill->r_idx]), n, x, y);

	/* Recalculate danger */
	borg_danger_wipe = TRUE;

	/* Wipe goals */
	goal = GOAL_NONE;

	/* Done */
	return;
}


/*
 * Force sleep onto a "kill" record
 * ??? Since this check is done at update_kill should I have it here?
 */
static void borg_sleep_kill(void)
{
	/* Recalculate danger */
	borg_danger_wipe = TRUE;
}


/*
 * Determine if a monster should be "viewable"
 */
static bool borg_follow_kill_aux(int i, int x, int y)
{
	int d;

	map_block *mb_ptr;

	borg_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];


	/* Distance to player */
	d = distance(c_y, c_x, y, x);

	/* Too far away */
	if (d > MAX_SIGHT) return (FALSE);

	/* Access the grid */
	mb_ptr = map_loc(x, y);

	/* Line of sight */
	if (mb_ptr->info & BORG_MAP_VIEW)
	{
		/* Use "illumination" */
		if (mb_ptr->flags & MAP_SEEN)
		{
			/* We can see invisible */
			if ((FLAG(bp_ptr, TR_SEE_INVIS)) || borg_see_inv) return (TRUE);

			/* Monster is not invisible */
			if (!(FLAG(r_ptr, RF_INVISIBLE))) return (TRUE);
		}

		/* Use "infravision" */
		if (d <= bp_ptr->see_infra)
		{
			/* Infravision works on "warm" creatures */
			if (!(FLAG(r_ptr, RF_COLD_BLOOD))) return (TRUE);
		}
	}


	/* Telepathy requires "telepathy" */
	if (FLAG(bp_ptr, TR_TELEPATHY))
	{
		/* Telepathy fails on "strange" monsters */
		if (FLAG(r_ptr, RF_EMPTY_MIND)) return (FALSE);
		if (FLAG(r_ptr, RF_WEIRD_MIND)) return (FALSE);

		/* Success */
		return (TRUE);
	}


	/* Nope */
	return (FALSE);
}


/*
 * Attempt to track monsters from one
 * square to another.
 *
 * This is a quadratic algorithm, but may be fast enough...
 */
static void observe_kill_move(int new_type, int old_type, int dist)
{
	int i, j;
	borg_kill *kill1, *kill2;
	map_block *mb_ptr1, *mb_ptr2;

	int x, y, d;

	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill1 = &borg_kills[i];

		/* Paranoia - ignore dead monsters */
		if (!kill1->r_idx) continue;

		/* Must be correct type */
		if (kill1->type != old_type) continue;

		x = kill1->x;
		y = kill1->y;

		/* Check the bounds */
		if (!map_in_bounds(x, y)) continue;

		mb_ptr1 = map_loc(x, y);

		for (j = 1; j < borg_kills_nxt; j++)
		{
			kill2 = &borg_kills[j];

			/* Don't use self */
			if (i == j) continue;

			/* Paranoia - ignore dead monsters */
			if (!kill2->r_idx) continue;

			/* Must be correct type */
			if (kill2->type != new_type) continue;

			/* Must be same race */
			if (kill2->r_idx != kill1->r_idx) continue;

			/* Calculate distance */
			d = distance(x, y, kill2->x, kill2->y);

			/* Too far away */
			if (d > dist) continue;

			/* Check the bounds */
			if (!map_in_bounds(kill2->x, kill2->y)) continue;

			mb_ptr2 = map_loc(kill2->x, kill2->y);

			/* Note */
			borg_note
				("# Tracking monster (%d) from (%d,%d) to (%d) (%d,%d)", i, x,
				 y, j, kill2->x, kill2->y);

			/* Change the location of the old one */
			kill1->x = kill2->x;
			kill1->y = kill2->y;

			/* Remove the new monster */
			borg_merge_kill(j);

			/* remove overwritten index from the old map position */
			if (mb_ptr1->kill == i)	mb_ptr1->kill = 0;

			/* Put the index on the new map position */
			mb_ptr2->kill = i;

			/* Save timestamp */
			kill1->when = borg_t;

			/* Update the monster */
			borg_update_kill(i);

			/* Recalculate danger */
			borg_danger_wipe = TRUE;

			/* Clear goals */
			if (!(FLAG(bp_ptr, TR_TELEPATHY)) && (goal == GOAL_TAKE))
			{
				goal = GOAL_NONE;
			}
		}
	}
}

static bool remove_bad_kills(u16b who)
{
	int ox, oy;

	borg_kill *kill = &borg_kills[who];

	ox = kill->x;
	oy = kill->y;

	/* Monster is out of bounds */
	if (!map_in_bounds(ox, oy))
	{
		borg_delete_kill(who, "out of bounds");
		return (TRUE);
	}

	/* Is the monster underneith us? */
	if ((c_x == ox) && (c_y == oy))
	{
		borg_delete_kill(who, "where'd it go?");
		return (TRUE);
	}

	/* Are we supposed to see this, but don't? */
	if (borg_follow_kill_aux(who, ox, oy) &&
		(map_loc(ox, oy)->monster != kill->r_idx))
	{
		char buf[100];

		/* Check again because BORG_MAP_VIEW != Line of Sight */
		if (distance(c_x, c_y, ox, oy) < MAX_SIGHT &&
			borg_los_pure(c_x, c_y, ox, oy))				  
		{
			(void)strnfmt(buf, 100, "vanished : %d, %d", map_loc(ox, oy)->monster,
						  kill->r_idx);

			borg_delete_kill(who, buf);
			return (TRUE);
		}
	}

	/* We haven't seen it for ages? */
	if (borg_t - kill->when > 2000)
	{
		borg_delete_kill(who, "expired");
		return (TRUE);
	}

	/* Did not remove monster */
	return (FALSE);
}

/*
 * Track remaining unaccounted for monsters
 */
static void handle_old_mons(byte type)
{
	u16b i;

	borg_kill *kill;

	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];

		/* Paranoia - ignore dead monsters */
		if (!kill->r_idx) continue;

		/* Must be of correct type */
		if (kill->type != type) continue;

		if (remove_bad_kills(i)) continue;

		/* Move the old monster to the used list */
		kill->type = BORG_MON_USED;
	}
}


/*
 * Attempt to locate a monster which could explain a message involving
 * the given monster name, near the given location, up to the given
 * distance from the given location.
 *
 * Invisible monsters, bizarre monsters, and unexplainable monsters are
 * all treated the same way, and should eventually contribute some amount
 * of basic "fear" to the current region.
 *
 * First, we attempt to convert "similar" objects into the desired monster,
 * then we attempt to convert "similar" monsters into the desired monster,
 * then we attempt to match an existing monster, and finally, we give up.
 *
 * XXX XXX XXX Hack -- To prevent fatal situations, every time we think
 * there may be a monster nearby, we look for a nearby object which could
 * be the indicated monster, and convert it into that monster.  This allows
 * us to correctly handle a room full of multiplying clear mushrooms.
 *
 * XXX XXX XXX When surrounded by multiple monsters of the same type,
 * we will ascribe most messages to one of those monsters, and ignore
 * the existance of all the other similar monsters.
 *
 * XXX XXX XXX Currently, confusion may cause messages to be ignored.
 */
static int borg_locate_kill(cptr who, int x, int y, int r)
{
	int i, d, r_idx;

	int b_i, b_d;

	borg_kill *kill;

	monster_race *r_ptr;

	/* Handle invisible monsters */
	if (streq(who, "It") || streq(who, "Someone") || streq(who, "Something"))
	{
		/* Note */
		borg_note("# Possible Invisible monster nearby.");

		/*
		 * If I can, cast detect inviso--time stamp it
		 * We stamp it now if we can, or later if we just did the spell
		 * That way we dont loop casting the spell.    APW
		 */
		if (need_see_inviso < borg_t)
		{
			need_see_inviso = borg_t;
		}

		/* Ignore */
		return (0);
	}

	/* Guess the monster race */
	r_idx = borg_guess_race_name(who);

	/* Paranoia */
	if (!r_idx)
	{
		/* Note */
		borg_note("# Possible strange monster nearby.");

		/* Ignore */
		return (0);
	}

	/* Access the monster race */
	r_ptr = &r_info[r_idx];

	/* Note */
	borg_note("# There is a monster '%s' within %d grids of %d,%d",
				  mon_race_name(r_ptr), r, x, y);


	/* Handle trappers and lurkers and mimics */
	if (FLAG(r_ptr, RF_CHAR_CLEAR) || FLAG(r_ptr, RF_CHAR_MIMIC))
	{
		/* Note */
		borg_note("# Bizarre monster nearby");
	}

	/*** Hack -- Find an existing monster ***/

	/* Nothing yet */
	b_i = -1;
	b_d = 999;

	/* Scan the monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];

		/* Skip "dead" monsters */
		if (!kill->r_idx) continue;

		/* Skip "different" monsters */
		if (kill->r_idx != r_idx) continue;

		/* Distance away */
		d = distance(kill->x, kill->y, x, y);

		/* In a darkened room with ESP we can get hit and ignore it */
		/* Check distance */
		if (d > r + 1) continue;

		/* Hopefully this will add fear to our grid */
		if (!borg_projectable(kill->x, kill->y, x, y)) continue;

		/* Track closest one */
		if (d > b_d) continue;

		/* Track it */
		b_i = i;
		b_d = d;
	}

	/* Found one */
	if (b_i >= 0)
	{
		kill = &borg_kills[b_i];

		/* Note */
		borg_note("# Matched a monster '%s' at (%d,%d)",
					  mon_race_name(&r_info[kill->r_idx]), kill->x, kill->y);


		/* Index */
		return (b_i);
	}


	/*** Oops ***/

	/* Note */
	borg_note("# Ignoring a monster '%s' near (%d,%d)",
				  mon_race_name(r_ptr), x, y);

	/* Oops */
	/* this is the case where we know the name of the monster */
	/* but cannot locate it on the monster list. */
	return (-1);
}


/*
 * Notice the "death" of a monster
 */
static void borg_count_death(cptr what)
{
	int r_idx;

	/* Handle invisible monsters */
	if (streq(what, "It") || streq(what, "Someone") || streq(what, "Something"))
	{
		/* Ignore */
		return;
	}

	/* Guess the monster race */
	r_idx = borg_guess_race_name(what);

	/* Paranoia */
	if (!r_idx) return;

	if (FLAG(&r_info[r_idx], RF_UNIQUE))
	{
		/* Reset unique on level flag */
		unique_on_level = FALSE;
	}
}


/* Mark detected region */
static void detect_region(byte flag)
{
	int x, y;

	map_block *mb_ptr;

	for (x = c_x - BORG_MAX_DETECT; x <= c_x + BORG_MAX_DETECT; x++)
	{
		for (y = c_y - BORG_MAX_DETECT; y <= c_y + BORG_MAX_DETECT; y++)
		{
			/* bounds checking */
			if (!map_in_bounds(x, y)) continue;

			/* Check distance */
			if (distance(c_x, c_y, x, y) > BORG_MAX_DETECT) continue;

			mb_ptr = map_loc(x, y);

			/* Set flag */
			mb_ptr->detect |= flag;
		}
	}
}

/*
 * Handle "detection" spells and "call lite"
 *
 * Note that we must use the "old" player location
 */
static bool borg_handle_self(cptr str)
{
	/* Handle failure */
	if (borg_failure)
	{
		borg_note("# Something failed");
	}

	/* Handle "call lite" */
	else if (prefix(str, "lite"))
	{
		/* Message */
		borg_note("# Called lite");
	}

	/* Handle "detect walls" */
	else if (prefix(str, "wall"))
	{
		/* Message */
		borg_note("# Detected walls");

		/* Mark detected walls */
		detect_region(BORG_DETECT_WALL);
	}

	/* Handle "detect traps" */
	else if (prefix(str, "trap"))
	{
		/* Message */
		borg_note("# Detected traps");

		/* Mark detected traps */
		detect_region(BORG_DETECT_TRAP);
	}

	/* Handle "detect doors" */
	else if (prefix(str, "door"))
	{
		/* Message */
		borg_note("# Detected doors");

		/* Mark detected doors */
		detect_region(BORG_DETECT_DOOR);
	}

	/* Handle "detect traps and doors" */
	else if (prefix(str, "both"))
	{
		/* Message */
		borg_note("# Detected traps and doors");

		/* Mark detected traps + doors */
		detect_region(BORG_DETECT_TRAP | BORG_DETECT_DOOR);
	}

	/* Handle "detect traps and doors and evil" */
	else if (prefix(str, "TDE"))
	{
		/* Message */
		borg_note("# Detected traps, doors & evil");

		/* Mark detected traps + doors + evil */
		detect_region(BORG_DETECT_TRAP | BORG_DETECT_DOOR | BORG_DETECT_EVIL);
	}

	/* Handle "detect evil" */
	else if (prefix(str, "evil"))
	{
		/* Message */
		borg_note("# Detected evil");

		/* Mark detected evil */
		detect_region(BORG_DETECT_EVIL);
	}

	/* Done */
	return (TRUE);
}


/* Try to add a town to the town list */
int borg_add_town(int x, int y, cptr town_name)
{
	int i;

	/* Not quite a town */
	if (prefix(town_name, "Bottom") ||
		strstr(town_name, "0 ft") ||
		prefix(town_name, "Lev "))
	{
		borg_oops("Trying to add %s as a town!", town_name);
		return (-1);
	}

	/* Loop through the towns */
	for (i = 0; i < borg_town_num; i++)
	{
		/* Find the matching town */
		if (streq(borg_towns[i].name, town_name)) break;
	}

	/* No need to duplicate towns */
	if (i < borg_town_num) return (i);

	/* Do we need to increase the size of the town array? */
	if (borg_town_num == borg_town_size)
	{
		borg_town *temp;

		/* Double size of arrays */
		borg_town_size *= 2;

		/* Make new (bigger) array */
		C_MAKE(temp, borg_town_size, borg_town);

		/* Copy into new array */
		C_COPY(temp, borg_towns, borg_town_num, borg_town);

		/* Get rid of old array */
		FREE(borg_towns);

		/* Use new array */
		borg_towns = temp;
	}

	/* Add this town */
	borg_towns[i].x = x;
	borg_towns[i].y = y;
	strncpy(borg_towns[i].name, town_name, strlen(town_name));

	borg_note("# Adding town = %s, x, = %d, y = %d", town_name, x, y);

	/* extend the list */
	borg_town_num += 1;

	return (i);
}


/* Find out the current towns name, add it to the array, return its index */
static int borg_add_town_screen(int x, int y)
{
	int wid, hgt;
	byte t_a;
	char buf[T_NAME_LEN];
	int count = 0;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Pick up town name */
	if (0 != borg_what_text(wid - T_NAME_LEN,
							hgt - 1,
							T_NAME_LEN - 1, &t_a, buf))
	{
		/* Failed to read off the screen */
		return (-1);
	}

	/* Is the borg somewhere in the wilderness? */
	if (prefix(buf, "Wilderness")) return (-1);

	/* Is the borg somewhere next to a dungeon? */
	if (prefix(buf, "Dungeon")) return (-1);

	/* Is the borg somewhere next to a dungeon with a quest? */
	if (prefix(buf, "Ruin")) return (-1);

	/* Is this a dungeon? */
	if (prefix(buf, "Bottom ") ||
		prefix(buf, "Lev ") ||
		strstr(buf, "0 ft")) return (-1);

	/* Discard leading spaces */
	while (buf[count] == ' ') count++;

	/* Try to add this town */
	return (borg_add_town(x, y, buf + count));
}


/* Add a dungeon to the array, overwrite an old one if it was an estimate */
void borg_add_dungeon(int x, int y, int min_depth, int max_depth, bool bottom)
{
	int i, b_i = 0;
	int d, b_d = BORG_MAX_DISTANCE;

	/* Do we need to increase the size of the dungeon array? */
	if (borg_dungeon_num == borg_dungeon_size)
	{
		borg_dungeon *temp;

		/* Double size of arrays */
		borg_dungeon_size *= 2;

		/* Make new (bigger) array */
		C_MAKE(temp, borg_dungeon_size, borg_dungeon);

		/* Copy into new array */
		C_COPY(temp, borg_dungeons, borg_dungeon_num, borg_dungeon);

		/* Get rid of old array */
		FREE(borg_dungeons);

		/* Use new array */
		borg_dungeons = temp;
	}


	/* Find closest dungeon */
	for (i = 0; i < borg_dungeon_num; i++)
	{
		/*
		 * There is a wilderness dungeon that has three stairs
		 * that all lead to the same dungeon.  Pick only one to
		 * avoid confusion.
		 */
		if ((borg_dungeons[i].x == x ||
			ABS(borg_dungeons[i].x - x) == 14) &&
			(borg_dungeons[i].y == y ||
			ABS(borg_dungeons[i].y - y) == 30)) return;

		/* Get the distance */
		d = distance(x, y, borg_dungeons[i].x, borg_dungeons[i].y);

		/* Is it closer? */
		if (d >= b_d) continue;

		/* remember */
		b_i = i;
		b_d = d;
	}

	/* It is close */
	if (b_d > 6 * WILD_BLOCK_SIZE) b_i = borg_dungeon_num++;

	/* The dungeon entrance is seen or spotted for the first time ever */
	if (!min_depth || !borg_dungeons[b_i].x)
	{
		/* Assign coordinates */
		borg_dungeons[b_i].x = x;
		borg_dungeons[b_i].y = y;

		/* Tell the player */
		borg_note("# Adding a %d dungeon at (%d, %d), min, max = %d, %d",
				 (min_depth + 9) / 10, x, y, min_depth, max_depth);
	}

	/* Add depth if it was given */
	if (min_depth)
	{
		/* Assign depth */
		borg_dungeons[b_i].min_depth = min_depth;
		borg_dungeons[b_i].max_depth = max_depth;
		borg_dungeons[b_i].bottom = bottom;
	}

}


/*
 * Save the borg information into the overhead map
 */
void borg_map_info(map_block *mb_ptr, const term_map *map, vptr dummy)
{
	int i;

	int x = map->x;
	int y = map->y;

	bool old_wall;
	bool new_wall;
	
	/* Hack - ignore parameter */
	(void) dummy;

	/* Don't do anything if the borg is inactive */
	if (!borg_active)
	{
		/* Done */
		return;
	}

	/* Save the old "wall" or "door" */
	old_wall = borg_cave_wall_grid(mb_ptr);

	/* Don't overwrite known info with unknown */
	if (map->terrain) mb_ptr->feat = map->terrain;

	/*
	 * Examine monsters
	 */
	if (map->monster)
	{
		borg_kill *kill;

		/* Is the monster known and not new? */
		if (mb_ptr->kill && (map->monster == mb_ptr->monster))
		{
			kill = &borg_kills[mb_ptr->kill];

			if (kill->type != BORG_MON_NEW)
			{
				/* Remove it from the old list. */
				kill->type = BORG_MON_USED;
			}
		}
		else
		{
			/* Is it a new monster? */
			if (mb_ptr->kill)
			{
				kill = &borg_kills[mb_ptr->kill];

				/* Move old entry into "moved" list */
				kill->type = BORG_MON_MOVE;
			}

			/* Get new kill */
			mb_ptr->kill = get_blank_kill();

			kill = &borg_kills[mb_ptr->kill];

			/* Set type */
			kill->type = BORG_MON_NEW;

			/* Fill in information for new monster */
			borg_new_kill(map->monster, mb_ptr->kill, x, y);
		}
	}
	else
	{
		if (mb_ptr->kill && (map->flags & MAP_SEEN))
		{
			/* Check */
			borg_kill *kill = &borg_kills[mb_ptr->kill];

			if (kill->type != BORG_MON_NEW)
			{
				if ((kill->x == x) && (kill->y == y))
				{
					/*
					 * We need to remove this from the list,
					 * it must have moved.
					 */
					kill->type = BORG_MON_MOVE;
				}
				else
				{
					borg_note("Strange kill %d at (%d,%d)", mb_ptr->kill,
								  kill->x, kill->y);
				}
			}

			/* Clear it */
			mb_ptr->kill = 0;
		}
	}

	/*
	 * Examine objects
	 */
	if (map->object || map->unknown)
	{
		/* Do we already know about this object? */
		if (mb_ptr->take)
		{
			borg_take *bt_ptr = &borg_takes[mb_ptr->take];

			if ((bt_ptr->unknown != map->unknown) ||
				((bt_ptr->k_idx != map->object) && !map->unknown))
			{
				borg_note("# The object %d is different! (%d,%d)",
							  mb_ptr->take, bt_ptr->k_idx, map->object);

				/* The object is different- delete it */
				borg_delete_take(mb_ptr->take);

				/* Make a new object */
				mb_ptr->take = borg_new_take(map->object, map->unknown, x, y);
			}
		}
		else
		{
			/* Make a new object */
			mb_ptr->take = borg_new_take(map->object, map->unknown, x, y);
		}
	}
	else
	{
		/* Do we think there is an object here that we cannot see? */
		if (mb_ptr->take && (map->flags & MAP_SEEN))
		{
			borg_note("# Removing missing object (%d)", mb_ptr->take);

			/* The object is no longer here - delete it */
			borg_delete_take(mb_ptr->take);
		}
	}

	/* Analyze terrain */
	switch (mb_ptr->feat)
	{
			/* Up stairs */
		case FEAT_LESS:
		{
			/* Check for an existing "up stairs" */
			for (i = 0; i < track_less_num; i++)
			{
				/* Stop if we already new about these stairs */
				if ((track_less_x[i] == x) && (track_less_y[i] == y)) break;
			}

			/* Track the newly discovered "up stairs" */
			if ((i == track_less_num) && (i < track_less_size))
			{
				track_less_x[i] = x;
				track_less_y[i] = y;
				track_less_num++;
			}
			/* Done */
			break;
		}

			/* Down stairs */
		case FEAT_MORE:
		{
			/* Check for an existing "down stairs" */
			for (i = 0; i < track_more_num; i++)
			{
				/* We already knew about that one */
				if ((track_more_x[i] == x) && (track_more_y[i] == y)) break;
			}

			/* Track the newly discovered "down stairs" */
			if ((i == track_more_num) && (i < track_more_size))
			{
				track_more_x[i] = x;
				track_more_y[i] = y;
				track_more_num++;
			}

			/* Hack!  use p_ptr instead of bp because bp is not yet updated */
			if (p_ptr->depth == 0)
			{
				/* Add this dungeon maybe */
				borg_add_dungeon(x, y, 0, 0, FALSE);
			}

			/* Done */
			break;
		}
	}

	if (map->field)
	{
		/* Get field type */
		field_thaum *t_ptr = &t_info[map->field];

		/* Is it a store or building? */
		if (t_ptr->type == FTYPE_BUILD)
		{
			/* Check for an existing shop */
			for (i = 0; i < borg_shop_num; i++)
			{
				/* Stop if we already knew about this shop */
				if ((borg_shops[i].x == x) && (borg_shops[i].y == y)) break;
			}

			/* Do we need to increase the size of the shop array? */
			if (i == borg_shop_size)
			{
				borg_shop *temp;

				/* Double size of arrays */
				borg_shop_size *= 2;

				/* Make new (bigger) array */
				C_MAKE(temp, borg_shop_size, borg_shop);

				/* Copy into new array */
				C_COPY(temp, borg_shops, borg_shop_num, borg_shop);

				/* Get rid of old array */
				FREE(borg_shops);

				/* Use new array */
				borg_shops = temp;
			}

			/* Track the newly discovered shop */
			if (i == borg_shop_num)
			{
				/* Position */
				borg_shops[i].x = x;
				borg_shops[i].y = y;

				borg_shops[i].town_num = borg_add_town_screen(x, y);

				/* Catch all the funny buildings */
				if (streq(t_ptr->name, "Weaponmaster"))
				{
					borg_shops[i].type = BUILD_WEAPONMASTER;
				}
				else if (streq(t_ptr->name, "Zymurgist"))
				{
					borg_shops[i].type = BUILD_RECHARGE;
				}
				else if (streq(t_ptr->name, "Magesmith (weapons)"))
				{
					borg_shops[i].type = BUILD_PLUS_WEAPON;
				}
				else if (streq(t_ptr->name, "Magesmith (armor)"))
				{
					borg_shops[i].type = BUILD_PLUS_ARMOUR;
				}
				else if (streq(t_ptr->name, "Mutatalist"))
				{
					borg_shops[i].type = BUILD_MUTATE;
				}
				else if (streq(t_ptr->name, "Map Maker"))
				{
					borg_shops[i].type = BUILD_MAP;
				}
				else if (streq(t_ptr->name, "Library"))
				{
					borg_shops[i].type = BUILD_LIBRARY;
				}
				else if (streq(t_ptr->name, "Casino"))
				{
					borg_shops[i].type = BUILD_CASINO;
				}
				else if (streq(t_ptr->name, "Inn"))
				{
					borg_shops[i].type = BUILD_INN;
				}
				else if (streq(t_ptr->name, "Healer"))
				{
					borg_shops[i].type = BUILD_HEALER;
				}
				else if (streq(t_ptr->name, "Magetower"))
				{
					borg_shops[i].type = BUILD_MAGETOWER0;
				}
				else if (streq(t_ptr->name, "Large Magetower"))
				{
					borg_shops[i].type = BUILD_MAGETOWER1;
				}
				else if (streq(t_ptr->name, "Small Castle"))
				{
					borg_shops[i].type = BUILD_CASTLE0;
				}
				else if (streq(t_ptr->name, "Large Castle"))
				{
					borg_shops[i].type = BUILD_CASTLE1;
				}

				/* Hack - we have never been here before */
				borg_shops[i].when = borg_t - 1000;

				/* One more shop */
				borg_shop_num++;
			}
		}
		
		/* MT - Handle adding of traps to the borg_traps array */
		else if(t_ptr->type == FTYPE_TRAP)
		{
			mb_ptr->trap = map->field;
		}
		/* MT - Handle Glyphs */
		else if(t_ptr->type == FTYPE_FIELD)
		{
			mb_ptr->m_effect = map->field;
		}
	}

	/* 
	 * Save the new "wall" or "door"
	 *
	 * Hack - use inline form of borg_cave_wall_grid macro
	 */
	new_wall = (f_info[map->terrain].flags & FF_BLOCK);

	/* Notice wall changes */
	if (old_wall != new_wall)
	{
		/* Remove this grid from any flow */
		if (new_wall) mb_ptr->flow = 255;

		/* Remove this grid from any flow */
		mb_ptr->info &= ~(BORG_MAP_ICKY | BORG_MAP_KNOW);
	}
}


/*
 * Save the borg information into the overhead map
 */
void borg_map_erase(vptr dummy)
{
	/* Hack -ignore parameter */
	(void) dummy;

	/* Forget the view */
	borg_forget_view();

	/* No objects here */
	borg_takes_cnt = 0;
	borg_takes_nxt = 1;

	/* Forget old objects */
	(void)C_WIPE(borg_takes, BORG_TAKES_MAX, borg_take);

	/* Forget old monsters */
	(void)C_WIPE(borg_kills, BORG_KILLS_MAX, borg_kill);
}


/*
 * Update the "map" based on visual info on the screen
 *
 * Note that we make assumptions about the grid under the player,
 * to prevent painful situations such as seeming to be standing
 * in a wall, or on a trap, etc.
 *
 * In general, we use the same "feat" codes as the game itself, but
 * sometimes we are just guessing (as with "visible traps"), and we
 * use some special codes, explained below.
 *
 * Note that we use the "feat" code of "FEAT_NONE" for grids which
 * have never been seen, or which, when seen, have always contained
 * an object or monster.  These grids are probably walls, unless
 * they contain a monster or object, in which case they are probably
 * floors, unless they contain a monster which passes through walls,
 * in which case they are probably walls.
 *
 * Note that we use the "feat" code of "FEAT_FLOOR" for grids which
 * were a normal floor last time we checked.  These grids may have
 * changed into non-floor grids recently (via earthquake?), unless
 * the grid is on the current panel, and is currently "lit" in some
 * manner, and does not contain a monster.
 *
 * Note that we use the other "feat" codes for grids which probably
 * contain the given feature type, unless several feature types use
 * the same symbol, in which case we use some "default" code, changing
 * our guess when messages provide us with more information.  This is
 * especially necessary for distinguishing magma from quartz, and for
 * distinguishing normal doors from locked doors from jammed doors.
 * Note that most "feat" codes, even if they are not "guesses", may
 * not be valid unless the grid is on the current panel.
 *
 * Note the "interesting" code used to learn which floor grids are "dark"
 * and which are "perma-lit", by tracking those floor grids which appear
 * to be "lit", and then marking all of these grids which do not appear
 * to be lit by the torch as "known" to be illuminated, and by marking
 * any grids which "disappear" or which are displayed as "dark floors"
 * as "known" to be "dark".  This leaves many grids, especially those
 * lit by the torch, as being neither lit nor dark.
 *
 * The basic problem is that, especially with no special options set,
 * the player has very little direct information about which grids
 * are perma-lit, since all non-floor grids are memorized when they
 * are seen, and torch-lit floor grids look just like perma-lit
 * floor grids.  Also, monsters hide any object or feature in their
 * grid, and objects hide any feature in their grid, and objects are
 * memorized when they are seen, and monsters can be detected by a
 * variety of methods, including infravision and telepathy.
 *
 * Note that we assume that normally, when the player steps onto
 * something, it disappears, and turns into a normal floor, unless
 * the player is stepping onto a grid which is normally "permanent"
 * (floors, stairs, store doors), in which case it does not change.
 *
 * Note that when we encounter a grid which blocks motion, but which
 * was previously thought to not block motion, we must be sure to
 * remove it from any "flow" which might be in progress, to prevent
 * nasty situations in which we attempt to flow into a wall grid
 * which was thought to be something else, like an unknown grid.
 *
 * In Zborg, the wilderness level shops and stairs are
 * handled in borg_update() under new levels.
 */
static void borg_update_map(void)
{
	int i;

	/* Mark the player grid as having been stepped on */
	track_step_x[track_step_num] = c_x;
	track_step_y[track_step_num] = c_y;
	track_step_num++;

	/* Hack - Clean the steps every so often */
	if (track_step_num > 75)
	{
		for (i = 0; i < 75; i++)
		{
			/* Move each step down one position */
			track_step_x[i] = track_step_x[i + 1];
			track_step_y[i] = track_step_y[i + 1];
		}
		/* reset the count */
		track_step_num = 75;
	}
}

/*
 * Increase the "region danger"
 */
static void borg_fear_grid(cptr who, int x, int y, uint k, bool seen_guy)
{
	int i, j;

	map_block *mb_ptr;

	/* Reduce fear if GOI is on */
	if (borg_goi)
	{
		k = k / 3;
	}

	/* Messages */
	if (seen_guy)
	{
		borg_note("#   Fearing region value %d.", k);
	}
	else
	{
		borg_note("# Fearing grid (%d,%d) value %d because of a non-LOS %s", x,
			 y, k, who);
	}

	/* Current region */

	for (i = -15; i < 15; i++)
	{
		for (j = -15; j < 15; j++)
		{
			if (!map_in_bounds(i + x, j + y)) continue;

			mb_ptr = map_loc(i + x, j + y);

			/* Add some fear */
			mb_ptr->fear += 2 * k / (8 + ABS(i) + ABS(j));
		}
	}


	/*
	 * There is some problems here, when the death
	 * of a monster decreases the fear, it ends up making
	 * about 2000 danger pts.  It needs to be fixed.
	 */
}


/*
 * Calculate base danger from a spell attack by an invisible monster
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 */
static int borg_fear_spell(int i)
{
	int z = 0;
	int p = 0;

	int ouch = 0;


	/* Damage taken */
	if (bp_ptr->oldhp > bp_ptr->chp) ouch = (bp_ptr->oldhp - bp_ptr->chp) * 2;


	/* Check the spell */
	switch (i)
	{
		case 0:
		{
			/* RF3_SHRIEK */
			p += 10;
			break;
		}

		case 1:
		{
			/* RF3_FAILED spell by monster.  Fear it! */
			/* It could be a unique like Azriel */
			p += bp_ptr->depth;
			break;
		}

		case 2:
		{
			/* RF3_XXX3X4 */
			break;
		}

		case 3:
		{
			/* RF3_XXX4X4 */
			break;
		}

		case 4:
		{
			/* RF3_ARROW */
			/* XXX FIXME this depends on the monster */
			z = (4 * 6);
			break;
		}

		case 5:
		{
			/* RF3_XXX6 */
			break;
		}

		case 6:
		{
			/* RF3_XXX7 */
			break;
		}

		case 7:
		{
			/* RF3_XXX8 */
			break;
		}

		case 8:
		{
			/* RF3_BR_ACID */
			if (FLAG(bp_ptr, TR_IM_ACID)) break;
			z = ouch;
			p += 40;
			break;
		}

		case 9:
		{
			/* RF3_BR_ELEC */
			if (FLAG(bp_ptr, TR_IM_ELEC)) break;
			z = ouch;
			p += 20;
			break;
		}

		case 10:
		{
			/* RF3_BR_FIRE */
			if (FLAG(bp_ptr, TR_IM_FIRE)) break;
			z = ouch;
			p += 40;
			break;
		}

		case 11:
		{
			/* RF3_BR_COLD */
			if (FLAG(bp_ptr, TR_IM_COLD)) break;
			z = ouch;
			p += 20;
			break;
		}

		case 12:
		{
			/* RF3_BR_POIS */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_POIS)) break;
			if (my_oppose_pois) break;
			p += 20;
			break;
		}

		case 13:
		{
			/* RF3_BR_NETH */
			z = ouch + 100;
			if (FLAG(bp_ptr, TR_RES_NETHER)) break;
			p += 50;
			if (FLAG(bp_ptr, TR_HOLD_LIFE)) break;
			/* do not worry about drain exp after level 50 */
			if (bp_ptr->lev >= 50) break;
			p += 150;
			break;
		}

		case 14:
		{
			/* RF3_BR_LITE */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_LITE)) break;
			if (FLAG(bp_ptr, TR_RES_BLIND)) break;
			p += 20;
			break;
		}

		case 15:
		{
			/* RF3_BR_DARK */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_DARK)) break;
			if (FLAG(bp_ptr, TR_RES_BLIND)) break;
			p += 20;
			break;
		}

		case 16:
		{
			/* RF3_BR_CONF */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_CONF)) break;
			p += 100;
			break;
		}

		case 17:
		{
			/* RF3_BR_SOUN */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_SOUND)) break;
			p += 50;
			break;
		}

		case 18:
		{
			/* RF3_BR_CHAO */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_CHAOS)) break;
			p += 200;
			if (!(FLAG(bp_ptr, TR_RES_NETHER))) p += 50;
			if (!(FLAG(bp_ptr, TR_HOLD_LIFE))) p += 50;
			if (!(FLAG(bp_ptr, TR_RES_CONF))) p += 50;
			if (bp_ptr->lev == 50) break;
			p += 100;
			break;
		}

		case 19:
		{
			/* RF3_BR_DISE */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_DISEN)) break;
			p += 500;
			break;
		}

		case 20:
		{
			/* RF3_BR_NEXU */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_NEXUS)) break;
			p += 100;
			break;
		}

		case 21:
		{
			/* RF3_BR_TIME */
			z = ouch;
			p += 200;
			break;
		}

		case 22:
		{
			/* RF3_BR_INER */
			z = ouch;
			p += 50;
			break;
		}

		case 23:
		{
			/* RF3_BR_GRAV */
			z = ouch;
			p += 50;
			if (FLAG(bp_ptr, TR_RES_SOUND)) break;
			p += 50;
			break;
		}

		case 24:
		{
			/* RF3_BR_SHAR */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_SHARDS)) break;
			p += 50;
			break;
		}

		case 25:
		{
			/* RF3_BR_PLAS */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_SOUND)) break;
			p += 50;
			break;
		}

		case 26:
		{
			/* RF3_BR_WALL */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_SOUND)) break;
			p += 50;
			break;
		}

		case 27:
		{
			/* RF3_BR_MANA */
			/* XXX XXX XXX */
			break;
		}

		case 28:
		{
			/* RF3_XXX5X4 */
			break;
		}

		case 29:
		{
			/* RF3_XXX6X4 */
			break;
		}

		case 30:
		{
			/* RF3_XXX7X4 */
			break;
		}

		case 31:
		{
			/* RF3_XXX8X4 */
			break;
		}

		case 32:
		{
			/* RF4_BA_ACID */
			if (FLAG(bp_ptr, TR_IM_ACID)) break;
			z = ouch;
			p += 40;
			break;
		}

		case 33:
		{
			/* RF4_BA_ELEC */
			if (FLAG(bp_ptr, TR_IM_ELEC)) break;
			z = ouch;
			p += 20;
			break;
		}

		case 34:
		{
			/* RF4_BA_FIRE */
			if (FLAG(bp_ptr, TR_IM_FIRE)) break;
			z = ouch;
			p += 40;
			break;
		}

		case 35:
		{
			/* RF4_BA_COLD */
			if (FLAG(bp_ptr, TR_IM_COLD)) break;
			z = ouch;
			p += 20;
			break;
		}

		case 36:
		{
			/* RF4_BA_POIS */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_POIS)) break;
			p += 20;
			break;
		}

		case 37:
		{
			/* RF4_BA_NETH */
			z = ouch + 100;
			if (FLAG(bp_ptr, TR_RES_NETHER)) break;
			p += 300;
			break;
		}

		case 38:
		{
			/* RF4_BA_WATE */
			z = ouch;
			p += 50;
			break;
		}

		case 39:
		{
			/* RF4_BA_MANA */
			z = ouch;
			break;
		}

		case 40:
		{
			/* RF4_BA_DARK */
			z = ouch;
			if (FLAG(bp_ptr, TR_RES_DARK)) break;
			if (FLAG(bp_ptr, TR_RES_BLIND)) break;
			p += 20;
			break;
		}

		case 41:
		{
			/* RF4_DRAIN_MANA */
			if (bp_ptr->msp) p += 10;
			break;
		}

		case 42:
		{
			/* RF4_MIND_BLAST */
			z = 20;
			break;
		}

		case 43:
		{
			/* RF4_BRAIN_SMASH */
			z = (12 * 15);
			p += 100;
			break;
		}

		case 44:
		{
			/* RF4_CAUSE_1 */
			z = (3 * 8);
			break;
		}

		case 45:
		{
			/* RF4_CAUSE_2 */
			z = (8 * 8);
			break;
		}

		case 46:
		{
			/* RF4_CAUSE_3 */
			z = (10 * 15);
			break;
		}

		case 47:
		{
			/* RF4_CAUSE_4 */
			z = (15 * 15);
			p += 50;
			break;
		}

		case 48:
		{
			/* RF4_BO_ACID */
			if (FLAG(bp_ptr, TR_IM_ACID)) break;
			z = ouch;
			p += 40;
			break;
		}

		case 49:
		{
			/* RF4_BO_ELEC */
			if (FLAG(bp_ptr, TR_IM_ELEC)) break;
			z = ouch;
			p += 20;
			break;
		}

		case 50:
		{
			/* RF4_BO_FIRE */
			if (FLAG(bp_ptr, TR_IM_FIRE)) break;
			z = ouch;
			p += 40;
			break;
		}

		case 51:
		{
			/* RF4_BO_COLD */
			if (FLAG(bp_ptr, TR_IM_COLD)) break;
			z = ouch;
			p += 20;
			break;
		}

		case 52:
		{
			/* RF4_BO_POIS */
			/* XXX XXX XXX */
			break;
		}

		case 53:
		{
			/* RF4_BO_NETH */
			z = ouch + 100;
			if (FLAG(bp_ptr, TR_RES_NETHER)) break;
			p += 200;
			break;
		}

		case 54:
		{
			/* RF4_BO_WATE */
			z = ouch;
			p += 20;
			break;
		}

		case 55:
		{
			/* RF4_BO_MANA */
			z = ouch;
			break;
		}

		case 56:
		{
			/* RF4_BO_PLAS */
			z = ouch;
			p += 20;
			break;
		}

		case 57:
		{
			/* RF4_BO_ICEE */
			z = ouch;
			p += 20;
			break;
		}

		case 58:
		{
			/* RF4_MISSILE */
			z = ouch;
			break;
		}

		case 59:
		{
			/* RF4_SCARE */
			p += 10;
			break;
		}

		case 60:
		{
			/* RF4_BLIND */
			p += 10;
			break;
		}

		case 61:
		{
			/* RF4_CONF */
			p += 10;
			break;
		}

		case 62:
		{
			/* RF4_SLOW */
			p += 5;
			break;
		}

		case 63:
		{
			/* RF4_HOLD */
			p += 20;
			break;
		}

		case 64:
		{
			/* RF5_HASTE */
			p += 10 + bp_ptr->depth;
			break;
		}

		case 65:
		{
			/* RF5_XXX1X6 */
			break;
		}

		case 66:
		{
			/* RF5_HEAL */
			p += 10;
			break;
		}

		case 67:
		{
			/* RF5_XXX2X6 */
			break;
		}

		case 68:
		{
			/* RF5_XXX3X6 */
			break;
		}

		case 69:
		{
			/* RF5_XXX4X6 */
			break;
		}

		case 70:
		{
			/* RF5_TELE_TO */
			p += 20 + bp_ptr->depth;
			break;
		}

		case 71:
		{
			/* RF5_TELE_AWAY */
			p += 10;
			break;
		}

		case 72:
		{
			/* RF5_TELE_LEVEL */
			p += 50;
			break;
		}

		case 73:
		{
			/* RF5_XXX5 */
			break;
		}

		case 74:
		{
			/* RF5_DARKNESS */
			break;
		}

		case 75:
		{
			/* RF5_TRAPS */
			p += 50;
			break;
		}

		case 76:
		{
			/* RF5_FORGET */
			/* if you have lots of cash this is not very scary... just re-ID. */
			if (bp_ptr->lev < 35)
				p += 500;
			else
				p += 50;
			break;
		}

		case 77:
		{
			/* RF5_XXX6X6 */
			break;
		}

		case 78:
		{
			/* RF5_XXX7X6 */
			break;
		}

		case 79:
		{
			/* RF5_XXX8X6 */
			break;
		}

		case 80:
		{
			/* RF5_S_MONSTER */
			p += 55;
			break;
		}

		case 81:
		{
			/* RF5_S_MONSTERS */
			p += 30;
			break;
		}

		case 82:
		{
			/* RF5_S_ANT */
			p += 15;
			break;
		}

		case 83:
		{
			/* RF5_S_SPIDER */
			p += 25;
			break;
		}

		case 84:
		{
			/* RF5_S_HOUND */
			p += 45;
			break;
		}

		case 85:
		{
			/* RF5_S_HYDRA */
			p += 70;
			break;
		}

		case 86:
		{
			/* RF5_S_ANGEL */
			p += 80;
			break;
		}

		case 87:
		{
			/* RF5_S_DEMON */
			p += 80;
			break;
		}

		case 88:
		{
			/* RF5_S_UNDEAD */
			p += 80;
			break;
		}

		case 89:
		{
			/* RF5_S_DRAGON */
			p += 80;
			break;
		}

		case 90:
		{
			/* RF5_S_HI_UNDEAD */
			p += 95;
			break;
		}

		case 91:
		{
			/* RF5_S_HI_DRAGON */
			p += 95;
			break;
		}

		case 92:
		{
			/* RF5_S_WRAITH */
			p += 95;
			break;
		}

		case 93:
		{
			/* RF5_S_UNIQUE */
			p += 50;
			break;
		}
	}

	/* Things which hurt us alot need to be a concern */
	if (ouch >= bp_ptr->chp / 2) ouch = ouch * 2;

	/* Notice damage */
	return (p + z);
}


/*
 * Look at the screen and update the borg
 *
 * Note that all the "important" messages that occured after our last
 * action have been "queued" in a usable form.  We must attempt to use
 * these messages to update our knowledge about the world, keeping in
 * mind that the world may have changed in drastic ways.
 *
 * Note that the "borg_t" variable corresponds *roughly* to player turns,
 * except that resting and "repeated" commands count as a single turn,
 * and "free" moves (including "illegal" moves, such as attempted moves
 * into walls, or tunneling into monsters) are counted as turns.
 *
 * Also note that "borg_t" is not incremented until the Borg is about to
 * do something, so nothing ever has a time-stamp of the current time.
 *
 * We rely on the fact that all "perma-lit" grids are memorized when
 * they are seen, so any grid on the current panel that appears "dark"
 * must not be perma-lit.
 *
 * We rely on the fact that all "objects" are memorized when they are
 * seen, so any grid on the current panel that appears "dark" must not
 * have an object in it.  But it could have a monster which looks like
 * an object, but this is very rare.  XXX XXX XXX
 *
 * XXX XXX XXX The basic problem with timestamping the monsters
 * and objects is that we often get a message about a monster, and so
 * we want to timestamp it, but then we cannot use the timestamp to
 * indicate that the monster has not been "checked" yet.  Perhaps
 * we need to do something like give each monster a "moved" flag,
 * and clear all the flags to FALSE each turn before tracking. (?)
 *
 * Note that when two monsters of the same race are standing next to
 * each other, and they both move, such that the second monster ends
 * up where the first one began, we will incorrectly assume that the
 * first monster stayed still, and either the second monster moved
 * two spaces, or the second monster disappeared and a third monster
 * appeared, which is technically possible, if the first monster ate
 * the second, and then cloned the third.
 *
 * There is a problem with monsters which look like objects, namely,
 * they are assumed to be objects, and then if they leave line of
 * sight, they disappear, and the Borg assumes that they are gone,
 * when really they should be identified as monsters.
 *
 * XXX XXX Hack -- note the fast direct access to the screen.
 */
void borg_update(void)
{
	int i, k;

	int hit_dist;

	cptr msg;

	cptr what;

	/*** Update the map ***/

	/* Update the map */
	borg_update_map();

	/* Update the objects */
	delete_dead_objects();

	/* Assume I can shoot here */
	successful_target = BORG_TARGET;

	/* Update the view */
	borg_update_view();

	/*** Track monsters ***/

	/* New monsters near 'moved' monsters */
	observe_kill_move(BORG_MON_NEW, BORG_MON_MOVE, 1);
	observe_kill_move(BORG_MON_NEW, BORG_MON_MOVE, 2);
	observe_kill_move(BORG_MON_NEW, BORG_MON_MOVE, 3);

	/* New monsters near 'old forgotten' monsters */
	observe_kill_move(BORG_MON_NEW, BORG_MON_OLD, 1);
	observe_kill_move(BORG_MON_NEW, BORG_MON_OLD, 2);
	observe_kill_move(BORG_MON_NEW, BORG_MON_OLD, 3);

	/* Scan all the remaining 'old' monsters */
	handle_old_mons(BORG_MON_OLD);
	handle_old_mons(BORG_MON_MOVE);

	/* Append remaining monsters to used list */
	borg_append_mon_list(BORG_MON_USED, BORG_MON_NEW);
	borg_append_mon_list(BORG_MON_USED, BORG_MON_OLD);

	/* Get rid of moved monsters we have not tracked */
	borg_wipe_mon(BORG_MON_MOVE);

	/* Append used monsters to 'old' list, and delete used monsters */
	borg_append_mon_list(BORG_MON_OLD, BORG_MON_USED);

	/*** Handle messages ***/

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Note the message */
		borg_note("# %s (+)", msg);
	}

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Hack -- Handle "SELF" info */
		if (prefix(msg, "SELF:"))
		{
			(void)borg_handle_self(what);
			borg_msg_use[i] = 1;
		}

		/* Handle "You feel..." */
		else if (prefix(msg, "FEELING:"))
		{
			borg_feeling = atoi(what);
			borg_msg_use[i] = 1;
		}
	}

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Handle "You hit xxx." */
		if (prefix(msg, "HIT:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, 0)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "You miss xxx." */
		else if (prefix(msg, "MISS:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, 0)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "You have killed xxx." */
		else if (prefix(msg, "KILL:"))
		{
			borg_count_death(what);

			borg_msg_use[i] = 2;

			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}

		/* Handle "The xxx disappears!"  via teleport other, and blinks away */
		else if (prefix(msg, "BLINK:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, 0)) > 0)
			{
				borg_delete_kill(k, "blinked");
				borg_msg_use[i] = 2;
			}
			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}

		/* Handle "xxx dies." */
		else if (prefix(msg, "DIED:"))
		{
			borg_count_death(what);

			borg_msg_use[i] = 2;

			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}

		/* Handle "xxx screams in pain." */
		else if (prefix(msg, "PAIN:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, 3)) > 0)
			{
				borg_msg_use[i] = 2;
			}
			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__FEAR:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, 0)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__BOLD:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, 0)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}
		else if (prefix(msg, "STATE_SLEEP:"))
		{
			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}
		else if (prefix(msg, "STATE__FEAR:"))
		{
			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}
		else if (prefix(msg, "STATE_CONFUSED:"))
		{
			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}


	}

	/* Process messages */
	/* getting distance to allow for 'hit's */
	hit_dist = 1;
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* if you have moved than do not count the monsters as unknown */
		/* unless they are very far away */
		if (prefix(msg, "SPELL_70") || prefix(msg, "SPELL_71"))
		{
			hit_dist = 100;
			break;
		}

		/* monsters move from earthquake */
		if (prefix(msg, "QUAKE"))
		{
			hit_dist = 3;
			break;
		}
	}

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Handle "You hit xxx." */
		if (prefix(msg, "HIT:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, hit_dist)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "You miss xxx." */
		else if (prefix(msg, "MISS:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, hit_dist)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "You have killed xxx." */
		else if (prefix(msg, "KILL:"))
		{
			borg_count_death(what);

			borg_msg_use[i] = 3;

			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}

		/* Handle "The xxx disappears!"  via teleport other, and blinks away */
		else if (prefix(msg, "BLINK:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, g_x, g_y, 1)) > 0)
			{
				borg_delete_kill(k, "blinked");
				borg_msg_use[i] = 3;
			}
			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}


		/* Handle "xxx dies." */
		else if (prefix(msg, "DIED:"))
		{
			borg_count_death(what);

			borg_msg_use[i] = 3;

			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}

		/* Handle "xxx screams in pain." */
		else if (prefix(msg, "PAIN:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
			/* Shooting (through darkness maybe) worked */
			successful_target = BORG_TARGET;
		}

		/* Handle "xxx hits you." */
		else if (prefix(msg, "HIT_BY:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 1)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "xxx misses you." */
		else if (prefix(msg, "MISS_BY:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 1)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE_SLEEP:"))
		{
			/* Notice changes */
			borg_sleep_kill();
			borg_msg_use[i] = 3;
		}

		/* Handle "awake" */
		else if (prefix(msg, "STATE_AWAKE:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__FEAR:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__BOLD:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Hack -- Handle "spell" */
		else if (prefix(msg, "SPELL_"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}
	}

	/*** Handle new levels ***/

	/* Hack -- note new levels */
	if (old_depth != bp_ptr->depth)
	{
		/* if we are not leaving town increment time since town clock */
		if (!old_depth)
			borg_time_town = 0;
		else
			borg_time_town += borg_t - borg_began;

		/* Hack -- Restart the clock */
		/* borg_t = 1000; */

		/* reset our vault/unique check */
		vault_on_level = FALSE;
		unique_on_level = FALSE;
		scaryguy_on_level = FALSE;

		/* reset our breeder flag */
		breeder_level = FALSE;

		/* reset our need to see inviso clock */
		need_see_inviso = 1;

		/* reset our 'shoot in the dark' flag */
		successful_target = BORG_TARGET;

		/* When level was begun */
		borg_began = borg_t;

		/* Not completed */
		borg_completed = FALSE;

		/* New danger thresh-hold */
		avoidance = bp_ptr->chp;

		/* Wipe the danger */
		borg_danger_wipe = TRUE;

		/* Examine the world */
		borg_do_spell = TRUE;
		borg_do_frame = TRUE;

		/* Enable some functions */
		borg_do_destroy = TRUE;

		/* Allow Pets to stick close */
		p_ptr->pet_follow_distance = PET_FOLLOW_ME;

		/* Mega-Hack -- Clear "call lite" stamp */
		when_call_lite = 0;

		/* Mega-Hack -- Clear "wizard lite" stamp */
		when_wizard_lite = 0;

		/* Mega-Hack -- Clear "detect traps" stamp */
		when_detect_traps = 0;

		/* Mega-Hack -- Clear "detect doors" stamp */
		when_detect_doors = 0;

		/* Mega-Hack -- Clear "detect walls" stamp */
		when_detect_walls = 0;

		/* Mega-Hack -- Clear "detect evil" stamp */
		when_detect_evil = 0;

		/* No goal yet */
		goal = GOAL_NONE;

		/* Hack -- Clear "shop" goals */
		goal_shop = -1;

		/* Do not use any stairs */
		stair_less = FALSE;
		stair_more = FALSE;

		/* Hack -- cannot rise past town */
		if (!bp_ptr->depth) goal_rising = FALSE;

		/* Assume not leaving the level */
		goal_leaving = FALSE;

		/* Assume not fleeing the level */
		goal_fleeing = FALSE;

		/* Assume not ignoring monsters */
		goal_ignoring = FALSE;

		/* No known stairs */
		track_less_num = 0;
		track_more_num = 0;

		/* No known glyph */
		track_glyph_num = 0;

		/* No known steps */
		track_step_num = 0;

		/* No known doors */
		track_door_num = 0;

		/* No monsters here */
		borg_kills_cnt = 0;
		borg_kills_nxt = 1;

		/* Forget old monsters */
		(void)C_WIPE(borg_kills, BORG_KILLS_MAX, borg_kill);

		/* Fake goal location */
		g_x = c_x;
		g_y = c_y;

		/* wipe out bad artifacts list */
		bad_obj_n = -1;
		
		/* save once per level */
		if (borg_flag_save) borg_save = TRUE;

		/* Save new depth */
		old_depth = bp_ptr->depth;

		borg_times_twitch = 0;
		borg_escapes = 0;
	}

	/* Handle old level */
	else
	{
		/* reduce GOI count. NOTE: do not reduce below 1.  That is done */
		/* when the spell is cast. */
		if (borg_goi > 1)
		{
			borg_goi -= borg_game_ratio;
		}

		/* Count down to blast off */
		if (goal_recalling > 1)
		{
			goal_recalling -= borg_game_ratio;

			/* dont let it get to 0 or borg will recast the spell */
			if (goal_recalling <= 0) goal_recalling = 1;
		}

		/* when we need to cast this spell again */
		if (borg_see_inv > 1)
		{
			borg_see_inv -= borg_game_ratio;
		}

		/* Reduce fear over time */
		if (!(borg_t % 10))
		{
			map_block *mb_ptr;

			MAP_ITT_START (mb_ptr)
			{
				if (mb_ptr->fear) mb_ptr->fear--;
			}
			MAP_ITT_END;
		}
	}

	/*** Handle messages ***/

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Handle "xxx dies." */
		if (prefix(msg, "DIED:"))
		{
			borg_count_death(what);

			borg_msg_use[i] = 4;
		}

		/* Handle "xxx screams in pain." */
		else if (prefix(msg, "PAIN:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "xxx hits you." */
		else if (prefix(msg, "HIT_BY:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, hit_dist)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "xxx misses you." */
		else if (prefix(msg, "MISS_BY:"))
		{
			/* Attempt to find the monster */

			if ((k = borg_locate_kill(what, c_x, c_y, hit_dist)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE_SLEEP:"))
		{
			/* Notice changes */
			borg_sleep_kill();
			borg_msg_use[i] = 4;
		}

		/* Handle "awake" */
		else if (prefix(msg, "STATE_AWAKE:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__FEAR:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__BOLD:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Hack -- Handle "spell" */
		else if (prefix(msg, "SPELL_"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, c_x, c_y, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}
	}
	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Handle "xxx hits you." */
		if (prefix(msg, "HIT_BY:"))
		{
			borg_fear_grid(what, c_x, c_y,
						   4 * ((bp_ptr->depth / 5) + 1), FALSE);
			borg_msg_use[i] = 5;
		}

		/* Handle "xxx misses you." */
		else if (prefix(msg, "MISS_BY:"))
		{
			borg_fear_grid(what, c_x, c_y,
						   2 * ((bp_ptr->depth / 5) + 1), FALSE);
			borg_msg_use[i] = 5;
		}

		/* Hack -- Handle "spell" */
		else if (prefix(msg, "SPELL_"))
		{
			borg_fear_grid(what, c_x, c_y, borg_fear_spell(atoi(msg + 6)),
						   FALSE);
			borg_msg_use[i] = 5;
		}
	}
	/* Display messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Final message */
		borg_note("# %s (%d)", msg, borg_msg_use[i]);
	}

	/*** Various things ***/

	/* Forget goals while "impaired" in any way */
	if (bp_ptr->status.blind || bp_ptr->status.confused ||
		bp_ptr->status.afraid || bp_ptr->status.image) goal = 0;

	/* Forget goals while "bleeding" in any way */
	if (bp_ptr->status.weak || bp_ptr->status.poisoned ||
		bp_ptr->status.cut || bp_ptr->status.stun ||
		bp_ptr->status.heavy_stun) goal = 0;

	/* Forget goals when HP changes */
	if (bp_ptr->chp < bp_ptr->oldhp) goal = 0;

	/* Save the hit points */
	bp_ptr->oldhp = bp_ptr->chp;

	/* Forget failure */
	borg_failure = FALSE;

	/* Forget the messages */
	borg_msg_len = 0;
	borg_msg_num = 0;
}


/*
 * Handle various "important" messages
 *
 * Actually, we simply "queue" them for later analysis
 */
void borg_react(cptr msg, cptr buf)
{
	int len;

	if (borg_dont_react)
		return;

	/* Note actual message */
	borg_note("> %s", msg);

	/* Extract length of parsed message */
	len = strlen(buf);

	/* Verify space */
	if (borg_msg_num + 1 > borg_msg_max)
	{
		borg_oops("too many messages");
		return;
	}

	/* Verify space */
	if (borg_msg_len + len + 1 > borg_msg_siz)
	{
		borg_oops("too much messages");
		return;
	}

	/* Assume not used yet */
	borg_msg_use[borg_msg_num] = 0;

	/* Save the message position */
	borg_msg_pos[borg_msg_num] = borg_msg_len;

	/* Save the message text */
	strcpy(borg_msg_buf + borg_msg_len, buf);

	/* Advance the buf */
	borg_msg_len += len + 1;

	/* Advance the pos */
	borg_msg_num++;
}


/*
 * Notice that the player has moved
 */
void borg_player_move(int x, int y, vptr dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;

	c_x = x;
	c_y = y;
}


/*
 * Sorting hook -- comp function -- see below
 *
 * We use "u" to point to an array of strings, and "v" to point to
 * an array of indexes, and we sort them together by the strings.
 */
static bool ang_sort_comp_hook_strings(vptr u, vptr v, int a, int b)
{
	cptr *text = (cptr *)(u);
	s16b *what = (s16b *)(v);

	int cmp;

	/* Compare the two strings */
	cmp = (strcmp(text[a], text[b]));

	/* Strictly less */
	if (cmp < 0) return (TRUE);

	/* Strictly more */
	if (cmp > 0) return (FALSE);

	/* Enforce "stable" sort */
	return (what[a] <= what[b]);
}


/*
 * Sorting hook -- swap function -- see below
 *
 * We use "u" to point to an array of strings, and "v" to point to
 * an array of indexes, and we sort them together by the strings.
 */
static void ang_sort_swap_hook_strings(vptr u, vptr v, int a, int b)
{
	cptr *text = (cptr *)(u);
	s16b *what = (s16b *)(v);

	cptr texttmp;
	s16b whattmp;

	/* Swap "text" */
	texttmp = text[a];
	text[a] = text[b];
	text[b] = texttmp;

	/* Swap "what" */
	whattmp = what[a];
	what[a] = what[b];
	what[b] = whattmp;
}



/*
 * Init this file.
 */
void borg_init_2(void)
{
	int i;

	int size;

	s16b *what;
	cptr *text;

	/* Make the towns in the wilderness */
	C_MAKE(borg_towns, borg_town_size, borg_town);

	/* Make the stores in the towns */
	C_MAKE(borg_shops, borg_shop_size, borg_shop);

	/* Make the dungeons in the wilderness */
	C_MAKE(borg_dungeons, borg_dungeon_size, borg_dungeon);

	/* Initialise los information */
	borg_vinfo_init();

	/* Allocate temp arrays */
	C_MAKE(what, z_info->r_max, s16b);
	C_MAKE(text, z_info->r_max, cptr);

	/*** Message tracking ***/

	/* No chars saved yet */
	borg_msg_len = 0;

	/* Maximum buffer size */
	borg_msg_siz = 4096;

	/* Allocate a buffer */
	C_MAKE(borg_msg_buf, borg_msg_siz, char);

	/* No msg's saved yet */
	borg_msg_num = 0;

	/* Maximum number of messages */
	borg_msg_max = 256;

	/* Allocate array of positions */
	C_MAKE(borg_msg_pos, borg_msg_max, s16b);

	/* Allocate array of use-types */
	C_MAKE(borg_msg_use, borg_msg_max, s16b);


	/*** Parse "unique" monster names ***/

	/* Start over */
	size = 0;

	/* Collect "unique" monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip non-monsters */
		if (!r_ptr->name) continue;

		/* Skip non-unique monsters */
		if (!FLAG(r_ptr, RF_UNIQUE)) continue;

		/* Use it */
		text[size] = mon_race_name(r_ptr);
		what[size] = i;
		size++;
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_hook_strings;
	ang_sort_swap = ang_sort_swap_hook_strings;

	/* Sort */
	ang_sort((void *) text, what, size);

	/* Save the size */
	borg_unique_size = size;

	/* Allocate the arrays */
	C_MAKE(borg_unique_text, borg_unique_size, cptr);
	C_MAKE(borg_unique_what, borg_unique_size, s16b);

	/* Save the entries */
	for (i = 0; i < size; i++) borg_unique_text[i] = text[i];
	for (i = 0; i < size; i++) borg_unique_what[i] = what[i];


	/*** Parse "normal" monster names ***/

	/* Start over */
	size = 0;

	/* Collect "normal" monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip non-monsters */
		if (!r_ptr->name) continue;

		/* Skip unique monsters */
		if (FLAG(r_ptr, RF_UNIQUE)) continue;

		/* Use it */
		text[size] = mon_race_name(r_ptr);
		what[size] = i;
		size++;
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_hook_strings;
	ang_sort_swap = ang_sort_swap_hook_strings;

	/* Sort */
	ang_sort((void *) text, what, size);

	/* Save the size */
	borg_normal_size = size;

	/* Allocate the arrays */
	C_MAKE(borg_normal_text, borg_normal_size, cptr);
	C_MAKE(borg_normal_what, borg_normal_size, s16b);

	/* Save the entries */
	for (i = 0; i < size; i++) borg_normal_text[i] = text[i];
	for (i = 0; i < size; i++) borg_normal_what[i] = what[i];

	/* Free the arrays */
	FREE(what);
	FREE((void *)text);
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
