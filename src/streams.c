/* CVS: Last edit by $Author: rr9 $ on $Date: 1999/11/24 21:52:01 $
 *
 * File: streams.c
 * Purpose: Used by dungeon generation. This file holds all the
 * functions that are applied to a level after the rest has been
 * generated, ie streams and level destruction.
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "generate.h"
#include "streams.h"
#include "grid.h"

#if 0
/* Taken from Eric Bock's RAngband */
/*
 * Places moss through dungeon.
 */
void add_moss(void)
{
	int i, tx, ty;
	int y, x, dir;


	/* Hack -- Choose starting point */
	y = rand_spread(cur_hgt / 2, cur_hgt / 2 - 10);
	x = rand_spread(cur_wid / 2, cur_wid / 2 - 10);

	/* Choose a random compass direction */
	dir = ddd[rand_int(8)];

	/* Place streamer into dungeon */
	while (TRUE)
	{
		/* One grid per density */
		for (i = 0; i < DUN_MOS_DEN; i++)
		{
			int d = DUN_MOS_RNG;

			/* Pick a nearby grid */
			while (1)
			{
				ty = rand_spread(y, d);
				tx = rand_spread(x, d);
				if (!in_bounds(ty, tx)) continue;
				break;
			}

			/* Only convert "outer" and "solid" walls and floors */
			if ((cave[ty][tx].feat != FEAT_WALL_OUTER) &&
				 (cave[ty][tx].feat != FEAT_WALL_SOLID) &&
				 (cave[ty][tx].feat != FEAT_FLOOR))
			{
				continue;
			}

			/* Clear previous contents, add proper moss type */
			if (cave[ty][tx].feat == FEAT_FLOOR)
				cave[ty][tx].feat = FEAT_FLOOR_M;
			else
				cave[ty][tx].feat = FEAT_WALL_M;
		}

		/* Advance the streamer */
		y += ddy[dir];
		x += ddx[dir];

		/* Stop at dungeon edge */
		if (!in_bounds(y, x)) break;
	}
}
#endif /* 0 */


/*
 * Places water through dungeon.
 */
void add_river(int feat1, int feat2, int depth)
{
	int tx, ty;
	int y, x, dir, dd, len, wid;
	int d;

	int ly, lx;
	int ny, nx;

	/* Hack -- Choose starting point */
	ly = y = rand_spread(cur_hgt / 2, cur_hgt / 2 - 10);
	lx = x = rand_spread(cur_wid / 2, cur_wid / 2 - 10);

	/* Choose a random direction */
	dd = rand_int(16);
	dir = cdd[dd / 2];

	wid = randint(DUN_WAT_RNG);

	/* Choose a length */
	len = rand_int(10) + 5;

	/* Find endpoint */
	if (dd % 2 == 0)
	{
		ny = y + len * ddy[dir];
		nx = x + len * ddx[dir];
	}
	/* Acquire the intermediate direction */
	else
	{
		ny = y + len * (ddy[dir] + ddy_cdd[((dd + 1) / 2) % 8]);
		nx = x + len * (ddx[dir] + ddx_cdd[((dd + 1) / 2) % 8]);
	}

	/* Place streamer into dungeon */
	while (TRUE)
	{
		for (ty = y - wid - 1; ty <= y + wid + 1; ty++)
		{
			for (tx = x - wid - 1; tx <= x + wid + 1; tx++)
			{
				if (!in_bounds(ty, tx)) continue;

				if (distance(ty, tx, y, x) > rand_spread(wid, 1)) continue;

				/* Do not convert permanent features */
				if (cave_perma_bold(ty, tx)) continue;

				/* Find the distance from the center */
				if (distance(ty, tx, ny, nx) > distance(ly, lx, ny, nx))
				{
					/* Beginning of river */
					d = distance(ty, tx, ly, lx);
				}
				else
				{
					/* Middle of river */
					d = dist_to_line(ty, tx, ly, lx, ny, nx);
				}

				/*
				 * Clear previous contents, add feature
				 * The border mainly gets feat2, while the center gets feat1
				 */
				if (!wid || ((d > wid * rand_spread(depth, 10) / 100) &&
					 (rand_int(d * 100 / wid) >= depth)))
				{
					set_cave_feat(ty, tx, feat2);
				}
				else
				{
					set_cave_feat(ty, tx, feat1);
				}

				/* Lava terrain glows */
				if ((feat1 == FEAT_DEEP_LAVA) ||
				    (feat1 == FEAT_SHAL_LAVA))
				{
					cave[ty][tx].info |= CAVE_GLOW;
				}

				/* Hack -- don't teleport here */
				cave[ty][tx].info |= CAVE_ICKY;
			}
		}

		/* Advance the streamer */
		if (dd % 2 == 0)
		{
			y += ddy[dir];
			x += ddx[dir];
		}
		/* Acquire the intermediate direction */
		else
		{
			y += ddy[dir] + ddy_cdd[((dd + 1) / 2) % 8];
			x += ddx[dir] + ddx_cdd[((dd + 1) / 2) % 8];
		}

		if (len-- == 0)
		{
			/* Change direction slowly */
			dd = rand_spread(dd, 1) % 16;
			dir = cdd[dd / 2];

			wid = rand_spread(wid, 1);

			depth = rand_spread(depth, 10);

			if (wid < 0) wid = 0;
			if (wid > DUN_WAT_RNG) wid = DUN_WAT_RNG;

			if (depth < 0) depth = 0;
			if (depth > 100) depth = 100;

			len = rand_int(10) + 5;

			/* Find starting point */
			ly = y;
			lx = x;

			/* Find endpoint */
			if (dd % 2 == 0)
			{
				ny = y + len * ddy[dir];
				nx = x + len * ddx[dir];
			}
			/* Acquire the intermediate direction */
			else
			{
				ny = y + len * (ddy[dir] + ddy_cdd[((dd + 1) / 2) % 8]);
				nx = x + len * (ddx[dir] + ddx_cdd[((dd + 1) / 2) % 8]);
			}
		}

		/* Stop at dungeon edge */
		if (!in_bounds(y, x)) break;
	}
}


/*
 * Places "streamers" of rock through dungeon
 *
 * Note that their are actually six different terrain features used
 * to represent streamers.  Three each of magma and quartz, one for
 * basic vein, one with hidden gold, and one with known gold.  The
 * hidden gold types are currently unused.
 */
void build_streamer(int feat, int chance)
{
	int		i, tx, ty;
	int		y, x, dir;
	int dummy = 0;

	cave_type *c_ptr;

	/* Hack -- Choose starting point */
	y = rand_spread(cur_hgt / 2, 10);
	x = rand_spread(cur_wid / 2, 15);

	/* Choose a random compass direction */
	dir = ddd[rand_int(8)];

	/* Place streamer into dungeon */
	while (dummy < SAFE_MAX_ATTEMPTS)
	{
		dummy++;

		/* One grid per density */
		for (i = 0; i < DUN_STR_DEN; i++)
		{
			int d = DUN_STR_RNG;

			/* Pick a nearby grid */
			while (1)
			{
				ty = rand_spread(y, d);
				tx = rand_spread(x, d);
				if (!in_bounds2(ty, tx)) continue;
				break;
			}

			/* Access the grid */
			c_ptr = &cave[ty][tx];

			/* Only convert "granite" walls */
			if (c_ptr->feat < FEAT_WALL_EXTRA) continue;
			if (c_ptr->feat > FEAT_WALL_SOLID) continue;

			/* Clear previous contents, add proper vein type */
			c_ptr->feat = feat;

			/* Hack -- Add some (known) treasure */
			if (rand_int(chance) == 0) c_ptr->feat += 0x04;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
				msg_print("Warning! Could not place streamer!");
			}
			return;
		}


		/* Advance the streamer */
		y += ddy[dir];
		x += ddx[dir];

		/* Quit before leaving the dungeon */
		if (!in_bounds(y, x)) break;
	}
}



/*
 * Place streams of water, lava, & trees -KMW-
 * This routine varies the placement based on dungeon level
 * otherwise is similar to build_streamer
 */
void build_streamer2(int feat, bool killwall, bool pool)
{
	int i, j, mid, tx, ty;
	int y, x, dir;
	int poolsize;


	/* Hack -- Choose starting point */
	y = rand_spread(cur_hgt / 2, 10);
	x = rand_spread(cur_wid / 2, 15);

	/* Choose a random compass direction */
	dir = ddd[rand_int(8)];

	if (!pool)
	{
		/* Place streamer into dungeon */
		while (TRUE)
		{
			/* One grid per density */
			for (i = 0; i < (DUN_STR_DWLW + 1); i++)
			{
				int d = DUN_STR_WLW;

				/* Pick a nearby grid */
				while (1)
				{
					ty = rand_spread(y, d);
					tx = rand_spread(x, d);
					if (!in_bounds(ty, tx)) continue;
					break;
				}

				/* Only convert non-permanent features */
				if (killwall)
				{
					if (cave[ty][tx].feat >= FEAT_PERM_EXTRA) continue;
					if (cave[ty][tx].feat == FEAT_LESS) continue;
					if (cave[ty][tx].feat == FEAT_MORE) continue;
				}
				else
				{
					if (cave[ty][tx].feat >= FEAT_MAGMA) continue;
					if (cave[ty][tx].feat == FEAT_LESS) continue;
					if (cave[ty][tx].feat == FEAT_MORE) continue;
				}

				/* Clear previous contents, add proper vein type */
				cave[ty][tx].feat = feat;
			}

			/* Advance the streamer */
			y += ddy[dir];
			x += ddx[dir];

			if (randint(20) == 1)
				dir = ddd[rand_int(8)]; /* change direction */

			/* Stop at dungeon edge */
			if (!in_bounds(y, x)) break;
		}
	}
	else
	{
		/* create pool */
		poolsize = 5 + randint(10);
		mid = poolsize / 2;

		/* One grid per density */
		for (i = 0; i < poolsize; i++)
		{
			for (j = 0; j < poolsize; j++)
			{
				tx = x + j;
				ty = y + i;

				if (!in_bounds(ty, tx)) continue;

				if (i < mid)
				{
					if (j < mid)
					{
						if ((i + j + 1) < mid)
							continue;
					}
					else if (j > (mid+ i))
						continue;
				}
				else if (j < mid)
				{
					if (i > (mid + j))
						continue;
				}
				else if ((i + j) > ((mid * 3)-1))
					continue;

				/* Only convert non-permanent features */
				if (killwall)
				{
					if (cave[ty][tx].feat >= FEAT_PERM_EXTRA) continue;
					if (cave[ty][tx].feat == FEAT_LESS) continue;
					if (cave[ty][tx].feat == FEAT_MORE) continue;
				}
				else
				{
					if (cave[ty][tx].feat >= FEAT_MAGMA) continue;
					if (cave[ty][tx].feat == FEAT_LESS) continue;
					if (cave[ty][tx].feat == FEAT_MORE) continue;
				}

				/* Clear previous contents, add proper vein type */
				cave[ty][tx].feat = feat;
			}
		}
	}
}



/*
 * Build a destroyed level
 */
void destroy_level(void)
{
	int y1, x1, y, x, k, t, n;

	cave_type *c_ptr;

	/* Note destroyed levels */
	if (cheat_room) msg_print("Destroyed Level");

	/* Drop a few epi-centers (usually about two) */
	for (n = 0; n < randint(5); n++)
	{
		/* Pick an epi-center */
		x1 = rand_range(5, cur_wid-1 - 5);
		y1 = rand_range(5, cur_hgt-1 - 5);

		/* Big area of affect */
		for (y = (y1 - 15); y <= (y1 + 15); y++)
		{
			for (x = (x1 - 15); x <= (x1 + 15); x++)
			{
				/* Skip illegal grids */
				if (!in_bounds(y, x)) continue;

				/* Extract the distance */
				k = distance(y1, x1, y, x);

				/* Stay in the circle of death */
				if (k >= 16) continue;

				/* Delete the monster (if any) */
				delete_monster(y, x);

				/* Destroy valid grids */
				if (cave_valid_bold(y, x))
				{
					/* Delete objects */
					delete_object(y, x);

					/* Access the grid */
					c_ptr = &cave[y][x];

					/* Wall (or floor) type */
					t = rand_int(200);

					/* Granite */
					if (t < 20)
					{
						/* Create granite wall */
						c_ptr->feat = FEAT_WALL_EXTRA;
					}

					/* Quartz */
					else if (t < 70)
					{
						/* Create quartz vein */
						c_ptr->feat = FEAT_QUARTZ;
					}

					/* Magma */
					else if (t < 100)
					{
						/* Create magma vein */
						c_ptr->feat = FEAT_MAGMA;
					}

					/* Floor */
					else
					{
						/* Create floor */
						c_ptr->feat = FEAT_FLOOR;
					}

					/* No longer part of a room or vault */
					c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

					/* No longer illuminated or known */
					c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW);
				}
			}
		}
	}
}


