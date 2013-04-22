/* File: generate.c */

/* Purpose: Dungeon generation */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/* Prfnoff -- reorganized generate.c into generate.c, genroom.c, genfeat.c, wild.c */
#include "angband.h"


/*
 * Note that Level generation is *not* an important bottleneck,
 * though it can be annoyingly slow on older machines...  Thus
 * we emphasize "simplicity" and "correctness" over "speed".
 *
 * This entire file is only needed for generating levels.
 * This may allow smart compilers to only load it when needed.
 *
 * Consider the "v_info.txt" file for vault generation.
 *
 * In this file, we use the "special" granite and perma-wall sub-types,
 * where "basic" is normal, "inner" is inside a room, "outer" is the
 * outer wall of a room, and "solid" is the outer wall of the dungeon
 * or any walls that may not be pierced by corridors.  Thus the only
 * wall type that may be pierced by a corridor is the "outer granite"
 * type.  The "basic granite" type yields the "actual" corridors.
 *
 * Note that we use the special "solid" granite wall type to prevent
 * multiple corridors from piercing a wall in two adjacent locations,
 * which would be messy, and we use the special "outer" granite wall
 * to indicate which walls "surround" rooms, and may thus be "pierced"
 * by corridors entering or leaving the room.
 *
 * Note that a tunnel which attempts to leave a room near the "edge"
 * of the dungeon in a direction toward that edge will cause "silly"
 * wall piercings, but will have no permanently incorrect effects,
 * as long as the tunnel can *eventually* exit from another side.
 * And note that the wall may not come back into the room by the
 * hole it left through, so it must bend to the left or right and
 * then optionally re-enter the room (at least 2 grids away).  This
 * is not a problem since every room that is large enough to block
 * the passage of tunnels is also large enough to allow the tunnel
 * to pierce the room itself several times.
 *
 * Note that no two corridors may enter a room through adjacent grids,
 * they must either share an entryway or else use entryways at least
 * two grids apart.  This prevents "large" (or "silly") doorways.
 *
 * To create rooms in the dungeon, we first divide the dungeon up
 * into "blocks" of 11x11 grids each, and require that all rooms
 * occupy a rectangular group of blocks.  As long as each room type
 * reserves a sufficient number of blocks, the room building routines
 * will not need to check bounds.  Note that most of the normal rooms
 * actually only use 23x11 grids, and so reserve 33x11 grids.
 *
 * Note that the use of 11x11 blocks (instead of the old 33x11 blocks)
 * allows more variability in the horizontal placement of rooms, and
 * at the same time has the disadvantage that some rooms (two thirds
 * of the normal rooms) may be "split" by panel boundaries.  This can
 * induce a situation where a player is in a room and part of the room
 * is off the screen.  It may be annoying enough to go back to 33x11
 * blocks to prevent this visual situation.
 *
 * Note that the dungeon generation routines are much different (2.7.5)
 * and perhaps "DUN_ROOMS" should be less than 50.
 *
 * XXX XXX XXX Note that it is possible to create a room which is only
 * connected to itself, because the "tunnel generation" code allows a
 * tunnel to leave a room, wander around, and then re-enter the room.
 *
 * XXX XXX XXX Note that it is possible to create a set of rooms which
 * are only connected to other rooms in that set, since there is nothing
 * explicit in the code to prevent this from happening.  But this is less
 * likely than the "isolated room" problem, because each room attempts to
 * connect to another room, in a giant cycle, thus requiring at least two
 * bizarre occurances to create an isolated section of the dungeon.
 *
 * Note that (2.7.9) monster pits have been split into monster "nests"
 * and monster "pits".  The "nests" have a collection of monsters of a
 * given type strewn randomly around the room (jelly, animal, or undead),
 * while the "pits" have a collection of monsters of a given type placed
 * around the room in an organized manner (orc, troll, giant, dragon, or
 * demon).  Note that both "nests" and "pits" are now "level dependant",
 * and both make 16 "expensive" calls to the "get_mon_num()" function.
 *
 * Note that the cave grid flags changed in a rather drastic manner
 * for Angband 2.8.0 (and 2.7.9+), in particular, dungeon terrain
 * features, such as doors and stairs and traps and rubble and walls,
 * are all handled as a set of 64 possible "terrain features", and
 * not as "fake" objects (440-479) as in pre-2.8.0 versions.
 *
 * The 64 new "dungeon features" will also be used for "visual display"
 * but we must be careful not to allow, for example, the user to display
 * hidden traps in a different way from floors, or secret doors in a way
 * different from granite walls, or even permanent granite in a different
 * way from granite.  XXX XXX XXX
 */


/*
 * Dungeon generation values
 */
#define DUN_UNUSUAL 194 /* Level/chance of unusual room (was 200) */
#define DUN_DEST    18  /* 1/chance of having a destroyed level */
#define SMALL_LEVEL 3   /* 1/chance of smaller size (3) */
#define EMPTY_LEVEL 15  /* 1/chance of being 'empty' (15) */
#define LAKE_LEVEL  7   /* 1/chance of being a lake on the level */
#define DARK_EMPTY  5   /* 1/chance of arena level NOT being lit (2) */
#define DUN_CAVERN  30  /* 1/chance of having a cavern level */

/* Number of rooms to attempt (was 50) */
#define DUN_ROOMS_MIN	10
#define DUN_ROOMS_MAX	100

static int dun_rooms;

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND_MIN	 5 /* Chance of random direction (was 10) */
#define DUN_TUN_RND_MAX	20
#define DUN_TUN_CHG_MIN	20 /* Chance of changing direction (was 30) */
#define DUN_TUN_CHG_MAX	60
#define DUN_TUN_CON_MIN 10 /* Chance of extra tunneling (was 15) */
#define DUN_TUN_CON_MAX	40
#define DUN_TUN_PEN_MIN 30 /* Chance of doors at room entrances (was 25) */
#define DUN_TUN_PEN_MAX 70
#define DUN_TUN_JCT_MIN 60 /* Chance of doors at tunnel junctions (was 90) */
#define DUN_TUN_JCT_MAX 90

static int dun_tun_rnd;
static int dun_tun_chg;
static int dun_tun_con;
static int dun_tun_pen;
static int dun_tun_jct;

/*
 * Dungeon streamer generation values
 */
#define DUN_STR_DEN     5	/* Density of streamers */
#define DUN_STR_RNG     2	/* Width of streamers */
#define DUN_STR_MAG     3	/* Number of magma streamers */
#define DUN_STR_MC     90	/* 1/chance of treasure per magma */
#define DUN_STR_QUA	    2	/* Number of quartz streamers */
#define DUN_STR_QC     40	/* 1/chance of treasure per quartz */

#define DUN_MOS_DEN     2	/* Density of moss streamers */
#define DUN_WAT_RNG     2	/* Width of rivers */
#define DUN_WAT_CHG    50	/* 1 in 50 chance of junction in river */


/*
 * Dungeon treausre allocation values
 */
#define DUN_AMT_ROOM	9	/* Amount of objects for rooms */
#define DUN_AMT_ITEM	3	/* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD	3	/* Amount of treasure for rooms/corridors */


/*
 * Dungeon generation data -- see "cave_gen()"
 */
dun_data *dun;



/*
 * Always picks a correct direction
 */
static void correct_dir(int *rdir, int *cdir, int y1, int x1, int y2, int x2)
{
	/* Extract vertical and horizontal directions */
	*rdir = (y1 == y2) ? 0 : (y1 < y2) ? 1 : -1;
	*cdir = (x1 == x2) ? 0 : (x1 < x2) ? 1 : -1;

	/* Never move diagonally */
	if (*rdir && *cdir)
	{
		if (rand_int(100) < 50)
		{
			*rdir = 0;
		}
		else
		{
			*cdir = 0;
		}
	}
}


/*
 * Pick a random direction
 */
static void rand_dir(int *rdir, int *cdir)
{
	/* Pick a random direction */
	int i = rand_int(4);

	/* Extract the dy/dx components */
	*rdir = ddy_ddd[i];
	*cdir = ddx_ddd[i];
}


/*
 * Returns random co-ordinates for player/monster/object
 */
static bool new_player_spot(void)
{
	int	y, x;
	int max_attempts = 5000;

	/* Place the player */
	while (max_attempts--)
	{
		/* Pick a legal spot */
		y = rand_range(1, cur_hgt - 2);
		x = rand_range(1, cur_wid - 2);

		/* Must be a "naked" floor grid */
		if (!cave_naked_bold(y, x)) continue;

		/* Refuse to start on anti-teleport grids */
		if (cave[y][x].info & (CAVE_ICKY)) continue;

		/* Done */
		break;
	}

	if (max_attempts < 1) /* Should be -1, actually if we failed... */
		return FALSE;


	/* Save the new player grid */
	py = y;
	px = x;

	return TRUE;
}



/*
 * Recursive fractal algorithm to place water through the dungeon.
 */
static void recursive_river(int x1, int y1, int x2, int y2, s16b pval, int width)
{
	int dx, dy, length, l, x, y;
	int changex, changey;
	int ty, tx;
	bool done;
	int feat1, feat2;


	/* Get the features -- Prfnoff */
	feat1 = cave_get_feat(pval, (FF1_TERRAIN | FF1_HARD));
	feat2 = cave_get_feat(pval, FF1_TERRAIN);

	length = distance(x1, y1, x2, y2);

	if (length > 4)
	{
		/*
		 * Divide path in half and call routine twice.
		 * There is a small chance of splitting the river
		 */
		dx = (x2 - x1) / 2;
		dy = (y2 - y1) / 2;

		if (dy != 0)
		{
			/* perturbation perpendicular to path */
			changex = randint(abs(dy)) * 2 - abs(dy);
		}
		else
		{
			changex = 0;
		}

		if (dx != 0)
		{
			/* perturbation perpendicular to path */
			changey = randint(abs(dx)) * 2 - abs(dx);
		}
		else
		{
			changey = 0;
		}

		if (!in_bounds(y1 + dy + changey, x1 + dx + changex))
		{
			changex = 0;
			changey = 0;
		}

		/* construct river out of two smaller ones */
		recursive_river(x1, y1, x1 + dx + changex, y1 + dy + changey, pval, width);
		recursive_river(x1 + dx + changex, y1 + dy + changey, x2, y2, pval, width);

		/* Split the river some of the time - junctions look cool */
		if ((randint(DUN_WAT_CHG) == 1) && (width > 0))
		{
			recursive_river(x1 + dx + changex, y1 + dy + changey,
			                x1 + 8 * (dx + changex), y1 + 8 * (dy + changey),
			                pval, width - 1);
		}
	}
	else
	{
		/*Actually build the river*/
		for (l = 0; l < length; l++)
		{
			x = x1 + l * (x2 - x1) / length;
			y = y1 + l * (y2 - y1) / length;

			done = FALSE;

			while (!done)
			{
				for (ty = y - width - 1; ty <= y + width + 1; ty++)
				{
					for (tx = x - width - 1; tx <= x + width + 1; tx++)
					{
						if (!in_bounds(ty, tx)) continue;

						if (cave[ty][tx].feat == feat1) continue;
						if (cave[ty][tx].feat == feat2) continue;

						if (distance(ty, tx, y, x) > rand_spread(width, 1)) continue;

						/* Do not convert permanent features */
						if (cave_perma_bold(ty, tx)) continue;

						/*
						 * Clear previous contents, add feature
						 * The border mainly gets feat2, while the center gets feat1
						 */
						if (distance(ty, tx, y, x) > width)
							cave[ty][tx].feat = feat2;
						else
							cave[ty][tx].feat = feat1;

						/* Lava terrain glows */
						if (pval == PV_TERRAIN_LAVA)
						{
							cave[ty][tx].info |= CAVE_GLOW;
						}

						/* Hack -- don't teleport here */
						cave[ty][tx].info |= CAVE_ICKY;
					}
				}

				done = TRUE;
			}
		}
	}
}


/*
 * Places water /lava through dungeon.
 */
static void add_river(s16b pval)
{
	int y2, x2;
	int y1 = 0, x1 = 0;
	int wid;

	/* Hack -- Choose starting point */
	y2 = randint(cur_hgt / 2 - 2) + cur_hgt / 2;
	x2 = randint(cur_wid / 2 - 2) + cur_wid / 2;

	/* Hack -- Choose ending point somewhere on boundary */
	switch(randint(4))
	{
		case 1:
		{
			/* top boundary */
			x1 = randint(cur_wid-2)+1;
			y1 = 1;
			break;
		}
		case 2:
		{
			/* left boundary */
			x1 = 1;
			y1 = randint(cur_hgt-2)+1;
			break;
		}
		case 3:
		{
			/* right boundary */
			x1 = cur_wid-1;
			y1 = randint(cur_hgt-2)+1;
			break;
		}
		case 4:
		{
			/* bottom boundary */
			x1 = randint(cur_wid-2)+1;
			y1 = cur_hgt-1;
			break;
		}
	}

	wid = randint(DUN_WAT_RNG);
	recursive_river(x1, y1, x2, y2, pval, wid);

	/* Hack - Save the location as a "room" */
	if (dun->cent_n < CENT_MAX)
	{
		dun->cent[dun->cent_n].y = y2;
		dun->cent[dun->cent_n].x = x2;
		dun->cent_n++;
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
static void build_streamer(u32b flags1, int chance)
{
	int		i, tx, ty;
	int		y, x, dir;
	int dummy = 0;
	int feat, gold;

	cave_type *c_ptr;

	/* Hack -- get features -- Prfnoff */
	feat = cave_get_feat(PV_VEIN_NO_GOLD, flags1);
	gold = cave_get_feat(PV_VEIN_K_GOLD, flags1);

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
			if (!cave_granite_grid(c_ptr)) continue;

			/* Clear previous contents, add proper vein type */
			c_ptr->feat = feat;

			/* Hack -- Add some (known) treasure */
			if (rand_int(chance) == 0) c_ptr->feat = gold;
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
 * Put trees near a hole in the dungeon roof  (rubble on ground + up stairway)
 * This happens in real world lava tubes.
 */
static void place_trees(int x, int y)
{
	int i, j;

	/* place trees/ rubble in ovalish distribution*/
	for (i = x - 3; i < x + 4; i++)
	{
		for (j = y - 3; j < y + 4; j++)
		{
			/* Want square to be in the circle and accessable.*/
			if (in_bounds(j, i) && (distance(j, i, y, x) < 4) && !cave_perma_bold(j, i))
			{
				/*
				 * Clear previous contents, add feature
				 * The border mainly gets trees, while the center gets rubble */
				if ((distance(j, i, y, x) > 1) || (randint(100) < 25))
				{
					if (randint(100) < 75)
						cave[j][i].feat = cave_get_feat(PV_TERRAIN_TREE, FF1_TERRAIN);
				}
				else
				{
					place_rubble(j, i);
				}

				/* Light area since is open above */
				cave[j][i].info |= CAVE_GLOW;
			}
		}
	}

	/* No up stairs in ironman mode */
	if (!ironman_downward && (randint(3) == 1))
	{
		place_up_stairs(y, x);
	}
}


/*
 * Build a destroyed level
 */
static void destroy_level(void)
{
	int y1, x1, y, x, k, t, n;

	/* Note destroyed levels */
	if (cheat_room) msg_print("Destroyed Level");

	/* Drop a few epi-centers (usually about two) */
	for (n = 0; n < randint(5); n++)
	{
		/* Pick an epi-center */
		x1 = rand_range(5, cur_wid - 1 - 5);
		y1 = rand_range(5, cur_hgt - 1 - 5);

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

					/* Wall (or floor) type */
					t = rand_int(200);

					/* Granite */
					if (t < 20)
					{
						/* Create granite wall */
						place_extra_wall(y, x);
					}

					/* Quartz */
					else if (t < 70)
					{
						/* Create quartz vein */
						place_quartz_vein(y, x);
					}

					/* Magma */
					else if (t < 100)
					{
						/* Create magma vein */
						place_magma_vein(y, x);
					}

					/* Floor */
					else
					{
						/* Create floor */
						place_floor(y, x);
					}

					wipe_room_grid(y, x);
				}
			}
		}
	}
}

/* Removed room-building stuff to genroom.c -- Prfnoff */


/*
 * Constructs a tunnel between two points
 *
 * This function must be called BEFORE any streamers are created,
 * since we use the special "granite wall" sub-types to keep track
 * of legal places for corridors to pierce rooms.
 *
 * We use "door_flag" to prevent excessive construction of doors
 * along overlapping corridors.
 *
 * We queue the tunnel grids to prevent door creation along a corridor
 * which intersects itself.
 *
 * We queue the wall piercing grids to prevent a corridor from leaving
 * a room and then coming back in through the same entrance.
 *
 * We "pierce" grids which are "outer" walls of rooms, and when we
 * do so, we change all adjacent "outer" walls of rooms into "solid"
 * walls so that no two corridors may use adjacent grids for exits.
 *
 * The "solid" wall check prevents corridors from "chopping" the
 * corners of rooms off, as well as "silly" door placement, and
 * "excessively wide" room entrances.
 *
 * Useful "feat" values: XXX XXX XXX
 *   FEAT_WALL_EXTRA -- granite walls
 *   FEAT_WALL_INNER -- inner room walls
 *   FEAT_WALL_OUTER -- outer room walls
 *   FEAT_WALL_SOLID -- solid room walls
 *   FEAT_PERM_EXTRA -- shop walls (perma)
 *   FEAT_PERM_INNER -- inner room walls (perma)
 *   FEAT_PERM_OUTER -- outer room walls (perma)
 *   FEAT_PERM_SOLID -- dungeon border (perma)
 */
static void build_tunnel(int row1, int col1, int row2, int col2)
{
	int y, x;
	int tmp_row, tmp_col;
	int row_dir, col_dir;
	int start_row, start_col;
	int main_loop_count = 0;

	bool door_flag = FALSE;

	cave_type *c_ptr;
	s16b pval; /* Prfnoff */


	/* Save the starting location */
	start_row = row1;
	start_col = col1;

	/* Start out in the correct direction */
	correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

	/* Keep going until done (or bored) */
	while ((row1 != row2) || (col1 != col2))
	{
		/* Mega-Hack -- Paranoia -- prevent infinite loops */
		if (main_loop_count++ > 2000) break;

		/* Allow bends in the tunnel */
		if (rand_int(100) < dun_tun_chg)
		{
			/* Acquire the correct direction */
			correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Random direction */
			if (rand_int(100) < dun_tun_rnd)
			{
				rand_dir(&row_dir, &col_dir);
			}
		}

		/* Get the next location */
		tmp_row = row1 + row_dir;
		tmp_col = col1 + col_dir;


		/* Extremely Important -- do not leave the dungeon */
		while (!in_bounds(tmp_row, tmp_col))
		{
			/* Acquire the correct direction */
			correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Random direction */
			if (rand_int(100) < dun_tun_rnd)
			{
				rand_dir(&row_dir, &col_dir);
			}

			/* Get the next location */
			tmp_row = row1 + row_dir;
			tmp_col = col1 + col_dir;
		}


		/* Access the location */
		c_ptr = &cave[tmp_row][tmp_col];

		/* Extract the "power" index -- Prfnoff */
		pval = cave_pval_grid(c_ptr);


		/* Avoid the edge of the dungeon and vaults -- Prfnoff */
		if (cave_permwall_grid(c_ptr)) continue;

		/* Avoid "solid" granite walls -- Prfnoff */
		if (cave_granite_grid(c_ptr) && (pval == PV_WALL_SOLID)) continue;

		/* Pierce "outer" walls of rooms */
		if (cave_granite_grid(c_ptr) && (pval == PV_WALL_OUTER))
		{
			/* Acquire the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Hack -- Avoid outer/solid permanent walls */
			if (cave_permwall_bold(y, x) && /* Prfnoff */
		        ((cave_pval_bold(y, x) == PV_PERMA_SOLID) ||
		         (cave_pval_bold(y, x) == PV_PERMA_OUTER))) continue;

			/* Hack -- Avoid outer/solid granite walls */
			if (cave_granite_bold(y, x) && /* Prfnoff */
		        ((cave_pval_bold(y, x) == PV_WALL_SOLID) ||
		         (cave_pval_bold(y, x) == PV_WALL_OUTER))) continue;

			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Save the wall location */
			if (dun->wall_n < WALL_MAX)
			{
				dun->wall[dun->wall_n].y = row1;
				dun->wall[dun->wall_n].x = col1;
				dun->wall_n++;
			}

			/* Forbid re-entry near this piercing */
			for (y = row1 - 1; y <= row1 + 1; y++)
			{
				for (x = col1 - 1; x <= col1 + 1; x++)
				{
					/* Convert adjacent "outer" walls as "solid" walls */
					if (cave_granite_bold(y, x) && /* Prfnoff */
					    (cave_pval_bold(y, x) == PV_WALL_OUTER))
					{
						/* Change the wall to a "solid" wall */
						place_solid_wall(y, x);
					}
				}
			}
		}

		/* Travel quickly through rooms */
		else if (c_ptr->info & (CAVE_ROOM))
		{
			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;
		}

		/* Tunnel through all other walls */
		else if (cave_granite_grid(c_ptr) || cave_permwall_grid(c_ptr)) /* Prfnoff */
		{
			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Save the tunnel location */
			if (dun->tunn_n < TUNN_MAX)
			{
				dun->tunn[dun->tunn_n].y = row1;
				dun->tunn[dun->tunn_n].x = col1;
				dun->tunn_n++;
			}

			/* Allow door in next grid */
			door_flag = FALSE;
		}

		/* Handle corridor intersections or overlaps */
		else
		{
			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Collect legal door locations */
			if (!door_flag)
			{
				/* Save the door location */
				if (dun->door_n < DOOR_MAX)
				{
					dun->door[dun->door_n].y = row1;
					dun->door[dun->door_n].x = col1;
					dun->door_n++;
				}

				/* No door in next grid */
				door_flag = TRUE;
			}

			/* Hack -- allow pre-emptive tunnel termination */
			if (rand_int(100) >= dun_tun_con)
			{
				/* Distance between row1 and start_row */
				tmp_row = row1 - start_row;
				if (tmp_row < 0) tmp_row = (-tmp_row);

				/* Distance between col1 and start_col */
				tmp_col = col1 - start_col;
				if (tmp_col < 0) tmp_col = (-tmp_col);

				/* Terminate the tunnel */
				if ((tmp_row > 10) || (tmp_col > 10)) break;
			}
		}
	}
}


/*
 * This routine adds the square to the tunnel
 * It also checks for SOLID walls - and returns a nearby
 * non-SOLID square in (x,y) so that a simple avoiding
 * routine can be used. The returned boolean value reflects
 * whether or not this routine hit a SOLID wall.
 *
 * "affectwall" toggles whether or not this new square affects
 * the boundaries of rooms. - This is used by the catacomb
 * routine.
 */
static bool set_tunnel(int *x, int *y, bool affectwall)
{
	int i, j, dx, dy;
	cave_type *c_ptr;
	s16b pval;


	if (!in_bounds(*y, *x)) return TRUE;

	c_ptr = &cave[*y][*x];

	pval = cave_pval_grid(c_ptr);

	if ((cave_permwall_grid(c_ptr) && /* Prfnoff */
	     ((pval == PV_PERMA_SOLID) || (pval == PV_PERMA_OUTER))) ||
	    (cave_granite_grid(c_ptr) && (pval == PV_WALL_INNER)))
	{
		/*
		 * Ignore permanent walls - sometimes cannot tunnel around them anyway
		 * so don't try - it just complicates things unnecessarily.
		 */
		return TRUE;
	}

	if (cave_granite_grid(c_ptr) && (pval == PV_WALL_EXTRA))
	{
		/* Save the tunnel location */
		if (dun->tunn_n < TUNN_MAX)
		{
			dun->tunn[dun->tunn_n].y = *y;
			dun->tunn[dun->tunn_n].x = *x;
			dun->tunn_n++;
		}

		return TRUE;
	}

	if (cave_plain_grid(c_ptr))
	{
		/* Don't do anything */
		return TRUE;
	}

	if (cave_granite_grid(c_ptr) && (pval == PV_WALL_OUTER) && affectwall)
	{
		/* Save the wall location */
		if (dun->wall_n < WALL_MAX)
		{
			dun->wall[dun->wall_n].y = *y;
			dun->wall[dun->wall_n].x = *x;
			dun->wall_n++;
		}

		/* Forbid re-entry near this piercing */
		for (j = *y - 1; j <= *y + 1; j++)
		{
			for (i = *x - 1; i <= *x + 1; i++)
			{
				/* Convert adjacent "outer" walls as "solid" walls */
				if (cave_granite_bold(j, i) &&
				    (cave_pval_bold(j, i) == PV_WALL_OUTER))
				{
					/* Change the wall to a "solid" wall */
					place_solid_wall(j, i);
				}
			}
		}
		place_floor(*y, *x);

		return TRUE;
	}

	if (cave_granite_grid(c_ptr) && (pval == PV_WALL_SOLID) && affectwall)
	{
		/* cannot place tunnel here - use a square to the side */

		/* find usable square and return value in (x,y) */

		i = 50;

		dy = 0;
		dx = 0;
		while ((i > 0) && cave_granite_bold(*y + dy, *x + dx) &&
		       (cave_pval_bold(*y + dy, *x + dx) == PV_WALL_SOLID))
		{
			dy = rand_int(3) - 1;
			dx = rand_int(3) - 1;

			if (!in_bounds(*y + dy, *x + dx))
			{
				dx = 0;
				dy = 0;
			}

			i--;
		}

		if (i == 0)
		{
			/* Failed for some reason: hack - ignore the solidness*/
			place_outer_wall(*y, *x);
			dx = 0;
			dy = 0;
		}

		/* Give new, acceptable coordinate. */
		*x = *x + dx;
		*y = *y + dy;

		return FALSE;
	}

	return TRUE;
}


/*
 * This routine creates the catacomb-like tunnels by removing extra rock.
 * Note that this routine is only called on "even" squares - so it gives
 * a natural checkerboard pattern.
 */
static void create_cata_tunnel(int x, int y)
{
	int x1, y1;

	/* Build tunnel */
	x1 = x - 1;
	y1 = y;
	set_tunnel(&x1, &y1, FALSE);

	x1 = x + 1;
	y1 = y;
	set_tunnel(&x1, &y1, FALSE);

	x1 = x;
	y1 = y - 1;
	set_tunnel(&x1, &y1, FALSE);

	x1 = x;
	y1 = y + 1;
	set_tunnel(&x1, &y1, FALSE);
}


/*
 * This routine does the bulk of the work in creating the new types of tunnels.
 * It is designed to use very simple algorithms to go from (x1,y1) to (x2,y2)
 * It doesn't need to add any complexity - straight lines are fine.
 * The SOLID walls are avoided by a recursive algorithm which tries random ways
 * around the obstical until it works.  The number of itterations is counted, and it
 * this gets too large the routine exits. This should stop any crashes - but may leave
 * small gaps in the tunnel where there are too many SOLID walls.
 *
 * Type 1 tunnels are extremely simple - straight line from A to B.  This is only used
 * as a part of the dodge SOLID walls algorithm.
 *
 * Type 2 tunnels are made of two straight lines at right angles. When this is used with
 * short line segments it gives the "cavelike" tunnels seen deeper in the dungeon.
 *
 * Type 3 tunnels are made of two straight lines like type 2, but with extra rock removed.
 * This, when used with longer line segments gives the "catacomb-like" tunnels seen near
 * the surface.
 */
static void short_seg_hack(int x1, int y1, int x2, int y2, int type, int count, bool *fail)
{
	int i, x, y;
	int length;

	/* Check for early exit */
	if (!(*fail)) return;

	length = distance(x1, y1, x2, y2);

	count++;

	if ((type == 1) && (length != 0))
	{

		for (i = 0; i <= length; i++)
		{
			x = x1 + i * (x2 - x1) / length;
			y = y1 + i * (y2 - y1) / length;
			if (!set_tunnel(&x, &y, TRUE))
			{
				if (count > 50)
				{
					/* This isn't working - probably have an infinite loop */
					*fail = FALSE;
					return;
				}

				/* solid wall - so try to go around */
				short_seg_hack(x, y, x1 + (i - 1) * (x2 - x1) / length, y1 + (i - 1) * (y2 - y1) / length, 1, count, fail);
				short_seg_hack(x, y, x1 + (i + 1) * (x2 - x1) / length, y1 + (i + 1) * (y2 - y1) / length, 1, count, fail);
			}
		}
	}
	else if ((type == 2) || (type == 3))
	{
		if (x1 < x2)
		{
			for (i = x1; i <= x2; i++)
			{
				x = i;
				y = y1;
				if (!set_tunnel(&x, &y, TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x, y, i - 1, y1, 1, count, fail);
					short_seg_hack(x, y, i + 1, y1, 1, count, fail);
				}
				if ((type == 3) && ((x + y) % 2))
				{
					create_cata_tunnel(i, y1);
				}
			}
		}
		else
		{
			for (i = x2; i <= x1; i++)
			{
				x = i;
				y = y1;
				if (!set_tunnel(&x, &y, TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x, y, i - 1, y1, 1, count, fail);
					short_seg_hack(x, y, i + 1, y1, 1, count, fail);
				}
				if ((type == 3) && ((x + y) % 2))
				{
					create_cata_tunnel(i, y1);
				}
			}

		}
		if (y1 < y2)
		{
			for (i = y1; i <= y2; i++)
			{
				x = x2;
				y = i;
				if (!set_tunnel(&x, &y, TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x, y, x2, i - 1, 1, count, fail);
					short_seg_hack(x, y, x2, i + 1, 1, count, fail);
				}
				if ((type == 3) && ((x + y) % 2))
				{
					create_cata_tunnel(x2, i);
				}
			}
		}
		else
		{
			for (i = y2; i <= y1; i++)
			{
				x = x2;
				y = i;
				if (!set_tunnel(&x, &y, TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x, y, x2, i - 1, 1, count, fail);
					short_seg_hack(x, y, x2, i + 1, 1, count, fail);
				}
				if ((type == 3) && ((x + y) % 2))
				{
					create_cata_tunnel(x2, i);
				}
			}
		}
	}
}


/*
 * This routine maps a path from (x1, y1) to (x2, y2) avoiding SOLID walls.
 * Permanent rock is ignored in this path finding- sometimes there is no
 * path around anyway -so there will be a crash if we try to find one.
 * This routine is much like the river creation routine in Zangband.
 * It works by dividing a line segment into two.  The segments are divided
 * until they are less than "cutoff" - when the corresponding routine from
 * "short_seg_hack" is called.
 * Note it is VERY important that the "stop if hit another passage" logic
 * stays as is.  Without this the dungeon turns into Swiss Cheese...
 */
static bool build_tunnel2(int x1, int y1, int x2, int y2, int type, int cutoff)
{
	int x3, y3, dx, dy;
	int changex, changey;
	int length;
	int i;
	bool retval, firstsuccede;


	length = distance(x1, y1, x2, y2);

	if (length > cutoff)
	{
		/*
		 * Divide path in half and call routine twice.
		 */
		dx = (x2 - x1) / 2;
		dy = (y2 - y1) / 2;

		/* perturbation perpendicular to path */
		changex = (rand_int(abs(dy) + 2) * 2 - abs(dy) - 1) / 2;

		/* perturbation perpendicular to path */
		changey = (rand_int(abs(dx) + 2) * 2 - abs(dx) - 1) / 2;

		/* Work out "mid" ponit */
		x3 = x1 + dx + changex;
		y3 = y1 + dy + changey;

		/* See if in bounds - if not - do not perturb point*/
		if (!in_bounds(y3, x3))
		{
			x3 = (x1 + x2) / 2;
			y3 = (y1 + y2) / 2;
		}

		if (cave_granite_bold(y3, x3) && (cave_pval_bold(y3, x3) == PV_WALL_SOLID))
		{
			/* move midpoint a bit to avoid problem. */

			i = 50;

			dy = 0;
			dx = 0;
			while ((i > 0) && cave_granite_bold(y3 + dy, x3 + dy) &&
			       (cave_pval_bold(y3 + dy, x3 + dy) == PV_WALL_SOLID))
			{
				dy = rand_int(3) - 1;
				dx = rand_int(3) - 1;
				if (!in_bounds(y3 + dy, x3 + dx))
				{
					dx = 0;
					dy = 0;
				}
				i--;
			}

			if (i == 0)
			{
				/* Failed for some reason: hack - ignore the solidness*/
				place_outer_wall(y3, x3);
				dx = 0;
				dy = 0;
			}
			y3 += dy;
			x3 += dx;
		}

		if (cave_plain_bold(y3, x3))
		{
			if (build_tunnel2(x1, y1, x3, y3, type, cutoff))
			{
				if ((cave[y3][x3].info & CAVE_ROOM) || (randint(100) > 95))
				{
					/* do second half only if works + if have hit a room */
					retval = build_tunnel2(x3, y3, x2, y2, type, cutoff);
				}
				else
				{
					/* have hit another tunnel - make a set of doors here */
					retval = FALSE;

					/* Save the door location */
					if (dun->door_n < DOOR_MAX)
					{
						dun->door[dun->door_n].y = y3;
						dun->door[dun->door_n].x = x3;
						dun->door_n++;
					}
				}
				firstsuccede = TRUE;
			}
			else
			{
				/* false- didn't work all the way */
				retval = FALSE;
				firstsuccede = FALSE;
			}
		}
		else
		{
			/* tunnel through walls */
			if (build_tunnel2(x1, y1, x3, y3, type, cutoff))
			{
				retval = build_tunnel2(x3, y3, x2, y2, type, cutoff);
				firstsuccede = TRUE;
			}
			else
			{
				/* false- didn't work all the way */
				retval = FALSE;
				firstsuccede = FALSE;
			}
		}
		if (firstsuccede)
		{
			/* only do this if the first half has worked */
			set_tunnel(&x3, &y3, TRUE);
		}
		/* return value calculated above */
		return retval;
	}
	else
	{
		/* Do a short segment */
		retval = TRUE;
		short_seg_hack(x1, y1, x2, y2, type, 0, &retval);

		/* Hack - ignore return value so avoid infinite loops */
		return TRUE;
	}
}


/*
 * Place a tunnel from stored tunnel data -- Prfnoff
 */
static void place_tunnel(void)
{
	int i, y, x;


	/* Turn the tunnel into corridor */
	for (i = 0; i < dun->tunn_n; i++)
	{
		/* Access the grid */
		y = dun->tunn[i].y;
		x = dun->tunn[i].x;

		/* Clear previous contents (if not a lake), add a floor */
		if (!cave_terrain_bold(y, x) || /* Prfnoff */
		    ((cave_pval_bold(y, x) != PV_TERRAIN_WATER) &&
		     (cave_pval_bold(y, x) != PV_TERRAIN_LAVA)))
		{
			place_floor(y, x);
		}
	}


	/* Apply the piercings that we found */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Access the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* Clear previous contents, add a floor */
		place_floor(y, x);

		/* Occasional doorway */
		if (rand_int(100) < dun_tun_pen)
		{
			/* Place a random door */
			place_random_door(y, x);
		}
	}
}




/*
 * Count the number of "corridor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(y1, x1)"
 *
 * XXX XXX This routine currently only counts actual "empty floor"
 * grids which are not in rooms.  We might want to also count stairs,
 * open doors, closed doors, etc.
 */
static int next_to_corr(int y1, int x1)
{
	int i, y, x, k = 0;

	cave_type *c_ptr;

	/* Scan adjacent grids */
	for (i = 0; i < 4; i++)
	{
		/* Extract the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Skip non floors */
		if (!cave_floor_bold(y, x)) continue;

		/* Access the grid */
		c_ptr = &cave[y][x];

		/* Skip non "empty floor" grids */
		if (!cave_plain_grid(c_ptr)) continue; /* Prfnoff */

		/* Skip grids inside rooms */
		if (c_ptr->info & (CAVE_ROOM)) continue;

		/* Count these grids */
		k++;
	}

	/* Return the number of corridors */
	return (k);
}


/*
 * Determine if the given location is "between" two walls,
 * and "next to" two corridor spaces.  XXX XXX XXX
 *
 * Assumes "in_bounds(y, x)"
 */
static bool possible_doorway(int y, int x)
{
	/* Count the adjacent corridors */
	if (next_to_corr(y, x) >= 2)
	{
		/* Check Vertical */
		if (cave_realwall_bold(y-1, x) && cave_realwall_bold(y+1, x)) /* Prfnoff */
		{
			return (TRUE);
		}

		/* Check Horizontal */
		if (cave_realwall_bold(y, x-1) && cave_realwall_bold(y, x+1)) /* Prfnoff */
		{
			return (TRUE);
		}
	}

	/* No doorway */
	return (FALSE);
}


/*
 * Places door at y, x position if at least 2 walls found
 */
static void try_door(int y, int x)
{
	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Ignore walls */
	if (cave_realwall_bold(y, x)) return;

	/* Ignore room grids */
	if (cave[y][x].info & (CAVE_ROOM)) return;

	/* Occasional door (if allowed) */
	if ((rand_int(100) < dun_tun_jct) && possible_doorway(y, x))
	{
		/* Place a door */
		place_random_door(y, x);
	}
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(void)
{
	int i, j, k, y, x, y1, x1;

	int max_vault_ok = 2;

	bool destroyed = FALSE;
	bool empty_level = FALSE;
	bool cavern = FALSE;
	int laketype = 0;

	dun_data dun_body;

	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), NULL);

	/* Global data */
	dun = &dun_body;

	if (!max_panel_rows) max_vault_ok--;
	if (!max_panel_cols) max_vault_ok--;

	/* Randomize the dungeon creation values */
	dun_rooms = rand_range(DUN_ROOMS_MIN, DUN_ROOMS_MAX);
	dun_tun_rnd = rand_range(DUN_TUN_RND_MIN, DUN_TUN_RND_MAX);
	dun_tun_chg = rand_range(DUN_TUN_CHG_MIN, DUN_TUN_CHG_MAX);
	dun_tun_con = rand_range(DUN_TUN_CON_MIN, DUN_TUN_CON_MAX);
	dun_tun_pen = rand_range(DUN_TUN_PEN_MIN, DUN_TUN_PEN_MAX);
	dun_tun_jct = rand_range(DUN_TUN_JCT_MIN, DUN_TUN_JCT_MAX);

	/* Prfnoff added ironman option */
	if (ironman_empty_levels || (empty_levels && (randint(EMPTY_LEVEL) == 1)))
	{
		empty_level = TRUE;
		if (cheat_room)
			msg_print("Arena level.");
	}


	/* Hack -- Start with basic granite */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			if (empty_level)
				place_floor(y, x);
			else
				/* Create granite wall */
				place_extra_wall(y, x);
		}
	}

	/* Possible "destroyed" level */
	if ((dun_level > 10) && (rand_int(DUN_DEST) == 0) && small_levels)
	{
		destroyed = TRUE;

#ifdef ALLOW_CAVERNS_AND_LAKES
		/* extra rubble around the place looks cool */
		build_lake(3);
#endif /* ALLOW_CAVERNS_AND_LAKES */
	}

#ifdef ALLOW_CAVERNS_AND_LAKES
	/* Make a lake some of the time */
	if (!terrain_streams && (rand_int(LAKE_LEVEL) == 0) &&
	    !empty_level && !destroyed)
	{
		/* Lake of Water */
		if (dun_level > 30) laketype = 2;

		/* Lake of Lava */
		if (dun_level > 60) laketype = 1;

		if (laketype != 0)
		{
			if (cheat_room)
				msg_print("Lake on the level.");
			build_lake(laketype);
		}
	}

	if ((dun_level > DUN_CAVERN) && !empty_level &&
	    (laketype == 0) && !destroyed && (randint(200) < dun_level))
	{
		cavern = TRUE;

		/* make a large fractal cave in the middle of the dungeon */

		if (cheat_room)
			msg_print("Cavern on level.");

		build_cavern();
	}
#endif /* ALLOW_CAVERNS_AND_LAKES */

	/* Hack -- No destroyed "quest" levels */
	if (quest_number(dun_level)) destroyed = FALSE;

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = cur_hgt / BLOCK_HGT;
	dun->col_rooms = cur_wid / BLOCK_WID;

	/* Initialize the room table */
	for (y = 0; y < dun->row_rooms; y++)
	{
		for (x = 0; x < dun->col_rooms; x++)
		{
			dun->room_map[y][x] = FALSE;
		}
	}

	/* No "crowded" rooms yet */
	dun->crowded = 0;

	/* No rooms yet */
	dun->cent_n = 0;

	/* Build some rooms */
	for (i = 0; i < dun_rooms; i++)
	{
		/* Pick a block for the room */
		y = rand_int(dun->row_rooms);
		x = rand_int(dun->col_rooms);

		/* Align dungeon rooms */
		if (dungeon_align)
		{
			/* Slide some rooms right */
			if ((x % 3) == 0) x++;

			/* Slide some rooms left */
			if ((x % 3) == 2) x--;
		}

		/* Attempt an "unusual" room */ /* Prfnoff */
		if (ironman_rooms || (rand_int(DUN_UNUSUAL) < dun_level))
		{
			/* Roll for room type */ /* Prfnoff */
			k = (ironman_rooms ? 0 : rand_int(100));

			/* Attempt a very unusual room */ /* Prfnoff */
			if (ironman_rooms || (rand_int(DUN_UNUSUAL) < dun_level))
			{
				/* Type 8 -- Greater vault (7%) */
				if (k < 7)
				{
					if (max_vault_ok > 1)
					{
						if (room_build(y, x, 8)) continue;
					}
					else
					{
						if (cheat_room) msg_print("Refusing a greater vault.");
					}
				}

				/* Type 7 -- Lesser vault (10%) */
				if (k < 17)
				{
					if (max_vault_ok > 0)
					{
						if (room_build(y, x, 7)) continue;
					}
					else
					{
						if (cheat_room) msg_print("Refusing a lesser vault.");
					}
				}


				/* Type 5 -- Monster nest (15%) */
				if ((k < 32) && room_build(y, x, 5)) continue;

				/* Type 6 -- Monster pit (10%) */
				if ((k < 42) && room_build(y, x, 6)) continue;

				/* Type 10 -- Random vault (8%) */
				if ((k < 50) && room_build(y, x, 10)) continue;
			}

			/* Type 4 -- Large room (25%) */
			if ((k < 25) && room_build(y, x, 4)) continue;

			/* Type 3 -- Cross room (25%) */
			if ((k < 50) && room_build(y, x, 3)) continue;

			/* Type 2 -- Overlapping (25%) */
			if ((k < 75) && room_build(y, x, 2)) continue;

			/* Type 11 -- Circular (10%)*/
			if ((k < 85) && room_build(y, x, 10)) continue;

			/* Type 12 -- Crypt (15%) */
			if ((k < 100) && room_build(y, x, 9)) continue;
		}

		/* The deeper you are, the more cavelike the rooms are */
		k = randint(100);

		/* No caves when a cavern exists: they look bad */
		if ((k < dun_level) && (!cavern) && (!empty_level) && (laketype == 0))
		{
			/* Type 9 -- Fractal cave */
			if (room_build(y, x, 9)) continue;
		}
		else
		{
			/* Attempt a "trivial" room */
			if (room_build(y, x, 1)) continue;
		}
		continue;
	}

	/* Make a hole in the dungeon roof sometimes at level 1 */
	if (terrain_streams && (dun_level == 1)) /* Prfnoff */
	{
		while (randint(DUN_MOS_DEN) == 1)
		{
			place_trees(randint(cur_wid - 2), randint(cur_hgt - 2));
		}
	}

	/* Destroy the level if necessary */
	if (destroyed) destroy_level();

	/* Hack -- Add some rivers */
	if (terrain_streams && (randint(3) == 1) && (randint(dun_level) > 5)) /* Prfnoff */
	{
		s16b pval;

		/* Choose water or lava */
		if ((rand_int(MAX_DEPTH) - 1) > dun_level)
		{
			pval = PV_TERRAIN_WATER;
		}
		else
		{
			pval = PV_TERRAIN_LAVA;
		}


	 	/* Only add river if matches lake type or if have no lake at all */
	 	if (((laketype == 1) && (pval == PV_TERRAIN_LAVA)) ||
	 	    ((laketype == 2) && (pval == PV_TERRAIN_WATER)) ||
		     (laketype == 0))
	 	{
			add_river(pval);
		}
	}


	/* Special boundary walls -- Top */
	for (x = 0; x < cur_wid; x++)
	{
		place_solid_perm(0, x);
	}

	/* Special boundary walls -- Bottom */
	for (x = 0; x < cur_wid; x++)
	{
		place_solid_perm(cur_hgt - 1, x);
	}

	/* Special boundary walls -- Left */
	for (y = 0; y < cur_hgt; y++)
	{
		place_solid_perm(y, 0);
	}

	/* Special boundary walls -- Right */
	for (y = 0; y < cur_hgt; y++)
	{
		place_solid_perm(y, cur_wid - 1);
	}


	/* Hack -- Scramble the room order */
	for (i = 0; i < dun->cent_n; i++)
	{
		int pick1 = rand_int(dun->cent_n);
		int pick2 = rand_int(dun->cent_n);
		y1 = dun->cent[pick1].y;
		x1 = dun->cent[pick1].x;
		dun->cent[pick1].y = dun->cent[pick2].y;
		dun->cent[pick1].x = dun->cent[pick2].x;
		dun->cent[pick2].y = y1;
		dun->cent[pick2].x = x1;
	}

	/* Start with no tunnel doors */
	dun->door_n = 0;

	/* Hack -- connect the first room to the last room */
	y = dun->cent[dun->cent_n-1].y;
	x = dun->cent[dun->cent_n-1].x;

	/* Connect all the rooms together */
	for (i = 0; i < dun->cent_n; i++)
	{
		/* Reset the arrays */
		dun->tunn_n = 0;
		dun->wall_n = 0;

		/* Connect the room to the previous room */
		if (pillar_tunnels && (randint(20) > dun_level) && (randint(100) < 25))
		{
			/* make catacomb-like tunnel */
			build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y, 3, 30);
		}
		else if (randint(dun_level) > 25)
		{
			/* make cave-like tunnel */
			build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y, 2, 2);
		}
		else
		{
			/* make normal tunnel */
			build_tunnel(dun->cent[i].y, dun->cent[i].x, y, x);
		}

		/* Place the tunnel -- Prfnoff */
		place_tunnel();

		/* Remember the "previous" room */
		y = dun->cent[i].y;
		x = dun->cent[i].x;
	}

	/* Place intersection doors	 */
	for (i = 0; i < dun->door_n; i++)
	{
		/* Extract junction location */
		y = dun->door[i].y;
		x = dun->door[i].x;

		/* Try placing doors */
		try_door(y, x - 1);
		try_door(y, x + 1);
		try_door(y - 1, x);
		try_door(y + 1, x);
	}


	/* Hack -- Add some magma streamers */
	for (i = 0; i < DUN_STR_MAG; i++)
	{
		build_streamer(FF1_VEIN, DUN_STR_MC);
	}

	/* Hack -- Add some quartz streamers */
	for (i = 0; i < DUN_STR_QUA; i++)
	{
		build_streamer((FF1_VEIN | FF1_HARD), DUN_STR_QC);
	}

	/* Place 3 or 4 down stairs near some walls */
	alloc_stairs((FF1_STAIR | FF1_HARD), rand_range(3, 4), 3);

	/* Place 1 or 2 up stairs near some walls */
	alloc_stairs(FF1_STAIR, rand_range(1, 2), 3);


	/* Determine the character location */
	if (!new_player_spot())
		return FALSE;

	/* Handle the quest monster placements */
	for (i = 0; i < max_quests; i++)
	{
		if ((quest[i].status == QUEST_STATUS_TAKEN) &&
		    ((quest[i].type == QUEST_TYPE_KILL_LEVEL) ||
		    (quest[i].type == QUEST_TYPE_RANDOM)) &&
		    (quest[i].level == dun_level) &&
			!(quest[i].flags & QUEST_FLAG_PRESET))
		{
			monster_race *r_ptr = &r_info[quest[i].r_idx];

			/* Hack -- "unique" monsters must be "unique" */
			if ((r_ptr->flags1 & RF1_UNIQUE) &&
			    (r_ptr->cur_num >= r_ptr->max_num))
			{
				/* The unique is already dead */
				quest[i].status = QUEST_STATUS_FINISHED;
			}
			else
			{
				bool group;

				/* Hard quests -> revive all monsters */
				if (ironman_hard_quests)
				{
					quest[i].cur_num = 0;
				}

				for (j = 0; j < (quest[i].max_num - quest[i].cur_num); j++)
				{
					for (k = 0; k < SAFE_MAX_ATTEMPTS; k++)
					{
						/* Find an empty grid */
						while (TRUE)
						{
							y = rand_int(cur_hgt);
							x = rand_int(cur_wid);
							if (!cave_naked_bold(y, x)) continue;

							/* No random quests for aquatic monsters */
							if (!cave_plain_bold(y, x)) continue; /* Prfnoff */

							if (distance(y, x, py, px) < 10) continue;
							else break;
						}

						if (r_ptr->flags1 & RF1_FRIENDS)
							group = FALSE;
						else
							group = TRUE;

						/* Try to place the monster */
						if (place_monster_aux(y, x, quest[i].r_idx, FALSE, group, FALSE, FALSE))
						{
							/* Success */
							break;
						}
						else
						{
							/* Failure - Try again */
							continue;
						}
					}
				}
			}
		}
	}


	/* Basic "amount" */
	k = (dun_level / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;

	/* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL;

	/* To make small levels a bit more playable */
	if (cur_hgt < MAX_HGT || cur_wid < MAX_WID)
	{
		int small_tester = i;

		i = (i * cur_hgt) / MAX_HGT;
		i = (i * cur_wid) / MAX_WID;
		i += 1;

		if (i > small_tester) i = small_tester;
		else if (cheat_hear)
		{
			msg_format("Reduced monsters base from %d to %d", small_tester, i);
		}
	}

	i += randint(8);

	/* Put some monsters in the dungeon */
	for (i = i + k; i > 0; i--)
	{
		(void)alloc_monster(0, TRUE);
	}

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ROOM, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, randnor(DUN_AMT_GOLD, 3));

	if (empty_level && ((randint(DARK_EMPTY) != 1) || (randint(100) > dun_level)))
	{
		/* Lite the cave */
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				cave[y][x].info |= (CAVE_GLOW);
			}
		}
	}

	return TRUE;
}


/*
 * Generate a quest level
 */
static void quest_gen(void)
{
	int x, y;


	/* Start with perm walls */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			place_solid_perm(y, x);
		}
	}

	/* Set the quest level */
	base_level = quest[p_ptr->inside_quest].level;
	dun_level = base_level;
	object_level = base_level;
	monster_level = base_level;

	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), NULL);

	init_flags = INIT_CREATE_DUNGEON | INIT_ASSIGN;
	process_dungeon_file("q_info.txt", 0, 0, MAX_HGT, MAX_WID);
}


/* Make a real level */
static bool level_gen(cptr *why)
{
	int level_height, level_width;

	if (ironman_small_levels || /* Prfnoff */
	    ((randint(SMALL_LEVEL) == 1) && small_levels))
	{
		if (cheat_room)
			msg_print("A 'small' dungeon level.");

		if (ironman_small_levels) /* Prfnoff */
		{
			level_height = 1;
			level_width = 1;
		}
		else
		{
			do
			{
				level_height = randint(MAX_HGT / SCREEN_HGT);
				level_width = randint(MAX_WID / SCREEN_WID);
			}
			while ((level_height == (MAX_HGT / SCREEN_HGT)) &&
			       (level_width == (MAX_WID / SCREEN_WID)));
		}

		cur_hgt = level_height * SCREEN_HGT;
		cur_wid = level_width * SCREEN_WID;

		/* Determine number of panels */
		max_panel_rows = level_height * 2 - 2;
		max_panel_cols = level_width * 2 - 2;

		/* Assume illegal panel */
		panel_row = max_panel_rows;
		panel_col = max_panel_cols;

		if (cheat_room)
		  msg_format("X:%d, Y:%d.", max_panel_cols, max_panel_rows);
	}
	else
	{
		/* Big dungeon */
		cur_hgt = MAX_HGT;
		cur_wid = MAX_WID;

		/* Determine number of panels */
		max_panel_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
		max_panel_cols = (cur_wid / SCREEN_WID) * 2 - 2;

		/* Assume illegal panel */
		panel_row = max_panel_rows;
		panel_col = max_panel_cols;
	}

	/* Make a dungeon */
	if (!cave_gen())
	{
		*why = "could not place player";
		return FALSE;
	}
	else return TRUE;
}

static byte extract_feeling(void)
{
	/* It takes 1000 game turns for "feelings" to recharge */
	if ((turn - old_turn) < 1000) return 0;

	/* Hack -- no feeling in the town */
	if (!dun_level) return 0;

	/* Hack -- Have a special feeling sometimes */
	if (good_item_flag && !p_ptr->preserve) return 1;

 	if (rating > 100) return 2;
	if (rating > 80) return 3;
	if (rating > 60) return 4;
	if (rating > 40) return 5;
	if (rating > 30) return 6;
	if (rating > 20) return 7;
	if (rating > 10) return 8;
	if (rating > 0) return 9;

	return 10;
}


/*
 * Generates a random dungeon level			-RAK-
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Hack -- allow auto-scumming via a gameplay option.
 */
void generate_cave(void)
{
	int y, x, num;


	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;

		cptr why = NULL;


		/* XXX XXX XXX XXX */
		o_max = 1;
		m_max = 1;

		/* Start with a blank cave */
		for (y = 0; y < MAX_HGT; y++)
		{
			for (x = 0; x < MAX_WID; x++)
			{
				/* No flags */
				cave[y][x].info = 0;

				/* No features */
				cave[y][x].feat = 0;

				/* No objects */
				cave[y][x].o_idx = 0;

				/* No monsters */
				cave[y][x].m_idx = 0;

				/* No mimic */
				cave[y][x].mimic = 0;

#ifdef MONSTER_FLOW
				/* No flow */
				cave[y][x].cost = 0;
				cave[y][x].when = 0;
#endif /* MONSTER_FLOW */
			}
		}

		/* Mega-Hack -- no player yet */
		px = py = 0;

		/* Mega-Hack -- no panel yet */
		panel_row_min = 0;
		panel_row_max = 0;
		panel_col_min = 0;
		panel_col_max = 0;

		/* Set the base level */
		base_level = dun_level;

		/* Reset the monster generation level */
		monster_level = base_level;

		/* Reset the object generation level */
		object_level = base_level;

		/* Nothing special here yet */
		good_item_flag = FALSE;

		/* Nothing good here yet */
		rating = 0;

#ifdef USE_SCRIPT
		if (!generate_level_callback(dun_level))
#endif /* USE_SCRIPT */
		{
			/* Quest levels -KMW- */
			if (p_ptr->inside_quest)
			{
				quest_gen();
			}

			/* Build the town */
			else if (!dun_level)
			{
				/* Make the wilderness */
				wilderness_gen();
			}

			/* Build a real level */
			else
			{
				okay = level_gen(&why);
			}
		}

		/* Extract the feeling */
		feeling = extract_feeling();


		/* Prevent object over-flow */
		if (o_max >= max_o_idx)
		{
			/* Message */
			why = "too many objects";

			/* Message */
			okay = FALSE;
		}

		/* Prevent monster over-flow */
		else if (m_max >= max_m_idx)
		{
			/* Message */
			why = "too many monsters";

			/* Message */
			okay = FALSE;
		}

		/* Mega-Hack -- "auto-scum" */
		if ((auto_scum || ironman_autoscum) && (num < 100) &&
		    !p_ptr->inside_quest)
		{
			/* Require "goodness" */
			if ((feeling > 9) ||
			    ((dun_level >= 5) && (feeling > 8)) ||
			    ((dun_level >= 10) && (feeling > 7)) ||
			    ((dun_level >= 20) && (feeling > 6)) ||
			    ((dun_level >= 40) && (feeling > 5)))
			{
				/* Give message to cheaters */
				if (cheat_room || cheat_hear ||
				    cheat_peek || cheat_xtra)
				{
					/* Message */
					why = "boring level";
				}

				/* Try again */
				okay = FALSE;
			}
		}

		/* Accept */
		if (okay) break;

		/* Message */
		if (why) msg_format("Generation restarted (%s)", why);

		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the monsters */
		wipe_m_list();
	}

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Remember when this level was "created" */
	old_turn = turn;
}
