/* File: generate.c */

/* Purpose: Dungeon generation */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

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

#include "angband.h"
#include "generate.h"
#include "grid.h"
#include "rooms.h"
#include "streams.h"

static int dun_rooms;

int dun_tun_rnd;
int dun_tun_chg;
int dun_tun_con;
int dun_tun_pen;
int dun_tun_jct;


/*
 * Dungeon generation data -- see "cave_gen()"
 */
dun_data *dun;

/*
 * Places some staircases near walls
 */
static bool alloc_stairs(int feat, int num, int walls)
{
	int y, x, i, j, flag;
	cave_type *c_ptr;

	if (feat == FEAT_LESS)
	{
		/* No up stairs in town or in ironman mode */
		if (ironman_downward || !p_ptr->depth) return TRUE;
	}
	else if (feat == FEAT_MORE)
	{
		/* No downstairs on quest levels */
		if (p_ptr->depth && is_quest_level(p_ptr->depth)) return TRUE;

		/* No downstairs at the bottom */
		if (p_ptr->depth >= MAX_DEPTH - 1) return TRUE;
	}

	/* Place "num" stairs */
	for (i = 0; i < num; i++)
	{
		/* Place some stairs */
		for (flag = FALSE; !flag;)
		{
			/* Try several times, then decrease "walls" */
			for (j = 0; !flag && j <= 10000; j++)
			{
				/* Pick a random grid */
				y = rand_range(p_ptr->min_hgt + 1, p_ptr->max_hgt - 2);
				x = rand_range(p_ptr->min_wid + 1, p_ptr->max_wid - 2);

				/* Access the grid */
				c_ptr = cave_p(x, y);

				/* Require "naked" floor grid */
				if (!cave_naked_grid(c_ptr)) continue;

				/* Require floor */
				if (c_ptr->feat != FEAT_FLOOR) continue;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(x, y) < walls) continue;

				/* Clear previous contents, add stairs */
				set_feat_grid(c_ptr, feat);

				/* All done */
				flag = TRUE;
			}

			/* If cannot find a blank spot - exit */
			if (!walls)
			{
				/* Placed at least one. */
				if (i > 0) return TRUE;

				/* Couldn't place any stairs */
				return FALSE;
			}

			/* Require fewer walls */
			walls--;
		}
	}

	/* Done */
	return TRUE;
}


/*
 * Allocates some objects (using "place" and "type")
 */
static void alloc_object(int set, int typ, int num)
{
	int x = 0, y = 0, k;
	int dummy = 0;
	cave_type *c_ptr = NULL;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		while (dummy < SAFE_MAX_ATTEMPTS)
		{
			bool room;

			dummy++;

			/* Location */
			y = rand_range(p_ptr->min_hgt + 1, p_ptr->max_hgt - 2);
			x = rand_range(p_ptr->min_wid + 1, p_ptr->max_wid - 2);

			c_ptr = cave_p(x, y);

			/* Require "naked" floor grid */
			if (!cave_naked_grid(c_ptr)) continue;

			/* Check for "room" */
			room = (c_ptr->info & CAVE_ROOM) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

			/* Traps cannot be placed on 'icky' grids (rivers/lakes) */
			if ((typ == ALLOC_TYP_TRAP) && (c_ptr->info & CAVE_ICKY)) continue;

			/* Accept it */
			break;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
				msg_print("Warning! Could not place object!");
			}
			return;
		}


		/* Place something */
		switch (typ)
		{
			case ALLOC_TYP_RUBBLE:
			{
				set_feat_grid(c_ptr, FEAT_RUBBLE);
				break;
			}

			case ALLOC_TYP_TRAP:
			{
				place_trap(x, y);
				break;
			}

			case ALLOC_TYP_GOLD:
			{
				place_gold(x, y);
				break;
			}

			case ALLOC_TYP_OBJECT:
			{
				place_object(x, y, FALSE, FALSE);
				break;
			}

			case ALLOC_TYP_INVIS:
			{
				/* Create invisible wall */
				set_feat_grid(c_ptr, FEAT_FLOOR);
				(void)place_field(x, y, FT_WALL_INVIS);
				break;
			}
		}
	}
}


/*
 * Count the number of "corridor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(x1, y1)"
 *
 * XXX XXX This routine currently only counts actual "empty floor"
 * grids which are not in rooms.  We might want to also count stairs,
 * open doors, closed doors, etc.
 */
static int next_to_corr(int x1, int y1)
{
	int i, y, x, k = 0;

	cave_type *c_ptr;

	/* Scan adjacent grids */
	for (i = 0; i < 4; i++)
	{
		/* Extract the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Access the grid */
		c_ptr = cave_p(x, y);

		/* Skip non floors */
		if (!cave_floor_grid(c_ptr)) continue;

		/* Skip non "empty floor" grids */
		if (c_ptr->feat != FEAT_FLOOR) continue;

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
 * Assumes "in_bounds(x, y)"
 */
static bool possible_doorway(int x, int y)
{
	/* Count the adjacent corridors */
	if (next_to_corr(x, y) >= 2)
	{
		/* Check Vertical */
		if ((cave_p(x, y - 1)->feat >= FEAT_MAGMA) &&
			(cave_p(x, y + 1)->feat >= FEAT_MAGMA))
		{
			return (TRUE);
		}

		/* Check Horizontal */
		if ((cave_p(x - 1, y)->feat >= FEAT_MAGMA) &&
			(cave_p(x + 1, y)->feat >= FEAT_MAGMA))
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
static void try_door(int x, int y)
{
	cave_type *c_ptr;

	/* Paranoia */
	if (!in_bounds(x, y)) return;

	c_ptr = cave_p(x, y);

	/* Ignore walls */
	if (c_ptr->feat >= FEAT_MAGMA) return;

	/* Ignore room grids */
	if (c_ptr->info & (CAVE_ROOM)) return;

	/* Occasional door (if allowed) */
	if ((randint0(100) < dun_tun_jct) && possible_doorway(x, y))
	{
		/* Place a door */
		place_random_door(x, y);
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

	int feat1, feat2;

	cave_type *c_ptr;

	bool destroyed = FALSE;
	bool empty_level = FALSE;
	bool cavern = FALSE;
	int laketype = 0;


	dun_data dun_body;

	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), NULL);

	/* Global data */
	dun = &dun_body;

	if (p_ptr->max_hgt - p_ptr->min_hgt < 23) max_vault_ok--;
	if (p_ptr->max_wid - p_ptr->min_wid < 34) max_vault_ok--;

	/* Hack - no vaults in moria mode */
	if (ironman_moria) max_vault_ok = 0;

	/* Randomize the dungeon creation values */
	dun_rooms = rand_range(DUN_ROOMS_MIN, DUN_ROOMS_MAX);
	dun_tun_rnd = rand_range(DUN_TUN_RND_MIN, DUN_TUN_RND_MAX);
	dun_tun_chg = rand_range(DUN_TUN_CHG_MIN, DUN_TUN_CHG_MAX);
	dun_tun_con = rand_range(DUN_TUN_CON_MIN, DUN_TUN_CON_MAX);
	dun_tun_pen = rand_range(DUN_TUN_PEN_MIN, DUN_TUN_PEN_MAX);
	dun_tun_jct = rand_range(DUN_TUN_JCT_MIN, DUN_TUN_JCT_MAX);

	/* Empty arena levels */
	if (ironman_empty_levels || (empty_levels && one_in_(EMPTY_LEVEL)))
	{
		empty_level = TRUE;

		if (cheat_room)
			msg_print("Arena level.");
	}


	/* Hack -- Start with basic granite */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
		{
			if (empty_level)
				set_feat_bold(x, y, FEAT_FLOOR);
			else
				/* Create granite wall */
				set_feat_bold(x, y, FEAT_WALL_EXTRA);
		}
	}

	/* Possible "destroyed" level */
	if ((p_ptr->depth > 15) && one_in_(DUN_DEST) && (small_levels))
	{
		destroyed = TRUE;

		/* extra rubble around the place looks cool */
		build_lake(LAKE_DESTROY);
	}

	/* Make a lake some of the time */
	if (one_in_(LAKE_LEVEL) && !empty_level && !destroyed && terrain_streams)
	{
		/* Lake of Water */
		if (p_ptr->depth > 52) laketype = LAKE_WATER;

		/* Lake of Lava */
		if (p_ptr->depth > 90) laketype = LAKE_LAVA;

		if (laketype != 0)
		{
			if (cheat_room)
				msg_print("Lake on the level.");
			build_lake(laketype);
		}
	}

	if (one_in_(DUN_CAV1 / (p_ptr->depth + DUN_CAV2)) && !empty_level &&
		(laketype == 0) && !destroyed && (p_ptr->depth >= MIN_CAVERN))
	{
		cavern = TRUE;

		/* make a large fractal cave in the middle of the dungeon */

		if (cheat_room)
			msg_print("Cavern on level.");

		build_cavern();
	}

	/* Hack -- No destroyed "quest" levels */
	if (is_quest_level(p_ptr->depth)) destroyed = FALSE;

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = (p_ptr->max_hgt - p_ptr->min_hgt) / BLOCK_HGT;
	dun->col_rooms = (p_ptr->max_wid - p_ptr->min_hgt) / BLOCK_WID;

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
		y = randint0(dun->row_rooms);
		x = randint0(dun->col_rooms);

		/* Align dungeon rooms */
		if (dungeon_align)
		{
			/* Slide some rooms right */
			if ((x % 3) == 0) x++;

			/* Slide some rooms left */
			if ((x % 3) == 2) x--;
		}

		/* Attempt an "unusual" room */
		if (ironman_rooms || (randint0(DUN_UNUSUAL) < p_ptr->depth))
		{
			/* Roll for room type */
			k = randint0(100);

			/* Attempt a very unusual room */
			if ((ironman_rooms && (randint0(DUN_UNUSUAL) < p_ptr->depth * 2)) ||
				(randint0(DUN_UNUSUAL) < p_ptr->depth))
			{
#ifdef FORCE_V_IDX
				if (room_build(x, y, 8)) continue;
#else
				/* Type 8 -- Greater vault (10%) */
				if (k < 10)
				{
					if (max_vault_ok > 1)
					{
						if (room_build(x, y, 8)) continue;
					}
					else
					{
						if (cheat_room)
							msg_format("Refusing a greater vault. %d",
									   max_vault_ok);
					}
				}

				/* Type 7 -- Lesser vault (13%) */
				if (k < 23)
				{
					if (max_vault_ok > 0)
					{
						if (room_build(x, y, 7)) continue;
					}
					else
					{
						if (cheat_room) msg_print("Refusing a lesser vault.");
					}
				}


				/* Type 5 -- Monster nest (8%) */
				if ((k < 31) && room_build(x, y, 5)) continue;

				/* Type 6 -- Monster pit (5%) */
				if ((k < 36) && room_build(x, y, 6)) continue;

				/* Type 10 -- Random vault (11%) */
				if (k < 47)
				{
					if (max_vault_ok > 0)
					{
						if (room_build(x, y, 10)) continue;
					}
					else
					{
						if (cheat_room) msg_print("Refusing a random vault.");
					}
				}
#endif

			}

			/* Type 4 -- Large room (15%) */
			if ((k < 15) && room_build(x, y, 4)) continue;

			/* Type 14 -- Large room (10%) */
			if ((k < 25) && room_build(x, y, 14)) continue;

			/* Type 13 -- Large Feature room (5%) */
			if ((k < 30) && room_build(x, y, 13)) continue;

			/* Type 3 -- Cross room (20%) */
			if ((k < 50) && room_build(x, y, 3)) continue;

			/* Type 2 -- Overlapping (25%) */
			if ((k < 75) && room_build(x, y, 2)) continue;

			/* Type 11 -- Parallelagram (5%) */
			if ((k < 80) && room_build(x, y, 15)) continue;

			/* Type 11 -- Circular (5%) */
			if ((k < 85) && room_build(x, y, 11)) continue;

			/* Type 12 -- Crypt (15%) */
			if ((k < 100) && room_build(x, y, 12)) continue;
		}

		/* The deeper you are, the more cavelike the rooms are */
		k = randint1(100);

		/* No caves when a cavern exists: they look bad */
		if ((k < p_ptr->depth) && (!cavern) && (!empty_level)
			&& (laketype == 0))
		{
			/* Type 9 -- Fractal cave */
			if (room_build(x, y, 9)) continue;
		}
		else
		{
			/* Attempt a "trivial" room */
			if (room_build(x, y, 1)) continue;
		}
	}

	/* Make a hole in the dungeon roof sometimes at level 1 */
	if ((p_ptr->depth == 1) && terrain_streams)
	{
		while (one_in_(DUN_MOS_DEN))
		{
			place_trees(rand_range(p_ptr->min_wid + 1, p_ptr->max_wid - 2),
						rand_range(p_ptr->min_hgt + 1, p_ptr->max_hgt - 2));
		}
	}

	/* Destroy the level if necessary */
	if (destroyed) destroy_level();

	/* Hack -- Add some rivers */
	if (one_in_(3) && (randint1(p_ptr->depth) > 5) && terrain_streams)
	{
		/* Choose water or lava */
		if (randint0(MAX_DEPTH * 2) > p_ptr->depth)
		{
			feat1 = FEAT_DEEP_WATER;
			feat2 = FEAT_SHAL_WATER;
		}
		else
		{
			feat1 = FEAT_DEEP_LAVA;
			feat2 = FEAT_SHAL_LAVA;
		}


		/* Only add river if matches lake type or if have no lake at all */
		if (((laketype == 1) && (feat1 == FEAT_DEEP_LAVA)) ||
			((laketype == 2) && (feat1 == FEAT_DEEP_WATER)) || (laketype == 0))
		{
			add_river(feat1, feat2);
		}
	}

	/* Special boundary walls -- Top */
	for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		set_feat_bold(x, p_ptr->min_hgt, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Bottom */
	for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		set_feat_bold(x, p_ptr->max_hgt - 1, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Left */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		set_feat_bold(p_ptr->min_wid, y, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Right */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		set_feat_bold(p_ptr->max_wid - 1, y, FEAT_PERM_SOLID);
	}


	/* Hack -- Scramble the room order */
	for (i = 0; i < dun->cent_n; i++)
	{
		int pick1 = randint0(dun->cent_n);
		int pick2 = randint0(dun->cent_n);
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
	y = dun->cent[dun->cent_n - 1].y;
	x = dun->cent[dun->cent_n - 1].x;

	/* Connect all the rooms together */
	for (i = 0; i < dun->cent_n; i++)
	{

		/* Reset the arrays */
		dun->tunn_n = 0;
		dun->wall_n = 0;

		/* Connect the room to the previous room */
#ifdef PILLAR_TUNNELS

		if ((randint1(20) > p_ptr->depth) && one_in_(4))
		{
			/* make catacomb-like tunnel */
			(void)build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y, 3, 30);
		}
		else if (randint1(p_ptr->depth) > 50)
#else
		if (randint1(p_ptr->depth) > 50)
#endif /* PILLAR_TUNNELS */
		{
			/* make cave-like tunnel */
			(void)build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y, 2, 2);
		}
		else
		{
			/* make normal tunnel */
			build_tunnel(dun->cent[i].x, dun->cent[i].y, x, y);
		}

		/* Turn the tunnel into corridor */
		for (j = 0; j < dun->tunn_n; j++)
		{
			/* Access the grid */
			y = dun->tunn[j].y;
			x = dun->tunn[j].x;

			/* Access the grid */
			c_ptr = cave_p(x, y);

			/* Deleting a locked or jammed door is problematical */
			delete_field_location(c_ptr);

			/* Clear previous contents (if not a lake), add a floor */
			if ((c_ptr->feat < FEAT_DEEP_WATER) ||
				(c_ptr->feat > FEAT_SHAL_LAVA))
			{
				set_feat_grid(c_ptr, FEAT_FLOOR);
			}
		}

		/* Apply the piercings that we found */
		for (j = 0; j < dun->wall_n; j++)
		{
			/* Access the grid */
			y = dun->wall[j].y;
			x = dun->wall[j].x;

			/* Access the grid */
			c_ptr = cave_p(x, y);

			/* Deleting a locked or jammed door is problematical */
			delete_field_location(c_ptr);

			/* Clear previous contents, add up floor */
			set_feat_grid(c_ptr, FEAT_FLOOR);

			/* Occasional doorway */
			if (randint0(100) < dun_tun_pen)
			{
				/* Place a random door */
				place_random_door(x, y);
			}
		}

		/* Remember the "previous" room */
		y = dun->cent[i].y;
		x = dun->cent[i].x;
	}

	/* Place intersection doors  */
	for (i = 0; i < dun->door_n; i++)
	{
		/* Extract junction location */
		y = dun->door[i].y;
		x = dun->door[i].x;

		/* Try placing doors */
		try_door(x, y - 1);
		try_door(x, y + 1);
		try_door(x - 1, y);
		try_door(x + 1, y);
	}


	/* Hack -- Add some magma streamers */
	for (i = 0; i < DUN_STR_MAG; i++)
	{
		build_streamer(FEAT_MAGMA, DUN_STR_MC);
	}

	/* Hack -- Add some quartz streamers */
	for (i = 0; i < DUN_STR_QUA; i++)
	{
		build_streamer(FEAT_QUARTZ, DUN_STR_QC);
	}

	/* Place 3 or 4 down stairs near some walls */
	if (!alloc_stairs(FEAT_MORE, rand_range(3, 4), 3)) return FALSE;

	/* Place 1 or 2 up stairs near some walls */
	if (!alloc_stairs(FEAT_LESS, rand_range(1, 2), 3)) return FALSE;

	/* Place quest monsters in the dungeon */
	trigger_quest_create(QC_DUN_MONST, NULL);

	/* Basic "amount" */
	k = (p_ptr->depth / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;

	/* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL;

	/* To make small levels a bit more playable */
	if (p_ptr->max_hgt < MAX_HGT || p_ptr->max_wid < MAX_WID)
	{
		int small_tester = i;

		i = (i * p_ptr->max_hgt) / MAX_HGT;
		i = (i * p_ptr->max_wid) / MAX_WID;
		i += 1;

		if (i > small_tester) i = small_tester;
		else if (cheat_hear)
		{
			msg_format("Reduced monsters base from %d to %d", small_tester, i);
		}
	}

	i += randint1(8);

	/* Put some monsters in the dungeon */
	for (i = i + k; i > 0; i--)
	{
		(void)alloc_monster(0, TRUE);
	}

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint1(k));

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint1(k));

	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT,
				 Rand_normal(DUN_AMT_ROOM, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT,
				 Rand_normal(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal(DUN_AMT_GOLD, 3));

	/* Put some invisible walls in the dungeon for nightmare mode */
	if (ironman_nightmare)
	{
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_INVIS,
					 Rand_normal(DUN_AMT_INVIS, 3));
	}

	if (empty_level && (!one_in_(DARK_EMPTY) || (randint1(100) > p_ptr->depth)))
	{
		/* Lite the cave */
		for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
		{
			for (x = p_ptr->min_hgt; x < p_ptr->max_wid; x++)
			{
				cave_p(x, y)->info |= (CAVE_GLOW);
			}
		}
	}

	/* Determine the character location */
	if (!new_player_spot())
		return FALSE;

	return TRUE;
}


static int map_wid_old = 66;


void map_panel_size(void)
{
	int wid, hgt;


	/* Only if the map exists */
	if (!character_dungeon) return;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Offset */
	wid -= COL_MAP + 1;

	/* reset panels */
	if (p_ptr->depth)
	{
		/* Determine number of panels (dungeon) */
		max_panel_rows = p_ptr->max_hgt - p_ptr->min_hgt;
		max_panel_cols = p_ptr->max_wid - p_ptr->min_wid;
	}
	else
	{
		/* Determine number of panels (wilderness) */
		max_panel_rows = max_wild * WILD_BLOCK_SIZE;
		max_panel_cols = max_wild * WILD_BLOCK_SIZE;
	}

	/* Assume illegal panel */
	panel_row_min = max_panel_rows;
	panel_col_min = max_panel_cols;

	/* Kill previous size of line */

#ifdef USE_TRANSPARENCY
	/* String of terrain characters along one row of the map */
	if (mp_ta) KILL(mp_ta);
	if (mp_tc) KILL(mp_tc);

#endif /* USE_TRANSPARENCY */

	/* String of characters along one row of the map */
	if (mp_a) KILL(mp_a);
	if (mp_c) KILL(mp_c);

	/* Save size */
	map_wid_old = wid;

	/* Make the new lines */

#ifdef USE_TRANSPARENCY
	/* String of terrain characters along one row of the map */
	C_MAKE(mp_ta, wid, byte);
	C_MAKE(mp_tc, wid, char);

#endif /* USE_TRANSPARENCY */

	/* String of characters along one row of the map */
	C_MAKE(mp_a, wid, byte);
	C_MAKE(mp_c, wid, char);
}

/* Make a real level */
static bool level_gen(cptr *why)
{
	int level_height, level_width;

	if (ironman_small_levels || (one_in_(SMALL_LEVEL) && small_levels))
	{
		if (cheat_room)
			msg_print("A 'small' dungeon level.");

		while (TRUE)
		{
			level_height = randint1(MAX_HGT / BLOCK_HGT);
			level_width = randint1(MAX_WID / BLOCK_WID);

			/* Exit if larger than one screen, but less than normal dungeon */
			if ((level_height < (MAX_HGT / BLOCK_HGT)) &&
				(level_height >= (22 / BLOCK_HGT)) &&
				(level_width < (MAX_WID / BLOCK_WID)) &&
				(level_width >= (66 / BLOCK_WID))) break;
		}

		/* Get bounds of dungeon */
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = level_height * BLOCK_HGT;
		p_ptr->min_wid = 0;
		p_ptr->max_wid = level_width * BLOCK_WID;

		if (cheat_room)
			msg_format("X:%d, Y:%d.", p_ptr->max_wid, p_ptr->max_hgt);
	}
	else
	{
		/* Big dungeon */
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = MAX_HGT;
		p_ptr->min_wid = 0;
		p_ptr->max_wid = MAX_WID;
	}

	/* Get the new region */
	dun_ptr->region = (s16b)create_region(p_ptr->max_wid, p_ptr->max_hgt,
										  REGION_CAVE);

	/* Grab the reference to it */
	incref_region(cur_region);

	/* Make a dungeon */
	if (!cave_gen())
	{
		*why = "could not place player";
		return FALSE;
	}

	return TRUE;
}


static byte extract_feeling(void)
{
	/* Hack -- no feeling in the town */
	if (!p_ptr->depth) return 0;

	/* Hack -- Have a special feeling sometimes */
	if (dun_ptr->good_item_flag && !preserve_mode) return 1;

	if (dun_ptr->rating > 100) return 2;
	if (dun_ptr->rating > 80) return 3;
	if (dun_ptr->rating > 60) return 4;
	if (dun_ptr->rating > 40) return 5;
	if (dun_ptr->rating > 30) return 6;
	if (dun_ptr->rating > 20) return 7;
	if (dun_ptr->rating > 10) return 8;
	if (dun_ptr->rating > 0) return 9;

	if ((turn - old_turn) > 50000L)
		chg_virtue(V_PATIENCE, 1);

	return 10;
}


/*
 * Delete a region from the region list
 *
 * Only call this when cleaning up the game during
 * exit - use unref_region() below normally.
 */
void del_region(int rg_idx)
{
	int i;

	/* Acquire region info */
	region_info *ri_ptr = &ri_list[rg_idx];

	/*
	 * Deallocate everything if region uses
	 * literal meanings of cave_type structure values.
	 *
	 * Note - quests have this flag unset.
	 * m_idx refers to race of monster.
	 * o_idx refers to type of object.
	 * fld_idx refers to type of field.
	 *
	 * (Rather than index of monster, object or fields.)
	 */
	if (ri_ptr->flags & REGION_CAVE)
	{
		/* Delete everything in the region */
		wipe_monsters(rg_idx);

		/*
		 * Objects are deleted after the monsters,
		 * because monsters carry them.
		 */
		wipe_objects(rg_idx);
		wipe_fields(rg_idx);
	}

	/* Deallocate the cave information */

	/* Free the cave */
	for (i = 0; i < ri_ptr->ysize; i++)
	{
		/* Allocate one row of the cave */
		FREE(rg_list[rg_idx][i]);
	}

	/* Free the region + info */
	KILL(rg_list[rg_idx]);
	(void)WIPE(&ri_list[rg_idx], region_info);

	/* Decrement counter */
	rg_cnt--;
}

/*
 * Decrease refcount on region - deallocate if empty
 */
int unref_region(int rg_idx)
{
	/* Acquire region info */
	region_info *ri_ptr = &ri_list[rg_idx];

	/* Paranoia */
	if (!ri_ptr->refcount) quit("Region refcount missmatch");

	/* Decrease refcount */
	ri_ptr->refcount--;

	/* Delete if just lost final reference */
	if (!ri_ptr->refcount)
	{
		/* Paranoia */
		if (!rg_list[rg_idx]) quit("Deleting unallocated region");

		del_region(rg_idx);

		/* Region no longer exists */
		return (0);
	}

	/* No change */
	return (rg_idx);
}


/*
 * Increase refcount on region
 */
void incref_region(int rg_idx)
{
	/* Acquire region info */
	region_info *ri_ptr = &ri_list[rg_idx];

	/* Paranoia */
	if (!rg_list[rg_idx]) quit("Incrementing unallocated region");

	/* Increase refcount */
	ri_ptr->refcount++;
}


/*
 * Set the global region
 */
void set_region(int rg_idx)
{
	/* Paranoia */
	if (rg_idx >= rg_max) quit("Setting invalid region");

	/* Set the region */
	cur_region = rg_idx;

	/* Set region pointer */
	cave_data = rg_list[cur_region];
}


/*
 * Delete all regions - and everything inside them
 */
void wipe_rg_list(void)
{
	int i;

	/* Wipe each active region */
	for (i = 1; i < rg_max; i++)
	{
		/*
		 * Hack - use del_region rather than unref_region.
		 *
		 * This function will not clean up all outstanding
		 * references to the regions.  Only call this when you
		 * know no such references exist.
		 */
		if (rg_list[i]) del_region(i);
	}

	/* Wipe the remaining objects, monsters and fields (in wilderness) */
	wipe_m_list();
	wipe_o_list();
	wipe_f_list();
}


/*
 * Do the actual work of allocating a region
 */
static void allocate_region(int rg_idx, int x, int y)
{
	int i;

	/* Acquire region info */
	region_info *ri_ptr = &ri_list[rg_idx];

	/* Save size */
	ri_ptr->xsize = x;
	ri_ptr->ysize = y;

	/* Hack set the refcount to zero - assume caller increments refcount */
	ri_ptr->refcount = 0;

	/* Make the array of pointers to the cave */
	C_MAKE(rg_list[rg_idx], y, cave_type *);

	/* Allocate and wipe each line of the region */
	for (i = 0; i < y; i++)
	{
		/* Allocate one row of the cave */
		C_MAKE(rg_list[rg_idx][i], x, cave_type);
	}

	/* Hack - set this region to be the currently used one */
	set_region(rg_idx);
}


/*
 * Allocate a region.
 *
 * (Usually used to store the dungeon,
 * However, can be used in the wilderness to store
 * town info.)
 *
 * This rountine should never fail - but be prepared
 * for when it does.
 */
int create_region(int x, int y, byte flags)
{
	int rg_idx;
	int i;

	if (rg_max < z_info->rg_max)
	{
		/* Get next space */
		rg_idx = rg_max;

		/* Expand region array */
		rg_max++;

		/* Count regions */
		rg_cnt++;

		/* Allocate the region */
		allocate_region(rg_idx, x, y);

		/* Save the flags */
		ri_list[rg_idx].flags = flags;

		/* Done */
		return (rg_idx);
	}

	/* Recycle dead regions */
	for (i = 1; i < rg_max; i++)
	{
		/* Skip used regions */
		if (rg_list[i]) continue;

		/* Count regions */
		rg_cnt++;

		/* Allocate the region */
		allocate_region(i, x, y);

		/* Save the flags */
		ri_list[i].flags = flags;

		/* Use this region */
		return (i);
	}

	/* Warn the player */
	msg_print("Too many regions!");

	/* Oops */
	return (0);
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
	int num;

	/* Hack - Reset the object theme */
	dun_theme.treasure = 20;
	dun_theme.combat = 20;
	dun_theme.magic = 20;
	dun_theme.tools = 20;

	/* Build the wilderness */
	if (!p_ptr->depth)
	{
		/* The "dungeon" is ready */
		character_dungeon = TRUE;

		/* Reset map panels */
		map_panel_size();

		return;
	}

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;

		cptr why = NULL;

		/* Set the base level */
		base_level = p_ptr->depth;

		/* Reset the monster generation level */
		monster_level = base_level;

		/* Reset the object generation level */
		object_level = base_level;

		/* Nothing special here yet */
		dun_ptr->good_item_flag = FALSE;

		/* Nothing good here yet */
		dun_ptr->rating = 0;

		okay = level_gen(&why);

		/* Extract the feeling */
		dun_ptr->feeling = extract_feeling();

		/* Prevent object over-flow */
		if (o_max >= z_info->o_max)
		{
			/* Message */
			why = "too many objects";

			/* Message */
			okay = FALSE;
		}
		/* Prevent monster over-flow */
		else if (m_max >= z_info->m_max)
		{
			/* Message */
			why = "too many monsters";

			/* Message */
			okay = FALSE;
		}

		/* Mega-Hack -- "auto-scum" */
		else if ((auto_scum || ironman_autoscum) && (num < 100))
		{
			/* Require "goodness" */
			if ((dun_ptr->feeling > 9) ||
				((p_ptr->depth >= 7) && (dun_ptr->feeling > 8)) ||
				((p_ptr->depth >= 15) && (dun_ptr->feeling > 7)) ||
				((p_ptr->depth >= 35) && (dun_ptr->feeling > 6)) ||
				((p_ptr->depth >= 70) && (dun_ptr->feeling > 5)))
			{
				/* Give message to cheaters */
				if (cheat_room || cheat_hear || cheat_peek || cheat_xtra)
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

		/* Delete the level - not good enough */
		dun_ptr->region = unref_region(dun_ptr->region);
	}

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Reset map panels */
	map_panel_size();

	/* Verify the panel */
	verify_panel();

#if 0
	/* Remove the CAVE_ROOM flags... reused as CAVE_MNLT */
	for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
	{
		for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
		{
			/* Clear the flag */
			cave_p(x, y)->info &= ~(CAVE_ROOM);
		}
	}
#endif /* 0 */

	/* Remember when this level was "created" */
	old_turn = turn;
}
