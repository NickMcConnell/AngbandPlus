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
	int         y, x, i, j, flag;
	int         more_num = 0;
	cave_type   *c_ptr;

	if (feat == FEAT_LESS)
	{
		/* No up stairs in town or in ironman mode */
		if (ironman_downward || !dun_level) return TRUE;

		if (dun_level > 1) more_num = (randint1(num) + 1) / 2;
	}
	else if (feat == FEAT_MORE)
	{
		/* No downstairs on quest levels */
		if ((dun_level > 1) && quest_number(dun_level)) return TRUE;

		/* No downstairs at the bottom */
		if (dun_level >= TINY_MAX_DEPTH - 1) return TRUE;

		if ((dun_level < TINY_MAX_DEPTH - 2) && !quest_number(dun_level + 1))
			more_num = (randint1(num) + 1) / 2;
	}

	/* Place "num" stairs */
	for (i = 0; i < num; i++)
	{
		/* Place some stairs */
		for (flag = FALSE; !flag; )
		{
			/* Try several times, then decrease "walls" */
			for (j = 0; !flag && j <= 10000; j++)
			{
				/* Pick a random grid */
				y = randint0(cur_hgt);
				x = randint0(cur_wid);

				/* Require "naked" floor grid */
				if (!cave_naked_bold(y, x)) continue;

				/* Require floor */
				if (cave[y][x].feat != FEAT_FLOOR) continue;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(y, x) < walls) continue;

				/* Access the grid */
				c_ptr = &cave[y][x];

				/* Clear previous contents, add stairs */
				if (i < more_num) c_ptr->feat = feat + 0x07;
				else c_ptr->feat = feat;

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
	int y, x, k;
	int dummy = 0;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		while (dummy < SAFE_MAX_ATTEMPTS)
		{
			bool room;

			dummy++;

			/* Location */
			y = randint0(cur_hgt);
			x = randint0(cur_wid);

			if (!in_bounds(y, x)) continue;

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x) &&
			    !((y == py) && (x == px))) continue;

			/* Check for "room" */
			room = (cave[y][x].info & CAVE_ROOM) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

			/* Accept it */
			break;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
#ifdef JP
msg_print("警告！アイテムを配置できません！");
#else
				msg_print("Warning! Could not place object!");
#endif

			}
			return;
		}


		/* Place something */
		switch (typ)
		{
			case ALLOC_TYP_RUBBLE:
			{
				place_rubble(y, x);
				break;
			}

			case ALLOC_TYP_TRAP:
			{
				place_trap(y, x);
				break;
			}

			case ALLOC_TYP_GOLD:
			{
				place_gold(y, x);
				break;
			}

			case ALLOC_TYP_OBJECT:
			{
				place_object(y, x, FALSE, FALSE);
				break;
			}

			case ALLOC_TYP_INVIS:
			{
				place_invis_wall(y, x);
				break;
			}
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
 * Assumes "in_bounds(y, x)"
 */
static bool possible_doorway(int y, int x)
{
	/* Count the adjacent corridors */
	if (next_to_corr(y, x) >= 2)
	{
		/* Check Vertical */
		if ((cave[y-1][x].feat >= FEAT_MAGMA) &&
		    (cave[y+1][x].feat >= FEAT_MAGMA))
		{
			return (TRUE);
		}

		/* Check Horizontal */
		if ((cave[y][x-1].feat >= FEAT_MAGMA) &&
		    (cave[y][x+1].feat >= FEAT_MAGMA))
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
	if (cave[y][x].feat >= FEAT_MAGMA) return;

	/* Ignore room grids */
	if (cave[y][x].info & (CAVE_ROOM)) return;

	/* Occasional door (if allowed) */
	if ((randint0(100) < dun_tun_jct) && possible_doorway(y, x))
	{
		/* Place a door */
		place_random_door(y, x);
	}
}


/* Place quest monsters */
static void place_quest_monsters(void)
{
	int i;

	/* Handle the quest monster placements */
	for (i = 0; i < max_quests; i++)
	{
		monster_race *r_ptr;
		bool group;
		int j;

		if ((quest[i].status != QUEST_STATUS_TAKEN) ||
		    ((quest[i].type != QUEST_TYPE_KILL_LEVEL) &&
		     (quest[i].type != QUEST_TYPE_RANDOM)) ||
		    (quest[i].level != dun_level) ||
		    (quest[i].flags & QUEST_FLAG_PRESET))
		{
			/* Ignore it */
			continue;
		}

		r_ptr = &r_info[quest[i].r_idx];

		/* Hack -- "unique" monsters must be "unique" */
		if ((r_ptr->flags1 & RF1_UNIQUE) &&
		    (r_ptr->cur_num >= r_ptr->max_num))
		{
			/* The unique is already dead */
			quest[i].status = QUEST_STATUS_FINISHED;
			continue;
		}

		group = (r_ptr->flags1 & RF1_FRIENDS) ? FALSE : TRUE;

		for (j = 0; j < (quest[i].max_num - quest[i].cur_num); j++)
		{
			int k;

			for (k = 0; k < SAFE_MAX_ATTEMPTS; k++)
			{
				int x, y, attempt;

				/* Find an empty grid */
				for (attempt = SAFE_MAX_ATTEMPTS; attempt; attempt--)
				{
					y = randint0(cur_hgt);
					x = randint0(cur_wid);
					if (!cave_naked_bold(y, x)) continue;
					if (distance(y, x, py, px) < 10) continue;
					else break;
				}

				/* Failed to place */
				if (!attempt) break;

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


/*
 * Generate various caverns and lakes
 *
 * There were moved from cave_gen().
 */
static void gen_caverns_and_lakes(void)
{
#ifdef ALLOW_CAVERNS_AND_LAKES
	/* Possible "destroyed" level */
	if ((dun_level > 10) && one_in_(DUN_DEST) && small_levels)
	{
		dun->destroyed = TRUE;

		/* extra rubble around the place looks cool */
		build_lake(LAKE_T_CAVE);
	}

	/* Make a lake some of the time */
	if (one_in_(LAKE_LEVEL) && !dun->empty_level && !dun->destroyed && terrain_streams)
	{
		/* Lake of Water */
		if (dun_level > 30) dun->laketype = LAKE_T_WATER;

		/* Lake of Lava */
		if (dun_level > 60) dun->laketype = LAKE_T_LAVA;

		if (dun->laketype)
		{
			if (cheat_room)
#ifdef JP
				msg_print("湖あり。");
#else
				msg_print("Lake on the level.");
#endif
			build_lake(dun->laketype);
		}
	}

	if (one_in_(CAVE_LEVEL - (dun_level / 10)) && !dun->empty_level &&
	    !dun->laketype && !dun->destroyed && (dun_level >= MIN_CAVERN))
	{
		dun->cavern = TRUE;

		/* make a large fractal cave in the middle of the dungeon */

		if (cheat_room)
#ifdef JP
			msg_print("洞窟あり。");
#else
			msg_print("Cavern on level.");
#endif
		build_cavern();
	}
#endif /* ALLOW_CAVERNS_AND_LAKES */

	/* Hack -- No destroyed "quest" levels */
	if (quest_number(dun_level)) dun->destroyed = FALSE;
}


/*
 * Put lava crater.
 */
static void place_lavas(int x, int y)
{
	int i, j;

	/* place shallow or deep lavas in ovalish distribution*/
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
				if ((distance(j, i, y, x) > 1) || (randint1(100) < 25))
				{
					if (randint1(100) < 75)
						cave[j][i].feat = FEAT_SHAL_LAVA;
				}
				else
				{
					cave[j][i].feat = FEAT_DEEP_LAVA;
				}
			}
		}
	}
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(void)
{
	int i, k, y, x;

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;

	dun->destroyed = FALSE;
	dun->empty_level = FALSE;
	dun->cavern = FALSE;
	dun->laketype = 0;


	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), NULL);

	/* Randomize the dungeon creation values */
	dun_tun_rnd = rand_range(DUN_TUN_RND_MIN, DUN_TUN_RND_MAX);
	dun_tun_chg = rand_range(DUN_TUN_CHG_MIN, DUN_TUN_CHG_MAX);
	dun_tun_con = rand_range(DUN_TUN_CON_MIN, DUN_TUN_CON_MAX);
	dun_tun_pen = rand_range(DUN_TUN_PEN_MIN, DUN_TUN_PEN_MAX);
	dun_tun_jct = rand_range(DUN_TUN_JCT_MIN, DUN_TUN_JCT_MAX);

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

	/* No rooms yet */
	dun->cent_n = 0;

	/* Empty arena levels */
	if (ironman_empty_levels || (empty_levels && one_in_(EMPTY_LEVEL)))
	{
		dun->empty_level = TRUE;

		if (cheat_room)
#ifdef JP
			msg_print("アリーナレベル");
#else
			msg_print("Arena level.");
#endif
	}


	if (dun->empty_level)
	{
		/* Start with floors */
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				cave[y][x].feat = FEAT_FLOOR;
			}
		}
	}
	else
	{
		/* Start with walls */
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				cave[y][x].feat = FEAT_WALL_EXTRA;
			}
		}
	}


	/* Generate various caverns and lakes */
	gen_caverns_and_lakes();


	/* Build some rooms */

	/*
	 * Build each type of room in turn until we cannot build any more.
	 */
	generate_rooms();

	/* Make a hole in the dungeon roof sometimes at level 1 */
	if ((dun_level == 1) && terrain_streams && one_in_(DUN_MOS_DEN))
	{
		int n = 0;

		do
		{
			int x = randint1(cur_wid - 2);
			int y = randint1(cur_hgt - 2);

			n++;
			if (!cave_floor_bold(y, x)) continue;
			place_trees(x, y);
			if (one_in_(DUN_MOS_DEN)) break;
		}
		while (n < 1000);
	}

	/* Make a crater in the dungeon sometimes under level 20 */
	if ((dun_level >= 20) && terrain_streams)
	{
		int n = 0;

		do
		{
			int x = randint1(cur_wid - 2);
			int y = randint1(cur_hgt - 2);

			n++;
			if (!cave_floor_bold(y, x)) continue;
			place_lavas(x, y);
			if (one_in_(DUN_MOS_DEN)) break;
		}
		while (n < 1000);
	}

	/* Destroy the level if necessary */
	if (dun->destroyed) destroy_level();

	/* Hack -- Add some rivers */
	if (one_in_(3) && (randint1(dun_level) > 5) && terrain_streams)
	{
		int feat1, feat2;

		/* Choose water or lava */
		if (randint1(TINY_MAX_DEPTH) - 1 > dun_level)
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
		if (((dun->laketype == LAKE_T_LAVA) && (feat1 == FEAT_DEEP_LAVA)) ||
		    ((dun->laketype == LAKE_T_WATER) && (feat1 == FEAT_DEEP_WATER)) ||
		     !dun->laketype)
		{
			add_river(feat1, feat2);
		}
	}

	/* Special boundary walls -- Top and bottom */
	for (x = 0; x < cur_wid; x++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		cave[0][x].feat = FEAT_PERM_SOLID;
		cave[cur_hgt - 1][x].feat = FEAT_PERM_SOLID;
	}

	/* Special boundary walls -- Left and right */
	for (y = 1; y < (cur_hgt - 1); y++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		cave[y][0].feat = FEAT_PERM_SOLID;
		cave[y][cur_wid - 1].feat = FEAT_PERM_SOLID;
	}


	/* Hack -- Scramble the room order */
	for (i = 0; i < dun->cent_n; i++)
	{
		int ty, tx;
		int pick = rand_range(0, i);

		ty = dun->cent[i].y;
		tx = dun->cent[i].x;
		dun->cent[i].y = dun->cent[pick].y;
		dun->cent[i].x = dun->cent[pick].x;
		dun->cent[pick].y = ty;
		dun->cent[pick].x = tx;
	}

	/* Start with no tunnel doors */
	dun->door_n = 0;

	/* Hack -- connect the first room to the last room */
	y = dun->cent[dun->cent_n-1].y;
	x = dun->cent[dun->cent_n-1].x;

	/* Connect all the rooms together */
	for (i = 0; i < dun->cent_n; i++)
	{
		int j;

		/* Reset the arrays */
		dun->tunn_n = 0;
		dun->wall_n = 0;

		/* Connect the room to the previous room */
		if (randint1(dun_level) > 25)
		{
			/* make cave-like tunnel */
			build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y, 2, 2);
		}
		else
		{
			/* make normal tunnel */
			build_tunnel(dun->cent[i].y, dun->cent[i].x, y, x);
		}

		/* Turn the tunnel into corridor */
		for (j = 0; j < dun->tunn_n; j++)
		{
			cave_type *c_ptr;

			/* Access the grid */
			y = dun->tunn[j].y;
			x = dun->tunn[j].x;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Clear previous contents (if not a lake), add a floor */
			if ((c_ptr->feat < FEAT_DEEP_WATER) ||
			    (c_ptr->feat > FEAT_SHAL_LAVA))
			{
				c_ptr->feat = FEAT_FLOOR;
			}
		}

		/* Apply the piercings that we found */
		for (j = 0; j < dun->wall_n; j++)
		{
			cave_type *c_ptr;

			/* Access the grid */
			y = dun->wall[j].y;
			x = dun->wall[j].x;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Clear previous contents, add up floor */
			c_ptr->feat = FEAT_FLOOR;

			/* Occasional doorway */
			if (randint0(100) < dun_tun_pen)
			{
				/* Place a random door */
				place_random_door(y, x);
			}
		}

		/* Remember the "previous" room */
		y = dun->cent[i].y;
		x = dun->cent[i].x;
	}

	/* Place intersection doors */
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

	place_quest_monsters();

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
#ifdef JP
			msg_format("モンスター数基本値を %d から %d に減らします", small_tester, i);
#else
			msg_format("Reduced monsters base from %d to %d", small_tester, i);
#endif
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
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ROOM, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, randnor(DUN_AMT_GOLD, 3));

	/* Put some invisible walls in the dungeon for nightmare mode */
	if (ironman_nightmare)
	{
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_INVIS, randnor(DUN_AMT_INVIS, 3));
	}

	if (dun->empty_level && (!one_in_(DARK_EMPTY) || (randint1(100) > dun_level)))
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

	/* Determine the character location */
	if (!new_player_spot())
		return FALSE;

	return TRUE;
}


/*
 * Builds the arena after it is entered -KMW-
 */
static void build_arena(void)
{
	int yval, y_height, y_depth, xval, x_left, x_right;
	register int i, j;

	yval = SCREEN_HGT / 2;
	xval = SCREEN_WID / 2;
	y_height = yval - 10 + SCREEN_HGT;
	y_depth = yval + 10 + SCREEN_HGT;
	x_left = xval - 32 + SCREEN_WID;
	x_right = xval + 32 + SCREEN_WID;

	for (i = y_height; i <= y_height + 5; i++)
		for (j = x_left; j <= x_right; j++)
		{
			cave[i][j].feat = FEAT_PERM_EXTRA;
			cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
		}
	for (i = y_depth; i >= y_depth - 5; i--)
		for (j = x_left; j <= x_right; j++)
		{
			cave[i][j].feat = FEAT_PERM_EXTRA;
			cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
		}
	for (j = x_left; j <= x_left + 17; j++)
		for (i = y_height; i <= y_depth; i++)
		{
			cave[i][j].feat = FEAT_PERM_EXTRA;
			cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
		}
	for (j = x_right; j >= x_right - 17; j--)
		for (i = y_height; i <= y_depth; i++)
		{
			cave[i][j].feat = FEAT_PERM_EXTRA;
			cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
		}

	cave[y_height+6][x_left+18].feat = FEAT_PERM_EXTRA;
	cave[y_height+6][x_left+18].info |= (CAVE_GLOW | CAVE_MARK);
	cave[y_depth-6][x_left+18].feat = FEAT_PERM_EXTRA;
	cave[y_depth-6][x_left+18].info |= (CAVE_GLOW | CAVE_MARK);
	cave[y_height+6][x_right-18].feat = FEAT_PERM_EXTRA;
	cave[y_height+6][x_right-18].info |= (CAVE_GLOW | CAVE_MARK);
	cave[y_depth-6][x_right-18].feat = FEAT_PERM_EXTRA;
	cave[y_depth-6][x_right-18].info |= (CAVE_GLOW | CAVE_MARK);

	i = y_height + 5;
	j = xval + SCREEN_WID;
	cave[i][j].feat = FEAT_BLDG_HEAD + 2;
	cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
	player_place(i + 1, j);
}


/*
 * Town logic flow for generation of arena -KMW-
 */
static void arena_gen(void)
{
	int y, x;
	int qy = SCREEN_HGT;
	int qx = SCREEN_WID;
	bool daytime;

	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
		daytime = TRUE;

	/* Night time */
	else
		daytime = FALSE;

	/* Start with solid walls */
	for (y = 0; y < MAX_HGT; y++)
	{
		for (x = 0; x < MAX_WID; x++)
		{
			/* Create "solid" perma-wall */
			cave[y][x].feat = FEAT_PERM_SOLID;

			/* Illuminate and memorize the walls */
			cave[y][x].info |= (CAVE_GLOW | CAVE_MARK);
		}
	}

	/* Then place some floors */
	for (y = qy + 1; y < qy + SCREEN_HGT - 1; y++)
	{
		for (x = qx + 1; x < qx + SCREEN_WID - 1; x++)
		{
			/* Create empty floor */
			cave[y][x].feat = FEAT_FLOOR;

			/* Darken and forget the floors */
			cave[y][x].info &= ~(CAVE_GLOW | CAVE_MARK);

			/* Day time */
			if (daytime)
			{
				/* Perma-Lite */
				cave[y][x].info |= (CAVE_GLOW);

				/* Memorize */
				if (view_perma_grids) cave[y][x].info |= (CAVE_MARK);
			}
		}
	}

	build_arena();

	place_monster_aux(py + 5, px, arena_monsters[p_ptr->arena_number],
	    FALSE, FALSE, FALSE, FALSE);
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
			cave[y][x].feat = FEAT_PERM_SOLID;
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
#ifdef TINYANGBAND
	/* Level size is always 2x2 */
	int level_height = 2;
	int level_width = 2;

	cur_hgt = level_height * SCREEN_HGT;
	cur_wid = level_width * SCREEN_WID;

	panel_row_min = cur_hgt;
	panel_col_min = cur_wid;
#else /* Other Variants */
	int level_height, level_width;

	if (always_small_levels || ironman_small_levels ||
		((randint1(SMALL_LEVEL) == 1) && small_levels))
	{
		if (cheat_room)
#ifdef JP
			msg_print("小さなフロア");
#else
			msg_print("A 'small' dungeon level.");
#endif
		if (dun_level < 10)
		{
			level_height = 2;
			level_width = 2;
		}
		else
		{
			do
			{
				level_height = randint1(MAX_HGT/SCREEN_HGT);
				level_width = randint1(MAX_WID/SCREEN_WID);
			}
			while ((level_height == MAX_HGT/SCREEN_HGT) &&
				(level_width == MAX_WID/SCREEN_WID));
		}

		cur_hgt = level_height * SCREEN_HGT;
		cur_wid = level_width * SCREEN_WID;

		/* Assume illegal panel */
		panel_row_min = cur_hgt;
		panel_col_min = cur_wid;

		if (cheat_room)
		  msg_format("X:%d, Y:%d.", cur_hgt, cur_wid);
	}
	else
	{
		/* Big dungeon */
		cur_hgt = MAX_HGT;
		cur_wid = MAX_WID;

		/* Assume illegal panel */
		panel_row_min = cur_hgt;
		panel_col_min = cur_wid;
	}
#endif

	/* Make a dungeon */
	if (!cave_gen())
	{
#ifdef JP
		*why = "プレイヤーを配置できない";
#else
		*why = "could not place player";
#endif
		return FALSE;
	}
	else return TRUE;
}


/*
 * Wipe all unnecessary flags after cave generation
 */
static void wipe_generate_cave_flags(void)
{
	int x, y;

	if (dun_level)
	{
		for (y = 1; y < cur_hgt - 1; y++)
		{
			for (x = 1; x < cur_wid - 1; x++)
			{
				/* There might be trap */
				cave[y][x].info |= CAVE_UNSAFE;
			}
		}
	}
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

	/* No longer in the trap detecteded region */
	p_ptr->dtrap = FALSE;

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;
		byte feeling;

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
		panel_row_min = -1;
		panel_row_max = -1;
		panel_col_min = -1;
		panel_col_max = -1;

		/* Set the base level */
		base_level = dun_level;

		/* Reset the monster generation level */
		monster_level = base_level;

		/* Reset the object generation level */
		object_level = base_level;

		/* Build the arena -KMW- */
		if (p_ptr->inside_arena)
		{
			/* Small arena */
			arena_gen();
		}

		/* Quest levels -KMW- */
		else if (p_ptr->inside_quest)
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

		/* Extract the feeling */
		feeling = get_dungeon_feeling();

		/* Prevent object over-flow */
		if (o_max >= max_o_idx)
		{
			/* Message */
#ifdef JP
why = "アイテムが多すぎる";
#else
			why = "too many objects";
#endif


			/* Message */
			okay = FALSE;
		}
		/* Prevent monster over-flow */
		else if (m_max >= max_m_idx)
		{
			/* Message */
#ifdef JP
why = "モンスターが多すぎる";
#else
			why = "too many monsters";
#endif


			/* Message */
			okay = FALSE;
		}

		/* Mega-Hack -- "auto-scum" */
		else if ((auto_scum || ironman_autoscum) && (num < 100) &&
				 !p_ptr->inside_quest)
		{
			/* Require "goodness" */
			if (((dun_level >= 5) && (feeling > 9)) ||
			    ((dun_level >= 15) && (feeling > 8)) ||
			    ((dun_level >= 40) && (feeling > 7)) ||
			    ((dun_level >= 70) && (feeling > 6)))
			{
				/* Give message to cheaters */
				if (cheat_room || cheat_hear ||
				    cheat_peek || cheat_xtra)
				{
					/* Message */
#ifdef JP
why = "退屈な階";
#else
					why = "boring level";
#endif

				}

				/* Try again */
				okay = FALSE;
			}
		}

		/* Accept */
		if (okay) break;

		/* Message */
#ifdef JP
if (why) msg_format("生成やり直し(%s)", why);
#else
		if (why) msg_format("Generation restarted (%s)", why);
#endif


		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the monsters */
		wipe_m_list();
	}

	wipe_generate_cave_flags();

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Remember when this level was "created" */
	old_turn = turn;

	/* No dungeon feeling yet */
	p_ptr->feeling_turn = old_turn;
	p_ptr->feeling = 0;

	/* Glow lava in floor */
	glow_lava_floor();
}
