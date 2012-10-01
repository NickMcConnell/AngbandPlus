/* File: generate.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Note that Level generation is *not* an important bottleneck,
 * though it can be annoyingly slow on older machines...  Thus
 * we emphasize "simplicity" and "correctness" over "speed".
 *
 * This entire file is only needed for generating levels.
 * This may allow smart compilers to only load it when needed.
 *
 * Consider the "vault.txt" file for vault generation.
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
 * Note that the use of 11x11 blocks (instead of the 33x11 panels)
 * allows more variability in the horizontal placement of rooms, and
 * at the same time has the disadvantage that some rooms (two thirds
 * of the normal rooms) may be "split" by panel boundaries.  This can
 * induce a situation where a player is in a room and part of the room
 * is off the screen.  This can be so annoying that the player must set
 * a special option to enable "non-aligned" room generation.
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
 *
 * Notes for NPPAngband 0.2.0
 * The addition of smaller (arena) levels was causing the level generation routine
 * to lock up approximately once every 200-300 generated levels.  Occasionally a
 * level with only one or zero rooms is generated, and functions such as place_player_spot
 * would lock up in an infinite soop trying to find a suitable spot.  To fix this, it was
 * simpler to reject the level and have things start over, rather than try to
 * identify and fix the problem.  Code was adopted from EY Angband, which uses an array for
 * tracking the rooms, rather than just marking them in the dungeon floor array.  Notice all
 * of the "while (true) loops now have an escape mechanism, and many of the functions have
 * been changed to bool.  If they return false, the level is considered defective
 * (ex. 5000 chances to place the player fail), and level generation is re-started.
 * I generated over 50,000 levels without incident once I made these changes. JG
 */

/*
 * Dungeon generation values
 */

#define DUN_UNUSUAL	180	/* Level/chance of unusual room */
#define DUN_DEST	35	/* 1/chance of having a destroyed level */
#define SMALL_LEVEL 10	/* 1/chance of smaller size */
#define THEMED_LEVEL_CHANCE	50	/* 1/chance of being a themed level */

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND	10	/* Chance of random direction */
#define DUN_TUN_CHG	30	/* Chance of changing direction */
#define DUN_TUN_CON	15	/* Chance of extra tunneling */
#define DUN_TUN_PEN	25	/* Chance of doors at room entrances */
#define DUN_TUN_JCT	90	/* Chance of doors at tunnel junctions */

/*
 * Dungeon streamer generation values
 */
#define DUN_STR_DEN	5	/* Density of streamers */
#define DUN_STR_RNG	2	/* Width of streamers */
#define DUN_STR_MAG	3	/* Number of magma streamers */
#define DUN_STR_MC	90	/* 1/chance of treasure per magma */
#define DUN_STR_QUA	2	/* Number of quartz streamers */
#define DUN_STR_QC	40	/* 1/chance of treasure per quartz */

/*
 * Dungeon treausre allocation values
 */
#define DUN_AMT_ROOM	9	/* Amount of objects for rooms */
#define DUN_AMT_ITEM	3	/* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD	3	/* Amount of treasure for rooms/corridors */

/*
 * Hack -- Dungeon allocation "places"
 */
#define ALLOC_SET_CORR		1	/* Hallway */
#define ALLOC_SET_ROOM		2	/* Room */
#define ALLOC_SET_BOTH		3	/* Anywhere */

/*
 * Hack -- Dungeon allocation "types"
 */
#define ALLOC_TYP_RUBBLE	1	/* Rubble */
#define ALLOC_TYP_TRAP		3	/* Trap */
#define ALLOC_TYP_GOLD		4	/* Gold */
#define ALLOC_TYP_OBJECT	5	/* Object */


/*
 * Maximum numbers of rooms along each axis (currently 6x18)
 */

#define MAX_ROOMS_ROW	(MAX_DUNGEON_HGT / BLOCK_HGT)
#define MAX_ROOMS_COL	(MAX_DUNGEON_WID / BLOCK_WID)

/*
 * Bounds on some arrays used in the "dun_data" structure.
 * These bounds are checked, though usually this is a formality.
 */
#define CENT_MAX	110
#define DOOR_MAX	200
#define WALL_MAX	500
#define TUNN_MAX	900

bool allow_uniques;


/*
 * Maximal number of room types
 */
#define ROOM_MAX	12
#define ROOM_MIN     2

/*
 * Simple structure to hold a map location
 */

typedef struct coord coord;

struct coord
{
	byte y;
	byte x;
};


/*
 * Room type information
 */

typedef struct room_data room_data;

struct room_data
{
	/* Required size in blocks */
	s16b dy1, dy2, dx1, dx2;

	/* Hack -- minimum level */
	s16b level;
};


/*
 * Structure to hold all "dungeon generation" data
 */

typedef struct dun_data dun_data;

struct dun_data
{
	/* Array of centers of rooms */
	int cent_n;
	coord cent[CENT_MAX];

	/* Array of possible door locations */
	int door_n;
	coord door[DOOR_MAX];

	/* Array of wall piercing locations */
	int wall_n;
	coord wall[WALL_MAX];

	/* Array of tunnel grids */
	int tunn_n;
	coord tunn[TUNN_MAX];

	/* Number of blocks along each axis */
	int row_rooms;
	int col_rooms;

	/* Array of which blocks are used */
	bool room_map[MAX_ROOMS_ROW][MAX_ROOMS_COL];

	/* Hack -- there is a pit/nest on this level */
	bool crowded;
};


/*
 * Dungeon generation data -- see "cave_gen()"
 */
static dun_data *dun;


/*
 * Array of room types (assumes 11x11 blocks)
 */
static const room_data room[ROOM_MAX] =
{
	{ 0, 0, 0, 0, 0 },		/* 0 = Nothing */
	{ 0, 0, -1, 1, 1 },		/* 1 = Simple (33x11) */
	{ 0, 0, -1, 1, 1 },		/* 2 = Overlapping (33x11) */
	{ 0, 0, -1, 1, 3 },		/* 3 = Crossed (33x11) */
	{ 0, 0, -1, 1, 3 },		/* 4 = Large (33x11) */
	{ 0, 0, -1, 1, 5 },		/* 5 = Monster nest (33x11) */
	{ 0, 0, -1, 1, 5 },		/* 6 = Monster pit (33x11) */
	{ 0, 1, -1, 1, 5 },		/* 7 = Lesser vault (33x22) */
	{ -1, 2, -2, 3, 10 },	/* 8 = Greater vault (66x44) */
	{ 0, 1, -1, 1, 0 },		/* 9 = Quest vault (44x22) */
	{ 0, 1, -1, 1, 0},		/* 10 = Starburst (33x22) */
	{ -1, 2, -2, 3, 0}		/* 11 = Great Starburst Room(66x44) */

	/*hack - if the dimensions for starburst rooms or great starburst rooms change,
	 these lines in build_type_starburst need changing:
	 if (giant_room)
	{
		dy = 19;
		dx = 30;
	}
	33x22
	else
	{
		dy = 10;
		dx = 14;
	}

	-JG
	 */

};

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
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 *
 * We count only granite walls and permanent walls, and stairs.
 */
static int next_to_walls(int y, int x)
{
	int k = 0;

	if (cave_feat[y+1][x] >= FEAT_WALL_EXTRA) k++;
	if (cave_feat[y-1][x] >= FEAT_WALL_EXTRA) k++;
	if (cave_feat[y][x+1] >= FEAT_WALL_EXTRA) k++;
	if (cave_feat[y][x-1] >= FEAT_WALL_EXTRA) k++;

	return (k);
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
 * Count unused blocks in the dungeon.
 */
static int count_unused_blocks(void)
{
  	int y, x, count;

  	for (count = 0, y = 0; y < dun->row_rooms; y++)
  	{
    	for (x = 0; x < dun->col_rooms; x++)
    	{
      		if (dun->room_map[y][x]) continue;

      		++count;
    	}
  	}

  return (count);
}

#define MAX_RANGE_TO_ROOM 15

/*
 * Returns random co-ordinates for player/monster/object
 * currently only used for player.
 */
static bool new_player_spot(void)
{
	int i = 0;

	int yy, xx;

	int x_location_tables [40];
	int y_location_tables [40];
	int rand_spot;

	int try;

	/*  */
	for (yy = 0; yy < p_ptr->cur_map_hgt; yy++)
	{
		for (xx = 0; xx < p_ptr->cur_map_wid; xx++)
		{

			/* Must be a "naked" floor grid */
			if (!cave_naked_bold(yy, xx)) continue;

			/* Refuse to start on anti-teleport grids */
			if (cave_info[yy][xx] & (CAVE_ICKY)) continue;
			if (cave_info[yy][xx] & (CAVE_ROOM)) continue;

			/* We like to be next to walls */
			if (next_to_walls(yy, xx) < 3) continue;

			/* Put the player outside rooms if needed */
			if ((p_ptr->cur_map_hgt == MAX_DUNGEON_HGT) &&
			    (p_ptr->cur_map_wid == MAX_DUNGEON_WID))
			{
			  	int i, d, x, y;
			  	bool found_room = FALSE;

			  	/* Scan the floor in every direction */
			  	for (d = 0; (d < 8) && !found_room; d++)
			  	{
			    	/* Limit the range */
			    	for (i = 1; i <= MAX_RANGE_TO_ROOM; i++)
			    	{
			      		/* Get coordinates */
			      		x = xx + i * ddx_ddd[d];
			     		y = yy + i * ddy_ddd[d];

			      		/* Outside limits */
			      		if (!in_bounds(y, x)) break;

			      		/* Walls */
			      		if ((cave_feat[y][x] >= FEAT_WALL_HEAD) &&
				  			(cave_feat[y][x] <= FEAT_WALL_TAIL)) break;

			      		/* Room, reject coordinates */
			      		if (cave_info[y][x] & (CAVE_ROOM))
			      		{
							found_room = TRUE;
							break;
			      		}
			    	}
			  	}

			  	if (found_room) continue;
			}

			/*don't go over size of array*/
			if (i < 40)
			{
				x_location_tables[i] = xx;
				y_location_tables[i] = yy;

				/*increase the counter*/
				i++;
			}

			/*put it in a random slot*/
			else
			{
				rand_spot = rand_int(40);

				x_location_tables[rand_spot] = xx;
				y_location_tables[rand_spot] = yy;
			}

		}
	}

	/*paranoia*/
	if (i == 0) return (FALSE);

	/*
	 * Try to place the player "i" times to reduce the number of failed
	 * levels. -DG-
	 */
	for (try = 0; try < i; try++)
	{
		/*select a location*/
		rand_spot = rand_int(i);

		/* Place the player, check for failure */
		if (player_place(y_location_tables[rand_spot], x_location_tables[rand_spot]))
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Convert existing terrain type to rubble
 */
static void place_rubble(int y, int x)
{
	/* Create rubble */
	cave_set_feat(y, x, FEAT_RUBBLE);
}


/*
 * Pick either an ordinary up staircase or an up shaft.
 */
static int pick_up_stairs(void)
{
	if (p_ptr->depth >= 2)
	{
		if (one_in_(2)) return (FEAT_LESS_SHAFT);
	}

	return (FEAT_LESS);
}


/*
 * Pick either an ordinary down staircase or an down shaft.
 */
static int pick_down_stairs(void)
{
	if ((p_ptr->depth < MAX_DEPTH - 2) &&
	    (!quest_check(p_ptr->depth + 1)))
	{
		if (one_in_(2)) return (FEAT_MORE_SHAFT);
	}

	return (FEAT_MORE);
}


/*
 * Place an up/down staircase at given location
 */
void place_random_stairs(int y, int x)
{
	/* Paranoia */
	if (!cave_clean_bold(y, x)) return;

	/* Create a staircase */
	if (!p_ptr->depth)
	{
		cave_set_feat(y, x, FEAT_MORE);
	}
	else if ((quest_check(p_ptr->depth)) || (p_ptr->depth >= MAX_DEPTH-1))
	{
		if (one_in_(2))	cave_set_feat(y, x, FEAT_LESS);
		else cave_set_feat(y, x, FEAT_LESS_SHAFT);
	}
	else if (one_in_(2))
	{
		if ((quest_check(p_ptr->depth + 1)) || (p_ptr->depth <= 1))
			cave_set_feat(y, x, FEAT_MORE);
		else if (one_in_(2)) cave_set_feat(y, x, FEAT_MORE);
		else cave_set_feat(y, x, FEAT_MORE_SHAFT);
	}
	else
	{
		if ((one_in_(2)) || (p_ptr->depth == 1)) cave_set_feat(y, x, FEAT_LESS);
		else cave_set_feat(y, x, FEAT_LESS_SHAFT);
	}
}



/*
 * Allocates some objects (using "place" and "type")
 */
static void alloc_object(int set, int typ, int num)
{
	int y, x, k, i;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		for (i = 0; i < 10000; i++)
		{
			bool is_room;

			/* Location */
			y = rand_int(p_ptr->cur_map_hgt);
			x = rand_int(p_ptr->cur_map_wid);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue;

			/* Check for "room" */
			is_room = (cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && is_room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !is_room) continue;

			/* Accept it */
			break;
		}

		/* No point found */
		if (i == 10000) return;

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
				place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
				break;
			}
		}
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
static bool build_streamer(int feat, int chance)
{
	int i, tx, ty;
	int y, x, dir;
	int tries1 = 0;
    int tries2 = 0;

	/* Hack -- Choose starting point */
	y = rand_spread(p_ptr->cur_map_hgt / 2, 10);
	x = rand_spread(p_ptr->cur_map_wid / 2, 15);

	/* Choose a random compass direction */
	dir = ddd[rand_int(8)];

	/* Place streamer into dungeon */
	while (TRUE)
	{
		tries1++;

		if (tries1 > 2500) return(FALSE);

		/* One grid per density */
		for (i = 0; i < DUN_STR_DEN; i++)
		{
			int d = DUN_STR_RNG;

			/* Pick a nearby grid */
			while (TRUE)
			{
				tries2 ++;
				if (tries2 > 2500) return (FALSE);
				ty = rand_spread(y, d);
				tx = rand_spread(x, d);
				if (!in_bounds(ty, tx)) continue;
				break;
			}

			/* Only convert "granite" walls */
			if (cave_feat[ty][tx] < FEAT_WALL_EXTRA) continue;
			if (cave_feat[ty][tx] > FEAT_WALL_SOLID) continue;

			/* Clear previous contents, add proper vein type */
			cave_set_feat(ty, tx, feat);

			/* Hack -- Add some (known) treasure */
			if (rand_int(chance) == 0) cave_feat[ty][tx] += 0x04;
		}

		/* Advance the streamer */
		y += ddy[dir];
		x += ddx[dir];

		/* Stop at dungeon edge */
		if (!in_bounds(y, x)) break;
	}

	return (TRUE);
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
		x1 = rand_range(5, p_ptr->cur_map_hgt-1 - 5);
		y1 = rand_range(5, p_ptr->cur_map_wid-1 - 5);

		/* Big area of affect */
		for (y = (y1 - 15); y <= (y1 + 15); y++)
		{
			for (x = (x1 - 15); x <= (x1 + 15); x++)
			{
				/* Skip illegal grids */
				if (!in_bounds_fully(y, x)) continue;

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
						cave_set_feat(y, x, FEAT_WALL_EXTRA);
					}

					/* Quartz */
					else if (t < 70)
					{
						/* Create quartz vein */
						cave_set_feat(y, x, FEAT_QUARTZ);
					}

					/* Magma */
					else if (t < 100)
					{
						/* Create magma vein */
						cave_set_feat(y, x, FEAT_MAGMA);
					}

					/* Floor */
					else
					{
						/* Create floor */
						cave_set_feat(y, x, FEAT_FLOOR);
					}

					/* No longer part of a room or vault */
					cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

					/* No longer illuminated */
					cave_info[y][x] &= ~(CAVE_GLOW);
				}
			}
		}
	}
}



/*
 * Create up to "num" objects near the given coordinates
 * Only really called by some of the "vault" routines.
 */
static void vault_objects(int y, int x, int num)
{
	int i, j, k;

	/* Attempt to place 'num' objects */
	for (; num > 0; --num)
	{
		/* Try up to 11 spots looking for empty space */
		for (i = 0; i < 11; ++i)
		{
			/* Pick a random location */
			while (1)
			{
				j = rand_spread(y, 2);
				k = rand_spread(x, 3);
				if (!in_bounds(j, k)) continue;
				break;
			}

			/* Require "clean" floor space */
			if (!cave_clean_bold(j, k)) continue;

			/* Place an item */
			if (rand_int(100) < 75)
			{
				place_object(j, k, FALSE, FALSE, DROP_TYPE_UNTHEMED);
			}

			/* Place gold */
			else
			{
				place_gold(j, k);
			}

			/* Placement accomplished */
			break;
		}
	}
}


/*
 * Place a trap with a given displacement of point
 */
static void vault_trap_aux(int y, int x, int yd, int xd)
{
	int count, y1, x1;

	/* Place traps */
	for (count = 0; count <= 5; count++)
	{
		/* Get a location */
		while (1)
		{
			y1 = rand_spread(y, yd);
			x1 = rand_spread(x, xd);
			if (!in_bounds(y1, x1)) continue;
			break;
		}

		/* Require "naked" floor grids */
		if (!cave_naked_bold(y1, x1)) continue;

		/* Place the trap */
		place_trap(y1, x1);

		/* Done */
		break;
	}
}


/*
 * Place some traps with a given displacement of given location
 */
static void vault_traps(int y, int x, int yd, int xd, int num)
{
	int i;

	for (i = 0; i < num; i++)
	{
		vault_trap_aux(y, x, yd, xd);
	}
}


/*
 * Place some sleeping monsters near the given location
 */
static void vault_monsters(int y1, int x1, int num)
{
	int k, i, y, x;
	int mon_level_old = monster_level;

	/* Temporary increase monster level */
	monster_level += 2;

	/* Try to summon "num" monsters "near" the given location */
	for (k = 0; k < num; k++)
	{
		/* Try nine locations */
		for (i = 0; i < 9; i++)
		{
			int d = 1;

			/* Pick a nearby location */
			scatter(&y, &x, y1, x1, d, 0);

			/* Require "empty" floor grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Place the monster (allow groups) */
			(void)place_monster(y, x, TRUE, TRUE);

			break;
		}
	}

	/* Restore monster level */
	monster_level = mon_level_old;
}



/*
 * Generate helper -- create a new room with optional light
 */
static void generate_room(int y1, int x1, int y2, int x2, int light)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_info[y][x] |= (CAVE_ROOM);
			if (light) cave_info[y][x] |= (CAVE_GLOW);
		}
	}
}


/*
 * Generate helper -- fill a rectangle with a feature
 */
static void generate_fill(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_set_feat(y, x, feat);
		}
	}
}


/*
 * Generate helper -- draw a rectangle with a feature
 */
static void generate_draw(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		cave_set_feat(y, x1, feat);
		cave_set_feat(y, x2, feat);
	}

	for (x = x1; x <= x2; x++)
	{
		cave_set_feat(y1, x, feat);
		cave_set_feat(y2, x, feat);
	}
}


/*
 * Generate helper -- split a rectangle with a feature
 */
static void generate_plus(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	for (y = y1; y <= y2; y++)
	{
		cave_set_feat(y, x0, feat);
	}

	for (x = x1; x <= x2; x++)
	{
		cave_set_feat(y0, x, feat);
	}
}


/*
 * Generate helper -- open all sides of a rectangle with a feature
 */
static void generate_open(int y1, int x1, int y2, int x2, int feat)
{
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	/* Open all sides */
	cave_set_feat(y1, x0, feat);
	cave_set_feat(y0, x1, feat);
	cave_set_feat(y2, x0, feat);
	cave_set_feat(y0, x2, feat);
}


/*
 * Generate helper -- open one side of a rectangle with a feature
 */
static void generate_hole(int y1, int x1, int y2, int x2, int feat)
{
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	/* Open random side */
	switch (rand_int(4))
	{
		case 0:
		{
			cave_set_feat(y1, x0, feat);
			break;
		}
		case 1:
		{
			cave_set_feat(y0, x1, feat);
			break;
		}
		case 2:
		{
			cave_set_feat(y2, x0, feat);
			break;
		}
		case 3:
		{
			cave_set_feat(y0, x2, feat);
			break;
		}
	}
}


/*
 * Room building routines.
 *
 * Six basic room types:
 *   1 -- normal
 *   2 -- overlapping
 *   3 -- cross shaped
 *   4 -- large room with features
 *   5 -- monster nests
 *   6 -- monster pits
 *   7 -- simple vaults
 *   8 -- greater vaults
 */


/*
 * Type 1 -- normal rectangular rooms
 */
static void build_type1(int y0, int x0)
{
	int y, x;

	int y1, x1, y2, x2;

	int light = FALSE;

	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;

	/* Pick a room size */
	y1 = y0 - randint(4);
	x1 = x0 - randint(11);
	y2 = y0 + randint(3);
	x2 = x0 + randint(11);


	/* Generate new room */
	generate_room(y1-1, x1-1, y2+1, x2+1, light);

	/* Generate outer walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);


	/* Hack -- Occasional pillar room */
	if (one_in_(20))
	{
		for (y = y1; y <= y2; y += 2)
		{
			for (x = x1; x <= x2; x += 2)
			{
				cave_set_feat(y, x, FEAT_WALL_INNER);
			}
		}
	}

	/* Hack -- Occasional ragged-edge room */
	else if (one_in_(50))
	{
		for (y = y1 + 2; y <= y2 - 2; y += 2)
		{
			cave_set_feat(y, x1, FEAT_WALL_INNER);
			cave_set_feat(y, x2, FEAT_WALL_INNER);
		}

		for (x = x1 + 2; x <= x2 - 2; x += 2)
		{
			cave_set_feat(y1, x, FEAT_WALL_INNER);
			cave_set_feat(y2, x, FEAT_WALL_INNER);
		}
	}
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static void build_type2(int y0, int x0)
{
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;

	int light = FALSE;


	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;


	/* Determine extents of room (a) */
	y1a = y0 - randint(4);
	x1a = x0 - randint(11);
	y2a = y0 + randint(3);
	x2a = x0 + randint(10);


	/* Determine extents of room (b) */
	y1b = y0 - randint(3);
	x1b = x0 - randint(10);
	y2b = y0 + randint(4);
	x2b = x0 + randint(11);


	/* Generate new room (a) */
	generate_room(y1a-1, x1a-1, y2a+1, x2a+1, light);

	/* Generate new room (b) */
	generate_room(y1b-1, x1b-1, y2b+1, x2b+1, light);

	/* Generate outer walls (a) */
	generate_draw(y1a-1, x1a-1, y2a+1, x2a+1, FEAT_WALL_OUTER);

	/* Generate outer walls (b) */
	generate_draw(y1b-1, x1b-1, y2b+1, x2b+1, FEAT_WALL_OUTER);

	/* Generate inner floors (a) */
	generate_fill(y1a, x1a, y2a, x2a, FEAT_FLOOR);

	/* Generate inner floors (b) */
	generate_fill(y1b, x1b, y2b, x2b, FEAT_FLOOR);
}



/*
 * Type 3 -- Cross shaped rooms
 *
 * Room "a" runs north/south, and Room "b" runs east/east
 * So a "central pillar" would run from x1a,y1b to x2a,y2b.
 *
 * Note that currently, the "center" is always 3x3, but I think that
 * the code below will work for 5x5 (and perhaps even for unsymetric
 * values like 4x3 or 5x3 or 3x4 or 3x5).
 */
static void build_type3(int y0, int x0)
{
	int y, x;

	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;

	int dy, dx, wy, wx;

	int light = FALSE;


	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;


	/* Pick inner dimension */
	wy = 1;
	wx = 1;

	/* Pick outer dimension */
	dy = rand_range(3, 4);
	dx = rand_range(3, 11);


	/* Determine extents of room (a) */
	y1a = y0 - dy;
	x1a = x0 - wx;
	y2a = y0 + dy;
	x2a = x0 + wx;

	/* Determine extents of room (b) */
	y1b = y0 - wy;
	x1b = x0 - dx;
	y2b = y0 + wy;
	x2b = x0 + dx;


	/* Generate new room (a) */
	generate_room(y1a-1, x1a-1, y2a+1, x2a+1, light);

	/* Generate new room (b) */
	generate_room(y1b-1, x1b-1, y2b+1, x2b+1, light);

	/* Generate outer walls (a) */
	generate_draw(y1a-1, x1a-1, y2a+1, x2a+1, FEAT_WALL_OUTER);

	/* Generate outer walls (b) */
	generate_draw(y1b-1, x1b-1, y2b+1, x2b+1, FEAT_WALL_OUTER);

	/* Generate inner floors (a) */
	generate_fill(y1a, x1a, y2a, x2a, FEAT_FLOOR);

	/* Generate inner floors (b) */
	generate_fill(y1b, x1b, y2b, x2b, FEAT_FLOOR);


	/* Special features */
	switch (randint(4))
	{
		/* Nothing */
		case 1:
		{
			break;
		}

		/* Large solid middle pillar */
		case 2:
		{
			/* Generate a small inner solid pillar */
			generate_fill(y1b, x1a, y2b, x2a, FEAT_WALL_INNER);

			break;
		}

		/* Inner treasure vault */
		case 3:
		{
			/* Generate a small inner vault */
			generate_draw(y1b, x1a, y2b, x2a, FEAT_WALL_INNER);

			/* Open the inner vault with a secret door */
			generate_hole(y1b, x1a, y2b, x2a, FEAT_SECRET);

			/* Place a treasure in the vault */
			place_object(y0, x0, FALSE, FALSE, DROP_TYPE_UNTHEMED);

			/* Let's guard the treasure well */
			vault_monsters(y0, x0, rand_int(2) + 3);

			/* Traps naturally */
			vault_traps(y0, x0, 4, 4, rand_int(3) + 2);

			break;
		}

		/* Something else */
		case 4:
		{
			/* Occasionally pinch the center shut */
			if (rand_int(3) == 0)
			{
				/* Pinch the east/west sides */
				for (y = y1b; y <= y2b; y++)
				{
					if (y == y0) continue;
					cave_set_feat(y, x1a - 1, FEAT_WALL_INNER);
					cave_set_feat(y, x2a + 1, FEAT_WALL_INNER);
				}

				/* Pinch the north/south sides */
				for (x = x1a; x <= x2a; x++)
				{
					if (x == x0) continue;
					cave_set_feat(y1b - 1, x, FEAT_WALL_INNER);
					cave_set_feat(y2b + 1, x, FEAT_WALL_INNER);
				}

				/* Open sides with secret doors */
				if (rand_int(3) == 0)
				{
					generate_open(y1b-1, x1a-1, y2b+1, x2a+1, FEAT_SECRET);
				}
			}

			/* Occasionally put a "plus" in the center */
			else if (rand_int(3) == 0)
			{
				generate_plus(y1b, x1a, y2b, x2a, FEAT_WALL_INNER);
			}

			/* Occasionally put a "pillar" in the center */
			else if (rand_int(3) == 0)
			{
				cave_set_feat(y0, x0, FEAT_WALL_INNER);
			}

			break;
		}
	}
}


/*
 * Type 4 -- Large room with an inner room
 *
 * Possible sub-types:
 *	1 - An inner room
 *	2 - An inner room with a small inner room
 *	3 - An inner room with a pillar or pillars
 *	4 - An inner room with a checkerboard
 *	5 - An inner room with four compartments
 */
static void build_type4(int y0, int x0)
{
	int y, x, y1, x1, y2, x2;

	int light = FALSE;


	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;


	/* Large room */
	y1 = y0 - 4;
	y2 = y0 + 4;
	x1 = x0 - 11;
	x2 = x0 + 11;


	/* Generate new room */
	generate_room(y1-1, x1-1, y2+1, x2+1, light);

	/* Generate outer walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);


	/* The inner room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_INNER);


	/* Inner room variations */
	switch (randint(5))
	{
		/* An inner room */
		case 1:
		{
			/* Open the inner room with a secret door */
			generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

			/* Place a monster in the room */
			vault_monsters(y0, x0, 1);

			break;
		}


		/* An inner room with a small inner room */
		case 2:
		{
			/* Open the inner room with a secret door */
			generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

			/* Place another inner room */
			generate_draw(y0-1, x0-1, y0+1, x0+1, FEAT_WALL_INNER);

			/* Open the inner room with a locked door */
			generate_hole(y0-1, x0-1, y0+1, x0+1, FEAT_DOOR_HEAD + randint(7));

			/* Monsters to guard the treasure */
			vault_monsters(y0, x0, randint(3) + 2);

			/* Object (80%) */
			if (rand_int(100) < 80)
			{
				place_object(y0, x0, FALSE, FALSE, DROP_TYPE_UNTHEMED);
			}

			/* Stairs (20%) */
			else
			{
				place_random_stairs(y0, x0);
			}

			/* Traps to protect the treasure */
			vault_traps(y0, x0, 4, 10, 2 + randint(3));

			break;
		}


		/* An inner room with an inner pillar or pillars */
		case 3:
		{
			/* Open the inner room with a secret door */
			generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

			/* Inner pillar */
			generate_fill(y0-1, x0-1, y0+1, x0+1, FEAT_WALL_INNER);

			/* Occasionally, two more Large Inner Pillars */
			if (rand_int(2) == 0)
			{
				/* Three spaces */
				if (rand_int(100) < 50)
				{
					/* Inner pillar */
					generate_fill(y0-1, x0-7, y0+1, x0-5, FEAT_WALL_INNER);

					/* Inner pillar */
					generate_fill(y0-1, x0+5, y0+1, x0+7, FEAT_WALL_INNER);
				}

				/* Two spaces */
				else
				{
					/* Inner pillar */
					generate_fill(y0-1, x0-6, y0+1, x0-4, FEAT_WALL_INNER);

					/* Inner pillar */
					generate_fill(y0-1, x0+4, y0+1, x0+6, FEAT_WALL_INNER);
				}
			}

			/* Occasionally, some Inner rooms */
			if (rand_int(3) == 0)
			{
				/* Inner rectangle */
				generate_draw(y0-1, x0-5, y0+1, x0+5, FEAT_WALL_INNER);

				/* Secret doors (random top/bottom) */
				place_secret_door(y0 - 3 + (randint(2) * 2), x0 - 3);
				place_secret_door(y0 - 3 + (randint(2) * 2), x0 + 3);

				/* Monsters */
				vault_monsters(y0, x0 - 2, randint(2));
				vault_monsters(y0, x0 + 2, randint(2));

				/* Objects */
				if (rand_int(3) == 0) place_object(y0, x0 - 2, FALSE, FALSE, DROP_TYPE_UNTHEMED);
				if (rand_int(3) == 0) place_object(y0, x0 + 2, FALSE, FALSE, DROP_TYPE_UNTHEMED);
			}

			break;
		}


		/* An inner room with a checkerboard */
		case 4:
		{
			/* Open the inner room with a secret door */
			generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

			/* Checkerboard */
			for (y = y1; y <= y2; y++)
			{
				for (x = x1; x <= x2; x++)
				{
					if ((x + y) & 0x01)
					{
						cave_set_feat(y, x, FEAT_WALL_INNER);
					}
				}
			}

			/* Monsters just love mazes. */
			vault_monsters(y0, x0 - 5, randint(3));
			vault_monsters(y0, x0 + 5, randint(3));

			/* Traps make them entertaining. */
			vault_traps(y0, x0 - 3, 2, 8, randint(3));
			vault_traps(y0, x0 + 3, 2, 8, randint(3));

			/* Mazes should have some treasure too. */
			vault_objects(y0, x0, 3);

			break;
		}


		/* Four small rooms. */
		case 5:
		{
			/* Inner "cross" */
			generate_plus(y1, x1, y2, x2, FEAT_WALL_INNER);

			/* Doors into the rooms */
			if (rand_int(100) < 50)
			{
				int i = randint(10);
				place_secret_door(y1 - 1, x0 - i);
				place_secret_door(y1 - 1, x0 + i);
				place_secret_door(y2 + 1, x0 - i);
				place_secret_door(y2 + 1, x0 + i);
			}
			else
			{
				int i = randint(3);
				place_secret_door(y0 + i, x1 - 1);
				place_secret_door(y0 - i, x1 - 1);
				place_secret_door(y0 + i, x2 + 1);
				place_secret_door(y0 - i, x2 + 1);
			}

			/* Treasure, centered at the center of the cross */
			vault_objects(y0, x0, 2 + randint(2));

			/* Gotta have some monsters */
			vault_monsters(y0 + 1, x0 - 4, randint(4));
			vault_monsters(y0 + 1, x0 + 4, randint(4));
			vault_monsters(y0 - 1, x0 - 4, randint(4));
			vault_monsters(y0 - 1, x0 + 4, randint(4));

			break;
		}
	}
}



/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a monster nest or monster pit or
 * the given type.
 *
 * None of the pits/nests are allowed to include "unique" monsters,
 * or monsters which can "multiply".
 *
 * Some of the pits/nests are asked to avoid monsters which can blink
 * away or which are invisible.  This is probably a hack.
 *
 * The old method used monster "names", which was bad, but the new
 * method uses monster race characters, which is also bad.  XXX XXX XXX
 */


/*
 * Helper function for "monster nest (jelly)"
 */
static bool vault_aux_jelly(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Require icky thing, jelly, mold, or mushroom */
	if (!strchr("ijm,", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (kobolds, orcs, nagas and yeeks)"
 */
static bool vault_aux_kobold_yeek_orc_naga(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "k", "o", or "y" monsters */
	if (!strchr("koyn", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (humanoid)"
 */
static bool vault_aux_humanoids(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "p" or "h" monsters */
	if (!strchr("ph", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (young dragon)"
 */
static bool vault_aux_youngdragon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "d" monsters */
	if (!strchr("d", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (animal)"
 */
static bool vault_aux_animal(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Hack -- No "Q" monsters */
	if (strchr("Q", r_ptr->d_char)) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Require "animal" flag */
	if (!(r_ptr->flags3 & (RF3_ANIMAL))) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (undead)"
 */
static bool vault_aux_undead(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Require Undead */
	if (!(r_ptr->flags3 & (RF3_UNDEAD))) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (orc)"
 */
static bool vault_aux_orc(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "o" monsters */
	if (!strchr("o", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (troll)"
 */
static bool vault_aux_troll(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "T" monsters */
	if (!strchr("T", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (troll)"
 */
static bool vault_aux_orc_ogre_troll_giant(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "T, o, O, or P" monsters */
	if (!strchr("ToOP", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (coins)"
 */
static bool vault_aux_coins(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "$" monsters */
	if (!strchr("$", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (hydras)"
 */
static bool vault_aux_hydras(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "T" monsters */
	if (!strchr("M", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (ogre)"
 */
static bool vault_aux_ogre(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "O" monsters */
	if (!strchr("O", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (hounds)"
 */
static bool vault_aux_hounds(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "Z" monsters */
	if (!strchr("Z", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (giant)"
 */
static bool vault_aux_giant(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "P" monsters */
	if (!strchr("P", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Hack -- breath type for "vault_aux_dragon()"
 */
static u32b vault_aux_dragon_mask4;


/*
 * Helper function for "monster pit (dragon)"
 */
static bool vault_aux_dragon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Hack -- Require "d" or "D" monsters */
	if (!strchr("Dd", r_ptr->d_char)) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require correct "breath attack" */
	if (r_ptr->flags4 != vault_aux_dragon_mask4) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (ancient dragon)"
 */
static bool vault_aux_ancdragon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "D" monsters */
	if (!strchr("D", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_all_demons(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "U" monsters */
	if (!strchr("Uu", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (minor demon)"
 */
static bool vault_aux_minor_demon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "U" monsters */
	if (!strchr("u", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_major_demon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- Require "U" monsters */
	if (!strchr("U", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}



/*set the get_mon_num_hook functional pointer based on theme*/
void get_mon_hook(byte theme)
{
	if 		(theme == LEV_THEME_CREEPING_COIN)	get_mon_num_hook = vault_aux_coins;
	else if (theme == LEV_THEME_ORC)			get_mon_num_hook = vault_aux_orc;
	else if (theme == LEV_THEME_TROLL)			get_mon_num_hook = vault_aux_troll;
	else if (theme == LEV_THEME_OGRE)			get_mon_num_hook = vault_aux_ogre;
	else if (theme == LEV_THEME_HOUND)			get_mon_num_hook = vault_aux_hounds;
	else if (theme == LEV_THEME_HYDRA) 			get_mon_num_hook = vault_aux_hydras;
	else if (theme == LEV_THEME_GIANT)			get_mon_num_hook = vault_aux_giant;
	else if (theme == LEV_THEME_DRAGON_YOUNG)	get_mon_num_hook = vault_aux_youngdragon;
	else if (theme == LEV_THEME_DRAGON_ACID)
	{
		get_mon_num_hook = vault_aux_dragon;
		vault_aux_dragon_mask4 = RF4_BRTH_ACID;
	}
	else if (theme == LEV_THEME_DRAGON_FIRE)
	{
		get_mon_num_hook = vault_aux_dragon;
		vault_aux_dragon_mask4 = RF4_BRTH_FIRE;
	}
	else if (theme == LEV_THEME_DRAGON_ELEC)
	{
		get_mon_num_hook = vault_aux_dragon;
		vault_aux_dragon_mask4 = RF4_BRTH_ELEC;
	}
	else if (theme == LEV_THEME_DRAGON_COLD)
	{
		get_mon_num_hook = vault_aux_dragon;
		vault_aux_dragon_mask4 = RF4_BRTH_COLD;
	}
	else if (theme == LEV_THEME_DRAGON_POIS)
	{
		get_mon_num_hook = vault_aux_dragon;
		vault_aux_dragon_mask4 = RF4_BRTH_POIS;
	}
	else if (theme == LEV_THEME_DRAGON_CHROMATIC)
	{
		get_mon_num_hook = vault_aux_dragon;
		vault_aux_dragon_mask4 = RF4_ELEM_BREATH_MASK;
	}
	else if (theme == LEV_THEME_DRAGON_MISC)
	{
		get_mon_num_hook = vault_aux_dragon;
	}
	else if (theme == LEV_THEME_DRAGON_ANCIENT)	get_mon_num_hook = vault_aux_ancdragon;
	else if (theme == LEV_THEME_JELLY)			get_mon_num_hook = vault_aux_jelly;
	else if (theme == LEV_THEME_ORC_NAGA_YEEK_KOBOLD)	get_mon_num_hook = vault_aux_kobold_yeek_orc_naga;
	else if (theme == LEV_THEME_ANIMAL)			get_mon_num_hook = vault_aux_animal;
	else if (theme == LEV_THEME_HUMANOID)		get_mon_num_hook = vault_aux_humanoids;
	else if (theme == LEV_THEME_DEMON_MINOR)	get_mon_num_hook = vault_aux_minor_demon;
	else if (theme == LEV_THEME_DEMON_ALL)		get_mon_num_hook = vault_aux_all_demons;
	else if (theme == LEV_THEME_DEMON_MAJOR)	get_mon_num_hook = vault_aux_major_demon;
	else if (theme == LEV_THEME_CAVE_DWELLER)	get_mon_num_hook = vault_aux_orc_ogre_troll_giant;
	else if (theme == LEV_THEME_UNDEAD)			get_mon_num_hook = vault_aux_undead;
}

/*return a theme for a monster nest*/
byte get_nest_theme(int nestlevel)
{
	int mindepth, whatnest;

	/*enforce minimum depth, to keep weak nests out of deep levels*/
	mindepth = nestlevel / 4;

	/*keep total to 100 or less*/
	if ((mindepth + nestlevel) > 100) nestlevel = 100 - mindepth;

	/* Hack -- Choose a nest type */
	whatnest = randint(nestlevel) + mindepth;

	if ((whatnest <= 25)  && (nestlevel <= 35))
	{
		/*coins, jelly, or kobolds/yeeks/orcs*/
		if (one_in_(3))			return LEV_THEME_CREEPING_COIN;
		else if (one_in_(2))	return LEV_THEME_JELLY;
		else					return LEV_THEME_ORC_NAGA_YEEK_KOBOLD;
	}

	/*hydras or young dragons*/
	else if (whatnest <= 50)
	{
		if (one_in_(3))			return LEV_THEME_CAVE_DWELLER;
		else if (one_in_(2))	return LEV_THEME_DRAGON_YOUNG;
		else					return LEV_THEME_HYDRA;
	}

	/*Animals or humanoids*/
	else if (whatnest <=75)
	{
		if (one_in_(2))			return LEV_THEME_ANIMAL;
		else					return LEV_THEME_HUMANOID;
	}

	/*Monster nest (undead) */
	else if (whatnest <=95)		return LEV_THEME_UNDEAD;

	/*Ancient Dragon Nest*/
	else						return LEV_THEME_DRAGON_ANCIENT;
}

/*return a theme for a monster pit*/
byte get_pit_theme(int pitlevel)
{
	int mindepth, whatpit;

	/*enforce minimum depth, to keep weak nests out of deep levels*/
	mindepth = pitlevel / 4;

	/*keep total to 100 or less, so results aren't skewed at deep depths*/
	if ((mindepth + pitlevel) > 100) pitlevel = 100 - mindepth;

	/* Hack -- Choose a nest type */
	whatpit = randint(pitlevel) + mindepth;

	/* Orc pit */
	if ((whatpit <= 20) && (pitlevel <= 35))
	{
		if (one_in_(2))	return LEV_THEME_CREEPING_COIN;
		else 			return LEV_THEME_ORC;
	}

	/*troll or ogre*/
	else if ((whatpit <= 35)  && (pitlevel <= 45))
	{
		if (one_in_(3))			return LEV_THEME_CAVE_DWELLER;
		else if (one_in_(2))	return LEV_THEME_TROLL;
		else			return LEV_THEME_OGRE;
	}
	else if ((whatpit <= 50) && (p_ptr->depth <= 60))
	{
		/* Hound, youngdragon, or hydra pit */
		if (one_in_(3))			return LEV_THEME_HOUND;
		else if (one_in_(2))	return LEV_THEME_HYDRA;
		else					return LEV_THEME_DRAGON_YOUNG;
	}

	/* Giant pit */
	else if ((whatpit <= 60) && (p_ptr->depth <= 80))	return LEV_THEME_GIANT;

	/* Dragon pit */
	else if (whatpit <= 80)
	{
		/* Pick dragon type */
		switch (rand_int(6))
		{
			/* Black */
			case 0: return LEV_THEME_DRAGON_ACID;
			/* Blue */
			case 1: return LEV_THEME_DRAGON_ELEC;
			/* Red */
			case 2: return LEV_THEME_DRAGON_FIRE;
			/* White */
			case 3: return LEV_THEME_DRAGON_COLD;
			/* Green */
			case 4: return LEV_THEME_DRAGON_POIS;
			/* Chromatic */
			default:return LEV_THEME_DRAGON_CHROMATIC;
		}
	}

	/* Ancient Dragon pit */
	else if (whatpit <= 90)	return LEV_THEME_DRAGON_ANCIENT;

	/* Demon pit */
	else					return LEV_THEME_DEMON_MAJOR;
}


/*
 * Type 5 -- Monster nests
 *
 * A monster nest is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type strewn about the room.
 *
 * The monsters are chosen from a set of 64 randomly selected monster
 * races, to allow the nest creation to fail instead of having "holes".
 *
 * Note the use of the "get_mon_num_prep()" function, and the special
 * "get_mon_num_hook()" restriction function, to prepare the "monster
 * allocation table" in such a way as to optimize the selection of
 * "appropriate" non-unique monsters for the nest.
 *
 * Note that the "get_mon_num()" function may (rarely) fail, in which
 * case the nest will be empty, and will not affect the level rating.
 *
 * Note that "monster nests" will never contain "unique" monsters.
 */
static void build_type_nest(int y0, int x0)
{
	int y, x, y1, x1, y2, x2;

	int i, j, harder_nest_check;

	s16b what[64];

	byte room_theme;

	bool empty = FALSE;

	int light = FALSE;

	byte is_quest_level = FALSE;

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/*no uniques in nests/pits*/
	allow_uniques = FALSE;

	/* Large room */
	y1 = y0 - 4;
	y2 = y0 + 4;
	x1 = x0 - 11;
	x2 = x0 + 11;

	/*check if we need a quest*/
	if (quest_check(p_ptr->depth) == QUEST_NEST) is_quest_level = TRUE;

	/* Generate new room */
	generate_room(y1-1, x1-1, y2+1, x2+1, light);

	/* Generate outer walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_INNER);

	/* Open the inner room with one or two secret doors */
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

	/*select the theme, or get the quest level theme*/
	if (is_quest_level) room_theme = q_ptr->theme;
	else room_theme = get_nest_theme(p_ptr->depth);

	/*get the mon_hook*/
	get_mon_hook(room_theme);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(p_ptr->depth +
							  (is_quest_level ? PIT_NEST_QUEST_BOOST : NEST_LEVEL_BOOST));

		/* Notice failure */
		if (!what[i]) empty = TRUE;
	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/*allow uniques again*/
	allow_uniques = TRUE;

	/* Oops */
	if (empty) return;

	/* Describe */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster nest (%s)", feeling_themed_level[room_theme]);
	}

	/* Increase the level rating */
	rating += 10;

	/* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
	if ((p_ptr->depth <= 40) &&
	    (randint(p_ptr->depth * p_ptr->depth + 1) < 300))
	{
		good_item_flag = TRUE;
	}

	/* Sort the entries XXX XXX XXX */
	for (i = 0; i < 64 - 1; i++)
	{
		/* Sort the entries */
		for (j = 0; j < 64 - 1; j++)
		{
			int i1 = j;
			int i2 = j + 1;

			int p1 = r_info[what[i1]].level;
			int p2 = r_info[what[i2]].level;

			/* Bubble */
			if (p1 > p2)
			{
				int tmp = what[i1];
				what[i1] = what[i2];
				what[i2] = tmp;
			}
		}
	}

	/* occasionally make nest of hardest monsters,
	 * if a random number of 100 is
	 * less than the level of the hardest
	 * monster minus the current level
	 */
	harder_nest_check = r_info[what[63]].level - p_ptr->depth;

	/*Hack - make some pits harder if deeper*/
	if (randint(100) < harder_nest_check)
	{
		/* Use the top 8 entries */
		for (i = 0; i < 32; i++)
		{

			what[i] = what[i + 32];
		}
	}

	/* Place some monsters */
	for (y = y0 - 2; y <= y0 + 2; y++)
	{
		for (x = x0 - 9; x <= x0 + 9; x++)
		{
			int r_idx = what[rand_int(64)];

			/* Place that "random" monster (no groups) */
			(void)place_monster_aux(y, x, r_idx, FALSE, FALSE);
		}
	}

	/* No teleporting inside pits/nests*/
	for (y = y0 - 2; y <= y0 + 2; y++)
	{
		for (x = x0 - 9; x <= x0 + 9; x++)
		{
			/* Part of a vault */
			cave_info[y][x] |= (CAVE_ICKY);
		}
	}


	/*final preps if this is a quest level*/
	if (is_quest_level)
	{
		q_ptr->cur_num = 0;
		q_ptr->max_num = 0;

		/* Square-by-square grid search for monsters */
		for (y = y0 - 2; y <= y0 + 2; y++)
		{
			for (x = x0 - 9; x <= x0 + 9; x++)
			{
				/*Is there a monster here?*/
				if (cave_m_idx[y][x] > 0)
				{
					monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

					/*mark it as a quest monster*/
					m_ptr->mflag |= (MFLAG_QUEST);

					/*increase the max_num counter*/
					q_ptr->max_num ++;
				}
			}
		}
	}


}



/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type organized in the room.
 *
 *
 * The inside room in a monster pit appears as shown below, where the
 * actual monsters in each location depend on the type of the pit
 *
 *   #####################
 *   #0000000000000000000#
 *   #0112233455543322110#
 *   #0112233467643322110#
 *   #0112233455543322110#
 *   #0000000000000000000#
 *   #####################
 *
 * Note that the monsters in the pit are now chosen by using "get_mon_num()"
 * to request 16 "appropriate" monsters, sorting them by level, and using
 * the "even" entries in this sorted list for the contents of the pit.
 *
 * Hack -- all of the "dragons" in a "dragon" pit must be the same "color",
 * which is handled by requiring a specific "breath" attack for all of the
 * dragons.  This may include "multi-hued" breath.  Note that "wyrms" may
 * be present in many of the dragon pits, if they have the proper breath.
 *
 * Note the use of the "get_mon_num_prep()" function, and the special
 * "get_mon_num_hook()" restriction function, to prepare the "monster
 * allocation table" in such a way as to optimize the selection of
 * "appropriate" non-unique monsters for the pit.
 *
 * Note that the "get_mon_num()" function may (rarely) fail, in which case
 * the pit will be empty, and will not effect the level rating.
 *
 * Note that "monster pits" will never contain "unique" monsters.
 */
static void build_type_pit(int y0, int x0)
{
	int what[16], harder_pit_check;

	int i, j, y, x, y1, x1, y2, x2;

	bool empty = FALSE;

	int light = FALSE;

	byte pit_theme;

	byte is_quest_level = FALSE;

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/*no uniques in nests/pits*/
	allow_uniques = FALSE;

	/*check if we need a quest*/
	if (quest_check(p_ptr->depth) == QUEST_PIT) is_quest_level = TRUE;

	/* Large room */
	y1 = y0 - 4;
	y2 = y0 + 4;
	x1 = x0 - 11;
	x2 = x0 + 11;

	/* Generate new room */
	generate_room(y1-1, x1-1, y2+1, x2+1, light);

	/* Generate outer walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_INNER);

	/* Open the inner room with one or two secret doors */
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

	/* Choose a pit type */
	if(is_quest_level) pit_theme = q_ptr->theme;
	else pit_theme = get_pit_theme(p_ptr->depth);

	/*get the monster hook*/
	get_mon_hook(pit_theme);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick some monster types */
	for (i = 0; i < 16; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(p_ptr->depth +
							  (is_quest_level ? PIT_NEST_QUEST_BOOST : PIT_LEVEL_BOOST));

		/* Notice failure */
		if (!what[i]) empty = TRUE;
	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/*allow uniques again*/
	allow_uniques = TRUE;

	/* Oops */
	if (empty) return;

	/* Sort the entries XXX XXX XXX */
	for (i = 0; i < 16 - 1; i++)
	{
		/* Sort the entries */
		for (j = 0; j < 16 - 1; j++)
		{
			int i1 = j;
			int i2 = j + 1;

			int p1 = r_info[what[i1]].level;
			int p2 = r_info[what[i2]].level;

			/* Bubble */
			if (p1 > p2)
			{
				int tmp = what[i1];
				what[i1] = what[i2];
				what[i2] = tmp;
			}
		}
	}

	/* occasionally make nest of hardest monsters,
	 * if a random number of 100 is
	 * less than the level of the hardest
	 * monster minus the current level
	 */
	harder_pit_check = r_info[what[15]].level - p_ptr->depth;

	/*Hack - make some pits harder if deeper*/
	if (randint(100) < harder_pit_check)
	{
		/* Use the top 8 entries */
		for (i = 0; i < 8; i++)
		{

			what[i] = what[i + 8];
		}
	}

	/* Use every other entry */
	else
	{
		for (i = 0; i < 8; i++)
		{
			/* Every other entry */
			what[i] = what[i * 2];
		}
	}



	/* Message */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster pit (%s)", feeling_themed_level[pit_theme]);
	}


	/* Increase the level rating */
	rating += 10;

	/* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
	if ((p_ptr->depth <= 40) &&
	    (randint(p_ptr->depth * p_ptr->depth + 1) < 300))
	{
		good_item_flag = TRUE;
	}

	/* Top and bottom rows */
	for (x = x0 - 9; x <= x0 + 9; x++)
	{
		place_monster_aux(y0 - 2, x, what[0], FALSE, FALSE);
		place_monster_aux(y0 + 2, x, what[0], FALSE, FALSE);
	}

	/* Middle columns */
	for (y = y0 - 1; y <= y0 + 1; y++)
	{
		place_monster_aux(y, x0 - 9, what[0], FALSE, FALSE);
		place_monster_aux(y, x0 + 9, what[0], FALSE, FALSE);

		place_monster_aux(y, x0 - 8, what[1], FALSE, FALSE);
		place_monster_aux(y, x0 + 8, what[1], FALSE, FALSE);

		place_monster_aux(y, x0 - 7, what[1], FALSE, FALSE);
		place_monster_aux(y, x0 + 7, what[1], FALSE, FALSE);

		place_monster_aux(y, x0 - 6, what[2], FALSE, FALSE);
		place_monster_aux(y, x0 + 6, what[2], FALSE, FALSE);

		place_monster_aux(y, x0 - 5, what[2], FALSE, FALSE);
		place_monster_aux(y, x0 + 5, what[2], FALSE, FALSE);

		place_monster_aux(y, x0 - 4, what[3], FALSE, FALSE);
		place_monster_aux(y, x0 + 4, what[3], FALSE, FALSE);

		place_monster_aux(y, x0 - 3, what[3], FALSE, FALSE);
		place_monster_aux(y, x0 + 3, what[3], FALSE, FALSE);

		place_monster_aux(y, x0 - 2, what[4], FALSE, FALSE);
		place_monster_aux(y, x0 + 2, what[4], FALSE, FALSE);
	}

	/* Above/Below the center monster */
	for (x = x0 - 1; x <= x0 + 1; x++)
	{
		place_monster_aux(y0 + 1, x, what[5], FALSE, FALSE);
		place_monster_aux(y0 - 1, x, what[5], FALSE, FALSE);
	}

	/* Next to the center monster */
	place_monster_aux(y0, x0 + 1, what[6], FALSE, FALSE);
	place_monster_aux(y0, x0 - 1, what[6], FALSE, FALSE);

	/* Center monster */
	place_monster_aux(y0, x0, what[7], FALSE, FALSE);

	/* No teleporting inside pits/nests*/
	for (y = y0 - 2; y <= y0 + 2; y++)
	{
		for (x = x0 - 9; x <= x0 + 9; x++)
		{
			/* Mark it */
			cave_info[y][x] |= (CAVE_ICKY);
		}
	}


	/*final preps if this is a quest level*/
	if (is_quest_level)
	{
		q_ptr->cur_num = 0;
		q_ptr->max_num = 0;

		/* Square-by-square grid search for monsters */
		for (y = y0 - 2; y <= y0 + 2; y++)
		{
			for (x = x0 - 9; x <= x0 + 9; x++)
			{
				/*Is there a monster here?*/
				if (cave_m_idx[y][x] > 0)
				{
					monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

					/*mark it as a quest monster*/
					m_ptr->mflag |= (MFLAG_QUEST);

					/*increase the max_num counter*/
					q_ptr->max_num ++;
				}
			}
		}
	}

}



/*
 * Hack -- fill in "vault" rooms
 */
static void build_vault(int y0, int x0, int ymax, int xmax, cptr data)
{
	int dx, dy, x, y;
	int ax, ay;
	bool flip_v = FALSE;
	bool flip_h = FALSE;

	byte quest_artifact_spots = 0;

	cptr t;

	/* Flip the vault (sometimes) */
	if (one_in_(2)) flip_v = TRUE;
	if (one_in_(2)) flip_h = TRUE;

	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{

		if (flip_v) ay = ymax - 1 - dy;
		else ay = dy;

		for (dx = 0; dx < xmax; dx++, t++)
		{

			if (flip_h) ax = xmax - 1 - dx;
			else ax = dx;

			/* Extract the location */
			x = x0 - (xmax / 2) + ax;
			y = y0 - (ymax / 2) + ay;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Lay down a floor */
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Part of a vault */
			cave_info[y][x] |= (CAVE_ROOM | CAVE_ICKY);

			/* Analyze the grid */
			switch (*t)
			{
				/* Granite wall (outer) */
				case '%':
				{
					cave_set_feat(y, x, FEAT_WALL_OUTER);
					break;
				}

				/* Granite wall (inner) */
				case '#':
				{
					cave_set_feat(y, x, FEAT_WALL_INNER);
					break;
				}

				/* Permanent wall (inner) */
				case 'X':
				{
					cave_set_feat(y, x, FEAT_PERM_INNER);
					break;
				}

				/* Treasure/trap */
				case '*':
				{
					if (rand_int(100) < 75)
					{
						place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
					}
					else
					{
						place_trap(y, x);
					}
					break;
				}

				/* Secret doors */
				case '+':
				{
					place_secret_door(y, x);
					break;
				}

				/* Trap */
				case '^':
				{
					place_trap(y, x);
					break;
				}
			}
		}
	}

	/*Count the 'Q's for a quest vault*/
	for (t = data; t - data < ymax * xmax; t++)
	{

		/* Hack -- count the quest spots */
		if (*t == 'Q') quest_artifact_spots++;

	}

	/* Place dungeon monsters and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		if (flip_v) ay = ymax - 1 - dy;
		else ay = dy;

		for (dx = 0; dx < xmax; dx++, t++)
		{

			if (flip_h) ax = xmax - 1 - dx;
			else ax = dx;

			/* Extract the grid */
			x = x0 - (xmax/2) + ax;
			y = y0 - (ymax/2) + ay;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Analyze the symbol */
			switch (*t)
			{
				/* Monster */
				case '&':
				{
					monster_level = p_ptr->depth + 4;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster */
				case '@':
				{
					monster_level = p_ptr->depth + 8;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster, plus treasure */
				case '9':
				{
					monster_level = p_ptr->depth + 7;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_UNTHEMED);
					object_level = p_ptr->depth;
					break;
				}

				/* Nasty monster and treasure */
				case '8':
				{
					monster_level = p_ptr->depth + 20;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					object_level = p_ptr->depth + 15;
					place_object(y, x, TRUE, TRUE, DROP_TYPE_UNTHEMED);
					object_level = p_ptr->depth;
					break;
				}

				/* Nasty monster and a chest */
				case '~':
				{
					monster_level = p_ptr->depth + 20;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					object_level = p_ptr->depth + 15;
					place_object(y, x, FALSE, FALSE, DROP_TYPE_CHEST);
					object_level = p_ptr->depth;
					break;
				}

				/* Quest chest */
				case 'Q':
				{
					monster_level = p_ptr->depth + 10;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;

					/*randomly pick from several quest artifacts spots to place the artifact*/
					if ((quest_artifact_spots > 0) && (one_in_(quest_artifact_spots)))
					{
						place_quest_artifact(y, x);

						/*don't place it again*/
						quest_artifact_spots = 0;
					}
					else
					{
						/*place a decent sized object*/
						object_level = p_ptr->depth + 7;
						place_object(y, x, TRUE, FALSE, DROP_TYPE_UNTHEMED);
						object_level = p_ptr->depth;

						/*This quest artifact spot is no longer an option*/
						quest_artifact_spots --;
					}

					break;
				}

				/* Monster and/or object */
				case ',':
				{
					if (rand_int(100) < 50)
					{
						monster_level = p_ptr->depth + 3;
						place_monster(y, x, TRUE, TRUE);
						monster_level = p_ptr->depth;
					}
					if (rand_int(100) < 50)
					{
						object_level = p_ptr->depth + 5;
						place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
						object_level = p_ptr->depth;
					}
					break;
				}
			}

			/*
			 * Make the monsters carry the objects instead of just stand on them.
			 */
			if ((cave_m_idx[y][x] > 0) && (cave_o_idx[y][x] > 0))
			{

				/* Get the object */
				object_type *o_ptr = &o_list[cave_o_idx[y][x]];

				/*but don't do gold*/
				if (o_ptr->tval != TV_GOLD)
				{
					/*Don't let the player see what the object it*/
					o_ptr->ident |= (IDENT_HIDE_CARRY);

					(void)monster_carry(cave_m_idx[y][x], o_ptr);

					/*remove the item from the floor*/
					floor_item_increase(cave_o_idx[y][x], -1);
					floor_item_optimize(cave_o_idx[y][x]);
				}
			}
		}
	}
}

/*
 * Type 7 -- simple vaults (see "vault.txt")
 */
static void build_type7(int y0, int x0)
{
	vault_type *v_ptr;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* Accept the first lesser vault */
		if (v_ptr->typ == 7) break;
	}

	/* Message */
	if (cheat_room) msg_format("Lesser vault (%s)", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
}


/*
 * Returns TRUE if all grids in a given rectangle have the given feature.
 * If "filled" is FALSE, only the perimeter of the rectantle is analyzed.
 */
static bool rectangle_has_feature(int y1, int x1, int y2, int x2,
    bool filled, int feat)
{
  	int y, x;

  	/* Check bounds */
  	if ((y1 > y2) || (x1 > x2)) return (FALSE);

  	for (y = y1; y <= y2; y++)
  	{
    	for (x = x1; x <= x2; x++)
    	{
      		if (filled || (x == x1) || (x == x2) || (y == y1) || (y == y2))
      		{
				/* We found another feature */
				if (cave_feat[y][x] != feat) return (FALSE);
      		}
    	}
  	}

  	return (TRUE);
}

/*
 * Returns TRUE if the grid is inside the inner room of greater vaults made of
 * permanent walls.
 */
static bool inside_inner_room(int y, int x)
{
  	int delta, dir, num_stop;
  	bool stop[8];

  	/*
   	 * We analyze the adyacent grids in all directions, but closer grids first.
   	 * So we need to know when to stop looking at a direction.
   	 */
  	for (dir = 0; dir < 8; dir++)
  	{
    	stop[dir] = FALSE;
  	}

  	delta = num_stop = 0;

  	/* Keep going until all trayectories are stopped */
  	while (num_stop < 8)
  	{
    	/* We analyze all adyacent grids at 1 "unit" of distance, then 2, etc. */
    	++delta;

    	/* Scan all directions */
    	for (dir = 0; (dir < 8); dir++)
    	{
      		int dy, dx;

      		/* Stopped? */
      		if (stop[dir]) continue;

      		/* Get the grid */
      		dy = y + delta * ddy_ddd[dir];
      		dx = x + delta * ddx_ddd[dir];

      		/* We can reach the border of the vault */
     		if (cave_feat[dy][dx] == FEAT_WALL_OUTER) return (FALSE);

      		/* We can reach a floor grid outside the inner room */
      		if ((cave_feat[dy][dx] == FEAT_FLOOR) &&
	  			((cave_info[dy][dx] & (CAVE_G_VAULT)) == 0)) return (FALSE);

      		/* We need to stop on non-floor grids */
      		if (cave_feat[dy][dx] != FEAT_FLOOR)
      		{
				stop[dir] = TRUE;
				++num_stop;
      		}
    	}
  	}

  return (TRUE);
}

/*
 * Mark greater vault grids with the CAVE_G_VAULT flag.
 * Returns TRUE if it succeds.
 */
static bool mark_g_vault(int y0, int x0, int ymax, int xmax)
{
  	int y1, x1, y2, x2, y, x;

  	/* Get the coordinates */
  	y1 = y0 - ymax / 2;
  	x1 = x0 - xmax / 2;
  	y2 = y1 + ymax - 1;
  	x2 = x1 + xmax - 1;

  	/* We *need* a perimeter of "outer" walls */
  	if (!rectangle_has_feature(y1, x1, y2, x2, FALSE, FEAT_WALL_OUTER))
  	{
    	return (FALSE);
  	}

  	/* Step 1 - Mark all grids inside that perimeter with the new flag */
  	for (y = y1 + 1; y < y2; y++)
  	{
    	for (x = x1 + 1; x < x2; x++)
    	{
      		cave_info[y][x] |= (CAVE_G_VAULT);
    	}
  	}

	/* Step 2 - Attempt to remove the CAVE_G_VAULT flag of grids outside
	 * permanent walls. Several ideas were tried, and this one works fine with
	 * almost all grids in all vaults. Note that the entry points of vaults are
	 * unmarked, even if they are granite walls, doors or traps. -DG-
	 * Jeff, if you do not like it, remove this loop and the function
	 * "inside_inner_room".
	 */
	for (y = y1 + 1; y < y2; y++)
 	{
    	for (x = x1 + 1; x < x2; x++)
    	{
      		/* Ignore permanent walls */
      		if (cave_feat[y][x] == FEAT_PERM_INNER) continue;

      		/* Process open floor, granite walls, doors and traps */
      		if (!inside_inner_room(y, x)) cave_info[y][x] &= ~(CAVE_G_VAULT);
    	}
  	}

  	return (TRUE);
}


/*
 * Type 8 -- greater vaults (see "vault.txt")
 */
static void build_type8(int y0, int x0)
{
	vault_type *v_ptr;

	/* Pick a greater vault */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* Accept the first greater vault */
		if (v_ptr->typ == 8) break;
	}

	/* Message */
	if (cheat_room) msg_format("Greater vault (%s)", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);

	/* Hack -- Mark vault grids with the CAVE_G_VAULT flag */
	if (mark_g_vault(y0, x0, v_ptr->hgt, v_ptr->wid))
	{
		my_strcpy(g_vault_name, v_name + v_ptr->name, sizeof(g_vault_name));
	}

}

/*
 * Type 9 -- quest vaults (see "vault.txt")
 */
static void build_type9(int y0, int x0)
{
	vault_type *v_ptr;

	/* Pick a quest vault */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* Accept the first quest vault */
		if (v_ptr->typ == 9) break;
	}

	/* Message */
	if (cheat_room) msg_format("Greater vault (%s)", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
}


static bool feat_needs_open_floor(int feat)
{
	if ((feat == FEAT_INVIS) ||
 		(feat == FEAT_GLYPH) ||
      	(feat == FEAT_OPEN) ||
      	(feat == FEAT_BROKEN) ||
      	(feat == FEAT_SECRET) ||
      	(feat == FEAT_RUBBLE))
    	return (TRUE);

	/* Stairs */
	if (feat >= FEAT_STAIR_HEAD && feat <= FEAT_STAIR_TAIL)
		return (TRUE);

	/* Doors */
	if (feat >= FEAT_DOOR_HEAD && feat <= FEAT_DOOR_TAIL)
		return (TRUE);

	return (FALSE);
}

/*
 * Make a starburst room. -LM-
 *
 * Starburst rooms are made in three steps:
 * 1: Choose a room size-dependant number of arcs.  Large rooms need to
 *    look less granular and alter their shape more often, so they need
 *    more arcs.
 * 2: For each of the arcs, calculate the portion of the full circle it
 *    includes, and its maximum effect range (how far in that direction
 *    we can change features in).  This depends on room size, shape, and
 *    the maximum effect range of the previous arc.
 * 3: Use the table "get_angle_to_grid" to supply angles to each grid in
 *    the room.  If the distance to that grid is not greater than the
 *    maximum effect range that applies at that angle, change the feature
 *    if appropriate (this depends on feature type).
 *
 * Usage notes:
 * - This function uses a table that cannot handle distances larger than
 *   20, so it calculates a distance conversion factor for larger rooms.
 * - This function is not good at handling rooms much longer along one axis
 *   than the other.
 * - It is safe to call this function on areas that might contain vaults or
 *   pits, because "icky" and occupied grids are left untouched.
 *
 * - Mixing these rooms (using normal floor) with rectangular ones on a
 *   regular basis produces a somewhat chaotic looking dungeon.  However,
 *   this code does works well for lakes, etc.
 *
 */
static bool generate_starburst_room(int y1, int x1, int y2, int x2,
    bool light, int feat, bool special_ok)
{
	int y0, x0, y, x, ny, nx;
	int i, d;
	int size;
	int dist, max_dist, dist_conv, dist_check;
	int height, width, arc_dist;
	int degree_first, center_of_arc, degree;

	/* Special variant room.  Discovered by accident. */
	bool make_cloverleaf = FALSE;

	/* Holds first degree of arc, maximum effect distance in arc. */
	int arc[45][2];

	/* Number (max 45) of arcs. */
	int arc_num;


	/* Make certain the room does not cross the dungeon edge. */
	if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);

	/* Robustness -- test sanity of input coordinates. */
	if ((y1 + 2 >= y2) || (x1 + 2 >= x2)) return (FALSE);


	/* Get room height and width. */
	height = 1 + y2 - y1;
	width  = 1 + x2 - x1;

	/* Note the "size" */
	size = 2 + div_round(width + height, 22);


	/* Get a shrinkage ratio for large rooms, as table is limited. */
	if ((width > 40) || (height > 40))
	{
    	if (width > height) dist_conv = 1 + (10 * width  / 40);
    	else                dist_conv = 1 + (10 * height / 40);
  	}
	else dist_conv = 10;

	/* Make a cloverleaf room sometimes.  (discovered by accident) */
	if ((special_ok) && (height > 10) && (one_in_(20)))
	{
    	arc_num = 12;
    	make_cloverleaf = TRUE;
	}

	/* Usually, we make a normal starburst. */
	else
	{
		/* Ask for a reasonable number of arcs. */
		arc_num = 8 + (height * width / 80);
		arc_num = rand_spread(arc_num, 3);
		if (arc_num < 8) arc_num = 8;
		if (arc_num > 45) arc_num = 45;
	}


	/* Get the center of the starburst. */
	y0 = y1 + height / 2;
	x0 = x1 + width  / 2;

	/* Start out at zero degrees. */
	degree_first = 0;


	/* Determine the start degrees and expansion distance for each arc. */
	for (i = 0; i < arc_num; i++)
	{
		/* Get the first degree for this arc (using 180-degree circles). */
    	arc[i][0] = degree_first;

		/* Get a slightly randomized start degree for the next arc. */
		degree_first += div_round(180, arc_num);

		/* Do not entirely leave the usual range */
		if (degree_first < 180 * (i+1) / arc_num)
		{
      		degree_first = 180 * (i+1) / arc_num;
		}
		if (degree_first > (180 + arc_num) * (i+1) / arc_num)
		{
			degree_first = (180 + arc_num) * (i+1) / arc_num;
		}

		/* Get the center of the arc (convert from 180 to 360 circle). */
		center_of_arc = degree_first + arc[i][0];

		/* Get arc distance from the horizontal (0 and 180 degrees) */
		if      (center_of_arc <=  90) arc_dist = center_of_arc;
		else if (center_of_arc >= 270) arc_dist = ABS(center_of_arc - 360);
		else                           arc_dist = ABS(center_of_arc - 180);

		/* Special case -- Handle cloverleafs */
		if ((arc_dist == 45) && (make_cloverleaf)) dist = 0;

		/*
		 * Usual case -- Calculate distance to expand outwards.  Pay more
 		 * attention to width near the horizontal, more attention to height
		 * near the vertical.
		 */
		else dist = ((height * arc_dist) + (width * (90 - arc_dist))) / 90;

		/* Randomize distance (should never be greater than radius) */
		arc[i][1] = rand_range(dist / 4, dist / 2);

		/* Keep variability under control (except in special cases). */
		if ((dist != 0) && (i != 0))
		{
			int diff = arc[i][1] - arc[i-1][1];

			if (ABS(diff) > size)
			{
				if (diff > 0)	arc[i][1] = arc[i-1][1] + size;
				else			arc[i][1] = arc[i-1][1] - size;
      		}
    	}
	}

	/* Neaten up final arc of circle by comparing it to the first. */
	if (TRUE)
	{
		int diff = arc[arc_num - 1][1] - arc[0][1];

		if (ABS(diff) > size)
		{
			if (diff > 0)	arc[arc_num - 1][1] = arc[0][1] + size;
			else			arc[arc_num - 1][1] = arc[0][1] - size;
		}
	}


	/* Precalculate check distance. */
	dist_check = 21 * dist_conv / 10;

	/* Change grids between (and not including) the edges. */
	for (y = y1 + 1; y < y2; y++)
	{
		for (x = x1 + 1; x < x2; x++)
		{
			/* Do not touch "icky" grids. */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* Do not touch occupied grids. */
			if (cave_m_idx[y][x] != 0) continue;
			if (cave_o_idx[y][x] != 0) continue;


			/* Get distance to grid. */
			dist = distance(y0, x0, y, x);

			/* Look at the grid if within check distance. */
			if (dist < dist_check)
			{
				/* Convert and reorient grid for table access. */
				ny = 20 + 10 * (y - y0) / dist_conv;
				nx = 20 + 10 * (x - x0) / dist_conv;

				/* Illegal table access is bad. */
				if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40))  continue;

				/* Get angle to current grid. */
				degree = get_angle_to_grid[ny][nx];

				/* Scan arcs to find the one that applies here. */
				for (i = arc_num - 1; i >= 0; i--)
				{
					if (arc[i][0] <= degree)
					{
						max_dist = arc[i][1];

						/* Must be within effect range. */
						if (max_dist >= dist)
						{
							/* Check for some features */
							if (feat_needs_open_floor(feat))
							{
								if (cave_feat[y][x] == FEAT_FLOOR)
								{
									cave_set_feat(y, x, feat);

		  							/* If light is 0 the illumination is taken from the floor */
		  							/* Is interesting when light is not 0 because you can
		   							 * create an illuminated starburst "room" inside another
		   							 * completely dark */
		  							if (light) cave_info[y][x] |= (CAVE_GLOW);
								}
							}
	      					else
	      					{
								/* Otherwise, always put feat */
								cave_set_feat(y, x, feat);

								if (feat != FEAT_WALL_EXTRA)	cave_info[y][x] |= (CAVE_ROOM);
								else					cave_info[y][x] &= ~(CAVE_ROOM);

								/* Illuminate */
								if (light)  cave_info[y][x] |= (CAVE_GLOW);
								else		cave_info[y][x] &= ~(CAVE_GLOW);
	      					}
	    				}

	    				/* Arc found.  End search */
	    				break;
					}
				}
			}
		}
	}

	/*
	 * If we placed floors or dungeon granite, all dungeon granite next
	 * to floors needs to become outer wall.
	 */
	if ((feat == FEAT_FLOOR) || (feat == FEAT_WALL_EXTRA))
	{
    	for (y = y1 + 1; y < y2; y++)
		{
			for (x = x1 + 1; x < x2; x++)
			{
				/* Floor grids only */
				if (cave_feat[y][x] == FEAT_FLOOR)
				{
	  				/* Look in all directions. */
	  				for (d = 0; d < 8; d++)
	  				{
	    				/* Extract adjacent location */
	    				int yy = y + ddy_ddd[d];
	    				int xx = x + ddx_ddd[d];

	    				/* Join to room */
	    				cave_info[yy][xx] |= (CAVE_ROOM);

	    				/* Illuminate if requested. */
	    				if (light) cave_info[yy][xx] |= (CAVE_GLOW);

	    				/* Look for dungeon granite. */
	    				if (cave_feat[yy][xx] == FEAT_WALL_EXTRA)
	    				{
	      					/* Turn into outer wall. */
	      					cave_set_feat(yy, xx, FEAT_WALL_OUTER);
	    				}
	  				}
				}
      		}
    	}
  	}

	/* Success */
	return (TRUE);
}


static void build_type_starburst(int y0, int x0, bool giant_room)
{
	int light = FALSE;

	int dy, dx;
	/*
	 * Hack - get the size of the room, could be large or very large.
	 */
	 /*66x44*/
	if (giant_room)
	{
		dy = 19;
		dx = 30;
	}
	/*33x22*/
	else
	{
		dy = 10;
		dx = 14;
	}

	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;

	if (one_in_(2))
    	generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
								 	light, FEAT_FLOOR, TRUE);
  	else
  	{
    	/* Test: create an inner starburst room (magma) */
    	/* Even glyphs can be used ;) */

    	/* Note special_ok == FALSE, no cloverleaf room */
    	generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
									light, FEAT_FLOOR, FALSE);

    	/* Beware of trapping the player at the first levels */
    	dy /= 3;
    	dx /= 3;
    	generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
									light, FEAT_WALL_INNER, TRUE);
	}
}



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
 * Useful "feat" values:
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
	int i, y, x;
	int tmp_row, tmp_col;
	int row_dir, col_dir;
	int start_row, start_col;
	int main_loop_count = 0;

	bool door_flag = FALSE;

	/* Reset the arrays */
	dun->tunn_n = 0;
	dun->wall_n = 0;

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
		if (rand_int(100) < DUN_TUN_CHG)
		{
			/* Get the correct direction */
			correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Random direction */
			if (rand_int(100) < DUN_TUN_RND)
			{
				rand_dir(&row_dir, &col_dir);
			}
		}

		/* Get the next location */
		tmp_row = row1 + row_dir;
		tmp_col = col1 + col_dir;

		/* Do not leave the dungeon!!! XXX XXX */
		while (!in_bounds_fully(tmp_row, tmp_col))
		{
			/* Get the correct direction */
			correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Random direction */
			if (rand_int(100) < DUN_TUN_RND)
			{
				rand_dir(&row_dir, &col_dir);
			}

			/* Get the next location */
			tmp_row = row1 + row_dir;
			tmp_col = col1 + col_dir;
		}

		/* Avoid the edge of the dungeon */
		if (cave_feat[tmp_row][tmp_col] == FEAT_PERM_SOLID) continue;

		/* Avoid the edge of vaults */
		if (cave_feat[tmp_row][tmp_col] == FEAT_PERM_OUTER) continue;

		/* Avoid "solid" granite walls */
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_SOLID) continue;

		/* Pierce "outer" walls of rooms */
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_OUTER)
		{
			/* Get the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Hack -- Avoid outer/solid permanent walls */
			if (cave_feat[y][x] == FEAT_PERM_SOLID) continue;
			if (cave_feat[y][x] == FEAT_PERM_OUTER) continue;

			/* Hack -- Avoid outer/solid granite walls */
			if (cave_feat[y][x] == FEAT_WALL_OUTER) continue;
			if (cave_feat[y][x] == FEAT_WALL_SOLID) continue;

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
					if (cave_feat[y][x] == FEAT_WALL_OUTER)
					{
						/* Change the wall to a "solid" wall */
						cave_set_feat(y, x, FEAT_WALL_SOLID);
					}
				}
			}
		}

		/* Travel quickly through rooms */
		else if (cave_info[tmp_row][tmp_col] & (CAVE_ROOM))
		{
			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;
		}

		/* Tunnel through all other walls */
		else if (cave_feat[tmp_row][tmp_col] >= FEAT_WALL_EXTRA)
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
			if (rand_int(100) >= DUN_TUN_CON)
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


	/* Turn the tunnel into corridor */
	for (i = 0; i < dun->tunn_n; i++)
	{
		/* Get the grid */
		y = dun->tunn[i].y;
		x = dun->tunn[i].x;

		/* Clear previous contents, add a floor */
		cave_set_feat(y, x, FEAT_FLOOR);
	}


	/* Apply the piercings that we found */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Get the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* Convert to floor grid */
		cave_set_feat(y, x, FEAT_FLOOR);

		/* Occasional doorway */
		if (rand_int(100) < DUN_TUN_PEN)
 		{
		  	if ((cave_feat[y-1][x] >= FEAT_MAGMA) &&
			    (cave_feat[y+1][x] >= FEAT_MAGMA))
			{
 				/* Place a random door */
 				place_random_door(y, x);
 			}
			else if ((cave_feat[y][x-1] >= FEAT_MAGMA) &&
			    (cave_feat[y][x+1] >= FEAT_MAGMA))
			{
			  /* Place a random door */
			  place_random_door(y, x);
			}
		}
	}
}


/*
 * Place the player outside all rooms and corridors. Build a tunnel to the
 * closest room.
 */
static bool new_player_safe_spot(void)
{
  	int i;
  	int y_location_tables[40];
  	int x_location_tables[40];
  	int rand_spot;
  	int x, y;
  	int close_room = 0, room_dist = 1000;
  	int try = 0;

  	/* Find unused blocks */
  	for (i = 0, y = 0; y < dun->row_rooms; y++)
  	{
    	for (x = 0; x < dun->col_rooms; x++)
    	{
      		/* Ignore used blocks */
      		if (dun->room_map[y][x]) continue;

      		/* We have space in the arrays */
      		if (i < 40)
      		{
				y_location_tables[i] = y;
				x_location_tables[i] = x;
				++i;
      		}
      		/* Put it in a random place */
      		else
      		{
				rand_spot = rand_int(i);
				y_location_tables[rand_spot] = y;
				x_location_tables[rand_spot] = x;
      		}
    	}
  	}

  	/* Paranoia */
  	if (i == 0)
  	{
    	if (cheat_room) msg_print("No unused block.");

    	return (FALSE);
  	}

  	/* Find a safe spot */
  	while (TRUE)
  	{
    	int d;

    	/* Hack - Avoid infinite loops */
    	if (++try >= 1000)
    	{
      		if (cheat_room) msg_print("No empty spot.");

      		return (FALSE);
    	}

    	/* Pick a random block */
    	rand_spot = rand_int(i);

    	/* Pick a random spot inside that block */
    	y = y_location_tables[rand_spot] * BLOCK_HGT + rand_int(BLOCK_HGT);
    	x = x_location_tables[rand_spot] * BLOCK_WID + rand_int(BLOCK_WID);

    	/* The spot must be a wall. Note that we do not need to call
     	 * in_bounds_fully
		 */
    	if (cave_feat[y][x] != FEAT_WALL_EXTRA) continue;

    	/* The spot must be surrounded by walls */
    	for (d = 0; d < 8; d++)
    	{
      		int dy = y + ddy_ddd[d];
      		int dx = x + ddx_ddd[d];

      		if ((cave_feat[dy][dx] != FEAT_WALL_EXTRA) &&
	  			(cave_feat[dy][dx] != FEAT_PERM_SOLID)) break;
    	}

    	/* We found a non-wall grid */
    	if (d < 8) continue;

    	/* We found the safe spot */
    	break;
  	}

  	/* Find the closest room */
  	for (i = 0; i < dun->cent_n; i++)
  	{
    	int dist = distance(y, x, dun->cent[i].y, dun->cent[i].x);

    	/* Ignore dangerous rooms */
    	if (cave_info[dun->cent[i].y][dun->cent[i].x] & (CAVE_ICKY)) continue;

    	/* We found a closer room */
    	if (dist < room_dist)
    	{
      		room_dist = dist;
      		close_room = i;
    	}
  	}

  	/* Connect the safe spot to the closest room */
  	build_tunnel(y, x, dun->cent[close_room].y, dun->cent[close_room].x);

  	/* build_tunnel goes crazy sometimes */
  	if (next_to_walls(y, x) < 3) return (FALSE);

  	/* Hack - build tunnel ignores the safe spot */
  	cave_set_feat(y, x, FEAT_FLOOR);

  	/* Actually place the player */
 	return player_place(y, x);
}


/*
 * Places some staircases near walls
 */
static bool alloc_stairs(int feat, int num)
{
	int x;

	/*smaller levels don't need that many stairs, but there are a minimum of 4 rooms*/
	if (dun->cent_n == 4) num = 1;
	else if (num > (dun->cent_n / 2)) num = dun->cent_n / 2;

	/* Place "num" stairs */
	for (x = 0; x < num; x++)
	{
		int i = 0;

		int yy, xx;

		int x_location_tables [40];
		int y_location_tables [40];
		int rand_spot;

		/*  */
		for (yy = 0; yy < p_ptr->cur_map_hgt; yy++)
		{
			for (xx = 0; xx < p_ptr->cur_map_wid; xx++)
			{

				/* Must be a "naked" floor grid */
				if (!cave_naked_bold(yy, xx)) continue;

				/*we like to be next to walls*/
				if ((next_to_walls(yy,xx) < 3) && (!cave_stair_bold(yy, xx))) continue;

				/*don't go over size of array*/
				if (i < 40)
				{
					x_location_tables[i] = xx;
					y_location_tables[i] = yy;

					/*increase the counter*/
					i++;
				}

				/*put it in a random slot*/
				else
				{
					rand_spot = rand_int(40);

					x_location_tables[rand_spot] = xx;
					y_location_tables[rand_spot] = yy;
				}

			}
		}


		/*paranoia*/
		if (i == 0) return (FALSE);

		/*select a location*/
		rand_spot = rand_int (i);

		/*get the coordinates*/
		yy = y_location_tables[rand_spot];
		xx = x_location_tables[rand_spot];

		/* Town -- must go down */
		if (!p_ptr->depth)
		{
			/* Clear previous contents, add down stairs */
			cave_set_feat(yy, xx, FEAT_MORE);
		}

		/* Quest -- must go up */
		else if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
				 (quest_check(p_ptr->depth) == QUEST_FIXED_U) ||
				 (p_ptr->depth >= MAX_DEPTH-1))
		{
			/* Clear previous contents, add up stairs */
			if (x != 0) cave_set_feat(yy, xx, FEAT_LESS);
			else cave_set_feat(yy, xx, pick_up_stairs());
		}

		/* Requested type */
		else
		{
			/* Allow shafts, but guarantee the first one is an ordinary stair */
			if (x != 0)
			{
				if      (feat == FEAT_LESS) feat = pick_up_stairs();
				else if (feat == FEAT_MORE) feat = pick_down_stairs();
			}

			/* Clear previous contents, add stairs */
			cave_set_feat(yy, xx, feat);
		}
	}

	return (TRUE);
}

/*
 * Count the number of "corridor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y1, x1)"
 *
 * This routine currently only counts actual "empty floor" grids
 * which are not in rooms.  We might want to also count stairs,
 * open doors, closed doors, etc.  XXX XXX
 */
static int next_to_corr(int y1, int x1)
{
	int i, y, x, k = 0;

	/* Scan adjacent grids */
	for (i = 0; i < 4; i++)
	{
		/* Extract the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Skip non floors */
		if (!cave_floor_bold(y, x)) continue;

		/* Skip non "empty floor" grids */
		if (cave_feat[y][x] != FEAT_FLOOR) continue;

		/* Skip grids inside rooms */
		if (cave_info[y][x] & (CAVE_ROOM)) continue;

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
 * Assumes "in_bounds_fully(y,x)"
 */
static bool possible_doorway(int y, int x)
{
	/* Count the adjacent corridors */
	if (next_to_corr(y, x) >= 2)
	{
		/* Check Vertical */
		if ((cave_feat[y-1][x] >= FEAT_MAGMA) &&
		    (cave_feat[y+1][x] >= FEAT_MAGMA))
		{
			return (TRUE);
		}

		/* Check Horizontal */
		if ((cave_feat[y][x-1] >= FEAT_MAGMA) &&
		    (cave_feat[y][x+1] >= FEAT_MAGMA))
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
	if (cave_feat[y][x] >= FEAT_MAGMA) return;

	/* Ignore room grids */
	if (cave_info[y][x] & (CAVE_ROOM)) return;

	/* Occasional door (if allowed) */
	if ((rand_int(100) < DUN_TUN_JCT) && possible_doorway(y, x))
	{
		/* Place a door */
		place_random_door(y, x);
	}
}

/*
 * Hack - mark some squares in teh dungeon, that are counted when the player
 * walks over them.  This is a painful, awful hack used to occasionally allow the
 * stores sell things outside their pre-determined inventory without allowing
 * the player to townscum and pick up items in the stores.  Players need to
 * explore the dungeon in order to occasionally walk over the squares, increasing
 * the chance of stores selling other items. -JG
 */
static void place_marked_squares(void)
{
	int y, x, k, i;
	s32b j;

	/*
	 * First factor in the number of "secret squares" based on size of dungeon
	 * Start with 100 since we are dealing with int variables.
	 * This will be reduced later
	 */
	j = 0;


	/*factor in dungeon size*/
	j += ((100 * p_ptr->cur_map_hgt) / MAX_DUNGEON_HGT);
	j += ((100 * p_ptr->cur_map_wid) / MAX_DUNGEON_WID);

	/*divide by 10*/
	j /= 10;

	/* Mark some squares */
	for (k = 0; k < j; k++)
	{
		bool is_room;

		/*put half of them in corridors, half in rooms*/
		if (one_in_(2)) is_room = TRUE;
		else is_room = FALSE;

		/* Pick a "legal" spot */
		for (i = 0; i < 10000; i++)
		{

			/* Location */
			y = rand_int(p_ptr->cur_map_hgt);
			x = rand_int(p_ptr->cur_map_wid);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue;

			/*Already Marked*/
			if (cave_info[y][x] & (CAVE_MARKED)) continue;

			/* Check if it should be a room or not "room" */
			if (((cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE) != is_room)
			{
				continue;
			}

			break;
		}

		/* No point found */
		if (i == 10000) return;

		/*mark it*/
		cave_info[y][x] |= (CAVE_MARKED);
	}

}

static void basic_granite(void)
{
	int y, x;

	/* Hack -- Start with basic granite */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Create granite wall */
			cave_set_feat(y, x, FEAT_WALL_EXTRA);
		}
	}
}


static bool place_traps_rubble_player(void)
{
	int k;

	/* Basic "amount" */
	k = (p_ptr->depth / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));

	/* Determine the character location, if it is needed */
	if ((p_ptr->px + p_ptr->py == 0) && !new_player_spot())
	{
		if (cheat_room) msg_format("Couldn't spot player.");

		return(FALSE);
	}

	/*
	 * Hack - place up to 10 "marked squares" in the dungeon if
	 * player is close to max_depth
	 */
	if ((p_ptr->depth + 3) > p_ptr->max_depth) place_marked_squares();

	return (TRUE);

}

static bool scramble_and_connect_rooms_stairs(void)
{
	int i, y1, x1, y, x;

	/* Start with no tunnel doors */
	dun->door_n = 0;

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

	/* Hack -- connect the first room to the last room */
	y = dun->cent[dun->cent_n-1].y;
	x = dun->cent[dun->cent_n-1].x;

	/* Connect all the rooms together */
	for (i = 0; i < dun->cent_n; i++)
	{
		/* Connect the room to the previous room */
		build_tunnel(dun->cent[i].y, dun->cent[i].x, y, x);

		/* Remember the "previous" room */
		y = dun->cent[i].y;
		x = dun->cent[i].x;
	}

	/* Attempt to place the player in a safe spot */
	/* Note that this code must be executed before placing intersection
	 * doors */
	if ((count_unused_blocks() > 0) && !new_player_safe_spot())
	{
	  	if (cheat_room) msg_format("Couldn't spot player.");

		return (FALSE);
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

	/* Place 3 or 5 down stairs near some walls */
	if (!(alloc_stairs(FEAT_MORE, (3 + randint(2)))))
	{
		if (cheat_room) msg_format("failed to place down stairs");

		return(FALSE);
	}

	/* Place 1 or 3 up stairs near some walls */
	if (!(alloc_stairs(FEAT_LESS, (1 + randint(2)))))
	{
		if (cheat_room) msg_format("failed to place down stairs");

		return(FALSE);
	}

	/* Hack -- Add some magma streamers */
	for (i = 0; i < DUN_STR_MAG; i++)
	{
		/*if we can't build streamers, something is wrong with level*/
		if (!build_streamer(FEAT_MAGMA, DUN_STR_MC)) return (FALSE);
	}

	/* Hack -- Add some quartz streamers */
	for (i = 0; i < DUN_STR_QUA; i++)
	{
		/*if we can't build streamers, something is wrong with level*/
		if (!build_streamer(FEAT_QUARTZ, DUN_STR_QC)) return (FALSE);
	}

	return (TRUE);

}

static void set_perm_boundry(void)
{
	int y, x;

	/* Special boundary walls -- Top */
	for (x = 0; x < p_ptr->cur_map_wid; x++)
	{
		y = 0;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Bottom */
	for (x = 0; x < p_ptr->cur_map_wid; x++)
	{
		y = p_ptr->cur_map_hgt-1;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Left */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		x = 0;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Right */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		x = p_ptr->cur_map_wid-1;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}
}


/*
 * Attempt to build a room of the given type at the given block
 *
 * Note that we restrict the number of "crowded" rooms to reduce
 * the chance of overflowing the monster list during level creation.
 */
static bool room_build(int by0, int bx0, int typ)
{
	int y, x;
	int by, bx;
	int by1, bx1, by2, bx2;

	/* Restrict level */
	if (p_ptr->depth < room[typ].level) return (FALSE);

	/* Restrict "crowded" rooms */
	if (dun->crowded && ((typ == 5) || (typ == 6))) return (FALSE);

	/* Extract blocks */
	by1 = by0 + room[typ].dy1;
	bx1 = bx0 + room[typ].dx1;
	by2 = by0 + room[typ].dy2;
	bx2 = bx0 + room[typ].dx2;

	/* Never run off the screen */
	if ((by1 < 0) || (by2 >= dun->row_rooms)) return (FALSE);
	if ((bx1 < 0) || (bx2 >= dun->col_rooms)) return (FALSE);

	/* Verify open space */
	for (by = by1; by <= by2; by++)
	{
		for (bx = bx1; bx <= bx2; bx++)
		{
			if (dun->room_map[by][bx]) return (FALSE);
		}
	}

	/* It is *extremely* important that the following calculation */
	/* be *exactly* correct to prevent memory errors XXX XXX XXX */

	/* Get the location of the room */
	y = ((by1 + by2 + 1) * BLOCK_HGT) / 2;
	x = ((bx1 + bx2 + 1) * BLOCK_WID) / 2;

	/* Build a room */
	switch (typ)
	{
		/* Build an appropriate room */
		case 11:build_type_starburst(y, x, TRUE); break;
		case 10:build_type_starburst(y, x, FALSE); break;
		case 9: build_type9(y, x); break;
		case 8: build_type8(y, x); break;
		case 7: build_type7(y, x); break;
		case 6: build_type_pit(y, x); break;
		case 5: build_type_nest(y, x); break;
		case 4: build_type4(y, x); break;
		case 3: build_type3(y, x); break;
		case 2: build_type2(y, x); break;
		case 1: build_type1(y, x); break;

		/* Paranoia */
		default: return (FALSE);
	}

	/* Save the room location */
	if (dun->cent_n < CENT_MAX)
	{
		dun->cent[dun->cent_n].y = y;
		dun->cent[dun->cent_n].x = x;
		dun->cent_n++;
	}

	/* Reserve some blocks */
	for (by = by1; by <= by2; by++)
	{
		for (bx = bx1; bx <= bx2; bx++)
		{
			dun->room_map[by][bx] = TRUE;
		}
	}

	/* Count "crowded" rooms */
	if ((typ == 5) || (typ == 6)) dun->crowded = TRUE;

	/* Success */
	return (TRUE);
}


/*select a monster type for a themed level*/
byte get_level_theme(s16b orig_theme_num)
{

	s16b mindepth, theme_depth;

	s16b theme_num = orig_theme_num;

	/*enforce minimum depth, to keep weak nests out of deep levels*/
	mindepth = theme_num / 4;

	/*keep total to 100 or less*/
	if ((mindepth + theme_num) > 100) theme_num = 100 - mindepth;

	/* Hack -- Choose a nest type */
	theme_depth = randint(theme_num) + mindepth;

	if ((theme_depth <= 20) && (orig_theme_num <= 28))
	{
		/*Coins, minor demons, Orcs, or a mixture of a couple monsters*/
		if (one_in_(4))			return (LEV_THEME_ORC_NAGA_YEEK_KOBOLD);
		else if (one_in_(3))	return (LEV_THEME_DEMON_MINOR);
		else if (one_in_(2))	return (LEV_THEME_CREEPING_COIN);
		else					return (LEV_THEME_ORC);
	}

	else if ((theme_depth <= 35)  && (orig_theme_num <= 40))
	{
		/*Trolls or Ogres, or a mixture of cave dwellers*/
		if (one_in_(3))			return (LEV_THEME_CAVE_DWELLER);
		else if (one_in_(2))	return (LEV_THEME_TROLL);
		else					return (LEV_THEME_OGRE);
	}

	else if ((theme_depth <= 50) && (orig_theme_num <= 60))
	{
		/* Hounds, Hydras, demon, or young dragon*/
		if (one_in_(4))			return (LEV_THEME_DEMON_ALL);
		else if (one_in_(3))	return (LEV_THEME_HOUND);
		else if (one_in_(2))	return (LEV_THEME_HYDRA);
		else					return (LEV_THEME_DRAGON_YOUNG);
	}

	/* Giant, animal, or humanoid pit */
	else if ((theme_depth <= 60) && (orig_theme_num <= 80))
	{
		/* Giant pit */
		if (one_in_(4))			return (LEV_THEME_HUMANOID);
		else if (one_in_(3))	return (LEV_THEME_UNDEAD);
		else if (one_in_(2))	return (LEV_THEME_GIANT);
		else 					return (LEV_THEME_ANIMAL);
	}

	/* Dragon pit */
	else if (theme_depth <= 75)
	{
		/* Pick dragon type */
		switch (rand_int(6))
		{
			/* Black */
			case 0:	return (LEV_THEME_DRAGON_ACID);
			/* Blue */
			case 1:	return (LEV_THEME_DRAGON_ELEC);
			/* Red */
			case 2:	return (LEV_THEME_DRAGON_FIRE);
			/* White */
			case 3:	return (LEV_THEME_DRAGON_COLD);
			/* Green */
			case 4:	return (LEV_THEME_DRAGON_POIS);
			/* Chromatic */
			default:return (LEV_THEME_DRAGON_CHROMATIC);
		}
	}


	else
	{
		/* Ancient Dragon pit */
	    if one_in_(2)	return (LEV_THEME_DRAGON_ANCIENT);

		/* > 90 - Demon pit */
		else			return (LEV_THEME_DEMON_MAJOR);
	}
}

#define MON_RARE_FREQ	15
#define MON_LESS_FREQ	50

/*
 *Helper function for max number of creatures on a themed level.
 This function is for non-uniques only.
 */
static byte max_themed_monsters(const monster_race *r_ptr, u32b max_power)
{
	/*first off, handle uniques*/
	if (r_ptr->flags1 & RF1_UNIQUE) return (r_ptr->max_num);

	/*don't allow 99 of the out of depth monsters*/
	if (r_ptr->level > p_ptr->depth + 3)
	{
		int lev_ood = p_ptr->depth - r_ptr->level;

		/*Too strong*/
		if (r_ptr->mon_power > max_power) return (0);

		else if (lev_ood > 5) return (MON_RARE_FREQ);
		else return (MON_LESS_FREQ);
	}
	else if ((r_ptr->level < p_ptr->depth - 5) && (r_ptr->level < 75))
	{
		int lev_ood = p_ptr->depth - r_ptr->level;

		/*Too weak*/
		if (r_ptr->mon_power < max_power / 20) return (0);

		else if (r_ptr->mon_power < max_power / 10) return (MON_RARE_FREQ);
		else if (lev_ood > 5) return (MON_LESS_FREQ);
	}
	/*The rigth depth*/
	return (r_ptr->max_num);
}

/*
 * Generate a themed dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool build_themed_level(void)
{
	int i;
	int r_idx;
	monster_race *r_ptr;
	int by, bx;
	byte is_quest_level = FALSE;
	long value;
	int total = 0;
	int max_depth;
	u32b max_diff, max_diff_unique;
	u32b highest_power = 0;

	alloc_entry *table = alloc_race_table;

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	byte level_theme;

	dun_data dun_body;

	u16b monster_number, potential_monsters;

	/* Global data */
	dun = &dun_body;

	/*check if we need a quest*/
	if (quest_check(p_ptr->depth) == QUEST_THEMED_LEVEL) is_quest_level = TRUE;

	/* Always a dungeon of 5 * 10 blocks*/
	p_ptr->cur_map_hgt = MAX_DUNGEON_HGT * 5 / 6;
	p_ptr->cur_map_wid = MAX_DUNGEON_WID * 5 / 9;

	/* Hack -- Start with basic granite */
	basic_granite();

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = p_ptr->cur_map_hgt / BLOCK_HGT;
	dun->col_rooms = p_ptr->cur_map_wid / BLOCK_WID;

	/* Initialize the room table */
	for (by = 0; by < dun->row_rooms; by++)
	{
		for (bx = 0; bx < dun->col_rooms; bx++)
		{
			dun->room_map[by][bx] = FALSE;
		}
	}

	/* No rooms yet */
	dun->cent_n = 0;

	/* Build some rooms, note starburst rooms fail alot*/
	for (i = 0; i < DUN_ROOMS * 2; i++)
	{
		/* Pick a block for the room */
		by = rand_int(dun->row_rooms);
		bx = rand_int(dun->col_rooms);

		/*make most% of the rooms starburst rooms*/
		if (i < DUN_ROOMS)
		{
			/*make the first 33% of  attempts at an unusually large room*/
			bool large_room = ((dun->cent_n < 1) ? 11 : 10);

			if (!room_build(by, bx, large_room)) continue;
		}

		/* or else Attempt a trivial room */
		else if (room_build(by, bx, 1)) continue;
	}

	/*start over on all themed levels with less than 6 rooms due to inevitable crash*/
	if (dun->cent_n < 4)
	{
		if (cheat_room) msg_format("not enough rooms");
		return (FALSE);
	}

	/*set the permanent walls*/
	set_perm_boundry();

	/*make the tunnels*/
	if (!scramble_and_connect_rooms_stairs())
	{
		if (cheat_room) msg_format("unable to scramble and connect rooms");

		return (FALSE);
	}

	if (!place_traps_rubble_player()) return (FALSE);

	/*note we are going to have an extremely crowded dungeon*/
	/*there is no need for objects on the floor -JG*/

	/*select the monster theme, get the feeling*/
	if (is_quest_level) level_theme = q_ptr->theme;
	else level_theme = get_level_theme(p_ptr->depth);

	/*insert the feeling now*/
	feeling = level_theme + LEV_THEME_HEAD;

	/*get the hook*/
	get_mon_hook(level_theme);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Monsters can be up to 7 levels out of depth, 10 for a quest */
	max_depth = p_ptr->depth +
				(is_quest_level ? THEMED_LEVEL_QUEST_BOOST : THEMED_LEVEL_NO_QUEST_BOOST);

	/*don't make it too easy if the player isn't diving very fast*/
	if (p_ptr->depth < p_ptr->max_lev)
	{
		max_depth += ((p_ptr->max_lev - p_ptr->depth) / 2);
	}

	/*boundry control*/
	if (max_depth > (MAX_DEPTH - 15)) max_depth = MAX_DEPTH - 15;

	/*get the average difficulty spanning 5 levels for monsters*/
	max_diff = max_diff_unique = 0;

	/*first get the total of the 5 levels*/
	for (i = 0; i < 5; i++)
	{
		/*put some boundry control on the highest level*/
		max_diff += mon_power_ave[max_depth + i][CREATURE_NON_UNIQUE];
		max_diff_unique += mon_power_ave[max_depth + i][CREATURE_UNIQUE];
	}

	/*now get the average*/
	max_diff /= 5;
	max_diff_unique /= 5;

	/* Quest levels are a little more crowded than non-quest levels*/
	if (is_quest_level) monster_number = 275;
	else monster_number = 250;

	/* Reduce the number as monsters get more powerful*/
	monster_number -= ((p_ptr->depth / 3) + randint(p_ptr->depth / 3));

	/*boundry control*/
	if (monster_number > (z_info->m_max	- 25)) monster_number = z_info->m_max - 25;

	/*start the counter for potential monsters*/
	potential_monsters = 0;

	/*
	 * Process the probabilities, starting from the back forward
	 */
	for (i = alloc_race_size - 1; i >= 0; i--)
	{

		/* Default */
		table[i].prob3 = 0;

		/* Monster is not a part of this theme*/
		if (table[i].prob2 == 0) continue;

		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/*enforce a maximum depth*/
		if (r_ptr->level > max_depth) continue;

		/*no player ghosts*/
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

		/* Uniques only for unique quests*/
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/*get the right difficulty*/
			if (r_ptr->mon_power > max_diff_unique) continue;

			/* no dead ones*/
			if (r_ptr->cur_num >= r_ptr->max_num) continue;
		}
		/*other monsters based on difficulty*/
		else
		{
			/*get the right difficulty*/
			if (r_ptr->mon_power > max_diff) continue;
		}

		/*hack - no town monsters*/
		if (table[i].level <= 0) continue;

		/* Depth Monsters never appear in quests*/
		if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) continue;

		/* Accept the monster*/
		table[i].prob3 = table[i].prob2;

		/*limit the probability of weaker or stronger monsters*/
		if (!(r_ptr->flags1 & (RF1_UNIQUE)))
		{
			byte num;

			/*once we have enough monsters, start limiting the weaker ones*/
			if (potential_monsters > (monster_number * 3)) num = max_themed_monsters(r_ptr, max_diff);
			else num = r_ptr->max_num;

			/*reduce the probability*/
			if (num == MON_RARE_FREQ) table[i].prob3 /= 10;
			else if (num == MON_LESS_FREQ) table[i].prob3 /= 3;
			else if (!num) table[i].prob3 = 0;

			potential_monsters += num;
		}

		/*but always allow uniques*/
		else potential_monsters += 1;

		/* Total */
		total += table[i].prob3;

		/*record the most powerful monster*/
		if (r_ptr->mon_power > highest_power) highest_power = r_ptr->mon_power;
	}

	/*no eligible creatures - should never happen*/
	if (!total)
	{
		/* Remove restriction */
		get_mon_num_hook = NULL;

		/* Prepare allocation table */
		get_mon_num_prep();

    	/* No monsters - no themed level */
		return (FALSE);

	}

 	/*place the monsters in the dungeon*/
	while (mon_cnt < monster_number)
	{
		int y, x;
		bool dont_use = FALSE;

		/* Pick a monster */
		value = rand_int(total);

		/* Find the monster */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/*don't attempt to re-place duplicate unique entries*/
		if (r_ptr->cur_num >= r_ptr->max_num) dont_use = TRUE;

		/*No more of this type of monster on the level, the monster type has been restricted*/
		if ((table[i].prob3 < table[i].prob2)  &&
			(r_ptr->cur_num >= max_themed_monsters(r_ptr, max_diff)))
		{
			dont_use = TRUE;
		}

		/*not a monster*/
		if (!r_idx) dont_use = TRUE;

		if (dont_use)
		{
			/*Take out the weakest monster and repeat*/
			total -= table[i].prob3;
			table[i].prob3 = 0;

			/*we have maxed out all possible types of monsters*/
			if (total < 1) break;

			/*go back to the beginning*/
			continue;
		}

		/* Pick a location */
		while (TRUE)
		{
			int dist;

			y = rand_int(p_ptr->cur_map_hgt);
			x = rand_int(p_ptr->cur_map_wid);

			if (!cave_floor_bold(y,x)) continue;

			/*better distance*/
			dist = distance(p_ptr->py, p_ptr->px, y, x);

			/*hack - too close*/
			if (dist < 5) continue;

			/*give the player a chance - no monsters starting in LOS*/
			if (dist < MAX_RANGE)
			{
				if (los(y, x, p_ptr->py, p_ptr->px)) continue;
			}

			/*found a spot*/
			break;
		}

		/* Attempt to place the monster, allow sleeping, don't allow groups*/
		(void)place_monster_aux(y, x, r_idx, TRUE, FALSE);

		/*Don't bother with mimics*/
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

			/*Clear the mimic flag*/
			m_ptr->mimic_k_idx = 0;
			m_ptr->mflag &= ~(MFLAG_MIMIC);
		}
	}

	/*final preps if this is a quest level*/
	if (is_quest_level)
	{
		int y, x;

		q_ptr->cur_num = 0;
		q_ptr->max_num = 0;

		/* Square-by-square grid search for monsters */
		for (y = 0; y < p_ptr->cur_map_hgt; y++)
		{
			for (x = 0; x < p_ptr->cur_map_wid; x++)
			{
				/*Is there a monster here?*/
				if (cave_m_idx[y][x] > 0)
				{
					monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
					monster_race *r_ptr = &r_info[m_ptr->r_idx];

					/*mark it as a quest monster*/
					m_ptr->mflag |= (MFLAG_QUEST);

					if (!(r_ptr->flags1 & RF1_UNIQUE))
					{
						m_ptr->mflag |= (MFLAG_FASTER);
						calc_monster_speed(m_ptr->fy, m_ptr->fx);
					}

					/*increase the max_num counter*/
					q_ptr->max_num ++;

					/*Not many of them sleeping, others lightly sleeping*/
					if (one_in_(2)) m_ptr->csleep = 0;
					else m_ptr->csleep /= 2;
				}
			}
		}
	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	return (TRUE);

}


/*
 * Generate an unthemed new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(void)
{
	int i, k;

	int by, bx;

	bool destroyed = FALSE;
	bool need_quest_room = FALSE;
	bool greater_vault = FALSE;

	/*Hack - get the quest type*/
	byte quest_on_level = quest_check(p_ptr->depth);

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* Hack - variables for allocations */
	s16b mon_gen, obj_gen;

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;

	/* Reset generation variables */
	mon_gen = MIN_M_ALLOC_LEVEL;
	obj_gen = Rand_normal(DUN_AMT_ROOM, 3);

	/* Possible "destroyed" level */
	if ((p_ptr->depth > 10) && (one_in_(DUN_DEST))) destroyed = TRUE;

	/*
	 * If we have visited the quest level, and found the
	 * artifact, don't re-create the vault.
	 */
	if (quest_on_level == QUEST_VAULT)
	{
		/*We have the artifact, but we haven't gone back to the guild yet*/
		if (quest_item_slot() > -1) quest_on_level = 0;
	}

	/*see if we need a quest room*/
	if (quest_on_level)
	{
		/* Hack -- No destroyed "quest" levels */
		destroyed = FALSE;

		switch (q_ptr->type)
		{
			case QUEST_VAULT:
			case QUEST_PIT:
			case QUEST_NEST:
			{
				need_quest_room = TRUE;
				break;
			}
			default: break;
		}
	}

	if ((adult_force_small_lev) || (one_in_(SMALL_LEVEL)) ||
		(quest_on_level == QUEST_VAULT))
    {
		int l, m;

		while (TRUE)
		{
			/*note: Panel height and width is 1/6 of max height/width*/
			l = randint(MAX_DUNGEON_HGT / (PANEL_HGT));
			m = randint(MAX_DUNGEON_WID / (PANEL_WID_FIXED));

			/*make the dungeon height & width a multiple of 2 to 6 of panel hgt & width*/
			p_ptr->cur_map_hgt = MAX(2, l) * (PANEL_HGT);
			p_ptr->cur_map_wid = MAX(2, m) * (PANEL_WID_FIXED);

			/* Exit if less than normal dungeon */
			if ((p_ptr->cur_map_hgt < MAX_DUNGEON_HGT) ||
				(p_ptr->cur_map_wid < MAX_DUNGEON_WID)) break;
		}

		if ((cheat_room) && (!adult_force_small_lev))
			 msg_format("A 'small' dungeon level (%dx%d).", m, l);

		if (!adult_force_small_lev) rating += ((m + l <= 3) ? 15 : 10);
	}
	else
	{
	/* Full-sized dungeon */
		p_ptr->cur_map_hgt = MAX_DUNGEON_HGT;
		p_ptr->cur_map_wid = MAX_DUNGEON_WID;
	}

	/*start with basic granite*/
	basic_granite();

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = p_ptr->cur_map_hgt / BLOCK_HGT;
	dun->col_rooms = p_ptr->cur_map_wid / BLOCK_WID;

	/* Initialize the room table */
	for (by = 0; by < dun->row_rooms; by++)
	{
		for (bx = 0; bx < dun->col_rooms; bx++)
		{
			dun->room_map[by][bx] = FALSE;
		}
	}

	/* No "crowded" rooms yet */
	dun->crowded = FALSE;

	/* No rooms yet */
	dun->cent_n = 0;

	/* Build some rooms */
	for (i = 0; i < DUN_ROOMS; i++)
	{
		/* Pick a block for the room */
		by = rand_int(dun->row_rooms);
		bx = rand_int(dun->col_rooms);

		/* Align dungeon rooms */
		if (dungeon_align)
		{
			/* Slide some rooms right */
			if ((bx % 3) == 0) bx++;

			/* Slide some rooms left */
			if ((bx % 3) == 2) bx--;
		}

		/* Destroyed levels are boring */
		if (destroyed)
		{
			/* Attempt a "trivial" room */
			if (room_build(by, bx, 1)) continue;

			/* Never mind */
			continue;
		}
		/*first make a necessary quest room*/
		if (need_quest_room)
		{
			if (quest_on_level == QUEST_VAULT)
			{
				if (room_build(by, bx, 9)) need_quest_room = FALSE;
				continue;
			}
			else if (quest_on_level == QUEST_PIT)
			{
				if (room_build(by, bx, 6)) need_quest_room = FALSE;
				continue;
			}
			else if (quest_on_level == QUEST_NEST)
			{
				if (room_build(by, bx, 5)) need_quest_room = FALSE;
				continue;
			}
		}


		/* Attempt an "unusual" room */
		if (rand_int(DUN_UNUSUAL) < p_ptr->depth)
		{
			/* Roll for room type */
			k = rand_int(100);

			/* Attempt a very unusual room */
			if (rand_int(DUN_UNUSUAL) < p_ptr->depth)
			{
				/* Type 8 -- Greater vault (10%) */
				if ((k < 10) && !greater_vault && room_build(by, bx, 8))
				{
					greater_vault = TRUE;
					continue;
				}

				/* Type 7 -- Lesser vault (15%) */
				if ((k < 25) && room_build(by, bx, 7)) continue;

				/* Type 6 -- Monster pit (15%) */
				if ((k < 40) && room_build(by, bx, 6)) continue;

				/* Type 5 -- Monster nest (10%) */
				if ((k < 50) && room_build(by, bx, 5)) continue;
			}

			/* Type 4 -- Large room (25%) */
			if ((k < 25) && room_build(by, bx, 4)) continue;

			/* Type 3 -- Cross room (25%) */
			if ((k < 50) && room_build(by, bx, 3)) continue;

			/* Type 2 -- Overlapping (50%) */
			if ((k < 100) && room_build(by, bx, 2)) continue;
		}

		/*occasionally attempt a starburst room*/
		if (one_in_(5))
		{
			bool giant_room = (one_in_(10) ? 11 : 10);

			if (room_build(by, bx, giant_room)) continue;
		}

		/* Attempt a trivial room */
		if (room_build(by, bx, 1)) continue;
	}

	/*set the permanent walls*/
	set_perm_boundry();

	/*start over on all levels with less than two rooms due to inevitable crash*/
	if (dun->cent_n < ROOM_MIN)
	{
		if (cheat_room) msg_format("not enough rooms");
		return (FALSE);
	}

	/*make the tunnels*/
	if (!scramble_and_connect_rooms_stairs())
	{
		if (cheat_room) msg_format("couldn't connect the rooms");
		return (FALSE);
	}

	/*don't destroy really small levels*/
	if (dun->cent_n < 10) destroyed = FALSE;

	/* Destroy the level if necessary */
	if (destroyed) destroy_level();

	/*place the stairs, traps, rubble, player, secret stairs*/
	if (!place_traps_rubble_player())
	{
		if (cheat_room) msg_format("couldn't place traps, rubble, or player");
		return (FALSE);
	}

	/* To make small levels a bit more playable */
	if (p_ptr->cur_map_hgt < MAX_DUNGEON_HGT || p_ptr->cur_map_wid < MAX_DUNGEON_WID)
	{
		int small_tester = mon_gen;

		mon_gen = ((mon_gen * p_ptr->cur_map_hgt) / MAX_DUNGEON_HGT) +1;
		mon_gen = ((mon_gen * p_ptr->cur_map_wid) / MAX_DUNGEON_WID) +1;

		if (mon_gen > small_tester) mon_gen = small_tester;
		else if (cheat_hear)
		{
			msg_format("Reduced monsters base from %d to %d", small_tester, i);
		}
	}

	/*paranoia*/
	if (mon_gen < 1) mon_gen = 1;

	mon_gen += randint(8);

	/* Put some monsters in the dungeon */
	for (i = mon_gen; i > 0; i--)
	{
		(void)alloc_monster(0, TRUE);
	}

	/* Ensure quest monsters */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Quest levels */
		if (q_ptr->active_level == p_ptr->depth)
		{
			int y, x;

			if ((q_ptr->type == QUEST_MONSTER) || (q_ptr->type == QUEST_FIXED))
			{
				int j;

				monster_race *r_ptr = &r_info[q_ptr->mon_idx];
				s16b num_questors;

				/* A certain number of questors */
				num_questors = q_ptr->max_num - q_ptr->cur_num;

				/* Ensure quest monsters */
				while (r_ptr->cur_num < num_questors)
				{
					/* Pick a location */
					while (TRUE)
					{
						y = rand_int(p_ptr->cur_map_hgt);
						x = rand_int(p_ptr->cur_map_wid);

						if (cave_naked_bold(y, x)) break;
					}


					/* Place the questor */
					place_monster_aux(y, x, q_ptr->mon_idx, TRUE, TRUE);
				}

				/* Process the monsters (backwards) */
				for (j = mon_max - 1; j >= 1; j--)
				{
					/* Access the monster */
					monster_type *m_ptr = &mon_list[j];

					/*mark it as a quest monster if applicable*/
					if 	(q_ptr->mon_idx == m_ptr->r_idx) m_ptr->mflag |= (MFLAG_QUEST);
				}
			}

			else if ((q_ptr->type == QUEST_UNIQUE) || (q_ptr->type == QUEST_FIXED_U))
			{

				/* Pick a location */
				while (TRUE)
				{
					y = rand_int(p_ptr->cur_map_hgt);
					x = rand_int(p_ptr->cur_map_wid);

					if (cave_naked_bold(y, x)) break;
				}

				/* Place the questor */
				place_monster_aux(y, x, q_ptr->mon_idx, TRUE, TRUE);

				/*Is there a monster here?*/
				if (cave_m_idx[y][x] > 0)
				{
					monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

					/*mark it as a quest monster*/
					m_ptr->mflag |= (MFLAG_QUEST);
				}
			}
		}
	}

	/* Put some objects in rooms */
	if (obj_gen > 0) alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, obj_gen);

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal(DUN_AMT_GOLD, 3));

	return(TRUE);

}



/*
 * Builds a store at a given pseudo-location
 *
 * As of 2.7.4 (?) the stores are placed in a more "user friendly"
 * configuration, such that the four "center" buildings always
 * have at least four grids between them, to allow easy running,
 * and the store doors tend to face the middle of town.
 *
 * The stores now lie inside boxes from 3-9 and 12-18 vertically,
 * and from 7-17, 21-31, 35-45, 49-59.  Note that there are thus
 * always at least 2 open grids between any disconnected walls.
 *
 * Note the use of "town_illuminate()" to handle all "illumination"
 * and "memorization" issues.
 */
static void build_store(int n, int yy, int xx)
{
	int y, x, y0, x0, y1, x1, y2, x2, tmp;


	/* Find the "center" of the store */
	y0 = yy * 9 + 6;
	x0 = xx * 11 + 11;

	/* Determine the store boundaries */
	y1 = y0 - randint((yy == 0) ? 2 : 1) - 1;
	y2 = y0 + randint((yy == 1) ? 2 : 1) + 1;
	x1 = x0 - randint(2) - 1;
	x2 = x0 + randint(2) + 1;

	/* Build an invulnerable rectangular building */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* Create the building */
			cave_set_feat(y, x, FEAT_PERM_EXTRA);
		}
	}

	/* Pick a door direction (S,N,E,W) */
	tmp = rand_int(4);

	/* Re-roll "annoying" doors */
	if (((tmp == 0) && (yy == 1)) ||
	    ((tmp == 1) && (yy == 0)) ||
	    ((tmp == 2) && (xx == 3)) ||
	    ((tmp == 3) && (xx == 0)))
	{
		/* Pick a new direction */
		tmp = rand_int(4);
	}

	/* Extract a "door location" */
	switch (tmp)
	{
		/* Bottom side */
		case 0:
		{
			y = y2;
			x = rand_range(x1, x2);
			break;
		}

		/* Top side */
		case 1:
		{
			y = y1;
			x = rand_range(x1, x2);
			break;
		}

		/* Right side */
		case 2:
		{
			y = rand_range(y1, y2);
			x = x2;
			break;
		}

		/* Left side */
		default:
		{
			y = rand_range(y1, y2);
			x = x1;
			break;
		}
	}

	/* Clear previous contents, add a store door */
	cave_set_feat(y, x, FEAT_SHOP_HEAD + n);
}




/*
 * Generate the "consistent" town features, and place the player
 *
 * Hack -- play with the R.N.G. to always yield the same town
 * layout, including the size and shape of the buildings, the
 * locations of the doorways, and the location of the stairs.
 */
static void town_gen_hack(void)
{
	int y, x, k, n, a, b;

	int rooms[MAX_STORES];


	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = seed_town;

	/*pick the one space to have the stairs*/
	a = rand_int(2);
	b = rand_int(5);

	/* Prepare an Array of "remaining stores", and count them */
	for (n = 0; n < MAX_STORES; n++) rooms[n] = n;

	/* Place two rows of stores */
	for (y = 0; y < 2; y++)
	{
		/* Place five stores per row, but leave one space for stairs */
		for (x = 0; x < 5; x++)
		{
			/*sincean odd number of stores, skip one and make stairs there*/
			if ((y == a) && (x ==b)) continue;

			/* Pick a random unplaced store */
			k = ((n <= 1) ? 0 : rand_int(n));

			/* Build that store at the proper location */
			build_store(rooms[k], y, x);

			/* Shift the stores down, remove one store */
			rooms[k] = rooms[--n];

		}
	}


	/* Place the stairs */
	while (TRUE)
	{
		/* Find the "center" of the empty store space, in the empty gap */
		y = a * 6 + 6 + rand_int (2);
		x = b * 11 + 11 + rand_int (2);

		/* Require a "naked" floor grid */
		if (cave_naked_bold(y, x)) break;
	}

	/* Clear previous contents, add down stairs */
	cave_set_feat(y, x, FEAT_MORE);

	/* Place the player */
	player_place(y, x);

	/* Hack -- use the "complex" RNG */
	Rand_quick = FALSE;
}




/*
 * Town logic flow for generation of new town
 *
 * We start with a fully wiped cave of normal floors.
 *
 * Note that town_gen_hack() plays games with the R.N.G.
 *
 * This function does NOT do anything about the owners of the stores,
 * nor the contents thereof.  It only handles the physical layout.
 *
 * We place the player on the stairs at the same time we make them.
 *
 * Hack -- since the player always leaves the dungeon by the stairs,
 * he is always placed on the stairs, even if he left the dungeon via
 * word of recall or teleport level.
 */
static void town_gen(void)
{
	int i, y, x;
	int residents;
	bool daytime;

	/* Restrict to single-screen size */
	p_ptr->cur_map_hgt = (2 * PANEL_HGT);
	p_ptr->cur_map_wid = (2 * PANEL_WID_FIXED);

	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
	{
		/* Day time */
		daytime = TRUE;

		/* Number of residents */
		residents = MIN_M_ALLOC_TD;
	}

	/* Night time */
	else
	{
		/* Night time */
		daytime = FALSE;

		/* Number of residents */
		residents = MIN_M_ALLOC_TN;
	}

	/* Start with solid walls */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Create "solid" perma-wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);
		}
	}

	/* Then place some floors */
	for (y = 1; y < p_ptr->cur_map_hgt-1; y++)
	{
		for (x = 1; x < p_ptr->cur_map_wid-1; x++)
		{
			/* Create empty floor */
			cave_set_feat(y, x, FEAT_FLOOR);
		}
	}

	/* Build stuff */
	town_gen_hack();

	/* Apply illumination */
	town_illuminate(daytime);

	/* Make some residents */
	for (i = 0; i < residents; i++)
	{
		/* Make a resident */
		(void)alloc_monster(3, TRUE);
	}
}


/*
 * Generate a random dungeon level
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Hack -- allow auto-scumming via a gameplay option.
 *
 * Note that this function resets "cave_feat" and "cave_info" directly.
 */
void generate_cave(void)
{
	int y, x, num, i;

	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Don't know feeling yet */
	do_feeling = FALSE;

	/*allow uniques to be generated everywhere but in nests/pits*/
	allow_uniques = TRUE;

	/* Generate num is increased below*/
	for (num = 0; TRUE;)
	{
		bool okay = TRUE;

		cptr why = NULL;

		/* Reset */
		o_max = 1;
		mon_max = 1;
		feeling = 0;

		/* Start with a blank cave */
		for (y = 0; y < MAX_DUNGEON_HGT; y++)
		{
			for (x = 0; x < MAX_DUNGEON_WID; x++)
			{
				/* No flags */
				cave_info[y][x] = 0;

				/* No features */
				cave_feat[y][x] = 0;

				/* No objects */
				cave_o_idx[y][x] = 0;

				/* No monsters */
				cave_m_idx[y][x] = 0;

#ifdef MONSTER_FLOW
				for(i = 0; i < MAX_FLOWS; i++)
				{
					cave_cost[i][y][x] = 0;
				}

				cave_when[y][x] = 0;
#endif /* MONSTER_FLOW */

			}
		}

		/* Mega-Hack -- no player yet */
		p_ptr->px = p_ptr->py = 0;

		/* Hack -- illegal panel */
		p_ptr->wy = MAX_DUNGEON_HGT;
		p_ptr->wx = MAX_DUNGEON_WID;

		/* Reset the monster generation level */
		monster_level = p_ptr->depth;

		/* Reset the object generation level */
		object_level = p_ptr->depth;

		/* Nothing special here yet */
		good_item_flag = FALSE;

		/* Nothing good here yet */
		rating = 0;

		/* Build the town */
		if (!p_ptr->depth)
		{
			/* Make a town */
			town_gen();

			/* Hack -- Clear stairs request */
			p_ptr->create_stair = 0;
		}

		/*
	 	 * It is possible for levels to be themed,
	 	 * but make sure the player isn't scumming
		 * or we are on a fixed quest level.
	 	 */
		else if(((p_ptr->depth >= 10) && (allow_altered_inventory) &&
	    		 (one_in_(THEMED_LEVEL_CHANCE)) && (allow_themed_levels) &&
			 	 (!quest_check(p_ptr->depth))) ||
				(quest_check(p_ptr->depth) == QUEST_THEMED_LEVEL))
		{
			if (build_themed_level())
			{

				/* Message. */
				if (cheat_room) msg_print("Themed level");

				okay = TRUE;
			}
			else okay = FALSE;
		}

		/* Build a real level */
		else
		{
			/* Make a dungeon, or report the failure to make one*/
			if (cave_gen()) okay = TRUE;
			else okay = FALSE;

		}

		/*message*/
		if(!okay)
		{
	   		if (cheat_room || cheat_hear || cheat_peek || cheat_xtra)
					why = "defective level";
		}

		else
		{

			/*themed levels already have thier feeling*/
			/* Extract the feeling */
			if (!feeling)
			{
				if (rating > 100) feeling = 2;
				else if (rating > 80) feeling = 3;
				else if (rating > 60) feeling = 4;
				else if (rating > 40) feeling = 5;
				else if (rating > 30) feeling = 6;
				else if (rating > 20) feeling = 7;
				else if (rating > 10) feeling = 8;
				else if (rating > 0) feeling = 9;
				else feeling = 10;

				/* Hack -- Have a special feeling sometimes */
				if (good_item_flag && !adult_preserve) feeling = 1;

				/* Hack -- no feeling in the town */
				if (!p_ptr->depth) feeling = 0;
			}

			/* Prevent object over-flow */
			if (o_max >= z_info->o_max)
			{
				/* Message */
				why = "too many objects";

				/* Message */
				okay = FALSE;
			}

			/* Prevent monster over-flow */
			if (mon_max >= z_info->m_max)
			{
				/* Message */
				why = "too many monsters";

				/* Message */
				okay = FALSE;
			}

			/* Mega-Hack -- "auto-scum" */
			if (auto_scum && (num < 100))
			{
				/*count this level*/
				num++;

				/* Require "goodness", but always accept themed levels */
				if ((feeling < LEV_THEME_HEAD) &&
					((feeling > 9) ||
				     ((p_ptr->depth >= 5) && (feeling > 8)) ||
				     ((p_ptr->depth >= 10) && (feeling > 7)) ||
				     ((p_ptr->depth >= 20) && (feeling > 6)) ||
				     ((p_ptr->depth >= 40) && (feeling > 5))))
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

		}

		/* Accept */
		if (okay) break;

		/* Message */
		if (why) msg_format("Generation restarted (%s)", why);

		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the monsters */
		wipe_mon_list();
	}

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Reset the number of traps on the level. */
	num_trap_on_level = 0;

	/*All of Angband knows about a thief*/
	if ((recent_failed_thefts > 30) && (p_ptr->depth))
	{
			msg_print("You hear hunting parties scouring the area for a notorious burgler.");
	}

}




