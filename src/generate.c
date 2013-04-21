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
 */


/*
 * Dungeon generation values
 */
#define DUN_ROOMS	50 /* Number of rooms to attempt */
#define DUN_UNUSUAL	200	/* Level/chance of unusual room */
#define DUN_DEST	15 /* 1/chance of having a destroyed level */

#define DUN_OPEN_FLOOR  10 /* Chance of having an open level */
#define DUN_OPEN_WATER  10
#define DUN_OPEN_CHAOS  10
#define DUN_OPEN_MAZE   10
#define DUN_OPEN_FOG    10

#define DUN_WILD_STAIRS 30 /* Chance of finding FEAT_SHAFT in the wild. */
#define DUN_WILD_VAULT  100	/* Chance of finding a wilderness vault. */

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND	10 /* Chance of random direction */
#define DUN_TUN_CHG	30 /* Chance of changing direction */
#define DUN_TUN_CON	15 /* Chance of extra tunneling */
#define DUN_TUN_PEN	25 /* Chance of doors at room entrances */
#define DUN_TUN_JCT	90 /* Chance of doors at tunnel junctions */

/*
 * Dungeon streamer generation values
 */
#define DUN_STR_DEN	5 /* Density of streamers */
#define DUN_STR_RNG	2 /* Width of streamers */
#define DUN_STR_MAG	3 /* Number of magma streamers */
#define DUN_STR_MC	90 /* 1/chance of treasure per magma */
#define DUN_STR_QUA	2 /* Number of quartz streamers */
#define DUN_STR_QC	40 /* 1/chance of treasure per quartz */
#define DUN_STR_WLW	1 /* Width of lava & water streamers -KMW- */
#define DUN_STR_DWLW	8 /* Density of water & lava streams -KMW- */

/*
 * Dungeon treasure allocation values
 */
#define DUN_AMT_ROOM	9 /* Amount of objects for rooms */
#define DUN_AMT_ITEM	4 /* Amount of objects for rooms/corridors */
#define DUN_AMT_ALTAR   3 /* Amount of altars */


/*
 * Hack -- Dungeon allocation "places"
 */
#define ALLOC_SET_CORR		1 /* Hallway */
#define ALLOC_SET_ROOM		2 /* Room */
#define ALLOC_SET_BOTH		3 /* Anywhere */

/*
 * Hack -- Dungeon allocation "types"
 */
#define ALLOC_TYP_RUBBLE	1 /* Rubble */
#define ALLOC_TYP_TRAP		3 /* Trap */
#define ALLOC_TYP_OBJECT	4 /* Object */
#define ALLOC_TYP_ALTAR         5 /* Altar */


/*
 * Maximum numbers of rooms along each axis (currently 6x18)
 */
#define MAX_ROOMS_ROW	(DUNGEON_HGT / BLOCK_HGT)
#define MAX_ROOMS_COL	(DUNGEON_WID / BLOCK_WID)


/*
 * Bounds on some arrays used in the "dun_data" structure.
 * These bounds are checked, though usually this is a formality.
 */
#define CENT_MAX	100
#define DOOR_MAX	200
#define WALL_MAX	500
#define TUNN_MAX	900


/*
 * Maximal number of room types
 */
#define ROOM_MAX	10



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
static room_data room[ROOM_MAX] = {
	{0, 0, 0, 0, 0}, /* 0 = Nothing */
	{0, 0, -1, 1, 1}, /* 1 = Simple (33x11) */
	{0, 0, -1, 1, 1}, /* 2 = Overlapping (33x11) */
	{0, 0, -1, 1, 3}, /* 3 = Crossed (33x11) */
	{0, 0, -1, 1, 3}, /* 4 = Large (33x11) */
	{0, 0, -1, 1, 5}, /* 5 = Monster nest (33x11) */
	{0, 0, -1, 1, 5}, /* 6 = Monster pit (33x11) */
	{0, 1, -1, 1, 5}, /* 7 = Lesser vault (33x22) */
	{-1, 2, -2, 3, 10},	/* 8 = Greater vault (66x44) */
	{-1, 2, -2, 3, 5}, /* 9 = Themed vault. */
};



/*
 * Always picks a correct direction
 */
static void correct_dir(int *rdir, int *cdir, int y1, int x1, int y2,
	int x2)
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
static void new_player_spot(void)
{
	int y = p_ptr->py, x = p_ptr->px;

	/* Place the player */
	while (1)
	{
		/* Pick a legal spot */
		y = rand_range(1, DUNGEON_HGT - 2);
		x = rand_range(1, DUNGEON_WID - 2);

		/* Must be a "naked" floor grid */
		if (!cave_naked_bold(y, x))
			continue;

		/* Refuse to start on anti-teleport grids */
		if (cave_info[y][x] & (CAVE_ICKY))
			continue;

		/* Done */
		break;
	}

	/* Place the player */
	player_place(y, x);
}


/*
 * Move the player, but try to keep centered on some location.
 */
static void old_player_spot(void)
{
	int y = p_ptr->py, x = p_ptr->px;
	int d = 4;

	/* Place the player */
	while (1)
	{
		d++;
		scatter(&y, &x, p_ptr->py, p_ptr->px, d / 5, 0);

		/* Must be a "naked" floor grid */
		if (!cave_naked_bold(y, x))
			continue;

		/* Refuse to start on anti-teleport grids */
		if (cave_info[y][x] & (CAVE_ICKY))
			continue;

		/* Done */
		break;
	}

	/* Place the player */
	player_place(y, x);
}


/*
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 *
 * We count only granite walls and permanent walls.
 */
static int next_to_walls(int y, int x)
{
	int k = 0;

	if (cave_feat[y + 1][x] >= FEAT_WALL_EXTRA)
		k++;
	if (cave_feat[y - 1][x] >= FEAT_WALL_EXTRA)
		k++;
	if (cave_feat[y][x + 1] >= FEAT_WALL_EXTRA)
		k++;
	if (cave_feat[y][x - 1] >= FEAT_WALL_EXTRA)
		k++;

	return (k);
}



/*
 * Convert existing terrain type to rubble
 */
static void place_rubble(int y, int x)
{
	/* Create rubble */
	cave_feat[y][x] = FEAT_RUBBLE;
}



/*
 * Convert existing terrain type to "up stairs"
 */
static void place_up_stairs(int y, int x)
{
	/* Create up stairs */
	cave_feat[y][x] = FEAT_LESS;
}


/*
 * Convert existing terrain type to "down stairs"
 */
static void place_down_stairs(int y, int x)
{
	/* Create down stairs */
  if (p_ptr->inside_special == SPECIAL_WILD) {
    cave_feat[y][x] = FEAT_SHAFT;

  } else {
    cave_feat[y][x] = FEAT_MORE;
  }
}





/*
 * Place an up/down staircase at given location
 */
static void place_random_stairs(int y, int x)
{
	/* Paranoia */
	if (!cave_clean_bold(y, x))
		return;

	/* Choose a staircase */
	if (!p_ptr->depth)
	{
		place_down_stairs(y, x);
	}
	else if (p_ptr->inside_special || (p_ptr->depth >= MAX_DEPTH - 1))
	{
		place_up_stairs(y, x);
	}
	else if (rand_int(100) < 50)
	{
		place_down_stairs(y, x);
	}
	else
	{
		place_up_stairs(y, x);
	}
}

/*
 * Place an altar at the given location
 */
static void place_altar(int y, int x)
{
	int alt, rar;

	while (TRUE)
	{
		alt = rand_int(MAX_GODS);
		rar = deity_info[alt].rarity % 4;

		if (p_ptr->depth < randnor(rar * 10, 3) || rand_int(rar) > 0)
			continue;

		break;
	}

	cave_feat[y][x] = FEAT_ALTAR_HEAD + alt;
}


/*
 * Place a locked door at the given location
 */
static void place_locked_door(int y, int x)
{
	/* Create locked door */
	cave_feat[y][x] = FEAT_DOOR_HEAD + randint(7);
}


/*
 * Place a secret door at the given location
 */
static void place_secret_door(int y, int x)
{
	/* Create secret door */
	cave_feat[y][x] = FEAT_SECRET;
}


/*
 * Place a random type of door at the given location
 */
static void place_random_door(int y, int x)
{
	int tmp;

	/* Choose an object */
	tmp = rand_int(1000);

	/* Open doors (300/1000) */
	if (tmp < 300)
	{
		/* Create open door */
		cave_feat[y][x] = FEAT_OPEN;
	}

	/* Broken doors (100/1000) */
	else if (tmp < 400)
	{
		/* Create broken door */
		cave_feat[y][x] = FEAT_BROKEN;
	}

	/* Secret doors (200/1000) */
	else if (tmp < 600)
	{
		/* Create secret door */
		cave_feat[y][x] = FEAT_SECRET;
	}

	/* Closed doors (300/1000) */
	else if (tmp < 900)
	{
		/* Create closed door */
		cave_feat[y][x] = FEAT_DOOR_HEAD + 0x00;
	}

	/* Locked doors (99/1000) */
	else if (tmp < 999)
	{
		/* Create locked door */
		cave_feat[y][x] = FEAT_DOOR_HEAD + randint(7);
	}

	/* Stuck doors (1/1000) */
	else
	{
		/* Create jammed door */
		cave_feat[y][x] = FEAT_DOOR_HEAD + 0x08 + rand_int(8);
	}
}



/*
 * Places some staircases near walls
 */
static void alloc_stairs(int feat, int num, int walls)
{
	int y, x, i, j, flag;


	/* Place "num" stairs */
	for (i = 0; i < num; i++)
	{
		/* Place some stairs */
		for (flag = FALSE; !flag;)
		{
			/* Try several times, then decrease "walls" */
			for (j = 0; !flag && j < 3000; j++)
			{

				/* Pick a random grid */
				y = rand_int(DUNGEON_HGT);
				x = rand_int(DUNGEON_WID);

				/* Require "naked" floor grid */
				if (!cave_naked_bold(y, x))
					continue;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(y, x) < walls)
					continue;

				/* Town -- must go down */
				if (!p_ptr->depth)
				{
					/* Clear previous contents, add down stairs */
				  if (p_ptr->inside_special == SPECIAL_WILD) {
				    cave_feat[y][x] = FEAT_SHAFT;
				  } else {
				    cave_feat[y][x] = FEAT_MORE;
				  }
				}

				/* Quest -- must go up */
				else if (p_ptr->inside_special == SPECIAL_QUEST ||
					(p_ptr->depth >= MAX_DEPTH - 1))
				{
					/* Clear previous contents, add up stairs */
					cave_feat[y][x] = FEAT_LESS;
				}

				/* Requested type */
				else
				{
					/* Clear previous contents, add stairs */
					cave_feat[y][x] = feat;
				}

				/* All done */
				flag = TRUE;
			}

			/* Require fewer walls */
			if (walls)
				walls--;
		}
	}
}




/*
 * Allocates some objects (using "place" and "type")
 */
static void alloc_object(int set, int typ, int num)
{
	int y, x, k;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		while (TRUE)
		{
			bool room;

			/* Location */
			y = rand_int(DUNGEON_HGT);
			x = rand_int(DUNGEON_WID);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x))
				continue;

			/* Check for "room" */
			room = (cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room)
				continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room)
				continue;

			/* Accept it */
			break;
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

			case ALLOC_TYP_OBJECT:
			{
				place_object(y, x, FALSE, FALSE);
				break;
			}

			case ALLOC_TYP_ALTAR:
			{
				place_altar(y, x);
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
static void build_streamer(int feat, int chance)
{
	int i, tx, ty;
	int y, x, dir;


	/* Hack -- Choose starting point */
	y = rand_spread(DUNGEON_HGT / 2, 10);
	x = rand_spread(DUNGEON_WID / 2, 15);

	/* Choose a random compass direction */
	dir = ddd[rand_int(8)];

	/* Place streamer into dungeon */
	while (TRUE)
	{
		/* One grid per density */
		for (i = 0; i < DUN_STR_DEN; i++)
		{
			int d = DUN_STR_RNG;

			/* Pick a nearby grid */
			while (1)
			{
				ty = rand_spread(y, d);
				tx = rand_spread(x, d);
				if (!in_bounds(ty, tx))
					continue;
				break;
			}

			/* Only convert "granite" walls */
			if (cave_feat[ty][tx] < FEAT_WALL_EXTRA)
				continue;
			if (cave_feat[ty][tx] > FEAT_WALL_SOLID)
				continue;

			/* Clear previous contents, add proper vein type */
			cave_feat[ty][tx] = feat;

			/* Hack -- Add some (known) treasure */
			if (rand_int(chance) == 0)
				cave_feat[ty][tx] += 0x04;
		}

		/* Advance the streamer */
		y += ddy[dir];
		x += ddx[dir];

		/* Stop at dungeon edge */
		if (!in_bounds(y, x))
			break;
	}
}


/*
 *  Place streams of water, lava, & trees -KMW-
 * This routine varies the placement based on dungeon level
 * otherwise is similar to build_streamer
 */
static void build_streamer2(int feat, int killwall)
{
	int i, j, mid, tx, ty;
	int y, x, dir;
	int poolchance;
	int poolsize;

	poolchance = randint(10);

	/* Hack -- Choose starting point */
	y = rand_spread(DUNGEON_HGT / 2, 10);
	x = rand_spread(DUNGEON_WID / 2, 15);

	/* Choose a random compass direction */
	dir = ddd[rand_int(8)];

	if (poolchance > 2)
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
					if (!in_bounds(ty, tx))
						continue;
					break;
				}

				/* Do not mess up vaults */
				if (cave_info[ty][tx] & CAVE_ICKY)
					continue;


				/* Only convert non-permanent features */
				if (killwall == 0)
				{
					if (cave_feat[ty][tx] >= FEAT_MAGMA)
						continue;
					if (cave_feat[ty][tx] == FEAT_LESS)
						continue;
					if (cave_feat[ty][tx] == FEAT_MORE)
						continue;
				}
				else
				{
					if (cave_feat[ty][tx] >= FEAT_PERM_EXTRA)
						continue;
					if (cave_feat[ty][tx] == FEAT_LESS)
						continue;
					if (cave_feat[ty][tx] == FEAT_MORE)
						continue;
				}

				/* Clear previous contents, add proper vein type */
				cave_feat[ty][tx] = feat;
			}

			/* Advance the streamer */
			y += ddy[dir];
			x += ddx[dir];

			if (randint(20) == 1)
				dir = ddd[rand_int(8)];	/* change direction */

			/* Stop at dungeon edge */
			if (!in_bounds(y, x))
				break;
		}
	}
	else if ((feat == FEAT_DEEP_WATER) || (feat == FEAT_DEEP_LAVA) ||
		(feat == FEAT_CHAOS_FOG))
	{ /* create pool */
		poolsize = 5 + randint(10);
		mid = poolsize / 2;
		/* One grid per density */
		for (i = 0; i < poolsize; i++)
		{
			for (j = 0; j < poolsize; j++)
			{
				tx = x + j;
				ty = y + i;

				if (!in_bounds(ty, tx))
					continue;

				if (i < mid)
				{
					if (j < mid)
					{
						if ((i + j + 1) < mid)
							continue;
					}
					else if (j > (mid + i))
						continue;
				}
				else if (j < mid)
				{
					if (i > (mid + j))
						continue;
				}
				else if ((i + j) > ((mid * 3) - 1))
					continue;

				/* Do not mess up vaults */
				if (cave_info[ty][tx] & CAVE_ICKY)
					continue;

				/* Only convert non-permanent features */
				if (cave_feat[ty][tx] >= FEAT_PERM_EXTRA)
					continue;
				if (cave_feat[ty][tx] == FEAT_LESS)
					continue;
				if (cave_feat[ty][tx] == FEAT_MORE)
					continue;
				/* Clear previous contents, add proper vein type */
				cave_feat[ty][tx] = feat;
			}
		}
	}
}


/*
 * Build a destroyed level
 */
static void destroy_level(void)
{
	int y1, x1, y, x, k, t, n;

	object_type *o_ptr;
	object_type *o_nxt;


	/* Note destroyed levels */
	if (cheat_room)
		msg_print("Destroyed Level");

	/* Drop a few epi-centers (usually about two) */
	for (n = 0; n < randint(5); n++)
	{
		/* Pick an epi-center */
		x1 = rand_range(5, DUNGEON_WID - 1 - 5);
		y1 = rand_range(5, DUNGEON_HGT - 1 - 5);

		/* Big area of affect */
		for (y = (y1 - 15); y <= (y1 + 15); y++)
		{
			for (x = (x1 - 15); x <= (x1 + 15); x++)
			{
				/* Skip illegal grids */
				if (!in_bounds_fully(y, x))
					continue;

				/* Extract the distance */
				k = distance(y1, x1, y, x);

				/* Stay in the circle of death */
				if (k >= 16)
					continue;

				/* Delete the monster (if any) */
				delete_monster(y, x);

				/* Destroy valid grids */
				if (cave_valid_bold(y, x))
				{

					/* Delete objects */
					o_ptr = cave_o_idx[y][x];

					while (TRUE)
					{
						if (!o_ptr)
							break;

						o_nxt = o_ptr->next;
						remove_object(o_ptr);
						o_ptr = o_nxt;
					}

					/* Wall (or floor) type */
					t = rand_int(200);

					/* Granite */
					if (t < 20)
					{
						/* Create granite wall */
						cave_feat[y][x] = FEAT_WALL_EXTRA;
					}

					/* Quartz */
					else if (t < 70)
					{
						/* Create quartz vein */
						cave_feat[y][x] = FEAT_QUARTZ;
					}

					/* Magma */
					else if (t < 100)
					{
						/* Create magma vein */
						cave_feat[y][x] = FEAT_MAGMA;
					}

					/* Floor */
					else
					{
						/* Create floor */
						cave_feat[y][x] = FEAT_FLOOR;
					}

					/* No longer part of a room or vault */
					cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

					/* No longer illuminated or known */
					cave_info[y][x] &= ~(CAVE_MARK | CAVE_GLOW);
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
				if (!in_bounds(j, k))
					continue;
				break;
			}

			/* Require "clean" floor space */
			if (!cave_clean_bold(j, k))
				continue;

			/* Place an item */
			place_object(j, k, FALSE, FALSE);

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
			if (!in_bounds(y1, x1))
				continue;
			break;
		}

		/* Require "naked" floor grids */
		if (!cave_naked_bold(y1, x1))
			continue;

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
 * Hack -- Place some sleeping monsters near the given location
 */
static void vault_monsters(int y1, int x1, int flags)
{
	/* Place the monster (allow groups) */
	monster_level = p_ptr->depth + 2;
	place_monster(y1, x1, flags);
	monster_level = p_ptr->depth;
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
 *   9 -- themed vaults
 */


/*
 * Type 1 -- normal rectangular rooms
 */
static void build_type1(int yval, int xval)
{
	int y, x, y2, x2;
	int y1, x1;

	bool light;


	/* Choose lite or dark */
	light = (p_ptr->depth <= randint(25));


	/* Pick a room size */
	y1 = yval - randint(4);
	y2 = yval + randint(3);
	x1 = xval - randint(11);
	x2 = xval + randint(11);


	/* Place a full floor under the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
			cave_info[y][x] |= (CAVE_ROOM);
			if (light)
				cave_info[y][x] |= (CAVE_GLOW);
		}
	}

	/* Walls around the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		cave_feat[y][x1 - 1] = FEAT_WALL_OUTER;
		cave_feat[y][x2 + 1] = FEAT_WALL_OUTER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		cave_feat[y1 - 1][x] = FEAT_WALL_OUTER;
		cave_feat[y2 + 1][x] = FEAT_WALL_OUTER;
	}


	/* Hack -- Occasional pillar room */
	if (rand_int(20) == 0)
	{
		for (y = y1; y <= y2; y += 2)
		{
			for (x = x1; x <= x2; x += 2)
			{
				cave_feat[y][x] = FEAT_WALL_INNER;
			}
		}
	}

	/* Hack -- Occasional ragged-edge room */
	else if (rand_int(50) == 0)
	{
		for (y = y1 + 2; y <= y2 - 2; y += 2)
		{
			cave_feat[y][x1] = FEAT_WALL_INNER;
			cave_feat[y][x2] = FEAT_WALL_INNER;
		}
		for (x = x1 + 2; x <= x2 - 2; x += 2)
		{
			cave_feat[y1][x] = FEAT_WALL_INNER;
			cave_feat[y2][x] = FEAT_WALL_INNER;
		}
	}
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static void build_type2(int yval, int xval)
{
	int y, x;
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;

	bool light;



	/* Choose lite or dark */
	light = (p_ptr->depth <= randint(25));


	/* Determine extents of the first room */
	y1a = yval - randint(4);
	y2a = yval + randint(3);
	x1a = xval - randint(11);
	x2a = xval + randint(10);

	/* Determine extents of the second room */
	y1b = yval - randint(3);
	y2b = yval + randint(4);
	x1b = xval - randint(10);
	x2b = xval + randint(11);


	/* Place a full floor for room "a" */
	for (y = y1a - 1; y <= y2a + 1; y++)
	{
		for (x = x1a - 1; x <= x2a + 1; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
			cave_info[y][x] |= (CAVE_ROOM);
			if (light)
				cave_info[y][x] |= (CAVE_GLOW);
		}
	}

	/* Place a full floor for room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		for (x = x1b - 1; x <= x2b + 1; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
			cave_info[y][x] |= (CAVE_ROOM);
			if (light)
				cave_info[y][x] |= (CAVE_GLOW);
		}
	}


	/* Place the walls around room "a" */
	for (y = y1a - 1; y <= y2a + 1; y++)
	{
		cave_feat[y][x1a - 1] = FEAT_WALL_OUTER;
		cave_feat[y][x2a + 1] = FEAT_WALL_OUTER;
	}
	for (x = x1a - 1; x <= x2a + 1; x++)
	{
		cave_feat[y1a - 1][x] = FEAT_WALL_OUTER;
		cave_feat[y2a + 1][x] = FEAT_WALL_OUTER;
	}

	/* Place the walls around room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		cave_feat[y][x1b - 1] = FEAT_WALL_OUTER;
		cave_feat[y][x2b + 1] = FEAT_WALL_OUTER;
	}
	for (x = x1b - 1; x <= x2b + 1; x++)
	{
		cave_feat[y1b - 1][x] = FEAT_WALL_OUTER;
		cave_feat[y2b + 1][x] = FEAT_WALL_OUTER;
	}



	/* Replace the floor for room "a" */
	for (y = y1a; y <= y2a; y++)
	{
		for (x = x1a; x <= x2a; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
		}
	}

	/* Replace the floor for room "b" */
	for (y = y1b; y <= y2b; y++)
	{
		for (x = x1b; x <= x2b; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
		}
	}
}



/*
 * Type 3 -- Cross shaped rooms
 *
 * Builds a room at a row, column coordinate
 *
 * Room "a" runs north/south, and Room "b" runs east/east
 * So the "central pillar" runs from x1a,y1b to x2a,y2b.
 *
 * Note that currently, the "center" is always 3x3, but I think that
 * the code below will work (with "bounds checking") for 5x5, or even
 * for unsymetric values like 4x3 or 5x3 or 3x4 or 3x5, or even larger.
 */
static void build_type3(int yval, int xval)
{
	int y, x, dy, dx, wy, wx;
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;

	bool light;



	/* Choose lite or dark */
	light = (p_ptr->depth <= randint(25));


	/* For now, always 3x3 */
	wx = wy = 1;

	/* Pick max vertical size (at most 4) */
	dy = rand_range(3, 4);

	/* Pick max horizontal size (at most 15) */
	dx = rand_range(3, 11);


	/* Determine extents of the north/south room */
	y1a = yval - dy;
	y2a = yval + dy;
	x1a = xval - wx;
	x2a = xval + wx;

	/* Determine extents of the east/west room */
	y1b = yval - wy;
	y2b = yval + wy;
	x1b = xval - dx;
	x2b = xval + dx;


	/* Place a full floor for room "a" */
	for (y = y1a - 1; y <= y2a + 1; y++)
	{
		for (x = x1a - 1; x <= x2a + 1; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
			cave_info[y][x] |= (CAVE_ROOM);
			if (light)
				cave_info[y][x] |= (CAVE_GLOW);
		}
	}

	/* Place a full floor for room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		for (x = x1b - 1; x <= x2b + 1; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
			cave_info[y][x] |= (CAVE_ROOM);
			if (light)
				cave_info[y][x] |= (CAVE_GLOW);
		}
	}


	/* Place the walls around room "a" */
	for (y = y1a - 1; y <= y2a + 1; y++)
	{
		cave_feat[y][x1a - 1] = FEAT_WALL_OUTER;
		cave_feat[y][x2a + 1] = FEAT_WALL_OUTER;
	}
	for (x = x1a - 1; x <= x2a + 1; x++)
	{
		cave_feat[y1a - 1][x] = FEAT_WALL_OUTER;
		cave_feat[y2a + 1][x] = FEAT_WALL_OUTER;
	}

	/* Place the walls around room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		cave_feat[y][x1b - 1] = FEAT_WALL_OUTER;
		cave_feat[y][x2b + 1] = FEAT_WALL_OUTER;
	}
	for (x = x1b - 1; x <= x2b + 1; x++)
	{
		cave_feat[y1b - 1][x] = FEAT_WALL_OUTER;
		cave_feat[y2b + 1][x] = FEAT_WALL_OUTER;
	}


	/* Replace the floor for room "a" */
	for (y = y1a; y <= y2a; y++)
	{
		for (x = x1a; x <= x2a; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
		}
	}

	/* Replace the floor for room "b" */
	for (y = y1b; y <= y2b; y++)
	{
		for (x = x1b; x <= x2b; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
		}
	}



	/* Special features (3/4) */
	switch (rand_int(4))
	{
			/* Large solid middle pillar */
		case 1:
		{
			for (y = y1b; y <= y2b; y++)
			{
				for (x = x1a; x <= x2a; x++)
				{
					cave_feat[y][x] = FEAT_WALL_INNER;
				}
			}
			break;
		}

			/* Inner treasure vault */
		case 2:
		{
			/* Build the vault */
			for (y = y1b; y <= y2b; y++)
			{
				cave_feat[y][x1a] = FEAT_WALL_INNER;
				cave_feat[y][x2a] = FEAT_WALL_INNER;
			}
			for (x = x1a; x <= x2a; x++)
			{
				cave_feat[y1b][x] = FEAT_WALL_INNER;
				cave_feat[y2b][x] = FEAT_WALL_INNER;
			}

			/* Place a secret door on the inner room */
			switch (rand_int(4))
			{
				case 0:
					place_secret_door(y1b, xval);
					break;
				case 1:
					place_secret_door(y2b, xval);
					break;
				case 2:
					place_secret_door(yval, x1a);
					break;
				case 3:
					place_secret_door(yval, x2a);
					break;
			}

			/* Place a treasure in the vault */
			place_object(yval, xval, FALSE, FALSE);

			/* Let's guard the treasure well */
			vault_monsters(yval, xval, MON_ALLOC_SLEEP | MON_ALLOC_HORDE);

			/* Traps naturally */
			vault_traps(yval, xval, 4, 4, rand_int(3) + 2);

			break;
		}

			/* Something else */
		case 3:
		{
			/* Occasionally pinch the center shut */
			if (rand_int(3) == 0)
			{
				/* Pinch the east/west sides */
				for (y = y1b; y <= y2b; y++)
				{
					if (y == yval)
						continue;
					cave_feat[y][x1a - 1] = FEAT_WALL_INNER;
					cave_feat[y][x2a + 1] = FEAT_WALL_INNER;
				}

				/* Pinch the north/south sides */
				for (x = x1a; x <= x2a; x++)
				{
					if (x == xval)
						continue;
					cave_feat[y1b - 1][x] = FEAT_WALL_INNER;
					cave_feat[y2b + 1][x] = FEAT_WALL_INNER;
				}

				/* Sometimes shut using secret doors */
				if (rand_int(3) == 0)
				{
					place_secret_door(yval, x1a - 1);
					place_secret_door(yval, x2a + 1);
					place_secret_door(y1b - 1, xval);
					place_secret_door(y2b + 1, xval);
				}
			}

			/* Occasionally put a "plus" in the center */
			else if (rand_int(3) == 0)
			{
				cave_feat[yval][xval] = FEAT_WALL_INNER;
				cave_feat[y1b][xval] = FEAT_WALL_INNER;
				cave_feat[y2b][xval] = FEAT_WALL_INNER;
				cave_feat[yval][x1a] = FEAT_WALL_INNER;
				cave_feat[yval][x2a] = FEAT_WALL_INNER;
			}

			/* Occasionally put a pillar in the center */
			else if (rand_int(3) == 0)
			{
				cave_feat[yval][xval] = FEAT_WALL_INNER;
			}

			break;
		}
	}
}


/*
 * Type 4 -- Large room with inner features
 *
 * Possible sub-types:
 *	1 - Just an inner room with one door
 *	2 - An inner room within an inner room
 *	3 - An inner room with pillar(s)
 *	4 - Inner room has a maze
 *	5 - A set of four inner rooms
 */
static void build_type4(int yval, int xval)
{
	int y, x, y1, x1;
	int y2, x2, tmp;

	bool light;



	/* Choose lite or dark */
	light = (p_ptr->depth <= randint(25));


	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;


	/* Place a full floor under the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
			cave_info[y][x] |= (CAVE_ROOM);
			if (light)
				cave_info[y][x] |= (CAVE_GLOW);
		}
	}

	/* Outer Walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		cave_feat[y][x1 - 1] = FEAT_WALL_OUTER;
		cave_feat[y][x2 + 1] = FEAT_WALL_OUTER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		cave_feat[y1 - 1][x] = FEAT_WALL_OUTER;
		cave_feat[y2 + 1][x] = FEAT_WALL_OUTER;
	}


	/* The inner room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* The inner walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		cave_feat[y][x1 - 1] = FEAT_WALL_INNER;
		cave_feat[y][x2 + 1] = FEAT_WALL_INNER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		cave_feat[y1 - 1][x] = FEAT_WALL_INNER;
		cave_feat[y2 + 1][x] = FEAT_WALL_INNER;
	}


	/* Inner room variations */
	switch (randint(5))
	{
			/* Just an inner room with a monster */
		case 1:

			/* Place a secret door */
			switch (randint(4))
			{
				case 1:
					place_secret_door(y1 - 1, xval);
					break;
				case 2:
					place_secret_door(y2 + 1, xval);
					break;
				case 3:
					place_secret_door(yval, x1 - 1);
					break;
				case 4:
					place_secret_door(yval, x2 + 1);
					break;
			}

			/* Place a monster in the room */
			vault_monsters(yval, xval, MON_ALLOC_SLEEP);

			break;


			/* Treasure Vault (with a door) */
		case 2:

			/* Place a secret door */
			switch (randint(4))
			{
				case 1:
					place_secret_door(y1 - 1, xval);
					break;
				case 2:
					place_secret_door(y2 + 1, xval);
					break;
				case 3:
					place_secret_door(yval, x1 - 1);
					break;
				case 4:
					place_secret_door(yval, x2 + 1);
					break;
			}

			/* Place another inner room */
			for (y = yval - 1; y <= yval + 1; y++)
			{
				for (x = xval - 1; x <= xval + 1; x++)
				{
					if ((x == xval) && (y == yval))
						continue;
					cave_feat[y][x] = FEAT_WALL_INNER;
				}
			}

			/* Place a locked door on the inner room */
			switch (randint(4))
			{
				case 1:
					place_locked_door(yval - 1, xval);
					break;
				case 2:
					place_locked_door(yval + 1, xval);
					break;
				case 3:
					place_locked_door(yval, xval - 1);
					break;
				case 4:
					place_locked_door(yval, xval + 1);
					break;
			}

			/* Monsters to guard the "treasure" */
			vault_monsters(yval, xval, MON_ALLOC_SLEEP | MON_ALLOC_HORDE);

			/* Object (80%) */
			if (rand_int(100) < 80)
			{
				place_object(yval, xval, FALSE, FALSE);
			}

			/* Stairs (20%) */
			else
			{
				place_random_stairs(yval, xval);
			}

			/* Traps to protect the treasure */
			vault_traps(yval, xval, 4, 10, 2 + randint(3));

			break;


			/* Inner pillar(s). */
		case 3:

			/* Place a secret door */
			switch (randint(4))
			{
				case 1:
					place_secret_door(y1 - 1, xval);
					break;
				case 2:
					place_secret_door(y2 + 1, xval);
					break;
				case 3:
					place_secret_door(yval, x1 - 1);
					break;
				case 4:
					place_secret_door(yval, x2 + 1);
					break;
			}

			/* Large Inner Pillar */
			for (y = yval - 1; y <= yval + 1; y++)
			{
				for (x = xval - 1; x <= xval + 1; x++)
				{
					cave_feat[y][x] = FEAT_WALL_INNER;
				}
			}

			/* Occasionally, two more Large Inner Pillars */
			if (rand_int(2) == 0)
			{
				tmp = randint(2);
				for (y = yval - 1; y <= yval + 1; y++)
				{
					for (x = xval - 5 - tmp; x <= xval - 3 - tmp; x++)
					{
						cave_feat[y][x] = FEAT_WALL_INNER;
					}
					for (x = xval + 3 + tmp; x <= xval + 5 + tmp; x++)
					{
						cave_feat[y][x] = FEAT_WALL_INNER;
					}
				}
			}

			/* Occasionally, some Inner rooms */
			if (rand_int(3) == 0)
			{
				/* Long horizontal walls */
				for (x = xval - 5; x <= xval + 5; x++)
				{
					cave_feat[yval - 1][x] = FEAT_WALL_INNER;
					cave_feat[yval + 1][x] = FEAT_WALL_INNER;
				}

				/* Close off the left/right edges */
				cave_feat[yval][xval - 5] = FEAT_WALL_INNER;
				cave_feat[yval][xval + 5] = FEAT_WALL_INNER;

				/* Secret doors (random top/bottom) */
				place_secret_door(yval - 3 + (randint(2) * 2), xval - 3);
				place_secret_door(yval - 3 + (randint(2) * 2), xval + 3);

				/* Monsters */
				vault_monsters(yval, xval - 2,
					MON_ALLOC_SLEEP | MON_ALLOC_HORDE);
				vault_monsters(yval, xval + 2,
					MON_ALLOC_SLEEP | MON_ALLOC_HORDE);

				/* Objects */
				if (rand_int(3) == 0)
					place_object(yval, xval - 2, FALSE, FALSE);
				if (rand_int(3) == 0)
					place_object(yval, xval + 2, FALSE, FALSE);
			}

			break;


			/* Maze inside. */
		case 4:

			/* Place a secret door */
			switch (randint(4))
			{
				case 1:
					place_secret_door(y1 - 1, xval);
					break;
				case 2:
					place_secret_door(y2 + 1, xval);
					break;
				case 3:
					place_secret_door(yval, x1 - 1);
					break;
				case 4:
					place_secret_door(yval, x2 + 1);
					break;
			}

			/* Maze (really a checkerboard) */
			for (y = y1; y <= y2; y++)
			{
				for (x = x1; x <= x2; x++)
				{
					if (0x1 & (x + y))
					{
						cave_feat[y][x] = FEAT_WALL_INNER;
					}
				}
			}

			/* Monsters just love mazes. */
			vault_monsters(yval, xval - 5,
				MON_ALLOC_SLEEP | MON_ALLOC_HORDE);
			vault_monsters(yval, xval + 5,
				MON_ALLOC_SLEEP | MON_ALLOC_HORDE);

			/* Traps make them entertaining. */
			vault_traps(yval, xval - 3, 2, 8, randint(3));
			vault_traps(yval, xval + 3, 2, 8, randint(3));

			/* Mazes should have some treasure too. */
			vault_objects(yval, xval, 3);

			break;


			/* Four small rooms. */
		case 5:

			/* Inner "cross" */
			for (y = y1; y <= y2; y++)
			{
				cave_feat[y][xval] = FEAT_WALL_INNER;
			}
			for (x = x1; x <= x2; x++)
			{
				cave_feat[yval][x] = FEAT_WALL_INNER;
			}

			/* Doors into the rooms */
			if (rand_int(100) < 50)
			{
				int i = randint(10);
				place_secret_door(y1 - 1, xval - i);
				place_secret_door(y1 - 1, xval + i);
				place_secret_door(y2 + 1, xval - i);
				place_secret_door(y2 + 1, xval + i);
			}
			else
			{
				int i = randint(3);
				place_secret_door(yval + i, x1 - 1);
				place_secret_door(yval - i, x1 - 1);
				place_secret_door(yval + i, x2 + 1);
				place_secret_door(yval - i, x2 + 1);
			}

			/* Treasure, centered at the center of the cross */
			vault_objects(yval, xval, 2 + randint(2));

			/* Gotta have some monsters. */
			vault_monsters(yval + 1, xval - 4,
				MON_ALLOC_SLEEP | MON_ALLOC_HORDE);
			vault_monsters(yval + 1, xval + 4,
				MON_ALLOC_SLEEP | MON_ALLOC_HORDE);
			vault_monsters(yval - 1, xval - 4,
				MON_ALLOC_SLEEP | MON_ALLOC_HORDE);
			vault_monsters(yval - 1, xval + 4,
				MON_ALLOC_SLEEP | MON_ALLOC_HORDE);

			break;
	}
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
 * Currently, a monster nest is one of
 *   a nest of "jelly" monsters   (Dungeon level 5 and deeper)
 *   a nest of "animal" monsters  (Dungeon level 30 and deeper)
 *   a nest of "undead" monsters  (Dungeon level 50 and deeper)
 *
 * Note that the "get_mon_num()" function may (rarely) fail, in which
 * case the nest will be empty, and will not affect the level rating.
 *
 * Note that "monster nests" will never contain "unique" monsters.
 */
static void build_type5(int yval, int xval)
{
	int y, x, y1, x1, y2, x2;

	if (seed_dungeon)
	{
		Rand_quick = FALSE;
	}


	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;


	/* Place the floor area */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
			cave_info[y][x] |= (CAVE_ROOM);
		}
	}

	/* Place the outer walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		cave_feat[y][x1 - 1] = FEAT_WALL_OUTER;
		cave_feat[y][x2 + 1] = FEAT_WALL_OUTER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		cave_feat[y1 - 1][x] = FEAT_WALL_OUTER;
		cave_feat[y2 + 1][x] = FEAT_WALL_OUTER;
	}


	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* The inner walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		cave_feat[y][x1 - 1] = FEAT_WALL_INNER;
		cave_feat[y][x2 + 1] = FEAT_WALL_INNER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		cave_feat[y1 - 1][x] = FEAT_WALL_INNER;
		cave_feat[y2 + 1][x] = FEAT_WALL_INNER;
	}


	/* Place a secret door */
	switch (randint(4))
	{
		case 1:
			place_secret_door(y1 - 1, xval);
			break;
		case 2:
			place_secret_door(y2 + 1, xval);
			break;
		case 3:
			place_secret_door(yval, x1 - 1);
			break;
		case 4:
			place_secret_door(yval, x2 + 1);
			break;
	}

	/* Place a monster, ensure escorts. */
	place_monster(yval, xval, MON_ALLOC_PIT);

	/* Describe */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster nest");
	}


	/* Increase the level rating */
	rating += 10;

	/* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
	if ((p_ptr->depth <= 40) &&
		(randint(p_ptr->depth * p_ptr->depth + 1) < 300))
	{
		good_item_flag = TRUE;
	}


	if (seed_dungeon)
	{
		Rand_quick = TRUE;
	}
}



/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type organized in the room.
 *
 */
static void build_type6(int yval, int xval)
{
	int y, x, y1, x1, y2, x2;


	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	if (seed_dungeon)
	{
		Rand_quick = FALSE;
	}


	/* Place the floor area */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			cave_feat[y][x] = FEAT_FLOOR;
			cave_info[y][x] |= (CAVE_ROOM);
		}
	}

	/* Place the outer walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		cave_feat[y][x1 - 1] = FEAT_WALL_OUTER;
		cave_feat[y][x2 + 1] = FEAT_WALL_OUTER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		cave_feat[y1 - 1][x] = FEAT_WALL_OUTER;
		cave_feat[y2 + 1][x] = FEAT_WALL_OUTER;
	}


	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* The inner walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		cave_feat[y][x1 - 1] = FEAT_WALL_INNER;
		cave_feat[y][x2 + 1] = FEAT_WALL_INNER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		cave_feat[y1 - 1][x] = FEAT_WALL_INNER;
		cave_feat[y2 + 1][x] = FEAT_WALL_INNER;
	}


	/* Place a secret door */
	switch (randint(4))
	{
		case 1:
			place_secret_door(y1 - 1, xval);
			break;
		case 2:
			place_secret_door(y2 + 1, xval);
			break;
		case 3:
			place_secret_door(yval, x1 - 1);
			break;
		case 4:
			place_secret_door(yval, x2 + 1);
			break;
	}

	/* Place some monsters, ensure escorts and groups. */
	place_monster(yval, xval, MON_ALLOC_PIT | MON_ALLOC_GROUP);

	/* Message */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster pit");
	}


	/* Increase the level rating */
	rating += 10;

	/* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
	if ((p_ptr->depth <= 40) &&
		(randint(p_ptr->depth * p_ptr->depth + 1) < 300))
	{
		good_item_flag = TRUE;
	}

	if (seed_dungeon)
	{
		Rand_quick = TRUE;
	}
}








static char hook_vault_monster_param;
static char hook_vault_object_param;

static bool hook_vault_place_player = FALSE;


static bool hook_vault_monster(int r_idx)
{
	if (r_info[r_idx].d_char == hook_vault_monster_param)
		return TRUE;
	return FALSE;
}

static bool hook_vault_object(int k_idx)
{
	if (k_info[k_idx].d_char == hook_vault_object_param)
		return TRUE;
	return FALSE;
}

/*
 * Hack -- fill in "vault" rooms
 * Added parameter for quest type -KMW-
 * Modified to read extended vault information -KMW-
 */
static void build_vault(int yval, int xval, vault_type* v_ptr) {
	int xmax = v_ptr->wid;
	int ymax = v_ptr->hgt;
	cptr data = v_text + v_ptr->text;
	cptr mdata = vm_text + v_ptr->m_text;
	cptr t = data;
	cptr t2 = mdata;

	int dx, dy, x, y, i, j;


	bool town_symb = (v_ptr->typ == 10 || v_ptr->typ == 11 ||
			  v_ptr->typ == 12);

	bool wild_symb = (v_ptr->typ == 13);

	char datum;
	byte number;

	int mode = MON_ALLOC_SLEEP;

	i = 0;
	j = 0;

	/* Flag quest monsters as such. */
	if (v_ptr->typ == 99)
	{
		mode |= MON_ALLOC_QUEST;
	}

	/* Vaults are different even in persistent dungeons. */
	if (seed_dungeon)
	{
		Rand_quick = FALSE;
	}

	/* Get the first chunk of data. */
	datum = t[0];
	number = t[1];

	/* Place dungeon features. */
	for (dy = 0; dy < ymax; dy++) {
	  for (dx = 0; dx < xmax; dx++) {

	    /* Extract the location */
	    x = xval - (xmax / 2) + dx;
	    y = yval - (ymax / 2) + dy;


	    /* Place some dungeon features. */
	    if (datum != ' ' && datum != '-') {

	      /* Lay down a floor */
	      cave_feat[y][x] = FEAT_FLOOR;

	      /* Part of a vault */
	      cave_info[y][x] |= (CAVE_ROOM);

	      if (!town_symb && !wild_symb) {
		cave_info[y][x] |= (CAVE_ICKY);
	      }

	      /* Shop, 0-7 */
	      if (isdigit(datum) && datum < '8') {
		int feat = FEAT_SHOP_HEAD + (datum - '0');

		cave_feat[y][x] = feat;
	      }

	      /* Building, a-z */
	      if (islower(datum)) {
		int feat = FEAT_BLDG_HEAD + (datum - 'a');

		cave_feat[y][x] = feat;
	      }

	      /* Analyze the grid */
	      switch (datum) {

		/* Granite wall (outer) */
	      case '%':
		cave_feat[y][x] = FEAT_WALL_OUTER;
		break;

		/* Granite wall (inner) */
	      case '#':
		cave_feat[y][x] = FEAT_WALL_INNER;
		break;

		/* Rubble. */
	      case ':':
		cave_feat[y][x] = FEAT_RUBBLE;
		break;

		/* Magma. */
	      case '&':
		cave_feat[y][x] = FEAT_MAGMA;
		break;

		/* Quartz. */
	      case '$':
		cave_feat[y][x] = FEAT_QUARTZ;
		break;

		/* Permanent wall (inner) */
	      case 'X':
		cave_feat[y][x] = FEAT_PERM_INNER;
		break;

		/* Quest Entrance */
	      case 'Q':
		cave_feat[y][x] = FEAT_QUEST_ENTER;
		break;

		/* Quest Exit -KMW- */
	      case 'E':
		cave_feat[y][x] = FEAT_QUEST_EXIT;
		break;

		/* Stairs up (exit without completing) -KMW- */
	      case '<':
		cave_feat[y][x] = FEAT_LESS;
		break;

		/* Stairs down */
	      case '>':
		cave_feat[y][x] = FEAT_MORE;
		break;

		/* Random altar. */
	      case 'O':
		place_altar(y, x);
		break;

		/* Grass. */
	      case 'A':
		cave_feat[y][x] = FEAT_GRASS;
		break;

		/* Swamp. */
	      case 'B':
		cave_feat[y][x] = FEAT_SWAMP;
		break;

		/* Mud. */
	      case 'C':
		cave_feat[y][x] = FEAT_MUD;
		break;

		/* Shrub. */
	      case 'H':
		cave_feat[y][x] = FEAT_SHRUB;
		break;

		/* Rocky hill */
	      case 'I':
		cave_feat[y][x] = FEAT_ROCKY_HILL;
		break;

		/* Water (shallow) -KMW- */
	      case 'V':
		cave_feat[y][x] = FEAT_SHAL_WATER;
		break;

		/* Water (deep) -KMW- */
	      case 'W':
		cave_feat[y][x] = FEAT_DEEP_WATER;
		break;

		/* Fog. */
	      case 'J':
		cave_feat[y][x] = FEAT_FOG;
		break;

		/* Lava (shallow) -KMW- */
	      case 'K':
		cave_feat[y][x] = FEAT_SHAL_LAVA;
		break;

		/* Lava (deep) -KMW- */
	      case 'L':
		cave_feat[y][x] = FEAT_DEEP_LAVA;
		break;

		/* Chaos fog */
	      case 'F':
		cave_feat[y][x] = FEAT_CHAOS_FOG;
		break;

		/* Glyph of warding */
	      case ';':
		/* Always permanent */
		cave_info[y][x] |= (CAVE_ICKY);
		cave_feat[y][x] = FEAT_GLYPH;
		break;

		/* Trees */
	      case 'Y':
		cave_feat[y][x] = FEAT_TREES;
		break;

		/* Treasure/trap */
	      case '*':
		if (rand_int(100) < 50) {
		  place_trap(y, x);
		}
		break;

		/* Secret doors */
	      case '+':
		place_secret_door(y, x);
		break;

		/* Regular doors */
	      case 'D':
		/* Unlocked doors in the town. */
		if (town_symb)
		  cave_feat[y][x] = FEAT_DOOR_HEAD;
		else
		  cave_feat[y][x] = FEAT_DOOR_HEAD + randint(4);
		break;

		/* Trap */
	      case '^':
		place_trap(y, x);
		break;

		/* Generator */
	      case 'G':
		if (v_ptr->mon[0]) {
		  create_generator(v_ptr->mon[0], y, x);
		}
		break;

	      case 'M':
		cave_feat[y][x] = FEAT_MOUNTAIN;
		break;

		/* Store exit. */
	      case 'S':
		cave_feat[y][x] = FEAT_STORE_EXIT;
		break;

		/* Shaft. */
	      case 'U':
		cave_feat[y][x] = FEAT_SHAFT;
		break;
	      }
	    }


	    /* Advance. */
	    number--;

	    /* End of a run. */
	    if (!number) {
	      t += 2;

	      datum = t[0];
	      number = t[1];
	    }
	  }
	}


	/* Get the first chunk of data. */
	datum = t2[0];
	number = t2[1];

	/* Place dungeon monsters and objects. */
	for (dy = 0; dy < ymax; dy++) {
	  for (dx = 0; dx < xmax; dx++) {


	    /* Extract the location */
	    x = xval - (xmax / 2) + dx;
	    y = yval - (ymax / 2) + dy;


	    /* 
	     * Place some monsters/objects. 
	     */
	    if (datum != ' ' && datum != '-') {

	      /* Monsters, 0-9 */
	      if (isdigit(datum)) {
		int i = v_ptr->mon[(datum - '0')];

		/* What a disgusting hack -- allow unfair monsters in */
		/* vaults and such. */
		bool um_opt = unfair_monsters;

		unfair_monsters = TRUE;
		place_monster_aux(y, x, i, mode);
		unfair_monsters = um_opt;
	      }

	      /* Monsters, a-z, A-Z */
	      if (isalpha(datum)) {
		s16b r_idx;

		hook_vault_monster_param = datum;
		get_mon_num_hook = hook_vault_monster;
		get_mon_num_prep();

		r_idx = get_mon_num(p_ptr->depth);

		if (r_idx) {
		  /* What a disgusting hack -- allow unfair monsters in */
		  /* vaults and such. */
		  bool um_opt = unfair_monsters;

		  unfair_monsters = TRUE;
		  place_monster_aux(y, x, r_idx, mode);
		  unfair_monsters = um_opt;
		}

		get_mon_num_hook = NULL;
		get_mon_num_prep();
	      }

	      /* Place the object with that picture. */
	      if (strchr("!\"$(),~'/=?[\\]_{|}", datum)) {
		s16b k_idx;

		hook_vault_object_param = datum;
		get_obj_num_hook = hook_vault_object;
		get_obj_num_prep();

		k_idx = get_obj_num(p_ptr->depth);

		if (k_idx) {
		  object_type *o_ptr = new_object();

		  object_prep(o_ptr, k_idx);
		  apply_magic(o_ptr, p_ptr->depth, TRUE, FALSE, FALSE);

		  floor_carry(y, x, o_ptr);
		}

		get_obj_num_hook = NULL;
		get_obj_num_prep();
	      }

	      switch (datum) {
		/* Treasure/trap */
	      case '*':
		if (rand_int(100) < 50) {
		  place_object(y, x, FALSE, FALSE);
		}
		break;

		/* Treasure -KMW- (Was: 'T') */
	      case '.':
		if (rand_int(100) < 75) {
		  place_object(y, x, FALSE, FALSE);

		} else if (rand_int(100) < 80) {
		  place_object(y, x, TRUE, FALSE);

		} else {
		  place_object(y, x, TRUE, TRUE);
		}
		break;

		/* [Arena] Monster */
	      case '&':
		if (town_symb) {
		  place_monster_aux(y, x, 
				    arena_monsters[p_ptr->which_arena]
				    [p_ptr->arena_number[p_ptr->which_arena]],
				    MON_ALLOC_ARENA | MON_ALLOC_JUST_ONE);
		} else {
		  monster_level = p_ptr->depth + 5;
		  place_monster(y, x, mode);
		  monster_level = p_ptr->depth;
		}
		break;

		/* Meaner monster (Was: '@') */
	      case ';':
		monster_level = p_ptr->depth + 11;
		place_monster(y, x, mode);
		monster_level = p_ptr->depth;
		break;

		/* Meaner monster, plus treasure (Was: '9') */
	      case '#':
		monster_level = p_ptr->depth + 9;
		place_monster(y, x, mode);
		monster_level = p_ptr->depth;
		object_level = p_ptr->depth + 7;
		place_object(y, x, TRUE, FALSE);
		object_level = p_ptr->depth;
		break;

		/* Nasty monster and treasure (Was: '8') */
	      case '^':
		monster_level = p_ptr->depth + 40;
		place_monster(y, x, mode);
		monster_level = p_ptr->depth;
		object_level = p_ptr->depth + 20;
		place_object(y, x, TRUE, TRUE);
		object_level = p_ptr->depth;
		break;

		/* Monster and/or object (Was: ',') */
	      case ':':
		if (rand_int(100) < 50) {
		  monster_level = p_ptr->depth + 3;
		  place_monster(y, x, mode);
		  monster_level = p_ptr->depth;
		}
		
		if (rand_int(100) < 50) {
		  object_level = p_ptr->depth + 7;
		  place_object(y, x, FALSE, FALSE);
		  object_level = p_ptr->depth;
		}
		break;

		/* Player position for quests (Was: 'P') */
	      case '@':
		if (p_ptr->inside_special != SPECIAL_WILD ||
		    hook_vault_place_player) {
		  player_place(y, x);
		}

		break;
	      }
	    }


	    /* Advance. */
	    number--;

	    /* End of a run. */
	    if (!number) {
	      t2 += 2;

	      datum = t2[0];
	      number = t2[1];
	    }

	  }
	}

	if (seed_dungeon)
	{
		Rand_quick = TRUE;
	}
}



/*
 * Type 7 -- simple vaults (see "v_info.txt")
 */
static void build_type7(int yval, int xval)
{
	vault_type *v_ptr;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Access a random vault record */
		v_ptr = &v_info[rand_int(MAX_V_IDX)];

		/* Accept the first lesser vault */
		if (v_ptr->typ == 7)
			break;
	}

	/* Message */
	if (cheat_room)
		msg_print("Lesser Vault");

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
		(randint((p_ptr->depth - 40) * (p_ptr->depth - 40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr);
}



/*
 * Type 8 -- greater vaults (see "v_info.txt")
 */
static void build_type8(int yval, int xval)
{
	vault_type *v_ptr;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Access a random vault record */
		v_ptr = &v_info[rand_int(MAX_V_IDX)];

		/* Accept the first greater vault */
		if (v_ptr->typ == 8)
			break;
	}

	/* Message */
	if (cheat_room)
		msg_print("Greater Vault");

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
		(randint((p_ptr->depth - 40) * (p_ptr->depth - 40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr);
}


/*
 * Type 9 -- themed vaults (see "v_info.txt")
 */
static void build_type9(int yval, int xval)
{
	vault_type *v_ptr;
	int vindex;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Access a random vault record */
		vindex = rand_int(MAX_V_IDX);
		v_ptr = &v_info[vindex];

		/* Accept the first greater vault */
		if (v_ptr->typ == 9)
			break;
	}

	/* Message */
	if (cheat_room)
		msg_format("Themed Vault %d", vindex);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
		(randint((p_ptr->depth - 40) * (p_ptr->depth - 40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr);
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
		if (main_loop_count++ > 2000)
			break;

		/* Allow bends in the tunnel */
		if (rand_int(100) < DUN_TUN_CHG)
		{
			/* Acquire the correct direction */
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
			/* Acquire the correct direction */
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
		if (cave_feat[tmp_row][tmp_col] == FEAT_PERM_SOLID)
			continue;

		/* Avoid the edge of vaults */
		if (cave_feat[tmp_row][tmp_col] == FEAT_PERM_OUTER)
			continue;

		/* Avoid "solid" granite walls */
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_SOLID)
			continue;

		/* Pierce "outer" walls of rooms */
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_OUTER)
		{
			/* Acquire the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Hack -- Avoid outer/solid permanent walls */
			if (cave_feat[y][x] == FEAT_PERM_SOLID)
				continue;
			if (cave_feat[y][x] == FEAT_PERM_OUTER)
				continue;

			/* Hack -- Avoid outer/solid granite walls */
			if (cave_feat[y][x] == FEAT_WALL_OUTER)
				continue;
			if (cave_feat[y][x] == FEAT_WALL_SOLID)
				continue;

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
						cave_feat[y][x] = FEAT_WALL_SOLID;
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
				if (tmp_row < 0)
					tmp_row = (-tmp_row);

				/* Distance between col1 and start_col */
				tmp_col = col1 - start_col;
				if (tmp_col < 0)
					tmp_col = (-tmp_col);

				/* Terminate the tunnel */
				if ((tmp_row > 10) || (tmp_col > 10))
					break;
			}
		}
	}


	/* Turn the tunnel into corridor */
	for (i = 0; i < dun->tunn_n; i++)
	{
		/* Access the grid */
		y = dun->tunn[i].y;
		x = dun->tunn[i].x;

		/* Clear previous contents, add a floor */
		cave_feat[y][x] = FEAT_FLOOR;
	}


	/* Apply the piercings that we found */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Access the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* Convert to floor grid */
		cave_feat[y][x] = FEAT_FLOOR;

		/* Occasional doorway */
		if (rand_int(100) < DUN_TUN_PEN)
		{
			/* Place a random door */
			place_random_door(y, x);
		}
	}
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
		if (!cave_floor_bold(y, x))
			continue;

		/* Skip non "empty floor" grids */
		if (cave_feat[y][x] != FEAT_FLOOR)
			continue;

		/* Skip grids inside rooms */
		if (cave_info[y][x] & (CAVE_ROOM))
			continue;

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
		if ((cave_feat[y - 1][x] >= FEAT_MAGMA) &&
			(cave_feat[y + 1][x] >= FEAT_MAGMA))
		{
			return (TRUE);
		}

		/* Check Horizontal */
		if ((cave_feat[y][x - 1] >= FEAT_MAGMA) &&
			(cave_feat[y][x + 1] >= FEAT_MAGMA))
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
	if (!in_bounds(y, x))
		return;

	/* Ignore walls */
	if (cave_feat[y][x] >= FEAT_MAGMA)
		return;

	/* Ignore room grids */
	if (cave_info[y][x] & (CAVE_ROOM))
		return;

	/* Occasional door (if allowed) */
	if ((rand_int(100) < DUN_TUN_JCT) && possible_doorway(y, x))
	{
		/* Place a door */
		place_random_door(y, x);
	}
}




/*
 * Attempt to build a room of the given type at the given block
 *
 * Note that we restrict the number of "crowded" rooms to reduce
 * the chance of overflowing the monster list during level creation.
 */
static bool room_build(int y0, int x0, int typ)
{
	int y, x, y1, x1, y2, x2;


	/* Restrict level */
	if (p_ptr->depth < room[typ].level)
		return (FALSE);

	/* Restrict "crowded" rooms */
	if (dun->crowded && ((typ == 5) || (typ == 6)))
		return (FALSE);

	/* Extract blocks */
	y1 = y0 + room[typ].dy1;
	y2 = y0 + room[typ].dy2;
	x1 = x0 + room[typ].dx1;
	x2 = x0 + room[typ].dx2;

	/* Never run off the screen */
	if ((y1 < 0) || (y2 >= dun->row_rooms))
		return (FALSE);
	if ((x1 < 0) || (x2 >= dun->col_rooms))
		return (FALSE);

	/* Verify open space */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			if (dun->room_map[y][x])
				return (FALSE);
		}
	}

	/* XXX XXX XXX It is *extremely* important that the following */
	/* calculation is *exactly* correct to prevent memory errors */

	/* Acquire the location of the room */
	y = ((y1 + y2 + 1) * BLOCK_HGT) / 2;
	x = ((x1 + x2 + 1) * BLOCK_WID) / 2;

	/* Build a room */
	switch (typ)
	{
			/* Build an appropriate room */
		case 8:
			build_type8(y, x);
			break;
		case 7:
			build_type7(y, x);
			break;
		case 9:
			build_type9(y, x);
			break;
		case 6:
			build_type6(y, x);
			break;
		case 5:
			build_type5(y, x);
			break;
		case 4:
			build_type4(y, x);
			break;
		case 3:
			build_type3(y, x);
			break;
		case 2:
			build_type2(y, x);
			break;
		case 1:
			build_type1(y, x);
			break;

			/* Paranoia */
		default:
			return (FALSE);
	}

	/* Save the room location */
	if (dun->cent_n < CENT_MAX)
	{
		dun->cent[dun->cent_n].y = y;
		dun->cent[dun->cent_n].x = x;
		dun->cent_n++;
	}

	/* Reserve some blocks */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			dun->room_map[y][x] = TRUE;
		}
	}

	/* Count "crowded" rooms */
	if ((typ == 5) || (typ == 6))
		dun->crowded = TRUE;

	/* Success */
	return (TRUE);
}


/*
 * Helper for plasma generation.
 */

static void perturb_point_mid(int x1, int x2, int x3, int x4, int xmid,
	int ymid, int rough, int depth_max)
{
	/* Average the four corners & perturb it a bit. */
	/* tmp is a random int +/- rough */
	int tmp2 = rough * 2 + 1;
	int tmp = randint(tmp2) - (rough + 1);

	int avg = ((x1 + x2 + x3 + x4) / 4) + tmp;

	/* Round up if needed. */
	if (((x1 + x2 + x3 + x4) % 4) > 1)
		avg++;

	/* Normalize */
	if (avg < 0)
		avg = 0;
	if (avg > depth_max)
		avg = depth_max;

	/* Set the new value. */
	cave_feat[ymid][xmid] = avg;
}


static void perturb_point_end(int x1, int x2, int x3, int xmid, int ymid,
	int rough, int depth_max)
{
	/* Average the three corners & perturb it a bit. */
	/* tmp is a random int +/- rough */
	int tmp2 = rough * 2 + 1;
	int tmp = randint(tmp2) - (rough + 1);

	int avg = ((x1 + x2 + x3) / 3) + tmp;

	/* Round up if needed. */
	if ((x1 + x2 + x3) % 3)
		avg++;

	/* Normalize */
	if (avg < 0)
		avg = 0;
	if (avg > depth_max)
		avg = depth_max;

	/* Set the new value. */
	cave_feat[ymid][xmid] = avg;
}


/*
 * A generic function to generate the plasma fractal.
 * Note that it uses ``cave_feat'' as temporary storage.
 * The values in ``cave_feat'' after this function
 * are NOT actual features; They are raw heights which
 * need to be converted to features.
 *
 *  A-----U-----B
 *  |           |
 *  L     M     R
 *  |           |
 *  C-----D-----D
 *
 * A, B, C, and D are given; the other five we calculate by
 * averaging the neighbors and adding a random offset.
 */

static void plasma_recursive(int x1, int y1, int x2, int y2, int depth_max,
	int rough)
{

	/* Find middle */
	int xmid = (x2 - x1) / 2 + x1;
	int ymid = (y2 - y1) / 2 + y1;

	/* Are we done? */
	if (x1 + 1 == x2)
	{
		return;
	}

	/* Calculate M */
	perturb_point_mid(cave_feat[y1][x1], cave_feat[y2][x1],
		cave_feat[y1][x2], cave_feat[y2][x2], xmid, ymid, rough,
		depth_max);

	/* Calculate U */
	perturb_point_end(cave_feat[y1][x1], cave_feat[y1][x2],
		cave_feat[ymid][xmid], xmid, y1, rough, depth_max);

	/* Calculate R */
	perturb_point_end(cave_feat[y1][x2], cave_feat[y2][x2],
		cave_feat[ymid][xmid], x2, ymid, rough, depth_max);

	/* Calculate B */
	perturb_point_end(cave_feat[y2][x2], cave_feat[y2][x1],
		cave_feat[ymid][xmid], xmid, y2, rough, depth_max);

	/* Calculate L */
	perturb_point_end(cave_feat[y2][x1], cave_feat[y1][x1],
		cave_feat[ymid][xmid], x1, ymid, rough, depth_max);


	/* Recurse the four quadrants */
	plasma_recursive(x1, y1, xmid, ymid, depth_max, rough);
	plasma_recursive(xmid, y1, x2, ymid, depth_max, rough);
	plasma_recursive(x1, ymid, xmid, y2, depth_max, rough);
	plasma_recursive(xmid, ymid, x2, y2, depth_max, rough);

}


/*
 * The default table in terrain level generation.
 */

static int terrain_table[2][22] = {
	/* Normal terrain table. */
	{
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,

			FEAT_MUD,
			FEAT_MUD,

			FEAT_SWAMP,
			FEAT_SWAMP,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,

			FEAT_SHRUB,
			FEAT_SHRUB,

			FEAT_TREES,
			FEAT_TREES,

			FEAT_ROCKY_HILL,

			FEAT_MOUNTAIN,
		},

	/* The watery terrain table. */
	{
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,

			FEAT_MUD,
			FEAT_MUD,
			FEAT_MUD,

			FEAT_SWAMP,
			FEAT_SWAMP,
			FEAT_SWAMP,

			FEAT_GRASS,
			FEAT_GRASS,

			FEAT_SHRUB,
		}
};



/*
 * The opposite procedure of the above table.
 */
static byte table_backwards(int feat, int type)
{
	switch (type)
	{
		case 0:

			switch (feat)
			{
				case FEAT_DEEP_WATER:
					return 0;
				case FEAT_SHAL_WATER:
					return 4;
				case FEAT_MUD:
					return 9;
				case FEAT_SWAMP:
					return 11;
				case FEAT_GRASS:
					return 13;
				case FEAT_SHRUB:
					return 16;
				case FEAT_TREES:
					return 18;
				case FEAT_ROCKY_HILL:
					return 20;
				case FEAT_MOUNTAIN:
					return 21;
				default:
					return 11;
			}

		case 1:

			switch (feat)
			{
				case FEAT_DEEP_WATER:
					return 0;
				case FEAT_SHAL_WATER:
					return 9;
				case FEAT_MUD:
					return 13;
				case FEAT_SWAMP:
					return 16;
				case FEAT_GRASS:
					return 19;
				case FEAT_SHRUB:
					return 21;
				default:
					return 11;
			}
	}

	/* Paranoia. */
	return 11;
}


/*
 * Create the light in the town -- i.e. handle day/night
 */
static void lite_up_town(bool daytime)
{
	int x, y, f, flg;

	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Interesting grids */
			if (daytime || !cave_boring_bold(y, x))
			{
				/* Illuminate the grid */
				cave_info[y][x] |= (CAVE_GLOW);

				/* Wiz-lite if appropriate. */
				if (wiz_lite_town && 
				    p_ptr->wild_x == 0 && p_ptr->wild_y == 0)
				{
					/* Memorize the grid */
					cave_info[y][x] |= (CAVE_MARK);
				}
			}
		}
	}

	flg = CAVE_GLOW;
	if (wiz_lite_town)
		flg |= CAVE_MARK;

	/* Now add light in appropriate places */
	for (y = 1; y < DUNGEON_HGT - 1; y++)
	{
		for (x = 1; x < DUNGEON_WID - 1; x++)
		{
			/* If this is a shop, light it and surrounding squares */
			f = cave_feat[y][x];
			if ((f >= FEAT_SHOP_HEAD && f <= FEAT_SHOP_TAIL) ||
				(f >= FEAT_BLDG_HEAD && f <= FEAT_BLDG_TAIL) ||
				(f == FEAT_STORE_EXIT))
			{
				cave_info[y - 1][x - 1] |= flg;
				cave_info[y - 1][x] |= flg;
				cave_info[y - 1][x + 1] |= flg;
				cave_info[y][x - 1] |= flg;
				cave_info[y][x] |= flg;
				cave_info[y][x + 1] |= flg;
				cave_info[y + 1][x - 1] |= flg;
				cave_info[y + 1][x] |= flg;
				cave_info[y + 1][x + 1] |= flg;
			}
		}
	}
}


/*
 * Generate a terrain level using ``plasma'' fractals.
 *
 */
static void terrain_gen(void) {

  int i, k;
  int x, y;
  int depth;
  int table_type = 0;
  int table_size;
  int roughness;
  int level_bg;
  int scroll = 0;
  bool quick_prev = Rand_quick;
  u32b value_prev = Rand_value;

  bool daytime = FALSE;

  if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2)) {
    daytime = TRUE;
  }


  /* HACK!!! */
  /* This is a total hack -- (0, 0) appears only right after birth. */
  if (p_ptr->px == 0 && p_ptr->py == 0) {
    hook_vault_place_player = TRUE;
  }

  /* Hack -- implement scrolly terrains. */
  if (!hook_vault_place_player && 
      ((p_ptr->py <= 2 || p_ptr->py >= DUNGEON_HGT - 3) || 
       (p_ptr->px <= 2 || p_ptr->px >= DUNGEON_WID - 3))) {

    /* Scroll up. */
    if (p_ptr->py <= 2) {
      scroll = 1;
      p_ptr->wild_y--;

      /* Scroll down. */
    } else if (p_ptr->py >= DUNGEON_HGT - 3) {
      scroll = 2;
      p_ptr->wild_y++;

      /* Scroll left. */
    } else if (p_ptr->px <= 2) {
      scroll = 3;
      p_ptr->wild_x--;

      /* Scroll right. */
    } else {
      scroll = 4;
      p_ptr->wild_x++;
    }
  }



  /* Initialize the four corners.
   * The corners are generated as permanent separately from the 
   * contents. This means that the wilderness is fairly "tileable".
   *
   * Again, the contents of the code below were taken arbitrarily,
   * I've no clear idea how predictable such a hashing function would be.
   */

  /* Set some generation parameters. */

  /* Table of terrain types, one for each depth. */
  /*
   * if (magik(30)) {
   * if (cheat_room) msg_print("Watery terrain level.");
   * }
   */

  table_size = 22;
  level_bg = 11;

  /* The roughness of the level. */
  roughness = 1;

  Rand_quick = TRUE;

#define HASH_CORNERS(X, Y) (((X) - (Y)) ^ (((X) + seed_wild) & (Y)))
#define HASH_LEVEL(X, Y)   (((Y) - (X)) ^ ((Y) & ((X) + seed_wild)))
  
  Rand_value = HASH_CORNERS(p_ptr->wild_x, p_ptr->wild_y);
  cave_feat[1][1] = rand_int(table_size);

  Rand_value = HASH_CORNERS(p_ptr->wild_x, p_ptr->wild_y + 1);
  cave_feat[DUNGEON_HGT - 2][1] = rand_int(table_size);

  Rand_value = HASH_CORNERS(p_ptr->wild_x + 1, p_ptr->wild_y);
  cave_feat[1][DUNGEON_WID - 2] = rand_int(table_size);

  Rand_value = HASH_CORNERS(p_ptr->wild_x + 1, p_ptr->wild_y + 1);
  cave_feat[DUNGEON_HGT - 2][DUNGEON_WID - 2] = rand_int(table_size);


  /* Terrain levels are always ``permanent''. 
   * Note the random bit shuffling to make a unique seed.
   * There's no real rationale behind this. Anyone know a good hashing
   * function for pairs of numbers? */
  Rand_quick = TRUE;
  Rand_value = HASH_LEVEL(p_ptr->wild_x, p_ptr->wild_y);

  /* Clear the rest of the level. */
  for (y = 2; y < DUNGEON_HGT - 2; y++) {
    for (x = 2; x < DUNGEON_WID - 2; x++) {

      /* Create level background */
      cave_feat[y][x] = level_bg;
    }
  }

  /* x1, y1, x2, y2, num_depths, roughness */

  plasma_recursive(1, 1, DUNGEON_WID - 2, DUNGEON_HGT - 2,
		   table_size - 1, roughness);



  /* Light all the grids.
   * HACK -- Assume that the starting town is known to the player
   * if "wiz_lite_town" is set.
   */

  for (y = 1; y < DUNGEON_HGT - 1; y++) {
    for (x = 1; x < DUNGEON_WID - 1; x++) {

      cave_feat[y][x] = terrain_table[table_type][cave_feat[y][x]];

      /* All grids are lit. */
      if (daytime) {
	cave_info[y][x] |= (CAVE_GLOW);

	if (wiz_lite_town && p_ptr->wild_x == 0 && p_ptr->wild_y == 0) {
	  cave_info[y][x] |= (CAVE_MARK);
	}
      }

      /* Nasty hack to allow pseudo-rooms */
      if (cave_floor_bold(y, x)) {
	cave_info[y][x] |= (CAVE_ROOM);
      }
    }
  }


  /* Special boundary walls -- Top */
  for (x = 0; x < DUNGEON_WID; x++) {
    y = 0;
    cave_feat[y][x] = FEAT_UNSEEN;
  }

  /* Special boundary walls -- Bottom */
  for (x = 0; x < DUNGEON_WID; x++) {
    y = DUNGEON_HGT - 1;
    cave_feat[y][x] = FEAT_UNSEEN;
  }

  /* Special boundary walls -- Left */
  for (y = 0; y < DUNGEON_HGT; y++) {
    x = 0;
    cave_feat[y][x] = FEAT_UNSEEN;
  }

  /* Special boundary walls -- Right */
  for (y = 0; y < DUNGEON_HGT; y++) {
    x = DUNGEON_WID - 1;
    cave_feat[y][x] = FEAT_UNSEEN;
  }

  /* Place a stairway down, sometimes. */
  if (rand_int(100) < DUN_WILD_STAIRS) {
    alloc_stairs(FEAT_SHAFT, 1, 0);
  }


  /* Mega-hack: Pick a new depth. */
  if (scroll) {
    if (p_ptr->wild_x == 0 && p_ptr->wild_y == 0) {
      depth = 0;

    } else {
      depth = p_ptr->depth + randnor(0, 3);

      if (depth < 0)
	depth = 0;

      if (depth >= MAX_DEPTH)
	depth = MAX_DEPTH - 1;
    }

    p_ptr->depth = depth;

  } else {
    p_ptr->depth = p_ptr->wilderness_depth;
  }

  /* Generate a wilderness vault. (Or town) */
  if (magik(DUN_WILD_VAULT) || p_ptr->depth == 0) {
    int number = (p_ptr->depth ? randnor(0, 1) : 1);

    if (number < 0) {
      number = -number;
    }

    if (number == 0) {
      number = 1;
    }

    while (number) {
      vault_type *v_ptr = NULL;
      int vindex = -1, vy, vx;
      int i;

      /* Pick a wilderness vault */
      if (p_ptr->wild_x == 0 && p_ptr->wild_y == 0) {
	vindex = p_ptr->which_town;
	v_ptr = &v_info[vindex];

      } else {
	for (i = 0; i < 1000; i++) {
	  /* Access a random vault record */
	  vindex = rand_int(MAX_V_IDX);
	  v_ptr = &v_info[vindex];

	  /* Accept the first vault */
	  if (v_ptr->typ == (p_ptr->depth ? 13 : 10))
	    break;
	}
      }

      /* Message */
      if (cheat_room)
	msg_format("Wilderness Vault %d", vindex);

      /* Boost the rating */
      rating += v_ptr->rat;

      vy = rand_range((v_ptr->hgt / 2) + 1,
		      DUNGEON_HGT - (v_ptr->hgt / 2) - 1);
      vx = rand_range((v_ptr->wid / 2) + 1,
		      DUNGEON_WID - (v_ptr->wid / 2) - 1);

      /* Redundant code -- turn off persistent levels. */
      Rand_quick = FALSE;

      build_vault(vy, vx, v_ptr);

      number--;
    }
  }


  if (hook_vault_place_player) {
    hook_vault_place_player = FALSE;

  } else {
    /* Find a new place for the player. */
    switch (scroll) {
    case 1:
      p_ptr->py = DUNGEON_HGT - 3;
      old_player_spot();
      break;

    case 2:
      p_ptr->py = 2;
      old_player_spot();
      break;

    case 3:
      p_ptr->px = DUNGEON_WID - 3;
      old_player_spot();
      break;

    case 4:
      p_ptr->px = 2;
      old_player_spot();
      break;

    default:
      if (p_ptr->wilderness_px > 0 && p_ptr->wilderness_py > 0) {
	p_ptr->px = p_ptr->wilderness_px;
	p_ptr->py = p_ptr->wilderness_py;

	old_player_spot();

      } else {
	new_player_spot();
      }
      break;
    }
  }


  /* Turn off persistent levels. */
  Rand_quick = FALSE;


  /* Reset the monster generation level */
  monster_level = p_ptr->depth;

  /* Reset the object generation level */
  object_level = p_ptr->depth;

  /* Basic "amount" */
  k = (p_ptr->depth / 3);
  if (k > 10)
    k = 10;
  if (k < 2)
    k = 2;

  /* Pick a base number of monsters */
  i = ((daytime ? MIN_M_ALLOC_WILD_DAY : MIN_M_ALLOC_WILD_NIGHT) +
       randint(4));

  /* Put some monsters in the dungeon */
  for (i = i + k; i > 0; i--) {
    alloc_monster(0, 0);
  }

  /* Put some water dwellers. 
   * Yes, that's right -- this code assumes that all aquatic 
   * monsters are nocturnal. */
  i = MIN_M_ALLOC_WILD_NIGHT + randint(4);

  for (i = i + k; i > 0; i--) {
    alloc_monster(0, MON_ALLOC_AQUATIC);
  }

  /* Put some objects in rooms */
  alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, 
	       randnor(DUN_AMT_ROOM, 3));

  /* Put some altars */
  alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_ALTAR, 
	       randnor(DUN_AMT_ALTAR, 3));

  /* Put some objects/gold in the dungeon */

  alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, 
	       randnor(DUN_AMT_ITEM, 3));

  Rand_quick = quick_prev;
  Rand_value = value_prev;
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static void cave_gen(void)
{
	int i, k, y, x, y1, x1;

	bool destroyed = FALSE;

	bool lit_level = FALSE;

	dun_data dun_body;

	byte level_bg = FEAT_WALL_EXTRA;
	s16b dun_rooms = DUN_ROOMS;

	/* Global data */
	dun = &dun_body;

	/* Allow open levels. */

	if (allow_open_levels)
	{
		int chance1 = DUN_OPEN_FLOOR;
		int chance2 = DUN_OPEN_WATER;
		int chance3 = DUN_OPEN_CHAOS;
		int chance4 = DUN_OPEN_MAZE;
		int chance5 = DUN_OPEN_FOG;

		if (weirdness_is_rare)
		{
			chance1 /= 2;
			chance2 /= 2;
			chance3 /= 2;
			chance4 /= 2;
			chance5 /= 2;
		}

		if (magik(chance1))
		{
			level_bg = FEAT_FLOOR;
			lit_level = TRUE;
		}
		else if (magik(chance2))
		{
			level_bg = FEAT_SHAL_WATER;
			lit_level = TRUE;
		}
		else if (magik(chance3))
		{
			level_bg = FEAT_CHAOS_FOG;
		}
		else if (magik(chance4))
		{
			level_bg = FEAT_NONE;
			lit_level = TRUE;
		}
		else if (magik(chance5))
		{
			level_bg = FEAT_FOG;
		}
	}

	/* Hack -- Start with basic granite (or not) */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			byte bg = level_bg;

			/* Hack -- try to make something random if unspecified. */
			if (!level_bg)
			{
				switch ((x + y + randint(12)) % 12)
				{
					case 0:
					case 1:
					case 2:
					case 3:
					case 4:
					case 5:
					case 6:
					case 7:
					case 8:
						bg = FEAT_FLOOR;
						break;
					case 9:
						bg = FEAT_WALL_EXTRA;
						break;
					case 10:
						bg = FEAT_QUARTZ;
						break;
					case 11:
						bg = FEAT_MAGMA;
						break;
				}
			}

			/* Create level background */
			cave_feat[y][x] = bg;
		}
	}

	/* Possible "destroyed" level */
	if ((p_ptr->depth > 10) && (rand_int(DUN_DEST) == 0))
		destroyed = TRUE;

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = DUNGEON_HGT / BLOCK_HGT;
	dun->col_rooms = DUNGEON_WID / BLOCK_WID;

	/* Initialize the room table */
	for (y = 0; y < dun->row_rooms; y++)
	{
		for (x = 0; x < dun->col_rooms; x++)
		{
			dun->room_map[y][x] = FALSE;
		}
	}

	/* No "crowded" rooms yet */
	dun->crowded = FALSE;


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
			if ((x % 3) == 0)
				x++;

			/* Slide some rooms left */
			if ((x % 3) == 2)
				x--;
		}

		/* Destroyed levels are boring */
		if (destroyed)
		{
			/* Attempt a "trivial" room */
			if (room_build(y, x, 1))
				continue;

			/* Never mind */
			continue;
		}


		/* Attempt a themed vault */
		if (allow_theme_vaults)
		{
			int chance = 70;

			if (weirdness_is_rare)
				chance = 10;

			if (magik(chance))
			{
				if (room_build(y, x, 9))
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
				if ((k < 10) && room_build(y, x, 8))
					continue;

				/* Type 7 -- Lesser vault (15%) */
				if ((k < 25) && room_build(y, x, 7))
					continue;

				/* Type 6 -- Monster pit (25%) */
				if ((k < 50) && room_build(y, x, 6))
					continue;

				/* Type 5 -- Monster nest (30%) */
				if ((k < 80) && room_build(y, x, 5))
					continue;
			}

			/* Type 4 -- Large room (25%) */
			if ((k < 25) && room_build(y, x, 4))
				continue;

			/* Type 3 -- Cross room (25%) */
			if ((k < 50) && room_build(y, x, 3))
				continue;

			/* Type 2 -- Overlapping (50%) */
			if ((k < 100) && room_build(y, x, 2))
				continue;
		}

		/* Attempt a trivial room */
		if (room_build(y, x, 1))
			continue;
	}

	/* Special boundary walls -- Top */
	for (x = 0; x < DUNGEON_WID; x++)
	{
		y = 0;

		/* Clear previous contents, add "solid" perma-wall */
		cave_feat[y][x] = FEAT_PERM_SOLID;
	}

	/* Special boundary walls -- Bottom */
	for (x = 0; x < DUNGEON_WID; x++)
	{
		y = DUNGEON_HGT - 1;

		/* Clear previous contents, add "solid" perma-wall */
		cave_feat[y][x] = FEAT_PERM_SOLID;
	}

	/* Special boundary walls -- Left */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		x = 0;

		/* Clear previous contents, add "solid" perma-wall */
		cave_feat[y][x] = FEAT_PERM_SOLID;
	}

	/* Special boundary walls -- Right */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		x = DUNGEON_WID - 1;

		/* Clear previous contents, add "solid" perma-wall */
		cave_feat[y][x] = FEAT_PERM_SOLID;
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
	y = dun->cent[dun->cent_n - 1].y;
	x = dun->cent[dun->cent_n - 1].x;

	/* Connect all the rooms together */
	for (i = 0; i < dun->cent_n; i++)
	{
		/* Connect the room to the previous room */
		build_tunnel(dun->cent[i].y, dun->cent[i].x, y, x);

		/* Remember the "previous" room */
		y = dun->cent[i].y;
		x = dun->cent[i].x;
	}

	/* Place intersection doors */
	if (level_bg == FEAT_WALL_EXTRA) {
	  for (i = 0; i < dun->door_n; i++) {
	    /* Extract junction location */
	    y = dun->door[i].y;
	    x = dun->door[i].x;

	    /* Try placing doors */
	    try_door(y, x - 1);
	    try_door(y, x + 1);
	    try_door(y - 1, x);
	    try_door(y + 1, x);
	  }
	}

	/* Hack -- Add some magma streamers */

	if (level_bg == FEAT_WALL_EXTRA)
	{
		for (i = 0; i < DUN_STR_MAG; i++)
		{
			build_streamer(FEAT_MAGMA, DUN_STR_MC);
		}

		/* Hack -- Add some quartz streamers */
		for (i = 0; i < DUN_STR_QUA; i++)
		{
			build_streamer(FEAT_QUARTZ, DUN_STR_QC);
		}
	}

	/* Destroy the level if necessary */
	if (destroyed)
		destroy_level();

	/* Add streamers of trees, water, or lava -KMW- */
	if ((p_ptr->depth <= 2) && (randint(20) > 15))
		for (i = 0; i < randint(DUN_STR_QUA); i++)
			build_streamer2(FEAT_TREES, 1);
	if ((p_ptr->depth <= 19) && (randint(20) > 15))
	{
		for (i = 0; i < randint(DUN_STR_QUA - 1); i++)
			build_streamer2(FEAT_SHAL_WATER, 0);
		if (randint(20) > 15)
		{
			for (i = 0; i < randint(DUN_STR_QUA); i++)
				build_streamer2(FEAT_DEEP_WATER, 1);
		}
	}
	else if ((p_ptr->depth > 19) && (randint(20) > 15))
	{
		for (i = 0; i < randint(DUN_STR_QUA); i++)
			build_streamer2(FEAT_SHAL_LAVA, 0);
		if (randint(20) > 15)
		{
			for (i = 0; i < randint(DUN_STR_QUA - 1); i++)
				build_streamer2(FEAT_DEEP_LAVA, 1);
		}
	}
	else if (randint(10) > 7)
	{
		for (i = 0; i < randint(DUN_STR_QUA); i++)
		{
			build_streamer2(FEAT_CHAOS_FOG, 1);
		}
	}

	/* Place 3 or 4 down stairs near some walls */
	alloc_stairs(FEAT_MORE, rand_range(3, 4), 3);

	/* Place 1 or 2 up stairs near some walls */
	alloc_stairs(FEAT_LESS, rand_range(1, 2), 3);

	/* Determine the character location */
	new_player_spot();

	/* Monsters and objects change even in persistent dungeons. */
	if (seed_dungeon)
	{
		Rand_quick = FALSE;
	}

	/* Basic "amount" */
	k = (p_ptr->depth / 3);
	if (k > 10)
		k = 10;
	if (k < 2)
		k = 2;


	/* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL + randint(8);


	/* Put some monsters in the dungeon */
	for (i = i + k; i > 0; i--)
	{
		/* Hack -- flooded levels get fishy inhabitants. */
		if (level_bg == FEAT_SHAL_WATER)
		{
			alloc_monster(0, MON_ALLOC_SLEEP | MON_ALLOC_AQUATIC);
		}

		alloc_monster(0, MON_ALLOC_SLEEP);
	}

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ROOM,
			3));

	/* Put some altars */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_ALTAR, randnor(DUN_AMT_ALTAR,
			3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ITEM,
			3));

	/* Do not light floors inside rooms. */
	/* Floors outside rooms or walls are lit. */

	if (lit_level)
	{
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{
				if (!(cave_info[y][x] & CAVE_ROOM) ||
					!(cave_floor_bold(y, x)))
				{
					cave_info[y][x] |= (CAVE_GLOW);
				}
			}
		}
	}
}


/*
 * Generate a shop.
 *
 * This is an ugly hack.
 */
static void store_gen(void)
{
	bool daytime;
	vault_type *v_ptr;
	int x, y, i, good_y = 0, good_x = 0;

	object_type *o_ptr = NULL;
	store_type *st_ptr = &store[p_ptr->s_idx];

	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
	{
		daytime = TRUE;
		/* Night time */
	}
	else
	{
		daytime = FALSE;
	}

	/* Start with rock */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			cave_feat[y][x] = FEAT_PERM_SOLID;
		}
	}

	v_ptr = &v_info[st_ptr->vault];

	y = (v_ptr->hgt / 2) + 2;
	x = (v_ptr->wid / 2) + 2;

	build_vault(y, x, v_ptr);

	/* Create the sun. */
	lite_up_town(daytime);

	/* Hack -- don't punish thefts. */
	hack_punish_theft = FALSE;

	/* Scatter the store's contents. */
	for (o_ptr = st_ptr->stock; o_ptr != NULL; o_ptr = o_ptr->next_global)
	{

		if (o_ptr->iy && o_ptr->ix)
		{
			floor_carry(o_ptr->iy, o_ptr->ix, o_ptr);

		}
		else
		{

			for (i = 0; i < 2000; i++)
			{
				y = randnor(y, 1);
				x = randnor(x, 1);

				if (in_bounds_fully(y, x) && cave_floor_bold(y, x))
				{

					good_y = y;
					good_x = x;

					if (cave_o_idx[y][x] == NULL || magik(25))
						break;
				}
			}


			if (good_y && good_x)
				floor_carry(good_y, good_x, o_ptr);

		}
	}

	/* Undo the hack. */
	hack_punish_theft = TRUE;

}





/*
 * Town logic flow for generation of arena -KMW-
 */
static void arena_gen(void)
{
	bool daytime;
	vault_type *v_ptr;
	int x, y;

	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
	{
		daytime = TRUE;
		/* Night time */
	}
	else
	{
		daytime = FALSE;
	}

	/* Start with rock */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			cave_feat[y][x] = FEAT_PERM_SOLID;
		}
	}

	v_ptr = &v_info[p_ptr->which_arena_layout];
	build_vault((v_ptr->hgt / 2) + 2, (v_ptr->wid / 2) + 2, v_ptr);

	/* Create the sun. */
	lite_up_town(daytime);
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
 *
 * Note that the above is completely false.
 */
static void town_gen(void) {

  p_ptr->inside_special = SPECIAL_WILD;

  terrain_gen();

  /* Grant Ghosts WoR */
  if (p_ptr->prace == RACE_GHOST && !p_ptr->prace_info) {
    p_ptr->prace_info = 1;
    p_ptr->max_depth = 0;
    msg_print("You return to your corporeal form.");
  }
}


/*
 * Generate a quest level -KMW-
 */
static void quest_gen(void)
{
	int x, y;
	vault_type *v_ptr;

	/* Paranoia */
	if (p_ptr->which_quest == 0)
		return;

	v_ptr = q_v_ptrs[p_ptr->which_quest - 1];

	/* Start with perm walls, if we want normal generation. */

	if (v_ptr->gen_info != 1)
	{
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{

				if (v_ptr->gen_info == 2)
				{
					cave_feat[y][x] = FEAT_FOG;
				}
				else
				{
					cave_feat[y][x] = FEAT_PERM_SOLID;
				}
			}
		}

	}
	else if (v_ptr->gen_info == 1)
	{
		p_ptr->wild_x = rand_range(-100, 100);
		p_ptr->wild_y = rand_range(-100, 100);
		terrain_gen();

	}

	/* Find a random place for the vault. */
	if (seed_dungeon)
	{
		Rand_quick = TRUE;
	}

	y =
		rand_range((v_ptr->hgt / 2) + 1,
		DUNGEON_HGT - (v_ptr->hgt / 2) - 1);
	x =
		rand_range((v_ptr->wid / 2) + 1,
		DUNGEON_WID - (v_ptr->wid / 2) - 1);

	if (seed_dungeon)
	{
		Rand_quick = FALSE;
	}

	build_vault(y, x, v_ptr);

	/* Quest is now officially in progress */
	quest_status[p_ptr->which_quest - 1] = QUEST_IN_PROGRESS;
}


/*
 * Generate a random dungeon level
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Hack -- allow auto-scumming via a gameplay option.
 *
 * Allow quasi-persistent dungeons using a seeded RNG.
 */
void generate_cave(void)
{
	int y, x, num;


	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Seed the RNG if appropriate */
	if (seed_dungeon)
	{
		Rand_quick = TRUE;
		Rand_value = seed_dungeon + p_ptr->depth;
	}

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;
		bool load = FALSE;

		cptr why = NULL;


		/* XXX XXX XXX XXX */
		o_max = 1;
		m_max = 1;

		/* Start with a blank cave */
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{
				/* No flags */
				cave_info[y][x] = 0;

				/* No objects */
				cave_o_idx[y][x] = 0;

				/* No monsters */
				cave_m_idx[y][x] = 0;

#ifdef MONSTER_FLOW
				/* No flow */
				cave_cost[y][x] = 0;
				cave_when[y][x] = 0;
#endif /* MONSTER_FLOW */

			}
		}


		/* Hack -- illegal panel */
		p_ptr->wy = DUNGEON_HGT;
		p_ptr->wx = DUNGEON_WID;


		/* Reset the monster generation level */
		monster_level = p_ptr->depth;

		/* Reset the object generation level */
		object_level = p_ptr->depth;

		/* Nothing special here yet */
		good_item_flag = FALSE;

		/* Nothing good here yet */
		rating = 0;
		pet_rating = 0;

		/* Restore an old dungeon. */
		if (p_ptr->load_dungeon)
		{
			if (load_dungeon(p_ptr->load_dungeon - 1))
			{
				mprint(MSG_ERROR, "Could not load temporary dungeon!");
			}
			else
			{
				load = TRUE;
			}

			p_ptr->load_dungeon = 0;

			/* Mega-hack: prevent autoscum. */
			num = 100;
		}

		if (!load)
		{
			/* Build the arena -KMW- */
			if (p_ptr->inside_special == SPECIAL_ARENA ||
			    p_ptr->inside_special == SPECIAL_MAGIC_ARENA)
			{
				arena_gen();
			}

			/* Quest levels -KMW- */
			else if (p_ptr->inside_special == SPECIAL_QUEST)
			{
				quest_gen();
			}

			/* Shop vault. */
			else if (p_ptr->inside_special == SPECIAL_STORE)
			{
				store_gen();
			}

			/* Build the wilderness */
			else if (p_ptr->inside_special == SPECIAL_WILD)
			{
				terrain_gen();
			}

			/* Build the town */
			else if (!p_ptr->depth)
			{
				/* Make a town */
				town_gen();
			}

			/* Build a real level */
			else
			{
			  cave_gen();
			}
		}

		/* Extract the feeling */
		if (rating > 100)
			feeling = 2;
		else if (rating > 80)
			feeling = 3;
		else if (rating > 60)
			feeling = 4;
		else if (rating > 40)
			feeling = 5;
		else if (rating > 30)
			feeling = 6;
		else if (rating > 20)
			feeling = 7;
		else if (rating > 10)
			feeling = 8;
		else if (rating > 0)
			feeling = 9;
		else
			feeling = 10;

		/* Normalize the pet rating. */
		if (pet_rating > 10)
			pet_rating = 10;

		/* Hack -- Have a special feeling sometimes */
		if (good_item_flag && !p_ptr->preserve)
			feeling = 1;

		/* It takes 1000 game turns for "feelings" to recharge */
		if ((turn - old_turn) < 1000)
			feeling = 0;

		/* Hack -- no feeling in the town */
		if (!p_ptr->depth)
			feeling = 0;


		/* Prevent object over-flow */
		if (o_max >= MAX_O_IDX)
		{
			/* Message */
			why = "too many objects";

			/* Message */
			okay = FALSE;
		}

		/* Prevent monster over-flow */
		if (m_max >= MAX_M_IDX)
		{
			/* Message */
			why = "too many monsters";

			/* Message */
			okay = FALSE;
		}

		/* Mega-Hack -- "auto-scum" */
		if (auto_scum && (num < 100) && !p_ptr->inside_special)
		{
			/* Require "goodness" */
			if ((feeling > 9) || ((p_ptr->depth >= 5) && (feeling > 8)) ||
				((p_ptr->depth >= 10) && (feeling > 7)) ||
				((p_ptr->depth >= 20) && (feeling > 6)) ||
				((p_ptr->depth >= 40) && (feeling > 5)))
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
		if (okay)
			break;


		/* Message */
		if (why)
			msg_format("Generation restarted (%s)", why);

		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the monsters */
		wipe_m_list();

	}

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Remember when this level was "created" */
	old_turn = turn;

	/* OK to summon more monsters. */
	p_ptr->number_pets = 0;

	/* Handle ``all-seeing'' flag */
	if (p_ptr->allseeing)
	{
		msg_print("You sense the living rock beneath your feet.");
		wiz_lite();
	}

	/* Handle criminal players. */
	if (!p_ptr->depth && p_ptr->sc < 1 && p_ptr->s_idx != 7 &&
		p_ptr->inside_special != SPECIAL_ARENA &&
		p_ptr->inside_special != SPECIAL_MAGIC_ARENA)
	{
		mprint(MSG_WARNING,
			"It seems your criminal tendencies aren't " "welcome here.");
		activate_generators();
	}


	if (p_ptr->prace == RACE_MUNCHKIN)
	{
		acquirement(p_ptr->py, p_ptr->px, 10, TRUE);
	}

	if (seed_dungeon)
	{
		Rand_quick = FALSE;
	}
}
