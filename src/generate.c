/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* generate.c: level generation */

#include "posband.h"

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
 *
 * Notes for PosBand 0.3.0
 * Extra terrain features (lava, grass and so on) required some hacking
 * of this level generator. One particularly time-consuming feature is "decayed" levels.
 * Still, it requires just the normal O(n) time (or so I think).
 * 
 */

static int next_to_walls(int y, int x); /*prototype for next_to_walls*/

/*
 * Dungeon generation values
 */

#define DUN_UNUSUAL	200	/* Level/chance of unusual room */
#define DUN_DEST	35	/* 1/chance of having a destroyed level */
#define SMALL_LEVEL 10	/* 1/chance of smaller size */

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
#define DUN_STR_DEN	5	/* Density of streamers -- unused */
#define DUN_STR_RNG	2	/* Width of streamers -- unused */
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
#define CENT_MAX	100
#define DOOR_MAX	200
#define WALL_MAX	500
#define TUNN_MAX	900


/*
 * Maximal number of room types
 */
#define ROOM_MAX	12
#define ROOM_MIN     	2

#define MAX_LAKE_TYPES	5
#define MAX_SCAT_TYPES	5

#define MAX_LAKES 	5

#define DG_STREAMERS_CROSS_FLOOR	0x00000001

/*
 * Dungeon generation mode
 */
typedef struct dun_generator dun_generator;
struct dun_generator
{
	int prob;                                /* Prob. of using this mode */
	byte minlev;				 /* Minimal level */
	        /* Used like history lines probabilities */
	u16b floor, streamer1, streamer2;        /* Features */
	byte strwidth, strdens;			 /* Streamers density and width */
	u32b flags;				 /* Flags */
	int decay;                               /* "Decay" factor */
	        /* Note: slows down generation considerably! */
	int lake_feat[MAX_LAKE_TYPES], lake_size[MAX_LAKE_TYPES];  /* Lakes features and sizes */
	int scat_feat[MAX_SCAT_TYPES], scat_prob[MAX_SCAT_TYPES];  /* Scattered features */
	        /* Probabilities here are in 1/1000's ! */
};

/* TODO: move this into lib/edit file */

static dun_generator generators[] =
{
	/* Basic dungeon (70% chance) */
	{
		70,
		0,
		FEAT_FLOOR,
		FEAT_QUARTZ,
		FEAT_MAGMA,
		2, 5,
		0,
		0,
		{ 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0 }
	},
	/* Field (5% chance) */
	{
		75,
		0,
		FEAT_FLOOR,
		FEAT_QUARTZ,
		FEAT_MAGMA,
		2, 5,
		0,
		0,
		{ FEAT_WATER, FEAT_TREE, 0, 0, 0 },
		{ 10, 5, 0, 0, 0 },
		{ FEAT_GRASS, 0, 0, 0, 0 },
		{ 250, 0, 0, 0, 0 }
	},
	/* Forest (5% chance) */
	{
		80,
		0,
		FEAT_GRASS,
		FEAT_WATER,
		FEAT_WATER,
		2, 20,
		DG_STREAMERS_CROSS_FLOOR,
		0,
		{ FEAT_WATER, FEAT_TREE, FEAT_GRASS, 0, 0 },
		{ 10, 20, 3, 0, 0 },
		{ FEAT_TREE, 0, 0, 0, 0 },
		{ 100, 0, 0, 0, 0 }
	},
	/* Beach (4% chance) */
	{
		84,
		20,
		FEAT_SAND,
		FEAT_WATER,
		FEAT_WATER,
		3, 15,
		DG_STREAMERS_CROSS_FLOOR,
		0,
		{ FEAT_WATER, FEAT_WATER, FEAT_ROCKS, FEAT_FLOOR, 0 },
		{ 10, 10, 5, 5, 0 },
		{ FEAT_ROCKS, 0, 0, 0, 0 },
		{ 35, 0, 0, 0, 0 }
	},
	/* Underwater cave (4% chance) */
	{
		88,
		30,
		FEAT_WATER,
		FEAT_QUARTZ,
		FEAT_MAGMA,
		2, 5,
		0,
		1,
		{ FEAT_ABYSS, FEAT_FLOOR, 0, 0, 0 },
		{ 5, 5, 0, 0, 0 },
		{ 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0 }
	},
	/* Glacier (4% chance) */
	{
		92,
		35,
		FEAT_ICE,
		FEAT_QUARTZ,
		FEAT_MAGMA,
		2, 5,
		0,
		1,
		{ FEAT_WATER, FEAT_ROCKS, FEAT_FLOOR, 0, 0 },
		{ 3, 5, 5, 0, 0 },
		{ FEAT_ROCKS, FEAT_FLOOR, 0, 0, 0 },
		{ 30, 30, 0, 0, 0 }
	},
	/* Mountains (4% chance) */
	{
		96,
		45,
		FEAT_ROCKS,
		FEAT_WALL_SOLID,
		FEAT_QUARTZ,
		4, 10,
		DG_STREAMERS_CROSS_FLOOR,
		1,
		{ FEAT_FLOOR, FEAT_ABYSS, FEAT_FOG, 0, 0 },
		{ 7, 7, 3, 0, 0 },
		{ FEAT_FLOOR, FEAT_ABYSS, FEAT_FOG, 0, 0 },
		{ 25, 25, 50, 0, 0 }
	},
	/* Abyss (2% chance) */
	{
		98,
		55,
		FEAT_FOG,
		FEAT_ABYSS,
		FEAT_ABYSS,
		4, 10,
		DG_STREAMERS_CROSS_FLOOR,
		0,
		{ FEAT_ABYSS, FEAT_ABYSS, 0, 0, 0 },
		{ 7, 7, 0, 0, 0 },
		{ FEAT_ABYSS, 0, 0, 0, 0 },
		{ 100, 0, 0, 0, 0 }
	},
	/* Volcano (2% chance) */
	{
		100,
		60,
		FEAT_FIRE,
		FEAT_LAVA,
		FEAT_ROCKS,
		3, 20,
		DG_STREAMERS_CROSS_FLOOR,
		2,
		{ FEAT_FIRE, FEAT_ROCKS, FEAT_ABYSS, 0, 0 },
		{ 15, 5, 20, 0, 0 },
		{ FEAT_ROCKS, 0, 0, 0, 0 },
		{ 25, 0, 0, 0, 0 }
	}
};

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
	
	/* Generator */
	dun_generator *gen;
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
	{ 0, 2, -1, 1, 5 },		/* 7 = Lesser vault (33x22) */
	{ -1, 4, -2, 5, 10 },		/* 8 = Greater vault (66x44) */
	{ 0, 1, -1, 1, 0 },		/* 9 = Quest vault (44x22) */
	{ 0, 1, -1, 1, 0 },		/* 10 = Starburst room (33x22) */
	{ -1, 2, -2, 3, 0 }		/* 11 = Greater starburst room (66x44) */
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
 * currently only used for player.
 */
static bool new_player_spot(void)
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

			/* Refuse to start on anti-teleport grids */
			if (cave_info[yy][xx] & (CAVE_ICKY)) continue;

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

	/* Place the player, check for failure */
	if (player_place(y_location_tables[rand_spot], x_location_tables[rand_spot]) == 0)
	{
		return (FALSE);
	}
	else return (TRUE);
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
	if ((p_ptr->depth >= 2) && (!quest_check(p_ptr->depth - 1)))
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
static void place_random_stairs(int y, int x)
{
	/* Paranoia */
	if (!cave_clean_bold(y, x)) return;

	/* Create a staircase */
	if (!p_ptr->depth)
	{
		cave_set_feat(y, x, FEAT_MORE);
	}
	else if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
			 (quest_check(p_ptr->depth) == QUEST_FIXED_U) ||
			 (p_ptr->depth >= MAX_DEPTH-1))
	{
		if (one_in_(2))	cave_set_feat(y, x, FEAT_LESS);
		else cave_set_feat(y, x, FEAT_LESS_SHAFT);
	}
	else if (one_in_(2))
	{
		if ((quest_check(p_ptr->depth + 1) == QUEST_FIXED) ||
			(quest_check(p_ptr->depth + 1) == QUEST_FIXED_U) ||
		 	(p_ptr->depth <= 1))
			cave_set_feat(y, x, FEAT_MORE);
		else if (one_in_(2)) cave_set_feat(y, x, FEAT_MORE);
		else cave_set_feat(y, x, FEAT_MORE_SHAFT);
	}
	else
	{
		if (one_in_(2))	cave_set_feat(y, x, FEAT_LESS);
		else cave_set_feat(y, x, FEAT_LESS_SHAFT);
	}
}


/*
 * Places some staircases near walls
 */
static bool alloc_stairs(int feat, int num)
{
	int x;

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
			cave_set_feat(yy, xx, pick_up_stairs());
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
			bool room;

			/* Location */
			y = rand_int(p_ptr->cur_map_hgt);
			x = rand_int(p_ptr->cur_map_wid);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue;

			/* Check for "room" */
			room = (cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

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
		for (i = 0; i < dun->gen->strdens; i++)
		{
			int d = dun->gen->strwidth;

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
			if (!(dun->gen->flags & DG_STREAMERS_CROSS_FLOOR))
			{
				if (cave_feat[ty][tx] < FEAT_WALL_EXTRA) continue;
				if (cave_feat[ty][tx] > FEAT_WALL_SOLID) continue;
			}
			
			/* Never convert permanent walls */
			if (cave_feat[ty][tx] >= FEAT_PERM_EXTRA &&
				cave_feat[ty][tx] <= FEAT_PERM_SOLID) continue;

			/* Clear previous contents, add proper vein type */
			cave_set_feat(ty, tx, feat);

			/* Hack -- Add some (known) treasure */
			if (rand_int(chance) == 0 && (feat == FEAT_QUARTZ || feat == FEAT_MAGMA))
				cave_feat[ty][tx] += 0x04;
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
						cave_set_feat(y, x, dun->gen->streamer2);
					}

					/* Magma */
					else if (t < 100)
					{
						/* Create magma vein */
						cave_set_feat(y, x, dun->gen->streamer1);
					}

					/* Floor */
					else
					{
						/* Create floor */
						cave_set_feat(y, x, dun->gen->floor);
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
 * Decay the level
 * Disabled for now
 */
#if 0
static void decay_level(void)
{
	int y, x, yy[10], xx[10], n, i;
	
	/* Cycle over the dungeon */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Reset 'affected' flag */
			cave_info[y][x] &= ~(CAVE_TEMP);

			/* Skip vaults */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;
			
			/* Affect only floor */
			if (cave_feat[y][x] != dun->gen->floor) continue;
			
			/* Remember this grid */
			xx[0] = x;
			yy[0] = y;
			n = 1;
			
			/* Decay from this grid */
			for (i = 0; i < dun->gen->decay; i++)
			{
				int tries = 0, k, dir;

				/* Keep trying */
				while (tries++ < 50)
				{				
					/* Find a grid */
					k = rand_int(n);
				
					/* Find a nearby grid */
					dir = rand_int(8);
					xx[n] = xx[k] + ddx_ddd[dir];
					yy[n] = yy[k] + ddy_ddd[dir];
				
					/* Check whether this grid is valid */
					
					/* Must be in bounds */
					if (!in_bounds_fully(y, x)) continue;

					/* Skip vaults */
					if (cave_info[yy[n]][xx[n]] & (CAVE_ICKY)) continue;
			
					/* Affect only non-permanent walls */
					if (cave_feat[yy[n]][xx[n]] < FEAT_MAGMA ||
						cave_feat[yy[n]][xx[n]] > FEAT_WALL_SOLID) continue;
					
					/* Mark the grid */
					cave_info[yy[n]][xx[n]] |= (CAVE_TEMP);
					n++;
					break;
				}
			}
		}
	}

	/* Cycle over the dungeon the second time */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Set the features */
			if (cave_info[y][x] & (CAVE_TEMP))
			{
				cave_set_feat(y, x, dun->gen->floor);
				cave_info[y][x] &= ~(CAVE_TEMP | CAVE_WALL);
			}
		}
	}
}
#endif /* 0 */

/*
 * Scatter some feature
 */
static void place_scattered_feat(int feat, int prob)
{
	int y, x;
	
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Don't step on walls */
			if (cave_info[y][x] & (CAVE_WALL)) continue;
			
			/* Random probability */
			if (rand_int(1000) < prob) cave_set_feat(y, x, feat);
		}
	}
}

/*
 * Make a random "lake" of some feature
 * Uses starburst methods, similar to project()
 */
static void make_lake(int feat, int size)
{
	int i, x, y;
	byte arc_first[45];
	byte arc_dist[45];
	int arc_num = 0, degree, max_dist, dist, ny, nx, y0, x0, rad;
	
	/* Randomize the size */
	rad = size / 2 + rand_int(size);
	
	/* Refuse to generate if the level is too small */
	if (rad * 2 > p_ptr->cur_map_wid || rad * 2 > p_ptr->cur_map_hgt) return;
	
	/* Mega-Hack -- size sometimes can be ~1G. No idea why */
	if (size < 0 || size > 100) return;

	/* Determine the starting coords -- not near the walls */
	y0 = rand_int(p_ptr->cur_map_hgt - rad * 2) + rad;
	x0 = rand_int(p_ptr->cur_map_wid - rad * 2) + rad;
	
	/* Set the starting grid */
	cave_set_feat(y0, x0, feat);
	
	/* Calculate the starburst */
	calc_starburst(1 + rad * 2, 1 + rad * 2, arc_first, arc_dist, &arc_num);
	
	/* Spread the marks */
	spread_cave_temp(x0, y0, rad, FALSE, TRUE);
	
	/* Scan the possible grids */
	for (y = y0 - rad; y <= y0 + rad; y++)
	{
		for (x = x0 - rad; x <= x0 + rad; x++)
		{
			/* Must be in_bounds_fully() */
			if (!in_bounds_fully(y, x)) continue;
			/* Must not be part of a vault */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;
			
			dist = distance(y, x, y0, x0);
			
			/* Get index in table */
			ny = y - y0 + 20;
			nx = x - x0 + 20;
			
			/* Check that it is valid */
			if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40)) continue;
			
			/* Get angle */
			degree = get_angle_to_grid[ny][nx];
			
			/* Scan the arcs */
			for (i = arc_num - 1; i >= 0; i--)
			{
				if (arc_first[i] <= degree)
				{	
					max_dist = arc_dist[i];
					
					if (max_dist >= dist)
					{
						cave_set_feat(y, x, feat);
					}
					break;
				}
			}
		}
	}
	
	/* Clear the temp marks */
	clear_temp_array();
}

/*
 * Cleans up dungeon
 * Currently only removes the "orphaned" doors
 * Fairly ineffective
 */
static void cleanup_dungeon(void)
{
	int y, x;
	
	/* Cycle over the dungeon */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Check for the door */
			if ((cave_feat[y][x] >= FEAT_DOOR_HEAD && cave_feat[y][x] <= FEAT_DOOR_TAIL) ||
				cave_feat[y][x] == FEAT_OPEN || cave_feat[y][x] == FEAT_BROKEN)
			{
				int i, k = 0;
				
				/* Count non-wall surroundings */
				for (i = 0; i < 8; i++)
				{
					int x1 = x + ddx_ddd[i];
					int y1 = y + ddy_ddd[i];
					
					/* Don't check in_bounds_fully(), shouldn't happen */
					
					if (cave_info[y1][x1] & (CAVE_WALL))
						k++;
				}
				
				/* Change the door to floor */
				if (k < 2) cave_set_feat(y, x, dun->gen->floor);
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
 *   9 -- quest vaults
 *  10 -- starburst
 *  11 -- greater starburst
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
	generate_fill(y1, x1, y2, x2, dun->gen->floor);


	/* Hack -- Occasional pillar room */
	if (rand_int(20) == 0)
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
	else if (rand_int(50) == 0)
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
	generate_fill(y1a, x1a, y2a, x2a, dun->gen->floor);

	/* Generate inner floors (b) */
	generate_fill(y1b, x1b, y2b, x2b, dun->gen->floor);
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
	generate_fill(y1a, x1a, y2a, x2a, dun->gen->floor);

	/* Generate inner floors (b) */
	generate_fill(y1b, x1b, y2b, x2b, dun->gen->floor);


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
	generate_fill(y1, x1, y2, x2, dun->gen->floor);


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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "T" monsters */
	if (!strchr("T", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (giant humanoids)"
 */
static bool vault_aux_orc_ogre_troll_giant(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "T"/"o"/"O"/"P" monsters */
	if (!strchr("ToOP", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (coins)"
 */
static bool vault_aux_coin(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "$" monsters */
	if (!strchr("$", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (hydras)"
 */
static bool vault_aux_hydra(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "M" monsters */
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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "d" or "D" monsters */
	if (!strchr("Dd", r_ptr->d_char)) return (FALSE);

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

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "D" monsters */
	if (!strchr("D", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_demon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "U" monsters */
	if (!strchr("U", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

static char mon_char;
/*
 * Helper function
 */
static bool vault_aux_char(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	
	return (r_ptr->d_char == mon_char);
}

static bool align_ok(int r_idx, byte align)
{
    	monster_race *r_ptr = &r_info[r_idx];

	switch (align)
	{
	    	case AL_HOSTILE:
		{
		    	return !(r_ptr->flags3 & (RF3_LAWFUL | RF3_CHAOTIC));
		}

		case AL_HOSTILE_C:
		{
		    	return ((r_ptr->flags3 & (RF3_CHAOTIC)) != 0);  /* Yes, != 0 *is* needed. */
		}

		case AL_HOSTILE_L:
		{
		    	return ((r_ptr->flags3 & (RF3_LAWFUL)) != 0);
		}
	}
	
	return (TRUE);
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
static void build_type5(int y0, int x0)
{
	int y, x, y1, x1, y2, x2;

	int i, j, harder_pit_check, mindepth, nestchooser, whatnest;

	s16b what[64];

	cptr name;

	bool empty = FALSE;

	int light = FALSE;

	byte aligned = 0; /* 0 = don't align; 1 = lawful/chaotic; 2 = lawful/neutral/chaotic */
	byte align = 0;

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
	generate_fill(y1, x1, y2, x2, dun->gen->floor);


	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_INNER);

	/* Open the inner room with a secret door */
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

	/*get current level*/
	nestchooser = p_ptr->depth;

	/*enforce minimum depth, to keep weak nests out of deep levels*/
	mindepth = nestchooser / 4;

	/*keep total to 100 or less*/
	if ((mindepth + nestchooser) > 100) nestchooser = 100 - mindepth;

	/* Hack -- Choose a nest type */
	whatnest = randint(nestchooser) + mindepth;

	if ((whatnest <= 25)  && (p_ptr->depth <= 30))
	{
		if (one_in_(3))

		/* Monster nest (creeping coins) */
		{
		    	/* Describe */
			name = "creeping coins";

			/* Restrict to coins */
			get_mon_num_hook = vault_aux_coin;
		}
		else

		if (one_in_(2))

		/* Monster nest (jelly) */
		{
			/* Describe */
			name = "jelly";

			/* Restrict to jelly */
			get_mon_num_hook = vault_aux_jelly;
		}
		else

		/* Monster nest (kobolds, yeeks, orcs) */
		{
			/* Describe */
			name = "kobold yeek orc";

			/* Restrict to kobolds, yeeks, orcs and nagas*/
			get_mon_num_hook = vault_aux_kobold_yeek_orc_naga;

			aligned = 1;
		}
	}


	else if (whatnest <= 50)
	{
	    	if (one_in_(2))

		/* Monster nest (young dragons) */
		{
			/* Describe */
			name = "young dragon";

			/* Restrict to young dragons */
			get_mon_num_hook = vault_aux_youngdragon;

			aligned = 1;
		}
		else

		/* Monster nest (hydras) */
		{
		    	/* Describe */
		    	name = "hydra";

			/* Restrict to hydras */
			get_mon_num_hook = vault_aux_hydra;
		}
	}

	else if (whatnest <=75)
	{
		if (one_in_(2))
		/*animal pit*/
		{
			/* Describe */
			name = "animal";

			/* Restrict to animal */
			get_mon_num_hook = vault_aux_animal;
		}

		else
		/* Monster nest (humanoid) */
		{
			/* Describe */
			name = "humaniod";

			/* Restrict to humanoids */
			get_mon_num_hook = vault_aux_humanoids;

			aligned = 2;
		}

	}

	/*Monster nest (undead) */
	else if (whatnest <=95)
	{
		name = "undead";

		/* Restrict to undead */
		get_mon_num_hook = vault_aux_undead;

		aligned = 2;
	}

	/*Ancient Dragon Next*/
	else
	{
		/* Message */
		name = "ancient dragons";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_ancdragon;

		aligned = 1;
	}


	/* Prepare allocation table */
	get_mon_num_prep();

	/* Force similar alignment, if required */
	switch (aligned)
	{
	    	case 0:
		{
		    	align = 0;  /* Any */
			break;
		}

		case 1:
		{
		    	if (one_in_(2)) align = AL_HOSTILE_L;
			else align = AL_HOSTILE_C;
			break;
		}

		case 2:
		{
		    	int z = rand_int(3);
			if (z == 0) align = AL_HOSTILE;
			else if (z == 1) align = AL_HOSTILE_L;
			else align = AL_HOSTILE_C;
			break;
		}
	}

	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		/* Get a (hard) monster type */
	    	for (j = 0; j < 100; j++)
		{
			what[i] = get_mon_num(p_ptr->depth + 5);
			if (align_ok(what[i], align)) break;
		}

		/* Notice failure */
		if (!what[i] || j >= 100) empty = TRUE;
	}


	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Oops */
	if (empty) return;

	/* Describe */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster nest (%s)", name);
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
	harder_pit_check = r_info[what[63]].level - p_ptr->depth;

	/*Hack - make some pits harder if deeper*/
	if (randint(100) < harder_pit_check)
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
}



/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type organized in the room.
 *
 * Monster types in the pit
 *   orc pit	(Dungeon Level 5 and deeper)
 *   troll pit	(Dungeon Level 20 and deeper)
 *   giant pit	(Dungeon Level 40 and deeper)
 *   dragon pit	(Dungeon Level 60 and deeper)
 *   demon pit	(Dungeon Level 80 and deeper)
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
static void build_type6(int y0, int x0)
{
	int whatpit, what[16], harder_nest_check;

	int i, j, y, x, y1, x1, y2, x2, pitchooser, mindepth;

	bool empty = FALSE;

	int light = FALSE;

	byte aligned = 0; /* 0 = don't align; 1 = lawful/chaotic; 2 = lawful/neutral/chaotic */
	byte align = 0;

	cptr name;

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
	generate_fill(y1, x1, y2, x2, dun->gen->floor);


	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_INNER);

	/* Open the inner room with a secret door */
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

	/* Choose a pit type */

	/*get current level*/
	pitchooser = p_ptr->depth;

	/*enforce minimum depth, to keep weak nests out of deep levels*/
	mindepth = pitchooser / 4;

	/*keep total to 100 or less, so results aren't skewed at deep depths*/
	if ((mindepth + pitchooser) > 100) pitchooser = 100 - mindepth;

	/* Hack -- Choose a nest type */
	whatpit = randint(pitchooser) + mindepth;

	if ((whatpit <= 20) && (p_ptr->depth <= 30))
	{
	    	if (one_in_(2))

		/* Creeping coins pit */
		{
		    	/* Message */
		    	name = "creeping coins";

			/* Restrict monster selection */
			get_mon_num_hook = vault_aux_coin;
		}
		
	    	else
		    
		/* Orc pit */
		{
			/* Message */
			name = "orc";

			/* Restrict monster selection */
			get_mon_num_hook = vault_aux_orc;
		}
	}


	else if ((whatpit <= 35)  && (p_ptr->depth <= 40))
	{
		if (one_in_(2))
		{
			/* Message */
			name = "troll";

			/* Restrict monster selection */
			get_mon_num_hook = vault_aux_troll;
		}

		else
		{
			/* Message */
			name = "ogre";

			/* Restrict monster selection */
			get_mon_num_hook = vault_aux_ogre;
		}
	}


	else if ((whatpit <= 50) && (p_ptr->depth <= 60))
	{
		/* Hound pit */
		if (one_in_(3))
		{
			/* Message */
			name = "hound";

			/* Restrict monster selection */
			get_mon_num_hook = vault_aux_hounds;
		}

		/* Hydra pit */
		else if (one_in_(2))
		{
		    	/* Message */
		    	name = "hydra";

			/* Restrict monster selection */
			get_mon_num_hook = vault_aux_hydra;
		}

		/*young dragon_pit*/
		else
		{
			/* Describe */
			name = "young dragon";

			/* Restrict to young dragons */
			get_mon_num_hook = vault_aux_youngdragon;

			aligned = 1;
		}


	}

	/* Giant pit */
	else if ((whatpit <= 60) && (p_ptr->depth <= 80))
	{
		/* Message */
		name = "giant";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_giant;
	}

	/* Dragon pit */
	else if (whatpit <= 80)
	{
		/* Pick dragon type */
		switch (rand_int(6))
		{
			/* Black */
			case 0:
			{
				/* Message */
				name = "acid dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BRTH_ACID;

				/* Done */
				break;
			}

			/* Blue */
			case 1:
			{
				/* Message */
				name = "electric dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BRTH_ELEC;

				/* Done */
				break;
			}

			/* Red */
			case 2:
			{
				/* Message */
				name = "fire dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BRTH_FIRE;

				/* Done */
				break;
			}

			/* White */
			case 3:
			{
				/* Message */
				name = "cold dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BRTH_COLD;

				/* Done */
				break;
			}

			/* Green */
			case 4:
			{
				/* Message */
				name = "poison dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BRTH_POIS;

				/* Done */
				break;
			}

			/* Chromatic */
			default:
			{
				/* Message */
				name = "chromatic dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (RF4_BRTH_ACID | RF4_BRTH_ELEC |
				                          RF4_BRTH_FIRE | RF4_BRTH_COLD |
				                          RF4_BRTH_POIS);

				/* Done */
				break;
			}

		}

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_dragon;
	}

	/* Ancient Dragon pit */
	else if (whatpit <= 90)
	{
		/* Message */
		name = "ancient dragons";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_ancdragon;

		aligned = 1;
	}
	/* Demon pit */
	else
	{
		/* Message */
		name = "demon";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_demon;
	}

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Force similar alignment, if required */
	switch (aligned)
	{
	    	case 0:
		{
		    	align = 0;  /* Any */
			break;
		}

		case 1:
		{
		    	if (one_in_(2)) align = AL_HOSTILE_L;
			else align = AL_HOSTILE_C;
			break;
		}

		case 2:
		{
		    	int z = rand_int(3);
			if (z == 0) align = AL_HOSTILE;
			else if (z == 1) align = AL_HOSTILE_L;
			else align = AL_HOSTILE_C;
			break;
		}
	}

	/* Pick some monster types */
	for (i = 0; i < 16; i++)
	{
		/* Get a (hard) monster type */
	    	for (j = 0; j < 100; j++)
		{
			what[i] = get_mon_num(p_ptr->depth + 6);
			if (align_ok(what[i], align)) break;
		}

		/* Notice failure */
		if (!what[i]) empty = TRUE;
	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

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
	harder_nest_check = r_info[what[15]].level - p_ptr->depth;

	/*Hack - make some pits harder if deeper*/
	if (randint(100) < harder_nest_check)
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
		msg_format("Monster pit (%s)", name);
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
}

/* Monster classes */
/* XXX These are unordered */
#define MCLASS_DRAGON_BABY	(-1)
#define MCLASS_DRAGON_YOUNG	(-2)
#define MCLASS_DRAGON_MATURE	(-3)
#define MCLASS_DRAGON_LESSER	(-4)  /* Young + Mature */
#define MCLASS_DRAGON_ANCIENT	(-5)
#define MCLASS_DRAGON_WYRM	(-6)
#define MCLASS_DRAGON_GREATER	(-7)  /* Ancient + Wyrm */
#define MCLASS_UNDEAD_ZOMBIE	(-13) /* Skeletons & Zombies */
#define MCLASS_UNDEAD_GHOST	(-14) /* Ghosts */
#define MCLASS_UNDEAD_HIGH	(-15) /* Liches, Wraiths, high-level Ghosts */
#define MCLASS_ANIMAL		(-16)
#define MCLASS_MOLD		(-19) /* Mold, Jelly, Mushroom */

/*
 * Pick a monster from one of "predefined" monster classes.
 */
static int vault_simple_choose(int class, int level)
{
	int r_idx;
	monster_race *r_ptr;
	char *m_name;
	
	while (TRUE)
	{
		/* Get a (hard) monster type */
		r_idx = get_mon_num(level);
		r_ptr = &r_info[r_idx];
		m_name = r_name + r_ptr->name;

		switch (class)
		{
			case MCLASS_DRAGON_BABY:
			{
				if (r_ptr->flags3 & (RF3_DRAGON) && strstr(m_name, "Baby"))
					return r_idx;
				break;
			}
			
			case MCLASS_DRAGON_YOUNG:
			{
				if (r_ptr->flags3 & (RF3_DRAGON) && strstr(m_name, "Young"))
					return r_idx;
				break;
			}
			
			case MCLASS_DRAGON_MATURE:
			{
				if (r_ptr->flags3 & (RF3_DRAGON) && (strstr(m_name, "Mature") || strstr(m_name, "drake")))
					return r_idx;
				break;
			}
			
			case MCLASS_DRAGON_LESSER:
			{
				if (r_ptr->flags3 & (RF3_DRAGON) && r_ptr->d_char == 'd')
					return r_idx;
				break;
			}
			
			case MCLASS_DRAGON_ANCIENT:
			{
				if (r_ptr->flags3 & (RF3_DRAGON) && strstr(m_name, "Ancient"))
					return r_idx;
				break;
			}
			
			case MCLASS_DRAGON_WYRM:
			{
				if (r_ptr->flags3 & (RF3_DRAGON) && strstr(m_name, "Wyrm"))
					return r_idx;
				break;
			}
			
			case MCLASS_DRAGON_GREATER:
			{
				if (r_ptr->flags3 & (RF3_DRAGON) && r_ptr->d_char == 'D')
					return r_idx;
				break;
			}

			case MCLASS_UNDEAD_ZOMBIE:
			{
				if (r_ptr->d_char == 'z' || r_ptr->d_char == 's')
					return r_idx;
				break;
			}
			
			case MCLASS_UNDEAD_GHOST:
			{
				if (r_ptr->d_char == 'G')
					return r_idx;
				break;
			}
			
			case MCLASS_UNDEAD_HIGH:
			{
				if (r_ptr->d_char == 'W' || r_ptr->d_char == 'L' ||
					(r_ptr->d_char == 'G' && r_ptr->level >= 40)) return r_idx;
				break;
			}

			case MCLASS_ANIMAL:
			{
				if (r_ptr->flags3 & (RF3_ANIMAL))
					return r_idx;
				break;
			}

			case MCLASS_MOLD:
			{
				if (r_ptr->d_char == 'm' || r_ptr->d_char == 'j' || r_ptr->d_char == ',')
					return r_idx;
				break;
			}

			default:
				msg_format("Unknown monster class %d!", class);
		}
	}
	
	return 0;
}

/*
 * Hack -- fill in "vault" rooms
 */
static void build_vault(int y0, int x0, int ymax, int xmax, cptr data, vault_type *v_ptr)
{
	int dx, dy, x, y, i;
	int ax, ay;
	bool flip_v = FALSE;
	bool flip_h = FALSE;

	cptr t;

	/* Flip the vault (sometimes) */
	if (rand_int(2) == 0) flip_v = TRUE;
	if (rand_int(2) == 0) flip_h = TRUE;

	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{

		if (flip_v) ay = ymax - dy;
		else ay = dy;

		for (dx = 0; dx < xmax; dx++, t++)
		{
			bool cod_set = FALSE;

			if (flip_h) ax = xmax - dx;
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
			
			/* Check if the symbol is encoded */
			for (i = 0; i < MAX_VAULT_COD; i++)
			{
				if (!v_ptr->cod[i].code) break;
				
				if (v_ptr->cod[i].code == *t)
				{
					/* Set the terrain */
					cave_set_feat(y, x, v_ptr->cod[i].terr);
					/* Place the monster */
					if (v_ptr->cod[i].race)
					{
						int r_idx;
						if (v_ptr->cod[i].race < 0)
							r_idx = vault_simple_choose(v_ptr->cod[i].race,
								p_ptr->depth + (v_ptr->typ == 8 ? 20 : 5));
						else
							r_idx = v_ptr->cod[i].race;
						place_monster_aux(y, x, r_idx, TRUE, FALSE);
						
					}
					cod_set = TRUE;
					break;
				}
			}

			/* Analyze the grid, if failed to decode */
			if (!cod_set) switch (*t)
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

				/* Rubble */
				case ':':
				{
					cave_set_feat(y, x, FEAT_RUBBLE);
					break;
				}
				
				/* Misc. terrain represented via 8-bit chars */
				case 'Á': cave_set_feat(y, x, FEAT_WATER); break;
				case 'Â': cave_set_feat(y, x, FEAT_ICE);   break;
				case '×': cave_set_feat(y, x, FEAT_FIRE);  break;
				case 'Ç': cave_set_feat(y, x, FEAT_LAVA);  break;
				case 'Ä': cave_set_feat(y, x, FEAT_SAND);  break;
				case 'Å': cave_set_feat(y, x, FEAT_ROCKS); break;
				case 'Ö': cave_set_feat(y, x, FEAT_FOG);   break;
				case 'Ú': cave_set_feat(y, x, FEAT_ABYSS); break;
				case 'É': cave_set_feat(y, x, FEAT_GRASS); break;
				case 'Ê': cave_set_feat(y, x, FEAT_TREE);  break;

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
				
				/* Mineral vein */
				case '/': cave_set_feat(y, x, FEAT_QUARTZ_K); break;
				
				/* Good treasure */
				case '"':
				{
					object_level = p_ptr->depth + 10;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_UNTHEMED);
					object_level = p_ptr->depth;
					break;
				}
				
				/* Various drops */
				case 'Ë':
				{
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_ARMOR);
					object_level = p_ptr->depth;
					break;
				}
				case 'Ì':
				{
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_WEAPON);
					object_level = p_ptr->depth;
					break;
				}
				case 'Í':
				case 'Î':
				{
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_JEWELRY);
					object_level = p_ptr->depth;
					break;
				}
				case 'Ð':
				{
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_POTION);
					object_level = p_ptr->depth;
					break;
				}
				case 'Ô':
				{
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_SCROLL);
					object_level = p_ptr->depth;
					break;
				}
				case 'Õ':
				{
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_ROD_WAND_STAFF);
					object_level = p_ptr->depth;
					break;
				}
				case 'Æ':
				{
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_GOLD);
					object_level = p_ptr->depth;
					break;
				}
				case 'Ã':
				{
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_CHEST);
					object_level = p_ptr->depth;
					break;
				}

				/* Great treasure */
				case '$':
				{
					object_level = p_ptr->depth + 25;
					place_object(y, x, TRUE, TRUE, DROP_TYPE_UNTHEMED);
					object_level = p_ptr->depth;
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


	/* Place dungeon monsters and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		if (flip_v) ay = ymax - dy;
		else ay = dy;

		for (dx = 0; dx < xmax; dx++, t++)
		{

			if (flip_h) ax = xmax - dx;
			else ax = dx;

			/* Extract the grid */
			x = x0 - (xmax/2) + ax;
			y = y0 - (ymax/2) + ay;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;
			
			if (*t & 0x80) continue;

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

					place_quest_chest(y, x, TRUE, TRUE);
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
				
				/* Nothing */
				case '.':
				case ' ':
				case '%':
				case '#':
				case 'X':
				case ':':
				case '*':
				case '/':
				case '"':
				case '$':
				case '+':
				case '^':
					break;

				/* Monster specified by character */
				default:
				{
					int r_idx;
					
					mon_char = (*t);
					
					/* Set hook */
					get_mon_num_hook = vault_aux_char;
					
					/* Prepare table */
					get_mon_num_prep();
					
					/* Get monster */
					r_idx = get_mon_num(p_ptr->depth + 5);
					
					/* Place it */
					place_monster_aux(y, x, r_idx, TRUE, FALSE);

					/* Remove restriction */
					get_mon_num_hook = NULL;

					/* Prepare allocation table */
					get_mon_num_prep();
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
	int i;
	bool okay;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* Accept the first lesser vault */
		if (v_ptr->typ != 7) continue;
		
		okay = TRUE;
		
		/* Check requirements */
		for (i = 0; i < MAX_VAULT_REQ; i++)
		{
			if (!v_ptr->req[i].type) break;
			switch (v_ptr->req[i].type)
			{
				/* Unique alive */
				case 1:
				{
					monster_race *r_ptr = &r_info[v_ptr->req[i].info];
					
					if (r_ptr->cur_num >= r_ptr->max_num) okay = FALSE;
				}

				/* Depth lesser than */
				case 2:
				{
					if (p_ptr->depth >= v_ptr->req[i].info) return;
					break;
				}
				
				/* Depth greater than */
				case 3:
				{
					if (p_ptr->depth < v_ptr->req[i].info) return;
					break;
				}
			}
		}

		if (okay) break;
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
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, v_ptr);
}



/*
 * Type 8 -- greater vaults (see "vault.txt")
 */
static void build_type8(int y0, int x0)
{
	vault_type *v_ptr;
	int i;
	bool okay;
        char note[120];

	/* Pick a greater vault */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* Accept the first greater vault */
		if (v_ptr->typ != 8) continue;

		okay = TRUE;
		
		/* Check requirements */
		for (i = 0; i < MAX_VAULT_REQ; i++)
		{
			if (!v_ptr->req[i].type) break;
			switch (v_ptr->req[i].type)
			{
				/* Unique alive */
				case 1:
				{
					monster_race *r_ptr = &r_info[v_ptr->req[i].info];
					
					if (r_ptr->cur_num >= r_ptr->max_num) okay = FALSE;
					break;
				}
				
				/* Depth lesser than */
				case 2:
				{
					if (p_ptr->depth >= v_ptr->req[i].info) return;
					break;
				}
				
				/* Depth greater than */
				case 3:
				{
					if (p_ptr->depth < v_ptr->req[i].info) return;
					break;
				}
			}
		}

		if (okay) break;

	}

	/* Message */
	if (cheat_room) msg_format("Greater vault (%s)", v_name + v_ptr->name);

	/* Make a note of the greater vault if auto-note is selected. */

        /* Build note and write */
        sprintf(note, "(%s)", v_name + v_ptr->name);

        do_cmd_note(note, p_ptr->depth);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, v_ptr);
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
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, v_ptr);
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
		cave_set_feat(y, x, dun->gen->floor);
	}


	/* Apply the piercings that we found */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Get the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* Convert to floor grid */
		cave_set_feat(y, x, dun->gen->floor);

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
		if (!cave_floor_bold(y, x)) continue;

		/* Skip non "empty floor" grids */
		if (cave_feat[y][x] != dun->gen->floor) continue;

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
		case 6: build_type6(y, x); break;
		case 5: build_type5(y, x); break;
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


/*
 * Place all "permanent" monsters somewhere in player's LOS.
 * Used for companions - that is, pets who follow you between levels.
 * Assumes player is already placed in valid location.
 */
static bool place_perm_monsters()
{
	int i, k, y, x;

	/* Put permanent monsters */
	for (i = 0; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		
		/* Skip dead */
		if (!m_ptr->r_idx) continue;
		
		/* Skip non-permanent (normally only permanent should be alive at this point) */
		if (!(m_ptr->mflag & (MFLAG_PERM))) continue;
		
		/* Try 50 times to place monster near player */
		for (k = 0; k < 50; k++)
		{
			scatter(&y, &x, p_ptr->py, p_ptr->px, 10, 0);
			if (cave_naked_bold(y, x))
			{
				m_ptr->fy = y;
				m_ptr->fx = x;
				cave_m_idx[y][x] = i;
				break;
			}
		}
		
		/* Complain */
		if (k == 50)
		{
			if (cheat_room) msg_format("Failed to place permanent monster (idx %d)", i);
			return (FALSE);
		}
	}
	
	return (TRUE);
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(void)
{
	int i, k, y, x, y1, x1;

	int by, bx;

	bool destroyed = FALSE;
	bool quest_vault = FALSE;

	/* Hack - variables for allocations */
	s16b mon_gen, obj_gen;

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;
	
	/* Set generator */
	if (adult_themed_levels)
	{
reset_gen:
		k = rand_int(100);
		i = 0;
		while (TRUE)
		{
			if (k < generators[i].prob)
			{
				dun->gen = &generators[i];
				break;
			}
			i++;
		}
	}
	else
	{
		dun->gen = &generators[0];
	}

	/* Ensure quest monsters */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Quest levels */
		if (q_ptr->active_level == p_ptr->depth)
		{
			/* Force the default generator */
			dun->gen = &generators[0];
		}
	}
	
	/* Check whether the level is acceptable */
	if (p_ptr->depth < dun->gen->minlev)
		goto reset_gen;
	
	/* Reset generation variables */
	mon_gen = MIN_M_ALLOC_LEVEL;
	obj_gen = Rand_normal(DUN_AMT_ROOM, 3);

	/* Possible "destroyed" level */

	if ((p_ptr->depth > 10) && (rand_int(DUN_DEST) == 0)) destroyed = TRUE;

	if (((adult_force_small_lev) || (randint(SMALL_LEVEL)==1)) &&
		(quest_check(p_ptr->depth) != QUEST_VAULT))
    {
		int l, m;

		while (TRUE)
		{
			l = randint(MAX_DUNGEON_HGT / (2 * PANEL_HGT));
			m = randint(MAX_DUNGEON_WID / (2 * PANEL_WID_FIXED));

			p_ptr->cur_map_hgt = l * (2 * PANEL_HGT);
			p_ptr->cur_map_wid = m * (2 * PANEL_WID_FIXED);

			/* Exit if less than normal dungeon */
			if ((p_ptr->cur_map_hgt < MAX_DUNGEON_HGT) || (p_ptr->cur_map_wid < MAX_DUNGEON_WID)) break;
		}

		if (cheat_room) msg_format("A 'small' dungeon level (%dx%d).", m, l);

		if (!adult_force_small_lev) rating += ((m + l <= 3) ? 15 : 10);
	}
	else
	{
	/* Full-sized dungeon */
		p_ptr->cur_map_hgt = MAX_DUNGEON_HGT;
		p_ptr->cur_map_wid = MAX_DUNGEON_WID;
	}


	/* Hack -- Start with basic granite */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Create granite wall */
			cave_set_feat(y, x, FEAT_WALL_EXTRA);
		}
	}

	/* Hack -- No destroyed "quest" levels */
	if (quest_check(p_ptr->depth)) destroyed = FALSE;

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

		/* First, build a quest vault if needed */
		if ((quest_check(p_ptr->depth) == QUEST_VAULT) && (quest_item_slot() == -1) &&
			(!quest_vault))
		{
			if (room_build(by, bx, 9)) quest_vault = TRUE;
			continue;
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
				if ((k < 10) && room_build(by, bx, 8)) continue;

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
		
		/* Occasionally attempt a starburst room */
		if (one_in_(5))
		{
			bool giant_room = (one_in_(10) ? 11 : 10);

			if (room_build(by, bx, giant_room)) continue;
		}


		/* Attempt a trivial room */
		if (room_build(by, bx, 1)) continue;
	}

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


	/*start over on all levels with less than two rooms due to inevitable crash*/
	if (dun->cent_n < ROOM_MIN)
	{
		if (cheat_room) msg_format("not enough rooms");
		return (FALSE);
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
		/* Connect the room to the previous room */
		build_tunnel(dun->cent[i].y, dun->cent[i].x, y, x);

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

	/*don't destroy really small levels*/
	if (dun->cent_n < 10) destroyed = FALSE;
	
	/* Decay the level if necessary */
	/* if (dun->gen->decay) decay_level(); */
	
	/* Place the scattered features */
	for (i = 0; i < MAX_SCAT_TYPES; i++)
	{
		if (dun->gen->scat_feat[i])
			place_scattered_feat(dun->gen->scat_feat[i], dun->gen->scat_prob[i]);
	}

	/* Hack -- Add some magma streamers */
	for (i = 0; i < DUN_STR_MAG; i++)
	{
		/*if we can't build streamers, something is wrong with level*/
		if (!build_streamer(dun->gen->streamer2, DUN_STR_MC)) return (FALSE);
	}

	/* Hack -- Add some quartz streamers */
	for (i = 0; i < DUN_STR_QUA; i++)
	{
		/*if we can't build streamers, something is wrong with level*/
		if (!build_streamer(dun->gen->streamer1, DUN_STR_QC)) return (FALSE);
	}
	
	/* Place the lakes */
	for (i = 0; i < MAX_LAKE_TYPES; i++)
	{
		if (dun->gen->lake_feat[i])
		{
			for (k = 0; k < MAX_LAKES; k++)
			{
				if (rand_int(3) == 0)
					make_lake(dun->gen->lake_feat[i], dun->gen->lake_size[i]);
			}
		}
	}

	/* Destroy the level if necessary */
	if (destroyed) destroy_level();

	/* Place 3 or 4 down stairs near some walls */
	if (!(alloc_stairs(FEAT_MORE, rand_range(3, 4))))
	{
		if (cheat_room) msg_format("failed to place down stairs");

		return(FALSE);
	}

	/* Place 1 or 2 up stairs near some walls */
	if (!(alloc_stairs(FEAT_LESS, rand_range(1, 2))))
	{
		if (cheat_room) msg_format("failed to place down stairs");

		return(FALSE);
	}

	/* Basic "amount" */
	k = (p_ptr->depth / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));

	/* Clean up */
	cleanup_dungeon();

	/* Determine the character location */
	if (!new_player_spot())
	{
		if (cheat_room) msg_format("couldn't spot player");

		return(FALSE);
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
	for (i = mon_gen + k; i > 0; i--)
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

			if ((q_ptr->type == QUEST_GUILD) || (q_ptr->type == QUEST_FIXED))
			{
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
			}

		}
	}

	/* Put some objects in rooms */
	if (obj_gen > 0) alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, obj_gen);

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal(DUN_AMT_GOLD, 3));
	
	/* Put permanent monsters... and that's all */
	return (place_perm_monsters());
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

bool pet_generate_hack = FALSE;

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
	
	/*
	 * Place permanent monsters.
	 * Note that we do not handle failure here!
	 */
	(void)place_perm_monsters();
	
	/*
	 * Hack used just after birth - generate a companion
	 * monster of a specific type.
	 */
	if (pet_generate_hack)
	{
		pet_generate_hack = FALSE; /* Never again */
		
		summon_pets_hack = TRUE;
		for (i = 1; i < 8; i++)
		{
			if (place_monster_aux(p_ptr->py + ddy_ddd[i], p_ptr->px + ddy_ddd[i], rp_ptr->pet_r_idx, FALSE, FALSE)) break;
		}
		summon_pets_hack = FALSE;
		
		/* Manually search for monster, then set his permanent flag. */
		for (i = 1; i < mon_max; i++)
		{
			if (mon_list[i].r_idx == rp_ptr->pet_r_idx)
				mon_list[i].mflag |= (MFLAG_PERM);
		}
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
	int y, x, num;
	
	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;

		cptr why = NULL;

		/* Reset */
		o_max = 1;
		/* mon_max = 1; */


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

				/* No flow */
				cave_cost[y][x] = 0;
				cave_when[y][x] = 0;
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

		/* Build a real level */
		else
		{
			/* Make a dungeon, or report the failure to make one*/
			okay = (cave_gen());
		}

		/*message*/
		if((!okay) && ((cheat_room || cheat_hear || cheat_peek || cheat_xtra)))
					why = "defective level";

		else
		{

			/* Extract the feeling */
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

			/* It takes 1000 game turns for "feelings" to recharge */
			if (((turn - old_turn) < 1000) && (old_turn > 1)) feeling = 0;

			/* Hack -- no feeling in the town */
			if (!p_ptr->depth) feeling = 0;

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
				/* Require "goodness" */
				if ((feeling > 9) ||
				    ((p_ptr->depth >= 5) && (feeling > 8)) ||
				    ((p_ptr->depth >= 10) && (feeling > 7)) ||
				    ((p_ptr->depth >= 20) && (feeling > 6)) ||
				    ((p_ptr->depth >= 40) && (feeling > 5)))
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
		}

		/* Message */
		if (why) msg_format("Generation restarted (%s)", why);

		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the monsters */
		wipe_mon_list();
	}

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Remember when this level was "created" */
	old_turn = turn;

	/* Reset the number of traps on the level. */
	num_trap_on_level = 0;

	/*All of Angband knows about a thief*/
	if ((recent_failed_thefts > 30) && (p_ptr->depth))
	{
			msg_print("You hear hunting parties scouring the area for a notorious burgler.");
	}

	/* Hack -- Issue a message about king monster */
	if (rp_ptr->king_r_idx)
	{
		int i;
		for (i = 0; i < mon_max; i++)
		{
			monster_type *m_ptr = &mon_list[i];
			if (m_ptr->r_idx == rp_ptr->king_r_idx)
			{
				msg_format("You sense the presence of the greatest %s alive...", p_name + rp_ptr->name);
				break;
			}
		}
	}
	
}




