/* File: generate.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
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

#define DUN_UNUSUAL_MORIA	300	/* Level/chance of unusual room */
#define DUN_UNUSUAL	180	/* Level/chance of unusual room */
#define DUN_DEST	35	/* 1/chance of having a destroyed level */
#define DUN_FRACTAL	25	/* 1/chance of having a fractal level */
#define SMALL_LEVEL	10	/* 1/chance of smaller size */
#define THEMED_LEVEL_CHANCE	75	/* 1/chance of being a themed level */
#define WILDERNESS_LEVEL_CHANCE	75 /* 1/chance of being a pseudo-wilderness level */
#define LABYRINTH_LEVEL_CHANCE	75 /* 1/chance of being a labrynth level */
#define GREATER_VAULT_LEVEL_CHANCE	150 /* 1/chance of being a greater vault level */

#define DUN_MAX_LAKES	3	/* Maximum number of lakes/rivers */
#define DUN_FEAT_RNG	2	/* Width of lake */

/* Maximum size of a fractal map */
#define MAX_FRACTAL_SIZE 65

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
#define DUN_STR_SAN	2	/* Number of sandstone streamers */
#define DUN_STR_SLV	40	/* Deepest level sandstone occurs instead of magma */
#define DUN_STR_GOL	20	/* 1/chance of rich mineral vein */
#define DUN_STR_GC	2	/* 1/chance of treasure per rich mineral vein */
#define DUN_STR_CRA	8	/* 1/chance of cracks through dungeon */
#define DUN_STR_CC	0	/* 1/chance of treasure per crack */

/*
 * Dungeon treausre allocation values
 */
#define DUN_AMT_ROOM	9	/* Amount of objects for rooms */
#define DUN_AMT_ITEM	3	/* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD	3	/* Amount of treasure for rooms/corridors */

#define DUN_AMT_ROOM_MORIA	7	/* Amount of objects for rooms */
#define DUN_AMT_ITEM_MORIA	2	/* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD_MORIA	2	/* Amount of treasure for rooms/corridors */

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
#define ALLOC_TYP_CHEST		6	/* Object */
#define ALLOC_TYP_PARCHMENT 7	/* Special parchment, for collection quests */


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


/*
 * These flags control the construction of starburst rooms and lakes
 */
#define STAR_BURST_ROOM		0x00000001	/* Mark grids with CAVE_ROOM */
#define STAR_BURST_LIGHT	0x00000002	/* Mark grids with CAVE_GLOW */
#define STAR_BURST_CLOVER	0x00000004	/* Allow cloverleaf rooms */
#define STAR_BURST_RAW_FLOOR	0x00000008	/* Floor overwrites dungeon */
#define STAR_BURST_RAW_EDGE	0x00000010	/* Edge overwrites dungeon */


bool allow_uniques;


/*
 * Maximal number of room types
 */
#define ROOM_MAX	15
#define ROOM_MIN	2

/*
 * Maximum distance between rooms
 */
#define MAX_RANGE_TO_ROOM 15


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
	{ -1, 2, -2, 3, 0},		/* 11 = Great Starburst (66x44) */

	/*
	 * Hack - if the dimensions for starburst rooms or great starburst
	 * rooms change, these lines in build_type_starburst need changing:
	 if (giant_room)
	 {
	 	dy = 19;
		dx = 30;
	 }
	 33x22
	 else
	 {
		dy = 9;
		dx = 14;
	 }

	 -JG
	 */

	{0, 1, -1, 1, 0},		/* 12 = Small Fractal (33x22) */
	{-1, 1, -1, 1, 0},		/* 13 = Medium Fractal (33x33) */
	{-1, 1, -2, 3, 0}		/* 14 = Big Fractal (66x33) */
};


/*
 * This table holds aditional flags for themed levels
 * These flags are used to forbid the generation of certain features BEFORE
 * placing any lakes or pools.
 * Example: you can assign LF1_LAVA to a red dragon level to avoid the
 * generation of ice, acid, oil, etc.
 * See "build_themed_level_nature"
 */
static struct
{
	byte theme;
	u32b flags;		/* A combination of the LF1_* flags */
} themed_level_flags[] =
{
	{LEV_THEME_DEMON_MINOR,	LF1_FIRE},
	{LEV_THEME_DEMON_ALL,	LF1_FIRE},
	{LEV_THEME_DEMON_MAJOR,	LF1_LAVA | LF1_FIRE},
	{LEV_THEME_DRAGON_FIRE,	LF1_FIRE},
	{LEV_THEME_DRAGON_ACID,	LF1_ACID},
	{LEV_THEME_DRAGON_ELEC,	LF1_WATER},
	{LEV_THEME_DRAGON_COLD,	LF1_ICE},
	{LEV_THEME_UNDEAD,		LF1_ICE},
	/* Add entries for more themed levels if needed */
};

/*
 * This table holds the default features used in *almost* all lakes and pools
 * of a themed level.
 * See "pick_proper_feature"
 */
static struct
{
	byte theme;
	u16b feature;
} themed_level_features[] =
{
	{LEV_THEME_DEMON_MINOR,	FEAT_FIRE},
	{LEV_THEME_DEMON_ALL,	FEAT_FIRE},
	{LEV_THEME_DEMON_MAJOR,	FEAT_LAVA},
	{LEV_THEME_DEMON_MAJOR,	FEAT_LAVA_H},
	{LEV_THEME_DEMON_MAJOR,	FEAT_FIRE},
	{LEV_THEME_DRAGON_FIRE,	FEAT_FIRE},
	{LEV_THEME_DRAGON_ACID,	FEAT_ACID},
	{LEV_THEME_DRAGON_ACID,	FEAT_ACID_H},
	{LEV_THEME_DRAGON_ELEC,	FEAT_WATER},
	{LEV_THEME_DRAGON_ELEC,	FEAT_WATER_H},
	{LEV_THEME_DRAGON_COLD,	FEAT_ICE},
	{LEV_THEME_TROLL,		FEAT_FSOIL},
	{LEV_THEME_OGRE,		FEAT_FSOIL},
	{LEV_THEME_OGRE,		FEAT_THICKET},
	/* Add entries for more themed levels if needed */
};

/*
 * A feature pair (wall + floor) that contains also a chance value
 * and a radius
 */
typedef struct
{
	u32b level_flag;
	u16b wall;
	u16b floor;
	u16b chance;
	byte rad;
} feature_selector_item_type;

/*
 * An array of feature pairs. It's used to pick random pairs based
 * on their chance values
 */
typedef struct
{
	feature_selector_item_type items[30];
	byte size;
	u16b total_chance;	/* The sum of the chances of each pair */
} feature_selector_type;

/*
 * Table of "magic" values used to ponder the size of the dungeon
 * 1 means "tiny": 2x2, 2x3, 2x4, 3x2, 3x3, 4x2
 * 2 means "small rectangle": 2x5, 2x6, 3x4, 4x3, 5x2, 6x2
 * 3 means "medium rectangle": 3x5, 3x6, 4x4, 5x3, 6x3
 * 4 means "medium": 4x5, 4x6, 5x4, 6x4
 * 5 means "large": 5x5, 5x6, 6x5
 * 6 means "largest": 6x6
 */
static byte dungeon_size_tab[7][7] =
{
	/*	0	1	2	3	4	5	6	*/
	{	0,	0,	0,	0,	0,	0,	0	},	/* 0 */
	{	0,	0,	0,	0,	0,	0,	0	},	/* 1 */
	{	0,	0,	1,	1,	1,	2,	2	},	/* 2 */
	{	0,	0,	1,	1,	2,	3,	3	},	/* 3 */
	{	0,	0,	1,	2,	3,	4,	4	},	/* 4 */
	{	0,	0,	2,	3,	4,	5,	5	},	/* 5 */
	{	0,	0,	2,	3,	4,	5,	6	},	/* 6 */
};

static bool room_build(int by0, int bx0, int typ);

/*
 * Initialize a feature selector
 */
static void feature_selector_init(feature_selector_type *fs_ptr)
{
	WIPE(fs_ptr, feature_selector_type);
}


/*
 * Add a pair to the feature selector
 * Return FALSE if there isn't more place in the array
 */
static bool feature_selector_add(feature_selector_type *fs_ptr, feature_selector_item_type *item)
{
	/* Check overflow */
	if (fs_ptr->size >= N_ELEMENTS(fs_ptr->items)) return (FALSE);

	/* Paranoia */
	if (item->chance == 0) item->chance = 1;

	/* Copy the pair */
	fs_ptr->items[fs_ptr->size] = *item;

	/* Update the total chance */
	fs_ptr->total_chance += item->chance;

	/* This value is copied to the new pair */
	fs_ptr->items[fs_ptr->size].chance = fs_ptr->total_chance;

	/* One element more */
	++fs_ptr->size;

	/* Success */
	return (TRUE);
}


/*
 * Pick and return a random pair of features
 * Return NULL if there isn't a suitable pair
 */
static feature_selector_item_type *feature_selector_select(feature_selector_type *fs_ptr)
{
	/* We must have something */
	if (fs_ptr->total_chance > 0)
	{
		/* Roll a random chance value */
		int chance = rand_int(fs_ptr->total_chance);

		int i;

		/* Traverse the array of pairs to see which one contains that value */
		for (i = 0; i < fs_ptr->size; i++)
		{
			/* Found one? */
			if (fs_ptr->items[i].chance > chance)
			{
				/* Return the pair */
				return &(fs_ptr->items[i]);
			}
		}
	}

	return (NULL);
}


/*
 * Return a number between 1 and 6 that describes the size of the dungeon
 */
static byte ponder_dungeon_size(void)
{
	/* Get real dungeon size in panels */
	int hgt = (p_ptr->cur_map_hgt / PANEL_HGT);
	int wid = (p_ptr->cur_map_wid / PANEL_WID_FIXED);

	/* Paranoia. Check limits */
	if (hgt > 6) hgt = 6;
	if (hgt < 2) hgt = 2;

	if (wid > 6) wid = 6;
	if (wid < 2) wid = 2;

	/* Ponder size */
	return (dungeon_size_tab[hgt][wid]);
}


/*
 * Return TRUE if the given feature is suitable to make a lake or river
 * on the current level
 */
static bool cave_feat_lake(int f_idx)
{
	/* Get the feature */
	feature_type *f_ptr = &f_info[f_idx];

	/* If this is TRUE we ignore the feature if certain elemental flags already exist in the level */
	bool reject_elements = FALSE;

	/* Require lake or river */
	if (!_feat_ff2_match(f_ptr, FF2_LAKE | FF2_RIVER)) return (FALSE);

	/* Special case. True lava changes all deep features */
	/*if ((current_flags & (LF1_LAVA)) && _feat_ff2_match(f_ptr, FF2_DEEP)) return (TRUE);*/

	/* Analyze the elemental flags */
	switch (_feat_ff3_match(f_ptr, TERRAIN_MASK))
	{
		/* "Boring" features */
		case 0:
		case ELEMENT_SAND:
		case ELEMENT_MUD:
		case ELEMENT_FOREST:
		{
			/* Don't mess with lava levels */
			if (level_flag & (LF1_LAVA)) return (FALSE);

			break;
		}

		case ELEMENT_LAVA:
		{
			/* True lava needs true lava or an empty dungeon */
			if (!(level_flag & (LF1_LAVA))) reject_elements = TRUE;

			break;
		}

		case ELEMENT_BMUD:
		case ELEMENT_BWATER:
		{
			/* These ones need lava, bmud, bwater or an empty dungeon */
			if (!(level_flag & (LF1_LAVA | LF1_BMUD | LF1_BWATER))) reject_elements = TRUE;

			break;
		}

		case ELEMENT_FIRE:
		case ELEMENT_OIL:
		{
			/* These two and true lava are compatible */
			if (!(level_flag & (LF1_OIL | LF1_FIRE | LF1_LAVA))) reject_elements = TRUE;

			break;
		}

		case ELEMENT_ACID:
		{
			/* Acid needs acid or an empty dungeon */
			if (!(level_flag & (LF1_ACID))) reject_elements = TRUE;

			break;
		}

		case ELEMENT_ICE:
		{
			/* Ice needs ice or an empty dungeon */
			if (!(level_flag & (LF1_ICE))) reject_elements = TRUE;

			break;
		}

		case ELEMENT_WATER:
		{
			/* Don't mess with lava levels */
			if (level_flag & (LF1_LAVA)) return (FALSE);

			/* Water needs water, boiling water or ice */
			/* Water is also compatible with acid (flavor) */
			if (!(level_flag & (LF1_ACID | LF1_BWATER | LF1_WATER | LF1_ICE)))
			{
				reject_elements = TRUE;
			}

			break;
		}
	}

	/* Test the presence of certaine flags in the level if necessary */
	if (reject_elements && (level_flag &
		(LF1_LAVA | LF1_FIRE | LF1_OIL | LF1_ACID | LF1_WATER | LF1_ICE | LF1_BMUD | LF1_BWATER)))
	{
		/* Failure */
		return (FALSE);
	}

	/* Success */
	return (TRUE);
}


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
		if (one_in_(2))
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
 * Returns true if you are next to stairs.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 */
static int next_to_stairs(int y, int x)
{
	int k = 0;

	if (cave_ff1_match(y + 1, x, FF1_STAIRS)) k++;
	if (cave_ff1_match(y - 1, x, FF1_STAIRS)) k++;
	if (cave_ff1_match(y, x + 1, FF1_STAIRS)) k++;
	if (cave_ff1_match(y, x - 1, FF1_STAIRS)) k++;

	return (k);
}


/*
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 *
 * We count only stairs, granite walls and permanent walls.
 */
static int next_to_walls(int y, int x)
{
	int k = 0;

	if (cave_ff1_match(y + 1, x, (FF1_WALL | FF1_STAIRS))) k++;
	if (cave_ff1_match(y - 1, x, (FF1_WALL | FF1_STAIRS))) k++;
	if (cave_ff1_match(y, x + 1, (FF1_WALL | FF1_STAIRS))) k++;
	if (cave_ff1_match(y, x - 1, (FF1_WALL | FF1_STAIRS))) k++;

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
 * Returns random co-ordinates for player/monster/object
 * currently only used for player.
 */
static bool new_player_spot_old(void)
{
	int i = 0;

	int yy, xx;

	int x_location_tables [40];
	int y_location_tables [40];
	int rand_spot;

	int tries;

	/* Find a spot for the player */
	for (yy = 0; yy < p_ptr->cur_map_hgt; yy++)
	{
		for (xx = 0; xx < p_ptr->cur_map_wid; xx++)
		{
			/* Must be a "start" floor grid */
			if (!cave_start_bold(yy, xx)) continue;

			/* Refuse to start on anti-teleport grids */
			if (cave_info[yy][xx] & (CAVE_ICKY)) continue;

			/* Refuse to start on room grids if necessary */
			if (!(*dun_cap->can_place_player_in_rooms)() &&
				(cave_info[yy][xx] & (CAVE_ROOM))) continue;

			/* We like to be next to walls */
			if (next_to_walls(yy, xx) < 3) continue;

			/* Don't go over size of array */
			if (i < 40)
			{
				x_location_tables[i] = xx;
				y_location_tables[i] = yy;

				/* Increase the counter */
				i++;
			}

			/* Put it in a random slot */
			else
			{
				rand_spot = rand_int(40);

				x_location_tables[rand_spot] = xx;
				y_location_tables[rand_spot] = yy;
			}

		}
	}

	/* Paranoia */
	if (i == 0) return (FALSE);

	/*
	 * Try to place the player "i" times to reduce the number of failed
	 * levels. -DG-
	 */
	for (tries = 0; tries < i; tries++)
	{
		/* Select a location */
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
	cave_set_feat(y, x, one_in_(5) ? FEAT_RUBBLE_H: FEAT_RUBBLE);
}


/*
 * Pick either an ordinary up staircase or an up shaft.
 */
static int pick_up_stairs(void)
{
	/* No shafts in Moria */
	if (game_mode == GAME_NPPMORIA) return (FEAT_LESS);

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
	/* No shafts in Moria */
	if (game_mode == GAME_NPPMORIA) return (FEAT_MORE);

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
		if ((p_ptr->depth < 2) || one_in_(2))	cave_set_feat(y, x, FEAT_LESS);
		else cave_set_feat(y, x, FEAT_LESS_SHAFT);
	}
	else if (one_in_(2))
	{
		/* No shafts in Moria */
		if (game_mode == GAME_NPPMORIA) cave_set_feat(y, x, FEAT_MORE);
		else if ((quest_check(p_ptr->depth + 1)) || (p_ptr->depth <= 1))
			cave_set_feat(y, x, FEAT_MORE);
		else if (one_in_(2)) cave_set_feat(y, x, FEAT_MORE);
		else cave_set_feat(y, x, FEAT_MORE_SHAFT);
	}
	else
	{
		if (game_mode == GAME_NPPMORIA) cave_set_feat(y, x, FEAT_LESS);
		else if ((one_in_(2)) || (p_ptr->depth == 1)) cave_set_feat(y, x, FEAT_LESS);
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

			switch (typ)
			{
				/* Some objects need specific terrain types */
				case ALLOC_TYP_GOLD:
				case ALLOC_TYP_OBJECT:
				case ALLOC_TYP_CHEST:
				case ALLOC_TYP_PARCHMENT:
				{
					/* Require "clean" grid */
					if (!cave_clean_bold(y, x)) continue;

					/* Hack -- Ensure object/gold creation */
					if (f_info[cave_feat[y][x]].dam_non_native > 0) continue;

					break;
				}
				/* Traps */
				case ALLOC_TYP_TRAP:
				{
					/* Require "trappable" floor grid */
					if (!cave_trappable_bold(y, x)) continue;

					/* Require "empty" grid */
					if (cave_m_idx[y][x]) continue;

					break;
				}

				/* ALLOC_TYP_RUBBLE */
				default:
				{
					/* Require "naked" floor grid */
					if (!cave_naked_bold(y, x)) continue;

					break;
				}
			}

			/* Check for "room" */
			is_room = ((cave_info[y][x] & (CAVE_ROOM)) != 0);

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
				place_trap(y, x, 0);
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
			case ALLOC_TYP_PARCHMENT:
			{
				object_type object_type_body;
				object_type *i_ptr = &object_type_body;

				/* Use a small wooden chest */
				int k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);

				/* Get a random chest */
				object_wipe(i_ptr);
				object_prep(i_ptr, k_idx);
				apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);

				/*Don't let the player see what the object it, and make it a quest item*/
				i_ptr->ident |= (IDENT_HIDE_CARRY | IDENT_QUEST);

				object_aware(i_ptr);
				object_known(i_ptr);

				/* Give it to the floor */
				(void)floor_carry(y, x, i_ptr);
				break;
			}
			default: break;
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
static bool build_streamer(u16b feat, int chance)
{
	int i, tx, ty;
	int y, x, dir;
	int tries1 = 0;
	int tries2 = 0;
	u16b new_feat;

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
				tries2++;
				if (tries2 > 2500) return (FALSE);
				ty = rand_spread(y, d);
				tx = rand_spread(x, d);
				if (!in_bounds(ty, tx)) continue;
				break;
			}

			/* Only convert "granite" walls */
			if (!strstr(f_name + f_info[cave_feat[ty][tx]].name, "granite")) continue;

			/* We'll add the given vein type */
			new_feat = feat;

			/* Hack -- Add some (known) treasure */
			if (one_in_(chance))
			{
				/* Quartz with treasure? */
				if (feat == FEAT_QUARTZ)
				{
					new_feat = FEAT_QUARTZ_H;
				}
				/* Magma with treasure? */
				else if (feat == FEAT_MAGMA)
				{
					new_feat = FEAT_MAGMA_H;
				}
			}

			/* Clear previous contents, add proper vein type */
			cave_set_feat(ty, tx, new_feat);
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

					/* Burn stuff */
					if (cave_ff2_match(y, x, FF2_HURT_FIRE))
					{
						cave_alter_feat(y, x, FS_HURT_FIRE);
					}

					/* Granite */
					else if (t < 20)
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

					/* Rubble */
					else if (t < 130)
					{
						/* Create rubble */
						cave_set_feat(y, x, FEAT_RUBBLE);
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

		/* Require "trappable" floor grids. Ignore monsters */
 		if (!cave_trappable_bold(y1, x1)) continue;

		/* Place the trap */
		place_trap(y1, x1, 0);

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
			(void)place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP));

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
	/*
	 * Rooms can be placed over big lit lakes sometimes. We have
	 * to explicitly remove the light in those cases. Note that outer
	 * walls are ignored (you can see the room from the lake).
	 */
	if (!light)
	{
		for (y = y1 + 1; y < y2; y++)
		{
			for (x = x1 + 1; x < x2; x++)
			{
				cave_info[y][x] &= ~(CAVE_GLOW);
			}
 		}
 	}
 }


/*
 * Generate helper -- fill a rectangle with a feature
 */
static void generate_fill(int y1, int x1, int y2, int x2, u16b feat)
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
static void generate_draw(int y1, int x1, int y2, int x2, u16b feat)
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
static void generate_plus(int y1, int x1, int y2, int x2, u16b feat)
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
static void generate_open(int y1, int x1, int y2, int x2, u16b feat)
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
static void generate_hole(int y1, int x1, int y2, int x2, u16b feat)
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
	if (effective_depth(p_ptr->depth) <= randint(25)) light = TRUE;

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
	if (effective_depth(p_ptr->depth) <= randint(25)) light = TRUE;

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
	if (effective_depth(p_ptr->depth) <= randint(25)) light = TRUE;

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
			generate_hole(y1b, x1a, y2b, x2a, get_secret_door_num());

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
			if (one_in_(3))
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
				if (one_in_(3))
				{
					generate_open(y1b-1, x1a-1, y2b+1, x2a+1, get_secret_door_num());
				}
			}

			/* Occasionally put a "plus" in the center */
			else if (one_in_(3))
			{
				generate_plus(y1b, x1a, y2b, x2a, FEAT_WALL_INNER);
			}

			/* Occasionally put a "pillar" in the center */
			else if (one_in_(3))
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
	if (effective_depth(p_ptr->depth) <= randint(25)) light = TRUE;

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
			generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

			/* Place a monster in the room */
			vault_monsters(y0, x0, 1);

			break;
		}

		/* An inner room with a small inner room */
		case 2:
		{
			u16b feat;

			/* Open the inner room with a secret door */
			generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

			/* Place another inner room */
			generate_draw(y0-1, x0-1, y0+1, x0+1, FEAT_WALL_INNER);

			/*Get a locked door*/

			/*Set the hook */
			get_feat_num_hook = vault_locked_door;
			get_feat_num_prep();

			/* Click! */
			feat = get_feat_num(effective_depth(p_ptr->depth));

			/* Clear the hook */
			get_feat_num_hook = NULL;
			get_feat_num_prep();

			/* Open the inner room with a locked door */
			generate_hole(y0-1, x0-1, y0+1, x0+1, feat);

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
			generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

			/* Inner pillar */
			generate_fill(y0-1, x0-1, y0+1, x0+1, FEAT_WALL_INNER);

			/* Occasionally, two more Large Inner Pillars */
			if (one_in_(2))
			{
				/* Three spaces */
				if (one_in_(2))
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
			if (one_in_(3))
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
				if (one_in_(3)) place_object(y0, x0 - 2, FALSE, FALSE, DROP_TYPE_UNTHEMED);
				if (one_in_(3)) place_object(y0, x0 + 2, FALSE, FALSE, DROP_TYPE_UNTHEMED);
			}

			break;
		}

		/* An inner room with a checkerboard */
		case 4:
		{
			/* Open the inner room with a secret door */
			generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

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
			if (one_in_(2))
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
 * Helper function for "monster pit (orc, orge, troll, giant)"
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
 * Helper function for "monster pit (servants of the valar)"
 */
static bool vault_aux_valar_servant(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Hack -- Require "A" monsters */
	if (!strchr("A", r_ptr->d_char)) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (dragon)"
 */
static bool vault_aux_dragon_elem(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	u32b breath_mask;

	/* Decline unique monsters if a nest or pit*/
	if ((!allow_uniques) &&
		(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Hack -- Require "d" or "D" monsters */
	if (!strchr("Dd", r_ptr->d_char)) return (FALSE);

	/*no player ghosts*/
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	 /* Get the elemental flags of this race */
	breath_mask = (r_ptr->flags4 & RF4_ELEM_BREATH_MASK);

	/* Ignore non-elemental and chromatic dragons */
	if (!breath_mask || (breath_mask == RF4_ELEM_BREATH_MASK)) return (FALSE);

	/* Ignore dragons that breath something else (dracolichs, dracolisks, etc.) */
	if ((r_ptr->flags4 & RF4_BREATH_MASK) != breath_mask) return (FALSE);

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

	/*check the demon flag*/
	if (!(r_ptr->flags3 & (RF3_DEMON))) return (FALSE);

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

	/*check the demon flag*/
	if (!(r_ptr->flags3 & (RF3_DEMON))) return (FALSE);

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

	/*check the demon flag*/
	if (!(r_ptr->flags3 & (RF3_DEMON))) return (FALSE);

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
	else if (theme == LEV_THEME_DRAGON_ELEMENTAL)
	{
		get_mon_num_hook = vault_aux_dragon_elem;
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
	else if (theme == LEV_THEME_DRAGON_ELEMENTAL) get_mon_num_hook = vault_aux_dragon_elem;
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
	else if (theme == LEV_THEME_VALAR_SERVANTS)	get_mon_num_hook = vault_aux_valar_servant;
}

/*return a theme for a monster nest*/
byte get_nest_theme(int nestlevel, bool quest_theme)
{
	int mindepth, whatnest;

	/*enforce minimum depth, to keep weak nests out of deep levels*/
	mindepth = nestlevel / 4;

	/*keep total to 100 or less*/
	if ((mindepth + nestlevel) > 100) nestlevel = 100 - mindepth;

	/* Hack -- Choose a nest type */
	whatnest = randint(nestlevel);

	/* Try for harder themes for quests */
	if (quest_theme)
	{
		int whatnest2 = randint(nestlevel);
		int whatnest3 = randint(nestlevel);
		if (whatnest2 > whatnest) whatnest = whatnest2;
		if (whatnest3 > whatnest) whatnest = whatnest3;

	}

	whatnest += mindepth;

	if ((whatnest <= 25)  && (nestlevel <= 35))
	{
		/*coins, jelly, or kobolds/yeeks/orcs*/
		if (one_in_(3))			return LEV_THEME_CREEPING_COIN;
		else if (one_in_(2))	return LEV_THEME_JELLY;
		else					return LEV_THEME_ORC_NAGA_YEEK_KOBOLD;
	}

	/*cave dwellers or young dragons*/
	else if (whatnest <= 50)
	{
		if (one_in_(2))			return LEV_THEME_CAVE_DWELLER;
		else 					return LEV_THEME_DRAGON_YOUNG;
	}

	/*Animals or humanoids*/
	else if (whatnest <=75)
	{
		if (one_in_(2))			return LEV_THEME_ANIMAL;
		else					return LEV_THEME_HUMANOID;
	}

	/*Monster nest (undead) */
	else if (whatnest <=95)
	{
		if (one_in_(2))			return LEV_THEME_VALAR_SERVANTS;
		else 					return LEV_THEME_UNDEAD;
	}

	/*Ancient Dragon Nest*/
	else						return LEV_THEME_DRAGON_ANCIENT;
}


/*
 * return a theme for a monster pit
 */
byte get_pit_theme(int pitlevel, bool quest_theme)
{
	int mindepth, whatpit;

	/*enforce minimum depth, to keep weak nests out of deep levels*/
	mindepth = pitlevel / 4;

	/*keep total to 100 or less, so results aren't skewed at deep depths*/
	if ((mindepth + pitlevel) > 100) pitlevel = 100 - mindepth;

	/* Hack -- Choose a nest type */
	whatpit = randint(pitlevel);

	/* Try for harder themes for quests */
	if (quest_theme)
	{
		int whatpit2 = randint(pitlevel);
		int whatpit3 = randint(pitlevel);
		if (whatpit2 > whatpit) whatpit = whatpit2;
		if (whatpit3 > whatpit) whatpit = whatpit3;
	}

	whatpit += mindepth;

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
	else if ((whatpit <= 50) && (effective_depth(p_ptr->depth) <= 60))
	{
		/* Hound, youngdragon, or hydra pit */
		if (one_in_(2))			return LEV_THEME_HOUND;
		else					return LEV_THEME_DRAGON_YOUNG;
	}

	/* Giant pit */
	else if ((whatpit <= 60) && (effective_depth(p_ptr->depth) <= 80))
	{
		if (one_in_(2))		return LEV_THEME_VALAR_SERVANTS;
		else 				return LEV_THEME_GIANT;
	}

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

	/* Either ancient Dragon pit, Major Demon pit, or Servant of the Valar Pit */
	else
	{
		/* Pick dragon type */
		switch (rand_int(3))
		{
			case 1: return LEV_THEME_DRAGON_ACID;
			case 2: return LEV_THEME_DRAGON_ANCIENT;
			default:return LEV_THEME_VALAR_SERVANTS;
		}
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
	generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());
	generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

	/*select the theme, or get the quest level theme*/
	if (is_quest_level) room_theme = q_ptr->q_theme;
	else room_theme = get_nest_theme(effective_depth(p_ptr->depth), FALSE);

	/*get the mon_hook*/
	get_mon_hook(room_theme);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(effective_depth(p_ptr->depth) +
							  (is_quest_level ? PIT_NEST_QUEST_BOOST : NEST_LEVEL_BOOST), y1, x1, MPLACE_NO_GHOST);

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
	if ((effective_depth(p_ptr->depth) <= 40) &&
	    (randint(effective_depth(p_ptr->depth) * effective_depth(p_ptr->depth) + 1) < 300))
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
	harder_nest_check = r_info[what[63]].level - effective_depth(p_ptr->depth);

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
			(void)place_monster_aux(y, x, r_idx, 0L);
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
		int counter = 19 * 5;
		int bonus_items = 5;

		q_ptr->q_num_killed = 0;
		q_ptr->q_max_num = 0;

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
					q_ptr->q_max_num ++;

					/* Randomly give 5 monsters a bonus item to drop. */
					/* Paranoia*/
					if ((bonus_items) && (counter))
					{
						if ((randint0(counter)) < bonus_items)
						{
							m_ptr->mflag |= (MFLAG_BONUS_ITEM);
							bonus_items--;
						}
						counter--;
					}
				}
				/* Mark quests for mimics where an object is created rather than a monster */
				else if (cave_o_idx[y][x] > 0)
				{
					object_type *o_ptr = &o_list[cave_o_idx[y][x]];

					if (o_ptr->mimic_r_idx)
					{
						o_ptr->ident |= (IDENT_QUEST);

						/*increase the max_num counter*/
						q_ptr->q_max_num ++;
					}
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
 * The inside room in a monster pit appears as shown in tables.c.
 *
 * Note that the monsters in the pit are now chosen by using "get_mon_num()"
 * to request 16 "appropriate" monsters, sorting them by level, and using
 * half of the entries in this sorted list for the contents of the pit.
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

	int i, j, y, x, y1, x1, y2, x2, row, col;

	bool empty = FALSE;

	int light = FALSE;

	byte pit_theme;

	byte is_quest_level = FALSE;

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* Pick one of the pit patterns */
	byte which_pit = randint0(MAX_PIT_PATTERNS);

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
	generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());
	generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

	/* Choose a pit type */
	if(is_quest_level) pit_theme = q_ptr->q_theme;
	else pit_theme = get_pit_theme(effective_depth(p_ptr->depth), FALSE);

	/*get the monster hook*/
	get_mon_hook(pit_theme);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick some monster types */
	for (i = 0; i < 16; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(effective_depth(p_ptr->depth) +
							  (is_quest_level ? PIT_NEST_QUEST_BOOST : PIT_LEVEL_BOOST), y1, x1, MPLACE_NO_GHOST);

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
	harder_pit_check = r_info[what[15]].level - effective_depth(p_ptr->depth);

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
	if ((effective_depth(p_ptr->depth) <= 40) &&
	    (randint(effective_depth(p_ptr->depth) * effective_depth(p_ptr->depth) + 1) < 300))
	{
		good_item_flag = TRUE;
	}

	/* Place the monsters in one of the patterns in tables.c */
	for (row = 0, y = y0 - 2; y <= y0 + 2; y++, row++)
	{
		for (col = 0, x = x0 - 9; x <= x0 + 9; x++, col++)
		{
			int r_idx = what[pit_room_maps[which_pit][row][col]];

			/* Place that "random" monster (no groups) */
			(void)place_monster_aux(y, x, r_idx, 0L);
		}
	}

	/* No teleporting inside pits/nests*/
	for (y = y0 - 2; y <= y0 + 2; y++)
	{
		for (x = x0 - 9; x <= x0 + 9; x++)
		{
			/* Mark it */
			cave_info[y][x] |= (CAVE_ICKY);
		}
	}

	/* Final preps if this is a quest level */
	if (is_quest_level)
	{
		int counter = 19 * 5;
		int bonus_items = 5;

		q_ptr->q_num_killed = 0;
		q_ptr->q_max_num = 0;

		/* Square-by-square grid search for monsters */
		for (y = y0 - 2; y <= y0 + 2; y++)
		{
			for (x = x0 - 9; x <= x0 + 9; x++)
			{
				/* Is there a monster here? */
				if (cave_m_idx[y][x] > 0)
				{
					monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

					/*mark it as a quest monster*/
					m_ptr->mflag |= (MFLAG_QUEST);

					/*increase the max_num counter*/
					q_ptr->q_max_num ++;

					/* Randomly give 5 monsters a bonus item to drop. */
					/* Paranoia*/
					if ((bonus_items) && (counter))
					{
						if ((randint0(counter)) < bonus_items)
						{
							m_ptr->mflag |= (MFLAG_BONUS_ITEM);
							bonus_items--;
						}
						counter--;
					}
				}
				/* Mark quests for mimics where an object is created rather than a monster */
				else if (cave_o_idx[y][x] > 0)
				{
					object_type *o_ptr = &o_list[cave_o_idx[y][x]];

					if (o_ptr->mimic_r_idx)
					{
						o_ptr->ident |= (IDENT_QUEST);

						/*increase the max_num counter*/
						q_ptr->q_max_num ++;
					}
				}
			}
		}
	}
}


/*
 * Determine if a monster is suitable for the vault
 */
static bool monster_vault_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* No lurkers or trappers */
	if (strchr(".", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Hack -- fill in "vault" rooms
 */
static void build_vault(int y0, int x0, const vault_type *v_ptr)
{
	int dx, dy, x, y;
	int ax, ay;
	bool flip_v = FALSE;
	bool flip_h = FALSE;
	int ymax = v_ptr->hgt;
	int xmax = v_ptr->wid;
	cptr data = v_text + v_ptr->text;

	byte quest_artifact_spots = 0;

	cptr t;
	int quest_type = quest_check(p_ptr->depth);
	bool quest_greater_vault = FALSE;
	bool quest_special_vault = FALSE;
	if (quest_type == QUEST_VAULT)
	{
		artifact_type *a_ptr = &a_info[QUEST_ART_SLOT];
		if (v_ptr->typ == 9) quest_special_vault = TRUE;

		/*Hack - allow the quest artifact to be created */
		a_ptr->a_cur_num = 0;
		a_ptr->a_max_num = 0;
	}
	else if (quest_type == QUEST_GREATER_VAULT)
	{
		if (v_ptr->typ == 8) quest_greater_vault = TRUE;
	}

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
					/* For vault quests, make it a normal empty square */
					if (quest_greater_vault) cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

					/* Otherwise a wall */
					else cave_set_feat(y, x, FEAT_WALL_OUTER);

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
						place_trap(y, x, 0);
					}
					break;
				}

				/* Secret doors */
				case '+':
				{
					place_locked_door(y, x);
					break;
				}

				/* Trap */
				case '^':
				{
					place_trap(y, x, 0);
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

	/*get the hook*/
	get_mon_num_hook = monster_vault_okay;

	/* Prepare allocation table */
	get_mon_num_prep();

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
					monster_level = effective_depth(p_ptr->depth) + 4;
					place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
					monster_level = effective_depth(p_ptr->depth);
					break;
				}

				/* Meaner monster */
				case '@':
				{
					monster_level = effective_depth(p_ptr->depth) + 8;
					place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
					monster_level = effective_depth(p_ptr->depth);
					break;
				}

				/* Meaner monster, plus treasure */
				case '9':
				{
					monster_level = effective_depth(p_ptr->depth) + 7;
					place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
					monster_level = effective_depth(p_ptr->depth);
					object_level = effective_depth(p_ptr->depth) + 7;
					place_object(y, x, TRUE, FALSE, DROP_TYPE_UNTHEMED);
					object_level = effective_depth(p_ptr->depth);
					break;
				}

				/* Nasty monster and treasure */
				case '8':
				{
					monster_level = effective_depth(p_ptr->depth) + 20;
					place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
					monster_level = effective_depth(p_ptr->depth);
					object_level = effective_depth(p_ptr->depth) + 15;
					place_object(y, x, TRUE, TRUE, DROP_TYPE_UNTHEMED);
					object_level = effective_depth(p_ptr->depth);
					break;
				}

				/* Nasty monster and a chest */
				case '~':
				{
					monster_level = effective_depth(p_ptr->depth) + 20;
					place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
					monster_level = effective_depth(p_ptr->depth);
					object_level = effective_depth(p_ptr->depth) + 15;
					place_object(y, x, FALSE, FALSE, DROP_TYPE_CHEST);
					object_level = effective_depth(p_ptr->depth);
					break;
				}

				/* Quest chest */
				case 'Q':
				{
					bool placed_quest_artifact = FALSE;
					monster_level = p_ptr->depth + 10;
					place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
					monster_level = p_ptr->depth;

					/*randomly pick from several quest artifacts spots to place the artifact*/
					if (!a_info[QUEST_ART_SLOT].a_cur_num)
					{
						if (one_in_(quest_artifact_spots))
						{
							placed_quest_artifact = (place_quest_artifact(y, x));
						}
					}
					if (!placed_quest_artifact)
					{
						/*place a decent sized object*/
						object_level = effective_depth(p_ptr->depth) + 7;
						place_object(y, x, TRUE, FALSE, DROP_TYPE_UNTHEMED);
						object_level = effective_depth(p_ptr->depth);

						/*This quest artifact spot is no longer an option*/
						quest_artifact_spots --;
					}

					break;
				}

				/* Monster and/or object */
				case ',':
				{
					if (one_in_(2))
					{
						monster_level = effective_depth(p_ptr->depth) + 3;
						place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
						monster_level = effective_depth(p_ptr->depth);
					}
					if (one_in_(2))
					{
						object_level = effective_depth(p_ptr->depth) + 5;
						place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
						object_level = effective_depth(p_ptr->depth);
					}
					break;
				}
			}

			/*mark it as a quest monster*/
			if ((cave_m_idx[y][x] > 0) && (quest_greater_vault || quest_special_vault))
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

				m_ptr->mflag |= (MFLAG_QUEST);
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

	/* Reset the allocation table */
	get_mon_num_hook = NULL;
	get_mon_num_prep();
}

/*
 * Type 7 -- simple vaults (see "vault.txt")
 */
static void build_type_lesser_vault(int y0, int x0)
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
	if ((effective_depth(p_ptr->depth) <= 50) ||
	    (randint((effective_depth(p_ptr->depth)-40) * (effective_depth(p_ptr->depth)-40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr);
}


/*
 * Mark greater vault grids with the CAVE_G_VAULT flag.
 */
static void mark_g_vault(int y0, int x0, int hgt, int wid)
{
	int y1, x1, y2, x2, y, x;
	/* Queue of reachable grids */
	grid_queue_type queue, *q_ptr = &queue;

	/* Get the corners */
	y1 = y0 - hgt / 2;
	x1 = x0 - wid / 2;
	y2 = y1 + hgt - 1;
	x2 = x1 + wid - 1;

	/* Step 1 - Mark all vault grids with the new flag */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_info[y][x] |= (CAVE_G_VAULT);
		}
	}

	/*
	 * Step 2 - Most vaults have an inner room, which usually contains
	 * the good stuff. Unmark all walls and floors outside this inner room
	 */

	/* We need a perimeter of outer walls surrounding the vault */
	if (cave_feat[y1][x1] != FEAT_WALL_OUTER) return;

	/* Create the queue */
	grid_queue_create(q_ptr, 500);

	/* Append the top-left corner of the vault to the queue */
	grid_queue_push(q_ptr, y1, x1);

	/* Unmark that grid */
	cave_info[y1][x1] &= ~(CAVE_G_VAULT);

	/* Process all the reachable vault grids */
	while (!GRID_QUEUE_EMPTY(q_ptr))
	{
		int d;

		/* Get the coordinates of the grid in the head of the queue */
		y = GRID_QUEUE_Y(q_ptr);
		x = GRID_QUEUE_X(q_ptr);

		/* Remove that grid from the queue */
		grid_queue_pop(q_ptr);

		/* Scan adjacent grids */
		for (d = 0; d < 8; d++)
		{
			/* Get coordinates */
			int yy = y + ddy_ddd[d];
			int xx = x + ddx_ddd[d];

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/*
			 * Ignore already processed grids, or grids outside
			 * the vault
			 */
			if (!(cave_info[yy][xx] & (CAVE_G_VAULT))) continue;

			/* Unmark that grid, its type doesn't matter */
			cave_info[yy][xx] &= ~(CAVE_G_VAULT);

			/* But keep processing only certain grid types */
			if (cave_ff1_match(yy, xx, FF1_FLOOR) ||
				(cave_feat[yy][xx] == FEAT_WALL_OUTER))
			{
				/* Append that grid to the queue */
				grid_queue_push(q_ptr, yy, xx);
			}
		}
	}

	/* Free resources */
	grid_queue_destroy(q_ptr);
}


/*
 * Type 8 -- greater vaults (see "vault.txt")
 */
static void build_type_greater_vault(int y0, int x0)
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

	/* Build the vault */
	build_vault(y0, x0, v_ptr);

	/* Mark vault grids with the CAVE_G_VAULT flag */
	mark_g_vault(y0, x0, v_ptr->hgt, v_ptr->wid);

	/* Remember the vault's name */
	my_strcpy(g_vault_name, v_name + v_ptr->name, sizeof(g_vault_name));
}


/*
 * Type 9 -- quest vaults (see "vault.txt")
 */
static void build_type_special_vault(int y0, int x0)
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
	if (cheat_room) msg_format("Special vault (%s)", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr);
}


/*
 * Note that the order we generate the dungeon is terrain features, then
 * rooms, then corridors, then streamers. This is important, because
 * (currently) we ensure that deep or hostile terrain is bridged by safe
 * terrain, and rooms (and vaults) alway have their respective walls intact.
 *
 * Note that rooms can be generated inside 'big' lakes, but not on regular
 * lakes. We take a risk here that 'big' lakes are less likely to have
 * areas rendered inaccessible by having a room block them.
 *
 * XXX XXX XXX Currently both types of lakes can have areas that are completely
 * blocked because of the 20% chance of filling a lake centre location with
 * a lake edge location. We should always guarantee that all areas are connected.
 *
 * XXX XXX These huge case statements should be cut down by using WALL, FLOOR,
 * etc. flags to take out the common cases and ensuring we never overwrite a
 * dun square with an edge square. But the resulting code might be less
 * efficient.
 */

/*
 * Places a terrain on another terrain
 */

void build_terrain(int y, int x, int feat)
{
	int oldfeat, newfeat;
	int k;

	feature_type *f_ptr;
	feature_type *f2_ptr;

	/* Get the feature */
	oldfeat = cave_feat[y][x];
	f_ptr = &f_info[oldfeat];

	/* Set the new feature */
	newfeat = oldfeat;
	f2_ptr = &f_info[feat];

	/* Paranoia */
	if (!oldfeat)
	{
		newfeat = feat;
	}
	/* Put the feature quickly if we are overriding boring walls */
	else if (_feat_ff1_match(f_ptr, FF1_WALL) &&
		!_feat_ff3_match(f_ptr, TERRAIN_MASK))
	{
		newfeat = feat;
	}
	/* Tunnel the old feature */
	else if (_feat_ff1_match(f2_ptr, FF1_FLOOR) &&
		_feat_ff1_match(f_ptr, FF1_CAN_TUNNEL))
	{
		newfeat = feat_state(oldfeat, FS_TUNNEL);
	}
	/* Chasm edge, stone bridge */
	else if (_feat_ff2_match(f2_ptr, FF2_BRIDGED))
	{
		newfeat = feat;
	}
	/*
	 * EXPERIMENTAL. Leave some grids untouched when overlapping lakes.
	 * Note that we check for a match of the LOS, PROJECT and MOVE flags to
	 * support rivers and tree branches properly (this is a hack).
	 */
	else if (_feat_ff3_match(f2_ptr, TERRAIN_MASK) &&
		_feat_ff3_match(f_ptr, TERRAIN_MASK) &&
		(_feat_ff1_match(f2_ptr, FF1_MOVE | FF1_LOS | FF1_PROJECT) ==
		_feat_ff1_match(f_ptr, FF1_MOVE | FF1_LOS | FF1_PROJECT)) &&
		one_in_(4))
	{
		newfeat = oldfeat;
	}
	/* Handle new lava */
	else if (_feat_ff3_match(f2_ptr, FF3_LAVA))
	{
		/* We are filling a hole in the dungeon */
		if (_feat_ff2_match(f_ptr, FF2_DEEP) &&
			!_feat_ff3_match(f_ptr, FF3_LAVA))
		{
			/* Heat the water */
			if (_feat_ff3_match(f_ptr, FF3_WATER))
			{
				newfeat = FEAT_BWATER;
			}
			/* Melt the old feature */
			else
			{
				newfeat = FEAT_BMUD;
			}
		}
		/* Burn old features */
		else if (_feat_ff2_match(f_ptr, FF2_HURT_FIRE))
		{
			newfeat = feat_state(oldfeat, FS_HURT_FIRE);
		}
		/* Lava overrides all */
		else
		{
			newfeat = feat;
		}
	}
	/* Handle old lava */
	else if (_feat_ff3_match(f_ptr, FF3_LAVA))
	{
		/* We are digging a hole in the lava */
		if (_feat_ff2_match(f2_ptr, FF2_DEEP))
		{
			/* Heat the water */
			if (_feat_ff3_match(f2_ptr, FF3_WATER))
			{
				newfeat = FEAT_BWATER;
			}
			/* Melt the new feature */
			else
			{
				newfeat = FEAT_BMUD;
			}
		}
	}

	/* Handle new fire */
	else if (_feat_ff3_match(f2_ptr, FF3_FIRE))
	{
		/* Burn the old feature */
		if (_feat_ff2_match(f_ptr, FF2_HURT_FIRE))
		{
			newfeat = feat_state(oldfeat, FS_HURT_FIRE);
		}
		/* Some features resist fire */
		else if (!_feat_ff3_match(f_ptr, FF3_ICE | FF3_WATER))
		{
			newfeat = feat;
		}
	}

	/* Preserve tree branches  */
	else if (_feat_ff3_match(f_ptr, FF3_NEED_TREE))
	{
		newfeat = oldfeat;
	}

	/* Handle new ice */
	else if (_feat_ff3_match(f2_ptr, FF3_ICE) &&
		_feat_ff2_match(f_ptr, FF2_HURT_COLD))
	{
		newfeat = feat_state(oldfeat, FS_HURT_COLD);
	}
	/* Handle new water */

	else if (_feat_ff3_match(f2_ptr, FF3_WATER))
	{
		if (!_feat_ff3_match(f_ptr, FF3_WATER))
		{
			newfeat = feat;
		}
		/* Note that old water is unnafected to avoid "wave lakes" */
		else if ((_feat_ff3_match(f_ptr, TERRAIN_MASK) != FF3_WATER) &&
 			_feat_ff2_match(f_ptr, FF2_HURT_WATER))
		{
			newfeat = feat_state(oldfeat, FS_HURT_WATER);
		}
	}
	/* All special cases were ignored. Put the feature */
	else
	{
		newfeat = feat;
	}

	/* Hack -- no change */
	if (newfeat == oldfeat) return;

	/* Get the chance to replace a feature */
	k = randint(100);

	/* Replace some features with something else (sometimes) */
	switch (newfeat)
	{
		case FEAT_LIMESTONE:
		{
			if (k <= 40) newfeat = FEAT_FLOOR;

			else if (k <= 60) newfeat = FEAT_WATER;

			break;
		}

		case FEAT_FLOOR_MUD:
		{
			if (k <= 40) newfeat = FEAT_MUD;

			break;
		}

		case FEAT_MUD_H:
		{
			if (k <= 10) newfeat = FEAT_FLOOR_EARTH;

			else if (k <= 23) newfeat = FEAT_FLOOR_MUD;

			else if (k <= 24) newfeat = FEAT_TREE;

			break;
		}

		case FEAT_BMUD:
		{
			if (k <= 10) newfeat = FEAT_BWATER;

			break;
		}

		case FEAT_BWATER:
		{
			if (k <= 10) newfeat = FEAT_MAGMA;

			else if (k <= 13) newfeat = FEAT_GEYSER;

			break;
		}

		case FEAT_FSOIL:
		{
			if (k <= 10) newfeat = FEAT_TREE;

			else if (k <= 20) newfeat = FEAT_FSOIL_D;

			else if (k <= 25) newfeat = oldfeat;

			else if (k <= 35) newfeat = FEAT_FSOIL_DYNAMIC;

			break;
		}

		case FEAT_ICE:
		{
			if (k <= 5) newfeat = FEAT_ICE_WALL_C;

			break;
		}

		case FEAT_ICE_WALL:
		{
			if (k <= 25) newfeat = FEAT_ICE_WALL_C;

			else if (k <= 50) newfeat = FEAT_ICE;

			break;
		}

		case FEAT_ICE_WALL_C:
		{
			if (k <= 90) newfeat = FEAT_ICE_WALL;

			break;
		}

		case FEAT_ACID:
		case FEAT_ACID_H:
		{
			if (k <= 5)
			{
				newfeat = FEAT_GRANITE_C;
			}

			break;
		}

		case FEAT_COAL:
		{
			if (k <= 5) newfeat = FEAT_BCOAL;

			else if (k <= 50) newfeat = FEAT_BURNT_S;

			else if (k <= 60) newfeat = FEAT_FIRE;

			break;
		}

		case FEAT_SHALE:
		{
			if (k <= 5) newfeat = FEAT_COAL;

			break;
		}

		case FEAT_SAND:
		{
			if (k <= 5) newfeat = FEAT_SANDSTONE;

			else if (k <= 10) newfeat = FEAT_QUARTZ;

			break;
		}

		case FEAT_SAND_H:
		{
			if (k <= 3) newfeat = FEAT_QUARTZ;

			else if (k <= 4) newfeat = FEAT_L_ROCK;

			else if (k <= 5) newfeat = FEAT_ROCK;

			else if (k <= 35) newfeat = FEAT_DUNE;

			break;
		}

		case FEAT_THICKET:
		{
			if (k <= 5) newfeat = FEAT_BUSH;

			else if (k <= 15) newfeat = FEAT_THORNS;

			else if (k <= 20) newfeat = FEAT_BRAMBLES;

			else if (k <= 21) newfeat = FEAT_TREE;

			else if (k <= 25) newfeat = FEAT_PEBBLES;

			else if (k <= 30)
			{
				if (level_flag & (ELEMENT_WATER)) newfeat = FEAT_WATER;
				else newfeat = FEAT_MUD;
			}

			else if (k <= 40) newfeat = FEAT_MUD;

			else if (k <= 70) newfeat = FEAT_EARTH;

			break;
		}

		case FEAT_LAVA:
		case FEAT_LAVA_H:
		{
			if (k <= 10) newfeat = FEAT_WALL_FIRE;

			else if (k <= 15) newfeat = FEAT_SCORCHED_WALL;

			break;
		}

		case FEAT_FLOOR_EARTH:
		{
			if (k <= 1) newfeat = FEAT_L_ROCK;

			else if (k <= 5) newfeat = FEAT_ROCK;

			else if (k <= 10) newfeat = FEAT_PEBBLES;

			break;
		}

		case FEAT_FIRE:
		{
			if (k <= 25) newfeat = FEAT_BURNT_S;

			break;
		}

	}

	/* Hack -- no change */
	if (newfeat == oldfeat) return;

	/* Set the new feature */
	cave_set_feat(y, x, newfeat);
}


/*
 * Returns TRUE if f_idx is a valid pool feature
 */
static bool cave_feat_pool(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Hack -- Ignore ice pools on non-ice levels */
	if (!(level_flag & LF1_ICE) && _feat_ff3_match(f_ptr, FF3_ICE))
	{
		return (FALSE);
	}

	/* Hack -- Ignore solid features */
	if (!_feat_ff1_match(f_ptr, FF1_MOVE))
	{
		return (FALSE);
	}

	/* All remaining lake features will be fine */
	return (cave_feat_lake(f_idx));
}


/*
 * Choose a terrain feature for the current level.
 * You can use a hook to ensure consistent terrain (lakes/pools).
 * This function handles themed levels as a special case. The "feeling"
 * global variable must be properly set to recognize the themed level. See
 * "build_themed_level".
 */
static u16b pick_proper_feature(bool (*feat_hook)(int f_idx))
{
	/* Default depth for the feature */
	int max_depth = effective_depth(p_ptr->depth);
	u16b feat;

	/* Special case - Themed levels with default features */
	/* Note that we have a small chance to ignore these features */
	if ((feeling >= LEV_THEME_HEAD) && (rand_int(100) < 75))
	{
		/* Get the theme */
		byte theme = feeling - LEV_THEME_HEAD;
		u16b features[10];
		u16b i, n;

		/* Find if we have to use default features for this level */
		for (i = n = 0; i < N_ELEMENTS(themed_level_features); i++)
		{
			/* Ignore mismatching themes */
			if (theme != themed_level_features[i].theme) continue;

			/* Get the feature */
			feat = themed_level_features[i].feature;

			/* Ignore features who are too deep for the player */
			if (f_info[feat].f_level > effective_depth(p_ptr->depth) + 25) continue;

			/* IMPORTANT - Check consistency with the level */
			if (feat_hook && !feat_hook(feat)) continue;

			/* Feature is OK */
			features[n++] = feat;

			/* Paranoia */
			if (n >= N_ELEMENTS(features)) break;
		}

		/* Pick a default feature, if any */
		if (n > 0) return (features[rand_int(n)]);
	}

	/* Special case - Themed levels with random features */
	if (feeling >= LEV_THEME_HEAD)
	{
		/* Note that we have a boost to depth in themed levels */
		max_depth += 7;

		/* Quests have a bigger boost to depth */
		if (quest_check(p_ptr->depth) == QUEST_THEMED_LEVEL)
		{
			max_depth += 10;
		}

		/* Check bounds */
		max_depth = MIN(max_depth, MAX_DEPTH - 1);
	}

	/* Set the given hook, if any */
	get_feat_num_hook = feat_hook;

	get_feat_num_prep();

	/* Pick a feature */
	feat = get_feat_num(max_depth);

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* Return the feature */
	return (feat);
}


/*
 * A fractal map is a matrix (MAX_FRACTAL_SIZE * MAX_FRACTAL_SIZE) of
 * small numbers. Each number in the map should be replaced with a dungeon
 * feature. This way we can use fractal maps to perform different actions like
 * building rooms, placing pools, etc.
 *
 * We are going to store this matrix in dynamic memory so we have to define
 * a couple of new types:
 */

/*
 * A row of the fractal map
 */
typedef byte fractal_map_wid[MAX_FRACTAL_SIZE];

/*
 * FRACTAL MAP. An array of rows. It can be used as a two-dimensional array.
 */
typedef fractal_map_wid *fractal_map;

/*
 * VERY IMPORTANT: The map must be a square. Its size must be
 * "power of 2 plus 1". Valid values are 3, 5, 9, 17, 33, 65, 129, 257, etc.
 * The maximum supported size is 65.
 *
 * Aditional comments about the construction of the map:
 * Only nine grids of this square are processed at the beginning.
 * These grids are the four corners (who MUST be previously set to meaningful
 * values) and five inner grids.
 * The inner grids are the middle points of each side plus the grid
 * at the center.
 * These nine grids can be viewed as a "3x3" square.
 * Example:
 *
 * a*b
 * ***
 * c*d
 *
 * The algorithm supplies the values for the five inner grids by
 * randomly chosing one *adjacent* corner. Example:
 *
 * aab
 * cbd
 * ccd
 *
 * The next step is to consider this "3x3" square as a part of a larger
 * square:
 *
 * a*a*b
 * *****
 * c*b*d
 * *****
 * c*c*d
 *
 * Now we have four "3x3" squares. The process is repeated for each one of
 * them. The process is stopped when the initial square can't be "magnified"
 * any longer.
 */

/* Available fractal map types */
enum
{
	FRACTAL_TYPE_17x33 = 0,
	FRACTAL_TYPE_33x65,
	FRACTAL_TYPE_9x9,
	FRACTAL_TYPE_17x17,
	FRACTAL_TYPE_33x33,
	MAX_FRACTAL_TYPES
};

static struct
{
	int hgt, wid;
} fractal_dim[MAX_FRACTAL_TYPES] =
{
	{17, 33},		/* FRACTAL_TYPE_17x33 */
	{33, 65},		/* FRACTAL_TYPE_33x65 */
	{9, 9},			/* FRACTAL_TYPE_9x9 */
	{17, 17},		/* FRACTAL_TYPE_17x17 */
	{33, 33},		/* FRACTAL_TYPE_33x33 */
};

/* These ones are the valid values for the map grids */
#define FRACTAL_NONE	0	/* Used only at construction time */
#define FRACTAL_WALL	1	/* Wall grid */
#define FRACTAL_EDGE	2	/* Wall grid adjacent to floor (outer wall) */
#define FRACTAL_FLOOR	3	/* Floor grid */
#define FRACTAL_POOL_1	4	/* Pool grid */
#define FRACTAL_POOL_2	5	/* Pool grid */
#define FRACTAL_POOL_3	6	/* Pool grid */

typedef struct fractal_template fractal_template;

/* Initialization function for templates */
typedef void (*fractal_init_func)(fractal_map map, fractal_template *t_ptr);

/*
 * A fractal template is used to set the basic shape of the fractal.
 *
 * Templates must provide values for at least the four corners of the map.
 * See the examples.
 */
struct fractal_template
{
	/* The type of the fractal map (one of FRACTAL_TYPE_*) */
	byte type;

	/* The maximum size of the fractal map (3, 5, 9, 17, 33 or 65) */
	int size;

	/* The initialization function for this template */
	fractal_init_func init_func;
};


/* Verify that a point is inside a fractal */
#define IN_FRACTAL(template,y,x) \
	(((y) >= 0) && ((y) < (template)->size) && \
	((x) >= 0) && ((x) < (template)->size))


/*
 * Places a line in a fractal map given its start and end points and a certain
 * grid type. To be used in template initialization routines.
 * Note: This is a very basic drawing routine. It works fine with vertical,
 * horizontal and the diagonal lines of a square. It doesn't support other
 * oblique lines well.
 */
static void fractal_draw_line(fractal_map map, fractal_template *t_ptr,
		int y1, int x1, int y2, int x2, byte content)
{
	int dx, dy;

	/* Get the proper increments to reach the end point */
	dy = ((y1 < y2) ? 1: (y1 > y2) ? -1: 0);
	dx = ((x1 < x2) ? 1: (x1 > x2) ? -1: 0);

	/* Draw the line */
	while (TRUE)
	{
		/* Stop at the first illegal grid */
		if (!IN_FRACTAL(t_ptr, y1, x1)) break;

		/* Set the new content of the grid */
		map[y1][x1] = content;

		/* We reached the end point? */
		if ((y1 == y2) && (x1 == x2)) break;

		/* Advance one position */
		y1 += dy;
		x1 += dx;
	}
}


/*
 * Places walls in the perimeter of a fractal map
 */
static void fractal_draw_borders(fractal_map map, fractal_template *t_ptr)
{
	int last = t_ptr->size - 1;

	/* Left */
	fractal_draw_line(map, t_ptr, 0, 0, last, 0, FRACTAL_WALL);
	/* Important: Leave some space for tunnels */
	fractal_draw_line(map, t_ptr, 0, 1, last, 1, FRACTAL_WALL);

	/* Right */
	fractal_draw_line(map, t_ptr, 0, last, last, last, FRACTAL_WALL);
	/* Important: Leave some space for tunnels */
	fractal_draw_line(map, t_ptr, 0, last - 1, last, last - 1, FRACTAL_WALL);

	/* Top */
	fractal_draw_line(map, t_ptr, 0, 1, 0, last - 1, FRACTAL_WALL);

	/* Bottom */
	fractal_draw_line(map, t_ptr, last, 1, last, last - 1, FRACTAL_WALL);
}


/*
 * Some fractal templates
 */

/* 17x33 template */
static void fractal1_init_func(fractal_map map, fractal_template *t_ptr)
{
	/* Borders */
	fractal_draw_borders(map, t_ptr);

	/*
	 * Mega-hack -- place walls in the middle of the 33x33 map to generate
	 * a 17x33 map
	 */
	fractal_draw_line(map, t_ptr, 16, 1, 16, 32, FRACTAL_WALL);

	map[8][8] = (one_in_(15) ? FRACTAL_POOL_1: FRACTAL_FLOOR);
	map[8][16] = (one_in_(15) ? FRACTAL_POOL_2: FRACTAL_FLOOR);
	map[8][24] = (one_in_(15) ? FRACTAL_POOL_3: FRACTAL_FLOOR);
}


/* 33x65 template */
static void fractal2_init_func(fractal_map map, fractal_template *t_ptr)
{
	int k;

	/* Borders */
	fractal_draw_borders(map, t_ptr);

	/*
	 * Mega-hack -- place walls in the middle of the 65x65 map to generate
	 * a 33x65 map
	 */
	fractal_draw_line(map, t_ptr, 32, 1, 32, 64, FRACTAL_WALL);

	k = rand_int(100);
	/* 1 in 5 chance to make a pool and 1 in 5 to leave the map untouched */
	if (k < 80) map[8][16] = ((k < 20) ? FRACTAL_POOL_2: FRACTAL_FLOOR);

	k = rand_int(100);
	/* 1 in 4 chance to make a pool and 1 in 4 to leave the map untouched */
	if (k < 75) map[8][32] = ((k < 25) ? FRACTAL_POOL_3: FRACTAL_FLOOR);

	k = rand_int(100);
	/* 1 in 5 chance to make a pool and 1 in 5 to leave the map untouched */
	if (k < 80) map[8][48] = ((k < 20) ? FRACTAL_POOL_1: FRACTAL_FLOOR);

	map[16][16] = (one_in_(4) ? FRACTAL_POOL_1: FRACTAL_FLOOR);
	map[16][32] = (one_in_(3) ? FRACTAL_POOL_2: FRACTAL_FLOOR);
	map[16][48] = (one_in_(4) ? FRACTAL_POOL_3: FRACTAL_FLOOR);

	k = rand_int(100);
	/* 1 in 5 chance to make a pool and 1 in 5 to leave the map untouched */
	if (k < 80) map[24][16] = ((k < 20) ? FRACTAL_POOL_3: FRACTAL_FLOOR);

	k = rand_int(100);
	/* 1 in 4 chance to make a pool and 1 in 4 to leave the map untouched */
	if (k < 75) map[24][32] = ((k < 25) ? FRACTAL_POOL_1: FRACTAL_FLOOR);

	k = rand_int(100);
	/* 1 in 5 chance to make a pool and 1 in 5 to leave the map untouched */
	if (k < 80) map[24][48] = ((k < 20) ? FRACTAL_POOL_2: FRACTAL_FLOOR);
}


/* 9x9 template for pools */
static void fractal3_init_func(fractal_map map, fractal_template *t_ptr)
{
	/*Unused*/
	(void)t_ptr;

	/* Walls in the corners */
	map[0][0] = FRACTAL_WALL;
	map[0][8] = FRACTAL_WALL;
	map[8][0] = FRACTAL_WALL;
	map[8][8] = FRACTAL_WALL;

	map[2][4] = FRACTAL_FLOOR;
	map[4][2] = FRACTAL_FLOOR;
	map[4][4] = FRACTAL_FLOOR;
	map[4][6] = FRACTAL_FLOOR;
	map[6][4] = FRACTAL_FLOOR;
}


/* 17x17 template for pools */
static void fractal4_init_func(fractal_map map, fractal_template *t_ptr)
{
	/*Unused*/
	(void)t_ptr;

	/* Walls in the corners */
	map[0][0] = FRACTAL_WALL;
	map[0][16] = FRACTAL_WALL;
	map[16][0] = FRACTAL_WALL;
	map[16][16] = FRACTAL_WALL;

	map[4][8] = FRACTAL_FLOOR;
	map[8][4] = FRACTAL_FLOOR;
	map[8][8] = FRACTAL_FLOOR;
	map[8][12] = FRACTAL_FLOOR;
	map[12][8] = FRACTAL_FLOOR;
}


/* 33x33 template */
static void fractal5_init_func(fractal_map map, fractal_template *t_ptr)
{
	bool flip_h = one_in_(2);

	/* Borders */
	fractal_draw_borders(map, t_ptr);

	if (one_in_(15)) map[8][flip_h ? 24: 8] = FRACTAL_FLOOR;

	map[16][16] = FRACTAL_FLOOR;

	if (one_in_(15)) map[24][flip_h ? 8: 24] = FRACTAL_FLOOR;
}


/*
 * A list of the available fractal templates.
 */
static fractal_template fractal_repository[] =
{
	{FRACTAL_TYPE_17x33, 33, fractal1_init_func},
	{FRACTAL_TYPE_33x65, 65, fractal2_init_func},
	{FRACTAL_TYPE_9x9, 9, fractal3_init_func},
	{FRACTAL_TYPE_17x17, 17, fractal4_init_func},
	{FRACTAL_TYPE_33x33, 33, fractal5_init_func},
};


/*
 * Wipes the contents of a fractal map and applies the given template.
 */
static void fractal_map_reset(fractal_map map, fractal_template *t_ptr)
{
	int x, y;

	/* Fill the map with FRACTAL_NONE */
	for (y = 0; y < t_ptr->size; y++)
	{
		for (x = 0; x < t_ptr->size; x++)
		{
			map[y][x] = FRACTAL_NONE;
		}
	}

	/* Call the initialization function to place some floors */
	if (t_ptr->init_func)
	{
		t_ptr->init_func(map, t_ptr);
	}
}


/*
 * Returns a *reset* fractal map allocated in dynamic memory.
 * You must deallocate the map with FREE when it isn't used anymore.
 */
static fractal_map fractal_map_create(fractal_template *t_ptr)
{
	/* The new map */
	fractal_map map;

	/* Allocate the map */
	map = C_ZNEW(t_ptr->size, fractal_map_wid);

	/* Reset the contents of the map */
	fractal_map_reset(map, t_ptr);

	/* Done */
	return (map);
}


/*#define DEBUG_FRACTAL_TEMPLATES 1*/

#ifdef DEBUG_FRACTAL_TEMPLATES

/* This table is used to convert the map grids to printable characters */
static char fractal_grid_to_char[] =
{
	' ',	/* FRACTAL_NONE */
	'#',	/* FRACTAL_WALL */
	'&',	/* FRACTAL_EDGE */
	'.',	/* FRACTAL_FLOOR*/
	'1',	/* FRACTAL_POOL_1*/
	'2',	/* FRACTAL_POOL_2*/
	'3',	/* FRACTAL_POOL_3*/
};

/*
 * Prints a fractal map to stdout. "title" is optional (can be NULL).
 */
static void fractal_map_debug(fractal_map map, fractal_template *t_ptr,
	char *title)
{
	int x, y;
	FILE *fff;
	static bool do_create = TRUE;

	fff = my_fopen("fractal.txt", do_create ? "w": "a");

	do_create = FALSE;

	if (!fff) return;

 	/* Show the optional title */
	if (title)
	{
		fputs(title, fff);

		putc('\n', fff);
	}

	/* Show the map */
	for (y = 0; y < t_ptr->size; y++)
	{
		for (x = 0; x < t_ptr->size; x++)
		{
			byte grid = map[y][x];
			char chr;

			/* Check for strange grids */
			if (grid >= N_ELEMENTS(fractal_grid_to_char))
			{
				chr = '?';
			}
			/* Translate to printable char */
			else
			{
				chr = fractal_grid_to_char[grid];
			}

			/* Show it */
			putc(chr, fff);
		}

		/* Jump to the next row */
		putc('\n', fff);
	}

	putc('\n', fff);

	/* Done */
	my_fclose(fff);
}

#endif /* DEBUG_FRACTAL_TEMPLATES */


/*
 * Completes a fractal map. The map must have been reset.
 */
static void fractal_map_complete(fractal_map map, fractal_template *t_ptr)
{
	int x, y, x1, y1, x2, y2, cx, cy;
	/*
	 * Set the initial size of the squares. At first, we have only
	 * one big square.
	 */
	int cur_size = t_ptr->size - 1;

	/*
	 * Construct the map using a variable number of iterations.
	 * Each iteration adds more details to the map.
	 * This algorithm is originally recursive but we made it iterative
	 * for efficiency.
	 */
	do
	{
		/* Get the vertical coordinates of the first square */
		y1 = 0;
		y2 = cur_size;

		/*
		 * Process the whole map. Notice the step used: (cur_size / 2)
		 * is the middle point of the current "3x3" square.
		 */
		for (y = 0; y < t_ptr->size; y += (cur_size / 2))
		{
			/* Change to the next "3x3" square, if needed */
			if (y > y2)
			{
				/*
				 * The end of the previous square becomes the
				 * beginning of the new square
				 */
				y1 = y2;

				/* Get the end of the new square */
				y2 += cur_size;
			}

			/* Get the horizontal coordinates of the first square */
			x1 = 0;
			x2 = cur_size;

			/* Notice the step */
			for (x = 0; x < t_ptr->size; x += (cur_size / 2))
			{
				/* Change to the next "3x3" square, if needed */
				if (x > x2)
				{
					/*
					 * The end of the previous square
					 * becomes the beginning of the new
					 * square
					 */
					x1 = x2;

					/* Get the end of the new square */
					x2 += cur_size;
				}

				/* IMPORTANT: ignore already processed grids */
				if (map[y][x] != FRACTAL_NONE) continue;

				/*
				 * Determine if the vertical coordinate of
				 * this grid should be fixed
				 */
				if ((y == y1) || (y == y2)) cy = y;
				/* Pick one *adjacent* corner randomly */
				else cy = ((rand_int(100) < 50) ? y1: y2);

				/*
				 * Determine if the horizontal coordinate of
				 * this grid should be fixed
				 */
				if ((x == x1) || (x == x2)) cx = x;
				/* Pick one *adjacent* corner randomly */
				else cx = ((rand_int(100) < 50) ? x1: x2);

				/* Copy the value of the chosed corner */
				map[y][x] = map[cy][cx];
			}
		}

	/* Decrease the size of the squares for the next iteration */
	cur_size /= 2;

	/* We stop when the squares can't be divided anymore */
	} while (cur_size > 1);
}


/*
 * Verify if all floor grids in a completed fractal map are connected.
 */
static int fractal_map_is_connected(fractal_map map, fractal_template *t_ptr)
{
	int x, y, i, connected = TRUE;
	fractal_map_wid *visited;
	/* Queue of visited grids */
	grid_queue_type queue, *q_ptr = &queue;

	/* Allocate a "visited" matrix */
	visited = C_ZNEW(t_ptr->size, fractal_map_wid);

	/* Create the queue */
	grid_queue_create(q_ptr, 500);

	/* Find a floor grid */
	for (y = 0; (y < t_ptr->size) && GRID_QUEUE_EMPTY(q_ptr); y++)
	{
		for (x = 0; (x < t_ptr->size) && GRID_QUEUE_EMPTY(q_ptr); x++)
		{
			/* Found one */
			if (map[y][x] >= FRACTAL_FLOOR)
			{
				/* Put it on the queue */
				grid_queue_push(q_ptr, y, x);

				/* Mark as visited */
				visited[y][x] = TRUE;
			}
		}
	}

	/* Paranoia. No floor grid was found */
	if (GRID_QUEUE_EMPTY(q_ptr))
	{
		/* Free resources */
		FREE(visited);
		grid_queue_destroy(q_ptr);

		/* Done */
		return (!connected);
	}

	/* Process all reachable floor grids */
	while (!GRID_QUEUE_EMPTY(q_ptr))
	{
		/* Get the visited grid at the front of the queue */
		y = GRID_QUEUE_Y(q_ptr);
		x = GRID_QUEUE_X(q_ptr);

		/* Remove that grid from the queue */
		grid_queue_pop(q_ptr);

		/* Scan all adjacent grids */
		for (i = 0; i < 8; i++)
		{
			/* Get coordinates */
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			/* Check bounds */
			if (!IN_FRACTAL(t_ptr, yy, xx)) continue;

			/* Ignore already processed grids */
			if (visited[yy][xx]) continue;

			/* Ignore walls */
			if (map[yy][xx] < FRACTAL_FLOOR) continue;

			/* Append the grid to the queue, if possible */
			if (!grid_queue_push(q_ptr, yy, xx)) continue;

			/* Mark as visited */
			visited[yy][xx] = TRUE;
		}
	}

	/* Find non-visited floor grids */
	for (y = 0; (y < t_ptr->size) && connected; y++)
	{
		for (x = 0; (x < t_ptr->size) && connected; x++)
		{
			/* Check the grid */
			if ((map[y][x] >= FRACTAL_FLOOR) && !visited[y][x])
			{
				/* Found a non-visited floor grid. Done */
				connected = FALSE;
			}
		}
	}

	/* Free resources */
	FREE(visited);
	grid_queue_destroy(q_ptr);

	/* Return answer */
	return connected;
}


/*
 * Places FRACTAL_EDGE walls in a completed fractal map. These grids were
 * created to be replaced by outer walls or other similar features.
 */
static void fractal_map_mark_edge(fractal_map map, fractal_template *t_ptr)
{
	int x, y, i;

	/* Process the whole map */
	for (y = 0; y < t_ptr->size; y++)
	{
		for (x = 0; x < t_ptr->size; x++)
		{
			/* Ignore wall grids */
			if (map[y][x] < FRACTAL_FLOOR) continue;

			/* Scan adjacent grids */
			for (i = 0; i < 8; i++)
			{
				/* Get coordinates */
				int yy = y + ddx_ddd[i];
				int xx = x + ddy_ddd[i];

				/* Check bounds */
				if (!IN_FRACTAL(t_ptr, yy, xx)) continue;

				/* Turn plain walls to edge walls */
				if (map[yy][xx] == FRACTAL_WALL)
				{
					map[yy][xx] = FRACTAL_EDGE;
				}
			}
		}
	}
}


/*
 * Construct a fractal room given a fractal map and the room center's coordinates.
 */
static void fractal_map_to_room(fractal_map map, byte fractal_type, int y0, int x0)
{
	int x, y, y1, x1, wid, hgt;
	bool light = FALSE;
	int floor_type;
	/*
	 * No pools for now. Note that we choose the features for the pool when
	 * we need them. If we pick three random features right now we might
	 * generate inconsistent levels.
	 */
	u16b pool1 = FEAT_NONE;
	u16b pool2 = FEAT_NONE;
	u16b pool3 = FEAT_NONE;

	/* Get the dimensions of the fractal map */
	hgt = fractal_dim[fractal_type].hgt;
	wid = fractal_dim[fractal_type].wid;

	/* Get top-left coordinate */
	y1 = y0 - hgt / 2;
	x1 = x0 - wid / 2;

	/* Occasional light */
	if (effective_depth(p_ptr->depth) <= randint(25)) light = TRUE;

	/* Use earth floor sometimes. EXPERIMENTAL */
	floor_type = rand_int(100);

	/* Apply the map to the dungeon */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			byte grid_type = map[y][x];
			/* Translate to dungeon coordinates */
			int yy = y1 + y;
			int xx = x1 + x;

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Translate each grid type to dungeon features */
			if (grid_type >= FRACTAL_FLOOR)
			{
				u16b feat = FEAT_NONE;

				/* Pool grid */
				if (grid_type == FRACTAL_POOL_1)
				{
					/* Pick a feature if necessary */
					if (pool1 == FEAT_NONE)
					{
						pool1 = pick_proper_feature(cave_feat_pool);
					}

					/* Use the pool feature */
					feat = pool1;
				}
				/* Pool grid */
				else if (grid_type == FRACTAL_POOL_2)
				{
					/* Pick a feature if necessary */
					if (pool2 == FEAT_NONE)
					{
						pool2 = pick_proper_feature(cave_feat_pool);
					}

					/* Use the pool feature */
					feat = pool2;
				}
				/* Pool grid */
				else if (grid_type == FRACTAL_POOL_3)
				{
					/* Pick a feature if necessary */
					if (pool3 == FEAT_NONE)
					{
						pool3 = pick_proper_feature(cave_feat_pool);
					}

					/* Use the pool feature */
					feat = pool3;
				}

				/* Place the selected pool feature */
				if (feat != FEAT_NONE)
				{
					build_terrain(yy, xx, feat);
				}
				/* Or place a floor */
				else
				{
					/* Use earth floor (15%) */
					if (floor_type < 15)
					{
						feat = FEAT_FLOOR_EARTH;
					}
					/* Use scattered earth floor (5%) */
					else if ((floor_type < 20) && one_in_(7))
					{
						feat = FEAT_FLOOR_EARTH;
					}
					/* Plain old floor (80%) */
					else
					{
						feat = FEAT_FLOOR;
					}

					/* Place floors */
					cave_set_feat(yy, xx, feat);
				}
			}
			else if (grid_type == FRACTAL_EDGE)
			{
				/* Place ice walls on ice levels */
				if (level_flag & LF1_ICE)
				{
					build_terrain(yy, xx, FEAT_ICE_WALL);
				}
				/* Place usual walls on other levels */
				else
				{
					cave_set_feat(yy, xx, FEAT_WALL_EXTRA);
				}
			}
			else
			{
				continue;
			}

			/* Mark the grid as a part of the room */
			cave_info[yy][xx] |= (CAVE_ROOM);

			/* Light the feature if needed */
			if (light)
			{
				cave_info[yy][xx] |= (CAVE_GLOW);
			}

 			/* Or turn off the lights */
			else if (grid_type != FRACTAL_EDGE)
			{
				cave_info[yy][xx] &= ~(CAVE_GLOW);
			}
		}
	}
}


/*
 * Creates a fractal map given a template and copy part of it in the given map
 */
static void fractal_map_merge_another(fractal_map map, fractal_template *t_ptr)
{
	int y, x;

	fractal_map map2;

	/* Create the map */
	map2 = fractal_map_create(t_ptr);

	/* Complete it */
	fractal_map_complete(map2, t_ptr);

	/* Merge the maps */
	for (y = 0; y < t_ptr->size; y++)
	{
		for (x = 0; x < t_ptr->size; x++)
		{
			/* Sometimes we overwrite a grid in the original map */
			if ((map[y][x] != map2[y][x]) && one_in_(4)) map[y][x] = map2[y][x];
		}
	}

	/* Free resources */
	FREE(map2);
}


/*
 * Build a fractal room given its center. Returns TRUE on success.
 */
static bool build_type_fractal(int y0, int x0, byte type)
{
	fractal_map map;
	fractal_template *t_ptr;
	int tries;
	bool do_merge = FALSE;

	/* Paranoia */
	if (type >= MAX_FRACTAL_TYPES) return (FALSE);

	/* Reset the loop counter */
	tries = 0;

	/* Get a fractal template */
	while (TRUE)
	{
		/* Get a template */
		int which = rand_int(N_ELEMENTS(fractal_repository));

		t_ptr = &fractal_repository[which];

		/* Check if the type matches the wanted one */
		if (t_ptr->type == type) break;

		/* Avoid infinite loops */
		if (++tries >= 100) return (FALSE);
	}

	/* Create and reset the fractal map */
	map = fractal_map_create(t_ptr);

#ifdef DEBUG_FRACTAL_TEMPLATES
	/* Show the template to the developer */
	fractal_map_debug(map, t_ptr, "Fractal");
#endif

	/* Make medium fractal rooms more exotic sometimes */
	if ((type == FRACTAL_TYPE_33x33) && !one_in_(3)) do_merge = TRUE;

	/* Reset the loop counter */
	tries = 0;

	/* Construct the fractal map */
	while (TRUE)
	{
		/* Complete the map */
		fractal_map_complete(map, t_ptr);

		/* Put another room on top of this one if necessary */
		if (do_merge) fractal_map_merge_another(map, t_ptr);

		/* Accept only connected maps */
		if (fractal_map_is_connected(map, t_ptr)) break;

		/* Avoid infinite loops */
		if (++tries >= 100)
		{
			/* Free resources */
			FREE(map);

			/* Failure */
			return (FALSE);
		}

		/* Reset the map. Try again */
		fractal_map_reset(map, t_ptr);
	}

	/* Get edge information */
	fractal_map_mark_edge(map, t_ptr);

	/* Place the room */
	fractal_map_to_room(map, type, y0, x0);

	/* Free resources */
	FREE(map);

	/* Success */
	return (TRUE);
}


/*
 * Build a pool in a room given the center of the pool and a feature.
 * Outer and solid walls, and permanent features are unnafected.
 * Returns TRUE on success.
 */
static bool build_pool(int y0, int x0, int feat, bool do_big_pool)
{
	byte type;
	int wid, hgt;
	int x, y, x1, y1;
	fractal_map map;
	fractal_template *t_ptr;

	/* Paranoia */
	if (!feat) return (FALSE);

	/* Set some basic info */
	if (do_big_pool)
	{
		type = FRACTAL_TYPE_17x17;
	}
	else
	{
		type = FRACTAL_TYPE_9x9;
	}

	/* Get the dimensions of the fractal map */
	hgt = fractal_dim[type].hgt;
	wid = fractal_dim[type].wid;

	/* Get the top-left grid of the pool */
	y1 = y0 - hgt / 2;
	x1 = x0 - wid / 2;

	/* Choose a template for the pool */
	while (TRUE)
	{
		/* Pick a random template */
		int which = rand_int(N_ELEMENTS(fractal_repository));

		t_ptr = &fractal_repository[which];

		/* Found the desired template type? */
		if (t_ptr->type == type) break;
	}

	/* Create and reset the fractal map */
	map = fractal_map_create(t_ptr);

	/* Complete the map */
	fractal_map_complete(map, t_ptr);

	/* Copy the map into the dungeon */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			/* Translate map coordinates to dungeon coordinates */
			int yy = y1 + y;
			int xx = x1 + x;

			/* Ignore non-floors grid types in the map */
			if (map[y][x] != FRACTAL_FLOOR) continue;

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/* A pool must be inside a room */
			if (!(cave_info[yy][xx] & (CAVE_ROOM))) continue;

			/* Ignore anti-teleport grids */
			if (cave_info[yy][xx] & (CAVE_ICKY)) continue;

			/* Ignore some walls, and permanent features */
			if (cave_ff1_match(yy, xx, FF1_OUTER |
				FF1_SOLID | FF1_PERMANENT)) continue;

			/* Set the feature */
			build_terrain(yy, xx, feat);
		}
	}

	/* Free resources */
	FREE(map);

	/* Success */
	return (TRUE);
}


/*
 * Mark a starburst shape in the dungeon with the CAVE_TEMP flag, given the
 * coordinates of a section of the dungeon in "box" format. -LM-, -DG-
 *
 * Starburst are made in three steps:
 * 1: Choose a box size-dependant number of arcs.  Large starburts need to
 *    look less granular and alter their shape more often, so they need
 *    more arcs.
 * 2: For each of the arcs, calculate the portion of the full circle it
 *    includes, and its maximum effect range (how far in that direction
 *    we can change features in).  This depends on starburst size, shape, and
 *    the maximum effect range of the previous arc.
 * 3: Use the table "get_angle_to_grid" to supply angles to each grid in
 *    the room.  If the distance to that grid is not greater than the
 *    maximum effect range that applies at that angle, change the feature
 *    if appropriate (this depends on feature type).
 *
 * Usage notes:
 * - This function uses a table that cannot handle distances larger than
 *   20, so it calculates a distance conversion factor for larger starbursts.
 * - This function is not good at handling starbursts much longer along one axis
 *   than the other.
 * This function doesn't mark any grid in the perimeter of the given box.
 *
 */
static bool mark_starburst_shape(int y1, int x1, int y2, int x2, u32b flag)
{
	int y0, x0, y, x, ny, nx;
	int i;
	int size;
	int dist, max_dist, dist_conv, dist_check;
	int height, width, arc_dist;
	int degree_first, center_of_arc, degree;

	/* Special variant starburst.  Discovered by accident. */
	bool make_cloverleaf = FALSE;

	/* Holds first degree of arc, maximum effect distance in arc. */
	int arc[45][2];

	/* Number (max 45) of arcs. */
	int arc_num;

	/* Make certain the starburst does not cross the dungeon edge. */
	if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);

	/* Robustness -- test sanity of input coordinates. */
	if ((y1 + 2 >= y2) || (x1 + 2 >= x2)) return (FALSE);

	/* Get room height and width. */
	height = 1 + y2 - y1;
	width  = 1 + x2 - x1;

	/* Note the "size" */
	size = 2 + div_round(width + height, 22);

	/* Get a shrinkage ratio for large starbursts, as table is limited. */
	if ((width > 40) || (height > 40))
	{
		if (width > height) dist_conv = 1 + (10 * width  / 40);
		else                dist_conv = 1 + (10 * height / 40);
	}
	else dist_conv = 10;

	/* Make a cloverleaf starburst sometimes.  (discovered by accident) */
	if ((flag & (STAR_BURST_CLOVER)) && (height > 10) && (one_in_(20)))
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
				else arc[i][1] = arc[i-1][1] - size;
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
			else arc[arc_num - 1][1] = arc[0][1] - size;
		}
	}

	/* Precalculate check distance. */
	dist_check = 21 * dist_conv / 10;

	/* Change grids between (and not including) the edges. */
	for (y = y1 + 1; y < y2; y++)
	{
		for (x = x1 + 1; x < x2; x++)
		{
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
							/* Mark the grid */
							cave_info[y][x] |= (CAVE_TEMP);
						}

						/* Arc found.  End search */
						break;
					}
				}
			}
		}
	}

	return (TRUE);
}


/*
 * Make a starburst room. -LM-, -DG-
 *
 * Usage notes:
 * - This function is not good at handling rooms much longer along one axis
 *   than the other.
 * - It is safe to call this function on areas that might contain vaults or
 *   pits, because "icky" and occupied grids are left untouched.
 */
static bool generate_starburst_room(int y1, int x1, int y2, int x2,
	u16b feat, u16b edge, u32b flag)
{
	int y, x, d;

	/* Mark the affected grids */
	if (!mark_starburst_shape(y1, x1, y2, x2, flag)) return (FALSE);

	/* Paranoia */
	if (edge == feat) edge = FEAT_NONE;

	/* Process marked grids */
	for (y = y1 + 1; y < y2; y++)
	{
		for (x = x1 + 1; x < x2; x++)
		{
			/* Marked grids only */
			if (!(cave_info[y][x] & (CAVE_TEMP))) continue;

			/* Do not touch "icky" grids. */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* Do not touch occupied grids. */
			if (cave_m_idx[y][x] != 0) continue;
			if (cave_o_idx[y][x] != 0) continue;

			/* Illuminate if requested */
			if (flag & (STAR_BURST_LIGHT))
			{
				cave_info[y][x] |= (CAVE_GLOW);
			}
			/* Or turn off the lights */
			else
			{
				cave_info[y][x] &= ~(CAVE_GLOW);
			}

			/* Floor overwrites the dungeon */
			if (flag & (STAR_BURST_RAW_FLOOR))
			{
				cave_set_feat(y, x, feat);
			}
			/* Floor is merged with the dungeon */
			else
			{
				build_terrain(y, x, feat);
			}

			/* Make part of a room if requested */
			if (flag & (STAR_BURST_ROOM))
			{
				cave_info[y][x] |= (CAVE_ROOM);
			}

			/* Special case. No edge feature */
			if (edge == FEAT_NONE)
			{
				/*
				 * We lite the outside grids anyway, to
				 * avoid lakes surrounded with blackness.
				 * We only do this if the lake is lit.
				 */
				if (!(flag & (STAR_BURST_LIGHT |
					STAR_BURST_ROOM))) continue;

				/* Look in all directions. */
				for (d = 0; d < 8; d++)
				{
					/* Extract adjacent location */
					int yy = y + ddy_ddd[d];
					int xx = x + ddx_ddd[d];

					/* Ignore annoying locations */
					if (!in_bounds_fully(yy, xx)) continue;

					/* Already processed */
					if (cave_info[yy][xx] & (CAVE_TEMP)) continue;

					/* Lite the feature */
					if (flag & (STAR_BURST_LIGHT))
					{
						cave_info[yy][xx] |= (CAVE_GLOW);
					}

					/* Make part of the room */
					if (flag & (STAR_BURST_ROOM))
					{
						cave_info[yy][xx] |= (CAVE_ROOM);
					}
				}

				/* Done */
				continue;
			}

			/* Common case. We have an edge feature */

			/* Look in all directions. */
			for (d = 0; d < 8; d++)
			{
				/* Extract adjacent location */
				int yy = y + ddy_ddd[d];
				int xx = x + ddx_ddd[d];

				/* Ignore annoying locations */
				if (!in_bounds_fully(yy, xx)) continue;

				/* Already processed */
				if (cave_info[yy][xx] & (CAVE_TEMP)) continue;

				/* Do not touch "icky" grids. */
				if (cave_info[yy][xx] & (CAVE_ICKY)) continue;

				/* Do not touch occupied grids. */
				if (cave_m_idx[yy][xx] != 0) continue;
				if (cave_o_idx[yy][xx] != 0) continue;

				/* Illuminate if requested. */
				if (flag & (STAR_BURST_LIGHT))
				{
					cave_info[yy][xx] |= (CAVE_GLOW);
				}

				/* Edge overwrites the dungeon */
				if (flag & (STAR_BURST_RAW_EDGE))
				{
					cave_set_feat(yy, xx, edge);
				}
				/* Edge is merged with the dungeon */
				else
				{
					build_terrain(yy, xx, edge);
				}

				/* Make part of a room if requested */
				if (flag & (STAR_BURST_ROOM))
				{
					cave_info[yy][xx] |= (CAVE_ROOM);
				}
			}
		}
	}

	/* Clear the mark */
	for (y = y1 + 1; y < y2; y++)
	{
		for (x = x1 + 1; x < x2; x++)
		{
			cave_info[y][x] &= ~(CAVE_TEMP);
		}
	}

	/* Success */
	return (TRUE);
}


static void build_type_starburst(int y0, int x0, bool giant_room)
{
	bool want_pools = (rand_int(150) < effective_depth(p_ptr->depth));
	/* Default floor and edge */
	u16b feat = FEAT_FLOOR;
	u16b edge = FEAT_WALL_EXTRA;
	/* Default flags, classic rooms */
	u32b flag = (STAR_BURST_ROOM | STAR_BURST_RAW_FLOOR |
		STAR_BURST_RAW_EDGE);

	int dy, dx;

	/*
	 * Hack - get the size of the room, could be large or very large.
	 */

	/* 66x44 */
	if (giant_room)
	{
		dy = 19;
		dx = 30;
	}
	/* 33x22 */
	else
	{
		dy = 9;
		dx = 14;
	}

	/* We have a second chance to build pools in themed levels */
	if (!want_pools && (feeling >= LEV_THEME_HEAD))
	{
		want_pools = (rand_int(90) < effective_depth(p_ptr->depth));
	}

	/* Occasional light */
	if (effective_depth(p_ptr->depth) <= randint(25)) flag |= (STAR_BURST_LIGHT);

	/* Frozen edge on ice levels */
	if (level_flag & (LF1_ICE))
	{
		edge = FEAT_ICE_WALL;

		/* Make ice walls interesting */
		flag &= ~(STAR_BURST_RAW_EDGE);
	}

	/* Case 1. Plain starburst room */
	if (rand_int(100) < 75)
	{
		/* Allow cloverleaf rooms if pools are disabled */
		if (!want_pools) flag |= (STAR_BURST_CLOVER);

		generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
			feat, edge, flag);
	}
	/* Case 2. Add an inner room */
	else
	{
		/* Note no cloverleaf room */
		generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
			feat, edge, flag);

		/* Special case. Create a solid wall formation */
		if (one_in_(2))
		{
			/* Classic rooms */
			if (edge == FEAT_WALL_EXTRA)
			{
				feat = FEAT_WALL_INNER;
			}

			/* Ice wall formation */
			else
			{
				feat = edge;

				/* Make ice walls interesting */
				flag &= ~(STAR_BURST_RAW_FLOOR);
			}

			/* No edge */
			edge = FEAT_NONE;
		}

		/* Adjust the size of the inner room */
		if (feat_ff1_match(edge, FF1_WALL))
		{
			dy /= 4;
			dx /= 4;
		}
		else
		{
			dy /= 3;
			dx /= 3;
		}

		generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
			feat, edge, flag);
	}

	/* Build pools */
	if (want_pools)
	{
		int i, n_pools, range;

		/* Randomize the number of pools */
		n_pools = randint(2);

		/* Adjust for giant rooms */
		if (giant_room) n_pools += 1;

		/* How far of room center? */
		range = giant_room ? 12: 5;

		/* Force the selection of a new feature */
		feat = FEAT_NONE;

		/* Place the pools */
		for (i = 0; i < n_pools; i++)
		{
			int tries;

			/* Pick a new feature */
			if (!feat || one_in_(4))
			{
				/* Choose a feature for the pool */
				feat = pick_proper_feature(cave_feat_pool);

				/* Got none */
				if (!feat) continue;
			}

			for (tries = 0; tries < 2500; tries++)
			{
				/* Get the center of the pool */
				int y = rand_spread(y0, range);
				int x = rand_spread(x0, range);

				/* Verify center */
				if (cave_feat[y][x] == FEAT_FLOOR)
				{
					build_pool(y, x, feat, giant_room);

					/* Done */
					break;
				}
			}
		}
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
			/* We can pierce 2 outer walls at the same time */
			int outer_x[2], outer_y[2], num_outer = 0;

			/* Get the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Hack -- Avoid outer/solid permanent walls */
			if (cave_feat[y][x] == FEAT_PERM_SOLID) continue;
			if (cave_feat[y][x] == FEAT_PERM_OUTER) continue;

			/* Hack -- Avoid solid granite walls */
			if (cave_feat[y][x] == FEAT_WALL_SOLID) continue;

			/* Check if we have only 2 consecutive outer walls */
			/* This is important to avoid disconnected rooms */
			if (cave_feat[y][x] == FEAT_WALL_OUTER)
			{
				/* Get the "next" location (again) */
				int yy = y + row_dir;
				int xx = x + col_dir;

				/* There is some other wall there, ignore */
				if (!cave_ff1_match(yy, xx, FF1_MOVE)) continue;

				/* Remember the first wall */
				outer_y[num_outer] = tmp_row;
				outer_x[num_outer] = tmp_col;
				++num_outer;

				/* Hack -- Go to the second wall */
				tmp_row = y;
				tmp_col = x;
			}

			/* Remember the current wall */
			outer_y[num_outer] = tmp_row;
			outer_x[num_outer] = tmp_col;
			++num_outer;

			/* Process the stored outer walls */
			for (i = 0; i < num_outer; i++)
			{
				/* Get the wall location */
				row1 = outer_y[i];
				col1 = outer_x[i];

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
		}

		/* Feature can be bridged */
		else if (cave_ff2_match(tmp_row, tmp_col, FF2_BRIDGE))
		{
			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Save the bridge location */
			if (dun->tunn_n < TUNN_MAX)
			{
				dun->tunn[dun->tunn_n].y = row1;
				dun->tunn[dun->tunn_n].x = col1;
				dun->tunn_n++;
			}
		}

		/* Feature can be tunneled. We ignore doors and inner walls */
		else if (cave_ff1_match(tmp_row, tmp_col,
			FF1_CAN_TUNNEL | FF1_DOOR | FF1_INNER) ==
				(FF1_CAN_TUNNEL))
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

		/* Travel quickly through rooms */
		else if (cave_info[tmp_row][tmp_col] & (CAVE_ROOM))
		{
			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;
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

		if (cave_ff2_match(y, x, FF2_BRIDGE))
		{
			/* Clear previous contents, bridge it */
			cave_alter_feat(y, x, FS_BRIDGE);
		}
		else
		{
			/* Clear previous contents, tunnel it */
			cave_alter_feat(y, x, FS_TUNNEL);
		}
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
			/* Check for unusual entry points to starburst/fractal rooms */
			if ((cave_wall_bold(y-1, x) && cave_wall_bold(y+1, x)) ||
				(cave_wall_bold(y, x-1) && cave_wall_bold(y, x+1)))
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
static bool new_player_spot_safe(void)
{
	int i;
	int y_location_tables[40];
	int x_location_tables[40];
	int rand_spot;
	int x, y;
	int close_room = 0, room_dist = 1000;
	int tries = 0;

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

	/* We need at least one unused block */
	if (i == 0)
	{
		if (cheat_room)
		{
			msg_print("No unused block for the player.");
		}

		return (FALSE);
	}

	/* Find a safe spot */
	while (TRUE)
	{
		int d;

		feature_type *f_ptr;

		/* Hack - Avoid infinite loops */
		if (++tries >= 1000)
		{
			if (cheat_room)
			{
				msg_print("No safe spot for the player.");
			}

			return (FALSE);
		}

		/* Pick a random block */
		rand_spot = rand_int(i);

		/* Pick a random spot inside that block */
		y = y_location_tables[rand_spot] * BLOCK_HGT + rand_int(BLOCK_HGT);
		x = x_location_tables[rand_spot] * BLOCK_WID + rand_int(BLOCK_WID);

		/* Too close to a vault/room */
		if (cave_info[y][x] & (CAVE_ICKY | CAVE_ROOM)) continue;

		/* Get the feature */
		f_ptr = &f_info[cave_feat[y][x]];

		/*
		 * The spot must be a wall. Note that we do not need to call
		 * in_bounds_fully
		 */
		if (!_feat_ff1_match(f_ptr, FF1_WALL)) continue;

		/* But we don't want certain walls */
		if (_feat_ff1_match(f_ptr, FF1_INNER | FF1_OUTER | FF1_SOLID |
			FF1_PERMANENT)) continue;

		/* The spot must be surrounded by walls */
		for (d = 0; d < 8; d++)
		{
			int yy = y + ddy_ddd[d];
			int xx = x + ddx_ddd[d];

			/* Too close to a vault/room */
			if (cave_info[yy][xx] & (CAVE_ICKY | CAVE_ROOM)) break;

			/* Get the feature */
			f_ptr = &f_info[cave_feat[yy][xx]];

			/* We need walls */
			if (!_feat_ff1_match(f_ptr, FF1_WALL)) break;

			/* We don't want certain walls around us */
			if (_feat_ff1_match(f_ptr, FF1_INNER | FF1_OUTER |
				FF1_SOLID)) break;
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
	if (cave_ff1_match(y, x, FF1_CAN_TUNNEL))
	{
		cave_alter_feat(y, x, FS_TUNNEL);
	}
	else
	{
		cave_set_feat(y, x, FEAT_FLOOR);
	}

	/* Actually place the player */
	return player_place(y, x);
}


/*
 * Places some staircases near walls
 */
static bool alloc_stairs(u16b feat, int num)
{
	int x;

	/* Limit the number of stairs */
	num = (*dun_cap->adjust_stairs_number)(num);

	/* Place "num" stairs */
	for (x = 0; x < num; x++)
	{
		int i = 0;

		int yy, xx;

		int x_location_tables [40];
		int y_location_tables [40];
		int rand_spot;

		/* Collect suitable grids */
		for (yy = 0; yy < p_ptr->cur_map_hgt; yy++)
		{
			for (xx = 0; xx < p_ptr->cur_map_wid; xx++)
			{
				/* Check allowance with dungeon capabilities */
				if (!(*dun_cap->can_place_stairs)(yy, xx)) continue;

				/* We like to be next to walls, but not other stairs */
				if (next_to_walls(yy, xx) < 3) continue;
				if (next_to_stairs(yy, xx) > 0) continue;

				/* Hack - not too many stairs in pillar rooms */
				if (cave_info[yy][xx] & (CAVE_ROOM))
				{
					if (!(one_in_(10))) continue;
				}

				/* No stairs in greater vaults */
				if (cave_info[yy][xx] & (CAVE_G_VAULT))	continue;

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
		else if ((no_down_stairs(p_ptr->depth)) ||
				 (effective_depth(p_ptr->depth) >= MAX_DEPTH-1))
		{
			/* Clear previous contents, add up stairs */
			if (x == 0) cave_set_feat(yy, xx, FEAT_LESS);
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
		if (!cave_ff1_match(y, x, FF1_FLOOR)) continue;

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
		if (cave_wall_bold(y-1, x) && cave_wall_bold(y+1, x))
		{
			return (TRUE);
		}

		/* Check Horizontal */
		if (cave_wall_bold(y, x-1) && cave_wall_bold(y, x+1))
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
	if (cave_wall_bold(y, x)) return;

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
 * Hack - mark some squares in the dungeon, that are counted when the player
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

	/* No secret quuares in Moria */
	if (game_mode == GAME_NPPMORIA) return;

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

			/* Require "plain" floor grid */
			if (!cave_plain_bold(y, x)) continue;

			/* Already Marked */
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
	if (dun->tunn_n > 0) alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));

	/* Determine the character location, if it is needed */
	if ((p_ptr->px + p_ptr->py == 0) && !new_player_spot_old()) return (FALSE);

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
	(void)new_player_spot_safe();

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
	if (!alloc_stairs(FEAT_MORE, (3 + randint0(3))))
	{
		if (cheat_room) msg_format("failed to place down stairs");

		return (FALSE);
	}

	/* Place 1 or 3 up stairs near some walls */
	if (!alloc_stairs(FEAT_LESS, (1 + randint0(3))))
	{
		if (cheat_room) msg_format("failed to place up stairs");

		return (FALSE);
	}

	/* Hack -- Sandstone streamers are shallow */
	if (rand_int(DUN_STR_SLV) > effective_depth(p_ptr->depth))
	{

		/* Hack -- Add some magma streamers */
		for (i = 0; i < DUN_STR_MAG; i++)
		{
			/*if we can't build streamers, something is wrong with level*/
			if (!build_streamer(FEAT_MAGMA, DUN_STR_MC)) return (FALSE);
		}
	}

	else
	{
		/* Hack -- Add some quartz streamers */
		for (i = 0; i < DUN_STR_QUA; i++)
		{
			/*if we can't build streamers, something is wrong with level*/
			if (!build_streamer(FEAT_QUARTZ, DUN_STR_QC)) return (FALSE);
		}
	}

	/* Hack -- Add a rich mineral vein very rarely */
	if (one_in_(DUN_STR_GOL))
	{
		if (!build_streamer(FEAT_QUARTZ, DUN_STR_GC)) return (FALSE);
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
	if (effective_depth(p_ptr->depth) < room[typ].level) return (FALSE);

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
		case 14:
		case 13:
		case 12:
		{
			byte fractal_type;

			switch (typ)
			{
				case 14: fractal_type = FRACTAL_TYPE_33x65; break;
				case 13: fractal_type = FRACTAL_TYPE_33x33; break;
				default: fractal_type = FRACTAL_TYPE_17x33; break;
			}

			if (!build_type_fractal(y, x, fractal_type)) return (FALSE);

			break;
		}
		case 11:build_type_starburst(y, x, TRUE); break;
		case 10:build_type_starburst(y, x, FALSE); break;
		case 9: build_type_special_vault(y, x); break;
		case 8: build_type_greater_vault(y, x); break;
		case 7: build_type_lesser_vault(y, x); break;
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


/*
 * Select a monster type for a themed level
 */
byte get_level_theme(s16b orig_theme_num, bool quest_level)
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
		/* Hounds, demon, servants of valar, or young dragon*/
		if (one_in_(4))			return (LEV_THEME_VALAR_SERVANTS);
		if (one_in_(3))			return (LEV_THEME_DEMON_ALL);
		else if (one_in_(2))	return (LEV_THEME_HOUND);
		else					return (LEV_THEME_DRAGON_YOUNG);
	}

	/* Giant, animal, servants of valar, or humanoid pit */
	else if ((theme_depth <= 60) && (orig_theme_num <= 80))
	{
		if (one_in_(5))			return (LEV_THEME_VALAR_SERVANTS);
		else if (one_in_(4))	return (LEV_THEME_HUMANOID);
		else if (one_in_(3))	return (LEV_THEME_UNDEAD);
		else if (one_in_(2))	return (LEV_THEME_GIANT);
		else 					return (LEV_THEME_ANIMAL);
	}

	/* Dragon pit */
	else if (theme_depth <= 75)
	{
		/* Don't do specific elements for quest levels.  Too easy for those with the right immunity. */
		if (quest_level) return (LEV_THEME_DRAGON_ELEMENTAL);

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
		if (one_in_(3))		return (LEV_THEME_VALAR_SERVANTS);

		/* Ancient Dragon pit */
		else if one_in_(2)	return (LEV_THEME_DRAGON_ANCIENT);

		/* > 90 - Demon pit */
		else			return (LEV_THEME_DEMON_MAJOR);
	}
}


/*
 *Helper function for max number of creatures on a themed level.
 This function is for non-uniques only.
 */
byte max_themed_monsters(const monster_race *r_ptr, u32b max_power)
{
	/*first off, handle uniques*/
	if (r_ptr->flags1 & RF1_UNIQUE) return (r_ptr->max_num);

	/*don't allow 99 of the out of depth monsters*/
	if (r_ptr->level > effective_depth(p_ptr->depth) + 3)
	{
		int lev_ood = effective_depth(p_ptr->depth) - r_ptr->level;

		/*Too strong*/
		if (r_ptr->mon_power > max_power) return (0);

		else if (lev_ood > 5) return (MON_RARE_FREQ);
		else return (MON_LESS_FREQ);
	}
	else if ((r_ptr->level < effective_depth(p_ptr->depth) - 5) && (r_ptr->level < 75))
	{
		int lev_ood = effective_depth(p_ptr->depth) - r_ptr->level;

		/*Too weak*/
		if (r_ptr->mon_power < max_power / 20) return (0);

		else if (r_ptr->mon_power < max_power / 10) return (MON_RARE_FREQ);
		else if (lev_ood > 5) return (MON_LESS_FREQ);
	}
	/*The rigth depth*/
	return (r_ptr->max_num);
}

#define ENABLE_WAVES 1

#ifdef ENABLE_WAVES

/*
 * Pick a point from the border of a rectangle given its top left corner (p1)
 * and its bottom right corner (p2).
 * Assumes (p1.x < p2.x) and (p1.y < p2.y)
 * Returns the selected point in "result".
 */
static void pick_point_from_border(coord p1, coord p2, coord *result)
{
	/* Pick a side of the rectangle */
	switch (rand_int(4))
	{
		/* Top border */
		case 0:
		{
			/* Fixed coordinate */
			result->y = p1.y;
			result->x = rand_range(p1.x, p2.x);
			break;
		}
		/* Bottom border */
		case 1:
		{
			/* Fixed coordinate */
			result->y = p2.y;
			result->x = rand_range(p1.x, p2.x);
			break;
		}
		/* Left border */
		case 2:
		{
			/* Fixed coordinate */
			result->x = p1.x;
			result->y = rand_range(p1.y, p2.y);
			break;
		}
		/* Right border */
		case 3:
		{
			/* Fixed coordinate */
			result->x = p2.x;
			result->y = rand_range(p1.y, p2.y);
			break;
		}
	}
}


/*
 * Construct a chain of waves for a water lake, given the coordinates of the
 * rectangle who contains the lake.
 */
static void build_waves(int y1, int x1, int y2, int x2)
{
	coord p1, p2;
	coord start, end;
	int dx, dy;
	int wid, hgt;

	int tries = 0;

	/* Fill in the point structures */
	p1.x = x1;
	p1.y = y1;

	p2.x = x2;
	p2.y = y2;

	/* Paranoia */
	if (!in_bounds_fully(y1, x1) || !in_bounds_fully(y2, x2)) return;

	/* Get the size of the lake */
	hgt = y2 - y1 + 1;
	wid = x2 - x1 + 1;

	/* We don't like small lakes */
	if ((hgt < 15) || (wid < 15)) return;

	/* Pick a pair of points from the border of the rectangle */
	while (TRUE)
	{
		int chain_wid, chain_hgt;

		/* Paranoia */
		if (++tries > 2500) return;

		/* Pick the points */
		pick_point_from_border(p1, p2, &start);
		pick_point_from_border(p1, p2, &end);

		/* These points define a rectangle. Get its size */
		chain_hgt = (int)start.y - (int)end.y;
		chain_hgt = ABS(chain_hgt) + 1;

		chain_wid = (int)start.x - (int)end.x;
		chain_wid = ABS(chain_wid) + 1;

		/* This new rectangle must be noticeable */
		if ((chain_hgt >= (2 * hgt / 3)) &&
			(chain_wid >= (2 * wid / 3))) break;
	}

	/* Reset the loop count */
	tries = 0;

	/* Join the points using waves (similar to build_tunnel) */
	while ((start.x != end.x) || (start.y != end.y))
	{
		int x, y, i, wave;

		/* Paranoia */
		if (++tries > 2500) return;

		/* Get the current coordinate of the chain */
		y = start.y;
		x = start.x;

		/* Get the next coordinate */
		correct_dir(&dy, &dx, start.y, start.x, end.y, end.x);

		/* Get the next coordinate of the chain (for later) */
		start.y += dy;
		start.x += dx;

		/* Paranoia */
		if (!in_bounds_fully(y, x)) break;

		/* Ignore non-water */
		if (!cave_ff3_match(y, x, FF3_WATER)) continue;

		/* Pick the proper "crest" feature */
		if (cave_ff2_match(y, x, FF2_DEEP))
		{
			wave = FEAT_CREST_H;
		}
		else
		{
			wave = FEAT_CREST;
		}

		/* A chain of waves is built using "crest of waves" */
		cave_set_feat(y, x, wave);

		/* Sometimes, surround the crest with "plain" waves */
		if (rand_int(100) < 50)	continue;

		/* Place the surrounding waves (similar to build_streamer) */
		for (i = 0; i < 2; i++)
		{
			/* Pick a nearby location (very close) */
			int yy = rand_spread(y, 1);
			int xx = rand_spread(x, 1);

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Ignore non-water */
			if(!cave_ff3_match(yy, xx, FF3_WATER)) continue;

			/* Pick the proper "wave" feature */
			if (cave_ff2_match(yy, xx, FF2_DEEP))
			{
				wave = FEAT_WAVE_H;
			}
			else
			{
				wave = FEAT_WAVE;
			}

			/* Place the wave */
			cave_set_feat(yy, xx, wave);
		}
	}
}

#endif /* ENABLE_WAVES */


/* Available size for lakes (in blocks) */
enum
{
	LAKE_DATA_2x2,
	LAKE_DATA_2x3,
	LAKE_DATA_3x3,
	LAKE_DATA_3x4,
	LAKE_DATA_4x4,
	LAKE_DATA_4x5,
	MAX_LAKE_DATA
};

/* Block information for lakes (sorted by size, smaller first) */
static const room_data lake_data[MAX_LAKE_DATA] =
{
	{0, 1, 0, 1, 1},		/* LAKE_DATA_2x2 */
	{0, 1, -1, 1, 1},		/* LAKE_DATA_2x3 */
	{-1, 1, -1, 1, 1},		/* LAKE_DATA_3x3 */
	{-1, 1, -1, 2, 1},		/* LAKE_DATA_3x4 */
	{-1, 2, -1, 2, 1},		/* LAKE_DATA_4x4 */
	{-1, 2, -2, 2, 1},		/* LAKE_DATA_4x5 */
};


/*
 * Build a lake using the given feature.
 * Returns TRUE on success.
 * The coordinates of its center are stored in y0 and x0.
 */
static bool build_lake(int feat, bool do_big_lake, int *y0, int *x0)
{
	int bx0, by0;
	int bx1, by1, bx2, by2;
	int wid, hgt;
	int tries = 0;
	int lake_idx;
	const room_data *ld;
	/* Starburst flags */
	u32b flag = 0;

	/*
	 * Notice special cases: these are replaced with passable features
	 * sometimes (build_terrain)
	 */
	bool solid_lake = (feat_ff1_match(feat, FF1_WALL) &&
		(feat != FEAT_LIMESTONE) && (feat != FEAT_COAL));

	/* Solid lakes are made very large sometimes */
	if (solid_lake && one_in_(2)) do_big_lake = TRUE;

	/* Prevent secluded regions in the dungeon at shallow depths */
	if (!solid_lake && (effective_depth(p_ptr->depth) <= 10)) do_big_lake = FALSE;

	/* Choose an initial size for the lake */
	if (do_big_lake)
	{
		if (one_in_(10)) lake_idx = LAKE_DATA_4x5;
		else if (one_in_(5)) lake_idx = LAKE_DATA_4x4;
		else lake_idx = LAKE_DATA_3x4;
	}
	else
	{
		/*
		 * Lakes at shallow levels are smaller, and some at deeper
		 * levels too
		 */
		if ((effective_depth(p_ptr->depth) >= 25) && one_in_(7))
		{
			lake_idx = LAKE_DATA_3x4;
		}
		else
		{
			lake_idx = LAKE_DATA_2x3;
		}
	}

	/* Adjust the size of the lake, if needed */
	while (TRUE)
	{
		/* Get block information for this kind of lake */
		ld = &lake_data[lake_idx];

		/* Get the size of the lake in blocks */
		hgt = ld->dy2 - ld->dy1 + 1;
		wid = ld->dx2 - ld->dx1 + 1;

		/* Can be placed in this dungeon? */
		if ((hgt <= dun->row_rooms) && (wid <= dun->col_rooms)) break;

		/* Try again with a smaller lake */
		--lake_idx;

		/* Level too small, give up */
		if (lake_idx < 0)
		{
			if (cheat_room)
			{
				msg_c_format(MSG_NOTICE, "Can't place lakes in this dungeon!");
			}

			return (FALSE);
		}
	}

	/* Try to get a location for the lake */
	while (TRUE)
	{
		/* Too many tries. Reject lake */
		if (++tries >= 200)
		{
			if (cheat_room)
			{
				msg_c_format(MSG_NOTICE, "Can't find a blocks for lakes in this dungeon!");
			}

			return (FALSE);
		}

		/* Get central block */
		by0 = rand_int(dun->row_rooms);
		bx0 = rand_int(dun->col_rooms);

		/* Get the blocks */
		by1 = by0 + ld->dy1;
		by2 = by0 + ld->dy2;
		bx1 = bx0 + ld->dx1;
		bx2 = bx0 + ld->dx2;

		/* Ignore blocks outside the dungeon */
		if ((by1 < 0) || (by2 >= dun->row_rooms)) continue;
		if ((bx1 < 0) || (bx2 >= dun->col_rooms)) continue;

		/* Found a suitable location */
		break;
	}

	/* Get total height and width of the available space */
	hgt *= BLOCK_HGT;
	wid *= BLOCK_WID;

	/* Get the center of the lake */
	*y0 = by1 * BLOCK_HGT + hgt / 2;
	*x0 = bx1 * BLOCK_WID + wid / 2;

	/* Store extra information for passable lakes */
	if (!solid_lake)
	{
		/* Forests are always lit. Others not so much */
		if (feat_ff3_match(feat, FF3_FOREST) ||
			(effective_depth(p_ptr->depth) <= randint(25)))
		{
			flag |= (STAR_BURST_LIGHT);
		}

		/* Lakes are rooms now */
		flag |= (STAR_BURST_ROOM);

		/* Connect the lake with the dungeon */
		if (dun->cent_n < CENT_MAX)
		{
			dun->cent[dun->cent_n].y = *y0;
			dun->cent[dun->cent_n].x = *x0;
			dun->cent_n++;
		}

		/* We won't build rooms over small lakes */
		if (!do_big_lake)
		{
			int bx, by;

			for (by = by1; by <= by2; by++)
			{
				for (bx = bx1; bx <= bx2; bx++)
				{
					/* Mark the blocks as used */
					dun->room_map[by][bx] = TRUE;
				}
			}
		}
	}

	/*
	 * Convenience. Get the distance from the center to the borders.
	 * Note that we substract some space to place tunnels later and to
	 * avoid dungeon permanent boundry
	 */
	hgt = (hgt - 4) / 2;
	wid = (wid - 4) / 2;

	/* Place the lake */
	generate_starburst_room(*y0 - hgt, *x0 - wid, *y0 + hgt, *x0 + wid,
		feat, f_info[feat].f_edge, flag);

#ifdef ENABLE_WAVES
	/* Special case -- Give some flavor to water lakes */
	if ((feat == FEAT_WATER) || (feat == FEAT_WATER_H))
	{
		int i;

		for (i = 0; i < (do_big_lake ? 2: 1); i++)
		{
			build_waves(*y0 - hgt, *x0 - wid, *y0 + hgt, *x0 + wid);
		}
	}
#endif /* ENABLE_WAVES */

	/* Success */
	return (TRUE);
}


/*
 * Build a river given a feature and its starting location
 */
static void build_river(int feat, int y, int x)
{
	/*
	 * This map contains the directions of the grids who must be converted
	 * to edge, given a compass direction [0-3]
	 */
	static byte edge_map[][3] =
	{
		{1, 2, 3},
		{7, 8, 9},
		{3, 6, 9},
		{1, 4, 7}
	};

	int i, dir, old_dir;
	int old_feat;
	int edge = f_info[feat].f_edge;
	/*
	 * Notice special cases: they are replaced by passable features
	 * sometimes (build_terrain)
	 */
	bool solid_river = (feat_ff1_match(feat, FF1_WALL) &&
		(feat != FEAT_LIMESTONE) && (feat != FEAT_COAL));

	/* Choose a random compass direction */
	dir = old_dir = rand_int(4);

	/* Place river into dungeon */
	while (TRUE)
	{
		/* Stop at dungeon edge */
		if (!in_bounds_fully(y, x)) break;

		/* Get the previous content of the grid */
		old_feat = cave_feat[y][x];

		/* Stop at permanent feature */
		if (feat_ff1_match(old_feat, FF1_PERMANENT)) break;

		/* Most rivers aren't pierced by rooms. */
		if (!solid_river)
		{
			/* Forbid rooms here */
			int by = y / BLOCK_HGT;
			int bx = x / BLOCK_WID;

			dun->room_map[by][bx] = TRUE;
		}

		/* Place a piece of the river, if needed */
		if (feat != old_feat) build_terrain(y, x, feat);

		/* Place river edge, if needed */
		if (edge != FEAT_NONE)
		{
			for (i = 0; i < 3; i++)
			{
				/* Use the map to modify only grids ahead */
				int yy = y + ddy[edge_map[dir][i]];
				int xx = x + ddx[edge_map[dir][i]];

				/* Ignore annoying locations */
				if (!in_bounds_fully(yy, xx)) continue;

				/* Get the previous content of the grid */
				old_feat = cave_feat[yy][xx];

				/* Avoid permanent features */
				if (feat_ff1_match(old_feat, FF1_PERMANENT)) continue;

				/* IMPORTANT: Don't overwrite the river */
				if (old_feat == feat) continue;

				/* Place river edge, if needed */
				if (edge != old_feat) build_terrain(yy, xx, edge);
			}
		}

		/* Stagger the river */
		if (one_in_(2))
		{
			dir = rand_int(4);
		}
		/* Advance the streamer using the original direction */
		else
		{
			dir = old_dir;
		}

		/* Get the next coordinates */
		y += ddy_ddd[dir];
		x += ddx_ddd[dir];
	}
}


/*
 * Place lakes and rivers given a feature
 */
static bool build_feature(int feat, bool do_big_lake)
{
	/* No coordinates yet */
	int x0 = 0, y0 = 0;

	/* Build a lake? */
	if (feat_ff2_match(feat, FF2_LAKE) ||
		!feat_ff2_match(feat, FF2_RIVER))
	{
		/* Try to place the lake. Get its center */
		if (!build_lake(feat, do_big_lake, &y0, &x0)) return (FALSE);
	}

	/* Build a river */
	if (feat_ff2_match(feat, FF2_RIVER) && (ponder_dungeon_size() > 2))
	{
		/* Pick starting coordinates, if needed */
		if ((y0 + x0) == 0)
		{
			y0 = randint(p_ptr->cur_map_hgt - 2);
			x0 = randint(p_ptr->cur_map_wid - 2);
		}

		/* Generate the river */
		build_river(feat, y0, x0);
	}

	/* Success */
	return (TRUE);
}


/*#define ENABLE_DEVELOPER_FEATURE 1 */

#ifdef ENABLE_DEVELOPER_FEATURE

static int get_feature_from_developer(void)
{
	ang_file *fp;
	char line[513], *s, *name;
	int feat = 0;

	fp = file_open("developer_feature.txt", MODE_WRITE, FTYPE_TEXT);

	if (!fp) return 0;

	while (file_getl(fp, line, sizeof(line)))
	{
		for (s = line; *s && ((*s == ' ') || (*s == '\t')); s++) ;

		if (!*s || (*s == '#')) continue;

		for (feat = 1; feat < z_info->f_max; feat++)
		{
			name = f_name + f_info[feat].name;

			if (name[0] == '~') ++name;

			if (streq(s, name))
			{
				file_close(fp);

				return cave_feat_lake(feat) ? feat: 0;
			}
		}
	}

	file_close(fp);

	return 0;
}

#endif /* ENABLE_DEVELOPER_FEATURE */

/*
 * Build lakes and rivers for the dungeon
 */
static void build_nature(void)
{
	int i;
	bool big;
	char name[80];

	int feat, dev_feat = FEAT_NONE;
	int count = 0, max_features;

	/* Get the maximum number of features based on level size */
	byte dun_size = ponder_dungeon_size();

	 /* Clear the level's restriction */
	 level_flag = 0;

	/* No NPP terrains option turned on */
	if (adult_simple_dungeons) return;

	/* Debug message */
	if (cheat_room)
	{
		msg_c_format(MSG_NOTICE, "Dungeon size: %d.", (int)dun_size);
	}

	if (dun_size == 1) max_features = (one_in_(4) ? 1: 0);

	else if (dun_size == 2) max_features = 1;

	else if (dun_size == 3) max_features = (one_in_(3) ? 2: 1);

	else max_features = DUN_MAX_LAKES;

	/* Check quests for specific element flags */
 	for (i = 0; i < z_info->q_max; i++)
	{
		/* Get the quest */
		quest_type *q_ptr = &q_info[i];

		/* Active quest? */
		if ((q_ptr->base_level == p_ptr->depth) && !is_quest_complete(i)) continue;

		/* Monster quests */
		if ((quest_fixed(q_ptr)) || (quest_single_r_idx(q_ptr)))
		{
			/* Restrict feature generation */
			level_flag |= get_level_flag_from_race(&r_info[q_ptr->mon_idx]);
		}
		/* themed quests */
		else if (quest_themed(q_ptr))
		{
			u16b j;

			/* Find specific element flags for the theme */
			for (j = 0; j < N_ELEMENTS(themed_level_flags); j++)
			{
				/* Ignore other themes */
				if (themed_level_flags[j].theme != q_ptr->q_theme) continue;

				/* Restrict feature generation */
				level_flag |= themed_level_flags[j].flags;

				/* Done */
				break;
			}
		}
	}

	/* The chance to get special terrain varies with dungeon depth */
	if ((level_flag == 0) && (rand_int(300) >= effective_depth(p_ptr->depth))) return;

	/* Debug message */
	if (level_flag && cheat_room)
	{
		msg_c_format(MSG_NOTICE, "Level flags added by quests.");
		debug_all_level_flags(level_flag);
	}

#ifdef ENABLE_DEVELOPER_FEATURE

	/* Mega-hack -- Force the generation of certain features -DG- */
	/* Only in debug-mode */
	if (DEBUG_MODE_ACTIVATED) dev_feat = get_feature_from_developer();


#endif /* ENABLE_DEVELOPER_FEATURE */

	/* Allocate some lakes and rivers */
	for (count = 0; count < max_features; count++)
	{
		/* Very small levels always get a feature -DG- */
		if ((max_features > 1) && (rand_int(100) < 20))
		{
			continue;
		}

		/* Developer, are you testing features? */
		if (dev_feat)
		{
			feat = dev_feat;

			dev_feat = FEAT_NONE;
		}
		/* NPP releases only need this "else" block */
		else
		{
			/* Pick a feature */
			feat = pick_proper_feature(cave_feat_lake);
		}

		/* Got a valid feature? */
		if (feat)
		{
			/* Try a big lake */
			if ((dun_size <= 3) || (randint(150) < effective_depth(p_ptr->depth)))
			{
				big = TRUE;
			}
			else
			{
				big = FALSE;
			}

			/* Report creation of lakes/rivers */
			if (cheat_room)
			{
				feature_desc(name, sizeof (name), feat, FALSE, FALSE);

				if (f_info[feat].f_edge)
				{
					char edge[80];

					feature_desc(edge, sizeof(edge), f_info[feat].f_edge,
							FALSE, FALSE);

					msg_format("Building %s%s surrounded by %s.",
							big ? "big ": "", name, edge);
				}
				else
				{
					msg_format ("Building %s%s.", big ? "big ": "", name);
				}
			}

			/* Build one lake/river. */
			build_feature(feat, big);
		}
	}

	/* Debug message */
	if (level_flag && cheat_room)
	{
		msg_c_format(MSG_NOTICE, "Level flags added by lake generation.");
		debug_all_level_flags(level_flag);
	}
}


/*
 * Build lakes and other terrain features for the given themed level
 */
static void build_themed_level_nature(byte theme)
{
	u16b i;

	/* Clear the level flag */
	level_flag = 0;

	/* No NPP terrains option turned on */
	if (adult_simple_dungeons) return;

	/* Find if the theme has some restrictions to generate terrain */
	for (i = 0; i < N_ELEMENTS(themed_level_flags); i++)
	{
		/* Found the theme? */
		if (theme == themed_level_flags[i].theme)
		{
			/* Apply the restriction */
			level_flag |= themed_level_flags[i].flags;

			/* Done */
			break;
		}
	}

	/* TODO: add lakes */
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
	int total_mon_placed = 0;

	int max_uniques = 3;

	alloc_entry *table = alloc_race_table;

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	byte level_theme;

	dun_data dun_body;

	u16b monster_number, potential_monsters;

	/* Global data */
	dun = &dun_body;

	/* Set the level type */
	set_dungeon_type(DUNGEON_TYPE_THEMED_LEVEL);

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

	/* Select the monster theme, get the feeling */
	if (is_quest_level) level_theme = q_ptr->q_theme;
	else level_theme = get_level_theme(effective_depth(p_ptr->depth), FALSE);

	/* Insert the feeling now */
	feeling = level_theme + LEV_THEME_HEAD;

	/* Setup special features */
	build_themed_level_nature(level_theme);

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

	/*start over on all themed levels with less than 4 rooms due to inevitable crash*/
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

	/*get the hook*/
	get_mon_hook(level_theme);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Monsters can be up to 7 levels out of depth, 10 for a quest */
	max_depth = p_ptr->depth + (is_quest_level ? THEMED_LEVEL_QUEST_BOOST : THEMED_LEVEL_NO_QUEST_BOOST);

	/*don't make it too easy if the player isn't diving very fast*/
	if (p_ptr->depth < p_ptr->max_lev)
	{
		max_depth += ((p_ptr->max_lev - effective_depth(p_ptr->depth)) / 2);
	}

	/* Undead themed levels are just too hard */
	if (level_theme == LEV_THEME_UNDEAD) max_depth -= 2;

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
	if (is_quest_level) monster_number = QUEST_THEMED_LEVEL_NUM;
	else monster_number = 250;

	/* Undead themed levels are just too hard */
	if (level_theme == LEV_THEME_UNDEAD) monster_number /= 2;

	/* Reduce the number as monsters get more powerful*/
	monster_number -= ((effective_depth(p_ptr->depth) / 3) + randint(p_ptr->depth / 3));

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
	while (total_mon_placed < monster_number)
	{
		int y, x;
		bool dont_use = FALSE;
		int tries = 0;

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

		/* Limit the number of uniques per level */
		if ((r_ptr->flags1 & (RF1_UNIQUE)) && (max_uniques < 1)) dont_use = TRUE;

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

			/* Can't find a location for this monster */
			if (++tries >= 2500)
			{
				/* Remove restriction */
				get_mon_num_hook = NULL;

				/* Prepare allocation table */
				get_mon_num_prep();

				/* No themed level */
				return (FALSE);
			}

			y = rand_int(p_ptr->cur_map_hgt);
			x = rand_int(p_ptr->cur_map_wid);

			if (!cave_empty_bold(y,x)) continue;

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
		if (!place_monster_aux(y, x, r_idx, MPLACE_SLEEP)) continue;

		/* Count it */
		total_mon_placed++;

		if (r_ptr->flags1 & (RF1_UNIQUE)) max_uniques--;

		/* Mark 1 in 17 monsters for a bonus item */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

			if ((mon_cnt % 15) == 0) m_ptr->mflag |= (MFLAG_BONUS_ITEM);
		}
	}

	/*final preps if this is a quest level*/
	if (is_quest_level)
	{
		q_ptr->q_num_killed = 0;
		q_ptr->q_max_num = 0;

		/*
		 * Go through every monster, and mark them as a questor,
		 * then make them slightly faster, and light sleepers
		 */
		/* Process the monsters */
		for (i = 1; i < mon_max; i++)
		{
			monster_type *m_ptr = &mon_list[i];
			monster_race *r_ptr;

			/* Ignore non-existant monsters */
			if (!m_ptr->r_idx) continue;

			r_ptr = &r_info[m_ptr->r_idx];

			/*mark it as a quest monster*/
			m_ptr->mflag |= (MFLAG_QUEST);

			if (!(r_ptr->flags1 & RF1_UNIQUE))
			{
				m_ptr->mflag &= ~(MFLAG_SLOWER);
				m_ptr->mflag |= (MFLAG_FASTER);
				calc_monster_speed(m_ptr->fy, m_ptr->fx);
			}

			/*increase the max_num counter*/
			q_ptr->q_max_num ++;

			/*Not many of them sleeping, others lightly sleeping*/
			if (one_in_(2)) m_ptr->m_timed[MON_TMD_SLEEP] = 0;
			else m_ptr->m_timed[MON_TMD_SLEEP] /= 2;
		}

		/* Process the mimic objects */
		for (i = 1; i < o_max; i++)
		{
			object_type *o_ptr = &o_list[i];

			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;

			if (!o_ptr->mimic_r_idx) continue;

			/* Mark it as a questor */
			o_ptr->ident |= (IDENT_QUEST);

			/*increase the max_num counter*/
			q_ptr->q_max_num ++;
		}
	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Try hard to place this level */
	rating += 25;

	return (TRUE);
}


/*
 * The center of a fog formation must be close to water/ice. This is the
 * radius of the area we check for water/ice grids.
 */
#define FOG_SRC_RAD 7

/*
 * Build a fog formation on the dungeon
 */
static void build_fog(void)
{
	int x, y, x1, y1;
	int wid, hgt;
	int tries = 0;
	fractal_template *t_ptr;
	fractal_map map;
	/* Check room grids */
	bool want_room = (*dun_cap->can_place_fog_in_rooms)();
	bool locate_near_source = one_in_(2);
	byte type = (one_in_(5) ? FRACTAL_TYPE_33x33: FRACTAL_TYPE_17x17);

	/* Pick a location */
	while (TRUE)
	{
		bool is_near_source = FALSE;
		bool is_room;

		/* Too many failed attempts. Give up */
		if (++tries > 2500) return;

		/* Pick a random spot */
		y = rand_int(p_ptr->cur_map_hgt);
		x = rand_int(p_ptr->cur_map_wid);

		/* Must be passable */
		if (!cave_ff1_match(y, x, FF1_MOVE)) continue;

		/* Check room grids */
		is_room = ((cave_info[y][x] & (CAVE_ROOM)) != 0);

		/* Accept/reject room grids */
		if (want_room != is_room) continue;

		/* There are other restrictions to the center grid? */
		if (!locate_near_source) break;

		/* Yes. Place fog near water or ice grids */
 		for (y1 = y - FOG_SRC_RAD; y1 <= y + FOG_SRC_RAD; y1++)
		{
			for (x1 = x - FOG_SRC_RAD; x1 <= x + FOG_SRC_RAD; x1++)
			{
				/* Ignore annoying locations */
				if (!in_bounds(y1, x1)) continue;

				/* Water/ice? */
				if (cave_ff3_match(y1, x1, FF3_WATER | FF3_ICE))
				{
					/* Done */
					is_near_source = TRUE;

					break;
				}
			}

			/* Done? */
			if (is_near_source) break;
		}

		/* Done? */
		if (is_near_source) break;
	}

	/* Pick a fractal template */
	while (TRUE)
	{
		/* Pick any template index */
		int which = rand_int(N_ELEMENTS(fractal_repository));

		/* Get the template */
		t_ptr = &fractal_repository[which];

		/* The size matches the desired type? */
		if (t_ptr->type == type) break;
	}

	/* Create and initialize a fractal map */
	map = fractal_map_create(t_ptr);

	/* Complete the fractal map */
	fractal_map_complete(map, t_ptr);

	/* Get the map size */
	hgt = fractal_dim[type].hgt;
	wid = fractal_dim[type].wid;

	/* Get the top-left corner */
	y1 = y - hgt / 2;
	x1 = x - wid / 2;

	/* Build the fog formation based on the fractal map */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			/* Get dungeon coordinates */
			int yy = y1 + y;
			int xx = x1 + x;

			/* The map doesn't specifies a floor grid */
			if (map[y][x] < FRACTAL_FLOOR) continue;

			/* Ignore annoying locations */
			if (!in_bounds(yy, xx)) continue;

			/* Must be passable */
			if (!cave_ff1_match(yy, xx, FF1_MOVE)) continue;

			/* Forbid fog over open doors and stairs */
			if (cave_ff1_match(yy, xx, FF1_DOOR | FF1_STAIRS)) continue;

			/* Create the fog */
			set_effect_permanent_cloud(FEAT_FOG, yy, xx, 0, 0);
		}
	}

	/* Free resources */
	FREE(map);
}


/*
 * Pick a location for the center of a dungeon transformation (region, wall, etc.)
 * The location is stored in py and px.
 * flag must the LF1_* flag of the transformation. It can be 0.
 * marked_grids and num_marked_grids contain the array of grids marked with CAVE_TEMP.
 * These marked grids are the possible candidates for transformation centers.
 * Return TRUE on success, FALSE on failure.
 */
static bool pick_transform_center(coord *marked_grids, int num_marked_grids,
		u32b flag, int *py, int *px)
{
	int max = 300;
	int cur = 0;
	coord *grids;
	int i, j, k;
	int x = 0;
	int y = 0;
	bool found = FALSE;
	int rad = MAX_SIGHT * 2;

	/* First, find a random grid of the given element in the dungeon */
	if (flag && level_flag)
	{
		/* Allocate storage for a list of features that match that element */
		grids = C_ZNEW(max, coord);

		/* Scan the dungeon */
		for (y = 0; y < p_ptr->cur_map_hgt; y++)
		{
			for (x = 0; x < p_ptr->cur_map_wid; x++)
			{
				/* Get the feature */
				u16b feat = cave_feat[y][x];

				/* It must be an elemental feature */
				if (!feat_ff3_match(feat, TERRAIN_MASK)) continue;

				/* It must match the given flag */
				if (get_level_flag(feat) != flag) continue;

				/* Put in on the list */
				if (cur < max)
				{
					k = cur++;
				}
				/* Overwrite the list if there isn't more space */
				else
				{
					k = rand_int(max);
				}

				/* Save the data */
				grids[k].y = y;
				grids[k].x = x;
			}
		}

		/* Second. Pick a marked grid that is near to a valid elemental grid */
		if (cur > 0)
		{
			/* Try several times */
			for (i = 0; (i < 50) && !found; i++)
			{
				/* Pick a random elemental grid */
				k = rand_int(cur);

				/* Try several times */
				for (j = 0; (j < 100) && !found; j++)
				{
					/* Pick a random grid near the elemental grid */
					y = rand_spread(grids[k].y, rad);
					x = rand_spread(grids[k].x, rad);

					/* Check bounds */
					if (!in_bounds(y, x)) continue;

					/* It must be marked */
					if (cave_info[y][x] & (CAVE_TEMP)) found = TRUE;
				}
			}
		}

		/* Free storage */
		FREE(grids);

		/* Found? */
		if (found)
		{
			/* Return that location */
			*py = y;
			*px = x;
			return (TRUE);
		}
	}

	/* Paranoia */
	if (num_marked_grids < 1)
	{
		return (FALSE);
	}

	/* Default case. Just put it on some random location */
	for (i = 0; i < 100; i++)
	{
		/* Pick a random index */
		k = rand_int(num_marked_grids);

		/* Get coordinates */
		y = marked_grids[k].y;
		x = marked_grids[k].x;

		/* Found a marked grid? */
		if (cave_info[y][x] & (CAVE_TEMP))
		{
			/* Return the location */
			*py = y;
			*px = x;
			return (TRUE);
		}
	}

	/* Failure */
	return (FALSE);
}


/*
 * Transform walls and floors in the dungeon based on the given feature selector.
 * The transformation is fractal shaped and the central point of each fractal is
 * contained in the given array of grids. The grids selected from the array must
 * have assigned the CAVE_TEMP flag too (this enables us to control the location
 * of the transformed regions)
 */
static void transform_regions(coord *grids, int num_grids, feature_selector_type *fs_ptr)
{
	int max = 0, i;
	byte dun_size;
	bool done_big = FALSE;

	/* Paranoia */
	if (fs_ptr->size < 1) return;

	/* Get dungeon size measure */
	dun_size = ponder_dungeon_size();

	/* Get a number of regions suitable for each size */
	if (dun_size == 1)
	{
		max = 3;
	}
	else if (dun_size == 2)
	{
		max = 5;
	}
	else if (dun_size == 3)
	{
		max = 6;
	}
	/* Medium and large dungeons */
	else
	{
		int k = rand_int(100);

		if (k < 10) max = 10;
		else if (k < 30) max = 9;
		else max = 8;
	}

	/* Message */
	if (cheat_room)
	{
		msg_format("transform_regions: changing %d region%s.", max, (max == 1) ? "": "s");
	}

	/* Transform "max" regions */
	for (i = 0; i < max; i++)
	{
		u16b wall, floor;
		int wid, hgt;
		int y, x, y1, x1;
		fractal_template *t_ptr;
		fractal_map map;
		byte type;
		int tries = 0;
		u32b flags;

		/* Pick a wall feature and an optional floor feature */
		while (TRUE)
		{
			/* Select a feature pair */
			feature_selector_item_type *item = feature_selector_select(fs_ptr);

			/* Got one */
			if (item)
			{
				/* Get wall */
				wall = item->wall;

				/* Get floor */
				floor = item->floor;

				/* Get element flags */
				flags = item->level_flag;

				/* Accept feature */
				break;
			}

			/* Can't get a valid pair. Done */
			if (++tries > 50) return;
		}

		/* Pick location */
		if (!pick_transform_center(grids, num_grids, flags, &y, &x)) return;

		/* Default region size */
		type = FRACTAL_TYPE_33x33;

		/* Try to get a big region */
		if (!done_big && (dun_size >= 4) && one_in_(10))
		{
			type = FRACTAL_TYPE_33x65;

			/* Success */
			done_big = TRUE;
		}

		/* Pick a fractal template */
		while (TRUE)
		{
			/* Pick any template index */
			int which = rand_int(N_ELEMENTS(fractal_repository));

			/* Get the template */
			t_ptr = &fractal_repository[which];

			/* The size matches the desired type? */
			if (t_ptr->type == type) break;
		}

		/* Create and initialize a fractal map */
		map = fractal_map_create(t_ptr);

		/* Complete the fractal map */
		fractal_map_complete(map, t_ptr);

		/* Get the map size */
		hgt = fractal_dim[type].hgt;
		wid = fractal_dim[type].wid;

		/* Get the top-left corner */
		y1 = y - hgt / 2;
		x1 = x - wid / 2;

		/* Transfor the dungeon */
		for (y = 0; y < hgt; y++)
		{
			for (x = 0; x < wid; x++)
			{
				/* Get dungeon coordinates */
				int yy = y1 + y;
				int xx = x1 + x;

				u16b feat;

				/* The map doesn't specifies a floor grid */
				if (map[y][x] < FRACTAL_FLOOR) continue;

				/* Ignore annoying locations */
				if (!in_bounds(yy, xx)) continue;

				/* Remove mark */
				cave_info[yy][xx] &= ~(CAVE_TEMP);

				/* Ignore forbidden locations */
				if (cave_info[yy][xx] & (CAVE_ICKY)) continue;

				/* Get the current feature */
				feat = cave_feat[yy][xx];

				/* Certain features are forbidden */
				if (feat_ff1_match(feat, FF1_PERMANENT | FF1_DOOR | FF1_STAIRS | FF1_HAS_GOLD)) continue;

				/* Elemental features too */
				if (feat_ff3_match(feat, TERRAIN_MASK)) continue;

				/* Ignore features that contain  objects */
				if (cave_o_idx[yy][xx]) continue;

				/* Replace walls */
				if (feat_ff1_match(feat, FF1_WALL))
				{
					u16b new_wall = wall;

					/* Flavor */
					if ((wall == FEAT_LAVA_W) ||
						(wall == FEAT_BMUD_WALL) ||
						(wall == FEAT_BWATER_WALL) ||
						(wall == FEAT_ACID_WALL))
					{
						int k = rand_int(100);

						if (k < 7) new_wall = FEAT_SCORCHED_WALL;

						else if (k < 10) new_wall = FEAT_GRANITE_C;
					}
					else if (wall == FEAT_SANDSTONE)
					{
						int k = rand_int(100);

						if (k < 5) new_wall = FEAT_GRANITE_C;
					}
					else if (wall == FEAT_ICE_WALL)
					{
						int k = rand_int(100);

						if (k < 10) new_wall = FEAT_ICE_WALL_C;
					}
					else if (wall == FEAT_ICE_WALL_C)
					{
						int k = rand_int(100);

						if (k < 10) new_wall = FEAT_ICE_WALL;
					}
					else if (wall == FEAT_COAL)
					{
						int k = rand_int(100);

						if (k < 10) new_wall = FEAT_SCORCHED_WALL;

						else if (k < 17) new_wall = FEAT_BCOAL;
					}
					else if (wall == FEAT_SHALE)
					{
						int k = rand_int(100);

						if (k < 10) new_wall = FEAT_COAL;

						else if (k < 17) new_wall = FEAT_QUARTZ;
					}
					else if (wall == FEAT_VINES)
					{
						int k = rand_int(100);

						if (k < 10) new_wall = FEAT_EARTH_WALL;
					}

					cave_set_feat(yy, xx, new_wall);
				}
				/* Replace floor if necessary */
				else if ((floor != FEAT_NONE) && feat_ff1_match(feat, FF1_MOVE))
				{
					u16b new_floor = floor;

					/* Flavor */
					if (floor == FEAT_FLOOR_WET)
					{
						int k = rand_int(100);

						if (k < 30) new_floor = FEAT_WATER;
					}
					else if (floor == FEAT_FSOIL)
					{
						int k = rand_int(100);

						if (k < 20) new_floor = FEAT_EARTH;

						else if (k < 21) new_floor = FEAT_TREE;

						else if (k < 26) new_floor = FEAT_BUSH;
					}
					else if (floor == FEAT_SAND)
					{
						int k = rand_int(100);

						if (k < 7) new_floor = FEAT_ROCK;
					}
					else if (floor == FEAT_FLOOR_MUD)
					{
						int k = rand_int(100);

						if (k < 10) new_floor = FEAT_MUD;

						else if (k < 15) new_floor = FEAT_MUD_H;
					}

					cave_set_feat(yy, xx, new_floor);
				}
			}
		}

		/* Free resources */
		FREE(map);
	}
}


/*
 * Tranform walls in the dungeon based on the wall features contained in the
 * given feature selector
 * The location of the target grids are taken from the given array
 * The "rad" field of the pairs contained in the feature selector is used
 * to sometimes expand the size of the walls (flavor).
 */
static void transform_walls(coord *grids, int num_grids, feature_selector_type *fs_ptr)
{
	int max, y, x, i;

	/* Get the number of grids to tranform */

	/* First we get a divisor */
	/* Just one feature */
	if (fs_ptr->size == 1) max = 20;

	/* Rare features */
	else if (fs_ptr->total_chance < 150) max = 15;

	/* Regular case */
	else max = 8 - fs_ptr->size;

	/* Must have a lower bound */
	if (max < 4) max = 4;

	/* Apply the divisor to the number of grids of the array */
	max = num_grids / max;

	/* Paranoia */
	if (max < 1) return;

	/* Flavor */
	max += (rand_int(max) / 2);

	/* Transform "max" walls */
	for (i = 0; i < max; i++)
	{
		int yy, xx;
		int rad;
		bool is_effect = FALSE;

		/* Get a wall */
		feature_selector_item_type *item = feature_selector_select(fs_ptr);

		/* Got none */
		if ((item == NULL) || (item->wall == FEAT_NONE)) continue;

		/* Find out if it is an effect */
		if (feat_ff2_match(item->wall, FF2_EFFECT)) is_effect = TRUE;

		/* Pick a location */
		if (!pick_transform_center(grids, num_grids, item->level_flag, &y, &x)) return;

		/* Get the radius */
		rad = item->rad;

		/* Flavor for radius equal to 1 */
		if ((item->rad == 1) && one_in_(2)) rad = 0;

		/* Flavor for radius equal to 2 */
		if (item->rad == 2)
		{
			int k = rand_int(100);
			if (k < 20) rad = 0;
			if (k < 60) rad = 1;
			else rad = 2;
		}

		/* Tranform the walls based on the calculated radius */
		for (yy = (y - rad); yy <= (y + rad); yy++)
		{
			for (xx = (x - rad); xx <= (x + rad); xx++)
			{
				/* Ignore annoying locations */
				if (!in_bounds(yy, xx)) continue;

				/* The grid  must be marked too */
				if (cave_info[yy][xx] & (CAVE_TEMP))
				{
					/* Effects on walls */
					if (is_effect)
					{
						set_effect_inscription(item->wall, yy, xx, SOURCE_EFFECT, 0);
					}
					/* Normal walls */
					else
					{
						cave_set_feat(yy, xx, item->wall);
					}

					/* Clear the mark */
					cave_info[yy][xx] &= ~(CAVE_TEMP);
				}
			}
		}
	}
}


/* Transformation types */
#define TRANSFORM_WALL		1
#define TRANSFORM_REGION	2

/*
 * A table that holds information of elemental features and the proper
 * transformations that must be triggered if they are present in the dungeon
 */
static struct elemental_transformation_info {
	u32b level_flag;/* The LF1_* flag that must be present in the current level */
	byte type;	/* The transformation type (one of the TRANSFORM_* constants) */
	u16b wall;
	u16b floor;
	u16b chance;
	byte rad;
} elemental_transformations[] =
{
	{LF1_ICE, TRANSFORM_REGION, FEAT_ICE_WALL, FEAT_ICE, 200, 0},
	{LF1_ICE, TRANSFORM_REGION, FEAT_ICE_WALL_C, FEAT_ICE, 200, 0},

	{LF1_WATER, TRANSFORM_REGION, FEAT_LIMESTONE, FEAT_FLOOR_WET, 150, 0},

	{LF1_LAVA, TRANSFORM_REGION, FEAT_LAVA_W, FEAT_BURNT_S, 200, 0},

	{LF1_SAND, TRANSFORM_REGION, FEAT_SANDSTONE, FEAT_SAND, 100, 0},
	{LF1_SAND, TRANSFORM_REGION, FEAT_SANDSTONE, FEAT_SAND_H, 10, 0},

	{LF1_OIL, TRANSFORM_REGION, FEAT_COAL, FEAT_NONE, 200, 0},
	{LF1_OIL, TRANSFORM_REGION, FEAT_SHALE, FEAT_NONE, 100, 0},

	{LF1_FOREST, TRANSFORM_WALL, FEAT_PUTRID_FLOWER, FEAT_NONE, 10, 1},
	{LF1_FOREST, TRANSFORM_REGION, FEAT_VINES, FEAT_FSOIL, 150, 0},

	{LF1_MUD, TRANSFORM_REGION, FEAT_EARTH_WALL, FEAT_FLOOR_MUD, 100, 0},

	{LF1_BWATER, TRANSFORM_REGION, FEAT_BWATER_WALL, FEAT_NONE, 200, 0},

	{LF1_BMUD, TRANSFORM_REGION, FEAT_BMUD_WALL, FEAT_NONE, 200, 0},

	{LF1_FIRE, TRANSFORM_REGION, FEAT_SCORCHED_WALL, FEAT_BURNT_S, 100, 0},

	{LF1_ACID, TRANSFORM_REGION, FEAT_ACID_WALL, FEAT_BURNT_S, 100, 0},

	/* Marks the end of the list */
	{0,0,0,0,0,0}
};


/*
 * Try to add the LF1_* flags of num_rolls terrain features to level_flag.
 */
static void roll_level_flag(int num_rolls)
{
	u16b feat;
	int i;

	/* Try with num_rolls features */
	for (i = 0; i < num_rolls; i++)
	{
		/* Pick a lake feature */
		feat = pick_proper_feature(cave_feat_lake);

		/* Is it an elemental feature? */
		if (feat_ff3_match(feat, TERRAIN_MASK))
		{
			/* Get the element flag */
			u32b flag = get_level_flag(feat);

			/* Debug message */
			if (cheat_room && !(level_flag & flag))
			{
				char name[80];

				describe_one_level_flag(name, sizeof(name), flag);

				msg_c_format(MSG_NOTICE, "Adding %s to level_flag.", name);
			}

			/* Extend level_flag */
			level_flag |= flag;
		}
	}
}


/*
 * Transforms walls and regions in the dungeon to add more flavor to the game
 */
static void transform_walls_regions(void)
{
	coord *grids;
	int max_grids = 200;
	int num_grids = 0, idx;
	int y, x, i;
	u16b feat;
	bool enable_nature = FALSE;

	/* Feature selectors */
	feature_selector_type wall_sel_body, *wall_sel_ptr = &wall_sel_body;
	feature_selector_type region_sel_body, *region_sel_ptr = &region_sel_body;
	feature_selector_item_type item_body, *item = &item_body;

	/* Ignore wilderness dungeons */
	if (!(*dun_cap->can_be_transformed)()) return;

	/* Flavor */
	if (one_in_(20)) return;

	/* Initialize the feature selectors */
	feature_selector_init(wall_sel_ptr);
	feature_selector_init(region_sel_ptr);

	/* Add glowing walls */
	if ((effective_depth(p_ptr->depth) < 50) || !one_in_(4))
	{
		item->level_flag = 0;
		item->wall = FEAT_ELVISH_WALL;
		item->floor = FEAT_NONE;
		item->chance = 100;
		item->rad = 1;

		feature_selector_add(wall_sel_ptr, item);
	}

	/* Add silent watchers */
	if ((effective_depth(p_ptr->depth) < 10) ? FALSE: (effective_depth(p_ptr->depth) >= 35) ? one_in_(10): one_in_(20))
	{
		item->level_flag = 0;
		item->wall = FEAT_SILENT_WATCHER;
		item->floor = FEAT_NONE;
		item->chance = 30;
		item->rad = 0;

		feature_selector_add(wall_sel_ptr, item);
	}

	/* Add inscription effect */
	item->wall = FEAT_WALL_INSCRIPTION;
	item->floor = FEAT_NONE;
	item->chance = 20;
	item->rad = 0;

	feature_selector_add(wall_sel_ptr, item);

	/* Add teleport walls */
	if ((effective_depth(p_ptr->depth) < 5) ? FALSE: (effective_depth(p_ptr->depth) >= 40) ? one_in_(2): one_in_(10))
	{
		item->level_flag = 0;
		item->wall = FEAT_ETHEREAL_WALL;
		item->floor = FEAT_NONE;
		item->chance = 30;
		item->rad = 0;

		feature_selector_add(wall_sel_ptr, item);
	}

	feat = 0;

	/* Count elemental features in the current level */
	for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
	{
		for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
		{
			if (cave_ff3_match(y, x, TERRAIN_MASK)) ++feat;
		}
	}

	/* We are allowed to use elemental features depending on element presence or dungeon depth */
	if ((feat > 15) || (rand_int(200) < effective_depth(p_ptr->depth)))
	{
		/* Add more flavor */
		roll_level_flag(level_flag ? 3: 10);

		/* Allow elemental features */
		enable_nature = TRUE;
	}

	/* Traverse the array of elemental transformations */
	for (i = 0; enable_nature; i++)
	{
		/* Get the current elemental transformation */
		struct elemental_transformation_info *et_ptr = &elemental_transformations[i];

		/* The end of the array was reached */
		if (!et_ptr->level_flag) break;

		/* Allow only features compatible with the current level type */
		if (!(level_flag & et_ptr->level_flag)) continue;

		/* Check depth of features */
		if (TRUE)
		{
			int depth = -1;
			int damage = -1;

			/* Check walls */
			if (et_ptr->wall)
			{
				feature_type *f_ptr = &f_info[et_ptr->wall];

				depth = MAX(f_ptr->f_level, depth);

				damage = MAX(f_ptr->dam_non_native, damage);
			}

			/* Check floors */
			if (et_ptr->floor)
			{
				feature_type *f_ptr = &f_info[et_ptr->floor];

				depth = MAX(f_ptr->f_level, depth);

				damage = MAX(f_ptr->dam_non_native, damage);
			}

			/* Feature is OOD */
			if (depth > (effective_depth(p_ptr->depth) + 20))
			{
				/* Feature is too dangerous */
				if (damage > (p_ptr->mhp / 2)) continue;

				/* Sometimes we allow this feature */
				if (depth > (effective_depth(p_ptr->depth) + 40))
				{
					if (!one_in_(7)) continue;
				}
				else
				{
					if (!one_in_(4)) continue;
				}
			}
		}

		/* Create a feature selector item */
		item->level_flag = et_ptr->level_flag;
		item->wall = et_ptr->wall;
		item->floor = et_ptr->floor;
		item->chance = et_ptr->chance;
		item->rad = et_ptr->rad;

		/* Give it to the proper selector based on the transformation type */
		if (et_ptr->type == TRANSFORM_WALL)
		{
			feature_selector_add(wall_sel_ptr, item);
		}
		else if (et_ptr->type == TRANSFORM_REGION)
		{
			feature_selector_add(region_sel_ptr, item);
		}
	}

	/* We don't have a single feature pair */
	if ((wall_sel_ptr->size + region_sel_ptr->size) == 0) return;

	/* Allocate space for the grids */
	grids = C_ZNEW(max_grids, coord);

	/* Collect room walls locations for the transformations */
	for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
	{
		for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
		{
			bool ignore = TRUE;

			/* Must be room grids, and they don't have to be forbidden */
			if ((cave_info[y][x] & (CAVE_ROOM | CAVE_ICKY)) != (CAVE_ROOM)) continue;

			/* Get the current feature */
			feat = cave_feat[y][x];

			/* Ignore non-walls */
			if (!feat_ff1_match(feat, FF1_WALL)) continue;

			/* Ignore certain wall types */
			if (feat_ff1_match(feat, FF1_PERMANENT | FF1_INNER | FF1_HAS_GOLD)) continue;

			/* Ignore elemental walls */
			if (feat_ff3_match(feat, TERRAIN_MASK)) continue;

			/* They must be outer walls */
			for (i = 0; i < 8; i++)
			{
				int yy = y + ddy_ddd[i];
				int xx = x + ddx_ddd[i];

				/* Ignore walls adjacent to doors and stairs */
				if (cave_ff1_match(yy, xx, FF1_DOOR | FF1_STAIRS))
				{
					ignore = TRUE;
					break;
				}

				/* We found a non-room grid. Remember that */
				/* Keep looking for doors or stairs */
				if (!(cave_info[yy][xx] & (CAVE_ROOM))) ignore = FALSE;
			}

			/* Ignore the wall if necessary */
			if (ignore) continue;

			/* Mark the wall */
			cave_info[y][x] |= (CAVE_TEMP);

			/*
			 * Remember only some of the valid walls
			 * This prevents an excesive concentration of transformed walls
			 * in the lower part of the dungeon
			 */
			if (one_in_(4))
			{
				/* We still have free space in the array */
				if (num_grids < max_grids) idx = num_grids++;

				/* Overwrite an occupied entry */
				else idx = rand_int(max_grids);

				/* Save the coordinates */
				grids[idx].y = y;
				grids[idx].x = x;
			}
		}
	}

	/* Apply the transformations */
	if (num_grids > 0)
	{
		/* To walls */
		if (wall_sel_ptr->size > 0) transform_walls(grids, num_grids, wall_sel_ptr);

		/* To regions */
		if (region_sel_ptr->size > 0) transform_regions(grids, num_grids, region_sel_ptr);
	}

	/* Clear the marks */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			cave_info[y][x] &= ~(CAVE_TEMP);
		}
	}

	/* Debug message */
	if (cheat_room)
	{
		debug_all_level_flags(level_flag);
	}

	/* Free resources */
	FREE(grids);
}


/*
 * Place some miscellaneous features on dungeon.
 * Use this when dungeon generation is finished.
 */
static void build_misc_features(void)
{
	/* Moria dungeons are simple */
	if (game_mode == GAME_NPPMORIA) return;

	/* Sometimes we place fog on water and ice levels */
	if ((level_flag & (LF1_WATER | LF1_ICE)) && !(level_flag & (LF1_LAVA | LF1_FIRE)) &&
		(effective_depth(p_ptr->depth) >= 35) && one_in_(3))
	{
		int i, k = 2;

		/* One fog formation more if the levels is big enough */
		if (dun->cent_n >= 7) ++k;

		/* Sometimes two formations more */
		if ((dun->cent_n >= 10) && one_in_(2)) ++k;

		/* Sometimes three formations more */
		if ((dun->cent_n >= 12) && one_in_(3)) ++k;

		/* Build the fog formations */
		for (i = 0; i < k; i++)
		{
			/* Build */
			build_fog();
		}
	}

	/* More flavor! */
	if (!adult_simple_dungeons) transform_walls_regions();
}


/*
 * Build a fractal formation in the current level using the given feature.
 * y0 and x0 are the center of the formation, if they are out of bounds another point
 * is picked randomly
 * fractal_type tells us the dimensions of the formation
 * Chance is the probability (in 100) to use feat2 for a grid.
 * If chance is 0 feat2 is ignored
 * If feat2 is FEAT_NONE the grid remains untouched
 * if mode is 0 the inner part of the fractal is used
 * if mode is 1 the edge of the fractal is used
 */
static void build_formation(int y0, int x0, u16b feat, byte fractal_type, int chance, u16b feat2, byte mode)
{
	int y, x;
	int hgt, wid;
	int tries;

	fractal_template *t_ptr;
	fractal_map map;

	tries = 0;

	/* Pick a fractal template */
	while (TRUE)
	{
		/* Pick one template randomly */
		t_ptr = &fractal_repository[rand_int(N_ELEMENTS(fractal_repository))];

		/* It is of the proper type */
		if (t_ptr->type == fractal_type) break;

		/* Failure */
		if (++tries > 100) return;
	}

	/* Initialize the map */
	map = fractal_map_create(t_ptr);

	/* Create fractal */
	fractal_map_complete(map, t_ptr);

	/* Mark edges if necessary */
	if (mode == 1) fractal_map_mark_edge(map, t_ptr);

	/* Get dimensiones */
	hgt = fractal_dim[fractal_type].hgt;
	wid = fractal_dim[fractal_type].wid;

	/* Pick a random center point if necessary */
	while (!in_bounds(y0, x0))
	{
		y0 = randint(p_ptr->cur_map_hgt - 1);
		x0 = randint(p_ptr->cur_map_wid - 1);
	}

	/* Get top-left corner */
	y0 -= hgt / 2;
	x0 -= wid / 2;

	/* Apply the fractal to the dungeon */
	for (y = 0; y < hgt; y++)
	{
		/* Get real row */
		int yy = y0 + y;

		for (x = 0; x < wid; x++)
		{
			/* Get real column */
			int xx = x0 + x;

			/* Ignore annyoing grids */
			if (!in_bounds(yy, xx)) continue;

			/* Verify if we have to modify the grid. It depends on mode */
			if (mode == 0)
			{
				if (map[y][x] < FRACTAL_FLOOR) continue;
			}
			else
			{
				if (map[y][x] != FRACTAL_EDGE) continue;
			}

			/* See if we have to replace feat with feat2 */
			if ((chance > 0) && (rand_int(100) < chance))
			{
				if (feat2 != FEAT_NONE) cave_set_feat(yy, xx, feat2);
			}
			/* Common case */
			else
			{
				cave_set_feat(yy ,xx, feat);
			}
		}
	}

	/* Free resources */
	FREE(map);
}


/*
 * Helper function for add_wilderness_quest_terrain.  Take a spot, add dangerous wall terrain, and surround
 * it with 3-4 dangerous floor terrains. *
 */
static void add_wilderness_quest_terrain_aux(int y, int x, u16b floor_terrain, u16b wall_terrain)
{
	int yy, xx;
	int y_places[8];
	int x_places[8];
	int num_places = 0;
	int num_floor = randint1(2);
	int i;

	/* Build a solid wall here */
	cave_set_feat(y, x, wall_terrain);

	/* Now find areas to add dangerous floor terrains */
	for (i = 0; i < 8; i++)
	{
		yy = y + ddy[i];
		xx = x + ddx[i];

		if (!in_bounds_fully(yy, xx)) continue;
		if (cave_ff1_match(yy, xx, FF1_PERMANENT))
		{
			/* Don't alter stairs or walls */
			if (cave_ff1_match(yy, xx, FF1_WALL | FF1_STAIRS))continue;
		}
		y_places[num_places] = yy;
		x_places[num_places] = xx;
		num_places++;
	}

	/* Boundry control */
	if (!num_places) return;

	/* Add up to num_floor of the dangerous floor terrains */
	for (i = 0; i < num_floor; i++)
	{
		int k = randint0(num_places);
		yy = y_places[k];
		xx = x_places[k];

		/* Paranoia */
		if (!in_bounds_fully(yy, xx)) continue;

		/* Place the floor terrain, and then eliminate this square from future choices */
		cave_set_feat(yy, xx, floor_terrain);
		num_places--;
		y_places[k] = y_places[num_places];
		x_places[k] = x_places[num_places];
	}
}


/*
 * Add several pockets of terrain that will eventually overrun the level
 */
static void add_wilderness_quest_terrain(u16b floor_terrain, u16b wall_terrain)
{
	int hgt = p_ptr->cur_map_hgt;
	int wid = p_ptr->cur_map_wid;
	int j;

	/*
	 * Add some patches of wall and floor in each corner of the dungeon.
	 */
	for (j = 0; j < 4; j++)
	{
		int i, x, y;
		int coord_count = 0;
		int y_start, y_end, x_start, x_end, slot;
		int y_places[900];
		int x_places[900];

		/*
		 * Just to avoid repeating the code below,
		 * we assign the x and y ranges here and then
		 * loop through the code below 4 times
		 */
		switch (j)
		{
			/* Top left */
			case 1: {y_start = 1; y_end = 31; x_start = 1; x_end = 31; break;}
			/* Bottom left */
			case 2: {y_start = (hgt - 31); y_end = hgt; x_start = 1; x_end = 31; break;}
			/* Top right */
			case 3: {y_start = 1; y_end = 31; x_start = (wid - 31); x_end = wid; break;}
			/* Bottom left */
			default: {y_start = (hgt - 31); y_end = hgt; x_start = (wid - 31); x_end = wid; break;}
		}

		for (y = y_start; y < y_end; y++)
		{
			for (x = x_start; x < x_end; x++)
			{
				/* Paranoia */
				if (!in_bounds_fully(y, x)) continue;

				/* Don't alter stairs or permanent walls */
				if (cave_ff1_match(y, x, FF1_PERMANENT))
				{
					if (cave_ff1_match(y, x, FF1_WALL | FF1_STAIRS))continue;
				}

				/* store this square */
				y_places[coord_count] = y;
				x_places[coord_count] = x;
				coord_count++;
			}
		}

		for (i = 0; i < 8; i++)
		{
			/* Paranoia */
			if (!coord_count) break;

			/* Pick one of the set of coordinates */
			slot = randint0(coord_count);
			y = y_places[slot];
			x = x_places[slot];

			/* Paranoia */
			if (!in_bounds_fully(y, x)) continue;

			/* Place the floor terrain, and then eliminate this square from future choices */
			add_wilderness_quest_terrain_aux(y, x, floor_terrain, wall_terrain);
			coord_count--;
			y_places[slot] = y_places[coord_count];
			x_places[slot] = x_places[coord_count];
		}
	}
}


/*
 * Place the irregular dungeon border of wilderness levels
 */
static void build_border(int y, int x, u16b feat)
{
	/* Ignore icky grids */
	if (cave_info[y][x] & (CAVE_ICKY)) return;

	/* Ignore grids with effects */
	if (cave_x_idx[y][x]) return;

	/* Ignore grids with objects */
	if (cave_o_idx[y][x]) return;

	/* Ignore grids with monsters */
	if (cave_m_idx[y][x]) return;

	/* Place the border */
	cave_set_feat(y, x, feat);
}


/*
 * Create the irregular borders of a wilderness levels.
 * Based on code created by Nick McConnel (FAangband)
 */
static void build_wilderness_borders(u16b feat)
{
	int hgt = p_ptr->cur_map_hgt;
	int wid = p_ptr->cur_map_wid;
	int y, x;
	int i;

	int start_offset = 4;
	int max_offset = 7;

	/* Top border */
	i = start_offset;

	for (x = 0; x < wid; x++)
	{
		/* Modify the offset by -1, +1 or 0 */
		i += (1 - rand_int(3));

		/* Check bounds */
		if (i > max_offset) i = max_offset;
		else if (i < 0) i = 0;

		/* Place border */
		for (y = 0; y < i; y++)
		{
			build_border(y, x, feat);
		}
	}

	/* Bottom border */
	i = start_offset;

	for (x = 0; x < wid; x++)
	{
		/* Modify the offset by -1, +1 or 0 */
		i += (1 - rand_int(3));

		/* Check bounds */
		if (i > max_offset) i = max_offset;
		else if (i < 0) i = 0;

		/* Place border */
		for (y = 0; y < i; y++)
		{
			build_border(hgt - 1 - y, x, feat);
		}
	}

	/* Left border */
	i = start_offset;

	for (y = 0; y < hgt; y++)
	{
		/* Modify the offset by -1, +1 or 0 */
		i += (1 - rand_int(3));

		/* Check bounds */
		if (i > max_offset) i = max_offset;
		else if (i < 0) i = 0;

		/* Place border */
		for (x = 0; x < i; x++)
		{
			build_border(y, x, feat);
		}
	}

	/* Right border */
	i = start_offset;

	for (y = 0; y < hgt; y++)
	{
		/* Modify the offset by -1, +1 or 0 */
		i += (1 - rand_int(3));

		/* Check bounds */
		if (i > max_offset) i = max_offset;
		else if (i < 0) i = 0;

		/* Place border */
		for (x = 0; x < i; x++)
		{
			build_border(y, wid - 1 - x, feat);
		}
	}
}


/*
 * Build a full forest level
 * Returns TRUE on success
 */
static bool build_forest_level(void)
{
	int y, x;
	int i, j;
	int hgt, wid, wid2;

	/* Make it smaller size */
	hgt = p_ptr->cur_map_hgt = MAX_DUNGEON_HGT;
	wid = p_ptr->cur_map_wid = MAX_DUNGEON_WID;

	 /* Actual maximum number of rooms on this level */
	dun->row_rooms = p_ptr->cur_map_hgt / BLOCK_HGT;
	dun->col_rooms = p_ptr->cur_map_wid / BLOCK_WID;

	/* Cache center point, height currently isn't needed. */
	wid2 = wid / 2;

	/* Initialize the dungoen with forest soil */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			cave_set_feat(y, x, FEAT_FSOIL);

			cave_info[y][x] |= (CAVE_ROOM);
		}
	}

	/* Place some initial big earth formations */
	for (i = 0; i < 5; i++)
	{
		build_formation(-1, -1, FEAT_EARTH, FRACTAL_TYPE_33x65, 0, FEAT_NONE, 0);
	}

	/* Place one big earth wall formation most of the time */
	j = (one_in_(4) ? 0: 1);

	for (i = 0; i < j; i++)
	{
		build_formation(-1, -1, FEAT_EARTH_WALL, FRACTAL_TYPE_33x65, 0, FEAT_NONE, 0);
	}

	/* Place some irregular rooms of variable size */
	j = 3 + rand_int(20);

	for (i = 0; i < j; i++)
	{
		build_formation(-1, -1, FEAT_WALL_EXTRA, one_in_(3) ? FRACTAL_TYPE_9x9: FRACTAL_TYPE_17x17, 15,
			FEAT_GRANITE_C, 1);
	}

	/* Place a dense forest on the left side of the level */
	j = hgt * wid2;

	j = 10 * j / 100;

	for (i = 0; i < j; i++)
	{
		/* Pick random grid */
		y = randint(hgt - 1);
		x = randint(wid2 - 1);

		cave_set_feat(y, x, FEAT_TREE);
	}

	/* Place a clearer forest on the right side */
	j = hgt * wid2;

	j = 5 * j / 100;

	for (i = 0; i < j; i++)
	{
		/* Pick random grid */
		y = randint(hgt - 1);
		x = wid2 + randint(wid2 - 1);

		cave_set_feat(y, x, FEAT_TREE);
	}

	/* Scatter random features through the level (they pierce things) */
	j = hgt * wid;

	j = 5 * j / 100;

	for (i = 0; i < j; i++)
	{
		/* Pick random grid */
		y = randint(hgt - 1);
		x = randint(wid - 1);

		/* Place earth */
		cave_set_feat(y, x, FEAT_EARTH);

		/* Pick random grid */
		y = randint(hgt - 1);
		x = randint(wid - 1);

		/* Place forest soil, sometimes dynamic */
		cave_set_feat(y, x, (i < (j / 5)) ? FEAT_FSOIL_DYNAMIC: FEAT_FSOIL);
	}

	/* Place a few mud grids */
	j = one_in_(2) ? 30: 10;

	for (i = 0; i < j; i++)
	{
		/* Pick random grid */
		y = randint(hgt - 1);
		x = randint(wid - 1);

		cave_set_feat(y, x, FEAT_MUD);
	}

	/* Place a big lake most of the time */
	j = (one_in_(4) ? 0: 1);

	for (i = 0; i < j; i++)
	{
		build_formation(-1, -1, one_in_(2) ? FEAT_WATER : FEAT_MUD, FRACTAL_TYPE_33x65, 15, FEAT_NONE, 0);
	}

	/* Place some smaller earth wall formations */
	for (i = 0; i < 7; i++)
	{
		build_formation(-1, -1, FEAT_EARTH_WALL, FRACTAL_TYPE_17x17, 10, one_in_(2) ? FEAT_MUD: FEAT_NONE, 0);
	}

	/* Place some small vaults */
	for (i = 0; i < 5; i++)
	{
		/* They are somewhat rare */
		if (one_in_(10))
		{
			/* Pick a location */
			for (j = 0; j < 50; j++)
			{
				/* Get coordinates */
				y = rand_int(dun->row_rooms);
				x = rand_int(dun->col_rooms);

				/* Place the room */
				if (room_build(y, x, 7)) break;
			}
		}
	}

	/* Success */
	return (TRUE);
}


/*
 * Find a random location in the dungeon for a monster of the given race and store it
 * in py and px.
 * Return TRUE if succeeds, FALSE if fails.
 * Sometimes we try to place the monster in native terrain first.
 */
static bool pick_monster_location(monster_race *r_ptr, int *py, int *px)
{
	int max = 300;
	int cur = 0;
	coord *grids;
	bool found = FALSE;
	int tries, k;
	int x = 0;
	int y = 0;

	/* Get the LF1_* flags of the monster */
	u32b flag = get_level_flag_from_race(r_ptr);

	/* Search a native grid if necessary. Note that we always try if the monster is unique. */
	if (flag && (level_flag & flag) && ((r_ptr->flags1 & RF1_UNIQUE) || one_in_(2)))
	{
		/* Allocate storage for candidate grids */
		grids = C_ZNEW(max, coord);

		/* Scan the map */
		for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
		{
			for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
			{
				/* Found a suitable grid? */
				if (!cave_empty_bold(y, x) ||
						!cave_ff3_match(y, x, TERRAIN_MASK) ||
						!is_monster_native(y, x, r_ptr)) continue;

				/* Put it on the grid list */
				if (cur < max)
				{
					k = cur++;
				}
				/* Overwrite the list if there isn't more space */
				else
				{
					k = rand_int(max);
				}

				/* Save the location */
				grids[k].y = y;
				grids[k].x = x;
			}
		}

		/* Pick one of the candidates */
		if (cur > 0)
		{
			/* Pick a random index */
			k = rand_int(cur);
			/* Get the location */
			y = grids[k].y;
			x = grids[k].x;
			/* Remember the event */
			found = TRUE;
		}

		/* Free dynamic storage */
		FREE(grids);

		/* Found the grid? */
		if (found)
		{
			/* Debug message */
			if (cheat_room)
			{
				msg_c_format(MSG_NOTICE, "Found a NATIVE location for \"%s\".",
					r_ptr->name_full);
			}

			/* Return the location */
			*py = y;
			*px = x;
			return (TRUE);
		}
	}

	/* Find a random location */
	for (tries = 0; tries < 1000; tries++)
	{
		/* Get random coordinates */
		y = randint(p_ptr->cur_map_hgt - 2);
		x = randint(p_ptr->cur_map_wid - 2);

		/* Suitable grid? */
		if (cave_empty_bold(y, x) && cave_no_dam_for_mon(y, x, r_ptr))
		{
			/* Debug message */
			if (cheat_room)
			{
				msg_c_format(MSG_NOTICE, "Found a random location for \"%s\".",
					r_ptr->name_full);
			}

			/* Return the location */
			*py = y;
			*px = x;
			return (TRUE);
		}
	}

	/* Failure */
	return (FALSE);
}


/*
 * Place monsters and objects on a level
 * Returns TRUE on success
 */
static bool place_monsters_objects(void)
{
	int i;
	/* Hack - variables for allocations */
	s16b mon_gen, obj_gen;

	/* Reset generation variables */
	mon_gen = (*dun_cap->get_monster_count)();

	/* To make small levels a bit more playable */
	if (p_ptr->cur_map_hgt < MAX_DUNGEON_HGT || p_ptr->cur_map_wid < MAX_DUNGEON_WID)
	{
		int small_tester = mon_gen;

		mon_gen = ((mon_gen * p_ptr->cur_map_hgt) / MAX_DUNGEON_HGT) + 1;
		mon_gen = ((mon_gen * p_ptr->cur_map_wid) / MAX_DUNGEON_WID) + 1;

		if (mon_gen > small_tester) mon_gen = small_tester;
		else if (cheat_hear)
		{
			msg_format("Reduced monsters base from %d to %d", small_tester, mon_gen);
		}
	}

	/* Paranoia */
	if (mon_gen < 1) mon_gen = 1;

	mon_gen += randint(8);

	/* Put some monsters in the dungeon */
	for (i = mon_gen; i > 0; i--)
	{
		(void)alloc_monster(0, (MPLACE_SLEEP | MPLACE_GROUP));
	}

	/*
	 * Ensure quest monsters in Moria.
	 *
	 * The purpose of this function is to, when at level 50 or below, to place either
	 * vil Iggy or The Balrog of Moria.
	 */
	if (game_mode == GAME_NPPMORIA)
	{
		if (p_ptr->depth >= MORIA_QUEST_DEPTH)
		{
			int mon_count = 0;
			int mon_choice;

			/* Count the special monsters */
			for (i = 1; i < z_info->r_max; i++)
			{
				monster_race *r_ptr = &r_info[i];

				if (!(r_ptr->flags2 & (RF2_SPECIAL))) continue;

				if (r_ptr->max_num == 0) continue;

				mon_count++;
			}

			/* Paranoia */
			if (mon_count)
			{
				int y, x;
				monster_race *r_ptr;

				mon_choice = randint1(mon_count);
				mon_count = 0;

				/* Find the special monster of choice */
				for (i = 1; i < z_info->r_max; i++)
				{
					monster_race *r_ptr = &r_info[i];

					if (!(r_ptr->flags2 & (RF2_SPECIAL))) continue;
					if (r_ptr->max_num == 0) continue;

					mon_count++;

					/* Found it */
					if (mon_choice == mon_count) break;
				}

				r_ptr = &r_info[i];

				/* No big deal if it fails. We just place it one the next time a level is generated. */
				if (pick_monster_location(r_ptr, &y, &x))
				{
					place_monster_aux(y, x, i, MPLACE_OVERRIDE);
				}
			}
		}
	}

	/* Ensure quest monsters Angband */
	else for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Quest levels */
		if ((q_ptr->base_level == p_ptr->depth) && !is_quest_complete(i))
		{
			monster_race *r_ptr = &r_info[q_ptr->mon_idx];
			int y, x;

			if ((quest_fixed(q_ptr)) || (quest_single_r_idx(q_ptr)))
			{
				int j;

				/* A certain number of questors */
				s16b num_questors = q_ptr->q_max_num - q_ptr->q_num_killed;

				/* Ensure quest monsters */
				while (r_ptr->cur_num < num_questors)
				{
					/* Pick a location */
					if (!pick_monster_location(r_ptr, &y, &x)) return FALSE;

					/* Place the questor */
					place_monster_aux(y, x, q_ptr->mon_idx, (MPLACE_SLEEP | MPLACE_GROUP));
				}

				/* Process the monsters (backwards) */
				for (j = mon_max - 1; j >= 1; j--)
				{
					/* Access the monster */
					monster_type *m_ptr = &mon_list[j];

					/*mark it as a quest monster if applicable*/
					if (q_ptr->mon_idx == m_ptr->r_idx) m_ptr->mflag |= (MFLAG_QUEST);
				}
			}
		}
	}

	/* Put some objects in rooms */
	obj_gen = (*dun_cap->get_object_count)();
	if (obj_gen > 0) alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, obj_gen);

	/* Put some objects/gold in the dungeon */
	obj_gen = (*dun_cap->get_extra_object_count)();
	if (obj_gen > 0) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, obj_gen);

	obj_gen = (*dun_cap->get_gold_count)();
	if (obj_gen > 0) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, obj_gen);

	return (TRUE);
}


/*
 * Helper. Place an horizontal chain of ice mountains
 * row is the center of the chain
 */
static void build_ice_mountains(int row)
{
	int y, x;
	int y1, y2;
	/* Maximum offset */
	int max_offset = 10;
	/* Minimum width of the mountains */
	int pad = 2;

	/* Initial vertical offset */
	y1 = y2 = 4;

	/* Mountains are placed horizontally */
	for (x = 0; x < p_ptr->cur_map_wid; x++)
	{
		/* Randomize the top offset */
		y1 += 1 - rand_int(3);

		/* Check bounds */
		if (y1 < 0) y1 = 0;
		else if (y1 > max_offset) y1 = max_offset;

		/* Randomize the bottom offset */
		y2 += 1 - rand_int(3);

		/* Check bounds */
		if (y2 < 0) y2 = 0;
		else if (y2 > max_offset) y2 = max_offset;

		/* Place ice walls between top and bottom  */
		for (y = row - y1 - pad; y <= row + y2 + pad; y++)
		{
			/* Check sanity */
			if (in_bounds(y, x))
			{
				int k = rand_int(100);
				u16b feat;

				/* Flavor */
				if (k < 90) feat = FEAT_ICE_WALL;
				else feat = FEAT_ICE_WALL_C;

				/* Place the wall */
				cave_set_feat(y, x, feat);
			}
		}
	}
}


/*
 * Builds an ice level. Returns TRUE on success, FALSE on error
 */
static bool build_ice_level(void)
{
	int y, x;
	int i, j;
	int hgt, wid;
	int hgt2;

	/* Make it a smaller size */
	hgt = p_ptr->cur_map_hgt = MAX_DUNGEON_HGT;
	wid = p_ptr->cur_map_wid = MAX_DUNGEON_WID;

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = p_ptr->cur_map_hgt / BLOCK_HGT;
	dun->col_rooms = p_ptr->cur_map_wid / BLOCK_WID;

	hgt2 = hgt / 2;

	/* Start with floors */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			cave_set_feat(y, x, FEAT_FLOOR);

			cave_info[y][x] |= (CAVE_ROOM);
		}
	}

	/* Put lots of ice */
	for (i = 0; i < 15; i++)
	{
		build_formation(-1, -1, FEAT_ICE, FRACTAL_TYPE_33x65, 0, FEAT_NONE, 0);
	}

	/* Put some "mountains" */
	build_ice_mountains(hgt2);

	/* Add some granite formations */
	for (i = 0; i < 15; i++)
	{
		build_formation(-1, -1, FEAT_GRANITE_C, FRACTAL_TYPE_17x17, 0, FEAT_NONE, 0);
	}

	j = 20;

	/* Put some irregular ice walls to break los */
	for (i = 0; i < j; i++)
	{
		int tries;

		for (tries = 0; tries < 200; tries++)
		{
			/* Get random coordinates */
			y = randint(hgt - 1);
			x = randint(wid - 1);

			/* Location must be passable */
			if (cave_ff1_match(y, x, FF1_MOVE)) break;
		}

		build_formation(y, x, FEAT_ICE_WALL_C, FRACTAL_TYPE_17x17, 40, FEAT_NONE, 1);
	}

	j = 70 + rand_int(51);

	/* Put some pebbles */
	for (i = 0; i < j; i++)
	{
		int tries;

		for (tries = 0; tries < 200; tries++)
		{
			/* Get random coordinates */
			y = randint(hgt - 1);
			x = randint(wid - 1);

			/* Ignore ice */
			if (cave_ff3_match(y, x, ELEMENT_ICE)) continue;

			/* Location must be passable */
			if (cave_ff1_match(y, x, FF1_MOVE)) break;
		}

		cave_set_feat(y, x, FEAT_PEBBLES);
	}

	j = one_in_(4) ? (2 + rand_int(3)): 0;

	/* Add some water pools, sometimes */
	for (i = 0; i < j; i++)
	{
		int tries;

		for (tries = 0; tries < 200; tries++)
		{
			/* Get random coordinates */
			y = randint(hgt - 1);
			x = randint(wid - 1);

			/* Location must be passable */
			if (cave_ff1_match(y, x, FF1_MOVE)) break;
		}

		build_formation(y, x, FEAT_WATER_H, FRACTAL_TYPE_17x33, 10, FEAT_NONE, 0);
	}

	j = 4;

	/* Pierce the ice mountains making some tunnels */
	for (i = 0; i < j; i++)
	{
		int y2;
		int x2;
		int dist;

		/* Get start x coordinate */
		x = randint(wid - 1);

		do
		{
			/* Get final x coordinate */
			x2 = randint(wid - 1);

			/*
			 * Calculate distance. The end must be somewhat
			 * far away from the start
			 */
			dist = ABS(x2 - x);
		} while ((dist < 20) || (dist > 40));

		/* Get start y coordinate */
		y = rand_range(1, hgt2 - 20);

		/* Get final y coordinate */
		y2 = rand_range(hgt2 + 20, hgt - 2);

		/* Sometimes we swap the y coordinates */
		if (one_in_(2))
		{
			int tmp = y;
			y = y2;
			y2 = tmp;
		}

		/* Make a tunnel */
		build_tunnel(y, x, y2, x2);
	}

	/* Place some fractal rooms more */
	for (i = 0; i < 5; i++)
	{
		/* Only part of the time */
		if (one_in_(2))
		{
			/* Pick a location */
			for (j = 0; j < 50; j++)
			{
				/* Get coordinates */
				y = rand_int(dun->row_rooms);
				x = rand_int(dun->col_rooms);

				/* Place the room */
				if (room_build(y, x, 13)) break;
			}
		}
	}

	return (TRUE);
}


/*
 * Lite all elemental features in the level (walls included), and their adjacent grids
 * If show_objects is TRUE we mark the objects placed on such grids
 */
static void light_elements(bool show_objects)
{
	int y, x;

	/* Look for interesting grids all over the dungeon */
	for (y = 1; y < p_ptr->cur_map_hgt - 1; y++)
	{
		for (x = 1; x < p_ptr->cur_map_wid - 1; x++)
		{
			int i;

			/* Must be an elemental feature */
			if (!cave_ff3_match(y, x, TERRAIN_MASK)) continue;

			/* Lite that grid and grids adjacent to it */
			/* We don't need to call in_bounds */
			for (i = 0; i < 9; i++)
			{
				int yy = y + ddy_ddd[i];
				int xx = x + ddx_ddd[i];

				/* Lite the grid */
				cave_info[yy][xx] |= (CAVE_GLOW | CAVE_MARK);

				/* Remember its objects if necessary */
				if (show_objects)
				{
					/* Get the index of the first object */
					s16b o_idx = cave_o_idx[yy][xx];

					/* Mark all the objects of the pile */
					while (o_idx)
					{
						/* Get the object */
						object_type *o_ptr = &o_list[o_idx];

						/* Mark the object */
						o_ptr->marked = TRUE;

						/* Go to the next object */
						o_idx = o_ptr->next_o_idx;
					}
				}
			}
		}
	}
}


/*
 * Determine if a monster is suitable for the arena"
 */
static bool monster_wilderness_labrynth_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* No breeders */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) return (FALSE);

	/* creature must move */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE)) return (FALSE);

	/* No mimics */
	if (r_ptr->flags1 & (RF1_CHAR_MIMIC)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Builds a pseudo-wilderness level on the dungeon
 * Returns TRUE on success, FALSE on error
 */
static bool build_wilderness_level(void)
{
	int y, x, i;
	dun_data dun_body;
	bool done_ice = FALSE;
	bool is_quest_level = FALSE;
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* Global data */
	dun = &dun_body;

	/* Clear it */
	memset(dun, 0, sizeof(dun_body));

	/* Set level type */
	set_dungeon_type(DUNGEON_TYPE_WILDERNESS);

	/* Reset terrain flags */
	level_flag = 0;

	/* Leave the player in the air for now */
	p_ptr->py = p_ptr->px = 0;

	/*check if we need a quest*/
	if (quest_check(p_ptr->depth) == QUEST_WILDERNESS)
	{
		is_quest_level = TRUE;
	}

	/* Try with a forest */
	if ((effective_depth(p_ptr->depth) < 35) || one_in_(2))
	{
		if (!build_forest_level())
		{
			if (cheat_room) msg_format("failed to build a forest level");

			return (FALSE);
		}
	}
	/* Or try with an ice level */
	else
	{
		if (!build_ice_level())
		{
			if (cheat_room) msg_format("failed to build an ice level");

			return (FALSE);
		}

		done_ice = TRUE;
	}

	/* Irregular borders */
	build_wilderness_borders(FEAT_WALL_EXTRA);

	/* Mandatory dungeon borders */
	set_perm_boundry();

	/* Place 3 or 5 down stairs near some walls */
	if (!alloc_stairs(FEAT_MORE, (3 + randint(2))))
	{
		if (cheat_room) msg_format("failed to place down stairs");

		return (FALSE);
	}

	/* Place 1 or 3 up stairs near some walls */
	if (!alloc_stairs(FEAT_LESS, (1 + randint(2))))
	{
		if (cheat_room) msg_format("failed to place down stairs");

		return (FALSE);
	}

	/* Place some things */
	if (!place_traps_rubble_player())
	{
		if (cheat_room) msg_format("failed to place traps, rubble and player");

  		return FALSE;
	}

	/* We don't want to trap the player in the level */
	/* Build a tunnel to some far location */
	if (TRUE)
	{
		/* Get the larger dimension of the dungeon */
		int d = MAX(p_ptr->cur_map_hgt, p_ptr->cur_map_wid);
		int tries = 0;

		/* Get the distance to that far location */
		d = ((2 * d) / 5);

		/* Pick a location */
		while (TRUE)
		{
			/* Get coordinates */
			y = randint(p_ptr->cur_map_hgt - 1);
			x = randint(p_ptr->cur_map_wid - 1);

			/* Check distance */
			if (distance(y, x, p_ptr->py, p_ptr->px) >= d) break;

			/* Too many tries */
			if (++tries > 200) return (FALSE);
		}

		/* Build the tunnel */
		build_tunnel(p_ptr->py, p_ptr->px, y, x);
	}

	/* Additional features */
	build_misc_features();

	/* Start destruction of the level for quests */
	if (is_quest_level)
	{
		/* These terrain types are processed in dungeon.c during the quest level */
		if (done_ice) add_wilderness_quest_terrain(FEAT_BWATER, FEAT_BWATER_WALL);
		else add_wilderness_quest_terrain(FEAT_BMUD, FEAT_BMUD_WALL);
	}

	/*get the hook*/
	get_mon_num_hook = monster_wilderness_labrynth_okay;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Place some things */
	if (!place_monsters_objects())
	{
		/* Reset the allocation table */
		get_mon_num_hook = NULL;
		get_mon_num_prep();

		if (cheat_room) msg_format("failed to place monsters and objects");

		return FALSE;
	}

	/* Reset the allocation table */
	get_mon_num_hook = NULL;
	get_mon_num_prep();

	/* Special illumination for ice levels */
	if (done_ice && ((effective_depth(p_ptr->depth) < 50) || one_in_(4))) light_elements(TRUE);

	/*final preps if this is a quest level*/
	if (is_quest_level)
	{
		u16b obj_count = WILDERNESS_COLLECT;
		u16b mon_count = 0;

		q_ptr->q_num_killed = 0;
		q_ptr->q_max_num = 0;

		/*
		 * Go through every monster, and mark them as a questor,
		 * then make them slightly faster, and light sleepers
		 */
		/* Process the monsters */
		for (i = 1; i < mon_max; i++)
		{
			monster_type *m_ptr = &mon_list[i];

			/* Ignore non-existant monsters */
			if (!m_ptr->r_idx) continue;

			/*mark it as a quest monster*/
			m_ptr->mflag |= (MFLAG_QUEST);

			/* Count it */
			mon_count++;

			/* One in 25 generate a bonus item */
			if ((mon_max % 25) == 0) m_ptr->mflag |= (MFLAG_BONUS_ITEM);
		}

		/* Process the monsters */
		for (i = 1; i < mon_max; i++)
		{
			monster_type *m_ptr = &mon_list[i];

			/* Ignore non-existant monsters */
			if (!m_ptr->r_idx) continue;

			if (randint0(mon_count) < obj_count)
			{
				object_type *i_ptr;
				object_type object_type_body;

				/* Make a piece of parchment */
				int k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);
				i_ptr = &object_type_body;
				object_wipe(i_ptr);
				object_prep(i_ptr, k_idx);
				apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);

				/*Don't let the player see what the object it, and make it a quest item*/
				i_ptr->ident |= (IDENT_HIDE_CARRY | IDENT_QUEST);

				object_aware(i_ptr);
				object_known(i_ptr);

				(void)monster_carry(i, i_ptr);

				/* One less object to drop */
				obj_count--;
			}

			/* One less monster to count */
			mon_count--;

			/* We are done */
			if (!obj_count) break;
		}

		/* Drop some additional parchments */
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_PARCHMENT, (WILDERNESS_COLLECT / 2));
	}

	/* Drop some chests to make the level worth exploring */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_CHEST, damroll(3, 3));

	/* Let the player see the whole level */
	wiz_light();

	/* Try hard to place this level */
	rating += 25;

	/* Success */
	return (TRUE);
}


/**
 * Used to convert (x, y) into an array index (i) in build_labrynth_level().
 */
static int lab_toi(int y, int x, int w)
{
	return y * w + x;
}


/**
 * Used to convert an array index (i) into (x, y) in labyrinth_gen().
 */
static void lab_toyx(int i, int w, int *y, int *x)
{
	*y = i / w;
	*x = i % w;
}


/**
 * Given an adjoining wall (a wall which separates two labyrinth cells)
 * set a and b to point to the cell indices which are separated. Used by
 * labyrinth_gen().
 */
static void lab_get_adjoin(int i, int w, int *a, int *b)
{
	int y, x;
	lab_toyx(i, w, &y, &x);
	if (x % 2 == 0)
	{
		*a = lab_toi(y - 1, x, w);
		*b = lab_toi(y + 1, x, w);
	}
	else
	{
		*a = lab_toi(y, x - 1, w);
		*b = lab_toi(y, x + 1, w);
	}
}


/**
 * Shuffle an array using Knuth's shuffle.
 */
static void shuffle(int *arr, int n)
{
	int i, j, k;
	for (i = 0; i < n; i++)
	{
		j = randint0(n - i) + i;
		k = arr[j];
		arr[j] = arr[i];
		arr[i] = k;
	}
}


/**
 * Return whether (x, y) is in a tunnel.
 *
 * For our purposes a tunnel is a horizontal or vertical path, not an
 * intersection. Thus, we want the squares on either side to walls in one
 * case (e.g. up/down) and open in the other case (e.g. left/right). We don't
 * want a square that represents an intersection point.
 *
 * The high-level idea is that these are squares which can't be avoided (by
 * walking diagonally around them).
 */
static bool lab_is_tunnel(int y, int x)
{
	bool west = cave_naked_bold(y, x - 1);
	bool east = cave_naked_bold(y, x + 1);
	bool north = cave_naked_bold(y - 1, x);
	bool south = cave_naked_bold(y + 1, x);

	return ((north == south) && (west == east) && (north != west));
}


/* Note the height and width must be an odd number */
#define LABYRINTH_HGT 41
#define LABYRINTH_WID 81
#define LABYRINTH_AREA (LABYRINTH_WID * LABYRINTH_HGT)


/**
 * Build a labyrinth level.
 *
 * Note that if the function returns FALSE, a level wasn't generated.
 * Labyrinths use the dungeon level's number to determine whether to generate
 * themselves (which means certain level numbers are more likely to generate
 * labyrinths than others).  Taken from Angband 3.3.
 */
static bool build_labyrinth_level(void)
{
	int i, j, k, y, x;
	bool is_quest_level = FALSE;
	int cave_squares_x[LABYRINTH_AREA];
	int cave_squares_y[LABYRINTH_AREA];
	int cave_squares_max = 0;

	/*
	 * Size of the actual labyrinth part must be odd.
	 * NOTE: these are not the actual dungeon size, but rather the size of the
	 * area we're generating a labyrinth in (which doesn't count the enclosing
	 * outer walls.
	 */

	int hgt = LABYRINTH_HGT;
	int wid = LABYRINTH_WID;
	int area = hgt * wid;

	/* NOTE: 'sets' and 'walls' are too large... we only need to use about
	 * 1/4 as much memory. However, in that case, the addressing math becomes
	 * a lot more complicated, so let's just stick with this because it's
	 * easier to read. */

	/* 'sets' tracks connectedness; if sets[i] == sets[j] then cells i and j
	 * are connected to each other in the maze. */
	int sets[LABYRINTH_AREA];

	/* 'walls' is a list of wall coordinates which we will randomize */
	int walls[LABYRINTH_AREA];

	/* Most labyrinths are lit */
	bool lit = ((randint0(p_ptr->depth) < 25) || (one_in_(2)));

	/* Many labyrinths are known */
	bool known = (lit && (randint0(p_ptr->depth) < 25));

	/* Most labyrinths have soft (diggable) walls */
	bool soft = ((randint0(p_ptr->depth) < 35) || (!one_in_(3)));

	/* Check if we need a quest */
	if (quest_check(p_ptr->depth) == QUEST_LABYRINTH)
	{
		is_quest_level = TRUE;
		known = TRUE;

		/* Permanent walls for the quests */
		soft = FALSE;

		/* Quest levels are much smaller */
		wid = hgt = LABYRINTH_QUEST_DIMENSIONS;
		area = LABYRINTH_QUEST_AREA;
	}

	p_ptr->cur_map_hgt = hgt + 2;
	p_ptr->cur_map_wid = wid + 2;

	/* Set level type */
	set_dungeon_type(DUNGEON_TYPE_LABYRINTH);

	/* Reset terrain flags */
	level_flag = 0;

	/* Leave the player in the air for now */
	p_ptr->py = p_ptr->px = 0;

	/*
	 * Build permanent walls.
	 */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Create permanent wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);
		}
	}

	/*
	 * Start with solid wall everywhere.
	 */
	for (y = 1; y < p_ptr->cur_map_hgt - 1; y++)
	{
		for (x = 1; x < p_ptr->cur_map_wid - 1; x++)
		{
			/* Either soft or permanent walls */
			cave_set_feat(y, x, soft ? FEAT_WALL_SOLID : FEAT_PERM_SOLID);
		}
	}

	/* Initialize each wall. */
	for (i = 0; i < area; i++)
	{
		walls[i] = i;
		sets[i] = -1;
	}

	/* Cut out a grid of 1x1 rooms which we will call "cells" */
	for (y = 0; y < hgt; y += 2)
	{
		for (x = 0; x < wid; x += 2)
		{
			int k = lab_toi(y, x, wid);
			sets[k] = k;
			cave_set_feat(y + 1, x + 1, FEAT_FLOOR);
			cave_info[y + 1][x + 1] |= (CAVE_ROOM);
			if (lit) cave_info[y + 1][x + 1] |= (CAVE_GLOW);
		}
	}

	/* Shuffle the walls, using Knuth's shuffle. */
	shuffle(walls, area);

	/*
	 * For each adjoining wall, look at the cells it divides. If they aren't
	 * in the same set, remove the wall and join their sets.
	 *
	 * This is a randomized version of Kruskal's algorithm.
	  */
	for (i = 0; i < area; i++)
	{
		int a, b, x, y;

		j = walls[i];

		/* If this cell isn't an adjoining wall, skip it */
		lab_toyx(j, wid, &y, &x);
		if ((x < 1 && y < 1) || (x > wid - 2 && y > hgt - 2)) continue;
		if (x % 2 == y % 2) continue;

		/* Figure out which cells are separated by this wall */
		lab_get_adjoin(j, wid, &a, &b);

		/* If the cells aren't connected, kill the wall and join the sets */
		if (sets[a] != sets[b])
		{
			int sa = sets[a];
			int sb = sets[b];
			cave_set_feat(y + 1, x + 1, FEAT_FLOOR);
			if (lit) cave_info[y + 1][x + 1] |= (CAVE_GLOW);

			for (k = 0; k < area; k++)
			{
				if (sets[k] == sb) sets[k] = sa;
			}
		}
	}

	/* Place 1 or 2 down stairs  */
	if (!alloc_stairs(FEAT_MORE, (randint1(2))))
	{
		if (cheat_room) msg_format("failed to place down stairs");

		return (FALSE);
	}

	/* Place 1 or 2 up stairs */
	if (!alloc_stairs(FEAT_LESS, (randint1(2))))
	{
		if (cheat_room) msg_format("failed to place down stairs");
		return (FALSE);
	}

	/* No doors, traps or rubble in quest levels */
	if (!is_quest_level)
	{
		/* Test each square in (random) order for openness */
		for (y = 1; y < p_ptr->cur_map_hgt - 1; y++)
		{
			for (x = 1; x < p_ptr->cur_map_wid - 1; x++)
			{
				if (cave_naked_bold(y, x))
				{
					/* Not in the rooms */
					if (!lab_is_tunnel(y, x)) continue;

					cave_squares_y[cave_squares_max] = y;
					cave_squares_x[cave_squares_max] = x;
					cave_squares_max++;
				}
			}
		}

		/* Paranoia */
		if (!cave_squares_max)
		{
			if (cheat_room) msg_format("failed to place doors");
			return (FALSE);
		}

		/* Generate a door for every 100 squares in the labyrinth */
		for (i = area / 100; i > 0; i--)
		{
			/* Paranoia */
			if (!cave_squares_max) break;

			x = randint0(cave_squares_max);

			/* Place same doors */
			place_random_door(cave_squares_y[x], cave_squares_x[x]);

			/* Replace the current with the top one */
			cave_squares_max--;
			cave_squares_y[x] = cave_squares_y[cave_squares_max];
			cave_squares_x[x] = cave_squares_x[cave_squares_max];
		}

		/* Place some traps and rubble */
		x = MAX(MIN(p_ptr->depth / 3, 10), 2);
		x = (3 * x * (hgt * wid)) / (MAX_DUNGEON_HGT * MAX_DUNGEON_WID);
		alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(x));
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(x));
	}

	/* Determine the character location, if it is needed */
	if (!new_player_spot_old())
	{
		if (cheat_room) msg_format("failed to place player");

		return (FALSE);
	}

	/* Only put monsters on non-quest levels */
	if (!is_quest_level)
	{
		/*get the hook*/
		get_mon_num_hook = monster_wilderness_labrynth_okay;

		/* Prepare allocation table */
		get_mon_num_prep();

		/* Slightly out of depth */
		monster_level = effective_depth(p_ptr->depth) + 2;

		/* Place some things */
		if (!place_monsters_objects())
		{
			if (cheat_room) msg_format("failed to place monsters and objects");

			/* Reset the allocation table */
			get_mon_num_hook = NULL;
			get_mon_num_prep();
			monster_level = effective_depth(p_ptr->depth);

			return FALSE;
		}

		/* Reset the allocation table */
		get_mon_num_hook = NULL;
		get_mon_num_prep();
		monster_level = effective_depth(p_ptr->depth);
	}

	else q_info->turn_counter = (turn - 170);

	/* If we want the players to see the maze layout, do that now */
	if (known) wiz_light();

	/* Try hard to place this level */
	rating += 25;

	/* Success */
	return (TRUE);
}


/*
 * Helper function for building and updating arena levels.
 * Also called from cave.c to have the walls gradually disappear
 * as the arena quest progresses.
 */
void update_arena_level(byte stage)
{
	byte y, x;

	/* No values higher than 9, see arena_level_map in table.c */
	if (stage >= ARENA_MAX_STAGES) return;

	/*
	 * Start with add floor spaces where appropriate.
	 * See arena_level_map in tables.c
	 */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Ignore the outer walls locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Look for an exact match to the stage */
			if (arena_level_map[y][x] != stage) continue;

			/* Expand the floor area */
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Make it all one big room, and light it up */
			cave_info[y][x] |= (CAVE_ROOM | CAVE_GLOW);
			if (character_dungeon) light_spot(y, x);
		}
	}

	/*
	 * On the permanent walls bordering the floor, make them
	 * an outer permanent wall.
	 */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			byte d;

			/* Ignore the outer walls locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Really set the feature */
			if (cave_feat[y][x] != FEAT_PERM_SOLID) continue;

			/* Look in all directions to see if it borders a floor space */
			for (d = 0; d < 8; d++)
			{
				/* Extract adjacent location */
				byte yy = y + ddy_ddd[d];
				byte xx = x + ddx_ddd[d];

				if (!in_bounds_fully(yy, xx)) continue;

				/* Square (y, x) borders the floor */
				if (cave_ff1_match(yy, xx, FF1_MOVE))
				{
					/* Set it to be an inner permanent wall */
					cave_set_feat(y, x, FEAT_PERM_INNER);

					/* Make it all one big room, and light it up */
					cave_info[y][x] |= (CAVE_ROOM | CAVE_GLOW);
					if (character_dungeon) light_spot(y, x);

					/* Go to the next square */
					break;
				}
			}
		}
	}
}

/*
 * Helper function for build_arena_level.  Because it is a simple,
 * empty 1x5 corridor, placing the player should be easy *
 */
static bool player_place_arena(void)
{
	u16b empty_squares_y[ARENA_LEVEL_AREA];
	u16b empty_squares_x[ARENA_LEVEL_AREA];
	byte empty_squares = 0;
	byte slot, y, x;

	/*
	 * Start with add floor spaces where appropriate.
	 * See where the new squares are
	 */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* New, and open square */
			if (cave_naked_bold(y, x))
			{
				empty_squares_y[empty_squares] = y;
				empty_squares_x[empty_squares] = x;
				empty_squares++;
			}
		}
	}

	/* Paranoia - shouldn't happen */
	if (!empty_squares) return (FALSE);

	/* Pick a square at random */
	slot = randint0(empty_squares);

	/* Hack - excape stairs stairs */
	p_ptr->create_stair = FEAT_LESS;

	return (player_place(empty_squares_y[slot], empty_squares_x[slot]));
}


/*
 * Build a small room to place the player in an arena-like quest.
 * This level should only be built for arena quests.
 * Returns TRUE on success, FALSE on error, but there should never be an error.
 * Monsters and objects are added later in cave.c about every 100 game turns.
 */
static bool build_arena_level(void)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	byte y, x;

	/* Set level type */
	set_dungeon_type(DUNGEON_TYPE_ARENA);

	/* Reset terrain flags */
	level_flag = 0;

	/* Leave the player in the air for now */
	p_ptr->py = p_ptr->px = 0;

	/* Make it a single size, normal room */
	p_ptr->cur_map_hgt = ARENA_LEVEL_HGT;
	p_ptr->cur_map_wid = ARENA_LEVEL_WID;

	/*
	 * Start with solid wall everywhere.
	 * See arena_level_map in tables.c
	 */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Create permanent wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);
		}
	}

	/* Build the initial arena */
	update_arena_level(0);

	/* Should never fail, since there is only a simple dungeon floor */
	if (!player_place_arena())
	{
		if (cheat_room) msg_format("Failed to place player");

		return (FALSE);
	}

	/* Mark the start of the quest */
	q_ptr->turn_counter = turn;

	/* Always use this level */
	rating += 100;

	/* Success */
	return (TRUE);
}


/*
 * Helper function for build_greater_vault_level.
 * Placing the player should be easy *
 */
static bool player_place_greater_vault_level(void)
{
	u16b empty_squares_y[250];
	u16b empty_squares_x[250];
	byte empty_squares = 0;
	byte slot, y, x;

	/*
	 * Start with add floor spaces where appropriate.
	 * See where the new squares are
	 */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		if (empty_squares == 250) break;

		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			if (!in_bounds_fully(y, x)) continue;

			/* Not part of the vault */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* We want to be next to a wall */
			if (!next_to_walls(y, x)) continue;

			/* New, and open square */
			if (cave_naked_bold(y, x))
			{
				empty_squares_y[empty_squares] = y;
				empty_squares_x[empty_squares] = x;
				empty_squares++;
			}
		}
	}

	/* Paranoia - shouldn't happen */
	if (empty_squares < 3) return (FALSE);

	/* Pick a square at random */
	slot = 0;

	/* Hack - escape stairs */
	p_ptr->create_stair = FEAT_LESS;

	if (!player_place(empty_squares_y[slot], empty_squares_x[slot])) return (FALSE);

	/* Select a new location for down stairs */
	empty_squares--;
	empty_squares_y[slot] = empty_squares_y[empty_squares];
	empty_squares_x[slot] = empty_squares_x[empty_squares];

	/* Pick a square at random */
	slot = randint0(empty_squares);

	/* Now place one up stair */
	cave_set_feat(empty_squares_y[slot], empty_squares_x[slot], FEAT_LESS);

	/* Select a new location for down stairs */
	empty_squares_y[slot] = empty_squares_y[empty_squares];
	empty_squares_x[slot] = empty_squares_x[empty_squares];
	empty_squares--;

	/* Pick a square at random */
	slot = randint0(empty_squares);

	/* Now place one down stair */
	cave_set_feat(empty_squares_y[slot], empty_squares_x[slot], FEAT_MORE);

	return (TRUE);
}


/*
 * Build a small room to place the player in an arena-like quest.
 * This level should only be built for arena quests.
 * Returns TRUE on success, FALSE on error, but there should never be an error.
 * Monsters and objects are added later in cave.c about every 100 game turns.
 * Surround it with a border three walls thick of normal granite.
 */
static bool build_greater_vault_level(void)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	bool is_quest_level = FALSE;
	vault_type *v_ptr;
	int hgt, wid;

	/*check if we need a quest*/
	if (quest_check(p_ptr->depth) == QUEST_GREATER_VAULT)
	{
		is_quest_level = TRUE;
	}

	 /* Get the vault record */
	if (is_quest_level)
	{
		v_ptr = &v_info[q_ptr->q_theme];
	}
	/* For ordinary greater vault levels, select one at random */
	else while (TRUE)
	{
		u16b vault_choice = randint0(z_info->v_max);

		/* Get a random vault record */
		v_ptr = &v_info[vault_choice];

		/* Accept the first greater vault */
		if (v_ptr->typ == 8) break;
	}

	/* Set level type */
	set_dungeon_type(DUNGEON_TYPE_GREATER_VAULT);

	/* Reset terrain flags */
	level_flag = 0;

	/* Leave the player in the air for now */
	p_ptr->py = p_ptr->px = 0;

	/* Make it a single greater vault with  */
	hgt = p_ptr->cur_map_hgt = v_ptr->hgt + 6;
	wid = p_ptr->cur_map_wid = v_ptr->wid + 6;

	/* All floors to start, except the outer boundry */
	generate_fill(0, 0, hgt - 1, wid - 1, FEAT_WALL_OUTER);
	generate_fill(4, 4, hgt - 5, wid - 5, FEAT_FLOOR);
	set_perm_boundry();

	/* Message */
	if (cheat_room) msg_format("Greater vault (%s)", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* Build the vault */
	build_vault((hgt / 2), (wid / 2), v_ptr);

	/* Mark vault grids with the CAVE_G_VAULT flag */
	mark_g_vault((hgt / 2), (wid / 2), v_ptr->hgt, v_ptr->wid);

	/* Remember the vault's name */
	my_strcpy(g_vault_name, v_name + v_ptr->name, sizeof(g_vault_name));

	/* Should never fail, since there is only a simple dungeon floor */
	if (!player_place_greater_vault_level())
	{
		if (cheat_room) msg_format("Failed to place player");

		return (FALSE);
	}

	/*final preps if this is a quest level*/
	if (is_quest_level)
	{
		s16b i;

		/*
		 * Go through every monster, and mark them as a questor.
		 */
		for (i = 1; i < mon_max; i++)
		{
			monster_type *m_ptr = &mon_list[i];

			/* Ignore non-existant monsters */
			if (!m_ptr->r_idx) continue;

			/*mark it as a quest monster*/
			m_ptr->mflag |= (MFLAG_QUEST);
		}
	}

	/* Mark the start of the quest */
	q_ptr->turn_counter = turn;

	/* Always place this level */
	rating += 100;

	/* Success */
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
	bool fractal_level = FALSE;

	/*Hack - get the quest type*/
	byte quest_on_level = quest_check(p_ptr->depth);

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;

	/* Set level type */
	set_dungeon_type(DUNGEON_TYPE_DEFAULT);

	/* Possible "destroyed" level, but not in Moria  */
	if (game_mode == GAME_NPPMORIA) destroyed = FALSE;
	else if ((effective_depth(p_ptr->depth) > 10) && (one_in_(DUN_DEST))) destroyed = TRUE;

	/* Possible "fractal" level */
	if (!destroyed && (effective_depth(p_ptr->depth) >= 15) && one_in_(DUN_FRACTAL) && (!adult_simple_dungeons)) fractal_level = TRUE;

	/*Clear the level flag*/
	level_flag = 0;

	/*
	 * If we have visited the quest level, and found the
	 * artifact, don't re-create the vault.
	 */
	if (quest_on_level == QUEST_VAULT)
	{
		/*
		 * We already started the quest. Perhaps we have the object but
		 * we haven't gone back to the guild yet
		 */
		if (q_ptr->q_flags & (QFLAG_STARTED)) quest_on_level = 0;
	}

	/*see if we need a quest room*/
	if (quest_on_level)
	{
		/* Hack -- No destroyed "quest" levels */
		destroyed = FALSE;

		switch (q_ptr->q_type)
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

	if (((adult_force_small_lev) || (one_in_(SMALL_LEVEL)) ||
		(quest_on_level == QUEST_VAULT)) && (game_mode != GAME_NPPMORIA))
	{
		int l, m;

		while (TRUE)
		{
			/*
			 * Note: Panel height and width is 1/6 of max
			 * height/width
			 */
			l = randint(MAX_DUNGEON_HGT / (PANEL_HGT));
			m = randint(MAX_DUNGEON_WID / (PANEL_WID_FIXED));

			/* Make 2 panels the minimum size */
			if (l < 2) l = 2;
			if (m < 2) m = 2;

			/* Not too small for quest levels */
			if (quest_on_level)
			{
				if (l < 4) l = 4;
				if (m < 4) m = 4;
			}

			/*
			 * Make the dungeon height & width a multiple
			 * of 2 to 6 of panel hgt & width
			 */
			p_ptr->cur_map_hgt = l * (PANEL_HGT);
			p_ptr->cur_map_wid = m * (PANEL_WID_FIXED);

			/* Exit if less than normal dungeon */
			if ((p_ptr->cur_map_hgt < MAX_DUNGEON_HGT) ||
				(p_ptr->cur_map_wid < MAX_DUNGEON_WID)) break;
		}

		if ((cheat_room) && (!adult_force_small_lev))
		{
			 msg_format("A 'small' dungeon level (%dx%d).", m, l);
		}

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

	/* No features on destroyed level, or in Moria */
	if ((!destroyed) && one_in_(2) && (game_mode != GAME_NPPMORIA) && randint0(100) < p_ptr->depth)
	{
		/* Build lakes and rivers */
		build_nature();
	}

	/* Build some rooms */
	for (i = 0; i < DUN_ROOMS; i++)
	{
		/* Pick a block for the room */
		by = rand_int(dun->row_rooms);
		bx = rand_int(dun->col_rooms);

		if (game_mode == GAME_NPPMORIA)
		{
			if (rand_int(DUN_UNUSUAL_MORIA) < p_ptr->depth)
			{
				k = randint(3);

				if (k == 1)
				{
					if (room_build(by, bx, 2)) continue;
				}
				else if (k == 2)
				{
					if (room_build(by, bx, 3)) continue;
				}
				else if (room_build(by, bx, 4)) continue;

			}

			if (room_build(by, bx, 1)) continue;
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

				/* Ensure the quest artifact was generated properly */
				if (a_info[QUEST_ART_SLOT].a_cur_num != 1)
				{
					if (cheat_room) msg_format("quest artifact not generated properly");
					return (FALSE);
				}

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

		/* Fractal rooms in fractal levels */
		if (fractal_level)
		{
			int room_idx = (one_in_(4) ? 14: one_in_(2) ? 13: 12);

			if (room_build(by, bx, room_idx)) continue;

			/* We want mostly fractal rooms */
			if (rand_int(100) < 90) continue;
		}



		/* Attempt an "unusual" room */
		else if (rand_int(DUN_UNUSUAL) < effective_depth(p_ptr->depth) )
		{

			/* Roll for room type */
			k = rand_int(100);

			/* Attempt a very unusual room */
			if (rand_int(DUN_UNUSUAL) < effective_depth(p_ptr->depth))
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

		/* Occasionally attempt a starburst room */
		/* Maximum chance: one in 20 */
		if ((randint(800) <= MIN(effective_depth(p_ptr->depth), 40)) && (!adult_simple_dungeons))
		{
			int room_idx = (one_in_(10) ? 11 : 10);

			if (room_build(by, bx, room_idx)) continue;
		}

		/* Occasionally attempt a fractal room */
		/* Maximum chance: one in 7 */
		if ((randint(490) <= MIN(effective_depth(p_ptr->depth), 70)) && (!adult_simple_dungeons))
		{
			if (one_in_(8) && room_build(by, bx, 14)) continue;

			if (!one_in_(3) && room_build(by, bx, 13)) continue;

			if (room_build(by, bx, 12)) continue;
		}

		/* Attempt a trivial room */
		if (room_build(by, bx, 1)) continue;
	}

	/* Set the permanent walls */
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

	/* Place flavor features (fog, regions, walls, etc.) */
	build_misc_features();

	/* Place monsters and objects */
	if (!place_monsters_objects()) return (FALSE);

	return (TRUE);

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
static void build_store(u16b feat, int yy, int xx)
{
	int y, x, y0, x0, y1, x1, y2, x2, tmp;

	if (game_mode == GAME_NPPMORIA)
	{
		/* Find the "center" of the store */
		y0 = yy * 10 + 5;
		x0 = xx * 16 + 16;

		/* Determine the store boundaries */
		y1 = y0 - randint1(3);
		y2 = y0 + randint1(4);
		x1 = x0 - randint1(5);
		x2 = x0 + randint1(6);
	}
	else
	{
		/* Find the "center" of the store */
		y0 = yy * 9 + 6;
		x0 = xx * 11 + 11;

		/* Determine the store boundaries */
		y1 = y0 - randint((yy == 0) ? 2 : 1) - 1;
		y2 = y0 + randint((yy == 1) ? 2 : 1) + 1;
		x1 = x0 - randint(2) - 1;
		x2 = x0 + randint(2) + 1;
	}

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

	/* Clear previous contents, add a store door (add base store number) */
	cave_set_feat(y, x, feat);
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
	int y, x, k, n;

	int border = (PANEL_HGT / 2 + 1);

	u16b i;

	u16b rooms[MAX_STORES];

	u16b max_stores = MAX_STORES;

	if (game_mode == GAME_NPPMORIA) max_stores = 6;

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = seed_town;

	/* Prepare an Array of "remaining stores" */
	for (n = 0; n < MAX_STORES; n++) rooms[n] = 0;

	/* hack - make sure the right number of stores is generated in the moria town */
	if (game_mode == GAME_NPPMORIA) n = 6;

	/* Scan the table */
	for (i = 0; i < z_info->f_max; i++)
	{
		/* Get the feature */
		feature_type *f_ptr = &f_info[i];

		/*We are looking for the shops*/
		if (!(f_ptr->f_flags1 & (FF1_SHOP))) continue;

		/*paranoia*/
		if (f_ptr->f_power >= max_stores) continue;

		/*We found a shop*/
		rooms[f_ptr->f_power] = i;
	}

	/* Place two rows of stores */
	for (y = 0; y < 2; y++)
	{
		int stores_row = max_stores / 2;

		/* Place five stores per row, but leave one space for stairs */
		for (x = 0; x < stores_row; x++)
		{
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
		bool found_spot = TRUE;

		/* Find the "center" of the empty store space, in the empty gap */
		int y1 = rand_range(border, (p_ptr->cur_map_hgt - border));
		int x1 = rand_range(border, (p_ptr->cur_map_wid - border));

		/* Require a "naked" floor grid */
		if (!cave_naked_bold(y1, x1)) found_spot = FALSE;

		/*Require it to be surrounded by empty squares, so it is not right up against a store*/
		else for (i = 0; i < 8; i++)
		{
			y = y1 + ddy_ddd[i];
			x = x1 + ddx_ddd[i];

			if (!cave_naked_bold(y, x))
			{
				/*No need to look any further, start over*/
				found_spot = FALSE;
				break;
			}
		}

		/*We have a spot*/
		if (found_spot)
		{
			y = y1;
			x = x1;
			break;
		}
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

	/* Set level type */
	set_dungeon_type(DUNGEON_TYPE_TOWN);

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
			cave_set_feat(y, x, FEAT_COBBLESTONE_FLOOR);
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
		(void)alloc_monster(3, (MPLACE_SLEEP | MPLACE_GROUP));
	}
}


/*
 * Select and return one of the DUNGEON_TYPE_* constants
 * The selection is restricted by a number of things like depth and quests.
 */
static int pick_dungeon_type(void)
{
	/* Town */
	if (p_ptr->depth == 0)
	{
		return DUNGEON_TYPE_TOWN;
	}

	/* Classic moria level */
	if (game_mode == GAME_NPPMORIA)	return DUNGEON_TYPE_DEFAULT;

	/* Themed level quest */
	if (quest_check(p_ptr->depth) == QUEST_THEMED_LEVEL)
	{
		return DUNGEON_TYPE_THEMED_LEVEL;
	}

	/* Themed level quest */
	if (quest_check(p_ptr->depth) == QUEST_ARENA_LEVEL)
	{
		return DUNGEON_TYPE_ARENA;
	}

	/* Themed level quest */
	if (quest_check(p_ptr->depth) == QUEST_WILDERNESS)
	{
		return DUNGEON_TYPE_WILDERNESS;
	}
	/* Labyrinth level quest */
	if (quest_check(p_ptr->depth) == QUEST_LABYRINTH)
	{
		return DUNGEON_TYPE_LABYRINTH;
	}
	if (quest_check(p_ptr->depth) == QUEST_GREATER_VAULT)
	{
		return DUNGEON_TYPE_GREATER_VAULT;
	}

	if (!quest_check(effective_depth(p_ptr->depth)))
	{
		/* Random themed level */
		if (allow_themed_levels && (effective_depth(p_ptr->depth) >= 10) && (!adult_simple_dungeons) &&
			one_in_(THEMED_LEVEL_CHANCE))
		{
			return DUNGEON_TYPE_THEMED_LEVEL;
		}

		/* Random wilderness level */
		if ((effective_depth(p_ptr->depth) > 10) && (!adult_simple_dungeons) &&
			one_in_(WILDERNESS_LEVEL_CHANCE))
		{
			return DUNGEON_TYPE_WILDERNESS;
		}

		/* Random labyrinth level */
		if ((effective_depth(p_ptr->depth) > 15) && one_in_(LABYRINTH_LEVEL_CHANCE))
		{
			return DUNGEON_TYPE_LABYRINTH;
		}

		/* Random labyrinth level */
		if ((effective_depth(p_ptr->depth) > 30) && one_in_(GREATER_VAULT_LEVEL_CHANCE))
		{
			return DUNGEON_TYPE_GREATER_VAULT;
		}
	}

	/* Classic level */
	return DUNGEON_TYPE_DEFAULT;
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
	int dungeon_type = DUNGEON_TYPE_DEFAULT;
	/* The time to live of a dungeon type */
	/* This is used to prevent problems with different failure chances in level generation */
	int dungeon_type_ttl = 0;

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
		x_max = 1;
		feeling = 0;

		/* Remove all dynamic features */
		wipe_dynamic_terrain();

		/* Paranoia. Clear the current elemental flags */
		level_flag = 0;

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

				/* No monsters */
				cave_x_idx[y][x] = 0;


				for(i = 0; i < MAX_FLOWS; i++)
				{
					cave_cost[i][y][x] = 0;
				}

#ifdef MONSTER_SMELL
				cave_when[y][x] = 0;
#endif /* MONSTER_SMELL */

			}
		}

		/* Mega-Hack -- no player yet */
		p_ptr->px = p_ptr->py = 0;

		/* Hack -- illegal panel */
		Term->offset_y = MAX_DUNGEON_HGT;
		Term->offset_x = MAX_DUNGEON_WID;

		/* Reset the monster generation level */
		monster_level = effective_depth(p_ptr->depth);

		/* Reset the object generation level */
		object_level = effective_depth(p_ptr->depth);

		/* Nothing special here yet */
		good_item_flag = FALSE;

		/* Nothing good here yet */
		rating = 0;

		/* The current dungeon type is still valid */
		if (dungeon_type_ttl > 0)
		{
			--dungeon_type_ttl;
		}
		/* We must reset the dungeon type */
		else
		{
			/* Select a new level type */
			dungeon_type = pick_dungeon_type();

			/* Set the time to live */
			dungeon_type_ttl = 200;
		}

		switch (dungeon_type)
		{
			case DUNGEON_TYPE_TOWN:
			{
				/* Make a town */
				town_gen();

				/* Hack -- Clear stairs request */
				p_ptr->create_stair = 0;

				/* Always okay */
				okay = TRUE;

				break;
			}
			case DUNGEON_TYPE_THEMED_LEVEL:
			{
				/* Make a themed level */
				okay = build_themed_level();

				break;
			}
			case DUNGEON_TYPE_WILDERNESS:
			{
				/* Make a wilderness level */
				okay = build_wilderness_level();

				break;
			}
			case DUNGEON_TYPE_ARENA:
			{
				/* Make a wilderness level */
				okay = build_arena_level();

				break;
			}
			case DUNGEON_TYPE_LABYRINTH:
			{
				/* Make a wilderness level */
				okay = build_labyrinth_level();

				break;
			}
			case DUNGEON_TYPE_GREATER_VAULT:
			{
				/* Make a wilderness level */
				okay = build_greater_vault_level();

				break;
			}
			default:
			{
				/* Make a classic level */
				okay = cave_gen();

				break;
			}
		}

		/*message*/
		if(!okay)
		{
	   		if (cheat_room || cheat_hear || cheat_peek || cheat_xtra)
			{
				why = "defective level";
			}
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
				     ((effective_depth(p_ptr->depth) >= 5) && (feeling > 8)) ||
				     ((effective_depth(p_ptr->depth) >= 10) && (feeling > 7)) ||
				     ((effective_depth(p_ptr->depth) >= 20) && (feeling > 6)) ||
				     ((effective_depth(p_ptr->depth) >= 40) && (feeling > 5))))
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

		}

		/* Accept */
		if (okay) break;

		/* Message */
		if (why) msg_format("Generation restarted (%s)", why);

		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the monsters */
		wipe_mon_list();

		/* Wipe the monsters */
		wipe_x_list();
	}

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Reset the number of traps on the level. */
	num_trap_on_level = 0;

	/* Clear the summon mask */
	dungeon_summon_mask_f7 = 0L;
}


/*
 * Allow escorts
 */
static bool can_place_escorts_true(s16b r_idx)
{
	return (TRUE);
}

/*
 * Allow escorts
 */
static bool can_place_escorts_false(s16b r_idx)
{
	return (FALSE);
}

/*
 * Player in rooms
 */
static bool can_place_player_in_rooms_false(void)
{
	return (FALSE);
}


/*
 * Player in rooms
 */
static bool can_place_player_in_rooms_true(void)
{
	return (TRUE);
}


/*
 * Valid location for stairs
 */
static bool can_place_stairs_default(int y, int x)
{
	return (cave_naked_bold(y, x) ? TRUE: FALSE);
}


/*
 * Adjust the number of stairs in a level
 */
static int adjust_stairs_number_default(int initial_amount)
{
	/* Smaller levels don't need that many stairs, but there are a minimum of 4 rooms */
	if (dun->cent_n > 0)
	{
		if (dun->cent_n <= 4) initial_amount = 1;
		else if (initial_amount > (dun->cent_n / 2)) initial_amount = dun->cent_n / 2;
	}

	return (initial_amount);
}


/*
 * Fog in rooms
 */
static bool can_place_fog_in_rooms_default(void)
{
	return (one_in_(3));
}


/*
 * Fog in rooms
 */
static bool can_place_fog_in_rooms_true(void)
{
	return (TRUE);
}


/*
 * Fog in rooms
 */
static bool can_place_fog_in_rooms_false(void)
{
	return (TRUE);
}


/*
 * Feature is interesting for the look command
 */
static bool can_target_feature_default(int f_idx)
{
	return (feat_ff1_match(f_idx, FF1_NOTICE) ? TRUE: FALSE);
}


/*
 * Dungeon can be transformed
 */
static bool can_be_transformed_true(void)
{
	return (TRUE);
}


/*
 * Dungeon can be transformed
 */
static bool can_be_transformed_false(void)
{
	return (FALSE);
}


/*
 * Non native monsters in elemental terrain
 */
static bool can_place_non_native_monsters_false(void)
{
	return (FALSE);
}


/*
 * Non native monsters in elemental terrain
 */
static bool can_place_non_native_monsters_true(void)
{
	return (TRUE);
}


/*
 * Allow repopulation of monsters on level
 */
static bool allow_level_repopulation_true(void)
{
	return (TRUE);
}


/*
 * Disallow repopulation of monsters on level
 */
static bool allow_level_repopulation_false(void)
{
	return (FALSE);
}


/*
 * Only allow summoners to be relocated from the current level
 */
static bool limited_level_summoning_true(void)
{
	return (TRUE);
}


/*
 * Summoned monsters can be appear from nowhere
 */
static bool limited_level_summoning_false(void)
{
	return (FALSE);
}


/*
 * Only allow summoners to be relocated from the current level
 */
static bool allow_monster_multiply_true(void)
{
	return (TRUE);
}


/*
 * Summoned monsters can be appear from nowhere
 */
static bool allow_monster_multiply_false(void)
{
	return (FALSE);
}


/* Allow breeders to slowly spread */
static bool allow_monster_multiply_quarter(void)
{
	if (one_in_(4)) return (TRUE);
	return (FALSE);
}


/*
 * Earthquakes and destruction are prevented.
 */
static bool prevent_destruction_true(void)
{
	return (TRUE);
}


/*
 * Earthquakes and destruction are allowed.
 */
static bool prevent_destruction_false(void)
{
	return (FALSE);
}


/*
 * Earthquakes and destruction are allowed, except in town.
 */
static bool prevent_destruction_default(void)
{
	if (!p_ptr->depth) return (TRUE);

	return (FALSE);
}


/*
 * Monsters in level
 */
static int get_monster_count_default(void)
{
	return (MIN_M_ALLOC_LEVEL);
}


/*
 * Objects in rooms
 */
static int get_object_count_default(void)
{
	return (Rand_normal(DUN_AMT_ROOM, 3));
}


/*
 * Objects in rooms
 */
static int get_object_count_zero(void)
{
	return (0);
}


/*
 * Gold in both rooms and corridors
 */
static int get_gold_count_default(void)
{
	return (Rand_normal(DUN_AMT_GOLD, 3));
}


/*
 * Gold in both rooms and corridors
 */
static int get_gold_count_zero(void)
{
	return (0);
}


/*
 * objects in both rooms and corridors
 */
static int get_extra_object_count_default(void)
{
	return (Rand_normal(DUN_AMT_ITEM, 3));
}


/*
 * Dungeon capabilities for classic levels
 */
static dungeon_capabilities_type dun_cap_body_default =
{
	can_place_escorts_true,
	can_place_player_in_rooms_false,
	can_place_stairs_default,
	adjust_stairs_number_default,
	can_place_fog_in_rooms_default,
	can_target_feature_default,
	can_be_transformed_true,
	can_place_non_native_monsters_false,
	allow_level_repopulation_true,
	limited_level_summoning_false,
	allow_monster_multiply_true,
	prevent_destruction_default,
	get_monster_count_default,
	get_object_count_default,
	get_gold_count_default,
	get_extra_object_count_default,
};

/*
 * Monsters in level
 */
static int get_monster_count_moria(void)
{
	int alloc_level = (p_ptr->depth / 3);

	/* Boundary Control */
	if (alloc_level < 2) alloc_level = 2;
	else if (alloc_level > 10) alloc_level = 10;

	alloc_level += MIN_M_ALLOC_LEVEL;

	return (alloc_level);
}


/*
 * Objects in rooms
 */
static int get_object_count_moria(void)
{
	return (Rand_normal(DUN_AMT_ROOM_MORIA, 3));
}


/*
 * Gold in both rooms and corridors
 */
static int get_gold_count_moria(void)
{
	return (Rand_normal(DUN_AMT_GOLD_MORIA, 3));
}

/*
 * objects in both rooms and corridors
 */
static int get_extra_object_count_moria(void)
{
	int alloc_level = (p_ptr->depth / 3);

	/* Boundary Control */
	if (alloc_level < 2) alloc_level = 2;
	else if (alloc_level > 10) alloc_level = 10;

	return (DUN_AMT_ITEM_MORIA + randint1(alloc_level));
}


/*
 * Dungeon capabilities for classic levels
 */
static dungeon_capabilities_type dun_cap_body_moria =
{
	can_place_escorts_false,
	can_place_player_in_rooms_false,
	can_place_stairs_default,
	adjust_stairs_number_default,
	can_place_fog_in_rooms_false,
	can_target_feature_default,
	can_be_transformed_false,
	can_place_non_native_monsters_false,
	allow_level_repopulation_true,
	limited_level_summoning_false,
	allow_monster_multiply_true,
	prevent_destruction_default,
	get_monster_count_moria,
	get_object_count_moria,
	get_gold_count_moria,
	get_extra_object_count_moria,
};



/*
 * Allow escorts
 */
static bool can_place_escorts_wild(s16b r_idx)
{
	u32b flags1 = r_info[r_idx].flags1;

	/* Uniques are allowed */
	if (flags1 & (RF1_UNIQUE)) return (TRUE);

	/* Monsters with escorts are allowed only part of the time */
	if (flags1 & (RF1_FRIEND | RF1_FRIENDS | RF1_ESCORT | RF1_ESCORTS)) return (one_in_(10));

	/* Default */
	return (FALSE);
}


/*
 * Valid location for stairs
 */
static bool can_place_stairs_wild(int y, int x)
{
	return (cave_plain_bold(y, x) ? TRUE: FALSE);
}


/*
 * Adjust the number of stairs in a level
 */
static int adjust_stairs_number_unchanged(int initial_amount)
{
	return (initial_amount);
}


/*
 * Feature is interesting for the look command
 */
static bool can_target_feature_wild(int f_idx)
{
	/* Only stairs and doors */
	return (feat_ff1_match(f_idx, FF1_STAIRS | FF1_DOOR) ? TRUE: FALSE);
}


/*
 * Monsters in level
 */
static int get_monster_count_wild(void)
{
	int count = 0;
	int y, x;
	u32b ff1 = (FF1_MOVE | FF1_PLACE);

	/* Count the grids that allow monsters in them */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Found one? Increment the count */
			if (cave_ff1_match(y, x, ff1) == ff1) ++count;
		}
	}

	/* Calculate the monster ratio */
	count = ((15 * count) / 1000);

	/* Paranoia */
	return (MAX(MIN_M_ALLOC_LEVEL, count));
}


/*
 * Objects in rooms
 */
static int get_object_count_wild(void)
{
	return (Rand_normal(90, 20));
}


/*
 * Gold in both rooms and corridors
 */
static int get_gold_count_wild(void)
{
	return (Rand_normal(30, 10));
}


/*
 * Objects in both rooms and corridors
 */
static int get_extra_object_count_zero(void)
{
	return (0);
}


/*
 * Dungeon capabilities for wilderness levels
 */
static dungeon_capabilities_type dun_cap_body_wild =
{
	can_place_escorts_wild,
	can_place_player_in_rooms_true,
	can_place_stairs_wild,
	adjust_stairs_number_unchanged,
	can_place_fog_in_rooms_true,
	can_target_feature_wild,
	can_be_transformed_false,
	can_place_non_native_monsters_true,
	allow_level_repopulation_false,
	limited_level_summoning_true,
	allow_monster_multiply_quarter,
	prevent_destruction_true,
	get_monster_count_wild,
	get_object_count_wild,
	get_gold_count_wild,
	get_extra_object_count_zero,
};


/*
 * Monsters in level
 */
static int get_monster_count_labyrinth(void)
{
	int count = 0;
	int y, x;
	u32b ff1 = (FF1_MOVE | FF1_PLACE);

	/* Count the grids that allow monsters in them */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Found one? Increment the count */
			if (cave_ff1_match(y, x, ff1) == ff1) ++count;
		}
	}

	/* Calculate the monster ratio */
	count = ((25 * count) / 1000);

	/* Paranoia */
	return (MAX(MIN_M_ALLOC_LEVEL, count));
}


/*
 * Objects in rooms
 */
static int get_object_count_labyrinth(void)
{
	return (Rand_normal(40, 10));
}


/*
 * Dungeon capabilities for labyrinth levels
 */
static dungeon_capabilities_type dun_cap_body_labyrinth =
{
	can_place_escorts_true,
	can_place_player_in_rooms_true,
	can_place_stairs_default,
	adjust_stairs_number_unchanged,
	can_place_fog_in_rooms_false,
	can_target_feature_default,
	can_be_transformed_false,
	can_place_non_native_monsters_true,
	allow_level_repopulation_false,
	limited_level_summoning_false,
	allow_monster_multiply_quarter,
	prevent_destruction_true,
	get_monster_count_labyrinth,
	get_object_count_labyrinth,
	get_gold_count_wild,
	get_extra_object_count_zero,
};


/*
 * Dungeon capabilities for arena levels
 */
static dungeon_capabilities_type dun_cap_body_arena =
{
	can_place_escorts_true,
	can_place_player_in_rooms_false,
	can_place_stairs_default,
	adjust_stairs_number_unchanged,
	can_place_fog_in_rooms_false,
	can_target_feature_default,
	can_be_transformed_false,
	can_place_non_native_monsters_true,
	allow_level_repopulation_false,
	limited_level_summoning_true,
	allow_monster_multiply_false,
	prevent_destruction_true,
	get_monster_count_labyrinth,
	get_object_count_zero,
	get_gold_count_zero,
	get_extra_object_count_zero,
};


/*
 * Dungeon capabilities for wilderness levels
 */
static dungeon_capabilities_type dun_cap_body_themed_level =
{
	can_place_escorts_true,
	can_place_player_in_rooms_false,
	can_place_stairs_default,
	adjust_stairs_number_default,
	can_place_fog_in_rooms_true,
	can_target_feature_default,
	can_be_transformed_true,
	can_place_non_native_monsters_true,
	allow_level_repopulation_false,
	limited_level_summoning_true,
	allow_monster_multiply_false,
	prevent_destruction_false,
	get_monster_count_labyrinth,
	get_object_count_zero,
	get_gold_count_zero,
	get_extra_object_count_zero,
};


/*
 * Dungeon capabilities for wilderness levels
 */
static dungeon_capabilities_type dun_cap_body_greater_vault =
{
	can_place_escorts_true,
	can_place_player_in_rooms_false,
	can_place_stairs_default,
	adjust_stairs_number_default,
	can_place_fog_in_rooms_false,
	can_target_feature_default,
	can_be_transformed_false,
	can_place_non_native_monsters_true,
	allow_level_repopulation_false,
	limited_level_summoning_false,
	allow_monster_multiply_quarter,
	prevent_destruction_true,
	get_monster_count_labyrinth,
	get_object_count_zero,
	get_gold_count_zero,
	get_extra_object_count_zero,
};


/*
 * Get the proper dungeon capabilities based on the given dungeon type
 */
void set_dungeon_type(u16b dungeon_type)
{
	/* Remember the type */
	p_ptr->dungeon_type = dungeon_type;

	/* Get the capabilities */
	switch (dungeon_type)
	{
		/* Special rules for wilderness levels */
		case DUNGEON_TYPE_WILDERNESS:
		{
			dun_cap = &dun_cap_body_wild;
			break;
		}
		/* Special rules for wilderness levels */
		case DUNGEON_TYPE_LABYRINTH:
		{
			dun_cap = &dun_cap_body_labyrinth;
			break;
		}
		case DUNGEON_TYPE_THEMED_LEVEL:
		{
			dun_cap = &dun_cap_body_themed_level;
			break;
		}
		case DUNGEON_TYPE_ARENA:
		{
			dun_cap = &dun_cap_body_arena;
			break;
		}
		case DUNGEON_TYPE_GREATER_VAULT:
		{
			dun_cap = &dun_cap_body_greater_vault;
			break;
		}
		/* Classic dungeons */
		default:
		{
			if (game_mode == GAME_NPPMORIA) dun_cap = &dun_cap_body_moria;

			else dun_cap = &dun_cap_body_default;
		}
	}
}

