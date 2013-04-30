/* File: generate.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
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
 * and perhaps "MAX_DUN_ROOMS" should be less than 50.
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
 * XXX Note there have been significant changes to how dungeon generation
 * works and many assumptions about the code need to be checked and
 * documented.
 */


/*
 * Dungeon generation values
 */
/* DUN_ROOMS now defined in defines.h */
#define DUN_NATURE_SURFACE	80	/* Chance in 100 of having lakes on surface */
#define DUN_NATURE_DEEP		40	/* Chance in 100 of having lakes at depth */
#define DUN_MAX_LAKES   3       /* Maximum number of lakes/rivers */
#define DUN_FEAT_RNG    2       /* Width of lake */


/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND     30      /* 1 in # chance of random direction */
#define DUN_TUN_ADJ     10      /* 1 in # chance of adjusting direction */
#define DUN_TUN_CAV     3      	/* 1 in # chance of random direction in caves */
#define DUN_TUN_LEN     10      /* 1 in # chance of cave tunnel becoming less random as it grows */
#define DUN_TUN_STYLE   10      /* 1 in # chance of changing style */
#define DUN_TUN_CRYPT   6       /* 1 in # chance of crypt niche having stairs or a monster */
#define DUN_TUN_CON     15      /* Chance of extra tunneling */
#define DUN_TUN_PEN     25      /* Chance of doors at room entrances */
#define DUN_TUN_JCT     90      /* Chance of doors at tunnel junctions */

#define DUN_TRIES		5		/* Number of tries to connect two rooms, before increasing 'scope' for
									connection attempts */


/*
 * Dungeon streamer generation values
 */
#define DUN_STR_WID          2  /* Width of streamers (can be higher) */
#define DUN_STR_CHG          16 /* 1/(4 + chance) of altering direction */
#define DUN_MAX_STREAMER     5  /* Number of streamers */


/*
 * Dungeon treasure allocation values
 */
#define DUN_AMT_ROOM    9       /* Amount of objects for rooms, doubled for strongholds */
#define DUN_AMT_ITEM    3       /* Amount of objects for rooms/corridors, quadrupled for crypts */
#define DUN_AMT_GOLD    3       /* Amount of treasure for rooms/corridors, quadrupled for mines */

/*
 * Hack -- Dungeon allocation "places"
 */
#define ALLOC_SET_CORR	  1       /* Hallway */
#define ALLOC_SET_ROOM	  2       /* Room */
#define ALLOC_SET_BOTH	  3       /* Anywhere */

/*
 * Hack -- Dungeon allocation "types"
 */
#define ALLOC_TYP_RUBBLE	1       /* Rubble */
#define ALLOC_TYP_TRAP		3       /* Trap */
#define ALLOC_TYP_GOLD		4       /* Gold */
#define ALLOC_TYP_OBJECT	5       /* Object */
#define ALLOC_TYP_FEATURE	6		/* Feature eg fountain */
#define ALLOC_TYP_BODY		7		/* Body parts. Placed if most powerful monster is out of depth. */


/*
 * Bounds on some arrays used in the "dun_data" structure.
 * These bounds are checked, though usually this is a formality.
 */
#define CENT_MAX	50	/* Consider DUN_ROOMS */
#define DOOR_MAX	100
#define NEXT_MAX	200
#define WALL_MAX	40
#define TUNN_MAX	300
#define SOLID_MAX	120
#define QUEST_MAX	30
#define STAIR_MAX	30
#define DECOR_MAX	30


/*
 * These flags control the construction of starburst rooms and lakes
 */
#define STAR_BURST_ROOM		0x00000001	/* Mark grids with CAVE_ROOM */
#define STAR_BURST_LIGHT	0x00000002	/* Mark grids with CAVE_GLOW */
#define STAR_BURST_CLOVER	0x00000004	/* Allow cloverleaf rooms */
#define STAR_BURST_RAW_FLOOR	0x00000008	/* Floor overwrites dungeon */
#define STAR_BURST_RAW_EDGE	0x00000010	/* Edge overwrites dungeon */
#define STAR_BURST_MOAT		0x00000020	/* Skip areas defined as a room */
#define STAR_BURST_RANDOM	0x00000040	/* Write terrain 60% of the time, else floor */
#define STAR_BURST_NEED_FLOOR	0x00000080	/* Write terrain only over floors */




/*
 * Maximal number of room types
 */
#define ROOM_MAX	23
#define ROOM_MIN	2


/*
 * Room type information
 */

typedef struct room_data_type room_data_type;

struct room_data_type
{
	/* Allocation information. */
	s16b chance[11];

	/* Minimum level on which room can appear. */
	byte min_level;
	byte max_number;
	byte count_as;
	byte unused;

	/* Level that this appears on */
	u32b theme;
};


/*
 * Structure to hold all "dungeon generation" data
 */

typedef struct dun_data dun_data;

struct dun_data
{
	/* Number of lakes */
	int lake_n;
	coord lake[DUN_MAX_LAKES];

	/* Streamers */
	int stream_n;
	s16b streamer[DUN_MAX_STREAMER];

	/* Array of centers of rooms */
	int cent_n;
	coord cent[CENT_MAX];

	/* Array of partitions of rooms */
	int part_n;
	int part[CENT_MAX];

	/* Array of possible door locations */
	int door_n;
	coord door[DOOR_MAX];

	/* Array of solid walls */
	int solid_n;
	coord solid[SOLID_MAX];
	s16b solid_feat[SOLID_MAX];

	/* Array of decorations next to room entrances and feature types */
	int next_n;
	coord next[NEXT_MAX];
	s16b next_feat[NEXT_MAX];

	/* Array of wall piercing locations */
	int wall_n;
	coord wall[WALL_MAX];

	/* Array of tunnel grids and feature types */
	int tunn_n;
	coord tunn[TUNN_MAX];
	s16b tunn_feat[TUNN_MAX];
	
	/* Array of tunnel decorations */
	int decor_n;
	coord decor[DECOR_MAX];
	int decor_t[DECOR_MAX];

	/* Array of good potential quest grids */
	s16b quest_n;
	coord quest[QUEST_MAX];

	/* Array of good potential stair grids */
	s16b stair_n;
	coord stair[STAIR_MAX];

	/* Number of blocks along each axis */
	int row_rooms;
	int col_rooms;

	/* Hack -- number of entrances to dungeon */
	byte entrance;

	/* Hack -- theme rooms */
	s16b theme_feat;
	
	/* Hack -- flooded rooms */
	s16b flood_feat;
	
	/* Array of which blocks are used */
	bool room_map[MAX_ROOMS_ROW][MAX_ROOMS_COL];
};


/*
 * Dungeon generation data -- see "cave_gen()"
 */
static dun_data *dun;


/*
 * Table of values that control how many times each type of room will,
 * on average, appear on 100 levels at various depths.  Each type of room
 * has its own row, and each column corresponds to dungeon levels 0, 10,
 * 20, and so on.  The final value is the minimum depth the room can appear
 * at.  -LM-
 *
 * Level 101 and below use the values for level 100.
 *
 * Rooms with lots of monsters or loot may not be generated if the object or
 * monster lists are already nearly full.  Rooms will not appear above their
 * minimum depth.  No type of room (other than type 1) can appear more than
 * DUN_ROOMS/2 times in any level.  Tiny levels will not have space for all
 * the rooms you ask for.
 *
 * The entries for room type 1 are blank because these rooms are built once
 * all other rooms are finished -- until the level fills up, or the room
 * count reaches the limit (DUN_ROOMS).
 */
static room_data_type room_data[ROOM_MAX] =
{
   /* Depth:         0   10   20   30   40   50   60   70   80   90  100  min max_num count, theme*/

   /* Nothing */  {{100,100, 100, 100, 100, 100, 100, 100, 100, 100, 100},  0,DUN_ROOMS * 3,	1, 0, LF1_MINE},
   /* 'Empty' */  {{100,100, 100, 100, 100, 100, 100, 100, 100, 100, 100},  0,DUN_ROOMS * 3,	1, 0, LF1_THEME & ~(LF1_STRONGHOLD | LF1_CAVE | LF1_WILD | LF1_LABYRINTH)},
   /* Walls   */  {{180,240, 300, 300, 300, 300, 300, 300, 300, 300, 300},  1,DUN_ROOMS,	1, 0, LF1_THEME & ~(LF1_STRONGHOLD | LF1_CAVE | LF1_DESTROYED | LF1_WILD | LF1_LABYRINTH)},
   /* Centre */   {{60, 100, 120, 140, 160, 180, 200, 200, 200, 200, 200},  1,DUN_ROOMS,	1, 0, LF1_THEME & ~(LF1_STRONGHOLD | LF1_CAVE | LF1_DESTROYED | LF1_WILD | LF1_LABYRINTH)},
   /* Lrg wall */ {{ 0,  30,  60,  80,  90,  95, 100, 100, 100, 100, 100},  3,DUN_ROOMS,	2, 0, LF1_STRONGHOLD | LF1_DUNGEON | LF1_CRYPT},
   /* Lrg cent */ {{ 0,  30,  60,  80,  90,  95, 100, 100, 100, 100, 100},  3,DUN_ROOMS,	2, 0, LF1_STRONGHOLD | LF1_DUNGEON | LF1_SEWER},
   /* Xlg cent */ {{ 0,   0,   0,   0,   0,   0,   4,   4,   4,   4,   4}, 11,DUN_ROOMS/2,	3, 0, LF1_STRONGHOLD},
   /* Chambers */ {{ 0,   2,   6,  12,  15,  18,  19,  20,  20,  20,  20},  7,	6,		3, 0, LF1_CHAMBERS},
   /* I. Room */  {{30,  60,  70,  80,  80,  75,  70,  67,  65,  62,  60},  0,  4,		1, 0, LF1_DUNGEON},
   /* L. Vault */ {{ 0,   1,   4,   9,  16,  27,  40,  55,  70,  80,  90},  7,	4,		2, 0, LF1_VAULT | LF1_CRYPT | LF1_DUNGEON},
   /* G. Vault */ {{ 0,   0,   1,   2,   3,   4,   6,   7,   8,  10,  12}, 20,	1,		3, 0, LF1_VAULT | LF1_STRONGHOLD},
   /* Starbrst */ {{ 0,   2,   6,  12,  15,  18,  19,  20,  20,  20,  20},  7,DUN_ROOMS,	2, 0, LF1_MINE | LF1_DUNGEON | LF1_CAVE | LF1_LAIR | LF1_SEWER},
   /* Hg star */  {{ 0,   0,   0,   0,   4,   4,   4,   4,   4,   4,   4}, 41,	1,		3, 0, LF1_MINE | LF1_CAVE | LF1_LAIR | LF1_SEWER},
   /* Fractal */  {{ 0,  30,  60,  80,  90,  95, 100, 100, 100, 100, 100},  3,DUN_ROOMS * 2/3,	2, 0, LF1_MINE | LF1_CAVE},
   /* Lrg fra */  {{ 0,   2,   6,  12,  15,  18,  19,  20,  20,  20,  20},  7,DUN_ROOMS / 2,	3, 0, LF1_MINE | LF1_DUNGEON | LF1_CAVE},
   /* Huge fra */ {{ 0,   0,   0,   0,   0,   4,   4,   4,   4,   4,   4}, 11,	1,		4, 0, LF1_CAVE},
   /* Lair */     {{ 0,   0,   0,   0,   4,   4,   4,   4,   4,   4,   4}, 41,	1,		1, 0, LF1_LAIR},
   /* Maze */     {{ 0,  15,  30,  40,  45,  45,  50,  50,  50,  50,  50}, 41,DUN_ROOMS/3,		1, 0, LF1_THEME & ~(LF1_DESTROYED | LF1_WILD)},
   /* Lrg maze */ {{ 0,   2,   6,  12,  15,  18,  19,  20,  20,  20,  20}, 41,	3,		2, 0, LF1_LABYRINTH | LF1_STRONGHOLD | LF1_CAVE | LF1_CRYPT | LF1_SEWER},
   /* Huge maze */{{ 0,   0,   0,   4,   6,   6,   8,   8,  10,  10,  10}, 41,	1,		3, 0, LF1_LABYRINTH},
   /* Lake */     {{ 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0}, 41,	1,		2, 0, LF1_WILD},
   /* Huge lake */{{ 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0}, 41,	1,		3, 0, LF1_WILD},
   /* Tower */    {{ 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0}, 41,	1,		3, 0, LF1_TOWER}
};



/* Build rooms in descending order of difficulty of placing e.g. size, frequency. */
static byte room_build_order[ROOM_MAX] = {ROOM_LAIR, ROOM_GREATER_VAULT, ROOM_HUGE_MAZE, ROOM_CHAMBERS,
						ROOM_HUGE_FRACTAL, ROOM_HUGE_STAR_BURST, ROOM_HUGE_CENTRE, ROOM_LARGE_MAZE, ROOM_LARGE_FRACTAL, 
						ROOM_LESSER_VAULT, ROOM_INTERESTING, ROOM_STAR_BURST, ROOM_MAZE, ROOM_FRACTAL, ROOM_LARGE_CENTRE,
						ROOM_LARGE_WALLS, ROOM_NORMAL_CENTRE, ROOM_NORMAL_WALLS, ROOM_NORMAL, 0};


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

	if (f_info[cave_feat[y+1][x]].flags1 & (FF1_WALL)) k++;
	if (f_info[cave_feat[y+1][x]].flags1 & (FF1_WALL)) k++;
	if (f_info[cave_feat[y+1][x]].flags1 & (FF1_WALL)) k++;
	if (f_info[cave_feat[y+1][x]].flags1 & (FF1_WALL)) k++;

	return (k);
}



/*
 * Replace terrain with a mix of terrain types. Returns true iff
 * this is a mixed terrain type.
 */
static bool variable_terrain(int *feat, int oldfeat)
{
	int k;
	bool varies = FALSE;

	/* Hack -- ensure variable terrain creates continuous space */
	if (((f_info[oldfeat].flags1 & (FF1_MOVE)) == 0) &&
		((f_info[oldfeat].flags3 & (FF3_EASY_CLIMB)) == 0))
	{
		if (f_info[oldfeat].flags1 & (FF1_TUNNEL)) oldfeat = feat_state(oldfeat, FS_TUNNEL);
		else if (f_info[oldfeat].flags2 & (FF2_BRIDGE)) oldfeat = feat_state(oldfeat, FS_BRIDGE);
		else oldfeat = FEAT_FLOOR;
	}

	/* Hack -- place trees to support need tree terrain */
	if (f_info[*feat].flags3 & (FF3_NEED_TREE))
	{
		k = randint(100);

		if (k<=15) *feat = FEAT_TREE;
		else if (k <= 25) *feat = feat_state(*feat, FS_NEED_TREE);
		
		varies = TRUE;
	}
	/* Hack -- place trees infrequently */
	else if (f_info[*feat].flags3 & (FF3_TREE))
	{
		k = randint(100);

		if (k<=85) *feat = oldfeat;
		
		varies = TRUE;
	}
	/* Hack -- place living terrain infrequently */
	else if (f_info[*feat].flags3 & (FF3_LIVING))
	{
		k = randint(100);
		if (k<=30) *feat = oldfeat;
		if (k<=90) *feat = FEAT_GRASS;

		varies = TRUE;
	}

	/* Hack -- place 'easy climb' terrain infrequently */
	else if (f_info[*feat].flags3 & (FF3_EASY_CLIMB))
	{
		k = randint(100);

		if ((k <= 20) && (f_info[*feat].flags2 & (FF2_CAN_DIG))) *feat = FEAT_RUBBLE;
		else if (k<=90) *feat = oldfeat;
		varies = TRUE;
	}
	
	/* Hack -- place 'adjacent terrain' infrequently */
	if (f_info[*feat].flags3 & (FF3_ADJACENT))
	{
		k = randint(100);

		if ((k <= 90) && ((f_info[f_info[*feat].edge].flags1 & (FF1_MOVE)) != 0)) *feat = f_info[*feat].edge;
		else if (k <= 90) *feat = oldfeat;
		else if (k <= 95) *feat = feat_state(*feat, FS_ADJACENT);
		varies = TRUE;
	}

	/* Hack -- place 'spreading terrain' infrequently */
	if (f_info[*feat].flags3 & (FF3_SPREAD))
	{
		k = randint(100);

		if ((k <= 90) && ((f_info[f_info[*feat].edge].flags1 & (FF1_MOVE)) != 0)) *feat = f_info[*feat].edge;
		else if ((k <= 90) && ((f_info[*feat].flags2 & (FF2_LAVA)) == 0)) *feat = oldfeat;
		else if (k <= 95) *feat = feat_state(*feat, FS_SPREAD);
		varies = TRUE;
	}
	
	/* Hack -- place 'erupting terrain' infrequently */
	if (f_info[*feat].flags3 & (FF3_ERUPT))
	{
		k = randint(100);

		if (k<=90) *feat = feat_state(*feat, FS_ERUPT);
		varies = TRUE;
	}
	
	/* Hack -- transform most 'timed' terrain */
	if (f_info[*feat].flags3 & (FF3_TIMED))
	{
		k = randint(100);

		if (k<=25) *feat = feat_state(*feat, FS_TIMED);
		varies = TRUE;
	}
	
	/* Some 'special' terrain types */
	if (*feat == oldfeat) switch (*feat)
	{
		/* Hack -- prevent random stone bridges in chasm */
		case FEAT_CHASM:
		case FEAT_CHASM_E:
			return (FALSE);
			
		default:
		{
			
			if ((f_info[*feat].edge) && ((f_info[f_info[*feat].edge].flags1 & (FF1_MOVE)) != 0))
			{
				k = randint(100);

				if (k<=20) *feat = f_info[*feat].edge;
				varies = TRUE;
			}
		}
	}

	return (varies);
}




/*
 * Hack -- mimic'ed feature for "room_info_feat()"
 */
static s16b room_info_feat_mimic;

/*
 *
 */
static bool room_info_feat(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	if (f_ptr->mimic == room_info_feat_mimic) return(TRUE);

	return(FALSE);
}


/*
 * Gets a feature that may match one of the 'mimic'ed features instead
 */
static s16b pick_room_feat(s16b feat)
{
	int newfeat;
	
	/* Paranoia */
	if (feat != f_info[feat].mimic) return (feat);
	
	/* Set feature hook */
	room_info_feat_mimic = feat;

	get_feat_num_hook = room_info_feat;

	/* Prepare allocation table */
	get_feat_num_prep();
	
	newfeat = get_feat_num(object_level);
	
	if (newfeat) feat = newfeat;
	
	get_feat_num_hook = NULL;

	/* Prepare allocation table */
	get_feat_num_prep();
	
	return(feat);
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
static void build_terrain(int y, int x, int feat)
{
	int oldfeat, newfeat;
	feature_type *f_ptr;
	feature_type *f2_ptr;

	/* Get the feature */
	oldfeat = cave_feat[y][x];
	f_ptr = &f_info[oldfeat];

	/* Set the new feature */
	newfeat = oldfeat;
	f2_ptr = &f_info[feat];

	/* Special cases first */
	switch (feat)
	{
		case FEAT_QSAND_H:
		case FEAT_SAND_H:
		{
			if (oldfeat == FEAT_WATER) newfeat = FEAT_QSAND_H;
			if (oldfeat == FEAT_WATER_H) newfeat = FEAT_QSAND_H;
			break;
		}
		case FEAT_ICE_WATER_K:
		case FEAT_ICE_WATER_H:
		{
			if (f_ptr->flags2 & (FF2_LAVA)) newfeat = FEAT_BWATER;
			else if (f_ptr->flags2 & (FF2_ICE | FF2_WATER)) newfeat = feat;
			break;
		}	
		case FEAT_BMUD:
		case FEAT_BWATER:
		{
			if (f_ptr->flags2 & (FF2_WATER)) newfeat = feat;
			if (f_ptr->flags2 & (FF2_HIDE_DIG)) newfeat = feat;
			if (oldfeat == FEAT_ICE) newfeat = FEAT_ICE_GEOTH;
			if (oldfeat == FEAT_ICE_C) newfeat = FEAT_ICE_GEOTH;
			if (oldfeat == FEAT_FLOOR_ICE) newfeat = FEAT_GEOTH;		    
			if (oldfeat == FEAT_LAVA) newfeat = FEAT_GEOTH;
			if (oldfeat == FEAT_LAVA_H) newfeat = feat;
			if (oldfeat == FEAT_LAVA_K) newfeat = feat;
			if (oldfeat == FEAT_FLOOR_EARTH) newfeat = FEAT_BMUD;
			if (oldfeat == FEAT_FLOOR_EARTH_T) newfeat = FEAT_BMUD;
			if (oldfeat == FEAT_SAND) newfeat = feat;
			if (oldfeat == FEAT_QSAND_H) newfeat = feat;
			if (oldfeat == FEAT_MUD_H) newfeat = FEAT_BMUD;
			if (oldfeat == FEAT_MUD_HT) newfeat = FEAT_BMUD;
			if (oldfeat == FEAT_MUD_K) newfeat = FEAT_BMUD;
			if (oldfeat == FEAT_EARTH) newfeat = FEAT_BMUD;
			break;
		}
		case FEAT_GEOTH:
		{
			if (oldfeat == FEAT_ICE) newfeat = FEAT_ICE_GEOTH;
			if (oldfeat == FEAT_ICE) newfeat = FEAT_ICE_GEOTH;
			if (oldfeat == FEAT_MUD_H) newfeat = FEAT_BMUD;
			if (oldfeat == FEAT_MUD_HT) newfeat = FEAT_BMUD;
			if (oldfeat == FEAT_MUD_K) newfeat = FEAT_BMUD;
			break;
		}
	}

	/* Have we handled a special case? */
	if (newfeat != oldfeat)
	{
		/* Nothing */
	}
	else if (!oldfeat)
	{
		newfeat = feat;
	}
	else if ((f_ptr->flags1 & (FF1_WALL))
		&& !(f_ptr->flags2 & (FF2_WATER | FF2_LAVA | FF2_ACID | FF2_OIL | FF2_ICE | FF2_CHASM))
		&& !(f_ptr->flags3 & (FF3_TREE)))
	{
		newfeat = feat;
	}
	else if (f2_ptr->flags2 & (FF2_BRIDGED))
	{
		newfeat = feat_state(oldfeat, FS_BRIDGE);
	}
	else if ((f_ptr->flags2 & (FF2_CHASM)) || (f2_ptr->flags2 & (FF2_CHASM)))
	{
		newfeat = feat_state(oldfeat,FS_CHASM);
	}
	else if (f_ptr->flags1 & (FF1_FLOOR))
	{
		newfeat = feat_state(feat,FS_TUNNEL);
	}
	else if (f2_ptr->flags1 & (FF1_FLOOR))
	{
		newfeat = feat_state(oldfeat,FS_TUNNEL);
	}
	else if (f_ptr->flags3 & (FF3_GROUND))
	{
		newfeat = feat;
	}
	else if (f2_ptr->flags3 & (FF3_GROUND))
	{
		newfeat = feat;
	}
	else if (f_ptr->flags2 & (FF2_LAVA))
	{
		if ((f2_ptr->flags2 & (FF2_ICE)) && (f2_ptr->flags1 & (FF1_WALL)) &&
                        (f2_ptr->flags2 & (FF2_CAN_OOZE)))
		{
			newfeat = FEAT_ICE_GEOTH_HC;
		}
		else if ((f2_ptr->flags2 & (FF2_ICE)) && (f2_ptr->flags1 & (FF1_WALL)))
		{
			newfeat = FEAT_ICE_GEOTH;
		}
		else if ((f2_ptr->flags2 & (FF2_HIDE_DIG)) && (f2_ptr->flags2 & (FF2_DEEP | FF2_FILLED)))
		{
			newfeat = FEAT_BMUD;
		}
		else if ((f2_ptr->flags2 & (FF2_WATER)) && (f2_ptr->flags2 & (FF2_DEEP | FF2_FILLED)))
		{
			newfeat = FEAT_BWATER;
		}
		else if (f2_ptr->flags2 & (FF2_WATER | FF2_ACID | FF2_OIL | FF2_ICE | FF2_CHASM))
		{
			newfeat = FEAT_GEOTH_LAVA;
		}
	}
	else if (f2_ptr->flags2 & (FF2_LAVA))
	{
		if ((f_ptr->flags2 & (FF2_ICE)) && (f_ptr->flags1 & (FF1_WALL)) &&
                        (f_ptr->flags2 & (FF2_CAN_OOZE)))
		{
			newfeat = FEAT_ICE_GEOTH_HC;
		}
		else if ((f_ptr->flags2 & (FF2_ICE)) && (f_ptr->flags1 & (FF1_WALL)))
		{
			newfeat = FEAT_ICE_GEOTH;
		}
		else if ((f_ptr->flags2 & (FF2_HIDE_DIG)) && (f_ptr->flags2 & (FF2_DEEP | FF2_FILLED)))
		{
			newfeat = FEAT_BMUD;
		}
		else if ((f_ptr->flags2 & (FF2_WATER)) && (f_ptr->flags2 & (FF2_DEEP | FF2_FILLED)))
		{
			newfeat = FEAT_BWATER;
		}
		else if (f_ptr->flags2 & (FF2_WATER | FF2_ACID | FF2_OIL | FF2_ICE | FF2_CHASM))
		{
			newfeat = FEAT_GEOTH_LAVA;
		}
		else
		{
			newfeat = feat;
		}
	}
	else if (f_ptr->flags2 & (FF2_ICE))
	{
		/* Handle case of ice wall over underwater */
                if ((f_ptr->flags1 & (FF1_WALL)) && (f_ptr->flags2 & (FF2_CAN_OOZE)))
		{
			if ((f2_ptr->flags2 & (FF2_WATER)) && (f2_ptr->flags2 & (FF2_FILLED))
			 && (f2_ptr->flags1 & (FF1_SECRET)))
			{
				newfeat = FEAT_UNDER_ICE_HC;
			}
			else if ((f2_ptr->flags2 & (FF2_WATER)) && (f2_ptr->flags2 & (FF2_FILLED)))
			{
				newfeat = FEAT_UNDER_ICE_KC;
			}
		}
		else if (f_ptr->flags1 & (FF1_WALL))
		{
			if ((f2_ptr->flags2 & (FF2_WATER)) && (f2_ptr->flags2 & (FF2_FILLED)))
			{
				newfeat = FEAT_UNDER_ICE;
			}
		}
		else if (f2_ptr->flags2 & (FF2_HURT_COLD)) newfeat = feat_state(feat,FS_HURT_COLD);
	}
	else if (f2_ptr->flags2 & (FF2_ICE))
	{
		/* Handle case of ice wall over underwater */
                if ((f2_ptr->flags1 & (FF1_WALL)) && (f2_ptr->flags2 & (FF2_CAN_OOZE)))
		{
			if ((f_ptr->flags2 & (FF2_WATER)) && (f_ptr->flags2 & (FF2_FILLED))
			 && (f_ptr->flags1 & (FF1_SECRET)))
			{
				newfeat = FEAT_UNDER_ICE_HC;
			}
			else if ((f_ptr->flags2 & (FF2_WATER)) && (f_ptr->flags2 & (FF2_FILLED)))
			{
				newfeat = FEAT_UNDER_ICE_KC;
			}
		}
		else if (f2_ptr->flags1 & (FF1_WALL))
		{
			if ((f_ptr->flags2 & (FF2_WATER)) && (f_ptr->flags2 & (FF2_FILLED)))
			{
				newfeat = FEAT_UNDER_ICE;
			}
		}
		else if (f_ptr->flags2 & (FF2_HURT_COLD)) newfeat = feat_state(oldfeat,FS_HURT_COLD);
		else if (f_ptr->flags2 & (FF2_HIDE_DIG)) newfeat = feat;
	}
	else if ((f_ptr->flags2 & (FF2_WATER)) || (f2_ptr->flags2 & (FF2_WATER)))
	{
		/* Hack -- we try and match water properties */
		u32b mask1 = (FF1_SECRET | FF1_LESS);
		u32b mask2 = (FF2_WATER | FF2_SHALLOW | FF2_FILLED | FF2_DEEP | FF2_ICE | FF2_LAVA | FF2_CHASM | FF2_HIDE_SWIM);
		u32b match1 = 0x0L;
		u32b match2 = FF2_WATER;

		int k_idx = f_info[oldfeat].k_idx;

		int i;

		/* Hack -- get most poisonous object */
		if (f_info[feat].k_idx > k_idx) k_idx = f_info[feat].k_idx;

		/* Hack -- get flags */
		if ((f_ptr->flags2 & (FF2_SHALLOW)) || (f2_ptr->flags2 & (FF2_SHALLOW)))
		{
			match2 |= FF2_SHALLOW;
		}
		else if ((f_ptr->flags2 & (FF2_FILLED)) || (f2_ptr->flags2 & (FF2_FILLED)))
		{
			match2 |= FF2_FILLED;
			match1 |= ((f_ptr->flags1 & (FF1_SECRET)) || (f2_ptr->flags1 & (FF1_SECRET)));
		}
		else
		{
			match2 |= FF2_DEEP;
			match2 |= ((f_ptr->flags2 & (FF2_HIDE_SWIM)) || (f2_ptr->flags2 & (FF2_HIDE_SWIM)));
			match1 |= ((f_ptr->flags1 & (FF1_SECRET)) || (f2_ptr->flags1 & (FF1_SECRET)));
			match1 |= ((f_ptr->flags1 & (FF1_LESS)) || (f2_ptr->flags1 & (FF1_LESS)));
		}

		for (i = 0;i < z_info->f_max;i++)
		{
			/* Hack -- force match */
			if ((f_info[i].flags1 & (mask1)) != match1) continue;
			if ((f_info[i].flags2 & (mask2)) != match2) continue;

			if (f_info[i].k_idx != k_idx) continue;

			newfeat = i;
		}
	}
	else if ((f_ptr->flags2 & (FF2_CAN_DIG)) || (f2_ptr->flags2 & (FF2_CAN_DIG)))
	{
		newfeat = feat;
	}

	/* Vary the terrain */
	if (variable_terrain(&feat, oldfeat))
	{
		newfeat = feat;
	}
	/* Pick features if needed */
	else if (f_info[newfeat].flags1 & (FF1_HAS_ITEM | FF1_HAS_GOLD))
	{
		newfeat = pick_room_feat(feat);
	}

	/* Hack -- no change */
	if (newfeat == oldfeat) return;

	/* Set the feature if we have a change */
	cave_set_feat(y,x,newfeat);

	/* Change reference */
        f2_ptr = &f_info[newfeat];

	/*
	 * Handle creation of big trees.
         *
         * Note hack to minimise number of calls to rand_int.
	 */
	if (f_info[newfeat].flags3 & (FF3_TREE))
	{
            int k = 0;
		int i;

            k = rand_int(2<<26);

		/* Place branches over trunk */
            if (k & (0xFF000000)) cave_alter_feat(y,x,FS_TREE);

		for (i = 0; i < 8; i++)
		{
			int yy,xx;

			yy = y + ddy_ddd[i];
			xx = x + ddx_ddd[i];
	
			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Ignore if not placing a tree */
			/* Hack -- we make it 150% as likely to place branches on non-diagonal locations */
                  if (!(k & (2 << i)) && !(k & (2 << (i+8) )) && !((i<4) && (k & (2 << (i+16)))) ) continue;

			/* Place branches */
			cave_alter_feat(yy,xx,FS_TREE);
		}
	}
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
 * Generate helper -- fill a rectangle with a feature. Place pillars if spacing set.
 */
static void generate_fill_pillars(int y1, int x1, int y2, int x2, int feat, int spacing, int scale)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* Check spacing */
			if ((spacing > 1) && (y % spacing < scale) && (x % spacing < scale) &&
					(x != x1) && (x != x2) && (y != y1) && (y != y2))
			{
				cave_set_feat(y, x, FEAT_WALL_INNER);
			}
			else
			{
				build_terrain(y, x, feat);
			}
		}
	}
}


/*
 * Generate helper -- draw a rectangle with a feature
 */
static void generate_rect(int y1, int x1, int y2, int x2, int feat)
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
 * Record good location for quest objects
 */
static bool add_quest(int y, int x)
{
	/* Needs to be in bounds */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Record location */
	if (dun->quest_n < QUEST_MAX)
	{
		dun->quest[dun->quest_n].y = y;
		dun->quest[dun->quest_n].x = x;
		dun->quest_n++;

		return (TRUE);
	}
	
	return (FALSE);
}



/* Convert a maze coordinate into a dungeon coordinate */
#define YPOS(y, y1)		((y1) + (y * (width_path + width_wall)) + width_outer)
#define XPOS(x, x1)		((x1) + (x * (width_path + width_wall)) + width_outer)


#define MAZE_WALL		0x00000001L	/* Surround maze with width 1 wall */
#define MAZE_SAVE		0x00000002L	/* Save contents overwritten by maze and try to place them again afterwards */
#define MAZE_ROOM		0x00000004L	/* Set the cave_room flag */
#define MAZE_LITE		0x00000008L	/* Set the cave_glow flag */
#define MAZE_OUTER_N	0x00000010L	/* Make north edge of maze have 'outer' walls (requires MAZE_WALL set) */
#define MAZE_OUTER_S	0x00000020L	/* Make south edge of maze have 'outer' walls (requires MAZE_WALL set) */
#define MAZE_OUTER_W	0x00000040L	/* Make west edge of maze have 'outer' walls (requires MAZE_WALL set) */
#define MAZE_OUTER_E	0x00000080L	/* Make east edge of maze have 'outer' walls (requires MAZE_WALL set) */
#define MAZE_EXIT_N		0x00000100L	/* Make exit on north edge of maze */
#define MAZE_EXIT_S		0x00000200L	/* Make exit on south edge of maze */
#define MAZE_EXIT_W		0x00000400L	/* Make exit on west edge of maze */
#define MAZE_EXIT_E		0x00000800L	/* Make exit on east edge of maze */
#define MAZE_DEAD		0x00001000L	/* Fill some dead ends with walls */
#define MAZE_POOL		0x00002000L	/* Fill some dead ends with pools */
#define MAZE_DOOR		0x00004000L	/* Hide some dead ends with secret doors */
#define MAZE_FILL		0x00008000L	/* Fill some dead ends with useful goodies */
#define MAZE_CRYPT		0x00010000L	/* Paths in maze have ragged edges like crypt corridors (width_path must be greater than 1)*/
#define MAZE_CAVE		0x00020000L	/* Paths in maze wander like cave corridors (width_path must be 2) */
#define MAZE_FLOOD		0x00040000L	/* Fill in some dead ends with flooded terrain - open these dead ends to surrounds */
#define MAZE_LOOP		0x00080000L	/* Open some dead ends */



/*
 * Build an acyclic maze inside a given rectangle.  - Eric Bock -
 * Construct the maze from a given pair of features.
 *
 * Note that the edge lengths should be odd for standard width_wall = 1 and width_path = 1 mazes.
 * 
 * Width_wall indicates the width of the maze 'walls' and should be >= 1.
 * Width_path indicates the width of the maze 'paths' and should be >= 1.
 * 
 * Note that the outer walls of the maze are always width 1, which makes the maze math a little
 * more complicated, but allows tunnels to correctly connect to the edge of the maze.
 * 
 * Important note particular for developers: width and height is given by x2 - x1 + 1 and
 * y2 - y1 + 1. So this means an odd width (for instance) 
 * 
 * Filled indicates how much of the maze dead ends are 're-filled' with walls.
 * Secrets indicates how much of the maze dead ends are hidden by secret doors.
 * Quests indicates how much of the maze dead ends are changed to quest locations.
 * 
 * If room is true, the maze will refill edges locations preferentially, and turn those edge
 * locations into non-room locations. The outer edges of the maze will also be converted into
 * solid and outer walls respectively if the feat_wall feature is a WALL. This allows the
 * construction of maze rooms required for labrynth levels.
 */
static bool draw_maze(int y1, int x1, int y2, int x2, s16b feat_wall,
    s16b feat_path, int width_wall, int width_path, s16b feat_pool, u32b flag)
{
	int i, j;
	int ydim, xdim;
	int grids;

	int y, x;
	int ty, tx;
	int dy, dx;
	int yi, xi;
	
	int width_outer = flag & (MAZE_WALL) ? 1 : 0;

	byte dir[4];
	byte dirs;
	
	int count = 0;
	
	int offset = ((flag & (MAZE_CRYPT | MAZE_CAVE)) != 0) && (rand_int(100) < 50) ? 1 : 0;
	
	/* For some reason, allocating this on the heap causes problems. Thus we allocate on the stack
	 * and worry about debugging memory errors another day. */
	s16b saved[DUNGEON_HGT * DUNGEON_WID];

	int solid = ((f_info[feat_wall].flags1 & (FF1_OUTER)) != 0) ? feat_state(feat_wall, FS_SOLID) : 0;	
	int inner = ((f_info[feat_wall].flags1 & (FF1_OUTER)) != 0) ? feat_state(feat_wall, FS_INNER) : feat_wall;
	
	/* Paranoia */
	if ((!feat_wall) || (!feat_path) || (feat_wall == feat_path) || (width_wall < 1) || (width_path < 1)) return (FALSE);

	/* Paranoia */
	if (!in_bounds_fully(y1, x1) || !in_bounds_fully(y2, x2)) return (FALSE);

	/* Paranoia */
	if ((y2 - y1 <= 0) || (x2 - x1 <= 0)) return (FALSE);
	
#if 0

	/* Extra crispy paranoia */
	if ((y2 - y1) % (width_wall + width_path) != (width_wall + 2) % (width_wall + width_path)) return;
	if ((x2 - x1) % (width_wall + width_path) != (width_wall + 2) % (width_wall + width_path)) return;
#endif
	
	if (cheat_room) msg_print(format("Drawing maze of %s (%d) filled with %s (%d).", f_name + f_info[feat_wall].name, width_wall,
			f_name + f_info[feat_path].name, width_path));	
	
	/* Save the existing terrain to overwrite the maze later */
	if ((flag & (MAZE_SAVE)) != 0)
	{
		/*saved = C_ZNEW((3 + y2 - y1) * (3 + x2 - x1), s16b);*/
	
		/* Save grids */
		for (y = 0; y <= y2 - y1; y++)
		{
			for (x = 0; x <= x2 - x1; x++)
			{
				saved[y * (2 + y2 - y1) + x] = cave_feat[y + y1][x + x1];
			}
		}
	}

	/* Start with a solid rectangle of the "inner" wall feat */
	generate_fill(y1, x1, y2, x2, inner);	
	
	/* Draw room if requested */
	if (flag & (MAZE_ROOM)) generate_room(y1, x1, y2, x2, ((flag & (MAZE_LITE)) != 0));
	
	/* If maze has a wall and feat_wall is an outer wall, surround by 'outer' and 'solid' sections of wall to allow corridor connections */
	if (((flag & (MAZE_WALL)) != 0) && (solid))
	{
		if (flag & (MAZE_OUTER_W | MAZE_OUTER_E))
		{
			for (y = y1; y <= y2; y++)
			{
				if (flag & (MAZE_OUTER_W)) cave_set_feat(y, x1, (y - y1 - 1) % (width_wall + width_path) < width_path ? feat_wall : solid);
				if (flag & (MAZE_OUTER_E)) cave_set_feat(y, x2, (y - y1 - 1) % (width_wall + width_path) < width_path ? feat_wall : solid);
			}
		}
		
		if (flag & (MAZE_OUTER_N | MAZE_OUTER_S))
		{
			for (x = x1; x <= x2; x++)
			{
				if (flag & (MAZE_OUTER_N)) cave_set_feat(y1, x, (x - x1 - 1) % (width_wall + width_path) < width_path ? feat_wall : solid);
				if (flag & (MAZE_OUTER_S)) cave_set_feat(y2, x, (x - x1 - 1) % (width_wall + width_path) < width_path ? feat_wall : solid);
			}
		}
	}
	
	/* Calculate dimensions.*/
	
	/* Note we correct by a factor of wall width, to allow the division to work
	 * minus two (for the requirement of two outer width 1 walls)
	 * plus 1 because y1 = 10, y2 = 10 implies a width 1 room. */
	
	ydim = ((y2 - y1) + width_wall - (2 * width_outer) + 1) / (width_wall + width_path);
	xdim = ((x2 - x1) + width_wall - (2 * width_outer) + 1) / (width_wall + width_path);

	/* Number of unexamined grids */
	grids = ydim * xdim - 1;

	/* Set the initial position */
	y = rand_int(ydim);
	x = rand_int(xdim);

	/* Place floor here */
	for (yi = YPOS(y, y1); yi < YPOS(y, y1) + width_path; yi++)
	{
		for (xi = XPOS(x, x1); xi < XPOS(x, x1) + width_path; xi++)
		{
			/* XXX We have to use the auxiliary cave_set_feat function.
			 * This is because trees and chasms would otherwise create infinite loops, as cave_set_feat(yi, xi, feat)
			 * does not guarantee that cave_feat[yi][xi] = feat_path. */
			cave_set_feat_aux(yi, xi, feat_path);
		}
	}

	/* Now build the maze */
	while ((grids) && (++count < 10000))
	{
		/* Only use maze grids */
		if ((cave_feat[YPOS(y, y1)][XPOS(x, x1)] == feat_path) || ((width_path > 1) &&
				(cave_feat[YPOS(y, y1)][XPOS(x, x1) + 1] == feat_path)))
		{
			/* Pick a target */
			ty = rand_int(ydim);
			tx = rand_int(xdim);

			while (TRUE)
			{
				dirs = 0;
				dy = 0;
				dx = 0;

				/* Calculate the dungeon position */
				j = YPOS(y, y1);
				i = XPOS(x, x1);

				/** Enumerate possible directions **/

				/* Up */
				if (y && (cave_feat[j - width_wall - width_path][i] != feat_path) && ((width_path == 1) ||
					(cave_feat[j - width_wall - width_path][i + 1] != feat_path)))  dir[dirs++] = 1;

				/* Down */
				if ((y < ydim - 1) && (cave_feat[j + width_wall + width_path][i] != feat_path) && ((width_path == 1) ||
					(cave_feat[j + width_wall + width_path][i + 1] != feat_path))) dir[dirs++] = 2;

				/* Left */
				if (x && (cave_feat[j][i - width_wall - width_path] != feat_path) && ((width_path == 1) ||
					(cave_feat[j + 1][i - width_wall - width_path] != feat_path))) dir[dirs++] = 3;

				/* Right */
				if ((x < xdim - 1) && (cave_feat[j][i + width_wall + width_path] != feat_path) && ((width_path == 1) ||
					(cave_feat[j + 1][i + width_wall + width_path] != feat_path))) dir[dirs++] = 4;

				/* Dead end; go to the next valid grid */
				if (!dirs) break;

				/* Pick a random direction */
				switch (dir[rand_int(dirs)])
				{
					/* Move up */
					case 1:  dy = -1;  break;

					/* Move down */
					case 2:  dy =  1;  break;

					/* Move left */
					case 3:  dx = -1;  break;

					/* Move right */
					case 4:  dx =  1;  break;
				}

				/* Place floors */
				for (yi = j - (dy < 0 ? width_wall + width_path : 0); yi < j + (dy < 0 ? 0 : width_path) + (dy > 0 ? width_wall + width_path : 0); yi++)
				{
					for (xi = i - (dx < 0 ? width_wall + width_path : 0); xi < i + (dx < 0 ? 0 : width_path) + (dx > 0 ? width_wall + width_path : 0); xi++)
					{
						/* Leave pillars in centre of width 3 corridors, except crypts and caves */
						if ((width_path == 3) && ((flag & (MAZE_CRYPT | MAZE_CAVE)) == 0))
						{
							/* Pillar in current grid */
							if ((yi == j + 1) && (xi == i + 1)) continue;
							
							/* Pillar in destination grid (note width_path replaced by 3) */
							if ((yi == j + 1 + (dy * (width_wall + 3))) && (xi == i + 1 + (dx * (width_wall + 3)))) continue;
							
							/* Pillar in-between if width_wall is 3 */
							if ((width_wall == 3) && ((yi == j + 1 + (dy * 3)) && (xi == i + 1 + (dx * 3)))) continue;
						}
						
						/* Leave pillars on edge of corridor for crypts */
						else if ((width_path > 2) && ((flag & (MAZE_CRYPT)) != 0))
						{
							/* Leave pillars on edges of corridor */
							if ((xi + yi) % 2 == offset)
							{
								if ((dy) && ((xi < i + 1) || (xi >= i + width_path - 1))) continue;
								else if ((dx) && ((yi < j + 1) || (yi >= j + width_path - 1))) continue;
							}
						}

						/* Draw the path */
						/* XXX We have to use the auxiliary cave_set_feat function.
						 * This is because trees and chasms would otherwise create infinite loops, as cave_set_feat(yi, xi, feat)
						 * does not guarantee that cave_feat[yi][xi] = feat_path. */
						cave_set_feat_aux(yi, xi, feat_path);
					}
				}
				
				/* Advance */
				y += dy;
				x += dx;

				/* One less grid to examine */
				grids--;

				/* Check for completion */
				if ((y == ty) && (x == tx)) break;
			}
		}

		/* Find a new position */
		y = rand_int(ydim);
		x = rand_int(xdim);
	}
	
	/* Warn the player */
	if (count >= 10000) msg_print("Bug: Bad maze on level. Please report.");
	
	/* Create exits */
	if (flag & (MAZE_EXIT_N | MAZE_EXIT_S | MAZE_EXIT_W | MAZE_EXIT_E))
	{
		int feat = feat_path;
		
		/* Get door instead */
		if (f_info[feat_wall].flags1 & (FF1_OUTER)) feat = feat_state(feat_wall, FS_DOOR);

		if (flag & (MAZE_EXIT_N))
		{
			dx = rand_int(xdim);
			
			for (x = 0; x < width_path; x++)
			{
				/* Convert to path grid */
				cave_set_feat(y1, XPOS(dx, x1) + x, feat);
			}
		}

		if (flag & (MAZE_EXIT_S))
		{
			dx = rand_int(xdim);
			
			for (x = 0; x < width_path; x++)
			{
				/* Convert to path grid */
				cave_set_feat(y2, XPOS(dx, x1) + x, feat);
			}
		}	
		
		if (flag & (MAZE_EXIT_W))
		{
			dy = rand_int(ydim);
			
			for (y = 0; y < width_path; y++)
			{
				/* Convert to path grid */
				cave_set_feat(YPOS(dy, y1) + y, x1, feat);
			}
		}
		
		if (flag & (MAZE_EXIT_E))
		{
			dy = rand_int(ydim);
			
			for (y = 0; y < width_path; y++)
			{
				/* Convert to path grid */
				cave_set_feat(YPOS(dy, y1) + y, x2, feat);
			}
		}
	}
	
	/* Clear temp flags */
	if (flag & (MAZE_FILL))
	{
		for (y = y1 + width_outer; y <= y2 - width_outer; y++)
		{
			for (x = x1 + width_outer; x <= x2 - width_outer; x++)
			{
				play_info[y][x] &= ~(PLAY_TEMP);
			}
		}
	}

	/* Partially fill the maze back in */
	if (flag & (MAZE_DEAD | MAZE_POOL))
	{
		int loops = (flag & (MAZE_LOOP)) ?  1 /* ydim * xdim / 4 */ : 0;
		int flood = (flag & (MAZE_FLOOD)) ? ydim * xdim / 4 : 0;
		int pools = (flag & (MAZE_POOL)) ? ydim * xdim / 4 : 0;
		int doors = (flag & (MAZE_DOOR)) ? ydim * xdim / 4 : 0;
		int stuff = (flag & (MAZE_FILL)) ? ydim * xdim / 4 : 0;
		
		/* Number of grids to fill back in */
		if (flag & (MAZE_DEAD)) grids = ydim * xdim / 4;

		/* Less dead ends if not surrounded by wall */
		if ((flag & (MAZE_WALL)) == 0)
		{
			int n = 2 * ydim + 2 * xdim - 4;
			grids -= n;
			if (pools) pools -= n;
			if (doors) doors -= n;
			if (stuff) stuff -= n;
		}
		
		/* Dead end filler */
		while (loops || grids || pools || doors || stuff)
		{
			int k = 0;
			
			for (y = 0; y < ydim; y++)
			{
				for (x = 0; x < xdim; x++)
				{
					dirs = 0;
					
					j = YPOS(y, y1);
					i = YPOS(x, x1);
					
					/* Already filled */
					if (cave_feat[j][i] != feat_path) continue;
					
					/* Already filled - marked with a temp flag */
					if (((flag & (MAZE_FILL | MAZE_DOOR)) != 0) && ((play_info[j][i] & (PLAY_TEMP)) != 0)) continue;
					
					/* Up */
					if ((y || (width_outer)) && (cave_feat[j - 1][i + width_path / 2] == feat_path)) dir[dirs++] = 1;
	
					/* Down */
					if (((y < ydim - 1) || (width_outer)) && (cave_feat[j + width_path][i + width_path / 2] == feat_path)) dir[dirs++] = 2;
	
					/* Left */
					if ((x || (width_outer)) && (cave_feat[j + width_path / 2][i - 1] == feat_path)) dir[dirs++] = 3;
	
					/* Right */
					if (((x < xdim - 1) || (width_outer)) && (cave_feat[j + width_path / 2][i + width_path] == feat_path)) dir[dirs++] = 4;

					/* Dead end */
					if ((dirs == 1) && (!rand_int(++k)))
					{
						ty = y;
						tx = x;
						
						/* Note the direction */
						switch (dir[0])
						{
							/* Move up */
							case 1:  dy = -1; dx = 0;  break;

							/* Move down */
							case 2:  dy =  1; dx = 0;  break;

							/* Move left */
							case 3:  dx = -1; dy = 0; break;

							/* Move right */
							case 4:  dx =  1; dy = 0; break;
						}
					}
				}
			}
			
			/* Paranoia */
			if (!k) break;
			
			/* Convert coordinates */
			/* FIXME: ty and tx may be uninitialized here!!! */
			y = ty;
			x = tx;
			j = YPOS(y, y1);
			i = YPOS(x, x1);
			
			/* Open up dead-end as a loop */
			if (loops)
			{
				/* Place floors */
			        /* FIXME: dy and dx may be uninitialized here!!! */
				for (yi = j - (dy < 0 ? width_wall + width_path : 0); yi < j + (dy < 0 ? 0 : width_path) + (dy > 0 ? width_wall + width_path : 0); yi++)
				{
					for (xi = i - (dx < 0 ? width_wall + width_path : 0); xi < i + (dx < 0 ? 0 : width_path) + (dx > 0 ? width_wall + width_path : 0); xi++)
					{
						/* Leave pillars in centre of width 3 corridors, except crypts and caves */
						if ((width_path == 3) && ((flag & (MAZE_CRYPT | MAZE_CAVE)) == 0))
						{
							/* Pillar in current grid */
							if ((yi == j + 1) && (xi == i + 1)) continue;
							
							/* Pillar in destination grid (note width_path replaced by 3) */
							if ((yi == j + 1 + (dy * (width_wall + 3))) && (xi == i + 1 + (dx * (width_wall + 3)))) continue;
							
							/* Pillar in-between if width_wall is 3 */
							if ((width_wall == 3) && ((yi == j + 1 + (dy * 3)) && (xi == i + 1 + (dx * 3)))) continue;
						}
						
						/* Leave pillars on edge of corridor for crypts */
						else if ((width_path > 2) && ((flag & (MAZE_CRYPT)) != 0))
						{
							/* Leave pillars on edges of corridor */
							if ((xi + yi) % 2 == offset)
							{
								if ((dy) && ((xi < i + 1) || (xi >= i + width_path - 1))) continue;
								else if ((dx) && ((yi < j + 1) || (yi >= j + width_path - 1))) continue;
							}
						}

						/* Draw the path */
						/* XXX We have to use the auxiliary cave_set_feat function.
						 * This is because trees and chasms would otherwise create infinite loops, as cave_set_feat(yi, xi, feat)
						 * does not guarantee that cave_feat[yi][xi] = feat_path. */
						cave_set_feat_aux(yi, xi, feat_path);
					}
				}
				
				loops--;
			}
			
			/* Fill in dead-end or flood */
			else if (grids || flood)
			{
				/* Fill in floors */
				for (yi = j - (dy < 0 ? width_wall : 0); yi < j + width_path + (dy > 0 ? width_wall : 0); yi++)
				{
					for (xi = i - (dx < 0 ? width_wall : 0); xi < i + width_path + (dx > 0 ? width_wall : 0); xi++)
					{
						if (grids) build_terrain(yi, xi, inner);
						else cave_set_feat(yi, xi, feat_pool);
					}
				}

				/* Move outer walls in and remove parts of room */
				if ((width_outer) && (solid) && ((flag & (MAZE_ROOM)) != 0))
				{
					/* Clear room flags as required */
					for (yi = j - (y ? width_wall : 1); yi < j + width_path + ((y < ydim - 1) ? width_wall : 1); yi++)
					{
						for (xi = i - (x ? width_wall : 1); xi < i + width_path + ((x < xdim - 1) ? width_wall : 1); xi++)
						{
							bool clear = TRUE;
							
							if ((yi < j - width_wall + 1) && (y) && ((cave_info[j - width_wall - 1][i] & (CAVE_ROOM)) != 0)) clear = FALSE;
							if ((xi < i - width_wall + 1) && (x) && ((cave_info[j][i - (x ? width_wall : 1) - 1] & (CAVE_ROOM)) != 0)) clear = FALSE;
							if ((yi >= j + width_path + width_wall - 1) && (y < ydim - 1) && ((cave_info[j + width_path + width_wall][i] & (CAVE_ROOM)) != 0)) clear = FALSE;
							if ((xi >= i + width_path + width_wall - 1) && (x < xdim - 1) && ((cave_info[j][i + width_path + width_wall] & (CAVE_ROOM)) != 0)) clear = FALSE;
							
							/* No longer in room */
							if (clear) cave_info[yi][xi] &= ~(CAVE_ROOM | CAVE_LITE);
							
							/* On edge of room */
							else
							{
								if (!grids) cave_info[yi][xi] &= ~(CAVE_ROOM | CAVE_LITE);
								else if (((yi - y1 - 1) % (width_wall + width_path) < width_path) &&
										((xi - x1 - 1) % (width_wall + width_path) < width_path)) cave_set_feat(yi, xi, feat_wall);
								else cave_set_feat(yi, xi, solid);
							}
						}
					}
				}
				
				if (grids) grids--;
				else flood--; 
			}

			/* Fill in pool */
			else if (pools)
			{
				/* Fill in floors */
				for (yi = j - (dy < 0 ? width_wall : 0); yi < j + width_path + (dy > 0 ? width_wall : 0); yi++)
				{
					for (xi = i - (dx < 0 ? width_wall : 0); xi < i + width_path + (dx > 0 ? width_wall : 0); xi++)
					{
						if (cave_feat[yi][xi] != feat_path) continue;
						cave_set_feat(yi, xi, feat_pool);
					}
				}

				/* Ensure that external tunnels don't connect to pools in dead ends.*/
				/* In flooded mazes, we open up the dead end to 'outside' the maze */
				if (solid)
				{
					/* Check west & east */
					for (yi = j - 1; yi <= j + width_path; yi++)
					{
						if (cave_feat[yi][i - 1] == feat_wall) cave_set_feat(yi, i - 1, solid);
						if (cave_feat[yi][i + width_path] == feat_wall) cave_set_feat(yi, i + width_path, solid);
					}
	
					/* Check north & south */
					for (xi = i - 1; xi <= i + width_path; xi++)
					{
						if (cave_feat[j - 1][xi] == feat_wall) cave_set_feat(j - 1, xi, solid);
						if (cave_feat[j + width_path][xi] == feat_wall) cave_set_feat(j + width_path, xi, solid);
						}
				}

				pools--;
			}
			
			/* Fill in stuff and doors */
			else if (stuff | doors)
			{
				/* Fill in stuff */
				for (yi = j; yi < j + width_path; yi++)
				{
					for (xi = i; xi < i + width_path; xi++)
					{
						if (play_info[yi][xi] & (PLAY_TEMP)) continue;
						
						/* Good candidate for quest */
						if (!rand_int(width_path)) add_quest(yi, xi);
						
						play_info[yi][xi] |= PLAY_TEMP;
					}
				}
				
				if (stuff) stuff--;
				else doors--;
			}
		}
	}
	
	/* Clear temp flags */
	if (flag & (MAZE_FILL))
	{
		for (y = y1 + width_outer; y <= y2 - width_outer; y++)
		{
			for (x = x1 + width_outer; x <= x2 - width_outer; x++)
			{
				play_info[y][x] &= ~(PLAY_TEMP);
			}
		}
	}

#if 0
	/* Distribute cave grids */
	if (flag & (MAZE_CAVE))
	{
		msg_print("cave maze");
		
		for (y = y1 + width_outer; y <= y2 - width_outer; y++)
		{
			for (x = x1 + width_outer; x <= x2 - width_outer; x++)
			{
				int move;
				byte add_info = ((flag & (MAZE_ROOM)) != 0 ? CAVE_ROOM : 0) | ((flag & (MAZE_LITE)) != 0 ? CAVE_LITE : 0);
				
				if (cave_feat[y][x] == inner) continue;
				if (cave_feat[y][x] == feat_wall) continue;
				if (cave_feat[y][x] == solid) continue;
				
				if (!y || !x) continue;

				move = 0;
				if (((y - y1 - width_outer) % (width_wall + width_path) < width_path) && (rand_int(100) < 50)) move += 1;
				if (((x - x1 - width_outer) % (width_wall + width_path) < width_path) && (rand_int(100) < 50)) move += 2;
				
				switch(move)
				{
					case 1:
					{
						cave_set_feat(y + 1, x, cave_feat[y][x]);
						cave_info[y+1][x] |= add_info;
						
						/* Important - this ensures the top and left hand walls are affected correctly */
						if  ((feat_wall != inner) && ((cave_feat[y-1][x] == solid) || (cave_feat[y-1][x] == feat_wall)) && 
								((cave_feat[y-1][x+1] == solid) || (cave_feat[y-1][x+1] == feat_wall) || (cave_feat[y-1][x+1] == inner)))
						{
							if (rand_int(100) < 75)
							{
								cave_set_feat(y, x, cave_feat[y-1][x]);
								cave_set_feat(y - 1, x, FEAT_WALL_EXTRA);
								if (flag & (MAZE_ROOM)) cave_info[y-1][x] &= ~(CAVE_ROOM | CAVE_GLOW);
							}
						}
						else if ((cave_feat[y-1][x] == inner) && (cave_feat[y-1][x+1] == inner) && (rand_int(100) < 75)) cave_set_feat(y, x, inner);
						break;
					}
					case 2:
					{
						cave_set_feat(y, x + 1, cave_feat[y][x]);
						cave_info[y][x+1] |= add_info;
						
						/* Important - this ensures the top and left hand walls are affected correctly */
						if  ((feat_wall != inner) && ((cave_feat[y][x-1] == solid) || (cave_feat[y][x-1] == feat_wall)) &&
								((cave_feat[y+1][x-1] == solid) || (cave_feat[y+1][x-1] == feat_wall) || (cave_feat[y+1][x-1] == inner)))
						{
							if (rand_int(100) < 75)
							{
								cave_set_feat(y, x, cave_feat[y][x-1]);
								cave_set_feat(y, x - 1, FEAT_WALL_EXTRA);
								if (flag & (MAZE_ROOM)) cave_info[y][x-1] &= ~(CAVE_ROOM | CAVE_GLOW);
							}
						}
						else if ((cave_feat[y][x-1] == inner) && (cave_feat[y+1][x-1] == inner) && (rand_int(100) < 75)) cave_set_feat(y, x, inner);
						break;
					}
					case 3:
					{
						cave_set_feat(y + 1, x + 1, cave_feat[y][x]);
						cave_info[y+1][x+1] |= add_info;
						
						/* Important - this ensures the top and left hand walls are affected correctly */
						if  ((feat_wall != inner) && ((cave_feat[y-1][x-1] == solid) || (cave_feat[y-1][x-1] == feat_wall)))
						{
							/* Nothing - this is rare enough that I don't want to write an overly complicated test for it */
						}
						else if ((cave_feat[y-1][x] == inner) && (cave_feat[y-1][x+1] == inner) && 
								(cave_feat[y][x-1] == inner) && (cave_feat[y+1][x-1] == inner) && (rand_int(100) < 75)) cave_set_feat(y, x, inner);
						break;
					}
				}
			}	
		}
	}
	
	/* Re-process tree / chasm / glowing /outside grids.  This is required because
	 * of the use of the cave_set_feat auxiliary function above. */
	if (((f_info[feat_path].flags2 & (FF2_GLOW | FF2_CHASM)) != 0) ||
			((f_info[feat_path].flags2 & (FF3_TREE | FF3_OUTSIDE)) != 0))
	{
		for (y = y1 + width_outer; y <= y2 - width_outer; y++)
		{
			for (x = x1 + width_outer; x <= x2 - width_outer; x++)
			{
				int feat = cave_feat[y][x];
				cave_set_feat(y, x, FEAT_FLOOR);
				cave_set_feat(y, x, feat);
			}
		}
	}
#endif

	/* Restore grids */
	if ((flag & (MAZE_SAVE)) != 0)
	{
		for (y = 0; y <= y2 - y1; y++)
		{
			for (x = 0; x <= x2 - x1; x++)
			{
				/* Hack -- always overwrite if room flag set and room flag is clear */
				if (((flag & (MAZE_ROOM)) != 0) && ((cave_info[y + y1][x + x1] & (CAVE_ROOM)) == 0))
				{
					cave_set_feat(y + y1, x + x1, saved[y * (2 + y2 - y1) + x]);
	
					continue;
				}
				
				/* Hack -- skip floor quickly */
				if (saved[y * (2 + y2 - y1) + x] == FEAT_FLOOR) continue;
				
				/* Hack -- skip 'extra' walls quickly */
				if (saved[y * (2 + y2 - y1) + x] == FEAT_WALL_EXTRA) continue;

				/* Hack -- skip 'outer' and 'solid' terrain quickly */
				if (f_info[saved[y * (2 + y2 - y1) + x] == FEAT_WALL_EXTRA].flags1 & (FF1_OUTER | FF1_SOLID)) continue;
				
				/* Passable terrain - overwrite floor */
				if (((f_info[saved[y * (2 + y2 - y1) + x]].flags1 & (FF1_MOVE)) != 0) ||
					((f_info[saved[y * (2 + y2 - y1) + x]].flags3 & (FF3_EASY_CLIMB)) != 0))
				{
					/* Must be placed on floor grid */
					if (cave_feat[y + y1][x + x1] == feat_path)
					{
						cave_set_feat(y + y1, x + x1, saved[y * (2 + y2 - y1) + x]);
					}
					
					/* Try adjacent saved */
					else for (i = 0; i < 8; i++)
					{
						int yy = y + y1 + ddy_ddd[i];
						int xx = x + x1 + ddx_ddd[i];
						
						/* Paranoia */
						if (!in_bounds_fully(yy, xx)) continue;
						
						if (cave_feat[yy][xx] == feat_path)
						{
							cave_set_feat(yy, xx, saved[y * (2 + y2 - y1) + x]);
							break;
						}
					}
				}
				/* Impassable terrain - overwrite walls */
				else
				{
					/* Must be placed on wall grid -- except outer or solid wall */
					if (((f_info[cave_feat[y + y1][x + x1]].flags1 & (FF1_OUTER | FF1_SOLID)) == 0) &&
							((cave_feat[y + y1][x + x1] == feat_wall) ||
							(((f_info[feat_wall].flags1 & (FF1_OUTER)) != 0) && cave_feat[y + y1][x + x1] == feat_state(feat_wall, FS_INNER))))
					{
						cave_set_feat(y + y1, x + x1, saved[y * (2 + y2 - y1) + x]);
					}
					/* Try adjacent saved */
					else for (i = 0; i < 8; i++)
					{
						int yy = y + y1 + ddy_ddd[i];
						int xx = x + x1 + ddx_ddd[i];
						
						/* Paranoia */
						if (!in_bounds_fully(yy, xx)) continue;
						
						if (((f_info[cave_feat[yy][xx]].flags1 & (FF1_OUTER | FF1_SOLID)) == 0) &&
							((cave_feat[yy][xx] == feat_wall) ||
							(((f_info[feat_wall].flags1 & (FF1_OUTER)) != 0) && cave_feat[yy][xx] == feat_state(feat_wall, FS_INNER))))
						{
							cave_set_feat(yy, xx, saved[y * (2 + y2 - y1) + x]);
							break;
						}
					}
				}
			}
		}
	
		/* Free the grids */
		/*FREE(saved);*/
	}
	
	/* Successful */
	return (TRUE);
}


#undef YPOS
#undef XPOS


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
	if ((!in_bounds_fully(y1, x1)) || (!in_bounds_fully(y2, x2))) return (FALSE);

	/* Robustness -- test sanity of input coordinates. */
	if ((y1 + 2 >= y2) || (x1 + 2 >= x2)) return (FALSE);

	/* Get room height and width. */
	height = 1 + y2 - y1;
	width  = 1 + x2 - x1;

	/* Note the "size" */
	size = 2 + (width + height) / 22;

	/* Get a shrinkage ratio for large starbursts, as table is limited. */
	if ((width > 40) || (height > 40))
	{
		if (width > height) dist_conv = 1 + (10 * width  / 40);
		else                dist_conv = 1 + (10 * height / 40);
	}
	else dist_conv = 10;

	/* Make a cloverleaf starburst sometimes.  (discovered by accident) */
	if ((flag & (STAR_BURST_CLOVER)) && (height > 10) && (!rand_int(20)))
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
		degree_first += 180 / arc_num;

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
							play_info[y][x] |= (PLAY_TEMP);
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
	s16b feat, s16b edge, u32b flag)
{
	int y, x, d;

	/* Paranoia */
	if (!feat) return (FALSE);

	/* Mark the affected grids */
	if (!mark_starburst_shape(y1, x1, y2, x2, flag)) return (FALSE);


	if (cheat_room) msg_print("Generating starburst.");

	/* Paranoia */
	if (edge == feat) edge = FEAT_NONE;

	/* Process marked grids */
	for (y = y1 + 1; y < y2; y++)
	{
		for (x = x1 + 1; x < x2; x++)
		{
			/* Floor overwrites the dungeon */
			if (flag & (STAR_BURST_MOAT))
			{			
				/* Non-room grids only for moats */
				if (cave_info[y][x] & (CAVE_ROOM)) continue;

				/* Mark grids next to rooms with terrain if required */
				else if (!(play_info[y][x] & (PLAY_TEMP)))
				{
					int i;
					
					/* Check adjacent grids for 'rooms' */
					for (i = 0; i < 8; i++)
					{
						int yy,xx;

						yy = y + ddy_ddd[i];
						xx = x + ddx_ddd[i];
	
						/* Ignore annoying locations */
						if (!in_bounds_fully(yy, xx)) continue;

						/* Mark with temp flag if next to room */
						if (cave_info[yy][xx] & (CAVE_ROOM))
						{
							/* Include in starburst */
							play_info[y][x] |= (PLAY_TEMP);
						}
					}
				}
			}

			/* Marked grids only */
			if (!(play_info[y][x] & (PLAY_TEMP))) continue;

			/* Do not touch "icky" grids. */
			if (room_has_flag(y, x, ROOM_ICKY)) continue;

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

			/* Only place on floor grids */
			if ((flag & (STAR_BURST_NEED_FLOOR)) && (cave_feat[y][x] != FEAT_FLOOR))
			{
				/* Do nothing */
			}
			/* Random sometimes places floor */
			else if ((flag & (STAR_BURST_RANDOM)) && (rand_int(100) < 40))
			{
				cave_set_feat(y, x, FEAT_FLOOR);
			}
			/* Floor overwrites the dungeon */
			else if (flag & (STAR_BURST_RAW_FLOOR))
			{
				cave_set_feat(y, x, feat);
			}
			/* Floor is merged with the dungeon */
			else
			{
				int tmp_feat = feat;

				/* Hack -- make variable terrain surrounded by floors */
				if (((f_info[cave_feat[y][x]].flags1 & (FF1_WALL)) != 0) &&
					(variable_terrain(&tmp_feat,feat))) cave_set_feat(y,x,FEAT_FLOOR);

				build_terrain(y, x, tmp_feat);
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
				if ((flag & (STAR_BURST_LIGHT |
					STAR_BURST_ROOM)) == 0) continue;

				/* Look in all directions. */
				for (d = 0; d < 8; d++)
				{
					/* Extract adjacent location */
					int yy = y + ddy_ddd[d];
					int xx = x + ddx_ddd[d];

					/* Ignore annoying locations */
					if (!in_bounds_fully(yy, xx)) continue;

					/* Already processed */
					if (play_info[yy][xx] & (PLAY_TEMP)) continue;

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
				if (play_info[yy][xx] & (PLAY_TEMP)) continue;

				/* Do not touch "icky" grids. */
				if (room_has_flag(yy, xx, ROOM_ICKY)) continue;

				/* Do not touch occupied grids. */
				if (cave_m_idx[yy][xx] != 0) continue;
				if (cave_o_idx[yy][xx] != 0) continue;

				/* Floor overwrites the dungeon */
				if (flag & (STAR_BURST_MOAT))
				{			
					/* Non-room grids only for moats */
					if (cave_info[y][x] & (CAVE_ROOM)) continue;
				}

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

				/* Allow light to spill out of rooms through transparent edges */
				if (flag & (STAR_BURST_ROOM))
				{
					cave_info[yy][xx] |= (CAVE_ROOM);
					
					/* Allow lighting up rooms to work correctly */
					if  (((flag & (STAR_BURST_LIGHT)) != 0) && (f_info[cave_feat[yy][xx]].flags1 & (FF1_LOS)))
					{
						/* Look in all directions. */
						for (d = 0; d < 8; d++)
						{
							/* Extract adjacent location */
							int yyy = yy + ddy_ddd[d];
							int xxx = xx + ddx_ddd[d];
							
							/* Hack -- light up outside room */
							cave_info[yyy][xxx] |= (CAVE_GLOW);
						}
					}
				}
			}
		}
	}

	/* Clear the mark */
	for (y = y1 + 1; y < y2; y++)
	{
		for (x = x1 + 1; x < x2; x++)
		{
			play_info[y][x] &= ~(PLAY_TEMP);
		}
	}

	/* Success */
	return (TRUE);
}





/*
 * Number to place for scattering
 */
#define NUM_SCATTER   7


/*
 * Generate helper -- draw a rectangle with a feature using a series of 'pattern' flags.
 */
static void generate_patt(int y1, int x1, int y2, int x2, s16b feat, u32b flag, u32b exclude, int dy, int dx, int scale, int scatter)
{
	int y, x, i, k;

	int y_alloc = 0, x_alloc = 0, choice;

	int offset = rand_int(100) < 50 ? 1 : 0;
	int max_offset = offset + 1;

	bool outer;
	bool use_edge;

	s16b edge = f_info[feat].edge;

	/* Ignore edge terrain - use central terrain */
	if ((flag & (RG1_IGNORE_EDGE)) != 0)
	{
		edge = feat;
	}
	/* Bridge or tunnel edges instead */
	else if ((flag & (RG1_BRIDGE_EDGE)) != 0)
	{
		if (f_info[feat].flags2 & (FF2_BRIDGE))
		{
			/* Bridge previous contents */
			edge = feat_state(feat, FS_BRIDGE);
		}
		/* Apply tunnel */
		else if (f_info[feat].flags1 & (FF1_TUNNEL))
		{
			/* Tunnel previous contents */
			edge = feat_state(feat, FS_TUNNEL);
		}
	}
	/* Paranoia */
	if (!dy || !dx) return;

	/* Pick features if needed */
	if ((feat) && (f_info[feat].mimic == feat))
	{
		/* Set feature hook */
		room_info_feat_mimic = feat;

		get_feat_num_hook = room_info_feat;

		/* Prepare allocation table */
		get_feat_num_prep();
	}

	/* Draw maze if required -- ensure minimum size of 5 in both directions (==> y2 - y1 >= 4), or 3 if maze decor (which has no outer walls) */
	if (((flag & (RG1_MAZE_PATH | RG1_MAZE_WALL | RG1_MAZE_DECOR)) != 0) && (ABS(y2 - y1) >= ((flag & (RG1_MAZE_DECOR)) ? 2 : 4))
			&& (ABS(x2 - x1) >= ((flag & (RG1_MAZE_DECOR)) ? 2 : 4)) && ((flag & (RG1_ALLOC)) == 0) && (feat))
	{
		/* Maze dimensions */
		/* Note: Ensure the correct ordering and that size is odd in both directions (==> y2 - y1 is even) */
		int y0 = (dy > 0) ? y1 : (y2 + ((y1 - y2) % 2 ? 1 : 0));
		int x0 = (dx > 0) ? x1 : (x2 + ((x1 - x2) % 2 ? 1 : 0));
		int y3 = (dy > 0) ? (y2 - ((y2 - y1) % 2 ? 1 : 0)) : y1;
		int x3 = (dx > 0) ? (x2 - ((x2 - x1) % 2 ? 1 : 0)) : x1;

		int wall = ((flag & (RG1_MAZE_WALL)) != 0) ? feat : (((flag & (RG1_MAZE_DECOR)) != 0) ? (edge && (edge != feat) ? edge : FEAT_FLOOR) : FEAT_WALL_OUTER);
		int path = ((flag & (RG1_MAZE_PATH | RG1_MAZE_DECOR)) != 0) ? feat : (edge && (edge != feat) ? edge : FEAT_FLOOR);
		int pool = 0;

		u32b maze_flags = MAZE_SAVE | MAZE_ROOM | ((flag & (RG1_MAZE_DECOR)) ? 0 : MAZE_WALL);
		
		/* Ensure light */
		if ((exclude & (RG1_DARK)) != 0) maze_flags |= MAZE_LITE;
				
		/* Need an exit? */
		if ((flag & (RG1_MAZE_DECOR)) == 0)
		{
			u32b temp_flags;
			
			do
			{
				temp_flags = 0L;
				
				/* Hack -- extend maze north & south if either side is directly next to outer wall */
				for (x = x0 + 1; x < x3; x++)
				{
					if (((maze_flags & (MAZE_OUTER_N)) == 0) && (y0 > 0) && ((f_info[cave_feat[y0-1][x]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)) temp_flags |= MAZE_OUTER_N;
					if (((maze_flags & (MAZE_OUTER_S)) == 0) && (y3 < DUNGEON_HGT-1) && ((f_info[cave_feat[y3+1][x]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)) temp_flags |= MAZE_OUTER_S;				                          
				}

				/* Extend north and south */
				if (temp_flags)
				{
					y0--; y3++;
				
					/* Add extra flags */
					maze_flags |= temp_flags;
				}
			} while (temp_flags);

			do
			{
				temp_flags = 0L;
				
				/* Hack -- extend maze east & west if either side is directly next to outer wall */
				for (y = y0 + 1; y < y3; y++)
				{
					if (((maze_flags & (MAZE_OUTER_W)) == 0) && (x0 > 0) && ((f_info[cave_feat[y][x0-1]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)) temp_flags |= MAZE_OUTER_W;
					if (((maze_flags & (MAZE_OUTER_E)) == 0) && (x3 < DUNGEON_WID-1) && ((f_info[cave_feat[y][x3+1]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)) temp_flags |= MAZE_OUTER_E; 
				}

				/* Extend north and south */
				if (temp_flags)
				{
					y0--; y3++;
				
					/* Add extra flags */
					maze_flags |= temp_flags;
				}
			} while (temp_flags);
			
			/* Outer wall north/south */
			for (x = x0 + 1; x < x3; x++)
			{
				if ((f_info[cave_feat[y0][x]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0) maze_flags |= MAZE_OUTER_N;
				if ((f_info[cave_feat[y3][x]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0) maze_flags |= MAZE_OUTER_S;				                          
			}
			
			/* Outer wall east/west */
			for (y = y0 + 1; y < y3; y++)
			{
				if ((f_info[cave_feat[y][x0]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0) maze_flags |= MAZE_OUTER_W;
				if ((f_info[cave_feat[y][x3]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0) maze_flags |= MAZE_OUTER_E;				                          
			}
			
			/* Two exits required? */
			if (((maze_flags & (MAZE_OUTER_N)) != 0) && ((maze_flags & (MAZE_OUTER_S)) != 0) && ((maze_flags & (MAZE_OUTER_E | MAZE_OUTER_W)) == 0))
			{
				maze_flags |= (MAZE_EXIT_W | MAZE_EXIT_E);
			}
			else if (((maze_flags & (MAZE_OUTER_W)) != 0) && ((maze_flags & (MAZE_OUTER_E)) != 0) && ((maze_flags & (MAZE_OUTER_N | MAZE_OUTER_S)) == 0))
			{
				maze_flags |= (MAZE_EXIT_N | MAZE_EXIT_S);
			}
			/* Zero or one exits required */
			else
			{
				u32b maze_exits = 0L;
				int k = 0;
				
				if (((maze_flags & (MAZE_OUTER_N)) == 0) && !(rand_int(++k))) maze_exits = MAZE_EXIT_N;
				if (((maze_flags & (MAZE_OUTER_S)) == 0) && !(rand_int(++k))) maze_exits = MAZE_EXIT_S;
				if (((maze_flags & (MAZE_OUTER_W)) == 0) && !(rand_int(++k))) maze_exits = MAZE_EXIT_W;
				if (((maze_flags & (MAZE_OUTER_E)) == 0) && !(rand_int(++k))) maze_exits = MAZE_EXIT_E;
				
				maze_flags |= maze_exits;				
			}
		}
		
		/* Paranoia */
		if (wall == path) path = FEAT_FLOOR;
		
		/* Hack -- swap path and wall if path does not allow movement */
		if (((f_info[path].flags1 & (FF1_MOVE)) == 0) && ((f_info[path].flags3 & (FF3_EASY_CLIMB)) == 0))
		{
			int temp = wall;
			wall = path;
			path = temp;
			
			/* Paranoia */
			if (((f_info[path].flags1 & (FF1_MOVE)) == 0) && ((f_info[path].flags3 & (FF3_EASY_CLIMB)) == 0))
			{
				pool = path;
				path = FEAT_FLOOR;
				
				if (pool) maze_flags |= (MAZE_POOL);
			}
		}

		/* Draw the maze. */
		draw_maze(y0, x0, y3, x3, wall, path, 1, scale, pool, maze_flags);
		
		/* Hack -- scatter items inside maze */
		flag |= (RG1_SCATTER);
		feat = 0;
		edge = 0;
	}

	/* Place starburst if required */
	else if (((flag & (RG1_STARBURST)) != 0) && ((flag & (RG1_ALLOC)) == 0) &&
			(ABS(y2 - y1) >= 4) && (ABS(y2 - y1) >= 4))
	{
		/* Ensure the correct ordering of directions */
		int y0 = (dy > 0) ? y1 : y2;
		int x0 = (dx > 0) ? x1 : x2;
		int y3 = (dy > 0) ? y2 : y1;
		int x3 = (dx > 0) ? x2 : x1;
		
		u32b star_flags = STAR_BURST_ROOM | STAR_BURST_NEED_FLOOR;

		/* Ensure light */
		if ((exclude & (RG1_DARK)) != 0) star_flags |= STAR_BURST_LIGHT;
		 
		/* Ensure random floor space */
		if (flag & (RG1_RANDOM | RG1_SCATTER | RG1_CHECKER)) star_flags |= STAR_BURST_RANDOM;
		
		/* Create the starburst */
		if (feat) generate_starburst_room(y0, x0, y3, x3, feat, edge, star_flags);

		/* Hack -- scatter items around the starburst */
		flag |= (RG1_SCATTER);
		feat = 0;
		edge = 0;
	}

	/* Use checkered for invalid mazes / starbursts */
	else if ((flag & (RG1_MAZE_PATH | RG1_MAZE_WALL | RG1_MAZE_DECOR | RG1_STARBURST)) != 0)
	{
		flag |= (RG1_CHECKER);
	}
	
	/* Scatter several about if requested */
	for (k = 0; k < ( ((flag & (RG1_SCATTER | RG1_TRAIL)) != 0) && ((flag & (RG1_ALLOC)) == 0) ? scatter : 1); k++)
	{
		/* Pick location */
		choice = 0;

		/* Scan the whole room */
		for (y = y1; (dy > 0) ? y <= y2 : y >= y2; y += dy)
		{
			for (x = x1; (dx > 0) ? x <= x2 : x >= x2; x += dx)
			{
				outer = ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) != 0);
				use_edge = FALSE;

				/* Checkered room */
				if (((flag & (RG1_CHECKER)) != 0) && ((x + y + offset) % 2)) continue;

				/* Only place on outer/solid walls */
				if (((flag & (RG1_OUTER)) != 0) && (cave_feat[y][x] != FEAT_WALL_OUTER) && (cave_feat[y][x] != FEAT_WALL_SOLID)) continue;

				/* Only place on floor otherwise */
				if (((flag & (RG1_OUTER)) == 0) && (cave_feat[y][x] != FEAT_FLOOR))
				{
					if (((flag & (RG1_CHECKER)) != 0) && (offset < max_offset)) offset++;

					continue;
				}

				/* Clear max_offset */
				max_offset = 0;

				/* Only place on edge of room if edge flag and not centre flag set */
				if (((flag & (RG1_EDGE)) != 0) && ((flag & (RG1_CENTRE)) == 0) && (cave_feat[y][x] != FEAT_WALL_OUTER))
				{
					for (i = 0; i < 8; i++)
					{
						if ((f_info[cave_feat[y + ddy_ddd[i]][x + ddx_ddd[i]]].flags1 & (FF1_OUTER)) != 0) use_edge = TRUE;
					}

					if (!use_edge) continue;
				}
				/* Don't place on edge of room if centre and not edge flag are set */
				else if (((flag & (RG1_CENTRE)) != 0) && ((flag & (RG1_EDGE)) == 0) && (cave_feat[y][x] != FEAT_WALL_OUTER))
				{
					bool accept = TRUE;

					for (i = 0; i < 4; i++)
					{
						if ((f_info[cave_feat[y + ddy_ddd[i]][x + ddx_ddd[i]]].flags1 & (FF1_OUTER)) != 0) accept = FALSE;
					}

					if (!accept) continue;

					use_edge = TRUE;
				}

				/* Leave inner area open */
				if ((flag & (RG1_INNER)) != 0)
				{
					if (((dy > 0) ? (y > y1) && (y + dy <= y2) : (y + dy >= y2) && (y < y1)) &&
						((dx > 0) ? (x > x1) && (x + dx <= x2) : (x + dx >= x2) && (x < x1))) continue; 
				}

				/* Random */
				if (((flag & (RG1_RANDOM)) != 0) && (rand_int(100) < 40)) continue;

				/* Only place next to last choice */
				if ((flag & (RG1_TRAIL)) != 0)
				{
					/* Place next to previous position */
					if ((k == 0) || (distance(y, y_alloc, x, x_alloc) < ABS(dy) + ABS(dx)))
					{
						if (rand_int(++choice) == 0)
						{
							y_alloc = y;
							x_alloc = x;
						}
					}
				}
				/* Maybe pick if placing one */
				else if ((flag & (RG1_ALLOC | RG1_SCATTER | RG1_TRAIL | RG1_8WAY)) != 0)
				{
					if (rand_int(++choice) == 0)
					{
						y_alloc = y;
						x_alloc = x;
					}
				}

				/* Set feature */
				else
				{
					int place_feat = feat;

					/* Use edge instead */
					if ((use_edge) && (edge)) place_feat = edge;

					/* Hack -- in case we don't place enough */
					if ((flag & (RG1_RANDOM)) != 0)
					{
						if (rand_int(++choice) == 0)
						{
							y_alloc = y;
							x_alloc = x;
						}
					}

					/* Pick a random feature? */
					if ((feat) && (f_info[feat].mimic == feat))
					{
						place_feat = get_feat_num(object_level);

						if (!place_feat) place_feat = feat;
					}

					/* Assign feature */
					if (place_feat)
					{
						/* Preserve the 'solid' status of a wall & ensure that at least 1 feature is placed */
						if (cave_feat[y][x] == FEAT_WALL_SOLID)
						{
							/* Place solid wall now */
							if ((f_info[place_feat].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)
							{
								cave_set_feat(y, x, (f_info[place_feat].flags1 & (FF1_OUTER)) != 0 ? feat_state(place_feat, FS_SOLID) : place_feat);
							}
							else if (dun->next_n < NEXT_MAX)
							{
								/* Overwrite solid wall later */
								dun->next[dun->next_n].y = y;
								dun->next[dun->next_n].x = x;
								dun->next_feat[dun->next_n] = place_feat;
								dun->next_n++;
							}
						}
						else
						{
							cave_set_feat(y, x, place_feat);

							/* Hack - fix outer walls if placing inside a room */
							if ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) && !(outer)) cave_alter_feat(y, x, FS_INNER);
						}

						/* Pick one choice for object on feature */
						if (((flag & (RG1_HAS_GOLD | RG1_HAS_ITEM)) != 0) && (rand_int(++choice) == 0))
						{
							y_alloc = y;
							x_alloc = x;
						}

						/* Otherwise don't place objects and features */
						continue;
					}

					/* Require "clean" floor space */
					if ((flag & (RG1_HAS_GOLD | RG1_HAS_ITEM)) != 0)
					{
						/* Either place or overwrite outer wall if required */
						if ((cave_clean_bold(y, x)) || (((flag & (RG1_OUTER)) != 0) && (cave_feat[y][x] == FEAT_WALL_OUTER)))
						{
							/* Hack -- erase outer wall */
							if (cave_feat[y][x] == FEAT_WALL_OUTER) cave_set_feat(y, x, FEAT_FLOOR);

							/* Drop gold 50% of the time if both defined */
							if (((flag & (RG1_HAS_GOLD)) != 0) && (((flag & (RG1_HAS_ITEM)) == 0) || (rand_int(100) < 50))) place_gold(y, x);
							else place_object(y, x, FALSE, FALSE);
						}
					}
				}
			}
		}

		/* Scatter objects around feature if both placed */
		if ((feat) && ((flag & (RG1_HAS_GOLD | RG1_HAS_ITEM)) != 0))
		{
			feat = 0;
			flag |= (RG1_SCATTER);

			/* Paranoia */
			if (!choice) continue;
		}

		/* Hack -- if we don't have enough the first time, scatter instead */
		if (((flag & (RG1_RANDOM)) != 0) && (choice < scatter))
		{
			flag &= ~(RG1_RANDOM);
			flag |= (RG1_SCATTER);

			/* Paranoia */
			if (!choice) continue;
		}

		/* Finally place in 8 directions */
		if (((flag & (RG1_8WAY)) != 0) && choice)
		{
			int place_feat = feat;

			/* Pick a random feature? */
			if ((feat) && (f_info[feat].mimic == feat))
			{
				place_feat = get_feat_num(object_level);

				if (!place_feat) place_feat = feat;
			}

			/* Loop through features */
			for (k = 0; k < MAX_SIGHT; k++)
			{
				for (i = 0; i < 8; i++)
				{
					/* Get position */
					y = y_alloc + k * ddy_ddd[i];
					x = x_alloc + k * ddx_ddd[i];

					/* Limit spread */
					if ((y < y1) || (y > y2) || (x < x1) || (x > x2)) continue;

					outer = ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) != 0);

					/* Assign feature */
					if (place_feat)
					{
						/* Hack -- if we are placing one feature, we replace it with a solid wall to ensure that it
						 * is not overwritten later on. We take advantage of the dun->next array to do this.
						 */
						if ((k == 0) && (dun->next_n < NEXT_MAX)) cave_feat[y][x] = FEAT_WALL_SOLID;

						/* Preserve the 'solid' status of a wall */
						if (cave_feat[y][x] == FEAT_WALL_SOLID)
						{
							/* Place solid wall now */
							if ((f_info[place_feat].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)
							{
								cave_set_feat(y, x, (f_info[place_feat].flags1 & (FF1_OUTER)) != 0 ? feat_state(place_feat, FS_SOLID) : place_feat);
							}
							else if (dun->next_n < NEXT_MAX)
							{
								/* Overwrite solid wall later */
								dun->next[dun->next_n].y = y;
								dun->next[dun->next_n].x = x;
								dun->next_feat[dun->next_n] = place_feat;
								dun->next_n++;
							}
						}
						else
						{
							cave_set_feat(y, x, place_feat);

							/* Hack - fix outer walls if placing inside a room */
							if ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) && !(outer)) cave_alter_feat(y, x, FS_INNER);
						}
					}

					/* Require "clean" floor space */
					if ((flag & (RG1_HAS_GOLD | RG1_HAS_ITEM)) != 0)
					{
						/* Either place or overwrite outer wall if required */
						if ((cave_clean_bold(y, x)) || (((flag & (RG1_OUTER)) != 0) && (cave_feat[y][x] == FEAT_WALL_OUTER)))
						{
							/* Hack -- erase outer wall */
							if (cave_feat[y][x] == FEAT_WALL_OUTER) cave_set_feat(y, x, FEAT_FLOOR);

							/* Drop gold 50% of the time if both defined */
							if (((flag & (RG1_HAS_GOLD)) != 0) && (((flag & (RG1_HAS_ITEM)) == 0) || (rand_int(100) < 50))) place_gold(y, x);
							else place_object(y, x, FALSE, FALSE);
						}
					}
				}
			}
		}

		/* Finally place if allocating a single feature */
		else if (((flag & (RG1_ALLOC | RG1_SCATTER | RG1_TRAIL)) != 0) && choice)
		{
			int place_feat = feat;
			int alloc_scale = ((flag & (RG1_SCATTER)) == 0) ? scale : 1;

			/* Occasionally scale up single allocation */
			if (((flag & (RG1_SCATTER | RG1_TRAIL)) == 0) && (rand_int(100) < p_ptr->depth))
			{
				alloc_scale++;
			}
			
			/* Get location */
			for (y = y_alloc; (dy > 0) ? y < (y_alloc + dy * alloc_scale) : y > (y_alloc + dy * alloc_scale); y += dy)
			{
				for (x = x_alloc; (dx > 0) ? x < (x_alloc + dx * alloc_scale) : x > (x_alloc + dx * alloc_scale); x += dx)
				{
					/* Don't overwrite anything interesting */
					if ((cave_feat[y][x] != FEAT_FLOOR) && (cave_feat[y][x] != FEAT_WALL_OUTER) && (cave_feat[y][x] != FEAT_WALL_SOLID)) continue;
					
					/* Is outer? */
					outer = ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) != 0);
		
					/* Pick a random feature? */
					if ((feat) && (f_info[feat].mimic == feat))
					{
						place_feat = get_feat_num(object_level);
		
						if (!place_feat) place_feat = feat;
					}
		
					/* Assign feature */
					if (place_feat)
					{
						/* Hack -- if we are placing one feature, we replace it with a solid wall to ensure that it
						 * is not overwritten later on. We take advantage of the dun->next array to do this.
						 */
						if (k == 0) cave_feat[y][x] = FEAT_WALL_SOLID;
		
						/* Preserve the 'solid' status of a wall */
						if (cave_feat[y][x] == FEAT_WALL_SOLID)
						{
							/* Place solid wall now */
							if ((f_info[place_feat].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)
							{
								cave_set_feat(y, x, (f_info[place_feat].flags1 & (FF1_OUTER)) != 0 ? feat_state(place_feat, FS_SOLID) : place_feat);
							}
							else
							{
								/* Overwrite solid wall later */
								dun->next[dun->next_n].y = y;
								dun->next[dun->next_n].x = x;
								dun->next_feat[dun->next_n] = place_feat;
								dun->next_n++;
							}
						}
						else
						{
							cave_set_feat(y, x, place_feat);
		
							/* Hack - fix outer walls if placing inside a room */
							if ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) && !(outer)) cave_alter_feat(y, x, FS_INNER);
						}
					}
		
					/* Require "clean" floor space */
					if ((flag & (RG1_HAS_GOLD | RG1_HAS_ITEM)) != 0)
					{
						/* Either place or overwrite outer wall if required */
						if ((cave_clean_bold(y, x)) || (((flag & (RG1_OUTER)) != 0) && (cave_feat[y][x] == FEAT_WALL_OUTER)))
						{
							/* Hack -- erase outer wall */
							if (cave_feat[y][x] == FEAT_WALL_OUTER) cave_set_feat(y, x, FEAT_FLOOR);
		
							/* Drop gold 50% of the time if both defined */
							if (((flag & (RG1_HAS_GOLD)) != 0) && (((flag & (RG1_HAS_ITEM)) == 0) || (rand_int(100) < 50))) place_gold(y, x);
							else place_object(y, x, FALSE, FALSE);
						}
					}
				}
			}
		}
	}

	/* Clear feature hook */
	if ((feat) && (f_info[feat].mimic == feat))
	{
		/* Clear the hook */
		get_feat_num_hook = NULL;

		get_feat_num_prep();				
	}
}



/*
 * Find a good spot for the next room.  -LM-
 *
 * Find and allocate a free space in the dungeon large enough to hold
 * the room calling this function.
 *
 * We allocate space in 11x11 blocks, but want to make sure that rooms
 * align neatly on the standard screen.  Therefore, we make them use
 * blocks in few 11x33 rectangles as possible.
 *
 * Be careful to include the edges of the room in height and width!
 *
 * Return TRUE and values for the center of the room if all went well.
 * Otherwise, return FALSE.
 */
static bool find_space(int *y, int *x, int height, int width)
{
	int i;
	int by, bx, by1, bx1, by2, bx2;
	int block_y, block_x;

	bool filled;


	/* Find out how many blocks we need. */
	int blocks_high = 1 + ((height - 1) / BLOCK_HGT);
	int blocks_wide = 1 + ((width - 1) / BLOCK_WID);

	/* Sometimes, little rooms like to have more space. */
	if (blocks_wide == 2)
	{
		if (!rand_int(3)) blocks_wide = 3;
	}
	else if (blocks_wide == 1)
	{
		if (!rand_int(2)) blocks_wide = rand_range(2, 3);
	}


	/* We'll allow twenty-five guesses. */
	for (i = 0; i < 25; i++)
	{
		filled = FALSE;

		/* Hack -- try to put stuff near lakes */
		if (i < dun->lake_n)
		{
			/* Pick a block 'near' the lake */
			block_y = dun->lake[i].y - rand_int(blocks_high);
			block_x = dun->lake[i].x - rand_int(blocks_wide);
			
			/* Keep in dungeon */
			if (block_y < 0) block_y = 0;
			if (block_x < 0) block_x = 0;
		}
		else
		{
			/* Pick a top left block at random */
			block_y = rand_int(dun->row_rooms - blocks_high);
			block_x = rand_int(dun->col_rooms - blocks_wide);
		}

		/* Itty-bitty rooms can shift about within their rectangle */
		if (blocks_wide < 3)
		{
			/* Rooms that straddle a border must shift. */
			if ((blocks_wide == 2) && ((block_x % 3) == 2))
			{
				if (!rand_int(2)) block_x--;
				else block_x++;
			}
		}

		/* Rooms with width divisible by 3 get fitted to a rectangle. */
		else if ((blocks_wide % 3) == 0)
		{
			/* Align to the left edge of a 11x33 rectangle. */
			if ((block_x % 3) == 2) block_x++;
			if ((block_x % 3) == 1) block_x--;
		}

		/*
		 * Big rooms that do not have a width divisible by 3 get
		 * aligned towards the edge of the dungeon closest to them.
		 */
		else
		{
			/* Shift towards left edge of dungeon. */
			if (block_x + (blocks_wide / 2) <= dun->col_rooms / 2)
			{
				if (((block_x % 3) == 2) && ((blocks_wide % 3) == 2))
					block_x--;
				if ((block_x % 3) == 1) block_x--;
			}

			/* Shift toward right edge of dungeon. */
			else
			{
				if (((block_x % 3) == 2) && ((blocks_wide % 3) == 2))
					block_x++;
				if ((block_x % 3) == 1) block_x++;
			}
		}

		/* Extract blocks */
		by1 = block_y + 0;
		bx1 = block_x + 0;
		by2 = block_y + blocks_high;
		bx2 = block_x + blocks_wide;

		/* Never run off the screen */
		if ((by1 < 0) || (by2 > dun->row_rooms)) continue;
		if ((bx1 < 0) || (bx2 > dun->col_rooms)) continue;

		/* Verify available space */
		for (by = by1; by < by2; by++)
		{
			for (bx = bx1; bx < bx2; bx++)
			{
				if (dun->room_map[by][bx])
				{
					filled = TRUE;
				}
			}
		}

		/* If space filled, try again. */
		if (filled) continue;


		/* It is *extremely* important that the following calculation */
		/* be *exactly* correct to prevent memory errors XXX XXX XXX */

		/* Acquire the location of the room */
		(*y) = ((by1 + by2) * BLOCK_HGT) / 2;
		(*x) = ((bx1 + bx2) * BLOCK_WID) / 2;

		/* Save the room location */
		if (dun->cent_n < CENT_MAX)
		{
			dun->cent[dun->cent_n].y = *y;
			dun->cent[dun->cent_n].x = *x;
			dun->cent_n++;
		}

		/* Reserve some blocks.  Mark each with the room index. */
		for (by = by1; by < by2; by++)
		{
			for (bx = bx1; bx < bx2; bx++)
			{
				dun->room_map[by][bx] = TRUE;
				dun_room[by][bx] = dun->cent_n;
			}
		}

		/* Get matching ecology */
		if (dun->cent_n <= cave_ecology.num_ecologies)
		{
			/* Pick which ecology */
			room_info[dun->cent_n].ecology = (1L << dun->cent_n);
		
			/* Pick which deepest monster */
			room_info[dun->cent_n].deepest_race = cave_ecology.deepest_race[dun->cent_n];
		}
		
		/* Get the closest ecology */
		else
		{
			int min_d = 3000;
			int d, pick = 1;

			for (i = 0; i < cave_ecology.num_ecologies; i++)
			{
				/* Find distance */
				d = distance((*y), (*x), dun->cent[i].y, dun->cent[i].x);
			
				/* Pick nearest ecology */
				if (d < min_d)
				{
					pick = i + 1;
					min_d = d;
				}
			}

			/* Pick which ecology */
			room_info[dun->cent_n].ecology = room_info[pick].ecology;
		
			/* Pick which deepest monster */
			room_info[dun->cent_n].deepest_race = room_info[pick].deepest_race;
		}		
		
		/* Success. */
		return (TRUE);
	}

	/* Failure. */
	return (FALSE);
}



/*
 *  Ensure that the terrain matches the required level flags.
 */
static bool check_level_flags(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Exclude terrain of various types */
	if (level_flag & (LF1_WATER | LF1_LAVA | LF1_ICE | LF1_ACID | LF1_OIL | LF1_LIVING))
	{
		if (!(level_flag & (LF1_LIVING)) && (f_ptr->flags3 & (FF3_LIVING)))
		{
			return (FALSE);
		}

		if (!(level_flag & (LF1_WATER)) && (f_ptr->flags2 & (FF2_WATER)))
		{
			return (FALSE);
		}

		if (!(level_flag & (LF1_LAVA)) && (f_ptr->flags2 & (FF2_LAVA)))
		{
			return (FALSE);
		}

		if (!(level_flag & (LF1_ICE)) && (f_ptr->flags2 & (FF2_ICE)))
		{
			return (FALSE);
		}

		if (!(level_flag & (LF1_ACID)) && (f_ptr->flags2 & (FF2_ACID)))
		{
			return (FALSE);
		}

		if (!(level_flag & (LF1_OIL)) && (f_ptr->flags2 & (FF2_OIL)))
		{
			return (FALSE);
		}
	}

	return (TRUE);

}



/*
 * Pick appropriate feature for lake.
 */
bool cave_feat_lake(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Require lake or river */
	if (!(f_ptr->flags2 & (FF2_RIVER)))
	{
		if (!(f_ptr->flags2 & (FF2_LAKE)))
		{
			return (FALSE);
		}
	}

	/* Okay */
	return (check_level_flags(f_idx));
}


/*
 * Returns TRUE if f_idx is a valid pool feature
 */
static bool cave_feat_pool(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Hack -- Ignore solid features */
	if ((f_ptr->flags1 & (FF1_MOVE)) == 0)
	{
		return (FALSE);
	}

	/* All remaining lake features will be fine */
	return (cave_feat_lake(f_idx));
}


/*
 * Returns TRUE if f_idx is a valid pool feature
 */
static bool cave_feat_streamer(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Require lake or river */
	if (!(f_ptr->flags1 & (FF1_STREAMER)))
	{
		return (FALSE);
	}
	
	/* MegaHack -- now only quartz/magma is a valid stream */
	if (f_idx > 54) return (FALSE);

	/* All remaining features depend on level flags */
	return (check_level_flags(f_idx));
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
	int max_depth = p_ptr->depth;
	u16b feat;

	/* Set the given hook, if any */
	get_feat_num_hook = feat_hook;

	get_feat_num_prep();

	/* Pick a feature */
	feat = get_feat_num(max_depth);

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* Set the level flags based on the feature */
	set_level_flags(feat);
	
	/* Return the feature */
	return (feat);
}



/*
 * Hack -- tval and sval range for "room_info_kind()"
 */
static byte room_info_kind_tval;
static byte room_info_kind_min_sval;
static byte room_info_kind_max_sval;


/*
 *
 */
static bool room_info_kind(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	if (k_ptr->tval != room_info_kind_tval) return (FALSE);
	if (k_ptr->sval < room_info_kind_min_sval) return (FALSE);
	if (k_ptr->sval > room_info_kind_max_sval) return (FALSE);

	return(TRUE);
}

/*
 * How we match on flags
 *
 * Match any = no descriptions encountered matching monster or level flags
 * Match chance = descriptions matching monster or level flags only
 * Match theme = descriptions matching feature or tval theme
 *
 */

#define MATCH_ANY 0
#define MATCH_CHANCE 1
#define MATCH_THEME 2

/*
 * Variables to indicate whether we have the name decided already.
 */

#define PICKED_NAME1	0x01L
#define PICKED_NAME2	0x02L


/*
 * Get the room description. Return when we have a valid description, with
 * various stuff set to allow placement of features and objects relevent
 * to the room description parsed so far.
 */
static bool get_room_info(int room, int *chart, int *j, u32b *place_flag, s16b *place_feat, byte *place_tval, byte *place_min_sval, byte *place_max_sval, byte *branch, byte *branch_on, byte *name, u32b exclude)
{
	int i, count, pick, match, chance;

	byte last_tval;
	s16b last_feat;
	
	bool placement = FALSE;
	
	int counter = 0;

	if (cheat_room) msg_format("Getting room info %d.",*chart);

	/* Record last placements */
	last_feat = *place_feat;
	last_tval = *place_tval;
	
	/* Clear placements */
	*place_tval = 0;
	*place_min_sval = 0;
	*place_max_sval = 0;
	*place_flag = 0;
	*place_feat = 0;
	
	/* Process the description */
	/* Note we need 1 space at end of room_desc_sections to terminate description */
	while ((*chart != 0) && (*j < ROOM_DESC_SECTIONS - 1))
	{
		/* Start over */
		i = 0;
		count = 0;
		pick = -1;
		match = MATCH_ANY;

		/* Get the start of entries in the table for this index */
		while ((*chart != d_info[i].chart) && (i < z_info->d_max)) { i++; }

		/* Reached end of chart */
		if (i >= z_info->d_max)
		{
			msg_format("Error: Reached end of chart looking for %d. Please report.", *chart);
			return (FALSE);
		}

		/* Cycle through valid entries */
		while (*chart == d_info[i].chart)
		{
			bool good_chance = FALSE;
			u32b exclude_hack = d_info[i].p_flag;

			if (counter++ > 5000)
			{
				msg_print("Error: aborting loop in room descriptions. Please report.");
				return (FALSE);
			}

			/* If not allowed at this depth, skip completely */
			if (p_ptr->depth < d_info[i].level_min) { i++; continue; }
			if (p_ptr->depth > d_info[i].level_max) { i++; continue; }

			/* Hack for excluding directions. This allows us to exclude 'directions' except 'all directions' */
			if ((exclude_hack & (RG1_NORTH | RG1_SOUTH | RG1_EAST | RG1_WEST)) == (RG1_NORTH | RG1_SOUTH | RG1_EAST | RG1_WEST))
			{
				exclude_hack &= ~(RG1_NORTH | RG1_SOUTH | RG1_EAST | RG1_WEST);
			}

			/* If exluding these flags, skip completely. */
			if ((exclude & (exclude_hack)) != 0) { i++; continue; }

			/* Exclude items or gold */
			if ((exclude & (RG1_HAS_ITEM)) && (d_info[i].tval) && (d_info[i].tval < TV_GOLD)) { i++;continue; }
			if ((exclude & (RG1_HAS_GOLD)) /* && (d_info[i].tval) */ && (d_info[i].tval >= TV_GOLD)) { i++;continue; }

			/* Don't place same features or objects twice */
			if (last_tval && (d_info[i].tval == last_tval)) {i++; continue;}
			if (last_feat && (d_info[i].feat == last_feat)) {i++; continue;}
			
			/* Reset chance */
			chance = 0;			

			/* If requires this level type, reduce chance of occurring */
			if ((d_info[i].l_flag) && ((level_flag & d_info[i].l_flag) != 0)) good_chance = TRUE;

			/* If not allowed because doesn't match level monster, reduce chance of occuring */
			else if ((cave_ecology.ready) && (cave_ecology.num_races))
			{
				/* Match main monster */
				monster_race *r_ptr = &r_info[room_info[room].deepest_race];

				/* Check for char match */
				if ((d_info[i].r_char) && (d_info[i].r_char == r_ptr->d_char)) good_chance = TRUE;

				/* Check for flag match */
				if (d_info[i].r_flag)
				{
					if ((d_info[i].r_flag < 33) && 
						((r_ptr->flags1 & (1L << (d_info[i].r_flag - 1))) != 0)) good_chance = TRUE;

					else if ((d_info[i].r_flag >= 33) && 
						(d_info[i].r_flag < 65) && 
						((r_ptr->flags2 & (1L << (d_info[i].r_flag - 33))) != 0)) good_chance = TRUE;

					else if ((d_info[i].r_flag >= 65) && 
						(d_info[i].r_flag < 97) && 
						((r_ptr->flags3 & (1L << (d_info[i].r_flag - 65))) != 0)) good_chance = TRUE;

					else if ((d_info[i].r_flag >= 97) && 
						(d_info[i].r_flag < 129) && 
						((r_ptr->flags4 & (1L << (d_info[i].r_flag - 97))) != 0)) good_chance = TRUE;

					else if ((d_info[i].r_flag >= 129) && 
						(d_info[i].r_flag < 161) && 
						((r_ptr->flags5 & (1L << (d_info[i].r_flag - 129))) != 0)) good_chance = TRUE;

					else if ((d_info[i].r_flag >= 161) && 
						(d_info[i].r_flag < 193) && 
						((r_ptr->flags6 & (1L << (d_info[i].r_flag - 161))) != 0)) good_chance = TRUE;

					else if ((d_info[i].r_flag >= 193) && 
						(d_info[i].r_flag < 225) && 
						((r_ptr->flags7 & (1L << (d_info[i].r_flag - 193))) != 0)) good_chance = TRUE;

					else if ((d_info[i].r_flag >= 225) && 
						(d_info[i].r_flag < 257) && 
						((r_ptr->flags8 & (1L << (d_info[i].r_flag - 225))) != 0)) good_chance = TRUE;

					else if ((d_info[i].r_flag >= 257) && 
						(d_info[i].r_flag < 289) && 
						((r_ptr->flags9 & (1L << (d_info[i].r_flag - 257))) != 0)) good_chance = TRUE;
				}
			}

			/* Good chance */
			if (good_chance)
			{
				/* Set chance */
				chance = d_info[i].chance;

				/* Improve match if placing objects or features */
				if ((match < MATCH_CHANCE) && ((d_info[i].feat) || (d_info[i].tval)))
				{
					count = 0;
					match = MATCH_CHANCE;
				}
			}

			/* Themes */
			if (dun->theme_feat)
			{
				int k;
				bool theme_match = FALSE;
				bool any_match = FALSE;
				
				for (k = 0; k < MAX_THEMES; k++)
				{
					if (d_info[i].theme[k])
					{
						any_match = TRUE;
					}
					
					if (d_info[i].theme[k] == dun->theme_feat)
					{
						theme_match = TRUE;
					}
				}

				/* Chance to match a theme */
				if (any_match)
				{
					/* Matched a theme */
					if (theme_match)
					{
						/* Must choose a matching theme for this section */
						if (match < MATCH_THEME)
						{
							chance = 0;
							match = MATCH_THEME;
						}
					}
					/* Already has a match to a theme in this section, and this choice doesn't match */
					else if (match == MATCH_THEME)
					{
						/* No chance of picking this choice */
						chance = 0;
					}
				}
			}

			/* Default chance */
			if ((!chance) && (match == MATCH_ANY)) chance = d_info[i].not_chance;

			/* Chance of room entry */
			if (chance)
			{
				/* Add to chances */
				count += chance;

				/* Check chance */
				if (rand_int(count) < chance) pick = i;
			}

			/* Increase index */
			i++;
		}

		/* Paranoia -- Have picked any entry? */
		if (pick >= 0)
		{
			/* Set index to choice */
			i = pick;

			/* Save index if we have anything to describe */
			/* Note hack for efficiency */
			if (((((*name) & (PICKED_NAME1)) == 0) && (strlen(d_name + d_info[i].name1) > 0))
				|| ( (((*name) & (PICKED_NAME2)) == 0) && (strlen(d_name + d_info[i].name2) > 0))
				|| (strlen(d_text + d_info[i].text) > 0))
			{
				/* Paranoia */
				if ((room < DUN_ROOMS) && (*j < ROOM_DESC_SECTIONS))
				{
					room_info[room].section[(*j)++] = i;
					
					if (cheat_room) msg_format("Saving room info %d.",i);
				}

				if (strlen(d_name + d_info[i].name1) > 0) *name |= PICKED_NAME1;
				if (strlen(d_name + d_info[i].name2) > 0) *name |= PICKED_NAME2;
			}

			/* Enter the next chart */
			*chart = d_info[i].next;

			/* Branch if required */
			if ((*branch_on) && (*chart == *branch_on))
			{
				/* Set alternate chart */
				*chart = *branch;

				/* Clear branch conditions */
				*branch = 0;
				*branch_on = 0;
			}

			/* Paranoia */
			if (room < DUN_ROOMS)
			{
				int k;
				
				/* Place flags except SEEN or HEARD */
				room_info[room].flags |= (d_info[i].flags & ~(ROOM_SEEN | ROOM_HEARD));

				/* Set room themes */
				for (k = 0; k < MAX_THEMES; k++)
				{
					if (!(room_info[room].theme[k]) && (d_info[i].theme[k])) room_info[room].theme[k] = d_info[i].theme[k];					
				}
			}
		
			/* Get tval */
			if (d_info[i].tval)
			{
				*place_tval = d_info[i].tval;
				*place_min_sval = d_info[i].min_sval;
				*place_max_sval = d_info[i].max_sval;
			}

			/* Get feature */
			if (d_info[i].feat) *place_feat = d_info[i].feat;

			/* Get branch */
			if (d_info[i].branch) *branch = d_info[i].branch;

			/* Get branch condition */
			if (d_info[i].branch_on) *branch_on = d_info[i].branch_on;

			/* Set dungeon theme. */
			if (!(dun->theme_feat))
			{
				int l;
				int k = 0;
				
				/* Try room themes */
				for (l = 0; l < MAX_THEMES; l++)
				{
					if ((d_info[i].theme[l]) && !(rand_int(++k))) dun->theme_feat = d_info[i].theme[l];
				}
				
				/* Also try the feature we are placing */
				if ((*place_feat) && !(rand_int(++k))) dun->theme_feat = *place_feat;
			}

			/* Add flags */
			*place_flag |= d_info[i].p_flag;

			/* Rows and columns exclude mazes and star bursts for next placement */
			if (d_info[i].p_flag & (RG1_ROWS | RG1_COLS)) exclude |= (RG1_STARBURST | RG1_MAZE_DECOR | RG1_MAZE_PATH | RG1_MAZE_WALL);

			/* Don't place yet */
			if ((*place_flag & (RG1_PLACE)) == 0) continue;

			/* Pick objects if needed */
			if (*place_tval)
			{
				/* Set object hooks if required */
				if (*place_tval < TV_GOLD)
				{
					room_info_kind_tval = *place_tval;
					room_info_kind_min_sval = *place_min_sval;
					room_info_kind_max_sval = *place_max_sval;

					get_obj_num_hook = room_info_kind;

					/* Prepare allocation table */
					get_obj_num_prep();

					/* Drop gold */
					*place_flag |= (RG1_HAS_ITEM);
				}
				else
				{
					/* Drop gold */
					*place_flag |= (RG1_HAS_GOLD);
				}
			}

			/* Record streamers for later */
			if ((*place_feat) && (f_info[*place_feat].flags1 & (FF1_STREAMER)) && (dun->stream_n < DUN_MAX_STREAMER))
			{
				int n;
				
				for (n = 0; n < dun->stream_n; n++)
				{
					/* Already added */
					if (dun->streamer[n] == *place_feat) break;
				}

				if (n == dun->stream_n)
				{
					/* Add dungeon streamer */
					dun->streamer[dun->stream_n++] = *place_feat;
				}
			}

			/* Placement */
			placement = TRUE;
			
			break;
		}

		/* Report errors */
		if (pick < 0)
		{
			break;
		}
	}

	/* Finished (chart = 0) */
	return (placement);
}


/*
 * Set room flags.
 *
 * Use the get_room_info function to set room flags only, not place anything.
 * We do this for interesting rooms, vaults, flooded rooms of all types and
 * anywhere else that we do not explicitly generate room contents.
 */
static void set_room_flags(int room, int type, bool light)
{
	int j = 0;

	u32b place_flag = 0L;
	u32b exclude = 0L;

	byte place_tval = 0;
	byte place_min_sval = 0;
	byte place_max_sval = 0;
	s16b place_feat = 0;

	byte name = 0L;

	byte branch = 0;
	byte branch_on = 0;

	/* Exclude light or dark */
	if (light) exclude |= RG1_DARK;
	else exclude |= RG1_LITE;

	/* Get room info */
	while (get_room_info(room, &type, &j, &place_flag, &place_feat, &place_tval, &place_min_sval, &place_max_sval, &branch, &branch_on, &name, exclude))
	{
		/* Don't place anything */
	}
	
	/* Clear object hook */
	if (get_obj_num_hook)
	{
		get_obj_num_hook = NULL;

		/* Prepare allocation table */
		get_obj_num_prep();
	}

	/* Paranoia */
	if (room < DUN_ROOMS)
	{
		/* Type */
		room_info[room].type = type;

		/* Further paranoia */
		if (j < ROOM_DESC_SECTIONS)
		{
			/* Terminate index list */
			room_info[room].section[j] = -1;
		}		
	}
}


typedef s16b pool_type[3];


/*
 *  Get room info for rooms where we do not have any north / south / east / west or clear place to drop items.
 */
static void set_irregular_room_info(int room, int type, bool light, u32b exclude, s16b *feat, s16b *edge, s16b *inner, s16b *alloc, pool_type *pool, int *n_pools)
{
	int j = 0;

	u32b place_flag = 0L;

	byte place_tval = 0;
	byte place_min_sval = 0;
	byte place_max_sval = 0;
	s16b place_feat = 0;

	byte name = 0L;

	byte branch = 0;
	byte branch_on = 0;

	/* Default exclusions */
	exclude |= (RG1_NORTH | RG1_SOUTH | RG1_EAST | RG1_WEST | RG1_HAS_ITEM | RG1_HAS_GOLD |
			RG1_MAZE_PATH | RG1_MAZE_WALL | RG1_MAZE_DECOR | RG1_CHECKER | RG1_ROWS | RG1_COLS |
			RG1_8WAY | RG1_3X3HIDDEN);
	
	/* Hack -- diagnostics */
	if (cheat_room) msg_print("Setting irregular room info.");

	/* Exclude light or dark */
	if (light) exclude |= RG1_DARK;
	else exclude |= RG1_LITE;

	/* Flooded dungeon */
	if (room_info[room].flags & (ROOM_FLOODED))
	{
		/* Flood floor */
		*feat = dun->flood_feat;
		
		/* Surround by edge */
		*edge = f_info[dun->flood_feat].edge;
		
		/* Has a passable feature */
		if ((*edge) && ((f_info[*edge].flags1 & (FF1_MOVE)) != 0))
		{
			room_info[room].flags &= ~(ROOM_FLOODED);
		}
		/* Must bridge otherwise */
		else
		{
			room_info[room].flags &= (ROOM_BRIDGED);			
		}

		/* Get 'flooded' room flags. This is always 'boring'. */
		set_room_flags(room, ROOM_NORMAL, light);
		
		/* Type */
		room_info[room].type = type;

		return;
	}

	/* Get room info */
	else while (get_room_info(room, &type, &j, &place_flag, &place_feat, &place_tval, &place_min_sval, &place_max_sval, &branch, &branch_on, &name,
		exclude))
	{
		/* Place features or items if needed */
		if (place_feat)
		{
			/* Place throughout room, unless scattering */
			if (((place_flag & (RG1_CENTRE)) != 0) &&
				((place_flag & (RG1_ALLOC | RG1_INNER | RG1_OUTER | RG1_STARBURST | RG1_SCATTER | RG1_RANDOM)) == 0))
			{
				/* Hack -- only fill irregular rooms with lake-like terrain, floors or ground */
				if (((f_info[place_feat].flags1 & (FF1_FLOOR)) == 0) &&
					((f_info[place_feat].flags2 & (FF2_LAKE | FF2_RIVER)) == 0) &&
					((f_info[place_feat].flags3 & (FF3_GROUND)) == 0))
				{
					if ((exclude & (RG1_INNER)) == 0)
					{
						place_flag |= (RG1_INNER);
					}
					else if ((exclude & (RG1_ALLOC)) == 0)
					{
						place_flag |= (RG1_ALLOC);
					}
					else if (((exclude & (RG1_RANDOM)) == 0) && ((place_flag & (RG1_SCATTER)) == 0))
					{
						place_flag |= (RG1_RANDOM);
					}

					/* Clear flags */
					place_flag &= ~(RG1_CENTRE);
				}
				/* Fill room with terrain */
				else
				{
					exclude |= RG1_CENTRE;
					*feat = place_feat;
					
					if (((place_flag & (RG1_EDGE)) != 0) && ((place_flag & (RG1_IGNORE_EDGE | RG1_BRIDGE_EDGE)) != 0))
					{
						exclude |= RG1_EDGE;
						*edge = place_feat;
	
						if ((place_flag & (RG1_BRIDGE_EDGE)) != 0)
						{
							if (f_info[*feat].flags2 & (FF2_BRIDGE))
							{
								/* Bridge previous contents */
								*edge = feat_state(*feat, FS_BRIDGE);
							}
							/* Apply tunnel */
							else if (f_info[*feat].flags1 & (FF1_TUNNEL))
							{
								/* Tunnel previous contents */
								*edge = feat_state(*feat, FS_TUNNEL);
							}
						}
					}
				}
			}

			if ((place_flag & (RG1_ALLOC | RG1_SCATTER)) != 0)
			{
				exclude |= (RG1_ALLOC | RG1_SCATTER);
				*alloc = place_feat;
				
				place_flag &= ~(RG1_INNER | RG1_STARBURST | RG1_OUTER | RG1_EDGE | RG1_SCATTER | RG1_RANDOM);
			}

			if ((place_flag & (RG1_INNER | RG1_STARBURST)) != 0)
			{
				exclude |= RG1_INNER | RG1_STARBURST;
				*inner = place_feat;
				
				/* Hack -- inner and outer --> inner only */
				place_flag &= ~(RG1_OUTER);
			}

			if ((place_flag & (RG1_EDGE)) != 0)
			{				
				exclude |= RG1_EDGE;
				*edge = place_feat;
			}

			if ((place_flag & (RG1_OUTER)) != 0)
			{				
				exclude |= RG1_OUTER;
				room_info[room].theme[THEME_SOLID] = place_feat;
			}

			if ((place_flag & (RG1_RANDOM)) != 0)
			{
				if (*n_pools >= 2) exclude |= RG1_RANDOM;
				*pool[(*n_pools)++] = place_feat;
			}
		}
	}

	/* Clear object hook */
	if (get_obj_num_hook)
	{
		get_obj_num_hook = NULL;

		/* Prepare allocation table */
		get_obj_num_prep();
	}
	
	/* Paranoia */
	if (room < DUN_ROOMS)
	{
		/* Type */
		room_info[room].type = type;

		/* Further paranoia */
		if (j < ROOM_DESC_SECTIONS)
		{
			/* Terminate index list */
			room_info[room].section[j] = -1;
		}
	}
}


/* Hack - spill light out of the edge of a room */
static void check_windows_y(int y1, int y2, int x)
{
	int y;
	
	for (y = y1; y <= y2; y++)
	{
		if (f_info[cave_feat[y][x]].flags1 & (FF1_LOS))
			cave_info[y][x] |= (CAVE_GLOW);	
	}
}


/* Hack - spill light out of the edge of a room */
static void check_windows_x(int x1, int x2, int y)
{
	int x;
	
	for (x = x1; x <= x2; x++)
	{
		if (f_info[cave_feat[y][x]].flags1 & (FF1_LOS))
			cave_info[y][x] |= (CAVE_GLOW);	
	}	
}


/*
 * Build a room consisting of two overlapping rooms.
 * Get the room description, and place stuff accordingly.
 */
static bool build_overlapping(int room, int type, int y1a, int x1a, int y2a, int x2a,
	int y1b, int x1b, int y2b, int x2b, bool light, int spacing, int scale, int scatter, bool pillars)
{
	int j = 0;
	int l = 1;
	int floor = FEAT_FLOOR;
	int edge = 0;

	u32b place_flag = 0L;
	u32b exclude = 0L;

	byte place_tval = 0;
	byte place_min_sval = 0;
	byte place_max_sval = 0;
	s16b place_feat = 0;

	byte name = 0L;

	byte branch = 0;
	byte branch_on = 0;

	bool try_simple = FALSE;

	int limit = 0;

	/* Make certain the overlapping room does not cross the dungeon edge. */
	if ((!in_bounds_fully(y1a, x1a)) || (!in_bounds_fully(y1b, x1b))
		 || (!in_bounds_fully(y2a, x2a)) || (!in_bounds_fully(y2b, x2b))) return (FALSE);

	/* Try 'simple' room */
	if ((y1a == y2a) && (y1b == y2b) && (x1a == x2a) && (x1b == x2b)) try_simple = TRUE;

	/* Flood dungeon if required */
	if (room_info[room].flags & (ROOM_FLOODED))
	{
		floor = dun->flood_feat;
		edge = f_info[dun->flood_feat].edge;

		if ((!edge) || ((f_info[edge].flags1 & (FF1_MOVE)) == 0))
		{
			room_info[room].flags |= (ROOM_BRIDGED);
		}
		else
		{
			/* Keep edge around edge of room */
			l = 2;
			
			room_info[room].flags |= (ROOM_EDGED);
		}
	}

	/* Exclude light or dark */
	if (light) exclude |= RG1_DARK;
	else exclude |= RG1_LITE;

	/* Generate new room (a) */
	generate_room(y1a, x1a, y2a, x2a, light);

	/* Generate new room (b) */
	generate_room(y1b, x1b, y2b, x2b, light);

	/* Generate outer walls (a) */
	generate_rect(y1a, x1a, y2a, x2a, FEAT_WALL_OUTER);

	/* Generate outer walls (b) */
	generate_rect(y1b, x1b, y2b, x2b, FEAT_WALL_OUTER);

	/* Make corners solid */
	cave_set_feat(y1a, x1a, FEAT_WALL_SOLID);
	cave_set_feat(y2a, x2a, FEAT_WALL_SOLID);
	cave_set_feat(y1a, x2a, FEAT_WALL_SOLID);
	cave_set_feat(y2a, x1a, FEAT_WALL_SOLID);
	cave_set_feat(y1b, x1b, FEAT_WALL_SOLID);
	cave_set_feat(y2b, x2b, FEAT_WALL_SOLID);
	cave_set_feat(y1b, x2b, FEAT_WALL_SOLID);
	cave_set_feat(y2b, x1b, FEAT_WALL_SOLID);

	/* Generate edge if required */
	if (edge)
	{
		/* Generate outer walls (a) */
		generate_rect(y1a + 1, x1a + 1, y2a - 1, x2a - 1, edge);

		/* Generate outer walls (b) */
		generate_rect(y1b + 1, x1b + 1, y2b - 1, x2b - 1, edge);
	}
	
	/* Generate inner floors (a) */
	generate_fill(y1a+l, x1a+l, y2a-l, x2a-l, FEAT_FLOOR);

	/* Generate inner floors (b) */
	generate_fill(y1b+l, x1b+l, y2b-l, x2b-l, FEAT_FLOOR);
	
	/* Generate inner floors (a) */
	generate_fill_pillars(y1a+l, x1a+l, y2a-l, x2a-l, floor, pillars ? spacing + 1: 0, scale);

	/* Generate inner floors (b) */
	generate_fill_pillars(y1b+l, x1b+l, y2b-l, x2b-l, floor, pillars ? spacing + 1: 0, scale);
	
	/* Flooded dungeon */
	if (room_info[room].flags & (ROOM_FLOODED))
	{
		/* Has edge we can move around. Treat as not flooded. */
		if (room_info[room].flags & (ROOM_EDGED)) room_info[room].flags &= ~(ROOM_FLOODED);
		
		/* Get 'flooded' room flags. This is always 'boring'. */
		set_room_flags(room, ROOM_NORMAL, light);
		
		/* Type */
		room_info[room].type = type;

		return (TRUE);
	}

	/* Get room info */
	else while (get_room_info(room, &type, &j, &place_flag, &place_feat, &place_tval, &place_min_sval, &place_max_sval, &branch, &branch_on, &name, exclude))
	{
		if (limit++ > 1000)
		{
			msg_format("Error: Keep trying to place stuff with chart item %d. Please report.", j);
			return (FALSE);
		}

		/* Place features or items if needed */
		if ((place_feat) || (place_tval))
		{
			int dy = ((place_flag & (RG1_ROWS)) != 0) ? 1 + spacing : 1;
			int dx = ((place_flag & (RG1_COLS)) != 0) ? 1 + spacing : 1;
			int outer = ((place_flag & (RG1_OUTER)) == 0) ? 1 : 0;
			
			int placements = 1;

			/* Place in centre of room */
			if ((place_flag & (RG1_CENTRE)) != 0)
			{
				int y1c = MAX(y1a, y1b) + 1;
				int y2c = MIN(y2a, y2b) - 1;
				int x1c = MAX(x1a, x1b) + 1;
				int x2c = MIN(x2a, x2b) - 1;
				u32b place_flag_temp = place_flag;

				/* Simple room - we can fill it completely */
				if ((try_simple) && ((place_flag & (RG1_NORTH | RG1_SOUTH | RG1_EAST | RG1_WEST)) == 0))
				{
					/* Expand edges */
					y1c--; y2c++; x1c--; x2c++;

					exclude |= (RG1_NORTH | RG1_SOUTH | RG1_EAST | RG1_WEST);
				}
				
				/* Fill all of vertical room */
				else if (((place_flag & (RG1_NORTH | RG1_SOUTH)) != 0) && ((place_flag & (RG1_WEST | RG1_EAST)) == 0))
				{
					if (place_flag & (RG1_NORTH)) y1c = MIN(y1a, y1b) + outer;
					if (place_flag & (RG1_SOUTH)) y2c = MAX(y2a, y2b) - outer;
						
					place_flag &= ~(RG1_NORTH | RG1_SOUTH);
				}
								
				/* Fill all of horizontal room */
				else if (((place_flag & (RG1_WEST | RG1_EAST)) != 0) && ((place_flag & (RG1_NORTH | RG1_SOUTH)) == 0))
				{
					if (place_flag & (RG1_WEST)) x1c = MIN(x1a, x1b) + outer;
					if (place_flag & (RG1_EAST)) x2c = MAX(x2a, x2b) - outer;
						
					place_flag &= ~(RG1_WEST | RG1_EAST);
				}

				/* Ensure some space */
				if (y1c >= y2c) { y1c = y2c - 1; y2c = y1c + 3;}
				if (x1c >= x2c) { x1c = x2c - 1; x2c = x1c + 3;}

				/* Hack -- 'outer' walls do not exist in centre of room */
				if ((place_flag_temp & (RG1_OUTER)) != 0)
				{
					place_flag_temp &= ~(RG1_OUTER);
					place_flag_temp |= (RG1_INNER);
				}

				/* Count other placements. This reduces the number of items found in the room */
				if ((place_flag & (RG1_WEST)) != 0) placements++;
				if ((place_flag & (RG1_EAST)) != 0) placements++;
				if ((place_flag & (RG1_NORTH)) != 0) placements++;
				if ((place_flag & (RG1_SOUTH)) != 0) placements++;				
				
				generate_patt(y1c, x1c, y2c, x2c, place_feat, place_flag_temp, exclude, dy, dx, scale, (scatter + placements - 1) / placements);
			}
			
			/* Hack -- we've already counted for centre */
			else
			{
				placements--;
				
				if ((place_flag & (RG1_WEST)) != 0) placements++;
				if ((place_flag & (RG1_EAST)) != 0) placements++;
				if ((place_flag & (RG1_NORTH)) != 0) placements++;
				if ((place_flag & (RG1_SOUTH)) != 0) placements++;				
			}

			/* Place in west of room */
			if ((place_flag & (RG1_WEST)) != 0)
			{
				int y1w = (x1a < x1b ? y1a : (x1a == x1b ? MIN(y1a, y1b) : y1b));
				int y2w = (x1a < x1b ? y2a : (x1a == x1b ? MAX(y2a, y2b) : y2b));
				int x1w = MIN(x1a, x1b) + outer;
				int x2w = (x1a == x1b ? x1a + 1 : MAX(x1a, x1b) - 1);
				
				/* No longer simple */
				try_simple = FALSE;

				/* Ensure some space */
				if (x2w <= x2w) x2w = x1w + 1;
				generate_patt(y1w, x1w, y2w, x2w, place_feat, place_flag, exclude, dy, dx, scale, (scatter + placements - 1) / placements);
			}

			/* Place in east of room */
			if ((place_flag & (RG1_EAST)) != 0)
			{
				int y1e = (x2a > x2b ? y1a : (x1a == x1b ? MIN(y1a, y1b): y1b));
				int y2e = (x2a > x2b ? y2a : (x1a == x1b ? MAX(y2a, y2b): y2b));
				int x1e = (x2a == x2b ? x2a - 1 : MIN(x2a, x2b) + 1);
				int x2e = MAX(x2a, x2b) - outer;

				/* No longer simple */
				try_simple = FALSE;

				/* Ensure some space */
				if (x1e >= x2e) x1e = x2e - 1;

					/* Draw from east to west */
					generate_patt(y1e, x2e, y2e, x1e, place_feat, place_flag, exclude, dy, -dx, scale, (scatter + placements - 1) / placements);
			}

			/* Place in north of room */
			if ((place_flag & (RG1_NORTH)) != 0)
			{
				int y1n = MIN(y1a, y1b) + outer;
				int y2n = (y1a == y1b ? y1a + 1 : MAX(y1a, y1b) - 1);
				int x1n = (y1a < y1b ? x1a : (y1a == y1b ? MIN(x1a, x1b): x1b));
				int x2n = (y1a < y1b ? x2a : (y1a == y1b ? MAX(x2a, x2b): x2b));

				/* No longer simple */
				try_simple = FALSE;

				/* Ensure some space */
				if (y2n <= y1n) y2n = y1n + 1;

				generate_patt(y1n, x1n, y2n, x2n, place_feat, place_flag, exclude, dy, dx, scale, (scatter + placements - 1) / placements);
			}

			/* Place in south of room */
			if ((place_flag & (RG1_SOUTH)) != 0)
			{
				int y1s = (y2a == y2b ? y2a - 1 : MIN(y2a, y2b) + 1);
				int y2s = MAX(y2a, y2b) - outer;
				int x1s = (y2a > y2b ? x1a : (y2a == y2b ? MIN(x1a, x1b): x1b));
				int x2s = (y2a > y2b ? x2a : (y2a == y2b ? MAX(x2a, x2b): x2b));

				/* No longer simple */
				try_simple = FALSE;

				/* Ensure some space */
				if (y1s >= y2s) y1s = y2s - 1;

				/* Draw from south to north */
				generate_patt(y2s, x1s, y1s, x2s, place_feat, place_flag, exclude, -dy, dx, scale, (scatter + placements - 1) / placements);
			}
		}

		/* Clear object hook */
		if (place_tval)
		{
			get_obj_num_hook = NULL;

			/* Prepare allocation table */
			get_obj_num_prep();
		}		
	}

	/* Clear object hook */
	if (get_obj_num_hook)
	{
		get_obj_num_hook = NULL;

		/* Prepare allocation table */
		get_obj_num_prep();
	}
	
	/* Hack - spill light out of room */
	if (light)
	{
		check_windows_y(y1a, y2a, x1a);
		check_windows_y(y1a, y2a, x2a);
		check_windows_x(x1a, x2a, y1a);
		check_windows_x(x1a, x2a, y2a);	
		check_windows_y(y1b, y2b, x1b);
		check_windows_y(y1b, y2b, x2b);
		check_windows_x(x1b, x2b, y1b);
		check_windows_x(x1b, x2b, y2b);			
	}

	/* Paranoia */
	if (room < DUN_ROOMS)
	{
		/* Type */
		room_info[room].type = type;

		/* Further paranoia */
		if (j < ROOM_DESC_SECTIONS)
		{
			/* Terminate index list */
			room_info[room].section[j] = -1;
		}
	}

	return(TRUE);
}




/* Maximum size of a fractal map */
#define MAX_FRACTAL_SIZE 65

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

	map[8][8] = (!rand_int(15) ? FRACTAL_POOL_1: FRACTAL_FLOOR);
	map[8][16] = (!rand_int(15) ? FRACTAL_POOL_2: FRACTAL_FLOOR);
	map[8][24] = (!rand_int(15) ? FRACTAL_POOL_3: FRACTAL_FLOOR);
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

	map[16][16] = (!rand_int(4) ? FRACTAL_POOL_1: FRACTAL_FLOOR);
	map[16][32] = (!rand_int(3) ? FRACTAL_POOL_2: FRACTAL_FLOOR);
	map[16][48] = (!rand_int(4) ? FRACTAL_POOL_3: FRACTAL_FLOOR);

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
	bool flip_h = !rand_int(2);

	/* Borders */
	fractal_draw_borders(map, t_ptr);

	if (!rand_int(15)) map[8][flip_h ? 24: 8] = FRACTAL_FLOOR;

	map[16][16] = FRACTAL_FLOOR;

	if (!rand_int(15)) map[24][flip_h ? 8: 24] = FRACTAL_FLOOR;
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
 * Ensures that the fractal map has at least n_pools differing pool types (at most 3).
 * 
 * Ensures that the pools it does have are sequentially numbered from 1 up
 */
static bool fractal_map_has_pools(fractal_map map, fractal_template *t_ptr, int n_pools)
{
	byte pools = 0;
	int x, y;
	
	/* Basic cases */
	if ((n_pools < 1) && (n_pools > 3)) return (TRUE);

	/* Process the whole map */
	for (y = 0; y < t_ptr->size; y++)
	{
		for (x = 0; x < t_ptr->size; x++)
		{
			/* Count different pool types */
			if ((map[y][x] >= FRACTAL_POOL_1) && ((pools & (1 << ((map[y][x]) - FRACTAL_POOL_1))) == 0))
			{
				pools |= 1 << ((map[y][x]) - FRACTAL_POOL_1);
				n_pools--;
			}
		}
	}

	/* Check that pools are sequentially numbered */
	if (!pools || (pools == 1) || (pools == 3) || (pools == 7)) return ((n_pools <= 0));
	
	/* Process the whole map */
	for (y = 0; y < t_ptr->size; y++)
	{
		for (x = 0; x < t_ptr->size; x++)
		{			
			/* Missing pool 2 */
			if ((map[y][x] >= FRACTAL_POOL_3) && (!(pools & 1)))
			{
				map[y][x]--;
			}							

			/* Missing pool 1 */
			if ((map[y][x] >= FRACTAL_POOL_2) && (!(pools & 1)))
			{
				map[y][x]--;
			}
		
		}
	}
	
	return (TRUE);
}



/*
 * Construct a fractal room given a fractal map and the room center's coordinates.
 */
static void fractal_map_to_room(fractal_map map, byte fractal_type, int y0, int x0, bool light, s16b floor, s16b wall, pool_type pool)
{
	int x, y, y1, x1, wid, hgt;

	/* Get the dimensions of the fractal map */
	hgt = fractal_dim[fractal_type].hgt;
	wid = fractal_dim[fractal_type].wid;

	/* Get top-left coordinate */
	y1 = y0 - hgt / 2;
	x1 = x0 - wid / 2;

	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;

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
				s16b feat = FEAT_NONE;

				/* Pool grid */
				if (grid_type == FRACTAL_POOL_1)
				{
					/* Use the pool feature */
					feat = pool[0];
				}
				/* Pool grid */
				else if (grid_type == FRACTAL_POOL_2)
				{
					/* Use the pool feature */
					feat = pool[1];
				}
				/* Pool grid */
				else if (grid_type == FRACTAL_POOL_3)
				{
					/* Use the pool feature */
					feat = pool[2];
				}

				/* Place the selected pool feature */
				if (feat != FEAT_NONE)
				{
					int tmp_feat = feat;

					/* Hack -- make variable terrain surrounded by floors */
					if (((f_info[cave_feat[yy][xx]].flags1 & (FF1_WALL)) != 0) &&
						(variable_terrain(&tmp_feat,feat))) cave_set_feat(yy,xx,FEAT_FLOOR);

					build_terrain(yy, xx, tmp_feat);
				}
				/* Or place a floor */
				else
				{
					int tmp_feat = floor;

					/* Hack -- make variable terrain surrounded by floors */
					if (((f_info[cave_feat[yy][xx]].flags1 & (FF1_WALL)) != 0) &&
						(variable_terrain(&tmp_feat,floor))) cave_set_feat(yy,xx,FEAT_FLOOR);

					build_terrain(yy, xx, tmp_feat);
				}
				
				/* Mark the grid as a part of the room */
				cave_info[yy][xx] |= (CAVE_ROOM);
			}
			else if ((grid_type == FRACTAL_EDGE) && (wall))
			{
				cave_set_feat(yy, xx, wall);

				/* Mark the grid as a part of the room */
				cave_info[yy][xx] |= (CAVE_ROOM);			

				/* Allow light to spill out of rooms through transparent edges */
				if (light)
				{
					/* Allow lighting up rooms to work correctly */
					if (f_info[cave_feat[yy][xx]].flags1 & (FF1_LOS))
					{
						int d;
						
						/* Look in all directions. */
						for (d = 0; d < 8; d++)
						{
							/* Extract adjacent location */
							int yyy = yy + ddy_ddd[d];
							int xxx = xx + ddx_ddd[d];
							
							/* Hack -- light up outside room */
							cave_info[yyy][xxx] |= (CAVE_GLOW);
						}
					}
				}
			}
			else if ((grid_type == FRACTAL_EDGE) && !(wall) && (light))
			{
				/* Hack -- light up outside room */
				cave_info[yy][xx] |= (CAVE_GLOW);
			}
			else
			{
				continue;
			}

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
			if ((map[y][x] != map2[y][x]) && !rand_int(4)) map[y][x] = map2[y][x];
		}
	}

	/* Free resources */
	FREE(map2);
}


/*
 * Build a fractal room given its center. Returns TRUE on success.
 */
static bool build_type_fractal(int room, int chart, int y0, int x0, byte type, bool light)
{
	fractal_map map;
	fractal_template *t_ptr;
	int tries;
	bool do_merge = FALSE;

	/* Default floor and edge */
	s16b feat = FEAT_FLOOR;
	s16b edge = FEAT_WALL_OUTER;
	s16b inner = FEAT_NONE;
	s16b alloc = FEAT_NONE;
	u32b exclude = (RG1_INNER | RG1_STARBURST);
	
	pool_type pool;

	int n_pools = 0;

	int i;

	/* Paranoia */
	if (type >= MAX_FRACTAL_TYPES) return (FALSE);
	
	/* Set irregular room info */
	set_irregular_room_info(room, chart, light, exclude, &feat, &edge, &inner, &alloc, &pool, &n_pools);

	/* Clear remaining pools */
	for (i = n_pools; i < 3; i++)
	{
		if (room_info[room].flags & (ROOM_FLOODED))
		{
			pool[i] = pick_proper_feature(cave_feat_pool);
		}
		else
		{
			pool[i] = FEAT_NONE;
		}
		
		/* Mark room if feat is not passable */
		if ((f_info[pool[i]].flags1 & (FF1_MOVE)) == 0)
		{
			/* Bridge into room */
			room_info[room].flags |= (ROOM_BRIDGED);
		}
	}

	/* Mark room if feat is not passable */
	if ((f_info[feat].flags1 & (FF1_MOVE)) == 0)
	{
		/* Bridge into room */
		room_info[room].flags |= (ROOM_BRIDGED);
	}

	/* Mark room if edge is not 'outer' */
	if ((f_info[edge].flags1 & (FF1_OUTER)) == 0)
	{
		/* Bridge into room */
		room_info[room].flags |= (ROOM_EDGED);
	}
	
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
	if ((type == FRACTAL_TYPE_33x33) && rand_int(3)) do_merge = TRUE;

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
		if ((fractal_map_is_connected(map, t_ptr)) && (fractal_map_has_pools(map, t_ptr, n_pools))) break;
		
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
	fractal_map_to_room(map, type, y0, x0, light, feat, edge, pool);

	/* Free resources */
	FREE(map);

	/* Hack -- place feature at centre */
	if (alloc)
	{
		alloc = pick_room_feat(alloc);
		
		cave_set_feat(y0, x0, alloc);

		for (i = 0; i < 8; i++)
		{
			int y = y0 + ddy_ddd[i];
			int x = x0 + ddx_ddd[i];
			
			/* Don't overwrite anything interesting */
			if ((cave_feat[y][x] != FEAT_FLOOR) && (cave_feat[y][x] != FEAT_WALL_OUTER) && (cave_feat[y][x] != FEAT_WALL_SOLID)) continue;

			if (rand_int(100) < (i >= 4 ? 40 : 20)) continue;
			cave_set_feat(y, x, alloc);
		}
	}

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
			if (room_has_flag(yy, xx, ROOM_ICKY)) continue;

			/* Ignore some walls, and permanent features */
			if ((f_info[cave_feat[yy][xx]].flags1 & (FF1_OUTER |
				FF1_SOLID | FF1_PERMANENT)) != 0) continue;

			/* Set the feature */
			build_terrain(yy, xx, feat);
		}
	}

	/* Free resources */
	FREE(map);

	/* Success */
	return (TRUE);
}


static void build_type_starburst(int room, int type, int y0, int x0, int dy, int dx, bool light)
{
	bool want_pools = (rand_int(150) < p_ptr->depth);

	bool giant_room = (dy >= 19) && (dx >= 33);

	/* Default terrain */
	s16b feat = FEAT_FLOOR;
	s16b edge = FEAT_WALL_OUTER;
	s16b inner = FEAT_NONE;
	s16b alloc = FEAT_NONE;
	s16b pool[3];

	int n_pools = 0;

	/* Default flags, classic rooms */
	u32b flag = (room_info[room].flags & (ROOM_FLOODED)) != 0 ? 0L : (STAR_BURST_RAW_EDGE);

	/* Set irregular room info */
	set_irregular_room_info(room, type, light, 0L, &feat, &edge, &inner, &alloc, &pool, &n_pools);

	/* Have pool contents */
	if (n_pools) want_pools = TRUE;

	/* Occasional light */
	if (light) flag |= (STAR_BURST_LIGHT);
	
	/* Mark as room */
	if (room) flag |= (STAR_BURST_ROOM);

	/* No floor - need inner room */
	if ((f_info[feat].flags1 & (FF1_MOVE)) == 0)
	{
		/* Floor */
		if (!inner) inner = FEAT_FLOOR;

		/* Hack -- no longer a room - but note we add flag later for 'inner' room */
		flag &= ~(STAR_BURST_ROOM);
	}

	/* Mark room if feat is not passable */
	if ((f_info[feat].flags1 & (FF1_MOVE)) == 0)
	{
		/* Bridge into room */
		room_info[room].flags |= (ROOM_BRIDGED);
	}

	/* Mark room if edge is not 'outer' */
	if ((edge) && ((f_info[edge].flags1 & (FF1_MOVE)) == 0))
	{
		/* Bridge into room */
		room_info[room].flags |= (ROOM_EDGED);
	}

	/* Case 1. Plain starburst room */
	if (!(inner) && (rand_int(100) < 75))
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
		if ((inner) || (!rand_int(2)))
		{
			if (!inner) inner = edge;

			/* Classic inner room (unless whole thing is not a room) */
			if (((f_info[inner].flags1 & (FF1_OUTER)) != 0) && ((room) || (flag & STAR_BURST_ROOM)))
			{
				feat = feat_state(inner, FS_INNER);
			}
			else
			{
				feat = inner;
			}

			/* No edge */
			edge = FEAT_NONE;
		}

		/* Adjust the size of the inner room */
		if ((f_info[edge].flags1 & (FF1_WALL)) != 0)
		{
			dy /= 4;
			dx /= 4;
		}
		else
		{
			dy /= 3;
			dx /= 3;
		}

		/* Minimum size */
		if (dy < 1) dy = 1;
		if (dx < 1) dx = 1;
		
		generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
			feat, edge, flag | (room ? STAR_BURST_ROOM : 0L));
	}

	/* Build pools */
	if (want_pools)
	{
		int i, range;

		/* Haven't allocated pools */
		if (!n_pools)
		{
			/* Randomize the number of pools */
			n_pools = randint(2);

			/* Adjust for giant rooms */
			if (giant_room) n_pools += 1;

			/* Place the pools */
			for (i = 0; i < n_pools; i++)
			{
				/* Choose a feature for the pool */
				pool[i] = pick_proper_feature(cave_feat_pool);

				/* Got none */
				if (!pool[i])
				{
					i--;
					n_pools--;
					continue;
				}
			}
		}

		/* How far of room center? */
		range = giant_room ? 12: 5;

		/* Force the selection of a new feature */
		feat = FEAT_NONE;

		/* Place the pools */
		for (i = 0; i < n_pools; i++)
		{
			int tries;

			for (tries = 0; tries < 2500; tries++)
			{
				/* Get the center of the pool */
				int y = rand_spread(y0, range);
				int x = rand_spread(x0, range);

				/* Verify center */
				if (cave_feat[y][x] == feat)
				{
					build_pool(y, x, pool[i], giant_room);

					/* Mark room */
					if ((f_info[pool[i]].flags1 & (FF1_MOVE)) == 0) room_info[room].flags |= (ROOM_BRIDGED);

					/* Done */
					break;
				}
			}
		}
	}

	/* Hack -- place feature at centre */
	if (alloc)
	{
		int i;
		
		alloc = pick_room_feat(alloc);
		
		cave_set_feat(y0, x0, alloc);

		if ((dy > 1) && (dx > 1)) for (i = 0; i < 8; i++)
		{
			int y = y0 + ddy_ddd[i];
			int x = x0 + ddx_ddd[i];
			
			/* Don't overwrite anything interesting */
			if ((cave_feat[y][x] != FEAT_FLOOR) && (cave_feat[y][x] != FEAT_WALL_OUTER) && (cave_feat[y][x] != FEAT_WALL_SOLID)) continue;

			if (rand_int(100) < (i >= 4 ? 40 : 20)) continue;
			cave_set_feat(y, x, alloc);
		}
	}
}




/*
 * Helper function to build chambers.  Fill a room matching
 * the rectangle input with a 'wall type', and surround it with inner wall.
 * Create a door in a random inner wall grid along the border of the
 * rectangle.
 *
 * XXX We may replace FEAT_MAGMA in Leon's original algorithm with a specified
 * 'wall feature'.
 */
static void make_chamber(int c_y1, int c_x1, int c_y2, int c_x2)
{
	int i, d, y, x;
	int count;

	/* Fill with soft granite (will later be replaced with floor). */
	generate_fill(c_y1+1, c_x1+1, c_y2-1, c_x2-1, FEAT_MAGMA);

	/* Generate inner walls over dungeon granite and magma. */
	for (y = c_y1; y <= c_y2; y++)
	{
		/* left wall */
		x = c_x1;

		if ((cave_feat[y][x] == FEAT_WALL_EXTRA) ||
			(cave_feat[y][x] == FEAT_MAGMA))
			cave_set_feat(y, x, FEAT_WALL_INNER);
	}

	for (y = c_y1; y <= c_y2; y++)
	{
		/* right wall */
		x = c_x2;

		if ((cave_feat[y][x] == FEAT_WALL_EXTRA) ||
			(cave_feat[y][x] == FEAT_MAGMA))
			cave_set_feat(y, x, FEAT_WALL_INNER);
	}

	for (x = c_x1; x <= c_x2; x++)
	{
		/* top wall */
		y = c_y1;

		if ((cave_feat[y][x] == FEAT_WALL_EXTRA) ||
			(cave_feat[y][x] == FEAT_MAGMA))
			cave_set_feat(y, x, FEAT_WALL_INNER);
	}

	for (x = c_x1; x <= c_x2; x++)
	{
		/* bottom wall */
		y = c_y2;

		if ((cave_feat[y][x] == FEAT_WALL_EXTRA) ||
			(cave_feat[y][x] == FEAT_MAGMA))
			cave_set_feat(y, x, FEAT_WALL_INNER);
	}

	/* Try a few times to place a door. */
	for (i = 0; i < 20; i++)
	{
		/* Pick a square along the edge, not a corner. */
		if (!rand_int(2))
		{
			/* Somewhere along the (interior) side walls. */
			if (!rand_int(2)) x = c_x1;
			else x = c_x2;
			y = c_y1 + rand_int(1 + ABS(c_y2 - c_y1));
		}
		else
		{
			/* Somewhere along the (interior) top and bottom walls. */
			if (!rand_int(2)) y = c_y1;
			else y = c_y2;
			x = c_x1 + rand_int(1 + ABS(c_x2 - c_x1));
		}

		/* If not an inner wall square, try again. */
		if (cave_feat[y][x] != FEAT_WALL_INNER) continue;

		/* Paranoia */
		if (!in_bounds_fully(y, x)) continue;

		/* Reset wall count */
		count = 0;

		/* If square has not more than two adjacent walls, and no adjacent doors, place door. */
		for (d = 0; d < 9; d++)
		{
			/* Extract adjacent (legal) location */
			int yy = y + ddy_ddd[d];
			int xx = x + ddx_ddd[d];

			/* No doors beside doors. */
			if (cave_feat[yy][xx] == FEAT_OPEN) break;

			/* Count the inner walls. */
			if (cave_feat[yy][xx] == FEAT_WALL_INNER) count++;

			/* No more than two walls adjacent (plus the one we're on). */
			if (count > 3) break;

			/* Checked every direction? */
			if (d == 8)
			{
				/* Place an open door. */
				cave_set_feat(y, x, FEAT_OPEN);

				/* Success. */
				return;
			}
		}
	}
}



/*
 * Expand in every direction from a start point, turning magma into rooms.
 * Stop only when the magma and the open doors totally run out.
 */
static void hollow_out_room(int y, int x)
{
	int d, yy, xx;

	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent location */
		yy = y + ddy_ddd[d];
		xx = x + ddx_ddd[d];

		/* Change magma to floor. */
		if (cave_feat[yy][xx] == FEAT_MAGMA)
		{
			cave_set_feat(yy, xx, FEAT_FLOOR);

			/* Hollow out the room. */
			hollow_out_room(yy, xx);
		}
		/* Change open door to broken door. */
		else if (cave_feat[yy][xx] == FEAT_OPEN)
		{
			cave_set_feat(yy, xx, FEAT_BROKEN);

			/* Hollow out the (new) room. */
			hollow_out_room(yy, xx);
		}
	}
}



/*
 * Build chambers
 *
 * Build a room, varying in size between 22x22 and 44x66, consisting of
 * many smaller, irregularly placed, chambers all connected by doors or
 * short tunnels. -LM-
 *
 * Plop down an area-dependent number of magma-filled chambers, and remove
 * blind doors and tiny rooms.
 *
 * Hollow out a chamber near the center, connect it to new chambers, and
 * hollow them out in turn.  Continue in this fashion until there are no
 * remaining chambers within two squares of any cleared chamber.
 *
 * Clean up doors.  Neaten up the wall types.  Turn floor grids into rooms,
 * illuminate if requested.
 *
 * Fill the room with up to 35 (sometimes up to 50) monsters of a creature
 * race or type that offers a challenge at the character's depth.  This is
 * similar to monster pits, except that we choose among a wider range of
 * monsters.
 *
 * Special quest levels modify some of these steps.
 */
static bool build_chambers(int y1, int x1, int y2, int x2, int monsters_left, bool light)
{
	int i;
	int d;
	int area, num_chambers;
	int y, x, yy, xx;
	int yy1, xx1, yy2, xx2, yy3, xx3;

	int count;

	/* Determine how much space we have. */
	area = ABS(y2 - y1) * ABS(x2 - x1);

	/* Calculate the number of smaller chambers to make. */
	num_chambers = 10 + area / 80;

	/* Build the chambers. */
	for (i = 0; i < num_chambers; i++)
	{
		int c_y1, c_x1, c_y2, c_x2;
		int size, w, h;

		/* Determine size of chamber. */
		size = 3 + rand_int(4);
		w = size + rand_int(10);
		h = size + rand_int(4);

		/* Pick an upper-left corner at random. */
		c_y1 = rand_range(y1, y2 - h);
		c_x1 = rand_range(x1, x2 - w);

		/* Determine lower-right corner of chamber. */
		c_y2 = c_y1 + h;
		c_x2 = c_x1 + w;

		/* Make me a (magma filled) chamber. */
		make_chamber(c_y1, c_x1, c_y2, c_x2);
	}

	/* Remove useless doors, fill in tiny, narrow rooms. */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			count = 0;

			/* Stay legal. */
			if (!in_bounds_fully(y, x)) continue;

			/* Check all adjacent grids. */
			for (d = 0; d < 8; d++)
			{
				/* Extract adjacent location */
				yy = y + ddy_ddd[d];
				xx = x + ddx_ddd[d];

				/* Count the walls and dungeon granite. */
				if ((cave_feat[yy][xx] == FEAT_WALL_INNER) ||
					(cave_feat[yy][xx] == FEAT_WALL_EXTRA)) count++;
			}

			/* Five adjacent walls:  Change non-chamber to wall. */
			if ((count == 5) && (cave_feat[y][x] != FEAT_MAGMA))
				cave_set_feat(y, x, FEAT_WALL_INNER);

			/* More than five adjacent walls:  Change anything to wall. */
			else if (count > 5) cave_set_feat(y, x, FEAT_WALL_INNER);
		}
	}

	/* Pick a random magma spot near the center of the room. */
	for (i = 0; i < 50; i++)
	{
		y = y1 + rand_spread((y2 - y1) / 2, (y2 - y1) / 4);
		x = x1 + rand_spread((x2 - x1) / 2, (x2 - x1) / 4);
		if (cave_feat[y][x] == FEAT_MAGMA) break;
	}

	/* Hollow out the first room. */
	cave_set_feat(y, x, FEAT_FLOOR);
	hollow_out_room(y, x);


	/* Attempt to change every in-room magma grid to open floor. */
	for (i = 0; i < 100; i++)
	{
		/* Assume this run will do no useful work. */
		bool joy = FALSE;

		/* Make new doors and tunnels between magma and open floor. */
		for (y = y1; y <= y2; y++)
		{
			for (x = x1; x <= x2; x++)
			{
				/* Current grid must be magma. */
				if (cave_feat[y][x] != FEAT_MAGMA) continue;

				/* Stay legal. */
				if (!in_bounds_fully(y, x)) continue;

				/* Check only horizontal and vertical directions. */
				for (d = 0; d < 4; d++)
				{
					/* Extract adjacent location */
					yy1 = y + ddy_ddd[d];
					xx1 = x + ddx_ddd[d];

					/* Find inner wall. */
					if (cave_feat[yy1][xx1] == FEAT_WALL_INNER)
					{
						/* Keep going in the same direction. */
						yy2 = yy1 + ddy_ddd[d];
						xx2 = xx1 + ddx_ddd[d];

						/* If we find open floor, place a door. */
						if ((in_bounds(yy2, xx2)) && (cave_feat[yy2][xx2] == FEAT_FLOOR))
						{
							joy = TRUE;

							/* Make a broken door in the wall grid. */
							cave_set_feat(yy1, xx1, FEAT_BROKEN);

							/* Hollow out the new room. */
							cave_set_feat(y, x, FEAT_FLOOR);
							hollow_out_room(y, x);

							break;
						}

						/* If we find more inner wall... */
						if ((in_bounds(yy2, xx2)) && (cave_feat[yy2][xx2] == FEAT_WALL_INNER))
						{
							/* ...Keep going in the same direction. */
							yy3 = yy2 + ddy_ddd[d];
							xx3 = xx2 + ddx_ddd[d];

							/* If we /now/ find floor, make a tunnel. */
							if ((in_bounds(yy3, xx3)) && (cave_feat[yy3][xx3] == FEAT_FLOOR))
							{
								joy = TRUE;

								/* Turn both wall grids into floor. */
								cave_set_feat(yy1, xx1, FEAT_FLOOR);
								cave_set_feat(yy2, xx2, FEAT_FLOOR);

								/* Hollow out the new room. */
								cave_set_feat(y, x, FEAT_FLOOR);
								hollow_out_room(y, x);

								break;
							}
						}
					}
				}
			}
		}

		/* If we could find no work to do, stop. */
		if (!joy) break;
	}


	/* Turn broken doors into a random kind of door, remove open doors. */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			if (cave_feat[y][x] == FEAT_OPEN)
				cave_set_feat(y, x, FEAT_WALL_INNER);
			else if (cave_feat[y][x] == FEAT_BROKEN)
				place_random_door(y, x);
		}
	}


	/* Turn all walls and magma not adjacent to floor into dungeon granite. */
	/* Turn all floors and adjacent grids into rooms, sometimes lighting them. */
	for (y = (y1-1 > 0 ? y1-1 : 0) ;
		y < (y2+2 < DUNGEON_HGT ? y2+2 : DUNGEON_HGT) ; y++)
	{
		for (x = (x1-1 > 0 ? x1-1 : 0) ;
			x < (x2+2 < DUNGEON_WID ? x2+2 : DUNGEON_WID) ; x++)
		{
			if ((cave_feat[y][x] == FEAT_WALL_INNER) ||
				(cave_feat[y][x] == FEAT_MAGMA))
			{
				for (d = 0; d < 9; d++)
				{
					/* Extract adjacent location */
					yy = y + ddy_ddd[d];
					xx = x + ddx_ddd[d];

					/* Stay legal */
					if (!in_bounds(yy, xx)) continue;

					/* No floors allowed */
					if (cave_feat[yy][xx] == FEAT_FLOOR) break;

					/* Turn me into dungeon granite. */
					if (d == 8)
					{
						cave_set_feat(y, x, FEAT_WALL_EXTRA);
					}
				}
			}
			if (cave_feat[y][x] == FEAT_FLOOR)
			{
				for (d = 0; d < 9; d++)
				{
					/* Extract adjacent location */
					yy = y + ddy_ddd[d];
					xx = x + ddx_ddd[d];

					/* Stay legal */
					if (!in_bounds(yy, xx)) continue;

					/* Turn into room. */
					cave_info[yy][xx] |= (CAVE_ROOM);

					/* Illuminate if requested. */
					if (light) cave_info[yy][xx] |= (CAVE_GLOW);
				}
			}
		}
	}


	/* Turn all inner wall grids adjacent to dungeon granite into outer walls. */
	for (y = (y1-1 > 0 ? y1-1 : 0) ; y < (y2+2 < DUNGEON_HGT ? y2+2 : DUNGEON_HGT) ; y++)
	{
		for (x = (x1-1 > 0 ? x1-1 : 0) ; x < (x2+2 < DUNGEON_WID ? x2+2 : DUNGEON_WID) ; x++)
		{
			/* Stay legal. */
			if (!in_bounds_fully(y, x)) continue;

			if (cave_feat[y][x] == FEAT_WALL_INNER)
			{
				for (d = 0; d < 9; d++)
				{
					/* Extract adjacent location */
					yy = y + ddy_ddd[d];
					xx = x + ddx_ddd[d];

					/* Look for dungeon granite */
					if (cave_feat[yy][xx] == FEAT_WALL_EXTRA)
					{
						/* Turn me into outer wall. */
						cave_set_feat(y, x, FEAT_WALL_OUTER);

						/* Done */
						break;
					}
				}
			}
		}
	}

	/*** Now we get to place the monsters. ***/

	/* Place the monsters. */
	for (i = 0; i < 300; i++)
	{
		/* Check for early completion. */
		if (!monsters_left) break;

		/* Pick a random in-room square. */
		y = rand_range(y1, y2);
		x = rand_range(x1, x2);

		/* Require a floor square with no monster in it already. */
		if (!cave_naked_bold(y, x)) continue;

		/* Enforce monster selection */
		cave_ecology.use_ecology = room_idx(y, x);
		cave_ecology.single_ecology = TRUE;
		
		/* Place a single monster.  Sleeping 2/3rds of the time. */
		place_monster_aux(y, x, get_mon_num(p_ptr->depth),
			(rand_int(3)), FALSE, 0L);
		
		/* End enforcement of monster selection */
		cave_ecology.single_ecology = FALSE;

		/* One less monster to place. */
		monsters_left--;
	}

	/* Success */
	return (TRUE);
}

/*
 * Place a monster in a vault and get the details. Add these to the ecology.
 */
static void vault_monster(int y, int x)
{
	int i;
	
	place_monster(y, x, TRUE, TRUE);

	if (cave_m_idx[y][x])
	{
		s16b r_idx = m_list[cave_m_idx[y][x]].r_idx;
		bool needed = TRUE;
		
		/* Check if vault monster in ecology */
		for (i = 0; i < cave_ecology.num_races; i++)
		{
			if (cave_ecology.race[i] == r_idx) needed = FALSE;
		}
		
		/* Get monster ecology */
		if (needed)
		{
			get_monster_ecology(r_idx);
		}
	}
}


/*
 * Hack -- fill in "vault" rooms
 */
static void build_vault(int room, int y0, int x0, int ymax, int xmax, cptr data)
{
	int dx, dy, x, y;

	cptr t;

	bool old_ecology = cave_ecology.ready;
	int old_num_ecologies = cave_ecology.num_ecologies;
	int new_num_ecologies = old_num_ecologies;

	/* Allow any monster */
	cave_ecology.ready = FALSE;
	
	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the location */
			x = x0 - (xmax / 2) + dx;
			y = y0 - (ymax / 2) + dy;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Lay down a floor */
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Part of a vault */
			cave_info[y][x] |= (CAVE_ROOM);

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
						place_object(y, x, FALSE, FALSE);
					}
					else
					{
						place_trap(y, x);
					}
					break;
				}

				/* Now trapped/locked doors */
				case '+':
				{
					place_locked_door(y, x);
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
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the grid */
			x = x0 - (xmax/2) + dx;
			y = y0 - (ymax/2) + dy;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Analyze the symbol */
			switch (*t)
			{
				/* Monster */
				case '&':
				{
					monster_level = p_ptr->depth + 5;
					vault_monster(y, x);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster */
				case '@':
				{
					monster_level = p_ptr->depth + 11;
					vault_monster(y, x);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster, plus treasure */
				case '9':
				{
					monster_level = p_ptr->depth + 9;
					vault_monster(y, x);
					monster_level = p_ptr->depth;
					object_level = p_ptr->depth + 7;
					place_object(y, x, TRUE, FALSE);
					object_level = p_ptr->depth;
					break;
				}

				/* Nasty monster and treasure */
				case '8':
				{
					monster_level = p_ptr->depth + 40;
					vault_monster(y, x);
					monster_level = p_ptr->depth;
					object_level = p_ptr->depth + 20;
					place_object(y, x, TRUE, TRUE);
					object_level = p_ptr->depth;
					break;
				}

				/* Monster and/or object */
				case ',':
				{
					if (rand_int(100) < 50)
					{
						monster_level = p_ptr->depth + 3;
						vault_monster(y, x);
						monster_level = p_ptr->depth;
					}
					if (rand_int(100) < 50)
					{
						object_level = p_ptr->depth + 7;
						place_object(y, x, FALSE, FALSE);
						object_level = p_ptr->depth;
					}
					break;
				}
			}
			
			/* Ensure vault has a single ecology */
			if (cave_ecology.num_ecologies > old_num_ecologies)
			{
				cave_ecology.num_ecologies = old_num_ecologies;
				new_num_ecologies = old_num_ecologies + 1;
			}
		}
	}

	/* Vault supports this room */
	room_info[room].ecology = (1L << ++cave_ecology.num_ecologies);
	room_info[room].deepest_race = cave_ecology.deepest_race[cave_ecology.num_ecologies];
	
	/* Ecology */
	cave_ecology.ready = old_ecology;
	cave_ecology.num_ecologies = new_num_ecologies;
}


/*
 * Hack -- fill in "tower" rooms
 *
 * Similar to vaults, but we never place monsters/traps/treasure.
 */
static void build_tower(int y0, int x0, int ymax, int xmax, cptr data)
{
	int dx, dy, x, y, i;

	cptr t;

	int monsters_left = 30;
	
	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the location */
			x = x0 - (xmax / 2) + dx;
			y = y0 - (ymax / 2) + dy;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Lay down a floor */
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Part of a vault */
			cave_info[y][x] |= (CAVE_ROOM);

			/* Hack -- always lite towers */
			cave_info[y][x] |= (CAVE_GLOW);

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
					break;
				}

				/* Now locked doors */
				case '+':
				{
					place_locked_door(y, x);
					break;
				}

				/* Trap */
				case '^':
				{
					break;
				}
			}
		}
	}
	
	/*** Now we get to place the monsters. ***/

	/* Place the monsters. */
	for (i = 0; i < 300; i++)
	{
		/* Check for early completion. */
		if (!monsters_left) break;

		/* Pick a random in-room square. */
		y = rand_range(y0 - ymax, y0 + ymax);
		x = rand_range(x0 - xmax, x0 + xmax);

		/* Require a floor square with no monster in it already. */
		if (!cave_naked_bold(y, x)) continue;

		/* Place a single monster.  Sleeping 2/3rds of the time. */
		place_monster_aux(y, x, get_mon_num(p_ptr->depth),
			(rand_int(3)), FALSE, 0L);
		
		/* One less monster to place. */
		monsters_left--;
	}

}


/*
 * Hack -- fill in "roof"
 *
 * This allows us to stack multiple towers on top of each other, so that
 * a player ascending from one tower to another will not fall.
 */
static void build_roof(int y0, int x0, int ymax, int xmax, cptr data)
{
	int dx, dy, x, y;

	cptr t;

	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the location */
			x = x0 - (xmax / 2) + dx;
			y = y0 - (ymax / 2) + dy;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Lay down a floor */
			cave_set_feat(y, x, FEAT_FLOOR);
		}
	}
}


/*#define ENABLE_WAVES 1 */

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

	/* Join the points using waves */
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
		if ((f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) == 0) continue;

		/* Pick the proper "crest" feature */
		if ((f_info[cave_feat[y][x]].flags2 & (FF2_DEEP)) == 0)
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


/*
 * Room type information
 */

typedef struct lake_size lake_size;

struct lake_size
{
	/* Required size in blocks */
	s16b dy1, dy2, dx1, dx2;

	/* Hack -- minimum level */
	s16b level;
};


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
static const lake_size lake_data[MAX_LAKE_DATA] =
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
static bool build_lake(int feat, bool do_big_lake, bool merge_lakes,
	int *y0, int *x0)
{
	int bx0, by0;
	int bx1, by1, bx2, by2;
	int wid, hgt;
	int tries = 0;
	int lake_idx;
	const lake_size *ld;
	/* Starburst flags */
	u32b flag = 0;
	/*
	 * Notice special cases: these are replaced with passable features
	 * sometimes (build_terrain)
	 */
	bool solid_lake = ((f_info[feat].flags1 & (FF1_WALL)) != 0);

	/* Solid lakes are made very large sometimes */
	if (solid_lake && !rand_int(3)) do_big_lake = TRUE;

	/* Choose an initial size for the lake */
	if (do_big_lake)
	{
		if (!rand_int(10)) lake_idx = LAKE_DATA_4x5;
		else if (!rand_int(5)) lake_idx = LAKE_DATA_4x4;
		else lake_idx = LAKE_DATA_3x4;
	}
	else
	{
		/*
		 * Lakes at shallow levels are smaller, and some at deeper
		 * levels too
		 */
		if ((p_ptr->depth >= 25) && !rand_int(7))
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
		if (lake_idx < 0) return (FALSE);
	}

	/* Try to get a location for the lake */
	while (TRUE)
	{
		/* Too many tries. Reject lake */
		if (++tries >= 75) return (FALSE);

		/* Pick a random block */
		if (!merge_lakes || solid_lake)
		{
			by0 = rand_int(dun->row_rooms);
			bx0 = rand_int(dun->col_rooms);
		}
		/* Force lake overlapping */
		else
		{
			/* Put the lakes in the middle of the level */
			by0 = (dun->row_rooms / 2);

			/* Slightly move the lake horizontally */
			/* Big lakes (-1, +0 or +1 blocks) */
			if (lake_idx > LAKE_DATA_2x3)
			{
				bx0 = (dun->col_rooms / 2) + (rand_int(3) - 1);
			}
			/* Small lakes (+0 or +1 blocks) */
			else
			{
				bx0 = (dun->col_rooms / 2) + rand_int(2);
			}
		}

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
		if (((f_info[feat].flags3 & (FF3_LIVING)) != 0) || 
			(p_ptr->depth <= randint(25)))
		{
			flag |= (STAR_BURST_LIGHT);
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
		feat, f_info[feat].edge, flag);

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
	int edge = f_info[feat].edge;
	/*
	 * Notice special cases: they are replaced by passable features
	 * sometimes (build_terrain)
	 */
	bool solid_river = ((f_info[feat].flags1 & (FF1_WALL)) != 0);

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
		if ((f_info[old_feat].flags1 & (FF1_PERMANENT)) != 0) break;

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
				if ((f_info[old_feat].flags1 & (FF1_PERMANENT)) != 0) continue;

				/* IMPORTANT: Don't overwrite the river */
				if (old_feat == feat) continue;

				/* Place river edge, if needed */
				if (edge != old_feat) build_terrain(yy, xx, edge);
			}
		}

		/* Stagger the river */
		if (!rand_int(2))
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
 * Record good location for stairwell, player start or quest object
 */
static bool add_stair(int y, int x)
{
	/* Needs to be in bounds */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Record location */
	if (dun->stair_n < STAIR_MAX)
	{
		dun->stair[dun->stair_n].y = y;
		dun->stair[dun->stair_n].x = x;
		dun->stair_n++;	

		return (TRUE);
	}
	
	return (FALSE);
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
 * Go in a semi-random direction from current location to target location.
 * Do not actually head away from the target grid.  Always make a turn.
 */
static void adjust_dir(int *row_dir, int *col_dir, int y1, int x1, int y2, int x2)
{
	/* Always turn 90 degrees. */
	if ((*row_dir == 0) || ((*row_dir != 0) && (*col_dir != 0) && (rand_int(100) < 50)))
	{
		*col_dir = 0;

		/* On the y-axis of target - freely choose a side to turn to. */
		if (y1 == y2) *row_dir = ((rand_int(2)) ? - 1 : 1);

		/* Never turn away from target. */
		else *row_dir = ((y1 < y2) ? 1 : -1);
	}
	else
	{
		*row_dir = 0;

		/* On the x-axis of target - freely choose a side to turn to. */
		if (x1 == x2) *col_dir = ((rand_int(2)) ? - 1 : 1);

		/* Never turn away from target. */
		else *col_dir = ((x1 < x2) ? 1 : -1);
	}
}


/*
 * Go in a completely random orthogonal direction.  If we turn around
 * 180 degrees, save the grid; it may be a good place to place stairs
 * and/or the player.
 */
static void rand_dir(int *row_dir, int *col_dir, int y, int x)
{
	/* Pick a random direction */
	int i = rand_int(4);

	/* Extract the dy/dx components */
	int row_dir_tmp = ddy_ddd[i];
	int col_dir_tmp = ddx_ddd[i];

	/* Save useful grids. */
	if ((-(*row_dir) == row_dir_tmp) && (-(*col_dir) == col_dir_tmp))
	{
		/* Save the current tunnel location if surrounded by walls. */
		if (next_to_walls(y, x) == 4)
		{
			add_stair(y, x);
		}
	}

	/* Save the new direction. */
	*row_dir = row_dir_tmp;
	*col_dir = col_dir_tmp;
}


/*
 * Go in a completely random direction.  If we turn around
 * 180 degrees, save the grid; it may be a good place to place stairs
 * and/or the player.
 */
static void rand_dir_cave(int *row_dir, int *col_dir, int y, int x)
{
	/* Pick a random direction */
	int i = rand_int(8);

	/* Extract the dy/dx components */
	int row_dir_tmp = ddy_ddd[i];
	int col_dir_tmp = ddx_ddd[i];

	/* Save useful grids. */
	if ((-(*row_dir) == row_dir_tmp) && (-(*col_dir) == col_dir_tmp))
	{
		/* Save the current tunnel location if surrounded by walls. */
		if ((in_bounds_fully(y, x)) && (dun->stair_n < STAIR_MAX) &&
			(next_to_walls(y, x) == 4))
		{
			add_stair(y, x);
		}
	}

	/* Save the new direction. */
	*row_dir = row_dir_tmp;
	*col_dir = col_dir_tmp;
}


/*
 * Leave 1 & 2 empty as these are used to vary the crenallations in crypt
 * corridors
 */

#define TUNNEL_STYLE	4L	/* First 'real' style */
#define TUNNEL_CRYPT_L	4L
#define TUNNEL_CRYPT_R	8L
#define TUNNEL_LARGE_L	16L
#define TUNNEL_LARGE_R	32L
#define TUNNEL_CAVE	64L

static u32b get_tunnel_style(void)
{
	int style = 0;
	int i = rand_int(100);

	/* Stronghold levels have width 2 corridors, or width 3 corridors, often with pillars */
	/* The style of the tunnel does not change after initial selection */
	if (level_flag & (LF1_STRONGHOLD))
	{
		if (i < 66) style |= (TUNNEL_LARGE_L);
		if (i > 33) style |= (TUNNEL_LARGE_R);
	}
	/* Dungeon levels have width 1 corridors, or width 2 corridors deeper in the dungeon */
	/* The style of the tunnel does not change after initial selection */
	else if (level_flag & (LF1_DUNGEON))
	{
		if (i < p_ptr->depth) style |= (i % 2) ? (TUNNEL_LARGE_L) : (TUNNEL_LARGE_R);
	}
	/* Sewer levels have width 2 corridors, or width 3 corridors, often with pillars */
	/* The centre of the corridor is filled with pool type terrain, and the corridor style
	   is fixed for each corridor. */
	else if (level_flag & (LF1_SEWER))
	{
		if (i < 50) style |= (TUNNEL_LARGE_L);
		if ((i < 25) || (i >= 75)) style |= (TUNNEL_LARGE_R);
	}
	/* Crypt levels have width 2 corridors, or width 3 corridors, with pillars on the edge of the corridor */
	/* The corridor style changes regularly. */
	else if (level_flag & (LF1_CRYPT))
	{
		if (i < 50) style |= (TUNNEL_CRYPT_L);
		if ((i < 25) || (i >= 75)) style |= (TUNNEL_CRYPT_R);
	}
	/* Cave levels have narrow, frequently random corridors. Mines occasionally do. */
	/* Mines have longer straights than other levels. */
	else if (((level_flag & (LF1_CAVE)) != 0) || (((level_flag & (LF1_MINE)) != 0) && (i < 33)))
	{
		style |= (TUNNEL_CAVE);
	}

	style |= rand_int(TUNNEL_STYLE);

	return (style);
}


/*
 * Record location for tunnel decoration
 */
static bool add_decor(int y, int x, int t)
{
	/* Needs to be in bounds */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Needs to be a wall */
	if ((f_info[cave_feat[y][x]].flags1 & (FF1_WALL)) == 0) return (FALSE);
	
	/* Record location and tunnel length */
	if (dun->decor_n < DECOR_MAX)
	{
		dun->decor[dun->decor_n].y = y;
		dun->decor[dun->decor_n].x = x;
		dun->decor_t[dun->decor_n] = t;
		dun->decor_n++;
		
		return (TRUE);
	}
	
	return (FALSE);
}

/*
 * Record location for entrance to room through outer wall
 */
static bool add_wall(int y, int x)
{
	/* XXX Don't need to check inbounds because of use of inbounds_fully_tunnel from caller */

	/* Needs an 'outer' wall */
	if ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) == 0) return (FALSE);

	/* Record location */
	if (dun->wall_n < WALL_MAX)
	{
		dun->wall[dun->wall_n].y = y;
		dun->wall[dun->wall_n].x = x;
		dun->wall_n++;	

		return (TRUE);
	}
	
	return (FALSE);
}


/*
 * Record location for entrance to room through outer wall
 */
static bool add_solid(int y, int x, int feat)
{
	/* Needs to be in bounds */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Needs an 'outer' wall */
	if ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) == 0) return (FALSE);

	/* Record location and feature */
	if (dun->solid_n < SOLID_MAX)
	{
		dun->solid[dun->solid_n].y = y;
		dun->solid[dun->solid_n].x = x;
		dun->solid_feat[dun->solid_n] = feat;
		dun->solid_n++;

		return (TRUE);
	}
	
	return (FALSE);
}


/*
 * Record location for tunnel
 */
static bool add_tunnel(int y, int x, int feat)
{
	/* XXX Don't need to check inbounds because of use of inbounds_fully_tunnel from caller */

	/* XXX We could bridge or tunnel a location and add it as a 'tunnel' */

	/* Record location and feature */
	if (dun->tunn_n < TUNN_MAX)
	{
		dun->tunn[dun->tunn_n].y = y;
		dun->tunn[dun->tunn_n].x = x;
		dun->tunn_feat[dun->tunn_n] = feat;
		dun->tunn_n++;

		return (TRUE);
	}
	
	return (FALSE);
}


/*
 * Record location for entrance to room through outer wall
 */
static bool add_next(int y, int x, int feat)
{
	/* Needs to be in bounds */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Needs an 'solid' wall */
	if ((f_info[cave_feat[y][x]].flags1 & (FF1_SOLID)) == 0) return (FALSE);

	/* Record location and feature */
	if (dun->next_n < NEXT_MAX)
	{
		dun->next[dun->next_n].y = y;
		dun->next[dun->next_n].x = x;
		dun->next_feat[dun->next_n] = f_info[feat].flags1 & (FF1_OUTER) ? feat_state(feat, FS_SOLID) : feat;
		dun->next_n++;

		return (TRUE);
	}
	
	return (FALSE);
}


/*
 * Record location for door at intersection
 */
static bool add_door(int y, int x)
{
	/* Needs to be in bounds */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Needs not to be a wall */
	if ((f_info[cave_feat[y][x]].flags1 & (FF1_WALL)) != 0) return (FALSE);

	/* Record location */
	if (dun->door_n < DOOR_MAX)
	{
		dun->door[dun->door_n].y = y;
		dun->door[dun->door_n].x = x;
		dun->door_n++;
		return (TRUE);
	}
	
	return (FALSE);
}


/*
 * Check if there are any "non-room" grids adjacent to the given grid.
 */
static bool near_edge(int y1, int x1)
{
	int y, x;

	/* Scan nearby grids */
	for (y = y1 - 2; y < y1 + 2; y++)
	{
		for (x = x1 - 2; x < x1 + 2; x++)
		{
			/* Skip grids inside rooms */
			if (cave_info[y][x] & (CAVE_ROOM)) continue;

			/* Count these grids */
			return (TRUE);
		}
	}

	/* Inside a room */
	return (FALSE);
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
 *
 *
 * We now style the tunnels. The following tunnel styles are supported:
 *
 * -- standard Angband tunnel
 * -- tunnel with pillared edges (on LF1_CRYPT levels)
 * -- width 2 or width 3 tunnel (on LF1_STRONGHOLD levels)
 * -- tunnels with lateral and diagonal interruptions (on LF1_CAVE levels)
 *
 * We also can fill tunnels now. Filled tunnels occur iff the last floor
 * space leaving the start room is not a FEAT_FLOOR or the first floor
 * space entering the finishing room is not a FEAT_FLOOR.
 *
 * We also put 'decorations' next to tunnel entrances. These are various
 * solid wall types relating to the room the tunnel goes from or to.
 * However, we use the decorations of the room at the other end of the tunnel
 * unless that room has no decorations, in which case we use our own.
 */
static bool build_tunnel(int row1, int col1, int row2, int col2, bool allow_overrun)
{
	int i, j, y, x;
	int tmp_row = row1, tmp_col = col1;
	int row_dir, col_dir;
	int start_row, start_col;
	int main_loop_count = 0;
	int last_turn = 0, first_door, last_door, first_tunn, first_next, first_stair;
	int start_tunnel = 0, start_decor = 0, end_decor = 0;
	int last_decor = 0;
	int door_l = 0;
	int door_r = 0;
	int old_row_dir = 0;
	int old_col_dir = 0;
	
	bool flood_tunnel = FALSE;
	bool door_flag = FALSE;
	bool overrun_flag = FALSE;
	bool decor_flag = FALSE;
	bool abort_and_cleanup = FALSE;
	
	/* Force style change */
	u32b style = get_tunnel_style();

	int start_room = room_idx_ignore_valid(row1, col1);
	int end_room = 0;

	/* Initialize some movement counters */
	int adjust_dir_timer = randint(DUN_TUN_ADJ * 2) * (((level_flag & (LF1_MINE)) != 0) ? 2 : 1);
	int rand_dir_timer   = randint(DUN_TUN_RND * 2);
	int correct_dir_timer = 0;
	int tunnel_style_timer = randint(DUN_TUN_STYLE * 2);
	int crypt_timer = randint(DUN_TUN_CRYPT * 2);
	int cave_length_timer = ((style & TUNNEL_CAVE) != 0) ? randint(DUN_TUN_LEN * 2) : 0;

	/* Not yet worried about our progress */
	int desperation = 0;

	/* Partition to mark array with */
	int part1 = CENT_MAX;

	/* Keep stronghold / dungeon / sewer corridors tidy */
	if ((level_flag & (LF1_STRONGHOLD | LF1_DUNGEON | LF1_SEWER)) != 0) tunnel_style_timer = -1;

	/* Readjust movement counter for caves */
	if ((style & TUNNEL_CAVE) != 0) rand_dir_timer = randint(DUN_TUN_CAV * 2);

	/* Reset the arrays */
	dun->tunn_n = 0;
	dun->wall_n = 0;
	dun->solid_n = 0;
	dun->decor_n = 0;

	/* Save the starting location */
	start_row = row1;
	start_col = col1;

	/* Record number of doorways */
	first_door = dun->door_n;
	last_door = dun->door_n;

	/* Record start locations in the event of aborting */
	first_tunn = dun->tunn_n;
	first_next = dun->next_n;
	first_stair = dun->stair_n;

	/* Record initial partition number */
	if (start_room)
	{
		part1 = dun->part[start_room-1];
		
		/* Flood if part of flooded dungeon */
		if ((room_info[start_room].flags & (ROOM_FLOODED)) != 0)
		{
			flood_tunnel = TRUE;
		}
	}

	/* Start out in the correct direction */
	correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

	/* Keep going until done (or bored) */
	while (TRUE) 
	{
		/* Mega-Hack -- Paranoia -- prevent infinite loops */
		if (main_loop_count++ > 2000)
		{
			if (cheat_room) msg_format("Main loop count exceeded. Aborting.", part1);
			abort_and_cleanup = TRUE;
			break;
		}

		/* Hack -- if we are not starting in a room, include starting grid. Also add as a possible stair location. */
		if ((dun->tunn_n == 0) && ((cave_info[row1][col1] & (CAVE_ROOM)) == 0))
		{
			row1 = row1 - row_dir;
			col1 = col1 - col_dir;

			/* Good candidate for stairs */
			add_stair(row1, col1);
			
			/* Decorate 'left-hand' corner of dead end */
			add_decor(row1 - row_dir + col_dir * (((style & (TUNNEL_LARGE_L)) != 0) ? 2 : 1),
							col1 - col_dir - row_dir * (((style & (TUNNEL_LARGE_L)) != 0) ? 2 : 1),
							dun->tunn_n);

			/* Decorate 'right-hand' corner of dead end */
			add_decor(row1 - row_dir + col_dir * (((style & (TUNNEL_LARGE_R)) != 0) ? 2 : 1),
							col1 - col_dir + row_dir * (((style & (TUNNEL_LARGE_R)) != 0) ? 2 : 1),
							dun->tunn_n);

			/* Force decoration */
			decor_flag = FALSE;
		}

		/* Try moving randomly if we seem stuck. */
		else if ((row1 != tmp_row) && (col1 != tmp_col))
		{
			desperation++;

			/* Try a 90 degree turn. */
			if (desperation == 1)
			{
				adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);
				adjust_dir_timer = 3;
			}

			/* Try turning randomly. */
			else if (desperation < 4)
			{
				rand_dir(&row_dir, &col_dir, row1, col1);
				correct_dir_timer = 2;
			}
			else
			{
				if (cheat_room) msg_format("Tunnel stuck from %d. Aborting.", part1);
				
				abort_and_cleanup = TRUE;
				break;
			}
		}

		/* We're making progress. */
		else
		{
			/* No worries. */
			desperation = 0;

			/* Count down times until next movement changes. */
			if (adjust_dir_timer > 0) adjust_dir_timer--;
			if (rand_dir_timer > 0) rand_dir_timer--;
			if (correct_dir_timer > 0) correct_dir_timer--;
			if (tunnel_style_timer > 0) tunnel_style_timer--;
			if (cave_length_timer > 0) cave_length_timer--;

			/* Adjust the tunnel style if required */
			if (tunnel_style_timer == 0)
			{
				/* Caves re-pick style less frequently due to hacking style */
				if (!cave_length_timer || (style % 4 == 3)) style = get_tunnel_style();
				tunnel_style_timer = randint(DUN_TUN_STYLE * 2);
			}
			
			/* Decrease cave tunnel randomness to allow it to find rooms further away */
			if (cave_length_timer == 0)
			{
				if ((style % 4) < 3) style++;
				cave_length_timer = randint(DUN_TUN_LEN * 2);
			}

			/* Make a random turn, set timer. */
			if (rand_dir_timer == 0)
			{
				if ((style & TUNNEL_CAVE) != 0)
				{
					rand_dir_cave(&row_dir, &col_dir, row1, col1);
					rand_dir_timer = randint((DUN_TUN_CAV * (1 + (style % 4))) * 2);
				}
				else
				{
					rand_dir(&row_dir, &col_dir, row1, col1);
					rand_dir_timer = randint(DUN_TUN_RND * 2);
				}

				correct_dir_timer = randint(4);
			}

			/* Adjust direction, set timer. */
			if (adjust_dir_timer == 0)
			{
				adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);

				adjust_dir_timer = randint(DUN_TUN_ADJ * 2) * (((level_flag & (LF1_MINE)) != 0) ? 2 : 1);
			}

			/* Go in correct direction. */
			if (correct_dir_timer == 0)
			{
				correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

				/* Don't use again unless needed. */
				correct_dir_timer = -1;
			}
		}

		/* Get the next location */
		tmp_row = row1 + row_dir;
		tmp_col = col1 + col_dir;

		/* Do not leave the dungeon */
		while (!in_bounds_fully_tunnel(tmp_row, tmp_col))
		{
			/* Fall back to last turn coords */
			if (!last_turn)
			{
				row1 = start_row;
				col1 = start_col;
			}
			else
			{
				row1 = dun->tunn[last_turn - 1].y;
				col1 = dun->tunn[last_turn - 1].x;
			}

			/* Fall back to last turn */
			dun->tunn_n = last_turn;
			dun->door_n = last_door;

			/* Clear decorations up to here */
			for ( ; dun->decor_n > 0; dun->decor_n--)
			{
				if (dun->decor_t[dun->decor_n - 1] < dun->tunn_n) break;
			}
			
			/* Set last decor */
			if (last_decor > dun->decor_n) last_decor = dun->decor_n;

			/* Back up some more */
			last_turn /= 2;
			last_door = first_door;

			/* Adjust direction */
			adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Get the next location */
			tmp_row = row1 + row_dir;
			tmp_col = col1 + col_dir;
		}

		/* Avoid "solid" granite walls */
		if (f_info[cave_feat[tmp_row][tmp_col]].flags1 & (FF1_SOLID)) continue;

		/* Pierce "outer" walls of rooms */
		if ((f_info[cave_feat[tmp_row][tmp_col]].flags1 & (FF1_OUTER)) &&
			(((room_has_flag(tmp_row, tmp_col, (ROOM_EDGED))) == 0) || !(near_edge(tmp_row, tmp_col))))
		{
			int wall1 = dun->wall_n;
			bool door = TRUE;
			bool pillar = FALSE;
			
			/* Get the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Hack -- Avoid outer/solid walls */
			if (f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) continue;
			if (f_info[cave_feat[y][x]].flags1 & (FF1_SOLID)) continue;

			/* No need to decorate */
			dun->decor_n = last_decor;
			decor_flag = TRUE;

			/* Save the wall location */
			if (add_wall(tmp_row, tmp_col))
			{
				/* Add a 'left-hand' wall piercing */
				if (style & (TUNNEL_LARGE_L))
				{
					if (add_wall(tmp_row + col_dir, tmp_col - row_dir))
					{
						/* Hack -- add regular pillars to some width 3 corridors */
						if ((((tmp_row + tmp_col) % ((style % 4) + 2)) == 0)
							&& ((style & (TUNNEL_CRYPT_L | TUNNEL_CRYPT_R))== 0)) pillar = TRUE;
					}
					else
					{
						door = FALSE;
					}
				}

				/* Add a 'right-hand' wall piercing */
				if (style & (TUNNEL_LARGE_R))
				{
					if (pillar) dun->wall_n -= 2;
					
				 	if (add_wall(tmp_row - col_dir, tmp_col + row_dir))
				 	{
						/* Done */
				 	}
				 	else
				 	{
				 		door = FALSE;
				 	}
				 	
				 	if (pillar) dun->wall_n++;
				}
			}

			/* Cancel if can't make all the doors */
			if (!door)
			{
				dun->wall_n = wall1;
				continue;
			}

			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Forbid re-entry near these piercings */
			for (i = wall1; i < dun->wall_n; i++)
			{
				for (y = dun->wall[i].y - 1; y <= dun->wall[i].y + 1; y++)
				{
					for (x = dun->wall[i].x - 1; x <= dun->wall[i].x + 1; x++)
					{
						bool doorway;
						
						doorway = FALSE;

						/* Avoid solidifying areas where we'll end up placing doors */
						for (j = wall1; j < dun->wall_n; j++)
						{
							if ((y == dun->wall[j].y) && (x == dun->wall[j].x)) doorway = TRUE; 
						}

						if (doorway) continue;

						/* Convert adjacent "outer" walls as "solid" walls */
						if (add_solid(y, x, cave_feat[y][x]))
						{
							/* Room */
							end_room = room_idx_ignore_valid(tmp_row, tmp_col);
							
							/* Change the wall to a "solid" wall */
							cave_alter_feat(y, x, FS_SOLID);

							/* Decorate next to the start and/or end of the tunnel with the starting room decorations */
							if ((start_room) && ((room_info[start_room].theme[THEME_SOLID]) || (start_room == end_room)))
							{
								/* Overwrite with alternate terrain from starting room later */
								add_next(y, x, room_info[start_room].theme[THEME_SOLID]);
							}
						}
					}
				}
			}
		}

		/* Travel quickly through rooms -- unless we have to overwrite an edge, or are bridging it
		 * from a non-flooded tunnel */
		else if ((cave_info[tmp_row][tmp_col] & (CAVE_ROOM)) && 
			((flood_tunnel) || ((room_has_flag(tmp_row, tmp_col, (ROOM_BRIDGED))) == 0)) &&
			(((room_has_flag(tmp_row, tmp_col, (ROOM_EDGED))) == 0) || !(near_edge(tmp_row, tmp_col))))
		{
			/* Room */
			end_room = room_idx_ignore_valid(tmp_row, tmp_col);

			/* Different room */
			if ((start_room) && (end_room) && (start_room < DUN_ROOMS)
				&& (end_room < DUN_ROOMS) && (start_room != end_room))
			{
				/* Different room in same partition */
				if (part1 == dun->part[end_room-1])
				{
					abort_and_cleanup = TRUE;
					if (cheat_room) msg_format("Loop in partition %d. Aborting.", part1);
					break;
				}

				/* Trying to connect flooded to unflooded area */
				else if ((flood_tunnel) && ((room_info[end_room].flags & (ROOM_FLOODED)) == 0))
				{
					abort_and_cleanup = TRUE;
					if (cheat_room) msg_format("Trying to flood unflooded partition %d. Aborting.", part1);
					break;
				}
				
				else if (part1 < CENT_MAX)
				{
					int part2 = dun->part[end_room-1];
					
					/* Merging successfully */
					if (cheat_room) msg_format("Merging partition %d (room %d) with %d (room %d) (length %d).", part1, start_room, part2, end_room, dun->tunn_n);

					/* Merge partitions */
					for (i = 0; i < dun->cent_n; i++)
					{
						if (dun->part[i] == part2) dun->part[i] = part1;
					}

					/* Accept the location */
					row1 = tmp_row;
					col1 = tmp_col;
					
					/* Accept tunnel */
					break;
				}
			}
			else
			{
				/* Accept the location */
				row1 = tmp_row;
				col1 = tmp_col;

				/* Set tunnel feature if feature is not a floor, or it is a sewers level */
				if (((cave_feat[tmp_row][tmp_col] != FEAT_FLOOR) || (level_flag & (LF1_SEWER)))
					&& (start_room < DUN_ROOMS))
				{
					start_tunnel = room_info[start_room].theme[THEME_TUNNEL];
				}
				/* Clear tunnel feature if feature is a floor */
				else
				{
					start_tunnel = 0;
				}

				if (start_room < DUN_ROOMS)
				{
					/* Set tunnel decor */
					start_decor = room_info[start_room].theme[THEME_SOLID];
				}
				
				door_flag = FALSE;
				decor_flag = TRUE;
				
				/* End tunnel if required */
				if (start_room != end_room) break;
			}
		}
		
		/* Bridge features (always in rooms, unless 'flooding') */
		else if (((f_info[cave_feat[tmp_row][tmp_col]].flags2 & (FF2_BRIDGE)) != 0) ||
			((cave_info[tmp_row][tmp_col] & (CAVE_ROOM)) && !(flood_tunnel)))
		{
			bool left_turn = FALSE;
			bool right_turn = FALSE;
			
			bool made_tunnel;
			
			/* Force bridges to consider correct direction frequently */
			if (correct_dir_timer < 0) correct_dir_timer = randint(4);
			
			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Add a bridge location */
			made_tunnel = add_tunnel(row1, col1, 0);
				
			if (made_tunnel)
			{
				/* Are we turning left? */
				if ((row_dir == -old_col_dir) || (col_dir == old_row_dir)) left_turn = TRUE;

				/* Are we turning right? */
				if ((row_dir == old_col_dir) || (col_dir == -old_row_dir)) right_turn = TRUE;				
				
				/* Add a 'left-hand' bridge, but ensure the full bridge is no more than 2 wide */
				if ((style & (TUNNEL_LARGE_L)) && (!(style & (TUNNEL_LARGE_R)) || (style % 2)))
				{
					if (f_info[cave_feat[row1+col_dir][col1-row_dir]].flags2 & (FF2_BRIDGE))
					{
						/* Add a bridge location */
						made_tunnel = add_tunnel(row1 + col_dir, col1 - row_dir, 0);
					}
					
					/* Add 'diagonally backwards left-hand' bridge if just turned right */
					if (made_tunnel && right_turn)
					{
						/* Add if bridgeable */
						if ((f_info[cave_feat[row1 - row_dir + col_dir ][col1 - col_dir - row_dir]].flags2 & (FF2_BRIDGE)) != 0)
						{
							made_tunnel = add_tunnel(row1 - row_dir + col_dir, col1 - col_dir - row_dir, 0);
						}
					}
				}

				/* Add a 'right-hand' bridge, but ensure the full bridge is no more than 2 wide */
				else if (style & (TUNNEL_LARGE_L | TUNNEL_LARGE_R))
				{
					if (f_info[cave_feat[row1-col_dir][col1+row_dir]].flags2 & (FF2_BRIDGE))
					{
						/* Add a bridge location */
						made_tunnel = add_tunnel(row1 - col_dir, col1 + row_dir, 0);
					}
					
					/* Add 'diagonally backwards right-hand' bridge if just turned left */
					if (made_tunnel && left_turn)
					{
						/* Add if tunnelable, but not inner, outer or solid */
						if ((f_info[cave_feat[row1 - row_dir - col_dir ][col1 - col_dir + row_dir]].flags2 & (FF2_BRIDGE)) != 0)
						{
							made_tunnel = add_tunnel(row1 - row_dir + col_dir, col1 - col_dir - row_dir, 0);
						}
					}
				}
			}
			
			/* Ran out of tunnel space */
			if (!made_tunnel)
			{
				if (cheat_room) msg_format("Tunnel length exceeded from %d. Aborting.", part1);
			
				/* Hack -- themed tunnels can get too long. So try unthemed. */
				level_flag &= ~(LF1_THEME);

				abort_and_cleanup = TRUE;
				break;				
			}

			/* Prevent door decoration in next grid */
			door_flag = TRUE;
			
			/* Force decoration in next grid */
			decor_flag = FALSE;
		}

		/* Tunnel through all other walls and bridge features */
		else if ((f_info[cave_feat[tmp_row][tmp_col]].flags1 & (FF1_TUNNEL))
				|| ((cave_info[tmp_row][tmp_col] & (CAVE_ROOM))))
		{
			bool pillar = FALSE;
			bool decor_l = FALSE;
			bool decor_r = FALSE;
			bool left_turn = FALSE;
			bool right_turn = FALSE;
			
			bool made_tunnel = TRUE;
			
			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/*
			 * Save the tunnel location unless an inner wall
			 * Note hack to differentiate a tunnel from a bridge, if start_tunnel not set.
			 */
			if ((f_info[cave_feat[row1][col1]].flags1 & (FF1_INNER)) == 0)
			{
				made_tunnel = add_tunnel(row1, col1, start_tunnel ? start_tunnel : 1);
				
				if (made_tunnel)
				{
					/* Did we turn around? Tunnel dead ends are good candidates for stairs.
					 * Note use of decor flag to ensure previous move was a tunnel build.
					 * 
					 * XXX Could also have been a move through 'outer' wall. Should be rare
					 * enough not to worry about.
					 */
					if ((decor_flag) && (row_dir == -old_row_dir) && (col_dir == -old_col_dir))
					{
						add_stair(row1 + old_row_dir, col1 + old_col_dir);
	
						/* Hack -- move decoration to corners of dead end. This 'fakes'
						 * a secret door. */
						if (dun->decor_n > 1)
							for (i = dun->decor_n - 2; i < dun->decor_n; i++)
						{
							dun->decor[i].y += old_row_dir;
							dun->decor[i].x += old_col_dir;
						}
	
						/* Force decoration */
						decor_flag = FALSE;
					}				
					
					/* Are we turning left? */
					if ((row_dir == -old_col_dir) || (col_dir == old_row_dir)) left_turn = TRUE;
	
					/* Are we turning right? */
					if ((row_dir == old_col_dir) || (col_dir == -old_row_dir)) right_turn = TRUE;
	
					/* Do we not need decoration? */
					if ((decor_flag) && (dun->tunn_n % (7 + (style % 4) * 5)))
					{
						dun->decor_n = last_decor;
					}
	
					/* Save the decoration */
					last_decor = dun->decor_n;
	
					/* Add width to tunnel if wide or crypt.
					 * Note checks to ensure width 2 tunnels near rooms.
					 */
					if ((style & (TUNNEL_CRYPT_L | TUNNEL_LARGE_L))
						|| ((style & (TUNNEL_CRYPT_R | TUNNEL_LARGE_R))
							&& ((f_info[cave_feat[row1 - col_dir][col1 + row_dir]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)))
					{
						/* Add 'left-hand' tunnel */
						if ((f_info[cave_feat[row1+col_dir][col1-row_dir]].flags1 & (FF1_TUNNEL))
							&& ((f_info[cave_feat[row1 + col_dir][col1 - row_dir]].flags1 & (FF1_OUTER | FF1_SOLID)) == 0)
							&& ((style & (TUNNEL_LARGE_L)) || !((row1 + col1 + style) % 2)))
						{
							/* Save the tunnel location unless an inner wall */
							if ((f_info[cave_feat[row1 + col_dir][col1 - row_dir]].flags1 & (FF1_INNER)) == 0)
							{
								made_tunnel = add_tunnel(row1 + col_dir, col1 - row_dir, 0);
								
								if (made_tunnel)						
								{
									/* Hack -- add regular pillars to some width 3 corridors */
									if ((((row1 + col1) % ((style % 4) + 2)) == 0)
										&& ((style & (TUNNEL_CRYPT_L | TUNNEL_CRYPT_R))== 0)) pillar = TRUE;
		
									/* Save the location for later stair allocation */
									if ((style & (TUNNEL_CRYPT_L | TUNNEL_CRYPT_R))
										&& (crypt_timer-- == 0))
									{
										add_stair(row1 + col_dir, col1 - row_dir);
		
										crypt_timer = randint(DUN_TUN_CRYPT * 2);
									}
									
									/* Decorate */
									if (add_decor(row1 + 2 * col_dir, col1 - 2 * row_dir, dun->tunn_n)) decor_l = TRUE;
								}
							}
						}
							
						/* Add 'diagonally backwards left-hand' tunnel if just turned right */
						if (made_tunnel && right_turn)
						{
							/* Add if tunnelable, but not inner, outer or solid */
							if (((f_info[cave_feat[row1 - row_dir + col_dir ][col1 - col_dir - row_dir]].flags1 & (FF1_TUNNEL)) != 0)
								&& ((f_info[cave_feat[row1 - row_dir + col_dir ][col1 - col_dir - row_dir]].flags1 & (FF1_INNER | FF1_OUTER | FF1_SOLID)) == 0))
							{
								made_tunnel = add_tunnel(row1 - row_dir + col_dir, col1 - col_dir - row_dir, 0);
							}
						}
					}
	
					/* Add width to tunnel if wide or crypt.
					 * Note checks to ensure width 2 tunnels near rooms.
					 */
					if ((style & (TUNNEL_CRYPT_R | TUNNEL_LARGE_R))
						|| ((style & (TUNNEL_CRYPT_L | TUNNEL_LARGE_L))
							&& ((f_info[cave_feat[row1 + col_dir][col1 - row_dir]].flags1 & (FF1_OUTER | FF1_SOLID)) != 0)))
					{
						/* Add 'right-hand' tunnel */
						if ((f_info[cave_feat[row1-col_dir][col1+row_dir]].flags1 & (FF1_TUNNEL))
							&& ((f_info[cave_feat[row1 - col_dir][col1 + row_dir]].flags1 & (FF1_OUTER | FF1_SOLID)) == 0)
							&& ((style & (TUNNEL_LARGE_R)) || !((row1 + col1 + style / 2) % 2)))
						{
							/* Save the tunnel location unless an 'inner' wall */
							if ((f_info[cave_feat[row1 - col_dir][col1 + row_dir]].flags1 & (FF1_INNER)) == 0)
							{
								if (made_tunnel && pillar) dun->tunn_n -= 2;
	
								made_tunnel = add_tunnel(row1 - col_dir, col1 + row_dir, 0);
								
								if (made_tunnel)
								{
									/* Save the location for later stair allocation */
									if ((style & (TUNNEL_CRYPT_L | TUNNEL_CRYPT_R))
										&& (crypt_timer-- == 0))
									{
										add_stair(row1 - col_dir, col1 + row_dir);
										
										crypt_timer = randint(DUN_TUN_CRYPT * 2);
									}
	
									/* Decorate */
									if (add_decor(row1 - 2 * col_dir, col1 + 2 * row_dir, dun->tunn_n)) decor_r = TRUE;
								}
	
								if (made_tunnel && pillar) dun->tunn_n++;
							}
						}
	
						/* Add 'diagonally backwards right-hand' tunnel if just turned left */
						if (made_tunnel && left_turn)
						{
							/* Add if tunnelable, but not inner, outer or solid */
							if (((f_info[cave_feat[row1 - row_dir - col_dir ][col1 - col_dir + row_dir]].flags1 & (FF1_TUNNEL)) != 0)
								&& ((f_info[cave_feat[row1 - row_dir - col_dir ][col1 - col_dir + row_dir]].flags1 & (FF1_INNER | FF1_OUTER | FF1_SOLID)) == 0))
							{
								made_tunnel = add_tunnel(row1 - row_dir + col_dir, col1 - col_dir - row_dir, 0);
							}
						}
					}
				}
			}
			
			/* Ran out of tunnel space */
			if (!made_tunnel)
			{
				if (cheat_room) msg_format("Tunnel length exceeded from %d. Aborting.", part1);
			
				/* Hack -- themed tunnels can get too long. So try unthemed. */
				level_flag &= ~(LF1_THEME);

				abort_and_cleanup = TRUE;
				break;				
			}

			/* Decorate 'left-hand side' if required */
			if (!decor_l)
			{
				add_decor(row1 + col_dir, col1 - row_dir, dun->tunn_n);
			}

			/* Decorate 'left-hand side' if required */
			if (!decor_r)
			{
				add_decor(row1 - col_dir, col1 + row_dir, dun->tunn_n);
			}

			/* Allow door in next grid */
			door_flag = FALSE;
			
			/* Don't require decoration next to this grid */
			decor_flag = TRUE;
		}

		/* Handle corridor intersections or overlaps */
		else
		{
			bool pillar = FALSE;

			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Prevent us following corridor length */
			if ((door_flag) && !(allow_overrun))
			{
				if ((overrun_flag) || (dun->door_n > first_door + 6))
				{
					if (cheat_room) msg_format("Overrun in doors from %d. Aborting.", part1);

					abort_and_cleanup = TRUE;
					break;
				}

				overrun_flag = TRUE;
			}

			/* Allowed left hand side door */
			if (!(door_flag) || (--door_l > 0))
			{
				/* Add 'left-hand' door */
				if (style & (TUNNEL_LARGE_L))
				{
					if (add_door(tmp_row + col_dir, tmp_col - row_dir))
					{
						/* Hack -- add regular pillars to some width 3 corridors */
						if ((((tmp_row + tmp_col) % ((style % 4) + 2)) == 0)
							&& ((style & (TUNNEL_CRYPT_L | TUNNEL_CRYPT_R))== 0)) pillar = TRUE;
					}
					
					/* Allow door in next grid */
					if (!door_flag) door_l = 3;
				}
			}

			/* Allowed left hand side door */
			if (!(door_flag) || (--door_r > 0))
			{			
				/* Add 'right-hand' door */
				if (style & (TUNNEL_LARGE_R))
				{
					
					if (add_door(tmp_row + col_dir, tmp_col - row_dir))
					{
						/* Allow door in next grid */
						if (!door_flag) door_r = 3;
					}
				}
				else
				{
					pillar = FALSE;
				}
			}
			
			/* Collect legal door locations */
			if ((!door_flag) && (!pillar))
			{
				/* Save the door location */
				add_door(row1, col1);
				
				/* No door in next grid */
				door_flag = TRUE;

				/* Haven't overrun doors yet */
				overrun_flag = FALSE;
			}

			/* Force decoration in next grid */
			decor_flag = FALSE;

			/* Hack -- allow pre-emptive tunnel termination */
			if ((rand_int(100) >= DUN_TUN_CON) && (dun->door_n < 3))
			{
				/* Distance between row1 and start_row */
				tmp_row = row1 - start_row;
				if (tmp_row < 0) tmp_row = (-tmp_row);

				/* Distance between col1 and start_col */
				tmp_col = col1 - start_col;
				if (tmp_col < 0) tmp_col = (-tmp_col);

				/* Terminate the tunnel */
				if ((tmp_row > 10) || (tmp_col > 10))
				{
					break;
				}
			}
		}
		
		/* Record old directions to check for dead ends */
		old_row_dir = row_dir;
		old_col_dir = col_dir;
			
		/* Fix up diagonals from cave tunnels after 1 move */
		/* Never move diagonally */
		if (row_dir && col_dir)
		{
			if (rand_int(100) < 50)
			{
				row_dir = 0;
			}
			else
			{
				col_dir = 0;
			}
		}

		/* End found */
		if ((row1 == row2) && (col1 == col2)) 
		{
			/* Room */
			end_room = room_idx_ignore_valid(row1, col1);
			
			/* Both ends are rooms */
			if ((start_room) && (end_room) &&
				(start_room < DUN_ROOMS) && (end_room < DUN_ROOMS))
			{	
				/* Different room in same partition */
				if (part1 == dun->part[end_room-1])
				{
					if (cheat_room) msg_format("Loop in partition %d endpoints. Aborting.", part1);

					abort_and_cleanup = TRUE;
					break;
				}

				/* Trying to connect flooded to unflooded area */
				else if ((flood_tunnel) && ((room_info[end_room].flags & (ROOM_FLOODED)) == 0))
				{
					abort_and_cleanup = TRUE;
					if (cheat_room) msg_format("Trying to flood unflooded partition %d. Aborting.", part1);
					break;
				}
				
				else if (part1 < CENT_MAX)
				{
					int part2 = dun->part[end_room-1];

					/* Merging successfully */
					if (cheat_room) msg_format("Merging partition %d (room %d) with %d (room %d) by reaching end point (length %d).", part1, start_room, part2, end_room, dun->tunn_n);

					/* Merge partitions */
					for (i = 0; i < dun->cent_n; i++)
					{
						if (dun->part[i] == part2) dun->part[i] = part1;
					}
					
					/* Paranoia */
					abort_and_cleanup = FALSE;
				}
			}

			/* Accept tunnel */
			break;
		}
	}


	/* We have to cleanup some stuff before returning if we abort */	
	if (abort_and_cleanup)
	{
		/* Clear intersections and decorations */
		dun->door_n = first_door;
		dun->next_n = first_next;
		dun->stair_n = first_stair;

		/* Remove the solid walls we applied */
		for (i = 0; i < dun->solid_n; i++)
		{
			/* Get the grid */
			y = dun->solid[i].y;
			x = dun->solid[i].x;

			cave_set_feat(y, x, dun->solid_feat[i]);
		}

		return FALSE;
	}

	/* Rewrite tunnel to room if we end up on a non-floor, or we are a sewer level, or the start or end tunnel is not a room, or 20% of the time otherwise */
	if ((cave_feat[row1][col1] != FEAT_FLOOR) || (level_flag & (LF1_SEWER)) || (!start_room) || (!end_room) || (rand_int(100) < 20))
	{
		/* Hack -- overwrite half of tunnel */
		if ((start_tunnel) || (!start_room))
		{
			/* Round up some times */
			if (((dun->tunn_n - first_tunn) % 2) && (rand_int(100) < 50)) first_tunn++;

			/* Adjust from half-way */
			first_tunn = first_tunn + (dun->tunn_n - first_tunn) / 2;
		}

		/* Remove tunnel terrain */
		if ((!start_room) || (!end_room))
		{
			for (i = ((!start_room) ? 0 : first_tunn); i < ((!end_room) ? dun->tunn_n : first_tunn); i++)
			{
				dun->tunn_feat[i] = 0;
			}
		}

		/* Overwrite starting tunnel terrain with end tunnel terrain */
		if ((end_room) && (end_room < DUN_ROOMS) && (room_info[end_room].theme[THEME_TUNNEL]))
		{
			for (i = first_tunn; i < dun->tunn_n; i++)
			{
				if (dun->tunn_feat[i]) dun->tunn_feat[i] = room_info[end_room].theme[THEME_TUNNEL];
			}
		}
	}
	
	/* If the ending room has decorations next to doors, overwrite the start */
	if ((end_room) && (end_room < DUN_ROOMS) && (room_info[end_room].theme[THEME_SOLID]) && (dun->next_n < NEXT_MAX))
	{
		int j;

		for (j = first_next; j < dun->next_n; j++)
		{
			/* Overwrite with alternate terrain from ending room later */
			dun->next_feat[j] = room_info[end_room].theme[THEME_SOLID];
		}
	}
	/* If ending room does not have decorations and neither does start, clear the above 'fake' decorations */
	else if ((start_room) && !(room_info[start_room].theme[THEME_SOLID]) && (start_room != end_room))
	{
		dun->next_n = first_next;
	}

	/* Get end decor */
	if ((end_room) && (end_room < DUN_ROOMS))
	{
		end_decor = room_info[end_room].theme[THEME_SOLID];
	}	

	/* Turn the tunnel into corridor */
	for (i = 0; i < dun->tunn_n; i++)
	{
		/* Get the grid */
		y = dun->tunn[i].y;
		x = dun->tunn[i].x;

		/* Apply feature - note hack */
		if (dun->tunn_feat[i] > 1)
		{
			/* Clear previous contents, write terrain */
			cave_set_feat(y, x, dun->tunn_feat[i]);
			
			continue;
		}	
			
		/* Apply sewer feature or flood corridor */
		if ((flood_tunnel) || ((dun->tunn_feat[i] == 1) && (level_flag & (LF1_SEWER))))
		{
			/* Guarantee flooded feature */
			while (!dun->flood_feat)
			{
				dun->flood_feat = pick_proper_feature(cave_feat_pool);
			}

			/* Clear previous contents, write terrain */
			build_terrain(y, x, dun->flood_feat);

			/*
			 * Only accept movement squares terrain in tunnels.
			 * Never fill tunnels with terrain that hits adjacent grids.
			 */
			if (((f_info[cave_feat[y][x]].flags1 & (FF1_MOVE))
				|| (f_info[cave_feat[y][x]].flags3 & (FF3_EASY_CLIMB)))
				&& ((f_info[dun->flood_feat].flags3 & (FF3_SPREAD | FF3_ADJACENT | FF3_ERUPT)) == 0))
			{
				continue;
			}

			/* Try the edge of the terrain instead */
			else if ((f_info[f_info[dun->flood_feat].edge].flags1 & (FF1_MOVE)) ||
				(f_info[f_info[dun->flood_feat].edge].flags3 & (FF3_EASY_CLIMB)))
			{
				cave_set_feat(y, x, f_info[dun->flood_feat].edge);
				continue;
			}

			/* Force terrain for sewers */
			else if ((dun->tunn_feat[i] == 1) && (level_flag & (LF1_SEWER)))
			{
				continue;
			}
		}
		
		/* Apply bridge */
		if (f_info[cave_feat[y][x]].flags2 & (FF2_BRIDGE))
		{
			/* Bridge previous contents */
			cave_alter_feat(y, x, FS_BRIDGE);
			
			continue;
		}
		
		/* Apply tunnel */
		if (f_info[cave_feat[y][x]].flags1 & (FF1_TUNNEL))
		{
			/* Tunnel previous contents */
			cave_alter_feat(y, x, FS_TUNNEL);
			
			continue;
		}
	}

	/* Apply the piercings that we found */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Get the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* Flood corridor */
		if (flood_tunnel)
		{
			/* Clear previous contents, write terrain */
			build_terrain(y, x, dun->flood_feat);

			/* Only accept movement squares in tunnels */
			if (((f_info[cave_feat[y][x]].flags1 & (FF1_MOVE))
				|| (f_info[cave_feat[y][x]].flags3 & (FF3_EASY_CLIMB)))
				&& ((f_info[dun->flood_feat].flags3 & (FF3_SPREAD | FF3_ADJACENT | FF3_ERUPT)) == 0))
			{
				continue;
			}

			/* Try the edge of the terrain instead */
			else if ((f_info[f_info[dun->flood_feat].edge].flags1 & (FF1_MOVE)) ||
				(f_info[f_info[dun->flood_feat].edge].flags3 & (FF3_EASY_CLIMB)))
			{
				cave_set_feat(y, x, f_info[dun->flood_feat].edge);
				continue;
			}
		}
		
		/* Convert to doorway if an outer wall */
		if ((f_info[cave_feat[y][x]].flags1 & (FF1_OUTER)) != 0)
		{
			cave_alter_feat(y, x, FS_DOOR);
		}
		
		/* Convert to floor grid */
		else cave_set_feat(y, x, FEAT_FLOOR);

		/* Occasional doorway */
		if (rand_int(100) < DUN_TUN_PEN)
		{
			/* Place a random door */
			place_random_door(y, x);
		}

		/* Place identical doors if next set of doors is adjacent */
		while ((i < dun->wall_n - 1) && (ABS(dun->wall[i+1].y - y) <= 1) && (ABS(dun->wall[i+1].x - x) <= 1))
		{
			cave_set_feat(dun->wall[i+1].y, dun->wall[i+1].x, cave_feat[y][x]);
			i++;
		}
	}
	
	/* Set up decorations */
	if (!start_decor) start_decor = end_decor;
	if (!end_decor) end_decor = start_decor;
	
	/*
	 * Hack -- 'no movement'/'no spread' tunnels get flood terrain
	 * added as wall decorations instead.
	 */
	if ((flood_tunnel) && ((((f_info[dun->flood_feat].flags1 & (FF1_MOVE)) == 0)
		&& ((f_info[dun->flood_feat].flags3 & (FF3_EASY_CLIMB)) == 0))
		|| ((f_info[dun->flood_feat].flags3 & (FF3_SPREAD | FF3_ADJACENT | FF3_ERUPT)) != 0)))
	{
		start_decor = dun->flood_feat;
		end_decor = dun->flood_feat;
	}

	/* Apply the decorations that we need */
	if (start_decor) for (i = 0; i < dun->decor_n; i++)
	{
		/* Get the grid */
		y = dun->decor[i].y;
		x = dun->decor[i].x;

		/* TODO: Should do something smarter here */
		/*
		 * We need to walk the decoration back to the previous location if required here
		 */
		if ((f_info[cave_feat[y][x]].flags1 & (FF1_WALL)) == 0) continue;
		
		/* Prevent vents and other movable paths getting overwritten */
		if ((f_info[cave_feat[y][x]].flags1 & (FF1_MOVE)) != 0) continue;
		if ((f_info[cave_feat[y][x]].flags3 & (FF3_EASY_CLIMB)) != 0) continue;

		/* Convert to decor grid */
		if (dun->decor_t[i] < dun->tunn_n / 2)
		{
			cave_set_feat(y, x, start_decor);
		}
		else
		{
			cave_set_feat(y, x, end_decor);			
		}
	}

	/* Clear stairs if flooding tunnel */
	if (flood_tunnel) dun->stair_n = first_stair;
	
	return TRUE;
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
static bool try_door(int y, int x)
{
	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Ignore walls */
	if (!(f_info[cave_feat[y][x]].flags1 & (FF1_WALL))) return (FALSE);

	/* Ignore room grids */
	if (cave_info[y][x] & (CAVE_ROOM)) return (FALSE);

	/* Occasional door (if allowed) */
	if ((rand_int(100) < DUN_TUN_JCT) && possible_doorway(y, x))
	{
		/* Place a door */
		place_random_door(y, x);

		return (TRUE);
	}

	return (FALSE);
}



/*
 * Type 0 room. Tunnel termination point.
 */
static bool build_type0(int room, int type)
{
	int y0, x0;

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, 1, 1)) return (FALSE);

	/* Paranoia */
	if (room < DUN_ROOMS)
	{
		/* Type */
		room_info[room].type = type;

		/* Terminate index list */
		room_info[room].section[0] = -1;
	}
	
	/* Good spot for a player/stair */
	add_stair(y0, x0);

	return (TRUE);
}


/*
 * Type 1, 2 & 3 room. Overlapping, with no 'visible' feature (1), feature on walls (2) or in centre (3).
 */
static bool build_type123(int room, int type)
{
	int y0, x0;
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;
	int height, width;
	int symetry = rand_int(100);

	bool light = FALSE;
	int spacing = 1;
	bool pillars = ((level_flag & (LF1_CRYPT)) != 0) || (rand_int(20) == 0);

	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;

	/* Determine extents of room (a) */
	y1a = randint(4);
	x1a = randint(13);
	y2a = randint(3);
	x2a = randint(9);

	/* Determine extents of room (b) */
	y1b = randint(3);
	x1b = randint(9);
	y2b = randint(4);
	x2b = randint(13);

	/* Sometimes express symetry */
	if (symetry < 30)
	{
		x1a = x2a; x2b = x1b;
	}

	/* Sometimes express symetry */
	if ((symetry < 15) || (symetry > 85))
	{
		y1a = y2a; y2b = y1b;
	}

	/* Calculate dimensions */
	height = MAX(y1a, y1b) + MAX(y2a, y2b) + 3;
	width = MAX(x1a, x1b) + MAX(x2a, x2b) + 3;

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);
	
	/* Rebalance centre */
	y0 -= (1 + MAX(y2a, y2b) - MAX(y1a, y1b)) / 2;
	x0 -= (1 + MAX(x2a, x2b) - MAX(x1a, x1b)) / 2;
	
	/* locate room (a) */
	y1a = y0 - y1a - 1;
	x1a = x0 - x1a - 1;
	y2a = y0 + y2a + 1;
	x2a = x0 + x2a + 1;

	/* locate room (b) */
	y1b = y0 - y1b - 1;
	x1b = x0 - x1b - 1;
	y2b = y0 + y2b + 1;
	x2b = x0 + x2b + 1;

	/* Build an overlapping room with the above shape */
	if (!build_overlapping(room, type, y1a, x1a, y2a, x2a, y1b, x1b, y2b, x2b, light, spacing, 1, NUM_SCATTER, pillars)) return (FALSE);

	return (TRUE);
}


/*
 * Type 4 & 5 room. Double-height overlapping, with feature on walls (4) or in the centre (5).
 */
static bool build_type45(int room, int type)
{
	int y0, x0;
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;
	int height, width;
	int symetry = rand_int(100);

	bool light = FALSE;
	int spacing = 2 + rand_int(2);
	bool pillars = TRUE;

	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;

	/* Determine extents of room (a) */
	y1a = randint(13);
	x1a = randint(13) + 6;
	y2a = randint(9);
	x2a = randint(9) + 5;

	/* Determine extents of room (b) */
	y1b = randint(9);
	x1b = randint(9) + 5;
	y2b = randint(13);
	x2b = randint(13) + 6;

	/* Sometimes express symetry */
	if (symetry < 30)
	{
		x1a = x2a; x2b = x1b;
	}

	/* Sometimes express symetry */
	if ((symetry < 20) || (symetry > 90))
	{
		y1a = y2a; y2b = y1b;
	}

	/* Calculate dimensions */
	height = MAX(y1a, y1b) + MAX(y2a, y2b) + 3;
	width = MAX(x1a, x1b) + MAX(x2a, x2b) + 3;

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* Rebalance centre */
	y0 -= (1 + MAX(y2a, y2b) - MAX(y1a, y1b)) / 2;
	x0 -= (1 + MAX(x2a, x2b) - MAX(x1a, x1b)) / 2;
	
	/* locate room (a) */
	y1a = y0 - y1a - 1;
	x1a = x0 - x1a - 1;
	y2a = y0 + y2a + 1;
	x2a = x0 + x2a + 1;

	/* locate room (b) */
	y1b = y0 - y1b - 1;
	x1b = x0 - x1b - 1;
	y2b = y0 + y2b + 1;
	x2b = x0 + x2b + 1;

	/* Build an overlapping room with the above shape */
	if (!build_overlapping(room, type, y1a, x1a, y2a, x2a, y1b, x1b, y2b, x2b, light, spacing, 1, NUM_SCATTER + 3, pillars)) return (FALSE);

	return (TRUE);
}



/*
 * Type 6 room. Triple-height, triple-width overlapping, with feature in the centre.
 */
static bool build_type6(int room, int type)
{
	int y0, x0;
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;
	int height, width;
	int symetry = rand_int(100);

	bool light = FALSE;
	int spacing = 4 + rand_int(4);
	bool pillars = TRUE;
	int scale = 1;

	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;

	/* Occasional fat pillars */
	if (rand_int(100) < p_ptr->depth)
	{
		scale++;
		spacing += 4;
	}
	
	/* Determine extents of room (a) */
	y1a = randint(8) + 4;
	x1a = randint(26) + 13;
	y2a = randint(6) + 3;
	x2a = randint(18) + 9;

	/* Determine extents of room (b) */
	y1b = randint(6) + 3;
	x1b = randint(18) + 9;
	y2b = randint(8) + 4;
	x2b = randint(26) + 13;

	/* Sometimes express symetry */
	if (symetry < 40)
	{
		x1a = x2a; x2b = x1b;
	}

	/* Sometimes express symetry */
	if ((symetry < 30) || (symetry > 90))
	{
		y1a = y2a; y2b = y1b;
	}

	/* Calculate dimensions */
	height = MAX(y1a, y1b) + MAX(y2a, y2b) + 3;
	width = MAX(x1a, x1b) + MAX(x2a, x2b) + 3;

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* Rebalance centre */
	y0 -= (1 + MAX(y2a, y2b) - MAX(y1a, y1b)) / 2;
	x0 -= (1 + MAX(x2a, x2b) - MAX(x1a, x1b)) / 2;
	
	/* locate room (a) */
	y1a = y0 - y1a - 1;
	x1a = x0 - x1a - 1;
	y2a = y0 + y2a + 1;
	x2a = x0 + x2a + 1;

	/* locate room (b) */
	y1b = y0 - y1b - 1;
	x1b = x0 - x1b - 1;
	y2b = y0 + y2b + 1;
	x2b = x0 + x2b + 1;

	/* Build an overlapping room with the above shape */
	if (!build_overlapping(room, type, y1a, x1a, y2a, x2a, y1b, x1b, y2b, x2b, light, spacing, scale, NUM_SCATTER + 8, pillars)) return (FALSE);

	return (TRUE);
}


/*
 * Type 7 room. Chambers. Build chambers of random size with 30 monsters.
 */
static bool build_type7(int room, int type)
{
	int y0, x0, height, width;
	int y1, x1, y2, x2;
	int size_mod = 0;
	bool flooded = ((room_info[room].flags & (ROOM_FLOODED)) != 0);

	/* Deeper in the dungeon, chambers are less likely to be lit. */
	bool light = (rand_range(25, 60) > p_ptr->depth) ? TRUE : FALSE;

	/* Rooms get (slightly) larger with depth */
	if (p_ptr->depth > rand_range(40, 140)) size_mod = 4;
	else size_mod = 3;

	/* Calculate the room size. */
	height = BLOCK_HGT * size_mod;
	width = BLOCK_WID * (size_mod + rand_int(3));

	/* Calculate additional space for moat */
	if (flooded)
	{
		height += 2 * BLOCK_HGT;
		width += 2 * BLOCK_WID;
	}

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* Calculate original room size */
	if (flooded)
	{
		height -= 2 * BLOCK_HGT;
		width -= 2 * BLOCK_WID;
	}

	/* Calculate the borders of the room. */
	y1 = y0 - (height / 2);
	x1 = x0 - (width / 2);
	y2 = y0 + (height - 1) / 2;
	x2 = x0 + (width - 1) / 2;

	/* Make certain the room does not cross the dungeon edge. */
	if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);

	/* Build the chambers */
	build_chambers(y1, x1, y2, x2, 30, light);

	/* Handle flooding */
	if (flooded)
	{
		/* Grow the moat around the chambers */
		y1 = MAX(y1 - BLOCK_HGT, 1);
		x1 = MAX(x1 - BLOCK_WID, 1);
		y2 = MIN(y2 + BLOCK_HGT, DUNGEON_HGT - 2);
		x2 = MIN(x2 + BLOCK_WID, DUNGEON_WID  - 2);

		/* Place the moat */
		generate_starburst_room(y1, x1, y2, x2, dun->flood_feat, f_info[dun->flood_feat].edge, STAR_BURST_RAW_EDGE | STAR_BURST_MOAT);
			
		/* Hack -- 'room' part is not flooded */
		room_info[room].flags &= ~(ROOM_FLOODED);
	}
	
	/* Set the chamber flags */
	set_room_flags(room, type, light);

	/* Paranoia */
	if (room < DUN_ROOMS)
	{
		/* Initialize room description */
		room_info[room].type = ROOM_CHAMBERS;
	}

	return (TRUE);
}


/*
 * Type 8, 9, 10. Either an interesting room, lesser or greater vault.
 */
static bool build_type8910(int room, int type)
{
	vault_type *v_ptr = NULL;
	int y0, x0;
	int limit = 0;
	int height, width;

	/* Hack -- no interesting rooms in flooded dungeon */
	if (type == ROOM_INTERESTING) room_info[room].flags &= ~(ROOM_FLOODED);

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Limit */
		if (limit++ > 500) return (FALSE);

		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* Accept the first room of this type */
		if (v_ptr->typ == type) break;
	}

	/* Get height and width */
	height = v_ptr->hgt;
	width = v_ptr->wid;

	/* Calculate additional space for moat */
	if ((room_info[room].flags & (ROOM_FLOODED)) != 0)
	{
		height += 2 * BLOCK_HGT;
		width += 2 * BLOCK_WID;
	}

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* Initialize room description */
	room_info[dun->cent_n].type = ROOM_INTERESTING + type - 8;

	/* Boost the rating */
	rating += v_ptr->rat;

	/* Hack -- Build the vault */
	build_vault(room, y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);

	/* Handle flooding */
	if (room_info[room].flags & (ROOM_FLOODED))
	{
		/* Grow the moat around the chambers */
		int y1 = MAX(y0 - height / 2, 1);
		int x1 = MAX(x0 - width / 2, 1);
		int y2 = MIN(y0 + (width + 1) / 2, DUNGEON_HGT - 2);
		int x2 = MIN(x0 + (width + 1) / 2, DUNGEON_WID  - 2);

		/* Place the moat */
		generate_starburst_room(y1, x1, y2, x2, dun->flood_feat, f_info[dun->flood_feat].edge, STAR_BURST_RAW_EDGE | STAR_BURST_MOAT);			

		/* Hack -- 'room' part is not flooded */
		room_info[room].flags &= ~(ROOM_FLOODED);
	}

	/* Set the vault / interesting room flags */
	set_room_flags(room, type, FALSE);

	/* Paranoia */
	if (room < DUN_ROOMS)
	{
		switch (type)
		{
			case 8:
				/* Initialize room description */
				room_info[room].type = ROOM_INTERESTING;

				break;

			case 9:
				/* Initialize room description */
				room_info[room].type = ROOM_LESSER_VAULT;

				break;

			case 10:
				/* Initialize room description */
				room_info[room].type = ROOM_GREATER_VAULT;

				break;
		}
	}

	return (TRUE);
}


/*
 * Type 11 or 12. Small or large starburst.
 */
static bool build_type1112(int room, int type)
{
	int y0, x0, dy, dx;

	/* Deeper in the dungeon, starbursts are less likely to be lit. */
	bool light = (rand_range(25, 60) > p_ptr->depth) ? TRUE : FALSE;

	switch (type)
	{
		case 14: case 13: dy = 19; dx = 33; break;
		default: dy = 9; dx = 14; break;
	}

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, dy * 2 + 1, dx * 2 + 1)) return (FALSE);

	/* Try building starburst */
	build_type_starburst(room, type, y0, x0, dy, dx, light);

	return (TRUE);
}


/*
 * Type 13, 14, 15. Small, medium or large fractal.
 */
static bool build_type131415(int room, int type)
{
	int y0, x0, height, width;

	byte fractal_type;

	/* Deeper in the dungeon, starbursts are less likely to be lit. */
	bool light = (rand_range(25, 60) > p_ptr->depth) ? TRUE : FALSE;

	switch (type)
	{
		case ROOM_HUGE_FRACTAL: fractal_type = FRACTAL_TYPE_33x65; height = 33; width = 65; break;
		case ROOM_LARGE_FRACTAL: fractal_type = FRACTAL_TYPE_33x33; height = 33; width = 33; break;
		default: fractal_type = FRACTAL_TYPE_17x33; height = 17; width = 33; break;
	}

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* Build fractal */
	if (!build_type_fractal(room, type, y0, x0, fractal_type, light)) return (FALSE);

	return (TRUE);
}


/*
 * Type 16. Lair.
 */
static bool build_type16(int room, int type)
{
	int y0, x0, height, width;
	int y1, x1, y2, x2;
	int size_mod = 0;

	int feat;
	
	u32b flag = (STAR_BURST_RAW_EDGE | STAR_BURST_MOAT | STAR_BURST_LIGHT);
	
	/* Deeper in the dungeon, lairs are less likely to be lit. */
	bool light = (rand_range(25, 60) > p_ptr->depth) ? TRUE : FALSE;

	/* Get a feature to surround the lair */
	feat = pick_proper_feature(cave_feat_lake);

	/* Lairs are always constant size */
	size_mod = 2;

	/* Lairs are never flooded */
	room_info[room].flags &= ~(ROOM_FLOODED);

	/* Calculate the room size. */
	height = BLOCK_HGT * size_mod;
	width = BLOCK_WID * (size_mod + rand_int(3));

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height + 2 * BLOCK_HGT, width + 2 * BLOCK_WID)) return (FALSE);

	/* Calculate the borders of the room. */
	y1 = y0 - (height / 2);
	x1 = x0 - (width / 2);
	y2 = y0 + (height - 1) / 2;
	x2 = x0 + (width - 1) / 2;

	/* Make certain the room does not cross the dungeon edge. */
	if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);

	/* Build the chambers */
	build_chambers(y1, x1, y2, x2, 30, light);

	/* Grow the moat around the chambers */
	y1 = MAX(y1 - BLOCK_HGT * 1, 1);
	x1 = MAX(x1 - BLOCK_WID * 1, 1);
	y2 = MIN(y2 + BLOCK_HGT * 1, DUNGEON_HGT - 2);
	x2 = MIN(x2 + BLOCK_WID * 1, DUNGEON_WID  - 2);

	/* Place the moat */
	generate_starburst_room(y1, x1, y2, x2,
		feat, f_info[feat].edge, flag);

	/* Set the chamber flags */
	set_room_flags(room, type, light);

	/* Paranoia */
	if (dun->cent_n < DUN_ROOMS)
	{
		/* Initialize room description */
		room_info[dun->cent_n].type = ROOM_LAIR;
	}
	
	return (TRUE);
}



/*
 * Type 17, 18, 19. Small, medium or large maze.
 */
static bool build_type171819(int room, int type)
{
	int y0, x0, width_wall, width_path, ydim, xdim, height, width;
	
	int y1, x1, y2, x2;
	
	int pool = 0;
	bool flooded = ((room_info[room].flags & (ROOM_FLOODED)) != 0);
	
	/* Maze flags */
	u32b maze_flags = (MAZE_OUTER_N | MAZE_OUTER_S | MAZE_OUTER_E | MAZE_OUTER_W | MAZE_WALL | MAZE_ROOM | MAZE_FILL);

	/* Occasional light */
	if (p_ptr->depth <= randint(25)) maze_flags |= (MAZE_LITE);

	/* Occasional loop */
	if (p_ptr->depth <= randint(25)) maze_flags |= (MAZE_LOOP);

	/* Occasionally fill in dead-ends */
	if (randint(100) < p_ptr->depth) maze_flags |= (MAZE_DEAD | MAZE_SAVE);
	
	/* Hack -- maze and no theme: pick one at random */
	if ((level_flag & (LF1_THEME)) == 0)
	{
		switch(randint(5))
		{
			case 1: level_flag |= (LF1_LABYRINTH); break;
			case 2: level_flag |= (LF1_CRYPT); break;
			case 3: level_flag |= (LF1_CAVE); break;
			case 4: level_flag |= (LF1_STRONGHOLD); break;
			case 5: level_flag |= (LF1_SEWER); break;
		}
	}
	
	switch (type)
	{
		case ROOM_HUGE_MAZE: width_wall = rand_int(9) / 3 + 1; width_path = rand_int(15) / 6 + 1; height = 33; width = 65; level_flag |= (LF1_LABYRINTH); break;
		case ROOM_LARGE_MAZE: width_wall = rand_int(15) / 4 + 1; width_path = 1; height = 33; width = 33; break;
		default: width_wall = rand_int(9) / 6 + 1; width_path = rand_int(9) / 6 + 1; height = 17; width = 33; break;
	}
	
	/* Ensure bigger paths on stronghold levels. This allows these corridors to connect correctly to the maze. */
	if (level_flag & (LF1_STRONGHOLD)) width_path++;
	
	/* Fill crypt mazes with pillars */
	else if (level_flag & (LF1_CRYPT))
	{
		width_path = 3;
		maze_flags |= (MAZE_CRYPT);
	}
	
	/* Fill cave mazes with lots of twisty passages */
	else if (level_flag & (LF1_CAVE))
	{
		width_path = 1;
		width_wall++;
		maze_flags |= (MAZE_CAVE);
	}

	/* Calculate maze size */
	ydim = (height + width_wall - 1) / (width_wall + width_path);
	xdim = (width + width_wall - 1) / (width_wall + width_path);
	
	/* Ensure minimums */
	if (ydim < 3) ydim = 3;
	if (xdim < 3) xdim = 3;	
	
	/* Recalculate height and width based on maze size */
	height = (ydim * width_path) + ((ydim - 1) * width_wall) + 2;
	width = (xdim * width_path) + ((xdim - 1) * width_wall) + 2;
	
	/* Calculate additional space for moat */
	if (flooded)
	{
		height += 2 * BLOCK_HGT;
		width += 2 * BLOCK_WID;
	}

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* Calculate original room size */
	if (flooded)
	{
		height -= 2 * BLOCK_HGT;
		width -= 2 * BLOCK_WID;
	}
	
	/* Dimensions of room */
	y1 = y0 - (height / 2);
	x1 = x0 - (width / 2);
	y2 = y0 + (height - 1) / 2;
	x2 = x0 + (width - 1) / 2;

	/* Flooded maze */
	if (flooded)
	{
		/* Flood floor */
		pool = dun->flood_feat;
		
		if (pool) maze_flags |= (MAZE_FLOOD);
	}
	/* Sewer maze */
	else if (level_flag & (LF1_SEWER))
	{
		/* Guarantee flooded feature */
		while (!pool)
		{
			pool = pick_proper_feature(cave_feat_pool);
		}
		
		maze_flags |= (MAZE_POOL);
	}
	
	/* Draw maze */
	if (draw_maze(y1, x1, y2, x2, FEAT_WALL_OUTER, FEAT_FLOOR, width_wall, width_path, pool, maze_flags))
	{		
		/* Handle flooding */
		if (flooded)
		{
			/* Grow the moat around the chambers */
			y1 = MAX(y1 - BLOCK_HGT, 1);
			x1 = MAX(x1 - BLOCK_WID, 1);
			y2 = MIN(y2 + BLOCK_HGT, DUNGEON_HGT - 2);
			x2 = MIN(x2 + BLOCK_WID, DUNGEON_WID  - 2);
	
			/* Place the moat */
			generate_starburst_room(y1, x1, y2, x2, dun->flood_feat, f_info[dun->flood_feat].edge, STAR_BURST_RAW_EDGE | STAR_BURST_MOAT);
		}
	
		/* Set the vault / interesting room flags */
		set_room_flags(room, type, FALSE);
		
		/* Hack -- 'room' part is not flooded */
		room_info[room].flags &= ~(ROOM_FLOODED);

		return (TRUE);
	}

	/* Failed */
	return (FALSE);
}




/*
 * Attempt to build a room of the given type.
 */
static bool room_build(int room, int type)
{
	/* Generating */
	if (cheat_room) msg_format("Building room type %d.", type);
	
	/* Flood if required */
	if (dun->flood_feat)
	{
		/* Always flood the first room */
		if (room == 1) room_info[room].flags |= (ROOM_FLOODED);
		
		/* Flood additional rooms if deep in dungeon - up to 2/3rds of all rooms */
		else if ((room % 3) && (room < (p_ptr->depth - f_info[dun->flood_feat].level) / 5)) room_info[room].flags |= (ROOM_FLOODED);
		
		/* This floods 1 room, plus a second room if terrain is more than 10 levels shallow, plus a third room if terrain is more than
		 * 20 levels shallow, then 25, 35, 40, 50, 55 etc. */
	}
	
	/* Build a room */
	switch (type)
	{
		/* Build an appropriate room */
		case ROOM_HUGE_MAZE: if (build_type171819(room,type)) return(TRUE); break;
		case ROOM_LARGE_MAZE: if (build_type171819(room,type)) return(TRUE); break;
		case ROOM_MAZE: if (build_type171819(room,type)) return(TRUE); break;
		case ROOM_LAIR: if (build_type16(room, type)) return(TRUE); break;
		case ROOM_HUGE_FRACTAL: if (build_type131415(room, type)) return(TRUE); break;
		case ROOM_LARGE_FRACTAL: if (build_type131415(room, type)) return(TRUE); break;
		case ROOM_FRACTAL: if (build_type131415(room, type)) return(TRUE); break;
		case ROOM_HUGE_STAR_BURST: if (build_type1112(room, type)) return(TRUE); break;
		case ROOM_STAR_BURST: if (build_type1112(room, type)) return(TRUE); break;
		case ROOM_GREATER_VAULT: if (build_type8910(room, type)) return(TRUE); break;
		case ROOM_LESSER_VAULT: if (build_type8910(room, type)) return(TRUE); break;
		case ROOM_INTERESTING: if (build_type8910(room, type)) return(TRUE); break;
		case ROOM_CHAMBERS: if (build_type7(room, type)) return(TRUE); break;
		case ROOM_HUGE_CENTRE: if (build_type6(room, type)) return(TRUE); break;
		case ROOM_LARGE_CENTRE: if (build_type45(room, type)) return(TRUE); break;
		case ROOM_LARGE_WALLS: if (build_type45(room, type)) return(TRUE); break;
		case ROOM_NORMAL_CENTRE: if (build_type123(room, type)) return(TRUE); break;
		case ROOM_NORMAL_WALLS: if (build_type123(room, type)) return(TRUE); break;
		case ROOM_NORMAL: if (build_type123(room, type)) return(TRUE); break;

		default: if (build_type0(room, type)) return(TRUE); break;
	}

	/* Failure */
	return (FALSE);
}



/*
 * Place lakes and rivers given a feature
 */
static bool build_feature(int feat, bool do_big_lake, bool merge_lakes)
{
	/* No coordinates yet */
	int x0 = 0, y0 = 0;

	/* Build a lake? */
	if (((f_info[feat].flags2 & (FF2_LAKE)) != 0) || ((f_info[feat].flags2 & (FF2_RIVER)) == 0))
	{
		/* Try to place the lake. Get its center */
		if (!build_lake(feat, do_big_lake, merge_lakes, &y0, &x0)) return (FALSE);
	}

	/* Build a river */
	if ((f_info[feat].flags2 & (FF2_RIVER)) != 0)
	{
		/* Pick starting coordinates, if needed */
		if ((y0 + x0) == 0)
		{
			y0 = randint(DUNGEON_HGT - 2);
			x0 = randint(DUNGEON_WID - 2);
		}

		/* Generate the river */
		build_river(feat, y0, x0);
	}
	
	/* Ensure interesting stuff in lake */
	dun->lake[dun->lake_n].y = y0;
	dun->lake[dun->lake_n++].x = x0;

	/* Success */
	return (TRUE);
}


/*
 * Build lakes and rivers for the dungeon
 */
static void build_nature(void)
{
	bool big = FALSE;
	bool done_big = FALSE;

	int feat;

	/* Flavor */
	bool merge_lakes = ((p_ptr->depth >= 30) && !rand_int(7));

	/* Get dungeon zone */
	dungeon_zone *zone=&t_info[0].zone[0];

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Allocate some lakes and rivers */
	for (dun->lake_n = 0; dun->lake_n < DUN_MAX_LAKES; )
	{
		/* Have placed features */
		if ((rand_int(100) >= ((level_flag & (LF1_SURFACE)) ? DUN_NATURE_SURFACE : DUN_NATURE_DEEP)) && !(zone->big) && !(zone->small))
		{
			break;
		}

		if ((dun->lake_n == 0) && (zone->big))
		{
			feat = zone->big;
			big = TRUE;
		}
		else if (zone->small)
		{
			feat = zone->small;
		}
		else
		{
			/* Pick a feature */
			feat = pick_proper_feature(cave_feat_lake);
		}

		/* Got a valid feature? */
		if (feat)
		{
			int dummy = feat;
			
			/* Try a big lake */
			if (!done_big)
			{
				big = (randint(150) < p_ptr->depth);

				/* Got one */
				done_big = big;
			}
			else
			{
				/* We have a big one already */
				big = FALSE;
			}

			/* On surface or dungeon filling ? */
			if ((level_flag & (LF1_SURFACE)) ||
				(((f_info[feat].flags1 & (FF1_WALL)) != 0) && !(variable_terrain(&dummy, feat))))
			{ 
				/* Report creation of lakes/rivers */
				if (cheat_room)
				{
					if (f_info[feat].edge)
					{
						msg_format("Building %s%s surrounded by %s.",
								big ? "big ": "", f_name + f_info[feat].name, f_name + f_info[f_info[feat].edge].name);
					}
					else
					{
						msg_format ("Building %s%s.", big ? "big ": "", f_name + f_info[feat].name);
					}
				}

				/* Build one lake/river on surface */
				if (!build_feature(feat, big, merge_lakes)) break;
			}		
			else
			{
				/* Report creation of lakes/rivers */
				if (cheat_room)
				{
					msg_format ("Flooding dungeon with %s.", f_name + f_info[feat].name);
				}									
				
				/* Flood the dungeon */
				dun->flood_feat = feat;
				
				break;
			}
		}
	}
}



/*
 * Value "1" means the grid will be changed, value "0" means it won't.
 *
 * We have 47 entries because 47 is not divisible by any reasonable
 * figure for streamer width.
 */
static bool streamer_change_grid[47] =
{
	0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1,
	1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0
};


/*
 * Places "streamers" of rock through dungeon.
 *
 * Note that there are actually six different terrain features used
 * to represent streamers.  Three each of magma and quartz, one for
 * basic vein, one with hidden gold, and one with known gold.  The
 * hidden gold types are currently unused.
 */
static void build_streamer(int feat)
{
	int table_start;
	int i;
	int y, x, dy, dx;
	int start_dir, dir;
	int out1, out2;
	bool change;

	/* Get chance */
	int chance = f_info[feat_state(feat, FS_STREAMER)].rarity;

	/* Initialize time until next turn, and time until next treasure */
	int time_to_treas = randint(chance * 2);
	int time_to_turn = randint(DUN_STR_CHG * 2);

	/* Set standard width.  Vary width sometimes. */
	int width = 2 * DUN_STR_WID + 1;
	if (!rand_int(6)) width += randint(3);
	else if (!rand_int(6)) width -= randint(3);
	if (width < 1) width = 1;

	/* Set expansion outward from centerline. */
	out1 = width / 2;
	out2 = (width + 1) / 2;


	/* Hack -- Choose starting point */
	y = rand_spread(DUNGEON_HGT / 2, DUNGEON_HGT / 4);
	x = rand_spread(DUNGEON_WID / 2, DUNGEON_WID / 4);

	/* Choose a random compass direction */
	dir = start_dir = ddd[rand_int(8)];

	/* Get an initial start position on the grid alteration table. */
	table_start = rand_int(47);

	/* Place streamer into dungeon */
	while (TRUE)
	{
		/* Advance streamer width steps on the table. */
		table_start += width;

		/*
		 * Change grids outwards along sides.  If moving diagonally,
		 * change a cross-shaped area.
		 */
		if (ddy[dir])
		{
			for (dx = x - out1; dx <= x + out2; dx++)
			{
				/* Stay within dungeon. */
				if (!in_bounds(y, dx)) continue;

				/* Only convert "granite" walls */
				if (cave_feat[y][dx] < FEAT_WALL_EXTRA) continue;
				if (cave_feat[y][dx] > FEAT_WALL_SOLID) continue;

				/* Skip vaults and whatnot */
				if (room_has_flag(y, x, ROOM_ICKY)) continue;

				i = table_start + dx - x;

				if ((i < 47) && (i >= 0)) change = streamer_change_grid[i];
				else change = streamer_change_grid[i % 47];

				/* No change to be made. */
				if (!change) continue;

				/* Clear previous contents, add proper vein type */
				cave_set_feat(y, dx, feat);

				/* Count down time to next treasure. */
				time_to_treas--;

				/* Hack -- Add some (known) treasure */
				if (time_to_treas == 0)
				{
					time_to_treas = randint(chance * 2);
					cave_feat[y][dx] = feat_state(feat, FS_STREAMER);
				}
			}
		}

		if (ddx[dir])
		{
			for (dy = y - out1; dy <= y + out2; dy++)
			{
				/* Stay within dungeon. */
				if (!in_bounds(dy, x)) continue;

				/* Only convert "granite" walls */
				if (cave_feat[dy][x] < FEAT_WALL_EXTRA) continue;
				if (cave_feat[dy][x] > FEAT_WALL_SOLID) continue;

				i = table_start + dy - y;

				if ((i < 47) && (i >= 0)) change = streamer_change_grid[i];
				else change = streamer_change_grid[i % 47];

				/* No change to be made. */
				if (!change) continue;

				/* Clear previous contents, add proper vein type */
				cave_set_feat(dy, x, feat);

				/* Count down time to next treasure. */
				time_to_treas--;

				/* Hack -- Add some (known) treasure */
				if (time_to_treas == 0)
				{
					time_to_treas = randint(chance * 2);
					cave_feat[dy][x] = feat_state(feat, FS_STREAMER);
				}
			}
		}

		/* Count down to next direction change. */
		time_to_turn--;

		/* Sometimes, vary direction slightly. */
		if (time_to_turn == 0)
		{
			/* Get time until next turn. */
			time_to_turn = randint(DUN_STR_CHG * 2);

			/* Randomizer. */
			i = rand_int(3);

			/* New direction is always close to start direction. */
			if (start_dir == 2)
			{
				if (i == 0) dir = 2;
				if (i == 1) dir = 1;
				else        dir = 3;
			}
			else if (start_dir == 8)
			{
				if (i == 0) dir = 8;
				if (i == 1) dir = 9;
				else        dir = 7;
			}
			else if (start_dir == 6)
			{
				if (i == 0) dir = 6;
				if (i == 1) dir = 3;
				else        dir = 9;
			}
			else if (start_dir == 4)
			{
				if (i == 0) dir = 4;
				if (i == 1) dir = 7;
				else        dir = 1;
			}
			else if (start_dir == 3)
			{
				if (i == 0) dir = 3;
				if (i == 1) dir = 2;
				else        dir = 6;
			}
			else if (start_dir == 1)
			{
				if (i == 0) dir = 1;
				if (i == 1) dir = 4;
				else        dir = 2;
			}
			else if (start_dir == 9)
			{
				if (i == 0) dir = 9;
				if (i == 1) dir = 6;
				else        dir = 8;
			}
			else if (start_dir == 7)
			{
				if (i == 0) dir = 7;
				if (i == 1) dir = 8;
				else        dir = 4;
			}
		}

		/* Advance the streamer */
		y += ddy[dir];
		x += ddx[dir];

		/* Stop at dungeon edge */
		if (!in_bounds(y, x)) break;
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
		x1 = rand_range(5, DUNGEON_WID-1 - 5);
		y1 = rand_range(5, DUNGEON_HGT-1 - 5);

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

				/* Destroy "outside" grids */
				if ((cave_valid_bold(y,x)) && (f_info[cave_feat[y][x]].flags3 & (FF3_OUTSIDE)))
				{
					/* Delete objects */
					delete_object(y, x);
	
					/* Burn stuff */
					if (f_info[cave_feat[y][x]].flags2 & (FF2_HURT_FIRE))
					{
						cave_alter_feat(y,x,FS_HURT_FIRE);
					}
					/* Don't touch chasms */
					else if (f_info[cave_feat[y][x]].flags2 & (FF2_CHASM))
					{
						/* Nothing */
					}
							/* Magma */
					else if (rand_int(100)< 15)
					{
						/* Create magma vein */
						cave_set_feat(y, x, FEAT_RUBBLE);
					}


				}
				/* Destroy valid grids */
				else if (cave_valid_bold(y, x))
				{
					/* Delete objects */
					delete_object(y, x);

					/* Wall (or floor) type */
					t = rand_int(200);

					/* Burn stuff */
					if (f_info[cave_feat[y][x]].flags2 & (FF2_HURT_FIRE))
					{
						cave_alter_feat(y,x,FS_HURT_FIRE);
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
					cave_info[y][x] &= ~(CAVE_ROOM);

					/* No longer illuminated */
					cave_info[y][x] &= ~(CAVE_GLOW);
				}
			}
		}
	}
}




/*
 *  Initialise a dungeon ecology based on a seed race.
 */
static void init_ecology(int r_idx)
{
	int i, j, k;
	int l = 0;
	int count = 0;

	/* Initialise the dungeon ecology */
	(void)WIPE(&cave_ecology, ecology_type);

	/* Count of different non-unique monsters in ecology */
	k = (MIN_ECOLOGY_RACES / 2) + rand_int((MIN_ECOLOGY_RACES + 1) / 2);
	
	/* Initialise ecology based on seed race */
	if (r_idx)
	{
		/* Get seed monster for ecology */
		get_monster_ecology(r_idx);
	}

	/* Get a seed monster for the ecology */
	else 
	{
		/* Set monster hook */
		get_mon_num_hook = dun_level_mon;

		/* Prepare allocation table */
		get_mon_num_prep();

		/* Get seed monster for ecology */
		get_monster_ecology(0);

		/* Clear monster hook */
		get_mon_num_hook = NULL;

		/* Prepare allocation table */
		get_mon_num_prep();
	}

	/* Get additional monsters for the ecology */
	while (TRUE)
	{
		/* Check ecology */
		for (i = l; i < cave_ecology.num_races; i++)
		{
			bool differs = TRUE;

			/* Don't count uniques */
			if ((r_info[cave_ecology.race[i]].flags1 & (RF1_UNIQUE)) != 0) continue;

			/* Check if appears already */
			for (j = 0; j < i; j++)
			{
				if (cave_ecology.race[i] == cave_ecology.race[j]) differs = FALSE;
			}

			/* Race not already counted */
			if (differs) k--;
		}
		
		/* Note last race checked */
		l = cave_ecology.num_races;
		
		/* Not enough different monsters */
		if ((k >= 0) && (cave_ecology.num_races < MAX_ECOLOGY_RACES) && (count++ < 100))
		{
			/* XXX Sometimes we have to cancel out due to not being able to
			 * find a dun_level_mon match for the level we are on.
			 */
			if (count < 10)
			{
				/* Set monster hook */
				get_mon_num_hook = dun_level_mon;
	
				/* Prepare allocation table */
				get_mon_num_prep();
			}
	
			/* Get seed monster for ecology */
			get_monster_ecology(0);
	
			if (get_mon_num_hook)
			{
				/* Clear monster hook */
				get_mon_num_hook = NULL;
	
				/* Prepare allocation table */
				get_mon_num_prep();
			}
		}
		else
		{
			break;
		}
	}

	/* Start the ecology */
	cave_ecology.ready = TRUE;
	cave_ecology.valid_hook = TRUE;
}


/*
 *  Place tower in the dungeon.
 */
static void place_tower()
{
	vault_type *v_ptr;
	int y, x;

	int by, bx;

	dungeon_zone *zone=&t_info[0].zone[0];

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Get the location of the tower */
	y = (DUNGEON_HGT) / 2;
	x = (DUNGEON_WID) / 2;

	/* Hack -- are we directly above another tower? */
	if ((p_ptr->depth == zone->level) && (p_ptr->depth > min_depth(p_ptr->dungeon)))
	{
		dungeon_zone *roof;
	
		get_zone(&roof,p_ptr->dungeon,p_ptr->depth-1);

		v_ptr = &v_info[roof->tower];

		build_roof(y, x, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
	}

	/* Get the vault */
	v_ptr = &v_info[zone->tower];

	/* Hack -- Build the tower */
	build_tower(y, x, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);

	/* Paranoia */
	if (dun->cent_n < CENT_MAX)
	{
		/* Set corridor here */
		dun->cent[dun->cent_n].y = y;
		dun->cent[dun->cent_n].x = x;
		dun->cent_n++;

		/* Reserve some blocks */
		for (by = (y - v_ptr->hgt / 2) / BLOCK_HGT; by <= (y + v_ptr->hgt / 2 + 1) / BLOCK_HGT; by++)
		{
			for (bx = (x - v_ptr->wid / 2) / BLOCK_WID; bx <= (x + v_ptr->wid / 2 + 1) / BLOCK_WID; bx++)
			{
				dun->room_map[by][bx] = TRUE;
	
				dun_room[by][bx] = dun->cent_n;
			}
		}

		/* Paranoia */
		if (dun->cent_n < DUN_ROOMS)
		{
			/* Initialise room description */
			room_info[dun->cent_n].type = ROOM_TOWER;

			/* Non-surface levels will be set to ROOM_ICKY at end of generation */
			room_info[dun->cent_n].flags = (level_flag & (LF1_SURFACE)) != 0 ? ROOM_ICKY : 0;

			room_info[dun->cent_n].theme[THEME_TUNNEL] = 0;
			room_info[dun->cent_n].theme[THEME_SOLID] = 0;
		}
	}

	/* Hack -- descending player always in tower */
	if ((level_flag & LF1_SURFACE) && ((f_info[p_ptr->create_stair].flags1 & (FF1_LESS)) != 0))
	{
		/* Clear previous contents, add dungeon entrance */
		place_random_stairs(y, x, FEAT_MORE);

		player_place(y, x, TRUE);
	}

	/* Hack -- always have upstairs in surface of tower */
	if ((level_flag & LF1_SURFACE) && (p_ptr->depth < max_depth(p_ptr->dungeon)))
	{
		feat_near(FEAT_LESS, y, x);
	}
}



/*
 *  Place rooms in the dungeon.
 */
static bool place_rooms()
{
	int i, j, k;

	int rooms_built = 0;

	/*
	 * Build each type of room in turn until we cannot build any more.
	 */
	for (i = ((level_flag & (LF1_ROOMS)) != 0) ? 0 : ROOM_MAX - 1;
			(i < ROOM_MAX) && (dun->cent_n < DUN_ROOMS - 1); i++)
	{
		/* What type of room are we building now? */
		int room_type = room_build_order[i];

		/* Check if this is the last room type we can place for this theme. If so, continue to place it. */
		bool last = FALSE;

		/* A series of checks for rooms only */
		if ((level_flag & (LF1_ROOMS)) != 0)
		{
			/* Skip at this depth */
			if (room_data[room_type].min_level > p_ptr->depth) continue;

			/* Skip if level themed and we don't match the theme */
			if (((level_flag & (LF1_THEME)) != 0) && ((room_data[room_type].theme & (level_flag)) == 0)) continue;

			/* Level is themed */
			if ((level_flag & (LF1_THEME)) != 0)
			{
				last = TRUE;

				/* Check remaining room types */
				for (k = i + 1; k < ROOM_MAX; k++)
				{
					/* Valid room type left */
					if ((room_data[room_build_order[k]].theme & (level_flag)) != 0) last = FALSE;
				}
			}
		}
		/* No rooms, just tunnels */
		else
		{
			/* Place tunnel endpoints only */
			room_type = 0;
		}

		/* Build the room. */
		while (((last) || (rand_int(100) < room_data[room_type].chance[p_ptr->depth < 100 ? p_ptr->depth / 10 : 10]))
			&& (rooms_built < room_data[room_type].max_number) && (dun->cent_n < DUN_ROOMS - 1) &&
				(room_build(dun->cent_n + 1, room_type)))
		{
			/* Built room */
			if (cheat_room) msg_format("Built room type %d.", room_type);

			/* Increase the room built count. */
			rooms_built += room_data[room_type].count_as;

			/* No theme chosen */
			if ((level_flag & (LF1_THEME)) == 0)
			{
				/* Hack -- no choice */
				int choice = -1;

				/* Reset count */
				k = 0;

				/* Pick a theme */
				for (j = 0; j < 32; j++)
				{
					/* Pick a theme */
					if ( ((room_data[room_type].theme & (1L << j)) != 0) && (rand_int(++k) == 0)) choice = j;
				}

				/* Set a theme if picked */
				if (choice >= 0)
				{
					level_flag |= (1L << choice);
				}
			}
		}
	}

	/* No tunnels if only zero or one room */
	if (dun->cent_n <= 1) level_flag &= ~(LF1_TUNNELS);

	/* Attempt successful */
	return (TRUE);
}



/*
 * Place boundary walls around the edge of the dungeon
 */
static void place_boundary_walls()
{
	int y, x;

	/* Special boundary walls -- Top */
	for (x = 0; x < DUNGEON_WID; x++)
	{
		y = 0;

		cave_feat[y][x] = FEAT_PERM_SOLID;

		cave_info[y][x] |= (CAVE_XLOS);
		cave_info[y][x] |= (CAVE_XLOF);
	}

	/* Special boundary walls -- Bottom */
	for (x = 0; x < DUNGEON_WID; x++)
	{
		y = DUNGEON_HGT-1;

		cave_feat[y][x] = FEAT_PERM_SOLID;

		cave_info[y][x] |= (CAVE_XLOS);
		cave_info[y][x] |= (CAVE_XLOF);
	}

	/* Special boundary walls -- Left */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		x = 0;

		cave_feat[y][x] = FEAT_PERM_SOLID;

		cave_info[y][x] |= (CAVE_XLOS);
		cave_info[y][x] |= (CAVE_XLOF);
	}

	/* Special boundary walls -- Right */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		x = DUNGEON_WID-1;

		cave_feat[y][x] = FEAT_PERM_SOLID;

		cave_info[y][x] |= (CAVE_XLOS);
		cave_info[y][x] |= (CAVE_XLOF);
	}
}


/*
 *  Place tunnels in the dungeon.
 */
static bool place_tunnels()
{
	int i, j, k, y, x, y1, x1;

	int counter = 0;
	int retries = 0;

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

	/* Partition rooms */
	for (i = 0; i < dun->cent_n; i++)
	{
		dun->part[i] = i;		
	}

	/*
	 * New tunnel generation routine.
	 *
	 * We partition the rooms into distinct partition numbers. We then find the room in
	 * each partition with the closest neighbour in an adjacent partition and attempt
	 * to connect the two rooms.
	 *
	 * When two rooms are connected by build_tunnel, the partitions are merged.
	 *
	 * We repeat, until there is only one partition.
	 */

	while (TRUE)
	{
		bool finished = TRUE;

		/* Abort */
		if (counter++ > DUN_ROOMS * DUN_ROOMS)
		{
			if (cheat_room) msg_format("Unable to connect rooms in %d attempted tunnels.", DUN_ROOMS * DUN_ROOMS);

			/* Megahack -- we have connectivity on surface anyway */
			if (level_flag & (LF1_SURFACE)) return (TRUE);

			return(FALSE);
		}
		
		retries++;

		/* Check each partition */
		for (i = 0; i < dun->cent_n; i++)
		{
			int dist = 30000;
			int c1 = -1;
			int c2 = -1;
			int r1 = -1;
			int r2 = -1;
			int n1 = 0;
			int n2 = 0;

			/* Pick a random member of this partition */
			for (j = 0; j < dun->cent_n; j++)
			{
				if (dun->part[j] != i) continue;

				/* Pick a random choice */
				if (!rand_int(++n1)) r1 = j;
			}

			/* Check members of this partition */
			for (j = 0; j < dun->cent_n; j++)
			{
				if (dun->part[j] != i) continue;

				/* Check all rooms */
				for (k = 0; k < dun->cent_n; k++)
				{
					int dist1;
					
					/* Skip itself */
					if (k == j) continue;

					/* Skip members of the same partition */
					if (dun->part[k] == i) continue;										

					/* Skip if state of flooding doesn't match */
					if ((retries > 2 * DUN_TRIES) &&
						((room_info[k].flags & (ROOM_FLOODED)) != (room_info[r1].flags & (ROOM_FLOODED))))
					{
						continue;
					}
					else if ((room_info[k].flags & (ROOM_FLOODED)) != (room_info[j].flags & (ROOM_FLOODED)))
					{
						continue;
					}

					/* Trying a random choice */
					if (retries > 2 * DUN_TRIES)
					{
						dist1 = distance(dun->cent[r1].y, dun->cent[r1].x, dun->cent[k].y, dun->cent[k].x);
					}
					else
					{
						dist1 = distance(dun->cent[j].y, dun->cent[j].x, dun->cent[k].y, dun->cent[k].x);
					}

					/* Pick this room sometimes */
					if (dist1 < dist)
					{
						dist = dist1;
						if (retries > 2 * DUN_TRIES) c1 = r1; else c1 = j;
						c2 = k;
					}

					/* Pick a random choice */
					if (!rand_int(++n2)) r2 = k;
				}
			}

			/* Use the choice */
			if ((c1 >= 0) && (c2 >= 0) && (c1 != c2) && (retries < 3 * DUN_TRIES) && (rand_int(4 * DUN_TRIES) > retries))
			{
				if (build_tunnel(dun->cent[c1].y, dun->cent[c1].x, dun->cent[c2].y, dun->cent[c2].x, retries > DUN_TRIES)) retries = 0;
			}
			/* Try a random choice */
			else if ((r1 >= 0) && (r2 >= 0) && (r1 != r2))
			{
				if (build_tunnel(dun->cent[r1].y, dun->cent[r1].x, dun->cent[r2].y, dun->cent[r2].x, TRUE)) retries = 0;
			}
			
			/* Try 'forcing' way into irregular rooms */
			if (((retries > 5 * DUN_TRIES) && (room_info[r1].type > ROOM_HUGE_CENTRE)) || (retries > 6 * DUN_TRIES))
			{
				/* Ignore outer walls */
				if ((room_info[r1].flags & (ROOM_EDGED)) == 0)
				{
					retries = 0;
					room_info[r1].flags |= (ROOM_EDGED);
				}
			}
		}

		/* Check if we have finished. All partition numbers will be the same. */
		for (i = 1; i < dun->cent_n; i++)
		{
			if (dun->part[i] != dun->part[i-1]) finished = FALSE;
		}

		/* Finished? */
		if (finished) break;
	}

	/* Place intersection doors */
	for (i = 0; i < dun->door_n; i++)
	{
		int dk = rand_int(100) < 50 ? 1 : -1;
		int count = 0;

		/* Extract junction location */
		y = dun->door[i].y;
		x = dun->door[i].x;

		/* Try a door in one direction */
		/* If the first created door is secret, stop */
		for (j = 0, k = rand_int(4); j < 4; j++)
		{
			if (try_door(y + ddy_ddd[k], x + ddx_ddd[k])) count++;

			if ((!count) && (f_info[cave_feat[y + ddy_ddd[k]][x + ddx_ddd[k]]].flags1 & (FF1_SECRET))) break;

			k = (4 + k + dk) % 4;
		}
	}

	/* Attempts successful */
	return (TRUE);
}


/*
 * Place decorations on walls (streamers and room entrance markings)
 */
static void place_decorations()
{
	int i, y, x;

	/* Place room decorations */
	for (i = 0; i < dun->next_n; i++)
	{
		/* Extract doorway location */
		y = dun->next[i].y;
		x = dun->next[i].x;

		/* Make sure we are replacing solid wall */
		if (!(f_info[cave_feat[y][x]].flags1 & (FF1_SOLID))) continue;

		/* Place feature if required */
		if (dun->next_feat[i]) cave_set_feat(y, x, dun->next_feat[i]);
	}

	/* Check rooms for streamer types */
	for (i = 0; i < dun->cent_n; i++)
	{
		int feat = room_info[i].theme[THEME_SOLID];
		int s = dun->stream_n;
		
		/* Record streamers for later */
		if ((feat) && (f_info[feat].flags1 & (FF1_STREAMER)) && (dun->stream_n < DUN_MAX_STREAMER))
		{
			int n;
				
			for (n = s; n < dun->stream_n; n++)
			{
				/* Already added */
				if (dun->streamer[n] == feat) break;
			}

			if (n == dun->stream_n)
			{
				/* Add dungeon streamer */
				dun->streamer[dun->stream_n++] = feat;
			}
		}
	}

	/* Allocate some streamers */
	for (i = 0; i < DUN_MAX_STREAMER; i++)
	{
		/* Pick a feature */
		int feat;

		/* Hack -- try to match dungeon themes */
		if (dun->stream_n)
		{
			feat = dun->streamer[i % dun->stream_n];
		}
		else
		{
			feat = pick_proper_feature(cave_feat_streamer);
		}

		/* Got a valid feature? */
		if (feat)
		{
			/* Generating */
			if (cheat_room) msg_format("Building %s streamer.", f_name + f_info[feat].name);

			/* Build one streamer. */
			build_streamer(feat);
		}
	}
}



/*
 * Returns a new spot for the player
 */
static bool new_player_spot(void)
{
	int i = 0;
	int y = 0;
	int x = 0;
	bool tunnel = FALSE;

	while (TRUE)
	{
		i++;

		/* Scan stored locations first. */
		if ((i < dun->stair_n) && (p_ptr->create_stair > 0))
		{
			/* Get location */
			y = dun->stair[i].y;
			x = dun->stair[i].x;

			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue;

			/* Success */
			break;
		}

		/* Then, search at random */
		else
		{
			/* Pick a random grid */
			y = rand_int(DUNGEON_HGT);
			x = rand_int(DUNGEON_WID);

			/* Mega hack -- try to place extra tunnel, except above surface in towers */
			if ((((level_flag & (LF1_TOWER)) == 0) || ((level_flag & (LF1_SURFACE)) != 0)) &&
				 (!(tunnel) && (i == 400)))
			{
				if (build_type0(0, 0))
				{
					place_tunnels();

					i = 0;

					tunnel = TRUE;
				}
			}

			/* We have failed */
			if (i > 1000) return (FALSE);

			/* Refuse to start on anti-teleport (vault) grids */
			if ((cave_info[y][x] & (CAVE_ROOM)) && (room_has_flag(y, x, ROOM_ICKY))) continue;

			/* Must be a safe start grid clear of monsters and objects  XXX */
			if ((i < 650) && (!cave_start_bold(y, x))) continue;

			/* Must be a floor grid clear of monsters and objects  XXX */
			if ((i < 700) && (!cave_naked_bold(y, x))) continue;
			
			/* Try nearly naked grids if desperate */
			if ((i >= 700) && (!cave_nearly_naked_bold(y, x))) continue;

			/* Try not to start in rooms */
			if ((i < 450) && (cave_info[y][x] & (CAVE_ROOM))) continue;

			/* Player prefers to be near walls. */
			if      ((i < 300) && (next_to_walls(y, x) < 2)) continue;
			else if ((i < 600) && (next_to_walls(y, x) < 1)) continue;

			/* Success */
			break;
		}
	}

	/* Place the stairs (if any) */
	/* Don't allow stairs if connected stair option is off */
	if ((p_ptr->create_stair) &&
		(((f_info[p_ptr->create_stair].flags1 & (FF1_STAIRS)) == 0) || (dungeon_stair)))
	{
		cave_set_feat(y, x, p_ptr->create_stair);
	}

	/* Place the player */
	player_place(y, x, TRUE);

	return (TRUE);
}



/*
 * Convert existing terrain type to rubble
 */
static void place_rubble(int y, int x)
{
	/* Put item under rubble */
	if (rand_int(100) < 5) cave_set_feat(y, x, FEAT_RUBBLE_H);

	/* Create rubble */
	else cave_set_feat(y, x, FEAT_RUBBLE);
}


/*
 * Places some staircases near walls
 */
static int alloc_stairs(int feat, int num, int walls)
{
	int y = 0, x = 0;
	
	int i = 0, j;

	/* Place "num" stairs */
	for (j = 0; j < num; j++)
	{
		while (num)
		{
			i++;

			/* Scan stored locations first. */
			if (i < dun->stair_n)
			{
				/* Get location */
				y = dun->stair[i].y;
				x = dun->stair[i].x;

				/* Require a "naked" floor grid */
				if (!cave_naked_bold(y, x)) continue;

				/* Hack -- only half the time */
				if (rand_int(100) < 50) continue;

				/* Success */
				break;
			}

			/* Then, search at random */
			else
			{
				/* Pick a random grid */
				y = rand_int(DUNGEON_HGT);
				x = rand_int(DUNGEON_WID);

				/* Reduce stairs if unable to place after a while */
				if ((i % 400) == 0) num--;

				/* Must be a safe start grid clear of monsters and objects  XXX */
				if ((i < 650) && (!cave_start_bold(y, x))) continue;

				/* Must be a floor grid clear of monsters and objects  XXX */
				if ((i < 700) && (!cave_naked_bold(y, x))) continue;

				/* Try nearly naked grids if desperate */
				if ((i >= 700) && (!cave_nearly_naked_bold(y, x))) continue;

				/* Player prefers to be near walls. */
				if      ((i % 300) == 0) walls--;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(y, x) < walls) continue;

				/* Success */
				break;
			}
		}

		/* Place fixed stairs */
		place_random_stairs(y, x, feat);
	}
	
	/* Number of stairs placed */
	return (num);
}



/*
 * Allocates some objects (using "place" and "type")
 */
static int alloc_object(int set, int typ, int num)
{
	int y = 0, x = 0;
	int k, i = 0;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		while (num)
		{
			bool room;

			bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

			/* Paranoia */
			i++;

			/* Scan stored quest locations first if allowed to place in rooms. */
			if ((i < dun->quest_n) && ((set == ALLOC_SET_ROOM) || (set == ALLOC_SET_BOTH)))
			{
				/* Get location */
				y = dun->quest[i].y;
				x = dun->quest[i].x;

				/* Require a "clean" floor grid */
				if (!cave_clean_bold(y, x)) continue;

				/* Require a "naked" floor grid */
				if (!cave_naked_bold(y, x)) continue;

				/* Success */
				break;
			}
			
			/* Scan stored stair locations first if allowed to place in corridors. */
			else if ((i < dun->stair_n) && ((set == ALLOC_SET_CORR) || (set == ALLOC_SET_BOTH)))
			{
				/* Get location */
				y = dun->stair[i].y;
				x = dun->stair[i].x;

				/* Require a "clean" floor grid */
				if (!cave_clean_bold(y, x)) continue;

				/* Require a "naked" floor grid */
				if (!cave_naked_bold(y, x)) continue;

				/* Success */
				break;
			}

			/* Then search at random */
			else
			{
				/* Location */
				y = rand_int(DUNGEON_HGT);
				x = rand_int(DUNGEON_WID);

				/* Reduce objects if unable to place after a while */
				if ((i % 400) == 0) num--;

				/* Require actual floor or ground grid */
				if (((f_info[cave_feat[y][x]].flags1 & (FF1_FLOOR)) == 0)
					&& ((f_info[cave_feat[y][x]].flags3 & (FF3_GROUND)) == 0)) continue;

				/* Check for "room" */
				room = (cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE;

				/* Require corridor? */
				if ((set == ALLOC_SET_CORR) && room && !surface) continue;

				/* Require room? */
				if ((set == ALLOC_SET_ROOM) && !room) continue;

				/* Success */
				break;
			}
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

			case ALLOC_TYP_FEATURE:
			{
				place_feature(y, x);
				break;
			}
			
			case ALLOC_TYP_BODY:
			{
				/* Hack -- pick body type */
				switch (rand_int(3))
				{
					case 0:
						tval_drop_idx = TV_BODY;
						break;
					case 1:
						tval_drop_idx = TV_SKIN;
						break;
					case 2:
						tval_drop_idx = TV_BONE;
						break;
				}
				
				place_object(y, x, FALSE, FALSE);
				
				tval_drop_idx = 0;
				break;
			}
		}
	}
	
	/* Number of objects placed */
	return (num);
}



/*
 * Place contents into dungeon.
 */
static bool place_contents()
{
	int i, k;

	/* Hack -- have less monsters during day light */
	if ((level_flag & (LF1_DAYLIGHT)) != 0) k = (p_ptr->depth / 6);
	else k = (p_ptr->depth / 3);

	if (k > 10) k = 10;
	if (k < 2) k = 2;

	/* Hack -- make sure we have rooms/corridors to place stuff */
	if ((level_flag & (LF1_ROOMS | LF1_TOWER)) != 0)
	{	
		/* Generating */
		if (cheat_room) msg_print("Placing stairs, rubble, traps.");

		/* Place 1 or 2 down stairs near some walls */
		if (!alloc_stairs(FEAT_MORE, rand_range(1, 2), 3)) return (FALSE);

		/* Place 1 or 2 up stairs near some walls */
		if (!alloc_stairs(FEAT_LESS, rand_range(1, 2), 3)) return (FALSE);

		/* Place 2 random stairs near some walls */
		alloc_stairs(0, 2, 3);

		/* Put some rubble in corridors -- we want to exclude towers unless other rooms on level */
		if ((level_flag & (LF1_ROOMS)) != 0) alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k * (((level_flag & (LF1_CRYPT | LF1_MINE | LF1_CAVE)) != 0) ? 3 : 1)));
	
		/* Place some traps in the dungeon */
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k * (((level_flag & (LF1_STRONGHOLD | LF1_CRYPT)) != 0) ? 2 : 1)));
	
		/* Place some features in rooms */
		alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_FEATURE, 1);
		
		/* If deepest monster is powerful, place lots of bodies around */
		if (r_info[cave_ecology.deepest_race[0]].level > p_ptr->depth * 5 / 4)
		{
			/* Place some bodies in the dungeon */
			alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_BODY, randint(k * (((level_flag & (LF1_STRONGHOLD | LF1_WILD)) != 0) ? 3 : 1)));
		}
	}
	/* FIXME - Check what assumptions we make here. */	
	else if (!dun->entrance)
	{
		/* Generating */
		if (cheat_room) msg_print("Placing dungeon entrance.");

		/* Place the dungeon entrance */
		if (!alloc_stairs(FEAT_ENTRANCE, 1, 0)) return (FALSE);
	}

	/* Determine the character location */
	if (((p_ptr->py == 0) || (p_ptr->px == 0)) && ! (new_player_spot())) return (FALSE);

	/* Pick a base number of monsters */
	/* Strongholds and sewers have more monsters */
	i = MIN_M_ALLOC_LEVEL + randint(8 * (((level_flag & (LF1_STRONGHOLD | LF1_SEWER | LF1_BATTLE)) != 0) ? 2 : 1));

	/* Generating */
	if (cheat_room) msg_print("Placing monsters.");

	/* If deepest monster is powerful, reduce total number of monsters */
	if (r_info[cave_ecology.deepest_race[0]].level > p_ptr->depth * 5 / 4) i = (i + 2) / 3;
	
	/* Put some monsters in the dungeon */
	for (i = i + k; i > 0; i--)
	{
		(void)alloc_monster(0, TRUE);
	}

	/* Hack -- make sure we have rooms to place stuff */
	if ((level_flag & (LF1_ROOMS | LF1_TOWER)) != 0)
	{
		/* Generating */
		if (cheat_room) msg_print("Placing objects, treasure.");

		/* Put some objects in rooms */
		alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ROOM * (((level_flag & (LF1_STRONGHOLD)) != 0) ? 2 : 1), 3));
	
		/* Put some objects/gold in the dungeon */
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ITEM * (((level_flag & (LF1_CRYPT)) != 0) ? 4 : 1), 3));
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal(DUN_AMT_GOLD* (((level_flag & (LF1_MINE)) != 0) ? 4 : 1), 3));
	}
	
	/* Successfully placed some stuff */
	return (TRUE);
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(void)
{
	int i, y, x;

	int by, bx;

	int base;

	dungeon_zone *zone=&t_info[0].zone[0];

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Create air */
	if (((level_flag & (LF1_TOWER)) != 0) && ((level_flag & (LF1_SURFACE)) == 0))
	{
		base = FEAT_CHASM;
	}
	/* Create ground */
	else if ((level_flag & (LF1_SURFACE)) != 0)
	{
		if (f_info[zone->fill].flags1 & (FF1_FLOOR)) base = zone->fill;
		else base = FEAT_GROUND;
	}
	/* Create granite wall */
	else
	{
		base = FEAT_WALL_EXTRA;
	}

	/* Hack -- Start with base */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			cave_set_feat(y,x,base);
		}
	}

	/* Hack -- Build terrain */
	/* XXX Get rid of this later */
	if ((zone->fill) && (base != FEAT_CHASM)) for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			build_terrain(y,x,zone->fill);
		}
	}

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = DUNGEON_HGT / BLOCK_HGT;
	dun->col_rooms = DUNGEON_WID / BLOCK_WID;

	/* Initialise 'zeroeth' room description */
	room_info[0].flags = 0;

	/* Initialize the room table */
	for (by = 0; by < dun->row_rooms; by++)
	{
		for (bx = 0; bx < dun->col_rooms; bx++)
		{
			dun->room_map[by][bx] = FALSE;
			dun_room[by][bx] = 0;
		}
	}

	/* No "entrance" yet */
	dun->entrance = FALSE;

	/* No rooms yet */
	dun->cent_n = 0;

	/* No quests yet */
	dun->quest_n = 0;

	/* No stairs yet */
	dun->stair_n = 0;

	/* Start with no tunnel doorways -- note needs to happen before placement of rooms now */
	dun->next_n = 0;

	/* Start with no themes */
	dun->theme_feat = 0;
	
	/* Start with no lakes */
	dun->lake_n = 0;
	
	/* Start with no streamers */
	dun->stream_n = 0;
	
	/* Start with no flooding */
	dun->flood_feat = 0;

	/* No features in a tower above the surface */
	if (((level_flag & (LF1_TOWER)) == 0) || ((level_flag & (LF1_SURFACE)) != 0))
	{
		/* Generating */
		if (cheat_room) msg_print("Building nature.");

		build_nature();
	}

	/* Set up the monster ecology before placing rooms */
	/* XXX Very early levels boring with ecologies enabled */
	if (p_ptr->depth > 3)
	{
		/* Generating */
		if (cheat_room) msg_print("Generating ecology.");

		/* Place guardian if permitted */
		if ((level_flag & (LF1_GUARDIAN)) != 0)
		{
			init_ecology(actual_guardian(zone->guard, p_ptr->dungeon));
		}
		/* Place any monster */
		else
		{
			init_ecology(0);
		}
	}

	/* Hack -- build a tower in the centre of the level */
	if ((zone->tower) && (p_ptr->depth >= min_depth(p_ptr->dungeon)))
	{
		/* Generating */
		if (cheat_room) msg_print("Building tower.");

		/* Place the tower */
		place_tower();
	}

	/* Build some rooms or tunnel endpoints */
	if ((level_flag & (LF1_ROOMS | LF1_TUNNELS)) != 0)
	{
		/* Generating */
		if (cheat_room) msg_print("Building rooms.");

		/* Place the rooms */
		if (!place_rooms()) return (FALSE);
	}

	/* Build boundary walls */
	place_boundary_walls();

	/* Build some tunnels */
	if ((level_flag & (LF1_TUNNELS)) != 0)
	{
		if (!place_tunnels()) return (FALSE);
	}

	/* Hack -- No destroyed "quest" levels */
	if (level_flag & (LF1_QUEST)) level_flag &= ~(LF1_DESTROYED);

	/* Hack -- No shallow destroyed levels */
	if (p_ptr->depth <= 20) level_flag &= ~(LF1_DESTROYED);

	/* Destroy the level if necessary */
	if ((level_flag & (LF1_DESTROYED)) != 0) destroy_level();

	/* Build streamers and entrance markings in caves */
	place_decorations();

	/* Build traps, treasure, rubble etc and place the player */
	if (!place_contents()) return (FALSE);

	/* Apply illumination */
	if ((level_flag & (LF1_SURFACE)) != 0) town_illuminate((level_flag & (LF1_DAYLIGHT)) != 0);

	/* Ensure quest monsters */
	if (is_quest(p_ptr->depth))
	{
		/* Ensure quest monsters */
		for (i = 1; i < z_info->r_max; i++)
		{
			monster_race *r_ptr = &r_info[i];

			/* Ensure quest monsters */
			if ((r_ptr->flags1 & (RF1_QUESTOR)) &&
			    (r_ptr->level == p_ptr->depth) &&
			    (r_ptr->cur_num <= 0))
			{
				int y, x;
				int count = 0;

				/* Pick a location */
				while (count++ < 1000)
				{
					y = rand_int(DUNGEON_HGT);
					x = rand_int(DUNGEON_WID);

					if (place_monster_here(y, x, i) > MM_FAIL) break;
				}

				if (count >= 1000)
				{
					if (cheat_room) msg_format("Could not place questor (%s).", r_name + r_info[i].name);
					
					return (FALSE);
				}

				/* Place the questor */
				place_monster_aux(y, x, i, FALSE, TRUE, 0L);
			}
		}
	}

	/* Ensure guardian monsters */
	if ((level_flag & (LF1_GUARDIAN)) != 0)
	{
		int y, x, guard;
		int count = 0;
		
		guard = actual_guardian(zone->guard, p_ptr->dungeon);

		/* Generating */
		if (cheat_room) msg_format("Placing guardian (%s).", r_name + r_info[guard].name);

		/* Pick a location */
		while (count++ < 1000)
		{
			y = rand_int(DUNGEON_HGT);
			x = rand_int(DUNGEON_WID);

			if (place_monster_here(y, x, guard) > MM_FAIL) break;
		}
		
		if (count >= 1000)
		{
			if (cheat_room) msg_format("Could not place guardian (%s).", r_name + r_info[guard].name);
			
			return (FALSE);
		}

		/* Place the questor */
		place_monster_aux(y, x, guard, FALSE, TRUE, 0L);
	}

	/* Generating */
	if (cheat_room) msg_print("Finished generating dungeon.");

	/* Hack -- restrict teleporation in towers */
	/* XXX Important that this occurs after placing the player */
	if ((level_flag & (LF1_TOWER)) != 0)
	{
		room_info[1].flags = (ROOM_ICKY);
	}

	/* Generation successful */
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
static void build_store(int feat, int yy, int xx)
{
	int y, x, y0, x0, y1, x1, y2, x2, tmp;

	/* Hack -- extract char value */
	byte d_char = f_info[feat].d_char;

	/* Hack -- don't build building for some 'special locations' */
	bool building = (((d_char > '0') && (d_char <= '8')) || (d_char == '+'));

	town_type *t_ptr = &t_info[p_ptr->dungeon];
	dungeon_zone *zone=&t_ptr->zone[0];;

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Find the "center" of the store */
	y0 = yy * 9 + 6;
	x0 = xx * 14 + 12;

	/* Determine the store boundaries */
	y1 = y0 - randint((yy == 0) ? 3 : 2);
	y2 = y0 + randint((yy == 1) ? 3 : 2);
	x1 = x0 - randint(5);
	x2 = x0 + randint(5);

	/* Hack -- decrease building size to create space for small terrain */
	if (zone->small)
	{
		if (x2 == 31) x2--;
		if (x1 == 35) x1++;
		if (x1 == 36) x1++;
		if (y2 == 9) y2--;
		if (y1 == 12) y1++;
	}

	/* Create a building? */
	if (building)
	{
		/* Build an invulnerable rectangular building */
		for (y = y1; y <= y2; y++)
		{
			for (x = x1; x <= x2; x++)
			{
				/* Create the building */
				cave_set_feat(y, x, FEAT_PERM_EXTRA);
			}
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

	int rooms[MAX_STORES];

	town_type *t_ptr = &t_info[p_ptr->dungeon];
	dungeon_zone *zone=&t_ptr->zone[0];;

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = seed_town;

	/* Then place some floors */
	for (y = 1; y < TOWN_HGT-1; y++)
	{
		for (x = 1; x < TOWN_WID-1; x++)
		{
			/* Create terrain on top */
			build_terrain(y, x, zone->big);
		}
	}

	/* MegaHack -- place small terrain north to south & bridge east to west */
	if (zone->small)
	{
		for (y = 1; y < TOWN_HGT-1; y++)
		{
			for (x = 32; x < 36; x++)
			{
				/* Create terrain on top */
				build_terrain(y, x, zone->small);
			}
		}

		for (y = 10; y < 12; y++)
		{
			for (x = 1; x < TOWN_WID-1; x++)
			{
				/* Create terrain on top */
				cave_alter_feat(y, x, FS_BRIDGE);
			}
		}

		/* Hack -- town square */
		if (feat_state(zone->big, FS_BRIDGE) == zone->small)
		{
			for (y = 2; y < 20; y++)
			{
				for (x = 6; x < 61; x++)
				{
					/* Exclude already built terrain */
					if ((y < 10) || (y >= 12) || (x< 32) || (x >= 36))
					{
						/* Create terrain on top */
						build_terrain(y, x, zone->small);
					}
				}
			}	
		}
	}

	/* Prepare an Array of "remaining stores", and count them */
	for (n = 0; n < MAX_STORES; n++) rooms[n] = n;

	/* Place two rows of stores */
	for (y = 0; y < 2; y++)
	{
		/* Place four stores per row */
		for (x = 0; x < 4; x++)
		{
			/* Pick a random unplaced store */
			k = ((n <= 1) ? 0 : rand_int(n));

			/* Build that store at the proper location */
			if (t_ptr->store[rooms[k]]) build_store(t_ptr->store[rooms[k]], y, x);
				
			/* Shift the stores down, remove one store */
			rooms[k] = rooms[--n];
		}
	}

	/* Place the dungeon entrance */
	while (TRUE)
	{
		/* Pick a location at least "three" from the outer walls */
		y = rand_range(3, TOWN_HGT - 4);
		x = rand_range(3, TOWN_WID - 4);

		/* Require a "starting" floor grid */
		if (cave_start_bold(y, x)) break;
	}

	/* Hack -- always have upstairs in surface of tower */
	if (((level_flag & (LF1_TOWER)) != 0) && (p_ptr->depth == min_depth(p_ptr->dungeon)))
	{
		cave_set_feat(y, x, FEAT_LESS);
	}
	else
	{
		/* Clear previous contents, add dungeon entrance */
		place_random_stairs(y, x, FEAT_ENTRANCE);
	}

	/* Place the player */
	player_place(y, x, TRUE);

	/* Sometimes we have to place upstairs as well */
	if (((t_info[p_ptr->dungeon].zone[0].tower) &&
		(p_ptr->depth < max_depth(p_ptr->dungeon)) && (p_ptr->depth > min_depth(p_ptr->dungeon)))
		|| (p_ptr->depth > min_depth(p_ptr->dungeon)))
	{
		/* Place the up stairs */
		while (TRUE)
		{
			/* Pick a location at least "three" from the outer walls */
			y = rand_range(3, TOWN_HGT - 4);
			x = rand_range(3, TOWN_WID - 4);

			/* Require a "naked" floor grid */
			if (cave_naked_bold(y, x)) break;
		}

		/* Clear previous contents, add up stairs */
		cave_set_feat(y, x, FEAT_LESS);
	}

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
static bool town_gen(void)
{
	int i, y, x;

	int residents;

	int by,bx;

	town_type *t_ptr = &t_info[p_ptr->dungeon];
	dungeon_zone *zone=&t_ptr->zone[0];;

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Initialize the room table */
	for (by = 0; by < MAX_ROOMS_ROW; by++)
	{
		for (bx = 0; bx < MAX_ROOMS_COL; bx++)
		{
			dun_room[by][bx] = 0;
		}
	}

	/* Initialise 'zeroeth' room description */
	room_info[0].flags = 0;

	/* Town does not have an ecology */
	cave_ecology.num_races = 0;
	cave_ecology.ready = FALSE;

	/* Day time */
	if ((level_flag & (LF1_DAYLIGHT)) != 0)
	{
		/* Number of residents */
		residents = MIN_M_ALLOC_TD;
	}

	/* Night time / underground */
	else
	{
		/* Number of residents */
		residents = MIN_M_ALLOC_TN;

	}

	/* Start with solid walls */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Create "solid" perma-wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);
		}
	}

	/* Then place some floors */
	for (y = 1; y < TOWN_HGT-1; y++)
	{
		for (x = 1; x < TOWN_WID-1; x++)
		{
			/* Create empty ground */
			cave_set_feat(y, x, f_info[zone->big].flags1 & (FF1_FLOOR) ? zone->big : FEAT_GROUND);
		}
	}

	/* Build stuff */
	town_gen_hack();

	/* Apply illumination */
	if ((level_flag & (LF1_SURFACE)) != 0) town_illuminate((level_flag & (LF1_DAYLIGHT)) != 0);

	/* Ensure guardian monsters */
	if (((level_flag & (LF1_GUARDIAN)) != 0) && ((level_flag & (LF1_DAYLIGHT)) == 0))
	{
		int y, x, guard;
		
		guard = actual_guardian(zone->guard, p_ptr->dungeon);

		/* Generating */
		if (cheat_room) msg_format("Placing guardian (%s).", r_name + r_info[guard].name);

		/* Pick a location */
		while (1)
		{
			y = rand_range(3, TOWN_HGT - 4);
			x = rand_range(3, TOWN_WID - 4);

			if (place_monster_here(y, x, guard) > MM_FAIL) break;
		}

		/* Place the questor */
		place_monster_aux(y, x, guard, FALSE, TRUE, 0L);
	}
	else
	{
		/* Ensure wandering monsters suit the dungeon level */
		get_mon_num_hook = dun_level_mon;

		/* Prepare allocation table */
		get_mon_num_prep();

		/* Make some residents */
		for (i = 0; i < residents; i++)
		{
			/* Make a resident */
			(void)alloc_monster(3, TRUE);
		}

		get_mon_num_hook = NULL;

		/* Prepare allocation table */
		get_mon_num_prep();
	}

	/* Always successful? */
	return(TRUE);
}


#if 0
/*
 * Ensure that the components required for any active quests are placed on the level.
 */
void ensure_quest()
{
	/* Hack -- ensure quest components */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		quest_type *q_ptr = &q_list[i];
		quest_event *qe_ptr = &(q_ptr->event[q_ptr->stage]);

		/* Hack -- player's actions don't change level */
		if (q_ptr->stage == QUEST_ACTION) qe_ptr = &(q_ptr->event[QUEST_ACTIVE]);

		/* Quest occurs on this level */
		if ((qe_ptr->dungeon == p_ptr->dungeon) && (qe_ptr->level == (p_ptr->depth - min_depth(p_ptr->dungeon))))
			{
			int n, j;

			n = 0;

			/* Hack -- quest partially completed */
			if (q_ptr->stage == QUEST_ACTION) n = q_ptr->event[QUEST_ACTION].number;

			/* Require features */
			if (qe_ptr->feat)
			{
				/* Check for feature type */
				while (n < qe_ptr->number)
				{
					/* Count quest features */
					for (y = 0; y < DUNGEON_HGT; y++)
					{
						for (x = 0; x < DUNGEON_WID; x++)
						{
							/* Check if feat okay */
							if (cave_feat[y][x] == qe_ptr->feat) n++;
						}
					}

					/* Try placing remaining features */
					for ( ; n < qe_ptr->number; n++)
					{
						/* Pick a "legal" spot */
						while (TRUE)
						{
							/* Location */
							y = rand_int(DUNGEON_HGT);
							x = rand_int(DUNGEON_WID);

							/* Require empty, clean, floor grid */
							if (!cave_naked_bold(y, x)) continue;

							/* Accept it */
							break;
						}

						/* Create the feature */
						cave_set_feat(y, x, qe_ptr->feat);

						/* Guard the feature */
						if (qe_ptr->race) race_near(qe_ptr->race, y, x);

						/* XXX Hide item in the feature */
					}
				}

				/* Amend quest numbers */
				if (n > qe_ptr->number) qe_ptr->number = n;
			}

			/* Require race */
			else if (qe_ptr->race)
			{
				n = 0;

				/* Check for monster race */
				while (n < qe_ptr->number)
				{
					/* Count quest races */
					for (j = 0; j < z_info->m_max; j++)
					{
						/* Check if monster okay */
						if (m_list[j].r_idx == qe_ptr->race) n++;
					}

					/* Try placing remaining monsters */
					for ( ; n < qe_ptr->number; n++)
					{
						/* Pick a "legal" spot */
						while (TRUE)
						{
							/* Location */
							y = rand_int(DUNGEON_HGT);
							x = rand_int(DUNGEON_WID);

							/* Require empty grid */
							if (!cave_empty_bold(y, x)) continue;

							/* Require monster can pass and survive on terrain */
							if (place_monster_here(y, x, qe_ptr->race) > MM_FAIL) continue;

							/* Accept it */
							break;
						}

						/* Create a new monster (awake, no groups) */
						(void)place_monster_aux(y, x, qe_ptr->race, FALSE, FALSE);

						/* XXX Monster should carry item */
						/* This is done as a part of death / fear etc. routine */
					}
				}

				/* Amend quest numbers */
				if (n > qe_ptr->number) qe_ptr->number = n;
			}

			/* Require object */
			else if ((qe_ptr->artifact) || (qe_ptr->ego_item_type) || (qe_ptr->kind))
			{
				n = 0;

				/* Check for object kind */
				while (n < qe_ptr->number)
				{
					/* Count quest objects */
					for (j = 0; j < z_info->m_max; j++)
					{
						/* Check if feat okay */
						if (o_list[j].k_idx)
						{
							if ((qe_ptr->artifact) && (o_list[j].name1 != qe_ptr->artifact)) continue;
							if ((qe_ptr->ego_item_type) && (o_list[j].name2 != qe_ptr->ego_item_type)) continue;
							if ((qe_ptr->kind) && (o_list[j].k_idx != qe_ptr->kind)) continue;

							n++;
						}
					}

					/* Try placing remaining objects */
					for ( ; n < qe_ptr->number; n++)
					{
						object_type object_type_body;
						object_type *o_ptr = &object_type_body;

						/* Pick a "legal" spot */
						while (TRUE)
						{
							/* Location */
							y = rand_int(DUNGEON_HGT);
							x = rand_int(DUNGEON_WID);

							/* Require empty grid */
							if (!cave_naked_bold(y, x)) continue;

							/* Prepare artifact */
							if (qe_ptr->artifact) qe_ptr->kind = lookup_kind(a_info[qe_ptr->artifact].tval, a_info[qe_ptr->artifact].sval);

							/* Prepare ego item */
							if ((qe_ptr->ego_item_type) && !(qe_ptr->kind)) qe_ptr->kind =
								lookup_kind(e_info[qe_ptr->ego_item_type].tval[0],
									e_info[qe_ptr->ego_item_type].min_sval[0]);

							/* Prepare object */
							object_prep(o_ptr, qe_ptr->kind);

							/* Prepare artifact */
							o_ptr->name1 = qe_ptr->artifact;

							/* Prepare ego item */
							o_ptr->name2 = qe_ptr->ego_item_type;

							/* Apply magic -- hack: use player level as reward level */
							apply_magic(o_ptr, p_ptr->max_lev * 2, FALSE, FALSE, FALSE);

							/* Several objects */
							if (o_ptr->number > 1) n += o_ptr->number -1;

							/* Accept it */
							break;
						}
					}
				}

				/* Amend quest numbers */
				if (n > qe_ptr->number) qe_ptr->number = n;
			}
		}
	}
}
#endif


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
	int i, j, y, x, num;

	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Important - prevent getting stuck in rock */
	p_ptr->word_return = 0;
	p_ptr->return_y = 0;
	p_ptr->return_x = 0;
	
	/* Generating */
	if (cheat_room) msg_format("Generating new level (level %d in %s)", p_ptr->depth, t_name + t_info[p_ptr->dungeon].name);

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;

		cptr why = NULL;

		/* Reset */
		o_max = 1;
		m_max = 1;
		
		/* There is no dynamic terrain */
		dyna_full = FALSE;
		dyna_n = 0;

		/* Start with a blank cave */
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{
				/* No flags */
				cave_info[y][x] = 0;

				/* No flags */
				play_info[y][x] = 0;

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
		
		/* Clear room info */
		for (i = 0; i < DUN_ROOMS; i++)
		{
			/* Initialise room */
			room_info[i].flags = 0;
			
			room_info[i].section[0] = -1;
			room_info[i].type = ROOM_NONE;
			room_info[i].vault = 0;
			room_info[i].deepest_race = 0;

			for (j = 0; j < MAX_THEMES; j++)
			{
				room_info[i].theme[j] = 0;
			}
		}

		/* Mega-Hack -- no player yet */
		p_ptr->px = p_ptr->py = 0;

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

		/* Initialise level flags */
		init_level_flags();

		/* Build the town */
		if (level_flag & (LF1_TOWN))
		{
			/* Make a town */
			if (!town_gen()) okay = FALSE;

			/* Report why */
			if (cheat_room) why = "defective town";
		}

		/* Build a real level */
		else
		{
			/* Make a dungeon */
			if (!cave_gen()) okay = FALSE;

			/* Report why */
			if (cheat_room) why = "defective dungeon";
		}

#if 0
		/* Ensure quest components */
		ensure_quest();
#endif
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

		/* It takes 1000 game turns for "feelings" to recharge */
		if ((old_turn) && ((turn - old_turn) < 1000)) feeling = 0;

		/* Hack -- no feeling in the town */
		if (level_flag & (LF1_TOWN)) feeling = 0;

		/* Prevent object over-flow */
		if (o_max >= z_info->o_max)
		{
			/* Message */
			why = "too many objects";

			/* Message */
			okay = FALSE;
		}

		/* Prevent monster over-flow */
		if (m_max >= z_info->m_max)
		{
			/* Message */
			why = "too many monsters";

			/* Message */
			okay = FALSE;

		}

		/* Accept */
		if (okay) break;

		/* Message */
		if (why) msg_format("Generation restarted (%s)", why);

		/* Interrupt? */
		if ((cheat_xtra) && (why) && (get_check("Interrupt? "))) break;

		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the monsters */
		wipe_m_list();
	}


	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Remember when this level was "created", except in town or surface locations */
	if (!(level_flag & (LF1_TOWN | LF1_SURFACE))) old_turn = turn;

	/* Hack -- always get a feeling leaving town or surface */
	else old_turn = 0;

	/* XXX Hurt light players sleep until it gets dark before entering surface levels */
	/* FIXME - Should really check that the player knows they are vulnerable to light */
	if (((p_ptr->cur_flags4 & (TR4_HURT_LITE)) != 0) && ((level_flag & (LF1_DAYLIGHT)) != 0))
	{
		/* Hack -- Set time to one turn before sun down */
		turn += ((10L * TOWN_DAWN) / 2) - (turn % (10L * TOWN_DAWN)) - 1;

		/* XXX Set food, etc */

		/* Inform the player */
		msg_print("You sleep during the day.");
	}

	/* Set dodging - 'just appeared' */
	p_ptr->dodging = 9;

	/* Redraw state */
	p_ptr->redraw |= (PR_STATE);

	/* Notify the player of changes to the map */
	if (!t_info[p_ptr->dungeon].visited)
	{
		/* This breaks for Angband */
		if (p_ptr->dungeon) for (i = 0; i < z_info->t_max; i++)
		{
			if (t_info[i].replace_ifvisited == p_ptr->dungeon)
			{
				msg_format("%^s has fallen.", t_name + t_info[i].name);
				msg_format("%^s now stands in its place.", t_name + t_info[t_info[i].replace_with].name);
			}

			if ((t_info[i].town_lockup_ifvisited == p_ptr->dungeon) && (r_info[t_info[i].town_lockup_monster].max_num > 0))
			{
				msg_format("%^s now terrorizes %s.", r_name + r_info[t_info[i].town_lockup_monster].name, t_name + t_info[i].name);
			}
			
			if ((t_info[i].guardian_ifvisited == p_ptr->dungeon) && (r_info[t_info[i].replace_guardian].max_num > 0))
			{
				msg_format("%^s now guards %s.", r_name + r_info[t_info[i].replace_guardian].name, t_name + t_info[i].name);
			}
		}
		
		/* Set this dungeon as visited */
		t_info[p_ptr->dungeon].visited = TRUE;

		/* Style tips */
		queue_tip(format("dungeon%d.txt", p_ptr->dungeon, i));
	}
	
	/* Set maximum depth for this dungeon */
	if (t_info[p_ptr->dungeon].max_depth < p_ptr->depth - min_depth(p_ptr->dungeon))
	{
		for (i = t_info[p_ptr->dungeon].max_depth; i < p_ptr->depth - min_depth(p_ptr->dungeon); i++)
		{
			/* Style tips */
			queue_tip(format("depth%d-%d.txt", p_ptr->dungeon, i));
		}

		/* Set new maximum depth */
		t_info[p_ptr->dungeon].max_depth = p_ptr->depth - min_depth(p_ptr->dungeon);
	}
	
	/* Hit by the plague */
	if (p_ptr->disease) suffer_disease();
}
