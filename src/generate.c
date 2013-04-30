/* File: generate.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001 Andrew Doull. Modifications to the Angband 2.9.1
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
 * The 64 new "dungeon features" will also be used for "visual display"
 * but we must be careful not to allow, for example, the user to display
 * hidden traps in a different way from floors, or secret doors in a way
 * different from granite walls, or even permanent granite in a different
 * way from granite.  XXX XXX XXX
 */


/*
 * Dungeon generation values
 */
/* DUN_ROOMS now defined in defines.h */
#define DUN_UNUSUAL     200     /* Level/chance of unusual room */
#define DUN_DEST	30      /* 1/chance of having a destroyed level */

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND     10      /* Chance of random direction */
#define DUN_TUN_CHG     30      /* Chance of changing direction */
#define DUN_TUN_CON     15      /* Chance of extra tunneling */
#define DUN_TUN_PEN     25      /* Chance of doors at room entrances */
#define DUN_TUN_JCT     90      /* Chance of doors at tunnel junctions */

/*
 * Dungeon streamer generation values
 */
#define DUN_STR_DEN     5       /* Density of streamers */
#define DUN_STR_RNG     2       /* Width of streamers */
#define DUN_STR_MAG     3       /* Number of magma streamers */
#define DUN_STR_MC      90      /* 1/chance of treasure per magma */
#define DUN_STR_QUA     2       /* Number of quartz streamers */
#define DUN_STR_QC      40      /* 1/chance of treasure per quartz */
#define DUN_STR_SAN     2       /* Number of sandstone streamers */
#define DUN_STR_SLV     40      /* Deepest level sandstone occurs instead of magma */
#define DUN_STR_GOL     20      /* 1/chance of rich mineral vein */
#define DUN_STR_GC      2       /* 1/chance of treasure per rich mineral vein */
#define DUN_STR_CRA     8       /* 1/chance of cracks through dungeon */
#define DUN_STR_CC      0       /* 1/chance of treasure per crack */

/*
 * Dungeon feature values
 */
#define DUN_FEAT_OILC   10      /* 1/chance of oil/coal feature level */
#define DUN_FEAT	40      /* Chance in 100 of having features */
#define DUN_MAX_LAKES   3       /* Maximum number of lakes/rivers */
#define DUN_FEAT_RNG    2       /* Width of lake */

/*
 * Dungeon treasure allocation values
 */
#define DUN_AMT_ROOM    9       /* Amount of objects for rooms */
#define DUN_AMT_ITEM    3       /* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD    3       /* Amount of treasure for rooms/corridors */

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
#define ALLOC_TYP_TRAP	  3       /* Trap */
#define ALLOC_TYP_GOLD	  4       /* Gold */
#define ALLOC_TYP_OBJECT	5       /* Object */
#define ALLOC_TYP_FEATURE	6	/* Feature eg fountain */

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
#define ROOM_MAX	9



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

	/* Hack -- number of entrances to dungeon */
	bool entrance;
};


/*
 * Dungeon generation data -- see "cave_gen()"
 */
static dun_data *dun;


/*
 * Array of room types (assumes 11x11 blocks)
 */
static room_data room[ROOM_MAX] =
{
	{ 0, 0, 0, 0, 0 },	      /* 0 = Nothing */
	{ 0, 0, -1, 1, 1 },	     /* 1 = Simple (33x11) */
	{ 0, 0, -1, 1, 1 },	     /* 2 = Overlapping (33x11) */
	{ 0, 0, -1, 1, 3 },	     /* 3 = Crossed (33x11) */
	{ 0, 0, -1, 1, 3 },	     /* 4 = Large (33x11) */
	{ 0, 0, -1, 1, 5 },	     /* 5 = Monster nest (33x11) */
	{ 0, 0, -1, 1, 5 },	     /* 6 = Monster pit (33x11) */
	{ 0, 1, -1, 1, 5 },	     /* 7 = Lesser vault (33x22) */
	{ -1, 2, -2, 3, 10 }    /* 8 = Greater vault (66x44) */
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
 */
static void new_player_spot(void)
{
	int y, x;

	int count=0;

	/* Place the player */
	while (1)
	{

		/* Pick a legal spot */
		y = rand_range(1, DUNGEON_HGT - 2);
		x = rand_range(1, DUNGEON_WID - 2);

		if (cave_naked_bold(y,x) && (count > 2000)) break;

		/* Must be a "start" floor grid */
		if (!cave_start_bold(y, x)) continue;

		/* Refuse to start in anti-teleport rooms */
		if (cave_info[y][x] & (CAVE_ROOM))
		{
			/* Don't allow teleport here */
			int by = y/BLOCK_HGT;
			int bx = x/BLOCK_HGT;
			int room = dun_room[by][bx];

			if (room_info[room].flags & (ROOM_ICKY)) continue;
		}

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

	if (f_info[cave_feat[y+1][x]].flags1 & (FF1_WALL)) k++;
	if (f_info[cave_feat[y+1][x]].flags1 & (FF1_WALL)) k++;
	if (f_info[cave_feat[y+1][x]].flags1 & (FF1_WALL)) k++;
	if (f_info[cave_feat[y+1][x]].flags1 & (FF1_WALL)) k++;

	return (k);
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
 * Convert existing terrain type to "up stairs"
 */
static void place_up_stairs(int y, int x)
{
	/* Create up stairs */
	cave_set_feat(y, x, FEAT_LESS);
}


/*
 * Convert existing terrain type to "down stairs"
 */
static void place_down_stairs(int y, int x)
{
	/* Create down stairs */
	cave_set_feat(y, x, FEAT_MORE);
}





/*
 * Place an up/down staircase at given location
 */
static void place_random_stairs(int y, int x)
{
	/* Paranoia */
	if (!cave_clean_bold(y, x)) return;

	/* Choose a staircase */
	if (p_ptr->depth == min_depth(p_ptr->dungeon))
	{
		place_down_stairs(y, x);
	}
	else if (is_quest(p_ptr->depth) || (p_ptr->depth >= max_depth(p_ptr->dungeon)))
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
 * Places some staircases near walls
 */
static void alloc_stairs(int feat, int num, int walls)
{
	int y, x, i, j, flag;


	/* Place "num" stairs */
	for (i = 0; i < num; i++)
	{
		/* Place some stairs */
		for (flag = FALSE; !flag; )
		{
			/* Try several times, then decrease "walls" */
			for (j = 0; !flag && j <= 3000; j++)
			{
				/* Pick a random grid */
				y = rand_int(DUNGEON_HGT);
				x = rand_int(DUNGEON_WID);

				/* Require actual floor grid */
				if (!(f_info[cave_feat[y][x]].flags1 & (FF1_FLOOR))) continue;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(y, x) < walls) continue;

				/* No dungeon, no stairs */
				if (min_depth(p_ptr->dungeon) == max_depth(p_ptr->dungeon))
				{
					/* Nothing */
				}
				/* Town -- must go down */
				else if (p_ptr->depth == min_depth(p_ptr->dungeon))
				{
					/* Clear previous contents, add down stairs */
					cave_set_feat(y, x, FEAT_MORE);
				}

				/* Top of tower -- must go down */
				else if ((t_info[p_ptr->dungeon].zone[0].tower) && (p_ptr->depth >= max_depth(p_ptr->dungeon)))
				{

					/* Clear previous contents, add down stairs */
					cave_set_feat(y, x, FEAT_MORE);

				}

				/* Bottom of tower dungeon -- must go up */
				else if ((t_info[p_ptr->dungeon].zone[0].tower) && (p_ptr->depth == 1))
				{

					/* Clear previous contents, add down stairs */
					cave_set_feat(y, x, FEAT_LESS);

				}
				/* Quest -- must go up */
				else if (is_quest(p_ptr->depth) || (p_ptr->depth >= max_depth(p_ptr->dungeon)))
				{
					/* Clear previous contents, add up stairs */
					cave_set_feat(y, x, FEAT_LESS);
				}

				/* Requested type */
				else
				{
					/* Clear previous contents, add stairs */
					cave_set_feat(y, x, feat);
				}

				/* All done */
				flag = TRUE;
			}

			/* Require fewer walls */
			if (walls) walls--;
		}
	}
}



/*
 * Allocates some objects (using "place" and "type")
 */
static void alloc_object(int set, int typ, int num)
{
	int y, x, k, c;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		c = 0;

		/* Pick a "legal" spot */
		while (TRUE)
		{
			bool room;

			bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

			/* Paranoia */
			if (c++ > 2000) return;

			/* Location */
			y = rand_int(DUNGEON_HGT);
			x = rand_int(DUNGEON_WID);

			/* Require actual floor grid */
			if (!(f_info[cave_feat[y][x]].flags1 & (FF1_FLOOR))) continue;

			/* Check for "room" */
			room = (cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room && !surface) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

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
		}
	}
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
	int k;

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


	/* Hack -- no change */
	if (newfeat == oldfeat) return;

	k = randint(100);

	if (f_info[newfeat].flags3 & (FF3_TREE))
	{
		if (k<=85) newfeat = oldfeat;
		if ((k > 85) && (k <= 92)) newfeat = FEAT_TREE;
		if ((k > 91) && (k <= 92)) newfeat = FEAT_TREE_BROKEN;
		if ((k > 92) && (k <= 93)) newfeat = FEAT_TREE_HIDE;
	}

	switch (newfeat) {


		case FEAT_BUSH:
		case FEAT_BUSH_HURT:
		case FEAT_BUSH_FOOD:
		case FEAT_BUSH_HURT_P:
		if (k<=30) newfeat = oldfeat;
		if (k<=90) newfeat = FEAT_GRASS;
		break;

		case FEAT_RUBBLE:
		if (k<90) newfeat = oldfeat;
		break;

		case FEAT_LIMESTONE:
		if (k<40) newfeat = FEAT_FLOOR;
		if ((k > 40) && (k <= 60)) newfeat = FEAT_WATER;
		break;

		case FEAT_ICE:
		if (k <= 10) newfeat = FEAT_ICE_C;
		break;

		case FEAT_ICE_GEOTH:
		if (k <= 10) newfeat = FEAT_ICE_GEOTH_HC;
		break;

		case FEAT_ICE_WATER_K:
		if (k<= 15) newfeat = FEAT_WATER_K;
		break;

		case FEAT_ICE_CHASM:
		if (k <= 80) newfeat = FEAT_FLOOR_ICE;
		if ((k > 80) && (k<90)) newfeat = FEAT_CHASM_E;
		break;

		case FEAT_ICE_FALLS:
		if (k <= 40) newfeat = FEAT_FLOOR_ICE;
		if (k <= 60) newfeat = FEAT_ICE_CHASM;
		if ((k > 60) && (k<80)) newfeat = FEAT_ICE_FALL;
		break;

		case FEAT_WATER_FALLS:
		if (k <= 60) newfeat = FEAT_WATER_H;
		if ((k > 60) && (k<80)) newfeat = FEAT_WATER;
		break;

		case FEAT_ACID_FALLS:
		if (k <= 60) newfeat = FEAT_ACID_H;
		if ((k > 60) && (k<80)) newfeat = FEAT_ACID;
		break;

		case FEAT_MUD:
		if (k <= 10) newfeat = FEAT_FLOOR_EARTH;
		if ((k> 10) && (k <= 13)) newfeat = FEAT_WATER;
		break;

		case FEAT_MUD_H:
		if (k <= 10) newfeat = FEAT_FLOOR_EARTH;
		if ((k> 10) && (k <= 23)) newfeat = FEAT_WATER_H;
		break;

		case FEAT_MUD_K:
		if (k <= 5) newfeat = FEAT_WATER_K;
		break;

		case FEAT_QSAND_H:
		if (k <= 25) newfeat = FEAT_SAND_H;
		if ((k> 25) && (k <= 28)) newfeat = FEAT_WATER;
		break;

		case FEAT_BWATER_FALLS:
		if (k <= 60) newfeat = FEAT_BWATER;
		if ((k > 60) && (k<80)) newfeat = FEAT_FLOOR_RUBBLE;
		break;

		case FEAT_BMUD:
		if (k <= 10) newfeat = FEAT_BWATER;
		if ((k> 10) && (k <= 13)) newfeat = FEAT_VENT_BWATER;
		break;

		case FEAT_GEOTH:
		if (k <= 5) newfeat = FEAT_VENT_STEAM;
		if ((k> 5) && (k <= 10)) newfeat = FEAT_VENT_GAS;
		break;

		case FEAT_GEOTH_LAVA:
		if (k <= 5) newfeat = FEAT_LAVA_H;
		if ((k> 5) && (k <= 10)) newfeat = FEAT_LAVA;
		if ((k> 10) && (k <= 13)) newfeat = FEAT_VENT_LAVA;
		break;

		case FEAT_LAVA_FALLS:
		if (k <= 60) newfeat = FEAT_LAVA_H;
		if ((k > 60) && (k<80)) newfeat = FEAT_FLOOR_RUBBLE;
		break;

	}

	/* Set the feature if we have a change */
	if (newfeat != oldfeat) cave_set_feat(y,x,newfeat);

}

/*
 * Places "lakes" of a feature through dungeon
 *
 */
static void build_feature(int y, int x, int feat, bool do_big_lake)
{
	int i, dir;
	int ty, tx, yi,xi;
	int by,bx;
	int feat1 = feat;
	int feat2 = f_info[feat].edge;

	int lake_width,lake_length;

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

	/* Hack -- increase the 'big'ness */
	if ((f_info[feat1].flags1 & (FF1_WALL)) && (variant_big_feats))
	{
		/* Make small go big */
		do_big_lake = TRUE;
	}

	/* Hack -- minimise holes in terrain */
	if (surface && !feat2) feat2 = feat1;

	/* Hack -- Save the room location */
	if (!(f_info[feat1].flags2 & (FF2_RIVER))
	       && (dun->cent_n < CENT_MAX))
	{
		dun->cent[dun->cent_n].y = y;
		dun->cent[dun->cent_n].x = x;
		dun->cent_n++;
	}

	if ((f_info[feat1].flags2 & (FF2_LAKE)) || !(f_info[feat1].flags2 & (FF2_RIVER)))
	{
		lake_width = DUN_FEAT_RNG;
		lake_length = 15+randint(p_ptr->depth/2);

		if (do_big_lake)
		{
			lake_width = DUN_FEAT_RNG+1;
			lake_length = 200;
		}

		/* Place lake into dungeon */
		for (i = 0; i<lake_length; i++)
		{
			/* Paranoia */
			if (!in_bounds_fully(y,x)) break;

			/* Pick a nearby grids */
			ty = y + rand_int(2* lake_width+1)- lake_width;
			tx = x + rand_int(2* lake_width+1)- lake_width;
			if (!in_bounds_fully(ty, tx)) continue;
			if (f_info[cave_feat[ty][tx]].flags1 & (FF1_PERMANENT)) continue;
		 
			y = ty;
			x = tx;

			/* Don't want to write over own feat */
			if (cave_feat[y][x] == feat1)
			{
				/* Choose a random compass direction */
				dir = ddd[rand_int(4)];
			
				/* Walk to edge of feature */
				while (cave_feat[y][x] == feat1)
				{
					y = y+ddy[dir];
					x = x+ddx[dir];

					/* Stop at dungeon edge */
					if (!in_bounds_fully(y, x)) break;
					if (f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)) break;
				}

			}

			if (!in_bounds_fully(y, x)) break;
			if (f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)) break;

			if ((!do_big_lake) && (!surface))
			{
				/* Don't allow rooms here */
				by = y/BLOCK_HGT;
				bx = x/BLOCK_HGT;
	
				dun->room_map[by][bx] = TRUE;
			}

			for (yi=y-lake_width;yi<=y+lake_width;yi++)
			{
				for (xi=x-lake_width;xi<=x+lake_width;xi++)
				{
	
					if ((yi==y-lake_width)||
					    (yi==y+lake_width)||
					    (xi==x-lake_width)||
					    (xi==x+lake_width))
					{
						if ((in_bounds_fully(yi,xi)) && (feat2)) build_terrain(yi,xi,feat2);
	
					}
					else
					{
						if ((in_bounds_fully(yi,xi))
							&& (randint(100)<(do_big_lake? 40:20)))
						{
							if (feat2) build_terrain(yi,xi,feat2);
						}
						else if (in_bounds_fully(yi,xi))
						{
							build_terrain(yi,xi,feat1);
						}
					}
				}
			}
		}

	}

	if (f_info[feat1].flags2 & (FF2_RIVER))
	{

		/* Choose a random compass direction */
		dir = ddd[rand_int(4)];
	
		/* Place river into dungeon */
		while (TRUE)
		{
			/* Stop at dungeon edge */
			if (!in_bounds_fully(y, x)) break;
			if (f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)) break;
	
			if (!(f_info[feat1].flags1 & (FF1_WALL)))
			{
				/* Don't allow rooms here */
				by = y/BLOCK_HGT;
				bx = x/BLOCK_HGT;
	
				dun->room_map[by][bx] = TRUE;
			}
	
			/*Add terrain*/
			build_terrain(y,x,feat1);
	
			if (feat2)
			{
				for (i=0;i<8;i++)
				{
					int di = ddd[i];
					int yi = y+ddy[di];
					int xi = x+ddx[di];
	
					if (!in_bounds_fully(yi, xi)) continue;
					if (f_info[cave_feat[yi][xi]].flags1 & (FF1_PERMANENT)) continue;
	
					build_terrain(yi,xi,feat2);
				}
			}
	
			/*Stagger the river*/
			if (rand_int(100)<50)
			{
				int dir2 = ddd[rand_int(4)];
	
				y += ddy[dir2];
				x += ddx[dir2];
			}
			/* Advance the streamer */
			else
			{
				y += ddy[dir];
				x += ddx[dir];
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
				if (!in_bounds_fully(ty, tx)) continue;
				break;
			}

			/* Only convert "granite" walls */
			if (cave_feat[ty][tx] < FEAT_WALL_EXTRA) continue;
			if (cave_feat[ty][tx] > FEAT_WALL_SOLID) continue;

			/* Clear previous contents, add proper vein type */
			cave_set_feat(ty, tx, feat);

			/* Hack -- Add some (known) treasure */
			if ((chance) &&(rand_int(chance) == 0)) cave_feat[ty][tx] += 0x04;
		}

		/* Advance the streamer */
		y += ddy[dir];
		x += ddx[dir];

		/* Stop at dungeon edge */
		if (!in_bounds_fully(y, x)) break;
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
 * Create up to "num" gold near the given coordinates
 * Only really called by the room_info routines
 */
static void vault_treasure(int y, int x, int num)
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

			/* Place gold */
			place_gold(j, k);

			/* Placement accomplished */
			break;
		}
	}
}


/*
 * Create up to "num" objects excluding gold near the given coordinates
 * Only really called by the room_info routines
 */
static void vault_items(int y, int x, int num)
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

			/* Place gold */
			place_object(j, k, FALSE,FALSE);

			/* Placement accomplished */
			break;
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
				place_object(j, k, FALSE, FALSE);
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
 * Hack -- Place some sleeping monsters near the given location
 */
static void vault_monsters(int y1, int x1, int num)
{
	int k, i, y, x;

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
			monster_level = p_ptr->depth + 2;
			(void)place_monster(y, x, TRUE, TRUE);
			monster_level = p_ptr->depth;

			/* Check we have number */
			if (++k>=num) break;
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
 * Hack -- flags type for "vault_aux_room_info()"
 */
static int room_info_mon_flag;


/*
 * Hack -- char type for "vault_aux_room_info()"
 */
static byte room_info_mon_char;


/*
 * Helper function for room info
 */
static bool room_info_mon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Hack -- Require matching graphic if set  */
	if ((room_info_mon_char) &&
		(room_info_mon_char != r_ptr->d_char)) return (FALSE);

	/* Require matching flag if set */
	if (room_info_mon_flag < 0) return (TRUE);

	if ((room_info_mon_flag < 32) && 
		!(r_ptr->flags1 & (1L << room_info_mon_flag))) return (FALSE);

	if ((room_info_mon_flag >= 32) && 
		(room_info_mon_flag < 64) && 
		!(r_ptr->flags2 & (1L << (room_info_mon_flag -32)))) return (FALSE);

	if ((room_info_mon_flag >= 64) && 
		(room_info_mon_flag < 96) && 
		!(r_ptr->flags3 & (1L << (room_info_mon_flag -64)))) return (FALSE);

	if ((room_info_mon_flag >= 96) && 
		(room_info_mon_flag < 128) && 
		!(r_ptr->flags4 & (1L << (room_info_mon_flag -96)))) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Hack -- flags type for "vault_aux_room_info()"
 */
static int room_info_kind_tval;

/*
 *
 */
static bool room_info_kind(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	if (k_ptr->tval == room_info_kind_tval) return (TRUE);

	return(FALSE);

}

/*
 * Get the room description, and place stuff accordingly.
 */
static void get_room_info(int y, int x)
{
	int i, j, chart, roll;

	int room = dun->cent_n+1;

	/* Initialise chart */
	chart = 1;
	j = 0;

	/* Room flags */
	room_info[room].flags =0;

	/* Process the description */
	while (chart && (j < ROOM_DESC_SECTIONS - 1))
	{
		/* Start over */
		i = 0;

		/* Roll for line */
		roll = randint(100);

		/* Get the proper entry in the table */
		while ((chart != d_info[i].chart) || (roll > d_info[i].roll)) i++;

		/* If not allowed on this level, drop to maximum result */
		if ((p_ptr->depth < d_info[i].level) || ((d_info[i].l_flag) && !(level_flag & d_info[i].l_flag)))
		{
			while ((chart != d_info[i].chart) || (100 > d_info[i].roll)) i++;
		}

		/* Save index */
		room_info[room].section[j++] = i;

		/* Enter the next chart */
		chart = d_info[i].next;

		/* Place flags except SEEN */
		room_info[room].flags |= (d_info[i].flags & ~(ROOM_SEEN));

		/* Option */
		if (!(variant_room_info)) continue;
		
		/* Place monster if needed */
		if ((d_info[i].r_flag) || (d_info[i].r_char))
		{
		  room_info_mon_flag = d_info[i].r_flag -1;
			room_info_mon_char = d_info[i].r_char;

			get_mon_num_hook = room_info_mon;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Place several monsters */
			vault_monsters(y,x,randint(2)+2);
		}

		/* Place objects if needed */
		if (d_info[i].tval)
		{
			room_info_kind_tval = d_info[i].tval;

			get_obj_num_hook = room_info_kind;

			/* Prepare allocation table */
			get_obj_num_prep();

			/* Place the items */
			if (d_info[i].tval == TV_GOLD) vault_treasure(y,x,randint(4)+2);
			else vault_items(y, x, randint(4)+2);

			get_obj_num_hook = NULL;

			/* Prepare allocation table */
			get_obj_num_prep();

		}
			
		/* Place features if needed */
		if (d_info[i].feat)
		{
			int ii, y1, x1;
			int num = 7 + rand_int(6);
			int k = 0;

			if (f_info[d_info[i].feat].flags3 & (FF3_ALLOC)) num /=2;

			for (ii = 0; ii < 15; ii++)
			{
				int d = 3;

				/* Pick a nearby location */
				scatter(&y1, &x1, y, x, d, 0);

				/* Require "empty" grid */
				if (!cave_start_bold(y1, x1)) continue;

				cave_set_feat(y1,x1,d_info[i].feat);

				if (k++ >= num) break;
			}
		}

	}

	/* Type */
	room_info[room].type = ROOM_NORMAL;

	/* Terminate index list */
	room_info[room].section[j] = -1;

	/* Hack -- only clear room info hook here */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();


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


	/* Hack -- mark light rooms */
	if (light) level_flag &= ~(LF1_DEST);
	else level_flag |= (LF1_DEST) ;

	/* Pretty description and maybe more monsters/objects/traps*/
	get_room_info(y0,x0);


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


	/* Hack -- mark light rooms */
	if (light) level_flag &= ~(LF1_DEST);
	else level_flag |= (LF1_DEST);

	/* Pretty description and maybe more monsters/objects/traps*/
	get_room_info(y0,x0);

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
			place_object(y0, x0, FALSE, FALSE);

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


	/* Hack -- mark light rooms */
	if (light) level_flag &= ~(LF1_DEST);
	else level_flag |= (LF1_DEST);


	/* Pretty description and maybe more monsters/objects/traps*/
	get_room_info(y0,x0);
}


/*
 * Type 4 -- Large room with an inner room
 *
 * Possible sub-types:
 *      1 - An inner room
 *      2 - An inner room with a small inner room
 *      3 - An inner room with a pillar or pillars
 *      4 - An inner room with a checkerboard
 *      5 - An inner room with four compartments
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
				place_object(y0, x0, FALSE, FALSE);
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
				if (rand_int(3) == 0) place_object(y0, x0 - 2, FALSE, FALSE);
				if (rand_int(3) == 0) place_object(y0, x0 + 2, FALSE, FALSE);
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

	/* Initialise room description */
	room_info[dun->cent_n+1].type = ROOM_LARGE;
	room_info[dun->cent_n+1].flags = 0;
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
 * Helper function for "monster nest (theme)"
 */
static bool vault_aux_theme_nest(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Accept monsters with flag */
	if (t_info[p_ptr->dungeon].r_flag)
	{
		int mon_flag = t_info[p_ptr->dungeon].r_flag-1;

		if ((mon_flag < 32) && 
			(r_ptr->flags1 & (1L << mon_flag))) return (TRUE);

		if ((mon_flag >= 32) && 
			(mon_flag < 64) && 
			(r_ptr->flags2 & (1L << (mon_flag -32)))) return (TRUE);

		if ((mon_flag >= 64) && 
			(mon_flag < 96) && 
			(r_ptr->flags3 & (1L << (mon_flag -64)))) return (TRUE);

		if ((mon_flag >= 96) && 
			(mon_flag < 128) && 
			(r_ptr->flags4 & (1L << (mon_flag -96)))) return (TRUE);

		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (theme)"
 */
static bool vault_aux_theme_pit(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Accept monsters with graphic */
	if (t_info[p_ptr->dungeon].r_char)
	{
		if (r_ptr->d_char == t_info[p_ptr->dungeon].r_char) return (TRUE);

		return (FALSE);
	}

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

	/* Hack -- Require orcs */
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

	/* Hack -- Require trolls monster */
	if (!strchr("T", r_ptr->d_char)) return (FALSE);

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

	/* Hack -- Require giants */
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

	/* Hack -- Require dragons (note that this includes "A" now)  */
	if (!strchr("ADd", r_ptr->d_char)) return (FALSE);

	/* Hack -- Require correct "breath attack" */
	if (r_ptr->flags4 != vault_aux_dragon_mask4) return (FALSE);

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
static void build_type5(int y0, int x0)
{
	int y, x, y1, x1, y2, x2;

	int tmp, i;

	s16b what[64];

	cptr name;

	bool empty = FALSE;

	int light = FALSE;

	int rating_bonus;

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

	/* Open the inner room with a secret door */
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);


	/* Hack -- Choose a nest type */
	tmp = randint(p_ptr->depth*2);

	/* Dungeon pit */
	if ((adult_campaign) && (tmp < 25))
	{
		/* Describe */
		name = "themed";

		get_mon_num_hook = vault_aux_theme_nest;

		/* Appropriate rating bonus */
		rating_bonus = 10;

		/* Initialise room description */
		room_info[dun->cent_n+1].type = ROOM_NEST_THEME;

	}
	/* Monster nest (jelly) */
	else if (tmp < 30)
	{
		/* Describe */
		name = "jelly";

		/* Restrict to jelly */
		get_mon_num_hook = vault_aux_jelly;

		/* Appropriate rating bonus */
		rating_bonus = 25 - p_ptr->depth;

		/* Initialise room description */
		room_info[dun->cent_n+1].type = ROOM_NEST_JELLY;
	}

	/* Monster nest (animal) */
	else if (tmp < 50)
	{
		/* Describe */
		name = "animal";

		/* Restrict to animal */
		get_mon_num_hook = vault_aux_animal;

		/* Appropriate rating bonus */
		rating_bonus = 45 - p_ptr->depth;


		/* Initialise room description */
		room_info[dun->cent_n+1].type = ROOM_NEST_ANIMAL;
	}

	/* Monster nest (undead) */
	else
	{
		/* Describe */
		name = "undead";

		/* Restrict to undead */
		get_mon_num_hook = vault_aux_undead;

		/* Appropriate rating bonus */
		rating_bonus = 85 - p_ptr->depth;

		/* Initialise room description */
		room_info[dun->cent_n+1].type = ROOM_NEST_UNDEAD;
	}

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(p_ptr->depth + 10);

		/* Notice failure */
		if (!what[i]) empty = TRUE;
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

	/* Initialize room description */
	room_info[dun->cent_n+1].flags = 0;

	/* Increase the level rating */
	if (rating_bonus > 0) rating += rating_bonus;

	/* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
	if ((p_ptr->depth <= 40) &&
	    (randint(p_ptr->depth * p_ptr->depth + 1) < 300))
	{
		good_item_flag = TRUE;
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
 *   orc pit    (Dungeon Level 5 and deeper)
 *   troll pit  (Dungeon Level 20 and deeper)
 *   giant pit  (Dungeon Level 40 and deeper)
 *   dragon pit (Dungeon Level 60 and deeper)
 *   demon pit  (Dungeon Level 80 and deeper)
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
	int tmp, what[16];

	int i, j, y, x, y1, x1, y2, x2;

	int rating_bonus=0;

	bool empty = FALSE;

	int light = FALSE;

	cptr name ="monster";


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

	/* Open the inner room with a secret door */
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);


	/* Choose a pit type */
	tmp = randint(p_ptr->depth*2);

	/* Dungeon pit */
	if ((adult_campaign) && (tmp < 10))
	{
		/* Describe */
		name = "themed";

		get_mon_num_hook = vault_aux_theme_pit;

		/* Appropriate rating bonus */
		rating_bonus = 10;

		/* Initialise room description */
		room_info[dun->cent_n+1].type = ROOM_PIT_THEME;

	}
	else if (tmp < 20)
	{
		/* Message */
		name = "orc";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_orc;

		/* Appropriate rating bonus */
		rating_bonus = 30 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_PIT_ORC;

	}

	/* Troll pit */
	else if (tmp < 40)
	{
		/* Message */
		name = "troll";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_troll;

		/* Appropriate rating bonus */
		rating_bonus = 35 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_PIT_TROLL;

	}

	/* Giant pit */
	else if (tmp < 60)
	{
		/* Message */
		name = "giant";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_giant;

		/* Appropriate rating bonus */
		rating_bonus = 65 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_PIT_GIANT;
	}

	/* Dragon pit */
	else if (tmp < 80)
	{
		/* Pick dragon type */
		/* Now include metallic dragons */
		switch (rand_int(11))
		{
			/* Black */
			case 0:
			{
				/* Message */
				name = "acid dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_ACID;

				/* Done */
				break;
			}

			/* Blue */
			case 1:
			{
				/* Message */
				name = "electric dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_ELEC;

				/* Done */
				break;
			}

			/* Red */
			case 2:
			{
				/* Message */
				name = "fire dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_FIRE;

				/* Done */
				break;
			}

			/* White */
			case 3:
			{
				/* Message */
				name = "cold dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_COLD;

				/* Done */
				break;
			}

			/* Green */
			case 4:
			{
				/* Message */
				name = "poison dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_POIS;

				/* Done */
				break;
			}

			/* Multi-hued */
			case 5:
			{
				/* Message */
				name = "multi-hued dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (RF4_BR_ACID | RF4_BR_ELEC |
							  RF4_BR_FIRE | RF4_BR_COLD |
							  RF4_BR_POIS);

				/* Done */
				break;
			}

			/* Brass */
			case 6:
			{
				/* Message */
				name = "brass dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (RF4_BR_ACID | RF4_BR_DISE);

				/* Done */
				break;
			}


			/* Copper */
			case 7:
			{
				/* Message */
				name = "copper dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (RF4_BR_ELEC | RF4_BR_NEXU);

				/* Done */
				break;
			}

			/* Bronze */
			case 8:
			{
				/* Message */
				name = "bronze dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (RF4_BR_FIRE | RF4_BR_CONF);

				/* Done */
				break;
			}

			/* Silver */
			case 9:
			{
				/* Message */
				name = "silver dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (RF4_BR_COLD | RF4_BR_NEXU);

				/* Done */
				break;
			}

			/* Gold */
			case 10:
			{
				/* Message */
				name = "gold dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (RF4_BR_COLD | RF4_BR_SOUN);

				/* Done */
				break;
			}


		}

		/* Appropriate rating bonus */
		rating_bonus = 75 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_PIT_DRAGON;

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_dragon;
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


	/* Pick some monster types */
	for (i = 0; i < 16; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(p_ptr->depth + 10);

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

	/* Select the entries */
	for (i = 0; i < 8; i++)
	{
		/* Every other entry */
		what[i] = what[i * 2];
	}


	/* Message */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster pit (%s)", name);
	}

	/* Increase the level rating */
	if (rating_bonus>0) rating += rating_bonus;

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



/*
 * Hack -- fill in "vault" rooms
 */
static void build_vault(int y0, int x0, int ymax, int xmax, cptr data)
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
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster */
				case '@':
				{
					monster_level = p_ptr->depth + 11;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster, plus treasure */
				case '9':
				{
					monster_level = p_ptr->depth + 9;
					place_monster(y, x, TRUE, TRUE);
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
					place_monster(y, x, TRUE, TRUE);
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
						place_monster(y, x, TRUE, TRUE);
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
		}
	}
}



/*
 * Type 7 -- simple vaults (see "v_info.txt")
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
	if (cheat_room) msg_print("Lesser Vault");

	/* Initialize room description */
	room_info[dun->cent_n+1].type = ROOM_LESSER_VAULT;
	room_info[dun->cent_n+1].flags |= (ROOM_ICKY);

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
 * Type 8 -- greater vaults (see "v_info.txt")
 */
static void build_type8(int y0, int x0)
{
	vault_type *v_ptr;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* Accept the first greater vault */
		if (v_ptr->typ == 8) break;
	}

	/* Message */
	if (cheat_room) msg_print("Greater Vault");

	/* Initialize room description */
	room_info[dun->cent_n+1].type = ROOM_GREATER_VAULT;
	room_info[dun->cent_n+1].flags |= ROOM_ICKY;

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
 * Hack -- fill in "tower" rooms
 *
 * Similar to vaults, but we never place monsters/traps/treasure.
 */
static void build_tower(int y0, int x0, int ymax, int xmax, cptr data)
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

		/* Tunnel through all other walls and bridge features */
		else if ((f_info[cave_feat[tmp_row][tmp_col]].flags1 & (FF1_TUNNEL)) ||
		(f_info[cave_feat[tmp_row][tmp_col]].flags2 & (FF2_BRIDGE)))
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

		if (f_info[cave_feat[y][x]].flags2 & (FF2_BRIDGE))
		{
			/* Clear previous contents, bridge it */
			cave_alter_feat(y, x, FS_BRIDGE);
		}
		else
		{
			/* Clear previous contents, bridge it */
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
		case 8: build_type8(y, x); break;
		case 7: build_type7(y, x); break;
		case 6: build_type6(y, x); break;
		case 5: build_type5(y, x); break;
		case 4: build_type4(y, x); break;
		case 3: build_type3(y, x); break;
		case 2: build_type2(y, x); break;
		case 1: build_type1(y, x); break;

		default: if ((!dun->entrance) && (p_ptr->depth < max_depth(p_ptr->dungeon)))
		{
			cave_set_feat(y,x,FEAT_ENTRANCE);
			dun->entrance = TRUE;
			break;
		}
	}


	/* Reserve some blocks */
	for (by = by1; by <= by2; by++)
	{
		for (bx = bx1; bx <= bx2; bx++)
		{
			dun->room_map[by][bx] = TRUE;

			dun_room[by][bx] = dun->cent_n+1;

		}
	}

	/* Save the room location */
	if (dun->cent_n < CENT_MAX)
	{
		dun->cent[dun->cent_n].y = y;
		dun->cent[dun->cent_n].x = x;
		dun->cent_n++;
	}

	/* Count "crowded" rooms */
	if ((typ == 5) || (typ == 6)) dun->crowded = TRUE;

	/* Success */
	return (TRUE);
}


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

	if (level_flag)
	{

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

	/* Okay */
	return (TRUE);
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static void cave_gen(void)
{
	int i, k, y, x, y1, x1;

	int by, bx;

	bool destroyed = FALSE;

	bool battlefield = FALSE;

	char *name;

	dun_data dun_body;

	dungeon_zone *zone=&t_info[0].zone[0];

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));
	bool daytime = (((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2)));

	/* Global data */
	dun = &dun_body;

	level_flag = 0x00;

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Hack -- Start with basic granite */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Create air */
			if ((zone->tower) && (p_ptr->depth > min_depth(p_ptr->dungeon))) cave_set_feat(y,x,FEAT_CHASM);

			/* Create ground */
			else if (surface)
			{
				cave_set_feat(y,x,FEAT_GROUND);
				if (zone->fill) build_terrain(y,x,zone->fill);
			}

			/* Create granite wall */
			else
			{
				cave_set_feat(y,x,FEAT_WALL_EXTRA);
				if (zone->fill) build_terrain(y,x,zone->fill);
			}
		}
	}

	/* Possible "destroyed" level */
	if ((p_ptr->depth > 10) && (rand_int(DUN_DEST) == 0)) destroyed = TRUE;

	/* Hack -- No destroyed "quest" levels */
	if (is_quest(p_ptr->depth)) destroyed = FALSE;

	/* Hack -- No destroyed "bottom" levels */
	if (p_ptr->depth == max_depth(p_ptr->dungeon)) destroyed = FALSE;

	/* Hack -- No destroyed unusual levels */
	if ((zone->fill) && !(f_info[zone->fill].flags1 & (FF1_WALL))) destroyed = FALSE;

	/* Hack -- No destroyed slightly unusual levels */
	if (!zone->big) destroyed = FALSE;
	if (!zone->small) destroyed = FALSE;

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


	/* No "crowded" rooms yet */
	dun->crowded = FALSE;

	/* No "entrance" yet */
	dun->entrance = FALSE;

	/* No rooms yet */
	dun->cent_n = 0;

	/* No features on destroyed level */
	if ((!destroyed) && (variant_lake_feats) && (!(zone->tower) || (p_ptr->depth <=min_depth(p_ptr->dungeon))) )
	{
		bool big = FALSE;
		bool done_big = FALSE;

		int feat;
		int count=0;

		/* Allocate some lakes and rivers*/
		while ((randint(100)<DUN_FEAT) || (zone->big) || (zone->small))
		{
			/* Increase count */
			count++;

			/* No more than 3 features */
			if (count > DUN_MAX_LAKES) break;

			get_feat_num_hook = cave_feat_lake;

			get_feat_num_prep();

			feat = get_feat_num(p_ptr->depth);

			get_feat_num_hook = NULL;

			get_feat_num_prep();

			if ((zone->big) && (count == 1)) feat = zone->big;
			else if (zone->small) feat = zone->small;

			if (feat)
			{			     
				if ((!done_big) && (variant_big_feats))
				{
					big = randint(150) < p_ptr->depth;

					if (zone->big) big = TRUE;

					done_big = big;
				}
				else
				{
					big = FALSE;
				}

				/* Room type */
				if (cheat_room)
				{

					name = (f_name + f_info[feat].name);

					if (f_info[feat].edge)
					{
						cptr edge;

						edge = (f_name + f_info[f_info[feat].edge].name);

						msg_format("Building %s%s surrounded by %s.", (big?"big ":""),name,edge);
					}
					else
					{
					msg_format ("Building %s%s.", (big?"big ":""),name);
					}
				}

				/* Hack -- Choose starting point */
				y = rand_spread(DUNGEON_HGT / 2, 10);
				x = rand_spread(DUNGEON_WID / 2, 15);

				build_feature(y, x, feat, big);

				if (f_info[feat].flags2 & (FF2_WATER)) level_flag |= (LF1_WATER);
				if (f_info[feat].flags2 & (FF2_LAVA)) level_flag |= (LF1_LAVA);
				if (f_info[feat].flags2 & (FF2_ICE)) level_flag |= (LF1_ICE);
				if (f_info[feat].flags2 & (FF2_ACID)) level_flag |= (LF1_ACID);
				if (f_info[feat].flags2 & (FF2_OIL)) level_flag |= (LF1_OIL);
				if (f_info[feat].flags2 & (FF2_CHASM)) level_flag |= (LF1_CHASM);

			}

			get_feat_num_hook = NULL;

		}

	}

	/* Hack -- build a tower in the centre of the level */
	if ((zone->tower) && (p_ptr->depth >= min_depth(p_ptr->dungeon)))
	{

		int typ = v_info[zone->tower].typ;

		/* Hack - set to center of level */
		int by0 = dun->row_rooms /2;
		int bx0 = dun->col_rooms /2;

		/* Extract blocks */
		int by1 = by0 + room[typ].dy1;
		int bx1 = bx0 + room[typ].dx1;
		int by2 = by0 + room[typ].dy2;
		int bx2 = bx0 + room[typ].dx2;

		vault_type *v_ptr;

		/* It is *extremely* important that the following calculation */
		/* be *exactly* correct to prevent memory errors XXX XXX XXX */
	
		/* Get the location of the tower */
		y = ((by1 + by2 + 1) * BLOCK_HGT) / 2;
		x = ((bx1 + bx2 + 1) * BLOCK_WID) / 2;

		/* Hack -- are we directly above another tower? */
		if ((p_ptr->depth == zone->level) && (p_ptr->depth > min_depth(p_ptr->dungeon)))
		{

			dungeon_zone *roof;
	
			get_zone(&roof,p_ptr->dungeon,p_ptr->depth-1);

			v_ptr = &v_info[roof->tower];

			build_roof(y, x, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
		}

		v_ptr = &v_info[zone->tower];

		/* Hack -- Build the tower */
		build_tower(y, x, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);

		/* Reserve some blocks */
		for (by = by1; by <= by2; by++)
		{
			for (bx = bx1; bx <= bx2; bx++)
			{
				dun->room_map[by][bx] = TRUE;
	
				dun_room[by][bx] = dun->cent_n+1;
	
			}
		}
	
		/* Initialise room description */
		room_info[dun->cent_n+1].type = ROOM_TOWER;
		room_info[dun->cent_n+1].flags = 0;
		dun->cent[dun->cent_n].y = y;
		dun->cent[dun->cent_n].x = x;
		dun->cent_n++;


		/* Hack -- descending player always in tower */
		if ((surface) && (p_ptr->create_up_stair))
		{
			player_place(y, x);
		}

		/* Hack -- always have upstairs */
		else if (surface)
		{
			feat_near(FEAT_LESS, y, x);
		}

	}

	/* Hack -- All levels deeper than 20 on surface are 'destroyed' */
	if ((p_ptr->depth > 20) && (surface)) destroyed = TRUE;

	/* Hack -- All levels with escorts are 'battlefields' */
	if (RF1_ESCORT & (1L << (t_info[p_ptr->dungeon].r_flag-1))) battlefield = TRUE;
	if (RF1_ESCORTS & (1L << (t_info[p_ptr->dungeon].r_flag-1))) battlefield = TRUE;

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


		/* Towers don't have other rooms above the surface */
		if ((zone->tower) && !(p_ptr->depth <= min_depth(p_ptr->dungeon)))
		{
			/* Never mind */
			continue;
		}
		/* Battle-fields don't have rooms */
		else if (battlefield)
		{

			/* Attempt a "non-existent" room */
			if (room_build(by, bx, 0)) continue;

			/* Never mind */
			continue;
		}

		/* Destroyed levels are boring */
		else if (destroyed)
		{
			/* Attempt a "trivial" room */
			if (room_build(by, bx, 1)) continue;

			/* Never mind */
			continue;
		}
		/* Otherwise don't build rooms */
		else if (surface)
		{

			/* Attempt a "non-existent" room */
			if (room_build(by, bx, 0)) continue;

			/* Never mind */
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

		/* Attempt a trivial room */
		if (room_build(by, bx, 1)) continue;
	}

	/* Special boundary walls -- Top */
	for (x = 0; x < DUNGEON_WID; x++)
	{
		y = 0;

		cave_feat[y][x] = FEAT_PERM_SOLID;

		cave_info[y][x] |= (CAVE_WALL);
	}

	/* Special boundary walls -- Bottom */
	for (x = 0; x < DUNGEON_WID; x++)
	{
		y = DUNGEON_HGT-1;

		cave_feat[y][x] = FEAT_PERM_SOLID;

		cave_info[y][x] |= (CAVE_WALL);
	}

	/* Special boundary walls -- Left */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		x = 0;

		cave_feat[y][x] = FEAT_PERM_SOLID;

		cave_info[y][x] |= (CAVE_WALL);
	}

	/* Special boundary walls -- Right */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		x = DUNGEON_WID-1;

		cave_feat[y][x] = FEAT_PERM_SOLID;

		cave_info[y][x] |= (CAVE_WALL);
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

	/* Paranoia */
	if (dun->cent_n)
	{
		/* Hack -- connect the first room to the last room */
		y = dun->cent[dun->cent_n-1].y;
		x = dun->cent[dun->cent_n-1].x;
	}

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


	/* Hack -- Sandstone streamers are shallow */
	if (rand_int(DUN_STR_SLV) > p_ptr->depth)
	{

		/* Hack -- Add some sandstone streamers */
		for (i = 0; i < DUN_STR_SAN; i++)
		{
			build_streamer(FEAT_SANDSTONE, 0);
		}
	}

	else
	{

		/* Hack -- Add some magma streamers */
		for (i = 0; i < DUN_STR_MAG; i++)
		{
			build_streamer(FEAT_MAGMA, DUN_STR_MC);
		}

	}

	/* Hack -- Add some quartz streamers */
	for (i = 0; i < DUN_STR_QUA; i++)
	{		
		build_streamer(FEAT_QUARTZ, DUN_STR_QC);
	}

	/* Hack -- Add a rich mineral vein very rarely */
	if ((variant_new_feats) && (!rand_int(DUN_STR_GOL)))
	{
		build_streamer(FEAT_QUARTZ, DUN_STR_GC);
	}

	/* Hack -- Add cracks through the dungeon occasionally */
	if ((variant_new_feats) && !(rand_int(DUN_STR_CRA)))
	{
		build_streamer(FEAT_WALL_C, DUN_STR_CC);
	}

	/* Destroy the level if necessary */
	if (destroyed) destroy_level();

	k = 0;

	/* Hack -- make sure we have rooms/corridors to place stuff */
	if ((((!surface) || (destroyed)) && !(battlefield)) || (zone->tower))
	{
		/* Place 3 or 4 down stairs near some walls */
		alloc_stairs(FEAT_MORE, rand_range(3, 4), 3);
	
		/* Place 1 or 2 up stairs near some walls */
		alloc_stairs(FEAT_LESS, rand_range(1, 2), 3);
	
		/* Basic "amount" */
		k = (p_ptr->depth / 3);
		if (k > 10) k = 10;
		if (k < 2) k = 2;
	
		/* Put some rubble in corridors */
		if (!zone->tower) alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));
	
		/* Place some traps in the dungeon */
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));
	
		/* Place some features in rooms */
		alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_FEATURE, 1);
	}
	/* Hack -- have more monsters at night */
	else if (!daytime)
	{
		/* Basic "amount" */
		k = (p_ptr->depth / 3);
		if (k > 10) k = 10;
		if (k < 2) k = 2;
	}

	/* Determine the character location */
	if ((p_ptr->py == 0) || (p_ptr->px == 0)) new_player_spot();

	/* Ensure wandering monsters suit the dungeon level */
	get_mon_num_hook = dun_level_mon;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL + randint(8);

	/* Put some monsters in the dungeon */
	for (i = i + k; i > 0; i--)
	{
		(void)alloc_monster(0, TRUE);
	}

	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Hack -- make sure we have rooms/corridors to place stuff */
	if ((((!surface) || (destroyed)) && !(battlefield)) || (zone->tower))
	{
		/* Put some objects in rooms */
		alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ROOM, 3));
	
		/* Put some objects/gold in the dungeon */
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ITEM, 3));
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal(DUN_AMT_GOLD, 3));
	}

	/* Apply illumination */
	if (surface) town_illuminate(daytime);

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

				/* Pick a location */
				while (1)
				{
					y = rand_int(DUNGEON_HGT);
					x = rand_int(DUNGEON_WID);

					if (cave_naked_bold(y, x)) break;
				}

				/* Place the questor */
				place_monster_aux(y, x, i, TRUE, TRUE);
			}
		}
	}
	/* Ensure guardian monsters */
	else if ((zone->guard) && (r_info[zone->guard].cur_num <= 0))
	{
		int y, x;

		/* Pick a location */
		while (1)
		{
			y = rand_int(DUNGEON_HGT);
			x = rand_int(DUNGEON_WID);

			if (place_monster_here(y, x, zone->guard)) break;
		}

		/* Place the questor */
		place_monster_aux(y, x, zone->guard, TRUE, TRUE);
	}
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
	bool building = (((d_char >= '0') && (d_char <= '8')) || (d_char == '+'));

	/* Find the "center" of the store */
	y0 = yy * 9 + 6;
	x0 = xx * 14 + 12;

	/* Determine the store boundaries */
	y1 = y0 - randint((yy == 0) ? 3 : 2);
	y2 = y0 + randint((yy == 1) ? 3 : 2);
	x1 = x0 - randint(5);
	x2 = x0 + randint(5);

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

#if 0
	/* Hack -- place small terrain */
	if (zone->small)
	{
		build_feature(TOWN_HGT/2, TOWN_WID/2, zone->small, FALSE);
	}
#endif

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

	/* Place the down stairs */
	while (TRUE)
	{
		/* Pick a location at least "three" from the outer walls */
		y = rand_range(3, TOWN_HGT - 4);
		x = rand_range(3, TOWN_WID - 4);

		/* Require a "naked" floor grid */
		if (cave_naked_bold(y, x)) break;
	}

	/* Clear previous contents, add down stairs */
	if (p_ptr->depth < max_depth(p_ptr->dungeon))
	{
		if (p_ptr->depth == min_depth(p_ptr->dungeon)) cave_set_feat(y, x, FEAT_ENTRANCE);
		else cave_set_feat(y, x, FEAT_MORE);
	}

	/* Place the player */
	player_place(y, x);

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
	if (p_ptr->depth > min_depth(p_ptr->dungeon)) cave_set_feat(y, x, FEAT_LESS);

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

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

	bool daytime = ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2));

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

	/* Day time */
	if (surface && daytime)
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
			cave_set_feat(y, x, FEAT_GROUND);
		}
	}

	/* Build stuff */
	town_gen_hack();

	/* Apply illumination */
	town_illuminate(daytime);

	/* Ensure guardian monsters */
	if (!(daytime && surface) && (zone->guard) && (r_info[zone->guard].cur_num <= 0))
	{
		/* Pick a location */
		while (1)
		{
			y = rand_range(3, TOWN_HGT - 4);
			x = rand_range(3, TOWN_WID - 4);

			/* Require a "naked" floor grid */
			if (cave_naked_bold(y, x)) break;
		}

		/* Place the questor */
		place_monster_aux(y, x, zone->guard, TRUE, TRUE);
	}

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

	dungeon_zone *zone=&t_info[0].zone[0];

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* The dungeon is not ready */
	character_dungeon = FALSE;

        /* There is no dynamic terrain */
        dyna_full = FALSE;

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;

		cptr why = NULL;


		/* Reset */
		o_max = 1;
		m_max = 1;

		/* Start with a blank cave */
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
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

		/* Build the town */
		if (!zone->fill)
		{
			/* Make a town */
			town_gen();
		}

		/* Build a real level */
		else
		{
			/* Make a dungeon */
			cave_gen();
		}

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
		if ((turn - old_turn) < 1000) feeling = 0;

		/* Hack -- no feeling in the town */
		if (!zone->fill) feeling = 0;

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

		/* Mega-Hack -- "auto-scum" */
		if (auto_scum && (num < 100) && !surface)
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



