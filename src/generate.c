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
 * Note that level generation is *not* an important bottleneck,
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
 */

/*
 * Dungeon generation values
 */
#define DUN_UNUSUAL	220	/* Level/chance of unusual room */
#define DUN_DEST	35	/* 1/chance of having a destroyed level */
#define SMALL_LEVEL 10	/* 1/chance of smaller size */

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND	0	/* Chance of random direction. */
#define DUN_TUN_CHG	77	/* Chance of changing direction. */
#define DUN_TUN_CON	0	/* Chance of extra tunneling */
#define DUN_TUN_PEN	0	/* Chance of doors at room entrances */
#define DUN_TUN_JCT	0	/* Chance of doors at tunnel junctions */

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
 * Dungeon treasure allocation values
 */
#define DUN_AMT_ROOM	1	/* Amount of objects for rooms */
#define DUN_AMT_GOLD	5	/* Amount of treasure for rooms/corridors */
#define DUN_AMT_PORTAL	1	/* Amount of portals for corridors */

/*
 * Hack -- Dungeon allocation "places"
 */
#define ALLOC_SET_CORR		1	/* Hallway */
#define ALLOC_SET_ROOM		2	/* Room */
#define ALLOC_SET_BOTH		3	/* Anywhere */
#define ALLOC_SET_TINY		4	/* Tiny rooms (icky non-rooms) */

/*
 * Hack -- Dungeon allocation "types"
 */
#define ALLOC_TYP_RUBBLE	1	/* Rubble */
#define ALLOC_TYP_TRAP		2	/* Trap */
#define ALLOC_TYP_GOLD		3	/* Gold */
#define ALLOC_TYP_OBJECT	4	/* Object */
#define ALLOC_TYP_FAERY_PORTAL	5	/* Faery portal */

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

	/* Array of partitions of rooms */
	int part_n;
	int part[CENT_MAX];

	/* Array of rooms styles */
	/* 1 = wilderness, 2 = poor civilized, 3 = normal civilized, 4 rich civilized, 5 = magical, 6 = holy */
	int style_n;
	int style[CENT_MAX];

	/* Array of monster themes */
	/* 1 = wilderness, 2 = poor civilized, 3 = normal civilized, 4 rich civilized, 5 = magical, 6 = holy */
	int monster_n;
	int monster[CENT_MAX];

	/* Array of joined rooms. Record the room number that this room is joined to. */
	int joined_n;
	int joined[CENT_MAX];

	/* Array of closest rooms. Record the closest room that is connected to this one, but not joined. */
	int closest_n;
	int closest[CENT_MAX];

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
static room_data room[ROOM_MAX] =
{
	{ 0, 0, 0, 0, 0 },		/* 0 = Nothing */
	{ 0, 0, -1, 1, 1 },		/* 1 = Simple (33x11) */
	{ 0, 0, -1, 1, 1 },		/* 2 = Overlapping (33x11) */
	{ 0, 0, -1, 1, 3 },		/* 3 = Crossed (33x11) */
	{ 0, 0, -1, 1, 3 },		/* 4 = Large (33x11) */
	{ 0, 0, -1, 1, 5 },		/* 5 = Monster nest (33x11) */
	{ 0, 0, -1, 1, 5 },		/* 6 = Monster pit (33x11) */
	{ 0, 1, -1, 1, 5 },		/* 7 = Lesser vault (44x22) */
	{ -1, 2, -2, 3, 10 },	/* 8 = Greater vault (66x44) */
	{ 0, 1, -1, 1, 0 }		/* 9 = Quest vault (44x22) */
};

/* Hack - variables for allocations */
static s16b mon_gen, obj_gen;

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

	/* Place the player */
	while (TRUE)
	{
		/* Pick a legal spot */
		y = rand_range(1, p_ptr->cur_map_hgt - 2);
		x = rand_range(1, p_ptr->cur_map_wid - 2);

		/* Must be a "naked" floor grid */
		if (!cave_naked_bold(y, x)) continue;

		/* Must have no decoration */
		if (decoration(y, x)) continue;

		/* Refuse to start on anti-teleport grids */
		if (cave_info[y][x] & (CAVE_ICKY)) continue;

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

	if (cave_feat[y+1][x] >= FEAT_WALL_EXTRA) k++;
	if (cave_feat[y-1][x] >= FEAT_WALL_EXTRA) k++;
	if (cave_feat[y][x+1] >= FEAT_WALL_EXTRA) k++;
	if (cave_feat[y][x-1] >= FEAT_WALL_EXTRA) k++;

	return (k);
}

/*
 * Count the number of walls and room squares adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 *
 * We count only granite walls and permanent walls.
 */
static int next_to_walls_and_room(int y, int x)
{
	int k = 0;

	if (cave_feat[y+1][x] >= FEAT_WALL_EXTRA) k++;
	else if (cave_info[y+1][x] & (CAVE_ROOM)) k++;

	if (cave_feat[y-1][x] >= FEAT_WALL_EXTRA) k++;
	else if (cave_info[y-1][x] & (CAVE_ROOM)) k++;

	if (cave_feat[y][x+1] >= FEAT_WALL_EXTRA) k++;
	else if (cave_info[y][x+1] & (CAVE_ROOM)) k++;

	if (cave_feat[y][x-1] >= FEAT_WALL_EXTRA) k++;
	else if (cave_info[y][x-1] & (CAVE_ROOM)) k++;

	return (k);
}

/*
 * Count the number of outer walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 */
static int next_to_outer_walls(int y, int x)
{
	int k = 0;

	if ((cave_feat[y+1][x] == FEAT_WALL_OUTER) && (!(decoration(y+1, x))))
	{
		k++;
		cave_set_feat(y+1, x, FEAT_WALL_INNER);
	}

	if ((cave_feat[y-1][x] == FEAT_WALL_OUTER) && (!(decoration(y-1, x))))
	{
		k++;
		cave_set_feat(y-1, x, FEAT_WALL_INNER);
	}

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
			for (j = 0; !flag && j <= 30000; j++)
			{
				/* Pick a random grid */
				y = rand_int(p_ptr->cur_map_hgt);
				x = rand_int(p_ptr->cur_map_wid);

				/* Require "naked" floor grid */
				if (!cave_naked_bold(y, x)) continue;

				/* No stairs near decorations */
				if decoration(y,x) continue;
				if decoration(y+1,x) continue;
				if decoration(y,x+1) continue;
				if decoration(y-1,x) continue;
				if decoration(y,x-1) continue;
				if decoration(y+1,x+1) continue;
				if decoration(y-1,x-1) continue;
				if decoration(y+1,x-1) continue;
				if decoration(y-1,x+1) continue;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(y, x) < walls) continue;

				/* Town -- must go down */
				if (!p_ptr->depth)
				{
					/* Clear previous contents, add down stairs */
					cave_set_feat(y, x, FEAT_MORE);
				}

				/* Quest -- must go up */
				else if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
						 (quest_check(p_ptr->depth) == QUEST_FIXED_U) ||
						 (p_ptr->depth >= MAX_DEPTH-1))
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
	int y, x, k, i;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		for (i = 0; i < 10000; i++)
		{
			bool room;
			bool icky;

			/* Location */
			y = rand_int(p_ptr->cur_map_hgt);
			x = rand_int(p_ptr->cur_map_wid);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue; 

			/* Check for "room" */
			room = (cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE;

			/* Check for "icky" */
			icky = (cave_info[y][x] & (CAVE_ICKY)) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

			/* Require tiny room? */
			if ((set == ALLOC_SET_TINY) && (room || !icky) && (rand_int(100) < 98)) continue;

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
				place_trap_dungeon(y, x);
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

			case ALLOC_TYP_FAERY_PORTAL:
			{
				place_decoration(y, x, WG_FAERY_PORTAL);
				break;
			}
		}
	}
}

/*
 * Count the number of "floor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y1, x1)"
 */
static int next_to_floor_tunnel(int y1, int x1)
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

		/* Tiny rooms are common behind cave paintings and other tiny rooms */
		if (t_list[cave_t_idx[y1][x1]].w_idx == WG_PAINTING)
		{
		}
		else if (t_list[cave_t_idx[y1][x1]].w_idx >= WG_WARD_SLUMBER_ACTIVE_HIDDEN)
		{
		}
		else if (cave_info[y][x] & (CAVE_ICKY))
		{
			/* The tiny rooms like to stick together! */
		}
		else if (rand_int(100) < 99) continue;

		/* Count these grids */
		k++;
	}

	/* Return the number of floor grids */
	return (k);
}

/*
 * Count the number of "floor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y1, x1)"
 */
static int next_to_floor(int y1, int x1)
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

		/* Skip grids outside rooms */
		if (!(cave_info[y][x] & (CAVE_ROOM))) continue;

		/* Count these grids */
		k++;
	}

	/* Return the number of floor grids */
	return (k);
}

/*
 * Count the number of shelves adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y1, x1)"
 */
static int next_to_feature(int y1, int x1, int feature)
{
	int i, y, x, k = 0;

	/* Scan adjacent grids */
	for (i = 0; i < 4; i++)
	{
		/* Extract the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		if (feature == WG_PAINTING)
		{
			if (t_list[cave_t_idx[y][x]].w_idx >= WG_WARD_SLUMBER_ACTIVE) k++;
		}

		if (!(t_list[cave_t_idx[y][x]].w_idx == feature)) continue;

		/* Count these grids */
		k++;
	}

	/* Return the number of floor grids */
	return (k);
}

/*
 * Count the number of floor square in a room next to a square
 *
 * Note -- Assumes "in_bounds_fully(y1, x1)"
 */
static int next_to_room_floor(int y, int x)
{
	int k = 0;

	if ((cave_feat[y+1][x] == FEAT_FLOOR) && (cave_info[y+1][x] & (CAVE_ROOM))) k++;
	if ((cave_feat[y][x+1] == FEAT_FLOOR) && (cave_info[y][x+1] & (CAVE_ROOM))) k++;
	if ((cave_feat[y-1][x] == FEAT_FLOOR) && (cave_info[y-1][x] & (CAVE_ROOM))) k++;
	if ((cave_feat[y][x-1] == FEAT_FLOOR) && (cave_info[y][x-1] & (CAVE_ROOM))) k++;
	if ((cave_feat[y+1][x+1] == FEAT_FLOOR) && (cave_info[y+1][x+1] & (CAVE_ROOM))) k++;
	if ((cave_feat[y-1][x-1] == FEAT_FLOOR) && (cave_info[y-1][x-1] & (CAVE_ROOM))) k++;
	if ((cave_feat[y+1][x-1] == FEAT_FLOOR) && (cave_info[y+1][x-1] & (CAVE_ROOM))) k++;
	if ((cave_feat[y-1][x+1] == FEAT_FLOOR) && (cave_info[y-1][x+1] & (CAVE_ROOM))) k++;

	/* Return the number of floor grids */
	return (k);
}

/*
 * Build a destroyed level
 */
static void destroy_level(void)
{
	int y1, x1, y, x, k, t, n;

	/* Note destroyed levels */
	if (cheat_room) message(MSG_CHEAT, 0, "Destroyed Level");

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
			while (TRUE)
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
 * Hack -- Place some sleeping monsters near the given location
 */
static void vault_monsters(int y1, int x1, int num)
{
	int k, i, y, x, xx;

	/* Try to summon "num" monsters "near" the given location */
	for (k = 0; k < num; k++)
	{
		/* Try nine locations */
		for (i = 0; i < 999; i++)
		{
			/* Pick a nearby location */
			scatter(&y, &x, y1, x1, 10);

			/* Require "empty" floor grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Place the monster (allow groups) */
			monster_level = p_ptr->depth + 2;
		
			for (xx = 0; xx < 999; xx++)	
			{
				if (place_monster(y, x)) break;
			}

			monster_level = p_ptr->depth;

			if (++k >= num) break;
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
 * Generate helper -- fill a tiny room
 */
static void generate_fill_tiny_room(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_set_feat(y, x, feat);
			cave_info[y][x] = (CAVE_ICKY);
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
 * Generate helper -- open one side of a rectangle with a feature
 */
static void generate_hole(int y1, int x1, int y2, int x2, int feat)
{
	int y0, x0;
	int xx = 0, yy = 0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	/* Open random side */
	switch (rand_int(4))
	{
		case 0:
		{
			xx = x0;
			yy = y1;
			break;
		}
		case 1:
		{
			xx = x1;
			yy = y0;
			break;
		}
		case 2:
		{
			xx = x0;
			yy = y2;
			break;
		}
		case 3:
		{
			xx = x2;
			yy = y0;
			break;
		}
	}

	cave_set_feat(yy, xx, feat);
	
	/* Hack - lock closed doors
	if (feat == FEAT_CLOSED) place_lock(yy, xx, TRUE, WG_DOOR_LOCK); */

	/* Hack - sometimes lock secret doors
	if ((feat == FEAT_SECRET) && rand_int(2)) place_lock(yy, xx, FALSE, WG_DOOR_LOCK); */
}

/*
 * Generate helper -- fill a rectangle with a tree
 */
static void generate_fill_tree(int y1, int x1, int y2, int x2, bool fountain, bool circular)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			if (cave_clean_bold(y, x))
			{
				if ((circular) && (x == x1) && (y == y1)) {}
				else if ((circular) && (x == x2) && (y == y1)) {} 
				else if ((circular) && (x == x1) && (y == y2)) {}
				else if ((circular) && (x == x2) && (y == y2)) {}
				else if (rand_int(100) < 37)
				{
					place_decoration(y, x, WG_TREE);
				}
				else if (rand_int(100) < 36)
				{
					place_decoration(y, x, WG_VEGETATION);
				}
				else if (rand_int(100) < 7)
				{
					place_decoration(y, x, WG_INTERESTING_VEGETATION);
				}
			}
		}
	}

	if (fountain)
	{
		place_decoration(y1 + ((y2-y1) / 2) + rand_int(2), x1 + ((x2-x1) / 2) + rand_int(2), WG_FOUNTAIN_HARPY + rand_int(11));
	}

	return;
}

/*
 * Generate helper -- place empty floor with some traps in a rectangle
 */
static void generate_fill_traps(int y1, int x1, int y2, int x2)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* Require "naked" floor grids */
			if (!cave_naked_bold(y, x)) continue;

			/* First delete existing trap */
			delete_trap(y, x);

			/* Sometimes place a trap */
			if (rand_int(100) < 7)
			{
				/* Place the trap */
				place_trap_dungeon(y, x);
			}
		}
	}
}

/*
 * Generate helper -- fill a rectangle with a table
 */
int generate_fill_table(int y1, int x1, int y2, int x2)
{
	int y, x;
	int success = 0;
	int height = y2 - y1 + 1;
	int width = x2 - x1 + 1;
	int feature;

	/* Try to fit the bigger special constructs first */

	/* 4x6 Broken Magic Circle */
	if (dun->style[dun->cent_n] == 5)
	{
		if (height > 4)
		{
			y1 = y1 + rand_int(height - 4);
			y2 = y1 + 3;
		}
		if (width > 6)
		{
			x1 = x1 + rand_int(width - 6);
			x2 = x1 + 5;
		}

		/* Make sure we aren't too close to a special wall */
		for (y = y1; y <= y2; y++)
		{
			for (x = x1; x <= x2; x++)
			{
				if (!cave_floor_bold(y, x))
				{
					/* If close to a special wall, generate no rectangle */
					if (decoration(y, x)) return(FALSE);
					if (decoration(y +1, x)) return(FALSE);
					if (decoration(y, x +1)) return(FALSE);
					if (decoration(y -1, x)) return(FALSE);
					if (decoration(y, x -1)) return(FALSE);
				}

				if (cave_feat[y][x] == FEAT_WALL_OUTER) return(FALSE);

				break;
			}
		}

		/* Make sure we don't paint on walls */
		if (cave_feat[y1+1][x1] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1][x1+1] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+2][x1+5] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+3][x1+4] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+2][x1] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+3][x1+1] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+1][x1+5] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1][x1+2] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1][x1+3] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1][x1+4] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+1][x1+1] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+1][x1+2] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+1][x1+3] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+1][x1+4] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+2][x1+1] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+2][x1+2] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+2][x1+3] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+2][x1+4] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+3][x1+2] == FEAT_WALL_OUTER) return(FALSE);
		if (cave_feat[y1+3][x1+3] == FEAT_WALL_OUTER) return(FALSE);

		int broken_border = randint(8);

		/* Draw the Edges */
		feature = WG_CIRCLE_BROKEN_EDGE_A;

		if (!(broken_border == 1)) place_decoration(y1+1, x1, feature);
		else place_decoration(y1+1, x1, WG_CIRCLE_MISSING_EDGE_A);

		if (!(broken_border == 2)) place_decoration(y1, x1+1, feature);
		else place_decoration(y1, x1+1, WG_CIRCLE_MISSING_EDGE_A);

		if (!(broken_border == 3)) place_decoration(y1+2, x1+5, feature);
		else place_decoration(y1+2, x1+5, WG_CIRCLE_MISSING_EDGE_A);

		if (!(broken_border == 4)) place_decoration(y1+3, x1+4, feature);
		else place_decoration(y1+3, x1+4, WG_CIRCLE_MISSING_EDGE_A);

		feature = WG_CIRCLE_BROKEN_EDGE_B;

		if (!(broken_border == 5)) place_decoration(y1+2, x1, feature);
		else place_decoration(y1+2, x1, WG_CIRCLE_MISSING_EDGE_B);

		if (!(broken_border == 6)) place_decoration(y1+3, x1+1, feature);
		else place_decoration(y1+3, x1+1, WG_CIRCLE_MISSING_EDGE_B);

		if (!(broken_border == 7)) place_decoration(y1+1, x1+5, feature);
		else place_decoration(y1+1, x1+5, WG_CIRCLE_MISSING_EDGE_B);

		if (!(broken_border == 8)) place_decoration(y1, x1+4, feature);
		else place_decoration(y1, x1+4, WG_CIRCLE_MISSING_EDGE_B);

		/* Draw the inside */
		feature = WG_CIRCLE_BROKEN;

		place_decoration(y1, x1+2, feature);
		place_decoration(y1, x1+3, feature);
		place_decoration(y1+1, x1+1, feature);
		place_decoration(y1+1, x1+2, feature);
		place_decoration(y1+1, x1+3, feature);
		place_decoration(y1+1, x1+4, feature);
		place_decoration(y1+2, x1+1, feature);
		place_decoration(y1+2, x1+2, feature);
		place_decoration(y1+2, x1+3, feature);
		place_decoration(y1+2, x1+4, feature);
		place_decoration(y1+3, x1+2, feature);
		place_decoration(y1+3, x1+3, feature);

		return(TRUE);
	}

	/* Make the ractangle smaller */
	else
	{
		if (rand_int(100) < 50) y1++;
		if (rand_int(100) < 50) y2--;
		if (rand_int(100) < 75) x1++;
		if (rand_int(100) < 75) x2--;

		if (x2 - x1 > 8)
		{
			if (rand_int(100) < 50) x1++;
			if (rand_int(100) < 50) x2--;
			if (rand_int(100) < 50) x1++;
			if (rand_int(100) < 50) x2--;
		}

		if (x2 - x1 > 5)
		{
			if (rand_int(100) < 50) x1++;
			if (rand_int(100) < 50) x2--;
			if (rand_int(100) < 50) x1++;
			if (rand_int(100) < 50) x2--;
		}

		if (((y2 - y1) > 3) && ((x2 - x1) > 3))
		{
			if (rand_int(100) < 60) y1++;
			if (rand_int(100) < 60) y2--;
			if (rand_int(100) < 50) x1++;
			if (rand_int(100) < 50) x2--;
		}

		/* Count height and width again */
		height = y2 - y1 + 1;
		width = x2 - x1 + 1;
	}

	/* Then try making smaller special constructs */
	if ((success == 0) && (dun->style[dun->cent_n] == 6) && (rand_int(100) < 90))
	{
		switch (randint(2))
		{
			case 1:
			{
				/* 3x3 altar */
				if ((height >= 3) && (width >= 3))
				{
					if (height > 3)
					{
						y1 = y1 + rand_int(height - 3);
						y2 = y1 + 2;
					}
					if (width > 3)
					{
						x1 = x1 + rand_int(width - 3);
						x2 = x1 + 2;
					}

					success = 1;
				}
				break;
			}
			case 2:
			{
				/* 2x5 altar */
				if ((height >= 2) && (width >= 5))
				{
					if (height > 2)
					{
						y1 = y1 + rand_int(height - 2);
						y2 = y1 + 1;
					}
					if (width > 5)
					{
						x1 = x1 + rand_int(width - 5);
						x2 = x1 + 4;
					}

					success = 2;
				}
				break;
			}
		}
	}

	/* Make sure we aren't too close to a shelf */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* If close to a shelf, generate no rectangle */
			if (decoration(y, x)) return(FALSE);
			if (decoration(y +1, x)) return(FALSE);
			if (decoration(y, x +1)) return(FALSE);
			if (decoration(y -1, x)) return(FALSE);
			if (decoration(y, x -1)) return(FALSE);

			if (cave_feat[y][x] == FEAT_WALL_OUTER) return(FALSE);
			if (cave_feat[y+1][x] == FEAT_WALL_OUTER) return(FALSE);
			if (cave_feat[y][x+1] == FEAT_WALL_OUTER) return(FALSE);
			if (cave_feat[y-1][x] == FEAT_WALL_OUTER) return(FALSE);
			if (cave_feat[y][x-1] == FEAT_WALL_OUTER) return(FALSE);
		}
	}


	/* Draw a 3x3 altar on a platform plus stairs */
	if (success == 1)
	{
		place_decoration(y1+1, x1+1, WG_ALTAR_OBSESSION + rand_int(5));

		place_decoration(y1, x1, WG_PLATFORM);
		place_decoration(y1, x2, WG_PLATFORM);
		place_decoration(y2, x1, WG_PLATFORM);
		place_decoration(y2, x2, WG_PLATFORM);
		place_decoration(y1, x1+1, WG_PLATFORM);
		place_decoration(y1+1, x1, WG_PLATFORM);
		place_decoration(y1+1, x2, WG_PLATFORM);
		place_decoration(y2, x1+1, WG_PLATFORM);

		/* Try to build steps, if there's free space */
		int ii = 0;
		int yyy = 0;
		int xxx = 0;

		for (ii = 0; ii <= 4; ii++)
		{
			switch (ii)
			{
				case 1:
				{
					yyy = y1 +1;
					xxx = x1 -1;
					break;
				}
				case 2:
				{
					yyy = y1 -1;
					xxx = x1 +1;
					break;
				}
				case 3:
				{
					yyy = y1 +1;
					xxx = x1 +3;
					break;
				}
				case 4:
				{
					yyy = y1 +3;
					xxx = x1 +1;
					break;
				}
			}

			if (cave_feat[yyy][xxx] == FEAT_WALL_OUTER) continue;
			if (cave_feat[yyy+1][xxx] == FEAT_WALL_OUTER) continue;
			if (cave_feat[yyy][xxx+1] == FEAT_WALL_OUTER) continue;
			if (cave_feat[yyy-1][xxx] == FEAT_WALL_OUTER) continue;
			if (cave_feat[yyy][xxx-1] == FEAT_WALL_OUTER) continue;

			place_decoration(yyy, xxx, WG_STEPS);
		}
	}

	/* Draw a 2x5 altar, with two possible shapes */
	else if ((success == 2) && (rand_int(100) < 50))
	{
		place_decoration(y1+1, x1+2, WG_ALTAR_OBSESSION + rand_int(5));
		place_decoration(y1+1, x1, WG_STATUE);
		place_decoration(y1, x1+1, WG_STATUE);
		place_decoration(y1, x2-1, WG_STATUE);
		place_decoration(y1+1, x2, WG_STATUE);
	}
	else if (success == 2)
	{
		place_decoration(y1, x1+2, WG_ALTAR_OBSESSION + rand_int(5));
		place_decoration(y1, x1, WG_STATUE);
		place_decoration(y1+1, x1+1, WG_STATUE);
		place_decoration(y1+1, x2-1, WG_STATUE);
		place_decoration(y1, x2, WG_STATUE);
	}

	/* Draw the table */
	else
	{
		/* Make tables a bit shorter */
		if ((x2 - x1) > 1)
		{
			if (rand_int(100) < 50) x1++;
			else x2--;
		}

		/* 3*3 or bigger tables look funny */
		for (y = 0; y < 8; y++)
		{
			/* Occasionally allow exactly 3*3 tables */
			if (((x2 - x1) == 2) && ((y2 - y1) == 2) && (rand_int(100) < 25)) break;

			if (((x2 - x1) > 1) && ((y2 - y1) > 1))
			{
				if (rand_int(100) < 40)
				{
					if (rand_int(100) < 50) y1++;
					else y2--;
				}
				else
				{
					if (rand_int(100) < 50) x1++;
					else x2--;
				}
			}
		}

		for (y = y1; y <= y2; y++)
		{
			for (x = x1; x <= x2; x++)
			{
				if (cave_clean_bold(y, x))
				{
					place_decoration(y, x, WG_TABLE);
				}
			}
		}

		/* Add 10 to theme. This means that there will be no animals around the table */
		dun->monster[dun->cent_n] += 10;
	}

	return(TRUE);
}

/*
 * Generate helper -- fill a rectangle with a shelf
 */
int generate_fill_shelf(int y1, int x1, int y2, int x2, int how_many)
{
	int y, x;
	int occurrences = 0;
	int max_occu = randint(3) + randint(3) + 3;
	int feature = 0;
	int special_wall_type;
	bool placed = FALSE;

	if (rand_int(100) < 50) y1--;
	else y2++;

	if (rand_int(100) < 50) x1--;
	else x2++;

	/* First count the occurrences */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			if (!cave_floor_bold(y, x))
			{
				occurrences++;

				/* If close to another special wall, generate no shelf */
				if (decoration(y, x)) return (0);
				if (decoration(y +1, x)) return (0);
				if (decoration(y, x +1)) return (0);
				if (decoration(y -1, x)) return (0);
				if (decoration(y, x -1)) return (0);
			}
			else break;
		}
	}

	/* Room style usually determines the special wall type */
	if (rand_int(6) < 1) special_wall_type = 2 + randint(4);
	else special_wall_type = dun->style[dun->cent_n];

	switch (special_wall_type)
	{
		case 3: feature = WG_CLOSET; break;
		case 4: feature = WG_RACK; break;
		case 5: feature = WG_SHELF; break;
		case 6: feature = WG_PAINTING; break;
	}

	if (occurrences > 2)
	{
		occurrences = 0;

		if (rand_int(100) < 50)
		{
			for (y = y1; y <= y2; y++)
			{
				for (x = x1; x <= x2; x++)
				{
					if ((!cave_floor_bold(y, x)) && (occurrences < max_occu))
					{
						occurrences++;
						if (next_to_floor(y,x) > 0) 
						{
							if (cave_feat[y][x] == FEAT_WALL_OUTER)
							{
								place_decoration(y, x, feature);
								placed = TRUE;

								if ((feature == WG_PAINTING) && (rand_int(5) < 1))
								{
									place_decoration(y, x, WG_WARD_SLUMBER_ACTIVE_HIDDEN + rand_int(10));
								}
							}
						}
					}
					else break;
				}
			}
		}
		else
		{
			for (y = y2; y >= y1; y--)
			{
				for (x = x2; x >= x1; x--)
				{
					if ((!cave_floor_bold(y, x)) && (occurrences < max_occu))
					{
						occurrences++;
						if (next_to_floor(y,x) > 0) 
						{
							if (cave_feat[y][x] == FEAT_WALL_OUTER)
							{
								place_decoration(y, x, feature);
								placed = TRUE;

								if ((feature == WG_PAINTING) && (rand_int(5) < 1))
								{
									place_decoration(y, x, WG_WARD_SLUMBER_ACTIVE_HIDDEN + rand_int(10));
								}
							}
						}
					}
					else break;
				}
			}
		}
	}

	/* If we placed any kind of shelf, lower the level's object count */
	if ((placed == TRUE) && (special_wall_type < 6))
	{
			if (obj_gen > 0) obj_gen--;
	}

	if (placed == TRUE) return (special_wall_type);
	else return (0);
}

/*
 * Generate helper -- draw a rectangle with a feature
 */
static void generate_outer_walls_rect(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;

	for (y = y1+1; y <= y2-1; y++)
	{
		cave_set_feat(y, x1, feat);
		cave_set_feat(y, x2, feat);
	}

	for (x = x1; x <= x2; x++)
	{
		if (cave_feat[y1-1][x] == FEAT_FLOOR)
		{
		}
		else if (cave_feat[y1][x] == FEAT_FLOOR)
		{
		}
		else cave_set_feat(y1, x, feat);

		if (cave_feat[y2+1][x] == FEAT_FLOOR)
		{
		}
		else if (cave_feat[y2][x] == FEAT_FLOOR)
		{
		}
		else cave_set_feat(y2, x, feat);
	}
}

/*
 * Generate helper -- remove joined walls
 */
static void remove_joined_walls_rect(int y1, int x1, int y2, int x2, int feat, int light)
{
	int x;
	int yyy, xxx;

	for (x = x1; x <= x2; x++)
	{
		if (cave_feat[y1-1][x] == FEAT_WALL_INNER)
		{
			cave_set_feat(y1, x, FEAT_FLOOR);
			cave_set_feat(y1-1, x, FEAT_FLOOR);

			/* Part of a room */
			cave_info[y1][x] |= (CAVE_ROOM);
			cave_info[y1-1][x] |= (CAVE_ROOM);

			if (light)
			{
				/* Light the area */
				for (yyy = y1-2; yyy < y1+2; yyy++)
				{
					for (xxx = x-1; xxx < x+2; xxx++)
					{
						cave_info[yyy][xxx] |= (CAVE_GLOW);
					}
				}
			}
		}

		if (cave_feat[y2+1][x] == FEAT_WALL_INNER)
		{
			cave_set_feat(y2, x, FEAT_FLOOR);
			cave_set_feat(y2+1, x, FEAT_FLOOR);

			/* Part of a room */
			cave_info[y2][x] |= (CAVE_ROOM);
			cave_info[y2+1][x] |= (CAVE_ROOM);

			if (light)
			{
				/* Light the area */
				for (yyy = y2-2; yyy < y2+3; yyy++)
				{
					for (xxx = x-1; xxx < x+2; xxx++)
					{
						cave_info[yyy][xxx] |= (CAVE_GLOW);
					}
				}
			}
		}
	}
}

/*
 * Generate helper -- join rooms if their walls are next to each other
 */
static void join_rooms_rect(int y1, int x1, int y2, int x2)
{
	int x;
	int joined_room = 0;

	for (x = x1; x <= x2; x++)
	{
		if (next_to_outer_walls(y1, x))
		{
			joined_room = room_idx_ignore_valid(y1-1, x);
		}

		if (next_to_outer_walls(y2, x))
		{
			joined_room = room_idx_ignore_valid(y2+1, x);
		}
	}

	if (joined_room)
	{
		dun->joined[dun->cent_n] = joined_room;
		dun->joined[joined_room] = dun->cent_n;

		dun->part[dun->cent_n] = dun->part[joined_room];
	}
}

/*
 * Helper function for generating a Chaos monster
 */
static bool vault_aux_chaos(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Usually drop all but Chaos */
	if (rand_int(100) < 88)
	{
		if (!(r_ptr->flags4 & (RF4_CHAOS))) return (FALSE);
	}

	/* Drop bigger groups half of the time */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 50)
		{
			if (r_ptr->flags4 & (RF1_GRP_27)) return (FALSE);
			if (r_ptr->flags4 & (RF1_GRP_18)) return (FALSE);
		}
	}

	/* Drop smaller groups sometimes */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 25)
		{
			if (r_ptr->flags4 & (RF1_GRP_9)) return (FALSE);
		}
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for generating an Aether  monster
 */
static bool vault_aux_aether(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Usually drop all but Aether */
	if (rand_int(100) < 88)
	{
		if (!(r_ptr->flags4 & (RF4_AETHER))) return (FALSE);
	}

	/* Drop bigger groups half of the time */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 50)
		{
			if (r_ptr->flags4 & (RF1_GRP_27)) return (FALSE);
			if (r_ptr->flags4 & (RF1_GRP_18)) return (FALSE);
		}
	}

	/* Drop smaller groups sometimes */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 25)
		{
			if (r_ptr->flags4 & (RF1_GRP_9)) return (FALSE);
		}
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for generating a Thornwild monster
 */
static bool vault_aux_thornwild(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Require Thornwild */
	if (rand_int(100) < 88)
	{
		if (!(r_ptr->flags4 & (RF4_THORNWILD))) return (FALSE);
	}

	/* Drop bigger groups half of the time */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 50)
		{
			if (r_ptr->flags4 & (RF1_GRP_27)) return (FALSE);
			if (r_ptr->flags4 & (RF1_GRP_18)) return (FALSE);
		}
	}

	/* Drop smaller groups sometimes */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 25)
		{
			if (r_ptr->flags4 & (RF1_GRP_9)) return (FALSE);
		}
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for generating a Thornwild monster (non-animal)
 */
static bool vault_aux_thornwild_no_animal(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (rand_int(100) < 88)
	{
		/* Require Thornwild */
		if (!(r_ptr->flags4 & (RF4_THORNWILD))) return (FALSE);

		/* Require non-animal */
		if (r_ptr->flags4 & (RF4_ANIMAL)) return (FALSE);

		/* Require non-plant */
		if (r_ptr->flags4 & (RF4_PLANT)) return (FALSE);
	}

	/* Drop bigger groups half of the time */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 50)
		{
			if (r_ptr->flags4 & (RF1_GRP_27)) return (FALSE);
			if (r_ptr->flags4 & (RF1_GRP_18)) return (FALSE);
		}
	}

	/* Drop smaller groups sometimes */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 25)
		{
			if (r_ptr->flags4 & (RF1_GRP_9)) return (FALSE);
		}
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for generating a Skultgard monster
 */
static bool vault_aux_skultgard(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (rand_int(100) < 88)
	{
		/* Require Thornwild */
		if (!(r_ptr->flags4 & (RF4_SKULTGARD))) return (FALSE);
	}

	/* Drop bigger groups half of the time */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 50)
		{
			if (r_ptr->flags4 & (RF1_GRP_27)) return (FALSE);
			if (r_ptr->flags4 & (RF1_GRP_18)) return (FALSE);
		}
	}

	/* Drop smaller groups sometimes */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 25)
		{
			if (r_ptr->flags4 & (RF1_GRP_9)) return (FALSE);
		}
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for generating a Skultgard monster (non-animal)
 */
static bool vault_aux_skultgard_no_animal(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	if (rand_int(100) < 88)
	{
		/* Require Thornwild */
		if (!(r_ptr->flags4 & (RF4_SKULTGARD))) return (FALSE);

		/* Require non-animal */
		if (r_ptr->flags4 & (RF4_ANIMAL)) return (FALSE);

		/* Require non-plant */
		if (r_ptr->flags4 & (RF4_PLANT)) return (FALSE);
	}

	/* Drop bigger groups half of the time */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 50)
		{
			if (r_ptr->flags4 & (RF1_GRP_27)) return (FALSE);
			if (r_ptr->flags4 & (RF1_GRP_18)) return (FALSE);
		}
	}

	/* Drop smaller groups sometimes */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (rand_int(100) < 25)
		{
			if (r_ptr->flags4 & (RF1_GRP_9)) return (FALSE);
		}
	}

	/* Okay */
	return (TRUE);
}

/*
 * Build a room consisting of two overlapping rooms.
 * Get the room description, and place stuff accordingly.
 */
static bool build_overlapping(int type, int y1a, int x1a, int y2a, int x2a,
	int y1b, int x1b, int y2b, int x2b, bool light, int spacing, bool pillars)
{
	int j = 0;
	int location = 0;
	int outer = 1;

	int y1n = 0;
	int y2n = 0;
	int x1n = 0;
	int x2n = 0;

	int y1s = 0;
	int y2s = 0;
	int x1s = 0;
	int x2s = 0;

	int y1e = 0;
	int y2e = 0;
	int x1e = 0;
	int x2e = 0;

	int y1w = 0;
	int y2w = 0;
	int x1w = 0;
	int x2w = 0;

	int y1c = 0;
	int y2c = 0;
	int x1c = 0;
	int x2c = 0;

	int extra_monsters = 0;

	/* Make certain the overlapping room does not cross the dungeon edge. */
	if ((!in_bounds_fully(y1a, x1a)) || (!in_bounds_fully(y1b, x1b))
		 || (!in_bounds_fully(y2a, x2a)) || (!in_bounds_fully(y2b, x2b))) return (FALSE);

	/* Check for two rooms joining into one (a) */
	join_rooms_rect(y1a, x1a, y2a, x2a);

	/* Check for two rooms joining into one (b) */
	join_rooms_rect(y1b, x1b, y2b, x2b);

	/* Remove joined walls (a) */
	remove_joined_walls_rect(y1a, x1a, y2a, x2a, FEAT_WALL_OUTER, light);

	/* Remove joined walls (b) */
	remove_joined_walls_rect(y1b, x1b, y2b, x2b, FEAT_WALL_OUTER, light);

	/* Generate outer walls (a) */
	generate_outer_walls_rect(y1a, x1a, y2a, x2a, FEAT_WALL_OUTER);

	/* Generate outer walls (b) */
	generate_outer_walls_rect(y1b, x1b, y2b, x2b, FEAT_WALL_OUTER);

	/* Generate new room (a) */
	generate_room(y1a, x1a, y2a, x2a, light);

	/* Generate new room (b) */
	generate_room(y1b, x1b, y2b, x2b, light);

	/* Make corners solid
	cave_set_feat(y1a, x1a, FEAT_WALL_SOLID);
	cave_set_feat(y2a, x2a, FEAT_WALL_SOLID);
	cave_set_feat(y1a, x2a, FEAT_WALL_SOLID);
	cave_set_feat(y2a, x1a, FEAT_WALL_SOLID);
	cave_set_feat(y1b, x1b, FEAT_WALL_SOLID);
	cave_set_feat(y2b, x2b, FEAT_WALL_SOLID);
	cave_set_feat(y1b, x2b, FEAT_WALL_SOLID);
	cave_set_feat(y2b, x1b, FEAT_WALL_SOLID); */

	/* Generate inner floors (a) */
	generate_fill(y1a+1, x1a+1, y2a-1, x2a-1, FEAT_FLOOR);

	/* Generate inner floors (b) */
	generate_fill(y1b+1, x1b+1, y2b-1, x2b-1, FEAT_FLOOR);

	/* North */
	y1n = MIN(y1a, y1b) + outer;
	y2n = (y1a == y1b ? y1a + 1 : MAX(y1a, y1b) - 1);
	x1n = (y1a < y1b ? x1a : (y1a == y1b ? MIN(x1a, x1b): x1b)) + outer;
	x2n = (y1a < y1b ? x2a : (y1a == y1b ? MAX(x2a, x2b): x2b)) - outer;
	if (y2n <= y1n) y2n = y1n + 1;

	/* South */
	y1s = (y2a == y2b ? y2a - 1 : MIN(y2a, y2b) + 1);
	y2s = MAX(y2a, y2b) - outer;
	x1s = (y2a > y2b ? x1a : (y2a == y2b ? MIN(x1a, x1b): x1b)) + outer;
	x2s = (y2a > y2b ? x2a : (y2a == y2b ? MAX(x2a, x2b): x2b)) - outer;
	if (y1s >= y2s - 1) y1s = y2s - 1;

	/* West */
	y1w = (x1a < x1b ? y1a : (x1a == x1b ? MIN(y1a, y1b) : y1b)) + outer;
	y2w = (x1a < x1b ? y2a : (x1a == x1b ? MAX(y2a, y2b) : y2b)) - outer;
	x1w = MIN(x1a, x1b) + outer;
	x2w = (x1a == x1b ? x1a + 1 : MAX(x1a, x1b) - 1);
	if (x2w <= x1w +2) x2w = x1w + 2;

	/* East */
	y1e = (x2a > x2b ? y1a : (x1a == x1b ? MIN(y1a, y1b): y1b)) + outer;
	y2e = (x2a > x2b ? y2a : (x1a == x1b ? MAX(y2a, y2b): y2b)) - outer;
	x1e = (x2a == x2b ? x2a - 1 : MIN(x2a, x2b) + 1) + outer;
	x2e = MAX(x2a, x2b) - outer;
	if (x1e >= x2e - 2) x1e = x2e - 2;

	/* Center */
	y1c = MAX(y1a, y1b) + 2;
	y2c = MIN(y2a, y2b) - 2;
	x1c = MAX(x1a, x1b) + 2;
	x2c = MIN(x2a, x2b) - 2;
	if (y1c >= y2c -2) { y1c = y2c - 2; y2c = y1c + 3;}
	if (x1c >= x2c -2) { x1c = x2c - 2; x2c = x1c + 3;}

	/*
 	* Get the room description, and place stuff accordingly.
 	*/
	int chart;

	int y = y1a;
	int x = x1a;

	int room = dun->cent_n+1;
	bool fountain = FALSE;

	/* Initialise chart */
	chart = 1;
	j = 0;

	/* Choose room type */
	switch (dun->style[dun->cent_n])
	{
		case 1:
		{
			/* Generate a wilderness room */
			location = randint(6);
			if (location <= 2) location = randint(4);

			if (rand_int(100) < 5) fountain = TRUE;

			switch (location)
			{
				case 1:
				{
					if (rand_int(100) < 85)
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						generate_fill_tree(y1n, x1n, y2n, x2n, fountain, FALSE);
						extra_monsters += 60;
					}
					break;
				}
				case 2:
				{
					if (rand_int(100) < 85)
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						generate_fill_tree(y1s, x1s, y2s, x2s, fountain, FALSE);
						extra_monsters += 60;
					}
					break;
				}
				case 3:
				{
					if (rand_int(100) < 85)
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						generate_fill_tree(y1e, x1e, y2e, x2e, fountain, FALSE);
						extra_monsters += 60;
					}
					break;
				}
				case 4:
				{
					if (rand_int(100) < 85)
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						generate_fill_tree(y1w, x1w, y2w, x2w, fountain, FALSE);
						extra_monsters += 60;
					}
					break;
				}
				default:
				{
					if (rand_int(100) < 85)
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						extra_monsters += 100;
						generate_fill_tree(y1c, x1c, y2c, x2c, fountain, TRUE);
					}
					break;
				}
			}

			break;
		}

		default:
		{
			/* Generate a civilized room */

			int special_wall = 0;
			int special_wall_chance = 3;

			/* More special walls deeper in the dungeon. This caps at DL 30, similarly to mon_gen maximum */
			if (rand_int(30) < p_ptr->depth) special_wall_chance ++;
			if (rand_int(30) < p_ptr->depth) special_wall_chance ++;

			/* Cave paintings are more common */
			if (dun->style[dun->cent_n] == 3) special_wall_chance ++;

			/* Generate a special wall */
			if ((dun->style[dun->cent_n] > 2) && (rand_int(18) < special_wall_chance))
			{
				location = randint(6);

				switch (location)
				{
					case 1:
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						special_wall = generate_fill_shelf(y1n, x1n, y2n, x2n, 1);
						if (special_wall > 0) extra_monsters += 50;

						if (special_wall == 3)
						{
							/* Generate traps (a) */
							generate_fill_traps(y1a+1, x1a+1, y2a-1, x2a-1);

							/* Generate traps (b) */
							generate_fill_traps(y1b+1, x1b+1, y2b-1, x2b-1);
						}

						break;
					}
					case 2:
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						special_wall = generate_fill_shelf(y1s, x1s, y2s, x2s, 1);
						if (special_wall > 0) extra_monsters += 50;

						if (special_wall == 3)
						{
							/* Generate traps (a) */
							generate_fill_traps(y1a+1, x1a+1, y2a-1, x2a-1);

							/* Generate traps (b) */
							generate_fill_traps(y1b+1, x1b+1, y2b-1, x2b-1);
						}

						break;
					}
					case 3:
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						special_wall = generate_fill_shelf(y1e, x1e, y2e, x2e, 1);
						if (special_wall > 0) extra_monsters += 50;

						if (special_wall == 3)
						{
							/* Generate traps (a) */
							generate_fill_traps(y1a+1, x1a+1, y2a-1, x2a-1);

							/* Generate traps (b) */
							generate_fill_traps(y1b+1, x1b+1, y2b-1, x2b-1);
						}

						break;
					}
					case 4:
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						special_wall = generate_fill_shelf(y1w, x1w, y2w, x2w, 1);
						if (special_wall > 0) extra_monsters += 50;

						if (special_wall == 3)
						{
							/* Generate traps (a) */
							generate_fill_traps(y1a+1, x1a+1, y2a-1, x2a-1);

							/* Generate traps (b) */
							generate_fill_traps(y1b+1, x1b+1, y2b-1, x2b-1);
						}

						break;
					}
					case 5:
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						special_wall = generate_fill_shelf(y1w, x1w, y2w, x2w, 2);
						if (special_wall > 0) extra_monsters += 50;

						if (special_wall == 3)
						{
							/* Generate traps (a) */
							generate_fill_traps(y1a+1, x1a+1, y2a-1, x2a-1);

							/* Generate traps (b) */
							generate_fill_traps(y1b+1, x1b+1, y2b-1, x2b-1);
						}

						special_wall = generate_fill_shelf(y1e, x1e, y2e, x2e, 2);
						if (special_wall > 0) extra_monsters += 50;

						if (special_wall == 3)
						{
							/* Generate traps (a) */
							generate_fill_traps(y1a+1, x1a+1, y2a-1, x2a-1);

							/* Generate traps (b) */
							generate_fill_traps(y1b+1, x1b+1, y2b-1, x2b-1);
						}

						break;
					}
					case 6:
					{
						/* The monster theme foor the room = room style */
						dun->monster[dun->cent_n] = dun->style[dun->cent_n];

						special_wall = generate_fill_shelf(y1n, x1n, y2n, x2n, 2);
						if (special_wall > 0) extra_monsters += 50;

						if (special_wall == 3)
						{
							/* Generate traps (a) */
							generate_fill_traps(y1a+1, x1a+1, y2a-1, x2a-1);

							/* Generate traps (b) */
							generate_fill_traps(y1b+1, x1b+1, y2b-1, x2b-1);
						}

						special_wall = generate_fill_shelf(y1s, x1s, y2s, x2s, 2);
						if (special_wall > 0) extra_monsters += 50;

						if (special_wall == 3)
						{
							/* Generate traps (a) */
							generate_fill_traps(y1a+1, x1a+1, y2a-1, x2a-1);

							/* Generate traps (b) */
							generate_fill_traps(y1b+1, x1b+1, y2b-1, x2b-1);
						}

						break;
					}
				}

				/* Fill room corners with the same decoration */
				if ((next_to_feature(y1a, x1a, WG_SHELF) > 1) && (cave_feat[y1a][x1a] == FEAT_WALL_OUTER)) place_decoration(y1a, x1a, WG_SHELF);
				if ((next_to_feature(y2a, x2a, WG_SHELF) > 1) && (cave_feat[y2a][x2a] == FEAT_WALL_OUTER)) place_decoration(y2a, x2a, WG_SHELF);
				if ((next_to_feature(y1a, x2a, WG_SHELF) > 1) && (cave_feat[y1a][x2a] == FEAT_WALL_OUTER)) place_decoration(y1a, x2a, WG_SHELF);
				if ((next_to_feature(y2a, x1a, WG_SHELF) > 1) && (cave_feat[y2a][x1a] == FEAT_WALL_OUTER)) place_decoration(y2a, x1a, WG_SHELF);
				if ((next_to_feature(y1b, x1b, WG_SHELF) > 1) && (cave_feat[y1b][x1b] == FEAT_WALL_OUTER)) place_decoration(y1b, x1b, WG_SHELF);
				if ((next_to_feature(y2b, x2b, WG_SHELF) > 1) && (cave_feat[y2b][x2b] == FEAT_WALL_OUTER)) place_decoration(y2b, x2b, WG_SHELF);
				if ((next_to_feature(y1b, x2b, WG_SHELF) > 1) && (cave_feat[y1b][x2b] == FEAT_WALL_OUTER)) place_decoration(y1b, x2b, WG_SHELF);
				if ((next_to_feature(y2b, x1b, WG_SHELF) > 1) && (cave_feat[y2b][x1b] == FEAT_WALL_OUTER)) place_decoration(y2b, x1b, WG_SHELF);

				if ((next_to_feature(y1a, x1a, WG_PAINTING) > 1) && (cave_feat[y1a][x1a] == FEAT_WALL_OUTER)) place_decoration(y1a, x1a, WG_PAINTING);
				if ((next_to_feature(y2a, x2a, WG_PAINTING) > 1) && (cave_feat[y2a][x2a] == FEAT_WALL_OUTER)) place_decoration(y2a, x2a, WG_PAINTING);
				if ((next_to_feature(y1a, x2a, WG_PAINTING) > 1) && (cave_feat[y1a][x2a] == FEAT_WALL_OUTER)) place_decoration(y1a, x2a, WG_PAINTING);
				if ((next_to_feature(y2a, x1a, WG_PAINTING) > 1) && (cave_feat[y2a][x1a] == FEAT_WALL_OUTER)) place_decoration(y2a, x1a, WG_PAINTING);
				if ((next_to_feature(y1b, x1b, WG_PAINTING) > 1) && (cave_feat[y1b][x1b] == FEAT_WALL_OUTER)) place_decoration(y1b, x1b, WG_PAINTING);
				if ((next_to_feature(y2b, x2b, WG_PAINTING) > 1) && (cave_feat[y2b][x2b] == FEAT_WALL_OUTER)) place_decoration(y2b, x2b, WG_PAINTING);
				if ((next_to_feature(y1b, x2b, WG_PAINTING) > 1) && (cave_feat[y1b][x2b] == FEAT_WALL_OUTER)) place_decoration(y1b, x2b, WG_PAINTING);
				if ((next_to_feature(y2b, x1b, WG_PAINTING) > 1) && (cave_feat[y2b][x1b] == FEAT_WALL_OUTER)) place_decoration(y2b, x1b, WG_PAINTING);

				if ((next_to_feature(y1a, x1a, WG_RACK) > 1) && (cave_feat[y1a][x1a] == FEAT_WALL_OUTER)) place_decoration(y1a, x1a, WG_RACK);
				if ((next_to_feature(y2a, x2a, WG_RACK) > 1) && (cave_feat[y2a][x2a] == FEAT_WALL_OUTER)) place_decoration(y2a, x2a, WG_RACK);
				if ((next_to_feature(y1a, x2a, WG_RACK) > 1) && (cave_feat[y1a][x2a] == FEAT_WALL_OUTER)) place_decoration(y1a, x2a, WG_RACK);
				if ((next_to_feature(y2a, x1a, WG_RACK) > 1) && (cave_feat[y2a][x1a] == FEAT_WALL_OUTER)) place_decoration(y2a, x1a, WG_RACK);
				if ((next_to_feature(y1b, x1b, WG_RACK) > 1) && (cave_feat[y1b][x1b] == FEAT_WALL_OUTER)) place_decoration(y1b, x1b, WG_RACK);
				if ((next_to_feature(y2b, x2b, WG_RACK) > 1) && (cave_feat[y2b][x2b] == FEAT_WALL_OUTER)) place_decoration(y2b, x2b, WG_RACK);
				if ((next_to_feature(y1b, x2b, WG_RACK) > 1) && (cave_feat[y1b][x2b] == FEAT_WALL_OUTER)) place_decoration(y1b, x2b, WG_RACK);
				if ((next_to_feature(y2b, x1b, WG_RACK) > 1) && (cave_feat[y2b][x1b] == FEAT_WALL_OUTER)) place_decoration(y2b, x1b, WG_RACK);

				if ((next_to_feature(y1a, x1a, WG_CLOSET) > 1) && (cave_feat[y1a][x1a] == FEAT_WALL_OUTER)) place_decoration(y1a, x1a, WG_CLOSET);
				if ((next_to_feature(y2a, x2a, WG_CLOSET) > 1) && (cave_feat[y2a][x2a] == FEAT_WALL_OUTER)) place_decoration(y2a, x2a, WG_CLOSET);
				if ((next_to_feature(y1a, x2a, WG_CLOSET) > 1) && (cave_feat[y1a][x2a] == FEAT_WALL_OUTER)) place_decoration(y1a, x2a, WG_CLOSET);
				if ((next_to_feature(y2a, x1a, WG_CLOSET) > 1) && (cave_feat[y2a][x1a] == FEAT_WALL_OUTER)) place_decoration(y2a, x1a, WG_CLOSET);
				if ((next_to_feature(y1b, x1b, WG_CLOSET) > 1) && (cave_feat[y1b][x1b] == FEAT_WALL_OUTER)) place_decoration(y1b, x1b, WG_CLOSET);
				if ((next_to_feature(y2b, x2b, WG_CLOSET) > 1) && (cave_feat[y2b][x2b] == FEAT_WALL_OUTER)) place_decoration(y2b, x2b, WG_CLOSET);
				if ((next_to_feature(y1b, x2b, WG_CLOSET) > 1) && (cave_feat[y1b][x2b] == FEAT_WALL_OUTER)) place_decoration(y1b, x2b, WG_CLOSET);
				if ((next_to_feature(y2b, x1b, WG_CLOSET) > 1) && (cave_feat[y2b][x1b] == FEAT_WALL_OUTER)) place_decoration(y2b, x1b, WG_CLOSET);

				/* Handle a special case: two corners next to each other. */
				/* A special wall needs to have at least one floor square in a room next to it. */
				if (next_to_room_floor(y1a, x1a) < 1) delete_trap(y1a, x1a);
				if (next_to_room_floor(y2a, x2a) < 1) delete_trap(y2a, x2a);
				if (next_to_room_floor(y1a, x2a) < 1) delete_trap(y1a, x2a);
				if (next_to_room_floor(y2a, x1a) < 1) delete_trap(y2a, x1a);
				if (next_to_room_floor(y1b, x1b) < 1) delete_trap(y1b, x1b);
				if (next_to_room_floor(y2b, x2b) < 1) delete_trap(y2b, x2b);
				if (next_to_room_floor(y1b, x2b) < 1) delete_trap(y1b, x2b);
				if (next_to_room_floor(y2b, x1b) < 1) delete_trap(y2b, x1b);
			}

			/* Generate a table */
			if (dun->style[dun->cent_n] < 3)
			{
				if (rand_int(11) < 2)
				{
					/* The monster theme foor the room = room style */
					dun->monster[dun->cent_n] = dun->style[dun->cent_n];

					if (generate_fill_table(y1c, x1c, y2c, x2c)) extra_monsters += 100;
				}
			}
			else
			{
				if (rand_int(5) < 2)
				{
					/* The monster theme foor the room = room style */
					dun->monster[dun->cent_n] = dun->style[dun->cent_n];

					if (generate_fill_table(y1c, x1c, y2c, x2c)) extra_monsters += 100;
				}
			}
		}

		break;
	}

	bool no_animal = FALSE;
	bool generate_monster = TRUE;
	int monster_theme = 0;
	if (dun->monster[dun->cent_n] > 9)
	{
		no_animal = TRUE;
		monster_theme = dun->monster[dun->cent_n] - 10;
	}
	else monster_theme = dun->monster[dun->cent_n];

	/* Place extra monsters to rooms with "interesting" features */
	if (rand_int(100) < extra_monsters)
	{

		/* Choose the monster hook according to the monster theme */
		switch (monster_theme)
		{
			case 1:
			case 2:
			case 6:
			{
				/* Wilderness */
				if (no_animal) get_mon_num_hook = vault_aux_thornwild_no_animal;
				else get_mon_num_hook = vault_aux_thornwild;
				break;
			}
			case 3:
			case 4:
			case 5:
			{
				/* Room with racks */
				if (no_animal) get_mon_num_hook = vault_aux_skultgard_no_animal;
				else get_mon_num_hook = vault_aux_skultgard;
				break;
			}
			default:
			{
				/* No theme */
				get_mon_num_hook = NULL;
				generate_monster = FALSE;
				break;
			}
		}

		/* Prepare allocation table */
		get_mon_num_prep();

		/* Place several monsters */
		if (generate_monster) vault_monsters(y, x, randint(3));

		/* Reset hook and prepare allocation table */
		get_mon_num_hook = NULL;
		get_mon_num_prep();

		/* Lower the level's monster count */
		if (mon_gen) mon_gen--;
	}

	/* Type */
	room_info[room].type = ROOM_NORMAL;

	/* Terminate index list */
	room_info[room].section[j] = -1;

	room_info[room].seen = FALSE;

	return(TRUE);
}

/*
 * Type 1, 2 & 3 room.
 */
static bool build_type123(int y0, int x0)
{
	int type = randint(3);
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;
	/* int height, width; */
	int symetry = rand_int(100);

	bool light = FALSE;
	int spacing = 1;
	bool pillars = 0;

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
	if (symetry < 20)
	{
		x1a = x2a; x2b = x1b;
	}

	/* Sometimes express symetry */
	if ((symetry < 10) || (symetry > 90))
	{
		y1a = y2a; y2b = y1b;
	}

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
	if (!build_overlapping(type, y1a, x1a, y2a, x2a, y1b, x1b, y2b, x2b, light, spacing, pillars)) return (FALSE);

	return (TRUE);
}

/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a monster nest or monster pit or
 * the given type.
 *
 * None of the pits/nests are allowed to include "unique" monsters.
 *
 * The old method used monster "names", which was bad, but the new
 * method uses monster race characters, which is also bad.  XXX XXX XXX
 */

/*
 * Helper function for "monster nest (rodent)"
 */
static bool vault_aux_rodent(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require rodent */
	if (!strchr("r", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

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
 * Helper function for "monster nest (jelly)"
 */
static bool vault_aux_vortex(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require icky thing, jelly, mold, or mushroom */
	if (!strchr("v", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (rodent)"
 */
static bool vault_aux_treasure(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require treasure */
	if (!strchr("$", r_ptr->d_char)) return (FALSE);

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
	if (!(r_ptr->flags4 & (RF4_ANIMAL))) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (horror)"
 */
static bool vault_aux_horror(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "N" monsters */
	if (!strchr("N", r_ptr->d_char)) return (FALSE);

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
	if (!(r_ptr->flags4 & (RF4_UNDEAD))) return (FALSE);

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
 * Helper function for "monster pit (person)"
 */
static bool vault_aux_person(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require People */
	if (!(r_ptr->flags4 & (RF4_PERSON))) return (FALSE);

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

#define DRAGON_PIT_TYPES	8

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
	if (r_ptr->s_flags1 != vault_aux_dragon_mask4) return (FALSE);

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
 *   a nest of "rodent" monsters  (Dungeon level 5 and deeper)
 *   a nest of "jelly" monsters   (Dungeon level 10 and deeper)
 *   a nest of "treasure" monsters(Dungeon level 15 and deeper)
 *   a nest of "vortex" monsters  (Dungeon level 25 and deeper)
 *   a nest of "animal" monsters  (Dungeon level 30 and deeper)
 *   a nest of "horror" monsters  (Dungeon level 45 and deeper)
 *   a nest of "undead" monsters  (Dungeon level 65 and deeper)
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

	int rating_bonus;

	s16b what[64];

	cptr name;

	bool empty = FALSE;

	int light = FALSE;

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
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_CLOSED);

	/* Hack -- Choose a nest type */
	tmp = randint(p_ptr->depth);

	/* Monster nest (rodent) */
	if (tmp < 10)
	{
		/* Describe */
		name = "rodent";

		/* Restrict to rodent */
		get_mon_num_hook = vault_aux_rodent;

		/* Appropriate rating bonus */
		rating_bonus = 15 - p_ptr->depth;

		/* Room info */
		room_info[dun->cent_n+1].type = ROOM_NEST_RODENT;
	}

	/* Monster nest (jelly) */
	else if (tmp < 15)
	{
		/* Describe */
		name = "jelly";

		/* Restrict to jelly */
		get_mon_num_hook = vault_aux_jelly;

		/* Appropriate rating bonus */
		rating_bonus = 25 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_NEST_JELLY;
	}

	/* Monster nest (treasure) */
	else if (tmp < 25)
	{
		/* Describe */
		name = "treasure";

		/* Restrict to jelly */
		get_mon_num_hook = vault_aux_treasure;

		/* Appropriate rating bonus */
		rating_bonus = 25 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_NEST_TREASURE;
	}

	/* Monster nest (vortex) */
	else if (tmp < 30)
	{
		/* Describe */
		name = "vortex";

		/* Restrict to jelly */
		get_mon_num_hook = vault_aux_vortex;

		/* Appropriate rating bonus */
		rating_bonus = 30 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_NEST_VORTEX;
	}

	/* Monster nest (animal) */
	else if (tmp < 45)
	{
		/* Describe */
		name = "animal";

		/* Restrict to animal */
		get_mon_num_hook = vault_aux_animal;

		/* Appropriate rating bonus */
		rating_bonus = 45 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_NEST_ANIMAL;
	}

	/* Monster nest (horror) */
	else if (tmp < 65)
	{
		/* Describe */
		name = "horror";

		/* Restrict to animal */
		get_mon_num_hook = vault_aux_horror;

		/* Appropriate rating bonus */
		rating_bonus = 60 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_NEST_HORROR;
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

	/* Initialize room description */
	room_info[dun->cent_n+1].seen = FALSE;

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

	/* Message for cheaters*/
	if (cheat_room)	message_format(MSG_CHEAT, 0, "Monster nest (%s)", name);

	/* Increase the level rating */
	if (rating_bonus > 0) rating += rating_bonus;

	/* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
	if ((p_ptr->depth <= 40) &&
	    (randint(p_ptr->depth * p_ptr->depth + 100) < 100))
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
			(void)place_monster_aux(y, x, r_idx, 0, TRUE, FALSE, PLACE_NO_UNIQUE);
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
 *   people pit (Dungeon Level 35 and deeper)
 *   giant pit	(Dungeon Level 50 and deeper)
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
	int tmp, what[16];

	int i, j, y, x, y1, x1, y2, x2;

	int rating_bonus;

	bool empty = FALSE;

	int light = FALSE;

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
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_INNER);

	/* Open the inner room with a secret door */
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_CLOSED);

	/* Choose a pit type */
	tmp = randint(p_ptr->depth);

	/* Orc pit */
	if (tmp < 20)
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
	else if (tmp < 35)
	{
		/* Message */
		name = "troll";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_troll;

		/* Appropriate rating bonus */
		rating_bonus = 35 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_PIT_TROLL;
	}

	/* Person pit */
	else if (tmp < 50)
	{
		/* Message */
		name = "person";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_person;

		/* Appropriate rating bonus */
		rating_bonus = 50 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_PIT_PERSON;
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
		/* Appropriate rating bonus */
		rating_bonus = 75 - p_ptr->depth;

		/* Pick dragon type */
		switch (rand_int(DRAGON_PIT_TYPES))
		{
			/* Black */
			case 0:
			{
				/* Message */
				name = "acid dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = SRF1_BR_ACID;

				/* Done */
				break;
			}

			/* Blue */
			case 1:
			{
				/* Message */
				name = "electric dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = SRF1_BR_ELEC;

				/* Done */
				break;
			}

			/* Red */
			case 2:
			{
				/* Message */
				name = "fire dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = SRF1_BR_FIRE;

				/* Done */
				break;
			}

			/* White */
			case 3:
			{
				/* Message */
				name = "cold dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = SRF1_BR_COLD;

				/* Done */
				break;
			}

			/* Green */
			case 4:
			{
				/* Message */
				name = "poison dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = SRF1_BR_POIS;

				/* Done */
				break;
			}

			/* Gold */
			case 5:
			{
				/* Message */
				name = "sound dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = SRF1_BR_SOUN;

				/* Done */
				break;
			}

			/* Silver */
			case 6:
			{
				/* Message */
				name = "shard dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = SRF1_BR_SHAR;

				/* Done */
				break;
			}

			/* Multi-hued */
			default:
			{
				/* Message */
				name = "multi-hued dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (SRF1_BR_ACID | SRF1_BR_ELEC |
				                          SRF1_BR_FIRE | SRF1_BR_COLD |
				                          SRF1_BR_POIS | SRF1_BR_SHAR |
										  SRF1_BR_SOUN);

				/* Done */
				break;
			}
		}

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_dragon;

		room_info[dun->cent_n+1].type = ROOM_PIT_DRAGON;
	}

	/* Demon pit */
	else
	{
		/* Message */
		name = "demon";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_demon;

		/* Appropriate rating bonus */
		rating_bonus = 95 - p_ptr->depth;

		room_info[dun->cent_n+1].type = ROOM_PIT_DEMON;
	}

	/* Initialize room description */
	room_info[dun->cent_n+1].seen = FALSE;

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

	/* Messages for cheaters */
	if (cheat_room) message_format(MSG_CHEAT, 0, "Monster pit (%s)", name);

	/* Increase the level rating */
	if (rating_bonus > 0)  rating += rating_bonus;

	/* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
	if ((p_ptr->depth <= 40) &&
	    (randint(p_ptr->depth * p_ptr->depth + 100) < 100))
	{
		good_item_flag = TRUE;
	}

	/* Top and bottom rows */
	for (x = x0 - 9; x <= x0 + 9; x++)
	{
		place_monster_aux(y0 - 2, x, what[0], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y0 + 2, x, what[0], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
	}

	/* Middle columns */
	for (y = y0 - 1; y <= y0 + 1; y++)
	{
		place_monster_aux(y, x0 - 9, what[0], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y, x0 + 9, what[0], 0, TRUE, FALSE, PLACE_NO_UNIQUE);

		place_monster_aux(y, x0 - 8, what[1], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y, x0 + 8, what[1], 0, TRUE, FALSE, PLACE_NO_UNIQUE);

		place_monster_aux(y, x0 - 7, what[1], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y, x0 + 7, what[1], 0, TRUE, FALSE, PLACE_NO_UNIQUE);

		place_monster_aux(y, x0 - 6, what[2], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y, x0 + 6, what[2], 0, TRUE, FALSE, PLACE_NO_UNIQUE);

		place_monster_aux(y, x0 - 5, what[2], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y, x0 + 5, what[2], 0, TRUE, FALSE, PLACE_NO_UNIQUE);

		place_monster_aux(y, x0 - 4, what[3], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y, x0 + 4, what[3], 0, TRUE, FALSE, PLACE_NO_UNIQUE);

		place_monster_aux(y, x0 - 3, what[3], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y, x0 + 3, what[3], 0, TRUE, FALSE, PLACE_NO_UNIQUE);

		place_monster_aux(y, x0 - 2, what[4], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y, x0 + 2, what[4], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
	}

	/* Above/Below the center monster */
	for (x = x0 - 1; x <= x0 + 1; x++)
	{
		place_monster_aux(y0 + 1, x, what[5], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
		place_monster_aux(y0 - 1, x, what[5], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
	}

	/* Next to the center monster */
	place_monster_aux(y0, x0 + 1, what[6], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
	place_monster_aux(y0, x0 - 1, what[6], 0, TRUE, FALSE, PLACE_NO_UNIQUE);

	/* Center monster */
	place_monster_aux(y0, x0, what[7], 0, TRUE, FALSE, PLACE_NO_UNIQUE);
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

				/* Secret doors */
				case '+':
				{
					place_closed_door(y, x);
					break;
				}

				/* Trap */
				case '^':
				{
					place_trap_dungeon(y, x);
					break;
				}

				/* Treasure/trap/chest */
				case '*':
				{
					int i = rand_int(100);

					if (i < 73)
					{
						place_object(y, x, FALSE, FALSE);
					}
					else if (i < 98)
					{
						place_trap_dungeon(y, x);
					}
					else 
					{
						place_chest(y, x);
					}
					break;
				}

				/* Monster */
				case '&':
				{
					monster_level = p_ptr->depth + 5;
					place_monster(y, x);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster */
				case '@':
				{
					monster_level = p_ptr->depth + 11;
					place_monster(y, x);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster, plus treasure */
				case '9':
				{
					monster_level = p_ptr->depth + 9;
					place_monster(y, x);
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
					place_monster(y, x);
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
						place_monster(y, x);
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

				/* Quest item */
				case 'Q':
				{
					place_quest_chest(y, x);
					break;
				}

				/* Chest */
				case '~':
				{
					place_chest(y, x);
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
	if (cheat_room) message_format(MSG_CHEAT, 0, "Lesser Vault (%s)", v_name + v_ptr->name);

	/* Initialize room description */
	room_info[dun->cent_n+1].type = ROOM_LESSER_VAULT;
	room_info[dun->cent_n+1].seen = FALSE;

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 100) < 150))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
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
	if (cheat_room) message_format(MSG_CHEAT, 0, "Greater Vault (%s)", v_name + v_ptr->name);

	/* Initialize room description */
	room_info[dun->cent_n+1].type = ROOM_GREATER_VAULT;
	room_info[dun->cent_n+1].seen = FALSE;

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 100) < 150))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
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

		/* Accept the first greater vault */
		if (v_ptr->typ == 9) break;
	}

	/* Message */
	if (cheat_room) message_format(MSG_CHEAT, 0, "Quest Vault (%s)", v_name + v_ptr->name);

	/* Initialize room description */
	room_info[dun->cent_n+1].type = ROOM_QUEST_VAULT;
	room_info[dun->cent_n+1].seen = FALSE;

	/* Boost the rating */
	rating += v_ptr->rat;

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
}

/*
 * Place a locked door at the given location
 */
static void place_locked_door(int y, int x)
{
	int yy, xx;

	/* Already a door nearby, don't generate another one */
	for (yy = y - 7; yy < y + 8; yy++)
	{
		for (xx = x - 7; xx < x + 8; xx++)
		{
			if (cave_feat[yy][xx] == FEAT_CLOSED) return;
		}
	}

	for (yy = y - 3; yy < y + 4; yy++)
	{
		for (xx = x - 3; xx < x + 4; xx++)
		{
			if ((cave_feat[yy][xx] == FEAT_WALL_OUTER) && (decoration(yy, xx))) return;
			if ((cave_feat[yy][xx] == FEAT_FLOOR) && (!(cave_info[yy][xx] & (CAVE_ROOM)))) return;
		}
	}

	/* Create locked door */
	cave_set_feat(y, x, FEAT_CLOSED);
	if (rand_int(5) < 3) place_lock(y, x, FALSE, WG_DOOR_LOCK);

	if (cheat_room) message(MSG_CHEAT, 0, "Locked door.");
}

/*
 * Place a secret door at the given location
 */
static bool place_locked_door_longer(int y, int x)
{
	int yy, xx;

	/* Already a door nearby, don't generate another one */
	for (yy = y - 7; yy < y + 8; yy++)
	{
		for (xx = x - 7; xx < x + 8; xx++)
		{
			if (cave_feat[yy][xx] == FEAT_CLOSED) return (FALSE);
		}
	}

	/* No locked doors near decorated walls */
	for (yy = y - 3; yy < y + 4; yy++)
	{
		for (xx = x - 3; xx < x + 4; xx++)
		{
			if ((cave_feat[yy][xx] == FEAT_WALL_OUTER) && (decoration(yy, xx))) return (FALSE);
		}
	}

	for (yy = y - 5; yy < y + 6; yy++)
	{
		for (xx = x - 5; xx < x + 6; xx++)
		{
			if ((cave_feat[yy][xx] == FEAT_FLOOR) && (!(cave_info[yy][xx] & (CAVE_ROOM)))) return (FALSE);
		}
	}

	/* Create locked door */
	cave_set_feat(y, x, FEAT_CLOSED);
	if (rand_int(5) < 3) place_lock(y, x, FALSE, WG_DOOR_LOCK);

	if (cheat_room) message(MSG_CHEAT, 0, "Locked door.");
	return (TRUE);
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
	int i, y = 0, x = 0;
	int iii;
	int yy, xx;
	int tmp_row, tmp_col;
	int row_dir, col_dir;
	int start_row, start_col;
	int main_loop_count = 0;
	int kk;
	int end_room;

	bool door_flag = FALSE;

	/* Reset the arrays */
	dun->tunn_n = 0;
	dun->wall_n = 0;

	/* Save the starting location */
	start_row = row1;
	start_col = col1;

	/* Mark the start room */
	int start_room;
	start_room = room_idx_ignore_valid(start_row, start_col);

	int no_join = 0;

	/* Start out in the correct direction */
	correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

	/* Keep going until done (or bored) */
	while ((row1 != row2) || (col1 != col2))
	{
		/* Mega-Hack -- Paranoia -- prevent infinite loops */
		if (main_loop_count++ > 200000)
		{
			adult_autoscum = TRUE;
			break;
		}

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
		if (cave_feat[tmp_row][tmp_col] == FEAT_PERM_SOLID) adult_autoscum = TRUE;

		/* Avoid the edge of vaults */
		if (cave_feat[tmp_row][tmp_col] == FEAT_PERM_OUTER) adult_autoscum = TRUE;

		/* Avoid "solid" granite walls
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_SOLID) continue; */

		/* Clean any nearby "opportunistic" locked doors before making "necessary" piercings */
		for (yy = tmp_row - 5; yy < tmp_row + 6; yy++)
		{
			for (xx = tmp_col - 6; xx < tmp_col + 7; xx++)
			{
				if (cave_feat[yy][xx] == FEAT_CLOSED)
				{
					delete_trap(yy, xx);
					cave_set_feat(yy, xx, FEAT_WALL_OUTER);
					if (cheat_room) message(MSG_CHEAT, 0, "Locked door removed.");

					/* Also clear the tunnel leading to the door */
					for (iii = 1; iii < 5; iii++)
					{
						if ((cave_feat[yy+iii][xx] == FEAT_FLOOR) && (!(cave_info[yy+iii][xx] & (CAVE_ROOM))))
						{
							if (next_to_walls_and_room(yy+iii, xx) == 3)
							{
								cave_set_feat(yy+iii, xx, FEAT_WALL_EXTRA);
							}
						}

						if ((cave_feat[yy-iii][xx] == FEAT_FLOOR) && (!(cave_info[y-iii][x] & (CAVE_ROOM))))
						{
							if (next_to_walls_and_room(yy-iii, xx) == 3)
							{
								cave_set_feat(yy-iii, xx, FEAT_WALL_EXTRA);
							}
						}

						if ((cave_feat[yy][xx+iii] == FEAT_FLOOR) && (!(cave_info[yy][xx+iii] & (CAVE_ROOM))))
						{
							if (next_to_walls_and_room(yy, xx+iii) == 3)
							{
								cave_set_feat(yy, xx+iii, FEAT_WALL_EXTRA);
							}
						}

						if ((cave_feat[yy][xx-iii] == FEAT_FLOOR) && (!(cave_info[yy][xx-iii] & (CAVE_ROOM))))
						{
							if (next_to_walls_and_room(yy, xx-iii) == 3)
							{
								cave_set_feat(yy, xx-iii, FEAT_WALL_EXTRA);
							}
						}
					}
				}
			}
		}

		/* Pierce "outer" walls of rooms */
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_OUTER)
		{
			/* We can pierce 2 outer walls at the same time
			int outer_x[2], outer_y[2], num_outer = 0; */

			/* Get the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Clean any nearby "opportunistic" locked doors before making "necessary" piercings */
			for (yy = y - 5; yy < y + 6; yy++)
			{
				for (xx = x - 6; xx < x + 7; xx++)
				{
					if (cave_feat[yy][xx] == FEAT_CLOSED)
					{
						delete_trap(yy, xx);
						cave_set_feat(yy, xx, FEAT_WALL_OUTER);
						if (cheat_room) message(MSG_CHEAT, 0, "Locked door removed.");

						/* Also clear the tunnel leading to the door */
						for (iii = 1; iii < 5; iii++)
						{
							if ((cave_feat[yy+iii][xx] == FEAT_FLOOR) && (!(cave_info[yy+iii][xx] & (CAVE_ROOM))))
							{
								if (next_to_walls_and_room(yy+iii, xx) == 3)
								{
									cave_set_feat(yy+iii, xx, FEAT_WALL_EXTRA);
								}
							}

							if ((cave_feat[yy-iii][xx] == FEAT_FLOOR) && (!(cave_info[yy-iii][xx] & (CAVE_ROOM))))
							{
								if (next_to_walls_and_room(yy-iii, xx) == 3)
								{
									cave_set_feat(yy-iii, xx, FEAT_WALL_EXTRA);
								}
							}

							if ((cave_feat[yy][xx+iii] == FEAT_FLOOR) && (!(cave_info[yy][xx+iii] & (CAVE_ROOM))))
							{
								if (next_to_walls_and_room(yy, xx+iii) == 3)
								{
									cave_set_feat(yy, xx+iii, FEAT_WALL_EXTRA);
								}
							}

							if ((cave_feat[yy][xx-iii] == FEAT_FLOOR) && (!(cave_info[yy][xx-iii] & (CAVE_ROOM))))
							{
								if (next_to_walls_and_room(yy, xx-iii) == 3)
								{
									cave_set_feat(yy, xx-iii, FEAT_WALL_EXTRA);
								}
							}
						}
					}
				}
			}

			/* Hack -- Avoid outer/solid permanent walls */
			if (cave_feat[y][x] == FEAT_PERM_SOLID) adult_autoscum = TRUE;
			if (cave_feat[y][x] == FEAT_PERM_OUTER) adult_autoscum = TRUE;

			/* Feng Shui -- Avoid making an opening next to tables and altars */
			if (no_hack(y +1, x)) adult_autoscum = TRUE;
			if (no_hack(y -1, x)) adult_autoscum = TRUE;
			if (no_hack(y, x -1)) adult_autoscum = TRUE;
			if (no_hack(y, x +1)) adult_autoscum = TRUE;

			/* Feng Shui -- Avoid making an opening next to secret doors */
			if (cave_feat[y + 1][x] == FEAT_SECRET) continue;
			if (cave_feat[y - 1][x] == FEAT_SECRET) continue;
			if (cave_feat[y][x -1] == FEAT_SECRET) continue;
			if (cave_feat[y][x +1] == FEAT_SECRET) continue;

			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Mark the end room */
			end_room = room_idx_ignore_valid(tmp_row, tmp_col);

			/* Save the wall location */
			if (dun->wall_n < WALL_MAX)
			{
				dun->wall[dun->wall_n].y = row1;
				dun->wall[dun->wall_n].x = col1;
				dun->wall_n++;
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
				if ((tmp_row > 10) || (tmp_col > 10))
				{
					no_join = 1;
					break;
				}
			}
		}
	}

	/* Opportunistic locked door creation */
	int yyyy, xxxx, iiii;
	int dist_y, dist_x;
	int deeper = 0;
	int tunnel = 0;
	bool ok;
	for (i = 0; i < dun->tunn_n; i++)
	{
		ok = TRUE;

		/* Get the grid */
		y = dun->tunn[i].y;
		x = dun->tunn[i].x;

		for (iiii = 0; iiii < dun->wall_n; iiii++)
		{
			/* Get the grid */
			yyyy = dun->wall[iiii].y;
			xxxx = dun->wall[iiii].x;

			if (yyyy > y) dist_y = yyyy - y;
			else dist_y = y - yyyy;

			if (xxxx > x) dist_x = xxxx - x;
			else dist_x = x - xxxx;

			if (distance(y, x, yyyy, xxxx) < 8)
			{
				if (dist_y < 5) ok = FALSE;
			}
		}

		if (ok == FALSE) continue;

		/* Clear previous contents, add a locked door */
		if (cave_feat[y][x+1] == FEAT_WALL_OUTER) place_locked_door(y, x+1);
		if (cave_feat[y][x-1] == FEAT_WALL_OUTER) place_locked_door(y, x-1);
		if (cave_feat[y+1][x] == FEAT_WALL_OUTER) place_locked_door(y+1, x);
		if (cave_feat[y-1][x] == FEAT_WALL_OUTER) place_locked_door(y-1, x);
		if (cave_feat[y+1][x+1] == FEAT_WALL_OUTER) place_locked_door(y+1, x+1);
		if (cave_feat[y-1][x-1] == FEAT_WALL_OUTER) place_locked_door(y-1, x-1);
		if (cave_feat[y+1][x-1] == FEAT_WALL_OUTER) place_locked_door(y+1, x-1);
		if (cave_feat[y-1][x+1] == FEAT_WALL_OUTER) place_locked_door(y-1, x+1);
	}

	/* Opportunistic locked door creation, part II */
	for (i = 0; i < dun->tunn_n; i++)
	{
		ok = TRUE;

		/* Get the grid */
		y = dun->tunn[i].y;
		x = dun->tunn[i].x;

		for (deeper = 2; deeper < 7; deeper++)
		{
			for (iiii = 0; iiii < dun->wall_n; iiii++)
			{
				/* Get the grid */
				yyyy = dun->wall[iiii].y;
				xxxx = dun->wall[iiii].x;

				if (yyyy > y) dist_y = yyyy - y;
				else dist_y = y - yyyy;

				if (xxxx > x) dist_x = xxxx - x;
				else dist_x = x - xxxx;

				if (distance(y, x, yyyy, xxxx) < 9)
				{
					if (dist_y < 6) ok = FALSE;
				}
			}

			if (ok == FALSE) continue;

			/* Clear previous contents, add a locked door */
			if ((cave_feat[y][x+deeper] == FEAT_WALL_OUTER) && (!(cave_feat[y][x+deeper+1] == FEAT_WALL_OUTER)))
			{
				if (place_locked_door_longer(y, x+deeper))
				{
					for (tunnel = 1; tunnel < deeper; tunnel++)
					{
						cave_set_feat(y, x + tunnel, FEAT_FLOOR);
					}
				}
			}
			if ((cave_feat[y][x-deeper] == FEAT_WALL_OUTER) && (!(cave_feat[y][x-deeper-1] == FEAT_WALL_OUTER)))
			{
				if (place_locked_door_longer(y, x-deeper))
				{
					for (tunnel = 1; tunnel < deeper; tunnel++)
					{
						cave_set_feat(y, x-tunnel, FEAT_FLOOR);
					}
				}
			}
			/* Don't tunnel as deep in vertically */
			if (deeper < 4)
			{
				if ((cave_feat[y+deeper][x] == FEAT_WALL_OUTER) && (!(cave_feat[y+deeper+1][x] == FEAT_WALL_OUTER)))
				{
					if (place_locked_door_longer(y+deeper, x))
					{
						for (tunnel = 1; tunnel < deeper; tunnel++)
						{
							cave_set_feat(y+tunnel, x, FEAT_FLOOR);
						}
					}
				}

				if ((cave_feat[y-deeper][x] == FEAT_WALL_OUTER) && (!(cave_feat[y-deeper-1][x] == FEAT_WALL_OUTER)))
				{
					if (place_locked_door_longer(y-deeper, x))
					{
						for (tunnel = 1; tunnel < deeper; tunnel++)
						{
							cave_set_feat(y-tunnel, x, FEAT_FLOOR);
						}
					}
				}
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
		delete_trap(y, x);
	}

	/* Apply the piercings that we found */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Get the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* Clean any nearby "opportunistic" locked doors before making "necessary" piercings */
		for (yy = y - 5; yy < y + 6; yy++)
		{
			for (xx = x - 6; xx < x + 7; xx++)
			{
				if (cave_feat[yy][xx] == FEAT_CLOSED)
				{
					delete_trap(yy, xx);
					cave_set_feat(yy, xx, FEAT_WALL_OUTER);
					if (cheat_room) message(MSG_CHEAT, 0, "Locked door removed.");

					/* Also clear the tunnel leading to the door */
					for (iii = 1; iii < 5; iii++)
					{
						if ((cave_feat[yy+iii][xx] == FEAT_FLOOR) && (!(cave_info[yy+iii][xx] & (CAVE_ROOM))))
						{
							if (next_to_walls_and_room(yy+iii, xx) == 3)
							{
								cave_set_feat(yy+iii, xx, FEAT_WALL_EXTRA);
							}
						}

						if ((cave_feat[yy-iii][xx] == FEAT_FLOOR) && (!(cave_info[yy-iii][xx] & (CAVE_ROOM))))
						{
							if (next_to_walls_and_room(yy-iii, xx) == 3)
							{
								cave_set_feat(yy-iii, xx, FEAT_WALL_EXTRA);
							}
						}

						if ((cave_feat[yy][xx+iii] == FEAT_FLOOR) && (!(cave_info[yy][xx+iii] & (CAVE_ROOM))))
						{
							if (next_to_walls_and_room(yy, xx+iii) == 3)
							{
								cave_set_feat(yy, xx+iii, FEAT_WALL_EXTRA);
							}
						}

						if ((cave_feat[yy][xx-iii] == FEAT_FLOOR) && (!(cave_info[yy][xx-iii] & (CAVE_ROOM))))
						{
							if (next_to_walls_and_room(yy, xx-iii) == 3)
							{
								cave_set_feat(yy, xx-iii, FEAT_WALL_EXTRA);
							}
						}
					}
				}
			}
		}

		/* Join the parts */
		end_room = room_idx_ignore_valid(y, x);

		for (kk = 0; kk < dun->cent_n; kk++)
		{
			if (dun->part[kk] == dun->part[end_room])
			{
				dun->part[kk] = dun->part[start_room];
			}
		}

		/* Decorated walls always have secret doors */
		/* Already a decorated secret door nearby? Don't make another */
		bool make_door = TRUE;
		if (decoration(y, x))
		{
			/* for (yy = y - 1; yy <= y + 1; yy++)
			{
				for (xx = x - 1; xx <= x + 1; xx++)
				{
					if ((decoration(yy, xx)) && (cave_feat[yy][xx] == FEAT_SECRET))
					{
						make_door = FALSE;
					}
				}
			} */

			if (make_door)
			{
				if (t_list[cave_t_idx[y][x]].w_idx == WG_SHELF)
				{
					place_decoration(y, x, WG_SHELF_SECRET_DOOR);
				}
				else if (t_list[cave_t_idx[y][x]].w_idx == WG_PAINTING)
				{
					place_decoration(y, x, WG_PAINTING_SECRET_DOOR);
				}
				else if (t_list[cave_t_idx[y][x]].w_idx == WG_RACK)
				{
					place_decoration(y, x, WG_RACK_SECRET_DOOR);
				}
				else if (t_list[cave_t_idx[y][x]].w_idx == WG_CLOSET)
				{
					place_decoration(y, x, WG_CLOSET_SECRET_DOOR);
				}
				else if (t_list[cave_t_idx[y][x]].w_idx >= WG_WARD_SLUMBER_ACTIVE_HIDDEN)
				{
					place_decoration(y, x, WG_PAINTING_SECRET_DOOR);
				}
			}
		}

		/* Occasional doorway */
		else if (rand_int(100) < DUN_TUN_PEN)
		{
			/* Place a random door */
			place_random_door(y, x);
		}

		/* Convert to floor grid */
		else cave_set_feat(y, x, FEAT_FLOOR);
	}

	/* Go through the piercings again, repairing any false starts */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Get the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* If the piercing doesn't have an empty floor grid neighbour outside the cave, it's a false start */
		/* Note that joining adjacent rooms has to work! This removes connections between them */
		if ((cave_feat[y+1][x] == FEAT_FLOOR) && (!(cave_info[y+1][x] & (CAVE_ROOM)))) continue;
		if ((cave_feat[y][x+1] == FEAT_FLOOR) && (!(cave_info[y][x+1] & (CAVE_ROOM)))) continue;
		if ((cave_feat[y-1][x] == FEAT_FLOOR) && (!(cave_info[y-1][x] & (CAVE_ROOM)))) continue;
		if ((cave_feat[y][x-1] == FEAT_FLOOR) && (!(cave_info[y][x-1] & (CAVE_ROOM)))) continue;

		/* Repair both ordinary and special walls */
		if (t_list[cave_t_idx[y][x]].w_idx == WG_SHELF_SECRET_DOOR) place_decoration(y, x, WG_SHELF);
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_CLOSET_SECRET_DOOR) place_decoration(y, x, WG_CLOSET);
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_RACK_SECRET_DOOR) place_decoration(y, x, WG_RACK);
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_PAINTING_SECRET_DOOR) place_decoration(y, x, WG_PAINTING);
		else cave_set_feat(y, x, FEAT_WALL_OUTER);
	}

	/* Go through the piercings yet again, repairing any piercings next to secret doors */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Already fixed */
		if (cave_feat[y][x-1] == FEAT_WALL_OUTER) continue;

		/* Get the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* If the piercing doesn't have an empty floor grid neighbour outside the cave, it's a false start */
		/* Note that joining adjacent rooms has to work! This removes connections between them */
		if ((cave_feat[y+1][x] == FEAT_FLOOR) && (!(cave_info[y+1][x] & (CAVE_ROOM))))
		{
			/* Feng Shui -- undo an opening next to secret doors */
			if (cave_feat[y + 1][x] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x] == FEAT_SECRET)
				{}
			else if (cave_feat[y][x + 1 ] == FEAT_SECRET)
				{}
			else if (cave_feat[y][x - 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y + 1][x + 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x - 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x + 1 ] == FEAT_SECRET)
				{}
			else if (cave_feat[y + 1][x - 1] == FEAT_SECRET)
				{}
			else continue;
		}
		if ((cave_feat[y][x+1] == FEAT_FLOOR) && (!(cave_info[y][x+1] & (CAVE_ROOM))))
		{
			/* Feng Shui -- undo an opening next to secret doors */
			if (cave_feat[y + 1][x] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x] == FEAT_SECRET)
				{}
			else if (cave_feat[y][x + 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y][x - 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y + 1][x + 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x - 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x + 1 ] == FEAT_SECRET)
				{}
			else if (cave_feat[y + 1][x - 1] == FEAT_SECRET)
				{}
			else continue;
		}
		if ((cave_feat[y-1][x] == FEAT_FLOOR) && (!(cave_info[y-1][x] & (CAVE_ROOM))))
		{
			/* Feng Shui -- undo an opening next to secret doors */
			if (cave_feat[y + 1][x] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x] == FEAT_SECRET)
				{}
			else if (cave_feat[y][x + 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y][x - 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y + 1][x + 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x - 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x + 1 ] == FEAT_SECRET)
				{}
			else if (cave_feat[y + 1][x - 1] == FEAT_SECRET)
				{}
			else continue;
		}
		if ((cave_feat[y][x-1] == FEAT_FLOOR) && (!(cave_info[y][x-1] & (CAVE_ROOM))))
		{
			/* Feng Shui -- undo an opening next to secret doors */
			if (cave_feat[y + 1][x] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x] == FEAT_SECRET)
				{}
			else if (cave_feat[y][x + 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y][x - 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y + 1][x + 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x - 1] == FEAT_SECRET)
				{}
			else if (cave_feat[y - 1][x + 1 ] == FEAT_SECRET)
				{}
			else if (cave_feat[y + 1][x - 1] == FEAT_SECRET)
				{}
			else continue;
		}

		/* Repair both ordinary and special walls */
		if (t_list[cave_t_idx[y][x]].w_idx == WG_SHELF_SECRET_DOOR) place_decoration(y, x, WG_SHELF);
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_CLOSET_SECRET_DOOR) place_decoration(y, x, WG_CLOSET);
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_RACK_SECRET_DOOR) place_decoration(y, x, WG_RACK);
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_PAINTING_SECRET_DOOR) place_decoration(y, x, WG_PAINTING);
		else cave_set_feat(y, x, FEAT_WALL_OUTER);
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
	int i, dist, nearest_dist;

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

	/* Reset some values */
	dun->style[dun->cent_n] = 0;
	dun->monster[dun->cent_n] = 0;
	dun->joined[dun->cent_n] = 100;
	dun->closest[dun->cent_n] = 100;
	nearest_dist = 10000;

	/* Or should we copy the style from a nearby room? */
	for (i = 0; i < dun->cent_n; i++)
	{
		dist = distance(dun->cent[i].y, dun->cent[i].x, y, x);
		if ((dist > 0) && (dist < nearest_dist) && (dist < 30))
		{
			nearest_dist = dist;
			if (dun->style[i])
			{
				dun->style[dun->cent_n] = dun->style[i];
			}
		}
	}

	/* Roll the style for the room */
	if (dun->style[dun->cent_n] == 0) dun->style[dun->cent_n] = randint(6);

	/* Build a room */
	switch (typ)
	{
		/* Build an appropriate room */
		case 9: build_type9(y, x); break;
		case 8: build_type8(y, x); break;
		case 7: build_type7(y, x); break;
		case 6: build_type6(y, x); break;
		case 5: build_type5(y, x); break;
		case 4: build_type123(y, x); break;
		case 3: build_type123(y, x); break;
		case 2: build_type123(y, x); break;
		case 1: build_type123(y, x); break;

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

			dun_room[by][bx] = dun->cent_n;
		}
	}

	/* Count "crowded" rooms */
	if ((typ == 5) || (typ == 6)) dun->crowded = TRUE;

	/* Success */
	return (TRUE);
}

/*
 * Make a connection between two rooms.
 * Helper function between cave_gen and build_tunnel.
 */
static void connect_rooms(int source, int target)
{
	int own_distance;
	int joined_distance;
	int closest_distance;

	/* If a room already connected to the target room is closer, tunnel there instead */
	if (!(dun->closest[target] == 100))
	{
		own_distance = distance(dun->cent[source].y, dun->cent[source].x, dun->cent[target].y, dun->cent[target].x);
		closest_distance = distance(dun->cent[source].y, dun->cent[source].x, dun->cent[dun->closest[target]].y, dun->cent[dun->closest[target]].x);
		if (own_distance > closest_distance)
		{

			/* If a room joined to the new target room is closer, tunnel there instead */
			if (!(dun->joined[target] == 100))
			{
				own_distance = distance(dun->cent[source].y, dun->cent[source].x, dun->cent[dun->joined[dun->closest[target]]].y, dun->cent[dun->joined[dun->closest[target]]].x);
				joined_distance = distance(dun->cent[source].y, dun->cent[source].x, dun->cent[dun->joined[dun->closest[target]]].y, dun->cent[dun->joined[dun->closest[target]]].x);
				if (own_distance > joined_distance)
				{
					build_tunnel(dun->cent[source].y, dun->cent[source].x, dun->cent[dun->joined[dun->closest[target]]].y, dun->cent[dun->joined[dun->closest[target]]].x);
					dun->closest[source] = dun->joined[dun->closest[target]];
				}
				else
				{
					build_tunnel(dun->cent[source].y, dun->cent[source].x, dun->cent[dun->closest[target]].y, dun->cent[dun->closest[target]].x);
					dun->closest[source] = dun->closest[target];
				}
			}
			else
			{
				build_tunnel(dun->cent[source].y, dun->cent[source].x, dun->cent[target].y, dun->cent[target].x);
				dun->closest[source] = target;
			}
		}
	}

	/* If a room joined to the target room is closer, tunnel there instead */
	else if (!(dun->joined[target] == 100))
	{
		own_distance = distance(dun->cent[source].y, dun->cent[source].x, dun->cent[target].y, dun->cent[target].x);
		joined_distance = distance(dun->cent[source].y, dun->cent[source].x, dun->cent[dun->joined[target]].y, dun->cent[dun->joined[target]].x);
		if (own_distance > joined_distance)
		{
			build_tunnel(dun->cent[source].y, dun->cent[source].x, dun->cent[dun->joined[target]].y, dun->cent[dun->joined[target]].x);
			dun->closest[source] = dun->joined[target];
		}
		else
		{
			build_tunnel(dun->cent[source].y, dun->cent[source].x, dun->cent[target].y, dun->cent[target].x);
			dun->closest[source] = target;
		}
	}

	else
	{
		build_tunnel(dun->cent[source].y, dun->cent[source].x, dun->cent[target].y, dun->cent[target].x);
		dun->closest[source] = target;
	}
}

/*
 * Attempt to allocate a random monster in the dungeon.
 *
 * Place the monster at least "dis" distance from the player.
 *
 * Use "slp" to choose the initial "sleep" status
 *
 * Use "monster_level" for the monster level
 */
bool alloc_typical_monster(int dis)
{
	int y, x;
	int attempts_left = 10000;
 
	/* Find a legal, distant, unoccupied, space */
	while (--attempts_left)
	{
		/* Pick a location */
		y = rand_int(p_ptr->cur_map_hgt);
		x = rand_int(p_ptr->cur_map_wid);

		/* Require "naked" floor grid */
		if (!cave_naked_bold(y, x)) continue;

		/* Accept far away grids */
		if (distance(y, x, p_ptr->py, p_ptr->px) > dis) break;

		break;
	}

	if (!attempts_left)
	{
		if (cheat_wizard || cheat_hear)
		{
			message(MSG_CHEAT, 0, "Warning! Could not allocate a new monster.");
		}

		return FALSE;
	}

	/* All "wandering" monsters are from Chaos or Aether */
	if (rand_int(100) < 50)
	{
		get_mon_num_hook = vault_aux_chaos;
	}
	else
	{
		get_mon_num_hook = vault_aux_aether;
	}

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Place several monsters and maybe items too */
	int number = randint(3);
	vault_monsters(y, x, number);
	if (rand_int(3) < 1) vault_objects(y, x, number - rand_int(3));

	/* Attempt to place the monster, allow groups
	if (place_monster(y, x)) return (TRUE); */

	/* Reset hook and prepare allocation table */
	get_mon_num_hook = NULL;
	get_mon_num_prep();

	/* Nope */
	return (FALSE);
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static void cave_gen(void)
{
	int i, k, y, x, y1, x1, kkk;

	int by, bx;

	bool destroyed = FALSE;
	bool quest_vault = FALSE;

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;

	/* Basic "amount" */
	k = (p_ptr->depth / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;

	/* Reset generation variables */
	mon_gen = 9 + randint(k/2) + randint(k/3) + rand_int(2);
	obj_gen = Rand_normal(DUN_AMT_ROOM + rand_int(2), 3);

	/* A small level */
	int l = 2;
	int m = 2;

	p_ptr->cur_map_hgt = l * (2 * PANEL_HGT);
	p_ptr->cur_map_wid = m * (2 * PANEL_WID) + PANEL_WID;

	if (cheat_room) message_format(MSG_CHEAT, 0, "A 'small' dungeon level (%dx%d).", m, l);

	rating += ((m + l <= 3) ? 15 : 10);

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

	/* Initialize 'zeroeth' room description */
	room_info[0].seen = FALSE;

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
	int dun_rooms_bonus = 0;
	if (!(dungeon_align)) dun_rooms_bonus = 10;

	for (i = 0; i < DUN_ROOMS + dun_rooms_bonus; i++)
	{
		/* Pick a block for the room */
		by = rand_int(dun->row_rooms);
		bx = rand_int(dun->col_rooms);

		/* Align dungeon rooms */
		if (dungeon_align)
		{
			/* Slide some rooms right */
			if (((bx % 3) == 0) && (rand_int(2) < 1)) bx++;

			/* Slide some rooms left */
			if (((bx % 3) == 2) && (rand_int(2) < 1)) bx--;
		}

		/* Destroyed levels are boring */
		if (destroyed)
		{
			/* Attempt a "trivial" room */
			if (room_build(by, bx, 1)) continue;

			/* Never mind */
			continue;
		}

		/* Build a quest vault if needed */
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
			kkk = rand_int(100);

			/* Attempt a very unusual room */
			if (rand_int(DUN_UNUSUAL) < p_ptr->depth)
			{
				/* Type 8 -- Greater vault (10%)
				if ((k < 10) && room_build(by, bx, 8)) continue; */

				/* Type 7 -- Lesser vault (15%) */
				if ((kkk < 25) && room_build(by, bx, 7)) continue;

				/* Type 6 -- Monster pit (14%)
				if ((k < 39) && room_build(by, bx, 6)) continue; */

				/* Type 5 -- Monster nest (11%)
				if ((k < 50) && room_build(by, bx, 5)) continue; */
			}

			/* Type 4 -- Large room (25%) */
			if ((kkk < 25) && room_build(by, bx, 4)) continue;

			/* Type 3 -- Cross room (25%) */
			if ((kkk < 50) && room_build(by, bx, 3)) continue;

			/* Type 2 -- Overlapping (50%) */
			if ((kkk < 100) && room_build(by, bx, 2)) continue;
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

	/* Start with no tunnel doors */
	dun->door_n = 0;

	int room1;
	int room2;
	int prev;
	int new_shuffle = 0;
	int bad_stuff = 0;
	int pick1, pick2;
	int part1;
	int style1;
	int monster1;
	int joined1;
	int closest1;
	int zzz;
	int room_distance = rand_int(8) + 46;
	int modified_distance;

	/* Partition rooms */
	for (i = 0; i < dun->cent_n; i++)
	{
		dun->part[i] = i;
	}

	/* Hack -- Scramble the room order */
	for (i = 0; i < dun->cent_n; i++)
	{
		/* Hack -- Scramble the room order */
		pick1 = rand_int(dun->cent_n);
		pick2 = rand_int(dun->cent_n);
		y1 = dun->cent[pick1].y;
		x1 = dun->cent[pick1].x;
		part1 = dun->part[pick1];
		style1 = dun->style[pick1];
		monster1 = dun->monster[pick1];
		joined1 = dun->joined[pick1];
		closest1 = dun->closest[pick1];
		dun->cent[pick1].y = dun->cent[pick2].y;
		dun->cent[pick1].x = dun->cent[pick2].x;
		dun->part[pick1] = dun->part[pick2];
		dun->style[pick1] = dun->style[pick2];
		dun->monster[pick1] = dun->monster[pick2];
		dun->joined[pick1] = dun->joined[pick2];
		dun->closest[pick1] = dun->closest[pick2];
		dun->cent[pick2].y = y1;
		dun->cent[pick2].x = x1;
		dun->part[pick2] = part1;
		dun->style[pick2] = style1;
		dun->monster[pick2] = monster1;
		dun->joined[pick2] = joined1;
		dun->closest[pick2] = closest1;
	}

	/* Scum for close distances */
	for (zzz = 0; zzz < 3000; zzz++)
	{
		new_shuffle = 0;
		bad_stuff = 0;

		/* Hack -- connect the first room to the last room */
		prev = dun->cent_n-1;
		y = dun->cent[dun->cent_n-1].y;
		x = dun->cent[dun->cent_n-1].x;

		/* Connect all the rooms together */
		for (i = 0; i < dun->cent_n; i++)
		{
			modified_distance = distance(dun->cent[i].y, dun->cent[i].x, y, x);
			if (!(dun->style[i] == dun->style[prev])) modified_distance -= 10;

			if (modified_distance > room_distance) bad_stuff++;

			/* Remember the "previous" room */
			y = dun->cent[i].y;
			x = dun->cent[i].x;
			prev = i;
		}

		/* Hack -- Scramble the room order */
		if (bad_stuff > 1)
		{
			for (i = 0; i < dun->cent_n; i++)
			{
				/* Hack -- Scramble the room order */
				pick1 = rand_int(dun->cent_n);
				pick2 = rand_int(dun->cent_n);
				y1 = dun->cent[pick1].y;
				x1 = dun->cent[pick1].x;
				part1 = dun->part[pick1];
				style1 = dun->style[pick1];
				monster1 = dun->monster[pick1];
				joined1 = dun->joined[pick1];
				closest1 = dun->closest[pick1];
				dun->cent[pick1].y = dun->cent[pick2].y;
				dun->cent[pick1].x = dun->cent[pick2].x;
				dun->part[pick1] = dun->part[pick2];
				dun->style[pick1] = dun->style[pick2];
				dun->monster[pick1] = dun->monster[pick2];
				dun->joined[pick1] = dun->joined[pick2];
				dun->closest[pick1] = dun->closest[pick2];
				dun->cent[pick2].y = y1;
				dun->cent[pick2].x = x1;
				dun->part[pick2] = part1;
				dun->style[pick2] = style1;
				dun->monster[pick2] = monster1;
				dun->joined[pick2] = joined1;
				dun->closest[pick2] = closest1;
			}

			adult_autoscum = TRUE;
		}
		else
		{
			adult_autoscum = FALSE;
			break;
		}
	}

	/* Hack -- connect the first room to the last room */
	prev = dun->cent_n-1;
	y = dun->cent[dun->cent_n-1].y;
	x = dun->cent[dun->cent_n-1].x;

	/* Connect all the rooms together */
	for (i = 0; i < dun->cent_n; i++)
	{
		connect_rooms(i, prev);
	}

	/* Test if the rooms are in the same part; if not, autoscum */
        for (i = 0; i < dun->cent_n; i++)
        {
                room1 = i;
                for (room2 = 0; room2 < dun->cent_n; room2++)
                {
                        if (dun->part[room1] == dun->part[room2]) continue;

			if (dun->part[room1] == dun->part[dun->closest[room2]]) continue;
			if (dun->part[room2] == dun->part[dun->closest[room1]]) continue;
			if (dun->part[dun->closest[room1]] == dun->part[dun->closest[room2]]) continue;
			if (dun->part[room1] == dun->part[dun->closest[dun->closest[room2]]]) continue;
			if (dun->part[room2] == dun->part[dun->closest[dun->closest[room1]]]) continue;
			if (dun->part[dun->closest[dun->closest[room1]]] == dun->part[dun->closest[room2]]) continue;
			if (dun->part[dun->closest[dun->closest[room2]]] == dun->part[dun->closest[room1]]) continue;
			if (dun->part[dun->closest[dun->closest[room1]]] == dun->part[dun->closest[dun->closest[room2]]]) continue;
			if (dun->part[dun->closest[dun->closest[room2]]] == dun->part[dun->closest[dun->closest[room1]]]) continue;
                        adult_autoscum = TRUE;
                }
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

	/* Hack -- Add some magma streamers
	for (i = 0; i < DUN_STR_MAG; i++)
	{
		build_streamer(FEAT_MAGMA, DUN_STR_MC);
	} */

	/* Hack -- Add some quartz streamers
	for (i = 0; i < DUN_STR_QUA; i++)
	{
		build_streamer(FEAT_QUARTZ, DUN_STR_QC);
	} */

	/* Destroy the level if necessary */
	if (destroyed) destroy_level();

	/* Place 3 or 4 down stairs near some walls */
	alloc_stairs(FEAT_MORE, rand_range(2, 2), 7);

	/* Place up stairs near some walls
	   Less up stairs in FayAngband */
	if ((1 > rand_int(3)) || ((quest_check(p_ptr->depth) == QUEST_FIXED) || (quest_check(p_ptr->depth) == QUEST_FIXED_U)))
	{
		alloc_stairs(FEAT_LESS, rand_range(1, 1), 7);
	}

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

	/* Place some traps in the dungeon
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k)); */

	/* Determine the character location */
	new_player_spot();

	/* Put some faery portals in the corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_FAERY_PORTAL, Rand_normal(DUN_AMT_PORTAL, 3));

	/* Create some tiny rooms with locked doors */
	int tiny_rooms = 0;
	int yy, xx;
	int rows = 0;
	int columns = 0;
	bool valid;
	int number_of_rooms = rand_int(5);
	while (tiny_rooms < number_of_rooms)
	{
		valid = TRUE;

		/* Room size */
		rows = rand_int(2)+2;
		columns = rand_int(3)+2;

		/* Pick a random grid */
		y = rand_int(p_ptr->cur_map_hgt - rows - 1);
		x = rand_int(p_ptr->cur_map_wid - columns - 1);

		/* All grids must be stone and be inside the dungeon */
		for (yy = y - 1; yy <= y + rows + 1; yy++)
		{
			for (xx = x - 1; xx <= x + columns + 1; xx++)
			{
				if (cave_feat[yy][xx] == FEAT_WALL_EXTRA) {}
				else if (cave_feat[yy][xx] == FEAT_WALL_OUTER) {}
				else if (cave_feat[yy][xx] == FEAT_WALL_INNER) {}
				else valid = FALSE;
			}

			if (valid == FALSE) continue;
		}

		if (valid == FALSE) continue;

		valid = FALSE;

		/* Try 30 times to place a door to a random location */
		for (i = 0; i < 30; i++)
		{
			yy = y + rand_int(rows + 2) - 1;
			xx = x + rand_int(columns + 2) - 1;

			if (next_to_floor_tunnel(yy, xx) > 0)
			{
				valid = TRUE;
				break;
			}
			else continue;
		}

		if (valid == FALSE) continue;

		/* Draw the inside */
		generate_fill_tiny_room(y, x, y + rows - 1, x + columns - 1, FEAT_FLOOR);

		/* Make a secret door on special walls */
		if (decoration(yy, xx))
		{
			if (t_list[cave_t_idx[yy][xx]].w_idx == WG_SHELF)
			{
				place_decoration(yy, xx, WG_SHELF_SECRET_DOOR);
			}
			else if (t_list[cave_t_idx[yy][xx]].w_idx == WG_PAINTING)
			{
				place_decoration(yy, xx, WG_PAINTING_SECRET_DOOR);
			}
			else if (t_list[cave_t_idx[yy][xx]].w_idx == WG_RACK)
			{
				place_decoration(yy, xx, WG_RACK_SECRET_DOOR);
			}
			else if (t_list[cave_t_idx[yy][xx]].w_idx == WG_CLOSET)
			{
				place_decoration(yy, xx, WG_CLOSET_SECRET_DOOR);
			}
			else if (t_list[cave_t_idx[yy][xx]].w_idx >= WG_WARD_SLUMBER_ACTIVE_HIDDEN)
			{
				place_decoration(yy, xx, WG_PAINTING_SECRET_DOOR);
			}
		}
		/* Otherwise place a closed door, and sometimes lock it */
		else
		{
			cave_set_feat(yy, xx, FEAT_CLOSED);
			if (rand_int(5) < 2) place_lock(yy, xx, FALSE, WG_DOOR_LOCK);
		}

		/* All done */
		tiny_rooms ++;
	}

	/* Put some monsters in the dungeon */
	for (i = mon_gen; i > 0; i--)
	{
		(void)alloc_typical_monster(0);
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
					place_monster_aux(y, x, q_ptr->mon_idx, 0, TRUE, TRUE, 0);
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
				place_monster_aux(y, x, u_info[q_ptr->mon_idx].r_idx, 
					q_ptr->mon_idx, TRUE, TRUE, PLACE_UNIQUE);
			}
		}
	}

	/* Put some gold in the dungeon */
	alloc_object(ALLOC_SET_TINY, ALLOC_TYP_GOLD, Rand_normal(DUN_AMT_GOLD, 3));

	/* Put some objects in tiny rooms */
	if (obj_gen > 0) alloc_object(ALLOC_SET_TINY, ALLOC_TYP_OBJECT, obj_gen);
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
	y1 = y0 - (1 + randint((yy == 0) ? 2 : 1));
	y2 = y0 + (1 + randint((yy == 1) ? 2 : 1));
	x1 = x0 - (1 + randint(3));
	x2 = x0 + (1 + randint(3));

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
	int y, x, k, n;

	int rooms[MAX_STORES];

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = seed_town;

	/* Prepare an Array of "remaining stores", and count them */
	for (n = 0; n < MAX_STORES; n++) rooms[n] = (n-1);

	/* Place two rows of stores */
	for (y = 0; y < 2; y++)
	{
		/* Place four stores per row */
		for (x = 0; x < 5; x++)
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
		/* Pick a location at least "three" from the outer walls */
		y = rand_range(3, p_ptr->cur_map_hgt - 4);
		x = rand_range(3, p_ptr->cur_map_wid - 4);

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

	int by,bx;

	/* Initialize the room table */
	for (by = 0; by < MAX_ROOMS_ROW; by++)
	{
		for (bx = 0; bx < MAX_ROOMS_COL; bx++)
		{
			dun_room[by][bx] = 0;
		}
	}

	/* Reset room info */
	room_info[0].seen = FALSE;

	/* Restrict to single-screen size */
	p_ptr->cur_map_hgt = (2 * PANEL_HGT);
	p_ptr->cur_map_wid = (2 * PANEL_WID);

	/* In Fay, it's always day time */
	daytime = TRUE;
	residents = MIN_M_ALLOC_TD;

	/* Hack -- Start with permanent rock */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Create granite wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);
		}
	}
	
	/* Place some floors */
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
	get_mon_num_hook = NULL;
	get_mon_num_prep();
	for (i = 0; i < residents; i++)
	{
		/* Make a resident */
		(void)alloc_monster(3);
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
		mon_max = 1;
		t_max = 1;

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
				/* No flow */
				cave_cost[y][x] = 0;
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
		if (!p_ptr->depth) town_gen();

		/* Build a real level */
		else cave_gen();

		/* Extract the feeling */
		if (rating > 100) p_ptr->feeling = 2;
		else if (rating > 80) p_ptr->feeling = 3;
		else if (rating > 60) p_ptr->feeling = 4;
		else if (rating > 40) p_ptr->feeling = 5;
		else if (rating > 30) p_ptr->feeling = 6;
		else if (rating > 20) p_ptr->feeling = 7;
		else if (rating > 10) p_ptr->feeling = 8;
		else if (rating > 0) p_ptr->feeling = 9;
		else p_ptr->feeling = 10;

		/* Hack -- Have a special feeling sometimes */
		if (good_item_flag && !adult_preserve) p_ptr->feeling = 1;

		/* Hack -- no feeling in the town */
		if (!p_ptr->depth) p_ptr->feeling = 0;

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
		if (adult_autoscum)
		{
			why = "all rooms not connected";
			adult_autoscum = FALSE;

			/* Try again */
			okay = FALSE;
		}

		/* Accept */
		if (okay) break;

		/* Message
		if (why) message_format(MSG_CHEAT, 0, "Generation restarted (%s)", why); */

		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the traps */
		wipe_t_list();

		/* Wipe the monsters */
		wipe_mon_list();
	}

	/* The dungeon is ready */
	character_dungeon = TRUE;
}
