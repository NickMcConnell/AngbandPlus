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
#define DUN_ROOMS	50	/* Number of rooms to attempt */
#define DUN_UNUSUAL	200	/* Level/chance of unusual room */
#define DUN_DEST	15	/* 1/chance of having a destroyed level */
#define DUN_CAVERN	30	/* 1/chance of having a cavern level */

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
#define ROOM_MAX	13



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
 * Array of minimum room depths
 */
static s16b  roomdep[ROOM_MAX] =
{0,/* 0 = Nothing */
1,/* 1 = Simple (33x11) */
1,/* 2 = Overlapping (33x11) */
3,/* 3 = Crossed (33x11) */
3,/* 4 = Large (33x11) */
5,/* 5 = Monster nest (33x11) */
5,/* 6 = Monster pit (33x11) */
5,/* 7 = Lesser vault (33x22) */
10,/* 8 = Greater vault (66x44) */
3,/* 9 = Fractal cave (42x24) */
5,/* 10 =Random vault (44x22) */
1,/* 11 =Circular rooms (22x22) */
3};/* 12 =Crypt (22x22) */


/*
 * Always pick a correct direction
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
	while (1)
	{
		/* Pick a legal spot */
		y = rand_range(1, DUNGEON_HGT - 2);
		x = rand_range(1, DUNGEON_WID - 2);

		/* Must be a "naked" floor grid */
		if (!cave_naked_bold(y, x)) continue;

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
 * Convert existing terrain type to rubble
 */
static void place_rubble(int y, int x)
{
	/* Create rubble */
	cave_set_feat(y, x, FEAT_RUBBLE);
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
	if (!p_ptr->depth)
	{
		place_down_stairs(y, x);
	}
	else if (is_quest(p_ptr->depth) || (p_ptr->depth >= MAX_DEPTH-1))
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
 * Place a secret door at the given location
 */
static void place_secret_door(int y, int x)
{
	/* Create secret door */
	cave_set_feat(y, x, FEAT_SECRET);
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
		cave_set_feat(y, x, FEAT_OPEN);
	}

	/* Broken doors (100/1000) */
	else if (tmp < 400)
	{
		/* Create broken door */
		cave_set_feat(y, x, FEAT_BROKEN);
	}

	/* Secret doors (200/1000) */
	else if (tmp < 600)
	{
		/* Create secret door */
		cave_set_feat(y, x, FEAT_SECRET);
	}

	/* Closed doors (300/1000) */
	else if (tmp < 900)
	{
		/* Create closed door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);
	}

	/* Locked doors (99/1000) */
	else if (tmp < 999)
	{
		/* Create locked door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + randint(7));
	}

	/* Stuck doors (1/1000) */
	else
	{
		/* Create jammed door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x08 + rand_int(8));
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

				/* Require "naked" floor grid */
				if (!cave_naked_bold(y, x)) continue;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(y, x) < walls) continue;

				/* Town -- must go down */
				if (!p_ptr->depth)
				{
					/* Clear previous contents, add down stairs */
					cave_set_feat(y, x, FEAT_MORE);
				}

				/* Quest -- must go up */
				else if (is_quest(p_ptr->depth) || (p_ptr->depth >= MAX_DEPTH-1))
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

/* Function that sees if a square is a floor.  (Includes range checking.) */
static bool get_is_floor(int x,int y)
{
	if (!in_bounds_fully(y,x))
	{
		/* Out of bounds */
		return(FALSE);	
	}
	
	/*Do the real check: */
	if (cave_feat[y][x]==FEAT_FLOOR) return(TRUE);
	
	return(FALSE);
}

/* Set a square to be floor.  (Includes range checking.) */
static void set_floor(int x,int y)
{
	if (!in_bounds_fully(y,x))
	{
		/* Out of bounds */
		return;	
	}
	if (cave_info[y][x]&CAVE_ROOM)
	{
		/*A room border don't touch */
		return;
	}
	
	/*Set to be floor */
	cave_feat[y][x]=FEAT_FLOOR;
	cave_info[y][x]&= ~CAVE_WALL;
}



/* This function tunnels around a room if it will cut off part of a cave system */
static void check_room_boundary(int x1,int y1,int x2,int y2)
{
	int count,x,y;
	bool old_is_floor, new_is_floor;

	/* Initialize */

	count=0;
	
	old_is_floor=get_is_floor(x1-1,y1);
	
	/*
	* Count the number of floor-wall boundaries around the room
	* Note: diagonal squares are ignored since the player can move diagonally
	* to bypass these if needed.
	*/
	
	/*Above the top boundary*/
	for(x=x1;x<=x2;x++)
	{
		new_is_floor=get_is_floor(x,y1-1);
		
		/* increment counter if they are different */
		if(new_is_floor!=old_is_floor) count++;
		
		old_is_floor=new_is_floor;	
	}
	
	/*Right boundary*/
	for(y=y1;y<=y2;y++)
	{
		new_is_floor=get_is_floor(x2+1,y);
		
		/* increment counter if they are different */
		if(new_is_floor!=old_is_floor) count++;
		
		old_is_floor=new_is_floor;	
	}
	
	/*Bottom boundary*/
	for(x=x2;x>=x1;x--)
	{
		new_is_floor=get_is_floor(x,y2+1);
		
		/* increment counter if they are different */
		if(new_is_floor!=old_is_floor) count++;
		
		old_is_floor=new_is_floor;	
	}
	
	/*Left boundary*/
	for(y=y2;y>=y1;y--)
	{
		new_is_floor=get_is_floor(x1-1,y);
		
		/* increment counter if they are different */
		if(new_is_floor!=old_is_floor) count++;
		
		old_is_floor=new_is_floor;	
	}

	
	/*If all the same, or only one connection exit. */
	if (count<=2) return;


    /*Tunnel around the room so to prevent problems with caves*/
	
	for(y=y1;y<=y2;y++)
	{
		for(x=x1;x<=x2;x++)
		{
		set_floor(x,y);
		}		
	}
	
}

/*
* This function is used to allocate the space needed by a room in the room_map
* array.
* x, y represent the size of the room (0...x-1) by (0...y-1).
* crowded is used to denote a monset nest.
* by0, bx0 are the positions in the room_map array given to the build_type'x'
* function.
* xx, yy are the returned center of the allocated room in coordinates for
* cave_feat and cave_info etc.
*/

static bool room_alloc(int x,int y,bool crowded,int by0,int bx0,int *xx,int *yy)
{
	int temp,bx1,bx2,by1,by2,by,bx; 
	/*Calculate number of room_map squares to allocate*/ 

	/*temp is total number along width*/
	temp=((x-1)/BLOCK_WID)+1;
	
	/*bx2=ending block*/
	bx2=temp/2+bx0;
	/*bx1=starting block (Note: rounding taken care of here.)*/
	bx1=bx2+1-temp;


	/*temp is total number along height*/
	temp=((y-1)/BLOCK_HGT)+1;

	/*by2=ending block*/
	by2=temp/2+by0;
	/*by1=starting block*/
	by1=by2+1-temp;



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

	/* Acquire the location of the room */
	*yy = ((by1 + by2 + 1) * BLOCK_HGT) / 2;
	*xx = ((bx1 + bx2 + 1) * BLOCK_WID) / 2;


	/* Save the room location */
	if (dun->cent_n < CENT_MAX)
	{
		dun->cent[dun->cent_n].y = *yy;
		dun->cent[dun->cent_n].x = *xx;
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
	if (crowded) dun->crowded = TRUE;

	/*
	* Hack- See if room will cut off a cavern.
	* If so, fix by tunneling outside the room in such a way as to connect the caves.
	*/
	
	check_room_boundary(*xx-x/2-1,*yy-y/2-1,*xx+(x-1)/2+1,*yy+(y-1)/2+1);

	/* Success */
	return (TRUE);
}


/*
 * Room building routines.
 *
 * Eleven basic room types:
 *   1 -- normal
 *   2 -- overlapping
 *   3 -- cross shaped
 *   4 -- large room with features
 *   5 -- monster nests
 *   6 -- monster pits
 *   7 -- simple vaults
 *   8 -- greater vaults
 *   9 -- fractal cave system
 *  10 -- random vault
 *  11 -- circular room
 *  12 -- crypt
 */


/*
 * Type 1 -- normal rectangular rooms
 */
static void build_type1(int by0,int bx0)
{
	int y0,x0;
   
	int y, x;
	
	int y1, x1, y2, x2;
        
	int xsize, ysize;
	
	int light = FALSE;
   	
	/* Pick a room size */
	y1 =randint(4);
	x1 =randint(11);
	y2 =randint(3);
	x2 =randint(11);
	
	xsize=x1+x2+1;
	ysize=y1+y2+1;
	
	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(xsize+2,ysize+2,FALSE,by0,bx0,&x0,&y0)) return;

	/* Occasional light */
	if (p_ptr->depth <= randint(25)) light = TRUE;
   

	/* Get corner values */
	y1 = y0 - ysize/2;
	x1 = x0 - xsize/2;
	y2 = y0 + (ysize-1)/2;
	x2 = x0 + (xsize-1)/2;


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
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static void build_type2(int by0, int bx0)
{
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;

	int y0,x0;
   
	int light = FALSE;   		
	
	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,FALSE,by0,bx0,&x0,&y0))return;

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
static void build_type3(int by0, int bx0)
{
	int y0,x0;	
	
  
	int y, x;

	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;

	int dy, dx, wy, wx;

	int light = FALSE;
   	
	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,FALSE,by0,bx0,&x0,&y0))return;

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
	switch (rand_int(4))
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
static void build_type4(int by0, int bx0)
{
	int y0,x0;	
	
	
	int y, x, y1, x1, y2, x2;

	int light = FALSE;

   	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,FALSE,by0,bx0,&x0,&y0))return;

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
static void build_type5(int by0, int bx0)
{
	int y0,x0;		
	
	int y, x, y1, x1, y2, x2;

	int tmp, i;

	
	s16b what[64];

	cptr name;

	bool empty = FALSE;

	int light = FALSE;

	/*Try to allocate space for room.  If fails, exit*/
   	if (!room_alloc(25,11,TRUE,by0,bx0,&x0,&y0))return;
	
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
	tmp = randint(p_ptr->depth);

	/* Monster nest (jelly) */
	if (tmp < 30)
	{
		/* Describe */
		name = "jelly";

		/* Restrict to jelly */
		get_mon_num_hook = vault_aux_jelly;
	}

	/* Monster nest (animal) */
	else if (tmp < 50)
	{
		/* Describe */
		name = "animal";

		/* Restrict to animal */
		get_mon_num_hook = vault_aux_animal;
	}

	/* Monster nest (undead) */
	else
	{
		/* Describe */
		name = "undead";

		/* Restrict to undead */
		get_mon_num_hook = vault_aux_undead;
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


	/* Increase the level rating */
	rating += 10;

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
static void build_type6(int by0, int bx0)
{
	int y0,x0;	
	
	
	int tmp, what[16];

	int i, j, y, x, y1, x1, y2, x2;

	bool empty = FALSE;

	int light = FALSE;
	
	cptr name;

   	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,TRUE,by0,bx0,&x0,&y0))return;

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
	tmp = randint(p_ptr->depth);

	/* Orc pit */
	if (tmp < 20)
	{
		/* Message */
		name = "orc";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_orc;
	}

	/* Troll pit */
	else if (tmp < 40)
	{
		/* Message */
		name = "troll";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_troll;
	}

	/* Giant pit */
	else if (tmp < 60)
	{
		/* Message */
		name = "giant";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_giant;
	}

	/* Dragon pit */
	else if (tmp < 80)
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
			default:
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

		}

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
						place_object(y, x, FALSE, FALSE);
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
static void build_type7(int by0, int bx0)
{
	int y0,x0;
	vault_type *v_ptr;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Access a random vault record */
		v_ptr = &v_info[rand_int(MAX_V_IDX)];

		/* Accept the first lesser vault */
		if (v_ptr->typ == 7) break;
	}
	
   	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc( v_ptr->wid,v_ptr->hgt,FALSE,by0,bx0,&x0,&y0))return;
	
	/* Message */
	if (cheat_room) msg_print("Lesser Vault");

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
static void build_type8(int by0, int bx0)
{
	int y0,x0;
	vault_type *v_ptr;

	/* Pick a greater vault */
	while (TRUE)
	{
		/* Access a random vault record */
		v_ptr = &v_info[rand_int(MAX_V_IDX)];

		/* Accept the first greater vault */
		if (v_ptr->typ == 8) break;
	}
   	
	/*Try to allocate space for room.  If fails, exit*/
   	if (!room_alloc( v_ptr->wid,v_ptr->hgt,FALSE,by0,bx0,&x0,&y0))return;
	
	/* Message */
	if (cheat_room) msg_print("Greater Vault");

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


/* Store routine for the fractal cave generator */ 
/* this routine probably should be an inline function or a macro. */

static void store_height(int x,int y,int x0,int y0,byte val,int xhsize,int yhsize,int cutoff)
{
	/* only write to points that are "blank" */
	if (cave_feat[y0-yhsize+y][x0-xhsize+x]!=255) return;
	
	 /* if on boundary set val>cutoff so walls are not as square */
	if (((x==0)||(y==0)||(x==xhsize*2)||(y==yhsize*2))&&(val<=cutoff)) val=cutoff+1;

	/* store the value in height-map format */
	cave_feat[y0-yhsize+y][x0-xhsize+x]=val; 

	return;
}


/*
* Explanation of the plasma fractal algorithm:
*
* A grid of points is created with the properties of a 'height-map'
* This is done by making the corners of the grid have a random value.
* The grid is then subdivided into one with twice the resolution.
* The new points midway between two 'known' points can be calculated
* by taking the average value of the 'known' ones and randomly adding
* or subtracting an amount proportional to the distance between those
* points.  The final 'middle' points of the grid are then calculated 
* by averaging all four of the originally 'known' corner points.  An
* random amount is added or subtracted from this to get a value of the 
* height at that point.  The scaling factor here is adjusted to the
* slightly larger distance diagonally as compared to orthogonally. 
*
* This is then repeated recursively to fill an entire 'height-map'
* A rectangular map is done the same way, except there are different
* scaling factors along the x and y directions.
*
* A hack to change the amount of correlation between points is done using
* the grd variable.  If the current step size is greater than grd then
* the point will be random, otherwise it will be calculated by the
* above algorithm.  This makes a maximum distance at which two points on
* the height map can affect each other.
*
* How fractal caves are made:
*
* When the map is complete, a cut-off value is used to create a cave.
* Heights below this value are "floor", and heights above are "wall".
* This also can be used to create lakes, by adding more height levels
* representing shallow and deep water/ lava etc.
*
* The grd variable affects the width of passages.
* The roug variable affects the roughness of those passages
*
* The tricky part is making sure the created cave is connected.  This 
* is done by 'filling' from the inside and only keeping the 'filled'
* floor.  Walls bounding the 'filled' floor are also kept.  Everything
* else is converted to the normal granite FEAT_WALL_EXTRA.
*/


/*
*  Note that this uses the cave_feat array in a very hackish way
*  the values are first set to zero, and then each array location
*  is used as a "heightmap"
*  The heightmap then needs to be converted back into the "feat" format.
*
*  grd=level at which fractal turns on.  smaller gives more mazelike caves
*  roug=roughness level.  16=normal.  higher values make things more convoluted
*    small values are good for smooth walls.
*  size=length of the side of the square cave system.
*/

static void generate_hmap(int y0, int x0,int xsiz,int ysiz,int grd,int roug,int cutoff)
{		
	int xhsize,yhsize,xsize,ysize,maxsize; 
	
	/* fixed point variables- these are stored as 256 x normal value
	* this gives 8 binary places of fractional part + 8 places of normal part*/
	
	u16b xstep,xhstep,ystep,yhstep,i,j,diagsize,xxsize,yysize;
	
	/* redefine size so can change the value if out of range */
	xsize=xsiz; 
	ysize=ysiz;

	/* Paranoia about size of the system of caves*/
	if (xsize>254) xsize=254;
	if (xsize<4) xsize=4;
	if (ysize>254) ysize=254;
	if (ysize<4) ysize=4;

	/* get offsets to middle of array*/
	xhsize=xsize/2;
	yhsize=ysize/2;

	/* fix rounding problem */
	xsize=xhsize*2;
	ysize=yhsize*2;

	/*
	* Scale factor for middle points:
	* About sqrt(2)*256 - correct for a square lattice
	* approximately correct for everything else.
	*/
	
	diagsize=362; 

	/*maximum of xsize and ysize*/
	maxsize=(xsize>ysize)?xsize:ysize;	

	/* Clear the section */
	for(i=0;i<=xsize;i++)
		{
		for(j=0;j<=ysize;j++)
			{/*255 is a flag for "not done yet" */
			cave_feat[(int)(y0-yhsize+j)][(int)(x0-xhsize+i)]=255; 
			/* Clear icky flag because may be redoing the cave */
			cave_info[(int)(y0-yhsize+j)][(int)(x0-xhsize+i)]&= ~(CAVE_ICKY); 
			}
		}

	/* Set the corner values just in case grd>size. */

	store_height(0,0,x0,y0,maxsize,xhsize,yhsize,cutoff);
	store_height(0,ysize,x0,y0,maxsize,xhsize,yhsize,cutoff);
	store_height(xsize,0,x0,y0,maxsize,xhsize,yhsize,cutoff);
	store_height(xsize,ysize,x0,y0,maxsize,xhsize,yhsize,cutoff);

	/* Set the middle square to be an open area. */
	store_height(xhsize,yhsize,x0,y0,0,xhsize,yhsize,cutoff);

	
	/*initialize the step sizes*/
	xstep=xhstep=xsize*256;
	ystep=yhstep=ysize*256;
	xxsize=xsize*256;
	yysize=ysize*256;

	/* fill in the rectangle with fractal height data - like the 'plasma fractal' in
	*  fractint.
	*/
	while ((xstep/256>1)||(ystep/256>1))
	{
		/*halve the step sizes*/
		xstep=xhstep;
		xhstep/=2;
		ystep=yhstep;
		yhstep/=2;				
		
		/*middle top to bottom.*/
		for(i=xhstep;i<=xxsize-xhstep;i+=xstep)  
		{
			for(j=0;j<=yysize;j+=ystep)
			{					
				if (xhstep/256>grd)
				{
					/* If greater than 'grid' level then is random */
					store_height(i/256,j/256,x0,y0,randint(maxsize),xhsize,yhsize,cutoff);
				}
			   	else 
				{
					/* Average of left and right points +random bit */
					store_height(i/256,j/256,x0,y0,
					(cave_feat[y0-yhsize+j/256][x0-xhsize+(i-xhstep)/256]
					+cave_feat[y0-yhsize+j/256][x0-xhsize+(i+xhstep)/256])/2
					+(randint(xstep/256)-xhstep/256)*roug/16,xhsize,yhsize,cutoff);
				}
			}
		}
		 
		 
		/*middle left to right.*/
		for(j=yhstep;j<=yysize-yhstep;j+=ystep) 
		{			
			for(i=0;i<=xxsize;i+=xstep)
		   	{
				if (xhstep/256>grd)
				{
					/* If greater than 'grid' level then is random */
					store_height(i/256,j/256,x0,y0,randint(maxsize),xhsize,yhsize,cutoff);
				}
		   		else 
				{
					/* Average of up and down points +random bit */
					store_height(i/256,j/256,x0,y0,
					(cave_feat[y0-yhsize+(j-yhstep)/256][x0-xhsize+i/256]
					+cave_feat[y0-yhsize+(j+yhstep)/256][x0-xhsize+i/256])/2
					+(randint(ystep/256)-yhstep/256)*roug/16,xhsize,yhsize,cutoff);
				}
			}
		}		
		
		/*center.*/
		for(i=xhstep;i<=xxsize-xhstep;i+=xstep)
		{
			for(j=yhstep;j<=yysize-yhstep;j+=ystep)  
			{
			   	if (xhstep/256>grd)
				{
					/* If greater than 'grid' level then is random */
					store_height(i/256,j/256,x0,y0,randint(maxsize),xhsize,yhsize,cutoff);			
				}
		   		else 
				{
				/* average over all four corners + scale by diagsize to 
				*reduce the effect of the square grid on the shape of the fractal */
				store_height(i/256,j/256,x0,y0,
				(cave_feat[y0-yhsize+(j-yhstep)/256][x0-xhsize+(i-xhstep)/256]
				+cave_feat[y0-yhsize+(j+yhstep)/256][x0-xhsize+(i-xhstep)/256]
				+cave_feat[y0-yhsize+(j-yhstep)/256][x0-xhsize+(i+xhstep)/256]
				+cave_feat[y0-yhsize+(j+yhstep)/256][x0-xhsize+(i+xhstep)/256])/4
				+(randint(xstep/256)-xhstep/256)*(diagsize/16)/256*roug,xhsize,yhsize,cutoff);
				}
			}
		}
	}
}

static bool hack_isnt_wall(int y, int x, int cutoff)
{
/* function used to convert from height-map back to the
*  normal angband cave format*/
	if (cave_info[y][x]&CAVE_ICKY) 
	{
		/* already done */
		return FALSE;	
	}
	else
	{
		/* Show that have looked at this square */
		cave_info[y][x]|= (CAVE_ICKY); 
		/*if less than cutoff then is a floor */
		if(cave_feat[y][x]<=cutoff) 
		{
			cave_feat[y][x]=FEAT_FLOOR; 
			return TRUE;
		}
		/*if greater than cutoff then is a wall */
		else
		{
			cave_feat[y][x]=FEAT_WALL_OUTER; 
			return FALSE;
		}
	}
}

static void fill_hack(int y0, int x0,int y,int x,int xsize,int ysize,int cutoff,int *amount)
/* Quick and nasty fill routine used to find the connected region of floor
*in the middle of the cave*/
{
	int i,j;
	/* check 8 neighbours +self (self is caught in the isnt_wall function) */
	for (i=-1;i<=1;i++) 
	{
		for (j=-1;j<=1;j++) 
		{
			/* If within bounds */
			if ((x+i>0)&&(x+i<xsize)&&(y+j>0)&&(y+j<ysize)) 
			{
				/*If not a wall or floor done before */
				if (hack_isnt_wall(y+j+y0-ysize/2,x+i+x0-xsize/2,cutoff))			
		 		{
					/* then fill from the new point*/
					fill_hack(y0,x0,y+j,x+i,xsize,ysize,cutoff,amount); 
					
					/* keep tally of size of cave system */
					(*amount)++; 
				}
			}			
			else 	
			{
				/* affect boundary */
				cave_info[y0+y+j-ysize/2][x0+x+i-xsize/2]|=CAVE_ICKY;
			}
		}
	}
}


static bool generate_fracave(int y0, int x0,int xsize,int ysize,int cutoff,bool light,bool room)
{
	int x,y,i,amount,xhsize,yhsize;
	
	/*offsets to middle from corner*/
	xhsize=xsize/2;
	yhsize=ysize/2;
	
	/* tally=0 */
	amount=0; 
	
	/*select region connected to center of cave system
	* this gets rid of alot of isolated one-sqaures that
	* can make teleport traps instadeaths... */
	fill_hack(y0,x0,yhsize,xhsize,xsize,ysize,cutoff,&amount);
	
	/* if tally too small, try again*/
	if (amount<10) 
	{
		/*too small -clear area and try again later*/
		for(x=0;x<=xsize;++x)
		{
			for(y=0;y<ysize;++y)
			{
				cave_feat[y0+y-yhsize][x0+x-xhsize]=FEAT_WALL_EXTRA;			
				cave_info[y0+y-yhsize][x0+x-xhsize]&= ~(CAVE_ICKY|CAVE_ROOM);
				cave_info[y0+y-yhsize][x0+x-xhsize]|=(CAVE_WALL);	 
			}
		}
		return FALSE;
	}

	/*Do boundarys- check to see if they are next to a filled region 
	*If not then they are set to normal granite
	*If so then they are marked as room walls. */
	for(i=0;i<=xsize;++i)
		{
			/* top boundary */
			if ((cave_info[0+y0-yhsize][i+x0-xhsize]&CAVE_ICKY)&&(room))		
			{
				/*Next to a 'filled' region?-set to be room walls */
				cave_info[y0+0-yhsize][x0+i-xhsize]|=(CAVE_WALL);
				if (light) cave_info[y0+0-yhsize][x0+i-xhsize]|=(CAVE_GLOW);
				cave_info[y0+0-yhsize][x0+i-xhsize]|=(CAVE_ROOM);
				cave_feat[y0+0-yhsize][x0+i-xhsize]=FEAT_WALL_OUTER;
			}
			else 
			{
				/*set to be normal granite*/
				cave_feat[y0+0-yhsize][x0+i-xhsize]=FEAT_WALL_EXTRA;
				cave_info[y0+0-yhsize][x0+i-xhsize]|=(CAVE_WALL);
			}
			
			/* bottom boundary */
			if ((cave_info[ysize+y0-yhsize][i+x0-xhsize]&CAVE_ICKY)&&(room))
			{
				/*Next to a 'filled' region?-set to be room walls */
				
				cave_info[y0+ysize-yhsize][x0+i-xhsize]|=(CAVE_WALL);
				if (light) cave_info[y0+ysize-yhsize][x0+i-xhsize]|=(CAVE_GLOW);
				cave_info[y0+ysize-yhsize][x0+i-xhsize]|=(CAVE_ROOM);
				cave_feat[y0+ysize-yhsize][x0+i-xhsize]=FEAT_WALL_OUTER;
			}
			else
			{
				/*set to be normal granite*/
				cave_feat[y0+ysize-yhsize][x0+i-xhsize]=FEAT_WALL_EXTRA;
				cave_info[y0+ysize-yhsize][x0+i-xhsize]|=(CAVE_WALL);
			}				
		
		/* clear the icky flag-don't need it any more */
		
		cave_info[y0+0-yhsize][x0+i-xhsize]&= ~(CAVE_ICKY);
		cave_info[y0+ysize-yhsize][x0+i-xhsize]&= ~(CAVE_ICKY);
		}

	/* Do the left and right boundaries minus the corners (done above) */

	for(i=1;i<ysize;++i)
	{
		/* left boundary */
		if ((cave_info[i+y0-yhsize][0+x0-xhsize]&CAVE_ICKY)&&(room))
		{
			/* room boundary */
			cave_info[y0+i-yhsize][x0+0-xhsize]|=(CAVE_WALL);
			if (light) cave_info[y0+i-yhsize][x0+0-xhsize]|=(CAVE_GLOW);
			cave_info[y0+i-yhsize][x0+0-xhsize]|=(CAVE_ROOM);
			cave_feat[y0+i-yhsize][x0+0-xhsize]=FEAT_WALL_OUTER;
		}
		else 
		{
			/* outside room */
			cave_feat[y0+i-yhsize][x0+0-xhsize]=FEAT_WALL_EXTRA;
			cave_info[y0+i-yhsize][x0+0-xhsize]|=(CAVE_WALL);
		}
		/* right boundary */
		if ((cave_info[i+y0-yhsize][xsize+x0-xhsize]&CAVE_ICKY)&&(room))
		{
			/* room boundary */
			cave_info[y0+i-yhsize][x0+xsize-xhsize]|=(CAVE_WALL);
			if (light) cave_info[y0+i-yhsize][x0+xsize-xhsize]|=(CAVE_GLOW);
			cave_info[y0+i-yhsize][x0+xsize-xhsize]|=(CAVE_ROOM);
			cave_feat[y0+i-yhsize][x0+xsize-xhsize]=FEAT_WALL_OUTER;
		}
		else
		{
			/* outside room */
			cave_feat[y0+i-yhsize][x0+xsize-xhsize]=FEAT_WALL_EXTRA;
			cave_info[y0+i-yhsize][x0+xsize-xhsize]|=(CAVE_WALL);
		}
		
		/* clear icky flag -done with it */
		cave_info[y0+i-yhsize][x0+0-xhsize]&= ~(CAVE_ICKY);
		cave_info[y0+i-yhsize][x0+xsize-xhsize]&= ~(CAVE_ICKY);
	}


	/* Do the rest: convert back to the normal format*
	* In other variants, may want to check to see if cave_feat< some value
	* if so, set to be water:- this will make interesting pools etc.
	* (I don't do this for standard angband.)
	*/
	for(x=1;x<xsize;++x)
	{	
		for(y=1;y<ysize;++y)
		{			
			if ((cave_feat[y0+y-yhsize][x0+x-xhsize]==FEAT_FLOOR)
				&&(cave_info[y0+y-yhsize][x0+x-xhsize]&CAVE_ICKY))
				
			{
				/*Clear the icky flag in the filled region*/
				cave_info[y0+y-yhsize][x0+x-xhsize]&= ~(CAVE_ICKY|CAVE_WALL);
				/*Set appropriate flags */				
				if (light) cave_info[y0+y-yhsize][x0+x-xhsize]|=(CAVE_GLOW);
				if (room)cave_info[y0+y-yhsize][x0+x-xhsize]|=(CAVE_ROOM);
			}
			else if ((cave_feat[y0+y-yhsize][x0+x-xhsize]==FEAT_WALL_OUTER)
				&&(cave_info[y0+y-yhsize][x0+x-xhsize]&CAVE_ICKY))
			{
				/*Walls */
				cave_info[y0+y-yhsize][x0+x-xhsize]&= ~(CAVE_ICKY);
				cave_info[y0+y-yhsize][x0+x-xhsize]|=(CAVE_WALL);
				if (light) cave_info[y0+y-yhsize][x0+x-xhsize]|=(CAVE_GLOW);
				if (room)
				{ 
					cave_info[y0+y-yhsize][x0+x-xhsize]|=(CAVE_ROOM);
				}
				else
				{
					cave_feat[y0+y-yhsize][x0+x-xhsize]=FEAT_WALL_EXTRA;
				}
			}
			else 
			{
				/*Clear the unconnected regions*/
				cave_feat[y0+y-yhsize][x0+x-xhsize]=FEAT_WALL_EXTRA;		
				cave_info[y0+y-yhsize][x0+x-xhsize]&= ~(CAVE_ICKY|CAVE_ROOM);
				cave_info[y0+y-yhsize][x0+x-xhsize]|=(CAVE_WALL);
			}
		}
	}

	/* XXX XXX XXX There is a slight problem when tunnels pierce the caves:
	* Extra doors appear inside the system.  (Its not very noticeable though.)
	* This can be removed by "filling" from the outside in.  This allows a separation
	* from FEAT_WALL_OUTER with FEAT_WALL_INNER.  (Internal walls are  F.W.OUTER instead.)
	* The extra effort for what seems to be only a minor thing (even non-existant if you
	* think of the caves not as normal rooms, but as holes in the dungeon), doesn't seem
	* worth it.*/

	return TRUE;
}


/* Driver routine to create fractal cave system */

static void build_type9(int by0, int bx0)
{
	int grd,roug,cutoff,xsize,ysize,y0,x0;

	bool done,light,room;

	/*get size: note 'Evenness'*/
	xsize=randint(22)*2+6; 
	ysize=randint(15)*2+6;	

	/*Try to allocate space for room.  If fails, exit*/
	if (room_alloc(xsize+1,ysize+1,FALSE,by0,bx0,&x0,&y0)==FALSE)return;

	light=done=FALSE;
	room=TRUE;
	
	if (p_ptr->depth <= randint(25)) light = TRUE;
	
	while (!done)
	{
		/* Note: size must be even or there are rounding problems 
		* This causes the tunnels not to connect properly to the room */	 
		
		/* testing values for these parameters feel free to adjust*/
		grd=1<<(randint(4)); 
		
		/*want average of about 16 */
		roug=randint(8)*randint(4); 
		
		/* about size/2 */
		cutoff=randint(xsize/4)+randint(ysize/4)+randint(xsize/4)+randint(ysize/4); 
		
		/* make it */
		generate_hmap(y0,x0,xsize,ysize,grd,roug,cutoff);
		
		/* Convert to normal format+ clean up*/
		done=generate_fracave(y0, x0,xsize,ysize,cutoff,light,room);
	}
}


/*makes a cave system in the center of the dungeon */

static void build_cavern(void)
{
	int grd,roug,cutoff,xsize,ysize,x0,y0;
	bool done,light,room;
	
	light=done=room=FALSE;
	if (p_ptr->depth <= randint(25)) light = TRUE;
	
	/* Make a cave the size of the dungeon */
	xsize=DUNGEON_WID-1;
	ysize=DUNGEON_HGT-1;
	x0=xsize/2;
	y0=ysize/2;

	/* Paranoia: make size even */
	xsize=x0*2;
	ysize=y0*2; 

	while (!done)
	{
		/* testing values for these parameters: feel free to adjust*/
		grd=1<<(randint(4)+4);
		
		/*want average of about 16 */
		roug=randint(8)*randint(4); 
		
		/* about size/2 */
		cutoff=xsize/2; 
		
		 /* make it */
		generate_hmap(y0,x0,xsize,ysize,grd,roug,cutoff);
				 
		/* Convert to normal format+ clean up*/
		done=generate_fracave(y0,x0,xsize,ysize,cutoff,light,room);
	}
}

/* Routine used by the random vault creators to add a door to a location
* Note that range checking has to be done in the calling routine.
*  The doors must be INSIDE the allocated region*/

static void add_door(int x,int y)

{
	/*Need to have a wall in the center square*/
	if (cave_feat[y][x]!=FEAT_WALL_OUTER) return; 

	/* look at:
	*  x#x
	*  .#.
	*  x#x
	*
	*  where x=don't care
	*  .=floor, #=wall
	*/

	if ((cave_feat[y-1][x]==FEAT_FLOOR)&&(cave_feat[y+1][x]==FEAT_FLOOR)
		&&(cave_feat[y][x-1]==FEAT_WALL_OUTER)&&(cave_feat[y][x+1]==FEAT_WALL_OUTER))
	{
		/*secret door*/
		place_secret_door(y,x);
	
		/*set boundarys so don't get wide doors*/
		cave_feat[y][x-1]=FEAT_WALL_SOLID;
		cave_feat[y][x+1]=FEAT_WALL_SOLID;
	}

	
	/* look at:
	*  x#x
	*  .#.
	*  x#x
	*
	*  where x=don't care
	*  .=floor, #=wall
	*/

	if ((cave_feat[y-1][x]==FEAT_WALL_OUTER)&&(cave_feat[y+1][x]==FEAT_WALL_OUTER)
		&&(cave_feat[y][x-1]==FEAT_FLOOR)&&(cave_feat[y][x+1]==FEAT_FLOOR))
	{
		/*secret door*/
		place_secret_door(y,x);
	
		/*set boundarys so don't get wide doors*/
		cave_feat[y-1][x]=FEAT_WALL_SOLID;
		cave_feat[y+1][x]=FEAT_WALL_SOLID;
	}
}


/* Routine that fills the empty areas of a room with treasure and monsters */

static void fill_treasure(int x1,int x2,int y1,int y2,int difficulty)
{
	int x,y,cx,cy,size;
	s32b value;
	
	/* center of room:*/
	cx=(x1+x2)/2;
	cy=(y1+y2)/2;

	/* Rough measure of size of vault= sum of lengths of sides*/
	size=abs(x2-x1)+abs(y2-y1);

	for (x=x1;x<=x2;x++)
	{		
		for(y=y1;y<=y2;y++)
		{
 			/* Thing added based on distance to center of vault
			* Difficulty is 1-easy to 10-hard*/
			value=((((s32b)(distance(cx,cy,x,y)))*100)/size)+randint(10)-difficulty;
		
			/*hack- empty square part of the time*/
			if (randint(100)-difficulty*3>50) value=20;
		
			 /* only on floor*/
			if (cave_feat[y][x]==FEAT_FLOOR)
			{
				/* The smaller 'value' is, the better the stuff*/
				if (value<0) 
				{
					/*Meanest monster + treasure*/
					monster_level = p_ptr->depth + 40;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					object_level = p_ptr->depth + 20;
					place_object(y, x, TRUE, FALSE);
					object_level = p_ptr->depth;				
				}
				else if(value<5)
				{
					/*Mean monster +treasure*/
					monster_level = p_ptr->depth + 20;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					object_level = p_ptr->depth + 10;
					place_object(y, x, TRUE, FALSE);
					object_level = p_ptr->depth;
				}
				else if(value<10)
				{
					/*Monster*/
					monster_level = p_ptr->depth + 9;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;				
				}
				else if(value<17)
				{
					/* Intentional Blank space */
					
					/*(Want some of the vault to be empty 
					* so have room for group monsters
					* +this is used in the hack above to lower
					* the density of stuff in the vault)*/
				}
				else if(value<23)
				{
					/*Object or trap*/
					if (rand_int(100) < 25)
					{
						place_object(y, x, FALSE, FALSE);
					}
					else
					{
						place_trap(y, x);
					}
				}
				else if(value<30)
				{
					/*Monster and trap*/
					monster_level = p_ptr->depth + 5;
					place_monster(y, x, TRUE, TRUE);
					monster_level = p_ptr->depth;
					place_trap(y, x);
				}
				else if(value<40) 
				{
					/*Monster or object*/
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
				}
				else if(value<50) 
				{
					/*Trap*/
					place_trap(y, x);
				}
				else 
				{
					/*Various Stuff*/
					
					/*20% monster,40% trap, 20% object, 20% blank space*/
					if (rand_int(100)<20) 
					{
						place_monster(y, x, TRUE, TRUE);
					}
					else if(rand_int(100)<50)
					{
						place_trap(y,x);
					}
					else if(rand_int(100)<50)
					{
						place_object(y,x,FALSE,FALSE);
					}
				}
				
			}
		}
	}
}

/*
* This function creates a random vault that looks like a collection of bubbles.
* It works by getting a set of corrdinates that represent the center of each
* bubble.  The entire room is made by seeing which bubble center is closest. If  
* two centers are equidistant then the square is a wall, otherwise it is a floor.
* The only exception is for squares really near a center, these are always floor.
* (It looks better than without this check.)
*
* Note: If two centers are on the same point then this algorithm will create a
*       blank bubble filled with walls. - This is prevented from happening.  
*/


static void build_bubble_vault(int x0,int y0,int xsize,int ysize)
{

	#define BUBBLENUM 10		/*number of bubbles*/

	/* array of center points of bubbles*/
	coord center[BUBBLENUM];

	int i,j,x,y,xhsize,yhsize;
	u16b min1,min2,temp;
	bool done;

	/* Offset from center to top left hand corner*/
	xhsize=xsize/2;
	yhsize=ysize/2;
	
	if (cheat_room) msg_print("Bubble Vault");

	/* Allocate center of bubbles*/
	center[0].x=randint(xsize-2)+1;
	center[0].y=randint(ysize-2)+1;
	for (i=1;i<BUBBLENUM;i++)
	{
		done=FALSE;
		/*get center and check to see if it is unique*/
		while (!done)
		{
			done=TRUE;
			x=randint(xsize-2)+1;
			y=randint(ysize-2)+1;
			for (j=0;j<i;j++);
			{
				/*rough test to see if there is an overlap*/
				if ((x==center[j].x)||(y==center[j].y)) done=FALSE;												
			}
		}
		center[i].x=x;
		center[i].y=y;
	}


	/* Top and bottom boundaries*/
	for (i=0;i<=xsize;i++)
	{
		cave_feat[y0-yhsize+0][x0-xhsize+i]=FEAT_WALL_OUTER;
		cave_info[y0-yhsize+0][x0-xhsize+i]|=(CAVE_ROOM|CAVE_ICKY|CAVE_WALL);
		cave_feat[y0-yhsize+ysize][x0-xhsize+i]=FEAT_WALL_OUTER;
		cave_info[y0-yhsize+ysize][x0-xhsize+i]|=(CAVE_ROOM|CAVE_ICKY|CAVE_WALL);
	}
	
	/*Left and right boundaries*/
	for (i=1;i<ysize;i++)
	{
		cave_feat[y0-yhsize+i][x0-xhsize+0]=FEAT_WALL_OUTER;
		cave_info[y0-yhsize+i][x0-xhsize+0]|=(CAVE_ROOM|CAVE_ICKY|CAVE_WALL);
		cave_feat[y0-yhsize+i][x0-xhsize+xsize]=FEAT_WALL_OUTER;
		cave_info[y0-yhsize+i][x0-xhsize+xsize]|=(CAVE_ROOM|CAVE_ICKY|CAVE_WALL);
	}

	/* Fill in middle with bubbles*/
	for (x=1;x<xsize;x++)
	{
		for(y=1;y<ysize;y++)
		{
			/*Get distances to two closest centers*/
		
			/*initialize*/
			min1=distance(x,y,center[0].x,center[0].y);
			min2=distance(x,y,center[1].x,center[1].y);
			if (min1>min2)
			{
				/*swap if in wrong order*/
				temp=min1;
				min1=min2;
				min2=temp;
			}
			
			/*Scan the rest*/
			for (i=2;i<BUBBLENUM;i++)
			{
				temp=distance(x,y,center[i].x,center[i].y);
				if (temp<min1)
				{
					/*smallest*/
					min2=min1;
					min1=temp;				
				}
				else if(temp<min2)
				{
					/*second smallest*/
				 	min2=temp;
				}
			}		
			if (((min2-min1)<=2)&&(!(min1<3)))
			{
				/*Boundary at midpoint+ not at inner region of bubble*/
				cave_feat[y0-yhsize+y][x0-xhsize+x]=FEAT_WALL_OUTER;
				cave_info[y0-yhsize+y][x0-xhsize+x]|=CAVE_WALL;
			}
			else
			{	
				/*middle of a bubble*/
				cave_feat[y0-yhsize+y][x0-xhsize+x]=FEAT_FLOOR;
				cave_info[y0-yhsize+y][x0-xhsize+x]&=(~CAVE_WALL);
			}
			
			/*clean up rest of flags*/
			cave_info[y0-yhsize+y][x0-xhsize+x]|=(CAVE_ROOM|CAVE_ICKY);
		}
		
	}

	/*Try to add some random doors*/
	for (i=0;i<500;i++)
	{
		x=randint(xsize-2)-xhsize+x0+1;
		y=randint(ysize-2)-yhsize+y0+1;
		add_door(x,y);
	}
	/*Fill with monsters and treasure, low difficulty*/
	fill_treasure(x0-xhsize+1,x0-xhsize+xsize-1,y0-yhsize+1,y0-yhsize+ysize-1,randint(5));
}


/* Overlay a rectangular room given its bounds
*This routine is used by build_room_vault
*The area inside the walls is not touched:
*only granite is removed- normal walls stay*/

static void build_room(int x1,int x2,int y1,int y2)
{
	int x,y,i,xsize,ysize,temp;
	
	/*Check if rectangle has no width*/
	if ((x1==x2)||(y1==y2)) return;	
	
	/*initialize*/
	if (x1>x2) 
	{
		/*Swap boundaries if in wrong order*/
		temp=x1;
		x1=x2;
		x2=temp;
	}	
	
	if (y1>y2) 
	{
		/*Swap boundaries if in wrong order*/
		temp=y1;
		y1=y2;
		y2=temp;
	}

	/* get total widths */
	xsize=x2-x1;
	ysize=y2-y1;


	/* Top and bottom boundaries*/
	for (i=0;i<=xsize;i++)
	{
		cave_feat[y1][x1+i]=FEAT_WALL_OUTER;
		cave_info[y1][x1+i]|=(CAVE_ROOM|CAVE_ICKY|CAVE_WALL);
		cave_feat[y2][x1+i]=FEAT_WALL_OUTER;
		cave_info[y2][x1+i]|=(CAVE_ROOM|CAVE_ICKY|CAVE_WALL);
	}
	
	/*Left and right boundaries*/
	for (i=1;i<ysize;i++)
	{
		cave_feat[y1+i][x1]=FEAT_WALL_OUTER;
		cave_info[y1+i][x1]|=(CAVE_ROOM|CAVE_ICKY|CAVE_WALL);
		cave_feat[y1+i][x2]=FEAT_WALL_OUTER;
		cave_info[y1+i][x2]|=(CAVE_ROOM|CAVE_ICKY|CAVE_WALL);
	}
	
	/* Middle*/
	for (x=1;x<xsize;x++)
	{
		for(y=1;y<ysize;y++)
		{
			if(cave_feat[y1+y][x1+x]==FEAT_WALL_EXTRA)
			{
				/*clear the untouched region*/
				cave_feat[y1+y][x1+x]=FEAT_FLOOR;
				cave_info[y1+y][x1+x]|=(CAVE_ROOM|CAVE_ICKY);
				cave_info[y1+y][x1+x]&=(~CAVE_WALL);			
			}
			else 
			{
				/*make it a room- but don't touch*/
				cave_info[y1+y][x1+x]|=(CAVE_ROOM|CAVE_ICKY);
			}
		}
	}
}


/* Create a random vault that looks like a collection of overlapping rooms*/

static void build_room_vault(int x0,int y0,int xsize,int ysize)
{
	int i,x1,x2,y1,y2,xhsize,yhsize;
	
	/*get offset from center*/
	xhsize=xsize/2;
	yhsize=ysize/2;
	
	if (cheat_room) msg_print("Room Vault");
	
	/*fill area so don't get problems with cavern levels*/
	for(x1=0;x1<=xsize;x1++)
	{
		for(y1=0;y1<=ysize;y1++)
		{
			cave_feat[y0-yhsize+y1][x0-xhsize+x1]=FEAT_WALL_EXTRA;
			cave_info[y0-yhsize+y1][x0-xhsize+x1]&=(~CAVE_ICKY);					
			cave_info[y0-yhsize+y1][x0-xhsize+x1]|=(CAVE_WALL);
		}
	}

	/*add ten random rooms*/
	for (i=0;i<10;i++)
	{
		x1=randint(xhsize)*2+x0-xhsize;
		x2=randint(xhsize)*2+x0-xhsize;
		y1=randint(yhsize)*2+y0-yhsize;
		y2=randint(yhsize)*2+y0-yhsize;
		build_room(x1,x2,y1,y2);
	}
	
	/*Add some random doors*/
	for (i=0;i<500;i++)
	{
		x1=randint(xsize-2)-xhsize+x0+1;
		y1=randint(ysize-2)-yhsize+y0+1;
		add_door(x1,y1);
	}
	
	/*Fill with monsters and treasure, high difficulty*/
	fill_treasure(x0-xhsize+1,x0-xhsize+xsize-1,y0-yhsize+1,y0-yhsize+ysize-1,randint(5)+5);
}


/* Create a random vault out of a fractal cave*/

static void build_cave_vault(int x0,int y0,int xsiz,int ysiz)
{
	int grd,roug,cutoff,xhsize,yhsize,xsize,ysize,x,y;
	bool done,light,room;

	/*round to make sizes even*/
	xhsize=xsiz/2;
	yhsize=ysiz/2;
	xsize=xhsize*2;
	ysize=yhsize*2;
	
	if (cheat_room) msg_print("Cave Vault");

	light=done=FALSE;
	room=TRUE;

	while (!done)
	{
		/* testing values for these parameters feel free to adjust*/
		grd=1<<(randint(4)-1); 
		
		/*want average of about 16 */
		roug=randint(8)*randint(4); 
		
		/* about size/2 */
		cutoff=randint(xsize/4)+randint(ysize/4)+randint(xsize/4)+randint(ysize/4); 
		
		/* make it */
		generate_hmap(y0,x0,xsize,ysize,grd,roug,cutoff); 
		
		/* Convert to normal format+ clean up*/
		done=generate_fracave(y0, x0,xsize,ysize,cutoff,light,room); 
	}
	
	/*Set icky flag because is a vault*/
	for (x=0;x<=xsize;x++)
		{
		for(y=0;y<=ysize;y++)
			{
			cave_info[y0-yhsize+y][x0-xhsize+x]|=CAVE_ICKY;
			}
		}
		
	/*Fill with monsters and treasure, low difficulty*/
	fill_treasure(x0-xhsize+1,x0-xhsize+xsize-1,y0-yhsize+1,y0-yhsize+ysize-1,randint(5));
}

/*
 * maze vault -- rectangular labyrinthine rooms
 *
 * maze vault uses two routines: 
 *    r_visit - a recursive routine that builds the labyrinth
 *    build_maze_vault - a driver routine that calls r_visit and adds
 *                  monsters, traps and treasure  
 *
 * The labyrinth is built by creating a spanning tree of a graph.
 * The graph vertices are at 
 *    (x,y) = (2j+x1,2k+y1)   j = 0,...,m-1    k = 0,...,n-1
 * and the edges are the vertical and horizontal nearest neighbors.
 *
 * The spanning tree is created by performing a suitably randomized
 * depth-first traversal of the graph. The only adjustable parameter
 * is the rand_int(3) below; it governs the relative density of
 * twists and turns in the labyrinth: smaller number, more twists.
 */
 static void r_visit(int y1, int x1, int y2, int x2, 
                     int node, int dir, int *visited)
 {
         int i, j, m, n, temp, x, y, adj[4];
 
         /* dimensions of vertex array */
         m = (x2-x1)/2 + 1;
         n = (y2-y1)/2 + 1;
 
         /* mark node visited and set it to a floor */
         visited[node] = 1;
         x = 2*(node % m) + x1;
         y = 2*(node / m) + y1;
         cave_set_feat( y, x, FEAT_FLOOR); 
 
         /* setup order of adjacent node visits */
         if( rand_int(3) == 0) { /* pick a random ordering */
                 for( i = 0; i < 4; i++)
                         adj[i] = i;
                 for( i = 0; i < 4; i++){
                         j = rand_int(4);
                         temp = adj[i];
                         adj[i] = adj[j];
                         adj[j] = temp;         
                 }
                 dir = adj[0];
         }
         else { /* pick a random ordering with dir first */
                 adj[0] = dir;
                 for( i = 1; i < 4; i++)
                         adj[i] = i;
                 for( i = 1; i < 4; i++){
                         j = 1 + rand_int(3);
                         temp = adj[i];
                         adj[i] = adj[j];
                         adj[j] = temp;         
                 }      
         }
         for( i = 0; i < 4; i++) {
                 switch (adj[i]) {
                 case 0:
                         /* (0,+) - check for bottom boundary */
                         if( node / m < n-1 && visited[node+m] == 0 ) {
                                 cave_set_feat( y+1, x, FEAT_FLOOR); 
                                 r_visit( y1, x1, y2, x2, node+m, dir, visited);
                         }
                         break;
                 case 1:
                         /* (0,-) - check for top boundary */
                         if( node / m > 0 && visited[node-m] == 0 ) {
                                 cave_set_feat( y-1, x, FEAT_FLOOR); 
                                 r_visit( y1, x1, y2, x2, node-m, dir, visited);
                         }
                         break;
                 case 2:
                         /* (+,0) - check for right boundary */
                         if( node % m < m-1 && visited[node+1] == 0 ) {
                                 cave_set_feat( y, x+1, FEAT_FLOOR); 
                                 r_visit( y1, x1, y2, x2, node+1, dir, visited);
                         }
                         break;
                 case 3:
                         /* (-,0) - check for left boundary */
                         if( node % m > 0 && visited[node-1] == 0 ) {
                                 cave_set_feat( y, x-1, FEAT_FLOOR); 
                                 r_visit( y1, x1, y2, x2, node-1, dir, visited);
                         }
                 } /* end switch */
         }
 }
  
 static void build_maze_vault(int x0,int y0,int xsize,int ysize)
 {
 	int dy, dx;
 	int y1, x1, y2, x2, y, x;
        int i, m, n, num_vertices, *visited;
 	int light = FALSE;
 
 	if (cheat_room) msg_print("Maze Vault");
 
 	/* Occasional light */
 	if (p_ptr->depth <= randint(25)) light = TRUE;
 
 	/* Pick a random room size */
    dy = ysize/2-1;
    dx = xsize/2-1;
 	
	y1 = y0 - dy;
 	x1 = x0 - dx;
 	y2 = y0 + dy;
 	x2 = x0 + dx;
 	
	
	/* generate the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{			
			cave_info[y][x] |= (CAVE_ROOM|CAVE_WALL|CAVE_ICKY);
			if ((x==x1-1)||(x==x2+1)||(y==y1-1)||(y==y2+1))
				cave_feat[y][x] =FEAT_WALL_OUTER;
			else
				cave_feat[y][x] =FEAT_WALL_INNER;
			if (light) cave_info[y][x] |= (CAVE_GLOW);
		}
	}	
 	
 
    /* dimensions of vertex array */
    m = dx + 1;
    n = dy + 1;
    num_vertices = m * n;
 
    /* initialize array of visited vertices */
    /* use ralloc here ? */
    visited = (int *) malloc( num_vertices * sizeof(int));
    for( i = 0; i < num_vertices; i++)
        visited[i] = 0;
         
    /* traverse the graph to create a spannng tree, pick a random root */
    r_visit( y1, x1, y2, x2, rand_int( num_vertices), 0, visited);
 
    /*Fill with monsters and treasure, low difficulty*/
	fill_treasure(x1,x2,y1,y2,randint(5));
 
    /* rnfree( visited,num_vertices * sizeof(int));*/
	free(visited);
 }

/* Build a "mini" checkerboard vault
*
* This is done by making a permanent wall maze and setting
* the diagonal sqaures of the checker board to be granite.
* The vault has two entrances on opposite sides to guarantee
* a way to get in even if the vault abuts a side of the dungeon. 
*/

static void build_mini_c_vault(int x0,int y0,int xsize,int ysize)
 {
 	int dy, dx;
 	int y1, x1, y2, x2, y, x, total;
    int i, m, n, num_vertices, *visited;
 
 	if (cheat_room) msg_print("Mini Checker Board Vault");
 
 	/* Pick a random room size */
    dy = ysize/2-1;
    dx = xsize/2-1;
 	
	y1 = y0 - dy;
 	x1 = x0 - dx;
 	y2 = y0 + dy;
 	x2 = x0 + dx;
 	
	
	/* generate the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{			
			cave_info[y][x] |= (CAVE_ROOM|CAVE_WALL|CAVE_ICKY);
			
			/* Permanent walls */
			cave_feat[y][x] =FEAT_PERM_INNER;
		}
	}	
 	
 
    /* dimensions of vertex array */
    m = dx + 1;
    n = dy + 1;
    num_vertices = m * n;
 
    /* initialize array of visited vertices */
    /* use ralloc here ? */
    visited = (int *) malloc( num_vertices * sizeof(int));
    for( i = 0; i < num_vertices; i++)
          visited[i] = 0;
         
    /* traverse the graph to create a spannng tree, pick a random root */
    r_visit( y1, x1, y2, x2, rand_int( num_vertices), 0, visited);
	
	/*Make it look like a checker board vault*/
	for(x=x1;x<=x2;x++)
	{
		for(y=y1;y<=y2;y++)
		{
			total=x-x1+y-y1;
			/* If total is odd- and is a floor then make a wall */
			if((total%2==1)&&(cave_feat[y][x]==FEAT_FLOOR))
			{
				cave_feat[y][x]=FEAT_WALL_INNER;
				cave_info[y][x]|=CAVE_WALL;
			}		
		}	
	}
	
	/*Make a couple of entrances*/
	if (randint(2)==1)
	{
		/* left and right */
		y=randint(dy)+dy/2;
		cave_feat[y1+y][x1-1]=FEAT_WALL_OUTER;		
		cave_feat[y1+y][x2+1]=FEAT_WALL_OUTER;
	}
	else
	{
		/* top and bottom */
		x=randint(dx)+dx/2;
		cave_feat[y1-1][x1+x]=FEAT_WALL_OUTER;
		cave_feat[y2+1][x1+x]=FEAT_WALL_OUTER;
	}	
	
    /*Fill with monsters and treasure, highest difficulty*/
	fill_treasure(x1,x2,y1,y2,10);
 
    /* rnfree( visited,num_vertices * sizeof(int));*/
	free(visited);
 }

/* Build a town/ castle by using a recursive algorithm.
* Basically divide each region in a probalistic way to create
* smaller regions.  When the regions get too small stop.
*
* The power variable is a measure of how well defended a region is.
* This alters the possible choices.
*/

static void build_recursive_room(int x1,int y1,int x2,int y2,int power)
{
	int xsize,ysize;
	int x,y;
    int choice;
	
	/* Temp variables */
	int t1,t2,t3,t4;
	
	xsize=x2-x1;
	ysize=y2-y1;
	
	if ((power<3)&&(xsize>12)&&(ysize>12))
	{
		/* Need outside wall +keep */
		choice=1;
	}
	else
	{
		if (power<10)
		{
			/* Make rooms + subdivide */
			if ((randint(10)>2)&&(xsize<8)&&(ysize<8))
			{
				choice=4;
			}
			else
			{
				choice=randint(2)+1;
			}				
		}
		else
		{
			/* Mostly subdivide */
			choice=randint(3)+1;
		}
	}
	
	/* Based on the choice made above, do something*/
	
	switch (choice)
	{
		case 1:
		{
			/*Outer walls */
	
			/*top and bottom*/
			for(x=x1;x<=x2;x++)
			{
				cave_feat[y1][x]=FEAT_WALL_OUTER;
				cave_info[y1][x]|=CAVE_WALL;
				cave_feat[y2][x]=FEAT_WALL_OUTER;
				cave_info[y2][x]|=CAVE_WALL;	
			}
	
			/*left and right*/
			for(y=y1+1;y<y2;y++)
			{
				cave_feat[y][x1]=FEAT_WALL_OUTER;
				cave_info[y][x1]|=CAVE_WALL;
				cave_feat[y][x2]=FEAT_WALL_OUTER;
				cave_info[y][x2]|=CAVE_WALL;		
			}
		
			/* Make a couple of entrances */
			if (randint(2)==1)
			{
				/* left and right */
				y=randint(ysize)+y1;
				cave_feat[y][x1]=FEAT_FLOOR;		
				cave_info[y][x1]&=(~CAVE_WALL);
				cave_feat[y][x2]=FEAT_FLOOR;
				cave_info[y][x2]&=(~CAVE_WALL);
			}
			else
			{
				/* top and bottom */
				x=randint(xsize)+x1;
				cave_feat[y1][x]=FEAT_FLOOR;
				cave_info[y1][x]&=(~CAVE_WALL);
				cave_feat[y2][x]=FEAT_FLOOR;
				cave_info[y2][x]&=(~CAVE_WALL);
			}				
	
			/*Select size of keep*/
			t1=randint(ysize/3)+y1;
			t2=y2-randint(ysize/3);
			t3=randint(xsize/3)+x1;
			t4=x2-randint(xsize/3);
		
			/*Do outside areas*/
		
			/*Above and below keep*/
			build_recursive_room(x1+1,y1+1,x2-1,t1,power+1);
			build_recursive_room(x1+1,t2,x2-1,y2,power+1);
			
			/*Left and right of keep*/
			build_recursive_room(x1+1,t1+1,t3,t2-1,power+3);
			build_recursive_room(t4,t1+1,x2-1,t2-1,power+3);

			/* Make the keep itself:*/
		
			x1=t3;
			x2=t4;
			y1=t1;
			y2=t2;
			xsize=x2-x1;
			ysize=y2-y1;
			power+=2;
			/*Deliberate lack of a "break;" */
		}
		case 4:
		{
			/* Try to build a room */
			if ((xsize<3)||(ysize<3))
			{
				for (y=y1;y<y2;y++)
				{
					for(x=x1;x<x2;x++)
					{
						cave_feat[y][x]=FEAT_WALL_INNER;
						cave_info[y][x]|=CAVE_WALL;
					}
				}
				
				/*Too small */
				return;
			}
			
			/* Make outside walls */
			/*top and bottom*/
			for(x=x1+1;x<=x2-1;x++)
			{
				cave_feat[y1+1][x]=FEAT_WALL_INNER;
				cave_info[y1+1][x]|=CAVE_WALL;
				cave_feat[y2-1][x]=FEAT_WALL_INNER;
				cave_info[y2-1][x]|=CAVE_WALL;	
			}
	
			/*left and right*/
			for(y=y1+1;y<=y2-1;y++)
			{
				cave_feat[y][x1+1]=FEAT_WALL_INNER;
				cave_info[y][x1+1]|=CAVE_WALL;
				cave_feat[y][x2-1]=FEAT_WALL_INNER;
				cave_info[y][x2-1]|=CAVE_WALL;		
			}
			
			/*Make a door*/
			y=randint(ysize-3)+y1+1;
		
			if (randint(2)==1)
			{
				/* left  */			
				cave_feat[y][x1+1]=FEAT_FLOOR;		
				cave_info[y][x1+1]&=(~CAVE_WALL);
			
			}
			else
			{
				/* right */				
				cave_feat[y][x2-1]=FEAT_FLOOR;
				cave_info[y][x2-1]&=(~CAVE_WALL);
			}				
			/* Build the room*/
			build_recursive_room(x1+2,y1+2,x2-2,y2-2,power+3);	
			break;
		}
		case 2: 
		{
			/* Try and divide vertically */	
	
			if (xsize<3)
			{
				/* Too small*/
				for (y=y1;y<y2;y++)
				{
					for(x=x1;x<x2;x++)
					{
						cave_feat[y][x]=FEAT_WALL_INNER;
						cave_info[y][x]|=CAVE_WALL;
					}
				}
				return;
			}
			t1=randint(xsize-2)+x1+1;
			build_recursive_room(x1,y1,t1,y2,power-2);
			build_recursive_room(t1+1,y1,x2,y2,power-2);			
			break;
		}
		case 3:
		{		
			/* Try and divide horizontally */
			if (ysize<3)
			{
				/* Too small*/
				for (y=y1;y<y2;y++)
				{
					for(x=x1;x<x2;x++)
					{
						cave_feat[y][x]=FEAT_WALL_INNER;
						cave_info[y][x]|=CAVE_WALL;
					}
				}
				return;
			}
			t1=randint(ysize-2)+y1+1;
			build_recursive_room(x1,y1,x2,t1,power-2);
			build_recursive_room(x1,t1+1,x2,y2,power-2);
			break;
		}	
	}
}

/* Build a castle*/

/*Driver routine: clear the region and call the recursive
* room routine.
*
*This makes a vault that looks like a castle/ city in the dungeon. 
*/
static void build_castle_vault(int x0,int y0,int xsize,int ysize)
{
	int dy, dx;
	int y1, x1, y2, x2;
	int y, x;
	/* Pick a random room size */
    dy = ysize/2-1;
    dx = xsize/2-1;
 	
	y1 = y0 - dy;
 	x1 = x0 - dx;
 	y2 = y0 + dy;
 	x2 = x0 + dx;
 	
	if (cheat_room) msg_print("Castle Vault");
	
	/* generate the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{			
			cave_info[y][x] |= (CAVE_ROOM|CAVE_ICKY);
			cave_info[y][x] &= (~CAVE_WALL);
			/* Make everything a floor */
			cave_feat[y][x] =FEAT_FLOOR;
		}
	}	
    
	/* Make the castle */
	build_recursive_room(x1,y1,x2,y2,randint(5));
	
	/*Fill with monsters and treasure, low difficulty*/
	fill_treasure(x1,x2,y1,y2,randint(3));
}

/*Random vaults*/

static void build_type10(int by0, int bx0)
{
	int y0,x0,xsize,ysize,vtype;
	
	/*Get size */
	/*big enough to look good, small enough to be fairly common.*/
	xsize=randint(22)+22; 
	ysize=randint(11)+11;	

	/*Allocate in room_map.  If will not fit, exit*/
	if (room_alloc(xsize+1,ysize+1,FALSE,by0,bx0,&x0,&y0)==FALSE) return;
	
	/* Boost the rating- higher than lesser vaults and lower than greater vaults*/
	rating += 10;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}
	
	/*Select type of vault*/
	vtype=randint(6);
	
	switch (vtype)
	{
		/* Build an appropriate room */
		case 1: build_bubble_vault(x0,y0,xsize,ysize); break;
		case 2: build_room_vault(x0,y0,xsize,ysize); break;
		case 3: build_cave_vault(x0,y0,xsize,ysize); break;
		case 4: build_maze_vault(x0,y0,xsize,ysize); break;
		case 5: build_mini_c_vault(x0,y0,xsize,ysize); break;
		case 6: build_castle_vault(x0,y0,xsize,ysize); break;
		/*I know how to add a few more... give me some time.*/
		
		/* Paranoia */
		default: return;
	}
}


/*
* Add outer wall to a floored region
* Note: no range checking is done so must be inside dungeon
* This routine also stomps on doors
*/
static void add_outer_wall(int x,int y,int light)
{
	int i,j;
	
	if (!in_bounds_fully(y,x)) return;
	
	
	/* hack- check to see if square has been visited before
	* if so, then exit (use room flag to do this) */
	if (cave_info[y][x]&CAVE_ROOM) return;

	/* set room flag */
	cave_info[y][x]|=CAVE_ROOM;
	
	if (light==TRUE) cave_info[y][x]|=CAVE_GLOW;
	
	if (cave_feat[y][x]==FEAT_FLOOR)
	{
		for(i=-1;i<=1;i++)
		{
			for(j=-1;j<=1;j++)
			{
				add_outer_wall(x+i,y+j,light);	
			}
		}		
	}
	else
	{
		/*Set bounding walls*/
		cave_feat[y][x]=FEAT_WALL_OUTER;
		cave_info[y][x]|=CAVE_WALL;	
	}
}


/*
 * Build an vertical oval room.
 * For every grid in the possible square, check the distance.
 * If it's less than the radius, make it a room square.
 * 
 * When done fill from the inside to find the walls,
 */
static void build_type11(int by0, int bx0)
{
	int rad, x, y, x0, y0;	
	int light = FALSE;
	
	/* Occasional light */
	if (randint(p_ptr->depth) <= 5) light = TRUE;

	rad = rand_int(10);
	
	/*Allocate in room_map.  If will not fit, exit*/
	if (room_alloc(rad*2+1,rad*2+1,FALSE,by0,bx0,&x0,&y0)==FALSE) return;
	
	/* set outsides so add_outer_wall works properly. */
	for (x=x0 -rad; x<=x0+rad; x++)
	{
		cave_info[y0-rad][x]|=CAVE_ROOM;
		cave_info[y0+rad][x]|=CAVE_ROOM;	
	}
	for (y=y0 -rad; y<=y0+rad; y++)
	{
		cave_info[y][x0-rad]|=CAVE_ROOM;
		cave_info[y][x0+rad]|=CAVE_ROOM;	
	}
	
	/*Make circular floor */
	for (x = x0 - rad; x <= x0 + rad; x++)
	{
		for (y = y0 - rad; y <= y0 + rad; y++)
		{			
			if (distance(y0, x0, y, x)<=rad-1)
			{
				/* inside- so is floor */
				cave_info[y][x] &= ~(CAVE_WALL);
				cave_feat[y][x] = FEAT_FLOOR;									
			}			
			else if(distance(y0, x0, y, x)<=rad+1)
			{
				/* make granite outside so cavern works */
				cave_info[y][x] |= (CAVE_WALL);
				cave_feat[y][x] = FEAT_WALL_EXTRA;									
			}
		}
	
	}
	
	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0,y0,light);
}


/*
 * Hacked distance formula - gives the 'wrong' answer.
 * Used to build crypts
 */
static int dist2 (int x1,int y1,int x2,int y2,int h1,int h2,int h3,int h4)
{
	int dx,dy;
	dx=abs(x2-x1);
	dy=abs(y2-y1);
	
	/* Basically this works by taking the normal pythagorean formula
	* and using an expansion to express this in a way without the
	* square root.  This approximate formula is then perturbed to give
	* the distorted results.  (I found this by making a mistake when I was
	* trying to fix the circular rooms.)
	*/
	
	/* h1-h4 are constants that describe the metric */ 
	if (dx>=2*dy) return (dx+(dy*h1)/h2);
	if (dy>=2*dx) return (dy+(dx*h1)/h2);
    return (((dx+dy)*128)/181+(dx*dx/(dy*h3)+dy*dy/(dx*h3))*h4);
	/* 128/181 is approx. 1/sqrt(2) */
}

/*
 * Build crypt room.
 * For every grid in the possible square, check the (fake) distance.
 * If it's less than the radius, make it a room square.
 * 
 * When done fill from the inside to find the walls,
 */
static void build_type12(int by0, int bx0)
{
	int rad, x, y, x0, y0;	
	int light = FALSE;
	bool emptyflag=TRUE;
	
	/* Make a random metric */
	int h1,h2,h3,h4;
	h1=randint(32)-16;
	h2=randint(16);
	h3=randint(32);
	h4=randint(32)-16;
	/* Occasional light */
	if (randint(p_ptr->depth) <= 5) light = TRUE;

	rad = rand_int(10);
	
	/*Allocate in room_map.  If will not fit, exit*/
	if (room_alloc(rad*2+1,rad*2+1,FALSE,by0,bx0,&x0,&y0)==FALSE) return;
	
	/* set outsides so add_outer_wall works properly. */
	for (x=x0 -rad; x<=x0+rad; x++)
	{
		cave_info[y0-rad][x]|=CAVE_ROOM;
		cave_info[y0+rad][x]|=CAVE_ROOM;	
	}
	for (y=y0 -rad; y<=y0+rad; y++)
	{
		cave_info[y][x0-rad]|=CAVE_ROOM;
		cave_info[y][x0+rad]|=CAVE_ROOM;	
	}
	
	/*Make floor */
	for (x = x0 - rad; x <= x0 + rad; x++)
	{
		for (y = y0 - rad; y <= y0 + rad; y++)
		{			
			if (dist2(y0, x0, y, x, h1, h2, h3, h4)<=rad-1)
			{
				/* inside- so is floor */
				cave_info[y][x] &= ~(CAVE_WALL);
				cave_feat[y][x] = FEAT_FLOOR;									
			}			
			else if(dist2(y0, x0, y, x, h1, h2, h3, h4)<=rad+1)
			{
				/* make granite outside so cavern works */
				cave_info[y][x] |= (CAVE_WALL);
				cave_feat[y][x] = FEAT_WALL_EXTRA;									
			}
		}
	
	}
	
	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0,y0,light);
	
	/* check to see if there is room for an inner vault */	
	for (x=x0-2;x<=x0+2;x++)
	{
		for(y=y0-2;y<=y0+2;y++)
		{
			if (cave_feat[y][x]!=FEAT_FLOOR)
			{
				/* wall in the way */
				emptyflag=FALSE;
			}
		}	
	}
	
	if ((emptyflag)&&randint(2)==1)
	{
		/* Build the vault */
		for (y = y0-1; y <= y0+1; y++)
		{
			cave_feat[y][x0-1]=FEAT_WALL_INNER;
			cave_info[y][x0-1]|=CAVE_WALL;
			cave_feat[y][x0+1]=FEAT_WALL_INNER;
			cave_info[y][x0+1]|=CAVE_WALL; 
		}
		for (x = x0-1; x <= x0+1; x++)
		{
			cave_feat[y0-1][x]=FEAT_WALL_INNER;
			cave_info[y0-1][x]|=CAVE_WALL;
			cave_feat[y0+1][x]=FEAT_WALL_INNER;
			cave_info[y0+1][x]|=CAVE_WALL;
		}

		/* Place a secret door on the inner room */
		switch (rand_int(4))
		{
			case 0: place_secret_door(y0, x0-1); break;
			case 1: place_secret_door(y0, x0+1); break;
			case 2: place_secret_door(y0-1, x0); break;
			case 3: place_secret_door(y0+1, x0); break;
		}

		/* Place a treasure in the vault */
		place_object(y0, x0, FALSE, FALSE);

		/* Let's guard the treasure well */
		vault_monsters(y0, x0, rand_int(2) + 3);

		/* Traps naturally */
		vault_traps(y0, x0, 4, 4, rand_int(3) + 2);
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
		if (cave_feat[tmp_row][tmp_col] == FEAT_PERM_SOLID) continue;

		/* Avoid the edge of vaults */
		if (cave_feat[tmp_row][tmp_col] == FEAT_PERM_OUTER) continue;

		/* Avoid "solid" granite walls */
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_SOLID) continue;

		/* Pierce "outer" walls of rooms */
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_OUTER)
		{
			/* Acquire the "next" location */
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
		/* Access the grid */
		y = dun->tunn[i].y;
		x = dun->tunn[i].x;

		/* Clear previous contents, add a floor */
		cave_set_feat(y, x, FEAT_FLOOR);
	}


	/* Apply the piercings that we found */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Access the grid */
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
	
	/* Restrict level */
	if (p_ptr->depth < roomdep[typ]) return (FALSE);

	/* Restrict "crowded" rooms */
	if (dun->crowded && ((typ == 5) || (typ == 6))) return (FALSE);

	/* Build a room */
	switch (typ)
	{
		/* Build an appropriate room */		
		case 12: build_type12(by0, bx0); break;
		case 11: build_type11(by0, bx0); break;
		case 10: build_type10(by0, bx0); break;
		case 9: build_type9(by0, bx0); break;
		case 8: build_type8(by0, bx0); break;
		case 7: build_type7(by0, bx0); break;
		case 6: build_type6(by0, bx0); break;
		case 5: build_type5(by0, bx0); break;
		case 4: build_type4(by0, bx0); break;
		case 3: build_type3(by0, bx0); break;
		case 2: build_type2(by0, bx0); break;
		case 1: build_type1(by0, bx0); break;

		/* Paranoia */
		default: return (FALSE);
	}
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
	bool cavern = FALSE;

	dun_data dun_body;


	/* Global data */
	dun = &dun_body;


	/* Hack -- Start with basic granite */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Create granite wall */
			cave_set_feat(y, x, FEAT_WALL_EXTRA);
		}
	}
	
	if ((p_ptr->depth >DUN_CAVERN)&&(randint(400)<p_ptr->depth))	
	{
		cavern=TRUE;
		
		/* make a large fractal cave in the middle of the dungeon */
		
		if (cheat_room)
			msg_print("Cavern on level.");
		
		build_cavern(); 
	}

	/* Possible "destroyed" level */
	if ((p_ptr->depth > 10) && (rand_int(DUN_DEST) == 0)) destroyed = TRUE;

	/* Hack -- No destroyed "quest" levels */
	if (is_quest(p_ptr->depth)) destroyed = FALSE;


	/* Actual maximum number of rooms on this level */
	dun->row_rooms = DUNGEON_HGT / BLOCK_HGT;
	dun->col_rooms = DUNGEON_WID / BLOCK_WID;

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
			/*The deeper you are, the more cavelike the rooms are */
			
			/*no caves when cavern exists: they look bad */
			k=randint(100);
			if ((k<p_ptr->depth)&&(!cavern))		  
				{
				/* Type 9 -- Fractal cave */
				if(room_build(by,bx,9)) continue;
				}
			else			
				/* Attempt a "trivial" room */
				if (room_build(by, bx, 1)) continue;

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
				/* Type 8 -- Greater vault (7%) */
				if ((k < 7) && room_build(by, bx, 8)) continue;

				/* Type 7 -- Lesser vault (10%) */
				if ((k < 17) && room_build(by, bx, 7)) continue;

				/* Type 6 -- Monster pit (15%) */
				if ((k < 32) && room_build(by, bx, 6)) continue;

				/* Type 5 -- Monster nest (10%) */
				if ((k < 42) && room_build(by, bx, 5)) continue;
			
				/* Type 10 -- Random vault (8%) */
				if ((k < 50) && room_build(by, bx, 10)) continue;
			}						
			/* Type 4 -- Large room (20%) */
			if ((k < 20) && room_build(by, bx, 4)) continue;

			/* Type 3 -- Cross room (15%) */
			if ((k < 35) && room_build(by, bx, 3)) continue;

			/* Type 2 -- Overlapping (25%) */
			if ((k < 60) && room_build(by, bx, 2)) continue;
			
			/* Type 11 -- Circular (25%) */
			if ((k < 85) && room_build(by, bx, 11)) continue;
			
			/* Type 12 -- Crypt (15%) */
			if ((k < 100) && room_build(by, bx, 12)) continue;	
		}
		
		/*The deeper you are, the more cavelike the rooms are*/		
		k=randint(100);
		
		/*no caves when cavern exists: they look bad */	
		if ((k<p_ptr->depth)&&(!cavern))		
			{
			/* Type 9 -- Fractal cave */
			if (room_build(by, bx, 9)) continue;
			}
		else			
			/* Attempt a "trivial" room */
			if (room_build(by, bx, 1)) continue;
		continue;
	}


	/* Special boundary walls -- Top */
	for (x = 0; x < DUNGEON_WID; x++)
	{
		y = 0;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Bottom */
	for (x = 0; x < DUNGEON_WID; x++)
	{
		y = DUNGEON_HGT-1;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Left */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		x = 0;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Right */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		x = DUNGEON_WID-1;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
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


	/* Destroy the level if necessary */
	if (destroyed) destroy_level();


	/* Place 3 or 4 down stairs near some walls */
	alloc_stairs(FEAT_MORE, rand_range(3, 4), 3);

	/* Place 1 or 2 up stairs near some walls */
	alloc_stairs(FEAT_LESS, rand_range(1, 2), 3);


	/* Determine the character location */
	new_player_spot();


	/* Basic "amount" */
	k = (p_ptr->depth / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;


	/* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL + randint(8);

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
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ROOM, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal(DUN_AMT_GOLD, 3));

	/* Ghosts love to inhabit destroyed levels, but will live elsewhere */
	i = (destroyed) ? 11 : 1;

	/* Try to place the ghost */
	while (i-- > 0)
	{

		/* Attempt to place a ghost */
		if (place_ghost())
		{

			/* Hack -- increase the rating */
			rating += 10;

			/* A ghost makes the level special */
			good_item_flag = TRUE;

			/* Stop trying to place the ghost */
			break;
		}
	}
}



/*
 * Builds a store at a given pseudo-location
 *
 * As of 2.8.1 (?) the town is actually centered in the middle of a
 * complete level, and thus the top left corner of the town itself
 * is no longer at (0,0), but rather, at (qy,qx), so the constants
 * in the comments below should be mentally modified accordingly.
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

	int qy = OLD_SCREEN_HGT;
	int qx = OLD_SCREEN_WID;


	/* Find the "center" of the store */
	y0 = qy + yy * 9 + 6;
	x0 = qx + xx * 14 + 12;

	/* Determine the store boundaries */
	y1 = y0 - randint((yy == 0) ? 3 : 2);
	y2 = y0 + randint((yy == 1) ? 3 : 2);
	x1 = x0 - randint(5);
	x2 = x0 + randint(5);

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

	int qy = OLD_SCREEN_HGT;
	int qx = OLD_SCREEN_WID;

	int rooms[MAX_STORES];


	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = seed_town;


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
			build_store(rooms[k], y, x);

			/* Shift the stores down, remove one store */
			rooms[k] = rooms[--n];
		}
	}


	/* Place the stairs */
	while (TRUE)
	{
		/* Pick a location at least "three" from the outer walls */
		y = qy + rand_range(3, OLD_SCREEN_HGT - 4);
		x = qx + rand_range(3, OLD_SCREEN_WID - 4);

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

	int qy = OLD_SCREEN_HGT;
	int qx = OLD_SCREEN_WID;

	bool daytime;


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
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Create "solid" perma-wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);
		}
	}

	/* Then place some floors */
	for (y = qy+1; y < qy+OLD_SCREEN_HGT-1; y++)
	{
		for (x = qx+1; x < qx+OLD_SCREEN_WID-1; x++)
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
		p_ptr->wy = DUNGEON_HGT;
		p_ptr->wx = DUNGEON_WID;


		/* Reset the monster generation level */
		monster_level = p_ptr->depth;

		/* Reset the object generation level and
		 * apply RNG bribery -TM- */
		if (p_ptr->depth)
		{
			object_level = p_ptr->depth + (p_ptr->rng_lev_bonus / 50);
			p_ptr->rng_lev_bonus = 0;
		}
		else object_level = p_ptr->depth;

		/* Nothing special here yet */
		good_item_flag = FALSE;

		/* Nothing good here yet */
		rating = 0;


		/* Build the town */
		if (!p_ptr->depth)
		{
			/* Make a town -TM- */
			if (p_ptr->new_town)
			{
				init_new_town();
			}
			else
			{
				town_gen();
			}
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
		if (good_item_flag) feeling = 1;

		/* It takes 1000 game turns for "feelings" to recharge */
		if ((turn - old_turn) < 1000) feeling = 0;

		/* Hack -- no feeling in the town */
		if (!p_ptr->depth) feeling = 0;


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


/*
 * Load the new town layout from t_info.txt. -TM-
 */

void init_new_town(void)
{
	int x, y = -1;
	byte type;
	int ver_maj, ver_min, ver_pat = 0;
	
	int sx, sy;

	int residents, i;

	bool daytime;

	FILE *fp;

	/* 15Kb Buffer */
	char buf[1024];

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "t_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 't_info.txt' file.");


	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Process 'V' for version stamp */
		if (buf[0] == 'V')
		{
			sscanf(buf, "V:%d.%d.%d", &ver_maj, &ver_min, &ver_pat);
			
			if ( (ver_maj != VERSION_MAJOR) ||
			     (ver_min != VERSION_MINOR) ||
			     (ver_pat != VERSION_PATCH) )
			{
				quit("Incorrect t_info.txt file version.");
			}
		}
		
		/* Process 'S' for start position */
		if (buf[0] == 'S')
		{
			sscanf(buf, "S:%d:%d", &sx, &sy);
		}

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Advance y coords */
			y++;

			for (x=0; x<DUNGEON_WID; x++)
			{
				switch (buf[2 + x])
				{
					/* Perma wall */
					case '#':
					{
						type = FEAT_PERM_SOLID;
						break;
					}

					/* Granite wall */
					case 'X':
					{
						type = FEAT_WALL_SOLID;
						break;
					}

					/* Magma vein */
					case '%':
					{
						type = FEAT_MAGMA;
						break;
					}

					/* Quartz vein */
					case '*':
					{
						type = FEAT_QUARTZ;
						break;
					}

					/* Door */
					case '+':
					{
						type = FEAT_DOOR_HEAD;
						break;
					}

					/* Rubble */
					case ':':
					{
						type = FEAT_RUBBLE;
						break;
					}

					/* Shops: */
					case '1':
					{
						type = FEAT_SHOP_HEAD + 0;
						break;
					}

					case '2':
					{
						type = FEAT_SHOP_HEAD + 1;
						break;
					}

					case '3':
					{
						type = FEAT_SHOP_HEAD + 2;
						break;
					}

					case '4':
					{
						type = FEAT_SHOP_HEAD + 3;
						break;
					}

					case '5':
					{
						type = FEAT_SHOP_HEAD + 4;
						break;
					}

					case '6':
					{
						type = FEAT_SHOP_HEAD + 5;
						break;
					}

					case '7':
					{
						type = FEAT_SHOP_HEAD + 6;
						break;
					}

					case '8':
					{
						type = FEAT_SHOP_HEAD + 7;
						break;
					}

					/* A Dark Tree */
					case 'T':
					{
						type = FEAT_DTREE;
						break;
					}

					/* A Light Tree */
					case 't':
					{
						type = FEAT_LTREE;
						break;
					}

					/* Dirt */
					case '.':
					{
						type = FEAT_DIRT;
						break;
					}

					/* Water */
					case 'W':
					{
						type = FEAT_WATER;
						break;
					}

					/* Grass */
					case ',':
					{
						type = FEAT_GRASS;
						break;
					}

					case '>':
					{
						type = FEAT_MORE;
						break;
					}

					default:
					{
						type = FEAT_FLOOR;
						break;
					}
				}

				/* Set the terrain */
				cave_set_feat(y, x, type);
			
			}

			/* Next... */
			continue;
		}

	}

	/* Make sure we did get a version number */
	if (!ver_maj || !ver_min || !ver_pat)
	{
		quit("Missing version stamp in t_info.txt file.");
	}
	
	/* Place the player */
	player_place(sy, sx);

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

	/* Light the town */
	(void)town_illuminate(daytime);

	/* Make some residents */
	for (i = 0; i < residents; i++)
	{
		/* Make a resident */
		(void)alloc_monster(3, TRUE);
	}

	/* Close it */
	my_fclose(fp);

	return;
}

