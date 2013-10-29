/* File: generate.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koenekerubb
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "script.h"


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
 * Note that a tunnel which attempts to leave a room near the edge
 * of the dungeon in a direction toward that edge will cause silly
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
#define DUN_DEST	30	/* 1/chance of having a destroyed level */

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND	10	/* Chance of random direction */
#define DUN_TUN_CHG	30	/* Chance of changing direction */
#define DUN_TUN_CON	15	/* Chance of extra tunneling */
#define DUN_TUN_PEN	30	/* Chance of doors at room entrances (was 25) */
#define DUN_TUN_JCT	80	/* Chance of doors at tunnel junctions (was 90) */

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
#define ALLOC_TYP_WATER		6	/* water */
#define ALLOC_TYP_OPEN_PIT	7	/* open pit */
#define ALLOC_TYP_TREES		8   /* trees */


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
#define ROOM_MAX	11



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
	{ 0, 1, -1, 1, 5 },		/* 9 = empty vault (uses lesser vault design) */
	{ -1, 2, -2, 3, 10 }	/* 10 = empty Greater vault (66x44) (extremely rare) */
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
 * Chooses a spot and places the player character
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

	/* 9% chance of rubble object */
	/* (was 10%, changed because rubble is more common now) */
	/* (previously was not created until the rubble was dug up */
	/* now you can find it by searching as well as by digging) */
	if (rand_int(100) < 9)
	{
		object_type *o_ptr;

		/* create the object */
		place_object(y, x, FALSE, FALSE);

		/* now hide it in the rubble */
		o_ptr = &o_list[cave_o_idx[y][x]];
		o_ptr->hidden = 1;
	}

	/* Create big rocks (after hiding the rubble object) */
	big_rocks(y, x);
}


/*
 * Convert existing terrain type to water and make a puddle around it
 */
static void place_puddle(int y, int x, bool vault)
{
	int i, spoty, spotx;
	int puddle_size = 3 + randint(3);
	
	/* (no enlarged puddles in vaults) */
	if (!vault)
	{
		/* sometimes allow bigger puddles in a swamp */
		if ((p_ptr->theme == 9) && (randint(100) < 13)) puddle_size += 3 + randint(7);
		/* bigger usual puddle size in swamps */
		else if ((p_ptr->theme == 9) && (randint(100) < 75)) puddle_size += 1 + randint(4);
		/* in forests, they are ponds, not puddles (bigger and only 1-2 of them) */
		else if ((p_ptr->theme == 1) && (p_ptr->theme == 2))
			puddle_size += 4 + randint(8);
	}

	/* water */
	cave_set_feat(y, x, FEAT_WATER);

	/* make a puddle */
	for (i = 0; i < puddle_size; i++)
	{
		if (get_nearby(y, x, &spoty, &spotx, 2))
		{
			cave_set_feat(spoty, spotx, FEAT_WATER);
			
			/* sometimes set new starting point */
			if (randint(100) < 35)
			{
				y = spoty;
				x = spotx;
			}
		}
		else if (randint(100) < 70)
		{
			/* try again */
			i -= 1;
		}
	}
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

			/* if placing water, allow monsters */
			if (typ == ALLOC_TYP_WATER)
			{
				if (!cave_clean_bold(y, x)) continue;
			}
			/* Require "naked" floor grid */
			else if (!cave_naked_bold(y, x)) continue;

			/* Check for "room" */
			room = (cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

			/* never randomly put puddles or trees in a vault */
			if (((typ == ALLOC_TYP_WATER) || (typ == ALLOC_TYP_TREES)) && 
				(cave_info[y][x] & (CAVE_ICKY)))
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

			case ALLOC_TYP_OPEN_PIT:
			{
				cave_set_feat(y, x, FEAT_OPEN_PIT);
				break;
			}

			case ALLOC_TYP_WATER:
			{
				place_puddle(y, x, FALSE);
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

			case ALLOC_TYP_TREES:
			{
				/* sometimes allow pairs */
				if (randint(100) < 67) place_monster_aux(y, x, 834, TRUE, TRUE);
				else place_monster_aux(y, x, 834, TRUE, FALSE);
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
					else if (t < 90)
					{
						/* Create magma vein */
						cave_set_feat(y, x, FEAT_MAGMA);
					}

					/* Rubble */
					else if ((t < 100) || ((t > 105) && (t < 110)))
					{
						/* Create rubble (no chance for buried object) */
						cave_set_feat(y, x, FEAT_RUBBLE);
						big_rocks(y, x);
					}

					/* open pits */
					else if (t < 112)
					{
						/* a pit that isn't a trap (and cannot be disarmed) */
						cave_set_feat(y, x, FEAT_OPEN_PIT);
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
			if (!cave_can_occupy_bold(y, x)) continue;

			/* Place the monster (allow groups) */
			(void)place_monster(y, x, TRUE, TRUE, FALSE);

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
	int y, x, dimness;

    /* how dim is the room's light? */
    int lmod = randint(50 - goodluck/2 + p_ptr->depth * 2 + p_ptr->corrupt);
    if (p_ptr->depth == 0) dimness = 0;
    else if ((lmod < 33) && (p_ptr->depth < 26)) dimness = 1;
    else if ((lmod < 66) && ((p_ptr->depth < 55) || (goodluck > 10))) dimness = 2;
    else if (lmod < 100) dimness = 3;
    else dimness = 4;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_info[y][x] |= (CAVE_ROOM);
			if (light)
            {
                 cave_info[y][x] |= (CAVE_GLOW);
#if EXPM
                 if (dimness == 0) cave_info[y][x] |= (DLIT_FULL);
                 else if (dimness == 1) cave_info[y][x] |= (DLIT_DIMA);
                 else if (dimness == 2) cave_info[y][x] |= (DLIT_DIMB);
                 else if (dimness == 3) cave_info[y][x] |= (DLIT_DIMC);
                 else /* dimness 4 */ cave_info[y][x] |= (DLIT_DIMD);
#endif
            }
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
 *   9 -- empty vaults
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
	if (p_ptr->depth < 25)
	{
		if (p_ptr->depth <= randint(25)) light = TRUE;
	}
	else if (p_ptr->depth <= 45)
	{
		if (randint(p_ptr->depth + 5) <= 2) light = TRUE;
	}
	else if (randint(p_ptr->depth + 10 + badluck/2 - goodluck/2) == 2) light = TRUE;

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
	else if ((p_ptr->depth > 25) && (randint(p_ptr->depth + 15 + badluck/2 - goodluck/2) == 2))
	     light = TRUE;


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
	else if ((p_ptr->depth > 25) && (randint(p_ptr->depth + 15 + badluck/2 - goodluck/2) == 2))
	     light = TRUE;


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


	/* Occasional light (more often lit than other room types) */
	if (p_ptr->depth <= randint(25)) light = TRUE;
	else if ((p_ptr->depth > 2) && (randint(p_ptr->depth + 5 + badluck/2 - goodluck/2) == 2))
	     light = TRUE;


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
 * (DJA: I can't find where it checks for the MULTIPLY flag?)
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

	/* Require worm, jelly, mold, or mushroom */
	if (!strchr("wjm,", r_ptr->d_char)) return (FALSE);

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
	/* often reject bugs and rodents because of creepy crawly nest */
	/* and worms because of jelly nest */
	if (strchr("awrFIS", r_ptr->d_char) &&
     (randint(10) < 7)) return (FALSE);
    /* small chance of rejecting hounds */
    if (strchr("Z", r_ptr->d_char) &&
     (randint(10) < 2)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (creepy crawlies)"
 *    (includes bugs, rodents, and snakes)
 *    (would include worms except they're in the jelly nest)
 */
static bool vault_aux_bugs(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "acrFIJS" monsters */
	if (!strchr("acrFIJS", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (imps & fairies)"
 */
static bool vault_aux_fairy(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "iy" monsters */
	if (!strchr("iy", r_ptr->d_char)) return (FALSE);

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
 * Helper function for "monster nest (great beasts)"
 */
static bool vault_aux_beast(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require "RMHlqN" monsters */
	if (!strchr("RMHlqN", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest/pit (special for theme)"
 * (sometimes used for both pits and nests)
 */
static bool vault_aux_theme(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Decline HELPER monsters */
	if (r_ptr->flags3 & (RF3_HELPER)) return (FALSE);

	/* Hack -- Require appropriately themed monsters */
	if (!theme_okay(r_idx, 0, TRUE)) return (FALSE);

	/* often reject never_move monsters */
	if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) && (rand_int(100) < 70))
		return (FALSE);

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
	if (!strchr("Tx", r_ptr->d_char)) return (FALSE);

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
	if (!strchr("uU&", r_ptr->d_char)) return (FALSE);

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
 *   a nest of "creepy crawly" monsters (Dungeon level 15 and deeper)
 *   a nest of "imps and fairy" monsters (Dungeon level 25 and deeper)
 *   a nest of "animal" monsters  (Dungeon level 30 and deeper)
 *   a nest of "great beast" monsters (Dungeon level 38 and deeper)
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
	bool special = FALSE;

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
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);


	/* Hack -- Choose a nest type */
	tmp = randint(p_ptr->depth);

    /* DJA randomize a little more */
	if ((randint(100) < 40) && (!p_ptr->theme))
	{
       int tmpb;
       if (p_ptr->depth < 70) tmpb = randint(p_ptr->depth+20);
       else tmpb = randint(90);
       if ((tmpb > tmp) && (randint(50) < 5 + badluck - goodluck)) tmp = tmpb;
       else if (tmpb > tmp) tmp = (tmp + tmpb) / 2;
       else if (tmpb < tmp) tmp = (tmp + tmpb) / 2 + 1;
    }

	/* if it's a themed level, make sure the nest is appropriate */
	if (p_ptr->theme == 3) tmp = 10; /* ICKY_PLACE- always jelly nest */
	else if (p_ptr->theme == 11) tmp = 20; /* BUG_CAVE- always creepy crawly nest */
	else if ((p_ptr->theme == 2) && (randint(100) < 85)) tmp = 24; /* FFOREST- usually imp/fairy nest */
	else if ((p_ptr->theme < 3) && (randint(100) < 65)) tmp = 30; /* FFOREST & CFOREST- animal nest */
	else if (p_ptr->theme < 3) tmp = 40; /* FFOREST & CFOREST- great beast nest */
	else if (((p_ptr->theme == 8) || (p_ptr->theme == 7)) && (randint(100) < 35))
		tmp = 40; /* CASTLE & FULL_MOON- great beasts, */
	else if (((p_ptr->theme == 8) || (p_ptr->theme == 7) || (p_ptr->theme == 13)) &&
		(randint(100) < 50)) tmp = 60; /* CASTLE, NIGHTMARE, FULL_MOON- undead */
	else if ((p_ptr->theme == 9) && (randint(100) < 60)) tmp = 20;
	else if ((p_ptr->theme == 9) && (randint(100) < 60)) tmp = 20; /* swamp- creepy crawlies or jelly */
	/* CASTLE, NIGHTMARE, HELL_HALL, GREPSE, DWARF_MINE - never animal */
	else if ((p_ptr->theme == 8) || (p_ptr->theme == 13) || (p_ptr->theme == 14) ||
		 (p_ptr->theme == 12) || (p_ptr->theme == 10)) special = TRUE;
	else if ((p_ptr->theme == 5) && (randint(100) < 40)) tmp = 20; /* EARTHY- creepy crawly */
	else if ((tmp > 25) && (tmp < 50)) /* animal/beast nests okay for most themes */;
	else if (p_ptr->theme) special = TRUE;

	/* special pit for themed levels */
	if (special)
	{
		name = "special theme";
		special = TRUE;
		get_mon_num_hook = vault_aux_theme; /* special */
	    if (p_ptr->depth < 40) rating += 10;
	}

	/* Monster nest (jelly) */
	if (tmp < 15)
	{
		/* Describe */
		name = "jelly";

		/* Restrict to jelly */
		get_mon_num_hook = vault_aux_jelly;

	    /* Increase the level rating (depending on nest type) */
	    if (p_ptr->depth < 40) rating += 3;
	}

	/* Monster nest (creepy crawly) */
	else if (tmp < 22)
	{
		/* Describe */
		name = "creepy crawly";

		/* Restrict to creepy crawlys */
		get_mon_num_hook = vault_aux_bugs;

	    /* Increase the level rating (depending on nest type) */
	    rating += 6;
	}

	/* Monster nest (imp and fairy) */
	else if (tmp < 25)
	{
		/* Describe */
		name = "imp and fairy";

		/* most fairies like light */
		if (randint(100) > p_ptr->depth + 15) light = TRUE;

		/* Restrict to imp or fairy */
		get_mon_num_hook = vault_aux_fairy;

	    /* Increase the level rating (depending on nest type) */
	    rating += 10;
	}

	/* Monster nest (animal) */
	else if (tmp < 37)
	{
		/* Describe */
		name = "animal";

		/* Restrict to animal */
		get_mon_num_hook = vault_aux_animal;

	    /* Increase the level rating (depending on nest type) */
	    rating += 10;
	}

	/* Monster nest (great beast) */
	else if (tmp < 50)
	{
		/* Describe */
		name = "great beast";

		/* Restrict to great beast */
		get_mon_num_hook = vault_aux_beast;

	    /* Increase the level rating (depending on nest type) */
	    rating += 12;
	}

	/* Monster nest (undead) */
	else
	{
		/* Describe */
		name = "undead";

		/* Restrict to undead */
		get_mon_num_hook = vault_aux_undead;

	    /* Increase the level rating (depending on nest type) */
	    rating += 13;
	}

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(p_ptr->depth + 10, FALSE);

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
			(void)place_monster_aux(y, x, r_idx, TRUE, FALSE);
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
	int tmp, what[16];

	int i, j, y, x, y1, x1, y2, x2;

	bool empty = FALSE;
	bool special = FALSE;

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
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);


	/* Choose a pit type */
	tmp = randint(p_ptr->depth);

	/* if it's a themed level, make sure the nest is appropriate */
	if ((p_ptr->theme == 8) && (randint(100) < 75)) tmp = 50 + rand_int(20); /* CASTLE- always giant or dragon or special */
	else if (((p_ptr->theme == 1) || (p_ptr->theme == 4) || (p_ptr->theme == 9)) &&
		randint(100) < 80) tmp = 70; /* VOLCANO, CFOREST, SWAMP - always (appropriate) dragon pit or special */
	else if ((p_ptr->theme == 10) && (randint(100) < 60)) tmp = randint(39); /* DWARF_MINE - always orc, troll or special */
	else if ((p_ptr->theme == 1) || (p_ptr->theme == 4) || (p_ptr->theme == 9) || 
		(p_ptr->theme == 10)) special = TRUE;
	else if ((p_ptr->theme == 8) && (randint(100) < 75)) tmp = 30; /* EARTHY - troll pit */
	else if ((p_ptr->theme == 3) && (randint(100) < 45)) tmp = 70; /* ICKY - dragons or special */
	else if (p_ptr->theme == 14) tmp = 90; /* HELL_HALL always demons */
	else if (((p_ptr->theme == 13) || (p_ptr->theme == 7)) && (randint(100) < 40))
		tmp = 90; /* NIGHTMARE / FULL_MOON - demons or special */
	else if ((p_ptr->theme) && (randint(100) < 80)) special = TRUE;

	/* special theme pit */
	if (special)
	{
		/* Message */
		name = "special theme";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_theme;
	}

	/* Orc pit */
	else if (tmp < 20)
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
		int whichd = rand_int(6);
		if (randint(100) < 33) whichd = rand_int(7); /* includes sound dragons */
		/* if it's a themed level, make sure the dragon type is appropriate */
		if (p_ptr->theme == 1) whichd = 3; /* cold dragons in CFOREST */
		else if (p_ptr->theme == 4) whichd = 2; /* fire dragons in VOLCANO */
		else if ((p_ptr->theme == 9) && (randint(100) < 70)) whichd = 4; /* poison dragons in SWAMP usually */
		else if ((p_ptr->theme == 9) || (p_ptr->theme == 3)) whichd = 0; /* acid dragons in ICKY_PLACE or SWAMP */
		else if ((p_ptr->theme == 7) && (randint(100) < 75)) whichd = 1;
		else if (p_ptr->theme == 7) whichd = 5; /* electric or multi-hued dragons in FULL_MOON */
		else if ((p_ptr->theme == 6) && (randint(100) < 20)) whichd = 6; /* orange dragons sometimes in WINDY_CAVE */
		else if ((p_ptr->theme == 12) && (randint(100) < 20)) whichd = 5; /* multi-hued sometimes in GREPSE */
		else if ((p_ptr->theme == 12) && (randint(100) < 67)) whichd = 9; /* confusion in GREPSE */
		else if ((p_ptr->theme == 6) || (p_ptr->theme == 12)) whichd = 7; /* silver dragons in WINDY_CAVE or GREPSE */
		else if (p_ptr->theme == 5) whichd = 8; /* grey dragons in EARTHY_CAVE */

		/* Pick dragon type */
		switch (whichd)
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

			/* (case 5: multi-hued) */

			/* sound (orange, usually theme only) */
			case 6:
			{
				/* Message */
				name = "sound dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_SOUN;

				/* Done */
				break;
			}

			/* Silver (theme only) */
			case 7:
			{
				/* Message */
				name = "silver dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_NEXU;

				/* Done */
				break;
			}

			/* Grey (theme only) */
			case 8:
			{
				/* Message */
				name = "grey dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_WALL;

				/* Done */
				break;
			}

			/* confusion (theme only) */
			case 9:
			{
				/* Message */
				name = "confusion dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_CONF;

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
		what[i] = get_mon_num(p_ptr->depth + 10, FALSE);

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


	/* Increase the level rating (orc pits decrease in rating with depth) */
	if ((tmp < 20) && (p_ptr->depth >= 9))
	{
		rating += 11 - (p_ptr->depth/9);
	}
	else
	{
		rating += 10;

		/* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
		if ((p_ptr->depth <= 40) &&
		    (randint(p_ptr->depth * p_ptr->depth + 1) < 300))
		{
			good_item_flag = TRUE;
		}
	}


	/* Top and bottom rows */
	for (x = x0 - 9; x <= x0 + 9; x++)
	{
		place_monster_aux(y0 - 2, x, what[0], TRUE, FALSE);
		place_monster_aux(y0 + 2, x, what[0], TRUE, FALSE);
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
 * Require monster appropriate for monster temple within the theme
 */
static int temple_theme(int level)
{
	int ridx;
	int die = randint(100);
	monster_race *r_ptr;
	monster_race *ar_ptr;

	switch (p_ptr->theme)
	{
		case 1: /* fairy forest */
		{
			if (level < 23) ridx = 356; /* centaur stargazer */
			else if (level < 29) ridx = 401; /* wild unicorn */
			else if (level < 48)
			{
				if (die < 65) ridx = 845; /* gnoyem */
				else ridx = 650; /* dark fairy king */
			}
			else if (level < 56)
			{
				if (die < 16) ridx = 665; /* white unicorn */
				else ridx = 680; /* dark fairy queen */
			}
			else
			{
				if (die < 65) ridx = 712; /* dark fairy fool */
				else ridx = 737; /* titan */
			}
		}
		case 2: /* cold forest */
		{
			r_ptr = &r_info[651]; /* check for Scatha */
			
			if (level < 26) ridx = 387; /* owlbear */
			else if (level < 33)
			{
				if (die < 65) ridx = 471; /* snow e. */
				else ridx = 494; /* mature blueish-white dragon */
			}
			else if (((level > 40) && (level < 55)) &&
				(r_ptr->cur_num < r_ptr->max_num))
			{
				ridx = 651; /* Scatha */
			}
			else if (level < 45)
			{
				if (die < 50) ridx = 570; /* wendigo */
				else if (die < 65) ridx = 530; /* ice elemental */
				else ridx = 579; /* ancient blueish-white dragon */
			}
			else if (level < 90) ridx = 718; /* great ice wyrm */
			else ridx = 795; /* ancient torrent elemental */
		}
		case 3: /* icky place */
		{
			r_ptr = &r_info[832]; /* check for the icky king */
			if ((level < 25) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 832;
			else if (level < 28)
			{
				if (die < 70) ridx = 413; /* mind flayer */
				else ridx = 470; /* ooze e. */
			}
			else if (level < 41)
			{
				if (die < 70) ridx = 545; /* master mind flayer */
				else ridx = 602; /* giant ameoba slime */
			}
			else if (level < 52) ridx = 674; /* nulfraz */
			else if (level < 89)
			{
				if (die < 35) ridx = 730; /* great bile wyrm */
				else if (die < 70) ridx = 721; /* greater hungry ghost */
				else ridx = 750; /* great wyrm of chaos */
			}
			else ridx = 798; /* plague slime */
		}
		case 4: /* volcano */
		{
			if (level < 34)
			{
				if (die < 50) ridx = 441; /* chimera */
				else ridx = 469; /* smoke e. */
			}
			else if (level < 45)
			{
				r_ptr = &r_info[587]; /* check for Kavlax */
				if ((die < 35) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 587;
				else if (die < 50) ridx = 620; /* 11-headed hydra */
				else ridx = 597; /* ancient red dragon */
			}
			else if (level < 85)
			{
				r_ptr = &r_info[656]; /* check for Smaug */
				ar_ptr = &r_info[744]; /* check for Glaurung */
				if ((die < 50) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 656;
				else if ((die < 45) && (ar_ptr->cur_num < ar_ptr->max_num)) ridx = 744;
				else if (die < 35) ridx = 649; /* 9-headed salamander hydra */
				else if (die < 65) ridx = 731; /* great hell wyrm */
				else ridx = 714; /* greater fire elemental */
			}
			else
			{
				if (die < 35) ridx = 797; /* giant ifrit */
				else ridx = 794; /* ancient lava xorn */
			}
		}
		case 5: /* earthy cave */
		{
			if (level < 22)
			{
				ridx = 310; /* zhelung */
			}
			else if (level < 35)
			{
				if ((level < 31) && (die < 50)) ridx = 439; /* xreek */
				else if (die < 45) ridx = 528; /* mature grey dragon */
				else if (die < 70) ridx = 456; /* blinking zhelung */
				else ridx = 475; /* crystal drake */
			}
			else if (level < 45)
			{
				if ((level < 42) && (die < 50)) ridx = 572; /* xorn */
				else if (level < 50) ridx = 628; /* xaren */
				else if (level < 62) ridx = 571; /* wyvern */
				else if (level < 80) ridx = 605; /* ancient zhelung */
				else ridx = 613; /* ancient grey dragon */
			}
			else if (level < 88)
			{
				if (die < 60) ridx = 717; /* greater earth e. */
				else if (level < 58) ridx = 640; /* great crystal drake */
				else ridx = 751; /* great wyrm of disruption */
			}
			else
			{
				if (die < 65) ridx = 796; /* giant xreek */
				else ridx = 794; /* ancient lava xorn */
			}
		}
		case 6: /* windy cave */
		{
			if ((level < 25) && (randint(100) < 35)) ridx = 299; /* gargoyle */
			else if (level < 34)
			{
				if (die < 65) ridx = 428; /* colbran */
				else ridx = 480; /* invisible stalker */
			}
			else if (level < 47)
			{
				if ((die < 50) && (level < 41)) ridx = 575; /* cloud giant */
				else if (die < 50) ridx = 627; /* storm giant */
				else if (die < 64) ridx = 612; /* ancient silver dragon */
				else ridx = 564; /* air elemental */
			}
			else if (level < 65)
			{
				r_ptr = &r_info[700]; /* check for the Phoenix */
				if ((die < 45) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 700;
				else if ((die < 60) && (level < 58)) ridx = 694; /* fallen archon */
				else if (die < 60) ridx = 719; /* great storm wyrm */
				else ridx = 716; /* greater air e. */
			}
			else
			{
				r_ptr = &r_info[758]; /* check for Pazuzu */
				if ((die < 55) && (level < 76))  ridx = 737; /* titan */
				else if (die < 55) ridx = 766; /* greater titan */
				else if (r_ptr->cur_num < r_ptr->max_num) ridx = 758; /* Pazuzu */
				else if (level < 93) ridx = 752; /* great wyrm of physics */
				else ridx = 795; /* ancient torrent e. */
			}
		}
		case 7: /* full moon */
		{
			if (level < 28)
			{
				if (die < 45) ridx = 389; /* wolf cheiftain */
				else ridx = 402; /* vampire */
			}
			else if (level < 35)
			{
				r_ptr = &r_info[444]; /* check for the Skeezix */
				if (r_ptr->cur_num < r_ptr->max_num) ridx = 444;
				else ridx = 848; /* black stalker cat */
			}
			else if (level < 57)
			{
				r_ptr = &r_info[626]; /* check for the Cerberus */
				if ((die < 50) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 626;
				else if ((die < 40) && (level < 50)) ridx = 631; /* doppleganger */
				else if (die < 50) ridx = 844; /* greater werewolf */
				else if ((die < 94) && (level < 53)) ridx = 616; /* wereraven */
				else if (die < 90) ridx = 670; /* high priest of Liart */
				else ridx = 641; /* static drake */
			}
			else if (level < 77)
			{
				r_ptr = &r_info[736]; /* check for Baphomet */
				ar_ptr = &r_info[728]; /* check for the White Cat of B. */
				if ((die < 40) && (ar_ptr->cur_num < ar_ptr->max_num)) ridx = 728;
				else if ((die > 50) && (level > 62) && 
					(r_ptr->cur_num < r_ptr->max_num)) ridx = 736;
				else if ((level < 69) || (die > 85)) ridx = 720; /* bronze idol */
				else ridx = 746; /* golden idol */
			}
			else
			{
				r_ptr = &r_info[760]; /* check for Draugluin */
				ar_ptr = &r_info[770]; /* check for Liart */
				if (r_ptr->cur_num < r_ptr->max_num) ridx = 760;
				else if (ar_ptr->cur_num < ar_ptr->max_num) ridx = 770;
				else ridx = 746; /* golden idol */
			}
		}
		case 8: /* haunted castle */
		{
			if (level < 24)
			{
				if (die < 20) ridx = 339; /* human mummy */
				else ridx = 353; /* gory ghost */
			}
			else if (level < 41)
			{
				if ((die < 25) && (level < 31)) ridx = 467; /* crypt wight */
				else if (die < 25) ridx = 510; /* lich */
				else if ((die < 35) && (level > 33)) ridx = 571; /* wyvern */
				else if ((die < 75) && (level < 35)) ridx = 449; /* golden naga */
				else if ((die > 80) && (level < 37)) ridx = 474; /* shadow drake */
				else if (die < 50) ridx = 559; /* black wraith */
				else ridx = 542; /* greater mummy */
			}
			else if (level < 44)
			{
				if (die < 40) ridx = 601; /* vampire lord */
				else ridx = 606; /* master lich */
			}
			else if (level < 61)
			{
				r_ptr = &r_info[702]; /* check for the Minotaur of Crete */
				if ((level > 53) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 702;
				else if ((die < 40) && (level < 51)) ridx = 659; /* gorgon */
				else if (die < 30) ridx = 847; /* barrow wight king */
				else if ((die < 70) && (level < 58)) ridx = 686; /* elder vampire */
				else if (level < 58) ridx = 685; /* demilich */
				else if (die < 70) ridx = 689; /* dracolich */
				else if (die > 94) ridx = 726; /* archlich */
				else ridx = 847; /* barrow wight king */
			}
			else
			{
				r_ptr = &r_info[771]; /* check for Vecna */
				ar_ptr = &r_info[762]; /* check for Ancalagon */
				if ((level > 85) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 771;
				else if ((die < 40) && (level > 78) &&
					(ar_ptr->cur_num < ar_ptr->max_num)) ridx = 762;
				else if (level < 80)
				{
					r_ptr = &r_info[734]; /* check for silent watchers (left) */
					ar_ptr = &r_info[735]; /* check for (right) */
					if (r_ptr->cur_num < r_ptr->max_num) ridx = 734;
					else if (ar_ptr->cur_num < ar_ptr->max_num) ridx = 735;
					else ridx = 737; /* titan */
				}
				else if (die < 25) ridx = 795; /* ancient torrent e. */
				else ridx = 766; /* greater titan */
			}
		}
		case 9: /* swamp */
		{
			if (level < 30)
			{
				if (die < 25) ridx = 417; /* 5-headed hydra */
				else if (die < 50) ridx = 421; /* hag */
				else if ((die < 63) && (level > 22)) ridx = 447; /* banshee */
				else ridx = 395; /* pooka */
			}
			else if (level < 53)
			{
				r_ptr = &r_info[523]; /* check for the Watcher in the Water */
				if ((die < 32) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 523;
				else if ((die < 50) && (level < 43)) ridx = 558; /* 7-headed hydra */
				else if (die < 30) ridx = 620; /* 11-headed hydra */
				else if ((die > 60) && (level < 47)) ridx = 582; /* ancient green */
				else if (die < 45) ridx = 659; /* gorgon */
				else if (die < 80) ridx = 674; /* nulfraz */
				else ridx = 655; /* kracken */
			}
			else if (level < 64)
			{
				r_ptr = &r_info[677]; /* check for Leviathon */
				ar_ptr = &r_info[699]; /* check for the Lernaean Hydra */
				if ((die < 45) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 677;
				else if ((die > 65) && (ar_ptr->cur_num < ar_ptr->max_num)) ridx = 699;
				else if (die > 55) ridx = 690; /* demogorgon */
				else if (level < 57) ridx = 674; /* nulfraz */
				else ridx = 715; /* greater water e. */
			}
			else
			{
				if ((die < 50) && (level < 90)) ridx = 730; /* great bile wyrm */
				else if (die < 50) ridx = 798; /* plague slime */
				else if (level < 103) ridx = 790; /* behemoth */
				else if (level < 118) ridx = 795; /* ancient torrent e. */
				else ridx = 804; /* giant behemoth */
			}
		}
		case 10: /* dwarf mine */
		{
			if (level < 44)
			{
				r_ptr = &r_info[523]; /* check for the Watcher in the Water */
				if ((die < 60) && (level > 22)) ridx = 839; /* grag high priest */
				else if ((die < 60) && (level > 31) &&
					(r_ptr->cur_num < r_ptr->max_num)) ridx = 523;
				else if ((die < 40) && (level < 26)) ridx = 310; /* zhelung */
				else if ((die < 40) && (level < 35)) ridx = 456; /* blinking zhelung */
				else if ((die < 65) && (level < 43)) ridx = 486; /* mithril golem */
				else if (die < 79) ridx = 846; /* the summoning dark */
				else if (die < 90) ridx = 536; /* rock troll */
				else if (level < 30) ridx = 830; /* blood diamond */
				else ridx = 838; /* ironwood tree */
			}
			else if (level < 70)
			{
				r_ptr = &r_info[672]; /* check for the Balrog of Moria */
				ar_ptr = &r_info[705]; /* check for Alberich */
				if ((die < 50) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 672;
				else if ((die > 65) && (level > 49) &&
					(ar_ptr->cur_num < ar_ptr->max_num)) ridx = 705;
				else if (die < 90) ridx = 666; /* lesser balrog */
				else ridx = 846; /* the summoning dark */
			}
			else
			{
				r_ptr = &r_info[749]; /* check for Fafner */
				if ((die > 60) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 749;
				else if (die < 50) ridx = 756; /* greater balrog */
				else if (level < 97) ridx = 727; /* silver idol */
				else ridx = 794; /* ancient lava xorn */
			}
		}
		case 11: /* bug cave */
		{
			if (level < 30)
			{
				if (die < 45) ridx = 322; /* dark elven lord */
				else if ((die < 60) && (level < 20)) ridx = 226; /* drider */
				else if (die < 70) ridx = 374; /* dark elven druid */
				else if ((die < 79) && (level < 24)) ridx = 338; /* xan */
				else if (level < 22) ridx = 336; /* rhino. beetle */
				else ridx = 419; /* killer slicer beetle */
			}
			else if (level < 50)
			{
				r_ptr = &r_info[551]; /* check for the Queen Ant */
				if ((die < 40) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 551;
				else if (die < 47) ridx = 654; /* high priest of achrya */
				else if ((die < 60) && (level < 44)) ridx = 548; /* aranea */
				else if (die < 74) ridx = 609; /* drider of achrya */
				else if (die < 90) ridx = 653; /* elder aranea */
				else ridx = 590; /* dark elven sorcerer */
			}
			else
			{
				r_ptr = &r_info[742]; /* check for Achrya */
				ar_ptr = &r_info[663]; /* check for Eol */
				if ((die < 45) && (level > 62) &&
					(r_ptr->cur_num < r_ptr->max_num)) ridx = 742;
				else if (die < 50) ridx = 654; /* high priest of achrya */
				else if ((die < 70) && (level < 70) &&
					(ar_ptr->cur_num < ar_ptr->max_num)) ridx = 663;
				else if (die < 90) ridx = 755; /* pit fiend (not much really deep bugs) */
				else if (level < 70) ridx = 609; /* drider of achrya */
				else
				{
					r_ptr = &r_info[761]; /* check for The Tarrasque */
					if (r_ptr->cur_num < r_ptr->max_num) ridx = 761;
					else if (level == 125) ridx = 806; /* weird thing */
					else ridx = 796 + rand_int(3);
				}
			}
		}
		case 12: /* domain of the grepse */
		{
			if (level < 25)
			{
				if (die < 45) ridx = 316; /* pink elephant */
				else if (die < 80) ridx = 352; /* camel-dog */
				else ridx = 308; /* minidrake */
			}
			else if (level < 50)
			{
				if ((die < 35) && (level < 43)) ridx = 544; /* hungry ghost */
				else if (die < 45) ridx = 661; /* wemu vyrm */
				else if (die < 75) ridx = 585; /* rastlig */
				else ridx = 631; /* doppleganger */
			}
			else
			{
				if (die < 33) ridx = 679; /* grepse devil */
				else if (die < 65) ridx = 725; /* jabberwock */
				else if ((die > 90) && (level > 102)) ridx = 799; /* spreading silver vines */
				else ridx = 768; /* greater silver wemu */
			}
		}
		case 13: /* nightmare */
		{
			if (level < 27)
			{
				if (die < 70) ridx = 386; /* bogeyman */
				else ridx = 393; /* yellow skeleton */
			}
			else if (level < 37)
			{
				r_ptr = &r_info[405]; /* check for Draebor */
				if ((die < 30) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 405;
				else if ((die < 34) && (level < 35)) ridx = 421; /* hag */
				else if (die < 35) ridx = 411; /* ghost */
				else if (die < 65) ridx = 477; /* banshee */
				else if (die < 90) ridx = 483; /* black skeleton */
				else ridx = 491; /* deep shadow */
			}
			else if (level < 43)
			{
				r_ptr = &r_info[567]; /* check for Bel-Shamharoth */
				if ((die < 65) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 567;
				else if (die < 50) ridx = 583; /* beholder */
				else if (die < 90) ridx = 555; /* furies */
				else ridx = 589; /* scyllis */
			}
			else if (level < 70)
			{
				r_ptr = &r_info[825]; /* check for Phantom of Eilenel */
				if ((die < 30) && (level > 52) &&
					(r_ptr->cur_num < r_ptr->max_num)) ridx = 825;
				else if ((die < 45) && (level < 66)) ridx = 658; /* erinyes */
				else if (die < 45) ridx = 725; /* jabberwock */
				else if ((die < 70) && (level < 55)) ridx = 623; /* headless horseman */
				else if (die < 70) ridx = 710; /* dullahan */
				else if (level < 61) ridx = 670; /* high priest of Liart */
				else ridx = 637; /* undead beholder */
			}
			else
			{
				r_ptr = &r_info[770]; /* check for Liart */
				if (die < 30) ridx = 725; /* jabberwock */
				else if (die < 55) ridx = 743; /* horned reaper */
				else if (r_ptr->cur_num < r_ptr->max_num) ridx = 770;
				else if (die < 75) ridx = 748; /* black reaver */
				else ridx = 750; /* great wyrm of chaos */
			}
		}
		case 14: /* hell hall */
		{
			if (level < 22)
			{
				if (die < 50) ridx = 281; /* imp */
				else ridx = 308; /* minidrake */
			}
			else if (level < 37)
			{
				if (die < 20) ridx = 518; /* bodak */
				else if (die < 85) ridx = 509; /* hellhound */
				else if (level < 32) ridx = 436; /* doombat */
				else ridx = 571; /* wyvern */
			}
			else if (level < 50)
			{
				if (die < 25) ridx = 648; /* marilith */
				else if (die < 50) ridx = 617; /* black reaper */
				else if (die < 75) ridx = 658; /* erinyes */
				else if (level < 42) ridx = 589; /* scyllis */
				else if (level < 45) ridx = 630; /* nalfeshnee */
				else ridx = 666; /* lesser balrog */
			}
			else if (level < 70)
			{
				if (die < 35) ridx = 713; /* horned devil lord */
				else if (die < 70) ridx = 694; /* fallen archon */
				else ridx = 727; /* silver idol */
			}
			else
			{
				r_ptr = &r_info[736]; /* check for Baphomet */
				ar_ptr = &r_info[663]; /* check for Lungorthin */
				if ((die < 30) && (r_ptr->cur_num < r_ptr->max_num)) ridx = 736;
				else if ((die < 30) && (level < 90)) ridx = 743; /* horned reaper */
				else if ((die < 60) && (ar_ptr->cur_num < ar_ptr->max_num)) ridx = 663;
				else if (die < 45) ridx = 746; /* golden idol */
				else if (die < 70) ridx = 756; /* greater balrog */
				else ridx = 755; /* pit fiend */
			}
		}
	}

	return ridx;
}


/*
 * Require monster appropriate for monster temple
 * (monster temples on themed levels are handled separately)
 */
static bool temple_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* monster temple monsters are supposed to be tough */
	if ((r_ptr->level < p_ptr->depth - 2) && (!(r_ptr->flags1 & (RF1_UNIQUE))))
		return (FALSE);
	else if (r_ptr->level < (p_ptr->depth * 3) / 4) return (FALSE);

	/* monster temple monsters */
	if (r_ptr->flags7 & RF7_TEMPLE) return (TRUE);

	/* assume not okay */
	return (FALSE);
}


/*
 * Require zoo monster
 */
static bool zoo_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	int minl;

	/* minimum level */
	if (p_ptr->depth > 30) minl = (p_ptr->depth / 9);
	else minl = 2;
	if (r_ptr->level < minl) return (FALSE);

	/* Require animals */
	if (!(r_ptr->flags3 & RF3_ANIMAL)) return (FALSE);

	/* Require non-bugs */
	if (r_ptr->flags3 & RF3_BUG) return (FALSE);

	/* you don't go to the zoo to see rodents.. */
	if (strchr("r", r_ptr->d_char)) return (FALSE);

	/* most hounds by themselves are weak */
	/* and the zoo vault doesn't have space in the zoo cages for groups */
	if ((strchr("Z", r_ptr->d_char)) && (r_ptr->level < 35)) return (FALSE);

	/* all others are okay */
	return (TRUE);
}


/*
 * Require WATER_HIDE monster (easy one)
 */
static bool water_hide_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Require WATER_HIDE flag */
	if (r_ptr->flags7 & (RF7_WATER_HIDE)) return (TRUE);

	/* ..or WATER_ONLY flag */
	if (r_ptr->flags7 & (RF7_WATER_ONLY)) return (TRUE);

	return (FALSE);
}


/*
 * Require tree monster (easy one)
 */
static bool tree_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Require tree */
	if (strchr("E", r_ptr->d_char)) return (TRUE);

	return (FALSE);
}


/*
 * Helper function for get_mon_match()
 */
static bool match_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	int minl;
	bool isfine = FALSE;

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* okay to have umber hulks in vaults, but should never force multiple hulks */
	/* (allow in earth cave only) */
	if ((r_ptr->flags2 & (RF2_KILL_WALL)) && (p_ptr->theme != 5)) return (FALSE);

	/* minimum level */
	if (p_ptr->depth > 38) minl = (p_ptr->depth / 3) + 1;
	else minl = 14;
	if (r_ptr->level <= (minl / 2) + 1) return (FALSE);

	/* Hack -- Require certain types */
	if (strchr("OPTDMHKUVXYg%&", r_ptr->d_char)) isfine = TRUE;

	/* Hack -- other types require min level */
	if ((strchr("noNhqtx@pe", r_ptr->d_char)) && (r_ptr->level >= minl)) isfine = TRUE;
	if ((strchr("ABuzlR", r_ptr->d_char)) && (r_ptr->level >= minl + 3)) isfine = TRUE;

	/* okay if good for theme */
	if ((isfine) && (p_ptr->theme) && (theme_okay(r_idx, 0, TRUE))) return (TRUE);
	else if (p_ptr->theme) return (FALSE);

	/* allow most other types rarely (no theme) */
	if (strchr("v$Fjkm", r_ptr->d_char)) return (FALSE);
	if ((r_ptr->level >= minl + 2) && (randint(100) < 12)) return (TRUE);

	/* assume not okay */
	return (FALSE);
}

/*
 * Certain types of monsters more likely to be matching monsters in a vault
 * mode = 0 (default) gets a monster to be the matching vault monsters
 * mode = 1 gets a WATER_HIDE monster
 * mode = 2 gets a zoo appropriate monster
 * mode = 3 gets a monster temple monster
 * mode = 4 gets a tree monster
 */
int get_mon_match(int level, int mode)
{
	int r_idx;

	if ((p_ptr->theme) && (mode == 3))
	{
		return temple_theme(level);
	}
	else if (mode == 3)
	{
		/* Require appropriate monsters */
		get_mon_num_hook = temple_okay;
	}
	else if (mode == 4)
	{
		/* Require appropriate monsters */
		get_mon_num_hook = tree_okay;
	}
	else if (mode == 2)
	{
		/* Require appropriate monsters */
		get_mon_num_hook = zoo_okay;
	}
	else if (mode == 1)
	{
		/* Require appropriate monsters */
		get_mon_num_hook = water_hide_okay;
	}
	else
	{
		/* Require appropriate monsters */
		get_mon_num_hook = match_okay;
	}

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster */
	r_idx = get_mon_num(level, TRUE);

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	return r_idx;
}


/*
 * Hack -- fill in "vault" rooms
 *
 * Note new vault symbols:
 * d: matching monsters, D: matching tougher monsters
 * R: rubble, o: open pit
 * c: small chest, $: normal big chest, C: good chest, G: great chest
 * ~: water, W: WATER_HIDE monster in water
 * T: monster temple monster
 * Z: zoo monster (non-bug animal)
 */
static void build_vault(int y0, int x0, int ymax, int xmax, cptr data)
{
	int dx, dy, x, y;
	int dr_idx, bigdr_idx;

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

				/* Rubble */
				case 'R':
				{
					place_rubble(y, x);
					break;
				}

				/* open pit */
				case 'o':
				{
					cave_set_feat(y, x, FEAT_OPEN_PIT);
					break;
				}

				/* water */
				case '~':
				{
					cave_set_feat(y, x, FEAT_WATER);
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

				/* Place the following monsters before other monsters */
				/* to make sure other monster groups don't keep them */
				/* from being placed */
				/* tree or tree monster */
				case 'E':
				{
					int thismon = get_mon_match(p_ptr->depth + randint(9), 4);
					if (randint(100) < 94) place_monster_aux(y, x, 834, TRUE, FALSE);
					else place_monster_aux(y, x, thismon, TRUE, FALSE);
					break;
				}

				/* Nasty monster and treasure (monster temple) */
				case 'T':
				{
					int thismon = get_mon_match(p_ptr->depth + 20 + randint(20), 3);
					monster_race *r_ptr = &r_info[thismon];
					place_monster_aux(y, x, thismon, TRUE, TRUE);
					/* If it picked a WATER_HIDE monster, sometimes add a puddle */
					if ((r_ptr->flags7 & (RF7_WATER_HIDE)) && (randint(100) < 40))
						place_puddle(y, x, TRUE);
					object_level = p_ptr->depth + 15;
					place_object(y, x, TRUE, TRUE);
					object_level = p_ptr->depth;
					break;
				}
			}
		}
	}


	 /*  Matching monsters:  (requires appropriate theme monster if there's a theme) */
	/* Pick a monster for 'd' */
	dr_idx = get_mon_match(p_ptr->depth + 4, 0);

	/* Pick a monster for 'D' */
	bigdr_idx = get_mon_match(p_ptr->depth + 10, 0);

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
					place_monster(y, x, TRUE, TRUE, TRUE);
					monster_level = p_ptr->depth;
					break;
				}

				/* Meaner monster */
				case '@':
				{
					monster_level = p_ptr->depth + 11;
					place_monster(y, x, TRUE, TRUE, TRUE);
					monster_level = p_ptr->depth;
					break;
				}

				/* Zoo monster and treasure */
				case '1':
				{
					int thismon = get_mon_match(p_ptr->depth + 6 + randint(4), 2);
					place_monster_aux(y, x, thismon, TRUE, TRUE);
					object_level = p_ptr->depth + 5;
					place_object(y, x, TRUE, TRUE);
					object_level = p_ptr->depth;
					break;
				}

				/* WATER_HIDE monster in water */
				case 'W':
				{
					int thismon = get_mon_match(p_ptr->depth + 4 + randint(5), 1);
					cave_set_feat(y, x, FEAT_WATER);
					place_monster_aux(y, x, thismon, TRUE, TRUE);
					/* (not sure if I want an object here also) */
					break;
				}

				/* matching monster, possible treasure */
				case 'd':
				{
					place_monster_aux(y, x, dr_idx, TRUE, TRUE);
					if (randint(100) < 11)
					{
						object_level = p_ptr->depth + 4;
						place_object(y, x, FALSE, FALSE);
						object_level = p_ptr->depth;
					}
					break;
				}

				/* meaner matching monster, possible treasure */
				case 'D':
				{
					place_monster_aux(y, x, bigdr_idx, TRUE, TRUE);
					if (randint(100) < 11)
					{
						object_level = p_ptr->depth + 6;
						place_object(y, x, TRUE, FALSE);
						object_level = p_ptr->depth;
					}
					break;
				}

				/* Meaner monster, plus treasure */
				case '9':
				{
					monster_level = p_ptr->depth + 9;
					place_monster(y, x, TRUE, TRUE, TRUE);
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
					place_monster(y, x, TRUE, TRUE, TRUE);
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
						place_monster(y, x, TRUE, TRUE, TRUE);
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

				/* chests */
				case '$':
				case 'c':
				case 'C':
				case 'G':
				{
					int mode;
					object_level = p_ptr->depth + 5;
					if (*t == 'c') mode = 1;
					else if (*t == 'C') mode = 3;
					else if (*t == 'G') mode = 4;
					else mode = 2;
					place_chest(y, x, mode);
					object_level = p_ptr->depth;
					break;
				}
			}
		}
	}
}



/*
 * Hack -- fill in rooms for empty vaults (type9)
 * has chances to retain some aspects of original design (objects & traps)
 * but never has forced out-of-depth objects or monsters
 * and does not raise level feeling.
 *
 * int design determines how likely it is that the original design aspects
 * are created. In a vault design, original monsters and objects are rarely
 * created. In a type9 room shape design, it is much more likely.
 */
static void build_empty(int y0, int x0, int ymax, int xmax, cptr data, int design)
{
	int dx, dy, x, y, dsg, die;
	int goods = 0; /* don't create too many treasures in an empty vault */
	bool permwall = FALSE;
	bool light = FALSE;

	cptr t;


	/* Occasional light */
	if (p_ptr->depth < 14)
	{
		if (p_ptr->depth <= randint(14)) light = TRUE;
	}
	else if (p_ptr->depth <= 45)
	{
		if (randint(p_ptr->depth + 9) <= 2) light = TRUE;
	}
	else if (randint(p_ptr->depth + 15 + badluck/2 - goodluck/2) == 2) light = TRUE;

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

			/* empty vault does not get cave_icky */
			if (light) cave_info[y][x] |= (CAVE_ROOM | CAVE_GLOW);
			else cave_info[y][x] |= (CAVE_ROOM);

			/* Analyze the grid */
			switch (*t)
			{
				/* Granite wall (outer) */
				case '%':
				{
					cave_set_feat(y, x, FEAT_WALL_OUTER);
					break;
				}

				/* formerly permanent wall, small chance to become rubble */
				case 'X':
				{
					dsg = 6;
					if (design == 1) dsg = 34;
					if (rand_int(100) < dsg) /* about 1/16 */
					{
						if (design == 1) cave_set_feat(y, x, FEAT_PERM_INNER);
						else place_rubble(y, x);
					}
					else
					{
						cave_set_feat(y, x, FEAT_WALL_INNER);
					}
					/* there was permanent wall in the original vault design */
					if ((design > 1) && (design < 9)) permwall = TRUE;
					break;
				}

				/* Granite wall (inner), chance to become rubble */
				case '#':
				{
					die = rand_int(100);
					if (design == 1) dsg = 1;
					/* usually if there were permanent walls originally, */
					/* then the granite was meant like a door, */
					/* so make it rubble (or a door) instead of granite. */
					else if ((permwall) && (design == 9)) dsg = 34;
					else if (permwall) dsg = 85;
					/* lesser vault design like original */
					else if ((design == 2) || (design == 9)) dsg = 8;
					/* default about 1/6 chance to become rubble */
					else dsg = 16;
					if ((die < dsg/3) && (permwall) && 
						((design == 7) || (design == 2)))
					{
						place_random_door(y, x);
					}
					else if (((die < dsg/2) && (permwall)) || ((die < dsg/5) && (!permwall)))
					{
						cave_set_feat(y, x, FEAT_FLOOR);
					}
					else if (die < dsg)
					{
						place_rubble(y, x);
					}
					else
					{
						cave_set_feat(y, x, FEAT_WALL_INNER);
					}
					break;
				}

				/* Rubble */
				case 'R':
				{
					if (design == 1) dsg = 96;
					else if (design == 2) dsg = 80;
					else dsg = 65;
					if (rand_int(100) < dsg)
					{
						place_rubble(y, x);
					}
					break;
				}

				/* doors */
				case '+':
				{
					dsg = 75;
					if (design == 2) dsg = 50;
					if (design == 1) dsg = 70;
					if (rand_int(100) < dsg)
					{
						place_random_door(y, x);
					}
					else
					{
						place_secret_door(y, x);
					}
					break;
				}

				/* Treasure/trap */
				case '*':
				{
					if (design == 1) dsg = randint(8);
					else if (design == 2) dsg = randint(60);
					else if (design == 9) dsg = randint(80);
					else dsg = randint(100);
					if ((goods > 2) && (randint(100) < 67)) dsg += randint(goods-1);
					if (dsg < 5)
					{
						place_object(y, x, FALSE, FALSE);
						goods += 1;
					}
					else if (dsg < 8)
					{
						place_trap(y, x);
					}
					break;
				}

				/* Trap */
				case '^':
				{
					if (design == 1) dsg = 80;
					else if (design == 2) dsg = 40;
					else if (design == 8) dsg = 6;
					else if (design == 9) dsg = 15;
					else dsg = 10;
					if (rand_int(100) < dsg) place_trap(y, x);
					break;
				}

				/* open pit */
				case 'o':
				{
					if (design == 1) dsg = 96;
					else if (design == 2) dsg = 80;
					else if (design == 9) dsg = 75;
					else dsg = 55;
					if (rand_int(100) < dsg)
					{
						cave_set_feat(y, x, FEAT_OPEN_PIT);
					}
					/* rarely pit replaced by a puddle */
					else if (rand_int(100) < dsg/11)
					{
						cave_set_feat(y, x, FEAT_WATER);
					}
					break;
				}

				/* water with chance of water monster */
				/* FEAT_WATER must be placed before the monster because WATER_ONLY */
				/* monsters cannot be placed in a spot where there isn't water  */
				case 'W':
				{
					cave_set_feat(y, x, FEAT_WATER);
					if (design == 1) dsg = 80;
					else if (design == 2) dsg = 16;
					else if (design == 9) dsg = 10;
					else dsg = 8;
					if (rand_int(100) < dsg)
					{
						int thismon = get_mon_match(p_ptr->depth, 1);
						place_monster_aux(y, x, thismon, TRUE, TRUE);
					}
					break;
				}

				/* water */
				case '~':
				{
					cave_set_feat(y, x, FEAT_WATER);
					break;
				}

				/* tree or 2% chance of tree monster */
				case 'E':
				{
					int thismon = get_mon_match(p_ptr->depth, 4);
					dsg = 97;
					if ((design == 2) || (design == 8)) dsg = 94;
					if (rand_int(100) < dsg) place_monster_aux(y, x, 834, TRUE, FALSE);
					else place_monster_aux(y, x, thismon, TRUE, FALSE);
					break;
				}

				/* chests */
				case '$':
				case 'c':
				case 'C':
				case 'G':
				{
					int mode = randint(2); /* normal small or large chest */
					if (design == 1) dsg = 70;
					else if (design == 2) dsg = 20;
					else dsg = 10;
					dsg -= goods; /* prevent too much treasure in empty vault */
					if (rand_int(100) < dsg/10) /* small chance for special chest */
					{
						if ((*t == 'G') && (randint(100) < 12)) mode = 4;
						else if ((*t == 'C') || (*t == 'G')) mode = 3;
						else if (*t == 'c') mode = 1;
						else mode = 2;
						place_chest(y, x, mode);
						if (mode == 4) goods += 3;
						else goods += mode;
					}
					else if (randint(100) < dsg)
					{
						place_chest(y, x, mode);
						goods += mode;
					}
					else if (randint(100) < dsg + 21)
					{
						mode = 0; /* ruined chest */
						place_chest(y, x, mode);
					}
					break;
				}
			}
		}
	}
	/* Place a few things separately */
	/* (this should make sure monster groups don't keep trees from getting */
	/*  placed among other things.) */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the location */
			x = x0 - (xmax / 2) + dx;
			y = y0 - (ymax / 2) + dy;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Analyze the grid */
			switch (*t)
			{
				/* Treasure, sometimes monster */
				case ',':
				case '1':
				{
					if (design == 1) dsg = 49;
					else if (design == 2) dsg = 11;
					else if (design == 9) dsg = 5;
					else dsg = 4;
					if (randint(100) < dsg - 1) vault_monsters(y0, x0, 1);
					dsg -= rand_int(goods+1); /* prevent too much treasure in empty vault */
					if (randint(100) < dsg)
					{
						place_object(y, x, FALSE, FALSE);
						goods += 1;
					}
					break;
				}

				/* treasure & monster */
				case 'T':
				case '9':
				case '8':
				{
					if (design == 1) dsg = 70;
					else if ((design == 2) || (design == 9)) dsg = 16;
					else if (design == 8) dsg = 7;
					else dsg = 9;
					dsg -= (randint(goods) - 1); /* prevent too much treasure in empty vault */
					if (randint(100) < 2)
					{
						place_object(y, x, TRUE, FALSE);
						goods += 2;
					}
					else if (randint(100) < dsg)
					{
						place_object(y, x, FALSE, FALSE);
						goods += 1;
					}
					/* fall through */
				}
				/* might still be some monsters around */
				case '&':
				case '@':
				{
					if (design == 1) dsg = 80;
					else if (design == 2) dsg = 20;
					else if (design == 9) dsg = 12;
					else dsg = 9;
					die = randint(100);
					if ((die < dsg/2) && (*t == 'T') && ((design == 2) || (design == 8)))
					{
						int thismon = get_mon_match(p_ptr->depth + 4, 3);
						monster_race *r_ptr = &r_info[thismon];
						place_monster_aux(y, x, thismon, TRUE, TRUE);

						/* If it picked a WATER_HIDE monster, sometimes add a puddle */
						if ((r_ptr->flags7 & (RF7_WATER_HIDE)) && (randint(100) < 40))
								place_puddle(y, x, TRUE);
					}
					else if (die < dsg/2)
					{
						monster_level = p_ptr->depth + 2 + randint(4);
						place_monster(y, x, TRUE, TRUE, TRUE);
						monster_level = p_ptr->depth;
					}
					else if (die < dsg)
					{
						monster_level = p_ptr->depth + randint(2);
						place_monster(y, x, TRUE, TRUE, TRUE);
						monster_level = p_ptr->depth;
					}
					break;
				}
				/* d and D monsters always fit the current theme, if any */
				case 'D':
				case 'd':
				{
					int depth = p_ptr->depth - 1;
					if (design == 1) dsg = 80;
					else if (design == 2) dsg = 22;
					else if (design == 9) dsg = 14;
					else dsg = 10;
					if (*t == 'D') depth += 3;
					if (randint(100) < dsg)
					{
						int thismon = get_mon_match(depth, 0);
						monster_race *r_ptr = &r_info[thismon];

						place_monster_aux(y, x, thismon, TRUE, TRUE);
					}
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
	int rarity, tries = 0;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* just in case */
		tries++;

		/* Reject anything that's not a lesser vault */
		if (!(v_ptr->typ == 7)) continue;

		/* vault rarity */
		rarity = v_ptr->rare;

		/* never reject vault ideal for the theme */
		if ((p_ptr->theme) && (v_ptr->itheme == p_ptr->theme)) break;
		/* */
		else if (p_ptr->theme) rarity = rarity * 2;

		/* roll for vault design rarity */
		if ((rarity > 1) && (rand_int(100) < rarity) && (tries < 90)) continue;

		/* accept */
		break;
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
 * Type 9 -- empty vaults (see "vault.txt")
 *
 * Empty vaults use three types of designs
 * design type 9 = different room shape for variety
 * design type 7 = emptied lesser vault design (rarer before depth 12)
 * design type 8 = emptied greater vault design (minimum depth 80, very rare)
 * psuedo design type 1 = type 9 design created exactly as designed.
 * psuedo design type 2 = type 7 design created much more similar to original design
 */
static void build_type9(int y0, int x0, bool great)
{
	vault_type *v_ptr;
	byte mtv;
	int rarity, tries = 0;

	/* Pick a vault design */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* just in case */
		tries++;

		/* rarity (use empty vault designs more often) */
		rarity = v_ptr->rare;

		/* never reject vault ideal for the theme (if it's an empty vault design)*/
		if ((p_ptr->theme) && (v_ptr->itheme == p_ptr->theme) &&
			(((v_ptr->typ == 7) && (v_ptr->useMT == 2)) ||
			(v_ptr->typ == 9))) break;

		if ((v_ptr->typ == 7) && (rarity < 10) && (v_ptr->useMT < 2)) rarity = 10;
		/* useMT = 2 means lesser vault design can be used for empty vault more often */
		if ((v_ptr->typ == 7) && (v_ptr->useMT == 2) && (p_ptr->theme)) rarity += 10;
		else if ((v_ptr->typ == 7) && (v_ptr->useMT == 2)) rarity += 5;
		/* inflate rarity of type 7 designs */
		else if ((p_ptr->depth < 12) && (v_ptr->typ == 7))
		{
			rarity = rarity*7;
			if (rarity > 90) rarity = 90;
		}
		else if (v_ptr->typ == 7)
		{
			rarity = rarity*4;
			if (rarity > 75) rarity = 75;
		}
		else if ((v_ptr->typ == 9) && (p_ptr->theme))
		{
			rarity += 5;
		}
		else if ((v_ptr->typ == 8) && (v_ptr->useMT == 2) && (rarity > 5))
		{
			rarity -= 5;
		}
		else if ((v_ptr->typ == 8) && (v_ptr->useMT < 2))
		{
			rarity += 5;
		}

		/* roll for vault design rarity */
		if ((rarity > 1) && (rand_int(100) < rarity) && (tries < 90)) continue;

		/* empty greater vault */
		if (great)
		{
			if ((v_ptr->typ == 8) && (v_ptr->useMT)) break;
		}
		else /* v_ptr->useMT is 0 if design should not be used for empty vault */
		{
			/* Accept the first vault design usable for an empty vault */
			if (((v_ptr->typ == 9) || (v_ptr->typ == 7)) && (v_ptr->useMT)) break;
		}
	}

	/* Message */
	if (cheat_room) msg_format("Empty vault (%s)", v_name + v_ptr->name);

	/* type9 has a chance to be built (almost) exactly as designed because */
	/* type9 rooms are not designed to be true vaults */
	if ((v_ptr->typ == 9) && (randint(100) < 34))
	{
		mtv = 1;
	}
	/* version of lesser vault deisgn, mostly emptied, but more like original design */
	else if ((v_ptr->typ == 7) && (v_ptr->useMT == 2) && (randint(100) < 12))
	{
		mtv = 2;
	}

	/* Boost the rating if it's an empty greater vault because it's still going to have some stuff */
	if (mtv == 8) rating += v_ptr->rat/6;

	/* empty vaults created just as designed often have guaranteed treasure */
	/* (v_ptr->rat is often 0 for type9 designs anyway, but not always) */
	else if (mtv == 1) rating += v_ptr->rat;

	/* version of lesser vault deisgn, mostly emptied, but more like original design */
	else if (mtv == 2) rating += v_ptr->rat/4;


	/* Hack -- Build the empty vault */
	/* (Has chance to retain some aspects of original design, but not nearly */
	/*  as much monsters, treasure, and traps.) */
	build_empty(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, mtv);
}


/*
 * Type 8 -- greater vaults (see "vault.txt")
 */
static void build_type8(int y0, int x0)
{
	vault_type *v_ptr;
	int rarity, tries = 0;

	/* Pick a lesser vault */
	while (TRUE)
	{
		/* Get a random vault record */
		v_ptr = &v_info[rand_int(z_info->v_max)];

		/* rarity */
		rarity = v_ptr->rare;

		/* just in case */
		tries++;

		/* Reject anything that's not a greater vault */
		if (!(v_ptr->typ == 8)) continue;

		/* never reject vault ideal for the theme */
		if ((p_ptr->theme) && (v_ptr->itheme == p_ptr->theme)) break;
		else if (p_ptr->theme) rarity += 10;

		/* roll for vault design rarity */
		if ((rarity > 1) && (rand_int(100) < rarity) && (tries < 90)) continue;

		/* accept */
		break;
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
		case 10: build_type9(y, x, TRUE); break; /* empty greater vault (vrare) */
		case 9: build_type9(y, x, FALSE); break; /* normal empty vault */
		case 8: build_type8(y, x); break; /* greater vault */
		case 7: build_type7(y, x); break; /* lesser vault */
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

	/* Count "crowded" rooms (pits and nests) */
	if ((typ == 5) || (typ == 6)) dun->crowded = TRUE;

	/* Success */
	return (TRUE);
}


/* 
 * Places traps, rubble, and sometimes other terrain
 *  as appropriate for the level theme if any.
 * was part of cave_gen(), but I separated it into a handy separate function
 */
static void alloc_terrain(int k)
{
	int puddles, pits;
	int amt = k;

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k + 1));

	if (!p_ptr->theme)
	{
		/* Sometimes allow random puddles */
		if ((amt > 9) && (randint(100) < 17))
		{
			/* Put some puddles in the dungeon */
			puddles = 1 + randint(3);
			alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_WATER, puddles);
			/* (replaces some rubble) */
			amt -= puddles;
			if (cheat_room) msg_print("(random puddles placed)");
		}
		/* Sometimes allow random open pits */
		else if ((amt > 9) && (randint(100) < 17))
		{
			/* Put some open pits in the dungeon */
			pits = 1 + randint(2);
			alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OPEN_PIT, pits);
			/* (replaces some rubble) */
			amt -= pits;
			if (cheat_room) msg_print("(random pits placed)");
		}
		/* Sometimes allow random trees (rarer) */
		if ((amt > 6) && (randint(100) < 10 - (p_ptr->depth/9)))
		{
			/* Put some trees in the dungeon */
			int trees = 2 + randint(3);
			alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_TREES, trees);
			/* (replaces some rubble) */
			amt -= (trees-1);
			if (cheat_room) msg_print("(random trees placed)");
		}
	}
	/* put several puddles in the swamp (at least 6) */
	else if (p_ptr->theme == 9)
	{
		if (amt > 7) puddles = 5 + randint(amt-3);
		else puddles = 5 + randint(4);
		alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_WATER, puddles);
		/* (replaces some rubble) */
		amt -= puddles;
		if (amt < 1) amt = randint(2);
	}
	/* often place a pond or two in the forests (these puddles are usually bigger) */
	else if ((p_ptr->theme == 1) || (p_ptr->theme == 2))
	{
		puddles = rand_int(3);
		if (puddles)
		{
			alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_WATER, puddles);
			amt -= rand_int(puddles+1);
		}
	}
	/* allow puddles in the icky place and bug cave (most bugs like water) */
	else if (((p_ptr->theme == 11) || (p_ptr->theme == 3)) && (amt > 4))
	{
		/* Put some puddles in the dungeon (sometimes) */
		if (rand_int(100) < 45)
		{
			puddles = randint(3) + 2;
			alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_WATER, puddles);

			/* (replaces some rubble) */
			if (amt > puddles + 1) amt -= (puddles + 1);
			else amt = amt/2;
		}
	}

	/* Forests have trees */
	if ((p_ptr->theme == 1) || (p_ptr->theme == 2))
	{
		/* Put some trees in the dungeon */
		int trees = 5 + randint(12);
		alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_TREES, trees);
		/* (don't replace rubble because forests already have reduced rubble) */
	}

	/* These themed levels have less rubble */
	if ((p_ptr->theme == 1) || (p_ptr->theme == 2) || (p_ptr->theme == 6))
	{
		/* Put some rubble in corridors (less) */
		amt -= (2 + randint(3));
		/* (sometimes none at all) */
		if (amt > 1) alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(amt));
		if (amt < 1) amt = 1;
	}
	/* These themed levels have a very random amount of rubble */
	else if ((p_ptr->theme == 3) || (p_ptr->theme == 7) || (p_ptr->theme == 12) ||
			(p_ptr->theme == 13))
	{
		int rub = ((amt+1)/3) + randint(6);
		/* Put some rubble in corridors */
		alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, rub);
	}
	/* These themed levels have extra rubble */
	else if ((p_ptr->theme == 5) || (p_ptr->theme == 8) || (p_ptr->theme == 10))
	{
		if (p_ptr->theme == 5) amt += 1;
		/* Put some rubble in corridors */
		alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(amt));
		/* ..and other places too */
		if (p_ptr->theme != 8) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_RUBBLE, randint((amt + 2) / 3));
		alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_RUBBLE, randint(amt));
	}
	else
	{
		/* Put some rubble in corridors */
		alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(amt + 1));
	}
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static void cave_gen(void)
{
	int i, k, y, x, y1, x1;

	int by, bx, evg, smag, sqtz;

	bool destroyed = FALSE;

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


	/* Possible "destroyed" level */
	if ((p_ptr->depth > 10) && (rand_int(DUN_DEST - (badluck/3)) == 0)) destroyed = TRUE;

	/* Hack -- No destroyed "quest" levels */
	if (is_quest(p_ptr->depth)) destroyed = FALSE;

	/* possible themed level (see RF7 flags in defines.h) */
	if ((p_ptr->depth) && (!destroyed) && (themed_levels))
	{
		bool do_theme = FALSE;
		/* usually 7.5% chance */
		int themechance = 6 + randint(2);
		/* for testing *** themechance = 90; */
		/* less likely close to Morgoth */
		if ((p_ptr->depth > 92) && (p_ptr->depth < 99)) themechance = 4;
		if (is_quest(p_ptr->depth)) themechance = 0;
		/* less likely in very early levels */
		if (p_ptr->depth < 9) themechance = p_ptr->depth - 1;
		/* more likely deeper than Morgoth */
		if (p_ptr->depth > 100) themechance *= 2;
		/* roll for themed type */
		if (rand_int(100) < themechance) do_theme = TRUE;

		/* chose type: 14 types of themed levels */
		if (do_theme)
		{
			int dieth = rand_int(91 + ((p_ptr->depth*3)/4) + ((badluck+1)/2));
			/* occationally randomize more */
			if (randint(100) < 9) dieth = rand_int(136) + 2;
			if (dieth < 10) p_ptr->theme = 3; /* ICKY_PLACE */
			else if (dieth < 21) p_ptr->theme = 11; /* BUG_CAVE */
			else if (dieth < 32) p_ptr->theme = 2;  /* FFOREST */
			else if (dieth < 43) p_ptr->theme = 1; /* CFOREST */
			else if (dieth < 50) p_ptr->theme = 10; /* DWARF_MINE */
			else if (dieth < 61) p_ptr->theme = 7; /* FULL_MOON */
			else if (dieth < 70) p_ptr->theme = 5; /* EARTHY_CAVE */
			else if (dieth < 79) p_ptr->theme = 6; /* WINDY_CAVE */
			else if (dieth < 88) p_ptr->theme = 4; /* VOLCANO */
			else if (dieth < 98) p_ptr->theme = 9; /* SWAMP */
			else if (dieth < 110) p_ptr->theme = 8; /* CASTLE */
			else if (dieth < 120) p_ptr->theme = 13; /* NIGHTMARE */
			else if (dieth < 130) p_ptr->theme = 12; /* GREPSE */
			/* hell hall rare after dl105 */
			else if ((dieth < 140) && (p_ptr->depth <= 105)) p_ptr->theme = 14; /* hell hall */
			else p_ptr->theme = randint(14);
			/* scariest three themes never appear early */
			if ((p_ptr->theme >= 12) && (p_ptr->depth < 35)) p_ptr->theme = 0;
			/* the very last levels have restricted themes (50% EARTHY_CAVE) */
			if (p_ptr->depth > 121)
			{
				int blah = randint(8);
				if (blah == 1) p_ptr->theme = 13; /* NIGHTMARE */
				else if (blah == 2) p_ptr->theme = 12; /* GREPSE */
				else if (blah == 3) p_ptr->theme = 4; /* VOLCANO */
				else if (blah == 4) p_ptr->theme = 9; /* SWAMP */
				else p_ptr->theme = 5; /* EARTHY_CAVE */
			}
		}
		else p_ptr->theme = 0;
	}
	else p_ptr->theme = 0;

	/* consistant themed level: dl 105 is always hell */
	if ((themed_levels) && (p_ptr->depth == 105)) p_ptr->theme = 14;

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
		int roomodds;
		/* Pick a block for the room */
		by = rand_int(dun->row_rooms);
		bx = rand_int(dun->col_rooms);

#if 0
		/* Align dungeon rooms */
		if (dungeon_align)
		{
			/* Slide some rooms right */
			if ((bx % 3) == 0) bx++;

			/* Slide some rooms left */
			if ((bx % 3) == 2) bx--;
		}
#endif

        roomodds = DUN_UNUSUAL;
		/* make >100 levels more interesting */
		if ((p_ptr->depth == 101) || (p_ptr->depth == 110) ||
			(p_ptr->depth == 115) || (p_ptr->depth == 120) ||
			(p_ptr->depth == 125)) p_ptr->seek_vault = TRUE;
		/* should we look for a vault? (find vault spell or > 100 levels) */
		if (p_ptr->seek_vault)
        {
           /* make >100 levels more interesting */
		if (((p_ptr->depth == 101) || (p_ptr->depth == 110) ||
			(p_ptr->depth == 115) || (p_ptr->depth == 120) ||
			(p_ptr->depth == 125)) && (p_ptr->find_vault <= 40))
			   p_ptr->find_vault = 40;
			else if (p_ptr->depth > 100) p_ptr->find_vault += 5;
			roomodds -= p_ptr->find_vault;
           if (roomodds > 150) roomodds = 150;
           destroyed = FALSE;
        }
        else if (goodluck > 6) roomodds -= goodluck/2;

		/* Destroyed levels are boring */
		if (destroyed)
		{
			int pth = p_ptr->depth - 7;
			if (pth < 1) pth = 1;
			if (pth > 65) pth = 65;
			/* can use empty vault design */
			if ((rand_int(DUN_UNUSUAL) < pth) && (room_build(by, bx, 9)))
				continue;

			/* Attempt a "trivial" room */
			if (room_build(by, bx, 1)) continue;

			/* Never mind */
			continue;
		}

		/* Attempt an "unusual" room */
		/* (DUN_UNUSUAL is 200, set at L109) */
		/* roomodds = DUN_UNUSUAL unless find vault spell is used */
		if (rand_int(roomodds) < p_ptr->depth)
		{
			/* Roll for room type */
			if (p_ptr->seek_vault)
            {
               k = rand_int(100 - p_ptr->find_vault);
               if ((p_ptr->depth > 3) && (p_ptr->depth < 120))
               {
                  /* seek_vault uses inverted depth */
                  roomodds -= 120 - p_ptr->depth;
               }
               else roomodds -= (p_ptr->find_vault/3) + 1;
            }
			else if (goodluck > 6) k = rand_int(100 - goodluck/4);
			else k = rand_int(100);

			/* Attempt a very unusual room */
			/* (pits & nests are less common than before because of themed levels) */
			if (rand_int(roomodds) < p_ptr->depth)
			{
			    int endgamechaos, nestodd = 44 + (p_ptr->depth/12 - 2);
				if (nestodd > 50) nestodd = 50;
				
				/* find vault shouldn't find pits and nests as often */
			    if ((p_ptr->seek_vault) && (k < 50) && (k > 25) && (randint(100) < 55))
                   k = rand_int(20) + 5 - (goodluck/4);

				/* greater vaults more common after dl100 */
				/* handy for testing as well as making the >100 levels more interesting */
				if (p_ptr->depth == 101) endgamechaos = 40;
				else if (p_ptr->depth > 101) endgamechaos = p_ptr->depth - 96;
				else endgamechaos = 0;

				/* Type 8 -- Greater vault (10%) */
				if ((k < 10 + endgamechaos) && room_build(by, bx, 8)) continue;

				/* Type 7 -- Lesser vault (15%) */
				if ((k < 25 + endgamechaos) && room_build(by, bx, 7)) continue;

				/* Type 6 -- Monster pit (13%, was 15%) */
				if ((k < 38 + endgamechaos) && room_build(by, bx, 6)) continue;

				/* Type 5 -- Monster nest (6%, 8% or 9%, was 10%) */
				if ((k < nestodd + endgamechaos) && room_build(by, bx, 5)) continue;

				/* type 9 -- empty vault */
				if ((k < 50 + endgamechaos) && room_build(by, bx, 9)) continue;
			}

			/* Type 4 -- Large room (25%) */
			if ((k < 25) && room_build(by, bx, 4)) continue;

			/* Type 3 -- Cross room (25%) */
			if ((k < 50) && room_build(by, bx, 3)) continue;

			/* Type 2 -- Overlapping (50%) */
			if ((k < 100) && room_build(by, bx, 2)) continue;
		}

		/* Attempt an "unusual" room (again) */
		/* (DUN_UNUSUAL is 200, set at L109) */
		if (rand_int((DUN_UNUSUAL + roomodds)/2) < (p_ptr->depth/2) + 22)
		{
			if (p_ptr->depth < 60) k = rand_int(99) - ((60-p_ptr->depth) / 2);
			else k = rand_int(99);

			/* odds for empty greater vault (much more likely after L100) */
			if (p_ptr->depth > 100) evg = p_ptr->depth - 57;
			else if ((p_ptr->depth == 100) || (p_ptr->depth == 99) || (p_ptr->depth < 80)) 
				evg = 0;
			else evg = (p_ptr->depth-78) / 2;

			/* extremely rare empty greater vault (only deeper than dL70) */
			if ((k == 1) && (rand_int(100) < evg) && room_build(by, bx, 10)) continue;
			
			/* small second chance for lesser vault */
			if ((k == 1) && room_build(by, bx, 7)) continue;

			/* empty vault */
			if ((k < 27) && room_build(by, bx, 9)) continue;
		}

		/* Attempt a trivial room */
		if (room_build(by, bx, 1)) continue;
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

	/* more veins to mine in a mine */
	smag = DUN_STR_MAG;
	sqtz = DUN_STR_QUA;
	if (p_ptr->theme == 10)
	{
		int sdie = randint(100);
		if (sdie < 34) smag += 1;
		else if (sdie < 75) sqtz += 1;
	}

	/* Hack -- Add some magma streamers */
	for (i = 0; i < smag; i++)
	{
		/* in dwarf mine, extra streamer is less likely to have treasure */
		if (i == DUN_STR_MAG) build_streamer(FEAT_MAGMA, DUN_STR_MC + 10);
		else build_streamer(FEAT_MAGMA, DUN_STR_MC);
	}

	/* Hack -- Add some quartz streamers */
	for (i = 0; i < sqtz; i++)
	{
		/* in dwarf mine, extra streamer is less likely to have treasure */
		if (i == DUN_STR_QUA) build_streamer(FEAT_QUARTZ, DUN_STR_QC + 20);
		else build_streamer(FEAT_QUARTZ, DUN_STR_QC);
	}


	/* Destroy the level if necessary */
	if (destroyed) destroy_level();


	/* Place 3 or 4 down stairs near some walls */
	alloc_stairs(FEAT_MORE, rand_range(3, 4), 3);

	/* Place 1 or 2 up stairs near some walls */
	alloc_stairs(FEAT_LESS, rand_range(1, 2), 3);


	/* Basic "amount" (made more random) */
	k = (p_ptr->depth / 3);
	if (k > 9) k = 7 + randint(4);
	if (k < 3) k = 1 + randint(2);

	/* separated into another function */
	/* (makes terrain appropriate for the theme, if any) */
	alloc_terrain(k);

	/* Determine the character location */
	new_player_spot();

	/* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL + randint(8);
	/* themed levels more likely to have more monsters */
	if ((i < MIN_M_ALLOC_LEVEL + 7) && (p_ptr->theme)) i += randint(2);

	/* Put some monsters in the dungeon */
	for (i = i + k; i > 0; i--)
	{
		(void)alloc_monster(0, TRUE);
	}

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

					if (cave_can_occupy_bold(y, x)) break;
				}

				/* Place the questor */
				place_monster_aux(y, x, i, TRUE, TRUE);
			}
		}
	}


	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ROOM, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, Rand_normal(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal(DUN_AMT_GOLD, 3));

    /* reset vault seeking */
	if (p_ptr->seek_vault)
    {
       p_ptr->seek_vault = FALSE;
       p_ptr->find_vault = 0;
    }
    else if (p_ptr->find_vault >= 5) p_ptr->find_vault -= 5;

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
	x0 = xx * 14 + 12;

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
		y = rand_range(3, TOWN_HGT - 4);
		x = rand_range(3, TOWN_WID - 4);

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

	/* never a theme in the town */
	p_ptr->theme = 0;

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
	for (y = 1; y < TOWN_HGT - 1; y++)
	{
		for (x = 1; x < TOWN_WID - 1; x++)
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
		mon_max = 1;


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
		Term->offset_y = DUNGEON_HGT;
		Term->offset_x = DUNGEON_WID;


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

#if 0
		/* Hack -- Have a special feeling sometimes */
		if (good_item_flag && adult_no_preserve) feeling = 1;
#endif

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
		/* no longer has any effect on the 1st 4 levels */
		if (adult_autoscum && (num < 100) && (p_ptr->depth > 4))
		{
			/* Require "goodness" */
			if ((feeling > 8) ||
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
		wipe_mon_list();
	}


	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Remember when this level was "created" */
	old_turn = turn;
}



