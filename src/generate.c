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
#include "z-queue.h"

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
 * two grids apart.  This prevents large doorways.
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
#define DUN_DEST	41	/* 1/chance of having a destroyed level (was 30) */

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND	10	/* Chance of random direction */
#define DUN_TUN_CHG	29	/* Chance of changing direction */
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
#define ALLOC_TYP_STATUE    9   /* statues */
#define ALLOC_TYP_FOUNT     10  /* fountain */


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
#define ROOM_MAX	12



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
 * (these are only ever used in the room_build() function)
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
	{ -1, 2, -2, 3, 15 },	/* 8 = Greater vault (66x44) */
	{ 0, 1, -1, 1, 1 },		/* 9 = empty vault (uses lesser vault design) */
	{ 0, 1, -1, 1, 10 },	/* 10 = Medium vault (33x22) */
	{ -1, 2, -2, 3, 20 }	/* 11 = empty Greater vault (66x44) (extremely rare) */
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
 * mode 0: original use: only counts granite walls and permanent walls.
 * mode 1: counts all walls in all eight directions
 */
static int next_to_walls(int y, int x, int mode)
{
	int k = 0;
	
	if (mode == 1)
	{
		int yb, xb, i;
		/* Scan adjacent grids */
		for (i = 0; i < 8; i++)
		{
			/* Extract the location */
			yb = y + ddy_ddd[i];
			xb = x + ddx_ddd[i];

			/* count walls */
			if (cave_feat[yb][xb] >= FEAT_MAGMA) k++;
		}
	}
	else /* count only four directions */
	{
		/* only count granite walls and permanent walls */
		if (cave_feat[y+1][x] >= FEAT_WALL_EXTRA) k++;
		if (cave_feat[y-1][x] >= FEAT_WALL_EXTRA) k++;
		if (cave_feat[y][x+1] >= FEAT_WALL_EXTRA) k++;
		if (cave_feat[y][x-1] >= FEAT_WALL_EXTRA) k++;
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

	/* 9% chance of rubble object */
	/* (was 10%, changed because rubble is more common now) */
	/* (previously was not created until the rubble was dug up */
	/* now you can find it by searching as well as by digging) */
	if (rand_int(100) < 9)
	{
		object_type *o_ptr;

		/* create the object */
		if (randint(100) < 70) place_object(y, x, FALSE, FALSE);
		else place_gold(y, x);

		/* now hide it in the rubble */
		o_ptr = &o_list[cave_o_idx[y][x]];
		o_ptr->hidden = 1;

		/* only track certain categories of items */
		if ((ego_item_p(o_ptr)) || (artifact_p(o_ptr)) ||
			(o_ptr->tval == TV_SPECIAL) || (o_ptr->tval == TV_CHEST) ||
			(do_rating(o_ptr, TRUE)))
		{
			/* place object() tracks depth */
			/* generated in rubble in a vault */
			if (cave_info[y][x] & (CAVE_ICKY)) o_ptr->vcode = 8;
			/* generated in rubble elsewhere */
			else o_ptr->vcode = 3;
		}
	}

	/* Create big rocks (after hiding the rubble object) */
	big_rocks(y, x);
}


/*
 * Convert existing terrain type to water and make a puddle around it
 * mode == 0 normal puddle
 * mode == 1 puddle in vault
 * mode == 2 fountain (just water in the spaces adjacent)
 */
void place_puddle(int y, int x, int mode)
{
	int yb, xb;
	int i, spoty, spotx;
	int puddle_size = 3 + randint(3);
	/* cavern level */
	bool cavernlvl = FALSE;
	if ((p_ptr->speclev >= 1) && (p_ptr->speclev <= 2)) cavernlvl = TRUE;
	
	/* (no enlarged puddles in vaults) */
	if (!mode)
	{
		/* sometimes allow bigger puddles in a swamp */
		if ((p_ptr->theme == 9) && (randint(100) < 13)) puddle_size += 3 + randint(7);
		/* bigger usual puddle size in swamps */
		else if ((p_ptr->theme == 9) && (randint(100) < 75)) puddle_size += 1 + randint(4);
		/* in forests, they are ponds, not puddles (bigger and only 1-2 of them) */
		else if ((p_ptr->theme == 1) && (p_ptr->theme == 2) && (cavernlvl))
			puddle_size += 10 + randint(8);
		else if ((p_ptr->theme == 1) && (p_ptr->theme == 2))
			puddle_size += 6 + randint(8);
		/* in a cavern with more open space, make puddles bigger */
		else if (cavernlvl) puddle_size += 2 + rand_int(3);
		if ((p_ptr->theme == 9) && (cavernlvl)) puddle_size += 1 + randint(4);
	}
	else if (mode == 2) puddle_size = 8 + rand_int(3);
	/* else mode is 1, puddle is in a vault */

	/* water */
	cave_set_feat(y, x, FEAT_WATER);
	
	/* fountain */
    if ((mode == 2) || ((puddle_size > 5) && (puddle_size < 14) && (randint(100) < 3)))
	{
		/* Put water in adjacent grids */
		for (i = 0; i < 8; i++)
		{
			/* Extract the location */
			yb = y + ddy_ddd[i];
			xb = x + ddx_ddd[i];

			/* out of bounds */
			if (!in_bounds_fully(yb, xb)) continue;

			/* don't flood the stairs (but can flood traps) */
			if ((cave_feat[yb][xb] == FEAT_LESS) || (cave_feat[yb][xb] == FEAT_MORE))
			    continue;

			/* make water */
			if (cave_floor_bold(yb, xb))
			{
				cave_set_feat(yb, xb, FEAT_WATER);
				puddle_size -= 1;
			}
			else if (randint(100) < 3) puddle_size -= 1;
		}
	}

	/* make a puddle (method 1) */
	if (((puddle_size > 6) && (rand_int(100) < 91)) || 
		((puddle_size) && (rand_int(100) < 41)))
	{
		for (yb = (y - 2); yb <= (y + 2); yb++)
		{
			for (xb = (x - 2); xb <= (x + 2); xb++)
			{
				if (puddle_size)
				{
                	/* out of bounds */
					if (!in_bounds_fully(yb, xb)) continue;

					/* don't flood the stairs (but can flood traps) */
					if ((cave_feat[yb][xb] == FEAT_LESS) || (cave_feat[yb][xb] == FEAT_MORE))
					    continue;
					/* already water there, don't be redundant */
					if (cave_feat[yb][xb] == FEAT_WATER) continue;

					/* occationally skip a spot so that the shape isn't too predicatble */
					if (rand_int(100) < 16 - (puddle_size*3)) continue;

					/* make water */
					if (cave_floor_bold(yb, xb))
					{
						cave_set_feat(yb, xb, FEAT_WATER);
					
						puddle_size -= 1;
					}
					
					/* occationally finish the puddle using the other method */
					if ((puddle_size) && (rand_int(100) < 7 - puddle_size)) break;
				}
			}
		}
    }
				
	/* make a puddle (method 2) */
	if (puddle_size)
	{
		for (i = 0; i < puddle_size; i++)
		{
			/* (checks for an appropriate place to put water) */
			if (get_nearby(y, x, &spoty, &spotx, 2))
			{
				cave_set_feat(spoty, spotx, FEAT_WATER);
			
				/* sometimes set new starting point */
				if (randint(100) < 34)
				{
					y = spoty;
					x = spotx;
				}
			}
			else if (randint(100) < 90)
			{
				/* try again */
				i -= 1;
			}
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
				if (next_to_walls(y, x, 0) < walls) continue;

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
 * Allocates some objects/terrain (using "place" and "type")
 */
static void alloc_object(int set, int typ, int num)
{
	int y, x, k, walls = 0;
	/* cavern level */
	bool cavernlvl = FALSE;
	if ((p_ptr->speclev >= 1) && (p_ptr->speclev <= 2)) cavernlvl = TRUE;
	/* in a cavern */
	if (cavernlvl) walls = 3;

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
			/* otherwise Require "naked" floor grid */
			else if (!cave_naked_bold(y, x)) continue;

			/* Check for room */
			/* doorways no longer have CAVE_ROOM so that trees and statues */
			/* (which use ALLOC_SET_ROOM) will never be placed in doorways */
			room = (cave_info[y][x] & (CAVE_ROOM)) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

			/* try to place treasure next to walls in a cavern (walls is 0 when not in a cavern) */
			if (((typ == ALLOC_TYP_OBJECT) || (typ == ALLOC_TYP_GOLD)) &&
				(next_to_walls(y, x, 1) < walls)) continue;

			/* never randomly put puddles or trees in a vault */
			if (((typ == ALLOC_TYP_WATER) || (typ == ALLOC_TYP_TREES)) && 
				(cave_info[y][x] & (CAVE_ICKY)))
				continue;

			/* don't put statues or trees in pits */
            if ((cave_feat[y][x] == FEAT_OPEN_PIT) &&
				((typ == ALLOC_TYP_STATUE) || (typ == ALLOC_TYP_TREES) ||
                (typ == ALLOC_TYP_FOUNT))) continue;

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
				place_puddle(y, x, 0);
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

			case ALLOC_TYP_STATUE:
			{
				int smallchance = 40;
				/* small (dwarf) statues more common in DWARF_MINE */
				if (p_ptr->theme == 10) smallchance = 70;
				/* slp flag for statues is used to tell if it's ruined */
				if (randint(100) < 2) place_monster_aux(y, x, 910, TRUE, FALSE);
				else if (randint(100) < 2) place_monster_aux(y, x, 909, TRUE, FALSE);
				/* 909 statue blocks line of sight, 910 (small) statue doesn't */
				else if (randint(100) < smallchance) 
					place_monster_aux(y, x, 910, FALSE, FALSE);
				else place_monster_aux(y, x, 909, FALSE, FALSE);
				break;
			}
			
			case ALLOC_TYP_FOUNT:
			{
				/* slp flag for statues is used to tell if it's ruined */
				if (randint(100) < 2) place_monster_aux(y, x, 911, TRUE, FALSE);
				else
				{
					/* mode 2 is for fountains: place water in adjacent spaces */
					place_puddle(y, x, 2);
					place_monster_aux(y, x, 911, FALSE, FALSE);
				}
                break;
			}
		}
	}
}



/*
 * Places "streamers" of rock through dungeon
 *
 * Note that there are actually six different terrain features used
 * to represent streamers.  Three each of magma and quartz, one for
 * basic vein, one with hidden gold, and one with known gold.  The
 * hidden gold types are currently unused.
 * 
#ifdef allow_rivers
 * This is now also used for making rivers. (not defined- didn't work very well)
 * dhei and dwid are only used when making a river in a cavern
#endif
 */
static void build_streamer(int feat, int chance, int dhei, int dwid)
{
	int i, tx, ty;
	int y, x, dir, odir, skips = 0, adv = 0;
	int sdensity = DUN_STR_DEN; /* 5 */
	int swidth = DUN_STR_RNG; /* 2 */
	bool tenddir = FALSE;

	if (feat == FEAT_WATER)
	{
		sdensity = 3;
		swidth = 1;
		if (rand_int(100) < 50) tenddir = TRUE;
	}

	if ((dhei) && (dwid))
	{
		/* Hack -- Choose starting point */
		y = rand_spread(dhei / 2, 12);
		x = rand_spread(dwid / 2, 17);
	}
	else
	{
		/* Hack -- Choose starting point */
		y = rand_spread(DUNGEON_HGT / 2, 10);
		x = rand_spread(DUNGEON_WID / 2, 15);
	}

	/* Choose a random compass direction */
	dir = ddd[rand_int(8)];
	odir = dir;

	/* Place streamer into dungeon */
	while (TRUE)
	{
		/* One grid per density */
		for (i = 0; i < sdensity; i++)
		{
			if ((skips > 6) && (feat == FEAT_WATER)) return;
			skips += 1;
			if ((!(i == 1)) || (!(feat == FEAT_WATER)))
			{
				/* Pick a nearby grid */
				while (1)
				{
					ty = rand_spread(y, swidth);
					tx = rand_spread(x, swidth);
					if (!in_bounds(ty, tx)) continue;
					break;
				}
			}
			else if (randint(100) < 96)
			{
				ty = y;
				tx = x;
			}
			else continue;

			if (feat == FEAT_WATER)
			{
				/* chance to end the river if it hits a vault */
				if (((cave_feat[ty][tx] > FEAT_WALL_SOLID) ||
					(cave_info[ty][tx] & (CAVE_ICKY))) && (randint(100) < 4)) return;
				/* FEAT_WALL_SOLID is near a doorway, > is permanent rock */
				if (cave_feat[ty][tx] >= FEAT_WALL_SOLID) continue;
				if (cave_info[ty][tx] & (CAVE_ICKY)) continue;
				if (((cave_feat[ty][tx] >= FEAT_DOOR_CLOSE) &&
		          (cave_feat[ty][tx] <= FEAT_DOOR_STUCK)) ||
					(cave_feat[ty][tx] == FEAT_SECRET)) continue;
				/* if ((i == 2) && (randint(100) < 6)) continue; */
				/* don't flood a space with a statue (trees are allowed) */
				if (cave_m_idx[ty][tx] > 0)
				{
					monster_type *n_ptr = &mon_list[cave_m_idx[ty][tx]];
					monster_race *nr_ptr = &r_info[n_ptr->r_idx];
					if ((nr_ptr->flags7 & (RF7_NONMONSTER)) &&
						(nr_ptr->flags3 & (RF3_NON_LIVING))) continue;
				}
				/* less likely to flood room walls */
				if ((cave_feat[ty][tx] == FEAT_WALL_OUTER) && (randint(100) < 55))
					continue;
			}
			else
			{
				/* Only convert granite walls */
				if (cave_feat[ty][tx] < FEAT_WALL_EXTRA) continue;
				if (cave_feat[ty][tx] > FEAT_WALL_SOLID) continue;
			
				/* Pillar mimmics are generated in granite, don't add a mineral here */
			    if (cave_m_idx[ty][tx]) continue;
			}

			/* Clear previous contents, add proper vein type */
			cave_set_feat(ty, tx, feat);
			skips = 0; /* reset skips on a success */

			/* Hack -- Add some (known) treasure (chance = 0 when used for trees) */
			if ((chance) && (rand_int(chance) == 0)) cave_feat[ty][tx] += 0x04;
		}

		/* don't let a river take up the whole level */
		if ((feat == FEAT_WATER) && (adv > 200)) return;

		/* Advance the streamer */
		y += ddy[dir];
		x += ddx[dir];
		adv += 1;
		/* change direction occationally (rivers aren't straight) */
		if ((feat == FEAT_WATER) && (randint(100) < 15))
		{
			/* let it tend toward the direction it started in */
			if ((!(dir == odir)) && (randint(100) < 30)) dir = odir;
			else if (randint(100) < 75)
			{
				if (tenddir) dir += 1;
				else dir -= 1;
			}
			else 
			{
				if (tenddir) dir -= 1;
				else dir += 1;
			}
		}
		else if (randint(100) < 70)
		{
			/* going horizontally doesn't seem to work well */
			if (dir == 4) dir = 7;
			if (dir == 6) dir = 3;
		}

		/* Stop at dungeon edge */
		if (!in_bounds(y, x)) return;

		/* outside of cavern */
		if ((dhei) && (dwid) && ((y > dhei+4) || (x > dwid+4))) return;
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
				delete_monster(y, x, FALSE);

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
	int y, x;

	if (light)
	{
	    for (y = y1; y <= y2; y++)
		{
			for (x = x1; x <= x2; x++)
			{
				/* cave_info[y][x] |= (CAVE_ROOM); */
				/* if (light) */ cave_info[y][x] |= (CAVE_GLOW);
			}
		}
	}
	/* Room walls & doorways shouldn't be considered part of the room */
	/* (see temp_aux_aux() in spells2.c) */
	y1 += 1;
	y2 -= 1;
	x1 += 1;
	x2 -= 1;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_info[y][x] |= (CAVE_ROOM);
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
	
	/* for rooms in caverns */
	if ((p_ptr->speclev >= 1) && (p_ptr->speclev <= 2) &&
		(feat == FEAT_WALL_OUTER))
	{
		bool doorsa = FALSE;
		bool doorsb = FALSE;
		bool anydoor = FALSE;
		int nddoor = 11;
		if (p_ptr->speclev == 2) nddoor = 7;

        /* create extra walls to help door placement */
		for (y = y1; y <= y2; y++)
		{
			cave_set_feat(y, x1+1, feat);
			cave_set_feat(y, x2-1, feat);
		}
	
		for (x = x1; x <= x2; x++)
	 	{
	 		cave_set_feat(y1+1, x, feat);
	  		cave_set_feat(y2-1, x, feat);
	  	}

	  	
		for (y = y1+1; y <= y2; y++)
		{
            if (!in_bounds_fully(y, x1)) continue;
			if ((next_to_floor(y, x1)) && (!doorsa))
				{ place_random_door(y, x1, FALSE); doorsa = TRUE; }
            if (!in_bounds_fully(y, x2)) continue;
			if ((next_to_floor(y, x2)) && (!doorsb))
				{ place_random_door(y, x2, FALSE); doorsb = TRUE; }
			if (doorsa || doorsb) anydoor = TRUE;
			if (randint(100) < nddoor) { doorsa = FALSE; doorsb = FALSE; }
		}
		doorsa = FALSE;
		doorsb = FALSE;

		for (x = x1+1; x <= x2; x++)
		{
            if (!in_bounds_fully(y1, x)) continue;
			if ((next_to_floor(y1, x)) && (!doorsa))
				{ place_random_door(y1, x, FALSE); doorsa = TRUE; }
            if (!in_bounds_fully(y2, x)) continue;
			if ((next_to_floor(y2, x)) && (!doorsb))
				{ place_random_door(y2, x, FALSE); doorsb = TRUE; }
			if (doorsa || doorsb) anydoor = TRUE;
			if (randint(100) < nddoor) { doorsa = FALSE; doorsb = FALSE; }
		}
		/* alert no door */
		if (!anydoor) p_ptr->speclev = 3;
		/* if we're making an overlapping room */
		else if (p_ptr->speclev == 3) p_ptr->speclev = 1;
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
 * basic room types:
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

	if (randint(100) < 11) /* more vertical room */
	{
		y1 = y0 - randint(5);
		x1 = x0 - randint(8);
		y2 = y0 + (1 + randint(3));
		x2 = x0 + randint(9);
	}

	/* Generate new room */
	generate_room(y1-1, x1-1, y2+1, x2+1, light);

	/* Generate outer walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);


	/* Occasional pillar room */
	if (rand_int(20) == 0)
	{
		bool pmimmic = FALSE;
        for (y = y1; y <= y2; y += 2)
		{
			for (x = x1; x <= x2; x += 2)
			{
				/* possible pillar mimmic */
				int mchance = 500;
				if (p_ptr->depth >= 75) mchance -= (p_ptr->depth - 70) * 2;
                if ((randint(mchance) == 1) && (p_ptr->depth >= 40) && (!pmimmic)) 
				{
					place_monster_aux(y, x, 900, TRUE, FALSE);
					/* usually forbid a second in the same room */
					if (randint(100) < 91) pmimmic = TRUE;
				}
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
				/* possible pillar mimmic */
                if ((randint(500) == 1) && (p_ptr->depth >= 50)) 
				{
					place_monster_aux(y0, x0, 900, TRUE, FALSE);
				}
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
			generate_hole(y0-1, x0-1, y0+1, x0+1, FEAT_DOOR_LOCKD);

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
			bool pmimmic = FALSE;
			/* Open the inner room with a secret door */
			generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

			/* Checkerboard */
			for (y = y1; y <= y2; y++)
			{
				for (x = x1; x <= x2; x++)
				{
					if ((x + y) & 0x01)
					{
						int mchance = 500;
						if (p_ptr->depth >= 75) mchance -= (p_ptr->depth - 70) * 2;
                        /* possible pillar mimmic */
						if ((randint(mchance) == 1) && (p_ptr->depth >= 40) && (!pmimmic)) 
						{
							place_monster_aux(y, x, 900, TRUE, FALSE);
							/* usually forbid a second in the same room */
							if (randint(100) < 90) pmimmic = TRUE;
						}
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
 * (DJA: actually it doesn't appear to check for the MULTIPLY flag?)
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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Require worm, jelly, slime, or mushroom */
	if (!strchr("wjS,", r_ptr->d_char)) return (FALSE);
	
	/* grepse (silver monsters) less common in jelly nests */
    if ((r_ptr->flags3 & (RF3_SILVER)) && (rand_int(100) < 53))
       return (FALSE);

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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Require animal flag */
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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Hack -- Require "acrFIJS" monsters */
	if (!strchr("acrFIJS", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (hydra)"
 */
static bool vault_aux_hydra(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Hack -- Require "M" monsters */
	if (!strchr("M", r_ptr->d_char)) return (FALSE);

	/* (usually) exclude the rastlig and jabberwock */
	if (!((badluck > 7) || ((goodluck < 7) && (p_ptr->depth > 90))))
	{
		if (r_ptr->flags3 & (RF3_SILVER)) return (FALSE);
		if (strstr(rname, "abberwoc")) return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (golem)"
 */
static bool vault_aux_golem(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* include another construct type */
	if ((randint(100) < 50) && (strstr(rname, "assault sphere"))) return (TRUE);

	/* Hack -- Require "g" monsters */
	if (!strchr("g", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (lesser undead)"
 */
static bool vault_aux_zombie(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* (usually) exclude drujs */
	if ((strchr("s", r_ptr->d_char)) && (r_ptr->level > 55) &&
		(p_ptr->depth < 96)) return (FALSE);
	/* allow all Vs deep */
	if ((p_ptr->depth >= 96) && (strchr("V", r_ptr->d_char))) return (TRUE);

	/* Hack -- Require "Gsz" monsters and allow lesser Vs */
	if ((!strchr("Gsz", r_ptr->d_char)) && 
		(!((strchr("V", r_ptr->d_char)) && (r_ptr->level < 30)))) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (abominations)"
 */
static bool vault_aux_abomination(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);

	/* Decline unique monsters and breeders */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);
	if (r_ptr->flags2 & (RF2_MULTIPLY)) return (FALSE);

	/* include these specific monsters */
	if (strstr(rname, "harpy")) return (TRUE);
	if (strstr(rname, "glarx")) return (TRUE);
	if (strstr(rname, "gorgon")) return (TRUE);
	if (strstr(rname, "ind flayer")) return (TRUE);
	if (strstr(rname, "wereraven")) return (TRUE);
	if (strstr(rname, "winged horror")) return (TRUE);
	if (strstr(rname, "abberwoc")) return (TRUE);

	/* allow the occational Q, L, or N */
	if ((strchr("Q", r_ptr->d_char)) && (randint(100) < 10)) return (TRUE);
	if ((strchr("L", r_ptr->d_char)) && (randint(100) < 11)) return (TRUE);
	if ((strchr("N", r_ptr->d_char)) && (randint(100) < 11)) return (TRUE);

	/* else require silver or "en" monsters (eyes and nagas) */
	if (r_ptr->flags3 & (RF3_SILVER)) return (TRUE);
	if (!strchr("en", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (evil humans)"
 */
static bool vault_aux_warlock(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* exclude any humans which are never evil (paladins, knight templar, etc) */
	/* priests can be evil -who knows who/what they're a priest of? */
	if (!((r_ptr->flags3 & (RF3_EVIL)) || (r_ptr->flags2 & (RF2_S_EVIL1)) ||
		(r_ptr->flags2 & (RF2_S_EVIL1))))
		return (FALSE);

	/* exclude the sullen drunkard (dL1) and other too-shallow stuff */
	if ((r_ptr->level < 2) || ((p_ptr->depth > 59) && (r_ptr->level < 10))) return (FALSE);

	/* headless horsemen are Ks but not humans */
	if ((strstr(rname, "headless")) || (strstr(rname, "dullahan"))) return (FALSE);
	/* hags ("t") are closer to human at least, so allow them */

	/* otherwise require "Kpt" monsters */
	if (!strchr("Kpt", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (evil humanoid)"
 */
static bool vault_aux_drow(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* exclude any monsters which are never evil */
	if (!((r_ptr->flags3 & (RF3_EVIL)) || (r_ptr->flags2 & (RF2_S_EVIL1)) ||
		(r_ptr->flags2 & (RF2_S_EVIL1))))
		return (FALSE);

	/* exclude the small kobold and other too-shallow monsters */
	if ((r_ptr->level < 2) || (r_ptr->level < p_ptr->depth/10)) return (FALSE);

	/* (usually) exclude mind flayers */
	if ((strstr(rname, "ind flayer")) && (randint(100) < 75)) return (FALSE);

	/* evil humanoids */
	if (!((strchr("@hk", r_ptr->d_char)) || (strstr(rname, "snakeman")) ||
		(strstr(rname, "hag")) || (strstr(rname, "headless")) ||
		(strstr(rname, "dullahan")))) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (water)"
 */
static bool vault_aux_aquarium(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);
		
	/* all nulls are water monsters */
	/* though some are too big to hide in shallow water */
	if (strchr("N", r_ptr->d_char)) return (TRUE);

	/* otherwise require WATER_HIDE or WATER_ONLY */
	if (!((r_ptr->flags7 & (RF7_WATER_HIDE)) ||
		(r_ptr->flags7 & (RF7_WATER_ONLY)))) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (imps & fairies)"
 */
static bool vault_aux_fairy(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	cptr dname = (r_name + r_ptr->name);

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Decline HELPER monsters */
	if (r_ptr->flags3 & (RF3_HELPER)) return (FALSE);

	/* almost all types of FEY, but exclude the headless horsemen */
	if ((strstr(dname, "headless")) || (strstr(dname, "dullahan"))) return (FALSE);

	/* otherwise require fairies */
	if (!(r_ptr->flags3 & (RF3_FEY))) return (FALSE);

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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);
	
	/* on NIGHTMARE themed levels (only), allow some non-undead nightmare monsters */
	if ((r_ptr->flags7 & (RF7_DARK_CITY)) && (p_ptr->theme == 13) &&
		(!strchr("ABJMZbptx", r_ptr->d_char))) return (TRUE);

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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);
	
	/* don't allow wimpy monsters */
    if (r_ptr->level < 5) return (FALSE);
	
	/* allow only deep monsters of some types */
	/* (allows hellhound, greater werewolf, behemoths) */
    if ((strchr("CR", r_ptr->d_char)) && (r_ptr->level >= 35)) return (TRUE);
	/* (allows giant python- only snake of L27 or deeper. we should have more IMO.) */
    if ((strchr("J", r_ptr->d_char)) && (r_ptr->level >= 27)) return (TRUE);
	/* (allows wereraven & winged horror) */
    if ((strchr("B", r_ptr->d_char)) && (r_ptr->level >= 46)) return (TRUE);
	/* (allows behir, guardian naga & goldern naga) */
    if ((strchr("n", r_ptr->d_char)) && (r_ptr->level >= 15)) return (TRUE);

	/* Otherwise require "MHlqN" monsters */
	/* (hydras, hybrids, lizards, quadrapeds & nulls) */
	if (!strchr("MHlqN", r_ptr->d_char)) return (FALSE);

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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Decline helpers and breeders */
	if (r_ptr->flags3 & (RF3_HELPER)) return (FALSE);
	if (r_ptr->flags2 & (RF2_MULTIPLY)) return (FALSE);

	/* Require appropriately themed monsters */
	if (!theme_okay(r_idx, 0, TRUE)) return (FALSE);

	/* often reject never_move monsters */
	if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) && (rand_int(100) < 75))
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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Hack -- Require orcs */
	if (!strchr("o", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (barracks)"
 */
static bool vault_aux_army(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Hack -- Require army monsters */
	if (!(r_ptr->flags7 & (RF7_ARMY))) return (FALSE);

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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Hack -- Require trolls */
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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Hack -- Require "d" or "D" monsters */
	if (!strchr("Dd", r_ptr->d_char)) return (FALSE);

	/* how can I allow BR_FEAR here? */
	/* (because great wyrms should breathe fear rather than casting SCARE) */
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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* Decline HELPER monsters */
	if (r_ptr->flags3 & (RF3_HELPER)) return (FALSE);

	/* require "uU&" monsters */
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
 * appropriate non-unique monsters for the nest.
 *
 * Currently, a monster nest is one of
 * 1 a nest of jelly monsters   (Dungeon level 5 and deeper)
 * 2 a nest of creepy crawly monsters (Dungeon level 15 and deeper)
 * 3 a nest of evil humans (Kpt)
 * 4 a nest of evil humanoids (@hk)
 * 5 a nest of imps and fairy monsters (Dungeon level 25 and deeper)
 * 6 a nest of water monsters (rare)
 * 7 a nest of animal monsters  (Dungeon level 30 and deeper)
 * 8 a nest of great beast monsters (Dungeon level 38 and deeper)
 * 9 a nest of undead monsters  (Dungeon level 50 and deeper)
 * 11 a nest of lesser undead monsters
 * 12 a nest of abominations (eyes, nagas, grepse, etc)
 *
 * Note that the "get_mon_num()" function may (rarely) fail, in which
 * case the nest will be empty, and will not affect the level rating.
 *
 * Note that "monster nests" will never contain "unique" monsters.
 */
static void build_type5(int y0, int x0)
{
	int y, x, y1, x1, y2, x2, xfac, yfac, yfacb;
	int tmp, i, nsize;
	bool prize = FALSE;
	int nesttyp;
	/* some nests use a fake theme */
	byte realtheme = p_ptr->theme;

	s16b what[64];

	cptr name;

	bool empty = FALSE;
	bool special = FALSE;
	bool needwater = FALSE;

	int light = FALSE;

	/* new: variable sized nests */
    int size = rand_int(100);
    if ((p_ptr->speclev >= 1) && (p_ptr->speclev <= 2)) size = rand_int(87);
    if (size < 8)
	{
		/* very small room */
		y1 = y0 - 2;
		y2 = y0 + 3;
		x1 = x0 - 3;
		x2 = x0 + 3;
		yfac = 0;
		yfacb = 1;
		xfac = 1;
		nsize = 0;
	}
    else if (size < 30)
	{
		/* small room */
		y1 = y0 - 3;
		y2 = y0 + 3;
		x1 = x0 - 4;
		x2 = x0 + 4;
		yfac = 1;
		yfacb = 1;
		xfac = 2;
		nsize = 1;
	}
	else if (size < 38)
	{
		/* medium-small room */
		y1 = y0 - 4;
		y2 = y0 + 3;
		x1 = x0 - 5;
		x2 = x0 + 5;
		yfac = 2;
		yfacb = 1;
		xfac = 3;
		nsize = 2;
	}
	else if (size < 58)
	{
		/* medium-sized room */
		y1 = y0 - 4;
		y2 = y0 + 4;
		x1 = x0 - 6;
		x2 = x0 + 6;
		yfac = 2;
		yfacb = 2;
		xfac = 4;
		nsize = 3;
	}
	else if (size < 73)
	{
		/* vertical medium-sized room */
		y1 = y0 - 7;
		y2 = y0 + 6;
		x1 = x0 - 3;
		x2 = x0 + 3;
		yfac = 5;
		yfacb = 4;
		xfac = 1;
		nsize = 4;
	}
	else if (size < 87)
	{
		/* medium-large room */
		y1 = y0 - 5;
		y2 = y0 + 4;
		x1 = x0 - 7;
		x2 = x0 + 7;
		yfac = 3;
		yfacb = 2;
		xfac = 5;
		nsize = 5;
	}
	else /* size >= 88 */
	{
		/* Large room */
		y1 = y0 - 4;
		y2 = y0 + 4;
		x1 = x0 - 11;
		x2 = x0 + 11;
		yfac = 2;
		yfacb = 2;
		xfac = 9;
		nsize = 6;
	}

	/* don't actually build the room till later */

	/* Hack -- Choose a nest type */
	tmp = randint(p_ptr->depth/2+50);

    /* DJA randomize a little more */
	if (((randint(100) < 40) && (!p_ptr->theme) && (p_ptr->depth > 10)) || 
		(p_ptr->depth > 80))
	      tmp = randint(90);

	/* choose a nest (modified method) */
	if (tmp < 11) nesttyp = 1; /* jelly 11 */
	else if (tmp < 22) nesttyp = 2; /* creepy crawly 11 */
	else if (tmp < 28) nesttyp = 3; /* evil humans 6 */
	else if (tmp < 34) nesttyp = 4; /* evil humanoids 6 */
	else if (tmp < 42) nesttyp = 5; /* imps & fairies 8 */
	else if (tmp < 45) nesttyp = 6; /* water monsters 3 */
	else if (tmp < 49) nesttyp = 12; /* abominations 4 */
	else if (tmp < 63) nesttyp = 7; /* animal 14 */
	else if ((tmp < 74) && (p_ptr->depth > 34)) nesttyp = 8; /* great beast 11 */
	else if ((tmp < 89) && (p_ptr->depth > 45)) nesttyp = 9; /* undead (max)15 */
	else if ((p_ptr->depth > 10) && (p_ptr->depth < 44) && (randint(100) < 40)) 
		nesttyp = 11; /* lesser undead */
	else
	{
		if ((p_ptr->depth > 42) && (randint(100) < 42)) nesttyp = randint(11);
		else if (p_ptr->depth > 40) nesttyp = randint(9);
		else nesttyp = randint(7);
		if (nesttyp >= 10) nesttyp += 1; /* so randint(11) = 1,2,3,4,5,6,7,8,9,11,12 */
	}

	/* if it's a themed level, make sure the nest is appropriate */
	if (p_ptr->theme == 3) nesttyp = 1; /* ICKY_PLACE- always jelly nest */
	else if ((p_ptr->theme == 11) && (randint(100) < 34)) nesttyp = 4; /* BUG_CAVE- evil humanoids (dark elves) */
	else if (p_ptr->theme == 11) nesttyp = 2; /* BUG_CAVE- always creepy crawly nest or evil humanoids */
	else if ((p_ptr->theme == 2) && (randint(100) < 85)) nesttyp = 5; /* FFOREST- usually imp/fairy nest */
	else if ((p_ptr->theme < 3) && (randint(100) < 65)) nesttyp = 7; /* FFOREST & CFOREST- animal nest */
	else if ((p_ptr->theme == 2) || (p_ptr->theme == 1)) nesttyp = 8; /* FFOREST & CFOREST- great beast nest */
	else if (((p_ptr->theme == 8) || (p_ptr->theme == 7)) && (randint(100) < 35))
		nesttyp = 8; /* CASTLE & FULL_MOON- great beasts, */
	else if (((p_ptr->theme == 8) || (p_ptr->theme == 7) || (p_ptr->theme == 13)) &&
		(randint(100) < 50)) nesttyp = 9; /* CASTLE, DARK_CITY, FULL_MOON- undead */
	else if ((p_ptr->theme == 9) && (randint(100) < 78)) nesttyp = randint(2); /* swamp- creepy crawlies or jelly */
	else if ((p_ptr->theme == 9) && (randint(100) < 25)) nesttyp = 6; /* swamp- water monsters */
	else if ((p_ptr->theme == 13) && (randint(100) < 40)) nesttyp = 5; /* hob city: usually fairy nest */
	else if (p_ptr->theme == 13) /* other nests are okay for DARK_CTY also */;
	else if ((p_ptr->theme == 10) && (randint(100) < 35)) nesttyp = 4; /* evil humanoids (black dwarves) */
	else if (((p_ptr->theme == 8) || (p_ptr->theme == 14) || (p_ptr->theme == 15)) && 
		(randint(100) < 30)) nesttyp = 3; /* evil humans in the castle, hell hall, and barracks */
	/* CASTLE, HELL_HALL, GREPSE, DWARF_MINE - never animal */
	else if ((p_ptr->theme == 8) || (p_ptr->theme == 14) ||
		 (p_ptr->theme == 12) || (p_ptr->theme == 10)) special = TRUE;
	else if ((p_ptr->theme == 5) && (randint(100) < 40)) nesttyp = 2; /* EARTHY- creepy crawly */
	else if ((nesttyp == 8) || (nesttyp == 7)) /* animal/beast nests okay for most themes */;
	else if ((p_ptr->theme == 15) && (randint(100) < 86)) return; /* else often just fail in BARRACKS */
	else if (p_ptr->theme) special = TRUE;

	/* fail to make nasty nests too shallow */
	if ((p_ptr->depth < 34) && (nesttyp == 8)) return;
	if ((p_ptr->depth < 45) && (nesttyp == 9)) return;
	/* abomination nests have a soft level cap */
	if ((p_ptr->depth < 31) && (nesttyp == 12) && 
		((randint(100) < 118-(p_ptr->depth*3)-badluck) || (p_ptr->depth < 13)))
		return;

	/* special nest for themed levels */
	if ((special) && (p_ptr->theme))
	{
		name = "special theme";
		get_mon_num_hook = vault_aux_theme; /* special */
	    if (p_ptr->depth < 40) rating += 4; /* was 10 */
	    else rating += 2; /* was 5 */
	}

	/* Monster nest (jelly) */
	else if (nesttyp == 1)
	{
		/* temporarily set theme to icky place to allow icky things in jelly pits only */
		p_ptr->theme = 3;
		
        /* Describe */
		name = "jelly";

		/* Restrict to jelly */
		get_mon_num_hook = vault_aux_jelly;

	    /* Increase the level rating (depending on nest type) */
	    if (p_ptr->depth < 40) rating += 1; /* was 3 */
	}

	/* Monster nest (creepy crawly) */
	else if (nesttyp == 2)
	{
		/* allow theme_only bugs */
		p_ptr->theme = 11;

		/* Describe */
		name = "creepy crawly";

		/* Restrict to creepy crawlys */
		get_mon_num_hook = vault_aux_bugs;

	    /* Increase the level rating (depending on nest type) */
	    rating += 1; /* was 6 */
	}

	/* Monster nest (evil humans) */
	else if (nesttyp == 3)
	{
		/* Describe */
		name = "(evil humans)";

		/* Restrict to evil humans */
		get_mon_num_hook = vault_aux_warlock;

	    /* Increase the level rating (depending on nest type) */
	    rating += 4; /* humans often have decent drops */
	}

	/* Monster nest (evil humanoids) */
	else if (nesttyp == 4)
	{
		/* Describe */
		name = "(evil humanoids)";

		/* Restrict to evil humans */
		get_mon_num_hook = vault_aux_drow;

	    /* Increase the level rating (depending on nest type) */
	    rating += 4; /* these often have decent drops too */
	}

	/* Monster nest (imp and fairy) */
	else if (nesttyp == 5)
	{
		/* allow theme_only fairies (?, sometimes) */
		int tofchance = 23;
		if (p_ptr->theme) tofchance = 0;
		if (rand_int(100) < tofchance) p_ptr->theme = 2;
		else if (rand_int(100) < tofchance) p_ptr->theme = 13;

		/* Describe */
		name = "imp and fairy";

		/* most fairies like light */
		if (randint(101 + goodluck) > (p_ptr->depth * 3)/4 + 20) light = TRUE;

		/* Restrict to imp or fairy */
		get_mon_num_hook = vault_aux_fairy;

	    /* Increase the level rating (depending on nest type) */
	    rating += 3; /* was 10, then 2 */
	}

	/* Monster nest (water monsters) */
	else if (nesttyp == 6)
	{
		/* usually allow theme_only swamp monsters */
		if ((!p_ptr->theme) && (randint(100) < 90)) p_ptr->theme = 9;

		/* Describe */
		name = "(water monsters)";
		needwater = TRUE;

		/* Restrict to appropriate monsters */
		get_mon_num_hook = vault_aux_aquarium;

	    /* Increase the level rating (depending on nest type) */
	    rating += 2; /* */
	}

	/* Monster nest (animal) */
	else if (nesttyp == 7)
	{
		/* Describe */
		name = "animal";

		/* Restrict to animal */
		get_mon_num_hook = vault_aux_animal;

	    /* Increase the level rating (depending on nest type) */
	    rating += 3; /* was 10, then 2 */
	}

	/* Monster nest (great beast) */
	else if (nesttyp == 8)
	{
		/* often allow greater werewolf and weremumak */
		if (randint(100) < 40) p_ptr->theme = 7;

		/* Describe */
		name = "great beast";

		/* Restrict to great beast */
		get_mon_num_hook = vault_aux_beast;

	    /* Increase the level rating (depending on nest type) */
	    rating += 4; /* was 12, then 3 */
	}

	/* Monster nest (lesser undead) */
	else if (nesttyp == 11)
	{
		/* often allow theme_only haunted castle undead */
		if (!p_ptr->theme)
		{
			if (randint(100) < 34) p_ptr->theme = 8;
			else if (randint(100) < 9) p_ptr->theme = 13;
		}

		/* Describe */
		name = "(lesser undead)";

		/* Restrict to evil humans */
		get_mon_num_hook = vault_aux_zombie;

	    /* Increase the level rating (depending on nest type) */
	    rating += 2; /* */
	}

	/* Monster nest (abominations) */
	else if (nesttyp == 12)
	{
		/* Describe */
		name = "(abominations)";

		/* Restrict to evil humans */
		get_mon_num_hook = vault_aux_abomination;

	    /* Increase the level rating (depending on nest type) */
	    rating += 3; /* */
	}

	/* Monster nest (undead) */
	else /* (nesttyp == 9) */
	{
		/* often allow theme_only haunted castle undead */
		if (!p_ptr->theme)
		{
			if (randint(100) < 38) p_ptr->theme = 8;
			else if (randint(100) < 11) p_ptr->theme = 13;
		}

		/* Describe */
		name = "undead";

		/* Restrict to undead */
		get_mon_num_hook = vault_aux_undead;

	    /* Increase the level rating (depending on nest type) */
	    rating += 4; /* was 13 */
	}

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		while (1)
		{
			int r_idx, outd = 10;
			monster_race *r_ptr;
			if (p_ptr->depth < 5) outd = 3;
			else if (p_ptr->depth < 17) outd = (p_ptr->depth + 1) / 2;
			else if (p_ptr->depth < 20) outd = 9;

			/* Get a (hard) monster type */
			what[i] = get_mon_num(p_ptr->depth + outd, TRUE);

			/* check for maximum population */
			if (what[i])
			{
				r_idx = what[i];
				r_ptr = &r_info[r_idx];
				
				/* maximum population */
				if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop)) continue;

				/* usually reject non-water monsters except for a water monster nest */
				if ((r_ptr->flags7 & (RF7_WATER_ONLY)) && (!(nesttyp == 6)) &&
					(randint(100) < 90)) continue;
			}
			
			/* accept */
			break;
		}

		/* Notice failure */
		if (!what[i]) empty = TRUE;
		
		/* sometimes set the theme back early  (this hopefully should prevent  */
		/*  too many THEME_ONLY monsters appearing out of theme or at least */
        /*  make it more rare) */
        if ((i > 8) && (randint(100) < 22)) p_ptr->theme = realtheme;
	}

	/* set theme back to what the level really is */
	p_ptr->theme = realtheme;
	
	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Oops */
	if (empty) return;

		/* Now build the room... */
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
	
	/* sometimes place a reward */
	if (tmp > 89) tmp = randint(89);
	tmp = 93 - tmp; /* much more likely in tougher nests */
	if (tmp < 35) tmp = tmp/2+1;
	if (rand_int(tmp) < 2)
	{
		place_chest(y1 + 1, x1 + 1, 4);
		place_chest(y2 - 1, x2 - 1, 3);
		rating += 2;
		prize = TRUE;
	}
	else if (rand_int(tmp) < 5)
	{ 
		place_chest(y1 + 1, x1 + 1, 4);
		rating += 1;
		prize = TRUE;
	}
	else if (rand_int(tmp) < 11)
	{ 
		place_chest(y1 + 1, x1 + 1, 3);
		if (p_ptr->depth < 38) rating += 1;
		prize = TRUE;
	}

#if nomore
	/* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
	if ((p_ptr->depth <= 40) &&
	    (randint(p_ptr->depth * p_ptr->depth + 1) < 300))
	{
		good_item_flag = TRUE;
	}
#endif

	/* Place some monsters */
	for (y = y0 - yfac; y <= y0 + yfacb; y++)
	{
		for (x = x0 - xfac; x <= x0 + xfac; x++)
		{
			int r_idx = what[rand_int(64)];
			monster_race *r_ptr = &r_info[r_idx];
			
#if maybelater
			/* chance to repick monster */
			if ((r_ptr->flags7 & (RF7_WATER_ONLY)) && (randint(100) < 22))
			{
				r_idx = what[rand_int(64)];
				r_ptr = &r_info[r_idx];
			}
#endif
			/* still a water only monster */
			if (r_ptr->flags7 & (RF7_WATER_ONLY)) needwater = TRUE;

			/* Place that monster (no groups) */
			(void)place_monster_aux(y, x, r_idx, TRUE, FALSE);
		}
	}
	
	/* A WATER_ONLY monster has been generated in the nest: just add water */
	if (needwater)
	{
		/* Fill nest with water */
		generate_fill(y1, x1, y2, x2, FEAT_WATER);
		
		if (cheat_room)
		{ 
			if (prize) msg_format("Water-filled monster nest with prize (%s, size %d)", name, nsize);
			else msg_format("Water-filled monster nest (%s, size %d)", name, nsize);
		}
	}

	/* Describe */
	else if (cheat_room)
	{
		/* Room type */
		if (prize) msg_format("Monster nest with prize (%s, size %d)", name, nsize);
		else msg_format("Monster nest (%s, size %d)", name, nsize);
	}

	/* count crowded rooms (don't count very small nests) */
	if ((nsize >= 2) && (!((p_ptr->speclev >= 1) && (p_ptr->speclev <= 2))))
		dun->crowded = TRUE;
}



/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a big room, with an inner room, containing
 * a collection of monsters of a given type organized in the room.
 *
 * Monster types in the pit
 *  1 orc pit	(Dungeon Level 5 and deeper)
 *  2 troll pit	(Dungeon Level 20 and deeper)
 *  3 lesser undead pit
 *  4 golem pit
 *  5 military pit
 *  6 giant pit	(Dungeon Level 40 and deeper)
 *  7 hydra pit
 *  8 dragon pit	(Dungeon Level 60 and deeper)
 *  9 demon pit	(Dungeon Level 80 and deeper)
 *
 * The inside room in a monster pit appears as shown below, where the
 * actual monsters in each location depend on the type of the pit
 *
 *   #####################
 *   #0000000000000000000#
 *   #0112233455543322110#
 *   #0112233467643322110#
 *   #0112233455543322110# 7x21
 *   #0000000000000000000#
 *   #####################
 *
 * I'd like to make an alternate pit size or two, maybe like this
 *   ################
 *   #00000000000000#
 *   #01223455432210#
 *   #01223455432210# 6x16
 *   #00000000000000#
 *   ################
 *
 * Note that the monsters in the pit are now chosen by using get_mon_num()
 * to request 16 appropriate monsters, sorting them by level, and using
 * the even entries in this sorted list for the contents of the pit.
 *
 * Hack -- all of the dragons in a dragon pit must be the same color,
 * which is handled by requiring a specific breath attack for all of the
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
static bool build_type6(int y0, int x0)
{
	int tmp, what[16];

	int i, j, y, x, y1, x1, y2, x2;
	int pittyp, failp;

	bool empty = FALSE;
	bool special = FALSE;
	int light = FALSE;

	cptr name;


	/* Large room */
	y1 = y0 - 4;
	y2 = y0 + 4;
	x1 = x0 - 11;
	x2 = x0 + 11;

	/* Don't actually build the room yet */

	/* Choose a pit type */
	/* tmp = randint(p_ptr->depth); */
	tmp = randint(p_ptr->depth/2+50);

    /* DJA randomize a little more */
	if (((randint(100) < 40) && (!p_ptr->theme) && (p_ptr->depth > 22)) || 
		(p_ptr->depth > 80))
	      tmp = randint(90);

	/* choose a pit (modified method) */
	if (tmp < 14) pittyp = 1; /* orc 14 */
	else if (tmp < 27) pittyp = 2; /* troll 13 */
	else if (tmp < 30) pittyp = 3; /* lesser undead 3 */
	else if (tmp < 35) pittyp = 4; /* golem 5 */
	else if (tmp < 44) pittyp = 5; /* military 9 */
	else if (tmp < 56) pittyp = 6; /* giant 12 */
	else if (tmp < 59) pittyp = 7; /* hydra 3 */
	else if ((tmp < 74) && (p_ptr->depth > 57)) pittyp = 8; /* dragon 15 */
	/* (occationally removes the depth restriction) */
	else if ((p_ptr->depth > 77) || (randint(100) < 4)) pittyp = 9; /* demon 16 with randint(90) */
	else
	{
		pittyp = randint(5);
		if (pittyp >= 3) pittyp += 1; /* 1,2,4,5,6 */
	}
	
	/* pits less common on themed levels */
	failp = 12;
	/* some themes usually just fail to make a pit */
	if ((p_ptr->theme == 13) && (rand_int(100) < 20)) pittyp = 3;
	else if ((p_ptr->theme == 13) || (p_ptr->theme == 2)) failp = 82;
	else if ((p_ptr->theme == 12) || (p_ptr->theme == 11)) failp = 65;
	else if (p_ptr->theme == 10) failp = 30;

	/* if it's a themed level, make sure the pit is appropriate */
	if ((p_ptr->theme == 8) && (pittyp == 1))
	{
		special = TRUE;
		failp = 55;
	}
	if (((p_ptr->theme == 1) || (p_ptr->theme == 4) || (p_ptr->theme == 9)) &&
		randint(100) < 70) pittyp = 8; /* VOLCANO, CFOREST, SWAMP - always (appropriate) dragon pit or special */
	else if ((p_ptr->theme == 10) && (randint(100) < 10)) pittyp = 9; /* occationally allow demon pits in dwarf mines */
	else if ((p_ptr->theme == 1) || (p_ptr->theme == 4) || (p_ptr->theme == 9) || 
		(p_ptr->theme == 10)) special = TRUE;
	else if ((p_ptr->theme == 13) && (randint(100) < 10)) pittyp = 3; /* lesser undead in the nightmare theme */
	else if ((p_ptr->theme == 5) && (randint(100) < 75)) pittyp = 2; /* EARTHY - troll pit */
	else if ((p_ptr->theme == 8) && (randint(100) < 23)) pittyp = 4 + rand_int(2); /* CASTLE - golems or military */
	else if ((p_ptr->theme == 4) && (randint(100) < 23) && (p_ptr->depth > 47)) pittyp = 7 + rand_int(2); /* VOLCANO- hydras or (red)dragons */
	else if ((p_ptr->theme == 3) && (randint(100) < 45)) pittyp = 8; /* ICKY - dragons or special */
	else if ((p_ptr->theme == 3) && (randint(100) < 5)) pittyp = 7; /* ICKY - or hydras */
	else if ((p_ptr->theme == 14) && (randint(100) < 80)) pittyp = 9; /* HELL_HALL usually demons */
	else if ((p_ptr->theme == 15) && (randint(100) < 35)) pittyp = 5; /* BARRACKS - usually army pit */
	else if (p_ptr->theme == 15) /* normal pit */;/* never do special theme pit for BARRACKS because that's what the ARMY pit is */
	else if (((p_ptr->theme == 7)) && (randint(100) < 40)) pittyp = 9; /* FULL_MOON - demons or special */
	else if ((p_ptr->theme) && (randint(100) < failp)) return FALSE; /* sometimes just fail */
	else if ((p_ptr->theme) && (randint(100) < 68)) special = TRUE;

	/* fail to make dragons/ demon pits too shallow */
	if ((p_ptr->depth < 57) && ((pittyp == 8) || (pittyp == 9))) return (FALSE);
	/* hydra pits have a soft level cap */
	if ((p_ptr->depth < 43) && (pittyp == 7) && (randint(100) < 86-badluck)) return (FALSE);

	/* special theme pit */
	/* paranoia- seems like I got a special pit on a non-themed level a time or two */
	if ((special) && (p_ptr->theme))
	{
		/* Message */
		name = "special theme";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_theme;
	}

	/* Orc pit */
	else if (pittyp == 1)
	{
		/* Message */
		name = "orc";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_orc;
	}

	/* Troll pit */
	else if (pittyp == 2)
	{
		/* Message */
		name = "troll";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_troll;
	}

	/* lesser undead pit */
	else if (pittyp == 3)
	{
		/* Message */
		name = "lesser undead";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_zombie;
	}

	/* golem pit */
	else if (pittyp == 4)
	{
		/* Message */
		name = "golem";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_golem;
	}

	/* Army pit */
	else if (pittyp == 5)
	{
		/* Message */
		name = "military";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_army;
	}

	/* Giant pit */
	else if (pittyp == 6)
	{
		/* Message */
		name = "giant";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_giant;
	}

	/* hydra pit */
	else if (pittyp == 7)
	{
		/* Message */
		name = "hydra";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_hydra;
	}

	/* Dragon pit */
	else if (pittyp == 8)
	{
		int whichd = rand_int(6);
		if (randint(100) < 30) whichd = rand_int(7); /* includes sound dragons */
		/* if it's a themed level, make sure the dragon type is appropriate */
		if (p_ptr->theme == 1) whichd = 3; /* cold dragons in CFOREST */
		else if (p_ptr->theme == 4) whichd = 2; /* fire dragons in VOLCANO */
		else if ((p_ptr->theme == 9) && (randint(100) < 70)) whichd = 4; /* poison dragons in SWAMP usually */
		else if ((p_ptr->theme == 9) || (p_ptr->theme == 3)) whichd = 0; /* acid dragons in ICKY_PLACE or SWAMP */
		else if ((p_ptr->theme == 7) && (randint(100) < 75)) whichd = 1;
		else if (p_ptr->theme == 7) whichd = 5; /* electric or multi-hued dragons in FULL_MOON */
		else if ((p_ptr->theme == 6) && (randint(100) < 22)) whichd = 6; /* orange dragons sometimes in WINDY_CAVE */
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
	else /* (pittyp == 9) */
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
		while (1)
		{
			int r_idx, outd = 10;
			monster_race *r_ptr;
			if (p_ptr->depth < 5) outd = 3;
			else if (p_ptr->depth < 17) outd = (p_ptr->depth + 1) / 2;
			else if (p_ptr->depth < 20) outd = 9;

			/* Get a (hard) monster type */
			what[i] = get_mon_num(p_ptr->depth + outd, TRUE);

			/* check for maximum population */
			if (what[i])
			{
				r_idx = what[i];
				r_ptr = &r_info[r_idx];
				
				/* maximum population */
				if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop)) continue;
			}
			
			/* accept */
			break;
		}

		/* Notice failure */
		if (!what[i]) empty = TRUE;
	}


	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Oops */
	if (empty) return FALSE;

		/* Now build the room */
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
	if ((pittyp = 8) && (p_ptr->depth < 68)) rating += 6; /* early dragon pit */
	else if ((pittyp = 8) || (pittyp = 9)) rating += 5; /* dragon or demon pit */
	/* (orc pits decrease in rating with depth) */
	else if ((pittyp = 1) && (p_ptr->depth >= 9))
	{
		rating += 3 - (p_ptr->depth/42); /* was 11 - (p_ptr->depth/9) */
	}
	else rating += 4; /* was 10 */

#if nomore
	/* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
	if ((p_ptr->depth <= 40) &&
	    (randint(p_ptr->depth * p_ptr->depth + 1) < 300))
	{
		good_item_flag = TRUE;
	}
#endif

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

	return TRUE;
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
			else if ((level < 35) && (die > 90)) ridx = 849; /* dryad */
			else if (level < 29) ridx = 401; /* wild unicorn */
			else if (level < 48)
			{
				if (die < 60) ridx = 845; /* gnoyem */
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
				(r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num))
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
			r_ptr = &r_info[587]; /* check for Kavlax */
			if (level < 34)
			{
				if (die < 50) ridx = 441; /* chimera */
				else ridx = 469; /* smoke e. */
			}
			else if ((level < 45) || ((die < 42) &&
				(r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num) && (randint(100) < 40)))
			{
				if ((die < 35) && (level > 38) && 
					(r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 587; /* Kavlax */
				else if (die < 50) ridx = 620; /* 11-headed hydra */
				else ridx = 597; /* ancient red dragon */
			}
			else if (level < 85)
			{
				r_ptr = &r_info[656]; /* check for Smaug */
				ar_ptr = &r_info[744]; /* check for Glaurung */
				if ((die < 50) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 656;
				else if ((die < 45) && (ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num)) ridx = 744;
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
			if (level < 25)
			{
				if (die < 40) ridx = 299; /* gargoyle */
				else ridx = 877; /* arrowhawk */
			}
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
				if ((die < 45) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 700;
				else if ((die < 60) && (level < 58)) ridx = 694; /* fallen archon */
				else if (die < 60) ridx = 719; /* great storm wyrm */
				else ridx = 716; /* greater air e. */
			}
			else
			{
				r_ptr = &r_info[758]; /* check for Pazuzu */
				if ((die < 55) && (level < 76))  ridx = 737; /* titan */
				else if (die < 55) ridx = 766; /* greater titan */
				else if (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num) ridx = 758; /* Pazuzu */
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
				if (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num) ridx = 444;
				else ridx = 848; /* black stalker cat */
			}
			else if (level < 57)
			{
				r_ptr = &r_info[626]; /* check for the Cerberus */
				if ((die < 50) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 626;
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
				if ((die < 40) && (ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num)) ridx = 728;
				else if ((die > 50) && (level > 62) && 
					(r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 736;
				else if ((level < 69) || (die > 85)) ridx = 720; /* bronze idol */
				else ridx = 746; /* golden idol */
			}
			else
			{
				r_ptr = &r_info[760]; /* check for Draugluin */
				ar_ptr = &r_info[770]; /* check for Liart */
				if (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num) ridx = 760;
				else if (ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num) ridx = 770;
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
				if ((level > 53) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 702;
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
				if ((level > 85) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 771;
				else if ((die < 40) && (level > 78) &&
					(ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num)) ridx = 762;
				else if (level < 80)
				{
					r_ptr = &r_info[734]; /* check for silent watchers (left) */
					ar_ptr = &r_info[735]; /* check for (right) */
					if (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num) ridx = 734;
					else if (ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num) ridx = 735;
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
				if ((die < 32) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 523;
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
				if ((die < 45) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 677;
				else if ((die > 65) && (ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num)) ridx = 699;
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
					(r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 523;
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
				if ((die < 50) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 672;
				else if ((die > 65) && (level > 49) &&
					(ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num)) ridx = 705;
				else if (die < 90) ridx = 666; /* lesser balrog */
				else ridx = 846; /* the summoning dark */
			}
			else
			{
				r_ptr = &r_info[749]; /* check for Fafner */
				if ((die > 60) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 749;
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
				if ((die < 40) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 551;
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
					(r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 742;
				else if (die < 50) ridx = 654; /* high priest of achrya */
				else if ((die < 70) && (level < 70) &&
					(ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num)) ridx = 663;
				else if (die < 90) ridx = 755; /* pit fiend (not much really deep bugs) */
				else if (level < 70) ridx = 609; /* drider of achrya */
				else
				{
					r_ptr = &r_info[761]; /* check for The Tarrasque */
					if (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num) ridx = 761;
					else if (level == 125) ridx = 806; /* greater imp */
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
				if (die < 54) ridx = 386; /* bogeyman */
				else if (die < 75) ridx = 353; /* gory ghost */
				else ridx = 864; /* erlbold ogre */
			}
			else if (level < 37)
			{
				r_ptr = &r_info[405]; /* check for Draebor */
				if ((die < 30) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 405;
				else if ((die < 34) && (level < 35)) ridx = 421; /* hag */
				else if (die < 38) ridx = 483; /* black skeleton */
				else if (die < 65) ridx = 477; /* banshee */
				else ridx = 865; /* erlbolg hag */
			}
			else if (level < 43)
			{
				r_ptr = &r_info[567]; /* check for Bel-Shamharoth */
				if ((die < 65) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 567;
				else if (die < 50) ridx = 583; /* beholder */
				else if (die < 88) ridx = 623; /* headless horseman */
				else ridx = 589; /* scyllis */
			}
			else if (level < 70)
			{
				r_ptr = &r_info[825]; /* check for Phantom of Eilenel */
				if ((die < 30) && (level > 52) &&
					(r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 825;
				else if ((die < 45) && (level < 57)) ridx = 650; /* df.king */
				else if ((die < 45) && (level > 62)) ridx = 637; /* undead beholder */
				else if (die < 65) ridx = 680; /* df.queen */
				else if (die < 70) ridx = 710; /* dullahan */
				else if (level < 61) ridx = 616; /* wereraven */
				else ridx = 725; /* jabberwock */
			}
			else
			{
				r_ptr = &r_info[770]; /* check for Liart */
				if (die < 27) ridx = 725; /* jabberwock */
				else if (die < 54) ridx = 710; /* dullahan */
				else if (die < 60) ridx = 637; /* undead beholder */
				else if (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num) ridx = 770;
				else if (die < 85) ridx = 748; /* black reaver */
				else ridx = 822; /* pandemonium fiend */
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
				if ((die < 30) && (r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) ridx = 736;
				else if ((die < 30) && (level < 90)) ridx = 743; /* horned reaper */
				else if ((die < 60) && (ar_ptr->curpop + ar_ptr->cur_num < ar_ptr->max_num)) ridx = 663;
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
	if ((r_ptr->level < p_ptr->depth - 3) && (!(r_ptr->flags1 & (RF1_UNIQUE))))
		return (FALSE);
	else if (r_ptr->level < (p_ptr->depth * 3) / 4) return (FALSE);

	/* monster temple monsters */
	if (r_ptr->flags7 & RF7_TEMPLE) return (TRUE);

	/* assume not okay */
	return (FALSE);
}

/*
 * Helper function for build_vault() to choose a missile launcher
 */
static bool vault_aux_launcher(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];\

	/* Require "m" monsters */
	if (strchr("m", r_ptr->d_char)) return (TRUE);

	/* otherwise reject */
	return (FALSE);
}

/*
 * machines (certain monsters qualify)
 */
static bool machine_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	cptr dname = (r_name + r_ptr->name);

	/* certain monsters */
	if ((strchr("M", r_ptr->d_char)) && (r_ptr->level > 70)) return (TRUE);
	if (strstr(dname, "teleporter box")) return (TRUE);

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
bool match_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	int minl = 2;
	bool isfine = FALSE;
	
	/* don't choose trees or statues */
	if (r_ptr->flags7 & (RF7_NONMONSTER)) return (FALSE);

	/* Decline unique monsters */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) return (FALSE);

	/* okay to have umber hulks in vaults, but should never force multiple hulks */
	/* (allow in earth cave only unless deep) */
	if ((r_ptr->flags2 & (RF2_KILL_WALL)) && (!(p_ptr->theme == 5)) && (p_ptr->depth < 80)) return (FALSE);

	/* minimum level */
	if (p_ptr->depth > 38) minl = (p_ptr->depth / 3) + 1;
	else if (p_ptr->depth >= 14) minl = 14;
	if (r_ptr->level <= (minl / 2) + 1) return (FALSE);

	/* Hack -- Require certain types */
	if (strchr("OPTDMHKUVXYg%&", r_ptr->d_char)) isfine = TRUE;

	/* Hack -- other types require min level */
	if ((strchr("noNhqtx@pe", r_ptr->d_char)) && (r_ptr->level >= minl)) isfine = TRUE;
	if ((strchr("ABuzlR", r_ptr->d_char)) && (r_ptr->level >= minl + 3)) isfine = TRUE;

	/* okay if good for theme */
	if ((isfine) && (p_ptr->theme) && (theme_okay(r_idx, 0, TRUE))) return (TRUE);
	/* may not be much in-theme monsters to chose from on very low levels */
	else if ((p_ptr->theme) && (p_ptr->depth > 9)) return (FALSE);

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
 * mode = 5 gets a missile launcher
 * mode = 6 -machine
 */
int get_mon_match(int level, int mode, bool vault)
{
	int r_idx;
	int tries = 0;
	bool forcecomm = FALSE; /* force a certain common result */

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
	else if (mode == 6)
	{
		if (randint(100) < 35) forcecomm = TRUE;
		/* Require appropriate monsters */
		else get_mon_num_hook = machine_okay;
	}
	else if (mode == 5)
	{
		/* Require appropriate monsters */
		get_mon_num_hook = vault_aux_launcher;
	}
	else if (mode == 1)
	{
		/* Require appropriate monsters */
		get_mon_num_hook = water_hide_okay;
	}
	else /* mode == 0 */
	{
		/* Require appropriate monsters */
		get_mon_num_hook = match_okay;
	}

	/* Prepare allocation table */
	get_mon_num_prep();

	while (1)
	{
		monster_race *r_ptr;

		if ((mode == 6) && (forcecomm)) r_idx = 914; /* teleporter box */
		/* Get a (hard) monster type */
		else r_idx = get_mon_num(level, vault);

		/* (debugging experiment) */
		if (tries > 1500)
		{
/* #if debugging */
			if (get_mon_num_hook == water_hide_okay) 
			{
				int ttheme = p_ptr->theme;
				p_ptr->theme = 9; /* temporarily change to swamp */
				r_idx = get_mon_force_theme(level);
				p_ptr->theme = ttheme; /* reset */
			}
			else if (get_mon_num_hook == temple_okay)
			{
				int ttheme = p_ptr->theme;
				p_ptr->theme = randint(12); /* temporarily change theme */
				r_idx = temple_theme(level);
				p_ptr->theme = ttheme; /* reset */
			}
			else if (get_mon_num_hook == machine_okay) r_idx = 914; /* teleporter box */
			else if (get_mon_num_hook == tree_okay)
#if debugelse
			/* hacky fix to endless loop */
			/* (tree_okay didn't work all the time -I think this is unneeded now but not sure) */
			if (get_mon_num_hook == tree_okay)
#endif
			{
				if (level < 20) r_idx = 248; /* mad apple tree L15 */
				else if ((level < 30) && (randint(100) < 90)) r_idx = 341; /* tangle tree L22 */
				else if (level < 31) r_idx = 365; /* whomping willow L24 */
				else if ((level < 40) && (randint(100) < 60)) r_idx = 379; /* raspberry forest tree L25 */
				else if (level < 42) r_idx = 416; /* tree of the old forest L28 */
				else if ((level < 59) || (randint(100) < 10)) r_idx = 485; /* restless huorn L34 */
				else if ((level < 70) && (p_ptr->theme == 10)) r_idx = 485; /* ironwood tree L48 */
				else if ((level < 75) && (randint(100) < 58)) r_idx = 248; /* sleepy willow tree L55 */
				else if ((level < 77) || (randint(100) < 60)) r_idx = 248; /* evil ent L54 */
				else if (level < 90) r_idx = 248; /* disenchantree L73 */
			}
			else if (get_mon_num_hook == vault_aux_launcher)
			{
				if ((level < 23) || (randint(100) > level+50)) r_idx = 71; /* arrow launcher L4 */
				else if ((level < 30) || ((level < 73) && (randint(100) < 22))) r_idx = 901; /* hidden arrow launcher L29 */
				else if ((level < 35) || (randint(100) < 14)) r_idx = 905; /* assult sphere L33 */
				else if ((level < 46) || (randint(100) < 11)) r_idx = 902; /* invisible turret L44 */
				else if (randint(100) < 16) r_idx = 903; /* flame thrower L50 */
				else if ((level < 56) || (randint(100) < 16)) r_idx = 904; /* camoflauged cannon L55 */
				else if ((level < 59) || (randint(100) < 15)) r_idx = 905; /* geyser L60 */
				else if ((level < 70) || (randint(100) < 25)) r_idx = 906; /* alarm system L65 */
				else if ((level < 91) || (randint(100) < 16)) r_idx = 907; /* advanced defence mechanism L75 */
				else r_idx = 908; /* supreme defence mechanism L102 */
			}
#if debugging
			else if (get_mon_num_hook == zoo_okay)
			{
			}
			else /* mode == 0 */
			{
				/* Require appropriate monsters */
				get_mon_num_hook = match_okay;
			}
			else msg_print("Warning gen_mon_num failed"); /* should prevent crash at least */
			break;
#endif
		}
		else tries += 1;
/* #endif */

		if (!r_idx) continue;

		if (tries > 500) break;

		/* check for maximum population */
		r_ptr = &r_info[r_idx];
		
		/* maximum population */
		if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop)) continue;
			
		/* accept */
		break;
	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	return r_idx;
}


/*
 * Hack -- fill in vault rooms
 *
 * Note new vault symbols:
 * d: matching monsters, D: matching tougher monsters
 * R: rubble, o: open pit
 * c: small chest, $: normal big chest, C: good chest, G: great chest
 * ~: water, W: WATER_HIDE monster in water
 * T: monster temple monster
 * Z: zoo monster (non-bug animal)
 * m: missile launcher
 * S: statue, s: statue with treasure
 * P: pillar mimmic
 * M: machine (includes teleporter box, some missile launchers & stuff)
 */
static void build_vault(int y0, int x0, int ymax, int xmax, cptr data, bool greater)
{
	int dx, dy, x, y;
	int dr_idx, bigdr_idx;
	bool bedoor = FALSE;

	cptr t;
	
	/* better chance for good stuff in vaults */
	uniqdrop = 2; /* 'T' and '8' raises uniqdrop to 3 */

	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			bool statu = FALSE;
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
					/* outer room walls shouldn't have CAVE_ROOM */
					cave_info[y][x] &= ~(CAVE_ROOM);
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
				case '_': /* (this should only be used for empty vault designs, but included here just in case) */
				{
					if ((*t == '_') && (rand_int(100) < 33)) place_random_door(y, x, TRUE);
					else place_secret_door(y, x);
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
				/* from being placed: tree or tree monster */
				case 'E':
				{
					int thismon;
					if ((randint(100) < 94) || (p_ptr->depth < 12)) place_monster_aux_real(y, x, 834, TRUE, 0, 1);
					else
					{
						thismon = get_mon_match(p_ptr->depth + randint(9), 4, TRUE);
						place_monster_aux_real(y, x, thismon, TRUE, 0, 1);
					}
					break;
				}

				/* machine (or various other stuff) */
				case 'M':
				{
					int mode = 0, die = rand_int(80 - (128-p_ptr->depth)/4);
					if (die < 20) cave_set_feat(y, x, FEAT_WALL_INNER);
					else if (die < 22) place_rubble(y, x);
					else if (die < 25) statu = TRUE;
					else if (die < 28) place_chest(y, x, 2);
					else if (die < 30)
					{
						/* occational chest mimmic */
						if (p_ptr->depth > 19 - badluck/2) place_monster_aux(y, x, 377, TRUE, FALSE);
						/* weaker version of chest mimmic */
						else if (rand_int(100) < 65) place_monster_aux(y, x, 912, TRUE, FALSE);
						else 
						{
							if (rand_int(100) < (goodluck+1) * 2) mode = randint(2);
							place_chest(y, x, mode); /* default mode 0 == ruined chest */
						}
					}
					else if ((die < 33) && (p_ptr->depth > 35)) /* pillar mimmic */
					{
						if (randint(100) < 70) place_monster_aux(y, x, 900, TRUE, FALSE);
						cave_set_feat(y, x, FEAT_WALL_INNER);
					}
					else /* machine (usually teleporter box) */
					{
						int thismon = get_mon_match(p_ptr->depth + 3, 6, TRUE);
						place_monster_aux_real(y, x, thismon, TRUE, 0, 1);
					}
					if (!statu) break;
					/* else fall through */
				}

				/* Statue (Later: chance for a monster mimmicking a statue) */
				case 'S':
				case 's':
				{
					/* the slp parameter for statues says if it's ruined or not */
                    if (randint(100) < 45) place_monster_aux_real(y, x, 910, FALSE, 0, 1);
					else place_monster_aux_real(y, x, 909, FALSE, 0, 1);
					if ((*t == 's') || ((statu) && (rand_int(100) < 16 + goodluck)))
						place_object(y, x, TRUE, FALSE);
					break;
				}

				/* possible fountain, statue, rubble, or nothing */
				/* (only thing which can be ruined in a non-emptied vault) */
				case 'f':
				{
					int die = randint(100);
					if (die < 30)
					{
						place_puddle(y, x, 2);
						place_monster_aux_real(y, x, 911, FALSE, 0, 1);
					}
                    else if (die < 35) place_monster_aux_real(y, x, 910, FALSE, 0, 1);
					else if (die < 45) place_monster_aux_real(y, x, 909, FALSE, 0, 1);
					else if (die < 50) place_rubble(y, x);
                    else if (die < 52) place_monster_aux_real(y, x, 910, TRUE, 0, 1);
					else if (die < 55) place_monster_aux_real(y, x, 909, TRUE, 0, 1);
					else if (die < 60) place_monster_aux_real(y, x, 911, TRUE, 0, 1);
					break;
				}

				/* Fountain (Later: chance for riverflow gargoyle mimmicking a fountain) */
				case 'F':
				{
					place_puddle(y, x, 2);
					place_monster_aux_real(y, x, 911, FALSE, 0, 1);
					if (randint(100) < 11) place_object(y, x, FALSE, FALSE);
					break;
				}

				/* Pillar mimmic (usually) */
				case 'P':
				{
					if (randint(100) < 70) place_monster_aux(y, x, 900, TRUE, FALSE);
					cave_set_feat(y, x, FEAT_WALL_INNER);
					break;
				}

				/* missile launcher (usually) */
				case 'm':
				{
					int thismon = get_mon_match(p_ptr->depth + rand_int(4 + badluck/2), 5, TRUE);
					if (randint(100) < 80) place_monster_aux(y, x, thismon, FALSE, FALSE);
					else if (randint(100) < 45) place_trap(y, x);
					break;
				}

				/* Nasty monster and treasure (monster temple) */
				case 'T':
				{
					int thismon, monlev;
					monster_race *r_ptr;
					/* different amount out of depth for lesser & greater vaults */
					if (greater) monlev = p_ptr->depth + 20 + rand_int(21); /* 20-40 */
					else monlev = p_ptr->depth + 11 + rand_int(10+((badluck+3)/2)); /* 11-21 */
                    thismon = get_mon_match(monlev, 3, TRUE);
					r_ptr = &r_info[thismon];
					place_monster_aux(y, x, thismon, TRUE, TRUE);
					/* If it picked a WATER_HIDE monster, usually add a puddle */
					if ((r_ptr->flags7 & (RF7_WATER_HIDE)) && (randint(100) < 80))
						place_puddle(y, x, 1);
					object_level = p_ptr->depth + 15;
					uniqdrop = 3;
					place_object(y, x, TRUE, TRUE);
					object_level = p_ptr->depth;
					uniqdrop = 2;
					break;
				}
			}
		}
	}
	
	 /*  Matching monsters:  (requires appropriate theme monster if there's a theme) */
	/* Pick a monster for 'd' */
	dr_idx = get_mon_match(p_ptr->depth + 3, 0, TRUE);

	/* Pick a monster for 'D' */
	bigdr_idx = get_mon_match(p_ptr->depth + 10, 0, TRUE);

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
				/* Granite wall (outer) */
				case '%':
				{
					/* try to make an entrance if we're in a cavern */
					if ((p_ptr->speclev >= 1) && (p_ptr->speclev <= 3))
					{
						if ((possible_doorway(y, x, FALSE)) &&
							((!bedoor) || (randint(100) < 20)))
						{
							place_random_door(y, x, FALSE);
							bedoor = TRUE;
						}
					}
					break;
				}
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

				/* Zoo monster (non-bug animal) and treasure */
				case '1':
				{
					int thismon = get_mon_match(p_ptr->depth + 6 + randint(4), 2, TRUE);
					place_monster_aux(y, x, thismon, TRUE, TRUE);
					if (rand_int(100) < 20)
					{
						object_level = p_ptr->depth + 5;
						place_object(y, x, TRUE, FALSE);
						object_level = p_ptr->depth;
                	}
					break;
				}

				/* group animal (this forces a group even if it doesn't normally come in groups) */
				case 'h':
				{
					int thismon, hdie = randint(100);
					if ((p_ptr->depth < 11) && (hdie < 50)) thismon = 127; /* rothe */
					else if ((p_ptr->depth < 12) && (hdie >= 96)) thismon = 166; /* cave bear */
					else if ((p_ptr->depth < 15) && (hdie > 45)) thismon = 182; /* white wolf */
					else if ((p_ptr->depth < 15) && (hdie < 15)) thismon = 212; /* killer bee */
					else if ((p_ptr->depth < 17) && (hdie < 4)) thismon = 156; /* king cobra */
					else if ((p_ptr->depth < 17) && (hdie < 8)) thismon = 174; /* bugbear */
					else if ((p_ptr->depth < 17) && (hdie < 60)) thismon = 235; /* warg */
					else if ((p_ptr->depth < 20) && (hdie >= 90)) thismon = 189; /* orangatan */
					else if ((p_ptr->depth < 20) && (hdie >= 75)) thismon = 249; /* mirkwood spider */
					else if ((p_ptr->depth < 20) && (hdie >= 65)) thismon = 203; /* black mamba */
					else if ((p_ptr->depth < 27) && (hdie < 10)) thismon = 216; /* zhang */
					else if ((p_ptr->depth < 27) && (hdie < 16)) thismon = 255; /* giant tarantula */
					else if ((p_ptr->depth < 27) && (hdie < 40)) thismon = 262; /* large ape */
					else if ((p_ptr->depth < 27) && (hdie < 55)) thismon = 293; /* blink dog */
					else if ((p_ptr->depth < 23) && (hdie > 55)) thismon = 296 + rand_int(3); /* elemental hound (fire/cold/elec) */
					else if ((p_ptr->depth < 27) && (hdie < 80)) thismon = 329 + rand_int(3); /* acid/air/earth hound */
					else if ((p_ptr->depth < 31) && (hdie >= 80)) thismon = 407; /* bats of gorgoroth */
					else if ((p_ptr->depth < 38) && (hdie < 10)) thismon = 268; /* tiger */
					else if ((p_ptr->depth < 39) && (hdie < 12)) thismon = 307; /* warhorse */
					else if ((p_ptr->depth < 38) && (hdie < 17)) thismon = 335; /* salamander */
					else if ((p_ptr->depth < 38) && (hdie < 29)) thismon = 351; /* giant horned bullfrog */
					else if ((p_ptr->depth < 38) && (hdie < 31)) thismon = 352; /* camel-dog */
					else if ((p_ptr->depth < 38) && (hdie < 55)) thismon = 398 + rand_int(2); /* nexus/vibration hound */
					else if ((p_ptr->depth > 70) && (hdie < 20)) thismon = 441; /* chimaera */
					else if ((p_ptr->depth > 80) && (hdie < 30)) thismon = 594; /* sphinx */
					else if ((p_ptr->depth > 80) && (hdie < 38)) thismon = 577; /* tyerrey */
					else if ((p_ptr->depth > 85) && (hdie < 45)) thismon = 647; /* mastodon */
					else if ((p_ptr->depth > 85) && (hdie < 55)) thismon = 652; /* winged horror */
					else if ((p_ptr->depth > 90) && (hdie < 60)) thismon = 843; /* weremumak */
					else if ((p_ptr->depth > 100) && (hdie < 74)) thismon = 791 + rand_int(3); /* physics/static/draining hound */
					else if ((p_ptr->depth > 105) && (hdie < 78)) thismon = 765; /* young behemoth */
					else if ((p_ptr->depth > 65) && (hdie > 86)) thismon = 610; /* mutlti-hued hound */
					else if ((p_ptr->depth > 65) && (hdie > 71)) thismon = 767; /* hellhound */
					else if ((p_ptr->depth > 68) && (hdie > 57)) thismon = 747; /* veriety hound */
					else if (hdie > 55) thismon = 503 + rand_int(3); /* inertia/gravity/impact hound */
					else if (hdie < 6) thismon = 437; /* manticore */
					else if (hdie < 16) thismon = 608; /* mumak */
					else if (hdie < 26) thismon = 722; /* chaos hound */
					else if (hdie < 36) thismon = 317; /* sabre-tooth tiger */
					else if (hdie < 43) thismon = 373; /* werebear */
					else if (hdie < 49) thismon = 387; /* owlbear */
					else if (hdie < 52) thismon = 842; /* swamp bat */
					else if (hdie < 54) thismon = 436; /* doombat */
					else /* if (hdie < 56) */ thismon = 779; /* mammoth bullfrog */
					place_monster_aux_real(y, x, thismon, FALSE, 2, 0);
					if (rand_int(100) < 22)
					{
						object_level = p_ptr->depth + 5;
						place_object(y, x, TRUE, TRUE);
						object_level = p_ptr->depth;
                	}
					break;
				}

				/* WATER_HIDE monster in water */
				case 'W':
				{
					int thismon = get_mon_match(p_ptr->depth + 4 + randint(5), 1, TRUE);
					cave_set_feat(y, x, FEAT_WATER);
					place_monster_aux(y, x, thismon, TRUE, TRUE);
					/* (maybe possible object here also?) */
					break;
				}

				/* matching monster, possible treasure */
				/* (no groups for matching monsters) */
				case 'd':
				{
					/* bugsearch = TRUE; */
					place_monster_aux(y, x, dr_idx, TRUE, FALSE);
					if (randint(100) < 11)
					{
						object_level = p_ptr->depth + 4;
						place_object(y, x, FALSE, FALSE);
						object_level = p_ptr->depth;
					}
					break;
				}

				/* meaner matching monster, possible treasure */
				/* (no groups for matching monsters) */
				case 'D':
				{
					/* bugsearch = TRUE; */
					place_monster_aux(y, x, bigdr_idx, TRUE, FALSE);
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
				/* different nastiness for lesser & greater vaults */
				case '8':
				{
					if (greater) monster_level = p_ptr->depth + 40;
					else monster_level = p_ptr->depth + 25;
					place_monster(y, x, TRUE, TRUE, TRUE);
					monster_level = p_ptr->depth;
					if (greater) object_level = p_ptr->depth + 20;
					else object_level = p_ptr->depth + 11;
					uniqdrop = 3;
					place_object(y, x, TRUE, TRUE);
					object_level = p_ptr->depth;
					uniqdrop = 2;
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
					int mode = 2;
					if (*t == 'c') mode = 1;
					else if (*t == 'C') mode = 3;
					else if (*t == 'G') mode = 4;
					place_chest(y, x, mode);
					break;
				}
			}
		}
	}
	
	bugsearch = FALSE;
	/* reset */
	uniqdrop = 0;
}



/*
 * Hack -- fill in rooms for empty vaults (type9)
 * has chances to retain some aspects of original design (objects & traps)
 * but rarely has forced out-of-depth objects or monsters
 *
 * int design determines how likely it is that the original design aspects
 * are created. In a vault design, original monsters and objects are rarely
 * created. In a type9 room shape design, it is much more likely.
 * design = 1 designed as empty vault, made almost just as designed.
 * design = 2 designed as lesser vault, made similar to design.
 * design = 7 designed as lesser vault, emptied.
 * design = 9 designed as empty vault, emptied.
 */
static void build_empty(int y0, int x0, int ymax, int xmax, cptr data, int design)
{
	int dx, dy, x, y, dsg, die;
	int goods = 0; /* don't create too many treasures in an empty vault */
	bool permwall = FALSE;
	bool light = FALSE;
	bool vicky = FALSE;
	bool bedoor = FALSE;
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

	if (design == 10) design = 2;
			
	/* CAVE_ICKY only if made as designed (or empty greater vault) */
	if ((design == 8) || (design == 2)) vicky = TRUE;
	if (design == 11) { vicky = TRUE; design = 1; }

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

			/* CAVE_ICKY only if made as designed */
			if ((light) && (vicky))
               cave_info[y][x] |= (CAVE_ROOM | CAVE_ICKY | CAVE_GLOW);
			else if (vicky) cave_info[y][x] |= (CAVE_ROOM | CAVE_ICKY);
			else if (light) cave_info[y][x] |= (CAVE_ROOM | CAVE_GLOW);
			else cave_info[y][x] |= (CAVE_ROOM);

			/* Analyze the grid */
			switch (*t)
			{
				/* Granite wall (outer) */
				case '%':
				{
					cave_set_feat(y, x, FEAT_WALL_OUTER);
					/* outer room walls shouldn't have CAVE_ROOM */
					cave_info[y][x] &= ~(CAVE_ROOM);
					break;
				}

				/* formerly permanent wall, small chance to become rubble */
				case 'X':
				{
					if (design == 1) dsg = 34; /* permwall */
					if (design == 9) dsg = 2;
					else dsg = 6;
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
				case 'P':
				case '#':
				{
					die = rand_int(100);
					if (design == 1) dsg = 1;
					/* usually if there were permanent walls originally, */
					/* then the granite was meant like a door, */
					/* so make it rubble (or a door) instead of granite. */
					else if (permwall) dsg = 80;
					/* lesser vault design like original */
					else if ((design == 2) || (design == 9)) dsg = 4;
					/* about 1/6 chance to become rubble for lesser vault designs */
					else dsg = 15;

					/* 'P' for Pillar mimmic */
					if ((rand_int(100) < 50) && (*t == 'P') && (p_ptr->depth >= 36))
					{
						place_monster_aux(y, x, 900, TRUE, FALSE);
						cave_set_feat(y, x, FEAT_WALL_INNER);
					}
					else if (((die < dsg/2) && (permwall)) || ((die < dsg/5) && (!permwall)))
					{
						cave_set_feat(y, x, FEAT_FLOOR);
					}
					else if (die < dsg)
					{
						place_rubble(y, x);
					}
					else if ((die < dsg/4) && (permwall) && ((design == 7) || (design == 2)))
					{
						place_random_door(y, x, TRUE);
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
					if (rand_int(100) < dsg) place_rubble(y, x);
					break;
				}

				/* doors */
				case '+': /* secret door */
				case '_': /* (usually) not secret door used for empty vault designs only */
				{
					if (*t == '_') dsg = 15;
					else if ((design == 2) || (design == 9)) dsg = 75;
					else if (design == 1) dsg = 96;
					else dsg = 35;
					if (rand_int(100) < dsg)
					{
						place_secret_door(y, x);
					}
					else
					{
						place_random_door(y, x, TRUE);
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
					/* sometimes pit replaced by a puddle */
					else
                    {
						int pud = dsg/11;
						if (p_ptr->theme == 9) pud += 32;
						if ((p_ptr->theme == 11) || (p_ptr->theme == 3)) pud += 16;
						if (rand_int(100) < pud)
						{
							cave_set_feat(y, x, FEAT_WATER);
						}
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
						int thismon = get_mon_match(p_ptr->depth, 1, FALSE);
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

				/* tree or small chance of tree monster */
				case 'E':
				{
					int thismon;
					if (p_ptr->depth < 12) dsg = 200; /* always normal tree on early lvls */
					else if (p_ptr->depth < 70) dsg = 99 - p_ptr->depth/14; /* was 97; */
					else dsg = 95 - (p_ptr->depth-65)/21;
					if ((design == 2) || (design == 8)) dsg -= 3; /* was dsg = 94 */
					if (rand_int(100) < dsg) place_monster_aux_real(y, x, 834, TRUE, 0, 1);
					else
					{
						thismon = get_mon_match(p_ptr->depth, 4, FALSE);
						place_monster_aux_real(y, x, thismon, TRUE, 0, 1);
					}
					break;
				}
				
				/* machine */
				case 'M':
				{
					int thismon = get_mon_match(p_ptr->depth + 3, 6, TRUE);
					place_monster_aux_real(y, x, thismon, TRUE, 0, 1);
					break;
				}

				/* Statue (Later: chance for a monster mimmicking a statue) */
				case 'S':
				case 's':
				{
					int dieb = randint(100);
					if (design == 1) dsg = 90;
					else if ((design == 2) || (design == 9)) dsg = 35;
					else dsg = 10;
					die = randint(100);
					if ((dieb < 15) || (die < (dsg+1)/2)) 
						place_monster_aux_real(y, x, 910, FALSE, 0, 1);
					else if ((dieb < 30) || (die < dsg)) 
						place_monster_aux_real(y, x, 909, FALSE, 0, 1);
					/* the slp parameter for statues says if it's ruined or not */
					else if (dieb < 50) place_monster_aux_real(y, x, 910, TRUE, 0, 1);
					else if (dieb < 70) place_monster_aux_real(y, x, 909, TRUE, 0, 1);
					else if (dieb < 90) place_rubble(y, x);
					if ((dieb < 70) && (randint(100) < 23-goods) && (*t == 's')) place_object(y, x, TRUE, FALSE);
					break;
				}

				/* Fountain (Later: chance for riverflow gargoyle mimmicking a fountain) */
				case 'F':
				case 'f':
				{
					if (design == 1) dsg = 85;
					else if (design == 2) dsg = 35;
					else if (design == 9) dsg = 30;
					else dsg = 10;
					if (*t == 'f') dsg = dsg / 2;
					die = randint(100);
					if (die < dsg)
					{
						place_puddle(y, x, 2);
						place_monster_aux_real(y, x, 911, FALSE, 0, 1);
					}
					/* the slp parameter for statues says if it's ruined or not */
					else if (die < dsg+10) place_monster_aux_real(y, x, 911, TRUE, 0, 1);
					else if (die < dsg+20) place_rubble(y, x);
					/* only 'f's turn into statues sometimes */
					else if (*t == 'F') break;
					else if (die < dsg+23) place_monster_aux_real(y, x, 910, TRUE, 0, 1);
					else if (die < dsg+28) place_monster_aux_real(y, x, 909, TRUE, 0, 1);
					else if (die < dsg+31) place_monster_aux_real(y, x, 910, FALSE, 0, 1);
					else if (die < dsg+35) place_monster_aux_real(y, x, 909, FALSE, 0, 1);
					break;
				}

				/* missile launcher */
				case 'm':
				{
					if (design == 1) dsg = 85;
					else if (design == 2) dsg = 35;
					else if (design == 9) dsg = 11;
					else dsg = 9;
					if (rand_int(100) < dsg)
					{
						int thismon = get_mon_match(p_ptr->depth + rand_int(4), 5, FALSE);
						place_monster_aux(y, x, thismon, FALSE, FALSE);
					}
					break;
				}

				/* chests */
				case '$':
				case 'c':
				case 'C':
				case 'G':
				{
					int mode = randint(2); /* normal small or large chest */
					die = randint(100);
					if (design == 1) dsg = 70;
					else if (design == 2) dsg = 20;
					else dsg = 10;
					dsg -= goods; /* prevent too much treasure in empty vault */
					if (die < dsg/10) /* small chance for special chest */
					{
						if ((*t == 'G') && (randint(100) < 12)) mode = 4;
						else if ((*t == 'C') || (*t == 'G')) mode = 3;
						else if (*t == 'c') mode = 1;
						else mode = 2;
						place_chest(y, x, mode);
						if (mode == 4) goods += 3;
						else goods += mode;
					}
					else if (die < dsg)
					{
						place_chest(y, x, mode);
						goods += mode;
					}
					else if (die < dsg + 6)
					{
						/* occational chest mimmic */
						if (p_ptr->depth > 19 - badluck/2) place_monster_aux(y, x, 377, TRUE, FALSE);
						/* weaker version of chest mimmic */
						else if (rand_int(100) < 65) place_monster_aux(y, x, 912, TRUE, FALSE);
						else 
						{
							if (rand_int(100) < (goodluck+1) * 2) mode = randint(2);
							else mode = 0; /* 0 = ruined chest */
							place_chest(y, x, mode);
						}
					}
					else if (die < dsg + 22)
					{
						place_chest(y, x, 0); /* 0 = ruined chest */
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
			
			if (goods < 1) goods = 1;

			/* Analyze the grid */
			switch (*t)
			{
				/* Granite wall (outer) */
				case '%':
				{
					/* try to make an entrance if we're in a cavern */
					if ((p_ptr->speclev >= 1) && (p_ptr->speclev <= 3))
					{
						if ((possible_doorway(y, x, FALSE)) &&
							((!bedoor) || (randint(100) < 20)))
						{
							place_random_door(y, x, FALSE);
							bedoor = TRUE;
						}
					}
					break;
				}
				/* Treasure, sometimes monster */
				case 'h':
				case '1':
				case ',':
				{
					if (design == 1) dsg = 45;
					else if (design == 2) dsg = 15;
					else if (design == 9) dsg = 8;
					else dsg = 5;
					if (((*t == '1') || (*t == 'h')) && (randint(100) < dsg))
					{
						/* place an animal */
                        int thismon = get_mon_match(p_ptr->depth + rand_int(3), 2, FALSE);
						place_monster_aux(y, x, thismon, TRUE, TRUE);
					}
					else if ((*t == ',') && (randint(100) < dsg - 1))
					{
						vault_monsters(y0, x0, 1);
					}
					if (dsg > 22) dsg = 22;
					if (goods) dsg -= rand_int(goods*2); /* prevent too much treasure in empty vault */
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
					if (design == 1) dsg = 80;
					else if (design == 2) dsg = 46;
					else if (design == 9) dsg = 16;
					else if (design == 8) dsg = 7;
					else dsg = 12;
					if (goods) dsg -= randint(goods*2); /* prevent too much treasure in empty vault */
					if ((rand_int(100) < dsg/12) && (!(*t == '9')))
					{
						place_object(y, x, TRUE, TRUE);
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
					if (design == 1) dsg = 70;
					else if (design == 2) dsg = 40;
					else if (design == 9) dsg = 28;
					else if (design == 8) dsg = 12;
					else dsg = 16;
					if ((*t == '8') || (*t == 'T')) dsg += dsg/4;
					die = randint(100);
					if ((die < dsg) && (*t == 'T') && 
						((design == 2) || (design == 8) || (design == 1)))
					{
						int thismon = get_mon_match(p_ptr->depth + 3 + randint(3), 3, FALSE);
						monster_race *r_ptr = &r_info[thismon];
						place_monster_aux(y, x, thismon, TRUE, TRUE);

						/* If it picked a WATER_HIDE monster, sometimes add a puddle */
						if ((r_ptr->flags7 & (RF7_WATER_HIDE)) && (randint(100) < 40))
								place_puddle(y, x, 1);
					}
					else if ((die < dsg/2) && ((*t == '9') || (*t == '8') || (*t == '@')))
					{
						monster_level = p_ptr->depth + 2 + randint(4);
						place_monster(y, x, TRUE, TRUE, TRUE);
						monster_level = p_ptr->depth;
					}
					else if (die < dsg)
					{
						monster_level = p_ptr->depth + rand_int(3);
						place_monster(y, x, TRUE, TRUE, TRUE);
						monster_level = p_ptr->depth;
					}
					break;
				}
				/* d and D monsters always fit the current theme, if any */
				case 'D':
				case 'd':
				{
					int depth = p_ptr->depth;
					if (design == 1) dsg = 80;
					else if (design == 2) dsg = 22;
					else if (design == 9) dsg = 14;
					else dsg = 10;
					if (*t == 'D') depth += 2;
					if (randint(100) < dsg)
					{
						int thismon = get_mon_match(depth, 0, FALSE);

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
static void build_type7(int y0, int x0, bool medium)
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

		/* Reject anything that's not the correct type of vault */
		if ((medium) && (!(v_ptr->typ == 10))) continue;
		else if ((!medium) && (!(v_ptr->typ == 7))) continue;

		/* vault rarity */
		rarity = v_ptr->rare;

		/* never reject vault ideal for the theme */
		if ((p_ptr->theme) && (v_ptr->itheme == p_ptr->theme)) break;
		/* bias toward vaults whicth are ideal for the theme */
		else if (p_ptr->theme) rarity = (rarity * 3) / 2;

		/* roll for vault design rarity */
		if ((rarity > 1) && (rand_int(100) < rarity) && (tries < 90)) continue;

		/* accept */
		break;
	}

	/* Message */
	if ((cheat_room) && (medium)) msg_format("Medium vault (%s)", v_name + v_ptr->name);
	else if (cheat_room) msg_format("Lesser vault (%s)", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

#if nomore
	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}
#endif

	/* Hack -- Build the vault */
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, FALSE);
}


/*
 * Type 9 -- empty vaults (see "vault.txt")
 *
 * Empty vaults use three types of designs
 * design type 9 = different room shape for variety
 * design type 7 = emptied lesser vault design (rarer before depth 12)
 * design type 8 = emptied greater vault design (minimum depth 80, very rare)
 * psuedo design type 1 = type 9 design created almost exactly as designed.
 * psuedo design type 2 = type 7 design created much more similar to original design
 * (great = 0 = normal empty vault, great = 1 = empty medium vault, great = 2 = empty GV
 */
static void build_type9(int y0, int x0, int great)
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
			(v_ptr->typ == 9)) && (!great)) break;

		if ((v_ptr->typ == 7) && (rarity < 15) && (v_ptr->useMT < 2)) rarity = 15;
		/* useMT = 2 means lesser vault design can be used for empty vault more often */
		if ((v_ptr->typ == 7) && (v_ptr->useMT == 2) && (p_ptr->theme)) rarity += 10;
		else if ((v_ptr->typ == 7) && (v_ptr->useMT == 2)) rarity += 5;
		/* inflate rarity of type 7 designs */
		else if ((p_ptr->depth < 12) && (v_ptr->typ == 7))
		{
			rarity = rarity*3;
			if (rarity > 93) rarity = 93;
		}
		else if (v_ptr->typ == 7)
		{
			rarity = (rarity*2) + 5;
			if (rarity > 80) rarity = 80;
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
			rarity += 10;
		}
		else if ((v_ptr->typ == 10) && (v_ptr->useMT == 2))
		{
			if (rarity > 10) rarity -= 10;
			else if (rarity > 1) rarity = 1;
		}

		/* roll for vault design rarity */
		if ((rarity > 1) && (rand_int(100) < rarity) && (tries < 90)) continue;

		/* empty greater vault */
		if (great == 2)
		{
			if ((v_ptr->typ == 8) && (v_ptr->useMT)) break;
		}
		/* empty medium vault */
		else if (great == 1)
		{
			if ((v_ptr->typ == 10) && (v_ptr->useMT)) break;
		}
		else /* v_ptr->useMT is 0 if design should not be used for empty vault */
		{
			/* Accept the first vault design usable for an empty vault */
			if (((v_ptr->typ == 9) || (v_ptr->typ == 7)) && (v_ptr->useMT)) break;
		}
	}

	/* Message */
	if (cheat_room)
	{
		if (great == 2) msg_format("Empty greater vault (%s)", v_name + v_ptr->name);
		else if (great == 1)  msg_format("Empty medium vault (%s)", v_name + v_ptr->name);
        else if (v_ptr->typ == 7) msg_format("Empty vault (%s)", v_name + v_ptr->name);
        else msg_format("Room design (%s)", v_name + v_ptr->name);
	}

	/* special case for minetown: lesser vault with an empty vault index */
	/* because it's too big to technically be a lesser vault */
	if ((v_ptr->typ == 9) && (v_ptr->rat == 11))
	{
		if (rand_int(100) < 46)
		{
			/* make as true lesser vault */
			rating += v_ptr->rat;
			build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, FALSE);
			return;
		}
		else if (rand_int(100) < 40) mtv = 2;
		else mtv = 7; /* make as emptied lesser vault */
	}
    /* type9 has a chance to be built (almost) exactly as designed because */
	/* type9 rooms are not designed to be vaults */
	else if ((v_ptr->typ == 9) && (rand_int(100) < 70) &&
		((v_ptr->rat == 0) || (p_ptr->depth > 3)))
	{
		mtv = 1;
	}
	/* version of lesser vault deisgn, mostly emptied, but more like original design */
	else if (v_ptr->typ == 7)
	{
		int mtvc = 6;
		if (v_ptr->useMT == 2) mtvc = 20;
		if ((rand_int(100) < mtvc) && (p_ptr->depth > v_ptr->rat-1)) mtv = 2;
		else mtv = v_ptr->typ;
	}
	else mtv = v_ptr->typ;

	/* Boost the rating if it's an empty greater vault because it's still going to have some stuff */
	if (mtv == 8) rating += v_ptr->rat/5;
	if (mtv == 10) rating += v_ptr->rat/4;
	/* empty vaults created just as designed often have guaranteed treasure */
	/* (v_ptr->rat is often 0 for type9 designs anyway, but not always) */
	else if (mtv == 1) rating += v_ptr->rat;
	/* version of lesser vault deisgn, mostly emptied, but more like original design */
	else if (mtv == 2) rating += v_ptr->rat/4;
	/* emptied lesser vault */
	else if (mtv == 7) rating += v_ptr->rat/8;
	/* room design (rat is usually 0) */
	else /* mtv == 9 */ rating += v_ptr->rat/2;
	
	/* type9 design with a rating made as designed- mark as cave_icky */
	if ((mtv == 1) && (v_ptr->rat > 1)) mtv = 11;

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
	build_vault(y0, x0, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, TRUE);
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
 * The "solid" wall check prevents corridors from chopping the
 * corners of rooms off, as well as "silly" door placement, and
 * excessively wide room entrances.
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
			place_random_door(y, x, FALSE);
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

		/* count stairs */
		if ((cave_feat[y][x] == FEAT_LESS) || (cave_feat[y][x] == FEAT_MORE))
			k++;

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
bool possible_doorway(int y, int x, bool trees)
{
	int nextto;
	if (trees) nextto = next_to_floor(y, x);
	else nextto = next_to_corr(y, x);
	
    /* Count the adjacent corridors */
	if (nextto >= 2)
	{
		if (trees) /* check CAVE_WALL flag, not feature when placing trees */
		{
			if ((cave_info[y-1][x] & (CAVE_WALL)) && (cave_info[y+1][x] & (CAVE_WALL)))
				return (TRUE);
			if ((cave_info[y][x-1] & (CAVE_WALL)) && (cave_info[y][x+1] & (CAVE_WALL)))
				return (TRUE);
		}
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
	if ((rand_int(100) < DUN_TUN_JCT) && possible_doorway(y, x, FALSE))
	{
		/* Place a door */
		place_random_door(y, x, FALSE);
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
	int rtyp = typ;
	if (typ == 12) typ = 10;

	/* Restrict level */
	if (p_ptr->depth < room[typ].level) return (FALSE);

	/* Restrict "crowded" rooms */
	if (!((p_ptr->speclev >= 1) && (p_ptr->speclev <= 3)))
	{
 	   if (dun->crowded && ((typ == 5) || (typ == 6))) return (FALSE);
    }

	/* Extract blocks */
	by1 = by0 + room[typ].dy1;
	bx1 = bx0 + room[typ].dx1;
	by2 = by0 + room[typ].dy2;
	bx2 = bx0 + room[typ].dx2;

	if ((p_ptr->speclev >= 1) && (p_ptr->speclev <= 3))
	{
		/* Never run off the screen */
		if ((by1 < 0) || (by2 >= DUNGEON_HGT-12)) return (FALSE);
		if ((bx1 < 0) || (bx2 >= DUNGEON_WID-12)) return (FALSE);
	}
	else
	{
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
	}

	/* It is *extremely* important that the following calculation */
	/* be *exactly* correct to prevent memory errors XXX XXX XXX */

	/* Get the location of the room */
	y = ((by1 + by2 + 1) * BLOCK_HGT) / 2;
	x = ((bx1 + bx2 + 1) * BLOCK_WID) / 2;

	typ = rtyp;

	/* Build a room */
	switch (typ)
	{
		/* Build an appropriate room */
		case 12: build_type9(y, x, 1); break; /* empty medium vault (rare) */
		case 11: build_type9(y, x, 2); break; /* empty greater vault (vrare) */
		case 10: build_type7(y, x, TRUE); break; /* medium vault */
		case 9: build_type9(y, x, 0); break; /* normal empty vault */
		case 8: build_type8(y, x); break; /* greater vault */
		case 7: build_type7(y, x, FALSE); break; /* lesser vault */
		/* allow pits and nests to fail (didn't work) */
		/* case 6: return (build_type6(y, x)); */
		/* case 5: return (build_type5(y, x)); */
		case 6: build_type6(y, x); break;
		case 5: build_type5(y, x); break;
		case 4: build_type4(y, x); break;
		case 3: build_type3(y, x); break;
		case 2: build_type2(y, x); break;
		case 1: build_type1(y, x); break;

		/* Paranoia */
		default: return (FALSE);
	}

	/* creating a room in a cavern failed */
	if (p_ptr->speclev == 3) return (FALSE);
	
	if (!((p_ptr->speclev >= 1) && (p_ptr->speclev <= 2)))
	{
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

		/* Count "crowded" rooms (pits) */
		/* (nests are done in the build_type5 function and are not counted if it is a very small nest) */
		if (typ == 6) dun->crowded = TRUE;
	}

	/* Success */
	return (TRUE);
}


/* 
 * Places traps, rubble, and sometimes other terrain
 *  as appropriate for the level theme if any.
 * was part of cave_gen(), but I separated it into a handy separate function
 * (cavernmod is 0 if it isn't a cavern level)
 */
static void alloc_terrain(int k, int cavernmod)
{
	int puddles, pits, statue;
	int amt = k;
	int treec = 11;
	if (cavernmod) treec += 3;

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k + 1));

	if (!p_ptr->theme)
	{
		/* Sometimes allow random puddles */
		if ((amt > 9) && (randint(100) < 17))
		{
			/* Put some puddles in the dungeon */
			puddles = 1 + randint(3);
			/* (replaces some rubble) */
			amt -= puddles;
			if ((cavernmod == 3) || (cavernmod == 2)) puddles += 1;
			if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_WATER, puddles);
			else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_WATER, puddles);
			if (cheat_room) msg_print("(random puddles placed)");
		}
		/* Sometimes allow random open pits */
		else if ((amt > 8) && (randint(100) < 15))
		{
			/* Put some open pits in the dungeon */
			pits = 1 + randint(3);
			alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OPEN_PIT, pits);
			if (cavernmod == 3) pits += 2;
			else if (cavernmod) pits += 1;
			/* (replaces some rubble) */
			amt -= (pits-1);
			if (cheat_room) msg_print("(random pits placed)");
		}
		/* Sometimes allow random trees (rarer) */
		if ((amt > 6) && (randint(100) < treec - (p_ptr->depth/9)))
		{
			/* Put some trees in the dungeon */
			int trees = 2 + randint(3);
			if ((cavernmod == 3) || (cavernmod == 2)) trees += 1;
			if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TREES, trees);
			else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_TREES, trees);
			/* (replaces some rubble) */
			if (!((cavernmod == 3) || (cavernmod == 2))) amt -= (trees-1);
			if (cheat_room) msg_print("(random trees placed)");
		}
	}
	/* put several puddles in the swamp (at least 7) */
	else if (p_ptr->theme == 9)
	{
		if (amt > 7) puddles = 6 + randint(amt-3);
		else puddles = 6 + randint(4);
		if ((cavernmod == 2) || (cavernmod == 3)) puddles += rand_int(cavernmod);
		if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_WATER, puddles);
		else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_WATER, puddles);
		/* (replaces some rubble) */
		amt -= puddles;
		if (amt < 1) amt = randint(2);
	}
	/* often place a pond or two in the forests (these puddles are usually bigger) */
	else if ((p_ptr->theme == 1) || (p_ptr->theme == 2))
	{
		puddles = rand_int(3);
		if (cavernmod == 3) puddles += 1;
		if (puddles)
		{
			if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_WATER, puddles);
			else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_WATER, puddles);
			amt -= rand_int(puddles+1);
		}
	}
	/* allow puddles in the icky place and bug cave (most bugs like water) */
	else if ((p_ptr->theme == 11) || (p_ptr->theme == 3))
	{
		/* Put some puddles in the dungeon (sometimes) */
		if (rand_int(100) < 55)
		{
			puddles = randint(3) + 2;
			if ((cavernmod == 2) || (cavernmod == 3)) puddles += rand_int(cavernmod);
		}
		else if (amt > 4) puddles = rand_int(3);
		if (puddles)
		{
			if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_WATER, puddles);
			else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_WATER, puddles);

			/* (replaces some rubble) */
			if ((amt > 4) && (amt > puddles + 1)) amt -= puddles;
			else if (amt > 4) amt = (amt+1)/2;
			else if (puddles > 1) amt -= 1;
		}
	}

	/* Forests have trees */
	if ((p_ptr->theme == 1) || (p_ptr->theme == 2))
	{
		/* Put some trees in the dungeon */
		int ntrees = 0, trees = 8 + randint(14); /* 9-22 */
		if ((p_ptr->depth > 40) && (rand_int(20) < p_ptr->depth/20 + 2) && (trees > 14)) 
			trees -= rand_int(p_ptr->depth/20);
		/* place more trees where there's space */
		if (cavernmod == 9) ntrees = 26 + randint(11); /* 27-37 */
		else if (cavernmod == 3) ntrees = 22 + randint(11); /* 23-33 */
		else if (cavernmod == 2) ntrees = 19 + randint(10); /* 20-29 */
		else if (cavernmod) ntrees = 14 + randint(13); /* 15-27 */
		if (trees < ntrees) trees = ntrees;
		if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TREES, trees);
		else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_TREES, trees);
		/* (don't replace rubble because forests already have reduced rubble) */
	}
	/* there are also trees in a swamp (usually) */
	else if ((p_ptr->theme == 9) && (randint(100) < 80))
	{
		/* Put some trees in the dungeon */
		int ntrees = 0, trees = 4 + randint(8); /* 5-12 */
		if ((p_ptr->depth > 40) && (rand_int(20) < p_ptr->depth/20 + 2) && (trees > 7))
			trees -= randint(p_ptr->depth/20);
		/* place more trees where there's space */
		if (cavernmod == 3) ntrees = 11 + randint(13); /* 12-24 */
		else if (cavernmod) ntrees = 7 + randint(11); /* 8-18 */
		if (trees < ntrees) trees = ntrees;
		if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TREES, trees);
		else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_TREES, trees);
		/* (don't replace rubble because forests already have reduced rubble) */
	}
	
	/* Statues (most common in CASTLE and DARK_CITY) */
	if ((p_ptr->theme == 8) && (randint(100) < 80)) statue = 2 + randint(5);
	/* DARK_CITY / nightmare */
	else if ((p_ptr->theme == 15) && (randint(100) < 60)) statue = 2 + randint(5);
	/* also often in the DWARF_MINE and BARRACKS */
	else if (((p_ptr->theme == 10) || (p_ptr->theme == 15)) && 
		(randint(100) < 35)) statue = randint(5);
	/* occationally on normal levels */
	else if ((!p_ptr->theme) && (rand_int(100) < 4)) statue = 2 + randint(4);
	else statue = 0;
    
	/* Place the statues */
    if (statue)
	{
		/* possibly make one of them a fountain */
        if (randint(100) < 6)
		{
			if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_FOUNT, 1);
			else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_FOUNT, 1);
			statue -= 1;
		}
		if (statue) 
		{
			if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_STATUE, statue);
			else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_STATUE, statue);
		}
		if ((!p_ptr->theme) && (cheat_room)) msg_print("(random statues placed)");
		/* */
		if (statue > 3) amt -= rand_int(statue - 2);
		if (amt < 1) amt = 1;
	}
	
	/* These themed levels have less rubble */
	if ((p_ptr->theme == 1) || (p_ptr->theme == 2) || (p_ptr->theme == 6))
	{
		/* Put some rubble in corridors (less) */
		amt -= (2 + randint(3));
		/* (often none at all) */
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
	/* These themed levels have extra rubble (EARTH_CAVE, CASTLE, and DWARF_MINE) */
	else if ((p_ptr->theme == 5) || (p_ptr->theme == 8) || (p_ptr->theme == 10))
	{
		if (p_ptr->theme == 5) amt += randint(2);
		if ((cavernmod == 3) || (cavernmod == 2)) amt += 1 + randint(cavernmod);
		else if (cavernmod) amt += randint(2);
		/* Put some rubble in corridors */
		alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(amt));
		/* ..and other places too */
		if (p_ptr->theme != 8) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_RUBBLE, randint((amt + 2) / 3));
		if (cavernmod) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_RUBBLE, randint(amt));
		else alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_RUBBLE, randint(amt));
	}
	else
	{
		/* Put some rubble in corridors */
		alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(amt + 1));
	}
}

#if togglecaverns
#else /* */
/*
 *   *** Caverns ***
 * Initialize the dungeon array, with a random percentage of squares open.
 */
void init_cavern(int dhei, int dwid, int density) 
{
	int size = dhei * dwid;

	int count = (size * density) / 100;

	/* Fill the edges with perma-rock, and rest with rock */
	generate_draw(0, 0, DUNGEON_HGT - 1, DUNGEON_WID - 1, FEAT_PERM_SOLID);
	generate_fill(1, 1, DUNGEON_HGT - 2, DUNGEON_WID - 2, FEAT_WALL_EXTRA);

	while (count > 0) 
	{
		int y = randint(dhei - 2);
		int x = randint(dwid - 2);
		if ((cave_feat[y][x] == FEAT_WALL_SOLID) ||
			(cave_feat[y][x] == FEAT_WALL_EXTRA))
		{
			cave_set_feat(y, x, FEAT_FLOOR);
			count--;
		}
	}
}

/*
 * Run a single pass of the mutate stuff for caverns.
 * reduces nooks and walls in open space and makes the cavern more regular
 * also makes a little more open space
 * mutmod varies the amount of open space it tends toward:
 * mutmods: 5 is rarest, then 3, then 4, then 2, and 1 is most common
 * mutmods: 5 is most dense, then 1, then 2, and 3 is least dense (4 is really random)
 */
void mutate_cavern(int dhei, int dwid, int mutmod, int density) 
{
	int y, x;
	int temp[DUNGEON_HGT][DUNGEON_WID];
	int modea, modeb, modec;
	/* more dense: high modea, low modeb, lower modec */
	/* less dense: low modea, high modeb, high modec */
	/* (modec should probably have the least variance) */
	if (mutmod == 2) {modea = 110; modeb = 13; modec = 85;}
	else if (mutmod == 3) {modea = 101; modeb = 16; modec = 88;} /* least dense */
	else if (mutmod == 5) {modea = 135; modeb = 3; modec = 84;} /* most dense */
	/* mode 4 changes with each call to mutate_cavern */
	else if (mutmod == 4) 
		{modea = 95 + randint(35); modeb = 5 + rand_int(16); modec = 79 + randint(21);}
	else /* mutmod 1 is default */ {modea = 125; modeb = 5; modec = 85;}

	for (y = 1; y < dhei - 1; y++) 
	{
		for (x = 1; x < dwid - 1; x++) 
		{
			int count = next_to_walls(y, x, 1);
#if original
			if (count > 5)
				temp[y][x] = FEAT_WALL_SOLID;
			else if (count < 4)
				temp[y][x] = FEAT_FLOOR;
#else
			if (count > 7) /* completely surrounded by walls */
				temp[y][x] = FEAT_WALL_EXTRA;
			else if ((count > 5) && (randint(100) < modea - (8-count)*25))
				temp[y][x] = FEAT_WALL_EXTRA;
			/* allow standalone pillars more often in wide open mutmods */
			else if (((mutmod == 3) || (mutmod == 2)) && (count < 3))
			{
				if (randint(100) < 75) temp[y][x] = FEAT_FLOOR;
				else temp[y][x] = cave_feat[y][x];
			}
			else if (((count < 4) && (randint(100) < modec)) || 
				    ((count >= 4) && (randint(100) < modeb)))
				temp[y][x] = FEAT_FLOOR;
#endif
			else
				temp[y][x] = cave_feat[y][x];

			/* try to prevent mutmod 4 from being too wide open */
			if (((mutmod == 4) && (density > 32) && (rand_int(102) < 2)) &&
				((modea < 112) && (modeb > 8)) || (modea < 105 || modeb > 13))
			{
				if ((modea < 110) || (modeb > 11) || ((modea < 105) && (modeb > 13)))
				{
					if (modea < 110) modea = 105 + randint(25);
					if (modeb > 10) modeb = 5 + rand_int(10);
				}
				else if (randint(100) < 55) modea = 100 + randint(30);
				else modeb = 5 + rand_int(12);
			}
		}
	}

	for (y = 1; y < dhei - 1; y++) 
	{
		for (x = 1; x < dwid - 1; x++) 
		{
			cave_set_feat(y, x, temp[y][x]);
		}
	}
}

/*
 * Fill an int[] with a single value.
 */
void array_filler(int data[], int value, int size) 
{
	int i;
	for (i = 0; i < size; i++) data[i] = value;
}

/*
 * Used to convert (x, y) into an array index (i).
 */
static int lab_toi(int y, int x, int w) 
{
	return y * w + x;
}
/**
 * Used to convert an array index (i) into (x, y).
 */
static void lab_toyx(int i, int w, int *y, int *x) 
{
	*y = i / w;
	*x = i % w;
}

/*
 * Determine if we need to worry about coloring a point, or can ignore it.
 */
int ignore_point(int dhei, int dwid, int colors[], int y, int x) 
{
	int n = lab_toi(y, x, dwid);

	if (y < 0 || x < 0 || y >= dhei || x >= dwid) return TRUE;
	if (colors[n]) return TRUE;
	if (cave_info[y][x] & (CAVE_ICKY)) return TRUE;
	if (((cave_feat[y][x] >= FEAT_DOOR_CLOSE) && (cave_feat[y][x] <= FEAT_DOOR_STUCK)) ||
		(cave_feat[y][x] == FEAT_OPEN) || (cave_feat[y][x] == FEAT_BROKEN) ||
		(cave_feat[y][x] == FEAT_SECRET) || (cave_feat[y][x] == FEAT_WATER) ||
        (cave_feat[y][x] == FEAT_RUBBLE)) return FALSE;
	if (cave_feat[y][x] != FEAT_FLOOR) return TRUE;
	return FALSE;
}

/* what's this? I wish they'd made the new V code as understandable as the old V code */
static int xds[] = {0, 0, 1, -1};
static int yds[] = {1, -1, 0, 0};

void glow_point(int y, int x) /* was i = -1; i <= -1 (for i and j) */
{
	int i, j;
	for (i = -2; i <= 3; i++)
		for (j = -2; j <= 3; j++)
			cave_info[y + i][x + j] |= (CAVE_GLOW);
}

/* What is all this color stuff?? */
/**
 * Color a particular point, and all adjacent points.
 */
void build_color_point(int dhei, int dwid, int colors[], int counts[], int y, int x, int color, bool lit) 
{
	int size = dhei * dwid;
	struct queue *queue = q_new(size);

	int added[DUNGEON_HGT * DUNGEON_WID];
	array_filler(added, 0, size);

	q_push_int(queue, lab_toi(y, x, dwid));

	counts[color] = 0;

	while (q_len(queue) > 0) 
	{
		int i, y2, x2;
		int n2 = q_pop_int(queue);

		lab_toyx(n2, dwid, &y2, &x2);

		if (ignore_point(dhei, dwid, colors, y2, x2)) continue;

		colors[n2] = color;
		counts[color]++;

		/*if (lit) glow_point(y2, x2);*/

		for (i = 0; i < 4; i++) 
		{
			int y3 = y2 + yds[i];
			int x3 = x2 + xds[i];
			int n3 = lab_toi(y3, x3, dwid);
			if (ignore_point(dhei, dwid, colors, y3, x3)) continue;
			if (added[n3]) continue;

			q_push_int(queue, n3);
			added[n3] = 1;
		}
	}

	q_free(queue);
}

/**
 * Create a color for each "NESW contiguous" region of the dungeon.
 */
void build_colors(int dhei, int dwid, int colors[], int counts[]) 
{
	int y, x;
	int color = 1;
	bool lit = FALSE;

	for (y = 0; y < dhei; y++) 
	{
		for (x = 0; x < dwid; x++) 
		{
			if (ignore_point(dhei, dwid, colors, y, x)) continue;
			build_color_point(dhei, dwid, colors, counts, y, x, color, lit);
			color++;
		}
	}
}

/*
 * Find and delete all small (<9 square) open regions.
 */
void clear_small_regions(int dhei, int dwid, int colors[], int counts[]) 
{
	int i;
	int size = dhei * dwid;

	int deleted[DUNGEON_HGT * DUNGEON_WID];
	array_filler(deleted, 0, size);

	for (i = 0; i < size; i++) 
	{
		if (counts[i] < 9) 
		{
			deleted[i] = 1;
			counts[i] = 0;
		}
	}

	for (i = 0; i < size; i++) 
	{
		if (deleted[colors[i]]) 
		{
			int x, y;
			lab_toyx(i, dwid, &y, &x);
			colors[i] = 0;
			/* don't modify if it's in a room */
			if (!(cave_info[y][x] & (CAVE_ROOM)))
				cave_set_feat(y, x, FEAT_WALL_EXTRA);
		}
	}
}


/*
 * Return the number of colors which have active cells.
 */
int count_colors(int counts[], int size) 
{
	int i;
	int num = 0;
	for (i = 0; i < size; i++) if (counts[i] > 0) num++;
	return num;
}

/*
 * Return the first color which has one or more active cells.
 */
int first_color(int counts[], int size) 
{
	int i;
	for (i = 0; i < size; i++) if (counts[i] > 0) return i;
	return -1;
}

/*
 * Find all cells of 'fromcolor' and repaint them to 'tocolor'.
 */
void fix_colors(int colors[], int counts[], int from, int to, int size) 
{
	int i;
	for (i = 0; i < size; i++) if (colors[i] == from) colors[i] = to;
	counts[to] += counts[from];
	counts[from] = 0;
}

/*
 * Create a tunnel connecting a region to one of its nearest neighbors.
 */
void join_region(int dhei, int dwid, int colors[], int counts[], int color) 
{
	int i;
	int size = dhei * dwid;

	struct queue *queue = q_new(size);

	int previous[DUNGEON_HGT * DUNGEON_WID];
	array_filler(previous, -1, size);

	for (i = 0; i < size; i++) 
	{
		if (colors[i] == color) 
		{
			q_push_int(queue, i);
			previous[i] = i;
		}
	}

	while (q_len(queue) > 0) 
	{
		int n = q_pop_int(queue);

		int color2 = colors[n];
		if (color2 && color2 != color) 
		{
			while (colors[n] != color) 
			{
				int x, y;
				lab_toyx(n, dwid, &y, &x);
				colors[n] = color;
				/* don't modify if it's in a room */
				if (!(cave_info[y][x] & (CAVE_ROOM)))
					cave_set_feat(y, x, FEAT_FLOOR);
				n = previous[n];
			}
			fix_colors(colors, counts, color2, color, size);
			return;
		}

		for (i = 0; i < 4; i++) 
		{
			int y, x, y2, x2, n2;
			lab_toyx(n, dwid, &y, &x);

			y2 = y + yds[i];
			x2 = x + xds[i];
			if (y2 < 0 || y2 >= dhei) continue;
			if (x2 < 0 || x2 >= dwid) continue;

			n2 = lab_toi(y2, x2, dwid);
			if (previous[n2] >= 0) continue;

			q_push_int(queue, n2);
			previous[n2] = n;
		}
	}

	q_free(queue);
}

/**
 * Start connecting regions, stopping when the cave is entirely connected.
 */
void join_regions(int dhei, int dwid, int colors[], int counts[]) 
{
	int size = dhei * dwid;
	int num = count_colors(counts, size);

	while (num > 1) 
	{
		int color = first_color(counts, size);
		join_region(dhei, dwid, colors, counts, color);
		num--;
	}
}

int open_count(int dhei, int dwid) 
{
	int x, y;
	int num = 0;
	for (y = 0; y < dhei; y++)
		for (x = 0; x < dwid; x++)
			num++;
	return num;
}

/**
 * The program's main function.
 */
bool cavern_gen(void) 
{
	int i, k, y, x, smag, sqtz;
 	int fmod, mutmod = 1, mutdie = rand_int(100);
	bool river = FALSE, litcave = FALSE;

	int dhei = randint(DUNGEON_HGT / 3 + 1) + (DUNGEON_HGT*2) / 3 - 2;
	int dwid = randint(DUNGEON_WID / 2) + DUNGEON_WID / 2 - 1;
	int size = dhei * dwid;

	/* the bigger the density number, the more open space (backwords) */
	int density = 26 + randint(12); /* was 24 + randint(16) (19) (27-38) */
	int times = 2 + randint(4); /* times to mutate the cavern */

	int colors[DUNGEON_HGT * DUNGEON_WID];
	int counts[DUNGEON_HGT * DUNGEON_WID];

	int tries = 0;

	/* If we're too shallow then don't do it */
	if (p_ptr->depth < 8) return FALSE;

	/* chance for shallow caverns to be lit */
	if (rand_int(100) < 65-((p_ptr->depth - 7)*3)) litcave = TRUE;

	/* modes of mutating caverns (added for DAJ) mutmod default is 1 (62 chance) */
	/* mutmods: 5 is most dense, then 1, then 2, and 3 is least dense (4 is really random) */
	if (mutdie < 5) mutmod = 5; /* 5 */
	else if (mutdie < 17) mutmod = 4; /* 14 */
	else if (mutdie < 32) mutmod = 2; /* 15 */
	else if (mutdie < 38) mutmod = 3; /* 6 */
	if (mutmod == 4) density = 26 + randint(10);

	/* dwarf mine and earth cave shouldn't be wide open */
	if (((p_ptr->theme == 5) || (p_ptr->theme == 10)) && 
		((mutmod == 3) || (mutmod == 2))) mutmod = 1;
	else if (((p_ptr->theme == 5) || (p_ptr->theme == 10)) && (density > 34)) density = 25 + randint(10);
	/* forests shouldn't be dense (at least not with rock, trees will be added later) */
	if (((p_ptr->theme == 1) || (p_ptr->theme == 2)) && 
		((mutmod == 5) || (mutmod == 1) || (mutmod == 4))) mutmod = 2 + rand_int(2);
	else if (((p_ptr->theme == 1) || (p_ptr->theme == 2)) && 
		(density < 30)) density = 29 + randint(11);
	/* these shouldn't be too dense either (windy cave, swamp, icky place) */
	if (((p_ptr->theme == 6) || (p_ptr->theme == 9) || (p_ptr->theme == 3)) && 
		(mutmod == 5)) mutmod = 1 + rand_int(2);
	else if (((p_ptr->theme == 6) || (p_ptr->theme == 9) || (p_ptr->theme == 3)) && 
		(density < 29)) density = 28 + randint(12);
	/* nightmare city, full moon: not too dense nor too wide open */
	if (((p_ptr->theme == 13) || (p_ptr->theme == 7)) && 
		((mutmod == 5) || (mutmod == 3))) mutmod = 1;

	/* check density */
	if ((mutmod == 5) && (density > 35)) density = 25 + randint(11); /* 25-36 */
	if ((mutmod == 2) && (density < 28)) density = 27 + randint(12); /* 28-39 */
	if ((mutmod == 3) && (density < 30)) density = 28 + randint(12); /* 29-40 */

	array_filler(colors, 0, size);
	array_filler(counts, 0, size);

	for (tries = 0; tries < 10; tries++) 
	{
		/* Build a random cavern and mutate it a number of times */
		init_cavern(dhei, dwid, density);
		for (i = 0; i < times; i++) mutate_cavern(dhei, dwid, mutmod, density);

		/* If there are enough open squares then we're done */
		if (open_count(dhei, dwid) >= size / 20) break;
	}

	/* If we couldn't make a big enough cavern then fail */
	if (tries == 10) return FALSE;

	/* mark that the current level is a cavern */
	p_ptr->speclev = 1;
	/* 2 here means wide open cavern */
	if ((mutmod == 3) || ((mutmod == 2) && (density > 34))) 
		p_ptr->speclev = 2;

	if (rand_int(100) < 36)
	{
		int by, bx, usedepth, roomno = 1, whilst = 0, ndroom = 9;
		if ((mutmod == 5) || (mutmod == 3)) ndroom += 2;
		if (mutmod == 4) ndroom = 5 + randint(4);
		
		usedepth = p_ptr->depth; /* only used for rolling against roomodd */
        /* occationally use deeper fake room depth for more randomness */
        if ((p_ptr->depth > 4) && (p_ptr->depth < 99) && (rand_int(100) < 5))
        {
            if (p_ptr->depth < 45) usedepth = 50;
            else usedepth += 2 + rand_int(9);
        }
		if ((usedepth > 15) && (randint(100) < 40)) usedepth = (usedepth + 20 + randint(79))/2;
		if (usedepth > 105) usedepth = 105;
		/* small chance for up to three rooms */
		if (rand_int(100) < ndroom) roomno += 1;
		if ((roomno > 1) && (rand_int(100) < ndroom)) roomno += 1;

		while (whilst < roomno)
		{
            if (p_ptr->speclev == 3) p_ptr->speclev = 1;
			by = rand_int(dhei / BLOCK_HGT);
			bx = rand_int(dwid / BLOCK_WID);
			/* Attempt an unusual room */
			if (rand_int(125) < usedepth)
			{
				int nestodd, echan, mchan = 26 + p_ptr->depth/8;
				if (mchan > 35) mchan = 35;
				nestodd = mchan + 6 + p_ptr->depth/8;
				if (nestodd > mchan + 13) nestodd = mchan + 13;
				echan = nestodd + 42;
				k = rand_int(100);

				/* Attempt a very unusual room */
				if (rand_int(165) < usedepth)
				{
					/* Type 8 -- Lesser vault (25%) */
					if ((k < 25) && room_build(by, bx, 7))
						{ whilst += 1; continue; }

					/* Type 7 -- Medium vault (~7%) */
					if ((k < mchan) && room_build(by, bx, 10))
						{ whilst += 1; continue; }

					/* Type 5 -- Monster nest (~9-10%) */
					if ((k < nestodd) && room_build(by, bx, 5))
						{ whilst += 1; continue; }
					if (p_ptr->speclev == 3) p_ptr->speclev = 1;

					/* type 9 -- empty vault (30%) */
					if ((k < echan) && room_build(by, bx, 9))
						{ whilst += 1; continue; }
					if (p_ptr->speclev == 3) p_ptr->speclev = 1;
				}

				/* Type 4 -- Large room (10%) */
				if ((k < 10) && room_build(by, bx, 4))
					{ whilst += 1; continue; }
				if (p_ptr->speclev == 3) p_ptr->speclev = 1;

				/* Type 3 -- Cross room (6%) */
				if ((k < 16) && room_build(by, bx, 3))
					{ whilst += 1; continue; }
				if (p_ptr->speclev == 3) p_ptr->speclev = 1;

				/* Type 2 -- Overlapping (39%) */
				if ((k < 35) && room_build(by, bx, 2))
					{ whilst += 1; continue; }
				if (p_ptr->speclev == 3) p_ptr->speclev = 1;

				/* type 9 -- empty vault (38%) */
				if ((k < 93) && room_build(by, bx, 9))
					{ whilst += 1; continue; }
				if (p_ptr->speclev == 3) p_ptr->speclev = 1;
			}

			/* Attempt a trivial room */
			if (room_build(by, bx, 1))
				{ whilst += 1; continue; }
		}
	}

	build_colors(dhei, dwid, colors, counts);
	clear_small_regions(dhei, dwid, colors, counts);
	join_regions(dhei, dwid, colors, counts);

#if togglestreamers
#else
	if ((mutmod == 5) || (mutmod == 1) || /* the two most dense modes */
		(p_ptr->theme == 10) ||			  /* dwarf mines */
		((mutmod == 4) && (randint(100) < 16))) /* and sometimes the random mode */
	{
		/* Don't know why streamers weren't in caverns originally */
		/* more veins to mine in a mine */
		smag = DUN_STR_MAG - rand_int(2);
		sqtz = DUN_STR_QUA - rand_int(2);

		/* Hack -- Add some magma streamers */
		for (i = 0; i < smag; i++)
		{
			build_streamer(FEAT_MAGMA, DUN_STR_MC, 0, 0);
		}

		/* Hack -- Add some quartz streamers */
		for (i = 0; i < sqtz; i++)
		{
			build_streamer(FEAT_QUARTZ, DUN_STR_QC, 0, 0);
		}
	}
#endif /* toggle streamers */

	/* Place 2-3 down stairs near some walls (was randint(3) ) */
 	alloc_stairs(FEAT_MORE, 1 + randint(2), 3);

	/* Place 1-2 up stairs near some walls */
	alloc_stairs(FEAT_LESS, randint(2), 3);

	/* General some rubble, traps and monsters */

	k = (p_ptr->depth / 3);
	/* Basic amount (made more random) */
	if (k > 8) k = 6 + rand_int(k-7); /* was randint(4)*/
	else if (k < 5) k = 1 + randint(3);
	if (k > 10) k = 9 + rand_int(3); /* cap */
	/* k = MAX(MIN(p_ptr->depth / 3, 10), 2);  original from V code */

	/* Scale number of monsters & items by cavern size */
	k = (2 * k * (dhei *  dwid)) / (DUNGEON_HGT * DUNGEON_WID);

	/* if it's a wide open dungeon place trees as if it's mutmod 3 (least dense mod) */
	if ((density > 39) || ((density > 35) && (mutmod == 3))) fmod = 9; /* exists only for this purpose */
	else if (density > 36) fmod = 3;
	else if ((density > 33) && (!(mutmod == 3))) fmod = 2;
	else fmod = mutmod;

	/* place traps, rubble, and maybe trees, statues, puddles, & pits */
	alloc_terrain(k, fmod);

	/* Determine the character location */
	new_player_spot();

	/* adjust k for monster population (to prevent having more monsters on early levels) */
	if ((k <= 9) && (k > 3) && (p_ptr->depth / 3 > 2)) k = (p_ptr->depth / 3);
	else if ((k <= 9) && (k > 3)) k = 2;
	/* Put some monsters in the dungeon */
	for (i = MIN_M_ALLOC_LEVEL + randint(8) + k; i > 0; i--)
		alloc_monster(rand_int(5), TRUE);

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, 3 + randint(7));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, 3 + randint(5));

	/* chance for shallow caverns to be lit (max dL28) */
	if (litcave) 
	{
		for (y = 0; y < dhei; y++) 
		{
			for (x = 0; x < dwid; x++) 
			{
				cave_info[y][x] |= (CAVE_GLOW);
			}
		}
	}

	if (cheat_room) msg_format("cavern level (mutmod %d, density %d)", mutmod, density);
	/* mark that the level is a cavern (again- reset to 1) */
	p_ptr->speclev = 1;
	
	return TRUE;
}
#endif

/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static void cave_gen(void)
{
	int i, k, y, x, y1, x1;
	int by, bx, evg, smag, sqtz;
	int usedepth, roomnum, smsize = 0;

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
	if ((p_ptr->depth > 11) && (rand_int(DUN_DEST - (badluck/3)) == 0)) destroyed = TRUE;

	/* Hack -- No destroyed "quest" levels */
	if (is_quest(p_ptr->depth)) destroyed = FALSE;

	/* don't destroy themed levels (unless appropriate) */
	/* only icky place, volcano, earth cave & dwarf mine are allowed to be destroyed */
	if ((p_ptr->theme) && (!(p_ptr->theme == 3)) && (!(p_ptr->theme == 4)) &&
		(!(p_ptr->theme == 5)) && (!(p_ptr->theme == 10)))
	{
		destroyed = FALSE;
	}

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = DUNGEON_HGT / BLOCK_HGT;
	dun->col_rooms = DUNGEON_WID / BLOCK_WID;
	
	/* vary the size of the dungeon (occationally) */
	roomnum = DUN_ROOMS;
	if (randint(100) < 6) /* 6 to 8, 100 only while testing */
	{
		if (randint(100) < 65)
		{
			roomnum = (roomnum * 4) / 5;
			dun->row_rooms = (dun->row_rooms * 4) / 5;
			dun->col_rooms = (dun->col_rooms * 4) / 5;
			if (cheat_room) msg_print("(smaller level: 4/5)");
			smsize = 2;
		}
		else if (randint(100) < 34)
		{
			roomnum = (roomnum * 3) / 4;
			dun->row_rooms = (dun->row_rooms * 3) / 4;
			dun->col_rooms = (dun->col_rooms * 3) / 4;
			if (cheat_room) msg_print("(smaller level: 3/4)");
			smsize = 3;
		}
		else if (randint(100) < 12) 
		{
			roomnum = (roomnum * 6) / 7;
			dun->row_rooms = (dun->row_rooms * 7) / 8;
			dun->col_rooms = (dun->col_rooms * 7) / 8;
			if (cheat_room) msg_print("(smaller level: 6/7)");
		}
		else if (randint(100) < 12) 
		{
			roomnum = (roomnum * 2) / 3;
			dun->row_rooms = (dun->row_rooms * 3) / 5;
			dun->col_rooms = (dun->col_rooms * 3) / 5;
			if (cheat_room) msg_print("(smaller level: 2/3)");
			smsize = 4;
			destroyed = FALSE;
		}
		else if (randint(100) < 12) 
		{
			roomnum = roomnum / 2;
			dun->row_rooms = (dun->row_rooms * 6) / 10;
			dun->col_rooms = (dun->col_rooms * 6) / 10;
			if (cheat_room) msg_print("(smaller level: half");
			smsize = 4;
			destroyed = FALSE;
		}
		else 
		{
			roomnum = (roomnum * 5) / 6;
			dun->row_rooms = (dun->row_rooms * 5) / 6;
			dun->col_rooms = (dun->col_rooms * 5) / 6;
			if (cheat_room) msg_print("(smaller level: 5/6)");
			smsize = 1;
		}
	}

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
	for (i = 0; i < roomnum; i++)
	{
		int roomodds = DUN_UNUSUAL;
		/* Pick a block for the room */
		by = rand_int(dun->row_rooms);
		bx = rand_int(dun->col_rooms);

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
			if (p_ptr->find_vault > 98) p_ptr->find_vault = 98;
			roomodds -= p_ptr->find_vault;
            if (roomodds > 160) roomodds = 160;
			if (roomodds < 2) roomodds = 2;
			destroyed = FALSE;
        }
        else if (goodluck > 6) roomodds -= goodluck/2;

		/* Destroyed levels are boring */
		if (destroyed)
		{
			int pth = p_ptr->depth/3 + 8;
			if (pth > 22) pth = 22;

				/* (slightly less boring than they used to be) */
			/* can use empty vault design */
			if ((rand_int(DUN_UNUSUAL) < pth) && (room_build(by, bx, 9)))
				continue;

			/* Overlapping */
			if ((rand_int(DUN_UNUSUAL) < pth + 7) && room_build(by, bx, 2)) continue;

			/* Cross room */
			if ((rand_int(DUN_UNUSUAL) < (pth+1)/2 ) && room_build(by, bx, 3)) continue;

			/* moated room */
			if ((rand_int(DUN_UNUSUAL) < (pth+2)/3 ) && room_build(by, bx, 4)) continue;

			/* make a blah room */
			if (room_build(by, bx, 1)) continue;

			/* Never mind */
			continue;
		}
		
		usedepth = p_ptr->depth; /* only used for rolling against roomodds */
        /* occationally use deeper fake room depth for more randomness */
        if ((p_ptr->depth > 4) && (p_ptr->depth < 99) && (rand_int(100) < 2))
        {
            if (p_ptr->depth < 45) usedepth = 50;
            else usedepth += 2 + rand_int(9);
        }
        
		/* Only attempt a GV if you are on the first room */
		if ((i == 0) && (rand_int(roomodds) < usedepth))
		{
			int gvchance = 0;

			/* greater vaults more common after dl100 */
			/* handy for testing as well as making the >100 levels more interesting */
			if (p_ptr->depth == 101) gvchance = 500; /* 45% */
			else if (p_ptr->depth > 101) gvchance = 280 + (p_ptr->depth - 99); /* min +20 for automatic find_vault */
			else if ((p_ptr->depth >= 93) && (p_ptr->depth < 99)) gvchance = 300;
			else if (p_ptr->depth >= 85) gvchance = 250;
			else if (p_ptr->depth >= 75) gvchance = 200;
			else if (p_ptr->depth >= 65) gvchance = 150; /* 15% */
			else if (usedepth >= 55) gvchance = p_ptr->depth + 10;
			else if (p_ptr->depth >= 10) gvchance = p_ptr->depth - 5;

			if (p_ptr->seek_vault) 
				gvchance += (p_ptr->find_vault * 2) / 3;

			/* roll to attempt a greater vault */
			if ((rand_int(1000) < gvchance) && room_build(by, bx, 8)) continue;
		}
        
		/* Attempt an unusual room */
		/* (DUN_UNUSUAL is 200, set at L109) */
		/* roomodds = DUN_UNUSUAL unless find vault spell is used */
		if (rand_int(roomodds) < usedepth)
		{
			/* vault seeking */
			if (p_ptr->seek_vault)
            {
				if (p_ptr->find_vault > 98) p_ptr->find_vault = 98; /* paranoia */

				k = rand_int(100 - p_ptr->find_vault);
               if ((p_ptr->depth > 4) && (p_ptr->depth < 120))
               {
                  /* seek_vault uses inverted depth */
                  roomodds -= 120 - p_ptr->depth;
               }
               else roomodds -= (p_ptr->find_vault/3) + 1;
            }
			/* Roll for room type */
			else k = rand_int(100);

			/* Attempt a very unusual room */
			if (rand_int(roomodds) < usedepth)
			{
			    int nestodd = 0, pitodd = 0, evodd = 0;
                
				if (p_ptr->depth < 21) nestodd = 45;
				else nestodd = 46 + (p_ptr->depth+4)/15;
				if ((p_ptr->theme) && (nestodd > 51)) nestodd = 51;
				else if (nestodd > 52) nestodd = 52;
				
				evodd = nestodd + 4;
				
				if (p_ptr->theme) pitodd = 37;
				else pitodd = 39;
				
				/* find vault shouldn't find pits and nests as often */
				if ((p_ptr->seek_vault) && (k < 48) && (k > 25) && 
					(randint(100) < 55)) k = rand_int(20) + goodluck/4;

				/* Type 8 -- Lesser vault (15%) */
				if ((k < 15) && room_build(by, bx, 7)) continue;

				/* Type 7 -- Medium vault (10%) */
				if ((k < 25) && room_build(by, bx, 10)) continue;

				/* Type 6 -- Monster pit (12/13%, was 15%) */
				if ((k < pitodd) && room_build(by, bx, 6)) continue;

				/* Type 5 -- Monster nest (6%, 8% or 9%, was 10%) */
				if ((k < nestodd) && room_build(by, bx, 5)) continue;

				/* type 9 -- empty vault */
				if ((k < evodd) && room_build(by, bx, 9)) continue;
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
		if (rand_int((DUN_UNUSUAL + roomodds)/2) < 45 + usedepth/8)
		{
			k = rand_int(99);

			/* odds for empty greater vault (much more likely after L100) */
			if ((is_quest(p_ptr->depth)) || (p_ptr->depth < 70)) evg = 0;
			else if (p_ptr->depth > 100) evg = p_ptr->depth - 60;
			else evg = (p_ptr->depth-50) / 2;

			/* extremely rare empty greater vault (only deeper than dL74) */
			if ((k == 1) && (rand_int(100) < evg) && room_build(by, bx, 11)) continue;
			
			/* small second chance for lesser vault */
			if ((k == 1) && room_build(by, bx, 7)) continue;

			/* rare empty medium vault */
			if ((k == 2) && (rand_int(100) < evg+42) && (p_ptr->depth > 15) && room_build(by, bx, 12)) continue;

			/* empty vault */
			if ((k < 35) && room_build(by, bx, 9)) continue;

			/* another chance for overlapping room */
			if ((k < 38) && room_build(by, bx, 2)) continue;
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

#ifdef allow_rivers
	/* try using build_streamer to create an underground river */
	/* (this didn't work very well) */
	if (((p_ptr->theme == 1) || (p_ptr->theme == 2) || (p_ptr->theme == 9)) &&
		(randint(100) < 22))
	{
		/* see how this works to make an underground river */
		build_streamer(FEAT_WATER, 0, 0, 0);
		smag -= randint(2);
		sqtz -= 1;
	}
#endif

	/* Hack -- Add some magma streamers */
	for (i = 0; i < smag; i++)
	{
		/* in dwarf mine, extra streamer is less likely to have treasure */
		if (i == DUN_STR_MAG) build_streamer(FEAT_MAGMA, DUN_STR_MC + 10, 0, 0);
		else build_streamer(FEAT_MAGMA, DUN_STR_MC, 0, 0);
	}

	/* Hack -- Add some quartz streamers */
	for (i = 0; i < sqtz; i++)
	{
		/* in dwarf mine, extra streamer is less likely to have treasure */
		if (i == DUN_STR_QUA) build_streamer(FEAT_QUARTZ, DUN_STR_QC + 20, 0, 0);
		else build_streamer(FEAT_QUARTZ, DUN_STR_QC, 0, 0);
	}


	/* Destroy the level if necessary */
	if (destroyed) destroy_level();


	/* Place 3 or 4 down stairs near some walls */
	alloc_stairs(FEAT_MORE, rand_range(3, 4), 3);

	/* Place 1 or 2 up stairs near some walls */
	alloc_stairs(FEAT_LESS, rand_range(1, 2), 3);

	k = (p_ptr->depth / 3);
	/* Basic amount (made more random) */
	if (k > 9) k = 7 + rand_int(k-6); /* was randint(4)*/
	else if (k < 6) k = 1 + randint(3);
	
	/* reduce amount slightly for smaller levels */
	if ((smsize > 1) && (k > 12)) k -= 2 + rand_int(smsize);
	else if (smsize > 1) k -= 1 + rand_int(smsize);
	else if ((smsize) && (randint(100) < 50)) k -= 1;
	if (k < 2) k = 2;

	/* separated into another function */
	/* (makes terrain appropriate for the theme, if any) */
	alloc_terrain(k, 0);

	/* Determine the character location */
	new_player_spot();

	/* reset low 'k' amounts (to prevent having more monsters on early levels) */
    if (k <= 9) k = (p_ptr->depth / 3);
    /* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL + randint(8);
	/* themed levels more likely to have more monsters */
	if ((i < MIN_M_ALLOC_LEVEL + 7) && (p_ptr->theme)) i += randint(2);
	/* reduce population slightly for smaller levels */
	if (smsize > 1) i -= randint(smsize);
	else if ((smsize) && ((p_ptr->depth < 11) || (randint(100) < 50))) i -= 1;

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
}



/*
 * Builds a store at a given pseudo-location
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
	while (((tmp == 0) && (yy == 1)) ||
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
 * Generate the consistent town features, and place the player
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
	
	/* place some trees  ..The problem with this is */
	/* that they'll grow back real fast if you destroy one of them. */
    alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TREES, 2 + randint(6));


	/* Place the stairs */
	while (TRUE)
	{
		/* Pick a location at least three spaces from the outer walls */
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
	bool daytime, forbidfullmoon;
	
	/* force some time between full moons in the town */
	if (((turn - p_ptr->lastfullmoon < 65000) &&
		(p_ptr->lastfullmoon)) ||
        (turn < 2500)) forbidfullmoon = TRUE;
	else forbidfullmoon = FALSE;

	/* same night as last visit to town */
    if ((turn - p_ptr->lastfullmoon < 5000) && (p_ptr->lastfullmoon))
	{
		p_ptr->theme = 7;
	}
	/* FULL_MOON theme is rarely allowed in town */
	else if ((rand_int(100 + goodluck) < ((p_ptr->max_depth+9)/20) + (badluck/3)) &&
		(p_ptr->max_depth > 8) && (p_ptr->lev > 5) && (!forbidfullmoon))
	{
		p_ptr->theme = 7;
		p_ptr->lastfullmoon = turn;
	}
	/* otherwise never a theme in the town */
	else p_ptr->theme = 0;

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

	/* The town is lit if the daylight spell is active */
	if (p_ptr->timed[TMD_DAYLIGHT]) daytime = TRUE;

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
 * (used every time the PC goes to a new dungeon level including the town)
 *
 * Hack -- regenerate any "overflow" levels
 * Hack -- allow auto-scumming via a gameplay option.
 *
 * Note that this function resets "cave_feat" and "cave_info" directly.
 */
void generate_cave(void)
{
	int y, x, num, cavernodd;
	s16b danger_lev;
	bool feelagain = FALSE;

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

		/*** Choose level theme  (moved here from cave_gen) ***/
		/* possible themed level (see RF7 flags in defines.h) */
		if ((p_ptr->depth > 2) && (themed_levels))
		{
			bool do_theme = FALSE;
			/* usually 7% chance */
			int themechance = 7;
			/* for testing *** themechance = 75; */
			/* less likely close to Morgoth */
			if ((p_ptr->depth > 95) && (p_ptr->depth < 99)) themechance -= 2;
			if (is_quest(p_ptr->depth)) themechance = 0;
			/* less likely in very early levels */
			if (p_ptr->depth < 10) themechance = p_ptr->depth - 2;
			/* more likely deeper than Morgoth */
			if (p_ptr->depth > 100) themechance *= 2;
			/* roll for themed type */
			if (rand_int(100) < themechance) do_theme = TRUE;

			/* chose type: 15 types of themed levels */
			if ((do_theme) || (p_ptr->speclev == 20) || 
				(p_ptr->speclev == 40) || (p_ptr->speclev == 60))
			{
				int dieth = rand_int(91 + ((p_ptr->depth*3)/4) + ((badluck+1)/2));
				/* occationally randomize more */
				if (randint(100) < 10) dieth = rand_int(136) + 2;
				/* icky place and bug cave meant for earlier levels */
				if ((p_ptr->depth > 64) && (dieth < 21)) dieth = rand_int(136) + 4;
				if (dieth < 10) p_ptr->theme = 3; /* ICKY_PLACE */
				else if (dieth < 20) p_ptr->theme = 11; /* BUG_CAVE */
				else if (dieth < 31) p_ptr->theme = 2;  /* FFOREST */
				else if (dieth < 43) p_ptr->theme = 1; /* CFOREST */
				else if (dieth < 51) p_ptr->theme = 10; /* DWARF_MINE */
				else if (dieth < 54) p_ptr->theme = 15; /* BARRACKS (less common) */
				else if (dieth < 65) p_ptr->theme = 7; /* FULL_MOON */
				else if (dieth < 72) p_ptr->theme = 5; /* EARTHY_CAVE */
				else if (dieth < 79) p_ptr->theme = 6; /* WINDY_CAVE */
				else if (dieth < 87) p_ptr->theme = 4; /* VOLCANO */
				else if (dieth < 98) p_ptr->theme = 9; /* SWAMP */
				else if (dieth < 110) p_ptr->theme = 8; /* CASTLE */
				else if (dieth < 120) p_ptr->theme = 13; /* NIGHTMARE */
				else if (dieth < 130) p_ptr->theme = 12; /* GREPSE */
				/* hell hall rare after dl105 */
				else if ((dieth < 140) && (p_ptr->depth < 105)) p_ptr->theme = 14; /* hell hall */
				else if (dieth < 146) p_ptr->theme = randint(15);
				else p_ptr->theme = randint(14);
				/* reset theme die */
				dieth = rand_int(91 + ((p_ptr->depth*3)/4) + ((badluck+1)/2));
				/* scariest two themes never appear early */
				if (((p_ptr->theme == 14) && (p_ptr->depth < 30)) ||
					(((p_ptr->theme == 12) || (p_ptr->theme == 15)) && (p_ptr->depth < 15)))
				{
					if (dieth < 9) p_ptr->theme = 1 + rand_int(3);
					else if (dieth < 17) p_ptr->theme = 9 + rand_int(3);
					else if ((dieth < 24) && (p_ptr->depth > 10)) p_ptr->theme = 7; /* FULL_MOON */
					else if ((dieth < 32) && (p_ptr->depth > 9)) p_ptr->theme = 15; /* BARRACKS */
	                else p_ptr->theme = 0;
				}
				/* These not meant for very deep levels */
				if (((p_ptr->theme == 2) || (p_ptr->theme == 10) ||
					(p_ptr->theme == 15)) && (p_ptr->depth > 79))
				{
					if (dieth > 110) p_ptr->theme = 12 + rand_int(3);
					else if (dieth > 68) p_ptr->theme = 0;
					else if ((dieth > 59) && (p_ptr->theme == 2)) p_ptr->theme = 16;
				}
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

			/* speclev 60+ = themed level with vault(s) */
			if (p_ptr->speclev >= 60)
			{
				if (p_ptr->speclev > 60) p_ptr->theme = p_ptr->speclev - 60;
				p_ptr->speclev = 1;
			}
			/* speclev 40+ = themed cavern */
			else if (p_ptr->speclev >= 40)
			{
				if (p_ptr->speclev > 40) p_ptr->theme = p_ptr->speclev - 40;
				p_ptr->speclev = 2;
			}
			/* speclev 20+ = chosen theme */
			else if (p_ptr->speclev > 20) p_ptr->theme = p_ptr->speclev - 20;
			/* persistant themed level: dL105 is always hell */
			else if (p_ptr->depth == 105) p_ptr->theme = 14;
		}
		else p_ptr->theme = 0;

#ifdef allow_rivers_testing
		/* for testing rivers (always a forest or swamp) */
		p_ptr->theme = randint(3);
		if (p_ptr->theme == 3) p_ptr->theme = 9;
#endif

		/* odds of getting a cavern level normally 9% */
		cavernodd = 9;
		/* barracks are never caverns */
		if (p_ptr->theme == 15) cavernodd = 0;
		/* forests are caverns with a lot of trees (dwarf mines also very likely) */
		if ((p_ptr->theme == 1) || (p_ptr->theme == 2) || (p_ptr->theme == 10)) cavernodd = 20;
		/* icky place, earth cave, windy cave, volcano, swamp more likely */
		if ((p_ptr->theme == 4) || (p_ptr->theme == 5) || (p_ptr->theme == 9)) cavernodd = 15;
		if ((p_ptr->theme == 6) || (p_ptr->theme == 3)) cavernodd = 14;
		/* castle, hell hall, full moon less likely */
		if ((p_ptr->theme == 8) || (p_ptr->theme == 14) || (p_ptr->theme == 7)) cavernodd = 5;

		/* wizmode force cavern command */
		if (p_ptr->speclev == 2) cavernodd = 100;
		/* wizmode seek vault command */
		if (p_ptr->speclev == 1)
		{
			p_ptr->seek_vault = TRUE;
			p_ptr->find_vault = 55;
		}

		/* reset special level holder */
		p_ptr->speclev = 0;

		/* Build the town */
		if (!p_ptr->depth) town_gen();
		/* 9 chance, (90 only while testing) */
		else if ((p_ptr->depth > 8) && (rand_int(100) < cavernodd) && 
			(!(p_ptr->seek_vault)) && (!is_quest(p_ptr->depth)))
		{
			/* in case of failure */
			if (!cavern_gen()) cave_gen(); 
		}
		/* Build a normal dungeon */
		else cave_gen();

		/* Extract the feeling (tentative scale) */
		if (rating > 100) feeling = 2;
		else if (rating > 80) feeling = 3;
		else if (rating > 60) feeling = 4;
		else if (rating > 40) feeling = 5;
		else if (rating > 30) feeling = 6;
		else if (rating > 20) feeling = 7;
		else if (rating > 9) feeling = 8;
		else if (rating > 0) feeling = 9;
		else feeling = 10;

#if 0
		/* Hack -- Have a special feeling sometimes */
		if (good_item_flag && adult_no_preserve) feeling = 1;
#endif

		/* It takes 1000 game turns for "feelings" to recharge */
		if (((turn - old_turn) < 1000) && (old_turn > 1) && (!cheat_noid)) feeling = 0;

		/* Hack -- no feeling in the town */
		if (!p_ptr->depth) feeling = 0;

		/* get the level of danger (cmd4.c) */
        danger_lev = get_danger_feeling();
		
		/* very high treasure level and low danger level: add a monster or three */
		if ((feeling > 0) && (feeling < 4) && (danger_lev > 7))
		{
			(void)alloc_monster(0, TRUE);
			if (rand_int(100) < 70) (void)alloc_monster(0, TRUE);
			if (rand_int(100) < 30) (void)alloc_monster(0, TRUE);
			feelagain = TRUE;
        }
        /* kindof high treasure, very low danger: add a monster or two (usually) */
        else if ((feeling > 0) && (feeling < 5) && (danger_lev > 8))
		{
			if (rand_int(100) < 70) { (void)alloc_monster(0, TRUE); feelagain = TRUE; }
			if (rand_int(100) < 30) { (void)alloc_monster(0, TRUE); feelagain = TRUE; }
        }
        else if ((feeling > 0) && (feeling < 6) && (danger_lev > 9))
		{
			if (rand_int(100) < 67) { (void)alloc_monster(0, TRUE); feelagain = TRUE; }
        }

		/* Re-Extract the feeling (tentative scale) */
		/* (might have changed after adding monsters */
		if (feelagain)
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
		
		/* removing level feelings also turns on a much milder version of auto-scum */
		else if (((level_numb) || (randint(100) < 50)) && (num < 100) && (p_ptr->depth > 9))
		{
			/* Usually require at least a little "goodness" */
			if ((randint(100) < 68 + goodluck - badluck) && ((feeling > 9) ||
			    ((p_ptr->depth >= 20) && (feeling > 8)) ||
			    ((p_ptr->depth > 35) && (feeling > 7))))
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
		/* occationally have a false feeling */
		if ((rand_int(105) < badluck + 1) && (feeling > 1) && (!cheat_noid))
		{
            if (feeling < 6) feeling += randint(5);
            else feeling -= randint(3);
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

    /* reset vault seeking */
	if (p_ptr->seek_vault)
    {
       p_ptr->seek_vault = FALSE;
       p_ptr->find_vault = 0;
    }
    else if (p_ptr->find_vault >= 5) p_ptr->find_vault -= 5;

	/* Remember when this level was "created" */
	old_turn = turn;
	p_ptr->danger_turn = turn;
}
