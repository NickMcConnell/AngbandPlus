/* File: generate.c */

/* Purpose: Dungeon generation */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#define SAFE_MAX_ATTEMPTS 5000

int template_race;

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
 * Note that the use of 11x11 blocks (instead of the old 33x11 blocks)
 * allows more variability in the horizontal placement of rooms, and
 * at the same time has the disadvantage that some rooms (two thirds
 * of the normal rooms) may be "split" by panel boundaries.  This can
 * induce a situation where a player is in a room and part of the room
 * is off the screen.  It may be annoying enough to go back to 33x11
 * blocks to prevent this visual situation.
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
#define DUN_UNUSUAL 194 /* Level/chance of unusual room (was 200) */
#define DUN_DEST    18  /* 1/chance of having a destroyed level */
#define SMALL_LEVEL 3   /* 1/chance of smaller size (3)*/
#define EMPTY_LEVEL 15  /* 1/chance of being 'empty' (15)*/
#define WATER_LEVEL 5   /* 1/chance of being 'flooded' (5)*/
#define DARK_EMPTY  5   /* 1/chance of arena level NOT being lit (2)*/
#define XTRA_MAGIC  10  /* 1/chance of having a level with more magic (10)*/
#define DUN_WILD_VAULT  50     /* Chance of finding a wilderness vault. */
#define DUN_WAT_RNG     2   /* Width of rivers */
#define DUN_WAT_CHG     50  /* 1 in 50 chance of junction in river */ 
#define DUN_CAVERN      30  /* 1/chance of having a cavern level */

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
#define DUN_STR_DEN     5	/* Density of streamers */
#define DUN_STR_RNG     2	/* Width of streamers */
#define DUN_STR_MAG     3	/* Number of magma streamers */
#define DUN_STR_MC     90	/* 1/chance of treasure per magma */
#define DUN_STR_QUA	    2	/* Number of quartz streamers */
#define DUN_STR_QC     40	/* 1/chance of treasure per quartz */
#define DUN_STR_WLW     1	/* Width of lava & water streamers -KMW- */
#define DUN_STR_DWLW    8	/* Density of water & lava streams -KMW- */


/*
 * Dungeon treausre allocation values
 */
#define DUN_AMT_ROOM	9	/* Amount of objects for rooms */
#define DUN_AMT_ITEM	3	/* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD	3	/* Amount of treasure for rooms/corridors */
#define DUN_AMT_ALTAR   3       /* Amount of altars */
#define DUN_AMT_BETWEEN 2       /* Amount of between gates */

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
#define ALLOC_TYP_ALTAR         6       /* Altar */
#define ALLOC_TYP_BETWEEN       7       /* Between */



/*
 * The "size" of a "generation block" in grids
 */
#define BLOCK_HGT	11
#define BLOCK_WID	11

/*
 * Maximum numbers of rooms along each axis (currently 6x6)
 */
#define MAX_ROOMS_ROW	(MAX_HGT / BLOCK_HGT)
#define MAX_ROOMS_COL	(MAX_WID / BLOCK_WID)


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
 * Array of room types depths
 */
static s16b roomdep[] =
{
        0,                              /* 0 = Nothing */
        1,                              /* 1 = Simple (33x11) */
        1,                              /* 2 = Overlapping (33x11) */
        3,                              /* 3 = Crossed (33x11) */
        3,                              /* 4 = Large (33x11) */
        5,                              /* 5 = Monster nest (33x11) */
        5,                              /* 6 = Monster pit (33x11) */
        5,                              /* 7 = Lesser vault (33x22) */
        10,                             /* 8 = Greater vault (66x44) */
        1,                              /* 9 = Circular rooms (22x22) */
        3,                              /* 10 = Fractal cave (42x24) */
};

/*
 * Feature of the dungeon(in function of dungeon_type)
 */

byte feat_wall_outer = FEAT_WALL_OUTER;
byte feat_wall_inner = FEAT_WALL_INNER;
s16b floor_type[100], fill_type[100];

/*
 * The array of to keep monsters [max_m_idx]
 */
monster_type *km_list;

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
static bool new_player_spot(void)
{
	int	y, x;
	int max_attempts = 5000;

	/* Place the player */
	while (max_attempts--)
	{
		/* Pick a legal spot */
		y = rand_range(1, cur_hgt - 2);
		x = rand_range(1, cur_wid - 2);

		/* Must be a "naked" floor grid */
		if (!cave_naked_bold(y, x)) continue;

		/* Refuse to start on anti-teleport grids */
		if (cave[y][x].info & (CAVE_ICKY)) continue;

		/* Done */
		break;

	}

	if (max_attempts < 1) /* Should be -1, actually if we failed... */
		return FALSE;


	/* Save the new player grid */
	py = y;
	px = x;

	return TRUE;
}



/*
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(y, x)"
 *
 * We count only granite walls and permanent walls.
 */
static int next_to_walls(int y, int x)
{
	int	k = 0;

        if (f_info[cave[y+1][x].feat].flags1 & FF1_WALL) k++;
        if (f_info[cave[y-1][x].feat].flags1 & FF1_WALL) k++;
        if (f_info[cave[y][x+1].feat].flags1 & FF1_WALL) k++;
        if (f_info[cave[y][x-1].feat].flags1 & FF1_WALL) k++;

	return (k);
}



/*
 * Convert existing terrain type to rubble
 */
static void place_rubble(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create rubble */
	c_ptr->feat = FEAT_RUBBLE;
}



/*
 * Convert existing terrain type to "up stairs"
 */
static void place_up_stairs(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create up stairs */
        if (rand_int(3) == 1)
                c_ptr->feat = FEAT_SHAFT_UP;
        else
                c_ptr->feat = FEAT_LESS;
        c_ptr->special = 0;
}

/*
 * Convert existing terrain type to "down stairs" with dungeon changing.
 */
static void place_magical_stairs(int y, int x, byte next)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create up stairs */
        c_ptr->feat = FEAT_MORE;
        c_ptr->special = next;
}


/*
 * Convert existing terrain type to "down stairs"
 */
static void place_down_stairs(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create down stairs */
        /* All thoses tests are necesary because a shaft can jump up to 4 levels */
        if ((rand_int(3) == 1) && (dun_level + 4 <= d_info[dungeon_type].maxdepth))
                c_ptr->feat = FEAT_SHAFT_DOWN;
        else
                c_ptr->feat = FEAT_MORE;
        c_ptr->special = 0;
}



/*
 * Place an altar at the given location
 */
static void place_altar(int y, int x) {

        cave_type *c_ptr = &cave[y][x];
        int alt, rar;

  while (TRUE) {
    alt = rand_int(MAX_GODS - 1);
    rar = deity_info[alt].rarity;

    if (dun_level < randnor(rar*10, 3) ||
        rand_int(rar) > 0) continue;

    break;
  }

  c_ptr->feat = FEAT_ALTAR_HEAD + alt;
}


/*
 * Place a between gate at the given location
 */
static void place_between(int y, int x) {

        cave_type *c_ptr = &cave[y][x],*c1_ptr;
        int gx,gy;

        while(TRUE)
        {
                /* Location */
                gy = rand_int(cur_hgt);
                gx = rand_int(cur_wid);

                /* Require "naked" floor grid */
                if (!cave_naked_bold(gy, gx)) continue;

                break;                
        }
        c1_ptr = &cave[gy][gx];

        c_ptr->feat = FEAT_BETWEEN;
        c_ptr->special = gx + (gy << 8);
        c1_ptr->feat = FEAT_BETWEEN;
        c1_ptr->special = x + (y << 8);
}

/*
 * Place an up/down staircase at given location
 */
static void place_random_stairs(int y, int x)
{
	/* Paranoia */
	if (!cave_clean_bold(y, x)) return;

	/* Choose a staircase */
	if (!dun_level)
	{
		place_down_stairs(y, x);
	}
        else if (is_quest(dun_level) && (dun_level > 1))
	{
                place_up_stairs(y, x);
	}
        else if (dun_level >= d_info[dungeon_type].maxdepth)
        {
                if (d_info[dungeon_type].next)
                {
                        place_magical_stairs(y, x, d_info[dungeon_type].next);
                }
                else
                {
                        place_up_stairs(y, x);
                }
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
 * Place a locked door at the given location
 */
static void place_locked_door(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create locked door */
	c_ptr->feat = FEAT_DOOR_HEAD + randint(7);
}


/*
 * Place a secret door at the given location
 */
static void place_secret_door(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create secret door */
	c_ptr->feat = FEAT_SECRET;
}


/*
 * Place a random type of door at the given location
 */
static void place_random_door(int y, int x)
{
	int tmp;

	cave_type *c_ptr = &cave[y][x];

	/* Choose an object */
	tmp = rand_int(1000);

	/* Open doors (300/1000) */
	if (tmp < 300)
	{
		/* Create open door */
		c_ptr->feat = FEAT_OPEN;
	}

	/* Broken doors (100/1000) */
	else if (tmp < 400)
	{
		/* Create broken door */
		c_ptr->feat = FEAT_BROKEN;
	}

	/* Secret doors (200/1000) */
	else if (tmp < 600)
	{
		/* Create secret door */
		c_ptr->feat = FEAT_SECRET;
	}

	/* Closed doors (300/1000) */
	else if (tmp < 900)
	{
		/* Create closed door */
		c_ptr->feat = FEAT_DOOR_HEAD + 0x00;
	}

	/* Locked doors (99/1000) */
	else if (tmp < 999)
	{
		/* Create locked door */
		c_ptr->feat = FEAT_DOOR_HEAD + randint(7);
	}

	/* Stuck doors (1/1000) */
	else
	{
		/* Create jammed door */
		c_ptr->feat = FEAT_DOOR_HEAD + 0x08 + (byte)rand_int(8);
	}
}



/*
 * Places some staircases near walls
 */
static void alloc_stairs(int feat, int num, int walls, int branch)
{
	int		y, x, i, j, flag;

	cave_type	*c_ptr;


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
				y = rand_int(cur_hgt);
				x = rand_int(cur_wid);

				/* Require "naked" floor grid */
				if (!cave_naked_bold(y, x)) continue;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(y, x) < walls) continue;

				/* Access the grid */
				c_ptr = &cave[y][x];

				/* Town -- must go down */
				if (!dun_level)
				{
					/* Clear previous contents, add down stairs */
                                        if (randint(3) == 1)
                                               c_ptr->feat = FEAT_SHAFT_DOWN;
                                        else
                                               c_ptr->feat = FEAT_MORE;
				}

				/* Quest -- must go up */
                                else if ((is_quest(dun_level) && (dun_level > 1)) || (dun_level >= d_info[dungeon_type].maxdepth))
				{
					/* Clear previous contents, add up stairs */
                                        if (randint(3) == 1)
                                               c_ptr->feat = FEAT_SHAFT_UP;
                                        else
                                               c_ptr->feat = FEAT_LESS;
				}

				/* Requested type */
				else
				{
					/* Clear previous contents, add stairs */
					c_ptr->feat = feat;
				}

                                c_ptr->special = branch;

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
	int dummy = 0;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		while (dummy < SAFE_MAX_ATTEMPTS)
		{
			bool room;

			dummy++;

			/* Location */
			y = rand_int(cur_hgt);
			x = rand_int(cur_wid);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue;

			/* Check for "room" */
			room = (cave[y][x].info & (CAVE_ROOM)) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

			/* Accept it */
			break;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
                                msg_format("Warning! Could not place object, type : %d!", typ);
			}
		return;
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

                        case ALLOC_TYP_ALTAR:
                        {
                                place_altar(y, x);
                                break;
                        }

                        case ALLOC_TYP_BETWEEN:
                        {
                                place_between(y, x);
                                break;
                        }
		}
	}
}

/*
 * Recursive fractal algorithm to place water through the dungeon.
 */
static void recursive_river(int x1,int y1, int x2, int y2, int feat1, int feat2,int width)
{
	int dx,dy,length,l,x,y;
	int changex, changey;
	int ty,tx;
        bool done;
	
	length=distance(x1,y1,x2,y2);
	if(length>4)
        {	
		/*Divide path in half and call routine twice.
		* There is a small chance of splitting the river
		*/ 
		dx=(x2-x1)/2;
		dy=(y2-y1)/2;
		
		if (dy!=0)
		{
			/* perturbation perpendicular to path */
			changex=randint(abs(dy))*2-abs(dy);
		}
        	else
		{
			changex=0;
		}
		
		if (dx!=0)
		{
			/* perturbation perpendicular to path */
			changey=randint(abs(dx))*2-abs(dx);
		}
		else
		{
			changey=0;
		}
		
		
		
		/* construct river out of two smaller ones */
		recursive_river(x1, y1, x1+dx+changex, y1+dy+changey, feat1, feat2, width);
		recursive_river(x1+dx+changex, y1+dy+changey, x2, y2, feat1, feat2, width);
		
		/* Split the river some of the time -junctions look cool */
		if ((randint(DUN_WAT_CHG)==1)&&(width>0))
		{
			recursive_river(x1+dx+changex, y1+dy+changey, x1+8*(dx+changex),
			  y1+8*(dy+changey), feat1, feat2, width-1);
		}
	}
	else
	{
		/*Actually build the river*/
		for (l=0;l<length;l++)
		{
			x=x1+l*(x2-x1)/length;
			y=y1+l*(y2-y1)/length;
			done=FALSE;
			while(!done)
 			{
				for (ty = y - width - 1; ty <= y + width + 1; ty++)
				{
					for (tx = x - width - 1; tx <= x + width + 1; tx++)
					{
						if (!in_bounds(ty, tx)) continue;
	
						if (cave[ty][tx].feat == feat1) continue;
						if (cave[ty][tx].feat == feat2) continue;
			
						if (distance(ty, tx, y, x) > rand_spread(width, 1)) continue;

						/* Do not convert permanent features */
						if (cave_perma_bold(ty, tx)) continue;
						
						/*
						 * Clear previous contents, add feature
						 * The border mainly gets feat2, while the center gets feat1 */
						if (distance(ty, tx, y, x) > width)
							cave[ty][tx].feat = feat2;
						else
							cave[ty][tx].feat = feat1;

						/* Lava terrain glows */
						if ((feat1 == FEAT_DEEP_LAVA) ||  (feat1 == FEAT_SHAL_LAVA))
						{
							cave[ty][tx].info |= CAVE_GLOW;
						}

						/* Hack -- don't teleport here */
						cave[ty][tx].info |= CAVE_ICKY;
					}
				}	
				done=TRUE;
			}
		}	
 	}
 }

/*
 * Places water through dungeon.
 */
static void add_river(int feat1, int feat2)
{
	int y2, x2;
	int y1=0, x1=0, wid;
	

	/* Hack -- Choose starting point */
	y2 = randint(cur_hgt /2 -2)+cur_hgt/2;
	x2 = randint(cur_wid /2 -2)+cur_wid/2;

	/* Hack -- Choose ending point somewhere on boundary */
	switch(randint(4))
	{
		case 1:
		{
			/* top boundary */
			x1=randint(cur_wid-2)+1;
			y1=1;			
			break;
		}
		case 2:
        	{
			/* left boundary */
			x1=1;
			y1=randint(cur_hgt-2)+1;
			break;
		}
		case 3:
		{
			/* right boundary */
			x1=cur_wid-1;
			y1=randint(cur_hgt-2)+1;
			break;
		}
		case 4:
		{
			/* bottom boundary */
			x1=randint(cur_wid-2)+1;
        		y1=cur_hgt-1;
			break;
		}
	}
	wid = randint(DUN_WAT_RNG);
	recursive_river(x1, y1, x2, y2, feat1, feat2, wid);	
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
	int		i, tx, ty;
	int		y, x, dir;
	int dummy = 0;

	cave_type *c_ptr;

	/* Hack -- Choose starting point */
	y = rand_spread(cur_hgt / 2, 10);
	x = rand_spread(cur_wid / 2, 15);

	/* Choose a random compass direction */
	dir = ddd[rand_int(8)];

	/* Place streamer into dungeon */
	while (dummy < SAFE_MAX_ATTEMPTS)
	{
		dummy++;

		/* One grid per density */
		for (i = 0; i < DUN_STR_DEN; i++)
		{
			int d = DUN_STR_RNG;

			/* Pick a nearby grid */
			while (1)
			{
				ty = rand_spread(y, d);
				tx = rand_spread(x, d);
				if (!in_bounds2(ty, tx)) continue;
				break;
			}

			/* Access the grid */
			c_ptr = &cave[ty][tx];

			/* Only convert "granite" walls */
                        if ((c_ptr->feat > FEAT_WALL_EXTRA) || 
                           (c_ptr->feat < FEAT_WALL_SOLID)) continue;

                        if(c_ptr->feat == FEAT_FLOOR) continue;

			/* Clear previous contents, add proper vein type */
			c_ptr->feat = feat;

			/* Hack -- Add some (known) treasure */
			if (rand_int(chance) == 0) c_ptr->feat += 0x04;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
				msg_print("Warning! Could not place streamer!");
			}
			return;
		}


		/* Advance the streamer */
		y += ddy[dir];
		x += ddx[dir];

		/* Quit before leaving the dungeon */
		if (!in_bounds(y, x)) break;
	}
}



/*
 * Place streams of water, lava, & trees -KMW-
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
	y = rand_spread(cur_hgt / 2, 10);
	x = rand_spread(cur_wid / 2, 15);

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
					if (!in_bounds(ty, tx)) continue;
					break;
				}

				/* Only convert non-permanent features */
				if (killwall == 0)
				{
					if (cave[ty][tx].feat >= FEAT_MAGMA) continue;
					if (cave[ty][tx].feat == FEAT_LESS) continue;
					if (cave[ty][tx].feat == FEAT_MORE) continue;
                                        if (cave[ty][tx].feat == FEAT_SHAFT_UP) continue;
                                        if (cave[ty][tx].feat == FEAT_SHAFT_DOWN) continue;
				}
				else
				{
					if (cave[ty][tx].feat >= FEAT_PERM_EXTRA) continue;
					if (cave[ty][tx].feat == FEAT_LESS) continue;
					if (cave[ty][tx].feat == FEAT_MORE) continue;
                                        if (cave[ty][tx].feat == FEAT_SHAFT_UP) continue;
                                        if (cave[ty][tx].feat == FEAT_SHAFT_DOWN) continue;
				}

				/* Clear previous contents, add proper vein type */
				cave[ty][tx].feat = feat;
			}

			/* Advance the streamer */
			y += ddy[dir];
			x += ddx[dir];

			if (randint(20) == 1)
				dir = ddd[rand_int(8)]; /* change direction */

			/* Stop at dungeon edge */
			if (!in_bounds(y, x)) break;
		}
	}
	else if ((feat == FEAT_DEEP_WATER) ||
	    (feat == FEAT_DEEP_LAVA))
	{
		/* create pool */
		poolsize = 5 + randint(10);
		mid = poolsize / 2;

		/* One grid per density */
		for (i = 0; i < poolsize; i++)
		{
			for (j = 0; j < poolsize; j++)
			{
				tx = x + j;
				ty = y + i;

				if (!in_bounds(ty, tx)) continue;

				if (i < mid)
				{
					if (j < mid)
					{
						if ((i + j + 1) < mid)
							continue;
					}
					else if (j > (mid+ i))
						continue;
				}
				else if (j < mid)
				{
					if (i > (mid + j))
						continue;
				}
				else if ((i + j) > ((mid * 3)-1))
					continue;

				/* Only convert non-permanent features */
				if (cave[ty][tx].feat >= FEAT_PERM_EXTRA) continue;
				if (cave[ty][tx].feat == FEAT_LESS) continue;
				if (cave[ty][tx].feat == FEAT_MORE) continue;
                                if (cave[ty][tx].feat == FEAT_SHAFT_UP) continue;
                                if (cave[ty][tx].feat == FEAT_SHAFT_DOWN) continue;

				/* Clear previous contents, add proper vein type */
				cave[ty][tx].feat = feat;
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

	cave_type *c_ptr;

	/* Note destroyed levels */
        if ((cheat_room)||(p_ptr->precognition)) msg_print("Destroyed Level");

	/* Drop a few epi-centers (usually about two) */
	for (n = 0; n < randint(5); n++)
	{
		/* Pick an epi-center */
		x1 = rand_range(5, cur_wid-1 - 5);
		y1 = rand_range(5, cur_hgt-1 - 5);

		/* Big area of affect */
		for (y = (y1 - 15); y <= (y1 + 15); y++)
		{
			for (x = (x1 - 15); x <= (x1 + 15); x++)
			{
				/* Skip illegal grids */
				if (!in_bounds(y, x)) continue;

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

					/* Access the grid */
					c_ptr = &cave[y][x];

					/* Wall (or floor) type */
					t = rand_int(200);

					/* Granite */
					if (t < 20)
					{
						/* Create granite wall */
						c_ptr->feat = FEAT_WALL_EXTRA;
					}

					/* Quartz */
					else if (t < 70)
					{
						/* Create quartz vein */
						c_ptr->feat = FEAT_QUARTZ;
					}

					/* Magma */
					else if (t < 100)
					{
						/* Create magma vein */
						c_ptr->feat = FEAT_MAGMA;
					}

					/* Floor */
					else
					{
						/* Create floor */
                                                c_ptr->feat = floor_type[rand_int(100)];
					}

					/* No longer part of a room or vault */
					c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

					/* No longer illuminated or known */
					c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW);
				}
			}
		}
	}
}

/* Function that sees if a square is a floor.  (Includes range checking.) */
static bool get_is_floor(int x,int y)
{
	if (!in_bounds(y,x))
	{
		/* Out of bounds */
		return(FALSE);	
        }
	
	/*Do the real check: */
        if (f_info[cave[y][x].feat].flags1 & FF1_FLOOR) return(TRUE);
	
	return(FALSE);
}

/* Set a square to be floor.  (Includes range checking.) */
static void set_floor(int x,int y)
{
	if (!in_bounds(y,x))
	{
		/* Out of bounds */
		return;	
	}
        if((cave[y][x].feat != d_info[dungeon_type].fill_type1 * (d_info[dungeon_type].fill_percent1?1:0)) &&
           (cave[y][x].feat != d_info[dungeon_type].fill_type2 * (d_info[dungeon_type].fill_percent2?1:0)) &&
           (cave[y][x].feat != d_info[dungeon_type].fill_type3 * (d_info[dungeon_type].fill_percent3?1:0)))
	{
		/* Not normal granite -probably a room border */
		return;
	}
	
	/*Set to be floor */
        cave[y][x].feat=floor_type[rand_int(100)];
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
	if ((count==0)||(count==2)) return;


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
 * Create up to "num" objects near the given coordinates
 * Only really called by some of the "vault" routines.
 */
static void vault_objects(int y, int x, int num)
{
	int dummy = 0;
	int i = 0, j = y, k = x;


	/* Attempt to place 'num' objects */
	for (; num > 0; --num)
	{
		/* Try up to 11 spots looking for empty space */
		for (i = 0; i < 11; ++i)
		{
			/* Pick a random location */
			while (dummy < SAFE_MAX_ATTEMPTS)
			{
				j = rand_spread(y, 2);
				k = rand_spread(x, 3);
				dummy++;
				if (!in_bounds(j, k)) continue;
				break;
			}


			if (dummy >= SAFE_MAX_ATTEMPTS)
			{
				if (cheat_room)
				{
					msg_print("Warning! Could not place vault object!");
				}
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
	int count = 0, y1 = y, x1 = x;
	int dummy = 0;

	/* Place traps */
	for (count = 0; count <= 5; count++)
	{
		/* Get a location */
		while (dummy < SAFE_MAX_ATTEMPTS)
		{
			y1 = rand_spread(y, yd);
			x1 = rand_spread(x, xd);
			dummy++;
			if (!in_bounds(y1, x1)) continue;
			break;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
				msg_print("Warning! Could not place vault trap!");
			}
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
			monster_level = dun_level + 2;
			(void)place_monster(y, x, TRUE, TRUE);
			monster_level = dun_level;
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
 * cave.feat and cave.info etc.
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
	* If so, fix by tunneling outside the room in such a way as to conect the caves.
	*/
	
	check_room_boundary(*xx-x/2-1,*yy-y/2-1,*xx+x/2+1,*yy+y/2+1);
        
	/* Success */
	return (TRUE);
}


/*
 * Room building routines.
 *
 * Room types:
 *   1 -- normal
 *   2 -- overlapping
 *   3 -- cross shaped
 *   4 -- large room with features
 *   5 -- monster nests
 *   6 -- monster pits
 *   7 -- simple vaults
 *   8 -- greater vaults
 *   9 -- circular rooms
 */

/*
 * Type 1 -- normal rectangular rooms
 */
static void build_type1(int by0, int bx0)
{
	int y, x, y2, x2, yval, xval;
	int y1, x1, xsize, ysize;

	bool light;

	cave_type *c_ptr;


	/* Choose lite or dark */
	light = (dun_level <= randint(25));


	/* Pick a room size */
	y1 =randint(4);
	x1 =randint(11);
	y2 =randint(3);
	x2 =randint(11);
	
	xsize=x1+x2+1;
	ysize=y1+y2+1;

	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(xsize+2,ysize+2,FALSE,by0,bx0,&xval,&yval)) return;

	/* Get corner values */
	y1 = yval - ysize/2;
	x1 = xval - xsize/2;
	y2 = yval + (ysize+1)/2;
	x2 = xval + (xsize+1)/2;

	/* Place a full floor under the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}

	/* Walls around the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y][x2+1];
                c_ptr->feat = feat_wall_outer;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y2+1][x];
                c_ptr->feat = feat_wall_outer;
	}


	/* Hack -- Occasional pillar room */
	if (rand_int(20) == 0)
	{
		for (y = y1; y <= y2; y += 2)
		{
			for (x = x1; x <= x2; x += 2)
			{
				c_ptr = &cave[y][x];
                                c_ptr->feat = feat_wall_inner;
			}
		}
	}

	/* Hack -- Occasional ragged-edge room */
	else if (rand_int(50) == 0)
	{
		for (y = y1 + 2; y <= y2 - 2; y += 2)
		{
			c_ptr = &cave[y][x1];
                        c_ptr->feat = feat_wall_inner;
			c_ptr = &cave[y][x2];
                        c_ptr->feat = feat_wall_inner;
		}
		for (x = x1 + 2; x <= x2 - 2; x += 2)
		{
			c_ptr = &cave[y1][x];
                        c_ptr->feat = feat_wall_inner;
			c_ptr = &cave[y2][x];
                        c_ptr->feat = feat_wall_inner;
		}
	}
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static void build_type2(int by0, int bx0)
{
        int                     y, x, yval, xval;
	int			y1a, x1a, y2a, x2a;
	int			y1b, x1b, y2b, x2b;

	bool		light;

	cave_type *c_ptr;



	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,FALSE,by0,bx0,&xval,&yval))return;

	/* Choose lite or dark */
	light = (dun_level <= randint(25));


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
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}

	/* Place a full floor for room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		for (x = x1b - 1; x <= x2b + 1; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}


	/* Place the walls around room "a" */
	for (y = y1a - 1; y <= y2a + 1; y++)
	{
		c_ptr = &cave[y][x1a-1];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y][x2a+1];
                c_ptr->feat = feat_wall_outer;
	}
	for (x = x1a - 1; x <= x2a + 1; x++)
	{
		c_ptr = &cave[y1a-1][x];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y2a+1][x];
                c_ptr->feat = feat_wall_outer;
	}

	/* Place the walls around room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		c_ptr = &cave[y][x1b-1];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y][x2b+1];
                c_ptr->feat = feat_wall_outer;
	}
	for (x = x1b - 1; x <= x2b + 1; x++)
	{
		c_ptr = &cave[y1b-1][x];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y2b+1][x];
                c_ptr->feat = feat_wall_outer;
	}



	/* Replace the floor for room "a" */
	for (y = y1a; y <= y2a; y++)
	{
		for (x = x1a; x <= x2a; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
		}
	}

	/* Replace the floor for room "b" */
	for (y = y1b; y <= y2b; y++)
	{
		for (x = x1b; x <= x2b; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
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
static void build_type3(int by0, int bx0)
{
	int			y, x, dy, dx, wy, wx;
	int			y1a, x1a, y2a, x2a;
	int			y1b, x1b, y2b, x2b;
	int			yval, xval;

	bool		light;

	cave_type *c_ptr;

	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,FALSE,by0,bx0,&xval,&yval))return;


	/* Choose lite or dark */
	light = (dun_level <= randint(25));


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
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}

	/* Place a full floor for room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		for (x = x1b - 1; x <= x2b + 1; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}


	/* Place the walls around room "a" */
	for (y = y1a - 1; y <= y2a + 1; y++)
	{
		c_ptr = &cave[y][x1a-1];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y][x2a+1];
                c_ptr->feat = feat_wall_outer;
	}
	for (x = x1a - 1; x <= x2a + 1; x++)
	{
		c_ptr = &cave[y1a-1][x];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y2a+1][x];
                c_ptr->feat = feat_wall_outer;
	}

	/* Place the walls around room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		c_ptr = &cave[y][x1b-1];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y][x2b+1];
                c_ptr->feat = feat_wall_outer;
	}
	for (x = x1b - 1; x <= x2b + 1; x++)
	{
		c_ptr = &cave[y1b-1][x];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y2b+1][x];
                c_ptr->feat = feat_wall_outer;
	}


	/* Replace the floor for room "a" */
	for (y = y1a; y <= y2a; y++)
	{
		for (x = x1a; x <= x2a; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
		}
	}

	/* Replace the floor for room "b" */
	for (y = y1b; y <= y2b; y++)
	{
		for (x = x1b; x <= x2b; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
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
					c_ptr = &cave[y][x];
                                        c_ptr->feat = feat_wall_inner;
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
				c_ptr = &cave[y][x1a];
                                c_ptr->feat = feat_wall_inner;
				c_ptr = &cave[y][x2a];
                                c_ptr->feat = feat_wall_inner;
			}
			for (x = x1a; x <= x2a; x++)
			{
				c_ptr = &cave[y1b][x];
                                c_ptr->feat = feat_wall_inner;
				c_ptr = &cave[y2b][x];
                                c_ptr->feat = feat_wall_inner;
			}

			/* Place a secret door on the inner room */
			switch (rand_int(4))
			{
				case 0: place_secret_door(y1b, xval); break;
				case 1: place_secret_door(y2b, xval); break;
				case 2: place_secret_door(yval, x1a); break;
				case 3: place_secret_door(yval, x2a); break;
			}

			/* Place a treasure in the vault */
			place_object(yval, xval, FALSE, FALSE);

			/* Let's guard the treasure well */
			vault_monsters(yval, xval, rand_int(2) + 3);

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
					if (y == yval) continue;
					c_ptr = &cave[y][x1a - 1];
                                        c_ptr->feat = feat_wall_inner;
					c_ptr = &cave[y][x2a + 1];
                                        c_ptr->feat = feat_wall_inner;
				}

				/* Pinch the north/south sides */
				for (x = x1a; x <= x2a; x++)
				{
					if (x == xval) continue;
					c_ptr = &cave[y1b - 1][x];
                                        c_ptr->feat = feat_wall_inner;
					c_ptr = &cave[y2b + 1][x];
                                        c_ptr->feat = feat_wall_inner;
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
				c_ptr = &cave[yval][xval];
                                c_ptr->feat = feat_wall_inner;
				c_ptr = &cave[y1b][xval];
                                c_ptr->feat = feat_wall_inner;
				c_ptr = &cave[y2b][xval];
                                c_ptr->feat = feat_wall_inner;
				c_ptr = &cave[yval][x1a];
                                c_ptr->feat = feat_wall_inner;
				c_ptr = &cave[yval][x2a];
                                c_ptr->feat = feat_wall_inner;
			}

			/* Occasionally put a pillar in the center */
			else if (rand_int(3) == 0)
			{
				c_ptr = &cave[yval][xval];
                                c_ptr->feat = feat_wall_inner;
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
static void build_type4(int by0, int bx0)
{
	int y, x, y1, x1;
        int y2, x2, tmp, yval, xval;

	bool light;

	cave_type *c_ptr;

	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,FALSE,by0,bx0,&xval,&yval))return;

	/* Choose lite or dark */
	light = (dun_level <= randint(25));

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
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}

	/* Outer Walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y][x2+1];
                c_ptr->feat = feat_wall_outer;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y2+1][x];
                c_ptr->feat = feat_wall_outer;
	}


	/* The inner room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* The inner walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
                c_ptr->feat = feat_wall_inner;
		c_ptr = &cave[y][x2+1];
                c_ptr->feat = feat_wall_inner;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
                c_ptr->feat = feat_wall_inner;
		c_ptr = &cave[y2+1][x];
                c_ptr->feat = feat_wall_inner;
	}


	/* Inner room variations */
	switch (randint(5))
	{
		/* Just an inner room with a monster */
		case 1:

		/* Place a secret door */
		switch (randint(4))
		{
			case 1: place_secret_door(y1 - 1, xval); break;
			case 2: place_secret_door(y2 + 1, xval); break;
			case 3: place_secret_door(yval, x1 - 1); break;
			case 4: place_secret_door(yval, x2 + 1); break;
		}

		/* Place a monster in the room */
		vault_monsters(yval, xval, 1);

		break;


		/* Treasure Vault (with a door) */
		case 2:

		/* Place a secret door */
		switch (randint(4))
		{
			case 1: place_secret_door(y1 - 1, xval); break;
			case 2: place_secret_door(y2 + 1, xval); break;
			case 3: place_secret_door(yval, x1 - 1); break;
			case 4: place_secret_door(yval, x2 + 1); break;
		}

		/* Place another inner room */
		for (y = yval - 1; y <= yval + 1; y++)
		{
			for (x = xval -  1; x <= xval + 1; x++)
			{
				if ((x == xval) && (y == yval)) continue;
				c_ptr = &cave[y][x];
                                c_ptr->feat = feat_wall_inner;
			}
		}

		/* Place a locked door on the inner room */
		switch (randint(4))
		{
			case 1: place_locked_door(yval - 1, xval); break;
			case 2: place_locked_door(yval + 1, xval); break;
			case 3: place_locked_door(yval, xval - 1); break;
			case 4: place_locked_door(yval, xval + 1); break;
		}

		/* Monsters to guard the "treasure" */
		vault_monsters(yval, xval, randint(3) + 2);

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
			case 1: place_secret_door(y1 - 1, xval); break;
			case 2: place_secret_door(y2 + 1, xval); break;
			case 3: place_secret_door(yval, x1 - 1); break;
			case 4: place_secret_door(yval, x2 + 1); break;
		}

		/* Large Inner Pillar */
		for (y = yval - 1; y <= yval + 1; y++)
		{
			for (x = xval - 1; x <= xval + 1; x++)
			{
				c_ptr = &cave[y][x];
                                c_ptr->feat = feat_wall_inner;
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
					c_ptr = &cave[y][x];
                                        c_ptr->feat = feat_wall_inner;
				}
				for (x = xval + 3 + tmp; x <= xval + 5 + tmp; x++)
				{
					c_ptr = &cave[y][x];
                                        c_ptr->feat = feat_wall_inner;
				}
			}
		}

		/* Occasionally, some Inner rooms */
		if (rand_int(3) == 0)
		{
			/* Long horizontal walls */
			for (x = xval - 5; x <= xval + 5; x++)
			{
				c_ptr = &cave[yval-1][x];
                                c_ptr->feat = feat_wall_inner;
				c_ptr = &cave[yval+1][x];
                                c_ptr->feat = feat_wall_inner;
			}

			/* Close off the left/right edges */
			c_ptr = &cave[yval][xval-5];
                        c_ptr->feat = feat_wall_inner;
			c_ptr = &cave[yval][xval+5];
                        c_ptr->feat = feat_wall_inner;

			/* Secret doors (random top/bottom) */
			place_secret_door(yval - 3 + (randint(2) * 2), xval - 3);
			place_secret_door(yval - 3 + (randint(2) * 2), xval + 3);

			/* Monsters */
			vault_monsters(yval, xval - 2, randint(2));
			vault_monsters(yval, xval + 2, randint(2));

			/* Objects */
			if (rand_int(3) == 0) place_object(yval, xval - 2, FALSE, FALSE);
			if (rand_int(3) == 0) place_object(yval, xval + 2, FALSE, FALSE);
		}

		break;


		/* Maze inside. */
		case 4:

		/* Place a secret door */
		switch (randint(4))
		{
			case 1: place_secret_door(y1 - 1, xval); break;
			case 2: place_secret_door(y2 + 1, xval); break;
			case 3: place_secret_door(yval, x1 - 1); break;
			case 4: place_secret_door(yval, x2 + 1); break;
		}

		/* Maze (really a checkerboard) */
		for (y = y1; y <= y2; y++)
		{
			for (x = x1; x <= x2; x++)
			{
				if (0x1 & (x + y))
				{
					c_ptr = &cave[y][x];
                                        c_ptr->feat = feat_wall_inner;
				}
			}
		}

		/* Monsters just love mazes. */
		vault_monsters(yval, xval - 5, randint(3));
		vault_monsters(yval, xval + 5, randint(3));

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
			c_ptr = &cave[y][xval];
                        c_ptr->feat = feat_wall_inner;
		}
		for (x = x1; x <= x2; x++)
		{
			c_ptr = &cave[yval][x];
                        c_ptr->feat = feat_wall_inner;
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
		vault_monsters(yval + 1, xval - 4, randint(4));
		vault_monsters(yval + 1, xval + 4, randint(4));
		vault_monsters(yval - 1, xval - 4, randint(4));
		vault_monsters(yval - 1, xval + 4, randint(4));

		break;
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
 * The old method made direct use of monster "names", which is bad.
 *
 * Note the use of Angband 2.7.9 monster race pictures in various places.
 */


/*
 * Helper function for "monster nest (jelly)"
 */
static bool vault_aux_jelly(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Also decline evil jellies (like death molds and shoggoths) */
	if (r_ptr->flags3 & (RF3_EVIL)) return (FALSE);

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
 * Helper function for "monster nest (chapel)"
 */
static bool vault_aux_chapel(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require "priest" or Angel */
	if (!((r_ptr->d_char == 'A') ||
		strstr((r_name + r_ptr->name),"riest")))
	{
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (kennel)"
 */
static bool vault_aux_kennel(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require a Zephyr Hound or a dog */
	return ((r_ptr->d_char == 'Z') || (r_ptr->d_char == 'C'));

}

/*
 * Helper function for "monster nest (treasure)"
 */
static bool vault_aux_treasure(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require "priest" or Angel */
	if (!((r_ptr->d_char == '!') || (r_ptr->d_char == '|') ||
		(r_ptr->d_char == '$') || (r_ptr->d_char == '?') ||
		(r_ptr->d_char == '=')))
	{
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster nest (clone)"
 */
static bool vault_aux_clone(int r_idx)
{
	return (r_idx == template_race);
}


/*
 * Helper function for "monster nest (symbol clone)"
 */
static bool vault_aux_symbol(int r_idx)
{
	return ((r_info[r_idx].d_char == (r_info[template_race].d_char))
		&& !(r_info[r_idx].flags1 & RF1_UNIQUE));
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
        int                     y, x, y1, x1, y2, x2, xval, yval;

	int			tmp, i;

	s16b		what[64];

	cave_type		*c_ptr;

	cptr		name;

	bool		empty = FALSE;

	bool (*old_get_mon_num_hook)(int r_idx);

	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,TRUE,by0,bx0,&xval,&yval))return;

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

        if (seed_dungeon) Rand_quick = FALSE;

	/* Place the floor area */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
			c_ptr->info |= (CAVE_ROOM);
		}
	}

	/* Place the outer walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y][x2+1];
                c_ptr->feat = feat_wall_outer;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y2+1][x];
                c_ptr->feat = feat_wall_outer;
	}


	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* The inner walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
                c_ptr->feat = feat_wall_inner;
		c_ptr = &cave[y][x2+1];
                c_ptr->feat = feat_wall_inner;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
                c_ptr->feat = feat_wall_inner;
		c_ptr = &cave[y2+1][x];
                c_ptr->feat = feat_wall_inner;
	}


	/* Place a secret door */
	switch (randint(4))
	{
		case 1: place_secret_door(y1 - 1, xval); break;
		case 2: place_secret_door(y2 + 1, xval); break;
		case 3: place_secret_door(yval, x1 - 1); break;
		case 4: place_secret_door(yval, x2 + 1); break;
	}


	/* Hack -- Choose a nest type */
	tmp = randint(dun_level);

	old_get_mon_num_hook = get_mon_num_hook;

	if ((tmp < 25) && (randint(2) != 1))
	{
            do  { template_race = randint(max_r_idx - 2); }
                while ((r_info[template_race].flags1 & RF1_UNIQUE)
                        || (((r_info[template_race].level) + randint(5)) >
                            (dun_level + randint(5))));
        if ((randint(2)!=1) && (dun_level >= (25 + randint(15))))
        {
            name = "symbol clone";
            get_mon_num_hook = vault_aux_symbol;
        }
        else
        {
            name = "clone";
            get_mon_num_hook = vault_aux_clone;
        }
    }
    else if (tmp < 25)
	/* Monster nest (jelly) */
	{
		/* Describe */
		name = "jelly";

		/* Restrict to jelly */
		get_mon_num_hook = vault_aux_jelly;
	}

	else if (tmp < 50)
	{
		name = "treasure";
		get_mon_num_hook = vault_aux_treasure;
	}

	/* Monster nest (animal) */
	else if (tmp < 65)
	{
		if (randint(3)==1)
		{
			name = "kennel";
			get_mon_num_hook = vault_aux_kennel;
		}
		else
		{
			/* Describe */
			name = "animal";

			/* Restrict to animal */
			get_mon_num_hook = vault_aux_animal;
		}
	}

	/* Monster nest (undead) */
	else
	{
		if (randint(3)==1)
		{
			name = "chapel";
			get_mon_num_hook = vault_aux_chapel;
		}
		else
		{
			/* Describe */
			name = "undead";

			/* Restrict to undead */
			get_mon_num_hook = vault_aux_undead;
		}
	}


	/* Prepare allocation table */
	get_mon_num_prep();


	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(dun_level + 10);

		/* Notice failure */
		if (!what[i]) empty = TRUE;
	}


	/* Remove restriction */
	get_mon_num_hook = old_get_mon_num_hook;

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Oops */
	if (empty) return;


	/* Describe */
        if ((cheat_room)||(p_ptr->precognition))
	{
		/* Room type */
		msg_format("Monster nest (%s)", name);
	}


	/* Increase the level rating */
	rating += 10;

	/* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
    if ((dun_level <= 40) && (randint(dun_level*dun_level + 50) < 300))
	{
		good_item_flag = TRUE;
	}


	/* Place some monsters */
	for (y = yval - 2; y <= yval + 2; y++)
	{
		for (x = xval - 9; x <= xval + 9; x++)
		{
			int r_idx = what[rand_int(64)];

			/* Place that "random" monster (no groups) */
            (void)place_monster_aux(y, x, r_idx, FALSE, FALSE, FALSE);
		}
	}

        if (seed_dungeon) Rand_quick = TRUE;
}



/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type organized in the room.
 *
 * Monster types in the pit  (list out of date...)
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
	int         tmp, what[16];
        int         i, j, y, x, y1, x1, y2, x2, xval, yval;
	bool        empty = FALSE;
	cave_type   *c_ptr;
	cptr        name;
	bool (*old_get_mon_num_hook)(int r_idx);

	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(25,11,TRUE,by0,bx0,&xval,&yval))return;

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

        if (seed_dungeon) Rand_quick = FALSE;

	/* Place the floor area */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			c_ptr = &cave[y][x];
                        c_ptr->feat = floor_type[rand_int(100)];
			c_ptr->info |= (CAVE_ROOM);
		}
	}

	/* Place the outer walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y][x2+1];
                c_ptr->feat = feat_wall_outer;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
                c_ptr->feat = feat_wall_outer;
		c_ptr = &cave[y2+1][x];
                c_ptr->feat = feat_wall_outer;
	}


	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* The inner walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
                c_ptr->feat = feat_wall_inner;
		c_ptr = &cave[y][x2+1];
                c_ptr->feat = feat_wall_inner;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
                c_ptr->feat = feat_wall_inner;
		c_ptr = &cave[y2+1][x];
                c_ptr->feat = feat_wall_inner;
	}


	/* Place a secret door */
	switch (randint(4))
	{
		case 1: place_secret_door(y1 - 1, xval); break;
		case 2: place_secret_door(y2 + 1, xval); break;
		case 3: place_secret_door(yval, x1 - 1); break;
		case 4: place_secret_door(yval, x2 + 1); break;
	}


	/* Choose a pit type */
	tmp = randint(dun_level);

	old_get_mon_num_hook = get_mon_num_hook;

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
	else if (tmp < 55)
	{
		/* Message */
		name = "giant";

		/* Restrict monster selection */
		get_mon_num_hook = vault_aux_giant;
	}

    else if (tmp < 70)
    {
        if (randint(4)!=1)
        {
            /* Message */
            name = "ordered clones";

                do  { template_race = randint(max_r_idx - 2); }
                    while ((r_info[template_race].flags1 & RF1_UNIQUE)
                            || (((r_info[template_race].level) + randint(5)) >
                                (dun_level + randint(5))));

            /* Restrict selection */
            get_mon_num_hook = vault_aux_symbol;
        }
        else
        {

            name = "ordered chapel";
            get_mon_num_hook = vault_aux_chapel;
        }

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
		what[i] = get_mon_num(dun_level + 10);

		/* Notice failure */
		if (!what[i]) empty = TRUE;
	}


	/* Remove restriction */
	get_mon_num_hook = old_get_mon_num_hook;

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Oops */
	if (empty) return;


	/* XXX XXX XXX */
	/* Sort the entries */
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
        if ((cheat_room)||(p_ptr->precognition))
	{
		/* Room type */
		msg_format("Monster pit (%s)", name);

        if ((cheat_hear)||(p_ptr->precognition))
        {
            /* Contents */
            for (i = 0; i < 8; i++)
            {
                /* Message */
                msg_print(r_name + r_info[what[i]].name);
            }
        }
	}


	/* Increase the level rating */
	rating += 10;

	/* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
	if ((dun_level <= 40) && (randint(dun_level*dun_level + 50) < 300))
	{
		good_item_flag = TRUE;
	}


	/* Top and bottom rows */
	for (x = xval - 9; x <= xval + 9; x++)
	{
		place_monster_aux(yval - 2, x, what[0], FALSE, FALSE, FALSE);
		place_monster_aux(yval + 2, x, what[0], FALSE, FALSE, FALSE);
	}

	/* Middle columns */
	for (y = yval - 1; y <= yval + 1; y++)
	{
		place_monster_aux(y, xval - 9, what[0], FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 9, what[0], FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 8, what[1], FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 8, what[1], FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 7, what[1], FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 7, what[1], FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 6, what[2], FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 6, what[2], FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 5, what[2], FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 5, what[2], FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 4, what[3], FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 4, what[3], FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 3, what[3], FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 3, what[3], FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 2, what[4], FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 2, what[4], FALSE, FALSE, FALSE);
	}

	/* Above/Below the center monster */
	for (x = xval - 1; x <= xval + 1; x++)
	{
		place_monster_aux(yval + 1, x, what[5], FALSE, FALSE, FALSE);
		place_monster_aux(yval - 1, x, what[5], FALSE, FALSE, FALSE);
	}

	/* Next to the center monster */
	place_monster_aux(yval, xval + 1, what[6], FALSE, FALSE, FALSE);
	place_monster_aux(yval, xval - 1, what[6], FALSE, FALSE, FALSE);

	/* Center monster */
	place_monster_aux(yval, xval, what[7], FALSE, FALSE, FALSE);

        if (seed_dungeon) {
          Rand_quick = TRUE;
        }
}



/*
 * Hack -- fill in "vault" rooms
 */
static void build_vault(int yval, int xval, int ymax, int xmax, cptr data)
{
        int dx, dy, x, y, bwy[8], bwx[8], i;
	
	cptr t;
	
	cave_type *c_ptr;

        /* Vaults are different even in persistent dungeons. */

        if (seed_dungeon) {
            Rand_quick = FALSE;
        }

        /* Clean the between gates arrays */
        for(i = 0; i < 8; i++)
        {
                bwy[i] = bwx[i] = 9999;
        }
	
	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the location */
			x = xval - (xmax / 2) + dx;
			y = yval - (ymax / 2) + dy;
			
			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;
			
			/* Access the grid */
			c_ptr = &cave[y][x];
			
			/* Lay down a floor */
                        c_ptr->feat = floor_type[rand_int(100)];
			
			/* Part of a vault */
			c_ptr->info |= (CAVE_ROOM | CAVE_ICKY);
			
			/* Analyze the grid */
			switch (*t)
			{
				/* Granite wall (outer) */
			case '%':
                                c_ptr->feat = feat_wall_outer;
				break;
				
				/* Granite wall (inner) */
			case '#':
				c_ptr->feat = FEAT_WALL_INNER;
				break;
				
				/* Permanent wall (inner) */
			case 'X':
				c_ptr->feat = FEAT_PERM_INNER;
				break;
				
				/* Treasure/trap */
			case '*':
				if (rand_int(100) < 75)
				{
					place_object(y, x, FALSE, FALSE);
				}
				else
				{
					place_trap(y, x);
				}
				break;
				
				/* Secret doors */
			case '+':
				place_secret_door(y, x);
				break;
				
				/* Trap */
			case '^':
				place_trap(y, x);
				break;

                                /* Glass wall */
                        case 'G':
                                c_ptr->feat = FEAT_GLASS_WALL;
				break;

                                /* Illusion wall */
                        case 'I':
                                c_ptr->feat = FEAT_ILLUS_WALL;
				break;
			}
		}
	}
	
	
	/* Place dungeon monsters and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the grid */
			x = xval - (xmax/2) + dx;
			y = yval - (ymax/2) + dy;
			
			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;
			
			/* Access the grid */
			c_ptr = &cave[y][x];
			
			/* Analyze the symbol */
			switch (*t)
			{
				/* Monster */
			case '&':
				{
					monster_level = dun_level + 5;
					place_monster(y, x, TRUE, TRUE);
					monster_level = dun_level;
					break;
				}

				/* Meaner monster */
			case '@':
				{
					monster_level = dun_level + 11;
					place_monster(y, x, TRUE, TRUE);
					monster_level = dun_level;
					break;
				}

				/* Meaner monster, plus treasure */
			case '9':
				{
					monster_level = dun_level + 9;
					place_monster(y, x, TRUE, TRUE);
					monster_level = dun_level;
					object_level = dun_level + 7;
					place_object(y, x, TRUE, FALSE);
					object_level = dun_level;
					break;
				}

				/* Nasty monster and treasure */
			case '8':
				{
					monster_level = dun_level + 40;
					place_monster(y, x, TRUE, TRUE);
					monster_level = dun_level;
					object_level = dun_level + 20;
					place_object(y, x, TRUE, TRUE);
					object_level = dun_level;
					break;
				}

				/* Monster and/or object */
			case ',':
				{
					if (rand_int(100) < 50)
					{
						monster_level = dun_level + 3;
						place_monster(y, x, TRUE, TRUE);
						monster_level = dun_level;
					}
					if (rand_int(100) < 50)
					{
						object_level = dun_level + 7;
						place_object(y, x, FALSE, FALSE);
						object_level = dun_level;
					}
					break;
				}

			case 'p':
                cave_set_feat(y, x, FEAT_PATTERN_START);
                break;

			case 'a':
                cave_set_feat(y, x, FEAT_PATTERN_1);
                break;

			case 'b':
                cave_set_feat(y, x, FEAT_PATTERN_2);
                break;

			case 'c':
                cave_set_feat(y, x, FEAT_PATTERN_3);
                break;

			case 'd':
                cave_set_feat(y, x, FEAT_PATTERN_4);
                break;

			case 'P':
                cave_set_feat(y, x, FEAT_PATTERN_END);
                break;

			case 'B':
                cave_set_feat(y, x, FEAT_PATTERN_XTRA1);
                break;

			case 'A':
                {
                    object_level = dun_level + 12;
					place_object(y, x, TRUE, FALSE);
					object_level = dun_level;
                }
                break;


                        /* Between gates */
                        case '0':
                        case '1':
                        case '2':
                        case '3':
                        case '4':
                        case '5':
                        case '6':
                        case '7':
                                /* Not found before */
                                if(bwy[*t - '0'] == 9999)
                                {
                                        cave_set_feat(y, x, FEAT_BETWEEN);
                                        bwy[*t - '0'] = y;
                                        bwx[*t - '0'] = x;
                                }
                                /* The second time */
                                else
                                {
                                        cave_set_feat(y, x, FEAT_BETWEEN);
                                        c_ptr->special = bwx[*t - '0'] + (bwy[*t - '0'] << 8);
                                        cave[bwy[*t - '0']][bwx[*t - '0']].special = x + (y << 8);
                                }
                                break;
			}
		}
	}

  if (seed_dungeon) {
    Rand_quick = TRUE;
  }
}


/*
 * Type 7 -- simple vaults (see "v_info.txt")
 */
static void build_type7(int by0, int bx0)
{
	vault_type	*v_ptr;
        int dummy = 0, xval, yval;

	/* Pick a lesser vault */
	while (dummy < SAFE_MAX_ATTEMPTS)
	{
		dummy++;

		/* Access a random vault record */
		v_ptr = &v_info[rand_int(max_v_idx)];

		/* Accept the first lesser vault */
		if (v_ptr->typ == 7) break;
	}

        /* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(v_ptr->wid,v_ptr->hgt,FALSE,by0,bx0,&xval,&yval)) 
	{
           if (cheat_room) msg_print("Could not allocate this vault here");
	   return;
	}

	if (dummy >= SAFE_MAX_ATTEMPTS)
	{
		if (cheat_room)
		{
			msg_print("Warning! Could not place lesser vault!");
		}
		return;
	}
    

#ifdef FORCE_V_IDX
        v_ptr = &v_info[FORCE_V_IDX];
#endif

	/* Message */
        if ((cheat_room)||(p_ptr->precognition)) msg_print("Lesser Vault");

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((dun_level <= 50) ||
		(randint((dun_level-40) * (dun_level-40) + 50) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
}



/*
 * Type 8 -- greater vaults (see "v_info.txt")
 */
static void build_type8(int by0, int bx0)
{
	vault_type	*v_ptr;
        int dummy = 0, xval, yval;

	/* Pick a lesser vault */
	while (dummy < SAFE_MAX_ATTEMPTS)
	{
		dummy++;

		/* Access a random vault record */
		v_ptr = &v_info[rand_int(max_v_idx)];

		/* Accept the first greater vault */
		if (v_ptr->typ == 8) break;
	}

	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(v_ptr->wid,v_ptr->hgt,FALSE,by0,bx0,&xval,&yval)) 
	{
           if (cheat_room) msg_print("Could not allocate this vault here");
	   return;
	}

	if (dummy >= SAFE_MAX_ATTEMPTS)
	{
		if (cheat_room)
		{
			msg_print("Warning! Could not place greater vault!");
		}
		return;
	}


#ifdef FORCE_V_IDX
        v_ptr = &v_info[FORCE_V_IDX];
#endif

	/* Message */
        if ((cheat_room)||(p_ptr->precognition)) msg_print("Greater Vault");

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((dun_level <= 50) ||
        (randint((dun_level-40) * (dun_level-40) + 50) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
}


/*
 * DAG:
 * Build an vertical oval room.
 * For every grid in the possible square, check the distance.
 * If it's less than or == than the radius, make it a room square.
 * If its less, make it a normal grid. If it's == make it an outer
 * wall.
 */
static void build_type9(int by0, int bx0)
{
        int rad, x, y, x0, y0;

	int light = FALSE;

	/* Occasional light */
	if (randint(dun_level) <= 5) light = TRUE;

	rad = rand_int(10);

	/*Allocate in room_map.  If will not fit, exit*/
	if (room_alloc(rad*2+1,rad*2+1,FALSE,by0,bx0,&x0,&y0)==FALSE) return;
	
	for (x = x0 - rad; x <= x0 + rad; x++)
	{
		for (y = y0 - rad; y <= y0 + rad; y++)
		{
			if(distance(y0, x0, y, x) == rad)
			{
				cave[y][x].info |= (CAVE_ROOM);
				if (light) cave[y][x].info |= (CAVE_GLOW);

                                cave[y][x].feat = feat_wall_outer;
			}

			if(distance(y0, x0, y, x) < rad)
			{
				cave[y][x].info |= (CAVE_ROOM);
				if (light) cave[y][x].info |= (CAVE_GLOW);

                                cave[y][x].feat = floor_type[rand_int(100)];
			}
		}
	}
}

/* Store routine for the fractal cave generator */ 
/* this routine probably should be an inline function or a macro. */
static void store_height(int x,int y,int x0,int y0,byte val,int xhsize,int yhsize,int cutoff)
{
	/* only write to points that are "blank" */
	if (cave[y0-yhsize+y][x0-xhsize+x].feat!=255) return;
	
	 /* if on boundary set val>cutoff so walls are not as square */
	if (((x==0)||(y==0)||(x==xhsize*2)||(y==yhsize*2))&&(val<=cutoff)) val=cutoff+1;

	/* store the value in height-map format */
	cave[y0-yhsize+y][x0-xhsize+x].feat=val; 

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
*  Note that this uses the cave.feat array in a very hackish way
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
			cave[(int)(y0-yhsize+j)][(int)(x0-xhsize+i)].feat=255; 
			/* Clear icky flag because may be redoing the cave */
			cave[(int)(y0-yhsize+j)][(int)(x0-xhsize+i)].info&= ~(CAVE_ICKY); 
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
					(cave[y0-yhsize+j/256][x0-xhsize+(i-xhstep)/256].feat
					+cave[y0-yhsize+j/256][x0-xhsize+(i+xhstep)/256].feat)/2
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
					(cave[y0-yhsize+(j-yhstep)/256][x0-xhsize+i/256].feat
					+cave[y0-yhsize+(j-yhstep)/256][x0-xhsize+i/256].feat)/2
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
				(cave[y0-yhsize+(j-yhstep)/256][x0-xhsize+(i-xhstep)/256].feat
				+cave[y0-yhsize+(j+yhstep)/256][x0-xhsize+(i-xhstep)/256].feat
				+cave[y0-yhsize+(j-yhstep)/256][x0-xhsize+(i+xhstep)/256].feat
				+cave[y0-yhsize+(j+yhstep)/256][x0-xhsize+(i+xhstep)/256].feat)/4
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
	if (cave[y][x].info&CAVE_ICKY) 
	{
		/* already done */
		return FALSE;	
	}
	else
	{
		/* Show that have looked at this square */
		cave[y][x].info|= (CAVE_ICKY); 
		/*if less than cutoff then is a floor */
		if(cave[y][x].feat<=cutoff) 
		{
                        cave[y][x].feat=floor_type[rand_int(100)]; 
			return TRUE;
		}
		/*if greater than cutoff then is a wall */
		else
		{
                        cave[y][x].feat=feat_wall_outer; 
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
				cave[y0+y+j-ysize/2][x0+x+i-xsize/2].info|=CAVE_ICKY;
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
	
        /*
         * Select region connected to center of cave system
         * this gets rid of alot of isolated one-sqaures that
         * can make teleport traps instadeaths...
         */
	fill_hack(y0,x0,yhsize,xhsize,xsize,ysize,cutoff,&amount);
	
	/* if tally too small, try again*/
	if (amount<10) 
	{
		/*too small -clear area and try again later*/
		for(x=0;x<=xsize;++x)
		{
			for(y=0;y<ysize;++y)
			{
                                cave[y0+y-yhsize][x0+x-xhsize].feat=fill_type[rand_int(100)];
				cave[y0+y-yhsize][x0+x-xhsize].info&= ~(CAVE_ICKY|CAVE_ROOM);
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
			if (cave[0+y0-yhsize][i+x0-xhsize].info&CAVE_ICKY)			
			{
				/*Next to a 'filled' region?-set to be room walls */
                                cave[y0+0-yhsize][x0+i-xhsize].feat=feat_wall_outer;
				if (light) cave[y0+0-yhsize][x0+i-xhsize].info|=(CAVE_GLOW);
				if (room) 
				{
					cave[y0+0-yhsize][x0+i-xhsize].info|=(CAVE_ROOM);
                                        cave[y0+0-yhsize][x0+i-xhsize].feat=feat_wall_outer;
				}
				else
				{
                                        cave[y0+0-yhsize][x0+i-xhsize].feat=fill_type[rand_int(100)];
				}
			}
                        else
                        {
                                /*set to be normal granite*/
                                cave[y0+0-yhsize][x0+i-xhsize].feat=fill_type[rand_int(100)];
                        }
                        
                        
			/* bottom boundary */
			if (cave[ysize+y0-yhsize][i+x0-xhsize].info&CAVE_ICKY)
			{
				/*Next to a 'filled' region?-set to be room walls */
                                cave[y0+ysize-yhsize][x0+i-xhsize].feat=feat_wall_outer;
				if (light) cave[y0+ysize-yhsize][x0+i-xhsize].info|=(CAVE_GLOW);
				if (room)
				{
					cave[y0+ysize-yhsize][x0+i-xhsize].info|=(CAVE_ROOM);
                                        cave[y0+ysize-yhsize][x0+i-xhsize].feat=feat_wall_outer;
				}
				else
				{
                                        cave[y0+ysize-yhsize][x0+i-xhsize].feat=fill_type[rand_int(100)];
				}      
                         }
                         else
                         {
                                /*set to be normal granite*/
                                cave[y0+ysize-yhsize][x0+i-xhsize].feat=fill_type[rand_int(100)];
                         }                               
		
		/* clear the icky flag-don't need it any more */
		
		cave[y0+0-yhsize][x0+i-xhsize].info&= ~(CAVE_ICKY);
		cave[y0+ysize-yhsize][x0+i-xhsize].info&= ~(CAVE_ICKY);
		}

	/* Do the left and right boundaries minus the corners (done above) */

	for(i=1;i<ysize;++i)
	{
		/* left boundary */
		if (cave[i+y0-yhsize][0+x0-xhsize].info&CAVE_ICKY)
		{
			/* room boundary */
                        cave[y0+i-yhsize][x0+0-xhsize].feat=feat_wall_outer;
			if (light) cave[y0+i-yhsize][x0+0-xhsize].info|=(CAVE_GLOW);
			if (room)
			{
				cave[y0+i-yhsize][x0+0-xhsize].info|=(CAVE_ROOM);
                                cave[y0+i-yhsize][x0+0-xhsize].feat=feat_wall_outer;
			}
			else
			{
                                cave[y0+i-yhsize][x0+0-xhsize].feat=fill_type[rand_int(100)];
			}
                }
                else 
                {
                        /* outside room */
                        cave[y0+i-yhsize][x0+0-xhsize].feat=fill_type[rand_int(100)];
                }
		/* right boundary */
		if (cave[i+y0-yhsize][xsize+x0-xhsize].info&CAVE_ICKY)
		{
			/* room boundary */
                        cave[y0+i-yhsize][x0+xsize-xhsize].feat=feat_wall_outer;
			if (light) cave[y0+i-yhsize][x0+xsize-xhsize].info|=(CAVE_GLOW);
			if (room)
			{
		        	cave[y0+i-yhsize][x0+xsize-xhsize].info|=(CAVE_ROOM);
                                cave[y0+i-yhsize][x0+xsize-xhsize].feat=feat_wall_outer;
			}
			else
			{
                                cave[y0+i-yhsize][x0+xsize-xhsize].feat=fill_type[rand_int(100)];
			}
                }
                else
                {
                        /* outside room */
                        cave[y0+i-yhsize][x0+xsize-xhsize].feat=fill_type[rand_int(100)];
                }
		
		/* clear icky flag -done with it */
		cave[y0+i-yhsize][x0+0-xhsize].info&= ~(CAVE_ICKY);
		cave[y0+i-yhsize][x0+xsize-xhsize].info&= ~(CAVE_ICKY);
	}


	/* Do the rest: convert back to the normal format*
	* In other variants, may want to check to see if cave.feat< some value
	* if so, set to be water:- this will make interesting pools etc.
	* (I don't do this for standard angband.)
	*/
	for(x=1;x<xsize;++x)
	{	
		for(y=1;y<ysize;++y)
		{			
                        if ((f_info[cave[y0+y-yhsize][x0+x-xhsize].feat].flags1 & FF1_FLOOR)
				&&(cave[y0+y-yhsize][x0+x-xhsize].info&CAVE_ICKY))
				
			{
				/*Clear the icky flag in the filled region*/
				cave[y0+y-yhsize][x0+x-xhsize].info&= ~CAVE_ICKY;
				/*Set appropriate flags */				
				if (light) cave[y0+y-yhsize][x0+x-xhsize].info|=(CAVE_GLOW);
				if (room) cave[y0+y-yhsize][x0+x-xhsize].info|=(CAVE_ROOM);		
			}
                        else if ((cave[y0+y-yhsize][x0+x-xhsize].feat==feat_wall_outer)
				&&(cave[y0+y-yhsize][x0+x-xhsize].info&CAVE_ICKY))
			{
				/*Walls */
				cave[y0+y-yhsize][x0+x-xhsize].info&= ~(CAVE_ICKY);
				if (light) cave[y0+y-yhsize][x0+x-xhsize].info|=(CAVE_GLOW);
				if (room)
				{ 
					cave[y0+y-yhsize][x0+x-xhsize].info|=(CAVE_ROOM);
				}
				else
				{
                                        cave[y0+y-yhsize][x0+x-xhsize].feat=fill_type[rand_int(100)];
				}
                        }
                        else 
                        {
                                /*Clear the unconnected regions*/
                                cave[y0+y-yhsize][x0+x-xhsize].feat=fill_type[rand_int(100)];
                                cave[y0+y-yhsize][x0+x-xhsize].info&= ~(CAVE_ICKY|CAVE_ROOM);
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

/*
 * Makes a cave system in the center of the dungeon
 */
static void build_cavern(void)
{
	int grd,roug,cutoff,xsize,ysize,x0,y0;
	bool done,light,room;
	
	light=done=room=FALSE;
	if (dun_level <= randint(25)) light = TRUE;
	
	/* Make a cave the size of the dungeon */
	xsize=cur_wid-1;
	ysize=cur_hgt-1;
	x0=xsize/2;
	y0=ysize/2;

	/* Paranoia: make size even */
	xsize=x0*2;
	ysize=y0*2; 

	while (!done)
	{
		/* testing values for these parameters: feel free to adjust*/
		grd=2^(randint(4)+4);
		
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

/*
 * Driver routine to create fractal cave system
 */
static void build_type10(int by0, int bx0)
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
	
	if (dun_level <= randint(25)) light = TRUE;
	
	while (!done)
	{
		/* Note: size must be even or there are rounding problems 
		* This causes the tunnels not to connect properly to the room */	 
		
		/* testing values for these parameters feel free to adjust*/
		grd=2^(randint(4)); 
		
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
static void build_tunnel(int row1, int col1, int row2, int col2, bool water)
{
	int i, y, x;
	int tmp_row, tmp_col;
	int row_dir, col_dir;
	int start_row, start_col;
	int main_loop_count = 0;

	bool door_flag = FALSE;

	cave_type *c_ptr;


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


		/* Extremely Important -- do not leave the dungeon */
		while (!in_bounds(tmp_row, tmp_col))
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


		/* Access the location */
		c_ptr = &cave[tmp_row][tmp_col];


		/* Avoid the edge of the dungeon */
		if (c_ptr->feat == FEAT_PERM_SOLID) continue;

		/* Avoid the edge of vaults */
		if (c_ptr->feat == FEAT_PERM_OUTER) continue;

		/* Avoid "solid" granite walls */
		if (c_ptr->feat == FEAT_WALL_SOLID) continue;

		/* Pierce "outer" walls of rooms */
                if (c_ptr->feat == feat_wall_outer)
		{
			/* Acquire the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Hack -- Avoid outer/solid permanent walls */
			if (cave[y][x].feat == FEAT_PERM_SOLID) continue;
			if (cave[y][x].feat == FEAT_PERM_OUTER) continue;

			/* Hack -- Avoid outer/solid granite walls */
                        if (cave[y][x].feat == feat_wall_outer) continue;
			if (cave[y][x].feat == FEAT_WALL_SOLID) continue;

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
                                        if ((cave[y][x].feat == feat_wall_outer) && !(d_info[dungeon_type].flags1 & DF1_NO_DOORS))
					{
                                                int randwallqm;
						/* Change the wall to a "solid" wall */
                                                randwallqm = rand_int(4);
                                                if (randwallqm == 2) cave[y][x].feat = FEAT_QUARTZ;
                                                else if (randwallqm == 3) cave[y][x].feat = FEAT_MAGMA;
                                                else cave[y][x].feat = FEAT_WALL_SOLID;
					}
				}
			}
		}

		/* Travel quickly through rooms */
		else if (c_ptr->info & (CAVE_ROOM))
		{
			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;
		}

		/* Tunnel through all other walls */
                else if ((c_ptr->feat >= FEAT_WALL_EXTRA || c_ptr->feat == FEAT_QUARTZ || c_ptr->feat == FEAT_MAGMA) && (c_ptr->feat != FEAT_GRASS) && (c_ptr->feat != FEAT_DIRT))
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

		/* Access the grid */
		c_ptr = &cave[y][x];

		/* Clear previous contents, add a floor */
                if(!water)
                {
                        c_ptr->feat = floor_type[rand_int(100)];
                }
                else c_ptr->feat = FEAT_SHAL_WATER;
	}


	/* Apply the piercings that we found */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Access the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* Access the grid */
		c_ptr = &cave[y][x];

		/* Clear previous contents, add up floor */
                c_ptr->feat = floor_type[rand_int(100)];

		/* Occasional doorway */
                if ((rand_int(100) < DUN_TUN_PEN) && !(d_info[dungeon_type].flags1 & DF1_NO_DOORS))
		{
			/* Place a random door */
			place_random_door(y, x);
		}
	}
}




/*
 * Count the number of "corridor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(y1, x1)"
 *
 * XXX XXX This routine currently only counts actual "empty floor"
 * grids which are not in rooms.  We might want to also count stairs,
 * open doors, closed doors, etc.
 */
static int next_to_corr(int y1, int x1)
{
	int i, y, x, k = 0;

	cave_type *c_ptr;

	/* Scan adjacent grids */
	for (i = 0; i < 4; i++)
	{
		/* Extract the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Skip non floors */
		if (!cave_floor_bold(y, x)) continue;

		/* Access the grid */
		c_ptr = &cave[y][x];

		/* Skip non "empty floor" grids */
                if ((c_ptr->feat != d_info[dungeon_type].floor1) &&
                    (c_ptr->feat != d_info[dungeon_type].floor2) &&
                    (c_ptr->feat != d_info[dungeon_type].floor3))
                        continue;

		/* Skip grids inside rooms */
		if (c_ptr->info & (CAVE_ROOM)) continue;

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
 * Assumes "in_bounds(y,x)"
 */
static bool possible_doorway(int y, int x)
{
	/* Count the adjacent corridors */
	if (next_to_corr(y, x) >= 2)
	{
		/* Check Vertical */
                if ((f_info[cave[y-1][x].feat].flags1 & FF1_WALL) &&
                    (f_info[cave[y+1][x].feat].flags1 & FF1_WALL))
		{
			return (TRUE);
		}

		/* Check Horizontal */
                if ((f_info[cave[y][x-1].feat].flags1 & FF1_WALL) &&
                    (f_info[cave[y][x+1].feat].flags1 & FF1_WALL))
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
        if (f_info[cave[y][x].feat].flags1 & FF1_WALL) return;

	/* Ignore room grids */
	if (cave[y][x].info & (CAVE_ROOM)) return;

	/* Occasional door (if allowed) */
        if ((rand_int(100) < DUN_TUN_JCT) && possible_doorway(y, x) && !(d_info[dungeon_type].flags1 & DF1_NO_DOORS))
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
static bool room_build(int y, int x, int typ)
{
	/* Restrict level */
	if ((dun_level < roomdep[typ]) && !ironman_rooms) return (FALSE);

	/* Restrict "crowded" rooms */
	if (dun->crowded && ((typ == 5) || (typ == 6))) return (FALSE);

	/* Build a room */
	switch (typ)
	{
		/* Build an appropriate room */
                case 10: build_type10(y, x); break;
		case  9: build_type9 (y, x); break;
		case  8: build_type8 (y, x); break;
		case  7: build_type7 (y, x); break;
		case  6: build_type6 (y, x); break;
		case  5: build_type5 (y, x); break;
		case  4: build_type4 (y, x); break;
		case  3: build_type3 (y, x); break;
		case  2: build_type2 (y, x); break;
		case  1: build_type1 (y, x); break;

		/* Paranoia */
		default: return (FALSE);
	}

	/* Success */
	return (TRUE);
}




/*
 * Helper for plasma generation.
 */
static void perturb_point_mid(int x1, int x2, int x3, int x4, 
			  int xmid, int ymid, int rough, int depth_max)
{
	/*
	 * Average the four corners & perturb it a bit.
	 * tmp is a random int +/- rough
	 */
	int tmp2 = rough*2 + 1;
	int tmp = randint(tmp2) - (rough + 1);

	int avg = ((x1 + x2 + x3 + x4) / 4) + tmp;

	/* Division always rounds down, so we round up again */
	if (((x1 + x2 + x3 + x4) % 4) > 1)
		avg++;

	/* Normalize */
	if (avg < 0) avg = 0;
	if (avg > depth_max) avg = depth_max;

	/* Set the new value. */
	cave[ymid][xmid].feat = avg;
}


static void perturb_point_end(int x1, int x2, int x3, 
			  int xmid, int ymid, int rough, int depth_max)
{
	/*
	 * Average the three corners & perturb it a bit.
	 * tmp is a random int +/- rough
	 */
	int tmp2 = rough*2 + 1;
	int tmp = randint(tmp2) - (rough + 1);

	int avg = ((x1 + x2 + x3) / 3) + tmp;

	/* Division always rounds down, so we round up again */
	if ((x1 + x2 + x3) % 3) avg++;

	/* Normalize */
	if (avg < 0) avg = 0;
	if (avg > depth_max) avg = depth_max;

	/* Set the new value. */
	cave[ymid][xmid].feat = avg;
}


/*
 * A generic function to generate the plasma fractal.
 * Note that it uses ``cave_feat'' as temporary storage.
 * The values in ``cave_feat'' after this function
 * are NOT actual features; They are raw heights which
 * need to be converted to features.
 */
static void plasma_recursive(int x1, int y1, int x2, int y2, 
			     int depth_max, int rough)
{
	/* Find middle */
	int xmid = (x2-x1)/2 + x1;
	int ymid = (y2-y1)/2 + y1;
	
	/* Are we done? */
	if (x1+1 == x2) return;
	
	perturb_point_mid(cave[y1][x1].feat, cave[y2][x1].feat, cave[y1][x2].feat,
		cave[y2][x2].feat, xmid, ymid, rough, depth_max);
	
	perturb_point_end(cave[y1][x1].feat, cave[y1][x2].feat,cave[ymid][xmid].feat,
		xmid, y1, rough, depth_max);
	
	perturb_point_end(cave[y1][x2].feat, cave[y2][x2].feat,cave[ymid][xmid].feat,
		x2, ymid, rough, depth_max);
	
	perturb_point_end(cave[y2][x2].feat, cave[y2][x1].feat,cave[ymid][xmid].feat,
		xmid, y2, rough, depth_max);
	
	perturb_point_end(cave[y2][x1].feat, cave[y1][x1].feat,cave[ymid][xmid].feat,
		x1, ymid, rough, depth_max);
	
	
	/* Recurse the four quadrants */
	plasma_recursive(x1, y1, xmid, ymid, depth_max, rough);
	plasma_recursive(xmid, y1, x2, ymid, depth_max, rough);
	plasma_recursive(x1, ymid, xmid, y2, depth_max, rough);
	plasma_recursive(xmid, ymid, x2, y2, depth_max, rough);
}


/*
 * Load a town or generate a terrain level using "plasma" fractals.
 *
 * x and y are the coordinates of the area in the wilderness.
 * Border and corner are optimization flags to speed up the
 * generation of the fractal terrain.
 * If border is set then only the border of the terrain should
 * be generated (for initializing the border structure).
 * If corner is set then only the corners of the area are needed.
 *
 * Return the number of floor grids
 */
static int generate_area(int y, int x, bool border, bool corner, bool refresh)
{
        int road, entrance;
	int x1, y1;
        int hack_floor = 0;

	/* Number of the town (if any) */
        /* p_ptr->town_num = wf_info[wild_map[y][x].feat].entrance; */
        /* if (!p_ptr->town_num) p_ptr->town_num = wild_map[y][x].entrance; */

        if ((p_ptr->town_num > 0) && (p_ptr->town_num < 1000))
	{
		/* Create the town */
		int xstart = 0;
		int ystart = 0;

		/* Reset the buildings */
		init_buildings();

		/* Initialize the town */
		init_flags = INIT_CREATE_DUNGEON;
                process_dungeon_file("t_info.txt", &ystart, &xstart, cur_hgt, cur_wid);
  	}
	else
	{
		int roughness = 1; /* The roughness of the level. */
		int terrain[3][3]; /* The terrain around the current area */
                int ym, xm, yp, xp;

                /* Place the player at the center */
                if(!p_ptr->oldpx) p_ptr->oldpx = MAX_WID / 2;
                if(!p_ptr->oldpy) p_ptr->oldpy = MAX_HGT / 2;

                /* Reset the town flag */
                p_ptr->town_num = 0;

		/* Initialize the terrain array */
                ym = ((y - 1) < 0)?0:(y - 1);
                xm = ((x - 1) < 0)?0:(x - 1);
                yp = ((y + 1) >= max_wild_y)?(max_wild_y - 1):(y + 1);
                xp = ((x + 1) >= max_wild_x)?(max_wild_x - 1):(x + 1);
                terrain[0][0] = wild_map[ym][xm].feat;
                terrain[0][1] = wild_map[ym][x].feat;
                terrain[0][2] = wild_map[ym][xp].feat;
                terrain[1][0] = wild_map[y][xm].feat;
                terrain[1][1] = wild_map[y][x].feat;
                terrain[1][2] = wild_map[y][xp].feat;
                terrain[2][0] = wild_map[yp][xm].feat;
                terrain[2][1] = wild_map[yp][x].feat;
                terrain[2][2] = wild_map[yp][xp].feat;

		/* Hack -- Use the "simple" RNG */
		Rand_quick = TRUE;

		/* Hack -- Induce consistant town layout */
                Rand_value = wild_map[y][x].seed;

		if (!corner)
		{
			/* Create level background */
			for (y1 = 0; y1 < MAX_HGT; y1++)
			{
				for (x1 = 0; x1 < MAX_WID; x1++)
				{
                                        cave[y1][x1].feat = MAX_WILD_TERRAIN / 2;
				}
			}
		}

		/*
		 * Initialize the four corners
		 * ToDo: calculate the medium height of the adjacent
		 * terrains for every corner.
		 */
                cave[1][1].feat = (byte)rand_int(MAX_WILD_TERRAIN);
                cave[MAX_HGT-2][1].feat = (byte)rand_int(MAX_WILD_TERRAIN);
                cave[1][MAX_WID-2].feat = (byte)rand_int(MAX_WILD_TERRAIN);
                cave[MAX_HGT-2][MAX_WID-2].feat = (byte)rand_int(MAX_WILD_TERRAIN);

		if (!corner)
		{
			/* x1, y1, x2, y2, num_depths, roughness */
                        plasma_recursive(1, 1, MAX_WID-2, MAX_HGT-2, MAX_WILD_TERRAIN-1, roughness);
		}

		/* Use the complex RNG */
		Rand_quick = FALSE;

		for (y1 = 1; y1 < MAX_HGT-1; y1++)
		{
			for (x1 = 1; x1 < MAX_WID-1; x1++)
			{
                                cave[y1][x1].feat = wf_info[terrain[1][1]].terrain[cave[y1][x1].feat];
			}
		}

	}

	if (!corner)
	{
		/*
		 * Place roads in the wilderness
		 * ToDo: make the road a bit more interresting
		 */
                road = wf_info[wild_map[y][x].feat].road;

		if (road & ROAD_NORTH)
		{
			/* North road */
			for (y1 = 1; y1 < MAX_HGT/2; y1++)
			{
				x1 = MAX_WID/2;
				cave[y1][x1].feat = FEAT_FLOOR;
			}
		}

		if (road & ROAD_SOUTH)
		{
			/* North road */
			for (y1 = MAX_HGT/2; y1 < MAX_HGT - 1; y1++)
			{
				x1 = MAX_WID/2;
				cave[y1][x1].feat = FEAT_FLOOR;
			}
		}

		if (road & ROAD_EAST)
		{
			/* East road */
			for (x1 = MAX_WID/2; x1 < MAX_WID - 1; x1++)
			{
				y1 = MAX_HGT/2;
				cave[y1][x1].feat = FEAT_FLOOR;
			}
		}

		if (road & ROAD_WEST)
		{
			/* West road */
			for (x1 = 1; x1 < MAX_WID/2; x1++)
			{
				y1 = MAX_HGT/2;
				cave[y1][x1].feat = FEAT_FLOOR;
			}
		}
	}

#if 0
		/* Hack -- Use the "simple" RNG */
		Rand_quick = TRUE;

		/* Hack -- Induce consistant town layout */
		Rand_value = wilderness[y][x].seed;

                /* Generate a wilderness vault. */
                if (magik(DUN_WILD_VAULT)) {  
                        vault_type *v_ptr;
                        int vindex, vy, vx;
                        int i;
   
                        /* Pick a wilderness vault */
                        for (i = 0; i < 1000; i++)
                        {
                                /* Access a random vault record */
                                vindex = rand_int(max_v_idx);
                                v_ptr = &v_info[vindex];

                                /* Accept the first greater vault */
                                if (v_ptr->typ == 10) break;
                        }

                        /* Message */
                        if (cheat_room) msg_format("Wilderness Vault %d", vindex);

                        /* Boost the rating */
                        rating += v_ptr->rat;

                        vy = rand_range((v_ptr->hgt/2)+1, MAX_HGT-(v_ptr->hgt/2)-1);
                        vx = rand_range((v_ptr->wid/2)+1, MAX_WID-(v_ptr->wid/2)-1);

                        build_vault(vy, vx, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text);
                }

		/* Use the complex RNG */
		Rand_quick = FALSE;
#endif

		/* Hack -- Use the "simple" RNG */
		Rand_quick = TRUE;

		/* Hack -- Induce consistant town layout */
                Rand_value = wild_map[y][x].seed;

                entrance = wf_info[wild_map[y][x].feat].entrance;
                if (!entrance) entrance = wild_map[y][x].entrance;

                /* Create the dungeon if requested on the map */
                if(entrance >= 1000)
                {
                        int dy, dx;

                        dy = rand_range(6, cur_hgt - 6);
                        dx = rand_range(6, cur_wid - 6);

                        cave[dy][dx].feat = FEAT_MORE;
                        cave[dy][dx].special = entrance - 1000;
                }

		/* Use the complex RNG */
		Rand_quick = FALSE;

        /* MEGA HACK -- set at least one floor grid */
        for (y1 = 1; y1 < cur_hgt - 1; y1++)
        {
                for (x1 = 1; x1 < cur_wid - 1; x1++)
                {
                        if (cave_floor_bold(y1, x1)) hack_floor++;
                }
        }

        /* NO floor ? put one */
        if (!hack_floor)
        {
                cave[cur_hgt / 2][cur_wid / 2].feat = FEAT_GRASS;
                cave[cur_hgt / 2][cur_wid / 2].special = 0;
                hack_floor = 1;
        }

	/* Set the monster generation level to the wilderness level */
        monster_level = wf_info[wild_map[y][x].feat].level;

	/* Set the object generation level to the wilderness level */
        object_level = wf_info[wild_map[y][x].feat].level;

        return hack_floor;
}

/*
 * Bring the imprinted pets from the old level
 */
void replace_all_friends()
{
        int i;

        /* Scan every saved pet */
        for (i = 0; i < max_m_idx; i++)
        {
                if((km_list[i].r_idx) && (km_list[i].imprinted))
                {
                        int y = py, x = px;
                        cave_type *c_ptr;
                        monster_type *m_ptr;

                        /* Find a suitable location */
                        get_pos_player(5, &y, &x);
                        c_ptr = &cave[y][x];

                        /* Get a m_idx to use */
                        c_ptr->m_idx = m_pop();
                        m_ptr = &m_list[c_ptr->m_idx];

                        /* Actualy place the monster */
                        m_list[c_ptr->m_idx] = km_list[i];
                        m_ptr->fy = y;
                        m_ptr->fx = x;
                        m_ptr->hold_o_idx = 0;

                        /* Erase the old monster */
                        km_list[i].r_idx = 0;
                        km_list[i].imprinted = FALSE;
                }
        }
}

/*
 * Save the imprinted pets from the old level
 */
void save_all_friends()
{
        int i;

        /* Scan every monster */
        for (i = 0; i < max_m_idx; i++)
        {
                if((m_list[i].r_idx) && (m_list[i].imprinted))
                {
                        /* Actualy save the monster */
                        km_list[i] = m_list[i];
                }
        }
}

/*
 * Border of the wilderness area
 */
static border_type border;

/*
 * Build the wilderness area outside of the town.
 * -KMW-
 */
void wilderness_gen(int refresh)
{
        int i, y, x, hack_floor;
	bool daytime;
	int xstart = 0;
	int ystart = 0;
	cave_type *c_ptr;

	/* Init the wilderness */
	process_dungeon_file("w_info.txt", &ystart, &xstart, cur_hgt, cur_wid);

	x = p_ptr->wilderness_x;
	y = p_ptr->wilderness_y;

	/* Set the correct monster hook */
	set_mon_num_hook();

	/* Prepare allocation table */
	get_mon_num_prep();

	/* North border */
        generate_area(y-1, x, TRUE, FALSE, refresh);

	for (i = 1; i < MAX_WID - 1; i++)
	{
		border.north[i] = cave[MAX_HGT-2][i].feat;
	}

	/* South border */
        generate_area(y+1, x, TRUE, FALSE, refresh);

	for (i = 1; i < MAX_WID - 1; i++)
	{
		border.south[i] = cave[1][i].feat;
	}

	/* West border */
        generate_area(y, x-1, TRUE, FALSE, refresh);

	for (i = 1; i < MAX_HGT - 1; i++)
	{
		border.west[i] = cave[i][MAX_WID-2].feat;
	}

	/* East border */
        generate_area(y, x+1, TRUE, FALSE, refresh);

	for (i = 1; i < MAX_HGT - 1; i++)
	{
		border.east[i] = cave[i][1].feat;
	}

	/* North west corner */
        generate_area(y-1, x-1, FALSE, TRUE, refresh);
	border.north_west = cave[MAX_HGT-2][MAX_WID-2].feat;

	/* North east corner */
        generate_area(y-1, x+1, FALSE, TRUE, refresh);
	border.north_east = cave[MAX_HGT-2][1].feat;

	/* South west corner */
        generate_area(y+1, x-1, FALSE, TRUE, refresh);
	border.south_west = cave[1][MAX_WID-2].feat;

	/* South east corner */
        generate_area(y+1, x+1, FALSE, TRUE, refresh);
	border.south_east = cave[1][1].feat;


	/* Create terrain of the current area */
        hack_floor = generate_area(y, x, FALSE, FALSE, refresh);


	/* Special boundary walls -- North */
	for (i = 0; i < MAX_WID; i++)
	{
		cave[0][i].feat = FEAT_PERM_SOLID;
		cave[0][i].mimic = border.north[i];
	}
	
	/* Special boundary walls -- South */
	for (i = 0; i < MAX_WID; i++)
	{
		cave[MAX_HGT-1][i].feat = FEAT_PERM_SOLID;
		cave[MAX_HGT-1][i].mimic = border.south[i];
	}
	
	/* Special boundary walls -- West */
	for (i = 0; i < MAX_HGT; i++)
	{
		cave[i][0].feat = FEAT_PERM_SOLID;
		cave[i][0].mimic = border.west[i];
	}
	
	/* Special boundary walls -- East */
	for (i = 0; i < MAX_HGT; i++)
	{
		cave[i][MAX_WID-1].feat = FEAT_PERM_SOLID;
		cave[i][MAX_WID-1].mimic = border.east[i];
	}

	/* North west corner */
	cave[0][0].mimic = border.north_west;

	/* North east corner */
	cave[0][MAX_WID-1].mimic = border.north_east;

	/* South west corner */
	cave[MAX_HGT-1][0].mimic = border.south_west;

	/* South east corner */
	cave[MAX_HGT-1][MAX_WID-1].mimic = border.south_east;


	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
		daytime = TRUE;
	else
		daytime = FALSE;

	/* Light up or darken the area */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave grid */
			c_ptr = &cave[y][x];

			if (daytime)
			{
				/* Assume lit */
				c_ptr->info |= (CAVE_GLOW);

				/* Hack -- Memorize lit grids if allowed */
				if (view_perma_grids) c_ptr->info |= (CAVE_MARK);
			}
			else
			{
				/* Darken "boring" features */
                                if (!(f_info[c_ptr->feat].flags1 & FF1_REMEMBER))
				{
					/* Forget the grid */
					c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);
				}
			}
		}
	}

	player_place(p_ptr->oldpy, p_ptr->oldpx);
	p_ptr->leftbldg = FALSE;

	if (!refresh)
	{
                int lim = (generate_encounter==TRUE)?60:MIN_M_ALLOC_TN;

                /*
                 * Can't have more monsters than floor grids -1(for the player,
                 * not needed but safer
                 */
                if (lim > hack_floor - 1) lim = hack_floor - 1;

		/* Make some residents */
                for (i = 0; i < lim; i++)
		{
			/* Make a resident */
                        (void)alloc_monster((generate_encounter==TRUE)?0:3, (generate_encounter==TRUE)?FALSE:TRUE);
		}
                if(generate_encounter) ambush_flag = TRUE;
                generate_encounter = FALSE;
        }

	/* Set rewarded quests to finished */
	for (i = 0; i < max_quests; i++)
	{
		if (quest[i].status == QUEST_STATUS_REWARDED)
			quest[i].status = QUEST_STATUS_FINISHED;
	}
}

/*
 * Build the wilderness area.
 * -DG-
 */
void wilderness_gen_small()
{
        int i, j, entrance;
	int xstart = 0;
	int ystart = 0;

        /* To prevent stupid things */
        for (i = 0; i < MAX_WID; i++)
        for (j = 0; j < MAX_HGT; j++)
	{
                cave[j][i].feat = FEAT_PERM_SOLID;
	}

	/* Init the wilderness */
	process_dungeon_file("w_info.txt", &ystart, &xstart, cur_hgt, cur_wid);

        /* Fill the map */
        for (i = 0; i < max_wild_x; i++)
        for (j = 0; j < max_wild_y; j++)
	{
                entrance = wf_info[wild_map[j][i].feat].entrance;
                if (!entrance) entrance = wild_map[j][i].entrance;

                cave[j][i].feat = wf_info[wild_map[j][i].feat].feat;
                if (wild_map[j][i].entrance) cave[j][i].feat = FEAT_MORE;
                if ((cave[j][i].feat == FEAT_MORE) && (entrance >= 1000)) cave[j][i].special = entrance - 1000;

                cave[j][i].info |= (CAVE_GLOW | CAVE_MARK);
	}

        /* Place the player */
        px = p_ptr->wilderness_x;
        py = p_ptr->wilderness_y;

	/* Set rewarded quests to finished */
	for (i = 0; i < max_quests; i++)
	{
		if (quest[i].status == QUEST_STATUS_REWARDED)
			quest[i].status = QUEST_STATUS_FINISHED;
	}
}


/*
 * Maze dungeon generator
 */
char maze[(MAX_WID / 2) + 2][(MAX_HGT / 2) + 2];

void dig(int x, int y, int d)
{
        int k;
        int dx = 0, dy = 0;

        /*
         * first, open the wall of the new cell
         * in the direction we come from.
         */
        switch (d)
        {
                case 0:
                        maze[x][y]|=4;
                        break;
                case 1:
                        maze[x][y]|=8;
                        break;
                case 2:
                        maze[x][y]|=1;
                        break;
                case 3:
                        maze[x][y]|=2;
                        break;
        }

        /*
         * try to chage direction, here 50% times.
         * with smaller values (say 25%) the maze
         * is made of long straight corridors. with
         * greaters values (say 75%) the maze is
         * very "turny".
         */
        if (rand_range(1,100)<50) d=rand_range(0,3);

        for (k=1;k<=4;k++)
        {
                switch (d)
                {
                        case 0: dx=+1;dy= 0;break;
                        case 1: dx= 0;dy=-1;break;
                        case 2: dx=-1;dy= 0;break;
                        case 3: dx= 0;dy=+1;break;
                }
                if (maze[x+dx][y+dy]==0)
                {
                        /*
                         * now, open the wall of the new cell
                         * in the direction we go to.
                         */
                        switch (d) {
                                case 0:
                                        maze[x][y]|=1;
                                        break;
                                case 1:
                                        maze[x][y]|=2;
                                        break;
                                case 2:
                                        maze[x][y]|=4;
                                        break;
                                case 3:
                                        maze[x][y]|=8;
                                        break;
                        }
                        dig(x+dx,y+dy,d);
                }
                d=(d+1)%4;
        }
}

void generate_maze()
{
        int i, j, d;
        int x, dx = 0;
        int y, dy = 0;
        int m_1 = 0, m_2 = 0;

        /*
         * the empty maze is:
         *
         * -1 -1 ... -1 -1
         * -1  0      0 -1
         *  .            .
         *  .            .
         * -1  0      0 -1
         * -1 -1 ... -1 -1
         *
         *  -1 are so-called "sentinel value".
         *   0 are empty cells.
         *
         *  walls are not represented, only cells.
         *  at the end of the algorithm each cell
         *  contains a value that is bit mask
         *  representing surrounding walls:
         *
         *         bit #1
         *
         *        +------+
         *        |      |
         * bit #2 |      | bit #0
         *        |      |
         *        +------+
         *
         *         bit #3
         *
         * d is the direction you are digging
         * to. d value is the bit number:
         * d=0 --> go east
         * d=1 --> go north
         * etc
         *
         * you need only 4 bits per cell.
         * this gives you a very compact
         * maze representation.
         *
         */

        for (i=0;i<=(cur_wid / 2)+1;i++)
                for (j=0;j<=(cur_hgt / 2)+1;j++)
                        maze[i][j]=-1;

        for (i=1;i<=(cur_wid / 2);i++)
                for (j=1;j<=(cur_hgt / 2);j++)
                        maze[i][j]= 0;

        d=rand_range(0,3);
        x=rand_range(1,(cur_wid / 2));
        y=rand_range(1,(cur_hgt / 2));

        dig(x,y,d);

        maze[x][y]=0;

        for (d=0;d<=3;d++)
        {
                switch (d)
                {
                        case 0:
                                dx=+1;dy= 0;m_1=1;m_2=4;
                                break;
                        case 1:
                                dx= 0;dy=-1;m_1=2;m_2=8;
                                break;
                        case 2:
                                dx=-1;dy= 0;m_1=4;m_2=1;
                                break;
                        case 3:
                                dx= 0;dy=+1;m_1=8;m_2=2;
                                break;
                }
                if (maze[x+dx][y+dy]!=-1)
                if ((maze[x+dx][y+dy]&m_2)!=0)
                maze[x][y]|=m_1;
        }

        /* Translate the maze bit array into a real dungeon map -- DG */
        for(i = 1; i <= (cur_wid / 2) - 2; i++)
        for(j = 1; j <= (cur_hgt / 2) - 2; j++)
        {
                if(maze[i][j])
                {
                        cave[j * 2][i * 2].feat = floor_type[rand_int(100)];
                }
                if(maze[i][j] & 1)
                {
                        cave[j * 2][(i * 2) + 1].feat = floor_type[rand_int(100)];
                }
                if(maze[i][j] & 8)
                {
                        cave[(j * 2) + 1][i * 2].feat = floor_type[rand_int(100)];
                }
        }
}

/*
 * Return the dungeon type of the current level(it can only return the
 * principal dungeons)
 */
byte calc_dungeon_type()
{
        int i;

        for (i = 0; i < max_d_idx; i++)
        {
                if ((dun_level >= d_info[i].mindepth) &&
                    (dun_level <= d_info[i].maxdepth) &&
                    (d_info[i].flags1 & DF1_PRINCIPAL))
                        return i;
        }
        return 0;
}

/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(void)
{
        int i, j, k, y, x, y1, x1, branch;

	int max_vault_ok = 2;
        int duntype;

	bool destroyed = FALSE;
	bool empty_level = FALSE;
        bool water_level = FALSE;
	bool cavern = FALSE;

	dun_data dun_body;
        duntype = randint(100);

        /* Fill the arrays of floors and walls in the good proportions */
        for (i = 0; i < 100; i++)
        {
                int lim1, lim2, lim3;

                lim1 = d_info[dungeon_type].floor_percent1;
                lim2 = lim1 + d_info[dungeon_type].floor_percent2;
                lim3 = lim2 + d_info[dungeon_type].floor_percent3;

                /* Mega-HACK -- if a druid request a flooded level, he/she obtains it */
                if((p_ptr->pclass == CLASS_DRUID) && (p_ptr->class_extra6 & CLASS_FLOOD_LEVEL))
                {
                        if (i < 33)
                                floor_type[i] = FEAT_DEEP_WATER;
                        else
                                floor_type[i] = FEAT_SHAL_WATER;
                }
                else
                {
                        if (i < lim1)
                                floor_type[i] = d_info[dungeon_type].floor1;
                        else if (i < lim2)
                                floor_type[i] = d_info[dungeon_type].floor2;
                        else if (i < lim3)
                                floor_type[i] = d_info[dungeon_type].floor3;
                }

                lim1 = d_info[dungeon_type].fill_percent1;
                lim2 = lim1 + d_info[dungeon_type].fill_percent2;
                lim3 = lim2 + d_info[dungeon_type].fill_percent3;
                if (i < lim1)
                        fill_type[i] = d_info[dungeon_type].fill_type1;
                else if (i < lim2)
                        fill_type[i] = d_info[dungeon_type].fill_type2;
                else if (i < lim3)
                        fill_type[i] = d_info[dungeon_type].fill_type3;

        }

        /* Mega hack -- remove the druid flood request */
        if((p_ptr->pclass == CLASS_DRUID) && (p_ptr->class_extra6 & CLASS_FLOOD_LEVEL))
        {
                p_ptr->class_extra6 &= ~(CLASS_FLOOD_LEVEL);
        }

        feat_wall_outer = d_info[dungeon_type].outer_wall;
        feat_wall_inner = d_info[dungeon_type].inner_wall;

	/* Set the correct monster hook */
	set_mon_num_hook();

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Global data */
	dun = &dun_body;

	if (!(max_panel_rows)) max_vault_ok--;
	if (!(max_panel_cols)) max_vault_ok--;

        if(!(d_info[dungeon_type].flags1 & DF1_MAZE))
        {
                if ((randint(EMPTY_LEVEL)==1) && empty_levels)
                {
                        empty_level = TRUE;
                        if ((cheat_room)||(p_ptr->precognition))
                                msg_print("Arena level.");
                }

                if ((randint(WATER_LEVEL)==1) && water_levels && (dungeon_type == DUNGEON_ANGBAND))
                {
                        water_level = TRUE;
                        if ((cheat_room)||(p_ptr->precognition))
                                msg_print("Flooded level.");
                }
        }

	/* Hack -- Start with basic granite */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			cave_type *c_ptr = &cave[y][x];

                        if (empty_level)
                                c_ptr->feat = floor_type[rand_int(100)];
                        else if ((dungeon_type == DUNGEON_ANGBAND) && (water_level)) /* Flooded Angband */
                                c_ptr->feat = FEAT_DEEP_WATER;
                        else 
                                c_ptr->feat = fill_type[rand_int(100)];
                }
	}


        if(!(d_info[dungeon_type].flags1 & DF1_MAZE))
        {

        /* Possible cavern */
        if ((d_info[dungeon_type].flags1 & DF1_CAVERN) && (rand_int(dun_level/2) > DUN_CAVERN))
        {
		cavern=TRUE;
		
                /* Make a large fractal cave in the middle of the dungeon */		
		if (cheat_room)
			msg_print("Cavern on level.");
		
		build_cavern(); 				
        }

	/* Possible "destroyed" level */
	if ((dun_level > 10) && (rand_int(DUN_DEST) == 0) && (small_levels))
		destroyed = TRUE;

	/* Hack -- No destroyed "quest" levels */
	if (is_quest(dun_level)) destroyed = FALSE;

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = cur_hgt / BLOCK_HGT;
	dun->col_rooms = cur_wid / BLOCK_WID;


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
	for (i = 0; i < DUN_ROOMS; i++)
	{
		/* Pick a block for the room */
		y = rand_int(dun->row_rooms);
		x = rand_int(dun->col_rooms);

		/* Align dungeon rooms */
		if (dungeon_align)
		{
			/* Slide some rooms right */
			if ((x % 3) == 0) x++;

			/* Slide some rooms left */
			if ((x % 3) == 2) x--;
		}

		/* Destroyed levels are boring */
		if (destroyed)
		{
			/*The deeper you are, the more cavelike the rooms are */
			
			/*no caves when cavern exists: they look bad */
			k=randint(100);
                        if ((k<dun_level)&&(!cavern))
				{
                                /* Type 10 -- Fractal cave */
                                if(room_build(y, x, 10)) continue;
				}
			else			
			/* Attempt a "trivial" room */
			if (room_build(y, x, 1)) continue;

			/* Never mind */
			continue;
		}

		/* Attempt an "unusual" room */
		if (ironman_rooms || (rand_int(DUN_UNUSUAL) < dun_level))
		{
                        /* Roll for room type */
                        k = (ironman_rooms ? 0 : rand_int(100));

                        /* Attempt a very unusual room */ /* test hack */
                        if (ironman_rooms || (rand_int(DUN_UNUSUAL) < dun_level))
			{
#ifdef FORCE_V_IDX
                                if (room_build(y, x, 8)) continue;
#else
                                /* Type 8 -- Greater vault (10%) */
                                if (k < 10)
                                {
                                    if (max_vault_ok > 1)
                                    {
                                        if (room_build(y, x, 8)) continue;
                                    }
                                    else
                                    {
                                        if (cheat_room) msg_print("Refusing a greater vault.");
                                    }
                                }

				/* Type 7 -- Lesser vault (15%) */
                                if (k < 25)
                                {
                                    if (max_vault_ok > 0)
                                    {
                                        if (room_build(y, x, 7)) continue;
                                    }
                                    else
                                    {
                                        if (cheat_room) msg_print("Refusing a lesser vault.");
                                    }
                                }


                                /* Type 5 -- Monster nest (15%) */
                                if ((k < 40) && room_build(y, x, 5)) continue;

                                /* Type 6 -- Monster pit (15%) */
                                if ((k < 55) && room_build(y, x, 6)) continue;
#endif

                        }

                        /* Type 4 -- Large room (25%) */
			if ((k < 25) && room_build(y, x, 4)) continue;

                        /* Type 3 -- Cross room (20%) */
                        if ((k < 45) && room_build(y, x, 3)) continue;

                        /* Type 2 -- Overlapping (20%) */
                        if ((k < 65) && room_build(y, x, 2)) continue;

                        /* Type 10 -- Fractal cave (15%) */
                        if ((k < 80) && room_build(y, x, 10)) continue;

                        /* Type 9 -- Circular (20%) */
			if ((k < 100) && room_build(y, x, 9)) continue;
		}

		/* Attempt a trivial room */
                if(d_info[dungeon_type].flags1 & DF1_CAVE)
                {
                        if (room_build(y, x, 10)) continue;
                }
                else
                {
                        if (room_build(y, x, 1)) continue;
                }
	}
        }

	/* Special boundary walls -- Top */
	for (x = 0; x < cur_wid; x++)
	{
		cave_type *c_ptr = &cave[0][x];

		/* Clear previous contents, add "solid" perma-wall */
		c_ptr->feat = FEAT_PERM_SOLID;
	}

	/* Special boundary walls -- Bottom */
	for (x = 0; x < cur_wid; x++)
	{
		cave_type *c_ptr = &cave[cur_hgt-1][x];

		/* Clear previous contents, add "solid" perma-wall */
		c_ptr->feat = FEAT_PERM_SOLID;
	}

	/* Special boundary walls -- Left */
	for (y = 0; y < cur_hgt; y++)
	{
		cave_type *c_ptr = &cave[y][0];

		/* Clear previous contents, add "solid" perma-wall */
		c_ptr->feat = FEAT_PERM_SOLID;
	}

	/* Special boundary walls -- Right */
	for (y = 0; y < cur_hgt; y++)
	{
		cave_type *c_ptr = &cave[y][cur_wid-1];

		/* Clear previous contents, add "solid" perma-wall */
		c_ptr->feat = FEAT_PERM_SOLID;
	}


        if(!(d_info[dungeon_type].flags1 & DF1_MAZE))
        {
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
                build_tunnel(dun->cent[i].y, dun->cent[i].x, y, x, water_level);

		/* Remember the "previous" room */
		y = dun->cent[i].y;
		x = dun->cent[i].x;
	}

	/* Place intersection doors	 */
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
        if ((dungeon_type == DUNGEON_MORDOR) || (dungeon_type == DUNGEON_ANGBAND))
	for (i = 0; i < DUN_STR_MAG; i++)
	{
		build_streamer(FEAT_MAGMA, DUN_STR_MC);
	}
        else build_streamer(FEAT_MAGMA, DUN_STR_MC);

	/* Hack -- Add some quartz streamers */
        if ((dungeon_type == DUNGEON_MORDOR) || (dungeon_type == DUNGEON_ANGBAND))
	for (i = 0; i < DUN_STR_QUA; i++)
	{
		build_streamer(FEAT_QUARTZ, DUN_STR_QC);
	}
        else build_streamer(FEAT_QUARTZ, DUN_STR_QC);


	/* Destroy the level if necessary */
	if (destroyed) destroy_level();

        /* Hack -- Add some rivers if requested */
        if((d_info[dungeon_type].flags1 & DF1_WATER_RIVER) && !rand_int(4))
        {
                if((cheat_room)||(p_ptr->precognition)) msg_print("River of water.");
                add_river(FEAT_DEEP_WATER, FEAT_SHAL_WATER);
        }
        if((d_info[dungeon_type].flags1 & DF1_LAVA_RIVER) && !rand_int(4))
        {
                if((cheat_room)||(p_ptr->precognition)) msg_print("River of lava.");
                add_river(FEAT_DEEP_LAVA, FEAT_SHAL_LAVA);
        }
        if(d_info[dungeon_type].flags1 & DF1_WATER_RIVERS)
        {
                int max = 3 + rand_int(2);
                bool said = FALSE;

                for(i = 0; i < max; i++)
                {
                        if(rand_int(3) == 0)
                        {
                                add_river(FEAT_DEEP_WATER, FEAT_SHAL_WATER);
                                if(!said && ((cheat_room)||(p_ptr->precognition))) msg_print("Rivers of water.");
                                said = TRUE;
                        }
                }
        }
        if(d_info[dungeon_type].flags1 & DF1_LAVA_RIVERS)
        {
                int max = 2 + rand_int(2);
                bool said = FALSE;

                for(i = 0; i < max; i++)
                {
                        if(rand_int(3) == 0)
                        {
                                add_river(FEAT_DEEP_LAVA, FEAT_SHAL_LAVA);
                                if(!said && ((cheat_room)||(p_ptr->precognition))) msg_print("Rivers of lava.");
                                said = TRUE;
                        }
                }
        }

	/* Add streamers of trees, water, or lava -KMW- */
	if ((dun_level <= 2) && (randint(20) > 15))
		for (i=0; i < randint(DUN_STR_QUA); i++)
			build_streamer2(FEAT_TREES,1);

	if ((dun_level <= 19) && (randint(20) > 15))
	{
		for (i=0; i < randint(DUN_STR_QUA - 1); i++)
			build_streamer2(FEAT_SHAL_WATER,0); 
		if (randint(20) > 15)
		{
			for (i=0; i < randint(DUN_STR_QUA); i++)
				build_streamer2(FEAT_DEEP_WATER,1);
		}
	}
	else if ((dun_level > 19) && (randint(20) > 15))
	{
		for (i=0; i < randint(DUN_STR_QUA); i++)
			build_streamer2(FEAT_SHAL_LAVA,0);
		if (randint(20) > 15)
		{
			for (i=0; i < randint(DUN_STR_QUA - 1); i++)
				build_streamer2(FEAT_DEEP_LAVA,1);
		}
	}
	else if ((randint(20) > 15))
	{
		for (i = 0; i < randint(DUN_STR_QUA - 1); i++)
			build_streamer2(FEAT_SHAL_WATER, 0);

		if (randint(20) > 15)
		{
			for (i = 0; i < randint(DUN_STR_QUA); i++)
				build_streamer2(FEAT_DEEP_WATER, 1);
		}
	}
        }
        else /* A MAZE */
        {
                generate_maze();
        }

        /* Is there a dungeon branch ? */
        if ((branch = get_branch()))
        {
                /* Place 1 down stair some walls */
                alloc_stairs(FEAT_MORE, 5, 3, branch);
        }

        /* Is there a father dungeon branch ? */
        if ((branch = get_fbranch()))
        {
                /* Place 1 down stair some walls */
                alloc_stairs(FEAT_LESS, 5, 3, branch);
        }

       if ((dun_level < d_info[dungeon_type].maxdepth) || ((dun_level == d_info[dungeon_type].maxdepth) && (!(d_info[dungeon_type].flags1 & DF1_NO_DOWN))))
       {
                /* Place 3 or 4 down stairs near some walls */
                alloc_stairs(FEAT_MORE, rand_range(3, 4), 3, 0);

                /* Place 1 or 2 down shafts near some walls */
                alloc_stairs(FEAT_SHAFT_DOWN, rand_range(0, 1), 3, 0);
        }

       if ((dun_level > d_info[dungeon_type].mindepth) || ((dun_level == d_info[dungeon_type].mindepth) && (!(d_info[dungeon_type].flags1 & DF1_NO_UP))))
       {
                /* Place 1 or 2 up stairs near some walls */
                alloc_stairs(FEAT_LESS, rand_range(1, 2), 3, 0);
  
                /* Place 0 or 1 up shafts near some walls */ 
                alloc_stairs(FEAT_SHAFT_UP, rand_range(0, 1), 3, 0);
        }

	/* Determine the character location */
	if (!new_player_spot())
		return FALSE;

        if ((dungeon_stair) && (!special_flag) && (dun_level) && (!is_quest(dun_level)) && !branch)
        {
                if(old_dun_level < dun_level)
                {
                        place_up_stairs(py ,px);
                }else{
                        place_down_stairs(py ,px);
                }
        }

        /* Monsters and objects change even in persistent dungeons. */
        if (seed_dungeon) {
            Rand_quick = FALSE;
        }

	/* Handle the quest monster placements */
	for (i = 0; i < max_quests; i++)
	{
		if ((quest[i].status == QUEST_STATUS_TAKEN) &&
		   ((quest[i].type == QUEST_TYPE_KILL_LEVEL) ||
		    (quest[i].type == QUEST_TYPE_RANDOM)) &&
		    (quest[i].level == dun_level))
		{
			monster_race *r_ptr = &r_info[quest[i].r_idx];

			/* Hack -- "unique" monsters must be "unique" */
                        /* Dungeon guardians are considered even more uniques :) */
                        if (((r_ptr->flags1 & RF1_UNIQUE) &&
                            (r_ptr->cur_num >= r_ptr->max_num)) ||
                            (r_ptr->flags9 & RF9_SPECIAL_GENE))
			{
				/* The unique is already dead */
				quest[i].status = QUEST_STATUS_FINISHED;
			}
			else
			{
				for (j = 0; j < (quest[i].max_num - quest[i].cur_num); j++)
				{
					for (k = 0; k < SAFE_MAX_ATTEMPTS; k++)
					{
						/* Find an empty grid */
						while (TRUE)
						{
							y = rand_int(cur_hgt);
							x = rand_int(cur_wid);
							if (!cave_naked_bold(y,x)) continue;
							if (distance(y, x, py, px) < 10) continue;
							else break;
						}

						/* Try to place the monster */
						if (place_monster_aux(y, x, quest[i].r_idx, FALSE, FALSE, FALSE))
						{
							/* Success */
							break;
						}
						else
						{
							/* Failure - Try again */
							continue;
						}
					}
				}
			}
		}
	}


	/* Basic "amount" */
	k = (dun_level / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;

	/* Pick a base number of monsters */
        i = d_info[dungeon_type].min_m_alloc_level;

	/* To make small levels a bit more playable */
	if (cur_hgt < MAX_HGT || cur_wid < MAX_WID)
	{
		int small_tester = i;

		i = (i * cur_hgt) / MAX_HGT;
		i = (i * cur_wid) / MAX_WID;
		i += 1;

		if (i > small_tester) i = small_tester;
		else if (cheat_hear)
		{
			msg_format("Reduced monsters base from %d to %d", small_tester, i);
		}
	}

	i += randint(8);

	/* Put some monsters in the dungeon */
	for (i = i + k; i > 0; i--)
	{
		(void)alloc_monster(0, TRUE);
	}

        for(i = 0; i < MAX_FATES; i++)
        {
                if(((!fates[i].serious)&&(randint(2)==1)) || (fates[i].serious))
                if((fates[i].fate)&&(fates[i].level == dun_level))
                {
                fate_flag = TRUE;
                
                switch(fates[i].fate)
                {
                        case FATE_FIND_O:
                        {
                                int oy = py + 1;
                                int ox = px;
                                object_type *q_ptr, forge;
                                
                                /* Get local object */
                                q_ptr = &forge;

                                /* Mega-Hack */
                                object_prep(q_ptr, fates[i].o_idx);

                                /* Mega-Hack */
                                apply_magic(q_ptr, dun_level, TRUE, TRUE, fates[i].serious);

                                get_pos_player(10, &oy, &ox);

                                /* Drop it from the heaven */
                                drop_near(q_ptr, -1, oy, ox);

                                fates[i].fate = FATE_NONE;
                                break;
                        }
                        case FATE_FIND_R:
                        {
                                int oy = py + 1;
                                int ox = px;
                                
                                get_pos_player(10, &oy, &ox);

                                place_monster_one(oy, ox, fates[i].r_idx, fates[i].serious, FALSE);

                                fates[i].fate = FATE_NONE;
                                break;
                        }
                        case FATE_FIND_A:
                        {
                                int oy = py + 1;
                                int ox = px;
                                
                                get_pos_player(10, &oy, &ox);

                                if (a_info[fates[i].a_idx].cur_num == 0)
				{
                                        artifact_type *a_ptr = &a_info[fates[i].a_idx];
                                        object_type *q_ptr, forge;
                                        int I_kind;

					/* Get local object */
					q_ptr = &forge;

					/* Wipe the object */
					object_wipe(q_ptr);

					/* Acquire the "kind" index */
					I_kind = lookup_kind(a_ptr->tval, a_ptr->sval);

					/* Create the artifact */
					object_prep(q_ptr, I_kind);

					/* Save the name */
                                        q_ptr->name1 = fates[i].a_idx;

					/* Extract the fields */
					q_ptr->pval = a_ptr->pval;
					q_ptr->ac = a_ptr->ac;
					q_ptr->dd = a_ptr->dd;
					q_ptr->ds = a_ptr->ds;
					q_ptr->to_a = a_ptr->to_a;
					q_ptr->to_h = a_ptr->to_h;
					q_ptr->to_d = a_ptr->to_d;
					q_ptr->weight = a_ptr->weight;

					/* Hack -- acquire "cursed" flag */
					if (a_ptr->flags3 & (TR3_CURSED)) q_ptr->ident |= (IDENT_CURSED);

					random_artifact_resistance(q_ptr);

                                        a_info[fates[i].a_idx].cur_num = 1;

					/* Drop the artifact from heaven */
                                        drop_near(q_ptr, -1, oy, ox);
				}
                                

                                fates[i].fate = FATE_NONE;
                                break;
                        }
                        case FATE_DIE:
                                msg_print("You stumble on a little rock on the floor.");
                                msg_print("You break your skull on the floor.");
                                p_ptr->chp = 0;
                                take_hit(1, "stupid fall");
                                fates[i].fate = FATE_NONE;
                                break;
                }
                }
        }
        /* Re scan the list to eliminate the inutile fate */
        for(i = 0; i < MAX_FATES; i++)
        {
                switch(fates[i].fate)
                {
                        case FATE_FIND_A:
                        {
                                if(a_info[fates[i].a_idx].cur_num == 1) fates[i].fate = FATE_NONE;
                                break;
                        }
                        case FATE_FIND_R:
                        {
                                if((r_info[fates[i].r_idx].cur_num == 1)&&(r_info[fates[i].r_idx].flags1 & RF1_UNIQUE)) fates[i].fate = FATE_NONE;
                                break;
                        }
                }
        }

	/* Place some traps in the dungeon */
        alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k * 2));

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ROOM, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, randnor(DUN_AMT_GOLD, 3));

        /* Put some altars */
        alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_ALTAR, randnor(DUN_AMT_ALTAR, 3));

        /* Put some between gates */
        alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_BETWEEN, randnor(DUN_AMT_BETWEEN, 3));

        /* Put an Artifact and Artifact Guardian is requested */
        if(d_info[dungeon_type].final_guardian && (d_info[dungeon_type].maxdepth == dun_level))
        {
                int oy;
                int ox;
                int m_idx, try = 4000;
                                
                /* Find a good position */
                while(try)
                {
                        /* Get a random spot */
                        oy = randint(cur_hgt - 4) + 2;
                        ox = randint(cur_wid - 4) + 2;

                        /* Is it a good spot ? */
                        if (cave_empty_bold(oy, ox)) break;

                        /* One less try */
                        try--;
                }

                /* Place the guardian */
                hack_allow_special = TRUE;
                place_monster_one(oy, ox, d_info[dungeon_type].final_guardian, FALSE, FALSE);

                m_idx = cave[oy][ox].m_idx;

                if(d_info[dungeon_type].final_artifact && m_idx)
                {
                                        artifact_type *a_ptr = &a_info[d_info[dungeon_type].final_artifact];
                                        object_type *q_ptr, forge, *o_ptr;
                                        int I_kind, o_idx;

					/* Get local object */
					q_ptr = &forge;

					/* Wipe the object */
					object_wipe(q_ptr);

					/* Acquire the "kind" index */
					I_kind = lookup_kind(a_ptr->tval, a_ptr->sval);

					/* Create the artifact */
					object_prep(q_ptr, I_kind);

					/* Save the name */
                                        q_ptr->name1 = d_info[dungeon_type].final_artifact;

					/* Extract the fields */
					q_ptr->pval = a_ptr->pval;
					q_ptr->ac = a_ptr->ac;
					q_ptr->dd = a_ptr->dd;
					q_ptr->ds = a_ptr->ds;
					q_ptr->to_a = a_ptr->to_a;
					q_ptr->to_h = a_ptr->to_h;
					q_ptr->to_d = a_ptr->to_d;
					q_ptr->weight = a_ptr->weight;

					/* Hack -- acquire "cursed" flag */
					if (a_ptr->flags3 & (TR3_CURSED)) q_ptr->ident |= (IDENT_CURSED);

					random_artifact_resistance(q_ptr);

                                        a_info[d_info[dungeon_type].final_artifact].cur_num = 1;

                                        /* Create the artifact in the guardian's inventory */
                                        /* Get new object */
                                        o_idx = o_pop();

                                        if(o_idx)
                                        {
                                                /* Get the item */
                                                o_ptr = &o_list[o_idx];

                                                /* Structure copy */
                                                object_copy(o_ptr, q_ptr);

                                                /* Build a stack */
                                                o_ptr->next_o_idx = m_list[m_idx].hold_o_idx;

                                                o_ptr->held_m_idx = m_idx;
                                                o_ptr->ix = 0;
                                                o_ptr->iy = 0;

                                                m_list[m_idx].hold_o_idx = o_idx;
                                        }
                }
                hack_allow_special = FALSE;
        }

        if ((empty_level) && (randint(DARK_EMPTY)!=1 || (randint(100) > dun_level)))
                wiz_lite();

    return TRUE;
}


/*
 * Builds the arena after it is entered -KMW-
 */
static void build_arena(void)
{
	int yval, y_height, y_depth, xval, x_left, x_right;
	register int i, j;

	yval = SCREEN_HGT / 2;
	xval = SCREEN_WID / 2;
	y_height = yval - 10 + SCREEN_HGT;
	y_depth = yval + 10 + SCREEN_HGT;
	x_left = xval - 32 + SCREEN_WID;
	x_right = xval + 32 + SCREEN_WID;

	for (i = y_height; i <= y_height + 5; i++)
		for (j = x_left; j <= x_right; j++)
		{
			cave[i][j].feat = FEAT_PERM_EXTRA;
			cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
		}
	for (i = y_depth; i >= y_depth - 5; i--)
		for (j = x_left; j <= x_right; j++)
		{
			cave[i][j].feat = FEAT_PERM_EXTRA;
			cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
		}
	for (j = x_left; j <= x_left + 17; j++)
		for (i = y_height; i <= y_depth; i++)
		{
			cave[i][j].feat = FEAT_PERM_EXTRA;
			cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
		}
	for (j = x_right; j >= x_right - 17; j--)
		for (i = y_height; i <= y_depth; i++)
		{
			cave[i][j].feat = FEAT_PERM_EXTRA;
			cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
		}

	cave[y_height+6][x_left+18].feat = FEAT_PERM_EXTRA;
	cave[y_height+6][x_left+18].info |= (CAVE_GLOW | CAVE_MARK);
	cave[y_depth-6][x_left+18].feat = FEAT_PERM_EXTRA;
	cave[y_depth-6][x_left+18].info |= (CAVE_GLOW | CAVE_MARK);
	cave[y_height+6][x_right-18].feat = FEAT_PERM_EXTRA;
	cave[y_height+6][x_right-18].info |= (CAVE_GLOW | CAVE_MARK);
	cave[y_depth-6][x_right-18].feat = FEAT_PERM_EXTRA;
	cave[y_depth-6][x_right-18].info |= (CAVE_GLOW | CAVE_MARK);

	i = y_height + 5;
	j = xval + SCREEN_WID;
	cave[i][j].feat = FEAT_BLDG_HEAD + 2;
	cave[i][j].info |= (CAVE_GLOW | CAVE_MARK);
	player_place(i+1, j);
}


/* 
 * Town logic flow for generation of arena -KMW-
 */
static void arena_gen(void)
{
	int y,x;
	int qy = SCREEN_HGT;
	int qx = SCREEN_WID;
	bool daytime;

	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
		daytime = TRUE;

	/* Night time */
	else
		daytime = FALSE;

	/* Start with solid walls */
	for (y = 0; y < MAX_HGT; y++)
	{
		for (x = 0; x < MAX_WID; x++)
		{
			/* Create "solid" perma-wall */
			cave[y][x].feat = FEAT_PERM_SOLID;

			/* Illuminate and memorize the walls */
			cave[y][x].info |= (CAVE_GLOW | CAVE_MARK);
		}
	}

	/* Then place some floors */
	for (y = qy+1; y < qy+SCREEN_HGT-1; y++)
	{
		for (x = qx+1; x < qx+SCREEN_WID-1; x++)
		{
			/* Create empty floor */
			cave[y][x].feat = FEAT_FLOOR;

			/* Darken and forget the floors */
			cave[y][x].info &= ~(CAVE_GLOW | CAVE_MARK);

			/* Day time */
			if (daytime)
			{
				/* Perma-Lite */
				cave[y][x].info |= (CAVE_GLOW);

				/* Memorize */
				if (view_perma_grids) cave[y][x].info |= (CAVE_MARK);
			}
		}
	}

	build_arena();

	place_monster_aux(py+5, px, arena_monsters[p_ptr->arena_number],
	    FALSE, FALSE, 0);
}


/*
 * Generate a quest level
 */
static void quest_gen(void)
{
	int x, y;

	int xstart = 0;
	int ystart = 0;


	/* Start with perm walls */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			cave[y][x].feat = FEAT_PERM_SOLID; 
		}
	}

	dun_level = quest[p_ptr->inside_quest].level;

	/* Set the correct monster hook */
	set_mon_num_hook();

	/* Prepare allocation table */
	get_mon_num_prep();

        hack_allow_special = TRUE;
	init_flags = INIT_CREATE_DUNGEON | INIT_ASSIGN;
	process_dungeon_file("q_info.txt", &ystart, &xstart, cur_hgt, cur_wid);
        hack_allow_special = FALSE;
}

/*
 * Hack -- fill in "special" rooms
 */
void build_special(int yval, int xval, int ymax, int xmax, cptr data)
{
        int dx, dy, x, y, bwy[128], bwx[128];

	cptr t;

	cave_type *c_ptr;
	
        vault_type    *v_ptr, forge;
	int             i;

        /* Clean the between gates arrays */
        for(i = 0; i < 128; i++)
        {
                bwy[i] = bwx[i] = 9999;
        }

        v_ptr = &forge;

        for (i=1;i<max_v_idx;i++)
	{
		v_ptr = &v_info[i];
                if ((v_ptr->lvl == dun_level) && (v_ptr->dun_type == dungeon_type))
			break;
	}
msg_format("v_ptr->lvl =%d, v_ptr->dun_type = %", v_ptr->lvl, v_ptr->dun_type);
msg_print(NULL);
	
	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the location */
			x = dx + 2;
			y = dy + 2;
			
			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Lay down a floor */
                        c_ptr->feat = FEAT_FLOOR;
		       
			/* Analyze the grid */
			switch (*t)
			{
				/* Granite wall (outer) */
				case '%':
				c_ptr->feat = FEAT_WALL_OUTER;
				break;

				/* Stairs up */
				case '<':
				place_up_stairs(y,x);
				break;

				/* Stairs down */
				case '>':
				place_down_stairs(y,x);
				break;

				/* Player position */
				case 'P':
				px=x;
				py=y;
				break;

				/* Granite wall (inner) */
				case '#':
				c_ptr->feat = FEAT_WALL_INNER;
				break;

				/* Permanent wall (inner) */
				case 'X':
				c_ptr->feat = FEAT_PERM_INNER;
				break;

				/* Secret doors */
				case '+':
				place_secret_door(y, x);
				break;

				/* Trap */
				case '^':
				place_trap(y, x);
				break;

                                /* Mountain */
                                case 'M':
                                c_ptr->feat = FEAT_MOUNTAIN;
				break;

                                /* Shallow Lava */
                                case 'l':
                                c_ptr->feat = FEAT_SHAL_LAVA;
				break;

                                /* Deep Lava */
                                case 'L':
                                c_ptr->feat = FEAT_DEEP_LAVA;
				break;

                                /* Shallow Water */
                                case 'w':
                                c_ptr->feat = FEAT_SHAL_WATER;
				break;

                                /* Deep Water */
                                case 'W':
                                c_ptr->feat = FEAT_DEEP_WATER;
				break;

                                /* Tree */
                                case 'T':
                                c_ptr->feat = FEAT_TREES;
				break;

                                /* Glass wall */
                                case 'G':
                                c_ptr->feat = FEAT_GLASS_WALL;
				break;

                                /* Illusion wall */
                                case 'I':
                                c_ptr->feat = FEAT_ILLUS_WALL;
				break;

				/* Random door */
				case 'D':
				place_random_door(y, x);
				break;
			}
		}
	}


	/* Place dungeon monsters and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Extract the grid */
			x = dx + 2;
			y = dy + 2;

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Analyze the symbol */
			switch (*t)
			{
				/* Monster */
				case 'a':       /* monster 1 */
				case 'b':       /* monster 2 */
				case 'c':       /* monster 3 */
				case 'd':       /* monster 4 */
				case 'e':       /* monster 5 */
				case 'f':       /* monster 6 */
				case 'g':       /* monster 7 */
				case 'h':       /* monster 8 */
				case 'i':       /* monster 9 */
				case 'j':       /* monster 10 */
				{
                                        monster_race    *r_ptr = &r_info[v_ptr->mon[*t - 'a']];
                                        if (r_ptr->flags1 & RF1_UNIQUE)
                                                r_ptr->max_num = 1;     /* make alive again */
                                        else
                                                r_ptr->max_num++;       /* make alive again */

                                        place_monster_aux(y,x,v_ptr->mon[*t - 'a'],FALSE,FALSE,FALSE);
					break;
				}

				/* Monster */
			case '&':
				{
					monster_level = dun_level + 5;
					place_monster(y, x, TRUE, TRUE);
					monster_level = dun_level;
					break;
				}

				/* Meaner monster */
			case '@':
				{
					monster_level = dun_level + 11;
					place_monster(y, x, TRUE, TRUE);
					monster_level = dun_level;
					break;
				}

				/* Meaner monster, plus treasure */
			case '9':
				{
					monster_level = dun_level + 9;
					place_monster(y, x, TRUE, TRUE);
					monster_level = dun_level;
					object_level = dun_level + 7;
					place_object(y, x, TRUE, FALSE);
					object_level = dun_level;
					break;
				}

				/* Nasty monster and treasure */
			case '8':
				{
					monster_level = dun_level + 40;
					place_monster(y, x, TRUE, TRUE);
					monster_level = dun_level;
					object_level = dun_level + 20;
					place_object(y, x, TRUE, TRUE);
					object_level = dun_level;
					break;
				}

				/* Monster and/or object */
			case ',':
				{
					if (rand_int(100) < 50)
					{
						monster_level = dun_level + 3;
						place_monster(y, x, TRUE, TRUE);
						monster_level = dun_level;
					}
					if (rand_int(100) < 50)
					{
						object_level = dun_level + 7;
						place_object(y, x, FALSE, FALSE);
						object_level = dun_level;
					}
					break;
				}

                                /* Artifact or Object Kind*/
                                case '1':       /* Artifact or Object Kind 1 */
                                case '2':       /* Artifact or Object Kind 2 */
                                case '3':       /* Artifact or Object Kind 3 */
                                if(v_ptr->item[*t - '1'] < 1000)
                                {
                                        artifact_type   *a_ptr = &a_info[v_ptr->item[*t - '1']];
					object_type     forge;
					object_type     *q_ptr;
					object_type     *o_ptr;
					
					s16b            o_idx;
					int             k_idx;
					
					q_ptr = &forge;
					object_wipe(q_ptr);
                                        a_ptr->cur_num = 1;
					k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);
					object_prep(q_ptr, k_idx);
                                        q_ptr->name1 = v_ptr->item[*t - '1'];

                                        /* Extract the other fields */
                                        q_ptr->pval = a_ptr->pval;
                                        q_ptr->ac = a_ptr->ac;
                                        q_ptr->dd = a_ptr->dd;
                                        q_ptr->ds = a_ptr->ds;
                                        q_ptr->to_a = a_ptr->to_a;
                                        q_ptr->to_h = a_ptr->to_h;
                                        q_ptr->to_d = a_ptr->to_d;
                                        q_ptr->weight = a_ptr->weight;

                                        o_idx = o_pop();
					o_ptr = &o_list[o_idx];
					object_copy(o_ptr,q_ptr);
					o_ptr->iy = y;
					o_ptr->ix = x;
					c_ptr = &cave[y][x];
					o_ptr->next_o_idx = c_ptr->o_idx;
					c_ptr->o_idx = o_idx;

					break;
				}
                                else
                                {
					object_type     forge;
					object_type     *q_ptr;
					object_type     *o_ptr;
					
					s16b            o_idx;
                                        int             k_idx = v_ptr->item[*t - '1'] - 1000;

					q_ptr = &forge;
					object_wipe(q_ptr);
					object_prep(q_ptr, k_idx);

                                        apply_magic(q_ptr, v_ptr->lvl, TRUE, TRUE, FALSE);

                                        o_idx = o_pop();
					o_ptr = &o_list[o_idx];
					object_copy(o_ptr,q_ptr);
					o_ptr->iy = y;
					o_ptr->ix = x;
					c_ptr = &cave[y][x];
					o_ptr->next_o_idx = c_ptr->o_idx;
					c_ptr->o_idx = o_idx;

					break;
                                }

                        /* Between gates */
                        case '4':
                        case '5':
                        case '6':
                        case '7':
                        case 'A':
                        case 'B':
                        case 'C':
                        case 'E':
                        case 'F':
                        case '0':
                                /* Not found before */
                                if(bwy[*t - '0'] == 9999)
                                {
                                        cave_set_feat(y, x, FEAT_BETWEEN);
                                        bwy[*t - '0'] = y;
                                        bwx[*t - '0'] = x;
                                }
                                /* The second time */
                                else
                                {
                                        cave_set_feat(y, x, FEAT_BETWEEN);
					c_ptr = &cave[y][x];
                                        c_ptr->special = bwx[*t - '0'] + (bwy[*t - '0'] << 8);
                                        cave[bwy[*t - '0']][bwx[*t - '0']].special = x + (y << 8);
                                }
                                break;
                	}
		}
	}
}


void build_special_level(void)
{
        vault_type    *v_ptr, forge;
	int             i;

        v_ptr = &forge;

        for (i=1;i<max_v_idx;i++)
	{
		v_ptr = &v_info[i];
                if ((v_ptr->lvl == dun_level) && (v_ptr->dun_type == dungeon_type))
			break;
	}
       
	build_special(33,99,v_ptr->hgt,v_ptr->wid,v_text + v_ptr->text);
}

/*
 * Give some magical energy to the each grid of the level
 */
void generate_grid_mana()
{
        int y, x, mana;
        bool xtra_magic = FALSE;

        if(randint(XTRA_MAGIC) == 1) xtra_magic = TRUE;

        if (((cheat_room)||(p_ptr->precognition)) && xtra_magic) msg_print("Magical level");

        for(y = 0; y < cur_hgt; y++)
        {
                for(x = 0; x < cur_wid; x++)
                {
                        cave_type *c_ptr = &cave[y][x];

                        /* Calculate the amount of mana in each grid */
                        mana = (((XTRA_MAGIC)?3:2) * m_bonus(255, dun_level) / 2) + ((XTRA_MAGIC)?10 + rand_int(10):0);

                        /* Never more than 255 or less than 0(parano‹a) */
                        mana = (mana < 0)?0:(mana > 255)?255:mana;

                        c_ptr->mana = mana;
                }
        }
}


/*
 * Generates a random dungeon level			-RAK-
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Hack -- allow auto-scumming via a gameplay option.
 */
void generate_cave(void)
{
	int tester_1, tester_2;
	int y, x, num;
        bool loaded = FALSE;
        char buf[5];

	/* The dungeon is not ready */
	character_dungeon = FALSE;

        /* Allocate and Wipe the to keep monster list */
        C_MAKE(km_list, max_m_idx, monster_type);

        /* Save the imprinted monsters */
        save_all_friends();
        wipe_m_list();

        /* Seed the RNG if appropriate */
        if (seed_dungeon) {
          Rand_quick = TRUE;
          Rand_value = seed_dungeon+dun_level;
        }

        if(get_dungeon_save(buf))
        {
		/* Start with a blank cave */
		for (y = 0; y < MAX_HGT; y++)
		{
			for (x = 0; x < MAX_WID; x++)
			{
				/* No flags */
				cave[y][x].info = 0;

				/* No features */
                                cave[y][x].feat = FEAT_PERM_INNER;

				/* No objects */
				cave[y][x].o_idx = 0;

				/* No monsters */
				cave[y][x].m_idx = 0;

				/* No traps */
				cave[y][x].t_idx = 0;

				/* No mimic */
				cave[y][x].mimic = 0;

                                /* No inscription */
                                cave[y][x].inscription = 0;

#ifdef MONSTER_FLOW
				/* No flow */
				cave[y][x].cost = 0;
				cave[y][x].when = 0;
#endif /* MONSTER_FLOW */
			}
		}
                loaded = load_dungeon(buf);
        }

        if(!loaded)
        {
	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;

		cptr why = NULL;


		/* XXX XXX XXX XXX */
		o_max = 1;

		/* Start with a blank cave */
		for (y = 0; y < MAX_HGT; y++)
		{
			for (x = 0; x < MAX_WID; x++)
			{
				/* No flags */
				cave[y][x].info = 0;

				/* No features */
                                cave[y][x].feat = 0;

				/* No objects */
				cave[y][x].o_idx = 0;

				/* No monsters */
				cave[y][x].m_idx = 0;

				/* No traps */
				cave[y][x].t_idx = 0;

				/* No mimic */
				cave[y][x].mimic = 0;

                                /* No inscription */
                                cave[y][x].inscription = 0;

#ifdef MONSTER_FLOW
				/* No flow */
				cave[y][x].cost = 0;
				cave[y][x].when = 0;
#endif /* MONSTER_FLOW */
			}
		}


		/* Mega-Hack -- no player yet */
                px = py = 0;


		/* Mega-Hack -- no panel yet */
		panel_row_min = 0;
		panel_row_max = 0;
		panel_col_min = 0;
		panel_col_max = 0;


		/* Reset the monster generation level */
		monster_level = dun_level;

		/* Reset the object generation level */
		object_level = dun_level;

		/* Nothing special here yet */
		good_item_flag = FALSE;

		/* Nothing good here yet */
		rating = 0;

		/* No special level here yet */
		special_flag = FALSE;

                /* No ambush here yet */
                ambush_flag = FALSE;

                /* No fated level here yet */
                fate_flag = FALSE;

		/* Build the arena -KMW- */
		if (p_ptr->inside_arena)
		{
			/* Small arena */
			arena_gen();
		}

                /* Build a special level */
                else if (spec_history[(dun_level - d_info[dungeon_type].mindepth < 0)?0:dun_level - d_info[dungeon_type].mindepth][dungeon_type] == 1)
                {
                        int x,y;
                        /* wipe all cave to permanant rock */
     
                        /* Big dungeon */
                        cur_hgt = MAX_HGT;
                        cur_wid = MAX_WID;

                        /* Determine number of panels */
                        max_panel_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
                        max_panel_cols = (cur_wid / SCREEN_WID) * 2 - 2;

                        /* Assume illegal panel */
                        panel_row = max_panel_rows;
                        panel_col = max_panel_cols;
       
                        for (y = 0; y < cur_hgt; y++)
                        {
                                for (x = 0; x < cur_wid; x++)
                                {
                                        cave_type *c_ptr =&cave[y][x];
                                        c_ptr->feat = FEAT_PERM_INNER;
                                }
                        }
                        build_special_level();
                        spec_history[(dun_level - d_info[dungeon_type].mindepth < 0)?0:dun_level - d_info[dungeon_type].mindepth][dungeon_type] = 2;
                        special_flag = TRUE;
                }

		/* Quest levels -KMW- */
		else if (p_ptr->inside_quest)
		{
			quest_gen();
#ifdef USE_PYTHON
                        perform_event(EVENT_ENTER_QUEST, Py_BuildValue("(ii)", p_ptr->inside_quest, dun_level));
#endif
		}

		/* Build the town */
		else if (!dun_level)
		{
                        /* Big wilderness mode */
                        if(!p_ptr->wild_mode)
                        {
                                /* Big town */
                                cur_hgt = MAX_HGT;
                                cur_wid = MAX_WID;

                                /* Determine number of panels */
                                max_panel_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
                                max_panel_cols = (cur_wid / SCREEN_WID) * 2 - 2;

                                /* Assume illegal panel */
                                panel_row = max_panel_rows;
                                panel_col = max_panel_cols;

                                /* Make the wilderness */
                                wilderness_gen(0);

                                okay = TRUE;
                        }
                        /* Small wilderness mode */
                        else
                        {
                                /* Big screen */
                                cur_hgt = MAX_HGT;
                                cur_wid = MAX_WID;

                                /* Determine number of panels */
                                max_panel_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
                                max_panel_cols = (cur_wid / SCREEN_WID) * 2 - 2;

                                /* Assume illegal panel */
                                panel_row = max_panel_rows;
                                panel_col = max_panel_cols;

                                /* Make the wilderness */
                                wilderness_gen_small();

                                okay = TRUE;
                        }
		}

		/* Build a real level */
		else
		{
                        if ((d_info[dungeon_type].flags1 & DF1_SMALLEST) && (!(d_info[dungeon_type].flags1 & DF1_BIG)))
                        {
                                if ((cheat_room)||(p_ptr->precognition))
					msg_print ("A 'small' dungeon level.");

                                cur_hgt = SCREEN_HGT;
                                cur_wid = SCREEN_WID;

				/* Determine number of panels */
                                max_panel_rows = 1;
                                max_panel_cols = 1;

				/* Assume illegal panel */
				panel_row = max_panel_rows;
				panel_col = max_panel_cols;

				if (cheat_room)
                                        msg_format("X:1, Y:1.");
                        }
                        else if ((((randint(SMALL_LEVEL)==1) && small_levels) || always_small_level ||
                            (d_info[dungeon_type].flags1 & DF1_SMALL)) && (!(d_info[dungeon_type].flags1 & DF1_BIG)))
			{
                                if ((cheat_room)||(p_ptr->precognition))
					msg_print ("A 'small' dungeon level.");

				tester_1 = randint(MAX_HGT/SCREEN_HGT);
				tester_2 = randint(MAX_WID/SCREEN_WID);

				cur_hgt = tester_1 * SCREEN_HGT;
				cur_wid = tester_2 * SCREEN_WID;

				/* Determine number of panels */
				max_panel_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
				max_panel_cols = (cur_wid / SCREEN_WID) * 2 - 2;

				/* Assume illegal panel */
				panel_row = max_panel_rows;
				panel_col = max_panel_cols;

				if (cheat_room)
					msg_format("X:%d, Y:%d.", max_panel_cols, max_panel_rows);
			}
			else
			{
				/* Big dungeon */
				cur_hgt = MAX_HGT;
				cur_wid = MAX_WID;

				/* Determine number of panels */
				max_panel_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
				max_panel_cols = (cur_wid / SCREEN_WID) * 2 - 2;

				/* Assume illegal panel */
                                panel_row = max_panel_rows;
				panel_col = max_panel_cols;
			}
			/* Make a dungeon */
                        special_flag = FALSE;
			if (!cave_gen())
			{
				why = "could not place player";
				okay = FALSE;
                        }
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
		if (good_item_flag && !p_ptr->preserve) feeling = 1;

		/* It takes 1000 game turns for "feelings" to recharge */
		if ((turn - old_turn) < 1000) feeling = 0;

		/* Hack -- no feeling in the town */
		if (!dun_level) feeling = 0;


		/* Prevent object over-flow */
		if (o_max >= max_o_idx)
		{
			/* Message */
			why = "too many objects";

			/* Message */
			okay = FALSE;
		}

		/* Prevent monster over-flow */
		if (m_max >= max_m_idx)
		{
			/* Message */
			why = "too many monsters";

			/* Message */
			okay = FALSE;
		}

		/* Mega-Hack -- "auto-scum" */
                if (auto_scum && (num < 100) && !p_ptr->inside_quest && dun_level)
		{
			/* Require "goodness" */
			if ((feeling > 9) ||
			    ((dun_level >= 5) && (feeling > 8)) ||
			    ((dun_level >= 10) && (feeling > 7)) ||
			    ((dun_level >= 20) && (feeling > 6)) ||
			    ((dun_level >= 40) && (feeling > 5)))
			{
				/* Give message to cheaters */
				if (cheat_room || cheat_hear ||
                                    cheat_peek || cheat_xtra || p_ptr->precognition)
				{
					/* Message */
					why = "boring level";
				}

				/* Try again */
				okay = FALSE;
			}
		}
break;
		/* Accept */
		if (okay) break;

		/* Message */
		if (why) msg_format("Generation restarted (%s)", why);

		/* Wipe the objects */
		wipe_o_list();

                /* Wipe the monsters */
                wipe_m_list();
	}

        /* Give some mana to each grid -- DG */
        generate_grid_mana();
        }

        /* Put the kept monsters -- DG */
        if(!p_ptr->wild_mode) replace_all_friends();

        /* HACK -- Reinitialize Druid's Level Drain */
        if(p_ptr->pclass == CLASS_DRUID) p_ptr->class_extra5 = FALSE;

#ifdef USE_PYTHON
        perform_event(EVENT_GENERATE_LVL, Py_BuildValue("(i)", dun_level));
#endif

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Remember when this level was "created" */
	old_turn = turn;

        if (seed_dungeon) {
          Rand_quick = FALSE;
        }

        /* Deallocate and Wipe the to keep monster list */
        C_FREE(km_list, max_m_idx, monster_type);
}

static void build_store(int n, int yy, int xx)
{
	int y, x, y0, x0, y1, x1, y2, x2, tmp;

	int qy = SCREEN_HGT;
	int qx = SCREEN_WID;


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

	int qy = SCREEN_HGT;
	int qx = SCREEN_WID;

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
		y = qy + rand_range(3, SCREEN_HGT - 4);
		x = qx + rand_range(3, SCREEN_WID - 4);

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
void town_gen(void)
{
	int i, y, x;

	int residents;

	int qy = SCREEN_HGT;
	int qx = SCREEN_WID;

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
        for (y = 0; y < MAX_HGT; y++)
	{
                for (x = 0; x < MAX_WID; x++)
		{
			/* Create "solid" perma-wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);
		}
	}

	/* Then place some floors */
	for (y = qy+1; y < qy+SCREEN_HGT-1; y++)
	{
		for (x = qx+1; x < qx+SCREEN_WID-1; x++)
		{
			/* Create empty floor */
			cave_set_feat(y, x, FEAT_FLOOR);
		}
	}

	/* Build stuff */
	town_gen_hack();

	/* Make some residents */
	for (i = 0; i < residents; i++)
	{
		/* Make a resident */
		(void)alloc_monster(3, TRUE);
	}
}
