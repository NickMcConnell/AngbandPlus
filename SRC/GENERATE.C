/* Purpose: Dungeon generation */

/* Code for making, stocking, and populating levels when generated.
 * Additions to level feelings.  Includes rooms of every kind, pits,
 * nests, vaults (inc. interpretation of v_info.txt).  Level feelings
 * and other messages, autoscummer behaviour.  Creation of the quests.
 *
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
#define DUN_UNUSUAL 194 /* Level/chance of unusual room (was 200) */
#define DUN_DEST    18  /* 1/chance of having a destroyed level */
#define SMALL_LEVEL 3   /* 1/chance of smaller size (3)*/
#define EMPTY_LEVEL 15  /* 1/chance of being 'empty' (15)*/
#define LAKE_LEVEL  7   /* 1/chance of being a lake on the level*/
#define DARK_EMPTY  5   /* 1/chance of arena level NOT being lit (2)*/
#define DUN_ARENA   15  /* 1/chance of having an arena level */
#define DUN_CAVERN  30	/* 1/chance of having a cavern level */

#define DUN_OPEN_WATER  25
#define DUN_OPEN_CHAOS  20 /* -GSN-, from Kamband */
#define DUN_OPEN_CHASM  20

/* Number of rooms to attempt (was 50) */
#define DUN_ROOMS_MIN	10
#define DUN_ROOMS_MAX	100

static int dun_rooms;

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND_MIN	 5 /* Chance of random direction (was 10) */
#define DUN_TUN_RND_MAX	20
#define DUN_TUN_CHG_MIN	20 /* Chance of changing direction (was 30) */
#define DUN_TUN_CHG_MAX	60
#define DUN_TUN_CON_MIN 10 /* Chance of extra tunneling (was 15) */
#define DUN_TUN_CON_MAX	40
#define DUN_TUN_PEN_MIN 30 /* Chance of doors at room entrances (was 25) */
#define DUN_TUN_PEN_MAX 70
#define DUN_TUN_JCT_MIN 60 /* Chance of doors at tunnel junctions (was 90) */
#define DUN_TUN_JCT_MAX 90

static int dun_tun_rnd;
static int dun_tun_chg;
static int dun_tun_con;
static int dun_tun_pen;
static int dun_tun_jct;

/*
 * Dungeon streamer generation values
 */
#define DUN_STR_DEN     5       /* Density of streamers */
#define DUN_STR_RNG     2       /* Width of streamers */
#define DUN_STR_MAG     3       /* Number of magma streamers */
#define DUN_STR_MC    120       /* 1/chance of treasure per magma */
#define DUN_STR_QUA     2   	/* Number of quartz streamers */
#define DUN_STR_QC     80       /* 1/chance of treasure per quartz */
#define DUN_STR_WLW     1       /* Width of lava & water streamers -KMW- */
#define DUN_STR_DWLW    8       /* Density of water & lava streams -KMW- */
#define DUN_WAT_DEN    15	/* Density of rivers */
#define DUN_WAT_RNG	2	/* Width of rivers */
#define DUN_STR_WAT	3	/* Max number of rivers */
#define DUN_WAT_CHG    50       /* 1 in 50 chance of junction in river */ 
#define DUN_MOS_DEN	2	/* Density of moss streamers */
#define DUN_MOS_RNG    10	/* Width of moss streamers */
#define DUN_STR_MOS	2	/* Number of moss streamers */


/*
 * Dungeon treausre allocation values
 */
#define DUN_AMT_ROOM    9       /* Amount of objects for rooms */
#define DUN_AMT_ITEM    3       /* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD    3       /* Amount of treasure for rooms/corridors */
#define DUN_AMT_ALTAR   1       /* Amount of altars */

/*
 * Hack -- Dungeon allocation "places"
 */
#define ALLOC_SET_CORR          1       /* Hallway */
#define ALLOC_SET_ROOM          2       /* Room */
#define ALLOC_SET_BOTH          3       /* Anywhere */

/*
 * Hack -- Dungeon allocation "types"
 */
#define ALLOC_TYP_RUBBLE        1       /* Rubble */
#define ALLOC_TYP_TRAP          3       /* Trap */
#define ALLOC_TYP_GOLD          4       /* Gold */
#define ALLOC_TYP_OBJECT        5       /* Object */
#define ALLOC_TYP_ALTAR         6       /* Altar -GSN- */



/*
 * The "size" of a "generation block" in grids
 */
#define BLOCK_HGT       11
#define BLOCK_WID       11

/*
 * Maximum numbers of rooms along each axis (currently 6x6)
 */
#define MAX_ROOMS_ROW   (MAX_HGT / BLOCK_HGT)
#define MAX_ROOMS_COL   (MAX_WID / BLOCK_WID)


/*
 * Bounds on some arrays used in the "dun_data" structure.
 * These bounds are checked, though usually this is a formality.
 */
#define CENT_MAX        100
#define DOOR_MAX        200
#define WALL_MAX        500
#define TUNN_MAX        900


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


static s16b roomdep[] =
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
3};/* 12 =Crypts (22x22) */


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
bool new_player_spot(void)
{
	int     y, x;
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
	int     k = 0;

	if (cave[y+1][x].feat >= FEAT_WALL_EXTRA) k++;
	if (cave[y-1][x].feat >= FEAT_WALL_EXTRA) k++;
	if (cave[y][x+1].feat >= FEAT_WALL_EXTRA) k++;
	if (cave[y][x-1].feat >= FEAT_WALL_EXTRA) k++;

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
}


/*
 * Convert existing terrain type to "down stairs"
 */
static void place_down_stairs(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	if (rand_int(3) == 1)
	c_ptr->feat = FEAT_SHAFT_DOWN;
	else
	c_ptr->feat = FEAT_MORE;
}





/*
 * Place an up/down staircase at given location
 */
static void place_random_stairs(int y, int x)
{
	bool up_stairs = TRUE;
	bool down_stairs = TRUE;


	/* Paranoia */
	if (!cave_clean_bold(y, x)) return;

	/* Town */
	if (!dun_level)
		up_stairs = FALSE;

	/* Ironman */
	if (ironman_downward)
		up_stairs = FALSE;

	/* Bottom */
	if (dun_level >= MAX_DEPTH - 1)
		down_stairs = FALSE;

	/* Quest-level */
	if (is_quest(dun_level) && (dun_level > 1))
		down_stairs = FALSE;

	/* We can't place both */
	if (down_stairs && up_stairs)
	{
		/* Choose a staircase randomly */
		if (rand_int(100) < 50)
			up_stairs = FALSE;
		else
			down_stairs = FALSE;
	}

	/* Place the stairs */
	if (up_stairs)
		place_up_stairs(y, x);
	else if (down_stairs)
		place_down_stairs(y, x);
}

/*
 * Place an altar at the given location
 */
static void place_altar(int y, int x) {

	cave_type *c_ptr = &cave[y][x];

	int alt = rand_int(MAX_GODS);


	if (randint(5)==1)
	  {
	     c_ptr->feat = FEAT_ALTAR_HEAD + alt;
	     if (cheat_room) msg_format ("placed altar no. %d", alt);
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

	/* Closed, locked, or stuck doors (400/1000) */
	else place_closed_door(y, x);
}



/*
 * Place a random type of normal door at the given location.
 */
void place_closed_door(int y, int x)
{
	int tmp;

	/* Choose an object */
	tmp = rand_int(400);

	/* Closed doors (300/400) */
	if (tmp < 300)
	{
		/* Create closed door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);
	}

	/* Locked doors (99/400) */
	else if (tmp < 399)
	{
		/* Create locked door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + randint(7));
	}

	/* Stuck doors (1/400) */
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
	int         y, x, i, j, flag;
	cave_type   *c_ptr;


	if (feat == FEAT_LESS || feat == FEAT_SHAFT_UP)
	{
		/* No up stairs in town or in ironman mode */
		if (ironman_downward || !dun_level) return;
	}
	else if (feat == FEAT_MORE || feat == FEAT_SHAFT_DOWN)
	{
		/* No downstairs on quest levels */
		if ((dun_level > 1) && quest_number(dun_level)) return;

		/* No downstairs at the bottom */
		if (dun_level >= MAX_DEPTH - 1) return;
	}

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

				/* Clear previous contents, add stairs */
				c_ptr->feat = feat;

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
			if (!cave_naked_bold(y, x) &&
			    !((y == py) && (x == px))) continue;

			/* Check for "room" */
			room = (cave[y][x].info & CAVE_ROOM) ? TRUE : FALSE;

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
				msg_print("Warning! Could not place object!");
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
		
		if (!in_bounds(y1+dy+changey,x1+dx+changex))
		{
			changex=0;
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
 * Places water/lava through dungeon.
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

	/* Hack - Save the location as a "room" */
	if (dun->cent_n < CENT_MAX)
	{
		dun->cent[dun->cent_n].y = y2;
		dun->cent[dun->cent_n].x = x2;
		dun->cent_n++;
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
	int             i, tx, ty;
	int             y, x, dir;
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
			if (c_ptr->feat < FEAT_WALL_EXTRA) continue;
			if (c_ptr->feat > FEAT_WALL_SOLID) continue;

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
  * Put trees near a hole in the dungeon roof  (rubble on ground + up stairway)
  * This happens in real world lava tubes.
 */

static void place_trees(int x,int y)
{	
	int i, j;
	/* place trees / rubble in ovalish distribution*/
	for (i=x-3;i<x+4;i++)
	{
		for (j=y-3;j<y+4;j++)
		{/* Want square to be in the circle and accessable.*/
		if (in_bounds(j,i)
			&& (distance(j, i, y, x) < 4)
			&& (!cave_perma_bold(j, i)))
			{/*
			  * Clear previous contents, add feature
			  * The border mainly gets trees, while the center gets rubble */
			if ((distance(j, i, y, x) > 1)||(randint(100)<25))
				{if (randint(100)<75)
					cave[j][i].feat = FEAT_TREES;
					
				}
			else
				cave[j][i].feat = FEAT_RUBBLE;
			
			/* Light area since is open above */
			cave[j][i].info|=CAVE_GLOW;
			}		
		}	
	}
	
	/* No up stairs in ironman mode */
	if ((!ironman_downward)&&(randint(3)==1))
		{
		/* up stair */
		cave[y][x].feat=FEAT_LESS;
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
	if (cheat_room) msg_print("Destroyed Level");

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
						c_ptr->feat = FEAT_FLOOR;
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
			monster_level = base_level + 2;
			(void)place_monster(y, x, TRUE, TRUE);
			monster_level = base_level;
		}
	}
}


/*
 * This funtion makes a very small room centred at (x0,y0)
 * This is used in crypts, and random elemental vaults.
 *
 * Note - this should be used only on allocated regions
 * within another room.
 */

static void microroom(int x0, int y0)
{
	int x, y;

	for (y = y0 - 1; y <= y0 + 1; y++)
	{
		cave[y][x0 - 1].feat = FEAT_WALL_INNER;
		cave[y][x0 + 1].feat = FEAT_WALL_INNER;
	}

	for (x = x0 - 1; x <= x0 + 1; x++)
	{
		cave[y0 - 1][x].feat = FEAT_WALL_INNER;
		cave[y0 + 1][x].feat = FEAT_WALL_INNER;
	}

	/* Place a secret door on one side */
	switch (rand_int(4))
	{
		case 0: place_secret_door(y0, x0 - 1); break;
		case 1: place_secret_door(y0, x0 + 1); break;
		case 2: place_secret_door(y0 - 1, x0); break;
		case 3: place_secret_door(y0 + 1, x0); break;
	}

	/* Add inner open space */
	cave[y0][x0].feat = FEAT_FLOOR;
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
	if (cave[y][x].feat==FEAT_FLOOR) return(TRUE);
	
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
	if (cave[y][x].info&CAVE_ROOM)
	{
		/*A room border don't touch */
		return;
	}
	
	/*Set to be floor if is a wall (don't touch lakes)*/
	if (cave[y][x].feat==FEAT_WALL_EXTRA)
		cave[y][x].feat=FEAT_FLOOR;
}



/*
 * This function tunnels around a room if
 * it will cut off part of a cave system.
 */
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
	* If so, fix by tunneling outside the room in such a way as to connect the caves.
	*/
	
 	check_room_boundary(*xx - x / 2 - 1, *yy - y / 2 - 1,
 	                    *xx + (x - 1) / 2 + 1, *yy + (y - 1) / 2 + 1);
        
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
 *   9 -- fractal caves
 *  10 -- random vaults
 *  11 -- circular rooms
 *  12 -- crypts
 */


/*
 * Type 1 -- normal rectangular rooms
 */
static void build_type1 (int by0, int bx0)
{
	int y, x, y2, x2, yval, xval;
	int y1, x1, xsize, ysize;

	bool light;

	cave_type *c_ptr;
	
	/* Pick a room size */
	y1 =randint(4);
	x1 =randint(11);
	y2 =randint(3);
	x2 =randint(11);
	
	xsize=x1+x2+1;
	ysize=y1+y2+1;

	/* Try to allocate space for room */
	if (!room_alloc(xsize+2,ysize+2,FALSE,by0,bx0,&xval,&yval)) return;

	/* Choose lite or dark */
	light = (dun_level <= randint(25));

	/* Get corner values */
	y1 = yval - ysize/2;
	x1 = xval - xsize/2;
	y2 = yval + (ysize-1)/2;
	x2 = xval + (xsize-1)/2;

	/* Place a full floor under the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			c_ptr = &cave[y][x];
			c_ptr->feat = FEAT_FLOOR;
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}

	/* Walls around the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y][x2+1];
		c_ptr->feat = FEAT_WALL_OUTER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y2+1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
	}

	/* Hack -- Occasional pillar room */
	if (rand_int(20) == 0)
	{
		for (y = y1; y <= y2; y += 2)
		{
			for (x = x1; x <= x2; x += 2)
			{
				c_ptr = &cave[y][x];
				c_ptr->feat = FEAT_WALL_INNER;
			}
		}
	}

	/* Hack -- Occasional room with four pillars */
	else if (rand_int(20) == 0)
	{
		if ((y1 + 4 < y2) && (x1 + 4 < x2))
		{
			c_ptr = &cave[y1 + 1][x1 + 1];
			c_ptr->feat = FEAT_WALL_INNER;

			c_ptr = &cave[y1 + 1][x2 - 1];
			c_ptr->feat = FEAT_WALL_INNER;

			c_ptr = &cave[y2 - 1][x1 + 1];
			c_ptr->feat = FEAT_WALL_INNER;

			c_ptr = &cave[y2 - 1][x2 - 1];
			c_ptr->feat = FEAT_WALL_INNER;
		}
	}

	/* Hack -- Occasional ragged-edge room */
	else if (rand_int(50) == 0)
	{
		for (y = y1 + 2; y <= y2 - 2; y += 2)
		{
			c_ptr = &cave[y][x1];
			c_ptr->feat = FEAT_WALL_INNER;
			c_ptr = &cave[y][x2];
			c_ptr->feat = FEAT_WALL_INNER;
		}
		for (x = x1 + 2; x <= x2 - 2; x += 2)
		{
			c_ptr = &cave[y1][x];
			c_ptr->feat = FEAT_WALL_INNER;
			c_ptr = &cave[y2][x];
			c_ptr->feat = FEAT_WALL_INNER;
		}
	}
 	/* Hack -- Occasional divided room */
 	else if (rand_int(50) == 0)
 	{
 		if (randint(100)<50)
 		{
 			/* horizontal wall */
 			for (x=x1;x<=x2;x++)
 			{
 				cave[yval][x].feat=FEAT_WALL_INNER;
 			}
 			
 			/* Prevent edge of wall from being tunneled */
 			cave[yval][x1-1].feat=FEAT_WALL_SOLID;  
 			cave[yval][x2+1].feat=FEAT_WALL_SOLID;
 		}
 		else
 		{
 			/*vertical wall */
 			for (y=y1;y<=y2;y++)
 			{
 				cave[y][xval].feat=FEAT_WALL_INNER;
 			}
 			  
 			/* Prevent edge of wall from being tunneled */
			cave[y1-1][xval].feat=FEAT_WALL_SOLID;
			cave[y2+1][xval].feat=FEAT_WALL_SOLID;
 		}
 		place_random_door(yval,xval);
	}
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static void build_type2(int by0, int bx0)
{
	int			y, x, xval, yval;
	int                     y1a, x1a, y2a, x2a;
	int                     y1b, x1b, y2b, x2b;

	bool            light;

	cave_type *c_ptr;


	/* Try to allocate space for room */
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
			c_ptr->feat = FEAT_FLOOR;
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
			c_ptr->feat = FEAT_FLOOR;
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}


	/* Place the walls around room "a" */
	for (y = y1a - 1; y <= y2a + 1; y++)
	{
		c_ptr = &cave[y][x1a-1];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y][x2a+1];
		c_ptr->feat = FEAT_WALL_OUTER;
	}
	for (x = x1a - 1; x <= x2a + 1; x++)
	{
		c_ptr = &cave[y1a-1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y2a+1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
	}

	/* Place the walls around room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		c_ptr = &cave[y][x1b-1];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y][x2b+1];
		c_ptr->feat = FEAT_WALL_OUTER;
	}
	for (x = x1b - 1; x <= x2b + 1; x++)
	{
		c_ptr = &cave[y1b-1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y2b+1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
	}



	/* Replace the floor for room "a" */
	for (y = y1a; y <= y2a; y++)
	{
		for (x = x1a; x <= x2a; x++)
		{
			c_ptr = &cave[y][x];
			c_ptr->feat = FEAT_FLOOR;
		}
	}

	/* Replace the floor for room "b" */
	for (y = y1b; y <= y2b; y++)
	{
		for (x = x1b; x <= x2b; x++)
		{
			c_ptr = &cave[y][x];
			c_ptr->feat = FEAT_FLOOR;
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
	int                     y, x, dy, dx, wy, wx;
	int                     y1a, x1a, y2a, x2a;
	int                     y1b, x1b, y2b, x2b;
	int			yval, xval;

	bool            light;

	cave_type *c_ptr;

	/* Try to allocate space for room */
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
			c_ptr->feat = FEAT_FLOOR;
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
			c_ptr->feat = FEAT_FLOOR;
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}


	/* Place the walls around room "a" */
	for (y = y1a - 1; y <= y2a + 1; y++)
	{
		c_ptr = &cave[y][x1a-1];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y][x2a+1];
		c_ptr->feat = FEAT_WALL_OUTER;
	}
	for (x = x1a - 1; x <= x2a + 1; x++)
	{
		c_ptr = &cave[y1a-1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y2a+1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
	}

	/* Place the walls around room "b" */
	for (y = y1b - 1; y <= y2b + 1; y++)
	{
		c_ptr = &cave[y][x1b-1];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y][x2b+1];
		c_ptr->feat = FEAT_WALL_OUTER;
	}
	for (x = x1b - 1; x <= x2b + 1; x++)
	{
		c_ptr = &cave[y1b-1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y2b+1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
	}


	/* Replace the floor for room "a" */
	for (y = y1a; y <= y2a; y++)
	{
		for (x = x1a; x <= x2a; x++)
		{
			c_ptr = &cave[y][x];
			c_ptr->feat = FEAT_FLOOR;
		}
	}

	/* Replace the floor for room "b" */
	for (y = y1b; y <= y2b; y++)
	{
		for (x = x1b; x <= x2b; x++)
		{
			c_ptr = &cave[y][x];
			c_ptr->feat = FEAT_FLOOR;
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
					c_ptr->feat = FEAT_WALL_INNER;
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
				c_ptr->feat = FEAT_WALL_INNER;
				c_ptr = &cave[y][x2a];
				c_ptr->feat = FEAT_WALL_INNER;
			}
			for (x = x1a; x <= x2a; x++)
			{
				c_ptr = &cave[y1b][x];
				c_ptr->feat = FEAT_WALL_INNER;
				c_ptr = &cave[y2b][x];
				c_ptr->feat = FEAT_WALL_INNER;
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
					c_ptr->feat = FEAT_WALL_INNER;
					c_ptr = &cave[y][x2a + 1];
					c_ptr->feat = FEAT_WALL_INNER;
				}

				/* Pinch the north/south sides */
				for (x = x1a; x <= x2a; x++)
				{
					if (x == xval) continue;
					c_ptr = &cave[y1b - 1][x];
					c_ptr->feat = FEAT_WALL_INNER;
					c_ptr = &cave[y2b + 1][x];
					c_ptr->feat = FEAT_WALL_INNER;
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
				c_ptr->feat = FEAT_WALL_INNER;
				c_ptr = &cave[y1b][xval];
				c_ptr->feat = FEAT_WALL_INNER;
				c_ptr = &cave[y2b][xval];
				c_ptr->feat = FEAT_WALL_INNER;
				c_ptr = &cave[yval][x1a];
				c_ptr->feat = FEAT_WALL_INNER;
				c_ptr = &cave[yval][x2a];
				c_ptr->feat = FEAT_WALL_INNER;
			}

			/* Occasionally put a pillar in the center */
			else if (rand_int(3) == 0)
			{
				c_ptr = &cave[yval][xval];
				c_ptr->feat = FEAT_WALL_INNER;
			}

			break;
		}
	}
}


/*
 * Type 4 -- Large room with inner features
 *
 * Possible sub-types:
 *      1 - Just an inner room with one door
 *      2 - An inner room within an inner room
 *      3 - An inner room with pillar(s)
 *      4 - Inner room has a maze
 *      5 - A set of four inner rooms
 */
static void build_type4(int by0, int bx0)
{
	int y, x, y1, x1;
	int y2, x2, tmp, yval, xval;

	bool light;

	cave_type *c_ptr;

	/*Try to allocate space for room */
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
			c_ptr->feat = FEAT_FLOOR;
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}

	/* Outer Walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y][x2+1];
		c_ptr->feat = FEAT_WALL_OUTER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y2+1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
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
		c_ptr->feat = FEAT_WALL_INNER;
		c_ptr = &cave[y][x2+1];
		c_ptr->feat = FEAT_WALL_INNER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
		c_ptr->feat = FEAT_WALL_INNER;
		c_ptr = &cave[y2+1][x];
		c_ptr->feat = FEAT_WALL_INNER;
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
				c_ptr->feat = FEAT_WALL_INNER;
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
				c_ptr->feat = FEAT_WALL_INNER;
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
					c_ptr->feat = FEAT_WALL_INNER;
				}
				for (x = xval + 3 + tmp; x <= xval + 5 + tmp; x++)
				{
					c_ptr = &cave[y][x];
					c_ptr->feat = FEAT_WALL_INNER;
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
				c_ptr->feat = FEAT_WALL_INNER;
				c_ptr = &cave[yval+1][x];
				c_ptr->feat = FEAT_WALL_INNER;
			}

			/* Close off the left/right edges */
			c_ptr = &cave[yval][xval-5];
			c_ptr->feat = FEAT_WALL_INNER;
			c_ptr = &cave[yval][xval+5];
			c_ptr->feat = FEAT_WALL_INNER;

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
					c_ptr->feat = FEAT_WALL_INNER;
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
			c_ptr->feat = FEAT_WALL_INNER;
		}
		for (x = x1; x <= x2; x++)
		{
			c_ptr = &cave[yval][x];
			c_ptr->feat = FEAT_WALL_INNER;
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

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

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

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

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

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require Undead */
	if (!(r_ptr->flags3 & (RF3_UNDEAD))) return (FALSE);

	/* No pet ghosts there */
	if ((r_ptr->flags7) & (RF7_PET)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster nest (chapel)"
 */
static bool vault_aux_chapel(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

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

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

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

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require mimics, coins, animated weapons... */
	if (!((r_ptr->d_char == '!') || (r_ptr->d_char == '|') ||
		(r_ptr->d_char == '$') || (r_ptr->d_char == '?') ||
		(r_ptr->d_char == '(') || (r_ptr->d_char == '/') ||
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
	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	return (r_idx == template_race);
}


/*
 * Helper function for "monster nest (symbol clone)"
 */
static bool vault_aux_symbol(int r_idx)
{
	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	return ((r_info[r_idx].d_char == (r_info[template_race].d_char))
		&& !(r_info[r_idx].flags1 & RF1_UNIQUE));
}


/*
 * Helper function for "monster pit (orc)"
 */
static bool vault_aux_orc(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	/* Decline unique monsters */
	if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

	/* Hack -- Require Orcs */
	if (!(r_ptr->flags3 & RF3_ORC)) return (FALSE);

	/* Okay */
	return (TRUE);
}



/*
 * Helper function for "monster pit (troll)"
 */
static bool vault_aux_troll(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Decline sea trolls */
	if (r_ptr->flags7 & (RF7_AQUATIC)) return (FALSE);

	/* Hack -- Require Trolls */
	if (!(r_ptr->flags3 & RF3_TROLL)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (giant)"
 */
static bool vault_aux_giant(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Hack -- Require Giants */
	if (!(r_ptr->flags3 & RF3_GIANT)) return (FALSE);

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

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require dragon */
	if (!(r_ptr->flags3 & RF3_DRAGON)) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

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

	/* Decline town monsters */
	if (!monster_dungeon(r_idx)) return FALSE;

	/* Decline unique monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Require demon */
	if (!(r_ptr->flags3 & RF3_DEMON)) return (FALSE);

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
	int	        y, x, y1, x1, y2, x2, xval, yval;

	int             tmp, i;

	s16b            what[64];

	cave_type       *c_ptr;

	cptr            name;

	bool            empty = FALSE;

	monster_hook_type vault_monster_hook;

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
			c_ptr->feat = FEAT_FLOOR;
			c_ptr->info |= (CAVE_ROOM);
		}
	}

	/* Place the outer walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y][x2+1];
		c_ptr->feat = FEAT_WALL_OUTER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y2+1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
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
		c_ptr->feat = FEAT_WALL_INNER;
		c_ptr = &cave[y][x2+1];
		c_ptr->feat = FEAT_WALL_INNER;
	}

	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
		c_ptr->feat = FEAT_WALL_INNER;
		c_ptr = &cave[y2+1][x];
		c_ptr->feat = FEAT_WALL_INNER;
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

	if ((tmp < 25) && (randint(2) != 1))
	{
		do
		{
			template_race = randint(max_r_idx - 1);
		}
		while ((r_info[template_race].flags1 & RF1_UNIQUE) ||
		       ((r_info[template_race].level + randint(5)) > (dun_level + randint(5))));

		if ((randint(2) != 1) && (dun_level >= (25 + randint(15))))
		{
			name = "symbol clone";
			vault_monster_hook = vault_aux_symbol;
		}
		else
		{
			name = "clone";
			vault_monster_hook = vault_aux_clone;
		}
	}

	/* Monster nest (jelly) */
        else if (tmp < 25)
	{
		/* Describe */
		name = "jelly";

		/* Restrict to jelly */
		vault_monster_hook = vault_aux_jelly;
	}

	else if (tmp < 50)
	{
		name = "treasure";
		vault_monster_hook = vault_aux_treasure;
	}

	/* Monster nest (animal) */
	else if (tmp < 65)
	{
		if (randint(3) == 1)
		{
			name = "kennel";
			vault_monster_hook = vault_aux_kennel;
		}
		else
		{
			/* Describe */
			name = "animal";

			/* Restrict to animal */
			vault_monster_hook = vault_aux_animal;
		}
	}

	/* Monster nest (undead) */
	else
	{
		if (randint(3)==1)
		{
			name = "chapel";
			vault_monster_hook = vault_aux_chapel;
		}
		else
		{
			/* Describe */
			name = "undead";

			/* Restrict to undead */
			vault_monster_hook = vault_aux_undead;
		}
	}


	/* Prepare allocation table */
	get_mon_num_prep(vault_monster_hook, NULL);


	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(dun_level + 10);

		/* Notice failure */
		if (!what[i]) empty = TRUE;
	}


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
                (void)place_monster_aux(y, x, r_idx, FALSE, FALSE, FALSE, FALSE);
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
static void build_type6(int by0, int bx0)
{
	int         tmp, what[16];
	int         i, j, y, x, y1, x1, y2, x2, xval, yval;
	bool        empty = FALSE;
	cave_type   *c_ptr;
	cptr        name;
	monster_hook_type vault_monster_hook;

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
			c_ptr->feat = FEAT_FLOOR;
			c_ptr->info |= (CAVE_ROOM);
		}
	}

	/* Place the outer walls */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		c_ptr = &cave[y][x1-1];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y][x2+1];
		c_ptr->feat = FEAT_WALL_OUTER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
		c_ptr = &cave[y2+1][x];
		c_ptr->feat = FEAT_WALL_OUTER;
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
		c_ptr->feat = FEAT_WALL_INNER;
		c_ptr = &cave[y][x2+1];
		c_ptr->feat = FEAT_WALL_INNER;
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		c_ptr = &cave[y1-1][x];
		c_ptr->feat = FEAT_WALL_INNER;
		c_ptr = &cave[y2+1][x];
		c_ptr->feat = FEAT_WALL_INNER;
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

	/* Orc pit */
	if (tmp < 20)
	{
		/* Message */
		name = "orc";

		/* Restrict monster selection */
		vault_monster_hook = vault_aux_orc;
	}

	/* Troll pit */
	else if (tmp < 40)
	{
		/* Message */
		name = "troll";

		/* Restrict monster selection */
		vault_monster_hook = vault_aux_troll;
	}

	/* Giant pit */
	else if (tmp < 55)
	{
		/* Message */
		name = "giant";

		/* Restrict monster selection */
		vault_monster_hook = vault_aux_giant;
	}

	else if (tmp < 70)
	{
		if (randint(4) != 1)
		{
			/* Message */
			name = "ordered clones";

			do
			{
				template_race = randint(max_r_idx - 1);
			}
			while ((r_info[template_race].flags1 & RF1_UNIQUE)
			  || (((r_info[template_race].level) + randint(5)) > (dun_level + randint(5))));

			/* Restrict selection */
			vault_monster_hook = vault_aux_symbol;
		}
		else
		{
			name = "ordered chapel";
			vault_monster_hook = vault_aux_chapel;
		}
        }


	/* Dragon pit */
	else if (tmp < 80)
	{
		/* Pick dragon type */
		switch (rand_int(9))
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

			/* Bronze */
			case 5:
			{
				/* Message */
				name = "confusion dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_CONF;

				/* Done */
				break;
			}

			/* Gold */
			case 6:
			{
				/* Message */
				name = "sound dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = RF4_BR_SOUN;

				/* Done */
				break;
			}

			/* Pseudo/Ethereal */
			case 7:
			{
				/* Message */
				name = "light/dark dragon";

				/* Restrict dragon breath type */
				vault_aux_dragon_mask4 = (RF4_BR_LITE | RF4_BR_DARK);

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
		vault_monster_hook = vault_aux_dragon;
	}

	/* Demon pit */
	else
	{
		/* Message */
		name = "demon";

		/* Restrict monster selection */
		vault_monster_hook = vault_aux_demon;
	}

	/* Prepare allocation table */
	get_mon_num_prep(vault_monster_hook, NULL);


	/* Pick some monster types */
	for (i = 0; i < 16; i++)
	{
		/* Get a (hard) monster type */
		what[i] = get_mon_num(dun_level + 10);

		/* Notice failure */
		if (!what[i]) empty = TRUE;
	}


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
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster pit (%s)", name);

	if (cheat_hear)
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
		place_monster_aux(yval - 2, x, what[0], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(yval + 2, x, what[0], FALSE, FALSE, FALSE, FALSE);
	}

	/* Middle columns */
	for (y = yval - 1; y <= yval + 1; y++)
	{
		place_monster_aux(y, xval - 9, what[0], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 9, what[0], FALSE, FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 8, what[1], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 8, what[1], FALSE, FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 7, what[1], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 7, what[1], FALSE, FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 6, what[2], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 6, what[2], FALSE, FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 5, what[2], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 5, what[2], FALSE, FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 4, what[3], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 4, what[3], FALSE, FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 3, what[3], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 3, what[3], FALSE, FALSE, FALSE, FALSE);

		place_monster_aux(y, xval - 2, what[4], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(y, xval + 2, what[4], FALSE, FALSE, FALSE, FALSE);
	}

	/* Above/Below the center monster */
	for (x = xval - 1; x <= xval + 1; x++)
	{
		place_monster_aux(yval + 1, x, what[5], FALSE, FALSE, FALSE, FALSE);
		place_monster_aux(yval - 1, x, what[5], FALSE, FALSE, FALSE, FALSE);
	}

	/* Next to the center monster */
	place_monster_aux(yval, xval + 1, what[6], FALSE, FALSE, FALSE, FALSE);
	place_monster_aux(yval, xval - 1, what[6], FALSE, FALSE, FALSE, FALSE);

	/* Center monster */
	place_monster_aux(yval, xval, what[7], FALSE, FALSE, FALSE, FALSE);

	if (seed_dungeon) Rand_quick = TRUE;

}


/* coordinate translation code */
static void coord_trans(int *x, int *y, int xoffset, int yoffset, int transno)
{
	int i;
	int temp;
	
	/*
	* transno specifies what transformation is required. (0-7)
	* The lower two bits indicate by how much the vault is rotated,
	* and the upper bit indicates a reflection.
	* This is done by using rotation matrices... however since
	* these are mostly zeros for rotations by 90 degrees this can
	* be expressed simply in terms of swapping and inverting the
	* x and y coordinates.
	*/
	
	for (i=0;i<=transno%4;i++)
	{
		/* rotate by 90 degrees */
		temp=*x;
		*x= -(*y);
		*y=temp;	
	}
	
	if (transno/4)
	{
		/* Reflect depending on status of 3rd bit. */
		*x= -(*x);
	}

	/* Add offsets so vault stays in the first quadrant */
	*x += xoffset;
	*y += yoffset;
}


/*
 * Hack -- fill in "vault" rooms
 */
static void build_vault(int yval, int xval, int ymax, int xmax, cptr data,
		int xoffset, int yoffset, int transno, int vaultnum)
{
	int dx, dy, x, y, i, j;

	cptr t;

	cave_type *c_ptr;
	vault_type *v_ptr;

	v_ptr = &v_info[vaultnum];
	
	/* Vaults are different even in persistent dungeons. */

	if (seed_dungeon) Rand_quick = FALSE;

	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/*prevent loop counter from being overwritten*/
			i=dx;
        		j=dy;
			
			/* Flip / rotate*/
			coord_trans(&i, &j, xoffset, yoffset, transno);
			
			if (transno%2)
			{
				/* no swap of x/y*/
				x = xval - (xmax / 2) + i;
				y = yval - (ymax / 2) + j;
			}
			else
			{
				/* swap of x/y*/
				x = xval - (ymax / 2) + i;
				y = yval - (xmax / 2) + j;
			}

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Lay down a floor */
			c_ptr->feat = FEAT_FLOOR;

			/* Part of a vault */
			c_ptr->info |= (CAVE_ROOM | CAVE_ICKY);

			/* Analyze the grid */
			switch (*t)
			{
				/* Granite wall (outer) */
			case '%':
				c_ptr->feat = FEAT_WALL_OUTER;
				break;

				/* Granite wall (inner) */
			case '#':
				c_ptr->feat = FEAT_WALL_INNER;
				break;

				/* Permanent wall (inner) */
			case 'X':
				c_ptr->feat = FEAT_PERM_INNER;
				break;

			/* Chaos fog & co. */

			case 'F':
				c_ptr->feat = FEAT_CHAOS_FOG;
				break;

			case 'K':
				c_ptr->feat = FEAT_SHAL_LAVA;
				break;
				
			case 'L':
				c_ptr->feat = FEAT_DEEP_LAVA;
				break;
				
			case 'V':
				c_ptr->feat = FEAT_SHAL_WATER;
				break;
				
			case 'W':
				c_ptr->feat = FEAT_DEEP_WATER;
				break;

			/* Glyph of warding */
			case ';':

			/* Always permanent */
				c_ptr->feat = FEAT_GLYPH;
				break;

			/* Trees */
			case 'Y':
				c_ptr->feat = FEAT_TREES;
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
				
			case 'D':
				place_locked_door(y, x);
				break;

				/* Trap */
			case '^':
				place_trap(y, x);
				break;
			}
		}
	}


	/* Place dungeon monsters and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* Prevent loop counter from being overwritten */
			i=dx;
			j=dy;
			
			/* Flip / rotate */
			coord_trans(&i, &j, xoffset, yoffset, transno);
			
			/* Extract the location */
			if (transno%2)
			{
				/* no swap of x/y*/
				x = xval - (xmax / 2) + i;
				y = yval - (ymax / 2) + j;
			}
			else
			{
				/* swap of x/y*/
				x = xval - (ymax / 2) + i;
				y = yval - (xmax / 2) + j;
			}

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Analyze the symbol */
			switch (*t)
			{
				/* Monster -KMW- */
				case 'a':
				case 'b':
				case 'c':
				case 'd':
				case 'e':
				case 'f':
				case 'g':
				case 'h':
				case 'i':
				case 'j':
				{
					if (*t == 'a') i = v_ptr->mon[0];
					else if (*t == 'b') i = v_ptr->mon[1];
					else if (*t == 'c') i = v_ptr->mon[2];
					else if (*t == 'd') i = v_ptr->mon[3];
					else if (*t == 'e') i = v_ptr->mon[4];
					else if (*t == 'f') i = v_ptr->mon[5];
					else if (*t == 'g') i = v_ptr->mon[6];
					else if (*t == 'h') i = v_ptr->mon[7];
					else if (*t == 'i') i = v_ptr->mon[8];
					else if (*t == 'j') i = v_ptr->mon[9];
					
					place_monster_aux (y, x, i, FALSE, FALSE, FALSE, FALSE);
					break;
				}
			
				/* Monster */
				case '&':
				{
					monster_level = base_level + 5;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					break;
				}

				/* Meaner monster */
				case '@':
				{
					monster_level = base_level + 11;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					break;
				}

				/* Meaner monster, plus treasure */
				case '9':
				{
					monster_level = base_level + 9;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					object_level = base_level + 7;
					place_object(y, x, TRUE, FALSE);
					object_level = base_level;
					break;
				}

				/* Nasty monster and treasure */
				case '8':
				{
					monster_level = base_level + 40;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					object_level = base_level + 20;
					place_object(y, x, TRUE, TRUE);
					object_level = base_level;
					break;
				}

				/* Monster and/or object */
				case ',':
				{
					if (rand_int(100) < 50)
					{
						monster_level = base_level + 3;
						place_monster(y, x, TRUE, TRUE);
						monster_level = base_level;
					}
					if (rand_int(100) < 50)
					{
						object_level = base_level + 7;
						place_object(y, x, FALSE, FALSE);
						object_level = base_level;
					}
					break;
				}

				case 'A':
				{
					object_level = base_level + 12;
					place_object(y, x, TRUE, FALSE);
					object_level = base_level;
				}
					break;
			}
		}
	}

	if (seed_dungeon) Rand_quick = TRUE;

}


/*
 * Type 7 -- simple vaults (see "v_info.txt")
 */
static void build_type7(int by0, int bx0)
{
	vault_type      *v_ptr;
	int dummy = 0,xval,yval;
	int x = 0, y = 0, num;
	int transno;
	int xoffset = 0, yoffset = 0;
        
	/* Pick a lesser vault */
	while (dummy < SAFE_MAX_ATTEMPTS)
	{
		dummy++;

		if (debug_vaults)
		{
		    num = get_quantity("Create which lesser vault? ", max_v_idx);
		    if (num <= 0) num = 1;
		    if (num > max_v_idx) num = max_v_idx;
		    v_ptr = &v_info[num];
		}        
		else num = rand_int(max_v_idx);
		
		/* Access a random vault record */
		v_ptr = &v_info[num];

		/* Accept the first lesser vault */
		if ((v_ptr->typ == 7) && (v_ptr->min_lev < dun_level + 1) && 
			(v_ptr->max_lev > dun_level - 1)) break;
		else if (debug_vaults) msg_print("This is not a lesser vault. Try again");
	}

	/* pick type of transformation (0-7)*/
	transno=rand_int(8);

	/* calculate offsets */
	x = v_ptr->wid;
	y = v_ptr->hgt;

	coord_trans(&x,&y,0,0,transno);
	
	if (x<0)
	{
		xoffset= -x - 1;
	}
	else
	{
		xoffset = 0;
	}

	if (y<0)
	{
		yoffset= -y - 1;
	}
	else
	{
		yoffset = 0;
	}


	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(abs(x), abs(y),FALSE, by0, bx0, &xval, &yval))
	{
	   if (debug_vaults) msg_print("Could not allocate this vault here");
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

	/* Message */
	if (cheat_room) msg_format("Lesser Vault (%s)", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((dun_level <= 50) ||
		(randint((dun_level-40) * (dun_level-40) + 50) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, xoffset, yoffset, transno, num);
}



/*
 * Type 8 -- greater vaults (see "v_info.txt")
 */
static void build_type8(int by0, int bx0)
{
	vault_type   *v_ptr;
	int dummy = 0, xval, yval;
	int x = 0, y = 0, num;
	int transno;
	int xoffset, yoffset;

	/* Pick a greater vault */
	while (dummy < SAFE_MAX_ATTEMPTS)
	{
		dummy++;

		if (debug_vaults)
		{
		    num = get_quantity("Create which greater vault? ", max_v_idx);
		    if (num <= 0) num = 1;
		    if (num > max_v_idx) num = max_v_idx;
		    v_ptr = &v_info[num];
		}        
		else num = rand_int(max_v_idx);

		/* Access a random vault record */
		v_ptr = &v_info[num];

		/* Accept the first greater vault */
		if ((v_ptr->typ == 8) && (v_ptr->min_lev < dun_level + 1) && 
			(v_ptr->max_lev > dun_level)) break;
		else if (debug_vaults) msg_print("This is not a greater vault. Try again");
	}

	/* pick type of transformation (0-7)*/
	transno=rand_int(8);
	
	/* calculate offsets */
	x=v_ptr->wid;
	y=v_ptr->hgt;
	
	coord_trans(&x,&y,0,0,transno);
	
	if (x<0)
	{
		xoffset= -x-1;
	}
	else
	{
		xoffset=0;
	}
	
	if (y<0)
	{
		yoffset= -y-1;
	}
	else
	{
		yoffset=0;
	}

	/*Try to allocate space for room.  If fails, exit*/
	if (!room_alloc(abs(x), abs(y), FALSE, by0, bx0, &xval, &yval))
	{
	   if (debug_vaults) msg_print("Could not allocate this vault here");
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


	/* Message */
	if (cheat_room) msg_format("Greater Vault (%s)", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((dun_level <= 50) ||
	(randint((dun_level-40) * (dun_level-40) + 50) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text, xoffset, yoffset, transno, num);
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

 	/*
 	 * Fill in the rectangle with fractal height data -
 	 * like the 'plasma fractal' in fractint.
         */
	while ((xstep/256>1)||(ystep/256>1))
	{
		/* Halve the step sizes*/
		xstep=xhstep;
		xhstep/=2;
		ystep=yhstep;
		yhstep/=2;				
		
		/* Middle top to bottom.*/
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
  			                (cave[y0 - yhsize + j / 256][x0 - xhsize + (i - xhstep) / 256].feat +
 					cave[y0 - yhsize + j / 256][x0 - xhsize + (i + xhstep) / 256].feat) / 2 +
 					(randint(xstep / 256) - xhstep / 256) * roug / 16, xhsize, yhsize, cutoff);
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

static bool hack_isnt_wall(int y, int x, int c1, int c2, int c3, int feat1, int feat2, int feat3)
{
/* function used to convert from height-map back to the
 *  normal angband cave format
 */
	if (cave[y][x].info&CAVE_ICKY) 
	{
		/* already done */
		return FALSE;	
	}
	else
	{
		/* Show that have looked at this square */
		cave[y][x].info|= (CAVE_ICKY); 
		/*Use cutoffs c1-c3 to allocate regions of floor /water/ lava etc. */
		if(cave[y][x].feat<=c1) 
		{
			
			/* 25% of the time use the other tile; it looks better this way*/
			if (randint(100)<75)
			{
				cave[y][x].feat=feat1; 
				return TRUE;
			}
			else
			{
				cave[y][x].feat=feat2; 
				return TRUE;
			}
		}
		else if(cave[y][x].feat<=c2) 
		{
			/* 25% of the time use the other tile : it looks better this way*/
			if (randint(100)<75)
			{
				cave[y][x].feat=feat2; 
				return TRUE;
			}
			else
			{
				cave[y][x].feat=feat1; 
				return TRUE;
			}
		}
		else if(cave[y][x].feat<=c3) 
		{
			cave[y][x].feat=feat3; 
			return TRUE;
		}
		/*if greater than cutoff then is a wall */
		else
		{
			cave[y][x].feat=FEAT_WALL_OUTER; 
			return FALSE;
		}
	}
}

 /*
  * Quick and nasty fill routine used to find the connected region
  * of floor in the middle of the cave
  */

static void fill_hack(int y0, int x0, int y, int x, int xsize, int ysize, 
                      int c1, int c2, int c3, int feat1, int feat2, int feat3, 
                      int *amount)
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
				if (hack_isnt_wall(y+j+y0-ysize/2,x+i+x0-xsize/2,
				      c1,c2,c3,feat1,feat2,feat3))			
				{
					/* then fill from the new point*/
					fill_hack(y0,x0,y+j,x+i,xsize,ysize,
						c1,c2,c3,feat1,feat2,feat3,amount); 
					
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
	
       /* Select region connected to center of cave system
	* this gets rid of alot of isolated one-squares that
	* can make teleport traps instadeaths... */
	fill_hack(y0,x0,yhsize,xhsize,xsize,ysize,cutoff,0,0,FEAT_FLOOR,FEAT_FLOOR,FEAT_FLOOR,&amount);
	
	/* if tally too small, try again*/
	if (amount<10) 
	{
		/*too small -clear area and try again later*/
		for(x=0;x<=xsize;++x)
		{
			for(y=0;y<=ysize;++y)
			{
				cave[y0+y-yhsize][x0+x-xhsize].feat=FEAT_WALL_EXTRA;			
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
			if ((cave[0+y0-yhsize][i+x0-xhsize].info&CAVE_ICKY)&&(room))
			{
				/*Next to a 'filled' region?-set to be room walls */
				cave[y0+0-yhsize][x0+i-xhsize].feat=FEAT_WALL_OUTER;
				if (light) cave[y0+0-yhsize][x0+i-xhsize].info|=(CAVE_GLOW);
				cave[y0+0-yhsize][x0+i-xhsize].info|=(CAVE_ROOM);
				cave[y0+0-yhsize][x0+i-xhsize].feat=FEAT_WALL_OUTER;
			}
                        else
                        {
                                /*set to be normal granite*/
                                cave[y0+0-yhsize][x0+i-xhsize].feat=FEAT_WALL_EXTRA;
                        }
                        
                        
			/* bottom boundary */
			if ((cave[ysize+y0-yhsize][i+x0-xhsize].info&CAVE_ICKY)&&(room))
			{
				/*Next to a 'filled' region?-set to be room walls */
				cave[y0+ysize-yhsize][x0+i-xhsize].feat=FEAT_WALL_OUTER;
				if (light) cave[y0+ysize-yhsize][x0+i-xhsize].info|=(CAVE_GLOW);
				cave[y0+ysize-yhsize][x0+i-xhsize].info|=(CAVE_ROOM);
				cave[y0+ysize-yhsize][x0+i-xhsize].feat=FEAT_WALL_OUTER;				
			}
			else
			{
				/*set to be normal granite*/
				cave[y0+ysize-yhsize][x0+i-xhsize].feat=FEAT_WALL_EXTRA;
			}				
		
		/* clear the icky flag-don't need it any more */
		
		cave[y0+0-yhsize][x0+i-xhsize].info&= ~(CAVE_ICKY);
		cave[y0+ysize-yhsize][x0+i-xhsize].info&= ~(CAVE_ICKY);
		}

	/* Do the left and right boundaries minus the corners (done above) */

	for(i=1;i<ysize;++i)
	{
		/* left boundary */
		if ((cave[i+y0-yhsize][0+x0-xhsize].info&CAVE_ICKY)&&(room))
		{
			/* room boundary */
			cave[y0+i-yhsize][x0+0-xhsize].feat=FEAT_WALL_OUTER;
			if (light) cave[y0+i-yhsize][x0+0-xhsize].info|=(CAVE_GLOW);
			cave[y0+i-yhsize][x0+0-xhsize].info|=(CAVE_ROOM);
			cave[y0+i-yhsize][x0+0-xhsize].feat=FEAT_WALL_OUTER;			
		}
		else 
		{
			/* outside room */
			cave[y0+i-yhsize][x0+0-xhsize].feat=FEAT_WALL_EXTRA;
		}
		/* right boundary */
		if ((cave[i+y0-yhsize][xsize+x0-xhsize].info&CAVE_ICKY)&&(room))
		{
			/* room boundary */
			cave[y0+i-yhsize][x0+xsize-xhsize].feat=FEAT_WALL_OUTER;
			if (light) cave[y0+i-yhsize][x0+xsize-xhsize].info|=(CAVE_GLOW);
			cave[y0+i-yhsize][x0+xsize-xhsize].info|=(CAVE_ROOM);
			cave[y0+i-yhsize][x0+xsize-xhsize].feat=FEAT_WALL_OUTER;		
		}
		else
		{
			/* outside room */
			cave[y0+i-yhsize][x0+xsize-xhsize].feat=FEAT_WALL_EXTRA;
		}
		/* clear icky flag -done with it */
		cave[y0+i-yhsize][x0+0-xhsize].info&= ~(CAVE_ICKY);
		cave[y0+i-yhsize][x0+xsize-xhsize].info&= ~(CAVE_ICKY);
	}

	/* Do the rest: convert back to the normal format*/
	for(x=1;x<xsize;++x)
	{	
		for(y=1;y<ysize;++y)
		{			
			if ((cave[y0+y-yhsize][x0+x-xhsize].feat==FEAT_FLOOR)
				&&(cave[y0+y-yhsize][x0+x-xhsize].info&CAVE_ICKY))
				
			{
				/*Clear the icky flag in the filled region*/
				cave[y0+y-yhsize][x0+x-xhsize].info&= ~CAVE_ICKY;
				/*Set appropriate flags */				
				if (light) cave[y0+y-yhsize][x0+x-xhsize].info|=(CAVE_GLOW);
				if (room) cave[y0+y-yhsize][x0+x-xhsize].info|=(CAVE_ROOM);		
			}
			else if ((cave[y0+y-yhsize][x0+x-xhsize].feat==FEAT_WALL_OUTER)
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
					
					cave[y0+y-yhsize][x0+x-xhsize].feat=FEAT_WALL_EXTRA;
					cave[y0+y-yhsize][x0+x-xhsize].info&= ~(CAVE_ROOM);
				}
			}
			else 
			{
				/*Clear the unconnected regions*/
				cave[y0+y-yhsize][x0+x-xhsize].feat=FEAT_WALL_EXTRA;			
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
	
	if (dun_level <= randint(25)) light = TRUE;
	
	while (!done)
	{
		/* Note: size must be even or there are rounding problems 
		* This causes the tunnels not to connect properly to the room */	 
		
		/* testing values for these parameters feel free to adjust*/
		grd=1<<(rand_int(4)); 
		
		/* want average of about 16 */
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
	bool done,light;
	
	light=done=FALSE;
	if (dun_level <= randint(50)) light = TRUE;
	
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
		grd=randint(4)+4;
		
		/*want average of about 16 */
		roug=randint(8)*randint(4); 
		
		/* about size/2 */
		cutoff=xsize/2; 
		
                /* make it */
		generate_hmap(y0+1,x0+1,xsize,ysize,grd,roug,cutoff);
				 
		/* Convert to normal format+ clean up*/
		done=generate_fracave(y0+1,x0+1,xsize,ysize,cutoff,light,FALSE);
	}
}

static bool generate_lake(int y0, int x0,int xsize,int ysize,int c1,int c2,int c3,int type)
{
	int x,y,i,amount,xhsize,yhsize;
	int feat1, feat2, feat3;
	
	/*offsets to middle from corner*/
	xhsize=xsize/2;
	yhsize=ysize/2;
	
	/* Get features based on type */	
	switch (type)
	{
		case 1: {
				/*Lava*/
				feat1= FEAT_DEEP_LAVA;
				feat2= FEAT_SHAL_LAVA;
				feat3= FEAT_FLOOR;
				}; break;
		case 2:{				
				/*Water*/
				feat1= FEAT_DEEP_WATER;
				feat2= FEAT_SHAL_WATER;
				feat3= FEAT_FLOOR;
				}; break;
		case 3: {
				/*Collapsed cave*/
				feat1= FEAT_FLOOR;
				feat2= FEAT_FLOOR;
				feat3= FEAT_RUBBLE;
				}; break;	
		case 4: {
				/*Earth vault*/
				feat1= FEAT_RUBBLE;
				feat2= FEAT_FLOOR;
				feat3= FEAT_RUBBLE;
				}; break;
		case 5: {
				/*Air vault*/
				feat1= FEAT_FLOOR;
				feat2= FEAT_TREES;
				feat3= FEAT_FLOOR;
				}; break;
		case 6: {
				/*Water vault*/
				feat1= FEAT_SHAL_WATER;
				feat2= FEAT_DEEP_WATER;
				feat3= FEAT_SHAL_WATER;
				}; break;
		case 7: {
				/*Fire Vault*/
				feat1= FEAT_SHAL_LAVA;
				feat2= FEAT_DEEP_LAVA;
				feat3= FEAT_SHAL_LAVA;
				}; break;

		/* Paranoia */
		default: return FALSE;
	}
	
	/* tally=0 */
	amount=0; 
	
	/*select region connected to center of cave system
	* this gets rid of alot of isolated one-sqaures that
	* can make teleport traps instadeaths... */
	fill_hack(y0,x0,yhsize,xhsize,xsize,ysize,c1,c2,c3,feat1,feat2,feat3,&amount);
	
	/* if tally too small, try again*/
	if (amount<10) 
	{
		/*too small -clear area and try again later*/
		for(x=0;x<=xsize;++x)
		{
			for(y=0;y<=ysize;++y)
			{
				cave[y0+y-yhsize][x0+x-xhsize].feat=FEAT_WALL_EXTRA;	
				cave[y0+y-yhsize][x0+x-xhsize].info&= ~(CAVE_ICKY);
			}
		}
		return FALSE;
	}

	/*Do boundarys- set to normal granite */
	for(i=0;i<=xsize;++i)
		{
		cave[y0+0-yhsize][x0+i-xhsize].feat=FEAT_WALL_EXTRA;
		cave[y0+ysize-yhsize][x0+i-xhsize].feat=FEAT_WALL_EXTRA;
		
		/* clear the icky flag-don't need it any more */
		
		cave[y0+0-yhsize][x0+i-xhsize].info&= ~(CAVE_ICKY);
		cave[y0+ysize-yhsize][x0+i-xhsize].info&= ~(CAVE_ICKY);
		}

	/* Do the left and right boundaries minus the corners (done above) */

	for(i=1;i<ysize;++i)
	{
		cave[y0+i-yhsize][x0+0-xhsize].feat=FEAT_WALL_EXTRA;
		cave[y0+i-yhsize][x0+xsize-xhsize].feat=FEAT_WALL_EXTRA;
				
		/* clear icky flag -done with it */
		
		cave[y0+i-yhsize][x0+0-xhsize].info&= ~(CAVE_ICKY);
		cave[y0+i-yhsize][x0+xsize-xhsize].info&= ~(CAVE_ICKY);
	}


	/* Do the rest: convert back to the normal format*/
	for(x=1;x<xsize;++x)
	{	
		for(y=1;y<ysize;++y)
		{			
			/* Fill unconnected regions with granite */
			if ((!(cave[y0+y-yhsize][x0+x-xhsize].info&CAVE_ICKY))||
				(cave[y0+y-yhsize][x0+x-xhsize].feat==FEAT_WALL_OUTER))
				cave[y0+y-yhsize][x0+x-xhsize].feat=FEAT_WALL_EXTRA;
			
			/* turn off icky flag (no longer needed.) */
			cave[y0+y-yhsize][x0+x-xhsize].info&= ~(CAVE_ICKY|CAVE_ROOM);
			
			/* Light lava and trees*/
			if ((cave[y0+y-yhsize][x0+x-xhsize].feat==FEAT_DEEP_LAVA)||
				(cave[y0+y-yhsize][x0+x-xhsize].feat==FEAT_SHAL_LAVA)||
				(cave[y0+y-yhsize][x0+x-xhsize].feat==FEAT_TREES))
			{
				cave[y0+y-yhsize][x0+x-xhsize].info|=CAVE_GLOW;	
			}
		}
	}

	return TRUE;
}

/*makes a lake/ collapsed cave system in the center of the dungeon */

static void build_lake(type)
{
	int grd,roug,xsize,ysize,x0,y0;
	bool done=FALSE;
	int c1,c2,c3;
	
	/* Make the size of the dungeon */
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
		grd=randint(3)+4;
		
		/*want average of about 16 */
		roug=randint(8)*randint(4); 
		
		/* Make up size of various componants*/
		/* Floor */
		c3=2*xsize/3;
		
		/*Deep water/lava*/ 
		c1=rand_int(c3/2)+rand_int(c3/2)-5;
		
		/* Shallow boundary*/
		c2=(c1+c3)/2;
		
		/* make it */
		generate_hmap(y0+1,x0+1,xsize,ysize,grd,roug,c3);
				 
		/* Convert to normal format+ clean up*/
		done=generate_lake(y0+1,x0+1,xsize,ysize,c1,c2,c3,type);
	}
}

/* Routine used by the random vault creators to add a door to a location
 * Note that range checking has to be done in the calling routine.
 * The doors must be INSIDE the allocated region
 */

static void add_door(int x,int y)

{
	/*Need to have a wall in the center square*/
	if (cave[y][x].feat!=FEAT_WALL_OUTER) return; 

	/* look at:
	*  x#x
	*  .#.
	*  x#x
	*
	*  where x=don't care
	*  .=floor, #=wall
	*/

	if ((cave[y-1][x].feat==FEAT_FLOOR)&&(cave[y+1][x].feat==FEAT_FLOOR)
		&&(cave[y][x-1].feat==FEAT_WALL_OUTER)&&(cave[y][x+1].feat==FEAT_WALL_OUTER))
	{
		/*secret door*/
		place_secret_door(y,x);
	
		/*set boundarys so don't get wide doors*/
		cave[y][x-1].feat=FEAT_WALL_SOLID;
		cave[y][x+1].feat=FEAT_WALL_SOLID;
	}

	
	/* look at:
	*  x#x
	*  .#.
	*  x#x
	*
	*  where x=don't care
	*  .=floor, #=wall
	*/

	if ((cave[y-1][x].feat==FEAT_WALL_OUTER)&&(cave[y+1][x].feat==FEAT_WALL_OUTER)
		&&(cave[y][x-1].feat==FEAT_FLOOR)&&(cave[y][x+1].feat==FEAT_FLOOR))
	{
		/*secret door*/
		place_secret_door(y,x);
	
		/*set boundarys so don't get wide doors*/
		cave[y-1][x].feat=FEAT_WALL_SOLID;
		cave[y+1][x].feat=FEAT_WALL_SOLID;
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
		
			 /* if floor, shallow water and lava*/
			if ((cave[y][x].feat==FEAT_FLOOR)||(cave[y][x].feat==FEAT_SHAL_WATER)||
				(cave[y][x].feat==FEAT_SHAL_LAVA))
			{
				/* The smaller 'value' is, the better the stuff*/
				if (value<0) 
				{
					/*Meanest monster + treasure*/
					monster_level = base_level + 40;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					object_level = base_level + 20;
					place_object(y, x, TRUE, FALSE);
					object_level = base_level;				
				}
				else if(value<5)
				{
					/*Mean monster +treasure*/
					monster_level = base_level + 20;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					object_level = base_level + 10;
					place_object(y, x, TRUE, FALSE);
					object_level = base_level;
				}
				else if(value<10)
				{
					/*Monster*/
					monster_level = base_level + 9;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;				
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
					monster_level = base_level + 5;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					place_trap(y, x);
				}
				else if(value<40) 
				{
					/*Monster or object*/
					if (rand_int(100) < 50)
					{
						monster_level = base_level + 3;
						place_monster(y, x, TRUE, TRUE);
						monster_level = base_level;
					}
					if (rand_int(100) < 50)
					{
						object_level = base_level + 7;
						place_object(y, x, FALSE, FALSE);
						object_level = base_level;
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
* It works by getting a set of coordinates that represent the center of each
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
		cave[y0-yhsize+0][x0-xhsize+i].feat=FEAT_WALL_OUTER;
		cave[y0-yhsize+0][x0-xhsize+i].info|=(CAVE_ROOM|CAVE_ICKY);
		cave[y0-yhsize+ysize][x0-xhsize+i].feat=FEAT_WALL_OUTER;
		cave[y0-yhsize+ysize][x0-xhsize+i].info|=(CAVE_ROOM|CAVE_ICKY);
	}
	
	/*Left and right boundaries*/
	for (i=1;i<ysize;i++)
	{
		cave[y0-yhsize+i][x0-xhsize+0].feat=FEAT_WALL_OUTER;
		cave[y0-yhsize+i][x0-xhsize+0].info|=(CAVE_ROOM|CAVE_ICKY);
		cave[y0-yhsize+i][x0-xhsize+xsize].feat=FEAT_WALL_OUTER;
		cave[y0-yhsize+i][x0-xhsize+xsize].info|=(CAVE_ROOM|CAVE_ICKY);
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
				cave[y0-yhsize+y][x0-xhsize+x].feat=FEAT_WALL_OUTER;
			}
			else
			{	
				/*middle of a bubble*/
				cave[y0-yhsize+y][x0-xhsize+x].feat=FEAT_FLOOR;
			}
			
			/*clean up rest of flags*/
			cave[y0-yhsize+y][x0-xhsize+x].info|=(CAVE_ROOM|CAVE_ICKY);
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
		cave[y1][x1+i].feat=FEAT_WALL_OUTER;
		cave[y1][x1+i].info|=(CAVE_ROOM|CAVE_ICKY);
		cave[y2][x1+i].feat=FEAT_WALL_OUTER;
		cave[y2][x1+i].info|=(CAVE_ROOM|CAVE_ICKY);
	}
	
	/*Left and right boundaries*/
	for (i=1;i<ysize;i++)
	{
		cave[y1+i][x1].feat=FEAT_WALL_OUTER;
		cave[y1+i][x1].info|=(CAVE_ROOM|CAVE_ICKY);
		cave[y1+i][x2].feat=FEAT_WALL_OUTER;
		cave[y1+i][x2].info|=(CAVE_ROOM|CAVE_ICKY);
	}
	
	/* Middle*/
	for (x=1;x<xsize;x++)
	{
		for(y=1;y<ysize;y++)
		{
			if(cave[y1+y][x1+x].feat==FEAT_WALL_EXTRA)
			{
				/*clear the untouched region*/
				cave[y1+y][x1+x].feat=FEAT_FLOOR;
				cave[y1+y][x1+x].info|=(CAVE_ROOM|CAVE_ICKY);			
			}
			else 
			{
				/*make it a room- but don't touch*/
				cave[y1+y][x1+x].info|=(CAVE_ROOM|CAVE_ICKY);
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
	
	/*fill area so don't get problems with arena levels*/
	for(x1=0;x1<=xsize;x1++)
	{
		for(y1=0;y1<=ysize;y1++)
		{
			cave[y0-yhsize+y1][x0-xhsize+x1].feat=FEAT_WALL_EXTRA;
			cave[y0-yhsize+y1][x0-xhsize+x1].info&=(~CAVE_ICKY);
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
		grd=1<<rand_int(4); 
		
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
			cave[y0-yhsize+y][x0-xhsize+x].info|=CAVE_ICKY;
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
 *                   monsters, traps and treasure
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
	int y, x, dy, dx;
	int y1, x1, y2, x2;
	int i, m, n, num_vertices, *visited;
	bool light;
	cave_type *c_ptr;
	
	if (cheat_room) msg_print("Maze Vault");
	
	/* Choose lite or dark */
	light = (dun_level <= randint(25));

	/* Pick a random room size- randomized by calling routine*/
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
			c_ptr = &cave[y][x];
			c_ptr->info |= (CAVE_ROOM | CAVE_ICKY);
			if ((x==x1-1)||(x==x2+1)||(y==y1-1)||(y==y2+1))
			c_ptr->feat = FEAT_WALL_OUTER;
			else
			c_ptr->feat =FEAT_WALL_INNER;
			if (light) c_ptr->info |= (CAVE_GLOW);
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
	
	/* traverse the graph to create a spaning tree, pick a random root */
	r_visit( y1, x1, y2, x2, rand_int( num_vertices), 0, visited);

	/* Fill with monsters and treasure, low difficulty*/
	fill_treasure(x1,x2,y1,y2,randint(5));

	free (visited);
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
			cave[y][x].info |= (CAVE_ROOM|CAVE_ICKY);
			
			/* Permanent walls */
			cave[y][x].feat =FEAT_PERM_INNER;
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
			if((total%2==1)&&(cave[y][x].feat==FEAT_FLOOR))
			{
				cave[y][x].feat=FEAT_WALL_INNER;
			}		
		}	
	}
	
	/*Make a couple of entrances*/
	if (randint(2)==1)
	{
		/* left and right */
		y=randint(dy)+dy/2;
		cave[y1+y][x1-1].feat=FEAT_WALL_OUTER;		
		cave[y1+y][x2+1].feat=FEAT_WALL_OUTER;
	}
	else
	{
		/* top and bottom */
		x=randint(dx)+dx/2;
		cave[y1-1][x1+x].feat=FEAT_WALL_OUTER;
		cave[y2+1][x1+x].feat=FEAT_WALL_OUTER;
	}	
	
     /*Fill with monsters and treasure, highest difficulty*/
	fill_treasure(x1,x2,y1,y2,10);
 
    /* rnfree( visited,num_vertices * sizeof(int));*/
	free(visited);
}

/* Build a town/castle by using a recursive algorithm.
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
				cave[y1][x].feat=FEAT_WALL_OUTER;
				cave[y2][x].feat=FEAT_WALL_OUTER;	
			}
	
			/*left and right*/
			for(y=y1+1;y<y2;y++)
			{
				cave[y][x1].feat=FEAT_WALL_OUTER;
				cave[y][x2].feat=FEAT_WALL_OUTER;		
			}
		
			/* Make a couple of entrances */
			if (randint(2)==1)
			{
				/* left and right */
				y=randint(ysize)+y1;
				cave[y][x1].feat=FEAT_FLOOR;
				cave[y][x2].feat=FEAT_FLOOR;
			}
			else
			{
				/* top and bottom */
				x=randint(xsize)+x1;
				cave[y1][x].feat=FEAT_FLOOR;
				cave[y2][x].feat=FEAT_FLOOR;
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
						cave[y][x].feat=FEAT_WALL_INNER;
					}
				}
				
				/*Too small */
				return;
			}
			
			/* Make outside walls */
			/*top and bottom*/
			for(x=x1+1;x<=x2-1;x++)
			{
				cave[y1+1][x].feat=FEAT_WALL_INNER;
				cave[y2-1][x].feat=FEAT_WALL_INNER;	
			}
	
			/*left and right*/
			for(y=y1+1;y<=y2-1;y++)
			{
				cave[y][x1+1].feat=FEAT_WALL_INNER;
				cave[y][x2-1].feat=FEAT_WALL_INNER;		
			}
			
			/*Make a door*/
			y=randint(ysize-3)+y1+1;
		
			if (randint(2)==1)
			{
				/* left  */			
				cave[y][x1+1].feat=FEAT_FLOOR;
			}
			else
			{
				/* right */				
				cave[y][x2-1].feat=FEAT_FLOOR;	
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
						cave[y][x].feat=FEAT_WALL_INNER;
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
						cave[y][x].feat=FEAT_WALL_INNER;
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
			cave[y][x].info |= (CAVE_ROOM|CAVE_ICKY);
			/* Make everything a floor */
			cave[y][x].feat =FEAT_FLOOR;
		}
	}	
    
	/* Make the castle */
	build_recursive_room(x1,y1,x2,y2,randint(5));
	
	/*Fill with monsters and treasure, low difficulty*/
	fill_treasure(x1,x2,y1,y2,randint(3));
}


/*
* Add outer wall to a floored region
* Note: no range checking is done so must be inside dungeon
* This routine also stomps on doors
*/
static void add_outer_wall(int x,int y,int light,int x1, int y1, int x2, int y2)
{
	int i,j;
	
	if (!in_bounds(y,x)) return;
	
	/* hack- check to see if square has been visited before
	* if so, then exit (use room flag to do this) */	
	if (cave[y][x].info&CAVE_ROOM) return;

	/* set room flag */
	cave[y][x].info|=CAVE_ROOM;
	
	if (light==TRUE) cave[y][x].info|=CAVE_GLOW;
	
	if (cave[y][x].feat==FEAT_FLOOR)
	{
		for(i=-1;i<=1;i++)
		{
			for(j=-1;j<=1;j++)
			{
				if ((x+i>=x1)&&(x+i<=x2)&&(y+j>=y1)&&(y+j<=y2))
					add_outer_wall(x+i,y+j,light,x1,y1,x2,y2);
			}
		}		
	}
	else if(cave[y][x].feat==FEAT_WALL_EXTRA)
	{
		/*Set bounding walls*/
		cave[y][x].feat=FEAT_WALL_OUTER;
			
	}
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
 * Build target vault.
 * This is made by two concentric "crypts" with perpendicular
 * walls creating the cross-hairs. 
 */
static void build_target_vault(int x0,int y0,int xsize,int ysize)
{
	int rad, x, y;
	
	/* Make a random metric */
	int h1,h2,h3,h4;
	h1=randint(32)-16;
	h2=randint(16);
	h3=randint(32);
	h4=randint(32)-16;
	
	if (cheat_room) msg_print("Target Vault");
	
	/* work out outer radius */
	if (xsize>ysize)
	{
		rad=ysize/2;
	}
	else
	{
		rad=xsize/2;
	}
	
	/*Make floor */
	for (x = x0 - rad; x <= x0 + rad; x++)
	{
		for (y = y0 - rad; y <= y0 + rad; y++)
		{			
			/*clear room flag*/
			cave[y][x].info &= ~(CAVE_ROOM);
			
			/* Vault - so is "icky" */
			cave[y][x].info |= CAVE_ICKY;
			
			if (dist2(y0, x0, y, x, h1, h2, h3, h4)<=rad-1)
			{				
				/* inside- so is floor */
				cave[y][x].feat = FEAT_FLOOR;										
			}
			else
			{
				/* make granite outside so arena works */				
				cave[y][x].feat = FEAT_WALL_EXTRA;										
			}
			
			/* proper boundary for arena*/
			if (((y+rad)==y0)||((y-rad)==y0)||((x+rad)==x0)||((x-rad)==x0))
			{
				cave[y][x].feat = FEAT_WALL_EXTRA;
			}			
		}	
	}
	
	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0,y0,FALSE,x0-rad-1,y0-rad-1,x0+rad+1,y0+rad+1);
	
	/* Add inner wall */
	for (x = x0 - rad/2; x <= x0 + rad/2; x++)
	{
		for (y = y0 - rad/2; y <= y0 + rad/2; y++)
		{			
			if (dist2(y0, x0, y, x, h1, h2, h3, h4)==rad/2)
			{				
				/* Make an internal wall */
				cave[y][x].feat = FEAT_WALL_INNER;							
			}
		}	
	}
	
	/* Add perpendicular walls */
	
	for (x=x0-rad;x<=x0+rad;x++)
	{
		cave[y0][x].feat=FEAT_WALL_INNER;
	}
	
	for (y=y0-rad;y<=y0+rad;y++)
	{
		cave[y][x0].feat=FEAT_WALL_INNER;
	}
	
	/* Make inner vault */
	for (y = y0-1; y <= y0+1; y++)
	{
		cave[y][x0-1].feat=FEAT_WALL_INNER;
		cave[y][x0+1].feat=FEAT_WALL_INNER; 
	}
	for (x = x0-1; x <= x0+1; x++)
	{
		cave[y0-1][x].feat = FEAT_WALL_INNER;
		cave[y0+1][x].feat = FEAT_WALL_INNER;
	}

	cave[y0][x0].feat=FEAT_FLOOR;
	
	
	/* Add doors to vault */
	/* get two distances so can place doors relative to centre */
	x=(rad-2)/4+1;
	y=rad/2+x;
	
	place_secret_door(y0, x0+x);
	place_secret_door(y0, x0+y);
	place_secret_door(y0, x0-x);
	place_secret_door(y0, x0-y);
	place_secret_door(y0+x, x0);
	place_secret_door(y0+y, x0);
	place_secret_door(y0-x, x0);
	place_secret_door(y0-y, x0);

	/* Fill with stuff - medium difficulty */
	fill_treasure(x0-rad,x0+rad,y0-rad,y0+rad,randint(3)+3);
}

/*
 * This routine uses a modified version of the lake code to make a
 * distribution of some terrain type over the vault.  This type
 * depends on the dungeon depth.
 *
 * Miniture rooms are then scattered across the vault.
 */

static void build_elemental_vault(x0,y0,xsiz,ysiz)
{
	int grd,roug;
	
	int c1,c2,c3;
	bool done=FALSE;
	
	int xsize,ysize,xhsize,yhsize,x,y,i;
	int type;

	if (cheat_room) msg_print("Elemental Vault");

	/*round to make sizes even*/
	xhsize=xsiz/2;
	yhsize=ysiz/2;
	xsize=xhsize*2;
	ysize=yhsize*2;

	if (dun_level<25)
	{
		/*Earth vault  (Rubble)*/
		type=4;
	}
	else if (dun_level<50)
	{
		/* Air vault (Trees)*/
		type=5;
	}
	else if (dun_level<75)
	{
		/*Water vault (shallow water)*/
		type=6;
	}
	else
	{
		/*Fire vault (shallow lava)*/
		type=7;
	}

	while (!done)
	{
		/* testing values for these parameters: feel free to adjust*/
		grd=1<<(rand_int(3));
		
		/*want average of about 16 */
		roug=randint(8)*randint(4); 
		
		/* Make up size of various componants*/
		/* Floor */
		c3=2*xsize/3;
		
		/*Deep water/lava*/ 
		c1=rand_int(c3/2)+rand_int(c3/2)-5;
		
		/* Shallow boundary*/
		c2=(c1+c3)/2;
		
		/* make it */
		generate_hmap(y0,x0,xsize,ysize,grd,roug,c3);
		
		/* Convert to normal format+ clean up*/
		done=generate_lake(y0,x0,xsize,ysize,c1,c2,c3,type);
	}
	
	/*Set icky flag because is a vault*/
	for (x=0;x<=xsize;x++)
	{
		for(y=0;y<=ysize;y++)
		{
			cave[y0-yhsize+y][x0-xhsize+x].info|=CAVE_ICKY;
		}
	}
	
	/* make a few rooms in the vault */
	for (i=1;i<=(xsize*ysize)/50;i++)
	{
		microroom(x0+rand_int(xsize-4)-xsize/2+2,y0+rand_int(ysize-4)-ysize/2+2);
	}
	
	/*Fill with monsters and treasure, low difficulty*/
	fill_treasure(x0-xhsize+1,x0-xhsize+xsize-1,y0-yhsize+1,y0-yhsize+ysize-1,randint(5));
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
	if ((dun_level <= 50) ||  (randint((dun_level-40) * (dun_level-40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}
	
	/*Select type of vault*/
	vtype=randint(8);

	switch (vtype)
	{
		/* Build an appropriate room */
		case 1: build_bubble_vault(x0,y0,xsize,ysize); break;
		case 2: build_room_vault(x0,y0,xsize,ysize); break;
		case 3: build_cave_vault(x0,y0,xsize,ysize); break;
		case 4: build_maze_vault(x0,y0,xsize,ysize); break;
		case 5: build_mini_c_vault(x0,y0,xsize,ysize); break;
		case 6: build_castle_vault(x0,y0,xsize,ysize); break;
		case 7: build_target_vault(x0,y0,xsize,ysize); break;
		case 8: build_elemental_vault(x0,y0,xsize,ysize);break;
		/*I know how to add a few more... give me some time.*/
		
		/* Paranoia */
		default: return;
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
	if (randint(dun_level) <= 15) light = TRUE;

	rad = rand_int(10);
	
	/*Allocate in room_map.  If will not fit, exit*/
	if (room_alloc(rad*2+1,rad*2+1,FALSE,by0,bx0,&x0,&y0)==FALSE) return;
	
	
	/*Make circular floor */
	for (x = x0 - rad; x <= x0 + rad; x++)
	{
		for (y = y0 - rad; y <= y0 + rad; y++)
		{			
			if (distance(y0, x0, y, x)<=rad-1)
			{
				/* inside- so is floor */				
				cave[y][x].feat = FEAT_FLOOR;									
			}			
			else if(distance(y0, x0, y, x)<=rad+1)
			{
				/* make granite outside so arena works */				
				cave[y][x].feat = FEAT_WALL_EXTRA;									
			}
		}
	
	}
	
	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0,y0,light,x0-rad,y0-rad,x0+rad,y0+rad);
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
	if (randint(dun_level) <= 5) light = TRUE;
	
	rad = randint(9);
	
	/*Allocate in room_map.  If will not fit, exit*/
	if (room_alloc(rad*2+3,rad*2+3,FALSE,by0,bx0,&x0,&y0)==FALSE) return;
	
	/*Make floor */
	for (x = x0 - rad; x <= x0 + rad; x++)
	{
		for (y = y0 - rad; y <= y0 + rad; y++)
		{			
			/*clear room flag*/
			cave[y][x].info &= ~(CAVE_ROOM);
			
			if (dist2(y0, x0, y, x, h1, h2, h3, h4)<=rad-1)
			{				
				/* inside- so is floor */
				cave[y][x].feat = FEAT_FLOOR;										
			}
			else if (distance(y0,x0,y,x)<3)
			{
				cave[y][x].feat = FEAT_FLOOR;
			}		
			else
			{
				/* make granite outside so arena works */				
				cave[y][x].feat = FEAT_WALL_EXTRA;										
			}
			
			/* proper boundary for arena*/
			if (((y+rad)==y0)||((y-rad)==y0)||((x+rad)==x0)||((x-rad)==x0))
			{
				cave[y][x].feat = FEAT_WALL_EXTRA;
			}
		}	
	}
	
	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0,y0,light,x0-rad-1,y0-rad-1,x0+rad+1,y0+rad+1);
	
	/* check to see if there is room for an inner vault */	
	for (x=x0-2;x<=x0+2;x++)
	{
		for(y=y0-2;y<=y0+2;y++)
		{
			if (cave[y][x].feat!=FEAT_FLOOR)
			{
				/* wall in the way */
				emptyflag=FALSE;
			}
		}	
	}
	
	if ((emptyflag)&&randint(2)==1)
	{
		/* Build the vault */
		microroom(x0,y0);

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
	int y, x;
	int tmp_row, tmp_col;
	int row_dir, col_dir;
	int start_row, start_col;
	int main_loop_count = 0;

	bool door_flag = FALSE;

	cave_type *c_ptr;

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
		if (rand_int(100) < dun_tun_chg)
		{
			/* Acquire the correct direction */
			correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Random direction */
			if (rand_int(100) < dun_tun_rnd)
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
			if (rand_int(100) < dun_tun_rnd)
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
		if (c_ptr->feat == FEAT_WALL_OUTER)
		{
			/* Acquire the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Hack -- Avoid outer/solid permanent walls */
			if (cave[y][x].feat == FEAT_PERM_SOLID) continue;
			if (cave[y][x].feat == FEAT_PERM_OUTER) continue;

			/* Hack -- Avoid outer/solid granite walls */
			if (cave[y][x].feat == FEAT_WALL_OUTER) continue;
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
					if (cave[y][x].feat == FEAT_WALL_OUTER)
					{
						/* Change the wall to a "solid" wall */
						cave[y][x].feat = FEAT_WALL_SOLID;
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
		else if (c_ptr->feat >= FEAT_WALL_EXTRA)
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
			if (rand_int(100) >= dun_tun_con)
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
}


/* 
* This routine adds the square to the tunnel 
* It also checks for SOLID walls - and returns a nearby
* non-SOLID square in (x,y) so that a simple avoiding
* routine can be used. The returned boolean value reflects
* whether or not this routine hit a SOLID wall.
*
* "affectwall" toggles whether or not this new square affects
* the boundaries of rooms. - This is used by the catacomb
* routine.
*/  

static bool set_tunnel(int *x,int *y,bool affectwall)
{
	int feat,i,j,dx,dy;
	
	if (!in_bounds(*y,*x)) return TRUE;
	
	feat=cave[*y][*x].feat;
	
	if ((feat==FEAT_PERM_OUTER)||(feat==FEAT_PERM_INNER)||(feat==FEAT_WALL_INNER))
	{
		/* 
		* ignore permanent walls - sometimes cannot tunnel around them anyway
		* so don't try - it just complicates things unnecessarily
		*/
		return TRUE;
	}
	
	if (feat==FEAT_WALL_EXTRA)
	{
		/* Save the tunnel location */
		if (dun->tunn_n < TUNN_MAX)
		{
				dun->tunn[dun->tunn_n].y = *y;
				dun->tunn[dun->tunn_n].x = *x;
				dun->tunn_n++;
		}
		return TRUE;
	}
	
	if (feat==FEAT_FLOOR)
	{
		/* Don't do anything */
		return TRUE;
	}
	
	if ((feat==FEAT_WALL_OUTER)&&(affectwall))
	{	/* Save the wall location */
		if (dun->wall_n < WALL_MAX)
		{
			dun->wall[dun->wall_n].y = *y;
			dun->wall[dun->wall_n].x = *x;
			dun->wall_n++;
		}			
		/* Forbid re-entry near this piercing */
		for (j = *y - 1; j <= *y + 1; j++)
		{
			for (i = *x - 1; i <= *x + 1; i++)
			{
				/* Convert adjacent "outer" walls as "solid" walls */
				if (cave[j][i].feat == FEAT_WALL_OUTER)
				{
					/* Change the wall to a "solid" wall */
					cave_set_feat(j, i, FEAT_WALL_SOLID);
				}
			}
		}		
		cave_set_feat(*y, *x, FEAT_FLOOR);
		return TRUE;
	}

	if ((feat==FEAT_WALL_SOLID)&&(affectwall))
	{
		/* cannot place tunnel here - use a square to the side */
	
		/* find usable square and return value in (x,y) */
				
		i=50;
			
		dy=0;
		dx=0;
		while ((i>0)&&(cave[*y+dy][*x+dx].feat==FEAT_WALL_SOLID))
		{
			dy=rand_int(3)-1;
			dx=rand_int(3)-1;
			if (!in_bounds(*y+dy,*x+dx))
			{
				dx=0;
				dy=0;					
			}
			i--;
		}
			
		if (i==0)
		{
			/* Failed for some reason: hack - ignore the solidness*/
			cave[*y][*x].feat=FEAT_WALL_OUTER;
			dx=0;
			dy=0;
		}
		/* give new, acceptable coordinate */
		*x= *x+dx;
		*y= *y+dy;
		return FALSE;	
	}	

	return TRUE;
}

/* 
* This routine creates the catacomb-like tunnels by removing extra rock.
* Note that this routine is only called on "even" squares - so it gives
* a natural checkerboard pattern.
*/
static void create_cata_tunnel(int x,int y)
{
	int x1,y1;
	
	/* Build tunnel */
	x1=x-1;
	y1=y;
	set_tunnel(&x1,&y1,FALSE);
	
	x1=x+1;
	y1=y;
	set_tunnel(&x1,&y1,FALSE);
	
	x1=x;
	y1=y-1;
	set_tunnel(&x1,&y1,FALSE);
	
	x1=x;
	y1=y+1;
	set_tunnel(&x1,&y1,FALSE);
}


/* 
* This routine does the bulk of the work in creating the new types of tunnels.
* It is designed to use very simple algorithms to go from (x1,y1) to (x2,y2)
* It doesn't need to add any complexity - straight lines are fine.
* The SOLID walls are avoided by a recursive algorithm which tries random ways
* around the obstical until it works.  The number of itterations is counted, and it
* this gets too large the routine exits. This should stop any crashes - but may leave
* small gaps in the tunnel where there are too many SOLID walls.
* 
* Type 1 tunnels are extremely simple - straight line from A to B.  This is only used
* as a part of the dodge SOLID walls algorithm.
*
* Type 2 tunnels are made of two straight lines at right angles. When this is used with
* short line segments it gives the "cavelike" tunnels seen deeper in the dungeon.
*
* Type 3 tunnels are made of two straight lines like type 2, but with extra rock removed.
* This, when used with longer line segments gives the "catacomb-like" tunnels seen near
* the surface.
*/  
static void short_seg_hack(int x1,int y1,int x2,int y2,int type,int count,bool *fail)
{
	int i,x,y;	
	int length;
	
	/* Check for early exit */
	if (!(*fail)) return;
	
	length=distance(x1,y1,x2,y2);
	
	count++;
	
	if ((type==1)&&(length!=0))
	{
		
		for (i=0;i<=length;i++)
		{
			x=x1+i*(x2-x1)/length;
			y=y1+i*(y2-y1)/length;
			if (!set_tunnel(&x,&y,TRUE))
			{
				if (count>50)
				{
					/* This isn't working - probably have an infinite loop */
					*fail=FALSE;
					return;				
				}
				
				/* solid wall - so try to go around */
				short_seg_hack(x,y,x1+(i-1)*(x2-x1)/length,y1+(i-1)*(y2-y1)/length,1
					,count,fail);
				short_seg_hack(x,y,x1+(i+1)*(x2-x1)/length,y1+(i+1)*(y2-y1)/length,1
					,count,fail);
			}
		}
	}
	else if ((type==2)||(type==3))
	{
		if (x1<x2)
		{
			for (i=x1;i<=x2;i++)
			{ 
				x=i;
				y=y1;
				if (!set_tunnel(&x,&y,TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x,y,i-1,y1,1,count,fail);
					short_seg_hack(x,y,i+1,y1,1,count,fail);
				}
				if ((type==3)&&((x+y)%2))
				{
					create_cata_tunnel(i,y1);
				}
			}
		}
		else
		{
			for (i=x2;i<=x1;i++)
			{ 
				x=i;
				y=y1;
				if (!set_tunnel(&x,&y,TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x,y,i-1,y1,1,count,fail);
					short_seg_hack(x,y,i+1,y1,1,count,fail);
				}
				if ((type==3)&&((x+y)%2))
				{
					create_cata_tunnel(i,y1);
				}
			}
		
		}
		if (y1<y2)
		{
			for (i=y1;i<=y2;i++)
			{
				x=x2;
				y=i;
				if (!set_tunnel(&x,&y,TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x,y,x2,i-1,1,count,fail);
					short_seg_hack(x,y,x2,i+1,1,count,fail);
				}
				if ((type==3)&&((x+y)%2))
				{
					create_cata_tunnel(x2,i);
				}
			}
		}
		else
		{
			for (i=y2;i<=y1;i++)
			{
				x=x2;
				y=i;
				if (!set_tunnel(&x,&y,TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x,y,x2,i-1,1,count,fail);
					short_seg_hack(x,y,x2,i+1,1,count,fail);
				}
				if ((type==3)&&((x+y)%2))
				{
					create_cata_tunnel(x2,i);
				}
			}
		}		
	}
}


/*
* This routine maps a path from (x1,y1) to (x2,y2) avoiding SOLID walls.
* Permanent rock is ignored in this path finding- sometimes there is no 
* path around anyway -so there will be a crash if we try to find one.
* This routine is much like the river creation routine in Zangband.
* It works by dividing a line segment into two.  The segments are divided
* until they are less than "cutoff" - when the corresponding routine from
* "short_seg_hack" is called.  
* Note it is VERY important that the "stop if hit another passage" logic
* stays as is.  Without this the dungeon turns into Swiss Cheese...
*/
static bool build_tunnel2(int x1, int y1, int x2, int y2,int type,int cutoff)
{
	int x3,y3,dx,dy;
	int changex,changey;
	int midval;
	int length;
	int i;
	bool retval,firstsuccede;
	
	length=distance(x1,y1,x2,y2);
	if(length>cutoff)
	{	
		/*
		* Divide path in half and call routine twice.
		*/ 
		dx=(x2-x1)/2;
		dy=(y2-y1)/2;
		
		/* perturbation perpendicular to path */
		changex=(rand_int(abs(dy)+2)*2-abs(dy)-1)/2;
		
		/* perturbation perpendicular to path */
		changey=(rand_int(abs(dx)+2)*2-abs(dx)-1)/2;
		
		/* Work out "mid" ponit */
		x3=x1+dx+changex;
		y3=y1+dy+changey;
		
		/* See if in bounds - if not -do not perturb point*/
		if (!in_bounds(y3,x3))
		{
			x3=(x1+x2)/2;
			y3=(y1+y2)/2;
		}		
		/* cache midvalue */
		midval=cave[y3][x3].feat;
		if (midval==FEAT_WALL_SOLID)
		{
			/* move midpoint a bit to avoid problem. */
			
			i=50;
			
			dy=0;
			dx=0;
			while ((i>0)&&(cave[y3+dy][x3+dx].feat==FEAT_WALL_SOLID))
			{
				dy=rand_int(3)-1;
				dx=rand_int(3)-1;
				if (!in_bounds(y3+dy,x3+dx))
				{
					dx=0;
					dy=0;					
				}
				i--;
			}
			
			if (i==0)
			{
				/* Failed for some reason: hack - ignore the solidness*/
				cave[y3][x3].feat=FEAT_WALL_OUTER;
				dx=0;
				dy=0;
			}
			y3+=dy;
			x3+=dx;
			midval=cave[y3][x3].feat;
		}
				
		if (midval==FEAT_FLOOR)
		{	
			if (build_tunnel2(x1, y1, x3, y3, type, cutoff))
			{
				if ((cave[y3][x3].info&CAVE_ROOM)||(randint(100)>95))
				{
					/*do second half only if works + if have hit a room*/
					retval=(build_tunnel2(x3, y3, x2, y2, type, cutoff));
				}
				else
				{
					/* have hit another tunnel - make a set of doors here */
					retval= FALSE;
					
					/* Save the door location */
					if (dun->door_n < DOOR_MAX)
					{
						dun->door[dun->door_n].y = y3;
						dun->door[dun->door_n].x = x3;
						dun->door_n++;
					}
				}				
				firstsuccede=TRUE;
			}
			else
			{
				/* false- didn't work all the way */
				retval= FALSE;
				firstsuccede=FALSE;
			}			
		}
		else
		{
			/* tunnel through walls */
			if(build_tunnel2(x1, y1, x3, y3, type, cutoff))
			{
				retval=(build_tunnel2(x3, y3, x2, y2, type, cutoff));
				firstsuccede=TRUE;
			}
			else
			{
				/* false- didn't work all the way */
				retval= FALSE;
				firstsuccede=FALSE;
			}
		}
		if (firstsuccede)
		{		
			/* only do this if the first half has worked */
			set_tunnel(&x3,&y3,TRUE);
		}
		/* return value calculated above */
		return retval;	
	}
	else 
	{ 
		/* Do a short segment */
		retval=TRUE;
		short_seg_hack(x1,y1,x2,y2,type,0,&retval);	
		
		/* Hack - ignore return value so avoid infinite loops */
		return TRUE;
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
		if (c_ptr->feat != FEAT_FLOOR) continue;

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
		if ((cave[y-1][x].feat >= FEAT_MAGMA) &&
		    (cave[y+1][x].feat >= FEAT_MAGMA))
		{
			return (TRUE);
		}

		/* Check Horizontal */
		if ((cave[y][x-1].feat >= FEAT_MAGMA) &&
		    (cave[y][x+1].feat >= FEAT_MAGMA))
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
	if (cave[y][x].feat >= FEAT_MAGMA) return;

	/* Ignore room grids */
	if (cave[y][x].info & (CAVE_ROOM)) return;

	/* Occasional door (if allowed) */
	if ((rand_int(100) < dun_tun_jct) && possible_doorway(y, x))
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
	if (dun_level < roomdep[typ]) return (FALSE);

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
 * Note that it uses ``cave.feat'' as temporary storage.
 * The values in ``cave.feat'' after this function
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
 * The default table in terrain level generation.
 */

static int terrain_table[MAX_WILDERNESS][18] =
{
	/* TERRAIN_EDGE */
	{
			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,

			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,

			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,

			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,

			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,

			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,
			FEAT_PERM_SOLID,
	},
	/* TERRAIN_TOWN */
	{
			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,
	},
	/* TERRAIN_DEEP_WATER */
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

			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
	},
	/* TERRAIN_SHALLOW_WATER */
	{
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,

			FEAT_FLOOR,
			FEAT_DIRT,
			FEAT_GRASS,
	},
	/* TERRAIN_SWAMP */
	{
			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_TREES,

			FEAT_TREES,
			FEAT_TREES,
			FEAT_TREES,

			FEAT_SWAMP,
			FEAT_SWAMP,
			FEAT_SWAMP,

			FEAT_SWAMP,
			FEAT_SWAMP,
			FEAT_SWAMP,

			FEAT_SWAMP,
			FEAT_SWAMP,
			FEAT_SWAMP,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,
	},
	/* TERRAIN_DIRT */
	{
			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_DIRT,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_DIRT,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_DIRT,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_DIRT,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_GRASS,
			FEAT_TREES,
			FEAT_TREES,
	},
	/* TERRAIN_GRASS */
	{
			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_DIRT,

			FEAT_DIRT,
			FEAT_GRASS,
			FEAT_GRASS,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,

			FEAT_GRASS,
			FEAT_TREES,
			FEAT_TREES,
	},
	/* TERRAIN_TREES */
	{
			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_DIRT,

			FEAT_TREES,
			FEAT_TREES,
			FEAT_TREES,

			FEAT_TREES,
			FEAT_TREES,
			FEAT_TREES,

			FEAT_TREES,
			FEAT_TREES,
			FEAT_TREES,

			FEAT_TREES,
			FEAT_TREES,
			FEAT_TREES,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,
	},
	/* TERRAIN_DESERT */
	{
			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_FLOOR,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_DIRT,

			FEAT_DIRT,
			FEAT_TREES,
			FEAT_TREES,
	},
	/* TERRAIN_SHALLOW_LAVA */
	{
			FEAT_DIRT,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,

			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,

			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,

			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,

			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,
	},
	/* TERRAIN_DEEP_LAVA */
	{
			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,

			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,

			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,

			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,

			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,

			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,
			FEAT_DEEP_LAVA,

	},
	/* TERRAIN_MOUNTAIN */
	{
			FEAT_FLOOR,
			FEAT_FLOOR,
			FEAT_GRASS,

			FEAT_GRASS,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_GRASS,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_TREES,
			FEAT_TREES,
			FEAT_TREES,

			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,

			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,
	},

};


/*
 * Load a town or generate a terrain level using "plasma" fractals.
 *
 * x and y are the coordinates of the area in the wilderness.
 * Border and corner are optimization flags to speed up the
 * generation of the fractal terrain.
 * If border is set then only the border of the terrain should
 * be generated (for initializing the border structure).
 * If corner is set then only the corners of the area are needed.
 */
static void generate_area(int y, int x, bool border, bool corner)
{
	int road;
	int x1, y1;

	/* Number of the town (if any) */
	p_ptr->town_num = wilderness[y][x].town;

	/* Set the base level */
	base_level = wilderness[y][x].level;

	/* Set the dungeon level */
	dun_level = 0;

	/* Set the monster generation level */
	monster_level = base_level;

	/* Set the object generation level */
	object_level = base_level;

	/* Create the town */
	if (p_ptr->town_num)
	{

		/* Reset the buildings */
		init_buildings();

		/* Initialize the town */
		if (border | corner)
			init_flags = INIT_CREATE_DUNGEON | INIT_ONLY_FEATURES;
		else
			init_flags = INIT_CREATE_DUNGEON;

		process_dungeon_file("t_info.txt", 0, 0, MAX_HGT, MAX_WID);
                       
	}
	else
	{
		int table_size = sizeof(terrain_table[0]) / sizeof(int);
		int roughness = 1; /* The roughness of the level. */
		int terrain = wilderness[y][x].terrain;;

		/* The outer wall is easy */
		if (terrain == TERRAIN_EDGE)
		{
			/* Create level background */
			for (y1 = 0; y1 < MAX_HGT; y1++)
			{
				for (x1 = 0; x1 < MAX_WID; x1++)
				{
					cave[y1][x1].feat = FEAT_PERM_SOLID;
				}
			}

			/* We are done already */
			return;
		}


		/* Hack -- Use the "simple" RNG */
		Rand_quick = TRUE;

		/* Hack -- Induce consistant town layout */
		Rand_value = wilderness[y][x].seed;

		if (!corner)
		{
			/* Create level background */
			for (y1 = 0; y1 < MAX_HGT; y1++)
			{
				for (x1 = 0; x1 < MAX_WID; x1++)
				{
					cave[y1][x1].feat = table_size / 2;
				}
			}
		}

		/*
		 * Initialize the four corners
		 * ToDo: calculate the medium height of the adjacent
		 * terrains for every corner.
		 */
		cave[1][1].feat = (byte)rand_int(table_size);
		cave[MAX_HGT-2][1].feat = (byte)rand_int(table_size);
		cave[1][MAX_WID-2].feat = (byte)rand_int(table_size);
		cave[MAX_HGT-2][MAX_WID-2].feat = (byte)rand_int(table_size);

		if (!corner)
		{
			/* x1, y1, x2, y2, num_depths, roughness */
			plasma_recursive(1, 1, MAX_WID-2, MAX_HGT-2, table_size-1, roughness);
		}

		for (y1 = 1; y1 < MAX_HGT-1; y1++)
		{
			for (x1 = 1; x1 < MAX_WID-1; x1++)
			{
				cave[y1][x1].feat = terrain_table[terrain][cave[y1][x1].feat];
			}
		}
	}

		/* Use the complex RNG */
		Rand_quick = FALSE;

	if (!corner)
	{
		/*
		 * Place roads in the wilderness
		 * ToDo: make the road a bit more interresting
		 */
		road = wilderness[y][x].road;

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
}

/*
 * Border of the wilderness area
 */
static border_type border;


/*
 * Build the wilderness area outside of the town.
 */
static void wilderness_gen()
{
	int i, y, x;
	bool daytime;
	cave_type *c_ptr;

	/* Init the wilderness */
	process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);

	x = p_ptr->wilderness_x;
	y = p_ptr->wilderness_y;

	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), NULL);

	/* North border */
	generate_area(y - 1, x, TRUE, FALSE);

	for (i = 1; i < MAX_WID - 1; i++)
	{
		border.north[i] = cave[MAX_HGT - 2][i].feat;
	}

	/* South border */
	generate_area(y + 1, x, TRUE, FALSE);

	for (i = 1; i < MAX_WID - 1; i++)
	{
		border.south[i] = cave[1][i].feat;
	}

	/* West border */
	generate_area(y, x - 1, TRUE, FALSE);

	for (i = 1; i < MAX_HGT - 1; i++)
	{
		border.west[i] = cave[i][MAX_WID - 2].feat;
	}

	/* East border */
	generate_area(y, x + 1, TRUE, FALSE);

	for (i = 1; i < MAX_HGT - 1; i++)
	{
		border.east[i] = cave[i][1].feat;
	}

	/* North west corner */
	generate_area(y - 1, x - 1, FALSE, TRUE);
	border.north_west = cave[MAX_HGT - 2][MAX_WID - 2].feat;

	/* North east corner */
	generate_area(y - 1, x + 1, FALSE, TRUE);
	border.north_east = cave[MAX_HGT - 2][1].feat;

	/* South west corner */
	generate_area(y + 1, x - 1, FALSE, TRUE);
	border.south_west = cave[1][MAX_WID - 2].feat;

	/* South east corner */
	generate_area(y + 1, x + 1, FALSE, TRUE);
	border.south_east = cave[1][1].feat;


	/* Create terrain of the current area */
	generate_area(y, x, FALSE, FALSE);


	/* Special boundary walls -- North */
	for (i = 0; i < MAX_WID; i++)
	{
		cave[0][i].feat = FEAT_PERM_SOLID;
		cave[0][i].mimic = border.north[i];
	}

	/* Special boundary walls -- South */
	for (i = 0; i < MAX_WID; i++)
	{
		cave[MAX_HGT - 1][i].feat = FEAT_PERM_SOLID;
		cave[MAX_HGT - 1][i].mimic = border.south[i];
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
		cave[i][MAX_WID - 1].feat = FEAT_PERM_SOLID;
		cave[i][MAX_WID - 1].mimic = border.east[i];
	}

	/* North west corner */
	cave[0][0].mimic = border.north_west;

	/* North east corner */
	cave[0][MAX_WID - 1].mimic = border.north_east;

	/* South west corner */
	cave[MAX_HGT - 1][0].mimic = border.south_west;

	/* South east corner */
	cave[MAX_HGT - 1][MAX_WID - 1].mimic = border.south_east;


	/* Day time */
	if ((bst(HOUR) < 18) && (bst(HOUR) > 6))
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

	/* Make some residents */
	for (i = 0; i < MIN_M_ALLOC_TN; i++)
	{
		/* Make a resident */
		(void)alloc_monster(1, TRUE);
	}

	/* Set rewarded quests to finished */
	for (i = 0; i < max_quests; i++)
	{
		if (quest[i].status == QUEST_STATUS_REWARDED)
			quest[i].status = QUEST_STATUS_FINISHED;
	}
}


/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(void)
{
	int i, j, k, y, x, y1, x1;

	int max_vault_ok = 2;

	cave_type *c_ptr;

	bool destroyed = FALSE;
	bool empty_level = FALSE;
	bool lit_level = FALSE;
	bool cavern = FALSE;
	int  laketype =0;

	int feat1, feat2;

	byte level_bg = FEAT_WALL_EXTRA;
	dun_data dun_body;

	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), NULL);

	/* Global data */
	dun = &dun_body;

	if (!max_panel_rows) max_vault_ok--;
	if (!max_panel_cols) max_vault_ok--;

	/* Randomize the dungeon creation values */
	dun_rooms = rand_range(DUN_ROOMS_MIN, DUN_ROOMS_MAX);
	dun_tun_rnd = rand_range(DUN_TUN_RND_MIN, DUN_TUN_RND_MAX);
	dun_tun_chg = rand_range(DUN_TUN_CHG_MIN, DUN_TUN_CHG_MAX);
	dun_tun_con = rand_range(DUN_TUN_CON_MIN, DUN_TUN_CON_MAX);
	dun_tun_pen = rand_range(DUN_TUN_PEN_MIN, DUN_TUN_PEN_MAX);
	dun_tun_jct = rand_range(DUN_TUN_JCT_MIN, DUN_TUN_JCT_MAX);

	if (ironman_empty_levels || (empty_levels && (randint(EMPTY_LEVEL) == 1)))
	{
		empty_level = TRUE;
		if (cheat_room)
			msg_print("Arena level.");
	}

	if (empty_level)
	{
	  if (magik(DUN_OPEN_WATER)) {
	    level_bg = FEAT_SHAL_WATER;
	    lit_level = TRUE;
	  } else if (magik(DUN_OPEN_CHASM)) {
	    level_bg = FEAT_DARK_PIT;
	    lit_level = TRUE;
	  } else if (magik(DUN_OPEN_CHAOS)) {
	    level_bg = FEAT_CHAOS_FOG;
	  }
	  else level_bg = FEAT_FLOOR;
	}

	/* Hack -- Start with basic granite (or not) */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
		  byte bg = level_bg;

		  /* Hack -- try to make something random if unspecified. */
		  if (!level_bg) {
		    switch ((x+y+randint(12)) % 12) {
		    case 0: case 1: case 2: case 3: case 4: case 5: case 6:
		    case 7: case 8:
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
		  cave[y][x].feat = bg;
		}
	}

	
	if ((rand_int(dun_level/2) > DUN_CAVERN) && (fractal_caves))
	{
		cavern=TRUE;
		
		/* make a large fractal cave in the middle of the dungeon */
		
		if (cheat_room)
			msg_print("Cavern on level.");
		
		build_cavern(); 				
	}


	/* Possible "destroyed" level */
	if ((dun_level > 10) && (rand_int(DUN_DEST) == 0) && (small_levels))
	{
		destroyed = TRUE;
		
		/* extra rubble around the place looks cool */
	        build_lake(3);
        }

	/* Make a lake some of the time */
	if ((rand_int(LAKE_LEVEL) == 0)&& (!empty_level)&& (!destroyed))
	{
		/* Lake of Water */
		if (dun_level > 30) laketype = 2;
		
		/* Lake of Lava */ 
		if (dun_level > 60) laketype = 1;
		
		if (laketype != 0)
		{
			if (cheat_room)
				msg_print("Lake on the level.");
			build_lake(laketype);		
		}
	}	

	if ((dun_level > DUN_CAVERN) && !empty_level &&
	    (laketype == 0) && !destroyed && (randint(200) < dun_level))
	{
		cavern = TRUE;
		
		/* make a large fractal cave in the middle of the dungeon */
		
		if (cheat_room)
			msg_print("Cavern on level.");
		
		build_cavern(); 				
	}
        
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
	for (i = 0; i < dun_rooms; i++)
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
			/* Attempt a "trivial" room */
			if (room_build(y, x, 1)) continue;
			continue;
		}

		/* Attempt an "unusual" room */
		if (rand_int(DUN_UNUSUAL) < dun_level)
		{
                
                /* Roll for room type */
                k = rand_int(100);

    	        /* Attempt a very unusual room */
	        if (rand_int(DUN_UNUSUAL) < dun_level)
		{
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

		/* Type 6 -- Monster pit (10%) */
		if ((k < 50) && room_build(y, x, 6)) continue;

        	/* Type 10 -- Random vault (15%) */
		if ((k < 65) && room_build(y, x, 10)) continue;
	    }

			/* Type 4 -- Large room (20%) */
			if ((k < 20) && room_build(y, x, 4)) continue;

			/* Type 3 -- Cross room (25%) */
			if ((k < 45) && room_build(y, x, 3)) continue;

			/* Type 2 -- Overlapping (30%) */
			if ((k < 75) && room_build(y, x, 2)) continue;
		       
			/* Type 11 -- Circular (10%) */
			if ((k < 85) && room_build(y, x, 11)) continue;
			
			/* Type 12 -- Crypt (15%) */
			if ((k < 100) && room_build(y, x, 12)) continue;
		}
		/*The deeper you are, the more cavelike the rooms are*/		
		k=randint(100);
		
		/*no caves when a cavern exists: they look bad */	
		if ((k<dun_level)&&(!cavern)&&(!empty_level)&&(laketype==0))		
			{
			/* Type 9 -- Fractal cave */
			if (room_build(y, x, 9)) continue;
			}
		else			
			/* Attempt a "trivial" room */
			if (room_build(y, x, 1)) continue;
		continue;
	}

	/* Make a hole in the dungeon roof sometimes at level 1 */	
	if (dun_level==1)
	{
		while (randint(DUN_MOS_DEN)==1)
		{
			place_trees(randint(cur_wid-2),randint(cur_hgt-2));
		}
	}
	
	/* Destroy the level if necessary */
	if (destroyed) destroy_level();
	
	/* Hack -- Add some rivers */
	if((randint(3)==1)&&(randint(dun_level)>5))
	{
	 	/* Choose water or lava */
		if (randint(MAX_DEPTH)-1 > dun_level)
		{
			feat1 = FEAT_DEEP_WATER;
			feat2 = FEAT_SHAL_WATER;
		}
		else
		{
			feat1 = FEAT_DEEP_LAVA;
			feat2 = FEAT_SHAL_LAVA;
		}
	 
	 
	 	/* Only add river if matches lake type or if have no lake at all */
	 	if (((laketype==1)&&(feat1==FEAT_DEEP_LAVA))||
	 		((laketype==2)&&(feat1==FEAT_DEEP_WATER))||
			(laketype==0))
	 	{
			add_river(feat1, feat2);
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
		
		/* Reset the arrays */
		dun->tunn_n = 0;
		dun->wall_n = 0;
		
		/* Connect the room to the previous room */
		if((randint(15)>dun_level)&&(randint(100)<75))
		{
			/* make catacomb-like tunnel */
			build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y,3,30);
		}
		else if (randint(dun_level)>25)
		{
			/* make cave-like tunnel */
			build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y,2,2);
		}
		else
		{
			/* make normal tunnel */
			build_tunnel(dun->cent[i].y, dun->cent[i].x, y, x);
		}

		/* Turn the tunnel into corridor */
		for (j = 0; j < dun->tunn_n; j++)
		{
			/* Access the grid */
			y = dun->tunn[j].y;
			x = dun->tunn[j].x;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Clear previous contents (if not a lake), add a floor */
			if ((c_ptr->feat<FEAT_DEEP_WATER)||(c_ptr->feat>FEAT_SHAL_LAVA))
			{
				c_ptr->feat = FEAT_FLOOR;
			}
		}

		/* Apply the piercings that we found */
		for (j = 0; j < dun->wall_n; j++)
		{
			/* Access the grid */
			y = dun->wall[j].y;
			x = dun->wall[j].x;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Clear previous contents, add up floor */
			c_ptr->feat = FEAT_FLOOR;

			/* Occasional doorway */
			if (rand_int(100) < dun_tun_pen)
			{
				/* Place a random door */
				place_random_door(y, x);
			}
		}
		
		/* Remember the "previous" room */
		y = dun->cent[i].y;
		x = dun->cent[i].x;
	}


	/* Place intersection doors      */
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

	/* Place 3 or 4 down stairs near some walls */
	alloc_stairs(FEAT_MORE, rand_range(3, 4), 3);
	
	/* Place 1 or 2 down shafts near some walls */	
	alloc_stairs(FEAT_SHAFT_DOWN, rand_range(0, 1), 3);

	/* Place 1 or 2 up stairs near some walls */
	alloc_stairs(FEAT_LESS, rand_range(1, 2), 3);

	/* Place 0 or 1 up shafts near some walls */	
	alloc_stairs(FEAT_SHAFT_UP, rand_range(0, 1), 3);

	/* Determine the character location */
	if (!new_player_spot())
		return FALSE;

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
		    (quest[i].level == dun_level) &&
			!(quest[i].flags & QUEST_FLAG_PRESET))
		{
			monster_race *r_ptr = &r_info[quest[i].r_idx];

			/* Hack -- "unique" monsters must be "unique" */
			if ((r_ptr->flags1 & RF1_UNIQUE) &&
			    (r_ptr->cur_num >= r_ptr->max_num))
			{
				/* The unique is already dead */
				quest[i].status = QUEST_STATUS_FINISHED;
			}
			else
			{
				bool group;

				/* Hard quests -> revive all monsters */
				if (ironman_hard_quests)
				{
					quest[i].cur_num = 0;
				}
				
				for (j = 0; j < (quest[i].max_num - quest[i].cur_num); j++)
				{
					for (k = 0; k < SAFE_MAX_ATTEMPTS; k++)
					{
						/* Find an empty grid */
						while (TRUE)
						{
							y = rand_int(cur_hgt);
							x = rand_int(cur_wid);
							if (!cave_naked_bold(y, x)) continue;
							if (distance(y, x, py, px) < 10) continue;
							else break;
						}

						if (r_ptr->flags1 & RF1_FRIENDS)
							group = FALSE;
						else
							group = TRUE;

						/* Try to place the monster */
						if (place_monster_aux(y, x, quest[i].r_idx, FALSE, group, FALSE, FALSE))
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
	i = MIN_M_ALLOC_LEVEL;

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

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ROOM, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ITEM, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, randnor(DUN_AMT_GOLD, 3));

	/* Put some altars */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_ALTAR, randnor(DUN_AMT_ALTAR, 1));

	if (empty_level && ((randint(DARK_EMPTY) != 1) || (randint(100) > dun_level)))
	{
		/* Lite the cave */
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				cave[y][x].info |= (CAVE_GLOW);
			}
		}
	}

    return TRUE;
}



/*
 * Generate a quest level
 */
static void quest_gen(void)
{
	int x, y;

	/* Start with perm walls */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			cave[y][x].feat = FEAT_PERM_SOLID;
		}
	}

	/* Set the quest level */
	base_level = quest[p_ptr->inside_quest].level;
	dun_level = base_level;
	object_level = base_level;
	monster_level = base_level;

	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), NULL);

	init_flags = INIT_CREATE_DUNGEON | INIT_ASSIGN;
	process_dungeon_file("q_info.txt", 0, 0, MAX_HGT, MAX_WID);
}


/*
 * Generates a random dungeon level                     -RAK-
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Hack -- allow auto-scumming via a gameplay option.
 */
void generate_cave(void)
{
	int tester_1, tester_2;
	int y, x, num;

	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Seed the RNG if appropriate */
	if (seed_dungeon) {
	  Rand_quick = TRUE;
	  Rand_value = seed_dungeon+dun_level;
	}

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;

		cptr why = NULL;


		/* XXX XXX XXX XXX */
		o_max = 1;
		m_max = 1;

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


		/* Set the base level */
		base_level = dun_level;

		/* Reset the monster generation level */
		monster_level = base_level;

		/* Reset the object generation level */
		object_level = base_level;

		/* Nothing special here yet */
		good_item_flag = FALSE;

		/* Nothing good here yet */
		rating = 0;

		/* Quest levels -KMW- */
		if (p_ptr->inside_quest)
		{
		    if (seed_dungeon) Rand_quick = FALSE;        
		    quest_gen();
		    if (seed_dungeon) Rand_quick = TRUE;
		}

		/* Build the town */
		else if (!dun_level)
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
			wilderness_gen();
		}

		/* Build a real level */
		else
		{
			if (((randint(SMALL_LEVEL)==1) && small_levels) || 
			    (always_small_levels || ironman_small_levels))
			{
				if (cheat_room)
					msg_print ("A 'small' dungeon level.");

				do
				{
					tester_1 = randint(MAX_HGT/SCREEN_HGT);
					tester_2 = randint(MAX_WID/SCREEN_WID);
				}
				while ((tester_1 == MAX_HGT/SCREEN_HGT) &&
				       (tester_2 == MAX_WID/SCREEN_WID));

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
			if (!cave_gen())
			{
				why = "could not place player";
				okay = FALSE;
			}
		}

		/* Calculate "average rating" */
		
		if (rating > 0)
		{
		   avg_rating += rating;
		   avg_rating /= 2;
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
		if (good_item_flag && !preserve) feeling = 1;

		/* It takes 1000 game turns for "feelings" to recharge */
		if ((!wizard) && ((turn - old_turn) < 1000)) feeling = 0;

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
		if ((auto_scum) && (num < 100) && !p_ptr->inside_quest)
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

	/* Reset the number of thefts on the level. -LM- */
	number_of_thefts_on_level = 0;

	if (seed_dungeon) {
	  Rand_quick = FALSE;
	}

}

