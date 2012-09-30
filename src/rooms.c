/*
 * File: rooms.c
 * Purpose: make rooms. Used by generate.c when creating dungeons.
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "generate.h"
#include "grid.h"
#include "rooms.h"


/*
 * Array of minimum room depths
 */
static const s16b roomdep[] =
{
	 0, /* 0  = Nothing */
	 1, /* 1  = Simple (33x11) */
	 1, /* 2  = Overlapping (33x11) */
	 3, /* 3  = Crossed (33x11) */
	 3, /* 4  = Large (33x11) */
	10, /* 5  = Monster nest (33x11) */
	10, /* 6  = Monster pit (33x11) */
	10, /* 7  = Lesser vault (33x22) */
	20, /* 8  = Greater vault (66x44) */
	 5, /* 9  = Fractal cave (52x28) */
	10, /* 10 = Random vault (44x22) */
	 3, /* 11 = Circular rooms (22x22) */
	10, /* 12 = Crypts (22x22) */
	 5, /* 13 = Large with feature (50x36) */
	 3, /* 14 = Large version 2 (33x11) */
	 3, /* 15 = Parallelagram room (37x15) */
};
	

/*
 * This funtion makes a very small room centred at (x0, y0)
 * This is used in crypts, and random elemental vaults.
 *
 * Note - this should be used only on allocated regions
 * within another room.
 */
static void build_small_room(int x0, int y0)
{
	/* Generate outer walls */
	generate_draw(y0 - 1, x0 - 1, y0 + 1, x0 + 1, FEAT_WALL_INNER);

	/* Place a secret door on one side */
	generate_door(y0 - 1, x0 - 1, y0 + 1, x0 + 1, TRUE);

	/* Add inner open space */
	cave[y0][x0].feat = FEAT_FLOOR;
}


/*
 * This function tunnels around a room if
 * it will cut off part of a cave system.
 */
static void check_room_boundary(int x1, int y1, int x2, int y2)
{
	int count, x, y;
	bool old_is_floor, new_is_floor;


	/* Initialize */
	count = 0;

	old_is_floor = get_is_floor(x1 - 1, y1);

	/*
	 * Count the number of floor-wall boundaries around the room
	 * Note: diagonal squares are ignored since the player can move diagonally
	 * to bypass these if needed.
	 */

	/* Above the top boundary */
	for (x = x1; x <= x2; x++)
	{
		new_is_floor = get_is_floor(x, y1 - 1);

		/* Increment counter if they are different */
		if (new_is_floor != old_is_floor) count++;

		old_is_floor = new_is_floor;
	}

	/* Right boundary */
	for (y = y1; y <= y2; y++)
	{
		new_is_floor = get_is_floor(x2 + 1, y);

		/* increment counter if they are different */
		if (new_is_floor != old_is_floor) count++;

		old_is_floor = new_is_floor;
	}

	/* Bottom boundary */
	for (x = x2; x >= x1; x--)
	{
		new_is_floor = get_is_floor(x, y2 + 1);

		/* increment counter if they are different */
		if (new_is_floor != old_is_floor) count++;

		old_is_floor = new_is_floor;
	}

	/* Left boundary */
	for (y = y2; y >= y1; y--)
	{
		new_is_floor = get_is_floor(x1 - 1, y);

		/* increment counter if they are different */
		if (new_is_floor != old_is_floor) count++;

		old_is_floor = new_is_floor;
	}

	/* If all the same, or only one connection exit. */
	if (count <= 2) return;


	/* Tunnel around the room so to prevent problems with caves */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			set_floor(x, y);
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
static bool room_alloc(int x, int y, bool crowded, int by0, int bx0, int *xx, int *yy)
{
	int temp, bx1, bx2, by1, by2, by, bx;

	/* Calculate number of room_map squares to allocate */

	/* temp is total number along width */
	temp = ((x - 1) / BLOCK_WID) + 1;

	/* bx2 = ending block */
	bx2 = temp / 2 + bx0;

	/* bx1 = starting block (Note: rounding taken care of here.) */
	bx1 = bx2 + 1 - temp;

	/* temp is total number along height */
	temp = ((y - 1) / BLOCK_HGT) + 1;

	/* by2 = ending block */
	by2 = temp / 2 + by0;

	/* by1 = starting block */
	by1 = by2 + 1 - temp;


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
	if (crowded) dun->crowded++;

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
 *  13 -- Large with feature
 *  14 -- Large version 2
 *  15 -- Parallelagram room
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

	/* Pick a room size */
	y1 = randint1(4);
	x1 = randint1(11);
	y2 = randint1(3);
	x2 = randint1(11);

	xsize = x1 + x2 + 1;
	ysize = y1 + y2 + 1;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 2, ysize + 2, FALSE, by0, bx0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));


	/* Get corner values */
	y1 = yval - ysize / 2;
	x1 = xval - xsize / 2;
	y2 = yval + (ysize - 1) / 2;
	x2 = xval + (xsize - 1) / 2;


	/* Generate new room */
	generate_room(y1 - 1, x1 - 1, y2 + 1, x2 + 1, light);

	/* Generate outer walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

	/* Hack -- Occasional pillar room */
	if (one_in_(20))
	{
		for (y = y1; y <= y2; y += 2)
		{
			for (x = x1; x <= x2; x += 2)
			{
				c_ptr = &cave[y][x];
				c_ptr->feat = FEAT_PILLAR;
			}
		}
	}

	/* Hack -- Occasional room with four pillars */
	else if (one_in_(40))
	{
		if ((y1 + 4 < y2) && (x1 + 4 < x2))
		{
			c_ptr = &cave[y1 + 1][x1 + 1];
			c_ptr->feat = FEAT_PILLAR;

			c_ptr = &cave[y1 + 1][x2 - 1];
			c_ptr->feat = FEAT_PILLAR;

			c_ptr = &cave[y2 - 1][x1 + 1];
			c_ptr->feat = FEAT_PILLAR;

			c_ptr = &cave[y2 - 1][x2 - 1];
			c_ptr->feat = FEAT_PILLAR;
		}
	}
	
	/* Hack -- Occasional room with rounded corners */
	else if (one_in_(40))
	{
		c_ptr = &cave[y1][x1];
		c_ptr->feat = FEAT_WALL_INNER;

		c_ptr = &cave[y1][x2];
		c_ptr->feat = FEAT_WALL_INNER;

		c_ptr = &cave[y2][x1];
		c_ptr->feat = FEAT_WALL_INNER;

		c_ptr = &cave[y2][x2];
		c_ptr->feat = FEAT_WALL_INNER;
	}
	
	/* Hack -- Occasional ragged-edge room */
	else if (one_in_(50))
	{
		for (y = y1 + 2; y <= y2 - 2; y += 2)
		{
			c_ptr = &cave[y][x1];
			c_ptr->feat = FEAT_PILLAR;
			c_ptr = &cave[y][x2];
			c_ptr->feat = FEAT_PILLAR;
		}
		for (x = x1 + 2; x <= x2 - 2; x += 2)
		{
			c_ptr = &cave[y1][x];
			c_ptr->feat = FEAT_PILLAR;
			c_ptr = &cave[y2][x];
			c_ptr->feat = FEAT_PILLAR;
		}
	}
	
	/* Hack -- Occasional divided room */
	else if (one_in_(50))
	{
		if (one_in_(2))
		{
			/* Horizontal wall */
			for (x = x1; x <= x2; x++)
			{
				cave[yval][x].feat = FEAT_WALL_INNER;
			}

			/* Prevent edge of wall from being tunneled */
			cave[yval][x1 - 1].feat = FEAT_WALL_SOLID;
			cave[yval][x2 + 1].feat = FEAT_WALL_SOLID;
		}
		else
		{
			/* Vertical wall */
			for (y = y1; y <= y2; y++)
			{
				cave[y][xval].feat = FEAT_WALL_INNER;
			}

			/* Prevent edge of wall from being tunneled */
			cave[y1 - 1][xval].feat = FEAT_WALL_SOLID;
			cave[y2 + 1][xval].feat = FEAT_WALL_SOLID;
		}

		place_random_door(yval, xval);
	}
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static void build_type2(int by0, int bx0)
{
	int			xval, yval;
	int			y1, x1, y2, x2;
	bool		light;
	
	int 		num, i;
	
#define MAX_ROOM_OVERLAY 	4

	coord		uleft[MAX_ROOM_OVERLAY];
	coord		lright[MAX_ROOM_OVERLAY];


	/* Try to allocate space for room. If fails, exit */
	if (!room_alloc(25, 11, FALSE, by0, bx0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Determine number of rooms to overlay */
	num = rand_range(2, MAX_ROOM_OVERLAY);

	/* Make num rooms */
	for (i = 0; i < num; i++)
	{
		/* Determine extents of a room */
		y1 = yval - randint0(5);
		y2 = yval + randint0(4);
		x1 = xval - randint0(12); 
		x2 = xval + randint0(11);

		/* Generate new room */
		generate_room(y1 - 1, x1 - 1, y2 + 1, x2 + 1, light);

		/* Generate outer walls */
		generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_OUTER);
		
		/* Save the bounds for later */
		uleft[i].x = x1;
		uleft[i].y = y1;
		lright[i].x = x2;
		lright[i].y = y2;
	}

	/* Make num rooms */
	for (i = 0; i < num; i++)
	{
		/* Generate inner floors */
		generate_fill(uleft[i].y, uleft[i].x,
		              lright[i].y, lright[i].x, FEAT_FLOOR);
	}
}



/*
 * Type 3 -- Cross shaped rooms
 *
 * Builds a room at a row, column coordinate
 *
 * Room "a" runs north/south, and Room "b" runs east/east
 * So the "central pillar" runs from x1a, y1b to x2a, y2b.
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
	cave_type   *c_ptr;


	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, by0, bx0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

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


	/* Generate new room (a) */
	generate_room(y1a - 1, x1a - 1, y2a + 1, x2a + 1, light);

	/* Generate new room (b) */
	generate_room(y1b - 1, x1b - 1, y2b + 1, x2b + 1, light);

	/* Generate outer walls (a) */
	generate_draw(y1a - 1, x1a - 1, y2a + 1, x2a + 1, FEAT_WALL_OUTER);

	/* Generate outer walls (b) */
	generate_draw(y1b - 1, x1b - 1, y2b + 1, x2b + 1, FEAT_WALL_OUTER);

	/* Generate inner floors (a) */
	generate_fill(y1a, x1a, y2a, x2a, FEAT_FLOOR);

	/* Generate inner floors (b) */
	generate_fill(y1b, x1b, y2b, x2b, FEAT_FLOOR);


	/* Special features (3/4) */
	switch (randint1(4))
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
			generate_door(y1b, x1a, y2b, x2a, TRUE);

			/* Place a treasure in the vault */
			place_object(yval, xval, FALSE, FALSE);

			/* Let's guard the treasure well */
			vault_monsters(yval, xval, rand_range(3, 4));

			/* Traps naturally */
			vault_traps(yval, xval, 4, 4, rand_range(2, 4));

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
				if (one_in_(3))
				{
					place_secret_door(yval, x1a - 1);
					place_secret_door(yval, x2a + 1);
					place_secret_door(y1b - 1, xval);
					place_secret_door(y2b + 1, xval);
				}
			}

			/* Occasionally put a "plus" in the center */
			else if (one_in_(3))
			{
				generate_plus(y1b, x1a, y2b, x2a, FEAT_WALL_INNER);
			}

			/* Occasionally put a pillar in the center */
			else if (one_in_(3))
			{
				c_ptr = &cave[yval][xval];
				c_ptr->feat = FEAT_PILLAR;
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
	int         y, x, y1, x1;
	int         y2, x2, tmp, yval, xval;
	bool        light;
	cave_type   *c_ptr;


	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, by0, bx0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	/* Generate new room */
	generate_room(y1 - 1, x1 - 1, y2 + 1, x2 + 1, light);

	/* Generate outer walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);


	/* The inner room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_INNER);


	/* Inner room variations */
	switch (randint1(5))
	{
		/* Just an inner room with a monster */
		case 1:
		{
			/* Open the inner room with a secret door */
			generate_door(y1 - 1, x1 - 1, y2 + 1, x2 + 1, TRUE);

			/* Place a monster in the room */
			vault_monsters(yval, xval, 1);

			break;
		}

		/* Treasure Vault (with a door) */
		case 2:
		{
			/* Open the inner room with a secret door */
			generate_door(y1 - 1, x1 - 1, y2 + 1, x2 + 1, TRUE);

			/* Place another inner room */
			generate_draw(yval - 1, xval - 1, yval + 1, xval + 1, FEAT_WALL_INNER);

			/* Open the inner room with a door */
			generate_door(yval - 1, xval - 1, yval + 1, xval + 1, FALSE);

			/* Monsters to guard the "treasure" */
			vault_monsters(yval, xval, rand_range(2, 4));

			/* Object (80%) */
			if (randint0(100) < 80)
			{
				place_object(yval, xval, FALSE, FALSE);
			}

			/* Stairs (20%) */
			else
			{
				place_random_stairs(yval, xval);
			}

			/* Traps to protect the treasure */
			vault_traps(yval, xval, 4, 10, rand_range(3, 6));

			break;
		}

		/* Inner pillar(s). */
		case 3:
		{
			/* Open the inner room with a secret door */
			generate_door(y1 - 1, x1 - 1, y2 + 1, x2 + 1, TRUE);

			/* Inner pillar */
			generate_fill(yval - 1, xval - 1, yval + 1, xval + 1, FEAT_WALL_INNER);

			/* Occasionally, two more Large Inner Pillars */
			if (one_in_(2))
			{
				tmp = randint1(2);
				
				/* Inner pillar */
				generate_fill(yval - 1, xval - 6 - tmp,
					 yval + 1, xval - 4 - tmp, FEAT_WALL_INNER);

				/* Inner pillar */
				generate_fill(yval - 1, xval + 4 + tmp,
					 yval + 1, xval + 6 + tmp, FEAT_WALL_INNER);
			}

			/* Occasionally, some Inner rooms */
			if (one_in_(3))
			{
				/* Inner rectangle */
				generate_draw(yval - 1, xval - 5, yval + 1, xval + 5, FEAT_WALL_INNER);
				
				/* Secret doors (random top/bottom) */
				place_secret_door(yval - 3 + (randint1(2) * 2), xval - 3);
				place_secret_door(yval - 3 + (randint1(2) * 2), xval + 3);

				/* Monsters */
				vault_monsters(yval, xval - 2, randint1(2));
				vault_monsters(yval, xval + 2, randint1(2));

				/* Objects */
				if (one_in_(3)) place_object(yval, xval - 2, FALSE, FALSE);
				if (one_in_(3)) place_object(yval, xval + 2, FALSE, FALSE);
			}

			break;
		}

		/* Maze inside. */
		case 4:
		{
			/* Open the inner room with a secret door */
			generate_door(y1 - 1, x1 - 1, y2 + 1, x2 + 1, TRUE);

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
			vault_monsters(yval, xval - 5, randint1(3));
			vault_monsters(yval, xval + 5, randint1(3));

			/* Traps make them entertaining. */
			vault_traps(yval, xval - 3, 2, 8, randint1(3));
			vault_traps(yval, xval + 3, 2, 8, randint1(3));

			/* Mazes should have some treasure too. */
			vault_objects(yval, xval, 3);

			break;
		}

		/* Four small rooms. */
		case 5:
		{
			/* Inner "cross" */
			generate_plus(y1, x1, y2, x2, FEAT_WALL_INNER);

			/* Doors into the rooms */
			if (randint0(100) < 50)
			{
				int i = randint1(10);
				place_secret_door(y1 - 1, xval - i);
				place_secret_door(y1 - 1, xval + i);
				place_secret_door(y2 + 1, xval - i);
				place_secret_door(y2 + 1, xval + i);
			}
			else
			{
				int i = randint1(3);
				place_secret_door(yval + i, x1 - 1);
				place_secret_door(yval - i, x1 - 1);
				place_secret_door(yval + i, x2 + 1);
				place_secret_door(yval - i, x2 + 1);
			}

			/* Treasure, centered at the center of the cross */
			vault_objects(yval, xval, rand_range(3, 4));

			/* Gotta have some monsters. */
			vault_monsters(yval + 1, xval - 4, randint1(4));
			vault_monsters(yval + 1, xval + 4, randint1(4));
			vault_monsters(yval - 1, xval - 4, randint1(4));
			vault_monsters(yval - 1, xval + 4, randint1(4));

			break;
		}
	}
}


/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a monster nest or monster pit or
 * the given type.
 *
 * None of the pits/nests are allowed to include "unique" monsters.
 */


/*
 * Monster validation macro
 *
 * Line 1 -- forbid town monsters
 * Line 2 -- forbid uniques
 * Line 3 -- forbid aquatic monsters
 */
#define vault_monster_okay(I) \
	(monster_dungeon(I) && \
	 !(r_info[I].flags1 & RF1_UNIQUE) && \
	 !(r_info[I].flags7 & RF7_AQUATIC))


/* Race index for "monster pit (clone)" */
static int vault_aux_race;

/* Race index for "monster pit (symbol clone)" */
static char vault_aux_char;

/* Breath mask for "monster pit (dragon)" */
static u32b vault_aux_dragon_mask4;


/*
 * Helper monster selection function
 */
static bool vault_aux_simple(int r_idx)
{
	/* Okay */
	return (vault_monster_okay(r_idx));
}


/*
 * Helper function for "monster nest (jelly)"
 */
static bool vault_aux_jelly(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

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

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

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

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

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

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require "priest" or Angel */
	if ((r_ptr->d_char != 'A') && !strstr((r_name + r_ptr->name), "riest"))
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

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require a Zephyr Hound or a dog */
	if (!strchr("CZ", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster nest (mimic)"
 */
static bool vault_aux_mimic(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require mimic */
	if (!strchr("!|$?=", r_ptr->d_char)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster nest (clone)"
 */
static bool vault_aux_clone(int r_idx)
{
	return (r_idx == vault_aux_race);
}


/*
 * Helper function for "monster nest (symbol clone)"
 */
static bool vault_aux_symbol(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Decline incorrect symbol */
	if (r_ptr->d_char != vault_aux_char) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (orc)"
 */
static bool vault_aux_orc(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require orc */
	if (!(r_ptr->flags3 & RF3_ORC)) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (troll)"
 */
static bool vault_aux_troll(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require troll */
	if (!(r_ptr->flags3 & RF3_TROLL)) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (giant)"
 */
static bool vault_aux_giant(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require giant */
	if (!(r_ptr->flags3 & RF3_GIANT)) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (dragon)"
 */
static bool vault_aux_dragon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require dragon */
	if (!(r_ptr->flags3 & RF3_DRAGON)) return (FALSE);

	/* Hack -- Require correct "breath attack" */
	if (r_ptr->flags4 != vault_aux_dragon_mask4) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_demon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require demon */
	if (!(r_ptr->flags3 & RF3_DEMON)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (lovecraftian)"
 */
static bool vault_aux_cthulhu(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Require eldritch horror */
	if (!(r_ptr->flags4 & RF4_ELDRITCH_HORROR)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "monster pit (clone)"
 */
static void vault_prep_clone(void)
{
	/* Apply the monster restriction */
	get_mon_num_prep(vault_aux_simple, NULL);

	/* Pick a race to clone */
	vault_aux_race = get_mon_num(p_ptr->depth + 10);

	/* Remove the monster restriction */
	get_mon_num_prep(NULL, NULL);
}


/*
 * Helper function for "monster pit (symbol clone)"
 */
static void vault_prep_symbol(void)
{
	int r_idx;

	/* Apply the monster restriction */
	get_mon_num_prep(vault_aux_simple, NULL);

	/* Pick a race to clone */
	r_idx = get_mon_num(p_ptr->depth + 10);

	/* Remove the monster restriction */
	get_mon_num_prep(NULL, NULL);

	/* Extract the symbol */
	vault_aux_char = r_info[r_idx].d_char;
}


/*
 * Helper function for "monster pit (dragon)"
 */
static void vault_prep_dragon(void)
{
	/* Pick dragon type */
	switch (randint0(6))
	{
		/* Black */
		case 0:
		{
			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF4_BR_ACID;

			/* Done */
			break;
		}

		/* Blue */
		case 1:
		{
			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF4_BR_ELEC;

			/* Done */
			break;
		}

		/* Red */
		case 2:
		{
			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF4_BR_FIRE;

			/* Done */
			break;
		}

		/* White */
		case 3:
		{
			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF4_BR_COLD;

			/* Done */
			break;
		}

		/* Green */
		case 4:
		{
			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF4_BR_POIS;

			/* Done */
			break;
		}

		/* Multi-hued */
		default:
		{
			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = (RF4_BR_ACID | RF4_BR_ELEC |
			                          RF4_BR_FIRE | RF4_BR_COLD |
			                          RF4_BR_POIS);

			/* Done */
			break;
		}
	}
}


typedef struct vault_aux_type vault_aux_type;


struct vault_aux_type
{
	cptr name;
	bool (*hook_func)(int r_idx);
	void (*prep_func)(void);
	int level;
	int chance;
};


static const vault_aux_type *pick_vault_type(const vault_aux_type *l_ptr)
{
	int tmp, total;

	const vault_aux_type *n_ptr;

	/* Calculate the total possibilities */
	for (n_ptr = l_ptr, total = 0; TRUE; n_ptr++)
	{
		/* Note end */
		if (!n_ptr->name) break;

		/* Ignore excessive depth */
		if (n_ptr->level > p_ptr->depth) continue;

		/* Count this possibility */
		total += n_ptr->chance * MAX_DEPTH / (p_ptr->depth - n_ptr->level + 5);
	}

	/* Pick a random type */
	tmp = randint0(total);

	/* Find this type */
	for (n_ptr = l_ptr, total = 0; TRUE; n_ptr++)
	{
		/* Note end */
		if (!n_ptr->name) break;

		/* Ignore excessive depth */
		if (n_ptr->level > p_ptr->depth) continue;

		/* Count this possibility */
		total += n_ptr->chance * MAX_DEPTH / (p_ptr->depth - n_ptr->level + 5);

		/* Found the type */
		if (tmp < total) break;
	}

	return (n_ptr->name ? n_ptr : NULL);
}


static const vault_aux_type nest_types[] =
{
	{"clone",	vault_aux_clone,	vault_prep_clone,	7,	2},
	{"jelly",	vault_aux_jelly,	NULL,			7,	6},
	{"symbol clone",vault_aux_symbol,	vault_prep_symbol,	40,	2},
	{"mimic",	vault_aux_mimic,	NULL,			45,	6},
	{"lovecraftian",vault_aux_cthulhu,	NULL,			80,	2},
	{"kennel",	vault_aux_kennel,	NULL,			50,	2},
	{"animal",	vault_aux_animal,	NULL,			50,	4},
	{"chapel",	vault_aux_chapel,	NULL,			90,	8},
	{"undead",	vault_aux_undead,	NULL,			90,	4},
	{NULL,		NULL,			NULL,			0,	0},
};


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
static void build_type5(int by0, int bx0)
{
	int y, x, y1, x1, y2, x2, xval, yval;
	int i;
	int what[64];

	int align = 0;

	const vault_aux_type *n_ptr = pick_vault_type(nest_types);

	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, TRUE, by0, bx0, &xval, &yval)) return;

	/* No type available */
	if (!n_ptr) return;

	/* Process a preparation function if necessary */
	if (n_ptr->prep_func) (*(n_ptr->prep_func))();

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	/* Generate new room */
	generate_room(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FALSE);

	/* Generate outer walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);


	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_INNER);


	/* Open the inner room with a secret door */
	generate_door(y1 - 1, x1 - 1, y2 + 1, x2 + 1, TRUE);

	/* Prepare allocation table */
	get_mon_num_prep(n_ptr->hook_func, NULL);

	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		int r_idx = 0, attempts = 100;

		while (attempts--)
		{
			/* Get a (hard) monster type */
			r_idx = get_mon_num(p_ptr->depth + 10);

			/* Decline incorrect alignment */
			if (((align < 0) && (r_info[r_idx].flags3 & RF3_GOOD)) ||
				 ((align > 0) && (r_info[r_idx].flags3 & RF3_EVIL)))
			{
				continue;
			}

			/* Accept this monster */
			break;
		}

		/* Notice failure */
		if (!r_idx || !attempts) return;

		/* Note the alignment */
		if (r_info[r_idx].flags3 & RF3_GOOD) align++;
		else if (r_info[r_idx].flags3 & RF3_EVIL) align--;

		what[i] = r_idx;
	}

	/* Describe */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster nest (%s)", n_ptr->name);
	}

	/* Increase the level rating */
	rating += 10;

	/* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
	if ((p_ptr->depth <= 40) && (randint1(p_ptr->depth * p_ptr->depth + 50) < 300))
	{
		good_item_flag = TRUE;
	}

	/* Place some monsters */
	for (y = yval - 2; y <= yval + 2; y++)
	{
		for (x = xval - 9; x <= xval + 9; x++)
		{
			int r_idx = what[randint0(64)];

			/* Place that "random" monster (no groups) */
			(void)place_monster_aux(y, x, r_idx, FALSE, FALSE, FALSE, FALSE);
		}
	}
}


static const vault_aux_type pit_types[] =
{
	{"orc",		vault_aux_orc,		NULL,			7,	1},
	{"troll",	vault_aux_troll,	NULL,			35,	4},
	{"giant",	vault_aux_giant,	NULL,			70,	4},
	{"lovecraftian",vault_aux_cthulhu,	NULL,			90,	8},
	{"clone",	vault_aux_symbol,	vault_prep_symbol,	85,	3},
	{"chapel",	vault_aux_chapel,	NULL,			85,	4},
	{"dragon",	vault_aux_dragon,	vault_prep_dragon,	80,	4},
	{"demon",	vault_aux_demon,	NULL,   		90,	4},
	{NULL,		NULL,			NULL,			0,	0},
};


/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type organized in the room.
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
	int y, x, y1, x1, y2, x2, xval, yval;
	int i, j;

	int what[16];

	int align = 0;

	const vault_aux_type *n_ptr = pick_vault_type(pit_types);

	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, TRUE, by0, bx0, &xval, &yval)) return;

	/* No type available */
	if (!n_ptr) return;

	/* Process a preparation function if necessary */
	if (n_ptr->prep_func) (*(n_ptr->prep_func))();

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	/* Generate new room */
	generate_room(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FALSE);

	/* Generate outer walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_INNER);

	/* Open the inner room with a secret door */
	generate_door(y1 - 1, x1 - 1, y2 + 1, x2 + 1, TRUE);

	/* Prepare allocation table */
	get_mon_num_prep(n_ptr->hook_func, NULL);

	/* Pick some monster types */
	for (i = 0; i < 16; i++)
	{
		int r_idx = 0, attempts = 100;

		while (attempts--)
		{
			/* Get a (hard) monster type */
			r_idx = get_mon_num(p_ptr->depth + 10);

			/* Decline incorrect alignment */
			if (((align < 0) && (r_info[r_idx].flags3 & RF3_GOOD)) ||
			    ((align > 0) && (r_info[r_idx].flags3 & RF3_EVIL)))
			{
				continue;
			}

			/* Accept this monster */
			break;
		}

		/* Notice failure */
		if (!r_idx || !attempts) return;

		/* Note the alignment */
		if (r_info[r_idx].flags3 & RF3_GOOD) align++;
		else if (r_info[r_idx].flags3 & RF3_EVIL) align--;

		what[i] = r_idx;
	}

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

	/* Message */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Monster pit (%s)", n_ptr->name);
	}

	/* Select the entries */
	for (i = 0; i < 8; i++)
	{
		/* Every other entry */
		what[i] = what[i * 2];

		if (cheat_hear)
		{
			/* Message */
			msg_print(r_name + r_info[what[i]].name);
		}
	}

	/* Increase the level rating */
	rating += 10;

	/* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
	if ((p_ptr->depth <= 40) && (randint1(p_ptr->depth * p_ptr->depth + 50) < 300))
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
	for (i = 0; i <= transno % 4; i++)
	{
		/* rotate by 90 degrees */
		temp = *x;
		*x = -(*y);
		*y = temp;
	}

	if (transno / 4)
	{
		/* Reflect depending on status of 3rd bit. */
		*x = -(*x);
	}

	/* Add offsets so vault stays in the first quadrant */
	*x += xoffset;
	*y += yoffset;
}


/*
 * Hack -- fill in "vault" rooms
 */
static void build_vault(int yval, int xval, int ymax, int xmax, cptr data,
		int xoffset, int yoffset, int transno)
{
	int dx, dy, x, y, i, j;

	cptr t;

	cave_type *c_ptr;


	/* Place dungeon features and objects */
	for (t = data, dy = 0; dy < ymax; dy++)
	{
		for (dx = 0; dx < xmax; dx++, t++)
		{
			/* prevent loop counter from being overwritten */
			i = dx;
			j = dy;

			/* Flip / rotate */
			coord_trans(&i, &j, xoffset, yoffset, transno);

			/* Extract the location */
			if (transno % 2)
			{
				/* no swap of x/y */
				x = xval - (xmax / 2) + i;
				y = yval - (ymax / 2) + j;
			}
			else
			{
				/* swap of x/y */
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
				case '%':
				{
					/* Granite wall (outer) */
					c_ptr->feat = FEAT_WALL_OUTER;
					break;
				}
				
				case '#':
				{
					/* Granite wall (inner) */
					c_ptr->feat = FEAT_WALL_INNER;
					break;
				}

				
				case 'X':
				{
					/* Permanent wall (inner) */
					c_ptr->feat = FEAT_PERM_INNER;
					break;
				}
				
				case '*':
				{
					/* Treasure/trap */
					if (randint0(100) < 75)
					{
						place_object(y, x, FALSE, FALSE);
					}
					else
					{
						place_trap(y, x);
					}
					break;
				}
				
				case '+':
				{
					/* Secret doors */
					place_secret_door(y, x);
					break;
				}

				case '^':
				{
					/* Trap */
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
			/* prevent loop counter from being overwritten */
			i = dx;
			j = dy;

			/* Flip / rotate */
			coord_trans(&i, &j, xoffset, yoffset, transno);

			/* Extract the location */
			if (transno % 2)
			{
				/* no swap of x/y */
				x = xval - (xmax / 2) + i;
				y = yval - (ymax / 2) + j;
			}
			else
			{
				/* swap of x/y */
				x = xval - (ymax / 2) + i;
				y = yval - (xmax / 2) + j;
			}

			/* Hack -- skip "non-grids" */
			if (*t == ' ') continue;

			/* Analyze the symbol */
			switch (*t)
			{
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
					if (randint0(100) < 50)
					{
						monster_level = base_level + 3;
						place_monster(y, x, TRUE, TRUE);
						monster_level = base_level;
					}
					if (randint0(100) < 50)
					{
						object_level = base_level + 7;
						place_object(y, x, FALSE, FALSE);
						object_level = base_level;
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
					object_level = base_level + 12;
					place_object(y, x, TRUE, FALSE);
					object_level = base_level;
				}
				break;
			}
		}
	}
}


/*
 * Type 7 -- simple vaults (see "v_info.txt")
 */
static void build_type7(int by0, int bx0)
{
	vault_type *v_ptr = NULL;
	int dummy = 0;
	int x, y;
	int xval, yval;
	int xoffset, yoffset;
	int transno;

	/* Pick a lesser vault */
	while (dummy < SAFE_MAX_ATTEMPTS)
	{
		dummy++;

		/* Access a random vault record */
		v_ptr = &v_info[randint0(max_v_idx)];

		/* Accept the first lesser vault */
		if (v_ptr->typ == 7) break;
	}

	/* No lesser vault found */
	if (!v_ptr) return;

	if (!one_in_(3))
	{
		/* pick type of transformation (0-7) */
		transno = randint0(8);
	}
	else
	{
		/* No change */
		transno = 3;
	}

	/* calculate offsets */
	x = v_ptr->wid;
	y = v_ptr->hgt;

	coord_trans(&x, &y, 0, 0, transno);

	if (x < 0)
	{
		xoffset = -x - 1;
	}
	else
	{
		xoffset = 0;
	}

	if (y < 0)
	{
		yoffset = -y - 1;
	}
	else
	{
		yoffset = 0;
	}

	/* Try to allocate space for room. */
	if (!room_alloc(ABS(x), ABS(y), FALSE, by0, bx0, &xval, &yval)) return;

	if (dummy >= SAFE_MAX_ATTEMPTS)
	{
		if (cheat_room)
			msg_print("Warning! Could not place lesser vault!");

		return;
	}


#ifdef FORCE_V_IDX
	v_ptr = &v_info[2];
#endif

	/* Message */
	if (cheat_room) msg_format("%s", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
		(randint1((p_ptr->depth - 40) * (p_ptr->depth - 40) + 50) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr->hgt, v_ptr->wid,
	            v_text + v_ptr->text, xoffset, yoffset, transno);
}


/*
 * Type 8 -- greater vaults (see "v_info.txt")
 */
static void build_type8(int by0, int bx0)
{
	vault_type *v_ptr = NULL;
	int dummy = 0;
	int xval, yval;
	int x, y;
	int transno;
	int xoffset, yoffset;

	/* Pick a greater vault */
	while (dummy < SAFE_MAX_ATTEMPTS)
	{
		dummy++;

		/* Access a random vault record */
		v_ptr = &v_info[randint0(max_v_idx)];

		/* Accept the first greater vault */
		if (v_ptr->typ == 8) break;
	}

	/* No greater vault found */
	if (!v_ptr) return;

	if (!one_in_(3))
	{
		/* pick type of transformation (0-7) */
		transno = randint0(8);
	}
	else
	{
		/* No Change */
		transno = 3;
	}

	/* calculate offsets */
	x = v_ptr->wid;
	y = v_ptr->hgt;

	coord_trans(&x, &y, 0, 0, transno);

	if (x < 0)
	{
		xoffset = - x - 1;
	}
	else
	{
		xoffset = 0;
	}

	if (y < 0)
	{
		yoffset = - y - 1;
	}
	else
	{
		yoffset = 0;
	}

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(ABS(x), ABS(y), FALSE, by0, bx0, &xval, &yval)) return;

	if (dummy >= SAFE_MAX_ATTEMPTS)
	{
		if (cheat_room)
			msg_print("Warning! Could not place greater vault!");

		return;
	}


#ifdef FORCE_V_IDX
	v_ptr = &v_info[rand_range(76, 79)];
#endif

	/* Message */
	if (cheat_room) msg_format("%s", v_name + v_ptr->name);

	/* Boost the rating */
	rating += v_ptr->rat;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint1((p_ptr->depth - 40) * (p_ptr->depth - 40) + 50) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Hack -- Build the vault */
	build_vault(yval, xval, v_ptr->hgt, v_ptr->wid,
	            v_text + v_ptr->text, xoffset, yoffset, transno);
}



/*
 * Type 9 -- Fractal cave system
 */
static void build_type9(int by0, int bx0)
{
	int grd, roug, cutoff, xsize, ysize, xhsize, yhsize, y0, x0;

	bool done, light;

	/* get size: note 'Evenness'*/
	xsize = randint1(22) * 2 + 6;
	ysize = randint1(10) * 2 + 6;
	
	/* round to make sizes even */
	xhsize = (xsize - 1) / 2;
	yhsize = (ysize - 1) / 2;
	xsize = xhsize * 2;
	ysize = yhsize * 2;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 1, ysize + 1, FALSE, by0, bx0, &x0, &y0)) return;

	light = done = FALSE;

	if (p_ptr->depth <= randint1(25)) light = TRUE;

	while (!done)
	{
		/* Note: size must be even or there are rounding problems
		* This causes the tunnels not to connect properly to the room */

		/* testing values for these parameters feel free to adjust */
		grd = 1 << (randint0(4));

		/* want average of about 16 */
		roug = randint1(8) * randint1(4);

		/* about size/2 */
		cutoff = randint1(xsize / 4) + randint1(ysize / 4) +
		         randint1(xsize / 4) + randint1(ysize / 4);

		/* make it */
		generate_hmap(y0, x0, xsize, ysize, grd, roug, cutoff);

		/* Convert to normal format + clean up */
		done = generate_fracave(y0, x0, xsize, ysize, cutoff, light);
	}
}



/*
 * Routine used by the random vault creators to add a door to a location
 * Note that range checking has to be done in the calling routine.
 *
 * The doors must be INSIDE the allocated region.
 */
static void add_door(int x, int y)
{
	/* Need to have a wall in the center square */
	if (cave[y][x].feat != FEAT_WALL_OUTER) return;

	/* look at:
	 *  x#x
	 *  .#.
	 *  x#x
	 *
	 *  where x=don't care
	 *  .=floor, #=wall
	 */

	if ((cave[y - 1][x].feat == FEAT_FLOOR) &&
		(cave[y + 1][x].feat == FEAT_FLOOR) &&
	    (cave[y][x - 1].feat == FEAT_WALL_OUTER) &&
		(cave[y][x + 1].feat == FEAT_WALL_OUTER))
	{
		/* secret door */
		place_secret_door(y, x);

		/* set boundarys so don't get wide doors */
		cave[y][x - 1].feat = FEAT_WALL_SOLID;
		cave[y][x + 1].feat = FEAT_WALL_SOLID;
	}


	/* look at:
	 *  x#x
	 *  .#.
	 *  x#x
	 *
	 *  where x = don't care
	 *  .=floor, #=wall
	 */
	if ((cave[y - 1][x].feat == FEAT_WALL_OUTER) &&
	    (cave[y + 1][x].feat == FEAT_WALL_OUTER) &&
	    (cave[y][x - 1].feat == FEAT_FLOOR) &&
	    (cave[y][x + 1].feat == FEAT_FLOOR))
	{
		/* secret door */
		place_secret_door(y, x);

		/* set boundarys so don't get wide doors */
		cave[y - 1][x].feat = FEAT_WALL_SOLID;
		cave[y + 1][x].feat = FEAT_WALL_SOLID;
	}
}


/*
 * Routine that fills the empty areas of a room with treasure and monsters.
 */
static void fill_treasure(int x1, int x2, int y1, int y2, int difficulty)
{
	int x, y, cx, cy, size;
	s32b value;

	/* center of room:*/
	cx = (x1 + x2) / 2;
	cy = (y1 + y2) / 2;

	/* Rough measure of size of vault= sum of lengths of sides */
	size = ABS(x2 - x1) + ABS(y2 - y1);

	for (x = x1; x <= x2; x++)
	{
		for (y = y1; y <= y2; y++)
		{
			/*
			 * Thing added based on distance to center of vault
			 * Difficulty is 1-easy to 10-hard
			 */
			value = ((((s32b)(distance(cx, cy, x, y))) * 100) / size) +
			        randint1(10) - difficulty;

			/* hack- empty square part of the time */
			if ((randint1(100) - difficulty * 3) > 50) value = 20;

			/* if floor, shallow water and lava */
			if ((cave[y][x].feat == FEAT_FLOOR) ||
			    (cave[y][x].feat == FEAT_SHAL_WATER) ||
			    (cave[y][x].feat == FEAT_SHAL_LAVA))
			{
				/* The smaller 'value' is, the better the stuff */
				if (value < 0)
				{
					/* Meanest monster + treasure */
					monster_level = base_level + 40;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					object_level = base_level + 20;
					place_object(y, x, TRUE, FALSE);
					object_level = base_level;
				}
				else if (value < 5)
				{
					/* Mean monster + treasure */
					monster_level = base_level + 20;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					object_level = base_level + 10;
					place_object(y, x, TRUE, FALSE);
					object_level = base_level;
				}
				else if (value < 10)
				{
					/* Monster */
					monster_level = base_level + 9;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
				}
				else if (value < 17)
				{
					/* Intentional Blank space */

					/*
					 * (Want some of the vault to be empty
					 * so have room for group monsters.
					 * This is used in the hack above to lower
					 * the density of stuff in the vault.)
					 */
				}
				else if (value < 23)
				{
					/* Object or trap */
					if (randint0(100) < 25)
					{
						place_object(y, x, FALSE, FALSE);
					}
					else
					{
						place_trap(y, x);
					}
				}
				else if (value < 30)
				{
					/* Monster and trap */
					monster_level = base_level + 5;
					place_monster(y, x, TRUE, TRUE);
					monster_level = base_level;
					place_trap(y, x);
				}
				else if (value < 40)
				{
					/* Monster or object */
					if (randint0(100) < 50)
					{
						monster_level = base_level + 3;
						place_monster(y, x, TRUE, TRUE);
						monster_level = base_level;
					}
					if (randint0(100) < 50)
					{
						object_level = base_level + 7;
						place_object(y, x, FALSE, FALSE);
						object_level = base_level;
					}
				}
				else if (value < 50)
				{
					/* Trap */
					place_trap(y, x);
				}
				else
				{
					/* Various Stuff */

					/* 20% monster, 40% trap, 20% object, 20% blank space */
					if (randint0(100) < 20)
					{
						place_monster(y, x, TRUE, TRUE);
					}
					else if (randint0(100) < 50)
					{
						place_trap(y, x);
					}
					else if (randint0(100) < 50)
					{
						place_object(y, x, FALSE, FALSE);
					}
				}
			}
		}
	}
}


/*
 * This function creates a random vault that looks like a
 * collection of bubbles.  It works by getting a set of
 * coordinates that represent the center of each bubble.
 * The entire room is made by seeing which bubble center
 * is closest. If two centers are equidistant then the grid
 * is a wall, otherwise it is a floor.  The only exception is
 * for squares really near a center, these are always floor.
 * (It looks better than without this check.)
 */
static void build_bubble_vault(int x0, int y0, int xsize, int ysize)
{
	/* number of bubbles */
	#define BUBBLENUM 10

	/* array of center points of bubbles */
	coord center[BUBBLENUM];

	int i, x, y;
	u16b min1, min2, temp;

	/* Offset from center to top left hand corner */
	int xhsize = xsize / 2;
	int yhsize = ysize / 2;

	cave_type *c_ptr;


	if (cheat_room) msg_print("Bubble Vault");

	/* Allocate center of bubbles */
	for (i = 0; i < BUBBLENUM; i++)
	{
		/* Uniqueness check removed */

		center[i].x = (u16b) rand_range(1, xsize - 2);
		center[i].y = (u16b) rand_range(1, ysize - 2);
	}

	/* Set vault flags */
	generate_vault(y0 - yhsize , x0 - xhsize,
		 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1);

	/* Draw outer walls */
	generate_draw(y0 - yhsize , x0 - xhsize,
		 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1, FEAT_WALL_OUTER);

	/* Fill in middle with bubbles */
	for (x = 1; x < xsize - 1; x++)
	{
		for (y = 1; y < ysize - 1; y++)
		{
			/* Get distances to two closest centers */

			/* initialize */
			min1 = distance(x, y, center[0].x, center[0].y);
			min2 = distance(x, y, center[1].x, center[1].y);

			if (min1 > min2)
			{
				/* swap if in wrong order */
				temp = min1;
				min1 = min2;
				min2 = temp;
			}

			/* Scan the rest */
			for (i = 2; i < BUBBLENUM; i++)
			{
				temp = distance(x, y, center[i].x, center[i].y);

				if (temp < min1)
				{
					/* smallest */
					min2 = min1;
					min1 = temp;
				}
				else if (temp < min2)
				{
					/* second smallest */
				 	min2 = temp;
				}
			}
			
			c_ptr = &cave[y0 - yhsize + y][x0 - xhsize + x];
			
			if (((min2 - min1) <= 2) && (!(min1 < 3)))
			{
				/* Boundary at midpoint+ not at inner region of bubble */
				c_ptr->feat = FEAT_WALL_OUTER;
			}
			else
			{
				/* middle of a bubble */
				c_ptr->feat = FEAT_FLOOR;
			}
		}
	}

	/* Try to add some random doors */
	for (i = 0; i < 500; i++)
	{
		x = rand_range(x0 - xhsize + 1, x0 - xhsize + xsize - 2);
		y = rand_range(y0 - yhsize + 1, y0 - yhsize + ysize - 2);
		add_door(x, y);
	}

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x0 - xhsize + 1, x0 - xhsize + xsize - 2,
		 y0 - yhsize + 1, y0 - yhsize + ysize - 2, randint1(5));
}


/*
 * Overlay a rectangular room given its bounds
 * This routine is used by build_room_vault
 * The area inside the walls is not touched:
 * only granite is removed- normal walls stay
 */
static void build_room(int x1, int x2, int y1, int y2)
{
	int x, y, xsize, ysize, temp;
	
	cave_type *c_ptr;
	
	/* Check if rectangle has no width */
	if ((x1 == x2) || (y1 == y2)) return;

	/* initialize */
	if (x1 > x2)
	{
		/* Swap boundaries if in wrong order */
		temp = x1;
		x1 = x2;
		x2 = temp;
	}

	if (y1 > y2)
	{
		/* Swap boundaries if in wrong order */
		temp = y1;
		y1 = y2;
		y2 = temp;
	}

	/* get total widths */
	xsize = x2 - x1;
	ysize = y2 - y1;

	/* Set vault flags */
	generate_vault(y1, x1, y2, x2);
	
	/* Draw outer wall */
	generate_draw(y1, x1, y2, x2, FEAT_WALL_OUTER);

	/* Middle */
	for (x = 1; x < xsize; x++)
	{
		for (y = 1; y < ysize; y++)
		{
			c_ptr = &cave[y1 + y][x1 + x];
			
			if (c_ptr->feat == FEAT_WALL_EXTRA)
			{
				/* Clear the untouched region */
				c_ptr->feat = FEAT_FLOOR;				
			}
		}
	}
}


/*
 * Create a random vault that looks like a collection of overlapping rooms
 */
static void build_room_vault(int x0, int y0, int xsize, int ysize)
{
	int i, x1, x2, y1, y2, xhsize, yhsize;

	/* get offset from center */
	xhsize = xsize / 2;
	yhsize = ysize / 2;

	if (cheat_room) msg_print("Room Vault");

	/* fill area so don't get problems with arena levels */
	generate_fill(y0 - yhsize, x0 - xhsize,
		 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1, FEAT_WALL_EXTRA);
	
	/* Clear the CAVE_ICKY flag in the region */
	clear_vault(y0 - yhsize, x0 - xhsize,
		 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1);

	/* add ten random rooms */
	for (i = 0; i < 10; i++)
	{
		x1 = randint1(xhsize) * 2 + x0 - xhsize;
		x2 = randint1(xhsize) * 2 + x0 - xhsize;
		y1 = randint1(yhsize) * 2 + y0 - yhsize;
		y2 = randint1(yhsize) * 2 + y0 - yhsize;
		build_room(x1, x2, y1, y2);
	}

	/* Add some random doors */
	for (i = 0; i < 500; i++)
	{
		x1 = rand_range(x0 - xhsize + 1, x0 - xhsize + xsize - 2);
		y1 = rand_range(y0 - yhsize + 1, y0 - yhsize + ysize - 2);
		add_door(x1, y1);
	}

	/* Fill with monsters and treasure, high difficulty */
	fill_treasure(x0 - xhsize + 1, x0 - xhsize + xsize - 1,
		 y0 - yhsize + 1, y0 - yhsize + ysize - 1, rand_range(5, 10));
}


/*
 * Create a random vault out of a fractal cave
 */
static void build_cave_vault(int x0, int y0, int xsiz, int ysiz)
{
	int grd, roug, cutoff, xhsize, yhsize, xsize, ysize;

	bool done;

	int x, y;
	cave_type *c_ptr;

	/* round to make sizes even */
	xhsize = (xsiz - 1) / 2;
	yhsize = (ysiz - 1) / 2;
	xsize = xhsize * 2;
	ysize = yhsize * 2;

	if (cheat_room) msg_print("Cave Vault");

	done = FALSE;

	while (!done)
	{
		/* testing values for these parameters feel free to adjust */
		grd = 1 << randint0(4);

		/* want average of about 16 */
		roug = randint1(8) * randint1(4);

		/* about size/2 */
		cutoff = randint1(xsize / 4) + randint1(ysize / 4) +
		         randint1(xsize / 4) + randint1(ysize / 4);

		/* make it */
		generate_hmap(y0, x0, xsize, ysize, grd, roug, cutoff);

		/* Convert to normal format+ clean up */
		done = generate_fracave(y0, x0, xsize, ysize, cutoff, FALSE);
	}

	/* Set vault flags */
	for (y = y0 - yhsize; y < y0 - yhsize + ysize; y++)
	{
		for (x = x0 - xhsize; x < x0 - xhsize + xsize; x++)
		{
			c_ptr = &cave[y][x];
			
			/* Is it a floor? */
			if (c_ptr->feat == FEAT_FLOOR)
			{
				/* Set the icky flag */
				c_ptr->info |= CAVE_ICKY;
			} 
		}
	}

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x0 - xhsize + 1, x0 - xhsize + xsize - 1,
		 y0 - yhsize + 1, y0 - yhsize + ysize - 1, randint1(5));
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
 *    (x, y) = (2j + x1, 2k + y1)   j = 0,...,m-1    k = 0,...,n-1
 * and the edges are the vertical and horizontal nearest neighbors.
 *
 * The spanning tree is created by performing a suitably randomized
 * depth-first traversal of the graph. The only adjustable parameter
 * is the randint0(3) below; it governs the relative density of
 * twists and turns in the labyrinth: smaller number, more twists.
 */
static void r_visit(int y1, int x1, int y2, int x2,
                    int node, int dir, int *visited)
{
	int i, j, m, n, temp, x, y, adj[4];

	/* dimensions of vertex array */
	m = (x2 - x1) / 2 + 1;
	n = (y2 - y1) / 2 + 1;

	/* mark node visited and set it to a floor */
	visited[node] = 1;

	x = 2 * (node % m) + x1;
	y = 2 * (node / m) + y1;
	cave_set_feat(y, x, FEAT_FLOOR);

	/* setup order of adjacent node visits */
	if (one_in_(3))
	{
		/* pick a random ordering */
		for (i = 0; i < 4; i++)
		{
			adj[i] = i;
		}
		
		for (i = 0; i < 4; i++)
		{
			j = randint0(4);
			
			/* Swap */
			temp = adj[i];
			adj[i] = adj[j];
			adj[j] = temp;
		}
		
		dir = adj[0];
	}
	else
	{
		/* pick a random ordering with dir first */
		adj[0] = dir;
		
		for (i = 1; i < 4; i++)
		{
			adj[i] = i;
		}
		for (i = 1; i < 4; i++)
		{
			j = randint1(3);
			
			/* Swap */
			temp = adj[i];
			adj[i] = adj[j];
			adj[j] = temp;
		}
	}

	for (i = 0; i < 4; i++)
	{
		switch (adj[i])
		{
			case 0:
			{
				/* (0,+) - check for bottom boundary */
				if ((node / m < n - 1) && (visited[node + m] == 0))
				{
					cave_set_feat(y + 1, x, FEAT_FLOOR);
					r_visit(y1, x1, y2, x2, node + m, dir, visited);
				}
				break;
			}
			
			case 1:
			{
				/* (0,-) - check for top boundary */
				if ((node / m > 0) && (visited[node - m] == 0))
				{
					cave_set_feat(y - 1, x, FEAT_FLOOR);
					r_visit(y1, x1, y2, x2, node - m, dir, visited);
				}
				break;
			}
			
			case 2:
			{
				/* (+,0) - check for right boundary */
				if ((node % m < m - 1) && (visited[node + 1] == 0))
				{
					cave_set_feat(y, x + 1, FEAT_FLOOR);
					r_visit(y1, x1, y2, x2, node + 1, dir, visited);
				}
				break;
			}
			
			case 3:
			{
				/* (-,0) - check for left boundary */
				if ((node % m > 0) && (visited[node - 1] == 0))
				{
					cave_set_feat(y, x - 1, FEAT_FLOOR);
					r_visit(y1, x1, y2, x2, node - 1, dir, visited);
				}
			}
		}
	}
}


static void build_maze_vault(int x0, int y0, int xsize, int ysize)
{
	int dy, dx;
	int y1, x1, y2, x2;
	int m, n, num_vertices, *visited;

	if (cheat_room) msg_print("Maze Vault");

	/* Pick a random room size - randomized by calling routine */
	dy = ysize / 2 - 1;
	dx = xsize / 2 - 1;

	y1 = y0 - dy;
	x1 = x0 - dx;
	y2 = y0 + dy;
	x2 = x0 + dx;

	/* generate the room */	
	generate_vault(y1 - 1, x1 - 1, y2 + 1, x2 + 1);
	
	/* Draw outer walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_OUTER);
	
	/* Fill room with inner walls */
	generate_fill(y1, x1, y2, x2, FEAT_WALL_INNER);

	/* dimensions of vertex array */
	m = dx + 1;
	n = dy + 1;
	num_vertices = m * n;

	/* initialize array of visited vertices */
	C_MAKE(visited, num_vertices, int);

	/* traverse the graph to create a spaning tree, pick a random root */
	r_visit(y1, x1, y2, x2, randint0(num_vertices), 0, visited);

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x1, x2, y1, y2, randint1(5));

	/* rnfree(visited, num_vertices * sizeof(int)); */
	C_FREE(visited, num_vertices, int);
}


/*
 * Build a "mini" checkerboard vault
 *
 * This is done by making a permanent wall maze and setting
 * the diagonal sqaures of the checker board to be granite.
 * The vault has two entrances on opposite sides to guarantee
 * a way to get in even if the vault abuts a side of the dungeon.
 */
static void build_mini_c_vault(int x0, int y0, int xsize, int ysize)
{
 	int dy, dx;
 	int y1, x1, y2, x2, y, x, total;
	int m, n, num_vertices;
	int *visited;
	
	cave_type *c_ptr;

 	if (cheat_room) msg_print("Mini Checker Board Vault");

 	/* Pick a random room size */
	dy = ysize / 2 - 1;
	dx = xsize / 2 - 1;

	y1 = y0 - dy;
 	x1 = x0 - dx;
 	y2 = y0 + dy;
 	x2 = x0 + dx;

	/* generate the room */	
	generate_vault(y1 - 1, x1 - 1, y2 + 1, x2 + 1);
	
	/* Fill room with perm walls. */
	generate_fill(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_PERM_INNER);

	/* dimensions of vertex array */
	m = dx + 1;
	n = dy + 1;
	num_vertices = m * n;

	/* initialize array of visited vertices */
	C_MAKE(visited, num_vertices, int);

	/* traverse the graph to create a spannng tree, pick a random root */
	r_visit(y1, x1, y2, x2, randint0(num_vertices), 0, visited);

	/* Make it look like a checker board vault */
	for (x = x1; x <= x2; x++)
	{
		for (y = y1; y <= y2; y++)
		{
			c_ptr = &cave[y][x];
			
			total = x - x1 + y - y1;
			
			/* If total is odd- and is a floor then make a wall */
			if ((total % 2 == 1) && (c_ptr->feat == FEAT_FLOOR))
			{
				c_ptr->feat = FEAT_WALL_INNER;
			}
		}
	}

	/* Make a couple of entrances */
	if (one_in_(2))
	{
		/* left and right */
		y = randint1(dy) + dy / 2;
		cave[y1 + y][x1 - 1].feat = FEAT_WALL_OUTER;
		cave[y1 + y][x2 + 1].feat = FEAT_WALL_OUTER;
	}
	else
	{
		/* top and bottom */
		x = randint1(dx) + dx / 2;
		cave[y1 - 1][x1 + x].feat = FEAT_WALL_OUTER;
		cave[y2 + 1][x1 + x].feat = FEAT_WALL_OUTER;
	}

	/* Fill with monsters and treasure, highest difficulty */
	fill_treasure(x1, x2, y1, y2, 10);

	/* rnfree(visited, num_vertices * sizeof(int)); */
	C_FREE(visited, num_vertices, int);
}


/*
 * Build a town/ castle by using a recursive algorithm.
 * Basically divide each region to create smaller regions.
 * When they get too small, stop.
 *
 * The power variable is a measure of how well defended a region is.
 * This alters the possible choices when a region is split.
 */
static void build_recursive_room(int x1, int y1, int x2, int y2, int power)
{
	int xsize, ysize;
	int x, y;
	int choice;

	/* Temp variables */
	int t1, t2, t3, t4;

	xsize = x2 - x1;
	ysize = y2 - y1;

	if ((power < 3) && (xsize > 12) && (ysize > 12))
	{
		/* Need outside wall +keep */
		choice = 1;
	}
	else
	{
		if (power < 10)
		{
			/* Make rooms + subdivide */
			if ((randint1(10) > 2) && (xsize < 8) && (ysize < 8))
			{
				choice = 4;
			}
			else
			{
				choice = randint1(2) + 1;
			}
		}
		else
		{
			/* Mostly subdivide */
			choice = randint1(3) + 1;
		}
	}

	/* Based on the choice made above, do something */

	switch (choice)
	{
		case 1:
		{
			/* Outer walls */
			generate_draw(y1, x1, y2, x2, FEAT_WALL_OUTER);

			/* Make a couple of entrances */
			if (one_in_(2))
			{
				/* left and right */
				y = randint1(ysize) + y1;
				cave[y][x1].feat = FEAT_FLOOR;
				cave[y][x2].feat = FEAT_FLOOR;
			}
			else
			{
				/* top and bottom */
				x = randint1(xsize) + x1;
				cave[y1][x].feat = FEAT_FLOOR;
				cave[y2][x].feat = FEAT_FLOOR;
			}

			/* Select size of keep */
			t1 = randint1(ysize / 3) + y1;
			t2 = y2 - randint1(ysize / 3);
			t3 = randint1(xsize / 3) + x1;
			t4 = x2 - randint1(xsize / 3);

			/* Do outside areas */

			/* Above and below keep */
			build_recursive_room(x1 + 1, y1 + 1, x2 - 1, t1, power + 1);
			build_recursive_room(x1 + 1, t2, x2 - 1, y2, power + 1);

			/* Left and right of keep */
			build_recursive_room(x1 + 1, t1 + 1, t3, t2 - 1, power + 3);
			build_recursive_room(t4, t1 + 1, x2 - 1, t2 - 1, power + 3);

			/* Make the keep itself: */
			x1 = t3;
			x2 = t4;
			y1 = t1;
			y2 = t2;
			xsize = x2 - x1;
			ysize = y2 - y1;
			power += 2;

			/* Fall through */
		}
		
		case 4:
		{
			/* Try to build a room */
			if ((xsize < 3) || (ysize < 3))
			{
				generate_fill(y1, x1, y2 - 1, x2 - 1, FEAT_WALL_INNER);

				/* Too small */
				return;
			}

			/* Make outside walls */
			generate_draw(y1 + 1, x1 + 1, y2 - 1, x2 - 1, FEAT_WALL_INNER);

			/* Make a door */
			y = rand_range(y1 + 1, y1 + ysize - 2);

			if (one_in_(2))
			{
				/* left */
				cave[y][x1 + 1].feat = FEAT_FLOOR;
			}
			else
			{
				/* right */
				cave[y][x2 - 1].feat = FEAT_FLOOR;
			}

			/* Build the room */
			build_recursive_room(x1 + 2, y1 + 2, x2 - 2, y2 - 2, power + 3);
			break;
		}
		
		case 2:
		{
			/* Try and divide vertically */
			if (xsize < 3)
			{
				generate_fill(y1, x1, y2 - 1, x2 - 1, FEAT_WALL_INNER);
				
				/* Too small */
				return;
			}

			t1 = rand_range(x1 + 2, x1 + xsize - 1);
			build_recursive_room(x1, y1, t1, y2, power - 2);
			build_recursive_room(t1 + 1, y1, x2, y2, power - 2);
			break;
		}
		
		case 3:
		{
			/* Try and divide horizontally */
			if (ysize < 3)
			{
				generate_fill(y1, x1, y2 - 1, x2 - 1, FEAT_WALL_INNER);
				
				/* Too small */
				return;
			}

			t1 = rand_range(y1 + 2, y1 + ysize - 1);
			build_recursive_room(x1, y1, x2, t1, power - 2);
			build_recursive_room(x1, t1 + 1, x2, y2, power - 2);
			break;
		}
	}
}


/* Build a castle */

/*
 * Driver routine: clear the region and call the recursive
 * room routine.
 *
 * This makes a vault that looks like a castle/ city in the dungeon.
 */
static void build_castle_vault(int x0, int y0, int xsize, int ysize)
{
	int dy, dx;
	int y1, x1, y2, x2;

	/* Pick a random room size */
	dy = ysize / 2 - 1;
	dx = xsize / 2 - 1;

	y1 = y0 - dy;
 	x1 = x0 - dx;
 	y2 = y0 + dy;
 	x2 = x0 + dx;

	if (cheat_room) msg_print("Castle Vault");

	/* generate the room */
	generate_vault(y1 - 1, x1 - 1, y2 + 1, x2 + 1);
	
	/* Make the whole room floor */
	generate_fill(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_FLOOR);

	/* Make the castle */
	build_recursive_room(x1, y1, x2, y2, randint1(5));

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x1, x2, y1, y2, randint1(3));
}


/*
 * Add outer wall to a floored region
 * Note: no range checking is done so must be inside dungeon
 * This routine also stomps on doors
 */
static void add_outer_wall(int x, int y, int light,
                           int x1, int y1, int x2, int y2)
{
	int i, j;
	cave_type *c_ptr;

	if (!in_bounds(y, x)) return;

	c_ptr = &cave[y][x];

	/* hack- check to see if square has been visited before
	* if so, then exit (use room flag to do this) */
	if (c_ptr->info & CAVE_ROOM) return;

	/* set room flag */
	c_ptr->info |= CAVE_ROOM;

	if (c_ptr->feat == FEAT_FLOOR)
	{
		for (i = -1; i <= 1; i++)
		{
			for (j = -1; j <= 1; j++)
			{
				if ((x + i >= x1) && (x + i <= x2) &&
					 (y + j >= y1) && (y + j <= y2))
				{
					add_outer_wall(x + i, y + j, light, x1, y1, x2, y2);	
				}
			}
		}

		if (light) c_ptr->info |= CAVE_GLOW;
	}
	else if (c_ptr->feat == FEAT_WALL_EXTRA)
	{
		/* Set bounding walls */
		c_ptr->feat = FEAT_WALL_OUTER;
		if (light) c_ptr->info |= CAVE_GLOW;
	}
	else if (c_ptr->feat == FEAT_PERM_OUTER)
	{
		/* Set bounding walls */
		if (light) c_ptr->info |= CAVE_GLOW;
	}
}


/*
 * Hacked distance formula - gives the 'wrong' answer.
 * Used to build crypts
 */
static int dist2(int x1, int y1, int x2, int y2,
                 int h1, int h2, int h3, int h4)
{
	int dx, dy;
	dx = ABS(x2 - x1);
	dy = ABS(y2 - y1);

	/* Basically this works by taking the normal pythagorean formula
	 * and using an expansion to express this in a way without the
	 * square root.  This approximate formula is then perturbed to give
	 * the distorted results.  (I found this by making a mistake when I was
	 * trying to fix the circular rooms.)
	 */

	/* h1-h4 are constants that describe the metric */
	if (dx >= 2 * dy) return (dx + (dy * h1) / h2);
	if (dy >= 2 * dx) return (dy + (dx * h1) / h2);
	return (((dx + dy) * 128) / 181 +
	        (dx * dx / (dy * h3) + dy * dy / (dx * h3)) * h4);
	/* 128/181 is approx. 1/sqrt(2) */
}


/*
 * Build target vault.
 * This is made by two concentric "crypts" with perpendicular
 * walls creating the cross-hairs.
 */
static void build_target_vault(int x0, int y0, int xsize, int ysize)
{
	int rad, x, y;

	/* Make a random metric */
	int h1, h2, h3, h4;
	h1 = randint1(32) - 16;
	h2 = randint1(16);
	h3 = randint1(32);
	h4 = randint1(32) - 16;

	if (cheat_room) msg_print("Target Vault");

	/* work out outer radius */
	if (xsize > ysize)
	{
		rad = ysize / 2;
	}
	else
	{
		rad = xsize / 2;
	}
	
	/* Outer walls */
	generate_draw(y0 - rad, x0 - rad, y0 + rad, x0 + rad, FEAT_WALL_EXTRA);

	/* Make floor */
	for (x = x0 - rad + 1; x <= x0 + rad - 1; x++)
	{
		for (y = y0 - rad + 1; y <= y0 + rad - 1; y++)
		{
			/* clear room flag */
			cave[y][x].info &= ~(CAVE_ROOM);

			/* Vault - so is "icky" */
			cave[y][x].info |= CAVE_ICKY;

			if (dist2(y0, x0, y, x, h1, h2, h3, h4) <= rad - 1)
			{
				/* inside- so is floor */
				cave[y][x].feat = FEAT_FLOOR;
			}
			else
			{
				/* make granite outside so arena works */
				cave[y][x].feat = FEAT_WALL_EXTRA;
			}
		}
	}

	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0, y0, FALSE, x0 - rad - 1, y0 - rad - 1,
	               x0 + rad + 1, y0 + rad + 1);

	/* Add inner wall */
	for (x = x0 - rad / 2; x <= x0 + rad / 2; x++)
	{
		for (y = y0 - rad / 2; y <= y0 + rad / 2; y++)
		{
			if (dist2(y0, x0, y, x, h1, h2, h3, h4) == rad / 2)
			{
				/* Make an internal wall */
				cave[y][x].feat = FEAT_WALL_INNER;
			}
		}
	}

	/* Make perpendicular walls */
	generate_plus(y0 - rad, x0 - rad, y0 + rad, x0 + rad, FEAT_WALL_INNER);

	/* Make inner vault */
	generate_draw(y0 - 1, x0 - 1, y0 + 1, x0 + 1, FEAT_WALL_INNER);
		
	/* Make inner room */
	cave[y0][x0].feat = FEAT_FLOOR;


	/* Add doors to vault */
	/* get two distances so can place doors relative to centre */
	x = (rad - 2) / 4 + 1;
	y = rad / 2 + x;

	add_door(x0 + x, y0);
	add_door(x0 + y, y0);
	add_door(x0 - x, y0);
	add_door(x0 - y, y0);
	add_door(x0, y0 + x);
	add_door(x0, y0 + y);
	add_door(x0, y0 - x);
	add_door(x0, y0 - y);

	/* Fill with stuff - medium difficulty */
	fill_treasure(x0 - rad, x0 + rad, y0 - rad, y0 + rad, rand_range(3, 6));
}


/*
 * This routine uses a modified version of the lake code to make a
 * distribution of some terrain type over the vault.  This type
 * depends on the dungeon depth.
 *
 * Miniature rooms are then scattered across the vault.
 */
static void build_elemental_vault(int x0, int y0, int xsiz, int ysiz)
{
	int grd, roug;
	int c1, c2, c3;
	bool done = FALSE;
	int xsize, ysize, xhsize, yhsize, i;
	int type;


	if (cheat_room) msg_print("Elemental Vault");

	/* round to make sizes even */
	xhsize = xsiz / 2;
	yhsize = ysiz / 2;
	xsize = xhsize * 2;
	ysize = yhsize * 2;

	if (p_ptr->depth < 25)
	{
		/* Earth vault  (Rubble) */
		type = LAKE_EEARTH;
	}
	else if (p_ptr->depth < 50)
	{
		/* Air vault (Trees) */
		type = LAKE_EAIR;
	}
	else if (p_ptr->depth < 75)
	{
		/* Water vault (shallow water) */
		type = LAKE_EWATER;
	}
	else
	{
		/* Fire vault (shallow lava) */
		type = LAKE_EFIRE;
	}

	while (!done)
	{
		/* testing values for these parameters: feel free to adjust */
		grd = 1 << (randint0(3));

		/* want average of about 16 */
		roug = randint1(8) * randint1(4);

		/* Make up size of various componants */
		/* Floor */
		c3 = 2 * xsize / 3;

		/* Deep water/lava */
		c1 = randint0(c3 / 2) + randint0(c3 / 2) - 5;

		/* Shallow boundary */
		c2 = (c1 + c3) / 2;

		/* make it */
		generate_hmap(y0, x0, xsize, ysize, grd, roug, c3);

		/* Convert to normal format + clean up */
		done = generate_lake(y0, x0, xsize, ysize, c1, c2, c3, type);
	}

	/* Set icky flag because is a vault */
	generate_vault(y0 - yhsize , x0 - xhsize,
		 y0 - yhsize + ysize, x0 - xhsize + xsize);

	/* make a few rooms in the vault */
	for (i = 1; i <= (xsize * ysize) / 50; i++)
	{
		build_small_room(x0 + randint0(xsize - 4) - xsize / 2 + 2,
		                 y0 + randint0(ysize - 4) - ysize / 2 + 2);
	}

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x0 - xhsize + 1, x0 - xhsize + xsize - 1,
	              y0 - yhsize + 1, y0 - yhsize + ysize - 1, randint1(5));
}


/*
 * This makes a vault that has many micro-rooms.
 */
static void build_micro_room_vault(int x0, int y0, int xsize, int ysize)
{
	int dy, dx;
	int y1, x1, y2, x2;

	int i, j;

	/* Pick a random room size */
	dy = ysize / 2 - 1;
	dx = xsize / 2 - 1;

	y1 = y0 - dy;
 	x1 = x0 - dx;
 	y2 = y0 + dy;
 	x2 = x0 + dx;

	if (cheat_room) msg_print("Micro-Room Vault");

	/* generate the room */
	generate_vault(y1 - 1, x1 - 1, y2 + 1, x2 + 1);
	
	/* Make the whole room floor */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);
	
	/* Make outer walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_OUTER);
	
	/* Make a grid of "features" */
	for (j = y1 + 2; j < y2 - 1; j += 4)
	{
		for (i = x1 + 2; i < x2 - 1; i +=4)
		{
			if (one_in_(2))
			{
				/* A tiny room */
				build_small_room(i, j);
			}
			else if (one_in_(2))
			{
				/* 1/4 chance for a pillar */
				generate_fill(j - 1, i - 1, j + 1, i + 1, FEAT_WALL_INNER); 
			}
			else if (one_in_(2))
			{
				/* 1/8 chance for a plus */
				generate_plus(j - 1, i - 1, j + 1, i + 1, FEAT_WALL_INNER);
			}
		}
	}
	
	/* Make a set of walls to break up the flow */
	for (j = y1; j < y2 - 1; j += 4)
	{
		for (i = x1; i < x2 - 1; i +=4)
		{
			if (one_in_(2))
			{
				cave[j][i].feat = FEAT_WALL_INNER;
			}
		}
	}
	
	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x1, x2, y1, y2, randint1(5));
}


/*
 * Type 10 -- Random vault
 */
static void build_type10(int by0, int bx0)
{
	int y0, x0, xsize, ysize, vtype;

	/* 
	 * Get size
	 * Big enough to look good, small enough to be fairly common.
	 */
	xsize = rand_range(22, 44);
	ysize = rand_range(11, 22);

	/* Allocate in room_map.  If will not fit, exit */
	if (!room_alloc(xsize + 1, ysize + 1, FALSE, by0, bx0, &x0, &y0)) return;

	/* Boost the rating- higher than lesser vaults and lower than greater vaults */
	rating += 10;

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
	    (randint1((p_ptr->depth - 40) * (p_ptr->depth - 40) + 1) < 400))
	{
		good_item_flag = TRUE;
	}

	/* Select type of vault */
	vtype = randint1(9);

	switch (vtype)
	{
		/* Build an appropriate room */
		case 1: build_bubble_vault(x0, y0, xsize, ysize); break;
		case 2: build_room_vault(x0, y0, xsize, ysize); break;
		case 3: build_cave_vault(x0, y0, xsize, ysize); break;
		case 4: build_maze_vault(x0, y0, xsize, ysize); break;
		case 5: build_mini_c_vault(x0, y0, xsize, ysize); break;
		case 6: build_castle_vault(x0, y0, xsize, ysize); break;
		case 7: build_target_vault(x0, y0, xsize, ysize); break;
		case 8: build_elemental_vault(x0, y0, xsize, ysize); break;
		case 9: build_micro_room_vault(x0, y0, xsize, ysize); break;
		/* I know how to add a few more... give me some time. */

		/* Paranoia */
		default: return;
	}
}


/*
 * Type 11 -- Vertical oval room.
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
	if (randint1(p_ptr->depth) <= 15) light = TRUE;

	rad = randint0(9);

	/* Allocate in room_map.  If will not fit, exit */
	if (!room_alloc(rad * 2 + 1, rad * 2 + 1, FALSE, by0, bx0, &x0, &y0)) return;

	/* Make circular floor */
	for (x = x0 - rad; x <= x0 + rad; x++)
	{
		for (y = y0 - rad; y <= y0 + rad; y++)
		{
			if (distance(y0, x0, y, x) <= rad - 1)
			{
				/* inside- so is floor */
				cave[y][x].feat = FEAT_FLOOR;
			}
			else if (distance(y0, x0, y, x) <= rad + 1)
			{
				/* make granite outside so arena works */
				cave[y][x].feat = FEAT_WALL_EXTRA;
			}
		}
	}

	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0, y0, light, x0 - rad, y0 - rad, x0 + rad, y0 + rad);
}


/*
 * Type 12 -- Crypt room.
 * For every grid in the possible square, check the (fake) distance.
 * If it's less than the radius, make it a room square.
 *
 * When done fill from the inside to find the walls,
 */
static void build_type12(int by0, int bx0)
{
	int rad, x, y, x0, y0;
	int light = FALSE;
	bool emptyflag = TRUE;

	/* Make a random metric */
	int h1, h2, h3, h4;
	h1 = randint1(32) - 16;
	h2 = randint1(16);
	h3 = randint1(32);
	h4 = randint1(32) - 16;

	/* Occasional light */
	if (randint1(p_ptr->depth) <= 5) light = TRUE;

	rad = randint1(9);

	/* Allocate in room_map.  If will not fit, exit */
	if (!room_alloc(rad * 2 + 3, rad * 2 + 3, FALSE, by0, bx0, &x0, &y0)) return;

	/* Add outer wall */
	generate_draw(y0 - rad, x0 - rad, y0 + rad, x0 + rad, FEAT_WALL_EXTRA);

	/* Make floor */
	for (x = x0 - rad + 1; x <= x0 + rad - 1; x++)
	{
		for (y = y0 - rad + 1; y <= y0 + rad - 1; y++)
		{
			/* clear room flag */
			cave[y][x].info &= ~(CAVE_ROOM);

			if (dist2(y0, x0, y, x, h1, h2, h3, h4) <= rad - 1)
			{
				/* inside - so is floor */
				cave[y][x].feat = FEAT_FLOOR;
			}
			else if (distance(y0, x0, y, x) < 3)
			{
				cave[y][x].feat = FEAT_FLOOR;
			}
			else
			{
				/* make granite outside so arena works */
				cave[y][x].feat = FEAT_WALL_EXTRA;
			}
		}
	}

	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0, y0, light, x0 - rad - 1, y0 - rad - 1,
	               x0 + rad + 1, y0 + rad + 1);

	/* Check to see if there is room for an inner vault */
	for (x = x0 - 2; x <= x0 + 2; x++)
	{
		for (y = y0 - 2; y <= y0 + 2; y++)
		{
			if (cave[y][x].feat != FEAT_FLOOR)
			{
				/* Wall in the way */
				emptyflag = FALSE;
			}
		}
	}

	if (emptyflag && one_in_(2))
	{
		/* Build the vault */
		build_small_room(x0, y0);

		/* Place a treasure in the vault */
		place_object(y0, x0, FALSE, FALSE);

		/* Let's guard the treasure well */
		vault_monsters(y0, x0, rand_range(2, 4));

		/* Traps naturally */
		vault_traps(y0, x0, 4, 4, rand_range(2, 4));
	}
}


/*
 * Type 13 -- Rectangular room with central fractal feature
 */
static void build_type13(int by0, int bx0)
{
	int grd, roug, xsize, ysize, xhsize, yhsize, y0, x0;

	int c1, c2, c3;
	int type = LAKE_CAVERN;
	
	bool done;

	/* get size: note 'Evenness'*/
	xsize = randint1(22) * 2 + 6;
	ysize = randint1(15) * 2 + 6;
	
	/* round to make sizes even */
	xhsize = (xsize - 1) / 2;
	yhsize = (ysize - 1) / 2;
	xsize = xhsize * 2;
	ysize = yhsize * 2;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 1, ysize + 1, FALSE, by0, bx0, &x0, &y0)) return;

	done = FALSE;
	
	/* Pick the type */	
	switch (randint0(5))
	{
		case 0:
		{
			/* Water */
			type = LAKE_WATER;
			break;
		}
		
		case 1:
		{
			/* Lava */
			type = LAKE_LAVA;
			break;
		}
		
		case 2:
		{
			/* Rock */
			type = LAKE_ROCK;
			break;
		}
		
		case 3:
		{
			/* Rubble - oooh treasure */
			type = LAKE_RUBBLE;
			break;
		}
		
		case 4:
		{
			/* Sand */
			type = LAKE_SAND;
			break;
		}
	}

	while (!done)
	{
		/* testing values for these parameters: feel free to adjust */
		grd = 1 << (randint0(4));

		/* want average of about 16 */
		roug = randint1(8) * randint1(4);

		/* Make up size of various componants */
		/* Floor */
		c3 = xsize;

		/* Deep water/lava */
		c1 = randint0(c3 / 2);

		/* Shallow boundary */
		c2 = (c1 + c3) / 2;

		/* make it */
		generate_hmap(y0, x0, xsize, ysize, grd, roug, c3);

		/* Convert to normal format + clean up */
		done = generate_lake(y0, x0, xsize, ysize, c1, c2, c3, type);
	}
		 
	/* Make inner passage */
	generate_draw(y0 - yhsize + 1, x0 - xhsize + 1,
		 y0 - yhsize + ysize - 2, x0 - xhsize + xsize - 2, FEAT_FLOOR);
}


/*
 * Type 14 -- Large room with inner walls
 *
 * Possible sub-types:
 *	1 - Many horizontal walls
 *	2 - S-shaped passage
 *	3 - Single horizontal wall
 *	4 - Inner Plus
 *	5 - Anti-Plus breaking the room into 4 segments
 */
static void build_type14(int by0, int bx0)
{
	int         y, x, y1, x1;
	int         y2, x2, yval, xval;
	bool        light;

	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, by0, bx0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	/* Generate new room */
	generate_room(y1 - 1, x1 - 1, y2 + 1, x2 + 1, light);

	/* Generate outer walls */
	generate_draw(y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

	/* Add some walls */
	
	/* Select what type of room */
	switch(randint0(5))
	{
		case 0:
		{
			/* Horizontal walls */
			for (x = x1 + 1; x < x2; x++)
			{
				cave[yval - 3][x].feat = FEAT_WALL_INNER;
				cave[yval - 1][x].feat = FEAT_WALL_INNER;
				cave[yval + 1][x].feat = FEAT_WALL_INNER;
				cave[yval + 3][x].feat = FEAT_WALL_INNER;
			}
			break;
		}
		
		case 1:
		{
			/* S shape */
			
			/* Select handedness */
			int sgn = (randint0(2) * 2 - 1) * 3;
			
			/* Top wall */
			for (y = y1; y <= yval; y++)
			{
				cave[y][xval + sgn].feat = FEAT_WALL_INNER;
			} 
			
			/* Bottom Wall */
			for (y = y2; y >= yval; y--)
			{
				cave[y][xval - sgn].feat = FEAT_WALL_INNER;
			}

			break;
		}
		
		case 2:
		{
			/* Horizontal internal wall */
			for (x = xval - 4; x <= xval + 4; x++)
			{
				cave[yval][x].feat = FEAT_WALL_INNER;
			}
			
			break;
		}
		
		case 3:
		{
			/* Plus */
			generate_plus(y1 + 2, x1 + 2, y2 - 2, x2 - 2, FEAT_WALL_INNER);
			
			break;
		}
		
		case 4:
		{
			/* Anti-plus */
			generate_plus(y1, x1, y2, x2, FEAT_WALL_INNER);
			generate_plus(y1 + 3, x1 + 5, y2 - 3, x2 - 5, FEAT_FLOOR);
			
			break;
		}
	}	
}


/*
 * Type 15 -- Parallelagram Shaped Rooms
 */
static void build_type15(int by0, int bx0)
{
	u16b		h, w;
	int         y, x, y1, x1, yval, xval;
	bool        light, type;
	
	/* Get size + shape */
	h = (u16b) rand_range(6, 15);
	w = (u16b) rand_range(11, 22);
	
	
	/* Try to allocate space for room. */
	if (!room_alloc(w + h, h, FALSE, by0, bx0, &xval, &yval)) return;
	
	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Get top left corner */
	x1 = xval - (w + h) / 2;
	y1 = yval - h / 2;
	
	/* Get handedness */
	type = (one_in_(2) ? TRUE : FALSE);
	
	/* Fill in floor */
	for (y = 1; y < h; y++)
	{
		for (x = x1; x < x1 + w; x++)
		{
			if (type)
			{
				/* Sloping down and right */
				cave[y + y1][x + y].feat = FEAT_FLOOR;
			}
			else
			{
				/* Sloping up and right */
				cave[y + y1][x + h - y].feat = FEAT_FLOOR;
			}
		}
	}

	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(xval, yval, light, x1, y1, x1 + h + w, y1 + h);
}


/*
 * Attempt to build a room of the given type at the given block
 *
 * Note that we restrict the number of "crowded" rooms to reduce
 * the chance of overflowing the monster list during level creation.
 */
bool room_build(int by0, int bx0, int typ)
{
	/* Restrict level */
	if ((p_ptr->depth < roomdep[typ]) && !ironman_rooms) return (FALSE);

	/* Restrict "crowded" rooms */
	if ((dun->crowded >= 2) && ((typ == 5) || (typ == 6))) return (FALSE);

	/* Build a room */
	switch (typ)
	{ 
		/* Build an appropriate room */
		case 15: build_type15(by0, bx0); break;
		case 14: build_type14(by0, bx0); break;
		case 13: build_type13(by0, bx0); break;
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

