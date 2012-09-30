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
 * This funtion makes a very small room centred at (x0, y0)
 * This is used in crypts, and random elemental vaults.
 *
 * Note - this should be used only on allocated regions
 * within another room.
 */
static void build_small_room(int x0, int y0)
{
	/* Generate outer walls */
	generate_draw(x0 - 1, y0 - 1, x0 + 1, y0 + 1, FEAT_WALL_INNER);

	/* Place a secret door on one side */
	generate_door(x0 - 1, y0 - 1, x0 + 1, y0 + 1, TRUE);

	/* Add inner open space */
	set_feat_bold(x0, y0, dun->feat_floor);
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
static bool room_alloc(int x, int y, bool crowded, int bx0, int by0, int *xx,
                       int *yy)
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
 */


/*
 * Type 1 -- normal rectangular rooms
 */
static void build_type1(int bx0, int by0)
{
	int y, x, y2, x2, yval, xval;
	int y1, x1, xsize, ysize;

	bool light;

	/* Pick a room size */
	y1 = randint1(4);
	x1 = randint1(11);
	y2 = randint1(3);
	x2 = randint1(11);

	xsize = x1 + x2 + 1;
	ysize = y1 + y2 + 1;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 2, ysize + 2, FALSE, bx0, by0, &xval, &yval))
		return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));


	/* Get corner values */
	y1 = yval - ysize / 2;
	x1 = xval - xsize / 2;
	y2 = yval + (ysize - 1) / 2;
	x2 = xval + (xsize - 1) / 2;


	/* Generate new room */
	generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);

	/* Generate outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);

	/* Hack -- Occasional pillar room */
	if (one_in_(20))
	{
		for (y = y1; y <= y2; y += 2)
		{
			for (x = x1; x <= x2; x += 2)
			{
				set_feat_bold(x, y, FEAT_PILLAR);
			}
		}
	}

	/* Hack -- Occasional room with four pillars */
	else if (one_in_(40))
	{
		if ((y1 + 4 < y2) && (x1 + 4 < x2))
		{
			set_feat_bold(x1 + 1, y1 + 1, FEAT_PILLAR);
			set_feat_bold(x2 - 1, y1 + 1, FEAT_PILLAR);
			set_feat_bold(x1 + 1, y2 - 1, FEAT_PILLAR);
			set_feat_bold(x2 - 1, y2 - 1, FEAT_PILLAR);
		}
	}

	/* Hack -- Occasional room with rounded corners */
	else if (one_in_(40))
	{
		set_feat_bold(x1, y1, FEAT_WALL_INNER);
		set_feat_bold(x2, y1, FEAT_WALL_INNER);
		set_feat_bold(x1, y2, FEAT_WALL_INNER);
		set_feat_bold(x2, y2, FEAT_WALL_INNER);
	}

	/* Hack -- Occasional ragged-edge room */
	else if (one_in_(50))
	{
		for (y = y1 + 2; y <= y2 - 2; y += 2)
		{
			set_feat_bold(x1, y, FEAT_PILLAR);
			set_feat_bold(x2, y, FEAT_PILLAR);
		}
		for (x = x1 + 2; x <= x2 - 2; x += 2)
		{
			set_feat_bold(x, y1, FEAT_PILLAR);
			set_feat_bold(x, y2, FEAT_PILLAR);
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
				set_feat_bold(x, yval, FEAT_WALL_INNER);
			}

			/* Prevent edge of wall from being tunneled */
			set_feat_bold(x1 - 1, yval, FEAT_WALL_SOLID);
			set_feat_bold(x2 + 1, yval, FEAT_WALL_SOLID);
		}
		else
		{
			/* Vertical wall */
			for (y = y1; y <= y2; y++)
			{
				set_feat_bold(xval, y, FEAT_WALL_INNER);
			}

			/* Prevent edge of wall from being tunneled */
			set_feat_bold(xval, y1 - 1, FEAT_WALL_SOLID);
			set_feat_bold(xval, y2 + 1, FEAT_WALL_SOLID);
		}

		place_random_door(xval, yval);
	}
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static void build_type2(int bx0, int by0)
{
	int xval, yval;
	int y1, x1, y2, x2;
	bool light;

	int num, i;

#define MAX_ROOM_OVERLAY 	4

	coord uleft[MAX_ROOM_OVERLAY];
	coord lright[MAX_ROOM_OVERLAY];


	/* Try to allocate space for room. If fails, exit */
	if (!room_alloc(25, 11, FALSE, bx0, by0, &xval, &yval)) return;

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
		generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);

		/* Generate outer walls */
		generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

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
		generate_fill(uleft[i].x, uleft[i].y,
					  lright[i].x, lright[i].y, dun->feat_floor);
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
static void build_type3(int bx0, int by0)
{
	int y, x, dy, dx, wy, wx;
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;
	int yval, xval;
	bool light;

	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, bx0, by0, &xval, &yval)) return;

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
	generate_room(x1a - 1, y1a - 1, x2a + 1, y2a + 1, light);

	/* Generate new room (b) */
	generate_room(x1b - 1, y1b - 1, x2b + 1, y2b + 1, light);

	/* Generate outer walls (a) */
	generate_draw(x1a - 1, y1a - 1, x2a + 1, y2a + 1, FEAT_WALL_OUTER);

	/* Generate outer walls (b) */
	generate_draw(x1b - 1, y1b - 1, x2b + 1, y2b + 1, FEAT_WALL_OUTER);

	/* Generate inner floors (a) */
	generate_fill(x1a, y1a, x2a, y2a, dun->feat_floor);

	/* Generate inner floors (b) */
	generate_fill(x1b, y1b, x2b, y2b, dun->feat_floor);


	/* Special features (3/4) */
	switch (randint1(4))
	{
		case 1:
		{
			/* Nothing */
			break;
		}

		case 2:
		{
			/* Large solid middle pillar */

			/* Generate a small inner solid pillar */
			generate_fill(x1b, y1a, x2b, y2a, FEAT_WALL_INNER);

			break;
		}

		case 3:
		{
			/* Inner treasure vault */

			/* Generate a small inner vault */
			generate_draw(x1b, y1a, x2b, y2a, FEAT_WALL_INNER);

			/* Open the inner vault with a secret door */
			generate_door(x1b, y1a, x2b, y2a, TRUE);

			/* Place a treasure in the vault */
			place_object(xval, yval, FALSE, FALSE, 0);

			/* Let's guard the treasure well */
			vault_monsters(xval, yval, rand_range(3, 4));

			/* Traps naturally */
			vault_traps(xval, yval, 4, 4, rand_range(2, 4));

			break;
		}

		case 4:
		{
			/* Something else */

			/* Occasionally pinch the center shut */
			if (one_in_(3))
			{
				/* Pinch the east/west sides */
				for (y = y1b; y <= y2b; y++)
				{
					if (y == yval) continue;

					set_feat_bold(x1a - 1, y, FEAT_WALL_INNER);
					set_feat_bold(x2a + 1, y, FEAT_WALL_INNER);
				}

				/* Pinch the north/south sides */
				for (x = x1a; x <= x2a; x++)
				{
					if (x == xval) continue;

					set_feat_bold(x, y1b - 1, FEAT_WALL_INNER);
					set_feat_bold(x, y2b + 1, FEAT_WALL_INNER);
				}

				/* Sometimes shut using secret doors */
				if (one_in_(3))
				{
					place_secret_door(x1a - 1, yval);
					place_secret_door(x2a + 1, yval);
					place_secret_door(xval, y1b - 1);
					place_secret_door(xval, y2b + 1);
				}
			}

			/* Occasionally put a "plus" in the center */
			else if (one_in_(3))
			{
				generate_plus(x1b, y1a, x2b, y2a, FEAT_WALL_INNER);
			}

			/* Occasionally put a pillar in the center */
			else if (one_in_(3))
			{
				set_feat_bold(xval, yval, FEAT_PILLAR);
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
static void build_type4(int bx0, int by0)
{
	int y, x, y1, x1;
	int y2, x2, tmp, yval, xval;
	bool light;

	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, bx0, by0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	/* Generate new room */
	generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);

	/* Generate outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);


	/* The inner room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_INNER);


	/* Inner room variations */
	switch (randint1(5))
	{
		case 1:
		{
			/* Just an inner room with a monster */

			/* Open the inner room with a secret door */
			generate_door(x1 - 1, y1 - 1, x2 + 1, y2 + 1, TRUE);

			/* Place a monster in the room */
			vault_monsters(xval, yval, 1);

			break;
		}

		case 2:
		{
			/* Treasure Vault (with a door) */

			/* Open the inner room with a secret door */
			generate_door(x1 - 1, y1 - 1, x2 + 1, y2 + 1, TRUE);

			/* Place another inner room */
			generate_draw(xval - 1, yval - 1, xval + 1, yval + 1,
						  FEAT_WALL_INNER);

			/* Open the inner room with a door */
			generate_door(xval - 1, yval - 1, xval + 1, yval + 1, FALSE);

			/* Monsters to guard the "treasure" */
			vault_monsters(xval, yval, rand_range(2, 4));

			/* Object (80%) */
			if (randint0(100) < 80)
			{
				place_object(xval, yval, FALSE, FALSE, 0);
			}

			/* Stairs (20%) */
			else
			{
				place_random_stairs(xval, yval);
			}

			/* Traps to protect the treasure */
			vault_traps(xval, yval, 10, 4, rand_range(3, 6));

			break;
		}

		case 3:
		{
			/* Inner pillar(s). */

			/* Open the inner room with a secret door */
			generate_door(x1 - 1, y1 - 1, x2 + 1, y2 + 1, TRUE);

			/* Inner pillar */
			generate_fill(xval - 1, yval - 1, xval + 1, yval + 1,
						  FEAT_WALL_INNER);

			/* Occasionally, two more Large Inner Pillars */
			if (one_in_(2))
			{
				tmp = randint1(2);

				/* Inner pillar */
				generate_fill(xval - 6 - tmp, yval - 1,
							  xval - 4 - tmp, yval + 1, FEAT_WALL_INNER);

				/* Inner pillar */
				generate_fill(xval + 4 + tmp, yval - 1,
							  xval + 6 + tmp, yval + 1, FEAT_WALL_INNER);
			}

			/* Occasionally, some Inner rooms */
			if (one_in_(3))
			{
				/* Inner rectangle */
				generate_draw(xval - 5, yval - 1, xval + 5, yval + 1,
							  FEAT_WALL_INNER);

				/* Secret doors (random top/bottom) */
				place_secret_door(xval - 3, yval - 3 + (randint1(2) * 2));
				place_secret_door(xval + 3, yval - 3 + (randint1(2) * 2));

				/* Monsters */
				vault_monsters(xval - 2, yval, randint1(2));
				vault_monsters(xval + 2, yval, randint1(2));

				/* Objects */
				if (one_in_(3)) place_object(xval - 2, yval, FALSE, FALSE, 0);
				if (one_in_(3)) place_object(xval + 2, yval, FALSE, FALSE, 0);
			}

			break;
		}

		case 4:
		{
			/* Maze inside. */

			/* Open the inner room with a secret door */
			generate_door(x1 - 1, y1 - 1, x2 + 1, y2 + 1, TRUE);

			/* Maze (really a checkerboard) */
			for (y = y1; y <= y2; y++)
			{
				for (x = x1; x <= x2; x++)
				{
					if (0x1 & (x + y))
					{
						set_feat_bold(x, y, FEAT_WALL_INNER);
					}
				}
			}

			/* Monsters just love mazes. */
			vault_monsters(xval - 5, yval, randint1(3));
			vault_monsters(xval + 5, yval, randint1(3));

			/* Traps make them entertaining. */
			vault_traps(xval - 3, yval, 8, 2, randint1(3));
			vault_traps(xval + 3, yval, 8, 2, randint1(3));

			/* Mazes should have some treasure too. */
			vault_objects(xval, yval, 3);

			break;
		}

		case 5:
		{
			/* Four small rooms. */

			/* Inner "cross" */
			generate_plus(x1, y1, x2, y2, FEAT_WALL_INNER);

			/* Doors into the rooms */
			if (randint0(100) < 50)
			{
				int i = randint1(10);
				place_secret_door(xval - i, y1 - 1);
				place_secret_door(xval + i, y1 - 1);
				place_secret_door(xval - i, y2 + 1);
				place_secret_door(xval + i, y2 + 1);
			}
			else
			{
				int i = randint1(3);
				place_secret_door(x1 - 1, yval + i);
				place_secret_door(x1 - 1, yval - i);
				place_secret_door(x2 + 1, yval + i);
				place_secret_door(x2 + 1, yval - i);
			}

			/* Treasure, centered at the center of the cross */
			vault_objects(xval, yval, rand_range(3, 4));

			/* Gotta have some monsters. */
			vault_monsters(xval - 4, yval + 1, randint1(4));
			vault_monsters(xval + 4, yval + 1, randint1(4));
			vault_monsters(xval - 4, yval - 1, randint1(4));
			vault_monsters(xval + 4, yval - 1, randint1(4));

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
	(!FLAG(&r_info[I], RF_WILD_TOWN) && \
	 !FLAG(&r_info[I], RF_UNIQUE) && \
	 !FLAG(&r_info[I], RF_AQUATIC))


/* Race index for "monster pit (clone)" */
static int vault_aux_race;

/* Race index for "monster pit (symbol clone)" */
static char vault_aux_char;

/* Breath mask for "monster pit (dragon)" */
static u32b vault_aux_dragon_mask4;

/* Breath mask for "monster pit (elemental)" */
static u32b vault_aux_elemental_mask4;

/* Attack type for "monster pit (elemental)" */
static int vault_aux_elemental_attack;


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
	if (FLAG(r_ptr, RF_EVIL)) return (FALSE);

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
	if (FLAG(r_ptr, RF_UNIQUE)) return (FALSE);

	/* Require "animal" flag */
	if (!FLAG(r_ptr, RF_ANIMAL)) return (FALSE);

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
	if (!FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

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
	if ((r_ptr->d_char != 'A') && !mon_name_cont(r_ptr, "riest"))
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
	if (!FLAG(r_ptr, RF_ORC)) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

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
	if (!FLAG(r_ptr, RF_TROLL)) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

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
	if (!FLAG(r_ptr, RF_GIANT)) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

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
	if (!FLAG(r_ptr, RF_DRAGON)) return (FALSE);

	/* Hack -- Require correct "breath attack" */
	if (r_ptr->flags[3] != vault_aux_dragon_mask4) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

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
	if (!FLAG(r_ptr, RF_DEMON)) return (FALSE);

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
	if (!FLAG(r_ptr, RF_ELDRITCH_HORROR)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "monster pit (elemental)"
 */
static bool vault_aux_elemental(int r_idx)
{
    int i;

	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!vault_monster_okay(r_idx)) return (FALSE);

	/* Hack -- Accept correct "breath attack" */
    if (r_ptr->flags[3] == vault_aux_elemental_mask4) return (TRUE);

    /* Accept correct "melee attack" */
    for (i = 0; i < 4; i++)
    {
        if (!r_ptr->blow[i].method) break;
        if (r_ptr->blow[i].effect == vault_aux_elemental_attack) return (TRUE);
    }

    /* Reject */
    return (FALSE);
}


/*
 * Helper function for "monster pit (clone)"
 */
static void vault_prep_clone(void)
{
	/* Pick a race to clone */
	get_filter_mon_num(p_ptr->depth + 10, vault_aux_simple);
}


/*
 * Helper function for "monster pit (symbol clone)"
 */
static void vault_prep_symbol(void)
{
	int r_idx;
	
	/* Pick a race to clone */
	r_idx = get_filter_mon_num(p_ptr->depth + 10, vault_aux_simple);

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
		case 0:
		{
			/* Black */

			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF3_BR_ACID;

			/* Done */
			break;
		}

		case 1:
		{
			/* Blue */

			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF3_BR_ELEC;

			/* Done */
			break;
		}

		case 2:
		{
			/* Red */

			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF3_BR_FIRE;

			/* Done */
			break;
		}

		case 3:
		{
			/* White */

			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF3_BR_COLD;

			/* Done */
			break;
		}

		case 4:
		{
			/* Green */

			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = RF3_BR_POIS;

			/* Done */
			break;
		}

		default:
		{
			/* Multi-hued */

			/* Restrict dragon breath type */
			vault_aux_dragon_mask4 = (RF3_BR_ACID | RF3_BR_ELEC |
									  RF3_BR_FIRE | RF3_BR_COLD | RF3_BR_POIS);

			/* Done */
			break;
		}
	}
}

/*
 * Helper function for "monster pit (elemental)"
 */
static void vault_prep_elemental(void)
{
	/* Pick dragon type */
	switch (randint0(3))
	{
		case 0:
		{
			/* Fire */

			/* Restrict elemental breath type */
            vault_aux_elemental_mask4 = RF3_BR_FIRE;

            /* Restrict melee attack type */
            vault_aux_elemental_attack = RBE_FIRE;

			/* Done */
			break;
		}

		case 1:
		{
			/* Cold */

			/* Restrict elemental breath type */
            vault_aux_elemental_mask4 = RF3_BR_COLD;

            /* Restrict melee attack type */
            vault_aux_elemental_attack = RBE_COLD;

			/* Done */
			break;
		}

        default:
		{
			/* Elec */

			/* Restrict elemental breath type */
            vault_aux_elemental_mask4 = RF3_BR_ELEC;

            /* Restrict melee attack type */
            vault_aux_elemental_attack = RBE_ELEC;

			/* Done */
			break;
        }
    }
}

typedef struct vault_aux_type vault_aux_type;


struct vault_aux_type
{
	cptr name;
	bool (*hook_func) (int r_idx);
	void (*prep_func) (void);
	int level;
	int chance;
};


/*
 * Pick a type of pit or nest out of a list at random.
 * Types with higher numbers of chances are more likely to be picked.
 *
 * This function tries to pick an out-of-depth type occasionally.
 */
static const vault_aux_type *pick_vault_type(const vault_aux_type *l_ptr)
{
	int tmp, total;

	const vault_aux_type *n_ptr;

	/* Calculate the total possibilities */
	for (n_ptr = l_ptr, total = 0; n_ptr->name; n_ptr++)
	{
		/* Count this possibility */
		if (n_ptr->level > p_ptr->depth)
		{
			/* Out of depth - decreased chances */
			total +=
				n_ptr->chance * MAX_DEPTH * 2 / (n_ptr->level - p_ptr->depth +
												 1);
		}
		else
		{
			/* Normal selection */
			total +=
				n_ptr->chance * MAX_DEPTH * 10 / (p_ptr->depth - n_ptr->level +
												  5);
		}
	}

	/* Pick a random type */
	tmp = randint0(total);

	/* Find this type */
	for (n_ptr = l_ptr, total = 0; n_ptr->name; n_ptr++)
	{
		/* Count this possibility */
		if (n_ptr->level > p_ptr->depth)
		{
			total +=
				n_ptr->chance * MAX_DEPTH * 2 / (n_ptr->level - p_ptr->depth +
												 1);
		}
		else
		{
			total +=
				n_ptr->chance * MAX_DEPTH * 10 / (p_ptr->depth - n_ptr->level +
												  5);
		}

		/* Found the type */
		if (tmp < total) break;
	}

	return (n_ptr->name ? n_ptr : NULL);
}


static const vault_aux_type nest_types[] =
{
	{"clone", vault_aux_clone, vault_prep_clone, 7, 6},
    {"jelly", vault_aux_jelly, NULL, 7, 2},
    {"elemental", vault_aux_elemental, vault_prep_elemental, 15, 3},
	{"symbol clone", vault_aux_symbol, vault_prep_symbol, 40, 6},
	{"mimic", vault_aux_mimic, NULL, 45, 2},
	{"lovecraftian", vault_aux_cthulhu, NULL, 80, 2},
	{"kennel", vault_aux_kennel, NULL, 50, 2},
	{"animal", vault_aux_animal, NULL, 50, 8},
	{"chapel", vault_aux_chapel, NULL, 90, 4},
	{"undead", vault_aux_undead, NULL, 90, 4},
	{NULL, NULL, NULL, 0, 0},
};

/*
 * Draw the outside of a pit / nest
 *
 * Add pillars on inner or outer wall - idea from Deric Taylor
 */
static void draw_pit(int x1, int y1, int x2, int y2, int xh, int yh)
{
	int x, y;

	/* Decide what type of hall pattern to use */
	int wid = (xh > 1) ? randint0(xh + 1) : 0;
	int hgt = (yh > 1) ? randint0(yh + 1) : 0;
	int outer = randint0(2);

	/* Generate new room */
	generate_room(x1, y1, x2, y2, FALSE);

	/* Generate outer walls */
	generate_draw(x1, y1, x2, y2, FEAT_WALL_OUTER);

	/* Shrink to inner floor */
	x1++;
	x2--;
	y1++;
	y2--;

	/* Generate inner floor */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);

	/* No pillars? */
	if (!wid && !hgt) return;

	/* Draw pillars */
	for (x = x1; x <= x2; x++)
	{
		for (y = y1; y <= y2; y++)
		{
			/* Only on checkerboard squares */
			if ((x + y) & 1) continue;

			/* Not in middle room area */
			if ((x >= x1 + xh) && (x <= x2 - xh) &&
				(y >= y1 + yh) && (y <= y2 - yh))
			{
				continue;
			}

			if (outer)
			{
				/* Outer wall */
				if ((x < x1 + wid) || (x > x2 - wid) ||
					(y < y1 + hgt) || (y > y2 - hgt))
				{
					set_feat_bold(x, y, FEAT_PILLAR);
				}
			}
			else
			{
				/* Inner wall */
				if ((x > x1 + xh - 1 - wid) && (x < x2 - xh + 1 + wid) &&
					(y > y1 + yh - 1 - hgt) && (y < y2 - yh + 1 + hgt))
				{
					set_feat_bold(x, y, FEAT_PILLAR);
				}
			}
		}
	}
}


/* Maximum pit sizes */
#define PIT_SIZE     3
#define PIT_HGT      ((PIT_SIZE) * 2 + 1)
#define PIT_WID      ((PIT_SIZE) * 8 + 3)


/*
 * Type 5 -- Monster nests
 *
 * A monster nest is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type strewn about the room.
 *
 * The hallway may vary in width, and may contain pillars.
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
static void build_type5(int bx0, int by0)
{
	int x, y;
	int wid, hgt;

	int dx, dy;

	int x1, y1;
	int x2, y2;

	int in_x1, in_y1;
	int in_x2, in_y2;

	int cx, cy;

	int x_cent, y_cent;
	int x_hall, y_hall;

	int i;

	int align = 0;

	int what[64];

	const vault_aux_type *n_ptr = pick_vault_type(nest_types);

	/* No type available */
	if (!n_ptr) return;

	/* Determine size of room - up to 7x27 now! */
	y_cent = rand_range(1, PIT_SIZE);

	/* Nests may have an aspect ratio up to 4:1 */
	x_cent = rand_range(3, 4 * y_cent + 1);

	/* Determine width of hall */
	x_hall = rand_range(1, x_cent / 3 + 1);
	y_hall = rand_range(1, y_cent + 1);

	/* Distances to outer wall */
	dx = x_cent + 1 + x_hall + 1;
	dy = y_cent + 1 + y_hall + 1;

	/* Size of room */
	wid = 2 * dx + 1;
	hgt = 2 * dy + 1;

	/* Try to allocate space for room. */
	if (!room_alloc(wid, hgt, TRUE, bx0, by0, &cx, &cy)) return;

	/* Process a preparation function if necessary */
	if (n_ptr->prep_func) (*(n_ptr->prep_func)) ();

	/* Outer room walls */
	x1 = cx - dx;
	x2 = cx + dx;
	y1 = cy - dy;
	y2 = cy + dy;

	/* Inner room walls */
	in_x1 = cx - x_cent - 1;
	in_x2 = cx + x_cent + 1;
	in_y1 = cy - y_cent - 1;
	in_y2 = cy + y_cent + 1;

	/* Draw the outer room */
	draw_pit(x1, y1, x2, y2, x_hall, y_hall);

	/* Generate inner walls */
	generate_draw(in_x1, in_y1, in_x2, in_y2, FEAT_WALL_INNER);

	/* Open the inner room with a secret door */
	generate_door(in_x1, in_y1, in_x2, in_y2, TRUE);

	/* Prepare allocation table */
	get_mon_num_prep(n_ptr->hook_func);

	/* Pick some monster types */
	for (i = 0; i < 64; i++)
	{
		int r_idx = 0, attempts = 100;

		while (attempts--)
		{
			/* Get a (hard) monster type */
			r_idx = get_mon_num(p_ptr->depth + 10);

			/* Decline incorrect alignment */
			if (((align < 0) && FLAG(&r_info[r_idx], RF_GOOD)) ||
				((align > 0) && FLAG(&r_info[r_idx], RF_EVIL)))
			{
				continue;
			}

			/* Accept this monster */
			break;
		}

		/* Notice failure */
		if (!r_idx || !attempts) return;

		/* Note the alignment */
		if (FLAG(&r_info[r_idx], RF_GOOD)) align++;
		else if (FLAG(&r_info[r_idx], RF_EVIL)) align--;

		what[i] = r_idx;
	}

	/* Reset allocation table */
	get_mon_num_prep(NULL);

	/* Describe */
	if (cheat_room)
	{
		/* Room type */
		msgf("Monster nest (%s)", n_ptr->name);
	}

	/* Shrink to contents of nest */
	in_x1++;
	in_x2--;
	in_y1++;
	in_y2--;

	/* Place some monsters */
	for (y = in_y1; y <= in_y2; y++)
	{
		for (x = in_x1; x <= in_x2; x++)
		{
			int r_idx = what[randint0(64)];

			/* Place that "random" monster (no groups) */
			(void)place_monster_aux(x, y, r_idx, FALSE, FALSE, FALSE, FALSE, TRUE);
		}
	}

	/* Increase the level rating */
	inc_rating(10);

	/* Sometimes nests cause a special feeling */
	if ((p_ptr->depth <= 40) &&
		(randint1(p_ptr->depth * p_ptr->depth + 50) < 300))
	{
		set_special();
	}
}


static const vault_aux_type pit_types[] =
{
	{"orc", vault_aux_orc, NULL, 7, 4},
	{"troll", vault_aux_troll, NULL, 35, 4},
	{"giant", vault_aux_giant, NULL, 70, 2},
	{"lovecraftian", vault_aux_cthulhu, NULL, 90, 1},
	{"clone", vault_aux_symbol, vault_prep_symbol, 85, 2},
	{"chapel", vault_aux_chapel, NULL, 85, 2},
	{"dragon", vault_aux_dragon, vault_prep_dragon, 80, 4},
	{"demon", vault_aux_demon, NULL, 90, 4},
	{NULL, NULL, NULL, 0, 0},
};


/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type organized in the room.
 *
 * The hallway may vary in width, and may contain pillars.
 * The inside room in a monster pit has a randomized, but ordered, layout.
 *
 * Note that the monsters in the pit are now chosen by using "get_mon_num()"
 * to request 16 "appropriate" monsters, sorting them by level.
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
static void build_type6(int bx0, int by0)
{
	int x, y;
	int wid, hgt;
	int in_wid, in_hgt;

	int dx, dy;

	int x1, y1;
	int x2, y2;

	int in_x1, in_y1;
	int in_x2, in_y2;

	int cx, cy;

	int x_cent, y_cent;
	int x_hall, y_hall;

	int i, j;

	int align = 0;

	int what[16];

	byte power[PIT_HGT][PIT_WID];

	int min = 256;
	int max = 0;

	bool copy_v = (randint0(2)) ? TRUE : FALSE;
	bool copy_h = (randint0(2)) ? TRUE : FALSE;

	const vault_aux_type *n_ptr = pick_vault_type(pit_types);

	/* No type available */
	if (!n_ptr) return;

	/* Determine size of room - up to 7x27 now! */
	y_cent = rand_range(1, PIT_SIZE);

	/* Pits may have an aspect ratio up to 4:1 */
	x_cent = rand_range(3, 4 * y_cent + 1);

	/* Determine width of hall */
	x_hall = rand_range(1, x_cent / 3 + 1);
	y_hall = rand_range(1, y_cent + 1);

	/* Distances to outer wall */
	dx = x_cent + 1 + x_hall + 1;
	dy = y_cent + 1 + y_hall + 1;

	/* Size of outer room */
	wid = 2 * dx + 1;
	hgt = 2 * dy + 1;

	/* Try to allocate space for room. */
	if (!room_alloc(wid, hgt, TRUE, bx0, by0, &cx, &cy)) return;

	/* Prepare the monster distribution array */
	for (i = 0; i < PIT_HGT; i++)
	{
		(void)C_WIPE(power[i], PIT_WID, byte);
	}

	/* Process a preparation function if necessary */
	if (n_ptr->prep_func) (*(n_ptr->prep_func)) ();

	/* Outer room walls */
	x1 = cx - dx;
	x2 = cx + dx;
	y1 = cy - dy;
	y2 = cy + dy;

	/* Inner room walls */
	in_x1 = cx - x_cent - 1;
	in_x2 = cx + x_cent + 1;
	in_y1 = cy - y_cent - 1;
	in_y2 = cy + y_cent + 1;

	/* Size of inner room */
	in_wid = 2 * x_cent + 1;
	in_hgt = 2 * y_cent + 1;

	/* Draw the outer room */
	draw_pit(x1, y1, x2, y2, x_hall, y_hall);

	/* Generate inner walls */
	generate_draw(in_x1, in_y1, in_x2, in_y2, FEAT_WALL_INNER);

	/* Open the inner room with a secret door */
	generate_door(in_x1, in_y1, in_x2, in_y2, TRUE);

	/* Prepare allocation table */
	get_mon_num_prep(n_ptr->hook_func);

	/* Pick some monster types */
	for (i = 0; i < 16; i++)
	{
		int r_idx = 0, attempts = 100;

		while (attempts--)
		{
			/* Get a (hard) monster type */
			r_idx = get_mon_num(p_ptr->depth + 10);

			/* Decline incorrect alignment */
			if (((align < 0) && FLAG(&r_info[r_idx], RF_GOOD)) ||
				((align > 0) && FLAG(&r_info[r_idx], RF_EVIL)))
			{
				continue;
			}

			/* Accept this monster */
			break;
		}

		/* Notice failure */
		if (!r_idx || !attempts) return;

		/* Note the alignment */
		if (FLAG(&r_info[r_idx], RF_GOOD)) align++;
		else if (FLAG(&r_info[r_idx], RF_EVIL)) align--;

		what[i] = r_idx;
	}

	/* Reset allocation table */
	get_mon_num_prep(NULL);

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
		msgf("Monster pit (%s)", n_ptr->name);
	}

	/* Create a random pit layout - EB */
	for (i = 0; i < 16; i++)
	{
		/* Pick a block inside the pit */
		int w = randint0(in_wid);
		int n = randint0(in_hgt);
		int e = randint0(in_wid);
		int s = randint0(in_hgt);

		/* Fix order */
		if (w > e)
		{
			x = w;
			w = e;
			e = x;
		}

		/* Fix order */
		if (n > s)
		{
			y = n;
			n = s;
			s = y;
		}

		/* Increase the power of this block */
		for (y = n; y <= s; y++)
		{
			for (x = w; x <= e; x++)
			{
				power[y][x]++;

				if (copy_v) power[in_hgt - 1 - y][x]++;
				if (copy_h) power[y][in_wid - 1 - x]++;

				if (copy_v && copy_h) power[in_hgt - 1 - y][in_wid - 1 - x]++;
			}
		}
	}

	/* Normalize */
	for (y = 0; y < in_hgt; y++)
	{
		for (x = 0; x < in_wid; x++)
		{
			/* Find least powerful monster */
			if (power[y][x] < min) min = power[y][x];

			/* Find most powerful monster */
			if (power[y][x] > max) max = power[y][x];
		}
	}

	/* Shrink to contents of pit */
	in_x1++;
	in_x2--;
	in_y1++;
	in_y2--;

	/* Place the monsters */
	for (y = 0; y < in_hgt; y++)
	{
		for (x = 0; x < in_wid; x++)
		{
			/* Normalize */
			power[y][x] = (power[y][x] - min) * 16 / (max - min + 1);

			/* And place the monster */
			place_monster_aux(x + in_x1, y + in_y1, what[power[y][x]], FALSE,
							  FALSE, FALSE, FALSE, TRUE);
		}
	}

	/* Increase the level rating */
	inc_rating(10);

	/* Sometimes pits cause a special feeling */
	if ((p_ptr->depth <= 40) &&
		(randint1(p_ptr->depth * p_ptr->depth + 50) < 300))
	{
		set_special();
	}
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
static void build_vault(int xval, int yval, int xmax, int ymax, cptr data,
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
			c_ptr = cave_p(x, y);

			/* Lay down a floor */
			set_feat_grid(c_ptr, dun->feat_floor);

			/* Part of a vault */
			c_ptr->info |= (CAVE_ROOM | CAVE_ICKY);

			/* Analyze the grid */
			switch (*t)
			{
				case '%':
				{
					/* Granite wall (outer) */
					set_feat_grid(c_ptr, FEAT_WALL_OUTER);
					break;
				}

				case '#':
				{
					/* Granite wall (inner) */
					set_feat_grid(c_ptr, FEAT_WALL_INNER);
					break;
				}


				case 'X':
				{
					/* Permanent wall (inner) */
					set_feat_grid(c_ptr, FEAT_PERM_INNER);
					break;
				}

				case '*':
				{
					/* Treasure/trap */
					if (randint0(100) < 75)
					{
						place_object(x, y, FALSE, FALSE, 0);
					}
					else
					{
						place_trap(x, y);
					}
					break;
				}

				case '+':
				{
					/* Secret doors */
					place_secret_door(x, y);
					break;
				}

				case '^':
				{
					/* Trap */
					place_trap(x, y);
					break;
				}

				case 'p':
				{
					set_feat_grid(c_ptr, FEAT_PATTERN_START);
					break;
				}

				case 'a':
				{
					set_feat_grid(c_ptr, FEAT_PATTERN_1);
					break;
				}

				case 'b':
				{
					set_feat_grid(c_ptr, FEAT_PATTERN_2);
					break;
				}

				case 'c':
				{
					set_feat_grid(c_ptr, FEAT_PATTERN_3);
					break;
				}

				case 'd':
				{
					set_feat_grid(c_ptr, FEAT_PATTERN_4);
					break;
				}

				case 'P':
				{
					set_feat_grid(c_ptr, FEAT_PATTERN_END);
					break;
				}

				case 'B':
				{
					set_feat_grid(c_ptr, FEAT_PATTERN_XTRA1);
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
				case '&':
				{
					/* Monster */
					(void)place_monster(x, y, TRUE, TRUE, 4);
					break;
				}

				case '@':
				{
					/* Meaner monster */
					(void)place_monster(x, y, TRUE, TRUE, 8);
					break;
				}

				case '9':
				{
					/* Meaner monster, plus treasure */
					(void)place_monster(x, y, TRUE, TRUE, 6);
					place_object(x, y, TRUE, FALSE, 6);
					break;
				}

				case '8':
				{
					/* Nasty monster and treasure */
					(void)place_monster(x, y, TRUE, TRUE, 25);
					place_object(x, y, TRUE, TRUE, 20);
					break;
				}

				case ',':
				{
					/* Monster and/or object */
					if (randint0(100) < 50)
					{
						(void)place_monster(x, y, TRUE, TRUE, 3);
					}
					if (randint0(100) < 50)
					{
						place_object(x, y, FALSE, FALSE, 5);
					}
					break;
				}

				case 'A':
				{
					/* Object */
					place_object(x, y, TRUE, FALSE, 10);
				}
					break;
			}
		}
	}
}


/*
 * Type 7 -- simple vaults (see "v_info.txt")
 */
static void build_type7(int bx0, int by0)
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
		v_ptr = &v_info[randint0(z_info->v_max)];

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
	if (!room_alloc(ABS(x), ABS(y), FALSE, bx0, by0, &xval, &yval)) return;

	if (dummy >= SAFE_MAX_ATTEMPTS)
	{
		if (cheat_room)
			msgf("Warning! Could not place lesser vault!");

		return;
	}

	/* Message */
	if (cheat_room) msgf("%s", v_name + v_ptr->name);

	/* Boost the rating */
	inc_rating(v_ptr->rat);

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
		(randint1((p_ptr->depth - 40) * (p_ptr->depth - 40) + 50) < 400))
	{
		set_special();
	}

	/* Hack -- Build the vault */
	build_vault(xval, yval, v_ptr->wid, v_ptr->hgt,
				v_text + v_ptr->text, xoffset, yoffset, transno);
}


/*
 * Type 8 -- greater vaults (see "v_info.txt")
 */
static void build_type8(int bx0, int by0)
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
		v_ptr = &v_info[randint0(z_info->v_max)];

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

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(ABS(x), ABS(y), FALSE, bx0, by0, &xval, &yval)) return;

	if (dummy >= SAFE_MAX_ATTEMPTS)
	{
		if (cheat_room)
			msgf("Warning! Could not place greater vault!");

		return;
	}

	/* Message */
	if (cheat_room) msgf("%s", v_name + v_ptr->name);

	/* Boost the rating */
	inc_rating(v_ptr->rat);

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
		(randint1((p_ptr->depth - 40) * (p_ptr->depth - 40) + 50) < 400))
	{
		set_special();
	}

	/* Hack -- Build the vault */
	build_vault(xval, yval, v_ptr->wid, v_ptr->hgt,
				v_text + v_ptr->text, xoffset, yoffset, transno);
}



/*
 * Type 9 -- Fractal cave system
 */
static void build_type9(int bx0, int by0)
{
	int grd, roug, cutoff, xsize, ysize, xhsize, yhsize, y0, x0;

	bool done, light;

	/* get size: note 'Evenness' */
	xsize = randint1(22) * 2 + 6;
	ysize = randint1(10) * 2 + 6;

	/* round to make sizes even */
	xhsize = (xsize - 1) / 2;
	yhsize = (ysize - 1) / 2;
	xsize = xhsize * 2;
	ysize = yhsize * 2;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 1, ysize + 1, FALSE, bx0, by0, &x0, &y0)) return;

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
		generate_hmap(x0, y0, xsize, ysize, grd, roug, cutoff);

		/* Convert to normal format + clean up */
		done = generate_fracave(x0, y0, xsize, ysize, cutoff, light);
	}
}



/*
 * Routine used by the random vault creators to add a door to a location
 * Note that range checking has to be done in the calling routine.
 *
 * The doors must be INSIDE the allocated region.
 */
static bool add_door(int x, int y)
{
	/* Need to have a wall in the center square */
	if (cave_p(x, y)->feat != FEAT_WALL_OUTER) return (FALSE);

	/* look at:
	 *  x#x
	 *  .#.
	 *  x#x
	 *
	 *  where x=don't care
	 *  .=floor, #=wall
	 */

	if ((cave_p(x, y - 1)->feat == dun->feat_floor) &&
		(cave_p(x, y + 1)->feat == dun->feat_floor) &&
		(cave_p(x - 1, y)->feat == FEAT_WALL_OUTER) &&
		(cave_p(x + 1, y)->feat == FEAT_WALL_OUTER))
	{
		place_random_door(x, y);

		/* set boundarys so don't get wide doors */
		set_feat_bold(x - 1, y, FEAT_WALL_SOLID);
		set_feat_bold(x + 1, y, FEAT_WALL_SOLID);

		return (TRUE);
	}


	/* look at:
	 *  x#x
	 *  .#.
	 *  x#x
	 *
	 *  where x = don't care
	 *  .=floor, #=wall
	 */
	if ((cave_p(x, y - 1)->feat == FEAT_WALL_OUTER) &&
		(cave_p(x, y + 1)->feat == FEAT_WALL_OUTER) &&
		(cave_p(x - 1, y)->feat == dun->feat_floor) &&
		(cave_p(x + 1, y)->feat == dun->feat_floor))
	{
		place_random_door(x, y);

		/* set boundarys so don't get wide doors */
		set_feat_bold(x, y - 1, FEAT_WALL_SOLID);
		set_feat_bold(x, y + 1, FEAT_WALL_SOLID);

		return (TRUE);
	}
	
	return (FALSE);
}


/*
 * Routine that fills the empty areas of a room with treasure and monsters.
 */
static void fill_treasure(int x1, int y1, int x2, int y2, int difficulty)
{
	int x, y, cx, cy, size;
	s32b value;

	cave_type *c_ptr;

	/* center of room: */
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

			c_ptr = cave_p(x, y);

			/* if floor, shallow water and lava */
			if ((c_ptr->feat == dun->feat_floor) ||
				(c_ptr->feat == FEAT_SHAL_WATER) ||
				(c_ptr->feat == FEAT_SHAL_LAVA))
			{
				/* The smaller 'value' is, the better the stuff */
				if (value < 0)
				{
					/* Meanest monster + treasure */
					(void)place_monster(x, y, TRUE, TRUE, 25);
					place_object(x, y, TRUE, FALSE, 25);
				}
				else if (value < 5)
				{
					/* Mean monster + treasure */
					(void)place_monster(x, y, TRUE, TRUE, 15);
					place_object(x, y, TRUE, FALSE, 10);
				}
				else if (value < 10)
				{
					/* Monster */
					(void)place_monster(x, y, TRUE, TRUE, 6);
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
						place_object(x, y, FALSE, FALSE, 0);
					}
					else
					{
						place_trap(x, y);
					}
				}
				else if (value < 30)
				{
					/* Monster and trap */
					(void)place_monster(x, y, TRUE, TRUE, 5);
					place_trap(x, y);
				}
				else if (value < 40)
				{
					/* Monster or object */
					if (randint0(100) < 50)
					{
						(void)place_monster(x, y, TRUE, TRUE, 3);
					}
					if (randint0(100) < 50)
					{
						place_object(x, y, FALSE, FALSE, 7);
					}
				}
				else if (value < 50)
				{
					/* Trap */
					place_trap(x, y);
				}
				else
				{
					/* Various Stuff */

					/* 20% monster, 40% trap, 20% object, 20% blank space */
					if (randint0(100) < 20)
					{
						(void)place_monster(x, y, TRUE, TRUE, 0);
					}
					else if (randint0(100) < 50)
					{
						place_trap(x, y);
					}
					else if (randint0(100) < 50)
					{
						place_object(x, y, FALSE, FALSE, 0);
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


	if (cheat_room) msgf("Bubble Vault");

	/* Allocate center of bubbles */
	for (i = 0; i < BUBBLENUM; i++)
	{
		/* Uniqueness check removed */

		center[i].x = (u16b)rand_range(1, xsize - 2);
		center[i].y = (u16b)rand_range(1, ysize - 2);
	}

	/* Set vault flags */
	generate_vault(x0 - xhsize, y0 - yhsize,
				   x0 - xhsize + xsize - 1, y0 - yhsize + ysize - 1);

	/* Draw outer walls */
	generate_draw(x0 - xhsize, y0 - yhsize,
				  x0 - xhsize + xsize - 1, y0 - yhsize + ysize - 1,
				  FEAT_WALL_OUTER);

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

			c_ptr = cave_p(x0 - xhsize + x, y0 - yhsize + y);

			if (((min2 - min1) <= 2) && (!(min1 < 3)))
			{
				/* Boundary at midpoint+ not at inner region of bubble */
				set_feat_grid(c_ptr, FEAT_WALL_OUTER);
			}
			else
			{
				/* middle of a bubble */
				set_feat_grid(c_ptr, dun->feat_floor);
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
	fill_treasure(x0 - xhsize + 1, y0 - yhsize + 1,
				  x0 - xhsize + xsize - 2, y0 - yhsize + ysize - 2,
				  randint1(5));
}


/*
 * Overlay a rectangular room given its bounds
 * This routine is used by build_room_vault
 * The area inside the walls is not touched:
 * only granite is removed- normal walls stay
 */
static void build_room(int x1, int y1, int x2, int y2)
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
	generate_vault(x1, y1, x2, y2);

	/* Draw outer wall */
	generate_draw(x1, y1, x2, y2, FEAT_WALL_OUTER);

	/* Middle */
	for (x = 1; x < xsize; x++)
	{
		for (y = 1; y < ysize; y++)
		{
			c_ptr = cave_p(x1 + x, y1 + y);

			if (c_ptr->feat == FEAT_WALL_EXTRA)
			{
				/* Clear the untouched region */
				set_feat_grid(c_ptr, dun->feat_floor);
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

	if (cheat_room) msgf("Room Vault");

	/* fill area so don't get problems with arena levels */
	generate_fill(x0 - xhsize, y0 - yhsize,
				  x0 - xhsize + xsize - 1, y0 - yhsize + ysize - 1,
				  FEAT_WALL_EXTRA);

	/* Clear the CAVE_ICKY flag in the region */
	clear_vault(x0 - xhsize, y0 - yhsize,
				x0 - xhsize + xsize - 1, y0 - yhsize + ysize - 1);

	/* add ten random rooms */
	for (i = 0; i < 10; i++)
	{
		x1 = randint1(xhsize) * 2 + x0 - xhsize;
		x2 = randint1(xhsize) * 2 + x0 - xhsize;
		y1 = randint1(yhsize) * 2 + y0 - yhsize;
		y2 = randint1(yhsize) * 2 + y0 - yhsize;
		build_room(x1, y1, x2, y2);
	}

	/* Add some random doors */
	for (i = 0; i < 500; i++)
	{
		x1 = rand_range(x0 - xhsize + 1, x0 - xhsize + xsize - 2);
		y1 = rand_range(y0 - yhsize + 1, y0 - yhsize + ysize - 2);
		add_door(x1, y1);
	}

	/* Fill with monsters and treasure, high difficulty */
	fill_treasure(x0 - xhsize + 1, y0 - yhsize + 1,
				  x0 - xhsize + xsize - 1, y0 - yhsize + ysize - 1,
				  rand_range(5, 10));
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

	if (cheat_room) msgf("Cave Vault");

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
		generate_hmap(x0, y0, xsize, ysize, grd, roug, cutoff);

		/* Convert to normal format+ clean up */
		done = generate_fracave(x0, y0, xsize, ysize, cutoff, FALSE);
	}

	/* Set vault flags */
	for (y = y0 - yhsize; y < y0 - yhsize + ysize; y++)
	{
		for (x = x0 - xhsize; x < x0 - xhsize + xsize; x++)
		{
			c_ptr = cave_p(x, y);

			/* Is it a floor? */
			if (c_ptr->feat == dun->feat_floor)
			{
				/* Set the icky flag */
				c_ptr->info |= CAVE_ICKY;
			}
		}
	}

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x0 - xhsize + 1, y0 - yhsize + 1,
				  x0 - xhsize + xsize - 1, y0 - yhsize + ysize - 1,
				  randint1(5));
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
static void r_visit(int x1, int y1, int x2, int y2,
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
	set_feat_bold(x, y, dun->feat_floor);

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
					set_feat_bold(x, y + 1, dun->feat_floor);
					r_visit(x1, y1, x2, y2, node + m, dir, visited);
				}
				break;
			}

			case 1:
			{
				/* (0,-) - check for top boundary */
				if ((node / m > 0) && (visited[node - m] == 0))
				{
					set_feat_bold(x, y - 1, dun->feat_floor);
					r_visit(x1, y1, x2, y2, node - m, dir, visited);
				}
				break;
			}

			case 2:
			{
				/* (+,0) - check for right boundary */
				if ((node % m < m - 1) && (visited[node + 1] == 0))
				{
					set_feat_bold(x + 1, y, dun->feat_floor);
					r_visit(x1, y1, x2, y2, node + 1, dir, visited);
				}
				break;
			}

			case 3:
			{
				/* (-,0) - check for left boundary */
				if ((node % m > 0) && (visited[node - 1] == 0))
				{
					set_feat_bold(x - 1, y, dun->feat_floor);
					r_visit(x1, y1, x2, y2, node - 1, dir, visited);
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

	if (cheat_room) msgf("Maze Vault");

	/* Pick a random room size - randomized by calling routine */
	dy = ysize / 2 - 1;
	dx = xsize / 2 - 1;

	y1 = y0 - dy;
	x1 = x0 - dx;
	y2 = y0 + dy;
	x2 = x0 + dx;

	/* generate the room */
	generate_vault(x1 - 1, y1 - 1, x2 + 1, y2 + 1);

	/* Draw outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

	/* Fill room with inner walls */
	generate_fill(x1, y1, x2, y2, FEAT_WALL_INNER);

	/* dimensions of vertex array */
	m = dx + 1;
	n = dy + 1;
	num_vertices = m * n;

	/* initialize array of visited vertices */
	C_MAKE(visited, num_vertices, int);

	/* traverse the graph to create a spaning tree, pick a random root */
	r_visit(x1, y1, x2, y2, randint0(num_vertices), 0, visited);

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x1, y1, x2, y2, randint1(5));

	FREE(visited);
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

	if (cheat_room) msgf("Mini Checker Board Vault");

	/* Pick a random room size */
	dy = ysize / 2 - 1;
	dx = xsize / 2 - 1;

	y1 = y0 - dy;
	x1 = x0 - dx;
	y2 = y0 + dy;
	x2 = x0 + dx;

	/* generate the room */
	generate_vault(x1 - 1, y1 - 1, x2 + 1, y2 + 1);

	/* Fill room with perm walls. */
	generate_fill(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_PERM_INNER);

	/* dimensions of vertex array */
	m = dx + 1;
	n = dy + 1;
	num_vertices = m * n;

	/* initialize array of visited vertices */
	C_MAKE(visited, num_vertices, int);

	/* traverse the graph to create a spannng tree, pick a random root */
	r_visit(x1, y1, x2, y2, randint0(num_vertices), 0, visited);

	/* Make it look like a checker board vault */
	for (x = x1; x <= x2; x++)
	{
		for (y = y1; y <= y2; y++)
		{
			c_ptr = cave_p(x, y);

			total = x - x1 + y - y1;

			/* If total is odd- and is a floor then make a wall */
			if ((total % 2 == 1) && (c_ptr->feat == dun->feat_floor))
			{
				set_feat_grid(c_ptr, FEAT_WALL_INNER);
			}
		}
	}

	/* Make a couple of entrances */
	if (one_in_(2))
	{
		/* left and right */
		y = randint1(dy) + dy / 2;

		set_feat_bold(x1 - 1, y1 + y, FEAT_WALL_OUTER);
		set_feat_bold(x2 + 1, y1 + y, FEAT_WALL_OUTER);
	}
	else
	{
		/* top and bottom */
		x = randint1(dx) + dx / 2;

		set_feat_bold(x1 + x, y1 - 1, FEAT_WALL_OUTER);
		set_feat_bold(x1 + x, y2 + 1, FEAT_WALL_OUTER);
	}

	/* Fill with monsters and treasure, highest difficulty */
	fill_treasure(x1, y1, x2, y2, 10);

	FREE(visited);
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
			generate_draw(x1, y1, x2, y2, FEAT_WALL_OUTER);

			/* Make a couple of entrances */
			if (one_in_(2))
			{
				/* left and right */
				y = randint1(ysize) + y1;

				set_feat_bold(x1, y, dun->feat_floor);
				set_feat_bold(x2, y, dun->feat_floor);
			}
			else
			{
				/* top and bottom */
				x = randint1(xsize) + x1;

				set_feat_bold(x, y1, dun->feat_floor);
				set_feat_bold(x, y2, dun->feat_floor);
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
				generate_fill(x1, y1, x2 - 1, y2 - 1, FEAT_WALL_INNER);

				/* Too small */
				return;
			}

			/* Make outside walls */
			generate_draw(x1 + 1, y1 + 1, x2 - 1, y2 - 1, FEAT_WALL_INNER);

			/* Make a door */
			y = rand_range(y1 + 1, y1 + ysize - 2);

			if (one_in_(2))
			{
				/* left */
				set_feat_bold(x1 + 1, y, dun->feat_floor);
			}
			else
			{
				/* right */
				set_feat_bold(x2 - 1, y, dun->feat_floor);
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
				generate_fill(x1, y1, x2 - 1, y2 - 1, FEAT_WALL_INNER);

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
				generate_fill(x1, y1, x2 - 1, y2 - 1, FEAT_WALL_INNER);

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

	if (cheat_room) msgf("Castle Vault");

	/* generate the room */
	generate_vault(x1 - 1, y1 - 1, x2 + 1, y2 + 1);

	/* Make the whole room floor */
	generate_fill(x1 - 1, y1 - 1, x2 + 1, y2 + 1, dun->feat_floor);

	/* Make the castle */
	build_recursive_room(x1, y1, x2, y2, randint1(5));

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x1, y1, x2, y2, randint1(3));
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

	if (!in_bounds2(x, y)) return;

	c_ptr = cave_p(x, y);

	/*
	 * Hack- check to see if square has been visited before
	 * if so, then exit (use room flag to do this)
	 */
	if (c_ptr->info & CAVE_ROOM) return;

	/* set room flag */
	c_ptr->info |= CAVE_ROOM;

	if (cave_floor_grid(c_ptr))
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
		set_feat_grid(c_ptr, FEAT_WALL_OUTER);
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
static int dist2(int x1, int y1, int x2, int y2, int h1, int h2, int h3, int h4)
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

	cave_type *c_ptr;

	/* Make a random metric */
	int h1, h2, h3, h4;
	h1 = randint1(32) - 16;
	h2 = randint1(16);
	h3 = randint1(32);
	h4 = randint1(32) - 16;

	if (cheat_room) msgf("Target Vault");

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
	generate_draw(x0 - rad, y0 - rad, x0 + rad, y0 + rad, FEAT_WALL_EXTRA);

	/* Make floor */
	for (x = x0 - rad + 1; x <= x0 + rad - 1; x++)
	{
		for (y = y0 - rad + 1; y <= y0 + rad - 1; y++)
		{
			c_ptr = cave_p(x, y);

			/* clear room flag */
			c_ptr->info &= ~(CAVE_ROOM);

			/* Vault - so is "icky" */
			c_ptr->info |= CAVE_ICKY;

			if (dist2(y0, x0, y, x, h1, h2, h3, h4) <= rad - 1)
			{
				/* inside- so is floor */
				set_feat_grid(c_ptr, dun->feat_floor);
			}
			else
			{
				/* make granite outside so arena works */
				set_feat_grid(c_ptr, FEAT_WALL_EXTRA);
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
				set_feat_bold(x, y, FEAT_WALL_INNER);
			}
		}
	}

	/* Make perpendicular walls */
	generate_plus(x0 - rad, y0 - rad, x0 + rad, y0 + rad, FEAT_WALL_INNER);

	/* Make inner vault */
	generate_draw(x0 - 1, y0 - 1, x0 + 1, y0 + 1, FEAT_WALL_INNER);

	/* Make inner room */
	set_feat_bold(x0, y0, dun->feat_floor);


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
	fill_treasure(x0 - rad, y0 - rad, x0 + rad, y0 + rad, rand_range(3, 6));
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

	byte f1, f2, f3;


	if (cheat_room) msgf("Elemental Vault");

	/* round to make sizes even */
	xhsize = xsiz / 2;
	yhsize = ysiz / 2;
	xsize = xhsize * 2;
	ysize = yhsize * 2;

	if (p_ptr->depth < 25)
	{
		/* Earth vault  (Rubble) */
		f1 = FEAT_RUBBLE;
		f2 = dun->feat_floor;
		f3 = FEAT_RUBBLE;
	}
	else if (p_ptr->depth < 50)
	{
		/* Air vault (Trees) */
		f1 = FEAT_GRASS;
		f2 = FEAT_TREES;
		f3 = FEAT_GRASS;
	}
	else if (p_ptr->depth < 75)
	{
		/* Water vault (shallow water) */
		f1 = FEAT_SHAL_WATER;
		f2 = FEAT_DEEP_WATER;
		f3 = FEAT_SHAL_WATER;
	}
	else
	{
		/* Fire vault (shallow lava) */
		f1 = FEAT_SHAL_LAVA;
		f2 = FEAT_DEEP_LAVA;
		f3 = FEAT_SHAL_LAVA;
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
		generate_hmap(x0, y0, xsize, ysize, grd, roug, c3);

		/* Convert to normal format + clean up */
		done = generate_lake(x0, y0, xsize, ysize, c1, c2, c3, f1, f2, f3);
	}

	/* Set icky flag because is a vault */
	generate_vault(x0 - xhsize, y0 - yhsize,
				   x0 - xhsize + xsize, y0 - yhsize + ysize);

	/* make a few rooms in the vault */
	for (i = 1; i <= (xsize * ysize) / 50; i++)
	{
		build_small_room(x0 + randint0(xsize - 4) - xsize / 2 + 2,
						 y0 + randint0(ysize - 4) - ysize / 2 + 2);
	}

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x0 - xhsize + 1, y0 - yhsize + 1,
				  x0 - xhsize + xsize - 1, y0 - yhsize + ysize - 1,
				  randint1(5));
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

	if (cheat_room) msgf("Micro-Room Vault");

	/* generate the room */
	generate_vault(x1 - 1, y1 - 1, x2 + 1, y2 + 1);

	/* Make the whole room floor */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);

	/* Make outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

	/* Make a grid of "features" */
	for (j = y1 + 2; j < y2 - 1; j += 4)
	{
		for (i = x1 + 2; i < x2 - 1; i += 4)
		{
			if (one_in_(2))
			{
				/* A tiny room */
				build_small_room(i, j);
			}
			else if (one_in_(2))
			{
				/* 1/4 chance for a pillar */
				generate_fill(i - 1, j - 1, i + 1, j + 1, FEAT_WALL_INNER);
			}
			else if (one_in_(2))
			{
				/* 1/8 chance for a plus */
				generate_plus(i - 1, j - 1, i + 1, j + 1, FEAT_WALL_INNER);
			}
		}
	}

	/* Make a set of walls to break up the flow */
	for (j = y1; j < y2 - 1; j += 4)
	{
		for (i = x1; i < x2 - 1; i += 4)
		{
			if (one_in_(2))
			{
				set_feat_bold(i, j, FEAT_WALL_INNER);
			}
		}
	}

	/* Fill with monsters and treasure, low difficulty */
	fill_treasure(x1, y1, x2, y2, randint1(5));
}


/*
 * Type 10 -- Random vault
 */
static void build_type10(int bx0, int by0)
{
	int y0, x0, xsize, ysize, vtype;

	/*
	 * Get size
	 * Big enough to look good, small enough to be fairly common.
	 */
	xsize = rand_range(22, 44);
	ysize = rand_range(11, 22);

	/* Allocate in room_map.  If will not fit, exit */
	if (!room_alloc(xsize + 1, ysize + 1, FALSE, bx0, by0, &x0, &y0)) return;

	/*
	 * Boost the rating- higher than lesser vaults
	 * and lower than greater vaults
	 */
	inc_rating(10);

	/* (Sometimes) Cause a special feeling */
	if ((p_ptr->depth <= 50) ||
		(randint1((p_ptr->depth - 40) * (p_ptr->depth - 40) + 1) < 400))
	{
		set_special();
	}

	/* Select type of vault */
	vtype = randint1(9);

	/* Build an appropriate room */
	switch (vtype)
	{
		case 1:
		{
			build_bubble_vault(x0, y0, xsize, ysize);
			break;
		}
		case 2:
		{
			build_room_vault(x0, y0, xsize, ysize);
			break;
		}
		case 3:
		{
			build_cave_vault(x0, y0, xsize, ysize);
			break;
		}
		case 4:
		{
			build_maze_vault(x0, y0, xsize, ysize);
			break;
		}
		case 5:
		{
			build_mini_c_vault(x0, y0, xsize, ysize);
			break;
		}
		case 6:
		{
			build_castle_vault(x0, y0, xsize, ysize);
			break;
		}
		case 7:
		{
			build_target_vault(x0, y0, xsize, ysize);
			break;
		}
		case 8:
		{
			build_elemental_vault(x0, y0, xsize, ysize);
			break;
		}
		case 9:
		{
			build_micro_room_vault(x0, y0, xsize, ysize);
			break;
		}
	}
}


/*
 * Type 11 -- Vertical oval room.
 * For every grid in the possible square, check the distance.
 * If it's less than the radius, make it a room square.
 *
 * When done fill from the inside to find the walls,
 */
static void build_type11(int bx0, int by0)
{
	int rad, x, y, x0, y0;
	int light = FALSE;

	/* Occasional light */
	if (randint1(p_ptr->depth) <= 15) light = TRUE;

	rad = rand_range(2, 9);

	/* Allocate in room_map.  If will not fit, exit */
	if (!room_alloc(rad * 2 + 1, rad * 2 + 1, FALSE, bx0, by0, &x0, &y0))
		return;

	/* Make circular floor */
	for (x = x0 - rad; x <= x0 + rad; x++)
	{
		for (y = y0 - rad; y <= y0 + rad; y++)
		{
			if (distance(x0, y0, x, y) <= rad - 1)
			{
				/* inside- so is floor */
				set_feat_bold(x, y, dun->feat_floor);
			}
			else if (distance(x0, y0, x, y) <= rad + 1)
			{
				/* make granite outside so arena works */
				set_feat_bold(x, y, FEAT_WALL_EXTRA);
			}
		}
	}

	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0, y0, light, x0 - rad, y0 - rad, x0 + rad, y0 + rad);

	if (one_in_(3))
	{
		rad = randint1(rad);
		
		/* Make circular liquid feature */
		for (x = x0 - rad; x <= x0 + rad; x++)
		{
			for (y = y0 - rad; y <= y0 + rad; y++)
			{
				if (distance(x0, y0, x, y) <= rad - 1)
				{
					/* inside- so is floor */
					set_feat_bold(x, y, dun->feat_shal_liquid);
				}
			}
		}
	}
}


/*
 * Type 12 -- Crypt room.
 * For every grid in the possible square, check the (fake) distance.
 * If it's less than the radius, make it a room square.
 *
 * When done fill from the inside to find the walls,
 */
static void build_type12(int bx0, int by0)
{
	int rad, x, y, x0, y0;
	int light = FALSE;
	bool emptyflag = TRUE;

	cave_type *c_ptr;

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
	if (!room_alloc(rad * 2 + 3, rad * 2 + 3, FALSE, bx0, by0, &x0, &y0))
		return;

	/* Add outer wall */
	generate_draw(x0 - rad, y0 - rad, x0 + rad, y0 + rad, FEAT_WALL_EXTRA);

	/* Make floor */
	for (x = x0 - rad + 1; x <= x0 + rad - 1; x++)
	{
		for (y = y0 - rad + 1; y <= y0 + rad - 1; y++)
		{
			c_ptr = cave_p(x, y);

			/* clear room flag */
			c_ptr->info &= ~(CAVE_ROOM);

			if (dist2(y0, x0, y, x, h1, h2, h3, h4) <= rad - 1)
			{
				/* inside - so is floor */
				set_feat_grid(c_ptr, dun->feat_floor);
			}
			else if (distance(x0, y0, x, y) < 3)
			{
				set_feat_grid(c_ptr, dun->feat_floor);
			}
			else
			{
				/* make granite outside so arena works */
				set_feat_grid(c_ptr, FEAT_WALL_EXTRA);
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
			if (cave_p(x, y)->feat != dun->feat_floor)
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
		place_object(x0, y0, FALSE, FALSE, 0);

		/* Let's guard the treasure well */
		vault_monsters(x0, y0, rand_range(2, 4));

		/* Traps naturally */
		vault_traps(x0, y0, 4, 4, rand_range(2, 4));
	}
}


/*
 * Type 13 -- Rectangular room with central fractal feature
 */
static void build_type13(int bx0, int by0)
{
	int grd, roug, xsize, ysize, xhsize, yhsize, y0, x0;

	int c1, c2, c3;
	byte f1 = 0, f2 = 0, f3 = 0;

	bool done;

	/* get size: note 'Evenness' */
	xsize = randint1(22) * 2 + 6;
	ysize = randint1(15) * 2 + 6;

	/* round to make sizes even */
	xhsize = (xsize - 1) / 2;
	yhsize = (ysize - 1) / 2;
	xsize = xhsize * 2;
	ysize = yhsize * 2;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 1, ysize + 1, FALSE, bx0, by0, &x0, &y0)) return;

	done = FALSE;

	/* Pick the type */
	switch (randint0(4))
	{
		case 0:
		{
			/* Water */
			f1 = FEAT_DEEP_WATER;
			f2 = FEAT_SHAL_WATER;
			f3 = dun->feat_floor;
			break;
		}

		case 1:
		{
			/* Lava */
			f1 = FEAT_DEEP_LAVA;
			f2 = FEAT_SHAL_LAVA;
			f3 = dun->feat_floor;
			break;
		}

		case 2:
		{
			/* Rock */
			f1 = FEAT_WALL_INNER;
			f2 = FEAT_WALL_INNER;
			f3 = dun->feat_floor;
			break;
		}

		case 3:
		{
			/* Rubble - oooh treasure */
			f1 = FEAT_RUBBLE;
			f2 = FEAT_RUBBLE;
			f3 = dun->feat_floor;
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
		generate_hmap(x0, y0, xsize, ysize, grd, roug, c3);

		/* Convert to normal format + clean up */
		done = generate_lake(x0, y0, xsize, ysize, c1, c2, c3, f1, f2, f3);
	}

	/* Make inner passage */
	generate_draw(x0 - xhsize + 1, y0 - yhsize + 1,
				  x0 - xhsize + xsize - 2, y0 - yhsize + ysize - 2, dun->feat_floor);
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
static void build_type14(int bx0, int by0)
{
	int y, x, y1, x1;
	int y2, x2, yval, xval;
	bool light;

	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, bx0, by0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	/* Generate new room */
	generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);

	/* Generate outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);

	/* Add some walls */

	/* Select what type of room */
	switch (randint0(5))
	{
		case 0:
		{
			/* Horizontal walls */
			for (x = x1 + 1; x < x2; x++)
			{
				set_feat_bold(x, yval - 3, FEAT_WALL_INNER);
				set_feat_bold(x, yval - 1, FEAT_WALL_INNER);
				set_feat_bold(x, yval + 1, FEAT_WALL_INNER);
				set_feat_bold(x, yval + 3, FEAT_WALL_INNER);
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
				set_feat_bold(xval + sgn, y, FEAT_WALL_INNER);
			}

			/* Bottom Wall */
			for (y = y2; y >= yval; y--)
			{
				set_feat_bold(xval - sgn, y, FEAT_WALL_INNER);
			}

			break;
		}

		case 2:
		{
			/* Horizontal internal wall */
			for (x = xval - 4; x <= xval + 4; x++)
			{
				set_feat_bold(x, yval, FEAT_WALL_INNER);
			}

			break;
		}

		case 3:
		{
			/* Plus */
			generate_plus(x1 + 2, y1 + 2, x2 - 2, y2 - 2, FEAT_WALL_INNER);

			break;
		}

		case 4:
		{
			/* Anti-plus */
			generate_plus(x1, y1, x2, y2, FEAT_WALL_INNER);
			generate_plus(x1 + 5, y1 + 3, x2 - 5, y2 - 3, dun->feat_floor);

			break;
		}
	}
}


/*
 * Type 15 -- Parallelogram Shaped Rooms
 */
static void build_type15(int bx0, int by0)
{
	u16b h, w;
	int y, x, y1, x1, yval, xval;
	bool light, type;

	/* Get size + shape */
	h = (u16b)rand_range(6, 15);
	w = (u16b)rand_range(11, 22);


	/* Try to allocate space for room. */
	if (!room_alloc(w + h, h, FALSE, bx0, by0, &xval, &yval)) return;

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
				set_feat_bold(x + y, y + y1, dun->feat_floor);
			}
			else
			{
				/* Sloping up and right */
				set_feat_bold(x + h - y, y + y1, dun->feat_floor);
			}
		}
	}

	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(xval, yval, light, x1, y1, x1 + h + w, y1 + h);
}


/*
 * Type 16 -- Rectangular room with chunks removed
 */
static void build_type16(int bx0, int by0)
{
	int xval, yval;
	int y1, x1, y2, x2;
	int tx1 = 0, tx2 = 0, ty1 = 0, ty2 = 0;
	bool light;

	int num, i;
	
	int xsize, ysize;
	
	/* Pick a room size */
	y1 = randint1(4);
	x1 = randint1(11);
	y2 = randint1(3);
	x2 = randint1(11);

	xsize = x1 + x2 + 1;
	ysize = y1 + y2 + 1;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 2, ysize + 2, FALSE, bx0, by0, &xval, &yval))
		return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));


	/* Get corner values */
	y1 = yval - ysize / 2;
	x1 = xval - xsize / 2;
	y2 = yval + (ysize - 1) / 2;
	x2 = xval + (xsize - 1) / 2;

	/* Generate inner floors */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);

	/* Fill boundary with random rectangles */

	/* Determine number of sections to use */
	num = rand_range(1, 2);

	for (i = 0; i < num; i++)
	{
		switch (randint1(4))
		{
			case 1:
			{
				/* Top Left */
				tx1 = x1;
				tx2 = rand_range((x1 + xval) / 2, xval - 1);
				ty1 = y1;
				ty2 = rand_range((y1 + yval) / 2, yval - 1);

				break;
			}
			
			case 2:
			{
				/* Bottom Left */
				tx1 = x1;
				tx2 = rand_range((x1 + xval) / 2, xval - 1);
				ty1 = rand_range(yval + 1, (yval + y2) / 2);
				ty2 = y2;
			
				break;
			}
		
			case 3:
			{
				/* Top Right */
				tx1 = rand_range(xval + 1, (xval + x2) / 2);
				tx2 = x2;
				ty1 = y1;
				ty2 = rand_range((y1 + yval) / 2, yval - 1);

				break;
			}
			
			case 4:
			{
				/* Bottom Right */
				tx1 = rand_range(xval + 1, (xval + x2) / 2);
				tx2 = x2;
				ty1 = rand_range(yval + 1, (yval + y2) / 2);
				ty2 = y2;

				break;
			}
		}
	
		/* Create regions */
		generate_fill(tx1, ty1, tx2, ty2, FEAT_WALL_EXTRA);
	}
	
	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(xval, yval, light, x1 - 1, y1 - 1, x2 + 1, y2 + 1);
}


/*
 * Test a point to see if it lies within a triangular
 * shaped region defined by three other points.
 *
 * det is the determinate of two vectors.
 *
 * (It is the sign of the "handedness" of the triangle * 1/2
 * of its area.)
 */
static bool test_tri(int px, int py, int x1, int y1, int x2, int y2, int x3, int y3, int det)
{
	/*
	 * Use the cross product to make sure the point is on the 'right side'
	 * of the angle defined by the particular side, and the vector from a
	 * vertex of that side to the specified point.
	 *
	 * Do this three times, once for each side of the triangle.
	 */
	if (det > 0)
	{
		if ((x2 - x1) * (py - y1) - (px - x1) * (y2 - y1) < 0) return (FALSE);
		if ((x3 - x2) * (py - y2) - (px - x2) * (y3 - y2) < 0) return (FALSE);
		if ((x1 - x3) * (py - y3) - (px - x3) * (y1 - y3) < 0) return (FALSE);
	}
	else
	{
		if ((x2 - x1) * (py - y1) - (px - x1) * (y2 - y1) > 0) return (FALSE);
		if ((x3 - x2) * (py - y2) - (px - x2) * (y3 - y2) > 0) return (FALSE);
		if ((x1 - x3) * (py - y3) - (px - x3) * (y1 - y3) > 0) return (FALSE);
	}

	/* Inside the region. */
	return (TRUE);
}
 
/*
 * Make sure two squares are connected by floors.
 */
static void connectsq(int x1, int y1, int x2, int y2)
{
	int x, y;
	int l, length = distance(x1, y1, x2, y2);
	
	/* Paranoia */
	if (!length) return;

	/* Be dumb, and use a straight line */
	for (l = 0; l <= length; l++)
	{
		x = x1 + l * (x2 - x1) / length;
		y = y1 + l * (y2 - y1) / length;
	
		set_feat_bold(x, y, dun->feat_floor);
	}
}


/*
 * Type 17 -- Room made of Triangles
 */
static void build_type17(int bx0, int by0)
{
	int xval, yval;
	int y1, x1, y2, x2;
	bool light;

	int det;

	int x, y;
	int vx1, vy1, vx2, vy2, vx3, vy3;

	int num, i;
	
	int xsize, ysize;
	
	/* Pick a room size */
	y1 = randint1(14);
	x1 = randint1(14);
	y2 = randint1(14);
	x2 = randint1(14);

	xsize = x1 + x2 + 1;
	ysize = y1 + y2 + 1;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 2, ysize + 2, FALSE, bx0, by0, &xval, &yval))
		return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Get corner values */
	y1 = yval - ysize / 2;
	x1 = xval - xsize / 2;
	y2 = yval + (ysize - 1) / 2;
	x2 = xval + (xsize - 1) / 2;
	
	/* Paranoia - Room is too small, just make a rectangle */
	if (xsize * ysize < 20)
	{
		/* Generate new room */
		generate_room(x1, y1, x2, y2, light);

		/* Generate inner floors */
		generate_fill(x1 + 1, y1 + 1, x2 - 1, y2 - 1, dun->feat_floor);
	
		/* Generate outer walls */
		generate_draw(x1, y1, x2, y2, FEAT_WALL_OUTER);
	
		/* Done */
		return;
	}
	
	/* Determine number of shapes to use */
	num = rand_range(2, 4);

	/* Fill with random triangles */

	/* Make num rooms */
	for (i = 0; i < num; i++)
	{
		do
		{
			/* Get vertices */
			vx1 = rand_range(x1, x2);
			vx2 = rand_range(x1, x2);
			vx3 = rand_range(x1, x2);
		
			vy1 = rand_range(y1, y2);
			vy2 = rand_range(y1, y2);
			vy3 = rand_range(y1, y2);
			
			/*
			 * Calculate the cross product of two vectors that
			 * define the sides of the triangle.
			 *
			 * This will be equal to twice the area of the triangle
			 * times a sign factor that depends on the ordering
			 * of the vertex points in space.
			 */
			det = (vx2 - vx1) * (vy3 - vy1) - (vx3 - vx1) * (vy2 - vy1);
		
			/* Make sure the triangle is large enough. */
		} while (abs(det) < 10);
		
		for (x = x1; x <= x2; x++)
		{
			for (y = y1; y <= y2; y++)
			{
				if (test_tri(x, y, vx1, vy1, vx2, vy2, vx3, vy3, det))
				{
					set_feat_bold(x, y, dun->feat_floor);
				}
			}
		}
		
		/* Hack - connect to room center */
		connectsq(xval, yval, (vx1 + vx2 + vx3) / 3, (vy1 + vy2 + vy3) / 3);
		
		/* Hack - connect vertexes to avoid problems with rounding */
		connectsq(vx1, vy1, vx2, vy2);
		connectsq(vx1, vy1, vx3, vy3);
		connectsq(vx3, vy3, vx2, vy2);
	}
	
	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(xval, yval, light, x1 - 1, y1 - 1, x2 + 1, y2 + 1);
}


/*
 * Type 18 -- Large room with many small rooms inside.
 *
 * (A jail).
 */
static void build_type18(int bx0, int by0)
{
	int y1, x1;
	int y2, x2, yval, xval;
	bool light;
	
	int i;

	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, bx0, by0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	/* Generate new room */
	generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);

	/* Generate inner floors */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);
	
	for (i = 0; i < 6; i++)
	{
		generate_draw(x1 - 1 + i * 4, y1 - 1, x1 + 3 + i * 4, yval - 1, FEAT_WALL_INNER);
		generate_draw(x1 - 1 + i * 4, yval + 1, x1 + 3 + i * 4, y2 + 1, FEAT_WALL_INNER);
		place_random_door(x1 + 1 + i * 4, yval - 1);
		place_random_door(x1 + 1 + i * 4, yval + 1);
	}
	
	/* Generate outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);
}


/*
 * Type 19 -- Channel or reservoir.
 */
static void build_type19(int bx0, int by0)
{
	int y, x, y1, x1;
	int y2, x2, yval, xval;
	bool light;
	
	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));
	
	/* Vertical */
	if (one_in_(2))
	{
		/* Try to allocate space for room. */
		if (!room_alloc(41, 11, FALSE, bx0, by0, &xval, &yval)) return;
		
		/* Large, long room */
		y1 = yval - 4;
		y2 = yval + 4;
		x1 = xval - 19;
		x2 = xval + 19;
		
		/* Generate new room */
		generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);
		
		/* Generate outer walls */
		generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);
		
		/* Generate floor */
		generate_draw(x1, y1, x2, y2, dun->feat_floor);
		
		/* Generate liquid */
		generate_draw(x1, y1 + 1, x2, y2 - 1, dun->feat_shal_liquid);

		for (x = x1; x <= x2; x++)
		{
			for (y = y1 + 2; y <= y2 - 2; y++)
			{
				if (randint1(p_ptr->depth + 10) > 10)
					set_feat_bold(x, y, dun->feat_deep_liquid);
				else
					set_feat_bold(x, y, dun->feat_shal_liquid);
			}
		}
	}
	else
	{
		/* Try to allocate space for room. */
		if (!room_alloc(11, 33, FALSE, bx0, by0, &xval, &yval)) return;
		
		/* Large, long room */
		y1 = yval - 15;
		y2 = yval + 15;
		x1 = xval - 4;
		x2 = xval + 4;
		
		/* Generate new room */
		generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);
		
		/* Generate outer walls */
		generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);
		
		/* Generate floor */
		generate_draw(x1, y1, x2, y2, dun->feat_floor);
		
		/* Generate liquid */
		generate_draw(x1 + 1, y1, x2 - 1, y2, dun->feat_shal_liquid);

		for (x = x1 + 2; x <= x2 - 2; x++)
		{
			for (y = y1; y <= y2; y++)
			{
				if (randint1(p_ptr->depth + 10) > 10)
					set_feat_bold(x, y, dun->feat_deep_liquid);
				else
					set_feat_bold(x, y, dun->feat_shal_liquid);
			}
		}
	}
}


/*
 * Type 20 -- Collapsed room.
 */
static void build_type20(int bx0, int by0)
{
	int y1, x1;
	int y2, x2, yval, xval;
	bool light;
	
	int xsize, ysize;
	
	int i;
	
	/* Pick a room size */
	y1 = randint1(4);
	x1 = randint1(11);
	y2 = randint1(3);
	x2 = randint1(11);

	xsize = x1 + x2 + 1;
	ysize = y1 + y2 + 1;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 2, ysize + 2, FALSE, bx0, by0, &xval, &yval))
		return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));
	
	/* Get corner values */
	y1 = yval - ysize / 2;
	x1 = xval - xsize / 2;
	y2 = yval + (ysize - 1) / 2;
	x2 = xval + (xsize - 1) / 2;
	
	/* Generate new room */
	generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);

	/* Generate outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);
	
	/* Fill with rock and rubble */
	for (i = randint1(xsize * ysize / 4); i > 0; i--)
	{
		if (one_in_(2))
		{
			/* Rock */
			set_feat_bold(rand_range(x1, x2), rand_range(y1, y2), FEAT_WALL_INNER);
		}
		else
		{
			/* Rubble */
			set_feat_bold(rand_range(x1, x2), rand_range(y1, y2), FEAT_RUBBLE);
		}
	}
}


/*
 * Type 21 -- Crypt Mk II.
 */
static void build_type21(int bx0, int by0)
{
	int y1, x1;
	int y2, yval, xval;
	bool light;
		
	int i;
	
	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, bx0, by0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	
	
	/* Generate new room */
	generate_room(x1 - 1, y1 - 1, x1 + 21, y2 + 1, light);
	
	/* Draw the chambers */
	for (i = 0; i < 3; i++)
	{
		/* Top room */
	
		/* Generate outer walls */
		generate_draw(x1 - 1 + i * 8, y1 - 1, x1 + 5 + i * 8, y1 + 3, FEAT_WALL_OUTER);
		
		/* Generate inner floors */
		generate_fill(x1 + i * 8, y1, x1 + 4 + i * 8, y1 + 2, dun->feat_floor);
		
		
		/* Bottom room */

		/* Generate outer walls */
		generate_draw(x1 - 1 + i * 8, y2 - 3, x1 + 5 + i * 8, y2 + 1, FEAT_WALL_OUTER);
		
		/* Generate inner floors */
		generate_fill(x1 + i * 8, y2 - 2, x1 + 4 + i * 8, y2, dun->feat_floor);
	}
	
	/*
	 * Hack - fill middle area with FEAT_WALL_INNER
	 * so it doesn't get disturbed.
	 */
	generate_draw(x1, yval - 1, x1 + 20, yval + 1, FEAT_WALL_INNER);
	
	/* Draw the connecting tunnels */
	for (i = 0; i < 3; i++)
	{
		generate_line(x1 + i * 8 + 2, y1 + 3, x1 + i * 8 + 2, y2 - 3, dun->feat_floor);
	}

	
	/* Finally - connect the chambers */
	generate_line(x1 - 1, yval, x1 + 21, yval, dun->feat_floor);
}


/*
 * Type 22 -- Large Chamber
 */
static void build_type22(int bx0, int by0)
{
	int xval, yval;
	int y1, x1, y2, x2;
	bool light;

	int x, y;

	int xcount, ycount;
	
	int xsize, ysize;
	
	/* Pick a room size */
	y1 = rand_range(5, 14);
	x1 = rand_range(5, 20);
	y2 = rand_range(5, 14);
	x2 = rand_range(5, 20);

	xsize = x1 + x2 + 1;
	ysize = y1 + y2 + 1;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 2, ysize + 2, FALSE, bx0, by0, &xval, &yval))
		return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Get corner values */
	y1 = yval - ysize / 2;
	x1 = xval - xsize / 2;
	y2 = yval + (ysize - 1) / 2;
	x2 = xval + (xsize - 1) / 2;
	
	/* Generate new room */
	generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);

	/* Generate outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);

	/* Work out how many pillars to use in each direction */
	xcount = xsize / 5;
	ycount = ysize / 5;

	/* Add some pillars */
	for (y = 0; y <= ycount; y++)
	{
		for (x = 0; x <= xcount; x++)
		{
			set_feat_bold(x1 + (xsize - 1) * x / xcount,
						  y1 + (ysize - 1) * y / ycount, FEAT_PILLAR);
		}
	}
}

/*
 * Type 23 -- Semicircular room.
 */
static void build_type23(int bx0, int by0)
{
	int rad, x, y, x0, y0;
	int light = FALSE;

	int xc, yc;

	/* Occasional light */
	if (randint1(p_ptr->depth) <= 15) light = TRUE;
	
	rad = rand_range(2, 9);

	/* Get orientation */
	if (one_in_(2))
	{
		/* Allocate in room_map.  If will not fit, exit */
		if (!room_alloc(rad + 1, rad * 2 + 1, FALSE, bx0, by0, &x0, &y0))
			return;
		
		/* Flip left or right? */
		if (one_in_(2))
		{
			xc = x0 - rad / 2;
		}
		else
		{
			xc = x0 + rad / 2;
		}
		
		/* Make semicircular floor */
		for (x = x0 - rad / 2; x <= x0 + rad / 2; x++)
		{
			for (y = y0 - rad; y <= y0 + rad; y++)
			{
				if (distance(xc, y0, x, y) <= rad - 1)
				{
					/* inside- so is floor */
					set_feat_bold(x, y, dun->feat_floor);
				}
					else if (distance(xc, y0, x, y) <= rad + 1)
				{
					/* make granite outside so arena works */
					set_feat_bold(x, y, FEAT_WALL_EXTRA);
				}
			}
		}
	}
	else
	{
		/* Allocate in room_map.  If will not fit, exit */
		if (!room_alloc(rad * 2 + 1, rad + 1, FALSE, bx0, by0, &x0, &y0))
			return;
		
		/* Flip up or down? */
		if (one_in_(2))
		{
			yc = y0 - rad / 2;
		}
		else
		{
			yc = y0 + rad / 2;
		}
	
		/* Make semicircular floor */
		for (x = x0 - rad; x <= x0 + rad; x++)
		{
			for (y = y0 - rad / 2; y <= y0 + rad / 2; y++)
			{
				if (distance(x0, yc, x, y) <= rad - 1)
				{
					/* inside- so is floor */
					set_feat_bold(x, y, dun->feat_floor);
				}
				else if (distance(x0, yc, x, y) <= rad + 1)
				{
					/* make granite outside so arena works */
					set_feat_bold(x, y, FEAT_WALL_EXTRA);
				}
			}
		}
	}
	
	/* Find visible outer walls and set to be FEAT_OUTER */
	add_outer_wall(x0, y0, light, x0 - rad, y0 - rad, x0 + rad, y0 + rad);
}


/*
 * Type 24 -- Hourglass-shaped room.
 */
static void build_type24(int bx0, int by0)
{
	int y1, x1;
	int y2, x2, yval, xval;
	bool light;
	
	int i;

	/* Try to allocate space for room. */
	if (!room_alloc(25, 11, FALSE, bx0, by0, &xval, &yval)) return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Large room */
	y1 = yval - 4;
	y2 = yval + 4;
	x1 = xval - 11;
	x2 = xval + 11;

	/* Generate new room */
	generate_room(x1 - 1, y1 - 1, x2 + 1, y2 + 1, light);
	
	/* Generate outer walls */
	generate_draw(x1 - 1, y1 - 1, x2 + 1, y2 + 1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(x1, y1, x2, y2, dun->feat_floor);
	
	/* Create triangular features */
	for (i = 1; i < 4; i++)
	{
		generate_line(x1 + i * 3, y1 + i, x2 - i * 3, y1 + i, FEAT_WALL_INNER);
		generate_line(x1 + i * 3, y2 - i, x2 - i * 3, y2 - i, FEAT_WALL_INNER);
	}
	
	/* Put a door in the middle */
	place_random_door(xval, yval);
}


/*
 * Overlay a room that will overlap others.
 */
static void overdraw_room(int x1, int y1, int x2, int y2, bool light)
{
	int x, y, i, j;
	cave_type *c_ptr;
	
	bool connected = FALSE;
	int overlap = 0;
	int empty = 0;
	int wallcount = 0;
	
	/* Pass 1: Check that there is overlap */
	for (x = x1; x <= x2; x++)
	{
		for(y = y1; y <= y2; y++)
		{
			c_ptr = cave_p(x, y);
			
			/* There is a room square here */
			if (c_ptr->info & CAVE_ROOM)
			{
				overlap++;
				
				/* Is it an outer wall? */
				if (c_ptr->feat == FEAT_WALL_OUTER)
				{
					wallcount++;
				}
			}
			else
			{
				empty++;
			}
		}
	}

	/* The room must connect with others */
	if (overlap < 5) return;

	/* Not too much overlap */
	if (overlap > 20) return;
	
	/* Does it add enough? */
	if (empty < 30) return;
	
	/* Pass 2: Add floor */
	for (x = x1 + 1; x <= x2 - 1; x++)
	{
		for(y = y1 + 1; y <= y2 - 1; y++)
		{
			c_ptr = cave_p(x, y);
			
			/* There is a room square here */
			if (c_ptr->info & CAVE_ROOM) continue;
			
			cave_set_feat(x, y, dun->feat_floor);
		}
	}
	
	/* Pass 3: Add outer walls */
	for (x = x1; x <= x2; x++)
	{
		for (y = y1; y <= y2; y++)
		{
			c_ptr = cave_p(x, y);
						
			/* There is a room square here */
			if (!(c_ptr->info & CAVE_ROOM))
			{
				if (c_ptr->feat == FEAT_WALL_EXTRA)
				{
					/* Scan for adjacent floor */
					for (i = -1; i <= 1; i++)
					{
						for (j = -1; j <= 1; j++)
						{
							/* Paranoia */
							if (!in_bounds(x + i, y + j)) continue;
							
							/* Are we next to a floor? */
							if (cave_p(x + i, y + j)->feat == dun->feat_floor)
							{
								/* Outer walls */
								cave_set_feat(x, y, FEAT_WALL_OUTER);
								
								/* Quickly exit the nested loops */
								goto exitloop;
							}
						}
					}
				}
			}
			
			exitloop: ;
		}
	}
	
	/* Pass 4: Add doors */
	for (x = x1; x <= x2; x++)
	{
		for(y = y1; y <= y2; y++)
		{
			c_ptr = cave_p(x, y);
			
			/* There is a room square here */
			if (c_ptr->info & CAVE_ROOM)
			{
				if (one_in_(wallcount / 2) && add_door(x, y))
				{
					connected = TRUE;
				}
			}
		}
	}
	
	/* Pass 5: Convert to inner walls, and build the room itself */
	for (x = x1; x <= x2; x++)
	{
		for(y = y1; y <= y2; y++)
		{
			c_ptr = cave_p(x, y);
			
			/* There is a room square here */
			if (c_ptr->info & CAVE_ROOM)
			{
				if (c_ptr->feat == FEAT_WALL_OUTER)
				{
					if (!connected && one_in_(wallcount--))
					{
						/* Emergency - open passage into room */
						cave_set_feat(x, y, dun->feat_floor);
						
						connected = TRUE;
					}
					else
					{
						/* Inner walls */
						cave_set_feat(x, y, FEAT_WALL_INNER);
					}
				}
			}
			else
			{
				if (c_ptr->feat != FEAT_WALL_EXTRA)
				{
					/* Add room */
					c_ptr->info |= CAVE_ROOM;
					
					if (light) c_ptr->info |= CAVE_GLOW;
				}
			}
		}
	}
}


/*
 * Type 25 -- Connected small rooms
 */
static void build_type25(int bx0, int by0)
{
	int xval, yval;
	int y1, x1, y2, x2;
	bool light;
	
	int xi1, yi1, xi2, yi2;
	
	int xsize, ysize;
	
	int i;
	
	/* Pick a room size */
	y1 = rand_range(5, 14);
	x1 = rand_range(5, 20);
	y2 = rand_range(5, 14);
	x2 = rand_range(5, 20);

	xsize = x1 + x2 + 1;
	ysize = y1 + y2 + 1;

	/* Try to allocate space for room.  If fails, exit */
	if (!room_alloc(xsize + 2, ysize + 2, FALSE, bx0, by0, &xval, &yval))
		return;

	/* Choose lite or dark */
	light = (p_ptr->depth <= randint1(25));

	/* Get corner values */
	y1 = yval - ysize / 2;
	x1 = xval - xsize / 2;
	y2 = yval + (ysize - 1) / 2;
	x2 = xval + (xsize - 1) / 2;
	
	/* Add "starting" room */
	
	/* Generate new room */
	generate_room(xval - 3, yval - 3, xval + 3, yval + 3, light);

	/* Generate outer walls */
	generate_draw(xval - 3, yval - 3, xval + 3, yval + 3, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(xval - 2, yval - 2, xval + 2, yval + 2, dun->feat_floor);

	/*
	 * Add other rooms
	 *
	 * Note that the size of the rooms is forced
	 * to be odd, and the walls are forced to be
	 * on "even" x and y coordinates.  This makes
	 * them fit together better.
	 */
	for (i = 20; i > 0; i--)
	{
		/* Pick new room to add */
		xi1 = rand_range(x1 - 1, x2 - 5);
		yi1 = rand_range(y1 - 1, y2 - 5);
		
		if (xi1 % 2) xi1++;
		if (yi1 % 2) yi1++;
		
		xi2 = xi1 + rand_range(5, 10);
		yi2 = yi1 + rand_range(5, 10);
		
		if (xi2 % 2) xi2++;
		if (yi2 % 2) yi2++;
		
		/* Out of the box */
		if ((xi2 > x2) || (yi2 > y2)) continue;
	
		/* Draw the room, and connect it */
		overdraw_room(xi1, yi1, xi2, yi2, light);
	}
}

#define ROOM_TYPES	30

typedef void (*room_build_type)(int, int);

typedef struct room_type room_type;

struct room_type
{
	const int depth;	/* Minimum depth */
	const int chance;       /* Relative probability (higher is more common) */
	const room_build_type build_func;	/* Function to build room */
	const u16b flags;
};


/*
 * There are some duplicate entries, which cause the more interesting rooms
 * to become more common at deeper depths.
 */
room_type room_list[ROOM_TYPES] =
{
	{1,  30, build_type1, RT_SIMPLE},	/* Simple Rectangle */
	{1,  10, build_type2, RT_FANCY},	/* Overlapping */
	{1,  10, build_type20, RT_RUIN},	/* Collapsed */
	{3,  10, build_type3, RT_FANCY},	/* Crossed */
	{3,  10, build_type4, RT_BUILDING | RT_CRYPT},	/* Large nested */
	{3,  10, build_type11, RT_NATURAL | RT_FANCY},	/* Circle */
	{3,  10, build_type14, RT_COMPLEX},	/* Large with walls */
	{3,  10, build_type15, RT_STRANGE},	/* Parallelogram */
	{3,  10, build_type16, RT_RUIN | RT_NATURAL},	/* Rectangle minus inverse overlap */
	{3,  10, build_type17, RT_RUIN},	/* Triangles */
	{3,  10, build_type23, RT_FANCY},	/* Semicircle */
	{3,  10, build_type24, RT_COMPLEX},	/* Hourglass */
	{3,  10, build_type25, RT_BUILDING},	/* Connected rooms */
	{5,  10, build_type9, RT_NATURAL},	/* Fractal cave */
	{5,  10, build_type13, RT_NATURAL},	/* Large with fractal feature */
	{5,  10, build_type18, RT_BUILDING},	/* Chambers */
	{5,  10, build_type19, RT_STRANGE},	/* Channel */
	{7,  10, build_type22, RT_COMPLEX},	/* Very large pillared chamber */
	{10, 10, build_type5, RT_ANIMAL | RT_TAG_CROWDED},	/* Monster nest */
	{10, 10, build_type12, RT_CRYPT},	/* Crypt I */
	{10, 10, build_type21, RT_CRYPT},	/* Crypt II */
	{12, 10, build_type7, RT_DENSE},	/* Small vault */
	{12, 10, build_type10, RT_RVAULT},	/* Random vault */
	{15, 10, build_type6, RT_DENSE | RT_TAG_CROWDED},	/* Monster pit */
	{20, 10, build_type8, RT_DENSE},	/* Large vault */
	{25, 10, build_type10, RT_RVAULT},	/* Random vault */
	{30, 10, build_type5, RT_ANIMAL | RT_TAG_CROWDED},	/* Monster nest */
	{35, 10, build_type7, RT_DENSE},	/* Small vault */
	{40, 10, build_type6, RT_DENSE | RT_TAG_CROWDED},	/* Monster pit */
	{45, 10, build_type8, RT_DENSE},	/* Large vault */
};


/*
 * Select a room type, and then attempt to build a it at the given block
 *
 * Note that we restrict the number of "crowded" rooms to reduce
 * the chance of overflowing the monster list during level creation.
 */
bool room_build(void)
{
	int x, y;

	int type = 0;

	int i;
	int val, total;

	int depth;

	/* Choose base depth */
	depth = p_ptr->depth;

	/* Occasionally give a chance for an "out-of-depth" room */
	if (one_in_(10)) depth += randint1(5);
	if (one_in_(10)) depth += randint1(5);
	
	/* Collect the total possible chance */
	total = 0;
	for (i = 0; i < ROOM_TYPES; i++)
	{
		if (depth < room_list[i].depth) continue;
		if (!(dun->room_types & room_list[i].flags)) continue;
		if ((dun->crowded >= 2) && (room_list[i].flags & RT_TAG_CROWDED)) continue;
		
		total += room_list[i].chance;
	}
	
	/* Find a room type for this dungeon */
	val = randint0(total);
	for (i = 0; i < ROOM_TYPES; i++)
	{
		
		if (depth < room_list[i].depth) continue;
		if (!(dun->room_types & room_list[i].flags)) continue;
		if ((dun->crowded >= 2) && (room_list[i].flags & RT_TAG_CROWDED)) continue;
		
		val -= room_list[i].chance;

		if (val < 0)
		{
			type = i;
			break;
		}
	}

	/* Pick a block for the room */
	x = randint0(dun->col_rooms);
	y = randint0(dun->row_rooms);
	
	/* Build a room at a random position */
	room_list[type].build_func(x, y);

	return (TRUE);
}
