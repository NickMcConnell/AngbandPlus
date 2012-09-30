/* File: wild2.c */

/* Purpose: Wilderness generation */

/*
 * Copyright (c) 1989, 1999 James E. Wilson, Robert A. Koeneke,
 * Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#include "wild.h"
#include "grid.h"

/*
 * Helper functions that can be called recursively.  (Need function prototypes.)
 * See make_wild_03() for an instance of this.
 * This ability will also be used by other routines in the future.
 */
void gen_block_helper(blk_ptr block_ptr, byte *data, int gen_type);
void blend_helper(cave_type *c_ptr, byte *data, int g_type);


/* Lighten / Darken new block depending on Day/ Night */
void light_dark_block(blk_ptr block_ptr, int x, int y)
{
	int i, j;

	bool daytime;
	cave_type *c_ptr;


	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
		daytime = TRUE;
	else
		daytime = FALSE;

	/* If is daytime - have seen this square */
	if (daytime) wild[y][x].done.info |= WILD_INFO_SEEN;

	/* Light up or darken the area */
	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			/* Get the cave grid */
			c_ptr = &block_ptr[j][i];

			/* Hack -- Notice spot */
			note_wild_spot(c_ptr);

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
				if (!(((c_ptr->feat >= FEAT_OPEN) &&
				    (c_ptr->feat <= FEAT_MORE)) ||
				    ((c_ptr->feat >= FEAT_CLOSED) &&
					(c_ptr->feat <= FEAT_PERM_SOLID))))
				{
					/* Forget the grid */
					c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);
				}
				else
				{
					/* Assume lit */
					c_ptr->info |= (CAVE_GLOW);

					/* Hack -- Memorize lit grids if allowed */
					if (view_perma_grids) c_ptr->info |= (CAVE_MARK);
				}
			}
		}
	}
}


/*
 * Lighten / Darken Wilderness
 */
static void day_night(void)
{
	u16b x, y;

	/* Light up or darken the area */
	for (y = 0; y < WILD_GRID_SIZE; y++)
	{
		for (x = 0; x < WILD_GRID_SIZE; x++)
		{
			/* Light or darken wilderness block */
			light_dark_block(wild_grid.block_ptr[y][x],
			                 (x + min_wid / 16),
			                 (y + min_hgt / 16));
		}
	}
}


/*
 * Access the old cave array.
 */
static cave_type *access_cave(int y, int x)
{
	return &cave[y][x];
}


/*
 * Access wilderness
 */
static cave_type *access_wild(int y, int x)
{
	/*
	 * Divide by 16 to get block.
	 * Logical AND with 15 to get location within block.
	 */

	return &wild_grid.block_ptr[(y>>4) - wild_grid.y]
		[(x>>4) - wild_grid.x][y & 15][x & 15];
}


/* Town currently stored in cave[][] */
static u16b cur_town = 0;

/* Information about town in cave[][] */
static int town_offset_y = 0;


/*
 * Builds a store at a given pseudo-location
 *
 * As of Z 2.5.0 the town is moved back to (0,0) - and is overlayed
 * on top of the wilderness.
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
static void build_store(int yy, int xx, store_type *st_ptr)
{
	int y, x, y0, x0, y1, x1, y2, x2, tmp;


	/* Find the "center" of the store */
	y0 = yy * 6 + 4;
	x0 = xx * 17 + 8;

	/* Determine the store boundaries */
	y1 = y0 - randint1(2);
	y2 = y0 + randint1(2);
	x1 = x0 - randint1(5);
	x2 = x0 + randint1(5);

	/* Build an invulnerable rectangular building */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* Create the building */
			cave[y][x].feat = FEAT_PERM_EXTRA;
		}
	}

	/* Pick a door direction (S,N,E,W) */
	tmp = randint0(4);

	/* Re-roll "annoying" doors */
	if (((tmp == 0) && (yy == 2)) ||
	    ((tmp == 1) && (yy == 0)) ||
	    ((tmp == 2) && (xx == 2)) ||
	    ((tmp == 3) && (xx == 0)))
	{
		/* Pick a new direction */
		tmp = randint0(4);
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
	cave[y][x].feat = FEAT_FLOOR;
	cave[y][x].fld_idx = wild_build[st_ptr->type].field;

	/* Save location of store door */
	st_ptr->x = x;
	st_ptr->y = y;
}



/*
 * Generate the "consistent" town features, and place the player
 *
 * Hack -- play with the R.N.G. to always yield the same town
 * layout, including the size and shape of the buildings, the
 * locations of the doorways, and the location of the stairs.
 *
 * This simple routine does not check the type of stores town_num wants.
 */
static void town_gen_hack(u16b town_num)
{
	int y, x, k, n, xx, yy;
	int rooms[MAX_STORES];
	byte feat;

	/* Prepare an array of "remaining stores", and count them */
	for (n = 0; n < MAX_STORES; n++) rooms[n] = n;

	/* Place three rows of stores */
	for (y = 0; y < 3; y++)
	{
		/* Place three stores per row */
		for (x = 0; x < 3; x++)
		{
			/* Pick a random unplaced store */
			k = ((n <= 1) ? 0 : randint0(n));

			/* Build that store at the proper location */
			build_store(y, x, &town[town_num].store[rooms[k]]);

			/* Shift the stores down, remove one store */
			rooms[k] = rooms[--n];
		}
	}


	/* Place the stairs */
	while (TRUE)
	{
		/* Pick a location at least "three" from the outer walls */
		yy = rand_range(3, TOWN_HGT - 4);
		xx = rand_range(3, TOWN_WID - 4);

		feat = cave[yy][xx].feat;

		/* If square is a shop then try again */
		if ((feat == FEAT_PERM_EXTRA) || (cave[yy][xx].fld_idx != 0))
			continue;

		/* Blank square */
		break;
	}

	/* Put dungeon floor next to stairs so they are easy to find. */
	for (y = -1; y <= 1; y++)
	{
		for (x = -1; x <= 1; x++)
		{
			/* Get feat at location */
			feat = cave[yy + y][xx + x].feat;

			if (feat == FEAT_PERM_EXTRA) continue;

			/* Convert square to dungeon floor */
			cave[yy + y][xx + x].feat = FEAT_FLOOR;
		}
	}

	/* Clear previous contents, add down stairs */
	cave[yy][xx].feat = FEAT_MORE;

	wild_stairs_x = xx;
	wild_stairs_y = yy;
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
 * xx and yy point to the location of the stairs (So the player can
 * start there.)
 *
 * (Vanilla town only now.)
 */
void van_town_gen(u16b town_num)
{
	int y, x;

	/* Place transparent area */
	for (y = 0; y < MAX_HGT; y++)
	{
		for (x = 0; x < MAX_WID; x++)
		{
			/* Create empty area */
			cave[y][x].feat = FEAT_PERM_EXTRA;
			cave[y][x].fld_idx = 0;
		}
	}

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = town[town_num].seed;

	/* Place some floors */
	for (y = 1; y < TOWN_HGT - 1; y++)
	{
		for (x = 1; x < TOWN_WID - 1; x++)
		{
			/* Create see-through terrain */
			cave[y][x].feat = FEAT_FLOOR;
		}
	}

	/* Build stuff */
	town_gen_hack(town_num);

	/* Town is now built */
	cur_town = 0;

	/* Hack -- use the "complex" RNG */
	Rand_quick = FALSE;
}



/*
 * Given a location - convert to a location in cave[][]
 */
static bool get_city_block_locat(int *x, int *y)
{
	*x = *x * 8;
	*y = (*y - town_offset_y * 2) * 8;


	if ((*x >=0) && (*y >= 0) && (*x < MAX_WID - 8) && (*y < MAX_HGT - 8))
	{
		return (TRUE);
	}
	else
	{
		return (FALSE);
	}
}

static void draw_general(int x0, int y0, store_type *st_ptr, int x, int y)
{
	int i, j;

	/* Ignore currently unused parameters */
	(void) x;
	(void) y;

	switch (st_ptr->type)
	{
		case BUILD_STAIRS:
		{
			/* Put dungeon floor next to stairs so they are easy to find. */
			for (i = -1; i <= 1; i++)
			{
				for (j = -1; j <= 1; j++)
				{
					/* Convert square to dungeon floor */
					cave[y0 + j][x0 + i].feat = FEAT_FLOOR;
				}
			}

			/* Clear previous contents, add down stairs */
			cave[y0][x0].feat = FEAT_MORE;

			break;
		}

		case BUILD_NONE:
		{
			int x1, y1, x2, y2;

			/* Determine the store boundaries */
			y1 = y0 - randint1(3);
			y2 = y0 + randint1(2);
			x1 = x0 - randint1(3);
			x2 = x0 + randint1(3);

			/* Build an invulnerable rectangular building */
			generate_fill(y1, x1, y2, x2, FEAT_PERM_EXTRA);

			/* No doors */

			break;
		}

		case BUILD_BLANK:
		{
			/* Do Nothing */

			break;
		}
	}
}

static void draw_store(int x0, int y0, store_type *st_ptr, int x, int y)
{
	int x1, y1, x2, y2;
	int i, j;
	int tmp;

	/* Determine the store boundaries */
	y1 = y0 - randint1(3);
	y2 = y0 + randint1(2);
	x1 = x0 - randint1(3);
	x2 = x0 + randint1(3);

	/* Build an invulnerable rectangular building */
	generate_fill(y1, x1, y2, x2, FEAT_PERM_EXTRA);

	/* Pick a door direction (S,N,E,W) */
	tmp = randint0(4);

	/* Extract a "door location" */
	switch (tmp)
	{
		/* Bottom side */
		case 0:
		{
			i = y2;
			j = rand_range(x1, x2);
			break;
		}

		/* Top side */
		case 1:
		{
			i = y1;
			j = rand_range(x1, x2);
			break;
		}

		/* Right side */
		case 2:
		{
			i = rand_range(y1, y2);
			j = x2;
			break;
		}

		/* Left side */
		default:
		{
			i = rand_range(y1, y2);
			j = x1;
			break;
		}
	}

	/* Clear previous contents, add a store door */
	cave[i][j].feat = FEAT_FLOOR;

	cave[i][j].fld_idx = wild_build[st_ptr->type].field;

	/* Save location of store door */
	st_ptr->x = x * 8 + j % 8;
	st_ptr->y = y * 8 + i % 8;
}


/* Draw a building / store of a given type at a given position */
static void draw_building(byte type, byte x, byte y, u16b store, u16b town_num)
{
	/* Really dodgy - just a rectangle, independent of type, for now */
	int xx, yy;

	/* Hack - save the rng seed */
	u32b rng_save_seed = Rand_value;

	store_type *st_ptr = &town[town_num].store[store];

	/* Hack, ignore building draw type for now */
	(void) type;

	/* Save location */
	xx = x;
	yy = y;

	/* Hack - set location of stairs so we can start on them. */
	if (st_ptr->type == BUILD_STAIRS)
	{
		wild_stairs_x = x * 8 + 4;
		wild_stairs_y = y * 8 + 4;
	}

	/* Get coords */
	if (!get_city_block_locat(&xx, &yy)) return;

	/* What are we drawing? */
	if (build_is_store(st_ptr->type))
	{
		/* Draw the store */
		draw_store(xx + 4, yy + 4, st_ptr, x, y);
	}
	else if (build_is_general(st_ptr->type))
	{
		/* Draw the general feature */
		draw_general(xx + 4, yy + 4, st_ptr, x, y);
	}
	else
	{
		/* Hack - Draw the "normal" building */
		draw_store(xx + 4, yy + 4, st_ptr, x, y);
	}

	/* Hack - restore the rng seed */
	Rand_value = rng_save_seed;
}

static void draw_gates(int x, int y, byte i, byte j, town_type *t_ptr)
{
	int k, xx = x, yy = y;

	/* Draw gates if visible */
	for (k = 0; k < MAX_GATES; k++)
	{
		if ((t_ptr->gates_x[k] == i) && (t_ptr->gates_y[k] == j))
		{
			/* Add doors (hack) */

			switch (k)
			{
				case 0:
				{
					/* Hack - shift gate if next to walls */
					if (cave[y + 2][x + 3].feat == FEAT_PERM_SOLID) yy -= 3;
					if (cave[y + 5][x + 3].feat == FEAT_PERM_SOLID) yy += 3;

					y = yy;

					/* Draw an empty square */
					generate_fill(y + 3, x + 3, y + 4, x + 4, FEAT_FLOOR);

					/* Right gate */
					cave[y + 3][x + 4].fld_idx = FT_LOCK_DOOR;
					cave[y + 3][x + 4].feat = FEAT_CLOSED;
					cave[y + 4][x + 4].fld_idx = FT_LOCK_DOOR;
					cave[y + 4][x + 4].feat = FEAT_CLOSED;

					return;
				}

				case 1:
				{
					/* Hack - shift gate if next to walls */
					if (cave[y + 2][x + 3].feat == FEAT_PERM_SOLID) yy -= 3;
					if (cave[y + 5][x + 3].feat == FEAT_PERM_SOLID) yy += 3;

					y = yy;

					/* Draw an empty square */
					generate_fill(y + 3, x + 3, y + 4, x + 4, FEAT_FLOOR);

					/* Left gate */
					cave[y + 3][x + 3].fld_idx = FT_LOCK_DOOR;
					cave[y + 3][x + 3].feat = FEAT_CLOSED;
					cave[y + 4][x + 3].fld_idx = FT_LOCK_DOOR;
					cave[y + 4][x + 3].feat = FEAT_CLOSED;

					return;
				}

				case 2:
				{
					/* Hack - shift gate if next to walls */
					if (cave[y + 3][x + 2].feat == FEAT_PERM_SOLID) xx -= 3;
					if (cave[y + 3][x + 5].feat == FEAT_PERM_SOLID) xx += 3;

					x = xx;

					/* Draw an empty square */
					generate_fill(y + 3, x + 3, y + 4, x + 4, FEAT_FLOOR);

					/* Bottom gate */
					cave[y + 4][x + 3].fld_idx = FT_LOCK_DOOR;
					cave[y + 4][x + 3].feat = FEAT_CLOSED;
					cave[y + 4][x + 4].fld_idx = FT_LOCK_DOOR;
					cave[y + 4][x + 4].feat = FEAT_CLOSED;

					return;
				}

				case 3:
				{
					/* Hack - shift gate if next to walls */
					if (cave[y + 3][x + 2].feat == FEAT_PERM_SOLID) xx -= 3;
					if (cave[y + 3][x + 5].feat == FEAT_PERM_SOLID) xx += 3;

					x = xx;

					/* Draw an empty square */
					generate_fill(y + 3, x + 3, y + 4, x + 4, FEAT_FLOOR);

					/* Top gate */
					cave[y + 3][x + 3].fld_idx = FT_LOCK_DOOR;
					cave[y + 3][x + 3].feat = FEAT_CLOSED;
					cave[y + 3][x + 4].fld_idx = FT_LOCK_DOOR;
					cave[y + 3][x + 4].feat = FEAT_CLOSED;

					return;
				}
			}
		}
	}
}

/* Actually draw the city on cave[][] */
void draw_city(u16b town_num)
{
	int y, x, k, l;
	int count = 0;
	byte i, j;
	byte magic;
	u16b build;

	bool city_block;

	town_type *t_ptr = &town[town_num];

	/* Place transparent area */
	for (j = 0; j < MAX_HGT; j++)
	{
		for (i = 0; i < MAX_WID; i++)
		{
			/* Create empty area */
			cave[j][i].feat = FEAT_NONE;
			cave[j][i].fld_idx = 0;
		}
	}

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = town[town_num].seed;

	/* Get value of "magic" level of buildings */
	magic = (byte)randint0(256);

	/* Generate plasma factal */
	clear_temp_block();
	set_temp_corner_val(WILD_BLOCK_SIZE * 64);
	set_temp_mid((u16b)(WILD_BLOCK_SIZE * town[town_num].pop));
	frac_block();

	/* Find area outside the city */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			if (temp_block[j][i] < WILD_BLOCK_SIZE * 128)
			{
				/* Outside the city */
				temp_block[j][i] = 0;
			}
		}
	}

	/* Find walls */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Is a "city block" */
			if (temp_block[j][i])
			{
				/* Scan around */
				for (k = -1; k <= 1; k++)
				{
					for (l = -1; l <= 1; l++)
					{
						/* In bounds? */
						if ((i + k >= 0) && (i + k < WILD_BLOCK_SIZE) &&
							(j + l >= 0) && (j + l < WILD_BLOCK_SIZE))
						{
							/* Is it outside? */
							if (!temp_block[j + l][i + k])
							{
								/* Make a wall */
								temp_block[j][i] = 1;
							}
						}
						else
						{
							/* Make a wall */
							temp_block[j][i] = 1;
						}
					}
				}
			}
		}
	}

	/* 'Fill' the town with buildings */
	count = fill_town_driver();

	/* Rescan walls to avoid "islands" */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Is a "wall block" */
			if (temp_block[j][i] == 1)
			{
				city_block = FALSE;

				/* Scan around */
				for (k = -1; k <= 1; k++)
				{
					for (l = -1; l <= 1; l++)
					{
						/* In bounds? */
						if ((i + k >= 0) && (i + k < WILD_BLOCK_SIZE) &&
							(j + l >= 0) && (j + l < WILD_BLOCK_SIZE))
						{
							/* Is it a city block? */
							if (temp_block[j + l][i + k] == 2)
							{
								/* We are next to a city */
								city_block = TRUE;
							}
						}
					}
				}

				/* No islands */
				if (!city_block) temp_block[j][i] = 0;
			}
		}
	}

	/* Draw walls to cave[][] if visible */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Are we a wall? */
			if (temp_block[j][i] == 1)
			{
				y = j;
				x = i;

				/* Get coords of block */
				if (get_city_block_locat(&x, &y))
				{
					/* It is on the "screen" */

					/* Wall goes up */
					if ((j > 0) && (temp_block[j - 1][i] == 1))
					{
						generate_fill(y, x + 3, y + 4, x + 4,
							 FEAT_PERM_SOLID);
					}

					/* Wall goes left */
					if ((i > 0) && (temp_block[j][i - 1] == 1))
					{
						generate_fill(y + 3, x, y + 4, x + 4,
							 FEAT_PERM_SOLID);
					}

					/* Wall goes right */
					if ((i < WILD_BLOCK_SIZE-1) && (temp_block[j][i + 1] == 1))
					{
						generate_fill(y + 3, x + 3, y + 4, x + 7,
							FEAT_PERM_SOLID);
					}

					/* Wall goes down */
					if ((j < WILD_BLOCK_SIZE-1) && (temp_block[j + 1][i] == 1))
					{
						generate_fill(y + 3, x + 3, y + 7, x + 4,
							FEAT_PERM_SOLID);
					}

					/* Draw the gates */
					draw_gates(x, y, i, j, t_ptr);
				}
			}
		}
	}

	/* Scan blocks in a random order */
	for (build = 0; count; build++)
	{
		/* Pick a square */
		i = (byte)randint0(count);

		/* Draw the building */
		draw_building(0, build_x[i], build_y[i], build, town_num);

		/*
		 * Decrement free space in city
		 * Note deliberate use of count-- in initialiser
		 */
		for (count--; i < count; i++)
		{
			/* Shift unallocated buildings down */
			build_x[i] = build_x[i + 1];
			build_y[i] = build_y[i + 1];
		}
	}

	/* Hack -- use the "complex" RNG */
	Rand_quick = FALSE;
}


/*
 * Generate the selected town
 */
static void town_gen(u16b town_num)
{
	/* Mega - hack */
	if (town[town_num].type == 1)
	{
		van_town_gen(town_num);
	}
	else
	{
		draw_city(town_num);
	}
}


/*
 * Overlay the town block
 * If the town is not build correctly, build it
 */
static void overlay_town(int y, int x, u16b w_town, blk_ptr block_ptr)
{
	int		i, j, xx, yy;

	bool redraw = FALSE;

	cave_type	*c_ptr;

	/* Do we have the right town? */
	if (cur_town != w_town)
	{
		/* Reset */
		cur_town = w_town;
		town_offset_y = 0;

		redraw = TRUE;
	}

	/* Find block to copy */
	xx = (x - town[cur_town].x) * 16;
	yy = (y - town[cur_town].y - town_offset_y) * 16;

	if (yy < 0)
	{
		yy += 4 * 16;
		town_offset_y -= 4;
		redraw = TRUE;
	}

	if (yy >= 4 * 16)
	{
		yy -= 4 * 16;
		town_offset_y += 4;
		redraw = TRUE;
	}

	/* Redraw the town? */
	if (redraw)
	{
		/* Make the town */
		town_gen(w_town);
	}

	/* copy 16x16 block from cave[][] */
	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			c_ptr = &cave[yy + j][xx + i];

			if (c_ptr->feat != FEAT_NONE)
			{
				/* Only copy if there is something there. */
				block_ptr[j][i].feat = c_ptr->feat;
			}

			/*
			 * Instantiate field
			 *
			 * Note that most types of field are not in this list.
			 *
			 * Doors, buildings, traps, quests etc.
			 * are all that are in this list.
			 */
			if (!(c_ptr->fld_idx)) continue;

			switch (t_info[c_ptr->fld_idx].type)
			{
				/* Nothing */
				case FTYPE_TRAP:
				{
					/* Activate the trap */
					if (place_field(y * 16 + j, x * 16 + i, c_ptr->fld_idx))
					{
						/* Hack - Initialise it (without "extra" information) */
						(void)field_hook_single(&block_ptr[j][i].fld_idx,
							 FIELD_ACT_INIT, NULL);
					}

					break;
				}

				case FTYPE_DOOR:
				{
					int data = 9;

					/* Add a door field */
					if (place_field(y * 16 + j, x * 16 + i, c_ptr->fld_idx))
					{
						/* Add "power" of lock / jam to the field */
						(void)field_hook_single(&block_ptr[j][i].fld_idx,
							 FIELD_ACT_INIT, &data);
					}

					break;
				}

				case FTYPE_BUILD:
				{
					/* Stores + buildings */
					(void) place_field(y * 16 + j, x * 16 + i, c_ptr->fld_idx);

					break;
				}
			}
		}
	}
}





/*
 * This function _must_ be called whenever the dungeon level changes.
 * It makes sure the bounds and access functions point to the correct
 * functions.  If this is not done - bad things happen.
 */

void change_level(int level)
{
	int x, y;

	/* Hack - reset trap detection flag */
	p_ptr->detected = FALSE;

	/* Clear the monster lights */
	clear_mon_lite();

	if (level == 0)
	{
		/* In the wilderness */

		/* Reset the bounds */
		min_hgt = wild_grid.y_min;
		max_hgt = wild_grid.y_max;
		min_wid = wild_grid.x_min;
		max_wid = wild_grid.x_max;

		/* Access the wilderness */
		area = access_wild;

		if (p_ptr->depth == 0)
		{
			/* Lighten / darken wilderness */
			day_night();
		}

		/* Only do this if everything is initialised */
		if (!character_dungeon) return;

		/*
		 * Restore the outside town if it exists
		 * This is mainly done to reinit the fields
		 */
		for (y = wild_grid.y; y < WILD_GRID_SIZE + wild_grid.y; y++)
		{
			for (x = wild_grid.x; x < WILD_GRID_SIZE + wild_grid.x; x++)
			{
				/* The block to use */
				blk_ptr block_ptr =
					 wild_grid.block_ptr[y - wild_grid.y][x - wild_grid.x];

				/* Overlay town */
				u16b w_town = wild[y][x].done.town;

				/* Is there a town? */
				if (w_town)
				{
					/* overlay town on wilderness */
					overlay_town(y, x, w_town, block_ptr);
				}
			}
		}
	}
	else
	{
		/* In the dungeon */

		/* Reset the bounds */
		min_hgt = 0;
		max_hgt = MAX_HGT;
		min_wid = 0;
		max_wid = MAX_WID;

		/* Access the cave */
		area = access_cave;

		/* No town stored in cave[][] */
		cur_town = 0;
	}
}

/* Delete a wilderness block */
static void del_block(blk_ptr block_ptr)
{
	int x, y;
	int m_idx;

	for (x = 0; x < WILD_BLOCK_SIZE; x++)
	{
		for (y = 0; y < WILD_BLOCK_SIZE; y++)
		{
			/* Clear old terrain data */
			block_ptr[y][x].info = 0;
			block_ptr[y][x].feat = 0;

			/* Delete monster on the square */
			m_idx = block_ptr[y][x].m_idx;

			/* Only delete if one exists */
			if (m_idx)
			{
				delete_monster_idx(m_idx);
				block_ptr[y][x].m_idx = 0;
			}

			/* Delete objects on the square */
			delete_object_location(&block_ptr[y][x]);

			/* Delete fields on the square */
			delete_field_aux(&block_ptr[y][x].fld_idx);
		}
	}
}


/* Clear the temporary block */
void clear_temp_block(void)
{
	int i, j;

	/* Clear the section */
	for (i = 0; i <= WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j <= WILD_BLOCK_SIZE; j++)
		{
			/* MAX_SHORT is a flag for "not done yet" */
			temp_block[j][i] = MAX_SHORT;
		}
	}
}


/* Set the corners of the temporary block to val */
void set_temp_corner_val(u16b val)
{
	temp_block[0][0] = val;
	temp_block[0][WILD_BLOCK_SIZE] = val;
	temp_block[WILD_BLOCK_SIZE][0] = val;
	temp_block[WILD_BLOCK_SIZE][WILD_BLOCK_SIZE] = val;
}


/* Set the middle of the temporary block to val */
void set_temp_mid(u16b val)
{
	temp_block[WILD_BLOCK_SIZE / 2][WILD_BLOCK_SIZE / 2] = val;
}


/*
 * Initialise the temporary block based on the value of a wilderness
 * 'info' flag for gradient information.
 *
 * This is used for rivers, beaches, lakes etc.
 */
static bool wild_info_bounds(int x, int y, byte info)
{
	int i, x1, y1;
	bool grad1[10], grad2[10], any;

	/* No flags set yet */
	any = FALSE;

	/* If center is set, then whole square is "on" */
	if (wild[y][x].done.info & info)
	{
		/* Set all flags */
		grad1[5] = TRUE;

		/* A flag is set */
		any = TRUE;
	}
	else
	{
		/* Check each adjacent square to see if flag is set */
		for (i = 1; i < 10; i++)
		{
			/* Get direction */
			x1 = x + ddx[i];
			y1 = y + ddy[i];

			grad1[i] = FALSE;

			/* Check bounds */
			if ((x1 >= 0) && (x1 < max_wild) && (y1 >= 0) && (y1 < max_wild))
			{
				/* Check flag status */
				if (wild[y1][x1].done.info & info)
				{
					/* Flag is set */
					grad1[i] = TRUE;
					any = TRUE;
				}
			}
		}
	}

	/* Exit if there are no set flags */
	if (any == FALSE) return (FALSE);

	/* Clear temporary block */
	clear_temp_block();

	/* Set grad2[] depending on values of grad1[] */

	/* If center is set - all are set */
	if (grad1[5])
	{
		for (i = 1; i < 10; i++)
		{
			grad2[i] = TRUE;
		}
	}
	else
	{
		/* Clear grad2[] */
		for (i = 1; i < 10; i++)
		{
			grad2[i] = FALSE;
		}

		/* Copy orthogonal flags */
		for (i = 1; i < 5; i++)
		{
			grad2[i * 2] = grad1[i * 2];
		}

		/* Set diagonally adjacent flags depending on values of orthogonal flags. */

		/* Upper left */
		if (grad1[4] || grad1[8])
		{
			grad2[7] = TRUE;
		}

		/* Upper right */
		if (grad1[8] || grad1[6])
		{
			grad2[9] = TRUE;
		}

		/* Lower right */
		if (grad1[6] || grad1[2])
		{
			grad2[3] = TRUE;
		}

		/* Lower left */
		if (grad1[2] || grad1[4])
		{
			grad2[1] = TRUE;
		}
	}

	/* If a flag is set - make that side maximum */
	for (i = 1; i < 10; i++)
	{
		/* Hack - get only orthogonal directions */
		x1 = (1 + ddx[i]) * WILD_BLOCK_SIZE / 2;
		y1 = (1 + ddy[i]) * WILD_BLOCK_SIZE / 2;

		if (grad2[i])
		{
			temp_block[y1][x1] = WILD_BLOCK_SIZE * 256;
		}
		else
		{
			temp_block[y1][x1] = WILD_BLOCK_SIZE * 64;
		}
	}

	/* There are flags set */
	return (TRUE);
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
 */
void frac_block(void)
{
	u16b lstep, hstep, i, j, size;

	/*
	 * Size is one bigger than normal blocks for speed
	 * of algorithm with 2^n + 1
	 */
	size = WILD_BLOCK_SIZE;

	/* Initialize the step sizes */
	lstep = hstep = size;

	/*
	 * Fill in the square with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while (hstep > 1)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* only write to points that are "blank" */
				if (temp_block[j][i] == MAX_SHORT)
				{
					/* Average of left and right points +random bit */
					temp_block[j][i] = (((temp_block[j][i - hstep] +
					         temp_block[j][i + hstep]) +
					         (randint1(lstep << 8) - (hstep << 8))) >> 1);
				}
			}
		}

		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
			{
				/* only write to points that are "blank" */
				if (temp_block[j][i] == MAX_SHORT)
				{
					/* Average of up and down points +random bit */
					temp_block[j][i] = (((temp_block[j - hstep][i] +
					          temp_block[j + hstep][i]) +
					          (randint1(lstep << 8) - (hstep << 8))) >> 1);
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
				/* only write to points that are "blank" */
				if (temp_block[j][i] == MAX_SHORT)
				{
					/*
					 * Average over all four corners + scale by 181 to
					 * reduce the effect of the square grid on the
					 * shape of the fractal
					 */
					temp_block[j][i] = ((temp_block[j - hstep][i - hstep] +
					       temp_block[j + hstep][i - hstep] +
					       temp_block[j - hstep][i + hstep] +
					       temp_block[j + hstep][i + hstep]) >> 2) +
					       (((randint1(lstep << 8) - (hstep << 8)) * 181) >> 8);
				}
			}
		}
	}
}


/*
 * This function smoothly interpolates between
 * the points on the grid.  (As opposed to frac_block()
 * which adds random offsets to make a rough
 * pattern.
 */
static void smooth_block(void)
{
	u16b lstep, hstep, i, j, size;

	/*
	 * Size is one bigger than normal blocks for speed
	 * of algorithm with 2^n + 1
	 */
	size = WILD_BLOCK_SIZE;

	/* Initialize the step sizes */
	lstep = hstep = size;

	while (hstep > 1)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* only write to points that are "blank" */
				if (temp_block[j][i] == MAX_SHORT)
				{
					/* Average of left and right points */
					temp_block[j][i] = ((temp_block[j][i - hstep] +
					                     temp_block[j][i + hstep]) >> 1);
				}
			}
		}


		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
			{
				/* only write to points that are "blank" */
				if (temp_block[j][i] == MAX_SHORT)
				{
					/* Average of up and down points */
					temp_block[j][i] = ((temp_block[j - hstep][i] +
					                     temp_block[j + hstep][i]) >> 1);
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
				/* only write to points that are "blank" */
				if (temp_block[j][i] == MAX_SHORT)
				{
					/* Average of corner points */
					temp_block[j][i] = ((temp_block[j - hstep][i - hstep] +
					                     temp_block[j + hstep][i - hstep] +
					                     temp_block[j - hstep][i + hstep] +
					                     temp_block[j + hstep][i + hstep]) >> 2);
				}
			}
		}
	}
}


/*
 * This function picks a terrain feature from a list of four
 * based on a "probability factor".  The further 'prob' is
 * from 'prob1' etc. the less likely that feature is.
 * This weights the distribution.
 *
 * As a special case, feature 0 is defined to be "nonexistant"
 * so that choices can be made with less than 4 features.
 */
static byte pick_feat(byte feat1, byte feat2, byte feat3, byte feat4,
                      byte prob1, byte prob2, byte prob3, byte prob4,
                      byte prob)
{
	/* Chance factors */
	u32b c1, c2, c3, c4, choice;

	/* Zero the chance factors */
	c1 = c2 = c3 = c4 = 0;


	/* Calculate chance factors if feature != 0 */
	if (feat1)
	{
		if (prob1 == prob)
		{
			c1 = 0x1000000;
		}
		else
		{
			c1 = 0x1000000 / ABS((long)prob1 - prob);
		}
	}
	if (feat2)
	{
		if (prob2 == prob)
		{
			c2 = 0x1000000;
		}
		else
		{
			c2 = 0x1000000 / ABS((long) prob2 - prob);
		}
	}
	if (feat3)
	{
		if (prob3 == prob)
		{
			c3 = 0x1000000;
		}
		else
		{
			c3 = 0x1000000 / ABS((long) prob3 - prob);
		}
	}

	if (feat4)
	{
		if (prob4 == prob)
		{
			c4 = 0x1000000;
		}
		else
		{
			c4 = 0x1000000 / ABS((long) prob4 - prob);
		}
	}

	/* get choice */
	choice = Rand_div(c1 + c2 + c3 + c4);

	/* Return terrain feature based on weighted chance */
	if (choice < c1) return (feat1);

	choice -= c1;
	if (choice < c2) return (feat2);

	choice -= c2;
	if (choice < c3) return (feat3);

	return (feat4);
}


/*
 * This function creates the sea based on the number in sea_type.
 * The higher the number - the greater the chance of deeper water.
 *
 * Note WILD_SEA and above generation types are reserved for use
 * with this function.
 */
static void make_wild_sea(blk_ptr block_ptr, byte sea_type)
{
	int i, j;

	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			block_ptr[j][i].feat = pick_feat(FEAT_SHAL_WATER, FEAT_DEEP_WATER,
					FEAT_OCEAN_WATER, FEAT_NONE, 0, 10, 20, 40, sea_type);
			block_ptr[j][i].info = 0;
		}
	}
}


/*
 * Build a road or a track at this location
 *
 * This function counts the number of other road / tracks
 * adjacent to this square, and uses that information to
 * build a plasma fractal.  The fractal then is used to
 * make a road.
 */
static void make_wild_road(blk_ptr block_ptr, int x, int y)
{
	int i, j, x1, y1;
	u16b grad1[10], grad2[10], any;

	cave_type *c_ptr;

	/* Only draw if road is on the square */
	if (!(wild[y][x].done.info & (WILD_INFO_TRACK | WILD_INFO_ROAD)))
	{
		/* No flags set yet */
		any = FALSE;

		/* Only do the sides */
		for (i = 2; i < 10; i += 2)
		{
			/* Get direction */
			x1 = x + ddx[i];
			y1 = y + ddy[i];

			grad1[i] = 0;

			/* Check bounds */
			if ((x1 >= 0) && (x1 < max_wild) && (y1 >= 0) && (y1 < max_wild))
			{
				/* Check flag status */
				if (wild[y1][x1].done.info & WILD_INFO_TRACK)
				{
					/* Flag is set */
					grad1[i] = TRACK_LEVEL;
					any = TRUE;
				}

				if (wild[y1][x1].done.info & WILD_INFO_ROAD)
				{
					/* Flag is set */
					grad1[i] = ROAD_LEVEL;
					any = TRUE;
				}
			}
		}

		/* No nearby roads */
		if (!any) return;

		/* Convert from grad1 to grad2 */
		for (i = 1; i < 10; i++)
		{
			grad2[i] = GROUND_LEVEL;
		}

		/* Upper left */
		if (grad1[4] && grad1[8])
		{
			grad2[7] = MAX(grad1[4], grad1[8]);

			any = FALSE;
		}

		/* Upper right */
		if (grad1[8] && grad1[6])
		{
			grad2[9] = MAX(grad1[8], grad1[6]);

			any = FALSE;
		}

		/* Lower right */
		if (grad1[6] && grad1[2])
		{
			grad2[3] = MAX(grad1[6], grad1[2]);

			any = FALSE;
		}

		/* Lower left */
		if (grad1[2] && grad1[4])
		{
			grad2[1] = MAX(grad1[2], grad1[4]);

			any = FALSE;
		}

		/* Hack - only if there really is a road */
		if (any) return;
	}
	else
	{
		/* Do everything */

		/* Check each adjacent square to see if is road or track */
		for (i = 1; i < 10; i++)
		{
			/* Get direction */
			x1 = x + ddx[i];
			y1 = y + ddy[i];

			grad2[i] = GROUND_LEVEL;

			/* Check bounds */
			if ((x1 >= 0) && (x1 < max_wild) && (y1 >= 0) && (y1 < max_wild))
			{
				/* Check flag status */
				if (wild[y1][x1].done.info & WILD_INFO_TRACK)
				{
					/* Flag is set */
					grad2[i] = TRACK_LEVEL;
				}

				if (wild[y1][x1].done.info & WILD_INFO_ROAD)
				{
					/* Flag is set */
					grad2[i] = ROAD_LEVEL;
				}
			}
		}
	}

	/* Clear temporary block */
	clear_temp_block();

	/* Set sides of block */
	for (i = 1; i < 10; i++)
	{
		x1 = (1 + ddx[i]) * WILD_BLOCK_SIZE / 2;
		y1 = (1 + ddy[i]) * WILD_BLOCK_SIZE / 2;

		temp_block[y1][x1] = grad2[i];
	}

	/* Build the road "density map" */
	smooth_block();

	/* Copy the result over the block */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Is it a road square? */
			if (temp_block[j][i] >= ROAD_BORDER)
			{
				/* Point to square */
				c_ptr = &block_ptr[j][i];

				if ((c_ptr->feat == FEAT_SHAL_WATER) ||
					(c_ptr->feat == FEAT_DEEP_WATER))
				{
					c_ptr->feat = FEAT_PEBBLES;
				}
				else if (c_ptr->feat == FEAT_OCEAN_WATER)
				{
					c_ptr->feat = FEAT_SHAL_WATER;
				}
				else
				{
					if (one_in_(3))
					{
						c_ptr->feat = FEAT_PEBBLES;
					}
					else
					{
						c_ptr->feat = FEAT_DIRT;
					}
				}
			}
		}
	}
}

/*
 * Using gradient information given in temp_block,
 * overlay on top of wilderness block.
 *
 * This is used to make rivers, beaches etc.
 */
static void wild_add_gradient(blk_ptr block_ptr, byte feat1, byte feat2)
{
	int i, j;

	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			if (temp_block[j][i] >= WILD_BLOCK_SIZE * 213)
			{
				/* 25% of the time use the other tile : it looks better this way */
				if (one_in_(4))
				{
					block_ptr[j][i].feat = feat1;
				}
				else
				{
					block_ptr[j][i].feat = feat2;
				}
			}
			else if (temp_block[j][i] >= WILD_BLOCK_SIZE * 128)
			{
				/* 25% of the time use the other tile : it looks better this way */
				if (one_in_(4))
				{
					block_ptr[j][i].feat = feat2;
				}
				else
				{
					block_ptr[j][i].feat = feat1;
				}
			}
		}
	}
}



/*
 * Make wilderness generation type 1
 *
 * Make a plasma fractal.  Convert the heightmap to terrain
 * via the pick_feat function.
 * This routine uses all data fields.
 * Odd fields in the data[] array are the terrain features.
 * The even fields are the region of the hieght-map where
 * those features are most common.
 */
static void make_wild_01(blk_ptr block_ptr, byte *data)
{
	int i, j;
	byte new_feat, element;


	/* Initialise temporary block */
	clear_temp_block();
	set_temp_corner_val(WILD_BLOCK_SIZE * 128);
	set_temp_mid(WILD_BLOCK_SIZE * 128);

	/* Generate plasma factal */
	frac_block();

	/* Make terrain block based on height map */
	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			/* Get value */
			element = temp_block[j][i] / WILD_BLOCK_SIZE;

			/* Work out terrain feature to use */
			new_feat = pick_feat(data[0], data[2], data[4], data[6],
				data[1], data[3], data[5], data[7], element);

			block_ptr[j][i].feat = new_feat;
			block_ptr[j][i].info = 0;
		}
	}
}


/*
 * Make wilderness generation type 2
 *
 * Make a uniform field from the feature in data[0]
 * Next, add the lower probability features in data[2], [4] etc.
 * using the probabilities in data[1], [3] etc.
 * Use feat = 0 to mark the end of the list of features.
 *
 * This uses a different probability function than type 1.
 * (It is cumulative.)
 *
 * This is good for making "flat density" regions like grasslands etc.
 */
static void make_wild_02(blk_ptr block_ptr, byte *data)
{
	int i, j, k;
	byte new_feat, feat, chance;

	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Init. counter */
			k = 0;

			/* Hack - if first feature is zero - use grass */
			feat = FEAT_GRASS;

			while (1)
			{
				/* Get feature */
				new_feat = data[k * 2];

				/* End of list? */
				if (new_feat == 0) break;

				/* Use new feature */
				feat = new_feat;

				/* Done counting? */
				if (k == 3) break;

				chance = data[k * 2 + 1];

				/* Exit if chance is zero */
				if (!chance) break;

				/* Stop if chance fails */
				if (randint0(chance + 1)) break;

				/* Increment counter + loop */
				k++;
			}

			/* Store feature in block */
			block_ptr[j][i].feat = feat;
		}
	}
}


/*
 * This function makes a wilderness type specifed by data[0].
 * It then overlays a "circle" of other terrain on top.
 * data[1], [2] and [3] specify these.
 * Note - this function is a major hack.  It uses the _number_
 * of another wilderness type - which in turn has its own data[]
 * and generation type.
 * It is possible to use recursion to make some interesting effects.
 * These include:  Tiny lakes of water, lava, acid.  Craters.  Rock pillars.
 *   Bogs.  Clumps of trees. etc.
 */
static void make_wild_03(blk_ptr block_ptr, byte *data)
{
	int i, j, element;

	/* Call the other routine to make the "base" terrain. */
	gen_block_helper(block_ptr, wild_gen_data[data[0]].data,
	                 wild_gen_data[data[0]].gen_routine);

	/* Initialise temporary block */
	clear_temp_block();

	/* Large in center - small on sides */
	set_temp_corner_val(WILD_BLOCK_SIZE * 64);
	set_temp_mid(WILD_BLOCK_SIZE * 256);

	/* Generate plasma factal */
	frac_block();

	/* Overlay the "circle" of terrain */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			element = temp_block[j][i];

			/* Outside circle? */
			if (element < WILD_BLOCK_SIZE * 128) continue;

			if ((element < WILD_BLOCK_SIZE * 171) && one_in_(2))
			{
				/* Outermost terrain */
				block_ptr[j][i].feat = data[1];
				continue;
			}

			if ((element < WILD_BLOCK_SIZE * 213) && one_in_(2))
			{
				/* Middle terrain */
				block_ptr[j][i].feat = data[2];
				continue;
			}

			/* Inner terrain */
			block_ptr[j][i].feat = data[3];
		}
	}
}

/*
 * This function blends adjacent sea blocks
 * (by picking the feat type to use)
 */
static void blend_sea(cave_type *c_ptr, byte sea_type)
{
	c_ptr->feat = pick_feat(FEAT_SHAL_WATER, FEAT_DEEP_WATER,
					FEAT_OCEAN_WATER, FEAT_NONE, 0, 10, 20, 40, sea_type);
}


/*
 * The function that picks a "blending feature" for wild. gen. type 1
 */
static void blend_wild_01(cave_type *c_ptr, byte *data)
{
	/* Store an "average" terrain feature */
	c_ptr->feat = pick_feat(data[0], data[2], data[4], data[6],
	                        data[1], data[3], data[5], data[7], 128);
}


/*
 * The function that picks a "blending feature" for wild. gen. type 2
 */
static void blend_wild_02(cave_type *c_ptr, byte *data)
{
	/* Store the most likely terrain feature */
	c_ptr->feat = data[0];
}


void blend_helper(cave_type *c_ptr, byte *data, int g_type)
{
	/* Based on type - choose wilderness block generation function */
	switch (g_type)
	{
		case 1:
			/* Fractal plasma with weighted terrain probabilites */
			blend_wild_01(c_ptr, data);
			break;

		case 2:
			/* Simple weighted probabilities on flat distribution */
			blend_wild_02(c_ptr, data);
			break;

		case 3:
			/* Use the other terrain's blend function */
			blend_helper(c_ptr, wild_gen_data[data[0]].data,
			             wild_gen_data[data[0]].gen_routine);
			break;

		default:
		msg_format("Illegal wilderness type %d ", g_type);
	}
}


/*
 * Blend a block based on the adjacent blocks
 * This makes the wilderness look much better.
 */
static void blend_block(int x, int y, blk_ptr block_ptr, u16b type)
{
	int i, j, dx, dy;

	u16b w_type;

	/* Blend based on height map */
	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			/* Chance to blend is 1 in 2 */
			if (quick_rand()) continue;

			/* Work out adjacent block */
			if (i < WILD_BLOCK_SIZE / 4)
			{
				dx = -1;
			}
			else if (i > (WILD_BLOCK_SIZE * 3) / 4)
			{
				dx = +1;
			}
			else
			{
				dx = 0;
			}

			if (j < WILD_BLOCK_SIZE / 4)
			{
				dy = -1;
			}
			else if (j > (WILD_BLOCK_SIZE * 3) / 4)
			{
				dy = +1;
			}
			else
			{
				dy = 0;
			}

			/* Check to see if adjacent square is not in bounds */
			if (((y + dy) < 0) || ((y + dy) >= max_wild) ||
			    ((x + dx) < 0) || ((x + dx) >= max_wild)) continue;

			/* Don't blend with yourself */
			if ((dx == 0) && (dy == 0)) continue;

			w_type = wild[y + dy][x + dx].done.wild;

			/* If adjacent type is the same as this one - don't blend */
			if (w_type == type) continue;

			/* The sea doesn't blend. (Use rivers) */
			if (w_type >= WILD_SEA)
			{
				if (type >= WILD_SEA)
				{
					blend_sea(&block_ptr[j][i], (byte)(w_type - WILD_SEA));
				}
				else
				{
					/* Do not try to blend sea with land */
					/* We need to fix the blocky look of oceans though */
					continue;
				}
			}

			/* Blend with generation type specified by gen_routine */
			blend_helper(&block_ptr[j][i], wild_gen_data[w_type].data,
			             wild_gen_data[w_type].gen_routine);
		}
	}
}


/*
 * Make the specified terrain type at a wilderness block
 */
void gen_block_helper(blk_ptr block_ptr, byte *data, int gen_type)
{
	/* Based on type - choose wilderness block generation function */
	switch (gen_type)
	{
		case 1:
			/* Fractal plasma with weighted terrain probabilites */
			make_wild_01(block_ptr, data);
			break;

		case 2:
			/* Uniform field + rare "out-crops" */
			make_wild_02(block_ptr, data);
			break;

		case 3:
			/* Use another type + overlay a "circle" of terrain. */
			make_wild_03(block_ptr, data);
			break;

		default:
			quit("Illegal wilderness block type.");
	}
}


/*
 * Fill the block with perm. walls. This is only used by the vanilla town option.
 */
static void fill_perm_wall(blk_ptr block_ptr)
{
	int i, j;

	/* Overlay the block with permament walls */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			block_ptr[j][i].feat = FEAT_PERM_OUTER;
		}
	}
}


/* Add monsters to the wilderness block */
static void add_monsters_block(int x, int y)
{
	int i, j, xx, yy;
	long prob;

	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
	{
		/* Monsters are rarer in the day */
		prob = 32786;
	}
	else
	{
		/* Monsters are more common at night */
		prob = 20000;
	}

	/*
	 * Probability of a monster being on a certain sqaure.
	 * Perhaps this should include the effects of stealth.
	 */
	prob /= (wild[y][x].done.mon_prob + 1);

	xx = x * 16;
	yy = y * 16;

	for (i = 0; i < 16; i++)
	{
		for (j = 0; j < 16; j++)
		{
			/* See if monster should go on square */
			if (!randint0(prob))
			{
				if (one_in_(2))
				{
					/* Monsters are awake */
					(void)place_monster(yy + j, xx + i, FALSE, TRUE);
				}
				else
				{
					/* Monsters are asleep */
					(void)place_monster(yy + j, xx + i, TRUE, TRUE);
				}
			}
		}
	}
}


/*
 * Add monsters to the boundary of the visible grid.
 */
static void add_monsters(void)
{
	int x, y;

	/* Add monsters */
	for (x = 0; x < WILD_GRID_SIZE; x++)
	{
		for (y = 0; y < WILD_GRID_SIZE; y++)
		{
			/* Only on bounding blocks */
			if (!((x == 0) || (x == WILD_GRID_SIZE - 1))) continue;
			if (!((y == 0) || (y == WILD_GRID_SIZE - 1))) continue;

			/* Not too close to player */
			if (distance(p_ptr->px / 16, p_ptr->py / 16,
			             x + wild_grid.x, y + wild_grid.y) < 3) continue;

			/* Set the monster generation level */

			/* Hack - use a number based on "law" statistic */
			monster_level = wild[y + wild_grid.y][x + wild_grid.x].done.mon_gen;

			/* Add monsters to block */
			add_monsters_block(x + wild_grid.x, y + wild_grid.y);
		}
	}
}


/*
 * Add monsters to the wilderness in all blocks
 */
void repopulate_wilderness(void)
{
	int x, y;

	/* Add monsters */
	for (x = 0; x < WILD_GRID_SIZE; x++)
	{
		for (y = 0; y < WILD_GRID_SIZE; y++)
		{
			/* Set the monster generation level */

			/* Hack - use a number based on "law" statistic */
			monster_level = wild[y + wild_grid.y][x + wild_grid.x].done.mon_gen;

			/* Add monsters to block */
			add_monsters_block(x + wild_grid.x, y + wild_grid.y);
		}
	}
}


/*
 * Make a new block based on the terrain type
 */
static void gen_block(int x, int y, blk_ptr block_ptr)
{
	u16b w_town, w_type;

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant wilderness blocks */
	Rand_value = wild_grid.wild_seed + x + y * max_wild;

	/* Generate a terrain block */

	/* Get wilderness type */
	w_type = wild[y][x].done.wild;

	/* Create sea terrains if type >= WILD_SEA */
	if (w_type >= WILD_SEA)
	{
		make_wild_sea(block_ptr, (byte)(w_type - WILD_SEA));
	}

	/* Hack -Check for the vanilla town wall option. */
	else if (w_type == 0)
	{
		/* Fill the block with permanent walls */
		fill_perm_wall(block_ptr);
	}
	else
	{
		/* Make terrain based on wilderness generation type */
		gen_block_helper(block_ptr, wild_gen_data[w_type].data,
			wild_gen_data[w_type].gen_routine);

		/* Blend with adjacent terrains */
		blend_block(x, y, block_ptr, w_type);

		/* Add water boundary effects. (Rivers / Ocean) */
		if (wild_info_bounds(x, y, WILD_INFO_WATER))
		{
			/* Hack, above function sets bounds */

			/* Generate plasma factal */
			frac_block();

			/* Overlay water */
			wild_add_gradient(block_ptr, FEAT_SHAL_WATER, FEAT_DEEP_WATER);
		}

		/* Add lava boundary effects. */
		if (wild_info_bounds(x, y, WILD_INFO_LAVA))
		{
			/* Hack, above function sets bounds */

			/* Generate plasma factal */
			frac_block();

			/* Overlay lava */
			wild_add_gradient(block_ptr, FEAT_SHAL_LAVA, FEAT_DEEP_LAVA);
		}

		/* Add acid boundary effects.*/
		if (wild_info_bounds(x, y, WILD_INFO_ACID))
		{
			/* Hack, above function sets bounds */

			/* Generate plasma factal */
			frac_block();

			/* Overlay acid */
			wild_add_gradient(block_ptr, FEAT_SHAL_ACID, FEAT_DEEP_ACID);
		}

		/* Add roads */
		make_wild_road(block_ptr, x, y);
	}

	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	/* Overlay town */
	w_town = wild[y][x].done.town;

	/* Is there a town? */
	if (w_town)
	{
		/* overlay town on wilderness */
		overlay_town(y, x, w_town, block_ptr);
	}

	/* Day / Night - lighten or darken the new block */
	light_dark_block(block_ptr, x, y);

	/* Set the object generation level */

	/* Hack - set object level to monster level */
	object_level = wild[y][x].done.mon_gen;

	/* Add monsters. (Not done) */


#ifdef USE_SCRIPT
	if (generate_wilderness_callback(y, x)) return;
#endif /* USE_SCRIPT */
}


/* Allocate all grids around player */
void init_wild_cache(void)
{
	int x, y;

	/* Allocate blocks around player */
	for (x = 0; x < WILD_GRID_SIZE; x++)
	{
		for (y = 0; y < WILD_GRID_SIZE; y++)
		{
			/* Link to the grid */
			wild_grid.block_ptr[y][x] = wild_cache[x + WILD_GRID_SIZE * y];
		}
	}
}


/*
 * Allocate all grids around player
 */
static void allocate_all(void)
{
	u16b x, y;
	blk_ptr block_ptr;

	/* Allocate blocks around player */
	for (y = 0; y < WILD_GRID_SIZE; y++)
	{
		for (x = 0; x < WILD_GRID_SIZE; x++)
		{
			/* The block to use */
			block_ptr = wild_cache[x + WILD_GRID_SIZE * y];

			/* Delete the block */
			del_block(block_ptr);

			/* Link to the grid */
			wild_grid.block_ptr[y][x] = block_ptr;

			/* Make the new block */
			gen_block(x + wild_grid.x, y + wild_grid.y, block_ptr);
		}
	}

	/* Add monsters */
	add_monsters();
}


/*
 * The following four functions shift the visible
 * section of the wilderness by 16 units. This is
 * done by scrolling the grid of pointers.
 */
static void shift_down(void)
{
	u16b i, j;
	blk_ptr block_ptr;

	for (i = 0; i < WILD_GRID_SIZE; i++)
	{
		/* The block on the edge */
		block_ptr = wild_grid.block_ptr[0][i];

		/* Delete the block */
		del_block(block_ptr);

		/* Scroll pointers */
		for (j = 1; j < WILD_GRID_SIZE; j++)
		{
			wild_grid.block_ptr[j - 1][i] = wild_grid.block_ptr[j][i];
		}

		/* Connect new grid to wilderness */
		wild_grid.block_ptr[WILD_GRID_SIZE - 1][i] = block_ptr;

		/* Make the new block */
		gen_block(i + wild_grid.x, WILD_GRID_SIZE - 1 + wild_grid.y,
		          block_ptr);
	}

	/* Add monsters */
	add_monsters();
}


static void shift_up(void)
{
	u16b i, j;
	blk_ptr block_ptr;

	for (i = 0; i < WILD_GRID_SIZE; i++)
	{
		/* The block on the edge */
		block_ptr = wild_grid.block_ptr[WILD_GRID_SIZE - 1][i];

		/* Delete the block */
		del_block(block_ptr);

		/* Scroll pointers */
		for (j = WILD_GRID_SIZE - 1; j > 0; j--)
		{
			wild_grid.block_ptr[j][i] = wild_grid.block_ptr[j - 1][i];
		}

		/* Connect new grid to wilderness */
		wild_grid.block_ptr[0][i] = block_ptr;

		/* Make the new block */
		gen_block(i + wild_grid.x, wild_grid.y, block_ptr);
	}

	/* Add monsters */
	add_monsters();
}


static void shift_right(void)
{
	u16b i, j;
	blk_ptr block_ptr;

	for (j = 0; j < WILD_GRID_SIZE; j++)
	{
		/* The block on the edge */
		block_ptr = wild_grid.block_ptr[j][0];

		/* Delete the block */
		del_block(block_ptr);

		/* Scroll pointers */
		for (i = 1; i < WILD_GRID_SIZE; i++)
		{
			wild_grid.block_ptr[j][i - 1] = wild_grid.block_ptr[j][i];
		}

		/* Connect new grid to wilderness */
		wild_grid.block_ptr[j][WILD_GRID_SIZE - 1] = block_ptr;

		/* Make the new block */
		gen_block(WILD_GRID_SIZE - 1 + wild_grid.x, j + wild_grid.y,
		          block_ptr);
	}

	/* Add monsters */
	add_monsters();
}


static void shift_left(void)
{
	u16b i, j;
	blk_ptr block_ptr;

	for (j = 0; j < WILD_GRID_SIZE; j++)
	{
		/* The block on the edge */
		block_ptr = wild_grid.block_ptr[j][WILD_GRID_SIZE - 1];

		/* Delete the block */
		del_block(block_ptr);

		/* Scroll pointers */
		for (i = WILD_GRID_SIZE - 1; i > 0; i--)
		{
			wild_grid.block_ptr[j][i] = wild_grid.block_ptr[j][i - 1];
		}

		/* Connect new grid to wilderness */
		wild_grid.block_ptr[j][0] = block_ptr;

		/* Make the new block */
		gen_block(wild_grid.x, j + wild_grid.y, block_ptr);
	}

	/* Add monsters */
	add_monsters();
}


/*
 * Centre grid of wilderness blocks around player.
 * This must be called after the player moves in the wilderness.
 * If the player is just walking around, all that needs to be done is
 * to scroll the grid of pointers - not recalculate them all.
 * However, when the player teleports, all have to ba allocated.
 */
void move_wild(void)
{
	int x, y, dx, dy;

	/* Get upper left hand block in grid. */

	/* Divide by 16 to get block from (x,y) coord */
	x = ((u16b)p_ptr->wilderness_x>>4);
	y = ((u16b)p_ptr->wilderness_y>>4);

	/* The player sees the wilderness block he is on. */
	wild[y][x].done.info |= WILD_INFO_SEEN;

	/* Recenter map */
	x -= WILD_GRID_SIZE / 2;
	y -= WILD_GRID_SIZE / 2;

	/* Move if out of bounds */
	if (x < 0) x = 0;
	if (y < 0) y = 0;
	if (x + WILD_GRID_SIZE > max_wild) x = max_wild - WILD_GRID_SIZE;
	if (y + WILD_GRID_SIZE > max_wild) y = max_wild - WILD_GRID_SIZE;

	/* Hack - set town */
	p_ptr->town_num =
	    wild[p_ptr->wilderness_y >> 4][p_ptr->wilderness_x >> 4].done.town;

	/*
	 * Hack - check to see if first block is the same.
	 * If so, the grid doesn't need to move.
	 */
	if ((x == wild_grid.x) && (y == wild_grid.y)) return;

	dx = x - wild_grid.x;
	dy = y - wild_grid.y;

	/* Store in upper left hand corner. */
	wild_grid.y = y;

	/* Recalculate boundaries */
	wild_grid.y_max = (y + WILD_GRID_SIZE) << 4;
	wild_grid.y_min = y << 4;

	max_hgt = wild_grid.y_max;
	min_hgt = wild_grid.y_min;

	/* Shift in only a small discrepency */
	if (ABS(dy) == 1)
	{
		if (dy == 1) shift_down();
		else shift_up();
	}
	else if (dy)
	{
		/* Too large of a shift */

		/* Store in upper left hand corner. */
		wild_grid.x = x;

		/* Recalculate boundaries */
		wild_grid.x_max = (x + WILD_GRID_SIZE) << 4;
		wild_grid.x_min = x << 4;

		max_wid = wild_grid.x_max;
		min_wid = wild_grid.x_min;

		allocate_all();
		return;
	}

	/* Store in upper left hand corner. */
	wild_grid.x = x;

	/* Recalculate boundaries */
	wild_grid.x_max = (x + WILD_GRID_SIZE) << 4;
	wild_grid.x_min = x << 4;

	max_wid = wild_grid.x_max;
	min_wid = wild_grid.x_min;

	/* Shift in only a small discrepency */
	if (ABS(dx) == 1)
	{
		if (dx == 1) shift_right();
		else shift_left();

		/* Done */
		return;
	}

	if (dx)
	{
		/* Too big of a jump */
		allocate_all();
	}
}
