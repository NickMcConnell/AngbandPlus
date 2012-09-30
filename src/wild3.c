/* File: wild3.c */

/* Purpose: Wilderness drawing */

/*
 * Copyright (c) 1989, 2003 James E. Wilson, Robert A. Koeneke,
 *                          Robert Ruehlmann, Steven Fuerst
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */
#include "angband.h"

#include "wild.h"

/*
 * Helper functions that can be called recursively.  (Need function prototypes.)
 * See make_wild_03() for an instance of this.
 * This ability will also be used by other routines in the future.
 */
static void gen_block_helper(blk_ptr block_ptr, byte *data, int gen_type,
							 bool road);
static void blend_helper(cave_type *c_ptr, byte *data, int g_type);



/*
 * Generate the selected place
 */
static void place_gen(place_type *pl_ptr)
{
	switch (pl_ptr->type)
	{
		case TOWN_OLD:
		{
			van_town_gen(pl_ptr);
			break;
		}
		case TOWN_FRACT:
		{
			draw_city(pl_ptr);
			break;
		}
		case TOWN_QUEST:
		{
			draw_quest(pl_ptr);
			break;
		}
		case TOWN_DUNGEON:
		{
			draw_dungeon(pl_ptr);
			break;
		}
		default:
		{
			quit("Unknown town/quest type in wilderness");
		}
	}

	/* Hack - set global region back to wilderness value */
	set_region(0);
}


/*
 * Overlay the town block
 * If the town is not built correctly, build it
 */
static void overlay_place(int x, int y, u16b w_place, blk_ptr block_ptr)
{
	int i, j, x1, y1, x2, y2;

	int fld_idx, type;

	wild_done_type *w_ptr = &wild[y][x].done;

	/* Generation level for monsters and objects */
	int level = w_ptr->mon_gen;

	cave_type *c_ptr;
	place_type *pl_ptr = &place[w_place];

	/* Check that place region exists */
	if (!pl_ptr->region)
	{
		/* Create the place */
		place_gen(pl_ptr);
	}

	/* Paranoia */
	if (!pl_ptr->region) quit("Could not get a region for the town/quest");

	/* Find block to copy */
	x1 = (x - place[w_place].x) * WILD_BLOCK_SIZE;
	y1 = (y - place[w_place].y) * WILD_BLOCK_SIZE;

	/* copy 16x16 block from the region */
	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			/* Get pointer to overlay info */
			c_ptr = access_region(x1 + i, y1 + j, pl_ptr->region);

			/* Only copy terrain if there is something there. */
			if (c_ptr->feat != FEAT_NONE)
			{
				/* Copy the terrain */
				block_ptr[j][i].feat = c_ptr->feat;
			}
			/* Get destination */
			x2 = x * WILD_BLOCK_SIZE + i;
			y2 = y * WILD_BLOCK_SIZE + j;


			/*
			 * Instantiate field
			 *
			 * Note that most types of field are not in this list.
			 *
			 * Doors, buildings, traps, quests etc.
			 * are all that are in this list.
			 */
			fld_idx = c_ptr->fld_idx;

			if (fld_idx)
			{
				type = t_info[c_ptr->fld_idx].type;
			}
			else
			{
				type = FTYPE_NOTHING;
			}

			switch (type)
			{
				case FTYPE_NOTHING:
				{
					/* Nothing */
					break;
				}

				case FTYPE_TRAP:
				{
					/* Trap */

					/* Activate the trap */
					if (place_field(x2, y2, c_ptr->fld_idx))
					{
						field_type *f_ptr = &fld_list[block_ptr[j][i].fld_idx];
					
						/* Initialise it */
						(void)field_script_single(f_ptr, FIELD_ACT_INIT, "");
					}

					break;
				}

				case FTYPE_DOOR:
				{
					/* Door */
					int data = 9;

					/* Add a door field */
					if (place_field(x2, y2, c_ptr->fld_idx))
					{
						field_type *f_ptr = &fld_list[block_ptr[j][i].fld_idx];
					
						/* Add "power" of lock / jam to the field */
						(void)field_script_single(f_ptr, FIELD_ACT_INIT,
												"i:", LUA_VAR_NAMED(data, "power"));
					}

					break;
				}

				case FTYPE_BUILD:
				{
					/* Stores + buildings */
					(void)place_field(x2, y2, c_ptr->fld_idx);

					break;
				}
			}
			
			/*
			 * Instantiate object
			 */
			place_specific_object(x2, y2, level, c_ptr->o_idx);

			/*
			 * Instantiate monster
			 */
			if (c_ptr->m_idx)
			{
				place_monster_one(x2, y2, c_ptr->m_idx, FALSE, FALSE, FALSE);
			}
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
										 (randint1(lstep * 256) -
										  (hstep * 256))) / 2);
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
										 (randint1(lstep * 256) -
										  (hstep * 256))) / 2);
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
										 temp_block[j + hstep][i +
															   hstep]) / 4) +
						(((randint1(lstep * 256) - (hstep * 256)) * 181) / 256);
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
										 temp_block[j][i + hstep]) / 2);
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
										 temp_block[j + hstep][i]) / 2);
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
										 temp_block[j + hstep][i + hstep]) / 4);
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
                      byte prob1, byte prob2, byte prob3, byte prob4, byte prob)
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
			c2 = 0x1000000 / ABS((long)prob2 - prob);
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
			c3 = 0x1000000 / ABS((long)prob3 - prob);
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
			c4 = 0x1000000 / ABS((long)prob4 - prob);
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
											 FEAT_OCEAN_WATER, FEAT_NONE, 0, 10,
											 20, 40, sea_type);
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

	bool bridge = FALSE, need_bridge = FALSE;


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
					
					/* Bridges are narrow */
					if (wild[y1][x1].done.info & WILD_INFO_WATER)
					{
						grad1[i] = TRACK_LEVEL;
					}
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
					
					/* Bridges are narrow */
					if (wild[y1][x1].done.info & WILD_INFO_WATER)
					{
						grad2[i] = TRACK_LEVEL;
					}
				}
			}
		}
	}

	/* Scan sides for ground */
	for (i = 1; i < 10; i++)
	{
		/* Get direction */
		x1 = x + ddx[i];
		y1 = y + ddy[i];
		
		if (wild[y1][x1].done.info & WILD_INFO_WATER)
		{
			/* We are over water - so we need a bridge */
			need_bridge = TRUE;
		}
		else
		{
			/* We are adjacent to solid ground */
			bridge = TRUE;
		}
	}
	
	/* Only use wood terrain if we need a bridge */
	if (!need_bridge) bridge = FALSE;

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

				/* Bad liquid terrain? */
				if ((c_ptr->feat == FEAT_SHAL_LAVA) ||
					(c_ptr->feat == FEAT_DEEP_LAVA) ||
					(c_ptr->feat == FEAT_SHAL_ACID) ||
					(c_ptr->feat == FEAT_DEEP_ACID))
				{
					c_ptr->feat = FEAT_PEBBLES;
				}
				else if (bridge)
				{
						c_ptr->feat = FEAT_FLOOR_WOOD;
				}
				
				else if ((c_ptr->feat == FEAT_SHAL_WATER) ||
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
static void make_wild_01(blk_ptr block_ptr, byte *data, bool road)
{
	int i, j;
	byte new_feat, element;

	/* Ignore road for now */
	(void)road;

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
static void make_wild_02(blk_ptr block_ptr, byte *data, bool road)
{
	int i, j, k;
	byte new_feat, feat, chance;

	/* Ignore road for now */
	(void)road;

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
static void make_wild_03(blk_ptr block_ptr, byte *data, bool road)
{
	int i, j, element;

	/* Call the other routine to make the "base" terrain. */
	gen_block_helper(block_ptr, wild_gen_data[data[0]].data,
					 wild_gen_data[data[0]].gen_routine, road);

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
 * Draw a pleasant field (farm)
 */
static void make_wild_04(blk_ptr block_ptr, byte *data, bool road)
{
	int x, y, x1, y1, x2, y2, i, j;
	int type;

	cave_type *c_ptr;

	/* Hack - ignore parameter */
	(void)data;

	/* Hack - generate and throw away a few random numbers */
	randint0(100);
	randint0(100);
	randint0(100);

	/* Get location of building */
	x = rand_range(4, 11);
	y = rand_range(3, 12);

	/* Get size of building */
	x1 = x - randint1(3);
	x2 = x + randint1(3);
	y1 = y - randint1(2);
	y2 = y + randint1(2);

	/* Get type of ground */
	switch (randint0(8))
	{
		case 0:
		case 1:
		case 2:
		{
			/* Grass */
			type = 1;
			break;
		}
		case 3:
#if 0
		{
			/* Use "underlying" type */
			gen_block_helper(block_ptr, wild_gen_data[data[0]].data,
							 wild_gen_data[data[0]].gen_routine, road);
			return;
		}
#endif
		case 4:
		{
			/* Alternating grass & dirt */
			type = 3;
			break;
		}
		case 5:
		{
			/* Dirt */
			type = 2;
			break;
		}
		case 6:
		{
			/* Dirt with building */
			type = 4;
			break;
		}
		default:
		{
			/* Grass with building */
			type = 5;
			break;
		}
	}

	/*
	 * If there is a road or river going through here we should
	 * use types 1 or 2 because we don't want roads running through
	 * buildings and other weirdness.
	 */
	if (road && (type > 2)) type = rand_range(1, 2);

	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Get location */
			c_ptr = &block_ptr[j][i];

			/* Place ground */
			if (type == 1 || (type == 3 && j % 2 == 0) || type == 5)
			{
				c_ptr->feat = FEAT_GRASS;
			}
			else
			{
				c_ptr->feat = FEAT_DIRT;
			}

			if ((i >= x1) && (i <= x2) && (j >= y1) && (j <= y2) && (type >= 4))
			{
				/* Build an invulnerable rectangular building */
				c_ptr->feat = FEAT_PERM_EXTRA;
			}
			else if ((i >= x1 - 1) && (i <= x2 + 1) &&
					 (j >= y1 - 1) && (j <= y2 + 1) && (type >= 4))
			{
				c_ptr->feat = FEAT_DIRT;
			}
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
							FEAT_OCEAN_WATER, FEAT_NONE, 0, 10, 20, 40,
							sea_type);
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


static void blend_helper(cave_type *c_ptr, byte *data, int g_type)
{
	/* Based on type - choose wilderness block generation function */
	switch (g_type)
	{
		case 1:
		{
			/* Fractal plasma with weighted terrain probabilites */
			blend_wild_01(c_ptr, data);
			break;
		}
		case 2:
		{
			/* Simple weighted probabilities on flat distribution */
			blend_wild_02(c_ptr, data);
			break;
		}
		case 3:
		{
			/* Use the other terrain's blend function */
			blend_helper(c_ptr, wild_gen_data[data[0]].data,
						 wild_gen_data[data[0]].gen_routine);
			/* break;  Restore this when we do something for case 4 */
		}
		case 4:
		{
			/* Don't do anything */
			break;
		}
		default:
		{
			msgf("Illegal wilderness type %d ", g_type);
		}
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

	/* Get current location */
	w_type = wild[y][x].done.wild;

	/* Farms do not blend */
	if (wild_gen_data[w_type].gen_routine == 4) return;

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
static void gen_block_helper(blk_ptr block_ptr, byte *data, int gen_type,
                             bool road)
{
	/* Based on type - choose wilderness block generation function */
	switch (gen_type)
	{
		case 1:
		{
			/* Fractal plasma with weighted terrain probabilites */
			make_wild_01(block_ptr, data, road);
			break;
		}
		case 2:
		{
			/* Uniform field + rare "out-crops" */
			make_wild_02(block_ptr, data, road);
			break;
		}
		case 3:
		{
			/* Use another type + overlay a "circle" of terrain. */
			make_wild_03(block_ptr, data, road);
			break;
		}
		case 4:
		{
			/* Draw a farm. */
			make_wild_04(block_ptr, data, road);
			break;
		}
		default:
		{
			quit("Illegal wilderness block type.");
		}
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

	xx = x * WILD_BLOCK_SIZE;
	yy = y * WILD_BLOCK_SIZE;

	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* See if monster should go on square */
			if (!randint0(prob))
			{
				if (one_in_(2))
				{
					/* Monsters are awake */
					(void)place_monster(xx + i, yy + j, FALSE, TRUE, 0);
				}
				else
				{
					/* Monsters are asleep */
					(void)place_monster(xx + i, yy + j, TRUE, TRUE, 0);
				}
			}
		}
	}
}

void light_dark_square(int x, int y, bool daytime)
{
	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);

	if (daytime)
	{
		/* Assume lit */
		c_ptr->info |= (CAVE_GLOW);

		/* Memorize lit grids */
		remember_grid(c_ptr, pc_ptr);

		/* If is daytime - have seen this square */
		wild[y / 16][x / 16].done.info |= WILD_INFO_SEEN;
	}
	else
	{
		/* Darken "boring" features */
		if (!(((c_ptr->feat >= FEAT_OPEN) &&
			   (c_ptr->feat <= FEAT_MORE)) ||
			  ((c_ptr->feat >= FEAT_CLOSED) &&
			   (c_ptr->feat <= FEAT_PERM_SOLID))))
		{
			/* Hack - Forget the grid */
			c_ptr->info &= ~(CAVE_GLOW);
			forget_grid(pc_ptr);
		}
		else
		{
			/* Assume lit */
			c_ptr->info |= (CAVE_GLOW);

			/* Memorize lit grids */
			remember_grid(c_ptr, pc_ptr);
		}
	}
	
	/* Hack -- Light spot */
	lite_spot(x, y);
}


/* Lighten / Darken new block depending on Day/ Night */
static void light_dark_block(int x, int y)
{
	int i, j;

	bool daytime;

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
			light_dark_square(x * WILD_BLOCK_SIZE + i, y * WILD_BLOCK_SIZE + j,
							  daytime);
		}
	}
}


/*
 * Make a new block based on the terrain type
 */
static void gen_block(int x, int y)
{
	u16b w_place, w_type;
	blk_ptr block_ptr = wild_grid[y][x];
	bool road = FALSE;

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant wilderness blocks */
	Rand_value = wild_seed + x + y * max_wild;

	/* Generate a terrain block */

	/* Get wilderness type */
	w_type = wild[y][x].done.wild;

	/* Is there a road here? */
	if (wild[y][x].done.info & (WILD_INFO_TRACK | WILD_INFO_ROAD))
	{
		road = TRUE;
	}

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
						 wild_gen_data[w_type].gen_routine, road);

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
		else if (wild_info_bounds(x, y, WILD_INFO_LAVA))
		{
			/* Hack, above function sets bounds */

			/* Generate plasma factal */
			frac_block();

			/* Overlay lava */
			wild_add_gradient(block_ptr, FEAT_SHAL_LAVA, FEAT_DEEP_LAVA);
		}

		/* Add acid boundary effects. */
		else if (wild_info_bounds(x, y, WILD_INFO_ACID))
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

	/* Overlay place */
	w_place = wild[y][x].done.place;

	/* Is there a place? */
	if (w_place)
	{
		/* overlay place on wilderness */
		overlay_place(x, y, w_place, block_ptr);

		/* Paranoia */
		if (!place[w_place].region) quit("Unallocated place region");
	}

	/* Day / Night - lighten or darken the new block */
	light_dark_block(x, y);

	/* Add monsters */
	add_monsters_block(x, y);
}


/*
 * Erase the player grid information in a block
 */
static void erase_grids(pblk_ptr block_ptr)
{
	int i, j;

	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* No memorised feature */
			block_ptr[i][j].feat = FEAT_NONE;

			/* All flags off */
			block_ptr[i][j].player = 0x00;
		}
	}
}


/*
 * The following four functions shift the visible
 * section of the wilderness by 16 units. This is
 * done by scrolling the grid of pointers.
 */
static void shift_down(void)
{
	u16b i, j;
	pblk_ptr block_ptr;

	for (i = 0; i < WILD_VIEW; i++)
	{
		/* The block on the edge */
		block_ptr = p_ptr->pwild[0][i];

		/* Delete the block */
		erase_grids(block_ptr);

		/* Scroll pointers */
		for (j = 1; j < WILD_VIEW; j++)
		{
			p_ptr->pwild[j - 1][i] = p_ptr->pwild[j][i];
		}

		/* Connect new grid to wilderness */
		p_ptr->pwild[WILD_VIEW - 1][i] = block_ptr;
	}
}


static void shift_up(void)
{
	u16b i, j;
	pblk_ptr block_ptr;

	for (i = 0; i < WILD_VIEW; i++)
	{
		/* The block on the edge */
		block_ptr = p_ptr->pwild[WILD_VIEW - 1][i];

		/* Delete the block */
		erase_grids(block_ptr);

		/* Scroll pointers */
		for (j = WILD_VIEW - 1; j > 0; j--)
		{
			p_ptr->pwild[j][i] = p_ptr->pwild[j - 1][i];
		}

		/* Connect new grid to wilderness */
		p_ptr->pwild[0][i] = block_ptr;
	}
}


static void shift_right(void)
{
	u16b i, j;
	pblk_ptr block_ptr;

	for (j = 0; j < WILD_VIEW; j++)
	{
		/* The block on the edge */
		block_ptr = p_ptr->pwild[j][0];

		/* Delete the block */
		erase_grids(block_ptr);

		/* Scroll pointers */
		for (i = 1; i < WILD_VIEW; i++)
		{
			p_ptr->pwild[j][i - 1] = p_ptr->pwild[j][i];
		}

		/* Connect new grid to wilderness */
		p_ptr->pwild[j][WILD_VIEW - 1] = block_ptr;
	}
}


static void shift_left(void)
{
	u16b i, j;
	pblk_ptr block_ptr;

	for (j = 0; j < WILD_VIEW; j++)
	{
		/* The block on the edge */
		block_ptr = p_ptr->pwild[j][WILD_VIEW - 1];

		/* Delete the block */
		erase_grids(block_ptr);

		/* Scroll pointers */
		for (i = WILD_VIEW - 1; i > 0; i--)
		{
			p_ptr->pwild[j][i] = p_ptr->pwild[j][i - 1];
		}

		/* Connect new grid to wilderness */
		p_ptr->pwild[j][0] = block_ptr;
	}
}


/* Delete a wilderness block */
static void del_block(int x, int y)
{
	blk_ptr block_ptr;
	int xx, yy;
	int m_idx;

	wild_type *w_ptr = &wild[y][x];
	place_type *pl_ptr = &place[w_ptr->done.place];

	if (!wild_refcount[y][x]) quit("Dead wilderness cache!");

	/* Decrement refcount */
	wild_refcount[y][x]--;

	/* Don't do anything if someone else is here */
	if (wild_refcount[y][x]) return;

	/* Is there a place? */
	if (w_ptr->done.place)
	{
		/* Decrease refcount region */
		pl_ptr->region = unref_region(pl_ptr->region);
	}

	/* Time to delete it - get block pointer */
	block_ptr = wild_grid[y][x];

	for (xx = 0; xx < WILD_BLOCK_SIZE; xx++)
	{
		for (yy = 0; yy < WILD_BLOCK_SIZE; yy++)
		{
			/* Clear old terrain data */
			block_ptr[yy][xx].info = 0;
			block_ptr[yy][xx].feat = 0;

			/* Delete monster on the square */
			m_idx = block_ptr[yy][xx].m_idx;

			/* Only delete if one exists */
			if (m_idx)
			{
				delete_monster_idx(m_idx);
				block_ptr[yy][xx].m_idx = 0;
			}

			/* Delete objects on the square */
			delete_object_list(&block_ptr[yy][xx].o_idx);

			/* Delete fields on the square */
			delete_field_location(&block_ptr[yy][xx]);
		}
	}

	/* Clear old reference */
	wild_grid[y][x] = NULL;

	/* Attach to head of the list */
	wild_cache[--wc_cnt] = block_ptr;
}

/*
 * Allocate a new block
 */
static void allocate_block(int x, int y)
{
	byte place_num = wild[y][x].done.place;

	/* Increment refcount */
	wild_refcount[y][x]++;

	/* Need to make the block if it doesn't exist */
	if (!wild_grid[y][x])
	{
		/* Paranoia */
		if (wc_cnt >= WILD_CACHE) quit("Out of wilderness cache");

		/* Get new block */
		wild_grid[y][x] = wild_cache[wc_cnt++];

	
		/* Are we in the process of loading the game? */
		if (character_loaded)
		{
			/* Generate the block */
			gen_block(x, y);

			if (place_num)
			{
				/* Increase refcount for region */
				incref_region(place[place_num].region);
			}
		}
		
		/* We need to make sure the refcounted regions work */
		else if (place_num)
		{
			place_type *pl_ptr = &place[place_num];

			/* Do we need to make the map? */
			if (!pl_ptr->region)
			{
				/* Create the place */
				place_gen(pl_ptr);
			}
			
			/* Increase refcount for region */
			incref_region(pl_ptr->region);
		}
	}
}


void shift_in_bounds(int *x, int *y)
{
	/* Vanilla town is special */
	if (vanilla_town)
	{
		*x = 0;
		*y = 0;
		return;
	}

	/* Recenter map */
	*x -= WILD_VIEW / 2;
	*y -= WILD_VIEW / 2;

	/* Move if out of bounds */
	if (*x < 0) *x = 0;
	if (*y < 0) *y = 0;

	/* Hack XXX This isn't set when we are called during loading */
	if (max_wild)
	{
		if (*x + WILD_VIEW >= max_wild) *x = max_wild - WILD_VIEW - 1;
		if (*y + WILD_VIEW >= max_wild) *y = max_wild - WILD_VIEW - 1;
	}
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
	int x, y;
	int ox = p_ptr->old_wild_x, oy = p_ptr->old_wild_y;
	int i, j;

	quest_type *q_ptr;
	place_type *pl_ptr;
	wild_done_type *w_ptr;

	/* Get upper left hand block in grid. */

	/* Divide by WILD_BLOCK_SIZE to get block from (x,y) coord */
	x = ((u16b)p_ptr->wilderness_x / WILD_BLOCK_SIZE);
	y = ((u16b)p_ptr->wilderness_y / WILD_BLOCK_SIZE);

	w_ptr = &wild[y][x].done;

	/* The player sees the wilderness block he is on. */
	w_ptr->info |= WILD_INFO_SEEN;

	/* Hack - set place */
	p_ptr->place_num = w_ptr->place;

	/* Move boundary */
	shift_in_bounds(&x, &y);

	/* If we haven't moved block - exit */
	if ((ox == x) && (oy == y)) return;
	
	/* We have moved, notice the new town/dungeon */
	activate_quests(0);
	
	pl_ptr = &place[p_ptr->place_num];

	/* Check for wilderness quests */
	if (pl_ptr->quest_num)
	{
		q_ptr = &quest[pl_ptr->quest_num];

		/* Some quests are completed by walking on them */
		if (q_ptr->x_type == QX_WILD_ENTER)
		{
			/* Done? */
			trigger_quest_complete(QX_WILD_ENTER, (vptr)q_ptr);
		}
	}

	/* Shift the player information */
	while (ox < x)
	{
		ox++;
		shift_right();
	}

	while (ox > x)
	{
		ox--;
		shift_left();
	}

	while (oy < y)
	{
		oy++;
		shift_down();
	}

	while (oy > y)
	{
		oy--;
		shift_up();
	}

	/* Reset bounds */
	p_ptr->min_wid = x * WILD_BLOCK_SIZE;
	p_ptr->min_hgt = y * WILD_BLOCK_SIZE;
	p_ptr->max_wid = p_ptr->min_wid + WILD_VIEW * WILD_BLOCK_SIZE;
	p_ptr->max_hgt = p_ptr->min_hgt + WILD_VIEW * WILD_BLOCK_SIZE;

	/* Allocate new blocks */
	for (i = 0; i < WILD_VIEW; i++)
	{
		for (j = 0; j < WILD_VIEW; j++)
		{
			allocate_block(x + i, y + j);
		}
	}

	/* Deallocate old blocks */
	for (i = 0; i < WILD_VIEW; i++)
	{
		for (j = 0; j < WILD_VIEW; j++)
		{
			del_block(p_ptr->old_wild_x + i, p_ptr->old_wild_y + j);
		}
	}

	/* Redraw depth */
	p_ptr->redraw |= (PR_DEPTH);

	/* Save the new location */
	p_ptr->old_wild_x = x;
	p_ptr->old_wild_y = y;
}


/*
 * Access the cave region data.
 */
static cave_type *access_cave(int x, int y)
{
	return (cave_p(x, y));
}


/*
 * Access player information in dungeon
 */
static pcave_type *access_pcave(int x, int y)
{
	return (&p_ptr->pcave[y][x]);
}

/*
 * Access wilderness
 */
static cave_type *access_wild(int x, int y)
{
	/*
	 * Divide by 16 to get block.
	 * Logical AND with 15 to get location within block.
	 */
	return (&wild_grid[y / WILD_BLOCK_SIZE][x / WILD_BLOCK_SIZE]
			[y & 15][x & 15]);
}

/*
 * Access player information in wilderness
 */
static pcave_type *access_pwild(int x, int y)
{
	/*
	 * Divide by 16 to get block.
	 * Logical AND with 15 to get location within block.
	 */
	return (&p_ptr->pwild[(y - p_ptr->min_hgt) / WILD_BLOCK_SIZE]
			[(x - p_ptr->min_wid) / WILD_BLOCK_SIZE][y & 15][x & 15]);
}


/*
 * Bounds checking
 *
 * Hack - in_bounds() and in_bounds2() are the same
 * in the wilderness.
 */
static bool in_bounds_wild(int x, int y)
{
	/* Make sure we are inside the wilderness */
	if ((y < 0) || (x < 0) ||
		(y >= max_wild * WILD_BLOCK_SIZE) || (x >= max_wild * WILD_BLOCK_SIZE))
	{
		return (FALSE);
	}

	/* Return TRUE if block is in use */
	return (wild_refcount[y / WILD_BLOCK_SIZE][x / WILD_BLOCK_SIZE] != 0);
}

static bool in_bounds_cave(int x, int y)
{
	return ((y > p_ptr->min_hgt) && (x > p_ptr->min_wid)
			&& (y < p_ptr->max_hgt - 1) && (x < p_ptr->max_wid - 1));
}

static bool in_bounds2_cave(int x, int y)
{
	return ((y >= p_ptr->min_hgt) && (x >= p_ptr->min_wid)
			&& (y < p_ptr->max_hgt) && (x < p_ptr->max_wid));
}

/*
 * In bounds for the player information?
 */
static bool in_bounds_wild_player(int x, int y)
{
	/* Use the same player bounds information as in_bounds_cave() */
	return ((y >= p_ptr->min_hgt) && (x >= p_ptr->min_wid)
			&& (y < p_ptr->max_hgt) && (x < p_ptr->max_wid));
}


/* Allocate all grids around player */
static void init_wild_cache(void)
{
	int x = p_ptr->old_wild_x, y = p_ptr->old_wild_y;
	int i, j;

	pblk_ptr block_ptr;

	/* Allocate blocks around player */
	for (i = 0; i < WILD_VIEW; i++)
	{
		for (j = 0; j < WILD_VIEW; j++)
		{
			/* Hack - erase the player knowledge */
			block_ptr = p_ptr->pwild[j][i];
			erase_grids(block_ptr);

			allocate_block(x + i, y + j);
		}
	}
}

/* Deallocate all grids around player */
static void del_wild_cache(void)
{
	int x = p_ptr->old_wild_x, y = p_ptr->old_wild_y;
	int i, j;

	if (!wc_cnt) quit("Deleting empty wilderness cache!");

	/* The player no longer is in the wilderness */
	character_dungeon = FALSE;

	/* Deallocate blocks around player */
	for (i = 0; i < WILD_VIEW; i++)
	{
		for (j = 0; j < WILD_VIEW; j++)
		{
			del_block(x + i, y + j);
		}
	}
}


/*
 * Fix problems due to dungeons not starting at level 1.
 *
 * direction is -1 for going down, and +1 for up.
 */
void move_dun_level(int direction)
{
	place_type *pl_ptr = &place[p_ptr->place_num];
	dun_type *d_ptr = pl_ptr->dungeon;
	
	/* Change depth */
	p_ptr->depth += direction;

	/* Leaving */
	p_ptr->state.leaving = TRUE;
		
	/* Out of bounds? */
	if (p_ptr->depth < d_ptr->min_level)
	{
		/* We have just decended - and have to decend more? */
		if (direction == 1)
		{
			p_ptr->depth = d_ptr->min_level;
		}
		else
		{
			/* Go to surface. */
			p_ptr->depth = 0;
		}
	}
	
	/* Make sure the deepest level is set correctly */
	d_ptr->recall_depth = MAX(d_ptr->recall_depth, p_ptr->depth);
}


/*
 * This function _must_ be called whenever the dungeon level changes.
 * It makes sure the bounds and access functions point to the correct
 * functions.  If this is not done - bad things happen.
 */

void change_level(int level)
{
	place_type *pl_ptr = &place[p_ptr->place_num];

	bool switched = FALSE;

	/* Hack - reset trap detection flag */
	p_ptr->state.detected = FALSE;

	/* Clear the monster lights */
	clear_mon_lite();

	/* Toggle list of active quests */
	activate_quests(level);

	if (level == 0)
	{
		if (pl_ptr->dungeon && pl_ptr->dungeon->region)
		{
			/* Delete dungeon */
			pl_ptr->dungeon->region = unref_region(pl_ptr->dungeon->region);
		}

		/* In the wilderness */
		p_ptr->px = (s16b)p_ptr->wilderness_x;
		p_ptr->py = (s16b)p_ptr->wilderness_y;

		/* Notice player location */
		Term_move_player();

		/* Used to be in the dungeon? */
		if (area_aux != access_wild) switched = TRUE;

		/* Access the wilderness */
		area_aux = access_wild;
		parea_aux = access_pwild;

		/* Bounds checking rountine */
		in_bounds = in_bounds_wild;
		in_bounds2 = in_bounds_wild;
		in_boundsp = in_bounds_wild_player;

		/* Initialise the boundary */
		p_ptr->min_wid = p_ptr->old_wild_x * WILD_BLOCK_SIZE;
		p_ptr->min_hgt = p_ptr->old_wild_y * WILD_BLOCK_SIZE;
		p_ptr->max_wid = p_ptr->min_wid + WILD_VIEW * WILD_BLOCK_SIZE;
		p_ptr->max_hgt = p_ptr->min_hgt + WILD_VIEW * WILD_BLOCK_SIZE;
		
		/* Update panels (later) */
		p_ptr->update |= (PU_MAP);

		/*
		 * Restore the outside town if it exists
		 * This is mainly done to reinit the fields
		 */
		if (switched)
		{
			/* Create wilderness */
			init_wild_cache();
		}
	}
	else
	{
		/* In the dungeon */
		if (pl_ptr->dungeon && pl_ptr->dungeon->region)
		{
			/* Delete old dungeon */
			pl_ptr->dungeon->region = unref_region(pl_ptr->dungeon->region);

			/* New dungeon is created in generate.c */
		}

		/* Used to be in the wilderness? */
		if (area_aux == access_wild)
		{
			/* Hack XXX XXX Delete the wilderness cache */
			del_wild_cache();
		}

		/* 
		 * Default bounds - allocated in generate.c
		 *
		 * Should these be set here at all???
		 */
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = MAX_HGT;
		p_ptr->min_wid = 0;
		p_ptr->max_wid = MAX_WID;

		/* Access the cave */
		area_aux = access_cave;
		parea_aux = access_pcave;
		
		/* Bounds checking */
		in_bounds = in_bounds_cave;
		in_bounds2 = in_bounds2_cave;
		in_boundsp = in_bounds2_cave;
	}

	/* Tell the rest of the world that the map is no longer valid */
	Term_erase_map();
}


/*
 * Get the base level for objects and monsters
 * around the player.
 */
int base_level(void)
{
	wild_done_type *w_ptr;

	/* Are we in the dungeon? */
	if (p_ptr->depth) return (p_ptr->depth);
	
	/* Point to wilderness block info */
	w_ptr = &wild[p_ptr->py / 16][p_ptr->px / 16].done;
	
	/* The level of the wilderness */
	return(w_ptr->mon_gen);
}


/*
 * What is the current dungeon?
 */
dun_type *dungeon(void)
{
	place_type *pl_ptr = &place[p_ptr->place_num];

	/* Return the dungeon */
	return (pl_ptr->dungeon);
}


/*
 * Delete all active things
 */
void wipe_all_list(void)
{
	int i;
	
	/* Hack - cull the players inventory */
	if (p_ptr->inventory) delete_object_list(&p_ptr->inventory);

	/* Clear the store cache */
	for (i = 0; i < store_cache_num; i++)
	{
		if (store_cache[i]->stock)
		{
			delete_object_list(&store_cache[i]->stock);
		}
	}
	store_cache_num = 0;

	if (p_ptr->depth)
	{
		/* In the dungeon */
		wipe_rg_list();

		/* No more dungeon */
		cur_region = 0;
	}
	else
	{
		/* In the wilderness - delete cache if it exists */
		if (wc_cnt) del_wild_cache();
	}

	/* reset function pointers */
	area_aux = NULL;
	parea_aux = NULL;

	in_bounds = NULL;
	in_bounds2 = NULL;
	in_boundsp = NULL;
}

/*
 * Get the maximum dungeon level ever reached.
 */
int max_dun_level_reached(void)
{
	int i, max = 0;
	
	place_type *pl_ptr;
	dun_type *d_ptr;
	
	/* Scan all places */
	for (i = 0; i < place_count; i++)
	{
		pl_ptr = &place[i];
		
		if (pl_ptr->dungeon)
		{
			d_ptr = pl_ptr->dungeon;
			
			/* Best depth? */
			if (d_ptr->recall_depth > max)
			{
				max = d_ptr->recall_depth;
			}
		}
	}

	/* Done */
	return (max);
}
