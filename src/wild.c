/* File: wild.c */

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

/*
 * Helper functions that can be called recursively.  (Need function prototypes.)
 * See make_wild_03() for an instance of this.
 * This ability will also be used by other routines in the future.
 */
void gen_block_helper(blk_ptr block_ptr, byte *data, int gen_type);
void blend_helper(cave_type *c_ptr, byte *data,int g_type);


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
	if (daytime)
	{
		wild[y][x].done.info |= WILD_INFO_SEEN;
	}

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
					(c_ptr->feat <= FEAT_SHAL_LAVA))))
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


/* Town currently stored in cave[][] */
static u16b cur_town;


/* Access the old cave array. */
static cave_type *access_cave(int y, int x)
{
	return &cave[y][x];
}

/* Access wilderness */
static cave_type *access_wild(int y, int x)
{
	/*
	 * Divide by 16 to get block.
	 * Logical AND with 15 to get location within block.
	 */

	return &wild_grid.block_ptr[((u16b) y>>4) - wild_grid.y]
		[((u16b) x>>4) - wild_grid.x][y & 15][x & 15];
}


/*
 * This function _must_ be called whenever the dungeon level changes.
 * It makes sure the bounds and access functions point to the correct
 * functions.  If this is not done - bad things happen.
 */

void change_level(int level)
{
	/* Hack - reset trap detection flag */
	p_ptr->detected = FALSE;
	
	/* Clear the monster lights */
	clear_mon_lite();
	
	if (!level)
	{
		/* In the wilderness */
		
		/* Reset the bounds */
		min_hgt = wild_grid.y_min;
		max_hgt = wild_grid.y_max;
		min_wid = wild_grid.x_min;
		max_wid = wild_grid.x_max;
		
		/* Access the wilderness */
		area = access_wild;

		if (!dun_level)
		{
			/* Lighten / darken wilderness */
			day_night();
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
static void build_store(int n, int yy, int xx)
{
	int y, x, y0, x0, y1, x1, y2, x2, tmp;

	/* Find the "center" of the store */
	y0 = yy * 6 + 4;
	x0 = xx * 17 + 8;

	/* Determine the store boundaries */
	y1 = y0 - randint(2);
	y2 = y0 + randint(2);
	x1 = x0 - randint(5);
	x2 = x0 + randint(5);

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
	tmp = rand_int(4);

	/* Re-roll "annoying" doors */
	if (((tmp == 0) && (yy == 2)) ||
	    ((tmp == 1) && (yy == 0)) ||
	    ((tmp == 2) && (xx == 2)) ||
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
	cave[y][x].feat = FEAT_SHOP_HEAD + n;
}

/* Dodgy replacement for SCREEN_WID and SCREEN_HGT */

/* This will be removed later. */
#define TOWN_WID	66
#define TOWN_HGT	22


static void add_town_wall(void)
{
	int i;
	
	/* Upper and lower walls */
	for (i = 0; i < TOWN_WID; i++)
	{
		/* Wall with doors in middle */
		if (i == TOWN_WID / 2)
		{
			/* Make town gates */
			cave[0][i].feat = FEAT_CLOSED;
			cave[TOWN_HGT - 1][i].feat = FEAT_CLOSED;
		}
		else
		{
			/* Make walls */
			cave[0][i].feat = FEAT_PERM_OUTER;
			cave[TOWN_HGT - 1][i].feat = FEAT_PERM_OUTER;
		}
	}

	/* Left and right walls */
	for (i = 1; i < TOWN_HGT - 1; i++)
	{
		/* Walls with doors in middle. */
		if (i == TOWN_HGT / 2)
		{
			/* Make town gates (locked) */
			cave[i][0].feat = FEAT_CLOSED;
			cave[i][TOWN_WID - 1].feat = FEAT_CLOSED;
		}
		else
		{
			/* Make walls */
			cave[i][0].feat = FEAT_PERM_OUTER;
			cave[i][TOWN_WID - 1].feat = FEAT_PERM_OUTER;
		}
	}
}

/*
 * Generate the "consistent" town features, and place the player
 *
 * Hack -- play with the R.N.G. to always yield the same town
 * layout, including the size and shape of the buildings, the
 * locations of the doorways, and the location of the stairs.
 */
static void town_gen_hack(u16b town_num, int *xx, int *yy)
{
	int y, x, k, n;

	/* This simple routine does not check the type of
	 * stores town_num wants.
	 */

	int rooms[MAX_STORES];
	byte feat;

	/* Prepare an Array of "remaining stores", and count them */
	for (n = 0; n < MAX_STORES; n++) rooms[n] = n;

	/* Place three rows of stores */
	for (y = 0; y < 3; y++)
	{
		/* Place three stores per row */
		for (x = 0; x < 3; x++)
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
		*yy = rand_range(3, TOWN_HGT - 4);
		*xx = rand_range(3, TOWN_WID - 4);

		feat = cave[*yy][*xx].feat;
		
		/* If square is a shop, exit */
		if (feat == FEAT_PERM_EXTRA) continue;
		if ((feat >= FEAT_SHOP_HEAD) &&
			(feat <= FEAT_SHOP_TAIL)) continue;
		
		/* Blank square */
		break;
	}

	/* Put dungeon floor next to stairs so they are easy to find. */
	for (y = -1; y <= 1; y++)
	{
		for (x = -1; x <= 1; x++)
		{
			/* Get feat at location */
			feat = cave[*yy + y][*xx + x].feat;

			/* If square is a shop, exit */
			if (feat == FEAT_PERM_EXTRA) continue;
			if ((feat >= FEAT_SHOP_HEAD) &&
			    (feat <= FEAT_SHOP_TAIL)) continue;

			/* Convert square to dungeon floor */
			cave[*yy + y][*xx + x].feat = FEAT_FLOOR;
		}
	}

	/* Clear previous contents, add down stairs */
	cave[*yy][*xx].feat = FEAT_MORE;

	/* Vanilla town mode already has a wall... but normal towns don't */
	if (!vanilla_town)
	{
		/* Add an outer wall */
		add_town_wall();
	}
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
 */
static void town_gen(u16b town_num, int *xx, int *yy)
{
	int y, x;

	/* Place transparent area */
	for (y = 0; y < MAX_HGT; y++)
	{
		for (x = 0; x < MAX_WID; x++)
		{
			/* Create empty area */
			cave[y][x].feat = vanilla_town ? FEAT_PERM_EXTRA : FEAT_NONE;
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
			if (vanilla_town)
			{
				cave[y][x].feat = FEAT_FLOOR;

			}
			else
			{
				/* Create empty floor */
				cave[y][x].feat = FEAT_PEBBLES;

				/* Create Dirt */
				if (!rand_int(3))
				{
					cave[y][x].feat = FEAT_DIRT;
				}

				/* Create see-through terrain */
				else if (!rand_int(20))
				{
					cave[y][x].feat = FEAT_NONE;
				}
			}
		}
	}

	/* Build stuff */
	town_gen_hack(town_num, xx, yy);

	/* Town is now built */
	cur_town = town_num;	
	
	/* Hack -- use the "complex" RNG */
	Rand_quick = FALSE;
}

/*
 * Overlay a section of a town onto the wilderness
 */
static void overlay_town(int y, int x, u16b w_town, blk_ptr block_ptr)
{
	int i, j, xx, yy;
	byte feat;

	/* Find block to copy */
	xx = (x - town[w_town].x) << 4;
	yy = (y - town[w_town].y) << 4;

	/* copy 16x16 block from cave[][] */
	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			feat = cave[yy + j][xx + i].feat;

			if (feat != FEAT_NONE)
			{
				/* Only copy if there is something there. */
				block_ptr[j][i].feat = feat;
			}
		}
	}
}

/*
 * Look to see if a wilderness block is able to have
 * a town overlayed on top.
 */
static bool town_blank(int x, int y, int xsize, int ysize)
{
	int i, j;
	wild_done_type *w_ptr;

	for (i = x - 1; i < x + xsize + 2; i++)
	{
		for (j = y - 1; j < y + ysize + 2; j++)
		{
			/* Hack - Not next to boundary */
			if ((i <= 0) || (i >= max_wild - 1)
				 || (j <= 0) || (j >= max_wild - 1))
			{
				return (FALSE);
			}

			w_ptr = &wild[j][i].done;

			/* No town already */
			if (w_ptr->town) return (FALSE);

			/* No water or lava */
			if (w_ptr->info & (WILD_INFO_RIVER | WILD_INFO_LAVA)) return (FALSE);

			/* No Ocean */
			if (w_ptr->wild >= WILD_SEA) return (FALSE);
		}
	}

	/* Ok then */
	return (TRUE);
}




/*
 * Initialise the town structures
 * At the moment there is only one type of town generator.
 *
 */
static void init_towns(void)
{
	int i, j;
	int x, y, xx, yy;

	wild_done_type *w_ptr;

	/* Variables to pick "easiest" town. */
	u16b best_town, town_value;

	/* No towns yet */
	town_count = 1;

	best_town = 1;
	town_value = 256;

	/*
	 * Try to add max_towns towns.
	 */
	while (town_count < max_towns)
	{
		/* Get random position */
		x = randint(max_wild);
		y = randint(max_wild);

		/* See if space is free */
		if (!town_blank(x, y, TOWN_WID / 16 + 1, TOWN_HGT / 16 + 1)) continue;

		/* Add town */
		strcpy(town[town_count].name, "town");
		town[town_count].seed = rand_int(0x10000000);
		town[town_count].numstores = 9;
		town[town_count].type = 1;
		town[town_count].x = x;
		town[town_count].y = y;

		/* Place town on wilderness */
		for (j = 0; j < (TOWN_HGT / 16 + 1); j++)
		{
			for (i = 0; i < (TOWN_WID / 16 + 1); i++)
			{
				w_ptr = &wild[town[town_count].y + j][town[town_count].x + i].done;

				/* Select easiest town */
				if (w_ptr->mon_gen < town_value)
				{
					town_value = w_ptr->mon_gen;
					best_town = town_count;
				}

				/*
				 * Add town to wilderness
				 * Note: only 255 towns can be stored currently.
				 */
				w_ptr->town = (byte)town_count;

				/* Monsters are easy */
				w_ptr->mon_gen = 0;

				/* Monsters are fairly common */
				w_ptr->mon_prob = 64;
			}
		}

		/* Increase number of towns */
		town_count++;
	}

	/* Make the best town - and get the location of the stairs */
	town_gen(best_town, &xx, &yy);

	/* Hack - Reset player position to be on the stairs in town */
	p_ptr->wilderness_x = town[best_town].x * 16 + xx;
	p_ptr->wilderness_y = town[best_town].y * 16 + yy;
}

/* Place a single town in the middle of the tiny wilderness */
static void init_vanilla_town(void)
{
	int i, j, xx, yy;

	/* Only one town */
	strcpy(town[1].name, "town");
	town[1].seed = rand_int(0x10000000);
	town[1].numstores = 9;
	town[1].type = 1;
	town[1].x = (max_wild / 2) - TOWN_WID / (WILD_BLOCK_SIZE * 2) - 1;
	town[1].y = (max_wild / 2) - TOWN_HGT / (WILD_BLOCK_SIZE * 2) - 1;

	/* Place town on wilderness */
	for (j = 0; j < (TOWN_HGT / 16 + 1); j++)
	{
		for (i = 0; i < (TOWN_WID / 16 + 1); i++)
		{
			wild[town[1].y + j][town[1].x + i].done.town = 1;
		}
	}

	/* Make the town - and get the location of the stairs */
	town_gen(1, &xx, &yy);

	/* Hack - Reset player position to be on the stairs in town */
	p_ptr->wilderness_x = town[1].x * 16 + xx;
	p_ptr->wilderness_y = town[1].y * 16 + yy;

	/* One town + 1 for bounds*/
	town_count = 2;
}

/*
 * This section deals with wilderness generation
 * - both at the start of the game, and sorting
 * out what routines are used to make things as
 * the player moves around.
 *
 * Note that these routines return zero as a fail
 * indicator.  They return a non zero value for
 * a success - the value being the node last added
 * to the decision tree.  (This usually can be
 * ignored.)
 *
 */



/*
 * This function returns a wilderness block type that fits
 * the required parameters.
 *
 * The set of generation types is stored in a "decision tree"
 * - so the required time to get the wilderness type from the
 * three parameters (hgt,pop,law) is proportional to log(n).
 * This speeds up wilderness generation alot.  (Note the
 * "obvious" method of using a linear search to find matching
 * wilderness creation functions is too slow.
 *
 * The "type" value has two different uses.  One is to specify
 * which axis of the parameter space is being split.  The other
 * is to show whether or not a node is a terminal "leaf" node.
 * If it is a leaf node - the value returned is the number of
 * the type of wilderness generation function.
 */
static u16b get_gen_type(byte hgt, byte pop, byte law)
{
	/* Current node in choice tree - node zero is the "trunk" */
	int node = 0;

	/* branch of tree to go down */
	bool branch = TRUE;

	wild_choice_tree_type *tree_ptr;

	/* Find matching generation type */

	/* The while loop is used instead of the "obvious" recursion */
	while (1)
	{
		/* Access Node */
		tree_ptr = &wild_choice_tree[node];

		/* If are near end - look at leaves of tree
		 *
		 * (cutoff == 0) is used as a flag since it doesn't
		 * split the possibility tree in any useful way.
		 */
		if (tree_ptr->cutoff == 0)
		{
			/* randomly choose branch */
			if (randint(tree_ptr->chance1 + tree_ptr->chance2) >
			   tree_ptr->chance2)
			{
				/* Chance1 of going "left" */
				branch = TRUE;
			}
			else
			{
				/* Chance2 of going "right" */
				branch = FALSE;
			}
		}
		else
		{
			/*
			 * Get lower two bits of type to decribe which of
			 * (hgt,pop,law) cutoff refers to.
			 */

			switch(tree_ptr->info & 3)
			{
				case 1:
				{
					/* Look at height */
					if (tree_ptr->cutoff >= hgt)
					{
						branch = TRUE;
					}
					else
					{
						branch = FALSE;
					}

					break;
				}
				case 2:
				{
					/* Look at population */
					if (tree_ptr->cutoff >= pop)
					{
						branch = TRUE;
					}
					else
					{
						branch = FALSE;
					}

					break;
				}
				case 3:
				{
					/* Look at lawfulness */
					if (tree_ptr->cutoff >= law)
					{
						branch = TRUE;
					}
					else
					{
						branch = FALSE;
					}

					break;
				}
				default:
				{
					msg_print("Invalid stat chosen!");

					break;
				}
			}
		}


		/* Look at the proper branch of the tree */
		if (branch)
		{
			/* Go "left" */

			/* See if references leaf node */
			if (tree_ptr->info & 4)
			{
				/* If the bit is set - leaf */
				return (tree_ptr->ptrnode1);
			}
			else
			{
				/* use the while loop to recurse */
				node = tree_ptr->ptrnode1;
			}
		}
		else
		{
			/* Go "right" */

			/* See if references leaf node */
			if (tree_ptr->info & 8)
			{
				/* If the bit is set - leaf */
				return (tree_ptr->ptrnode2);
			}
			else
			{
				/* use the while loop to recurse */
				node = tree_ptr->ptrnode2;
			}
		}
	}
	
	/* For some dumb compilers */
	return(0);
}

/* The number of allocated nodes in the decsion tree */
static u16b d_tree_count;

/*
 * This function creates a new node on the decision tree.
 * It then connects that node to the node referenced by
 * the variable "node".  The process of making the link
 * stomps on any information about the old link.
 *
 * branch == TRUE is "left", FALSE is "right"
 *
 * This function returns the location of the new node in
 * the decision tree array.
 */
static u16b create_node(u16b node, bool branch)
{
	u16b new_node;

	wild_choice_tree_type	*tree_ptr;

	if (d_tree_count >= max_w_node)
	{
		/*
		 * Return zero (known as the location of the tree's
		 * "root" - so can act as a flag) if all of the
		 * memory allocated has been used.
		 *
		 * Always check the return value - and report the error
		 *
		 * The number of nodes required is roughly proportional
		 * to nlog(n) for a random input of ranges.
		 */
		 return (0);
	}

	/* Get location of new node */
	new_node = d_tree_count;

	/* Access old node */
	tree_ptr = &wild_choice_tree[node];

	if (branch)
	{
		/* Link new node to left of old */
		tree_ptr->ptrnode1 = new_node;

		/* Link is not to a leaf */
		tree_ptr->info &= ~4;
	}
	else
	{
		/* Link new node to right of old */
		tree_ptr->ptrnode2 = new_node;

		/* Link is not to a leaf */
		tree_ptr->info &= ~8;
	}

	/* Increase count of allocated nodes */
	d_tree_count++;

	return (new_node);
}

/*
 * This function deletes the last node on the decision tree.
 * It is needed for a major hack when something is being added
 * to a "null" region.
 *
 * This routine, and the above routine are the only ones that can
 * modify the number of nodes in the array.  (This makes checking
 * for the array-full case much easier.)
 */
static void delete_last_node(void)
{
	d_tree_count--;
}

/*
 * This function adds a node to the tree between a "leaf"
 * and the rest of the tree.  As nodes are added to the "leaf"
 * the chance of each wilderness generation type is collated.
 *
 * Note - this function used so that several different wilderness
 * generation types can exist within the same region in parameter
 * space.  Each possibility has a "chance" associated with it.
 * At generation time - the RNG is used to determine which node
 * in the "leaf" of several different generation types is used.
 * The wilderness generation type of that node is then used.
 *
 * This function also takes care of the case when something is
 * being added to a "null" node.  "Null" nodes of type zero
 * describe areas of parameter space that are outside the currently
 * used area.  This is needed because the decision tree starts out
 * empty.  As wilderness generation types are added, the null area
 * is slowly clipped away.  If at the end of creating the decision
 * tree and there is any "null" area left, the types do not fill
 * parameter space.  This will flag an error.
 */

static u16b add_node_chance(u16b type, u16b node, bool branch)
{
	/* Is the "leaf" a tree of nodes - or a single node. */
	bool is_tree;

	/* The node inserted into the decision tree */
	u16b new_node;
	wild_choice_tree_type	*tree_ptr;

	/* The old connection */
	u16b old_node;

	tree_ptr = &wild_choice_tree[node];

	if (branch)
	{
		old_node = tree_ptr->ptrnode1;

		/* Check for null case. */
		if (old_node == 0)
		{
			/* Easy - just replace with data */
		 	tree_ptr->ptrnode1 = type;

			/* Return current node. */
			return (node);
		}

		/* Get left leaf status */
		is_tree = (wild_choice_tree[old_node].info & 4);

	}
	else
	{
		old_node = tree_ptr->ptrnode2;

		/* Check for null case. */
		if (old_node == 0)
		{
			/* Easy - just replace with data */
		 	tree_ptr->ptrnode2 = type;

			/* Return current node. */
			return (node);
		}

		/* Get right leaf status */
		is_tree = (wild_choice_tree[old_node].info & 8);
	}

	/* Insert new node */
	new_node = create_node(node, branch);

	/* Error if array is full */
	if (new_node == 0)
	{
		/* Return zero as error code */
		return (0);
	}

	/* Access node */
	tree_ptr = &wild_choice_tree[new_node];

	/* Cutoff = 0 since is a leaf node */
	tree_ptr->cutoff = 0;

	/* Connect to old leaf */
	tree_ptr->ptrnode1 = old_node;

	/* Connect to new type */
	tree_ptr->ptrnode2 = type;


	if (is_tree)
	{
		/* Set "info" bit-flag */
		/* Only new node is a pointer to gen. type */
		tree_ptr->info = 8;

		/* Calculate the chance fields */
		tree_ptr->chance1 = wild_choice_tree[old_node].chance1 +
			wild_choice_tree[old_node].chance2;

		tree_ptr->chance2 = wild_gen_data[type].chance;
	}
	else
	{
		/* Set "info" bit-flag */
		/* Both links are to wild. gen. types. */
		tree_ptr->info = 8 + 4;

		/* Calculate the chance fields */
		tree_ptr->chance1 = wild_gen_data[old_node].chance;
		tree_ptr->chance2 = wild_gen_data[type].chance;
	}

	/* Return location of new node if required */
	return (new_node);
}


/*
 * This function copies the contents of one "leaf" (specified by
 * node1 + branch1) to the side of another node.
 *
 * This is needed because as the tree splits the parameter space
 * the leaves occupy regions.  When new wild. gen. types are added
 * to the decision tree - the "leaves" may not match their size.
 * This means that the leaves need to be split - in other words
 * copied.
 */
static u16b copy_branch(u16b node1, bool branch1, u16b node2, bool branch2)
{
	/* This function assumes that the "leaves" are of this form:
	*
	*StartNode
	* /  \
	*x  Node
	*    / \
	* type Node
	*       / \
	*    type Node
	*          / \
	*       type type
	*
	* (Where one pointer connects to a node, and one to a wild. gen. type)
	*/

	/*
	 * The complexity of this function is due to the large number of
	 * possibilities:  both branches can be left of right, and the node
	 * can be terminal or not.  This gives a set of nested "if's" resulting
	 * in eight small sections of code.
	 */
	u16b new_node;
	u16b temp_node;

	wild_choice_tree_type	*tree_ptr1;
	wild_choice_tree_type	*tree_ptr2;

	/* point to node to be copied from */
	tree_ptr1 = &wild_choice_tree[node1];

	/* work out what has to be copied. */
	if (branch1)
	{
		if (tree_ptr1->info & 4)
		{
			/* need to copy tree of nodes */

			/* make new node */
			new_node = create_node(node2, branch2);

			/* Exit on failure */
			if (new_node == 0) return (0);

			/* Point to block to copy */
			temp_node = tree_ptr1->ptrnode1;
			tree_ptr1 = &wild_choice_tree[temp_node];

			/* Point to new block */
			tree_ptr2 = &wild_choice_tree[new_node];

			/* Copy data to new node */
			tree_ptr2->info = tree_ptr1->info;
			tree_ptr2->cutoff = tree_ptr1->cutoff;
			tree_ptr2->chance1 = tree_ptr1->chance1;
			tree_ptr2->chance2 = tree_ptr1->chance2;
			tree_ptr2->ptrnode1 = tree_ptr1->ptrnode1;
			tree_ptr2->ptrnode2 = tree_ptr1->ptrnode2;

			/* Recurse along branches to this node */
			if (!(tree_ptr2->info & 4))
			{
				/* Recurse along "left" branch */
				if (copy_branch(temp_node, TRUE, new_node, TRUE) == 0)
					return (0);
			}

			if (!(tree_ptr2->info & 8))
			{
				/* Recurse along "right" branch */
				if (copy_branch(temp_node, TRUE, new_node, TRUE) == 0)
					return (0);
			}

			/* Done */
			return (new_node);
		}
		else
		{
			/* point to node to be copied to */
			tree_ptr2 = &wild_choice_tree[node2];

			/* only need to copy a single wild. gen. type */
			if (branch2)
			{
				/* terminal branch */
				tree_ptr2->info |= 4;

				/* Copy information */
				tree_ptr2->ptrnode1 = tree_ptr1->ptrnode1;
				tree_ptr2->chance1 = tree_ptr1->chance1;
			}
			else
			{
				/* terminal branch */
				tree_ptr2->info |= 8;

				/* Copy information */
				tree_ptr2->ptrnode2 = tree_ptr1->ptrnode1;
				tree_ptr2->chance2 = tree_ptr1->chance1;
			}

			/* done */
			return (node2);
		}
	}
	else
	{
		if (tree_ptr1->info & 8)
		{
			/* need to copy tree of nodes */

			/* make new node */
			new_node = create_node(node2, branch2);

			/* Exit on failure */
			if (new_node == 0) return (0);

			/* Point to block to copy */
			temp_node = tree_ptr1->ptrnode2;
			tree_ptr1 = &wild_choice_tree[temp_node];

			/* Point to new block */
			tree_ptr2 = &wild_choice_tree[new_node];

			/* Copy data to new node */
			tree_ptr2->info = tree_ptr1->info;
			tree_ptr2->cutoff = tree_ptr1->cutoff;
			tree_ptr2->chance1 = tree_ptr1->chance1;
			tree_ptr2->chance2 = tree_ptr1->chance2;
			tree_ptr2->ptrnode1 = tree_ptr1->ptrnode1;
			tree_ptr2->ptrnode2 = tree_ptr1->ptrnode2;

			/* Recurse along branches to this node */
			if (!(tree_ptr2->info & 4))
			{
				/* Recurse along "left" branch */
				if (copy_branch(temp_node, TRUE, new_node, TRUE) == 0)
					return (0);
			}

			if (!(tree_ptr2->info & 8))
			{
				/* Recurse along "right" branch */
				if (copy_branch(temp_node, TRUE, new_node, TRUE) == 0)
					return (0);
			}

			/* Done */
			return (new_node);
		}
		else
		{
			/* point to node to be copied to */
			tree_ptr2 = &wild_choice_tree[node2];

			/* only need to copy a single wild. gen. type */
			if (branch2)
			{
				/* terminal branch */
				tree_ptr2->info |= 4;

				/* Copy information */
				tree_ptr2->ptrnode1 = tree_ptr1->ptrnode2;
				tree_ptr2->chance1 = tree_ptr1->chance2;
			}
			else
			{
				/* terminal branch */
				tree_ptr2->info |= 8;

				/* Copy information */
				tree_ptr2->ptrnode2 = tree_ptr1->ptrnode2;
				tree_ptr2->chance2 = tree_ptr1->chance2;
			}

			/* done */
			return (node2);
		}
	}
}


/*
 * This function is used to add a wilderness generation type within another
 * typed region of parameter space described by the decision tree.  This is
 * the only function that actually extends the decision tree itself.  (The
 * add_node_chance() function increases the size of the "leaves" though.)
 *
 * The bounding box of the bigger region (number 1) is repeatedly clipped onto
 * the sides of the smaller region2.  This can result with up to 6 nodes being
 * used.  Finally, when the two regions are the same size, the add_node_chance()
 * function is called to extend the "leaves" of the decision tree.
 *
 * This function must be called with a new empty node.  The node must be connected
 * to the tree by the calling routine.  (This "feature" is so that this routine
 * can be used to initialise an empty decision tree.)  This means that the calling
 * routine must check for the "null" node + completely filled case. XXX XXX
 */

static u16b add_node_inside(u16b node, u16b type1, wild_bound_box_type *bound1,
	 u16b type2, wild_bound_box_type *bound2)
{
	/* The node inserted into the decision tree */
	u16b new_node;
	wild_choice_tree_type	*tree_ptr;

	tree_ptr = &wild_choice_tree[node];

	if (bound1->hgtmin != bound2->hgtmin)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->hgtmin;

		/* Excess is smaller than cutoff */
		tree_ptr->ptrnode1 = type1;

		/* Cutoff = hgt , ptrnode1 = wild. gen. type. */
		tree_ptr->info = 1 + 4;

		/* Wipe chance values  (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, FALSE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->hgtmax != bound2->hgtmax)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->hgtmax;

		/* Excess is larger than cutoff */
		tree_ptr->ptrnode2 = type1;

		/* Cutoff = hgt , ptrnode2 = wild. gen. type. */
		tree_ptr->info = 1 + 8;

		/* Wipe chance values  (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, TRUE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->popmin != bound2->popmin)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->popmin;

		/* Excess is smaller than cutoff */
		tree_ptr->ptrnode1 = type1;

		/* Cutoff = pop , ptrnode1 = wild. gen. type. */
		tree_ptr->info = 2 + 4;

		/* Wipe chance values  (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, FALSE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->popmax != bound2->popmax)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->popmax;

		/* Excess is larger than cutoff */
		tree_ptr->ptrnode2 = type1;

		/* Cutoff = pop , ptrnode2 = wild. gen. type. */
		tree_ptr->info = 2 + 8;

		/* Wipe chance values  (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, TRUE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->lawmin != bound2->lawmin)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->lawmin;

		/* Excess is smaller than cutoff */
		tree_ptr->ptrnode1 = type1;

		/* Cutoff = law , ptrnode1 = wild. gen. type. */
		tree_ptr->info = 3 + 4;

		/* Wipe chance values  (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, FALSE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->lawmax != bound2->lawmax)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->lawmax;

		/* Excess is larger than cutoff */
		tree_ptr->ptrnode2 = type1;

		/* Cutoff = law , ptrnode2 = wild. gen. type. */
		tree_ptr->info = 3 + 8;

		/* Wipe chance values  (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, TRUE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	/*
	 * "null" case - don't need the extra node.
	 * Hack - delete extra node and go up one (should be the last node on the
	 * array.)  XXX XXX XXX
	 * Once there - look for "null" type on _one_ branch.
	 * The other branch was previously a link to the now deleted node.
	 * Replace that link with the wilderness gen. type.
	 *
	 * This only works because we know that at least one node was added
	 * to the bottom of the array.  This is why this routine must never
	 * be called with a "null" region the same size as the region to be
	 * added.
	 */
	if (type1 == 0)
	{
		/* Delete last node on array - and move back one. */
		delete_last_node();
		node--;
		tree_ptr = &wild_choice_tree[node];

		/* look "left" for null */
		if (tree_ptr->ptrnode1 == 0)
		{
			/* Paranoia - check for both branches null */
			if (tree_ptr->ptrnode2 == 0) return (0);

			/* link to wild. gen. type. */
			tree_ptr->ptrnode2 = type2;

			/* right branch is to a wild. gen. type - not a node. */
			tree_ptr->info |= 8;

			/* Done */
			return (node);
		}

		/* look "right" for null */
		if (tree_ptr->ptrnode2 == 0)
		{
			/* Paranoia - check for both branches null */
			if (tree_ptr->ptrnode1 == 0) return (0);

			/* link to wild. gen. type. */
			tree_ptr->ptrnode1 = type2;

			/* left branch is to a wild. gen. type - not a node. */
			tree_ptr->info |= 4;

			/* Done */
			return (node);
		}
	}

	/*
	 * Have two wild. gen. types that want to be in the same region of
	 * parameter space.  This is accomedated by using the "chance" fields.
	 * chance1 of going "left", and chance2 of going "right".  This state
	 * is flagged by having cutoff == 0.
	 */

	/* Set flag for existance of "chance" fields. */
	tree_ptr->cutoff = 0;

	/* connect to wild. gen. types */
	tree_ptr->ptrnode1 = type1;
	tree_ptr->ptrnode2 = type2;

	/* Set info flag to show both branches are "leaves"*/
	tree_ptr->info = 8 + 4;

	/* Look up chances and add to node. */
	tree_ptr->chance1 = wild_gen_data[type1].chance;
	tree_ptr->chance2 = wild_gen_data[type2].chance;

	/* Done */
	return (node);
}

/*
 * This routine compares two bounding boxes and returns true if they are the same.
 */

static bool compare_bounds(wild_bound_box_type *bound1, wild_bound_box_type *bound2)
{
	return ((bound2->hgtmin == bound1->hgtmin) &&
		(bound2->hgtmax == bound1->hgtmax) &&

		(bound2->popmin == bound1->popmin) &&
		(bound2->popmax == bound1->popmax) &&

		(bound2->lawmin == bound1->lawmin) &&
		(bound2->lawmax == bound1->lawmax));
}

/*
 * This function adds a type within a leaf that has a bigger bounding box.
 * This means that the nodes containing the leaf are copied several times
 * until the bounding boxes match - and the node can be added to the leaf.
 *
 * This function is similar to the above one - except that the input is
 * node + branch rather than just node. (This is to simplify the copying
 * function.)
 */
static u16b inside_leaf(u16b node, u16b type, wild_bound_box_type *bound1,
	 wild_bound_box_type *bound2, bool branch)
{


	/* The node inserted into the decision tree */
	u16b new_node;
	u16b branch_node;
	wild_choice_tree_type	*tree_ptr;

	tree_ptr = &wild_choice_tree[node];

	if (bound1->hgtmin != bound2->hgtmin)
	{
		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;

		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->hgtmin;

		/* Cutoff = hgt */
		tree_ptr->info = 1;

		/* work out branch to follow */
		branch = FALSE;
	}

	if (bound1->hgtmax != bound2->hgtmax)
	{

		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;

		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->hgtmax;

		/* Cutoff = hgt */
		tree_ptr->info = 1;

		/* work out branch to follow */
		branch = TRUE;
	}

	if (bound1->popmin != bound2->popmin)
	{

		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;

		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->popmin;

		/* Cutoff = pop */
		tree_ptr->info = 2;

		/* work out branch to follow */
		branch = FALSE;
	}

	if (bound1->popmax != bound2->popmax)
	{

		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;

		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->popmax;

		/* Cutoff = pop */
		tree_ptr->info = 2;

		/* work out branch to follow */
		branch = TRUE;
	}

	if (bound1->lawmin != bound2->lawmin)
	{

		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;

		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->lawmin;

		/* Cutoff = law */
		tree_ptr->info = 3;

		/* work out branch to follow */
		branch = FALSE;
	}

	if (bound1->lawmax != bound2->lawmax)
	{

		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;

		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->lawmax;

		/* Cutoff = law */
		tree_ptr->info = 3;

		/* work out branch to follow */
		branch = TRUE;
	}

	/* Finally - merge wild. gen. type with leaf of the same size */
	return (add_node_chance(type, node, branch));
}


/*
 * This function copies the parameter bounds from one variable to another.
 */
static void copy_bounds(wild_bound_box_type *bound1,
	 wild_bound_box_type *bound2)
{
	bound2->hgtmin = bound1->hgtmin;
	bound2->hgtmax = bound1->hgtmax;

	bound2->popmin = bound1->popmin;
	bound2->popmax = bound1->popmax;

	bound2->lawmin = bound1->lawmin;
	bound2->lawmax = bound1->lawmax;
}


/*
 * Add a wilderness generation function to the decision tree.
 *
 * There are many special cases to take care of here.  First the
 * current tree is followed until the required region either
 * 1) Is split
 * 2) Is subsumed inside a "leaf" node.
 * 3) Takes over a "null" node.
 *
 * Note: Null nodes exist because no generation routine covers the
 * whole parameter space.  This means that the inital state of the
 * decision tree does not cover every case.  Therefore, as nodes are
 * added - checks are made to see if the region falls ouside of the
 * current "reach" of the tree.
 */
static u16b add_node(wild_bound_box_type *bound,
	 wild_bound_box_type *cur_bound, u16b type, u16b node)
{

	/*
	 * Temp storage of the current bounds and current type bounds
	 * (Used in splitting a region that overlaps a cutoff)
	 */
	wild_bound_box_type temp_bound1;
	wild_bound_box_type temp_bound2;

	u16b oldnode = node;

	bool branch = FALSE;

	wild_choice_tree_type	*tree_ptr;
	/* Scan tree until hit a leaf or split required region */

	/* Use a while loop instead of recursion to follow tree */
	while (1)
	{
		/* Access Node */
		tree_ptr = &wild_choice_tree[node];

		/* If are near end - look at leaves of tree
		 *
		 * (cutoff == 0) is used as a flag since it doesn't
		 * split the possibility tree in any useful way.
		 */
		if (tree_ptr->cutoff == 0)
		{
			/* leaf node */
			return (inside_leaf(oldnode, type, bound, cur_bound, branch));
		}
		else
		{
			/*
			 * Get lower two bits of type to decribe which of
			 * (hgt,pop,law) cutoff refers to.
			 */

			switch(tree_ptr->info & 3)
			{
				case 1:
				{
					/* Look at height */
					if (tree_ptr->cutoff >= bound->hgtmax)
					{
						branch = TRUE;

						cur_bound->hgtmax = tree_ptr->cutoff;
					}
					else if (tree_ptr->cutoff <= bound->hgtmin)
					{
						branch = FALSE;

						cur_bound->hgtmin = tree_ptr->cutoff;
					}
					else
					{
						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* upper bound = cutoff */
						temp_bound1.hgtmax = tree_ptr->cutoff;
						temp_bound2.hgtmax = tree_ptr->cutoff;

						/* rescan with smaller domain */
						if (!add_node(&temp_bound1, &temp_bound2, type, node))
							return (0);

						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* lower bound = cutoff */
						temp_bound1.hgtmin = tree_ptr->cutoff;
						temp_bound2.hgtmin = tree_ptr->cutoff;

						/* rescan with smaller domain */
						return (add_node(&temp_bound1, &temp_bound2, type, node));
					}
					break;
				}
				case 2:
				{
					/* Look at population */
					if (tree_ptr->cutoff >= bound->popmax)
					{
						branch = TRUE;

						cur_bound->popmax = tree_ptr->cutoff;
					}
					else if (tree_ptr->cutoff <= bound->popmin)
					{
						branch = FALSE;

						cur_bound->popmin = tree_ptr->cutoff;
					}
					else
					{
						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* upper bound = cutoff */
						temp_bound1.popmax = tree_ptr->cutoff;
						temp_bound2.popmax = tree_ptr->cutoff;

						/* rescan with smaller domain */
						if (!add_node(&temp_bound1, &temp_bound2, type, node))
							return (0);

						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* lower bound = cutoff */
						temp_bound1.popmin = tree_ptr->cutoff;
						temp_bound2.popmin = tree_ptr->cutoff;

						/* rescan with smaller domain */
						return (add_node(&temp_bound1, &temp_bound2, type, node));
					}
					break;
				}
				case 3:
				{
					/* Look at lawfulness */
					if (tree_ptr->cutoff >= bound->lawmax)
					{
						branch = TRUE;

						cur_bound->lawmax = tree_ptr->cutoff;
					}
					else if (tree_ptr->cutoff <= bound->lawmin)
					{
						branch = FALSE;

						cur_bound->lawmin = tree_ptr->cutoff;
					}
					else
					{
						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* upper bound = cutoff */
						temp_bound1.lawmax = tree_ptr->cutoff;
						temp_bound2.lawmax = tree_ptr->cutoff;

						/* rescan with smaller domain */
						if (!add_node(&temp_bound1, &temp_bound2, type, node))
							return (0);

						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* lower bound = cutoff */
						temp_bound1.lawmin = tree_ptr->cutoff;
						temp_bound2.lawmin = tree_ptr->cutoff;

						/* rescan with smaller domain */
						return (add_node(&temp_bound1, &temp_bound2, type, node));
					}
					break;
				}
				default:
				{
					msg_format("Info - %d", tree_ptr->info);
					msg_print("Invalid stat chosen!");

					break;
				}
			}
		}


		/* Look at the proper branch of the tree */
		if (branch)
		{
			/* Go "left" */

			/* See if references leaf node */
			if (tree_ptr->info & 4)
			{
				/* Hit leaf node */

				/* store connection */
				oldnode = tree_ptr->ptrnode1;

				/* Take care of null case */
				if ((oldnode == 0) && compare_bounds(cur_bound, bound))
				{
					/* simply set the branch to point to the wild. gen. type */
					tree_ptr->ptrnode1 = type;

					/* done - don't return zero as can happen with the root node */
					return (1);
				}

				/* Make new node */
				node = create_node(node, TRUE);
				if (node == 0) return (0);

				return (add_node_inside(node, oldnode, cur_bound, type, bound));
			}
			else
			{
				/* use the while loop to recurse */

				oldnode = node;
				node = tree_ptr->ptrnode1;
			}
		}
		else
		{
			/* Go "right" */

			/* See if references leaf node */
			if (tree_ptr->info & 8)
			{
				/* Hit leaf node */

				/* store connection */
				oldnode = tree_ptr->ptrnode2;

				/* Take care of null case */
				if ((oldnode == 0) && compare_bounds(cur_bound, bound))
				{
					/* simply set the branch to point to the wild. gen. type */
					tree_ptr->ptrnode2 = type;

					/* done - don't return zero as can happen with the root node */
					return (1);
				}

				/* Make new node */
				node = create_node(node, FALSE);
				if (node == 0) return (0);

				return (add_node_inside(node, oldnode, cur_bound, type, bound));
			}
			else
			{
				/* use the while loop to recurse */
				oldnode = node;
				node = tree_ptr->ptrnode2;
			}
		}
	}
}

/*
 * Initialise the decision tree with the first wilderness generation type.
 */

u16b init_choice_tree(wild_bound_box_type *bound, u16b type)
{
	wild_bound_box_type start_bounds;

	/* The decision tree has one (empty) node */
	d_tree_count = 1;

	/*
	 * Set the starting bounds of the decision tree - this covers
	 * the whole parameter space used by the wilderness generation
	 * types.
	 */
	start_bounds.hgtmin = 0;
	start_bounds.hgtmax = 255;

	start_bounds.popmin = 0;
	start_bounds.popmax = 255;

	start_bounds.lawmin = 0;
	start_bounds.lawmax = 255;

	/* Assume first node is cleared by C_MAKE */

	/*
	 * Start the tree off by adding the type within a "null" region covering
	 * the whole parameter space.  (Note this routine requires one empty node.
	 * - that is why d_tree_count starts out as one.)
	 */
	return (add_node_inside(0, 0, &start_bounds, type, bound));
}


u16b add_node_tree_root(wild_bound_box_type *bound, u16b type)
{
	/* default bounds */
	wild_bound_box_type start_bounds;

	start_bounds.hgtmin = 0;
	start_bounds.hgtmax = 255;

	start_bounds.popmin = 0;
	start_bounds.popmax = 255;

	start_bounds.lawmin = 0;
	start_bounds.lawmax = 255;

	/* Add to root of tree */
	return (add_node(bound, &start_bounds, type, 0));
}


/* Testing code - remove later. */
void test_decision_tree(void)
{
	byte hgt, pop, law;

	u16b type;

	/* get parameters */
	msg_print("Type in hgt");

	hgt = (byte)get_quantity(NULL, 255);

	msg_print("Type in pop");

	pop = (byte)get_quantity(NULL, 255);

	msg_print("Type in law");

	law = (byte)get_quantity(NULL, 255);

	/* Get value from decision tree */
	type = get_gen_type(hgt, pop, law);

	msg_format("Type returned: %d .", type);
}

#if 0
/*
 * "Testing" function, used to find where the "invisible monster" bug
 * is being caused.
 * This tests the wilderness to see if everything is ok in the monster-
 * wilderness data structures.
 */
void test_mon_wild_integrity(void)
{
	int i, j;
	cave_type *c_ptr;
	monster_type *m_ptr;

	/* Only when in wilderness */
	if (dun_level) return;

	/* Check the wilderness */
	for (i = min_wid; i < max_wid; i++)
	{
		for (j = min_hgt; j < max_hgt; j++)
		{
			/* Point to location */
			c_ptr = area(j, i);

			/* Want a monster */
			if (!c_ptr->m_idx) continue;

			m_ptr = &m_list[c_ptr->m_idx];

			/* Dead monster? */
			if (!m_ptr->r_idx)
			{
				msg_print("Dead Monster");
			}

			if (c_ptr->m_idx > m_max)
			{
				msg_print("Monster index inconsistancy.");
			}

			if ((m_ptr->fy != j) || (m_ptr->fx != i))
			{
				msg_print("Monster location inconsistancy.");
				msg_format("Monster x, cave x,%d,%d",m_ptr->fx, i);
				msg_format("Monster y, cave y,%d,%d",m_ptr->fy, j);
			}
		}
	}
}
#endif /* 0 */

/*
 * Test to see that there are no null nodes in the decision tree.
 */
static void test_wild_data(void)
{
	int i;

	for (i=0; i < d_tree_count; i++)
	{
		if ((wild_choice_tree[i].ptrnode1 == 0) ||
			(wild_choice_tree[i].ptrnode2 == 0))
		{
			msg_format("Missing value at %d ", i);
			msg_format("Cutoff %d ", wild_choice_tree[i].cutoff);

			/*
			 * The "missing value" will be close to the error in
			 * w_info.txt
			 *
			 * The cutoff provides a hint as to the lawmax value of
			 * the error.  (Note - if this is zero - the error is
			 * in a "leaf" and is a bug in the code.)
			 */
		}
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
static void clear_temp_block(void)
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
static void set_temp_corner_val(u16b val)
{
	temp_block[0][0] = val;
	temp_block[0][WILD_BLOCK_SIZE] = val;
	temp_block[WILD_BLOCK_SIZE][0] = val;
	temp_block[WILD_BLOCK_SIZE][WILD_BLOCK_SIZE] = val;
}

/* Set the middle of the temporary block to val */
static void set_temp_mid(u16b val)
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
		else
		{
			/* If out of bounds - use middle square */

			/* Check flag status */
			if (wild[y][x].done.info & info)
			{
				/* Flag is set */
				grad1[i] = TRUE;
				any = TRUE;
			}
		}
	}

	/* Exit if there are no set flags */
	if (any == FALSE) return (FALSE);

	/* Clear temporary block */
	clear_temp_block();

	/* Set grad2[] depending on values of grad1[] */

	/* If center is set - just copy */
	if (grad1[5])
	{
		for (i = 1; i < 10; i++)
		{
			grad2[i] = grad1[i];
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

static void frac_block(void)
{
	/*
	 * fixed point variables- these are stored as 256 x normal value
	 * this gives 8 binary places of fractional part + 8 places of normal part
	 */

	u16b lstep, hstep, i, j, ii, jj, size;

	/* Size is one bigger than normal blocks for speed of algorithm with 2^n + 1 */
	size = WILD_BLOCK_SIZE;

	/* Initialize the step sizes */
	lstep = hstep = size * 256;
	size = size * 256;

	/*
	 * Fill in the square with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while (hstep > 256)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* cache values of i,j divided by 256 */
				ii = i >> 8;
				jj = j >> 8;

				/* only write to points that are "blank" */
				if (temp_block[jj][ii] == MAX_SHORT)
				{
					/* Average of left and right points +random bit */
					temp_block[jj][ii] =
					((temp_block[jj][(i - hstep) >> 8] +
					temp_block[jj][(i + hstep) >>8]) >> 1) +
					((randint(lstep) - hstep) >> 1);
				}
			}
		}


		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
		   	{
				/* cache values of i,j / 256 */
				ii = i >> 8;
				jj = j >> 8;

				/* only write to points that are "blank" */
				if (temp_block[jj][ii] == MAX_SHORT)
				{
					/* Average of up and down points +random bit */
					temp_block[jj][ii] =
					((temp_block[(j - hstep) >> 8][ii]
					+ temp_block[(j + hstep) >> 8][ii]) >> 1)
					+ ((randint(lstep) - hstep) >> 1);
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
			   	/* cache values of i,j / 256 */
				ii = i >> 8;
				jj = j >> 8;

				/* only write to points that are "blank" */
				if (temp_block[jj][ii] == MAX_SHORT)
				{
					/* average over all four corners + scale by 181 to
					 * reduce the effect of the square grid on the shape of the fractal */
					temp_block[jj][ii] =
					((temp_block[(j - hstep) >> 8][(i - hstep) >> 8]
					+ temp_block[(j + hstep) >> 8][(i - hstep) >> 8]
					+ temp_block[(j - hstep) >> 8][(i + hstep) >> 8]
					+ temp_block[(j + hstep) >> 8][(i + hstep) >> 8]) >> 2)
					+ (((randint(lstep) - hstep) * 181) >> 8);
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
			c1 = 0x1000000 / abs((long)prob1 - prob);
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
			c2 = 0x1000000 / abs((long) prob2 - prob);
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
			c3 = 0x1000000 / abs((long) prob3 - prob);
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
			c4 = 0x1000000 / abs((long) prob4 - prob);
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

#if 0

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
	int i, j, ii, jj;
	cave_type *c_ptr;

	/* Only draw if road is on the sqaure */
	if (!(wild[y][x].done.info & (WILD_INFO_TRACK | WILD_INFO_ROAD))) return;

	/* Clear the temporary block */
	clear_temp_block();

	/* Set the corner + side + middle values depending on road status */
	for (i = x - 1; i <= x + 1; i++)
	{
		for (j = y - 1; j <= y + 1; j++)
		{
			ii = ((1 + i - x) * WILD_BLOCK_SIZE) / 2;
			jj = ((1 + j - y) * WILD_BLOCK_SIZE) / 2;

			temp_block[jj][ii] = WILD_BLOCK_SIZE * 60;

			/* Bounds checking */
			if ((i < 0) || (j < 0) || (i >= max_wild) || (j >= max_wild)) continue;

			/* Is it a track? */
			if (wild[j][i].done.info & WILD_INFO_TRACK)
			{
				temp_block[jj][ii] = WILD_BLOCK_SIZE * 180;
			}

			/* Is it a road? */
			if (wild[j][i].done.info & WILD_INFO_ROAD)
			{
				temp_block[jj][ii] = WILD_BLOCK_SIZE * 210;
			}
		}
	}

	/* Build the fractal */
	frac_block();

	/* Copy the result over the block */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Is it a road square? */
			if (temp_block[j][i] >= WILD_BLOCK_SIZE * 160)
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
					if (rand_int(3))
					{
						c_ptr->feat = FEAT_DIRT;
					}
					else
					{
						c_ptr->feat = FEAT_PEBBLES;
					}
				}
			}
		}
	}
}

#endif /* 0 */

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
				if (randint(100) < 75)
				{
					block_ptr[j][i].feat = feat2;
				}
				else
				{
					block_ptr[j][i].feat = feat1;
				}
			}
			else if (temp_block[j][i] >= WILD_BLOCK_SIZE * 128)
			{
				/* 25% of the time use the other tile : it looks better this way */
				if (randint(100) < 75)
				{
					block_ptr[j][i].feat = feat1;
				}
				else
				{
					block_ptr[j][i].feat = feat2;
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
				if (rand_int(chance + 1)) break;

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

			if ((element < WILD_BLOCK_SIZE * 171) && (rand_int(2) == 1))
			{
				/* Outermost terrain */
				block_ptr[j][i].feat = data[1];
				continue;
			}

			if ((element < WILD_BLOCK_SIZE * 213) && (rand_int(2) == 1))
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

void blend_helper(cave_type *c_ptr, byte *data,int g_type)
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

	/* Initialise temporary block */
	clear_temp_block();

	/* Boundary is at half probability */
	set_temp_corner_val(WILD_BLOCK_SIZE * 128);

	/* This is the "full" value so that the center of the block stays as normal */
	set_temp_mid(WILD_BLOCK_SIZE * 256);

	/* Generate plasma factal */
	frac_block();

	/* Blend based on height map */
	for (j = 0; j < WILD_BLOCK_SIZE; j++)
	{
		for (i = 0; i < WILD_BLOCK_SIZE; i++)
		{
			/* Chance to blend is based on element in fractal */
			if (rand_int(WILD_BLOCK_SIZE * 256) > temp_block[j][i]) continue;

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

			/* The sea doesn't blend. (Use rivers) */
			if (w_type >= WILD_SEA) continue;

			/* If adjacent type is the same as this one - don't blend */
			if (w_type == type) continue;

			/* Blend with generation type specified by gen_routine */
			blend_helper(&block_ptr[j][i], wild_gen_data[w_type].data,
				 wild_gen_data[w_type].gen_routine);
		}
	}
}

/* Make the specified terrain type at a wilderness block */
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
 * Fill the block with perm. wall  This is only used by the vanilla town option.
 */
static void fill_perm_wall(blk_ptr block_ptr)
{
	int i, j;
	/* Overlay the block with grass */
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
			if (!rand_int(prob))
			{
				if (rand_int(2))
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

/* Add monsters to the boundary of the visible grid. */
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
			if (distance(px / 16, py / 16,
				 x + wild_grid.x, y + wild_grid.y) < 3) continue;

			/* Set the monster generation level */

			/* Hack - use a number based on "law" statistic */
			monster_level = wild[y + wild_grid.y][x + wild_grid.x].done.mon_gen;

			/* Add monsters to block */
			add_monsters_block(x + wild_grid.x, y + wild_grid.y);
		}
	}
}


/* Add monsters to the wilderness in all blocks */
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

/* Make a new block based on the terrain type */
static void gen_block(int x, int y, blk_ptr block_ptr)
{
	u16b w_town, w_type;
	int dummy1, dummy2;

	/*
	 * XXX XXX Later - most of this will be table driven.
	 */

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

	/* Hack -Check for the vanilla town grass option. */
	else if (w_type == 0)
	{
		/* Fill the block with grass */
		fill_perm_wall(block_ptr);
	}
	else
	{
		/* Make terrain based on wilderness generation type */
		gen_block_helper(block_ptr, wild_gen_data[w_type].data,
			wild_gen_data[w_type].gen_routine);

		/* Blend with adjacent terrains */
		blend_block(x, y, block_ptr, w_type);

		/* Add water boundary effects.  (Rivers / Ocean) */
		if (wild_info_bounds(x, y, WILD_INFO_RIVER))
		{
			/* Hack, above function sets bounds */

			/* Generate plasma factal */
			frac_block();

			/* Overlay water */
			wild_add_gradient(block_ptr, FEAT_SHAL_WATER, FEAT_DEEP_WATER);
		}

		/* Add roads */
#if 0
		make_wild_road(block_ptr, x, y);
#endif /* 0 */

		/* Add lava (Not Done) */
	}
	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	/* Overlay town */
	w_town = wild[y][x].done.town;

	/* Is there a town? */
	if (w_town)
	{
		/* Is it the right town? */
		if (cur_town != w_town)
		{
			/* Make the town */
			town_gen(w_town, &dummy1, &dummy2);

			init_buildings();
		}

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
static void init_wild_cache(void)
{
	int x, y;

	/* Allocate blocks around player */
	for (x = 0; x < WILD_GRID_SIZE; x++)
	{
		for (y = 0; y < WILD_GRID_SIZE; y++)
		{
			/* Link to the grid */
			wild_grid.block_ptr[y][x] = wild_cache[x  + WILD_GRID_SIZE * y];
		}
	}
}


/* Allocate all grids around player */
static void allocate_all(void)
{
	u16b x, y;
	blk_ptr block_ptr;

	/* Allocate blocks around player */
	for (x = 0; x < WILD_GRID_SIZE; x++)
	{
		for (y = 0; y < WILD_GRID_SIZE; y++)
		{
			/* The block to use */
			block_ptr = wild_cache[x  + WILD_GRID_SIZE * y];

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
 * section of the wilderness by 16 units.  This is
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
			wild_grid.block_ptr[j - 1][i] =
				wild_grid.block_ptr[j][i];
		}

		/* Connect new grid to wilderness */
		wild_grid.block_ptr[WILD_GRID_SIZE - 1][i] = block_ptr;

		/* Make the new block */
		gen_block(i + wild_grid.x,
			WILD_GRID_SIZE - 1 + wild_grid.y, block_ptr);
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
			wild_grid.block_ptr[j][i] =
				wild_grid.block_ptr[j - 1][i];
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
			wild_grid.block_ptr[j][i - 1] =
				wild_grid.block_ptr[j][i];
		}

		/* Connect new grid to wilderness */
		wild_grid.block_ptr[j][WILD_GRID_SIZE - 1] = block_ptr;

		/* Make the new block */
		gen_block(WILD_GRID_SIZE - 1 + wild_grid.x,
			j + wild_grid.y, block_ptr);
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
			wild_grid.block_ptr[j][i] =
				wild_grid.block_ptr[j][i - 1];
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
	x = ((u16b) p_ptr->wilderness_x>>4);
	y = ((u16b) p_ptr->wilderness_y>>4);

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
	p_ptr->town_num = wild[p_ptr->wilderness_y >> 4]
		[p_ptr->wilderness_x >> 4].done.town;

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
	if (abs(dy) == 1)
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
	if (abs(dx) == 1)
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


#if 0

typedef struct road_type road_type;
struct road_type
{
	/* location of point */
	s16b x;
	s16b y;

	/* Number of connections */
	byte connect;

	coord con_pts[4];
};


static road_type *road_pt;


/*
 * Actually add a road to the wilderness.
 * The type of road depends on the toughness
 * of the monsters nearby.
 */
static void add_road_wild(s32b ny, s32b nx)
{
	/* Add the road to the wilderness */
	if (wild[ny][nx].done.mon_gen > 32)
	{
		/* Make a track */
		wild[ny][nx].done.info |= WILD_INFO_TRACK;
	}
	else
	{
		/* Make a road */
		wild[ny][nx].done.info |= WILD_INFO_ROAD;
	}
}

static void create_roads(void)
{
	/* Number of iterations to do */
	u32b n = max_wild * max_wild / 20;

	/* Save maximum number of points possible */
	u32b points = n;

	bool flag;

	s16b i, j, best_point;

	s32b x, y;
	s32b ny, nx, dx, dy;

	s32b dist, min_dist;

	road_type *r_ptr, *j_ptr, *n_ptr;

	/* Current maximum number of points in the road point array */
	u16b road_pt_max = 0;

	/* Make Road Array */
	C_MAKE(road_pt, points, road_type);

	/* Create starting points from towns */
	for (i = 1; i < town_count; i++)
	{
		/*
		 * This is a huge hack now.
		 * Later - this should look up the town's type
		 * to see how big it is, and where the entrances
		 * are.
		 */

		x = town[i].x;
		y = town[i].y;

		/* Towns are two wide, and four high */

		/* Get new point */
		r_ptr = &road_pt[road_pt_max];

		switch (rand_int(4))
		{
			case 0:
				r_ptr->x = x;
				r_ptr->y = y;
			break;
			case 1:
				r_ptr->x = x + 1;
				r_ptr->y = y + 1;
			break;
			case 2:
				r_ptr->x = x + 3;
				r_ptr->y = y + 1;
			break;
			case 3:
				r_ptr->x = x + 2;
				r_ptr->y = y;
			break;
		}

		r_ptr->connect = 0;

		/* Add the extra point */
		road_pt_max++;
	}

	/* The main loop */
	while ((road_pt_max > 5) && (n > 0))
	{
		/* Decrement counter */
		n--;

		/*
		 * Pick 6 points, with the chance of keeping
		 * a point dependant on how many connections it has.
		 * (A quick way of picking a "good" point.)
		 */

		/* Get a random starting point */
		r_ptr = &road_pt[rand_int(road_pt_max)];

		for (i = 0; i < 5; i++)
		{
			/* Get a new point */
			j_ptr =  &road_pt[rand_int(road_pt_max)];

			/* Chance to keep based on # of connections */
			if (j_ptr->connect < randint(6)) r_ptr = j_ptr;
		}

 		/* Initialise variables */
		min_dist = max_wild * max_wild * 2;
		best_point = -1;

		/* Find the closest point to the chosen one. */
		for (i = 0; i < road_pt_max; i++)
		{
			/* Get a new point */
			j_ptr =  &road_pt[i];

			/* Not same point as chosen one. */
			if (j_ptr == r_ptr) continue;

			x = j_ptr->x - r_ptr->x;
			y = j_ptr->y - r_ptr->y;

			dist = x*x + y*y;

			/* Is it closer? */
			if (dist < min_dist)
			{
				flag = TRUE;

				if (r_ptr->connect)
				{
					/* It is already connected to this point? */
					for (j = 0; j < r_ptr->connect; j++)
					{
						if ((j_ptr->x == r_ptr->con_pts[j].x) &&
							(j_ptr->y == r_ptr->con_pts[j].y))
						{
							flag = FALSE;
						}
					}
				}

				/* See if both are at the same town */
				if ((wild[r_ptr->y][r_ptr->x].done.town) &&
					((wild[j_ptr->y][j_ptr->x].done.town) ==
					 (wild[r_ptr->y][r_ptr->x].done.town)))
				{
					flag = FALSE;
				}

				/* If not connected, record it. */
				if (flag)
				{
					min_dist = dist;
					best_point = i;
				}
			}
		}

		/* Paranioa */
		if (best_point == -1) continue;

		/* Point to best point */
		j_ptr = &road_pt[best_point];

		/* If is close, connect the two points */
		if (min_dist < 400)
		{
			/* Recalculate distance exactly */
			dist = distance(j_ptr->y, j_ptr->x, r_ptr->y, r_ptr->x);

			/* Connect by line */
			for (i = 1; i < dist; i++)
			{
				x = j_ptr->x + i * (r_ptr->x - j_ptr->x) / dist;
				y = j_ptr->y + i * (r_ptr->y - j_ptr->y) / dist;

				/* Add the road to the wilderness */
				add_road_wild(y, x);
			}

			/* Add connection information */
			i = j_ptr->connect;

			j_ptr->con_pts[i].x = r_ptr->x;
			j_ptr->con_pts[i].y = r_ptr->y;
			j_ptr->connect++;

			i = r_ptr->connect;

			r_ptr->con_pts[i].x = j_ptr->x;
			r_ptr->con_pts[i].y = j_ptr->y;
			r_ptr->connect++;

			/*
			 * Chance to remove points based on # of connections.
			 * Note that we have to check to see if the points are
			 * last in the list because that is a special case.
			 * (Making the second pointer invalid is bad.)
			 */

			/* See if want to delete j_ptr */
			if (((!wild[j_ptr->y][j_ptr->x].done.town)
				 && (j_ptr->connect > rand_int(3) + 1)) || (j_ptr->connect >= 4))
			{
				/* Look for special case */
				if (r_ptr == &road_pt[road_pt_max - 1])
				{
					/* Structure Copy */
					*j_ptr = *r_ptr;

					/* move r_ptr */
					r_ptr = j_ptr;

				}
				else
				{
					/* Structure Copy */
					*j_ptr = road_pt[road_pt_max - 1];
				}

				/* Decrease maximum */
				road_pt_max--;
			}

			/* See if want to delete r_ptr */
			if (((!wild[r_ptr->y][r_ptr->x].done.town)
				 && (r_ptr->connect > rand_int(3) + 1)) || (r_ptr->connect >= 4))
			{
				/* Structure Copy */
				*r_ptr = road_pt[road_pt_max - 1];

				/* decrease maximum */
				road_pt_max--;
			}

			/* Done for this set */
			continue;
		}

		/* Hack to reduce overdraw */
		/*if (min_dist * 2 > n) continue;*/


		/* Get shifts */
		dx = abs(r_ptr->x - j_ptr->x);
		dy = abs(r_ptr->y - j_ptr->y);

		if (dx > 0)
		{
			ny = randint(dx) - dx / 2;
		}
		else
		{
			ny = 0;
		}

		if (dy > 0)
		{
			nx = randint(dy) - dy / 2;
		}
		else
		{
			nx = 0;
		}

		/* Get new point */
		nx += (r_ptr->x + j_ptr->x)/2;
		ny += (r_ptr->y + j_ptr->y)/2;

		/* Bounds checking */
		if (nx < 0) nx = 0;
		if (nx >= max_wild) nx = max_wild - 1;
		if (ny < 0) ny = 0;
		if (ny >= max_wild) ny = max_wild - 1;

		/* Inside sea? */
		if (wild[ny][nx].done.wild >= WILD_SEA) continue;

		/* Inside river? */
		if (wild[ny][nx].done.info & WILD_INFO_RIVER) continue;

		/* Chance to add based on strength of monsters */
		/*if (wild[ny][nx].done.mon_gen > (randint(40) + 20)) continue;*/

		/* write to point */

		/*
		 * If the list is too big,
		 *  pick a the point to replace randomly.
		 */
		if (road_pt_max >= points)
		{
			/* Get a random point in the list */
			n_ptr = &road_pt[rand_int(road_pt_max)];

			while ((n_ptr == r_ptr) || (n_ptr == j_ptr))
			{
				/* Get a random point in the list */
				n_ptr = &road_pt[rand_int(road_pt_max)];
			}
		}
		else
		{
			/* Expand list, and get new point on end */
			n_ptr = &road_pt[road_pt_max];

			road_pt_max++;
		}

		/* Add the point */
		n_ptr->x = nx;
		n_ptr->y = ny;
		n_ptr->connect = 0;

		/* Add the road to the wilderness */
		add_road_wild(ny, nx);
	}

	/* Done with array */
	C_KILL(road_pt, points, road_type);
}

#endif /* 0 */

/*
 * Sorting hook -- comp function -- by "wilderness height"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by the value in wild[y][x].gen.hgt_map
 */
static bool ang_sort_comp_height(vptr u, vptr v, int a, int b)
{
	s16b *x = (s16b*)(u);
	s16b *y = (s16b*)(v);

	int ha, hb;

	/* Get heights */
	ha = wild[y[a]][x[a]].gen.hgt_map;
	hb = wild[y[b]][x[b]].gen.hgt_map;

	/* Compare them */
	return (ha >= hb);
}

/*
 * Sorting hook -- swap function -- by "wilderness height"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by the value in wild[y][x].gen.hgt_map
 */
static void ang_sort_swap_height(vptr u, vptr v, int a, int b)
{
	s16b *x = (s16b*)(u);
	s16b *y = (s16b*)(v);

	s16b temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;

	/* Swap "y" */
	temp = y[a];
	y[a] = y[b];
	y[b] = temp;
}

/*
 * Make river between two points.
 * Do not change the value of the two points
 */
static void link_river(int x1, int x2, int y1, int y2, int sea_level)
{
	int x, y, dx, dy, changex, changey;
	int length, l;

	length = distance(x1, y1, x2, y2);

	if (length > 4)
	{
		/*
		 * Divide path in half and call routine twice.
		 * There is a small chance of splitting the river
		 */
		dx = (x2 - x1) / 2;
		dy = (y2 - y1) / 2;

		if (dy != 0)
		{
			/* perturbation perpendicular to path */
			changex = randint(abs(dy)) * 2 - abs(dy);
		}
		else
		{
			changex = 0;
		}

		if (dx != 0)
		{
			/* perturbation perpendicular to path */
			changey = randint(abs(dx)) * 2 - abs(dx);
		}
		else
		{
			changey = 0;
		}

		/* Check bounds */
		if ((y1 + dy + changey < 0) || (y1 + dy + changey >= max_wild))
		{
			changey = 0;
		}

		if ((x1 + dx + changex < 0) || (x1 + dx + changex >= max_wild))
		{
			changex = 0;
		}

		/* construct river out of two smaller ones */
		link_river(x1, x1 + dx + changex, y1, y1 + dy + changey, sea_level);
		link_river(x1 + dx + changex, x2, y1 + dy + changey, y2, sea_level);
	}
	else
	{
		/* Actually build the river */
		for (l = 0; l < length; l++)
		{
			x = x1 + l * (x2 - x1) / length;
			y = y1 + l * (y2 - y1) / length;

			/*
			 * Hack - set height to be sea level.
			 * This is checked for later, to make ocean boundaries.
			 * Thus rivers and beaches use the same code.
			 */
			wild[y][x].gen.hgt_map = 7 * sea_level / 8;
		}
	}
}

/*
 * Make a few rivers in the wilderness.
 *
 * This is done by generating a few random "starting points"
 * The highest closest points are connected by a fractal line.
 * This is repeated until the highest point is below sea level.
 *
 * Hack - the rivers are denoted by hgt= sea_level * 7/8
 * This means that the river and sea boundaries use the same code.
 */

static void create_rivers(int sea_level)
{
	int i, cur_posn, high_posn, dh, river_start;
	int cx, cy, ch;
	long dist, dx, dy, val, h_val;

	/* Number of river starting points. */
	river_start = (long) max_wild * max_wild / 1000;

	/* paranoia - bounds checking */
	if (river_start > TEMP_MAX) river_start = TEMP_MAX;

	/* Make some random starting positions */
	for (i = 0; i < river_start; i++)
	{
		temp_y[i] = (s16b)rand_int(max_wild);
		temp_x[i] = (s16b)rand_int(max_wild);
	}

	temp_n = river_start;


	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_height;
	ang_sort_swap = ang_sort_swap_height;

	/* Sort positions by height of wilderness */
	ang_sort(temp_x, temp_y, temp_n);

	/* Start at highest position */
	cur_posn = 0;

	cx = temp_x[cur_posn];
	cy = temp_y[cur_posn];

	ch = wild[cy][cx].gen.hgt_map;

	/*
	 * Link highest position to closest next highest position.
	 * Stop when all positions above sea level are used, or
	 * (rarely) if there is only one left in the array.
	 */
	while ((ch > sea_level) && (temp_n > cur_posn + 1))
	{
		/* The highest position is at (0,0) in the array. */

		/* Find the closest next highest one. */
		high_posn = cur_posn + 1;

		/* Distance squared */
		dy = cy - temp_y[high_posn];
		dx = cx - temp_x[high_posn];

		dist = dy * dy + dx * dx;

		/* Change in Height */
		dh = ch - wild[temp_y[high_posn]][temp_x[high_posn]].gen.hgt_map;

		/* Small val for close high positions */
		h_val = dh * dist;

		/* Check the other positions in the array */
		for (i = high_posn + 1; i < temp_n; i++)
		{
			/* Distance squared */
			dy = cy - temp_y[i];
			dx = cx - temp_x[i];

			dist = dy * dy + dx * dx;

			/* Change in Height */
			dh = ch - wild[temp_y[i]][temp_x[i]].gen.hgt_map;

			/* Small val for close high positions */
			val = dh * dist;

			/* Is this position better than previous best? */
			if (val < h_val)
			{
				h_val = val;
				high_posn = i;
			}
		}

		/* Make river between two points */
		link_river(cx, temp_x[high_posn], cy, temp_y[high_posn], sea_level);

		/* Get new highest point */
		cur_posn++;

		cx = temp_x[cur_posn];
		cy = temp_y[cur_posn];

		ch = wild[cy][cx].gen.hgt_map;
	}

	/* hack - reset viewable grid set. */
	temp_n = 0;
}




/* this routine probably should be an inline function or a macro. */
static void store_hgtmap(int x, int y, int val)
{
	/* bounds checking */
	if (val < 0) val = 0;
	if ((val >> 4) >= max_wild) val = (max_wild << 4) - 1;

	/* Save distribution information */
	wild_temp_dist[val >> 4] = 1;

	/* store the value in height-map format */
	wild[y][x].gen.hgt_map = val;

	return;
}

/*
 * This function creates the first of the three parameters used to generate
 * the wilderness.  This is done by making a plasma fractal.  The distribution
 * of the values in the height map is stored so that they can be scaled to
 * generate a wilderness with an even distribution of terrain.
 */
static void create_hgt_map(void)
{
	int grd;

	/*
	 * fixed point variables- these are stored as 16 x normal value
	 * this gives 4 binary places of fractional part + 12 places of normal part
	 */

	int lstep, hstep, i, j, ii, jj, size;

	/* Size is one bigger than normal blocks for speed of algorithm with 2^n + 1 */
	size = max_wild - 1;

	/* Clear the section */
	for (i = 0; i <= size; i++)
	{
		for (j = 0; j <= size; j++)
		{
			/* MAX_SHORT is a flag for "not done yet" */
			wild[j][i].gen.hgt_map = MAX_SHORT;
		}

		/* Clear distribution information */
		wild_temp_dist[i] = 0;
	}

	/* Set maximum correlation length to be 256 squares */
	grd = 16 * 16;

	/* Set the corner values just in case grd > size. */
	store_hgtmap(0, 0, rand_int(size));
	store_hgtmap(size, 0, rand_int(size));
	store_hgtmap(0, size, rand_int(size));
	store_hgtmap(size, size, rand_int(size));

	/* Initialize the step sizes */
	lstep = hstep = size * 16;
	size = size * 16;

	/*
	 * Fill in the square with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while (hstep > 16)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* cache values of i,j divided by 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.hgt_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_hgtmap(ii, jj, randint(max_wild * 16));
					}
			   		else
					{
						/* Average of left and right points +random bit */
						store_hgtmap(ii, jj,
						((wild[jj][(i - hstep) >> 4].gen.hgt_map +
						wild[jj][(i + hstep) >> 4].gen.hgt_map) >> 1) +
						((randint(lstep) - hstep) >> 1));
					}
				}
			}
		}


		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
		   	{
				/* cache values of i,j / 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.hgt_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_hgtmap(ii, jj, randint(max_wild * 16));
					}
		   			else
					{
						/* Average of up and down points +random bit */
						store_hgtmap(ii, jj,
						((wild[(j - hstep) >> 4][ii].gen.hgt_map
						+ wild[(j + hstep) >> 4][ii].gen.hgt_map) >> 1)
						+ ((randint(lstep) - hstep) >> 1));
					}
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
			   	/* cache values of i,j / 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.hgt_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_hgtmap(ii, jj, randint(max_wild * 16));
					}
		   			else
					{
						/* average over all four corners + scale by 181 to
						 * reduce the effect of the square grid on the shape of the fractal */
						store_hgtmap(ii, jj,
						((wild[(j - hstep) >> 4][(i - hstep) >> 4].gen.hgt_map
						+ wild[(j + hstep) >> 4][(i - hstep) >> 4].gen.hgt_map
						+ wild[(j - hstep) >> 4][(i + hstep) >> 4].gen.hgt_map
						+ wild[(j + hstep) >> 4][(i + hstep) >> 4].gen.hgt_map) >> 2)
						+ (((randint(lstep) - hstep) * 181) >> 8));
					}
				}
			}
		}
	}
}


/* this routine probably should be an inline function or a macro. */
static void store_popmap(int x, int y, int val, u16b sea)
{
	/* bounds checking */
	if (val < 0) val = 0;
	if ((val >> 4) >= max_wild) val = (max_wild << 4) - 1;

	/* Save distribution information (only if not below sea level) */
	if (wild[y][x].gen.hgt_map > sea) wild_temp_dist[val >> 4] = 1;

	/* store the value in height-map format */
	wild[y][x].gen.pop_map = val;

	return;
}

/*
 * This function creates the second of the three parameters used to generate
 * the wilderness.  This is done by making a plasma fractal.
 */
static void create_pop_map(u16b sea)
{
	int grd;

	/*
	 * fixed point variables- these are stored as 16 x normal value
	 * this gives 4 binary places of fractional part + 12 places of normal part
	 */

	int lstep, hstep, i, j, ii, jj, size;

	/* Size is one bigger than normal blocks for speed of algorithm with 2^n + 1 */
	size = max_wild - 1;

	/* Clear the section */
	for (i = 0; i <= size; i++)
	{
		for (j = 0; j <= size; j++)
		{
			/* MAX_SHORT is a flag for "not done yet" */
			wild[j][i].gen.pop_map = MAX_SHORT;
		}

		/* Clear distribution information */
		wild_temp_dist[i] = 0;
	}

	/* Set maximum correlation length to be 256 squares */
	grd = 16 * 16;

	/* Set the corner values just in case grd > size. */
	store_popmap(0, 0, rand_int(size), sea);
	store_popmap(size, 0, rand_int(size), sea);
	store_popmap(0, size, rand_int(size), sea);
	store_popmap(size, size, rand_int(size), sea);

	/* Initialize the step sizes */
	lstep = hstep = size * 16;
	size = size * 16;

	/*
	 * Fill in the square with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while (hstep > 16)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* cache values of i,j divided by 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.pop_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_popmap(ii, jj, randint(max_wild * 16), sea);
					}
			   		else
					{
						/* Average of left and right points +random bit */
						store_popmap(ii, jj,
						((wild[jj][(i - hstep) >> 4].gen.pop_map +
						wild[jj][(i + hstep) >> 4].gen.pop_map) >> 1) +
						((randint(lstep) - hstep) >> 1), sea);
					}
				}
			}
		}


		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
		   	{
				/* cache values of i,j / 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.pop_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_popmap(ii, jj, randint(max_wild * 16), sea);
					}
		   			else
					{
						/* Average of up and down points +random bit */
						store_popmap(ii, jj,
						((wild[(j - hstep) >> 4][ii].gen.pop_map
						+ wild[(j + hstep) >> 4][ii].gen.pop_map) >> 1)
						+ ((randint(lstep) - hstep) >> 1), sea);
					}
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
			   	/* cache values of i,j / 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.pop_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_popmap(ii, jj, randint(max_wild * 16), sea);
					}
		   			else
					{
						/* average over all four corners + scale by 181 to
						 * reduce the effect of the square grid on the shape of the fractal */
						store_popmap(ii, jj,
						((wild[(j - hstep) >> 4][(i - hstep) >> 4].gen.pop_map
						+ wild[(j + hstep) >> 4][(i - hstep) >> 4].gen.pop_map
						+ wild[(j - hstep) >> 4][(i + hstep) >> 4].gen.pop_map
						+ wild[(j + hstep) >> 4][(i + hstep) >> 4].gen.pop_map) >> 2)
						+ (((randint(lstep) - hstep) * 181) >> 8), sea);
					}
				}
			}
		}
	}
}


/* this routine probably should be an inline function or a macro. */
static void store_lawmap(int x, int y, int val, u16b sea)
{
	/* bounds checking */
	if (val < 0) val = 0;
	if ((val >> 4) >= max_wild) val = (max_wild << 4) - 1;

	/* Save distribution information (only if not below sea level) */
	if (wild[y][x].gen.hgt_map > sea) wild_temp_dist[val >> 4] = 1;

	/* store the value in height-map format */
	wild[y][x].gen.law_map = val;

	return;
}

/*
 * This function creates the third of the three parameters used to generate
 * the wilderness.  This is done by making a plasma fractal.
 */
static void create_law_map(u16b sea)
{
	int grd;

	/*
	 * fixed point variables- these are stored as 16 x normal value
	 * this gives 4 binary places of fractional part + 12 places of normal part
	 */

	int lstep, hstep, i, j, ii, jj, size;

	/* Size is one bigger than normal blocks for speed of algorithm with 2^n + 1 */
	size = max_wild - 1;

	/* Clear the section */
	for (i = 0; i <= size; i++)
	{
		for (j = 0; j <= size; j++)
		{
			/* MAX_SHORT is a flag for "not done yet" */
			wild[j][i].gen.law_map = MAX_SHORT;
		}

		/* Clear distribution information */
		wild_temp_dist[i] = 0;
	}

	/* Set maximum correlation length to be 256 squares */
	grd = 16 * 16;

	/* Set the corner values just in case grd > size. */
	store_lawmap(0, 0, rand_int(size), sea);
	store_lawmap(size, 0, rand_int(size), sea);
	store_lawmap(0, size, rand_int(size), sea);
	store_lawmap(size, size, rand_int(size), sea);

	/* Initialize the step sizes */
	lstep = hstep = size * 16;
	size = size * 16;

	/*
	 * Fill in the square with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while (hstep > 16)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* cache values of i,j divided by 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.law_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_lawmap(ii, jj, randint(max_wild * 16), sea);
					}
			   		else
					{
						/* Average of left and right points +random bit */
						store_lawmap(ii, jj,
						((wild[jj][(i - hstep) >> 4].gen.law_map +
						wild[jj][(i + hstep) >> 4].gen.law_map) >> 1) +
						((randint(lstep) - hstep) >> 1), sea);
					}
				}
			}
		}


		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
		   	{
				/* cache values of i,j / 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.law_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_lawmap(ii, jj, randint(max_wild * 16), sea);
					}
		   			else
					{
						/* Average of up and down points +random bit */
						store_lawmap(ii, jj,
						((wild[(j - hstep) >> 4][ii].gen.law_map
						+ wild[(j + hstep) >> 4][ii].gen.law_map) >> 1)
						+ ((randint(lstep) - hstep) >> 1), sea);
					}
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
			   	/* cache values of i,j / 16 */
				ii = i >> 4;
				jj = j >> 4;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.law_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_lawmap(ii, jj, randint(max_wild * 16), sea);
					}
		   			else
					{
						/* average over all four corners + scale by 181 to
						 * reduce the effect of the square grid on the shape of the fractal */
						store_lawmap(ii, jj,
						((wild[(j - hstep) >> 4][(i - hstep) >> 4].gen.law_map
						+ wild[(j + hstep) >> 4][(i - hstep) >> 4].gen.law_map
						+ wild[(j - hstep) >> 4][(i + hstep) >> 4].gen.law_map
						+ wild[(j + hstep) >> 4][(i + hstep) >> 4].gen.law_map) >> 2)
						+ (((randint(lstep) - hstep) * 181) >> 8), sea);
					}
				}
			}
		}
	}
}

/* Finish making the wilderness - recenter the screen around the player. */
static void wild_done(void)
{
	px = (s16b)p_ptr->wilderness_x;
	py = (s16b)p_ptr->wilderness_y;

	map_panel_size();

	/* Hack - delete all items / monsters / fields in wilderness */
	wipe_o_list();
	wipe_m_list();
	wipe_f_list();

	/* Clear cache */
	init_wild_cache();

	/* Fix location of grid */

	/*
	 * Hack - set the coords to crazy values so move_wild() works
	 * when change_level is called to make the player actually enter the
	 * new wilderness.
	 */

	wild_grid.x = max_wild + 1;
	wild_grid.y = max_wild + 1;

	/* hack */
	dun_level = 1;

	/* Change to the wilderness - but do not light anything yet.*/
	change_level(0);

	/* Change back to inside wilderness */
	dun_level = 0;

	/* Refresh random number seed */
	wild_grid.wild_seed = rand_int(0x10000000);

	/* Make the wilderness block cache. */
	move_wild();
}

/*
 * Create the wilderness
 *
 * This is done by making three plasma fractals
 * The three values for each 16x16 block are then passed into
 * the decision tree code to get a wilderness type.  (This
 * is done for speed.  The binary tree takes O(log(n)) steps to
 * find a matching type from w_info.txt, a linear search will
 * obviously be a O(n) algorithm.  With hundreds of types, the
 * difference is noticable.
 *
 * The old three values for height, law level, and population level
 * are then merged to work out the monster generation statistics for
 * each 16x16 block.
 *
 * Finally towns are placed. 
 *
 * Problem: The towns don't take into account the hpl of the wilderness
 * properly yet.  There may need to be another union to store the information
 * as the wildness is being made...
 
 *
 * This code is incomplete: 
 * No lakes yet.
 * No roads yet.
 * No specials yet.
 */

void create_wilderness(void)
{
	int i,j;

	u16b hgt_min, hgt_max, pop_min, pop_max, law_min, law_max;
	u16b sea_level;

	long hgt, pop, law, hgt_scale, pop_scale, law_scale;

	/* Test wilderness generation information */
	test_wild_data();

	/* Minimal wilderness */
	if (vanilla_town)
	{
		/* Tiny wilderness */
		max_wild = WILD_GRID_SIZE + 1;

		/* Mega Hack - make an "empty" wilderness (all ocean). */
		for (i = 0; i < max_wild; i++)
		{
			for (j = 0; j < max_wild; j++)
			{
				/* Mega Hack - Use the 0 value (normally empty) to denote grass. */
				wild[j][i].done.wild = 0;

				/* Nothing interesting here */
				wild[j][i].done.info = 0;

				/* No town yet */
				wild[j][i].done.town = 0;

				/* Monsters are easy */
				wild[j][i].done.mon_gen = 0;

				/* Monsters are fairly common */
				wild[j][i].done.mon_prob = 64;
			}
		}

		/* Make a single vanilla town. */
		init_vanilla_town();

		/* Done */
		wild_done();
		return;
	}

	/* Huge wilderness */
	max_wild = max_wild_size;

	/* Create "height" information of wilderness */
	create_hgt_map();

	/* work out extremes of height so it can be scaled. */
	hgt_min = hgt_max = pop_min = pop_max = law_min = law_max = 0;

	/* minimum height */
	for (i = 0; i < max_wild; i++)
	{
		if (wild_temp_dist[i] != 0)
		{
			hgt_min = i;
			break;
		}
	}

	/* maximum height */
	for (i = max_wild - 1; i >= 0; i--)
	{
		if (wild_temp_dist[i] != 0)
		{
			hgt_max = i;
			break;
		}
	}

	/* The sea covers 1/4 of the wilderness */
	sea_level = hgt_min + (hgt_max - hgt_min) / 4;

	/* Height scale factor */
	hgt_scale = (hgt_max - sea_level);

	/* Rescale minimum. */
	sea_level *= 16;

	/* Add in rivers... */
	create_rivers(sea_level);

	/* create "population density" information */
	create_pop_map(sea_level);

	/* work out extremes of population so it can be scaled. */

	/* minimum population */
	for (i = 0; i < max_wild; i++)
	{
		if (wild_temp_dist[i] != 0)
		{
			pop_min = i;
			break;
		}
	}

	/* maximum population */
	for (i = max_wild - 1; i >= 0; i--)
	{
		if (wild_temp_dist[i] != 0)
		{
			pop_max = i;
			break;
		}
	}

	/* Population scale factor */
	pop_scale = (pop_max - pop_min);

	/* Rescale minimum. */
	pop_min *= 16;

	create_law_map(sea_level);

	/* work out extremes of "lawfulness" so it can be scaled. */

	/* minimum lawfulness */
	for (i = 0; i < max_wild; i++)
	{
		if (wild_temp_dist[i] != 0)
		{
			law_min = i;
			break;
		}
	}

	/* maximum lawfulness */
	for (i = max_wild - 1; i >= 0; i--)
	{
		if (wild_temp_dist[i] != 0)
		{
			law_max = i;
			break;
		}
	}

	/* Lawfulness scale factor */
	law_scale = (law_max - law_min);

	/* Rescale minimum. */
	law_min *= 16;

	/* Fill wilderness with terrain */
	for (i = 0; i < max_wild; i++)
	{
		for (j = 0; j < max_wild; j++)
		{
			/*
			 * Store parameters before change the information
			 * in the union.  (Want to scale values to be 0 - 255)
			 */
			pop = (wild[j][i].gen.pop_map - pop_min) * 16 / pop_scale;
			law = (wild[j][i].gen.law_map - law_min) * 16 / law_scale;


			/* If above sea level - use decision tree to get terrain. */
			if (wild[j][i].gen.hgt_map <= sea_level)
			{
				/* Height */
				hgt = wild[j][i].gen.hgt_map;

				/* Ocean */
				wild[j][i].done.wild = 65535 - ((long) hgt * 64) / sea_level;

				if (hgt > sea_level / 2)
				{
					/* Set to be water boundary */
					wild[j][i].done.info = WILD_INFO_RIVER;
				}
				else
				{
					/* No rivers / roads / all unknown */
					wild[j][i].done.info = 0;
				}
			}
			else
			{
				/* Terrains from decision tree */

				/*
				 * Get height from (0 - 255)
				 */
				hgt = (wild[j][i].gen.hgt_map - sea_level) * 16 / hgt_scale;


				/* Get wilderness type. */
				wild[j][i].done.wild = get_gen_type((byte)hgt, (byte)pop, (byte)law);

				/* No rivers / roads / all unknown */
				wild[j][i].done.info = 0;
			}

			/* No town yet */
			wild[j][i].done.town = 0;

			/* Mega hack - set monster toughness and density */

			/* Toughness (level 0 - 64) */
			wild[j][i].done.mon_gen = (256 - law) * (256 - pop)  / 1024;

			/* No monsters (probability 0 - 16) */
			wild[j][i].done.mon_prob = pop / 16;
		}
	}

	/* Free up memory used to create the wilderness */
#if 0
	C_FREE(wild_choice_tree, max_w_node, wild_choice_tree_type);
	C_FREE(wild_temp_dist, max_wild, byte);
#endif

	/* A dodgy town generation routine */
	init_towns();

#if 0
	/* Connect the towns with roads */
	create_roads();
#endif /* 0 */
	
	/* Done */
	wild_done();
}
