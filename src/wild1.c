/* File: wild1.c */

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

/* The starting position of the player */
int wild_stairs_x = 0;
int wild_stairs_y = 0;

/* Is the building a store? */
bool build_is_store(s16b type)
{
	switch (type)
	{
		case BUILD_STORE_GENERAL:
		case BUILD_STORE_ARMOURY:
		case BUILD_STORE_WEAPON:
		case BUILD_STORE_TEMPLE:
		case BUILD_STORE_ALCHEMIST:
		case BUILD_STORE_MAGIC:
		case BUILD_STORE_BLACK:
		case BUILD_STORE_HOME:
		case BUILD_STORE_BOOK:
		{
			/* It is a store */
			return(TRUE);
		}
	}
	
	/* Not a store */
	return(FALSE);
}

/* Is the building a general feature? */
bool build_is_general(byte type)
{
	switch (type)
	{
		case BUILD_STAIRS:
		case BUILD_NONE:
		case BUILD_BLANK:
		{
			/* It is a general town feature */
			return(TRUE);
		}
	}
	
	/* Nope it isn't */
	return (FALSE);
}

/* Find a place for the player */
static void place_player_start(u32b *x, u32b *y, u16b this_town)
{
	/* Hack - Reset player position to be on the stairs in town */
	*x = town[this_town].x * 16 + wild_stairs_x;
	*y = town[this_town].y * 16 + wild_stairs_y;
}


/* Select a store or building "appropriate" for a given position */
static u16b select_building(byte pop, byte magic, byte law, u16b *build,
	 int build_num)
{
	int i;
	
	u16b b_select[MAX_CITY_BUILD];

	s32b total = 0;
	
	/* Draw stairs first for small towns */
	if ((build_num < 10) && (!build[BUILD_STAIRS])) return(BUILD_STAIRS);
	
	/* Next, we need a general store */
	if (!build[BUILD_STORE_GENERAL]) return(BUILD_STORE_GENERAL);
	
	for (i = 0; i < MAX_CITY_BUILD; i++)
	{
		/* All have equal prob. + effect due to total count */
		b_select[i] = 1 + build[i] * 2;
	}
	
	/* Dungeons are not in large cities */
	if (build_num > 11) b_select[BUILD_STAIRS] = 0;
	
	/* Blank buildings don't exist for small towns */
	if (build_num < 10)
	{
		b_select[BUILD_NONE] = 0;
		b_select[BUILD_BLANK] = 0;
	}

	/* Blank buildings are much more common for large towns */
	if (build_num > 9)
	{
		b_select[BUILD_NONE] = 1;
		b_select[BUILD_BLANK] = 1;
	}
	
	/* Not more than one home */
	if (build[BUILD_STORE_HOME]) b_select[BUILD_STORE_HOME] = 0;

	/* Some buildings are normally rare */
	b_select[BUILD_RECHARGE] += 2;
	b_select[BUILD_PLUS_WEAPON] += 2;
	b_select[BUILD_PLUS_ARMOUR] += 2;
	b_select[BUILD_MUTATE] += 4;
	
	/* Calculate total */
	for (i = 0; i < MAX_CITY_BUILD; i++)
	{
		if (b_select[i]) total += 256 / b_select[i];
	}

	/* Pick a building */
	total = randint0(total);
	
	/* Later add checks for silliness */
	/* (A small town with 5 "homes" would be silly */


	/* Find which building we've got */
	for (i = 0; i < MAX_CITY_BUILD; i++)
	{
		if (b_select[i]) total -= 256 / b_select[i];
		
		if (total <= 0) return (i);
	}


	/* paranoia - we didn't find it */
	msg_print("FAILED to generate building!");
	
	return(0);
	
#if 0	
	/* Just select the buildings in order... */
	while (TRUE)
	{
		for (i = 0; i < MAX_CITY_BUILD; i++)
		{
			if (build[i] == count) return (i);
		}
	
		count++;
	}
#endif /* 0 */	
} 

static void general_init(int town_num, int store_num, byte general_type)
{
	/* Activate that feature */
	store_type *st_ptr = &town[town_num].store[store_num];

	/* Set the type */
	st_ptr->type = general_type;

	/* Initialize */
	st_ptr->store_open = 0;
	st_ptr->insult_cur = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;
	st_ptr->stock_num = 0;
	st_ptr->last_visit = 0;
}


/* Create a city + contained stores and buildings */
static bool create_city(int x, int y, int town_num)
{
	int i, j, k, l;
	
/*	int pop = wild[y][x].trans.pop_map; */
	int pop = (1 << randint0(7)) + 128;
	int law = wild[y][x].trans.law_map;
	int magic, temp;
	int count = 0;	
	int build_num = 0, build_tot;
	byte building;
	byte gate_value[MAX_GATES];
	byte gate_num[MAX_GATES];

	bool city_block;
	u32b rng_seed_save;

	wild_gen2_type *w_ptr;
	town_type *t_ptr = &town[town_num];

	u16b build[MAX_CITY_BUILD];
	u16b build_list[WILD_BLOCK_SIZE * WILD_BLOCK_SIZE];

	/* Wipe the list of allocated buildings */
	(void)C_WIPE(build, MAX_CITY_BUILD, u16b);
	(void)C_WIPE(build_list, (WILD_BLOCK_SIZE * WILD_BLOCK_SIZE), u16b);
	
	/* Add town */
	strcpy(t_ptr->name, "town");
	t_ptr->seed = randint0(0x10000000);
	
	t_ptr->type = 2;
	t_ptr->x = x;
	t_ptr->y = y;

	t_ptr->pop = pop;
	
	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = t_ptr->seed;
	
	/* We don't have to save this in the town structure */
	magic = randint0(256);
	
	/* Generate plasma factal */
	clear_temp_block();
	set_temp_corner_val(WILD_BLOCK_SIZE * 64);
	set_temp_mid(WILD_BLOCK_SIZE * pop);
	frac_block();
	
	/* Copy the temp block to the town block */
	for (i = 0; i < WILD_BLOCK_SIZE + 1; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE + 1; j++)
		{
			temp = temp_block[j][i];
			
			if (temp < WILD_BLOCK_SIZE * 128)
			{
				/* Outside the town */
				town_block[j][i] = 0;
			}
			else
			{
				town_block[j][i] = temp;
			}
		}
	}
	
	/* Hack - save seed of rng */
	rng_seed_save = Rand_value;
	
	/* 
	 * Generate second fractal
	 */
	clear_temp_block();
	set_temp_corner_val(WILD_BLOCK_SIZE * 64);
	set_temp_mid(WILD_BLOCK_SIZE * law);
	frac_block();
	
	/* Restore the old seed */
	Rand_value = rng_seed_save;
	
	/* Find walls */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Is a "city block" */
			if (town_block[j][i])
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
							if (!town_block[j + l][i + k])
							{
								/* Make a wall */
								town_block[j][i] = 1;
							}
						}
						else
						{
							/* Make a wall */
							town_block[j][i] = 1;
						}
					}
				}

				/* Count "buildable blocks" */				
				if (town_block[j][i] != 1) count++;
			}
		}
	}
	
		
	/* Too few squares??? */
	if (count < 6) return (FALSE);

	/* Rescan walls to avoid "islands" */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Is a "wall block" */
			if (town_block[j][i] == 1)
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
							if (town_block[j + l][i + k] > 1)
							{
								/* We are next to a city */
								city_block = TRUE;
							}
						}
					}
				}
				
				/* No islands */
				if (!city_block) town_block[j][i] = 0;
			}
		}
	}
	
	/* Clear the gates locations */
	(void)C_WIPE(t_ptr->gates_x, MAX_GATES, byte);
	(void)C_WIPE(t_ptr->gates_y, MAX_GATES, byte);	
	(void)C_WIPE(gate_num, MAX_GATES, byte);
	
	
	/* Initialise min and max values */
	gate_value[0] = 0;
	gate_value[1] = 255;
	gate_value[2] = 0;
	gate_value[3] = 255;
	
	/* Hack - save seed of rng */
	rng_seed_save = Rand_value;
	
	/*
	 * Link wilderness to the new city
	 * and find position of town gates.
	 */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			/* Is it a city block? */
			if (town_block[j][i])
			{
				w_ptr =	&wild[y + j / 2][x + i / 2].trans;

				/*
				 * Add city to wilderness
				 * Note: only 255 towns can be stored currently.
				 */
				w_ptr->town = (byte)town_num;
				
				/* Hack - make a flat area around the town */
				w_ptr->info |= WILD_INFO_ROAD;
				
				/* Right gate */
				if (gate_value[0] < i)
				{
					/* save it */
					gate_value[0] = i;
					gate_num[0] = 2;
					t_ptr->gates_x[0] = i;
					t_ptr->gates_y[0] = j;
				}
				else if ((gate_value[0] == i) && (randint0(gate_num[0]) == 0))
				{
					/* save it */
					gate_value[0] = i;
					gate_num[0]++;
					t_ptr->gates_x[0] = i;
					t_ptr->gates_y[0] = j;
				}
				
				/* Left gate */
				if (gate_value[1] > i)
				{
					/* save it */
					gate_value[1] = i;
					gate_num[1] = 2;
					t_ptr->gates_x[1] = i;
					t_ptr->gates_y[1] = j;
				}
				else if ((gate_value[1] == i) && (randint0(gate_num[1]) == 0))
				{
					/* save it */
					gate_value[1] = i;
					gate_num[1]++;
					t_ptr->gates_x[1] = i;
					t_ptr->gates_y[1] = j;
				}
				
				/* Bottom gate */
				if (gate_value[2] < j)
				{
					/* save it */
					gate_value[2] = j;
					gate_num[2] = 2;
					t_ptr->gates_x[2] = i;
					t_ptr->gates_y[2] = j;
				}
				else if ((gate_value[2] == j) && (randint0(gate_num[2]) == 0))
				{
					/* save it */
					gate_value[2] = j;
					gate_num[2]++;
					t_ptr->gates_x[2] = i;
					t_ptr->gates_y[2] = j;
				}
				
				/* Top gate */
				if (gate_value[3] > j)
				{
					/* save it */
					gate_value[3] = j;
					gate_num[3] = 2;
					t_ptr->gates_x[3] = i;
					t_ptr->gates_y[3] = j;
				}
				else if ((gate_value[3] == j) && (randint0(gate_num[3]) == 0))
				{
					/* save it */
					gate_value[3] = j;
					gate_num[3]++;
					t_ptr->gates_x[3] = i;
					t_ptr->gates_y[3] = j;
				}
			}
		}
	}
		
	/* Restore the old seed */
	Rand_value = rng_seed_save;
	
	/* Save the total number of buildings */
	build_tot = count;
	
	/* Scan blocks in a random order */
	while (count)
	{
		/* Pick a square */		
		i = randint0(WILD_BLOCK_SIZE);
		j = randint0(WILD_BLOCK_SIZE);

		/* Find some room for a building */
		while (town_block[j][i] <= 1)
		{
			/* Scan across town_block */
			i++;

			if (i == WILD_BLOCK_SIZE)
			{
				/* New line */
				i = 0;
				j++;

				if (j == WILD_BLOCK_SIZE)
				{
					/* Restart from the begining */
					j = 0;
				}
			}
		}

		/* Get parameters for the 8x8 section the building is on */
		pop = town_block[j][i] / WILD_BLOCK_SIZE;
		law = temp_block[j][i] / WILD_BLOCK_SIZE;

		/* 
		 * "place" building, and then record in the
		 * list of allocated buildings.
		 */
		building = select_building(pop, magic, law, build, build_tot);
		
		/* Count number of this type */
		build[building]++;
		
		/* Record list of created buildings */
		build_list[build_num++] = building;
		
		/* Decrement free space in city */
		count--;
		
		town_block[j][i] = 0;
	}
	
	/*
	 * Generate store and building data structures
	 */
	
	/* Allocate the stores */
	C_MAKE(t_ptr->store, build_num, store_type);
	t_ptr->numstores = build_num;

	/* Initialise the stores */
	for (i = 0; i < build_num; i++)
	{
		building = build_list[i];
		
		if (build_is_store(building))
		{
			/* Initialise the store */
			store_init(town_num, i, building);
		}
		else if (build_is_general(building))	
		{
			/* Initialise general feature */
			general_init(town_num, i, building);
		}
		else
		{
			/* Initialise the building */
			build_init(town_num, i, building);
		}
	}

	/* Success */
	return (TRUE);
}


/*
 * Initialise the town structures
 *
 * We have cities now...
 */
static void init_towns(void)
{
	int x, y, i;
	
	wild_gen2_type *w_ptr;
	
	/* Variables to pick "easiest" town. */
	u16b best_town = 0, town_value = 0;
	
	/* No towns yet */
	town_count = 1;
	
	/*
	 * Try to add max_towns towns.
	 */
	while (town_count < max_towns)
	{
		/* Get random position */
		x = randint0(max_wild);
		y = randint0(max_wild);
	
		/*
		 * See if a city will fit.
		 * (Need a 8x8 block free.)
		 */
		if (!town_blank(x, y, 8, 8)) continue;
	
		/* Generate it */
		if (create_city(x, y, town_count))
		{
			w_ptr = &wild[y][x].trans;
			
			/* Select easiest town */
			if ((w_ptr->law_map + w_ptr->pop_map) > town_value)
			{
				/* Check to see if the town has stairs */
				for (i = 0; i < town[town_count].numstores; i++)
				{
					if (town[town_count].store[i].type == BUILD_STAIRS)
					{
						/* Save this town */
						town_value = w_ptr->law_map + w_ptr->pop_map;
						best_town = town_count;
						
						/* Done */
						break;
					}
				}
			}
			
			/* Increment number of towns */
			town_count++;
		}
	}
			
	/* Build starting city / town */
	draw_city(best_town);
	
	place_player_start(&p_ptr->wilderness_x, &p_ptr->wilderness_y, best_town);
}


/* Set wilderness stats depending on town type */
void set_mon_wild_values(byte town_type, wild_done_type *w_ptr)
{
	/* This function is very rudimentary at the moment */

	/* One and only one type of monster distribution */
	switch (town_type)
	{
		case 0:
		{
			/* Monsters are easy */
			w_ptr->mon_gen = 0;

			/* Monsters are fairly common */
			w_ptr->mon_prob = 64;
			break;
		}

		default:
		{
			/* Monsters are easy */
			w_ptr->mon_gen = 0;

			/* Monsters are fairly common */
			w_ptr->mon_prob = 64;
		}
	}
}


/*
 * Place a single town in the middle of the tiny wilderness
 */
static void init_vanilla_town(void)
{
	int i, j;

	/* Only one town */
	strcpy(town[1].name, "town");
	town[1].seed = randint0(0x10000000);
	town[1].numstores = 9;
	town[1].type = 1;
	town[1].x = (max_wild / 2) - TOWN_WID / (WILD_BLOCK_SIZE * 2) - 1;
	town[1].y = (max_wild / 2) - TOWN_HGT / (WILD_BLOCK_SIZE * 2) - 1;

	/* Allocate the stores */
	C_MAKE(town[1].store, MAX_STORES, store_type);

	/* Init the stores */
	for (i = 0; i < MAX_STORES; i++)
	{
		/* Initialize */
		store_init(1, i, (byte)i);
	}

	/* Place town on wilderness */
	for (j = 0; j < (TOWN_HGT / 16 + 1); j++)
	{
		for (i = 0; i < (TOWN_WID / 16 + 1); i++)
		{
			wild[town[1].y + j][town[1].x + i].done.town = 1;
		}
	}

	/* Make the town - and get the location of the stairs */
	van_town_gen(1);

	place_player_start(&p_ptr->wilderness_x, &p_ptr->wilderness_y, 1);

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
	while (TRUE)
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
			if (randint1(tree_ptr->chance1 + tree_ptr->chance2) >
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
	return (0);
}


/* The number of allocated nodes in the decision tree */
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
		 * to nlog(n) for a random input of ranges.  Since the
		 * ranges in w_info.txt are in a "nice" order, the
		 * coefficient for this algorithmic complexity is fairly small.
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
	wild_choice_tree_type *tree_ptr;

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
 * This function must be called with a new empty node.  The node must be
 * connected to the tree by the calling routine.  (This "feature" is so that
 * this routine can be used to initialise an empty decision tree.)  This means
 * that the calling routine must check for the "null" node + completely filled
 * case. XXX XXX
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
 * This routine compares two bounding boxes 
 * and returns true if they are the same.
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
	wild_choice_tree_type *tree_ptr;

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
	while (TRUE)
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
	if (p_ptr->depth) return;

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

	for (i = 0; i < d_tree_count; i++)
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


/*
 * Is the specified town / dungeon / special able to be
 * connected by roads?
 */
static bool is_road_town(u16b town_num)
{
	/* Hack - change this when we implement other things */

	return (TRUE);
}


/*
 * Link two points in the wilderness with a road
 */
static void road_link(u16b x1, u16b y1, u16b x2, u16b y2)
{
	s16b xn, yn, i;
	s16b dx, dy, changex, changey;

	u16b dist = distance(x1, y1, x2, y2);

	wild_gen2_type *w_ptr;

	if (dist > 6)
	{
		/* Divide path in half and call routine twice. */
		dx = (x2 - x1) / 2;
		dy = (y2 - y1) / 2;

		if (dy != 0)
		{
			/* perturbation perpendicular to path */
			changex = randint1(abs(dy)) - abs(dy) / 2;
		}
		else
		{
			changex = 0;
		}

		if (dx != 0)
		{
			/* perturbation perpendicular to path */
			changey = randint1(abs(dx)) - abs(dx) / 2;
		}
		else
		{
			changey = 0;
		}

		xn = x1 + dx + changex;
		yn = y1 + dy + changey;

		/* Bounds checking */
		if (xn < 0) xn = 0;
		if (yn < 0) yn = 0;
		if (xn >= max_wild) xn = max_wild - 1;
		if (yn >= max_wild) yn = max_wild - 1;

		/* Link the roads up */
		road_link(x1, y1, xn, yn);
		road_link(xn, yn, x2, y2);

		/* Done */
		return;
	}

	/* Hack - not too small */
	if (dist < 2) return;

	/* Connect the road */
	for (i = 0; i <= dist; i++)
	{
		xn = x1 + i * (x2 - x1) / dist;
		yn = y1 + i * (y2 - y1) / dist;

		w_ptr = &wild[yn][xn].trans;

		/* No bridges yet */
		if (w_ptr->info & (WILD_INFO_WATER | WILD_INFO_LAVA | WILD_INFO_ACID))
		{
			continue;
		}

		/* Add the road to the wilderness */
		if (w_ptr->law_map + w_ptr->pop_map < 256)
		{
			w_ptr->info |= WILD_INFO_TRACK;
		}
		else
		{
			w_ptr->info |= WILD_INFO_ROAD;
		}
	}
}


/*
 * Try to find a connecting square to a town.
 *
 * x and y point to a square outside the town.
 * A line is drawn from that point to the town.
 * The "gate" closest to the point where this
 * imaginary line crosses the town border is then
 * stored into x, y.  (Wilderness coords)
 */
static void road_connect(u16b *x, u16b *y, u16b town_num)
{
	town_type *t_ptr = &town[town_num];
	
	/* Big distance */
	int dist = max_wild * 2;
	int cdist, k;
	
	u16b x1 = *x, y1 = *y;
	
	/* Check town type */
	if (t_ptr->type == 2)
	{
		for (k = 0; k < MAX_GATES; k++)
		{
			/* Get distance from gate to target square */
			cdist = distance(x1, y1, t_ptr->x + t_ptr->gates_x[k] / 2,
				 t_ptr->y + t_ptr->gates_y[k] / 2);
			
			if (cdist < dist)
			{
				/* save minimal path */
				dist = cdist;
				
				switch (k)
				{			
					case 0:
					{
						*x = t_ptr->x + (t_ptr->gates_x[0] + 1) / 2;
						*y = t_ptr->y + t_ptr->gates_y[0] / 2;				
						break;
					}
					
					case 1:
					{
						*x = t_ptr->x + (t_ptr->gates_x[1] - 1) / 2;
						*y = t_ptr->y + t_ptr->gates_y[1] / 2;
						break;
					}
					
					case 2:
					{
						*x = t_ptr->x + t_ptr->gates_x[2] / 2;
						*y = t_ptr->y + (t_ptr->gates_y[2] + 1) / 2;
						break;
					}
					
					case 3:
					{
						*x = t_ptr->x + t_ptr->gates_x[3] / 2;
						*y = t_ptr->y + (t_ptr->gates_y[3] - 1) / 2;
						break;
					}
				}
			}
		}

		/* Done */
		return;
	}

	/* Dodgy hack = just output median town square */
	*x = town[town_num].x + 2;
	*y = town[town_num].y;
}


/*
 * Create the roads in the wildernes.
 *
 * Link towns that are close together.
 *
 * Look for good places to place "crossroads"
 */
static void create_roads(void)
{
	u16b i, j, towns = 0, links = 0;

	u16b x1, x2, x3, y1, y2, y3;

	s16b town1, town2, town3, town4;

	u16b dist, dist2, max_dist;

	u16b **link_list;
	u16b *town_number;


	/* Find number of linkable towns */
	for (i = 1; i < town_count; i++)
	{
		if (is_road_town(i))
		{
			/* Increment number of towns */
			towns++;
		}
	}

	/* Make towns x towns array of u16b's */
	C_MAKE(link_list, towns, u16b_ptr);

	for (i = 0; i < towns; i++)
	{
		C_MAKE(link_list[i], towns, u16b);
	}

	/* Town lookup table */
	C_MAKE(town_number, towns, u16b);

	/* Fill the lookup table */
	towns = 0;

	for (i = 1; i < town_count; i++)
	{
		if (is_road_town(i))
		{
			town_number[towns] = i;

			/* Increment number of towns */
			towns++;
		}
	}

	/* Tabulate distances less than ROAD_DIST */
	for (i = 0; i < towns; i++)
	{
		for (j = i + 1; j < towns; j++)
		{
			/* Get distance */
			dist = distance(town[town_number[i]].x, town[town_number[i]].y,
			                town[town_number[j]].x, town[town_number[j]].y);

			/* Only save it if the distance is smaller than ROAD_DIST */
			if (dist < ROAD_DIST)
			{
				link_list[j][i] = dist;
				link_list[i][j] = dist;

				links += 2;
			}
		}
	}

	/* While there are unconnected links left */
	while (links)
	{
		max_dist = ROAD_DIST;
		town1 = -1;
		town2 = -1;

		/* Find the shortest link */
		for (i = 0; i < towns; i++)
		{
			for (j = i + 1; j < towns; j++)
			{
				/* Get distance */
				dist = link_list[j][i];

				/* Already linked or no link at all? */
				if (!dist) continue;

				if (dist < max_dist)
				{
					/* This link is better */
					max_dist = dist;
					town1 = i;
					town2 = j;
				}
			}
		}

		/* No third town yet */
		town3 = -1;

		/* Max distance is 2x the dist between the two towns */
		max_dist += max_dist;

		/*
		 * Compare the connections for the two towns to see
		 * if they share a connection in common.
		 *
		 * Pick the shortest such dual link.
		 */
		for (i = 0; i < towns; i++)
		{
			/* Distance from town1 to the new town */
			dist = link_list[town1][i];

			/* No link? */
			if (!dist) continue;

			/* Distance from town2 to the new town */
			dist2 = link_list[town2][i];

			/* No link? */
			if (!dist2) continue;

			if ((dist2 == ROAD_DIST * 2 + 1) &&
			    (link_list[i][town1] != ROAD_DIST * 2 + 1))
			{
				/* Prevent "overlinking" with third town */
				link_list[i][town1] = ROAD_DIST * 2 + 1;
				link_list[town1][i] = ROAD_DIST * 2 + 1;

				links -=2;
			}

			if ((dist == ROAD_DIST * 2 + 1) &&
			    (link_list[i][town2] != ROAD_DIST * 2 + 1))
			{
				/* Prevent "overlinking" with third town */
				link_list[i][town2] = ROAD_DIST * 2 + 1;
				link_list[town2][i] = ROAD_DIST * 2 + 1;

				links -= 2;
			}

			/* There is a link! */
			if (dist + dist2 < max_dist)
			{
				/* Save the possible cross-roads partner */
				town3 = i;

				/* Update distance so we pick the closest three towns */
				max_dist = dist + dist2;
			}
		}

		if (town3 != -1)
		{
			/* Mark towns as connected to each other */
			link_list[town1][town2] = ROAD_DIST * 2 + 1;
			link_list[town1][town3] = ROAD_DIST * 2 + 1;
			link_list[town2][town1] = ROAD_DIST * 2 + 1;
			link_list[town2][town3] = ROAD_DIST * 2 + 1;
			link_list[town3][town1] = ROAD_DIST * 2 + 1;
			link_list[town3][town2] = ROAD_DIST * 2 + 1;

			/* Decrement link total */
			links -= 6;

			/* Have a triangle of connected towns */
			town1 = town_number[town1];
			town2 = town_number[town2];
			town3 = town_number[town3];

			/* Find midpoint */
			x2 = (town[town1].x + town[town2].x + town[town3].x) / 3;
			y2 = (town[town1].y + town[town2].y + town[town3].y) / 3;

			/* Connect the three towns to the midpoint */
			x1 = x2;
			x1 = y2;

			/* Get connection square for town1 */
			road_connect(&x1, &y1, town1);

			/* Link town1 with the midpoint */
			road_link(x1, y1, x2, y2);

			x1 = x2;
			x1 = y2;

			/* Get connection square for town2 */
			road_connect(&x1, &y1, town2);

			/* Link town2 with the midpoint */
			road_link(x1, y1, x2, y2);

			x1 = x2;
			x1 = y2;

			/* Get connection square for town3 */
			road_connect(&x1, &y1, town3);

			/* Link town1 with the midpoint */
			road_link(x1, y1, x2, y2);
		}
		else
		{
			dist = link_list[town1][town2];
			max_dist = (dist / 2) + 1;

			/* Mark the towns as connected to each other */
			link_list[town1][town2] = ROAD_DIST * 2 + 1;
			link_list[town2][town1] = ROAD_DIST * 2 + 1;

			/* Decrement link total */
			links -= 2;

			/* Hack - save the town number in link_list */
			town3 = town1;
			town4 = town2;

			/* Hack - set j to be zero */
			j = 0;

			town1 = town_number[town1];
			town2 = town_number[town2];

			/* Get first point */
			x1 = town[town2].x;
			y1 = town[town2].y;

			/* Get second point */
			x2 = town[town1].x;
			y2 = town[town2].y;

			/*
			 * In some cases, the road will "run into" other towns.
			 * The following code hopefully checks for that.
			 */
			for (i = 0; i < towns; i++)
			{
				/* Ignore the towns we want to connect */
				if ((i == town3) || (i == town4)) continue;

				/* Get location of the current town */
				x3 = town[town_number[i]].x;
				y3 = town[town_number[i]].y;

				/* See if is close */
				if ((distance(x1, y1, x3, y3) > max_dist) &&
					(distance(x2, y2, x3, y3) > max_dist)) continue;

				/* See if the town is "in the way" */
				if (dist_to_line(y3, x3, y1, x1, y2, x2) > dist / ROAD_MIN)
				{
					continue;
				}

				/* We have a problem - set j to be 1 (a flag) */
				j = 1;

				/* Exit */
				break;
			}

			/* If there are no problems - link the two towns */
			if (j == 0)
			{
				/* Get connection square for town1 */
				road_connect(&x1, &y1, town1);

				x2 = x1;
				y2 = y1;

				/* Get connection square for town2 */
				road_connect(&x2, &y2, town2);

				/* Link the two towns */
				road_link(x1, y1, x2, y2);
			}
		}
	}

	/* Free the array */
	for (i = 0; i < towns; i++)
	{
		C_FREE(link_list[i], towns, u16b);
	}

	C_FREE(link_list, towns, u16b_ptr);

	/* Town lookup table */
	C_FREE(town_number, towns, u16b);

	/* Done */
}



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
	ha = wild[y[a]][x[a]].trans.hgt_map;
	hb = wild[y[b]][x[b]].trans.hgt_map;

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
static void link_river(int x1, int x2, int y1, int y2)
{
	int xn, yn;
	int x, y, dx, dy, changex, changey;
	int length, l;

	length = distance(x1, y1, x2, y2);

	if (length > 6)
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
			changex = randint1(abs(dy)) - abs(dy) / 2;
		}
		else
		{
			changex = 0;
		}

		if (dx != 0)
		{
			/* perturbation perpendicular to path */
			changey = randint1(abs(dx)) - abs(dx) / 2;
		}
		else
		{
			changey = 0;
		}

		xn = x1 + dx + changex;
		yn = y1 + dy + changey;

		/* Bounds checking */
		if (xn < 0) xn = 0;
		if (yn < 0) yn = 0;
		if (xn >= max_wild) xn = max_wild - 1;
		if (yn >= max_wild) yn = max_wild - 1;

		/* construct river out of two smaller ones */
		link_river(x1, xn, y1, yn);
		link_river(xn, x2, yn, y2);
	}
	else
	{
		/* Actually build the river */
		for (l = 0; l < length; l++)
		{
			x = x1 + l * (x2 - x1) / length;
			y = y1 + l * (y2 - y1) / length;

			/* Set the river flag */
			wild[y][x].trans.info |= WILD_INFO_WATER;
		}
	}
}


/*
 * Make a few rivers in the wilderness.
 *
 * This is done by generating a few random "starting points"
 * The highest closest points are connected by a fractal line.
 * This is repeated until the highest point is below sea level.
 */
static void create_rivers(void)
{
	int i, cur_posn, high_posn, dh, river_start;
	int cx, cy, ch;
	int r1, r2;
	
	long val, h_val;

	/* Number of river starting points. */
	river_start = RIVER_NUM * RIVER_NUM;

	/* paranoia - bounds checking */
	if (river_start > TEMP_MAX) river_start = TEMP_MAX;

	/* Make some random starting positions */
	for (i = 0; i < river_start; i++)
	{
		/* Evenly spread out the points */
		r1 = ((i % RIVER_NUM) * max_wild) / RIVER_NUM;
		r2 = r1 + (max_wild / RIVER_NUM);
		
		temp_y[i] = (s16b)rand_range(r1, r2);
		
		r1 = ((i / RIVER_NUM) * max_wild) / RIVER_NUM;;
		r2 = r1 + (max_wild / RIVER_NUM);
		
		temp_x[i] = (s16b)rand_range(r1, r2);
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

	ch = wild[cy][cx].trans.hgt_map;

	/*
	 * Link highest position to closest next highest position.
	 * Stop when all positions above sea level are used, or
	 * (rarely) if there is only one left in the array.
	 */
	while ((ch > (256 / SEA_FRACTION)) && (temp_n > cur_posn + 1))
	{
		/* The highest position is at (0,0) in the array. */

		/* Find the closest next highest one. */
		high_posn = cur_posn + 1;

		/* Large value that should be bigger than anything below. */
		h_val = 10000;

		/* Check the other positions in the array */
		for (i = high_posn; i < temp_n; i++)
		{
			/* Hack - ignore deltas that already have been matched */
			if ((temp_x[i] == -1) || (temp_y[i] == -1)) continue;

			/* Change in Height */
			dh = ch - wild[temp_y[i]][temp_x[i]].trans.hgt_map;

			/* Small val for close high positions */
			/*val = dh + distance(cx, cy, temp_x[i], temp_y[i]);*/
			val = distance(cx, cy, temp_x[i], temp_y[i]);

			/* Is this position better than previous best? */
			if (val < h_val)
			{
				h_val = val;
				high_posn = i;
			}
		}

		/* No match */
		if (h_val == 10000) break;

		/* Make river between two points */
		link_river(cx, temp_x[high_posn], cy, temp_y[high_posn]);

		/*
		 * Mega hack - flag below sea level points
		 * to stop "deltas" being made.
		 */
		if (wild[temp_y[high_posn]][temp_x[high_posn]].trans.hgt_map <
			 (256 / SEA_FRACTION))
		{
			temp_x[high_posn] = -1;
			temp_y[high_posn] = -1;
		}

		/* Get new highest point */
		cur_posn++;

		cx = temp_x[cur_posn];
		cy = temp_y[cur_posn];

		while (((cx == -1) || (cy == -1)) && (cur_posn < temp_n - 1))
		{
			/* Ignore the point below sea level - already linked */
			cur_posn++;

			cx = temp_x[cur_posn];
			cy = temp_y[cur_posn];
		}

		/* Hack - failure to find a new node */
		if (cur_posn >= temp_n - 1) break;

		ch = wild[cy][cx].trans.hgt_map;
	}

	/* hack - reset viewable grid set. */
	temp_n = 0;
}


/*
 * Create random lakes.
 *
 * This is done by using the frac_block routine
 * to build a 17x17 plasma fractal.  This is interpreted
 * via a cutoff to make the lake.
 *
 * There are several types of lake - (water, lava and acid)
 * The type depends on the HPL of the location.
 *
 * Note the logic used to see that lava and acid lakes do not
 * overlap rivers, and that all lakes are above sea level.
 */
void create_lakes(void)
{
	int count, i, j, x ,y;

	wild_gen2_type *w_ptr;

	bool river, clear;
	byte lake_type;

	/* Try LAKE_NUM times */
	for (count = 0; count < LAKE_NUM; count++)
	{
		/* Make a plasma fractal */

		/* Initialise temporary block */
		clear_temp_block();
		set_temp_corner_val(WILD_BLOCK_SIZE * 256);
		set_temp_mid(WILD_BLOCK_SIZE * 64);

		/* Generate plasma factal */
		frac_block();

		/* Get location */
		x = randint1(max_wild - 16 - 1);
		y = randint1(max_wild - 16 - 1);

		/* Clear river flag */
		river = FALSE;

		/* Is the area clear? */
		clear = TRUE;

		/* Look for free space */
		for (i = x; i < x + 16; i++)
		{
			/* Early exit */
			if (!clear) break;

			for (j = y; j < y + 16; j++)
			{
				w_ptr = &wild[j][i].trans;

				/* If non-lake square */
				if (temp_block[j - y][i - x] > WILD_BLOCK_SIZE * 128) continue;

				/* Below sea level? */
				if (w_ptr->hgt_map <= 256 / SEA_FRACTION)
				{
					clear = FALSE;
					break;
				}

				if (w_ptr->info & WILD_INFO_WATER) river = TRUE;
			}
		}

		/* Try again somewhere else */
		if (!clear) continue;

		/* What type of lake do we want? */
		if (river)
		{
			/* Water */
			lake_type = 1;
		}
		else
		{
			w_ptr = &wild[y][x].trans;

			if ((w_ptr->law_map > 64) || (w_ptr->pop_map > 64))
			{
				/* Water if in lawful or populous region */
				lake_type = 1;
			}
			else
			{
				if (w_ptr->hgt_map > 128)
				{
					/* Lava */
					lake_type = 2;
				}
				else
				{
					/* Acid */
					lake_type = 3;
				}
			}
		}

		/* Make the lake */
		for (i = 0; i < 16; i++)
		{
			for (j = 0; j < 16; j++)
			{
				/* If non-lake square */
				if (temp_block[j][i] > WILD_BLOCK_SIZE * 128) continue;

				w_ptr = &wild[j + y][i + x].trans;

				switch (lake_type)
				{
					case 1:
					{
						w_ptr->info |= WILD_INFO_WATER;
						break;
					}
					case 2:
					{
						w_ptr->info |= WILD_INFO_LAVA;
						break;
					}
					case 3:
					{
						w_ptr->info |= WILD_INFO_ACID;
						break;
					}
				}
			}
		}
	}
}


/*
 * Plasma routines used to build the wilderness.
 * These store the results scaled by a factor of 16
 * (Done for a less "griddy" result.)
 */

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
	store_hgtmap(0, 0, randint0(size));
	store_hgtmap(size, 0, randint0(size));
	store_hgtmap(0, size, randint0(size));
	store_hgtmap(size, size, randint0(size));

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
						store_hgtmap(ii, jj, randint1(max_wild * 16));
					}
			   		else
					{
						/* Average of left and right points +random bit */
						store_hgtmap(ii, jj,
						((wild[jj][(i - hstep) >> 4].gen.hgt_map +
						wild[jj][(i + hstep) >> 4].gen.hgt_map) >> 1) +
						((randint1(lstep) - hstep) >> 1));
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
						store_hgtmap(ii, jj, randint1(max_wild * 16));
					}
		   			else
					{
						/* Average of up and down points +random bit */
						store_hgtmap(ii, jj,
						((wild[(j - hstep) >> 4][ii].gen.hgt_map
						+ wild[(j + hstep) >> 4][ii].gen.hgt_map) >> 1)
						+ ((randint1(lstep) - hstep) >> 1));
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
						store_hgtmap(ii, jj, randint1(max_wild * 16));
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
						+ (((randint1(lstep) - hstep) * 181) >> 8));
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
	store_popmap(0, 0, randint0(size), sea);
	store_popmap(size, 0, randint0(size), sea);
	store_popmap(0, size, randint0(size), sea);
	store_popmap(size, size, randint0(size), sea);

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
						store_popmap(ii, jj, randint1(max_wild * 16), sea);
					}
			   		else
					{
						/* Average of left and right points +random bit */
						store_popmap(ii, jj,
						((wild[jj][(i - hstep) >> 4].gen.pop_map +
						wild[jj][(i + hstep) >> 4].gen.pop_map) >> 1) +
						((randint1(lstep) - hstep) >> 1), sea);
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
						store_popmap(ii, jj, randint1(max_wild * 16), sea);
					}
		   			else
					{
						/* Average of up and down points +random bit */
						store_popmap(ii, jj,
						((wild[(j - hstep) >> 4][ii].gen.pop_map
						+ wild[(j + hstep) >> 4][ii].gen.pop_map) >> 1)
						+ ((randint1(lstep) - hstep) >> 1), sea);
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
						store_popmap(ii, jj, randint1(max_wild * 16), sea);
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
						+ (((randint1(lstep) - hstep) * 181) >> 8), sea);
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

	/*
	 * Size is one bigger than normal blocks for speed of
	 * algorithm with 2^n + 1
	 */

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
	store_lawmap(0, 0, randint0(size), sea);
	store_lawmap(size, 0, randint0(size), sea);
	store_lawmap(0, size, randint0(size), sea);
	store_lawmap(size, size, randint0(size), sea);

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
						store_lawmap(ii, jj, randint1(max_wild * 16), sea);
					}
			   		else
					{
						/* Average of left and right points +random bit */
						store_lawmap(ii, jj,
						((wild[jj][(i - hstep) >> 4].gen.law_map +
						wild[jj][(i + hstep) >> 4].gen.law_map) >> 1) +
						((randint1(lstep) - hstep) >> 1), sea);
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
						store_lawmap(ii, jj, randint1(max_wild * 16), sea);
					}
		   			else
					{
						/* Average of up and down points +random bit */
						store_lawmap(ii, jj,
						((wild[(j - hstep) >> 4][ii].gen.law_map
						+ wild[(j + hstep) >> 4][ii].gen.law_map) >> 1)
						+ ((randint1(lstep) - hstep) >> 1), sea);
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
						store_lawmap(ii, jj, randint1(max_wild * 16), sea);
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
						+ (((randint1(lstep) - hstep) * 181) >> 8), sea);
					}
				}
			}
		}
	}
}


/*
 * Finish making the wilderness - recenter the screen around the player.
 */
static void wild_done(void)
{
	p_ptr->px = (s16b)p_ptr->wilderness_x;
	p_ptr->py = (s16b)p_ptr->wilderness_y;

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
	p_ptr->depth = 1;

	/* Not in dungeon yet */
	character_dungeon = FALSE;

	/* Change to the wilderness - but do not light anything yet.*/
	change_level(0);

	/* Change back to inside wilderness */
	p_ptr->depth = 0;

	/* Refresh random number seed */
	wild_grid.wild_seed = randint0(0x10000000);

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
 * No specials yet.
 */
void create_wilderness(void)
{
	int i, j;

	u16b hgt_min, hgt_max, pop_min, pop_max, law_min, law_max;
	byte sea_level;

	byte hgt, pop, law;
	long hgt_scale, pop_scale, law_scale;

	wild_tp_ptr w_ptr;


	/* Test wilderness generation information */
	test_wild_data();

	/* Minimal wilderness */
	if (vanilla_town)
	{
		/* Tiny wilderness */
		max_wild = WILD_GRID_SIZE + 1;

		/* Mega Hack - make an "empty" wilderness. */
		for (i = 0; i < max_wild; i++)
		{
			for (j = 0; j < max_wild; j++)
			{
				/* Mega Hack - Use the 0 value (normally empty) to denote grass. */
				w_ptr = &wild[j][i];

				w_ptr->done.wild = 0;

				/* Nothing interesting here */
				w_ptr->done.info = 0;

				/* No town yet */
				w_ptr->done.town = 0;

				/* Monsters are easy */
				w_ptr->done.mon_gen = 0;

				/* Monsters are fairly common */
				w_ptr->done.mon_prob = 64;
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

	/* Height scale factor */
	hgt_scale = (hgt_max - hgt_min);

	/*
	 * The sea covers 1/SEA_FRACTION of the wilderness
	 */
	sea_level = (byte) (hgt_scale / SEA_FRACTION);

	hgt_min *= 16;

	/* create "population density" information */
	create_pop_map(sea_level * 16 + hgt_min);

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

	create_law_map(sea_level * 16 + hgt_min);

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
			/* Get wilderness grid */
			w_ptr = &wild[j][i];

			/*
			 * Store parameters before change the information
			 * in the union.  (Want to scale values to be 0 - 255)
			 */

			hgt = (byte) ((w_ptr->gen.hgt_map - hgt_min) * 16 / hgt_scale);
			pop = (byte) ((w_ptr->gen.pop_map - pop_min) * 16 / pop_scale);
			law = (byte) ((w_ptr->gen.law_map - law_min) * 16 / law_scale);

			/*
			 * Go to transition data structure
			 */
			w_ptr->trans.hgt_map = hgt;
			w_ptr->trans.pop_map = pop;
			w_ptr->trans.law_map = law;

			/* No town yet */
			w_ptr->trans.town = 0;

			/* Type of town */
			w_ptr->trans.town_type = 0;

			/* No info flags set yet */
			w_ptr->trans.info = 0;
		}
	}


	/*
	 * Add in large level features.
	 */

	/* Add in rivers... */
	create_rivers();

	/* Add in lakes... */
	create_lakes();

	/* Add towns + dungeons etc */
	init_towns();

	/* Connect the towns with roads */
	create_roads();


	/* Convert the wilderness into the final data structure */
	/* Fill wilderness with terrain */
	for (i = 0; i < max_wild; i++)
	{
		for (j = 0; j < max_wild; j++)
		{
			byte town, info, town_type;

			/* Get wilderness grid */
			w_ptr = &wild[j][i];

			/* Save town and info status */
			town = w_ptr->trans.town;
			town_type = w_ptr->trans.town_type;
			info = w_ptr->trans.info;

			/* Get HPL of grid */
			hgt = w_ptr->trans.hgt_map;
			pop = w_ptr->trans.pop_map;
			law = w_ptr->trans.law_map;

			if (hgt < 256 / SEA_FRACTION)
			{
				/* Ocean */
				wild[j][i].done.wild = 65535 - hgt;

				if (hgt > 512 / SEA_FRACTION)
				{
					/* Set to be water boundary */
					w_ptr->done.info = WILD_INFO_WATER;
				}
				else
				{
					/* No rivers / roads / all unknown */
					w_ptr->done.info = 0;
				}
			}
			else
			{
				/* Rescale the height */
				hgt = hgt - 256 / SEA_FRACTION;
				hgt = (hgt * SEA_FRACTION) / (SEA_FRACTION - 1);

				/* Get wilderness type. */
				w_ptr->done.wild = get_gen_type(hgt, pop, law);
			}

			/* Town */
			w_ptr->done.town = town;

			/* Set wilderness monsters if not in town */
			if (!town)
			{
				/* Toughness (level 0 - 64) */
				w_ptr->done.mon_gen = ((256 - law) + (256 - pop)) / 4 ;

				/* No monsters (probability 0 - 16) */
				w_ptr->done.mon_prob = pop / 16;
			}
			else
			{
				/* Set values depending on type of town */
				set_mon_wild_values(town_type, &w_ptr->done);
			}

			/* Info flags */
			w_ptr->done.info = info;
		}
	}


	/* Free up memory used to create the wilderness */
#if 0
	C_FREE(wild_choice_tree, max_w_node, wild_choice_tree_type);
	C_FREE(wild_temp_dist, max_wild, byte);
#endif

	/* Done */
	wild_done();
}
