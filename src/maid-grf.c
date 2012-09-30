
/* File: maid-grf.c */

/* Purpose: Interface for graphical ports */

/*
 * Copyright (c) 2002 S. Fuerst
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "maid-grf.h"

#ifdef SUPPORT_GAMMA

/* Table of gamma values */
byte gamma_table[256];

/* Table of ln(x/256) * 256 for x going from 0 -> 255 */
static const s16b gamma_helper[256] =
{
	0, -1420, -1242, -1138, -1065, -1007, -961, -921, -887, -857, -830, -806,
	-783, -762, -744, -726,
	-710, -694, -679, -666, -652, -640, -628, -617, -606, -596, -586, -576,
	-567, -577, -549, -541,
	-532, -525, -517, -509, -502, -495, -488, -482, -475, -469, -463, -457,
	-451, -455, -439, -434,
	-429, -423, -418, -413, -408, -403, -398, -394, -389, -385, -380, -376,
	-371, -367, -363, -359,
	-355, -351, -347, -343, -339, -336, -332, -328, -325, -321, -318, -314,
	-311, -308, -304, -301,
	-298, -295, -291, -288, -285, -282, -279, -276, -273, -271, -268, -265,
	-262, -259, -257, -254,
	-251, -248, -246, -243, -241, -238, -236, -233, -231, -228, -226, -223,
	-221, -219, -216, -214,
	-212, -209, -207, -205, -203, -200, -198, -196, -194, -192, -190, -188,
	-186, -184, -182, -180,
	-178, -176, -174, -172, -170, -168, -166, -164, -162, -160, -158, -156,
	-155, -153, -151, -149,
	-147, -146, -144, -142, -140, -139, -137, -135, -134, -132, -130, -128,
	-127, -125, -124, -122,
	-120, -119, -117, -116, -114, -112, -111, -109, -108, -106, -105, -103,
	-102, -100, -99, -97,
	-96, -95, -93, -92, -90, -89, -87, -86, -85, -83, -82, -80, -79, -78, -76,
	-75,
	-74, -72, -71, -70, -68, -67, -66, -65, -63, -62, -61, -59, -58, -57, -56,
	-54,
	-53, -52, -51, -50, -48, -47, -46, -45, -44, -42, -41, -40, -39, -38, -37,
	-35,
	-34, -33, -32, -31, -30, -29, -27, -26, -25, -24, -23, -22, -21, -20, -19,
	-18,
	-17, -16, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1
};


/*
 * Build the gamma table so that floating point isn't needed.
 *
 *  ANGBAND_X11_GAMMA is
 * 256 * (1 / gamma), rounded to integer. A recommended value
 * is 183, which is an approximation of the Macintosh hardware
 * gamma of 1.4.
 *
 *   gamma	ANGBAND_X11_GAMMA
 *   -----	-----------------
 *   1.2	213
 *   1.25	205
 *   1.3	197
 *   1.35	190
 *   1.4	183
 *   1.45	177
 *   1.5	171
 *   1.6	160
 *   1.7	151
 *   ...
 *
 * XXX XXX The environment variable, or better,
 * the interact with colours command should allow users
 * to specify gamma values (or gamma value * 100).
 */
void build_gamma_table(int gamma)
{
	int i, n;

	/*
	 * value is the current sum.
	 * diff is the new term to add to the series.
	 */
	long value, diff;

	/* Paranoia */
	if (gamma < 0) gamma = 0;
	if (gamma > 255) gamma = 255;

	/* Hack - convergence is bad in these cases. */
	gamma_table[0] = 0;
	gamma_table[255] = 255;

	for (i = 1; i < 255; i++)
	{
		/*
		 * Initialise the Taylor series
		 *
		 * value and diff have been scaled by 256
		 */

		n = 1;
		value = 256 * 256;
		diff = ((long)gamma_helper[i]) * (gamma - 256);

		while (diff)
		{
			value += diff;
			n++;

			/*
			 * Use the following identiy to calculate the gamma table.
			 * exp(x) = 1 + x + x^2/2 + x^3/(2*3) + x^4/(2*3*4) +...
			 *
			 * n is the current term number.
			 *
			 * The gamma_helper array contains a table of
			 * ln(x/256) * 256
			 * This is used because a^b = exp(b*ln(a))
			 *
			 * In this case:
			 * a is i / 256
			 * b is gamma.
			 *
			 * Note that everything is scaled by 256 for accuracy,
			 * plus another factor of 256 for the final result to
			 * be from 0-255.  Thus gamma_helper[] * gamma must be
			 * divided by 256*256 each iteration, to get back to
			 * the original power series.
			 */
			diff = (((diff / 256) * gamma_helper[i]) *
					(gamma - 256)) / (256 * n);
		}

		/*
		 * Store the value in the table so that the
		 * floating point pow function isn't needed.
		 */
		gamma_table[i] = ((long)(value / 256) * i) / 256;
	}
}

#endif /* SUPPORT_GAMMA */

/*
 * Get the name of the default font to use for the term.
 */
cptr get_default_font(int term_num)
{
	cptr font;

	char buf[80];

	/* Window specific font name */
	strnfmt(buf, 80, "ANGBAND_X11_FONT_%d", term_num);

	/* Check environment for that font */
	font = getenv(buf);

	/* Check environment for "base" font */
	if (!font) font = getenv("ANGBAND_X11_FONT");

	/* No environment variables, use default font */
	if (!font)
	{
		switch (term_num)
		{
			case 0:
			{
				font = DEFAULT_X11_FONT_0;
				break;
			}
			case 1:
			{
				font = DEFAULT_X11_FONT_1;
				break;
			}
			case 2:
			{
				font = DEFAULT_X11_FONT_2;
				break;
			}
			case 3:
			{
				font = DEFAULT_X11_FONT_3;
				break;
			}
			case 4:
			{
				font = DEFAULT_X11_FONT_4;
				break;
			}
			case 5:
			{
				font = DEFAULT_X11_FONT_5;
				break;
			}
			case 6:
			{
				font = DEFAULT_X11_FONT_6;
				break;
			}
			case 7:
			{
				font = DEFAULT_X11_FONT_7;
				break;
			}
			default:
			{
				font = DEFAULT_X11_FONT;
			}
		}
	}

	return (font);
}

#ifdef USE_GRAPHICS
/*
 * Make sure the graphical tiles we want are available.
 *
 * Note - we _must_ be passed an array 1024 in size for the filename.
 */
bool pick_graphics(int graphics, int *xsize, int *ysize, char *filename)
{
	int old_graphics = use_graphics;

	use_graphics = GRAPHICS_NONE;
	use_transparency = FALSE;
	
	if ((graphics == GRAPHICS_ANY) || (graphics == GRAPHICS_DAVID_GERVAIS))
	{
		/* Try the "32x32.bmp" file */
		path_build(filename, 1024, ANGBAND_DIR_XTRA, "graf/32x32.bmp");

		/* Use the "32x32.bmp" file if it exists */
		if (0 == fd_close(fd_open(filename, O_RDONLY)))
		{
			use_transparency = TRUE;

			*xsize = 32;
			*ysize = 32;
		}
		
		use_graphics = GRAPHICS_DAVID_GERVAIS;
		
		/* Did we change the graphics? */
		return (old_graphics != use_graphics);
	}
	
	/* We failed, or we want 16x16 graphics */
	if ((graphics == GRAPHICS_ANY) || (graphics == GRAPHICS_ADAM_BOLT) ||
		 (graphics == GRAPHICS_HALF_3D))
	{
		/* Try the "16x16.bmp" file */
		path_build(filename, 1024, ANGBAND_DIR_XTRA, "graf/16x16.bmp");

		/* Use the "16x16.bmp" file if it exists */
		if (0 == fd_close(fd_open(filename, O_RDONLY)))
		{
			use_transparency = TRUE;

			*xsize = 16;
			*ysize = 16;

			/* Use graphics */
			if (graphics == GRAPHICS_HALF_3D)
			{
				use_graphics = GRAPHICS_HALF_3D;
			}
			else
			{
				use_graphics = GRAPHICS_ADAM_BOLT;
			}
			
			/* Did we change the graphics? */
			return (old_graphics != use_graphics);
		}
	}

	/* We failed, or we want 8x8 graphics */
	if ((graphics == GRAPHICS_ANY) || (graphics == GRAPHICS_ORIGINAL))
	{
		/* Try the "8x8.bmp" file */
		path_build(filename, 1024, ANGBAND_DIR_XTRA, "graf/8x8.bmp");

		/* Use the "8x8.bmp" file if it exists */
		if (0 == fd_close(fd_open(filename, O_RDONLY)))
		{
			/* Use graphics */
			use_graphics = GRAPHICS_ORIGINAL;

			*xsize = 8;
			*ysize = 8;
		}
	}

	/* Did we change the graphics? */
	return (old_graphics != use_graphics);
}

/*
 * Is a square in a bigtiled region?
 */
bool is_bigtiled(int x, int y)
{
	if ((use_bigtile)
		&& (y >= Term->scr->big_y1)
		&& (y <= Term->scr->big_y2)
		&& (x >= Term->scr->big_x1))
	{
		return (TRUE);
	}
	
	return (FALSE);
}

void toggle_bigtile(void)
{
	if (use_bigtile)
	{
		/* Hack - disable bigtile mode */
		Term_bigregion(-1, -1, -1);
		
		use_bigtile = FALSE;
	}
	else
	{
		use_bigtile = TRUE;
	}
	
	/* Hack - redraw everything + recalc bigtile regions */
	angband_term[0]->resize_hook();
}

#endif /* USE_GRAPHICS */


/*
 * The callbacks
 */
static callback_list *callbacks[CALL_MAX];

/*
 * Initialise the callbacks
 */
void init_term_callbacks(void)
{
	/* Wipe the array */
	(void) C_WIPE(callbacks, CALL_MAX, callback_list *);
}

/*
 * Free the callbacks
 */
void free_term_callbacks(void)
{
	int i;
	callback_list *p, *p_next;

	for (i = 0; i < CALL_MAX; i++)
	{
		p = callbacks[i];

		while (p)
		{
			p_next = p->next;
			FREE(p);
			p = p_next;
		}
	}
}

/*
 * Register a callback
 */
void set_callback(callback_type call_func, int number, vptr data)
{
	/* Create a new callback */
	callback_list *node;
	
	MAKE(node, callback_list);
	
	/* Save information into node */
	node->next = callbacks[number];
	node->data = data;
	node->func = call_func;
	
	/* Insert at the head of the list */
	callbacks[number] = node;
}

void del_callback(int number, vptr data)
{
	callback_list **p;
	callback_list *temp;
	
	p = &callbacks[number];
	
	/* Scan the list */
	while (*p)
	{
		/* A match? */
		if ((*p)->data == data)
		{
			/* Delete this node */
			temp = *p;
			*p = (*p)->next;
			FREE(temp);
		
			return;
		}
	
		/* Point to next node */
		p = &((*p)->next);
	}
	
	quit("Callback does not exist");
}

/*
 * Code for the overhead mini-map
 *
 * This is used by the borg to store the map information.
 * It is used by some ports to display a mini-map.  It
 * is used by the tk port to display nearly everything.
 *
 * It is also used by the "overhead map" term type.
 */


/* List of 16x16 blocks for the overhead map */
map_blk_ptr_ptr *map_cache;

/* Refcount for map cache */
s16b *map_cache_refcount;

/* Location of cache blocks */
int *map_cache_x;
int *map_cache_y;

/* The map itself - grid of 16x16 blocks*/
int **map_grid;

/* Player location */
static int player_x = 0;
static int player_y = 0;

/*
 * Access the player location
 */
void map_get_player(int *x, int *y)
{
	*x = player_x;
	*y = player_y;
}

/*
 * Clear the map when changing a level.
 */
static void clear_map(void)
{
	int i, j;

	/* Erase the map */
	for (i = 0; i < WILD_SIZE; i++)
	{
		for (j = 0; j < WILD_SIZE; j++)
		{
			/* Set unused */
			map_grid[i][j] = -1;
		}
	}

	/* Erase the cache */
	for (i = 0; i < MAP_CACHE; i++)
	{
		map_cache_refcount[i] = 0;

		/* Flag that the block isn't used */
		map_cache_x[i] = -1;
	}
}

/*
 * Create the map information
 */
void init_overhead_map(void)
{
	int i, j;

	/* Make the list of pointers to blocks */
	C_MAKE(map_cache, MAP_CACHE, map_blk_ptr_ptr);

	/* Refcount for cache blocks */
	C_MAKE(map_cache_refcount, MAP_CACHE, s16b);

	/* Cache block locations */
	C_MAKE(map_cache_x, MAP_CACHE, int);
	C_MAKE(map_cache_y, MAP_CACHE, int);

	/* Allocate each block */
	for (i = 0; i < MAP_CACHE; i++)
	{
		/* Allocate block */
		C_MAKE(map_cache[i], WILD_BLOCK_SIZE, map_blk_ptr);

		/* Allocate rows of a block */
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			C_MAKE(map_cache[i][j], WILD_BLOCK_SIZE, map_block);
		}
	}

	/* Allocate the overhead map itself */
	C_MAKE(map_grid, WILD_SIZE, int *);

	for (i = 0; i < WILD_SIZE; i++)
	{
		/* Allocate one row of the wilderness */
		C_MAKE(map_grid[i], WILD_SIZE, int);
	}

	/* Initialize */
	clear_map();
}

/*
 * Delete the overhead map
 */
void del_overhead_map(void)
{
	int i, j;

	/* Free refcount for cache blocks */
	FREE(map_cache_refcount);

	/* Cache block locations */
	FREE(map_cache_x);
	FREE(map_cache_y);

	/* Delete each block */
	for (i = 0; i < MAP_CACHE; i++)
	{
		/* Deallocate rows of a block */
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			FREE(map_cache[i][j]);
		}

		/* Free block */
		FREE(map_cache[i]);
	}

	/* Free the list of pointers to blocks */
	FREE(map_cache);

	for (i = 0; i < WILD_SIZE; i++)
	{
		/* Free one row of the wilderness */
		FREE(map_grid[i]);
	}

	/* Free the overhead map itself */
	FREE(map_grid);
}


/*
 * Erase a block
 */
static void clear_block(int block)
{
	int i, j;

	map_block *mb_ptr;

	/* Wipe each square */
	for (i = 0; i < WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			mb_ptr = &map_cache[block][i][j];

			(void)WIPE(mb_ptr, map_block);
		}
	}

	/* Was this used? */
	if (map_cache_x[block] != -1)
	{
		/* Mark map block as unused */
		map_grid[map_cache_y[block]][map_cache_x[block]] = -1;

		/* Set "unused block" flag */
		map_cache_x[block] = -1;
	}
}


/*
 * Find an empty block to use
 */
static int get_empty_block(void)
{
	int i;
	int dist, best_dist = 0;
	int best_block = 0;

	int px, py;

	/* Get player block location */
	px = player_x / 16;
	py = player_y / 16;

	/* Scan for a used but out of los block */
	for (i = 0; i < MAP_CACHE; i++)
	{
		/* Get block out of los */
		if (map_cache_refcount[i]) continue;

		/* Check to see if unused */
		if (map_cache_x[i] < 0)
		{
			best_block = i;
			break;
		}

		/* Get rough dist from player */
		dist = ABS(map_cache_x[i] - px) + ABS(map_cache_y[i] - py);

		/* Save furthest block */
		if (dist > best_dist)
		{
			best_dist = dist;
			best_block = i;
		}
	}

	/* Erase the block */
	clear_block(best_block);

	/* Return the furthest unused block from the player */
	return (best_block);
}


/*
 * Is the location in bounds on the map?
 */
bool map_in_bounds(int x, int y)
{
	if (x < 0 || x >= WILD_BLOCK_SIZE * WILD_SIZE) return (FALSE);
	if (y < 0 || y >= WILD_BLOCK_SIZE * WILD_SIZE) return (FALSE);
	return (map_grid[y >> 4][x >> 4] != -1);
}


/*
 * Save information into a block location
 */
static void save_map_location(int x, int y, const term_map *map)
{
	map_blk_ptr_ptr mbp_ptr;
	map_block *mb_ptr;

	int x1 = x / WILD_BLOCK_SIZE;
	int y1 = y / WILD_BLOCK_SIZE;

	int block_num;
	
	callback_list *callback;

	/* Does the location exist? */
	if (!map_in_bounds(x, y))
	{
		/* Create a new block there */
		block_num = get_empty_block();

		/* Set this block up */
		mbp_ptr = map_cache[block_num];

		/* Link to the map */
		map_grid[y1][x1] = block_num;

		/* Save block coordinates */
		map_cache_x[block_num] = x1;
		map_cache_y[block_num] = y1;
	}
	else
	{
		block_num = map_grid[y1][x1];
		mbp_ptr = map_cache[block_num];
	}

	mb_ptr = &mbp_ptr[y & 15][x & 15];

	/* Increment refcount depending on visibility */
	if (map->flags & MAP_ONCE)
	{
		/* Wasn't seen, and now is */
		if (!(mb_ptr->flags & (MAP_ONCE)))
		{
			map_cache_refcount[block_num]++;
		}
	}
	else
	{
		/* Was seen, and now is not */
		if (mb_ptr->flags & MAP_ONCE)
		{
			/* Paranoia */
			if (!map_cache_refcount[block_num])
			{
				quit("Decrementing invalid overhead map loc");
			}

			map_cache_refcount[block_num]--;
		}
	}
	
	/* Save the tile data */
	mb_ptr->a = map->a;
	mb_ptr->c = map->c;
	mb_ptr->ta = map->ta;
	mb_ptr->tc = map->tc;

	for (callback = callbacks[CALL_MAP_INFO]; callback; callback = callback->next)
	{
		/* Execute the callback */
		((map_info_hook_type)callback->func) (mb_ptr, map, callback->data);
	}

	/* Save the flags */
	mb_ptr->flags = map->flags;

	/* Save the priority */
	mb_ptr->priority = map->priority;

#ifdef TERM_CAVE_MAP

	/* Save the information */
	mb_ptr->terrain = map->terrain;
	mb_ptr->field = map->field;

	mb_ptr->object = map->object;
	mb_ptr->unknown = map->unknown;

	mb_ptr->monster = map->monster;
	mb_ptr->m_flags = map->m_flags;
	mb_ptr->m_hp = map->m_hp;

#endif /* TERM_CAVE_MAP */
}

/*
 * Save the player location
 */
static void set_player_location(int x, int y)
{
	callback_list *callback;

	player_x = x;
	player_y = y;

	/* Tell the port that the player has moved */
	for (callback = callbacks[CALL_PLAYER_MOVE]; callback; callback = callback->next)
	{
		/* Execute the callback */
		((player_move_hook_type)callback->func) (x, y, callback->data);
	}
}


/*
 * Get the information in the map
 */
map_block *map_loc(int x, int y)
{
	return (&map_cache[map_grid[y / WILD_BLOCK_SIZE][x / WILD_BLOCK_SIZE]]
			[y & 15][x & 15]);
}


/* put the banners on the screen */
static void display_banner(wild_done_type *w_ptr)
{
	int wid, hgt;

	place_type *pl_ptr;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Do we have a place here? */
	pl_ptr = (w_ptr->place ? &place[w_ptr->place] : NULL);

	/* Show the place name, if it is on the map */
	if (pl_ptr && (w_ptr->info & WILD_INFO_SEEN))
	{
		cptr banner;
		cptr place_dir;
		int i;

		bool visited_town = FALSE;
		bool home_in_town = FALSE;
		bool castle_in_town = FALSE;

		/* Is it a town */
		if (pl_ptr->numstores)
		{
			/* Upper banner */
			banner = pl_ptr->name;

			/* Display town name */
			put_fstr(1 + (wid - strlen(banner)) / 2, 0, banner);

			/* Find out if there are homes or castles here */
			for (i = 0; i < pl_ptr->numstores; i++)
			{
				store_type *st_ptr = &pl_ptr->store[i];

				/* Is there a home? */
				if (st_ptr->type == BUILD_STORE_HOME) home_in_town = TRUE;

				/* Is there a castle? */
				if (st_ptr->type == BUILD_CASTLE0 ||
					st_ptr->type == BUILD_CASTLE1) castle_in_town = TRUE;

				/* Stores are not given coordinates until you visit a town */
				if (st_ptr->x != 0 && st_ptr->y != 0) visited_town = TRUE;
			}

			/* Prevent knowledge from leaking out */
			home_in_town   &= visited_town;
			castle_in_town &= visited_town;

			/* Find out the lower banner */
			if (home_in_town)
			{
				if (castle_in_town)
				{
					/* Town with home and castle */
					banner = "Move around, press * for town, h for home, c for castle or any key to exit.";
				}
				else
				{
					/* Town with home and no castle */
					banner = "Move around, press * for town, h for home or any key to exit.";
				}
			}
			/* Town with no home */
			else
			{
				if (castle_in_town)
				{
					/* Town with castle and no home */
					banner = "Move around, press * for town, c for castle or any key to exit.";
				}
				else
				{
					/* Town with no castle and no home */
					banner = "Move around, press * for town or any key to exit.";
				}
			}

			/* Display lower banner */
			put_fstr(1 + (wid - strlen(banner)) / 2, hgt - 1, banner);
		}
		/* So it is in the wilderness */
		else
		{
			/* Display standard bottom line */
			put_fstr(wid / 2 - 23, hgt - 1,
					"Move around or hit any other key to continue.");

			/* It is a wilderness dungeon */
			if (pl_ptr->dungeon)
			{
				/* Fetch closest known town and direction */
				banner = describe_quest_location(&place_dir,
								pl_ptr->x, pl_ptr->y, TRUE);

				/* Did the player go into the dungeon? */
				if (pl_ptr->dungeon->recall_depth == 0)
				{
					/* It is still guarded by monsters */
					banner = format("Guarded dungeon %s of %s.", place_dir, banner);
				}
				else
				{
					/* No monsters to guard it */
					banner = format("Unguarded dungeon %s of %s.", place_dir, banner);
				}
			}
			/* It is a wilderness quest */
			else
			{
				/* Fetch wilderness quest name */
				banner = quest[pl_ptr->quest_num].name;
			}

			/* Display wilderness place name */
			put_fstr((wid - strlen(banner)) / 2, 0, banner);
		}
	}
	else
	{
		/* Display standard bottom line */
		put_fstr(wid / 2 - 23, hgt - 1,
				"Move around or hit any other key to continue.");
	}
}


/* Display info about the home in one town */
static bool dump_home_info(FILE *fff, int town)
{
	int i, k;
	bool visited_town = FALSE;

	store_type *st_ptr;
	object_type *o_ptr;

	for (i = 0; i < place[town].numstores; i++)
	{
		st_ptr = &place[town].store[i];

		/* Stores are not given coordinates until you visit a town */
		if (st_ptr->x != 0 && st_ptr->y != 0) visited_town = TRUE;

		/* The only interest is homes */
		if (st_ptr->type == BUILD_STORE_HOME)
		{
			/* Header with name of the town */
			froff(fff, "  [Home Inventory - %s]\n\n", place[i].name);

			/* Home -- if anything there */
			if (st_ptr->stock)
			{
				char o_name[256];
			
				/* Initialise counter */
				k = 0;

				/* Dump all available items */
				OBJ_ITT_START (st_ptr->stock, o_ptr)
				{
					/* Describe object */
					object_desc(o_name, o_ptr, TRUE, 3, 256);
					
					/* Clean formatting escape sequences */
					fmt_clean(o_name);
				
					/* List the item, inlcuding its colour */
					froff(fff, " %s" CLR_SET_DEFAULT " %s\n",
						color_seq[tval_to_attr[o_ptr->tval]], o_name);

					/* Increment counter */
					k++;
				}
				OBJ_ITT_END;

				/* Add an empty line */
				froff(fff, "\n\n");
			}
			/* The home is empty */
			else
			{
				froff(fff, "  [Empty]\n\n");
			}
		}
	}

	/* No home, no show */
	return (visited_town);
}


/* This function predicts whether keystroke c on a town has any effect */
static bool dump_info_test(char c, int town)
{
	int i;
	bool visited_town = FALSE;
	bool build_found = FALSE;

	store_type *st_ptr;

	/* Paranoia */
	if (place[town].numstores == 0) return (FALSE);
	
	/* Find out if this command makes sense */
	switch (c)
	{
		case '*':
		{
			/* Display the list of shops always works */
			return (TRUE);
		}

		case 'h':
		{
			/* Display the items in the home needs a home */
			for (i = 0; i < place[town].numstores; i++)
			{
				st_ptr = &place[town].store[i];

				/* Stores are not given coordinates until you visit a town */
				if (st_ptr->x != 0 && st_ptr->y != 0) visited_town = TRUE;

				/* The only interest is homes */
				if (st_ptr->type == BUILD_STORE_HOME) build_found = TRUE;
			}

			/* Return success */
			return (build_found && visited_town);
		}

		case 'c':
		{
			/* Display the items in the home needs a home */
			for (i = 0; i < place[town].numstores; i++)
			{
				st_ptr = &place[town].store[i];

				/* Stores are not given coordinates until you visit a town */
				if (st_ptr->x != 0 && st_ptr->y != 0) visited_town = TRUE;

				/* The only interest is homes */
				if (st_ptr->type == BUILD_CASTLE0 ||
					st_ptr->type == BUILD_CASTLE1) build_found = TRUE;
			}

			/* Return success */
			return (build_found && visited_town);
		}
	}

	return (FALSE);
}


/* Show the knowledge the player has about a town */
static bool do_cmd_view_map_aux(char c, int town)
{
	FILE *fff;

	char file_name[1024];

	cptr title = NULL;

	/* Call this proc with a place that is a town */
	if (place[town].numstores == 0) return (FALSE);

	/* go away if nothing will happen */
	if (!dump_info_test(c, town)) return (FALSE);

	/* Open temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);

	/* Show what? */
	switch (c)
	{
		case '*':
		{
			/* Display the list of shops */
			dump_town_info(fff, town, FALSE);
			title = "Town info";

			break;
		}

		case 'h':
		{
			/* Display the items in the home */
			(void)dump_home_info(fff, town);
			title = "Home info";

			break;
		}

		case 'c':
		{
			/* Display the quests taken */
			dump_castle_info(fff, town);
			title = "Castle info";

			break;
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, title, 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);

	/* And curtain */
	return (TRUE);
}

/* Keep the offset for the resize */
static int map_cx = 0;
static int map_cy = 0;

static void resize_big_map(void)
{
	int cx, cy;
	wild_done_type *w_ptr;

	cx = map_cx;
	cy = map_cy;

	/* Make a new map */
	display_map(&cx, &cy);

	/* Get wilderness square */
	w_ptr = &wild[map_cy + p_ptr->py / WILD_BLOCK_SIZE]
				 [map_cx + p_ptr->px / WILD_BLOCK_SIZE].done;

	/* print the banners */
	display_banner(w_ptr);

	/* Show the cursor */
	Term_gotoxy(cx, cy);
}

/*
 * Display a "small-scale" map of the dungeon for the player
 *
 * Currently, the "player" is displayed on the map.  XXX XXX XXX
 */
void do_cmd_view_map(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int cy, cx;
	int wid, hgt;

	void (*hook) (void);

	/* No overhead map in vanilla town mode. */
	if (!p_ptr->depth && vanilla_town) return;

	/* Remember what the resize hook was */
	hook = angband_term[0]->resize_hook;

	/* Hack - change the redraw hook so bigscreen works */
	angband_term[0]->resize_hook = resize_big_map;

	/* Note */
	prtf(0, 0, "Please wait...");

	/* Flush */
	Term_fresh();

	/* Clear the screen */
	Term_clear();

	if (p_ptr->depth)
	{
		/* In the dungeon - All we have to do is display the map */

		/* Get size */
		Term_get_size(&wid, &hgt);

		/* No offset from player */
		cx = 0;
		cy = 0;

		/* Match offset for the resize */
		map_cx = cx;
		map_cy = cy;

		/* Display the map */
		display_map(&cx, &cy);

		/* Wait for it */
		put_fstr((wid - COL_MAP) / 2, hgt - 1, "Hit any key to continue");

		/* Hilite the player */
		Term_gotoxy(cx, cy);

		/* Get any key */
		(void)inkey();
	}
	else
	{
		/* Offset from player */
		int x, y;

		/* Direction */
		int d;

		/* Input character */
		char c;
		
		wild_done_type *w_ptr;
		
		/* No offset yet */
		x = 0;
		y = 0;

		/* In the wilderness - Display the map + move it around */

		while (TRUE)
		{
			/* Reset offset of map */
			cx = x;
			cy = y;

			/* Match offset for the resize */
			map_cx = cx;
			map_cy = cy;

			display_map(&cx, &cy);

			/* Get wilderness square */
			w_ptr = &wild[y + py / WILD_BLOCK_SIZE][x + px / WILD_BLOCK_SIZE].done;

			/* Get the banners on the screen */
			display_banner(w_ptr);

			/* Show the cursor */
			Term_gotoxy(cx, cy);

			/* Draw it */
			Term_fresh();

			/* Get a response */
			c = inkey();

			/* Allow a redraw */
			if (c == KTRL('R'))
			{
				/* Do the redraw */
				do_cmd_redraw();

				continue;
			}

			/* On a town?  -- MT */
			if (w_ptr->place)
			{
				/* Check if this is an info command */
				if (do_cmd_view_map_aux(c, w_ptr->place)) continue;
			}

			/* Done if not a direction */
			d = get_keymap_dir(c);

			if (!d) break;

			x += ddx[d];
			y += ddy[d];

			/* Bounds checking */
			if (x + px / WILD_BLOCK_SIZE < 0)
			{
				x = -px / WILD_BLOCK_SIZE;
			}
			if (y + py / WILD_BLOCK_SIZE < 0)
			{
				y = -py / WILD_BLOCK_SIZE;
			}
			if (x + px / WILD_BLOCK_SIZE > max_wild - 2)
			{
				x = max_wild - px / WILD_BLOCK_SIZE - 2;
			}
			if (y + py / WILD_BLOCK_SIZE > max_wild - 2)
			{
				y = max_wild - py / WILD_BLOCK_SIZE - 2;
			}
		}
	}

	/* Hack - change the redraw hook so bigscreen works */
	angband_term[0]->resize_hook = hook;

	/* The size may have changed during the scores display */
	angband_term[0]->resize_hook();

	/* Hack - Flush it */
	Term_fresh();
}



/*
 * Two arrays listing the effects of "brightness"
 * and "darkness" on various "base" colours.
 *
 * This is used to do dynamic lighting effects in ascii :-)
 */

static const byte lighting_colours[16] =
{
	/* TERM_DARK */
	TERM_L_DARK,

	/* TERM_WHITE */
	TERM_YELLOW,

	/* TERM_SLATE */
	TERM_WHITE,

	/* TERM_ORANGE */
	TERM_YELLOW,

	/* TERM_RED */
	TERM_RED,

	/* TERM_GREEN */
	TERM_L_GREEN,

	/* TERM_BLUE */
	TERM_BLUE,

	/* TERM_UMBER */
	TERM_L_UMBER,

	/* TERM_L_DARK */
	TERM_SLATE,

	/* TERM_L_WHITE */
	TERM_WHITE,

	/* TERM_VIOLET */
	TERM_L_RED,

	/* TERM_YELLOW */
	TERM_YELLOW,

	/* TERM_L_RED */
	TERM_L_RED,

	/* TERM_L_GREEN */
	TERM_YELLOW,

	/* TERM_L_BLUE */
	TERM_L_BLUE,

	/* TERM_L_UMBER */
	TERM_L_UMBER,
};

static const byte darking_colours[16] =
{
	/* TERM_DARK */
	TERM_DARK,

	/* TERM_WHITE */
	TERM_SLATE,

	/* TERM_SLATE */
	TERM_L_DARK,

	/* TERM_ORANGE */
	TERM_UMBER,

	/* TERM_RED */
	TERM_RED,

	/* TERM_GREEN */
	TERM_GREEN,

	/* TERM_BLUE */
	TERM_BLUE,

	/* TERM_UMBER */
	TERM_RED,

	/* TERM_L_DARK */
	TERM_L_DARK,

	/* TERM_L_WHITE */
	TERM_SLATE,

	/* TERM_VIOLET */
	TERM_BLUE,

	/* TERM_YELLOW */
	TERM_ORANGE,

	/* TERM_L_RED */
	TERM_L_RED,

	/* TERM_L_GREEN */
	TERM_GREEN,

	/* TERM_L_BLUE */
	TERM_L_BLUE,

	/* TERM_L_UMBER */
	TERM_UMBER
};



#ifdef VARIABLE_PLAYER_GRAPH

/* Magic numbers */
#define BMP_FIRST_PC_CLASS		164
#define BMP_FIRST_PC_RACE		128

static void variable_player_graph(byte *a, char *c)
{
	if (use_graphics != GRAPHICS_ADAM_BOLT)
	{
		if (!streq(ANGBAND_SYS, "ibm"))
		{
			if (use_graphics)
			{
				*a = BMP_FIRST_PC_CLASS + p_ptr->pclass;
				*c = BMP_FIRST_PC_RACE + p_ptr->prace;
			}
		}
		else
		{
			if (use_graphics)
			{
				if (p_ptr->psex == SEX_FEMALE) *c = (char)242;
				switch (p_ptr->pclass)
				{
					case CLASS_PALADIN:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_WHITE;
						else
							*a = TERM_WHITE;
						*c = 253;
						break;
					}
					case CLASS_WARRIOR_MAGE:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_RED;
						else
							*a = TERM_VIOLET;
						break;
					}
					case CLASS_CHAOS_WARRIOR:
					{
						*a = randint1(14);
						break;
					}
					case CLASS_MAGE:
					case CLASS_HIGH_MAGE:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_RED;
						else
							*a = TERM_RED;
						*c = 248;
						break;
					}
					case CLASS_PRIEST:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_BLUE;
						else
							*a = TERM_BLUE;
						*c = 248;
						break;
					}
					case CLASS_RANGER:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_GREEN;
						else
							*a = TERM_GREEN;
						break;
					}
					case CLASS_ROGUE:
					{
						if (p_ptr->lev < 20)
							*a = TERM_SLATE;
						else
							*a = TERM_L_DARK;
						break;
					}
					case CLASS_WARRIOR:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_UMBER;
						else
							*a = TERM_UMBER;
						break;
					}
					case CLASS_MONK:
					case CLASS_MINDCRAFTER:
					{
						if (p_ptr->lev < 20)
							*a = TERM_L_UMBER;
						else
							*a = TERM_UMBER;
						*c = 248;
						break;
					}
					default:
					{
						/* Unknown */
						*a = TERM_WHITE;
					}
				}

				switch (p_ptr->prace)
				{
					case RACE_GNOME:
					case RACE_HOBBIT:
					{
						*c = 144;
						break;
					}
					case RACE_DWARF:
					{
						*c = 236;
						break;
					}
					case RACE_HALF_ORC:
					{
						*c = 243;
						break;
					}
					case RACE_HALF_TROLL:
					{
						*c = 184;
						break;
					}
					case RACE_ELF:
					case RACE_HALF_ELF:
					case RACE_HIGH_ELF:
					{
						*c = 223;
						break;
					}
					case RACE_HALF_OGRE:
					{
						*c = 168;
						break;
					}
					case RACE_HALF_GIANT:
					case RACE_HALF_TITAN:
					case RACE_CYCLOPS:
					{
						*c = 145;
						break;
					}
					case RACE_YEEK:
					{
						*c = 209;
						break;
					}
					case RACE_KLACKON:
					{
						*c = 229;
						break;
					}
					case RACE_KOBOLD:
					{
						*c = 204;
						break;
					}
					case RACE_NIBELUNG:
					{
						*c = 144;
						break;
					}
					case RACE_DARK_ELF:
					{
						*c = 223;
						break;
					}
					case RACE_DRACONIAN:
					{
						if (p_ptr->lev < 20)
							*c = 240;
						else if (p_ptr->lev < 40)
							*c = 22;
						else
							*c = 137;
						break;
					}
					case RACE_MIND_FLAYER:
					{
						*c = 236;
						break;
					}
					case RACE_IMP:
					{
						*c = 142;
						break;
					}
					case RACE_GOLEM:
					{
						*c = 6;
						break;
					}
					case RACE_SKELETON:
					{
						if (p_ptr->pclass == CLASS_MAGE ||
							p_ptr->pclass == CLASS_PRIEST ||
							p_ptr->pclass == CLASS_HIGH_MAGE ||
							p_ptr->pclass == CLASS_MONK ||
							p_ptr->pclass == CLASS_MINDCRAFTER)
							*c = 159;
						else
							*c = 181;
						break;
					}
					case RACE_ZOMBIE:
					case RACE_GHOUL:
					{
						*c = 221;
						break;
					}
					case RACE_VAMPIRE:
					{
						*c = 217;
						break;
					}
					case RACE_SPECTRE:
					{
						*c = 241;
						break;
					}
					case RACE_SPRITE:
					{
						*c = 244;
						break;
					}
					case RACE_BEASTMAN:
					{
						*c = 154;
						break;
					}
				}
			}
		}
	}
}
#endif /* VARIABLE_PLAYER_GRAPH */


/*
 * Hack -- Legal monster codes
 */
static cptr image_monster_hack =
	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";


/*
 * Mega-Hack -- Hallucinatory monster
 */
static void image_monster(byte *ap, char *cp)
{
	int n = strlen(image_monster_hack);

	/* Random symbol from set above */
	if (use_graphics)
	{
		(*cp) = r_info[randint1(z_info->r_max - 1)].x_char;
		(*ap) = r_info[randint1(z_info->r_max - 1)].x_attr;
	}
	else
		/* Text mode */
	{
		(*cp) = (image_monster_hack[randint0(n)]);

		/* Random color */
		(*ap) = randint1(15);
	}
}


/*
 * Hack -- Legal object codes
 */
static cptr image_object_hack = "?/|\\\"!$()_-=[]{},~";


/*
 * Mega-Hack -- Hallucinatory object
 */
static void image_object(byte *ap, char *cp)
{
	int n = strlen(image_object_hack);

	if (use_graphics)
	{
		(*cp) = k_info[randint1(z_info->k_max - 1)].x_char;
		(*ap) = k_info[randint1(z_info->k_max - 1)].x_attr;
	}
	else
	{
		(*cp) = (image_object_hack[randint0(n)]);

		/* Random color */
		(*ap) = randint1(15);
	}
}


/*
 * Hack -- Random hallucination
 */
static void image_random(byte *ap, char *cp)
{
	/* Normally, assume monsters */
	if (randint0(100) < 75)
	{
		image_monster(ap, cp);
	}

	/* Otherwise, assume objects */
	else
	{
		image_object(ap, cp);
	}
}

/*
 * Table of the GF type for each breath
 */
static int breath_gf[32] =
{
	GF_NONE,	/* RF3_SHRIEK */
	GF_NONE,	/* RF3_ELDRITCH_HORROR */
	GF_NONE,	/* RF3_XXX3 */
	GF_NONE,	/* RF3_ROCKET */
	GF_NONE,	/* RF3_ARROW */
	GF_NONE,	/* RF3_XXX6 */
	GF_NONE,	/* RF3_XXX7 */
	GF_NONE,	/* RF3_XXX8 */
	GF_ACID,	/* RF3_BR_ACID */
	GF_ELEC,	/* RF3_BR_ELEC */
	GF_FIRE,	/* RF3_BR_FIRE */
	GF_COLD,	/* RF3_BR_COLD */
	GF_POIS,	/* RF3_BR_POIS */
	GF_NETHER,	/* RF3_BR_NETH */
	GF_LITE,	/* RF3_BR_LITE */
	GF_DARK,	/* RF3_BR_DARK */
	GF_CONFUSION,	/* RF3_BR_CONF */
	GF_SOUND,	/* RF3_BR_SOUN */
	GF_CHAOS,	/* RF3_BR_CHAO */
	GF_DISENCHANT,	/* RF3_BR_DISE */
	GF_NEXUS,	/* RF3_BR_NEXU */
	GF_TIME,	/* RF3_BR_TIME */
	GF_INERTIA,	/* RF3_BR_INER */
	GF_GRAVITY,	/* RF3_BR_GRAV */
	GF_SHARDS,	/* RF3_BR_SHAR */
	GF_PLASMA,	/* RF3_BR_PLAS */
	GF_FORCE,	/* RF3_BR_WALL */
	GF_MANA,	/* RF3_BR_MANA */
	GF_NONE,	/* RF3_BA_NUKE */
	GF_NUKE,	/* RF3_BR_NUKE */
	GF_NONE,	/* RF3_BA_CHAO */
	GF_DISINTEGRATE	/* RF3_BR_DISI */
};


/*
 * Hack -- Get colour based on breaths of monster
 *
 * (This may be a little slow....
 */
static byte breath_attr(const monster_race *r_ptr)
{
	/* Mask out the breath flags */
	u32b flags = r_ptr->flags[3] & RF3_BREATHS;
	u32b mask;

	/* See if we breathe anything at all */
	if (flags)
	{
		byte a;
		char c;

		cptr s;

		int i;
		int prob = 1;
		int choice = 0;

		/* Pick breath */
		for (i = 8, mask = 256; i < 32; i++, mask += mask)
		{
			if (flags & mask)
			{
				/* See if we choose this spell */
				if (one_in_(prob)) choice = i;

				/* Decrease probability of picking next 'spell' */
				prob++;
			}
		}

		/* Paranoia */
		if (choice)
		{
			/* Lookup the default colors for this type */
			s = gf_color[breath_gf[choice]];

			/* Oops */
			if (!s) return (TERM_WHITE);

			/* Pick a random color */
			c = s[randint0(strlen(s))];

			/* Lookup this color */
			a = strchr(color_char, c) - color_char;

			/*
			 * Invalid color (note check for < 0 removed, gave a silly
			 * warning because bytes are always >= 0 -- RG)
			 */
			if (a > 15) return (TERM_WHITE);

			/* Use this color */
			return (a);
		}
	}

	/* Just do any of 7 colours */
	switch (randint1(7))
	{
		case 1: return (TERM_RED);
		case 2: return (TERM_L_RED);
		case 3: return (TERM_WHITE);
		case 4: return (TERM_L_GREEN);
		case 5: return (TERM_BLUE);
		case 6: return (TERM_L_DARK);
		case 7: return (TERM_GREEN);
	}

	/* For the compilers... */
	return (TERM_WHITE);
}


/*
 * Extract tile info for monster.
 *
 * Note how we manually alter the tile type to simulate effects
 * in ascii mode.  We must make sure not to break the images
 * when in (semi) graphics mode, where either the monster
 * or the floor it is standing on is non-ascii.
 */
static void map_mon_info(monster_type *m_ptr, monster_race *r_ptr, byte *a, char *c,
						term_map *map)
{
	byte feat_not_ascii;

	byte ma = *a;
	char mc = *c;

	/* Visible monster */
	if (m_ptr->ml)
	{
		/* Visible monster */
		map->monster = m_ptr->r_idx;

		/* Keep this grid */
		map->flags |= MAP_ONCE;

		/* Get monster information */
		if (m_ptr->csleep) map->m_flags |= MONST_ASLEEP;
		if (is_friendly(m_ptr)) map->m_flags |= MONST_FRIEND;
		if (is_pet(m_ptr)) map->m_flags |= MONST_PET;
		if (m_ptr->confused) map->m_flags |= MONST_CONFUSED;
		if (m_ptr->monfear) map->m_flags |= MONST_FEAR;
		if (m_ptr->stunned) map->m_flags |= MONST_STUN;
		if (m_ptr->invulner) map->m_flags |= MONST_INVULN;

		/* Get scaled monster hp */
		map->m_hp = m_ptr->hp * 10 / m_ptr->maxhp;
			
		/* Hack -- hallucination */
		if (p_ptr->tim.image)
		{
			/* Hallucinatory monster */
			image_monster(a, c);
			return;
		}
		else
		{		
			feat_not_ascii = ((*a) & 0x80);

			/* Desired attr */
			if (!FLAG(r_ptr, RF_ATTR_CLEAR) || feat_not_ascii)
			{
				ma = r_ptr->x_attr;
			}

			/* Desired char */
			if (!FLAG(r_ptr, RF_CHAR_CLEAR) || feat_not_ascii)
			{
				mc = r_ptr->x_char;
			}

			/* Ignore weird codes + graphics */
			if (!(ma & 0x80))
			{
				/* Multi-hued monster */
				if (FLAG(r_ptr, RF_ATTR_MULTI))
				{
					/* Is it a shapechanger? */
					if (FLAG(r_ptr, RF_SHAPECHANGER))
					{
						if (use_graphics)
						{
							mc = r_info[randint1(z_info->r_max - 1)].x_char;
							ma = r_info[randint1(z_info->r_max - 1)].x_attr;
						}
						else
						{
							mc = (one_in_(25) ?
								 image_object_hack[randint0
											   (strlen(image_object_hack))] :
								 image_monster_hack[randint0
												(strlen(image_monster_hack))]);
						}
					}

					/* Multi-hued attr */
					if (FLAG(r_ptr, RF_ATTR_ANY))
						ma = randint1(15);
					else
					{
						/* Pick colour based on breaths */
						ma = breath_attr(r_ptr);
					}
				}
				/* Mimics' colors vary */
				else if (((mc == '\"') || (mc == '!') || (mc == '='))
						 && !FLAG(r_ptr, RF_UNIQUE))
				{
					/* Use char */ ;

					/* Use semi-random attr */
					ma = GET_ARRAY_INDEX(m_list, m_ptr) % 15 + 1;
				}
			}
		}
	}
	
	/* Save results */
	*a = ma;
	*c = mc;
}


/*
 * Extract the attr/char to display at the given (legal) map location
 *
 * Basically, we "paint" the chosen attr/char in several passes, starting
 * with any known "terrain features" (defaulting to darkness), then adding
 * any known "objects", and finally, adding any known "monsters".  This
 * is not the fastest method but since most of the calls to this function
 * are made for grids with no monsters or objects, it is fast enough.
 *
 * Note that the "zero" entry in the feature/object/monster arrays are
 * used to provide "special" attr/char codes, with "monster zero" being
 * used for the player attr/char, "object zero" being used for the "stack"
 * attr/char, and "feature zero" being used for the "nothing" attr/char,
 * though this function makes use of only "feature zero".
 *
 * Note that monsters can have some "special" flags, including "ATTR_MULTI",
 * which means their color changes, and "ATTR_CLEAR", which means they take
 * the color of whatever is under them, and "CHAR_CLEAR", which means that
 * they take the symbol of whatever is under them.  Technically, the flag
 * "CHAR_MIMIC" is supposed to indicate that a monster looks strange when
 * examined, but this flag is currently ignored.
 *
 * Note the effects of hallucination.  Objects always appear as random
 * "objects", monsters as random "monsters", and normal grids occasionally
 * appear as random "monsters" or "objects", but note that these random
 * "monsters" and "objects" are really just "colored ascii symbols".
 *
 * Note the use of the new "terrain feature" information.  Note that the
 * assumption that all interesting "objects" and "terrain features" are
 * memorized allows extremely optimized processing below.  Note the use
 * of separate flags on objects to mark them as memorized allows a grid
 * to have memorized "terrain" without granting knowledge of any object
 * which may appear in that grid.
 *
 * We use the players memorised information to pick the terrain feature
 * to use.  This allows massive simplification - getting rid of all the
 * checks to tha old CAVE_MARK flag, and allowing the player is blind case
 * to be merged into the main line code.  The section picking the terrain
 * attr/feat is now less than a page long - which is important because
 * this routine is a major bottleneck.
 *
 * Note the "special lighting effects" which can be activated for floor
 * grids using the "view_special_lite" option causing certain grids to be
 * displayed using special colors.
 *
 * Note the "special lighting effects" which can be activated for wall
 * grids using the "view_granite_lite" option causing certain grids to be
 * displayed using special colors.
 *
 * The lighting and darkening of colours is handled by two arrays.  We can
 * also lighten and darken some terrains in the 16x16 tileset.
 *
 * Note that bizarre things must be done when the "attr" and/or "char"
 * codes have the "high-bit" set, since these values are used to encode
 * various "special" pictures in some versions, and certain situations,
 * such as "multi-hued" or "clear" monsters, cause the attr/char codes
 * to be "scrambled" in various ways.
 *
 *
 * Save data into the term_map struct so we can pass it to the ports
 * hooking the overhead map code.  The status of the square (x, y) has
 * probably changed.  This allows the main-???.c files to not access
 * internal game data, which may or may not be accessable.
 */

static void map_info(int x, int y, byte *ap, char *cp, byte *tap, char *tcp)
{
	feature_type *f_ptr;

	object_type *o_ptr;
	object_kind *k_ptr;

	monster_type *m_ptr;
	monster_race *r_ptr;

	field_type *fld_ptr;

	/* Get location */
	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);
	
	/* Get the memorized feature */
	byte feat = pc_ptr->feat;
	
	/* Info flags */
	byte player = pc_ptr->player;

	byte a;
	char c;

	s16b halluc = p_ptr->tim.image;
	bool visible = player & GRID_SEEN;
	bool glow = c_ptr->info & CAVE_GLOW;
	bool lite = (c_ptr->info & CAVE_MNLT) || (player & GRID_LITE);
	
	bool float_field = FALSE;
	
	term_map map;
	
	/* Clear map info */
	(void)WIPE(&map, term_map);
	
	/* Save known data */
	map.terrain = feat;
	
	/* Save location */
	map.x = x;
	map.y = y;
	
	/* Default priority */
	map.priority = 0;
	
	/* Pointer to the feature */
	f_ptr = &f_info[feat];
		
	/* Hack -- rare random hallucination, except on outer dungeon walls */
	if (halluc && !p_ptr->tim.blind && (feat != FEAT_PERM_SOLID) && one_in_(256))
	{
		/* Hallucinate */
		image_random(&a, &c);

		if (glow)
		{
			map.flags = MAP_GLOW;
		}
		
		(*tap) = a;
		(*tcp) = c;
	}
	else
	{
		/* Visible */
		if (visible)
		{
			map.flags = MAP_SEEN | MAP_ONCE;

			if (glow) map.flags |= MAP_GLOW;
			if (lite) map.flags |= MAP_LITE;
		}
	
		/* The feats attr */
		a = f_ptr->x_attr;

		/* The feats char */
		c = f_ptr->x_char;

		/*
		 * Look for lighting effects.
		 *
		 * Need to have lighting on and the player is not blind.
		 * We then need to have a grid that is allowed to be lit.
		 */
		if (view_bright_lite && !p_ptr->tim.blind
			&& (!(f_ptr->flags & FF_BLOCK)
				|| (view_granite_lite && !view_torch_grids)))
		{
			/* It's not in view or no lighting effects? */
			if (((!(player & (GRID_VIEW))) && view_special_lite)
				|| !visible)
			{
				/* If is ascii graphics */
				if (a < 16)
				{
					/* Use darkened colour */
					a = darking_colours[a];
				}
				else if ((use_graphics == GRAPHICS_ADAM_BOLT)
						 && (f_ptr->flags & FF_USE_TRANS))
				{
					/* Use a dark tile */
					c++;
				}
			}
			else if (lite && view_yellow_lite)
			{
				/* Use the torch effect */
				if (a < 16)
				{
					/* Use bright colour */
					a = lighting_colours[a];
				}
				else if ((use_graphics == GRAPHICS_ADAM_BOLT)
						 && (f_ptr->flags & FF_USE_TRANS))
				{
					/* Use a light tile */
					c += 2;
				}
			}
		}
		
		/* Save the terrain info for the transparency effects */

		/* Does the feature have "extended terrain" information? */
		if (f_ptr->w_attr)
		{
			/*
			 * Store extended terrain information. 
			 * Note hack to get lighting right.
			 */
			(*tap) = f_ptr->w_attr + a - f_ptr->x_attr;
			(*tcp) = f_ptr->w_char + c - f_ptr->x_char;
		}
		else
		{
			(*tap) = a;
			(*tcp) = c;
		}
	}


	/* Fields */
	FLD_ITT_START (c_ptr->fld_idx, fld_ptr)
	{
		/* Memorized, visible fields */
		if ((fld_ptr->info & (FIELD_INFO_MARK | FIELD_INFO_VIS)) ==
			(FIELD_INFO_MARK | FIELD_INFO_VIS))
		{
			/* Remember field type */
			map.field = fld_ptr->t_idx;

			/* Keep this grid */
			map.flags |= MAP_ONCE;
			
			/* High priority tile */
			map.priority = 20;
		
			/* Which display level to use? */
			if (fld_ptr->info & FIELD_INFO_FEAT)
			{
				/* Terrain level */
				if ((use_graphics == GRAPHICS_ADAM_BOLT)
					&& (fld_ptr->info & (FIELD_INFO_TRANS)))
				{
					/* Take into account dynamic lighting. */
					c += fld_ptr->f_char - f_ptr->x_char;
				}
				else
				{
					/* Normal char */
					c = fld_ptr->f_char;
				}

				/* Normal attr */
				a = fld_ptr->f_attr;

				/* Save the terrain info for the transparency effects */
				(*tap) = a;
				(*tcp) = c;
			}
			else
			{
				/* Tile */
				c = fld_ptr->f_char;
				a = fld_ptr->f_attr;
			
				/* Do we need to look at objects? */
				if (!(fld_ptr->info & (FIELD_INFO_IGNORE)))
				{
					/* Above objects */
					float_field = TRUE;
				}
			}

			/* Done */
			break;
		}
	}
	FLD_ITT_END;
	
	/* Objects */
	OBJ_ITT_START (c_ptr->o_idx, o_ptr)
	{
		/* Memorized objects */
		if (o_ptr->info & (OB_SEEN))
		{
			k_ptr = &k_info[o_ptr->k_idx];

			/* Flavoured object */
			if (k_ptr->flavor)
			{
				/* Save flavor character */
				map.unknown = k_ptr->d_char;
			}
			else
			{
				/* Save object */
				map.object = o_ptr->k_idx;
			}

			/* Keep this grid */
			map.flags |= MAP_ONCE;
			
			/* High priority tile */
			map.priority = 20;
		
			/* A field is obscuring the view to the object */
			if (float_field) break;
			
			/* Hack -- hallucination */
			if (halluc)
			{
				image_object(&a, &c);
			}
			else
			{
				/* Normal char */
				c = object_char(o_ptr);

				/* Normal attr */
				a = object_attr(o_ptr);
			}

			/* Done */
			break;
		}
	}
	OBJ_ITT_END;
		
	
	/* Handle monsters */
	if (c_ptr->m_idx)
	{
		m_ptr = &m_list[c_ptr->m_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Get monster tile info */
		map_mon_info(m_ptr, r_ptr, &a, &c, &map);
		
		/* Not hallucinating and Mimic in los? */
		if (!halluc && visible && !m_ptr->ml && FLAG(r_ptr, RF_CHAR_MIMIC))
		{
			/* Keep this grid */
			map.flags |= MAP_ONCE;

			/* Save mimic character */
			map.unknown = r_ptr->d_char;
		}
		
		/* High priority tile */
		map.priority = 24;
	}

	/* Hack -- fake monochrome */
	if (fake_monochrome)
	{
		if (p_ptr->tim.invuln || !use_color) a = TERM_WHITE;
		else if (p_ptr->tim.wraith_form) a = TERM_L_DARK;
	}
	
	/* Handle "player" */
	if ((x == p_ptr->px) && (y == p_ptr->py))
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		a = r_ptr->x_attr;

		/* Get the "player" char */
		c = r_ptr->x_char;
#ifdef VARIABLE_PLAYER_GRAPH

		variable_player_graph(&a, &c)
#endif /* VARIABLE_PLAYER_GRAPH */

		/* High priority tile */
		map.priority = 50;
	}
	
	/* Save the info */
	(*ap) = a;
	(*cp) = c;
	
	/* Save tile information */
	map.a = a;
	map.c = c;
	map.ta = (*tap);
	map.tc = (*tcp);
	
	/* Save information in map */
	save_map_location(x, y, &map);
}


/*
 * Update the overhead map (used when the visuals change)
 */
void update_overhead_map(void)
{
	map_block *mb_ptr;

	byte a, ta;
	char c, tc;

	int x, y;

	MAP_ITT_START (mb_ptr)
	{
		MAP_GET_LOC(x, y);
		
		if (in_boundsp(x, y))
		{
			/* Update the known tile at the location */
			map_info(x, y, &a, &c, &ta, &tc);
		}
	}
	MAP_ITT_END;
}


/*
 * Erase the map
 */
void Term_erase_map(void)
{
	callback_list *callback;

	/* Notify erasure of the map */
	for (callback = callbacks[CALL_MAP_ERASE]; callback; callback = callback->next)
	{
		/* Execute the callback */
		((map_erase_hook_type)callback->func) (callback->data);
	}

	/* Actually clear the map */
	clear_map();
}


/*
 * Prints the map of the dungeon
 *
 * Note that, for efficiency, we contain an "optimized" version
 * of both "lite_spot()" and "print_rel()", and that we use the
 * "lite_spot()" function to display the player grid, if needed.
 *
 * This function may be called when the cache is wrong.
 */
void prt_map(void)
{
	int x, y;
	int v;

	/* map bounds */
	s16b xmin, xmax, ymin, ymax;

	int wid, hgt;

	byte *pa;
	char *pc;

	byte *pta;
	char *ptc;

	/* Get size */
	get_map_size(&wid, &hgt);

	/* Access the cursor state */
	(void)Term_get_cursor(&v);

	/* Hide the cursor */
	(void)Term_set_cursor(0);
	
	/* Get bounds */
	xmin = p_ptr->panel_x1;
	xmax = p_ptr->panel_x2 - 1;
	ymin = p_ptr->panel_y1;
	ymax = p_ptr->panel_y2 - 1;
		
	/* Pointers to current position in the string */
	pa = mp_a;
	pc = mp_c;

	pta = mp_ta;
	ptc = mp_tc;

	/* Dump the map */
	for (y = ymin; y <= ymax; y++)
	{
		/* Clear the arrays in case panel doesn't fill screen */
		for (x = 0; x < wid; x++)
		{
			mp_a[x] = 0;
			mp_c[x] = 0;
			mp_ta[x] = 0;
			mp_tc[x] = 0;
		}
		
		/* Scan the columns of row "y" */
		for (x = xmin; x <= xmax; x++)
		{
			if (in_bounds2(x, y))
			{
				/* Get map info */
				map_info(x, y, pa, pc, pta, ptc);
			}

			/* Advance */
			pa++;
			pc++;
			pta++;
			ptc++;
		}


		/* Point to start of line */
		pa = mp_a;
		pc = mp_c;
		pta = mp_ta;
		ptc = mp_tc;

		/* Efficiency -- Redraw that row of the map */
		Term_queue_line(COL_MAP, y - p_ptr->panel_y1 + ROW_MAP,
						wid, pa, pc, pta, ptc);
	}

	/* Restore the cursor */
	(void)Term_set_cursor(v);
}


void display_dungeon(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int x, y;
	byte a;
	char c;

	int wid = Term->wid / 2, hgt = Term->hgt / 2;

	byte ta;
	char tc;

	for (x = px - wid + 1; x <= px + wid; x++)
	{
		for (y = py - hgt + 1; y <= py + hgt; y++)
		{
			if (in_boundsp(x, y))
			{
				/* Update this square */
				map_info(x, y, &a, &c, &ta, &tc);

				/* Hack -- Queue it */
				Term_queue_char(x - px + wid - 1, y - py + hgt - 1, a, c, ta,
								tc);
			}
			else
			{
				/* Clear out-of-bound tiles */

				/* Access darkness */
				feature_type *f_ptr = &f_info[FEAT_NONE];

				/* Normal attr */
				a = f_ptr->x_attr;

				/* Normal char */
				c = f_ptr->x_char;

				/* Hack -- Queue it */
				Term_queue_char(x - px + wid - 1, y - py + hgt - 1, a, c, a, c);
			}
		}
	}
}


/*
 * Hack -- priority array (see below)
 *
 * Note that all "walls" always look like "secret doors" (see "map_info()").
 *
 * This really needs to be done a better way.
 */
static const byte priority_table[][2] =
{
	/* Dark */
	{FEAT_NONE, 2},

	/* Floors */
	{FEAT_FLOOR, 5},

	/* Walls */
	{FEAT_SECRET, 10},
	{FEAT_WALL_EXTRA, 10},
	{FEAT_WALL_INNER, 10},
	{FEAT_WALL_OUTER, 10},
	{FEAT_WALL_SOLID, 10},


	/* Perm Walls */
	{FEAT_PERM_EXTRA, 10},
	{FEAT_PERM_INNER, 10},
	{FEAT_PERM_OUTER, 10},
	{FEAT_PERM_SOLID, 10},

	/* Quartz */
	{FEAT_QUARTZ, 11},

	/* Magma */
	{FEAT_MAGMA, 12},

	/* Rubble */
	{FEAT_RUBBLE, 13},

	/* Open doors */
	{FEAT_OPEN, 15},
	{FEAT_BROKEN, 15},

	/* Closed doors */
	{FEAT_CLOSED, 17},

	/* Hidden gold */
	{FEAT_QUARTZ_K, 19},
	{FEAT_MAGMA_K, 19},

	/* water, lava, & trees */
	{FEAT_DEEP_WATER, 20},
	{FEAT_SHAL_WATER, 20},
	{FEAT_DEEP_LAVA, 20},
	{FEAT_SHAL_LAVA, 20},
	{FEAT_DIRT, 6},
	{FEAT_GRASS, 6},
	{FEAT_TREES, 6},
	{FEAT_MOUNTAIN, 20},

	/* Stairs */
	{FEAT_LESS, 25},
	{FEAT_MORE, 25},

	/* End */
	{0, 0}
};


/*
 * Hack -- a priority function (see below)
 */
static byte priority(byte feat)
{
	int i = 0;

	/* Scan the table */
	while (priority_table[i][1])
	{
		/* Does the feature match? */
		if (priority_table[i][0] == feat)
		{
			return (priority_table[i][1]);
		}

		/* Next entry */
		i++;
	}

	/* Default - assume floor */
	return (5);
}


/*
 * Equivalent function to map_info, but for displaying
 * the reduced-size dungeon map.
 *
 * We need to calculate priority as well as the symbols to display.
 *
 * We cheat by getting the symbol recorded previously.
 */
static int display_map_info(int x, int y, char *c, byte *a, char *tc, byte *ta)
{
	int tp;

	byte feat;

	map_block *mb_ptr;
	
	if (!map_in_bounds(x, y))
	{
		/*
		 * Out of bounds 
		 * XXX Hack try anyway. 
		 *
		 * map_info() should bring the square in bounds.
		 */
		map_info(x, y, a, c, ta, tc);
	}
	
	
	/* Get overhead map square */
	mb_ptr = map_loc(x, y);

	/* Default to precalculated priority */
	tp = mb_ptr->priority;
	
	if (!tp)
	{
		/* Get terrain feature */
		feat = parea(x, y)->feat;

		/* Extract the priority of that attr/char */
		tp = priority(feat);
	}
	
	/* Get tile */
	if (mb_ptr->a)
	{
		/* Get attributes from overhead map */
		*a = mb_ptr->a;
		*c = mb_ptr->c;
		*ta = mb_ptr->ta;
		*tc = mb_ptr->tc;
	}
	else
	{
		map_info(x, y, a, c, ta, tc);
	}
	
	/* Return priority */
	return (tp);
}


/*
 * Display a "small-scale" map of the dungeon in the active Term
 *
 * Note that the "map_info()" function must return fully colorized
 * data or this function will not work correctly.
 *
 * Note that this function must "disable" the special lighting
 * effects so that the "priority" function will work.
 *
 * Note the use of a specialized "priority" function to allow this
 * function to work with any graphic attr/char mappings, and the
 * attempts to optimize this function where possible.
 *
 * cx and cy are offsets from the position of the player.  This
 * allows the map to be shifted around - but only works in the
 * wilderness.  cx and cy return the position of the player on the
 * possibly shifted map.
 */
void display_map(int *cx, int *cy)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, j, x, y;

	byte feat;

	byte ta;
	char tc;

	byte tta;
	char ttc;

	byte tp;

	bool road;

	u16b w_type, w_info, twn;

	byte **ma;
	char **mc;

	byte **mp;

	byte **mta;
	char **mtc;

	int hgt, wid, yrat, xrat, xfactor, yfactor;
	
	place_type *pl_ptr;
	
		
	/* Hack - disable bigtile mode */
	if (use_bigtile)
	{
		Term_bigregion(-1, -1, -1);
	}

	/* Get size */
	Term_get_size(&wid, &hgt);
	hgt -= 2;
	wid -= 2;
	
	/* Paranoia */
	if ((hgt < 3) || (wid < 3))
	{
		/*
		 * Need to place the player...
		 * This is wrong, but the map is too small anyway.
		 */
		(*cy) = ROW_MAP;
		(*cx) = COL_MAP;
		return;
	}

	/* Allocate the maps */
	C_MAKE(ma, (hgt + 2), byte *);
	C_MAKE(mc, (hgt + 2), char *);
	C_MAKE(mp, (hgt + 2), byte *);

	C_MAKE(mta, (hgt + 2), byte *);
	C_MAKE(mtc, (hgt + 2), char *);

	/* Allocate and wipe each line map */
	for (i = 0; i < (hgt + 2); i++)
	{
		/* Allocate one row each array */
		C_MAKE(ma[i], (wid + 2), byte);
		C_MAKE(mc[i], (wid + 2), char);
		C_MAKE(mp[i], (wid + 2), byte);

		C_MAKE(mta[i], (wid + 2), byte);
		C_MAKE(mtc[i], (wid + 2), char);
	}

	/* Clear the chars and attributes */
	for (y = 0; y < hgt + 2; ++y)
	{
		for (x = 0; x < wid + 2; ++x)
		{
			/* Nothing here */
			ma[y][x] = TERM_WHITE;
			mc[y][x] = ' ';

			mta[y][x] = TERM_WHITE;
			mtc[y][x] = ' ';

			/* No priority */
			mp[y][x] = 0;
		}
	}

	if (!p_ptr->depth)
	{
		/* Plot wilderness */

		/* work out coords of player in wilderness */
		x = px / 16 + *cx;
		y = py / 16 + *cy;

		/* recenter */
		x = x - wid / 2;
		if (x + wid >= max_wild) x = max_wild - wid - 1;
		if (x < 0) x = 0;

		y = y - hgt / 2;
		if (y + hgt >= max_wild) y = max_wild - hgt - 1;
		if (y < 0) y = 0;

		/* Player location in wilderness */
		(*cy) += py / 16 - y + ROW_MAP;
		(*cx) += px / 16 - x + 1;

		/* Fill in the map */
		for (i = 0; i < wid; ++i)
		{
			for (j = 0; j < hgt; ++j)
			{
				/* Only draw blocks inside map */
				if (((x + i + 1) >= max_wild)
					|| ((y + j + 1) >= max_wild)) continue;

				/* Only draw blocks that have been seen */
				if (!(wild[j + y][i + x].done.info & WILD_INFO_SEEN)) continue;

				w_type = wild[j + y][i + x].done.wild;
				w_info = wild[j + y][i + x].done.info;

				if (w_type < WILD_SEA)
				{
					/* Normal terrain */
					feat = wild_gen_data[w_type].feat;

					/* Allow roads to be drawn */
					road = TRUE;
				}
				else
				{
					feat = FEAT_DEEP_WATER;

					/* No roads please */
					road = FALSE;
				}

				/* Add in effect of other specials */
				if (w_info & (WILD_INFO_WATER))
				{
					feat = FEAT_DEEP_WATER;
				}
				else if (w_info & (WILD_INFO_ACID))
				{
					feat = FEAT_DEEP_ACID;
				}
				else if (w_info & (WILD_INFO_LAVA))
				{
					feat = FEAT_DEEP_LAVA;
				}

				/* This is a nasty hack */

				/* Add in effects of roads */
				if ((w_info & (WILD_INFO_ROAD)) && road)
				{
					ma[j + 1][i + 1] = TERM_UMBER;
					mc[j + 1][i + 1] = '+';
					feat = FEAT_NONE;
				}
				else if ((w_info & (WILD_INFO_TRACK)) && road)
				{
					ma[j + 1][i + 1] = TERM_L_UMBER;
					mc[j + 1][i + 1] = '+';
					feat = FEAT_NONE;
				}

				/* Hack - draw places */
				/* Eventually will get attr,char from place data structure. */

				twn = wild[j + y][i + x].done.place;

				/* If there is a place... */
				if (twn)
				{
					pl_ptr = &place[twn];
				
					switch (place[twn].type)
					{
						case TOWN_QUEST:
						{
							/* Hack make a char / attr from depth */
							wild_type *w_ptr = &wild[pl_ptr->y][pl_ptr->x];

							int depth = (w_ptr->done.mon_gen + 9) / 10;

							if (depth > 9) depth = 9;
							
							/* Quests are red */
							ma[j + 1][i + 1] = TERM_RED;
							mc[j + 1][i + 1] = '0' + depth;
							feat = FEAT_NONE;
							
							break;
						}
						
						case TOWN_DUNGEON:
						{
							/* Hack make a char / attr from depth */
							int depth = (pl_ptr->dungeon->min_level + 9) / 10;
							
							if (depth > 9) depth = 9;

							/* Dungeons are blue */
							ma[j + 1][i + 1] = TERM_L_BLUE;
							mc[j + 1][i + 1] = '0' + depth;
							feat = FEAT_NONE;
							
							break;
						}
						
						default:
						{
							/* Towns are white */
							ma[j + 1][i + 1] = TERM_WHITE;
							mc[j + 1][i + 1] = pl_ptr->name[0];
							feat = FEAT_NONE;
						
							break;
						}
					}
				}

				/* Finally show position of player */
				if ((i + x == px / 16) && (j + y == py / 16))
				{
					ma[j + 1][i + 1] = TERM_WHITE;
					mc[j + 1][i + 1] = '@';
					feat = FEAT_NONE;
				}

				if (feat)
				{
					/* Get attr / char pair for wilderness block type */
					ma[j + 1][i + 1] = f_info[feat].x_attr;
					mc[j + 1][i + 1] = f_info[feat].x_char;

					if (f_info[feat].w_attr)
					{
						mta[j + 1][i + 1] = f_info[feat].w_attr;
						mtc[j + 1][i + 1] = f_info[feat].w_char;
					}
					else
					{
						mta[j + 1][i + 1] = ma[j + 1][i + 1];
						mtc[j + 1][i + 1] = mc[j + 1][i + 1];
					}
				}
			}
		}
	}
	else
	{
		yrat = p_ptr->max_hgt - p_ptr->min_hgt;
		xrat = p_ptr->max_wid - p_ptr->min_wid;

		/* Get scaling factors */
		yfactor = ((yrat / hgt < 4) && (yrat > hgt)) ? 10 : 1;
		xfactor = ((xrat / wid < 4) && (xrat > wid)) ? 10 : 1;

		yrat = (yrat * yfactor + hgt - 1) / hgt;
		xrat = (xrat * xfactor + wid - 1) / wid;

		/* Player location in dungeon */
		(*cy) = py * yfactor / yrat + ROW_MAP;
		(*cx) = px * xfactor / xrat + 1;

		/* Fill in the map of dungeon */
		for (i = p_ptr->min_wid; i < p_ptr->max_wid; ++i)
		{
			for (j = p_ptr->min_hgt; j < p_ptr->max_hgt; ++j)
			{
				/* Location */
				x = i * xfactor / xrat + 1;
				y = j * yfactor / yrat + 1;

				/* Get priority and symbol */
				tp = display_map_info(i, j, &tc, &ta, &ttc, &tta);

				/* Save "best" */
				if (mp[y][x] < tp)
				{
					/* Save the char */
					mc[y][x] = tc;

					/* Save the attr */
					ma[y][x] = ta;

					/* Save the transparency graphic */
					mtc[y][x] = ttc;
					mta[y][x] = tta;

					/* Save priority */
					mp[y][x] = tp;
				}
			}
		}
	}

	/* Corners */
	i = wid + 1;
	j = hgt + 1;

	/* Draw the corners */
	mc[0][0] = '+';
	mc[0][i] = '+';
	mc[j][0] = '+';
	mc[j][i] = '+';

	/* Draw the horizontal edges */
	for (i = 1; i <= wid; i++)
	{
		mc[0][i] = '-';
		mc[j][i] = '-';
	}

	/* Draw the vertical edges */
	for (j = 1; j <= hgt; j++)
	{
		mc[j][0] = '|';
		mc[j][i] = '|';
	}

	/* Display each map line in order */
	for (j = 0; j < hgt + 2; ++j)
	{
		/* Display the line */
		for (i = 0; i < wid + 2; ++i)
		{
			ta = ma[j][i];
			tc = mc[j][i];

			tta = mta[j][i];
			ttc = mtc[j][i];

			/* Hack -- Queue it */
			Term_queue_char(i, j, ta, tc, tta, ttc);
		}
	}

	/* Free each line map */
	for (i = 0; i < (hgt + 2); i++)
	{
		/* Free one row each array */
		FREE(ma[i]);
		FREE(mc[i]);
		FREE(mta[i]);
		FREE(mtc[i]);
		FREE(mp[i]);
	}

	/* Free the maps */
	FREE(ma);
	FREE(mc);
	FREE(mta);
	FREE(mtc);
	FREE(mp);
}


/*
 * Redraw (on the screen) a given MAP location
 *
 * This function should only be called on "legal" grids
 */
void lite_spot(int x, int y)
{
	byte a;
	char c;

	byte ta;
	char tc;

	/* Paranoia */
	if (!character_dungeon) return;

	if (in_boundsp(x, y))
	{
		/* Update this square */
		map_info(x, y, &a, &c, &ta, &tc);

		/* Redraw if on screen */
		if (panel_contains(x, y))
		{
			/* Real coordinates convert to screen positions */
			x -= p_ptr->panel_x1 - COL_MAP;
			y -= p_ptr->panel_y1 - ROW_MAP;

			/* Hack -- Queue it */
			Term_queue_char(x, y, a, c, ta, tc);
		}
	}
}

/*
 * Moves the cursor to a given MAP (x, y) location
 */
void move_cursor_relative(int x, int y)
{
	/* Real coordinates convert to screen positions */
	x -= p_ptr->panel_x1 - COL_MAP;
	y -= p_ptr->panel_y1 - ROW_MAP;

	/* Go there */
	Term_gotoxy(x, y);
}


/*
 * Place an attr/char pair at the given map coordinate, if legal.
 */
void print_rel(char c, byte a, int x, int y)
{
	/* Only do "legal" locations */
	if (panel_contains(x, y))
	{
		/* Hack -- fake monochrome */
		if (fake_monochrome)
		{
			if (p_ptr->tim.invuln || !use_color) a = TERM_WHITE;
			else if (p_ptr->tim.wraith_form) a = TERM_L_DARK;
		}

		/* Real coordinates convert to screen positions */
		x -= p_ptr->panel_x1 - COL_MAP;
		y -= p_ptr->panel_y1 - ROW_MAP;

		/* Draw the char using the attr */
		Term_queue_char(x, y, a, c, a, c);
	}
}

/*
 * The player has moved
 */
void Term_move_player(void)
{
	set_player_location(p_ptr->px, p_ptr->py);
}



#ifdef TERM_USE_LIST

/* Lists of objects on the player */
list_item *equipment;
int equip_num;

list_item *inventory;
int inven_num;


/* Current list (Usually used for stores) */
list_item *cur_list;
int cur_num;


/*
 * Delete the object list
 */
static void delete_list(list_item **l_ptr_ptr, int *num)
{
	int i;

	list_item *l_ptr;

	for (i = 0; i < *num; i++)
	{
		/* Get item */
		l_ptr = &((*l_ptr_ptr)[i]);

		/* Delete strings */
		string_free(l_ptr->o_name);
		string_free(l_ptr->xtra_name);
	}

	/* No more items */
	*num = 0;

	/* Kill list */
	KILL(*l_ptr_ptr);
}

/*
 * Copy a list from t_ptr to l_ptr_ptr
 *
 * The first list comes from the game, the second is
 * stored here for later use by the ports / borg.
 *
 * We assume l_ptr_ptr points to a NULL pointer.
 */
static void copy_list(term_list *t_ptr, int num1, list_item **l_ptr_ptr,
                      int *num2)
{
	list_item *l_ptr;
	term_list *tl_ptr;

	int i;

	/* Paranoia */
	if (*l_ptr_ptr) quit("Trying to copy over an allocated list.");

	/* We don't need to make an empty list */
	if (!num1) return;

	/* Save number of items in list */
	*num2 = num1;

	/* Create the list */
	C_MAKE(*l_ptr_ptr, num1, list_item);

	for (i = 0; i < num1; i++)
	{
		l_ptr = &((*l_ptr_ptr)[i]);
		tl_ptr = &t_ptr[i];

		/* Duplicate flags */
		l_ptr->kn_flags[0] = tl_ptr->kn_flags[0];
		l_ptr->kn_flags[1] = tl_ptr->kn_flags[1];
		l_ptr->kn_flags[2] = tl_ptr->kn_flags[2];
		l_ptr->kn_flags[3] = tl_ptr->kn_flags[3];

		/* Duplicate cost */
		l_ptr->cost = tl_ptr->cost;

		/* Duplicate item type and weight */
		l_ptr->k_idx = tl_ptr->k_idx;
		l_ptr->weight = tl_ptr->weight;
		l_ptr->number = tl_ptr->number;

		/* Duplicate info */
		l_ptr->info = tl_ptr->info;

		/* Duplicate pval /tval */
		l_ptr->pval = tl_ptr->pval;
		l_ptr->tval = tl_ptr->tval;

		/* Duplicate timeout */
		l_ptr->timeout = tl_ptr->timeout;

		/* Duplicate bonuses */
		l_ptr->to_h = tl_ptr->to_h;
		l_ptr->to_d = tl_ptr->to_d;
		l_ptr->to_a = tl_ptr->to_a;

		/* Duplicate AC */
		l_ptr->ac = tl_ptr->ac;

		/* Duplicate Dice */
		l_ptr->dd = tl_ptr->dd;
		l_ptr->ds = tl_ptr->ds;

		/* Duplicate strings */
		l_ptr->o_name = string_make(tl_ptr->o_name);
		l_ptr->xtra_name = string_make(tl_ptr->xtra_name);
	}
}

/*
 * Save the object list so that the port can access it.
 */
static void save_object_list(term_list *l_ptr, int num, byte list_type)
{
	callback_list *callback;

	if (list_type == LIST_INVEN)
	{
		/* Delete old inventory list */
		delete_list(&inventory, &inven_num);

		/* Copy over with the new list */
		copy_list(l_ptr, num, &inventory, &inven_num);
	}

	if (list_type == LIST_EQUIP)
	{
		/* Delete old equipment list */
		delete_list(&equipment, &equip_num);

		/* Copy over with the new list */
		copy_list(l_ptr, num, &equipment, &equip_num);
	}

	if ((list_type == LIST_STORE) || (list_type == LIST_HOME))
	{
		/* Delete the old current list */
		delete_list(&cur_list, &cur_num);

		/* Copy over with the new list */
		copy_list(l_ptr, num, &cur_list, &cur_num);
	}

	/* Notify port */
	for (callback = callbacks[CALL_OBJECT_LIST]; callback; callback = callback->next)
	{
		/* Execute the callback */
		((list_notice_hook_type)callback->func) (list_type, callback->data);
	}
}

/*
 * Set the basic object flags to send to the port
 */
static void set_basic_flags(term_list *l_ptr, object_type *o_ptr, bool in_store)
{
	object_flags oflags;

	/* Known flags */
	object_flags_known(o_ptr, &oflags);
	
	l_ptr->kn_flags[0] = oflags.flags[0];
	l_ptr->kn_flags[1] = oflags.flags[1];
	l_ptr->kn_flags[2] = oflags.flags[2];
	l_ptr->kn_flags[3] = oflags.flags[3];

	/* Type of object */
	if (object_aware_p(o_ptr) || in_store)
	{
		l_ptr->k_idx = o_ptr->k_idx;
	}

	/* Weight and number */
	l_ptr->weight = o_ptr->weight;
	l_ptr->number = o_ptr->number;

	/* Save information */
	l_ptr->info = o_ptr->info;

	/* Hack - Save cost (If not in a store, this will be inaccurate) */
	l_ptr->cost = o_ptr->temp_cost;

	/* Do we have extra name information? */
	if (object_known_p(o_ptr) && (o_ptr->xtra_name))
	{
		l_ptr->xtra_name = string_make(quark_str(o_ptr->xtra_name));
	}
	else
	{
		l_ptr->xtra_name = NULL;
	}

	/* Damage dice */
	l_ptr->dd = o_ptr->dd;
	l_ptr->ds = o_ptr->ds;

	/* Hack - only send AC information if isn't a wand */
	if (o_ptr->tval != TV_WAND)
	{
		l_ptr->ac = o_ptr->ac;
	}

	/* Identified items yield extra information */
	if (object_known_p(o_ptr))
	{
		/* Bonuses */
		l_ptr->to_h = o_ptr->to_h;
		l_ptr->to_d = o_ptr->to_d;
		l_ptr->to_a = o_ptr->to_a;

		/* Pval */
		if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
		{
			/* Wand and staff charges */
			l_ptr->pval = o_ptr->pval;
		}
	}

	/* Pval */
	if (KN_FLAG(o_ptr, TR_PVAL_MASK))
	{
		/* Normal items with noticable pval */
		l_ptr->pval = o_ptr->pval;
	}

	/* Tval */
	l_ptr->tval = o_ptr->tval;

	/* Timeout */
	if (o_ptr->tval == TV_LITE)
	{
		/* Lights have "obvious" timeouts */
		l_ptr->timeout = o_ptr->timeout;
	}
	else
	{
		/* Rods can charge in piles */
		if ((o_ptr->number > 1) && (o_ptr->tval == TV_ROD))
		{
			int power;
			object_kind *k_ptr = &k_info[o_ptr->k_idx];

			/*
			 * Find out how many rods are charging.
			 */
			power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
			if (power > o_ptr->number) power = o_ptr->number;

			/* Hack - Set timeout to number of charging items */
			l_ptr->timeout = power;
		}
		else
		{
			/* Are we charging? */
			l_ptr->timeout = o_ptr->timeout ? 1 : 0;
		}
	}
}

/*
 * Write out the equipment so that the ports can access it.
 *
 * This is equivalent to Term_write_list() below, except with
 * a static array of objects rather than a list.
 */
void Term_write_equipment(void)
{
	term_list *list, *l_ptr;

	int i;
	object_type *o_ptr;
	char o_name[256];

	/* Create the list */
	C_MAKE(list, EQUIP_MAX, term_list);

	/* Fill with information */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		/* Get object */
		o_ptr = &p_ptr->equipment[i];

		if (!o_ptr->k_idx) continue;

		/* Get object list element */
		l_ptr = &list[i];

		/* Set object flags */
		set_basic_flags(l_ptr, o_ptr, FALSE);

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3, 256);

		l_ptr->o_name = string_make(o_name);
	}

	/* Save for later */
	save_object_list(list, EQUIP_MAX, LIST_EQUIP);

	for (i = 0; i < EQUIP_MAX; i++)
	{
		l_ptr = &list[i];

		/* Free the strings */
		string_free(l_ptr->o_name);
		string_free(l_ptr->xtra_name);
	}

	/* Free the list */
	FREE(list);
}


/*
 * Angband-specific code used to send a list of objects to the port.
 * This allows in-game data to be read.
 *
 * Hack - we assume every object in the list is 'visible'
 */
void Term_write_list(s16b o_idx, byte list_type)
{
	term_list *list, *l_ptr;

	int i = 0;
	object_type *o_ptr;
	char o_name[256];

	/* Get list length */
	int num = get_list_length(o_idx);

	/* Empty? */
	if (!num)
	{
		/* We have an empty list */
		save_object_list(NULL, 0, list_type);

		return;
	}

	/* Create the list */
	C_MAKE(list, num, term_list);

	/* Fill with information */
	OBJ_ITT_START (o_idx, o_ptr)
	{
		/* Get object list element */
		l_ptr = &list[i];

		/* Stores are special */
		if (list_type == LIST_STORE)
		{
			/* Set object flags */
			set_basic_flags(l_ptr, o_ptr, TRUE);
		
			/* Describe the object */
			object_desc_store(o_name, o_ptr, TRUE, 3, 256);
		}
		else
		{
			/* Set object flags */
			set_basic_flags(l_ptr, o_ptr, FALSE);
		
			/* Describe the object */
			object_desc(o_name, o_ptr, TRUE, 3, 256);
		}

		l_ptr->o_name = string_make(o_name);

		/* Increment counter */
		i++;
	}
	OBJ_ITT_END;

	/* Save for later */
	save_object_list(list, num, list_type);

	for (i = 0; i < num; i++)
	{
		l_ptr = &list[i];

		/* Free the strings */
		string_free(l_ptr->o_name);
		string_free(l_ptr->xtra_name);
	}

	/* Free the list */
	FREE(list);
}

#else  /* TERM_USE_LIST */

/*** Make generic do-nothing functions ***/

void Term_write_equipment(void)
{
	/* Do nothing */
}

void Term_write_list(s16b o_idx, byte list_type)
{
	/* Ignore parameters */
	(void)o_idx;
	(void)list_type;

	/* Do nothing */
}

#endif /* TERM_USE_LIST */

