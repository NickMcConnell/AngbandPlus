
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
	sprintf(buf, "ANGBAND_X11_FONT_%d", term_num);

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
bool pick_graphics(int graphics, int *xsize, int *ysize, char *filename)
{
	int old_graphics = use_graphics;

	use_graphics = GRAPHICS_NONE;
	use_transparency = FALSE;

	if ((graphics == GRAPHICS_ANY)
		|| (graphics == GRAPHICS_ADAM_BOLT) || (graphics == GRAPHICS_HALF_3D))
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
		}
	}

	/* We failed, or we want 8x8 graphics */
	if (!use_graphics
		&& ((graphics == GRAPHICS_ANY) || (graphics == GRAPHICS_ORIGINAL)))
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
	if (old_graphics == use_graphics) return (FALSE);

	/* Success */
	return (TRUE);
}
#endif /* USE_GRAPHICS */

#ifdef TERM_USE_CALLBACKS

/*
 * The callbacks
 */
static callback_type *callbacks;

/*
 * Initialise the callbacks
 */
void init_term_callbacks(void)
{
	/* Create and wipe the array */
	C_MAKE(callbacks, CALL_MAX, callback_type);
}

/*
 * Free the callbacks
 */
void free_term_callbacks(void)
{
	/* Deallocate the array */
	FREE(callbacks);
}

/*
 * Register a callback
 */
callback_type set_callback(callback_type call_func, int number)
{
	/* Save the old callback */
	callback_type temp = callbacks[number];

	/* Register the new callback */
	callbacks[number] = call_func;

	/* Return the old callback to chain into */
	return (temp);
}

#else  /* TERM_USE_CALLBACKS */

/*
 * Initialise the callbacks
 */
void init_term_callbacks(void)
{
	/* Do nothing */
}

/*
 * Free the callbacks
 */
void free_term_callbacks(void)
{
	/* Do nothing */
}


#endif /* TERM_USE_CALLBACKS */


#ifdef TERM_USE_MAP

static bool map_init = FALSE;

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

	/* Do not initialize twice */
	if (map_init) return;

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

	/* The map exists */
	map_init = TRUE;

	/* Initialize */
	clear_map();
}

/*
 * Delete the overhead map
 */
void del_overhead_map(void)
{
	int i, j;

	/* Do not remove twice */
	if (!map_init) return;

	/* Free refcount for cache blocks */
	FREE(map_cache_refcount);

	/* Cache block locations */
	FREE(map_cache_x);
	FREE(map_cache_y);

	/* Delete each block */
	for (i = 0; i < MAP_CACHE; i++)
	{
		/* Allocate rows of a block */
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

	/* The map no longer exists */
	map_init = FALSE;
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
static void save_map_location(int x, int y, term_map *map)
{
	map_blk_ptr_ptr mbp_ptr;
	map_block *mb_ptr;

	int x1 = x / WILD_BLOCK_SIZE;
	int y1 = y / WILD_BLOCK_SIZE;

	int block_num;

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

	/* Remember info by calling hook */
	if (callbacks[CALL_MAP_INFO])
	{
		/* Execute the callback */
		((map_info_hook_type)callbacks[CALL_MAP_INFO]) (mb_ptr, map);
	}

	/* Save the flags */
	mb_ptr->flags = map->flags;

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
	player_x = x;
	player_y = y;

	/* Tell the port that the player has moved */
	/* Remember info by calling hook */
	if (callbacks[CALL_PLAYER_MOVE])
	{
		/* Execute the callback */
		((player_move_hook_type) callbacks[CALL_PLAYER_MOVE]) (x, y);
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


/*
 * Angband-specific code designed to allow the map to be sent
 * to the port as required.  This allows the main-???.c file
 * not to access internal game data, which may or may not
 * be accessable.
 */
void Term_write_map(int x, int y, cave_type *c_ptr, pcave_type *pc_ptr)
{
	term_map map;

	int fld_idx, next_f_idx;

	monster_type *m_ptr;
	object_type *o_ptr;
	field_type *fld_ptr;

	monster_race *r_ptr;
	object_kind *k_ptr;

	bool visible = pc_ptr->player & GRID_SEEN;
	bool glow = c_ptr->info & CAVE_GLOW;
	bool lite = (c_ptr->info & CAVE_MNLT) || (pc_ptr->player & GRID_LITE);

	/* Paranoia */
	if (!map_init) return;

	/* clear map info */
	(void)WIPE(&map, term_map);

	/* Save known data */
	map.terrain = pc_ptr->feat;

	/* Visible, and not hallucinating */
	if (visible && !p_ptr->image)
	{
		map.flags = MAP_SEEN | MAP_ONCE;

		if (glow) map.flags |= MAP_GLOW;
		if (lite) map.flags |= MAP_LITE;
	}

	/* Not hallucinating */
	if (!p_ptr->image)
	{
		/* Save known monsters */
		if (c_ptr->m_idx)
		{
			m_ptr = &m_list[c_ptr->m_idx];

			/* Visible monster */
			if (m_ptr->ml)
			{
				map.monster = m_ptr->r_idx;

				/* Keep this grid */
				map.flags |= MAP_ONCE;

				/* Get monster information */
				if (m_ptr->csleep) map.m_flags |= MONST_ASLEEP;
				if (is_friendly(m_ptr)) map.m_flags |= MONST_FRIEND;
				if (is_pet(m_ptr)) map.m_flags |= MONST_PET;
				if (m_ptr->confused) map.m_flags |= MONST_CONFUSED;
				if (m_ptr->monfear) map.m_flags |= MONST_FEAR;
				if (m_ptr->stunned) map.m_flags |= MONST_STUN;
				if (m_ptr->invulner) map.m_flags |= MONST_INVULN;

				/* Get scaled monster hp */
				map.m_hp = m_ptr->hp * 10 / m_ptr->maxhp;
			}

			/* Mimic in los? */
			else if (visible)
			{
				r_ptr = &r_info[m_ptr->r_idx];

				if (r_ptr->flags1 & RF1_CHAR_MIMIC)
				{
					/* Keep this grid */
					map.flags |= MAP_ONCE;

					/* Save mimic character */
					map.unknown = r_ptr->d_char;
				}
			}
		}

		/* Fields */
		for (fld_idx = c_ptr->fld_idx; fld_idx; fld_idx = next_f_idx)
		{
			/* Acquire field */
			fld_ptr = &fld_list[fld_idx];

			/* Acquire next field */
			next_f_idx = fld_ptr->next_f_idx;

			/* Memorized, visible fields */
			if ((fld_ptr->info & (FIELD_INFO_MARK | FIELD_INFO_VIS)) ==
				(FIELD_INFO_MARK | FIELD_INFO_VIS))
			{
				map.field = fld_ptr->t_idx;

				/* Keep this grid */
				map.flags |= MAP_ONCE;

				/* Stop looking */
				break;
			}
		}

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

				/* Stop looking */
				break;
			}
		}
		OBJ_ITT_END;
	}
	else
	{
		map.flags = glow ? MAP_GLOW : 0;
	}

	/* Save location */
	map.x = x;
	map.y = y;

	/* Save information in map */
	save_map_location(x, y, &map);
}

/*
 * Erase the map
 */
void Term_erase_map(void)
{
	/* Paranoia */
	if (!map_init) return;

	/* Notify erasure of the map */
	if (callbacks[CALL_MAP_ERASE])
	{
		/* Execute the callback */
		((map_erase_hook_type)callbacks[CALL_MAP_ERASE]) ();
	}

	/* Actually clear the map */
	clear_map();
}

/*
 * The player has moved
 */
void Term_move_player(void)
{
	set_player_location(p_ptr->px, p_ptr->py);
}

#else  /* TERM_USE_MAP */

/*** Make generic do-nothing functions ***/


void Term_write_map(int x, int y, cave_type *c_ptr, pcave_type *pc_ptr)
{
	/* Ignore all parameters */
	(void)x;
	(void)y;
	(void)c_ptr;
	(void)pc_ptr;

	/* Do nothing */
}

void Term_erase_map(void)
{
	/* Do nothing */
}

void Term_move_player(void)
{
	/* Do nothing */
}

#endif /* TERM_USE_MAP */


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
		l_ptr->kn_flags1 = tl_ptr->kn_flags1;
		l_ptr->kn_flags2 = tl_ptr->kn_flags2;
		l_ptr->kn_flags3 = tl_ptr->kn_flags3;

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
	if (callbacks[CALL_OBJECT_LIST])
	{
		/* Execute the callback */
		((list_notice_hook_type)callbacks[CALL_OBJECT_LIST]) (list_type);
	}
}

/*
 * Set the basic object flags to send to the port
 */
static void set_basic_flags(term_list *l_ptr, object_type *o_ptr, bool in_store)
{
	/* Known flags */
	object_flags_known(o_ptr, &l_ptr->kn_flags1,
					   &l_ptr->kn_flags2, &l_ptr->kn_flags3);

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
	if (o_ptr->kn_flags1 & (TR1_PVAL_MASK))
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
