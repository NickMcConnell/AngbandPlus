/* File: generate.c */

/* Purpose: Wilderness related things */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Various defines for the wilderness
 */
#define DUN_WILD_VAULT          50      /* Chance of finding a wilderness vault. */


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
int generate_area(int y, int x, bool border, bool corner, bool refresh)
{
        int road, entrance;
	int x1, y1;
        int hack_floor = 0;

	/* Number of the town (if any) */
        p_ptr->town_num = wf_info[wild_map[y][x].feat].entrance;
        if (!p_ptr->town_num) p_ptr->town_num = wild_map[y][x].entrance;

	{
		int roughness = 1; /* The roughness of the level. */
		int terrain[3][3]; /* The terrain around the current area */
                int ym, xm, yp, xp;

                /* Place the player at the center */
                if(!p_ptr->oldpx) p_ptr->oldpx = MAX_WID / 2;
                if(!p_ptr->oldpy) p_ptr->oldpy = MAX_HGT / 2;

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

        /* Should we create a town ? */
        if ((p_ptr->town_num > 0) && (p_ptr->town_num < 1000))
	{
		/* Create the town */
		int xstart = 0;
		int ystart = 0;

		/* Initialize the town */
		init_flags = INIT_CREATE_DUNGEON;
                process_dungeon_file("t_info.txt", &ystart, &xstart, cur_hgt, cur_wid);
	}
        else
        {
                /* Reset the town flag */
                p_ptr->town_num = 0;
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
                        cave[dy][dx].info |= (CAVE_GLOW | CAVE_MARK);
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

                /* Show it if we know it */
                if (wild_map[j][i].known)
                {
                        cave[j][i].info |= (CAVE_GLOW | CAVE_MARK);
                }
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

/* Show a small radius of wilderness around the player */
void reveal_wilderness_around_player(int y, int x, int h, int w)
{
        int i, j;

        /* Circle or square ? */
        if (h == 0)
        {
                for (i = x - w; i < x + w; i++)
                {
                        for (j = y - w; j < y + w; j++)
                        {
                                /* Bound checking */
                                if (!in_bounds(j, i)) continue;

                                /* We want a radius, not a "squarus" :) */
                                if (distance(y, x, j, i) >= w) continue;

                                /* New we know here */
                                wild_map[j][i].known = TRUE;

                                /* Only if we are in overview */
                                if (p_ptr->wild_mode)
                                {
                                        cave[j][i].info |= (CAVE_GLOW | CAVE_MARK);

                                        /* Show it */
                                        lite_spot(j, i);
                                }
                        }
                }
        }
        else
        {
                for (i = x; i < x + w; i++)
                {
                        for (j = y; j < y + h; j++)
                        {
                                /* Bound checking */
                                if (!in_bounds(j, i)) continue;

                                /* New we know here */
                                wild_map[j][i].known = TRUE;

                                /* Only if we are in overview */
                                if (p_ptr->wild_mode)
                                {
                                        cave[j][i].info |= (CAVE_GLOW | CAVE_MARK);

                                        /* Show it */
                                        lite_spot(j, i);
                                }
                        }
                }
        }
}
