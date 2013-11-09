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
 * Fill the arrays of floors and walls in the good proportions
 */
void set_floor_and_wall(byte type)
{
	static byte cur_type = 255;
	int i;

	/* Already filled */
	if (cur_type == type) return;

	cur_type = type;

	for (i = 0; i < 100; i++)
	{
		int lim1, lim2, lim3;

		lim1 = d_info[type].floor_percent1;
		lim2 = lim1 + d_info[type].floor_percent2;
		lim3 = lim2 + d_info[type].floor_percent3;

		if (i < lim1)
			floor_type[i] = d_info[type].floor1;
		else if (i < lim2)
			floor_type[i] = d_info[type].floor2;
		else if (i < lim3)
			floor_type[i] = d_info[type].floor3;

		lim1 = d_info[type].fill_percent1;
		lim2 = lim1 + d_info[type].fill_percent2;
		lim3 = lim2 + d_info[type].fill_percent3;
		if (i < lim1)
			fill_type[i] = d_info[type].fill_type1;
		else if (i < lim2)
			fill_type[i] = d_info[type].fill_type2;
		else if (i < lim3)
			fill_type[i] = d_info[type].fill_type3;
	}
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
	int tmp = randint1(tmp2) - (rough + 1);

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
	int tmp2 = rough * 2 + 1;
	int tmp = randint0(tmp2) - rough;

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
 * Note that it uses ``cave_feat'' as temporary storage.
 * The values in ``cave_feat'' after this function
 * are NOT actual features; They are raw heights which
 * need to be converted to features.
 */
static void plasma_recursive(int x1, int y1, int x2, int y2,
			     int depth_max, int rough)
{
	/* Find middle */
	int xmid = (x2 - x1) / 2 + x1;
	int ymid = (y2 - y1) / 2 + y1;

	/* Are we done? */
	if (x1 + 1 == x2) return;

	perturb_point_mid(cave[y1][x1].feat, cave[y2][x1].feat, cave[y1][x2].feat,
		cave[y2][x2].feat, xmid, ymid, rough, depth_max);

	perturb_point_end(cave[y1][x1].feat, cave[y1][x2].feat, cave[ymid][xmid].feat,
		xmid, y1, rough, depth_max);

	perturb_point_end(cave[y1][x2].feat, cave[y2][x2].feat, cave[ymid][xmid].feat,
		x2, ymid, rough, depth_max);

	perturb_point_end(cave[y2][x2].feat, cave[y2][x1].feat, cave[ymid][xmid].feat,
		xmid, y2, rough, depth_max);

	perturb_point_end(cave[y2][x1].feat, cave[y1][x1].feat, cave[ymid][xmid].feat,
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
			FEAT_DIRT,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,

			FEAT_TREES,
			FEAT_DEEP_GRASS,
			FEAT_SHAL_WATER,

			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,
			FEAT_SHAL_WATER,

			FEAT_SWAMP,
			FEAT_SWAMP,
			FEAT_SWAMP,

			FEAT_SWAMP,
			FEAT_SWAMP,
			FEAT_SWAMP,
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
			FEAT_FLOWER,
			FEAT_DEEP_GRASS,

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
			FEAT_FLOWER,
			FEAT_DEEP_GRASS,

			FEAT_DEEP_GRASS,
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
			FEAT_DEEP_GRASS,

			FEAT_DEEP_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,
	},
	/* TERRAIN_DESERT */
	{
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
			FEAT_DIRT,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_GRASS,
	},
	/* TERRAIN_SHALLOW_LAVA */
	{
			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,

			FEAT_SHAL_LAVA,
			FEAT_SHAL_LAVA,
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
			FEAT_MOUNTAIN,
	},
	/* TERRAIN_DEEP_LAVA */
	{
			FEAT_DIRT,
			FEAT_DIRT,
			FEAT_DIRT,

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
			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,
	},
	/* TERRAIN_MOUNTAIN */
	{
			FEAT_FLOOR,
			FEAT_DEEP_GRASS,
			FEAT_GRASS,

			FEAT_GRASS,
			FEAT_DIRT,
			FEAT_DIRT,

			FEAT_TREES,
			FEAT_TREES,
			FEAT_MOUNTAIN,

			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,

			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,

			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,
			FEAT_MOUNTAIN,
	},
	/* TERRAIN_TUNDRA */
	{
			FEAT_MOUNTAIN,
			FEAT_TREES,
			FEAT_DEEP_GRASS,

			FEAT_GRASS,
			FEAT_GRASS,
			FEAT_DIRT,

			FEAT_DIRT,
			FEAT_TUNDRA,
			FEAT_TUNDRA,

			FEAT_TUNDRA,
			FEAT_TUNDRA,
			FEAT_TUNDRA,

			FEAT_TUNDRA,
			FEAT_TUNDRA,
			FEAT_TUNDRA,

			FEAT_TUNDRA,
			FEAT_TUNDRA,
			FEAT_SHAL_WATER,
	},
	/* TERRAIN_DEEP_SEA */
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

			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,

			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
			FEAT_DEEP_WATER,
	},

};


static void generate_wilderness_area(int terrain, u32b seed, bool corner)
{
	int x1, y1;
	int table_size = sizeof(terrain_table[0]) / sizeof(int);
	int roughness = 1; /* The roughness of the level. */

	/* The outer wall is easy */
	if ((terrain == TERRAIN_EDGE) && !astral_mode)
	{
		/* Create level background */
		for (y1 = 0; y1 < MAX_HGT; y1++)
		{
			for (x1 = 0; x1 < MAX_WID; x1++)
			{
				cave[y1][x1].feat = FEAT_DEEP_WATER;
			}
		}

		/* We are done already */
		return;
	}


	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant town layout */
	Rand_value = seed;

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
	cave[1][1].feat = (byte)randint0(table_size);
	cave[MAX_HGT-2][1].feat = (byte)randint0(table_size);
	cave[1][MAX_WID-2].feat = (byte)randint0(table_size);
	cave[MAX_HGT-2][MAX_WID-2].feat = (byte)randint0(table_size);

	if (!corner)
	{
		/* x1, y1, x2, y2, num_depths, roughness */
		plasma_recursive(1, 1, MAX_WID-2, MAX_HGT-2, table_size-1, roughness);
	}

	/* Use the complex RNG */
	Rand_quick = FALSE;

	for (y1 = 1; y1 < MAX_HGT-1; y1++)
	{
		for (x1 = 1; x1 < MAX_WID-1; x1++)
		{
			cave[y1][x1].feat = terrain_table[terrain][cave[y1][x1].feat];
		}
	}
}



static bool genocide_of_barmamutha(void)
{
	int select = 0;
	char ch;
	int wid, hgt;

	if ((p_ptr->visit & (1L << (TOWN_BARMAMUTHA - 1))) ||
	    !r_info[MON_RONWE].max_num || !r_info[MON_LEONARD].max_num)
	{
		p_ptr->visit |= (1L << (TOWN_BARMAMUTHA - 1));
		return FALSE;
	}

	/* Clear the screen */
	Term_clear();

	Term_get_size(&wid, &hgt);

	prt("騎士レオナール (人間)", 2, 1);
	prt("クエスト情報 (危険度: 30 階相当)", 5, 0);
	prt("バルマムッサの虐殺", 7, 0);

	c_put_str(TERM_YELLOW, "バルマムッサの住民は既に戦う気力もなく、武装蜂起させるのはやはり", 8, 0);
	c_put_str(TERM_YELLOW, "無理のようだな。", 9, 0);
	c_put_str(TERM_YELLOW, "……。よく聞いてくれ。これから町の住人を一人残らず殺すんだ。", 10, 0);
	c_put_str(TERM_YELLOW, "こうなることを予想されていた公爵様のご命令なんだ。", 11, 0);
	c_put_str(TERM_YELLOW, "……従ってくれるな？こうしなければウォルスタに明日はないッ！", 12, 0);

	c_put_str(TERM_L_RED, "どちらか選択してください。", 15, 0);
	prt("a) わかっています。", 17, 0);
	prt("b) 馬鹿なことはやめるんだッ！", 19, 0);

	while (TRUE)
	{
		ch = inkey();
		if ((ch == 'A' || ch == 'a'))
		{
			select = 1;
			break;
		}
		if ((ch == 'B' || ch == 'b'))
		{
			select = 2;
			break;
		}
	}

	/* Clear the screen */
	Term_clear();

	if (select == 1)
	{
		c_put_str(TERM_L_BLUE, "「…わかっています。理想のために、この手を汚しましょう。」", 0, 0);

		c_put_str(TERM_YELLOW, "「…すまない。彼らの犠牲を無駄にはしない。」", 2, 0);
		c_put_str(TERM_YELLOW, "「では早速実行にかかろう。君は収容所を頼む。町は私がやる。", 4, 0);
		c_put_str(TERM_YELLOW, "……一人も逃がさないようにな」", 5, 0);

		change_your_alignment_lnc(300);
		change_chaos_frame(ETHNICITY_WALSTANIAN, -350);
		change_chaos_frame(ETHNICITY_GARGASTAN, -350);
		change_chaos_frame(ETHNICITY_BACRUM, -50);

		p_ptr->inside_quest = QUEST_BARMAMUTHA_L;
	}
	else if (select == 2)
	{
		c_put_str(TERM_L_BLUE, "「馬鹿なことはやめるんだッ！罪もない人々を殺して何が大儀だッ！」", 0, 0);

		c_put_str(TERM_YELLOW, "「やはり、きみは幼い…。その無垢な心がうらやましい…。", 2, 0);
		c_put_str(TERM_YELLOW, "自分の手を汚してでも理想を貫く…、", 3, 0);
		c_put_str(TERM_YELLOW, "それができなければ戦いに参加してはいけない…、", 4, 0);
		c_put_str(TERM_YELLOW, "いけないんだよッ！！」", 5, 0);

		change_your_alignment_lnc(-300);
		change_chaos_frame(ETHNICITY_WALSTANIAN, 20);
		change_chaos_frame(ETHNICITY_BACRUM, 20);
		change_chaos_frame(ETHNICITY_ZENOBIAN, 20);
		change_chaos_frame(ETHNICITY_LODIS, 20);

		p_ptr->inside_quest = QUEST_BARMAMUTHA_C;
	}

	init_flags = INIT_ASSIGN;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);
	quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;

	/* Flush input */
	flush();

	/* Wait for response */
	pause_line(hgt - 1);

	/* Clear the screen */
	Term_clear();

	p_ptr->visit |= (1L << (TOWN_BARMAMUTHA - 1));
	return TRUE;
}



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
		if (p_ptr->town_num == TOWN_BARMAMUTHA)
		{
			if (!corner && !border)
			{
				if (genocide_of_barmamutha()) return;
			}
		}

		/* Reset the buildings */
		init_buildings();

		/* Initialize the town */
		if (corner || border)
			init_flags = INIT_CREATE_DUNGEON | INIT_ONLY_FEATURES;
		else
			init_flags = INIT_CREATE_DUNGEON;

		process_dungeon_file("t_info.txt", 0, 0, MAX_HGT, MAX_WID);

		if (!corner && !border) p_ptr->visit |= (1L << (p_ptr->town_num - 1));
	}
	else
	{
		int terrain = wilderness[y][x].terrain;
		u32b seed = wilderness[y][x].seed;

		generate_wilderness_area(terrain, seed, corner);
	}

	if (!wilderness[y][x].town)
	{
		/*
		 * Place roads in the wilderness
		 * ToDo: make the road a bit more interresting
		 */
		if (wilderness[y][x].road)
		{
			int ty = MAX_HGT / 2;
			int tx = MAX_WID / 2;
			int i, j, ny, nx;
			int x1 = 0, y1 = 0;
			bool gen_enc_extra = (!corner && !border && generate_encounter);

			/* Create bridge */
			if ((wilderness[y][x].terrain == TERRAIN_DEEP_WATER) || (wilderness[y][x].terrain == TERRAIN_DEEP_SEA))
			{
				for (i = 0; i < 9; i++)
				{
					if (wilderness[y + ddy_ddd[i]][x + ddx_ddd[i]].road)
					{
						switch (ddy_ddd[i])
						{
						case -1: y1 = 1; break;
						case  0: y1 = ty; break;
						case  1: y1 = MAX_HGT - 2; break;
						}
						switch (ddx_ddd[i])
						{
						case -1: x1 = 1; break;
						case  0: x1 = tx; break;
						case  1: x1 = MAX_WID - 2; break;
						}

						for (ny = y1, nx = x1; (ny != ty) || (nx != tx); mmove2(&ny, &nx, y1, x1, ty, tx))
						{
							for (j = 0; j < 9; j++)
							{
								if (in_bounds(ny + ddy_ddd[j], nx + ddx_ddd[j]))
									cave[ny + ddy_ddd[j]][nx + ddx_ddd[j]].feat = FEAT_WALL_SOLID;
							}
						}
					}
				}

				for (j = 0; j < 9; j++)
				{
					cave[ty + ddy_ddd[j]][tx + ddx_ddd[j]].feat = FEAT_TREES;
				}
			}

			/* Create road */
			cave[ty][tx].feat = FEAT_FLOOR;
			if (gen_enc_extra) cave[ty][tx].info |= (CAVE_EXTRA);

			for (i = 0; i < 9; i++)
			{
				if (wilderness[y + ddy_ddd[i]][x + ddx_ddd[i]].road)
				{
					switch (ddy_ddd[i])
					{
					case -1: y1 = 1; break;
					case  0: y1 = ty; break;
					case  1: y1 = MAX_HGT - 2; break;
					}
					switch (ddx_ddd[i])
					{
					case -1: x1 = 1; break;
					case  0: x1 = tx; break;
					case  1: x1 = MAX_WID - 2; break;
					}

					for (ny = y1, nx = x1; (ny != ty) || (nx != tx); mmove2(&ny, &nx, y1, x1, ty, tx))
					{
						cave[ny][nx].feat = FEAT_FLOOR;
						if (gen_enc_extra) cave[ny][nx].info |= (CAVE_EXTRA);
					}
				}
			}

			/* Reallocate player on ambush */
			if (gen_enc_extra)
			{
				ny = p_ptr->oldpy;
				nx = p_ptr->oldpx;
				while (!(cave[ny][nx].info & CAVE_EXTRA))
				{
					ny = randint1(MAX_HGT - 2);
					nx = randint1(MAX_WID - 2);
				}
				if (cave[ny][nx].m_idx) delete_monster_idx(cave[ny][nx].m_idx);
				p_ptr->oldpy = ny;
				p_ptr->oldpx = nx;
				for (ny = 0; ny < cur_hgt; ny++)
				{
					for (nx = 0; nx < cur_wid; nx++)
					{
						cave[ny][nx].info &= ~(CAVE_EXTRA);
					}
				}
			}
		}
	}

	if (wilderness[y][x].entrance && !wilderness[y][x].town &&
		!(d_info[wilderness[y][x].entrance].flags1 & DF1_CLOSED) &&
		(p_ptr->total_winner || !(d_info[wilderness[y][x].entrance].flags1 & DF1_WINNER)))
	{
		int dy, dx;

		/* Hack -- Use the "simple" RNG */
		Rand_quick = TRUE;

		/* Hack -- Induce consistant town layout */
                Rand_value = wilderness[y][x].seed;

		dy = rand_range(6, cur_hgt - 6);
		dx = rand_range(6, cur_wid - 6);

		cave[dy][dx].feat = (d_info[wilderness[y][x].entrance].flags1 & DF1_UPWARD) ? FEAT_ENTRANCE_UPWARD : FEAT_ENTRANCE;
		cave[dy][dx].special = (byte)wilderness[y][x].entrance;

		/* Use the complex RNG */
		Rand_quick = FALSE;
	}
}


/*
 * Border of the wilderness area
 */
static border_type border;


/*
 * Build the wilderness area outside of the town.
 */
void wilderness_gen(void)
{
	int i, y, x, lim;
	cave_type *c_ptr;

	/* Big town */
	cur_hgt = MAX_HGT;
	cur_wid = MAX_WID;

	/* Assume illegal panel */
	panel_row_min = cur_hgt;
	panel_col_min = cur_wid;

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
	if (p_ptr->inside_quest) return;


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

	/* Light up or darken the area */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave grid */
			c_ptr = &cave[y][x];

			if (is_daytime())
			{
				/* Assume lit */
				c_ptr->info |= (CAVE_GLOW);

				/* Hack -- Memorize lit grids if allowed */
				if (view_perma_grids && !p_ptr->blind) c_ptr->info |= (CAVE_MARK);
			}
			else
			{
				/* Assume dark */
				c_ptr->info &= ~(CAVE_GLOW);

				/* Darken "boring" features */
				if ((c_ptr->feat <= FEAT_INVIS) ||
				    ((c_ptr->feat >= FEAT_DEEP_WATER) &&
				     (c_ptr->feat <= FEAT_MOUNTAIN) &&
				     (c_ptr->feat != FEAT_MUSEUM)) ||
				    (x == 0) || (x == cur_wid-1) ||
				    (y == 0) || (y == cur_hgt-1))
				{
					/* Forget the grid */
					c_ptr->info &= ~(CAVE_MARK);
				}
				else if ((c_ptr->feat == FEAT_ENTRANCE) || (c_ptr->feat == FEAT_ENTRANCE_UPWARD))
				{
					/* Hack -- Memorize lit grids if allowed */
					if (view_perma_grids) c_ptr->info |= (CAVE_MARK);
				}
			}
		}
	}

	if (p_ptr->teleport_town)
	{
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				/* Get the cave grid */
				c_ptr = &cave[y][x];

				if (((c_ptr->feat - FEAT_BLDG_HEAD) == 4) || (((p_ptr->town_num == TOWN_ARMORICA) || (p_ptr->town_num == TOWN_LOST_ISLAND)) && ((c_ptr->feat - FEAT_BLDG_HEAD) == 0)))
				{
					if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx);
					p_ptr->oldpy = y;
					p_ptr->oldpx = x;
				}
			}
		}
		p_ptr->teleport_town = FALSE;
	}

	else if (p_ptr->leaving_dungeon)
	{
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				/* Get the cave grid */
				c_ptr = &cave[y][x];

				if ((c_ptr->feat == FEAT_ENTRANCE) || (c_ptr->feat == FEAT_ENTRANCE_UPWARD))
				{
					if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx);
					p_ptr->oldpy = y;
					p_ptr->oldpx = x;
				}
			}
		}
		p_ptr->teleport_town = FALSE;
	}

	player_place(p_ptr->oldpy, p_ptr->oldpx);
	/* p_ptr->leaving_dungeon = FALSE;*/

	lim = generate_encounter ? 40 : MIN_M_ALLOC_TN;

	/* Make some residents */
	for (i = 0; i < lim; i++)
	{
		u32b mode = PM_IN_GENERATE;

		if (!(generate_encounter || (one_in_(2) && (!p_ptr->town_num))))
			mode |= PM_ALLOW_SLEEP;

		/* Make a resident */
		(void)alloc_monster(generate_encounter ? 0 : 3, mode);
	}

	/* Remove some residents on the bridge across the ocean */
	x = p_ptr->wilderness_x;
	y = p_ptr->wilderness_y;
	if (wilderness[y][x].road && (wilderness[y][x].terrain == TERRAIN_DEEP_WATER))
	{
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				/* Get the cave grid */
				c_ptr = &cave[y][x];

				if ((c_ptr->feat == FEAT_FLOOR) || (c_ptr->feat == FEAT_TREES))
				{
					if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx);
				}
			}
		}
	}

	if (generate_encounter) ambush_flag = TRUE;
	generate_encounter = FALSE;

	/* Fill the arrays of floors and walls in the good proportions */
	set_floor_and_wall(0);

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
	int i, j;

	/* To prevent stupid things */
	for (i = 0; i < MAX_WID; i++)
		for (j = 0; j < MAX_HGT; j++)
		{
			cave[j][i].feat = FEAT_PERM_SOLID;
		}

	/* Init the wilderness */
	process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);

	/* Fill the map */
	for (i = 0; i < max_wild_x; i++)
		for (j = 0; j < max_wild_y; j++)
		{
			if (wilderness[j][i].town && (wilderness[j][i].town != TOWN_LOST_ISLAND))
			{
				cave[j][i].feat = FEAT_TOWN;
				cave[j][i].special = wilderness[j][i].town;
			}
			else if (wilderness[j][i].road) cave[j][i].feat = FEAT_FLOOR;
			else if (wilderness[j][i].entrance && !(d_info[wilderness[j][i].entrance].flags1 & DF1_CLOSED) &&
				(p_ptr->total_winner || !(d_info[wilderness[j][i].entrance].flags1 & DF1_WINNER)))
			{
				cave[j][i].feat = (d_info[wilderness[j][i].entrance].flags1 & DF1_UPWARD) ? FEAT_ENTRANCE_UPWARD : FEAT_ENTRANCE;
				cave[j][i].special = (byte)wilderness[j][i].entrance;
			}
			else
			{
				cave[j][i].feat = conv_terrain2feat[wilderness[j][i].terrain];
				if (wilderness[j][i].terrain == TERRAIN_EDGE) cave[j][i].mimic = FEAT_DEEP_SEA;
			}

			cave[j][i].info |= (CAVE_GLOW | CAVE_MARK);
		}

	cur_hgt = (s16b)max_wild_y;
	cur_wid = (s16b)max_wild_x;

	if (cur_hgt > MAX_HGT) cur_hgt = MAX_HGT;
	if (cur_wid > MAX_WID) cur_wid = MAX_WID;

	/* Assume illegal panel */
	panel_row_min = cur_hgt;
	panel_col_min = cur_wid;

	/* Place the player */
	px = p_ptr->wilderness_x;
	py = p_ptr->wilderness_y;

	p_ptr->town_num = 0;
}


typedef struct wilderness_grid wilderness_grid;

struct wilderness_grid
{
	int		terrain;    /* Terrain type */
	int		town;       /* Town number */
	s16b	level;		/* Level of the wilderness */
	byte	road;       /* Road */
	char	name[32];	/* Name of the town/wilderness */
	char	ethnic;     /* Ethnicity of town */
};


static wilderness_grid w_letter[255];


/*
 * Parse a sub-file of the "extra info"
 */
errr parse_line_wilderness(char *buf, int xmin, int xmax, int *y, int *x)
{
	int i, num;
	char *zz[33];


	/* Paranoia */
	if (!(buf[0] == 'W')) return (PARSE_ERROR_GENERIC);

	switch (buf[2])
	{
		/* Process "W:F:<letter>:<terrain>:<town>:<road>:<ethnic>:<name> */
#ifdef JP
	case 'E':
		return 0;
	case 'F':
	case 'J':
#else
	case 'J':
		return 0;
	case 'F':
	case 'E':
#endif
	{
		if ((num = tokenize(buf+4, 7, zz, 0)) > 1)
		{
			int index = zz[0][0];

			if (num > 1)
				w_letter[index].terrain = atoi(zz[1]);
			else
				w_letter[index].terrain = 0;

			if (num > 2)
				w_letter[index].level = atoi(zz[2]);
			else
				w_letter[index].level = 0;

			if (num > 3)
				w_letter[index].town = atoi(zz[3]);
			else
				w_letter[index].town = 0;

			if (num > 4)
				w_letter[index].road = atoi(zz[4]);
			else
				w_letter[index].road = 0;

			if (num > 5)
				w_letter[index].ethnic = atoi(zz[5]);
			else
				w_letter[index].ethnic = -1;

			if (num > 6)
				strcpy(w_letter[index].name, zz[6]);
			else
				w_letter[index].name[0] = 0;
		}
		else
		{
				/* Failure */
			return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
		}
		
		break;
	}

	/* Process "W:D:<layout> */
	/* Layout of the wilderness */
	case 'D':
	{
		/* Acquire the text */
		char *s = buf+4;

		/* Length of the text */
		int len = strlen(s);

		for (*x = xmin, i = 0; ((*x < xmax) && (i < len)); (*x)++, s++, i++)
		{
			int idx = s[0];

			wilderness[*y][*x].terrain = w_letter[idx].terrain;

			wilderness[*y][*x].level = w_letter[idx].level;

			wilderness[*y][*x].town = w_letter[idx].town;

			wilderness[*y][*x].road = w_letter[idx].road;

			town[w_letter[idx].town].ethnic = w_letter[idx].ethnic;

			strcpy(town[w_letter[idx].town].name, w_letter[idx].name);
		}

		(*y)++;

		break;
	}

	/* Process "W:P:<x>:<y> - starting position in the wilderness */
	case 'P':
	{
		if ((p_ptr->wilderness_x == 0) &&
		    (p_ptr->wilderness_y == 0))
		{
			if (tokenize(buf+4, 2, zz, 0) == 2)
			{
				p_ptr->wilderness_y = atoi(zz[0]);
				p_ptr->wilderness_x = atoi(zz[1]);

				if ((p_ptr->wilderness_x < 1) ||
				    (p_ptr->wilderness_x > max_wild_x) ||
				    (p_ptr->wilderness_y < 1) ||
				    (p_ptr->wilderness_y > max_wild_y))
				{
					return (PARSE_ERROR_OUT_OF_BOUNDS);
				}
			}
			else
			{
				return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
			}
		}

		break;
	}

	default:
		/* Failure */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	for (i = 1; i < max_d_idx; i++)
	{
		if (!d_info[i].maxdepth) continue;
		wilderness[d_info[i].dy][d_info[i].dx].entrance = i;
		if (!wilderness[d_info[i].dy][d_info[i].dx].town)
			wilderness[d_info[i].dy][d_info[i].dx].level = d_info[i].mindepth;
	}

	/* Success */
	return (0);
}


/*
 * Generate the random seeds for the wilderness
 */
void seed_wilderness(void)
{
	int x, y;

	/* Init wilderness seeds */
	for (x = 0; x < max_wild_x; x++)
	{
		for (y = 0; y < max_wild_y; y++)
		{
			wilderness[y][x].seed = randint0(0x10000000);
			wilderness[y][x].entrance = 0;
		}
	}
}


/*
 * Pointer to wilderness_type
 */
typedef wilderness_type *wilderness_type_ptr;

/*
 * Initialize wilderness array
 */
errr init_wilderness(void)
{
	int i;

	/* Allocate the wilderness (two-dimension array) */
	C_MAKE(wilderness, max_wild_y, wilderness_type_ptr);
	C_MAKE(wilderness[0], max_wild_x * max_wild_y, wilderness_type);

	/* Init the other pointers */
	for (i = 1; i < max_wild_y; i++)
		wilderness[i] = wilderness[0] + i * max_wild_x;

	generate_encounter = FALSE;

	return 0;
}


bool change_wild_mode(void)
{
	int i;
	bool have_pet = FALSE;

	if (astral_mode)
	{
#ifdef JP
		msg_print("荒野なんてない。");
#else
		msg_print("No global map.");
#endif
		return FALSE;
	}
	if (!p_ptr->wild_mode)
	{
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];

			if (!m_ptr->r_idx) continue;
			if (is_pet(m_ptr) && i != p_ptr->riding) have_pet = TRUE;
			if (m_ptr->csleep) continue;
			if (m_ptr->cdis > MAX_SIGHT) continue;
			if (!is_hostile(m_ptr)) continue;
#ifdef JP
			msg_print("敵がすぐ近くにいるときは広域マップに入れない！");
#else
			msg_print("You cannot enter global map, since there is some monsters nearby!");
#endif
			energy_use = 0;
			return FALSE;
		}

		if (have_pet)
		{
#ifdef JP
			if(!get_check_strict("ペットを置いて広域マップに入りますか？", CHECK_OKAY_CANCEL))
#else
			if(!get_check_strict("Do you leave your pets behind? ", CHECK_OKAY_CANCEL))
#endif
			{
				energy_use = 0;
				return FALSE;
			}
		}
			
		energy_use = 1000;
	}

	stop_singing();
	set_action(ACTION_NONE);

	p_ptr->wild_mode = !p_ptr->wild_mode;

	/* Leaving */
	p_ptr->leaving = TRUE;

	return TRUE;
}
