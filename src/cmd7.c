/* File: cmd3.c */

/* Purpose: Inventory commands */

/*
 * Copyright (c) 2010 Mango Juice
 *
 * This software is licensed under the terms of the Gnu Product License (GPL),
 * most recent version as of December 18, 2010.
 * 
 * This file contains the "do_cmd_list" function and its helper functions.
 * 
 */

#include "angband.h"

#define LIST_MONSTER 0
#define LIST_OBJECT 1
#define LIST_OBSTACLE 2
#define LIST_UNEXPLORED 3
#define LIST_INTERESTING 4
#define LIST_ALL 5
#define LIST_TERRAIN 6
#define LIST_VISIBLE 7
	
cptr list_description[8] =
{
		"List of monsters",
		"List of objects",
		"List of obstacles",
		"List of unexplored tiles",
		"List of interesting grids",
		"List of all known grids",
		"List known terrain",
		"List of visible tiles"
};

static void dump_wild_tile_summary(FILE *fff, int x, int y, int setting)
{
	char pos[30];
	
	
	/* Format pos string for vector to x,y */
	sprintf(pos, "%d%c %d%c:",
			ABS((p_ptr->wilderness_y / 16) - y), 
			((p_ptr->wilderness_y / 16) > y ? 'N' : 'S'),
			ABS((p_ptr->wilderness_x / 16) - x), 
			((p_ptr->wilderness_x / 16) > x ? 'W' : 'E')	);

	if (setting == LIST_UNEXPLORED)
	{
		froff (fff, "%s: Unexplored\n", pos);
	}
	else if (setting == LIST_TERRAIN)
	{
		byte feat = wild_gen_data[wild[y][x].done.wild].feat;
		feature_type *ft_ptr = &f_info[feat];
		if (wild[y][x].done.place)
		{
			place_type *pl_ptr = &place[wild[y][x].done.place];
			/* Mention the place instead of the terrain */
			switch (pl_ptr->type)
			{
				case PL_TOWN_OLD:
				case PL_TOWN_FRACT:
				case PL_TOWN_MINI:
					froff(fff, "%s %s%s\n", pos, pl_ptr->name, pl_ptr->dungeon ? " -- Stairs" : "");
					return;
				case PL_QUEST_PIT:
					froff(fff, "%s Quest: %s\n", pos, quest[pl_ptr->quest_num].name);
					return;
				case PL_DUNGEON:
				{
					dun_type *d_ptr = pl_ptr->dungeon;
					froff(fff, "%s %s \n", pos, dungeon_type_name(d_ptr->habitat));
					return;
				}
				case PL_FARM:
					froff(fff, "%s A farm\n", pos);
					return;
				case PL_QUEST_STAIR:
				{
					quest_type * q_ptr = &quest[pl_ptr->quest_num];
					/* Say nothing if the quest hasn't been taken yet. */
					if (!(q_ptr->status < QUEST_STATUS_TAKEN) && q_ptr->flags & QUEST_FLAG_KNOWN && 
						q_ptr->status != QUEST_STATUS_FINISHED && q_ptr->status != QUEST_STATUS_FINISHED_FAILED)
					{
						froff(fff, "%s A quest\n", pos);
						return;
					}
				}
			}
		}
		froff(fff, "%s %s\n", pos, (f_name + ft_ptr->name));
	}
}

static void dump_tile_summary(FILE *fff, int x, int y, int setting)
{
	bool show_monster = FALSE;
	bool show_object = FALSE;
	bool show_field = FALSE;
	bool show_terrain = FALSE;
	bool show_interesting_terrain = FALSE;
	bool showed_something = FALSE;
	char pos[30];
	
	/* Format pos string for vector to x,y */
	sprintf(pos, "%d%c %d%c",
			ABS(p_ptr->py - y), 
			(p_ptr->py > y ? 'N' : 'S'),
			ABS(p_ptr->px - x), 
			(p_ptr->px > x ? 'W' : 'E')	);

	
	switch (setting)
	{
		case LIST_MONSTER:
			show_monster = TRUE;
			show_field = TRUE;
			show_interesting_terrain = TRUE;
			break;
		case LIST_OBJECT:
			show_object = TRUE;
			break;
		case LIST_OBSTACLE:
			show_terrain = TRUE;
			break;
		case LIST_UNEXPLORED:
			/* Assume there's nothing else to show, and that this tile is unexplored */
			froff (fff, "%s: Unexplored\n", pos);
			return;
		case LIST_INTERESTING:
			show_monster = TRUE;
			show_object = TRUE;
			show_field = TRUE;
			show_interesting_terrain = TRUE;
			break;
		case LIST_ALL:
			show_monster = show_object = show_field = show_terrain = TRUE;
			break;
		case LIST_TERRAIN:
			show_terrain = TRUE;
			break;
		case LIST_VISIBLE:
			show_object = show_monster = show_field = show_terrain = TRUE;
			break;
	}
	cave_type *c_ptr = area(x,y);
	pcave_type *pc_ptr = parea(x,y);
	
	/* Monsters first */
	monster_type *m_ptr = &m_list[c_ptr->m_idx];

	if (show_monster && c_ptr->m_idx && m_ptr->r_idx && m_ptr->ml && 
			!(m_ptr->smart & SM_MIMIC))
	{
		
		froff(fff, "%s: %v (%s)\n", pos, MONSTER_FMT(m_ptr, 0x08), look_mon_desc(c_ptr->m_idx));
	}
	
	/* Then objects */
	if (show_object)
	{
		/* First, check for undetected mimics */
		if (c_ptr->m_idx) 
		{
			/* Print it out if the monster is alive, visible, and known to be a mimic */
			if (m_ptr->r_idx && m_ptr->ml && (m_ptr->smart & SM_MIMIC))
			{
				char m_name[80];
				if (mimic_desc(m_name, &r_info[m_ptr->r_idx]))
				{
					froff(fff, "%s: %s\n", pos, m_name);
				}
			}
		}
		
		object_type *o_ptr;
		OBJ_ITT_START (c_ptr->o_idx, o_ptr)
		{
			if ((o_ptr->info & OB_SEEN) && (!SQUELCH(o_ptr->k_idx) || FLAG(o_ptr, TR_SQUELCH)))
				froff(fff, "%s: %v\n", pos, OBJECT_FMT(o_ptr, TRUE, 3));
		}
		OBJ_ITT_END;

	}
	
	if (show_field)
	{
		field_type * f_ptr;
		/* Scan all fields in the grid */
		FLD_ITT_START (c_ptr->fld_idx, f_ptr)
		{
			field_thaum *t_ptr = &t_info[f_ptr->t_idx];

			char fld_name[41];

			/* Do not describe this field */
			if (f_ptr->info & FIELD_INFO_NO_LOOK) continue;

			/* Describe if if is visible and known. */
			if (f_ptr->info & FIELD_INFO_MARK)
			{
				char *name = NULL;

				/* See if it has a special name */
				field_script_single(f_ptr, FIELD_ACT_LOOK, ":s", LUA_RETURN(name));

				if (name)
				{
					/* Copy the string into the temp buffer */
					strncpy(fld_name, name, 40);

					/* Anything there? */
					if (!fld_name[0])
					{
						/* Default to field name */
						strncpy(fld_name, t_ptr->name, 40);
					}

					/* Free string allocated to hold return value */
					string_free(name);
				}
				else
				{
					/* Default to field name */
					strncpy(fld_name, t_ptr->name, 40);
				}

				/* Describe the field */
				froff(fff, "%s: %s", pos, fld_name);
			}
		}
		FLD_ITT_END;
	}
	
	if (show_terrain || show_interesting_terrain)
	{
		feature_type *ft_ptr = &f_info[c_ptr->feat];
		/* Definitely show if show_terrain */
		bool show = show_terrain;

		if (!show && show_interesting_terrain)
		{
			if (cave_pattern_grid(c_ptr)) show = TRUE;
			else 
			{
				switch (c_ptr->feat)
				{
					/* Include "flooded" spots */
					case FEAT_DEEP_WATER:
					case FEAT_SHAL_WATER:
					case FEAT_DEEP_ACID:
					case FEAT_DEEP_LAVA:
					case FEAT_SHAL_ACID:
					case FEAT_SHAL_LAVA:
					case FEAT_DEEP_SWAMP:
					case FEAT_SHAL_SWAMP:
					case FEAT_OCEAN_WATER:
					/* Include doors */
					case FEAT_BROKEN:
					case FEAT_OPEN:
					case FEAT_CLOSED:
					/* Include stairs */
					case FEAT_LESS:
					case FEAT_MORE:
					/* Include diggable stuff */
					case FEAT_RUBBLE:
					case FEAT_QUARTZ_K:
					case FEAT_MAGMA_K:
						show = TRUE;
				}
			}
		}
		if (show) froff(fff, "%s: %s\n", pos, (f_name + ft_ptr->name));
	}
}

/*
 * Return TRUE if x0,y0 is closer than (or equal) to x1,y1
 */
static bool ang_sort_comp_true_distance_aux(int x0, int y0, int x1, int y1, int x2, int y2)
{
	int da, db;

	/* Distance to the first point */
	da = distance(x0,y0,x1,y1);
	
	/* Distance to the second point */
	db = distance(x0,y0,x2,y2);

	/* Compare the distances */
	return (da <= db);
}

static bool ang_sort_comp_true_distance(vptr u, vptr v, int a, int b) 
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	return (ang_sort_comp_true_distance_aux(px,py,((s16b*)u)[a],((s16b*)v)[a],
			((s16b*)u)[b],((s16b*)v)[b]));
}

static bool ang_sort_comp_wild_distance(vptr u, vptr v, int a, int b) 
{
	int py = p_ptr->wilderness_y / 16;
	int px = p_ptr->wilderness_x / 16;

	return (ang_sort_comp_true_distance_aux(px,py,((s16b*)u)[a],((s16b*)v)[a],
			((s16b*)u)[b],((s16b*)v)[b]));
}

static bool ang_sort_comp_wild_place_distance(vptr u, vptr v, int a, int b) 
{
	int py = p_ptr->wilderness_y / 16;
	int px = p_ptr->wilderness_x / 16;

	place_type *pl_ptr = &place[((s16b*)u)[a]];
	place_type *pl_ptr2 = &place[((s16b*)u)[b]];

	return (ang_sort_comp_true_distance_aux(px,py,pl_ptr->x,pl_ptr->y,pl_ptr2->x,pl_ptr2->y));
}

static void ang_sort_swap_wild(vptr u, vptr v, int a, int b)
{
	s16b *x = (s16b *)(u);

	s16b temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;
}

static bool wild_grid_matches_list_setting(int x, int y, int setting)
{
	wild_done_type *w_ptr = &wild[y][x].done;

	if (setting == LIST_UNEXPLORED)
	{
		/* Don't accept known tiles */
		if (w_ptr->info & WILD_INFO_SEEN) return FALSE;
		int i;
		for (i = 0; i < 10; i++)
		{
			int px = x + ddx[i];
			int py = y + ddy[i];
			/* Make sure we don't crash! */
			if (px < 0 || px >= WILD_SIZE || py < 0 || py >= WILD_SIZE) continue;
			w_ptr = &wild[py][px].done;

			/* This is a "frontier" unexplored cell if it's next to an explored one */
			if (w_ptr->info & WILD_INFO_SEEN) return TRUE;
		}
		/* Unexplored, but all adjacent cells are also unexplored */
		return FALSE;
	}
	else if (setting = LIST_TERRAIN)
	{
		/* Don't accept unknown tiles */
		if (!(w_ptr->info & WILD_INFO_SEEN)) return FALSE;
		
		return TRUE;
	}

	/* Fall through, just in case.  But make a note of it */
	msgf("Did not get return value in wild_grid_matches_list_setting().");
	return TRUE;
}

static bool grid_matches_list_setting(int x, int y, int setting)
{
	cave_type *c_ptr = area(x,y);
	pcave_type *pc_ptr = parea(x,y);

	/* only List_unexplored and List_visible are interested in non-known grids */
	if (setting != LIST_UNEXPLORED && setting != LIST_VISIBLE && !(pc_ptr->player & GRID_KNOWN))
	{
		return FALSE;
	}
	
	switch(setting)
	{
		case LIST_MONSTER:
			if (c_ptr->m_idx) 
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];

				/* Return true if the monster is alive, visible, not an unknown mimic, and we're not
				   hallucinating */
				if (m_ptr->r_idx && m_ptr->ml && !query_timed(TIMED_IMAGE) && !(m_ptr->smart & SM_MIMIC))
					return TRUE;
			}
			/* If no monster, check for a trap. */
			if (is_visible_trap(c_ptr))
			{
				return(TRUE);
			}
			if (cave_pattern_grid(c_ptr))
			{
				/* Pattern is dangerous */
				return TRUE;
			}
			switch (c_ptr->feat)
			{
				case FEAT_DEEP_WATER:
				case FEAT_SHAL_WATER:
				case FEAT_DEEP_ACID:
				case FEAT_DEEP_LAVA:
				case FEAT_SHAL_ACID:
				case FEAT_SHAL_LAVA:
				case FEAT_DEEP_SWAMP:
				case FEAT_SHAL_SWAMP:
				case FEAT_OCEAN_WATER:
					/* These are all dangerous to some degree */
					return TRUE;
				default:
					return FALSE;
			}
		case LIST_OBJECT:
			if (c_ptr->o_idx) 
			{
				/* Return true so long as something here is visible and not squelched */
				object_type *o_ptr;
				OBJ_ITT_START (c_ptr->o_idx, o_ptr)
				{
					if ((o_ptr->info & OB_SEEN) && (!SQUELCH(o_ptr->k_idx) || FLAG(o_ptr, TR_SQUELCH)))
						return TRUE;
				}
				OBJ_ITT_END;
			}
			/* Get fooled by mimics too */
			if (c_ptr->m_idx) 
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];

				/* Return true if the monster is alive, visible, and known to be a mimic */
				if (m_ptr->r_idx && m_ptr->ml && !query_timed(TIMED_IMAGE) && (m_ptr->smart & SM_MIMIC))
					return TRUE;
			}
			return FALSE;
		case LIST_OBSTACLE:
		{
			return cave_wall_grid(c_ptr);
		}
		case LIST_UNEXPLORED:
		{
			if (pc_ptr->player & GRID_KNOWN) return FALSE;
			int i;
			for (i = 0; i < 10; i++)
			{
				int px = x + ddx[i];
				int py = y + ddy[i];
				/* Make sure we don't crash! */
				if (px < p_ptr->min_wid || px >= p_ptr->max_wid ||
					py < p_ptr->min_hgt || py >= p_ptr->max_hgt) continue;
				pc_ptr = parea(px, py);
				/* This is a "frontier" unexplored cell if it's next to an explored one,
				 * and moreover, the explored one is either a walkable tile or a (known) closed
				 * door.
				 */
				if (pc_ptr->player & GRID_KNOWN && (!cave_wall_grid(area(px,py)) 
						|| area(px,py)->feat == FEAT_CLOSED)) return TRUE;
			}
			/* Unexplored, but all adjacent cells are also unexplored */
			return FALSE;
		}
		case LIST_INTERESTING:
		{
			field_type *f_ptr;

			/* Include monsters, objects, hazards */
			if (grid_matches_list_setting(x,y,LIST_MONSTER) || grid_matches_list_setting(x,y,LIST_OBJECT)) return TRUE;
			switch (c_ptr->feat)
			{
				/* Include doors */
				case FEAT_BROKEN:
				case FEAT_OPEN:
				case FEAT_CLOSED:
				/* Include stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				/* Include diggable stuff */
				case FEAT_RUBBLE:
				case FEAT_QUARTZ_K:
				case FEAT_MAGMA_K:
					return TRUE;
			}
			/* Scan all fields in the grid */
			FLD_ITT_START (c_ptr->fld_idx, f_ptr)
			{
				/* Memorized , lookable field */
				if ((f_ptr->info & (FIELD_INFO_MARK | FIELD_INFO_NO_LOOK)) ==
					FIELD_INFO_MARK) return (TRUE);
			}
			FLD_ITT_END;
			
			/* Otherwise, it's not interesting */
			return FALSE;
		}
		case LIST_ALL:
		case LIST_TERRAIN:
		{
			return TRUE;
		}
		case LIST_VISIBLE:
		{
			return ((pc_ptr->player & GRID_VIEW) && (pc_ptr->player & GRID_KNOWN));
		}
		default: 
			setting = 0;
	}
	/* Fall through, just in case.  But make a note of it */
	msgf("Did not get return value in grid_matches_list().");
	return TRUE;
}

static bool point_matches_dir_aux (int x, int y, int px, int py, int dir)
{
	/* dir = 5 means don't filter */
	if (dir == 5) return TRUE;
	
	int dx = x - px; 
	int dy = y - py;
	
	/* Make sure the vector has the right sign of its x and y components */
	if (SGN(dx) && SGN(ddx[dir]) && (SGN(dx) != SGN(ddx[dir])))
		return FALSE;
	if (SGN(dy) && SGN(ddy[dir]) && (SGN(dy) != SGN(ddy[dir])))
		return FALSE;

	/* If the direction has no x (y) component, make sure the y (x) 
	 * component of the vector is no smaller than the y component.
	 */
	if (!SGN(ddx[dir]) && ABS(dy) < ABS(dx)) return FALSE;
	if (!SGN(ddy[dir]) && ABS(dx) < ABS(dy)) return FALSE;
	
	return TRUE;
}

static bool point_matches_dir (x, y, dir)
{
	return (point_matches_dir_aux(x, y, p_ptr->px, p_ptr->py, dir));
}

static void do_cmd_list_aux (int setting)
{
	int k, x, y, dir;
	char com;

	/* Can't do this while hallucinating. */
	if (query_timed(TIMED_IMAGE))
	{
		msgf ("You can't do that while you're hallucinating.");
		return;
	}
	
	FILE *fff;

	if (!get_com("Direction? ", &com))
		dir = 5;
	else dir = get_keymap_dir(com);
	if (dir < 1 || dir > 9) dir = 5;
	
	char file_name[1024];
	int place_order[place_count];
	
	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return;

	temp_n = 0;

	/* Filter grids for what we are looking for */
	/* Ensure we look at every close grid */
	for (y = MAX(p_ptr->py - MAX_RANGE,p_ptr->min_hgt); 
			y < MIN(p_ptr->py + MAX_RANGE + 1,p_ptr->max_hgt); y++)
	{
		for (x = MAX(p_ptr->px - MAX_RANGE,p_ptr->min_wid); 
				x <= MIN(p_ptr->px + MAX_RANGE + 1,p_ptr->max_wid); x++)
		{
			if (!point_matches_dir(x,y,dir)) continue;
			if (grid_matches_list_setting(x,y,setting))
			{
				temp_x[temp_n] = x;
				temp_y[temp_n++] = y;
			}
		}
	}
	
	/* Look at all remaining grids until we run out of memory */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		/* Skip values we did before */
		if (y == MAX(p_ptr->py - MAX_RANGE,p_ptr->min_hgt))
		{
			y = MIN(p_ptr->py + MAX_RANGE + 1,p_ptr->max_hgt);
			continue;
		}
		for (x = p_ptr->max_wid; x < p_ptr->max_wid; x++)
		{
			/* Skip values we did before */
			if (x == MAX(p_ptr->px - MAX_RANGE,p_ptr->min_wid))
			{
				x = MIN(p_ptr->px + MAX_RANGE + 1,p_ptr->max_wid);
				continue;
			}
			if (!point_matches_dir(x,y,dir)) continue;
			if (grid_matches_list_setting(x,y,setting))
			{
				temp_x[temp_n] = x;
				temp_y[temp_n++] = y;
			}
			
			if (temp_n >= TEMP_MAX) break;
		}
		if (temp_n >= TEMP_MAX) break;
	}
	
	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_true_distance;
	ang_sort_swap = ang_sort_swap_distance;
	ang_sort(temp_x, temp_y, temp_n);

	/* Display the places, in order */
	for (k = 0; k < temp_n; k++)
	{
		dump_tile_summary(fff, temp_x[k], temp_y[k], setting);
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, list_description[setting], 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
}

static bool do_cmd_list_monster(int dummy) {
	/* Ignore parameter */
	dummy = 0;
	
	do_cmd_list_aux(LIST_MONSTER);
	return FALSE; 
}
static bool do_cmd_list_object(int dummy) {
	/* Ignore parameter */
	dummy = 0;
	
	do_cmd_list_aux(LIST_OBJECT);
	return FALSE; 
}

static bool do_cmd_list_obstacle(int dummy) 
{
	/* Ignore parameter */
	dummy = 0;
	
	do_cmd_list_aux(LIST_OBSTACLE);
	return FALSE; 
}

static bool do_cmd_list_unexplored(int dummy)
{
	/* Ignore parameter */
	dummy = 0;
	
	do_cmd_list_aux(LIST_UNEXPLORED);
	return FALSE; 
}

static bool do_cmd_list_visible(int dummy)
{
	/* Ignore parameter */
	dummy = 0;
	
	do_cmd_list_aux(LIST_VISIBLE);
	return FALSE; 
}


static bool do_cmd_list_interesting(int dummy) 
{
	/* Ignore parameter */
	dummy = 0;
	
	do_cmd_list_aux(LIST_INTERESTING);
	return FALSE; 
}

static bool do_cmd_list_all(int dummy)
{
	/* Ignore parameter */
	dummy = 0;
	
	do_cmd_list_aux(LIST_ALL);
	return FALSE; 
}

static bool do_cmd_list_terrain(int dummy)
{
	/* Ignore parameter */
	dummy = 0;
	
	do_cmd_list_aux(LIST_TERRAIN);
	return FALSE; 
}

static bool do_cmd_list_wilderness_tiles(int setting)
{
	int i, j, px, py, x, y;
	char com;
	int dir;

	FILE *fff;

	char file_name[1024];
	
	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);

	temp_n = 0;

	if (!get_com("Direction? ", &com))
		dir = 5;
	else dir = get_keymap_dir(com);
	if (dir < 1 || dir > 9) dir = 5;
	
	px = p_ptr->wilderness_x / 16;
	py = p_ptr->wilderness_y / 16;
	
	/* Close wild grids first */
	for (x = px-20; x < px+20; x++)
	{
		for (y = py-20; y < py+20; y++) 
		{
			if (!point_matches_dir_aux(x,y,px,py,dir)) continue;
			if (x < 0 || y < 0 || x >= WILD_SIZE-1 || y >= WILD_SIZE-1)
				continue;
			if (!wild_grid_matches_list_setting(x,y,setting))
				continue;
			temp_y[temp_n] = y;
			temp_x[temp_n++] = x;
		}
	}

	/* Now all wild grids until temp_n gets too big */
	for (x = 0; x < WILD_SIZE; x++)
	{
		for (y = 0; y < WILD_SIZE; y++)
		{
			if (!point_matches_dir_aux(x,y,px,py,dir)) continue;
			if (x >= px-20 && x < px+20) continue;
			if (y >= py-20 && y < py+20) continue; 
			if (!wild_grid_matches_list_setting(x,y,setting))
				continue;
			temp_y[temp_n] = y;
			temp_x[temp_n++] = x;
			if (temp_n >= TEMP_MAX) break;
		}
		if (temp_n >= TEMP_MAX) break;
	} 
	
	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_wild_distance;
	ang_sort_swap = ang_sort_swap_distance;
	/* Sort it */
	ang_sort(temp_x, temp_y, temp_n);

	/* Display the places, in order */
	for (i = 0; i < temp_n; i++)
	{
		dump_wild_tile_summary(fff, temp_x[i], temp_y[i], setting);
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	if (setting == LIST_UNEXPLORED)
		(void)show_file(file_name, "Unexplored wilderness tiles" , 0, 0);
	else
		(void)show_file(file_name, "Known wilderness tiles" , 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);

	return (FALSE);
}

static bool do_cmd_list_unexplored_wilderness(int dummy)
{
	return do_cmd_list_wilderness_tiles(LIST_UNEXPLORED);
}

static bool do_cmd_list_wilderness_terrain(int dummy)
{
	return do_cmd_list_wilderness_tiles(LIST_TERRAIN);
}

/*
 * Dump info about a town to the given file
 */
void dump_place_summary(FILE *fff, int pl)
{
	int j, x, y;
	int px, py;
	
	char buf[256];
	char pos[30];

	char c;
	byte a;

	place_type *pl_ptr = &place[pl];

	/* Only display if the player knows about this place */
	if (!pl_ptr->seen) return;

	/* Set x and y based on the location.  
	 * TODO: Do better.  This compares only the NW corner of the wilderness place
	 * to the player's wilderness location.  
	 */
	x = pl_ptr->x + (pl_ptr->xsize)/2;
	y = pl_ptr->y + (pl_ptr->ysize)/2;
	px = p_ptr->wilderness_x / 16;
	py = p_ptr->wilderness_y / 16;
	
	/* Build the location string */
	sprintf(pos, "%d%c %d%c:",
			(py > y ? py - y : y-py), 
			(py > y ? 'N' : 'S'),
			(px > x ? px - x : x-px), 
			(px > x ? 'W' : 'E')	);

	
	/* Build the string to display */
	switch (pl_ptr->type)
	{
		case PL_TOWN_OLD:
		case PL_TOWN_FRACT:
		case PL_TOWN_MINI:
			sprintf(buf, "%s %s%s (%dx%d)\n", pos, pl_ptr->name, pl_ptr->dungeon ? " -- Stairs" : "", 
					pl_ptr->xsize-1, pl_ptr->ysize-1);
			break;
		case PL_QUEST_PIT:
			sprintf(buf, "%s Quest: %s (%dx%d)\n", pos, quest[pl_ptr->quest_num].name, 
					pl_ptr->xsize, pl_ptr->ysize);
			break;
		case PL_DUNGEON:
		{
			dun_type *d_ptr = pl_ptr->dungeon;
			sprintf(buf, "%s %s (%dx%d)\n", pos, dungeon_type_name(d_ptr->habitat), 
					pl_ptr->xsize-1, pl_ptr->ysize-1);
			break;
		}
		case PL_FARM:
			/* skip farms */
			return;
		case PL_QUEST_STAIR:
		{
			quest_type * q_ptr = &quest[pl_ptr->quest_num];
			/* Say nothing if the quest hasn't been taken yet. */
			if (q_ptr->status < QUEST_STATUS_TAKEN || !(q_ptr->flags & QUEST_FLAG_KNOWN))
				return;

			/* Say nothing if the quest is totally finished. */
			if (q_ptr->status == QUEST_STATUS_FINISHED || q_ptr->status == QUEST_STATUS_FINISHED_FAILED)
				return;
			
			sprintf(buf, "%s A quest (1x1)\n", pos);
			break;
		}
	}
	
	/* Write it */
	froff(fff,"%s",buf);
}

/*
 * Display information about wilderness areas
 */
static bool do_cmd_list_places(int dummy)
{
	int k, x, y;

	FILE *fff;

	char file_name[1024];
	int place_order[place_count];
	
	/* Hack - ignore parameter */
	(void) dummy;

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);

	/* Sort the places in terms of distance */
	/* Cycle through the places */
	temp_n = 0;
	for (k = 1; k < place_count; k++)
	{
		/* Use the temp_x array, blank out the temp_y array since we aren't using it */
		temp_y[temp_n] = 0;
		temp_x[temp_n++] = k;
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_wild_place_distance;
	ang_sort_swap = ang_sort_swap_wild;
	ang_sort(temp_x, temp_y, temp_n);

	/* Display the places, in order */
	for (k = 0; k < temp_n; k++)
	{
		dump_place_summary(fff, temp_x[k]);
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Known wilderness places", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);

	return (FALSE);
}

/* Some gaps for options that should not show up always */
static menu_type list_menu[16] =
{
		{"List of monsters and other hazards", NULL, do_cmd_list_monster, MN_ACTIVE | MN_CLEAR},
		{"List of objects", NULL, do_cmd_list_object, MN_ACTIVE | MN_CLEAR},
		{"List of obstacles", NULL, do_cmd_list_obstacle, MN_ACTIVE | MN_CLEAR},
		{"List of unexplored cells", NULL, do_cmd_list_unexplored, MN_ACTIVE | MN_CLEAR},
		{"List of terrain", NULL, do_cmd_list_terrain, MN_ACTIVE | MN_CLEAR},
		{"List of interesting tiles", NULL, do_cmd_list_interesting, MN_ACTIVE | MN_CLEAR},
		{"List of everything", NULL, do_cmd_list_all, MN_ACTIVE | MN_CLEAR},
		{"List of things visible from current location", NULL, do_cmd_list_visible, MN_ACTIVE | MN_CLEAR},
	MENU_END,
	MENU_END,
	MENU_END,
	MENU_END,
		{"List of wilderness places", NULL, do_cmd_list_places, MN_ACTIVE | MN_CLEAR},
		{"List of unexplored wilderness areas", NULL, do_cmd_list_unexplored_wilderness, MN_ACTIVE | MN_CLEAR}, 
		{"List of wilderness terrain", NULL, do_cmd_list_wilderness_terrain, MN_ACTIVE | MN_CLEAR}, 
	MENU_END
};


/*
 * Interact with "lists"
 */
void do_cmd_list(void)
{
	int nr, last_option = 8;

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);
	
	/* start at the first free spot */
	nr = last_option;

	/* Copy in the wilderness displays */
	if (!vanilla_town)
	{
		list_menu[nr++] = list_menu[12];
		list_menu[nr++] = list_menu[13];
		list_menu[nr++] = list_menu[14];
	}

	/* Display the menu */
	display_menu(list_menu, -1, FALSE, NULL, "Display lists of map tiles:");

	/* Clear these options again */
	for (; nr >= last_option; nr--)
	{
		/* menu item 15 contains a MENU_END */
		list_menu[nr] = list_menu[15];
	}
}
