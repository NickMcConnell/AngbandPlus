/* File: monster2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



/*
 * Return another race for a monster to polymorph into.  -LM-
 *
 * Perform a modified version of "get_mon_num()", with exact minimum and
 * maximum depths and preferred monster types.
 */
s16b poly_r_idx(const monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	s16b base_idx = m_ptr->r_idx;

	alloc_entry *table = alloc_race_table;

	int i, min_lev, max_lev, r_idx;
	long total, value;

	/* Source monster's level and symbol */
	int r_lev = r_ptr->level;
	char d_char = r_ptr->d_char;

	/* Hack -- Uniques and quest monsters never polymorph */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (m_ptr->mflag & (MFLAG_QUEST)))
	{
		return (base_idx);
	}

	/* Allowable level of new monster */
	min_lev = (MAX(        1, r_lev - 1 - r_lev / 5));
	max_lev = (MIN(MAX_DEPTH, r_lev + 1 + r_lev / 5));

	/* Reset sum */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Assume no probability */
		table[i].prob3 = 0;

		/* Ignore illegal monsters - only those that don't get generated. */
		if (!table[i].prob1) continue;

		/* Not below the minimum base depth */
		if (table[i].level < min_lev) continue;

		/* Not above the maximum base depth */
		if (table[i].level > max_lev) continue;

		/* Get the monster index */
		r_idx = table[i].index;

		/* We're polymorphing -- we don't want the same monster */
		if (r_idx == base_idx) continue;

		/* Do not polymorph into a quest monster */
		/* if ((q_idx) && (r_idx == q_idx)) continue; */

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/* Hack -- No uniques */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Bias against monsters far from initial monster's depth */
		if (table[i].level < (min_lev + r_lev) / 2) table[i].prob3 /= 4;
		if (table[i].level > (max_lev + r_lev) / 2) table[i].prob3 /= 4;

		/* Bias against monsters not of the same symbol */
		if (r_ptr->d_char != d_char) table[i].prob3 /= 4;

		/* Sum up probabilities */
		total += table[i].prob3;
	}

	/* No legal monsters */
	if (total == 0)
	{
		return (base_idx);
	}


	/* Pick a monster */
	value = rand_int(total);

	/* Find the monster */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value = value - table[i].prob3;
	}

	/* Result */
	return (table[i].index);
}




/*
 * Delete a monster by index.
 *
 * When a monster is deleted, all of its objects are deleted.
 */
void delete_monster_idx(int i)
{
	int x, y, m;

	monster_type *m_ptr = &mon_list[i];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	s16b this_o_idx, next_o_idx = 0;

	/* Get location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Remove the monster from the moment array */
	for (m = 0; m < move_moment_num; m++)
	{
		move_moment_type *mm_ptr = &mon_moment_info[m];

		/* We found the monster? */
		if (mm_ptr->m_idx == i)
		{
			/* Delete moment info */
			(void)WIPE(mm_ptr, move_moment_type);

			/* Done */
			break;
		}
	}

	/* Hack -- Reduce the racial counter */
	r_ptr->cur_num--;

	/* Hack -- count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro--;

	/* Hack -- remove target monster */
	if (p_ptr->target_who == i) target_set_monster(0);

	/* Hack -- remove tracked monster */
	if (p_ptr->health_who == i) health_track(0);

	/* Monster is gone */
	cave_m_idx[y][x] = 0;

	/* If the monster was a player ghost, the unique player ghost info. */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) remove_player_ghost();

	/* Delete objects */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- efficiency */
		o_ptr->held_m_idx = 0;

		/* Delete the object */
		delete_object_idx(this_o_idx);
	}


	/* Wipe the Monster */
	(void)WIPE(m_ptr, monster_type);

	/* Count monsters */
	mon_cnt--;

	p_ptr->redraw |= PR_MONLIST;

	/* Visual update */
	light_spot(y, x);
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(int y, int x)
{
	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Delete the monster (if any) */
	if (cave_m_idx[y][x] > 0) delete_monster_idx(cave_m_idx[y][x]);
}


/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_monsters_aux(int i1, int i2)
{
	int y, x;

	monster_type *m_ptr;

	s16b this_o_idx, next_o_idx = 0;


	/* Do nothing */
	if (i1 == i2) return;


	/* Old monster */
	m_ptr = &mon_list[i1];

	/* Location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Update the cave */
	cave_m_idx[y][x] = i2;

	/* Repair objects being carried by monster */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Reset monster pointer */
		o_ptr->held_m_idx = i2;
	}

	/* Hack -- Update the target */
	if (p_ptr->target_who == i1) p_ptr->target_who = i2;

	/* Hack -- Update the health bar */
	if (p_ptr->health_who == i1) p_ptr->health_who = i2;

	/* Hack -- move monster */
	COPY(&mon_list[i2], &mon_list[i1], monster_type);

	/* Hack -- wipe hole */
	(void)WIPE(&mon_list[i1], monster_type);
}


/*
 * Compact and Reorder the monster list
 *
 * This function can be very dangerous, use with caution!
 *
 * When compacting monsters, we first delete far away monsters without
 * objects, starting with those of lowest level.  Then nearby monsters and
 * monsters with objects get compacted, then unique monsters, and only then
 * are quest monsters affected.  -LM-
 *
 * After "compacting" (if needed), we "reorder" the monsters into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */

void compact_monsters(int size)
{
	int i, j, cnt;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Paranoia -- refuse to wipe too many monsters at one time */
	if (size > z_info->m_max / 2) size = z_info->m_max / 2;

	/* Paranoia -- refuse to wipe too many monsters at one time */
	if (size > z_info->m_max / 2) size = z_info->m_max / 2;

	/* Compact */
	if (size)
	{
		/* Get quest monster index (if any) */
		/* int q_idx = q_info[quest_num(p_ptr->depth)].r_idx; */

		s16b *mon_lev;
		s16b *mon_index;

		/* Allocate the "mon_lev and mon_index" arrays */
		mon_lev = C_ZNEW(mon_max, s16b);
		mon_index = C_ZNEW(mon_max, s16b);

		/* Message */
		msg_print("Compacting monsters...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP | PR_MONSTER);

		/* Scan the monster list */
		for (i = 1; i < mon_max; i++)
		{
			m_ptr = &mon_list[i];
			r_ptr = &r_info[m_ptr->r_idx];

			/* Dead monsters have minimal level (but are counted!) */
			if (!m_ptr->r_idx) mon_lev[i] = -1L;

			/* Get the monster level */
			else
			{
				mon_lev[i] = r_ptr->level;

				/* Quest monsters always get compacted last */
				if ((r_ptr->flags1 & (RF1_QUESTOR)) || (m_ptr->mflag & (MFLAG_QUEST)))
					mon_lev[i] += MAX_DEPTH * 3;

				/*same with active quest monsters*/
				else if (q_info[quest_num(p_ptr->depth)].mon_idx == m_ptr->r_idx)
					mon_lev[i] += MAX_DEPTH * 3;
				/* Uniques are protected */
				else if (r_ptr->flags1 & (RF1_UNIQUE)) mon_lev[i] += MAX_DEPTH * 2;

				/* Nearby monsters are protected */
				else if ((character_dungeon) && (m_ptr->cdis < MAX_SIGHT))
					mon_lev[i] += MAX_DEPTH;

				/* Monsters with objects are protected */
				else if (m_ptr->hold_o_idx) mon_lev[i] += MAX_DEPTH;
			}

			/* Save this monster index */
			mon_index[i] = i;
		}

	/* Sort all the monsters by (adjusted) level */
		for (i = 0; i < mon_max - 1; i++)
		{
			for (j = 0; j < mon_max - 1; j++)
			{
				int j1 = j;
				int j2 = j + 1;

				/* Bubble sort - ascending values */
				if (mon_lev[j1] > mon_lev[j2])
				{
					s16b tmp_lev = mon_lev[j1];
					u16b tmp_index = mon_index[j1];

					mon_lev[j1] = mon_lev[j2];
					mon_index[j1] = mon_index[j2];

					mon_lev[j2] = tmp_lev;
					mon_index[j2] = tmp_index;
				}
			}
		}

		/* Delete monsters until we've reached our quota */
		for (cnt = 0, i = 0; i < mon_max; i++)
		{
			/* We've deleted enough monsters */
			if (cnt >= size) break;
			/* Get this monster, using our saved index */
			m_ptr = &mon_list[mon_index[i]];

			/* "And another one bites the dust" */
			cnt++;

			/* No need to delete dead monsters again */
			if (!m_ptr->r_idx) continue;

			/* Delete the monster */
			delete_monster_idx(mon_index[i]);
		}

		/* Free the "mon_lev and mon_index" arrays */
		FREE(mon_lev);
		FREE(mon_index);
	}

	/* Excise dead monsters (backwards!) */
	for (i = mon_max - 1; i >= 1; i--)
	{
		/* Get the i'th monster */
		monster_type *m_ptr = &mon_list[i];

		/* Skip real monsters */
		if (m_ptr->r_idx) continue;

		/* Move last monster into open hole */
		compact_monsters_aux(mon_max - 1, i);

		/* Compress "mon_max" */
		mon_max--;
	}
}


/*
 * Delete/Remove all the monsters when the player leaves the level
 *
 * This is an efficient method of simulating multiple calls to the
 * "delete_monster()" function, with no visual effects.
 */
void wipe_mon_list(void)
{
	int i;

	/* Delete all the monsters */
	for (i = mon_max - 1; i >= 1; i--)
	{
		monster_type *m_ptr = &mon_list[i];

		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* If the monster was a player ghost, the unique player ghost info. */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) remove_player_ghost();

		/* Hack -- Reduce the racial counter */
		r_ptr->cur_num--;

		/* Monster is gone */
		cave_m_idx[m_ptr->fy][m_ptr->fx] = 0;

		/* Wipe the Monster */
		(void)WIPE(m_ptr, monster_type);
	}

	/* Paranoia - Clear the move moment array */
	for (i = 0; i < move_moment_num; i++)
	{
		move_moment_type *mm_ptr = &mon_moment_info[i];

		/* Monster */
		if (mm_ptr->m_idx > 0)
		{
			(void)WIPE(mm_ptr, move_moment_type);
		}
	}

	/* Reset "mon_max" */
	mon_max = 1;

	/* Reset "mon_cnt" */
	mon_cnt = 0;

	/* Hack -- reset "reproducer" count */
	num_repro = 0;

	/* Hack -- no more target */
	target_set_monster(0);

	/* Hack -- no more tracking */
	health_track(0);

}


/*
 * Get and return the index of a "free" monster.
 *
 * This routine should almost never fail, but it *can* happen.
 */
s16b mon_pop(void)
{
	int i;

	/* Normal allocation */
	if (mon_max < z_info->m_max)
	{
		/* Get the next hole */
		i = mon_max;

		/* Expand the array */
		mon_max++;

		/* Count monsters */
		mon_cnt++;

		/* Return the index */
		return (i);
	}


	/* Recycle dead monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr;

		/* Get the monster */
		m_ptr = &mon_list[i];

		/* Skip live monsters */
		if (m_ptr->r_idx) continue;

		/* Count monsters */
		mon_cnt++;

		/* Use this monster */
		return (i);
	}


	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) msg_print("Too many monsters!");

	/* Try not to crash */
	return (0);
}


/*
 * Apply a "monster restriction function" to the "monster allocation table"
 */
errr get_mon_num_prep(void)
{
	int i;

	/* Scan the allocation table */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Get the entry */
		alloc_entry *entry = &alloc_race_table[i];

		/* Accept monsters which pass the restriction, if any */
		if (!get_mon_num_hook || (*get_mon_num_hook)(entry->index))
		{
			/* Accept this monster */
			entry->prob2 = entry->prob1;
		}

		/* Do not use this monster */
		else
		{
			/* Decline this monster */
			entry->prob2 = 0;
		}
	}

	/* Success */
	return (0);
}



/*
 * Choose a monster race that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "monster allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" monster, in
 * a relatively efficient manner.
 *
 * Note that "town" monsters will *only* be created in the town, and
 * "normal" monsters will *never* be created in the town, unless the
 * "level" is "modified", for example, by polymorph or summoning.
 *
 * There is a small chance (1/50) of "boosting" the given depth by
 * a small amount (up to four levels), except in the town, and
 * a minimum depth enforcer for creature (unless specific monsters
 * are being called)
 *
 * It is (slightly) more likely to acquire a monster of the given level
 * than one of a lower level.  This is done by choosing several monsters
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no monsters are "appropriate", then this function will
 * fail, and re+turn zero, but this should *almost* never happen.
 */
s16b get_mon_num(int level, int y, int x, byte mp_flags)
{
	int i, p, j, k, mindepth;

	int r_idx;

	long value, total;

	monster_race *r_ptr;

	alloc_entry *table = alloc_race_table;

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	bool quest_level = FALSE;

	bool native_only = TRUE;

	/* Cache damage to non-native monsters */
	u16b dam_non_native = f_info[cave_feat[y][x]].dam_non_native;

	/*Get the terrain native flags*/
	u32b native_flags = f_info[cave_feat[y][x]].f_flags3;

	/*filter out non-terrain flags*/
	native_flags &= TERRAIN_MASK;

	if (quest_themed(q_ptr) && (guild_quest_level() == p_ptr->depth)) quest_level = TRUE;

	/* Boost the level, but not for quest levels.  That has already been done */
	if ((level > 0) && (!quest_level))
	{
		/* Occasional "nasty" monste */
		if (one_in_(NASTY_MON))
		{
			/* Pick a level bonus */
			int d = level / 4 + 2;

			/* Boost the level */
			level += ((d < 5) ? d : 5);
		}

		/* Occasional "nasty" monster */
		if (one_in_(NASTY_MON))
		{
			/* Pick a level bonus */
			int d = level / 4 + 2;

			/* Boost the level */
			level += ((d < 5) ? d : 5);
		}
	}

	/* Reset total */
	total = 0L;

	/*enforce a mininum depth on monsters,
	 *which slowly drops if no monsters are available.
	 */
	if ((!(get_mon_num_hook)) || (quest_level)) mindepth = level / 5;
	else mindepth = level / 7;

	/*
	 * Hack -- Allow any monster in elemental terrain. -DG-
	 */
	if ((*dun_cap->can_place_non_native_monsters)()) native_only = FALSE;

	/*
	 * Hack -- Avoid too many native monsters. -DG-
	 */
	else if ((p_ptr->lev < 48) ? (rand_int(10) > 0): (rand_int(4) > 0)) native_only = FALSE;

	do
	{
		/* Process probabilities */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Monsters are sorted by depth */
			if (table[i].level > level) break;

			/* Default */
			table[i].prob3 = 0;

			/* Hack -- No town monsters in dungeon */
			if ((level > 0) && (table[i].level <= 0)) continue;

			/* Get the "r_idx" of the chosen monster */
			r_idx = table[i].index;

			/* Get the actual race */
			r_ptr = &r_info[r_idx];

			if (r_ptr->cur_num >= r_ptr->max_num) continue;

			/* Hack -- No low depth monsters monsters in deeper
			 * parts of the dungeon, except uniques,
			 * Note mindepth is given a lower value just before this "for loop")
			 * when using the get_mon_num_hook to limit the creature species.
			 */
			if ((level > 0) && (table[i].level < mindepth) &&
				(!(r_ptr->flags1 & (RF1_UNIQUE)))) continue;

			/* Hack - sometimes prevent mimics*/
			if (r_ptr->flags1 & (RF1_CHAR_MIMIC))
			{
				if (mp_flags & (MPLACE_NO_MIMIC)) continue;
			}

			/* Hack -- some monsters can only be placed via treatment */
			if (r_ptr->flags2 & (RF2_SPECIAL)) continue;

			/* Hack -- "unique" monsters must be "unique" */
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				bool do_continue = FALSE;

				/*No player ghosts if the option is set, or not called for*/
				if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
				{
					if (mp_flags & (MPLACE_NO_GHOST)) continue;
					if (adult_no_player_ghosts) continue;

					/* Already a player ghost on the level */
					if (ghost_r_idx) continue;

					/* Hack -- No easy player ghosts. */
					if (((p_ptr->depth - r_ptr->level) > 6) &&
						 (r_ptr->level < 60))	continue;
				}

				/* Check quests for uniques*/
				for (k = 0; k < z_info->q_max; k++)
				{
					if (quest_slot_single_r_idx(k))
					{
						/* Is this unique marked for a quest? */
						if (q_info[k].mon_idx == table[i].index)
						{
							/* These uniques are placed somewhere else */
							do_continue = TRUE;

							break;
						}
					}
				}

				if (do_continue) continue;
			}

			/* Depth Monsters never appear out of depth */
			if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (r_ptr->level > effective_depth(p_ptr->depth)))
			{
				continue;
			}

			/*
			 * This is an abbreviated form of the is_monster_native function.
			 * Slightly quicker than calling the function.
			 * If the terrain is special terrain
			 */
			if (native_flags != 0L)
			{
				u32b monster_native = r_ptr->r_native;

				/*Filter out the non-relevant monster native flags*/
				monster_native &= native_flags;

				/*Not a match*/
				if (monster_native != native_flags)
				{
					/*On the first try, get native creatures only*/
					if (native_only) continue;

					/*Or else just get a creature that won't be damaged*/
					else if ((dam_non_native > 0) && !(r_ptr->flags3 & (RF3_FLYING))) continue;
				}
			}

			/* Accept */
			table[i].prob3 = table[i].prob2;

			/* Total */
			total += table[i].prob3;
		}

		/*slowly reduce if the mindepth is too high*/
		if (mindepth <= 6) mindepth --;
		else mindepth -= 5;

		/*We have gone through at least once*/
		native_only = FALSE;

	}

	while ((total <= 0) && (mindepth > 0));

	/* No legal monsters */
	if (total <= 0) return (0);

	/* Pick a monster */
	value = rand_int(total);

	/* Find the monster */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value = value - table[i].prob3;
	}


	/* Power boost */
	p = rand_int(100);

	/* Try for a "harder" monster once (50%) or twice (10%) */
	if (p < 60)
	{
		/* Save old */
		j = i;

		/* Pick a monster */
		value = rand_int(total);

		/* Find the monster */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
	}

	/* Try for a "harder" monster twice (10%) */
	if (p < 10)
	{
		/* Save old */
		j = i;

		/* Pick a monster */
		value = rand_int(total);

		/* Find the monster */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
	}

	/* Result */
	return (table[i].index);
}

/*
 * Helper function for display monlist.  Prints the number of creatures, followed
 * by either a singular or plural version of the race name as appropriate.
 */
static void get_mon_name(char *output_name, size_t max, int r_idx, int in_los)
{
	/* Get monster race and name */
	monster_race *r_ptr = &r_info[r_idx];

	char race_name[80];

	/* Player ghosts get special markings */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{
		/* Get the ghost name. */
		my_strcpy(race_name, player_ghost_name, sizeof(race_name));

		my_strcpy(output_name, "[G] ", max);
	}

	/* Unique names don't have a number */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		monster_desc_race(race_name, sizeof(race_name), r_idx);

		my_strcpy(output_name, "[U] ", max);
	}

	/* Get the monster race name for normal races*/
	else
	{
		monster_desc_race(race_name, sizeof(race_name), r_idx);

		my_strcpy(output_name, format("%3d ", in_los), max);

		/* Make it plural, if needed. */
		if (in_los > 1)
		{
			plural_aux(race_name, sizeof(race_name));
		}
	}

	/* Mix the quantity and the header. */
	my_strcat(output_name, race_name, max);
}

/* Figure out which monsters to display in the sidebar */
void update_mon_sidebar_list(void)
{
	monster_type *m_ptr;
	monster_type *m2_ptr;
	monster_race *r_ptr;
	monster_race *r2_ptr;
	int i;
	int sidebar_count = 0;
	int adj_count = 0;
	int los_count = 0;
	int vis_count = 0;
	int adjacent_monsters[8];
	int *line_of_sight_monsters;
	int *visible_monsters;

	/* Allocate the arrays */
	line_of_sight_monsters = C_ZNEW(mon_max, int);
	visible_monsters = C_ZNEW(mon_max, int);

	/* First clear the old list */
	C_WIPE(sidebar_monsters, SIDEBAR_MONSTER_MAX, int);

	/* First list the targeted monster, if there is one */
	if (p_ptr->health_who)
	{
		/* Must be visible */
	 	if (mon_list[p_ptr->health_who].ml)
	 	{
	 		sidebar_monsters[sidebar_count] = p_ptr->health_who;
	 		sidebar_count++;

	 		/* We are tracking this one */
	 		mon_list[p_ptr->health_who].sidebar = TRUE;
	 	}
	}

	/* Scan the list of monsters on the level */
	for (i = 1; i < mon_max; i++)
	{
		m_ptr = &mon_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Ignore dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Only consider visible monsters */
		if (!m_ptr->ml) continue;

		/* Hack - ignore lurkers and trappers */
		if (r_ptr->d_char == '.') continue;

		/* Already recorded target monster */
		if (i == p_ptr->health_who) continue;

		/* Now decide which list to include them in first adjacent monsters*/
		if ((GET_SQUARE((p_ptr->py - m_ptr->fy)) + GET_SQUARE((p_ptr->px - m_ptr->fx))) < 2)
		{
			adjacent_monsters[adj_count] = i;
			adj_count++;
			continue;
		}
		/* Projectable ones next */
		if (m_ptr->project)
		{
			line_of_sight_monsters[los_count] = i;
			los_count++;
			continue;
		}

		/* Visible, not projectable last */
		visible_monsters[vis_count] = i;
		vis_count++;
		continue;
	}

	/* Sort the lists by monster power using bubble sort */
	/*  First do adjacent monsters */
	for (i = 0; i < adj_count; i++)
	{
		int j;

		for (j = i + 1; j < adj_count; j++)
		{
			m_ptr = &mon_list[adjacent_monsters[i]];
			r_ptr = &r_info[m_ptr->r_idx];
			m2_ptr = &mon_list[adjacent_monsters[j]];
			r2_ptr = &r_info[m2_ptr->r_idx];

			if (r_ptr->mon_power < r2_ptr->mon_power)
			{
				int temp = adjacent_monsters[i];
				adjacent_monsters[i] = adjacent_monsters[j];
				adjacent_monsters[j] = temp;
			}
		}
	}

	/* Now add them to the list */
	for (i = 0; i < adj_count; i++)
	{
		sidebar_monsters[sidebar_count] = adjacent_monsters[i];

		/* We are tracking this one */
		mon_list[sidebar_monsters[sidebar_count]].sidebar = TRUE;

		sidebar_count++;

		/* paranoia - would only happen if SIDEBAR_MONSTER_MAX was less than 10*/
		if (sidebar_count >= SIDEBAR_MONSTER_MAX)
		{
			/* Release the arrays */
			FREE(line_of_sight_monsters);
			FREE(visible_monsters);
			return;
		}
	}

	/*  Next do projectable monsters */
	for (i = 0; i < los_count; i++)
	{
		int j;

		for (j = i + 1; j < los_count; j++)
		{
			m_ptr = &mon_list[line_of_sight_monsters[i]];
			r_ptr = &r_info[m_ptr->r_idx];
			m2_ptr = &mon_list[line_of_sight_monsters[j]];
			r2_ptr = &r_info[m2_ptr->r_idx];

			if (r_ptr->mon_power < r2_ptr->mon_power)
			{
				int temp = line_of_sight_monsters[i];
				line_of_sight_monsters[i] = line_of_sight_monsters[j];
				line_of_sight_monsters[j] = temp;
			}
		}
	}

	/* Now add them to the list */
	for (i = 0; i < los_count; i++)
	{
		sidebar_monsters[sidebar_count] = line_of_sight_monsters[i];

		/* We are tracking this one */
		mon_list[sidebar_monsters[sidebar_count]].sidebar = TRUE;

		sidebar_count++;

		/* Check to see if the list is full */
		if (sidebar_count >= SIDEBAR_MONSTER_MAX)
		{
			/* Release the arrays */
			FREE(line_of_sight_monsters);
			FREE(visible_monsters);

			return;
		}
	}

	/*  Finally to other viewable monsters monsters */
	for (i = 0; i < vis_count; i++)
	{
		int j;

		for (j = i + 1; j < vis_count; j++)
		{
			m_ptr = &mon_list[visible_monsters[i]];
			r_ptr = &r_info[m_ptr->r_idx];
			m2_ptr = &mon_list[visible_monsters[j]];
			r2_ptr = &r_info[m2_ptr->r_idx];

			if (r_ptr->mon_power < r2_ptr->mon_power)
			{
				int temp = visible_monsters[i];
				visible_monsters[i] = visible_monsters[j];
				visible_monsters[j] = temp;
			}
		}
	}

	/* Now add them to the list */
	for (i = 0; i < vis_count; i++)
	{
		sidebar_monsters[sidebar_count] = visible_monsters[i];

		/* We are tracking this one */
		mon_list[sidebar_monsters[sidebar_count]].sidebar = TRUE;

		sidebar_count++;

		/* Check to see if the list is full */
		if (sidebar_count >= SIDEBAR_MONSTER_MAX)
		{
			/* Release the arrays */
			FREE(line_of_sight_monsters);
			FREE(visible_monsters);
			return;
		}
	}

	/* Release the arrays */
	FREE(line_of_sight_monsters);
	FREE(visible_monsters);
}


/*
 * Display visible monsters in a window
 */
void display_monlist(void)
{
	u16b i, j, k;
	int max;
	int line = 1, x = 0;
	int cur_x;
	u16b total_count = 0, disp_count = 0, type_count = 0, los_count = 0;

	byte attr;

	char buf[80];

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_race *r2_ptr;

	monster_vis *list;

	u16b *order;

	bool in_term = (Term != angband_term[0]);

	/* Hallucination is weird */
	if (p_ptr->timed[TMD_IMAGE])
	{
		if (in_term)
			clear_from(0);
		Term_gotoxy(0, 0);
		text_out_to_screen(TERM_ORANGE,
			"You can't believe what you are seeing! It's like a dream!");

		return;
	}

	/* Clear the term if in a subwindow, set x otherwise */
	if (in_term)
	{
		clear_from(0);
		max = Term->hgt - 1;
	}
	else
	{
		x = 13;
		max = Term->hgt - 2;
	}

	/* Allocate the primary array */
	list = C_ZNEW(z_info->r_max, monster_vis);

	/* Scan the list of monsters on the level */
	for (i = 1; i < mon_max; i++)
	{
		m_ptr = &mon_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Ignore dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Only consider visible monsters */
		if (!m_ptr->ml) continue;

		/* Hack - ignore lurkers and trappers */
		if (r_ptr->d_char == '.') continue;

		/* If this is the first one of this type, count the type */
		if (!list[m_ptr->r_idx].count) type_count++;
		if (!list[m_ptr->r_idx].s_attr)
		{


			list[m_ptr->r_idx].s_attr = m_ptr->m_attr ? m_ptr->m_attr : r_ptr->x_attr;
		}

		/* Check for LOS */
		if (m_ptr->project)
		{
			/* Increment the total number of in-LOS monsters */
			los_count++;

			/* Increment the LOS count for this monster type */
			list[m_ptr->r_idx].los++;

			/* Check if asleep and increment accordingly */
			if (m_ptr->m_timed[MON_TMD_SLEEP]) list[m_ptr->r_idx].los_asleep++;

		}
		/* Not in LOS so increment if asleep */
		else if (m_ptr->m_timed[MON_TMD_SLEEP]) list[m_ptr->r_idx].asleep++;

		/* Bump the count for this race, and the total count */
		if (m_ptr->mflag & (MFLAG_HIDE)) list[m_ptr->r_idx].hidden++;
		else list[m_ptr->r_idx].count++;

		total_count++;
	}

	/* Note no visible monsters at all */
	if (!total_count)
	{
		/* Player is Blind */
		if (p_ptr->timed[TMD_BLIND])
		{
			c_prt(TERM_ORANGE, "You can't see anything!", 0, 0);
		}

		/* Clear display and print note */
		else c_prt(TERM_SLATE, "You see no monsters.", 0, 0);
		if (!in_term)
		    Term_addstr(-1, TERM_WHITE, "  (Press any key to continue.)");

		/* Free up memory */
		FREE(list);

		/* Done */
		return;
	}

	/* Allocate the secondary array */
	order = C_ZNEW(type_count, u16b);

	/* Sort, because we cannot rely on monster.txt being ordered */

	/* Populate the ordered array, starting at 1 to ignore @ */
	for (i = 1; i < z_info->r_max; i++)
	{
		/* No monsters of this race are visible */
		if ((list[i].count + list[i].hidden) == 0) continue;

		/* Get the monster info */
		r_ptr = &r_info[i];

		/* Fit this monster into the sorted array */
		for (j = 0; j < type_count; j++)
		{
			/* If we get to the end of the list, put this one in */
			if (!order[j])
			{
				order[j] = i;
				break;
			}

			/* Get the monster info for comparison */
			r2_ptr = &r_info[order[j]];

			/* Monsters are sorted by depth */
			/* Monsters of same depth are sorted by power */
			if ((r_ptr->level > r2_ptr->level) ||
				((r_ptr->level == r2_ptr->level) &&
				(r_ptr->mon_power > r2_ptr->mon_power)))
			{
				/* Move weaker monsters down the array */
				for (k = type_count - 1; k > j; k--)
				{
					order[k] = order[k - 1];
				}

				/* Put current monster in the right place */
				order[j] = i;
				break;
			}
		}
	}

   	/* Message for monsters in LOS - even if there are none */
	if (!los_count) prt(format("You can see no monsters."), 0, 0);
	else prt(format("You can see %d monster%s", los_count, (los_count == 1
		? ":" : "s:")), 0, 0);

	/* Print out in-LOS monsters in descending order */
	for (i = 0; (i < type_count) && (line < max); i++)
	{
		int r_idx = order[i];
		char output_name[80];

		int in_los = list[r_idx].los;

		/* Skip if there are none of these in LOS */
		if (!in_los) continue;


		r_ptr = &r_info[r_idx];

		/* Reset position */
		cur_x = x;

		/* Note that these have been displayed */
		disp_count += in_los;

		/* Get monster race and name */
		get_mon_name(output_name, sizeof(output_name), r_idx, in_los);

		/* Display uniques in a special colour */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			attr = TERM_VIOLET;
		}
		else if (r_ptr->level > effective_depth(p_ptr->depth))
		{
			attr = TERM_RED;
		}
		else attr = TERM_WHITE;

		/* Build the monster name */
		if (list[r_idx].los == 1)
		{
			if (list[r_idx].los_asleep == 1)
			{
				strnfmt(buf, sizeof(buf), "%s (asleep) ", output_name);
			}
			else strnfmt(buf, sizeof(buf), "%s", output_name);
		}
		else
		{
			if (list[r_idx].los_asleep > 0)
			{
				strnfmt(buf, sizeof(buf), "%s (%d asleep) ",
						output_name, list[r_idx].los_asleep);
			}
			else strnfmt(buf, sizeof(buf),  "%s", output_name);
		}

		/* Add hidden count */
		if (list[r_idx].hidden)
		{
			my_strcat(buf, format(" %d hidden", list[r_idx].hidden), sizeof(buf));
		}

		/* Display the pict */
		Term_putch(cur_x++, line, list[r_idx].s_attr, r_ptr->x_char);
		if (use_bigtile) Term_putch(cur_x++, line, 255, -1);
		Term_putch(cur_x++, line, TERM_WHITE, ' ');

		/* Print and bump line counter */
		c_prt(attr, buf, line, cur_x);
		line++;

		/* Page wrap */
		if (!in_term && (line == max) && disp_count != total_count)
		{
			prt("-- more --", line, x);
			anykey();

			/* Clear the screen */
			for (line = 1; line <= max; line++)
				prt("", line, 0);

			/* Reprint Message */
			prt(format("You can see %d monster%s",
				los_count, (los_count > 0 ? (los_count == 1 ?
				":" : "s:") : "s.")), 0, 0);

			/* Reset */
			line = 1;
		}
	}

   	/* Message for monsters outside LOS, if there are any */
	if (total_count > los_count)
	{
		/* Leave a blank line */
		line++;

		prt(format("You are aware of %d %smonster%s",
		(total_count - los_count), (los_count > 0 ? "other " : ""),
		((total_count - los_count) == 1 ? ":" : "s:")), line++, 0);
	}

	/* Print out non-LOS monsters in descending order */
	for (i = 0; (i < type_count) && (line < max); i++)
	{
		int r_idx = order[i];
		char output_name[80];

		int out_of_los = list[r_idx].count - list[r_idx].los;

		/* Skip if there are none of these out of LOS */
		if (!out_of_los) continue;

		r_ptr = &r_info[r_idx];

		/* Reset position */
		cur_x = x;

		/* Note that these have been displayed */
		disp_count += out_of_los;

		/* Get monster race and name */
		get_mon_name(output_name, sizeof(output_name), r_idx, out_of_los);

		/* Display uniques in a special colour */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			attr = TERM_VIOLET;
		}
		else if (r_ptr->level > effective_depth(p_ptr->depth))
		{
			attr = TERM_RED;
		}
		else attr = TERM_WHITE;

		/* Build the monster name */
		if (out_of_los == 1)
		{
			if (list[r_idx].asleep == 1)
			{
				strnfmt(buf, sizeof(buf), "%s (asleep) ", output_name);
			}
			else
			{
				strnfmt(buf, sizeof(buf), "%s ", output_name);
			}
		}
		else
		{
			if (list[r_idx].asleep > 0)
			{
				strnfmt(buf, sizeof(buf), "%s (%d asleep) ", output_name, list[r_idx].asleep);
			}
			else strnfmt(buf, sizeof(buf),  "%s", output_name);
		}

		/* Display the pict */
		Term_putch(cur_x++, line, list[r_idx].s_attr, r_ptr->x_char);
		if (use_bigtile) Term_putch(cur_x++, line, 255, -1);
		Term_putch(cur_x++, line, TERM_WHITE, ' ');

		/* Print and bump line counter */
		c_prt(attr, buf, line, cur_x);
		line++;

		/* Page wrap */
		if (!in_term && (line == max) && disp_count != total_count)
		{
			prt("-- more --", line, x);
			anykey();

			/* Clear the screen */
			for (line = 1; line <= max; line++)
				prt("", line, 0);

			/* Reprint Message */
			prt(format("You are aware of %d %smonster%s",
				(total_count - los_count), (los_count > 0 ?
				"other " : ""), ((total_count - los_count) > 0
				? ((total_count - los_count) == 1 ? ":" : "s:")
				: "s.")), 0, 0);

			/* Reset */
			line = 1;
		}
	}

	/* Print "and others" message if we've run out of space */
	if (disp_count != total_count)
	{
		strnfmt(buf, sizeof buf, "  ...and %d others.", total_count - disp_count);
		c_prt(TERM_WHITE, buf, line, x);
	}

	/* Otherwise clear a line at the end, for main-term display */
	else
	{
		prt("", line, x);
	}

	if (!in_term)
		Term_addstr(-1, TERM_WHITE, "  (Press any key to continue.)");

	/* Free the arrays */
	FREE(list);
	FREE(order);

}

/*
 * Build a string describing a monster in some way.
 *
 * We can correctly describe monsters based on their visibility.
 * We can force all monsters to be treated as visible or invisible.
 * We can build nominatives, objectives, possessives, or reflexives.
 * We can selectively pronominalize hidden, visible, or all monsters.
 * We can use definite or indefinite descriptions for hidden monsters.
 * We can use definite or indefinite descriptions for visible monsters.
 *
 * Pronominalization involves the gender whenever possible and allowed,
 * so that by cleverly requesting pronominalization / visibility, you
 * can get messages like "You hit someone.  She screams in agony!".
 *
 * Reflexives are acquired by requesting Objective plus Possessive.
 *
 * I am assuming that no monster name is more than 65 characters long,
 * so that "char desc[80];" is sufficiently large for any result, even
 * when the "offscreen" notation is added.
 *
 * Note that the "possessive" for certain unique monsters will look
 * really silly, as in "Morgoth, King of Darkness's".  We should
 * perhaps add a flag to "remove" any "descriptives" in the name.
 *
 * Note that "offscreen" monsters will get a special "(offscreen)"
 * notation in their name if they are visible but offscreen.  This
 * may look silly with possessives, as in "the rat's (offscreen)".
 * Perhaps the "offscreen" descriptor should be abbreviated.
 *
 * Mode Flags:
 *   0x01 --> Objective (or Reflexive)
 *   0x02 --> Possessive (or Reflexive)
 *   0x04 --> Use indefinites for hidden monsters ("something")
 *   0x08 --> Use indefinites for visible monsters ("a kobold")
 *   0x10 --> Pronominalize hidden monsters
 *   0x20 --> Pronominalize visible monsters
 *   0x40 --> Assume the monster is hidden
 *   0x80 --> Assume the monster is visible
 *  0x100 --> Ignore suffixes like (hidden) and (offscreen)
 *
 * Useful Modes:
 *   0x00 --> Full nominative name ("the kobold") or "it"
 *   0x04 --> Full nominative name ("the kobold") or "something"
 *   0x80 --> Banishment resistance name ("the kobold")
 *   0x88 --> Killing name ("a kobold")
 *   0x22 --> Possessive, genderized if visable ("his") or "its"
 *   0x23 --> Reflexive, genderized if visable ("himself") or "itself"
 */
void monster_desc(char *desc, size_t max, const monster_type *m_ptr, int mode)
{
	cptr res;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	cptr name = r_ptr->name_full;

	bool seen, pron;

	/*
	 * Is it a hidden mimic? If so, describe
	 * it as an object and exit.
	 */

	/* Can we "see" it (forced, or not hidden + visible) */
	seen = ((mode & (0x80)) || (!(mode & (0x40)) && m_ptr->ml));

	/* Sexed Pronouns (seen and forced, or unseen and allowed) */
	pron = ((seen && (mode & (0x20))) || (!seen && (mode & (0x10))));

	/* First, try using pronouns, or describing hidden monsters */
	if (!seen || pron)
	{
		/* an encoding of the monster "sex" */
		int kind = 0x00;

		/* Extract the gender (if applicable) */
		if (r_ptr->flags1 & (RF1_FEMALE)) kind = 0x20;
		else if (r_ptr->flags1 & (RF1_MALE)) kind = 0x10;

		/* Ignore the gender (if desired) */
		if (!m_ptr || !pron) kind = 0x00;

		/* Assume simple result */
		res = "it";

		/* Brute force: split on the possibilities */
		switch (kind + (mode & 0x07))
		{
			/* Neuter, or unknown */
			case 0x00: res = "it"; break;
			case 0x01: res = "it"; break;
			case 0x02: res = "its"; break;
			case 0x03: res = "itself"; break;
			case 0x04: res = "something"; break;
			case 0x05: res = "something"; break;
			case 0x06: res = "something's"; break;
			case 0x07: res = "itself"; break;

			/* Male (assume human if vague) */
			case 0x10: res = "he"; break;
			case 0x11: res = "him"; break;
			case 0x12: res = "his"; break;
			case 0x13: res = "himself"; break;
			case 0x14: res = "someone"; break;
			case 0x15: res = "someone"; break;
			case 0x16: res = "someone's"; break;
			case 0x17: res = "himself"; break;

			/* Female (assume human if vague) */
			case 0x20: res = "she"; break;
			case 0x21: res = "her"; break;
			case 0x22: res = "her"; break;
			case 0x23: res = "herself"; break;
			case 0x24: res = "someone"; break;
			case 0x25: res = "someone"; break;
			case 0x26: res = "someone's"; break;
			case 0x27: res = "herself"; break;
		}

		/* Copy the result */
		my_strcpy(desc, res, max);
	}


	/* Handle visible monsters, "reflexive" request */
	else if ((mode & 0x02) && (mode & 0x01))
	{
		/* The monster is visible, so use its gender */
		if (r_ptr->flags1 & (RF1_FEMALE)) my_strcpy(desc, "herself", max);
		else if (r_ptr->flags1 & (RF1_MALE)) my_strcpy(desc, "himself", max);
		else my_strcpy(desc, "itself", max);
	}


	/* Handle all other visible monster requests */
	else
	{
		/* It could be a player ghost. */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			/* Get the ghost name. */
			my_strcpy(desc, player_ghost_name, max);
		}

		/* It could be a Unique */
		else if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Start with the name (thus nominative and objective) */
			my_strcpy(desc, name, max);
		}

		/* It could be an indefinite monster */
		else if (mode & 0x08)
		{
			/* XXX Check plurality for "some" */

			/* Indefinite monsters need an indefinite article */
			my_strcpy(desc, my_is_vowel(name[0]) ? "an " : "a ", max);
			my_strcat(desc, name, max);
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			my_strcpy(desc, "the ", max);
			my_strcat(desc, name, max);
		}

		/* Handle the Possessive as a special afterthought */
		if (mode & 0x02)
		{
			/* XXX Check for trailing "s" */

			/* Simply append "apostrophe" and "s" */
			my_strcat(desc, "'s", max);
		}

		/* XXX Perhaps we should use a different attr/char */
		if ((m_ptr->mflag & (MFLAG_HIDE)) && !(mode & 0x100))
		{
			/* Append special notation */
			my_strcat(desc, " (hidden)", max);
		}

		/* Mention "offscreen" monsters XXX XXX */
		if (!panel_contains(m_ptr->fy, m_ptr->fx) && !(mode & 0x100))
		{
			/* Append special notation */
			my_strcat(desc, " (offscreen)", max);
		}
	}
}


/*
 * Build a string describing a monster race, currently used for quests.
 *
 * Assumes a singular monster.  This may need to be run through the
 * plural_aux function in the quest.c file.  (Changes "wolf" to
 * wolves, etc.....)
 *
 * I am assuming that no monster name is more than 65 characters long,
 * so that "char desc[80];" is sufficiently large for any result, even
 * when the "offscreen" notation is added.
 *
 */
void monster_desc_race(char *desc, size_t max, int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	cptr name = r_ptr->name_full;

	/* Write the name */
	my_strcpy(desc, name, max);
}

/*
 * Learn about a monster (by "probing" it)
 */
void lore_probe_monster_aux(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];

	int i;

	i = randint (3);

	/*learn 2 out of three of their basic flags.....*/
	switch (i)
	{
		case 1:
		{

			l_ptr->r_l_flags1 = r_ptr->flags1;
			l_ptr->r_l_flags2 = r_ptr->flags2;
			break;
		}

		case 2:
		{
			l_ptr->r_l_flags1 = r_ptr->flags1;
			l_ptr->r_l_flags3 = r_ptr->flags3;
			break;
		}

		default:
		{
			l_ptr->r_l_flags2 = r_ptr->flags2;
			l_ptr->r_l_flags3 = r_ptr->flags3;
			break;
		}
	}

	/*probing is now much more informative*/
	i = randint (4);

	switch (i)
	{
		case 1:
		{
			/*learn their breaths, and shrieking, firing arrows, etc.....*/
			l_ptr->r_l_flags4 = r_ptr->flags4;
			break;
		}

		case 2:
		{
			/*learn many of monster's offensive spells*/
			l_ptr->r_l_flags5 = r_ptr->flags5;
			break;
		}

		case 3:
		{
			/*learn many of monster's offensive spells*/
			l_ptr->r_l_flags6 = r_ptr->flags6;
			break;
		}

		default:
		{
			/*learn many of their other spells*/
			l_ptr->r_l_flags7 = r_ptr->flags7;
			break;
		}
	}

	/*learn the native terrain one in three times*/
	if (one_in_(3)) l_ptr->r_l_native = r_ptr->r_native;

	/* Hack -- Increse the sightings, and ranged attacks */
	if (l_ptr->sights < MAX_SHORT)	l_ptr->sights += (MAX_SHORT - l_ptr->sights) / 100;
	if (l_ptr->ranged < MAX_UCHAR)	l_ptr->ranged += (MAX_UCHAR - l_ptr->ranged) / 5;

	i = randint (3);
	switch (i)
	{
		case 1:
		{
			/* Observe "maximal" attacks */
			for (i = 0; i < MONSTER_BLOW_MAX; i++)
			{
				/* Examine "actual" blows */
				if (r_ptr->blow[i].effect || r_ptr->blow[i].method)
				{
					/* Hack -- increase observations */
					l_ptr->blows[i] += (MAX_UCHAR - l_ptr->blows[i]) / 2;
				}
			}
			break;
		}

		case 2:
		{
			/* Hack -- Maximal info */
			l_ptr->wake = l_ptr->ignore = MAX_UCHAR;

			break;
		}

		default:
		{
			/* Hack -- know the treasure drops*/
			l_ptr->drop_gold = l_ptr->drop_item =
			(((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
	 		 ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
	  		 ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
	 		 ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
	 		 ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
	 		 ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

			/* Hack -- but only "valid" drops */
			if (r_ptr->flags1 & RF1_ONLY_GOLD) l_ptr->drop_item = 0;
			if (r_ptr->flags1 & RF1_ONLY_ITEM) l_ptr->drop_gold = 0;

			break;

		}

	}

}

/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe_monster(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];

	/*increase the information*/
	lore_probe_monster_aux(m_ptr->r_idx);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->redraw |= (PR_MONSTER);
	}
}


/*
 * Take note that the given monster just dropped some treasure
 *
 * Note that learning the "CHEST/GOOD"/"GREAT" flags gives information
 * about the treasure (even when the monster is killed for the first
 * time, such as uniques, and the treasure has not been examined yet).
 *
 * This "indirect" method is used to prevent the player from learning
 * exactly how much treasure a monster can drop from observing only
 * a single example of a drop.  This method actually observes how much
 * gold and items are dropped, and remembers that information to be
 * described later by the monster recall code.
 */
void lore_treasure(int m_idx, int num_item, int num_gold)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];


	/* Note the number of things dropped */
	if (num_item > l_ptr->drop_item) l_ptr->drop_item = num_item;
	if (num_gold > l_ptr->drop_gold) l_ptr->drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (r_ptr->flags1 & (RF1_DROP_CHEST)) l_ptr->r_l_flags1 |= (RF1_DROP_CHEST);
	if (r_ptr->flags1 & (RF1_DROP_GOOD)) l_ptr->r_l_flags1 |= (RF1_DROP_GOOD);
	if (r_ptr->flags1 & (RF1_DROP_GREAT)) l_ptr->r_l_flags1 |= (RF1_DROP_GREAT);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->redraw |= (PR_MONSTER);
	}
}



/*
 * This function updates the monster record of the given monster
 *
 * This involves extracting the distance to the player (if requested),
 * and then checking for visibility (natural, infravision, see-invis,
 * telepathy), updating the monster visibility flag, redrawing (or
 * erasing) the monster when its visibility changes, and taking note
 * of any interesting monster flags (cold-blooded, invisible, etc).
 *
 * Note the new "mflag" field which encodes several monster state flags,
 * including "view" for when the monster is currently in line of sight,
 * and "mark" for when the monster is currently visible via detection.
 *
 * The only monster fields that are changed here are "cdis" (the
 * distance from the player), "ml" (visible to the player), and
 * "mflag" (to maintain the "MFLAG_VIEW" flag).
 *
 * Note the special "update_monsters()" function which can be used to
 * call this function once for every monster.
 *
 * Note the "full" flag which requests that the "cdis" field be updated,
 * this is only needed when the monster (or the player) has moved.
 *
 * Every time a monster moves, we must call this function for that
 * monster, and update the distance, and the visibility.  Every time
 * the player moves, we must call this function for every monster, and
 * update the distance, and the visibility.  Whenever the player "state"
 * changes in certain ways ("blindness", "infravision", "telepathy",
 * and "see invisible"), we must call this function for every monster,
 * and update the visibility.
 *
 * Routines that change the "illumination" of a grid must also call this
 * function for any monster in that grid, since the "visibility" of some
 * monsters may be based on the illumination of their grid.
 *
 * Note that this function is called once per monster every time the
 * player moves.  When the player is running, this function is one
 * of the primary bottlenecks, along with "update_view()" and the
 * "process_monsters()" code, so efficiency is important.
 *
 * Note the optimized "inline" version of the "distance()" function.
 *
 * A monster is "visible" to the player if (1) it has been detected
 * by the player, (2) it is close to the player and the player has
 * telepathy, or (3) it is close to the player, and in line of sight
 * of the player, and it is "illuminated" by some combination of
 * infravision, torch light, or permanent light (invisible monsters
 * are only affected by "light" if the player can see invisible), or
 * (4) a detected mimic.
 *
 * Monsters which are not on the current panel may be "visible" to
 * the player, and their descriptions will include an "offscreen"
 * reference.  Currently, offscreen monsters cannot be targetted
 * or viewed directly, but old targets will remain set.  XXX XXX
 *
 * The player can choose to be disturbed by several things, including
 * "disturb_move" (monster which is viewable moves in some way), and
 * "disturb_near" (monster which is "easily" viewable moves in some
 * way).  Note that "moves" includes "appears" and "disappears".
 */
void update_mon(int m_idx, bool full)
{
	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int d;

	/* Current location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Seen at all */
	bool is_visible = FALSE;

	/* Seen by vision */
	bool easy = FALSE;

	/* Compute distance and projection status */
	if (full)
	{
		/* Distance components */
		int dy = (py > fy) ? (py - fy) : (fy - py);
		int dx = (px > fx) ? (px - fx) : (fx - px);

		/* Approximate distance */
		d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

		/* Restrict distance */
		if (d > 255) d = 255;

		/* Save the distance */
		m_ptr->cdis = d;

	}

	/* Extract distance */
	else
	{
		/* Extract the distance */
		d = m_ptr->cdis;
	}

	/* Detected */
	if (m_ptr->mflag & (MFLAG_MARK)) is_visible = TRUE;

	/* Nearby */
	if (d <= MAX_SIGHT)
	{
		/* Update projectable status */
		m_ptr->project = FALSE;

		if (m_ptr->cdis < MAX_SIGHT)
		{
			if(projectable(py, px, fy, fx, PROJECT_NONE))
			{
				m_ptr->project = TRUE;
			}
		}

		/* Basic telepathy */
		if (p_ptr->state.telepathy)
		{
			/* Empty mind, no telepathy */
			if (r_ptr->flags2 & (RF2_EMPTY_MIND))
			{
				/* Memorize flags */
				l_ptr->r_l_flags2 |= (RF2_EMPTY_MIND);
			}

			/* Weird mind, occasional telepathy */
			else if (r_ptr->flags2 & (RF2_WEIRD_MIND))
			{
				/* Monster is rarely detectable */
				if (((turn / 10) % 10) == (m_idx % 10))
				{
					/* Detectable */
					is_visible = TRUE;

					/* Memorize flags */
					l_ptr->r_l_flags2 |= (RF2_WEIRD_MIND);

					/* Hack -- Memorize mental flags */
					if (r_ptr->flags2 & (RF2_SMART)) l_ptr->r_l_flags2 |= (RF2_SMART);
					if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->r_l_flags2 |= (RF2_STUPID);
				}
			}

			else if ((m_ptr->mflag & (MFLAG_HIDE)) && !player_can_fire_bold(fy, fx))
			{
				;
			}

			/* Normal mind, allow telepathy */
			else
			{
				/* Detectable */
				is_visible = TRUE;

				/* The monster is easy to see (disturb_near) */
 				if (player_has_los_bold(fy, fx) || player_can_fire_bold(fy, fx))
 				{
 					easy = TRUE;
 				}

				/* Hack -- Memorize mental flags */
				if (r_ptr->flags2 & (RF2_SMART)) l_ptr->r_l_flags2 |= (RF2_SMART);
				if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->r_l_flags2 |= (RF2_STUPID);
			}
		}

		/* Normal line of sight, and not blind, and monster isn't hidden  */
		if (player_has_los_bold(fy, fx) && !p_ptr->timed[TMD_BLIND] && !(m_ptr->mflag & (MFLAG_HIDE)))
		{
			bool do_invisible = FALSE;
			bool do_cold_blood = FALSE;

			/* Use "infravision" */
			if (d <= p_ptr->state.see_infra)
			{
				/* Handle "cold blooded" monsters */
				if (r_ptr->flags2 & (RF2_COLD_BLOOD))
				{
					/* Take note */
					do_cold_blood = TRUE;
				}

				/* Handle "warm blooded" monsters */
				else
				{
					/* Easy to see */
					easy = is_visible = TRUE;
				}
			}

	       /* Use "lite carriers" */
		    if ((r_ptr->flags2 & (RF2_HAS_LIGHT)) &&
				!(r_ptr->flags2 & (RF2_INVISIBLE))) easy = is_visible = TRUE;

			/* Use "illumination" */
			if (player_can_see_bold(fy, fx))
			{
				/* Handle "invisible" monsters */
				if (r_ptr->flags2 & (RF2_INVISIBLE))
				{
					/* Take note */
					do_invisible = TRUE;

					/* See invisible */
					if (p_ptr->state.see_inv)
					{
						/* Easy to see */
						easy = is_visible = TRUE;
					}
				}

				/* Handle "normal" monsters */
				else
				{
					/* Easy to see */
					easy = is_visible = TRUE;
				}
			}

			/* Visible */
			if (is_visible)
			{
				/* Memorize flags */
				if (do_invisible) l_ptr->r_l_flags2 |= (RF2_INVISIBLE);
				if (do_cold_blood) l_ptr->r_l_flags2 |= (RF2_COLD_BLOOD);
			}
		}
	}


	/* The monster is now visible */
	if (is_visible)
	{
		/* It was previously unseen */
		/* ... or a hidden monster that is currently being detected */
		if ((!m_ptr->ml) ||
			((m_ptr->mflag & (MFLAG_MARK | MFLAG_HIDE)) == (MFLAG_MARK | MFLAG_HIDE)))
		{
			/* Mark as visible */
			m_ptr->ml = TRUE;

			/* Draw the monster */
			light_spot(fy, fx);

			/* Update health bar as needed */
			if ((p_ptr->health_who == m_idx)  || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH | PR_MON_MANA);

			/* Hack -- Count "fresh" sightings */
			if (l_ptr->sights < MAX_SHORT) l_ptr->sights++;

			/* Player knows if it has light */
			if (r_ptr->flags2 & (RF2_HAS_LIGHT)) l_ptr->r_l_flags2 |= RF2_HAS_LIGHT;

			/* Disturb on visibility change */
			if (disturb_move)
			{
				/* Disturb if monster is not a townsman, or if fairly weak */
				if (!(m_ptr->mflag & (MFLAG_TOWN)) || (p_ptr->lev < 10))
				{
					disturb(1, 0);


				}
			}

			/* Window stuff */
			p_ptr->redraw |= PR_MONLIST;

		}
	}

	/* The monster is not visible */
	else
	{
		/* It was previously seen */
		if (m_ptr->ml)
		{
			/* Mark as not visible */
			m_ptr->ml = FALSE;

			/* Erase the monster */
			light_spot(fy, fx);

			/* Update health bar as needed */
			if ((p_ptr->health_who == m_idx)  || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH | PR_MON_MANA);

			/* Disturb on visibility change */
			if (disturb_move)
			{
				/* Disturb if monster is not a townsman, or if fairly weak */
				if (!(m_ptr->mflag & (MFLAG_TOWN)) || (p_ptr->lev < 10))
				{
					disturb(1, 0);
				}
			}

			/* Window stuff */
			p_ptr->redraw |= PR_MONLIST;

		}
	}

	/* The monster is now easily visible */
	if (easy)
	{
		/* Change */
		if (!(m_ptr->mflag & (MFLAG_VIEW)))
		{
			/* Mark as easily visible */
			m_ptr->mflag |= (MFLAG_VIEW);

			/* Disturb on appearance */
			if (disturb_near)
			{
				/* Disturb if monster is not a townsman, or if fairly weak */
				if (!(m_ptr->mflag & (MFLAG_TOWN)) || (p_ptr->lev < 10))
				{
					disturb(1, 0);
				}
			}

		}
	}

	/* The monster is not easily visible */
	else
	{
		/* Change */
		if (m_ptr->mflag & (MFLAG_VIEW))
		{
			/* Mark as not easily visible */
			m_ptr->mflag &= ~(MFLAG_VIEW);

			/* Disturb on disappearance */
			if (disturb_near)
			{
				/* Disturb if monster is not a townsman, or if fairly weak */
				if (!(m_ptr->mflag & (MFLAG_TOWN)) || (p_ptr->lev < 10))
				{
					disturb(1, 0);
				}

				/* Re-draw monster list window */
				p_ptr->redraw |= PR_MONLIST;
			}

		}
	}
}


/*
 * This function simply updates all the (non-dead) monsters (see above).
 */
void update_monsters(bool full)
{
	int i;

	/* Update each (live) monster */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Update the monster */
		update_mon(i, full);
	}

}


/*
 * Find the right object for a mimic
 * note: lurkers/trappers should return 0
 */
static s16b get_mimic_k_idx(int r_idx)
{
	int i;
	int final_value = 0;
	monster_race *r_ptr = &r_info[r_idx];

	/* Hack - look at default character */
	switch (r_ptr->d_char)
	{

		case '$':
		{
			char mon_name[MAX_MON_LONG_NAME];

			my_strcpy(mon_name, r_ptr->name_full, sizeof(mon_name));

			/* make it all lowecase for simplicity of checking */
			string_lower(mon_name);

			/* Look for textual clues */
			if (strstr(mon_name, " copper "))     	return (lookup_kind(TV_GOLD, SV_GOLD_COPPER));
			if (strstr(mon_name, " silver "))     	return (lookup_kind(TV_GOLD, SV_GOLD_SILVER));
			if (strstr(mon_name, " garnet"))       	return (lookup_kind(TV_GOLD, SV_GOLD_GARNET));
			if (strstr(mon_name, " gold"))       	return (lookup_kind(TV_GOLD, SV_GOLD_GOLD));
			if (strstr(mon_name, " mithril"))    	return (lookup_kind(TV_GOLD, SV_GOLD_MITHRIL));
			if (strstr(mon_name, " opal"))    		return (lookup_kind(TV_GOLD, SV_GOLD_OPALS));
			if (strstr(mon_name, " sapphire"))    	return (lookup_kind(TV_GOLD, SV_GOLD_SAPPHIRES));
			if (strstr(mon_name, " ruby"))    		return (lookup_kind(TV_GOLD, SV_GOLD_RUBIES));
			if (strstr(mon_name, " emerald"))    	return (lookup_kind(TV_GOLD, SV_GOLD_EMERALD));
			if (strstr(mon_name, " diamond"))    	return (lookup_kind(TV_GOLD, SV_GOLD_DIAMOND));
			if (strstr(mon_name, " adamantite ")) 	return (lookup_kind(TV_GOLD, SV_GOLD_ADAMANTITE));
			break;
		}

		/*
		 * Various mimics, such as weapon mimic, armor and dragon armor mimic, and dungeon spellbook.
		 * Special handling for scrolls, since they share the same character as the
		 * dungeon spellbook.
		 */
		case ')':
		case '|':
		case '?':
		case '[':
		{
			cptr name = r_ptr->name_full;

			/* 	Handle scrolls first */
			if (strstr(name, "Scroll"))
			{

				/* Analyze every object */
				for (i = 1; i < z_info->k_max; i++)
				{
					object_kind *k_ptr = &k_info[i];

					/* Skip "empty" objects */
					if (!k_ptr->name) continue;

					/*skip all non-scrolls*/
					if (k_ptr->tval != TV_SCROLL) continue;

					/*don't mimic known items*/
					if (k_ptr->aware) continue;

					/*skip artifacts, let's not annoy the player*/
					if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

					/*we have a suitable object to mimic*/
					if ((final_value == 0) || (one_in_(4))) final_value = i;

				}

				/* Mimic a powerful scroll if they are all identified */
				if (!final_value)
				{
					i = randint(5);
					switch (i)
					{
						case (1): 	return (lookup_kind(TV_SCROLL, SV_SCROLL_RUNE_OF_PROTECTION));
						case (2): 	return (lookup_kind(TV_SCROLL, SV_SCROLL_BANISHMENT));
						case (3): 	return (lookup_kind(TV_SCROLL, SV_SCROLL_MASS_BANISHMENT));
						case (4): 	return (lookup_kind(TV_SCROLL, SV_SCROLL_ACQUIREMENT));
						default:	return (lookup_kind(TV_SCROLL, SV_SCROLL_STAR_ACQUIREMENT));
					}
				}

				return(final_value);
			}

			/* Handle the armor, dragon armor, weapon, and dungeon spellbook mimics */
			return get_object_mimic_k_idx(r_ptr);
		}

		case '!':
		{
			/* Analyze every object */
			for (i = 1; i < z_info->k_max; i++)
			{
				object_kind *k_ptr = &k_info[i];

				/* Skip "empty" objects */
				if (!k_ptr->name) continue;

				/*skip all non-potions*/
				if (k_ptr->tval != TV_POTION) continue;

				/*don't mimic known items*/
				if (k_ptr->aware) continue;

				/*skip artifacts, let's not annoy the player*/
				if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

				/*we have a suitable object to mimic*/
				if ((final_value == 0) || (one_in_(10))) final_value = i;

			}

			/* Mimic a powerful potion if they are all identified */
			if (!final_value)
			{
				i = randint(5);
				switch (i)
				{
					case (1): 	return (lookup_kind(TV_POTION, SV_POTION_EXPERIENCE));
					case (2): 	return (lookup_kind(TV_POTION, SV_POTION_LIFE));
					case (3): 	return (lookup_kind(TV_POTION, SV_POTION_STAR_HEALING));
					case (4): 	return (lookup_kind(TV_POTION, SV_POTION_RESTORE_MANA));
					default:	return (lookup_kind(TV_POTION, SV_POTION_HEALING));
				}
			}

			return(final_value);
		}

		case '=':
		{
			/* Analyze every object */
			for (i = 1; i < z_info->k_max; i++)
			{
				object_kind *k_ptr = &k_info[i];

				/* Skip "empty" objects */
				if (!k_ptr->name) continue;

				/*skip all non-rings*/
				if (k_ptr->tval != TV_RING) continue;

				/*don't mimic known items*/
				if (k_ptr->aware) continue;

				/*skip artifacts, let's not annoy the player*/
				if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

				/*we have a suitable object to mimic*/
				if ((final_value == 0) || (one_in_(5))) final_value = i;

			}

			/* Mimic a powerful ring if they are all identified */
			if (!final_value)
			{
				i = randint(5);
				switch (i)
				{
					case (1): 	return (lookup_kind(TV_RING, SV_RING_SPEED));
					case (2): 	return (lookup_kind(TV_RING, SV_RING_SPEED));
					case (3): 	return (lookup_kind(TV_RING, SV_RING_SPEED));
					case (4): 	return (lookup_kind(TV_RING, SV_RING_RESIST_NETHER));
					default:	return (lookup_kind(TV_RING, SV_RING_RESIST_POIS));
				}
			}

			return(final_value);
		}

		/*staffs*/
		case '_':
		{
			/* Analyze every object */
			for (i = 1; i < z_info->k_max; i++)
			{
				object_kind *k_ptr = &k_info[i];

				/* Skip "empty" objects */
				if (!k_ptr->name) continue;

				/*skip all non-staffs*/
				if (k_ptr->tval != TV_STAFF) continue;

				/*don't mimic known items*/
				if (k_ptr->aware) continue;

				/*skip artifacts, let's not annoy the player*/
				if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

				/*we have a suitable object to mimic*/
				if ((final_value == 0) || (one_in_(6))) final_value = i;

			}

			/* Mimic a powerful staff if they are all identified */
			if (!final_value)
			{
				i = randint(5);
				switch (i)
				{
					case (1): 	return (lookup_kind(TV_STAFF, SV_STAFF_SPEED));
					case (2): 	return (lookup_kind(TV_STAFF, SV_STAFF_DESTRUCTION));
					case (3): 	return (lookup_kind(TV_STAFF, SV_STAFF_HOLINESS));
					case (4): 	return (lookup_kind(TV_STAFF, SV_STAFF_BANISHMENT));
					default:	return (lookup_kind(TV_STAFF, SV_STAFF_MASS_IDENTIFY));
				}
			}

			return(final_value);
		}

		/*mushrooms*/
		case ',':
		{
			/* Analyze every object */
			for (i = 1; i < z_info->k_max; i++)
			{
				object_kind *k_ptr = &k_info[i];

				/* Skip "empty" objects */
				if (!k_ptr->name) continue;

				/*skip all non-mushrooms*/
				if (k_ptr->tval != TV_FOOD) continue;
				if (k_ptr->sval >= SV_FOOD_RATION) continue;

				/*don't mimic known items*/
				if (k_ptr->aware) continue;

				/*skip artifacts, let's not annoy the player*/
				if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

				/*we have a suitable object to mimic*/
				if ((final_value == 0) || (one_in_(5))) final_value = i;

			}

			/* Mimic a good mushroom if they are all identified */
			if (!final_value)
			{
				i = randint(5);
				switch (i)
				{
					case (1): 	return (lookup_kind(TV_FOOD, SV_FOOD_RESTORING));
					case (2): 	return (lookup_kind(TV_FOOD, SV_FOOD_CURE_SERIOUS));
					case (3): 	return (lookup_kind(TV_FOOD, SV_FOOD_BLINDNESS));
					case (4): 	return (lookup_kind(TV_FOOD, SV_FOOD_RESTORE_STR));
					default:	return (lookup_kind(TV_FOOD, SV_FOOD_RESTORE_CON));
				}
			}

			return(final_value);
		}

		/*rods and wands*/
		case '-':
		{
			cptr name = r_ptr->name_full;

			if (strstr(name, "Wand mimic"))
			{
				/* Analyze every object */
				for (i = 1; i < z_info->k_max; i++)
				{
					object_kind *k_ptr = &k_info[i];

					/* Skip "empty" objects */
					if (!k_ptr->name) continue;

					/*skip all non-wands*/
					if (k_ptr->tval != TV_WAND) continue;

					/*don't mimic known items*/
					if (k_ptr->aware) continue;

					/*skip artifacts, let's not annoy the player*/
					if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

					/*we have a suitable object to mimic*/
					if ((final_value == 0) || (one_in_(5))) final_value = i;

				}

				/* Mimic a powerful wand if they are all identified */
				if (!final_value)
				{
					i = randint(5);
					switch (i)
					{
						case (1): 	return (lookup_kind(TV_WAND, SV_WAND_TELEPORT_AWAY));
						case (2): 	return (lookup_kind(TV_WAND, SV_WAND_DRAGON_FIRE));
						case (3): 	return (lookup_kind(TV_WAND, SV_WAND_DRAGON_COLD));
						case (4): 	return (lookup_kind(TV_WAND, SV_WAND_TELEPORT_AWAY));
						default:	return (lookup_kind(TV_WAND, SV_WAND_DRAGON_BREATH));
					}
				}

				return(final_value);
			}

			if (strstr(name, "Rod mimic"))
			{
				/* Analyze every object */
				for (i = 1; i < z_info->k_max; i++)
				{
					object_kind *k_ptr = &k_info[i];

					/* Skip "empty" objects */
					if (!k_ptr->name) continue;

					/*skip all non-rods*/
					if (k_ptr->tval != TV_ROD) continue;

					/*don't mimic known items*/
					if (k_ptr->aware) continue;

					/*skip artifacts, let's not annoy the player*/
					if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

					/*we have a suitable object to mimic*/
					if ((final_value == 0) || (one_in_(5))) final_value = i;

				}

				/* Mimic a powerful rod if they are all identified */
				if (!final_value)
				{
					i = randint(5);
					switch (i)
					{
						case (1): 	return (lookup_kind(TV_ROD, SV_ROD_DETECTION));
						case (2): 	return (lookup_kind(TV_ROD, SV_ROD_HEALING));
						case (3): 	return (lookup_kind(TV_ROD, SV_ROD_RESTORATION));
						case (4): 	return (lookup_kind(TV_ROD, SV_ROD_STAR_IDENTIFY));
						default:	return (lookup_kind(TV_ROD, SV_ROD_MASS_IDENTIFY));
					}
				}

				return(final_value);
			}

			/*can be 0 if all items are identified*/
			return(final_value);
		}

		case '"':
		{
			/* Analyze every object */
			for (i = 1; i < z_info->k_max; i++)
			{
				object_kind *k_ptr = &k_info[i];

				/* Skip "empty" objects */
				if (!k_ptr->name) continue;

				/*skip all non-amulets*/
				if (k_ptr->tval != TV_AMULET) continue;

				/*don't mimic known items*/
				if (k_ptr->aware) continue;

				/*skip artifacts, let's not annoy the player*/
				if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

				/*we have a suitable object to mimic*/
				if ((final_value == 0) || (one_in_(5))) final_value = i;

			}

			/* Mimic a good amulet if they are all identified */
			if (!final_value)
			{
				i = randint(5);
				switch (i)
				{
					case (1): 	return (lookup_kind(TV_AMULET, SV_AMULET_THE_MAGI));
					case (2): 	return (lookup_kind(TV_AMULET, SV_AMULET_DEVOTION));
					case (3): 	return (lookup_kind(TV_AMULET, SV_AMULET_WEAPONMASTERY));
					case (4): 	return (lookup_kind(TV_AMULET, SV_AMULET_TRICKERY));
					default:	return (lookup_kind(TV_AMULET, SV_AMULET_RESIST));
				}
			}

			return(final_value);
		}

		/*chests*/
		case '~':
		{
			i = randint(10);

			/* Look for textual clues */
			if (i <  7) return (lookup_kind(TV_CHEST, (SV_CHEST_MIN_SMALL + rand_int (3))));
			else if (i <  10) return (lookup_kind(TV_CHEST, (SV_CHEST_MIN_LARGE + rand_int (3))));
			else return (lookup_kind(TV_CHEST, SV_CHEST_JEWELED_LARGE));
		}

		default: return (0);
	}


	/* Result */
	return (0);
}


/* Place an mimic object in the dungeon */
static bool place_mimic_object(int y, int x, int r_idx)
{

	s16b k_idx = get_mimic_k_idx(r_idx);
	object_type object_type_body;
	object_type *o_ptr = &object_type_body;

	/* Failure */
	if (!k_idx) return (FALSE);

	/* Wipe the object */
	object_wipe(o_ptr);

	/* Create either a gold mimic or an object mimic */
	if (k_info[k_idx].tval == TV_GOLD)
	{
		coin_type = k_info[k_idx].sval;
		make_gold(o_ptr);
		coin_type = 0;
	}
	else
	{
		object_prep(o_ptr, k_idx);

		/* Apply magic (allow artifacts) */
		apply_magic(o_ptr, object_level, FALSE, TRUE, TRUE, FALSE);
	}

	/* Mark it as a mimic */
	o_ptr->mimic_r_idx = r_idx;

	return (drop_near(o_ptr, 0, y, x));
}

/*
 * Make a monster carry an object
 */
s16b monster_carry(int m_idx, object_type *j_ptr)
{
	s16b o_idx;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &mon_list[m_idx];

	/* Scan objects already being held for combination */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		if ((o_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

		/* Check for combination */
		if (object_similar(o_ptr, j_ptr))
		{
			/* Combine the items */
			object_absorb(o_ptr, j_ptr);

			/* Result */
			return (this_o_idx);
		}
	}


	/* Make an object */
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr;

		/* Get new object */
		o_ptr = &o_list[o_idx];

		/* Copy object */
		object_copy(o_ptr, j_ptr);

		/* Forget mark */
		o_ptr->marked = FALSE;

		/* Forget location */
		o_ptr->iy = o_ptr->ix = 0;

		/* Link the object to the monster */
		o_ptr->held_m_idx = m_idx;

		/* Link the object to the pile */
		o_ptr->next_o_idx = m_ptr->hold_o_idx;

		/* Link the monster to the object */
		m_ptr->hold_o_idx = o_idx;
	}

	/* Result */
	return (o_idx);
}

/*
 * Helper function for monster_swap.  When a player is being moved,
 * returns true if the old terrain is a different terrain type than the new
 * terrain.
 */
static bool player_terrain_changed(int y1, int x1, int y2, int x2)
{
	int feat1 = cave_feat[y1][x1];
	int feat2 = cave_feat[y2][x2];
	u32b elem_flags1 = feat_ff3_match(feat1, TERRAIN_MASK);
	u32b elem_flags2 = feat_ff3_match(feat2, TERRAIN_MASK);

	/* Different terrain type */
	if (elem_flags1 != elem_flags2) return (TRUE);

	/* Different damage level */
	if (f_info[feat1].dam_non_native != f_info[feat2].dam_non_native) return (TRUE);

	return(FALSE);
}

/*
 * Swap the players/monsters (if any) at two locations XXX XXX XXX
 */
void monster_swap(int y1, int x1, int y2, int x2)
{
	int m1, m2;

	monster_type *m_ptr;
	monster_race *r_ptr;

	feature_lore *f_l_ptr = &f_l_list[cave_feat[y2][x2]];

	/* Monsters */
	m1 = cave_m_idx[y1][x1];
	m2 = cave_m_idx[y2][x2];

	/* Update grids */
	cave_m_idx[y1][x1] = m2;
	cave_m_idx[y2][x2] = m1;

	/* Monster 1 */
	if (m1 > 0)
	{
		m_ptr = &mon_list[m1];

		r_ptr = &r_info[m_ptr->r_idx];

		/* Unhide the monster if necessary */
		if ((m_ptr->mflag & (MFLAG_HIDE)) && !(cave_ff2_match(y2, x2, FF2_COVERED) &&
			is_monster_native(y2, x2, r_ptr)))
		{
			monster_unhide(m_ptr);
		}

		/* Move monster */
		m_ptr->fy = y2;
		m_ptr->fx = x2;

		/* Update monster */
		(void)update_mon(m1, TRUE);

		/* Try to hide the monster */
		monster_hide(m_ptr);
	}

	/* Player 1 */
	else if (m1 < 0)
	{
		/* Check if we need to update the statusline */
		if (player_terrain_changed(y1, x1, y2, x2)) p_ptr->redraw |= (PR_STATUS);

		/* Move player */
		p_ptr->py = y2;
		p_ptr->px = x2;

		/* Take any accumulated damage from terrain */
		process_player_terrain_damage();

		/* Dead player? */
		if (p_ptr->is_dead) return;

		/*Note the stealth effect*/
		if ((player_can_observe()) && (f_l_ptr->f_l_stealth_adj < MAX_UCHAR)) f_l_ptr->f_l_stealth_adj++;

		/*Automatically track the feature the player is on unless player is tracking a feature*/
		if ((!p_ptr->target_set) || (p_ptr->target_who != 0)) feature_kind_track(cave_feat[y2][x2]);

		/* Update the trap detection status */
		p_ptr->redraw |= (PR_DTRAP | PR_ITEMLIST);

		/* Update the panel */
		p_ptr->update |= (PU_PANEL | PU_STEALTH);

		/* Update the visuals (and monster distances) */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);
	}

	/* Monster 2 */
	if (m2 > 0)
	{
		m_ptr = &mon_list[m2];

		r_ptr = &r_info[m_ptr->r_idx];

		/* Unhide the monster if necessary */
		if ((m_ptr->mflag & (MFLAG_HIDE)) && !(cave_ff2_match(y1, x1, FF2_COVERED) &&
			is_monster_native(y1, x1, r_ptr)))
		{
			monster_unhide(m_ptr);
		}

		/* Move monster */
		m_ptr->fy = y1;
		m_ptr->fx = x1;

		/* Update monster */
		(void)update_mon(m2, TRUE);

		/* Try to hide the monster */
		monster_hide(m_ptr);

		/* Redraw monster list */
		p_ptr->redraw |= (PR_MONLIST);
	}

	/* Player 2 */
	else if (m2 < 0)
	{
		/* Check if we need to update the statusline */
		if (player_terrain_changed(y1, x1, y2, x2)) p_ptr->redraw |= (PR_STATUS);

		/* Move player */
		p_ptr->py = y1;
		p_ptr->px = x1;

		/* Take any accumulated damage from terrain */
		process_player_terrain_damage();

		/* Dead player? */
		if (p_ptr->is_dead) return;

		/*Note the stealth effect*/
		if ((player_can_observe()) && (f_l_ptr->f_l_stealth_adj < MAX_UCHAR)) f_l_ptr->f_l_stealth_adj++;

		/*Automatically track the feature the player is on unless player is tracking a feature*/
		if ((!p_ptr->target_set) || (p_ptr->target_who != 0)) feature_kind_track(cave_feat[y1][x1]);

		/* Update the trap detection status, itemlist and monlist */
		p_ptr->redraw |= (PR_DTRAP | PR_ITEMLIST | PR_MONLIST);

		/* Update the panel and player stealth */
		p_ptr->update |= (PU_PANEL | PU_STEALTH);

		/* Update the visuals (and monster distances) */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

		/* Window stuff */
		p_ptr->redraw |= (PR_MAP | PR_FEATURE);
	}

	/* Redraw */
	light_spot(y1, x1);
	light_spot(y2, x2);
}


/*
 * Place the player in the dungeon XXX XXX
 */
bool player_place(int y, int x)
{

	/* Paranoia XXX XXX */
	if (cave_m_idx[y][x] != 0) return (FALSE);

	/* Save player location */
	p_ptr->py = y;
	p_ptr->px = x;

	/* Mark cave grid */
	cave_m_idx[y][x] = -1;

	/* Hack -- track this feature */
	feature_kind_track(cave_feat[y][x]);

	/* Window stuff */
	p_ptr->redraw |= (PR_FEATURE);

	/* Success */
	return (TRUE);
}

/*
 * Hide a monster in terrain, if possible
 */
void monster_hide(monster_type *m_ptr)
{
	/* Get the monster race */
	monster_race *r_ptr= &r_info[m_ptr->r_idx];

	/* Get location of the monster */
	int y = m_ptr->fy;
	int x = m_ptr->fx;

	/* Paranoia */
	if (m_ptr->mflag & (MFLAG_HIDE)) return;

	/* Don't hide flying monsters */
	/*if (m_ptr->mflag & (MFLAG_FLYING)) return;*/

	/* Check for suitable terrain */
	if (cave_ff2_match(y, x, FF2_COVERED) && is_monster_native(y, x, r_ptr))
	{
		/* Hack --- tell the player if something hides */
		if (character_dungeon && (cave_info[y][x] & (CAVE_MARK)) && m_ptr->ml &&
			player_can_fire_bold(y, x) && player_can_observe())
		{
			char m_name[80];
			char feat_name[80];
			/* Get the feature */
			u16b feat = cave_feat[y][x];

			/* Get the terrain lore */
			feature_lore *f_l_ptr = &f_l_list[feat];

			/* Get the monster lore */
			monster_lore *l_ptr = &l_list[m_ptr->r_idx];

			/* Update the terrain lore */
			f_l_ptr->f_l_flags2 |= (FF2_COVERED);

			/* Update monster lore */
			l_ptr->r_l_native |= feat_ff3_match(feat, r_ptr->r_native);

			/* Get the monster name */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);

			/* Get the feature name */
			feature_desc(feat_name, sizeof(feat_name), feat, FALSE, TRUE);

			/* Show a message */
			if (m_ptr->m_timed[MON_TMD_SLEEP])
			{
				msg_c_format(MSG_HIDE_UNHIDE, "It seems that %s was engulfed by the %s.",
					m_name, feat_name);
			}
			else
			{
				msg_c_format(MSG_HIDE_UNHIDE, "%^s hides in the %s.", m_name, feat_name);
			}

		}

		/* Mark the monster */
		m_ptr->mflag |= (MFLAG_HIDE);

		 /* Update 'monster list' window */
		p_ptr->redraw |= PR_MONLIST;

		/* Update the graphics of the monster */
		if (character_dungeon)
		{
			/* Remember visibility */
			bool seen = m_ptr->ml;

			update_mon((int)(m_ptr - mon_list), FALSE);

			/* Hack -- Show a special character for hidden monsters */
			if (seen && m_ptr->ml) light_spot(y, x);
		}
	}
}

/*
 * Unhide a monster in terrain, if possible
 */
void monster_unhide(monster_type *m_ptr)
{
	int y;
	int x;

	bool seen;

	monster_race *r_ptr;

	/* Paranoia */
	if (!(m_ptr->mflag & (MFLAG_HIDE))) return;

	/* Get monster location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Get the monster race */
	r_ptr = &r_info[m_ptr->r_idx];

	/* Reveal the monster */
	m_ptr->mflag &= ~(MFLAG_HIDE);

	/* Remember visibility */
	seen = m_ptr->ml;

	/* And update */
	update_mon((int)(m_ptr - mon_list), FALSE);

	/* Hack -- Show a special character for hidden monsters */
	if (seen && m_ptr->ml) light_spot(y, x);

	/* Hack --- tell the player if something unhides */
	if ((cave_info[y][x] & (CAVE_MARK)) && m_ptr->ml && player_can_observe() &&
		player_can_fire_bold(y, x))
	{
		char m_name[80];
		char feat_name[80];
		/* Get the feature */
		u16b feat = cave_feat[y][x];

		/* Get the terrain lore */
		feature_lore *f_l_ptr = &f_l_list[feat];

		/* Get the monster lore */
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Mark the feature lore */
		f_l_ptr->f_l_flags2 |= (FF2_COVERED);

		/* Mark the monster lore */
		l_ptr->r_l_native |= feat_ff3_match(feat, r_ptr->r_native);

		/* Get the monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Get feature name */
		feature_desc(feat_name, sizeof(feat_name), feat, FALSE, TRUE);

		/* Notify */
		msg_c_format(MSG_HIDE_UNHIDE, "%^s emerges from the %s.", m_name, feat_name);
	}

	/* Update 'monster list' window */
	p_ptr->redraw |= PR_MONLIST;
}


/*
 * Place a copy of a monster in the dungeon XXX XXX
 */
s16b monster_place(int y, int x, monster_type *n_ptr)
{
	s16b m_idx;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Paranoia XXX XXX */
	if (cave_m_idx[y][x] != 0) return (0);

	/* Get a new record */
	m_idx = mon_pop();

	/* Oops */
	if (m_idx)
	{
		/* Make a new monster */
		cave_m_idx[y][x] = m_idx;

		/* Get the new monster */
		m_ptr = &mon_list[m_idx];

		/* Copy the monster XXX */
		COPY(m_ptr, n_ptr, monster_type);

		/* Location */
		m_ptr->fy = y;
		m_ptr->fx = x;

		/* Clear flags */
		m_ptr->mflag &= ~(MFLAG_HIDE);

		/* Get the new race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Hack -- Notice new multi-hued monsters */
		if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

		/* Hack -- Count the number of "reproducers" */
		if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;

		/* Protect flying monsters from dangerous terrain */
		if (!cave_no_dam_for_mon(y, x, r_ptr) && (r_ptr->flags3 & (RF3_FLYING)))
		{
			/* Make the monster fly */
			m_ptr->mflag |= (MFLAG_FLYING);
		}

		/* Count racial occurances */
		r_ptr->cur_num++;

		/* Place as hidden as appropriate */
		monster_hide(m_ptr);

		/* Update the monster */
		update_mon(m_idx, TRUE);

	}

	/* Result */
	return (m_idx);
}

/*
 * Determine if a town-dweller is not a threat.
 *
 * Freely admitted:  this whole concept is a hack.
 */
static bool no_threat(const monster_race *r_ptr)
{
	int i;

	/*if they can cast spells, they are a threat*/
	if (r_ptr->freq_ranged) return (TRUE);

	/* Scan blows */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
	{
		/* Extract the attack information */
		int effect = r_ptr->blow[i].effect;
		int method = r_ptr->blow[i].method;
		int d_dice = r_ptr->blow[i].d_dice;
		int d_side = r_ptr->blow[i].d_side;

		/* Hack -- no more attacks */
		if (!method) break;

		/* Can hurt the character (more than a little bit)  XXX XXX */
		if (d_dice * d_side > 2) return (FALSE);

		/* Can steal from the character */
		if ((effect == RBE_EAT_GOLD) || (effect == RBE_EAT_ITEM)) return (FALSE);

	}

	/* Harmless */
	return (TRUE);
}

/*calculate the monster_speed of a monster at a given location*/
void calc_monster_speed(int y, int x)
{
	int speed, i;

	/*point to the monster at the given location & the monster race*/
	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Paranoia XXX XXX */
	if (cave_m_idx[y][x] == 0) return;

	if (game_mode == GAME_NPPMORIA)
	{
		speed = r_ptr->r_speed;
		if (m_ptr->m_timed[MON_TMD_SLOW]) speed--;
		if (m_ptr->m_timed[MON_TMD_FAST]) speed++;
		m_ptr->m_speed = speed;

		return;
	}

	/* Get the monster base speed */
	speed = r_ptr->r_speed;

	/*note: a monster should only have one of these flags*/
	if (m_ptr->mflag & (MFLAG_SLOWER))
	{
		/* Allow some small variation each time to make pillar dancing harder */
		i = calc_energy_gain(r_ptr->r_speed) / 10;
		speed -= rand_range(0, i);
	}
	else if (m_ptr->mflag & (MFLAG_FASTER))
	{
		/* Allow some small variation each time to make pillar dancing harder */
		i = calc_energy_gain(r_ptr->r_speed) / 10;
		speed += rand_range(0, i);
	}

	/*factor in the hasting and slowing counters*/
	if (m_ptr->m_timed[MON_TMD_FAST]) speed += 10;
	if (m_ptr->m_timed[MON_TMD_SLOW]) speed -= 10;

	/*set the speed and return*/
	m_ptr->m_speed = speed;

	return;
}



/*
 * Attempt to place a monster of the given race at the given location.
 *
 * To give the player a sporting chance, any monster that appears in
 * line-of-sight and is extremely dangerous can be marked as
 * "FORCE_SLEEP", which will cause them to be placed with low energy,
 * which often (but not always) lets the player move before they do.
 *
 * This routine refuses to place out-of-depth "FORCE_DEPTH" monsters.
 *
 * XXX XXX XXX Use special "here" and "dead" flags for unique monsters,
 * remove old "cur_num" and "max_num" fields.
 *
 * XXX XXX XXX Actually, do something similar for artifacts, to simplify
 * the "preserve" mode, and to make the "what artifacts" flag more useful.
 *
 * This is the only function which may place a monster in the dungeon,
 * except for the savefile loading code.
 */
static bool place_monster_one(int y, int x, int r_idx, byte mp_flags)
{

	monster_race *r_ptr;

	monster_type *n_ptr;
	monster_type monster_type_body;

	cptr name;

	/* Handle failure of the "get_mon_num()" function */
	if (!r_idx) return (FALSE);

	/* Race */
	r_ptr = &r_info[r_idx];

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* No new monsters on labyrinth, themed and wilderness levels */
	if ((!(*dun_cap->allow_level_repopulation)()) && (character_dungeon == TRUE))
	{
		/* Unless we are revealing a mimic or replacing a missing quest monster */
		if (!(mp_flags & (MPLACE_OVERRIDE))) return (FALSE);
	}

	/*
	 * Place a mimic object rather than a monster if called for
	 */
	if (!(mp_flags & (MPLACE_NO_MIMIC)) &&
		(r_ptr->flags1 & (RF1_CHAR_MIMIC)))
	{
		return (place_mimic_object(y, x, r_idx));
	}

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Hack -- no creation on glyph of warding */
	if (cave_player_glyph_bold(y, x)) return (FALSE);

	/* The monster must be able to exist in this grid */
	if (!cave_exist_mon(r_ptr, y, x, FALSE, FALSE, FALSE)) return (FALSE);

	/* Paranoia */
	if (!r_ptr->r_speed) return (FALSE);

	/* Limit the population */
	if (r_ptr->cur_num >= r_ptr->max_num)
	{
		return (FALSE);
	}

	/* Name */
	name = r_ptr->name_full;

	/* Hack -- "unique" monsters must be "unique" */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		int i;

		/* Check quests for uniques*/
		for (i = 0; i < z_info->q_max; i++)
		{
			quest_type *q_ptr = &q_info[i];

			if (quest_fixed(q_ptr))
			{
				/*is this unique marked for a quest?*/
				if (q_ptr->mon_idx == r_idx)
				{
					/*Is it at the proper depth?*/
					/* Special placement of the moria monsters */
					if (game_mode == GAME_NPPMORIA)
					{
						if(p_ptr->depth < q_ptr->base_level)	return (FALSE);
					}

					else if(p_ptr->depth != q_ptr->base_level)  return (FALSE);

				}
			}
		}

	}

	/* Hack -- only 1 player ghost at a time */
	if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && ghost_r_idx)
	{
		/* Cannot create */
		return (FALSE);
	}


	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (effective_depth(p_ptr->depth) < r_ptr->level))
	{
		/* Cannot create */
		return (FALSE);
	}

	/* Get local monster */
	n_ptr = &monster_type_body;

	/* Clean out the monster */
	(void)WIPE(n_ptr, monster_type);

	/* Save the race */
	n_ptr->r_idx = r_idx;

	/*
	 * If the monster is a player ghost, perform various manipulations
	 * on it, and forbid ghost creation if something goes wrong.
	 */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{

		if (!prepare_ghost(r_idx))
		{
			return (FALSE);
		}

		name = player_ghost_name;
	}

	/* Town level has some special rules */
	if ((!p_ptr->depth) && (!r_ptr->level))
	{
		/* Hack -- harmless townsmen are not threatening */
		if ((r_ptr->d_char == 't') && (no_threat(r_ptr)))
			n_ptr->mflag |= (MFLAG_TOWN);

		/* Hack -- town dogs and city cats are not out for blood */
		else if ((r_ptr->d_char == 'f') || (r_ptr->d_char == 'C'))
			n_ptr->mflag |= (MFLAG_TOWN);

		/* Mega-hack -- no nasty town dwellers when starting out */
		else if (!p_ptr->lev) return (FALSE);
	}

	/* Enforce sleeping if needed */
	if ((mp_flags & (MPLACE_SLEEP)) && r_ptr->sleep)
	{
		n_ptr->m_timed[MON_TMD_SLEEP] = rand_range((r_ptr->sleep + 1) / 2, r_ptr->sleep);
	}

	/* Assign maximal hitpoints */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
	{
		n_ptr->maxhp = (r_ptr->hdice * r_ptr->hside);
	}
	/*assign hitpoints using dice rolls*/
	else
	{
		n_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* Mark minimum range for recalculation */
	n_ptr->min_range = 0;

	/* Initialize mana */
	n_ptr->mana = r_ptr->mana;

	/* And start out fully healthy */
	n_ptr->hp = n_ptr->maxhp;



	/* 75% non-unique monsters vary their speed*/
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		if (!(one_in_(4)))
		{
			if (one_in_(2))
			n_ptr->mflag |= (MFLAG_SLOWER);
			else n_ptr->mflag |= (MFLAG_FASTER);
		}
	}


	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Give almost no starting energy (avoids clumped movement) */
		n_ptr->m_energy = (byte)rand_int(10);
	}

	else
	{
		/* Give a random starting energy */
		n_ptr->m_energy = (byte)rand_int(50);
	}

	/* Hack - Mark the monsters as summoned by a questor */
	/* "summoner" is a global variable */
	if (summoner && (summoner->mflag & (MFLAG_QUEST)))
	{
		/* Only for uniques */
		if (r_info[summoner->r_idx].flags1 & (RF1_UNIQUE)) n_ptr->mflag |= (MFLAG_QUEST_SUMMON);
	}

	/*mark the using_flow as needing updating*/
	n_ptr->using_flow = NEED_FLOW;

	/* Place the monster in the dungeon */
	if (!monster_place(y, x, n_ptr)) return (FALSE);

	/*calculate the monster_speed*/
	calc_monster_speed(y, x);

	/* Powerful monster */
	if (r_ptr->level > effective_depth(p_ptr->depth))
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
			if (cheat_hear) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - effective_depth(p_ptr->depth)) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
			if (cheat_hear) msg_format("Deep Monster (%s).", name);
			/* Boost rating by half delta-depth */
			rating += (r_ptr->level - effective_depth(p_ptr->depth)) / 2;
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
		if (cheat_hear) msg_format("Unique (%s).", name);
	}

	/* Success */
	return (TRUE);
}

/* Used to place a mimic near a location. This is used when a
 * mimic object reveals itself.  It can't always appear on its original square
 * if there is a monster standing on top of it.
 *
 * Return true if the mimic is placed.
 *
 * Put a message in the queue if requested to do so.
 */
static bool place_mimic_near(int y, int x, int r_idx, bool message, bool questor)
{
	bool success = FALSE;
	int i, y1, x1;
	int final_x = x;
	int final_y = y;

	/* Initially try the spot the mimic is on */
	if (place_monster_one(y, x, r_idx, MPLACE_NO_MIMIC | MPLACE_OVERRIDE)) success = TRUE;

	/* Now search around the space, one layer at a time */
	else for (i = 1; ((i <= 5) && (!success)); i++)
	{
		for (y1 = (y - i); ((y1 <= y + i) && (!success)); y1++)
		{
			for (x1 = (x - i); ((x1 <= x + i) && (!success)); x1++)
			{

				if ((!in_bounds_fully(y1, x1))) continue;

				/* We only want to check the squares on the outer-edge of the box */
				if ((ABS(y1 - y) < i) && (ABS(x1 - x) < i)) continue;

				/* Should be in line of sight of original spot */
				if (!los(y1, x1, y, x)) continue;

				/* Try to place a monster on this spot */
				if (!place_monster_one(y1, x1, r_idx, (MPLACE_NO_MIMIC | MPLACE_OVERRIDE))) continue;

				/* We are done */
				success = TRUE;
				final_y = y1;
				final_x = x1;
			}
		}
	}

	/* Make it appear with a sudden attack. */
	if (success)
	{
		s16b m_idx = cave_m_idx[final_y][final_x];
		monster_type *m_ptr = &mon_list[m_idx];

		m_ptr->m_energy = ENERGY_TO_MOVE;
		m_ptr->mflag |= (MFLAG_ALWAYS_CAST);

		/* Mark it as a quest monster if necessary */
		if (questor) m_ptr->mflag |= (MFLAG_QUEST);

		if (message)
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0x88);

			/* Finally, handle the message */
			add_monster_message(NULL, m_idx, MON_MSG_MIMIC_REVEAL);
			add_monster_message(m_name, m_idx, MON_MSG_MIMIC_APPEARS);
		}
	}

	return (success);
}

/*reveal a mimic, re-light the spot, and print a message if asked for*/
void reveal_mimic(int o_idx, bool message)
{
	/* Get the object */
	object_type *o_ptr = &o_list[o_idx];

	bool questor = ((o_ptr->ident & (IDENT_QUEST)) ? TRUE : FALSE);

	/* Paranoia */
	if (!o_ptr->mimic_r_idx) return;

	/* If we fail to to place the mimic, return */
	if (!place_mimic_near(o_ptr->iy, o_ptr->ix, o_ptr->mimic_r_idx, message, questor)) return;

	/* Delete the object */
	delete_object_idx(o_idx);

	/* Disturb */
	disturb(0, 0);
}



/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX	18

/*
 * Attempt to place a group of monsters around the given location.
 *
 * Hack -- A group of monsters counts as a single individual for the
 * level rating.
 */
static bool place_monster_group(int y, int x, int r_idx, bool slp, s16b group_size)
{
 	monster_race *r_ptr = &r_info[r_idx];

	int old, n, i;
	int start;
	int reduce;

	int hack_n = 0;

 	byte hack_y[GROUP_MAX];
 	byte hack_x[GROUP_MAX];

	/* Hard monsters, smaller groups */
 	if (r_ptr->level > effective_depth(p_ptr->depth))
 	{
		reduce = (r_ptr->level - effective_depth(p_ptr->depth)) / 2;
		group_size -= randint(reduce);
 	}

	if (group_size < 2) group_size = 2;

 	/* Maximum size */
	if (group_size > GROUP_MAX) group_size = GROUP_MAX;

	/* Save the rating */
	old = rating;

	/* Start on the monster */
	hack_n = 1;
	hack_x[0] = x;
	hack_y[0] = y;

	/* Puddle monsters, breadth first, up to group_size */
	for (n = 0; (n < hack_n) && (hack_n < group_size); n++)
	{
		/* Grab the location */
		int hx = hack_x[n];
		int hy = hack_y[n];

		/* Random direction */
		start = rand_int(8);

		/* Check each direction, up to group_size */
		for (i = start; (i < 8 + start) && (hack_n < group_size); i++)
		{
			int mx = hx + ddx_ddd[i % 8];
			int my = hy + ddy_ddd[i % 8];

			/* Attempt to place another monster */
			if (place_monster_one(my, mx, r_idx, slp))
			{
				/* Add it to the "hack" set */
				hack_y[hack_n] = my;
				hack_x[hack_n] = mx;
				hack_n++;
			}
		}
	}

	/* Hack -- restore the rating */
	rating = old;

	/* Success */
	return (TRUE);
}

/*
 * Hack -- help pick an escort type
 */
static int place_monster_idx = 0;

/*
 * Hack -- help pick an escort type
 */
static bool place_monster_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[place_monster_idx];

	monster_race *z_ptr = &r_info[r_idx];

	/* Require similar "race" */
	if (z_ptr->d_char != r_ptr->d_char) return (FALSE);

	/* Skip more advanced monsters */
	if (z_ptr->level > r_ptr->level) return (FALSE);

	/* Skip unique monsters */
	if (z_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Paranoia -- Skip identical monsters */
	if (place_monster_idx == r_idx) return (FALSE);

	/* Okay */
	return (TRUE);
}




/*
 * Attempt to place an escort of monsters around the given location
 */
static void place_monster_escort(int y, int x, int leader_idx, bool slp)
{
	int escort_size;
	int n, i;
	s16b escort_monster_level = monster_level;
	int escort_idx = 0;

	/* Random direction */
	int start;

	monster_race *r_ptr = &r_info[leader_idx];

	int hack_n = 0;

	byte hack_y[GROUP_MAX];
	byte hack_x[GROUP_MAX];

	/* Save previous monster restriction value. */
	bool (*get_mon_num_hook_temp)(int r_idx) = get_mon_num_hook;

	/* Calculate the number of escorts we want. */
	if (r_ptr->flags1 & (RF1_ESCORTS)) escort_size = rand_range(12, 18);
	else escort_size = rand_range(4, 6);

	/* Can never have more escorts than maximum group size */
	if (escort_size > GROUP_MAX) escort_size = GROUP_MAX;

	/* Use the leader's monster type to restrict the escorts. */
	place_monster_idx = leader_idx;

	/* Set the escort hook */
	get_mon_num_hook = place_monster_okay;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Build monster table, get index of first escort */
	escort_idx = get_mon_num(escort_monster_level, y, x, (MPLACE_NO_MIMIC | MPLACE_NO_GHOST));


	while (!escort_idx)
	{
		/* Build monster table, get index of first escort */
		escort_idx = get_mon_num(escort_monster_level, y, x, (MPLACE_NO_MIMIC | MPLACE_NO_GHOST));

		/* No eligible escorts.  Try a slightly deeper depth if monster is out-of-depth */
		if ((!escort_idx) && (escort_monster_level < r_ptr->level)) escort_monster_level++;

		/* Avoid a game freeze if escorts aren't possible*/
		else break;
	}


	/* Start on the monster */
	hack_n = 1;
	hack_x[0] = x;
	hack_y[0] = y;

	/* Puddle monsters, breadth first, up to escort_size */
	for (n = 0; (n < hack_n) && (hack_n < escort_size); n++)
	{
		/* Grab the location */
		int hx = hack_x[n];
		int hy = hack_y[n];

		/* Random direction */
		start = rand_int(8);

		/* Check each direction, up to escort_size */
		for (i = start; (i < 8 + start) && (hack_n < escort_size); i++)
		{
			int mx = hx + ddx_ddd[i % 8];
			int my = hy + ddy_ddd[i % 8];

			/* Place a group of escorts if needed */
			if ((r_info[escort_idx].flags1 & (RF1_FRIENDS)) &&
				!place_monster_group(my, mx, escort_idx, slp, (rand_range(4, 8))))
			{
				continue;
			}

			/* Place a group of escorts if needed */
			else if ((r_info[escort_idx].flags1 & (RF1_FRIEND)) &&
				!place_monster_group(my, mx, escort_idx, slp, (rand_range(2, 3))))
			{
				continue;
			}

			/* Attempt to place another monster */
			else if (!place_monster_one(my, mx, escort_idx, slp))
			{
				continue;
			}

			/* Add grid to the "hack" set */
			hack_y[hack_n] = my;
			hack_x[hack_n] = mx;
			hack_n++;

			/* Get index of the next escort */
			escort_idx = get_mon_num(escort_monster_level, y, x, (MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
		}
	}

	/* Return to previous monster restrictions (usually none) */
	get_mon_num_hook = get_mon_num_hook_temp;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* XXX - rebuild monster table */
	(void)get_mon_num(monster_level, y, x, 0L);
}


/*
 * Attempt to place a monster of the given race at the given location
 *
 * Note that certain monsters are now marked as requiring "friends".
 * These monsters, if successfully placed, and if the "grp" parameter
 * is TRUE, will be surrounded by a "group" of identical monsters.
 *
 * Note that certain monsters are now marked as requiring an "escort",
 * which is a collection of monsters with similar "race" but lower level.
 *
 * Some monsters induce a fake "group" flag on their escorts.
 *
 * Note the "bizarre" use of non-recursion to prevent annoying output
 * when running a code profiler.
 *
 * Note the use of the new "monster allocation table" code to restrict
 * the "get_mon_num()" function to "legal" escort types.
 */
bool place_monster_aux(int y, int x, int r_idx, byte mp_flags)
{

	monster_race *r_ptr = &r_info[r_idx];

	/* Place one monster, or fail */
	if (!place_monster_one(y, x, r_idx, mp_flags)) return (FALSE);

	/* Require the "group" flag */
	if (!(mp_flags & (MPLACE_GROUP))) return (TRUE);

	/* Friends for certain monsters */
	if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		(void)place_monster_group(y, x, r_idx, mp_flags, (s16b)rand_range(6, 10));
	}

	else if (r_ptr->flags1 & (RF1_FRIEND))
	{
		/* Attempt to place a small group */
		(void)place_monster_group(y, x, r_idx, mp_flags, (s16b)(rand_range(2, 3)));
	}

	/* Escorts for certain monsters */
	if ((r_ptr->flags1 & (RF1_ESCORT)) || (r_ptr->flags1 & (RF1_ESCORTS)))
	{
		place_monster_escort(y, x, r_idx, mp_flags);
	}

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "monster_level"
 */
bool place_monster(int y, int x, byte mp_flags)
{
	int r_idx;

	/* Pick a monster */
	r_idx = get_mon_num(monster_level, y, x, mp_flags);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	if (place_monster_aux(y, x, r_idx, mp_flags)) return (TRUE);

	/* Oops */
	return (FALSE);
}


/*
 * Attempt to allocate a random monster in the dungeon.
 *
 * Place the monster at least "dis" distance from the player.
 *
 * Use "slp" to choose the initial "sleep" status
 *
 * Use "monster_level" for the monster level
 */
bool alloc_monster(int dis, byte mp_flags)
{
	int r_idx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;
	int attempts_left = 10000;

	/* Find a legal, distant, unoccupied, space */
	while (attempts_left)
	{
		--attempts_left;

		/* Pick a location */
		y = rand_int(p_ptr->cur_map_hgt);
		x = rand_int(p_ptr->cur_map_wid);

		/* Require a grid that all monsters can exist in. */
		if (!cave_empty_bold(y, x)) continue;

		/* Accept far away grids */
		if (distance(y, x, py, px) >  dis) break;
	}

	if (!attempts_left)
	{
		if (cheat_xtra || cheat_hear)
		{
			msg_print("Warning! Could not allocate a new monster.");
		}

		return (FALSE);
	}

	/* Pick a monster */
	r_idx = get_mon_num(monster_level, y, x, mp_flags);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster. */
	if (!(*dun_cap->can_place_escorts)(r_idx)) mp_flags &= ~(MPLACE_GROUP);

	/* Check ability to place escorts */
	if (place_monster_aux(y, x, r_idx, mp_flags)) return (TRUE);

	/* Nope */
	return (FALSE);
}




/*
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	bool okay = FALSE;
	int i;

	/* Player ghosts cannot be summoned. */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- no specific type specified */
	if (!summon_specific_type) return (TRUE);

	/* Check our requirements */
	switch (summon_specific_type)
	{

		case SUMMON_ANT:
		{
			okay = ((r_ptr->d_char == 'a') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
 			break;
 		}


		case SUMMON_SPIDER:
		{
			okay = ((r_ptr->d_char == 'S') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HOUND:
		{
			okay = (((r_ptr->d_char == 'C') || (r_ptr->d_char == 'Z')) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HYDRA:
		{
			okay = ((r_ptr->d_char == 'M') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_AINU:
		{
			okay = ((r_ptr->d_char == 'A') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));

			break;
		}

		case SUMMON_DEMON:
		{
			okay = ((r_ptr->flags3 & (RF3_DEMON)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}


		case SUMMON_UNDEAD:
		{
			okay = ((r_ptr->flags3 & (RF3_UNDEAD)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));

			break;
		}

		case SUMMON_DRAGON:
		{
			okay = ((r_ptr->flags3 & (RF3_DRAGON)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HI_DEMON:
		{
			okay = ((r_ptr->d_char == 'U') &&
					(r_ptr->flags3 & (RF3_DEMON)));
			break;
		}

		case SUMMON_HI_UNDEAD:
		{
			okay = ((r_ptr->d_char == 'L') ||
			        (r_ptr->d_char == 'V') ||
			        (r_ptr->d_char == 'W'));
			break;
		}


		case SUMMON_HI_DRAGON:
		{
			okay = (r_ptr->d_char == 'D');
			break;
		}

		case SUMMON_WRAITH:
		{
			okay = ((r_ptr->d_char == 'W') &&
			        (r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_UNIQUE:
		{
			if ((r_ptr->flags1 & (RF1_UNIQUE)) != 0) okay = TRUE;
			break;
		}

		case SUMMON_HI_UNIQUE:
		{
			if (((r_ptr->flags1 & (RF1_UNIQUE)) != 0) &&
				(r_ptr->level > (MAX_DEPTH / 2))) okay = TRUE;
			break;
		}


		case SUMMON_KIN:
		{
			okay = ((r_ptr->d_char == summon_kin_type) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_ANIMAL:
		{
			okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_BERTBILLTOM:
		{
			okay = ((r_ptr->d_char == 'T') &&
				(r_ptr->flags1 & (RF1_UNIQUE)) &&
				  ((strstr(r_ptr->name_full, "Bert")) ||
				   (strstr(r_ptr->name_full, "Bill")) ||
				   (strstr(r_ptr->name_full, "Tom" ))));
			break;
		}


		case SUMMON_THIEF:
		{
			int effect;

			/* Scan through all the blows */
			for (i = 0; i < MONSTER_BLOW_MAX; i++)
			{
				/* Extract information about the blow effect */
				effect = r_ptr->blow[i].effect;
				if (effect == RBE_EAT_GOLD) okay = TRUE;
				if (effect == RBE_EAT_ITEM) okay = TRUE;
			}
			break;
		}

		default:
		{
			break;
		}

	}

	/* Result */
	return (okay);
}

/*
 * Attempt to summon creatures who are already on the level.
 *
 */
static bool summon_from_level(int y1, int x1, int lev, int type)
{
	int i, x, y;
	u16b *monster_list;
	u16b mon_count = 0;
	monster_type *m_ptr;

	monster_list = C_ZNEW(mon_max, u16b);

	/* Look for a location */
	for (i = 0; i < 75; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		if (cave_player_glyph_bold(y, x)) continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 75)
	{
		FREE(monster_list);
		return (FALSE);
	}

	/* Save the "summon" type */
	summon_specific_type = type;

	for (i = 1; i < mon_max; i++)
	{
		/* Check the i'th monster */
		m_ptr = &mon_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		if (!summon_specific_okay(m_ptr->r_idx)) continue;

		/* Already in line of sight of the player */
		if (m_ptr->project) continue;

		/* Record this one */
		monster_list[mon_count] = i;
		mon_count++;
	}

	/* No eligible monsters */
	if (!mon_count)
	{
		FREE(monster_list);
		return (FALSE);
	}

	/* Select one, and summon it */
	i = randint0(mon_count);
	m_ptr = &mon_list[monster_list[i]];
	monster_swap(m_ptr->fy, m_ptr->fx, y, x);

	/* Wake it up, make it active, and give the player time to react */
	wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);
	m_ptr->mflag |= (MFLAG_ACTV);

	FREE(monster_list);
	return (TRUE);
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 20 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH (XXX) will summon Uniques
 * Note: SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Uniques
 * Note: None of the other summon codes will ever summon Uniques.
 *
 * We usually do not summon monsters greater than the given depth.  -LM-
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "get_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 * For levels that forbid summoning, we try to bring over other monsters from the same level.
 */
bool summon_specific(int y1, int x1, int lev, int type, byte mp_flags)
{
	int i, x, y, r_idx;

	/* No summoning on verious levels, unless the override flag is present
	 * Override is used for actions such as for reading scrolls of summon monster or wands of polymorph
	 */
	if ((*dun_cap->limited_level_summoning)())
	{
		if (!(mp_flags & (MPLACE_OVERRIDE))) return (summon_from_level(y1, x1, lev, type));
	}

	/* Allow the monster to be placed */
	else mp_flags |= MPLACE_OVERRIDE;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		if (cave_player_glyph_bold(y, x)) continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Save the "summon" type */
	summon_specific_type = type;

	/* Require "okay" monsters */
	get_mon_num_hook = summon_specific_okay;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the given level */
	r_idx = get_mon_num(lev, y, x, MPLACE_NO_GHOST);

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx)
	{
		/* First try to call other creatures on the same level */
		return (summon_from_level(y1, x1, lev, type));
	}

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, MPLACE_GROUP | MPLACE_NO_MIMIC | mp_flags)) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 *
 * Override is to ensure that the cloning happens on levels where multipication is restricted
 */
bool multiply_monster(int m_idx, bool override)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	byte mon_flags = 0L;

 	int i, y, x;

 	bool result = FALSE;

	u16b grid[8];
	int grids = 0;

	/* Scan the adjacent floor grids */
	for (i = 0; i < 8; i++)
	{
		y = m_ptr->fy + ddy_ddd[i];
		x = m_ptr->fx + ddx_ddd[i];

		/* Must be fully in bounds */
		if (!in_bounds_fully(y, x)) continue;

		/* This grid is OK for this monster (should monsters be able to dig?) */
		if (cave_exist_mon(r_ptr, y, x, FALSE, FALSE, FALSE))
		{
			/* Save this grid */
			grid[grids++] = GRID(y, x);
		}
	}

	/* No grids available */
	if (!grids) return (FALSE);

	/* Pick a grid at random */
	i = rand_int(grids);

	/* Get the coordinates */
	y = GRID_Y(grid[i]);
	x = GRID_X(grid[i]);

	if (override) mon_flags |= MPLACE_OVERRIDE;
	else if ((*dun_cap->allow_monster_multiply)()) mon_flags |= MPLACE_OVERRIDE;

	/* Create a new monster (awake, no groups) */
	result = place_monster_aux(y, x, m_ptr->r_idx, mon_flags);

 	/* Result */
 	return (result);
}

/*
 * The NULL-terminated array of string actions used to format stacked messages.
 * Singular and plural modifiers are encoded in the same string. Example:
 * "[is|are] hurt" is expanded to "is hurt" if you request the singular form.
 * The string is expanded to "are hurt" if the plural form is requested.
 * The singular and plural parts are optional. Example:
 * "rear[s] up in anger" only includes a modifier for the singular form.
 * Any of these strings can start with "~", in which case we consider that
 * string as a whole message, not as a part of a larger message. This
 * is useful to display Moria-like death messages.
 */
static const char *msg_repository[MAX_MON_MSG + 1] =
{
	/* Dummy action */
	"[is|are] hurt.",    		/* MON_MSG_NONE */

	/* From message_pain */
	"[is|are] unharmed.",		/* MON_MSG_UNHARMED  */
	"barely notice[s].",		/* MON_MSG_BARELY_NOTICE  */
	"flinch[es].",				/*  MON_MSG_FLINCH */
	"squelch[es].",				/* MON_MSG_SQUELCH  */
	"quiver[s] in pain.",		/* MON_MSG_QUIVER  */
	"writhe[s] about.",			/*  MON_MSG_WRITHE_ABOUT */
	"writhe[s] in agony.",		/* MON_MSG_WRITHE_IN_AGONY  */
	"jerk[s].",					/* MON_MSG_JERK  */
	"jerk[s] limply.",			/*  MON_MSG_JERK_LIMPLY */
	"jerk[s] in pain.",			/* MON_MSG_JERK_IN_PAIN  */
	"jerk[s] in agony.",		/* MON_MSG_JERK_IN_AGONY  */
	"jerk[s] feebly.", 			/* MON_MSG_JERK_FEEBLY */
	"shrug[s] off the attack.",  /*  MON_MSG_SHRUG_OFF */
	"snarl[s].",				/*  MON_MSG_SNARL */
	"snarl[s] with pain.",		/* MON_MSG_SNARL_WITH_PAIN  */
	"howl[s] in pain.",			/* MON_MSG_HOWL_IN_PAIN  */
	"howl[s] in agony.",		/* MON_MSG_HOWL_IN_AGONY  */
	"yelp[s] feebly.",			/* MON_MSG_YELP_FEEBLY  */
	"yelp[s] in pain.",			/* MON_MSG_YELP_IN_PAIN  */
	"hiss[es].",				/* MON_MSG_HISS  */
	"hiss[es] furiously.",		/* MON_MSG_HISS_FURIOUSLY  */
	"hiss[es] with pain.",		/* MON_MSG_HISS_WITH_PAIN  */
	"hiss[es] in agony.",		/* MON_MSG_HISS_IN_AGONY  */
	"rear[s] up in anger.",		/* MON_MSG_REAR_UP_IN_ANGER  */
	"growl[s] angrily.",		/* MON_MSG_GROWL_ANGRILY  */
	"mewl[s] in pain.",			/* MON_MSG_MEWL_IN_PAIN  */
	"mewl[s] pitifully.",		/* MON_MSG_MEWL_PITIFULLY  */
	"ignore[s] the attack.",	/* MON_MSG_IGNORE_ATTACK  */
	"drone[s] angrily.",		/* MON_MSG_DRONE_ANGRILY  */
	"scuttle[s] about.",		/* MON_MSG_SCUTTLE_ABOUT  */
	"twitch[es] in pain.",		/* MON_MSG_TWITCH_IN_PAIN  */
	"flap[s] angrily.",			/* MON_MSG_FLAP_ANGRILY  */
	"jeer[s] in pain.",			/* MON_MSG_JEER_IN_PAIN  */
	"squawk[s] with pain.",		/* MON_MSG_SQUAWK_WITH_PAIN  */
	"twitter[s] in agony.",		/* MON_MSG_TWITTER_IN_AGONY  */
	"flutter[s] about.",		/* MON_MSG_FLUTTER_ABOUT  */
	"chirp[s] feebly.",			/* MON_MSG_CHIRP_FEEBLY  */
	"rattle[s].",				/* MON_MSG_RATTLE  */
	"clatter[s].",				/* MON_MSG_CLATTER  */
	"shake[s].",				/* MON_MSG_SHAKE  */
	"stagger[s].",				/* MON_MSG_STAGGER  */
	"crumple[s].",				/* MON_MSG_CRUMPLE  */
	"grunt[s].",				/* MON_MSG_GRUNT  */
	"grunt[s] with pain.",		/* MON_MSG_GRUNT_WITH_PAIN  */
	"moan[s].",					/* MON_MSG_MOAN  */
	"groan[s].",				/* MON_MSG_GROAN  */
	"hesitate[s].",				/* MON_MSG_HESITATE  */
	"squeal[s] in pain.",		/* MON_MSG_SQUEAL_IN_PAIN  */
	"shriek[s] in pain.",		/* MON_MSG_SHRIEK_IN_PAIN  */
	"shriek[s] in agony.",		/* MON_MSG_SHRIEK_IN_AGONY  */
	"cr[ies|y] out feebly.",	/* MON_MSG_CRY_OUT_FEEBLY  */
	"cr[ies|y] out in pain.",	/* MON_MSG_CRY_OUT_IN_PAIN  */
	"scream[s] in pain.",		/* MON_MSG_SCREAM_IN_PAIN  */
	"scream[s] in agony.",		/* MON_MSG_SCREAM_IN_AGONY  */
	"[is|are] sterilized.",		/* MON_MSG_STERILIZE  */

	/* From project_m */ 		/* MON_MSG_DIE */
	"die[s].",   				/* MON_MSG_DIE  */
	"[is|are] destroyed.",		/* MON_MSG_DESTROYED */
	"[is|are] embedded in the wall.",	/* MON_MSG_BURIED_ROCK */
	"resist[s] a lot.",			/* MON_MSG_RESIST_A_LOT */
	"[is|are] hit hard.",		/* MON_MSG_HIT_HARD */
	"resist[s].",				/* MON_MSG_RESIST */
	"[is|are] immune.",			/* MON_MSG_IMMUNE */
	"resist[s] somewhat.",		/* MON_MSG_RESIST_SOMEWHAT */
	"[is|are] unaffected!",		/* MON_MSG_UNAFFECTED */
	"spawn[s]!",				/* MON_MSG_SPAWN */
	"look[s] healthier.",		/* MON_MSG_HEALTHIER */
	"fall[s] asleep!",			/* MON_MSG_FALL_ASLEEP */
	"wake[s] up.",				/* MON_MSG_WAKES_UP */
	"stir[s].",					/* MON_MSG_STIRS */
	"cringe[s] from the light!",/* MON_MSG_CRINGE_LIGHT */
	"shrivel[s] away in the light!",	/* MON_MSG_SHRIVEL_LIGHT */
	"lose[s] some skin!",		/* MON_MSG_LOSE_SKIN */
	"dissolve[s]!",				/* MON_MSG_DISSOLVE */
	"catch[es] fire!",			/* MON_MSG_CATCH_FIRE */
	"[is|are] badly frozen.", 	 /* MON_MSG_BADLY_FROZEN */
	"[is|are] badly burned.", 	 /* MON_MSG_BADLY_BURNED */
	"[is|are] severely poisoned.", 	 /* MON_MSG_BADLY_POISONED */
	"shudder[s].",				/* MON_MSG_SHUDDER */
	"become[s] aware of your crafty abilities.",/* MON_MSG_AWARE_OF_CRAFTY_ABILITIES */
	"take[s] heed of your cunning tactics.",/* MON_MSG_AWARE_OF_CUNNING_TACTICS  */
	"sense[s] your crafty abilities.",  /* MON_MSG_SENSE_CRAFTY_ABILITIES */
	"sense[s] you are a cunning foe.",	/* MON_MSG_SENSE_CUNNING_FOE */
	"change[s]!",				/* MON_MSG_CHANGE */
	"disappear[s]!",			/* MON_MSG_DISAPPEAR */
	"[is|are] even more stunned.",		/* MON_MSG_MORE_DAZED */
	"[is|are] stunned.",		/* MON_MSG_DAZED*/
	"[is|are] no longer stunned.",	/* MON_MSG_NOT_DAZED */
	"[is|are] more confused.",	/* MON_MSG_MORE_CONFUSED */
	"[is|are] confused.",		/* MON_MSG_CONFUSED */
	"[is|are] no longer confused.",/* MON_MSG_NOT_CONFUSED */
	"[is|are] more slowed.",		/* MON_MSG_MORE_SLOWED */
	"[is|are] slowed.",			/* MON_MSG_SLOWED */
	"speed[s] up.",				/* MON_SNG_NOT_SLOWED */
	"[is|are] more hasted.",		/* MON_MSG_MORE_HASTED */
	"[is|are] hasted.",			/* MON_MSG_HASTED */
	"[is|are] no longer hasted.",/* MON_MSG_NOT_HASTED */
	"[is|are] more terrified!",	/* MON_MSG_MORE_AFRAID */
	"flee[s] in terror!",		/* MON_MSG_FLEE_IN_TERROR */
	"[is|are] no longer afraid.",/* MON_MSG_NOT_AFRAID */
	"~You hear [a|several] scream[|s] of agony!",/* MON_MSG_MORIA_DEATH */
	"disintegrate[s]!",		/* MON_MSG_DISENTEGRATES */
	"melt[s] away.",		/* MON_MSG_MELTS_AWAY */
	"freeze[s] and shatter[s].",  /* MON_MSG_FREEZE_SHATTER */
	"choke[s] and die[s].",  /* MON_MSG_CHOKE_DIE */
	"lose[s] some mana!",		/* MON_MSG_MANA_DRAIN */
	"~There [is|are] [a|several] mimic[|s]!",		/* MON_MSG_MIMIC_REVEAL */
	"appear[s]!",				/* MON_MSG_MIMIC_APPEARS */


	NULL						/* MAX_MON_MSG */
};



/*
 * Dump a message describing a monster's reaction to damage
 *
 * Technically should attempt to treat "Beholder"'s as jelly's
 */
void message_pain(int m_idx, int dam)
{
	long oldhp, newhp, tmp;
	int percentage;
	bool is_jelly = FALSE;
	bool is_hound = FALSE;
	bool is_reptile = FALSE;
	bool is_feline = FALSE;
	bool is_insect = FALSE;
	bool is_bird = FALSE;
	bool is_skeleton = FALSE;
	bool is_zombie = FALSE;
	bool is_other = FALSE;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int msg_code = MON_MSG_UNHARMED;

	char m_name[80];

	/* Get the monster name */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Notice non-damage */
	if (dam == 0)
	{
		add_monster_message(m_name, m_idx, msg_code);

		return;
	}

	/* Note -- subtle fix -CFT */
	newhp = (long)(m_ptr->hp);
	oldhp = newhp + (long)(dam);
	tmp = (newhp * 100L) / oldhp;
	percentage = (int)(tmp);

	/* Figure out which type of creature this is */
	if (game_mode == GAME_NPPMORIA)
	{
		if (strchr("eJmQCOw", r_ptr->d_char)) is_jelly = TRUE;
		if (strchr("j", r_ptr->d_char)) is_hound = TRUE;
		if (strchr("cfR", r_ptr->d_char)) is_reptile = TRUE;
		if (r_ptr->flags1 & (RF1_CHAR_MIMIC)) is_reptile = TRUE;
		if (strchr("AaFKltS", r_ptr->d_char)) is_insect = TRUE;
		if (strchr("s", r_ptr->d_char)) is_skeleton = TRUE;
		if (strchr("zM", r_ptr->d_char)) is_zombie = TRUE;
		if (strchr("XUbqr,", r_ptr->d_char)) is_other = TRUE;
	}
	else
	{
		if (strchr("ejmvQw", r_ptr->d_char)) is_jelly = TRUE;
		if (strchr("CZ", r_ptr->d_char)) is_hound = TRUE;
		if (strchr("cR", r_ptr->d_char)) is_reptile = TRUE;
		if (r_ptr->flags1 & (RF1_CHAR_MIMIC)) is_reptile = TRUE;
		if (strchr("f", r_ptr->d_char)) is_feline = TRUE;
		if (strchr("alFIKS", r_ptr->d_char)) is_insect = TRUE;
		if (strchr("B", r_ptr->d_char)) is_bird = TRUE;
		if (strchr("s", r_ptr->d_char)) is_skeleton = TRUE;
		if (strchr("z", r_ptr->d_char)) is_zombie = TRUE;
		if (strchr("XMbqr,", r_ptr->d_char)) is_other = TRUE;
	}

	/* Floating Eyes, Jelly's, Mold's, Vortex's, Quthl's */
	if (is_jelly)
	{
		if (percentage > 95)
			msg_code = MON_MSG_BARELY_NOTICE;
		else if (percentage > 75)
			msg_code = MON_MSG_FLINCH;
		else if (percentage > 50)
			msg_code = MON_MSG_SQUELCH;
		else if (percentage > 35)
			msg_code = MON_MSG_QUIVER;
		else if (percentage > 20)
			msg_code = MON_MSG_WRITHE_ABOUT;
		else if (percentage > 10)
			msg_code = MON_MSG_WRITHE_IN_AGONY;
		else
			msg_code = MON_MSG_JERK_LIMPLY;
	}

	/* Dogs and Hounds */
	else if (is_hound)
	{
		if (percentage > 95)
			msg_code = MON_MSG_SHRUG_OFF;
		else if (percentage > 75)
			msg_code = MON_MSG_SNARL_WITH_PAIN;
		else if (percentage > 50)
			msg_code = MON_MSG_YELP_IN_PAIN;
		else if (percentage > 35)
			msg_code = MON_MSG_HOWL_IN_PAIN;
		else if (percentage > 20)
			msg_code = MON_MSG_HOWL_IN_AGONY;
		else if (percentage > 10)
			msg_code = MON_MSG_WRITHE_IN_AGONY;
		else
			msg_code = MON_MSG_YELP_FEEBLY;
	}

	/* Snakes, Reptiles, Centipedes, Mimics */
	else if (is_reptile)
	{
		if (percentage > 95)
			msg_code = MON_MSG_BARELY_NOTICE;
		else if (percentage > 75)
			msg_code = MON_MSG_HISS;
		else if (percentage > 50)
			msg_code = MON_MSG_REAR_UP_IN_ANGER;
		else if (percentage > 35)
			msg_code = MON_MSG_HISS_FURIOUSLY;
		else if (percentage > 20)
			msg_code = MON_MSG_WRITHE_ABOUT;
		else if (percentage > 10)
			msg_code = MON_MSG_WRITHE_IN_AGONY;
		else
			msg_code = MON_MSG_JERK_LIMPLY;
	}

	/* Felines */
	else if (is_feline)
	{
		if (percentage > 95)
			msg_code = MON_MSG_SHRUG_OFF;
		else if (percentage > 75)
			msg_code = MON_MSG_SNARL;
		else if (percentage > 50)
			msg_code = MON_MSG_GROWL_ANGRILY;
		else if (percentage > 35)
			msg_code = MON_MSG_HISS_WITH_PAIN;
		else if (percentage > 20)
			msg_code = MON_MSG_MEWL_IN_PAIN;
		else if (percentage > 10)
			msg_code = MON_MSG_HISS_IN_AGONY;
		else
			msg_code = MON_MSG_MEWL_PITIFULLY;
	}

	/* Ants, Lice, Flies, Insects, Beetles, Spiders */
	else if (is_insect)
	{
		if (percentage > 95)
			msg_code = MON_MSG_IGNORE_ATTACK;
		else if (percentage > 75)
			msg_code = MON_MSG_DRONE_ANGRILY;
		else if (percentage > 50)
			msg_code = MON_MSG_SCUTTLE_ABOUT;
		else if (percentage > 35)
			msg_code = MON_MSG_TWITCH_IN_PAIN;
		else if (percentage > 20)
			msg_code = MON_MSG_JERK_IN_PAIN;
		else if (percentage > 10)
			msg_code = MON_MSG_JERK_IN_AGONY;
		else
			msg_code = MON_MSG_JERK_FEEBLY;
	}

	/* Birds */
	else if (is_bird)
	{
		if (percentage > 95)
			msg_code = MON_MSG_SHRUG_OFF;
		else if (percentage > 75)
			msg_code = MON_MSG_FLAP_ANGRILY;
		else if (percentage > 50)
			msg_code = MON_MSG_JEER_IN_PAIN;
		else if (percentage > 35)
			msg_code = MON_MSG_SQUAWK_WITH_PAIN;
		else if (percentage > 20)
			msg_code = MON_MSG_TWITTER_IN_AGONY;
		else if (percentage > 10)
			msg_code = MON_MSG_FLUTTER_ABOUT;
		else
			msg_code = MON_MSG_CHIRP_FEEBLY;
	}

	/* Skeletons (ignore, rattle, stagger) */
	else if (is_skeleton)
	{
		if (percentage > 95)
			msg_code = MON_MSG_IGNORE_ATTACK;
		else if (percentage > 75)
			msg_code = MON_MSG_JERK;
		else if (percentage > 50)
			msg_code = MON_MSG_RATTLE;
		else if (percentage > 35)
			msg_code = MON_MSG_CLATTER;
		else if (percentage > 20)
			msg_code = MON_MSG_SHAKE;
		else if (percentage > 10)
			msg_code = MON_MSG_STAGGER;
		else
			msg_code = MON_MSG_CRUMPLE;
	}

	/* Zombies and Mummies (ignore, groan, stagger) */
	else if (is_zombie)
	{
		if (percentage > 95)
			msg_code = MON_MSG_IGNORE_ATTACK;
		else if (percentage > 75)
			msg_code = MON_MSG_GRUNT;
		else if (percentage > 50)
			msg_code = MON_MSG_JERK;
		else if (percentage > 35)
			msg_code = MON_MSG_MOAN;
		else if (percentage > 20)
			msg_code = MON_MSG_GROAN;
		else if (percentage > 10)
			msg_code = MON_MSG_HESITATE;
		else
			msg_code = MON_MSG_STAGGER;
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (is_other)
	{
		if (percentage > 95)
			msg_code = MON_MSG_IGNORE_ATTACK;
		else if (percentage > 75)
			msg_code = MON_MSG_GRUNT_WITH_PAIN;
		else if (percentage > 50)
			msg_code = MON_MSG_SQUEAL_IN_PAIN;
		else if (percentage > 35)
			msg_code = MON_MSG_SHRIEK_IN_PAIN;
		else if (percentage > 20)
			msg_code = MON_MSG_SHRIEK_IN_AGONY;
		else if (percentage > 10)
			msg_code = MON_MSG_WRITHE_IN_AGONY;
		else
			msg_code = MON_MSG_CRY_OUT_FEEBLY;
	}

	/* Another type of monsters (shrug,cry,scream) */
	else
	{
		if (percentage > 95)
			msg_code = MON_MSG_SHRUG_OFF;
		else if (percentage > 75)
			msg_code = MON_MSG_GRUNT_WITH_PAIN;
		else if (percentage > 50)
			msg_code = MON_MSG_CRY_OUT_IN_PAIN;
		else if (percentage > 35)
			msg_code = MON_MSG_SCREAM_IN_PAIN;
		else if (percentage > 20)
			msg_code = MON_MSG_SCREAM_IN_AGONY;
		else if (percentage > 10)
			msg_code = MON_MSG_WRITHE_IN_AGONY;
		else
			msg_code = MON_MSG_CRY_OUT_FEEBLY;
	}

	/* Save the message for later */
	add_monster_message(m_name, m_idx, msg_code);
}




#define SINGULAR_MON	1
#define PLURAL_MON		2

/*
 * Returns a pointer to a statically allocatted string containing a formatted
 * message based on the given message code and the quantity flag.
 * The contents of the returned value will change with the next call
 * to this function
 */
static char *get_mon_msg_action(byte msg_code, bool do_plural)
{
	static char buf[200];
	const char *action;

	u16b n = 0;
	/* Regular text */
	byte flag = 0;

	/* Put the message characters in the buffer */
	for (action = msg_repository[msg_code]; *action; action++)
	{
		/* Check available space */
		if (n >= (sizeof(buf) - 1)) break;

		/* Are we parsing a quantity modifier? */
		if (flag)
		{
			/* Check the presence of the modifier's terminator */
			if (*action == ']')
			{
				/* Go back to parsing regular text */
				flag = 0;

				/* Skip the mark */
				continue;
			}

			/* Check if we have to parse the plural modifier */
			if (*action == '|')
			{
				/* Switch to plural modifier */
				flag = PLURAL_MON;

				/* Skip the mark */
				continue;
			}

			/* Ignore the character if we need the other part */
			if ((flag == PLURAL_MON) != do_plural) continue;
		}

		/* Do we need to parse a new quantity modifier? */
		else if (*action == '[')
		{
			/* Switch to singular modifier */
			flag = SINGULAR_MON;

			/* Skip the mark */
			continue;
		}

		/* Append the character to the buffer */
		buf[n++] = *action;
	}

	/* Terminate the buffer */
	buf[n] = '\0';

	/* Done */
	return (buf);
}


/*
 * Plays a sound for some messages of the given race
 */
static void play_mon_msg_sound(u16b r_idx, byte msg_code)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Death/destruction */
	if ((msg_code == MON_MSG_DIE) || (msg_code == MON_MSG_DESTROYED))
	{
		/* Assume normal death sound */
		int soundfx = MSG_KILL;

		/* Play a special sound if the monster was unique */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			/* Mega-Hack -- Morgoth -- see monster_death() */
			if (r_ptr->flags1 & RF1_DROP_CHOSEN)
				soundfx = MSG_KILL_KING;
			else
				soundfx = MSG_KILL_UNIQUE;
		}

		/* Play the sound */
		sound(soundfx);
	}
	/* Monster is fleeing in terror */
	else if (msg_code == MON_MSG_FLEE_IN_TERROR)
	{
		/* Play the sound */
		sound(MSG_FLEE);
	}
}

/*
 * Tracks which monster has had which pain message stored, so redundant messages
 * don't happen due to monster attacks hitting other monsters.
 * Returns TRUE if the message is redundant.
 */
static bool redundant_monster_message(int m_idx, int msg_code)
{
	int i;

	/* No messages yet */
	if (!size_mon_hist) return FALSE;

	for (i = 0; i < size_mon_hist; i++)
	{
		/* Not the same monster */
		if (m_idx != mon_message_hist[i].monster_idx) continue;

		/* Not the same code */
		if (msg_code != mon_message_hist[i].message_code) continue;

		/* We have a match. */
		return (TRUE);
	}

	return (FALSE);
}



/*
 * Stack a codified message for the given monster race. You must supply
 * the description of some monster of this race. You can also supply
 * different monster descriptions for the same race.
 * Return TRUE on success.
 */
bool add_monster_message(const char *mon_name, int m_idx, int msg_code)
{
	int i;
	byte mon_flags = 0;

	monster_type *m_ptr = &mon_list[m_idx];
	int r_idx = m_ptr->r_idx;

	if (redundant_monster_message(m_idx, msg_code)) return (FALSE);

	/* Paranoia */
	if (!mon_name || !mon_name[0]) mon_name = "it";

	/* Monster is invisible or out of LOS */
	if (streq(mon_name, "it") || streq(mon_name, "something"))
	{
		/* Special mark */
		r_idx = 0;
	}

	/* Save the "hidden" mark, if present */
	if (strstr(mon_name, "(hidden)")) mon_flags |= 0x01;

	/* Save the "offscreen" mark, if present */
	if (strstr(mon_name, "(offscreen)")) mon_flags |= 0x02;

	/* Query if the message is already stored */
	for (i = 0; i < size_mon_msg; i++)
	{
		/* We found the race and the message code */
		if ((mon_msg[i].mon_race == r_idx) &&
			(mon_msg[i].mon_flags == mon_flags) &&
			(mon_msg[i].msg_code == msg_code))
		{
			/* Can we increment the counter? */
			if (mon_msg[i].mon_count < MAX_UCHAR)
			{
				/* Stack the message */
				++(mon_msg[i].mon_count);
			}

			/* Success */
			return (TRUE);
		}
	}

	/* The message isn't stored. Check free space */
	if (size_mon_msg >= MAX_STORED_MON_MSG) return (FALSE);

	/* Assign the message data to the free slot */
	mon_msg[i].mon_race = r_idx;
	mon_msg[i].mon_flags = mon_flags;
	mon_msg[i].msg_code = msg_code;
	/* Just this monster so far */
	mon_msg[i].mon_count = 1;

	/* One more entry */
	++size_mon_msg;

	p_ptr->notice |= PN_MON_MESSAGE;

	/* record which monster had this message stored */
	if (size_mon_hist >= MAX_STORED_MON_CODES) return (TRUE);
	mon_message_hist[size_mon_hist].monster_idx = m_idx;
	mon_message_hist[size_mon_hist].message_code = msg_code;
	size_mon_hist++;

	/* Success */
	return (TRUE);
}


/*
 * Show and delete the stacked monster messages.
 */
void flush_monster_messages(void)
{
	int i;
	int r_idx;
	int count;
	monster_race *r_ptr;
	char buf[512];
	char *action;
	bool action_only;

	/* We use either ascii or system-specific encoding */
	int encoding = (xchars_to_file) ? SYSTEM_SPECIFIC : ASCII;

	/* Show every message */
	for (i = 0; i < size_mon_msg; i++)
	{
		/* Cache the monster count */
		count = mon_msg[i].mon_count;

		/* Paranoia */
		if (count < 1) continue;

		/* Start with an empty string */
		buf[0] = '\0';

		/* Cache the race index */
		r_idx = mon_msg[i].mon_race;

		/* Is it a regular race? */
		if (r_idx > 0)
		{
			/* Get the race */
			r_ptr = &r_info[r_idx];
		}
		/* It's the special mark for non-visible monsters */
		else
		{
			/* No race */
			r_ptr = NULL;
		}

		/* Get the proper message action */
		action = get_mon_msg_action(mon_msg[i].msg_code, (count > 1));

		/* Special message? */
		action_only = (*action == '~');

		/* Format the proper message for visible monsters */
		if (r_ptr && !action_only)
		{
			char race_name[80];

			/* Get the race name */
			my_strcpy(race_name, r_ptr->name_full, sizeof(buf));

			/* Special case. Player ghosts */
			if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
			{
				/* Format the ghost name along with the race name */
				/*
				 * Note that we can use the ghost name even if the ghost
				 * was already destroyed
				 */
				strnfmt(buf, sizeof(buf), player_ghost_name);
			}
			/* Uniques */
			else if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				/* Just copy the race name */
				my_strcpy(buf, r_ptr->name_full, sizeof(buf));
			}
			/* We have more than one monster */
			else if (count > 1)
			{
				/* Get the plural of the race name */
				plural_aux(race_name, sizeof(race_name));

				/* Put the count and the race name together */
				strnfmt(buf, sizeof(buf), "%d %s", count, race_name);
			}
			/* Normal lonely monsters */
			else
			{
				/* Just add a slight flavor */
				strnfmt(buf, sizeof(buf), "the %s", race_name);
			}

		}
		/* Format the message for non-viewable monsters if necessary */
		else if (!r_ptr && !action_only)
		{
			if (count > 1)
			{
				/* Show the counter */
				strnfmt(buf, sizeof(buf), "%d monsters", count);
			}
			else
			{
				/* Just one non-visible monster */
				my_strcpy(buf, "it", sizeof(buf));
			}
		}

		/* Special message. Nuke the mark */
		if (action_only)
		{
			++action;
		}
		/* Regular message */
		else
		{
			/* Add special mark. Hidden monster */
			if (mon_msg[i].mon_flags & 0x01) my_strcat(buf, " (hidden)", sizeof(buf));

			/* Add special mark. Monster is offscreen */
			if (mon_msg[i].mon_flags & 0x02) my_strcat(buf, " (offscreen)", sizeof(buf));

			/* Add the separator */
		    my_strcat(buf, " ", sizeof(buf));
		}

		/* Append the action to the message */
		my_strcat(buf, action, sizeof(buf));

		/* Translate to accented characters */
		/* Translate the note to the desired encoding */
		xstr_trans(buf, encoding);

		/* Capitalize the message */
		*buf = my_toupper((unsigned char)*buf);

		/* Play a sound for certain messages */
		play_mon_msg_sound(r_idx, mon_msg[i].msg_code);

		/* Show the message */
		msg_print(buf);
	}

	/* Delete all the stacked messages and history */
	size_mon_msg = 0;
	size_mon_hist = 0;
}


/*
 * Monster learns about an "observed" resistance.
 *
 * The LRN_xxx const indicates the type of resistance to be
 * investigated.
 *
 * SM_xxx flags are set appropriately.
 *
 * -DRS-, -BR-
 */
 void update_smart_learn(int m_idx, int what)
 {
 	monster_type *m_ptr = &mon_list[m_idx];

 	monster_race *r_ptr = &r_info[m_ptr->r_idx];

 	/* Too stupid to learn anything */
 	if (r_ptr->flags2 & (RF2_STUPID)) return;

 	/* Not intelligent, only learn sometimes */
 	if (!(r_ptr->flags2 & (RF2_SMART)) && (rand_int(100) < 50)) return;

 	/* XXX XXX XXX */

 	/* Analyze the knowledge */
 	switch (what)
 	{

		/* Slow/paralyze attacks learn about free action and saving throws */
		case LRN_FREE_SAVE:
 		{
			if (p_ptr->state.skills[SKILL_SAVE] >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->state.skills[SKILL_SAVE] >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
 			if (p_ptr->state.free_act) m_ptr->smart |= (SM_IMM_FREE);
			else m_ptr->smart &= ~(SM_IMM_FREE);
 			break;
 		}

		/* Mana attacks learn if you have any mana to attack */
		case LRN_MANA:
 		{
 			if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
			else m_ptr->smart &= ~(SM_IMM_MANA);
 			break;
 		}

		/* Acid attacks learn about Acid resists and immunities */
		case LRN_ACID:
 		{
 			if (p_ptr->state.resist_acid) m_ptr->smart |= (SM_RES_ACID);
			else m_ptr->smart &= ~(SM_RES_ACID);
 			if (p_ptr->timed[TMD_OPP_ACID]) m_ptr->smart |= (SM_OPP_ACID);
			else m_ptr->smart &= ~(SM_OPP_ACID);
 			if (p_ptr->state.immune_acid) m_ptr->smart |= (SM_IMM_ACID);
			else m_ptr->smart &= ~(SM_IMM_ACID);
 			break;
 		}

		/* Electircal attacks learn about Electrical resists and immunities */
		case LRN_ELEC:
 		{
 			if (p_ptr->state.resist_elec) m_ptr->smart |= (SM_RES_ELEC);
			else m_ptr->smart &= ~(SM_RES_ELEC);
 			if (p_ptr->timed[TMD_OPP_ELEC]) m_ptr->smart |= (SM_OPP_ELEC);
			else m_ptr->smart &= ~(SM_OPP_ELEC);
 			if (p_ptr->state.immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
			else m_ptr->smart &= ~(SM_IMM_ELEC);
 			break;
 		}

		/* Fire attacks learn about Fire resists and immunities */
		case LRN_FIRE:
 		{
 			if (p_ptr->state.resist_fire) m_ptr->smart |= (SM_RES_FIRE);
			else m_ptr->smart &= ~(SM_RES_FIRE);
 			if (p_ptr->timed[TMD_OPP_FIRE]) m_ptr->smart |= (SM_OPP_FIRE);
			else m_ptr->smart &= ~(SM_OPP_FIRE);
 			if (p_ptr->state.immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
			else m_ptr->smart &= ~(SM_IMM_FIRE);
 			break;
 		}

		/* Cold attacks learn about Cold resists and immunities */
		case LRN_COLD:
 		{
 			if (p_ptr->state.resist_cold) m_ptr->smart |= (SM_RES_COLD);
			else m_ptr->smart &= ~(SM_RES_COLD);
			if (p_ptr->timed[TMD_OPP_COLD]) m_ptr->smart |= (SM_OPP_COLD);
			else m_ptr->smart &= ~(SM_OPP_COLD);
 			if (p_ptr->state.immune_cold) m_ptr->smart |= (SM_IMM_COLD);
			else m_ptr->smart &= ~(SM_IMM_COLD);
 			break;
 		}

		/* Poison attacks learn about Poison resists */
		case LRN_POIS:
 		{
 			if (p_ptr->state.resist_pois) m_ptr->smart |= (SM_RES_POIS);
			else m_ptr->smart &= ~(SM_RES_POIS);
 			if (p_ptr->timed[TMD_OPP_POIS]) m_ptr->smart |= (SM_OPP_POIS);
			else m_ptr->smart &= ~(SM_OPP_POIS);
			if (p_ptr->state.immune_pois) m_ptr->smart |= (SM_IMM_POIS);
			else m_ptr->smart &= ~(SM_IMM_POIS);
 			break;
 		}

		/* Fear attacks learn about resist fear and saving throws */
		case LRN_FEAR_SAVE:
 		{
			if (p_ptr->state.skills[SKILL_SAVE] >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->state.skills[SKILL_SAVE] >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
 			if (p_ptr->state.resist_fear) m_ptr->smart |= (SM_RES_FEAR);
			else m_ptr->smart &= ~(SM_RES_FEAR);
 			break;
 		}

		/* Light attacks learn about light and blindness resistance */
		case LRN_LIGHT:
 		{
 			if (p_ptr->state.resist_light) m_ptr->smart |= (SM_RES_LIGHT);
			else m_ptr->smart &= ~(SM_RES_LIGHT);
			if (p_ptr->state.resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			else m_ptr->smart &= ~(SM_RES_BLIND);
 			break;
 		}

		/* Darkness attacks learn about dark and blindness resistance */
		case LRN_DARK:
 		{
 			if (p_ptr->state.resist_dark) m_ptr->smart |= (SM_RES_DARK);
			else m_ptr->smart &= ~(SM_RES_DARK);
			if (p_ptr->state.resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			else m_ptr->smart &= ~(SM_RES_BLIND);
 			break;
 		}

		/*
		 * Some Blindness attacks learn about blindness resistance
		 * Others (below) do more
		 */
		case LRN_BLIND:
 		{
 			if (p_ptr->state.resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			else m_ptr->smart &= ~(SM_RES_BLIND);
 			break;
 		}

		/*
		 * Some Confusion attacks learn about confusion resistance
		 * Others (below) do more
		 */
		case LRN_CONFU:
 		{
 			if (p_ptr->state.resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
 			break;
 		}

		/*
		 * Some sound attacks learn about sound and confusion resistance, and saving throws
		 * Others (below) do less.
		 */
		case LRN_SOUND:
 		{
 			if (p_ptr->state.resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
 			break;
 		}

		/* Shards attacks learn about shards resistance */
		case LRN_SHARD:
 		{
			if (p_ptr->state.resist_shard) m_ptr->smart |= (SM_RES_SHARD);
			else m_ptr->smart &= ~(SM_RES_SHARD);
 			break;
 		}

		/*
		 *  Some Nexus attacks learn about Nexus resistance only
		 *  Others (below) do more
		 */
		case LRN_NEXUS:
 		{
 			if (p_ptr->state.resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
			else m_ptr->smart &= ~(SM_RES_NEXUS);
 			break;
 		}

		/* Nether attacks learn about Nether resistance */
		case LRN_NETHR:
 		{
 			if (p_ptr->state.resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
			else m_ptr->smart &= ~(SM_RES_NETHR);
 			break;
 		}

		/* Chaos attacks learn about Chaos, Confusion and Nether resistance */
		case LRN_CHAOS:
 		{
 			if (p_ptr->state.resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
			else m_ptr->smart &= ~(SM_RES_CHAOS);
			if (p_ptr->state.resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
			else m_ptr->smart &= ~(SM_RES_NETHR);
			if (p_ptr->state.resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
 			break;
 		}

		/* Disenchantment attacks learn about disenchantment resistance */
		case LRN_DISEN:
 		{
 			if (p_ptr->state.resist_disen) m_ptr->smart |= (SM_RES_DISEN);
			else m_ptr->smart &= ~(SM_RES_DISEN);
 			break;
 		}

		/* Some attacks learn only about saving throws (cause wounds, etc) */
		case LRN_SAVE:
		{
			if (p_ptr->state.skills[SKILL_SAVE] >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->state.skills[SKILL_SAVE] >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			break;
		}

		/* Archery attacks don't learn anything */
		case LRN_ARCH:
		{
			break;
		}

		/* Poison archery attacks learn about poison resists */
		case LRN_PARCH:
		{
			if (p_ptr->state.resist_pois) m_ptr->smart |= (SM_RES_POIS);
			else m_ptr->smart &= ~(SM_RES_POIS);
			if (p_ptr->timed[TMD_OPP_POIS]) m_ptr->smart |= (SM_OPP_POIS);
			else m_ptr->smart &= ~(SM_OPP_POIS);
			if (p_ptr->state.immune_pois) m_ptr->smart |= (SM_IMM_POIS);
			else m_ptr->smart &= ~(SM_IMM_POIS);
			break;
		}

		/* Ice attacks learn aboyt sound/shards/cold resists and cold immunity */
		case LRN_ICE:
		{
			if (p_ptr->state.resist_cold) m_ptr->smart |= (SM_RES_COLD);
			else m_ptr->smart &= ~(SM_RES_COLD);
			if (p_ptr->timed[TMD_OPP_COLD]) m_ptr->smart |= (SM_OPP_COLD);
			else m_ptr->smart &= ~(SM_OPP_COLD);
			if (p_ptr->state.immune_cold) m_ptr->smart |= (SM_IMM_COLD);
			else m_ptr->smart &= ~(SM_IMM_COLD);
			if (p_ptr->state.resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			if (p_ptr->state.resist_shard) m_ptr->smart |= (SM_RES_SHARD);
			break;
		}

		/* Plasma attacks learn about sound */
		case LRN_PLAS:
		{
			if (p_ptr->state.resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			break;
		}

		/* GF_LAVA attacks learn about Lava nativity */
		case LRN_LAVA:
		{
		 	if (p_ptr->p_native & (P_NATIVE_LAVA)) m_ptr->smart |= (SM_NAT_LAVA);
			else m_ptr->smart &= ~(SM_NAT_LAVA);
		 	break;
		 }

		/*
		 * Some sounds attacks learna about sound resistance only
		 * Others (above) do more
		 */
		case LRN_SOUND2:
		{
			if (p_ptr->state.resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			break;
		}

		/* Water and storm attacks learn about sound/confusion resists */
		case LRN_STORM:
		case LRN_WATER:
		{
			if (p_ptr->state.resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			if (p_ptr->state.resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
			break;
		}

		/*
		 * Some nexus attacks learn about Nexus resist and saving throws
		 * Others (above) do more
		 */
		case LRN_NEXUS_SAVE:
		{
			if (p_ptr->state.skills[SKILL_SAVE] >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->state.skills[SKILL_SAVE] >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->state.resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
			break;
		}

		/*
		 * Some Blindness attacks learn about blindness resistance and saving throws
		 * Others (above) do less
		 */
		case LRN_BLIND_SAVE:
		{
			if (p_ptr->state.skills[SKILL_SAVE] >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->state.skills[SKILL_SAVE] >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->state.resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			break;
		}

		/*
		 * Some Confusion attacks learn about confusion resistance and saving throws
		 * Others (above) do less
		 */
		case LRN_CONFU_SAVE:
		{
			if (p_ptr->state.skills[SKILL_SAVE] >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->state.skills[SKILL_SAVE] >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->state.resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
			break;
		}
	}
}
