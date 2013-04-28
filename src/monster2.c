/* File: monster2.c */

/*
 * Polymorph, delete and compact monsters.  Wipe the monster list, get a
 * new monster index, rebuild the monster allocation table and draw a new
 * monster from it.  Get the name of a monster.  Update monster visibility
 * and distance, actually move them into a grid, let them pick up objects.
 * Place a monster in the dungeon, build packs and escorts, generate
 * monsters randomly.  Summon monsters.  Handle frightened, doomed, and
 * multiplying monsters.  Monster pain messages.  Update a monster's
 * knowledge of the character.  Hurt and kill monsters, handle monster
 * death and drops.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



/*
 * Return another race for a monster to polymorph into.
 *
 * Perform a modified version of "get_mon_num()", with exact minimum and
 * maximum depths and preferred monster types.
 */
s16b poly_r_idx(int base_idx)
{
	monster_race *r_ptr = &r_info[base_idx];

	alloc_entry *table = alloc_race_table;

	int i, min_lev, max_lev, r_idx;
	long value;

	int q_idx = q_info[quest_num(p_ptr->depth)].r_idx;

	/* Source monster's level and symbol */
	int r_lev = r_ptr->level;
	char d_char = r_ptr->d_char;


	/* Hack -- Uniques and quest monsters never polymorph */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (base_idx == q_idx))
	{
		return (base_idx);
	}

	/* Allowable level of new monster */
	min_lev = (MAX(        1, r_lev - 1 - r_lev / 5));
	max_lev = (MIN(MAX_DEPTH, r_lev + 1 + r_lev / 5));

	/* Reset sum of final monster probabilities. */
	alloc_race_total = 0L;

	/* Hack -- illegal monster generation level (forces rebuild) */
	old_monster_level = -1;

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
		if ((q_idx) && (r_idx == q_idx)) continue;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/* Hack -- No uniques */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Forced-depth monsters only appear at their level. */
		if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) &&
		    (r_ptr->level != p_ptr->depth)) continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Bias against monsters far from initial monster's depth */
		if (table[i].level < (min_lev + r_lev) / 2) table[i].prob3 /= 4;
		if (table[i].level > (max_lev + r_lev) / 2) table[i].prob3 /= 4;
		if (table[i].level < min_lev)               table[i].prob3 /= 4;
		if (table[i].level > max_lev)               table[i].prob3 /= 4;


		/* Bias against monsters not of the same symbol */
		if (r_ptr->d_char != d_char) table[i].prob3 /= 4;

		/* Sum up probabilities */
		alloc_race_total += table[i].prob3;
	}

	/* No legal monsters */
	if (alloc_race_total == 0)
	{
		return (base_idx);
	}


	/* Pick a monster */
	value = rand_int(alloc_race_total);

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
	int x, y;

	monster_type *m_ptr = &m_list[i];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	s16b this_o_idx, next_o_idx = 0;


	/* Get location */
	y = m_ptr->fy;
	x = m_ptr->fx;


	/* Hack -- Reduce the racial counter */
	r_ptr->cur_num--;

	/* Hack -- remove target monster */
	if (p_ptr->target_who == i) target_set_monster(0);

	/* Hack -- remove tracked monster */
	if (p_ptr->health_who == i) health_track(0);

	/* Update monster list window (later) */
	p_ptr->window |= (PW_M_LIST);


	/* Monster is gone */
	cave_m_idx[y][x] = 0;

	/*
	 * Total Hack -- If the monster was a player ghost, remove it from
	 * the monster memory, ensure that it never appears again, and clear
	 * its bones file selector.
	 */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{
		l_ptr->sights = 0;
		l_ptr->pkills = 1;
		l_ptr->tkills = 0;
		bones_selector = 0;
	}


	/* Delete objects */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Delete the object */
		delete_object_idx(this_o_idx);
	}


	/* Wipe the Monster */
	WIPE(m_ptr, monster_type);

	/* Count monsters */
	m_cnt--;


	/* Visual update */
	lite_spot(y, x);
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
 * Move a monster from index i1 to index i2 in the monster list
 */
static void compact_monsters_aux(int i1, int i2)
{
	int y, x;

	monster_type *m_ptr;

	s16b this_o_idx, next_o_idx = 0;


	/* Do nothing */
	if (i1 == i2) return;


	/* Old monster */
	m_ptr = &m_list[i1];

	/* Location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Update the cave */
	cave_m_idx[y][x] = i2;

	/* Repair objects being carried by monster */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get object */
		o_ptr = &o_list[this_o_idx];

		/* Get next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Adjust held-by index */
		o_ptr->held_m_idx = i2;
	}

	/* Hack -- Update the target */
	if (p_ptr->target_who == i1) p_ptr->target_who = i2;

	/* Hack -- Update the health bar */
	if (p_ptr->health_who == i1) p_ptr->health_who = i2;

	/* Hack -- move monster */
	COPY(&m_list[i2], &m_list[i1], monster_type);

	/* Hack -- Wipe hole */
	WIPE(&m_list[i1], monster_type);
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

	/* Compact */
	if (size)
	{
		/* Get quest monster index (if any) */
		int q_idx = q_info[quest_num(p_ptr->depth)].r_idx;

		s16b *mon_lev;
		s16b *mon_index;

		/* Allocate the "mon_lev and mon_index" arrays */
		C_MAKE(mon_lev, m_max, s16b);
		C_MAKE(mon_index, m_max, s16b);


		/* Message */
		msg_print("Compacting monsters...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);


		/* Scan the monster list */
		for (i = 1; i < m_max; i++)
		{
			m_ptr = &m_list[i];
			r_ptr = &r_info[m_ptr->r_idx];

			/* Dead monsters have minimal level (but are counted!) */
			if (!m_ptr->r_idx) mon_lev[i] = -1L;

			/* Get the monster level */
			else
			{
				mon_lev[i] = r_ptr->level;

				/* Quest monsters always get compacted last */
				if (r_ptr->flags1 & (RF1_QUESTOR)) mon_lev[i] += MAX_DEPTH * 3;
				else if ((q_idx) && (m_ptr->r_idx == q_idx))
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
		for (i = 0; i < m_max - 1; i++)
		{
			for (j = 0; j < m_max - 1; j++)
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
		for (cnt = 0, i = 0; i < m_max; i++)
		{
			/* We've deleted enough monsters */
			if (cnt >= size) break;

			/* Get this monster, using our saved index */
			m_ptr = &m_list[mon_index[i]];

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
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Get the i'th monster */
		m_ptr = &m_list[i];

		/* Skip real monsters */
		if (m_ptr->r_idx) continue;

		/* Move last monster into open hole */
		compact_monsters_aux(m_max - 1, i);

		/* Compress "m_max" */
		m_max--;
	}
}


/*
 * Delete/Remove all the monsters when the player leaves the level
 *
 * This is an efficient method of simulating multiple calls to the
 * "delete_monster()" function, with no visual effects.
 */
void wipe_m_list(void)
{
	int i;

	/* Delete all the monsters */
	for (i = m_max - 1; i >= 1; i--)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];


		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Total Hack -- Clear player ghost information. */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			l_ptr->sights = 0;
			l_ptr->pkills = 1;
			l_ptr->tkills = 0;
			bones_selector = 0;
		}

		/* Hack -- Reduce the racial counter */
		r_ptr->cur_num--;

		/* Monster is gone */
		cave_m_idx[m_ptr->fy][m_ptr->fx] = 0;

		/* Wipe the Monster */
		WIPE(m_ptr, monster_type);
	}

	/* Reset "m_max" */
	m_max = 1;

	/* Reset "m_cnt" */
	m_cnt = 0;

	/* Hack -- no more target */
	target_set_monster(0);

	/* Hack -- no more tracking */
	health_track(0);

	/* Hack -- make sure there is no player ghost */
	bones_selector = 0;
}


/*
 * Acquires and returns the index of a "free" monster.
 *
 * This routine should almost never fail, but it *can* happen.
 */
s16b m_pop(void)
{
	int i;

	/* Normal allocation */
	if (m_max < z_info->m_max)
	{
		/* Get the next hole */
		i = m_max;

		/* Expand the array */
		m_max++;

		/* Count monsters */
		m_cnt++;

		/* Return the index */
		return (i);
	}


	/* Recycle dead monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr;

		/* Get monster */
		m_ptr = &m_list[i];

		/* Skip live monsters */
		if (m_ptr->r_idx) continue;

		/* Count monsters */
		m_cnt++;

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

	/* Hack -- rebuild the final monster generation probability table */
	(void)get_mon_num(0);

	/* Success */
	return (0);
}



/*
 * Choose a monster race that seems "appropriate" to the given level
 *
 * We use this function not only to pick a monster but to build a
 * table of probabilities.  This table can be used again and again, if
 * certain conditions (generation level being the most important) don't
 * change.
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
 * There is a small chance (1/25) of "boosting" the given depth by
 * a small amount, except in the town.
 *
 * Low-level monsters avoid the deep dungeon.
 *
 * Note that if no monsters are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 */
s16b get_mon_num(int level)
{
	int i;

	int r_idx;

	long value;
	int failure = 0;
	int temp_level = level;
	bool quick = FALSE;

	monster_race *r_ptr;

	alloc_entry *table = alloc_race_table;


	/* Low-level monsters avoid the deep dungeon. */
	int depth_rare = 2 * level / 3;
	int depth_none = level / 3;


	/* Calculate a luck factor (this can get very nasty!) */
	int nasty_mon_chance = NASTY_MON -
		(p_ptr->luck >= 100 ? 0 : (100 - p_ptr->luck) / 4);

	/* Never get too absurd */
	if (nasty_mon_chance < 3) nasty_mon_chance = 3;


	/* Sometimes, monsters in the dungeon can be out of depth */
	if (p_ptr->depth != 0)
	{
		/* Occasional boost to maximum level */
		if (one_in_(nasty_mon_chance))
		{
			/* Boost the level  (from 1 to 5) */
			temp_level += MIN(5, div_round(level, 10) + 1);

			/* Occasional second boost */
			if (one_in_(nasty_mon_chance))
			{
				/* Boost the level  (from 1 to 5) */
				temp_level += MIN(5, div_round(level, 10) + 1);
			}
		}
	}

	/* We are using the same generation level as last time */
	if ((temp_level == old_monster_level) && (p_ptr->depth))
	{
		/* We are using the same generation restrictions as last time */
		if (get_mon_num_hook == old_get_mon_num_hook)
		{
			/* There is no need to rebuild the generation table */
			if (alloc_race_total) quick = TRUE;
		}
	}


	/* We are not using quick generation */
	if (!quick)
	{
		/* Remember the generation level we are using */
		old_monster_level = temp_level;

		/* Remember the restrictions we are using */
		old_get_mon_num_hook = get_mon_num_hook;


		/* Try hard to find a suitable monster */
		while (TRUE)
		{
			/* Reset sum of final monster probabilities. */
			alloc_race_total = 0L;

			/* Process probabilities */
			for (i = 0; i < alloc_race_size; i++)
			{
				/* Assume no probability */
				table[i].prob3 = 0;

				/* Ignore illegal monsters */
				if (!table[i].prob2) continue;

				/* Monsters are sorted by depth */
				if (table[i].level > temp_level) continue;

				/* Hack -- No town monsters in dungeon */
				if ((p_ptr->depth != 0) && (table[i].level <= 0)) continue;

				/* Paranoia -- No dungeon monsters in town */
				if ((p_ptr->depth == 0) && (table[i].level > 0)) continue;

				/* Get the monster index */
				r_idx = table[i].index;

				/* Get the actual race */
				r_ptr = &r_info[r_idx];

				/* Hack -- some monsters are unique */
				if ((r_ptr->flags1 & (RF1_UNIQUE)) &&
					 (r_ptr->cur_num >= r_ptr->max_num)) continue;

				/* Forced-depth monsters only appear at their level. */
				if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) &&
					 (r_ptr->level != p_ptr->depth)) continue;

				/* We know how much space is available */
				if (monster_space)
				{
					/* Monster has many friends or escorts - require a lot of space */
					if ((r_ptr->flags1 & (RF1_FRIENDS)) ||
						 (r_ptr->flags1 & (RF1_ESCORTS)))
					{
						if (monster_space < SPACE_LOTS) continue;
					}

					/* Monster has a few friends or escorts - require some space */
					else if ((r_ptr->flags1 & (RF1_FRIEND)) ||
								(r_ptr->flags1 & (RF1_ESCORT)))
					{
						if (monster_space < SPACE_SOME) continue;
					}

					/* Hack -- Quylthulgs need summon room */
					else if (r_ptr->d_char == 'Q')
					{
						if (monster_space < SPACE_LOTS) continue;
					}
				}

				/* Accept */
				table[i].prob3 = table[i].prob2;

				/* Keep low-level monsters rare */
				if (table[i].level < depth_rare) table[i].prob3 /= 4;
				if (table[i].level < depth_none) table[i].prob3 = 0;

				/* Sum up probabilities */
				alloc_race_total += table[i].prob3;
			}

			/* No legal monsters */
			if (alloc_race_total == 0L)
			{
				failure++;

				if (failure <= 4)
				{
					/* Try relaxing the level restrictions */
					temp_level += 3;
				}
				else
				{
					/* Our monster restrictions are too stringent. */
					return (0);
				}
			}

			/* Success */
			else break;
		}
	}

	/* Pick a monster */
	value = rand_int(alloc_race_total);

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
 *
 * Useful Modes:
 *   0x00 --> Full nominative name ("the kobold") or "it"
 *   0x04 --> Full nominative name ("the kobold") or "something"
 *   0x80 --> Genocide resistance name ("the kobold")
 *   0x88 --> Killing name ("a kobold")
 *   0x22 --> Possessive, genderized if visible ("his", "her", "its")
 *            We do not currently use "hers" anywhere.
 *   0x23 --> Reflexive, genderized if visible ("himself") or "itself"
 *   0x31 --> "him", "her", "it" for any monster
 */
void monster_desc(char *desc, const monster_type *m_ptr, int mode)
{
	cptr res;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	cptr name = (r_name + r_ptr->name);
	char racial_name[40] = "oops";

	bool seen, pron;

	/* Can we "see" it (forced, or not hidden + visible) */
	seen = ((mode & (0x80)) || (!(mode & (0x40)) &&
	        (m_ptr->ml) && (!(m_ptr->mflag & (MFLAG_DLIM)))));

	/* Sexed Pronouns (seen and forced, or unseen and allowed) */
	pron = ((seen && (mode & (0x20))) || (!seen && (mode & (0x10))));


	/* First, try using pronouns, or describing hidden monsters */
	if (!seen || pron)
	{
		/* An encoding of the monster "sex" */
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
		strcpy(desc, res);
	}


	/* Handle visible monsters, "reflexive" request */
	else if ((mode & 0x02) && (mode & 0x01))
	{
		/* The monster is visible, so use its gender */
		if (r_ptr->flags1 & (RF1_FEMALE)) strcpy(desc, "herself");
		else if (r_ptr->flags1 & (RF1_MALE)) strcpy(desc, "himself");
		else strcpy(desc, "itself");
	}


	/* Handle all other visible monster requests */
	else
	{
		/* It could be a player ghost. */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			/* Get the ghost name. */
			strcpy(desc, ghost_name);

			/* Get the racial name. */
			strcpy(racial_name, r_name + r_ptr->name);

			/* Build the ghost name. */
			strcat(desc, ", the ");
			strcat(desc, racial_name);
		}

		/* It could be a Unique */
		else if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Start with the name (thus nominative and objective) */
			strcpy(desc, name);
		}

		/* It could be an indefinite monster */
		else if (mode & 0x08)
		{
			/* XXX Check plurality for "some" */

			/* Indefinite monsters need an indefinite article */
			strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ");
			strcat(desc, name);
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			strcpy(desc, "the ");
			strcat(desc, name);
		}

		/* Handle the Possessive as a special afterthought */
		if (mode & 0x02)
		{
			/* XXX Check for trailing "s" */

			/* Simply append "apostrophe" and "s" */
			strcat(desc, "'s");
		}

		/* Mention "offscreen" monsters XXX XXX */
		if (!(mode & (0x80)) &&
		    ((ABS(m_ptr->fy - p_ptr->wy) >= map_rows) ||
		     (ABS(m_ptr->fx - p_ptr->wx) >= SCREEN_WID)))
		{
			/* Append special notation */
			strcat(desc, " (offscreen)");
		}
	}
}



/*
 * Learn about a monster (by "probing" it)
 *
 * Return the number of new flags learnt.  -Mogami-
 */
int lore_do_probe(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int i;
	int n = 0;

	/* Count unknown flags */
	for (i = 0; i < 32; i++)
	{
		if (!(l_ptr->flags1 & (1L << i)) &&
		     (r_ptr->flags1 & (1L << i))) n++;
		if (!(l_ptr->flags2 & (1L << i)) &&
		     (r_ptr->flags2 & (1L << i))) n++;
		if (!(l_ptr->flags3 & (1L << i)) &&
		     (r_ptr->flags3 & (1L << i))) n++;
	}


	/* Hack -- Memorize some flags */
	l_ptr->flags1 = r_ptr->flags1;
	l_ptr->flags2 = r_ptr->flags2;
	l_ptr->flags3 = r_ptr->flags3;

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}

	/* Return the number of new flags learnt */
	return (n);
}







/*
 * This function updates the visibility and (optionally) the distance to
 * the character of the given monster.
 *
 * It can use a lot of CPU time; efficiency is important.
 *
 * Visibility must be checked when:
 * - a monster or the character moves,
 * - certain character conditions (like blindness) activate or wear off
 * - certain character bonuses (like infravision) change
 * - the character's field of sight (FOS) of field of vision (FOV) change
 * - the dungeon is altered (as with wall to mud or earthquake)
 * - grids gain or lose permanent light
 *
 * Certain ways of seeing a monster -- telepathy, infravision, normal
 * sight -- allow one to see the monster now, but not necessarily see it
 * if it moves.  Other ways, especially those enabled by skills, allow you
 * not only to see the monster but to detect it as it moves about between
 * turns, similar to detection spells.  -LM-
 *
 * Some detection methods are random or pseudo-random.  It is important
 * that these only be checked in specific circumstances:  specifically,
 * that they not be checked when the player presses "control-R" and,
 * ideally, only when the character moves or expends energy.  -LM-
 *
 * Distance is checked (using inline math, not the "distance()" function)
 * whenever this monster moves, or whenever the character moves.
 *
 * Notes:
 * Monsters which are not on the current panel may be visible to
 * the player, and their descriptions will include an "offscreen"
 * reference.
 *
 * The player can choose to be disturbed by several things, including
 * "disturb_move" (monster enters, leaves, or moves about when visible),
 * and "disturb_near" (monster enters or leaves vision)
 */
bool update_mon(int m_idx, bool full, bool complete)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int d;

	/* Current location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;


	/* Quality of old visibility and of new visibility */
	int vision;
	int old_vision;

	/* Quality of lingering detection -- assume none */
	int detection = 0;

	/* Assume not directly seen with vision */
	bool easy = FALSE;

	bool redraw = FALSE;


	/* Compute distance */
	if (full)
	{
		int py = p_ptr->py;
		int px = p_ptr->px;

		/* Distance components */
		int dy = (py > fy) ? (py - fy) : (fy - py);
		int dx = (px > fx) ? (px - fx) : (fx - px);

		/* Approximate distance */
		d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));

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


	/* Determine previous visibility */
	if (m_ptr->ml)
	{
		if (m_ptr->mflag & (MFLAG_DLIM)) old_vision = 1;
		else                             old_vision = 2;
	}
	else
	{
		old_vision = 0;
	}

	/* Determine new visibility */
	vision = 0;

	/* Detected or sensed for this turn */
	if (m_ptr->mflag & (MFLAG_MARK)) vision = detection = 1;
	if (m_ptr->mflag & (MFLAG_FULL)) vision = detection = 2;

	/* Clear display limitations */
	m_ptr->mflag &= ~(MFLAG_DLIM);

	/* Nearby */
	if (d <= MAX_SIGHT)
	{
		/* Basic telepathy */
		if (p_ptr->telepathy)
		{
			/* Empty mind, no telepathy */
			if (r_ptr->flags2 & (RF2_EMPTY_MIND))
			{
				/* Memorize flags */
				l_ptr->flags2 |= (RF2_EMPTY_MIND);
			}

			/* Weird mind, occasional telepathy */
			else if (r_ptr->flags2 & (RF2_WEIRD_MIND))
			{
				/* Monster is rarely detectable (only check sometimes) */
				if ((complete) && ((turn / 10) % 10) == (m_idx % 10))
				{
					/* Detectable */
					vision = MAX(vision, 2);

					/* Memorize flags */
					l_ptr->flags2 |= (RF2_WEIRD_MIND);

					/* Hack -- Memorize mental flags */
					if (r_ptr->flags2 & (RF2_SMART)) l_ptr->flags2 |= (RF2_SMART);
					if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->flags2 |= (RF2_STUPID);
				}
			}

			/* Normal mind, allow telepathy */
			else
			{
				/* Detectable */
				vision = MAX(vision, 2);

				/* Hack -- Memorize mental flags */
				if (r_ptr->flags2 & (RF2_SMART)) l_ptr->flags2 |= (RF2_SMART);
				if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->flags2 |= (RF2_STUPID);
			}
		}

		/* Priestly awareness of evil */
		if (p_ptr->esp_evil)
		{
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				vision = MAX(vision, 2);
			}
		}

		/* Normal line of sight, and not blind */
		if (player_has_los_bold(fy, fx) && !p_ptr->blind)
		{
			bool do_invisible = FALSE;
			bool do_cold_blood = FALSE;

			/* Use "infravision" */
			if (d <= p_ptr->see_infra)
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
					vision = MAX(vision, 2);
					easy = TRUE;
				}
			}

			/* Use "illumination" */
			if (player_can_see_bold(fy, fx))
			{
				/* Handle "invisible" monsters */
				if (r_ptr->flags2 & (RF2_INVISIBLE))
				{
					/* Take note */
					do_invisible = TRUE;

					/* See invisible */
					if ((p_ptr->see_inv) || (p_ptr->detect_inv))
					{
						/* Easy to see */
						vision = MAX(vision, 2);
						easy = TRUE;
					}
				}

				/* Handle "normal" monsters */
				else
				{
					/* Easy to see */
					vision = MAX(vision, 2);
					easy = TRUE;
				}


				/* Visible mimics (but not lurkers) can sometimes be noticed */
				if ((easy) && (complete) && (m_ptr->mflag & (MFLAG_MIME)) &&
				    (r_ptr->d_char != '.'))
				{
					/* Allow awareness (rarely) */
					if ((d < 8) && (one_in_(5)) &&
					    (d < randint(get_skill(S_PERCEPTION, 2, 8))))
					{
						char m_name[80];

						/* Get the monster name ("a kobold") */
						monster_desc(m_name, m_ptr, 0x88);

						/* Notice */
						msg_format("You see %s.", m_name);
						m_ptr->mflag &= ~(MFLAG_MIME);

						/* Update health bar (if not in use) */
						if (!p_ptr->health_who)
						{
							p_ptr->health_who = m_idx;
							p_ptr->redraw |= (PR_HEALTH);
						}
					}
				}
			}

			/* Handle "shining" monsters */
			if (r_ptr->flags2 & (RF2_IS_LIT))
			{
				/* Shining monsters can still be invisible */
				if (!(r_ptr->flags2 & (RF2_INVISIBLE)) ||
				     (p_ptr->see_inv) || (p_ptr->detect_inv))
				{
					/* Note that monster is shining */
					l_ptr->flags2 |= (RF2_IS_LIT);

					/* Shining monsters are harder to see from afar */
					vision = MAX(vision, (d < 10) ? 2 : 1);
				}
			}

			/* Visible */
			if (vision)
			{
				/* Memorize flags */
				if (do_invisible)  l_ptr->flags2 |= (RF2_INVISIBLE);
				if (do_cold_blood) l_ptr->flags2 |= (RF2_COLD_BLOOD);
			}
		}

		/* Handle talent-based monster detection effects */
		if (complete)
		{
			int temp = 0;
			int skill = 0;

			/* Monster is an animal -- use nature skill */
			if (r_ptr->flags3 & (RF3_ANIMAL))
			{
				temp = get_skill(S_NATURE, 0, 100);
				if (temp > skill) skill = temp;
			}

			/* Monster is demonic -- use piety skill */
			if (r_ptr->flags3 & (RF3_DEMON))
			{
				temp = get_skill(S_PIETY, 0, 100);
				if (temp > skill) skill = temp;
			}

			/* Monster is undead -- use blood dominion skill */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				temp = get_skill(S_DOMINION, 0, 100);
				if (temp > skill) skill = temp;
			}

			/* Display the monster according to skill */
			if (skill >= 40)
			{
				/* Use the quick RNG */
				Rand_quick = TRUE;

				/* Seed it with the current turn and monster index  XXX */
				Rand_value = turn + m_idx;

				/* Try for limited visibility (always, with skill >= 70) */
				if ((skill >= 70) || (rand_int(72 - skill) < 2))
				{
					vision = MAX(vision, 1);
					detection = MAX(detection, 1);

					/* Try for full visibility (always, with skill == 100) */
					if ((skill == 100) || (rand_int(102 - skill) < 2))
					{
						vision = MAX(vision, 2);
						detection = MAX(detection, 2);
					}
				}

				/* Do not use the quick RNG */
				Rand_quick = FALSE;
			}
		}

		/* Chance to "hear" unseen monsters near to you  -EZ- */
		if ((complete) && (!vision) && (d <= 1))
		{
			/* Note how much awareness we have  (max of about 40-45) */
			int awareness = rsqrt(get_skill(S_PERCEPTION, 0, 900)) +
				p_ptr->skill_awr;

			/* Use the quick RNG */
			Rand_quick = TRUE;

			/* Seed it with the current turn and monster index  XXX */
			Rand_value = turn + m_idx;

			/* Attempt to hear the monster */
			if (awareness > 5 + r_ptr->level / 5 +
			    rand_int(45 + r_ptr->level / 2))
			{
				/* Limited "sight" */
				vision = MAX(vision, 1);
				detection = MAX(detection, 1);
			}

			/* Do not use the quick RNG */
			Rand_quick = FALSE;
		}
	}


	/* Detect this monster */
	if (detection >= 1) m_ptr->mflag |= (MFLAG_MARK);
	if (detection >= 2) m_ptr->mflag |= (MFLAG_FULL);

	/* Remember limited visibility */
	if (vision == 1) m_ptr->mflag |= (MFLAG_DLIM);


	/* Visibility change */
	if (vision != old_vision)
	{
		/* Reset visibility */
		m_ptr->ml = (vision > 0);

		/* Draw or erase the monster */
		lite_spot(fy, fx);

		/* We redraw the monster */
		redraw = TRUE;

		/* Update health bar as needed */
		if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

		/* Update monster list window */
		p_ptr->window |= (PW_M_LIST);

		/* Hack -- Count "fresh" sightings  XXX */
		if ((mon_fully_visible(m_ptr)) && (l_ptr->sights < MAX_SHORT))
		{
			l_ptr->sights++;
		}

		/* Disturb on visibility change */
		if (disturb_move)
		{
			/* Disturb if monster is not a townsman, or if fairly weak */
			if (!(m_ptr->mflag & (MFLAG_TOWN)) || (p_ptr->power < 10))
			{
				disturb(1, 0);
			}
		}
	}

	/* Hack -- Character "forgets" about (far-away) mimics */
	if ((r_ptr->flags1 & (RF1_CHAR_MIMIC)) && (!m_ptr->ml) &&
	    (d >= MAX_SIGHT + 5))
	{
		/* Mimic hides */
		m_ptr->mflag |= (MFLAG_MIME);
	}

	/* Monster enters or leaves field of sight (eyes, infavision) */
	if ((( easy) && (!(m_ptr->mflag & (MFLAG_VIEW)))) ||
	    ((!easy) &&   (m_ptr->mflag & (MFLAG_VIEW))))
	{
		/* Mark as easily visible */
		if (easy) m_ptr->mflag |= (MFLAG_VIEW);

		/* No longer easily viewable */
		else      m_ptr->mflag &= ~(MFLAG_VIEW);

		/* Disturb on appearance */
		if (disturb_near)
		{
			/* Disturb if monster is not a townsman, or if fairly weak */
			if (!(m_ptr->mflag & (MFLAG_TOWN)) || (p_ptr->power < 10))
			{
				disturb(1, 0);
			}
		}
	}

	/* Return "monster was redrawn" */
	return (redraw);
}


/*
 * This function simply updates all the (non-dead) monsters (see above).
 */
void update_monsters(bool full)
{
	int i;

	/* Update each (live) monster */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Update the monster -- do not handle some kinds of detection */
		(void)update_mon(i, full, FALSE);
	}
}



/*
 * Make a monster carry an object
 */
s16b monster_carry(int m_idx, object_type *j_ptr)
{
	s16b o_idx;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];
	object_type *o_ptr;


	/* Scan objects already being held for combination */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

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
 * Swap the players/monsters (if any) at two locations XXX XXX XXX
 */
void monster_swap(int y1, int x1, int y2, int x2)
{
	int m1, m2;
	int i;

	monster_type *m_ptr;


	/* Monsters */
	m1 = cave_m_idx[y1][x1];
	m2 = cave_m_idx[y2][x2];


	/* Update grids */
	cave_m_idx[y1][x1] = m2;
	cave_m_idx[y2][x2] = m1;


	/* Monster 1 */
	if (m1 > 0)
	{
		m_ptr = &m_list[m1];

		/* Move monster */
		m_ptr->fy = y2;
		m_ptr->fx = x2;

		/* Update monster */
		(void)update_mon(m1, TRUE, FALSE);
	}

	/* Player 1 */
	else if (m1 < 0)
	{
		/* Clear direction of movement */
		p_ptr->move_dir = 0;

		/* If player has moved no more than one grid, save direction */
		if ((ABS(p_ptr->py - y2) <= 1) &&  (ABS(p_ptr->px - x2) <= 1))
		{
			for (i = 1; i < 10; i++)
			{
				if ((ddy[i] == y2 - p_ptr->py) && (ddx[i] == x2 - p_ptr->px))
				{
					p_ptr->move_dir = i;
				}
			}
		}

		/* Move player */
		p_ptr->py = y2;
		p_ptr->px = x2;

		/* Update the panel */
		p_ptr->update |= (PU_PANEL);

		/* Update the visuals (and monster distances) */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
	}

	/* Monster 2 */
	if (m2 > 0)
	{
		m_ptr = &m_list[m2];

		/* Move monster */
		m_ptr->fy = y1;
		m_ptr->fx = x1;

		/* Update monster */
		(void)update_mon(m2, TRUE, FALSE);
	}

	/* Player 2 */
	else if (m2 < 0)
	{
		/* Clear direction of movement */
		p_ptr->move_dir = 0;

		/* If player has moved no more than one grid, save direction */
		if ((ABS(p_ptr->py - y1) <= 1) &&  (ABS(p_ptr->px - x1) <= 1))
		{
			for (i = 1; i < 10; i++)
			{
				if ((ddy[i] == y1 - p_ptr->py) && (ddx[i] == x1 - p_ptr->px))
				{
					p_ptr->move_dir = i;
				}
			}
		}

		/* Move player */
		p_ptr->py = y1;
		p_ptr->px = x1;

		/* Update the panel */
		p_ptr->update |= (PU_PANEL);

		/* Update the visuals (and monster distances) */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
	}

	/* Redraw */
	lite_spot(y1, x1);
	lite_spot(y2, x2);
}


/*
 * Place the player in the dungeon XXX XXX
 */
s16b player_place(int y, int x)
{
	/* Paranoia XXX XXX */
	if (cave_m_idx[y][x] != 0) return (0);


	/* Save player location */
	p_ptr->py = y;
	p_ptr->px = x;

	/* Mark cave grid */
	cave_m_idx[y][x] = -1;

	/* Success */
	return (-1);
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
	m_idx = m_pop();

	/* Oops */
	if (m_idx)
	{
		/* Make a new monster */
		cave_m_idx[y][x] = m_idx;

		/* Get new monster */
		m_ptr = &m_list[m_idx];

		/* Copy the monster XXX */
		COPY(m_ptr, n_ptr, monster_type);

		/* Location */
		m_ptr->fy = y;
		m_ptr->fx = x;

		/* Update the monster */
		(void)update_mon(m_idx, TRUE, FALSE);

		/* Get new race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Count racial occurrences */
		r_ptr->cur_num++;
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
		if (d_dice * d_side > 3) return (FALSE);

		/* Can steal from the character */
		if ((effect == RBE_EAT_GOLD) || (effect == RBE_EAT_ITEM)) return (FALSE);
	}

	/* Harmless */
	return (TRUE);
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
static bool place_monster_one(int y, int x, int r_idx, bool slp)
{
	int i;

	monster_race *r_ptr;
	monster_type *n_ptr;
	monster_type monster_type_body;

	int precog = get_skill(S_PERCEPTION, 0, 100);

	cptr name;

	/* Handle failure of the "get_mon_num()" function */
	if (!r_idx) return (FALSE);

	/* Race */
	r_ptr = &r_info[r_idx];

	/* The monster must be able to exist in this grid */
	if (!cave_exist_mon(r_ptr, y, x, FALSE, FALSE)) return (FALSE);

	/* Paranoia */
	if (!r_ptr->name) return (FALSE);

	/* Name */
	name = (r_name + r_ptr->name);

	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
		return (FALSE);
	}

	/* Hack -- only 1 player ghost at a time */
	if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && bones_selector)
	{
		/* Cannot create */
		return (FALSE);
	}

	/* Forced-depth monsters do not appear out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (p_ptr->depth < r_ptr->level))
	{
		/* Get quest monster index (if any) */
		quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

		/* Cannot create -- unless a quest monster */
		if (r_idx != q_ptr->r_idx) return (FALSE);
	}

	/* Get local monster */
	n_ptr = &monster_type_body;

	/* Clean out the monster */
	WIPE(n_ptr, monster_type);


	/* Save the race */
	n_ptr->r_idx = r_idx;



	/*
	 * If the monster is a player ghost, perform various manipulations
	 * on it, and forbid ghost creation if something goes wrong.
	 */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{
		if (!prepare_ghost(r_idx, n_ptr, FALSE)) return (FALSE);

		name = format("%s, the %s", ghost_name, name);
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
		else if (!calc_spent_exp()) return (FALSE);
	}

	/* Enforce sleeping if needed */
	if (slp && r_ptr->sleep)
	{
		n_ptr->csleep = rand_range((r_ptr->sleep + 1) / 2, r_ptr->sleep);
	}

	/* Assign maximal hitpoints */
	if ((r_ptr->flags1 & (RF1_FIXED_HPS)) || (r_ptr->flags1 & (RF1_UNIQUE)))
	{
		n_ptr->maxhp = r_ptr->hitpoints;
	}
	else
	{
		/* Hitpoints usually vary from 75% to 125% of normal */
		s16b spread = div_round(r_ptr->hitpoints, 8);
		s16b tmp = Rand_normal(r_ptr->hitpoints, spread);

		/* Verify hitpoints */
		if (tmp < 1) tmp = 1;

		/* Assign hitpoints */
		n_ptr->maxhp = (u16b)tmp;
	}


	/* Mark minimum range for recalculation */
	n_ptr->min_range = 0;

	/* Monsters like to use harassment spells at first  XXX XXX */
	if (r_ptr->level > 20) n_ptr->harass = BASE_HARASS;

	/* Weaker monsters don't live long enough to spend forever harassing */
	else n_ptr->harass = LOW_HARASS;


	/* Initialize mana */
	n_ptr->mana = r_ptr->mana;

	/* And start out fully healthy */
	n_ptr->hp = n_ptr->maxhp;

	/* Extract the monster base speed */
	n_ptr->mspeed = r_ptr->speed;

	/* Hack -- small racial variety */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		/* Allow some small variation per monster */
		i = div_round(extract_energy[r_ptr->speed], 10);
		if (i) n_ptr->mspeed += rand_spread(0, i);
	}

	/* Force some monsters to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/*
		 * Give almost no starting energy
		 * (randomizing avoids clumped movement for monster groups)
		 */
		n_ptr->energy = rand_int(10);
	}
	else
	{
		/* Give a random starting energy */
		n_ptr->energy = rand_int(50);
	}

	/* Mimics start out hidden */
	if (r_ptr->flags1 & (RF1_CHAR_MIMIC))
	{
		n_ptr->mflag |= (MFLAG_MIME);
	}

	/* Place the monster in the dungeon */
	if (!monster_place(y, x, n_ptr)) return (FALSE);


	/* Unique monster */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique is out of depth */
		if (r_ptr->level > p_ptr->depth)
		{
			/* Message - precognition */
			if (can_precog(50, LEV_REQ_PRECOG))
			{
				if ((r_ptr->level > p_ptr->depth + 5) &&
				    (can_precog(80, LEV_REQ_PRECOG + 30)))
				{
					precog_msg("You feel the presence of something very dangerous.");
				}
				else if (can_precog(100, LEV_REQ_PRECOG + 10))
					precog_msg("A chill runs through your body.");
				else
					precog_msg("You feel somewhat uneasy.");
			}

			/* Message - cheat */
			if (cheat_hear) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			level_rating += (r_ptr->level - p_ptr->depth) * 2;
		}

		/* Unique is not out of depth */
		else
		{
			if (can_precog(67, LEV_REQ_PRECOG + 5))
			{
				if (can_precog(100, LEV_REQ_PRECOG + 20))
					precog_msg("You feel a momentary chill.");
				else
					precog_msg("You feel somewhat uneasy.");
			}

			/* Message - cheat */
			if (cheat_hear) msg_format("Unique (%s).", name);
		}
	}

	/* Deep normal monsters */
	else if (r_ptr->level > p_ptr->depth)
	{
		/* Message - cheat */
		if (cheat_hear) msg_format("Deep Monster (%s).", name);

		/* Boost rating by half delta-depth */
		level_rating += (r_ptr->level - p_ptr->depth) / 2;

		/* Message - precognition */
		if (((r_ptr->level - p_ptr->depth) >= 10) &&
		    (can_precog(60, LEV_REQ_PRECOG + 30)))
		{
			precog_msg("You feel extremely uneasy.");
		}

		else if (((r_ptr->level - p_ptr->depth) >= 5) &&
		         (can_precog(60, LEV_REQ_PRECOG + 20)))
		{
			precog_msg("You feel rather uneasy.");
		}

		else if (((r_ptr->level - p_ptr->depth) >= 2) &&
		         (can_precog(50, LEV_REQ_PRECOG + 10)))
		{
			precog_msg("You feel somewhat uneasy.");
		}
	}


	/* Sometimes generate spurious precognition messages */
	if ((precog >= LEV_REQ_PRECOG + 10) && (precog < 93) &&
	    (one_in_(precog - LEV_REQ_PRECOG)))
	{
		/* Frequency depends on depth */
		if (p_ptr->depth > rand_int(500))
		{
			/* Generic message */
			precog_msg("You feel somewhat uneasy.");
		}
	}

	/* Success */
	return (TRUE);
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
static bool place_monster_group(int y, int x, int r_idx, bool slp,
	s16b group_size)
{
	monster_race *r_ptr = &r_info[r_idx];

	int old, n, i;
	int start;
	int reduce;

	int hack_n = 0;

	byte hack_y[GROUP_MAX];
	byte hack_x[GROUP_MAX];

	/* Hard monsters, smaller groups */
	if (r_ptr->level > p_ptr->depth)
	{
		reduce = (r_ptr->level - p_ptr->depth) / 2;
		group_size -= randint(reduce);
	}

	/* Minimum size */
	if (group_size < 1) group_size = 1;

	/* Maximum size */
	if (group_size > GROUP_MAX) group_size = GROUP_MAX;


	/* Save the rating */
	old = level_rating;

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
	level_rating = old;


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
	int escort_size, escort_idx;
	int n, i;

	/* Random direction */
	int start;

	monster_race *r_ptr = &r_info[leader_idx];

	int level = r_ptr->level;

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
	escort_idx = get_mon_num(monster_level);

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
			escort_idx = get_mon_num(level);
		}
	}

	/* Return to previous monster restrictions (usually none) */
	get_mon_num_hook = get_mon_num_hook_temp;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* XXX - rebuild monster table */
	(void)get_mon_num(monster_level);
}

/*
 * Attempt to place a monster of the given race at the given location
 *
 * Monsters may have some friends, or lots of friends.  They may also
 * have a few escorts, or lots of escorts.
 *
 * Note the use of the new "monster allocation table" code to restrict
 * the "get_mon_num()" function to legal escort types.
 */
bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Place one monster, or fail */
	if (!place_monster_one(y, x, r_idx, slp)) return (FALSE);

	/* Require the "group" flag */
	if (!grp) return (TRUE);


	/* Friends for certain monsters */
	if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		/* Attempt to place a large group */
		(void)place_monster_group(y, x, r_idx, slp, (s16b)rand_range(6, 10));
	}

	else if (r_ptr->flags1 & (RF1_FRIEND))
	{
		/* Attempt to place a small group */
		(void)place_monster_group(y, x, r_idx, slp, (s16b)(rand_range(2, 3)));
	}

	/* Escorts for certain monsters */
	if ((r_ptr->flags1 & (RF1_ESCORT)) || (r_ptr->flags1 & (RF1_ESCORTS)))
	{
		place_monster_escort(y, x, r_idx, slp);
	}

	/* Success */
	return (TRUE);
}


/*
 * Scan around the given grid, seeing how much space is available to place
 * monsters that don't pass through walls.
 */
static void check_monster_space(int y, int x)
{
	int n, i;
	int hack_n = 0;

	byte hack_y[8];
	byte hack_x[8];

	/* Start on the given grid - assume it is available */
	hack_n = 1;
	hack_x[0] = x;
	hack_y[0] = y;


	/* Spread outwards, looking for good grids */
	for (n = 0; (n < hack_n) && (hack_n < 7); n++)
	{
		/* Grab the location */
		int hx = hack_x[n];
		int hy = hack_y[n];

		/* Check each direction */
		for (i = 0; (i < 8) && (hack_n < 7); i++)
		{
			int mx = hx + ddx_ddd[i];
			int my = hy + ddy_ddd[i];

			/* Grid must not already have been checked */
			if (cave_info[my][mx] & (CAVE_TEMP)) continue;

			/* Grid must be passable by all monsters */
			if (!cave_passable_bold(y, x)) continue;

			/* Grid must be unoccupied */
			if (cave_m_idx[y][x] != 0) continue;

			/* Add grid to the "hack" set */
			hack_y[hack_n] = my;
			hack_x[hack_n] = mx;
			hack_n++;

			/* We've now checked this grid */
			cave_info[my][mx] |= (CAVE_TEMP);
		}
	}

	/* Clear CAVE_TEMP flags */
	for (i = 0; i < hack_n; i++)
		cave_info[hack_y[i]][hack_x[i]] &= ~(CAVE_TEMP);


	/* Only the (assumed) start grid is available */
	if (hack_n == 1) monster_space = SPACE_ONE;

	/* Four or fewer grids are available */
	else if (hack_n <= 4) monster_space = SPACE_SOME;

	/* Five or more grids are available */
	else monster_space = SPACE_LOTS;
}

/*
 * Hack -- attempt to place a monster at the given location.
 *
 * Attempt to find a monster appropriate to the "monster_level".
 *
 * This function, unlike "alloc_monster()" and some others, tries to make
 * sure that group monsters and monsters with escorts have enough room to
 * place their companions.
 */
bool place_monster(int y, int x, bool slp, bool grp)
{
	int r_idx;


	/* Confirm that grid is not occupied */
	if (cave_m_idx[y][x]) return (FALSE);

	/* Check space, apply restrictions to monster generation */
	check_monster_space(y, x);

	/* Pick a monster */
	r_idx = get_mon_num(monster_level);

	/* Forget knowledge of space available */
	monster_space = SPACE_UNKNOWN;

	/* Handle failure */
	if (!r_idx) return (FALSE);


	/* Attempt to place the monster */
	if (place_monster_aux(y, x, r_idx, slp, grp)) return (TRUE);

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
bool alloc_monster(int dis, bool slp)
{
	monster_race *r_ptr;

	int r_idx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;


	/* Pick a monster */
	r_idx = get_mon_num(monster_level);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Get the monster */
	r_ptr = &r_info[r_idx];


	/* Find a legal, distant, unoccupied, space */
	while (TRUE)
	{
		/* Pick a location */
		y = rand_int(dungeon_hgt);
		x = rand_int(dungeon_wid);

		/* Require a grid that the monster can exist in. */
		if (!cave_exist_mon(r_ptr, y, x, FALSE, FALSE)) continue;

		/* Do not put random monsters in marked rooms. */
		if ((!character_dungeon) && (cave_info[y][x] & (CAVE_TEMP)))
			continue;

		/* Accept only far away grids */
		if ((dis == 0) || (distance(y, x, py, px) > dis)) break;
	}

	/* Attempt to place the monster (use sleep value, allow groups) */
	if (place_monster_aux(y, x, r_idx, slp, TRUE)) return (TRUE);

	/* Nope */
	return (FALSE);
}




/*
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;

/*
 * Hack -- the maximum summon level
 */
static int summon_level;


/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	int i;

	bool okay = FALSE;


	/* Player ghosts cannot be summoned. */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

	/* Hack -- no specific type specified */
	if (!summon_specific_type) return (TRUE);

	/* Check our requirements */
	switch (summon_specific_type)
	{
		case SUMMON_KIN:
		{
			okay = ((r_ptr->d_char == summon_kin_type) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_BEETLE:
		{
			okay = ((r_ptr->d_char == 'K') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

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

		case SUMMON_ANIMAL:
		{
			okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_ORC:
		{
			okay = ((r_ptr->d_char == 'o') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_DRAGON:
		{
			okay = ((r_ptr->flags3 & (RF3_DRAGON)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HI_DRAGON:
		{
			okay = (r_ptr->d_char == 'D');
			break;
		}

		case SUMMON_DEMON:
		{
			okay = ((r_ptr->flags3 & (RF3_DEMON)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HI_DEMON:
		{
			okay = (r_ptr->d_char == '&');
			break;
		}

		case SUMMON_UNDEAD:
		{
			okay = ((r_ptr->flags3 & (RF3_UNDEAD)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HI_UNDEAD:
		{
			okay = ((r_ptr->d_char == 'L') ||
				(r_ptr->d_char == 'V') ||
				(r_ptr->d_char == 'W'));
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

		case SUMMON_ELEMENTAL:
		{
			okay = (r_ptr->d_char == 'E');
			break;
		}

		case SUMMON_VORTEX:
		{
			okay = (r_ptr->d_char == 'v');
			break;
		}

		case SUMMON_HYBRID:
		{
			okay = (r_ptr->d_char == 'H');
			break;
		}

		case SUMMON_BIRD:
		{
			okay = (r_ptr->d_char == 'B');
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

		case SUMMON_BERTBILLTOM:
		{
			okay = ((r_ptr->d_char == 'T') &&
				(r_ptr->flags1 & (RF1_UNIQUE)) &&
				  ((strstr((r_name + r_ptr->name), "Bert")) ||
				   (strstr((r_name + r_ptr->name), "Bill")) ||
				   (strstr((r_name + r_ptr->name), "Tom" ))));
			break;
		}

		case SUMMON_ANGEL:
		{
			okay = ((r_ptr->d_char == 'A') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_INDEX:
		{
			okay = (r_idx == summon_index_type);
			break;
		}
	}

	/* Result */
	return (okay);
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * We usually do not summon monsters greater than the given depth.  -LM-
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "get_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 */
bool summon_specific(int y1, int x1, bool scattered, int lev, int type)
{
	int i, x, y, d, r_idx;

	/* Allow groups in most cases */
	bool grp = ((type == SUMMON_ORC && rand_int(5)) ? FALSE : TRUE);

	bool spot_found = FALSE;


	/* Paranoia -- requre a legal grid */
	if (!in_bounds_fully(y1, x1)) return (FALSE);

	/* Look for a location */
	for (i = 0; i < (scattered ? 40 : 20); ++i)
	{
		/* Pick a distance */
		if (type == SUMMON_ORC) d = 10;
		else if (scattered) d = rand_range(2, 6);
		else d = (i / 10) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require passable terrain, with no other creature or player. */
		if (!cave_passable_bold(y, x)) continue;
		if (cave_m_idx[y][x] != 0) continue;

		/* Hack -- no summon on glyph of warding */
		if (cave_glyph(y, x)) continue;

		/* Success */
		spot_found = TRUE;

		/* Okay */
		break;
	}

	/* Failure */
	if (!spot_found) return (FALSE);


	/* Save the "summon" type */
	summon_specific_type = type;

	/* Require "okay" monsters */
	get_mon_num_hook = summon_specific_okay;

	/* Limit summon level */
	summon_level = lev;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster.  Usually do not exceed maximum level. */
	r_idx = get_mon_num(summon_level);


	/* Clear monster restrictions */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Hack -- illegal monster generation level (forces rebuild) */
	old_monster_level = -1;


	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, usually allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, grp)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Change monster fear.
 *
 * Monsters can be frightened or panicking.  In both cases, they try to
 * retreat, but when actually panicking, they cannot cast spells that don't
 * either heal or move them.
 */
void set_mon_fear(monster_type *m_ptr, int v, bool panic)
{
	/* Set monfear */
	m_ptr->monfear = v;

	/* Monster is panicking */
	if ((m_ptr->monfear) && (panic)) m_ptr->min_range = PANIC_RANGE;

	/* Otherwise, reset monster combat ranges (later) */
	else m_ptr->min_range = 0;
}


/*
 * Doom all monsters of a given type.
 */
static void monsters_doomed(int r_idx)
{
	int i;

	/* Scan the monster list */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Monster is of the given type -- it is DOOMED */
		if (m_ptr->r_idx == r_idx) m_ptr->mflag |= (MFLAG_DOOM);
	}
}


/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
bool multiply_monster(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

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
		if (cave_exist_mon(r_ptr, y, x, FALSE, FALSE))
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

	/* Create a new monster (awake, no groups) */
	result = place_monster_aux(y, x, m_ptr->r_idx, FALSE, FALSE);

	/* Hack -- if too many mana flies or chaos tiles breed, they become DOOMED */
	if (((m_ptr->r_idx == MON_MANA_FLY) ||
	     (m_ptr->r_idx == MON_CHAOS_TILE)) &&
	    ((r_ptr->cur_num > 45 + p_ptr->depth) ||
	     (m_cnt > z_info->m_max - 50)))
	{
		monsters_doomed(m_ptr->r_idx);
	}

	/* Result */
	return (result);
}



/*
 * Dump a message describing a monster's reaction to damage
 *
 * Extra detail by -EB-
 *
 * We should have different messages for non-visible monsters.  XXX XXX
 */
void message_pain(int m_idx, int dam)
{
	int percentage;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char m_name[80];


	/* Get the monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Notice non-damage */
	if (dam == 0)
	{
		msg_format("%^s is unharmed.", m_name);
		return;
	}

	/* Calculate damage percentage (perfect rounding) */
	percentage = div_round(100 * m_ptr->hp, m_ptr->hp + dam);

	/* Floating Eyes, Jellies, Molds, Vortexes, Quylthulgs */
	if (strchr("ejmvQ", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s barely notices.", m_name);
		else if (percentage > 75)
			msg_format("%^s flinches.", m_name);
		else if (percentage > 50)
			msg_format("%^s squelches.", m_name);
		else if (percentage > 35)
			msg_format("%^s quivers in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s writhes about.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s jerks limply.", m_name);
	}

	/* Dogs and Hounds */
	else if (strchr("CZ", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s snarls with pain.", m_name);
		else if (percentage > 50)
			msg_format("%^s yelps in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s howls in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s howls in agony.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s whimpers feebly.", m_name);
	}

	/* Snakes, Reptiles, Centipedes, Mimics */
	else if (strchr("cJR", r_ptr->d_char) ||
	         r_ptr->flags1 & (RF1_CHAR_MIMIC))
	{
		if (percentage > 95)
			msg_format("%^s barely notices.", m_name);
		else if (percentage > 75)
			msg_format("%^s hisses.", m_name);
		else if (percentage > 50)
			msg_format("%^s rears up in anger.", m_name);
		else if (percentage > 35)
			msg_format("%^s hisses furiously.", m_name);
		else if (percentage > 20)
			msg_format("%^s writhes about.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s jerks limply.", m_name);
	}

	/* Felines */
	else if (strchr("f", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s snarls.", m_name);
		else if (percentage > 50)
			msg_format("%^s growls angrily.", m_name);
		else if (percentage > 35)
			msg_format("%^s hisses with pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s mewls in pain.", m_name);
		else if (percentage > 10)
			msg_format("%^s hisses in agony.", m_name);
		else
			msg_format("%^s mewls pitifully.", m_name);
	}

	/* Ants, Lice, Flies, Insects, Beetles, Spiders */
	else if (strchr("alFIKS", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s drones angrily.", m_name);
		else if (percentage > 50)
			msg_format("%^s scuttles about.", m_name);
		else if (percentage > 35)
			msg_format("%^s twitches in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s jerks in pain.", m_name);
		else if (percentage > 10)
			msg_format("%^s jerks in agony.", m_name);
		else
			msg_format("%^s jerks feebly.", m_name);
	}

	/* Birds */
	else if (strchr("B", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s flaps angrily.", m_name);
		else if (percentage > 50)
			msg_format("%^s jeers in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s squawks with pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s twitters in agony.", m_name);
		else if (percentage > 10)
			msg_format("%^s flutters about.", m_name);
		else
			msg_format("%^s chirps feebly.", m_name);
	}

	/* Skeletons (ignore, rattle, stagger) */
	else if (strchr("s", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s jerks.", m_name);
		else if (percentage > 50)
			msg_format("%^s rattles.", m_name);
		else if (percentage > 35)
			msg_format("%^s clatters.", m_name);
		else if (percentage > 20)
			msg_format("%^s shakes.", m_name);
		else if (percentage > 10)
			msg_format("%^s staggers.", m_name);
		else
			msg_format("%^s crumples.", m_name);
	}

	/* Zombies and Mummies (ignore, groan, stagger) */
	else if (strchr("zM", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s grunts.", m_name);
		else if (percentage > 50)
			msg_format("%^s jerks.", m_name);
		else if (percentage > 35)
			msg_format("%^s moans.", m_name);
		else if (percentage > 20)
			msg_format("%^s groans.", m_name);
		else if (percentage > 10)
			msg_format("%^s hesitates.", m_name);
		else
			msg_format("%^s staggers.", m_name);
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (strchr("Xbqr", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s grunts with pain.", m_name);
		else if (percentage > 50)
			msg_format("%^s squeals in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s shrieks in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s shrieks in agony.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s cries out feebly.", m_name);
	}

	/* Another type of monsters (shrug,cry,scream) */
	else
	{
		if (percentage > 95)
			msg_format("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s grunts with pain.", m_name);
		else if (percentage > 50)
			msg_format("%^s cries out in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s screams in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s screams in agony.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s cries out feebly.", m_name);
	}
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
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Too stupid to learn anything */
	if (r_ptr->flags2 & (RF2_STUPID)) return;

	/* Not intelligent, only learn sometimes */
	if (!(r_ptr->flags2 & (RF2_SMART)) && (one_in_(2))) return;

	/* XXX XXX XXX */

	/* Analyze the knowledge */
	switch (what)
	{

		/* Slow/paralyze attacks learn about free action and saving throws */
		case LRN_FREE_SAVE:
		{
			if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
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
			if (p_ptr->resist_acid) m_ptr->smart |= (SM_RES_ACID);
			else m_ptr->smart &= ~(SM_RES_ACID);
			if (p_ptr->oppose_acid) m_ptr->smart |= (SM_OPP_ACID);
			else m_ptr->smart &= ~(SM_OPP_ACID);
			if (p_ptr->immune_acid) m_ptr->smart |= (SM_IMM_ACID);
			else m_ptr->smart &= ~(SM_IMM_ACID);
			break;
		}

		/* Electrical attacks learn about Electrical resists and immunities */
		case LRN_ELEC:
		{
			if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
			else m_ptr->smart &= ~(SM_RES_ELEC);
			if (p_ptr->oppose_elec) m_ptr->smart |= (SM_OPP_ELEC);
			else m_ptr->smart &= ~(SM_OPP_ELEC);
			if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
			else m_ptr->smart &= ~(SM_IMM_ELEC);
			break;
		}

		/* Fire attacks learn about Fire resists and immunities */
		case LRN_FIRE:
		{
			if (p_ptr->resist_fire) m_ptr->smart |= (SM_RES_FIRE);
			else m_ptr->smart &= ~(SM_RES_FIRE);
			if (p_ptr->oppose_fire) m_ptr->smart |= (SM_OPP_FIRE);
			else m_ptr->smart &= ~(SM_OPP_FIRE);
			if (p_ptr->immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
			else m_ptr->smart &= ~(SM_IMM_FIRE);
			break;
		}

		/* Cold attacks learn about Cold resists and immunities */
		case LRN_COLD:
		{
			if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
			else m_ptr->smart &= ~(SM_RES_COLD);
			if (p_ptr->oppose_cold) m_ptr->smart |= (SM_OPP_COLD);
			else m_ptr->smart &= ~(SM_OPP_COLD);
			if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);
			else m_ptr->smart &= ~(SM_IMM_COLD);
			break;
		}

		/* Poison attacks learn about Poison resists */
		case LRN_POIS:
		{
			if (p_ptr->resist_pois) m_ptr->smart |= (SM_RES_POIS);
			else m_ptr->smart &= ~(SM_RES_POIS);
			if (p_ptr->oppose_pois) m_ptr->smart |= (SM_OPP_POIS);
			else m_ptr->smart &= ~(SM_OPP_POIS);
			break;
		}

		/* Fear attacks learn about resist fear and saving throws */
		case LRN_FEAR_SAVE:
		{
			if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->resist_fear) m_ptr->smart |= (SM_RES_FEAR);
			else m_ptr->smart &= ~(SM_RES_FEAR);
			break;
		}

		/* Light attacks learn about light and blindness resistance */
		case LRN_LITE:
		{
			if (p_ptr->resist_lite) m_ptr->smart |= (SM_RES_LITE);
			else m_ptr->smart &= ~(SM_RES_LITE);
			break;
		}

		/* Darkness attacks learn about dark and blindness resistance */
		case LRN_DARK:
		{
			if (p_ptr->resist_dark) m_ptr->smart |= (SM_RES_DARK);
			else m_ptr->smart &= ~(SM_RES_DARK);
			break;
		}

		/*
		 * Some Blindness attacks learn about blindness resistance
		 * Others (below) do more
		 */
		case LRN_BLIND:
		{
			if (p_ptr->resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			else m_ptr->smart &= ~(SM_RES_BLIND);
			break;
		}

		/*
		 * Some Confusion attacks learn about confusion resistance
		 * Others (below) do more
		 */
		case LRN_CONFU:
		{
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
			break;
		}

		/*
		 * Some sound attacks learn about sound and confusion resistance, and saving throws
		 * Others (below) do less.
		 */
		case LRN_SOUND:
		{
			if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
			if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			break;
		}

		/* Shards attacks learn about shards resistance */
		case LRN_SHARD:
		{
			if (p_ptr->resist_shard) m_ptr->smart |= (SM_RES_SHARD);
			else m_ptr->smart &= ~(SM_RES_SHARD);
			break;
		}

		/*
		 *  Some Nexus attacks learn about Nexus resistance only
		 *  Others (below) do more
		 */
		case LRN_NEXUS:
		{
			if (p_ptr->resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
			else m_ptr->smart &= ~(SM_RES_NEXUS);
			break;
		}

		/* Nether attacks learn about Nether resistance */
		case LRN_NETHR:
		{
			if (p_ptr->resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
			else m_ptr->smart &= ~(SM_RES_NETHR);
			break;
		}

		/* Chaos attacks learn about Chaos resistance */
		case LRN_CHAOS:
		{
			if (p_ptr->resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
			else m_ptr->smart &= ~(SM_RES_CHAOS);
			break;
		}

		/* Disenchantment attacks learn about disenchantment resistance */
		case LRN_DISEN:
		{
			if (p_ptr->resist_disen) m_ptr->smart |= (SM_RES_DISEN);
			else m_ptr->smart &= ~(SM_RES_DISEN);
			break;
		}

		/* Some attacks learn only about saving throws (cause wounds, etc) */
		case LRN_SAVE:
		{
			if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
		}

		/* Archery attacks don't learn anything */
		case LRN_ARCH:
		{
			break;
		}

		/* Poison archery attacks learn about poison resists */
		case LRN_PARCH:
		{
			if (p_ptr->resist_pois) m_ptr->smart |= (SM_RES_POIS);
			else m_ptr->smart &= ~(SM_RES_POIS);
			if (p_ptr->oppose_pois) m_ptr->smart |= (SM_OPP_POIS);
			else m_ptr->smart &= ~(SM_OPP_POIS);
			break;
		}

		/* Ice attacks learn about sound/shards/cold resists and cold immunity */
		case LRN_ICE:
		{
			if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
			else m_ptr->smart &= ~(SM_RES_COLD);
			if (p_ptr->oppose_cold) m_ptr->smart |= (SM_OPP_COLD);
			else m_ptr->smart &= ~(SM_OPP_COLD);
			if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);
			else m_ptr->smart &= ~(SM_IMM_COLD);
			if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			if (p_ptr->resist_shard) m_ptr->smart |= (SM_RES_SHARD);
			break;
		}

		/* Plasma attacks learn about fire/lightning resists/immunities */
		case LRN_PLAS:
		{
			if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
			else m_ptr->smart &= ~(SM_RES_ELEC);
			if (p_ptr->oppose_elec) m_ptr->smart |= (SM_OPP_ELEC);
			else m_ptr->smart &= ~(SM_OPP_ELEC);
			if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
			else m_ptr->smart &= ~(SM_IMM_ELEC);
			if (p_ptr->resist_fire) m_ptr->smart |= (SM_RES_FIRE);
			else m_ptr->smart &= ~(SM_RES_FIRE);
			if (p_ptr->oppose_fire) m_ptr->smart |= (SM_OPP_FIRE);
			else m_ptr->smart &= ~(SM_OPP_FIRE);
			if (p_ptr->immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
			else m_ptr->smart &= ~(SM_IMM_FIRE);
			break;
		}

		/*
		 * Some sounds attacks learn about sound resistance only
		 * Others (above) do more
		 */
		case LRN_SOUND2:
		{
			if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			break;
		}

		/*
		 * Storm attacks learn about Electrical/Cold/Acid resists/immunities,
		 * and about confusion resist
		 */
		case LRN_STORM:
		{
			if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
			else m_ptr->smart &= ~(SM_RES_ELEC);
			if (p_ptr->oppose_elec) m_ptr->smart |= (SM_OPP_ELEC);
			else m_ptr->smart &= ~(SM_OPP_ELEC);
			if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
			else m_ptr->smart &= ~(SM_IMM_ELEC);
			if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
			else m_ptr->smart &= ~(SM_RES_COLD);
			if (p_ptr->oppose_cold) m_ptr->smart |= (SM_OPP_COLD);
			else m_ptr->smart &= ~(SM_OPP_COLD);
			if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);
			else m_ptr->smart &= ~(SM_IMM_COLD);
			if (p_ptr->resist_acid) m_ptr->smart |= (SM_RES_ACID);
			else m_ptr->smart &= ~(SM_RES_ACID);
			if (p_ptr->oppose_acid) m_ptr->smart |= (SM_OPP_ACID);
			else m_ptr->smart &= ~(SM_OPP_ACID);
			if (p_ptr->immune_acid) m_ptr->smart |= (SM_IMM_ACID);
			else m_ptr->smart &= ~(SM_IMM_ACID);
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
		}

		/* Water attacks learn about sound/confusion resists */
		case LRN_WATER:
		{
			if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
		}

		/*
		 * Some nexus attacks learn about Nexus resist and saving throws
		 * Others (above) do more
		 */
		case LRN_NEXUS_SAVE:
		{
			if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
			break;
		}

		/*
		 * Some Blindness attacks learn about blindness resistance and saving throws
		 * Others (above) do less
		 */
		case LRN_BLIND_SAVE:
		{
			if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			break;
		}

		/*
		 * Some Confusion attacks learn about confusion resistance and saving throws
		 * Others (above) do less
		 */
		case LRN_CONFU_SAVE:
		{
			if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
			break;
		}

		/* Note darkness resistance and saving throw */
		case LRN_DARK_SAVE:
		{
			if (p_ptr->resist_dark) m_ptr->smart |= (SM_RES_DARK);
			else m_ptr->smart &= ~(SM_RES_DARK);

			if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			break;
		}

		default: break;
	}
}


/*
 * Special death effects for monsters.
 *
 * Code adopted from Zangband.
 *
 * Be very careful here:  It is quite easy to smash the stack with
 * multiple explosions, set up endless loops, etc.  Note the use of
 * a special monster race immunity.
 */
void mon_death_effect(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	object_type *i_ptr;
	object_type object_type_body;

	s16b unique_quark = 0;

	int i;


	/* Get the monster's location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;


	/* Monsters of the same race are immune */
	project_immune = m_ptr->r_idx;

	/* Hack -- inscribe items that a unique drops  -clefs- */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		unique_quark = quark_add(r_name + r_ptr->name);
	}


	/* Animated torch */
	if (m_ptr->r_idx == MON_ANIM_TORCH)
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Make a torch */
		object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));

		/* Apply magic */
		apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE);

		/* Drop the torch */
		drop_near(i_ptr, -1, fy, fx);
	}

	/* Mana Fly */
	else if (m_ptr->r_idx == MON_MANA_FLY)
	{
		/* Explode */
		(void)mon_explode(m_idx, 1, fy, fx, 20, GF_MANA);

		/* Cute message */
		if (player_has_los_bold(fy, fx)) msg_print("Pop!");
	}

	/* Unmaker */
	else if (m_ptr->r_idx == MON_UNMAKER)
	{
		/* Explode */
		(void)mon_explode(m_idx, 3, fy, fx, 80, GF_CHAOS);
	}

	/* Undead Mystic */
	else if (m_ptr->r_idx == MON_UNDEAD_MYSTIC)
	{
		/* Kill one, and another usually pops up */
		if (!one_in_(4))
		{
			/* Summon a new monster */
			summon_index_type = MON_UNDEAD_MYSTIC;
			if (summon_specific(fy, fx, FALSE, MAX_DEPTH, SUMMON_INDEX))
			{
				/* Message  XXX XXX */
				if (player_can_see_bold(fy, fx))
					msg_print("The corpse gets back up!");
			}
		}
	}

	/* Tyrant of Hell */
	else if (m_ptr->r_idx == MON_HELL_TYRANT)
	{
		/* Killing a Tyrant of Hell frightens the demons nearby */
		p_ptr->proj_mon_flags = (RF3_DEMON);
		fear_monsters(150);
	}

	/* Living tornado */
	else if (m_ptr->r_idx == MON_TORNADO)
	{
		/* Require junk */
		required_tval = TV_JUNK;

		/* Drop lots of junk */
		for (i = 0; i < randint(7); i++)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Make an object and drop it */
			if (make_object(i_ptr, FALSE, FALSE, FALSE))
			{
				drop_near(i_ptr, -1, fy, fx);
			}
		}

		required_tval = 0;
	}

	/* Morgoth, Lord of Darkness */
	else if (m_ptr->r_idx == MON_MORGOTH)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Mega-Hack -- Prepare to make "Grond" */
		object_prep(i_ptr, lookup_kind(TV_HAFTED, SV_GROND));

		/* Mega-Hack -- Mark this item as "Grond" */
		i_ptr->artifact_index = ART_GROND;

		/* Mega-Hack -- Actually create "Grond" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);

		/* Add note */
		if (unique_quark) i_ptr->note = unique_quark;

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, fy, fx);

		/* Get local object */
		i_ptr = &object_type_body;

		/* Mega-Hack -- Prepare to make "Morgoth" */
		object_prep(i_ptr, lookup_kind(TV_CROWN, SV_MORGOTH));

		/* Mega-Hack -- Mark this item as "Morgoth" */
		i_ptr->artifact_index = ART_MORGOTH;

		/* Mega-Hack -- Actually create "Morgoth" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);

		/* Add note */
		if (unique_quark) i_ptr->note = unique_quark;

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, fy, fx);
	}

	/* Error */
	else
	{
		char m_name[80];

		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Message */
		msg_format("%^s is marked as having a special death effect, but no code exists for it.", m_name);
	}
}

/*
 * Take note that the given monster just dropped some treasure
 *
 * Note that learning the "GOOD"/"GREAT" flags gives information
 * about the treasure (even when the monster is killed for the first
 * time, such as uniques, and the treasure has not been examined yet).
 *
 * This "indirect" method is used to prevent the player from learning
 * exactly how much treasure a monster can drop from observing only
 * a single example of a drop.  This method actually observes how much
 * gold and items are dropped, and remembers that information to be
 * described later by the monster recall code.
 */
static void lore_treasure(int m_idx, int num_item, int num_gold)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	/* Note the number of things dropped */
	if (num_item > l_ptr->drop_item) l_ptr->drop_item = num_item;
	if (num_gold > l_ptr->drop_gold) l_ptr->drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (r_ptr->flags1 & (RF1_DROP_GOOD)) l_ptr->flags1 |= (RF1_DROP_GOOD);
	if (r_ptr->flags1 & (RF1_DROP_GREAT)) l_ptr->flags1 |= (RF1_DROP_GREAT);
	if (r_ptr->flags1 & (RF1_DROP_CHEST)) l_ptr->flags1 |= (RF1_DROP_CHEST);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}
}


/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * Note the use of actual "monster names". XXX XXX XXX
 */
static int get_coin_type(const monster_race *r_ptr)
{
	cptr name = (r_name + r_ptr->name);

	/* Analyze various kinds of monsters */
	if (strchr("$g+f", r_ptr->d_char))
	{
		/* Look for textual clues */
		if (strstr(name, " copper "))       return (SV_COPPER);
		if (strstr(name, " silver "))       return (SV_SILVER);
		if (strstr(name, " gold "))         return (SV_GOLD);
		if (strstr(name, " mithril "))      return (SV_MITHRIL);
		if (strstr(name, " adamantite "))   return (SV_ADAMANTITE);

		/* Look for textual clues */
		if (strstr(name, "Copper "))        return (SV_COPPER);
		if (strstr(name, "Silver "))        return (SV_SILVER);
		if (strstr(name, "Gold "))          return (SV_GOLD);
		if (strstr(name, "Mithril "))       return (SV_MITHRIL);
		if (strstr(name, "Adamantite "))    return (SV_ADAMANTITE);

		/* Look for textual clues */
		if (strstr(name, "Giant Sapphire")) return (SV_SAPPHIRE);
		if (strstr(name, "Giant Emerald"))  return (SV_EMERALD);
		if (strstr(name, "Giant Ruby"))     return (SV_RUBY);
		if (strstr(name, "Giant Diamond"))  return (SV_DIAMOND);

		if (strstr(name, "Aetheroi"))       return (SV_AETHEROI);
	}

	/* Assume nothing */
	return (0);
}

/*
 * Place some down stairs after the player has completed a quest.
 *
 * This is still not a particularly clever function, but at least it's
 * not brain-dead.
 */
static void drop_quest_stairs(int y0, int x0)
{
	u16b grid[100];
	int grids;

	int i, j, y, x;


	/* Try several times to place the stairs */
	for (i = 0; i < 3; i++)
	{
		grids = 0;

		/* Search nearby grids */
		for (j = 0; j < grids_in_radius[3]; j++)
		{
			y = y0 + nearby_grids_y[j];
			x = x0 + nearby_grids_x[j];

			/* Must be fully in bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Must be in LOS from the monster's position */
			if (!los(y0, x0, y, x)) continue;

			/* First try -- Accept non-wall grids with no objects */
			if ((i == 0) && (cave_floor_bold(y, x)) &&
			    (cave_o_idx[y][x] == 0))
			{
				grid[grids++] = GRID(y, x);
			}
			/* Second try -- Accept any passable grid with no objects */
			if ((i == 1) && (cave_passable_bold(y, x)) &&
			    (cave_o_idx[y][x] == 0))
			{
				grid[grids++] = GRID(y, x);
			}
			/* Third try -- Accept any grid that can be "destroyed" */
			if ((i == 2) && (cave_valid_bold(y, x)))
			{
				grid[grids++] = GRID(y, x);
			}
		}

		/* At least one grid is available */
		if (grids) break;
	}

	/* If there are still no grids available, we get desperate */
	if (!grids)
	{
		while (TRUE)
		{
			y = rand_int(dungeon_hgt);
			x = rand_int(dungeon_wid);

			if (cave_passable_bold(y, x))
			{
				grid[grids++] = GRID(y, x);
				break;
			}
		}
	}


	/* Pick a grid at random */
	j = rand_int(grids);

	/* Get the coordinates */
	y = GRID_Y(grid[j]);
	x = GRID_X(grid[j]);

	/* Destroy any objects XXX XXX */
	delete_object(y, x);

	/* Explain the staircase */
	msg_print("A magical staircase appears...");

	/* Create stairs down */
	cave_set_feat(y, x, FEAT_MORE);

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
}


/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.  We
 * handle Morgoth and Sauron manually, to avoid slip-ups.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 *
 * Note that monsters can now carry objects; when a monster dies,
 * it drops whatever it is carrying.
 */
void monster_death(int m_idx)
{
	int i, j, y, x;

	int dump_item = 0;
	int dump_gold = 0;
	int gold_chance = 0;

	int number = 0;

	bool questlevel = FALSE;
	bool completed  = FALSE;
	bool fixedquest = FALSE;

	s16b unique_quark = 0;
	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = m_ptr->ml;

	bool good =  (r_ptr->flags1 & (RF1_DROP_GOOD))  ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (r_ptr->flags1 & (RF1_ONLY_ITEM)) ? FALSE : TRUE;
	bool do_item = (r_ptr->flags1 & (RF1_ONLY_GOLD)) ? FALSE : TRUE;

	object_type *i_ptr;
	object_type object_type_body;


	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Hack -- The death of a unique is always known */
	if (r_ptr->flags1 & (RF1_UNIQUE)) visible = TRUE;


	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get object */
		o_ptr = &o_list[this_o_idx];

		/* Get next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Paranoia */
		o_ptr->held_m_idx = 0;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Copy the object */
		object_copy(i_ptr, o_ptr);

		/* Delete the object */
		delete_object_idx(this_o_idx);

		/* Drop it */
		drop_near(i_ptr, -1, y, x);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;


	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
	if (r_ptr->flags1 & (RF1_DROP_90))
	{
		/* Hack -- ensure treasure in some cases. */
		if (r_ptr->flags1 & (RF1_DROP_CHEST | RF1_DROP_GREAT)) number++;
		else if (rand_int(100) < 90) number++;
	}

	if (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

	/* Hack -- handle creatures made of precious metals and gems */
	coin_type = get_coin_type(r_ptr);

	/* Average dungeon and monster levels */
	object_level = (p_ptr->depth + r_ptr->level) / 2;


	/* Hack -- inscribe items that a unique drops  -clefs- */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		unique_quark = quark_add(r_name + r_ptr->name);
	}


	/* We are allowed to drop gold, but only sometimes */
	if ((do_gold) && (do_item))
	{
		/* Chance of gold (if allowed) is normally 30% */
		gold_chance = 30;

		/* Low-level monsters drop gold much more often */
		if (MIN(85, p_ptr->depth) > r_ptr->level)
			gold_chance += 3 * (MIN(85, p_ptr->depth) - r_ptr->level);
		if (gold_chance > 90) gold_chance = 90;
	}

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Make gold when allowed */
		if (do_gold && (!do_item || (rand_int(100) < gold_chance)))
		{
			bool made_gold = FALSE;

			/* Make special treasures */
			if (great) made_gold = make_special_gold(i_ptr);

			/* Make special treasures sometimes */
			else if ((good) && (rand_int(100) < 30 + object_level / 5))
				made_gold = make_special_gold(i_ptr);

			/* Make ordinary treasures */
			else made_gold = make_gold(i_ptr);

			/* Note the gold */
			if (made_gold)
			{
				/* Add note */
				if (unique_quark) i_ptr->note = unique_quark;

				/* Assume seen XXX XXX */
				dump_gold++;

				/* Drop it in the dungeon */
				drop_near(i_ptr, -1, y, x);
			}
		}

		/* Make chest. */
		else if (r_ptr->flags1 & (RF1_DROP_CHEST))
		{
			required_tval = TV_CHEST;
			if (make_object(i_ptr, FALSE, FALSE, TRUE))
			{
				/* Add note */
				if (unique_quark) i_ptr->note = unique_quark;

				/* Assume seen XXX XXX XXX */
				dump_item++;

				/* Drop it in the dungeon */
				drop_near(i_ptr, -1, y, x);
			}
			required_tval = 0;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (make_object(i_ptr, good, great, FALSE))
			{
				/* Add note */
				if (unique_quark) i_ptr->note = unique_quark;

				/* Assume seen XXX XXX XXX */
				dump_item++;

				/* Drop it in the dungeon */
				drop_near(i_ptr, -1, y, x);
			}
		}
	}

	/* Reset the object level */
	object_level = p_ptr->depth;

	/* Reset "coin" type */
	coin_type = 0;


	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}


	/* Sauron is slain */
	if (m_ptr->r_idx == MON_SAURON)
	{
		/* Build magical stairs */
		drop_quest_stairs(y, x);

		/* Some extra fame */
		p_ptr->fame += 20;

		/* Take note of fame */
		left_panel_display(DISPLAY_FAME, 0);

		/* Continue the story */
		tell_story(MON_SAURON);
	}

	/* Morgoth, Lord of Darkness, is slain */
	if (m_ptr->r_idx == MON_MORGOTH)
	{
		/* Build magical stairs */
		drop_quest_stairs(y, x);

		/* Total winner */
		p_ptr->total_winner = TRUE;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

		/* Lots of fame */
		p_ptr->fame += 40;

		/* Take note of fame */
		left_panel_display(DISPLAY_FAME, 0);

		/* Continue the story */
		tell_story(MON_MORGOTH);
	}


	/* Only process dungeon kills */
	if (!p_ptr->depth) return;


	/* Count incomplete quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Quest level? */
		if (q_ptr->active_level == p_ptr->depth)
		{
			/* One on the level */
			questlevel = TRUE;

			/* Require "Quest Monsters" */
			if (q_ptr->r_idx == m_ptr->r_idx)
			{
				/* Mark kills */
				q_ptr->cur_num++;

				/* Completed quest? */
				if (q_ptr->cur_num == q_ptr->max_num)
				{
					/* Mark complete */
					q_ptr->active_level = 0;

					/* Mark fixed quests */
					if (q_ptr->type == QUEST_FIXED) fixedquest = TRUE;

					/* One complete */
					completed = TRUE;
				}

				/* Take note of quest */
				left_panel_display(DISPLAY_QUEST, 0);
			}
		}
	}

	/* Require a quest level */
	if (!questlevel) return;

	/* Require all quests on this level to be completed */
	if (!completed) return;


	/* Handle random quests */
	if (!fixedquest)
	{
		/* Give a message */
		msg_print("You have completed your quest - collect your reward at the Inn!");

		return;
	}

	/* Fixed quests are handled individually */
}

/*
 * Decrease a monster's hit points, handle monster death.
 * Note:  No other code should be used to hurt monsters.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Hack -- we delay fear messages by passing around a "fear" flag.
 */
bool mon_take_hit(int m_idx, int who, int dam, bool *fear, cptr note)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	s32b new_exp, new_exp_frac;

	char path[1024];

	/* Calculate character power based on total experience */
	int power = calc_exp_power();


	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);


	/* Allow the debugging of damage done. */
	if ((who < 0) && (p_ptr->wizard))
	{
		msg_format("You do %d (out of %d) damage.", dam, m_ptr->hp);
	}

	/* Practice a skill (if any is specified) */
	if ((skill_being_used) && (m_ptr->hp > 0))
	{
		/* Calculate base monster experience */
		new_exp = (long)r_ptr->mexp * r_ptr->level / power;

		/* Adjust for damage percentage (never over 100%) */
		new_exp *= MIN(dam, m_ptr->hp);
		new_exp /= m_ptr->hp;

		/* Practice */
		practice_skill(new_exp, skill_being_used);
	}

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Increase the noise level slightly. */
		if (add_wakeup_chance <= 8000) add_wakeup_chance += 300;

		/* Death by Missile/Spell attack */
		if (note)
		{
			/* Hack -- allow message suppression */
			if (strlen(note) <= 1)
			{
				/* Be silent */
			}
			else
			{
				message_format(MSG_KILL, 0, "%^s%s", m_name, note);
			}
		}

		/* A monster kill */
		else if (who >= 0)
		{
			/* At present, there is no generic "monster kill" message */
		}

		/* Physical kill by player */
		else
		{
			cptr kill_str;

			/* Invisible monsters are "killed" */
			if (!m_ptr->ml) kill_str = "killed";

			/* Non-living monsters are "destroyed" */
			else if (monster_nonliving(r_ptr))
			{
				kill_str = "destroyed";
			}

			/* Other monsters are "slain" */
			else kill_str = "slain";

			/* Kill message */
			message_format(MSG_KILL, 0, "You have %s %s.",
				kill_str, m_name);
		}

		/* Handle a player kill - experience */
		if (who < 0)
		{
			/* Give some experience for the kill */
			new_exp = ((long)r_ptr->mexp * r_ptr->level) / power;

			/* Get fractional exp */
			new_exp_frac = (long)p_ptr->exp_frac;

			/* Add new fractional exp */
			new_exp_frac += ((long)r_ptr->mexp * r_ptr->level) %
				power * 1000L / power;

			/* Keep track of experience */
			if (new_exp_frac >= 1000L)
			{
				new_exp++;
				p_ptr->exp_frac = (u16b)(new_exp_frac - 1000);
			}
			else
			{
				p_ptr->exp_frac = (u16b)new_exp_frac;
			}

			/*
			 * Hack -- If your weapon is hungry, it gets some of the exp, if
			 * you killed a living monster by hand.
			 */
			if ((p_ptr->feed_weapon) && (!monster_nonliving(r_ptr)))
			{
				s32b steal_xp;

				/* Weapon takes 10% of earned exp */
				steal_xp = new_exp / 10;
				new_exp -= steal_xp;

				/* Add to weapon's "food supply" */
				if ((long)p_ptr->soul_reserve + steal_xp > 30000L)
				{
					p_ptr->soul_reserve = 30000;
				}
				else
				{
					p_ptr->soul_reserve += (s16b)steal_xp;
				}

				/* No longer feeding */
				p_ptr->feed_weapon = FALSE;
			}

			/* Gain experience */
			gain_exp(new_exp, skill_being_used);
		}

		/* Some monsters have special death effects. */
		if (r_ptr->flags2 & (RF2_FUNKY_DEATH)) mon_death_effect(m_idx);

		/* Generate treasure */
		monster_death(m_idx);


		/* Handle a player kill - miscellaneous stuff */
		if (who < 0)
		{
			/* A unique is dead */
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				/* Uniques stay dead */
				r_ptr->max_num = 0;

				/* Fame may go up */
				if (p_ptr->fame < r_ptr->level / 2)
				    p_ptr->fame = r_ptr->level / 2;
			}

			/*
			 * When the player kills a player ghost, the bones file that
			 * it used is (often) deleted.
			 */
			if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
			{
				if (one_in_(3))
				{
					sprintf(path, "%s/bone.%03d", ANGBAND_DIR_BONE,
						bones_selector);
					remove(path);
				}
			}

			/* Recall even invisible uniques or winners */
			if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
			{
				/* Count kills this life */
				if (l_ptr->pkills < MAX_SHORT) l_ptr->pkills++;

				/* Count kills in all lives */
				if (l_ptr->tkills < MAX_SHORT) l_ptr->tkills++;

				/* Hack -- Auto-recall */
				monster_race_track(m_ptr->r_idx);
			}

			/* Increment the total kills count */
			p_ptr->total_kills++;


			/* Player score may be affected */
			p_ptr->update |= (PU_SCORE);

			/* Take note of new kill count */
			left_panel_display(DISPLAY_TOTAL_KILLS, 0);
		}

		/* Delete the monster */
		delete_monster_idx(m_idx);

		/* Not afraid */
		(*fear) = FALSE;

		/* Monster is dead */
		return (TRUE);
	}

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint(dam);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			/* Reduce fear */
			m_ptr->monfear -= tmp;
		}

		/* Cure all the fear */
		else
		{
			/* Cancel fear */
			set_mon_fear(m_ptr, 0, FALSE);

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	else if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)) && (dam > 0))
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;


		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if ((randint(10) >= percentage) ||
		   ((dam >= m_ptr->hp) && (!one_in_(5))))
		{
			int fear_amt;

			/* Hack -- note fear */
			(*fear) = TRUE;

			/* Hack -- Add some timed fear */
			fear_amt = rand_range(20, 30);

			/* Get frightened */
			set_mon_fear(m_ptr, fear_amt, TRUE);
		}
	}

	/* Monster is maddened -- may recover sanity */
	if ((m_ptr->mflag & (MFLAG_MADD)) &&
	    ((who < 0) || ((who == 0) && (one_in_(2)))))
	{
		/* Monster is no longer maddened */
		m_ptr->mflag &= ~(MFLAG_MADD);

		/* Take note if visible */
		if (m_ptr->ml)
		{
			char m_name[80];
			char m_poss[80];

			/* Extract monster name/poss */
			monster_desc(m_name, m_ptr, 0);
			monster_desc(m_poss, m_ptr, 0x22);

			/* Message */
			msg_format("%^s comes to %s senses.", m_name, m_poss);
		}
	}

	/* Monster will always go active */
	m_ptr->mflag |= (MFLAG_ACTV);

	/* Visible monsters lose their mimic status */
	if (m_ptr->ml) m_ptr->mflag &= ~(MFLAG_MIME);


	/* Recalculate desired minimum range */
	if (dam > 0) m_ptr->min_range = 0;

	/* Not dead yet */
	return (FALSE);
}
