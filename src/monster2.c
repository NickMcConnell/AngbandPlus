/* File: monster2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"




/*
 * Return another race for a monster to polymorph into.  -LM-
 *
 * Perform a modified version of "get_mon_num()", with exact minimum and
 * maximum depths and preferred monster types.
 */
s16b poly_r_idx(int base_idx)
{
	monster_race *r_ptr = &r_info[base_idx];

	alloc_entry *table = alloc_race_table;

	int i, min_lev, max_lev, r_idx;
	long total, value;

	/* int q_idx = q_info[quest_num(p_ptr->depth)].r_idx; */

	/* Source monster's level and symbol */
	int r_lev = r_ptr->level;
	char d_char = r_ptr->d_char;


	/* Hack -- Uniques and quest monsters never polymorph */
	if (r_ptr->flags1 & (RF1_UNIQUE))
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
	int x, y;

	monster_type *m_ptr = &m_list[i];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	s16b this_o_idx, next_o_idx = 0;


	/* Get location */
	y = m_ptr->fy;
	x = m_ptr->fx;


	/* Hack -- Reduce the racial counter */
	r_ptr->cur_num--;

	/* Decrement visibility count */
	if (m_ptr->ml)
	{
		update_mon_vis(m_ptr->r_idx, -1);
	}

	/* Hack -- remove target monster */
	if (p_ptr->target_who == i) target_set_monster(0);

	/* Hack -- remove tracked monster */
	if (p_ptr->health_who == i) health_track(0);


	/* Monster is gone */
	cave_m_idx[y][x] = 0;


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
	COPY(&m_list[i2], &m_list[i1], monster_type);

	/* Hack -- wipe hole */
	(void)WIPE(&m_list[i1], monster_type);
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
		/* int q_idx = q_info[quest_num(p_ptr->depth)].r_idx; */

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
		FREE(mon_lev, s16b);
		FREE(mon_index, s16b);
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

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Mega-Hack -- preserve Unique's XXX XXX XXX */

		/* Hack -- Reduce the racial counter */
		r_ptr->cur_num--;

		/* Clear seen list */
		r_ptr->r_see = 0;

		/* Monster is gone */
		cave_m_idx[m_ptr->fy][m_ptr->fx] = 0;

		/* Wipe the Monster */
		(void)WIPE(m_ptr, monster_type);
	}

	/* Reset "m_max" */
	m_max = 1;

	/* Reset "m_cnt" */
	m_cnt = 0;

	/* Hack -- reset "reproducer" count */
	num_repro = 0;

	/* Hack -- no more target */
	target_set_monster(0);

	/* Hack -- no more tracking */
	health_track(0);

	/* Hack -- reset "visible" counter */
	p_ptr->max_seen_r_idx = 0;
	p_ptr->window |= PW_VISIBLE;
}


/*
 * Get and return the index of a "free" monster.
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

		/* Get the monster */
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
 * a small amount (up to four levels), except in the town.
 *
 * It is (slightly) more likely to acquire a monster of the given level
 * than one of a lower level.  This is done by choosing several monsters
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no monsters are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 */
s16b get_mon_num(int level)
{
	int i;

	int r_idx;

	long value, total;

	monster_race *r_ptr;

	alloc_entry *table = alloc_race_table;

	/* Low-level monsters avoid the deep dungeon. */
	/* int depth_rare = 4 * level / 5; */
	/* int depth_none = 2 * level / 3; */
	int depth_rare = 4 * level / 6;
	int depth_none = 2 * level / 3;
	
	/* Sometimes, monsters in the dungeon can be out of depth */
	if (p_ptr->depth != 0)
	{
		/* Occasional boost to maximum level */
		if (one_in_(NASTY_MON))
		{
			/* Boost the level  (from 1 to 5) */
			level += MIN(5, div_round(level, 10) + 1);

			/* Occasional second boost */
			if (one_in_(NASTY_MON))
			{
				/* Boost the level  (from 1 to 5) */
				level += MIN(5, div_round(level, 10) + 1);
			}
		}
	}


	/* Reset total */
	total = 0L;

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

		/* Hack -- "unique" monsters must be "unique" */
		if ((r_ptr->flags1 & (RF1_UNIQUE)) &&
		    (r_ptr->cur_num >= r_ptr->max_num))
		{
			continue;
		}

		/* Depth Monsters never appear out of depth */
		if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (r_ptr->level > p_ptr->depth))
		{
			continue;
		}

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Keep low-level monsters rare */
		if (table[i].level < depth_rare) table[i].prob3 /= 4;
		if (table[i].level < depth_none) table[i].prob3 = 0;

		/* Total */
		total += table[i].prob3;
	}

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
 *   0x22 --> Possessive, genderized if visable ("his") or "its"
 *   0x23 --> Reflexive, genderized if visable ("himself") or "itself"
 */
 
/* I am very confused.the 2.9.3 code, and the 4GAI like this to be */
/* const monster_type *m_ptr, but is_pet doesn't like it. I removed it */
/* before and it didn't break anything, so I'm removing it again - though */
/* I have no idea what it does. . . */
void monster_desc(char *desc, monster_type *m_ptr, int mode)
{
	cptr res;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	cptr name = (r_name + r_ptr->name);

	bool seen, pron;


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
		/* It could be a Unique */
		if (r_ptr->flags1 & (RF1_UNIQUE))
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
			/* pet monsters need a possive */
			if (is_pet(m_ptr))
				(void)strcpy(desc, "your ");
			/* Definite monsters need a definite article */
			else
				(void)strcpy(desc, "the ");
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
		if (!panel_contains(m_ptr->fy, m_ptr->fx))
		{
			/* Append special notation */
			strcat(desc, " (offscreen)");
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

	cptr name = (r_name + r_ptr->name);

	/* Write the name */
	my_strcpy(desc, name, max);
}

/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];


	/* Hack -- Memorize some flags */
	l_ptr->r_flags1 = r_ptr->flags1;
	l_ptr->r_flags2 = r_ptr->flags2;
	l_ptr->r_flags3 = r_ptr->flags3;
	l_ptr->r_flags8 = r_ptr->flags8;

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
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
void lore_treasure(int m_idx, int num_item, int num_gold)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];


	/* Note the number of things dropped */
	if (num_item > l_ptr->r_drop_item) l_ptr->r_drop_item = num_item;
	if (num_gold > l_ptr->r_drop_gold) l_ptr->r_drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (r_ptr->flags1 & (RF1_DROP_GOOD)) l_ptr->r_flags1 |= (RF1_DROP_GOOD);
	if (r_ptr->flags1 & (RF1_DROP_GREAT)) l_ptr->r_flags1 |= (RF1_DROP_GREAT);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}
}


void update_mon_vis(u16b r_idx, int increment)
{
	monster_race *r_ptr = &r_info[r_idx];
	int i;

        /* Changes on screen
        p_ptr->window |= PW_VISIBLE; */

	/* Paranoia */
#if 0
	if (!r_ptr->r_see && (increment == -1)) core("Monster visibility error!");
#else /* 0 */
	/* Ignore the bug, until we know what is really going on. */
	if (!r_ptr->r_see && (increment == -1)) return;
#endif /* 0 */

	/* Update the counter */
	r_ptr->r_see += increment;

	/* Update 'most powerful seen monster' */
	if (r_ptr->r_see)
	{
		/* Check to see if we have spotted a more powerful monster */
		if (r_idx > p_ptr->max_seen_r_idx)
		{
			/* Track this monster */
			p_ptr->max_seen_r_idx = r_idx;
		}
	}
	else
	{
		/* Look to see if we need to recalculate max_seen_r_idx */
		if (r_idx == p_ptr->max_seen_r_idx)
		{
			for (i = r_idx - 1; i > 0; i--)
			{
				/* Can we see this monster? */
				if (r_info[i].r_see) break;
			}

			/* Record it */
			p_ptr->max_seen_r_idx = i;
		}
	}
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
				l_ptr->r_flags2 |= (RF2_EMPTY_MIND);
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
					l_ptr->r_flags2 |= (RF2_WEIRD_MIND);

					/* Hack -- Memorize mental flags */
					if (r_ptr->flags2 & (RF2_SMART)) l_ptr->r_flags2 |= (RF2_SMART);
					if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->r_flags2 |= (RF2_STUPID);
				}
			}

			/* Normal mind, allow telepathy */
			else
			{
				/* Detectable */
				vision = MAX(vision, 2);

				/* Hack -- Memorize mental flags */
				if (r_ptr->flags2 & (RF2_SMART)) l_ptr->r_flags2 |= (RF2_SMART);
				if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->r_flags2 |= (RF2_STUPID);
			}
		}
#if 0
		/* Priestly awareness of evil */
		if (p_ptr->esp_evil)
		{
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				vision = MAX(vision, 2);
			}
		}
#endif
		if (p_ptr->wormsense)
		{
			if (!(r_ptr->flags2 & (RF2_PASS_WALL)))
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
					if ((p_ptr->see_inv) || (p_ptr->tim_invis))
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
#if 0

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
#endif
			}
#if 0
			/* Handle "shining" monsters */
			if (r_ptr->flags2 & (RF2_IS_LIT))
			{
				/* Shining monsters can still be invisible */
				if (!(r_ptr->flags2 & (RF2_INVISIBLE)) ||
				     (p_ptr->see_inv) || (p_ptr->detect_inv))
				{
					/* Note that monster is shining */
					l_ptr->r_flags2 |= (RF2_IS_LIT);

					/* Shining monsters are harder to see from afar */
					vision = MAX(vision, (d < 10) ? 2 : 1);
				}
			}
#endif
			/* Visible */
			if (vision)
			{
				/* Memorize flags */
				if (do_invisible)  l_ptr->r_flags2 |= (RF2_INVISIBLE);
				if (do_cold_blood) l_ptr->r_flags2 |= (RF2_COLD_BLOOD);
			}
		}

#if 0
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
			if (awareness > 5 + (r_ptr->level * 2)/ 5 +
			    rand_int(45 + r_ptr->level))
			{
				/* Limited "sight" */
				vision = MAX(vision, 1);
				detection = MAX(detection, 1);
			}

			/* Do not use the quick RNG */
			Rand_quick = FALSE;
		}
#endif
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

		/* Update monster visibility counter */
		update_mon_vis(m_ptr->r_idx, ((vision - old_vision)/2));

		/* Update monster list window */
		p_ptr->window |= (PW_VISIBLE);

		/* Hack -- Count "fresh" sightings  XXX */
		if ((mon_fully_visible(m_ptr)) && (l_ptr->r_sights < MAX_SHORT))
		{
			l_ptr->r_sights++;
		}

		/* Disturb on visibility change */
		if (disturb_move)
		{
			/* Disturb if monster is not a townsman, or if fairly weak */
			if (!(m_ptr->mflag & (MFLAG_TOWN)))
			{
				disturb(1, 0);
			}
		}
	}
#if 0
	/* Hack -- Character "forgets" about (far-away) mimics */
	if ((r_ptr->flags1 & (RF1_CHAR_MIMIC)) && (!m_ptr->ml) &&
	    (d >= MAX_SIGHT + 5))
	{
		/* Mimic hides */
		m_ptr->mflag |= (MFLAG_MIME);
	}
#endif
	/* Monster enters or leaves field of sight (eyes, infavision) */
	if ((( easy) && (!(m_ptr->mflag & (MFLAG_VIEW)))) ||
	    ((!easy) &&   (m_ptr->mflag & (MFLAG_VIEW))))
	{
		/* Mark as easily visible */
		if (easy) m_ptr->mflag |= (MFLAG_VIEW);

		/* No longer easily viewable */
		else      m_ptr->mflag &= ~(MFLAG_VIEW);

		/* Disturb on appearance or disappearance */
		if (disturb_near)
		{
			/* Disturb if monster is not a townsman, or if fairly weak */
			if (!(m_ptr->mflag & (MFLAG_TOWN)))
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


	/* Scan objects already being held for combination */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

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
 * Swap the players/monsters (if any) at two locations XXX XXX XXX
 */
void monster_swap(int y1, int x1, int y2, int x2)
{
	int m1, m2;

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
s16b monster_place(int y, int x, monster_type *n_ptr, bool pet)
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

		/* Get the new monster */
		m_ptr = &m_list[m_idx];

		/* Copy the monster XXX */
		COPY(m_ptr, n_ptr, monster_type);

		/* Location */
		m_ptr->fy = y;
		m_ptr->fx = x;

		/* Update the monster */
		update_mon(m_idx, TRUE, FALSE);

		/* Get the new race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Pet? */
		if (pet)
		{
			set_pet(m_ptr);
		}
		/* Friendly? */
		else if (r_ptr->flags2 & (RF2_FRIENDLY))
		{
			set_friendly(m_ptr);
		}

		/* Hack -- Notice new multi-hued monsters */
		if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

		/* Count racial occurances */
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
	for (i = 0; i < 4; i++)
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
static bool place_monster_one(int y, int x, int r_idx, bool slp, bool pet, bool clone)
{
	int i;

	monster_race *r_ptr;

	monster_type *n_ptr;
	monster_type monster_type_body;

	cptr name;


	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Hack -- no creation on glyph of warding */
	if (cave_feat[y][x] == FEAT_GLYPH) return (FALSE);


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
		if (clone)
		{
			/* allow cloning of uniques */
		}
		/* Cannot create */
		else return (FALSE);
	}


	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (p_ptr->depth < r_ptr->level))
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
		/* Hitpoints usually vary from 75% to 125% of normal  -EZ- */
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
	if (r_ptr->level > 10) n_ptr->harass = BASE_HARASS;

	/* Weaker monsters don't live long enough to spend forever harassing */
	else n_ptr->harass = LOW_HARASS;


	/* Initialize mana */
	n_ptr->mana = r_ptr->mana;

	/* And start out fully healthy */
	n_ptr->hp = n_ptr->maxhp;

	/* Extract the monster base speed */
	n_ptr->mspeed = r_ptr->speed;

	if (clone) n_ptr->mflag |= (MFLAG_CLON);

	/* Hack -- small racial variety */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		/* Allow some small variation per monster */
		i = extract_energy[r_ptr->speed] / 10;
		if (i) n_ptr->mspeed += rand_spread(0, i);
	}


	/* Give almost no starting energy (avoids clumped movement) */
	n_ptr->energy = (byte)rand_int(10);

#if 0 /* moved the above (energy) from below */
	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
	}
	else
	{
		/* Give a random starting energy */
		n_ptr->energy = (byte)rand_int(50);
	}
#endif /* All monsters should mostly move after player */

	/* Place the monster in the dungeon */
	if (!monster_place(y, x, n_ptr, pet)) return (FALSE);



	/* Powerful monster */
	if (r_ptr->level > p_ptr->depth)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
			if (cheat_hear) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - p_ptr->depth) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
			if (cheat_hear) msg_format("Deep Monster (%s).", name);

			/* Boost rating by half delta-depth */
			rating += (r_ptr->level - p_ptr->depth) / 2;
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


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX	8


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
			if (place_monster_one(my, mx, r_idx, slp, FALSE, FALSE))
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
	if (r_ptr->flags1 & (RF1_ESCORTS)) escort_size = rand_range(6, 8);
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
			else if (!place_monster_one(my, mx, escort_idx, slp, FALSE, FALSE))
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
bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp, bool pet, bool clone)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Place one monster, or fail */
	if (!place_monster_one(y, x, r_idx, slp, pet, clone)) return (FALSE);

	/* if a pet is being summoned, you only get one. */
	/* if (pet) return (TRUE); */

	/* Require the "group" flag */
	if (!grp) return (TRUE);

	/* Friends for certain monsters */
	if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		/* Attempt to place a large group */
		(void)place_monster_group(y, x, r_idx, slp, (s16b)rand_range(4, 8));
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
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "monster_level"
 */
bool place_monster(int y, int x, bool slp, bool grp, bool pet)
{
	int r_idx;

	/* Pick a monster */
	r_idx = get_mon_num(monster_level);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	if (place_monster_aux(y, x, r_idx, slp, grp, pet, FALSE)) return (TRUE);

	/* Oops */
	return (FALSE);
}




/*
 * XXX XXX XXX Player Ghosts are such a hack, they have been completely
 * removed until Angband 2.8.0, in which there will actually be a small
 * number of "unique" monsters which will serve as the "player ghosts".
 * Each will have a place holder for the "name" of a deceased player,
 * which will be extracted from a "bone" file, or replaced with a
 * "default" name if a real name is not available.  Each ghost will
 * appear exactly once and will not induce a special feeling.
 *
 * Possible methods:
 *   (s) 1 Skeleton
 *   (z) 1 Zombie
 *   (M) 1 Mummy
 *   (G) 1 Polterguiest, 1 Spirit, 1 Ghost, 1 Shadow, 1 Phantom
 *   (W) 1 Wraith
 *   (V) 1 Vampire, 1 Vampire Lord
 *   (L) 1 Lich
 *
 * Possible change: Lose 1 ghost, Add "Master Lich"
 *
 * Possible change: Lose 2 ghosts, Add "Wraith", Add "Master Lich"
 *
 * Possible change: Lose 4 ghosts, lose 1 vampire lord
 *
 * Note that ghosts should never sleep, should be very attentive, should
 * have maximal hitpoints, drop only good (or great) items, should be
 * cold blooded, evil, undead, immune to poison, sleep, confusion, fear.
 *
 * Base monsters:
 *   Skeleton
 *   Zombie
 *   Mummy
 *   Poltergeist
 *   Spirit
 *   Ghost
 *   Vampire
 *   Wraith
 *   Vampire Lord
 *   Shadow
 *   Phantom
 *   Lich
 *
 * This routine will simply extract ghost names from files, and
 * attempt to allocate a player ghost somewhere in the dungeon,
 * note that normal allocation may also attempt to place ghosts,
 * so we must work with some form of default names.
 *
 * XXX XXX XXX
 */





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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;
	int	attempts_left = 10000;

	/* Find a legal, distant, unoccupied, space */
	while (--attempts_left)
	{
		/* Pick a location */
		y = rand_int(DUNGEON_HGT);
		x = rand_int(DUNGEON_WID);

		/* Require a grid that all monsters can exist in. */
		if (!cave_floor_bold(y, x)) continue;

		/* Accept far away grids */
		if (distance(y, x, py, px) > dis) break;
	}

	if (!attempts_left)
	{
		if (cheat_xtra || cheat_hear)
		{
			msg_print("Warning! Could not allocate a new monster.");
		}

		return FALSE;
	}

	/* Attempt to place the monster, allow groups */
	if (place_monster(y, x, slp, TRUE, FALSE)) return (TRUE);

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


	/* Hack -- no specific type specified */
	if (!summon_specific_type) return (TRUE);


	/* Check our requirements */
	switch (summon_specific_type)
	{
		case SUMMON_CUTTENCLIP:
		{
			okay = ((r_ptr->flags8 & (RF8_CUTTENCLIP)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}			
		case SUMMON_BEASTMAN:
		{
			okay = (((r_ptr->d_char == 'B') ||
					 (r_ptr->d_char == 'b')) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_ANIMAL:
		{
			okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}
		case SUMMON_AUTOMATA:
		{
			okay = ((r_ptr->flags3 & (RF3_AUTOMATA)) &&
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
			okay = ((r_ptr->d_char == 'Z') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_MONKEY:
		{
			okay = ((r_ptr->d_char == 'M') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_ALIEN:
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

		case SUMMON_ELEMENTAL:
		{
			okay = ((r_ptr->flags3 & (RF3_ELEMENTAL)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_KIN:
		{
			okay = ((r_ptr->d_char == summon_kin_type) &&
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

		case SUMMON_PLANT:
		{
			okay = ((r_ptr->d_char == 'T') ||
			        (r_ptr->d_char == 'n'));
			break;
		}

		case SUMMON_HI_ELEMENTAL:
		{
			okay = (r_ptr->d_char == 'E');
			break;
		}

		case SUMMON_HI_DEMON:
		{
			okay = (r_ptr->d_char == 'U');
			break;
		}

		case SUMMON_LO_DEMON:
		{
			okay = (r_ptr->d_char == 'u');
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
			okay = (r_ptr->flags1 & (RF1_UNIQUE)) ? TRUE : FALSE;
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
 */
bool summon_specific(int y1, int x1, int lev, int type, bool pet)
{
	int i, x, y, r_idx;

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
		if (cave_feat[y][x] == FEAT_GLYPH) continue;

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
	r_idx = get_mon_num(lev);


	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, FALSE, pet, FALSE)) return (FALSE);

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
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 * Currently the pet variable is not used - pets currently spawn as normal
 * and produce hostile monsters
 */
bool multiply_monster(int m_idx, bool pet, bool clone)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i, y, x;

	bool result = FALSE;

	u16b grid[8];
	int grids = 0;
	
	if (pet)
	{
		/* do nothing - how's this for line bloat */
	}

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
	result = place_monster_aux(y, x, m_ptr->r_idx, FALSE, FALSE, FALSE, clone);

	/* Result */
	return (result);
}






/*
 * Dump a message describing a monster's reaction to damage
 *
 * Technically should attempt to treat "Beholder"'s as jelly's
 */
void message_pain(int m_idx, int dam)
{
	long oldhp, newhp, tmp;
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

	/* Note -- subtle fix -CFT */
	newhp = (long)(m_ptr->hp);
	oldhp = newhp + (long)(dam);
	tmp = (newhp * 100L) / oldhp;
	percentage = (int)(tmp);


	/* Jelly's, Mold's, Vortex's, Quthl's */
	if (strchr("IJRSTcklmnrw", r_ptr->d_char))
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

	/* Dogs, Hounds and cats*/
	else if (strchr("Zf", r_ptr->d_char))
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
			msg_format("%^s yelps feebly.", m_name);
	}

	/* One type of monsters (Constructs/non-living) */
	else if (strchr("CEFGKVWaegsvz", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s twitches from the attack.", m_name);
		else if (percentage > 50)
			msg_format("%^s shakes from the attack.", m_name);
		else if (percentage > 35)
			msg_format("%^s vibrates from the attack.", m_name);
		else if (percentage > 20)
			msg_format("%^s shudders from the attack.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes from the attack.", m_name);
		else
			msg_format("%^s jerks limply.", m_name);
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (strchr("ABPMbhpt", r_ptr->d_char))
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
			msg_format("%^s roars in agony.", m_name);
		else
			msg_format("%^s yells out feebly.", m_name);
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

	int save;
	
	save = p_ptr->skill_sav;

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

			if (save >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (save >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
			else m_ptr->smart &= ~(SM_IMM_FREE);
			break;
		}

		/* Fire attacks learn about Fire resists and immunities */
		case LRN_FIRE:
		{
			if (p_ptr->res[RS_FIR] > 20) m_ptr->smart |= (SM_OPP_FIRE);
			break;
		}

		/* Earth attacks learn about Earth resists and immunities */
		case LRN_EARTH:
		{
			if (p_ptr->res[RS_EAR] > 20) m_ptr->smart |= (SM_OPP_EARTH);
			break;
		}

		/* Air attacks learn about Air resists and immunities */
		case LRN_AIR:
		{
			if (p_ptr->res[RS_AIR] > 20) m_ptr->smart |= (SM_OPP_AIR);
			break;
		}

		/* Water attacks learn about Water resists and immunities */
		case LRN_WATER:
		{
			if (p_ptr->res[RS_WTR] > 20) m_ptr->smart |= (SM_OPP_WATER);
			break;
		}

		/* Elec attacks learn about Elec resists and immunities */
		case LRN_ELEC:
		{
			if (p_ptr->res[RS_ELC] > 20) m_ptr->smart |= (SM_OPP_ELEC);
			break;
		}

		/* Ice attacks learn about Ice resists and immunities */
		case LRN_ICE:
		{
			if (p_ptr->res[RS_ICE] > 20) m_ptr->smart |= (SM_OPP_ICE);
			break;
		}

		/* Acid attacks learn about Acid resists and immunities */
		case LRN_ACID:
		{
			if (p_ptr->res[RS_ACD] > 20) m_ptr->smart |= (SM_OPP_ACID);
			break;
		}

		/* Poison attacks learn about Poison resists and immunities */
		case LRN_POISON:
		{
			if (p_ptr->res[RS_PSN] > 20) m_ptr->smart |= (SM_OPP_POISON);
			break;
		}

		/* Time attacks learn about Time resists and immunities */
		case LRN_TIME:
		{
			if (p_ptr->res[RS_TIM] > 20) m_ptr->smart |= (SM_OPP_TIME);
			break;
		}

		/* Ether attacks learn about Ether resists and immunities */
		case LRN_ETHER:
		{
			if (p_ptr->res[RS_ETH] > 20) m_ptr->smart |= (SM_OPP_ETHER);
			break;
		}

		/* Sound attacks learn about Sound resists and immunities */
		case LRN_SOUND:
		{
			if (p_ptr->res[RS_SND] > 20) m_ptr->smart |= (SM_OPP_SOUND);
			break;
		}

		/* Nether attacks learn about Nether resists and immunities */
		case LRN_NETHER:
		{
			if (p_ptr->res[RS_NTH] > 20) m_ptr->smart |= (SM_OPP_NETHER);
			break;
		}

		/* Light attacks learn about Light resists and immunities */
		case LRN_LIGHT:
		{
			if (p_ptr->res[RS_LIT] > 20) m_ptr->smart |= (SM_OPP_LIGHT);
			break;
		}

		/* Dark attacks learn about Dark resists and immunities */
		case LRN_DARK:
		{
			if (p_ptr->res[RS_DRK] > 20) m_ptr->smart |= (SM_OPP_DARK);
			break;
		}

		/* Psi attacks learn about Psi resists and immunities */
		case LRN_PSI:
		{
			if (p_ptr->res[RS_PSI] > 20) m_ptr->smart |= (SM_OPP_PSI);
			break;
		}

		/* Tk attacks learn about Tk resists and immunities */
		case LRN_TK:
		{
			if (p_ptr->res[RS_TLK] > 20) m_ptr->smart |= (SM_OPP_TK);
			break;
		}

		/* Spirit attacks learn about spirit resists and immunities */
		case LRN_SPIRIT:
		{
			if (p_ptr->res[RS_SPI] > 20) m_ptr->smart |= (SM_OPP_SPIRIT);
			break;
		}

		/* Mana attacks learn if you have any mana to attack */
		case LRN_MANA:
		{
			if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
			else m_ptr->smart &= ~(SM_IMM_MANA);
			break;
		}

		/* Fear attacks learn about resist fear and saving throws */
		case LRN_FEAR:
		{
			if (save >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (save >= 100) m_ptr->smart |= (SM_PERF_SAVE);
			else m_ptr->smart &= ~(SM_PERF_SAVE);
			if (p_ptr->resist_fear) m_ptr->smart |= (SM_IMM_FEAR);
			else m_ptr->smart &= ~(SM_IMM_FEAR);
			break;
		}


		/*
		 * Some Blindness attacks learn about blindness resistance
		 * Others (below) do more
		 */
		case LRN_BLIND:
		{
			if (p_ptr->resist_blind) m_ptr->smart |= (SM_IMM_BLIND);
			else m_ptr->smart &= ~(SM_IMM_BLIND);
			break;
		}

		/*
		 * Some Confusion attacks learn about confusion resistance
		 * Others (below) do more
		 */
		case LRN_CONFU:
		{
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_IMM_CONFU);
			else m_ptr->smart &= ~(SM_IMM_CONFU);
			break;
		}

		/* Some attacks learn only about saving throws (cause wounds, etc) */
		case LRN_SAVE:
		{
			if (save >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
			else m_ptr->smart &= ~(SM_GOOD_SAVE);
			if (save >= 100) m_ptr->smart |= (SM_PERF_SAVE);
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
			break;
		}
	}
}



