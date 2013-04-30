/* File: monster2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
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

	/* Hack -- count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro--;

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
 * When actually "compacting" monsters, we base the saving throw
 * on a combination of monster level, distance from player, and
 * current "desperation".
 *
 * After "compacting" (if needed), we "reorder" the monsters into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */
void compact_monsters(int size)
{
	int i, num, cnt;

	int cur_lev, cur_dis, chance;


	/* Message (only if compacting) */
	if (size) msg_print("Compacting monsters...");


	/* Compact at least 'size' objects */
	for (num = 0, cnt = 1; num < size; cnt++)
	{
		/* Get more vicious each iteration */
		cur_lev = 5 * cnt;

		/* Get closer each iteration */
		cur_dis = 5 * (20 - cnt);

		/* Check all the monsters */
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];

			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- skip "dead" monsters */
			if (!m_ptr->r_idx) continue;

			/* Hack -- High level monsters start out "immune" */
			if (r_ptr->level > cur_lev) continue;

			/* Ignore nearby monsters */
			if ((cur_dis > 0) && (m_ptr->cdis < cur_dis)) continue;

			/* Saving throw chance */
			chance = 90;

			/* Only compact "Quest" Monsters in emergencies */
			if ((r_ptr->flags1 & (RF1_QUESTOR)) && (cnt < 1000)) chance = 100;

			/* Try not to compact Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) chance = 99;

			/* All monsters get a saving throw */
			if (rand_int(100) < chance) continue;

			/* Delete the monster */
			delete_monster_idx(i);

			/* Count the monster */
			num++;
		}
	}


	/* Excise dead monsters (backwards!) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Get the i'th monster */
		monster_type *m_ptr = &m_list[i];

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
	int i, j, p;

	int r_idx;

	long value, total;

	monster_race *r_ptr;

	alloc_entry *table = alloc_race_table;

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

	bool daytime = ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2));

	/* Boost the level */
	if (level > 0)
	{
		/* Occasional "nasty" monster */
		if (rand_int(NASTY_MON) == 0)
		{
			/* Pick a level bonus */
			int d = level / 4 + 2;

			/* Boost the level */
			level += ((d < 5) ? d : 5);
		}

		/* Occasional "nasty" monster */
		if (rand_int(NASTY_MON) == 0)
		{
			/* Pick a level bonus */
			int d = level / 4 + 2;

			/* Boost the level */
			level += ((d < 5) ? d : 5);
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

		/* No town monsters in dungeon */
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

		/* Hack -- "questor" monsters and guardians must be placed specifically */
		if ((r_ptr->flags1 & (RF1_QUESTOR)) || (r_ptr->flags1 & (RF1_GUARDIAN))) continue;

		/* Depth Monsters never appear out of depth */
		if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (r_ptr->level > p_ptr->depth)) continue;

		/* Hack -- No MULTIPLY monsters on surface */
		if (surface && (r_ptr->flags2 & (RF2_MULTIPLY))) continue;

		/* Hack -- No NEVER_MOVE monsters on surface */
		if (surface && (r_ptr->flags1 & (RF1_NEVER_MOVE))) continue;

		/* Hack -- No HURT_LITE monsters on surface in daytime */
		if (surface && daytime && (r_ptr->flags3 & (RF3_HURT_LITE))) continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Don't like monsters that don't 'suit' a particular level */
		/* Currently we don't do this quite right because we should
		   favour monsters that qualify for place_monster_here. XXX XXX */
		if (level_flag & (LF1_WATER | LF1_LAVA | LF1_ICE | LF1_ACID | LF1_CHASM))
		{
			if (level_flag & (LF1_WATER))
			{
				if (!(r_ptr->flags2 & (RF2_CAN_SWIM))&&
					!(r_ptr->flags3 & (RF3_NONLIVING))) table[i].prob3 /=4;
			}
			if (level_flag & (LF1_LAVA))
			{
				if (!(r_ptr->flags3 & (RF3_IM_FIRE))) table[i].prob3 /=4;
			}
			if (level_flag & (LF1_ICE))
			{
				if (!(r_ptr->flags3 & (RF3_IM_COLD))) table[i].prob3 /=4;
			}
			if (level_flag & (LF1_ACID))
			{
				if (!(r_ptr->flags3 & (RF3_IM_ACID))) table[i].prob3 /=4;
			}
			if (level_flag & (LF1_CHASM))
			{
				if (!(r_ptr->flags2 & (RF2_CAN_CLIMB)) &&
					!(r_ptr->flags2 & (RF2_CAN_FLY))) table[i].prob3 /=4;
			}
		}

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
 * Display visible monsters in a window
 */
void display_monlist(void)
{
	int idx, n;
	int line = 0;

	char *m_name;
	char buf[80];

	monster_type *m_ptr;
	monster_race *r_ptr;

	u16b *race_counts;

	/* Allocate the array */
	C_MAKE(race_counts, z_info->r_max, u16b);

	/* Iterate over mon_list */
	for (idx = 1; idx < z_info->m_max; idx++)
	{
		m_ptr = &m_list[idx];

		/* Only visible monsters */
		if (!m_ptr->ml) continue;

		/* Bump the count for this race */
		race_counts[m_ptr->r_idx]++;
	}


	/* Iterate over mon_list ( again :-/ ) */
	for (idx = 1; idx < z_info->m_max; idx++)
	{
		m_ptr = &m_list[idx];

		/* Only visible monsters */
		if (!m_ptr->ml) continue;

		/* Do each race only once */
		if (!race_counts[m_ptr->r_idx]) continue;

		/* Get monster race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Get the monster name */
		m_name = r_name + r_ptr->name;

		/* Obtain the length of the description */
		n = strlen(m_name);

		/* Display multiple monsters */
		if (race_counts[m_ptr->r_idx] > 1)
		{
			/* Add race count */
			sprintf(buf, "%d", race_counts[m_ptr->r_idx]);
			Term_putstr(0, line, strlen(buf), TERM_WHITE, buf);
			Term_addstr(-1, TERM_WHITE, " ");

			/* Display the entry itself */
			Term_addstr(-1, TERM_WHITE, m_name);

			/* XXX Need to pluralise this properly */
			Term_addstr(-1, TERM_WHITE, "s");

			n+= strlen(buf) + 2;
		}
		/* Display single monsters */
		else
		{
			/* Display the entry itself */
			Term_putstr(0, line, n, TERM_WHITE, m_name);
		}

		/* Append the "standard" attr/char info */
		Term_addstr(-1, TERM_WHITE, " ('");
		Term_addch(r_ptr->d_attr, r_ptr->d_char);
		Term_addstr(-1, TERM_WHITE, "')");
		n += 6;

		/* Monster graphic on one line */
		if (!(use_dbltile) && !(use_trptile))
		{
			/* Append the "optional" attr/char info */
			Term_addstr(-1, TERM_WHITE, "/('");

			Term_addch(r_ptr->x_attr, r_ptr->x_char);

			if (use_bigtile)
			{
				if (r_ptr->x_attr & 0x80)
					Term_addch(255, -1);
				else
					Term_addch(0, ' ');

				n++;
			}

			Term_addstr(-1, TERM_WHITE, "')");
			n += 6;
		}

		/* Erase the rest of the line */
		Term_erase(n, line, 255);

		/* Don't display again */
		race_counts[m_ptr->r_idx] = 0;

		/* Bump line counter */
		line++;
	}

	/* Free the race counters */
	FREE(race_counts);

	/* Erase the rest of the window */
	for (idx = line; idx < Term->hgt; idx++)
	{
		/* Erase the line */
		Term_erase(0, idx, 255);
	}
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
 *   0x08 --> Use indefinites for visible monsters ("a goblin")
 *   0x10 --> Pronominalize hidden monsters
 *   0x20 --> Pronominalize visible monsters
 *   0x40 --> Assume the monster is hidden
 *   0x80 --> Assume the monster is visible
 *
 * Useful Modes:
 *   0x00 --> Full nominative name ("the goblin") or "it"
 *   0x04 --> Full nominative name ("the goblin") or "something"
 *   0x80 --> Banishment resistance name ("the goblin")
 *   0x88 --> Killing name ("a goblin")
 *   0x22 --> Possessive, genderized if visable ("his") or "its"
 *   0x23 --> Reflexive, genderized if visable ("himself") or "itself"
 */
void monster_desc(char *desc, const monster_type *m_ptr, int mode)
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

		/* Mention "hidden" monsters XXX XXX */
		/* Note we only see "hidden" monsters with detection,
		   or telepathy, and this is different to non-visible
		   monsters handled above.
		   We need to warn players because otherwise they
		   will try and target these monsters.*/

		/* XXX Perhaps we should use a different attr/char */
		if (m_ptr->mflag & (MFLAG_HIDE))
		{
			/* Append special notation */
			strcat(desc, " (hidden)");
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
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];


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
	if (num_item > l_ptr->drop_item) l_ptr->drop_item = num_item;
	if (num_gold > l_ptr->drop_gold) l_ptr->drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (r_ptr->flags1 & (RF1_DROP_GOOD)) l_ptr->flags1 |= (RF1_DROP_GOOD);
	if (r_ptr->flags1 & (RF1_DROP_GREAT)) l_ptr->flags1 |= (RF1_DROP_GREAT);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
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
 * are only affected by "light" if the player can see invisible), and
 * it is not hidden in a feature somehow.
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
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int d;

	/* Current location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	/* Seen at all */
	bool flag = FALSE;

	/* Seen by vision */
	bool easy = FALSE;

	/* Compute distance */
	if (full)
	{
		int py = p_ptr->py;
		int px = p_ptr->px;

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
	if (m_ptr->mflag & (MFLAG_MARK)) flag = TRUE;


	/* Nearby */
	if (d <= MAX_SIGHT)
	{
		/* Basic telepathy */
		if ((p_ptr->cur_flags3 & (TR3_TELEPATHY)) != 0)
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
				/* One in ten individuals are detectable */
				if ((m_idx % 10) == 5)
				{

					/* Detectable */
					flag = TRUE;

					/* Memorize flags */
					l_ptr->flags2 |= (RF2_WEIRD_MIND);

					/* Hack -- Memorize mental flags */
					if (r_ptr->flags2 & (RF2_SMART)) l_ptr->flags2 |= (RF2_SMART);
					if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->flags2 |= (RF2_STUPID);
					if (r_ptr->flags3 & (RF3_NONVOCAL)) l_ptr->flags2 |= (RF3_NONVOCAL);
				}
			}

			/* Normal mind, allow telepathy */
			else
			{
				/* Detectable */
				flag = TRUE;

				/* Hack -- Memorize mental flags */
				if (r_ptr->flags2 & (RF2_SMART)) l_ptr->flags2 |= (RF2_SMART);
				if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->flags2 |= (RF2_STUPID);
				if (r_ptr->flags3 & (RF3_NONVOCAL)) l_ptr->flags2 |= (RF3_NONVOCAL);

			}

#ifdef ALLOW_OBJECT_INFO_MORE
			/* Visible */
			if (flag) equip_can_flags(0x0L,0x0L,TR3_TELEPATHY,0x0L);
#endif
		}

		/* Magical sensing */
		if (((p_ptr->cur_flags3 & (TR3_ESP_ORC)) != 0) && (r_ptr->flags3 & (RF3_ORC)))
		{
			flag = TRUE;
			l_ptr->flags3 |= (RF3_ORC);

#ifdef ALLOW_OBJECT_INFO_MORE
			/* Visible */
			equip_can_flags(0x0L,0x0L,TR3_ESP_ORC,0x0L);
#endif
		}

		/* Magical sensing */
		if (((p_ptr->cur_flags3 & (TR3_ESP_GIANT)) != 0) && (r_ptr->flags3 & (RF3_GIANT)))
		{
			flag = TRUE;
			l_ptr->flags3 |= (RF3_GIANT);

#ifdef ALLOW_OBJECT_INFO_MORE
			/* Visible */
			equip_can_flags(0x0L,0x0L,TR3_ESP_GIANT,0x0L);
#endif
		}

		/* Magical sensing */
		if (((p_ptr->cur_flags3 & (TR3_ESP_TROLL)) != 0) && (r_ptr->flags3 & (RF3_TROLL)))
		{
			flag = TRUE;
			l_ptr->flags3 |= (RF3_TROLL);

#ifdef ALLOW_OBJECT_INFO_MORE
			/* Visible */
			equip_can_flags(0x0L,0x0L,TR3_ESP_TROLL,0x0L);
#endif
		}

		/* Magical sensing */
		if (((p_ptr->cur_flags3 & (TR3_ESP_DRAGON)) != 0) && (r_ptr->flags3 & (RF3_DRAGON)))
		{
			flag = TRUE;
			l_ptr->flags3 |= (RF3_DRAGON);

#ifdef ALLOW_OBJECT_INFO_MORE
			/* Visible */
			equip_can_flags(0x0L,0x0L,TR3_ESP_DRAGON,0x0L);
#endif
		}

		/* Magical sensing */
		if (((p_ptr->cur_flags3 & (TR3_ESP_DRAGON)) != 0) && (r_ptr->flags3 & (RF3_DEMON)))
		{
			flag = TRUE;
			l_ptr->flags3 |= (RF3_DEMON);

#ifdef ALLOW_OBJECT_INFO_MORE
			/* Visible */
			equip_can_flags(0x0L,0x0L,TR3_ESP_DEMON,0x0L);
#endif
		}

		/* Magical sensing */
		if (((p_ptr->cur_flags3 & (TR3_ESP_UNDEAD)) != 0) && (r_ptr->flags3 & (RF3_UNDEAD)))
		{
			flag = TRUE;
			l_ptr->flags3 |= (RF3_UNDEAD);

#ifdef ALLOW_OBJECT_INFO_MORE
			/* Visible */
			equip_can_flags(0x0L,0x0L,TR3_ESP_UNDEAD,0x0L);
#endif
		}

		/* Magical sensing */
		if (((p_ptr->cur_flags3 & (TR3_ESP_NATURE)) != 0) && (r_ptr->flags3 & (RF3_ANIMAL | RF3_PLANT | RF3_INSECT)))
		{
			flag = TRUE;
			if (r_ptr->flags3 & (RF3_ANIMAL)) l_ptr->flags3 |= (RF3_ANIMAL);
			if (r_ptr->flags3 & (RF3_PLANT)) l_ptr->flags3 |= (RF3_PLANT);
			if (r_ptr->flags3 & (RF3_INSECT)) l_ptr->flags3 |= (RF3_INSECT);

#ifdef ALLOW_OBJECT_INFO_MORE
			/* Visible */
			equip_can_flags(0x0L,0x0L,TR3_ESP_NATURE,0x0L);
#endif
		}

		/* Style sensing */
		if ((p_ptr->pstyle == WS_SLAY_ORC) && (r_ptr->flags3 & (RF3_ORC))
		 && ((m_idx % 10) == 5))
		{
			flag = TRUE;

			l_ptr->flags3 |= (RF3_ORC);
		}

		/* Style sensing */
		if ((p_ptr->pstyle == WS_SLAY_GIANT) && (r_ptr->flags3 & (RF3_GIANT))
		 && ((m_idx % 10) == 5))
		{
			flag = TRUE;

			l_ptr->flags3 |= (RF3_GIANT);
		}

		/* Style sensing */
		if ((p_ptr->pstyle == WS_SLAY_TROLL) && (r_ptr->flags3 & (RF3_TROLL))
		 && ((m_idx % 10) == 5))
		{
			flag = TRUE;

			l_ptr->flags3 |= (RF3_TROLL);

		}

		/* Style sensing */
		if ((p_ptr->pstyle == WS_SLAY_DRAGON) && (r_ptr->flags3 & (RF3_DRAGON))
		 && ((m_idx % 10) == 5))
		{
			flag = TRUE;

			l_ptr->flags3 |= (RF3_DRAGON);

		}

		/* Style sensing */
		if ((p_ptr->pstyle == WS_SLAY_DEMON) && (r_ptr->flags3 & (RF3_DEMON))
		 && ((m_idx % 10) == 5))
		{
			flag = TRUE;

			l_ptr->flags3 |= (RF3_DEMON);
		}


		/* Style sensing */
		if ((p_ptr->pstyle == WS_SLAY_UNDEAD) && (r_ptr->flags3 & (RF3_UNDEAD))
		 && ((m_idx % 10) == 5))
		{
			flag = TRUE;

			l_ptr->flags3 |= (RF3_UNDEAD);

		}

		/* Style sensing */
		if ((p_ptr->pstyle == WS_SLAY_ANIMAL) && (r_ptr->flags3 & (RF3_ANIMAL))
		 && ((m_idx % 10) == 5))
		{
			flag = TRUE;

			l_ptr->flags3 |= (RF3_ANIMAL);
		}

		/* Style sensing */
		if ((p_ptr->pstyle == WS_SLAY_EVIL) && (r_ptr->flags3 & (RF3_EVIL))
		 && ((m_idx % 10) == 5))
		{
			flag = TRUE;

			l_ptr->flags3 |= (RF3_EVIL);
		}

		/* Normal line of sight, and not blind */
		if ((!p_ptr->blind) && (player_has_los_bold(fy, fx)))
		{
			bool do_invisible = FALSE;
			bool do_cold_blood = FALSE;
#ifdef ALLOW_OBJECT_INFO_MORE
			bool do_warm_blood = FALSE;
#endif
			/* Hidden */
			if (m_ptr->mflag & (MFLAG_HIDE))
			{
			}
			/* Use "infravision" */
			else if (d <= p_ptr->see_infra)
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
#ifdef ALLOW_OBJECT_INFO_MORE
					/* Take note */
					do_warm_blood = TRUE;
#endif

#ifdef ALLOW_OBJECT_INFO_MORE
					if (!flag && !(r_ptr->flags2 & (RF2_EMPTY_MIND | RF2_WEIRD_MIND))) equip_not_flags(0x0L,0x0L,TR3_TELEPATHY,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_ORC))) equip_not_flags(0x0L,0x0L,TR3_ESP_ORC,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_TROLL))) equip_not_flags(0x0L,0x0L,TR3_ESP_TROLL,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_GIANT))) equip_not_flags(0x0L,0x0L,TR3_ESP_GIANT,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_DRAGON))) equip_not_flags(0x0L,0x0L,TR3_ESP_DRAGON,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_DEMON))) equip_not_flags(0x0L,0x0L,TR3_ESP_DEMON,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_UNDEAD))) equip_not_flags(0x0L,0x0L,TR3_ESP_UNDEAD,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_ANIMAL | RF3_PLANT | RF3_INSECT))) equip_not_flags(0x0L,0x0L,TR3_ESP_NATURE,0x0L);
#endif
					/* Easy to see */
					easy = flag = TRUE;
				}
			}

			/* Use "illumination" */
			if (player_can_see_bold(fy, fx))
			{
				/* Hidden */
				if (m_ptr->mflag & (MFLAG_HIDE))
				{
				}

				/* Handle "invisible" monsters */
				else if ((r_ptr->flags2 & (RF2_INVISIBLE)) || (m_ptr->tim_invis))
				{
					/* Take note */
					do_invisible = TRUE;

					/* See invisible */
					if ((p_ptr->cur_flags3 & (TR3_SEE_INVIS)) != 0)
					{

#ifdef ALLOW_OBJECT_INFO_MORE
						if (!flag && !(r_ptr->flags2 & (RF2_EMPTY_MIND | RF2_WEIRD_MIND)) &&
							!(l_ptr->flags2 & (RF2_EMPTY_MIND | RF2_WEIRD_MIND))) equip_not_flags(0x0L,0x0L,TR3_TELEPATHY,0x0L);

						if (!flag && (l_ptr->flags3 & (RF3_ORC))) equip_not_flags(0x0L,0x0L,TR3_ESP_ORC,0x0L);
						if (!flag && (l_ptr->flags3 & (RF3_TROLL))) equip_not_flags(0x0L,0x0L,TR3_ESP_TROLL,0x0L);
						if (!flag && (l_ptr->flags3 & (RF3_GIANT))) equip_not_flags(0x0L,0x0L,TR3_ESP_GIANT,0x0L);
						if (!flag && (l_ptr->flags3 & (RF3_DRAGON))) equip_not_flags(0x0L,0x0L,TR3_ESP_DRAGON,0x0L);
						if (!flag && (l_ptr->flags3 & (RF3_DEMON))) equip_not_flags(0x0L,0x0L,TR3_ESP_DEMON,0x0L);
						if (!flag && (l_ptr->flags3 & (RF3_UNDEAD))) equip_not_flags(0x0L,0x0L,TR3_ESP_UNDEAD,0x0L);
						if (!flag && (l_ptr->flags3 & (RF3_ANIMAL | RF3_PLANT | RF3_INSECT))) equip_not_flags(0x0L,0x0L,TR3_ESP_NATURE,0x0L);
#endif
						/* Easy to see */
						easy = flag = TRUE;
					}
				}

				/* Handle "normal" monsters */
				else
				{
#ifdef ALLOW_OBJECT_INFO_MORE
					if (!flag && !(r_ptr->flags2 & (RF2_EMPTY_MIND | RF2_WEIRD_MIND)) &&
						!(l_ptr->flags2 & (RF2_EMPTY_MIND | RF2_WEIRD_MIND))) equip_not_flags(0x0L,0x0L,TR3_TELEPATHY,0x0L);

					if (!flag && (l_ptr->flags3 & (RF3_ORC))) equip_not_flags(0x0L,0x0L,TR3_ESP_ORC,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_TROLL))) equip_not_flags(0x0L,0x0L,TR3_ESP_TROLL,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_GIANT))) equip_not_flags(0x0L,0x0L,TR3_ESP_GIANT,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_DRAGON))) equip_not_flags(0x0L,0x0L,TR3_ESP_DRAGON,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_DEMON))) equip_not_flags(0x0L,0x0L,TR3_ESP_DEMON,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_UNDEAD))) equip_not_flags(0x0L,0x0L,TR3_ESP_UNDEAD,0x0L);
					if (!flag && (l_ptr->flags3 & (RF3_ANIMAL | RF3_PLANT | RF3_INSECT))) equip_not_flags(0x0L,0x0L,TR3_ESP_NATURE,0x0L);
#endif

					/* Easy to see */
					easy = flag = TRUE;
				}
			}

			/* Visible */
			if (flag)
			{
				/* Memorize flags */
				if (do_invisible)
				{
					if (r_ptr->flags2 & RF2_INVISIBLE) l_ptr->flags2 |= (RF2_INVISIBLE);
#ifdef ALLOW_OBJECT_INFO_MORE
					if ((p_ptr->cur_flags3 & (TR3_SEE_INVIS)) != 0) equip_can_flags(0x0L,0x0L,TR3_SEE_INVIS,0x0L);
#endif
				}
				if (do_cold_blood) l_ptr->flags2 |= (RF2_COLD_BLOOD);
#ifdef ALLOW_OBJECT_INFO_MORE
				if (do_warm_blood)
				{
					if (rp_ptr->infra < d) equip_can_flags(TR1_INFRA,0x0L,0x0L,0x0L);
				}
#endif
				if (r_ptr->flags2 & (RF2_HAS_LITE)) l_ptr->flags2 |= (RF2_HAS_LITE);

			}
		}
	}


	/* The monster is now visible */
	if (flag)
	{
		/* It was previously unseen */
		if (!m_ptr->ml)
		{
			/* Mark as visible */
			m_ptr->ml = TRUE;

			/* Draw the monster */
			lite_spot(fy, fx);

			/* Update health bar as needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Mega Hack -- If not seen before, disturb */
			if ((!l_ptr->sights) && (disturb_new)) disturb(1, 0);

			/* Hack -- Count "fresh" sightings */
			if (l_ptr->sights < MAX_SHORT) l_ptr->sights++;

			/* Disturb on visibility change */
			if (disturb_move) disturb(1, 0);

			/* Window stuff */
			p_ptr->window |= PW_MONLIST;
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
			lite_spot(fy, fx);

			/* Update health bar as needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Disturb on visibility change */
			if (disturb_move) disturb (1, 0);

			/* Window stuff */
			p_ptr->window |= PW_MONLIST;
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

			/* Disturb on visibility change */
			if (disturb_near) disturb(1, 0);
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

			/* Disturb on visibility change */
			if (disturb_near) disturb(1, 0);
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
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Update the monster */
		update_mon(i, full);
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
		o_ptr->ident &= ~(IDENT_MARKED);

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
 *  Helper function for monster swap. Update player based on moving y1, x1 to y2, x2
 */
static void player_swap(const int y1, const int x1, const int y2, const int x2)
{
	int by1 = y1/BLOCK_HGT;
	int bx1 = x1/BLOCK_WID;
	int by2 = y2/BLOCK_HGT;
	int bx2 = x2/BLOCK_WID;

	bool outside;

	feature_type *f_ptr = &f_info[cave_feat[y2][x2]];

	/* Move player */
	p_ptr->py = y2;
	p_ptr->px = x2;

	/* Update the panel */
	p_ptr->update |= (PU_PANEL);

	/* Update the visuals (and monster distances) */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

	/* Update the bonuses -- due to mud etc */
	p_ptr->update |= (PU_BONUS | PU_RUNES); 

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Update room description (if needed) */
	/* Line 1 -- we are entering a room */
	/* Line 2 -- which is different from the last room */
	/* Line 3 -- or we were not in a room */
	/* Line 4 -- or we move to a lit portion of the room */
	if ((cave_info[y2][x2] & (CAVE_ROOM)) &&
	 ((dun_room[by1][bx1] != dun_room[by2][bx2]) ||
		!(cave_info[y1][x1] & (CAVE_ROOM)) ||
		((cave_info[y2][x2] & (CAVE_GLOW)) != (cave_info[y1][x1] & (CAVE_GLOW))) ))
	{
		p_ptr->window |= (PW_ROOM_INFO);
		p_ptr->update |= (PU_ROOM_INFO);

		/* Room is perma-lit */
		if (cave_info[y2][x2] & (CAVE_GLOW)) room_info[dun_room[by2][bx2]].flags |= (ROOM_SEEN);
	}

	/* Update view if moved outside/inside */
	outside = ((p_ptr->depth == min_depth(p_ptr->dungeon)) && 
		(f_ptr->flags3 & (FF3_OUTSIDE)));

	/* Changed inside/outside */
	if (outside != p_ptr->outside)
	{
		p_ptr->redraw |= (PR_MAP);
	}

	/* Change state */
	p_ptr->outside = outside;

	/* Hack -- display 'furnishings' */
	if (((f_ptr->flags3 & (FF3_ALLOC)) != 0) &&
		((f_ptr->flags1 & (FF1_TRAP)) == 0))
	{
		msg_format("You %s %s %s.", p_ptr->blind || no_lite()? "feel" : "see", is_a_vowel((f_name + f_ptr->name)[0]) ? "an" : "a",
			f_name + f_ptr->name);

		play_info[y2][x2] |= (PLAY_MARK);

		lite_spot(y2, x2);
	}

	/* If blind, silently notice what the player is on */
	else if ((p_ptr->blind || no_lite()) && ((play_info[y2][x2] & (PLAY_MARK)) == 0) &&
		((f_info[cave_feat[y2][x2]].flags1 & (FF1_NOTICE)) != 0))
	{
		play_info[y2][x2] |= (PLAY_MARK);

		lite_spot(y2, x2);
	}
}


/*
 * Swap the players/monsters (if any) at two locations XXX XXX XXX
 */
void monster_swap(int y1, int x1, int y2, int x2)
{
	int m1, m2;

	monster_type *m_ptr;
	monster_race *r_ptr;

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
		r_ptr = &r_info[m_ptr->r_idx];

		/* Move monster */
		m_ptr->fy = y2;
		m_ptr->fx = x2;

		/* Some monsters radiate lite when moving */
		if (r_ptr->flags2 & (RF2_HAS_LITE | RF2_NEED_LITE))
		{
			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
		}

		/* Some monsters radiate damage when moving */
		if (r_ptr->flags2 & (RF2_HAS_AURA))
		{
			int flg = (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_PLAY |
			PROJECT_HIDE | PROJECT_WALL);

			/* The target is attacked by a ball attack */
			(void)mon_blow_ranged(m1, y2, x2, RBM_AURA, 2, flg, NULL);
		}

		/* Some monsters trail damage when moving */
		else if (r_ptr->flags2 & (RF2_TRAIL))
		{
			int flg = (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_PLAY |
			PROJECT_HIDE | PROJECT_WALL);

			/* The target is attacked by a ball attack */
			(void)mon_blow_ranged(m1, y2, x2, RBM_TRAIL, 0, flg, NULL);
		}

		/* Update monster */
		update_mon(m1, TRUE);
	}

	/* Player 1 */
	else if (m1 < 0)
	{
		player_swap(y1, x1, y2, x2);
	}

	/* Monster 2 */
	if (m2 > 0)
	{
		m_ptr = &m_list[m2];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Move monster */
		m_ptr->fy = y1;
		m_ptr->fx = x1;


		/* Some monsters radiate lite when moving */
		if (r_ptr->flags2 & (RF2_HAS_LITE | RF2_NEED_LITE))
		{
			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
		}

		/* Some monsters radiate damage when moving */
		if (r_ptr->flags2 & (RF2_HAS_AURA))
		{
			int flg = (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_PLAY |
			PROJECT_HIDE | PROJECT_WALL);

			/* The target is attacked by a ball attack */
			(void)mon_blow_ranged(m2, y1, x1, RBM_AURA, 2, flg, NULL);
		}

		/* Some monsters trail damage when moving */
		else if (r_ptr->flags2 & (RF2_TRAIL))
		{
			int flg = (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_PLAY |
			PROJECT_HIDE | PROJECT_WALL);

			/* The target is attacked by a ball attack */
			(void)mon_blow_ranged(m2, y1, x1, RBM_TRAIL, 0, flg, NULL);
		}

		/* Update monster */
		update_mon(m2, TRUE);
	}

	/* Player 2 */
	else if (m2 < 0)
	{
		player_swap(y2, x2, y1, x1);
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

	/* Update view if moved outside/inside */
	p_ptr->outside = ((p_ptr->depth == min_depth(p_ptr->dungeon)) && 
			(f_info[cave_feat[p_ptr->py][p_ptr->px]].flags3 & (FF3_OUTSIDE)));

	/* Success */
	return (-1);
}

/* XXX XXX Checking by blow method is broken currently, because we
 * historically damage monsters even when they are immune to fire etc. So we
 * should have an advanced flow by threat level for monsters. The borg does this
 * effectively, but is somewhat computationally expensive and still isn't
 * perfect. Besides, implementing borg code for monsters is Angband 3.0.0 work.
 *
 * Instead, we rely on marking the level whenever a particular
 * terrain type is generated on that level. Then we prefer to generate monsters
 * on that level that have immunity to that particular terrain type(s). Even
 * that is a bit broken because some immunities rely on knowing the prefix
 * to a monster flag, a) we don't want to restrict monster types too heavily.
 * ie water and lava, require "magma" and "water" elementals to have correct
 * immunity and b) checking for a prefix all the time would add to the CPU
 * flow cost.
 *
 * So we give monsters 'fake' immunity.
 * All swimming monsters and digging monsters, while swimming or digging are
 * IM_WATER.
 * All IM_FIRE swimming monsters are IM_BWATER.
 * All IM_FIRE digging monsters are IM_BMUD.
 * All BR_FIRE monsters are IM_LAVA.
 * All NEVER_MOVE monsters ignore falling damage from pits (But not trap doors
 * /falls).
 * All NONLIVING monsters are IM_WATER.
 * All NONLIVING IM_FIRE monsters are IM_BWATER and IM_BMUD.
 *
 * Note the difference between NONLIVING and CAN_SWIM/CAN_DIG monsters is that
 * non-living monsters are unable to 'pop-up' and attack or cast spells while
 * in deep terrain. They are instead silently 'pushed over'.
 *
 * We must make sure that we never apply blows to monsters from terrain
 * that qualify for fake immunity unless we need to (Say from a trap).
 *
 * This has the downside that we can allow a player to manipulate how monsters
 * are generated and move through judicious use of the flow options. This has
 * always been the case however. To minimise the risk of outright manipulation
 * we should confuse a monster whenever it is stuck in terrain it can't seem to
 * move out of. This is better than any attempt to use a flow algorithm that
 * is not sufficiently smart.
 *
 * By not sufficiently smart, we mean not taking in account of a case such as
 * being smartest to flow across lava to get to water which we can swim in
 * to attack the player in cases for creatures that are both capable and not
 * capable of traversing both, one or neither type of terrain.
 *
 * And don't use this an argument against terrain, because we fail to handle
 * exactly the same type of cases for doors and/or permanent features in
 * vanilla Angband. Witness the grief caused by vaults on levels with Morgoth
 * and numerous player exploits possible under this scenario.
 *
 * We then stop monsters flowing that can't flow through all non-wall terrain
 * on the level. This is currently unimplemented.
 *
 * XXX We should risk traps based on monster hp.
 */

bool mon_resist_feat(int feat, int r_idx)
{
	feature_type *f_ptr;
	monster_race *r_ptr;

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

	bool daytime = ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2));

	bool outside = ((f_info[feat].flags3 & (FF3_OUTSIDE)) ? TRUE : FALSE);

	/* Get feature info */
	f_ptr= &f_info[feat];
	
	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Race */
	r_ptr = &r_info[r_idx];

	/* Always get burnt by daylight */
	if ((surface && daytime && outside) && (r_ptr->flags3 & (RF3_HURT_LITE))) return (FALSE);

	/* Always risk traps if stupid */
	if ((f_ptr->flags1 & (FF1_HIT_TRAP)) &&
	    (r_ptr->flags2 & (RF2_STUPID))) return (TRUE);

	/* Always fail for spells */
	if (f_ptr->spell) return (FALSE);

	/* Sometimes risk traps if not smart */
	if ((f_ptr->flags1 & (FF1_HIT_TRAP)) &&
	    !(r_ptr->flags2 & (RF2_SMART)) &&
	    (rand_int(100)<5)) return (TRUE);

	/* Check trap/feature attack */
	if (f_ptr->blow.method)
	{
		switch (f_ptr->blow.effect)
		{

			case GF_ICE:
			case GF_HURT:
			case GF_UN_BONUS:
			case GF_UN_POWER:
			case GF_EAT_GOLD:
			case GF_EAT_ITEM:
			case GF_EAT_FOOD:
			case GF_EAT_LITE:
			return ((f_ptr->blow.d_dice * f_ptr->blow.d_side)==0);
			break;

			case GF_POIS:
			if (!(r_ptr->flags3 & (RF3_IM_POIS))) return (FALSE);
			break;

			case GF_ACID:
			case GF_VAPOUR:
			if (!(r_ptr->flags3 & (RF3_IM_ACID))) return (FALSE);
			break;

			case GF_ELEC:
			if (!(r_ptr->flags3 & (RF3_IM_ELEC))) return (FALSE);
			break;

			case GF_FIRE:
			case GF_SMOKE:
			if (!(r_ptr->flags3 & (RF3_IM_FIRE))) return (FALSE);
			break;

			case GF_LAVA:
			if (!(r_ptr->flags3 & (RF3_RES_LAVA))) return (FALSE);
			break;

			case GF_WATER_WEAK:
			case GF_WATER:
			if (r_ptr->flags3 & (RF3_NONLIVING))  return (TRUE);
			if ((r_ptr->flags2 & (RF2_CAN_SWIM)) && (f_ptr->flags2 & (FF2_CAN_SWIM)))
			{
				return (TRUE);
			}
			else if ((r_ptr->flags2 & (RF2_CAN_DIG)) && (f_ptr->flags2 & (FF2_CAN_DIG)))
			{
				return (TRUE);
			}
			else
			{
				return (FALSE);
			}

			break;

			case GF_BWATER:
			case GF_STEAM:
			if ((r_ptr->flags3 & (RF3_NONLIVING)) && (r_ptr->flags3 & (RF3_IM_FIRE))) return (TRUE); 
			if ((r_ptr->flags2 & (RF2_CAN_SWIM)) && (f_ptr->flags2 & (FF2_CAN_SWIM)))
			{
				if (!(r_ptr->flags3 & (RF3_IM_FIRE))) return (FALSE);
			}
			else
			{
				return(FALSE);
			}
			break;

			case GF_BMUD:
			if ((r_ptr->flags3 & (RF3_NONLIVING))&& (r_ptr->flags3 & (RF3_IM_FIRE))) return (TRUE); 
			if ((r_ptr->flags2 & (RF2_CAN_DIG)) && (f_ptr->flags2 & (FF2_CAN_DIG)))
			{
				if (!(r_ptr->flags3 & (RF3_IM_FIRE))) return (FALSE);
			}
			else
			{
				return(FALSE);
			}
			break;

			case GF_COLD:
			if (!(r_ptr->flags3 & (RF3_IM_COLD))) return (FALSE);
			break;

			case GF_BLIND:
			case GF_CONFUSION:
			if (!(r_ptr->flags3 & (RF3_NO_CONF))) return (FALSE);
			break;

			case GF_TERRIFY:
			if (!(r_ptr->flags3 & (RF3_NO_FEAR))) return (FALSE);
			break;

			case GF_PARALYZE:
			if (!(r_ptr->flags3 & (RF3_NO_SLEEP))) return (FALSE);
			break;

			case GF_LOSE_STR:
			case GF_LOSE_INT:
			case GF_LOSE_WIS:
			case GF_LOSE_DEX:
			case GF_LOSE_CON:
			case GF_LOSE_CHR:
			case GF_LOSE_ALL:
			case GF_EXP_10:
			case GF_EXP_20:
			case GF_EXP_40:
			case GF_EXP_80:
			if (!(r_ptr->flags3 & (RF3_UNDEAD))) return (FALSE);
			break;

			case GF_SHATTER:
			return (FALSE);
			break;

			case GF_NOTHING:
			break;

			case GF_FALL_MORE:
			if (!(r_ptr->flags2 & (RF2_CAN_FLY))) return (FALSE);
			break;

			case GF_FALL:
			case GF_FALL_SPIKE:
			case GF_FALL_POIS:
			if (!(r_ptr->flags1 & (RF1_NEVER_MOVE))) return (FALSE);
			break;


			default:
			return (FALSE);
			break;
		}

	}


	return(TRUE);
}

/* ANDY - The routine we use to determine if we can place a monster
 * on a particular terrain type. This returns an integer defining
 * what mode of movement the monster is using (climbing, swimming etc.)
 * or a FALSE result if the monster cannot move (at all) on this terrain.
 *
 * Note the integer is negative if the monster doesn't 'like' the terrain.
 */
int place_monster_here(int y, int x, int r_idx)
{
	int feat;

	feature_type *f_ptr;
	monster_race *r_ptr;

	/* Improve efficiency -- forced boolean */
	bool resist;

        /* Get feature */
	feat = cave_feat[y][x];

	/* Get feature info */
	f_ptr= &f_info[cave_feat[y][x]];

	/* Paranoia */
	if (!r_idx) return (MM_FAIL);

	/* Race */
	r_ptr = &r_info[r_idx];

	/* Monster resists? */
	resist = mon_resist_feat(feat,r_idx);

	/* Hack -- Player traps */
	if ((cave_o_idx[y][x]) && (f_info[feat].flags1 & (FF1_HIT_TRAP))) resist = TRUE;

	/* Check for pass wall */
	if (resist &&
		(r_ptr->flags2 & (RF2_PASS_WALL))) return (MM_PASS);

	/* Check for swimming */
	if (resist &&
		(r_ptr->flags2 & (RF2_CAN_SWIM | RF2_MUST_SWIM)) &&
		(f_ptr->flags2 & (FF2_CAN_SWIM)))
	{
		return(MM_SWIM);
	}
	else if (r_ptr->flags2 & (RF2_MUST_SWIM))
	{
		return(MM_DROWN);
	}

	/* Check for digging */
	if (resist &&
		(r_ptr->flags2 & (RF2_CAN_DIG)) &&
		(f_ptr->flags2 & (FF2_CAN_DIG)))
	{
		return(MM_DIG);
	}

	/* Check if we don't need to breath -- note move check because we now mark walls as 'filled' */
	if (resist &&
		(r_ptr->flags3 & (RF3_NONLIVING)) &&
		(f_ptr->flags1 & (FF1_MOVE)) &&
                (f_ptr->flags2 & (FF2_DEEP | FF2_FILLED)))
	{
		return (MM_UNDER);
	}

	/* Hack -- check for oozing */
	if (resist &&
		(r_ptr->flags3 & (RF3_OOZE)) &&
		(f_ptr->flags2 & (FF2_CAN_OOZE)))
	{
		return(MM_OOZE);
	}


	/* Hack -- check for flying. */
	if ((r_ptr->flags2 & (RF2_CAN_FLY | RF2_MUST_FLY)) &&
		(f_ptr->flags2 & (FF2_CAN_FLY)))
	{
		return(MM_FLY);
	}

	else if (r_ptr->flags2 & (RF2_MUST_FLY))
	{
		return(MM_DROWN);
	}

	/* Hack -- check for climbing */
	if ((r_ptr->flags2 & (RF2_CAN_CLIMB)) && 
		(f_ptr->flags3 & (FF3_EASY_CLIMB)))
	{
		return(MM_CLIMB);
	}

	/* Hack -- check for climbing. */
	if ((r_ptr->flags2 & (RF2_CAN_CLIMB)) && 
		(f_ptr->flags2 & (FF2_CAN_FLY)))
	{
		int i;

		for (i=0;i<8;i++)
		{
			int d,yi,xi;
			
			d = ddd[i];
			yi = ddy[d];
			xi = ddx[d];

			if (f_info[cave_feat[yi][xi]].flags2 & (FF2_CAN_CLIMB)) return(MM_CLIMB);
		}

	}

	/* Get mimiced feat if covered/bridged */
	if ((f_ptr->flags2 & (FF2_COVERED)) || (f_ptr->flags2 & (FF2_BRIDGED)))
	{
		feat = f_ptr->mimic;
		resist = mon_resist_feat(feat,r_idx);
	}

	/* Regular move/climb/drown */
	if ((f_ptr->flags1 & (FF1_MOVE)) || (f_ptr->flags3 & (FF3_EASY_CLIMB)))
	{
		if (!resist) return (MM_DROWN);
		if (f_ptr->flags2 & (FF2_DEEP | FF2_FILLED)) return (MM_DROWN);
		if (!(f_ptr->flags1 & (FF1_MOVE))) return (MM_CLIMB);
		
		return (MM_WALK);
	}

	return(MM_FAIL);

}

/*
 * Hide a monster in terrain or position it over terrain as necessary
 */
void monster_hide(int y, int x, int mmove, monster_type *m_ptr)
{
	/* Hack -- don't summon on surface */
	bool surface = p_ptr->depth == min_depth(p_ptr->dungeon);

	/* Get the feature */
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

        /* Get the race */
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Over the ceiling of a building */
	if ((m_ptr->mflag & (MFLAG_OVER)) &&
		(m_ptr->mflag & (MFLAG_HIDE)))
	{
		/* Don't change state or update further */
		return;
	}

	/* Update flags */
	if (((m_ptr->mflag & (MFLAG_OVER))==0)
	       && !(f_ptr->flags1 & (FF1_MOVE))
		   && !(f_ptr->flags3 & (FF3_EASY_CLIMB))
	       && (mmove != MM_PASS))
	{
		/* Nothing -- we are stuck inside a terrain feature */
	}
	else if ((f_ptr->flags3 & (FF3_EASY_CLIMB)))
	{
		m_ptr->mflag |= (MFLAG_OVER);
	}
	else if ((mmove == MM_FLY) || (mmove == MM_CLIMB))
	{
		m_ptr->mflag |= (MFLAG_OVER);
	}
	else if (!(surface) || (f_ptr->flags3 & (FF3_OUTSIDE)))
	{
		m_ptr->mflag &= ~(MFLAG_OVER);
	}

	/* Never hide if over terrain, except on surface, and outside */
	if (m_ptr->mflag & (MFLAG_OVER))
	{
		m_ptr->mflag &= ~(MFLAG_HIDE);
	}
	/* Never hide in terrain we don't resist */
	else if (!mon_resist_feat(cave_feat[y][x],m_ptr->r_idx))
	{
		m_ptr->mflag &= ~(MFLAG_HIDE);
	}

	/* Hack -- cannot hide in open air */
	else if (cave_feat[y][x] == FEAT_CHASM)
	{
		m_ptr->mflag &= ~(MFLAG_HIDE);
	}

	/* Set hide flag if passing through floor/ceiling (phasing) */
	else if (mmove == MM_PASS)
	{
		m_ptr->mflag |= (MFLAG_HIDE);
	}

	/* Maintain hidden/unhidden state if covered */
	else if (f_ptr->flags2 & (FF2_COVERED))
	{
		/* Nothing */
	}

	/* Set hide flag if HIDE_SNEAK and monster is sneaky */
	else if ((f_ptr->flags2 & (FF2_HIDE_SNEAK))
		&& (r_ptr->flags2 & (RF2_SNEAKY))
		&& !(m_ptr->mflag & (MFLAG_OVER)) )
	{
		m_ptr->mflag |= (MFLAG_HIDE);
	}
	/* Set hide flag if digging and HIDE_DIG */
	else if ((f_ptr->flags2 & (FF2_HIDE_DIG)) && (mmove == MM_DIG))
	{
		m_ptr->mflag |= (MFLAG_HIDE);
	}
	/* Set hide flag if swimming and HIDE_SWIM */
	else if ((f_ptr->flags2 & (FF2_HIDE_SWIM)) && (mmove == MM_SWIM))
	{
		m_ptr->mflag |=(MFLAG_HIDE);
	}
	/* Set hide flag if EASY_HIDE, with conditions */
	else if (f_ptr->flags3 & (FF3_EASY_HIDE))
	{
		/* Covered/bridged features are special */
                if (f_ptr->flags2 & (FF2_BRIDGED))
		{
			/* Nothing */
		}
		else
		{
			m_ptr->mflag |=(MFLAG_HIDE);
		}
	}
	else
	{
		m_ptr->mflag &= ~(MFLAG_HIDE);
	}

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

		/* Get the new monster */
		m_ptr = &m_list[m_idx];

		/* Copy the monster XXX */
		COPY(m_ptr, n_ptr, monster_type);

		/* Location */
		m_ptr->fy = y;
		m_ptr->fx = x;

		/* Clear flags */
		m_ptr->mflag &= ~(MFLAG_OVER | MFLAG_HIDE);

		/* Place as hidden as appropriate */
		monster_hide(y, x, (place_monster_here(y,x,m_ptr->r_idx) > 0), m_ptr);

		/* Update the monster */
		update_mon(m_idx, TRUE);

		/* Get the new race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Hack -- Notice new multi-hued monsters */
		if ((r_ptr->flags1 & (RF1_ATTR_MULTI)) || !(r_ptr->flags9 & (RF9_ATTR_METAL))) shimmer_monsters = TRUE;

		/* Hack -- Count the number of "reproducers" */
		if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;

		/* Count racial occurances */
		r_ptr->cur_num++;
	}

	/* Result */
	return (m_idx);
}


/*
 *  Calculate monster armour class. Takes into account temporary conditions and stats.
 *
 *  We return two different values, depending on ranged or melee ac. Temporary
 *  shield spells and monsters with shields have twice as much effect at range.
 */
int calc_monster_ac(const monster_type *m_ptr, bool ranged)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int ac;

	/* Get the base ac */
	ac = r_ptr->ac;

	/* Modify by dexterity */
	if (m_ptr->mflag & (MFLAG_CLUMSY)) ac -= 5;
	else if (m_ptr->mflag & (MFLAG_SKILLFUL)) ac += 5;

	/* Hack -- Armoured monsters carry shields at range */
	if ((r_ptr->flags2 & (RF2_ARMOR)) && (ranged))
	{
		/* Notice this */
		if ((m_ptr->ml) && (!l_ptr->flags2 & (RF2_ARMOR))) l_ptr->flags2 |= (RF2_ARMOR);

		/* Hack -- assume a shield provides 1/4 of overall base ac */
		ac += r_ptr->ac / 4;
	}

	/* Shield spell counts for double at range */
	if ((m_ptr->shield) && (ranged)) ac += 100;
	else if (m_ptr->shield) ac += 50;

	/* Modify by temporary conditions */
	if (m_ptr->berserk) ac -= 10;
	if (m_ptr->bless) ac += 5;
	if (m_ptr->stunned) ac -= 10;
	if (m_ptr->blind) ac -= 10;

	if (ac <= 0) ac = 1;

	return(ac);
}


/*
 *  Calculate monster maximum hit points.
 */
int calc_monster_hp(const monster_type *m_ptr)
{
	int hp;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Assign maximal hitpoints */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
	{
		hp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		hp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* Hack -- Scale down hit points by monster armour */
	hp -= (hp * ((r_ptr->ac < 150) ? r_ptr->ac : 150) / 250);

	/* Adjust for monster constitution stat */
	if (m_ptr->mflag & (MFLAG_SICK)) hp = hp * 9 / 10;
	else if (m_ptr->mflag & (MFLAG_HEALTHY)) hp = hp * 11 / 10;

	if (hp < 1) hp = 1;

	return(hp);
}


/* Calculate the monster_speed of a monster */
byte calc_monster_speed(const monster_type *m_ptr)
{
	int speed, i;

	/* Get the monster race */
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster has same speed as player? */
	if (r_ptr->flags9 & (RF9_SAME_SPEED))
	{
		return (p_ptr->pspeed);
	}

	/* Get the monster base speed */
	speed = r_ptr->speed;

	/*note: a monster should only have one of these flags*/
	if (m_ptr->mflag & (MFLAG_SLOW))
	{
		/* Allow some small variation each time to make pillar dancing harder */
		i = extract_energy[r_ptr->speed] / 10;
		speed -= rand_spread(0, i);
	}
	else if (m_ptr->mflag & (MFLAG_FAST))
	{
		/* Allow some small variation each time to make pillar dancing harder */
		i = extract_energy[r_ptr->speed] / 10;
		speed += rand_spread(0, i);
	}

	/* Factor in the hasting and slowing counters*/
	if (m_ptr->hasted) speed += 10;
	if (m_ptr->slowed) speed -= 10;

	return (speed);
}

void set_monster_haste(s16b m_idx, s16b counter, bool message)
{
	/*get the monster at the given location*/
	monster_type *m_ptr = &m_list[m_idx];

	bool recalc = FALSE;

	char m_name[80];

	/* Get monster name*/
	monster_desc(m_name, m_ptr, 0);

	/*see if we need to recalculate speed*/
	if (m_ptr->hasted)
	{
		/*monster is no longer hasted and speed needs to be recalculated*/
		if (counter == 0)
		{
			recalc = TRUE;

			/*give a message*/
			if (message) msg_format("%^s slows down.", m_name);
		}
	}
	else
	{
		/*monster is now hasted and speed needs to be recalculated*/
		if (counter > 0)
		{
			recalc = TRUE;

			/*give a message*/
			if (message) msg_format("%^s starts moving faster.", m_name);
		}
	}

	/*update the counter*/
	m_ptr->hasted = counter;

	/*re-calculate speed if necessary*/
	if (recalc) m_ptr->mspeed = calc_monster_speed(m_ptr);

	return;
}

void set_monster_slow(s16b m_idx, s16b counter, bool message)
{
	/*get the monster at the given location*/
	monster_type *m_ptr = &m_list[m_idx];

	bool recalc = FALSE;

	char m_name[80];

	/* Get monster name*/
	monster_desc(m_name, m_ptr, 0);

	/*see if we need to recalculate speed*/
	if (m_ptr->slowed)
	{
		/*monster is no longer slowed and speed needs to be recalculated*/
		if (counter == 0)
		{
			recalc = TRUE;

			/*give a message*/
			if (message) msg_format("%^s speeds up.", m_name);
		}
	}
	else
	{
		/*monster is now slowed and speed needs to be recalculated*/
		if (counter > 0)
		{
			recalc = TRUE;

			/*give a message*/
			if (message) msg_format("%^s starts moving slower.", m_name);
		}
	}

	/*update the counter*/
	m_ptr->slowed = counter;

	/*re-calculate speed if necessary*/
	if (recalc) m_ptr->mspeed = calc_monster_speed(m_ptr);

	return;
}

/*
 * Given a monster and possibly a blow number, return the 
 * first object index that it can use as ammunition. If
 * created is set to true, create ammunition for the monster
 * if there is none.
 *
 * blow == -1: Check all blows. Return ammo slot for monster
 * ammunition. If created set true, create ammunition for
 * all blows which require it. If return 0, monster does
 * not require ammuntion.
 * blow == 0..3: Check this one blow. Return -1 if there
 * is no ammunition for this blow. If created set true,
 * create ammunition for this one blow which requires it.
 * Return 0 if monster does not require ammunition for this
 * blow.
 *
 * XXX Hack: we used hard-coded kinds for efficiency
 * purposes to save unnecessary lookups to lookup_kind()
 *
 * XXX Maybe coat ammunition with an appropriate potion
 * in order that the player can use it later on.
 */
int find_monster_ammo(int m_idx, int blow, bool created)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int ammo = 0;
	int ammo_tval = 0;
	int ammo_sval = 0;
	int ammo_kind = 0;
	int this_o_idx, next_o_idx, i;

	object_type *o_ptr;
	object_type object_type_body;

	/* Examine the attacks */
	for (i = (blow < 0 ? 0 : blow); i < (blow < 0 ? 4 : blow + 1); i++)
	{
		int method, effect, d_dice, d_side;

		/* Skip non-attacks */
		if (!r_ptr->blow[i].method) continue;

		/* Extract the attack info */
		method = r_ptr->blow[i].method;
		effect = r_ptr->blow[i].effect;
		d_dice = r_ptr->blow[i].d_dice;
		d_side = r_ptr->blow[i].d_side;

		/* Ranged? */
		if (method < RBM_MIN_RANGED) continue;

		switch (method)
		{
			case RBM_SPORE:
			{
				ammo_kind = 596;
				ammo_tval = TV_EGG;
				ammo_sval = SV_EGG_SPORE;
				break;
			}
			case RBM_BOULDER:
			{
				ammo_kind = 598;
				ammo_tval = TV_JUNK;
				ammo_sval = SV_JUNK_ROCK;
				break;
			}
			case RBM_ARROW:
			{
				ammo_tval = TV_ARROW;
				if (d_dice <= 3)  ammo_sval = SV_AMMO_NORMAL;
				else if (d_dice <= 12) ammo_sval = SV_AMMO_STEEL;
				else if (d_dice <= 27) ammo_sval = SV_AMMO_SPECIAL;
				else ammo_sval = SV_AMMO_HEAVY;
				break;
			}
			case RBM_XBOLT:
			{
				ammo_tval = TV_BOLT;
				if (d_dice <= 4)  ammo_sval = SV_AMMO_NORMAL;
				else if (d_dice <= 16) ammo_sval = SV_AMMO_STEEL;
				else if (d_dice <= 36) ammo_sval = SV_AMMO_SPECIAL;
				else ammo_sval = SV_AMMO_HEAVY;
				break;
			}
			case RBM_SPIKE:
			{
				ammo_kind = 345;
				ammo_tval = TV_SPIKE;
				ammo_sval = 0;
				break;
			}
			case RBM_DART:
			{
				ammo_kind = 434;
				ammo_tval = TV_POLEARM;
				ammo_sval = SV_DART;
				break;
			}
			case RBM_SHOT:
			{
				ammo_tval = TV_SHOT;
				if (d_dice < 2)  ammo_sval = SV_AMMO_LIGHT;
				else if (d_dice < 8) ammo_sval = SV_AMMO_NORMAL;
				else if (d_dice < 18) ammo_sval = SV_AMMO_STEEL;
				else ammo_sval = SV_AMMO_HEAVY;
				break;
			}
			case RBM_FLASK:
			{
				if (d_dice < 7)
				{
					ammo_tval = TV_FLASK;
					ammo_sval = SV_FLASK_OIL;
				}
				else
				{
					ammo_tval = TV_POTION;
					ammo_sval = SV_POTION_DETONATIONS;
				}
				break;
			}
		}

		/* Blow doesn't need ammo */
		if (!ammo_tval) continue;

		/* Scan monster inventory */
		for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Check if ammo */
			if (o_ptr->tval == ammo_tval) ammo = this_o_idx;

			/* Prefer correct ammo */
			if (o_ptr->sval == ammo_sval) break;
		}

		/* Hack - Checking a single blow */
		if ((blow >= 0) && !(ammo)) return (-1);

		/* Monster has ammo */
		if (!created || (ammo != 0)) continue;

		/* Create some ammo for the monster */
		if (!ammo_kind) ammo_kind = lookup_kind(ammo_tval, ammo_sval);

		/* Get the object body */
		o_ptr = &object_type_body;

		/* Prepare the item */
		object_prep(o_ptr, ammo_kind);

		/* Give the monster some shots */
		o_ptr->number = (byte)rand_range(1, r_ptr->level);

		/* Flavour spores */
		if (o_ptr->tval == TV_EGG) o_ptr->name3 = m_ptr->r_idx;

		/* Archers get more shots */
		if (r_ptr->flags2 & (RF2_ARCHER)) o_ptr->number += (byte)rand_range(1, r_ptr->level);

		/* Boulder / flask throwers get less shots */
		if ((ammo_tval == TV_JUNK) || (ammo_tval == TV_FLASK) || (ammo_tval == TV_POTION)) o_ptr->number = (o_ptr->number + 1) / 2;

		/* Sense magic */
		o_ptr->feeling = sense_magic(o_ptr,cp_ptr->sense_type,TRUE);

		/* Monster carries the object */
		ammo = monster_carry(m_idx, o_ptr);

		return (ammo);
	}

	/* Monster has no ammo and needs it */
	if (ammo_tval && !ammo) return (-1);

	return (ammo);
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
	monster_race *r_ptr;

	monster_type *n_ptr;
	monster_type monster_type_body;

	cptr name;

	dungeon_zone *zone=&t_info[0].zone[0];

	/* Get the feature */
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x))
	{
		return (FALSE);
	}

	/* Require monster can survive on terrain */
	if (place_monster_here(y, x, r_idx) <= MM_FAIL)
	{
		return (FALSE);
	}

	/* Hack -- no creation on glyph of warding */
	if (f_ptr->flags1 & (FF1_GLYPH)) return (FALSE);


	if (!r_idx) msg_print("Bug: no monster!");

	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Race */
	r_ptr = &r_info[r_idx];

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


	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (p_ptr->depth < r_ptr->level))
	{
		/* Cannot create */
		return (FALSE);
	}


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

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - p_ptr->depth);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
		if (cheat_hear) msg_format("Unique (%s).", name);
	}


	/* Get local monster */
	n_ptr = &monster_type_body;

	/* Clean out the monster */
	(void)WIPE(n_ptr, monster_type);


	/* Save the race */
	n_ptr->r_idx = r_idx;

	/* Town level has some special rules */
	if (!zone->fill)
	{
		/* Hack -- harmless townsmen are not threatening */
		if (r_ptr->flags9 & (RF9_TOWNSFOLK)) n_ptr->mflag |= (MFLAG_TOWN);

		/* Mega-hack -- no nasty town dwellers when starting out */
		else if (!p_ptr->max_exp) return (FALSE);
	}

	/* Enforce sleeping if needed */
	if (slp && r_ptr->sleep)
	{
		int val = r_ptr->sleep;
		n_ptr->csleep = ((val * 2) + randint(val * 10));
	}

	/* Hack -- allow uniques to have their stats drained multiple times */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Uniques always start out the best possible */
		n_ptr->mflag |= (MFLAG_STRONG | MFLAG_SMART | MFLAG_WISE | MFLAG_SKILLFUL | MFLAG_HEALTHY);
	}
	/* Hack -- small racial variety - see the mflag defines */
	else
	{
		u32b variety = rand_int(0x1000);

		/* Set initially variation */
		n_ptr->mflag |= variety << 16;

		/* Reroll */
		variety = rand_int(32);

		/* Adjust frequencies so 1/4 better, 1/2 average, 1/4 worse */
		if ((n_ptr->mflag & (MFLAG_SLOW)) && (variety & 0x01)) n_ptr->mflag &= ~(MFLAG_SLOW | MFLAG_FAST);
		if ((n_ptr->mflag & (MFLAG_WEAK)) && (variety & 0x02))  n_ptr->mflag &= ~(MFLAG_WEAK | MFLAG_STRONG);
		if ((n_ptr->mflag & (MFLAG_STUPID)) && (variety & 0x04))  n_ptr->mflag &= ~(MFLAG_STUPID | MFLAG_SMART);
		if ((n_ptr->mflag & (MFLAG_NAIVE)) && (variety & 0x08)) n_ptr->mflag &= ~(MFLAG_NAIVE | MFLAG_WISE);
		if ((n_ptr->mflag & (MFLAG_CLUMSY)) && (variety & 0x10))  n_ptr->mflag &= ~(MFLAG_CLUMSY | MFLAG_SKILLFUL);
		if ((n_ptr->mflag & (MFLAG_SICK))  && (variety & 0x20)) n_ptr->mflag &= ~(MFLAG_SICK | MFLAG_HEALTHY);
	}

	/* Calculate the monster hp */
	n_ptr->maxhp = calc_monster_hp(n_ptr);

	/* And start out fully healthy */
	n_ptr->hp = n_ptr->maxhp;

	/* And start out with full mana */
	n_ptr->mana = r_ptr->mana;

	/* Calculate the monster_speed*/
	n_ptr->mspeed = calc_monster_speed(n_ptr);

	/* And start with random energy */
	n_ptr->energy = (byte)rand_int(100);

	/* Some monsters radiate lite when born */
	if ((r_ptr->flags2 & (RF2_HAS_LITE))
			|| ((r_ptr->flags2 & (RF2_NEED_LITE)) && !(p_ptr->cur_lite)))
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Place the monster in the dungeon */
	if (!monster_place(y, x, n_ptr)) return (FALSE);

	/* Give the monster some ammunition */
	(void)find_monster_ammo(cave_m_idx[y][x], -1, TRUE);

	/* Certain monsters created in giant spider webs */
	if (r_ptr->flags2 & (RF2_HAS_WEB))
	{
		int flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_HIDE;
							
		(void)project(0, 0, y, x, y, x, 0, GF_WEB, flg, 0, 0);
	}

	/* Success */
	return (TRUE);
}



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
	int adjust;

	int hack_n = 0;

	byte hack_y[GROUP_MAX];
	byte hack_x[GROUP_MAX];

	/* Hack -- don't adjust if one monster */
	if (group_size > 1)
	{
		/* Hard monsters, smaller groups */
		if (r_ptr->level > p_ptr->depth)
		{
			adjust = (r_ptr->level - p_ptr->depth) / 2;
			group_size -= randint(adjust);
		}

		/* Easier monsters, bigger groups */
		if (p_ptr->depth > r_ptr->level)
		{
			adjust = (p_ptr->depth - r_ptr->level) / 2;
			group_size += randint(adjust);
		}
	}

	/* Minimum size */
	if (group_size < 1) group_size = 1;

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
				!place_monster_group(my, mx, escort_idx, slp, (rand_range(3, 5))))
			{
				continue;
			}

			/* Place a group of escorts if needed */
			if ((r_info[escort_idx].flags1 & (RF1_FRIEND)) &&
				!place_monster_group(my, mx, escort_idx, slp, 2))
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
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * Hack -- help decide if a monster race is "okay" to summon
 *
 * XXX We pass through a number of parameters as global
 * variables. If these are not set, it means we are looking
 * for a candidate race, which all subsequent summonings from
 * this spell will be based on. e.g. Find me a monster, and
 * summon one or more of its kin.
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
		case SUMMON_KIN:
		case RAISE_DEAD:
		{
			if (summon_char_type)
			{
				okay = ((r_ptr->d_char == summon_char_type) && 
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			else
			{
				okay = TRUE;
			}
			break;
		}

		/* Hack -- we combine two different flag types */
		case ANIMATE_DEAD:
		{
			/* Hack -- animate skulls */
			if ((summon_char_type == '~') && (summon_attr_type == TERM_WHITE))
			{
				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Match on undead */
					(r_ptr->flags3 & (RF3_UNDEAD)) &&

					/* Match on name */
					((strstr(r_name + r_ptr->name, "Skull")) ||

					/* Match on name */
					(strstr(r_name + r_ptr->name, "skull")) ));

			}
			/* Hack -- animate hands */
			else if ((summon_char_type == '~') && (summon_attr_type == TERM_RED))
			{
				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Match on undead */
					(r_ptr->flags3 & (RF3_UNDEAD)) &&

					/* Match on name */
					((strstr(r_name + r_ptr->name, "Hand")) ||

					/* Match on name */
					(strstr(r_name + r_ptr->name, "hand")) ));

			}
			else if (summon_char_type && summon_flag_type)
			{
				u32b summon_flag3_type = summon_flag_type & (0x0000FFFFL);
				u32b summon_flag9_type = summon_flag_type & (0xFFFF0000L);

				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Match char */
					(((r_ptr->d_char == summon_char_type) &&

					/* Match flag */
					((r_ptr->flags3 & (summon_flag3_type)) ||

					/* Match flag */
					(r_ptr->flags9 & (summon_flag9_type)) )) ||

					/* Also can match some high level undead */
					((summon_char_type == 's') && (r_ptr->d_char == 'L')) ||

					((summon_char_type == 'z') && (r_ptr->d_char == 'N'))) );
			}
			else if (summon_flag_type)
			{
				u32b summon_flag3_type = summon_flag_type & (0x0000FFFFL);
				u32b summon_flag9_type = summon_flag_type & (0xFFFF0000L);

				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Match 'animated' undead */
					( ((strchr("sz", r_ptr->d_char)) &&

					/* Match any */
					((r_ptr->flags3 & (summon_flag3_type)) ||

					/* Match any */
					(r_ptr->flags9 & (summon_flag9_type)) )) ||

					/* Also can match some high level undead without restriction */
					(strchr("LN", r_ptr->d_char)) ));
			}
			else if (summon_char_type)
			{
				/* Match char */
				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Match char */
					(r_ptr->d_char == summon_char_type));
			}
			else
			{
				/* Match 'animated' undead */
				okay = ((strchr("szLN", r_ptr->d_char)) &&
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			break;
		}

		case SUMMON_PLANT:
		{
			okay = ((r_ptr->flags3 & (RF3_PLANT)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_INSECT:
		{
			okay = ((r_ptr->flags3 & (RF3_INSECT)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		/* Hack -- try to summon birds, beasts or reptiles based on summoner */
		case SUMMON_ANIMAL:
		{
			if (summon_flag_type)
			{
				okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
					!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Check 'skin' */
					((r_ptr->flags8 & (RF8_SKIN_MASK)) ?

					/* Has feathers, scales or fur? */
					((r_ptr->flags8 & (summon_flag_type)) ? TRUE : FALSE) :

					/* Has none of the above - treat as scales... */
					((r_ptr->flags8 & (RF8_HAS_SCALE)) ? TRUE : FALSE)));
			}
			else
			{
				okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			break;
		}

		case SUMMON_HOUND:
		{
			okay = (((r_ptr->d_char == 'C') || (r_ptr->d_char == 'Z')) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_SPIDER:
		{
			okay = ((r_ptr->d_char == 'S') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		/* Hack -- mage priests only hang out with mage priests, but
			warrior priests hang out with warriors and priests */
		case SUMMON_CLASS:
		{
			if (summon_flag_type)
			{
				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Check 'class' is mage or priest and we want a mage and/or priest */
					((r_ptr->flags2 & (RF2_MAGE | RF2_PRIEST)) && (summon_flag_type & (RF2_MAGE | RF2_PRIEST)) ?

					/* Exact match on mage / priest component */
					((r_ptr->flags2 & (RF2_MAGE | RF2_PRIEST)) == (summon_flag_type & (RF2_MAGE | RF2_PRIEST)) ? TRUE : FALSE) :

					/* Match any other */
					((r_ptr->flags2 & (summon_flag_type)) ? TRUE : FALSE) ));
			}
			else
			{
				okay = ((r_ptr->flags2 & (RF2_CLASS_MASK)) &&
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			break;
		}

		/* Hack -- we combined two different flag types */
		case SUMMON_RACE:
		{
			if (summon_flag_type)
			{
				u32b summon_flag3_type = summon_flag_type & (0x0000FFFFL);
				u32b summon_flag9_type = summon_flag_type & (0xFFFF0000L);

				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Match any */
					((r_ptr->flags3 & (summon_flag3_type)) ||

					/* Match any */
					(r_ptr->flags9 & (summon_flag9_type)) ));
			}
			else
			{
				okay = ((r_ptr->flags3 & (RF3_RACE_MASK)) &&
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			break;
		}

		case SUMMON_GROUP:
		case ANIMATE_ELEMENT:
		{
			if (summon_group_type)
			{
				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&

					/* Has a group */
					(r_ptr->grp_idx) && 

					/* Match group index */
					(r_ptr->grp_idx == summon_group_type));
			}
			else
			{
				okay = ((r_ptr->grp_idx) &&
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			break;
		}

		case ANIMATE_OBJECT:
		{
			if (summon_attr_type && summon_char_type)
			{
				okay = (((r_ptr->d_attr == summon_attr_type) ||
					(r_ptr->flags1 & (RF1_ATTR_CLEAR)) ||
					(r_ptr->flags9 & (RF9_ATTR_INDEX))) && 
					(r_ptr->d_char == summon_char_type) && 
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			else if (summon_attr_type)
			{
				okay = (((r_ptr->d_attr == summon_attr_type) ||
					(r_ptr->flags1 & (RF1_ATTR_CLEAR)) ||
					(r_ptr->flags9 & (RF9_ATTR_INDEX))) && 
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			else if (summon_char_type)
			{
				okay = ((r_ptr->d_char == summon_char_type) && 
					!(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			else
			{
				okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&
					strchr("&:;.,'!_-\\/[]~$%^*(){}+=<>?#",r_ptr->d_char));
			}
			break;
		}

		case SUMMON_FRIEND:
		{
			if (summon_race_type)
			{
				okay = (r_idx == summon_race_type);
			}
			else
			{
				okay = TRUE;
			}
			break;
		}

		case SUMMON_UNIQUE_FRIEND:
		{
			if (summon_attr_type && summon_char_type)
			{
				okay = ((r_ptr->d_attr == summon_attr_type) && 
					(r_ptr->d_char == summon_char_type) && 
					(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			else if (summon_attr_type)
			{
				okay = ((r_ptr->d_attr == summon_attr_type) && 
					(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			else if (summon_char_type)
			{
				okay = ((r_ptr->d_char == summon_char_type) && 
					(r_ptr->flags1 & (RF1_UNIQUE)));
			}
			else
			{
				okay = (r_ptr->flags1 & (RF1_UNIQUE));
			}
			break;
		}

		case SUMMON_ORC:
		{
			okay = ((r_ptr->flags3 & (RF3_ORC)) && !(r_ptr->flags3 & ((RF3_RACE_MASK) & ~(RF3_ORC))) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_TROLL:
		{
			okay = ((r_ptr->flags3 & (RF3_TROLL)) && !(r_ptr->flags3 & ((RF3_RACE_MASK) & ~(RF3_TROLL))) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_GIANT:
		{
			okay = ((r_ptr->flags3 & (RF3_GIANT)) && !(r_ptr->flags3 & ((RF3_RACE_MASK) & ~(RF3_GIANT))) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_DEMON:
		{
			okay = ((r_ptr->flags3 & (RF3_DEMON)) && !(r_ptr->flags3 & ((RF3_RACE_MASK) & ~(RF3_DEMON))) &&
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
			okay = ((r_ptr->flags3 & (RF3_DRAGON)) && !(r_ptr->flags3 & ((RF3_RACE_MASK) & ~(RF3_DRAGON))) &&
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
			okay = ((r_ptr->d_char == 'D') ||
			        (r_ptr->d_char == 'A'));
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


		default:
		{
			break;
		}

	}

	/* Result */
	return (okay);
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

	/* Friends for uniques - we have to reallocate separately to avoid recursion */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->flags1 & (RF1_FRIEND | RF1_FRIENDS)))
	{
		int i, s_idx;

		/* Save the unique attr and char */
		summon_attr_type = r_ptr->d_attr;
		summon_char_type = r_ptr->d_char;

		/* Save the "summon" type */
		summon_specific_type = SUMMON_UNIQUE_FRIEND;

		/* Place friend or friends */
		for (i = 0; i < (r_ptr->flags1 & (RF1_FRIENDS) ? 4 : 1); i++)
		{
			/* Require "okay" monsters */
			get_mon_num_hook = summon_specific_okay;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Pick a monster, using the monster race level */
			s_idx = get_mon_num(r_ptr->level);

			/* Remove restriction */
			get_mon_num_hook = NULL;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Handle failure */
			if (!s_idx) continue;

			/* Place one monster, or fail */
			if (!place_monster_one(y, x, s_idx, slp)) return (FALSE);

			/* Escorts for certain monsters */
			if ((r_info[s_idx].flags1 & (RF1_ESCORT)) || (r_info[s_idx].flags1 & (RF1_ESCORTS)))
			{
				place_monster_escort(y, x, s_idx, slp);
			}
		}
	}
	/* A friend for certain monsters */
	else if (r_ptr->flags1 & (RF1_FRIEND))
	{
		/* Attempt to place a second monster */
		(void)place_monster_group(y, x, r_idx, slp, 1);
	}
	/* Friends for certain monsters */
	else if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		/* Attempt to place a large group */
		(void)place_monster_group(y, x, r_idx, slp, (s16b)rand_range(3, 5));
	}

	/* Escorts for certain monsters */
	if ((r_ptr->flags1 & (RF1_ESCORT)) || (r_ptr->flags1 & (RF1_ESCORTS)))
	{
		place_monster_escort(y, x, r_idx, slp);
	}

	/* Certain monsters created in giant spider webs */
	if (r_ptr->flags2 & (RF2_HAS_WEB))
	{
		int k;

		int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_THRU | PROJECT_HIDE;
							
		/* Shoot web out in 8 directions when placed */
		for (k = 0; k < 8; k++) (void)project(0, 10, y, x, y + 99 * ddy_ddd[k], x + 99 * ddx_ddd[k], 0, GF_WEB, flg, 0, 0);
	}

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "monster_level"
 */
bool place_monster(int y, int x, bool slp, bool grp)
{
	int r_idx;

	/* Pick a monster */
	r_idx = get_mon_num(monster_level);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	if (place_monster_aux(y, x, r_idx, slp, grp)) return (TRUE);

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

	int y, x, c;

	c = 0;

	/* Find a legal, distant, unoccupied, space */
	while (1)
	{
		if (c++ > 2000) return (FALSE);

		/* Pick a location */
		y = rand_int(DUNGEON_HGT);
		x = rand_int(DUNGEON_WID);

		/* Require "naked" floor grid */
		if (!cave_naked_bold(y, x)) continue;

		/* Accept far away grids */
		if (distance(y, x, py, px) > dis) break;
	}

	/* Attempt to place the monster, allow groups */
	if (place_monster(y, x, slp, TRUE)) return (TRUE);

	/* Nope */
	return (FALSE);
}






/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE iff a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH (XXX) will summon Uniques
 * Note: SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Uniques
 * Note: None of the other summon codes will ever summon Uniques.
 *
 * This function has been changed.  We now take the "monster level"
 * of the summoning monster as a parameter, and use that, along with
 * the current dungeon level, to help determine the level of the
 * desired monster.  Note that this is an upper bound, and also
 * tends to "prefer" monsters of that level.  Currently, we use
 * the average of the dungeon and monster levels, and then add
 * five to allow slight increases in monster power.
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "get_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 */
bool summon_specific(int y1, int x1, int lev, int type)
{
	int i, x, y, r_idx;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		if (!in_bounds_fully(y,x)) continue;

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summoning on glyph of warding */
		if (f_info[cave_feat[y][x]].flags1 & (FF1_GLYPH)) continue;

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

	/* Pick a monster, using the level calculation */
	r_idx = get_mon_num(lev);

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, TRUE)) return (FALSE);

	/* Hack -- Set specific summoning parameters if not currently set */
	switch (summon_specific_type)
	{
		case SUMMON_KIN:
		{
			if (!summon_char_type) summon_char_type = r_info[r_idx].d_char;
			break;
		}

		/* Hack -- we combine two different flag types */
		case SUMMON_RACE:
		{
			if (!summon_flag_type)
			{
				summon_flag_type |= r_info[r_idx].flags3 & (RF3_RACE_MASK);
				summon_flag_type |= r_info[r_idx].flags9 & (RF9_RACE_MASK);
			}
			break;
		}

		/* Hack -- try to summon birds, beasts or reptiles based on summoner */
		case SUMMON_ANIMAL:
		{
			if (!summon_flag_type)
			{
				summon_flag_type |= r_info[r_idx].flags8 & (RF8_SKIN_MASK);

				if (!summon_flag_type) summon_flag_type = RF8_HAS_SCALE;
			}
			break;
		}

		/* Hack -- mage priests only hang out with mage priests, but
			warrior priests hang out with warriors and priests */
		case SUMMON_CLASS:
		{
			if (!summon_flag_type)
			{
				summon_flag_type = r_info[r_idx].flags2 & (RF2_CLASS_MASK);
			}
			break;
		}

		case SUMMON_GROUP:
		case ANIMATE_ELEMENT:
		{
			if (!summon_group_type)
			{
				summon_group_type = r_info[r_idx].grp_idx;
			}
			break;
		}

		case SUMMON_FRIEND:
		{
			if (!summon_race_type)
			{
				summon_race_type = r_idx;
			}
			break;
		}

		case SUMMON_UNIQUE_FRIEND:
		{
			if (!summon_attr_type && !summon_char_type)
			{
				summon_char_type = r_info[r_idx].d_char;
				summon_attr_type = r_info[r_idx].d_attr;
			}
			break;
		}
	}

	/* Success */
	return (TRUE);
}

/*
 * Unlike above routine, we only place one of the monster, regardless
 * of type and we do allow uniques, but not if already created.
 */
bool summon_specific_one(int y1, int x1, int r_idx, bool slp)
{
	int i, x, y;

	/* Get the monster */
	monster_race *r_ptr = &r_info[r_idx];

	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) &&
	    (r_ptr->cur_num >= r_ptr->max_num))
	{
		return (FALSE);
	}

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summoning on glyph of warding */
		if (f_info[cave_feat[y][x]].flags1 & (FF1_GLYPH)) continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Attempt to place the monster (awake or asleep, do not allow groups) */
	if (!place_monster_aux(y, x, r_idx, slp, FALSE)) return (FALSE);

	/* Hack -- monster does not drop anything */
	m_list[cave_m_idx[y][x]].mflag |= (MFLAG_MADE);

	/* Success */
	return (TRUE);
}



/*
 * Let the given object become alive.
 *
 * Note that "animation" REQUIRES empty space.
 *
 */
bool animate_object(int item)
{
	cptr p;

	char o_name[80];

	object_type *o_ptr;

	bool result = FALSE;

	int y1, x1;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
		y1 = p_ptr->py;
		x1 = p_ptr->px;
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
		y1 = o_ptr->iy;
		x1 = o_ptr->ix;
	}

	/* Paranoia */
	if (o_ptr->name3 <= 0) return (FALSE);

	/* Summon the specific race */
	result = summon_specific_one(y1, x1, o_ptr->name3, FALSE);

	/* Hack -- no result */
	if (!result)
	{
		/* Keep trying */
		o_ptr->timeout++;

		/* Return */
		return (FALSE);
	}

	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Correct message */
	switch (o_ptr->tval)
	{
		case TV_EGG:
			p = "hatched.";
			break;

		case TV_BODY:
			p = "come back from the dead!";
			break;

	        case TV_HOLD:
			p = "broken open!";
			break;

		default:
			p = "come to life!";
			break;
	}

	/* Notify player if carrying */
	if (item >= 0)
	{
		/* Message */
		msg_format("Your %s %s %s", o_name,
		   ((o_ptr->stackc == 1) ? "has" :
		   ((!(o_ptr->stackc) && (o_ptr->number == 1)) ?
		   "has" : "have")), p);

		/* Destroy the item */
		if (o_ptr->stackc)
		{
			if (o_ptr->number == o_ptr->stackc) inven_drop_flags(o_ptr);
			inven_item_increase(item, -(o_ptr->stackc));
		}
		else
		{
			inven_drop_flags(o_ptr);
			inven_item_increase(item,-(o_ptr->number));
		}

		inven_item_optimize(item);
	}
	else
	{
		/* Message if object known */
		if (o_ptr->ident & (IDENT_MARKED)) msg_format("The %s %s %s", o_name,
		   ((o_ptr->stackc == 1) ? "has" :
		   ((!(o_ptr->stackc) && (o_ptr->number == 1)) ?
		   "has" : "have")), p);

		/* Destroy the item */
		if (o_ptr->stackc) floor_item_increase(0 - item, -(o_ptr->stackc));
		else floor_item_increase(0 - item, -(o_ptr->number));

		floor_item_optimize(0 - item);
	}

	return (TRUE);
}



/*
 * Change monster fear.
 *
 * Monsters can be frightened or panicking.  In both cases, they try to
 * retreat, but when actually panicking, they cannot cast spells that don't
 * either heal or move them.
 */
void set_monster_fear(monster_type *m_ptr, int v, bool panic)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/*hack - monsters who cannot be scared are unaffected*/
	if (r_ptr->flags3 & (RF3_NO_FEAR)) v = 0;

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
 */
bool multiply_monster(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	int i, y, x;

	bool result = FALSE;

	/* Try up to 18 times */
	for (i = 0; i < 18; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, m_ptr->fy, m_ptr->fx, d, 0);

		/* Require an "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Require monster can survive on terrain */
		if (place_monster_here(y, x, m_ptr->r_idx) <= MM_FAIL) continue;

		/* Create a new monster (awake, no groups) */
		result = place_monster_aux(y, x, m_ptr->r_idx, FALSE, FALSE);

		/* Hack -- monster does not drop anything */
		m_list[cave_m_idx[y][x]].mflag |= (MFLAG_MADE);

		/* Done */
		break;
	}

	/* Result */
	return (result);
}





/*
 * Dump a message describing a monster's reaction to damage
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
#if 0
		msg_format("%^s is unharmed.", m_name);
#endif
		return;
	}

	/* Note -- subtle fix -CFT */
	newhp = (long)(m_ptr->hp);
	oldhp = newhp + (long)(dam);
	tmp = (newhp * 100L) / (oldhp);
	percentage = (int)(tmp);


	/* Hack -- avoid mentioning minor damage */
	if (!(m_ptr->ml) && (percentage > 95)) return;

	/* Hack -- avoid mentioning if out of sight */
	if (m_ptr->cdis > MAX_SIGHT) return;

#if 0
	/* Hack -- if seen, only report changes in 'damage state'*/
	if (m_ptr->ml)
	{
		int percentage2;

		tmp = (oldhp * 100L) / (long)(m_ptr->maxhp);
		percentage = (int)(tmp);

		tmp = (newhp * 100L) / (long)(m_ptr->maxhp);
		percentage2 = (int)(tmp);

		/* Notify the player only if monster 'damage state' changes */
		if (((percentage >= 60) && (percentage2 < 60))
			|| ((percentage >= 25) && (percentage2 < 25))
			|| ((percentage >= 10) && (percentage2 < 10)))
		{
			msg_format("%^s is %s.", m_name, look_mon_desc(m_idx));
		}
		return;
	}
#endif
	/* Nonvocal monsters */
	if (r_ptr->flags3 & (RF3_NONVOCAL))
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
			msg_format("%^s yelps feebly.", m_name);
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (strchr("FIKRSXabclrs", r_ptr->d_char))
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
