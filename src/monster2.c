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
 * Delete a monster by index.
 *
 * When a monster is deleted, all of its objects are deleted.
 */
void delete_monster_idx(int i)
{
	int x, y;

	monster_type *m_ptr = &m_list[i];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_unique *u_ptr = &u_info[m_ptr->u_idx];

	s16b this_o_idx, next_o_idx = 0;

	/* Get location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Hack -- Reduce the racial counter */
	r_ptr->cur_num--;

	/* Hack -- Make unique available again, unless dead */
	if (m_ptr->u_idx && !u_ptr->dead)
	{
		r_ptr->cur_unique++;
		u_ptr->depth = -1;
	}

	/* Hack -- count the number of "reproducers" */
	if (r_ptr->flags1 & (RF1_MULTIPLY)) num_repro--;

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
	if (size) message(MSG_GENERIC, 0, "Compacting monsters...");

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

			monster_race *r_ptr;

			/* Paranoia -- skip "dead" monsters */
			if (!m_ptr->r_idx) continue;

			r_ptr = get_monster_real(m_ptr);

			/* Hack -- High level monsters start out "immune" */
			if (r_ptr->level > cur_lev) continue;

			/* Ignore nearby monsters */
			if ((cur_dis > 0) && (m_ptr->cdis < cur_dis)) continue;

			/* Saving throw chance */
			chance = 90;

			/* Only compact "Quest" Monsters in emergencies */
			if ((cnt < 1000) && (q_info[quest_num(p_ptr->depth)].r_idx == m_ptr->r_idx)) 
				chance = 100;

			/* Try not to compact Unique Monsters */
			if (m_ptr->u_idx) chance = 99;

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
		monster_unique *u_ptr = &u_info[m_ptr->u_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Make unique available again */
		if (m_ptr->u_idx) 
		{
			r_ptr->cur_unique++;
			u_ptr->depth = -1;
		}

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
	if (character_dungeon) message(MSG_GENERIC, 0, "Too many monsters!");

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

		/* Hack -- No town monsters in dungeon */
		if ((level > 0) && (table[i].level <= 0)) continue;

		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/* Hack -- "unique" monsters must be "unique" */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			bool quest = FALSE;

			if (r_ptr->cur_unique != 1) continue;
			
			for (j = 0; j < z_info->q_max; j++)
			{
				if ((q_info[j].r_idx == r_idx) && (q_info[j].active_level != 0) &&
					(q_info[j].active_level != p_ptr->depth)) 
				{
					quest = TRUE;
					break;
				}
			}

			if (quest) continue;
		}

		/* Accept */
		table[i].prob3 = table[i].prob2;

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
void monster_desc(char *desc, monster_type *m_ptr, int mode)
{
	cptr res;

	cptr name;

	bool seen, pron;

	monster_race *r_ptr;
	
	/* Paranoia */
	if (!m_ptr->r_idx) return; 

	/* Get monster info */
	r_ptr = get_monster_real(m_ptr);
	name = monster_name(m_ptr);

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
		if (r_ptr->flags1 & RF1_FEMALE) kind = 0x20;
		else if (r_ptr->flags1 & RF1_MALE) kind = 0x10;

		/* Ignore the gender (if desired) */
		if (!pron) kind = 0x00;

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
		if (r_ptr->flags1 & RF1_FEMALE) strcpy(desc, "herself");
		else if (r_ptr->flags1 & RF1_MALE) strcpy(desc, "himself");
		else strcpy(desc, "itself");
	}

	/* Handle all other visible monster requests */
	else
	{
		/* It could be a Unique */
		if (m_ptr->u_idx)
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
void lore_do_probe(monster_type *m_ptr)
{
	monster_race *r_ptr = get_monster_real(m_ptr);

	/* Hack -- Memorize some flags */
	lore_learn(m_ptr, LRN_FLAG1, r_ptr->flags1, FALSE);
	lore_learn(m_ptr, LRN_FLAG2, r_ptr->flags2, FALSE);
	lore_learn(m_ptr, LRN_FLAG3, r_ptr->flags3, FALSE);
	lore_learn(m_ptr, LRN_FLAG4, r_ptr->flags4, FALSE);

	/* Update monster recall window */
	if (term_mon_race_idx == m_ptr->r_idx)
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

	/* Note the number of things dropped */
	if (num_item) lore_learn(m_ptr, LRN_ITEM, num_item, FALSE);
	if (num_gold) lore_learn(m_ptr, LRN_GOLD, num_gold, FALSE);
	
	/* Hack -- memorize the good/great flags */
	lore_learn(m_ptr, LRN_FLAG1, RF1_DROP_GOOD, FALSE);
	lore_learn(m_ptr, LRN_FLAG1, RF1_DROP_GREAT, FALSE);

	/* Update monster recall window */
	if (term_mon_race_idx == m_ptr->r_idx)
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
 * are only affected by "light" if the player can see invisible).
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
	monster_race *r_ptr;

	int d;
	
	/* Current location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	/* Seen at all */
	bool flag = FALSE;

	/* Seen by vision */
	bool easy = FALSE;

	/* Things we learn about the monster */
	u32b cache_flag1 = 0;
	u32b cache_flag2 = 0;

	/* Skip dead monster */
	if (!m_ptr->r_idx) return; 

	/* Get monster stats */
	r_ptr = get_monster_real(m_ptr);

	/* Compute distance */
	if (full)
	{
		/* Distance components */
		int dy = (p_ptr->py > fy) ? (p_ptr->py - fy) : (fy - p_ptr->py);
		int dx = (p_ptr->px > fx) ? (p_ptr->px - fx) : (fx - p_ptr->px);

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
		if (p_ptr->telepathy)
		{
			/* Empty mind, no telepathy */
			if (r_ptr->flags2 & (RF2_EMPTY_MIND)) cache_flag2 |= RF2_EMPTY_MIND;

			/* Weird mind, occasional telepathy */
			else if (r_ptr->flags2 & (RF2_WEIRD_MIND))
			{
				/* One in ten individuals are detectable */
				if ((m_idx % 10) == 5)
				{
					/* Detectable */
					flag = TRUE;

					/* Memorize flags */
					cache_flag2 |= RF2_WEIRD_MIND;

					/* Hack -- Memorize mental flags */
					if (r_ptr->flags1 & (RF1_SMART)) cache_flag1 |= (RF1_SMART);
					if (r_ptr->flags1 & (RF1_STUPID)) cache_flag1 |= (RF1_STUPID);
				}
			}

			/* Normal mind, allow telepathy */
			else
			{
				/* Detectable */
				flag = TRUE;

				/* Hack -- Memorize mental flags */
				if (r_ptr->flags1 & (RF1_SMART)) cache_flag1 |= (RF1_SMART);
				if (r_ptr->flags1 & (RF1_STUPID)) cache_flag1 |= (RF1_STUPID);
			}
		}

		/* Normal line of sight, and not blind or hallucinating*/
		if (player_has_los_bold(fy, fx) && !p_ptr->blind && !p_ptr->image)
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
					easy = flag = TRUE;
				}
			}

	        /* Use "lite carriers" */
		    if ((r_ptr->flags1 & (RF1_HAS_LITE)) &&
				!(r_ptr->flags2 & (RF2_INVISIBLE))) easy=flag=TRUE;

			/* Use "illumination" */
			if (player_can_see_bold(fy, fx))
			{
				/* Handle "invisible" monsters */
				if (r_ptr->flags2 & (RF2_INVISIBLE))
				{
					/* Take note */
					do_invisible = TRUE;

					/* See invisible */
					if (p_ptr->see_inv)
					{
						/* Easy to see */
						easy = flag = TRUE;
					}
				}

				/* Handle "normal" monsters */
				else
				{
					/* Easy to see */
					easy = flag = TRUE;
				}
			}

			/* Visible */
			if (flag)
			{
				/* Memorize flags */
				if (do_invisible) cache_flag2 |= (RF2_INVISIBLE);
				if (do_cold_blood) cache_flag2 |= (RF2_COLD_BLOOD);
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

			/* Update monster list window */
			p_ptr->window |= (PW_VISIBLE);

			/* Hack -- Count "fresh" sightings */
			lore_learn(m_ptr, LRN_SIGHTS, 0, FALSE);

			/* Disturb on appearance */
			if (disturb_move) disturb(1);

			/* Player knows if it has light */
			if (r_ptr->flags1 & RF1_HAS_LITE) cache_flag1 |= RF1_HAS_LITE;
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

			/* Update monster list window */
			p_ptr->window |= (PW_VISIBLE);

			/* Disturb on disappearance */
			if (disturb_move) disturb(1);
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
			if (disturb_near) disturb(1);
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
			if (disturb_near) disturb(1);
		}
	}

	/* We learnt something */
	if (cache_flag1) lore_learn(m_ptr, LRN_FLAG1, cache_flag1, FALSE);
	if (cache_flag2) lore_learn(m_ptr, LRN_FLAG2, cache_flag2, FALSE);
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

	int by1 = y1/BLOCK_HGT;
	int bx1 = x1/BLOCK_WID;
	int by2 = y2/BLOCK_HGT;
	int bx2 = x2/BLOCK_WID;

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
		update_mon(m1, TRUE);
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

		/* Update the flow */
		p_ptr->update |= (PU_UPDATE_FLOW);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* 
		 * Update room description (if needed) 
		 * Line 1 -- we are entering a room 
		 * Line 2 -- which is different from the last room 
		 * Line 3 -- or we were not in a room 
		 */
		if ((cave_info[y2][x2] & (CAVE_ROOM)) &&
			((dun_room[by1][bx1] != dun_room[by2][bx2]) ||
			!(cave_info[y1][x1] & (CAVE_ROOM))))
		{
			p_ptr->window |= (PW_ROOM_INFO);
			p_ptr->update |= (PU_ROOM_INFO);
		}
	}

	/* Monster 2 */
	if (m2 > 0)
	{
		m_ptr = &m_list[m2];

		/* Move monster */
		m_ptr->fy = y1;
		m_ptr->fx = x1;

		/* Update monster */
		update_mon(m2, TRUE);
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

		/* Update the flow */
		p_ptr->update |= (PU_UPDATE_FLOW);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* 
		 * Update room description (if needed) 
		 * Line 1 -- we are entering a room 
		 * Line 2 -- which is different from the last room 
		 * Line 3 -- or we were not in a room 
		 */
		if ((cave_info[y1][x1] & (CAVE_ROOM)) &&
			((dun_room[by1][bx1] != dun_room[by2][bx2]) ||
			!(cave_info[y2][x2] & (CAVE_ROOM))))
		{
			p_ptr->window |= (PW_ROOM_INFO);
			p_ptr->update |= (PU_ROOM_INFO);
		}
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

		/* Get the new monster */
		m_ptr = &m_list[m_idx];

		/* Copy the monster XXX */
		COPY(m_ptr, n_ptr, monster_type);

		/* Location */
		m_ptr->fy = y;
		m_ptr->fx = x;

		/* Update the monster */
		update_mon(m_idx, TRUE);

		/* Get the new race */
		r_ptr = get_monster_real(m_ptr);

		/* Hack -- Notice new multi-hued monsters */
		if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

		/* Hack -- Count the number of "reproducers" */
		if (r_ptr->flags1 & (RF1_MULTIPLY)) num_repro++;

		/* Count racial occurances */
		r_info[m_ptr->r_idx].cur_num++;
	}

	/* Result */
	return (m_idx);
}

/*
 * Hack - get color for "mimics" - will give a "legal" color based on the options.
 */
static byte get_attr_mimic(char r_char)
{
	switch (r_char)
	{
		case '$': /* Creeping gems */
		{
			switch (rand_int(5))
			{
			case 0: return TERM_RED;
			case 1: return TERM_WHITE;
			case 2: return TERM_L_WHITE;
			case 3: return TERM_BLUE;
			case 4: return TERM_GREEN;
			}

			/* Paranoia */
			break;
		}
		case '?': /* Book Mimics */
		{
			switch (rand_int(11))
			{
			case 0: case 1: case 2: case 3: return TERM_L_RED;
			case 4: case 5: case 6: case 7: return TERM_L_GREEN;
			case 8: return TERM_RED;
			case 9: return TERM_GREEN;
			case 10: return TERM_BLUE;
			}

			/* Paranoia */
			break;
		}
		case '|': /* Sword Mimics */
		{
			switch (rand_int(100))
			{
			case 0: case 1: case 2: return TERM_RED;
			case 3: case 4: return TERM_L_DARK;
			case 5: return TERM_BLUE;
			case 6: return TERM_VIOLET;
			default : return TERM_L_WHITE; /* Usually */
			}

			/* Paranoia */
			break;
		}
		case '=': /* Ring Mimics */
		{
			return (ring_col[rand_int(SV_RING_MAX)]);
		}
		case '!': /* Potion Mimics */
		{
			return (potion_col[rand_int(SV_POTION_MAX)]);
		}
	}

	/* Totally random */
	return randint(15);
}

/*
 * Attempt to place a monster of the given race at the given location.
 *
 * To give the player a sporting chance, any monster that appears in
 * line-of-sight and is extremely dangerous can be marked as
 * "FORCE_SLEEP", which will cause them to be placed with low energy,
 * which often (but not always) lets the player move before they do.
 *
 * This is the only function which may place a monster in the dungeon,
 * except for the savefile loading code.
 */
static bool place_monster_one(int y, int x, int r_idx, bool slp, byte mode)
{
	int i, j, l;
	int u_idx = 0;

	monster_race *r_ptr;
	monster_type *n_ptr;
	monster_unique *u_ptr;

	monster_type monster_type_body;

	cptr name;

	bool force_unique;
	bool unique = FALSE;

	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* no creation on glyph of warding */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Race */
	r_ptr = &r_info[r_idx];

	force_unique = ((r_ptr->flags1 & (RF1_UNIQUE)) ? TRUE : FALSE);

	/* Paranoia */
	if (!r_ptr->name) return (FALSE);

	/* Try to place a unique */
	if ((r_ptr->cur_unique) && (mode != PLACE_NO_U))
	{
		/* Force a unique */
		if (force_unique)
		{
			for (u_idx = 0; u_idx < z_info->u_max ; u_idx++)
			{
				u_ptr = &u_info[u_idx];

				/* Find the first living unique */
				if (u_ptr->r_idx == r_idx) 
				{
					if (u_ptr->dead) continue;
					if (u_ptr->depth > -1) continue;

					break;
				}
			}

			/* Paranoia */
			if (u_idx == z_info->u_max) return (FALSE);

			/* We're making a unique */
			unique = TRUE;
		}
		/* A unique exists on a non-unique template */
		else
		{
			/* Look for uniques from this race*/
			for (u_idx = 0; u_idx < z_info->u_max ; u_idx++)
			{
				u_ptr = &u_info[u_idx];

				/* Found one */
				if (u_ptr->r_idx == r_idx) 
				{
					/* Check if unique is alive */
					if (u_ptr->dead) continue;

					/* Check if unique is not already here */
					if (u_ptr->depth > -1) continue;

					/* In PLACE_UNIQ mode, always place a unique */
					if (mode != PLACE_UNIQ)
					{
						/* Enforce minimum "depth" (loosely) */
						if (u_ptr->level > p_ptr->depth)
						{
							/* Get the "out-of-depth factor" */
							int d = (u_ptr->level - p_ptr->depth) * 2;

							/* Roll for out-of-depth creation */
							if (rand_int(d) != 0) continue;
						}

						/* Rarity check */
						if (rand_int(u_ptr->rarity) != 0) continue;
					}

					unique = TRUE;

					break;
				}
			}

			if (!unique) u_idx = 0;
		}
	}
	/* No uniques left */
	else if (force_unique) return FALSE;

	/* Handle uniques in quests*/
	if (unique)
	{
		for (j = 0; j < z_info->q_max; j++)
		{
			if ((q_info[j].r_idx == r_idx) && (q_info[j].active_level != 0) &&
				(q_info[j].active_level != p_ptr->depth)) 
			{
				return (FALSE);
			}
		}
	}

	name = monster_name_idx(r_idx, 0, u_idx);

	/* Powerful monster */
	if (r_ptr->level > p_ptr->depth)
	{
		/* Unique monsters */
		if (unique)
		{
			/* Message for cheaters */
			if (cheat_hear) message_format(MSG_CHEAT, 0, "Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - p_ptr->depth) * 2;
			if (!force_unique) rating += (u_ptr->level - p_ptr->depth) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
			if (cheat_hear) message_format(MSG_CHEAT, 0, "Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - p_ptr->depth);
		}
	}

	/* Note the monster */
	else if (unique)
	{
		if ((!force_unique) && (u_ptr->level > p_ptr->depth))
		{
			/* Message for cheaters */
			if (cheat_hear) message_format(MSG_CHEAT, 0, "Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (u_ptr->level - p_ptr->depth) * 2;
		}

		/* Unique monsters induce message */
		else if (cheat_hear) message_format(MSG_CHEAT, 0, "Unique (%s).", name);
	}

	/* Get local monster */
	n_ptr = &monster_type_body;

	/* Clean out the monster */
	(void)WIPE(n_ptr, monster_type);

	/* Save the race */
	n_ptr->r_idx = r_idx;

	if (unique)
	{
		/* Mark as unique */
		n_ptr->u_idx = u_idx;

		/* Mark unique's location */
		u_ptr->depth = p_ptr->depth;

		/* One less unique available */
		r_ptr->cur_unique--;

		/* Build a fake monster to get stuff from */
		r_ptr = get_monster_fake(r_idx, 0, u_idx);
	}

	/* Ego monster type */
	if (r_ptr->flags1 & RF1_EGO) 
	{
		/* Ten tries */
		while (!n_ptr->s_idx)
		{
			monster_special *s_ptr;

			/* Try to apply a random prefix */
			l = randint(z_info->s_max - 1);
			s_ptr = &s_info[l];

			/* Not a real prefix */
			if (!s_ptr->rarity) continue;

			/* Check for illegal flags */
			if (r_ptr->flags4 & s_ptr->no_flag4) continue;

			/* Roll for rarity */
			if (rand_int(s_ptr->rarity) == 0)
			{
				n_ptr->s_idx = l;
			}
		}
	}

	/* Monster color */
	if (r_ptr->flags1 & RF1_ATTR_MIMIC) n_ptr->attr = get_attr_mimic(r_ptr->d_char);
	else n_ptr->attr = r_ptr->x_attr;

	/* Enforce sleeping if needed */
	if (slp && r_ptr->sleep)
	{
		int val = r_ptr->sleep;
		n_ptr->csleep = ((val * 2) + randint(val * 10));
	}

	/* Assign maximal hitpoints */
	if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || adult_nightmare_mode)
	{
		n_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		n_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

 	if (adult_easy_mode) n_ptr->maxhp = (n_ptr->maxhp / 2) + 1;

	if (n_ptr->maxhp < 1) n_ptr->maxhp = 1;

	/* And start out fully healthy */
	n_ptr->hp = n_ptr->maxhp;

	/* Extract the monster base speed */
	n_ptr->mspeed = r_ptr->speed;

	/* Hack -- small racial variety */
	if (!unique)
	{
		/* Allow some small variation per monster */
		i = extract_energy[r_ptr->speed] / 10;
		if (i) n_ptr->mspeed += rand_spread(0, i);
	}

	if (adult_nightmare_mode) n_ptr->mspeed += randint(5);

	/* Give a random starting energy */
	n_ptr->energy = (byte)rand_int(100);

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		n_ptr->mflag |= (MFLAG_NICE);

		/* Optimize -- Repair flags */
		repair_mflag_nice = TRUE;
	}

	/* Monster is still being born */
	n_ptr->mflag |= (MFLAG_BORN);

	/* Optimize -- Repair flags */
	repair_mflag_born = TRUE;

	/* Place the monster in the dungeon */
	if (!monster_place(y, x, n_ptr)) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX	18

/*
 * Attempt to place a "group" of monsters around the given location
 *
 * Eyangband:
 *
 * Monsters usually only form groups if OOD (all monsters resident higher than they
 * were in Vanilla to balance this), though a small chance exists to form groups
 * at any depth. The deeper you go, the more monsters likely to be in the group,
 * up to the max is 24 monsters, "big" groups double the size of the group.
 */

static bool place_monster_group(int y, int x, int r_idx, bool slp, bool big, byte mode)
{
	monster_race *r_ptr = &r_info[r_idx];

	int old, n, i;
	int total, size;
	int max_size = ((big) ? (GROUP_MAX * 3) / 2 : GROUP_MAX);

	int hack_n;

	byte hack_y[GROUP_MAX*2];
	byte hack_x[GROUP_MAX*2];

	/* Determine max size of group */
	if (r_ptr->level < p_ptr->depth) size = 1 + (p_ptr->depth - r_ptr->level) * 2;
	else size = 1;

	/* Big groups */
	if (big) size = (size * 3) / 2;

	/* Pick a group size */
	total = randint(size);

	/* Small chance for a larger group */
	if (rand_int(5) == 0) total += randint(2);

	/* Minimum size */
	if (total < 1) total = 1;

	/* Maximum size */
	if (total > max_size) total = max_size;

	/* Save the rating */
	old = rating;

	/* Start on the monster */
	hack_n = 1;
	hack_x[0] = x;
	hack_y[0] = y;

	/* Puddle monsters, breadth first, up to total */
	for (n = 0; (n < hack_n) && (hack_n < total); n++)
	{
		/* Grab the location */
		int hx = hack_x[n];
		int hy = hack_y[n];

		/* Check each direction, up to total */
		for (i = 0; (i < 8) && (hack_n < total); i++)
		{
			int mx = hx + ddx_ddd[i];
			int my = hy + ddy_ddd[i];

			/* Walls and Monsters block flow */
			if (!cave_empty_bold(my, mx)) continue;

			/* Attempt to place another monster */
			if (place_monster_one(my, mx, r_idx, slp, mode))
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
 * Hack -- help pick a companion type
 */
static bool place_companion_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[place_monster_idx];

	monster_race *z_ptr = &r_info[r_idx];

	/* Skip more advanced monsters */
	if (z_ptr->level != r_ptr->level) return (FALSE);

	/* Require unique monsters */
	if (!z_ptr->cur_unique) return (FALSE);

	/* Require companion flag */
	if (!(z_ptr->flags1 & (RF1_COMPANION))) return (FALSE);

	/* Okay */
	return (TRUE);
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
 * Also, some monsters can get "companions", which are uniques from the 
 * same depth. This should be used only with uniques.
 *
 * Note the "bizarre" use of non-recursion to prevent annoying output
 * when running a code profiler.
 *
 * Note the use of the new "monster allocation table" code to restrict
 * the "get_mon_num()" function to "legal" escort types.
 */
bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp, byte mode)
{
	int i;
	u32b f1;

	monster_race *r_ptr = &r_info[r_idx];

	bool unique = ((r_ptr->flags1 & (RF1_UNIQUE)) ? TRUE : FALSE);

	/* Place one monster, or fail */
	if (!place_monster_one(y, x, r_idx, slp, mode)) return (FALSE);

	/* Require the "group" flag */
	if (!grp) return (TRUE);

	/* Get all flags for uniques */
	if (unique)
	{
		monster_unique *u_ptr;

		for (i = 1; i < z_info->u_max ; i++)
		{
			u_ptr = &u_info[i];

			if (u_ptr->r_idx == r_idx) break;
		}

		/* Paranoia */
		if (i == z_info->u_max) return (TRUE);

		/* Flags */
		f1 = (u_ptr->flags1 | r_ptr->flags1);

	}
	else f1 = r_ptr->flags1;

	/* Companions for certain monsters */
	if (f1 & (RF1_COMPANION))
	{
		/* Try to place a "companions" (several times) */
		for (i = 0; i < 10; i++)
		{
			int nx, ny, z, d = 3;

			/* Pick a location */
			scatter(&ny, &nx, y, x, d);

			/* Require empty grids */
			if (!cave_empty_bold(ny, nx)) continue;

			/* Set the escort index */
			place_monster_idx = r_idx;

			/* Set the escort hook */
			get_mon_num_hook = place_companion_okay;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Pick a random race */
			z = get_mon_num(r_ptr->level);

			/* Remove restriction */
			get_mon_num_hook = NULL;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Handle failure */
			if (!z) break;

			/* Place a single escort */
			(void)place_monster_one(ny, nx, z, slp, PLACE_UNIQ);
		}
	}

	/* Friends for certain monsters */
	if (f1 & (RF1_FRIENDS))
	{
		bool big = FALSE;

		if (f1 & RF1_MANY) big = TRUE;

		/* Attempt to place a group */
		(void)place_monster_group(y, x, r_idx, slp, big, PLACE_NO_U);
	}

	/* Escorts for certain monsters */
	if (f1 & (RF1_ESCORTS))
	{
		/* Try to place several "escorts" */
		for (i = 0; i < 50; i++)
		{
			int nx, ny, z, d = 3;

			/* Pick a location */
			scatter(&ny, &nx, y, x, d);

			/* Require empty grids */
			if (!cave_empty_bold(ny, nx)) continue;

			/* Set the escort index */
			place_monster_idx = r_idx;

			/* Set the escort hook */
			get_mon_num_hook = place_monster_okay;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Pick a random race */
			z = get_mon_num(r_ptr->level);

			/* Remove restriction */
			get_mon_num_hook = NULL;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Handle failure */
			if (!z) break;

			/* Place a single escort */
			(void)place_monster_one(ny, nx, z, slp, PLACE_NO_U);

			/* Place a "group" of escorts if needed */
			if ((r_info[z].flags1 & RF1_FRIENDS) || (f1 & (RF1_MANY)))
			{
				/* Place a group of monsters */
				(void)place_monster_group(ny, nx, z, slp, FALSE, PLACE_NO_U);
			}
		}
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
	if (place_monster_aux(y, x, r_idx, slp, grp, 0)) return (TRUE);

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
	int y, x;
	int	attempts_left = 10000;
 
	/* Find a legal, distant, unoccupied, space */
	while (--attempts_left)
	{
		/* Pick a location */
		y = rand_int(p_ptr->cur_hgt);
		x = rand_int(p_ptr->cur_wid);

		/* Require "naked" floor grid */
		if (!cave_naked_bold(y, x)) continue;

		/* Accept far away grids */
		if (distance(y, x, p_ptr->py, p_ptr->px) > dis) break;
	}

	if (!attempts_left)
	{
		if (cheat_wizard || cheat_hear)
		{
			message(MSG_CHEAT, 0, "Warning! Could not allocate a new monster.");
		}

		return FALSE;
	}

	/* Attempt to place the monster, allow groups */
	if (place_monster(y, x, slp, TRUE)) return (TRUE);

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
		case SUMMON_ANIMALS:
		{
			okay = ((r_ptr->flags4 & (RF4_ANIMAL)) &&
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

		case SUMMON_FAERY:
		{
			okay = ((r_ptr->flags4 & (RF4_FAERY)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}
		
		case SUMMON_HYDRA:
		{
			okay = ((r_ptr->d_char == 'Y') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HORROR:
		{
			okay = ((r_ptr->d_char == 'N') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_DEMON:
		{
			okay = ((r_ptr->flags4 & (RF4_DEMON)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_UNDEAD:
		{
			okay = ((r_ptr->flags4 & (RF4_UNDEAD)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_DRAGON:
		{
			okay = ((r_ptr->flags4 & (RF4_DRAGON)) &&
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
			okay = (((r_ptr->flags4 & (RF4_UNDEAD)) && (r_ptr->level > 40)) || 
				(r_ptr->d_char == 'V') || (r_ptr->d_char == 'W') || (r_ptr->d_char == 'L'));
			break;
		}

		case SUMMON_HI_DRAGON:
		{
			okay = (r_ptr->d_char == 'D');
			break;
		}

		case SUMMON_HI_DEMON:
		{
			okay = (r_ptr->d_char == 'U');
			break;
		}

		case SUMMON_UNIQUE:
		{
			/* note - forces uniques */
			okay = (r_ptr->cur_unique > 0);
			break;
		}
	}

	/* Result */
	return (okay);
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
	byte mode;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* 
		 * XXX no summon on glyph of warding 
		 * Since we don't know which monster this is yet, all anti-monster glyphs
		 * necessarily stop summoning of all monster types.
		 */
		if (trap_monster(y, x) && trap_glyph(y, x)) continue;

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
	r_idx = get_mon_num((p_ptr->depth + lev) / 2 + 5);

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Mega-hack - determine if to force, allow, or disallow uniques */
	if (type == SUMMON_UNIQUE) mode = PLACE_UNIQ;
	else if ((type == SUMMON_HI_DRAGON) || (type == SUMMON_HI_DEMON) || 
		(type == SUMMON_HI_UNDEAD))	mode = 0;
	else mode = PLACE_NO_U;

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, TRUE, mode)) return (FALSE);

	/* Success */
	return (TRUE);
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
		scatter(&y, &x, m_ptr->fy, m_ptr->fx, d);

		/* Require an "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Create a new monster (awake, no groups) */
		result = place_monster_aux(y, x, m_ptr->r_idx, FALSE, FALSE, PLACE_NO_U);

		/* Done */
		break;
	}

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
	monster_race *r_ptr = get_monster_real(m_ptr);

	char m_name[80];

	/* Get the monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Notice non-damage */
	if (dam == 0)
	{
		message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is unharmed.", m_name);
		return;
	}

	/* Note -- subtle fix -CFT */
	newhp = (long)(m_ptr->hp);
	oldhp = newhp + (long)(dam);
	tmp = (newhp * 100L) / oldhp;
	percentage = (int)(tmp);

	/* Jelly's, Mold's, Vortex's, Quthl's */
	if (strchr("jmvQ", r_ptr->d_char))
	{
		if (percentage > 95)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s barely notices.", m_name);
		else if (percentage > 75)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s flinches.", m_name);
		else if (percentage > 50)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s squelches.", m_name);
		else if (percentage > 35)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s quivers in pain.", m_name);
		else if (percentage > 20)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s writhes about.", m_name);
		else if (percentage > 10)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s writhes in agony.", m_name);
		else
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s jerks limply.", m_name);
	}

	/* Dogs and Hounds */
	else if (strchr("CZ", r_ptr->d_char))
	{
		if (percentage > 95)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s snarls with pain.", m_name);
		else if (percentage > 50)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s yelps in pain.", m_name);
		else if (percentage > 35)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s howls in pain.", m_name);
		else if (percentage > 20)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s howls in agony.", m_name);
		else if (percentage > 10)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s writhes in agony.", m_name);
		else
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s yelps feebly.", m_name);
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (strchr("FIKMRSXabclqrst", r_ptr->d_char))
	{
		if (percentage > 95)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s ignores the attack.", m_name);
		else if (percentage > 75)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s grunts with pain.", m_name);
		else if (percentage > 50)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s squeals in pain.", m_name);
		else if (percentage > 35)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s shrieks in pain.", m_name);
		else if (percentage > 20)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s shrieks in agony.", m_name);
		else if (percentage > 10)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s writhes in agony.", m_name);
		else
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s cries out feebly.", m_name);
	}

	/* Another type of monsters (shrug,cry,scream) */
	else
	{
		if (percentage > 95)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s grunts with pain.", m_name);
		else if (percentage > 50)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s cries out in pain.", m_name);
		else if (percentage > 35)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s screams in pain.", m_name);
		else if (percentage > 20)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s screams in agony.", m_name);
		else if (percentage > 10)
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s writhes in agony.", m_name);
		else
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s cries out feebly.", m_name);
	}
}

/*
 * Learn about an "observed" resistance.
 */
void update_smart_learn(int m_idx, int what)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	/* Too stupid to learn anything */
	if (r_ptr->flags1 & (RF1_STUPID)) return;

	/* Not intelligent, only learn sometimes */
	if (!(r_ptr->flags1 & (RF1_SMART)) && (rand_int(100) < 50)) return;

	/* XXX XXX XXX */

	/* Analyze the knowledge */
	switch (what)
	{
		case DRS_FREE:
		{
			if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
			break;
		}

		case DRS_MANA:
		{
			if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
			break;
		}

		case DRS_BRAVE:
		{
			if (p_ptr->bravery) m_ptr->smart |= (SM_IMM_BRAVE);
			break;
		}

		case DRS_NO_BLIND:
		{
			if (p_ptr->no_blind) m_ptr->smart |= (SM_IMM_BLIND);
			break;
		}

		case DRS_RES_ACID:
		{
			if (p_ptr->res[RS_ACD] > 20) m_ptr->smart |= (SM_RES_ACID);
			break;
		}

		case DRS_RES_ELEC:
		{
			if (p_ptr->res[RS_ELC] > 20) m_ptr->smart |= (SM_RES_ELEC);
			break;
		}

		case DRS_RES_FIRE:
		{
			if (p_ptr->res[RS_FIR] > 20) m_ptr->smart |= (SM_RES_FIRE);
			break;
		}

		case DRS_RES_COLD:
		{
			if (p_ptr->res[RS_CLD] > 20) m_ptr->smart |= (SM_RES_COLD);
			break;
		}

		case DRS_RES_POIS:
		{
			if (p_ptr->res[RS_PSN] > 20) m_ptr->smart |= (SM_RES_POIS);
			break;
		}

		case DRS_RES_LITE:
		{
			if (p_ptr->res[RS_LIT] > 20) m_ptr->smart |= (SM_RES_LITE);
			break;
		}

		case DRS_RES_DARK:
		{
			if (p_ptr->res[RS_DRK] > 20) m_ptr->smart |= (SM_RES_DARK);
			break;
		}

		case DRS_RES_CONFU:
		{
			if (p_ptr->res[RS_CNF] > 20) m_ptr->smart |= (SM_RES_CONFU);
			break;
		}

		case DRS_RES_SOUND:
		{
			if (p_ptr->res[RS_SND] > 20) m_ptr->smart |= (SM_RES_SOUND);
			break;
		}

		case DRS_RES_SHARD:
		{
			if (p_ptr->res[RS_SHR] > 20) m_ptr->smart |= (SM_RES_SHARD);
			break;
		}

		case DRS_RES_NEXUS:
		{
			if (p_ptr->res[RS_NEX] > 20) m_ptr->smart |= (SM_RES_NEXUS);
			break;
		}

		case DRS_RES_NETHR:
		{
			if (p_ptr->res[RS_NTH] > 20) m_ptr->smart |= (SM_RES_NETHR);
			break;
		}

		case DRS_RES_CHAOS:
		{
			if (p_ptr->res[RS_CHS] > 20) m_ptr->smart |= (SM_RES_CHAOS);
			break;
		}

		case DRS_RES_DISEN:
		{
			if (p_ptr->res[RS_DSN] > 20) m_ptr->smart |= (SM_RES_DISEN);
			break;
		}

		case DRS_RES_TIME:
		{
			if (p_ptr->res[RS_TIM] > 20) m_ptr->smart |= (SM_RES_TIME);
			break;
		}

		case DRS_RES_MANA:
		{
			if (p_ptr->res[RS_MNA] > 20) m_ptr->smart |= (SM_RES_MANA);
			break;
		}

		case DRS_RES_DISEASE:
		{
			if (p_ptr->res[RS_DIS] > 20) m_ptr->smart |= (SM_RES_DISEASE);
			break;
		}

		case DRS_RES_WATER:
		{
			if (p_ptr->res[RS_WTR] > 20) m_ptr->smart |= (SM_RES_WATER);
			break;
		}
	}
}

/*
 * Calculate a monster's EXP value 
 */
void mon_exp(int r_idx, int u_idx, u32b *exint, u32b *exfrac)
{
	monster_race *r_ptr = get_monster_fake(r_idx, 0, u_idx);

	int div = p_ptr->lev;

	if (!(u_idx) && 
		(rp_ptr->special==RACE_SPECIAL_ANGEL) && !(r_ptr->flags4 & (RF4_EVIL))) div *=2;

	*exint = (long)r_ptr->mexp / div;

	/* calculate the fractional exp part scaled by 100, */
	/* must use long arithmetic to avoid overflow  */
	*exfrac = (((long)r_ptr->mexp % div) * 1000L / div);
}
