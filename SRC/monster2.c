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

	s16b this_o_idx, next_o_idx = 0;


	/* Get location */
	y = m_ptr->fy;
	x = m_ptr->fx;


	/* Hack -- Reduce the racial counter */
	r_ptr->cur_num--;

	/* Hack -- count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro--;


	/* Hack -- remove target monster */
	if (i == p_ptr->target_who) p_ptr->target_who = 0;

	/* Hack -- remove tracked monster */
	if (i == p_ptr->health_who) health_track(0);


	/* Monster is gone */
	cave_m_idx[y][x] = 0;


	/* Delete objects */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- efficiency */
		o_ptr->held_m_idx = 0;

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

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Reset monster pointer */
		o_ptr->held_m_idx = i2;
	}

	/* Hack -- Update the target */
	if (p_ptr->target_who == i1) p_ptr->target_who = i2;

	/* Hack -- Update the health bar */
	if (p_ptr->health_who == i1) health_track(i2);

	/* Hack -- move monster */
	COPY(&m_list[i2], &m_list[i1], monster_type);

	/* Hack -- wipe hole */
	WIPE(&m_list[i1], monster_type);
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
	if (size && alert_failure) msg_print("Compacting monsters...");


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

			/* Check for quest completion */
			if (r_ptr->flags1 & (RF1_QUESTOR)) check_quest(r_ptr, m_ptr, FALSE);

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

		/* Mega-Hack -- preserve Ghosts */
		if (m_ptr->r_idx >= RACE_MAX_NORMAL)
		{
			save_bones(m_ptr->r_idx - RACE_MAX_NORMAL);
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

	/* Hack -- reset "reproducer" count */
	num_repro = 0;

	/* Hack -- no more target */
	p_ptr->target_who = 0;

	/* Hack -- no more tracking */
	health_track(0);
}


/*
 * Acquires and returns the index of a "free" monster race.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
s16b r_pop(void)
{
	int i;

	/* Initial allocation */
	if (r_max < MAX_R_IDX)
	{
		/* Get next space */
		i = r_max;

		/* Expand monster race array */
		r_max++;

		r_cnt++;

		/* Use this race */
		return (i);
	}

	r_cnt = RACE_MAX_NORMAL;

	/* Recycle dead races */
	for (i = RACE_MAX_NORMAL; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr;

		/* Acquire race */
		r_ptr = &r_info[i];

		/* Skip live races */
		if (r_ptr->cur_num) continue;

		r_cnt++;

		/* Use this race */
		return (i);
	}

	/* Warn the player */
	if (character_dungeon && alert_failure) msg_print("Too many monster races!");

	/* Oops */
	return (0);
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
	if (m_max < MAX_M_IDX)
	{
		/* Access the next hole */
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

		/* Acquire monster */
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


#if 0
/*
 * Prepare a monster race based on an existing race.
 */
void race_prep(int r_idx, int s_idx)
{
	int i;

	monster_race *r_ptr = &r_info[r_idx];
	monster_race *s_ptr = &r_info[s_idx];
	monster_lore *l_ptr = &l_info[r_idx];
	monster_lore *m_ptr = &l_info[s_idx];

	strcpy (r_name + r_ptr->name, r_name + s_ptr->name);

	r_ptr->hdice = s_ptr->hdice;
	r_ptr->hside = s_ptr->hside;

	r_ptr->ac = s_ptr->ac;

	r_ptr->sleep = s_ptr->sleep;
	r_ptr->aaf = s_ptr->aaf;
	r_ptr->speed = s_ptr->speed;

	r_ptr->mexp = s_ptr->mexp;

	r_ptr->wt = s_ptr->wt;

	r_ptr->freq_inate = s_ptr->freq_inate;
	r_ptr->freq_spell = s_ptr->freq_spell;

	r_ptr->flags1 = s_ptr->flags1;
	r_ptr->flags2 = s_ptr->flags2;
	r_ptr->flags3 = s_ptr->flags3;
	r_ptr->flags4 = s_ptr->flags4;
	r_ptr->flags5 = s_ptr->flags5;
	r_ptr->flags6 = s_ptr->flags6;

	/* Copy the blows */
	for (i = 0; i < 4; i++)
	{
		COPY(&r_ptr->blow[i], &s_ptr->blow[i], monster_blow);
	}

	/* Copy the inventory */
	for (i = 0; i < 10; i++)
	{
		COPY(&r_ptr->inv[i], &s_ptr->inv[i], monster_inv);
	}

	r_ptr->level = s_ptr->level;
	r_ptr->rarity = s_ptr->rarity;

	r_ptr->d_attr = s_ptr->d_attr;
	r_ptr->d_char = s_ptr->d_char;

	r_ptr->x_attr = s_ptr->x_attr;
	r_ptr->x_char = s_ptr->x_char;

	r_ptr->max_num = s_ptr->max_num;
	r_ptr->cur_num = s_ptr->cur_num;

	COPY(l_ptr, m_ptr, monster_lore);
}
#endif


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

		/* Access the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Access the actual race */
		r_ptr = &r_info[r_idx];

		/* Hack -- "unique" monsters must be "unique" */
		if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
		{
			continue;
		}

		/* Depth Monsters never appear out of depth */
		if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (r_ptr->level > p_ptr->depth))
		{
			continue;
		}

		/* The number of quest monsters is fixed */
		if ((r_ptr->flags1 & RF1_QUESTOR) && (p_ptr->quest_cur < p_ptr->quest_max))
		{
			continue;
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

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	cptr name = (r_name + r_ptr->name);

	bool seen, pron;

	int h_idx;

	if (p_ptr->image)
	{
		h_idx = rand_int(RACE_MAX_NORMAL);
		r_ptr = &r_info[h_idx];
		name = (r_name + r_ptr->name);
	}


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
		if (!(r_ptr->flags1 & RF1_LIMIT) && (r_ptr->flags1 & (RF1_UNIQUE)))
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
void lore_do_probe(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_lore *l_ptr = &l_info[m_ptr->r_idx];

	/* Hack -- Memorize some flags */
	l_ptr->flags1 = l_ptr->flags1;
	l_ptr->flags2 = l_ptr->flags2;
	l_ptr->flags3 = l_ptr->flags3;

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

	monster_lore *l_ptr = &l_info[m_ptr->r_idx];

	/* Note the number of things dropped */
	if (num_item > l_ptr->drop_item) l_ptr->drop_item = num_item;
	if (num_gold > l_ptr->drop_gold) l_ptr->drop_gold = num_gold;

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
 * This involves extracting the distance to the player (if needed),
 * and then checking for visibility (natural, infravision, see-invis,
 * telepathy), updating the monster visibility flag, redrawing (or
 * erasing) the monster when the visibility changes, and taking note
 * of any interesting monster flags (cold-blooded, invisible, etc).
 *
 * Note the new "mflag" field which encodes several monster state
 * flags, including "view" for "currently in line of sight" and
 * "mark" for "currently visible via detection".
 *
 * The only monster fields that are changed here are "cdis" (the
 * distance from the player), "ml" (visible to the player), and
 * "mflag" (to maintain the "MFLAG_VIEW" flag).
 *
 * Note the special "update_monsters()" function which can be used
 * to call this function for every monster.
 *
 * Note the "full" flag which requests that the "cdis" field be
 * updated, often, such an update is not needed.
 *
 * Every time a monster moves, we must call this function for that
 * monster, and update the distance, and the visibility.  Every time
 * the player moves, we must call this function for every monster, and
 * update the distance, and the visibility.  Whenever the player "state"
 * changes in certain ways ("blindness", "infravision", "telepathy",
 * and "see invisible"), we must call this function for every monster,
 * and update the visibility.
 *
 * Routines that change the "illumination" of grids must also call
 * this function, since the "visibility" of some monsters may be
 * based on the illumination of their grid.
 *
 * Note that this function is called once per monster every time the
 * player moves.  When the player is running, this function is one
 * of the primary bottlenecks, along with "update_view()" and the
 * "process_monsters()" code, so efficiency is important.
 *
 * Note the optimized "inline" version of the "distance()" function.
 *
 * Note the optimized "inline" version of the "player_can_see_bold()"
 * function, which has been so optimized that it no longer has the
 * same semantics, in particular, monsters in perma-lit walls can
 * be seen even from the wrong side of the wall.
 *
 * A monster is "visible" to the player if (1) it has been detected
 * by the player, (2) it is close to the player and the player has
 * telepathy, or (3) it is close to the player, and in line of sight
 * of the player, and it is "illuminated" by some combination of
 * light, infravision, and/or see-invisible.
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
	monster_lore *l_ptr = &l_info[m_ptr->r_idx];

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
			}
		}

		/* Normal line of sight, and not blind */
		if ((cave_info[fy][fx] & (CAVE_VIEW)) && !p_ptr->blind)
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

			/* Use "illumination" */
			if (cave_info[fy][fx] & (CAVE_LITE | CAVE_GLOW))
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
				if (do_invisible) l_ptr->flags2 |= (RF2_INVISIBLE);
				if (do_cold_blood) l_ptr->flags2 |= (RF2_COLD_BLOOD);
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

			/* Hack -- Count "fresh" sightings */
			if (l_ptr->sights < MAX_SHORT) l_ptr->sights++;

			/* Disturb on appearance */
			if (disturb_move) disturb(1, 0);
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

			/* Disturb on disappearance */
			if (disturb_move) disturb(1, 0);
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

			/* Disturb on disappearance */
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

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
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

		/* Memorize monster */
		o_ptr->held_m_idx = m_idx;

		/* Build stack */
		o_ptr->next_o_idx = m_ptr->hold_o_idx;

		/* Build stack */
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
		update_mon(m1, TRUE);
	}

	/* Player 1 */
	else if (m1 < 0)
	{
		/* Move player */
		p_ptr->py = y2;
		p_ptr->px = x2;

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

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
		update_mon(m2, TRUE);
	}

	/* Player 2 */
	else if (m2 > 0)
	{
		/* Move player */
		p_ptr->py = y1;
		p_ptr->px = x1;

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

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

		/* Acquire new monster */
		m_ptr = &m_list[m_idx];

		/* Copy the monster XXX */
		COPY(m_ptr, n_ptr, monster_type);

		/* Location */
		m_ptr->fy = y;
		m_ptr->fx = x;

		/* Update the monster */
		update_mon(m_idx, TRUE);

		/* Acquire new race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Hack -- Notice new multi-hued monsters */
		if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

		/* Hack -- Count the number of "reproducers" */
		if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;

		/* Count racial occurances */
		r_ptr->cur_num++;
	}

	/* Result */
	return (m_idx);
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
	int i, n, m_idx;

	monster_race *r_ptr;
	monster_xtra *x_ptr;

	monster_type *n_ptr;
	monster_type monster_type_body;

	monster_inv	*i_ptr;

	object_type *o_ptr;
	object_type object_type_body;

	byte tval = 0;

	cptr name;


	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Hack -- no creation on glyph of warding */
	if (cave_feat[y][x] == FEAT_GLYPH) return (FALSE);


	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Race */
	r_ptr = &r_info[r_idx];
	x_ptr = &x_info[r_idx];

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
	WIPE(n_ptr, monster_type);


	/* Save the race */
	n_ptr->r_idx = r_idx;


	/* Enforce sleeping if needed */
	if (slp && r_ptr->sleep)
	{
		int val = r_ptr->sleep;
		n_ptr->csleep = ((val * 2) + randint(val * 10));
	}

	/* Assign maximal hitpoints */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
	{
		n_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		n_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* And start out fully healthy */
	n_ptr->hp = n_ptr->maxhp;


	/* Extract the monster base speed */
	n_ptr->bspeed = r_ptr->speed;

	/* Allow some small variation per monster */
	i = extract_energy[r_ptr->speed] / 10;
	if (i) n_ptr->bspeed = randnor(n_ptr->bspeed, i / 4);

	/* Give a random starting energy */
	n_ptr->energy = rand_int(100);

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

	m_idx = cave_m_idx[y][x];

	/* Obtain local object */
	o_ptr = &object_type_body;
	i_ptr = &x_ptr->inv[0];

	object_level = (r_ptr->level + p_ptr->depth) / 2;

	/* Handle creeping coins */
	coin_type = get_coin_type(r_ptr);

	for (i = 0; i < 10 && i_ptr[i].tval; i++)
	{
		/* Prevent level restarts */
		if (o_max >= MAX_O_IDX - 100) break;

		/* Check for random tvals */
		switch (i_ptr[i].tval)
		{
			/* Pick a single tval */
			case TV_SINGLE:
			{
				tval = TV_ANY_ITEM;
				break;
			}
			/* Pick a tval set */
			case TV_MULTI:
			{
				tval = rand_int(TV_MAX_MULTI - TV_MIN_MULTI + 1) + TV_MIN_MULTI;
				break;
			}
			/* Use the current tval */
			default:
			{
				tval = i_ptr[i].tval;
			}
		}

		n = damroll(i_ptr[i].q_dice, i_ptr[i].q_side);
		for (; n > 0; n--)
		{
			/* Sometimes monsters don't have items */
			if (rand_int(100) >= i_ptr[i].chance) continue;

			object_wipe(o_ptr);
			o_ptr->tval = tval;

			if (o_ptr->tval != TV_GOLD)
			{
				if (make_object(o_ptr, i_ptr[i].good, i_ptr[i].great))
				{
					/* Only give real objects to monsters */
					monster_carry(m_idx, o_ptr);
				}
			}
			else
			{
				if (make_gold(o_ptr))
				{
					/* Only give real objects to monsters */
					monster_carry(m_idx, o_ptr);
				}
			}

			/* Don't pick another tval if we don't want one */
			if (i_ptr[i].tval == TV_SINGLE)
			{
				tval = o_ptr->tval;
			}
		}
	}

	object_level = p_ptr->depth;

	/* Reset coin type */
	coin_type = 0;

	/* Give the ghosts their inventory */
	if (r_idx >= RACE_MAX_NORMAL)
	{
		ghost_type *g_ptr = &g_info[r_idx - RACE_MAX_NORMAL];

		/* Mark the level */
		if (!character_dungeon) ghost_flag = TRUE;

		for (i = 0; i < INVEN_TOTAL; i++)
		{
			o_ptr = &g_ptr->inv[i];

			if (!o_ptr->k_idx) continue;

			/* Give the ghost the item */
			monster_carry(m_idx, o_ptr);
		}
	}

	/* Success */
	return (TRUE);
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX	32


/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int y, int x, int r_idx, bool slp)
{
	monster_race *r_ptr = &r_info[r_idx];

	int old, n, i;
	int total = 0, extra = 0;

	int hack_n = 0;

	byte hack_y[GROUP_MAX];
	byte hack_x[GROUP_MAX];


	/* Pick a group size */
	total = randint(13);

	/* Hard monsters, small groups */
	if (r_ptr->level > p_ptr->depth)
	{
		extra = r_ptr->level - p_ptr->depth;
		extra = 0 - randint(extra);
	}

	/* Easy monsters, large groups */
	else if (r_ptr->level < p_ptr->depth)
	{
		extra = p_ptr->depth - r_ptr->level;
		extra = randint(extra);
	}

	/* Hack -- limit group reduction */
	if (extra > 12) extra = 12;

	/* Modify the group size */
	total += extra;

	/* Minimum size */
	if (total < 1) total = 1;

	/* Maximum size */
	if (total > GROUP_MAX) total = GROUP_MAX;


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
			int mx, my;

			scatter(&my, &mx, hy, hx, 4, 0);

			/* Walls and Monsters block flow */
			if (!cave_empty_bold(my, mx)) continue;

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
bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp)
{
	int i;

	monster_race *r_ptr = &r_info[r_idx];


	/* Place one monster, or fail */
	if (!place_monster_one(y, x, r_idx, slp)) return (FALSE);


	/* Require the "group" flag */
	if (!grp) return (TRUE);


	/* Friends for certain monsters */
	if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		/* Attempt to place a group */
		(void)place_monster_group(y, x, r_idx, slp);
	}


	/* Escorts for certain monsters */
	if (r_ptr->flags1 & (RF1_ESCORT))
	{
		/* Try to place several "escorts" */
		for (i = 0; i < 50; i++)
		{
			int nx, ny, z, d = 3;

			/* Pick a location */
			scatter(&ny, &nx, y, x, d, 0);

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
			(void)place_monster_one(ny, nx, z, slp);

			/* Place a "group" of escorts if needed */
			if ((r_info[z].flags1 & (RF1_FRIENDS)) ||
				 (r_ptr->flags1 & (RF1_ESCORTS)))
			{
				/* Place a group of monsters */
				(void)place_monster_group(ny, nx, z, slp);
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
 *   (G) 1 Poltergeist, 1 Spirit, 1 Ghost, 1 Shadow, 1 Phantom
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
bool alloc_monster(int dis, int slp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	/* Find a legal, distant, unoccupied, space */
	while (1)
	{
		/* Pick a location */
		y = rand_range(1, DUNGEON_HGT - 2);
		x = rand_range(1, DUNGEON_WID - 2);

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
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	cptr name = (r_name + r_ptr->name);

	bool okay = FALSE;

	/* Hack -- no specific type specified */
	if (!summon_specific_type) return (TRUE);

	/* 
	 * If the value is positive, assume it stores the character desired.
	 * Allow Uniques.
	 */
	if (summon_specific_type > 0)
	{
		return (r_ptr->d_char == (char)summon_specific_type);
	}
	
	/* Check our requirements */
	switch (summon_specific_type)
	{
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

		case SUMMON_ORC:
		{
			okay = ((r_ptr->d_char == 'o') &&
					  !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_ANGBAND:
		{
			okay = ((r_ptr->d_char == 'o') ||
					  (r_ptr->d_char == 'O') ||
					  (r_ptr->d_char == 'T') ||
					  (r_ptr->d_char == 'd') ||
					  (r_ptr->d_char == 'D') ||
					  (strstr(name,"balrog")) ||
					  (strstr(name,"Balrog")));
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
			okay = (r_ptr->flags1 & (RF1_UNIQUE));
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
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH (XXX) will summon Unique's
 * Note: SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Unique's
 * Note: None of the other summon codes will ever summon Unique's.
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

	/* Pick a monster, using the level calculation */
	r_idx = get_mon_num((p_ptr->depth + lev) / 2 + 5);


	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, TRUE)) return (FALSE);

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

	/* Require some health */
	if (m_ptr->hp == 1) return FALSE;

	/* Try up to 18 times */
	for (i = 0; i < 18; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, m_ptr->fy, m_ptr->fx, d, 0);

		/* Require an "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Create a new monster (awake, no groups) */
		result = place_monster_aux(y, x, m_ptr->r_idx, FALSE, FALSE);

		/* Done */
		break;
	}

	/* Health is divided */
	if (result)
	{
		m_ptr->hp = m_ptr->hp / 2;

		m_ptr = &m_list[cave_m_idx[y][x]];

		m_ptr->hp = m_ptr->hp / 2;
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


	/* Jellies, Molds, Vortices, Quylthulgs, Eyes */
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


	/* Snakes, Hydrae, Reptiles, Mimics */
	else if (strchr("JMR?!$|", r_ptr->d_char))
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


	/* Ants, Centipedes, Flies, Insects, Beetles, Spiders */
	else if (strchr("acFIKS", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s chitters.", m_name);
		else if (percentage > 50)
			msg_format("%^s scuttles about.", m_name);
		else if (percentage > 35)
			msg_format("%^s twitters.", m_name);
		else if (percentage > 20)
			msg_format("%^s jerks in pain.", m_name);
		else if (percentage > 10)
			msg_format("%^s jerks in agony.", m_name);
		else
			msg_format("%^s twitches.", m_name);
	}


	/* Birds */
	else if (strchr("B", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s chirps.", m_name);
		else if (percentage > 75)
			msg_format("%^s twitters.", m_name);
		else if (percentage > 50)
			msg_format("%^s squawks.", m_name);
		else if (percentage > 35)
			msg_format("%^s chatters.", m_name);
		else if (percentage > 20)
			msg_format("%^s jeers.", m_name);
		else if (percentage > 10)
			msg_format("%^s flutters about.", m_name);
		else
			msg_format("%^s squeaks.", m_name);
	}


	/* Dragons, Demons, High Undead */
	else if (strchr("duDLUW", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s flinches.", m_name);
		else if (percentage > 50)
			msg_format("%^s hisses in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s snarls with pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s roars with pain.", m_name);
		else if (percentage > 10)
			msg_format("%^s gasps.", m_name);
		else
			msg_format("%^s snarls feebly.", m_name);
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
	else if (strchr("Xbilqrst", r_ptr->d_char))
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
 * Learn about an "observed" resistance.
 */
void update_smart_learn(int m_idx, int what)
{

#ifdef DRS_SMART_OPTIONS

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];


	/* Not allowed to learn */
	if (!smart_learn) return;

	/* Too stupid to learn anything */
	if (r_ptr->flags2 & (RF2_STUPID)) return;

	/* Not intelligent, only learn sometimes */
	if (!(r_ptr->flags2 & (RF2_SMART)) && (rand_int(100) < 50)) return;


	/* XXX XXX XXX */

	/* Analyze the knowledge */
	switch (what)
	{
		case DRS_FREE:
		{
			if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
			else m_ptr->smart &= ~(SM_IMM_FREE);
			break;
		}

		case DRS_MANA:
		{
			if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
			break;
		}

		case DRS_RES_ACID:
		{
			if (p_ptr->resist_acid) m_ptr->smart |= (SM_RES_ACID);
			else m_ptr->smart &= ~(SM_RES_ACID);
			if (p_ptr->oppose_acid) m_ptr->smart |= (SM_OPP_ACID);
			else m_ptr->smart &= ~(SM_OPP_ACID);
			if (p_ptr->immune_acid) m_ptr->smart |= (SM_IMM_ACID);
			else m_ptr->smart &= ~(SM_IMM_ACID);
			break;
		}

		case DRS_RES_ELEC:
		{
			if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
			else m_ptr->smart &= ~(SM_RES_ELEC);
			if (p_ptr->oppose_elec) m_ptr->smart |= (SM_OPP_ELEC);
			else m_ptr->smart &= ~(SM_OPP_ELEC);
			if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
			else m_ptr->smart &= ~(SM_IMM_ELEC);
			break;
		}

		case DRS_RES_FIRE:
		{
			if (p_ptr->resist_fire) m_ptr->smart |= (SM_RES_FIRE);
			else m_ptr->smart &= ~(SM_RES_FIRE);
			if (p_ptr->oppose_fire) m_ptr->smart |= (SM_OPP_FIRE);
			else m_ptr->smart &= ~(SM_OPP_FIRE);
			if (p_ptr->immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
			else m_ptr->smart &= ~(SM_IMM_FIRE);
			break;
		}

		case DRS_RES_COLD:
		{
			if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
			else m_ptr->smart &= ~(SM_RES_COLD);
			if (p_ptr->oppose_cold) m_ptr->smart |= (SM_OPP_COLD);
			else m_ptr->smart &= ~(SM_OPP_COLD);
			if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);
			else m_ptr->smart &= ~(SM_IMM_COLD);
			break;
		}

		case DRS_RES_POIS:
		{
			if (p_ptr->resist_pois) m_ptr->smart |= (SM_RES_POIS);
			else m_ptr->smart &= ~(SM_RES_POIS);
			if (p_ptr->oppose_pois) m_ptr->smart |= (SM_OPP_POIS);
			else m_ptr->smart &= ~(SM_OPP_POIS);
			break;
		}

		case DRS_RES_FEAR:
		{
			if (p_ptr->resist_fear) m_ptr->smart |= (SM_RES_FEAR);
			else m_ptr->smart &= ~(SM_RES_FEAR);
			break;
		}

		case DRS_RES_LITE:
		{
			if (p_ptr->resist_lite) m_ptr->smart |= (SM_RES_LITE);
			else m_ptr->smart &= ~(SM_RES_LITE);
			break;
		}

		case DRS_RES_DARK:
		{
			if (p_ptr->resist_dark) m_ptr->smart |= (SM_RES_DARK);
			else m_ptr->smart &= ~(SM_RES_DARK);
			break;
		}

		case DRS_RES_BLIND:
		{
			if (p_ptr->resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			else m_ptr->smart &= ~(SM_RES_BLIND);
			break;
		}

		case DRS_RES_CONFU:
		{
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			else m_ptr->smart &= ~(SM_RES_CONFU);
			break;
		}

		case DRS_RES_SOUND:
		{
			if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			else m_ptr->smart &= ~(SM_RES_SOUND);
			break;
		}

		case DRS_RES_SHARD:
		{
			if (p_ptr->resist_shard) m_ptr->smart |= (SM_RES_SHARD);
			else m_ptr->smart &= ~(SM_RES_SHARD);
			break;
		}

		case DRS_RES_NEXUS:
		{
			if (p_ptr->resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
			else m_ptr->smart &= ~(SM_RES_NEXUS);
			break;
		}

		case DRS_RES_NETHR:
		{
			if (p_ptr->resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
			else m_ptr->smart &= ~(SM_RES_NETHR);
			break;
		}

		case DRS_RES_CHAOS:
		{
			if (p_ptr->resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
			else m_ptr->smart &= ~(SM_RES_CHAOS);
			break;
		}

		case DRS_RES_DISEN:
		{
			if (p_ptr->resist_disen) m_ptr->smart |= (SM_RES_DISEN);
			else m_ptr->smart &= ~(SM_RES_DISEN);
			break;
		}
	}

#endif /* DRS_SMART_OPTIONS */

}


