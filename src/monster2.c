/* File: monster2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "learn.h"
#include "option.h"
#include "raceflag.h"
#include "summon.h"

#include "keypad.h"

/*
 * There is a 1/50 (2%) chance of inflating the requested monster_level
 * during the creation of a monsters (see "get_mon_num()" in "monster2.c").
 * Lower values yield harder monsters more often.
 */
#define NASTY_MON	50		/* 1/chance of inflated monster level */

/*
 * Pronoun arrays, by gender
 */
static const char* const wd_himself[3] =
{ "itself", "himself", "herself" };


/*
 * Delete a monster by index.
 *
 * When a monster is deleted, all of its objects are deleted.
 */
void delete_monster_idx(const m_idx_type i)
{
	monster_type* const m_ptr = mon_list+i;
	monster_race* const r_ptr = m_ptr->race();

	s16b this_o_idx, next_o_idx = 0;


	/* Get location */
	coord g = m_ptr->loc;

	/* Hack -- Reduce the racial counter */
	r_ptr->cur_num--;

	/* Hack -- count the number of "reproducers" */
	if (r_ptr->flags[1] & RF1_MULTIPLY) num_repro--;

	/* Hack -- remove target monster */
	if (p_ptr->target_who == i) target_set_monster(0);

	/* Hack -- remove tracked monster */
	if (p_ptr->health_who == i) health_track(0);


	/* Monster is gone */
	cave_m_idx[g.y][g.x] = 0;


	/* Delete objects */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- efficiency */
		o_ptr->held_m_idx = 0;

		/* Delete the object */
		delete_object_idx(this_o_idx);
	}


	/* Wipe the Monster */
	WIPE(m_ptr);

	/* Count monsters */
	mon_cnt--;


	/* Visual update */
	lite_spot(g);
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(coord g)
{
	/* Paranoia */
	if (!in_bounds(g.y, g.x)) return;

	/* Delete the monster (if any) */
	if (cave_m_idx[g.y][g.x] > 0) delete_monster_idx(cave_m_idx[g.y][g.x]);
}


/*
 * Move a monster from index i1 to index i2 in the object list
 */
static void compact_monsters_aux(int i1, int i2)
{
	/* Do nothing */
	if (i1 == i2) return;

	monster_type* const m_ptr = &mon_list[i1];	/* Old monster */
	s16b this_o_idx;
	s16b next_o_idx = 0;

	/* Update the cave */
	cave_m_idx[m_ptr->loc.y][m_ptr->loc.x] = i2;

	/* Repair objects being carried by monster */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

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
	mon_list[i2] = mon_list[i1];

	/* Hack -- wipe hole */
	WIPE(m_ptr);
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

	/* Message (only if compacting) */
	if (size) msg_print("Compacting monsters...");


	/* Compact at least 'size' objects */
	for (num = 0, cnt = 1; num < size; cnt++)
	{
		const int cur_lev = 5 * cnt;	/* Get more vicious each iteration */
		const int cur_dis = 5 * (20 - cnt);	/* Get closer each iteration */

		/* Check all the monsters */
		for (i = 1; i < mon_max; i++)
		{
			const monster_type* const m_ptr = &mon_list[i];
			const monster_race* const r_ptr = m_ptr->race();

			/* Paranoia -- skip "dead" monsters */
			if (!m_ptr->r_idx) continue;

			/* Hack -- High level monsters start out "immune" */
			if (r_ptr->level > cur_lev) continue;

			/* Ignore nearby monsters */
			if ((cur_dis > 0) && (m_ptr->cdis < cur_dis)) continue;

			/* Only compact "Quest" Monsters in emergencies */
			if ((r_ptr->flags[0] & RF0_QUESTOR) && (cnt < 1000)) continue;

			{	/* Saving throw chance; try not to compact Unique Monsters */
			int chance = (r_ptr->flags[0] & RF0_UNIQUE) ? 99 : 90;

			/* All monsters get a saving throw */
			if (rand_int(100) < chance) continue;
			}

			/* Delete the monster */
			delete_monster_idx(i);

			/* Count the monster */
			num++;
		}
	}


	/* Excise dead monsters (backwards!) */
	for (i = mon_max - 1; i >= 1; i--)
	{
		/* Skip real monsters */
		if (mon_list[i].r_idx) continue;

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
		monster_type* const m_ptr = &mon_list[i];
		monster_race* const r_ptr = m_ptr->race();

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Mega-Hack -- preserve Unique's XXX XXX XXX */

		/* Hack -- Reduce the racial counter */
		r_ptr->cur_num--;

		/* Monster is gone */
		cave_m_idx[m_ptr->loc.y][m_ptr->loc.x] = 0;

		/* Wipe the Monster */
		WIPE(m_ptr);
	}

	mon_max = 1;			/* Reset "mon_max" */
	mon_cnt = 0;			/* Reset "mon_cnt" */
	num_repro = 0;			/* Reset "reproducer" count */
	target_set_monster(0);	/* No more target */
	health_track(0);		/* No more tracking */
}


/*
 * Get and return the index of a "free" monster.
 *
 * This routine should almost never fail, but it *can* happen.
 */
static s16b mon_pop(void)
{
	int i;


	/* Normal allocation */
	if (mon_max < z_info->m_max)
	{
		i = mon_max;	/* Get the next hole */
		mon_max++;		/* Expand the array */
		mon_cnt++;		/* Count monsters */
		return (i);		/* Return the index */
	}


	/* Recycle dead monsters */
	for (i = 1; i < mon_max; i++)
	{
		/* Skip live monsters */
		if (mon_list[i].r_idx) continue;
		mon_cnt++;	/* Count monsters */
		return (i);	/* Use this monster */
	}


	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) msg_print("Too many monsters!");

	/* Try not to crash */
	return (0);
}


/*
 * Apply a "monster restriction function" to the "monster allocation table"
 */
errr get_mon_num_prep(int_test* get_mon_num_hook)
{
	int i;

	/* Scan the allocation table */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Get the entry */
		alloc_entry* const entry = &alloc_race_table[i];

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
 * Pick a monster.
 */
static int pick_monster(long total, alloc_entry *table)
{
	int i;

	/* Pick a monster */
	long value = rand_int(total);

	/* Find the monster */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value -= table[i].prob3;
	}
	return i;
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
	int i, p;
	int r_idx;
	long total = 0L;	/* Reset total */

	monster_race *r_ptr;

	alloc_entry *table = alloc_race_table;


	/* Boost the level */
	if (level > 0)
	{
		int boost_index = rand_int(NASTY_MON*NASTY_MON);

		/* KBB: following is not worth a function. */
		/* Occasional "nasty" monster */
		if (2*NASTY_MON-1 > boost_index)
		{
			int d = level / 4 + 2;	/* Pick a level bonus */
			level += MIN(d, 5);		/* Boost the level */
		}

		/* Occasional "nasty" monster */
		if (0 == boost_index)
		{
			int d = level / 4 + 2;	/* Pick a level bonus */
			level += MIN(d, 5);		/* Boost the level */
		}
	}


	/* Process probabilities */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Default */
		table[i].prob3 = 0;

		/* No monsters more powerful than requested */
		if (table[i].level > level) continue;

		/* No town monsters in dungeon */
		if ((level > 0) && (table[i].level <= 0)) continue;

		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &monster_type::r_info[r_idx];

		/* Hack -- "unique" monsters must be "unique" */
		if ((r_ptr->flags[0] & RF0_UNIQUE) &&
		    (r_ptr->cur_num >= r_ptr->max_num))
			continue;

		/* Depth Monsters never appear out of depth */
		if ((r_ptr->flags[0] & RF0_FORCE_DEPTH) &&
			(r_ptr->level > p_ptr->depth))
			continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Total */
		total += table[i].prob3;
	}

	
	if (total <= 0) return 0;	/* No legal monsters */


	/* Pick a monster */
	i = pick_monster(total, alloc_race_table);

	/* Power boost */
	p = rand_int(10);

	/* KBB: following is not worth a function. */
	/* Try for a "harder" monster once (50%) or twice (10%) */
	if (p < 6)
	{
		int j = i;	/* Save old */

		i = pick_monster(total, alloc_race_table);

		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
	}

	/* Try for a "harder" monster twice (10%) */
	if (p < 1)
	{
		int j = i;	/* Save old */

		i = pick_monster(total, alloc_race_table);

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

	const char* m_name;
	char buf[80];

	monster_race *r_ptr;

	u16b* race_counts;


	/* Allocate the array */
	C_MAKE(race_counts, z_info->r_max, u16b);

	/* Iterate over mon_list */
	for (idx = 1; idx < mon_max; idx++)
	{
		const monster_type* const m_ptr = &mon_list[idx];

		/* Only visible monsters */
		if (!m_ptr->ml) continue;

		/* Bump the count for this race */
		race_counts[m_ptr->r_idx]++;
	}


	/* Iterate over mon_list ( again :-/ ) */
	for (idx = 1; idx < mon_max; idx++)
	{
		const monster_type* const m_ptr = &mon_list[idx];

		/* Only visible monsters */
		if (!m_ptr->ml) continue;

		/* Do each race only once */
		if (!race_counts[m_ptr->r_idx]) continue;

		/* Get monster race */
		r_ptr = m_ptr->race();

		/* Get the monster name */
		m_name = r_ptr->name();

		/* Obtain the length of the description */
		n = strlen(m_name);

		/* Display the entry itself */
		Term_putstr(0, line, n, TERM_WHITE, m_name);

		/* Append the "standard" attr/char info */
		Term_addstr(-1, TERM_WHITE, " ('");
		Term_addch(r_ptr->d_attr, r_ptr->d_char);
		Term_addstr(-1, TERM_WHITE, "')");
		n += 6;

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

		Term_addstr(-1, TERM_WHITE, "'):");
		n += 7;

		/* Add race count */
		sprintf(buf, "%d", race_counts[m_ptr->r_idx]);
		Term_addch(TERM_WHITE, '[');
		Term_addstr(strlen(buf), TERM_WHITE, buf);
		Term_addch(TERM_WHITE, ']');
		n += strlen(buf) + 2;

		/* Don't do this race again */
		race_counts[m_ptr->r_idx] = 0;

		/* Erase the rest of the line */
		Term_erase(n, line, 255);

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
 *   0x08 --> Use indefinites for visible monsters ("a kobold")
 *   0x10 --> Pronominalize hidden monsters
 *   0x20 --> Pronominalize visible monsters
 *   0x40 --> Assume the monster is hidden
 *   0x80 --> Assume the monster is visible
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
	const char* res;

	monster_race *r_ptr = m_ptr->race();
	const char* name = r_ptr->name();

	/* Can we "see" it (forced, or not hidden + visible) */
	bool seen = ((mode & (0x80)) || (!(mode & (0x40)) && m_ptr->ml));

	/* Sexed Pronouns (seen and forced, or unseen and allowed) */
	bool pron = ((seen && (mode & (0x20))) || (!seen && (mode & (0x10))));


	/* First, try using pronouns, or describing hidden monsters */
	if (!seen || pron)
	{
		/* an encoding of the monster "sex" */
		int kind = 0x00;

		/* Extract the gender (if applicable) */
		if (r_ptr->flags[0] & (RF0_FEMALE)) kind = 0x20;
		else if (r_ptr->flags[0] & (RF0_MALE)) kind = 0x10;

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
		my_strcpy(desc, wd_himself[race_gender_index(*r_ptr)], max);
	}


	/* Handle all other visible monster requests */
	else
	{
		/* It could be a Unique */
		if (r_ptr->flags[0] & RF0_UNIQUE)
		{
			/* Start with the name (thus nominative and objective) */
			my_strcpy(desc, name, max);
		}

		/* It could be an indefinite monster */
		else if (mode & 0x08)
		{
			/* XXX Check plurality for "some" */

			/* Indefinite monsters need an indefinite article */
			my_strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ", max);
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

		/* Mention "offscreen" monsters XXX XXX */
		if (!panel_contains(m_ptr->loc))
		{
			/* Append special notation */
			my_strcat(desc, " (offscreen)", max);
		}
	}
}




/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(const m_idx_type m_idx)
{
	monster_type* const m_ptr = mon_list+m_idx;
	monster_race* const r_ptr = m_ptr->race();
	monster_lore* const l_ptr = m_ptr->lore();


	/* Hack -- Memorize some flags */
	C_COPY(l_ptr->flags, r_ptr->flags, RACE_FLAG_STRICT_UB);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx) p_ptr->redraw |= (PR_MONSTER);
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
void lore_treasure(const m_idx_type m_idx, int num_item, int num_gold)
{
	monster_type* const m_ptr = mon_list+m_idx;
	monster_race* const r_ptr = m_ptr->race();
	monster_lore* const l_ptr = m_ptr->lore();


	/* Note the number of things dropped */
	if (num_item > l_ptr->drop_item) l_ptr->drop_item = num_item;
	if (num_gold > l_ptr->drop_gold) l_ptr->drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (r_ptr->flags[0] & RF0_DROP_GOOD) l_ptr->flags[0] |= RF0_DROP_GOOD;
	if (r_ptr->flags[0] & RF0_DROP_GREAT) l_ptr->flags[0] |= RF0_DROP_GREAT;

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx) p_ptr->redraw |= (PR_MONSTER);
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
void update_mon(const m_idx_type m_idx, bool full)
{
	monster_type* m_ptr = mon_list+m_idx;
	monster_race* r_ptr = m_ptr->race();
	monster_lore* l_ptr = m_ptr->lore();

	int d;

	/* Current location */
	int fy = m_ptr->loc.y;
	int fx = m_ptr->loc.x;

	/* Seen at all */
	bool flag = FALSE;

	/* Seen by vision */
	bool easy = FALSE;


	/* Compute distance */
	if (full)
	{
		d = distance(p_ptr->loc.y,p_ptr->loc.x,m_ptr->loc.y,m_ptr->loc.x);

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
			if (r_ptr->flags[1] & RF1_EMPTY_MIND)
			{
				/* Memorize flags */
				l_ptr->flags[1] |= RF1_EMPTY_MIND;
			}

			/* Weird mind, occasional telepathy */
			else if (r_ptr->flags[1] & RF1_WEIRD_MIND)
			{
				/* One in ten individuals are detectable */
				if ((m_idx % 10) == 5)
				{
					/* Detectable */
					flag = TRUE;

					/* Memorize flags */
					l_ptr->flags[1] |= RF1_WEIRD_MIND;

					/* Hack -- Memorize mental flags */
					if (r_ptr->flags[1] & RF1_SMART) l_ptr->flags[1] |= RF1_SMART;
					if (r_ptr->flags[1] & RF1_STUPID) l_ptr->flags[1] |= RF1_STUPID;
				}
			}

			/* Normal mind, allow telepathy */
			else
			{
				/* Detectable */
				flag = TRUE;

				/* Hack -- Memorize mental flags */
				if (r_ptr->flags[1] & RF1_SMART) l_ptr->flags[1] |= RF1_SMART;
				if (r_ptr->flags[1] & RF1_STUPID) l_ptr->flags[1] |= RF1_STUPID;
			}
		}

		/* Normal line of sight, and not blind */
		if (player_has_los_bold(fy, fx) && !p_ptr->timed[TMD_BLIND])
		{
			bool do_invisible = FALSE;
			bool do_cold_blood = FALSE;

			/* Use "infravision" */
			if (d <= p_ptr->see_infra)
			{
				/* Handle "cold blooded" monsters */
				if (r_ptr->flags[1] & RF1_COLD_BLOOD)
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
			if (player_can_see_bold(fy, fx))
			{
				/* Handle "invisible" monsters */
				if (r_ptr->flags[1] & RF1_INVISIBLE)
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
				if (do_invisible) l_ptr->flags[1] |= RF1_INVISIBLE;
				if (do_cold_blood) l_ptr->flags[1] |= RF1_COLD_BLOOD;
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
			lite_spot(m_ptr->loc);

			/* Update health bar as needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Hack -- Count "fresh" sightings */
			if (l_ptr->sights < MAX_S16B) l_ptr->sights++;

			/* Disturb on appearance */
			if (OPTION(disturb_move)) disturb(1, 0);

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
			lite_spot(m_ptr->loc);

			/* Update health bar as needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Disturb on disappearance */
			if (OPTION(disturb_move)) disturb(1, 0);

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
			if (OPTION(disturb_near)) disturb(1, 0);
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
			if (OPTION(disturb_near)) disturb(1, 0);
		}
	}
}




/*
 * This function simply updates all the (non-dead) monsters (see above).
 */
void update_monsters(bool full)
{
	m_idx_type i(mon_max-1);
	while(1)
	{
		/* live monsters get updated */
		if (mon_list[i].r_idx) update_mon(i, full);
		if (1==i) break;
		--i;
	}
}




/*
 * Make a monster carry an object
 */
s16b monster_carry(const m_idx_type m_idx, object_type *j_ptr)
{
	s16b o_idx;
	s16b this_o_idx, next_o_idx = 0;
	monster_type* const m_ptr = mon_list+m_idx;

	/* Scan objects already being held for combination */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

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
		object_type *o_ptr = &o_list[o_idx];	/* Get new object */

		*o_ptr = *j_ptr;

		/* Forget mark */
		o_ptr->marked = FALSE;

		/* Forget location */
		o_ptr->loc.clear();

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
void monster_swap(coord g1, coord g2)
{
	/* Monsters */
	int m1 = cave_m_idx[g1.y][g1.x];
	int m2 = cave_m_idx[g2.y][g2.x];


	/* Update grids */
	cave_m_idx[g1.y][g1.x] = m2;
	cave_m_idx[g2.y][g2.x] = m1;


	/* Monster 1 */
	if (m1 > 0)
	{
		/* Move monster */
		mon_list[m1].loc = g2;

		/* Update monster */
		update_mon(m1, TRUE);
	}

	/* Player 1 */
	else if (m1 < 0)
	{
		/* Move player */
		p_ptr->loc = g2;

		/* Update the panel, visuals, monster distances, flow */
		p_ptr->update |= (PU_PANEL | PU_UPDATE_VIEW | PU_DISTANCE | PU_UPDATE_FLOW);

		/* Window stuff */
		p_ptr->redraw |= (PR_MAP);
	}

	/* Monster 2 */
	if (m2 > 0)
	{
		/* Move monster */
		mon_list[m2].loc = g1;

		/* Update monster */
		update_mon(m2, TRUE);
	}

	/* Player 2 */
	else if (m2 < 0)
	{
		/* Move player */
		p_ptr->loc = g1;

		/* Update the panel, visuals, monster distances, flow */
		p_ptr->update |= (PU_PANEL | PU_UPDATE_VIEW | PU_DISTANCE | PU_UPDATE_FLOW);

		/* Window stuff */
		p_ptr->redraw |= (PR_MAP);
	}


	/* Redraw */
	lite_spot(g1);
	lite_spot(g2);
}


/*
 * Place the player in the dungeon XXX XXX
 */
s16b player_place(int y, int x)
{
	/* Paranoia XXX XXX */
	if (cave_m_idx[y][x] != 0) return (0);


	/* Save player location */
	p_ptr->loc.y = y;
	p_ptr->loc.x = x;

	/* Mark cave grid */
	cave_m_idx[y][x] = -1;

	/* Success */
	return (-1);
}


/*
 * Place a copy of a monster in the dungeon XXX XXX
 */
s16b monster_place(coord g, monster_type *n_ptr)
{
	s16b m_idx;

	/* Paranoia XXX XXX */
	if (cave_m_idx[g.y][g.x] != 0) return (0);


	/* Get a new record */
	m_idx = mon_pop();

	/* Oops */
	if (m_idx)
	{
		monster_type* m_ptr = &mon_list[m_idx];			/* Get the new monster */
		monster_race* r_ptr = n_ptr->race();	/* Get the new race */

		cave_m_idx[g.y][g.x] = m_idx;	/* Make a new monster */
		*m_ptr = *n_ptr;				/* Copy the monster XXX */
		m_ptr->loc = g;					/* Location */
		update_mon(m_idx, TRUE);		/* Update the monster */

		/* Hack -- Notice new multi-hued monsters */
		if (r_ptr->flags[0] & RF0_ATTR_MULTI) shimmer_monsters = TRUE;

		/* Hack -- Count the number of "reproducers" */
		if (r_ptr->flags[1] & RF1_MULTIPLY) num_repro++;

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
static bool place_monster_one(coord g, int r_idx, bool slp)
{
	assert(0 < r_idx && r_idx < z_info->r_max);
	int i;

	monster_race *r_ptr;

	monster_type monster_type_body;
	monster_type *n_ptr = &monster_type_body;	/* Get local monster */

	const char* name;


	/* Paranoia */
	if (!in_bounds(g.y, g.x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(g.y, g.x)) return (FALSE);

	/* Hack -- no creation on glyph of warding */
	if (cave_feat[g.y][g.x] == FEAT_GLYPH) return (FALSE);


	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Race */
	r_ptr = &monster_type::r_info[r_idx];

	/* Paranoia */
	if (!r_ptr->_name) return (FALSE);

	/* Name */
	name = r_ptr->name();


	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags[0] & RF0_UNIQUE) && (r_ptr->cur_num >= r_ptr->max_num))
	{
		return FALSE;	/* Cannot create */
	}


	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags[0] & RF0_FORCE_DEPTH) && (p_ptr->depth < r_ptr->level))
	{
		return FALSE;	/* Cannot create */
	}


	/* Powerful monster */
	if (r_ptr->level > p_ptr->depth)
	{
		/* Unique monsters */
		if (r_ptr->flags[0] & RF0_UNIQUE)
		{
			/* Message for cheaters */
			if (OPTION(cheat_hear)) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - p_ptr->depth) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
			if (OPTION(cheat_hear)) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - p_ptr->depth);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags[0] & RF0_UNIQUE)
	{
		/* Unique monsters induce message */
		if (OPTION(cheat_hear)) msg_format("Unique (%s).", name);
	}


	/* Clean out the monster */
	WIPE(n_ptr);


	/* Save the race */
	n_ptr->r_idx = r_idx;


	/* Enforce sleeping if needed */
	if (slp && r_ptr->sleep)
	{
		int val = r_ptr->sleep;
		n_ptr->csleep = ((val * 2) + randint(val * 10));
	}


	/* Assign maximal hitpoints if forced */
	n_ptr->mhp = (r_ptr->flags[0] & RF0_FORCE_MAXHP) ? r_ptr->h.maxroll()
													 : r_ptr->h.damroll();

	n_ptr->chp = n_ptr->mhp;		/* And start out fully healthy */
	n_ptr->speed = r_ptr->speed;	/* Extract the monster base speed */

	/* Hack -- small racial variety */
	if (!(r_ptr->flags[0] & RF0_UNIQUE))
	{
		/* Allow some small variation per monster */
		i = extract_energy[r_ptr->speed] / 10;
		if (i) n_ptr->speed += rand_spread(0, i);
	}


	/* Give a random starting energy */
	n_ptr->energy = (byte)rand_int(100);

	/* Force monster to wait for player */
	if (r_ptr->flags[0] & RF0_FORCE_SLEEP)
	{
		/* Monster is still being nice */
		n_ptr->mflag |= (MFLAG_NICE);

		/* Optimize -- Repair flags */
		repair_mflag_nice = TRUE;
	}

	/* Place the monster in the dungeon */
	if (!monster_place(g, n_ptr)) return (FALSE);

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
static bool place_monster_group(coord g, int r_idx, bool slp)
{
	monster_race *r_ptr = &monster_type::r_info[r_idx];

	int old, n, i;
	int total, extra = 0;

	int hack_n;
	coord hack[GROUP_MAX];


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
	hack[0] = g;

	/* Puddle monsters, breadth first, up to total */
	for (n = 0; (n < hack_n) && (hack_n < total); n++)
	{
		/* Grab the location */
		coord h = hack[n];

		/* Check each direction, up to total */
		for (i = 0; (i < KEYPAD_DIR_MAX) && (hack_n < total); i++)
		{
			coord m(h+dd_coord_ddd[i]);

			/* Walls and Monsters block flow */
			if (!cave_empty_bold(m.y, m.x)) continue;

			/* Attempt to place another monster */
			if (place_monster_one(m, r_idx, slp))
			{
				/* Add it to the "hack" set */
				hack[hack_n++] = m;
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
	monster_race *r_ptr = &monster_type::r_info[place_monster_idx];
	monster_race *z_ptr = &monster_type::r_info[r_idx];

	/* Require similar "race" */
	if (z_ptr->d_char != r_ptr->d_char) return FALSE;

	/* Skip more advanced monsters */
	if (z_ptr->level > r_ptr->level) return FALSE;

	/* Skip unique monsters */
	if (z_ptr->flags[0] & RF0_UNIQUE) return FALSE;

	/* Paranoia -- Skip identical monsters */
	if (place_monster_idx == r_idx) return FALSE;

	/* Okay */
	return TRUE;
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
bool place_monster_aux(coord g, int r_idx, bool slp, bool grp)
{
	int i;

	monster_race *r_ptr = &monster_type::r_info[r_idx];


	/* Place one monster, or fail */
	if (!place_monster_one(g, r_idx, slp)) return FALSE;

	if (!grp) return TRUE;	/* Require the "group" flag */


	/* Friends for certain monsters */
	if (r_ptr->flags[0] & RF0_FRIENDS)
	{
		place_monster_group(g, r_idx, slp);	/* Attempt to place a group */
	}


	/* Escorts for certain monsters */
	if (r_ptr->flags[0] & RF0_ESCORT)
	{
		/* Try to place several "escorts" */
		for (i = 0; i < 50; i++)
		{
			coord n;
			int z, d = 3;

			/* Pick a location */
			scatter(n, g, d, 0);

			/* Require empty grids */
			if (!cave_empty_bold(n.y, n.x)) continue;

			place_monster_idx = r_idx;	/* Set the escort index */
			get_mon_num_prep(&place_monster_okay);	/* Set the escort hook */
			z = get_mon_num(r_ptr->level);	/* Pick a random race */
			get_mon_num_prep(NULL);		/* Remove restriction */

			if (!z) break;				/* Handle failure */

			place_monster_one(n, z, slp);	/* Place a single escort */

			/* Place a "group" of escorts if needed */
			if ((monster_type::r_info[z].flags[0] & RF0_FRIENDS) ||
			    (r_ptr->flags[0] & RF0_ESCORTS))
			{
				/* Place a group of monsters */
				place_monster_group(n, z, slp);
			}
		}
	}


	/* Success */
	return TRUE;
}


/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "monster_level"
 */
bool place_monster(coord g, bool slp, bool grp)
{
	int r_idx = get_mon_num(monster_level);	/* Pick a monster */

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	return place_monster_aux(g, r_idx, slp, grp);
}




/*
 * XXX XXX XXX Player Ghosts are such a hack, they have been completely
 * removed.
 *
 * An idea for reintroducing them is to create a small number of
 * "unique" monsters which will serve as the "player ghosts".
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
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	coord g;
	int	attempts_left = 10000;

	/* Find a legal, distant, unoccupied, space */
	while (--attempts_left)
	{
		/* Pick a location */
		g.y = rand_int(DUNGEON_HGT);
		g.x = rand_int(DUNGEON_WID);

		/* Require "naked" floor grid */
		if (!cave_naked_bold(g.y, g.x)) continue;

		/* Accept far away grids */
		if (distance(g.y, g.x, py, px) > dis) break;
	}

	if (!attempts_left)
	{
		if (OPTION(cheat_xtra) || OPTION(cheat_hear))
		{
			msg_print("Warning! Could not allocate a new monster.");
		}

		return FALSE;
	}

	/* Attempt to place the monster, allow groups */
	return place_monster(g, slp, TRUE);

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
	monster_race *r_ptr = &monster_type::r_info[r_idx];

	bool okay = FALSE;


	/* Hack -- no specific type specified */
	if (!summon_specific_type) return (TRUE);


	/* Check our requirements */
	switch (summon_specific_type)
	{
		case SUMMON_ANIMAL:
		{
			okay = ((r_ptr->flags[2] & RF2_ANIMAL) &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_SPIDER:
		{
			okay = ((r_ptr->d_char == 'S') &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_HOUND:
		{
			okay = ((NULL!=strchr("CZ",r_ptr->d_char)) &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_HYDRA:
		{
			okay = ((r_ptr->d_char == 'M') &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_ANGEL:
		{
			okay = ((r_ptr->d_char == 'A') &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_DEMON:
		{
			okay = ((r_ptr->flags[2] & RF2_DEMON) &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_UNDEAD:
		{
			okay = ((r_ptr->flags[2] & RF2_UNDEAD) &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_DRAGON:
		{
			okay = ((r_ptr->flags[2] & RF2_DRAGON) &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_KIN:
		{
			okay = ((r_ptr->d_char == summon_kin_type) &&
			        !(r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_HI_UNDEAD:
		{
			okay = (NULL!=strchr("LVW",r_ptr->d_char));
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

		case SUMMON_WRAITH:
		{
			okay = ((r_ptr->d_char == 'W') &&
			        (r_ptr->flags[0] & RF0_UNIQUE));
			break;
		}

		case SUMMON_UNIQUE:
		{
			okay = (r_ptr->flags[0] & RF0_UNIQUE);
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
bool summon_specific(coord g, int lev, int type)
{
	coord new_g;
	int i, r_idx;


	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(new_g, g, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(new_g.y, new_g.x)) continue;

		/* Hack -- no summon on glyph of warding */
		if (cave_feat[new_g.y][new_g.x] == FEAT_GLYPH) continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);


	/* Save the "summon" type */
	summon_specific_type = type;


	/* Require "okay" monsters */
	get_mon_num_prep(&summon_specific_okay);


	/* Pick a monster, using the level calculation */
	r_idx = get_mon_num((p_ptr->depth + lev) / 2 + 5);


	/* Remove restriction */
	get_mon_num_prep(NULL);


	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(new_g, r_idx, FALSE, TRUE)) return (FALSE);

	/* Success */
	return (TRUE);
}





/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
bool multiply_monster(const m_idx_type m_idx)
{
	monster_type* const m_ptr = mon_list+m_idx;

	coord g;
	int i;

	/* Try up to 18 times */
	for (i = 0; i < 18; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(g, m_ptr->loc, d, 0);

		/* Require an "empty" floor grid */
		if (cave_empty_bold(g.y, g.x)) break;

		/* Create a new monster (awake, no groups) */
		return place_monster_aux(g, m_ptr->r_idx, FALSE, FALSE);
	}

	/* fell through */
	return false;
}


static size_t pain_index(int percentage)
{
	if (percentage > 95)
		return 0;
	else if (percentage > 75)
		return 1;
	else if (percentage > 50)
		return 2;
	else if (percentage > 35)
		return 3;
	else if (percentage > 20)
		return 4;
	else if (percentage > 10)
		return 5;
	else
		return 6;
}

static const char* const jelly_pain_msg[7]
	=	{	"%^s barely notices.",
			"%^s flinches.",
			"%^s squelches.",
			"%^s quivers in pain.",
			"%^s writhes about.",
			"%^s writhes in agony.",
			"%^s jerks limply."
		};

static const char* const hound_pain_msg[7]
	=	{	"%^s shrugs off the attack.",
			"%^s snarls with pain.",
			"%^s yelps in pain.",
			"%^s howls in pain.",
			"%^s howls in agony.",
			"%^s writhes in agony.",
			"%^s yelps feebly."
		};

static const char* const squeal_pain_msg[7]
	=	{	"%^s ignores the attack.",
			"%^s grunts with pain.",
			"%^s squeals in pain.",
			"%^s shrieks in pain.",
			"%^s shrieks in agony.",
			"%^s writhes in agony.",
			"%^s cries out feebly."
		};

static const char* const scream_pain_msg[7]
	=	{	"%^s ignores the attack.",
			"%^s grunts with pain.",
			"%^s cries out in pain.",
			"%^s screams in pain.",
			"%^s screams in agony.",
			"%^s writhes in agony.",
			"%^s cries out feebly."
		};

static const int pain_noise_intensity[7]
	=	{	1,
			2,
			5,
			10,
			10,
			2,
			1
		};

/*
 * Dump a message describing a monster's reaction to damage
 *
 * Technically should attempt to treat "Beholder"'s as jelly's
 */
void message_pain(const m_idx_type m_idx, int dam)
{
	const monster_type* const m_ptr = mon_list+m_idx;
	char m_name[80];


	/* Get the monster name */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Notice non-damage */
	if (dam == 0)
	{
		msg_format("%^s is unharmed.", m_name);
		return;
	}

	/* Note -- subtle fix -CFT */
	{
	const long newhp = m_ptr->chp;
	const long oldhp = newhp + (long)(dam);
	const int percentage = (int)((newhp * 100L) / oldhp);
	const int pain_level = pain_index(percentage);
	const char race_char = m_ptr->race()->d_char;

	assert((0 <= pain_level) && (7 > pain_level));

	/* Jelly's, Mold's, Vortex's, Quthl's */
	/* Unlike the others, these don't make noise when hit */
	if (strchr("jmvQ", race_char))
	{
		msg_format(jelly_pain_msg[pain_index(percentage)], m_name);
	}

	/* Dogs and Hounds */
	else if (strchr("CZ", race_char))
	{
		msg_format(hound_pain_msg[pain_index(percentage)], m_name);
		apply_noise(m_ptr->loc, pain_noise_intensity[pain_index(percentage)]);
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (strchr("FIKMRSXabclqrst", race_char))
	{
		msg_format(squeal_pain_msg[pain_index(percentage)], m_name);
		apply_noise(m_ptr->loc, pain_noise_intensity[pain_index(percentage)]);
	}

	/* Another type of monsters (shrug,cry,scream) */
	else
	{
		msg_format(scream_pain_msg[pain_index(percentage)], m_name);
		apply_noise(m_ptr->loc, pain_noise_intensity[pain_index(percentage)]);
	}
	}
}


/*
 * Learn about an "observed" resistance.
 */
void update_smart_learn(const m_idx_type m_idx, int what)
{
	/* Not allowed to learn */
	if (!OPTION(adult_smart_learn)) return;

	monster_type* const m_ptr = mon_list+m_idx;
	monster_race* const r_ptr = m_ptr->race();

	/* Too stupid to learn anything */
	if (r_ptr->flags[1] & RF1_STUPID) return;

	/* Not intelligent, only learn sometimes */
	if (!(r_ptr->flags[1] & RF1_SMART) && one_in_(2)) return;


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

		case DRS_RES_ACID:
		{
			if (p_ptr->resist_acid) m_ptr->smart |= (SM_RES_ACID);
			if (p_ptr->timed[TMD_OPP_ACID]) m_ptr->smart |= (SM_OPP_ACID);
			if (p_ptr->immune_acid) m_ptr->smart |= (SM_IMM_ACID);
			break;
		}

		case DRS_RES_ELEC:
		{
			if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
			if (p_ptr->timed[TMD_OPP_ELEC]) m_ptr->smart |= (SM_OPP_ELEC);
			if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
			break;
		}

		case DRS_RES_FIRE:
		{
			if (p_ptr->resist_fire) m_ptr->smart |= (SM_RES_FIRE);
			if (p_ptr->timed[TMD_OPP_FIRE]) m_ptr->smart |= (SM_OPP_FIRE);
			if (p_ptr->immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
			break;
		}

		case DRS_RES_COLD:
		{
			if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
			if (p_ptr->timed[TMD_OPP_COLD]) m_ptr->smart |= (SM_OPP_COLD);
			if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);
			break;
		}

		case DRS_RES_POIS:
		{
			if (p_ptr->resist_pois) m_ptr->smart |= (SM_RES_POIS);
			if (p_ptr->timed[TMD_OPP_POIS]) m_ptr->smart |= (SM_OPP_POIS);
			break;
		}

		case DRS_RES_FEAR:
		{
			if (p_ptr->resist_fear) m_ptr->smart |= (SM_RES_FEAR);
			break;
		}

		case DRS_RES_LITE:
		{
			if (p_ptr->resist_lite) m_ptr->smart |= (SM_RES_LITE);
			break;
		}

		case DRS_RES_DARK:
		{
			if (p_ptr->resist_dark) m_ptr->smart |= (SM_RES_DARK);
			break;
		}

		case DRS_RES_BLIND:
		{
			if (p_ptr->resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			break;
		}

		case DRS_RES_CONFU:
		{
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			break;
		}

		case DRS_RES_SOUND:
		{
			if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			break;
		}

		case DRS_RES_SHARD:
		{
			if (p_ptr->resist_shard) m_ptr->smart |= (SM_RES_SHARD);
			break;
		}

		case DRS_RES_NEXUS:
		{
			if (p_ptr->resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
			break;
		}

		case DRS_RES_NETHR:
		{
			if (p_ptr->resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
			break;
		}

		case DRS_RES_CHAOS:
		{
			if (p_ptr->resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
			break;
		}

		case DRS_RES_DISEN:
		{
			if (p_ptr->resist_disen) m_ptr->smart |= (SM_RES_DISEN);
			break;
		}
	}

}

bool
monster_has_attack(const monster_race* const r_ptr, byte this_method)
{
	int i;
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
		{
		if (r_ptr->blow[i].effect == this_method) return (TRUE);
		};
	return (FALSE);
}

