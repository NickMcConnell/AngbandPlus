/* File: monster2.c */

/* Purpose: misc code for monsters */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

cptr horror_desc[MAX_SAN_HORROR] =
{
	"abominable",
	"abysmal",
	"appalling",
	"baleful",
	"blasphemous",

	"disgusting",
	"dreadful",
	"filthy",
	"grisly",
	"hideous",

	"hellish",
	"horrible",
	"infernal",
	"loathsome",
	"nightmarish",

	"repulsive",
	"sacrilegious",
	"terrible",
	"unclean",
	"unspeakable",
};

cptr funny_desc[MAX_SAN_FUNNY] =
{
	"silly",
	"hilarious",
	"absurd",
	"insipid",
	"ridiculous",

	"laughable",
	"ludicrous",
	"far-out",
	"groovy",
	"postmodern",

	"fantastic",
	"dadaistic",
	"cubistic",
	"cosmic",
	"awesome",

	"incomprehensible",
	"fabulous",
	"amazing",
	"incredible",
	"chaotic",

	"wild",
	"preposterous",
};

cptr funny_comments[MAX_SAN_COMMENT] =
{
	"Wow, cosmic, man!",
	"Rad!",
	"Groovy!",
	"Cool!",
	"Far out!"
};

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

	/* Get location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Hack -- Reduce the racial counter */
	r_ptr->cur_num--;

	/* Hack -- count the number of "reproducers" */
	if (FLAG(r_ptr, RF_MULTIPLY)) num_repro--;
	
	/* Notice changes in lighting */		
	if (FLAG(r_ptr, RF_LITE_1) || FLAG(r_ptr, RF_LITE_2))
	{
		/* Update some things */
		p_ptr->update |= (PU_MON_LITE);
	}

	/* Decrement visibility count */
	if (m_ptr->ml && !(m_ptr->smart & SM_MIMIC))
	{
		update_mon_vis(m_ptr->r_idx, -1);
	}

	/* Hack -- remove target monster */
	if (i == p_ptr->target_who) p_ptr->target_who = 0;

	/* Hack -- remove tracked monster */
	if (i == p_ptr->health_who) health_track(0);


	/* Monster is gone */
	if (in_bounds2(x, y))
	{
		area(x, y)->m_idx = 0;
	}

	/* Delete objects */
	delete_object_list(&m_ptr->hold_o_idx);

	/* Wipe the Monster */
	(void)WIPE(m_ptr, monster_type);

	/* Count monsters */
	m_cnt--;

	/* Visual update */
	lite_spot(x, y);
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(int x, int y)
{
	cave_type *c_ptr;

	/* Paranoia */
	if (!in_bounds2(x, y)) return;

	/* Check the grid */
	c_ptr = area(x, y);

	/* Delete the monster (if any) */
	if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx);
}


/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_monsters_aux(int i1, int i2)
{
	int y, x;

	cave_type *c_ptr;

	monster_type *m_ptr;

	/* Do nothing */
	if (i1 == i2) return;


	/* Old monster */
	m_ptr = &m_list[i1];

	/* Location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Cave grid */
	c_ptr = area(x, y);

	/* Update the cave */
	c_ptr->m_idx = i2;

	/* Hack -- Update the target */
	if (p_ptr->target_who == i1) p_ptr->target_who = i2;

	/* Hack -- Update the health bar */
	if (p_ptr->health_who == i1) health_track(i2);

	/* Structure copy */
	COPY(&m_list[i2], &m_list[i1], monster_type);

	/* Wipe the hole */
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
	if (size) msgf("Compacting monsters...");


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
			if (FLAG(r_ptr, RF_QUESTOR) && (cnt < 1000)) chance = 100;

			/* Try not to compact Unique Monsters */
			if (FLAG(r_ptr, RF_UNIQUE)) chance = 99;

			/* All monsters get a saving throw */
			if (randint0(100) < chance) continue;

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

		/* Hack - kill monsters out of bounds. */
		if (!in_bounds2(m_ptr->fx, m_ptr->fy))
		{
			delete_monster_idx(i);
		}

		/* Skip real monsters */
		if (m_ptr->r_idx) continue;

		/* Move last monster into open hole */
		compact_monsters_aux(m_max - 1, i);

		/* Compress "m_max" */
		m_max--;
	}
}


/*
 * Delete/Remove all the monsters in the game
 *
 * This is an efficient method of simulating multiple calls to the
 * "delete_monster()" function, with no visual effects.
 */
void wipe_m_list(void)
{
	int i, x, y;

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

		/* Check to see if monster is accessable on map */
		y = m_ptr->fy;
		x = m_ptr->fx;

		if (in_bounds2(x, y))
		{
			/* Monster is gone */
			area(x, y)->m_idx = 0;
		}

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
	p_ptr->target_who = 0;

	/* Hack -- no more tracking */
	health_track(0);

	/* Hack -- reset "visible" counter */
	p_ptr->max_seen_r_idx = 0;
	p_ptr->window |= PW_VISIBLE;
}


/*
 * Wipe monsters in region
 */
void wipe_monsters(int rg_idx)
{
	int i;
	monster_type *m_ptr;

	/* Delete all the monsters */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Enforce region */
		if (m_ptr->region != rg_idx) continue;

		/* Delete the monster */
		delete_monster_idx(i);
	}

	/* Compress the monster list */
	compact_monsters(0);
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
	if (character_dungeon) msgf("Too many monsters!");

	/* Try not to crash */
	return (0);
}


/*
 * Apply a "monster restriction function" to the "monster allocation table"
 */
void get_mon_num_prep(monster_hook_type monster_hook)
{
	int i;

	/* Scan the allocation table */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Get the entry */
		alloc_entry *entry = &alloc_race_table[i];

		/* Accept monsters which pass the restriction, if any */

		/*
		 * Hack - check for silly monsters here.
		 * This makes more sense then adding the test to every
		 * hook function.
		 */
		if ((!monster_hook || (*monster_hook) (entry->index))
			&& (silly_monsters || !FLAG(&r_info[entry->index], RF_SILLY)))
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
	return;
}


/*
 * Are we allowed to place monsters of this race on this square?
 */
bool test_monster_square(cave_type *c_ptr, monster_race *r_ptr)
{
	/* Permanent walls are out */
	if (cave_perma_grid(c_ptr) && !cave_floor_grid(c_ptr)) return (FALSE);

	/* Nor on the Pattern */
	if (cave_pattern_grid(c_ptr)) return (FALSE);
	
	/* Check to see if fields dissallow placement or movement */
	if (fields_have_flags(c_ptr, FIELD_INFO_NO_ENTER))
	{
		/* Cannot create */
		return (FALSE);
	}

	/* Set the monster list */
	switch (c_ptr->feat)
	{
		case FEAT_OCEAN_WATER:
		{
			if (!(FLAG(r_ptr, RF_WILD_OCEAN))) return (FALSE);
			
			/* Hack - no break */
		}
		
		case FEAT_DEEP_WATER:
		{
			if (!((FLAG(r_ptr, RF_AQUATIC)) ||
				(FLAG(r_ptr, RF_CAN_FLY)) ||
				(FLAG(r_ptr, RF_CAN_SWIM))))
			{
				return (FALSE);
			}
			
			/* Hack - no break */
		}

		case FEAT_SHAL_WATER:
		{
			if (FLAG(r_ptr, RF_AURA_FIRE)) return (FALSE);
			
			return (TRUE);
		}
		case FEAT_DEEP_LAVA:
		case FEAT_SHAL_LAVA:
		{
			/* Immunity to fire is nice */
			if (FLAG(r_ptr, RF_IM_FIRE)) return (TRUE);
		
			/* If we are cold - then we can't cross */
			if (FLAG(r_ptr, RF_AURA_COLD)) return (FALSE);
			
			/* If we can't fly, then we can't cross */
			if (!(FLAG(r_ptr, RF_CAN_FLY))) return (FALSE);
			break;
		}
		case FEAT_DEEP_ACID:
		case FEAT_SHAL_ACID:
		{
			/* Immunity to acid is nice */
			if (FLAG(r_ptr, RF_IM_ACID)) return (TRUE);
			
			/* If we can't fly, then we can't cross */
			if (!(FLAG(r_ptr, RF_CAN_FLY))) return (FALSE);
			break;
		}
		case FEAT_DEEP_SWAMP:
		case FEAT_SHAL_SWAMP:
		{
			/* Immunity to poison is nice */
			if (FLAG(r_ptr, RF_IM_POIS)) return (TRUE);
			
			/* If we can't fly, then we can't cross */
			if (!(FLAG(r_ptr, RF_CAN_FLY))) return (FALSE);
			break;
		}
	}
	
	/* Aquatic monster */
	if ((FLAG(r_ptr, RF_AQUATIC)) && !(FLAG(r_ptr, RF_CAN_FLY)))
	{
		return FALSE;
	}

	/* Looks ok then */
	
	return (TRUE);
}

/*
 * Should the monster be placed at this point in the wilderness?
 */
static bool test_monster_wild(wild_done_type *w_ptr, monster_race *r_ptr)
{
	byte mon_wild;
	
	/* Are we a town or city? */
	if (w_ptr->place)
	{
		/* Not a quest? */
		if ((!place[w_ptr->place].quest_num) &&
			(FLAG(r_ptr, RF_WILD_TOWN))) return TRUE;
	}


	/* Ocean? */
	if (w_ptr->wild > WILD_SEA)
	{
		if (FLAG(r_ptr, RF_WILD_OCEAN)) return TRUE;
	
		return FALSE;
	}
	
	/* Shore */
	if (w_ptr->info & WILD_INFO_WATER)
	{
		if (FLAG(r_ptr, RF_WILD_SHORE)) return TRUE;

		return FALSE;
	}
	
	/* Acid */
	if (w_ptr->info & WILD_INFO_ACID)
	{
		/* Immunity to acid is nice */
		if (FLAG(r_ptr, RF_IM_ACID)) return TRUE;
			
		/* If we can't fly, then we can't cross */
		if (FLAG(r_ptr, RF_CAN_FLY)) return TRUE;
		
		return FALSE;
	}
	
	/* Lava */
	if (w_ptr->info & WILD_INFO_LAVA)
	{
		/* Immunity to fire is nice */
		if (FLAG(r_ptr, RF_IM_FIRE)) return TRUE;
		
		/* If we are cold - then we can't cross */
		if (FLAG(r_ptr, RF_AURA_COLD)) return FALSE;
			
		/* If we can't fly, then we can't cross */
		if (FLAG(r_ptr, RF_CAN_FLY)) return TRUE;
		
		return FALSE;
	}
	
	
	/*
	 * Get type of wilderness.
	 */
	mon_wild = wild_gen_data[w_ptr->wild].rough_type;
	
	/* Test to see if the monster likes this terrain */
	if (r_ptr->flags[7] & mon_wild) return TRUE;

	if (!mon_wild)
	{
		/* No other terrain - use grass */
		if (FLAG(r_ptr, RF_WILD_GRASS)) return TRUE;
	}

	return FALSE;
}



/*
 * Filter the list of creatable monsters based on location
 */
static int filter_mon_loc(int x, int y)
{
	wild_done_type *w_ptr;
	monster_race *r_ptr;
	
	place_type *pl_ptr = &place[p_ptr->place_num];

	int level;

	int i;
	
	/* In the wilderness? */
	if (!p_ptr->depth)
	{
		/* Point to wilderness block info */
		w_ptr = &wild[y / 16][x / 16].done;
		
		/* Scan the allocation table */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Get the entry */
			alloc_entry *entry = &alloc_race_table[i];
		
			/* Only bother checking monsters we would have created */
			if (entry->prob2)
			{
				/* Get the race */
				r_ptr = &r_info[entry->index];
		
				/* Not allowed in this part of the wilderness? */
				if (!test_monster_wild(w_ptr, r_ptr))
				{
					entry->prob2 = 0;
				}
			}
		}
		
		/* The level of the monsters */
		level = w_ptr->mon_gen;
	}
	else
	{
		/* In the dungeon */
		
		/* Scan the allocation table */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Get the entry */
			alloc_entry *entry = &alloc_race_table[i];
		
			/* Only bother checking monsters we would have created */
			if (entry->prob2)
			{
				/* Get the race */
				r_ptr = &r_info[entry->index];
				
				/* Not a monster for this dungeon? */
				if (!(r_ptr->flags[7] & (pl_ptr->dungeon->habitat)))
				{
					entry->prob2 = 0;
				}
			}
		}
		
		/* The level of the monsters */
		level = base_level();
	}
	
	return (level);
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
 * a small amount (up to ten levels), except in the town.
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

	long value1, value2, total;

	monster_race *r_ptr;

	alloc_entry *table = alloc_race_table;


	/* Boost the level */
	if (level > 0)
	{
		/* Nightmare mode allows more out-of depth monsters */
		if (ironman_nightmare && one_in_(NASTY_MON))
		{
			/* What a bizarre calculation */
			level = 1 + (level * MAX_DEPTH / randint1(MAX_DEPTH));
		}
		else
		{
			int checks = 2;

			for ( ; checks > 0; checks--)
			{
				/* Occasional "nasty" monster */
				if (one_in_(NASTY_MON))
				{
					/* Boost the level */
					level += 7;
				}
			}

			/* Luck gives occasional very out-of-depth monsters */
			if ((FLAG(p_ptr, TR_STRANGE_LUCK)) && one_in_(13))
			{
				level += randint1(one_in_(7) ? 40 : 10);
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

		/* Access the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Access the actual race */
		r_ptr = &r_info[r_idx];

		/* Hack -- "unique" monsters must be "unique" */
		if ((FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
			&& (r_ptr->cur_num >= r_ptr->max_num))
		{
			continue;
		}

		/* Hack -- don't create questors */
		if (FLAG(r_ptr, RF_QUESTOR))
		{
			continue;
		}

		/* Depth Monsters never appear out of depth */
		if (FLAG(r_ptr, RF_FORCE_DEPTH)
			&& (r_ptr->level > p_ptr->depth))
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
	value1 = randint0(total);

	/* Power boost */
	p = randint0(100);

	/* Try for a "better" monster once (50%) or twice (10%) */
	if (p < 60)
	{
		value2 = randint0(total);

		/* Is it better? */
		if (value2 > value1)
		{
			/* This hack works because the monster table is sorted by depth */
			value1 = value2;
		}
	}

	/* Try for a "better" monster twice (10%) */
	if (p < 10)
	{
		value2 = randint0(total);

		/* Is it better? */
		if (value2 > value1)
		{
			/* This hack works because the monster table is sorted by depth */
			value1 = value2;
		}
	}

	/* Find the monster */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Found the entry */
		if (value1 < table[i].prob3)
		{
			return (table[i].index);
		}

		/* Decrement */
		value1 -= table[i].prob3;
	}

	msgf("Aborting - Could not generate a monster!!!! %d", total);

	/* Result */
	return (0);
}


/*
 * Get a monster from the list using a filter function
 * at the given level
 */
s16b get_filter_mon_num(int level, monster_hook_type monster_hook)
{
	s16b race;

	/* Apply the monster restriction */
	get_mon_num_prep(monster_hook);

	/* Pick a race */
	race = get_mon_num(level);

	/* Remove the monster restriction */
	get_mon_num_prep(NULL);

	return (race);
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
 * If no m_ptr arg is given (?), the monster is assumed to be hidden,
 * unless the "Assume Visible" mode is requested.
 * Does this really work???  It looks like r_ptr is initialised even
 * if m_ptr is NULL.  Perhaps this craziness can be removed. -SF-
 *
 *
 * If no r_ptr arg is given, it is extracted from m_ptr and r_info
 * If neither m_ptr nor r_ptr is given, the monster is assumed to
 * be neuter, singular, and hidden (unless "Assume Visible" is set),
 * in which case you may be in trouble... :-)
 *
 * I am assuming that no monster name is more than 70 characters long,
 * so that "char desc[80];" is sufficiently large for any result.
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
void monster_desc(char *desc, const monster_type *m_ptr, int mode, int max)
{
	cptr res;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	cptr name = mon_race_name(r_ptr);
	char silly_name[1024];
	bool seen, pron;
	bool named = FALSE;
	
	int n;

	/* Are we hallucinating? (Idea from Nethack...) */
	if (p_ptr->tim.image)
	{
		if (one_in_(2))
		{
			if (!get_rnd_line("silly.txt", m_ptr->r_idx, silly_name))
				named = TRUE;
		}

		if (!named)
		{
			monster_race *hallu_race;

			do
			{
				hallu_race = &r_info[randint1(z_info->r_max - 1)];
			}
			while (FLAG(hallu_race, RF_UNIQUE));

			strcpy(silly_name, mon_race_name(hallu_race));
		}

		/* Better not strcpy it, or we could corrupt r_info... */
		name = silly_name;
	}

	/* Can we "see" it (exists + forced, or visible + not unforced) */
	seen = (m_ptr && ((mode & 0x80) || (!(mode & 0x40) && m_ptr->ml)));

	/* Sexed Pronouns (seen and allowed, or unseen and allowed) */
	pron = (m_ptr && ((seen && (mode & 0x20)) || (!seen && (mode & 0x10))));


	/* First, try using pronouns, or describing hidden monsters */
	if (!seen || pron)
	{
		/* an encoding of the monster "sex" */
		int kind = 0x00;

		/* Extract the gender (if applicable) */
		if (FLAG(r_ptr, RF_FEMALE)) kind = 0x20;
		else if (FLAG(r_ptr, RF_MALE)) kind = 0x10;

		/* Ignore the gender (if desired) */
		if (!m_ptr || !pron) kind = 0x00;


		/* Assume simple result */
		res = "it";

		/* Brute force: split on the possibilities */
		switch (kind + (mode & 0x07))
		{
				/*** Neuter, or unknown ***/
			case 0x00:
			{
				res = "it";
				break;
			}
			case 0x01:
			{
				res = "it";
				break;
			}
			case 0x02:
			{
				res = "its";
				break;
			}
			case 0x03:
			{
				res = "itself";
				break;
			}
			case 0x04:
			{
				res = "something";
				break;
			}
			case 0x05:
			{
				res = "something";
				break;
			}
			case 0x06:
			{
				res = "something's";
				break;
			}
			case 0x07:
			{
				res = "itself";
				break;
			}

				/*** Male (assume human if vague) ***/
			case 0x10:
			{
				res = "he";
				break;
			}
			case 0x11:
			{
				res = "him";
				break;
			}
			case 0x12:
			{
				res = "his";
				break;
			}
			case 0x13:
			{
				res = "himself";
				break;
			}
			case 0x14:
			{
				res = "someone";
				break;
			}
			case 0x15:
			{
				res = "someone";
				break;
			}
			case 0x16:
			{
				res = "someone's";
				break;
			}
			case 0x17:
			{
				res = "himself";
				break;
			}

				/*** Female (assume human if vague) ***/
			case 0x20:
			{
				res = "she";
				break;
			}
			case 0x21:
			{
				res = "her";
				break;
			}
			case 0x22:
			{
				res = "her";
				break;
			}
			case 0x23:
			{
				res = "herself";
				break;
			}
			case 0x24:
			{
				res = "someone";
				break;
			}
			case 0x25:
			{
				res = "someone";
				break;
			}
			case 0x26:
			{
				res = "someone's";
				break;
			}
			case 0x27:
			{
				res = "herself";
				break;
			}
		}

		/* Copy the result */
		strnfmt(desc, max, "%s", res);
	}


	/* Handle visible monsters, "reflexive" request */
	else if ((mode & 0x02) && (mode & 0x01))
	{
		/* The monster is visible, so use its gender */
		if (FLAG(r_ptr, RF_FEMALE)) strnfmt(desc, max, "herself");
		else if (FLAG(r_ptr, RF_MALE)) strnfmt(desc, max, "himself");
		else
			strnfmt(desc, max, "itself");
	}


	/* Handle all other visible monster requests */
	else
	{
		/* It could be a Unique */
		if ((FLAG(r_ptr, RF_UNIQUE)) && !p_ptr->tim.image)
		{
			/* Start with the name (thus nominative and objective) */
			n = strnfmt(desc, max, "%s", name);
		}

		/* It could be an indefinite monster */
		else if (mode & 0x08)
		{
			/* XXX Check plurality for "some" */

			/* Indefinite monsters need an indefinite article */
			n = strnfmt(desc, max, is_a_vowel(name[0]) ? "an %s" : "a %s", name);
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			if (is_pet(m_ptr))
				n = strnfmt(desc, max, "your %s", name);
			else
				n = strnfmt(desc, max, "the %s", name);
		}

		/* Handle the Possessive as a special afterthought */
		if (mode & 0x02)
		{
			/* XXX Check for trailing "s" */

			/* Simply append "apostrophe" and "s" */
			strnfcat(desc, max, &n, "'s");
		}
	}
}


/*
 * Wrapper around monster_desc() for the '%v'
 * format option.  This allows monster_desc() to be
 * called in a format string.
 *
 * The parameters are monster_type (m_ptr) and mode(int).
 */
void monster_fmt(char *buf, uint max, cptr fmt, va_list *vp)
{
	const monster_type *m_ptr;
	int mode;
	
	/* Unused parameter */
	(void)fmt;
	
	/* Get the object */
	m_ptr = va_arg(*vp, const monster_type*);
	
	/* Get the mode */
	mode = va_arg(*vp, int);
	
	/* Print the description into the buffer */
	monster_desc(buf, m_ptr, mode, max);
}


/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Hack -- Memorize some flags */
	r_ptr->r_flags[0] = r_ptr->flags[0];
	r_ptr->r_flags[1] = r_ptr->flags[1];
	r_ptr->r_flags[2] = r_ptr->flags[2];

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

	/* Note the number of things dropped */
	if (num_item > r_ptr->r_drop_item) r_ptr->r_drop_item = num_item;
	if (num_gold > r_ptr->r_drop_gold) r_ptr->r_drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (FLAG(r_ptr, RF_DROP_GOOD)) r_ptr->r_flags[0] |= (RF0_DROP_GOOD);
	if (FLAG(r_ptr, RF_DROP_GREAT)) r_ptr->r_flags[0] |= (RF0_DROP_GREAT);

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

	/* Changes on screen */
	p_ptr->window |= PW_VISIBLE;

	/* Paranoia */
#if 0
	if (!r_ptr->r_see && (increment == -1)) core("Monster visibility error!");
#else  /* 0 */
	/* Ignore the bug, until we know what is really going on. */
	if (!r_ptr->r_see && (increment == -1)) return;
#endif /* 0 */

	/* Update the counter */
	r_ptr->r_see += increment;

	/* Disturb if necessary */
	if (disturb_view)
		disturb(FALSE);

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
		/* Look to see if we need to recalculate max_seen_ridx */
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
 * "disturb_near" (monster which is "easily" viewable moves in some
 * way).  Note that "moves" includes "appears" and "disappears".
 */
void update_mon(int m_idx, bool full)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int d;

	/* Current location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	/* Seen at all */
	bool flag = FALSE;

	/* Seen by vision */
	bool easy = FALSE;

	pcave_type *pc_ptr;

	/* Exit if monster does not exist. */
	if (!m_idx) return;

	/* Compute distance */
	if (full)
	{
		int py = p_ptr->py;
		int px = p_ptr->px;

		/* Distance components */
		int dy = (py > fy) ? (py - fy) : (fy - py);
		int dx = (px > fx) ? (px - fx) : (fx - px);

		/* Approximate distance */
		d = (dy > dx) ? (dy + (dx / 2)) : (dx + (dy / 2));

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
		if (FLAG(p_ptr, TR_TELEPATHY))
		{
			/* Empty mind, no telepathy */
			if (FLAG(r_ptr, RF_EMPTY_MIND))
			{
				/* Memorize flags */
				r_ptr->r_flags[1] |= (RF1_EMPTY_MIND);
			}

			/* Weird mind, occasional telepathy */
			else if (FLAG(r_ptr, RF_WEIRD_MIND))
			{
				/* One in ten individuals are detectable */
				if ((m_idx % 10) == 5)
				{
					/* Detectable */
					flag = TRUE;

					/* Memorize flags */
					r_ptr->r_flags[1] |= (RF1_WEIRD_MIND);

					/* Hack -- Memorize mental flags */
					if (FLAG(r_ptr, RF_SMART)) r_ptr->r_flags[1] |=
							(RF1_SMART);
					if (FLAG(r_ptr, RF_STUPID)) r_ptr->r_flags[1] |=
							(RF1_STUPID);
				}
			}

			/* Normal mind, allow telepathy */
			else
			{
				/* Detectable */
				flag = TRUE;

				/* Hack -- Memorize mental flags */
				if (FLAG(r_ptr, RF_SMART)) r_ptr->r_flags[1] |= (RF1_SMART);
				if (FLAG(r_ptr, RF_STUPID)) r_ptr->r_flags[1] |=
						(RF1_STUPID);
			}
		}

		/* Paranoia */
		if (in_boundsp(fx, fy))
		{
			pc_ptr = parea(fx, fy);

			/* Normal line of sight, and not blind */
			if (player_has_los_grid(pc_ptr) && !p_ptr->tim.blind)
			{
				bool do_invisible = FALSE;
				bool do_cold_blood = FALSE;

				/* Use "infravision" */
				if (d <= p_ptr->see_infra)
				{
					/* Handle "cold blooded" monsters */
					if (FLAG(r_ptr, RF_COLD_BLOOD))
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
				if ((player_can_see_grid(pc_ptr))
					|| FLAG(r_ptr, RF_LITE_1) || FLAG(r_ptr, RF_LITE_2))
				{
					/* Handle "invisible" monsters */
					if (FLAG(r_ptr, RF_INVISIBLE))
					{
						/* Take note */
						do_invisible = TRUE;

						/* See invisible */
						if (FLAG(p_ptr, TR_SEE_INVIS))
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
					if (do_invisible) r_ptr->r_flags[1] |= (RF1_INVISIBLE);
					if (do_cold_blood) r_ptr->r_flags[1] |= (RF1_COLD_BLOOD);
				}
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

			/* Increment monster visibility counter if we know about it */
			if (!(m_ptr->smart & SM_MIMIC))
			{
				update_mon_vis(m_ptr->r_idx, 1);
			}

			/* Draw the monster */
			lite_spot(fx, fy);

			/* Update health bar as needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Hack -- Count "fresh" sightings */
			if (r_ptr->r_sights < MAX_SHORT) r_ptr->r_sights++;
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

			/* Decrement monster visibility counter if we know about it */
			if (!(m_ptr->smart & SM_MIMIC))
			{
				update_mon_vis(m_ptr->r_idx, -1);
			}

			/* Erase the monster */
			lite_spot(fx, fy);

			/* Update health bar as needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
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
monster_type *place_monster_one(int x, int y, int r_idx, bool slp, bool friendly,
                       bool pet)
{
	int i;

	cave_type *c_ptr;

	monster_type *m_ptr;

	monster_race *r_ptr = &r_info[r_idx];

	cptr name;

	/* Paranoia */
	if (!r_idx) return (NULL);

	/* Paranoia */
	if (!r_ptr->name) return (NULL);
	
	/* Lookup the name of the monster */
	name = mon_race_name(r_ptr);

	/* Verify location */
	if (!in_bounds2(x, y)) return (NULL);

	/* Access the location */
	c_ptr = area(x, y);
	
	/* Walls also stops generation if we aren't ghostly */
	if (cave_wall_grid(c_ptr) && !(FLAG(r_ptr, RF_PASS_WALL)))
	{
		return (NULL);
	}

	/* Not if other monster is here */
	if (c_ptr->m_idx) return (NULL);

	/* Not if player is here */
	if ((y == p_ptr->py) && (x == p_ptr->px)) return (NULL);
	
	if (!test_monster_square(c_ptr, r_ptr)) return (NULL);

	/* Hack -- "unique" monsters must be "unique" */
	if ((FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
		&& (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
		return (NULL);
	}

	/* Depth monsters may NOT be created out of depth, unless in Nightmare mode */
	if (FLAG(r_ptr, RF_FORCE_DEPTH) && (p_ptr->depth < r_ptr->level)
		&& (!ironman_nightmare || FLAG(r_ptr, RF_QUESTOR)))
	{
		/* Cannot create */
		return (NULL);
	}

	/* 
	 * Test for fields that will not allow monsters to
	 * be generated on them.  (i.e. Glyph of warding)
	 */
	if (fields_have_flags(c_ptr, FIELD_INFO_NO_MPLACE)) return (NULL);

	/* Powerful monster */
	if (r_ptr->level > p_ptr->depth)
	{
		/* Unique monsters */
		if (FLAG(r_ptr, RF_UNIQUE))
		{
			/* Message for cheaters */
			if (cheat_hear) msgf("Deep Unique (%s).", name);


			/* Boost rating by twice delta-depth */
			inc_rating((r_ptr->level - p_ptr->depth) * 2);
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
			if (cheat_hear) msgf("Deep Monster (%s).", name);
	
			if (!FLAG(r_ptr, RF_FRIENDS))
			{
				/* Boost rating by delta-depth */
				inc_rating(r_ptr->level - p_ptr->depth);
			}
		}
	}

	/* Note the monster */
	else if (FLAG(r_ptr, RF_UNIQUE))
	{
		/* Unique monsters induce message */
		if (cheat_hear) msgf("Unique (%s).", name);
	}

	/* Make a new monster */
	c_ptr->m_idx = m_pop();

	/* Mega-Hack -- catch "failure" */
	if (!c_ptr->m_idx) return (NULL);

	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;

	/* Region */
	m_ptr->region = cur_region;

	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

	/* Unknown distance */
	m_ptr->cdis = 0;

	/* No flags */
	m_ptr->mflag = 0;

	/* Not visible */
	m_ptr->ml = FALSE;

	/* Pet? */
	if (pet)
	{
		set_pet(m_ptr);
	}
	/* Friendly? */
	else if (friendly || (FLAG(r_ptr, RF_FRIENDLY)))
	{
		set_friendly(m_ptr);
	}

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
	if (slp && r_ptr->sleep && !ironman_nightmare)
	{
		int val = r_ptr->sleep;
		m_ptr->csleep = ((val * 2) + randint1(val * 10));
	}

	/* Assign maximal hitpoints */
	if (FLAG(r_ptr, RF_FORCE_MAXHP))
	{
		m_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		m_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* Monsters have double hitpoints in Nightmare mode */
	if (ironman_nightmare)
	{
		u32b hp = m_ptr->maxhp * 2L;

		m_ptr->maxhp = (s16b)MIN(30000, hp);
	}

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;


	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;

	/* Hack -- small racial variety */
	if (!(FLAG(r_ptr, RF_UNIQUE)))
	{
		/* Allow some small variation per monster */
		i = extract_energy[r_ptr->speed] / 10;
		if (i) m_ptr->mspeed += rand_spread(0, i);
	}


	/* Give a random starting energy */
	m_ptr->energy = (byte)randint0(100);

	/* Nightmare monsters are more prepared */
	if (ironman_nightmare)
	{
		m_ptr->energy *= 2;
	}

	/* Force monster to wait for player, unless in Nightmare mode */
	if ((FLAG(r_ptr, RF_FORCE_SLEEP)) && !ironman_nightmare)
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		p_ptr->change |= (PC_REPAIR);
	}

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_MOVE);
	}

	/* Hack - are we a mimic? */
	if (FLAG(r_ptr, RF_CHAR_MIMIC))
	{
		/* The player doesn't know about us yet */
		m_ptr->smart |= SM_MIMIC;
	}

	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;


	/* Hack -- Count the number of "reproducers" */
	if (FLAG(r_ptr, RF_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (FLAG(r_ptr, RF_ATTR_MULTI)) p_ptr->change |= (PC_SHIMMER);

	/* Success */
	return (m_ptr);
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX	32


/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int x, int y, int r_idx, bool slp,
                                bool friendly, bool pet)
{
	monster_race *r_ptr = &r_info[r_idx];

	int n, i;
	int total = 0, extra = 0;

	int hack_n = 0;

	int hack_y[GROUP_MAX];
	int hack_x[GROUP_MAX];

	cave_type *c_ptr;

	/* Pick a group size */
	total = randint1(13);

	/* Hard monsters, small groups */
	if (r_ptr->level > p_ptr->depth)
	{
		extra = r_ptr->level - p_ptr->depth;
		extra = 0 - randint1(extra);
	}

	/* Easy monsters, large groups */
	else if (r_ptr->level < p_ptr->depth)
	{
		extra = p_ptr->depth - r_ptr->level;
		extra = randint1(extra);
	}

	/* Hack -- limit group reduction */
	if (extra > 12) extra = 12;

	/* Modify the group size */
	total += extra;

	/* Minimum size */
	if (total < 1) total = 1;

	/* Maximum size */
	if (total > GROUP_MAX) total = GROUP_MAX;


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

			scatter(&mx, &my, hx, hy, 4);

			/* paranoia */
			if (!in_bounds2(mx, my)) continue;

			/* Not on player */
			if ((my == p_ptr->py) && (mx == p_ptr->px)) continue;

			/* Walls and Monsters block flow */
			c_ptr = area(mx, my);
			if (!cave_empty_grid(c_ptr)) continue;

			/* Attempt to place another monster */
			if (place_monster_one(mx, my, r_idx, slp, friendly, pet))
			{
				/* Add it to the "hack" set */
				hack_y[hack_n] = my;
				hack_x[hack_n] = mx;
				hack_n++;
			}
		}
	}

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
	if (FLAG(z_ptr, RF_UNIQUE)) return (FALSE);

	/* Paranoia -- Skip identical monsters */
	if (place_monster_idx == r_idx) return (FALSE);

	/* Good vs. evil */
	if ((FLAG(r_ptr, RF_EVIL) && FLAG(z_ptr, RF_GOOD))
		|| (FLAG(r_ptr, RF_GOOD) && FLAG(z_ptr, RF_EVIL)))
	{
		return FALSE;
	}

	/* Hostile vs. non-hostile */
	if (FLAG(r_ptr, RF_FRIENDLY) != FLAG(z_ptr, RF_FRIENDLY))
		return FALSE;

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
monster_type *place_monster_aux(int x, int y, int r_idx, bool slp, bool grp,
                       bool friendly, bool pet, bool summon)
{
	int i;
	monster_race *r_ptr = &r_info[r_idx];
	cave_type *c_ptr;

	monster_type *m_ptr;


	/* Place one monster, or fail */
	m_ptr = place_monster_one(x, y, r_idx, slp, friendly, pet);

	if (!m_ptr) return (NULL);

	/* Require the "group" flag */
	if (!grp) return (m_ptr);

	/* Friends for certain monsters */
	if (FLAG(r_ptr, RF_FRIENDS))
	{
		/* Attempt to place a group */
		(void)place_monster_group(x, y, r_idx, slp, friendly, pet);
	}


	/* Escorts for certain monsters */
	if (FLAG(r_ptr, RF_ESCORT))
	{
		/* Set the escort index */
		place_monster_idx = r_idx;

		/* Try to place several "escorts" */
		for (i = 0; i < 50; i++)
		{
			int nx, ny, z, d = 3;

			/* Pick a location */
			scatter(&nx, &ny, x, y, d);

			/* paranoia */
			if (!in_bounds2(x, y)) continue;

			/* Not on player */
			if ((y == p_ptr->py) && (x == p_ptr->px)) continue;

			/* Require empty grids */
			c_ptr = area(nx, ny);
			if (!cave_empty_grid(c_ptr)) continue;

			/* Prepare allocation table */
			get_mon_num_prep(place_monster_okay);
			
			/* Default to filtering out monsters not normally on this dungeon */
			if (!summon)
			{
				(void) filter_mon_loc(nx, ny);
			}
			
			/* Pick a random race */
			z = get_mon_num(r_ptr->level);

			/* Handle failure */
			if (!z) break;

			/* Place a single escort */
			(void)place_monster_one(nx, ny, z, slp, friendly, pet);

			/* Place a "group" of escorts if needed */
			if (FLAG(&r_info[z], RF_FRIENDS)
				|| FLAG(r_ptr, RF_ESCORTS))
			{
				/* Place a group of monsters */
				(void)place_monster_group(nx, ny, z, slp, friendly, pet);
			}
		}
	}

	/* Success */
	return (m_ptr);
}


/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the level returned by
 * filter_mon_loc()
 */
bool place_monster(int x, int y, bool slp, bool grp, int delta_level)
{
	int r_idx;
	int level;

	/* Prepare allocation table */
	get_mon_num_prep(NULL);
	level = filter_mon_loc(x, y);

	/* Pick a monster */
	r_idx = get_mon_num(level + delta_level);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	if (place_monster_aux(x, y, r_idx, slp, grp, FALSE, FALSE, FALSE)) return (TRUE);

	/* Oops */
	return (FALSE);
}


#ifdef MONSTER_HORDES

bool alloc_horde(int x, int y)
{
	monster_race *r_ptr = NULL;

	int r_idx = 0;
	int m_idx;

	int attempts = 1000;
	int cy = y;
	int cx = x;
	
	int level;

	/* Prepare allocation table */
	get_mon_num_prep(NULL);
	level = filter_mon_loc(x, y);

	while (--attempts)
	{
		/* Pick a monster */
		r_idx = get_mon_num(level);

		/* Handle failure */
		if (!r_idx) return (FALSE);

		r_ptr = &r_info[r_idx];

		if (!(FLAG(r_ptr, RF_UNIQUE))) break;
	}

	if (attempts < 1) return FALSE;

	attempts = 1000;

	while (--attempts)
	{
		/* Attempt to place the monster */
		if (place_monster_aux(x, y, r_idx, FALSE, FALSE, FALSE, FALSE, FALSE)) break;
	}

	if (attempts < 1) return FALSE;

	m_idx = area(x, y)->m_idx;

	summon_kin_type = r_ptr->d_char;

	for (attempts = rand_range(5, 15); attempts; attempts--)
	{
		scatter(&cx, &cy, x, y, 5);

		(void)summon_specific(m_idx, cx, cy, p_ptr->depth + 5, SUMMON_KIN, TRUE,
							  FALSE, FALSE);

		y = cy;
		x = cx;
	}

	return TRUE;
}

#endif /* MONSTER_HORDES */



/*
 * Attempt to allocate a random monster in the dungeon.
 *
 * Place the monster at least "dis" distance from the player.
 *
 * Use "slp" to choose the initial "sleep" status
 */
bool alloc_monster(int dis, bool slp, int delta_level)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y = 0, x = 0;
	int attempts_left = 10000;
	cave_type *c_ptr;

	/* Find a legal, distant, unoccupied, space */
	while (--attempts_left)
	{
		/* Pick a location */
		y = rand_range(p_ptr->min_hgt, p_ptr->max_hgt - 1);
		x = rand_range(p_ptr->min_wid, p_ptr->max_wid - 1);

		/* Not on player */
		if ((y == py) && (x == px)) continue;

		/* Require empty floor grid (was "naked") */
		c_ptr = area(x, y);
		if (!cave_empty_grid(c_ptr)) continue;

		/* Accept far away grids */
		if (distance(x, y, px, py) > dis) break;
	}

	if (!attempts_left)
	{
		if (cheat_xtra || cheat_hear)
		{
			msgf("Warning! Could not allocate a new monster. Small level?");
		}

		return (FALSE);
	}


#ifdef MONSTER_HORDES
	if (randint1(5000) <= p_ptr->depth)
	{
		if (alloc_horde(x, y))
		{
			if (cheat_hear) msgf("Monster horde.");
			return (TRUE);
		}
	}
	else
	{
#endif /* MONSTER_HORDES */

		/* Attempt to place the monster, allow groups */
		if (place_monster(x, y, slp, TRUE, delta_level)) return (TRUE);

#ifdef MONSTER_HORDES
	}
#endif /* MONSTER_HORDES */

	/* Nope */
	return (FALSE);
}




/*
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * Hack -- the index of the summoning monster
 */
static int summon_specific_who = -1;


/*
 * Hack -- the hostility of the summoned monster
 */
static int summon_specific_hostile = TRUE;


/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	bool okay = FALSE;

	/* Hack -- identify the summoning monster */
	if (summon_specific_who > 0)
	{
		monster_type *m_ptr = &m_list[summon_specific_who];
		monster_race *s_ptr = &r_info[m_ptr->r_idx];

		/* Do not summon enemies */

		/* Good vs. evil */
		if ((FLAG(r_ptr, RF_EVIL) && FLAG(s_ptr, RF_GOOD))
			|| (FLAG(r_ptr, RF_GOOD) && FLAG(s_ptr, RF_EVIL)))
		{
			return FALSE;
		}

		/* Hostile vs. non-hostile */
		if (is_hostile(m_ptr) != summon_specific_hostile)
		{
			return FALSE;
		}
	}
	/* Use the player's alignment */
	else if (summon_specific_who < 0)
	{
		/* Do not summon enemies of the pets */
		if (((p_ptr->align < 0) && (FLAG(r_ptr, RF_GOOD)))
			|| ((p_ptr->align > 0) && (FLAG(r_ptr, RF_EVIL))))
		{
			return FALSE;
		}
	}

	/* Hack -- no specific type specified */
	if (!summon_specific_type) return (TRUE);

	/* Check our requirements */
	switch (summon_specific_type)
	{
		case SUMMON_ANT:
		{
			okay = ((r_ptr->d_char == 'a') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_SPIDER:
		{
			okay = ((r_ptr->d_char == 'S') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_HOUND:
		{
			okay = (((r_ptr->d_char == 'C') || (r_ptr->d_char == 'Z'))
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_HYDRA:
		{
			okay = ((r_ptr->d_char == 'M') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_ANGEL:
		{
			okay = ((r_ptr->d_char == 'A') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_DEMON:
		{
			okay = ((FLAG(r_ptr, RF_DEMON))
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_UNDEAD:
		{
			okay = ((FLAG(r_ptr, RF_UNDEAD))
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_DRAGON:
		{
			okay = ((FLAG(r_ptr, RF_DRAGON))
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_HI_UNDEAD:
		{
			okay = ((r_ptr->d_char == 'L') || (r_ptr->d_char == 'V')
					|| (r_ptr->d_char == 'W'));
			break;
		}

		case SUMMON_HI_DRAGON:
		{
			okay = (r_ptr->d_char == 'D');
			break;
		}

		case SUMMON_AMBERITES:
		{
			okay = FLAG(r_ptr, RF_AMBERITE);
			break;
		}

		case SUMMON_UNIQUE:
		{
			okay = FLAG(r_ptr, RF_UNIQUE);
			break;
		}

		case SUMMON_BIZARRE1:
		{
			okay = ((r_ptr->d_char == 'm') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}
		case SUMMON_BIZARRE2:
		{
			okay = ((r_ptr->d_char == 'b') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}
		case SUMMON_BIZARRE3:
		{
			okay = ((r_ptr->d_char == 'Q') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_BIZARRE4:
		{
			okay = ((r_ptr->d_char == 'v') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_BIZARRE5:
		{
			okay = ((r_ptr->d_char == '$') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_BIZARRE6:
		{
			okay =
				(((r_ptr->d_char == '!') || (r_ptr->d_char == '?')
				  || (r_ptr->d_char == '=') || (r_ptr->d_char == '$')
				  || (r_ptr->d_char == '|')) && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_CYBER:
		{
			okay = ((r_ptr->d_char == 'U') && (FLAG(r_ptr, RF_ROCKET))
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}


		case SUMMON_KIN:
		{
			okay = ((r_ptr->d_char == summon_kin_type)
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_DAWN:
		{
			okay = (mon_name_cont(r_ptr, "the Dawn")
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_ANIMAL:
		{
			okay = ((FLAG(r_ptr, RF_ANIMAL))
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_ANIMAL_RANGER:
		{
			okay = ((FLAG(r_ptr, RF_ANIMAL))
					&& (strchr("abcflqrwBCIJKMRS", r_ptr->d_char))
					&& !(FLAG(r_ptr, RF_DRAGON))
					&& !(FLAG(r_ptr, RF_EVIL))
					&& !(FLAG(r_ptr, RF_UNDEAD))
					&& !(FLAG(r_ptr, RF_DEMON))
					&& !(r_ptr->flags[3] || r_ptr->flags[4] || r_ptr->flags[5])
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_HI_UNDEAD_NO_UNIQUES:
		{
			okay =
				(((r_ptr->d_char == 'L') || (r_ptr->d_char == 'V')
				  || (r_ptr->d_char == 'W'))
				 && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_HI_DRAGON_NO_UNIQUES:
		{
			okay = ((r_ptr->d_char == 'D') && !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_NO_UNIQUES:
		{
			okay = (!(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_PHANTOM:
		{
			okay = (mon_name_cont(r_ptr, "Phantom")
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_ELEMENTAL:
		{
			okay = (mon_name_cont(r_ptr, "lemental")
					&& !(FLAG(r_ptr, RF_UNIQUE)));
			break;
		}

		case SUMMON_BLUE_HORROR:
		{
			okay = (mon_name_cont(r_ptr, "lue horror")
					&& !(FLAG(r_ptr, RF_UNIQUE)));
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
 * Note: SUMMON_UNIQUE and SUMMON_AMBERITES will summon Unique's
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
bool summon_specific(int who, int x1, int y1, int req_lev, int type, bool group,
                     bool friendly, bool pet)
{
	int i, x, y, r_idx;
	cave_type *c_ptr;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&x, &y, x1, y1, d);

		/* paranoia */
		if (!in_bounds2(x, y)) continue;

		/* Not on top of player */
		if ((y == p_ptr->py) && (x == p_ptr->px)) continue;

		/* Require "empty" floor grid */
		c_ptr = area(x, y);
		if (!cave_empty_grid(c_ptr)) continue;

		/* ... nor on the Pattern */
		if (cave_pattern_grid(c_ptr)) continue;

		/* Check for a field that blocks movement */
		if (fields_have_flags(c_ptr, FIELD_INFO_NO_ENTER)) continue;

		/* 
		 * Test for fields that will not allow monsters to
		 * be generated on them.  (i.e. Glyph of warding)
		 */
		if (fields_have_flags(c_ptr, FIELD_INFO_NO_MPLACE)) continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Save the summoner */
	summon_specific_who = who;

	/* Save the "summon" type */
	summon_specific_type = type;

	/* Save the hostility */
	summon_specific_hostile = (!friendly && !pet);

	/* Prepare allocation table */
	get_mon_num_prep(summon_specific_okay);

	/* Pick a monster, using the level calculation */
	r_idx = get_mon_num((base_level() + req_lev) / 2 + 5);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(x, y, r_idx, FALSE, group, friendly, pet, TRUE))
		return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * A "dangerous" function, creates a pet of the specified type
 */
monster_type *summon_named_creature(int x1, int y1, int r_idx, bool slp,
									 bool group_ok, bool pet)
{
	int i, x, y;

	cave_type *c_ptr;
	
	monster_type *m_ptr = NULL;

	/* Paranoia */
	/* if (!r_idx) return; */

	/* Prevent illegal monsters */
	if (r_idx >= z_info->r_max) return FALSE;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&x, &y, x1, y1, d);

		/* paranoia */
		if (!in_bounds2(x, y)) continue;

		/* Not on top of player */
		if ((y == p_ptr->py) && (x == p_ptr->px)) continue;

		/* Require empty grids */
		c_ptr = area(x, y);
		if (!cave_empty_grid(c_ptr)) continue;

		/* Place it (allow groups) */
		m_ptr = place_monster_aux(x, y, r_idx, slp, group_ok, FALSE, pet, TRUE);
		if (m_ptr) break;
	}

	return (m_ptr);
}

/*
 * Same as above, but set SM_CLONE,
 * and assume there is only one monster created, which is awake.
 */
monster_type *summon_cloned_creature(int x1, int y1, int r_idx, bool pet)
{
	monster_type *m_ptr = summon_named_creature(x1, y1, r_idx, FALSE, FALSE, pet);

	if (m_ptr)
	{
		/* Set the cloned flag, so no treasure is dropped */
		m_ptr->smart |= SM_CLONED;
	}

	return (m_ptr);
}


/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
monster_type *multiply_monster(int m_idx, bool clone, bool friendly, bool pet)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_type *t_ptr = NULL;

	int i, y, x;

	cave_type *c_ptr;

	/* Try up to 18 times */
	for (i = 0; i < 18; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&x, &y, m_ptr->fx, m_ptr->fy, d);

		/* paranoia */
		if (!in_bounds2(x, y)) continue;

		/* Not on top of player */
		if ((y == p_ptr->py) && (x == p_ptr->px)) continue;

		/* Require an "empty" floor grid */
		c_ptr = area(x, y);
		if (!cave_empty_grid(c_ptr)) continue;

		/* Create a new monster (awake, no groups) */
		t_ptr = place_monster_aux(x, y, m_ptr->r_idx, FALSE, FALSE, friendly, pet, TRUE);

		/* Done */
		break;
	}

	if (clone && t_ptr) t_ptr->smart |= SM_CLONED;

	/* Result */
	return (t_ptr);
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
	monster_desc(m_name, m_ptr, 0, 80);

	/* Notice non-damage */
	if (dam == 0)
	{
		msgf("%^s is unharmed.", m_name);
		return;
	}

	/* Note -- subtle fix -CFT */
	newhp = (long)(m_ptr->hp);
	oldhp = newhp + (long)(dam);
	tmp = (newhp * 100L) / oldhp;
	percentage = (int)(tmp);


	/* Mushrooms, Eyes, Jellies, Molds, Vortices, Worms, Quylthulgs */
	if (strchr(",ejmvwQ", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s barely notices.", m_name);
		else if (percentage > 75)
			msgf("%^s flinches.", m_name);
		else if (percentage > 50)
			msgf("%^s squelches.", m_name);
		else if (percentage > 35)
			msgf("%^s quivers in pain.", m_name);
		else if (percentage > 20)
			msgf("%^s writhes about.", m_name);
		else if (percentage > 10)
			msgf("%^s writhes in agony.", m_name);
		else
			msgf("%^s jerks limply.", m_name);
	}


	/* Fish */
	else if (strchr("~", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s barely notices.", m_name);
		else if (percentage > 75)
			msgf("%^s flinches.", m_name);
		else if (percentage > 50)
			msgf("%^s hesitates.", m_name);
		else if (percentage > 35)
			msgf("%^s quivers in pain.", m_name);
		else if (percentage > 20)
			msgf("%^s writhes about.", m_name);
		else if (percentage > 10)
			msgf("%^s writhes in agony.", m_name);
		else
			msgf("%^s jerks limply.", m_name);
	}


	/* Golems, Walls, Doors, Stairs */
	else if (strchr("g#+<>", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s shrugs off the attack.", m_name);
		else if (percentage > 50)
			msgf("%^s roars thunderously.", m_name);
		else if (percentage > 35)
			msgf("%^s rumbles.", m_name);
		else if (percentage > 20)
			msgf("%^s grunts.", m_name);
		else if (percentage > 10)
			msgf("%^s hesitates.", m_name);
		else
			msgf("%^s crumples.", m_name);
	}


	/* Snakes, Hydrae, Reptiles, Mimics */
	else if (strchr("JMR", r_ptr->d_char) || !isalpha(r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s barely notices.", m_name);
		else if (percentage > 75)
			msgf("%^s hisses.", m_name);
		else if (percentage > 50)
			msgf("%^s rears up in anger.", m_name);
		else if (percentage > 35)
			msgf("%^s hisses furiously.", m_name);
		else if (percentage > 20)
			msgf("%^s writhes about.", m_name);
		else if (percentage > 10)
			msgf("%^s writhes in agony.", m_name);
		else
			msgf("%^s jerks limply.", m_name);
	}


	/* Felines */
	else if (strchr("f", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s roars.", m_name);
		else if (percentage > 50)
			msgf("%^s growls angrily.", m_name);
		else if (percentage > 35)
			msgf("%^s hisses with pain.", m_name);
		else if (percentage > 20)
			msgf("%^s mewls in pain.", m_name);
		else if (percentage > 10)
			msgf("%^s hisses in agony.", m_name);
		else
			msgf("%^s mewls pitifully.", m_name);
	}


	/* Ants, Centipedes, Flies, Insects, Beetles, Spiders */
	else if (strchr("acFIKS", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s chitters.", m_name);
		else if (percentage > 50)
			msgf("%^s scuttles about.", m_name);
		else if (percentage > 35)
			msgf("%^s twitters.", m_name);
		else if (percentage > 20)
			msgf("%^s jerks in pain.", m_name);
		else if (percentage > 10)
			msgf("%^s jerks in agony.", m_name);
		else
			msgf("%^s twitches.", m_name);
	}


	/* Birds */
	else if (strchr("B", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s chirps.", m_name);
		else if (percentage > 75)
			msgf("%^s twitters.", m_name);
		else if (percentage > 50)
			msgf("%^s squawks.", m_name);
		else if (percentage > 35)
			msgf("%^s chatters.", m_name);
		else if (percentage > 20)
			msgf("%^s jeers.", m_name);
		else if (percentage > 10)
			msgf("%^s flutters about.", m_name);
		else
			msgf("%^s squeaks.", m_name);
	}


	/* Dragons, Demons, High Undead */
	else if (strchr("duDLUW", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s flinches.", m_name);
		else if (percentage > 50)
			msgf("%^s hisses in pain.", m_name);
		else if (percentage > 35)
			msgf("%^s snarls with pain.", m_name);
		else if (percentage > 20)
			msgf("%^s roars with pain.", m_name);
		else if (percentage > 10)
			msgf("%^s gasps.", m_name);
		else
			msgf("%^s snarls feebly.", m_name);
	}


	/* Skeletons */
	else if (strchr("s", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s shrugs off the attack.", m_name);
		else if (percentage > 50)
			msgf("%^s rattles.", m_name);
		else if (percentage > 35)
			msgf("%^s stumbles.", m_name);
		else if (percentage > 20)
			msgf("%^s rattles.", m_name);
		else if (percentage > 10)
			msgf("%^s staggers.", m_name);
		else
			msgf("%^s clatters.", m_name);
	}


	/* Zombies */
	else if (strchr("z", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s shrugs off the attack.", m_name);
		else if (percentage > 50)
			msgf("%^s groans.", m_name);
		else if (percentage > 35)
			msgf("%^s moans.", m_name);
		else if (percentage > 20)
			msgf("%^s hesitates.", m_name);
		else if (percentage > 10)
			msgf("%^s grunts.", m_name);
		else
			msgf("%^s staggers.", m_name);
	}


	/* Ghosts */
	else if (strchr("G", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s shrugs off the attack.", m_name);
		else if (percentage > 50)
			msgf("%^s moans.", m_name);
		else if (percentage > 35)
			msgf("%^s wails.", m_name);
		else if (percentage > 20)
			msgf("%^s howls.", m_name);
		else if (percentage > 10)
			msgf("%^s moans softly.", m_name);
		else
			msgf("%^s sighs.", m_name);
	}


	/* Dogs and Hounds */
	else if (strchr("CZ", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s snarls with pain.", m_name);
		else if (percentage > 50)
			msgf("%^s yelps in pain.", m_name);
		else if (percentage > 35)
			msgf("%^s howls in pain.", m_name);
		else if (percentage > 20)
			msgf("%^s howls in agony.", m_name);
		else if (percentage > 10)
			msgf("%^s writhes in agony.", m_name);
		else
			msgf("%^s yelps feebly.", m_name);
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (strchr("Xbilqrt", r_ptr->d_char))
	{
		if (percentage > 95)
			msgf("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s grunts with pain.", m_name);
		else if (percentage > 50)
			msgf("%^s squeals in pain.", m_name);
		else if (percentage > 35)
			msgf("%^s shrieks in pain.", m_name);
		else if (percentage > 20)
			msgf("%^s shrieks in agony.", m_name);
		else if (percentage > 10)
			msgf("%^s writhes in agony.", m_name);
		else
			msgf("%^s cries out feebly.", m_name);
	}

	/* Another type of monsters (shrug,cry,scream) */
	else
	{
		if (percentage > 95)
			msgf("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msgf("%^s grunts with pain.", m_name);
		else if (percentage > 50)
			msgf("%^s cries out in pain.", m_name);
		else if (percentage > 35)
			msgf("%^s screams in pain.", m_name);
		else if (percentage > 20)
			msgf("%^s screams in agony.", m_name);
		else if (percentage > 10)
			msgf("%^s writhes in agony.", m_name);
		else
			msgf("%^s cries out feebly.", m_name);
	}
}


/*
 * Learn about an "observed" resistance.
 */
void update_smart_learn(int m_idx, int what)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Too stupid to learn anything */
	if (FLAG(r_ptr, RF_STUPID)) return;

	/* Not intelligent, only learn sometimes */
	if (!FLAG(r_ptr, RF_SMART) && (randint0(100) < 50)) return;


	/* XXX XXX XXX */

	/* Analyze the knowledge */
	switch (what)
	{
		case DRS_ACID:
		{
			if (FLAG(p_ptr, TR_RES_ACID)) m_ptr->smart |= (SM_RES_ACID);
			if (p_ptr->tim.oppose_acid) m_ptr->smart |= (SM_OPP_ACID);
			if (FLAG(p_ptr, TR_IM_ACID)) m_ptr->smart |= (SM_IMM_ACID);
			break;
		}
		case DRS_ELEC:
		{
			if (FLAG(p_ptr, TR_RES_ELEC)) m_ptr->smart |= (SM_RES_ELEC);
			if (p_ptr->tim.oppose_elec) m_ptr->smart |= (SM_OPP_ELEC);
			if (FLAG(p_ptr, TR_IM_ELEC)) m_ptr->smart |= (SM_IMM_ELEC);
			break;
		}
		case DRS_FIRE:
		{
			if (FLAG(p_ptr, TR_RES_FIRE)) m_ptr->smart |= (SM_RES_FIRE);
			if (p_ptr->tim.oppose_fire) m_ptr->smart |= (SM_OPP_FIRE);
			if (FLAG(p_ptr, TR_IM_FIRE)) m_ptr->smart |= (SM_IMM_FIRE);
			break;
		}
		case DRS_COLD:
		{
			if (FLAG(p_ptr, TR_RES_COLD)) m_ptr->smart |= (SM_RES_COLD);
			if (p_ptr->tim.oppose_cold) m_ptr->smart |= (SM_OPP_COLD);
			if (FLAG(p_ptr, TR_IM_COLD)) m_ptr->smart |= (SM_IMM_COLD);
			break;
		}
		case DRS_POIS:
		{
			if (FLAG(p_ptr, TR_RES_POIS)) m_ptr->smart |= (SM_RES_POIS);
			if (p_ptr->tim.oppose_pois) m_ptr->smart |= (SM_OPP_POIS);
			break;
		}
		case DRS_NETH:
		{
			if (FLAG(p_ptr, TR_RES_NETHER)) m_ptr->smart |= (SM_RES_NETH);
			break;
		}
		case DRS_LITE:
		{
			if (FLAG(p_ptr, TR_RES_LITE)) m_ptr->smart |= (SM_RES_LITE);
			break;
		}
		case DRS_DARK:
		{
			if (FLAG(p_ptr, TR_RES_DARK)) m_ptr->smart |= (SM_RES_DARK);
			break;
		}
		case DRS_FEAR:
		{
			if (FLAG(p_ptr, TR_RES_FEAR)) m_ptr->smart |= (SM_RES_FEAR);
			break;
		}
		case DRS_CONF:
		{
			if (FLAG(p_ptr, TR_RES_CONF)) m_ptr->smart |= (SM_RES_CONF);
			break;
		}
		case DRS_CHAOS:
		{
			if (FLAG(p_ptr, TR_RES_CHAOS)) m_ptr->smart |= (SM_RES_CHAOS);
			break;
		}
		case DRS_DISEN:
		{
			if (FLAG(p_ptr, TR_RES_DISEN)) m_ptr->smart |= (SM_RES_DISEN);
			break;
		}
		case DRS_BLIND:
		{
			if (FLAG(p_ptr, TR_RES_BLIND)) m_ptr->smart |= (SM_RES_BLIND);
			break;
		}
		case DRS_NEXUS:
		{
			if (FLAG(p_ptr, TR_RES_NEXUS)) m_ptr->smart |= (SM_RES_NEXUS);
			break;
		}
		case DRS_SOUND:
		{
			if (FLAG(p_ptr, TR_RES_SOUND)) m_ptr->smart |= (SM_RES_SOUND);
			break;
		}
		case DRS_SHARD:
		{
			if (FLAG(p_ptr, TR_RES_SHARDS)) m_ptr->smart |= (SM_RES_SHARD);
			break;
		}
		case DRS_FREE:
		{
			if (FLAG(p_ptr, TR_FREE_ACT)) m_ptr->smart |= (SM_IMM_FREE);
			break;
		}
		case DRS_MANA:
		{
			if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
			break;
		}
		case DRS_REFLECT:
		{
			if (FLAG(p_ptr, TR_REFLECT)) m_ptr->smart |= (SM_IMM_REFLECT);
			break;
		}
	}
}
