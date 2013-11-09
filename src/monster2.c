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

/*
 * Set the target of counter attack
 */
void set_target(monster_type *m_ptr, int y, int x)
{
	m_ptr->target_y = y;
	m_ptr->target_x = x;
}


/*
 * Reset the target of counter attack
 */
void reset_target(monster_type *m_ptr)
{
	set_target(m_ptr, 0, 0);
}


/*
 *  Extract monster race pointer of a monster's true form
 */
monster_race *real_r_ptr(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	return r_ptr;
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
	real_r_ptr(m_ptr)->cur_num--;

	/* Hack -- count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro--;

	/* Hack -- Count the number of "anti-magic monsters" */
	if (r_ptr->flags3 & RF3_ANTI_MAGIC)
	{
		int j;

		/* Pack the list */
		for (j = 0; j < num_anti_magic; j++)
		{
			/* Move the last target to here */
			if (anti_magic_m_idx[j] == i)
			{
				if (j < (num_anti_magic - 1))
					anti_magic_m_idx[j] = anti_magic_m_idx[num_anti_magic - 1];
				num_anti_magic--;
				break;
			}
		}
	}

	/* Hack -- Count the number of "fear field monsters" */
	if (r_ptr->flags3 & RF3_FEAR_FIELD)
	{
		int j;

		/* Pack the list */
		for (j = 0; j < num_fear_field; j++)
		{
			/* Move the last target to here */
			if (fear_field_m_idx[j] == i)
			{
				if (j < (num_fear_field - 1))
					fear_field_m_idx[j] = fear_field_m_idx[num_fear_field - 1];
				num_fear_field--;
				break;
			}
		}
	}


	if (MON_CSLEEP(m_ptr)) (void)set_monster_csleep(i, 0);
	if (MON_FAST(m_ptr)) (void)set_monster_fast(i, 0);
	if (MON_SLOW(m_ptr)) (void)set_monster_slow(i, 0);
	if (MON_STUNNED(m_ptr)) (void)set_monster_stunned(i, 0);
	if (MON_CONFUSED(m_ptr)) (void)set_monster_confused(i, 0);
	if (MON_MONFEAR(m_ptr)) (void)set_monster_monfear(i, 0);
	if (MON_STONING(m_ptr)) (void)set_monster_stoning(i, 0);
	if (MON_MELT_WEAPON(m_ptr)) (void)set_monster_melt_weapon(i, 0);
	if (MON_OPPOSITE_ELEM(m_ptr)) (void)set_monster_opposite_elem(i, 0);
	if (MON_SILENT(m_ptr)) (void)set_monster_silent(i, 0);
	if (MON_INVULNER(m_ptr)) (void)set_monster_invulner(i, 0, FALSE);

	/* Hack -- remove target monster */
	if (i == target_who) target_who = 0;

	/* Hack -- remove tracked monster */
	if (i == p_ptr->health_who) health_track(0);

	if (pet_t_m_idx == i ) pet_t_m_idx = 0;
	if (riding_t_m_idx == i) riding_t_m_idx = 0;
	if (p_ptr->riding == i) p_ptr->riding = 0;

	/* Monster is gone */
	cave[y][x].m_idx = 0;


	/* Delete objects */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/*
		 * o_ptr->held_m_idx is needed in delete_object_idx()
		 * to prevent calling lite_spot()
		 */

		/* Delete the object */
		delete_object_idx(this_o_idx);
	}


	if (is_pet(m_ptr)) check_pets_num_and_align(m_ptr, FALSE);


	/* Wipe the Monster */
	(void)WIPE(m_ptr, monster_type);

	/* Count monsters */
	m_cnt--;

	/* Visual update */
	if (!character_icky) lite_spot(y, x);

	/* Update some things */
	if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_SELF_LITE_1 | RF7_HAS_LITE_2 | RF7_SELF_LITE_2))
		p_ptr->update |= (PU_MON_LITE);
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(int y, int x)
{
	cave_type *c_ptr;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Check the grid */
	c_ptr = &cave[y][x];

	/* Delete the monster (if any) */
	if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx);
}


/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_monsters_aux(int i1, int i2)
{
	int y, x, i;

	cave_type *c_ptr;

	monster_type *m_ptr;

	s16b this_o_idx, next_o_idx = 0;


	/* Do nothing */
	if (i1 == i2) return;


	/* Old monster */
	m_ptr = &m_list[i1];

	/* Location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Cave grid */
	c_ptr = &cave[y][x];

	/* Update the cave */
	c_ptr->m_idx = i2;

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
	if (target_who == i1) target_who = i2;

	/* Hack -- Update the target */
	if (pet_t_m_idx == i1) pet_t_m_idx = i2;
	if (riding_t_m_idx == i1) riding_t_m_idx = i2;

	/* Hack -- Update the riding */
	if (p_ptr->riding == i1) p_ptr->riding = i2;

	/* Hack -- Update the health bar */
	if (p_ptr->health_who == i1) health_track(i2);

	/* Hack -- Update parent index */
	if (is_pet(m_ptr))
	{
		for (i = 1; i < m_max; i++)
		{
			monster_type *m2_ptr = &m_list[i];

			if (m2_ptr->parent_m_idx == i1)
				m2_ptr->parent_m_idx = i2;
		}
	}

	/* Structure copy */
	COPY(&m_list[i2], &m_list[i1], monster_type);

	/* Wipe the hole */
	(void)WIPE(&m_list[i1], monster_type);

	for (i = 0; i < MAX_MTIMED; i++)
	{
		int mproc_idx = get_mproc_idx(i1, i);
		if (mproc_idx >= 0) mproc_list[i][mproc_idx] = i2;
	}
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
	int		i, num, cnt;
	int		cur_lev, cur_dis, chance;

	/* Message (only if compacting) */
#ifdef JP
	if (size) msg_print("モンスター情報を圧縮しています...");
#else
	if (size) msg_print("Compacting monsters...");
#endif


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

			if (i == p_ptr->riding) continue;

			/* Ignore nearby monsters */
			if ((cur_dis > 0) && (m_ptr->cdis < cur_dis)) continue;

			/* Saving throw chance */
			chance = 90;

			/* Only compact "Quest" Monsters in emergencies */
			if ((r_ptr->flags1 & (RF1_QUESTOR)) && (cnt < 1000)) chance = 100;

			/* Try not to compact Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) chance = 100;

			/* All monsters get a saving throw */
			if (randint0(100) < chance) continue;

			/* Check for quest completion */
			check_quest_completion(m_ptr);

			if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
			{
				char m_name[80];

				monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
				do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_COMPACT, m_name);
			}

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
	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Delete all the monsters */
	for (i = m_max - 1; i >= 1; i--)
	{
		m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Monster is gone */
		cave[m_ptr->fy][m_ptr->fx].m_idx = 0;

		/* Wipe the Monster */
		(void)WIPE(m_ptr, monster_type);

	}

	for (i = 1; i < (max_r_idx + runeweapon_num); i++)
	{
		r_ptr = &r_info[i];

		/* Mega-Hack -- preserve Unique's XXX XXX XXX */

		/* Hack -- Reduce the racial counter */
		r_ptr->cur_num = 0;
	}

	for (i = 0; i < MAX_STOCK_MON; i++)
	{
		m_ptr = &stock_mon[i];

		/* Skip unstocked monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Increase the racial counter */
		real_r_ptr(m_ptr)->cur_num++;
	}

	/*
	 * Wiping racial counters of all monsters and incrementing of racial
	 * counters of monsters in party_mon[] is required to prevent multiple
	 * generation of unique monster who is the minion of player.
	 */

	/* Hack -- Wipe the racial counter of all monster races */
	for (i = 1; i < max_r_idx; i++) r_info[i].cur_num = 0;

	/* Reset "m_max" */
	m_max = 1;

	/* Reset "m_cnt" */
	m_cnt = 0;

	/* Reset "mproc_max[]" */
	for (i = 0; i < MAX_MTIMED; i++) mproc_max[i] = 0;

	/* Hack -- reset "reproducer" count */
	num_repro = 0;

	/* Hack -- reset "anti-magic monster" count */
	num_anti_magic = 0;

	/* Hack -- reset "fear field monster" count */
	num_fear_field = 0;

	/* Hack -- no more target */
	target_who = 0;
	pet_t_m_idx = 0;
	riding_t_m_idx = 0;

	/* Hack -- no more tracking */
	health_track(0);

	/* Not found runeweapons come back */
	for (i = 1; i <= runeweapon_num; i++)
	{
		r_ptr = &r_info[runeweapon_r_idx_from(i)];
		if ((r_ptr->max_num == 0) && !(runeweapon_list[i].status & RW_STATUS_FOUND))
			r_ptr->max_num = 1;
	}
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
	if (m_max < max_m_idx)
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
#ifdef JP
	if (character_dungeon) msg_print("モンスターが多すぎる！");
#else
	if (character_dungeon) msg_print("Too many monsters!");
#endif


	/* Try not to crash */
	return (0);
}




/*
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * Hack -- the index of the summoning monster
 */
static int summon_specific_who = -1;


static bool summon_unique_okay = FALSE;


static bool summon_specific_aux(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	int okay = FALSE;

	/* Check our requirements */
	switch (summon_specific_type)
	{
		case SUMMON_ANT:
		{
			okay = (r_ptr->d_char == 'a');
			break;
		}

		case SUMMON_SPIDER:
		{
			okay = (r_ptr->d_char == 'S');
			break;
		}

		case SUMMON_HOUND:
		{
			okay = ((r_ptr->d_char == 'C') || (r_ptr->d_char == 'Z'));
			break;
		}

		case SUMMON_BEAST:
		{
			okay = (r_ptr->d_char == 'H');
			break;
		}

		case SUMMON_ANGEL:
		{
			okay = (r_ptr->d_char == 'A' && ((r_ptr->flags3 & RF3_EVIL) || (r_ptr->flags3 & RF3_GOOD)));
			break;
		}

		case SUMMON_DEMON:
		{
			okay = (r_ptr->flags3 & RF3_DEMON);
			break;
		}

		case SUMMON_UNDEAD:
		{
			okay = (r_ptr->flags3 & RF3_UNDEAD);
			break;
		}

		case SUMMON_DRAGON:
		{
			okay = (r_ptr->flags3 & RF3_DRAGON);
			break;
		}

		case SUMMON_HI_DEMON:
		{
			okay = (r_ptr->d_char == 'U');
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

		case SUMMON_TEMPLES:
		{
			okay = (r_ptr->flags3 & (RF3_TEMPLE)) ? TRUE : FALSE;
			break;
		}

		case SUMMON_UNIQUE:
		{
			okay = (r_ptr->flags1 & (RF1_UNIQUE)) ? TRUE : FALSE;
			break;
		}

		case SUMMON_MOLD:
		{
			okay = (r_ptr->d_char == 'm');
			break;
		}

		case SUMMON_GOLEM:
		{
			okay = (r_ptr->d_char == 'g');
			break;
		}

		case SUMMON_KIN:
		{
			okay = ((r_ptr->d_char == summon_kin_type) &&
			        !(r_ptr->flags1 & RF1_NO_ESCORT));
			break;
		}

		case SUMMON_ANIMAL:
		{
			okay = (r_ptr->flags3 & (RF3_ANIMAL));
			break;
		}

		case SUMMON_ANIMAL_RANGER:
		{
			okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
			       (my_strchr("abcflqrwBCHIJKMRS", r_ptr->d_char)) &&
			       !(r_ptr->flags3 & (RF3_DRAGON)) &&
			       !(r_ptr->flags3 & (RF3_EVIL)) &&
			       !(r_ptr->flags3 & (RF3_UNDEAD)) &&
			       !(r_ptr->flags3 & (RF3_DEMON)) &&
			       !(r_ptr->flags2 & (RF2_MULTIPLY)) &&
			       !(r_ptr->flags4 || r_ptr->flags5 || r_ptr->flags6));
			break;
		}

		case SUMMON_PHANTOM:
		{
			okay = (r_idx == MON_PHANTOM_B || r_idx == MON_PHANTOM_W);
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

		case SUMMON_HUMANS:
		{
			okay = (!(r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) &&
			        !(r_ptr->flags2 & RF2_ETHNICITY_MASK) &&
			        !(r_ptr->flags3 & (RF3_EVIL | RF3_GOOD)) &&
			        !(r_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2)) &&
			         (r_ptr->d_char == 'p'));
			break;
		}

		case SUMMON_RIDING:
		{
			okay = (r_ptr->flags7 & RF7_RIDING);
			break;
		}

		case SUMMON_RIDING_UNIQUE:
		{
			okay = ((r_ptr->flags1 & RF1_UNIQUE) && (r_ptr->flags7 & RF7_RIDING));
			break;
		}

		case SUMMON_GOOD_LAW:
		{
			okay = ((r_ptr->flags3 & RF3_GOOD) && (r_ptr->flags7 & RF7_LAWFUL));
			break;
		}

		case SUMMON_EAGLES:
		{
			okay = ((r_ptr->d_char == 'B') &&
				(r_ptr->flags8 & RF8_WILD_MOUNTAIN) &&
				(r_ptr->flags8 & RF8_WILD_ONLY));
			break;
		}

		case SUMMON_ZENOBIAN_FORCES:
		{
			okay = (r_ptr->flags7 & RF7_ZENOBIAN_FORCES) ? TRUE : FALSE;
			break;
		}

		case SUMMON_PIRANHAS:
		{
			okay = (r_idx == MON_PIRANHA);
			break;
		}

		case SUMMON_FIRE:
		{
			okay = ((r_ptr->flags3 & RF3_ELEM_FIRE)  &&
						 ((r_ptr->d_char == 'E') ||
						 (r_ptr->d_char == 'v') ||
						 (r_ptr->d_char == 'Z')));

			break;
		}

		case SUMMON_AQUA:
		{
			okay = ((r_ptr->flags3 & RF3_ELEM_AQUA)  &&
						 ((r_ptr->d_char == 'E') ||
						 (r_ptr->d_char == 'v') ||
						 (r_ptr->d_char == 'Z')));
			break;
		}

		case SUMMON_EARTH:
		{
			okay = ((r_ptr->flags3 & RF3_ELEM_EARTH)  &&
						 ((r_ptr->d_char == 'E') ||
						 (r_ptr->d_char == 'v') ||
						 (r_ptr->d_char == 'Z')));
			break;
		}

		case SUMMON_WIND:
		{
			okay = ((r_ptr->flags3 & RF3_ELEM_WIND)  &&
						 ((r_ptr->d_char == 'E') ||
						 (r_ptr->d_char == 'v') ||
						 (r_ptr->d_char == 'Z')));
			break;
		}

		case SUMMON_ARMAGE_GOOD:
		{
			okay = (r_ptr->d_char == 'A' && (r_ptr->flags3 & RF3_GOOD));
			break;
		}

		case SUMMON_ARMAGE_EVIL:
		{
			okay = ((r_ptr->flags3 & RF3_DEMON) ||
				(r_ptr->d_char == 'A' && (r_ptr->flags3 & RF3_EVIL)));
			break;
		}
	}

	/* Result */
	/* Since okay is int, "return (okay);" is not correct. */
	return (bool)(okay ? TRUE : FALSE);
}


/*
 * Some dungeon types restrict the possible monsters.
 * Return TRUE is the monster is OK and FALSE otherwise
 */
static bool restrict_monster_to_dungeon(int r_idx)
{
	dungeon_info_type *d_ptr = &d_info[dungeon_type];
	monster_race *r_ptr = &r_info[r_idx];
	byte a;

	if (d_ptr->flags1 & DF1_BEGINNER)
	{
		if (r_ptr->level > dun_level)
			return FALSE;
	}

	if (d_ptr->special_div == 64) return TRUE;
	if (summon_specific_type) return TRUE;

	if(d_ptr->mode == DUNGEON_MODE_AND)
	{
		if (d_ptr->mflags1)
		{
			if((d_ptr->mflags1 & r_ptr->flags1) != d_ptr->mflags1)
				return FALSE;
		}
		if (d_ptr->mflags2)
		{
			if((d_ptr->mflags2 & r_ptr->flags2) != d_ptr->mflags2)
				return FALSE;
		}
		if (d_ptr->mflags3)
		{
			if((d_ptr->mflags3 & r_ptr->flags3) != d_ptr->mflags3)
				return FALSE;
		}
		if (d_ptr->mflags4)
		{
			if((d_ptr->mflags4 & r_ptr->flags4) != d_ptr->mflags4)
				return FALSE;
		}
		if (d_ptr->mflags5)
		{
			if((d_ptr->mflags5 & r_ptr->flags5) != d_ptr->mflags5)
				return FALSE;
		}
		if (d_ptr->mflags6)
		{
			if((d_ptr->mflags6 & r_ptr->flags6) != d_ptr->mflags6)
				return FALSE;
		}
		if (d_ptr->mflags7)
		{
			if((d_ptr->mflags7 & r_ptr->flags7) != d_ptr->mflags7)
				return FALSE;
		}
		if (d_ptr->mflags8)
		{
			if((d_ptr->mflags8 & r_ptr->flags8) != d_ptr->mflags8)
				return FALSE;
		}
		if (d_ptr->mflags9)
		{
			if((d_ptr->mflags9 & r_ptr->flags9) != d_ptr->mflags9)
				return FALSE;
		}
		if (d_ptr->mflagsa)
		{
			if((d_ptr->mflagsa & r_ptr->flagsa) != d_ptr->mflagsa)
				return FALSE;
		}
		if (d_ptr->mflagsr)
		{
			if((d_ptr->mflagsr & r_ptr->flagsr) != d_ptr->mflagsr)
				return FALSE;
		}
		for(a = 0; a < 5; a++)
			if(d_ptr->r_char[a] && (d_ptr->r_char[a] != r_ptr->d_char)) return FALSE;
	}
	else if(d_ptr->mode == DUNGEON_MODE_NAND)
	{
		byte ok[11 + 5], i = 0, j = 0;

		if (d_ptr->mflags1)
		{
			i++;
			if(d_ptr->mflags1 & r_ptr->flags1)
				ok[0] = 1;
		}
		if (d_ptr->mflags2)
		{
			i++;
			if(d_ptr->mflags2 & r_ptr->flags2)
				ok[1] = 1;
		}
		if (d_ptr->mflags3)
		{
			i++;
			if(d_ptr->mflags3 & r_ptr->flags3)
				ok[2] = 1;
		}
		if (d_ptr->mflags4)
		{
			i++;
			if(d_ptr->mflags4 & r_ptr->flags4)
				ok[3] = 1;
		}
		if (d_ptr->mflags5)
		{
			i++;
			if(d_ptr->mflags5 & r_ptr->flags5)
				ok[4] = 1;
		}
		if (d_ptr->mflags6)
		{
			i++;
			if(d_ptr->mflags6 & r_ptr->flags6)
				ok[5] = 1;
		}
		if (d_ptr->mflags7)
		{
			i++;
			if(d_ptr->mflags7 & r_ptr->flags7)
				ok[6] = 1;
		}
		if (d_ptr->mflags8)
		{
			i++;
			if(d_ptr->mflags8 & r_ptr->flags8)
				ok[7] = 1;
		}
		if (d_ptr->mflags9)
		{
			i++;
			if(d_ptr->mflags9 & r_ptr->flags9)
				ok[8] = 1;
		}
		if (d_ptr->mflagsa)
		{
			i++;
			if(d_ptr->mflagsa & r_ptr->flagsa)
				ok[9] = 1;
		}
		if (d_ptr->mflagsr)
		{
			i++;
			if(d_ptr->mflagsr & r_ptr->flagsr)
				ok[10] = 1;
		}

		for(a = 0; a < 5; a++)
		{
			if(d_ptr->r_char[a])
			{
				i++;
				if (d_ptr->r_char[a] != r_ptr->d_char) ok[11 + a] = 1;
			}
		}

		j = ok[0] + ok[1] + ok[2] + ok[3] + ok[4] + ok[5] + ok[6] + ok[7] + ok[8] + ok[9] + ok[10] + ok[11] + ok[12] + ok[13] + ok[14] + ok[15];

		if(i == j) return FALSE;
	}
	else if(d_ptr->mode == DUNGEON_MODE_OR)
	{
		byte ok = FALSE, i;
		s32b flag;

		for(i = 0; i < 32; i++)
		{
			flag = d_ptr->mflags1 & (1 << i);
			if(r_ptr->flags1 & flag) ok = TRUE;

			flag = d_ptr->mflags2 & (1 << i);
			if(r_ptr->flags2 & flag) ok = TRUE;

			flag = d_ptr->mflags3 & (1 << i);
			if(r_ptr->flags3 & flag) ok = TRUE;

			flag = d_ptr->mflags4 & (1 << i);
			if(r_ptr->flags4 & flag) ok = TRUE;

			flag = d_ptr->mflags5 & (1 << i);
			if(r_ptr->flags5 & flag) ok = TRUE;

			flag = d_ptr->mflags6 & (1 << i);
			if(r_ptr->flags6 & flag) ok = TRUE;

			flag = d_ptr->mflags7 & (1 << i);
			if(r_ptr->flags7 & flag) ok = TRUE;

			flag = d_ptr->mflags8 & (1 << i);
			if(r_ptr->flags8 & flag) ok = TRUE;

			flag = d_ptr->mflags9 & (1 << i);
			if(r_ptr->flags9 & flag) ok = TRUE;

			flag = d_ptr->mflagsa & (1 << i);
			if(r_ptr->flagsa & flag) ok = TRUE;

			flag = d_ptr->mflagsr & (1 << i);
			if(r_ptr->flagsr & flag) ok = TRUE;
		}
		for(a = 0; a < 5; a++)
			if(d_ptr->r_char[a] == r_ptr->d_char) ok = TRUE;

		return ok;
	}
	else if(d_ptr->mode == DUNGEON_MODE_NOR)
	{
		byte ok = TRUE, i;
		s32b flag;

		for(i = 0; i < 32; i++)
		{
			flag = d_ptr->mflags1 & (1 << i);
			if(r_ptr->flags1 & flag) ok = FALSE;

			flag = d_ptr->mflags2 & (1 << i);
			if(r_ptr->flags2 & flag) ok = FALSE;

			flag = d_ptr->mflags3 & (1 << i);
			if(r_ptr->flags3 & flag) ok = FALSE;

			flag = d_ptr->mflags4 & (1 << i);
			if(r_ptr->flags4 & flag) ok = FALSE;

			flag = d_ptr->mflags5 & (1 << i);
			if(r_ptr->flags5 & flag) ok = FALSE;

			flag = d_ptr->mflags6 & (1 << i);
			if(r_ptr->flags6 & flag) ok = FALSE;

			flag = d_ptr->mflags7 & (1 << i);
			if(r_ptr->flags7 & flag) ok = FALSE;

			flag = d_ptr->mflags8 & (1 << i);
			if(r_ptr->flags8 & flag) ok = FALSE;

			flag = d_ptr->mflags9 & (1 << i);
			if(r_ptr->flags9 & flag) ok = FALSE;

			flag = d_ptr->mflagsa & (1 << i);
			if(r_ptr->flagsa & flag) ok = FALSE;

			flag = d_ptr->mflagsr & (1 << i);
			if(r_ptr->flagsr & flag) ok = FALSE;
		}
		for(a = 0; a < 5; a++)
			if(d_ptr->r_char[a] == r_ptr->d_char) ok = FALSE;
		return ok;
	}

	return TRUE;
}

/*
 * Apply a "monster restriction function" to the "monster allocation table"
 */
errr get_mon_num_prep(monster_hook_type monster_hook,
					  monster_hook_type monster_hook2)
{
	int i;

	s16b alloc_race_size_2 = alloc_race_size + runeweapon_num;

	/* Todo: Check the hooks for non-changes */

	/* Set the new hooks */
	get_mon_num_hook = monster_hook;
	get_mon_num2_hook = monster_hook2;

	/* Scan the allocation table */
	for (i = 0; i < alloc_race_size_2; i++)
	{
		monster_race	*r_ptr;

		/* Get the entry */
		alloc_entry *entry = &alloc_race_table[i];

		entry->prob2 = 0;
		r_ptr = &r_info[entry->index];

		/* Skip monsters which don't pass the restriction */
		if ((get_mon_num_hook && !((*get_mon_num_hook)(entry->index))) ||
		    (get_mon_num2_hook && !((*get_mon_num2_hook)(entry->index))))
			continue;

		/* Hack -- don't create questors */
		if (r_ptr->flags1 & RF1_QUESTOR)
			continue;

		if (r_ptr->flags7 & RF7_GUARDIAN)
			continue;

		/* Depth Monsters never appear out of depth */
		if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) &&
		    (r_ptr->level > dun_level))
			continue;

		/* Accept this monster */
		entry->prob2 = entry->prob1;

		if (dun_level && (!p_ptr->inside_quest || quest_is_fixed(p_ptr->inside_quest)) && !restrict_monster_to_dungeon(entry->index))
		{
			int hoge = entry->prob2 * d_info[dungeon_type].special_div;
			entry->prob2 = hoge / 64;
			if (randint0(64) < (hoge & 0x3f)) entry->prob2++;
		}
	}

	/* Success */
	return (0);
}


static int mysqrt(int n)
{
	int tmp = n>>1;
	int tasu = 10;
	int kaeriti = 1;

	if (!tmp)
	{
		if (n) return 1;
		else return 0;
	}

	while(tmp)
	{
		if ((n/tmp) < tmp)
		{
			tmp >>= 1;
		}
		else break;
	}
	kaeriti = tmp;
	while(tasu)
	{
		if ((n/tmp) < tmp)
		{
			tasu--;
			tmp = kaeriti;
		}
		else
		{
			kaeriti = tmp;
			tmp += tasu;
		}
	}
	return kaeriti;
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
	int			i, j, p;

	int			r_idx;

	long		value, total;

	monster_race	*r_ptr;

	alloc_entry		*table = alloc_race_table;

	int pls_kakuritu, pls_level;
	int hoge=mysqrt(level*10000L);

	s16b alloc_race_size_2 = alloc_race_size + runeweapon_num;

	if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;

	if ((dungeon_turn > hoge*(TURNS_PER_TICK*500L)) && !level)
	{
		pls_kakuritu = MAX(2, NASTY_MON-((dungeon_turn/(TURNS_PER_TICK*2500L)-hoge/10)));
		pls_level = MIN(8,3 + dungeon_turn/(TURNS_PER_TICK*20000L)-hoge/40);
	}
	else
	{
		pls_kakuritu = NASTY_MON;
		pls_level = 2;
	}

	/* Boost the level */
	if ((level > 0) && !(d_info[dungeon_type].flags1 & DF1_BEGINNER))
	{
		/* Occasional "nasty" monster */
		if (!randint0(pls_kakuritu))
		{
			/* Pick a level bonus */
			int d = MIN(5, level/10) + pls_level;

			/* Boost the level */
			level += d;
		}

		/* Occasional "nasty" monster */
		if (!randint0(pls_kakuritu))
		{
			/* Pick a level bonus */
			int d = MIN(5, level/10) + pls_level;

			/* Boost the level */
			level += d;
		}
	}


	/* Reset total */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_race_size_2; i++)
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
		if (((r_ptr->flags1 & (RF1_UNIQUE)) ||
		     (r_ptr->flags7 & (RF7_NAZGUL))) &&
		    (r_ptr->cur_num >= r_ptr->max_num))
		{
			continue;
		}

		if ((r_ptr->flags7 & (RF7_UNIQUE2)) &&
		    (r_ptr->cur_num >= 1))
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
	value = randint0(total);

	/* Find the monster */
	for (i = 0; i < alloc_race_size_2; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value = value - table[i].prob3;
	}


	/* Power boost */
	p = randint0(100);

	/* Try for a "harder" monster once (50%) or twice (10%) */
	if (p < 60)
	{
		/* Save old */
		j = i;

		/* Pick a monster */
		value = randint0(total);

		/* Find the monster */
		for (i = 0; i < alloc_race_size_2; i++)
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
		value = randint0(total);

		/* Find the monster */
		for (i = 0; i < alloc_race_size_2; i++)
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
 * If no m_ptr arg is given (?), the monster is assumed to be hidden,
 * unless the "Assume Visible" mode is requested.
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
 *  0x200 --> Ignore hallucination, and penetrate shape change
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
	cptr            res;
	monster_race    *r_ptr;

	cptr            name;
	char            buf[128];
	char            silly_name[1024];
	bool            seen, pron;
	bool            named = FALSE;

	r_ptr = &r_info[m_ptr->ap_r_idx];

	name = (r_name + r_ptr->name);

	/* Are we hallucinating? (Idea from Nethack...) */
	if (p_ptr->image && !(mode & MD_IGNORE_HALLU))
	{
		if (one_in_(2))
		{
#ifdef JP
			if (!get_rnd_line("silly_j.txt", m_ptr->r_idx, silly_name))
#else
			if (!get_rnd_line("silly.txt", m_ptr->r_idx, silly_name))
#endif

				named = TRUE;
		}

		if (!named)
		{
			monster_race *hallu_race;

			do
			{
				hallu_race = &r_info[randint1(max_r_idx - 1)];
			}
			while (hallu_race->flags1 & RF1_UNIQUE);

			strcpy(silly_name, (r_name + hallu_race->name));
		}

		/* Better not strcpy it, or we could corrupt r_info... */
		name = silly_name;
	}

	/* Can we "see" it (exists + forced, or visible + not unforced) */
	seen = (m_ptr && ((mode & MD_ASSUME_VISIBLE) || (!(mode & MD_ASSUME_HIDDEN) && m_ptr->ml)));

	/* Sexed Pronouns (seen and allowed, or unseen and allowed) */
	pron = (m_ptr && ((seen && (mode & MD_PRON_VISIBLE)) || (!seen && (mode & MD_PRON_HIDDEN))));


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
#ifdef JP
		res = "何か";
#else
		res = "it";
#endif


		/* Brute force: split on the possibilities */
		switch (kind + (mode & (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE)))
		{
			/* Neuter, or unknown */
#ifdef JP
			case 0x00:                                                    res = "何か"; break;
			case 0x00 + (MD_OBJECTIVE):                                   res = "何か"; break;
			case 0x00 + (MD_POSSESSIVE):                                  res = "何かの"; break;
			case 0x00 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "何か自身"; break;
			case 0x00 + (MD_INDEF_HIDDEN):                                res = "何か"; break;
			case 0x00 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "何か"; break;
			case 0x00 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "何か"; break;
			case 0x00 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "それ自身"; break;
#else
			case 0x00:                                                    res = "it"; break;
			case 0x00 + (MD_OBJECTIVE):                                   res = "it"; break;
			case 0x00 + (MD_POSSESSIVE):                                  res = "its"; break;
			case 0x00 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "itself"; break;
			case 0x00 + (MD_INDEF_HIDDEN):                                res = "something"; break;
			case 0x00 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "something"; break;
			case 0x00 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "something's"; break;
			case 0x00 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "itself"; break;
#endif


			/* Male (assume human if vague) */
#ifdef JP
			case 0x10:                                                    res = "彼"; break;
			case 0x10 + (MD_OBJECTIVE):                                   res = "彼"; break;
			case 0x10 + (MD_POSSESSIVE):                                  res = "彼の"; break;
			case 0x10 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "彼自身"; break;
			case 0x10 + (MD_INDEF_HIDDEN):                                res = "誰か"; break;
			case 0x10 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "誰か"; break;
			case 0x10 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "誰かの"; break;
			case 0x10 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "彼自身"; break;
#else
			case 0x10:                                                    res = "he"; break;
			case 0x10 + (MD_OBJECTIVE):                                   res = "him"; break;
			case 0x10 + (MD_POSSESSIVE):                                  res = "his"; break;
			case 0x10 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "himself"; break;
			case 0x10 + (MD_INDEF_HIDDEN):                                res = "someone"; break;
			case 0x10 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "someone"; break;
			case 0x10 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "someone's"; break;
			case 0x10 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "himself"; break;
#endif


			/* Female (assume human if vague) */
#ifdef JP
			case 0x20:                                                    res = "彼女"; break;
			case 0x20 + (MD_OBJECTIVE):                                   res = "彼女"; break;
			case 0x20 + (MD_POSSESSIVE):                                  res = "彼女の"; break;
			case 0x20 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "彼女自身"; break;
			case 0x20 + (MD_INDEF_HIDDEN):                                res = "誰か"; break;
			case 0x20 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "誰か"; break;
			case 0x20 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "誰かの"; break;
			case 0x20 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "彼女自身"; break;
#else
			case 0x20:                                                    res = "she"; break;
			case 0x20 + (MD_OBJECTIVE):                                   res = "her"; break;
			case 0x20 + (MD_POSSESSIVE):                                  res = "her"; break;
			case 0x20 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "herself"; break;
			case 0x20 + (MD_INDEF_HIDDEN):                                res = "someone"; break;
			case 0x20 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "someone"; break;
			case 0x20 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "someone's"; break;
			case 0x20 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "herself"; break;
#endif

		}

		/* Copy the result */
		(void)strcpy(desc, res);
	}


	/* Handle visible monsters, "reflexive" request */
	else if ((mode & (MD_POSSESSIVE | MD_OBJECTIVE)) == (MD_POSSESSIVE | MD_OBJECTIVE))
	{
		/* The monster is visible, so use its gender */
#ifdef JP
		if (r_ptr->flags1 & (RF1_FEMALE)) strcpy(desc, "彼女自身");
		else if (r_ptr->flags1 & (RF1_MALE)) strcpy(desc, "彼自身");
		else strcpy(desc, "それ自身");
#else
		if (r_ptr->flags1 & RF1_FEMALE) strcpy(desc, "herself");
		else if (r_ptr->flags1 & RF1_MALE) strcpy(desc, "himself");
		else strcpy(desc, "itself");
#endif

	}


	/* Handle all other visible monster requests */
	else
	{
		/* Tanuki? */
		if (is_pet(m_ptr) && m_ptr->ap_r_idx != m_ptr->r_idx)
		{
#ifdef JP
				char *t;
				strcpy(buf, name);
				t = buf;
				while(strncmp(t, "』", 2) && *t) t++;
				if (*t)
				{
					*t = '\0';
					(void)sprintf(desc, "%s？』", buf);
				}
				else
					(void)sprintf(desc, "%s？", name);
#else
				(void)sprintf(desc, "%s?", name);
#endif
		}
		else

		/* It could be a Unique */
		if ((r_ptr->flags1 & RF1_UNIQUE) && !(p_ptr->image && !(mode & MD_IGNORE_HALLU)))
		{
			/* Start with the name (thus nominative and objective) */
			(void)strcpy(desc, name);
		}

		/* It could be an indefinite monster */
		else if (mode & MD_INDEF_VISIBLE)
		{
			/* XXX Check plurality for "some" */

			/* Indefinite monsters need an indefinite article */
#ifdef JP
			(void)strcpy(desc, "");
#else
			(void)strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ");
#endif

			(void)strcat(desc, name);
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			if (is_pet(m_ptr))
#ifdef JP
				(void)strcpy(desc, "あなたの");
#else
				(void)strcpy(desc, "your ");
#endif

			else
#ifdef JP
				(void)strcpy(desc, "");
#else
				(void)strcpy(desc, "the ");
#endif

			(void)strcat(desc, name);
		}

		if (m_ptr->nickname)
		{
#ifdef JP
			sprintf(buf,"「%s」",quark_str(m_ptr->nickname));
#else
			sprintf(buf," called %s",quark_str(m_ptr->nickname));
#endif
			strcat(desc,buf);
		}

		if ((m_ptr->fy == py) && (m_ptr->fx == px))
		{
#ifdef JP
			strcat(desc,"(乗馬中)");
#else
			strcat(desc,"(riding)");
#endif
		}

		if ((mode & MD_IGNORE_HALLU) && m_ptr->ap_r_idx != m_ptr->r_idx)
		{
			strcat(desc, format("(%s)", r_name + r_info[m_ptr->r_idx].name));
		}

		/* Handle the Possessive as a special afterthought */
		if (mode & MD_POSSESSIVE)
		{
			/* XXX Check for trailing "s" */
			
			/* Simply append "apostrophe" and "s" */
#ifdef JP
			(void)strcat(desc, "の");
#else
			(void)strcat(desc, "'s");
#endif

		}
	}
}




/*
 * Learn about a monster (by "probing" it)
 *
 * Return the number of new flags learnt.  -Mogami-
 */
int lore_do_probe(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	int i, n = 0;
	byte tmp_byte;

	/* Maximal info about awareness */
	if (r_ptr->r_wake != MAX_UCHAR) n++;
	if (r_ptr->r_ignore != MAX_UCHAR) n++;
	r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;
				
	/* Observe "maximal" attacks */
	for (i = 0; i < 4; i++)
	{
		/* Examine "actual" blows */
		if (r_ptr->blow[i].effect || r_ptr->blow[i].method)
		{
			/* Maximal observations */
			if (r_ptr->r_blows[i] != MAX_UCHAR) n++;
			r_ptr->r_blows[i] = MAX_UCHAR;
		}
	}
				
	/* Maximal drops */
	tmp_byte = 
		(((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

	/* Only "valid" drops */
	if (!(r_ptr->flags1 & RF1_ONLY_GOLD))
	{
		if (r_ptr->r_drop_item != tmp_byte) n++;
		r_ptr->r_drop_item = tmp_byte;
	}
	if (!(r_ptr->flags1 & RF1_ONLY_ITEM))
	{
		if (r_ptr->r_drop_gold != tmp_byte) n++;
		r_ptr->r_drop_gold = tmp_byte;
	}
				
	/* Observe many spells */
	if (r_ptr->r_cast_spell != MAX_UCHAR) n++;
	r_ptr->r_cast_spell = MAX_UCHAR;
				
	/* Count unknown flags */
	for (i = 0; i < 32; i++)
	{
		if (!(r_ptr->r_flags1 & (1L << i)) &&
		    (r_ptr->flags1 & (1L << i))) n++;
		if (!(r_ptr->r_flags2 & (1L << i)) &&
		    (r_ptr->flags2 & (1L << i))) n++;
		if (!(r_ptr->r_flags3 & (1L << i)) &&
		    (r_ptr->flags3 & (1L << i))) n++;
		if (!(r_ptr->r_flags4 & (1L << i)) &&
		    (r_ptr->flags4 & (1L << i))) n++;
		if (!(r_ptr->r_flags5 & (1L << i)) &&
		    (r_ptr->flags5 & (1L << i))) n++;
		if (!(r_ptr->r_flags6 & (1L << i)) &&
		    (r_ptr->flags6 & (1L << i))) n++;
		if (!(r_ptr->r_flags7 & (1L << i)) &&
		    (r_ptr->flags7 & (1L << i))) n++;
		if (!(r_ptr->r_flagsa & (1L << i)) &&
		    (r_ptr->flagsa & (1L << i))) n++;
		if (!(r_ptr->r_flagsr & (1L << i)) &&
		    (r_ptr->flagsr & (1L << i))) n++;
	}

	/* Hack -- know all the flags */
	r_ptr->r_flags1 = r_ptr->flags1;
	r_ptr->r_flags2 = r_ptr->flags2;
	r_ptr->r_flags3 = r_ptr->flags3;
	r_ptr->r_flags4 = r_ptr->flags4;
	r_ptr->r_flags5 = r_ptr->flags5;
	r_ptr->r_flags6 = r_ptr->flags6;
	r_ptr->r_flags7 = r_ptr->flags7;
	r_ptr->r_flagsa = r_ptr->flagsa;
	r_ptr->r_flagsr = r_ptr->flagsr;

	/* Know about evolution */
	if (!(r_ptr->r_xtra1 & MR1_SINKA)) n++;
	r_ptr->r_xtra1 |= MR1_SINKA;

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}

	/* Return the number of new flags learnt */
	return n;
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
	if (r_ptr->flags1 & (RF1_DROP_GOOD)) r_ptr->r_flags1 |= (RF1_DROP_GOOD);
	if (r_ptr->flags1 & (RF1_DROP_GREAT)) r_ptr->r_flags1 |= (RF1_DROP_GREAT);
	if (r_ptr->flags1 & (RF1_DROP_SPECIAL)) r_ptr->r_flags1 |= (RF1_DROP_SPECIAL);

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

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool do_disturb = disturb_move;

	int d;

	/* Current location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	/* Seen at all */
	bool flag = FALSE;

	/* Seen by vision */
	bool easy = FALSE;

	/* Do disturb? */
	if (disturb_high)
	{
		monster_race *ap_r_ptr = &r_info[m_ptr->ap_r_idx];

		if (ap_r_ptr->r_tkills && ap_r_ptr->level >= p_ptr->lev)
			do_disturb = TRUE;
	}

	/* Compute distance */
	if (full)
	{
		int dy, dx;

		/* Distance components */
		dy = (py > fy) ? (py - fy) : (fy - py);
		dx = (px > fx) ? (px - fx) : (fx - px);

		/* Approximate distance */
		d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

		/* Restrict distance */
		if (d > 255) d = 255;

		if (!d) d = 1;

		/* Save the distance from player */
		m_ptr->cdis = d;

		if (p_ptr->use_decoy && !is_pet(m_ptr))
		{
			int t_py = p_ptr->decoy_y;
			int t_px = p_ptr->decoy_x;
			int dd;

			/* Distance components */
			dy = (t_py > fy) ? (t_py - fy) : (fy - t_py);
			dx = (t_px > fx) ? (t_px - fx) : (fx - t_px);

			/* Approximate distance */
			dd = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

			/* Restrict distance */
			if (dd > 255) dd = 255;

			if (!dd) dd = 1;

			/* Save the distance from decoy */
			m_ptr->ddis = dd;
		}
		else
		{
			m_ptr->ddis = d;
		}
	}

	/* Extract distance */
	else
	{
		/* Extract the distance */
		d = m_ptr->cdis;
	}


	/* Detected */
	if (m_ptr->mflag2 & (MFLAG2_MARK)) flag = TRUE;


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
				r_ptr->r_flags2 |= (RF2_EMPTY_MIND);
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
					r_ptr->r_flags2 |= (RF2_WEIRD_MIND);

					/* Hack -- Memorize mental flags */
					if (r_ptr->flags2 & (RF2_SMART)) r_ptr->r_flags2 |= (RF2_SMART);
					if (r_ptr->flags2 & (RF2_STUPID)) r_ptr->r_flags2 |= (RF2_STUPID);
				}
			}

			/* Normal mind, allow telepathy */
			else
			{
				/* Detectable */
				flag = TRUE;

				/* Hack -- Memorize mental flags */
				if (r_ptr->flags2 & (RF2_SMART)) r_ptr->r_flags2 |= (RF2_SMART);
				if (r_ptr->flags2 & (RF2_STUPID)) r_ptr->r_flags2 |= (RF2_STUPID);
			}
		}

		/* Magical sensing */
		if (p_ptr->esp_dragon && (r_ptr->flags3 & RF3_DRAGON))
		{
			flag = TRUE;
			r_ptr->r_flags3 |= (RF3_DRAGON);
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
				if ((r_ptr->flags2 & RF2_COLD_BLOOD) && !(r_ptr->flags2 & RF2_AURA_FIRE))
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
				if (do_invisible) r_ptr->r_flags2 |= (RF2_INVISIBLE);
				if (do_cold_blood) r_ptr->r_flags2 |= (RF2_COLD_BLOOD);
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
			if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

			/* Hack -- Count "fresh" sightings */
			if (m_ptr->ap_r_idx == m_ptr->r_idx && 
				 r_ptr->r_sights < MAX_SHORT) r_ptr->r_sights++;

			/* Disturb on appearance */
			if (disturb_near && (projectable(m_ptr->fy, m_ptr->fx, py, px) && projectable(py, px, m_ptr->fy, m_ptr->fx)))
			{
				if (disturb_pets || is_hostile(m_ptr))
					disturb(1, 0);
			}
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
			if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

			/* Disturb on disappearance */
			if (do_disturb)
			{
				if (disturb_pets || is_hostile(m_ptr))
					disturb(1, 0);
			}
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
			if (do_disturb)
			{
				if (disturb_pets || is_hostile(m_ptr))
					disturb(1, 0);
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
			if (do_disturb)
			{
				if (disturb_pets || is_hostile(m_ptr))
					disturb(1, 0);
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
 *  Set initial racial appearance of a monster
 */
static int initial_r_appearance(int r_idx)
{
	return r_idx;
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
static bool place_monster_one(int who, int y, int x, int r_idx, u32b mode)
{
	int			i, cmi;

	cave_type		*c_ptr;

	monster_type	*m_ptr;

	monster_race	*r_ptr = &r_info[r_idx];

	cptr		name = (r_name + r_ptr->name);

	/* DO NOT PLACE A MONSTER IN THE SMALL SCALE WILDERNESS !!! */
	if(p_ptr->wild_mode) return FALSE;

	/* Verify location */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space (if not ghostly) */
	if (!(!dun_level && (cave[y][x].feat == FEAT_MOUNTAIN) && ((r_ptr->flags8 & RF8_WILD_MOUNTAIN) || (r_ptr->flags7 & RF7_CAN_FLY))) &&
	    !(cave_empty_bold2(y, x) || (mode & PM_IGNORE_TERRAIN)) &&
	    !((r_ptr->flags2 & RF2_PASS_WALL) &&
	    !(cave_perma_bold(y, x) || cave[y][x].m_idx ||
	    ((y == py) && (x == px))))) return (FALSE);

	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Paranoia */
	if (!r_ptr->name) return (FALSE);

	/* Hack -- no creation on anti-magic field */
	if (!(mode & PM_IGNORE_AMGRID))
	{
		if (is_anti_magic_grid(who, y, x))
		{
			if (!(r_info[m_list[who].r_idx].flags2 & RF2_POWERFUL) || !one_in_(3)) return FALSE;
		}
	}

	if (!(mode & PM_IGNORE_TERRAIN) &&
	    !monster_can_cross_terrain(cave[y][x].feat, r_ptr))
	{
		return FALSE;
	}

	/* Hack -- Don't sleep on the air */
	if ((mode & PM_ALLOW_SLEEP) && r_ptr->sleep &&
		((cave[y][x].feat == FEAT_DARK_PIT) || (cave[y][x].feat == FEAT_AIR)))
	{
		return FALSE;
	}

	/* Hack -- "unique" monsters must be "unique" */
	if (((r_ptr->flags1 & (RF1_UNIQUE)) ||
	     (r_ptr->flags7 & (RF7_NAZGUL))) &&
	    (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
		return (FALSE);
	}

	if ((r_ptr->flags7 & (RF7_UNIQUE2)) &&
	    (r_ptr->cur_num >= 1))
	{
		return (FALSE);
	}

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level < r_ptr->level) &&
	    (r_ptr->flags1 & (RF1_QUESTOR)))
	{
		/* Cannot create */
		return (FALSE);
	}

	if(quest_number(dun_level))
	{
		int hoge = quest_number(dun_level);
		if((quest[hoge].type == QUEST_TYPE_KILL_LEVEL) || (quest[hoge].type == QUEST_TYPE_RANDOM))
		{
			if(r_idx == quest[hoge].r_idx)
			{
				int number_mon, i2, j2;
				number_mon = 0;

				/* Count all quest monsters */
				for (i2 = 0; i2 < cur_wid; ++i2)
					for (j2 = 0; j2 < cur_hgt; j2++)
						if (cave[j2][i2].m_idx > 0)
							if (m_list[cave[j2][i2].m_idx].r_idx == quest[hoge].r_idx)
								number_mon++;
				if(number_mon + quest[hoge].cur_num >= quest[hoge].max_num)
					return FALSE;
			}
		}
	}

	if (p_ptr->use_decoy)
	{
		if ((y == p_ptr->decoy_y) && (x == p_ptr->decoy_x)) break_decoy();
	}

	/* Access the location */
	c_ptr = &cave[y][x];

	if (is_glyph_grid(c_ptr))
	{
		if (randint1(BREAK_GLYPH) < (r_ptr->level+20))
		{
			/* Describe observable breakage */
			if (c_ptr->info & CAVE_MARK)
			{
#ifdef JP
				msg_print("守りのルーンが壊れた！");
#else
				msg_print("The rune of protection is broken!");
#endif

			}

			/* Forget the rune */
			c_ptr->info &= ~(CAVE_MARK);

			/* Break the rune */
			c_ptr->info &= ~(CAVE_OBJECT);
			c_ptr->mimic = 0;

			/* Notice */
			note_spot(y, x);
		}
		else return FALSE;
	}

	/* Runeweapon monster */
	if (monster_is_runeweapon(r_idx))
	{
#ifdef JP
		if (cheat_hear) msg_format("魔武器モンスター (%s)。", name);
#else
		if (cheat_hear) msg_format("Runeweapon monster (%s).", name);
#endif
	}

	/* Powerful monster */
	else if (r_ptr->level > dun_level)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
#ifdef JP
			if (cheat_hear) msg_format("深層のユニーク・モンスター (%s)。", name);
#else
			if (cheat_hear) msg_format("Deep Unique (%s).", name);
#endif
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
#ifdef JP
			if (cheat_hear) msg_format("深層のモンスター (%s)。", name);
#else
			if (cheat_hear) msg_format("Deep Monster (%s).", name);
#endif
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
#ifdef JP
		if (cheat_hear) msg_format("ユニーク・モンスター (%s)。", name);
#else
		if (cheat_hear) msg_format("Unique (%s).", name);
#endif

	}

	/* Make a new monster */
	c_ptr->m_idx = m_pop();
	hack_m_idx_ii = c_ptr->m_idx;

	/* Mega-Hack -- catch "failure" */
	if (!c_ptr->m_idx) return (FALSE);


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;
	m_ptr->ap_r_idx = initial_r_appearance(r_idx);

	m_ptr->sub_align = SUB_ALIGN_NEUTRAL;

	/* Sub-alignment (GNE) of a monster */
	if ((who > 0) && !(r_ptr->flags3 & (RF3_EVIL | RF3_GOOD)))
		m_ptr->sub_align |= (m_list[who].sub_align & SUB_ALIGN_GNE_MASK);
	else
	{
		if (r_ptr->flags3 & RF3_EVIL) m_ptr->sub_align |= SUB_ALIGN_EVIL;
		if (r_ptr->flags3 & RF3_GOOD) m_ptr->sub_align |= SUB_ALIGN_GOOD;
	}

	/* Sub-alignment (LNC) of a monster */
	if ((who > 0) && !(r_ptr->flags7 & (RF7_LAWFUL | RF7_CHAOTIC)))
		m_ptr->sub_align |= (m_list[who].sub_align & SUB_ALIGN_LNC_MASK);
	else
	{
		if (r_ptr->flags7 & RF7_LAWFUL) m_ptr->sub_align |= SUB_ALIGN_LAWFUL;
		if (r_ptr->flags7 & RF7_CHAOTIC) m_ptr->sub_align |= SUB_ALIGN_CHAOTIC;
	}

	/* Sub-classification (Temple or White) */
	if ((who > 0) && !(r_ptr->flags3 & RF3_TEMPLE) && !(r_ptr->flags7 & RF7_ZENOBIAN_FORCES))
		m_ptr->sub_align |= (m_list[who].sub_align & SUB_ALIGN_CLASS_MASK);
	else
	{
		if (r_ptr->flags3 & RF3_TEMPLE) m_ptr->sub_align |= SUB_ALIGN_TEMPLE;
		if (r_ptr->flags7 & RF7_ZENOBIAN_FORCES) m_ptr->sub_align |= SUB_ALIGN_WHITE;
	}

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;

	/* Default elements */
	if (r_ptr->r_elem == NO_ELEM) m_ptr->elem = randint0(ELEM_NUM);
	else m_ptr->elem = r_ptr->r_elem;

	/* No "timed status" yet */
	for (cmi = 0; cmi < MAX_MTIMED; cmi++) m_ptr->mtimed[cmi] = 0;
	m_ptr->silent_song = FALSE;

	/* Unknown distance */
	m_ptr->cdis = 0;
	m_ptr->ddis = 0;

	reset_target(m_ptr);

	m_ptr->nickname = 0;

	m_ptr->exp = 0;

	/* No flags */
	m_ptr->mflag = 0;
	m_ptr->mflag2 = 0;


	/* Your pet summons its pet. */
	if (who > 0 && is_pet(&m_list[who]))
	{
		mode |= PM_FORCE_PET;
		m_ptr->parent_m_idx = who;
	}
	else
	{
		m_ptr->parent_m_idx = 0;
	}

	if (mode & PM_NO_PET) m_ptr->mflag2 |= MFLAG2_NOPET;

	/* Not visible */
	m_ptr->ml = FALSE;

	/* Pet? */
	if (mode & PM_FORCE_PET)
	{
		set_pet(m_ptr);
	}

	/* Friendly? */
	else if ((r_ptr->flags7 & RF7_FRIENDLY) ||
		 (mode & PM_FORCE_FRIENDLY) || is_friendly_idx(who))
	{
		if (!monster_has_hostile_alignment(NULL, r_ptr)) set_friendly(m_ptr);
	}
	else if (pclass_is_(CLASS_MEDIUM) &&
		 (r_ptr->d_char == 'q') &&
		 !(r_ptr->flags3 & RF3_EVIL) &&
		 !(r_ptr->flags1 & RF1_UNIQUE) &&
		 !(r_ptr->flags7 & RF7_NAZGUL) &&
		 !(r_ptr->flags7 & RF7_UNIQUE2))
	{
		/* symbol 'q' monsters are friendly to Medium */
		if (!monster_has_hostile_alignment(NULL, r_ptr)) set_friendly(m_ptr);
	}

	if (!p_ptr->inside_arena)
	{
		if (pclass_is_(CLASS_TEMPLEKNIGHT))
		{
			if ((r_ptr->flags3 & RF3_TEMPLE) && !(r_ptr->flags1 & RF1_QUESTOR))
			{
				if ((who <= 0) && (is_hostile(m_ptr))) set_friendly(m_ptr);
			}
		}
		else if (pclass_is_(CLASS_WHITEKNIGHT))
		{
			if ((r_ptr->flags7 & RF7_ZENOBIAN_FORCES) && !(r_ptr->flags1 & RF1_QUESTOR))
			{
				if ((who <= 0) && (is_hostile(m_ptr))) set_friendly(m_ptr);
			}
		}
	}

	/* Assume no sleeping */
	m_ptr->mtimed[MTIMED_CSLEEP] = 0;

	/* Enforce sleeping if needed */
	if ((mode & PM_ALLOW_SLEEP) && r_ptr->sleep)
	{
		int val = r_ptr->sleep;
		(void)set_monster_csleep(c_ptr->m_idx, (val * 2) + randint1(val * 10));
	}

	/* Assign maximal hitpoints */
	if (r_ptr->flags1 & RF1_FORCE_MAXHP)
	{
		m_ptr->max_maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		m_ptr->max_maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* あとでホワイト／テンプルの調整 */

	m_ptr->max_maxhp = MIN(MAX_MAX_MAXHP, m_ptr->max_maxhp);
	m_ptr->maxhp = m_ptr->max_maxhp;
	if (m_ptr->maxhp < 1) m_ptr->maxhp = 1;

	/* And start out fully healthy */
	if (m_ptr->r_idx == MON_WOUNDED_BEAR)
		m_ptr->hp = m_ptr->maxhp / 2;
	else m_ptr->hp = m_ptr->maxhp;


	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;

	/* Hack -- small racial variety */
	if (!(r_ptr->flags1 & RF1_UNIQUE) && !p_ptr->inside_arena)
	{
		/* Allow some small variation per monster */
	  if(one_in_(4)){
		i = SPEED_TO_ENERGY(r_ptr->speed) / 3;
		if (i) m_ptr->mspeed += rand_spread(0, i);
	  }
	  else{
		i = SPEED_TO_ENERGY(r_ptr->speed) / 10;
		if (i) m_ptr->mspeed += rand_spread(0, i);
	  }
	}

	if (mode & PM_HASTE) (void)set_monster_fast(c_ptr->m_idx, 100);

	if (m_ptr->mspeed > 199) m_ptr->mspeed = 199;

	/* Give a random starting energy */
	m_ptr->energy_need = ENERGY_NEED() - (s16b)randint0(100);

	/* Force monster to wait for player */
	if ((r_ptr->flags1 & RF1_FORCE_SLEEP) || (d_info[dungeon_type].flags1 & DF1_VAULT))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_monsters = TRUE;
	}

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_BORN);
	}


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Count the monsters on the level */
	real_r_ptr(m_ptr)->cur_num++;

	/*
	 * Memorize location of the unique monster in saved floors.
	 * A unique monster move from old saved floor.
	 */
	if (character_dungeon &&
	    ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL)))
		real_r_ptr(m_ptr)->floor_id = p_ptr->floor_id;

	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & RF2_MULTIPLY) num_repro++;


	/* Hack -- Count the number of "anti-magic monsters" */
	if (r_ptr->flags3 & RF3_ANTI_MAGIC)
		anti_magic_m_idx[num_anti_magic++] = c_ptr->m_idx;


	/* Hack -- Count the number of "fear field monsters" */
	if (r_ptr->flags3 & RF3_FEAR_FIELD)
		fear_field_m_idx[num_fear_field++] = c_ptr->m_idx;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & RF1_ATTR_MULTI) shimmer_monsters = TRUE;

	if (p_ptr->warning && character_dungeon)
	{
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			cptr color;
			object_type *o_ptr;
			char o_name[MAX_NLEN];

			if (r_ptr->level > p_ptr->lev + 30)
#ifdef JP
				color = "黒く";
#else
				color = "black";
#endif
			else if (r_ptr->level > p_ptr->lev + 15)
#ifdef JP
				color = "紫色に";
#else
				color = "purple";
#endif
			else if (r_ptr->level > p_ptr->lev + 5)
#ifdef JP
				color = "ルビー色に";
#else
				color = "deep red";
#endif
			else if (r_ptr->level > p_ptr->lev - 5)
#ifdef JP
				color = "赤く";
#else
				color = "red";
#endif
			else if (r_ptr->level > p_ptr->lev - 15)
#ifdef JP
				color = "ピンク色に";
#else
				color = "pink";
#endif
			else
#ifdef JP
				color = "白く";
#else
				color = "white";
#endif

			o_ptr = choose_warning_item();
			object_desc(o_name, o_ptr, (OD_NAME_ONLY | OD_STORE));
#ifdef JP
			msg_format("%sは%s光った。",o_name, color);
#else
			msg_format("%s glows %s.",o_name, color);
#endif
		}
	}

	if (is_explosive_rune_grid(c_ptr))
	{
		/* Break the ward */
		if (randint1(BREAK_MINOR_GLYPH) > r_ptr->level)
		{
			/* Describe observable breakage */
			if (c_ptr->info & CAVE_MARK)
			{
#ifdef JP
				msg_print("ルーンが爆発した！");
#else
				msg_print("The rune explodes!");
#endif

				project(0, 2, y, x, 2 * (c_ptr->special + damroll(7, 7)), GF_MANA, (PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP | PROJECT_NO_HANGEKI), MODIFY_ELEM_MODE_NONE);
			}
		}
		else
		{
#ifdef JP
			msg_print("爆発のルーンは解除された。");
#else
			msg_print("An explosive rune was disarmed.");
#endif
		}

		/* Forget the rune */
		c_ptr->info &= ~(CAVE_MARK);

		/* Break the rune */
		c_ptr->info &= ~(CAVE_OBJECT);
		c_ptr->mimic = 0;
		c_ptr->special = 0;

		note_spot(y, x);
		lite_spot(y, x);
	}

	/* Success */
	return (TRUE);
}


/*
 *  improved version of scatter() for place monster
 */

#define MON_SCAT_MAXD 10

static bool mon_scatter(int *yp, int *xp, int y, int x, int max_dist)
{
	int place_x[MON_SCAT_MAXD];
	int place_y[MON_SCAT_MAXD];
	int num[MON_SCAT_MAXD];
	int i;
	int nx, ny;

	if (max_dist >= MON_SCAT_MAXD)
		return FALSE;

	for (i = 0; i < MON_SCAT_MAXD; i++)
		num[i] = 0;

	for (nx = x - max_dist; nx <= x + max_dist; nx++)
		for (ny = y - max_dist; ny <= y + max_dist; ny++)
		{
			/* Ignore annoying locations */
			if (!in_bounds(ny, nx)) continue;

			/* Require "line of sight" */
			if (!los(y, x, ny, nx)) continue;

			/* Walls and Monsters block flow */
			if (!cave_empty_bold2(ny, nx)) continue;

			i = distance(y, x, ny, nx);

			if (i > max_dist)
				continue;

			num[i]++;

			/* random swap */
			if(one_in_(num[i]))
			{
				place_x[i] = nx;
				place_y[i] = ny;
			}
		}

	i = 0;
	while (i < MON_SCAT_MAXD && 0 == num[i])
		i++;
	if (i >= MON_SCAT_MAXD)
		return FALSE;

	*xp = place_x[i];
	*yp = place_y[i];

	return TRUE;
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX	32


/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int who, int y, int x, int r_idx, u32b mode)
{
	monster_race *r_ptr = &r_info[r_idx];

	int n, i;
	int total = 0, extra = 0;

	int hack_n = 0;

	byte hack_y[GROUP_MAX];
	byte hack_x[GROUP_MAX];


	/* Pick a group size */
	total = randint1(10);

	/* Hard monsters, small groups */
	if (r_ptr->level > dun_level)
	{
		extra = r_ptr->level - dun_level;
		extra = 0 - randint1(extra);
	}

	/* Easy monsters, large groups */
	else if (r_ptr->level < dun_level)
	{
		extra = dun_level - r_ptr->level;
		extra = randint1(extra);
	}

	/* Hack -- limit group reduction */
	if (extra > 9) extra = 9;

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

			scatter(&my, &mx, hy, hx, 4, 0);

			/* Walls and Monsters block flow */
			if (!cave_empty_bold2(my, mx)) continue;

			/* Attempt to place another monster */
			if (place_monster_one(who, my, mx, r_idx, mode))
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
static int place_monster_m_idx = 0;

/*
 * Hack -- help pick an escort type
 */
static bool place_monster_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[place_monster_idx];
	monster_type *m_ptr = &m_list[place_monster_m_idx];

	monster_race *z_ptr = &r_info[r_idx];

	/* Hack - Escorts have to have the same dungeon flag */
	if (mon_hook_dungeon(place_monster_idx) != mon_hook_dungeon(r_idx)) return (FALSE);

	/* Require specified "race" */
	if ((z_ptr->d_char != r_ptr->escort_char[0]) &&
	    (z_ptr->d_char != r_ptr->escort_char[1]) &&
	    (z_ptr->d_char != r_ptr->escort_char[2]) &&
	    (z_ptr->d_char != r_ptr->escort_char[3]))
		return (FALSE);

	/* Skip more advanced monsters */
	if (z_ptr->level > r_ptr->level) return (FALSE);

	/* Skip unique monsters */
	if (z_ptr->flags1 & RF1_UNIQUE) return (FALSE);

	/* This monster never be "escort" */
	if (z_ptr->flags1 & RF1_NO_ESCORT) return (FALSE);

	/* Paranoia -- Skip identical monsters */
	if (place_monster_idx == r_idx) return (FALSE);

	/* Escort of neutral (GNE) leader is only neutral (GNE) */
	if (!(r_ptr->flags3 & (RF3_EVIL | RF3_GOOD)))
	{
		if (z_ptr->flags3 & (RF3_EVIL | RF3_GOOD)) return FALSE;
	}

	/* Skip different alignment */
	if (monster_has_hostile_alignment(m_ptr, z_ptr))
		return FALSE;

	if (r_ptr->flags7 & RF7_FRIENDLY)
	{
		if (monster_has_hostile_alignment(NULL, z_ptr)) return FALSE;
	}

	/* Decline Temple Knights */
	if (!(r_ptr->flags3 & RF3_TEMPLE) && (z_ptr->flags3 & RF3_TEMPLE)) return FALSE;

	/* Decline Zenobian Forces */
	if (!(r_ptr->flags7 & RF7_ZENOBIAN_FORCES) && (z_ptr->flags7 & RF7_ZENOBIAN_FORCES)) return FALSE;

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
bool place_monster_aux(int who, int y, int x, int r_idx, u32b mode)
{
	int             i;
	monster_race    *r_ptr = &r_info[r_idx];

	/* Place one monster, or fail */
	if (!place_monster_one(who, y, x, r_idx, mode)) return (FALSE);


	/* Require the "group" flag */
	if (!(mode & PM_ALLOW_GROUP)) return (TRUE);

	place_monster_m_idx = hack_m_idx_ii;

	/* Friends for certain monsters */
	if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		/* Attempt to place a group */
		(void)place_monster_group(who, y, x, r_idx, mode);
	}


	/* Escorts for certain monsters */
	if (r_ptr->flags1 & (RF1_ESCORT))
	{
		/* Set the escort index */
		place_monster_idx = r_idx;

		/* Try to place several "escorts" */
		for (i = 0; i < 32; i++)
		{
			int nx, ny, z, d = 3;

			/* Pick a location */
			scatter(&ny, &nx, y, x, d, 0);

			/* Require empty grids */
			if (!cave_empty_bold2(ny, nx)) continue;

			/* Prepare allocation table */
			get_mon_num_prep(place_monster_okay, get_monster_hook2(ny, nx));

			/* Pick a random race */
			z = get_mon_num(r_ptr->level);

			/* Handle failure */
			if (!z) break;

			/* Place a single escort */
			(void)place_monster_one(place_monster_m_idx, ny, nx, z, mode);

			/* Place a "group" of escorts if needed */
			if ((r_info[z].flags1 & RF1_FRIENDS) ||
			    (r_ptr->flags1 & RF1_ESCORTS))
			{
				/* Place a group of monsters */
				(void)place_monster_group(place_monster_m_idx, ny, nx, z, mode);
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
bool place_monster(int y, int x, u32b mode)
{
	int r_idx;

	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), get_monster_hook2(y, x));

	/* Pick a monster */
	r_idx = get_mon_num(monster_level);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	if (place_monster_aux(0, y, x, r_idx, mode)) return (TRUE);

	/* Oops */
	return (FALSE);
}


#ifdef MONSTER_HORDES

bool alloc_horde(int y, int x)
{
	monster_race *r_ptr = NULL;
	int r_idx = 0;
	int m_idx;
	int attempts = 1000;
	int cy = y;
	int cx = x;

	/* Prepare allocation table */
	get_mon_num_prep(get_monster_hook(), get_monster_hook2(y, x));

	while (--attempts)
	{
		/* Pick a monster */
		r_idx = get_mon_num(monster_level);

		/* Handle failure */
		if (!r_idx) return (FALSE);

		r_ptr = &r_info[r_idx];

		if (r_ptr->flags1 & RF1_UNIQUE) continue;

		break;
	}
	if (attempts < 1) return FALSE;

	attempts = 1000;

	while (--attempts)
	{
		/* Attempt to place the monster */
		if (place_monster_aux(0, y, x, r_idx, PM_ALLOW_GROUP | PM_IGNORE_AMGRID)) break;
	}

	if (attempts < 1) return FALSE;

	m_idx = cave[y][x].m_idx;

	summon_kin_type = r_ptr->d_char;

	for (attempts = randint1(10) + 5; attempts; attempts--)
	{
		scatter(&cy, &cx, y, x, 5, 0);

		(void)summon_specific(m_idx, cy, cx, dun_level + 5, SUMMON_KIN, PM_ALLOW_GROUP | PM_IGNORE_AMGRID);

		y = cy;
		x = cx;
	}

	return TRUE;
}

#endif /* MONSTER_HORDES */


/*
 * Put an Guardian
 */
bool alloc_guardian(u32b mode)
{
	int guardian = d_info[dungeon_type].final_guardian;

	if (guardian && (d_info[dungeon_type].maxdepth == dun_level) && (r_info[guardian].cur_num < r_info[guardian].max_num))
	{
		int oy;
		int ox;
		int try = 4000;

		if (mode & PM_IN_GENERATE)
		{
			int num;

			/* Is pet? */
			for (num = 0; num < 21; num++)
			{
				if (party_mon[num].r_idx == guardian) return FALSE;
			}
		}

		/* Find a good position */
		while (try)
		{
			/* Get a random spot */
			oy = randint1(cur_hgt - 4) + 2;
			ox = randint1(cur_wid - 4) + 2;

			/* Is it a good spot ? */
			if (cave_empty_bold2(oy, ox) && monster_can_cross_terrain(cave[oy][ox].feat, &r_info[guardian]))
			{
				/* Place the guardian */
				if (place_monster_aux(0, oy, ox, guardian, (PM_ALLOW_GROUP | PM_NO_PET | PM_IGNORE_AMGRID))) return TRUE;
			}
			/* One less try */
			try--;
		}
	}

	return FALSE;
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
bool alloc_monster(int dis, u32b mode)
{
	int			y = 0, x = 0;
	int         attempts_left = 10000;

	/* Put an Guardian */
	if (alloc_guardian(mode)) return TRUE;

	/* Find a legal, distant, unoccupied, space */
	while (attempts_left--)
	{
		/* Pick a location */
		y = randint0(cur_hgt);
		x = randint0(cur_wid);

		/* Require empty floor grid (was "naked") */
		if (dun_level || (wilderness[p_ptr->wilderness_y][p_ptr->wilderness_x].terrain != TERRAIN_MOUNTAIN))
		{
			if (!cave_empty_bold2(y, x)) continue;
		}
		else
		{
			if (!cave_empty_bold2(y, x) && (cave[y][x].feat != FEAT_MOUNTAIN)) continue;
		}

		/* Accept far away grids */
		if (distance(y, x, py, px) > dis) break;
	}

	if (!attempts_left)
	{
		if (cheat_xtra || cheat_hear)
		{
#ifdef JP
			msg_print("警告！新たなモンスターを配置できません。小さい階ですか？");
#else
			msg_print("Warning! Could not allocate a new monster. Small level?");
#endif

		}

		return (FALSE);
	}


#ifdef MONSTER_HORDES
	if (randint1(5000) <= dun_level)
	{
		if (alloc_horde(y, x))
		{
#ifdef JP
			if (cheat_hear) msg_print("モンスターの大群");
#else
			if (cheat_hear) msg_print("Monster horde.");
#endif

			return (TRUE);
		}
	}
	else
	{
#endif /* MONSTER_HORDES */

		/* Attempt to place the monster, allow groups */
		if (place_monster(y, x, (mode | PM_ALLOW_GROUP | PM_IGNORE_AMGRID))) return (TRUE);

#ifdef MONSTER_HORDES
	}
#endif /* MONSTER_HORDES */

	/* Nope */
	return (FALSE);
}




/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Hack - Only summon dungeon monsters */
	if (!mon_hook_dungeon(r_idx)) return (FALSE);

	/* Hack -- identify the summoning monster */
	if (summon_specific_who > 0)
	{
		monster_type *m_ptr = &m_list[summon_specific_who];

		if ((summon_specific_type != SUMMON_TEMPLES) && (r_ptr->flags3 & RF3_TEMPLE)) return FALSE;
		if ((summon_specific_type != SUMMON_ZENOBIAN_FORCES) && (r_ptr->flags7 & RF7_ZENOBIAN_FORCES)) return FALSE;

		/* Do not summon enemies */

		/* Friendly vs. opposite aligned normal or pet */
		if (monster_has_hostile_alignment(m_ptr, r_ptr))
		{
			return FALSE;
		}
	}
	/* Use the player's alignment */
	else if (summon_specific_who < 0)
	{
		/* Do not summon enemies of the pets */
		if (monster_has_hostile_alignment(NULL, r_ptr)) return FALSE;
	}

	/* Hack -- no specific type specified */
	if (!summon_specific_type) return (TRUE);

	if (!summon_unique_okay && ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL))) return FALSE;

	return (summon_specific_aux(r_idx));
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_TEMPLES will summon Unique's
 * Note: SUMMON_HI_DEMON, SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Unique's
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
bool summon_specific(int who, int y1, int x1, int lev, int type, u32b mode)
{
	int x, y, r_idx;

	if (p_ptr->inside_arena) return (FALSE);

	if (!mon_scatter(&y, &x, y1, x1, 2)) return FALSE;

	/* Save the summoner */
	summon_specific_who = who;

	/* Save the "summon" type */
	summon_specific_type = type;

	summon_unique_okay = (mode & PM_ALLOW_UNIQUE) ? TRUE : FALSE;

	/* Prepare allocation table */
	get_mon_num_prep(summon_specific_okay, get_monster_hook2(y, x));

	/* Pick a monster, using the level calculation */
	r_idx = get_mon_num((dun_level + lev) / 2 + 5);

	/* Handle failure */
	if (!r_idx)
	{
		summon_specific_type = 0;
		return (FALSE);
	}

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(who, y, x, r_idx, mode))
	{
		summon_specific_type = 0;
		return (FALSE);
	}

	summon_specific_type = 0;
	/* Success */
	return (TRUE);
}

/* A "dangerous" function, creates a pet of the specified type */
bool summon_named_creature(int who, int oy, int ox, int r_idx, u32b mode)
{
	int x, y;

	/* Paranoia */
	/* if (!r_idx) return; */

	/* Prevent illegal monsters */
	if ((r_idx >= max_r_idx) && !monster_is_runeweapon(r_idx)) return FALSE;

	if (p_ptr->inside_arena) return FALSE;

	if (!mon_scatter(&y, &x, oy, ox, 2)) return FALSE;

	/* Place it (allow groups) */
	return place_monster_aux(who, y, x, r_idx, mode);
}


/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
bool multiply_monster(int m_idx, bool clone, u32b mode)
{
	monster_type	*m_ptr = &m_list[m_idx];

	int y, x;

	if (!mon_scatter(&y, &x, m_ptr->fy, m_ptr->fx, 1))
		return FALSE;

	if (m_ptr->mflag2 & MFLAG2_NOPET) mode |= PM_NO_PET;

	/* Create a new monster (awake, no groups) */
	if (!place_monster_aux(m_idx, y, x, m_ptr->r_idx, mode))
		return FALSE;

	if (clone)
	{
		m_list[hack_m_idx_ii].smart1 |= SM1_CLONED;
		m_list[hack_m_idx_ii].mflag2 |= MFLAG2_NOPET;
	}

	m_list[hack_m_idx_ii].elem = m_ptr->elem;

	return TRUE;
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
#ifdef JP
		msg_format("%^sはダメージを受けていない。", m_name);
#else
		msg_format("%^s is unharmed.", m_name);
#endif

		return;
	}

	/* Note -- subtle fix -CFT */
	newhp = (long)(m_ptr->hp);
	oldhp = newhp + (long)(dam);
	tmp = (newhp * 100L) / oldhp;
	percentage = (int)(tmp);


	/* Mushrooms, Eyes, Jellies, Molds, Vortices, Worms, Quylthulgs */
	if (my_strchr(",ejmvwQ", r_ptr->d_char))
	{
#ifdef JP
		if (percentage > 95)
			msg_format("%^sはほとんど気にとめていない。", m_name);
		else if (percentage > 75)
			msg_format("%^sはしり込みした。", m_name);
		else if (percentage > 50)
			msg_format("%^sは縮こまった。", m_name);
		else if (percentage > 35)
			msg_format("%^sは痛みに震えた。", m_name);
		else if (percentage > 20)
			msg_format("%^sは身もだえした。", m_name);
		else if (percentage > 10)
			msg_format("%^sは苦痛で身もだえした。", m_name);
		else
			msg_format("%^sはぐにゃぐにゃと痙攣した。", m_name);
#else
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
#endif

	}


	/* Fish */
	else if (my_strchr("l", r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%^sはほとんど気にとめていない。", m_name);
#else
			msg_format("%^s barely notices.", m_name);
#endif
		else if (percentage > 75)
#ifdef JP
			msg_format("%^sはしり込みした。", m_name);
#else
			msg_format("%^s flinches.", m_name);
#endif
		else if (percentage > 50)
#ifdef JP
			msg_format("%^sは躊躇した。", m_name);
#else
			msg_format("%^s hesitates.", m_name);
#endif
		else if (percentage > 35)
#ifdef JP
			msg_format("%^sは痛みに震えた。", m_name);
#else
			msg_format("%^s quivers in pain.", m_name);
#endif
		else if (percentage > 20)
#ifdef JP
			msg_format("%^sは身もだえした。", m_name);
#else
			msg_format("%^s writhes about.", m_name);
#endif
		else if (percentage > 10)
#ifdef JP
			msg_format("%^sは苦痛で身もだえした。", m_name);
#else
			msg_format("%^s writhes in agony.", m_name);
#endif
		else
#ifdef JP
			msg_format("%^sはぐにゃぐにゃと痙攣した。", m_name);
#else
			msg_format("%^s jerks limply.", m_name);
#endif
	}


	/* Golems, Walls, Doors, Stairs */
	else if (my_strchr("g#+<>", r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%sは攻撃を気にとめていない。", m_name);
#else
			msg_format("%^s ignores the attack.", m_name);
#endif
		else if (percentage > 75)
#ifdef JP
			msg_format("%sは攻撃に肩をすくめた。", m_name);
#else
			msg_format("%^s shrugs off the attack.", m_name);
#endif
		else if (percentage > 50)
#ifdef JP
			msg_format("%^sは雷鳴のように吠えた。", m_name);
#else
			msg_format("%^s roars thunderously.", m_name);
#endif
		else if (percentage > 35)
#ifdef JP
			msg_format("%^sは苦しげに吠えた。", m_name);
#else
			msg_format("%^s rumbles.", m_name);
#endif
		else if (percentage > 20)
#ifdef JP
			msg_format("%^sはうめいた。", m_name);
#else
			msg_format("%^s grunts.", m_name);
#endif
		else if (percentage > 10)
#ifdef JP
			msg_format("%^sは躊躇した。", m_name);
#else
			msg_format("%^s hesitates.", m_name);
#endif
		else
#ifdef JP
			msg_format("%^sはくしゃくしゃになった。", m_name);
#else
			msg_format("%^s crumples.", m_name);
#endif
	}


	/* Snakes, Hydrae, Reptiles, Mimics */
	else if (my_strchr("JMR", r_ptr->d_char) || !isalpha(r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%^sはほとんど気にとめていない。", m_name);
#else
			msg_format("%^s barely notices.", m_name);
#endif
		else if (percentage > 75)
#ifdef JP
			msg_format("%^sはシーッと鳴いた。", m_name);
#else
			msg_format("%^s hisses.", m_name);
#endif
		else if (percentage > 50)
#ifdef JP
			msg_format("%^sは怒って頭を上げた。", m_name);
#else
			msg_format("%^s rears up in anger.", m_name);
#endif
		else if (percentage > 35)
#ifdef JP
			msg_format("%^sは猛然と威嚇した。", m_name);
#else
			msg_format("%^s hisses furiously.", m_name);
#endif
		else if (percentage > 20)
#ifdef JP
			msg_format("%^sは身もだえした。", m_name);
#else
			msg_format("%^s writhes about.", m_name);
#endif
		else if (percentage > 10)
#ifdef JP
			msg_format("%^sは苦痛で身もだえした。", m_name);
#else
			msg_format("%^s writhes in agony.", m_name);
#endif
		else
#ifdef JP
			msg_format("%^sはぐにゃぐにゃと痙攣した。", m_name);
#else
			msg_format("%^s jerks limply.", m_name);
#endif
	}


	/* Felines */
	else if (my_strchr("f", r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%sは攻撃に肩をすくめた。", m_name);
#else
			msg_format("%^s shrugs off the attack.", m_name);
#endif
		else if (percentage > 75)
#ifdef JP
			msg_format("%^sは吠えた。", m_name);
#else
			msg_format("%^s roars.", m_name);
#endif
		else if (percentage > 50)
#ifdef JP
			msg_format("%^sは怒って吠えた。", m_name);
#else
			msg_format("%^s growls angrily.", m_name);
#endif
		else if (percentage > 35)
#ifdef JP
			msg_format("%^sは痛みでシーッと鳴いた。", m_name);
#else
			msg_format("%^s hisses with pain.", m_name);
#endif
		else if (percentage > 20)
#ifdef JP
			msg_format("%^sは痛みで弱々しく鳴いた。", m_name);
#else
			msg_format("%^s mewls in pain.", m_name);
#endif
		else if (percentage > 10)
#ifdef JP
			msg_format("%^sは苦痛にうめいた。", m_name);
#else
			msg_format("%^s hisses in agony.", m_name);
#endif
		else
#ifdef JP
			msg_format("%sは哀れな鳴き声を出した。", m_name);
#else
			msg_format("%^s mewls pitifully.", m_name);
#endif
	}


	/* Ants, Centipedes, Flies, Insects, Beetles, Spiders */
	else if (my_strchr("acIKS", r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%sは攻撃を気にとめていない。", m_name);
#else
			msg_format("%^s ignores the attack.", m_name);
#endif
		else if (percentage > 75)
#ifdef JP
			msg_format("%^sはキーキー鳴いた。", m_name);
#else
			msg_format("%^s chitters.", m_name);
#endif

		else if (percentage > 50)
#ifdef JP
			msg_format("%^sはヨロヨロ逃げ回った。", m_name);
#else
			msg_format("%^s scuttles about.", m_name);
#endif

		else if (percentage > 35)
#ifdef JP
			msg_format("%^sはうるさく鳴いた。", m_name);
#else
			msg_format("%^s twitters.", m_name);
#endif

		else if (percentage > 20)
#ifdef JP
			msg_format("%^sは痛みに痙攣した。", m_name);
#else
			msg_format("%^s jerks in pain.", m_name);
#endif

		else if (percentage > 10)
#ifdef JP
			msg_format("%^sは苦痛で痙攣した。", m_name);
#else
			msg_format("%^s jerks in agony.", m_name);
#endif

		else
#ifdef JP
			msg_format("%^sはピクピクひきつった。", m_name);
#else
			msg_format("%^s twitches.", m_name);
#endif

	}


	/* Birds */
	else if (my_strchr("B", r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%^sはさえずった。", m_name);
#else
			msg_format("%^s chirps.", m_name);
#endif

		else if (percentage > 75)
#ifdef JP
			msg_format("%^sはピーピー鳴いた。", m_name);
#else
			msg_format("%^s twitters.", m_name);
#endif

		else if (percentage > 50)
#ifdef JP
			msg_format("%^sはギャーギャー鳴いた。", m_name);
#else
			msg_format("%^s squawks.", m_name);
#endif

		else if (percentage > 35)
#ifdef JP
			msg_format("%^sはギャーギャー鳴きわめいた。", m_name);
#else
			msg_format("%^s chatters.", m_name);
#endif

		else if (percentage > 20)
#ifdef JP
			msg_format("%^sは苦しんだ。", m_name);
#else
			msg_format("%^s jeers.", m_name);
#endif

		else if (percentage > 10)
#ifdef JP
			msg_format("%^sはのたうち回った。", m_name);
#else
			msg_format("%^s flutters about.", m_name);
#endif

		else
#ifdef JP
			msg_format("%^sはキーキーと鳴き叫んだ。", m_name);
#else
			msg_format("%^s squeaks.", m_name);
#endif

	}


	/* Dragons, Demons, High Undead */
	else if (my_strchr("duDLUW", r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%sは攻撃を気にとめていない。", m_name);
#else
			msg_format("%^s ignores the attack.", m_name);
#endif

		else if (percentage > 75)
#ifdef JP
			msg_format("%^sはしり込みした。", m_name);
#else
			msg_format("%^s flinches.", m_name);
#endif

		else if (percentage > 50)
#ifdef JP
			msg_format("%^sは痛みでシーッと鳴いた。", m_name);
#else
			msg_format("%^s hisses in pain.", m_name);
#endif

		else if (percentage > 35)
#ifdef JP
			msg_format("%^sは痛みでうなった。", m_name);
#else
			msg_format("%^s snarls with pain.", m_name);
#endif

		else if (percentage > 20)
#ifdef JP
			msg_format("%^sは痛みに吠えた。", m_name);
#else
			msg_format("%^s roars with pain.", m_name);
#endif

		else if (percentage > 10)
#ifdef JP
			msg_format("%^sは苦しげに叫んだ。", m_name);
#else
			msg_format("%^s gasps.", m_name);
#endif

		else
#ifdef JP
			msg_format("%^sは弱々しくうなった。", m_name);
#else
			msg_format("%^s snarls feebly.", m_name);
#endif

	}


	/* Skeletons */
	else if (my_strchr("s", r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%sは攻撃を気にとめていない。", m_name);
#else
			msg_format("%^s ignores the attack.", m_name);
#endif

		else if (percentage > 75)
#ifdef JP
			msg_format("%sは攻撃に肩をすくめた。", m_name);
#else
			msg_format("%^s shrugs off the attack.", m_name);
#endif

		else if (percentage > 50)
#ifdef JP
			msg_format("%^sはカタカタと笑った。", m_name);
#else
			msg_format("%^s rattles.", m_name);
#endif

		else if (percentage > 35)
#ifdef JP
			msg_format("%^sはよろめいた。", m_name);
#else
			msg_format("%^s stumbles.", m_name);
#endif

		else if (percentage > 20)
#ifdef JP
			msg_format("%^sはカタカタ言った。", m_name);
#else
			msg_format("%^s rattles.", m_name);
#endif

		else if (percentage > 10)
#ifdef JP
			msg_format("%^sはよろめいた。", m_name);
#else
			msg_format("%^s staggers.", m_name);
#endif

		else
#ifdef JP
			msg_format("%^sはガタガタ言った。", m_name);
#else
			msg_format("%^s clatters.", m_name);
#endif

	}


	/* Zombies */
	else if (my_strchr("z", r_ptr->d_char))
	{
		if (percentage > 95)
#ifdef JP
			msg_format("%sは攻撃を気にとめていない。", m_name);
#else
			msg_format("%^s ignores the attack.", m_name);
#endif

		else if (percentage > 75)
#ifdef JP
			msg_format("%sは攻撃に肩をすくめた。", m_name);
#else
			msg_format("%^s shrugs off the attack.", m_name);
#endif

		else if (percentage > 50)
#ifdef JP
			msg_format("%^sはうめいた。", m_name);
#else
			msg_format("%^s groans.", m_name);
#endif

		else if (percentage > 35)
#ifdef JP
			msg_format("%sは苦しげにうめいた。", m_name);
#else
			msg_format("%^s moans.", m_name);
#endif

		else if (percentage > 20)
#ifdef JP
			msg_format("%^sは躊躇した。", m_name);
#else
			msg_format("%^s hesitates.", m_name);
#endif

		else if (percentage > 10)
#ifdef JP
			msg_format("%^sはうなった。", m_name);
#else
			msg_format("%^s grunts.", m_name);
#endif

		else
#ifdef JP
			msg_format("%^sはよろめいた。", m_name);
#else
			msg_format("%^s staggers.", m_name);
#endif

	}


	/* Ghosts */
	else if (my_strchr("G", r_ptr->d_char))

	{
		if (percentage > 95)
#ifdef JP
			msg_format("%sは攻撃を気にとめていない。", m_name);
#else
			msg_format("%^s ignores the attack.", m_name);
#endif

		else if (percentage > 75)
#ifdef JP
			msg_format("%sは攻撃に肩をすくめた。", m_name);
#else
			msg_format("%^s shrugs off the attack.", m_name);
#endif

		else if (percentage > 50)
#ifdef JP
			msg_format("%sはうめいた。", m_name);
#else
			msg_format("%^s moans.", m_name);
#endif

		else if (percentage > 35)
#ifdef JP
			msg_format("%^sは泣きわめいた。", m_name);
#else
			msg_format("%^s wails.", m_name);
#endif

		else if (percentage > 20)
#ifdef JP
			msg_format("%^sは吠えた。", m_name);
#else
			msg_format("%^s howls.", m_name);
#endif

		else if (percentage > 10)
#ifdef JP
			msg_format("%sは弱々しくうめいた。", m_name);
#else
			msg_format("%^s moans softly.", m_name);
#endif

		else
#ifdef JP
			msg_format("%^sはかすかにうめいた。", m_name);
#else
			msg_format("%^s sighs.", m_name);
#endif

	}


	/* Dogs and Hounds */
	else if (my_strchr("CZ", r_ptr->d_char))
	{
#ifdef JP
		if (percentage > 95)
			msg_format("%^sは攻撃に肩をすくめた。", m_name);
		else if (percentage > 75)
			msg_format("%^sは痛みでうなった。", m_name);
		else if (percentage > 50)
			msg_format("%^sは痛みでキャンキャン吠えた。", m_name);
		else if (percentage > 35)
			msg_format("%^sは痛みで鳴きわめいた。", m_name);
		else if (percentage > 20)
			msg_format("%^sは苦痛のあまり鳴きわめいた。", m_name);
		else if (percentage > 10)
			msg_format("%^sは苦痛でもだえ苦しんだ。", m_name);
		else
			msg_format("%^sは弱々しく吠えた。", m_name);
#else
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
#endif

	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (my_strchr("Xbilqrt", r_ptr->d_char))
	{
#ifdef JP
		if (percentage > 95)
			msg_format("%^sは攻撃を気にとめていない。", m_name);
		else if (percentage > 75)
			msg_format("%^sは痛みでうなった。", m_name);
		else if (percentage > 50)
			msg_format("%^sは痛みで叫んだ。", m_name);
		else if (percentage > 35)
			msg_format("%^sは痛みで絶叫した。", m_name);
		else if (percentage > 20)
			msg_format("%^sは苦痛のあまり絶叫した。", m_name);
		else if (percentage > 10)
			msg_format("%^sは苦痛でもだえ苦しんだ。", m_name);
		else
			msg_format("%^sは弱々しく叫んだ。", m_name);
#else
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
#endif

	}

	/* Another type of monsters (shrug,cry,scream) */
	else
	{
#ifdef JP
		if (percentage > 95)
			msg_format("%^sは攻撃に肩をすくめた。", m_name);
		else if (percentage > 75)
			msg_format("%^sは痛みでうなった。", m_name);
		else if (percentage > 50)
			msg_format("%^sは痛みで叫んだ。", m_name);
		else if (percentage > 35)
			msg_format("%^sは痛みで絶叫した。", m_name);
		else if (percentage > 20)
			msg_format("%^sは苦痛のあまり絶叫した。", m_name);
		else if (percentage > 10)
			msg_format("%^sは苦痛でもだえ苦しんだ。", m_name);
		else
			msg_format("%^sは弱々しく叫んだ。", m_name);
#else
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
#endif

	}
}


/*
 * Learn about an "observed" resistance.
 */
void update_smart_learn(int m_idx, int what)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];


	/* Not allowed to learn */
	if (!smart_learn) return;

	/* Too stupid to learn anything */
	if (r_ptr->flags2 & (RF2_STUPID)) return;

	/* Not intelligent, only learn sometimes */
	if (!(r_ptr->flags2 & (RF2_SMART)) && (randint0(100) < 50)) return;


	/* XXX XXX XXX */

	/* Analyze the knowledge */
	switch (what)
	{
	case DRS_ACID:
		if (p_ptr->resist_acid) m_ptr->smart1 |= (SM1_RES_ACID);
		if (p_ptr->oppose_acid) m_ptr->smart1 |= (SM1_OPP_ACID);
		if (p_ptr->immune_acid) m_ptr->smart1 |= (SM1_IMM_ACID);
		break;

	case DRS_ELEC:
		if (p_ptr->resist_elec) m_ptr->smart1 |= (SM1_RES_ELEC);
		if (p_ptr->oppose_elec) m_ptr->smart1 |= (SM1_OPP_ELEC);
		if (p_ptr->immune_elec) m_ptr->smart1 |= (SM1_IMM_ELEC);
		break;

	case DRS_FIRE:
		if (p_ptr->resist_fire) m_ptr->smart1 |= (SM1_RES_FIRE);
		if (p_ptr->oppose_fire) m_ptr->smart1 |= (SM1_OPP_FIRE);
		if (p_ptr->immune_fire) m_ptr->smart1 |= (SM1_IMM_FIRE);
		break;

	case DRS_COLD:
		if (p_ptr->resist_cold) m_ptr->smart1 |= (SM1_RES_COLD);
		if (p_ptr->oppose_cold) m_ptr->smart1 |= (SM1_OPP_COLD);
		if (p_ptr->immune_cold) m_ptr->smart1 |= (SM1_IMM_COLD);
		break;

	case DRS_POIS:
		if (p_ptr->resist_pois) m_ptr->smart1 |= (SM1_RES_POIS);
		if (p_ptr->oppose_pois) m_ptr->smart1 |= (SM1_OPP_POIS);
		break;

	case DRS_NETH:
		if (p_ptr->resist_neth) m_ptr->smart1 |= (SM1_RES_NETH);
		if (p_ptr->evil_equip || prace_is_(RACE_GHOST)) m_ptr->smart2 |= (SM2_IMM_NETH);
		break;

	case DRS_LITE:
		if (p_ptr->resist_lite) m_ptr->smart1 |= (SM1_RES_LITE);
		break;

	case DRS_DARK:
		if (p_ptr->resist_dark) m_ptr->smart1 |= (SM1_RES_DARK);
		if (p_ptr->evil_equip || WRAITH_FORM()) m_ptr->smart2 |= (SM2_IMM_DARK);
		break;

	case DRS_FEAR:
		if (p_ptr->resist_fear) m_ptr->smart1 |= (SM1_RES_FEAR);
		break;

	case DRS_CONF:
		if (p_ptr->resist_conf) m_ptr->smart1 |= (SM1_RES_CONF);
		break;

	case DRS_CHAOS:
		if (p_ptr->resist_chaos) m_ptr->smart1 |= (SM1_RES_CHAOS);
		break;

	case DRS_DISEN:
		if (p_ptr->resist_disen) m_ptr->smart1 |= (SM1_RES_DISEN);
		break;

	case DRS_BLIND:
		if (p_ptr->resist_blind) m_ptr->smart1 |= (SM1_RES_BLIND);
		break;

	case DRS_STONE:
		if (p_ptr->resist_stone) m_ptr->smart1 |= (SM1_RES_STONE);
		break;

	case DRS_SOUND:
		if (p_ptr->resist_sound) m_ptr->smart1 |= (SM1_RES_SOUND);
		break;

	case DRS_SHARD:
		if (p_ptr->resist_shard) m_ptr->smart1 |= (SM1_RES_SHARD);
		break;

	case DRS_WATER:
		if (p_ptr->resist_water) m_ptr->smart2 |= (SM2_RES_WATER);
		break;

	case DRS_PLASMA:
		if (p_ptr->zoshonel_protect) m_ptr->smart2 |= (SM2_IMM_PLASMA);
		break;

	case DRS_TIME:
		if (p_ptr->resist_time) m_ptr->smart2 |= (SM2_RES_TIME);
		break;

	case DRS_TELE:
		if (p_ptr->anti_tele || p_ptr->earth_spike) m_ptr->smart2 |= (SM2_IMM_TELE);
		break;

	case DRS_DRAIN:
		if ((rp_ptr->r_flags & PRF_DEMON) || (cp_ptr->c_flags & PCF_DEMON)) m_ptr->smart2 |= (SM2_IMM_DRAIN);
		if ((rp_ptr->r_flags & PRF_UNDEAD) || (cp_ptr->c_flags & PCF_UNDEAD)) m_ptr->smart2 |= (SM2_IMM_DRAIN);
		break;

	case DRS_AVOID0:
		if (p_ptr->wind_guard) m_ptr->smart2 |= (SM2_IMM_AVOID0);
		break;

	case DRS_AVOID1:
		if (p_ptr->wind_guard && (p_ptr->stat_use[A_INT] >= (18 + 150))) m_ptr->smart2 |= (SM2_IMM_AVOID1);
		break;

	case DRS_AVOID2:
		if (p_ptr->wind_guard && (p_ptr->stat_use[A_INT] >= (18 + 200))) m_ptr->smart2 |= (SM2_IMM_AVOID2);
		break;

	case DRS_FREE:
		if (p_ptr->free_act) m_ptr->smart1 |= (SM1_IMM_FREE);
		break;

	case DRS_MANA:
		if (!p_ptr->msp) m_ptr->smart1 |= (SM1_IMM_MANA);
		break;

	case DRS_REFLECT:
		if (p_ptr->reflect) m_ptr->smart1 |= (SM1_IMM_REFLECT);
		break;
	}
}


/*
 * Place the player in the dungeon XXX XXX
 */
bool player_place(int y, int x)
{
	/* Paranoia XXX XXX */
	if (cave[y][x].m_idx != 0) return FALSE;

	/* Save player location */
	py = y;
	px = x;

	/* Success */
	return TRUE;
}


/*
 * Drop all items carried by a monster
 */
void monster_drop_carried_objects(monster_type *m_ptr)
{
	s16b this_o_idx, next_o_idx = 0;
	object_type forge;
	object_type *o_ptr;
	object_type *q_ptr;


	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Get local object */
		q_ptr = &forge;

		/* Copy the object */
		object_copy(q_ptr, o_ptr);

		/* Forget monster */
		q_ptr->held_m_idx = 0;

		/* Delete the object */
		delete_object_idx(this_o_idx);

		/* Drop it */
		(void)drop_near(q_ptr, -1, m_ptr->fy, m_ptr->fx);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;
}


#define MON_ANTI_MAGIC_RAD 3

/*
 * Is there an anti-magic grid for caster?
 * Note: If who = -1, caster is player.
 *       If who = 0, natural summoning.
 */
bool is_anti_magic_grid(int who, int y, int x)
{
	if (!character_dungeon) return FALSE;

	/* Nearby anti-magic monsters */
	if (num_anti_magic)
	{
		int i;
		monster_type *am_ptr;

		for (i = 0; i < num_anti_magic; i++)
		{
			am_ptr = &m_list[anti_magic_m_idx[i]];

			/* Skip distant grid */
			if (distance(am_ptr->fy, am_ptr->fx, y, x) > MON_ANTI_MAGIC_RAD) continue;

			/* Require line of sight */
			if (!los(am_ptr->fy, am_ptr->fx, y, x)) continue;

			/* Monster must be 'an enemy' */
			if (who > 0)
			{
				if (!are_enemies(am_ptr, &m_list[who])) continue;
			}
			else if (who < 0)
			{
				if (!is_hostile(am_ptr)) continue;

				if (am_ptr->ml)
				{
					r_info[am_ptr->r_idx].r_flags3 |= RF3_ANTI_MAGIC;

					/* Window stuff */
					p_ptr->window |= (PW_MONSTER);
				}
			}

			/* There is an anti-magic grid */
			return TRUE;
		}
	}

	/* You are level 30+ Terror-Knight or wield anti-magic field equipment */
	if (p_ptr->anti_magic_field > 0)
	{
		/* Skip distant grid */
		if (distance(py, px, y, x) > p_ptr->anti_magic_field) return FALSE;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) return FALSE;

		/* Monster must be 'an enemy' */
		if (who > 0)
		{
			if (!is_hostile(&m_list[who])) return FALSE;
		}
		else if (who < 0) return FALSE;

		/* There is an anti-magic grid */
		return TRUE;
	}

	/* There is not an anti-magic grid */
	return FALSE;
}

#define FEAR_FIELD_RAD 3

/*
 * Is there a fear field grid for attacker?
 * Note: If who = 0, attacker is player.
 *       If who < 0, natural attack (not applied).
 * Return value is hostile fear field count. (0: none)
 */
int is_fear_field_grid(int who, int y, int x)
{
	int count = 0;

	if (!character_dungeon) return 0;

	/* Resist fear field */
	if (!who)
	{
		if (p_ptr->resist_fear) return 0;
	}
	else if (who > 0)
	{
		if (r_info[m_list[who].r_idx].flags3 & RF3_NO_FEAR) return 0;
	}
	else return 0;

	/* Nearby fear field monsters */
	if (num_fear_field)
	{
		int i;
		monster_type *ff_ptr;

		for (i = 0; i < num_fear_field; i++)
		{
			ff_ptr = &m_list[fear_field_m_idx[i]];

			/* Skip distant grid */
			if (distance(ff_ptr->fy, ff_ptr->fx, y, x) > FEAR_FIELD_RAD) continue;

			/* Require line of sight */
			if (!los(ff_ptr->fy, ff_ptr->fx, y, x)) continue;

			/* Monster must be 'an enemy' */
			if (who)
			{
				if (!are_enemies(ff_ptr, &m_list[who])) continue;
			}
			else
			{
				if (!is_hostile(ff_ptr)) continue;

				if (ff_ptr->ml)
				{
					r_info[ff_ptr->r_idx].r_flags3 |= RF3_FEAR_FIELD;

					/* Window stuff */
					p_ptr->window |= (PW_MONSTER);
				}
			}

			/* There is a fear field grid */
			count++;
		}
	}

	/* You are level 20+ Terror-Knight or wield fear field equipment */
	if (p_ptr->fear_field)
	{
		/* Skip distant grid */
		if (distance(py, px, y, x) > FEAR_FIELD_RAD) return count;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) return count;

		/* Monster must be 'an enemy' */
		if (who > 0)
		{
			if (!is_hostile(&m_list[who])) return count;
		}
		else return count;

		/* There is a fear field grid */
		count++;
	}

	/* Return result */
	return count;
}

/*
 * Set a "blow" record for runeweapon
 */
static void runeweapon_blow(monster_race *r_ptr, int from, int end, int m, int e, int d, int s)
{
	int i;

	for (i = from; i <= end; i++)
	{
		/* Save the data */
		r_ptr->blow[i].method = (byte)m;
		r_ptr->blow[i].effect = (byte)e;
		r_ptr->blow[i].d_dice = (byte)MIN(255, d);
		r_ptr->blow[i].d_side = (byte)MIN(255, s);
	}
}

#include "init.h"

/*
 * Create the "Runeweapon" unique
 * Note: "Runeweapon" is the FAKE monster.
 */
bool create_runeweapon(int specific)
{
	monster_race    *r_ptr;
	runeweapon_type *runeweapon;
	object_type     *o_ptr;
	u32b            flgs[TR_FLAG_SIZE];
	char            t_name[MAX_NLEN];
	cptr            base_name = NULL;
#ifdef JP
	char            E_t_name[MAX_NLEN];
	cptr            E_base_name = NULL;
	bool            prefix = FALSE;
#endif
	int             alloc_race_idx;

	/* Don't exist previous runeweapon */
	if (!runeweapon_num) return FALSE;

	if ((specific < 1) || (specific > runeweapon_num)) return FALSE;

	r_ptr = &r_info[runeweapon_r_idx_from(specific)];

	/* Access runeweapon */
	runeweapon = &runeweapon_list[specific];

	/* Wipe the monster */
	(void)WIPE(r_ptr, monster_race);

	/* Get object */
	o_ptr = &runeweapon->weapon;
	if (!o_ptr->k_idx) return FALSE;

#ifdef JP
	strcpy(t_name, quark_str(o_ptr->art_name));
	if (strncmp(t_name, "『" , 2) != 0) prefix = TRUE;
#endif

	/**** Basic info ****/

	/* "G:" */
	r_ptr->d_attr = TERM_VIOLET;

	/* "I:" */
	r_ptr->speed = MIN(100 + runeweapon->level + runeweapon->reincarnate_cnt * 10, 255);
	r_ptr->hdice = MIN((u16b)runeweapon->hp + (u16b)runeweapon->sp / 2, 65535);
	r_ptr->hside = 10;
	r_ptr->aaf = 100;
	r_ptr->sleep = 0;

	/* "W:" */
	r_ptr->level = runeweapon->level * 2 + 7;
	r_ptr->rarity = 14;
	r_ptr->mexp = 99999;

	/* "F:" */
	r_ptr->flags1 |= (RF1_UNIQUE | RF1_FORCE_MAXHP | RF1_FORCE_SLEEP);
	r_ptr->flags2 |= (RF2_COLD_BLOOD | RF2_BASH_DOOR);
	r_ptr->flags3 |= (RF3_NONLIVING | RF3_NO_FEAR | RF3_NO_CONF | RF3_NO_SLEEP);
	r_ptr->flags7 |= (RF7_CAN_FLY);
	r_ptr->flagsr |= (RFR_RES_ACID | RFR_RES_ELEC | RFR_RES_FIRE | RFR_RES_COLD | RFR_RES_POIS);

	/**** Additional info ****/

	/* Get alignment */
	if (runeweapon->align & SUB_ALIGN_EVIL) r_ptr->flags3 |= (RF3_EVIL);
	if (runeweapon->align & SUB_ALIGN_GOOD) r_ptr->flags3 |= (RF3_GOOD);
	if (runeweapon->align & SUB_ALIGN_LAWFUL) r_ptr->flags7 |= (RF7_LAWFUL);
	if (runeweapon->align & SUB_ALIGN_CHAOTIC) r_ptr->flags7 |= (RF7_CHAOTIC);

	/* Get element */
	r_ptr->flags3 |= (RF3_ELEM_FIRE << runeweapon->elem);
	r_ptr->r_elem = runeweapon->elem;

	object_flags(o_ptr, flgs);

	/* Get resistance */
	if (have_flag(flgs, TR_RES_LITE)) r_ptr->flagsr |= (RFR_RES_LITE);
	if (have_flag(flgs, TR_RES_DARK)) r_ptr->flagsr |= (RFR_RES_DARK);
	if (have_flag(flgs, TR_RES_CONF)) r_ptr->flagsr |= (RFR_RES_CONF);
	if (have_flag(flgs, TR_RES_SOUND)) r_ptr->flagsr |= (RFR_RES_SOUN);
	if (have_flag(flgs, TR_RES_SHARDS)) r_ptr->flagsr |= (RFR_RES_SHAR);
	if (have_flag(flgs, TR_RES_NETHER)) r_ptr->flagsr |= (RFR_RES_NETH);
	if (have_flag(flgs, TR_RES_STONE)) r_ptr->flagsr |= (RFR_RES_STON);
	if (have_flag(flgs, TR_RES_CHAOS)) r_ptr->flagsr |= (RFR_RES_CHAO);
	if (have_flag(flgs, TR_RES_DISEN)) r_ptr->flagsr |= (RFR_RES_DISE);
	if (have_flag(flgs, TR_NO_TELE)) r_ptr->flags3 |= (RF3_RES_TELE);
	if (have_flag(flgs, TR_ANTI_MAGIC)) r_ptr->flags3 |= (RF3_ANTI_MAGIC);
	if (have_flag(flgs, TR_WRAITH)) r_ptr->flags2 |= (RF2_PASS_WALL);
	if (have_flag(flgs, TR_FEAR_FIELD)) r_ptr->flags3 |= (RF3_FEAR_FIELD);

	/* Get ability from weapon */
	switch (o_ptr->tval)
	{
	case TV_BOW:
		r_ptr->d_char = '}';
		switch (o_ptr->sval)
		{
		case SV_RUNEBOW:
#ifdef JP
			base_name = "魔弓";
			E_base_name = "Runebow";
#else
			base_name = "Runebow";
#endif
			runeweapon_blow(r_ptr, 0, 0, RBM_SHOOT, RBE_HURT, (20000 - runeweapon->bow_energy) / 500, runeweapon->bow_tmul * 4);
			runeweapon_blow(r_ptr, 1, 2, RBM_HIT, RBE_HURT, (20000 - runeweapon->bow_energy) / 500, runeweapon->bow_tmul * 4);
			r_ptr->freq_spell = 100 / 1;
			r_ptr->flags4 |= (RF4_SHOOT);
			r_ptr->flags6 |= (RF6_BLINK);
			r_ptr->ac = 145;
			break;

		case SV_RUNEGUN:
#ifdef JP
			base_name = "魔銃";
			E_base_name = "Runegun";
#else
			base_name = "Runegun";
#endif
			runeweapon_blow(r_ptr, 0, 0, RBM_SHOOT, RBE_HURT, (20000 - runeweapon->bow_energy) / 500, runeweapon->bow_tmul * 4);
			runeweapon_blow(r_ptr, 1, 2, RBM_HIT, RBE_HURT, (20000 - runeweapon->bow_energy) / 500, runeweapon->bow_tmul * 4);
			r_ptr->freq_spell = 100 / 2;
			r_ptr->flags4 |= (RF4_ROCKET | RF4_SHOOT_GUN);
			r_ptr->ac = 145;
			break;
		}
		break;

	case TV_HAFTED:
		r_ptr->d_char = '\\';
		switch (o_ptr->sval)
		{
		case SV_RUNEHAMMER:
#ifdef JP
			base_name = "魔槌";
			E_base_name = "Runehammer";
#else
			base_name = "Runehammer";
#endif
			runeweapon_blow(r_ptr, 0, 2, RBM_HIT, RBE_SHATTER, o_ptr->dd * 3 / 2, o_ptr->ds * 3 / 2);
			r_ptr->ac = 200;
			break;

		case SV_RUNEWHIP:
#ifdef JP
			base_name = "魔鞭";
			E_base_name = "Runewhip";
#else
			base_name = "Runewhip";
#endif
			runeweapon_blow(r_ptr, 0, 2, RBM_HIT, RBE_HURT, o_ptr->dd, o_ptr->ds);
			r_ptr->freq_spell = 100 / 4;
			r_ptr->flags6 |= (RF6_S_ANT | RF6_S_SPIDER | RF6_S_BEAST | RF6_S_HOUND | RF6_S_HI_DRAGON);
			r_ptr->ac = 160;
			break;

		case SV_RUNESTAFF:
#ifdef JP
			base_name = "魔杖";
			E_base_name = "Runestaff";
#else
			base_name = "Runestaff";
#endif
			runeweapon_blow(r_ptr, 0, 2, RBM_HIT, RBE_UN_POWER, o_ptr->dd, o_ptr->ds);
			r_ptr->freq_spell = 100 / 3;
			r_ptr->flags5 |= (RF5_BA_MANA | RF5_BA_DARK | RF5_BA_LITE | RF5_BO_MANA);
			r_ptr->flags6 |= (RF6_S_HI_DRAGON);
			r_ptr->flagsa |= (RFA_PETRO_CLOUD | RFA_SAND_STORM | RFA_PURE_ELEM_BEAM);
			r_ptr->flagsa |= (RFA_FIRE_STORM << runeweapon->elem);
			r_ptr->ac = 110;
			break;

		case SV_RUNEFAN:
#ifdef JP
			base_name = "魔扇";
			E_base_name = "Runefan";
#else
			base_name = "Runefan";
#endif
			runeweapon_blow(r_ptr, 0, 2, RBM_HIT, RBE_DR_MANA, o_ptr->dd, o_ptr->ds);
			r_ptr->freq_spell = 100 / 2;
			r_ptr->flags4 |= (RF4_DISPEL);
			r_ptr->flagsa |= (RFA_SALAMANDER | RFA_FENRER | RFA_GNOME | RFA_THUNDERBIRD | RFA_IGNIS_FATUUS | RFA_DARK_LORE);
			r_ptr->ac = 110;
			break;
		}
		break;

	case TV_POLEARM:
		r_ptr->d_char = '/';
		switch (o_ptr->sval)
		{
		case SV_RUNESPEAR:
#ifdef JP
			base_name = "魔槍";
			E_base_name = "Runespear";
#else
			base_name = "Runespear";
#endif
			runeweapon_blow(r_ptr, 0, 2, RBM_HIT, RBE_HURT, o_ptr->dd * 3 / 2, o_ptr->ds * 3 / 2);
			r_ptr->ac = 160;
			break;

		case SV_RUNEAXE:
#ifdef JP
			base_name = "魔斧";
			E_base_name = "Runeaxe";
#else
			base_name = "Runeaxe";
#endif
			runeweapon_blow(r_ptr, 0, 2, RBM_HIT, RBE_SHATTER, o_ptr->dd * 3 / 2, o_ptr->ds * 3 / 2);
			r_ptr->ac = 175;
			break;
		}
		break;

	case TV_SWORD:
		r_ptr->d_char = '|';
		switch (o_ptr->sval)
		{
		case SV_RUNEBLADE:
#ifdef JP
			base_name = "魔剣";
			E_base_name = "Runeblade";
#else
			base_name = "Runeblade";
#endif
			runeweapon_blow(r_ptr, 0, 2, RBM_SLASH, RBE_SUPERHURT, o_ptr->dd, o_ptr->ds);
			r_ptr->freq_spell = 100 / 5;
			r_ptr->flags6 |= (RF6_BLINK | RF6_TPORT | RF6_TELE_TO);
			r_ptr->ac = 160;
			break;

		case SV_RUNECLAW:
#ifdef JP
			base_name = "魔爪";
			E_base_name = "Runeclaw";
#else
			base_name = "Runeclaw";
#endif
			runeweapon_blow(r_ptr, 0, 2, RBM_SLASH, RBE_SUPERHURT, o_ptr->dd, o_ptr->ds);
			r_ptr->freq_spell = 100 / 5;
			r_ptr->flags6 |= (RF6_BLINK | RF6_TPORT | RF6_TELE_TO);
			r_ptr->ac = 145;
			break;
		}
		break;
	}

	/* Construct the body of your ancestor */
	if (astral_mode && (specific == 1))
	{
		r_ptr->d_char = '@';
		r_ptr->d_attr = elem_attr(runeweapon->elem);

		r_ptr->hside *= 3;

		r_ptr->flags1 |= (RF1_QUESTOR);
		r_ptr->flags2 &= ~(RF2_COLD_BLOOD);
		r_ptr->flags2 |= (RF2_EMPTY_MIND | RF2_REGENERATE);
		r_ptr->flags3 &= ~(RF3_NONLIVING);
		r_ptr->flags7 &= ~(RF7_CAN_FLY);

		switch (runeweapon->race)
		{
		case RACE_HUMAN:
			r_ptr->flags2 |= (RF2_HUMAN);
			break;

		case RACE_HAWKMAN:
		case RACE_VULTAN:
		case RACE_RAVEN:
			r_ptr->flags2 |= (RF2_HUMAN);
			r_ptr->flags7 |= (RF7_CAN_FLY);
			break;

		case RACE_LIZARDMAN:
			r_ptr->flags3 |= (RF3_DRAGON);
			break;

		case RACE_FAIRY:
			r_ptr->flags7 |= (RF7_CAN_FLY);
			break;

		case RACE_GREMLIN:
			r_ptr->flags3 |= (RF3_DEMON);
			r_ptr->flags7 |= (RF7_CAN_FLY);
			break;

		case RACE_SKELETON:
			r_ptr->flags2 |= (RF2_COLD_BLOOD);
			r_ptr->flags3 |= (RF3_UNDEAD);
			break;

		case RACE_GHOST:
			r_ptr->flags2 |= (RF2_INVISIBLE | RF2_COLD_BLOOD | RF2_PASS_WALL);
			r_ptr->flags3 |= (RF3_UNDEAD);
			r_ptr->flags7 |= (RF7_CAN_FLY);
			break;

		case RACE_GOBLIN:
			r_ptr->flags3 |= (RF3_ORC);
			break;
		}

#ifdef JP
		sprintf(t_name, "『%s』", runeweapon->ancestor);
		strcpy(E_t_name, "The Body of the Your Ancestor");
#else
		sprintf(t_name, "The Body of %s", runeweapon->ancestor);
#endif
	}
	else
	{
#ifdef JP
		sprintf(t_name, "%s%s%s", prefix ? quark_str(o_ptr->art_name) : "", base_name, prefix ? "" : quark_str(o_ptr->art_name));
		sprintf(E_t_name, "The %s", E_base_name);
#else
		sprintf(t_name, "The %s %s", base_name, quark_str(o_ptr->art_name));
#endif
	}

	/* Get the name */
	r_ptr->name = r_head.name_size - MAX_NLEN * (MAX_RUNEWEAPON - specific + 1);
	strcpy(r_name + r_ptr->name, t_name);
#ifdef JP
	r_ptr->E_name = r_ptr->name + strlen(t_name) + 1;
	strcpy(r_name + r_ptr->E_name, E_t_name);
#endif

	if (astral_mode && (specific == 1))
	{
		r_ptr->text = r_head.text_size - MAX_NLEN * 4 * MAX_RUNEWEAPON;
		strcpy(r_text + r_ptr->text, "自らの魂を剣に封じ、主を失ったあなたの先祖の肉体は、魂であるあなたの手中の剣を熱望している。その肉体は、あなたの手から剣を奪うためにはいかなる手段もためらわないだろう。");
	}
	else r_ptr->text = 0;

	r_ptr->cur_num = 0;
	r_ptr->max_num = 1;

	/* Make sure it looks right */
	r_ptr->x_attr = r_ptr->d_attr;
	r_ptr->x_char = r_ptr->d_char;

	/**** Prepare allocation unit ****/

	alloc_race_idx = alloc_race_size + specific - 1;

	/* Wipe the allocation unit */
	(void)WIPE(&alloc_race_table[alloc_race_idx], alloc_entry);

	alloc_race_table[alloc_race_idx].index = runeweapon_r_idx_from(specific);
	alloc_race_table[alloc_race_idx].level = r_ptr->level;
	alloc_race_table[alloc_race_idx].prob1 = 100 / r_ptr->rarity;
	alloc_race_table[alloc_race_idx].prob2 = 100 / r_ptr->rarity;
	alloc_race_table[alloc_race_idx].prob3 = 100 / r_ptr->rarity;

	return TRUE;
}

/*
 * Verify the "Runeweapon" uniques
 */
void verify_runeweapon(void)
{
	int             i, j, num = 0;
	monster_type    *m_ptr;
	monster_race    *r_ptr;
	s16b            r_idx;
	s16b            alloc_idx;
	s16b            illegal_r_idx[MAX_RUNEWEAPON];
	bool            allow_appear;

	/* No runeweapon */
	if (!runeweapon_num) return;

	/* Scan runeweapons */
	for (i = 1; i <= runeweapon_num; i++)
	{
		r_idx = runeweapon_r_idx_from(i);
		r_ptr = &r_info[r_idx];
		alloc_idx = alloc_race_size + i - 1;
		allow_appear = TRUE;

		if (runeweapon_list[i].status & RW_STATUS_ILLEGAL)
		{
			if (!p_ptr->wizard && (!astral_mode || (i > 1))) allow_appear = FALSE;
		}
		if (astral_mode)
		{
			if (i > 1) allow_appear = FALSE;
		}

		if (allow_appear)
		{
			/* Set the name offset */
			r_ptr->name = r_head.name_size - MAX_NLEN * (MAX_RUNEWEAPON - i + 1);
#ifdef JP
			r_ptr->E_name = r_ptr->name + strlen(r_name + r_ptr->name) + 1;
#endif
			if (astral_mode && (i == 1)) r_ptr->text = r_head.text_size - MAX_NLEN * 4 * MAX_RUNEWEAPON;
			else r_ptr->text = 0;

			/* Create the allocation unit */
			alloc_race_table[alloc_idx].index = r_idx;
			alloc_race_table[alloc_idx].level = r_ptr->level;
			alloc_race_table[alloc_idx].prob1 = 100 / r_ptr->rarity;
			alloc_race_table[alloc_idx].prob2 = 100 / r_ptr->rarity;
			alloc_race_table[alloc_idx].prob3 = 100 / r_ptr->rarity;
		}
		else
		{
			/* Remove the name offset */
			r_ptr->text = 0;
#ifdef JP
			r_ptr->E_name = 0;
#endif
			r_ptr->name = 0;

			/* Wipe the allocation unit */
			(void)WIPE(&alloc_race_table[alloc_idx], alloc_entry);

			illegal_r_idx[num++] = r_idx;
		}
	}

	/* No illegal runeweapon */
	if (!num) return;

	/* If illegal weapons are in non-wizard mode, remove them */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Ignore normal monsters */
		if (!monster_is_runeweapon(m_ptr->r_idx)) continue;

		for (j = 0; j < num; j++)
		{
			if (m_ptr->r_idx == illegal_r_idx[j])
			{
				/* Check for quest completion */
				check_quest_completion(m_ptr);

				if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
				{
					char m_name[80];

					monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
					do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_COMPACT, m_name);
				}

				delete_monster_idx(i);
				break;
			}
		}
	}
}

/*
 * Remove the specified "Runeweapon" unique (except clone)
 */
void remove_runeweapon(int specific)
{
	int             i, r_idx;
	monster_type    *m_ptr;

	/* No runeweapon */
	if (!runeweapon_num) return;

	if ((specific < 1) || (specific > runeweapon_num)) return;

	r_idx = runeweapon_r_idx_from(specific);

	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		if (m_ptr->r_idx == r_idx)
		{
			if (!(m_ptr->smart1 & SM1_CLONED))
			{
				/* Check for quest completion */
				check_quest_completion(m_ptr);

				delete_monster_idx(i);
			}
			break;
		}
	}
}

/*
 * Is this index is "Runeweapon"?
 */
bool monster_is_runeweapon(int r_idx)
{
	return (r_idx >= max_r_idx) && (r_idx < (max_r_idx + runeweapon_num));
}
