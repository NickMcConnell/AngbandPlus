/* File: monmove.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



/*
 * Terrified monsters will turn to fight if they are slower than the
 * character, and closer to him than this distance.
 */
#define TURN_RANGE      3

/*
 * Calculate minimum and desired combat ranges.  -BR-
 */
static void find_range(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u16b p_lev, m_lev;
	u16b p_chp, p_mhp;
	u16b m_chp, m_mhp;
	u32b p_val, m_val;

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) m_ptr->min_range = FLEE_RANGE;

	/* Some monsters run when low on mana */
	else if ((r_ptr->flags2 & (RF2_LOW_MANA_RUN)) &&
	    ((m_ptr->mana < r_ptr->mana / 6) || 
	    (m_ptr->mana == 0))) m_ptr->min_range = FLEE_RANGE;

	/* Hack -- townsmen go about their business */
	else if (m_ptr->mflag & (MFLAG_TOWN)) m_ptr->min_range = 1;

	/* Breeders cannot be terrified */
	else if (r_ptr->flags2 & (RF2_MULTIPLY)) m_ptr->min_range = 1;

	/* Allow monster terror */
	else
	{
		/* Minimum distance - stay at least this far if possible */
		m_ptr->min_range = 1;

		/* Examine player power  */
		p_lev = p_ptr->lev;

		/* Examine monster power (level plus morale) */
		m_lev = (r_ptr->level * 2) +
			(cave_m_idx[m_ptr->fy][m_ptr->fx] % 10) + 25;

		/* Optimize extreme cases below */
		if (m_lev < p_lev + 4) m_ptr->min_range = FLEE_RANGE;
		else if (m_lev + 3 < p_lev)
		{
			/* Examine player health */
			p_chp = p_ptr->chp;
			p_mhp = p_ptr->mhp;

			/* Examine monster health */
			m_chp = m_ptr->hp;
			m_mhp = m_ptr->maxhp;

			/* Prepare to optimize the calculation */
			p_val = (p_lev * p_mhp) + (p_chp << 2);	/* div p_mhp */
			m_val = (m_lev * m_mhp) + (m_chp << 2);	/* div m_mhp */

			/* Strong players scare strong monsters */
			if (p_val * m_mhp > m_val * p_mhp)
				m_ptr->min_range = FLEE_RANGE;
		}
	}

	if (m_ptr->min_range < FLEE_RANGE)
	{
		/* Creatures that don't move never like to get too close */
		if (r_ptr->flags1 & (RF1_NEVER_MOVE)) m_ptr->min_range += 3;

		/* Spellcasters that don't strike never like to get too close */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW)) m_ptr->min_range += 3;
	}

	/* Maximum range to flee to */
	if (m_ptr->min_range > FLEE_RANGE) m_ptr->min_range = FLEE_RANGE;

	/* Nearby monsters that cannot run away will stand and fight */
	if ((m_ptr->cdis < TURN_RANGE) && (m_ptr->mspeed < p_ptr->pspeed))
		m_ptr->min_range = 1;

	/* Now find preferred range */
	m_ptr->best_range = m_ptr->min_range;

	/* Archers are quite happy at a good distance */
	if (r_ptr->flags2 & (RF2_ARCHER)) m_ptr->best_range += 3;

	if (r_ptr->freq_ranged > 24)
	{
		/* Heavy spell casters will sit back and cast */
		if (m_ptr->mana > r_ptr->mana / 4) m_ptr->best_range += 3;

		/* Breathers like point blank range */
		if (((r_ptr->flags4 & (RF4_BREATH_MASK)) ||
		     (r_ptr->flags5 & (RF5_BREATH_MASK)) ||
		     (r_ptr->flags6 & (RF6_BREATH_MASK)) ||
		     (r_ptr->flags7 & (RF7_BREATH_MASK))) &&
		    (m_ptr->best_range < 6) &&
		    (m_ptr->hp > m_ptr->maxhp / 2))
		{
			m_ptr->best_range = 6;
		}
	}
}


/*
 * Given a central direction at position [dir #][0], return a series
 * of directions radiating out on both sides from the central direction
 * all the way back to its rear.
 *
 * Side directions come in pairs; for example, directions '1' and '3'
 * flank direction '2'.  The code should know which side to consider
 * first.  If the left, it must add 10 to the central direction to
 * access the second part of the table.
 */
static byte side_dirs[20][8] =
{
	{ 0, 0, 0, 0, 0, 0, 0, 0 },	/* bias right */
	{ 1, 4, 2, 7, 3, 8, 6, 9 },
	{ 2, 1, 3, 4, 6, 7, 9, 8 },
	{ 3, 2, 6, 1, 9, 4, 8, 7 },
	{ 4, 7, 1, 8, 2, 9, 3, 6 },
	{ 5, 5, 5, 5, 5, 5, 5, 5 },
	{ 6, 3, 9, 2, 8, 1, 7, 4 },
	{ 7, 8, 4, 9, 1, 6, 2, 3 },
	{ 8, 9, 7, 6, 4, 3, 1, 2 },
	{ 9, 6, 8, 3, 7, 2, 4, 1 },

	{ 0, 0, 0, 0, 0, 0, 0, 0 },	/* bias left */
	{ 1, 2, 4, 3, 7, 6, 8, 9 },
	{ 2, 3, 1, 6, 4, 9, 7, 8 },
	{ 3, 6, 2, 9, 1, 8, 4, 7 },
	{ 4, 1, 7, 2, 8, 3, 9, 6 },
	{ 5, 5, 5, 5, 5, 5, 5, 5 },
	{ 6, 9, 3, 8, 2, 7, 1, 4 },
	{ 7, 4, 8, 1, 9, 2, 6, 3 },
	{ 8, 7, 9, 4, 6, 1, 3, 2 },
	{ 9, 8, 6, 7, 3, 4, 2, 1 }
};


/*
 * Get and return the strength (age) of scent in a given grid.
 *
 * Return "-1" if no scent exists in the grid.
 */
int get_scent(int y, int x)
{
	int age;
	int scent;

	/* Check Bounds */
	if (!(in_bounds(y, x))) return (-1);

	/* Sent trace? */
	scent = cave_when[y][x];

	/* No scent at all */
	if (!scent) return (-1);

	/* Get age of scent */
	age = scent - scent_when;

	/* Return the age of the scent */
	return (age);
}


/*
 * Can the monster catch a whiff of the character?
 *
 * Many more monsters can smell, but they find it hard to smell and
 * track down something at great range.
 */
static bool monster_can_smell(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int age;

	/* Get the age of the scent here */
	age = get_scent(m_ptr->fy, m_ptr->fx);

	/* No scent */
	if (age == -1) return (FALSE);

	/* Scent is too old */
	if (age > SMELL_STRENGTH) return (FALSE);


	if ((r_ptr->flags2 & (RF2_TRACKER)) || strchr("Z", r_ptr->d_char))
	{
		/* I smell a character! */
		return (TRUE);
	}

	/* Other monsters can sometimes make good use of scent */
	else if (strchr("DRdfq", r_ptr->d_char))
	{
		if (age <= SMELL_STRENGTH - 10)
		{
			/* Something's in the air... */
			return (TRUE);
		}
	}

	/* You're imagining things. */
	return (FALSE);
}

/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */
static int summon_possible(int y1, int x1)
{
	int y, x;
	int num_clear=0;

	/* Start at the player's location, and check 2 grids in each dir */
	for (y = y1 - 2; y <= y1 + 2; y++)
	{
		for (x = x1 - 2; x <= x1 + 2; x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(y, x)) continue;

			/* Only check a circular area */
			if (distance(y1, x1, y, x) > 2) continue;

			/* Hack: no summon on glyph of warding */
			if (cave_feat[y][x] == FEAT_GLYPH) continue;

			/* Require empty floor grid in line of sight */
			if (cave_empty_bold(y, x) && los(y1, x1, y, x))
			{
				num_clear++;
			}
		}
	}

	return (num_clear);
}

/*
 * Fully update monster's knowledge of the player.
 * Used by player ghost (and all monsters with smart_cheat).
 */
static void update_smart_cheat(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	
	/* Set up the saving throw */
	int save;

	
	save = p_ptr->skill_sav;	
	
	/* Know weirdness */
	if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
	if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
	if (save >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
	if (save >= 100) m_ptr->smart |= (SM_PERF_SAVE);
	if (p_ptr->resist_fear) m_ptr->smart |= (SM_IMM_FEAR);
	if (p_ptr->resist_blind) m_ptr->smart |= (SM_IMM_BLIND);
	if (p_ptr->resist_confu) m_ptr->smart |= (SM_IMM_CONFU);

	/* Know immunities */
    /* need something for percentile resists here */
	if (p_ptr->res[RS_FIR] > 20) m_ptr->smart |= (SM_OPP_FIRE);
	if (p_ptr->res[RS_EAR] > 20) m_ptr->smart |= (SM_OPP_EARTH);
	if (p_ptr->res[RS_AIR] > 20) m_ptr->smart |= (SM_OPP_AIR);
	if (p_ptr->res[RS_WTR] > 20) m_ptr->smart |= (SM_OPP_WATER);
	if (p_ptr->res[RS_ELC] > 20) m_ptr->smart |= (SM_OPP_ELEC);
	if (p_ptr->res[RS_ICE] > 20) m_ptr->smart |= (SM_OPP_ICE);
	if (p_ptr->res[RS_ACD] > 20) m_ptr->smart |= (SM_OPP_ACID);
	if (p_ptr->res[RS_PSN] > 20) m_ptr->smart |= (SM_OPP_POISON);
	if (p_ptr->res[RS_TIM] > 20) m_ptr->smart |= (SM_OPP_TIME);
	if (p_ptr->res[RS_ETH] > 20) m_ptr->smart |= (SM_OPP_ETHER);
	if (p_ptr->res[RS_SND] > 20) m_ptr->smart |= (SM_OPP_SOUND);
	if (p_ptr->res[RS_NTH] > 20) m_ptr->smart |= (SM_OPP_NETHER);
	if (p_ptr->res[RS_LIT] > 20) m_ptr->smart |= (SM_OPP_LIGHT);
	if (p_ptr->res[RS_DRK] > 20) m_ptr->smart |= (SM_OPP_DARK);
	if (p_ptr->res[RS_PSI] > 20) m_ptr->smart |= (SM_OPP_PSI);
	if (p_ptr->res[RS_TLK] > 20) m_ptr->smart |= (SM_OPP_TK);
	if (p_ptr->res[RS_SPI] > 20) m_ptr->smart |= (SM_OPP_SPIRIT);
	

	return;
}

/*
 * Used to determine the player's known level of resistance to a
 * particular spell.
 *
 * The LRN_xxx constant determines what type of resistance is
 * applicable.  The monster's SM_xxx flags, as well as player
 * conditions, are referred to as needed.
 * -BR-
 */
static int find_resist(int m_idx, int spell_lrn)
{
	monster_type *m_ptr = &m_list[m_idx];

	int a;
	u32b smart;

	/* Nothing Known */
	if (!m_ptr->smart) return (0);

	/* get smart flags */
	smart=m_ptr->smart;

	/* Which spell */
	switch (spell_lrn)
	{
		/* Spells 'resisted' by AC, Dex, etc.
		 * Currently no assessment is made */
		case LRN_ARCH:
		{
			return (0);
		}
		/* As above, but poisonous. */
		case LRN_PARCH:
		{
			/* if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS))) return (10); */
			/* else return (0); */
		}
		/* Fire Spells */
		case LRN_FIRE:
		{
			if (smart & (SM_OPP_FIRE)) return (p_ptr->res[RS_FIR]);
			else return (0);
		}
		/* Earth Spells */
		case LRN_EARTH:
		{
			if (smart & (SM_OPP_EARTH)) return (p_ptr->res[RS_EAR]);
			else return (0);
		}
		/* Air Spells */
		case LRN_AIR:
		{
			if (smart & (SM_OPP_AIR)) return (p_ptr->res[RS_AIR]);
			else return (0);
		}
		/* Water Spells */
		case LRN_WATER:
		{
			if (smart & (SM_OPP_WATER)) return (p_ptr->res[RS_WTR]);
			else return (0);
		}
		/* Electricty Spells */
		case LRN_ELEC:
		{
			if (smart & (SM_OPP_ELEC)) return (p_ptr->res[RS_ELC]);
			else return (0);
		}
		/* Ice Spells */
		case LRN_ICE:
		{
			if (smart & (SM_OPP_ICE)) return (p_ptr->res[RS_ICE]);
			else return (0);
		}
		/* Acid Spells */
		case LRN_ACID:
		{
			if (smart & (SM_OPP_ACID)) return (p_ptr->res[RS_ACD]);
			else return (0);
		}
		/* Poison Spells */
		case LRN_POISON:
		{
			if (smart & (SM_OPP_POISON)) return (p_ptr->res[RS_PSN]);
			else return (0);
		}
		/* Time Spells */
		case LRN_TIME:
		{
			if (smart & (SM_OPP_TIME)) return (p_ptr->res[RS_TIM]);
			else return (0);
		}
		/* Ether Spells */
		case LRN_ETHER:
		{
			if (smart & (SM_OPP_ETHER)) return (p_ptr->res[RS_ETH]);
			else return (0);
		}
		/* Sound Spells */
		case LRN_SOUND:
		{
			if (smart & (SM_OPP_SOUND)) return (p_ptr->res[RS_SND]);
			else return (0);
		}
		/* Nether Spells */
		case LRN_NETHER:
		{
			if (smart & (SM_OPP_NETHER)) return (p_ptr->res[RS_NTH]);
			else return (0);
		}
		/* Light Spells */
		case LRN_LIGHT:
		{
			if (smart & (SM_OPP_LIGHT)) return (p_ptr->res[RS_LIT]);
			else return (0);
		}
		/* Darkness Spells */
		case LRN_DARK:
		{
			if (smart & (SM_OPP_DARK)) return (p_ptr->res[RS_DRK]);
			else return (0);
		}
		/* Psychic Spells */
		case LRN_PSI:
		{
			if (smart & (SM_OPP_PSI)) return (p_ptr->res[RS_PSI]);
			else return (0);
		}
		/* Telekinetic Spells */
		case LRN_TK:
		{
			if (smart & (SM_OPP_TK)) return (p_ptr->res[RS_TLK]);
			else return (0);
		}
		/* Spirit Spells */
		case LRN_SPIRIT:
		{
			if (smart & (SM_OPP_SPIRIT)) return (p_ptr->res[RS_SPI]);
			else return (0);
		}
		/* Spells that attack player mana */
		case LRN_MANA:
		{
			if (smart & (SM_IMM_MANA)) return (100);
			else return (0);
		}
		/* Spells Requiring Save or Resist Fear */
		case LRN_FEAR:
		{
			a = 0;
			if (smart & (SM_IMM_FEAR)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->afraid) a += 50;
			}
			return (a);
		}
		/* Spells Requiring Save or Resist Blindness */
		case LRN_BLIND:
		{
			a = 0;
			if (smart & (SM_IMM_BLIND)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->blind) a += 50;
			}
			return (a);
		}
		/* Spells Requiring Save or Resist Confusion */
		case LRN_CONFU:
		{
			a = 0;
			if (smart & (SM_IMM_CONFU)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->confused) a += 50;
			}
			return (a);
		}

		/* Spells Requiring Save or Free Action */
		case LRN_FREE_SAVE:
		{
			a = 0;
			if (smart & (SM_IMM_FREE)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else if (p_ptr->paralyzed) a = 80;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->slow) a += 50;
			}
			return (a);
		}

		/* Spells Requiring Save  */
		case LRN_SAVE:
		{
			if (smart & (SM_PERF_SAVE)) return (100);
			else if (smart & (SM_GOOD_SAVE)) return (30);
			return (0);
		}

		/* Anything else */
		default:
		{
			return (0);
		}
	}
}


/*
 * Used to exclude spells which are too expensive for the
 * monster to cast.  Excludes all spells that cost more than the
 * current available mana.
 *
 * Smart monsters may also exclude spells that use a lot of mana,
 * even if they have enough.
 *
 * -BR-
 */
void remove_expensive_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i, max_cost;

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);

	/* Determine maximum amount of mana to be spent */
	/* Smart monsters will usually not blow all their mana on one spell */
	if (r_ptr->flags2 & (RF2_SMART)) max_cost=(m_ptr->mana * (rand_range(4, 10))) / 10;
		
	/* Otherwise spend up to the full current mana */
	else max_cost=m_ptr->mana;

	/* check innate spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (mana_cost_RF4[i] > max_cost) f4 &= ~(0x00000001 << i);
	}

	/* check normal spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (mana_cost_RF5[i] > max_cost) f5 &= ~(0x00000001 << i);
	}

	/* check other spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (mana_cost_RF6[i] > max_cost) f6 &= ~(0x00000001 << i);
	}

	/* check other spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (mana_cost_RF7[i] > max_cost) f7 &= ~(0x00000001 << i);
	}

	/* Modify the spell list. */
	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
	(*f7p) = f7;

}

/*
 * Intelligent monsters use this function to filter away spells
 * which have no benefit.
 */
void remove_useless_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p, bool require_los)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);

	/* Don't regain mana if full */
	if (m_ptr->mana >= r_ptr->mana) f6 &= ~(RF6_ADD_MANA);

	/* Don't heal if not enough mana to make it useful */
	if (m_ptr->mana < r_ptr->spell_power/4) f6 &= ~(RF6_HEAL);

	/* Don't heal if full */
	if (m_ptr->hp >= m_ptr->maxhp) f6 &= ~(RF6_HEAL);

	/* Don't Haste if Hasted */
	if (m_ptr->mspeed > r_ptr->speed +5) f6 &= ~(RF6_HASTE);

	/* Don't cure if not needed */
	if (!((m_ptr->stunned) ||(m_ptr->monfear) ||
	      (m_ptr->mspeed < r_ptr->speed - 5)))
		f6 &= ~(RF6_CURE);

	/* Don't jump in already close, or don't want to be close */
	if (!(m_ptr->cdis > m_ptr->best_range) && require_los)
		f6 &= ~(RF6_TELE_SELF_TO);
	if (m_ptr->min_range > 5) f6 &= ~(RF6_TELE_SELF_TO);
	
	/* Modify the spell list. */
	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
	(*f7p) = f7;
}

/*
 * Count the number of castable spells.
 *
 * If exactly 1 spell is available cast it.  If more than more is
 * available, and the random bit is set, pick one.
 *
 * Used as a short cut in 'choose_attack_spell' to circumvent AI
 * when there is only 1 choice. (random=FALSE)
 *
 * Also used in 'choose_attack_spell' to circumvent AI when
 * casting randomly (random=TRUE), as with dumb monsters.
 */
static int choose_attack_spell_fast(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p, bool do_random)
{
	int i, num=0;
	byte spells[128];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);

	/* Extract the "innate" spells */
	for (i = 0; i < 32; i++)
	{
		if (f4 & (1L << i)) spells[num++] = i + 32 * 3;
	}

	/* Extract the "attack" spells */
	for (i = 0; i < 32; i++)
	{
		if (f5 & (1L << i)) spells[num++] = i + 32 * 4;
	}

	/* Extract the "miscellaneous" spells */
	for (i = 0; i < 32; i++)
	{
		if (f6 & (1L << i)) spells[num++] = i + 32 * 5;
	}

	/* Extract the "summon" spells */
	for (i = 0; i < 32; i++)
	{
		if (f7 & (1L << i)) spells[num++] = i + 32 * 6;
	}

	/* Paranoia */
	if (num == 0) return (0);

	/* Go quick if possible */
	if (num == 1)
	{
		/* Hack - Don't cast if known to be immune, unless
		 * casting randomly anyway.  */
		if (!(do_random))
		{
			if (spells[0] < 128)
			{
				if (find_resist(m_idx, spell_desire_RF4[spells[0]-96][D_RES]) == 100) return (0);
			}
			else if (spells[0] < 160)
			{
				if (find_resist(m_idx, spell_desire_RF5[spells[0]-128][D_RES]) == 100) return (0);
			}
			else if (spells[0] < 192)
			{
				if (find_resist(m_idx, spell_desire_RF6[spells[0]-160][D_RES]) == 100) return (0);
			}
			else
			{
				if (find_resist(m_idx, spell_desire_RF7[spells[0]-192][D_RES]) == 100) return (0);
			}
		}

		/* Otherwise cast the one spell */
		else return (spells[0]);
	}

	/*
	 * If we aren't allowed to choose at random
	 * and we have multiple spells left, give up on quick
	 * selection
	 */
	if (!(do_random)) return (0);

	/* Pick at random */
	return (spells[rand_int(num)]);
}

/*
 * Have a monster choose a spell.
 *
 * Monster at m_idx uses this function to select a legal attack spell.
 * Spell casting AI is based here.
 *
 * First the code will try to save time by seeing if
 * choose_attack_spell_fast is helpful.  Otherwise, various AI
 * parameters are used to calculate a 'desirability' for each spell.
 * There is some randomness.  The most desirable spell is cast.
 *
 * archery_only can be used to restrict us to arrow/boulder type attacks.
 *
 * Returns the spell number, of '0' if no spell is selected.
 *
 *-BR-
 */
int choose_ranged_attack(int m_idx, int *tar_y, int *tar_x, bool archery_only, bool pet)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	byte *spell_desire;

	u32b f4, f5, f6, f7;

	byte spell_range;

	bool do_random = FALSE;

	bool require_los = TRUE;

	bool is_harass = FALSE;
	bool is_best_harass = FALSE;
	bool is_breath = FALSE;

	int i, py = p_ptr->py, px = p_ptr->px;
	int breath_hp, breath_maxhp, path, spaces;
	int bx, by;

	int want_hps=0, want_escape=0, want_mana=0, want_summon=0;
	int want_tactic=0, cur_range=0;

	int best_spell=0, best_spell_rating=0;
	int cur_spell_rating;

	/* Extract the racial spell flags */
	/* Hm, what we're going to do here is if the player has anti-magic active */
	/* then we will not extract spell flags for any spells */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;
	f7 = r_ptr->flags7;

	/*default: target the player*/
	if (!pet)
	{
		*tar_y = p_ptr->py;
		*tar_x = p_ptr->px;
	}
	
	bx = *tar_x;
	by = *tar_y;	
	
	/* Check what kinds of spells can hit player */
	path=projectable(m_ptr->fy, m_ptr->fx, by, bx, PROJECT_CHCK);

	/* do we have the player in sight at all? */
	if (path==PROJECT_NO)
	{
		bool clear_ball_spell = TRUE;

		/*are we in range smart or annoyed (and not stupid), and have access to ball spells?*/
		if ((m_ptr->cdis < MAX_RANGE) && 
			((r_ptr->flags2 & (RF2_SMART)) || (r_ptr->flags2 & (RF2_ARCHER))) &&
			 ((r_ptr->flags4 & (RF4_BALL_MASK)) ||
			  (r_ptr->flags5 & (RF5_BALL_MASK)) ||
			  (r_ptr->flags6 & (RF6_BALL_MASK)) ||
			  (r_ptr->flags7 & (RF7_BALL_MASK))))
		{

			int alt_y, alt_x, alt_path, best_y, best_x, best_path;

			/*start with no alternate shot*/
			best_y =  best_x = best_path  = 0;

			/* Check for impassable terrain */
			for (i = 0; i < 8; i++)
			{
				alt_y = by + ddy_ddd[i];
				alt_x = bx + ddx_ddd[i];

				alt_path = projectable(m_ptr->fy, m_ptr->fx, alt_y, alt_x, PROJECT_CHCK);

				if (alt_path == PROJECT_NO) continue;

				if (alt_path == PROJECT_NOT_CLEAR)
				{
					/* if (!similar_monsters(m_ptr->fy, m_ptr->fx, alt_y, alt_x)) continue; */


					/*we already have a NOT_CLEAR path*/
					/* if ((best_path == PROJECT_NOT_CLEAR) && (one_in_(2))) continue; */
					if (best_path == PROJECT_NOT_CLEAR) continue;
				}

				/*
			 	 * PROJECT_CLEAR, or monster has an
			 	 * empty square to lob a ball spell at player
			  	 */
				best_y = alt_y;
				best_x = alt_x;
				best_path = alt_path;
				/*we want to keep ball spells*/
				clear_ball_spell = FALSE;

				if (best_path == PROJECT_CLEAR) break;
			}

			if (best_y + best_x > 0)
			{
				/*default: target the player*/
				*tar_y = best_y;
				*tar_x = best_x;
			}

		}

		/*We don't have a reason to try a ball spell*/
		if (clear_ball_spell)
		{
			f4 &= ~(RF4_BALL_MASK);
			f5 &= ~(RF5_BALL_MASK);
			f6 &= ~(RF6_BALL_MASK);
			f7 &= ~(RF7_BALL_MASK);
		}

		/* Flat out 75% chance of not casting if the player is not in sight */
		/* In addition, most spells don't work without a player around */
		if (!one_in_(4)) return (0);

		require_los=FALSE;
	}

	
	/* Are we restricted to archery? */
	/* Note - we have assumed for speed that no archery attacks
	 * cost mana, all are 'bolt' like, and that the player can not
	 * be highly resistant to them. */
	if (archery_only)
	{
		/* check for a clean shot */
		if (!(path == PROJECT_CLEAR))
		{
			/* restrict to ball archery spells */
			f4 &= (RF4_ARCHERY_MASK);
			f5 &= (RF5_ARCHERY_MASK);
			f6 &= (RF6_ARCHERY_MASK);
			f7 &= (RF7_ARCHERY_MASK);
			
			f4 &= ~(RF4_BASIC_ARCHERY_MASK);
			f5 &= ~(RF5_BASIC_ARCHERY_MASK);
			f6 &= ~(RF6_BASIC_ARCHERY_MASK);
			f7 &= ~(RF7_BASIC_ARCHERY_MASK);
			
		} 

		else
		{
			/* restrict to archery */
			f4 &= (RF4_ARCHERY_MASK);
			f5 &= (RF5_ARCHERY_MASK);
			f6 &= (RF6_ARCHERY_MASK);
			f7 &= (RF7_ARCHERY_MASK);
		}
		
		/* choose at random from restricted list */
		return (choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, TRUE));
	}

	/* Remove spells the 'no-brainers'*/
	/* Spells that require LOS */
	if (!require_los)
	{
		f4 &= (RF4_NO_PLAYER_MASK);
		f5 &= (RF5_NO_PLAYER_MASK);
		f6 &= (RF6_NO_PLAYER_MASK);
		f7 &= (RF7_NO_PLAYER_MASK);
	}
	else if (path==PROJECT_NOT_CLEAR)
	{
		f4 &= ~(RF4_BOLT_MASK);
		f5 &= ~(RF5_BOLT_MASK);
		f6 &= ~(RF6_BOLT_MASK);
		f7 &= ~(RF7_BOLT_MASK);
	}

	if (p_ptr->anti_magic)
	{
		f4 &= (RF4_ANTI_MAGIC_MASK);
		f5 &= (RF5_ANTI_MAGIC_MASK);
		f6 &= (RF6_ANTI_MAGIC_MASK);
		f7 &= (RF7_ANTI_MAGIC_MASK);		
	}

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7) return (0);

	/* Spells we can not afford */
	remove_expensive_spells(m_idx, &f4, &f5, &f6, &f7);

	/* Don't lash if too far or close */
	if ((m_ptr->cdis > 3) || (m_ptr->cdis < 2)) f4 &= ~(RF4_LASH);

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7) return (0);

	/* Stupid monsters choose at random. */
	if (r_ptr->flags2 & (RF2_STUPID)) return (choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, TRUE));

	/* Remove spells that have no benefit
	 * Does not include the effects of player resists/immunities */
	remove_useless_spells(m_idx, &f4, &f5, &f6, &f7, require_los);

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7) return (0);

	/* Sometimes non-dumb monsters cast randomly (though from the
	 * restricted list)
	 */
	if ((r_ptr->flags2 & (RF2_SMART)) && (one_in_(10))) do_random = TRUE;
	if ((!(r_ptr->flags2 & (RF2_SMART))) && (one_in_(5))) do_random = TRUE;

	/* Try 'fast' selection first.
	 * If there is only one spell, choose that spell.
	 * If there are multiple spells, choose one randomly if the 'random' flag is set.
	 * Otherwise fail, and let the AI choose.
	 */
	best_spell=choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, do_random);
	if (best_spell) return (best_spell);

	/* If we get this far, we are using the full-up AI.  Calculate
	   some parameters. */

	/* Figure out if we are hurt */
	if (m_ptr->hp < m_ptr->maxhp/8) want_hps += 3;
	else if (m_ptr->hp < m_ptr->maxhp/4) want_hps += 2;
	else if (m_ptr->hp < m_ptr->maxhp/2) want_hps++;

	/* Figure out if we want mana */
	if (m_ptr->mana < r_ptr->mana/4) want_mana +=2;
	else if (m_ptr->mana < r_ptr->mana/2) want_mana++;

	/* Figure out if we want to scram */
	if (want_hps) want_escape = want_hps - 1;
	if (m_ptr->min_range == FLEE_RANGE) want_escape++;

	/* Desire to keep minimum distance */
	if (m_ptr->cdis < m_ptr->best_range) want_tactic++;
	if (m_ptr->cdis < m_ptr->min_range)
		want_tactic += (m_ptr->min_range - m_ptr->cdis + 1) / 2;
	if (want_tactic > 3) want_tactic=3;

	/* Check terrain for purposes of summoning spells */
	spaces=summon_possible(py,px);
	if (spaces > 10) want_summon=3;
	else if (spaces > 3) want_summon=2;
	else if (spaces > 0) want_summon=1;

	/* Find monster properties; Add an offset so that things are OK near zero */
	breath_hp = (m_ptr->hp > 2000 ? m_ptr->hp : 2000);
	breath_maxhp = (m_ptr->maxhp > 2000 ? m_ptr->maxhp : 2000);

	/* Cheat if requested */
	if (smart_cheat)
	{
		update_smart_cheat(m_idx);
	}

	/* The conditionals are written for speed rather than readability
	 * They should probably stay that way. */
	for (i = 0; i < 128; i++)
	{
		/* Do we even have this spell? */
		if (i < 32)
		{
			if (!(f4 &(1L <<  i    ))) continue;
			spell_desire=&spell_desire_RF4[i][0];
			spell_range=spell_range_RF4[i];
			if (RF4_HARASS_MASK &(1L << (i   ))) is_harass=TRUE;
			else is_harass=FALSE;
			if (RF4_BREATH_MASK &(1L << (i   ))) is_breath=TRUE;
			else is_breath=FALSE;
		}
		else if (i < 64)
		{
			if (!(f5 &(1L << (i-32)))) continue;
			spell_desire=&spell_desire_RF5[i-32][0];
			spell_range=spell_range_RF5[i-32];
			if (RF5_HARASS_MASK &(1L << (i-32))) is_harass=TRUE;
			else is_harass=FALSE;
			if (RF5_BREATH_MASK &(1L << (i-32))) is_breath=TRUE;
			else is_breath=FALSE;
		}
		else if (i < 96)
		{
			if (!(f6 &(1L << (i-64)))) continue;
			spell_desire=&spell_desire_RF6[i-64][0];
			spell_range=spell_range_RF6[i-64];
			if (RF6_HARASS_MASK &(1L << (i-64))) is_harass=TRUE;
			else is_harass=FALSE;
			if (RF6_BREATH_MASK &(1L << (i-64))) is_breath=TRUE;
			else is_breath=FALSE;
		}
		else
		{
			if (!(f7 &(1L << (i-96)))) continue;
			spell_desire=&spell_desire_RF7[i-96][0];
			spell_range=spell_range_RF7[i-96];
			if (RF7_HARASS_MASK &(1L << (i-96))) is_harass=TRUE;
			else is_harass=FALSE;
			if (RF7_BREATH_MASK &(1L << (i-96))) is_breath=TRUE;
			else is_breath=FALSE;
		}

		/* Base Desirability */
		cur_spell_rating = spell_desire[D_BASE];

		/* modified for breath weapons */
		if (is_breath) cur_spell_rating = (cur_spell_rating * breath_hp) / breath_maxhp;

		/* Bonus if want summon and this spell is helpful */
		if (spell_desire[D_SUMM] && want_summon) cur_spell_rating +=
						      want_summon * spell_desire[D_SUMM];

		/* Bonus if wounded and this spell is helpful */
		if (spell_desire[D_HURT] && want_hps) cur_spell_rating +=
							want_hps * spell_desire[D_HURT];

		/* Bonus if low on mana and this spell is helpful */
		if (spell_desire[D_MANA] && want_mana) cur_spell_rating +=
							 want_mana * spell_desire[D_MANA];

		/* Bonus if want to flee and this spell is helpful */
		if (spell_desire[D_ESC] && want_escape) cur_spell_rating +=
							  want_escape * spell_desire[D_ESC];

		/* Bonus if want a tactical move and this spell is helpful */
		if (spell_desire[D_TACT] && want_tactic) cur_spell_rating +=
							   want_tactic * spell_desire[D_TACT];

		/* Penalty if this spell is resisted */
		if (spell_desire[D_RES])
		      cur_spell_rating = (cur_spell_rating * (100 - find_resist(m_idx, spell_desire[D_RES])))/100;

		/* Penalty for range if attack drops off in power */
		if (spell_range)
		{
			cur_range = m_ptr->cdis;
			while (cur_range-- > spell_range)
				cur_spell_rating = (cur_spell_rating * spell_desire[D_RANGE])/100;
		}

		/* Bonus for harassment spells at first */
		if (is_harass && m_ptr->harass) cur_spell_rating += 2*cur_spell_rating/3;

		/* Random factor; less random for smart monsters */
		if (r_ptr->flags2 & (RF2_SMART)) cur_spell_rating *= 16 + rand_int(9);
		else cur_spell_rating *= 12 + rand_int(17);

		/* Deflate for testing purposes */
		cur_spell_rating /= 20;

		/* Is this the best spell yet? */
		if (cur_spell_rating > best_spell_rating)
		{
			best_spell_rating = cur_spell_rating;
			best_spell = i + 96;
			is_best_harass = is_harass;
		}
	}

	if (p_ptr->wizard)
	{
		msg_format("Spell rating: %i.", best_spell_rating);
	}

	/* If we used a harassment spell, lower the bias to use them early */
	if (is_best_harass && m_ptr->harass) m_ptr->harass--;

	/* Return Best Spell */
	return (best_spell);
}


/*
 * Can the monster exist in this grid?
 *
 * Because this function is designed for use in monster placement and
 * generation as well as movement, it cannot accept monster-specific
 * data, but must rely solely on racial information.
 */
bool cave_exist_mon(monster_race *r_ptr, int y, int x, bool occupied_ok,
	bool can_dig)
{
	int feat;

	/* Check Bounds */
	if (!in_bounds(y, x)) return (FALSE);

	/* Check location */
	feat = cave_feat[y][x];

	/* The grid is already occupied. */
	if (cave_m_idx[y][x] != 0)
	{
		if (!occupied_ok) return (FALSE);
	}

	/* Glyphs -- must break first */
	if (feat == FEAT_GLYPH) return (FALSE);


	/*** Check passability of various features. ***/

	/* Feature is not a wall */
	if (!(cave_info[y][x] & (CAVE_WALL)))
	{
		/* Floor -- safe for everything */
		if (feat == FEAT_FLOOR) return (TRUE);

		/* Anything else that's not a wall we assume to be legal. */
		return (TRUE);
		/* Code for monsters to not stand terrain */
	}


	/* Feature is a wall */
	else
	{
		/* Rubble is always OK */
		/* if (feat == FEAT_RUBBLE) return (TRUE); */

		/* Permanent walls are never OK */
		if ((feat >= FEAT_PERM_EXTRA) && (feat <= FEAT_PERM_SOLID))
			return (FALSE);

		/* Monster can pass through walls */
		if (r_ptr->flags2 & (RF2_PASS_WALL)) return (TRUE);

		/* Monster can dig through walls, and is allowed to. */
		if ((r_ptr->flags2 & (RF2_KILL_WALL)) && (can_dig)) return (TRUE);

		else return (FALSE);
	}

	/* Catch weirdness */
	return (FALSE);
}


/*
 * Can the monster enter this grid?  How easy is it for them to do so?
 *
 * The code that uses this function sometimes assumes that it will never
 * return a value greater than 100.
 *
 * The usage of exp to determine whether one monster can kill another is
 * a kludge.  Maybe use HPs, plus a big bonus for acidic monsters
 * against monsters that don't like acid.
 *
 * The usage of exp to determine whether one monster can push past
 * another is also a tad iffy, but ensures that black orcs can always
 * push past other black orcs.
 */
static int cave_passable_mon(monster_type *m_ptr, int y, int x, bool *bash)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Assume nothing in the grid other than the terrain hinders movement */
	int move_chance = 100;

	int feat;

	/* Check Bounds */
	if (!in_bounds(y, x)) return (FALSE);

	/* Check location */
	feat = cave_feat[y][x];

	/* The grid is occupied by the player. */
	if (cave_m_idx[y][x] < 0)
	{
		/* Monster has no melee blows - character's grid is off-limits. */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (0);

		/* Any monster with melee blows can attack the character. */
		else move_chance = 100;
	}

	/* The grid is occupied by a monster. */
	else if (cave_m_idx[y][x] > 0)
	{
		monster_type *n_ptr = &m_list[cave_m_idx[y][x]];
		monster_race *nr_ptr = &r_info[n_ptr->r_idx];

		/* Kill weaker monsters */
		if (((r_ptr->flags2 & (RF2_KILL_BODY)) &&
		    (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
		    (r_ptr->mexp > nr_ptr->mexp)) ||
			are_enemies(m_ptr, n_ptr) ||
			m_ptr->confused)
		{
			move_chance = 100;
		}

		/* Push past weaker or similar monsters */
		else if (r_ptr->mexp >= nr_ptr->mexp)
		{
			/* It's easier to push past weaker monsters */
			if (r_ptr->mexp == nr_ptr->mexp) move_chance = 40;
			else move_chance = 80;
		}

		/* Cannot do anything to clear away the other monster */
		else return (0);
	}

	/* Glyphs */
	if (feat == FEAT_GLYPH)
	{
		/* Glyphs are hard to break */
		/* Maybe too hard. Keep an eye on this */
		if (move_chance > 100 * r_ptr->level / BREAK_GLYPH)
			 move_chance = 100 * r_ptr->level / BREAK_GLYPH;
	}


	/*** Check passability of various features. ***/

	/* Feature is not a wall */
	if (!(cave_info[y][x] & (CAVE_WALL)))
	{
		/* Floor */
		if (feat == FEAT_FLOOR)
		{
			/* Any monster can handle floors */
			return (move_chance);
		}

		/* Anything else that's not a wall we assume to be passable. */
		else return (move_chance);
	}


	/* Feature is a wall */
	else
	{
		/* Can the monster move easily through walls? */
		bool move_wall = FALSE;
		if ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
		    (r_ptr->flags2 & (RF2_KILL_WALL)))
		{
			move_wall = TRUE;
		}

		/* Standard dungeon granite and seams */
		if ((feat >= FEAT_MAGMA) && (feat <= FEAT_WALL_SOLID))
		{
			/* Impassible except for monsters that move through walls */
			/* If the player is in the wall, the monster can attack */
			if (move_wall) return (move_chance);
			else if (cave_m_idx[y][x] < 0) return (move_chance);
			else return (0);
		}

		/* Permanent walls are never passable */
		if ((feat >= FEAT_PERM_EXTRA) && (feat <= FEAT_PERM_SOLID))
			return (0);

		/* Doors */
		if (((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL)) ||
		     (feat == FEAT_SECRET))
		{
			int unlock_chance = 0;
			int bash_chance = 0;

			/* Monster can open doors */
			if (r_ptr->flags2 & (RF2_OPEN_DOOR))
			{
				/* Closed doors and secret doors */
				if ((feat == FEAT_DOOR_HEAD) || (feat == FEAT_SECRET))
				{
					/*
					 * Note:  This section will have to be rewritten if
					 * secret doors can be jammed or locked as well.
					 */


					/*
					 * It usually takes two turns to open a door
					 * and move into the doorway.
					 */
					return (MIN(50, move_chance));
				}

				/*
				 * Locked doors (not jammed).  Monsters know how hard
				 * doors in their neighborhood are to unlock.
				 */
				else if (feat < FEAT_DOOR_HEAD + 0x08)
				{
					int lock_power, ability;

					/* Door power (from 35 to 245) */
					lock_power = 35 * (feat - FEAT_DOOR_HEAD);

					/* Calculate unlocking ability (usu. 11 to 200) */
					ability = (r_ptr->level * 2) + 10;
					if (r_ptr->flags2 & (RF2_SMART)) ability *= 2;
					if (strchr("ph", r_ptr->d_char))
						ability = 3 * ability / 2;

					/*
					 * Chance varies from 5% to over 100%.  XXX XXX --
					 * we ignore the fact that it takes extra time to
					 * open the door and walk into the entranceway.
					 */
					unlock_chance = (MAX(5, (100 * ability / lock_power)));
				}
			}

			/* Monster can bash doors */
			if (r_ptr->flags2 & (RF2_BASH_DOOR))
			{
				int door_power, bashing_power;

				/* Door power (from 60 to 420) */
				/*
				 * XXX - just because a door is difficult to unlock
				 * shouldn't mean that it's hard to bash.  Until the
				 * character door bashing code is changed, however,
				 * we'll stick with this.
				 */
				door_power = 60 + 60 * ((feat - FEAT_DOOR_HEAD) % 8);

				/*
				 * Calculate bashing ability (usu. 21 to 300).  Note:
				 * This formula assumes Oangband-style HPs.
				 */
				bashing_power = 20 + r_ptr->level + m_ptr->hp / 15;

				/* if ( || (r_ptr->flags3 & (RF3_TROLL))) */
				 
				if (r_ptr->flags3 & (RF3_BEASTMAN)) bashing_power = 3 * bashing_power / 2;
 				 
 				 
				/*
				 * Chance varies from 2% to over 100%.  Note that
				 * monsters "fall" into the entranceway in the same
				 * turn that they bash the door down.
				 */
				bash_chance = (MAX(2, (100 * bashing_power / door_power)));
			}

			/*
			 * A monster cannot both bash and unlock a door in the same
			 * turn.  It needs to pick one of the two methods to use.
			 */
			if (unlock_chance > bash_chance) *bash = FALSE;
			else *bash = TRUE;

			return (MIN(move_chance, (MAX(unlock_chance, bash_chance))));
		}

		/* Rubble */
		if (feat == FEAT_RUBBLE)
		{
			/* Some monsters move easily through rubble */
			if ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
				(r_ptr->flags2 & (RF2_KILL_WALL)))
			{
				return (move_chance);
			}

			/* For most monsters, rubble is impassible. */
			else return (0);
		}

		/* Any wall grid that isn't explicitly made passable is impassible. */
		return (0);
	}

	/* Catch weirdness */
	return (0);
}


/*
 * Get a target for a monster using the special "townsman" AI.
 */
static void get_town_target(monster_type *m_ptr)
{
	int i, feat;
	int y, x;


	/* Clear target */
	m_ptr->ty = 0;
	m_ptr->tx = 0;

	/* Hack -- Usually choose a random store */
	if (!one_in_(3))
	{
		i = FEAT_SHOP_HEAD + rand_int(MAX_STORES);

		/* Try to find the store XXX XXX */
		for (y = 1; y < TOWN_HGT - 1; y++)
		{
			for (x = 1; x < TOWN_WID - 1; x++)
			{
				feat = cave_feat[y][x];

				/* Is our store */
				if (feat == i)
				{
					m_ptr->ty = y;
					m_ptr->tx = x;
					break;
				}
			}

			/* Store found */
			if (m_ptr->ty) return;
		}
	}

	/* No store chosen */
	if (!m_ptr->ty)
	{
		for (i = 0;; i++)
		{
			/* Pick a grid on the edge of the map (simple test) */
			if (i < 100)
			{
				if (one_in_(2))
				{
					/* Pick a random location along the N/S walls */
					x = rand_range(1, TOWN_WID - 1);

					if (one_in_(2)) y = TOWN_HGT - 2;
					else            y = 2;
				}
				else
				{
					/* Pick a random location along the E/W walls */
					y = rand_range(1, TOWN_HGT - 1);

					if (one_in_(2)) x = TOWN_WID - 2;
					else            x = 2;
				}
			}
			else
			{
				y = rand_range(1, TOWN_HGT - 2);
				x = rand_range(1, TOWN_WID - 2);
			}

			/* Require "empty" floor grids */
			if (cave_empty_bold(y, x))
			{
				m_ptr->ty = y;
				m_ptr->tx = x;
				break;
			}
		}
	}
}


/*
 * Helper function for monsters that want to advance toward the character.
 * Assumes that the monster isn't frightened, and is not in LOS of the
 * character.
 *
 * Ghosts and rock-eaters do not use flow information, because they
 * can - in general - move directly towards the character.  We could make
 * them look for a grid at their preferred range, but the character
 * would then be able to avoid them better (it might also be a little
 * hard on those poor warriors...).
 *
 * Other monsters will use target information, then their ears, then their
 * noses (if they can), and advance blindly if nothing else works.
 *
 * When flowing, monsters prefer non-diagonal directions.
 *
 * XXX - At present, this function does not handle difficult terrain
 * intelligently.  Monsters using flow may bang right into a door that
 * they can't handle.  Fixing this may require code to set monster
 * paths.
 */
static void get_move_advance(monster_type *m_ptr, int *ty, int *tx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, y1, x1;

	int lowest_cost = 250;

	bool can_use_sound = FALSE;
	bool can_use_scent = FALSE;
	bool impassable = FALSE;
	bool dummy;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];


	/* Monster can go through rocks */
	if ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
	    (r_ptr->flags2 & (RF2_KILL_WALL)))
	{
		/* Check for impassable terrain */
		for (i = 0; i < 8; i++)
		{
			y = m_ptr->fy + ddy_ddd[i];
			x = m_ptr->fx + ddx_ddd[i];

			if (cave_passable_mon(m_ptr, y, x, &dummy) == 0)
			{
				impassable = TRUE;
				break;
			}
		}

		/* Usually head straight for character */
		if (!impassable)
		{
			*ty = py;
			*tx = px;
			return;
		}
	}

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* Use target information if available */
	if ((m_ptr->ty) && (m_ptr->tx))
	{
		*ty = m_ptr->ty;
		*tx = m_ptr->tx;
		return;
	}

	/* If we can hear noises, advance towards them */
	if (cave_cost[y1][x1])
	{
		can_use_sound = TRUE;
	}

	/* Otherwise, try to follow a scent trail */
	else if (monster_can_smell(m_ptr))
	{
		can_use_scent = TRUE;
	}

	/* Otherwise, advance blindly */
	if ((!can_use_sound) && (!can_use_scent))
	{
		*ty = py;
		*tx = px;
		return;
	}

	/* Using flow information.  Check nearby grids, diagonals first. */
	for (i = 7; i >= 0; i--)
	{
		/* Get the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Check Bounds */
		if (!in_bounds(y, x)) continue;

		/* We're following a scent trail */
		if (can_use_scent)
		{
			int age = get_scent(y, x);
			if (age == -1) continue;

			/* Accept younger scent */
			if (lowest_cost < age) continue;
			lowest_cost = age;
		}

		/* We're using sound */
		else
		{
			int cost = cave_cost[y][x];

			/* Accept louder sounds */
			if ((cost == 0) || (lowest_cost < cost)) continue;
			lowest_cost = cost;
		}

		/* Save the location */
		*ty = y;
		*tx = x;
	}
}


/*
 * "Do not be seen."
 *
 * Monsters in LOS that want to retreat are primarily interested in
 * finding a nearby place that the character can't see into.
 * Search for such a place with the lowest cost to get to up to 15
 * grids away.
 *
 * Look outward from the monster's current position in a square-
 * shaped search pattern.  Calculate the approximate cost in monster
 * turns to get to each passable grid, using a crude route finder.  Penal-
 * ize grids close to or approaching the character.  Ignore hiding places
 * with no safe exit.  Once a passable grid is found that the character
 * can't see, the code will continue to search a little while longer,
 * depending on how pricey the first option seemed to be.
 *
 * If the search is successful, the monster will target that grid,
 * and (barring various special cases) run for it until it gets there.
 *
 * We use a limited waypoint system (see function "get_route_to_target()"
 * to reduce the likelihood that monsters will get stuck at a wall between
 * them and their target (which is kinda embarrassing...).
 *
 * This function does not yield perfect results; it is known to fail
 * in cases where the previous code worked just fine.  The reason why
 * it is used is because its failures are less common and (usually)
 * less embarrassing than was the case before.  In particular, it makes
 * monsters great at not being seen.
 *
 * This function is fairly expensive.  Call it only when necessary.
 */
static bool find_safety(monster_type *m_ptr, int *ty, int *tx)
{
	int i, j, d;

	/* Scanning range for hiding place search. */
	byte scan_range = 15;

	int y, x, yy, xx;

	int countdown = scan_range;

	int least_cost = 100;
	int least_cost_y = 0;
	int least_cost_x = 0;
	int chance, cost, parent_cost;
	bool dummy;

	/* Factors for converting table to actual dungeon grids */
	int conv_y, conv_x;

	/*
	 * Allocate and initialize a table of movement costs.
	 * Both axis must be (2 * scan_range + 1).
	 */
	byte safe_cost[31][31];

	for (i = 0; i < 31; i++)
	{
		for (j = 0; j < 31; j++)
		{
			safe_cost[i][j] = 0;
		}
	}

	conv_y = scan_range - m_ptr->fy;
	conv_x = scan_range - m_ptr->fx;

	/* Mark the origin */
	safe_cost[scan_range][scan_range] = 1;

	/* If the character's grid is in range, mark it as being off-limits */
	if ((ABS(m_ptr->fy - p_ptr->py) <= scan_range) &&
	    (ABS(m_ptr->fx - p_ptr->px) <= scan_range))
	{
		safe_cost[p_ptr->py + conv_y][p_ptr->px + conv_x] = 100;
	}

	/* Work outward from the monster's current position */
	for (d = 0; d < scan_range; d++)
	{
		for (y = scan_range - d; y <= scan_range + d; y++)
		{
			for (x = scan_range - d; x <= scan_range + d;)
			{
				int x_tmp;

				/*
				 * Scan all grids of top and bottom rows, just
				 * outline other rows.
				 */
				if ((y != scan_range - d) && (y != scan_range + d))
				{
					if (x == scan_range + d) x_tmp = 999;
					else x_tmp = scan_range + d;
				}
				else x_tmp = x + 1;

				/* Grid and adjacent grids must be legal */
				if (!in_bounds_fully(y - conv_y, x - conv_x))
				{
					x = x_tmp;
					continue;
				}

				/* Grid is inaccessible (or at least very difficult to enter) */
				if ((safe_cost[y][x] == 0) || (safe_cost[y][x] >= 100))
				{
					x = x_tmp;
					continue;
				}

				/* Get the accumulated cost to enter this grid */
				parent_cost = safe_cost[y][x];

				/* Scan all adjacent grids */
				for (i = 0; i < 8; i++)
				{
					yy = y + ddy_ddd[i];
					xx = x + ddx_ddd[i];

					/* check bounds */
					if ((yy < 0) || (yy > 30) || (xx < 0) || (xx > 30)) continue;

					/*
					 * Handle grids with empty cost and passable grids
					 * with costs we have a chance of beating.
					 */
					if ((safe_cost[yy][xx] == 0) ||
					      ((safe_cost[yy][xx] > parent_cost + 1) &&
					       (safe_cost[yy][xx] < 100)))
					{
						/* Get the cost to enter this grid */
						chance = cave_passable_mon(m_ptr, yy - conv_y,
						         xx - conv_x, &dummy);

						/* Impassable */
						if (!chance)
						{
							/* Cannot enter this grid */
							safe_cost[yy][xx] = 100;
							continue;
						}

						/* Calculate approximate cost (in monster turns) */
						cost = 100 / chance;

						/* Next to character */
						if (distance(yy - conv_y, xx - conv_x,
						    p_ptr->py, p_ptr->px) <= 1)
						{
							/* Don't want to maneuver next to the character */
							cost += 3;
						}

						/* Mark this grid with a cost value */
						safe_cost[yy][xx] = parent_cost + cost;

						/* Character can't see this grid */
						if (!player_can_see_bold(yy - conv_y, xx - conv_x))
						{
							int this_cost = safe_cost[yy][xx];

							/* Penalize grids that approach character */
							if (ABS(p_ptr->py - (yy - conv_y)) <
							    ABS(m_ptr->fy - (yy - conv_y)))
							{
								 this_cost *= 2;
							}
							if (ABS(p_ptr->px - (xx - conv_x)) <
							    ABS(m_ptr->fx - (xx - conv_x)))
							{
								 this_cost *= 2;
							}

							/* Accept lower-cost, sometimes accept same-cost options */
							if ((least_cost > this_cost) ||
							    (least_cost == this_cost && one_in_(2)))
							{
								bool has_escape = FALSE;

								/* Scan all adjacent grids for escape routes */
								for (j = 0; j < 8; j++)
								{
									/* Calculate real adjacent grids */
									int yyy = yy - conv_y + ddy_ddd[i];
									int xxx = xx - conv_x + ddx_ddd[i];

									/* Check bounds */
									if (!in_bounds(yyy, xxx)) continue;

									/* Look for any passable grid that isn't in LOS */
									if ((!player_can_see_bold(yyy, xxx)) &&
									    (cave_passable_mon(m_ptr, yyy, xxx, &dummy)))
									{
										/* Not a one-grid cul-de-sac */
										has_escape = TRUE;
										break;
									}
								}

								/* Ignore cul-de-sacs */
								if (has_escape == FALSE) continue;

								least_cost = this_cost;
								least_cost_y = yy;
								least_cost_x = xx;

								/*
								 * Look hard for alternative hiding places if
								 * this one seems pricey.
								 */
								countdown = 1 + least_cost - d;
							}
						}
					}
				}

				/* Adjust x as instructed */
				x = x_tmp;
			}
		}

		/*
		 * We found a good place a while ago, and haven't done better
		 * since, so we're probably done.
		 */
		if (countdown-- == 0) break;
	}

	/* We found a place that can be reached in reasonable time */
	if (least_cost < 50)
	{
		/* Convert to actual dungeon grid. */
		y = least_cost_y - conv_y;
		x = least_cost_x - conv_x;

		/* Move towards the hiding place */
		*ty = y;
		*tx = x;

		/* Target the hiding place */
		m_ptr->ty = y;
		m_ptr->tx = x;

		return (TRUE);
	}


	/* No good place found */
	return (FALSE);
}


/*
 * Helper function for monsters that want to retreat from the character.
 * Used for any monster that is terrified, frightened, is looking for a
 * temporary hiding spot, or just wants to open up some space between it
 * and the character.
 *
 * If the monster is well away from danger, let it relax.
 * If the monster's current target is not in LOS, use it (+).
 * If the monster is not in LOS, and cannot pass through walls, try to
 * use flow (noise) information.
 * If the monster is in LOS, even if it can pass through walls,
 * search for a hiding place (helper function "find_safety()"):
 * search for a hiding place (helper function "find_safety()").
 * If no hiding place is found, and there seems no way out, go down
 * fighting.
 *
 * If none of the above solves the problem, run away blindly.
 *
 * (+) There is one exception to the automatic usage of a target.  If the
 * target is only out of LOS because of "knight's move" rules (distance
 * along one axis is 2, and along the other, 1), then the monster will try
 * to find another adjacent grid that is out of sight.  What all this boils
 * down to is that monsters can now run around corners properly!
 *
 * Return TRUE if the monster did actually want to do anything.
 */
static bool get_move_retreat(monster_type *m_ptr, int *ty, int *tx)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i;
	int y, x;

	bool done = FALSE;
	bool dummy;


	/* If the monster is well away from danger, let it relax. */
	if (m_ptr->cdis >= FLEE_RANGE)
	{
		return (FALSE);
	}

	/* Monster has a target */
	if ((m_ptr->ty) && (m_ptr->tx))
	{
		/* It's out of LOS; keep using it, except in "knight's move" cases */
		if (!player_has_los_bold(m_ptr->ty, m_ptr->tx))
		{
			/* Get axis distance from character to current target */
			int dist_y = ABS(p_ptr->py - m_ptr->ty);
			int dist_x = ABS(p_ptr->px - m_ptr->tx);

			/* It's only out of LOS because of "knight's move" rules */
			if (((dist_y == 2) && (dist_x == 1)) ||
			    ((dist_y == 1) && (dist_x == 2)))
			{
				/*
				 * If there is another grid adjacent to the monster that
				 * the character cannot see into, and it isn't any harder
				 * to enter, use it instead.  Prefer diagonals.
				 */
				for (i = 7; i >= 0; i--)
				{
					y = m_ptr->fy + ddy_ddd[i];
					x = m_ptr->fx + ddx_ddd[i];

					/* Check Bounds */
					if (!in_bounds(y, x)) continue;

					if (player_has_los_bold(y, x)) continue;

					if ((y == m_ptr->ty) && (x == m_ptr->tx)) continue;

					if (cave_passable_mon(m_ptr, m_ptr->ty, m_ptr->tx, &dummy) >
					    cave_passable_mon(m_ptr, y, x, &dummy)) continue;

					m_ptr->ty = y;
					m_ptr->tx = x;
					break;
				}
			}

			/* Move towards the target */
			*ty = m_ptr->ty;
			*tx = m_ptr->tx;
			return (TRUE);
		}

		/* It's in LOS; cancel it. */
		else
		{
			m_ptr->ty = 0;
			m_ptr->tx = 0;
		}
	}

	/* The monster is not in LOS, but thinks it's still too close. */
	if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
	{
		/* Monster cannot pass through walls */
		if (!((r_ptr->flags2 & (RF2_PASS_WALL)) ||
		      (r_ptr->flags2 & (RF2_KILL_WALL))))
		{
			/* Run away from noise */
			if (cave_cost[m_ptr->fy][m_ptr->fx])
			{
				int start_cost = cave_cost[m_ptr->fy][m_ptr->fx];

				/* Look at adjacent grids, diagonals first */
				for (i = 7; i >= 0; i--)
				{
					y = m_ptr->fy + ddy_ddd[i];
					x = m_ptr->fx + ddx_ddd[i];

					/* Check bounds */
					if (!in_bounds(y, x)) continue;

					/* Accept the first non-visible grid with a higher cost */
					if (cave_cost[y][x] > start_cost)
					{
						if (!player_has_los_bold(y, x))
						{
							*ty = y;  *tx = x;
							done = TRUE;
							break;
						}
					}
				}

				/* Return if successful */
				if (done) return (TRUE);
			}
		}

		/* No flow info, or don't need it -- see bottom of function */
	}

	/* The monster is in line of sight. */
	else
	{
		int prev_cost = cave_cost[m_ptr->fy][m_ptr->fx];
		int start = rand_int(8);

		/* Look for adjacent hiding places */
		for (i = start; i < 8 + start; i++)
		{
			y = m_ptr->fy + ddy_ddd[i % 8];
			x = m_ptr->fx + ddx_ddd[i % 8];

			/* Check Bounds */
			if (!in_bounds(y, x)) continue;

			/* No grids in LOS */
			if (player_has_los_bold(y, x)) continue;

			/* Grid must be pretty easy to enter */
			if (cave_passable_mon(m_ptr, y, x, &dummy) < 50) continue;

			/* Accept any grid that doesn't have a lower flow (noise) cost. */
			if (cave_cost[y][x] >= prev_cost)
			{
				*ty = y;
				*tx = x;
				prev_cost = cave_cost[y][x];

				/* Success */
				return (TRUE);
			}
		}

		/* Find a nearby grid not in LOS of the character. */
		if (find_safety(m_ptr, ty, tx) == TRUE) return (TRUE);

		/*
		 * No safe place found.  If monster is in LOS and close,
		 * it will turn to fight.
		 */
		if ((player_has_los_bold(m_ptr->fy, m_ptr->fx)) &&
		    (m_ptr->cdis < TURN_RANGE) && !(is_pet(m_ptr)))
		{
			/* Turn and fight */
			set_mon_fear(m_ptr, 0, FALSE);

			/* Forget target */
			m_ptr->ty = 0;    m_ptr->tx = 0;

			/* Charge!  XXX XXX */
			m_ptr->min_range = 1;  m_ptr->best_range = 1;

			/* Visible */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s turns to fight!", m_name);
			}

			/* Charge! */
			*ty = p_ptr->py;
			*tx = p_ptr->px;
			return (TRUE);
		}
	}

	/* Move directly away from character. */
	*ty = -(p_ptr->py - m_ptr->fy);
	*tx = -(p_ptr->px - m_ptr->fx);

	/* We want to run away */
	return (TRUE);
}


/*
 * Choose the probable best direction for a monster to move in.  This
 * is done by choosing a target grid and then finding the direction that
 * best approaches it.
 *
 * Monsters that cannot move always attack if possible.
 * Frightened monsters retreat.
 * Monsters adjacent to the character attack if possible.
 *
 * Monster packs lure the character into open ground and then leap
 * upon him.  Monster groups try to surround the character.  -KJ-
 *
 * Monsters not in LOS always advance (this avoids player frustration).
 * Monsters in LOS will advance to the character, up to their standard
 * combat range, to a grid that allows them to target the character, or
 * just stay still if they are happy where they are, depending on the
 * tactical situation and the monster's preferred and minimum combat
 * ranges.
 * NOTE:  Here is an area that would benefit from more development work.
 *
 * Non-trivial movement calculations are performed by the helper
 * functions "get_move_advance" and "get_move_retreat", which keeps
 * this function relatively simple.
 *
 * The variable "must_use_target" is used for monsters that can't
 * currently perceive the character, but have a known target to move
 * towards.  With a bit more work, this will lead to semi-realistic
 * "hunting" behavior.
 *
 * Return FALSE if monster doesn't want to move or can't.
 */
 
/*
 * NEWPETCODE This is where the 1) pet movement needs to be and 2)
 * where the monsters need a chance to target other monsters
 * must use target is in use for monster that are not interested in the 
 * character. examine for use in the pet code. INFO NOT 100%correct
 *
 */ 
static bool get_move(monster_type *m_ptr, int *ty, int *tx, bool *fear,
                     bool must_use_target)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int i, start;
	int y, x;

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Assume no movement */
	*ty = m_ptr->fy;
	*tx = m_ptr->fx;

	
	/* I'm pretty sure the pet movement loop needs to be handled here */
	/* first. NEWPETCODE. It's in the old code as normal movement first thing. */
	/* turns out that it doesn't go here. */
	
	/*
	 * Monsters that cannot move will attack the character if he is
	 * adjacent.  Otherwise, they cannot move.
	 */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE))
	{
		/* Hack -- memorize lack of moves after a while. */
		if (!(l_ptr->r_flags1 & (RF1_NEVER_MOVE)))
		{
			if ((mon_fully_visible(m_ptr)) && (one_in_(20)))
				l_ptr->r_flags1 |= (RF1_NEVER_MOVE);
		}

		/* Is character in range? */
		if ((m_ptr->cdis <= 1) && (cave_passable_bold(py, px)))
		{
			/* Monster can't melee either (pathetic little creature) */
			if (r_ptr->flags1 & (RF1_NEVER_BLOW))
			{
				/* Hack -- memorize lack of attacks after a while */
				if (!(l_ptr->r_flags1 & (RF1_NEVER_BLOW)))
				{
					if ((mon_fully_visible(m_ptr)) && (one_in_(10)))
						l_ptr->r_flags1 |= (RF1_NEVER_BLOW);
				}
			}

			/* Can attack */
			else
			{
				/* Kill. */
				*fear = FALSE;
				*ty = py;
				*tx = px;
				return (TRUE);
			}
		}

		/* If we can't hit anything, do not move */
		return (FALSE);
	}


	/*
	 * Monster is only allowed to use targeting information.
	 */
	if (must_use_target)
	{
		*ty = m_ptr->ty;
		*tx = m_ptr->tx;
		return (TRUE);
	}


	/*** Handle monster fear -- only for monsters that can move ***/
	/* NEWPETCODE -- Here code needs to go to cause pets to keep their */
	/* distance from the player I may be able to use flee range also */
	/* Is the monster scared? - the code I need to use for below looks */
	/* like it's contained under mon_will_run in old meele2 */
	if ((m_ptr->min_range >= FLEE_RANGE) || (m_ptr->monfear)) *fear = TRUE;
	else *fear = FALSE;

	/* Monster is frightened or terrified. */
	if (*fear)
	{
		/* The character is too close to avoid, and faster than we are */
		if ((!m_ptr->monfear) && (m_ptr->cdis < TURN_RANGE) &&
		     (p_ptr->pspeed > m_ptr->mspeed))
		{
			/* Recalculate range */
			find_range(m_ptr);

			/* Note changes in monster attitude */
			if (m_ptr->min_range < m_ptr->cdis)
			{
				/* Cancel fear */
				*fear = FALSE;

				/* No message -- too annoying */

				/* Charge! */
				*ty = py;
				*tx = px;

				return (TRUE);
			}
		}

		/* The monster is within 25 grids of the character */
		else if (m_ptr->cdis < FLEE_RANGE)
		{
			/* Find and move towards a hidey-hole */
			get_move_retreat(m_ptr, ty, tx);
			return (TRUE);
		}

		/* Monster is well away from danger */
		else
		{
			/* No need to move */
			return (FALSE);
		}
	}


	/* If the character is adjacent, attack or back off.  */
	if ((!*fear) && (m_ptr->cdis <= 1))
	{
		/* Monsters that cannot attack back off. */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW))
		{
			/* Hack -- memorize lack of attacks after a while */
			if (!(l_ptr->r_flags1 & (RF1_NEVER_BLOW)))
			{
				if ((mon_fully_visible(m_ptr)) && (one_in_(10)))
					l_ptr->r_flags1 |= (RF1_NEVER_BLOW);
			}

			/* Back away */
			*fear = TRUE;
		}

		else
		{
			/* All other monsters attack. */
			*ty = py;
			*tx = px;
			return (TRUE);
		}
	}


	/* Animal packs try to lure the character into the open. */
	if ((!*fear) && (r_ptr->flags1 & (RF1_FRIENDS)) &&
	    (r_ptr->flags3 & (RF3_ANIMAL)) &&
	    (!(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL))))
	{
		/* Animal has to be willing to melee */
		if (m_ptr->min_range == 1)
		{
			/*
			 * If character vulnerability has not yet been
			 * calculated this turn, calculate it now.
			 */
			if (p_ptr->vulnerability == 0)
			{
				/* Attack disabled or aggravating player -EB- */
				if (p_ptr->blind || p_ptr->image || p_ptr->confused ||
					 p_ptr->afraid || p_ptr->paralyzed || p_ptr->aggravate)
				{
					p_ptr->vulnerability = 10;
				}

				/* Count passable grids next to player */
				for (i = 0; i < 8; i++)
				{
					y = py + ddy_ddd[i];
					x = px + ddx_ddd[i];

					/* Check Bounds */
					if (!in_bounds(y, x)) continue;

					/* Count floor grids (generic passable) */
					if (cave_floor_bold(y, x))
					{
						p_ptr->vulnerability++;
					}
				}

				/*
				 * Take character weakness into account (this
				 * always adds at least one)
				 */
				p_ptr->vulnerability += (p_ptr->mhp / MAX(1, p_ptr->chp));

				/* Is seriously weakened -- go berserk */
				if (p_ptr->chp < 2 * p_ptr->mhp / 5)
					p_ptr->vulnerability = 100;

				/* Sometimes go berserk at random  XXX */
				else if ((p_ptr->vulnerability > 4) &&
				         (one_in_(15 - p_ptr->vulnerability)))
				{
					p_ptr->vulnerability = 100;
				}
			}

			/* Character is insufficiently vulnerable. */
			if (p_ptr->vulnerability <= 4)
			{
				/* If we're in sight, find a hiding place */
				if (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_SEEN))
				{
					/* Find a safe spot to lurk in */
					if (get_move_retreat(m_ptr, ty, tx))
					{
						*fear = TRUE;
					}
					else
					{
						/* No safe spot -- charge */
						*ty = py;
						*tx = px;
					}
				}

				/* Otherwise, we advance cautiously */
				else
				{
					/* Advance, ... */
					get_move_advance(m_ptr, ty, tx);

					/* ... but make sure we stay hidden. */
					if (m_ptr->cdis > 1) *fear = TRUE;
				}

				/* done */
				return (TRUE);
			}
		}
	}

	/* Monster groups try to surround the character. */
	if ((!*fear) && (r_ptr->flags1 & (RF1_FRIENDS)) &&
	    (m_ptr->cdis <= 3) && (player_has_los_bold(m_ptr->fy, m_ptr->fx)))
	{
		start = rand_int(8);

		/* Find a random empty square next to the player to head for */
		for (i = start; i < 8 + start; i++)
		{
			/* Pick squares near player */
			y = py + ddy_ddd[i % 8];
			x = px + ddx_ddd[i % 8];

			/* Check Bounds */
			if (!in_bounds(y, x)) continue;

			/* Ignore occupied grids */
			if (cave_m_idx[y][x] != 0) continue;

			/* Ignore grids that monster can't enter immediately */
			if (!cave_exist_mon(r_ptr, y, x, FALSE, TRUE)) continue;

			/* Accept */
			*ty = y;
			*tx = x;
			return (TRUE);
		}
	}

	/* Monster can go through rocks - head straight for character */
	if ((!*fear) && ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
	                 (r_ptr->flags2 & (RF2_KILL_WALL))))
	{
		*ty = py;
		*tx = px;
		return (TRUE);
	}


	/* No special moves made -- use standard movement */

	/* Not frightened */
	if (!*fear)
	{
		/*
		 * XXX XXX -- The monster cannot see the character.  Make it
		 * advance, so the player can have fun ambushing it.
		 */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			/* Advance */
			get_move_advance(m_ptr, ty, tx);
		}

		/* Monster can see the character */
		else
		{
			/* Always reset the monster's target */
			m_ptr->ty = py;
			m_ptr->tx = px;

			/* Monsters too far away will advance. */
			if (m_ptr->cdis > m_ptr->best_range)
			{
				*ty = py;
				*tx = px;
			}

			/* Monsters not too close will often advance */
			else if ((m_ptr->cdis > m_ptr->min_range)  && (one_in_(2)))
			{
				*ty = py;
				*tx = px;
			}

			/* Monsters that can't target the character will advance. */
			else if (projectable(m_ptr->fy, m_ptr->fx,
			         py, px, 0) == PROJECT_NO)
			{
				*ty = py;
				*tx = px;
			}

			/* Otherwise they will stay still or move randomly. */
			else
			{
				/*
				 * It would be odd if monsters that move randomly
				 * were to stay still.
				 */
				if (r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25))
				{
					/* Pick a random grid next to the monster */
					i = rand_int(8);

					*ty = m_ptr->fy + ddy_ddd[i];
					*tx = m_ptr->fx + ddx_ddd[i];
				}

				/* Monsters could look for better terrain... */
			}
		}
	}

	/* Monster is frightened */
	else
	{
		/* Back away -- try to be smart about it */
		get_move_retreat(m_ptr, ty, tx);
	}


	/* We do not want to move */
	if ((*ty == m_ptr->fy) && (*tx == m_ptr->fx)) return (FALSE);

	/* We want to move */
	return (TRUE);
}

/* 
 * Choose the probable best direction for a pet monster to move in. I have
 * not the first fucking clue if this is going to work or not, but I'm basing
 * it off LM, and BR's code, so I have high hopes. The basic idea is in that 
 * Process monster, really obvious monster things are taken into account (random
 * movement, and such) and then the monster gets a target to move to, and then
 * another function is called which intelligently moves the monster towards that
 * target. I'm inserting a statement, that checks for pet/friendhood, and then picks
 * a target for that friend, and lets make and proccess move handle that normally.
 * This is done by choosing a target grid, and finding the direction that best 
 * approaches it.
 */
static bool get_pet_move(monster_type *m_ptr, int *ty, int *tx,
						 bool must_use_target, bool friendly)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	monster_type *t_ptr;
	monster_race *tr_ptr;


	bool avoid, lonely, distant;
	int i;
	int t_idx;
	
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Assume no movement */
	*ty = m_ptr->fy;
	*tx = m_ptr->fx;

	/*
	 * pets that cannot move will attack monsters if they is
	 * adjacent.  Otherwise, they cannot move.
	 */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE))
	{
		/* Hack -- memorize lack of moves after a while. */
		if (!(l_ptr->r_flags1 & (RF1_NEVER_MOVE)))
		{
			if ((mon_fully_visible(m_ptr)) && (one_in_(20)))
				l_ptr->r_flags1 |= (RF1_NEVER_MOVE);
		}
		
		/* pets never seek to attack the player */
		
		/* If we can't hit anything, do not move */
		return (FALSE);
	}

	/*
	 * Monster is only allowed to use targeting information.
	 */
	if (must_use_target)
	{
		*ty = m_ptr->ty;
		*tx = m_ptr->tx;
		return (TRUE);
	}

	if ((p_ptr->pet_follow_distance < 0) &&
					  (m_ptr->cdis <= (0 - p_ptr->pet_follow_distance)))
	{				  
		avoid = TRUE;
	}
	else avoid = FALSE;
	
	/* Do we want to find the player? */
	if (!avoid && (m_ptr->cdis > p_ptr->pet_follow_distance)) lonely = TRUE;
	else lonely = FALSE;
	
	/* Should we find the player if we can't find a monster? */
	if (m_ptr->cdis > PET_SEEK_DIST) distant = TRUE;
	else distant = FALSE;

	/* Scan thru all monsters */
	for (i = 1; i < m_max; i++)
	{
		t_idx = i;
		t_ptr = &m_list[t_idx];
		tr_ptr = &r_info[t_ptr->r_idx];

		/* The monster itself isn't a target */
		if (t_ptr == m_ptr) continue;

		/* Paranoia -- Skip dead monsters */
		if (!t_ptr->r_idx) continue;

		/* Hack -- only fight away from player */
		if (p_ptr->pet_follow_distance < 0)
		{
			/* No fighting near player */
			if (t_ptr->cdis <= (0 - p_ptr->pet_follow_distance))
			{
				continue;
			}
		}
		/* Hack -- no fighting away from player */
		else if ((m_ptr->cdis < t_ptr->cdis) &&
					(t_ptr->cdis > p_ptr->pet_follow_distance))
		{
			continue;
		}

		/* Monster must be 'an enemy' */
		if (!are_enemies(m_ptr, t_ptr)) continue;

		/* Monster must be projectable if we can't pass through walls */
		if (!(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) &&
			!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx, PROJECT_CHCK))
		{
			continue;
		}

		/* OK -- we've got a target */
		*ty = t_ptr->fy;
		*tx = t_ptr->fx;
		return (TRUE);
	}
	
	/* Find the player if necessary */
	if (avoid || lonely || distant)
	{
		/* Remember the leash length */
		int dis = p_ptr->pet_follow_distance;

		/* Hack -- adjust follow distance temporarily */
		if (p_ptr->pet_follow_distance > PET_SEEK_DIST)
		{
			p_ptr->pet_follow_distance = PET_SEEK_DIST;
		}
		
		/* Find the player */
		*ty = py;
		*tx = px;
	
		/* Restore the leash */
		p_ptr->pet_follow_distance = dis;
	}
	
	/* Are we trying to avoid the player? */
	if (((p_ptr->pet_follow_distance < 0) &&
			  (m_ptr->cdis <= (0 - p_ptr->pet_follow_distance))) ||
			  (m_ptr->cdis <= (p_ptr->pet_follow_distance)))
	{
		get_move_retreat(m_ptr, ty, tx);
		return (TRUE);
	}


	if (friendly)
	{
		for (i = 1; i < m_max; i++)
		{
			t_idx = i;
			t_ptr = &m_list[t_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
	
			/* The monster itself isn't a target */
			if (t_ptr == m_ptr) continue;
	
			/* Paranoia -- Skip dead monsters */
			if (!t_ptr->r_idx) continue;
	
			/* Monster must be 'an enemy' */
			if (!are_enemies(m_ptr, t_ptr)) continue;
	
			/* Monster must be projectable if we can't pass through walls */
			if (!(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) &&
				!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx, 0L))
			{
				continue;
			}
	
			/* OK -- we've got a target */
			*ty = t_ptr->fy;
			*tx = t_ptr->fx;
			return (TRUE);
		}
	}
	
	/* We do not want to move */
	if ((*ty == m_ptr->fy) && (*tx == m_ptr->fx)) return (FALSE);

	/* We want to move */
	return (TRUE);

}

/*
 * A simple method to help fleeing monsters who are having trouble getting
 * to their target.  It's very stupid, but works fairly well in the
 * situations it is called upon to resolve.  XXX XXX
 *
 * If this function claims success, ty and tx must be set to a grid
 * adjacent to the monster.
 *
 * Return TRUE if this function actually did any good.
 */
static bool get_route_to_target(monster_type *m_ptr, int *ty, int *tx)
{
	int i, j;
	int y, x, yy, xx;
	int target_y, target_x, dist_y, dist_x;

	bool dummy;
	bool below = FALSE;
	bool right = FALSE;

	target_y = 0;
	target_x = 0;

	/* Is the target further away vertically or horizontally? */
	dist_y = ABS(m_ptr->ty - m_ptr->fy);
	dist_x = ABS(m_ptr->tx - m_ptr->fx);

	/* Target is further away vertically than horizontally */
	if (dist_y > dist_x)
	{
		/* Find out if the target is below the monster */
		if (m_ptr->ty - m_ptr->fy > 0) below = TRUE;

		/* Search adjacent grids */
		for (i = 0; i < 8; i++)
		{
			y = m_ptr->fy + ddy_ddd[i];
			x = m_ptr->fx + ddx_ddd[i];

			/* Check bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Grid is not passable */
			if (!cave_passable_mon(m_ptr, y, x, &dummy)) continue;

			/* Grid will take me further away */
			if ((( below) && (y < m_ptr->fy)) ||
			    ((!below) && (y > m_ptr->fy)))
			{
				continue;
			}

			/* Grid will not take me closer or further */
			else if (y == m_ptr->fy)
			{
				/* See if it leads to better things */
				for (j = 0; j < 8; j++)
				{
					yy = y + ddy_ddd[j];
					xx = x + ddx_ddd[j];

					/* Grid does lead to better things */
					if ((( below) && (yy > m_ptr->fy)) ||
					    ((!below) && (yy < m_ptr->fy)))
					{
						/* But it is not passable */
						if (!cave_passable_mon(m_ptr, yy, xx, &dummy)) continue;

						/*
						 * Accept (original) grid, but don't immediately claim
						 * success
						 */
						target_y = y;
						target_x = x;
					}
				}
			}

			/* Grid will take me closer */
			else
			{
				/* Don't look this gift horse in the mouth. */
				*ty = y;
				*tx = x;
				return (TRUE);
			}
		}
	}

	/* Target is further away horizontally than vertically */
	else if (dist_x > dist_y)
	{
		/* Find out if the target is right of the monster */
		if (m_ptr->tx - m_ptr->fx > 0) right = TRUE;

		/* Search adjacent grids */
		for (i = 0; i < 8; i++)
		{
			y = m_ptr->fy + ddy_ddd[i];
			x = m_ptr->fx + ddx_ddd[i];

			/* Check bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Grid is not passable */
			if (!cave_passable_mon(m_ptr, y, x, &dummy)) continue;

			/* Grid will take me further away */
			if ((( right) && (x < m_ptr->fx)) ||
			    ((!right) && (x > m_ptr->fx)))
			{
				continue;
			}

			/* Grid will not take me closer or further */
			else if (x == m_ptr->fx)
			{
				/* See if it leads to better things */
				for (j = 0; j < 8; j++)
				{
					yy = y + ddy_ddd[j];
					xx = x + ddx_ddd[j];

					/* Grid does lead to better things */
					if ((( right) && (xx > m_ptr->fx)) ||
					    ((!right) && (xx < m_ptr->fx)))
					{
						/* But it is not passable */
						if (!cave_passable_mon(m_ptr, yy, xx, &dummy)) continue;

						/* Accept (original) grid, but don't immediately claim success */
						target_y = y;
						target_x = x;
					}
				}
			}

			/* Grid will take me closer */
			else
			{
				/* Don't look this gift horse in the mouth. */
				*ty = y;
				*tx = x;
				return (TRUE);
			}
		}
	}

	/* Target is the same distance away along both axes. */
	else
	{
		/* XXX XXX - code something later to fill this hole. */
		return (FALSE);
	}

	/* If we found a solution, claim success */
	if ((target_y) && (target_x))
	{
		*ty = target_y;
		*tx = target_x;
		return (TRUE);
	}

	/* No luck */
	return (FALSE);
}


/*
 * Confused monsters bang into walls and doors, and wander into lava or
 * water.  This function assumes that the monster does not belong in this
 * grid, and therefore should suffer for trying to enter it.
 */
static void make_confused_move(monster_type *m_ptr, int y, int x)
{
	char m_name[80];

	int feat;

	bool seen = FALSE;
	bool fear = FALSE;
	bool death = TRUE;

	bool confused = m_ptr->confused;

	/* Check Bounds (fully) */
	if (!in_bounds_fully(y, x)) return;

	/* Check location */
	feat = cave_feat[y][x];

	/* Check visibility */
	if ((m_ptr->ml) && (cave_info[y][x] & (CAVE_SEEN))) seen = TRUE;


	/* Get the monster name/poss */
	monster_desc(m_name, m_ptr, 0);


	/* Feature is a wall */
	if (cave_info[y][x] & (CAVE_WALL))
	{
		/* Feature is a (known) door */
		if ((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL))
		{
			if (seen && confused)
				msg_format("%^s bangs into a door.", m_name);
		}

		/* Rubble */
		else if (feat == FEAT_RUBBLE)
		{
			if (seen && confused)
				msg_format("%^s staggers into some rubble.", m_name);
		}

		/* Otherwise, we assume that the feature is a "wall".  XXX  */
		else
		{
			if (seen && confused)
				msg_format("%^s bashes into a wall.", m_name);
		}

		/* Sometimes stun the monster, but only lightly */
		if ((one_in_(3)) && (m_ptr->stunned < 5))
			m_ptr->stunned += 3;
	}

	/* Feature is not a wall */
	else
	{
		/* No changes */
	}


	/* Monster is frightened */
	if ((!death) && (fear) && (seen))
	{
		msg_format("%^s panics!", m_name);
	}
}


/*
 * Given a target grid, calculate the grid the monster will actually
 * attempt to move into.
 *
 * The simplest case is when the target grid is adjacent to us and
 * able to be entered easily.  Usually, however, one or both of these
 * conditions don't hold, and we must pick an initial direction, than
 * look at several directions to find that most likely to be the best
 * choice.  If so, the monster needs to know the order in which to try
 * other directions on either side.  If there is no good logical reason
 * to prioritize one side over the other, the monster will act on the
 * "spur of the moment", using current turn as a randomizer.
 *
 * The monster then attempts to move into the grid.  If it fails, this
 * function returns FALSE and the monster ends its turn.
 *
 * The variable "fear" is used to invoke any special rules for monsters
 * wanting to retreat rather than advance.  For example, such monsters
 * will not leave an non-viewable grid for a viewable one and will try
 * to avoid the character.
 *
 * The variable "bash" remembers whether a monster had to bash a door
 * or not.  This has to be remembered because the choice to bash is
 * made in a different function than the actual bash move.  XXX XXX  If
 * the number of such variables becomes greater, a structure to hold them
 * would look better than passing them around from function to function.
 */
static bool make_move(monster_type *m_ptr, int *ty, int *tx, bool fear,
	bool *bash)
{
	int i, j;

	/* Start direction, current direction */
	int dir0, dir;

	/* Deltas, absolute axis distances from monster to target grid */
	int dy, ay, dx, ax;

	/* Existing monster location, proposed new location */
	int oy, ox, ny, nx;

	bool avoid = FALSE;
	bool passable = FALSE;
	bool look_again = FALSE;

	int chance;

	/* Remember where monster is */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Get the change in position needed to get to the target */
	dy = *ty - oy;
	dx = *tx - ox;

	/* Is the target grid adjacent to the current monster's position? */
	if ((!fear) && (dy >= -1) && (dy <= 1) && (dx >= -1) && (dx <= 1))
	{
		/* If it is, try the shortcut of simply moving into the grid */

		/* Get the probability of entering this grid */
		chance = cave_passable_mon(m_ptr, *ty, *tx, bash);

		/* Grid must be pretty easy to enter, or monster must be confused */
		if ((m_ptr->confused) || (chance >= 50))
		{
			/*
			 * Amusing messages and effects for confused monsters trying
			 * to enter terrain forbidden to them.
			 */
			if ((m_ptr->confused) && (chance <= 25))
			{
				/* Sometimes hurt the poor little critter */
				if (one_in_(5)) make_confused_move(m_ptr, *ty, *tx);

				/* Do not actually move */
				if (!chance) return (FALSE);
			}

			/* We can enter this grid */
			if ((chance >= 100) || (chance > rand_int(100)))
			{
				return (TRUE);
			}

			/* Failure to enter grid.  Cancel move */
			else
			{
				return (FALSE);
			}
		}
	}



	/* Calculate vertical and horizontal distances */
	ay = ABS(dy);
	ax = ABS(dx);

	/* We mostly want to move vertically */
	if (ay > (ax * 2))
	{
		/* Choose between directions '8' and '2' */
		if (dy < 0)
		{
			/* We're heading up */
			dir0 = 8;
			if ((dx < 0) || (dx == 0 && (turn % 2 == 0))) dir0 += 10;
		}
		else
		{
			/* We're heading down */
			dir0 = 2;
			if ((dx > 0) || (dx == 0 && (turn % 2 == 0))) dir0 += 10;
		}
	}

	/* We mostly want to move horizontally */
	else if (ax > (ay * 2))
	{
		/* Choose between directions '4' and '6' */
		if (dx < 0)
		{
			/* We're heading left */
			dir0 = 4;
			if ((dy > 0) || (dy == 0 && (turn % 2 == 0))) dir0 += 10;
		}
		else
		{
			/* We're heading right */
			dir0 = 6;
			if ((dy < 0) || (dy == 0 && (turn % 2 == 0))) dir0 += 10;
		}
	}

	/* We want to move up and sideways */
	else if (dy < 0)
	{
		/* Choose between directions '7' and '9' */
		if (dx < 0)
		{
			/* We're heading up and left */
			dir0 = 7;
			if ((ay < ax) || (ay == ax && (turn % 2 == 0))) dir0 += 10;
		}
		else
		{
			/* We're heading up and right */
			dir0 = 9;
			if ((ay > ax) || (ay == ax && (turn % 2 == 0))) dir0 += 10;
		}
	}

	/* We want to move down and sideways */
	else
	{
		/* Choose between directions '1' and '3' */
		if (dx < 0)
		{
			/* We're heading down and left */
			dir0 = 1;
			if ((ay > ax) || (ay == ax && (turn % 2 == 0))) dir0 += 10;
		}
		else
		{
			/* We're heading down and right */
			dir0 = 3;
			if ((ay < ax) || (ay == ax && (turn % 2 == 0))) dir0 += 10;
		}
	}


	/*
	 * Now that we have an initial direction, we must determine which
	 * grid to actually move into.
	 */
	if (TRUE)
	{
		/* Build a structure to hold movement data */
		typedef struct move_data move_data;
		struct move_data
		{
			int move_chance;
			bool move_bash;
		};
		move_data moves_data[8];


		/*
		 * Scan each of the eight possible directions, in the order of
		 * priority given by the table "side_dirs", choosing the one that
		 * looks like it will get the monster to the character - or away
		 * from him - most effectively.
		 */
		for (i = 0; i <= 8; i++)
		{
			/* Out of options */
			if (i == 8) break;

			/* Get the actual direction */
			dir = side_dirs[dir0][i];

			/* Get the grid in our chosen direction */
			ny = oy + ddy[dir];
			nx = ox + ddx[dir];

			/* Check Bounds */
			if (!in_bounds(ny, nx)) continue;

			/* Store this grid's movement data. */
			moves_data[i].move_chance =
				cave_passable_mon(m_ptr, ny, nx, bash);
			moves_data[i].move_bash = *bash;


			/* Confused monsters must choose the first grid */
			if (m_ptr->confused) break;

			/* If this grid is totally impassable, skip it */
			if (moves_data[i].move_chance == 0) continue;

			/* Frightened monsters work hard not to be seen. */
			if (fear)
			{
				/* Monster is having trouble navigating to its target. */
				if ((m_ptr->ty) && (m_ptr->tx) && (i >= 2) &&
				    (distance(m_ptr->fy, m_ptr->fx, m_ptr->ty, m_ptr->tx) > 1))
				{
					/* Look for an adjacent grid leading to the target */
					if (get_route_to_target(m_ptr, ty, tx))
					{
						/* Calculate the chance to enter the grid */
						chance = cave_passable_mon(m_ptr, *ty, *tx, bash);

						/* Try to move into the grid */
						if ((chance < 100) && (randint(100) > chance))
						{
							/* Can't move */
							return (FALSE);
						}

						/* Can move */
						return (TRUE);
					}

					/* No good route found */
					else if (i >= 3)
					{
						/*
						 * We can't get to our hiding place.  We're in line of fire.
						 * The only thing left to do is go down fighting.  XXX XXX
						 */
						 if ((m_ptr->ml) && (player_can_see_bold(oy, ox)))
						 {
							char m_name[80];

							/* Cancel fear */
							set_mon_fear(m_ptr, 0, FALSE);

							/* Turn and fight */
							fear = FALSE;

							/* Forget target */
							m_ptr->ty = 0;    m_ptr->tx = 0;

							/* Charge!  XXX XXX */
							m_ptr->min_range = 1;  m_ptr->best_range = 1;

							/* Get the monster name */
							monster_desc(m_name, m_ptr, 0);

							/* Dump a message */
							msg_format("%^s turns to fight!", m_name);

							/* Hack -- lose some time  XXX XXX */
							return (FALSE);
						}
					}
				}

				/* Attacking the character as a first choice? */
				if ((i == 0) && (ny == p_ptr->py) && (nx == p_ptr->px))
				{
					/* Need to rethink some plans XXX XXX XXX */
					m_ptr->ty = 0;
					m_ptr->tx = 0;
				}

				/* Monster is visible */
				if (m_ptr->ml)
				{
					/* And is in LOS */
					if (player_has_los_bold(oy, ox))
					{
						/* Accept any easily passable grid out of LOS */
						if ((!player_has_los_bold(ny, nx)) &&
							(moves_data[i].move_chance > 40))
						{
							break;
						}
					}
					else
					{
						/* Do not enter a grid in LOS */
						if (player_has_los_bold(ny, nx))
						{
							moves_data[i].move_chance = 0;
							continue;
						}
					}
				}

				/* Monster can't be seen, and is not in a "seen" grid. */
				if ((!m_ptr->ml) && (!player_can_see_bold(oy, ox)))
				{
					/* Do not enter a "seen" grid */
					if (player_can_see_bold(ny, nx))
					{
						moves_data[i].move_chance = 0;
						continue;
					}
				}
			}

			/* XXX XXX -- Sometimes attempt to break glyphs. */
			if ((cave_feat[ny][nx] == FEAT_GLYPH) && (!fear) &&
			    (one_in_(5)))
			{
				break;
			}

			/* Initial direction is almost certainly the best one */
			if ((i == 0) && (moves_data[i].move_chance >= 80))
			{
				/*
				 * If backing away and close, try not to walk next
				 * to the character, or get stuck fighting him.
				 */
				if ((fear) && (m_ptr->cdis <= 2) &&
					(distance(p_ptr->py, p_ptr->px, ny, nx) <= 1))
				{
					avoid = TRUE;
				}

				else break;
			}

			/* Either of the first two side directions looks good */
			else if (((i == 1) || (i == 2)) &&
			         (moves_data[i].move_chance >= 50))
			{
				/* Accept the central direction if at least as good */
				if ((moves_data[0].move_chance >=
				     moves_data[i].move_chance))
				{
					if (avoid)
					{
						/* Frightened monsters try to avoid the character */
						if (distance(p_ptr->py, p_ptr->px, ny, nx) == 0)
						{
							i = 0;
						}
					}
					else
					{
						i = 0;
					}
				}

				/* Accept this direction */
				break;
			}

			/* This is the first passable direction */
			if (!passable)
			{
				/* Note passable */
				passable = TRUE;

				/* All the best directions are blocked. */
				if (i >= 3)
				{
					/* Settle for "good enough" */
					break;
				}
			}

			/* We haven't made a decision yet; look again. */
			if (i == 7) look_again = TRUE;
		}


		/* We've exhausted all the easy answers. */
		if (look_again)
		{
			/* There are no passable directions. */
			if (!passable)
			{
				return (FALSE);
			}

			/* We can move. */
			for (j = 0; j < 8; j++)
			{
				/* Accept the first option, however poor.  XXX */
				if (moves_data[j].move_chance)
				{
					i = j;
					break;
				}
			}
		}

		/* If no direction was acceptable, end turn */
		if (i >= 8)
		{
			return (FALSE);
		}

		/* Get movement information (again) */
		dir = side_dirs[dir0][i];
		*bash = moves_data[i].move_bash;

		/* No good moves, so we just sit still and wait. */
		if ((dir == 5) || (dir == 0))
		{
			return (FALSE);
		}

		/* Get grid to move into */
		*ty = oy + ddy[dir];
		*tx = ox + ddx[dir];

		/*
		 * Amusing messages and effects for confused monsters trying
		 * to enter terrain forbidden to them.
		 */
		if ((m_ptr->confused) && (moves_data[i].move_chance <= 25))
		{
			/* Sometimes hurt the poor little critter */
			if (one_in_(5)) make_confused_move(m_ptr, *ty, *tx);

			/* Do not actually move */
			if (!moves_data[i].move_chance) return (FALSE);
		}

		/* Try to move in the chosen direction.  If we fail, end turn. */
		if ((moves_data[i].move_chance < 100) &&
		    (randint(100) > moves_data[i].move_chance))
		{
			return (FALSE);
		}
	}


	/* Monster is frightened, and is obliged to fight. */
	if ((fear) && (cave_m_idx[*ty][*tx] < 0))
	{
		/* Cancel fear */
		set_mon_fear(m_ptr, 0, FALSE);

		/* Turn and fight */
		fear = FALSE;

		/* Forget target */
		m_ptr->ty = 0;    m_ptr->tx = 0;

		/* Charge!  XXX XXX */
		m_ptr->min_range = 1;  m_ptr->best_range = 1;

		/* Message if seen */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Dump a message */
			/* msg_format("%^s turns on you!", m_name); */
		}
	}

	/* We can move. */
	return (TRUE);
}




/*
 * If one monster moves into another monster's grid, they will
 * normally swap places.  If the second monster cannot exist in the
 * grid the first monster left, this can't happen.  In such cases,
 * the first monster tries to push the second out of the way.
 */
static bool push_aside(monster_type *m_ptr, monster_type *n_ptr)
{
	/* Get racial information about the second monster */
	monster_race *nr_ptr = &r_info[n_ptr->r_idx];

	int y, x, i;
	int dir = 0;


	/*
	 * Translate the difference between the locations of the two
	 * monsters into a direction of travel.
	 */
	for (i = 0; i < 10; i++)
	{
		/* Require correct difference along the y-axis */
		if ((n_ptr->fy - m_ptr->fy) != ddy[i]) continue;

		/* Require correct difference along the x-axis */
		if ((n_ptr->fx - m_ptr->fx) != ddx[i]) continue;

		/* Found the direction */
		dir = i;
		break;
	}

	/* Favor either the left or right side on the "spur of the moment". */
	if (turn % 2 == 0) dir += 10;

	/* Check all directions radiating out from the initial direction. */
	for (i = 0; i < 7; i++)
	{
		int side_dir = side_dirs[dir][i];

		y = n_ptr->fy + ddy[side_dir];
		x = n_ptr->fx + ddx[side_dir];

		/* Illegal grid */
		if (!in_bounds_fully(y, x)) continue;

		/* Grid is not occupied, and the 2nd monster can exist in it. */
		if (cave_exist_mon(nr_ptr, y, x, FALSE, TRUE))
		{
			/* Push the 2nd monster into the empty grid. */
			monster_swap(n_ptr->fy, n_ptr->fx, y, x);
			return (TRUE);
		}
	}

	/* We didn't find any empty, legal grids */
	return (FALSE);
}

/*
 * Process a monster's move.
 *
 * All the plotting and planning has been done, and all this function
 * has to do is move the monster into the chosen grid.
 *
 * This may involve attacking the character, breaking a glyph of
 * warding, bashing down a door, etc..  Once in the grid, monsters may
 * stumble into monster traps, hit a scent trail, pick up or destroy
 * objects, and so forth.
 *
 * A monster's move may disturb the character, depending on which
 * disturbance options are set.
 */
static void process_move(monster_type *m_ptr, int ty, int tx, bool bash)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int feat;

	/* Existing monster location, proposed new location */
	int oy, ox, ny, nx;

	/* Default move, default lack of view */
	bool do_move = TRUE;
	bool do_view = FALSE;

	/* Assume nothing */
	bool did_open_door = FALSE;
	bool did_bash_door = FALSE;
	bool did_take_item = FALSE;
	bool did_kill_item = FALSE;
	bool did_kill_body = FALSE;
	bool did_pass_wall = FALSE;
	bool did_kill_wall = FALSE;


	/* Remember where monster is */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Get the destination */
	ny = ty;
	nx = tx;

	/* Check Bounds */
	if (!in_bounds(ny, nx)) return;


	/* The grid is occupied by the player. */
	if (cave_m_idx[ny][nx] < 0)
	{
		/* Prevent this attack (paranoia) if it is a pet */
		/* Attack if possible */
		if (!(r_ptr->flags1 & (RF1_NEVER_BLOW)) && !(is_pet(m_ptr)))
		{
			(void)make_attack_normal(m_ptr);
		}

		/* End move */
		do_move = FALSE;
	}

	/* Can still move */
	if (do_move)
	{
		/* Get the feature in the grid that the monster is trying to enter. */
		feat = cave_feat[ny][nx];

		/* Entering a wall */
		if (cave_info[ny][nx] & (CAVE_WALL))
		{
			/* Monsters that kill walls sometimes kill rubble too */
			if (feat == FEAT_RUBBLE)
			{
				if ((r_ptr->flags2 & (RF2_KILL_WALL)) && (one_in_(3)))
				{
					/* Forget the rubble -- if in sight */
					if (player_can_see_bold(ny, nx))
					{
						do_view = TRUE;

						/* Note that the monster killed the wall */
						did_kill_wall = TRUE;
					}

					/* Forget the rubble */
					cave_info[ny][nx] &= ~(CAVE_MARK);

					/* Reduce the rubble to floor */
					cave_set_feat(ny, nx, FEAT_FLOOR);


				}
			}

			/* Monster passes through walls (and doors) */
			else if (r_ptr->flags2 & (RF2_PASS_WALL))
			{
				/* Monster went through a wall */
				did_pass_wall = TRUE;
			}

			/* Monster destroys walls (and doors) */
			else if (r_ptr->flags2 & (RF2_KILL_WALL))
			{
				bool msg = FALSE;

				/* Noise distance depends on monster "dangerousness"  XXX */
				int noise_dist = 3 + MIN(8, m_ptr->hp / p_ptr->lev);

				/* Forget the wall */
				cave_info[ny][nx] &= ~(CAVE_MARK);

				/* Note that the monster killed the wall */
				if (player_can_see_bold(ny, nx))
				{
					do_view = TRUE;
					did_kill_wall = TRUE;
				}

				/* Output warning messages if the racket gets too loud */
				else if (m_ptr->cdis <= noise_dist)
				{
					msg = TRUE;
				}

				/* Grid is currently a door */
				if (cave_closed_door(ny, nx))
				{
					cave_set_feat(ny, nx, FEAT_BROKEN);
					if (msg) msg_print("You hear a door being smashed open.");
				}

				/* Grid is anything else */
				else
				{
					cave_set_feat(ny, nx, FEAT_FLOOR);
					if (msg) msg_print("You hear grinding noises.");
				}
			}

			/* Doors */
			else if (cave_closed_door(ny, nx))
			{
				/* Monster bashes the door down */
				if (bash)
				{
					/* Character is not too far away */
					if (m_ptr->cdis < 25)
					{
						/* Message */
						msg_print("You hear a door burst open!");
					}

					/* Note that the monster bashed the door (if visible) */
					did_bash_door = TRUE;

					/* Break down the door */
					if (one_in_(2)) cave_set_feat(ny, nx, FEAT_BROKEN);
					else cave_set_feat(ny, nx, FEAT_OPEN);

					/* Handle doors in sight */
					if (player_can_see_bold(ny, nx))
					{
						/* Always disturb */
						disturb(0, 0);

						do_view = TRUE;
					}

					/* Optional disturb for non-viewable doors */
					else if (disturb_minor) disturb(0, 0);
				}

				/* Monster opens the door */
				else
				{
					/* Locked doors */
					if (cave_feat[ny][nx] != FEAT_DOOR_HEAD + 0x00)
					{
						/* Unlock the door */
						cave_set_feat(ny, nx, FEAT_DOOR_HEAD + 0x00);

						/* Do not move */
						do_move = FALSE;
					}

					/* Ordinary doors */
					else
					{
						/* Note that the monster opened the door (if visible) */
						did_open_door = TRUE;

						/* Open the door */
						cave_set_feat(ny, nx, FEAT_OPEN);

						/* Step into doorway sometimes */
						if (!one_in_(5)) do_move = FALSE;
					}

					/* Handle doors in sight */
					if (player_can_see_bold(ny, nx))
					{
						/* Do not disturb automatically */

						do_view = TRUE;
					}
				}
			}

			/* Paranoia -- Ignore all features not added to this code */
			else return;
		}

		/* Glyphs */
		else if (cave_feat[ny][nx] == FEAT_GLYPH)
		{
			/* Describe observable breakage */
			if (cave_info[ny][nx] & (CAVE_MARK))
			{
				msg_print("The rune of protection is broken!");
			}

			/* Forget the rune */
			cave_info[ny][nx] &= ~(CAVE_MARK);

			/* Break the rune */
			cave_set_feat(ny, nx, FEAT_FLOOR);
		}
	}

	/* Monster is allowed to move */
	if (do_move)
	{
		/* The grid is occupied by a monster. */
		if (cave_m_idx[ny][nx] > 0)
		{
			monster_type *n_ptr = &m_list[cave_m_idx[ny][nx]];
			monster_race *nr_ptr = &r_info[n_ptr->r_idx];

			/* Attack other monsters */
			/* If A) Monster has KILL_BODY flag, AND the target monster is not a unique */
			/* AND the monster that is attacking has > exp as the target AND target is not plant */
			/* B) the monsters are_enemies (Hostile V. Evil or hostile, or pet vs. non pet)  */
			/* C) The monster is confused. */
			if (((r_ptr->flags2 & (RF2_KILL_BODY)) &&
			    (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
			    (r_ptr->mexp > nr_ptr->mexp) &&
			    (!(nr_ptr->flags3 & (RF3_PLANT)))) ||
			    (are_enemies(m_ptr, n_ptr)) ||
			    (m_ptr->confused))
			{
				do_move = FALSE;

				if (r_ptr->flags2 & RF2_KILL_BODY) l_ptr->r_flags2 |= (RF2_KILL_BODY);

				/* attack */
				if ((n_ptr->r_idx) && (n_ptr->hp >= 0))
				{
					/* This used to be m_idx, cave_m_idx[ny][nx] - I assume it needs */
					/* the idx, so I'm getting it from the saved position of the monster */
					if (monst_attack_monst(cave_m_idx[m_ptr->fy][m_ptr->fx], cave_m_idx[ny][nx]))
					return;
				}
			}

			/* Swap with or push aside the other monster */
			/* Otherwise. . . */
			else
			{
				/* The other monster cannot switch places */
				if (!cave_exist_mon(nr_ptr, m_ptr->fy, m_ptr->fx, TRUE, TRUE))
				{
					/* Try to push it aside */
					if (!push_aside(m_ptr, n_ptr))
					{
						do_move = FALSE;
					}
				}
			}
		}
	}


	/* Monster can (still) move */
	if (do_move)
	{
		/* Move the monster */
		monster_swap(oy, ox, ny, nx);

		/* Cancel target when reached */
		if ((m_ptr->ty == ny) && (m_ptr->tx == nx))
		{
			m_ptr->ty = 0;
			m_ptr->tx = 0;
		}

		/*
		 * If a member of a monster group capable of smelling hits a
		 * scent trail while out of LOS of the character, it will
		 * communicate this to similar monsters.
		 */
		if ((!player_has_los_bold(ny, nx)) && (r_ptr->flags1 & (RF1_FRIENDS)) &&
		    (monster_can_smell(m_ptr)) && (get_scent(oy, ox) == -1) &&
		    (!m_ptr->ty) && (!m_ptr->tx) && !is_pet(m_ptr))
		{
			int i;
			monster_type *n_ptr;
			monster_race *nr_ptr;

			/* Scan all other monsters */
			for (i = m_max - 1; i >= 1; i--)
			{
				/* Access the monster */
				n_ptr = &m_list[i];
				nr_ptr = &r_info[n_ptr->r_idx];

				/* Ignore dead monsters */
				if (!n_ptr->r_idx) continue;

				/* Ignore monsters with the wrong symbol */
				if (r_ptr->d_char != nr_ptr->d_char) continue;

				/* Ignore monsters with specific orders */
				if ((n_ptr->ty) || (n_ptr->ty)) continue;

				/* Ignore monsters picking up a good scent */
				if (get_scent(n_ptr->fy, n_ptr->fx) < SMELL_STRENGTH - 10)
					continue;

				/* Ignore monsters not in LOS */
				if (!los(m_ptr->fy, m_ptr->fx, n_ptr->fy, n_ptr->fx))
					continue;

				/* Activate all other monsters and give directions */
				n_ptr->csleep = 0;
				n_ptr->mflag |= (MFLAG_ACTV);
				n_ptr->ty = ny;   n_ptr->tx = nx;
			}
		}

		/* Monster is visible and not cloaked */
		if (m_ptr->ml)
		{
			/* don't be disturbed by pets */
			if (is_pet(m_ptr))
			{
				/* ignore */
			}

			/* Player will always be disturbed if monster moves adjacent */
			else if (m_ptr->cdis == 1) disturb(1, 0);

			/* Hack -- ignore townspeople if strong enough  -clefs- */
			else if ((m_ptr->mflag & (MFLAG_TOWN)) && (p_ptr->lev >= 5))
			{
				/* Ignore */
			}

			/* Option -- be disturbed by all other monster movement */
			else if (disturb_move) disturb(0, 0);

			/* Option -- be disturbed by monster movement in LOS */
			else if ((disturb_near) &&
			         (player_has_los_bold(ny, nx)))
			{
				disturb(0, 0);
			}
		}

		/* Take or kill objects on the floor */
		if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
			 (r_ptr->flags2 & (RF2_KILL_ITEM)))
		{
			u32b f1, f2, f3;

			u32b flg3 = 0L;

			char m_name[80];
			char o_name[120];

			s16b this_o_idx, next_o_idx = 0;


			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[ny][nx]; this_o_idx;
			     this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;


				/* Skip gold */
				if (o_ptr->tval == TV_GOLD)
				{
					continue;
				}

				/* Extract some flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* React to objects that hurt the monster */
				if (f1 & (TR1_SLAY_ELEMENTAL)) 	flg3 |= (RF3_ELEMENTAL);
				if (f1 & (TR1_SLAY_DINOSAUR)) 	flg3 |= (RF3_DINOSAUR);
				if (f1 & (TR1_SLAY_CONSTRUCT))	flg3 |= (RF3_CONSTRUCT);
				if (f1 & (TR1_SLAY_AUTOMATA)) 	flg3 |= (RF3_AUTOMATA);
				if (f1 & (TR1_SLAY_DEMON)) 		flg3 |= (RF3_DEMON);
				if (f1 & (TR1_SLAY_UNDEAD)) 	flg3 |= (RF3_UNDEAD);
				if (f1 & (TR1_SLAY_ANIMAL)) 	flg3 |= (RF3_ANIMAL);
				if (f1 & (TR1_SLAY_EVIL)) 		flg3 |= (RF3_EVIL);
				if (f1 & (TR1_SLAY_ALIEN)) 		flg3 |= (RF3_ALIEN);
				if (f1 & (TR1_SLAY_BEASTMAN)) 	flg3 |= (RF3_BEASTMAN);

				/* The object cannot be picked up by the monster */
				if (artifact_p(o_ptr) || (r_ptr->flags3 & (flg3)) ||
					(o_ptr->ident & IDENT_QUEST))
				{
					/* Only give a message for "take_item" */
					if (r_ptr->flags2 & (RF2_TAKE_ITEM))
					{
						/* Take note */
						did_take_item = TRUE;

						/* Describe observable situations */
						if (m_ptr->ml && player_has_los_bold(ny, nx))
						{
							/* Get the object name */
							object_desc(o_name, o_ptr, TRUE, 3);

							/* Get the monster name */
							monster_desc(m_name, m_ptr, 0x04);

							/* Dump a message */
							msg_format("%^s tries to pick up %s, but fails.",
								   m_name, o_name);
						}
					}
				}

				/* Pick up the item */
				else if (r_ptr->flags2 & (RF2_TAKE_ITEM))
				{
					object_type *i_ptr;
					object_type object_type_body;

					/* Take note */
					did_take_item = TRUE;

					/* Describe observable situations */
					if (player_has_los_bold(ny, nx) && m_ptr->ml)
					{
						/* Get the object name */
						object_desc(o_name, o_ptr, TRUE, 3);

						/* Get the monster name */
						monster_desc(m_name, m_ptr, 0x04);

						/* Dump a message */
						msg_format("%^s picks up %s.", m_name, o_name);
					}

					/* Get local object */
					i_ptr = &object_type_body;

					/* Obtain local object */
					object_copy(i_ptr, o_ptr);

					/* Delete the object */
					delete_object_idx(this_o_idx);

					/* Carry the object */
					(void)monster_carry(cave_m_idx[m_ptr->fy][m_ptr->fx], i_ptr);
				}

				/* Destroy the item (unless you're a pet)*/
				else if (!is_pet(m_ptr))
				{
					/* Take note */
					did_kill_item = TRUE;

					/* Describe observable situations */
					if (player_has_los_bold(ny, nx))
					{
						/* Get the object name */
						object_desc(o_name, o_ptr, TRUE, 3);

						/* Get the monster name */
						monster_desc(m_name, m_ptr, 0x04);

						/* Dump a message */
						msg_format("%^s crushes %s.", m_name, o_name);
					}

					/* Delete the object */
					delete_object_idx(this_o_idx);
				}
			}
		}
	}             /* End of monster's move */



	/* Notice changes in view */
	if (do_view)
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Learn things from observable monster */
	if (mon_fully_visible(m_ptr))
	{
		/* Monster opened a door */
		if (did_open_door) l_ptr->r_flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) l_ptr->r_flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) l_ptr->r_flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) l_ptr->r_flags2 |= (RF2_KILL_ITEM);

		/* Monster ate another monster */
		if (did_kill_body) l_ptr->r_flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) l_ptr->r_flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall) l_ptr->r_flags2 |= (RF2_KILL_WALL);
	}
}





/*
 * Offsets for the spell indices
 */
#define RF4_OFFSET 32 * 3
#define RF5_OFFSET 32 * 4
#define RF6_OFFSET 32 * 5


/*
 * Monster takes its turn.
 */
static void process_monster(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int i, k, y, x;
	int ty, tx;
	int chance = 0;
	int choice = 0;
	int dir;
	bool fear = FALSE;

	bool bash;
	bool friendly = FALSE;

	/* Assume the monster is able to perceive the player. */
	bool aware = TRUE;
	bool must_use_target = FALSE;
	bool invisible = FALSE;

	/* Will the monster move randomly? */
	bool random_move = FALSE;

	/* Determine if pet gets angry */
	bool gets_angry = FALSE;

	/* if player is invisible - monsters cannot see him. */
	/* MUST BE FIXED */
	if (p_ptr->tim_invisiblity || p_ptr->invisiblity) invisible = TRUE;

	/* If monster is sleeping, it loses its turn. */
	if (m_ptr->csleep) return;
	if (m_ptr->stasis) return;

	/* Calculate the monster's preferred combat range when needed */
	if (m_ptr->min_range == 0) find_range(m_ptr);

	/* Monster is in active mode. */
	if (m_ptr->mflag & (MFLAG_ACTV))
	{
		/*
		 * Character is outside of scanning range and well outside
		 * of sighting range.  Monster does not have a target.
		 */
		if ((m_ptr->cdis >= FLEE_RANGE) && (m_ptr->cdis > r_ptr->aaf) &&
		    (!m_ptr->ty) && (!m_ptr->tx))
		{
			/* Monster cannot smell the character */
			if (!cave_when[m_ptr->fy][m_ptr->fx]) m_ptr->mflag &= ~(MFLAG_ACTV);
			else if (!monster_can_smell(m_ptr))   m_ptr->mflag &= ~(MFLAG_ACTV);
		}
	}

	/* Monster is in passive mode. */
	else
	{
		/* Character is inside scanning range */
		if (m_ptr->cdis <= r_ptr->aaf) m_ptr->mflag |= (MFLAG_ACTV);

		/* Monster has a target */
		else if ((m_ptr->ty) && (m_ptr->tx)) m_ptr->mflag |= (MFLAG_ACTV);

		/* The monster is catching too much of a whiff to ignore */
		else if (cave_when[m_ptr->fy][m_ptr->fx])
		{
			if (monster_can_smell(m_ptr)) m_ptr->mflag |= (MFLAG_ACTV);
		}
	}

	/* A monster in passive mode will end its turn at this point. */
	if (!(m_ptr->mflag & (MFLAG_ACTV))) return;

	/* No one wants to be your friend if you're aggravating */
	if (!is_hostile(m_ptr) && p_ptr->aggravate)
		gets_angry = TRUE;

	/* Demons sometimes become hostile. Sorry. 'dems the breaks */
	if (!is_hostile(m_ptr) && (r_ptr->flags3 & RF3_DEMON))
	{
		if (randint(r_ptr->level) > (randint(100) + (p_ptr->skills[SK_RITUAL_MAGIC].skill_rank * 3 / 2)))
			gets_angry = TRUE; 
	}

	/* Paranoia... no pet uniques outside wizard mode -- TY */
	if (is_pet(m_ptr) && (r_ptr->flags1 & RF1_UNIQUE)) gets_angry = TRUE;

	if (gets_angry)
	{
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);
		msg_format("%^s suddenly becomes hostile!", m_name);
		set_hostile(m_ptr);
	}

	/* Hack -- Always redraw the current target monster health bar */
	if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])
		p_ptr->redraw |= (PR_HEALTH);


	/* Attempt to multiply if able to and allowed */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) &&
	    (!m_ptr->confused) && (!m_ptr->monfear) && (is_hostile(m_ptr)) &&
	    (m_cnt < z_info->m_max - 50))
	{
		/* Count the adjacent monsters */
		for (k = 0, y = m_ptr->fy - 1; y <= m_ptr->fy + 1; y++)
		{
			for (x = m_ptr->fx - 1; x <= m_ptr->fx + 1; x++)
			{
				/* Check Bounds */
				if (!in_bounds(y, x)) continue;

				/* Count monsters */
				if (cave_m_idx[y][x] > 0) k++;
			}
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
		{
			/* Try to multiply - do I want the children of breeders to be pets? */
			if (multiply_monster(cave_m_idx[m_ptr->fy][m_ptr->fx], FALSE, FALSE))
			{
				/* Take note if visible */
				if (mon_fully_visible(m_ptr))
				{
					l_ptr->r_flags2 |= (RF2_MULTIPLY);
				}

				/* Multiplying takes energy */
				return;
			}
		}
	}


	/*** Ranged attacks ***/
	/* NEWPETCODE */
	
	if (r_ptr->freq_ranged)
	{
		int tar_y, tar_x;
		/* Testing, these are not necessary for the funtion */
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);

		/* Extract the ranged attack probability. */
		chance = r_ptr->freq_ranged;

		/* Cannot use ranged attacks beyond maximum range. */
		if ((chance) && (m_ptr->cdis > MAX_RANGE)) chance = 0;

		/* Cannot use ranged attacks when confused or not aware. */
		if ((chance) && ((m_ptr->confused) || (!aware))) chance = 0;
	
		/* Can't cast a ranged attack (at the player) if invisible */
		if ((chance) && (invisible)) chance = 0;
	
		/* Cannot cast spells when nice */
		if ((chance) && (m_ptr->mflag & (MFLAG_NICE))) chance = 0;

		/* Stunned monsters use ranged attacks half as often. */
		if ((chance) && (m_ptr->stunned)) chance /= 2;

		/* Monster can use ranged attacks */
		/* The monster must not be friendly, or a pet! */
		if ((chance) && (rand_int(100) < chance) && (is_hostile(m_ptr)))
		{
			/* Pick a ranged attack */
			choice = choose_ranged_attack(cave_m_idx[m_ptr->fy][m_ptr->fx], &tar_y, &tar_x, FALSE, FALSE);
		}
	
		/* Roll to use ranged attacks failed, but monster is an archer. */
		/* This seems to trigger regardless of any other circumstances - */
		/* The only requirement seems to be that choice failed */
		/* Adding (chance) */
		if ((choice == 0) && (r_ptr->flags2 & (RF2_ARCHER)) && (is_hostile(m_ptr)) && (chance))
		{
			/* Pick an archery attack (usually) */
			if ((!one_in_(5)) && (m_ptr->cdis > 1))
				choice = choose_ranged_attack(cave_m_idx[m_ptr->fy][m_ptr->fx], &tar_y, &tar_x, TRUE, FALSE);
		}
	
		/* For pets, check to make a ranged attack, slighty lower chance of casting a */
		/* spell versus another monster. . . (and wasting that mana) */
		/* NEWPETCODE - NOTE!!! I'm not sure how this is screwing up spell frequency */
		/* since it checks chance above, and again down here. */
		if ((choice == 0) && (rand_int(100) < chance))
		{
			if (monst_spell_monst(cave_m_idx[m_ptr->fy][m_ptr->fx])) return;
		}
	
		if ((choice == 0) && (r_ptr->flags2 & (RF2_ARCHER)) && (!is_hostile(m_ptr)) && (chance))
		{
				if (monst_spell_monst(cave_m_idx[m_ptr->fy][m_ptr->fx])) return;
		}

		/* Selected a ranged attack? */
		if (choice != 0)
		{
			/* Execute said attack */
			make_attack_ranged(m_ptr, choice, tar_y, tar_x);

			/* End turn */
			return;
		}

	}
	
	/*** Movement ***/

	/* Assume no movement */
	ty = 0;
	tx = 0;


	/*
	 * Innate semi-random movement.  Monsters adjacent to the character
	 * can always strike accurately at him (monster isn't confused).
	 */
	if ((r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25)) && (m_ptr->cdis > 1))
	{
		chance = 0;

		/* RAND_25 and RAND_50 are cumulative */
		if (r_ptr->flags1 & (RF1_RAND_25))
		{
			chance += 25;
			if (mon_fully_visible(m_ptr)) l_ptr->r_flags1 |= (RF1_RAND_25);
		}
		if (r_ptr->flags1 & (RF1_RAND_50))
		{
			chance += 50;
			if (mon_fully_visible(m_ptr)) l_ptr->r_flags1 |= (RF1_RAND_50);
		}

		/* Chance of moving randomly */
		if (rand_int(100) < chance) random_move = TRUE;
	}


	/* Monster cannot perceive the character */
	if ((!aware) && (!random_move))
	{
		/* Monster has a known target */
		if ((m_ptr->ty) && (m_ptr->tx)) must_use_target = TRUE;

		/* Monster is just going to have to search at random */
		else random_move = TRUE;
	}

	/* Monster is using the special "townsman" AI */
	else if (m_ptr->mflag & (MFLAG_TOWN))
	{
		/* Always have somewhere to go */
		if ((!m_ptr->ty) || (!m_ptr->tx) ||
		    (cave_shop_bold(m_ptr->fy, m_ptr->fx)))
		{
			/* Get a new target */
			get_town_target(m_ptr);
		}

		/* Not interested in the character */
		must_use_target = TRUE;
	}
	
	if (invisible)
	{
		random_move = TRUE;
	}


	/*** Find a target to move to ***/

	/* Monster is genuinely confused */
	if (m_ptr->confused)
	{
		/* Choose any direction except five and zero */
		dir = rand_int(8);

		/* Monster can try to wander into /anything/... */
		ty = m_ptr->fy + ddy_ddd[dir];
		tx = m_ptr->fx + ddx_ddd[dir];
	}

	/* Monster isn't confused, just moving semi-randomly */
	else if (random_move)
	{
		int start = rand_int(8);
		bool dummy;

		/* Is the monster scared? */
		if ((!(r_ptr->flags1 & (RF1_NEVER_MOVE))) &&
		    ((m_ptr->min_range >= FLEE_RANGE) ||
		     (m_ptr->monfear)))
		{
			fear = TRUE;
		}

		/* Look at adjacent grids, starting at random. */
		for (i = start; i < 8 + start; i++)
		{
			y = m_ptr->fy + ddy_ddd[i % 8];
			x = m_ptr->fx + ddx_ddd[i % 8];

			/* Accept first passable grid. */
			if (cave_passable_mon(m_ptr, y, x, &dummy) != 0)
			{
				ty = y;
				tx = x;
				break;
			}
		}

		/* No passable grids found */
		if ((ty == 0) && (tx == 0)) return;

		/* Cannot move, target grid does not contain the character */
		if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) &&
		    (cave_m_idx[ty][tx] >= 0))
		{
			/* Cannot move */
			return;
		}
	}
	
	/* Normal movement */
	/* NEWPETCODE -- right here is where the pet code chooses it's movement */
	/* Or gets a target. i.e. this is the body of the pet code - line 4825 of */
	/* melee2. What I think I should do here, is write a function that handles */
	/* pet movement */
	/* checks to see if the monster is friendly or a pet */
	else if (!is_hostile(m_ptr))
	{	
		/* If so checks to see if the monster is a pet */
		if (!is_pet(m_ptr)) friendly = TRUE;		
		/* Calculates to see the monsters target, takes into account if it's a friendly */
		if(!get_pet_move(m_ptr, &ty, &tx, must_use_target, friendly))  return;
	}
	else
	{
		/* Choose a pair of target grids, or cancel the move. */
		if (!get_move(m_ptr, &ty, &tx, &fear, must_use_target)) return;
	}

	/* Calculate the actual move.  Cancel move on failure to enter grid. */
	if (!make_move(m_ptr, &ty, &tx, fear, &bash)) return;

	/* Change terrain, move the monster, handle secondary effects. */
	process_move(m_ptr, ty, tx, bash);

	/* End turn */
	return;
}

/*
 * Monster regeneration of HPs and mana, and recovery from all temporary
 * conditions.
 *
 * This function is called a lot, and is therefore fairly expensive.
 */
static void recover_monster(monster_type *m_ptr, bool regen)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int frac;

	bool visible = FALSE;


	/* Visible monsters must be both seen and noticed */
	if (m_ptr->ml)
	{
		visible = TRUE;
	}

	/* Handle any area-effects of the monster - only if active */
	if ((r_ptr->flags8 & (RF8_CLOUD_SURROUND)) && (!m_ptr->csleep))
	{
		/* Assume no affect */
		bool affect = FALSE;

		int typ = 0, dam = 0, rad = 0;
#if 0
		/* The Nazgul always drain light */
		if (r_ptr->d_char == 'W') affect = TRUE;

		/* Silver jellies drain light only if their grid is lit */
		else if ((r_ptr->d_char == 'j') &&
		         (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_GLOW)))
		{
			affect = TRUE;
		}
#endif
		/* Other monsters wait for the character to approach */
		/*else*/ if (m_ptr->cdis <= 5) affect = TRUE;

		/* Affect surroundings if appropriate */
		if (affect)
		{
			/* Get information */
			cloud_surround(m_ptr->r_idx, &typ, &dam, &rad);

			/* Learn about monster (before visibility changes) */
			if ((m_ptr->ml) && (r_ptr->flags8 & (RF8_CLOUD_SURROUND)))
			{
				l_ptr->r_flags8 |= (RF8_CLOUD_SURROUND);
			}

			/* Release of cloud (can affect visibility) */
			if (typ) cloud(cave_m_idx[m_ptr->fy][m_ptr->fx], typ,
			                   dam, rad);
		}
	}

	/* Every 100 game turns, regenerate monsters */
	if (regen)
	{
		/*
		 * Hack -- in order to avoid a monster of 200 hitpoints having twice
		 * the regeneration of one with 199, and because we shouldn't randomize
		 * things (since we don't randomize character regeneration), we use
		 * current turn to smooth things out.
		 */
		int smooth = (turn / 100) % 100;

		/* Regenerate mana, if needed */
		if (m_ptr->mana < r_ptr->mana)
		{
			frac = (r_ptr->mana + smooth) / 100;

			/* Regenerate */
			m_ptr->mana += frac;

			/* Do not over-regenerate */
			if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;

			/* Fully healed -> flag minimum range for recalculation */
			if (m_ptr->mana == r_ptr->mana) m_ptr->min_range = 0;
		}

		/* Allow hp regeneration, if needed. */
		if (m_ptr->hp < m_ptr->maxhp)
		{
			frac = (m_ptr->maxhp + smooth) / 100;

			/* Some monsters regenerate quickly */
			if (r_ptr->flags2 & (RF2_REGENERATE)) frac *= 2;

			/* Minimal regeneration rate */
			if (!frac) frac = 1;

			/* Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Fully healed -> flag minimum range for recalculation */
			if (m_ptr->hp == m_ptr->maxhp) m_ptr->min_range = 0;
		}
	}


	/* Monster is sleeping, but character is within detection range */
	if ((m_ptr->csleep) && (m_ptr->cdis <= r_ptr->aaf))
	{
		/* Aggravated by the player */
		if (p_ptr->aggravate)
		{
			/* Reset sleep counter */
			m_ptr->csleep = 0;

			/* Notice the "waking up" */
			if (visible)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s wakes up.", m_name);
			}
		}

		/* Standard noise */
		else
		{
			int divisor, d;

			/* Monsters are disturbed less if far away */
			divisor = 300 + m_ptr->cdis * 200;

			/* Monsters are disturbed more if in LOS */
			if (player_has_los_bold(m_ptr->fy, m_ptr->fx)) divisor /= 2;

			/* Get disturbance (additional noise counts double) */
			d = div_round(total_wakeup_chance + add_wakeup_chance, divisor);

			/* Still asleep */
			if (m_ptr->csleep > d)
			{
				/* Monster's sleep is disturbed */
				m_ptr->csleep -= d;

				/* Notice the "not waking up" */
				if (visible)
				{
					/* Hack -- Count the ignores */
					if (l_ptr->r_ignore < MAX_UCHAR)
					{
						l_ptr->r_ignore++;
					}

					/* We are making a substantial amount of extra noise */
					if (add_wakeup_chance >= 1000)
					{
						char m_name[80];

						/* Acquire the monster name */
						monster_desc(m_name, m_ptr, 0);

						/* Warning */
						msg_format("%^s stirs.", m_name);
					}
				}
			}

			/* Just woke up */
			else
			{
				/* Reset sleep counter */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if (visible)
				{
					char m_name[80];

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);

					/* Hack -- Count the wakings */
					if (l_ptr->r_wake < MAX_UCHAR)
					{
						l_ptr->r_wake++;
					}
				}
			}
		}
	}

	/* Recover from stuns */
	if (m_ptr->stunned)
	{
		int d = 1;

		/* Make a "saving throw" against stun. */
		if (rand_int(180) < r_ptr->level + 10)
		{
			/* Recover fully */
			d = m_ptr->stunned;
		}

		/* Hack -- Recover from stun */
		if (m_ptr->stunned > d)
		{
			/* Recover somewhat */
			m_ptr->stunned -= d;
		}

		/* Fully recover */
		else
		{
			/* Recover fully */
			m_ptr->stunned = 0;

			/* Message if visible */
			if (visible)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer stunned.", m_name);
			}
		}
	}


	/* Recover from confusion */
	if (m_ptr->confused)
	{
		int d = randint(div_round(r_ptr->level, 5) + 1);

		/* Still confused */
		if (m_ptr->confused > d)
		{
			/* Reduce the confusion */
			m_ptr->confused -= d;
		}

		/* Recovered */
		else
		{
			/* No longer confused */
			m_ptr->confused = 0;

			/* Message if visible */
			if (visible)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer confused.", m_name);
			}
		}
	}


	/* Recover courage */
	if (m_ptr->monfear)
	{
		/* Random recovery from fear */
		int d = randint(div_round(r_ptr->level, 10) + 1);

		/* Still afraid */
		if (m_ptr->monfear > d)
		{
			/* Reduce the fear */
			m_ptr->monfear -= d;
		}

		/* Recover from fear, take note if seen */
		else
		{
			/* Cancel fear */
			set_mon_fear(m_ptr, 0, FALSE);

			/* Recalculate minimum range immediately */
			find_range(m_ptr);

			/* Visual note - only if monster isn't terrified */
			if ((visible) && (m_ptr->min_range < FLEE_RANGE))
			{
				char m_name[80];
				char m_poss[80];

				/* Acquire the monster name/poss */
				monster_desc(m_name, m_ptr, 0);
				monster_desc(m_poss, m_ptr, 0x22);

				/* Dump a message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);
			}
		}
	}

	/* Recover from Stasis */
	if (m_ptr->stasis)
	{
		/* Random recovery from fear */
		int d = randint(div_round(r_ptr->level, 4) + 1);
		
		/* Still held */
		if (m_ptr->stasis > d)
		{
			m_ptr->stasis -= d;
		}
		else 
		{
			/* reset stasis */
			m_ptr->stasis = 0;
			
			if (visible)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer held.", m_name);
				
			}
		}
	}

	/*
	 * Handle significantly hasted or slowed creatures.  Random variations
	 * are not large enough to activate this code.
	 */
	if ((m_ptr->mspeed > r_ptr->speed + 4) || (m_ptr->mspeed < r_ptr->speed - 4))
	{
		/* Uniques recover pretty quickly */
		int chance = ((r_ptr->flags1 & (RF1_UNIQUE)) ? 10 : 25);

		/* 4% or 10% chance that slowed monsters will return to normal speed. */
		if ((m_ptr->mspeed < r_ptr->speed) && (one_in_(chance)))
		{
			m_ptr->mspeed = r_ptr->speed;

			/* Visual note */
			if (visible)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Message. */
				msg_format("%^s is no longer slowed.", m_name);
			}
		}

		/* 2-4% chance that hasted monsters will return to normal speed. */
		else if ((m_ptr->mspeed > r_ptr->speed) &&
		         (one_in_(25 + r_ptr->level / 2)))
		{
			m_ptr->mspeed = r_ptr->speed;

			/* Visual note */
			if (visible)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Message. */
				msg_format("%^s is no longer hasted.", m_name);
			}
		}
	}

	/* Hack -- Update the health bar (always) */
	if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])
		p_ptr->redraw |= (PR_HEALTH);
}


/*
 * Process all living monsters, once per game turn.
 *
 * Scan through the list of all living monsters, (backwards, so we can
 * excise any "freshly dead" monsters).
 *
 * Every ten game turns, allow monsters to recover from temporary con-
 * ditions.  Every 100 game turns, regenerate monsters.  Give energy to
 * each monster, and allow fully energized monsters to take their turns.
 *
 * This function and its children are responsible for at least a third of
 * the processor time in normal situations.  If the character is resting,
 * this may rise substantially.
 */
void process_monsters(byte minimum_energy)
{
	int i = 0;
	monster_type *m_ptr = &m_list[i];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Only process some things every so often */
	bool recover = FALSE;
	bool regen = FALSE;

	/* Time out temporary conditions every ten game turns */
	if (turn % 10 == 0)
	{
		recover = TRUE;

		/* Regenerate hitpoints and mana every 100 game turns */
		if (turn % 100 == 0) regen = TRUE;
	}

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Player is dead or leaving the current level */
		if (p_ptr->leaving) break;

		/* Access the monster */
		m_ptr = &m_list[i];

		/* Ignore dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* Calculate "upkeep" for pets */
		if (is_pet(m_ptr))
		{
			total_friends++;
			total_friend_levels += r_ptr->level;
		}

		/* Ignore monsters that have already been handled */
		if (m_ptr->mflag & (MFLAG_MOVE)) continue;

		/* Leave monsters without enough energy for later */
		if (m_ptr->energy < minimum_energy) continue;

		/* Prevent reprocessing */
		m_ptr->mflag |= (MFLAG_MOVE);

		/* Handle temporary monster attributes every ten game turns */
		if (recover) recover_monster(m_ptr, regen);

		/* Give this monster some energy */
		m_ptr->energy += extract_energy[m_ptr->mspeed];

		/* End the turn of monsters without enough energy to move */
		if (m_ptr->energy < 100) continue;

		/* Use up some energy */
		m_ptr->energy -= 100;

		/* Let the monster take its turn */
		process_monster(m_ptr);
	}
}


/*
 * Clear 'moved' status from all monsters.
 *
 * Clear noise if appropriate.
 */
void reset_monsters(void)
{
	int i;
	monster_type *m_ptr;

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Monster is ready to go again */
		m_ptr->mflag &= ~(MFLAG_MOVE);
	}
}
