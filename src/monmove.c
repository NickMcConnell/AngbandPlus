/* File: monmove.c */

/*
 * Monster movement and ranged attack AI.
 *
 * Ranges, learning, tests for terrain suitability and character resists.
 * The spellcasting AI.  The movement AI.  Move a monster, process effects.
 * Regenerate and recover.  Move all monsters.
 *
 * Copyright (c) 2007 Leon Marrick & Bahman Rabii, Ben Harrison,
 * James E. Wilson, Robert A. Koeneke
 *
 * Additional code and concepts by David Reeve Sward, Keldon Jones,
 * and others.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
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
	    (m_ptr->mana < r_ptr->mana / 6)) m_ptr->min_range = FLEE_RANGE;

	/* Hack -- townsmen go about their business */
	else if (m_ptr->mflag & (MFLAG_TOWN)) m_ptr->min_range = 2;

	/* Allow monster terror */
	else
	{
		/* Minimum distance - stay at least this far if possible */
		m_ptr->min_range = 1;

		/* Examine player power  */
		p_lev = p_ptr->power / 2;

		/* Examine monster power (level plus morale) */
		m_lev = r_ptr->level +
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
			{
				m_ptr->best_range = m_ptr->min_range = FLEE_RANGE;
				return;
			}
		}
	}

	/* If the monster has a specified combat range, use it */
	if ((r_ptr->combat_range) && (m_ptr->min_range != FLEE_RANGE))
	{
		/* Use given range as preferred range */
		m_ptr->best_range = r_ptr->combat_range;

		/* Allow designer to specify minimum range as well */
		if (r_ptr->combat_range >= 6)
			m_ptr->min_range = MIN(5, r_ptr->combat_range / 2);
		else
			m_ptr->min_range = 1;

		/* Done */
		return;
	}


	/* Find preferred range for monsters without a specified range */
	m_ptr->best_range = m_ptr->min_range;

	/* Archers are quite happy at a good distance */
	if (r_ptr->flags2 & (RF2_ARCHER)) m_ptr->best_range += 3;

	/* Heavy spell casters */
	if (r_ptr->freq_ranged > 24)
	{
		/* Heavy spell casters will sit back and cast (needs work) */
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
const byte side_dirs[20][8] =
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

	/* Canines and Zephyr Hounds are amazing trackers */
	if (strchr("CZ", r_ptr->d_char))
	{
		/* I smell a character! */
		return (TRUE);
	}

	/* So are the Nazgul */
	else if ((strchr("W", r_ptr->d_char)) &&
	         (r_ptr->flags1 & (RF1_UNIQUE)))
	{
		/* Bloodscent! */
		return (TRUE);
	}

	/* Other monsters can sometimes make good use of scent */
	else if (strchr("fkoqyHORTY", r_ptr->d_char))
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
			if (cave_glyph(y, x)) continue;

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

	/* Know weirdness */
	if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
	if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
	if (p_ptr->skill_sav >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
	if (p_ptr->skill_sav >= 100) m_ptr->smart |= (SM_PERF_SAVE);

	/* Know immunities */
	if (p_ptr->immune_acid) m_ptr->smart |= (SM_IMM_ACID);
	if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
	if (p_ptr->immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
	if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);

	/* Know oppositions */
	if (p_ptr->oppose_acid) m_ptr->smart |= (SM_OPP_ACID);
	if (p_ptr->oppose_elec) m_ptr->smart |= (SM_OPP_ELEC);
	if (p_ptr->oppose_fire) m_ptr->smart |= (SM_OPP_FIRE);
	if (p_ptr->oppose_cold) m_ptr->smart |= (SM_OPP_COLD);
	if (p_ptr->oppose_pois) m_ptr->smart |= (SM_OPP_POIS);

	/* Know resistances */
	if (p_ptr->resist_acid) m_ptr->smart |= (SM_RES_ACID);
	if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
	if (p_ptr->resist_fire) m_ptr->smart |= (SM_RES_FIRE);
	if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
	if (p_ptr->resist_pois) m_ptr->smart |= (SM_RES_POIS);
	if (p_ptr->resist_fear) m_ptr->smart |= (SM_RES_FEAR);
	if (p_ptr->resist_lite) m_ptr->smart |= (SM_RES_LITE);
	if (p_ptr->resist_dark) m_ptr->smart |= (SM_RES_DARK);
	if (p_ptr->resist_blind) m_ptr->smart |= (SM_RES_BLIND);
	if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
	if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
	if (p_ptr->resist_shard) m_ptr->smart |= (SM_RES_SHARD);
	if (p_ptr->resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
	if (p_ptr->resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
	if (p_ptr->resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
	if (p_ptr->resist_disen) m_ptr->smart |= (SM_RES_DISEN);

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
			if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS))) return (10);
			else return (0);
		}
		/* Acid Spells */
		case LRN_ACID:
		{
			if (smart & (SM_IMM_ACID)) return (100);
			else if ((smart & (SM_OPP_ACID)) && (smart & (SM_RES_ACID))) return (70);
			else if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID))) return (40);
			else return (0);
		}
		/* Lightning Spells */
		case LRN_ELEC:
		{
			if (smart & (SM_IMM_ELEC)) return (100);
			else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC))) return (70);
			else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC))) return (40);
			else return (0);
		}
		/* Fire Spells */
		case LRN_FIRE:
		{
			if (smart & (SM_IMM_FIRE)) return (100);
			else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE))) return (70);
			else if ((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE))) return (40);
			else return (0);
		}
		/* Cold Spells */
		case LRN_COLD:
		{
			if (smart & (SM_IMM_COLD)) return (100);
			else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD))) return (70);
			else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD))) return (40);
			else return (0);
		}
		/* Ice Spells */
		case LRN_ICE:
		{
			if (smart & (SM_IMM_COLD)) a=90;
			else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD))) a = 60;
			else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD))) a = 30;
			else a = 0;
			if (smart & (SM_RES_SOUND)) a += 5;
			if (smart & (SM_RES_SHARD)) a += 5;
			return (a);
		}
		/* Poison Spells */
		case LRN_POIS:
		{
			if ((smart & (SM_OPP_POIS)) && (smart & (SM_RES_POIS))) return (80);
			else if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS))) return (55);
			else return (0);
		}
		/* Plasma Spells */
		case LRN_PLAS:
		{
			a=0;
			if (smart & (SM_IMM_FIRE)) a += 50;
			else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE))) a += 35;
			else if ((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE))) a += 20;
			if (smart & (SM_IMM_ELEC)) a += 50;
			else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC))) a += 35;
			else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC))) a += 20;
			return (a);
		}
		/* Light Spells */
		case LRN_LITE:
		{
			if (smart & (SM_RES_LITE)) return (30);
			else return (0);
		}
		/* Darkness Spells */
		case LRN_DARK:
		{
			if (smart & (SM_RES_DARK)) return (30);
			else return (0);
		}
		/* Confusion Spells, damage dealing */
		case LRN_CONFU:
		{
			if (smart & (SM_RES_CONFU)) return (30);
			else return (0);
		}
		/* Sound Spells */
		case LRN_SOUND:
		{
			a=0;
			if (smart & (SM_RES_SOUND)) a += 30;
			if (smart & (SM_RES_CONFU)) a += 10;
			if (smart & (SM_PERF_SAVE)) a += 10;
			else if (smart & (SM_GOOD_SAVE)) a += 5;
			else return (a);
		}
		/* Irresistible, but sound prevents stun */
		case LRN_SOUND2:
		{
			if (smart & (SM_RES_SOUND)) return (5);
			else return (0);
		}
		/* Shards Spells */
		case LRN_SHARD:
		{
			if (smart & (SM_RES_SHARD)) return (30);
			else return (0);
		}
		/* Nexus Spells */
		case LRN_NEXUS:
		{
			if (smart & (SM_RES_NEXUS)) return (30);
			else return (0);
		}
		/* Nether Spells */
		case LRN_NETHR:
		{
			if (smart & (SM_RES_NETHR)) return (30);
			else return (0);
		}
		/* Chaos Spells */
		case LRN_CHAOS:
		{
			if (smart & (SM_RES_CHAOS)) return (30);
			else return (0);
		}
		/* Disenchantment Spells */
		case LRN_DISEN:
		{
			if (smart & (SM_RES_DISEN)) return (30);
			else return (0);
		}
		/* Storm Spells */
		case LRN_STORM:
		{
			a=0;
			if (smart & (SM_IMM_ELEC)) a += 15;
			else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC))) a += 10;
			else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC))) a += 5;
			if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)) ||(smart & (SM_IMM_COLD))) a += 5;
			if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID)) ||(smart & (SM_IMM_ACID))) a += 5;
			if (smart & (SM_RES_CONFU)) a += 10;
			return (a);
		}
		/* Water Spells */
		case LRN_WATER:
		{
			a=0;
			if (smart & (SM_RES_CONFU)) a += 10;
			if (smart & (SM_RES_SOUND)) a += 5;
			return (a);
		}
		/* Wind Spells */
		case LRN_WIND:
		{
			if (smart & (SM_RES_CONFU)) return (5);
			else return (0);
		}
		/* Spells that attack player mana */
		case LRN_MANA:
		{
			if (smart & (SM_IMM_MANA)) return (100);
			else return (0);
		}
		/* Spells Requiring Save or Resist Nexus */
		case LRN_NEXUS_SAVE:
		{
			if (smart & (SM_RES_NEXUS)) return (100);
			else if (smart & (SM_PERF_SAVE)) return (100);
			else if (smart & (SM_GOOD_SAVE)) return (30);
			else return (0);
		}
		/* Spells Requiring Save or Resist Fear */
		case LRN_FEAR_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_FEAR)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->afraid) a += 50;
			}
			return (a);
		}
		/* Spells Requiring Save or Resist Blindness */
		case LRN_BLIND_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_BLIND)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->blind) a += 50;
			}
			return (a);
		}
		/* Spells Requiring Save or Resist Confusion */
		case LRN_CONFU_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_CONFU)) a = 100;
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
			else return (0);
		}

		/* Spells Requiring Darkness/Save */
		case LRN_DARK_SAVE:
		{
			a = 0;

			if (smart & (SM_RES_DARK)) a += 25;

			if (smart & (SM_PERF_SAVE)) a += 25;
			else if (smart & (SM_GOOD_SAVE)) a += 15;
			return (a);
		}

		/* Spells Requiring Holy save */
		case LRN_HOLY_SAVE:
		{
			if (p_ptr->realm == PRIEST) return (75);
			else if (p_ptr->realm != NECRO) return (25);
			else return (0);
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
static void remove_expensive_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p)
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
	if (r_ptr->flags2 & (RF2_SMART))
		max_cost=(m_ptr->mana * (rand_range(4, 10))) / 10;

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
 * Monsters don't use spells out of range.
 */
static void remove_limited_range_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p)
{
	monster_type *m_ptr = &m_list[m_idx];
	int dist = m_ptr->cdis;
	int i;
	u32b flag;

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);


	/* Don't lash if too far or close */
	if (f4 & (RF4_LASH))
	{
		if ((dist > 3) || (dist < 2)) f4 &= ~(RF4_LASH);
	}

	/* Remove all ranged attacks that are strictly limited range */
	for (i = 0, flag = 1L; i < 32; i++, flag <<= 1)
	{
		/*
		 * If spell is limited range, and has no desirability past that
		 * range, remove it.
		 */
		if ((f4 & (flag)) && (spell_range_RF4[i] < dist) &&
		    (spell_desire_RF4[i][D_RANGE] == 0))
		{
			f4 &= ~(flag);
		}
		if ((f5 & (flag)) && (spell_range_RF5[i] < dist) &&
		    (spell_desire_RF5[i][D_RANGE] == 0))
		{
			f5 &= ~(flag);
		}
		if ((f6 & (flag)) && (spell_range_RF6[i] < dist) &&
		    (spell_desire_RF6[i][D_RANGE] == 0))
		{
			f6 &= ~(flag);
		}
		if ((f7 & (flag)) && (spell_range_RF7[i] < dist) &&
		    (spell_desire_RF7[i][D_RANGE] == 0))
		{
			f7 &= ~(flag);
		}
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
static void remove_useless_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p, bool require_los)
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
	if (m_ptr->hasted) f6 &= ~(RF6_HASTE);

	/* Don't cure if not needed */
	if (!((m_ptr->stunned) ||(m_ptr->monfear) ||
	      (m_ptr->slowed) || (m_ptr->mflag & (MFLAG_BLBR))))
		f6 &= ~(RF6_CURE);

	/* Don't jump in already close, or don't want to be close */
	if (!(m_ptr->cdis > m_ptr->best_range) && require_los)
		f6 &= ~(RF6_TELE_SELF_TO);
	if (m_ptr->min_range > 5) f6 &= ~(RF6_TELE_SELF_TO);

	/* Don't cast tele_to if you're adjacent already */
	if (m_ptr->cdis == 1) f6 &= ~(RF6_TELE_TO);

	/* Don't breathe at players in a wall */
	/* Reconsider this rule  XXX XXX XXX DEBUG */
	if (cave_wall_bold(p_ptr->py, p_ptr->px))
	{
		f4 &= ~(RF4_BREATH_MASK);
		f5 &= ~(RF5_BREATH_MASK);
		f6 &= ~(RF6_BREATH_MASK);
		f7 &= ~(RF7_BREATH_MASK);
	}

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
static int choose_ranged_attack(int m_idx, bool archery_only)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	byte *spell_desire;

	u32b f4, f5, f6, f7;

	byte spell_range;

	bool do_random = FALSE;

	bool require_los = TRUE;
	bool splash_ball = FALSE;

	bool is_harass = FALSE;
	bool is_best_harass = FALSE;
	bool is_breath = FALSE;

	int i, s, py = p_ptr->py, px = p_ptr->px;
	int breath_hp, breath_maxhp, path, spaces;

	int want_hps=0, want_escape=0, want_mana=0, want_summon=0;
	int want_tactic=0, cur_range=0;

	int best_spell=0, best_spell_rating=0;
	int cur_spell_rating;

	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;
	f7 = r_ptr->flags7;

	/* Check what kinds of spells can hit player */
	path = projectable(m_ptr->fy, m_ptr->fx, py, px, PROJECT_CHCK);

	/* Are we restricted to archery? */
	/* Note - we have assumed for speed that no archery attacks
	 * cost mana, all are 'bolt' like, and that the player can not
	 * be highly resistant to them. */
	if (archery_only)
	{
		/* check for a clean shot */
		if (!(path == PROJECT_CLEAR)) return (0);

		/* restrict to archery */
		f4 &= (RF4_ARCHERY_MASK);
		f5 &= (RF5_ARCHERY_MASK);
		f6 &= (RF6_ARCHERY_MASK);
		f7 &= (RF7_ARCHERY_MASK);

		/* choose at random from restricted list */
		return (choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, TRUE));
	}

	/* do we have the player in sight at all? */
	if (path==PROJECT_NO)
	{
		/* Flat out 75% chance of not casting if the player is not in sight */
		/* In addition, most spells don't work without a player around */
		if (!one_in_(4)) return (0);

		/* Try to abuse LOS against player */
		/* Note, this interfaces poorly with monsters not knowing where the player is */
		for (i = 0, s = rand_int(8); i < 8; i++, s++)
		{
			/* Require casting onto actual squares */
			if (!cave_project_bold(py + ddy[ddc[s % 8]], px + ddx[ddc[s % 8]]))
				continue;

			/* Check path to square */
			path = projectable(m_ptr->fy, m_ptr->fx, py + ddy[ddc[s % 8]], px + ddx[ddc[s % 8]], PROJECT_CHCK);

			if (path != PROJECT_NO)
			{
				splash_ball = TRUE;

				/* Adjust target */
				py = py + ddx[ddc[s % 8]];
				px = px + ddx[ddc[s % 8]];
				break;
			}
		}

		require_los=FALSE;
	}

	/* Special case -- Angels avoid users of holy lore */
	if ((p_ptr->realm == PRIEST) && (r_ptr->d_char == 'A') &&
	    (!p_ptr->unsanctified)) return (0);


	/* Remove spells the 'no-brainers'*/
	/* Can cast spells that would deal splash damage to player or that don't require LOS */
	if (splash_ball)
	{
		f4 &= (RF4_NO_PLAYER_MASK | RF4_BALL_MASK);
		f5 &= (RF5_NO_PLAYER_MASK | RF5_BALL_MASK);
		f6 &= (RF6_NO_PLAYER_MASK | RF6_BALL_MASK);
		f7 &= (RF7_NO_PLAYER_MASK | RF7_BALL_MASK);
	}

	/* Can only cast spells that don't require LOS */
	else if (!require_los)
	{
		f4 &= (RF4_NO_PLAYER_MASK);
		f5 &= (RF5_NO_PLAYER_MASK);
		f6 &= (RF6_NO_PLAYER_MASK);
		f7 &= (RF7_NO_PLAYER_MASK);
	}

	/* Remove bolt spells */
	else if (path==PROJECT_NOT_CLEAR)
	{
		f4 &= ~(RF4_BOLT_MASK);
		f5 &= ~(RF5_BOLT_MASK);
		f6 &= ~(RF6_BOLT_MASK);
		f7 &= ~(RF7_BOLT_MASK);
	}

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7) return (0);

	/* Spells we can not afford */
	remove_expensive_spells(m_idx, &f4, &f5, &f6, &f7);

	/* Spells that won't reach that far */
	remove_limited_range_spells(m_idx, &f4, &f5, &f6, &f7);

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

	/* Cheat if requested, or if a player ghost. */
	if ((birth_smart_cheat) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
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
	feature_type *f_ptr;


	/* Check bounds */
	if (!in_bounds(y, x)) return (FALSE);

	/* The grid is already occupied. */
	if (cave_m_idx[y][x] != 0)
	{
		if (!occupied_ok) return (FALSE);
	}

	/* Glyphs -- must break first */
	if ((num_glyph_on_level) && (cave_glyph(y, x))) return (FALSE);


	/* Get information about the feature in this grid */
	f_ptr = &f_info[cave_feat[y][x]];


	/* Floors -- safe for everything */
	if (f_ptr->flags & (TF_FLOOR)) return (TRUE);

	/* Walls and closed doors */
	else if ((f_ptr->flags & (TF_WALL)) || (f_ptr->flags & (TF_DOOR_CLOSED)))
	{
		/* Test later */
	}

	/* Passable features other than floors and closed doors */
	else if (f_ptr->flags & (TF_PASSABLE))
	{
		int feat = cave_feat[y][x];

		/* Earthbound demons, firebreathers, and red elementals cannot handle water */
		if (feat == FEAT_WATER)
		{
			if (r_ptr->flags2 & (RF2_FLYING)) return (TRUE);

			if ((r_ptr->flags4 & (RF4_BRTH_FIRE)) ||
			    (strchr("I&", r_ptr->d_char)) ||
			   ((strchr("E", r_ptr->d_char)) &&
			   ((r_ptr->d_attr == TERM_RED) || (r_ptr->d_attr == TERM_L_RED))))
			{
				return (FALSE);
			}

			else return (TRUE);
		}

		/* Only fiery or strong flying creatures can handle lava */
		if (feat == FEAT_LAVA)
		{
			if (r_ptr->flags3 & (RF3_IM_FIRE)) return (TRUE);
			else if (r_ptr->flags2 & (RF2_FLYING))
			{
				/* Get HPs */
				int hp = r_ptr->hitpoints;

				/* Only strong monsters */
				if (hp >= 50) return (TRUE);
			}

			return (FALSE);
		}

		/* Anything else that's passable we assume to be legal. */
		return (TRUE);
	}


	/*** Feature is a wall or closed door ***/

	/* Permanent walls are never OK */
	if (f_ptr->flags & (TF_PERMANENT)) return (FALSE);

	/* Monster can pass through walls */
	if (r_ptr->flags2 & (RF2_PASS_WALL)) return (TRUE);

	/* Monster can dig through walls, and is allowed to. */
	if ((r_ptr->flags2 & (RF2_KILL_WALL)) && (can_dig)) return (TRUE);

	/* Most monsters cannot exist in walls or closed doors */
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
 *
 * Issue:  We do not take into account the fact that it takes two
 * turns to handle most doors:  one to open the door, and another to
 * move into the doorway.
 */
static int cave_passable_mon(monster_type *m_ptr, int y, int x, bool *bash)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	feature_type *f_ptr;

	/* Assume nothing in the grid other than the terrain hinders movement */
	int move_chance = 100;

	int feat;

	/* Assume no bash */
	*bash = FALSE;

	/* Check Bounds */
	if (!in_bounds(y, x)) return (0);

	/* Check location */
	feat = cave_feat[y][x];

	/* The grid is occupied by the player. */
	if (cave_m_idx[y][x] < 0)
	{
		/* Monster has no melee blows - character's grid is off-limits. */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (0);
	}

	/* The grid is occupied by a monster. */
	else if (cave_m_idx[y][x] > 0)
	{
		monster_type *n_ptr = &m_list[cave_m_idx[y][x]];
		monster_race *nr_ptr = &r_info[n_ptr->r_idx];

		/* Kill weaker monsters */
		if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
		    (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
		    (r_ptr->mexp > nr_ptr->mexp))
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
		else
		{
			return (0);
		}


		/* Uniques are better at pushing past monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) move_chance += 20;

		/* Make sure return value does not exceed 100 */
		if (move_chance > 100) move_chance = 100;
	}

	/* Glyphs */
	if ((num_glyph_on_level) && (cave_glyph(y, x)))
	{
		/* Glyphs are hard to break */
		if (move_chance > 100 * r_ptr->level / BREAK_GLYPH)
		    move_chance = 100 * r_ptr->level / BREAK_GLYPH;
	}


	/* The grid is occupied by the player */
	if (cave_m_idx[y][x] < 0)
	{
		/* Any terrain can be attacked over */
		return (move_chance);
	}

	/* The grid is being hit by an effect */
	if (cave_info[y][x] & (CAVE_EFFT))
	{
		/* Prefer not to enter (this bit needs work) */
		if (move_chance > 33) move_chance = 33;
	}


	/* Get information about the feature in this grid */
	f_ptr = &f_info[cave_feat[y][x]];


	/* Floors -- safe for everything */
	if (f_ptr->flags & (TF_FLOOR)) return (move_chance);

	/* Non-floor passable features and doors */
	else if (f_ptr->flags & (TF_PASSABLE | TF_DOOR_ANY))
	{
		int feat = cave_feat[y][x];

		/* Closed doors */
		if (f_ptr->flags & (TF_DOOR_CLOSED))
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


					/* No special problems */
					return (MIN(100, move_chance));
				}

				/*
				 * Locked doors (not jammed).  Monsters know how hard
				 * doors in their neighborhood are to unlock.
				 */
				else if (feat < FEAT_DOOR_HEAD + 0x08)
				{
					int lock_power, ability;

					/* Door power (from 50 to 350) */
					lock_power = 50 * (feat - FEAT_DOOR_HEAD);

					/* Calculate unlocking ability (usu. 10 to 90) */
					ability = (r_ptr->level / 2) + 10;
					if (r_ptr->flags2 & (RF2_SMART)) ability = 3 * ability / 2;
					if (strchr("ph", r_ptr->d_char)) ability = 3 * ability / 2;

					/* Chance varies from 3% to over 100%. */
					unlock_chance = MAX(3, (100 * ability / lock_power));
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
				if (feat >= FEAT_DOOR_HEAD)
					door_power = 80 + 80 * ((feat - FEAT_DOOR_HEAD) % 8);
				else
					door_power = 80;

				/*
				 * Calculate bashing ability (usu. 6 to 290).  Note:
				 * This formula assumes Oangband-style HPs.
				 */
				bashing_power = 5 + r_ptr->level + m_ptr->hp / 15;

				if (r_ptr->flags3 & (RF3_GIANT | RF3_TROLL))
					bashing_power = 3 * bashing_power / 2;

				/*
				 * Chance varies from 1% to over 100%.  Note that
				 * monsters "fall" into the entranceway in the same
				 * turn that they bash the door down.
				 */
				bash_chance = MAX(1, (100 * bashing_power / door_power));
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

			/* For most monsters, rubble takes more time to cross. */
			else return (MIN(25, move_chance));
		}

		/* Trees */
		if (feat == FEAT_TREE)
		{
			/* Some monsters can pass right through trees */
			if (r_ptr->flags2 & (RF2_PASS_WALL)) return (move_chance);

			/* Some monsters know trees well */
			if (r_ptr->flags3 & (RF3_ANIMAL))
			{
				return (move_chance);
			}
			else
			{
				/* For many monsters, trees take more time to cross. */
				return (MIN(50, move_chance));
			}
		}

		if (feat == FEAT_WATER)
		{
			/* Flying monsters can always cross water */
			if (r_ptr->flags2 & (RF2_FLYING)) return (move_chance);

			/* Some monsters can pass right over water */
			if (r_ptr->flags2 & (RF2_PASS_WALL)) return (move_chance);

			/* If it resists water, it can move easily in it */
			if (r_ptr->flags3 & (RF3_RES_WATER)) return (move_chance);

			/* Earthbound demons and firebreathers don't like water */
			if (r_ptr->flags4 & (RF4_BRTH_FIRE)) return (MIN(25, move_chance));
			if (strchr("I&", r_ptr->d_char)) return (MIN(25, move_chance));

			/* "Red" elementals don't like water */
			if (strchr("E", r_ptr->d_char) &&
			   ((r_ptr->d_attr == TERM_RED) || (r_ptr->d_attr == TERM_L_RED)))
			{
				return (MIN(25, move_chance));
			}

			/* Animals are (usually) comfortable in water */
			if (r_ptr->flags3 & (RF3_ANIMAL)) return (MIN(75, move_chance));

			/* For many monsters, water take more time to cross. */
			return (MIN(50, move_chance));
		}

		/* Lava */
		if (feat == FEAT_LAVA)
		{
			/* Only fiery or strong flying creatures will cross lava */
			if (r_ptr->flags3 & (RF3_IM_FIRE)) return (move_chance);

			if ((r_ptr->flags2 & (RF2_FLYING)) && (m_ptr->hp >= 50))
				return (move_chance);

			/* Everything else doesn't want to enter lava */
			return (MIN(20, move_chance));
		}

		/* Anything else we assume to be easily passable. */
		return (move_chance);
	}

	/* Permanent walls are impassable */
	else if (f_ptr->flags & (TF_PERMANENT))
	{
		return (0);
	}


	/*** Feature is not ordinarily passable ***/


	/* Can the monster move easily through walls? */
	if ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
		(r_ptr->flags2 & (RF2_KILL_WALL)))
	{
		/* Allow easy movement */
		return (move_chance);
	}

	/* If not, it cannot move through */
	return (0);
}


/*
 * Maddened monsters will often attack adjacent monsters in melee.  They
 * only do this if they are "maddened by the presence of the character".
 * The end effect is something like that of Nethack's Rings of Conflict.
 *
 * There are a very large number of omissions in this code.  If, as, and
 * when monsters attacking other monsters becomes an important feature of
 * Sangband, a rewrite will become essential.  I direct your attention to
 * Zangband, ToME, Hengband, and other variants with well-developed code.
 */
static bool melee_another_monster(int m_idx, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int y, x, i;

	/* Store grids with monsters or the character */
	u16b occupied[8];
	int occupied_grids = 0;


	/* Monster cannot melee */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* Scan adjacent grids */
	for (i = 0; i < 8; i++)
	{
		/* Get coordinates */
		y = fy + ddy_ddd[i];
		x = fx + ddx_ddd[i];

		/* Note occupied grids */
		if (cave_m_idx[y][x])
		{
			occupied[occupied_grids] = GRID(y, x);
			occupied_grids++;
		}
	}

	/* No adjacent monsters */
	if (!occupied_grids)
	{
		/* Clear target */
		y = 0;    x = 0;

		/* Maddened monster may target another monster */
		if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
			mad_mon_retarget(fy, fx, &y, &x);

		/* Adjust target if appropriate */
		if ((y) && (x))
		{
			m_ptr->ty = y;
			m_ptr->tx = x;
		}

		/* Haven't done anything yet */
		return (FALSE);
	}

	/* Randomly choose among the occupied grids */
	i = rand_int(occupied_grids);

	/* Get the grid */
	y = GRID_Y(occupied[i]);
	x = GRID_X(occupied[i]);

	/* Attack the character */
	if (cave_m_idx[y][x] < 0)
	{
		(void)make_attack_normal(m_ptr, y, x);
	}

	/* Attack whatever monster is in this grid  XXX */
	else
	{
		mad_mon_melee(m_idx, m_ptr, y, x);
	}

	/* We successfully went insane. */
	return (TRUE);
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
		/* Hard-coded shop locations  XXX XXX XXX */
		i = FEAT_SHOP_HEAD + rand_int(MAX_STORES);

		/* Try to find the store XXX XXX */
		for (y = 1; y < dungeon_hgt - 1; y++)
		{
			for (x = 1; x < dungeon_wid - 1; x++)
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
					x = rand_range(1, dungeon_wid - 2);

					if (one_in_(2)) y = 1;
					else                  y = dungeon_hgt - 2;
				}
				else
				{
					/* Pick a random location along the E/W walls */
					y = rand_range(1, dungeon_hgt - 2);

					if (one_in_(2)) x = 1;
					else                  x = dungeon_wid - 2;
				}
			}
			else
			{
				y = rand_range(1, dungeon_hgt - 2);
				x = rand_range(1, dungeon_wid - 2);
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

					/* Check bounds */
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

						/* Character does not have los to this grid */
						if (!player_has_los_bold(yy - conv_y, xx - conv_x))
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
										/* Not a one-grid cu-de-sac */
										has_escape = TRUE;
										break;
									}
								}

								/* Ignore cu-de-sacs */
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
 * down to is that monsters can now run around corridor corners properly!
 *
 * Return TRUE if the monster did actually want to do anything.
 */
static bool get_move_retreat(monster_type *m_ptr, int *ty, int *tx)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i;
	int y, x;

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
				int greatest_cost = start_cost;
				int grid_n = grids_in_radius[3];

				/* Scan local grids (up to radius 3) */
				for (i = 1; i < grid_n; i++)
				{
					y = m_ptr->fy + nearby_grids_y[i];
					x = m_ptr->fx + nearby_grids_x[i];

					/* Check bounds */
					if (!in_bounds(y, x)) continue;

					/* Go for higher cost from character */
					if (cave_cost[y][x] > greatest_cost)
					{
						if (!player_has_los_bold(y, x))
						{
							greatest_cost = cave_cost[y][x];
							*ty = y;  *tx = x;
						}
					}
				}

				/* If we found a valid grid, accept it */
				if (greatest_cost > start_cost) return (TRUE);
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
		if (find_safety(m_ptr, ty, tx)) return (TRUE);

		/*
		 * No safe place found.  If monster is in LOS and close,
		 * it will turn to fight.
		 */
		if ((player_has_los_bold(m_ptr->fy, m_ptr->fx)) &&
		    (m_ptr->cdis < TURN_RANGE))
		{
			/* If panicked, lose a turn */
			bool lose_turn = ((m_ptr->monfear) ? TRUE : FALSE);

			/* Turn and fight */
			set_mon_fear(m_ptr, 0, FALSE);

			/* Forget target */
			m_ptr->ty = 0;    m_ptr->tx = 0;

			/* Charge!  XXX XXX */
			m_ptr->min_range = 1;  m_ptr->best_range = 1;

			/* Visible, but not adjacent */
			if ((m_ptr->ml) && (m_ptr->cdis > 1))
			{
				char m_name[DESC_LEN];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0x40);

				/* Dump a message */
				msg_format("%^s turns to fight!", m_name);
			}

			/* Charge! */
			*ty = p_ptr->py;
			*tx = p_ptr->px;

			/* Sometimes lose a turn, sometimes react immediately */
			return (!lose_turn);
		}
	}

	/* Move directly away from character. */
	*ty = -(p_ptr->py - m_ptr->fy);
	*tx = -(p_ptr->px - m_ptr->fx);

	/* We want to run away */
	return (TRUE);
}

/*
 * Check to see if any my friends I can see are hurt.
 *
 * Used by pack AI to rush player if he is using ball spells during an ambush.
 */

bool comrade_hurt(monster_type *m_ptr, bool include_self)
{
	monster_type *n_ptr;
	int i;

	if (m_ptr->hp < m_ptr->maxhp && include_self) return (TRUE);

	/* Loop over all monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		n_ptr = &m_list[i];

		/* Check monster index */
		if (n_ptr->r_idx != m_ptr->r_idx) continue;

		/* Check HP */
		if (n_ptr->hp == n_ptr->maxhp) continue;

		/* Check LOS */
		if (!projectable(n_ptr->fy, n_ptr->fx, m_ptr->fy, m_ptr->fx, PROJECT_JUMP)) continue;

		/* We can see a hurt comrade */
		return (TRUE);
	}

	return (FALSE);
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

	/*
	 * Monsters that cannot move will attack the character if he is
	 * adjacent.  Otherwise, they cannot move.
	 */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE))
	{
		/* Hack -- memorize lack of moves after a while. */
		if (!(l_ptr->flags1 & (RF1_NEVER_MOVE)))
		{
			if ((mon_fully_visible(m_ptr)) && (one_in_(20)))
				l_ptr->flags1 |= (RF1_NEVER_MOVE);
		}

		/* Is character in range? */
		if (m_ptr->cdis <= 1)
		{
			/* Monster can't melee either (pathetic little creature) */
			if (r_ptr->flags1 & (RF1_NEVER_BLOW))
			{
				/* Hack -- memorize lack of attacks after a while */
				if (!(l_ptr->flags1 & (RF1_NEVER_BLOW)))
				{
					if ((mon_fully_visible(m_ptr)) && (one_in_(10)))
						l_ptr->flags1 |= (RF1_NEVER_BLOW);
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

	/* Is the monster scared? */
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
			if (m_ptr->min_range <= m_ptr->cdis)
			{
				/* Cancel fear */
				*fear = FALSE;

				/* Visual note */
				if (mon_fully_visible(m_ptr))
				{
					char m_name[DESC_LEN];
					char m_poss[DESC_LEN];

					/* Get the monster name/poss */
					monster_desc(m_name, m_ptr, 0x40);
					monster_desc(m_poss, m_ptr, 0x22);

					/* Dump a message */
					msg_format("%^s turns to fight!", m_name);
				}
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
			if (!(l_ptr->flags1 & (RF1_NEVER_BLOW)))
			{
				if ((mon_fully_visible(m_ptr)) && (one_in_(10)))
					l_ptr->flags1 |= (RF1_NEVER_BLOW);
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
	if ((!*fear) && (m_ptr->min_range == 1) &&
	    (r_ptr->flags1 & (RF1_FRIENDS)) &&
	    (r_ptr->flags3 & (RF3_ANIMAL)) &&
	    (!(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL))))
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

				/* Check bounds */
				if (!in_bounds(y, x)) continue;

				/* Count passable grids */
				if (cave_passable_bold(y, x)) p_ptr->vulnerability++;
			}

			/*
			 * Take character weakness into account (this
			 * always adds at least one)
			 */
			p_ptr->vulnerability += (p_ptr->mhp / MAX(1, p_ptr->chp));

			/* Is seriously weakened -- go berserk */
			if (p_ptr->chp < 2 * p_ptr->mhp / 5)
				p_ptr->vulnerability = 120;

			/* Sometimes go berserk if character is in the open */
			else if ((p_ptr->vulnerability > 4) &&
					 (one_in_(15 - p_ptr->vulnerability)))
			{
				p_ptr->vulnerability = 120;
			}
		}

		/* We or comrades are hurt */
		else if ((p_ptr->vulnerability < 100) && (comrade_hurt(m_ptr, TRUE)))
		{
			/* The ambush has been blown -- charge! */
			p_ptr->vulnerability = 120;
		}


		/* Character is insufficiently vulnerable. */
		if (p_ptr->vulnerability <= 4)
		{
			/* If we're in sight or in LOF, find a hiding place */
			if (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_FIRE | CAVE_SEEN))
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
			else if (!player_can_fire_bold(m_ptr->fy, m_ptr->fx))
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
	char m_name[DESC_LEN];

	int feat;
	cptr note_dies;

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
	monster_desc(m_name, m_ptr, 0x40);


	/* Feature is a wall (includes secret doors) */
	if (cave_wall_bold(y, x))
	{
		if (seen && confused)
			msg_format("%^s bashes into a wall.", m_name);

		/* Light stunning */
		if ((one_in_(5)) && (m_ptr->stunned < 9)) m_ptr->stunned += 3;
	}

	/* Feature is a (non-wall) door */
	else if (cave_closed_door(y, x))
	{
		if (seen && confused)
			msg_format("%^s bangs into a door.", m_name);

		/* Light stunning */
		if ((one_in_(5)) && (m_ptr->stunned < 9)) m_ptr->stunned += 3;
	}

	/* Feature is passable */
	else if (cave_passable_bold(y, x))
	{
		/* Rubble */
		if (feat == FEAT_RUBBLE)
		{
			if (seen && confused)
				msg_format("%^s staggers into some rubble.", m_name);

			/* Light stunning */
			if ((one_in_(5)) && (m_ptr->stunned < 9)) m_ptr->stunned += 3;
		}

		/* Tree */
		else if (feat == FEAT_TREE)
		{
			if (seen && confused)
				msg_format("%^s wanders into a tree.", m_name);

			/* Light stunning */
			if ((one_in_(5)) && (m_ptr->stunned < 5)) m_ptr->stunned += 3;
		}

		/* Lava */
		else if (feat == FEAT_LAVA)
		{
			/* Only 1 time in 5 */
			if (!one_in_(5)) return;

			/* Assume death */
			note_dies = " is burnt to death in lava!";

			if (mon_take_hit(cave_m_idx[m_ptr->fy][m_ptr->fx], 0,
				50 + m_ptr->maxhp / 50, &fear, note_dies))
			{
				death = TRUE;
			}
			else
			{
				if (seen && confused)
					msg_format("%^s is burnt by lava.", m_name);
			}
		}

		/* Water */
		else if (feat == FEAT_WATER)
		{
			/* Only 1 time in 5 */
			if (!one_in_(5)) return;

			/* Assume death */
			note_dies = " is drowned!";

			if (mon_take_hit(cave_m_idx[m_ptr->fy][m_ptr->fx], 0,
				5 + m_ptr->maxhp / 33, &fear, note_dies))
			{
				death = TRUE;
			}
			else
			{
				if (seen && confused)
					msg_format("%^s falls into the water.", m_name);
			}

			/* Lose some energy */
			mon_adjust_energy(m_ptr, 25);
		}
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

	int chance;

	/* Remember where monster is */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Get the change in position needed to get to the target */
	dy = *ty - oy;
	dx = *tx - ox;

	/* Calculate vertical and horizontal distances */
	ay = ABS(dy);
	ax = ABS(dx);

	/* Is the target grid adjacent to the current monster's position? */
	if ((!fear) && (ay <= 1) && (ax <= 1))
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
				/* Wander into things */
				make_confused_move(m_ptr, *ty, *tx);

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
		for (i = 0; i < 8; i++)
		{
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
						if ((m_ptr->ml) && (player_can_fire_bold(oy, ox)))
						{
							/* Cancel fear */
							set_mon_fear(m_ptr, 0, FALSE);

							/* Turn and fight */
							fear = FALSE;

							/* Forget target */
							m_ptr->ty = 0;    m_ptr->tx = 0;

							/* Charge! */
							m_ptr->min_range = 1;  m_ptr->best_range = 1;

							/* Redo move */
							i = 0;
						}
					}
				}

				/* Attacking the character as a first choice? */
				if ((i == 0) && (ny == p_ptr->py) && (nx == p_ptr->px))
				{
					continue;
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
			if ((cave_glyph(ny, nx)) && (!fear) && (one_in_(5)))
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

			/* This is the first passable grid */
			if (!passable)
			{
				/* Note that this is a passable grid */
				passable = TRUE;

				/* Accept this grid if all the direct ones are blocked. */
				if (i >= 3) break;
			}
		}

		/* We've exhausted all the easy answers. */
		if (i >= 7)
		{
			/* Try anything */
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

		/* No choice made -- cancel move */
		if (i >= 8) return (FALSE);

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
			char m_name[DESC_LEN];

			/* Get the monster name */
			monster_desc(m_name, m_ptr, 0x40);

			/* Dump a message */
			msg_format("%^s turns on you!", m_name);
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
 * Try to slip through a secret door unobserved.  -LM-
 *
 * This is an old-school, Moria-style, howling hack, the kind Ben Harrison
 * warned you about.  But it's cool.
 */
static bool slip_through_secret_door(int *y2, int *x2, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int y, x;
	int i, dir;

	/* Scan adjacent grids */
	for (y = 0, x = 0, dir = 1;; dir++)
	{
		/* Get (reverse) coordinates */
		y = *y2 - ddy[dir];
		x = *x2 - ddx[dir];

		/* Find the reverse of the direction to the monster's own grid */
		if ((y == m_ptr->fy) && (x == m_ptr->fx)) break;

		/* We've checked all directions without success. */
		if (dir >= 9) return (FALSE);
	}

	/* Check three grids on the opposite side of the door */
	for (i = 0; i < 3; i++)
	{
		/* Check this direction */
		int dir2 = side_dirs[dir][i];

		/* Get the grid in that direction */
		y = *y2 + ddy[dir2];
		x = *x2 + ddx[dir2];

		/* Skip grids which can be seen directly */
		if (player_can_see_bold(y, x)) continue;

		/* Check the grid for legality */
		if (cave_exist_mon(r_ptr, y, x, FALSE, FALSE))
		{
			/* We're good.  Accept this grid. */
			*y2 = y;
			*x2 = x;
			return (TRUE);
		}
	}

	/* No luck */
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

	/* Hack -- a visible monster in LOS loses any hidden mimic status */
	if ((m_ptr->ml) && (m_ptr->mflag & (MFLAG_MIME)) &&
	    (player_can_see_bold(oy, ox)))
	{
		char m_name[DESC_LEN];

		/* No longer hidden */
		m_ptr->mflag &= ~(MFLAG_MIME);

		/* Get monster name */
		monster_desc(m_name, m_ptr, 0x48);

		/* Notice monster (if fully visible) */
		if (mon_fully_visible(m_ptr)) msg_format("%^s appears.", m_name);

		/* Focus on this monster, unless otherwise occupied */
		if (!p_ptr->health_who)
		{
			p_ptr->health_who = cave_m_idx[m_ptr->fy][m_ptr->fx];
			p_ptr->redraw |= (PR_HEALTH);
		}
	}

	/* Break glyphs */
	if (cave_glyph(ny, nx))
	{
		/* Describe observable breakage */
		if (cave_info[ny][nx] & (CAVE_MARK))
		{
			msg_print("The rune of protection is broken!");
		}

		/* Break the rune */
		remove_trap_kind(ny, nx, TRAP_GLYPH);
	}

	/* The grid is occupied by the player. */
	if (cave_m_idx[ny][nx] < 0)
	{
		/* Attack if possible */
		if (!(r_ptr->flags1 & (RF1_NEVER_BLOW)))
		{
			(void)make_attack_normal(m_ptr, ny, nx);
		}

		/* End move */
		do_move = FALSE;
	}

	/* Can still move */
	if (do_move)
	{
		/* Handle walls and doors */
		if ((cave_wall_bold(ny, nx)) || (cave_closed_door(ny, nx)))
		{
			/* Get the feature in the grid that the monster is trying to enter. */
			feat = cave_feat[ny][nx];

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

					/* Reduce the rubble to floor */
					cave_set_feat(ny, nx, get_nearby_floor(ny, nx));
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
				/* Note that the monster killed the wall */
				if (player_can_see_bold(ny, nx))
				{
					do_view = TRUE;
					did_kill_wall = TRUE;
				}

				/* Grid is currently a door */
				if (cave_closed_door(ny, nx))
				{
					cave_set_feat(ny, nx, FEAT_BROKEN);
					if (m_ptr->cdis < 25)
						msg_print("You hear a door being smashed open.");
				}

				/* Grid is anything else */
				else
				{
					cave_set_feat(ny, nx, FEAT_RUBBLE);
					if ((!m_ptr->ml) && (m_ptr->cdis <= 6))
						msg_print("You hear grinding noises.");
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
					if (player_can_see_or_infra_bold(ny, nx))
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
					bool do_open_door = FALSE;

					/* Secret doors - currently, never locked */
					if (cave_feat[ny][nx] == FEAT_SECRET)
					{
						/* The character can (directly) see our move */
						if ((player_can_see_bold(ny, nx)) ||
						    (player_can_see_bold(m_ptr->fy, m_ptr->fx)))
						{
							/* Just open the door */
							do_open_door = TRUE;
						}

						/* Try to keep the door a secret - ala Moria */
						else
						{
							int y2 = ny;
							int x2 = nx;

							/* Can we slip through the door safely? */
							if (slip_through_secret_door(&y2, &x2, m_ptr))
							{
								/* Take (approximately) three turns */
								if (one_in_(3))
								{
									/* Accept the new coordinates (three moves) */
									ny = y2;
									nx = x2;
								}
								else
								{
									do_move = FALSE;
								}
							}
							else
							{
								/* Just open the door */
								do_open_door = TRUE;
							}
						}

					}

					/* Locked doors */
					else if (cave_feat[ny][nx] != FEAT_DOOR_HEAD + 0x00)
					{
						/* Unlock the door */
						cave_set_feat(ny, nx, FEAT_DOOR_HEAD + 0x00);

						/* Do not move */
						do_move = FALSE;
					}

					/* Ordinary doors */
					else
					{
						do_open_door = TRUE;
					}

					/* Open the door */
					if (do_open_door)
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

			/* Walls cannot be traversed unless explicitly handled */
			else return;
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

			/* XXX - Kill (much) weaker monsters */
			if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
			    (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
			    (r_ptr->mexp > nr_ptr->mexp * 2))
			{
				/* Note that the monster killed another monster (if visible) */
				did_kill_body = TRUE;

				/* Notice the kill  -TNB- */
				if ((m_ptr->mflag & MFLAG_VIEW) ||
				    (n_ptr->mflag & MFLAG_VIEW))
				{
					char m_name[DESC_LEN], n_name[DESC_LEN];

					/* Get the monster names */
					monster_desc(m_name, m_ptr, 0x04);
					monster_desc(n_name, n_ptr, 0x04);

					/* Dump a message */
					msg_format("%^s kills %s.", m_name, n_name);
				}

				/* Kill the monster */
				delete_monster(ny, nx);
			}

			/* Swap with or push aside the other monster */
			else
			{
				/* The other monster cannot switch places */
				if (!cave_exist_mon(nr_ptr, m_ptr->fy, m_ptr->fx, TRUE, TRUE))
				{
					/* Try to push it aside */
					if (!push_aside(m_ptr, n_ptr))
					{
						/* Cancel move on failure */
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

		/* Handle monster death  XXX XXX */
		if (cave_m_idx[ny][nx] <= 0) return;

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
		    (!m_ptr->ty) && (!m_ptr->tx))
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
				if ((n_ptr->ty) || (n_ptr->tx)) continue;

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
		if ((m_ptr->ml) && (!(m_ptr->mflag & (MFLAG_MIME))))
		{
			/* Player will always be heavily disturbed if monster moves adjacent */
			if (m_ptr->cdis == 1) disturb(1, 0);

			/* Hack -- ignore townspeople if strong enough  -clefs- */
			else if ((m_ptr->mflag & (MFLAG_TOWN)) && (p_ptr->power >= 10))
			{
				/* Ignore */
			}

			/* Option -- be disturbed by all other monster movement */
			else if (disturb_move) disturb(0, 0);

			/* Always be disturbed by monster movement in LOS */
			else if (player_has_los_bold(ny, nx))
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

			char m_name[DESC_LEN];
			char o_name[DESC_LEN];

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

				/* Skip essences */
				if (o_ptr->tval == TV_ESSENCE)
				{
					continue;
				}

				/* Gold cannot be destroyed, but can be picked up  -clefs- */
				if ((o_ptr->tval == TV_GOLD) &&
				    (r_ptr->flags2 & (RF2_KILL_ITEM)))
				{
					continue;
				}

				/* Extract some flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* React to objects that hurt the monster */
				if (f1 & (TR1_SLAY_DRAGON))  flg3 |= (RF3_DRAGON);
				if (f1 & (TR1_KILL_DRAGON))  flg3 |= (RF3_DRAGON);
				if (f1 & (TR1_SLAY_TROLL))   flg3 |= (RF3_TROLL);
				if (f1 & (TR1_SLAY_GIANT))   flg3 |= (RF3_GIANT);
				if (f1 & (TR1_SLAY_ORC))     flg3 |= (RF3_ORC);
				if (f1 & (TR1_SLAY_DEMON))   flg3 |= (RF3_DEMON);
				if (f1 & (TR1_SLAY_UNDEAD))  flg3 |= (RF3_UNDEAD);
				if (f1 & (TR1_SLAY_ANIMAL))  flg3 |= (RF3_ANIMAL);
				if (f1 & (TR1_SLAY_EVIL))    flg3 |= (RF3_EVIL);

				/* The object cannot be picked up by the monster */
				if (artifact_p(o_ptr) || (r_ptr->flags3 & (flg3)))
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
							object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

							/* Get the monster name */
							monster_desc(m_name, m_ptr, 0x44);

							/* Dump a message */
							msg_format("%^s goes to pick up %s, but drops it again hastily.",
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
					if (player_has_los_bold(ny, nx) &&
					    (m_ptr->mflag & (MFLAG_VIEW)))
					{
						cptr a = "";
						if ((o_ptr->tval == TV_GOLD) &&
						    (o_ptr->sval < SV_SPECIAL_GOLD_MIN))
						{
							a = "some ";
						}

						/* Get the object name */
						object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

						/* Get the monster name */
						monster_desc(m_name, m_ptr, 0x44);

						/* Dump a message */
						msg_format("%^s picks up %s%s.", m_name, a, o_name);
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

				/* Destroy the item */
				else
				{
					/* Take note */
					did_kill_item = TRUE;

					/* Describe observable situations */
					if (player_has_los_bold(ny, nx) &&
					    (m_ptr->mflag & (MFLAG_VIEW)))
					{
						/* Get the object name */
						object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

						/* Get the monster name */
						monster_desc(m_name, m_ptr, 0x44);

						/* Dump a message */
						message_format(MSG_DESTROY, 0, "%^s crushes %s.", m_name, o_name);
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
		if (did_open_door) l_ptr->flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) l_ptr->flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) l_ptr->flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) l_ptr->flags2 |= (RF2_KILL_ITEM);

		/* Monster ate another monster */
		if (did_kill_body) l_ptr->flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) l_ptr->flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall) l_ptr->flags2 |= (RF2_KILL_WALL);
	}
}



/*
 * Determine whether the player is invisible to a monster.
 *
 * We use a normal distribution for monster awareness; an invisibility
 * rating just a little above awareness rapidly increases the effects of
 * invisibility, but it is difficult to become totally invisible to most
 * monsters.
 *
 * Character invisibility can make monsters not cast spells and move
 * randomly, and reduce their likelihood of scoring hits.  Nearby
 * characters are always easier to see, but in the first case, nearness
 * is much more important.
 */
bool player_invis(monster_type *m_ptr, bool apply_dist)
{
	s16b inv, mlv;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Get noise (aggravation counts as being extremely noisy) */
	int noise = total_wakeup_chance;
	if (p_ptr->aggravate) noise = MAX(noise, 10000);

	/* Get invisibility strength */
	inv = p_ptr->invisible;

	/* The character can hide in darkness (+0 to +20) */
	if (!player_can_see_bold(p_ptr->py, p_ptr->px))
	{
		inv += darkness_ratio(1) / (15 - get_skill(S_STEALTH, 0, 10));
	}

	/* The more noise you are making, the easier it is to notice you */
	inv -= noise / 400;

	/* Character is not invisible */
	if (inv <= 0) return (FALSE);

	/* Base monster perceptiveness increases with level  (5 -> 21) */
	mlv = (s16b) 5 + r_ptr->level / 6;

	/* Adjust perception for monster race */
	if (r_ptr->flags3 & (RF3_UNDEAD))        mlv += 20;
	else if (r_ptr->flags3 & (RF3_NO_SLEEP)) mlv += 5;
	else if (r_ptr->flags3 & (RF3_DRAGON))   mlv += 5;
	else if (r_ptr->flags3 & (RF3_DEMON))    mlv += 5;
	else if (r_ptr->flags3 & (RF3_ORC))      mlv = (mlv * 3) / 4;
	else if (r_ptr->flags3 & (RF3_TROLL))    mlv = (mlv * 3) / 4;

	/* Adjust perception and invisibility for various things */
	if (r_ptr->flags2 & (RF2_STUPID))        mlv = (mlv * 3) / 4;
	if (r_ptr->flags2 & (RF2_SMART))         mlv = (mlv * 5) / 4;
	if (r_ptr->flags1 & (RF1_UNIQUE))        mlv += 10;
	if (r_ptr->flags1 & (RF1_QUESTOR))       inv = 0;
	if (r_ptr->flags2 & (RF2_INVISIBLE))     inv = 0;
	if (r_ptr->flags2 & (RF2_NOMISS))        inv = 0;

	/* Nearby characters are more noticeable */
	if (m_ptr->cdis < 4)
	{
		if (apply_dist) inv = (inv * m_ptr->cdis) / 4;
		else            inv -= (4 - m_ptr->cdis) * 3;
	}

	/* We have been hurt, and character is in line of fire */
	if ((player_can_fire_bold(m_ptr->fy, m_ptr->fx)) &&
	    (m_ptr->hp < m_ptr->maxhp))
	{
		/* We start to look very hard indeed */
		mlv *= 2;
	}

	/* We are wary */
	else if (monster_wary(m_ptr))
	{
		mlv = mlv * 3 / 2;
	}

	/* Ignore weak invisibility */
	if (inv < mlv / 2) return (FALSE);

	/* Return whether the character is invisible (highly random) */
	return (inv > Rand_normal(mlv, mlv / 2));
}

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

	/* Assume the monster is able to perceive the player. */
	bool aware = TRUE;
	bool must_use_target = FALSE;

	/* Will the monster move randomly? */
	bool random_move = FALSE;

	/* Monster is doomed */
	if (m_ptr->mflag & (MFLAG_DOOM))
	{
		/* Die sooner or later */
		if (one_in_(10))
		{
			mon_take_hit(cave_m_idx[m_ptr->fy][m_ptr->fx], 0, 9999, &fear, NULL);
			return;
		}
	}

	/* Monster loses one turn */
	if (m_ptr->mflag & (MFLAG_TURN))
	{
		m_ptr->mflag &= ~(MFLAG_TURN);
		return;
	}

	/* If monster is sleeping, it loses its turn. */
	if (m_ptr->csleep) return;

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
			/* Go inactive unless you can smell the character */
			if ((!cave_when[m_ptr->fy][m_ptr->fx]) ||
			    (!monster_can_smell(m_ptr)))
			{
				m_ptr->mflag &= ~(MFLAG_ACTV);
				m_ptr->mflag &= ~(MFLAG_WARY);
			}
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

		/* The monster is in line of sight */
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
			m_ptr->mflag |= (MFLAG_ACTV);
	}

	/* A monster in passive mode will end its turn at this point. */
	if (!(m_ptr->mflag & (MFLAG_ACTV))) return;


	/* Hack -- Always redraw the current target monster health bar */
	if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])
		p_ptr->redraw |= (PR_HEALTH);


	/* Attempt to multiply if able to and allowed */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) && (!m_ptr->confused) &&
	    (!m_ptr->monfear) && (!(m_ptr->mflag & (MFLAG_DOOM))) &&
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

		/* Shadow breeders multiply only in darkness */
		if ((m_ptr->r_idx == MON_SHADOW_BREEDER) &&
		    ((player_can_see_bold(m_ptr->fy, m_ptr->fx)) ||
		     (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_GLOW | CAVE_LITE))))
		{
			k = 999;
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(cave_m_idx[m_ptr->fy][m_ptr->fx]))
			{
				/* Take note if visible */
				if (mon_fully_visible(m_ptr))
				{
					l_ptr->flags2 |= (RF2_MULTIPLY);

					/* Make a sound (not when unseen (?)) */
					sound(MSG_MULTIPLY);
				}

				/* Multiplying takes energy */
				return;
			}
		}
	}


	/* Is the character invisible to the monster? */
	if (player_invis(m_ptr, TRUE))
	{
		/* Monster is not aware of the character */
		aware = FALSE;

		/* Monster has no target */
		if ((!m_ptr->ty) && (!m_ptr->tx)) random_move = TRUE;
	}

	/* Hack -- Smeagol also speaks.  From Zangband. */
	if ((m_ptr->r_idx == MON_SMEAGOL) &&
	    (player_has_los_bold(m_ptr->fy, m_ptr->fx)) &&
	    (one_in_(6)))
	{
		char filename[DESC_LEN];
		char speech[DESC_LEN];
		char m_name[DESC_LEN];

		/* Smeagol says different things if he's frightened */
		if (m_ptr->min_range < MAX_RANGE) strcpy(filename, "smeagol.txt");
		else                              strcpy(filename, "smeagolr.txt");

		/* Speak */
		if (get_rnd_line(filename, speech) == 0)
		{
			monster_desc(m_name, m_ptr, 0x40);
			msg_format("%^s %s", m_name, speech);
		}
	}


	/*** Ranged attacks ***/

	/* Extract the ranged attack probability. */
	chance = r_ptr->freq_ranged;

	/* Cannot use ranged attacks beyond maximum range. */
	if ((chance) && (m_ptr->cdis > MAX_RANGE)) chance = 0;

	/* Cannot use ranged attacks when confused or not aware. */
	if ((chance) && ((m_ptr->confused) || (!aware))) chance = 0;

	/* Stunned monsters use ranged attacks half as often. */
	if ((chance) && (m_ptr->stunned)) chance /= 2;

	/* Monster can use ranged attacks */
	if ((chance) && (rand_int(100) < chance))
	{
		/* Pick a ranged attack */
		choice = choose_ranged_attack(cave_m_idx[m_ptr->fy][m_ptr->fx], FALSE);
	}

	/* Roll to use ranged attacks failed, but monster is an archer. */
	if ((choice == 0) && (r_ptr->flags2 & (RF2_ARCHER)) && (!m_ptr->monfear))
	{
		/* Pick an archery attack (usually) */
		if ((!one_in_(5)) && (m_ptr->cdis > 1))
			choice = choose_ranged_attack(cave_m_idx[m_ptr->fy][m_ptr->fx], TRUE);
	}

	/* Selected a ranged attack? */
	if (choice != 0)
	{
		/* Execute said attack */
		make_attack_ranged(m_ptr, choice);

		/* End turn */
		return;
	}

	/*** Movement ***/

	/* Assume no movement */
	ty = 0;
	tx = 0;

	/* Is the monster scared? */
	if ((!(r_ptr->flags1 & (RF1_NEVER_MOVE))) &&
		 ((m_ptr->min_range >= FLEE_RANGE) ||
		  (m_ptr->monfear)))
	{
		fear = TRUE;
	}

	/*
	 * Innate semi-random movement.  Attacks are not random, nor is fleeing.
	 */
	if ((r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25)) &&
	    (!fear) && (m_ptr->cdis > 1) &&
	    (player_has_los_bold(m_ptr->fy, m_ptr->fx)))
	{
		chance = 0;

		/* RAND_25 and RAND_50 are cumulative */
		if (r_ptr->flags1 & (RF1_RAND_25))
		{
			chance += 25;
			if (mon_fully_visible(m_ptr)) l_ptr->flags1 |= (RF1_RAND_25);
		}
		if (r_ptr->flags1 & (RF1_RAND_50))
		{
			chance += 50;
			if (mon_fully_visible(m_ptr)) l_ptr->flags1 |= (RF1_RAND_50);
		}

		/* Chance of moving randomly */
		if (rand_int(100) < chance) random_move = TRUE;
	}

	/* Monster has gone mad, and chooses to attack another creature */
	if ((m_ptr->mflag & (MFLAG_MADD)) &&
	    (melee_another_monster(cave_m_idx[m_ptr->fy][m_ptr->fx], m_ptr)))
	{
		/* Done with our turn */
		return;
	}

	/* Monster cannot perceive the character */
	else if ((!aware) && (!random_move))
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
	}

	/* Normal movement */
	else
	{
		/* Choose a pair of target grids, or cancel the move. */
		if (!get_move(m_ptr, &ty, &tx, &fear, must_use_target))
			return;
	}

	/* Cannot move, target grid does not contain the character */
	if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) &&
		 (cave_m_idx[ty][tx] >= 0))
	{
		/* Cannot move */
		return;
	}

	/* Calculate the actual move.  Cancel move on failure to enter grid. */
	if (!make_move(m_ptr, &ty, &tx, fear, &bash)) return;

	/* Change terrain, move the monster, handle secondary effects. */
	process_move(m_ptr, ty, tx, bash);

	/* End turn */
	return;
}



/*
 * A table that provides pseudo-random values in applications where
 * true randomization would be impracticable.  The order of numbers is
 * designed to provide a relatively even spread of values above and
 * below any given breakpoint.
 *
 * This table is used with values of more than one digit by randomizing
 * each digit successively.
 */
static byte pseudo_randomize[10] = { 1, 8, 3, 6, 0, 9, 2, 7, 4, 5 };


/*
 * Monster regeneration of HPs and mana, and recovery from all temporary
 * conditions.
 *
 * To make it easier to balance code that inflicts nasty effects on
 * monsters, stunning, fear, and confusion wear off at the same rate.
 *
 * This function is called a lot, and is therefore fairly expensive.
 */
static void recover_monster(monster_type *m_ptr, bool regen)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int frac;
	int speed_adjust = 0;

	bool visible = FALSE;
	bool notice = FALSE;


	/* Visible monsters must be both seen and noticed */
	if (mon_fully_visible(m_ptr) && (!p_ptr->image))
	{
		visible = TRUE;
		notice = TRUE;

		/* Allow characters to ignore some things */
		if ((!disturb_move) && ((!(m_ptr->mflag & (MFLAG_VIEW))))) notice = FALSE;
	}

	/* Handle any area-effects of the monster - only if active */
	if ((r_ptr->flags2 & (RF2_CLOUD_SURROUND)) &&
	    (m_ptr->mflag & (MFLAG_ACTV)))
	{
		/* Assume no affect */
		bool affect = FALSE;

		int typ = 0, dam = 0, rad = 0;

		/* The Nazgul always drain light */
		if (r_ptr->d_char == 'W') affect = TRUE;

		/* Silver jellies/ants drain light when their grid is lit */
		else if ((strchr("aj", r_ptr->d_char)) &&
		         (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_GLOW)))
		{
			affect = TRUE;
		}

		/* Other monsters wait for the character to approach */
		else if (m_ptr->cdis <= 5) affect = TRUE;

		/* Affect surroundings if appropriate */
		if (affect)
		{
			/* Get information */
			cloud_surround(m_ptr->r_idx, &typ, &dam, &rad);

			/* Learn about monster (before visibility changes) */
			if ((m_ptr->ml) && (r_ptr->flags2 & (RF2_CLOUD_SURROUND)))
			{
				l_ptr->flags2 |= (RF2_CLOUD_SURROUND);
			}

			/* Release of cloud (can affect visibility) */
			if (typ) mon_cloud(cave_m_idx[m_ptr->fy][m_ptr->fx], typ,
			                   dam, rad);
		}
	}

	/*
	 * Monsters have a (small) chance to recover from the Black Breath;
	 * maybe they have some Athelas handy...
	 */
	if ((m_ptr->mflag & (MFLAG_BLBR)) && (one_in_(250 - r_ptr->level)))
	{
		m_ptr->mflag &= ~(MFLAG_BLBR);

		if (visible)
		{
			char m_name[DESC_LEN];

			/* Get the monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Dump a message */
			msg_format("%^s recovers from the Black Breath.", m_name);
		}
	}

	/* Maddened monsters have a small chance of coming to their senses */
	if ((m_ptr->mflag & (MFLAG_MADD)) && (one_in_(50)))
	{
		/* Monster is no longer maddened */
		m_ptr->mflag &= ~(MFLAG_MADD);

		/* Take note if visible */
		if (notice)
		{
			char m_name[DESC_LEN];
			char m_poss[DESC_LEN];

			/* Extract monster name/poss */
			monster_desc(m_name, m_ptr, 0);
			monster_desc(m_poss, m_ptr, 0x22);

			/* Message */
			msg_format("%^s comes to %s senses.", m_name, m_poss);
		}
	}


	/* Every 100 game turns, regenerate monsters */
	if (regen)
	{
		int smooth = -1, r;

		/* Regenerate mana, if needed */
		if (m_ptr->mana < r_ptr->mana)
		{
			/* Randomize digits of game turn to smooth out regeneration */
			smooth = 10 * pseudo_randomize[(turn /  100) % 10] +
			              pseudo_randomize[(turn / 1000) % 10];

			/* Monster regeneration depends on maximum mana */
			frac = ((m_ptr->maxhp + 12) + smooth) / 100;

			/* Regenerate */
			m_ptr->mana += frac;

			/* Do not over-regenerate */
			if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;

			/* Got some mana -> flag minimum range for recalculation */
			if (m_ptr->min_range >= FLEE_RANGE)
			{
				if (m_ptr->mana >= r_ptr->mana / 2) m_ptr->min_range = 0;
			}
		}

		/*
		 * Allow hp regeneration, if needed, unless suffering from
		 * the Black Breath.  Most monsters regenerate at just under half
		 * the speed that an unhindered character does.
		 */
		if ((m_ptr->hp < m_ptr->maxhp) && (!(m_ptr->mflag & (MFLAG_BLBR))))
		{
			if (smooth < 0)
			{
				smooth = 10 * pseudo_randomize[(turn /  100) % 10] +
								  pseudo_randomize[(turn / 1000) % 10];
			}

			/* Monster regeneration depends on maximum HPs */
			r = 3 * (m_ptr->maxhp + 12) / 2;

			/* Important -- HPs above 500 are half as effective */
			if (r > 768) r -= (r - 768) / 2;

			/* HPs above 2000 are one-quarter as effective */
			if (r > 1893) r -= (r - 1893) / 2;

			/* Some monsters regenerate quickly */
			if (r_ptr->flags2 & (RF2_REGENERATE)) r *= 2;

			/* Inactive monsters rest */
			if (!(m_ptr->mflag & (MFLAG_ACTV))) r *= 2;

			/* Calculate regeneration (may be zero) */
			frac = (r + smooth) / 100;

			/* Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Handle fleeing monsters */
			if (m_ptr->min_range >= FLEE_RANGE)
			{
				/* Mostly healed -> flag minimum range for recalculation */
				if (m_ptr->hp >= 2 * m_ptr->maxhp / 3) m_ptr->min_range = 0;

				/* Monster has recovered some HPs, and is in LOS */
				else if ((m_ptr->ml) &&
				         (player_has_los_bold(m_ptr->fy, m_ptr->fx)) &&
				         (m_ptr->hp >= m_ptr->maxhp / 3))
				{
					m_ptr->min_range = 0;
				}

				/* Recalculate combat range */
				if (m_ptr->min_range == 0) find_range(m_ptr);

				/* Newly brave monster */
				if ((m_ptr->min_range < FLEE_RANGE) && (notice))
				{
					char m_name[DESC_LEN];
					char m_poss[DESC_LEN];

					/* Get the monster name/poss */
					monster_desc(m_name, m_ptr, 0);
					monster_desc(m_poss, m_ptr, 0x22);

					/* Dump a message */
					msg_format("%^s recovers %s courage.", m_name, m_poss);
				}
			}
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
			if (notice)
			{
				char m_name[DESC_LEN];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s wakes up.", m_name);
			}
		}

		/* Standard noise */
		else
		{
			int d;

			/* Get noise (additional noise counts triple) */
			long noise = total_wakeup_chance + (add_wakeup_chance * 2);

			/* High-level monsters are slightly more aware (balances stealth) */
			long ignore = 133 - r_ptr->level / 3;

			/* Monsters are disturbed less if far away */
			ignore *= (3 + m_ptr->cdis);

			/* Monsters are disturbed more if in LOS */
			if (player_has_los_bold(m_ptr->fy, m_ptr->fx)) ignore /= 2;

			/* Get disturbance */
			d = rand_int(div_round(noise, ignore) + 1);

			/* Still asleep */
			if (m_ptr->csleep > d)
			{
				/* Monster's sleep is disturbed */
				m_ptr->csleep -= d;

				/* Notice the "not waking up" */
				if (visible)
				{
					/* Hack -- Count the ignores */
					if (l_ptr->ignore < MAX_UCHAR)
					{
						l_ptr->ignore++;
					}

					/* We are making a substantial amount of extra noise */
					if ((add_wakeup_chance >= 1000) && (notice))
					{
						char m_name[DESC_LEN];

						/* Get the monster name */
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
					char m_name[DESC_LEN];

					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					if (notice) msg_format("%^s wakes up.", m_name);

					/* Hack -- Count the wakings */
					if (l_ptr->wake < MAX_UCHAR)
					{
						l_ptr->wake++;
					}
				}
			}
		}
	}

	/* Recover from stuns */
	if (m_ptr->stunned)
	{
		int d = randint(3 + div_round(r_ptr->level, 20));

		/* Reduce stunning */
		if (m_ptr->stunned > d)
		{
			m_ptr->stunned -= d;
		}

		/* Fully recover */
		else
		{
			/* Recover fully */
			m_ptr->stunned = 0;

			/* Message if visible */
			if (notice)
			{
				char m_name[DESC_LEN];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer stunned.", m_name);
			}
		}
	}


	/* Recover from confusion */
	if (m_ptr->confused)
	{
		int d = randint(3 + div_round(r_ptr->level, 20));

		/* Reduce confusion */
		if (m_ptr->confused > d)
		{
			m_ptr->confused -= d;
		}

		/* Fully recover */
		else
		{
			/* No longer confused */
			m_ptr->confused = 0;

			/* Message if visible */
			if (notice)
			{
				char m_name[DESC_LEN];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer confused.", m_name);
			}
		}
	}


	/* Recover courage */
	if (m_ptr->monfear)
	{
		int d = randint(3 + div_round(r_ptr->level, 20));

		/* Reduce fear */
		if (m_ptr->monfear > d)
		{
			m_ptr->monfear -= d;
		}

		/* Fully recover */
		else
		{
			/* Cancel fear */
			set_mon_fear(m_ptr, 0, FALSE);

			/* Recalculate minimum range immediately */
			find_range(m_ptr);

			/* Stay terrified if low in HP  XXX */
			if (((100L * m_ptr->hp) / m_ptr->maxhp) < 30)
			{
				m_ptr->min_range = m_ptr->best_range = FLEE_RANGE;
			}

			/* Visual note - only if monster isn't terrified */
			if ((notice) && (m_ptr->min_range < FLEE_RANGE))
			{
				char m_name[DESC_LEN];
				char m_poss[DESC_LEN];

				/* Get the monster name/poss */
				monster_desc(m_name, m_ptr, 0);
				monster_desc(m_poss, m_ptr, 0x22);

				/* Dump a message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);
			}
		}
	}

	/* Handle hasted monsters */
	if (m_ptr->hasted)
	{
		/* Time out hasting */
		m_ptr->hasted--;

		/* No longer hasted */
		if (!m_ptr->hasted)
		{
			/* Mention changes if visible */
			if (notice)
			{
				char m_name[DESC_LEN];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Message. */
				msg_format("%^s is no longer hasted.", m_name);
			}
		}
		else
		{
			/* Note speed */
			speed_adjust += 10;
		}
	}

	/* Handle slowed monsters */
	if (m_ptr->slowed)
	{
		/* Time out slow */
		m_ptr->slowed--;

		/* No longer slowed */
		if (!m_ptr->slowed)
		{
			/* Mention changes if visible */
			if (notice)
			{
				char m_name[DESC_LEN];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Message. */
				msg_format("%^s is no longer slowed.", m_name);
			}
		}
		else
		{
			/* Note slowness */
			speed_adjust -= 10;
		}
	}

	/* Handle wary monsters */
	if ((monster_wary(m_ptr)) && (regen) &&
	    (!(m_ptr->mflag & (MFLAG_ACTV))) &&
	    (num_recent_thefts <= get_skill(S_STEALTH, 2, 4)))
	{
		/* No longer wary */
		m_ptr->mflag &= ~(MFLAG_WARY);
	}

	/* Special case -- Angels avoid users of holy lore */
	if ((p_ptr->realm == PRIEST) &&
	    (r_info[m_ptr->r_idx].d_char == 'A') &&
	    (!p_ptr->unsanctified))
	{
		m_ptr->best_range = m_ptr->min_range = FLEE_RANGE;
	}

	/*
	 * Monsters can blink around uncontrollably.
	 */
	if ((p_ptr->phasing_foes) && (m_ptr->cdis < MAX_SIGHT + 5))
	{
		char m_name[DESC_LEN];

		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Require that monster not be totally immune */
		if (!(r_ptr->flags3 & (RF3_RES_NEXUS)) &&
		    !(r_ptr->flags3 & (RF3_RES_TPORT)) &&
		    !(r_ptr->flags4 & (RF4_BRTH_NEXUS)) &&
		    !(prefix(m_name, "Nexus")))
		{
			int power, resist, tmp;

			/* Get power of spell */
			power = 3 * p_ptr->power / 2;

			/* Get resistance of monster */
			if (r_ptr->flags1 & (RF1_UNIQUE)) resist = r_ptr->level + 20;
			else resist = r_ptr->level + 10;

			if (r_ptr->d_char == 'D') resist += r_ptr->level / 3;
			if (r_ptr->d_char == 'P') resist += r_ptr->level / 4;
			if (r_ptr->d_char == 'd') resist += r_ptr->level / 4;
			if (r_ptr->d_char == 'X') resist += 10;
			if (r_ptr->d_char == 'g') resist += 5;

			if (r_ptr->flags2 & (RF2_FLYING)) resist += 10;
			if (r_ptr->flags2 & (RF2_PASS_WALL)) resist += r_ptr->level / 3;
			if (r_ptr->flags2 & (RF2_PASS_WALL)) resist += 20;

			if (r_ptr->flags6 & (RF6_BLINK)) resist += 10;
			if (r_ptr->flags6 & (RF6_TPORT)) resist += 10;
			if (r_ptr->flags6 & (RF6_TELE_LEVEL)) resist += 20;

			/* Get remaining spell strength */
			tmp = power - resist;

			/* Try to blink the monster */
			if ((tmp > 0) && (rand_int(resist) < tmp))
			{
				teleport_away(cave_m_idx[m_ptr->fy][m_ptr->fx],
					MIN(15, (2 + tmp / 5)), FALSE);
			}
		}
	}


	/* Hack -- Update the health bar (always) */
	if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])
		p_ptr->redraw |= (PR_HEALTH);
}



/*
 * Process a single monster (it is getting a free move, or some such).
 *
 * This code is far from being perfectly "correct".
 */
void monster_free_moves(monster_type *m_ptr, int perc_move)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int game_turns, speed;

	bool move = FALSE;


	/* Determine how many tens of game turns we have available */
	game_turns = div_round(perc_move, extract_energy[p_ptr->pspeed]);

	/* Use up each of those blocks of time */
	while (game_turns > 0)
	{
		/* Use up some time */
		game_turns--;

		/* Get monster base speed */
		speed = m_ptr->mspeed;

		/* Monster is slowed or hasted - adjust effective speed */
		if (m_ptr->slowed) speed -= 10;
		if (m_ptr->hasted) speed += 10;

		/* The speed of some monsters matches that of the character */
		if (r_ptr->flags2 & (RF2_SAMESPD))
			speed = p_ptr->pspeed + speed - 110;

		/* Give this monster some energy */
		m_ptr->energy += extract_energy[speed];

		/* Can the monster move? */
		while (m_ptr->energy >= 100)
		{
			/* Use up some energy */
			m_ptr->energy -= 100;

			/* Dump a message (only once) */
			if (!move)
			{
				char m_name[DESC_LEN];

				/* Get the monster name "the kobold" */
				monster_desc(m_name, m_ptr, 0x44);

				/* Message */
				msg_format("%^s reacts!", m_name);
				move = TRUE;
			}

			/* Monster takes a turn */
			process_monster(m_ptr);
		}
	}
}


/*
 * Sorting hook -- comp function -- array of movement moments.
 */
static bool ang_sort_comp_hook_moment(const void *u, const void *v, int a, int b)
{
	move_moment_type *mm = (move_moment_type*)(u);

	/* Unused parameter */
	(void)v;

	/* Sort by increasing movement moment */
	return (mm[a].moment <= mm[b].moment);
}

/*
 * Sorting hook -- swap function -- array of movement moments.
 */
static void ang_sort_swap_hook_moment(void *u, void *v, int a, int b)
{
	move_moment_type *mm = (move_moment_type*)(u);
	move_moment_type temp_moment;

	/* Unused parameter */
	(void)v;

	/* Swap records */
	COPY(&temp_moment, &mm[a], move_moment_type);
	COPY(&mm[a], &mm[b], move_moment_type);
	COPY(&mm[b], &temp_moment, move_moment_type);
}


/*
 * Process monsters, the character, and other entities.  -LM-
 *
 * Give the character energy.  If the character has >= 100 energy,
 * store character index for later movement.
 *
 * Every ten game turns, allow monsters to recover from temporary con-
 * ditions.  Every 100 game turns, regenerate monsters.  Give energy to
 * each monster, store monster index of all monsters with >= 100 energy
 * for later movement.
 *
 * All entities that move this turn are sorted by "movement moment",
 * the exact instant within the course of a game turn in which the
 * entity has exactly 100 energy, and may move.  Lower movement moments
 * take priority.
 */
void process_entities(void)
{
	int i, speed;
	int energy_per_turn, old_energy, moment;
	int idx;
	u16b dummy = 0;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Time out temporary monster conditions every ten game turns */
	bool recover = (turn % 10 == 0);

	/* Regenerate monster hitpoints and mana every 100 game turns */
	bool regen = (turn % 100 == 0);


	/* Clear the movement moment table */
	move_moment_num = 0;


	/* Give the character some energy (unless leaving) */
	if (!p_ptr->leaving)
	{
		/* Give character energy */
		p_ptr->energy += extract_energy[p_ptr->pspeed];

		/* Can the character move? */
		if (p_ptr->energy >= 100)
		{
			/* Determine how much energy the character gets per turn */
			energy_per_turn = extract_energy[p_ptr->pspeed];

			/* Note how much energy the character had last turn */
			old_energy = p_ptr->energy - energy_per_turn;

			/* Calculate movement moment - Hugo Kornelis - */
			moment = 100 * (100 - old_energy) / (energy_per_turn);

			/* Insert character into movement table */
			move_moment[move_moment_num].m_idx = -1;
			move_moment[move_moment_num++].moment = moment;
		}
	}


	/* Process the monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];

		/* Ignore dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Get the monster race */
		r_ptr = &r_info[m_ptr->r_idx];


		/* Handle temporary monster attributes and regeneration */
		if (recover) recover_monster(m_ptr, regen);


		/* Get monster base speed */
		speed = m_ptr->mspeed;

		/* Monster is slowed or hasted -- adjust effective speed */
		if (m_ptr->slowed) speed -= 10;
		if (m_ptr->hasted) speed += 10;

		/* The speed of some monsters matches that of the character */
		if (r_ptr->flags2 & (RF2_SAMESPD))
			speed = p_ptr->pspeed + speed - 110;

		/* Determine how much energy the monster gets per turn */
		energy_per_turn = extract_energy[speed];

		/* Give this monster some energy */
		m_ptr->energy += energy_per_turn;


		/* Ignore monsters with less than 100 energy */
		if (m_ptr->energy < 100) continue;


		/* Insert monster into the movement moment table */
		move_moment[move_moment_num].m_idx = i;

		/* Note how much energy the monster had last turn */
		old_energy = m_ptr->energy - energy_per_turn;

		/* Calculate movement moment - Hugo Kornelis - */
		moment = 100 * (100 - old_energy) / (energy_per_turn);

		/* Save it, go to next slot */
		move_moment[move_moment_num++].moment = moment;
	}


	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook_moment;
	ang_sort_swap = ang_sort_swap_hook_moment;

	/* Sort the movement table by decreasing movement moment */
	ang_sort(move_moment, &dummy, move_moment_num);


	/* Process monsters and the character, in order of priority */
	for (i = 0; i < move_moment_num; i++)
	{
		/* Get next entity index */
		idx = move_moment[i].m_idx;

		/* This is a monster */
		if (idx > 0)
		{
			/* Character is dead or leaving the current level */
			if (p_ptr->leaving) continue;

			/* Get the monster race */
			m_ptr = &m_list[idx];

			/* Require that monster still have at least 100 energy */
			if (m_ptr->energy >= 100)
			{
				/* Use up some energy */
				m_ptr->energy -= 100;

				/* Monster takes a turn */
				process_monster(m_ptr);
			}
		}

		/* This is the character */
		else if (idx < 0)
		{
			/* Can the character move? */
			while (p_ptr->energy >= 100 && !p_ptr->leaving)
			{
				/* Let the character take a turn */
				process_player();
			}
		}
	}
}

