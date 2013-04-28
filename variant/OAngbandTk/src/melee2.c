/* File: melee2.c */

/* Monster learning, monster distance attacks and spells, fear, flow/
 * movement, monster AI effecting movement and spells, process a monster 
 * (with spells and actions of all kinds, reproduction, effects of any 
 * terrain on monster movement, picking up and destroying objects), 
 * process all monsters.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke,
 * Bahman Rabii
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


#include "tnb.h" /* TNB */


#define SPEAK_CHANCE 16

#ifdef DRS_SMART_OPTIONS


/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Original idea and code by "DRS" (David Reeves Sward).
 *
 * Major modifications by "BEN" (Ben Harrison).
 *
 * Closely related to monster AI (orginal code by Keldon Jones).
 *
 * All of this code heavily modified by Bahman Rabii (BR).
 *
 * Give monsters more intelligent attack/spell selection based on
 * observations of previous attacks on the player, and/or by allowing
 * the monster to "cheat" and know the player status.
 *
 * Maintain an idea of the player status, and use that information
 * to occasionally eliminate "ineffective" spell attacks.  We could
 * also eliminate ineffective normal attacks, but there is no reason
 * for the monster to do this, since he gains no benefit.
 * Note that MINDLESS monsters are not allowed to use this code.
 * And non-INTELLIGENT monsters only use it partially effectively.
 *
 * Actually learn what the player resists, and use that information
 * to remove attacks or spells before using them.  This will require
 * much less space, if I am not mistaken.  Thus, each monster gets a
 * set of 32 bit flags, "smart", build from the various "SM_*" flags.
 *
 * This has the added advantage that attacks and spells are related.
 * The "smart_learn" option means that the monster "learns" the flags
 * that should be set, and "smart_cheat" means that he "knows" them.
 * So "smart_cheat" means that the "smart" field is always up to date,
 * while "smart_learn" means that the "smart" field is slowly learned.
 * Both of them have the same effect on the "choose spell" routine.
 * As of Oangband 0.5.0 "smart_learn" is always on, and the smart
 * learn flags are saved properly. -BR-
 */

/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */
static int summon_possible(int y1, int x1)
{
	int y, x;
	int num_clear = 0;

	/* Start at the player's location, and check 2 grids in each dir */
	for (y = y1 - 2; y <= y1 + 2; y++)
	{
		for (x = x1 - 2; x <= x1 + 2; x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(y, x))
				continue;

			/* Only check a circular area */
			if (distance(y1, x1, y, x) > 2)
				continue;

			/* Hack: no summon on glyph of warding */
			if (cave_feat[y][x] == FEAT_GLYPH)
				continue;

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
 * Fully update monsters knowledge of the player.
 * Used by player ghost (and all monsters with smart_cheat).
 */
static void update_smart_cheat(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	/* Know weirdness */
	if (p_ptr->free_act)
		m_ptr->smart |= (SM_IMM_FREE);
	if (!p_ptr->msp)
		m_ptr->smart |= (SM_IMM_MANA);
	if (p_ptr->skill_sav >= 75)
		m_ptr->smart |= (SM_GOOD_SAVE);
	if (p_ptr->skill_sav >= 100)
		m_ptr->smart |= (SM_PERF_SAVE);

	/* Know immunities */
	if (p_ptr->immune_acid)
		m_ptr->smart |= (SM_IMM_ACID);
	if (p_ptr->immune_elec)
		m_ptr->smart |= (SM_IMM_ELEC);
	if (p_ptr->immune_fire)
		m_ptr->smart |= (SM_IMM_FIRE);
	if (p_ptr->immune_cold)
		m_ptr->smart |= (SM_IMM_COLD);

	/* Know oppositions */
	if (p_ptr->oppose_acid)
		m_ptr->smart |= (SM_OPP_ACID);
	if (p_ptr->oppose_elec)
		m_ptr->smart |= (SM_OPP_ELEC);
	if (p_ptr->oppose_fire)
		m_ptr->smart |= (SM_OPP_FIRE);
	if (p_ptr->oppose_cold)
		m_ptr->smart |= (SM_OPP_COLD);
	if (p_ptr->oppose_pois)
		m_ptr->smart |= (SM_OPP_POIS);

	/* Know resistances */
	if (p_ptr->resist_acid)
		m_ptr->smart |= (SM_RES_ACID);
	if (p_ptr->resist_elec)
		m_ptr->smart |= (SM_RES_ELEC);
	if (p_ptr->resist_fire)
		m_ptr->smart |= (SM_RES_FIRE);
	if (p_ptr->resist_cold)
		m_ptr->smart |= (SM_RES_COLD);
	if (p_ptr->resist_pois)
		m_ptr->smart |= (SM_RES_POIS);
	if (p_ptr->resist_fear)
		m_ptr->smart |= (SM_RES_FEAR);
	if (p_ptr->resist_lite)
		m_ptr->smart |= (SM_RES_LITE);
	if (p_ptr->resist_dark)
		m_ptr->smart |= (SM_RES_DARK);
	if (p_ptr->resist_blind)
		m_ptr->smart |= (SM_RES_BLIND);
	if (p_ptr->resist_confu)
		m_ptr->smart |= (SM_RES_CONFU);
	if (p_ptr->resist_sound)
		m_ptr->smart |= (SM_RES_SOUND);
	if (p_ptr->resist_shard)
		m_ptr->smart |= (SM_RES_SHARD);
	if (p_ptr->resist_nexus)
		m_ptr->smart |= (SM_RES_NEXUS);
	if (p_ptr->resist_nethr)
		m_ptr->smart |= (SM_RES_NETHR);
	if (p_ptr->resist_chaos)
		m_ptr->smart |= (SM_RES_CHAOS);
	if (p_ptr->resist_disen)
		m_ptr->smart |= (SM_RES_DISEN);

	return;
}

/*
 * Used to determine the player's known level of resistance to a
 * particular spell.
 *
 * The LRN_xxx constant determines what type of resistance is
 * applicable.  The monster's SM_xxx flags, as well as player
 * condistions, are refered to as needed.
 * BR
 */
static int find_resist(int m_idx, int spell_lrn)
{
	monster_type *m_ptr = &m_list[m_idx];

	int a;
	u32b smart;

	/* Nothing Known */
	if (!m_ptr->smart)
		return (0);

	/* get smart flags */
	smart = m_ptr->smart;

	/* Which spell */
	switch (spell_lrn)
	{
			/* Spells 'resisted' by AC, Dex, etc. 
			 * Currently no assesment is made */
		case LRN_ARCH:
		{
			return (0);
		}
			/* As above, but poisonous. */
		case LRN_PARCH:
		{
			if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS)))
				return (10);
			else
				return (0);
		}
			/* Acid Spells */
		case LRN_ACID:
		{
			if (smart & (SM_IMM_ACID))
				return (100);
			else if ((smart & (SM_OPP_ACID)) && (smart & (SM_RES_ACID)))
				return (70);
			else if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID)))
				return (40);
			else
				return (0);
		}
			/* Lightning Spells */
		case LRN_ELEC:
		{
			if (smart & (SM_IMM_ELEC))
				return (100);
			else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC)))
				return (70);
			else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC)))
				return (40);
			else
				return (0);
		}
			/* Fire Spells */
		case LRN_FIRE:
		{
			if (smart & (SM_IMM_FIRE))
				return (100);
			else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE)))
				return (70);
			else if ((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE)))
				return (40);
			else
				return (0);
		}
			/* Cold Spells */
		case LRN_COLD:
		{
			if (smart & (SM_IMM_COLD))
				return (100);
			else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD)))
				return (70);
			else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)))
				return (40);
			else
				return (0);
		}
			/* Ice Spells */
		case LRN_ICE:
		{
			if (smart & (SM_IMM_COLD))
				a = 90;
			else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD)))
				a = 60;
			else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)))
				a = 30;
			else
				a = 0;
			if (smart & (SM_RES_SOUND))
				a += 5;
			if (smart & (SM_RES_SHARD))
				a += 5;
			return (a);
		}
			/* Poison Spells */
		case LRN_POIS:
		{
			if ((smart & (SM_OPP_POIS)) && (smart & (SM_RES_POIS)))
				return (80);
			else if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS)))
				return (55);
			else
				return (0);
		}
			/* Plasma Spells */
		case LRN_PLAS:
		{
			a = 0;
			if (smart & (SM_IMM_FIRE))
				a += 50;
			else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE)))
				a += 35;
			else if ((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE)))
				a += 20;
			if (smart & (SM_IMM_ELEC))
				a += 50;
			else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC)))
				a += 35;
			else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC)))
				a += 20;
			return (a);
		}
			/* Light Spells */
		case LRN_LITE:
		{
			if (smart & (SM_RES_LITE))
				return (30);
			else
				return (0);
		}
			/* Darkness Spells */
		case LRN_DARK:
		{
			if (smart & (SM_RES_DARK))
				return (30);
			else
				return (0);
		}
			/* Confusion Spells, damage dealing */
		case LRN_CONFU:
		{
			if (smart & (SM_RES_CONFU))
				return (30);
			else
				return (0);
		}
			/* Sound Spells */
		case LRN_SOUND:
		{
			a = 0;
			if (smart & (SM_RES_SOUND))
				a += 30;
			if (smart & (SM_RES_CONFU))
				a += 10;
			if (smart & (SM_PERF_SAVE))
				a += 10;
			else if (smart & (SM_GOOD_SAVE))
				a += 5;
			else
				return (a);
		}
			/* Unresistable, but sound prevents stun */
		case LRN_SOUND2:
		{
			if (smart & (SM_RES_SOUND))
				return (5);
			else
				return (0);
		}
			/* Shards Spells */
		case LRN_SHARD:
		{
			if (smart & (SM_RES_SHARD))
				return (30);
			else
				return (0);
		}
			/* Nexus Spells */
		case LRN_NEXUS:
		{
			if (smart & (SM_RES_NEXUS))
				return (30);
			else
				return (0);
		}
			/* Nether Spells */
		case LRN_NETHR:
		{
			if (smart & (SM_RES_NETHR))
				return (30);
			else
				return (0);
		}
			/* Chaos Spells */
		case LRN_CHAOS:
		{
			if (smart & (SM_RES_CHAOS))
				return (30);
			else
				return (0);
		}
			/* Disenchantment Spells */
		case LRN_DISEN:
		{
			if (smart & (SM_RES_DISEN))
				return (30);
			else
				return (0);
		}
			/* Storm Spells */
		case LRN_STORM:
		{
			a = 0;
			if (smart & (SM_IMM_ELEC))
				a += 15;
			else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC)))
				a += 10;
			else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC)))
				a += 5;
			if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)) ||
				(smart & (SM_IMM_COLD)))
				a += 5;
			if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID)) ||
				(smart & (SM_IMM_ACID)))
				a += 5;
			if (smart & (SM_RES_CONFU))
				a += 10;
			return (a);
		}
			/* Water Spells */
		case LRN_WATER:
		{
			a = 0;
			if (smart & (SM_RES_CONFU))
				a += 10;
			if (smart & (SM_RES_SOUND))
				a += 5;
			return (a);
		}
			/* Spells that attack player mana */
		case LRN_MANA:
		{
			if (smart & (SM_IMM_MANA))
				return (100);
			else
				return (0);
		}
			/* Spells Requiring Save or Resist Nexus */
		case LRN_NEXUS_SAVE:
		{
			if (smart & (SM_RES_NEXUS))
				return (100);
			else if (smart & (SM_PERF_SAVE))
				return (100);
			else if (smart & (SM_GOOD_SAVE))
				return (30);
			else
				return (0);
		}
			/* Spells Requiring Save or Resist Fear */
		case LRN_FEAR_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_FEAR))
				a = 100;
			else if (smart & (SM_PERF_SAVE))
				a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE))
					a += 30;
				if (p_ptr->afraid)
					a += 50;
			}
			return (a);
		}
			/* Spells Requiring Save or Resist Blindness */
		case LRN_BLIND_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_BLIND))
				a = 100;
			else if (smart & (SM_PERF_SAVE))
				a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE))
					a += 30;
				if (p_ptr->blind)
					a += 50;
			}
			return (a);
		}
			/* Spells Requiring Save or Resist Confusion */
		case LRN_CONFU_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_CONFU))
				a = 100;
			else if (smart & (SM_PERF_SAVE))
				a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE))
					a += 30;
				if (p_ptr->confused)
					a += 50;
			}
			return (a);
		}
			/* Spells Requiring Save or Free Action */
		case LRN_FREE_SAVE:
		{
			a = 0;
			if (smart & (SM_IMM_FREE))
				a = 100;
			else if (smart & (SM_PERF_SAVE))
				a = 100;
			else if (p_ptr->paralyzed)
				a = 80;
			else
			{
				if (smart & (SM_GOOD_SAVE))
					a += 30;
				if (p_ptr->slow)
					a += 50;
			}
			return (a);
		}

			/* Spells Requiring Save  */
		case LRN_SAVE:
		{
			if (smart & (SM_PERF_SAVE))
				return (100);
			else if (smart & (SM_GOOD_SAVE))
				return (30);
			else
				return (0);
		}
			/* Anything else */
		default:
		{
			return (0);
		}
	}
}

#endif

/*
 * Used to exclude spells which are too expensive for the
 * monster to cast.  Excludes all spells that cost more than the
 * current availble mana.
 *
 * Smart monsters may also exclude spells that use a lot of mana,
 * even if they have enough.
 *
 * -BR-
 */
static void remove_expensive_spells(int m_idx, u32b * f4p, u32b * f5p,
	u32b * f6p, u32b * f7p)
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
		max_cost = (m_ptr->mana * (4 + rand_int(7))) / 10;

	/* Otherwise spend up to the full current mana */
	else
		max_cost = m_ptr->mana;

	/* check innate spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (mana_cost_RF4[i] > max_cost)
			f4 &= ~(0x00000001 << i);
	}

	/* check normal spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (mana_cost_RF5[i] > max_cost)
			f5 &= ~(0x00000001 << i);
	}

	/* check other spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (mana_cost_RF6[i] > max_cost)
			f6 &= ~(0x00000001 << i);
	}

	/* check other spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (mana_cost_RF7[i] > max_cost)
			f7 &= ~(0x00000001 << i);
	}

	/* Modify the spell list. */
	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
	(*f7p) = f7;

}

/*
 * Intellegent monsters use this function to filter away spells
 * which have no benefit.
 */
static void remove_useless_spells(int m_idx, u32b * f4p, u32b * f5p,
	u32b * f6p, u32b * f7p, bool los)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);

	/* Don't regain mana if full */
	if (m_ptr->mana >= r_ptr->mana)
		f6 &= ~(RF6_ADD_MANA);

	/* Don't heal if not enough mana to make it useful */
	if (m_ptr->mana < r_ptr->spell_power / 4)
		f6 &= ~(RF6_HEAL);

	/* Don't heal if full */
	if (m_ptr->hp >= m_ptr->maxhp)
		f6 &= ~(RF6_HEAL);

	/* Don't lash if too far or close */
	if ((m_ptr->cdis > 3) || (m_ptr->cdis < 2))
		f4 &= ~(RF4_LASH);

	/* Don't Haste if Hasted */
	if (m_ptr->mspeed > r_ptr->speed + 5)
		f6 &= ~(RF6_HASTE);

	/* Don't cure if not needed */
	if (!((m_ptr->stunned) || (m_ptr->monfear) ||
			(m_ptr->mspeed < r_ptr->speed - 5) || (m_ptr->black_breath)))
		f6 &= ~(RF6_CURE);

	/* Don't jump in already close, or don't want to be close */
	if (!(m_ptr->cdis > m_ptr->best_range) && los)
		f6 &= ~(RF6_TELE_SELF_TO);
	if (m_ptr->min_range > 5)
		f6 &= ~(RF6_TELE_SELF_TO);

	/* Modify the spell list. */
	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
	(*f7p) = f7;

}

/*
 * Count the number of castable spells.
 *
 * If exactly 1 spell is availble cast it.  If more than more is
 * available, and the random bit is set, pick one.
 *
 * Used as a short cut in 'choose_attack_spell' to circumvent AI 
 * when there is only 1 choice. (random=FALSE)
 *
 * Also used in 'choose_attack_spell' to circumvent AI when 
 * casting randomly (random=TRUE), as with dumb monsters.
 */
static int choose_attack_spell_fast(int m_idx, u32b * f4p, u32b * f5p,
	u32b * f6p, u32b * f7p, bool random)
{
	int i, num = 0;
	byte spells[128];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);

	/* Extract the "innate" spells */
	for (i = 0; i < 32; i++)
	{
		if (f4 & (1L << i))
			spells[num++] = i + 32 * 3;
	}

	/* Extract the "attack" spells */
	for (i = 0; i < 32; i++)
	{
		if (f5 & (1L << i))
			spells[num++] = i + 32 * 4;
	}

	/* Extract the "miscellaneous" spells */
	for (i = 0; i < 32; i++)
	{
		if (f6 & (1L << i))
			spells[num++] = i + 32 * 5;
	}

	/* Extract the "summon" spells */
	for (i = 0; i < 32; i++)
	{
		if (f7 & (1L << i))
			spells[num++] = i + 32 * 6;
	}

	/* Paranoia */
	if (num == 0)
		return 0;

	/* Go quick if possible */
	if (num == 1)
	{
		/* Hack - Don't cast if known to be immune, unless
		 * casting randomly anyway.  */
		if (!(random))
		{
			if (spells[0] < 128)
			{
				if (find_resist(m_idx,
						spell_desire_RF4[spells[0] - 96][D_RES]) == 100)
					return (0);
			}
			else if (spells[0] < 160)
			{
				if (find_resist(m_idx,
						spell_desire_RF5[spells[0] - 128][D_RES]) == 100)
					return (0);
			}
			else if (spells[0] < 192)
			{
				if (find_resist(m_idx,
						spell_desire_RF6[spells[0] - 160][D_RES]) == 100)
					return (0);
			}
			else
			{
				if (find_resist(m_idx,
						spell_desire_RF7[spells[0] - 192][D_RES]) == 100)
					return (0);
			}
		}

		/* Otherwise cast the one spell */
		else
			return (spells[0]);
	}

	/* 
	 * If we aren't allowed to choose at random
	 * and we have multiple spells left, give up on quick
	 * selection
	 */
	if (!(random))
		return (0);

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
 * minimum can be used to set a desirability threshhold to actually 
 * pick a spell.
 *
 * Returns the spell number, of '0' if no spell is selected.
 *
 *-BR-
 */
static int choose_attack_spell(int m_idx, bool archery_only, int minimum)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	byte *spell_desire;

	u32b f4, f5, f6, f7;

	byte spell_range;

	bool rand = FALSE;

	bool los = TRUE;

	bool is_harass = FALSE;

	int i, py = p_ptr->py, px = p_ptr->px;
	int spower, hp, rlev, path;

	int want_hps = 0, want_escape = 0, want_mana = 0, want_summon = 0;
	int want_tactic = 0, cur_range = 0;

	int best_spell = 0, best_spell_rating = minimum;
	int cur_spell_rating;

	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;
	f7 = r_ptr->flags7;

	/* Check what kinds of spells can hit player */
	path =
		projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px,
		PROJECT_CHCK);

	/* do we have the player in sight at all? */
	if (path == PROJECT_NO)
	{
		/* Flat out 75% chance of not casting if the player is not in sight */
		/* In addition, most spells don't work without a player around */
		if (rand_int(4) != 0)
			return (0);

		los = FALSE;
	}

	/* Are we restricted to archery? */
	/* Note - we have assumed for speed that no archery attacks
	 * cost mana, all are 'bolt' like, and that the player can not
	 * be highly resistant to them. */
	if (archery_only)
	{
		/* check for a clean shot */
		if (!(path == PROJECT_CLEAR))
			return (0);

		/* restrict to archery */
		f4 &= (RF4_ARCHERY_MASK);
		f5 &= (RF5_ARCHERY_MASK);
		f6 &= (RF6_ARCHERY_MASK);
		f7 &= (RF7_ARCHERY_MASK);

		/* choose at random from restricted list */
		return (choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, TRUE));
	}

	/* Remove spells the 'no-brainers' */
	/* Spells that require LOS */
	if (!los)
	{
		f4 &= (RF4_NO_PLAYER_MASK);
		f5 &= (RF5_NO_PLAYER_MASK);
		f6 &= (RF6_NO_PLAYER_MASK);
		f7 &= (RF7_NO_PLAYER_MASK);
	}
	else if (path == PROJECT_NOT_CLEAR)
	{
		f4 &= ~(RF4_BOLT_MASK);
		f5 &= ~(RF5_BOLT_MASK);
		f6 &= ~(RF6_BOLT_MASK);
		f7 &= ~(RF7_BOLT_MASK);
	}

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7)
		return (0);

	/* Spells we can not afford */
	remove_expensive_spells(m_idx, &f4, &f5, &f6, &f7);

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7)
		return (0);

	/* Stupid monsters choose at random. */
	if (r_ptr->flags2 & (RF2_STUPID))
		return (choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, TRUE));

	/* Remove spells that have no benefit
	 * Does not include the effects of player resists/immunities */
	remove_useless_spells(m_idx, &f4, &f5, &f6, &f7, los);

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7)
		return (0);

	/* Sometimes non-dumb monsters cast randomly (though from the
	 * restricted list) 
	 */
	if ((r_ptr->flags2 & (RF2_SMART)) && (rand_int(10) == 0))
		rand = TRUE;
	if ((!(r_ptr->flags2 & (RF2_SMART))) && (rand_int(5) == 0))
		rand = TRUE;

	/* Try 'fast' selection first.
	 * If there is only one spell, choose that spell.
	 * If there are multiple spells, choose one randomly if the 'rand' flag is set.
	 * Otherwise fail, and let the AI choose.
	 */
	best_spell = choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, rand);
	if (best_spell)
		return (best_spell);

	/* If we get this far, we are using the full-up AI.  Calculate
	 * some parameters. */

	/* Figure out if we are hurt */
	if (m_ptr->hp < m_ptr->maxhp / 8)
		want_hps += 3;
	else if (m_ptr->hp < m_ptr->maxhp / 4)
		want_hps += 2;
	else if (m_ptr->hp < m_ptr->maxhp / 2)
		want_hps++;

	/* Figure out if we want mana */
	if (m_ptr->mana < r_ptr->mana / 4)
		want_mana += 2;
	else if (m_ptr->mana < r_ptr->mana / 2)
		want_mana++;

	/* Figure out if we want to scram */
	want_escape = 0;
	if (want_hps)
		want_escape = want_hps - 1;
	if (r_ptr->flags2 & (RF2_LOW_MANA_RUN))
		want_escape += want_mana;
	if (m_ptr->min_range == MAX_SIGHT + 5)
		want_escape++;

	/* Desire to keep minimum distance */
	want_tactic = 1;
	if (!(m_ptr->min_range < m_ptr->cdis))
		want_tactic += ((m_ptr->min_range - m_ptr->cdis) / 2) + 1;
	if (want_tactic > 3)
		want_tactic = 3;

	/* Check terrain for purposes of summoning spells */
	want_summon = (summon_possible(py, px) + 2) / 3;
	if (want_summon > 2)
		want_summon = 2;

	/* Find monster properties */
	spower = ((r_ptr->spell_power >= 2) ? r_ptr->spell_power : 2);
	hp = m_ptr->hp;
	rlev = r_ptr->level;

	/* Cheat if requested, or if a player ghost. */
	if ((smart_cheat) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
		update_smart_cheat(m_idx);

	/* The conditionals are written for speed rather than readability
	 * They should probably stay that way. */
	for (i = 0; i < 128; i++)
	{
		/* Do we even have this spell? */
		if (i < 32)
		{
			if (!(f4 & (1L << i)))
				continue;
			spell_desire = spell_desire_RF4[i];
			spell_range = spell_range_RF4[i];
			if (RF4_HARASS_MASK & (1L << (i)))
				is_harass = TRUE;
			else
				is_harass = FALSE;
		}
		else if (i < 64)
		{
			if (!(f5 & (1L << (i - 32))))
				continue;
			spell_desire = spell_desire_RF5[i - 32];
			spell_range = spell_range_RF5[i - 32];
			if (RF5_HARASS_MASK & (1L << (i - 32)))
				is_harass = TRUE;
			else
				is_harass = FALSE;
		}
		else if (i < 96)
		{
			if (!(f6 & (1L << (i - 64))))
				continue;
			spell_desire = spell_desire_RF6[i - 64];
			spell_range = spell_range_RF6[i - 64];
			if (RF6_HARASS_MASK & (1L << (i - 64)))
				is_harass = TRUE;
			else
				is_harass = FALSE;
		}
		else
		{
			if (!(f7 & (1L << (i - 64))))
				continue;
			spell_desire = spell_desire_RF7[i - 96];
			spell_range = spell_range_RF7[i - 96];
			if (RF7_HARASS_MASK & (1L << (i - 96)))
				is_harass = TRUE;
			else
				is_harass = FALSE;
		}

		/* Desirability for spell power */
		cur_spell_rating = spell_desire[D_SPOWER] * spower;

		/* Bonus for hps (Breaths) */
		if (spell_desire[D_HPS])
		{
			cur_spell_rating +=
				((spell_desire[D_HPS] * ((hp >= 2000) ? 2000 : hp)) / 10);

			/* low level creatures need to be encouraged to breath more */
			cur_spell_rating += 200;
		}

		/* Bonus for monster level */
		if (spell_desire[D_RLEV])
			cur_spell_rating += spell_desire[D_RLEV] * rlev;

		/* Bonus if want summon and this spell is helpful */
		if (spell_desire[D_SUMM] && want_summon)
			cur_spell_rating +=
				(want_summon * rlev * spell_desire[D_SUMM]);

		/* Bonus if wounded and this spell is helpful */
		if (spell_desire[D_HURT] && want_hps)
			cur_spell_rating += (want_hps * spell_desire[D_HURT] * spower);

		/* Bonus if low on mana and this spell is helpful */
		if (spell_desire[D_MANA] && want_mana)
			cur_spell_rating +=
				(want_mana * spell_desire[D_MANA] * spower);

		/* Bonus if want to flee and this spell is helpful */
		if (spell_desire[D_ESC] && want_escape)
			cur_spell_rating += (want_escape * spell_desire[D_ESC] * rlev);

		/* Bonus if want a tactical move and this spell is helpful */
		if (spell_desire[D_TACT] && want_tactic)
			cur_spell_rating +=
				(want_tactic * spell_desire[D_TACT] * rlev);

		/* Penalty if this spell is resisted */
		if (spell_desire[D_RES])
			cur_spell_rating =
				(cur_spell_rating * (100 - find_resist(m_idx,
						spell_desire[D_RES]))) / 100;

		/* Penalty for range if attack drops off in power */
		if (spell_range)
		{
			cur_range = m_ptr->cdis;
			while (cur_range-- > spell_range)
				cur_spell_rating =
					(cur_spell_rating * spell_desire[D_RANGE]) / 100;
		}

		/* Bonus for harassment spells at first */
		if (is_harass && m_ptr->harass)
			cur_spell_rating += 2 * cur_spell_rating / 3;

		/* Random factor; less random for smart monsters */
		if (r_ptr->flags2 & (RF2_SMART))
			cur_spell_rating *= 8 + rand_int(5);
		else
			cur_spell_rating *= 6 + rand_int(9);

		/* Is this the best spell yet? */
		if (cur_spell_rating > best_spell_rating)
		{
			best_spell_rating = cur_spell_rating;
			best_spell = i + 96;
		}

	}

	if (p_ptr->wizard)
		msg_format("Spell rating: %i.", best_spell_rating);

	/* If we used a harassment spell, lower the bias to use them early */
	if (is_harass && m_ptr->harass)
		m_ptr->harass--;

	/* Return Best Spell */
	return (best_spell);

}

/*
 * Using an input value for average damage, and another that 
 * controls variability, return the actual base damage of a 
 * monster's attack spell.  The larger the value for "dice", the 
 * less likely the damage will vary greatly.
 *
 * Do not return a value greater than 3/2rds or less than half the 
 * average, or that differs from the average by more than 150.
 */
static int get_dam(int av_dam, int dice)
{
	int dam;

	/* Handle extreme values. */
	if (av_dam < 2)
		return (av_dam == 1 ? 1 : 0);
	if (dice < 2)
		dice = 2;

	/* Calculate actual damage. */
	if ((av_dam < 6) || (av_dam < dice))
		dam = damroll(1, av_dam * 2 - 1);
	else if (av_dam < 12)
		dam = damroll(2, av_dam - 1);
	else
		dam =
			damroll(dice,
			(av_dam * 2 / dice) - 1) + rand_int(((av_dam * 2) % dice) + 1);

	/* Boundary control (to reduce instadeaths). */
	if (av_dam > 9)
	{
		/* Control proportions. */
		if (dam > 3 * av_dam / 2)
			dam = 3 * av_dam / 2;
		if (dam < av_dam / 2)
			dam = av_dam / 2;

		/* Control actual damage. */
		if (dam > av_dam + 150)
			dam = av_dam + 150;
		if (dam < av_dam - 150)
			dam = av_dam - 150;
	}

	/* Return the calculated damage. */
	return (dam);
}


/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void mon_bolt(int m_idx, int typ, int dam)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY;

	/* Target the player with a bolt attack */
	(void) project(m_idx, 0, py, px, dam, typ, flg, 0, 0);
}

/*
 * Cast a beam at the player, sometimes with limited range.
 * Do not stop if we hit a monster
 * Affect grids, monsters, and the player
 */
static void mon_beam(int m_idx, int typ, int dam, int range)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

	/* Use a arc of degree 0 if range is limited. */
	if (range < MAX_SIGHT)
	{
		flg |= PROJECT_ARC;

		/* Paranoia */
		if (range > 25)
			range = 25;

		/* Target the player with a limited-range beam. */
		(void) project(m_idx, range, py, px, dam, typ, flg, 0,
			(byte) (range * 10));
	}

	/* Otherwise, use a standard beam. */
	else
	{
		flg |= PROJECT_BEAM;

		/* Target the player with a standard beam attack. */
		(void) project(m_idx, 0, py, px, dam, typ, flg, 0, 0);

	}
}

/*
 * Cast a ball spell at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and (specifically) the player
 */
static void mon_ball(int m_idx, int typ, int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

	/* Target the player with a ball attack */
	(void) project(m_idx, rad, py, px, dam, typ, flg, 0, 0);
}

/*
 * Breathe or cast an arc-shaped spell at the player.
 * Use an arc spell of specified range and width.
 * Optionally, do not harm monsters with the same r_idx.
 * Affect grids, objects, monsters, and (specifically) the player
 */
static void mon_arc(int m_idx, int typ, bool noharm, int dam, int rad,
	int degrees_of_arc)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Diameter of source of energy is normally, but not always, 20. */
	int diameter_of_source = 20;

	int flg =
		PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_ARC |
		PROJECT_PLAY;

	/* Can optionally ignore monsters with the same r_idx. */
	if (noharm)
		flg |= PROJECT_SAFE;

	/* Radius of zero means no fixed limit. */
	if (rad == 0)
		rad = MAX_SIGHT;

	/* Calculate the effective diameter of the energy source, if necessary. */
	if (degrees_of_arc < 60)
	{
		if (degrees_of_arc == 0)
			diameter_of_source = rad * 10;
		else
			diameter_of_source = diameter_of_source * 60 / degrees_of_arc;
	}
	/* XXX XXX -- POWERFUL monster breaths lose less damage with range. */
	if (r_ptr->flags2 & (RF2_POWERFUL))
		diameter_of_source *= 2;

	/* Max */
	if (diameter_of_source > 250)
		diameter_of_source = 250;

	/* Target the player with an arc-shaped attack. */
	(void) project(m_idx, rad, py, px, dam, typ, flg, degrees_of_arc,
		(byte) diameter_of_source);
}


/*
 * Monster attempts to make a ranged (non-melee) attack.
 *
 * Determine if monster can attack at range, then see if it will.  Use 
 * the helper function "choose_attack_spell()" to pick a physical ranged 
 * attack, magic spell, or summon.  Execute the attack chosen.  Process 
 * its effects, and update character knowledge of the monster.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Note the special "MFLAG_NICE" flag, which prevents a monster from using
 * any spell attacks until the player has had a single chance to move.
 */
bool make_attack_spell(int m_idx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int k, chance, thrown_spell, rlev, spower, rad;
	bool archery_only = FALSE;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	/* Target player */
	int x = px;
	int y = py;

	/* Summon count */
	int count = 0;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Can the player see the monster casting the spell? */
	bool seen = (!blind && m_ptr->ml);

	/*** Determine if monster /can/ use a ranged attack. ***/

	/* Cannot cast spells beyond maximum range. */
	if (m_ptr->cdis > MAX_RANGE)
		return (FALSE);

	/* Cannot cast spells when confused. */
	if (m_ptr->confused)
		return (FALSE);

	/* Cannot cast spells when nice. */
	if (m_ptr->mflag & (MFLAG_NICE))
		return (FALSE);

	/* Cannot cast spells if player is leaving the level. */
	if (p_ptr->leaving)
		return (FALSE);


	/*** Determine if the monster /will/ use a ranged attack. */

	/* 
	 * Extract the spell probability.  Currently, innate ranged attacks and 
	 * magic spells have the same probability.  This may not always be so.
	 */
	chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

	/* Stunned monsters cast spells half as often. */
	if (m_ptr->stunned)
		chance /= 2;

	/* Can cast no spells. */
	if (!chance)
		return (FALSE);

	/* Cannot cast a spell this time.  Archers can still fire missiles.  */
	if (randint(100) > chance)
	{
		/* If not an ARCHER, give up */
		if (!(r_ptr->flags2 & (RF2_ARCHER)))
			return (FALSE);

		/* If at point blank, give up */
		if (m_ptr->cdis < 2)
			return (FALSE);

		/* Occasionally give up anyway */
		if (rand_int(8) == 0)
			return (FALSE);

		/* Otherwise free shot */
		else
			archery_only = TRUE;
	}

	/* Choose a spell to cast */
	thrown_spell = choose_attack_spell(m_idx, archery_only, 0);

	/* Abort if no spell was chosen */
	if (!thrown_spell)
		return (FALSE);

	/* Hack - Spend mana */
	if (thrown_spell >= 224)
		return (FALSE);
	else if (thrown_spell >= 192)
		m_ptr->mana -= mana_cost_RF7[thrown_spell - 192];
	else if (thrown_spell >= 160)
		m_ptr->mana -= mana_cost_RF6[thrown_spell - 160];
	else if (thrown_spell >= 128)
		m_ptr->mana -= mana_cost_RF5[thrown_spell - 128];
	else if (thrown_spell >= 96)
		m_ptr->mana -= mana_cost_RF4[thrown_spell - 96];
	else
		return (FALSE);

	/*** Get some info. ***/

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Extract the monster's spell power.  Must be at least 1. */
	spower = ((r_ptr->spell_power > 1) ? r_ptr->spell_power : 1);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, 0x88);

	sound(SNDGRP_MONSTER_SPELL, thrown_spell - 96, 0); /* ALLOW_SOUND */

	/*** Execute the ranged attack chosen. ***/
	switch (thrown_spell)
	{
			/* RF4_SHRIEK */
		case 96 + 0:
		{
			disturb(1, 0);
			if (r_ptr->flags2 & (RF2_SMART))
				msg_format("%^s shouts for help.", m_name);
			else
				msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx, FALSE);
			break;
		}

			/* RF4_LASH  (used for whips, but also for spitting) */
		case 96 + 1:
		{
			/* Attack type and descriptions. */
			int typ;
			cptr desc;
			cptr add_of;


			/* Get damage and effect of first melee blow. */
			int effect = r_ptr->blow[0].effect;
			int damage = damroll(r_ptr->blow[0].d_dice,
				r_ptr->blow[0].d_side);

			/* Add some more damage for other melee blows. */
			if (r_ptr->blow[1].d_dice)
				damage +=
					damroll(r_ptr->blow[1].d_dice,
					r_ptr->blow[1].d_side) / 2;
			if (r_ptr->blow[2].d_dice)
				damage +=
					damroll(r_ptr->blow[2].d_dice,
					r_ptr->blow[2].d_side) / 2;
			if (r_ptr->blow[3].d_dice)
				damage +=
					damroll(r_ptr->blow[3].d_dice,
					r_ptr->blow[3].d_side) / 2;

			/* Stop if no damage possible */
			if (!damage)
				break;

			/* Determine projection type, using effect.
			 * GF_WHIP is used for pure damage with no 
			 * extras -- many attacks need this.
			 */
			switch (effect)
			{
				case RBE_HURT:
				{
					typ = GF_WHIP;
					desc = "";
					add_of = "";
					break;
				}
				case RBE_ACID:
				{
					/* Some of attack is pure damage, and so 
					 * resists should not be allowed to reduce 
					 * damage as much as they do normally.
					 * Damage will be reduced later.
					 */
					if (p_ptr->resist_acid)
						damage *= 2;
					if (p_ptr->oppose_acid)
						damage *= 2;
					typ = GF_ACID;
					desc = " acid";
					add_of = " of";
					break;
				}
				case RBE_ELEC:
				{
					if (p_ptr->resist_elec)
						damage *= 2;
					if (p_ptr->oppose_elec)
						damage *= 2;

					typ = GF_ELEC;
					desc = " lightning";
					add_of = " of";
					break;
				}
				case RBE_FIRE:
				{
					if (p_ptr->resist_fire)
						damage *= 2;
					if (p_ptr->oppose_fire)
						damage *= 2;

					typ = GF_FIRE;
					desc = " fire";
					add_of = " of";
					break;
				}
				case RBE_COLD:
				{
					if (p_ptr->resist_cold)
						damage *= 2;
					if (p_ptr->oppose_cold)
						damage *= 2;

					typ = GF_COLD;
					desc = " frost";
					add_of = " of";
					break;
				}
				case RBE_POISON:
				{
					if (p_ptr->resist_pois)
						damage *= 2;
					if (p_ptr->oppose_pois)
						damage *= 2;

					typ = GF_POIS;
					desc = " venom";
					add_of = " of";
					break;
				}
				case RBE_BLIND:
				{
					typ = GF_DARK;
					desc = " blackness";
					add_of = " of";
					break;
				}
				case RBE_CONFUSE:
				case RBE_PARALYZE:
				{
					typ = GF_DARK;
					desc = " confusion";
					add_of = " of";
					break;
				}

				case RBE_UN_BONUS:
				case RBE_UN_POWER:
				{
					typ = GF_DISENCHANT;
					desc = " unmagic";
					add_of = " of";
					break;
				}
				case RBE_LOSE_STR:
				case RBE_LOSE_DEX:
				case RBE_LOSE_CON:
				case RBE_LOSE_INT:
				case RBE_LOSE_WIS:
				case RBE_LOSE_CHR:
				case RBE_LOSE_ALL:
				{
					typ = GF_TIME;
					desc = " ruination";
					add_of = " of";
					break;
				}
				case RBE_EXP_10:
				case RBE_EXP_20:
				case RBE_EXP_40:
				case RBE_EXP_80:
				{
					typ = GF_NETHER;
					desc = " withering";
					add_of = " of";
					break;
				}
				default:
				{
					typ = GF_WHIP;
					desc = "";
					add_of = "";
					break;
				}
			}
			/* XXX -- Animals spit.   Acid-users spit. */
			if ((r_ptr->flags3 & (RF3_ANIMAL)) || (typ == GF_ACID))
			{
				if (blind)
					msg_print("You hear a soft sound.");
				else
					msg_format("%^s spits%s at you.", m_name, desc);
			}
			/* All other creatures use a whip. */
			else
			{
				if (blind)
					msg_print("You hear a crack.");
				else
					msg_format("%^s lashes at you with a whip%s%s.",
						m_name, add_of, desc);
			}

			/* Crack the whip, or spit - range 3 */
			mon_beam(m_idx, typ, damage, 3);

			break;
		}

			/* RF4_BOULDER */
		case 96 + 2:
		{
			disturb(1, 0);
			if (blind)
				msg_print("You hear something grunt with exertion.");
			else
				msg_format("%^s hurls a boulder at you.", m_name);
			mon_bolt(m_idx, GF_ROCK, get_dam(spower * 4, 10));
			break;
		}

			/* RF4_SHOT */
		case 96 + 3:
		{
			disturb(1, 0);
			if (blind)
				msg_print("You hear something whirl towards you.");
			else if (spower < 5)
				msg_format("%^s slings a pebble at you.", m_name);
			else if (spower < 15)
				msg_format("%^s slings a leaden pellet at you.", m_name);
			else
				msg_format("%^s slings a seeker shot at you.", m_name);

			mon_bolt(m_idx, GF_SHOT, get_dam(spower * 4, 10));
			break;
		}

			/* RF4_ARROW */
		case 96 + 4:
		{
			disturb(1, 0);
			if (spower < 8)
			{
				if (blind)
					msg_print("You hear a soft twang.");
				else
					msg_format("%^s fires a small arrow.", m_name);
			}
			else if (spower < 15)
			{
				if (blind)
					msg_format("You hear a twang.");
				else
					msg_format("%^s fires an arrow.", m_name);
			}
			else
			{
				if (blind)
					msg_print("You hear a loud thwang.");
				else
					msg_format("%^s fires a seeker arrow.", m_name);
			}

			mon_bolt(m_idx, GF_ARROW, get_dam(spower * 4, 10));
			break;
		}

			/* RF4_BOLT */
		case 96 + 5:
		{
			disturb(1, 0);
			if (spower < 8)
			{
				if (blind)
					msg_print("You hear a soft twung.");
				else
					msg_format("%^s fires a little bolt.", m_name);
			}
			else if (spower < 15)
			{
				if (blind)
					msg_format("You hear a twung.");
				else
					msg_format("%^s fires a crossbow bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_print("You hear a loud thwung.");
				else
					msg_format("%^s fires a seeker bolt.", m_name);
			}

			mon_bolt(m_idx, GF_ARROW, get_dam(spower * 4, 10));
			break;
		}

			/* RF4_MISSL */
		case 96 + 6:
		{
			disturb(1, 0);
			if (blind)
				msg_print("You hear something coming at you.");
			else
				msg_format("%^s fires a missile.", m_name);
			mon_bolt(m_idx, GF_MISSILE, get_dam(spower * 3, 10));
			break;
		}

			/* RF4_PMISSL */
		case 96 + 7:
		{
			disturb(1, 0);
			if (blind)
				msg_print("You hear a soft 'fftt' sound.");
			else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
				msg_format("%^s hurls a black dart at you!", m_name);
			else
				msg_format("%^s whips a poisoned dart at you.", m_name);
			mon_bolt(m_idx, GF_PMISSILE, get_dam(spower * 3, 10));
			break;
		}


			/* RF4_BRTH_ACID */
		case 96 + 8:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes acid.", m_name);

			/* 
			 * Breaths are 40-degree arcs for POWERFUL monsters, 
			 * 20 degrees for others.
			 */
			mon_arc(m_idx, GF_ACID, TRUE,
				((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_ELEC */
		case 96 + 9:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes lightning.", m_name);
			mon_arc(m_idx, GF_ELEC, TRUE,
				((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_FIRE */
		case 96 + 10:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes fire.", m_name);
			mon_arc(m_idx, GF_FIRE, TRUE,
				((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_COLD */
		case 96 + 11:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes frost.", m_name);
			mon_arc(m_idx, GF_COLD, TRUE,
				((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_POIS */
		case 96 + 12:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes gas.", m_name);
			mon_arc(m_idx, GF_POIS, TRUE,
				((m_ptr->hp / 4) > 500 ? 500 : (m_ptr->hp / 4)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 50 : 30));
			break;
		}

			/* RF4_BRTH_PLAS */
		case 96 + 13:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes plasma.", m_name);
			mon_arc(m_idx, GF_PLASMA, TRUE,
				((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_LITE */
		case 96 + 14:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes light.", m_name);
			mon_arc(m_idx, GF_LITE, TRUE,
				((3 * m_ptr->hp / 10) > 500 ? 500 : (3 * m_ptr->hp / 10)),
				0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_DARK */
		case 96 + 15:
		{
			disturb(1, 0);

			if (!(r_ptr->flags2 & (RF2_MORGUL_MAGIC)))
			{
				if (blind)
					msg_format("%^s breathes.", m_name);
				msg_format("%^s breathes darkness.", m_name);
				mon_arc(m_idx, GF_DARK, TRUE,
					((3 * m_ptr->hp / 10) >
					500 ? 500 : (3 * m_ptr->hp / 10)), 0,
					(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			}
			else
			{
				if (blind)
					msg_format("%^s breathes.", m_name);
				msg_format("%^s breathes Night.", m_name);
				mon_arc(m_idx, GF_MORGUL_DARK, TRUE,
					((3 * m_ptr->hp / 10) >
						500 ? 500 : (3 * m_ptr->hp / 10)), 0,
					(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			}
			break;
		}

			/* RF4_BRTH_CONFU */
		case 96 + 16:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes confusion.", m_name);
			mon_arc(m_idx, GF_CONFUSION, TRUE,
				((3 * m_ptr->hp / 10) > 500 ? 500 : (3 * m_ptr->hp / 10)),
				0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_SOUND */
		case 96 + 17:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes sound.", m_name);
			mon_arc(m_idx, GF_SOUND, TRUE,
				((m_ptr->hp / 4) > 250 ? 250 : (m_ptr->hp / 4)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 50 : 30));
			break;
		}

			/* RF4_BRTH_SHARD */
		case 96 + 18:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes shards.", m_name);
			mon_arc(m_idx, GF_SHARD, TRUE,
				((3 * m_ptr->hp / 10) > 400 ? 400 : (3 * m_ptr->hp / 10)),
				0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_INER */
		case 96 + 19:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes inertia.", m_name);
			mon_arc(m_idx, GF_INERTIA, TRUE,
				((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_GRAV */
		case 96 + 20:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes gravity.", m_name);
			mon_arc(m_idx, GF_GRAVITY, TRUE,
				((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_FORCE */
		case 96 + 21:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes force.", m_name);
			mon_arc(m_idx, GF_FORCE, TRUE,
				((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 60 : 60));
			break;
		}

			/* RF4_BRTH_NEXUS */
		case 96 + 22:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes nexus.", m_name);
			mon_arc(m_idx, GF_NEXUS, TRUE,
				((m_ptr->hp / 3) > 300 ? 300 : (m_ptr->hp / 3)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_NETHR */
		case 96 + 23:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes nether.", m_name);
			mon_arc(m_idx, GF_NETHER, TRUE,
				((3 * m_ptr->hp / 10) > 600 ? 600 : (3 * m_ptr->hp / 10)),
				0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_CHAOS */
		case 96 + 24:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes chaos.", m_name);
			mon_arc(m_idx, GF_CHAOS, TRUE,
				((3 * m_ptr->hp / 10) > 600 ? 600 : (3 * m_ptr->hp / 10)),
				0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_DISE */
		case 96 + 25:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes disenchantment.", m_name);
			mon_arc(m_idx, GF_DISENCHANT, TRUE,
				((3 * m_ptr->hp / 10) > 400 ? 400 : (3 * m_ptr->hp / 10)),
				0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_BRTH_TIME */
		case 96 + 26:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes time.", m_name);
			mon_arc(m_idx, GF_TIME, TRUE,
				((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)), 0,
				(r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

			/* RF4_XXX4 */
		case 96 + 27:
		{
			break;
		}

			/* RF4_XXX5 */
		case 96 + 28:
		{
			break;
		}

			/* RF4_XXX6 */
		case 96 + 29:
		{
			break;
		}

			/* RF4_XXX7 */
		case 96 + 30:
		{
			break;
		}

			/* RF4_XXX8 */
		case 96 + 31:
		{
			break;
		}

			/* RF5_BALL_ACID */
		case 128 + 0:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts an acid ball.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts an acid ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s invokes a storm of acid.", m_name);
				if (spower < 120)
					rad = 3;
				else
					rad = 4;
			}
			mon_ball(m_idx, GF_ACID, get_dam(5 * spower, 6), rad);
			break;
		}

			/* RF5_BALL_ELEC */
		case 128 + 1:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a ball of electricity.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a ball of electricity.", m_name);
				rad = 2;
			}

			/* Electricity is the most variable of all attacks at high level. */
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);

				if (rand_int(3) != 0)
				{
					msg_format("%^s invokes a storm of electricity.",
						m_name);
					if (spower < 120)
						rad = 3;
					else
						rad = 4;
					spower = 3 * spower / 4;
				}
				else
				{
					msg_format
						("%^s calls a massive stroke of lightning down upon you!",
						m_name);
					rad = 0;
					spower = 3 * spower / 2;
				}
			}
			mon_ball(m_idx, GF_ELEC, get_dam(5 * spower, 6), rad);
			break;
		}

			/* RF5_BALL_FIRE */
		case 128 + 2:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a ball of %sfire.", m_name,
						(r_ptr->flags2 & (RF2_UDUN_MAGIC) ? "hell" : ""));
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a ball of %sfire.", m_name,
						(r_ptr->flags2 & (RF2_UDUN_MAGIC) ? "hell" : ""));
				rad = 2;
			}
			else if (spower < 110)
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else if (r_ptr->flags2 & (RF2_UDUN_MAGIC))
					msg_format("%^s invokes a storm of Udun-fire.",
						m_name);
				else
					msg_format("%^s invokes a firestorm.", m_name);
				rad = 3;
			}
			else
			{
				if (blind)
					msg_format("%^s intones in rising wrath.", m_name);
				else if (r_ptr->flags2 & (RF2_UDUN_MAGIC))
					msg_format("%^s calls upon the fires of Udun!",
						m_name);

				else
					msg_format("%^s conjures up a maelstrom of fire!",
						m_name);
				rad = 4;
			}
			if (r_ptr->flags2 & (RF2_UDUN_MAGIC))
				mon_ball(m_idx, GF_HELLFIRE, get_dam(spower * 6, 6), rad);
			else
				mon_ball(m_idx, GF_FIRE, get_dam(5 * spower, 6), rad);
			break;
		}

			/* RF5_BALL_COLD */
		case 128 + 3:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a frost ball.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a frost ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s invokes a storm of frost.", m_name);
				if (spower < 120)
					rad = 3;
				else
					rad = 4;
			}
			mon_ball(m_idx, GF_COLD, get_dam(5 * spower, 6), rad);
			break;
		}

			/* RF5_BALL_POIS */
		case 128 + 4:
		{
			disturb(1, 0);
			if (spower < 15)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a stinking cloud.", m_name);
				rad = 2;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a venomous cloud.", m_name);
				rad = 3;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s invokes a storm of poison.", m_name);
				if (spower < 120)
					rad = 4;
				else
					rad = 5;
			}
			mon_ball(m_idx, GF_POIS, get_dam(2 * spower, 8), rad);
			break;
		}

			/* RF5_BALL_LITE */
		case 128 + 5:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a ball of light.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s invokes a starburst.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s invokes a starburst.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_LITE, get_dam(3 * spower, 12), rad);
			break;
		}

			/* RF5_BALL_DARK */
		case 128 + 6:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
					msg_format("%^s casts a ball of Night.", m_name);
				else
					msg_format("%^s casts a ball of darkness.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
					msg_format("%^s casts a ball of Night.", m_name);
				else
					msg_format("%^s casts a ball of darkness.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
					msg_format("%^s invokes a storm of Night.", m_name);
				else
					msg_format("%^s invokes a darkness storm.", m_name);
				if (spower < 110)
					rad = 3;
				else
					rad = 4;
			}
			if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
				mon_ball(m_idx, GF_MORGUL_DARK, get_dam(3 * spower, 12),
					rad);
			else
				mon_ball(m_idx, GF_DARK, get_dam(3 * spower, 12), rad);
			break;
		}

			/* RF5_BALL_CONFU */
		case 128 + 7:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a ball of confusion.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a ball of confusion.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s invokes a storm of confusion.",
						m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_CONFUSION, get_dam(3 * spower, 12), rad);
			break;
		}

			/* RF5_BALL_SOUND */
		case 128 + 8:
		{
			disturb(1, 0);
			if (spower < 15)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s calls up a blast of sound.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s invokes a thunderclap.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s unleashes a cacophony of sound.",
						m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_SOUND, get_dam(2 * spower, 12), rad);
			break;
		}

			/* RF5_BALL_SHARD */
		case 128 + 9:
		{
			disturb(1, 0);
			if (spower < 15)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s calls up up a blast of shards.",
						m_name);
				rad = 1;
			}
			else if (spower < 90)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s calls up a whirlwind of shards.",
						m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s invokes a storm of knives!", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_SHARD, get_dam(3 * spower, 12), rad);
			break;
		}

			/* RF5_BALL_STORM */
		case 128 + 10:
		{
			disturb(1, 0);

			if (spower < 30)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s gestures fluidly.", m_name);
				msg_print("You are surrounded by a little storm.");
				rad = 2;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s gestures fluidly.", m_name);
				msg_print("You are engulfed in a whirlpool.");
				rad = 3;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s gestures fluidly.", m_name);
				msg_print
					("You are lost in a raging tempest of wind and water!");
				rad = 5;
			}
			mon_ball(m_idx, GF_STORM, get_dam(4 * spower, 6), rad);
			break;
		}

			/* RF5_BALL_NETHR */
		case 128 + 11:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s whispers nastily.", m_name);
				else
					msg_format("%^s casts an orb of nether.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs a deadly word.", m_name);
				else
					msg_format("%^s casts a nether ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s intones with deadly menace.", m_name);
				else
					msg_format("%^s calls up a storm of nether magics.",
						m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_NETHER, get_dam(3 * spower, 12), rad);
			break;
		}

			/* RF5_BALL_CHAOS */
		case 128 + 12:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a sphere of chaos.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a ball of raw chaos.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s invokes a storm of chaos.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_CHAOS, get_dam(3 * spower, 8), rad);
			break;
		}

			/* RF5_BALL_MANA */
		case 128 + 13:
		{
			disturb(1, 0);
			if (spower < 40)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a mana burst.", m_name);
				rad = 1;
			}
			else if (spower < 90)
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a mana ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind)
					msg_format("%^s chants powerfully.", m_name);
				else
					msg_format("%^s invokes a storm of mana.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_MANA, get_dam(2 * spower, 16), rad);

			break;
		}

			/* RF5_XXX2 */
		case 128 + 14:
		{
			break;
		}

			/* RF5_XXX3 */
		case 128 + 15:
		{
			break;
		}

			/* RF5_BOLT_ACID */
		case 128 + 16:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts an acid bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a bolt of acid.", m_name);
			}
			mon_bolt(m_idx, GF_ACID, get_dam(4 * spower, 6));
			break;
		}

			/* RF5_BOLT_ELEC */
		case 128 + 17:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a bolt of electricity.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a bolt of lightning.", m_name);
			}
			mon_bolt(m_idx, GF_ELEC, get_dam(4 * spower, 6));
			break;
		}

			/* RF5_BOLT_FIRE */
		case 128 + 18:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a fire bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s throws a fiery sphere at you.",
						m_name);
			}
			mon_bolt(m_idx, GF_FIRE, get_dam(4 * spower, 6));
			break;
		}

			/* RF5_BOLT_COLD */
		case 128 + 19:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a frost bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a frost bolt.", m_name);
			}
			mon_bolt(m_idx, GF_COLD, get_dam(4 * spower, 6));
			break;
		}

			/* RF5_BOLT_POIS */
		case 128 + 20:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a poison bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a bolt of venom.", m_name);
			}
			mon_bolt(m_idx, GF_POIS, get_dam(12 * spower / 5, 8));
			break;
		}

			/* RF5_BOLT_PLAS */
		case 128 + 21:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a plasma bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a bolt of plasma.", m_name);
			}
			mon_bolt(m_idx, GF_PLASMA, get_dam(5 * spower, 8));
			break;
		}

			/* RF5_BOLT_ICE */
		case 128 + 22:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts an ice bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a bolt of ice.", m_name);
			}
			mon_bolt(m_idx, GF_ICE, get_dam(5 * spower, 6));
			break;
		}

			/* RF5_BOLT_WATER */
		case 128 + 23:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a water bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a water bolt.", m_name);
			}
			mon_bolt(m_idx, GF_WATER, get_dam(2 * spower, 12));
			break;
		}

			/* RF5_BOLT_NETHR */
		case 128 + 24:
		{
			disturb(1, 0);
			if (spower < 80)
			{
				if (blind)
					msg_format("%^s whispers nastily.", m_name);
				else
					msg_format("%^s casts a nether bolt.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs a deadly word.", m_name);
				else
					msg_format("%^s hurls a black orb at you.", m_name);
			}
			mon_bolt(m_idx, GF_NETHER, get_dam(5 * spower / 2, 12));
			break;
		}

			/* RF5_BOLT_MANA */
		case 128 + 25:
		{
			disturb(1, 0);
			if ((spower < 5) || (spower <= rlev / 10))
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts a magic missile.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts a mana bolt.", m_name);
			}
			mon_bolt(m_idx, GF_MANA, get_dam(2 * spower, 16));
			break;
		}

			/* RF5_BOLT_XXX1 */
		case 128 + 26:
		{
		}

			/* RF5_BEAM_ELEC */
		case 128 + 27:
		{
			disturb(1, 0);
			if (blind)
				msg_print("You feel a crackling in the air.");
			else
				msg_format("%^s shoots a spark of lightning at you.",
					m_name);

			mon_beam(m_idx, GF_ELEC, get_dam(5 * spower, 6), 6);
			break;
		}

			/* RF5_BEAM_ICE */
		case 128 + 28:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s casts an icy lance.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s murmurs deeply.", m_name);
				else
					msg_format("%^s casts an icy lance.", m_name);
			}
			mon_beam(m_idx, GF_ICE, get_dam(5 * spower, 6), 12);
			break;
		}

			/* RF5_BEAM_NETHR */
		case 128 + 29:
		{
			disturb(1, 0);
			if (spower < 30)
			{
				if (blind)
					msg_format("%^s whispers nastily.", m_name);
				else
					msg_format("%^s casts a beam of nether.", m_name);
			}
			else if (spower < 90)
			{
				if (blind)
					msg_format("%^s murmurs a deadly word.", m_name);
				else
					msg_format("%^s hurls a nether lance.", m_name);
			}
			else
			{
				if (blind)
					msg_format("%^s intones with deadly menace.", m_name);
				else
					msg_format("%^s unleashes a ray of death.", m_name);
			}
			mon_beam(m_idx, GF_NETHER, get_dam(3 * spower, 12), 10);
			break;
		}

			/* RF5_ARC__HFIR */
		case 128 + 30:
		{
			disturb(1, 0);
			/* Must be powerful and have Udun-magic to get an arc. */
			if ((spower > 50) && (r_ptr->flags2 & (RF2_UDUN_MAGIC)))
			{
				if (blind)
					msg_format("%^s speaks a word of peril.", m_name);
				else
					msg_format("%^s invokes a storm of hellfire!", m_name);

				/* Absolutely formidable close up, less so at range. */
				mon_arc(m_idx, GF_HELLFIRE, FALSE, get_dam(7 * spower, 8),
					6, 60);
			}

			/* For weak Udun-magic casters, a column of hellfire. */
			else if (r_ptr->flags2 & (RF2_UDUN_MAGIC))
			{
				if (blind)
					msg_format("%^s murmurs darkly.", m_name);
				else
					msg_format
						("%^s gestures, and you are enveloped in hellfire.",
						m_name);
				mon_ball(m_idx, GF_HELLFIRE, get_dam(5 * spower, 8), 0);
			}

			/* Column of fire for everyone else. */
			else
			{
				if (blind)
					msg_format("%^s mutters.", m_name);
				else
					msg_format
						("%^s gestures, and you are enveloped in fire.",
						m_name);
				mon_ball(m_idx, GF_FIRE, get_dam(5 * spower, 8), 0);

			}
			break;
		}

			/* RF5_ARC__WALL */
		case 128 + 31:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mutters.", m_name);
			else
				msg_format("%^s calls up a wall of force.", m_name);
			mon_arc(m_idx, GF_FORCE, FALSE, get_dam(3 * spower, 18), 8,
				60);
			break;
		}

			/* RF6_HASTE */
		case 160 + 0:
		{
			disturb(1, 0);
			if (blind)
			{
				msg_format("%^s mumbles.", m_name);
			}
			else
			{
				msg_format("%^s concentrates on %s body.", m_name, m_poss);
			}

			/* Allow a quick speed increase if not already greatly hasted. */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				msg_format("%^s starts moving faster.", m_name);
				m_ptr->mspeed += 10;
			}

			/* Allow small speed increases to base+20 */
			else if (m_ptr->mspeed < r_ptr->speed + 20)
			{
				msg_format("%^s starts moving slightly faster.", m_name);
				m_ptr->mspeed += 2;
			}

			break;
		}

			/* RF6_ADD_MANA */
		case 160 + 1:
		{
			int j;

			disturb(1, 0);

			if (seen)
			{
				if (blind)
				{
					msg_format("%^s mumbles.", m_name);
				}
				else
				{
					msg_format("%^s gathers %s power.", m_name, m_poss);
				}
			}
			/* Increase current mana */
			j = (spower / 20) + 5;
			if (j > r_ptr->mana - m_ptr->mana)
				m_ptr->mana = r_ptr->mana;
			else
				m_ptr->mana += j;

			break;
		}

			/* RF6_HEAL */
		case 160 + 2:
		{
			int cost;

			disturb(1, 0);

			/* Message */
			if (seen)
			{
				if (blind)
				{
					msg_format("%^s mumbles.", m_name);
				}
				else
				{
					msg_format("%^s concentrates on %s wounds.", m_name,
						m_poss);
				}
			}

			/* Costs mana (and has a limit of spower * 5) */
			if (((m_ptr->maxhp - m_ptr->hp) <= spower * 5) &&
				((m_ptr->maxhp - m_ptr->hp) <= m_ptr->mana * 50))
			{
				m_ptr->hp = m_ptr->maxhp;
				cost = 1 + ((m_ptr->maxhp - m_ptr->hp) / 50);
			}
			else if (spower <= m_ptr->mana * 10)
			{
				m_ptr->hp += spower * 5;
				cost = (spower + 9) / 10;
			}
			else
			{
				m_ptr->hp += m_ptr->mana * 50;
				cost = m_ptr->mana;
			}

			if (cost > m_ptr->mana)
				m_ptr->mana = 0;
			else
				m_ptr->mana -= cost;

			/* Fully healed */
			if (m_ptr->hp >= m_ptr->maxhp)
			{
				/* Fully healed */
				m_ptr->hp = m_ptr->maxhp;

				/* Message */
				if (seen)
				{
					msg_format("%^s looks very healthy!", m_name);
				}
				else
				{
					msg_format("%^s sounds very healthy!", m_name);
				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (seen)
				{
					msg_format("%^s looks healthier.", m_name);
				}
				else
				{
					msg_format("%^s sounds healthier.", m_name);
				}
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == m_idx)
				p_ptr->redraw |= (PR_HEALTH);

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				m_ptr->monfear = 0;

				/* Message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);
			}

			break;
		}

			/* RF6_CURE */
		case 160 + 3:
		{
			msg_format("%^s concentrates on %s ailments.", m_name, m_poss);

			/* Cancel stunning */
			if (m_ptr->stunned)
			{
				/* Cancel stunning */
				m_ptr->stunned = 0;

				/* Message */
				msg_format("%^s is no longer stunned.", m_name);
			}

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				m_ptr->monfear = 0;

				/* Message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);

			}

			/* Cancel (major) slowing */
			if (m_ptr->mspeed < r_ptr->speed - 5)
			{
				/* Cancel slowing */
				m_ptr->mspeed = r_ptr->speed;

				/* Message */
				msg_format("%^s is no longer slowed.", m_name);
			}

			/* Cancel Black Breath sometimes. */
			if ((m_ptr->black_breath) && (rlev + 20 > rand_int(120)))
			{
				/* Cancel Black Breath */
				m_ptr->black_breath = 0;

				/* Message */
				msg_format("The hold of the Black Breath on %s is broken.",
					m_name);
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == m_idx)
				p_ptr->redraw |= (PR_HEALTH);

			break;
		}

			/* RF6_BLINK */
		case 160 + 4:
		{
			disturb(1, 0);
			msg_format("%^s blinks away.", m_name);
			teleport_away(m_idx, 10);
			break;
		}

			/* RF6_TPORT */
		case 160 + 5:
		{
			disturb(1, 0);
			msg_format("%^s teleports away.", m_name);
			teleport_away(m_idx, MAX_SIGHT * 2 + 5);

			/* Hack - 'teleport stress' fatigues the monster */
			/* m_ptr->mana -= m_ptr->mana/3; */

			break;
		}

			/* RF6_XXX3 */
		case 160 + 6:
		{
			break;
		}

			/* RF6_TELE_SELF_TO */
		case 160 + 7:
		{
			/* Move monster near player (also updates "m_ptr->ml"). */
			teleport_towards(m_ptr->fy, m_ptr->fx, py, px);

			/* Monster is now visible, but wasn't before. */
			if ((!seen) && (m_ptr->ml))
			{
				/* Get the name (using "A"/"An") again. */
				monster_desc(ddesc, m_ptr, 0x08);

				/* Message */
				sound(SNDGRP_EVENT, SND_MONSTER_TELEPORT, 0x02); /* ALLOW_SOUND */
				msg_format("%^s suddenly appears.", ddesc);
			}
			/* Monster was visible before, but isn't now. */
			else if ((seen) && (!m_ptr->ml))
			{
				/* Message */
				sound(SNDGRP_EVENT, SND_MONSTER_TELEPORT, 0x02); /* ALLOW_SOUND */
				msg_format("%^s blinks away.", m_name);
			}
			/* Monster is visible both before and after. */
			else if ((seen) && (m_ptr->ml))
			{
				/* Message */
				sound(SNDGRP_EVENT, SND_MONSTER_TELEPORT, 0x02); /* ALLOW_SOUND */
				msg_format("%^s blinks toward you.", m_name);
			}

			/* Have we seen them at any point?  If so, we will learn about the spell. */
			if (m_ptr->ml)
				seen = TRUE;

			break;
		}
			/* RF6_TELE_TO */
		case 160 + 8:
		{
			disturb(1, 0);
			msg_format("%^s commands you to return.", m_name);
			teleport_player_to(m_ptr->fy, m_ptr->fx);
			break;
		}

			/* RF6_TELE_AWAY */
		case 160 + 9:
		{
			disturb(1, 0);
			msg_format("%^s teleports you away.", m_name);
			teleport_player(100, TRUE);
			break;
		}

			/* RF6_TELE_LEVEL */
		case 160 + 10:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles strangely.", m_name);
			else
				msg_format("%^s gestures at your feet.", m_name);
			if (p_ptr->resist_nexus)
			{
				msg_print("You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				teleport_player_level();
			}
			break;
		}

			/* RF6_XXX5 */
		case 160 + 11:
		{
			break;
		}

			/* RF6_DARKNESS */
		case 160 + 12:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s gestures in shadow.", m_name);
			(void) unlite_area(0, 3);
			break;
		}

			/* RF6_TRAPS */
		case 160 + 13:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles, and then cackles evilly.",
					m_name);
			else
				msg_format("%^s casts a spell and cackles evilly.",
					m_name);
			(void) trap_creation();
			break;
		}

			/* RF6_FORGET */
		case 160 + 14:
		{
			disturb(1, 0);
			msg_format("%^s tries to blank your mind.", m_name);

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else if (lose_all_info())
			{
				sound(SNDGRP_EVENT, SND_AMNESIA, 0); /* ALLOW_SOUND */
				msg_print("Your memories fade away.");
			}
			break;
		}

			/* RF6_DRAIN_MANA */
		case 160 + 15:
		{
			if (p_ptr->csp)
			{
				int r1;

				/* Disturb if legal */
				disturb(1, 0);

				/* Basic message */
				msg_format("%^s draws psychic energy from you!", m_name);

				/* Attack power */
				r1 = (randint(spower) / 20) + 1;

				/* Full drain */
				if (r1 >= p_ptr->csp)
				{
					r1 = p_ptr->csp;
					p_ptr->csp = 0;
					p_ptr->csp_frac = 0;
				}

				/* Partial drain */
				else
				{
					p_ptr->csp -= r1;
				}

				/* Redraw mana */
				p_ptr->redraw |= (PR_MANA);

				/* Window stuff */
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

				/* Replenish monster mana */
				if (m_ptr->mana < r_ptr->mana)
				{
					if (r1 > r_ptr->mana - m_ptr->mana)
					{
						r1 -= r_ptr->mana - m_ptr->mana;
						m_ptr->mana = r_ptr->mana;
					}
					else
					{
						m_ptr->mana += r1;
						r1 = 0;
					}
				}

				/* Heal the monster with remaining energy */
				if ((m_ptr->hp < m_ptr->maxhp) && (r1))
				{
					/* Heal */
					m_ptr->hp += (30 * (r1 + 1));
					if (m_ptr->hp > m_ptr->maxhp)
						m_ptr->hp = m_ptr->maxhp;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == m_idx)
						p_ptr->redraw |= (PR_HEALTH);

					/* Special message */
					if (seen)
					{
						msg_format("%^s appears healthier.", m_name);
					}
				}
			}
			break;
		}

			/* RF6_XXX6 */
		case 160 + 16:
		{
			break;
		}

			/* RF6_XXX7 */
		case 160 + 17:
		{
			break;
		}

			/* RF6_MIND_BLAST */
		case 160 + 18:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel something focusing on your mind.");
			}
			else
			{
				msg_format("%^s gazes deep into your eyes.", m_name);
			}

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");
				if (!p_ptr->resist_confu)
				{
					(void) set_confused(p_ptr->confused + rand_int(4) + 4);
				}
				take_hit(damroll(8, 8), ddesc);
			}
			break;
		}

			/* RF6_BRAIN_SMASH */
		case 160 + 19:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel something focusing on your mind.");

			}
			else
			{
				msg_format("%^s looks deep into your eyes.", m_name);
			}
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");
				take_hit(damroll(12, 15), ddesc);
				if (!p_ptr->resist_blind)
				{
					(void) set_blind(p_ptr->blind + 8 + rand_int(8));
				}
				if (!p_ptr->resist_confu)
				{
					(void) set_confused(p_ptr->confused + rand_int(4) + 4);
				}
				if (!p_ptr->free_act)
				{
					(void) set_paralyzed(p_ptr->paralyzed + rand_int(4) +
						4);
				}
				(void) set_slow(p_ptr->slow + rand_int(4) + 4);
			}
			break;
		}

			/* RF6_WOUND */
		case 160 + 20:
		{
			disturb(1, 0);

			if (spower < 7)
			{
				if (blind)
					msg_format("%^s mumbles.", m_name);
				else
					msg_format("%^s points at you and curses.", m_name);
				k = 1;
			}
			else if (spower < 15)
			{
				if (blind)
					msg_format("%^s mumbles deeply.", m_name);
				else
					msg_format("%^s points at you and curses horribly.",
						m_name);
				k = 2;
			}
			else if (spower < 30)
			{
				if (blind)
					msg_format("%^s murmurs loudly.", m_name);
				else
					msg_format("%^s points at you, incanting terribly.",
						m_name);
				k = 3;
			}
			else if (spower < 55)
			{
				if (blind)
					msg_format("%^s cries out wrathfully.", m_name);
				else
					msg_format
						("%^s points at you, screaming words of peril!",
						m_name);
				k = 4;
			}
			else
			{
				if (blind)
					msg_format("%^s screams the word 'DIE!'", m_name);
				else
					msg_format
						("%^s points at you, screaming the word DIE!",
						m_name);
				k = 5;
			}

			if (rand_int(rlev / 2 + 70) < p_ptr->skill_sav)
			{
				msg_format("You resist the effects%c",
					(spower < 30 ? '.' : '!'));
			}
			else
			{
				/* Inflict damage. */
				take_hit(get_dam(spower * 4, 6), ddesc);

				/* Cut the player depending on strength of spell. */
				if (k == 1)
					(void) set_cut(p_ptr->cut + 8 + damroll(2, 4));
				if (k == 2)
					(void) set_cut(p_ptr->cut + 23 + damroll(3, 8));
				if (k == 3)
					(void) set_cut(p_ptr->cut + 46 + damroll(4, 12));
				if (k == 4)
					(void) set_cut(p_ptr->cut + 95 + damroll(8, 15));
				if (k == 5)
					(void) set_cut(1200);
			}
			break;
		}

			/* RF6_XXX6 */
		case 160 + 21:
		{
			break;
		}

			/* RF6_XXX7 */
		case 160 + 22:
		{
			break;
		}

			/* RF6_XXX8 */
		case 160 + 23:
		{
			break;
		}

			/* RF6_XXX9 */
		case 160 + 24:
		{
			break;
		}

			/* RF6_HUNGER */
		case 160 + 25:
		{
			if (blind)
				msg_print("You suddenly feel hungry.");
			else
				msg_format
					("%^s gestures at you, and you suddenly feel hungry.",
					m_name);

			if (randint(100) > p_ptr->skill_sav)
			{
				/* Reduce food abruptly.  */
				(void) set_food(p_ptr->food - (p_ptr->food / 3));
			}

			else
				msg_print("You resist the effects!");

			break;
		}

			/* RF6_XX11 */
		case 160 + 26:
		{
			break;
		}

			/* RF6_SCARE */
		case 160 + 27:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles, and you hear scary noises.",
					m_name);
			else
				msg_format("%^s casts a fearful illusion.", m_name);
			if (p_ptr->resist_fear)
			{
				msg_print("You refuse to be frightened.");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You refuse to be frightened.");
			}
			else
			{
				(void) set_afraid(p_ptr->afraid + rand_int(3) + 3);
			}
			break;
		}

			/* RF6_BLIND */
		case 160 + 28:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a spell, burning your eyes!",
					m_name);
			if (p_ptr->resist_blind)
			{
				msg_print("You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				(void) set_blind(12 + rand_int(4));
			}
			break;
		}

			/* RF6_CONF */
		case 160 + 29:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles, and you hear puzzling noises.",
					m_name);
			else
				msg_format("%^s creates a mesmerising illusion.", m_name);
			if (p_ptr->resist_confu)
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else
			{
				(void) set_confused(p_ptr->confused + rand_int(4) + 4);
			}
			break;
		}

			/* RF6_SLOW */
		case 160 + 30:
		{
			disturb(1, 0);
			msg_format("%^s drains power from your muscles!", m_name);
			if (p_ptr->free_act)
			{
				msg_print("You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				(void) set_slow(p_ptr->slow + rand_int(6) + 6);
			}
			break;
		}

			/* RF6_HOLD */
		case 160 + 31:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s stares deep into your eyes!", m_name);
			if (p_ptr->free_act)
			{
				msg_print("You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_format("You resist the effects!");
			}
			else
			{
				(void) set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
			}
			break;
		}


			/* RF7_S_KIN */
		case 192 + 0:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons %s %s.", m_name, m_poss,
					((r_ptr->flags1) & RF1_UNIQUE ? "minions" : "kin"));

			/* Hack -- Set the letter of the monsters to summon */
			summon_kin_type = r_ptr->d_char;
			for (k = 0; k < (r_ptr->level > 40 ? 4 : 3); k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_KIN);
			}

			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

			/* RF7_XXX */
		case 192 + 1:
			break;
		case 192 + 2:
			break;


			/* RF7_S_MONSTER */
		case 192 + 3:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons help!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, 0);
			}
			if (blind && count)
				msg_print("You hear something appear nearby.");
			break;
		}

			/* RF7_S_MONSTERS */
		case 192 + 4:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons monsters!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, 0);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF7_XXX */
		case 192 + 5:
			break;
		case 192 + 6:
			break;
		case 192 + 7:
			break;

			/* RF7_S_ANT */
		case 192 + 8:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons ants.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_ANT);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF7_S_SPIDER */
		case 192 + 9:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons spiders.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_SPIDER);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF7_S_HOUND */
		case 192 + 10:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons hounds.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_HOUND);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF7_S_ANIMAL */
		case 192 + 11:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons natural creatures.",
					m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_ANIMAL);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF7_XXX */
		case 192 + 12:
			break;
		case 192 + 13:
			break;

			/* RF7_S_THIEF */
		case 192 + 14:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s whistles.", m_name);
			else
				msg_format("%^s whistles up a den of thieves!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_THIEF);
			}
			if (blind && count)
				msg_print("You hear many sneaky things appear nearby.");
			break;
		}

			/* Summon Bert, Bill, and Tom */
			/* No messages unless sucessful */
			/* RF7_S_BERTBILLTOM */
		case 192 + 15:
		{
			for (k = 0; k < 2; k++)
			{
				count +=
					summon_specific(y, x, FALSE, rlev, SUMMON_BERTBILLTOM);
			}
			if (blind && count)
				msg_print("You hear heavy footsteps approaching.");
			else if (count)
				msg_format("%^s calls up his friends!", m_name);
			break;
		}

			/* RF7_XXX */
		case 192 + 16:
			break;
		case 192 + 17:
			break;
		case 192 + 18:
			break;
		case 192 + 19:
			break;

			/* RF7_S_DRAGON */
		case 192 + 20:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons a dragon!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_DRAGON);
			}
			if (blind && count)
				msg_print("You hear something appear nearby.");
			break;
		}

			/* RF7_S_HI_DRAGON */
		case 192 + 21:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons ancient dragons!",
					m_name);
			for (k = 0; k < 4; k++)
			{
				count +=
					summon_specific(y, x, FALSE, rlev, SUMMON_HI_DRAGON);
			}
			if (blind && count)
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}

			/* RF7_XXX */
		case 192 + 22:
			break;
		case 192 + 23:
			break;

			/* RF7_S_DEMON */
		case 192 + 24:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			{
				if (!(blind))
					msg_format
						("%^s magically summons a hellish adversary!",
						m_name);
				for (k = 0; k < 1; k++)
				{
					count +=
						summon_specific(y, x, FALSE, rlev, SUMMON_DEMON);
				}
				if (blind && count)
					msg_print("You hear something appear nearby.");
			}
			break;
		}

			/* RF7_S_HI_DEMON */
		case 192 + 25:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons greater demons!",
					m_name);
			for (k = 0; k < 6; k++)
			{
				count +=
					summon_specific(y, x, FALSE, rlev, SUMMON_HI_DEMON);
			}
			if (blind && count)
			{
				msg_print("You hear many evil things appear nearby.");
			}
			break;
		}

			/* RF7_XXX */
		case 192 + 26:
			break;
		case 192 + 27:
			break;

			/* RF7_S_UNDEAD */
		case 192 + 28:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons an undead adversary!",
					m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_UNDEAD);
			}
			if (blind && count)
				msg_print("You hear something appear nearby.");
			break;
		}

			/* RF7_S_HI_UNDEAD */
		case 192 + 29:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons greater undead!",
					m_name);
			for (k = 0; k < 4; k++)
			{
				count +=
					summon_specific(y, x, FALSE, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

			/* Summon the Ringwraiths */
			/* RF7_S_WRAITH */
		case 192 + 30:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format
					("%^s magically summons mighty undead opponents!",
					m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_WRAITH);
			}
			for (k = 0; (k < 6) && (count < 6); k++)
			{
				count +=
					summon_specific(y, x, FALSE, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

			/* Summon Uniques */
			/* RF7_S_UNIQUE */
		case 192 + 31:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons special opponents!",
					m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_UNIQUE);
			}
			for (k = 0; (k < 6) && (count < 6); k++)
			{
				count +=
					summon_specific(y, x, FALSE, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}

			/* Paranoia */
		default:
		{
			msg_print
				("A monster tried to cast a spell that has not yet been defined.");
		}
	}

	/* Learn Player Resists */
	if (thrown_spell < 128)
	{
		update_smart_learn(m_idx,
			spell_desire_RF4[thrown_spell - 96][D_RES]);
	}
	else if (thrown_spell < 160)
	{
		update_smart_learn(m_idx,
			spell_desire_RF5[thrown_spell - 128][D_RES]);
	}
	else if (thrown_spell < 192)
	{
		update_smart_learn(m_idx,
			spell_desire_RF6[thrown_spell - 160][D_RES]);
	}
	else if (thrown_spell < 224)
	{
		update_smart_learn(m_idx,
			spell_desire_RF7[thrown_spell - 192][D_RES]);
	}

	/* Mark minimum desired range for recalculation */
	m_ptr->min_range = 0;

	/* Remember what the monster did to us */
	if (seen)
	{
		/* Innate spell */
		if (thrown_spell < 32 * 4)
		{
			l_ptr->flags4 |= (1L << (thrown_spell - 32 * 3));
			if (l_ptr->cast_inate < MAX_UCHAR)
				l_ptr->cast_inate++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32 * 5)
		{
			l_ptr->flags5 |= (1L << (thrown_spell - 32 * 4));
			if (l_ptr->cast_spell < MAX_UCHAR)
				l_ptr->cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32 * 6)
		{
			l_ptr->flags6 |= (1L << (thrown_spell - 32 * 5));
			if (l_ptr->cast_spell < MAX_UCHAR)
				l_ptr->cast_spell++;
		}

		/* Summon spell */
		else if (thrown_spell < 32 * 7)
		{
			l_ptr->flags7 |= (1L << (thrown_spell - 32 * 6));
			if (l_ptr->cast_spell < MAX_UCHAR)
				l_ptr->cast_spell++;
		}

		/* Remember special flags */
		if (r_ptr->flags2 & (RF2_ARCHER))
			l_ptr->flags2 |= RF2_ARCHER;
		if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
			l_ptr->flags2 |= RF2_MORGUL_MAGIC;
		if (r_ptr->flags2 & (RF2_UDUN_MAGIC))
			l_ptr->flags2 |= RF2_UDUN_MAGIC;

	}

	if (seen && p_ptr->wizard)
		msg_format("%^s has %i mana remaining.", m_name, m_ptr->mana);

	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
	{
		l_ptr->deaths++;
	}

	/* Handle aborted/failed spells -- ALLOW_SOUND */
	sound_cancel();

	/* A spell was cast */
	return (TRUE);
}

#ifdef MONSTER_FLOW

/*
 * Choose the "best" direction for "flowing"
 *
 * Note that ghosts and rock-eaters are never allowed to "flow",
 * since they should move directly towards the player.
 *
 * Prefer "non-diagonal" directions, but twiddle them a little
 * to angle slightly towards the player's actual location.
 *
 * Allow very perceptive monsters to track old "spoor" left by
 * previous locations occupied by the player.  This will tend
 * to have monsters end up either near the player or on a grid
 * recently occupied by the player (and left via "teleport").
 *
 * Note that if "smell" is turned on, all monsters get vicious.
 *
 * Also note that teleporting away from a location will cause
 * the monsters who were chasing you to converge on that location
 * as long as you are still near enough to "annoy" them without
 * being close enough to chase directly.  I have no idea what will
 * happen if you combine "smell" with low "aaf" values.
 */
static bool get_moves_aux(int m_idx, int *yp, int *xp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, y1, x1;

	int when = 0;
	int cost = 999;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster can go through rocks */
	if (r_ptr->flags2 & (RF2_PASS_WALL))
		return (FALSE);
	if (r_ptr->flags2 & (RF2_KILL_WALL))
		return (FALSE);

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* The player has never been near the monster grid */
	if (cave_when[y1][x1] == 0)
		return (FALSE);

	/* Monster is too far away to notice the player. */
	if (cave_cost[y1][x1] > MONSTER_FLOW_DEPTH)
		return (FALSE);
	if (cave_cost[y1][x1] > r_ptr->aaf)
		return (FALSE);

	/* Hack -- Player can see us, run towards him */
	if (player_has_los_bold(y1, x1))
		return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		/* Get the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Ignore illegal locations */
		if (cave_when[y][x] == 0)
			continue;

		/* Ignore ancient locations */
		if (cave_when[y][x] < when)
			continue;

		/* Ignore distant locations */
		if (cave_cost[y][x] > cost)
			continue;

		/* Save the cost and time */
		when = cave_when[y][x];
		cost = cave_cost[y][x];

		/* Hack -- Save the "twiddled" location */
		(*yp) = py + 16 * ddy_ddd[i];
		(*xp) = px + 16 * ddx_ddd[i];
	}

	/* No legal move (?) */
	if (!when)
		return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Provide a location to flee to, but give the player a wide berth.
 *
 * A monster may wish to flee to a location that is behind the player,
 * but instead of heading directly for it, the monster should "swerve"
 * around the player so that he has a smaller chance of getting hit.
 */
static bool get_fear_moves_aux(int m_idx, int *yp, int *xp)
{
	int y, x, y1, x1, fy, fx, py, px, gy = 0, gx = 0;
	int when = 0, score = -1;
	int i;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Player location */
	py = p_ptr->py;
	px = p_ptr->px;

	/* Monster location */
	fy = m_ptr->fy;
	fx = m_ptr->fx;

	/* Desired destination */
	y1 = fy - (*yp);
	x1 = fx - (*xp);

	/* The player is not currently near the monster grid */
	if (cave_when[fy][fx] < cave_when[py][px])
	{
		/* No reason to attempt flowing */
		return (FALSE);
	}

	/* Monster is too far away to use flow information.  Sighting impaired
	 * * on themed levels.
	 */
	if (cave_cost[fy][fx] > MONSTER_FLOW_DEPTH)
		return (FALSE);
	if (cave_cost[fy][fx] > r_ptr->aaf)
		return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		int dis, s;

		/* Get the location */
		y = fy + ddy_ddd[i];
		x = fx + ddx_ddd[i];

		/* Ignore illegal locations */
		if (cave_when[y][x] == 0)
			continue;

		/* Ignore ancient locations */
		if (cave_when[y][x] < when)
			continue;

		/* Calculate distance of this grid from our destination */
		dis = distance(y, x, y1, x1);

		/* Score this grid */
		s = 5000 / (dis + 3) - 500 / (cave_cost[y][x] + 1);

		/* No negative scores */
		if (s < 0)
			s = 0;

		/* Ignore lower scores */
		if (s < score)
			continue;

		/* Save the score and time */
		when = cave_when[y][x];
		score = s;

		/* Save the location */
		gy = y;
		gx = x;
	}

	/* No legal move (?) */
	if (!when)
		return (FALSE);

	/* Find deltas */
	(*yp) = fy - gy;
	(*xp) = fx - gx;

	/* Success */
	return (TRUE);
}


#endif



#ifdef MONSTER_AI

/*
 * Hack -- Precompute a bunch of calls to distance() in find_safety() and
 * find_hiding().
 *
 * The pair of arrays dist_offsets_y[n] and dist_offsets_x[n] contain the
 * offsets of all the locations with a distance of n from a central point,
 * with an offset of (0,0) indicating no more offsets at this distance.
 *
 * This is, of course, fairly unreadable, but it eliminates multiple loops
 * from the previous version.
 *
 * It is probably better to replace these arrays with code to compute
 * the relevant arrays, even if the storage is pre-allocated in hard
 * coded sizes.   At the very least, code should be included which is
 * able to generate and dump these arrays (ala "los()").  XXX XXX XXX
 *
 * Also, the storage needs could be halved by using bytes.  XXX XXX XXX
 *
 * These arrays could be combined into two big arrays, using sub-arrays
 * to hold the offsets and lengths of each portion of the sub-arrays, and
 * this could perhaps also be used somehow in the "look" code.  XXX XXX XXX
 */


static sint d_off_y_0[] = { 0 };

static sint d_off_x_0[] = { 0 };


static sint d_off_y_1[] = { -1, -1, -1, 0, 0, 1, 1, 1, 0 };

static sint d_off_x_1[] = { -1, 0, 1, -1, 1, -1, 0, 1, 0 };


static sint d_off_y_2[] = { -1, -1, -2, -2, -2, 0, 0, 1, 1, 2, 2, 2, 0 };

static sint d_off_x_2[] = { -2, 2, -1, 0, 1, -2, 2, -2, 2, -1, 0, 1, 0 };


static sint d_off_y_3[] = { -1, -1, -2, -2, -3, -3, -3, 0, 0, 1, 1, 2, 2,
	3, 3, 3, 0
};

static sint d_off_x_3[] = { -3, 3, -2, 2, -1, 0, 1, -3, 3, -3, 3, -2, 2,
	-1, 0, 1, 0
};


static sint d_off_y_4[] = { -1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4, 0,
	0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 0
};

static sint d_off_x_4[] = { -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, -4, 4,
	-4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, 0
};


static sint d_off_y_5[] = { -1, -1, -2, -2, -3, -3, -4, -4, -4, -4, -5, -5,
	-5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5,
	5, 0
};

static sint d_off_x_5[] = { -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1, 0, 1,
	-5, 5, -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1,
	0, 1, 0
};


static sint d_off_y_6[] = { -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
	-6, -6, -6, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
	5, 5, 6, 6, 6, 0
};

static sint d_off_x_6[] = { -6, 6, -5, 5, -5, 5, -4, 4, -2, -3, 2, 3, -1,
	0, 1, -6, 6, -6, 6, -5, 5, -5, 5, -4, 4, -2,
	-3, 2, 3, -1, 0, 1, 0
};


static sint d_off_y_7[] = { -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
	-6, -6, -6, -6, -7, -7, -7, 0, 0, 1, 1, 2, 2, 3,
	3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 0
};

static sint d_off_x_7[] = { -7, 7, -6, 6, -6, 6, -5, 5, -4, -5, 4, 5, -2,
	-3, 2, 3, -1, 0, 1, -7, 7, -7, 7, -6, 6, -6,
	6, -5, 5, -4, -5, 4, 5, -2, -3, 2, 3, -1, 0,
	1, 0
};


static sint d_off_y_8[] = { -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
	-6, -6, -7, -7, -7, -7, -8, -8, -8, 0, 0, 1, 1,
	2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
	8, 8, 8, 0
};

static sint d_off_x_8[] = { -8, 8, -7, 7, -7, 7, -6, 6, -6, 6, -4, -5, 4,
	5, -2, -3, 2, 3, -1, 0, 1, -8, 8, -8, 8, -7,
	7, -7, 7, -6, 6, -6, 6, -4, -5, 4, 5, -2, -3,
	2, 3, -1, 0, 1, 0
};


static sint d_off_y_9[] = { -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
	-7, -7, -7, -7, -8, -8, -8, -8, -9, -9, -9, 0,
	0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7,
	7, 8, 8, 8, 8, 9, 9, 9, 0
};

static sint d_off_x_9[] = { -9, 9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4,
	-5, 4, 5, -2, -3, 2, 3, -1, 0, 1, -9, 9, -9,
	9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4, -5,
	4, 5, -2, -3, 2, 3, -1, 0, 1, 0
};


static sint *dist_offsets_y[10] = {
	d_off_y_0, d_off_y_1, d_off_y_2, d_off_y_3, d_off_y_4,
	d_off_y_5, d_off_y_6, d_off_y_7, d_off_y_8, d_off_y_9
};

static sint *dist_offsets_x[10] = {
	d_off_x_0, d_off_x_1, d_off_x_2, d_off_x_3, d_off_x_4,
	d_off_x_5, d_off_x_6, d_off_x_7, d_off_x_8, d_off_x_9
};

#endif /* MONSTER_AI */


/*
 * Choose a "safe" location near a monster for it to run toward.
 *
 * A location is "safe" if it can be reached quickly and the player
 * is not able to fire into it (it isn't a "clean shot").  So, this will
 * cause monsters to "duck" behind walls.  Hopefully, monsters will also
 * try to run towards corridor openings if they are in a room.
 *
 * This function may take lots of CPU time if lots of monsters are
 * fleeing.
 *
 * Return TRUE if a safe location is available.
 */
static bool find_safety(int m_idx, int *yp, int *xp)
{
#ifdef MONSTER_AI

	monster_type *m_ptr = &m_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, dy, dx, d, dis;
	int gy = 0, gx = 0, gdis = 0;

	sint *y_offsets;
	sint *x_offsets;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Get the lists of points with a distance d from (fx, fy) */
		y_offsets = dist_offsets_y[d];
		x_offsets = dist_offsets_x[d];

		/* Check the locations */
		for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
			dx != 0 || dy != 0; i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds_fully(y, x))
				continue;

			/* Skip locations in a wall */
			if (!cave_passable_bold(y, x))
				continue;

			/* Check distance */
			if (distance(y, x, fy, fx) != d)
				continue;

			/* Check for "availability" */
			/* Ignore grids very far from the player */
			if (cave_when[y][x] < cave_when[py][px])
				continue;

			/* Ignore too-distant grids */
			if (cave_cost[y][x] > cave_cost[fy][fx] + 2 * d)
				continue;

			/* Check for absence of shot (more or less) */
			if (!player_can_see_bold(y, x))
			{
				/* Calculate distance from player */
				dis = distance(y, x, py, px);

				/* Remember if further than previous */
				if (dis > gdis)
				{
					gy = y;
					gx = x;
					gdis = dis;
				}
			}
		}

		/* Check for success */
		if (gdis > 0)
		{
			/* Good location */
			(*yp) = fy - gy;
			(*xp) = fx - gx;

			/* Found safe place */
			return (TRUE);
		}
	}

#endif /* MONSTER_AI */

	/* No safe place */
	return (FALSE);
}


/*
 * Choose a good hiding place near a monster for it to run toward.
 *
 * Pack monsters will use this to "ambush" the player and lure him out
 * of corridors into open space so they can swarm him.
 *
 * Return TRUE if a good location is available.
 */
static bool find_hiding(int m_idx, int *yp, int *xp)
{
	monster_type *m_ptr = &m_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, dy, dx, d, dis;
	int gy = 0, gx = 0, gdis = 999, min;

	sint *y_offsets, *x_offsets;

	/* Closest distance to get */
	min = distance(py, px, fy, fx) * 3 / 4 + 2;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Get the lists of points with a distance d from (fx, fy) */
		y_offsets = dist_offsets_y[d];
		x_offsets = dist_offsets_x[d];

		/* Check the locations */
		for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
			dx != 0 || dy != 0; i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds_fully(y, x))
				continue;

			/* Skip occupied locations */
			if (!cave_empty_bold(y, x))
				continue;

			/* Check distance */
			if (distance(y, x, fy, fx) != d)
				continue;

			/* Check for hidden, available grid */
			if (!player_can_see_bold(y, x) &&
				(projectable(fy, fx, y, x, PROJECT_CHCK)))
			{
				/* Calculate distance from player */
				dis = distance(y, x, py, px);

				/* Remember if closer than previous */
				if (dis < gdis && dis >= min)
				{
					gy = y;
					gx = x;
					gdis = dis;
				}
			}
		}

		/* Check for success */
		if (gdis < 999)
		{
			/* Good location */
			(*yp) = fy - gy;
			(*xp) = fx - gx;

			/* Found good place */
			return (TRUE);
		}
	}

	/* No good place */
	return (FALSE);
}



/*
 * Choose "logical" directions for monster movement
 *
 * We store the directions in a special "mm" array
 */
static bool get_moves(int m_idx, int mm[5])
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int y, ay, x, ax;

	int move_val = 0;

	int y2 = py;
	int x2 = px;
	byte min_range = m_ptr->min_range;

	bool done = FALSE;
	bool fear = FALSE;
	bool too_close = FALSE;
	bool too_far = FALSE;

	/* Is the monster scared */
	if ((min_range == MAX_SIGHT + 5) || (m_ptr->monfear))
		fear = TRUE;

	/* Adjust minimum raneg for themed level sight differences */
	if (p_ptr->themed_level && (m_ptr->hp == m_ptr->maxhp) &&
		(min_range > (MAX_SIGHT / 2) + 5))
		min_range = (MAX_SIGHT / 2) + 5;

	/* Are we too close or too far? */
	/* Strictly, we should probably set too_far to TRUE if out of line
	 * of sight.  Instead we handle that later (only if it matters) to save CPU.
	 */
	if ((m_ptr->cdis < min_range) && (!(r_ptr->flags1 & (RF1_NEVER_MOVE))))
		too_close = TRUE;
	if (m_ptr->cdis > m_ptr->best_range)
		too_far = TRUE;

#ifdef MONSTER_FLOW
	/* Flow towards the player */
	(void) get_moves_aux(m_idx, &y2, &x2);
#endif

	/* Extract the "pseudo-direction" */
	y = m_ptr->fy - y2;
	x = m_ptr->fx - x2;

#ifdef MONSTER_AI

	/* Normal animal packs try to lure the player into the open. */
	if ((r_ptr->flags1 & RF1_FRIENDS) && (r_ptr->flags3 & RF3_ANIMAL) &&
		!too_close && !((r_ptr->flags2 & (RF2_PASS_WALL)) ||
			(r_ptr->flags2 & (RF2_KILL_WALL))))
	{
		int i, passable = 0;

		/* Count passable grids next to player */
		for (i = 0; i < 8; i++)
		{
			/* Check grid */
			if (cave_passable_bold(py + ddy_ddd[i], px + ddx_ddd[i]))
			{
				/* One more passable grid */
				passable++;
			}
		}

		/* Not in the open and strong player. */
		if ((passable < 5) && (p_ptr->chp > p_ptr->mhp / 2) &&
			(p_ptr->csp + 1 > (p_ptr->msp + 3) / 4))
		{
			/* Find hiding place */
			if (find_hiding(m_idx, &y, &x))
				done = TRUE;
		}
	}

	/* Monster groups often try to surround the player, unless frightened. */
	if ((r_ptr->flags1 & RF1_FRIENDS) && !done && !too_close &&
		(randint(2) == 1))
	{
		int i;

		/* Find an empty square near the player to fill */
		for (i = 0; i < 8; i++)
		{
			/* Pick squares near player (semi-randomly) */
			y2 = py + ddy_ddd[(m_idx + i) & 7];
			x2 = px + ddx_ddd[(m_idx + i) & 7];

			/* Already there? */
			if ((m_ptr->fy == y2) && (m_ptr->fx == x2))
			{
				/* Attack the player */
				y2 = py;
				x2 = px;

				break;
			}

			/* Ignore filled grids */
			if (!cave_passable_bold(y2, x2))
				continue;

			/* Try to fill this hole */
			break;
		}
#endif

		/* Extract the new "pseudo-direction" */
		y = m_ptr->fy - y2;
		x = m_ptr->fx - x2;

		/* Done */
		done = TRUE;
	}

	/* Apply fear */
	if (!done && fear)
	{
		/* Try to find safe place */
		if (!find_safety(m_idx, &y, &x))
		{
			/* If no safe place, just run away. */
			y = (-y);
			x = (-x);

			done = TRUE;
		}

#ifdef MONSTER_AI

		else
		{
			/* Adjust movement */
			if (get_fear_moves_aux(m_idx, &y, &x))
				done = TRUE;
		}
#endif
	}

	/* Now choose final direction */
	if (!done)
	{

		/* Want to back off, but not actually scared. */
		if (too_close)
		{
			y = (-y);
			x = (-x);
		}

		/* Sometime, we may decide close enough is close enough */
		else if ((!too_far) && (m_ptr->cdis > 1) && (rand_int(2) == 0))
		{

			/* Don't actually stop unless you have a shot */
			if (projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px,
					PROJECT_CHCK) == PROJECT_NO)
			{
				y = 0;
				x = 0;
			}
		}
	}

	/* Check for no move */
	if (!x && !y)
		return (FALSE);


	/* Extract the "absolute distances" */
	ax = ABS(x);
	ay = ABS(y);

	/* Do something weird */
	if (y < 0)
		move_val += 8;
	if (x > 0)
		move_val += 4;

	/* Prevent the diamond maneuvre */
	if (ay > (ax << 1))
	{
		move_val++;
		move_val++;
	}
	else if (ax > (ay << 1))
	{
		move_val++;
	}

	/* Analyze */
	switch (move_val)
	{
		case 0:
		{
			mm[0] = 9;
			if (ay > ax)
			{
				mm[1] = 8;
				mm[2] = 6;
				mm[3] = 7;
				mm[4] = 3;
			}
			else
			{
				mm[1] = 6;
				mm[2] = 8;
				mm[3] = 3;
				mm[4] = 7;
			}
			break;
		}

		case 1:
		case 9:
		{
			mm[0] = 6;
			if (y < 0)
			{
				mm[1] = 3;
				mm[2] = 9;
				mm[3] = 2;
				mm[4] = 8;
			}
			else
			{
				mm[1] = 9;
				mm[2] = 3;
				mm[3] = 8;
				mm[4] = 2;
			}
			break;
		}

		case 2:
		case 6:
		{
			mm[0] = 8;
			if (x < 0)
			{
				mm[1] = 9;
				mm[2] = 7;
				mm[3] = 6;
				mm[4] = 4;
			}
			else
			{
				mm[1] = 7;
				mm[2] = 9;
				mm[3] = 4;
				mm[4] = 6;
			}
			break;
		}

		case 4:
		{
			mm[0] = 7;
			if (ay > ax)
			{
				mm[1] = 8;
				mm[2] = 4;
				mm[3] = 9;
				mm[4] = 1;
			}
			else
			{
				mm[1] = 4;
				mm[2] = 8;
				mm[3] = 1;
				mm[4] = 9;
			}
			break;
		}

		case 5:
		case 13:
		{
			mm[0] = 4;
			if (y < 0)
			{
				mm[1] = 1;
				mm[2] = 7;
				mm[3] = 2;
				mm[4] = 8;
			}
			else
			{
				mm[1] = 7;
				mm[2] = 1;
				mm[3] = 8;
				mm[4] = 2;
			}
			break;
		}

		case 8:
		{
			mm[0] = 3;
			if (ay > ax)
			{
				mm[1] = 2;
				mm[2] = 6;
				mm[3] = 1;
				mm[4] = 9;
			}
			else
			{
				mm[1] = 6;
				mm[2] = 2;
				mm[3] = 9;
				mm[4] = 1;
			}
			break;
		}

		case 10:
		case 14:
		{
			mm[0] = 2;
			if (x < 0)
			{
				mm[1] = 3;
				mm[2] = 1;
				mm[3] = 6;
				mm[4] = 4;
			}
			else
			{
				mm[1] = 1;
				mm[2] = 3;
				mm[3] = 4;
				mm[4] = 6;
			}
			break;
		}

		default: /* case 12: */
		{
			mm[0] = 1;
			if (ay > ax)
			{
				mm[1] = 2;
				mm[2] = 4;
				mm[3] = 3;
				mm[4] = 7;
			}
			else
			{
				mm[1] = 4;
				mm[2] = 2;
				mm[3] = 7;
				mm[4] = 3;
			}
			break;
		}
	}

	/* Want to move */
	return (TRUE);
}



/*
 * Hack -- compare the "strength" of two monsters XXX XXX XXX
 */
static int compare_monsters(monster_type * m_ptr, monster_type * n_ptr)
{
	monster_race *r_ptr;

	u32b mexp1, mexp2;

	/* Race 1 */
	r_ptr = &r_info[m_ptr->r_idx];

	/* Extract mexp */
	mexp1 = r_ptr->mexp;

	/* Race 2 */
	r_ptr = &r_info[n_ptr->r_idx];

	/* Extract mexp */
	mexp2 = r_ptr->mexp;

	/* Compare */
	if (mexp1 < mexp2)
		return (-1);
	if (mexp1 > mexp2)
		return (1);

	/* Assume equal */
	return (0);
}


static void find_range(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u16b p_lev, m_lev;
	u16b p_chp, p_mhp;
	u16b m_chp, m_mhp;
	u32b p_val, m_val;

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear)
		m_ptr->min_range = MAX_SIGHT + 5;

	/* Some monsters run when low on mana */
	else if ((r_ptr->flags2 & (RF2_LOW_MANA_RUN)) &&
		(m_ptr->mana < r_ptr->mana / 6))
		m_ptr->min_range = MAX_SIGHT + 5;

	else
	{

		/* Minimum distance - stay at least this far if possible */
		m_ptr->min_range = 1;

		/* Examine player power (level) */
		p_lev = p_ptr->lev;

		/* Examine monster power (level plus morale) */
		m_lev = r_ptr->level + (m_idx & 0x08) + 25;

		/* Optimize extreme cases below */
		if (m_lev < p_lev + 4)
			m_ptr->min_range = MAX_SIGHT + 5;
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
				m_ptr->min_range = MAX_SIGHT + 5;
		}
	}

	if (m_ptr->min_range < MAX_SIGHT + 5)
	{
		/* Creatures that don't move never like to get too close */
		if (r_ptr->flags1 & (RF1_NEVER_MOVE))
			m_ptr->min_range += 3;

		/* Spellcasters that don't stike never like to get too close */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW))
			m_ptr->min_range += 3;
	}

	/* Maximum range to flee to (reduced elsewhere for themed levels */
	if (!(m_ptr->min_range < MAX_SIGHT + 5))
		m_ptr->min_range = MAX_SIGHT + 5;

	/* Nearby monsters that cannot run away will not become run unless
	 * completely afraid */
	else if ((m_ptr->cdis < 4) && (m_ptr->mspeed < p_ptr->pspeed))
		m_ptr->min_range = 1;

	/* Now find prefered range */
	m_ptr->best_range = m_ptr->min_range;

	/* Archers are quite happy at a good distance */
	if (r_ptr->flags2 & (RF2_ARCHER))
		m_ptr->best_range += 3;

	if (((r_ptr->freq_inate + r_ptr->freq_spell) / 2) > 15)
	{
		/* Heavy spell casters will sit back and cast */
		if (m_ptr->mana > r_ptr->mana / 4)
			m_ptr->best_range += 3;

		/* Breathers like point blank range */
		if (((r_ptr->flags4 & (RF4_BREATH_MASK)) ||
				(r_ptr->flags5 & (RF5_BREATH_MASK)) ||
				(r_ptr->flags6 & (RF6_BREATH_MASK)) ||
				(r_ptr->flags7 & (RF7_BREATH_MASK))) &&
			(m_ptr->best_range < 6) && (m_ptr->hp > m_ptr->maxhp / 4))
			m_ptr->best_range = 6;
	}
}




/*
 * Process a monster
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level.  This should prevent the level from being "swamped" by
 * reproducing monsters.  It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it.  Actually,
 * the same thing happens to normal monsters when they hit a door
 *
 * In addition, monsters which *cannot* open or bash down a door
 * will still stand there trying to open it...
 * This has been partially fixed in Oangband; a monster will "stagger" 
 * around instead.
 *
 * Technically, need to check for monster in the way combined
 * with that monster being in a wall (or door?) XXX
 */
static void process_monster(int m_idx, int total_wakeup_chance)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int i, d, oy, ox, ny, nx;
	int chance = 0;

	int mm[5];

	/* Assume the monster is able to perceive the player. */
	bool aware = TRUE;

	/* Does a monster stagger around?  Shall it stagger around in order 
	 * to avoid a door or dungeon feature it can't handle? -LM-
	 */
	bool stagger;
	bool stagger_temp = FALSE;

	bool do_turn;
	bool do_move;
	bool do_view;

	bool did_open_door;
	bool did_bash_door;
	bool did_take_item;
	bool did_kill_item;
	bool did_move_body;
	bool did_kill_body;
	bool did_pass_wall;
	bool did_kill_wall;

	/* Find the monster's desired range if needed */
	if (!m_ptr->min_range)
		find_range(m_idx);

	/* HACK - occassionally recalculate range at random */
	else if (rand_int(20) == 0)
		find_range(m_idx);

	/* Players hidden in shadow are almost imperceptable. */
	if ((p_ptr->superstealth) && (!p_ptr->aggravate))
	{
		/* Low-level monsters will find it difficult to locate the player. */
		if ((p_ptr->lev * 2 > r_ptr->level) &&
			(rand_int(p_ptr->lev * 2 - r_ptr->level) != 0))
			aware = FALSE;
	}

	/* Monsters have a chance to recover from the Black Breath; maybe
	 * they have some Athelas handy... -LM-
	 */
	if ((m_ptr->black_breath) && (rand_int(250 - r_ptr->level) == 0))
	{
		m_ptr->black_breath = FALSE;
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Acquire the monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Dump a message */
			msg_format("%^s recovers from the Black Breath.", m_name);
		}
	}

	/* Handle "sleep" */
	if (m_ptr->csleep)
	{
		/* Aggravated by the player */
		if (p_ptr->aggravate)
		{
			/* Reset sleep counter */
#if 1 /* TNB */
			monster_disturb(m_idx);
#else
			m_ptr->csleep = 0;
#endif


			/* Notice the "waking up" */
#if 1 /* TNB */
			if ((m_ptr->ml && disturb_move) || (m_ptr->mflag & MFLAG_VIEW))
#else
			if (m_ptr->ml)
#endif
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx)
					p_ptr->redraw |= (PR_HEALTH);

				/* Dump a message */
				msg_format("%^s wakes up.", m_name);
			}

			/* Efficiency XXX XXX */
			return;
		}

		/* Get the origin */
		oy = m_ptr->fy;
		ox = m_ptr->fx;

		/* Chance for extra noise to wake up monsters in LOS. */
		if ((add_wakeup_chance > 0) && (player_has_los_bold(oy, ox)))
		{
			if (add_wakeup_chance > rand_int(10000))
			{
				/* Disturb the monster quite a lot. */
				if (m_ptr->csleep <= 50)
					m_ptr->csleep = 0;
				else
					m_ptr->csleep -= 50;

				/* Notice the monsters that wake up or are disturbed. */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);

					if (m_ptr->csleep)
						msg_format("%^s stirs.", m_name);
					else
					{
						/* Hack -- Update the health bar */
						if (p_ptr->health_who == m_idx)
							p_ptr->redraw |= (PR_HEALTH);


						msg_format("%^s wakes up.", m_name);
					}
				}
			}
		}


		/* Hack -- See if monster "notices" player.  Now a percentage chance. */
		if (total_wakeup_chance > rand_int(10000))
		{
			int d = 1;

			/* Wake up faster near the player */
			if (m_ptr->cdis < 50)
				d = (100 / (m_ptr->cdis + 1));

			/* Still asleep */
			if (m_ptr->csleep > d)
			{
				/* Monster wakes up "a little bit" */
				m_ptr->csleep -= d;

				/* Notice the "not waking up" */
				if (m_ptr->ml)
				{
					/* Hack -- Count the ignores */
					if (l_ptr->ignore < MAX_UCHAR)
					{
						l_ptr->ignore++;
					}
				}
			}

			/* Just woke up */
			else
			{
				/* Reset sleep counter */
#if 1 /* TNB */
				monster_disturb(m_idx);
#else
				m_ptr->csleep = 0;
#endif

				/* Notice the "waking up" */
#if 1 /* TNB */
			if ((m_ptr->ml && disturb_move) || (m_ptr->mflag & MFLAG_VIEW))
#else
			if (m_ptr->ml)
#endif	/* TNB */
				{
					char m_name[80];

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == m_idx)
						p_ptr->redraw |= (PR_HEALTH);

					/* Hack -- Count the wakings */
					if (l_ptr->wake < MAX_UCHAR)
					{
						l_ptr->wake++;
					}
				}
			}
		}

		/* Still sleeping */
		if (m_ptr->csleep)
			return;
	}


	/* Handle "stun" */
	if (m_ptr->stunned)
	{
		int d = 1;

		/* Make a "saving throw" against stun.  Changed in Oangband. */
		if (rand_int(330) < r_ptr->level + 10)
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
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer stunned.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx)
					p_ptr->redraw |= (PR_HEALTH);
			}
		}

		/* Stunned monsters can still move. */
	}


	/* Handle confusion */
	if (m_ptr->confused)
	{
		int d = randint(r_ptr->level / 10 + 1);

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
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer confused.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx)
					p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}


	/* Handle "fear" */
	if (m_ptr->monfear)
	{
		/* Amount of "boldness" */
		int d = randint(r_ptr->level / 10 + 1);

		/* Still afraid */
		if (m_ptr->monfear > d)
		{
			/* Reduce the fear */
			m_ptr->monfear -= d;
		}

		/* Recover from fear, take note if seen */
		else
		{
			/* No longer afraid */
			m_ptr->monfear = 0;

			/* Flag minimum range for recalculation */
			m_ptr->min_range = 0;

			/* Visual note */
			if (m_ptr->ml)
			{
				char m_name[80];
				char m_poss[80];

				/* Acquire the monster name/poss */
				monster_desc(m_name, m_ptr, 0);
				monster_desc(m_poss, m_ptr, 0x22);

				/* Dump a message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx)
					p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}

	/* Handle stasis.  Fast monsters get no bonus (unfortunately, slow 
	 * one are penalized). -LM-
	 */
	if ((m_ptr->stasis) && (rand_int(extract_energy[m_ptr->mspeed]) < 10))
	{
		int d = 1;

		/* Sudden emergence.  Uniques get a bonus. */
		if (50 + r_ptr->level +
			(r_ptr->flags1 & (RF1_UNIQUE) ? r_ptr->level / 2 : 0) >
			randint(800)) m_ptr->stasis = 0;

		/* Gradual emergence. */
		else if (m_ptr->stasis > d)
			m_ptr->stasis -= d;
		else
			m_ptr->stasis = 0;

		/* Visual note */
		if ((!m_ptr->stasis) && (m_ptr->ml))
		{
			char m_name[80];
			char m_poss[80];

			/* Acquire the monster name/poss */
			monster_desc(m_name, m_ptr, 0);
			monster_desc(m_poss, m_ptr, 0x22);

			/* Dump a message */
			msg_format("%^s emerges from a Holding spell.", m_name);
		}
	}

	/* If still in stasis, do nothing. */
	if (m_ptr->stasis)
		return;


	/* Handle significantly hasted or slowed creatures.  Random variations 
	 * are not large enough to activate this code. -LM-
	 */
	if ((m_ptr->mspeed > r_ptr->speed + 4) ||
		(m_ptr->mspeed < r_ptr->speed - 4))
	{
		/* 1.5% chance that slowed monsters will return to normal speed. */
		if ((m_ptr->mspeed < r_ptr->speed) && (randint(67) == 1))
		{
			m_ptr->mspeed = r_ptr->speed;

			/* Visual note */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Message. */
				msg_format("%s is no longer slowed.", m_name);
			}
		}

		/* 1% chance that hasted monsters will return to normal speed. */
		else if ((m_ptr->mspeed > r_ptr->speed) && (randint(100) == 1))
		{
			m_ptr->mspeed = r_ptr->speed;

			/* Visual note */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Message. */
				msg_format("%s is no longer hasted.", m_name);
			}
		}
	}


	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx)
		p_ptr->redraw |= (PR_HEALTH);

	/* Get the origin */
	oy = m_ptr->fy;
	ox = m_ptr->fx;


	/* Attempt to "mutiply" if able and allowed */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) && (num_repro < MAX_REPRO))
	{
		int k, y, x;

		/* Count the adjacent monsters */
		for (k = 0, y = oy - 1; y <= oy + 1; y++)
		{
			for (x = ox - 1; x <= ox + 1; x++)
			{
				/* Count monsters */
				if (cave_m_idx[y][x] > 0)
					k++;
			}
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(m_idx))
			{
				/* Take note if visible */
				if (m_ptr->ml)
				{
					l_ptr->flags2 |= (RF2_MULTIPLY);
				}

				if (m_ptr->mflag & MFLAG_VIEW)
					sound(SNDGRP_EVENT, SND_MONSTER_MULTIPLY, 0x02); /* ALLOW_SOUND */

				/* Multiplying takes energy */
				return;
			}
		}
	}

	/* This code is taken from Zangband, with some simplifications. -LM- */
	if ((r_ptr->flags2 & (RF2_SPEAKING)) && (rand_int(SPEAK_CHANCE) == 0)
		&& (!m_ptr->monfear))
	{
		/* Certain monsters in LOS can speak. */
		if (player_has_los_bold(oy, ox))
		{
			char m_name[80];
			char bravado[80];

			/* Acquire the monster name/poss */
			if (m_ptr->ml)
				monster_desc(m_name, m_ptr, 0);

			/* Default name */
			else
				strcpy(m_name, "It");

			get_rnd_line("bravado.txt", bravado);
			msg_format("%^s %s", m_name, bravado);
		}
	}

	/* Player ghosts may have a unique message they can say.  They will only 
	 * say it once (unless reloaded). -LM-
	 */
	else if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) &&
		(player_has_los_bold(oy, ox)) && (ghost_string_type == 1) &&
		(ghost_has_spoken == FALSE) && (randint(3) == 1))
	{
		char m_name[80];

		/* Acquire the monster name/poss.  The player ghost will always 
		 * be identified, to heighten the effect.
		 */
		monster_desc(m_name, m_ptr, 0);

		msg_format("%^s says: '%s'", m_name, ghost_string);

		ghost_has_spoken = TRUE;
	}


	/* If the monster can perceive the player, attempt to cast a spell. */
	if (aware && make_attack_spell(m_idx))
		return;


	/* Reset */
	stagger = FALSE;

	/* Confused or cannot perceive the player */
	if (m_ptr->confused || (!aware))
	{
		/* Stagger */
		stagger = TRUE;
	}

	/* Random movement */
	else if (r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25))
	{
		/* Random movement (25%) */
		if (!(r_ptr->flags1 & (RF1_RAND_50)))
		{
			/* Random */
			if (rand_int(100) < 25)
			{
				/* Memorize flags */
				if (m_ptr->ml)
					l_ptr->flags1 |= (RF1_RAND_25);

				/* Stagger */
				stagger = TRUE;
			}
		}

		/* Random movement (50%) */
		else if (!(r_ptr->flags1 & (RF1_RAND_25)))
		{
			/* Random */
			if (rand_int(100) < 50)
			{
				/* Memorize flags */
				if (m_ptr->ml)
					l_ptr->flags1 |= (RF1_RAND_50);

				/* Stagger */
				stagger = TRUE;
			}
		}

		/* Random movement (75%) */
		else
		{
			/* Random */
			if (rand_int(100) < 75)
			{
				/* Memorize flags */
				if (m_ptr->ml)
					l_ptr->flags1 |= (RF1_RAND_50 | RF1_RAND_25);

				/* Stagger */
				stagger = TRUE;
			}
		}
	}

	/* Normal movement */
	if (!stagger)
	{
		/* Logical moves, may do nothing */
		if (!get_moves(m_idx, mm))
			return;
	}


	/* Assume nothing */
	do_turn = FALSE;
	do_move = FALSE;
	do_view = FALSE;

	/* Assume nothing */
	did_open_door = FALSE;
	did_bash_door = FALSE;
	did_take_item = FALSE;
	did_kill_item = FALSE;
	did_move_body = FALSE;
	did_kill_body = FALSE;
	did_pass_wall = FALSE;
	did_kill_wall = FALSE;


	/* Process all four possible moves */
	for (i = 0; i < 5; i++)
	{
		/* Try again, if monster cannot deal with a feature it has encountered. -LM- */
	  I_cannot_handle_feature:

		/* Get the direction (or stagger) */
		d = (stagger || stagger_temp ? ddd[rand_int(8)] : mm[i]);

		/* Clear a temporary stagger, and movement permission. */
		if (stagger_temp)
		{
			stagger_temp = FALSE;
			do_move = FALSE;
		}

		/* Get the destination */
		ny = oy + ddy[d];
		nx = ox + ddx[d];


		/* Floor is open? */
		if (cave_passable_bold(ny, nx))
		{
			/* Go ahead and (try to) move */
			do_move = TRUE;
		}

		/* Permanent wall */
		else if (cave_feat[ny][nx] >= FEAT_PERM_EXTRA)
		{
			/* Nothing */
		}

		/* Monster moves through walls (and doors) */
		else if (r_ptr->flags2 & (RF2_PASS_WALL))
		{
			/* Pass through walls/doors/rubble */
			do_move = TRUE;

			/* Monster went through a wall */
			did_pass_wall = TRUE;
		}

		/* Monster destroys walls (and doors) */
		else if (r_ptr->flags2 & (RF2_KILL_WALL))
		{
			bool do_gold = FALSE, do_rubble = FALSE; /* TNB */

			/* Eat through walls/doors/rubble */
			do_move = TRUE;

			/* Monster destroyed a wall */
			did_kill_wall = TRUE;

#if 1 /* TNB */

			/* */
			if (cave_feat[ny][nx] == FEAT_RUBBLE)
			{
				do_rubble = TRUE;
			}

			/* Monsters reveal treasure too! */
			if (cave_feat[ny][nx] >= FEAT_MAGMA_H &&
				cave_feat[ny][nx] <= FEAT_QUARTZ_K)
			{
				do_gold = TRUE;
			}

#endif /* TNB */

			/* Forget the wall */
			cave_info[ny][nx] &= ~(CAVE_MARK);

			/* Notice */
			cave_set_feat(ny, nx, FEAT_FLOOR);

#if 1 /* TNB */

			if (do_gold)
			{
				/* Place some gold */
				place_gold(ny, nx);
			}

			if (do_rubble && rand_int(100) < 10)
			{
				/* Place an object */
				place_object(ny, nx, FALSE, FALSE, FALSE);
			}

#endif /* TNB */

			/* Note changes to viewable region */
			if (player_has_los_bold(ny, nx))
				do_view = TRUE;
		}

		/* Handle doors and secret doors */
		else if (((cave_feat[ny][nx] >= FEAT_DOOR_HEAD) &&
				(cave_feat[ny][nx] <= FEAT_DOOR_TAIL)) ||
			(cave_feat[ny][nx] == FEAT_SECRET))
		{
			bool may_bash = TRUE;

			/* Take a turn */
			do_turn = TRUE;

			/* Creature can open doors. */
			if (r_ptr->flags2 & (RF2_OPEN_DOOR))
			{
				/* Closed doors and secret doors */
				if ((cave_feat[ny][nx] == FEAT_DOOR_HEAD) ||
					(cave_feat[ny][nx] == FEAT_SECRET))
				{
					/* The door is open */
					did_open_door = TRUE;

					/* Do not bash the door */
					may_bash = FALSE;

					/* Hack -- prevent a certain player trick, and 
					 * sometimes step into doorway.
					 */
					if (randint(5) == 1)
						do_move = TRUE;
				}

				/* Locked doors (not jammed) */
				else if (cave_feat[ny][nx] < FEAT_DOOR_HEAD + 0x08)
				{
					int k;

					/* Door power (from 0 to 8) */
					k = ((cave_feat[ny][nx] - FEAT_DOOR_HEAD) & 0x07);

					/* Try to unlock it XXX XXX XXX */
					/* Formula altered in Oangband. */
					chance = 20 + (2 * r_ptr->level / 5) - (k * 5);
					if (chance < 10 + r_ptr->level / 4)
						chance = 10 + r_ptr->level / 4;

					if (chance > rand_int(100))
					{
						/* Unlock the door */
						cave_set_feat(ny, nx, FEAT_DOOR_HEAD + 0x00);

						/* Do not bash the door */
						may_bash = FALSE;
					}
				}
			}

			/* Stuck doors -- attempt to bash them down if allowed */
			if (may_bash && (r_ptr->flags2 & (RF2_BASH_DOOR)))
			{
				int k;

				/* Door power (from 0 to 8) */
				k = ((cave_feat[ny][nx] - FEAT_DOOR_HEAD) & 0x07);

				/* Attempt to Bash XXX XXX XXX. */
				/* Formula altered in Oangband. */
				chance = (m_ptr->hp / 10) - (k * 15);
				if (chance < 2 + r_ptr->level / 10)
					chance = 2 + r_ptr->level / 10;

				if (chance > rand_int(100))
				{
					/* Message */
					sound(SNDGRP_EVENT, SND_BREAK_DOOR, 0);	/* ALLOW_SOUND */
					msg_print("You hear a door burst open!");

					/* Disturb (sometimes) */
					if (disturb_minor)
						disturb(0, 0);

					/* The door was bashed open */
					did_bash_door = TRUE;

					/* Hack -- fall into doorway */
					do_move = TRUE;
				}
			}

			/* A monster that cannot open or bash doors will look 
			 * around for other options.  To avoid endless loops 
			 * should it find itelf totally surrounded by doors, it 
			 * won't do this always. -LM-
			 */
			else if ((randint(25) != 1) &&
				!(r_ptr->flags2 & (RF2_OPEN_DOOR)) &&
				!(r_ptr->flags2 & (RF2_BASH_DOOR)))
			{
				stagger_temp = TRUE;
				goto I_cannot_handle_feature;
			}


			/* Deal with doors in the way */
			if (did_open_door || did_bash_door)
			{
				/* Break down the door */
				if (did_bash_door && (rand_int(100) < 50))
				{
					cave_set_feat(ny, nx, FEAT_BROKEN);
				}

				/* Open the door */
				else
				{
					cave_set_feat(ny, nx, FEAT_OPEN);
				}

				/* Handle viewable doors */
				if (player_has_los_bold(ny, nx))
					do_view = TRUE;
			}
		}


		/* Hack -- check for Glyph of Warding */
		if (do_move && (cave_feat[ny][nx] == FEAT_GLYPH))
		{
			/* Assume no move allowed */
			do_move = FALSE;

			/* Break the ward */
			if (randint(BREAK_GLYPH) < r_ptr->level)
			{
				/* Describe observable breakage */
				if (cave_info[ny][nx] & (CAVE_MARK))
				{
					sound(SNDGRP_EVENT, SND_OBJECT_DESTROYED, 0); /* ALLOW_SOUND */
					msg_print("The rune of protection is broken!");
				}

				/* Forget the rune */
				cave_info[ny][nx] &= ~(CAVE_MARK);

				/* Break the rune */
				cave_set_feat(ny, nx, FEAT_FLOOR);

				/* One less glyph on the level */
				num_glyph_on_level--;

				/* Allow movement */
				do_move = TRUE;
			}
		}

		/* Some monsters never attack */
		if (do_move && (cave_m_idx[ny][nx] < 0) &&
			(r_ptr->flags1 & (RF1_NEVER_BLOW)))
		{
			/* Hack -- memorize lack of attacks after a while */
			if ((m_ptr->ml) && (randint(20) == 1))
				l_ptr->flags1 |= (RF1_NEVER_BLOW);

			/* Do not move */
			do_move = FALSE;
		}


		/* The player is in the way.  Attack him. */
		if (do_move && (cave_m_idx[ny][nx] < 0))
		{
			/* Do the attack */
			(void) make_attack_normal(m_idx, ny, nx);

			/* Do not move */
			do_move = FALSE;

			/* Took a turn */
			do_turn = TRUE;
		}


		/* Some monsters never move */
		if (do_move && (r_ptr->flags1 & (RF1_NEVER_MOVE)))
		{
			/* Hack -- memorize lack of attacks after a while. */
			if ((m_ptr->ml) && (randint(20) == 1))
				l_ptr->flags1 |= (RF1_NEVER_MOVE);

			/* Do not move */
			do_move = FALSE;
		}

		/* Handle terrain that can't be crossed by all monsters, but that 
		 * can be attacked over. -LM-
		 */
		if (do_move)
		{
			switch (cave_feat[ny][nx])
			{

					/* Rubble is a nuisance to cross, except for creatures 
					 * that either bore or pass through walls.  Smart mon-
					 * sters are stopped 50% of the time, and other monsters 
					 * are stopped 67% of the time.
					 */
				case FEAT_RUBBLE:
				{
					if ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
						(r_ptr->flags2 & (RF2_KILL_WALL)))
						break;

					if (r_ptr->flags2 & (RF2_SMART))
					{
						if (randint(2) == 1)
							do_move = FALSE;
					}
					else
					{
						if (randint(3) != 1)
							do_move = FALSE;
					}

					break;
				}
					/* Trees aren't easy either, but they can be passed 
					 * through, flown over, and animals can cross them 
					 * more readily. */
				case FEAT_TREE:
				{
					if (r_ptr->flags2 & (RF2_PASS_WALL))
						break;

					if ((!(r_ptr->flags2 & (RF2_FLYING))) &&
						(!(r_ptr->flags3 & (RF3_ANIMAL))) &&
						(randint(2) == 1)) do_move = FALSE;

					break;
				}
					/* Only fiery creatures or strong flying creatures will 
					 * cross lava.  Other creatures may look for other options.
					 */
				case FEAT_LAVA:
				{
					if (r_ptr->flags3 & (RF3_IM_FIRE))
						break;

					else if ((r_ptr->flags2 & (RF2_FLYING)) &&
						(m_ptr->hp > 49)) break;

					else if (randint(10) != 1)
					{
						stagger_temp = TRUE;
						goto I_cannot_handle_feature;
					}
					else
						do_move = FALSE;
					break;
				}
					/* Non-flying fire breathers and demons are stopped by 
					 * water.  They will normally look for other options.
					 */
				case FEAT_WATER:
				{
					if (((!(strchr("uU", r_ptr->d_char))) &&
							(!(r_ptr->flags4 & (RF4_BRTH_FIRE)))) ||
						(r_ptr->flags2 & (RF2_FLYING)))
						break;

					else if (randint(10) != 1)
					{
						stagger_temp = TRUE;
						goto I_cannot_handle_feature;
					}
					else
						do_move = FALSE;
					break;
				}

					/* No other traversable terrain hinders movement. */
				default:
				{
					break;
				}
			}
		}


		/* A monster is in the way.  Try to either kill it or push by it. */
		if (do_move && (cave_m_idx[ny][nx] > 0))
		{
			monster_type *n_ptr = &m_list[cave_m_idx[ny][nx]];
			monster_race *nr_ptr = &r_info[cave_m_idx[ny][nx]];

			/* Assume no movement */
			do_move = FALSE;

			/* Kill weaker monsters */
			if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
				(!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
				(compare_monsters(m_ptr, n_ptr) > 0))
			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster ate another monster */
				did_kill_body = TRUE;

				/* Message XXX XXX XXX */
#if 1 /* TNB */
				if ((m_ptr->mflag & MFLAG_VIEW) ||
					(n_ptr->mflag & MFLAG_VIEW))
				{
					char m_name[80], n_name[80];

					/* Acquire the monster names */
					monster_desc(m_name, m_ptr, 0x04);
					monster_desc(n_name, n_ptr, 0x04);

					/* Dump a message */
					sound(SNDGRP_EVENT, SND_MONSTER_KILL_OTHER, 0);
					msg_format("%^s kills %s.", m_name, n_name);
				}
#endif /* TNB */

				/* Kill the monster */
				delete_monster(ny, nx);
			}

			/* Push past weaker or similar monsters (unless leaving a wall) */
			if ((r_ptr->flags2 & (RF2_MOVE_BODY)) && (randint(2) == 1) &&
				(compare_monsters(m_ptr, n_ptr) >= 0) &&
				(cave_passable_bold(m_ptr->fy, m_ptr->fx)))
			{

				/* Allow movement */
				do_move = TRUE;

				/* Monster pushed past another monster */
				did_move_body = TRUE;
			}
		}


		/* Creature has been allowed move */
		if (do_move)
		{
			s16b this_o_idx, next_o_idx = 0;

			/* Take a turn */
			do_turn = TRUE;

			/* Move the monster */
			monster_swap(oy, ox, ny, nx);

			/* Rogues may set traps for monsters.  They can be fairly
			 * deadly, but monsters can also sometimes disarm or fly
			 * over them. -LM-
			 */
			if (cave_feat[ny][nx] == FEAT_MONSTER_TRAP)
			{
				if ((r_ptr->flags2 & (RF2_SMART)) && (randint(3) == 1))
				{
					if (m_ptr->ml)
					{
						char m_name[80];

						/* Acquire the monster name/poss */
						monster_desc(m_name, m_ptr, 0);

						sound(SNDGRP_EVENT, SND_DISARM, 0); /* ALLOW_SOUND */
						msg_format("%s finds your trap and disarms it.",
							m_name);
					}

					/* Kill the trap, decrement the monster trap count. */
					cave_set_feat(ny, nx, FEAT_FLOOR);
					num_trap_on_level--;
				}

				/* Traps seldom effect flying monsters or ghosts. */
				else if (((r_ptr->flags2 & (RF2_PASS_WALL)) ||
						(r_ptr->flags2 & (RF2_FLYING))) &&
					(randint(4) != 1))
				{
					if (m_ptr->ml)
					{
						char m_name[80];

						/* Acquire the monster name/poss */
						monster_desc(m_name, m_ptr, 0);

						msg_format("%s flies over your trap.", m_name);
					}
				}

				/* I thought traps only effected players!  Unfair! */
				else
				{
					/* Monster fear. */
					bool fear = FALSE;

					/* Assume a default death */
					cptr note_dies = " dies.";

					/* Some monsters get "destroyed" */
					if ((r_ptr->flags3 & (RF3_DEMON)) ||
						(r_ptr->flags3 & (RF3_UNDEAD)) ||
						(r_ptr->flags2 & (RF2_STUPID)) ||
						(strchr("Evg", r_ptr->d_char)))
					{
						/* Special note at death */
						note_dies = " is destroyed.";
					}

					if (player_has_los_bold(ny, nx))
					{
						char m_name[80];

						/* Acquire the monster name/poss */
						if (m_ptr->ml)
							monster_desc(m_name, m_ptr, 0);

						/* Default name */
						else
							strcpy(m_name, "Something");

						sound(SNDGRP_EVENT, SND_MTRAP_HIT, 0); /* ALLOW_SOUND */
						msg_format("%s sets off your cunning trap!",
							m_name);
					}

					else
						msg_print
							("You hear anguished yells in the distance.");

					/* Sometimes, the trap is destroyed. */
					if (randint(3) == 1)
					{
						cave_set_feat(ny, nx, FEAT_FLOOR);
						num_trap_on_level--;
					}

					/* Hurt the monster.  Big bruisers fall hard. */
					if (mon_take_hit(cave_m_idx[ny][nx],
							(1 + randint(p_ptr->lev * 3) +
								m_ptr->maxhp / 20), &fear, note_dies))
						return;

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

#if 0 /* ALLOW_SOUND */
						/* Sound */
						sound(SOUND_FLEE);
#endif /* ALLOW_SOUND */
						/* Get "the monster" or "it" */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						msg_format("%^s flees in terror!", m_name);
					}
				}

			}

			/* Possible disturb */
			if (m_ptr->ml && (disturb_move ||
					((m_ptr->mflag & (MFLAG_VIEW)) && disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}


			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[ny][nx]; this_o_idx;
				this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Acquire object */
				o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Skip gold */
				if (o_ptr->tval == TV_GOLD)
					continue;

				/* Take or Kill objects on the floor */
				if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
					(r_ptr->flags2 & (RF2_KILL_ITEM)))
				{
					u32b f1, f2, f3;

					u32b flg3 = 0L;

					char m_name[80];
					char o_name[O_NAME_MAX];

					/* Extract some flags */
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Acquire the object name */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0x04);

					/* React to objects that hurt the monster */
					if (f1 & (TR1_SLAY_DRAGON))
						flg3 |= (RF3_DRAGON);
					if (f1 & (TR1_SLAY_TROLL))
						flg3 |= (RF3_TROLL);
					if (f1 & (TR1_SLAY_GIANT))
						flg3 |= (RF3_GIANT);
					if (f1 & (TR1_SLAY_ORC))
						flg3 |= (RF3_ORC);
					if (f1 & (TR1_SLAY_DEMON))
						flg3 |= (RF3_DEMON);
					if (f1 & (TR1_SLAY_UNDEAD))
						flg3 |= (RF3_UNDEAD);
					if (f1 & (TR1_SLAY_ANIMAL))
						flg3 |= (RF3_ANIMAL);
					if (f1 & (TR1_SLAY_EVIL))
						flg3 |= (RF3_EVIL);

					/* The object cannot be picked up by the monster */
					if (artifact_p(o_ptr) || (r_ptr->flags3 & flg3))
					{
						/* Only give a message for "take_item" */
						if (r_ptr->flags2 & (RF2_TAKE_ITEM))
						{
							/* Take note */
							did_take_item = TRUE;

							/* Describe observable situations */
							if (m_ptr->ml && player_has_los_bold(ny, nx))
							{
								/* Dump a message */
								msg_format
									("%^s tries to pick up %s, but fails.",
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
#if 1 /* TNB */
						if (m_ptr->mflag & MFLAG_VIEW)
#else
						if (player_has_los_bold(ny, nx))
#endif

						{
							/* Dump a message */
							sound(SNDGRP_EVENT, SND_MONSTER_PICKUP, 0);	/* ALLOW_SOUND */
							msg_format("%^s picks up %s.", m_name, o_name);
						}

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain local object */
						object_copy(i_ptr, o_ptr);

						/* Delete the object */
						delete_object_idx(this_o_idx);

						/* Carry the object */
						(void) monster_carry(m_idx, i_ptr);
					}

					/* Destroy the item */
					else
					{
						/* Take note */
						did_kill_item = TRUE;

						/* Describe observable situations */
#if 1 /* TNB */
						if (m_ptr->mflag & MFLAG_VIEW)
#else
						if (player_has_los_bold(ny, nx))
#endif

						{
							/* Dump a message */
							sound(SNDGRP_EVENT, SND_OBJECT_DESTROYED, 0); /* ALLOW_SOUND */
							msg_format("%^s crushes %s.", m_name, o_name);
						}

						/* Delete the object */
						delete_object_idx(this_o_idx);
					}
				}
			}
		}

		/* Stop when done */
		if (do_turn)
			break;
	}

	/* If we haven't done anything, try casting a spell again */
	if (!do_turn && !do_move && !m_ptr->monfear && aware)
	{
		/* Cast spell */
		if (make_attack_spell(m_idx))
			return;
	}

	/* Notice changes in view */
	if (do_view)
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Fully update the flow XXX XXX XXX */
		p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);
	}


	/* Learn things from observable monster */
	if (m_ptr->ml)
	{
		/* Monster opened a door */
		if (did_open_door)
			l_ptr->flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door)
			l_ptr->flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item)
			l_ptr->flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item)
			l_ptr->flags2 |= (RF2_KILL_ITEM);

		/* Monster pushed past another monster */
		if (did_move_body)
			l_ptr->flags2 |= (RF2_MOVE_BODY);

		/* Monster ate another monster */
		if (did_kill_body)
			l_ptr->flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall)
			l_ptr->flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall)
			l_ptr->flags2 |= (RF2_KILL_WALL);
	}


	/* Hack -- get "bold" if out of options */
	if (!do_turn && !do_move && m_ptr->monfear && aware)
	{
		/* No longer afraid */
		m_ptr->monfear = 0;

		/* Flag minimum range for recalculation */
		m_ptr->min_range = 0;

		/* Message if seen */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Acquire the monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Dump a message */
			msg_format("%^s turns to fight!", m_name);
		}

		/* XXX XXX XXX Actually do something now (?) */
	}

}




/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), energizing each
 * monster, and allowing fully energized monsters to move, attack, pass, etc.
 *
 * Note that monsters can never move in the monster array (except when the
 * "compact_monsters()" function is called by "dungeon()" or "save_player()").
 *
 * This function is responsible for at least half of the processor time
 * on a normal system with a "normal" amount of monsters and a player doing
 * normal things.
 *
 * When the player is resting, virtually 90% of the processor time is spent
 * in this function, and its children, "process_monster()" and "make_move()".
 *
 * Most of the rest of the time is spent in "update_view()" and "lite_spot()",
 * especially when the player is running.
 *
 * Note the special "MFLAG_BORN" flag, which prevents monsters from doing
 * anything during the game turn in which they are created.  This flag is
 * optimized via the "repair_mflag_born" flag.
 *
 * Note the special "MFLAG_NICE" flag, which prevents "nasty" monsters from
 * using any of their spell attacks until the player gets a turn.  This flag
 * is optimized via the "repair_mflag_nice" flag.
 *
 * This function is modified by FM's patch to take as an input the a threshhold
 * amount of energy, rather than a fixed threshhold of 100.  This allows monsters
 * to be processed before and after the player on a given turn depending on their
 * energy level.  This function no longer applies energy to the monsters.
 */
void process_monsters(byte minimum_energy)
{
	int i;
	int fy, fx, total_wakeup_chance;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* The likelyhood, in 1/100ths of a percent, that any monster
	 * will be disturbed. -LM-
	 */
	total_wakeup_chance = p_ptr->base_wakeup_chance + add_wakeup_chance;

	/* People don't make much noise when resting. */
	if (p_ptr->resting)
		total_wakeup_chance /= 2;

	/* Players hidden in shadow are almost imperceptable. */
	if ((p_ptr->superstealth) && (!p_ptr->aggravate))
	{
		add_wakeup_chance = 0;

		/* Monsters almost never wake up. */
		if (total_wakeup_chance > 100)
			total_wakeup_chance = 100;
	}

	/* Repair "born" flags */
	if (repair_mflag_born)
	{
		/* Clear flag */
		repair_mflag_born = FALSE;

		/* Process the monsters */
		for (i = 1; i < m_max; i++)
		{
			/* Access the monster */
			m_ptr = &m_list[i];

			/* Ignore "dead" monsters */
			/* if (!m_ptr->r_idx) continue; */

			/* Clear "born" flag */
			m_ptr->mflag &= ~(MFLAG_BORN);
		}
	}

	if (p_ptr->wizard && wiz_free_moves)
		return;	/* TNB */

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Handle "leaving" */
		if (p_ptr->leaving)
			break;


		/* Access the monster */
		m_ptr = &m_list[i];


		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx)
			continue;


		/* Ignore "born" monsters XXX XXX */
		if (m_ptr->mflag & (MFLAG_BORN))
			continue;


		/* Not enough energy to move */
		if (m_ptr->energy < minimum_energy)
			continue;

		/* Use up "some" energy */
		m_ptr->energy -= 100;


		/* Heal monster? XXX XXX XXX */


		/* Access the race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Monsters can "sense" the player.  Sighting impaired on themed 
		 * levels (unless hurt).
		 */
		if (m_ptr->cdis <= ((p_ptr->themed_level &&
					(m_ptr->hp ==
m_ptr->maxhp)) ? r_ptr->aaf / 2 : r_ptr->aaf))
		{
			/* Process the monster */
			process_monster(i, total_wakeup_chance);

			/* Continue */
			continue;
		}


		/* Access the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;

		/* Monsters can "see" the player (backwards) XXX XXX */
		if (player_has_los_bold(fy, fx))
		{
			/* Hack -- Reduced sighting on themed levels (unless hurt). -LM- */
			if ((p_ptr->themed_level && (m_ptr->hp == m_ptr->maxhp)) &&
				(m_ptr->cdis > MAX_SIGHT / 2))
				continue;

			/* Process the monster */
			process_monster(i, total_wakeup_chance);

			/* Continue */
			continue;
		}

	}

}
