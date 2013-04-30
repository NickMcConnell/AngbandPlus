/* File: melee2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Variable to store whether a monster saw the player
 */
static bool mon_not_see_player;

/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Original idea and code by "DRS" (David Reeve Sward).
 *
 * Major modifications by "BEN" (Ben Harrison).
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
 */

/*
 * Internal probability routine
 */
static bool int_outof(monster_race *r_ptr, int prob)
{
	/* Ignore probabilities above 100 */
	if (prob > 100) prob = 100;

	/* Non-Smart monsters are half as "smart" */
	if (!(r_ptr->flags1 & (RF1_SMART))) prob = prob / 2;

	/* Roll the dice */
	return (rand_int(100) < prob);
}

/*
 * Check player invisibility - Returns true if the player is not seen
 * Might also mark monster memory
 */
static bool check_player_visibility(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	if ((m_ptr->blinded) || ((p_ptr->invis) && !(r_ptr->flags2 & (RF2_SEE_INVIS))))
	{
		/* Chance of seeing player depends on stealth */
		int skill = p_ptr->skill[SK_STL];
	
		if (!m_ptr->blinded) 
		{
			/* Reduce skill rate by lite radius */
			skill -= p_ptr->cur_lite;

			/* No negative skills */
			if (skill < 0) skill = 0;
		}
		
		/* Check for success */
 		if (randint(invis_chance[skill]) >= 10)	return TRUE;

		return FALSE;
	}
	
	if ((p_ptr->invis) && (r_ptr->flags2 & (RF2_SEE_INVIS)))
		lore_learn(m_ptr, LRN_FLAG2, RF2_SEE_INVIS, FALSE);

	return FALSE;
}

/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(int m_idx, u32b *sf1p, u32b *sf2p, u32b *sf3p)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	u32b sf1 = (*sf1p);
	u32b sf2 = (*sf2p);
	u32b sf3 = (*sf3p);

	u32b smart = 0L;

	/* Too stupid to know anything */
	if (r_ptr->flags1 & (RF1_STUPID)) return;

	/* Hack -- Occasionally forget player status */
	if (m_ptr->smart && (rand_int(100) < 1)) m_ptr->smart = 0L;

	/* Use the memorized flags */
	smart = m_ptr->smart;

	/* Cheat if requested */
	if (adult_smart_cheat)
	{
		/* Know weirdness */
		if (p_ptr->free_act) smart |= (SM_IMM_FREE);
		if (p_ptr->bravery)  smart |= (SM_IMM_BRAVE);
		if (p_ptr->no_blind) smart |= (SM_IMM_BLIND);
		if (p_ptr->no_confuse) smart |= (SM_IMM_CONF);
		if (!p_ptr->msp) smart |= (SM_IMM_MANA);

		/* Know resistances */
		if (p_ptr->res[RS_ACD] > 20) smart |= (SM_RES_ACID);
		if (p_ptr->res[RS_ELC] > 20) smart |= (SM_RES_ELEC);
		if (p_ptr->res[RS_FIR] > 20) smart |= (SM_RES_FIRE);
		if (p_ptr->res[RS_CLD] > 20) smart |= (SM_RES_COLD);
		if (p_ptr->res[RS_PSN] > 20) smart |= (SM_RES_POIS);
		if (p_ptr->res[RS_LIT] > 20) smart |= (SM_RES_LITE);
		if (p_ptr->res[RS_DRK] > 20) smart |= (SM_RES_DARK);
		if (p_ptr->res[RS_SND] > 20) smart |= (SM_RES_SOUND);
		if (p_ptr->res[RS_SHR] > 20) smart |= (SM_RES_SHARD);
		if (p_ptr->res[RS_NEX] > 20) smart |= (SM_RES_NEXUS);
		if (p_ptr->res[RS_NTH] > 20) smart |= (SM_RES_NETHR);
		if (p_ptr->res[RS_CHS] > 20) smart |= (SM_RES_CHAOS);
		if (p_ptr->res[RS_DIS] > 20) smart |= (SM_RES_DISEN);
		if (p_ptr->res[RS_TIM] > 20) smart |= (SM_RES_TIME);
		if (p_ptr->res[RS_DIS] > 20) smart |= (SM_RES_DISEASE);
		if (p_ptr->res[RS_WTR] > 20) smart |= (SM_RES_WATER);
		if (p_ptr->res[RS_MNA] > 20) smart |= (SM_RES_MANA);
	}

	/* Nothing known */
	if (!smart) return;

	if (smart & (SM_RES_ACID))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_ACD]))
		{
			sf1 &= ~(SRF1_BR_ACID);
			sf2 &= ~(SRF2_BA_ACID);
			sf2 &= ~(SRF2_BO_ACID);
		}
	}

	if (smart & (SM_RES_ELEC))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_ELC]))
		{
			sf1 &= ~(SRF1_BR_ELEC);
			sf2 &= ~(SRF2_BA_ELEC);
			sf2 &= ~(SRF2_BO_ELEC);
		}
	}

	if (smart & (SM_RES_FIRE))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_FIR]))
		{
			sf1 &= ~(SRF1_BR_FIRE);
			sf2 &= ~(SRF2_BA_FIRE);
			sf2 &= ~(SRF2_BO_FIRE);
		}
	}

	if (smart & (SM_RES_COLD))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_CLD]))
		{
			sf1 &= ~(SRF1_BR_COLD);
			sf2 &= ~(SRF2_BA_COLD);
			sf2 &= ~(SRF2_BO_COLD);
		}
	}

	if (smart & (SM_RES_POIS))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_PSN]))
		{
			sf1 &= ~(SRF1_BR_POIS);
			sf2 &= ~(SRF2_BA_POIS);
		}
	}

	if (smart & (SM_RES_DISEASE))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_DIS])) sf1 &= ~(SRF1_BR_DISEASE);
	}

	if (smart & (SM_IMM_BRAVE))
	{
		if (int_outof(r_ptr, 100)) sf2 &= ~(SRF2_SCARE);
	}

	if (smart & (SM_IMM_CONF))
	{
		if (int_outof(r_ptr, 100)) sf2 &= ~(SRF2_CONF);
	}

	if (smart & (SM_RES_LITE))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_LIT])) sf1 &= ~(SRF1_BR_LITE);
	}

	if (smart & (SM_RES_DARK))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_DRK]))
		{
			sf1 &= ~(SRF1_BR_DARK);
			sf2 &= ~(SRF2_BA_DARK);
		}
	}

	if (smart & (SM_RES_WATER))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_WTR]))
		{
			sf2 &= ~(SRF2_BO_WATE);
			sf2 &= ~(SRF2_BA_WATE);
		}
	}

	if (smart & (SM_IMM_BLIND))
	{
		if (int_outof(r_ptr, 100)) sf2 &= ~(SRF2_BLIND);
	}

	if (smart & (SM_RES_SOUND))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_SND])) sf1 &= ~(SRF1_BR_SOUN);
	}

	if (smart & (SM_RES_SHARD))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_SHR])) sf1 &= ~(SRF1_BR_SHAR);
	}

	if (smart & (SM_RES_NEXUS))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_NEX]))
		{
			sf1 &= ~(SRF1_BR_NEXU);
			sf3 &= ~(SRF3_TELE_LEVEL);
		}
	}

	if (smart & (SM_RES_NETHR))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_NTH]))
		{
			sf1 &= ~(SRF1_BR_NETH);
			sf2 &= ~(SRF2_BA_NETH);
			sf2 &= ~(SRF2_BO_NETH);
		}
	}

	if (smart & (SM_RES_CHAOS))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_CHS])) sf1 &= ~(SRF1_BR_CHAO);
	}

	if (smart & (SM_RES_DISEN))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_DSN])) sf1 &= ~(SRF1_BR_DISE);
	}

	if (smart & (SM_RES_TIME))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_TIM])) sf1 &= ~(SRF1_BR_TIME);
	}

	if (smart & (SM_RES_MANA))
	{
		if (int_outof(r_ptr, p_ptr->res[RS_MNA]))
		{
			sf1 &= ~(SRF1_BR_MANA);
			sf2 &= ~(SRF2_BA_MANA);
			sf2 &= ~(SRF2_BO_MANA);
		}
	}

	if (smart & (SM_IMM_FREE))
	{
		if (int_outof(r_ptr, 100)) sf2 &= ~(SRF2_HOLD);
		if (int_outof(r_ptr, 100)) sf2 &= ~(SRF2_SLOW);
	}

	if (smart & (SM_IMM_MANA))
	{
		if (int_outof(r_ptr, 100)) sf2 &= ~(SRF2_DRAIN_MANA);
	}

	(*sf1p) = sf1;
	(*sf2p) = sf2;
	(*sf3p) = sf3;
}

/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */
static bool summon_possible(int y1, int x1)
{
	int y, x;

	/* Start at the player's location, and check 2 grids in each dir */
	for (y = y1 - 2; y <= y1 + 2; y++)
	{
		for (x = x1 - 2; x <= x1 + 2; x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(y, x)) continue;

			/* Only check a circular area */
			if (distance(y1, x1, y, x) > 2) continue;

			/* 
			 * XXX no summon on glyph of warding 
			 * Since we don't know which monster this is yet, all anti-monster glyphs
			 * necessarily stop summoning of all monster types.
			 */
			if (trap_monster(y, x) && trap_glyph(y, x)) continue;

			/* Require empty floor grid in line of sight */
			if (cave_empty_bold(y, x) && los(y1, x1, y, x))
			{
				return (TRUE);
			}
		}
	}

	return (FALSE);
}

/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will return FALSE if a monster
 * is in the way.
 *
 * Then we should perhaps instead supply a flag to "projectable()".  XXX XXX
 */
static bool clean_shot(int y1, int x1, int y2, int x2)
{
	int y, x;

	int grid_n;
	u16b grid_g[512];

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, PROJECT_STOP);

	/* Source and target the same */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = GRID_Y(grid_g[grid_n-1]);
	x = GRID_X(grid_g[grid_n-1]);

	/* May not end in a wall grid */
	if (!cave_floor_bold(y, x)) return (FALSE);

	/* May not end in an unrequested grid */
	if ((y != y2) || (x != x2)) return (FALSE);

	/* Assume okay */
	return (TRUE);
}

/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void bolt(int m_idx, int typ, int dam_hp)
{
	int flg = PROJECT_STOP | PROJECT_KILL;

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, p_ptr->py, p_ptr->px, dam_hp, typ, flg);
}

/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void breath(int m_idx, int typ, int dam_hp)
{
	int rad;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	/* Determine the radius of the blast */
	rad = (r_ptr->flags2 & (RF2_WIDE_BREATH)) ? 3 : 2;

	/* Target the player with a ball attack */
	(void)project(m_idx, rad, p_ptr->py, p_ptr->px, dam_hp, typ, flg);
}

/*
 * Offsets for the spell indices
 */
#define SRF1_OFFSET 32 * 3
#define SRF2_OFFSET 32 * 4
#define SRF3_OFFSET 32 * 5

/*
 * Have a monster choose a spell to cast.
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * This function could be an efficiency bottleneck.
 */
static int choose_attack_spell(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	/* Extract the racial spell flags */
	u32b sf1 = r_ptr->s_flags1;
	u32b sf2 = r_ptr->s_flags2;
	u32b sf3 = r_ptr->s_flags3;
	
	u32b sf1_mask = 0L;
	u32b sf2_mask = 0L;
	u32b sf3_mask = 0L;

	int num = 0;
	byte spells[96];

	int i, py = p_ptr->py, px = p_ptr->px;

	bool has_escape, has_attack, has_summon, has_tactic_close, has_tactic_far;
	bool has_annoy, has_haste, has_heal;

	/* Hack -- allow "desperate" spells */
	if ((r_ptr->flags1 & (RF1_SMART)) && (m_ptr->hp < m_ptr->maxhp / 10) && (rand_int(100) < 50))
	{
		/* Require intelligent spells */
		sf1 &= (SRF1_INT_MASK);
		sf2 &= (SRF2_INT_MASK);
		sf3 &= (SRF3_INT_MASK);

		/* No spells left */
		if (!(sf1 || sf2 || sf3)) return (0);
	}

	/* Remove the "ineffective" spells */
	if (m_ptr->smart)
	{
		remove_bad_spells(m_idx, &sf1, &sf2, &sf3);

		/* No spells left */
		if (!(sf1 || sf2 || sf3)) return (0);
	}

	/* Check whether summons and bolts are worth it. */
	if (!(r_ptr->flags1 & (RF1_STUPID)))
	{
		/* Check for a clean bolt shot */
		if ((sf1 & (SRF1_BOLT_MASK) ||
			 sf2 & (SRF2_BOLT_MASK) ||
			 sf3 & (SRF3_BOLT_MASK)) &&
			!clean_shot(m_ptr->fy, m_ptr->fx, py, px))
		{
			/* Remove spells that will only hurt friends */
			sf1 &= ~(SRF1_BOLT_MASK);
			sf2 &= ~(SRF2_BOLT_MASK);
			sf3 &= ~(SRF3_BOLT_MASK);
		}

		/* Check for a possible summon */
		if (!(summon_possible(py, px)))
		{
			/* Remove summoning spells */
			sf1 &= ~(SRF1_SUMMON_MASK);
			sf2 &= ~(SRF2_SUMMON_MASK);
			sf3 &= ~(SRF3_SUMMON_MASK);
		}

		/* No spells left */
		if (!(sf1 || sf2 || sf3)) return (0);
	}

	/* If the monster is blinded, restrict to innate spells */
	if (m_ptr->blinded)
	{
		sf1 &= (SRF1_INNATE_MASK);
		sf2 &= (SRF2_INNATE_MASK);
		sf3 &= (SRF3_INNATE_MASK);

		/* No spells left */
		if (!(sf1 || sf2 || sf3)) return (0);
	}

	/* Limit spell choice if outside random monster spell range */
	if (m_ptr->cdis > (randint(6) + randint(6) + 4))
	{
		/* No spells that require being in range */
		sf1 &= ~(SRF1_SIGHT_MASK);
		sf2 &= ~(SRF2_SIGHT_MASK);
		sf3 &= ~(SRF3_SIGHT_MASK);
		sf1 &= ~(SRF1_SUMMON_MASK);
		sf2 &= ~(SRF2_SUMMON_MASK);
		sf3 &= ~(SRF3_SUMMON_MASK);
		sf1 &= ~(SRF1_ANNOY_MASK);
		sf2 &= ~(SRF2_ANNOY_MASK);
		sf3 &= ~(SRF3_ANNOY_MASK);

		/* No spells left */
		if (!(sf1 || sf2 || sf3)) return (0);
	}

	/* Limit spell choice if they can't see player (not LOS) */
	if (mon_not_see_player)
	{
		/* No spells that require targetting, and only a 50% chance of non-healing
		   spells */
		if (rand_int(100) < 75)
		{
			sf1 &= ~(SRF1_SIGHT_MASK);
			sf2 &= ~(SRF2_SIGHT_MASK);
			sf3 &= ~(SRF3_SIGHT_MASK);
		}
		else
		{
			/* Always enable healing spells */
			sf1 &= (SRF1_HEAL_MASK);
			sf2 &= (SRF2_HEAL_MASK);
			sf3 &= (SRF3_HEAL_MASK);
		}

		/* No spells left */
		if (!(sf1 || sf2 || sf3)) return (0);
	}

	/* If the monster is calmed, monster will only use heal/escape spells */
	if (m_ptr->calmed)
	{
		sf1 &= ((SRF1_HEAL_MASK) | (SRF1_ESCAPE_MASK));
		sf2 &= ((SRF2_HEAL_MASK) | (SRF2_ESCAPE_MASK));
		sf3 &= ((SRF3_HEAL_MASK) | (SRF3_ESCAPE_MASK));

		/* No spells left */
		if (!(sf1 || sf2 || sf3)) return (0);
	}

	/* Smart monsters restrict their spell choices. */
	if (!(r_ptr->flags1 & (RF1_STUPID)))
	{
		/* What have we got? */
		has_escape = ((sf1 & (SRF1_ESCAPE_MASK)) ||
		              (sf2 & (SRF2_ESCAPE_MASK)) ||
		              (sf3 & (SRF3_ESCAPE_MASK)));
		has_attack = ((sf1 & (SRF1_ATTACK_MASK)) ||
		              (sf2 & (SRF2_ATTACK_MASK)) ||
		              (sf3 & (SRF3_ATTACK_MASK)));
		has_summon = ((sf1 & (SRF1_SUMMON_MASK)) ||
		              (sf2 & (SRF2_SUMMON_MASK)) ||
		              (sf3 & (SRF3_SUMMON_MASK)));
		has_tactic_close = ((sf1 & (SRF1_TACTIC_CLOSE_MASK)) ||
							(sf2 & (SRF2_TACTIC_CLOSE_MASK)) ||
							(sf3 & (SRF3_TACTIC_CLOSE_MASK)));
		has_tactic_far = ((sf1 & (SRF1_TACTIC_FAR_MASK)) ||
						  (sf2 & (SRF2_TACTIC_FAR_MASK)) ||
						  (sf3 & (SRF3_TACTIC_FAR_MASK)));
		has_annoy = ((sf1 & (SRF1_ANNOY_MASK)) ||
		             (sf2 & (SRF2_ANNOY_MASK)) ||
		             (sf3 & (SRF3_ANNOY_MASK)));
		has_haste = ((sf1 & (SRF1_HASTE_MASK)) ||
		             (sf2 & (SRF2_HASTE_MASK)) ||
		             (sf3 & (SRF3_HASTE_MASK)));
		has_heal = ((sf1 & (SRF1_HEAL_MASK)) ||
		            (sf2 & (SRF2_HEAL_MASK)) ||
		            (sf3 & (SRF3_HEAL_MASK)));

		/*** Try to pick an appropriate spell type ***/

		/* Hurt badly or afraid, attempt to flee */
		if (has_escape && ((m_ptr->hp < m_ptr->maxhp / 4) || m_ptr->monfear))
		{
			/* Choose escape spell */
			sf1_mask = (SRF1_ESCAPE_MASK);
			sf2_mask = (SRF2_ESCAPE_MASK);
			sf3_mask = (SRF3_ESCAPE_MASK);
		}

		/* Still hurt badly, couldn't flee, attempt to heal */
		else if (has_heal && m_ptr->hp < m_ptr->maxhp / 4)
		{
			/* Choose heal spell */
			sf1_mask = (SRF1_HEAL_MASK);
			sf2_mask = (SRF2_HEAL_MASK);
			sf3_mask = (SRF3_HEAL_MASK);
		}

		/* Player is close and we have attack spells, blink away */
		else if (has_tactic_close && has_attack && 
			(distance(py, px, m_ptr->fy, m_ptr->fx) < 4) && (rand_int(100) < 75))
		{
			/* Choose tactical spell */
			sf1_mask = (SRF1_TACTIC_CLOSE_MASK);
			sf2_mask = (SRF2_TACTIC_CLOSE_MASK);
			sf3_mask = (SRF3_TACTIC_CLOSE_MASK);
		}

		/* Player is far try to get nearer */
		else if (has_tactic_far && (distance(py, px, m_ptr->fy, m_ptr->fx) > 2))
		{
			byte x;

			/* Small chance if we have attack spells, large if we don't */
			if (has_attack) x = 5;
			else x = 85;
			
			if (rand_int(100) < x)
			{
				/* Choose tactical spell */
				sf1_mask = (SRF1_TACTIC_FAR_MASK);
				sf2_mask = (SRF2_TACTIC_FAR_MASK);
				sf3_mask = (SRF3_TACTIC_FAR_MASK);
			}
		}

		/* We're hurt (not badly), try to heal */
		else if (has_heal && (m_ptr->hp < m_ptr->maxhp * 3 / 4) &&
		         (rand_int(100) < 60))
		{
			/* Choose heal spell */
			sf1_mask = (SRF1_HEAL_MASK);
			sf2_mask = (SRF2_HEAL_MASK);
			sf3_mask = (SRF3_HEAL_MASK);
		}

		/* Summon if possible (sometimes) */
		else if (has_summon && (rand_int(100) < 50))
		{
			/* Choose summon spell */
			sf1_mask = (SRF1_SUMMON_MASK);
			sf2_mask = (SRF2_SUMMON_MASK);
			sf3_mask = (SRF3_SUMMON_MASK);
		}

		/* Attack spell (most of the time) */
		else if (has_attack && (rand_int(100) < 85))
		{
			/* Choose attack spell */
			sf1_mask = (SRF1_ATTACK_MASK);
			sf2_mask = (SRF2_ATTACK_MASK);
			sf3_mask = (SRF3_ATTACK_MASK);
		}

		/* Try another tactical spell (sometimes) */
		else if (has_tactic_close && (rand_int(100) < 40))
		{
			/* Choose tactic spell */
			sf1_mask = (SRF1_TACTIC_CLOSE_MASK);
			sf2_mask = (SRF2_TACTIC_CLOSE_MASK);
			sf3_mask = (SRF3_TACTIC_CLOSE_MASK);
		}

		/* Try another tactical spell (sometimes) */
		else if (has_tactic_far && (rand_int(100) < 20))
		{
			/* Choose tactic spell */
			sf1_mask = (SRF1_TACTIC_FAR_MASK);
			sf2_mask = (SRF2_TACTIC_FAR_MASK);
			sf3_mask = (SRF3_TACTIC_FAR_MASK);
		}

		/* Haste self if we aren't already somewhat hasted (rarely) */
		else if (has_haste && (rand_int(100) < (20 + m_ptr->bspeed - m_ptr->mspeed)))
		{
			/* Choose haste spell */
			sf1_mask = (SRF1_HASTE_MASK);
			sf2_mask = (SRF2_HASTE_MASK);
			sf3_mask = (SRF3_HASTE_MASK);
		}

		/* Annoy player (most of the time) */
		else if (has_annoy && (rand_int(100) < 85))
		{
			/* Choose annoyance spell */
			sf1_mask = (SRF1_ANNOY_MASK);
			sf2_mask = (SRF2_ANNOY_MASK);
			sf3_mask = (SRF3_ANNOY_MASK);
		}

		/* Else choose no spell (The masks default to this.) */

		/* Keep only the interesting spells */
		sf1 &= sf1_mask;
		sf2 &= sf2_mask;
		sf3 &= sf3_mask;

		/* Anything left? */
		if (!(sf1 || sf2 || sf3)) return (0);
	}

	/* Extract the "innate" spells */
	for (i = 0; i < 32; i++)
	{
		if (sf1 & (1L << i)) spells[num++] = i + SRF1_OFFSET;
		if (sf2 & (1L << i)) spells[num++] = i + SRF2_OFFSET;
		if (sf3 & (1L << i)) spells[num++] = i + SRF3_OFFSET;
	}

	/* Paranoia */
	if (num == 0) return (0);

	/* Pick at random */
	return (spells[rand_int(num)]);
}

/*
 * Creatures can cast spells, shoot missiles, and breathe.
 *
 * Returns "TRUE" if a spell (or whatever) was (successfully) cast.
 *
 * XXX XXX XXX This function could use some work, but remember to
 * keep it as optimized as possible, while retaining generic code.
 *
 * Verify the various "blind-ness" checks in the code.
 *
 * XXX XXX XXX Note that several effects should really not be "seen"
 * if the player is blind.  See also "effects.c" for other "mistakes".
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Perhaps smart monsters should decline to use "bolt" spells if
 * there is a monster in the way, unless they wish to kill it.
 *
 * It will not be possible to "correctly" handle the case in which a
 * monster attempts to attack a location which is thought to contain
 * the player, but which in fact is nowhere near the player, since this
 * might induce all sorts of messages about the attack itself, and about
 * the effects of the attack, which the player might or might not be in
 * a position to observe.  Thus, for simplicity, it is probably best to
 * only allow "faulty" attacks by a monster if one of the important grids
 * (probably the initial or final grid) is in fact in view of the player.
 * It may be necessary to actually prevent spell attacks except when the
 * monster actually has line of sight to the player.  Note that a monster
 * could be left in a bizarre situation after the player ducked behind a
 * pillar and then teleported away, for example.
 *
 * Note that certain spell attacks do not use the "project()" function
 * but "simulate" it via the "direct" variable, which is always at least
 * as restrictive as the "project()" function.  This is necessary to
 * prevent "blindness" attacks and such from bending around walls.
 *
 * Note that this function attempts to optimize the use of spells for the
 * cases in which the monster has no spells, or has spells but cannot use
 * them, or has spells but they will have no "useful" effect.  Note that
 * this function has been an efficiency bottleneck in the past.
 *
 * Note the special "MFLAG_NICE" flag, which prevents a monster from using
 * any spell attacks until the player has had a single chance to move.
 */
static bool make_attack_spell(int m_idx)
{
	int k, thrown_spell, rlev;

	int failrate;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);
	
	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	/* Target player */
	int x = p_ptr->px;
	int y = p_ptr->py;

	/* Summon count */
	int count = 0;

	/* Extract the blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Extract the "see-able-ness" */
	bool seen = (!blind && m_ptr->ml);

	/* Assume "normal" target */
	bool normal = TRUE;

	/* Assume "projectable" */
	bool direct = TRUE;

	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	/* Cannot cast spells when nice */
	if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);

	/* Only do spells occasionally */
	if (rand_int(100) >= r_ptr->freq_spell) return (FALSE);

	/* If player is unseen, lower chance of casting spell */
	if (mon_not_see_player && (rand_int(100) < 50)) return (FALSE);

	/* Hack -- require projectable player */
	if (normal)
	{
		/* Check range */
		if (m_ptr->cdis > MAX_RANGE) return (FALSE);

		/* Check path */
		if (!projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px)) return (FALSE);
	}

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Handle "leaving" */
	if (p_ptr->leaving) return (FALSE);

	/* Choose a spell to cast */
	thrown_spell = choose_attack_spell(m_idx);

	/* Abort if no spell was chosen */
	if (!thrown_spell) return (FALSE);

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, sizeof(m_poss), m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, 0x88);

	/* Calculate spell failure rate */
	failrate = 25 - (rlev + 3) / 4;

	/* Handle "stunned" */
	if (m_ptr->stunned) failrate *= 3;

	/* Some monsters will never fail (for jellies and such) */
	if (r_ptr->flags2 & (RF2_NEVER_FAIL)) failrate = 0;

	/* Check for spell failure (innate attacks never fail) */
	if ((thrown_spell >= SRF2_OFFSET) && (rand_int(100) < failrate))
	{
		/* Message */
		message_format(MSG_MON_FAIL, m_ptr->r_idx, "%^s tries to cast a spell, but fails.", m_name);

		return (TRUE);
	}

	/* Cast the spell. */
	switch (thrown_spell)
	{
		/* SRF1_SHRIEK */
		case SRF1_OFFSET + 0:
		{
			if (!direct) break;
			disturb(1);
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx);
			break;
		}

		/* SRF1_XXX2X4 */
		case SRF1_OFFSET + 1:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s makes a strange noise.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s throws a dart.", m_name);
			bolt(m_idx, GF_ARROW, damroll(2, 10));
			break;
		}

		/* SRF1_XXX3X4 */
		case SRF1_OFFSET + 2:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s makes a strange noise.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s throws a javelin.", m_name);
			bolt(m_idx, GF_ARROW, damroll(4, 10));
			break;
		}

		/* SRF1_XXX4X4 */
		case SRF1_OFFSET + 3:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s makes a strange noise.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s throws a heavy spear.", m_name);
			bolt(m_idx, GF_ARROW, damroll(8, 10));
			break;
		}

		/* SRF1_ARROW_1 */
		case SRF1_OFFSET + 4:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s makes a strange noise.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s fires an arrow.", m_name);
			bolt(m_idx, GF_ARROW, damroll(1, 10));
			break;
		}

		/* SRF1_ARROW_2 */
		case SRF1_OFFSET + 5:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s makes a strange noise.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s fires an arrow!", m_name);
			bolt(m_idx, GF_ARROW, damroll(3, 10));
			break;
		}

		/* SRF1_ARROW_3 */
		case SRF1_OFFSET + 6:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s makes a strange noise.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s fires a missile.", m_name);
			bolt(m_idx, GF_ARROW, damroll(5, 10));
			break;
		}

		/* SRF1_ARROW_4 */
		case SRF1_OFFSET + 7:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s makes a strange noise.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s fires a missile!", m_name);
			bolt(m_idx, GF_ARROW, damroll(7, 10));
			break;
		}

		/* SRF1_BR_ACID */
		case SRF1_OFFSET + 8:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes acid.", m_name);
			breath(m_idx, GF_ACID,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* SRF1_BR_ELEC */
		case SRF1_OFFSET + 9:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes lightning.", m_name);
			breath(m_idx, GF_ELEC,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* SRF1_BR_FIRE */
		case SRF1_OFFSET + 10:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes fire.", m_name);
			breath(m_idx, GF_FIRE,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* SRF1_BR_COLD */
		case SRF1_OFFSET + 11:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes frost.", m_name);
			breath(m_idx, GF_COLD,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* SRF1_BR_POIS */
		case SRF1_OFFSET + 12:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes gas.", m_name);
			breath(m_idx, GF_POIS,
			       ((m_ptr->hp / 2) > 800 ? 800 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

		/* SRF1_BR_NETH */
		case SRF1_OFFSET + 13:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes nether.", m_name);
			breath(m_idx, GF_NETHER,
			       ((m_ptr->hp / 4) > 550 ? 550 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* SRF1_BR_LITE */
		case SRF1_OFFSET + 14:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes light.", m_name);
			breath(m_idx, GF_LITE,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_LITE);
			break;
		}

		/* SRF1_BR_DARK */
		case SRF1_OFFSET + 15:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes darkness.", m_name);
			breath(m_idx, GF_DARK,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_DARK);
			break;
		}

		/* SRF1_BR_SOUN */
		case SRF1_OFFSET + 16:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes sound.", m_name);
			breath(m_idx, GF_SOUND,
			       ((m_ptr->hp / 4) > 500 ? 500 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_SOUND);
			break;
		}

		/* SRF1_BR_CHAO */
		case SRF1_OFFSET + 17:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes chaos.", m_name);
			breath(m_idx, GF_CHAOS,
			       ((m_ptr->hp / 4) > 500 ? 500 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_CHAOS);
			break;
		}

		/* SRF1_BR_DISE */
		case SRF1_OFFSET + 18:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes disenchantment.", m_name);
			breath(m_idx, GF_DISENCHANT,
			       ((m_ptr->hp / 4) > 500 ? 500 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_DISEN);
			break;
		}

		/* SRF1_BR_NEXU */
		case SRF1_OFFSET + 19:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes nexus.", m_name);
			breath(m_idx, GF_NEXUS,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_NEXUS);
			break;
		}

		/* SRF1_BR_TIME */
		case SRF1_OFFSET + 20:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes time.", m_name);
			breath(m_idx, GF_TIME,
			       ((m_ptr->hp / 2) > 150 ? 150 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_TIME);
			break;
		}

		/* SRF1_BR_INER */
		case SRF1_OFFSET + 21:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes inertia.", m_name);
			breath(m_idx, GF_INERTIA,
			       ((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)));
			break;
		}

		/* SRF1_BR_GRAV */
		case SRF1_OFFSET + 22:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes gravity.", m_name);
			breath(m_idx, GF_GRAVITY,
			       ((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3)));
			break;
		}

		/* SRF1_BR_SHAR */
		case SRF1_OFFSET + 23:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes shards.", m_name);
			breath(m_idx, GF_SHARD,
			       ((m_ptr->hp / 4) > 500 ? 500 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_SHARD);
			break;
		}

		/* SRF1_BR_PLAS */
		case SRF1_OFFSET + 24:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes plasma.", m_name);
			breath(m_idx, GF_PLASMA,
			       ((m_ptr->hp / 2) > 150 ? 150 : (m_ptr->hp / 2)));
			break;
		}

		/* SRF1_BR_FORCE */
		case SRF1_OFFSET + 25:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes force.", m_name);
			breath(m_idx, GF_FORCE,
			       ((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)));
			break;
		}

		/* SRF1_BR_MANA */
		case SRF1_OFFSET + 26:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes mana.", m_name);
			breath(m_idx, GF_MANA,
			       ((m_ptr->hp / 4) > 550 ? 550 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_MANA);
			break;
		}

		/* SRF1_BR_WATER */
		case SRF1_OFFSET + 27:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes water.", m_name);
			breath(m_idx, GF_WATER,
			       ((m_ptr->hp / 4) > 250 ? 250 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_WATER);
			break;
		}

		/* SRF1_BR_DISEASE */
		case SRF1_OFFSET + 28:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s breathes disease.", m_name);
			breath(m_idx, GF_DISEASE,
			       ((m_ptr->hp / 2) > 800 ? 800 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_DISEASE);
			break;
			break;
		}

		/* SRF1_XXX7X4 */
		case SRF1_OFFSET + 29:
		{
			break;
		}

		/* SRF1_XXX8X4 */
		case SRF1_OFFSET + 30:
		{
			break;
		}

		/* SRF1_XXX8X4 */
		case SRF1_OFFSET + 31:
		{
			break;
		}

		/* SRF2_BA_ACID */
		case SRF2_OFFSET + 0:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts an acid ball.", m_name);
			breath(m_idx, GF_ACID, randint(rlev * 3) + 15);
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* SRF2_BA_ELEC */
		case SRF2_OFFSET + 1:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a lightning ball.", m_name);
			breath(m_idx, GF_ELEC, randint((rlev * 3) / 2) + 8);
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* SRF2_BA_FIRE */
		case SRF2_OFFSET + 2:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a fire ball.", m_name);
			breath(m_idx, GF_FIRE, randint((rlev * 7) / 2) + 10);
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* SRF2_BA_COLD */
		case SRF2_OFFSET + 3:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a frost ball.", m_name);
			breath(m_idx, GF_COLD, randint((rlev * 3) / 2) + 10);
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* SRF2_BA_POIS */
		case SRF2_OFFSET + 4:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a stinking cloud.", m_name);
			breath(m_idx, GF_POIS, damroll(12, 2));
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

		/* SRF2_BA_NETH */
		case SRF2_OFFSET + 5:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a nether ball.", m_name);
			breath(m_idx, GF_NETHER, (50 + damroll(10, 10) + rlev));
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* SRF2_BA_WATE */
		case SRF2_OFFSET + 6:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s gestures fluidly.", m_name);
			message(MSG_EFFECT, 0, "You are engulfed in a whirlpool.");
			breath(m_idx, GF_WATER, randint((rlev * 5) / 2) + 50);
			break;
		}

		/* SRF2_BA_MANA */
		case SRF2_OFFSET + 7:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles powerfully.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s invokes a mana storm.", m_name);
			breath(m_idx, GF_MANA, (rlev * 5) + damroll(10, 10));
			break;
		}

		/* SRF2_BA_DARK */
		case SRF2_OFFSET + 8:
		{
			int n = (rlev / 10) + 3;
			if (n > 10) n = 10;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles powerfully.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s invokes a darkness storm.", m_name);
			breath(m_idx, GF_DARK, (rlev * 5) + damroll(n, n));
			update_smart_learn(m_idx, DRS_RES_DARK);
			break;
		}

		/* SRF2_DRAIN_MANA */
		case SRF2_OFFSET + 9:
		{
			if (!direct) break;
			if (p_ptr->csp)
			{
				int r1;

				/* Disturb if legal */
				disturb(1);

				/* Basic message */
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s draws psychic energy from you!", m_name);

				/* Attack power */
				r1 = (randint(rlev) / 2) + 1;

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

				/* Heal the monster */
				if (m_ptr->hp < m_ptr->maxhp)
				{
					/* Heal */
					m_ptr->hp += (6 * r1);
					if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

					/* Special message */
					if (seen)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s appears healthier.", m_name);
					}
				}
			}
			update_smart_learn(m_idx, DRS_MANA);
			break;
		}

		/* SRF2_MIND_BLAST */
		case SRF2_OFFSET + 10:
		{
			if (!direct) break;
			disturb(1);
			if (!seen)
			{
				message(MSG_MONSTER, 0, "You feel something focusing on your mind.");
			}
			else
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s gazes deep into your eyes.", m_name);
			}

			if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				message(MSG_EFFECT, 0, "Your mind is blasted by psionic energy.");
				if (!p_ptr->no_confuse)
				{
					(void)set_confused(p_ptr->confused + rand_int(4) + 4);
				}
				damage_player(damroll(8, 8), ddesc);
			}
			break;
		}

		/* SRF2_BRAIN_SMASH */
		case SRF2_OFFSET + 11:
		{
			if (!direct) break;
			disturb(1);
			if (!seen)
			{
				message(MSG_MONSTER, 0, "You feel something focusing on your mind.");
			}
			else
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s looks deep into your eyes.", m_name);
			}
			if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				message(MSG_EFFECT, 0, "Your mind is blasted by psionic energy.");
				damage_player(damroll(12, 15), ddesc);
				if (!p_ptr->no_blind)
				{
					(void)set_blind(p_ptr->blind + 8 + rand_int(8));
				}
				if (!p_ptr->no_confuse)
				{
					(void)set_confused(p_ptr->confused + rand_int(4) + 4);
				}
				if (!p_ptr->free_act)
				{
					(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
				}
				(void)set_slow(p_ptr->slow + rand_int(4) + 4);
			}
			break;
		}

		/* SRF2_CAUSE_1 */
		case SRF2_OFFSET + 12:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s points at you and curses.", m_name);
			if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				take_hit(damroll(3, 8), ddesc);
			}
			break;
		}

		/* SRF2_CAUSE_2 */
		case SRF2_OFFSET + 13:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s points at you and curses horribly.", m_name);
			if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				take_hit(damroll(8, 8), ddesc);
			}
			break;
		}

		/* SRF2_CAUSE_3 */
		case SRF2_OFFSET + 14:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles loudly.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s points at you, incanting terribly!", m_name);
			if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				take_hit(damroll(10, 15), ddesc);
			}
			break;
		}

		/* SRF2_CAUSE_4 */
		case SRF2_OFFSET + 15:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s screams the word 'DIE!'", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s points at you, screaming the word DIE!", m_name);
			if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				take_hit(damroll(15, 15), ddesc);
				if (!p_ptr->no_cut) set_cut(p_ptr->cut + damroll(10, 10));
			}
			break;
		}

		/* SRF2_BO_ACID */
		case SRF2_OFFSET + 16:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a acid bolt.", m_name);
			bolt(m_idx, GF_ACID,
			     damroll(7, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* SRF2_BO_ELEC */
		case SRF2_OFFSET + 17:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a lightning bolt.", m_name);
			bolt(m_idx, GF_ELEC,
			     damroll(4, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* SRF2_BO_FIRE */
		case SRF2_OFFSET + 18:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a fire bolt.", m_name);
			bolt(m_idx, GF_FIRE,
			     damroll(9, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* SRF2_BO_COLD */
		case SRF2_OFFSET + 19:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a frost bolt.", m_name);
			bolt(m_idx, GF_COLD,
			     damroll(6, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* SRF2_XXX1 */
		case SRF2_OFFSET + 20:
		{
			/* XXX XXX XXX */
			break;
		}

		/* SRF2_BO_NETH */
		case SRF2_OFFSET + 21:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a nether bolt.", m_name);
			bolt(m_idx, GF_NETHER,
			     30 + damroll(5, 5) + (rlev * 3) / 2);
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* SRF2_BO_WATE */
		case SRF2_OFFSET + 22:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a water bolt.", m_name);
			bolt(m_idx, GF_WATER,
			     damroll(10, 10) + (rlev));
			break;
		}

		/* SRF2_BO_MANA */
		case SRF2_OFFSET + 23:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a mana bolt.", m_name);
			bolt(m_idx, GF_MANA,
			     randint(rlev * 7 / 2) + 50);
			break;
		}

		/* SRF2_BO_PLAS */
		case SRF2_OFFSET + 24:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a plasma bolt.", m_name);
			bolt(m_idx, GF_PLASMA,
			     10 + damroll(8, 7) + (rlev));
			break;
		}

		/* SRF2_BO_ICEE */
		case SRF2_OFFSET + 25:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts an ice bolt.", m_name);
			bolt(m_idx, GF_ICE,
			     damroll(6, 6) + (rlev));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* SRF2_MISSILE */
		case SRF2_OFFSET + 26:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a magic missile.", m_name);
			bolt(m_idx, GF_MISSILE,
			     damroll(2, 6) + (rlev / 3));
			break;
		}

		/* SRF2_SCARE */
		case SRF2_OFFSET + 27:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles, and you hear scary noises.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a fearful illusion.", m_name);
			if (p_ptr->bravery)
			{
				message(MSG_RESIST, 0, "You refuse to be frightened.");
			}
			else if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You refuse to be frightened.");
			}
			else
			{
				(void)set_afraid(p_ptr->afraid + 5 + randint((rlev / 10) + 1));
			}
			update_smart_learn(m_idx, DRS_BRAVE);
			break;
		}

		/* SRF2_BLIND */
		case SRF2_OFFSET + 28:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a spell, burning your eyes!", m_name);
			if (p_ptr->no_blind)
			{
				message(MSG_RESIST, 0, "You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				(void)set_blind(12 + rand_int(4));
			}
			update_smart_learn(m_idx, DRS_NO_BLIND);
			break;
		}

		/* SRF2_CONF */
		case SRF2_OFFSET + 29:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles, and you hear puzzling noises.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s creates a mesmerising illusion.", m_name);
			if (p_ptr->no_confuse)
			{
				message(MSG_RESIST, 0, "You disbelieve the feeble spell.");
			}
			else if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You disbelieve the feeble spell.");
			}
			else
			{
				(void)set_confused(p_ptr->confused + rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_RES_CONFU);
			break;
		}

		/* SRF2_SLOW */
		case SRF2_OFFSET + 30:
		{
			if (!direct) break;
			disturb(1);
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s drains power from your muscles!", m_name);
			if (p_ptr->free_act)
			{
				message(MSG_RESIST, 0, "You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				(void)set_slow(p_ptr->slow + rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}

		/* SRF2_HOLD */
		case SRF2_OFFSET + 31:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s stares deep into your eyes!", m_name);
			if (p_ptr->free_act)
			{
				message(MSG_RESIST, 0, "You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}



		/* SRF3_HASTE */
		case SRF3_OFFSET + 0:
		{
			disturb(1);
			if (blind)
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			}
			else
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s concentrates on %s body.", m_name, m_poss);
			}

			/* Allow quick speed increases to base + 10 */
			if (m_ptr->mspeed < m_ptr->bspeed + 10)
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s starts moving faster.", m_name);
				m_ptr->mspeed += 10;
			}

			/* Allow small speed increases to base + 20 */
			else if (m_ptr->mspeed < m_ptr->bspeed + 20)
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s starts moving faster.", m_name);
				m_ptr->mspeed += 2;
			}

			break;
		}

		/* SRF3_XXX1X6 */
		case SRF3_OFFSET + 1:
		{
			break;
		}

		/* SRF3_HEAL */
		case SRF3_OFFSET + 2:
		{
			disturb(1);

			/* Message */
			if (blind)
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			}
			else
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s concentrates on %s wounds.", m_name, m_poss);
			}

			/* Heal some */
			m_ptr->hp += (rlev * 6);

			/* Fully healed */
			if (m_ptr->hp >= m_ptr->maxhp)
			{
				/* Fully healed */
				m_ptr->hp = m_ptr->maxhp;

				/* Message */
				if (seen)
				{
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s looks REALLY healthy!", m_name);
				}
				else
				{
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s sounds REALLY healthy!", m_name);
				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (seen)
				{
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s looks healthier.", m_name);
				}
				else
				{
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s sounds healthier.", m_name);
				}
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				m_ptr->monfear = 0;

				/* Message */
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s recovers %s courage.", m_name, m_poss);
			}

			break;
		}

		/* SRF3_XXX2X6 */
		case SRF3_OFFSET + 3:
		{
			break;
		}

		/* SRF3_BLINK */
		case SRF3_OFFSET + 4:
		{
			disturb(1);
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s blinks away.", m_name);
			teleport_away(m_idx, 10);
			break;
		}

		/* SRF3_TPORT */
		case SRF3_OFFSET + 5:
		{
			disturb(1);
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s teleports away.", m_name);
			teleport_away(m_idx, MAX_SIGHT * 2 + 5);
			break;
		}

		/* SRF3_BLINK_TO */
		case SRF3_OFFSET + 6:
		{
			if (!direct) break;
			disturb(1);

			if (p_ptr->nexus_y == 0)
			{
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s appears beside you.", m_name);
			}

			teleport_monster_to(m_idx, p_ptr->py, p_ptr->px);
			break;
		}

		/* SRF3_XXX4X6 */
		case SRF3_OFFSET + 7:
		{
			break;
		}

		/* SRF3_TELE_TO */
		case SRF3_OFFSET + 8:
		{
			if (!direct) break;
			disturb(1);
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s commands you to return.", m_name);
			teleport_player_to(m_ptr->fy, m_ptr->fx);
			break;
		}

		/* SRF3_TELE_AWAY */
		case SRF3_OFFSET + 9:
		{
			if (!direct) break;
			disturb(1);
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s teleports you away.", m_name);
			teleport_player(100);
			break;
		}

		/* SRF3_TELE_LEVEL */
		case SRF3_OFFSET + 10:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles strangely.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s gestures at your feet.", m_name);
			if (!resist_effect(RS_NEX))
			{
				message(MSG_RESIST, 0, "You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else
			{
				teleport_player_level();
			}
			update_smart_learn(m_idx, DRS_RES_NEXUS);
			break;
		}

		/* SRF3_DARKNESS */
		case SRF3_OFFSET + 11:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s gestures in shadow.", m_name);
			(void)unlite_area(0, 3);
			break;
		}

		/* SRF3_FORGET */
		case SRF3_OFFSET + 12:
		{
			if (!direct) break;
			disturb(1);
			message_format(MSG_MONSTER, m_ptr->r_idx, "%^s tries to blank your mind.", m_name);

			if (rand_int(100) < p_ptr->skill[SK_SAV])
			{
				message(MSG_RESIST, 0, "You resist the effects!");
			}
			else if (lose_all_info())
			{
				message(MSG_EFFECT, 0, "Your memories fade away.");
			}
			break;
		}

		/* SRF3_TRAPS1 */
		case SRF3_OFFSET + 13:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles, and then cackles evilly.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a spell and cackles evilly.", m_name);
			(void)trap_creation(1);
			break;
		}

		/* SRF3_TRAPS2 */
		case SRF3_OFFSET + 14:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles, and then cackles evilly.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a spell and cackles evilly.", m_name);
			(void)trap_creation(2);
			break;
		}

		/* SRF3_TRAPS3 */
		case SRF3_OFFSET + 15:
		{
			if (!direct) break;
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles, and then cackles evilly.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s casts a spell and cackles evilly.", m_name);
			(void)trap_creation(3);
			break;
		}

		/* SRF3_S_KIN */
		case SRF3_OFFSET + 16:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons %s %s.", m_name, m_poss,
			                (m_ptr->u_idx ? "minions" : "kin"));

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			/* Hack -- Set the letter of the monsters to summon */
			summon_kin_type = r_ptr->d_char;
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_KIN);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_KIN, "You hear monsters appear nearby.");
			}
			break;
		}

		/* SRF3_HI_DEMON */
		case SRF3_OFFSET + 17:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons greater demons!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_DEMON);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_HI_DEMON, "You hear many evil things appear nearby.");
			}
			break;
		}

		/* SRF3_S_MONSTER */
		case SRF3_OFFSET + 18:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons help!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, 0);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, 0, "You hear something appear nearby.");
			}
			break;
		}

		/* SRF3_S_MONSTERS */
		case SRF3_OFFSET + 19:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons monsters!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, rlev, 0);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, 0, "You hear monsters appear nearby.");
			}
			break;
		}

		/* SRF3_S_ANIMALS */
		case SRF3_OFFSET + 20:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons natural creatures.", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_ANIMALS);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_ANIMALS, "You hear monsters appear nearby.");
			}
			break;
		}

		/* SRF3_S_SPIDER */
		case SRF3_OFFSET + 21:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons spiders.", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_SPIDER);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_SPIDER, "You hear monsters appear nearby.");
			}
			break;
		}

		/* SRF3_S_HOUND */
		case SRF3_OFFSET + 22:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons hounds.", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HOUND);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_HOUND, "You hear monsters appear nearby.");
			}
			break;
		}

		/* SRF3_S_FAERY */
		case SRF3_OFFSET + 23:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons faeries.", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_FAERY);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_FAERY, "You hear monsters appear nearby.");
			}
			break;
		}

		/* SRF3_S_HYDRA */
		case SRF3_OFFSET + 24:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons hydras.", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HYDRA);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_HYDRA, "You hear monsters appear nearby.");
			}
			break;
		}

		/* SRF3_S_HORROR */
		case SRF3_OFFSET + 25:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons a nameless horror!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HORROR);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_HORROR, "You hear something appear nearby.");
			}
			break;
		}

		/* SRF3_S_DEMON */
		case SRF3_OFFSET + 26:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons a hellish adversary!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_DEMON);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_DEMON, "You hear something appear nearby.");
			}
			break;
		}

		/* SRF3_S_UNDEAD */
		case SRF3_OFFSET + 27:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons an undead adversary!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_UNDEAD);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_UNDEAD, "You hear something appear nearby.");
			}
			break;
		}

		/* SRF3_S_DRAGON */
		case SRF3_OFFSET + 28:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons a dragon!", m_name);
			for (k = 0; k < 1; k++)

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			{
				count += summon_specific(y, x, rlev, SUMMON_DRAGON);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_DRAGON, "You hear something appear nearby.");
			}
			break;
		}

		/* SRF3_S_HI_UNDEAD */
		case SRF3_OFFSET + 29:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons greater undead!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 5; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_HI_UNDEAD, "You hear many creepy things appear nearby.");
			}
			break;
		}

		/* SRF3_S_HI_DRAGON */
		case SRF3_OFFSET + 30:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons ancient dragons!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 5; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_DRAGON);
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_HI_DRAGON, "You hear many powerful things appear nearby.");
			}
			break;
		}

		/* SRF3_S_UNIQUE */
		case SRF3_OFFSET + 31:
		{
			disturb(1);
			if (blind) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s mumbles.", m_name);
			else message_format(MSG_MONSTER, m_ptr->r_idx, "%^s magically summons special opponents!", m_name);

			/* Boost rlev by Monster Summon Power? Increase it by one. This prevents scumming summoners. */
			if (rand_int(40) < p_ptr->monster_summon_power) rlev += p_ptr->monster_summon_power;
			p_ptr->monster_summon_power++;

			for (k = 0; k < 5; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_UNIQUE);
			}
			for (k = 0; k < 5; k++)
			{
				/* Hack - summon hi demons, undead, or dragons */
				count += summon_specific(y, x, rlev, SUMMON_HI_DEMON + rand_int(3));
			}
			if (blind && count)
			{
				message(MSG_SUMMON, SUMMON_UNIQUE, "You hear many powerful things appear nearby.");
			}
			break;
		}
	}

	/* Remember what the monster did to us */
	if (seen)
	{
		/* Innate spell */
		if (thrown_spell < 32 * 4)
		{
			lore_learn(m_ptr, LRN_S_FLAG1, (1L << (thrown_spell - SRF1_OFFSET)), TRUE);
			lore_learn(m_ptr, LRN_CASTS, 0, TRUE);
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32 * 5)
		{
			lore_learn(m_ptr, LRN_S_FLAG2, (1L << (thrown_spell - SRF2_OFFSET)), TRUE);
			lore_learn(m_ptr, LRN_CASTS, 0, TRUE);
		}

		/* Special spell */
		else if (thrown_spell < 32 * 6)
		{
 			lore_learn(m_ptr, LRN_S_FLAG3, (1L << (thrown_spell - SRF3_OFFSET)), TRUE);
			lore_learn(m_ptr, LRN_CASTS, 0, TRUE);
		}
	}

	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead) lore_learn(m_ptr, LRN_PDEATH, 0, TRUE);

	/* A spell was cast */
	return (TRUE);
}

/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players.  See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency.  Like not using "mod" or "div" when possible.  And
 * attempting to check the conditions in an optimal order.  Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
static int mon_will_run(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = get_monster_real(m_ptr);

	u16b p_lev, m_lev;
	u16b p_chp, p_mhp;
	u16b m_chp, m_mhp;
	u32b p_val, m_val;

	/* Keep monsters from running too far away */
	if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);

	/* Nearby monsters will not become terrified */
	if (m_ptr->cdis <= 5) return (FALSE);

	/* Examine player power (level) */
	p_lev = p_ptr->lev;

	/* Examine monster power (level plus morale) */
	m_lev = r_ptr->level + (m_idx & 0x08) + 25;

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4) return (FALSE);
	if (m_lev + 4 <= p_lev) return (TRUE);

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
	if (p_val * m_mhp > m_val * p_mhp) return (TRUE);

	/* Assume no terror */
	return (FALSE);
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
	int i, y, x, y1, x1;

	int when = 0;
	int cost = 999;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	/* Monster flowing disabled */
	if (!adult_flow_by_sound) return (FALSE);

	/* Monster can go through rocks */
	if (r_ptr->flags2 & (RF2_PASS_WALL)) return (FALSE);
	if (r_ptr->flags2 & (RF2_KILL_WALL)) return (FALSE);

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* The player is not currently near the monster grid */
	if (cave_when[y1][x1] < cave_when[p_ptr->py][p_ptr->px])
	{
		/* The player has never been near the monster grid */
		if (cave_when[y1][x1] == 0) return (FALSE);

		/* The monster is not allowed to track the player */
		if (!adult_flow_by_smell) return (FALSE);
	}

	/* Monster is too far away to notice the player */
	if (cave_cost[y1][x1] > MONSTER_FLOW_DEPTH) return (FALSE);
	if (cave_cost[y1][x1] > r_ptr->aaf) return (FALSE);

	/* Hack -- Player can see us, run towards him */
	if (player_has_los_bold(y1, x1)) return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		/* Get the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Ignore illegal locations */
		if (cave_when[y][x] == 0) continue;

		/* Ignore ancient locations */
		if (cave_when[y][x] < when) continue;

		/* Ignore distant locations */
		if (cave_cost[y][x] > cost) continue;

		/* Save the cost and time */
		when = cave_when[y][x];
		cost = cave_cost[y][x];

		/* Hack -- Save the "twiddled" location */
		(*yp) = p_ptr->py + 16 * ddy_ddd[i];
		(*xp) = p_ptr->px + 16 * ddx_ddd[i];
	}

	/* No legal move (?) */
	if (!when) return (FALSE);

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

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	/* Monster flowing disabled */
	if (!adult_flow_by_sound) return (FALSE);

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

	/* Monster is too far away to use flow information */
	if (cave_cost[fy][fx] > MONSTER_FLOW_DEPTH) return (FALSE);
	if (cave_cost[fy][fx] > r_ptr->aaf) return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		int dis, s;

		/* Get the location */
		y = fy + ddy_ddd[i];
		x = fx + ddx_ddd[i];

		/* Ignore illegal locations */
		if (cave_when[y][x] == 0) continue;

		/* Ignore ancient locations */
		if (cave_when[y][x] < when) continue;

		/* Calculate distance of this grid from our destination */
		dis = distance(y, x, y1, x1);

		/* Score this grid */
		s = 5000 / (dis + 3) - 500 / (cave_cost[y][x] + 1);

		/* No negative scores */
		if (s < 0) s = 0;

		/* Ignore lower scores */
		if (s < score) continue;

		/* Save the score and time */
		when = cave_when[y][x];
		score = s;

		/* Save the location */
		gy = y;
		gx = x;
	}

	/* No legal move (?) */
	if (!when) return (FALSE);

	/* Find deltas */
	(*yp) = fy - gy;
	(*xp) = fx - gx;

	/* Success */
	return (TRUE);
}

#endif /* MONSTER_FLOW */

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
 * coded sizes.  At the very least, code should be included which is
 * able to generate and dump these arrays (ala "los()").  XXX XXX XXX
 *
 * Also, the storage needs could be reduced by using char.  XXX XXX XXX
 *
 * These arrays could be combined into two big arrays, using sub-arrays
 * to hold the offsets and lengths of each portion of the sub-arrays, and
 * this could perhaps also be used somehow in the "look" code.  XXX XXX XXX
 */

static const int d_off_y_0[] =
{ 0 };

static const int d_off_x_0[] =
{ 0 };

static const int d_off_y_1[] =
{ -1, -1, -1, 0, 0, 1, 1, 1, 0 };

static const int d_off_x_1[] =
{ -1, 0, 1, -1, 1, -1, 0, 1, 0 };


static const int d_off_y_2[] =
{ -1, -1, -2, -2, -2, 0, 0, 1, 1, 2, 2, 2, 0 };

static const int d_off_x_2[] =
{ -2, 2, -1, 0, 1, -2, 2, -2, 2, -1, 0, 1, 0 };


static const int d_off_y_3[] =
{ -1, -1, -2, -2, -3, -3, -3, 0, 0, 1, 1, 2, 2,
  3, 3, 3, 0 };

static const int d_off_x_3[] =
{ -3, 3, -2, 2, -1, 0, 1, -3, 3, -3, 3, -2, 2,
  -1, 0, 1, 0 };


static const int d_off_y_4[] =
{ -1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4, 0,
  0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 0 };

static const int d_off_x_4[] =
{ -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, -4, 4,
  -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, 0 };


static const int d_off_y_5[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -4, -4, -5, -5,
  -5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5,
  5, 0 };

static const int d_off_x_5[] =
{ -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1, 0, 1,
  -5, 5, -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1,
  0, 1, 0 };


static const int d_off_y_6[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
  5, 5, 6, 6, 6, 0 };

static const int d_off_x_6[] =
{ -6, 6, -5, 5, -5, 5, -4, 4, -2, -3, 2, 3, -1,
  0, 1, -6, 6, -6, 6, -5, 5, -5, 5, -4, 4, -2,
  -3, 2, 3, -1, 0, 1, 0 };


static const int d_off_y_7[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, -6, -7, -7, -7, 0, 0, 1, 1, 2, 2, 3,
  3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 0 };

static const int d_off_x_7[] =
{ -7, 7, -6, 6, -6, 6, -5, 5, -4, -5, 4, 5, -2,
  -3, 2, 3, -1, 0, 1, -7, 7, -7, 7, -6, 6, -6,
  6, -5, 5, -4, -5, 4, 5, -2, -3, 2, 3, -1, 0,
  1, 0 };


static const int d_off_y_8[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -6, -6, -7, -7, -7, -7, -8, -8, -8, 0, 0, 1, 1,
  2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
  8, 8, 8, 0 };

static const int d_off_x_8[] =
{ -8, 8, -7, 7, -7, 7, -6, 6, -6, 6, -4, -5, 4,
  5, -2, -3, 2, 3, -1, 0, 1, -8, 8, -8, 8, -7,
  7, -7, 7, -6, 6, -6, 6, -4, -5, 4, 5, -2, -3,
  2, 3, -1, 0, 1, 0 };


static const int d_off_y_9[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -7, -7, -7, -7, -8, -8, -8, -8, -9, -9, -9, 0,
  0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7,
  7, 8, 8, 8, 8, 9, 9, 9, 0 };

static const int d_off_x_9[] =
{ -9, 9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4,
  -5, 4, 5, -2, -3, 2, 3, -1, 0, 1, -9, 9, -9,
  9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4, -5,
  4, 5, -2, -3, 2, 3, -1, 0, 1, 0 };


static const int *dist_offsets_y[10] =
{
	d_off_y_0, d_off_y_1, d_off_y_2, d_off_y_3, d_off_y_4,
	d_off_y_5, d_off_y_6, d_off_y_7, d_off_y_8, d_off_y_9
};

static const int *dist_offsets_x[10] =
{
	d_off_x_0, d_off_x_1, d_off_x_2, d_off_x_3, d_off_x_4,
	d_off_x_5, d_off_x_6, d_off_x_7, d_off_x_8, d_off_x_9
};

/*
 * Choose a "safe" location near a monster for it to run toward.
 *
 * A location is "safe" if it can be reached quickly and the player
 * is not able to fire into it (it isn't a "clean shot").  So, this will
 * cause monsters to "duck" behind walls.  Hopefully, monsters will also
 * try to run towards corridor openings if they are in a room.
 *
 * This function may take lots of CPU time if lots of monsters are fleeing.
 *
 * Return TRUE if a safe location is available.
 */
static bool find_safety(int m_idx, int *yp, int *xp)
{

#ifdef MONSTER_FLOW

	monster_type *m_ptr = &mon_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int i, y, x, dy, dx, d, dis;
	int gy = 0, gx = 0, gdis = 0;

	const int *y_offsets, *x_offsets;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Get the lists of points with a distance d from (fx, fy) */
		y_offsets = dist_offsets_y[d];
		x_offsets = dist_offsets_x[d];

		/* Check the locations */
		for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
		     dx != 0 || dy != 0;
		     i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Skip locations in a wall */
			if (!cave_floor_bold(y, x)) continue;

			/* Check for "availability" (if monsters can flow) */
			if (adult_flow_by_sound)
			{
				/* Ignore grids very far from the player */
				if (cave_when[y][x] < cave_when[p_ptr->py][p_ptr->px]) continue;

				/* Ignore too-distant grids */
				if (cave_cost[y][x] > cave_cost[fy][fx] + 2 * d) continue;
			}

			/* Check for absence of shot (more or less) */
			if (!player_has_los_bold(y,x))
			{
				/* Calculate distance from player */
				dis = distance(y, x, p_ptr->py, p_ptr->px);

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

#endif /* MONSTER_FLOW */

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
	monster_type *m_ptr = &mon_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int i, y, x, dy, dx, d, dis;
	int gy = 0, gx = 0, gdis = 999, min;

	const int *y_offsets, *x_offsets;

	/* Closest distance to get */
	min = distance(p_ptr->py, p_ptr->px, fy, fx) * 3 / 4 + 2;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Get the lists of points with a distance d from (fx, fy) */
		y_offsets = dist_offsets_y[d];
		x_offsets = dist_offsets_x[d];

		/* Check the locations */
		for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
		     dx != 0 || dy != 0;
		     i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Skip occupied locations */
			if (!cave_empty_bold(y, x)) continue;

			/* Check for hidden, available grid */
			if (!player_has_los_bold(y, x) && (clean_shot(fy, fx, y, x)))
			{
				/* Calculate distance from player */
				dis = distance(y, x, p_ptr->py, p_ptr->px);

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
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	int y, ay, x, ax;

	int move_val = 0;

	int y2 = p_ptr->py;
	int x2 = p_ptr->px;

	bool done = FALSE;

#ifdef MONSTER_FLOW

	/* Flow towards the player */
	if (adult_flow_by_sound)
	{
		/* Flow towards the player */
		(void)get_moves_aux(m_idx, &y2, &x2);
	}

#endif /* MONSTER_FLOW */

	/* Extract the "pseudo-direction" */
	y = m_ptr->fy - y2;
	x = m_ptr->fx - x2;

	/* Normal animal packs try to get the player out of corridors. */
	if (adult_smart_packs && (r_ptr->flags4 & RF4_ANIMAL) &&
	    !((r_ptr->flags2 & RF2_PASS_WALL) || (r_ptr->flags2 & RF2_KILL_WALL)) &&
		((r_ptr->flags1 & RF1_GRP_9) || (r_ptr->flags1 & RF1_GRP_18) || 
		 (r_ptr->flags1 & RF1_GRP_27)))
	{
		int i, room = 0;

		/* Count room grids next to player */
		for (i = 0; i < 8; i++)
		{
			/* Check grid */
			if (cave_info[p_ptr->py + ddy_ddd[i]][p_ptr->px + ddx_ddd[i]] & (CAVE_ROOM))
			{
				/* One more room grid */
				room++;
			}
		}

		/* Not in a room and strong player */
		if ((room < 8) && (p_ptr->chp > p_ptr->mhp / 2))
		{
			/* Find hiding place */
			if (find_hiding(m_idx, &y, &x)) done = TRUE;
		}
	}

	/* Apply fear */
	if (!done && mon_will_run(m_idx))
	{
		/* Try to find safe place */
		if (!(find_safety(m_idx, &y, &x)))
		{
			/* This is not a very "smart" method XXX XXX */
			y = (-y);
			x = (-x);
		}

#ifdef MONSTER_FLOW

		else
		{
			/* Attempt to avoid the player */
			if (adult_flow_by_sound)
			{
				/* Adjust movement */
				if (get_fear_moves_aux(m_idx, &y, &x)) done = TRUE;
			}
		}

#endif /* MONSTER_FLOW */

	}

	/* Monster groups try to surround the player */
	if (!done && adult_smart_packs && 
		((r_ptr->flags1 & RF1_GRP_9) || (r_ptr->flags1 & RF1_GRP_18) || 
		(r_ptr->flags1 & RF1_GRP_27)))
	{
		int i;

		/* Find an empty square near the player to fill */
		for (i = 0; i < 8; i++)
		{
			/* Pick squares near player (semi-randomly) */
			y2 = p_ptr->py + ddy_ddd[(m_idx + i) & 7];
			x2 = p_ptr->px + ddx_ddd[(m_idx + i) & 7];

			/* Already there? */
			if ((m_ptr->fy == y2) && (m_ptr->fx == x2))
			{
				/* Attack the player */
				y2 = p_ptr->py;
				x2 = p_ptr->px;

				break;
			}

			/* Ignore filled grids */
			if (!cave_empty_bold(y2, x2)) continue;

			/* Try to fill this hole */
			break;
		}

		/* Extract the new "pseudo-direction" */
		y = m_ptr->fy - y2;
		x = m_ptr->fx - x2;
	}

	/* Check for no move */
	if (!x && !y) return (FALSE);

	/* Extract the "absolute distances" */
	ax = ABS(x);
	ay = ABS(y);

	/* Do something weird */
	if (y < 0) move_val += 8;
	if (x > 0) move_val += 4;

	/* Prevent the diamond manuvere */
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
static int compare_monsters(const monster_type *m_ptr, const monster_type *n_ptr)
{
	monster_race *r_ptr;

	u32b mexp1, mexp2;

	/* Race 1 */
	r_ptr = get_monster_real(m_ptr);

	/* Extract mexp */
	mexp1 = r_ptr->mexp;

	/* Race 2 */
	r_ptr = get_monster_real(n_ptr);

	/* Extract mexp */
	mexp2 = r_ptr->mexp;

	/* Compare */
	if (mexp1 < mexp2) return (-1);
	if (mexp1 > mexp2) return (1);

	/* Assume equal */
	return (0);
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
 * will still stand there trying to open it...  XXX XXX XXX
 *
 * Technically, need to check for monster in the way combined
 * with that monster being in a wall (or door?) XXX
 */
static void monster_action(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	int i, d, oy, ox, ny, nx;

	int mm[5];

	bool xxx = FALSE;

	bool stagger;

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

	bool attack_stopped_by_cover;

	char m_name[80];

	/* Get the monster name */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Check if monster saw player this round */
	mon_not_see_player = check_player_visibility(m_idx);

	i = 0;

	/* Handle "sleep" */
	if (m_ptr->sleep)
	{
		u32b notice;

		/* Aggravation */
		if (p_ptr->aggravate)
		{
			/* Reset sleep counter */
			m_ptr->sleep = 0;

			/* Notice the "waking up" */
			if (m_ptr->ml)
			{
				/* Dump a message */
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s wakes up.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}

			/* No more action */
			return;
		}

		/* Anti-stealth */
		notice = rand_int(1024);

		/* Hack -- See if monster "notices" player */
		if ((notice * notice * notice) <= p_ptr->noise)
		{
			d = 1;

			/* Wake up faster near the player */
			if (m_ptr->cdis < 50) d = (100 / m_ptr->cdis);

			/* Still asleep */
			if (m_ptr->sleep > d)
			{
				/* Monster wakes up "a little bit" */
				m_ptr->sleep -= d;

				/* Notice the "not waking up" */
				lore_learn(m_ptr, LRN_IGNORES, 0, FALSE);
			}

			/* Just woke up */
			else
			{
				/* Reset sleep counter */
				m_ptr->sleep = 0;

				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s wakes up.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

					/* Hack -- Count the wakings */
					lore_learn(m_ptr, LRN_WAKES, 0, FALSE);
				}
			}
		}

		/* No more action (whether asleep or awake) */
		return;
	}

	/* Get the origin */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Stunned */
	if (m_ptr->stunned)
	{
		/* 50% chance of doing nothing */
		if (randint(100) < 50) return;
	}

	/* Attempt to "mutiply" if able and allowed */
	if ((r_ptr->flags1 & (RF1_MULTIPLY)) && (num_repro < MAX_REPRO))
	{
		int k, y, x;

		/* Count the adjacent monsters */
		for (k = 0, y = oy - 1; y <= oy + 1; y++)
		{
			for (x = ox - 1; x <= ox + 1; x++)
			{
				/* Count monsters */
				if (cave_m_idx[y][x] > 0) k++;
			}
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(m_idx, FALSE))
			{
				/* Take note if visible */
				lore_learn(m_ptr, LRN_FLAG1, RF1_MULTIPLY, FALSE);

				/* Multiplying takes energy */
				return;
			}
		}
	}

	/* Attempt to cast a spell */
	if (make_attack_spell(m_idx)) return;

	/* Check for player invisibility/monster blindness*/
	stagger = mon_not_see_player;

	/* Confused */
	if (m_ptr->confused)
	{
		/* Stagger */
		stagger = TRUE;
	}

	/* calmed - might move randomly */
	if ((m_ptr->calmed) && (rand_int(100) < 30))
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
				lore_learn(m_ptr, LRN_FLAG1, RF1_RAND_25, FALSE);

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
				lore_learn(m_ptr, LRN_FLAG1, RF1_RAND_50, FALSE);

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
				if (m_ptr->ml) lore_learn(m_ptr, LRN_FLAG1, (RF1_RAND_25 | RF1_RAND_50), FALSE);

				/* Stagger */
				stagger = TRUE;
			}
		}
	}

	/* Normal movement */
	if (!stagger)
	{
		/* Logical moves, may do nothing */
		if (!get_moves(m_idx, mm)) return;
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
	attack_stopped_by_cover = FALSE;

	/* Process moves */
	for (i = 0; i < 5; i++)
	{
		/* Get the direction (or stagger) */
		d = (stagger ? ddd[rand_int(8)] : mm[i]);

		/* Get the destination */
		ny = oy + ddy[d];
		nx = ox + ddx[d];

		/* Floor is open? */
		if (cave_floor_bold(ny, nx))
		{
			/* Go ahead and move */
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

		/* Quest Chest */
		else if (cave_feat[ny][nx] == FEAT_QST_CHEST)
		{
			/* Nothing */
		}

		/* Monster destroys walls (and doors) */
		else if (r_ptr->flags2 & RF2_KILL_WALL)
		{
			if (rand_int(100) < r_ptr->level * 2)
			{
				/* Eat through walls/doors/rubble */
				do_move = TRUE;

				/* Monster destroyed a wall */
				did_kill_wall = TRUE;

				/* Forget the wall */
				cave_info[ny][nx] &= ~(CAVE_MARK);

				/* Notice */
				cave_set_feat(ny, nx, FEAT_FLOOR);

				/* Check for decorations */
				if (decoration(ny, nx))
				{
					delete_trap(ny, nx);
				}

				/* Check for traps */
				if (trap_lock(ny, nx))
				{
					delete_trap(ny, nx);
				}

				/* Note changes to viewable region */
				if (player_has_los_bold(ny, nx)) do_view = TRUE;
			}
			/* Waste a turn */
			else do_turn = TRUE;
		}

		/* Handle doors and secret doors */
		else if ((cave_feat[ny][nx] == FEAT_CLOSED) ||
		         (cave_feat[ny][nx] == FEAT_SECRET))
		{
			bool may_bash = TRUE;

			/* Take a turn */
			do_turn = TRUE;

			/* Creature can open doors. */
			if (r_ptr->flags2 & (RF2_OPEN_DOOR))
			{
				/* Locked doors (not jammed) */
				if (trap_lock(ny, nx) && trap_monster(ny, nx))
				{
					int k;
					trap_type *t_ptr = &t_list[cave_t_idx[ny][nx]];
					int mon_power = r_ptr->level;

					if (r_ptr->flags2 & RF2_PICK_LOCK) mon_power *= 3;

					/* Door power */
					k = (t_ptr->charges);

					/* Try to unlock it XXX XXX XXX */
					if (rand_int(r_ptr->level) > k)
					{
						/* The door is open */
						did_open_door = TRUE;

						/* Do not bash the door */
						may_bash = FALSE;
					}
				}
				else
				{
					/* The door is open */
					did_open_door = TRUE;

					/* Do not bash the door */
					may_bash = FALSE;
				}
			}

			/* Stuck doors -- attempt to bash them down if allowed */
			if (may_bash && (r_ptr->flags2 & (RF2_BASH_DOOR)))
			{
 				int mon_power = r_ptr->level;

				/* Reduce monster power depending on health */
				mon_power *= m_ptr->hp;
				mon_power /= m_ptr->maxhp;

				if (mon_power < 1) mon_power = 1;

				/* Locked doors (not jammed) */
				if (trap_lock(ny, nx) && trap_monster(ny, nx))
				{
					int k;
					trap_type *t_ptr = &t_list[cave_t_idx[ny][nx]];

					/* Door power */
					k = t_ptr->charges * 2;
				}

				/* Attempt to Bash XXX XXX XXX */
				if (rand_int(mon_power) > 5 + rand_int(1000))
				{
					/* Message */
					message(MSG_GENERIC, 0, "You hear a door burst open!");

					/* Disturb (sometimes) */
					if (disturb_minor) disturb(0);

					/* The door was bashed open */
					did_bash_door = TRUE;

					/* Hack -- fall into doorway */
					do_move = TRUE;
				}
			}

			/* Deal with doors in the way */
			if (did_open_door || did_bash_door)
			{
				/* Break down the door */
				if (did_bash_door && (rand_int(100) < 50))
				{
					cave_set_feat(ny, nx, FEAT_FLOOR);

					/* Remove the lock */
					delete_trap(ny, nx);
				}

				/* Handle decorated doors properly */
				else if ((t_list[cave_t_idx[ny][nx]].w_idx == WG_SHELF_CLOSED_DOOR) ||
					(t_list[cave_t_idx[ny][nx]].w_idx == WG_SHELF_SECRET_DOOR))
				{
					place_decoration(ny, nx, WG_SHELF_OPEN_DOOR);
				}
				else if ((t_list[cave_t_idx[ny][nx]].w_idx == WG_PAINTING_CLOSED_DOOR) ||
					(t_list[cave_t_idx[ny][nx]].w_idx == WG_PAINTING_SECRET_DOOR))
				{
					place_decoration(ny, nx, WG_PAINTING_OPEN_DOOR);
				}
				else if ((t_list[cave_t_idx[ny][nx]].w_idx == WG_RACK_CLOSED_DOOR) ||
					(t_list[cave_t_idx[ny][nx]].w_idx == WG_RACK_SECRET_DOOR))
				{
					place_decoration(ny, nx, WG_RACK_OPEN_DOOR);
				}
				else if ((t_list[cave_t_idx[ny][nx]].w_idx == WG_CLOSET_CLOSED_DOOR) ||
					(t_list[cave_t_idx[ny][nx]].w_idx == WG_CLOSET_SECRET_DOOR))
				{
					place_decoration(ny, nx, WG_CLOSET_OPEN_DOOR);
				}

				/* Open the door */
				else
				{
					cave_set_feat(ny, nx, FEAT_OPEN);

					/* Remove the lock */
					delete_trap(ny, nx);
				}

				/* Handle viewable doors */
				if (player_has_los_bold(ny, nx)) do_view = TRUE;
			}
		}

		/* Check for terrain that is difficult to move over */
		/* Also check melee attack penalties due to cover at this time */
		switch (t_list[cave_t_idx[ny][nx]].w_idx)
		{
			case WG_VEGETATION:
			case WG_INTERESTING_VEGETATION:
			{
				if (r_ptr->flags2 & (RF2_NEVER_MOVE)) break;

				/* Vegetation doesn't hinder attacking */
				if (cave_m_idx[ny][nx] < 0)
				{
				}
				/* Earthbound monsters can't jump */
				else if (m_ptr->earthbound)
				{
					do_move = FALSE;
				}
				/* Some monsters pass through vegetation easily */
				else if (r_ptr->flags2 & (RF2_PASS_VEGETATION))
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_PASS_VEGETATION, TRUE);
				 	break;
				}
				/* Some monsters fly over obstacles */
				else if (r_ptr->flags2 & (RF2_FLYING))
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_FLYING, TRUE);
				 	break;
				}
				/* Some monsters are bad at jumping*/
				else if ((r_ptr->flags2 & (RF2_BAD_JUMPER)) && (rand_int(100) < 80))
				{
					do_move = FALSE;

					/* Took a turn */
					do_turn = TRUE;
				}
				else if ((rand_int(100) < 60) && (do_move))
				{
					do_move = FALSE;

					/* Took a turn */
					do_turn = TRUE;
				}
				else if (do_move)
				{
					if (m_ptr->ml)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s hops over some bushes.", m_name);
					}
				}
				break;
			}
			case WG_SPIKES:
			{
				if (r_ptr->flags2 & (RF2_NEVER_MOVE)) break;

				/* Spikes do not hinder attacking */
				if (cave_m_idx[ny][nx] < 0)
				{
				}
				/* Earthbound monsters can't jump */
				else if (m_ptr->earthbound)
				{
					cptr note_dies = " dies.";

					if (rand_int(100) < 85)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s steps on a spike.", m_name);
					}
					else
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s steps on spikes, destroying them.", m_name);
						delete_trap(ny, nx);
					}

					if (mon_take_hit(m_idx, (randint(10)), &xxx, note_dies))
					{
						break;
					}

					/* Poison */
					if (!(r_ptr->flags3 & RF3_RES_POIS))
					{
						/* Already partially poisoned */
						if (m_ptr->poisoned)
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is more poisoned.", m_name);
						}
						/* Was not poisoned */
						else
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is poisoned.", m_name);
						}

						m_ptr->poisoned += randint(30) + 10;
					}
				}
				/* Some monsters pass through vegetation easily */
				else if (r_ptr->flags2 & (RF2_PASS_VEGETATION))
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_PASS_VEGETATION, TRUE);
				 	break;
				}
				/* Some monsters fly over obstacles */
				else if (r_ptr->flags2 & (RF2_FLYING))
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_FLYING, TRUE);
				 	break;
				}
				/* Some monsters are bad at jumping*/
				else if ((r_ptr->flags2 & (RF2_BAD_JUMPER)) && (rand_int(100) < 80))
				{
					cptr note_dies = " dies.";

					if (rand_int(100) < 85)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s steps on a spike.", m_name);
					}
					else
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s steps on spikes, destroying them.", m_name);
						delete_trap(ny, nx);
					}

					if (mon_take_hit(m_idx, (randint(10)), &xxx, note_dies))
					{
						break;
					}

					/* Poison */
					if (!(r_ptr->flags3 & RF3_RES_POIS))
					{
						/* Already partially poisoned */
						if (m_ptr->poisoned)
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is more poisoned.", m_name);
						}
						/* Was not poisoned */
						else
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is poisoned.", m_name);
						}

						m_ptr->poisoned += randint(30) + 10;
					}
				}
				else if ((rand_int(100) < 60) && (do_move))
				{
					cptr note_dies = " dies.";

					if (rand_int(100) < 85)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s steps on a spike.", m_name);
					}
					else
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s steps on spikes, destroying them.", m_name);
						delete_trap(ny, nx);
					}

					if (mon_take_hit(m_idx, (randint(10)), &xxx, note_dies))
					{
						break;
					}

					/* Poison */
					if (!(r_ptr->flags3 & RF3_RES_POIS))
					{
						/* Already partially poisoned */
						if (m_ptr->poisoned)
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is more poisoned.", m_name);
						}
						/* Was not poisoned */
						else
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is poisoned.", m_name);
						}

						m_ptr->poisoned += randint(30) + 10;
					}
				}
				else if (do_move)
				{
					if (m_ptr->ml)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s hops over spikes.", m_name);
					}
				}
				break;
			}
			case WG_TABLE:
			case WG_PLATFORM:
			{
				/* Some monsters fly over obstacles. Mark the monster memory. */
				if ((r_ptr->flags2 & (RF2_FLYING)) && (!(m_ptr->earthbound)))
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_FLYING, TRUE);
				 	break;
				}
				if (t_list[cave_t_idx[oy][ox]].w_idx == WG_TABLE) break;
				if (t_list[cave_t_idx[oy][ox]].w_idx == WG_PLATFORM) break;
				if ((t_list[cave_t_idx[oy][ox]].w_idx >= WG_ALTAR_OBSESSION) &&
					(t_list[cave_t_idx[oy][ox]].w_idx <= WG_ALTAR_DECEIT)) break;

				/* First check whether lower ground hinders attacking */
				if (cave_m_idx[ny][nx] < 0)
				{
					if (rand_int(5) < 2)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s tries to attack but you easily dodge it.", m_name);
						attack_stopped_by_cover = TRUE;
					}
				}
				/* Monster cannot move */
				else if (r_ptr->flags2 & (RF2_NEVER_MOVE)) break;
				/* Climbing steps never fails */
				else if (t_list[cave_t_idx[oy][ox]].w_idx == WG_STEPS) break;
				/* Earthbound monsters can't jump */
				else if (m_ptr->earthbound)
				{
					do_move = FALSE;
				}
				/* Some monsters are bad at jumping */
				else if ((r_ptr->flags2 & (RF2_BAD_JUMPER)) && (rand_int(100) < 80))
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_BAD_JUMPER, TRUE);
					do_move = FALSE;

					/* Took a turn */
					do_turn = TRUE;
				}
				/* Check whether movement is stopped */
				else if ((rand_int(100) < 60) && (do_move))
				{
					do_move = FALSE;

					/* Took a turn */
					do_turn = TRUE;
				}
				else if ((t_list[cave_t_idx[ny][nx]].w_idx == WG_TABLE) && (do_move))
				{
					if (m_ptr->ml)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s hops on the table.", m_name);
					}
				}
				else if ((t_list[cave_t_idx[ny][nx]].w_idx == WG_PLATFORM) && (do_move))
				{
					if (m_ptr->ml)
					{
						message_format(MSG_MONSTER, m_ptr->r_idx, "%^s hops on the platform.", m_name);
					}
				}

				break;
			}
		}

		/* Check for protection circles and Glyph of Warding (and traps) */
		if (do_move && (trap_glyph(ny, nx)))
		{
			int glyph_power;

			/* Check if a relevant glyph type */
			if (mon_glyph_check(m_idx, ny, nx))
			{
				/* Monsters can often attack player on the edge of protection circles, but not on glyphs */
				if (!((cave_m_idx[ny][nx] < 0) && (decoration(ny, nx))))
				{
					do_move = FALSE;

					/* Took a turn */
					do_turn = TRUE;
				}
				else
				{
					if (rand_int(100) < 50)
					{
						do_move = FALSE;
						do_turn = FALSE;

						/* Message */
						if (r_ptr->flags4 & RF4_ANIMAL)
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s fights with an illusionary image of you.", m_name);
						}
						if (r_ptr->flags4 & RF4_PERSON)
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s fights with an illusionary image of you.", m_name);
						}
						if (r_ptr->flags4 & RF4_DEMON)
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s suddenly feels very weak.", m_name);
						}
						if (r_ptr->flags4 & RF4_UNDEAD)
						{
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s suddenly feels very weak.", m_name);
						}
					}
				}

				if (t_list[cave_t_idx[ny][nx]].w_idx == WG_GLYPH_LESSER) glyph_power = 150;
					else glyph_power = 550;
			}
			/* If not affected by the glyph, a good chance of breaking it */
			else glyph_power = 250;

			if (adult_nightmare_mode) glyph_power /= 2;

			/* Break the ward. Protection circles never break. */
			if ((randint(glyph_power) < r_ptr->level) && (!(decoration(ny, nx))))
			{
				/* Describe observable breakage */
				if (cave_info[ny][nx] & (CAVE_MARK))
				{
					message(MSG_GENERIC, 0, "The magic of the glyph has faded!");
				}

				/* Forget the rune */
				cave_info[ny][nx] &= ~(CAVE_MARK);

				/* Delete glyph */
				delete_trap(ny, nx);

				/* Allow movement */
				do_move = TRUE;
			}

			/* Small chance of moving on a protection circle */
			if ((decoration(ny, nx)) && (rand_int(100) < 20))
			{
				do_move = TRUE;
			}
		}

		/* Some monsters never attack */
		if (do_move && (cave_m_idx[ny][nx] < 0) && (r_ptr->flags2 & (RF2_NEVER_BLOW)))
		{
			/* Hack -- memorize lack of attacks */
			lore_learn(m_ptr, LRN_FLAG2, RF2_NEVER_BLOW, FALSE);

			/* Do not move */
			do_move = FALSE;
		}

		/* calmed monsters never attack */
		if (do_move && (cave_m_idx[ny][nx] < 0) && m_ptr->calmed)
		{
			/* Do not move */
			do_move = FALSE;
		}

		/* The player is in the way.  Attack him. */
		if (do_move && (cave_m_idx[ny][nx] < 0))
		{
			/* Don't attack if the attack was stopped by cover */
			if (!attack_stopped_by_cover)
			{
				/* Do the attack */
				(void)make_attack_normal(m_idx);
			}

			/* Do not move */
			do_move = FALSE;

			/* Took a turn */
			do_turn = TRUE;
		}

		/* Some monsters never move */
		if (do_move && (r_ptr->flags2 & (RF2_NEVER_MOVE)))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) lore_learn(m_ptr, LRN_FLAG2, RF2_NEVER_MOVE, FALSE);

			/* Do not move */
			do_move = FALSE;
		}

		/* A monster is in the way */
		if (do_move && (cave_m_idx[ny][nx] > 0))
		{
			monster_type *n_ptr = &mon_list[cave_m_idx[ny][nx]];

			/* Assume no movement */
			do_move = FALSE;

			/* Kill weaker monsters */
			if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
			    (compare_monsters(m_ptr, n_ptr) > 0))
			{
				/* Allow movement */
				do_move = TRUE;

				/* Update visuals */
				p_ptr->window |= (PW_VISIBLE);

				/* Monster ate another monster */
				did_kill_body = TRUE;

				/* Message XXX XXX XXX */

				/* Kill the monster */
				delete_monster(ny, nx);
			}
		}

		/* A monster is in the way */
		if (do_move && (cave_m_idx[ny][nx] > 0))
		{
			monster_type *n_ptr = &mon_list[cave_m_idx[ny][nx]];

			/* Assume no movement */
			do_move = FALSE;

			/* Push past weaker monsters (unless leaving a wall) */
			if ((r_ptr->flags2 & (RF2_MOVE_BODY)) &&
			    (compare_monsters(m_ptr, n_ptr) > 0) &&
			    (cave_floor_bold(m_ptr->fy, m_ptr->fx)))
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

			/* Possible disturb */
			if (m_ptr->ml &&
			    (disturb_move || ((m_ptr->mflag & (MFLAG_VIEW)) && disturb_near)))
			{
				/* Disturb */
				disturb(0);
			}

			/* If he carries a light, update lights */
			if ((view_monster_lite) && (r_ptr->flags2 & (RF2_HAS_LITE))) do_view = TRUE;

			/*
			 * Get hit by Warding Runes? Check all four cardinal directions
			 **/
			int i = 0;
			int x_add = 0;
			int y_add = 0;
			int distance = 0;

			/* Obtain monster info */
			int resist = 1;
			if (m_ptr->u_idx) resist = 4;

			/* Calculate success chance */
			int plev = p_ptr->lev;
			int dam = ((plev > 12) ? plev : 6 + plev/2);
			int success_chance = 100 - (100*(r_ptr->level + resist + 1)) / ((dam > 5) ? dam * 2 : 10);

			for (i = 1; i < 5; i++)
			{
				switch (i)
				{
					case 1: y_add = 1; x_add = 0; break;
					case 2: y_add = -1; x_add = 0; break;
					case 3: x_add = 1; y_add = 0; break;
					case 4: x_add = -1; y_add = 0; break;
				}

				for (distance = 1; distance <= MAX_SIGHT; distance++)
				{
					/* Don't continue outside rooms */
					if (!(cave_info[ny][nx] & (CAVE_ROOM))) break;

					switch (t_list[cave_t_idx[ny+(y_add * distance)][nx+x_add * distance]].w_idx)
					{
						case WG_WARD_SLUMBER_ACTIVE_HIDDEN:
						{
							if (m_ptr->ml) place_decoration(ny+(y_add * distance), nx+(x_add * distance), WG_WARD_SLUMBER_ACTIVE);
							/* Fall through */
						}
						case WG_WARD_SLUMBER_ACTIVE:
						case WG_WARD_SLUMBER_ACTIVE_MASTERED:
						case WG_WARD_SLUMBER_ACTIVE_INCOMPREHENSIBLE:
						{
							if ((r_ptr->flags3 & (RF3_NO_SLEEP)) || (!(success_chance > rand_int(100))))
							{
								/* Memorize a flag */
								if (r_ptr->flags3 & (RF3_NO_SLEEP))
								{
									lore_learn(m_ptr, LRN_FLAG3, RF3_NO_SLEEP, FALSE);
								}

								if (m_ptr->ml)
								{
									message_format(MSG_EFFECT, m_ptr->r_idx, "A Rune of Slumber flashes, but %s resists!", m_name);
								}
							}
							else 
							{
								if (m_ptr->ml)
								{
									message_format(MSG_EFFECT, m_ptr->r_idx, "A Rune of Slumber flashes. %^s falls asleep!", m_name);
								}
								m_ptr->sleep = 500;
							}

							break;
						}
						case WG_WARD_TERROR_ACTIVE_HIDDEN:
						{
							if (m_ptr->ml) place_decoration(ny+(y_add * distance), nx+(x_add * distance), WG_WARD_TERROR_ACTIVE);
							/* Fall through */
						}
						case WG_WARD_TERROR_ACTIVE:
						case WG_WARD_TERROR_ACTIVE_MASTERED:
						case WG_WARD_TERROR_ACTIVE_INCOMPREHENSIBLE:
						{
							if ((r_ptr->flags3 & (RF3_NO_FEAR)) || (!(success_chance > rand_int(100))))
							{
								/* Memorize a flag */
								if (r_ptr->flags3 & (RF3_NO_FEAR))
								{
									lore_learn(m_ptr, LRN_FLAG3, RF3_NO_FEAR, FALSE);
								}

								if (m_ptr->ml)
								{
									message_format(MSG_EFFECT, m_ptr->r_idx, "A Rune of Terror flashes, but %s resists!", m_name);
								}
							}
							else 
							{
								int do_fear = damroll(3, (dam / 2)) + 1;

								/* Increase fear */
								int tmp = m_ptr->monfear + do_fear;

								/* Set fear */
								m_ptr->monfear = (tmp < 200) ? tmp : 200;

								if (m_ptr->ml)
								{
									message_format(MSG_EFFECT, m_ptr->r_idx, "A Rune of Terror flashes. %^s flees in terror!", m_name);
								}
							}

							break;
						}
						case WG_WARD_CHANGE_ACTIVE_HIDDEN:
						{
							if (m_ptr->ml) place_decoration(ny+(y_add * distance), nx+(x_add * distance), WG_WARD_CHANGE_ACTIVE);
							/* Fall through */
						}
						case WG_WARD_CHANGE_ACTIVE:
						case WG_WARD_CHANGE_ACTIVE_MASTERED:
						case WG_WARD_CHANGE_ACTIVE_INCOMPREHENSIBLE:
						{
							if ((!(success_chance > rand_int(100))) || (m_ptr->u_idx))
							{
								if (m_ptr->ml)
								{
									message_format(MSG_EFFECT, m_ptr->r_idx, "A Rune of Change flashes, but %s resists!", m_name);
								}
							}
							else
							{
								if (m_ptr->ml)
								{
									message_format(MSG_EFFECT, m_ptr->r_idx, "A Rune of Change flashes. %^s changes!", m_name);
								}

								/* Pick a "new" monster race */
								int tmp = poly_r_idx(m_ptr->r_idx, dam);

								/* "Kill" the "old" monster */
								delete_monster_idx(cave_m_idx[ny][nx]);

								/* Create a new monster (no groups) */
								(void)place_monster_aux(ny, nx+0, tmp, 0, FALSE, FALSE, PLACE_NO_UNIQUE);

								/* Hack -- Assume success XXX XXX XXX */

								/* Hack -- Get new monster */
								m_ptr = &mon_list[cave_m_idx[ny][nx]];

								/* Hack -- Get new race */
								r_ptr = &r_info[m_ptr->r_idx];
							}

							break;
						}
						case WG_WARD_DEATH_ACTIVE_HIDDEN:
						{
							if (m_ptr->ml) place_decoration(ny+(y_add * distance), nx+(x_add * distance), WG_WARD_DEATH_ACTIVE);
							/* Fall through */
						}
						case WG_WARD_DEATH_ACTIVE:
						case WG_WARD_DEATH_ACTIVE_MASTERED:
						case WG_WARD_DEATH_ACTIVE_INCOMPREHENSIBLE:
						{
							if (r_ptr->flags4 & (RF3_NO_CUT))
							{
								if (m_ptr->ml)
								{
									message_format(MSG_EFFECT, m_ptr->r_idx, "Rune of Blood flashes. %^s is immune!", m_name);
									lore_learn(m_ptr, LRN_FLAG3, RF3_NO_CUT, FALSE);
								}
							}
							else
							{
								/* Already bleeding */
								if (m_ptr->bleeding)
								{
									if (m_ptr->ml)
									{
										message_format(MSG_EFFECT, m_ptr->r_idx, "Rune of Blood flashes. %^s is bleeding more strongly.", m_name);
									}
								}
								/* Was not bleeding */
								else
								{
									if (m_ptr->ml)
									{
										message_format(MSG_EFFECT, m_ptr->r_idx, "Rune of Blood flashes. %^s is bleeding.", m_name);
									}
								}

								int cut = rand_int(50) + (1.5 * p_ptr->depth);
								cut = rand_int(50) + 30;
								m_ptr->bleeding += cut;
							}

							break;
						}
						case WG_WARD_CURSING_ACTIVE_HIDDEN:
						{
							if (m_ptr->ml) place_decoration(ny+(y_add * distance), nx+(x_add * distance), WG_WARD_CURSING_ACTIVE);
							/* Fall through */
						}
						case WG_WARD_CURSING_ACTIVE:
						case WG_WARD_CURSING_ACTIVE_MASTERED:
						case WG_WARD_CURSING_ACTIVE_INCOMPREHENSIBLE:
						{
							if ((!(success_chance > rand_int(100))) || (m_ptr->cursed))
							{
								if (m_ptr->ml)
								{
									message_format(MSG_EFFECT, m_ptr->r_idx, "A Rune of Evil Eye flashes but %s resists!", m_name);
								}
							}
							else 
							{
								/* Set fear */
								m_ptr->cursed = 1;

								if (m_ptr->ml)
								{
									message_format(MSG_FLEE, m_ptr->r_idx, "A Rune of Evil Eye flashes. %^s is cursed!", m_name);
								}
							}

							break;
						}
					}

					/* Don't continue through walls */
					if (!cave_floor_bold((ny + (y_add * distance)), (nx + (x_add * distance)))) break;
				}
			}

			/*
			 * Monster traps, taken from Oangband for now.
			 */
			if ((trap_monster(ny, nx)) && !(trap_lock(ny, nx)) && !(trap_glyph(ny, nx)))
			{
				if ((r_ptr->flags1 & (RF1_SMART)) && (randint(3) == 1))
				{
					if (m_ptr->ml)
					{
						char m_name[80];

						/* Acquire the monster name */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0);

						message_format(MSG_MISS, m_ptr->r_idx, "%^s finds your trap and disarms it.", m_name);
					}

					/* Kill the trap */
					delete_trap(ny, nx);
				}

				/* Traps seldom affect flying monsters or ghosts. */
				else if ((r_ptr->flags2 & (RF2_PASS_WALL)) && (rand_int(4) != 0))
				{
					if (m_ptr->ml)
					{
						char m_name[80];

						/* Acquire the monster name/poss */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0);

						message_format(MSG_MISS, m_ptr->r_idx, "%^s passes through your trap.", m_name);
					}
				}

				/* I thought traps only affected players!  Unfair! */
				else
				{
					/* Assume a default death */
					cptr note_dies = " dies.";

					/* Some monsters get "destroyed" */
					if (!monster_alive(TRUE, m_ptr)) note_dies = " is destroyed.";

					/* Player is in line of sight */
					if (player_has_los_bold(ny, nx))
					{
						char m_name[80];

						/* Acquire the monster name/poss */
						if (m_ptr->ml) monster_desc(m_name, sizeof(m_name), m_ptr, 0);

						/* Default name */
						else strcpy(m_name, "Something");

						message_format(MSG_HIT, m_ptr->r_idx, "%^s sets off your cunning trap!", m_name);

						t_list[cave_t_idx[ny][nx]].visible = TRUE;
					}

					/* Monster is not in LOS */
					else message(MSG_HIT, 0, "You hear anguished yells in the distance.");

					/* Sometimes the trap is destroyed. */
					if (rand_int(3) == 0)
					{
						delete_trap(ny, nx);
					}

					/* Hurt the monster.  Big bruisers fall hard. */
					/* 
					 * Changed proportional damage
					 * to be based on current
					 * hitpoints but increased the 
					 * fraction
					 */
					if (mon_take_hit(m_idx, (1 + randint(p_ptr->lev * 3) 
						+ m_ptr->hp / 15), &xxx, note_dies))
					{
						return;
					}
				}
			}

			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[ny][nx]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Skip gold */
				if (o_ptr->tval == TV_GOLD) continue;

				/* Take or Kill objects on the floor */
				if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
				    (r_ptr->flags2 & (RF2_KILL_ITEM)))
				{
					byte slays[SL_MAX];

					u32b flg2 = 0L;
					u32b flg4 = 0L;

					char m_name[80];
					char o_name[80];

					/* Get the object name */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, (display_insc_msg ? 3 : 2));

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);

					/* Get the weapon bonuses */
					weapon_slays(o_ptr, slays);

					/* React to objects that hurt the monster */
					if (slays[SL_ANTI_HUMANOID])	flg4 |= (RF4_HUMANOID);
					if (slays[SL_ANTI_DRAGON])		flg4 |= (RF4_DRAGON);
					if (slays[SL_ANTI_PERSON])		flg4 |= (RF4_PERSON);
					if (slays[SL_ANTI_UNDEAD])		flg4 |= (RF4_UNDEAD);
					if (slays[SL_ANTI_ANIMAL])		flg4 |= (RF4_ANIMAL);
					if (slays[SL_ANTI_PLANT])		flg4 |= (RF4_PLANT);
					if (slays[SL_ANTI_FAERY])		flg4 |= (RF4_FAERY);
					if (slays[SL_ANTI_DEMON])		flg4 |= (RF4_DEMON);
					if (slays[SL_ANTI_CHAOS])		flg4 |= (RF4_CHAOS);
					if (slays[SL_ANTI_EVIL])		flg4 |= (RF4_EVIL);
					if (slays[SL_BRAND_ELEC])		flg2 |= (RF2_HURT_ELEC);
					if (slays[SL_BRAND_ACID])		flg2 |= (RF2_HURT_ACID);
					if (slays[SL_BRAND_FIRE])		flg2 |= (RF2_HURT_FIRE);
					if (slays[SL_BRAND_COLD])		flg2 |= (RF2_HURT_COLD);
					if (slays[SL_BRAND_LITE])		flg2 |= (RF2_HURT_LITE);
					if (slays[SL_BRAND_DARK])		flg2 |= (RF2_HURT_DARK);

					/* The object cannot be picked up by the monster */
					if (o_ptr->a_idx || (o_ptr->tval == TV_QUEST) || 
						(r_ptr->flags4 & flg4) || (r_ptr->flags2 & flg2))
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
								message_format(MSG_MON_FAIL, m_ptr->r_idx, "%^s tries to pick up %s, but fails.",
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
						if (player_has_los_bold(ny, nx))
						{
							/* Dump a message */
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s picks up %s.", m_name, o_name);
						}

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain local object */
						object_copy(i_ptr, o_ptr);

						/* Delete the object */
						delete_object_idx(this_o_idx);

						/* Carry the object */
						(void)monster_carry(m_idx, i_ptr);
					}

					/* Destroy the item */
					else
					{
						/* Take note */
						did_kill_item = TRUE;

						/* Describe observable situations */
						if (player_has_los_bold(ny, nx))
						{
							/* Dump a message */
							message_format(MSG_MONSTER, m_ptr->r_idx, "%^s crushes %s.", m_name, o_name);
						}

						/* Delete the object */
						delete_object_idx(this_o_idx);
					}
				}
			}
		}

		/* Stop when done */
		if (do_turn) break;
	}

	/* If we haven't done anything, try casting a spell again */
	if (!do_turn && !do_move)
	{
		/* Cast spell */
		if (make_attack_spell(m_idx)) return;
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
		{
			lore_learn(m_ptr, LRN_FLAG2, RF2_OPEN_DOOR, FALSE);
			if (r_ptr->flags2 & RF2_PICK_LOCK) 
				lore_learn(m_ptr, LRN_FLAG2, RF2_PICK_LOCK, FALSE);
		}

		/* Monster bashed a door */
		if (did_bash_door) lore_learn(m_ptr, LRN_FLAG2, RF2_BASH_DOOR, FALSE);

		/* Monster tried to pick something up */
		if (did_take_item) lore_learn(m_ptr, LRN_FLAG2, RF2_TAKE_ITEM, FALSE);

		/* Monster tried to crush something */
		if (did_kill_item) lore_learn(m_ptr, LRN_FLAG2, RF2_KILL_ITEM, FALSE);

		/* Monster pushed past another monster */
		if (did_move_body) lore_learn(m_ptr, LRN_FLAG2, RF2_MOVE_BODY, FALSE);

		/* Monster ate another monster */
		if (did_kill_body) lore_learn(m_ptr, LRN_FLAG2, RF2_KILL_BODY, FALSE);

		/* Monster passed through a wall */
		if (did_pass_wall) lore_learn(m_ptr, LRN_FLAG2, RF2_PASS_WALL, FALSE);

		/* Monster destroyed a wall */
		if (did_kill_wall) lore_learn(m_ptr, LRN_FLAG2, RF2_KILL_WALL, FALSE);
	}

	/* Hack -- get "bold" if out of options */
	if (!do_turn && !do_move && m_ptr->monfear)
	{
		if (rand_int(6) < 1)
		{
			/* No longer afraid */
			m_ptr->monfear = 0;

			/* Message if seen */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s turns to fight!", m_name);
			}
		}
		else
		{
			/* Spend the turn doing nothing */
			do_turn = TRUE;

			/* Message if seen */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s trembles in fear.", m_name);
			}
		}
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
 * in this function, and its child, "monster_action()".
 *
 * (Note - the above was written before the split of status/action handling.)
 *
 * Most of the rest of the time is spent in "update_view()" and "lite_spot()",
 * especially when the player is running.
 */
void process_monsters_action(byte minimum_energy)
{
	int i;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Process the monsters (backwards) */
	for (i = mon_max - 1; i >= 1; i--)
	{
		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Get the monster */
		m_ptr = &mon_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Not enough energy to move */
		if (m_ptr->energy < minimum_energy) continue;

		/* Use up "some" energy */
		m_ptr->energy -= 100;

		/* Get the race */
		r_ptr = get_monster_real(m_ptr);

		/* Monsters can "sense" the player */
		if (m_ptr->cdis <= r_ptr->aaf)
		{
			/* Process the monster */
			monster_action(i);

			/* Continue */
			continue;
		}

		/* Monsters can "see" the player (backwards) XXX XXX */
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			/* Process the monster */
			monster_action(i);

			/* Continue */
			continue;
		}

#ifdef MONSTER_FLOW

		/* Hack -- Monsters can "smell" the player from far away */
		if (adult_flow_by_sound)
		{
			/* Check the flow (normal aaf is about 20) */
			if ((cave_when[m_ptr->fy][m_ptr->fx] == cave_when[p_ptr->py][p_ptr->px]) &&
			    (cave_cost[m_ptr->fy][m_ptr->fx] < MONSTER_FLOW_DEPTH) &&
			    (cave_cost[m_ptr->fy][m_ptr->fx] < r_ptr->aaf))
			{
				/* Process the monster */
				monster_action(i);

				/* Continue */
				continue;
			}
		}

#endif /* MONSTER_FLOW */

	}
}

/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), energizing each
 * monster, and allowing fully energized monsters to move, attack, pass, etc.
 *
 * Handle monster "status".
 * Note that "sleeping" monsters waking up isn't handled here, because it counts
 * as an "action"; therefore, it is handled within monster_action(). However,
 * monsters falling back to sleep is handled here.
 */
void process_monsters_status(void)
{
	int i, d;

	monster_type *m_ptr;
	monster_race *r_ptr;

	bool xxx = FALSE;

	/* Every 10 game turns */
	if (turn % 10) return;

	/* Handle "leaving" */
	if (p_ptr->leaving) return;

	/* Process the monsters (backwards) */
	for (i = mon_max - 1; i >= 1; i--)
	{
		/* Get the monster */
		m_ptr = &mon_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Get the race */
		r_ptr = get_monster_real(m_ptr);

		/* Allow regeneration (if needed) */
		if (m_ptr->hp < m_ptr->maxhp)
		{
			int regen_rate = BASE_MON_REGEN;

			/* Regenerating monsters */
			if (r_ptr->flags2 & RF2_REGENERATE) regen_rate /= 2;

			/* Check for regeneration */
			if (!(turn % regen_rate))
			{
				/* Hack -- Base regeneration */
				d = m_ptr->maxhp / 100;

				/* Hack -- Minimal regeneration rate */
				if (!d) d = 1;

				/* Hack -- Regenerate */
				m_ptr->hp += d;

				/* Do not over-regenerate */
				if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

				/* Redraw (later) if needed */
				if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
			}
		}

		/* Handle "bleeding" */
		if (m_ptr->bleeding)
		{
			d = 1 + (m_ptr->maxhp / 50); 
			if (d > m_ptr->bleeding) d = m_ptr->bleeding;

			/* Hurt the monster, stop processing it if it dies */
			if (mon_take_hit(i, d, &xxx," bleeds to death.")) continue;

			/* Hack -- Recover from bleeding */
			if (m_ptr->bleeding > d) m_ptr->bleeding -= d;

			/* Fully recover */
			else
			{
				/* Recover fully */
				m_ptr->bleeding = 0;

				/* Message if visible */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is no longer bleeding.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
				}
			}
		}

		/* Handle "poisoned" */
		if (m_ptr->poisoned)
		{
			d = (m_ptr->poisoned) / 10; 
			if (d < 1) d = 1;

			/* Hurt the monster, stop processing it if it dies */
			if (mon_take_hit(i, d, &xxx," dies of poison.")) continue;

			/* Hack -- Recover from bleeding */
			if (m_ptr->poisoned > d) m_ptr->poisoned -= d;


			/* Fully recover */
			else
			{
				/* Recover fully */
				m_ptr->poisoned = 0;

				/* Message if visible */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is no longer poisoned.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
				}
			}
		}

		/* Handle "stun" */
		if (m_ptr->stunned)
		{
			d = 1;

			/* Make a "saving throw" against stun */
			if (rand_int(5000) < ((r_ptr->level * r_ptr->level) - m_ptr->stunned))
			{
				/* Recover fully */
				d = m_ptr->stunned;
			}

			/* Hack -- Recover from stun */
			if (m_ptr->stunned > d)	m_ptr->stunned -= d;

			/* Fully recover */
			else
			{
				/* Recover fully */
				m_ptr->stunned = 0;

				/* Message if visible */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is no longer stunned.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
				}
			}
		}

		/* Handle confusion */
		if (m_ptr->confused)
		{
			d = randint(r_ptr->level / 10 + 1);

			/* Still confused */
			if (m_ptr->confused > d) m_ptr->confused -= d;

			/* Recovered */
			else
			{
				/* No longer confused */
				m_ptr->confused = 0;

				/* Message if visible */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is no longer confused.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
				}
			}
		}

		/* Handle earthbind */
		if (m_ptr->earthbound)
		{
			d = randint(r_ptr->level / 10 + 1);

			/* Still confused */
			if (m_ptr->earthbound > d) m_ptr->earthbound -= d;

			/* Recovered */
			else
			{
				/* No longer confused */
				m_ptr->earthbound = 0;

				/* Message if visible */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is no longer earthbound.", m_name);
				}
			}
		}

		/* Handle blindness */
		if (m_ptr->blinded)
		{
			d = randint(r_ptr->level / 10 + 1);

			/* Still blinded */
			if (m_ptr->blinded > d) m_ptr->blinded -= d;

			/* Recovered */
			else
			{
				/* No longer blinded */
				m_ptr->blinded = 0;

				/* Message if visible */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is no longer blinded.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
				}
			}
		}

		/* Handle "fear" */
		if (m_ptr->monfear)
		{
			/* Amount of "boldness" */
			d = randint(r_ptr->level / 10 + 1);

			/* Still afraid */
			if (m_ptr->monfear > d)	m_ptr->monfear -= d;

			/* Recover from fear, take note if seen */
			else
			{
				/* No longer afraid */
				m_ptr->monfear = 0;

				/* Visual note */
				if (m_ptr->ml)
				{
					char m_name[80];
					char m_poss[80];

					/* Get the monster name/poss */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);
					monster_desc(m_poss, sizeof(m_poss), m_ptr, 0x22);

					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s recovers %s courage.", m_name, m_poss);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
				}
			}
		}

		/* Handle "calm" */
		if (m_ptr->calmed)
		{
			/* Amount of "anger" */
			d = randint(r_ptr->level/10 + adj_chr_calm[p_stat(A_CHR)]);

			/* Handle aggravate */
			if (p_ptr->aggravate) d = m_ptr->calmed;

			/* Still calmed */
			if (m_ptr->calmed > d)
			{
				if (rand_int(100) < 25) m_ptr->calmed -= d;
			}

			/* Recover from passivity, take note if seen */
			else
			{
				/* No longer calm */
				m_ptr->calmed = 0;

				/* Visual note */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name/poss */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
					message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is no longer calm.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
				}
			}
		}

		/* Monster speeds restore more slowly */
		if (turn % 100) continue;

		/* Monster speed correcting */
		if (m_ptr->mspeed != m_ptr->bspeed)
		{
			if (m_ptr->mspeed > m_ptr->bspeed) m_ptr->mspeed--;
			else m_ptr->mspeed++;
		}

		/* Monsters can occasionally fall asleep */
		if (turn % 1000) continue;
		
		/* Player is out of the monster's sensing area, and monster is out of LOS */
		if ((m_ptr->cdis > r_ptr->aaf) && !player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			int chance;

			/* Stupid monsters likely to fall asleep */
			if (r_ptr->flags1 & RF1_STUPID) chance = 33;
			/* Smart monsters are unlikely to fall asleep */
			else if (r_ptr->flags1 & RF1_SMART) chance = 10;
			/* Most monsters somewhat likely */
			else chance = 20;

			/* Only fall asleep if awake or close to awake */
			if ((m_ptr->sleep < (r_ptr->sleep * 2)) && (rand_int(100) < chance))
			{
				m_ptr->sleep = ((r_ptr->sleep * 2) + randint(r_ptr->sleep * 10));
			}
		}
	}
}
