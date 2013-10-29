/* File: melee2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#ifdef DRS_SMART_OPTIONS

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
static bool int_outof(const monster_race *r_ptr, int prob)
{
	/* Non-Smart monsters are half as "smart" */
	if (!(r_ptr->flags2 & (RF2_SMART))) prob = prob / 2;

	/* Roll the dice */
	return (rand_int(100) < prob);
}



/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);

	u32b smart = 0L;


	/* Too stupid to know anything */
	if (r_ptr->flags2 & (RF2_STUPID)) return;


	/* Must be cheating or learning */
	if (!adult_ai_cheat && !adult_ai_learn) return;


	/* Update acquired knowledge */
	if (adult_ai_learn)
	{
		/* Hack -- Occasionally forget player status */
		if (m_ptr->smart && (rand_int(100) < 1)) m_ptr->smart = 0L;

		/* Use the memorized flags */
		smart = m_ptr->smart;
	}


	/* Cheat if requested */
	if (adult_ai_cheat)
	{
		/* Know weirdness */
		if (p_ptr->free_act) smart |= (SM_IMM_FREE);
		if (!p_ptr->msp) smart |= (SM_IMM_MANA);
		if ((cp_ptr->flags & CF_HEAVY_BONUS) || (cp_ptr->flags & CF_KNIGHT)) smart |= (SM_IMM_MANA);

		/* Know immunities */
		if (p_ptr->immune_acid) smart |= (SM_IMM_ACID);
		if (p_ptr->immune_elec) smart |= (SM_IMM_ELEC);
		if (p_ptr->immune_fire) smart |= (SM_IMM_FIRE);
		if (p_ptr->immune_cold) smart |= (SM_IMM_COLD);

		/* Know oppositions */
		if (p_ptr->timed[TMD_OPP_ACID]) smart |= (SM_OPP_ACID);
		if (p_ptr->timed[TMD_OPP_ELEC]) smart |= (SM_OPP_ELEC);
		if (p_ptr->timed[TMD_OPP_FIRE]) smart |= (SM_OPP_FIRE);
		if (p_ptr->timed[TMD_OPP_COLD]) smart |= (SM_OPP_COLD);
		if (p_ptr->timed[TMD_OPP_POIS]) smart |= (SM_OPP_POIS);

		/* Know resistances */
		if (p_ptr->resist_acid) smart |= (SM_RES_ACID);
		if (p_ptr->resist_elec) smart |= (SM_RES_ELEC);
		if (p_ptr->resist_fire) smart |= (SM_RES_FIRE);
		if (p_ptr->resist_cold) smart |= (SM_RES_COLD);
		if (p_ptr->resist_pois) smart |= (SM_RES_POIS);
		if (p_ptr->resist_fear) smart |= (SM_RES_FEAR);
	/*	if (p_ptr->resist_charm) smart |= (SM_RES_CHARM); */
		if (p_ptr->resist_lite) smart |= (SM_RES_LITE);
		if (p_ptr->resist_dark) smart |= (SM_RES_DARK);
		if (p_ptr->resist_blind) smart |= (SM_RES_BLIND);
		if (p_ptr->resist_confu) smart |= (SM_RES_CONFU);
		if (p_ptr->resist_sound) smart |= (SM_RES_SOUND);
		if (p_ptr->resist_shard) smart |= (SM_RES_SHARD);
		if (p_ptr->resist_nexus) smart |= (SM_RES_NEXUS);
		if (p_ptr->resist_nethr) smart |= (SM_RES_NETHR);
		if (p_ptr->resist_chaos) smart |= (SM_RES_CHAOS);
		if (p_ptr->resist_disen) smart |= (SM_RES_DISEN);
	}


	/* Nothing known */
	if ((!smart) && (!p_ptr->see_inv)) return;


	if (smart & (SM_IMM_ACID))
	{
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_ACID);
	}
	else if ((smart & (SM_OPP_ACID)) && (smart & (SM_RES_ACID)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_ACID);
	}
	else if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_ACID);
	}


	if (smart & (SM_IMM_ELEC))
	{
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_BR_ELEC);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BA_ELEC);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_ELEC);
	}
	else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_ELEC);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_ELEC);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_ELEC);
	}
	else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_ELEC);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_ELEC);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_ELEC);
	}


	if (smart & (SM_IMM_FIRE))
	{
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_BR_FIRE);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BA_FIRE);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_FIRE);
	}
	else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_FIRE);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_FIRE);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_FIRE);
	}
	else if ((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_FIRE);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_FIRE);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_FIRE);
	}


	if (smart & (SM_IMM_COLD))
	{
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_BR_COLD);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BA_COLD);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_ICEE);
	}
	else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_COLD);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_COLD);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_ICEE);
	}
	else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_COLD);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_COLD);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_ICEE);
	}


	if ((smart & (SM_OPP_POIS)) && (smart & (SM_RES_POIS)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_POIS);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_POIS);
	}
	else if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_POIS);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_POIS);
	}


	if (smart & (SM_RES_FEAR))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_SCARE);
	}

	if (smart & (SM_RES_LITE))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_LITE);
	}

	if (smart & (SM_RES_DARK))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_DARK);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BA_DARK);
	}

	if (smart & (SM_RES_BLIND))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BLIND);
	}

	if (smart & (SM_RES_CONFU))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_CONF);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_CONF);
	}

	if (smart & (SM_RES_SOUND))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_SOUN);
	}

	if (smart & (SM_RES_SHARD))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_SHAR);
	}

	if (smart & (SM_RES_NEXUS))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_NEXU);
		if (int_outof(r_ptr, 50)) f6 &= ~(RF6_TELE_LEVEL);
	}

	if (smart & (SM_RES_NETHR))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_NETH);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BA_NETH);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BO_NETH);
	}

	if (smart & (SM_RES_CHAOS))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_CHAO);
	}

	if (smart & (SM_RES_DISEN))
	{
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_BR_DISE);
	}


	if (smart & (SM_IMM_FREE))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_HOLD);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_SLOW);
	}

	if (smart & (SM_IMM_MANA))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_DRAIN_MANA);
	}

	/* temporary invisibility spell */
	if (p_ptr->see_inv)
	{
		if (int_outof(r_ptr, 100)) f6 &= ~(RF6_INVIS);
	}

	/* XXX XXX XXX No spells left? */
	/* if (!f4 && !f5 && !f6) ... */


	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
}


#endif /* DRS_SMART_OPTIONS */


#ifdef MONSTER_AI

/*
 * Determine if there is a space near the selected spot in which
 * a summoned creature can appear
 */
static bool summon_possible(int y1, int x1)
{
	int y, x;

	/* Start at the location, and check 2 grids in each dir */
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
			if (cave_can_occupy_bold(y, x) && los(y1, x1, y, x))
			{
				return (TRUE);
			}
		}
	}

	return FALSE;
}



/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will return FALSE if a monster
 * is in the way.
 *
 * Then we should perhaps instead supply a flag to "projectable()".  XXX XXX
 */
bool clean_shot(int y1, int x1, int y2, int x2, bool okwall)
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
	if ((!okwall) && (!cave_floor_bold(y, x))) return (FALSE);

	/* May not end in an unrequested grid */
	if ((y != y2) || (x != x2)) return (FALSE);

	/* Assume okay */
	return (TRUE);
}

#endif /* MONSTER_AI */


/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void bolt(int m_idx, int typ, int dam_hp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg, pflg;
	
	if (typ == GF_BOULDER) flg = PROJECT_GRID | PROJECT_STOP | PROJECT_KILL;
	else flg = PROJECT_STOP | PROJECT_KILL;

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, py, px, dam_hp, typ, flg, pflg);
}


/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 *
 * isbreath (mode) == 0 normal ball spell
 * isbreath == 1 breath attack
 */
static void breath(int m_idx, int typ, int dam_hp, int isbreath)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int rad, pflg;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	if (isbreath == 1) pflg = PROJO_BRETH;
    	
	/* raise breath damage for low hp monsters */
	if ((r_ptr->flags2 & (RF2_BR_STRONG)) && (dam_hp < 31) &&
       (!(r_ptr->flags2 & (RF2_BR_WEAK))) && (r_ptr->hdice * r_ptr->hside < 150))
	{
		int die = randint(100) + (goodluck/3) - (badluck/2);
		if (die < 5) dam_hp = (dam_hp * 11) / 4;
		else if (die < 52) dam_hp = dam_hp * 2;
		else if (die < 76) dam_hp = (dam_hp * 3) / 2;
		else if (die < 92) dam_hp = dam_hp + 2;
	}
           
	/* BR_WEAK flag lowers breath damage */
	if ((r_ptr->flags2 & (RF2_BR_WEAK)) && (dam_hp > 13))
	{
		int die = randint(100);
		if (die < 5) dam_hp = dam_hp / 4;
		else if (die < 55) dam_hp = dam_hp / 3;
		else if (die < 75) dam_hp = ((dam_hp * 2) / 5);
		else if (die < 95) dam_hp = dam_hp / 2;
		else dam_hp = ((dam_hp * 3) / 5);
	}

	/* Determine the radius of the blast */
	rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

    /* shorter range for BR_WEAK) */
    if (r_ptr->flags2 & (RF2_BR_WEAK)) range = 5;

	/* HELPER monsters target whoever the player asked us to target */
	if (r_ptr->flags3 & (RF3_HELPER))
	{
		py = p_ptr->target_row;
		px = p_ptr->target_col;
	}

	/* Target the player with a ball attack */
	(void)project(m_idx, rad, py, px, dam_hp, typ, flg, pflg);
}


/*
 * Offsets for the spell indices
 */
#define RF4_OFFSET 32 * 3
#define RF5_OFFSET 32 * 4
#define RF6_OFFSET 32 * 5


/*
 * Have a monster choose a spell to cast.
 *
 * Note that the monster's spell list has already had "useless" spells
 * (bolts that won't hit the player, summons without room, etc.) removed.
 * Perhaps that should be done by this function.
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * This function could be an efficiency bottleneck.
 */
static int choose_attack_spell(int m_idx, u32b f4, u32b f5, u32b f6)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4_mask = 0L;
	u32b f5_mask = 0L;
	u32b f6_mask = 0L;

	int num = 0;
	byte spells[96];

	int i, py = p_ptr->py, px = p_ptr->px;

#ifdef MONSTER_AI

	bool has_escape, has_attack, has_summon, has_tactic;
	bool has_annoy, has_haste, has_heal;


		/* What have we got? */
		has_escape = ((f4 & (RF4_ESCAPE_MASK)) ||
		              (f5 & (RF5_ESCAPE_MASK)) ||
		              (f6 & (RF6_ESCAPE_MASK)));
		has_attack = ((f4 & (RF4_ATTACK_MASK)) ||
		              (f5 & (RF5_ATTACK_MASK)) ||
		              (f6 & (RF6_ATTACK_MASK)));
		has_summon = ((f4 & (RF4_SUMMON_MASK)) ||
		              (f5 & (RF5_SUMMON_MASK)) ||
		              (f6 & (RF6_SUMMON_MASK)));
		has_tactic = ((f4 & (RF4_TACTIC_MASK)) ||
		              (f5 & (RF5_TACTIC_MASK)) ||
		              (f6 & (RF6_TACTIC_MASK)));
		has_annoy = ((f4 & (RF4_ANNOY_MASK)) ||
		             (f5 & (RF5_ANNOY_MASK)) ||
		             (f6 & (RF6_ANNOY_MASK)));
		has_haste = ((f4 & (RF4_HASTE_MASK)) ||
		             (f5 & (RF5_HASTE_MASK)) ||
		             (f6 & (RF6_HASTE_MASK)));
		has_heal = ((f4 & (RF4_HEAL_MASK)) ||
		            (f5 & (RF5_HEAL_MASK)) ||
		            (f6 & (RF6_HEAL_MASK)));

	/* Smart monsters restrict their spell choices. */
	if (adult_ai_smart && !(r_ptr->flags2 & (RF2_STUPID)))
	{
		/*** Try to pick an appropriate spell type ***/

		/* Hurt badly or afraid, attempt to flee */
		if (has_escape && ((m_ptr->hp < m_ptr->maxhp / 4) || m_ptr->monfear))
		{
			/* Choose escape spell */
			f4_mask = (RF4_ESCAPE_MASK);
			f5_mask = (RF5_ESCAPE_MASK);
			f6_mask = (RF6_ESCAPE_MASK);
		}

		/* Still hurt badly, couldn't flee, attempt to heal */
		else if (has_heal && m_ptr->hp < m_ptr->maxhp / 4)
		{
			/* Choose heal spell */
			f4_mask = (RF4_HEAL_MASK);
			f5_mask = (RF5_HEAL_MASK);
			f6_mask = (RF6_HEAL_MASK);
		}

		/* Player is close and we have attack spells, blink away */
		else if (has_tactic && (distance(py, px, m_ptr->fy, m_ptr->fx) < 4) &&
		         has_attack && (rand_int(100) < 75))
		{
			/* Choose tactical spell */
			f4_mask = (RF4_TACTIC_MASK);
			f5_mask = (RF5_TACTIC_MASK);
			f6_mask = (RF6_TACTIC_MASK);
		}

		/* We're hurt (not badly), try to heal */
		else if (has_heal && (m_ptr->hp < m_ptr->maxhp * 3 / 4) &&
		         (rand_int(100) < 60))
		{
			/* Choose heal spell */
			f4_mask = (RF4_HEAL_MASK);
			f5_mask = (RF5_HEAL_MASK);
			f6_mask = (RF6_HEAL_MASK);
		}

		/* Summon if possible (sometimes) */
		else if (has_summon && (rand_int(100) < 50))
		{
			/* Choose summon spell */
			f4_mask = (RF4_SUMMON_MASK);
			f5_mask = (RF5_SUMMON_MASK);
			f6_mask = (RF6_SUMMON_MASK);
		}

		/* Attack spell (most of the time) */
		else if (has_attack && (rand_int(100) < 85))
		{
			/* Choose attack spell */
			f4_mask = (RF4_ATTACK_MASK);
			f5_mask = (RF5_ATTACK_MASK);
			f6_mask = (RF6_ATTACK_MASK);
		}

		/* Try another tactical spell (sometimes) */
		else if (has_tactic && (rand_int(100) < 50))
		{
			/* Choose tactic spell */
			f4_mask = (RF4_TACTIC_MASK);
			f5_mask = (RF5_TACTIC_MASK);
			f6_mask = (RF6_TACTIC_MASK);
		}

		/* Haste self if we aren't already somewhat hasted (rarely) */
		else if (has_haste && (rand_int(100) < (20 + r_ptr->speed - m_ptr->mspeed)))
		{
			/* Choose haste spell */
			f4_mask = (RF4_HASTE_MASK);
			f5_mask = (RF5_HASTE_MASK);
			f6_mask = (RF6_HASTE_MASK);
		}

		/* Annoy player (most of the time) */
		else if (has_annoy && (rand_int(100) < 85))
		{
			/* Choose annoyance spell */
			f4_mask = (RF4_ANNOY_MASK);
			f5_mask = (RF5_ANNOY_MASK);
			f6_mask = (RF6_ANNOY_MASK);
		}

		/* Else choose no spell (The masks default to this.) */

		/* Keep only the interesting spells */
		f4 &= f4_mask;
		f5 &= f5_mask;
		f6 &= f6_mask;

		/* Anything left? */
		if (!(f4 || f5 || f6)) return (0);
	}
	/* Smart monsters should have some AI even if AI_SMART isn't turned on */
	else if (r_ptr->flags2 & (RF2_SMART))
	{
		bool chosen = FALSE;
        /* Hurt badly or afraid, attempt to flee */
		if (has_escape && ((m_ptr->hp < m_ptr->maxhp / 4) || m_ptr->monfear))
		{
			/* Choose escape spell */
			f4_mask = (RF4_ESCAPE_MASK);
			f5_mask = (RF5_ESCAPE_MASK);
			f6_mask = (RF6_ESCAPE_MASK);
			chosen = TRUE;
		}

		/* Still hurt badly, couldn't flee, attempt to heal */
		else if (has_heal && m_ptr->hp < m_ptr->maxhp / 4)
		{
			/* Choose heal spell */
			f4_mask = (RF4_HEAL_MASK);
			f5_mask = (RF5_HEAL_MASK);
			f6_mask = (RF6_HEAL_MASK);
			chosen = TRUE;
		}

		/* Player is close and we prefer attack spells, blink away */
		else if (has_tactic && (distance(py, px, m_ptr->fy, m_ptr->fx) < 4) &&
		         has_attack && (r_ptr->flags2 & (RF2_RANGE)) && (rand_int(100) < 75))
		{
			/* Choose tactical spell */
			f4_mask = (RF4_TACTIC_MASK);
			f5_mask = (RF5_TACTIC_MASK);
			f6_mask = (RF6_TACTIC_MASK);
			chosen = TRUE;
		}
		/* else chose randomly */

		/* only restrict spells if we've chosen a specific spell type */
        if (chosen)
		{
			/* Keep only the interesting spells */
			f4 &= f4_mask;
			f5 &= f5_mask;
			f6 &= f6_mask;

			/* Anything left? */
			if (!(f4 || f5 || f6)) return (0);
	    }
    }

#endif /* MONSTER_AI */

	/* Extract the "innate" spells */
	for (i = 0; i < 32; i++)
	{
		if (f4 & (1L << i)) spells[num++] = i + RF4_OFFSET;
	}

	/* Extract the "normal" spells */
	for (i = 0; i < 32; i++)
	{
		if (f5 & (1L << i)) spells[num++] = i + RF5_OFFSET;
	}

	/* Extract the "bizarre" spells */
	for (i = 0; i < 32; i++)
	{
		if (f6 & (1L << i)) spells[num++] = i + RF6_OFFSET;
	}

	/* Paranoia */
	if (num == 0) return 0;

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
 * if the player is blind.
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
bool make_attack_spell(int m_idx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int k, die, chance, thrown_spell, rlev;
	int failrate, surround, bshield;
	int mcdis, askill;
    int dir, damage, basebr, brdie;
	int savechance;
	bool controlled, strong, innate;

	u32b f4, f5, f6;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	/* Summon count */
	int count = 0;


	/* Extract the blind-ness */
	bool blind = (p_ptr->timed[TMD_BLIND] ? TRUE : FALSE);

	/* Extract the "see-able-ness" */
	bool seen = (!blind && m_ptr->ml);

	/* Assume "normal" target */
	bool normal = TRUE;

	/* Assume "projectable" */
	bool direct = TRUE;


	/* Cannot cast spells when nice */
	if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);

	/* Hack -- Extract the spell probability */
	chance = (r_ptr->freq_innate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance) return (FALSE);

	/* Only do spells occasionally */
	if (rand_int(100) >= chance) return (FALSE);

	/* monsters which haven't noticed you should cast less often */
    if ((m_ptr->csleep) && (randint(100) < 45)) return (FALSE);

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

	/* Hack -- require projectable player */
	/* why "if (normal)" when normal is always TRUE here? */
	if (normal)
	{
         /* DJA: Some Monsters can teleport toward the player when not in LOS */
         /* (this should prevent Sauron from being trapped in a vault) */
         if ((r_ptr->flags6 & (RF6_TPORT)) && (!m_ptr->monfear) &&
             (rand_int(100) < rlev / 8) && (rlev > 40) &&
             (m_ptr->cdis > MAX_RANGE + 4) && (m_ptr->hp >= m_ptr->maxhp / 2))
         {
			/* teleport towards the player */
			teleport_away(m_idx, MAX_SIGHT * 2 + 1, 2);
			
			/* reset seeableness (because it might have changed after teleport) */
            if ((!blind) && (player_can_see_bold(m_ptr->fy, m_ptr->fx))) seen = TRUE;
            if (((r_ptr->flags2 & (RF2_INVISIBLE)) || (m_ptr->tinvis)) &&
                (!p_ptr->see_inv)) seen = FALSE;
			  
	        /* Remember the spell */
	        if (seen)
	        {
			   disturb(1, 0);
               msg_format("%^s teleports into view.", m_name);
			   l_ptr->flags6 |= (RF6_TPORT);
			   if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
	        }
	        return (TRUE);
         }
		/* Check range */
		if (m_ptr->cdis > MAX_RANGE) return (FALSE);
		/* Shorter range in town */
		if ((!p_ptr->depth) && (m_ptr->cdis > MAX_TOWNR)) return (FALSE);

		/* Check path */
		if (!projectable(m_ptr->fy, m_ptr->fx, py, px))
		{
           /* DJA: Monsters should be able to blink */
           /* toward the player when not in LOS */
           if ((r_ptr->flags6 & (RF6_BLINK)) && (randint(100) < 20 + rlev / 9))
           {
			  /* blink away if afraid, otherwise blink toward the player */
              if ((m_ptr->monfear) || (m_ptr->hp < m_ptr->maxhp / 2))
                 teleport_away(m_idx, 10, 3);
              else teleport_away(m_idx, 10, 2);
			  
	          /* Remember the spell */
	          if ((seen) && (player_can_see_bold(m_ptr->fy, m_ptr->fx)))
	          {
			     disturb(1, 0);
                 msg_format("%^s blinks into view.", m_name);
			     l_ptr->flags6 |= (RF6_BLINK);
			     if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
	          }
	          return (TRUE);
           }
           return (FALSE);
        }
	}
	
	/* no combat spells if monster hasn't noticed you */
	/* (this allows BLINK, INVIS, TPORT, DARKNESS, HEAL, HEAL_KIN, and HEAL_OTHR) */
    if ((m_ptr->roaming) || (m_ptr->truce))
	{
		f4 &= (RF4_NONCOMBAT_MASK);
		f5 &= (RF5_NONCOMBAT_MASK);
		f6 &= (RF6_NONCOMBAT_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
    }


	/* Hack -- allow "desperate" spells */
	if ((r_ptr->flags2 & (RF2_SMART)) &&
	    (m_ptr->hp < m_ptr->maxhp / 10) &&
	    (rand_int(100) < 50))
	{
		/* Require intelligent spells */
		f4 &= (RF4_INT_MASK);
		f5 &= (RF5_INT_MASK);
		f6 &= (RF6_INT_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}


#ifdef DRS_SMART_OPTIONS

	/* Remove the "ineffective" spells */
	remove_bad_spells(m_idx, &f4, &f5, &f6);

#endif /* DRS_SMART_OPTIONS */

	/* Check whether summons and bolts are worth it. */
	if ((adult_ai_smart && !(r_ptr->flags2 & (RF2_STUPID))) ||
		(r_ptr->flags2 & (RF2_SMART)))
	{
		/* Check for a clean bolt shot */
		if ((f4 & (RF4_BOLT_MASK) ||
			 f5 & (RF5_BOLT_MASK) ||
			 f6 & (RF6_BOLT_MASK)) &&
			!clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE))
		{
			/* Remove spells that will only hurt friends */
			f4 &= ~(RF4_BOLT_MASK);
			f5 &= ~(RF5_BOLT_MASK);
			f6 &= ~(RF6_BOLT_MASK);
		}

		/* Check for a possible summon */
		if (!(summon_possible(m_ptr->fy, m_ptr->fx)))
		{
			/* Remove summoning spells */
			f4 &= ~(RF4_SUMMON_MASK);
			f5 &= ~(RF5_SUMMON_MASK);
			f6 &= ~(RF6_SUMMON_MASK);
		}
	}

	/* If monster has HEAL_OTHR or HEAL_KIN spell */
	/* check to see if there are other monsters around to heal */
	if ((f6 & (RF6_HEALO_MASK)) && !(r_ptr->flags2 & (RF2_STUPID)))
	{
		bool kin = TRUE;
		if (r_ptr->flags6 & (RF6_HEAL_OTHR)) kin = FALSE;
		
		/* test run to the spell function */
		if (!heal_monsters(0, m_ptr, kin))
		{
			/* don't cast it if there are no monsters to heal */
            f6 &= ~(RF6_HEALO_MASK);
		}
	}

	/* Monsters don't cast EXPLODE when it can be hurt in the explosion */
	if ((r_ptr->flags4 & (RF4_EXPLODE)) && !(r_ptr->flags2 & (RF2_STUPID)))
	{
		bool immunewall = FALSE;
		if ((r_ptr->flags2 & (RF2_PASS_WALL)) || (r_ptr->flags2 & (RF2_KILL_WALL)))
			immunewall = TRUE;
        if ((m_ptr->cdis < 3) && (!immunewall))
		{
            f4 &= ~(RF4_EXPLODE);
		}
	}
	
	/* Monsters shouldn't cast TELE_TO when they're afraid or already next to the PC */
    if (!(r_ptr->flags2 & (RF2_STUPID)))
	{
		if ((m_ptr->cdis < 2) || (m_ptr->monfear))
		{
			/* remove the spell only if PC is not on a glyph of warding */
            if (!(cave_feat[p_ptr->py][p_ptr->px] == FEAT_GLYPH))
				f6 &= ~(RF6_TELE_TO);
		}
	}

	/* No spells left */
	if (!f4 && !f5 && !f6) return (FALSE);

	/* Handle "leaving" */
	if (p_ptr->leaving) return (FALSE);


	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, sizeof(m_poss), m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, 0x88);


	/* Choose a spell to cast */
	thrown_spell = choose_attack_spell(m_idx, f4, f5, f6);

	/* Abort if no spell was chosen */
	if (!thrown_spell) return (FALSE);
    
	/* OFFSET+27: EXPLOSE spell is not innate */
	if ((thrown_spell < RF5_OFFSET) && (!(thrown_spell == RF4_OFFSET+27)))
		innate = TRUE;
	else innate = FALSE;

    failrate = 0;
#ifdef MONSTER_AI
	/* Calculate spell failure rate */
	failrate = 25 - (rlev + 3) / 4;

	/* Hack -- Stupid monsters will never fail (for jellies and such) */
	if (!adult_ai_smart || r_ptr->flags2 & (RF2_STUPID)) failrate = 0;
	/* innate attacks never fail unless confused or stunned */
	if (innate) failrate = 0;
#endif /* MONSTER_AI */

    /* DJA: new effect of monster stunning */
    /* fails about 5/6 of the time if confused or 2/3 of the time if stunned */
    if ((m_ptr->confused) && (m_ptr->stunned)) failrate = 100;
    if (m_ptr->confused) failrate += 84;
    else if (m_ptr->stunned) failrate += 67;

    /* innate attacks rarely fail (even if confused or stunned) */
    if (innate) failrate = failrate / 4;
	/* The luck factor */
	if ((badluck > 9) && (failrate > 5)) failrate -= (badluck-8) / 2;
	if ((goodluck > 9) && (failrate) && (failrate < 80)) failrate += (goodluck-8) / 2;

	/* Check for spell failure */
	if ((rand_int(100) < failrate) && (!(thrown_spell == RF4_OFFSET+0)))
	{
		/* Message */
		if ((innate) && (seen))
		{
			/* THROW, T_AXE, or BOULDER */
            if ((thrown_spell == RF4_OFFSET+2) || (thrown_spell == RF4_OFFSET+3) ||
				(thrown_spell == RF4_OFFSET+31))
				msg_format("%^s drops something.", m_name);
			/* ARROW_1 & ARROW_2 */
			else if ((thrown_spell == RF4_OFFSET+4) || (thrown_spell == RF4_OFFSET+5))
				msg_format("%^s fumbles with %s bow.", m_name, m_poss);
			/* ARROW_3 & ARROW_4 */
			else if ((thrown_spell == RF4_OFFSET+6) || (thrown_spell == RF4_OFFSET+7))
				msg_format("The %^s misfires a missile.", m_name);
            else msg_format("%^s coughs.", m_name);
		}
		else if (seen) msg_format("%^s tries to cast a spell, but fails.", m_name);

	    return (TRUE);
	}
/* #endif * MONSTER_AI */

    /* primary spellcasters get damage reduction from surrounding magic */
    surround = 0;
	if (cp_ptr->flags & CF_POWER_SHIELD) surround = p_ptr->lev + 10;
	else if ((cp_ptr->flags & CF_ZERO_FAIL) && (p_ptr->lev > 20)) surround = p_ptr->lev/3 + 5;
	if (p_ptr->timed[TMD_BRAIL]) surround += 10 + (goodluck/2);
	/* luck can also have an effect on this */
	if (goodluck > 7) surround += goodluck;
	if ((goodluck > 14) && (randint(100) < 67)) surround += goodluck - 7;
	if ((surround > 0) && (badluck > 15)) surround -= badluck/2;
	if (surround < 8) surround = 0; /* minimum positive is 8 */

	/* breath shield effect */
	/* (separated from 'surround' because surround affects more than just breaths) */
    if ((p_ptr->breath_shield) && (p_ptr->timed[TMD_BR_SHIELD])) bshield = 2;
    else if ((p_ptr->breath_shield) || (p_ptr->timed[TMD_BR_SHIELD])) bshield = 1;
	else bshield = 0;

	/* most dragons and other big breathers have BR_STRONG */
    if (r_ptr->flags2 & (RF2_BR_STRONG)) strong = TRUE;
	else strong = FALSE;

    /* breath damage not always based on current hit points */
    basebr = m_ptr->hp;
    brdie = rand_int(100 - badluck + (goodluck*2));
    if ((brdie < 12) && (m_ptr->hp < (m_ptr->maxhp * 5) / 6) && (!strong))
    {
       basebr += randint(((m_ptr->maxhp * 5) / 6) - m_ptr->hp);
    }
    else if ((brdie < 2) && (m_ptr->hp + 55 < m_ptr->maxhp)) basebr += 50;
    else if (brdie > 92) basebr = (m_ptr->hp * 3) / 4;

    /* breath should always do at least 1hp damage */
    if (basebr < 6) basebr = 6;

	/* Cast the spell. */
	switch (thrown_spell)
	{
		/* RF4_SHRIEK */
		case RF4_OFFSET+0:
		{
			if ((m_ptr->silence) && (goodluck > 4)) return FALSE;
			if (!direct) break;
			disturb(1, 0);
			sound(MSG_SHRIEK);
			if (seen) msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx);
			break;
		}

		/* RF4_ */
		case RF4_OFFSET+1:
		{
			/* nothing yet */;
		}

		/* RF4_T_AXE */
		case RF4_OFFSET+2:
		{
			disturb(1, 0);
			mcdis = m_ptr->cdis;
			if (mcdis > 3) mcdis -= 3;
			else mcdis = 0;
			/* monster ranged weapons should not always hit */
			askill = 88 + (rlev/5) - mcdis*2;
			if (cave_feat[m_ptr->fy][m_ptr->fx] == FEAT_OPEN_PIT) askill -= 15;
			if (r_ptr->flags2 & (RF2_RANGE)) askill += 15;
			/* dodge ac */
			askill -= adj_dex_dis[p_ptr->stat_ind[A_DEX]] * 2;
			if (randint(100) < askill)
            {
				if (blind) msg_format("%^s throws something.", m_name);
				else msg_format("%^s throws a throwing axe at you!", m_name);
				if (rlev > 22) bolt(m_idx, GF_THROW, damroll(3, 4));
				else bolt(m_idx, GF_THROW, damroll(2, 4));
            }
            else /* missed */
            {
				if (blind) msg_format("%^s throws something and curses.", m_name);
				else msg_format("%^s throws a throwing axe at you but misses.", m_name);
            }
			break;
		}

		/* RF4_THROW */
		case RF4_OFFSET+3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s throws something.", m_name);
			else msg_format("%^s throws a piece of junk from the floor.", m_name);
			if (rlev > 22) bolt(m_idx, GF_THROW, damroll(2, 4));
			else bolt(m_idx, GF_THROW, damroll(1, 4));
            (void)inc_timed(TMD_STUN, (randint(3)));
			break;
		}

		/* RF4_ARROW_1 */
		case RF4_OFFSET+4:
		{
			disturb(1, 0);
			mcdis = m_ptr->cdis;
			if (mcdis > 2) mcdis -= 2;
			else mcdis = 0;
			/* monster arrows should not always hit */
			askill = 80 + (rlev/5) - mcdis*2;
			if (cave_feat[m_ptr->fy][m_ptr->fx] == FEAT_OPEN_PIT) askill -= 15;
			if (r_ptr->flags2 & (RF2_RANGE)) askill += 20;
			/* dodge ac */
			askill -= adj_dex_dis[p_ptr->stat_ind[A_DEX]] * 2;
			if (randint(100) < askill)
            {
                if (blind) msg_format("%^s makes a strange noise.", m_name);
                else msg_format("%^s fires an arrow.", m_name);
                bolt(m_idx, GF_ARROW, damroll(1, 6));
            }
            else /* missed */
            {
                if (blind) msg_format("%^s makes a strange noise and curses.", m_name);
                else msg_format("%^s fires an arrow but misses.", m_name);
            }
			break;
		}

		/* RF4_ARROW_2 */
		case RF4_OFFSET+5:
		{
			disturb(1, 0);
			mcdis = m_ptr->cdis;
			if (mcdis > 3) mcdis -= 3;
			else mcdis = 0;
			/* monster arrows should not always hit */
			askill = 85 + (rlev/5) - mcdis*2;
			if (cave_feat[m_ptr->fy][m_ptr->fx] == FEAT_OPEN_PIT) askill -= 15;
			if (r_ptr->flags2 & (RF2_RANGE)) askill += 20;
			/* dodge ac */
			askill -= adj_dex_dis[p_ptr->stat_ind[A_DEX]] * 2;
			if (randint(100) < askill)
            {
                if (blind) msg_format("%^s makes a strange noise.", m_name);
                else msg_format("%^s fires an arrow.", m_name);
                damage = damroll(3, 6);
                if ((rlev > 19) && (randint(100) < 35)) damage += randint((rlev-18)/2 + 1);
                bolt(m_idx, GF_ARROW, damage);
            }
            else /* missed */
            {
                if (blind) msg_format("%^s makes a strange noise and curses.", m_name);
                else msg_format("%^s fires an arrow but misses.", m_name);
            }
			break;
		}

		/* RF4_ARROW_3 */
		case RF4_OFFSET+6:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires a missile.", m_name);
			/* (always hit because these are like stronger magic missiles) */
			damage = damroll(5, 6);
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			bolt(m_idx, GF_ARROW, damage);
			break;
		}

		/* RF4_ARROW_4 */
		case RF4_OFFSET+7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires a missile!", m_name);
			damage = damroll(7, 6);
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			bolt(m_idx, GF_ARROW, damage);
			break;
		}

		/* RF4_BR_ACID */
		case RF4_OFFSET+8:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_ACID);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes acid.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 3) > 1600 ? 1600 : (basebr / 3));
            }
            else 
            {
			   damage = ((basebr / 4) > 1200 ? 1200 : (basebr / 4));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_ACID, damage, 1);
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* RF4_BR_ELEC */
		case RF4_OFFSET+9:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_ELEC);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes lightning.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 3) > 1600 ? 1600 : (basebr / 3));
            }
            else 
            {
			   damage = ((basebr / 4) > 1200 ? 1200 : (basebr / 4));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_ELEC, damage, 1);
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* RF4_BR_FIRE */
		case RF4_OFFSET+10:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_FIRE);
			if (r_ptr->flags3 & (RF3_HELPER)) 
			{
               msg_format("%^s asks you who to breathe fire at.", m_name);
			   spellswitch = 13; /* prevents using old target */
			   if (!get_aim_dir(&dir)) break;
			   /* breath() changes target to player's target for HELPER monsters */
			   damage = ((basebr / 3) > 1200 ? 1200 : (basebr / 3));
			   breath(m_idx, GF_FIRE, damage, 1);
				/* fire_ball(GF_FIRE, 0, basebr / 3, 4); */
			   break;
            }
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes fire.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 3) > 1600 ? 1600 : (basebr / 3));
            }
            else 
            {
			   damage = ((basebr / 4) > 1200 ? 1200 : (basebr / 4));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_FIRE, damage, 1);
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* RF4_BR_COLD */
		case RF4_OFFSET+11:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_FROST);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes frost.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 3) > 1600 ? 1600 : (basebr / 3));
            }
            else 
            {
			   damage = ((basebr / 4) > 1200 ? 1200 : (basebr / 4));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_COLD, damage, 1);
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* RF4_BR_POIS */
		case RF4_OFFSET+12:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_GAS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gas.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 3) > 800 ? 800 : (basebr / 3));
            }
            else 
            {
			   damage = ((basebr / 4) > 720 ? 720 : (basebr / 4));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_POIS, damage, 1);
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

		/* RF4_BR_NETH */
		case RF4_OFFSET+13:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_NETHER);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nether.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 550 ? 550 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 500 ? 500 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_NETHER, damage, 1);
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* RF4_BR_LITE */
		case RF4_OFFSET+14:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_LIGHT);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes light.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 400 ? 400 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 300 ? 300 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_LITE, damage, 1);
			update_smart_learn(m_idx, DRS_RES_LITE);
			break;
		}

		/* RF4_BR_DARK */
		case RF4_OFFSET+15:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_DARK);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes darkness.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 400 ? 400 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 300 ? 300 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_DARK, damage, 1);
			update_smart_learn(m_idx, DRS_RES_DARK);
			break;
		}

		/* RF4_BR_CONF */
		case RF4_OFFSET+16:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_CONF);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes confusion.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 400 ? 400 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 300 ? 300 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_CONFUSION, damage, 1);
			update_smart_learn(m_idx, DRS_RES_CONFU);
			break;
		}

		/* RF4_BR_SOUN */
		case RF4_OFFSET+17:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_SOUND);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes sound.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 500 ? 500 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 400 ? 400 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_SOUND, damage, 1);
			update_smart_learn(m_idx, DRS_RES_SOUND);
			break;
		}

		/* RF4_BR_CHAO */
		case RF4_OFFSET+18:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_CHAOS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes chaos.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 500 ? 500 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 430 ? 430 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_CHAOS, damage, 1);
			update_smart_learn(m_idx, DRS_RES_CHAOS);
			break;
		}

		/* RF4_BR_DISE */
		case RF4_OFFSET+19:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_DISENCHANT);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes disenchantment.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 500 ? 500 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 390 ? 390 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_DISENCHANT, damage, 1);
			update_smart_learn(m_idx, DRS_RES_DISEN);
			break;
		}

		/* RF4_BR_NEXU */
		case RF4_OFFSET+20:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_NEXUS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nexus.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 400 ? 400 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 360 ? 360 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_NEXUS, damage, 1);
			update_smart_learn(m_idx, DRS_RES_NEXUS);
			break;
		}

		/* RF4_BR_TIME */
		case RF4_OFFSET+21:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_TIME);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes time.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 4) > 200 ? 200 : (basebr / 4));
            }
            else 
            {
			   damage = ((basebr / 4) > 150 ? 150 : (basebr / 4));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_TIME, damage, 1);
			break;
		}

		/* RF4_BR_INER */
		case RF4_OFFSET+22:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_INERTIA);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes inertia.", m_name);
			damage = ((basebr / 6) > 180 ? 180 : (basebr / 6));
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_INERTIA, damage, 1);
			break;
		}

		/* RF4_BR_GRAV */
		case RF4_OFFSET+23:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_GRAVITY);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gravity.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 4) > 270 ? 270 : (basebr / 4));
            }
            else 
            {
			   damage = ((basebr / 4) > 200 ? 200 : (basebr / 4));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_GRAVITY, damage, 1);
			break;
		}

		/* RF4_BR_SHAR */
		case RF4_OFFSET+24:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_SHARDS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes shards.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 500 ? 500 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 350 ? 350 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_SHARD, damage, 1);
			update_smart_learn(m_idx, DRS_RES_SHARD);
			break;
		}

		/* RF4_BR_PLAS */
		case RF4_OFFSET+25:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
			disturb(1, 0);
			sound(MSG_BR_PLASMA);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes plasma.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 210 ? 210 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 6) > 150 ? 150 : (basebr / 6));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_PLASMA, damage, 1);
			break;
		}

		/* RF4_BR_WALL */
		case RF4_OFFSET+26:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
            disturb(1, 0);
			sound(MSG_BR_FORCE);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes force.", m_name);
			damage = ((basebr / 6) > 200 ? 200 : (basebr / 6));
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_FORCE, damage, 1);
			break;
		}

		/* RF4_EXPLODE */
		case RF4_OFFSET+27:
		{
			int exploderad;
            disturb(1, 0);
			sound(MSG_BR_FORCE);
			if (blind) msg_format("You hear an ominous rumbling.");
			else msg_format("%^s causes an explosion!", m_name);
			/* cause an earthquake on the PC's square which damages the PC */
			if (m_ptr->cdis < 4) exploderad = 2;
			else exploderad = 3;
			earthquake(p_ptr->py, p_ptr->px, exploderad, 40, 4, FALSE);
			/* just in case the PC dodges the quake blast.. */
			damage = damroll(4, rlev/10 + 1);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 400));
			breath(m_idx, GF_SHARD, damage, 0);
			breath(m_idx, GF_BOULDER, damage, 0);
			break;
		}

		/* RF4_BR_FEAR Breathe fear */
		case RF4_OFFSET+28:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3)) break;
			disturb(1, 0);
			sound(MSG_BR_CONF);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes fear.", m_name);
			damage = ((basebr / 8) > 90 ? 90 : (basebr / 8));
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 500));
			breath(m_idx, GF_BRFEAR, damage, 1);
			break;
		}

		/* RF4_BR_SLIME */
		case RF4_OFFSET+29:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3))
            {
                msg_format("%^s breathes away from you.", m_name);
                break;
            }
            disturb(1, 0);
			sound(MSG_BR_PLASMA);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes slime.", m_name);
			if (r_ptr->flags2 & (RF2_BR_STRONG))
            {
			   damage = ((basebr / 6) > 140 ? 140 : (basebr / 6));
            }
            else 
            {
			   damage = ((basebr / 7) > 110 ? 110 : (basebr / 7));
            }
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 250));
			/* breath shield effect */
			if (bshield == 2) damage -= (damage / 3);
			else if (bshield == 1) damage -= (damage / 4);
			breath(m_idx, GF_BRSLIME, damage, 1);
			break;
		}

		/* RF4_BR_AMNS  (Breathe amnesia) */
		case RF4_OFFSET+30:
		{
            if ((m_ptr->charmed) && (m_ptr->cdis < 3)) break;
			disturb(1, 0);
			sound(MSG_BR_CONF);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes amnesia.", m_name);
			damage = ((basebr / 8) > 90 ? 90 : (basebr / 8));
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 500));
			breath(m_idx, GF_AMNESIA, damage, 1);
			break;
		}

		/* RF4_BOULDER */
		case RF4_OFFSET+31:
		{
			disturb(1, 0);
			damage = damroll(1 + rlev / 7, 12);
	        /* extra damage reduction from surrounding magic */
			if (surround) damage -= (damage * (surround / 500));
			/* GF_BOULDER requires damage or has alternate effect */
			if (damage < 1) damage = 1;
			mcdis = m_ptr->cdis;
			if (mcdis > 3) mcdis -= 3;
			else mcdis = 0;
			/* monster range weapons should not always hit */
			askill = 86 + (rlev/5) - mcdis*2;
			if (cave_feat[m_ptr->fy][m_ptr->fx] == FEAT_OPEN_PIT) askill -= 15;
			if (r_ptr->flags2 & (RF2_RANGE)) askill += 15;
			/* dodge ac */
			askill -= adj_dex_dis[p_ptr->stat_ind[A_DEX]] * 2;
			if (randint(100) < askill)
            {
				if (blind) msg_format("You hear something grunt with exertion.", m_name);
				else msg_format("%^s hurls a boulder at you!", m_name);
                bolt(m_idx, GF_BOULDER, damage);
            }
            else /* missed */
            {
				if (blind) msg_format("You hear something grunt with exertion.", m_name);
				else msg_format("%^s hurls a boulder at you but misses.", m_name);
            }
			break;
		}


		/* RF5_BA_ACID */
		case RF5_OFFSET+0:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts an acid ball.", m_name);
			damage = randint(rlev * 3) + 15;
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			breath(m_idx, GF_ACID, damage, 0);
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* RF5_BA_ELEC */
		case RF5_OFFSET+1:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a lightning ball.", m_name);
			damage = randint(rlev * 3 / 2) + 8;
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			breath(m_idx, GF_ELEC, damage, 0);
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* RF5_BA_FIRE */
		case RF5_OFFSET+2:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a fire ball.", m_name);
			damage = randint(rlev * 7 / 2) + 10;
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			breath(m_idx, GF_FIRE, damage, 0);
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* RF5_BA_COLD */
		case RF5_OFFSET+3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a frost ball.", m_name);
			damage = randint(rlev * 3 / 2) + 10;
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			breath(m_idx, GF_COLD, damage, 0);
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* RF5_BA_POIS (stinking cloud) */
		case RF5_OFFSET+4:
		{
            disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a cloud of poison.", m_name);
			if (rlev < 25) damage = damroll(12, 2);
			else damage = damroll(12, 2 + ((rlev-10)/15));
			breath(m_idx, GF_POIS, damage, 0);
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

		/* RF5_BA_NETH */
		case RF5_OFFSET+5:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a nether ball.", m_name);
			damage = 50 + damroll(9, 10) + rlev;
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			breath(m_idx, GF_NETHER, damage, 0);
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* RF5_BA_WATE */
		case RF5_OFFSET+6:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s gestures fluidly.", m_name);
			msg_print("You are engulfed in a whirlpool.");
			damage = randint(rlev * 5 / 2) + 40;
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			breath(m_idx, GF_WATER, damage, 0);
			break;
		}

		/* RF5_BA_MANA */
		case RF5_OFFSET+7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
			else msg_format("%^s invokes a mana storm.", m_name);
			damage = (rlev * 4) + damroll(10, 10);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			breath(m_idx, GF_MANA, damage, 0);
			break;
		}

		/* RF5_BA_DARK */
		case RF5_OFFSET+8:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
			else msg_format("%^s invokes a darkness storm.", m_name);
			damage = (rlev * 3) + damroll(10, 10) + randint(rlev);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			breath(m_idx, GF_DARK, damage, 0);
			update_smart_learn(m_idx, DRS_RES_DARK);
			break;
		}

		/* RF5_DRAIN_MANA */
		case RF5_OFFSET+9:
		{
			if (m_ptr->silence) return FALSE;
			if (!direct) break;
			if (p_ptr->csp)
			{
				int r1;
				/* Disturb if legal */
				disturb(1, 0);

				/* Basic message */
				msg_format("%^s draws psychic energy from you!", m_name);

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
                    /* Barbarians and knights have weak mana */
/* ..I heard a mind flayer rampaged your family reunion and starved */
                    if ((cp_ptr->flags & CF_HEAVY_BONUS) || (cp_ptr->flags & CF_KNIGHT))
                    {
					   if (seen)
					   {
						   msg_format("%^s seems unsatisfied.", m_name);
					   }
			           update_smart_learn(m_idx, DRS_MANA);
			           break; /* (monster doesn't get healed) */
                    }
                    
					/* Heal */
					m_ptr->hp += (6 * r1);
					if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

					/* Special message */
					if (seen)
					{
						msg_format("%^s appears healthier.", m_name);
					}
				}
			}
			update_smart_learn(m_idx, DRS_MANA);
			break;
		}

		/* RF5_MIND_BLAST */
		case RF5_OFFSET+10:
		{
			if (!direct) break;
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel something focusing on your mind.");
			}
			else
			{
				msg_format("%^s gazes deep into your eyes.", m_name);
			}

			savechance = 100 + (badluck/2) - (goodluck/4);
			if (p_ptr->timed[TMD_CLEAR_MIND]) savechance -= 15;
			if (r_ptr->flags2 & (RF2_POWERFUL)) savechance += 10;
			if (rand_int(savechance) < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
				if (badluck) take_hit(randint(8), ddesc);
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");
				if (!p_ptr->resist_confu)
				{
					(void)inc_timed(TMD_CONFUSED, rand_int(4) + 4);
				}
				damage = damroll(8, 8);
				take_hit(damage, ddesc);
			}
			break;
		}

		/* RF5_BRAIN_SMASH */
		case RF5_OFFSET+11:
		{
			if (!direct) break;
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel something focusing on your mind.");
			}
			else
			{
				msg_format("%^s looks deep into your eyes.", m_name);
			}

			savechance = 105 + (badluck/2) - (goodluck/4);
			if (p_ptr->timed[TMD_CLEAR_MIND]) savechance -= 15;
			if (r_ptr->flags2 & (RF2_POWERFUL)) savechance += 10;
			if (rand_int(savechance) < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");
				damage = damroll(11, 15);
				take_hit(damage, ddesc);
				if (!p_ptr->resist_blind)
				{
					(void)inc_timed(TMD_BLIND, 8 + rand_int(8));
				}
				if (!p_ptr->resist_confu)
				{
					(void)inc_timed(TMD_CONFUSED, rand_int(4) + 4);
				}
				if (!p_ptr->free_act)
				{
					(void)inc_timed(TMD_PARALYZED, rand_int(4) + 4);
				}
				if (!p_ptr->timed[TMD_SUST_SPEED]) (void)inc_timed(TMD_SLOW, rand_int(4) + 4);
			}
			break;
		}

		/* RF5_CAUSE_1 */
		case RF5_OFFSET+12:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s points at you and curses.", m_name);
			savechance = 105 + (badluck/2) - (goodluck/3);
			if (r_ptr->flags2 & (RF2_POWERFUL)) savechance += 20;
			if (rand_int(savechance) < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				take_hit(damroll(3, 8), ddesc);
			}
			break;
		}

		/* RF5_CAUSE_2 */
		case RF5_OFFSET+13:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s points at you and curses horribly.", m_name);
			savechance = rand_int(104 + (badluck/2) - (goodluck/2));
			if (r_ptr->flags2 & (RF2_POWERFUL)) savechance += 25;
			if (savechance < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				damage = damroll(7, 8);
		        /* extra damage reduction from surrounding magic */
				if (surround) damage -= (damage * (surround / 500));
				take_hit(damage, ddesc);
			}
			break;
		}

		/* RF5_CAUSE_3 */
		case RF5_OFFSET+14:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles loudly.", m_name);
			else msg_format("%^s points at you, incanting terribly!", m_name);
			savechance = rand_int(101 + (badluck/2) - (goodluck/4));
			if (r_ptr->flags2 & (RF2_POWERFUL)) savechance += 25;
			if (savechance < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				damage = damroll(9, 15);
		        /* extra damage reduction from surrounding magic */
				if (surround) damage -= (damage * (surround / 500));
				take_hit(damage, ddesc);
			}
			break;
		}

		/* RF5_CAUSE_4 */
		case RF5_OFFSET+15:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s screams the word 'DIE!'", m_name);
			else msg_format("%^s points at you, screaming the word DIE!", m_name);
			savechance = rand_int(101 + (badluck/2) - (goodluck/4));
			if (r_ptr->flags2 & (RF2_POWERFUL)) savechance += 25;
			if (savechance < p_ptr->skills[SKILL_SAV])
			{
				if ((badluck > 1) && (randint((badluck * 2) + 15) > 9))
				{
				   msg_print("You mostly resist the effects!");
				   (void)inc_timed(TMD_CUT, damroll(6, 6));
                }
				else msg_print("You resist the effects!");
			}
			else
			{
				damage = damroll(14, 15);
		        /* extra damage reduction from surrounding magic */
				if (surround) damage -= (damage * (surround / 500));
				take_hit(damage, ddesc);
				(void)inc_timed(TMD_CUT, damroll(10, 10));
			}
			break;
		}

		/* RF5_BO_ACID */
		case RF5_OFFSET+16:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a acid bolt.", m_name);
			damage = damroll(7, 8) + (rlev / 3);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			bolt(m_idx, GF_ACID, damage);
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* RF5_BO_ELEC */
		case RF5_OFFSET+17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a lightning bolt.", m_name);
			damage = damroll(4, 8) + (rlev / 3);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			bolt(m_idx, GF_ELEC, damage);
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* RF5_BO_FIRE */
		case RF5_OFFSET+18:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a fire bolt.", m_name);
			damage = damroll(9, 8) + (rlev / 3);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			bolt(m_idx, GF_FIRE, damage);
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* RF5_BO_COLD */
		case RF5_OFFSET+19:
		{
			disturb(1, 0);
			if (r_ptr->flags3 & (RF3_HELPER)) 
			{
               msg_format("%^s gives you a frost bolt to throw as a free action.", m_name);
			   spellswitch = 13; /* prevents using old target */
			   if (!get_aim_dir(&dir)) break;
			   /* ("gives you frost bolt" means okay to project from PC) */
			   fire_bolt_or_beam(10 + (goodluck*2), GF_COLD, dir, 
                                 damroll(6, 8) + (rlev / 3));
			   break;
            }
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a frost bolt.", m_name);
			damage = damroll(6, 8) + (rlev / 3);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			bolt(m_idx, GF_COLD, damage);			     
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* RF5_BO_POIS */
		case RF5_OFFSET+20:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a poison bolt.", m_name);
			damage = 3 + damroll(7, 6) + (rlev / 3);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			bolt(m_idx, GF_POIS, damage);
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
   		}

		/* RF5_BO_NETH */
		case RF5_OFFSET+21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a nether bolt.", m_name);
			damage = 30 + damroll(5, 5) + (rlev * 3) / 2;
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 500));
			bolt(m_idx, GF_NETHER, damage);
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* RF5_BO_WATE */
		case RF5_OFFSET+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a water bolt.", m_name);
			if (rlev < 35)
			   {
			      damage = damroll(8, 8) + (rlev/2);
               }
	        else 
               {
			      damage = damroll(10, 8) + (rlev);
               }
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 350));
		    bolt(m_idx, GF_WATER, damage);
			break;
		}

		/* RF5_BO_MANA */
		case RF5_OFFSET+23:
		{
			disturb(1, 0);
			if (r_ptr->flags3 & (RF3_HELPER)) 
			{
               msg_format("%^s asks you who to fire a mana ball at.", m_name);
			   spellswitch = 13; /* prevents using old target */
			   if (!get_aim_dir(&dir)) break;
			   /* not real mana bolt because we don't want to destroy objects */
			   /* maybe this does too much damage? */
			   /* fire_ball(GF_MANA, dir, damroll(50, 3) + randint(25), 1); */
			   damage = damroll(25, 5) + randint(45);
			   /* breath() changes the target to player's target for HELPER monsters */
			   breath(m_idx, GF_MISSILE, damage, 0);
			   break;
            }
			if (blind) msg_format("%^s exerts magic power.", m_name);
			else msg_format("%^s casts a mana bolt.", m_name);
			/* damage for Morgoth's mana bolt is 61-330 instead of 51-350 */
			if (rlev > 90) rlev = 90;
			damage = randint(rlev * 3) + ((rlev * 2)/3);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 350));
			bolt(m_idx, GF_MANA, damage);
			break;
		}

		/* RF5_BO_PLAS */
		case RF5_OFFSET+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a plasma bolt.", m_name);
			damage = 10 + damroll(8, 7) + (rlev);
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 350));
			bolt(m_idx, GF_PLASMA, damage);
			break;
		}

		/* RF5_BO_ICEE */
		case RF5_OFFSET+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts an ice bolt.", m_name);
			damage = damroll(6, 6) + (rlev/2 + randint(rlev/2));
	        /* extra damage reduction from surrounding magic */
			if (surround > 0) damage -= (damage * (surround / 400));
			bolt(m_idx, GF_ICE, damage);
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* RF5_MISSILE */
		case RF5_OFFSET+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a magic missile.", m_name);
			bolt(m_idx, GF_MISSILE,
			     damroll(2, 6) + (rlev / 3));
			break;
		}

		/* RF5_SCARE */
		case RF5_OFFSET+27:
		{
			disturb(1, 0);
			sound(MSG_CAST_FEAR);
			if (r_ptr->flags3 & (RF3_HELPER)) 
			{
			   if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
			   else msg_format("%^s casts a fearful illusion.", m_name);
			   scare_monsters(rlev/2 + randint(rlev/2));
			   break;
            }
			if (!direct) break;
            die = randint(100);
            if (r_ptr->flags2 & (RF2_POWERFUL)) die = die + 25;
            if (p_ptr->timed[TMD_FRENZY]) die = die - 10;
            if (p_ptr->resist_charm) die = die - 5;
			if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
			else msg_format("%^s casts a fearful illusion.", m_name);
			if (p_ptr->resist_fear)
			{
				msg_print("You refuse to be frightened.");
			}
			else if (die < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You refuse to be frightened.");
			}
			else
			{
            	(void)clear_timed(TMD_CHARM);
                if (die > 100)
                {
				     (void)inc_timed(TMD_AFRAID, rand_int(6) + 6);
                }
                else 
                {
				     (void)inc_timed(TMD_AFRAID, rand_int(4) + 4);
                }
			}
			update_smart_learn(m_idx, DRS_RES_FEAR);
			break;
		}

		/* RF5_BLIND */
		case RF5_OFFSET+28:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a spell, burning your eyes!", m_name);
			if (p_ptr->resist_blind)
			{
				msg_print("You are unaffected!");
			}
			else if (r_ptr->flags2 & (RF2_POWERFUL))
	        {
                    if (rand_int(120) < p_ptr->skills[SKILL_SAV])
				    {
                      msg_print("You resist the effects!");
                    }
                    else
                    {
                      (void)set_timed(TMD_BLIND, 12 + rand_int(5));
                    }
            }
			else if (rand_int(100) < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				(void)set_timed(TMD_BLIND, 12 + rand_int(4));
			}
			update_smart_learn(m_idx, DRS_RES_BLIND);
			break;
		}

		/* RF5_CONF */
		case RF5_OFFSET+29:
		{
			if (r_ptr->flags3 & (RF3_HELPER)) 
			{
               if (randint(10) < 3)
               {
                  msg_format("%^s casts confuse monsters.", m_name);
				  spellswitch = 13; /* prevents using old target */
			      if (!get_aim_dir(&dir)) break;
				/* breath() changes the target to player's target for HELPER monsters */
			      breath(m_idx, GF_OLD_CONF, 30 + randint(30), 0);
               }
               else
               {
                  msg_format("%^s asks which monster you want it to confuse.", m_name);
				  spellswitch = 13; /* prevents using old target */
			      if (!get_aim_dir(&dir)) break;
			      (void)confuse_monster(dir, 25 + randint(35));
               }
               break;
            } 
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else msg_format("%^s creates a mesmerising illusion.", m_name);
			if (p_ptr->resist_confu)
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else if (r_ptr->flags2 & (RF2_POWERFUL))
			{
                    if (rand_int(120) < p_ptr->skills[SKILL_SAV])
				    {
                      msg_print("You disbelieve the feeble spell.");
                    }
                    else
                    {
                      (void)inc_timed(TMD_CONFUSED, rand_int(5) + 4);
                    }
            }
			else if (rand_int(100) < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else
			{
				(void)inc_timed(TMD_CONFUSED, rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_RES_CONFU);
			break;
		}

		/* RF5_SLOW */
		case RF5_OFFSET+30:
		{
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s drains power from your muscles!", m_name);
			if ((p_ptr->free_act) || (p_ptr->timed[TMD_SUST_SPEED]))
			{
				msg_print("You are unaffected!");
			}
			else if (r_ptr->flags2 & (RF2_POWERFUL))
			{
                    if (rand_int(120) < p_ptr->skills[SKILL_SAV])
				    {
                      msg_print("You resist the effects!");
                    }
                    else
                    {
                      (void)inc_timed(TMD_SLOW, rand_int(5) + 4);
                    }
            }
			else if (rand_int(100) < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				(void)inc_timed(TMD_SLOW, rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}

		/* RF5_HOLD */
		case RF5_OFFSET+31:
		{
			if (r_ptr->flags3 & (RF3_HELPER)) 
			{
               msg_format("%^s casts sleep monsters.", m_name);
               (void)sleep_monsters(25 + randint(20));
               break;
            } 
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s stares deep into your eyes!", m_name);
			if (p_ptr->free_act)
			{
				msg_print("You are unaffected!");
			}
			else if (r_ptr->flags2 & (RF2_POWERFUL))
			{
                 if (rand_int(110) < p_ptr->skills[SKILL_SAV])
				 {
                     msg_print("You resist the effects!");
                 }
                 else
                 {
                     (void)inc_timed(TMD_PARALYZED, rand_int(5) + 4);
                 }
            }
			else if (rand_int(100) < p_ptr->skills[SKILL_SAV])
			{
				msg_format("You resist the effects!");
			}
			else
			{
				(void)inc_timed(TMD_PARALYZED, rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}



		/* RF6_HASTE */
		case RF6_OFFSET+0:
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

			/* Allow quick speed increases to base+10 */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				msg_format("%^s starts moving faster.", m_name);
				m_ptr->mspeed += 10;
			}

			/* Allow small speed increases to base+20 */
			else if (m_ptr->mspeed < r_ptr->speed + 20)
			{
				msg_format("%^s moves even faster.", m_name);
				m_ptr->mspeed += 2;
			}

			break;
		}

		/* RF6_HEAL_KIN */
		case RF6_OFFSET+1:
		{
			int healmon;
			if (m_ptr->silence) return FALSE;
			if (seen) msg_format("%^s casts a spell to heal %s kin.", m_name, m_poss);
			disturb(1, 0);
			healmon = 1 + rlev/2 + randint((rlev * 3) / 2);
			/* simulates project_los with GF_OLD_HEAL */
			(void)heal_monsters(healmon, m_ptr, TRUE);
			break;
		}

		/* RF6_HEAL */
		case RF6_OFFSET+2:
		{
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);

			/* Message */
			if (seen)
			{
				msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
			}
			else if (m_ptr->cdis < 10)
			{
				msg_format("%^s mumbles.", m_name);
			}

			/* Heal some */
			m_ptr->hp += (s32b)(rlev * 4) + randint(rlev * 2);

			/* Fully healed */
			if (m_ptr->hp >= m_ptr->maxhp)
			{
				/* Fully healed */
				m_ptr->hp = m_ptr->maxhp;

				/* Message */
				if (seen) msg_format("%^s looks fully healthy!", m_name);
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (seen) msg_format("%^s looks healthier.", m_name);
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				m_ptr->monfear = 0;

				/* Message */
				if (seen) msg_format("%^s recovers %s courage.", m_name, m_poss);
			}
			if (r_ptr->flags3 & (RF3_HELPER))
			{
               msg_format("%^s heals you also.", m_name);
               (void)hp_player(20 + randint(30));
            } 
			break;
		}

		/* RF6_HEAL_OTHR */
		case RF6_OFFSET+3:
		{
			int healmon;
			if (m_ptr->silence) return FALSE;
			if (seen) msg_format("%^s casts a spell of mass healing.", m_name);
			disturb(1, 0);
			healmon = 1 + rlev/2 + randint((rlev * 3) / 2);
			/* simulates project_los with GF_OLD_HEAL */
			(void)heal_monsters(healmon, m_ptr, FALSE);
			break;
		}

		/* RF6_BLINK */
		case RF6_OFFSET+4:
		{
			disturb(1, 0);
			if (seen) msg_format("%^s blinks away.", m_name);
			/* if monster is afraid, try to blink away from the player */
			if (m_ptr->monfear) teleport_away(m_idx, 10, 3);
			else teleport_away(m_idx, 10, 0);
			m_ptr->bold = FALSE;
			break;
		}

		/* RF6_TPORT */
		case RF6_OFFSET+5:
		{
			disturb(1, 0);
			if (seen) msg_format("%^s teleports away.", m_name);
			/* monster can hide again after teleporting (done in teleport_away() ) */
			teleport_away(m_idx, MAX_SIGHT * 2 + 6, 0);
			m_ptr->bold = FALSE;
			break;
		}

		/* RF6_XXX3X6 - RF6_INVIS */
		case RF6_OFFSET+6:
		{
			disturb(1, 0);
			if ((seen) && (!p_ptr->see_inv)) msg_format("%^s dissapears!", m_name);
			m_ptr->monseen = 0;
			m_ptr->tinvis = randint(4) + 3 + (rlev/9);
			/* monster can hide again after turning invisible */
			if (!p_ptr->see_inv)
			{
			    if (m_ptr->monseen > 2) m_ptr->monseen -= randint(2);
				else if ((m_ptr->monseen) && (rand_int(100) < r_ptr->stealth*8))
				  m_ptr->monseen -= 1;
			}
			break;
		}

		/* RF6_XXX4X6 */
		case RF6_OFFSET+7:
		{
			break;
		}

		/* RF6_TELE_TO */
		case RF6_OFFSET+8:
		{
			/* if monster is afraid, then it doesn't want to bring the player closer */
			if (m_ptr->monfear) break;
			if (!direct) break;
			/* demon ward spell protects from TELE_TO from demons */
			if ((r_ptr->flags3 & (RF3_DEMON)) && (p_ptr->timed[TMD_DEMON_WARD]))
			{
			    if (seen) msg_format("%^s attempts to cast a spell, but fails.", m_name);
			    break;
            }
			disturb(1, 0);
			msg_format("%^s commands you to return.", m_name);
			teleport_player_to(m_ptr->fy, m_ptr->fx);
			break;
		}

		/* RF6_TELE_AWAY */
		case RF6_OFFSET+9:
		{
			if (!direct) break;
			/* TELE_AWAY has very short range in town */
			if ((!p_ptr->depth) && (m_ptr->cdis > 5)) break;
			disturb(1, 0);
			msg_format("%^s tells you to go away.", m_name);
			controlled = FALSE;
			/* controlled teleport (much harder when caused by a monster) */
		    if (p_ptr->telecontrol)
		    {
                int resistcrtl = (rlev * 3) / 2;
                if (resistcrtl < 50) resistcrtl = 50;
                if (control_tport(resistcrtl, 101)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(100);
			/* monster can hide again after teleporting you away */
		    if (m_ptr->monseen > 2) m_ptr->monseen -= 1;
			else if ((m_ptr->monseen > 1) && (rand_int(100) < r_ptr->stealth*5))
			  m_ptr->monseen -= 1;
			break;
		}

		/* RF6_TELE_LEVEL */
		case RF6_OFFSET+10:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles strangely.", m_name);
			else msg_format("%^s gestures at your feet.", m_name);
			savechance = 100 + badluck - (goodluck/2);
			if (p_ptr->resist_nexus)
			{
				msg_print("You are unaffected!");
			}
			else if (rand_int(savechance) < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				teleport_player_level();
			}
			update_smart_learn(m_idx, DRS_RES_NEXUS);
			break;
		}

		/* RF6_XXX5  - now RF6_CURSE_PC */
		case RF6_OFFSET+11:
		{
			int savet;
			if (!direct) break;
			disturb(1, 0);
			/* This message can be the same, blind or not */
			/*if (blind) msg_format("%^s mumbles.", m_name);
			else*/ msg_format("%^s whispers a curse of misfortune.", m_name);
			savet = (p_ptr->skills[SKILL_SAV] * 3) / 4;
			if (r_ptr->flags2 & (RF2_POWERFUL)) savet -= 20;
			/* blessing guards against curses */
			if (p_ptr->timed[TMD_BLESSED]) savet += 17;
			/* the luck factor */
			savet += goodluck - badluck;
			if (rand_int(100) < savet)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				int rlevb = ((rlev > 40) ? 40 : rlev);
				int diebad;
				if (p_ptr->depth > rlevb+10) rlevb += (p_ptr->depth - (rlevb+9)) / 2;
				diebad = randint(50 + (rlevb*2) + badluck - goodluck);
				if (p_ptr->timed[TMD_BLESSED]) diebad -= 10;
				if ((diebad >= 75) && (randint(100) < 35))
				{
					msg_print("You feel especially unlucky");
					(void)inc_timed(TMD_WITCH, randint(diebad - 85) + 1);
				}
				else if ((diebad > 70) && (randint(100) < 40))
				{
					msg_print("You feel especially unlucky");
					p_ptr->luck -= 1;
				}
				/* cap duration */
				if (diebad > 70) diebad = 69 + randint(6);
				if (diebad < 0) msg_print("You feel like you got lucky");
				else (void)inc_timed(TMD_CURSE, randint(diebad) + 25);
			}
			break;
		}

		/* RF6_DARKNESS */
		case RF6_OFFSET+12:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s gestures in shadow.", m_name);
			(void)unlite_area(0, 3, FALSE);
			break;
		}

		/* RF6_TRAPS */
		case RF6_OFFSET+13:
		{
			if (!direct) break;
			disturb(1, 0);
			sound(MSG_CREATE_TRAP);
			if (blind) msg_format("%^s mumbles, and then cackles evilly.", m_name);
			else msg_format("%^s casts a spell and cackles evilly.", m_name);
			(void)trap_creation();
			break;
		}

		/* RF6_FORGET */
		case RF6_OFFSET+14:
		{
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s tries to blank your mind.", m_name);

            savechance = 100 - ((goodluck+1)/3);
            if (r_ptr->flags2 & (RF2_POWERFUL)) savechance += 10;
			if (rand_int(savechance) < p_ptr->skills[SKILL_SAV])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				if (rlev < 20) inc_timed(TMD_AMNESIA, 4 + randint(5));
				else inc_timed(TMD_AMNESIA, (rlev/4) + randint(rlev/4));
				/* re-added forgetting the map which had been removed (less than 50% chance) */
				if (randint((200-savechance) + p_ptr->skills[SKILL_SAV]/2) < 45 + ((badluck+1)/2)) wiz_dark();
			}
			break;
		}

		/* RF6_S_SILVER */
		case RF6_OFFSET+15:
		{
			int some = randint(rlev/10) + 1;
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_ANGEL);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s summons silver grepse.", m_name);
			for (k = 0; k < some; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_SILVER);
			}
			if (blind && count)
			{
				msg_print("You hear sweet deceitful singing.");
			}
			break;
		}

		/* RF6_S_KIN */
		case RF6_OFFSET+16:
		{
			if (m_ptr->silence) return FALSE;
			/* no summoning kin in the town */
			if (!p_ptr->depth) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_MONSTER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons %s %s.", m_name, m_poss,
			                ((r_ptr->flags1) & RF1_UNIQUE ?
			                 "minions" : "kin"));

			/* Hack -- Set the letter of the monsters to summon */
			summon_kin_type = r_ptr->d_char;
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_KIN);
			}
			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_HI_DEMON */
		case RF6_OFFSET+17:
		{
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_HI_DEMON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater demons!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HI_DEMON);
			}
			if (blind && count)
			{
				msg_print("You hear many evil things appear nearby.");
			}
			break;
		}

		/* RF6_S_MONSTER */
		case RF6_OFFSET+18:
		{
			int styp = 0;
            if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_MONSTER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons help!", m_name);
			
			/* To prevent monsters summoning contradictory monsters */
			if (r_ptr->flags3 & (RF3_HURT_FIRE)) styp = SUMMON_NOFIRE;
			else if (r_ptr->flags7 & (RF7_HATE_WATER)) styp = SUMMON_NOWATER;
			else if ((r_ptr->flags3 & (RF3_EVIL)) ||
				(r_ptr->flags3 & (RF3_HURT_LITE))) styp = SUMMON_NOLIGHT;
			
			for (k = 0; k < 1; k++)
			{
				/* exception for Scroll of Aquirement town mimmic */
                if ((r_ptr->level == 0) && (p_ptr->max_depth > 4)) count += summon_specific(m_ptr->fy, m_ptr->fx, p_ptr->max_depth/4 + 1, styp);
                else count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, styp);
			}
			if (blind && count)
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_MONSTERS */
		case RF6_OFFSET+19:
		{
			int samt = 6 + randint(2); /* 7-8 */
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_MONSTER);
			if (randint(100) < (goodluck + 1) * 2)
			{
				if (goodluck > 2) samt -= randint(goodluck/3 + 1);
				else samt -= 1;
			}
			if ((randint(100) < (badluck + 1) * 2) && 
				((samt < 8) || (badluck > 5))) samt += 1;
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons monsters!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, 0);
			}
			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_ANIMAL */
		case RF6_OFFSET+20:
		{
			int styp = SUMMON_ANIMAL;
            if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_ANIMAL);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons animals.", m_name);
			
			/* To prevent monsters summoning contradictory monsters */
			if (r_ptr->flags3 & (RF3_HURT_FIRE)) styp = SUMMON_ANI_NOFIRE;
			else if (r_ptr->flags7 & (RF7_HATE_WATER)) styp = SUMMON_ANI_NOWATER;
			else if ((r_ptr->flags3 & (RF3_EVIL)) ||
				(r_ptr->flags3 & (RF3_HURT_LITE))) styp = SUMMON_ANI_NOLIGHT;

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, styp);
			}
			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_SPIDER */
		case RF6_OFFSET+21:
		{
			int samt = 6;
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_SPIDER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons spiders.", m_name);
			if (randint(100) < (goodluck + 1) * 2) samt -= 1;
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_SPIDER);
			}
			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_HOUND */
		case RF6_OFFSET+22:
		{
			int samt = 6;
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_HOUND);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hounds.", m_name);
			if (randint(100) < (goodluck + 1) * 2) samt -= 1;
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HOUND);
			}
			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_HYDRA */
		case RF6_OFFSET+23:
		{
			int samt = 6;
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_HYDRA);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hydras.", m_name);
			if (randint(100) < (goodluck + 5) * 2) samt -= randint(2);
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HYDRA);
			}
			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_ANGEL (summon ape) */
		case RF6_OFFSET+24:
		{
			int samt = 1 + rand_int(2);
		    cptr rname = (r_name + r_ptr->name);
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_ANIMAL);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons something hairy!", m_name);
			if ((randint(100) < (goodluck + 1) * 2) && (samt > 1)) samt -= 1;
			if (randint(100) < (badluck + 1) * 2) samt += 1;
			/* spellswitch 40: The Wicked witch of the West */
            /* prefers winged monkeys */
			if (strstr(rname, "West")) spellswitch = 40;
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_ANGEL);
			}
			if (blind && count)
			{
				msg_print("You hear something appear nearby ..and it spells like a monkey.");
			}
			/* reset the global hack */
			spellswitch = 0;
			break;
		}

		/* RF6_S_DEMON */
		case RF6_OFFSET+25:
		{
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_DEMON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a hellish adversary!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_DEMON);
			}
			if (blind && count)
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_UNDEAD */
		case RF6_OFFSET+26:
		{
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_UNDEAD);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons an undead adversary!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_DRAGON */
		case RF6_OFFSET+27:
		{
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_DRAGON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a dragon!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_DRAGON);
			}
			if (blind && count)
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_HI_UNDEAD */
		case RF6_OFFSET+28:
		{
			int samt = 8;
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_HI_UNDEAD);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater undead!", m_name);
			if (randint(100) < (goodluck + 1) * 2) samt -= 1;
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many powerful creepy things appear nearby.");
			}
			break;
		}

		/* RF6_S_HI_DRAGON */
		case RF6_OFFSET+29:
		{
			int samt = 8;
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_HI_DRAGON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons ancient dragons!", m_name);
			if (randint(100) < (goodluck + 1) * 2) samt -= 1;
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HI_DRAGON);
			}
			if (blind && count)
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}

		/* RF6_S_WRAITH */
		case RF6_OFFSET+30:
		{
			int samt = 8;
			/* silence summons can't stop Sauron from summoning his ringwraiths */
            if ((m_ptr->silence) && (rlev < 99)) return FALSE;
            
			disturb(1, 0);
			sound(MSG_SUM_WRAITH);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons mighty undead!", m_name);
			if (randint(100) < (goodluck + 1) * 2) samt -= 1;
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HI_UNDEAD);
			}
			if (randint(100) < (goodluck + 1) * 2) samt -= 1;
			for (k = 0; k < samt + 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_WRAITH);
			}
			if (blind && count)
			{
				msg_print("You hear mighty creepy things appear nearby.");
			}
			break;
		}

		/* RF6_S_UNIQUE */
		case RF6_OFFSET+31:
		{
			int samt = 8;
			if (m_ptr->silence) return FALSE;
			disturb(1, 0);
			sound(MSG_SUM_UNIQUE);
			if (randint(100) < (goodluck + 1) * 2) samt -= 1;
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_UNIQUE);
			}
			if (blind) msg_format("%^s mumbles.", m_name);
			else if (count) msg_format("%^s magically summons special opponents!", m_name);
			else msg_format("%^s magically summons special monsters!", m_name);
			for (k = 0; k < samt; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}
	}


	/* Remember what the monster did to us */
	if (seen)
	{
		/* Innate spell */
		if (thrown_spell < 32*4)
		{
			l_ptr->flags4 |= (1L << (thrown_spell - 32*3));
			if (l_ptr->cast_innate < MAX_UCHAR) l_ptr->cast_innate++;
			
			if (r_ptr->flags2 & (RF2_BR_STRONG)) l_ptr->flags2 |= (RF2_BR_STRONG);
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32*5)
		{
			l_ptr->flags5 |= (1L << (thrown_spell - 32*4));
			if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32*6)
		{
			l_ptr->flags6 |= (1L << (thrown_spell - 32*5));
			if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
		}
	}


	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
	{
		l_ptr->deaths++;
	}


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
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u16b p_lev, m_lev;
	u16b p_chp, p_mhp;
	u16b m_chp, m_mhp;
	u32b p_val, m_val;

	/* Keep monsters from running too far away */
	if (m_ptr->cdis > MAX_SIGHT + 6) return (FALSE);
	
	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);
	
	/* monsters with no options never run away */
	if (m_ptr->bold) return (FALSE);

	/* mindless undead should never run away (unless turn undead is used) */
	if ((r_ptr->flags3 & (RF3_UNDEAD)) && (r_ptr->flags2 & (RF2_STUPID))) return (FALSE);

	/* Nearby monsters will not become terrified */
	if (m_ptr->cdis < 5) return (FALSE);

	/* Examine player power (level) */
	p_lev = p_ptr->lev;

	/* Examine monster power (level plus morale) */
	m_lev = r_ptr->level + (m_idx & 0x08) + 25;

	/* undead are less likely to run away */
	if (r_ptr->flags3 & (RF3_UNDEAD)) m_lev += 6;

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

/* finds whether a monster is near a permanent wall
 * this decides whether PASS_WALL & KILL_WALL monsters 
 * use the monster flow code
 */
static bool near_permwall(const monster_type *m_ptr)
{
	int y, x;
	int my = m_ptr->fy;
	int mx = m_ptr->fx;
	
	/* if PC is in LOS, there's no need to go around walls */
    if (projectable(my, mx, p_ptr->py, p_ptr->px)) return FALSE;
    
    /* PASS_WALL & KILL_WALL monsters occationally flow for a turn anyway */
    if (rand_int(100) < 2) return TRUE;
    
	/* Search the nearby grids, which are always in bounds */
	for (y = (my - 2); y <= (my + 2); y++)
	{
		for (x = (mx - 2); x <= (mx + 2); x++)
		{
            if (!in_bounds_fully(y, x)) continue;
            /* vault walls are always FEAT_PERM_INNER */
            if ((cave_feat[y][x] == FEAT_PERM_INNER)) return TRUE;
		}
	}
	return FALSE;
}

#ifdef MONSTER_FLOW

/*
 * Choose the "best" direction for "flowing"
 *
 * Note that ghosts and rock-eaters are (almost) never allowed to 
 * "flow", since they should move directly towards the player.
 *
 * Prefer non-diagonal directions, but twiddle them a little
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
	
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i, y, x, y1, x1;

	int when = 0;
	int cost = 999;

	/* Monster flowing disabled */
	if (!adult_ai_sound) return (FALSE);

	/* Monster can go through rocks */
	if ((r_ptr->flags2 & (RF2_PASS_WALL)) || (r_ptr->flags2 & (RF2_KILL_WALL)))
	{
         /* still have to go around permanent walls */
         if (!near_permwall(m_ptr)) return (FALSE);
    }

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* Hack -- Player can see us, run towards him */
	if (player_has_los_bold(y1, x1)) return (FALSE);

	/* The player is not currently near the monster grid */
	if (cave_when[y1][x1] < cave_when[py][px])
	{
		/* The player has never been near the monster grid */
		if (cave_when[y1][x1] == 0) return (FALSE);

		/* The monster is not allowed to track the player */
		/* (allow monsters to flow towards a fake target anyway) */
		if (!adult_ai_smell) return (FALSE);
	}

	/* Monster is too far away to notice the player */
	if (cave_cost[y1][x1] > MONSTER_FLOW_DEPTH) return (FALSE);
	if (cave_cost[y1][x1] > r_ptr->aaf) return (FALSE);

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
		(*yp) = py + 16 * ddy_ddd[i];
		(*xp) = px + 16 * ddx_ddd[i];
	}

	/* No legal move (?) */
	if (!when) return (FALSE);

	/* Success */
	return (TRUE);
}

#ifdef MONSTER_AI

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
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster flowing disabled */
	if (!adult_ai_sound) return (FALSE);

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

#endif /* MONSTER_AI */

#endif /* MONSTER_FLOW */


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

#endif /* MONSTER_AI */


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

#ifdef MONSTER_AI
#ifdef MONSTER_FLOW

	monster_type *m_ptr = &mon_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, dy, dx, d, dis;
	int gy = 0, gx = 0, gdis = 0;

	const int *y_offsets;
	const int *x_offsets;

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
			if (adult_ai_sound)
			{
				/* Ignore grids very far from the player */
				if (cave_when[y][x] < cave_when[py][px]) continue;

				/* Ignore too-distant grids */
				if (cave_cost[y][x] > cave_cost[fy][fx] + 2 * d) continue;
			}

			/* Check for absence of shot (more or less) */
			if (!player_has_los_bold(y,x))
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

#endif /* MONSTER_FLOW */
#endif /* MONSTER_AI */

	/* No safe place */
	return (FALSE);
}


#ifdef MONSTER_AI

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

	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, dy, dx, d, dis;
	int gy = 0, gx = 0, gdis = 999, min;

	const int *y_offsets, *x_offsets;

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
		     dx != 0 || dy != 0;
		     i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Skip occupied locations */
			if (!cave_can_occupy_bold(y, x)) continue;

			/* Check for hidden, available grid */
			if (!player_has_los_bold(y, x) && (clean_shot(fy, fx, y, x, FALSE)))
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

#endif /* MONSTER_AI */

/*
 *  Get a random location (Straight from the teleportation code)
 *  Used to get a roaming monster's destination
 *  (Here's my ASS: artificial stupidity system)
 * 
 *  Water tries to find a destination spot in water.
 */
static bool get_random_des(int dy, int dx, int *toy, int *tox, bool water)
{ 
    int dis, min, tries;
	int i, x, y, d;
	bool look, extended = FALSE;

	dis = 120;
    
	/* if you're lucky, the destination is a spot far away from the player */
    if (randint(goodluck + 50) > 40)
    {
		dy = p_ptr->py;
		dx = p_ptr->px;
		dis += randint(30);
	}

	look = TRUE;

	  /* Initialize */
	  y = dy;
	  x = dx;

	  /* Minimum distance */
	  min = dis / 2;

	  /* never too close if a water monster needs water */
	  if (water) min = 0;

 	  /* Look until done */
	  while (look)
	  {
		/* Verify max distance */
		if (dis > 200) dis = 200;
		
		tries = 500;

		/* Try several locations */
		for (i = 0; i < tries; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(dy, dis);
				x = rand_spread(dx, dis);
				d = distance(dy, dx, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Require 'can_occupy' floor space */
			if (!cave_can_occupy_bold(y, x)) continue;

			/* Water monster is looking for a water destination */
			/* (it's possible that there's no water on the level so it */
			/*  has to give up looking eventually) */
			if ((water) && (!(cave_feat[y][x] == FEAT_WATER)) && ((i < 300) ||
				(!extended)))
				continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = (dis * 3)/2;
		extended = TRUE;

		/* Decrease the minimum distance */
		if (min > 1) min = min / 2;
      }

	  /* found a destination */
      (*toy) = y;
      (*tox) = x;
      return TRUE;
}

/*
 * Choose "logical" directions for monster movement
 *
 * We store the directions in a special "mm" array
 */
static bool get_moves(int m_idx, int mm[5])
{
	int y, ay, x, ax, y2, x2;
	int move_val;
	bool done, needwater = FALSE;
	bool friendflag = FALSE;

	int py = p_ptr->py;
	int px = p_ptr->px;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
    
	/* start trying to get to the player again if temporarily roaming */
	if ((m_ptr->roaming > 20) && (!m_ptr->csleep) && (randint(100) < 16)) 
		m_ptr->roaming = 0;

	/* WATER_ONLY monster out of water tries to get back to water */
	if ((r_ptr->flags7 & (RF7_WATER_ONLY)) &&
		(!(cave_feat[m_ptr->fy][m_ptr->fx] == FEAT_WATER))) needwater = TRUE;

	/* get roaming destination (go towards somewhere besides the player) */
   	if ((m_ptr->roaming) || (m_ptr->truce) || (needwater))
	{
        /* do we need a destination? */
   	    bool roam_to = FALSE;
       	int groupdes = 0;
        
        if (m_ptr->roaming == 9)
   	    {
           /* monster has been running into a wall so get a new destination */
   	       roam_to = TRUE;

			/* reset roaming */
			if (m_ptr->roaming == 9) m_ptr->roaming = 1;
       	}
        
        /* roaming in groups */
   	    if ((m_ptr->roaming > 10) && (m_ptr->roaming < 19) && 
       	   (m_ptr->roam_to_y) && (m_ptr->roam_to_x))
        {
   	       /* already has a destination */
       	}
        /* roaming groups destinations */
        else if ((m_ptr->roaming > 10) && (m_ptr->roaming < 20))
        {
           /* where is the rest of the group going? */
           if (strchr("pt", r_ptr->d_char)) groupdes = roamgroup1;
           else if (strchr("q", r_ptr->d_char)) groupdes = roamgroup2;
   	       else if (strchr("hS", r_ptr->d_char)) groupdes = roamgroup3;
       	   else if (strchr("Y", r_ptr->d_char)) groupdes = roamgroup4;
           else if (strchr("xT", r_ptr->d_char)) groupdes = roamgroup5;
   	       else if (strchr("oO", r_ptr->d_char)) groupdes = roamgroup6;
       	   else if (strchr("C", r_ptr->d_char)) groupdes = roamgroup7;
           else groupdes = roamgroup8;
   	       /* no destination */
       	   if (!groupdes) roam_to = TRUE;
           else 
   	       {
       	       /* convert groupdes to roam_to_x and y */
           	   m_ptr->roam_to_y = (groupdes/1000);
               m_ptr->roam_to_x = groupdes - (m_ptr->roam_to_y * 1000);
   	       }
       	}
        
        /* no current destination so we need one */ 
   	    if ((!m_ptr->roam_to_y) || (!m_ptr->roam_to_x)) roam_to = TRUE;
        
		if (!roam_to)
		{
			/* we're gotten to our destination, so we need a new one */ 
			if ((m_ptr->fy == m_ptr->roam_to_y) && (m_ptr->fx == m_ptr->roam_to_x))
				roam_to = TRUE;
       	}
        
        /* get a new 'roam_to_y' and 'roam_to_x' */
   	    if (roam_to)
       	{
            int toy, tox;
   	        (void)get_random_des(m_ptr->fy, m_ptr->fx, &toy, &tox, needwater);
                 
       	    m_ptr->roam_to_y = toy;
           	m_ptr->roam_to_x = tox;
        }
       
   	    /* destination */
       	py = m_ptr->roam_to_y;
        px = m_ptr->roam_to_x;
        
   	    /* roaming in groups (convert and save group destination */
       	/*  so the rest of the group will know where to go) */
        if ((m_ptr->roaming > 10) && (m_ptr->roaming < 20) && (roam_to))
   	    {
       	   groupdes = (m_ptr->roam_to_y * 1000);
       	   groupdes += m_ptr->roam_to_x;
           if (strchr("pt", r_ptr->d_char)) roamgroup1 = groupdes;
           else if (strchr("qZ", r_ptr->d_char)) roamgroup2 = groupdes;
           else if (strchr("hx@", r_ptr->d_char)) roamgroup3 = groupdes;
           else if (strchr("Y", r_ptr->d_char)) roamgroup4 = groupdes;
           else if (strchr("T", r_ptr->d_char)) roamgroup5 = groupdes;
           else if (strchr("oO", r_ptr->d_char)) roamgroup6 = groupdes;
           else if (strchr("C", r_ptr->d_char)) roamgroup7 = groupdes;
   	       else roamgroup8 = groupdes;
   	    }
    }
    
	move_val = 0;
	y2 = py;
	x2 = px;

	done = FALSE;

#ifdef MONSTER_FLOW

	/* Flow towards the player */
	/* (This is unusable for roaming monsters because of monster flow */
	/* stuff in cave.c is always directed at the PC) */
	if ((adult_ai_sound) && (!m_ptr->roaming))
	{
		/* Flow towards the player */
		(void)get_moves_aux(m_idx, &y2, &x2);
	}
#endif /* MONSTER_FLOW */

	/* Extract the "pseudo-direction" */
	y = m_ptr->fy - y2;
	x = m_ptr->fx - x2;

#ifdef MONSTER_AI

	if ((r_ptr->flags1 & RF1_FRIENDS) || (r_ptr->flags2 & RF2_FRIEND1))
		friendflag = TRUE;

	/* Normal animal packs try to get the player out of corridors. */
	if (adult_ai_packs && (!m_ptr->roaming) && (!m_ptr->truce) &&
    	(friendflag) && (r_ptr->flags3 & RF3_ANIMAL) &&
	    !((r_ptr->flags2 & (RF2_PASS_WALL)) || (r_ptr->flags2 & (RF2_KILL_WALL))))
	{
		int i, room = 0;

		/* Count room grids next to player */
		for (i = 0; i < 8; i++)
		{
			/* Check grid */
			if (cave_info[py + ddy_ddd[i]][px + ddx_ddd[i]] & (CAVE_ROOM))
			{
				/* One more room grid */
				room++;
			}
		}

		/* Not in a room and strong player */
		if ((room < 4) && (p_ptr->chp > p_ptr->mhp / 2) &&
			(p_ptr->lev > r_ptr->level/3))
		{
			/* Find hiding place */
			if (find_hiding(m_idx, &y, &x)) done = TRUE;
		}
	}

#endif /* MONSTER_AI */

	/* Apply fear */
	if ((!done) && (!m_ptr->roaming) && (mon_will_run(m_idx)))
	{
		/* Try to find safe place */
		if (!(adult_ai_smart && find_safety(m_idx, &y, &x)))
		{
			/* This is not a very "smart" method XXX XXX */
			y = (-y);
			x = (-x);
		}

#ifdef MONSTER_AI
#ifdef MONSTER_FLOW

		else
		{
			/* Attempt to avoid the player */
			if (adult_ai_sound)
			{
				/* Adjust movement */
				get_fear_moves_aux(m_idx, &y, &x);
			}
		}

#endif /* MONSTER_FLOW */
#endif /* MONSTER_AI */

		done = TRUE;
	}


#ifdef MONSTER_AI

	/* Monster groups try to surround the player */
	if (!done && adult_ai_packs && (friendflag) && 
		(!m_ptr->roaming) && (!m_ptr->truce))
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
			if (!cave_can_occupy_bold(y2, x2)) continue;

			/* Try to fill this hole */
			break;
		}

		/* Extract the new "pseudo-direction" */
		y = m_ptr->fy - y2;
		x = m_ptr->fx - x2;
	}

#endif /* MONSTER_AI */

	/* Check for no move */
	if (!x && !y) return (FALSE);

	/* Extract the "absolute distances" */
	ax = ABS(x);
	ay = ABS(y);

	/* Do something weird */
	if (y < 0) move_val += 8;
	if (x > 0) move_val += 4;

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
static int compare_monsters(const monster_type *m_ptr, const monster_type *n_ptr, bool killm)
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

	/* don't push past monsters of the same race */
	if ((!killm) && (r_ptr->flags2 & (RF2_MOVE_BODY))) mexp2 *= 2;

	/* Compare */
	if (mexp1 < mexp2) return (-1);
	if (mexp1 > mexp2) return (1);

	/* Assume equal */
	return (0);
}

/* Staff of zapping effect */
/* return damage and do damage later */
int do_zapping(const monster_type *m_ptr, int y, int x)
{
	cptr note_dies; 
	bool fear;          
	 /* get stuff */
     /* monster_type *m_ptr = &mon_list[m_idx]; */
	 monster_race *r_ptr = &r_info[m_ptr->r_idx];
	 monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	 int zap = 0;
	int odds = 4;
	if ((m_ptr->csleep) && (m_ptr->roaming)) odds = 5;
	else if (m_ptr->csleep) odds = 8;

	/* Prepare death note */
	note_dies = " dies.";
	/* Some monsters get "destroyed" */
	if ((r_ptr->flags3 & (RF3_DEMON)) ||
      (r_ptr->flags2 & (RF2_STUPID)) ||
	     (r_ptr->flags3 & (RF3_NON_LIVING)))
	{
		/* Special note at death */
		note_dies = " is destroyed.";
	}
	 
     if ((!cave_floor_bold(y, x)) && (m_ptr->cdis > 1) && (randint(odds+1) == 1))
     {
         /* in case of PASS_WALL monsters */
         msg_print("A spark hits the wall.");
     }
     else if ((randint(odds) == 1) && (player_has_los_bold(y, x)))
     {
	     /* Get the monster name */
		 char m_name[80];
		 monster_desc(m_name, sizeof(m_name), m_ptr, 0);
                          
         msg_format("%^s is zapped with electricity.", m_name);
         if (p_ptr->lev > 13) zap = randint(p_ptr->lev/3) + (p_ptr->lev/12);
         else zap = randint(4);
         if (p_ptr->resist_elec) zap += randint(p_ptr->lev/4 + 1);
         if (cp_ptr->flags & CF_POWER_SHIELD) zap += (goodluck/2) + (p_ptr->lev/10);
         if (r_ptr->flags3 & (RF3_BUG)) zap += randint(p_ptr->lev/3 + 1);

		 if (r_ptr->flags3 & (RF3_IM_ELEC))
	     {
            if (m_ptr->ml)
            {
               msg_format("%^s resists.", m_name);
               l_ptr->flags3 |= (RF3_IM_ELEC);
            } 
            zap = zap/3;
         }                  
     }     
	
	if (!zap) return 0;
	/* true if monster dies (returns anyway) */
	fear = FALSE;
	(void)mon_take_hit(cave_m_idx[y][x], zap, &fear, note_dies);

    return zap;
}

/* Daylight effect (similar to zapping) */
bool do_daylight(int m_idx, int ny, int nx)
{
	 char m_name[80];
	 cptr note_dies;
	 bool fear;
     /* get stuff */
     monster_type *m_ptr = &mon_list[m_idx];
	 monster_race *r_ptr = &r_info[m_ptr->r_idx];
	 monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	 int ldam, chance;

	 /* if not hurt by light, then doesn't hurt as often */
	 if (!r_ptr->flags3 & (RF3_HURT_LITE)) chance = 4;
	 else chance = 3;

     if (cave_info[ny][nx] & (CAVE_WALL))
     {
        /* in case of PASS_WALL monsters (skip because it makes it crash) */
        return FALSE;
     }
     else if ((randint(chance) == 1) && (player_has_los_bold(ny, nx)))
     {
	      if ((r_ptr->flags3 & (RF3_HURT_LITE)) && (r_ptr->flags3 & (RF3_UNDEAD)))
	      {
             ldam = damroll(2, (p_ptr->lev/6));
             if (m_ptr->ml)
             {
                l_ptr->flags3 |= (RF3_HURT_LITE);
                l_ptr->flags3 |= (RF3_UNDEAD);
             }
          }
	      else if (r_ptr->flags3 & (RF3_HURT_LITE))
	      {
             ldam = damroll(1, (p_ptr->lev/4));
             if (m_ptr->ml) l_ptr->flags3 |= (RF3_HURT_LITE);
          }
	      else if (r_ptr->flags3 & (RF3_UNDEAD))
	      {
             ldam = damroll(1, (p_ptr->lev/5));
             if (m_ptr->ml) l_ptr->flags3 |= (RF3_UNDEAD);
          }
	      else if (r_ptr->flags3 & (RF3_DEMON))
	      {
             ldam = damroll(1, (p_ptr->lev/6));
             if (m_ptr->ml) l_ptr->flags3 |= (RF3_DEMON);
          }
          else return FALSE; /* doesn't affect other monsters */
	 
	     /* Get the monster name */
		 monster_desc(m_name, sizeof(m_name), m_ptr, 0);
              
         /* Prepare death note */
	     note_dies = " shrivels away in the light!";
	     if (ldam <= m_ptr->hp) msg_format("%^s cringes from the light!", m_name);

         /* true if monster dies */
         /* (does not use GF_LITE or GF_LITE_WEAK because */
         /* we don't want the spaces permanently lit) */
         fear = FALSE;
         if (mon_take_hit(m_idx, ldam, &fear, note_dies)) return TRUE;
     }
     
     return FALSE;
}

/* effect of sphere of charm */
bool charm_sphere(const monster_type *m_ptr)
{
    int ridx, eplev;
	bool ischarmed = TRUE;
    int charmres = 0;

	/* get the monster race */
    monster_race *r_ptr;
    r_ptr = &r_info[m_ptr->r_idx];
	ridx = m_ptr->r_idx;

    /* light fairies (not easily charmed) */
    if (strchr("y", r_ptr->d_char)) charmres = 7;
    /* unicorns (not easily charmed but charmable) */
    if ((strchr("Y", r_ptr->d_char)) && (r_ptr->flags3 & (RF3_HURT_DARK))) charmres = 6;
    /* other creatures of light */
    else if (r_ptr->flags3 & (RF3_HURT_DARK)) charmres = 4;
    /* (most) animals and bugs can be charmed */
    else if (r_ptr->flags3 & (RF3_ANIMAL)) charmres = 1;
    else if (r_ptr->flags3 & (RF3_BUG)) charmres = 3;
    /* young dragons can also be charmed (but not easily) */
    else if ((strchr("d", r_ptr->d_char)) && (r_ptr->level < 34)) charmres = 4;
    /* nothing else is charmable */
    else return FALSE;

    /*** charm resistance (multiplied by (r_ptr->level/2)) ***/
    
    /* uniques (can be charmed but much harder) */
    if (r_ptr->flags1 & (RF1_UNIQUE)) charmres += 4;

    /* black cats, werebeats, etc.. (most of these also have EVIL flag) */
    if (r_ptr->flags3 & (RF3_HURT_SILV)) charmres += 3;

    /* evil (young dragons already have strong resistance) */
    if ((m_ptr->evil) && (r_ptr->flags3 & (RF3_DRAGON))) charmres += 1;
    else if (m_ptr->evil) charmres += 7;

    /* bats */
    if ((strchr("b", r_ptr->d_char)) && (charmres < 4)) charmres = 5;
    /* flying monsters are generally harder to charm */
    else if ((r_ptr->flags2 & (RF2_FLY)) && (charmres < 4)) charmres += 2;

    /* other flags which make a monster hard to charm */
    if (r_ptr->flags2 & (RF2_SMART)) charmres += 3;
    if (r_ptr->flags3 & (RF3_RES_NETH)) charmres += 1;

    /* HURT_LITE impossible to charm: */
    /* driders, araneas, spider uniques, and gnawing bug 9*/
    if (r_ptr->flags3 & (RF3_HURT_LITE)) charmres += 20;
    
    /* other exceptions which should be harder to charm */
    if (ridx == 645) charmres += 3; /* Humbaba */
    if (ridx == 725) charmres += 4; /* Jabberwock */
    /* grim & xan (xan is impossible to charm) */
    if ((ridx == 220) || (ridx == 338)) charmres += 9;
    /* basilisk & related */
    if ((ridx == 186) || (ridx == 340) || (ridx == 415) || (ridx == 644)) charmres += 3;
    /* doombat & hellhounds */
    if ((ridx == 436) || (ridx == 509) || (ridx == 767)) charmres += 2;

    /* exceptions which should be charmable */
    if (ridx == 626) charmres = 2; /* The Cerberus  */
    if (ridx == 761) charmres = 2; /* The Tarrasque */
    /* white unicorn (extremely hard to charm but charmable) */
    if (ridx == 605) charmres = 5;
    /* Beruthiel's cats (very hard to charm but charmable) */
    if ((ridx >= 781) && (ridx <= 789)) charmres = 4;

    /* max eplev is 140 (only with max plev, max charisma, and max goodluck) */
    eplev = p_ptr->lev + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + goodluck;
    if (p_ptr->lev < 5) eplev = 5 + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + goodluck;
    /* normal animals don't resist */
    if (charmres < 2) eplev = eplev * 2;

    /* bad luck */
    if (badluck > 12) charmres += (badluck - 10) / 3;
    
    /* can you charm this monster? */
    charmres = charmres * (r_ptr->level/2);
    if (charmres > eplev) ischarmed = FALSE;
    
    return ischarmed;
}

/*
 * Have a monster look to see if it has any sleeping friends nearby
 * and wake them up.
 *
 * First called with dowake = false to check to see if the monster
 * has any sleeping friends nearby.  If so, it is called again with 
 * dowake = true to wake up the friends.
 */
static bool wakeup_friends(int m_idx, bool dowake)
{
	int i, y, x, oy, ox, dist, iloud;
	bool wokeup = FALSE;
	bool leader = FALSE;
	char m_name[80];
	char om_name[80];
	int loud = 75 + badluck + randint(50) - (p_ptr->skills[SKILL_STL]*2);
	
	/* get the monster who is looking for friends */
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* town monsters don't wake up their friends */
	if (r_ptr->level < 1) return FALSE;

	/* location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Monster won't call for help if it can't see the PC */
	if (!projectable(y, x, p_ptr->py, p_ptr->px)) return FALSE;

	/* monsters leads a group of escorts */
	if ((r_ptr->flags1 & (RF1_ESCORT)) || (r_ptr->flags2 & (RF2_ESCORT1)))
		leader = TRUE;

	if (dowake)
	{
		/* get monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* message appropriate to type */
		if (strchr("hiknoptuxyKOPTUVY@&", r_ptr->d_char))
		{
		    if (leader) msg_format("%^s yells for its escorts to wake up!", m_name);
		    else msg_format("%^s yells for its friends to wake up!", m_name);
		}
		else if (strchr("CZ", r_ptr->d_char))
		{
			msg_format("%^s barks at its pack to wake up!", m_name);
		}
		else 
		{
		    if (leader) msg_format("%^s wakes up its escorts!", m_name);
		    else msg_format("%^s wakes up its friends!", m_name);
		}
	}

	/* Look for sleeping buddies */
	for (i = 1; i < mon_max; i++)
	{
		/* get the other monsters */
		monster_type *om_ptr = &mon_list[i];
		monster_race *or_ptr = &r_info[om_ptr->r_idx];

		/* Paranoia -- skip "dead" monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* location */
		oy = om_ptr->fy;
		ox = om_ptr->fx;

		/* only nearby friends */
		dist = distance(y, x, oy, ox);
		if (dist >= MAX_RANGE) continue;

		/* friends further away less likely to wake up */
		iloud = loud - (dist*2);

		/* Monster won't call for help if it can't see its friends */
		if (!projectable(y, x, oy, ox)) continue;
		
		/* a friend means a monster of the same race */
		/* a leader wakes up its escorts (doesn't require same race) */
		if ((m_ptr->r_idx != om_ptr->r_idx) && (!dowake) && (!leader)) continue;

		/* ..but wake up all monsters of the same type */
		if (r_ptr->d_char != or_ptr->d_char) continue;

		/* sleeping or roaming monsters only */
		if (!om_ptr->csleep) continue;
		
		/* if it gets this far, then the monster sees a sleeping (or roaming) */
		/* friend, and he's done looking. */
		if (!dowake) return TRUE;

		/* now the monster actually yells for his buddies to wake up */
		if ((om_ptr->csleep <= iloud) && (or_ptr->sleep < 255))
		{
			/* wake up the friend */
			om_ptr->csleep = 0;
			om_ptr->roaming = 0;
			wokeup = TRUE;

			if (om_ptr->ml)
			{
				/* get the monster's buddy's name */
				monster_desc(om_name, sizeof(om_name), om_ptr, 0);

				/* message */
				msg_format("%^s wakes up.", om_name);
			}
		}
		else
		{
			/* some of it's buddies aren't waking up easily */
			om_ptr->csleep -= ((iloud+1)/2 + randint(iloud/2));
		}
	}

	/* return if any buddies woke up (or if there are any to wake up) */
	return wokeup;
}

/*
 * monster is revolted by your smell
 */
static bool do_repugnance(int m_idx)
{
	bool fearless;
	int flee = damroll(3, 10) + 1;
	int erlev;
	char m_name[80];

	/* get the monster */
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	/* effective monster level */
	erlev = r_ptr->level + 1;

	/* sometimes stronger */
	if (randint((flee*2) + goodluck) > 50) flee += 5 + randint(15);

	fearless = FALSE;
	if ((r_ptr->flags3 & (RF3_UNDEAD)) ||
		(r_ptr->flags3 & (RF3_NON_LIVING))) fearless = TRUE;

	if (r_ptr->flags1 & (RF1_UNIQUE)) erlev += 20;

	if ((r_ptr->flags3 & (RF3_SILVER)) || (r_ptr->flags3 & (RF3_DEMON)))
		erlev += 5;
	else if (r_ptr->flags3 & (RF3_BUG)) erlev += 10;
	else if ((r_ptr->flags3 & (RF3_ORC)) || (r_ptr->flags3 & (RF3_TROLL)) ||
		(r_ptr->flags3 & (RF3_GIANT)) || (r_ptr->flags3 & (RF3_HURT_SILV)))
		erlev += 1;
	else erlev -= 5;

	/* Attempt a saving throw */
	if ((fearless) || (erlev > randint(11 + flee) + 10))
	{
		flee = 0;
	}

	if (flee)
	{
		/* get the monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* message */
		if (!m_ptr->monfear) msg_format("%^s turns to run away from your smell.", m_name);

		/* monster runs away */
		if (m_ptr->monfear + flee > 200) m_ptr->monfear = 200;
		else m_ptr->monfear += flee;

		return TRUE;
	}

	return FALSE;
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
 * the same thing happens to normal monsters when they hit a door.
 *
 * Technically, need to check for monster in the way combined
 * with that monster being in a wall (or door?) XXX
 */
static void process_monster(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	
	int ridx = m_ptr->r_idx;
    int malertrad, noticerange;
	bool allow_mult, pause, gonext, ifsmart, fishdie, nowake = FALSE;
	char m_name[80], n_name[80];

	int i, d, oy, ox, ny, nx;

	int mm[5];

	bool stagger;
	bool openpit;

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
	bool did_pass_door;
	bool did_kill_wall;
	bool fail_climb = FALSE;
	bool invisible = FALSE;

	/* ordinary trees don't do anything (no 'wakes up' messages) */
	if (r_ptr->flags7 & (RF7_NONMONSTER)) return;

	/* Get the origin */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* temporarily invisibility */
	if (m_ptr->tinvis) m_ptr->tinvis -= 1;

	/* Handle roaming or sleeping monsters (hasn't noticed the player yet) */
	if (m_ptr->csleep)
	{
		u32b notice;
		/* threshhold so that certain monsters never wake unless attacked */
		if (r_ptr->sleep == 255) nowake = TRUE;

		/* Aggravation */
		/* note: aggravation can now be nullifed by very high stealth */
		/* see calc_bonuses() */
		if (p_ptr->aggravate)
		{
			/* Notice the "waking up" */
			if (m_ptr->ml)
			{
				/* Get the monster name */
				char m_name[80];
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				if ((m_ptr->roaming) && (!nowake)) msg_format("%^s notices you.", m_name);
				else if (!m_ptr->roaming) msg_format("%^s wakes up.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}

			/* non-aggressive monsters just start roaming */
			if (nowake)
			{
				if (!m_ptr->roaming) m_ptr->roaming = 1;
			}
			else
			{
				/* notice the player */
				m_ptr->csleep = 0;
				m_ptr->roaming = 0;
			}

			/* Efficiency XXX XXX */
			/* monsters who were already awake have time to act */
			/* on the same turn that they notice you */
			if (!m_ptr->roaming) return;

			/* stop roaming if it's noticed you */
			if ((!m_ptr->csleep) && ((!nowake) || (player_has_los_bold(oy, ox))))
				m_ptr->roaming = 0;
		}
		
		/* reduced detection radius for special types of aggravation */
        if (r_ptr->aaf > 2) malertrad = (r_ptr->aaf * 2) / 3;
        else malertrad = r_ptr->aaf;

		/* Liches aggravate animals and creatures of light */
		if ((p_ptr->timed[TMD_BECOME_LICH]) &&
		   (m_ptr->cdis <= malertrad) &&
		   ((r_ptr->flags3 & (RF3_HURT_DARK)) ||
		   (r_ptr->flags3 & (RF3_ANIMAL))))
		{
			/* Reset sleep counter */
			m_ptr->csleep = 0;

			/* Notice the "waking up" */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				if (m_ptr->roaming) msg_format("%^s notices you.", m_name);
				else msg_format("%^s wakes up.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}

			/* Efficiency XXX XXX */
			/* monsters who were already awake have time to act */
			/* on the same turn that they notice you */
			if (!m_ptr->roaming) return;

			/* stop roaming since it's noticed you */
			m_ptr->roaming = 0;
		}
		
		/* Black magic aggravates demons */
		if ((p_ptr->timed[TMD_WITCH]) &&
		   (m_ptr->cdis <= malertrad) && 
		   (r_ptr->flags3 & (RF3_DEMON)))
		{
			/* Reset sleep counter */
			m_ptr->csleep = 0;

			/* Notice the "waking up" */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				if (m_ptr->roaming) msg_format("%^s notices you.", m_name);
				else msg_format("%^s wakes up.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}

			/* Efficiency XXX XXX */
			/* monsters who were already awake have time to act */
			/* on the same turn that they notice you */
			if (!m_ptr->roaming) return;

			/* stop roaming since it's noticed you */
			m_ptr->roaming = 0;
		}

		/* Anti-stealth (originally: notice = rand_int(1024);  ) */
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx)) noticerange = 900 + (m_ptr->cdis*2);
		else if (projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px)) noticerange = 1000;
		else noticerange = 1200;

		/* monsters notice you easier when you stink */
		if ((p_ptr->timed[TMD_STINKY]) && (m_ptr->cdis < 5))
			noticerange = noticerange/2;

		/* awake in a dark space within the PC's light radius (very likely to notice the PC) */
		if ((!(cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_GLOW))) && (m_ptr->roaming) &&
			(m_ptr->cdis < p_ptr->cur_lite))
		{
			if (p_ptr->cur_lite < 4) noticerange = noticerange/2 - 40;
			else if (m_ptr->cdis < 3) noticerange -= noticerange/3;
		}

		notice = rand_int(noticerange);

		/* Hack -- See if monster "notices" player */
		if (((notice * notice * notice) <= p_ptr->noise) && (!nowake))
		{
			int d = 1;

			/* Wake up faster near the player */
			if (m_ptr->cdis < 50) d = (100 / m_ptr->cdis);

			/* monsters notice you easier when you stink */
			if ((p_ptr->timed[TMD_STINKY]) && (m_ptr->cdis < 5)) d += 2;

			/* niceness */
			if ((p_ptr->nice) &&
			   (!m_ptr->evil) &&
		       ((r_ptr->flags3 & (RF3_HURT_DARK)) ||
		       (r_ptr->flags3 & (RF3_ANIMAL))))
            {
               if (d > 2) d = (d - 1) / 2;
               /* takes no notice of PC */
               else if (!m_ptr->roaming) /* d == 1 or 2 */ return;
            }

			/* roaming monsters notice the player easier */
			if ((m_ptr->roaming) && (m_ptr->cdis < 60) &&
				(player_has_los_bold(m_ptr->fy, m_ptr->fx)))
				d = (150 / m_ptr->cdis);
			else if ((m_ptr->roaming) && (m_ptr->cdis < 31) &&
				(!player_has_los_bold(m_ptr->fy, m_ptr->fx)))
				d += (50 - m_ptr->cdis) / 15;

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

            /* monster might feel nice and pay no attention to you */
			else if ((p_ptr->nice) && 
			   (!m_ptr->evil) &&
               (goodluck > 0) && (randint(100) < 33) &&
		       ((r_ptr->flags3 & (RF3_HURT_DARK)) ||
		       (r_ptr->flags3 & (RF3_ANIMAL))))
            {
                  m_ptr->csleep += (goodluck * 20) + 20;
            }

			/* Just noticed you */
			else
			{
				/* Reset sleep counter */
				m_ptr->csleep = 0;

				/* Notice being noticed */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Dump a message */
				    if (m_ptr->roaming) msg_format("%^s notices you.", m_name);
				    else msg_format("%^s wakes up.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

					/* Hack -- Count the wakings */
					if (l_ptr->wake < MAX_UCHAR)
					{
						l_ptr->wake++;
					}
				}
			}
		}
	
	    /* asleep but displayed as awake in monster list */
	    if ((m_ptr->csleep) && (!m_ptr->display_sleep))
	    {
		   /* update monster list */
		   p_ptr->window |= PW_MONLIST;
		   window_stuff();
        }

		/* Still sleeping */
		if ((m_ptr->csleep) && (!m_ptr->roaming))
		{
			return;
		}

		/* monster stops roaming if it's noticed you */
		if (!m_ptr->csleep) m_ptr->roaming = 0;

		/* Disturb when roaming monster notices you */
		if ((!m_ptr->csleep) && (disturb_near) &&
			(m_ptr->ml)) disturb(1, 0);
	}

    /* awake but displayed as asleep in monster list */
	if ((!m_ptr->csleep) && (m_ptr->display_sleep))
	{
	   /* update monster list */
	   p_ptr->window |= PW_MONLIST;
	   window_stuff();
    }

	/* Handle "stun" */
	if (m_ptr->stunned)
	{
		int d = 1;

		/* Make a "saving throw" against stun */
		if (rand_int(5000) <= r_ptr->level * r_ptr->level)
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

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer stunned.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}
		}
		/* Still stunned (changed effect: */
		/* shouldn't completely keep a monster from acting) */
		/* if (m_ptr->stunned) return; */
	}

	/* Handle timed silence summons */
	if (m_ptr->silence)
	{
        int d, msave;

        /* monster saving throw to shake off the enchantment (lower is stronger) */
        if ((r_ptr->flags2 & (RF2_SMART)) && (r_ptr->flags1 & (RF1_UNIQUE))) msave = 7;
        else if (r_ptr->flags2 & (RF2_SMART)) msave = 10;
        else if (r_ptr->flags1 & (RF1_UNIQUE)) msave = 12;
        else msave = 15;

        if ((r_ptr->level / msave) > 8) d = randint(5) + 1;
        else if ((r_ptr->level / msave) > 6) d = randint(6);
        else if ((r_ptr->level / msave) < 2) d = 1;
        else d = randint(r_ptr->level / msave);

		/* Still silenced */
		if (m_ptr->silence > d)
		{
			/* Reduce the duration */
			m_ptr->silence -= d;
		}

		/* Recovered */
		else
		{
			/* No longer silenced */
			m_ptr->silence = 0;

			/* Message if visible */
			if (m_ptr->ml)
			{
				/* Get the monster name */
				char m_name[80];
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer silenced.", m_name);
				
				/* possible disturb */
				if (disturb_minor) disturb(1, 0);				
			}
		}
    }

	/* Handle paladin truce spell */
	if (m_ptr->truce)
	{
		int d = randint(r_ptr->level / 15 + 1);

		/* Still peaceful */
		if (m_ptr->truce > d)
		{
			/* Reduce the duration */
			m_ptr->truce -= d;
		}

		/* Recovered */
		else
		{
			/* No longer peaceful */
			m_ptr->truce = 0;

			/* Message if visible */
			if (m_ptr->ml)
			{
				/* Get the monster name */
				char m_name[80];
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer bound by your truce.", m_name);
				
				/* possible disturb */
				if (disturb_minor) disturb(1, 0);				
			}
		}
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

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer confused.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}


	/* Handle "fear" */
	if (m_ptr->monfear)
	{
		/* Amount of "boldness" */
		int d = randint(r_ptr->level / 11 + 1);

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

			/* Visual note */
			if (m_ptr->ml)
			{
				char m_name[80];
				char m_poss[80];

				/* Get the monster name/poss */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);
				monster_desc(m_poss, sizeof(m_poss), m_ptr, 0x22);

				/* Dump a message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}


	/* monster is currently in an open pit */
	if (cave_feat[oy][ox] == FEAT_OPEN_PIT) openpit = TRUE;
	else openpit = FALSE;
	
	allow_mult = FALSE;
	if (r_ptr->flags2 & (RF2_MULTIPLY)) allow_mult = TRUE;

	/* multiply much less often when it hasn't noticed the player */
	/* (just because that would be mean to have a room fill up with */
	/* giant gnats when the player hasn't even seen them yet..) */
	if ((m_ptr->roaming) && (randint(100) < 96)) allow_mult = FALSE;

	/* Attempt to "mutiply" if able and allowed */
	if ((allow_mult) && (num_repro < MAX_REPRO))
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
			if (multiply_monster(m_idx))
			{
				/* Take note if visible */
				if (m_ptr->ml)
				{
					l_ptr->flags2 |= (RF2_MULTIPLY);

					/* Make a sound */
					sound(MSG_MULTIPLY);
				}

				/* Multiplying takes energy */
				return;
			}
		}
	}

    /* sphere of charm */
	m_ptr->charmed = FALSE;
	
    if ((p_ptr->timed[TMD_SPHERE_CHARM]) && (m_ptr->cdis < 5))
    {
        m_ptr->charmed = charm_sphere(m_ptr);
       
       /* charmed and nice monsters might go back to sleep */
       /* only time p_ptr->nice can apply to anything other than animals and creatures of light */
       if ((p_ptr->nice) && (m_ptr->charmed) && (!m_ptr->evil) &&
          (goodluck > 0) && (randint(100) < 16))
	   {
	      bool startroam;
		  /* Get the monster name */
	      /* char m_name[80]; */
	      monster_desc(m_name, sizeof(m_name), m_ptr, 0);
	   
	      if (r_ptr->sleep * 2 < 50) m_ptr->csleep += (goodluck * 25) + 50;
	      else m_ptr->csleep += (goodluck * 25) + (r_ptr->sleep * 2);
          startroam = FALSE;

          if ((r_ptr->flags2 & (RF2_ROAM2)) && (randint(100) < 75)) startroam = TRUE;
          else if ((r_ptr->flags2 & (RF2_ROAM1)) && (randint(100) < 20)) startroam = TRUE;
          
          if (startroam)
          {
             msg_format("%^s starts ignoring you.", m_name);
             m_ptr->roaming = 1;
          }
          else 
          {
             msg_format("%^s goes back to sleep.", m_name);
			 return;
          }
       }
    }
    
    /* roaming but no csleep means only temporarily roaming because of running */
    /* into a wall, so stop roaming if player comes into line of sight */
    /* if ((m_ptr->roaming > 20) && (player_has_los_bold(m_ptr->fy, m_ptr->fx))) */
	if ((m_ptr->roaming) && (!m_ptr->csleep) && (m_ptr->cdis <= MAX_RANGE) && 
		(projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px)))
    {
        m_ptr->roaming = 0;
    }

	/* Attempt to cast a spell */
	if (make_attack_spell(m_idx)) return;

	/* sometimes have monster look for sleeping friends to wake up */
	/* (wakeup_friends FALSE checks for sleeping friends) */
	if ((m_ptr->cdis < 6) && (!m_ptr->csleep) && (wakeup_friends(m_idx, FALSE)) &&
		!((r_ptr->flags2 & (RF2_STUPID)) || (r_ptr->flags2 & (RF2_EMPTY_MIND))))
	{
		/* (less likely to wake up friends if it doesn't come in groups) */
		int wakebud = 6;
		if ((r_ptr->flags2 & (RF2_FRIEND1)) || (r_ptr->flags1 & (RF1_FRIENDS)))
			wakebud = 16;
		/* leaders wake up their escorts */
		if ((r_ptr->flags1 & (RF1_ESCORT)) || (r_ptr->flags2 & (RF2_ESCORT1)))
			wakebud = 12;
		if (m_ptr->stunned) wakebud -= 6;
		if (m_ptr->confused) wakebud -= 4;

		if (randint(100) < wakebud)
		{
			/* (wakeup_friends TRUE to actually wake up the sleeping friends) */
			if (wakeup_friends(m_idx, TRUE))
			{
				/* sometimes this takes up their whole turn */
				/* (but never if nobody actually woke up) */
				if (randint(100) < 16 + goodluck) return;
			}
		}
	}

	/* Reset */
	stagger = FALSE;

	/* Confused */
	if (m_ptr->confused)
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
				if (m_ptr->ml) l_ptr->flags1 |= (RF1_RAND_25);

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
				if (m_ptr->ml) l_ptr->flags1 |= (RF1_RAND_50);

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
				if (m_ptr->ml) l_ptr->flags1 |= (RF1_RAND_50 | RF1_RAND_25);

				/* Stagger */
				stagger = TRUE;
			}
		}
	}

	/* monsters roaming in groups stagger more often */
	if ((m_ptr->roaming > 10) && (m_ptr->roaming < 20))
    {
        if (rand_int(100) < 18) stagger = TRUE;
    }
	/* stagger sometimes when roaming (or has truce) */
	else if (((m_ptr->roaming) || (m_ptr->truce)) && 
		(rand_int(100) < 9)) stagger = TRUE;
	
	/* make monster stagger sometimes if charmed or HELPER */
	/* so it doesn't just keep bumping into the player */
	if (((m_ptr->charmed) || (r_ptr->flags3 & (RF3_HELPER)) || (m_ptr->stunned))
         && (rand_int(100) < 21)) stagger = TRUE;

    /* nice monsters stagger sometimes */
    else if ((p_ptr->nice) && (!m_ptr->evil) &&
       (goodluck > 0) && (randint(100) < 16) && 
	   ((r_ptr->flags3 & (RF3_HURT_DARK)) ||
	   (r_ptr->flags3 & (RF3_ANIMAL))))
     {
        stagger = TRUE;
     }

	/* monster never staggers if it is holding the player (mainly for Grip) */
    if ((p_ptr->timed[TMD_BEAR_HOLD]) && (p_ptr->held_m_idx == m_idx))
		stagger = FALSE;

	/* Normal movement */
	if (!stagger)
	{
		/* Logical moves, may do nothing */
		if (!get_moves(m_idx, mm))
		{
			return;
		}
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
	did_pass_door = FALSE;
	did_kill_wall = FALSE;

	/* can't rely on m_ptr->ml for this */
	if ((r_ptr->flags2 & (RF2_INVISIBLE)) || (m_ptr->tinvis)) invisible = TRUE;

	/* does the monster pause to shoot or cast? */
    pause = FALSE;

	/* if it's a WATER_ONLY monster, it may suffocate if out of water */
	if (rand_int(100) < 17) fishdie = TRUE;
	else fishdie = FALSE;

	/* Process moves */
	for (i = 0; i < 5; i++)
	{
		/* Get the direction (or stagger) */
		d = (stagger ? ddd[rand_int(8)] : mm[i]);

		/* Get the destination */
		ny = oy + ddy[d];
		nx = ox + ddx[d];

		/* WATER_ONLY monsters can't stand being out of water */
		if ((r_ptr->flags7 & (RF7_WATER_ONLY)) && (fishdie) &&
			(!(cave_feat[oy][ox] == FEAT_WATER)) &&
			(!(cave_feat[ny][nx] == FEAT_WATER)))
		{
			if (m_ptr->hp > 4) m_ptr->hp -= (1 + randint((m_ptr->hp / 2) + 1));
			if (m_ptr->hp <= 4) m_ptr->hp -= randint(2);
			if (m_ptr->hp < 0)
			{
				if (m_ptr->ml) msg_format("%^s dies from being out of water!", m_name);
				delete_monster_idx(m_idx, FALSE);
				return;
			}
			else if (m_ptr->ml)
			{
				msg_format("%^s is having trouble breathing out of water.", m_name);
			}
			/* don't hurt the monster more than once */
			fishdie = FALSE;
		}


		/* Handle water first */
		/* fire-based monsters will not go in water, or take damage if they do */
		if ((cave_feat[ny][nx] == FEAT_WATER) && 
				(r_ptr->flags7 & (RF7_HATE_WATER)))
		{
			if ((i < 4) || (randint(100) < 75) || (m_ptr->hp < 10))
			{
				do_move = FALSE;
			}
		}

		/* WATER_HIDE monsters like water and hesitate to leave it */
		else if ((cave_feat[oy][ox] == FEAT_WATER) &&
				(!(cave_feat[ny][nx] == FEAT_WATER)) && 
				(r_ptr->flags7 & (RF7_WATER_HIDE)) && (m_ptr->cdis > 2))
		{
			int staynhide;
			/* hiding in the water, so don't want to leave */
			if ((player_can_see_bold(oy, ox)) && (!m_ptr->ml)) staynhide = 95;
			else if (!m_ptr->ml) staynhide = 75;
			else if (r_ptr->flags2 & (RF2_RANGE)) staynhide = 80;
			else staynhide = 20;

			if (randint(100) < staynhide)
			{
				do_move = FALSE;
				if (r_ptr->flags2 & (RF2_RANGE)) pause = TRUE;
			}
		}
		/* most monsters ignore water */
		else if (cave_feat[ny][nx] == FEAT_WATER)
		{
		    /* Go ahead and move */
		    do_move = TRUE;
		}
		/* WATER_ONLY monsters never leave the water */
		/* (WATER_ONLY monsters should not also have WATER_HIDE) */
		else if ((cave_feat[oy][ox] == FEAT_WATER) && (r_ptr->flags7 & (RF7_WATER_ONLY)) &&
			(cave_m_idx[ny][nx] >= 0))
		{
			do_move = FALSE;
		}

		/* Floor is open? */
		else if (cave_floor_bold(ny, nx))
		{
		    bool closer, further, maystop = FALSE, nomelee = FALSE;
            /* Go ahead and move (tentative) */
		    do_move = TRUE;
		    /* is the new space closer to the player */
		    if (distance(ny, nx, p_ptr->py, p_ptr->px) < m_ptr->cdis) closer = TRUE;
		    else closer = FALSE;
		    if (distance(ny, nx, p_ptr->py, p_ptr->px) > m_ptr->cdis) further = TRUE;
		    else further = FALSE;

			/* prefer not going into a pit (unless it flies or is a climber) */
			if ((cave_feat[ny][nx] == FEAT_OPEN_PIT) && (!(r_ptr->flags2 & (RF2_FLY))))
			{
				if ((m_ptr->roaming) && (!strchr("SXacr:%&", r_ptr->d_char)))
					do_move = FALSE;
			}
			/* does it move, but have no melee blows? */
			if ((r_ptr->flags1 & (RF1_NEVER_BLOW)) && (!(r_ptr->flags1 & (RF1_NEVER_MOVE))))
			{
				maystop = TRUE;
				nomelee = TRUE;
            }
			/* moving would give away the disguise */
			if (m_ptr->disguised) maystop = TRUE;

			/* Crude AI improvement: If it prefers shooting/casting to melee */
			/* It might decide not to move (pause and shoot/cast later) */
			if (((r_ptr->flags2 & (RF2_RANGE)) || (maystop)) &&
				(!m_ptr->confused) && (!m_ptr->roaming))
	        {
            	int py = p_ptr->py;
              	int px = p_ptr->px;
                int stopodd = 20;
                if (m_ptr->disguised) stopodd = 80;
			    if (clean_shot(oy, ox, py, px, FALSE)) stopodd += 19;
			    /* monsters who don't melee have no interest in coming closer to the PC */
			    if ((m_ptr->cdis < 3) && (nomelee) && (closer)) stopodd += 55;
			    else if ((m_ptr->cdis < 3) && (nomelee) && (further)) stopodd -= 5;
			    else if (nomelee) stopodd += 25;
			    /* don't want to get close to PC if you prefer range attacks */
			    else if ((m_ptr->cdis < 3) && (closer)) stopodd += 21;
			    else if ((m_ptr->cdis < 4) && (closer)) stopodd += 9;
			    else if ((m_ptr->cdis < 3) && (!further)) stopodd += 9;
			    /* don't like to shoot from inside a pit (hurts to-hit) */
				if (openpit) stopodd = stopodd/2;
			    if ((randint(100) < stopodd) && (projectable(oy, ox, py, px)))
                {
                   do_move = FALSE;
                   pause = TRUE;
                  
                   /* He has given up his move so he can have another chance to shoot */
                   do_turn = TRUE;
                }
            }
		}

		/* Permanent wall */
		else if (cave_feat[ny][nx] >= FEAT_PERM_EXTRA)
		{
            if ((m_ptr->roaming) && (m_ptr->roaming < 20))
            {
               if ((m_ptr->roaming == 9) || (m_ptr->roaming == 19))
               {
                  /* running into a wall, need a new destination */
               }
               else if (i > 1) m_ptr->roaming += 1;
            }
            /* not roaming, but haven't been able to find a move */
            /* magic roaming number 30: temporary roaming */
            else if ((i == 4) && (randint(100) < 22)) m_ptr->roaming = 30;

            /* maybe stagger next time through the loop */
			if (randint(100) < 9) stagger = TRUE;
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
			/* Eat through walls/doors/rubble */
			do_move = TRUE;

			/* Monster destroyed a wall */
			did_kill_wall = TRUE;

			/* Forget the wall */
			cave_info[ny][nx] &= ~(CAVE_MARK);

			/* Notice */
			cave_set_feat(ny, nx, FEAT_FLOOR);

			/* Note changes to viewable region */
			if (player_has_los_bold(ny, nx)) do_view = TRUE;
		}
		
		/* bump into the wall */
		else if (cave_feat[ny][nx] >= FEAT_MAGMA)
		{
            if ((m_ptr->roaming) && (m_ptr->roaming < 20))
            {
               if ((m_ptr->roaming == 9) || (m_ptr->roaming == 19))
               {
                  /* running into a wall, need a new destination */
               }
               else if (i > 1) m_ptr->roaming += 1;
            }
            /* not roarming, but haven't been able to find a move */
            /* magic roaming number 30 */
            else if ((i == 4) && (randint(100) < 22)) m_ptr->roaming = 30;

            /* maybe stagger next time through the loop */
			if (randint(100) < 10) stagger = TRUE;
		}

		/* Handle PASS_DOOR monsters */
		else if ((((cave_feat[ny][nx] >= FEAT_DOOR_HEAD) &&
		          (cave_feat[ny][nx] <= FEAT_DOOR_TAIL)) ||
		         /* (cave_feat[ny][nx] == FEAT_SECRET) || */
				  (cave_feat[ny][nx] == FEAT_RUBBLE)) &&
  				  (r_ptr->flags2 & (RF2_PASS_DOOR)))

		{
			/* Pass through doors */
			do_move = TRUE;

			if ((player_can_see_bold(ny, nx)) && (!invisible))
			{
				/* Get the monster name (after marking as visible early) */
				m_ptr->ml = TRUE;
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				l_ptr->flags2 |= (RF2_PASS_DOOR);
			}
		}

		/* Handle doors and secret doors */
		else if (((cave_feat[ny][nx] >= FEAT_DOOR_HEAD) &&
		          (cave_feat[ny][nx] <= FEAT_DOOR_TAIL)) ||
		         (cave_feat[ny][nx] == FEAT_SECRET))
		{
			bool may_bash = TRUE;

			/* roaming monsters don't bash down doors */
            if ((m_ptr->roaming) && (m_ptr->roaming < 20)) may_bash = FALSE;
			if (m_ptr->truce) may_bash = FALSE;

			/* Take a turn */
			if ((r_ptr->flags2 & (RF2_OPEN_DOOR)) || (r_ptr->flags2 & (RF2_BASH_DOOR)))
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
				}

				/* Locked doors (not jammed) */
				else if (cave_feat[ny][nx] < FEAT_DOOR_HEAD + 0x08)
				{
					int k;

					/* Door power */
					k = ((cave_feat[ny][nx] - FEAT_DOOR_HEAD) & 0x07);
					if ((m_ptr->confused) || (m_ptr->stunned)) k += 2;

					/* roaming monsters don't bash doors so they should */
					/* be able to open them easier so they don't just */
					/* keep running into walls */
					if (m_ptr->roaming) k -= 5;

					/* Try to unlock it XXX XXX XXX */
					if (rand_int(m_ptr->hp / 10) > k)
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
				int k, bashhp;

				/* Door power */
				k = ((cave_feat[ny][nx] - FEAT_DOOR_HEAD) & 0x07);
				/* inflate so stuck doors can actually block monsters sometimes */
				k = (k + 1) * 3;
				/* make up for having less levels of door power */
				if (cave_feat[ny][nx] > FEAT_DOOR_TAIL - 2)
					k += 1 + randint(2 + (goodluck+1)/2);
				
				/* monster's bashing power */
				bashhp = (m_ptr->hp+4) / 15;
				if (bashhp > 240) bashhp = 240;
				/* some types bash worse than others */
				if ((strchr("BCFIJSZbefkpqrst", r_ptr->d_char)) && (bashhp > 10))
                   bashhp = (bashhp * 4) / 5;
                /* lower max for most humans & humanoids and some others */
				if ((strchr("FIkptr@", r_ptr->d_char)) && (bashhp > 60)) bashhp = 60;
				if ((strchr("BKbho", r_ptr->d_char)) && (bashhp > 120)) bashhp = 120;
				/* some types bash better than others */
				if ((strchr("DENOPTUXYdg%&", r_ptr->d_char)) && (bashhp <= 200))
                   bashhp = (bashhp * 6) / 5;

				/* if a monster has BASH_DOOR then they should always have some chance */
				if (bashhp < k + 2)
				{
					int bchance = 50 - goodluck + (badluck*2);
					int dlevel = 52 + (p_ptr->depth/2);
					int obash = bashhp + 1;
					bashhp = k;
					if (r_ptr->level < dlevel) bchance -= (dlevel - r_ptr->level) / 3;
					if (rand_int(100) < bchance/2) bashhp += 1 + randint(obash);
					else if (rand_int(100) < bchance) bashhp += 2;
					if (badluck > 5) bashhp += (badluck/5);
				}
				/* confused / stunned penalty */
				if ((m_ptr->confused) && (m_ptr->stunned)) bashhp = bashhp/3;
				else if ((m_ptr->confused) || (m_ptr->stunned)) bashhp = (bashhp*2)/3;
				if ((badluck > 8) && (bashhp <= k+1) && (r_ptr->level > 5)) bashhp = k+2;

				/* Attempt to Bash */
				if (rand_int(bashhp) > k)
				{
					/* Message */
					msg_print("You hear a door burst open!");

					/* Disturb (sometimes) */
					if (disturb_minor) disturb(0, 0);

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
					cave_set_feat(ny, nx, FEAT_BROKEN);
				}

				/* Open the door */
				else
				{
					cave_set_feat(ny, nx, FEAT_OPEN);
				}

				/* Handle viewable doors */
				if (player_has_los_bold(ny, nx)) do_view = TRUE;
			}
		}
		
		/* if PCplayer is standing on rubble, don't try to climb it, just attack */
		else if ((cave_feat[ny][nx] == FEAT_RUBBLE) && (cave_m_idx[ny][nx] < 0))
		{
			do_move = TRUE;
		}
		/* [DJA: added FLY flag: can fly over rubble] */
		/* monsters that are too big to fly between the rubble and the ceiling */
		/* don't get the FLY flag even if they can fly (like ancient dragons) */
		else if ((cave_feat[ny][nx] == FEAT_RUBBLE) && (r_ptr->flags2 & (RF2_FLY)))
		{
			do_move = TRUE;

			if ((player_can_see_bold(ny, nx)) && (!invisible))
			{
				/* Get the monster name (after marking as visible early) */
				/* DJAXXX doesn't take monster stealth into account */
				m_ptr->ml = TRUE;
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				msg_format("%^s flies over the rubble.", m_name);
				l_ptr->flags2 |= (RF2_FLY);
			}
        }
		/* monsters can move or climb over rubble if they have BASH_DOOR */
		/* but not easily  */
		else if ((cave_feat[ny][nx] == FEAT_RUBBLE) && (r_ptr->flags2 & (RF2_BASH_DOOR)))
		{
           	int bashstr, perc, die;
           	bool aclimb = FALSE;
			object_type *o_ptr;
	        cptr dname = (r_name + r_ptr->name);
           	/* hack for dwarves' digging */
  			if (strstr(dname, "dwarf")) bashstr = 60;
           	/* some monsters are better at digging/bashing than others */
			if (strchr("XP:", r_ptr->d_char)) bashstr = 65;
            else if (strchr("DETg%&", r_ptr->d_char)) bashstr = 50;
            else if (strchr("ZOMdnox", r_ptr->d_char)) bashstr = 40;
            else bashstr = 35;

			/* hard to climb over rubble from inside of a pit (doesn't affect climbers) */
			if (openpit) bashstr -= 10;
            
            /* climbers */
            if (strchr("JScra", r_ptr->d_char))
            {
               aclimb = TRUE; /* always climb, never remove rubble */
               if (strchr("Sca", r_ptr->d_char)) bashstr = 70;
               else if (strchr("J", r_ptr->d_char)) bashstr = 50;
               else bashstr = 40;
            }

		    /* how well they dig/climb depends on monster's health and maxhps */
		    /* average healthy L60 bone devil: bashstr = 81 */
		    /* average healthy L15 light hound: bashstr = 48 */
		    /* average healthy L12 black dwarf: bashstr = 72 */
		    perc = 100L * m_ptr->hp / m_ptr->maxhp;
		    if (perc >= 60) bashstr += 9 + r_ptr->level/5;
		    else if (perc >= 25) bashstr += 2 + r_ptr->level/6;
		    else if (perc >= 10) bashstr -= 1;
		    else bashstr -= 10;
		    if (m_ptr->maxhp > 50) bashstr += m_ptr->maxhp/50;
            if ((m_ptr->confused) && (m_ptr->stunned)) bashstr = bashstr/3;
            else if ((m_ptr->confused) || (m_ptr->stunned)) bashstr = (bashstr*2)/3;
           	
            die = randint(920 + p_ptr->depth);
            if (die < bashstr)
            {
                /* dwarves dig rather than climb */
  			    if (strstr(dname, "dwarf")) die -= 25;
				/* even moreso for wall monsters */
				else if (strchr("#", r_ptr->d_char)) die -= 30;
				/* others that are more likely to remove rubble than climb it */
                else if (strchr("DEMOPTXg%", r_ptr->d_char)) die -= 15;

				/* remove rubble */
                if ((die < (bashstr * 2) / 5) && (!aclimb))
                {
                    bool unbury = FALSE;
					cave_set_feat(ny, nx, FEAT_FLOOR);

					/* Scan all objects in the grid */
					for (o_ptr = get_first_object(ny, nx); o_ptr; o_ptr = get_next_object(o_ptr))
					{
						/* objects buried in rubble are no longer buried */
						if (o_ptr->hidden)
						{
							o_ptr->hidden = 0;
							unbury = TRUE;
						}
					}

					if (unbury) p_ptr->window |= PW_OBJLIST;

					/* message */
					if ((player_can_see_bold(ny, nx)) && (!invisible))
					{
						/* Get the monster name (after marking as visible early) */
				        m_ptr->ml = TRUE;
				        monster_desc(m_name, sizeof(m_name), m_ptr, 0);

						msg_format("%^s digs through the rubble.", m_name);
					}
					else msg_print("You hear rocks being moved.");
                }
                /* climb over without removing it */
                else if ((player_can_see_bold(ny, nx)) && (!invisible))
                {
					/* Get the monster name (after marking as visible early) */
			        m_ptr->ml = TRUE;
			        monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				    msg_format("%^s climbs over the rubble.", m_name);
                }

				do_move = TRUE;

				if (player_can_see_bold(ny, nx))
                {
                   do_view = TRUE;

                   /* remember BASH_DOOR flag */
				   if (m_ptr->ml) did_bash_door = TRUE;
                }
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
					msg_print("The rune of protection is broken!");
				}

				/* Forget the rune */
				cave_info[ny][nx] &= ~(CAVE_MARK);

				/* Break the rune */
				cave_set_feat(ny, nx, FEAT_FLOOR);

				/* Allow movement */
				do_move = TRUE;
			}
		}

		/* Some monsters never attack */
		if (do_move && (cave_m_idx[ny][nx] < 0) &&
		    (r_ptr->flags1 & (RF1_NEVER_BLOW)))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) l_ptr->flags1 |= (RF1_NEVER_BLOW);

			/* Do not move */
			do_move = FALSE;
		}
		
		/* stop roaming if about to move into the player */
		if ((do_move) && (m_ptr->roaming) && (cave_m_idx[ny][nx] < 0) && 
			(!m_ptr->charmed) && (r_ptr->sleep < 240))
		{
            /* Get the monster name */
		    char m_name[80];
		    monster_desc(m_name, sizeof(m_name), m_ptr, 0);
		    
			/* how did he not notice the player earlier? */
            if ((m_ptr->roaming) && (m_ptr->csleep))
            {
				msg_format("%^s suddenly notices you.", m_name);
				/* another magic roaming number: monster is caught off guard */
				m_ptr->csleep = 0;
				m_ptr->roaming = 29; /* monster gets penalty to-hit */
            }
            else m_ptr->roaming = 0;
        }
		/* non-agressive monsters don't attack */
		if ((m_ptr->roaming) && (m_ptr->roaming < 29) &&
			(r_ptr->sleep >= 240)) nowake = TRUE;
		else nowake = FALSE;

		/* The player is in the way.  Attack him. */
		if ((do_move && (cave_m_idx[ny][nx] < 0)) && (!nowake) &&
			(!m_ptr->charmed) && (!m_ptr->truce))
		{
			/* can't stay disguised while attacking */
			m_ptr->disguised = 0;
            /* Do the attack */
			(void)make_attack_normal(m_idx);
			
			/* ensure not roaming */
			m_ptr->roaming = 0;

			/* Do not move */
			do_move = FALSE;

			/* Took a turn */
			do_turn = TRUE;
		}

		/* Some monsters never move */
		if (do_move && (r_ptr->flags1 & (RF1_NEVER_MOVE)))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) l_ptr->flags1 |= (RF1_NEVER_MOVE);

			/* Do not move */
			do_move = FALSE;
		}
		
		/* tmp do_move */
        gonext = do_move;

        /* A monster is in the way (eat it?) */
		if (do_move && (cave_m_idx[ny][nx] > 0))
		{
			bool eaten = FALSE;
			monster_type *n_ptr = &mon_list[cave_m_idx[ny][nx]];
			monster_race *nr_ptr = &r_info[n_ptr->r_idx];

			/* Assume no movement */
			do_move = FALSE;

			/* Kill weaker monsters */
			/* (not if confused or stunned, and don't kill trees) */
			if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
               (!m_ptr->stunned) && (!m_ptr->confused) &&
			    (compare_monsters(m_ptr, n_ptr, TRUE) > 0) &&
				(!strchr("E", nr_ptr->d_char)))

			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster ate another monster */
				did_kill_body = TRUE;

				/* Get the hungry monster's name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);
				/* Get the unfortunate monster's name */
				monster_desc(n_name, sizeof(n_name), n_ptr, 0);

				/* message */
				if (strchr("RDMHNvw", r_ptr->d_char)) eaten = TRUE;
                if ((m_ptr->ml) && (eaten)) msg_format("%^s eats %s.", m_name, n_name);
                else if (m_ptr->ml) msg_format("%^s tramples %s.", m_name, n_name);
				else if (n_ptr->ml) msg_format("%^s is trampled.", n_name);

				/* Kill the monster */
				/* if it's eaten it can't return, otherwise it can */
				if (eaten) delete_monster(ny, nx, FALSE);
				else delete_monster(ny, nx, TRUE);
			}
		}

		/* A monster is in the way (move it?) */
		/* (previous if sets do_move to false, so use gonext instead) */
		if (gonext && (cave_m_idx[ny][nx] > 0))
		{
			monster_type *n_ptr = &mon_list[cave_m_idx[ny][nx]];

			/* Assume no movement */
			do_move = FALSE;

			/* Push past weaker monsters (unless leaving a wall) */
			if ((r_ptr->flags2 & (RF2_MOVE_BODY)) &&
			    (compare_monsters(m_ptr, n_ptr, FALSE) > 0) &&
                (!m_ptr->stunned) && (!m_ptr->confused) &&
			    (cave_floor_bold(m_ptr->fy, m_ptr->fx)))
			{
				/* don't push past a tree */
				if (!((r_ptr->flags7 & (RF7_NONMONSTER)) && 
					(r_ptr->flags7 & (RF7_BLOCK_LOS))))
				{
					/* Allow movement */
					do_move = TRUE;

					/* Monster pushed past another monster */
					did_move_body = TRUE;

					/* XXX XXX XXX Message */
				}
			}
		}

        /* A monster is in the way (wake it up?) */
		/* (previous if sets do_move to false, so use gonext instead) */
		if (gonext && (cave_m_idx[ny][nx] > 0))
		{
			int odds = 35;
			monster_type *n_ptr = &mon_list[cave_m_idx[ny][nx]];
			monster_race *nr_ptr = &r_info[n_ptr->r_idx];

            /* don't move */
            if (!did_move_body) do_move = FALSE;
			/* almost always wake the monster you're pushing past */
			if (did_move_body) odds += 55;
			/* less likely to wake monsters while roaming */
			if (m_ptr->roaming) odds -= 14;
			/* don't wake non-agressive monsters */
			if (nr_ptr->sleep == 255) odds = 0;

			/* sleeping monster in the way */
			if ((randint(100) < odds) &&
				(!n_ptr->roaming) && (n_ptr->csleep))
			{
				int wake;
				/* Get the monster's name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);
				/* Get the sleeping monster's name */
				monster_desc(n_name, sizeof(n_name), n_ptr, 0);

                /* nudge the monster you're running in to instead of moving */
                wake = 1;
                if (r_ptr->level > 2) wake = (r_ptr->level * 2) / 3;
                if (wake > 50) wake = 50;
				if (did_move_body) wake += 25;
                if (n_ptr->csleep > wake)
                {
                    n_ptr->csleep -= ((wake+1)/2);
                }
                else
                {
                    bool nlos = FALSE;
                    if (player_has_los_bold(m_ptr->fy, m_ptr->fx)) nlos = TRUE;
                    if ((projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px)) &&
                       (m_ptr->cdis <= MAX_RANGE)) nlos = TRUE;
                    
                    if (nlos)
                    {
                        n_ptr->csleep = 0;
                    }
                    /* if player isn't in sight, the nudged monster roams */
                    /* (wakes up but remains unaware of the PC) */
                    else
                    {
                        n_ptr->roaming = 1;
                        if (n_ptr->csleep < 51) n_ptr->csleep += ((wake+1)/2);
                    }
                    
                    /* notice if it wakes up */
                    if ((n_ptr->ml) && (m_ptr->ml))
                    {
                        msg_format("%^s wakes up %s.", m_name, n_name);
                    }
                    else if (n_ptr->ml)
                    {
                        if (n_ptr->roaming) msg_format("%^s wakes up but doesn't notice you.", n_name);
                        else msg_format("%^s wakes up.", n_name);
                    }
                }
            }
        }

		/* Creature has been allowed move */
		if (do_move)
		{
			s16b this_o_idx, next_o_idx = 0;

			/* Take a turn */
			do_turn = TRUE;

			/* Move the monster (don't swap with the player) */
			if (cave_m_idx[ny][nx] >= 0)
			{
				bool needclimb = TRUE;
				/* these monsters never have trouble getting out of pits */
				if ((r_ptr->flags2 & (RF2_FLY)) || (r_ptr->flags2 & (RF2_PASS_WALL)) ||
					(r_ptr->flags2 & (RF2_KILL_WALL))) needclimb = FALSE;

				/* monster is climbing out of a pit */
				if ((openpit) && (needclimb))
				{
					int climbstr, die, perc;
					climbstr = 40;
					if (r_ptr->flags2 & (RF2_PASS_DOOR)) climbstr += 20;
					else if (r_ptr->flags2 & (RF2_BASH_DOOR)) climbstr += 10;
					if (strchr("FISXacelr:%&", r_ptr->d_char)) climbstr += 250;
					else if (strchr("DGHEJPUnuxw#,", r_ptr->d_char)) climbstr += 90;
					else if (strchr("LMNTWdin", r_ptr->d_char)) climbstr += 60;
					else if (strchr("ORZgoy", r_ptr->d_char)) climbstr += 30;
					else if (strchr("AKjkpqst", r_ptr->d_char)) climbstr += 10;

					/* how well they climb depends on monster's health and maxhps */
				    perc = 100L * m_ptr->hp / m_ptr->maxhp;
				    if (perc >= 60) climbstr += 9 + r_ptr->level/5;
				    else if (perc >= 25) climbstr += 3 + r_ptr->level/8;
				    else if (perc >= 10) climbstr = (climbstr*2)/3;
				    else climbstr -= climbstr/3;
					if ((m_ptr->confused) && (m_ptr->stunned)) climbstr = climbstr/3;
					else if ((m_ptr->confused) || (m_ptr->stunned)) climbstr = (climbstr*2)/3;

					/* much easier than climbing over rubble */
					die = randint(250 + p_ptr->depth/2);
					if (die > climbstr)
					{
						fail_climb = TRUE;
						if ((player_has_los_bold(ny, nx)) && (m_ptr->ml))
						{
							/* Get the monster name */
							monster_desc(m_name, sizeof(m_name), m_ptr, 0);
							msg_format("%^s is having trouble climbing out of the pit.", m_name);
						}
					}
				}

				if (!fail_climb)
				{
					/* now actually move the monster */
					monster_swap(oy, ox, ny, nx);

					/* HATE_WATER monster take damage if they go into water */
					/* (HATE_WATER monsters with < 10 hp will never go into water) */
					if ((cave_feat[ny][nx] == FEAT_WATER) && 
						(r_ptr->flags7 & (RF7_HATE_WATER)))
					{
						m_ptr->hp -= randint(5) + 1;
						monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);
						msg_format("%^s cringes in the water.", m_name);
					}
					/* if POWERFUL and breathes fire and/or plasma, then it might dry up water */
					if (((r_ptr->flags4 & (RF4_BR_PLAS)) || (r_ptr->flags4 & (RF4_BR_FIRE))) &&
						(r_ptr->flags2 & (RF2_POWERFUL)) && (cave_feat[ny][nx] == FEAT_WATER))
					{
						if (randint(100) < 33 + (r_ptr->level/3))
						{
							monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);
							msg_format("The water under %s evaporates.", m_name);
							cave_set_feat(ny, nx, FEAT_FLOOR);
						}
					}
				}
			}

			/* Monster is dragging the player with it */
			if ((p_ptr->timed[TMD_BEAR_HOLD]) && (cave_m_idx[ny][nx] == p_ptr->held_m_idx))
			{
				if (distance(ny, nx, p_ptr->py, p_ptr->px) > m_ptr->cdis) monster_swap(p_ptr->py, p_ptr->px, oy, ox);
			}

			/* Possible disturb */
			if (m_ptr->ml &&
			    (disturb_move ||
			     ((m_ptr->mflag & (MFLAG_VIEW)) &&
	             (!m_ptr->csleep) && disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}


			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[ny][nx]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;
				bool nopickup = FALSE;

				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Skip gold */
				if (o_ptr->tval == TV_GOLD) continue;
				
				/* don't let monsters carry the special chests out of vaults */
				/* also, they wouldn't bother with ruined chests */
                if ((o_ptr->tval == TV_CHEST) && ((o_ptr->sval == SV_RUINED_CHEST) || 
				   (o_ptr->sval == SV_SP_SILVER_CHEST) || (o_ptr->sval == SV_SP_GOLD_CHEST)))
				{
                    /* special chests can't be picked up or destroyed by monsters */
                    if ((o_ptr->sval == SV_SP_SILVER_CHEST) || (o_ptr->sval == SV_SP_GOLD_CHEST))
                       continue;
                    /* let the monsters destroy ruined chests */
                    else nopickup = TRUE;
                }

				/* monsters usually don't notice objects buried in rubble */
				if ((o_ptr->hidden) && (randint(100) < 80 - (r_ptr->level/5)))
				{
					nopickup = TRUE;
				}

				/* Take or Kill objects on the floor */
				if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
				    (r_ptr->flags2 & (RF2_KILL_ITEM)))
				{
					u32b f1, f2, f3, f4;

					u32b flg3 = 0L;

					char m_name[80];
					char o_name[80];

					/* Extract some flags */
					object_flags(o_ptr, &f1, &f2, &f3, &f4);

					/* Get the object name */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

					/* Get the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);

					/* React to objects that hurt the monster */
					if (f2 & (TR2_KILL_DRAGON)) flg3 |= (RF3_DRAGON);
					if (f2 & (TR2_KILL_DEMON)) flg3 |= (RF3_DEMON);
					if (f2 & (TR2_KILL_UNDEAD)) flg3 |= (RF3_UNDEAD);
					if (f1 & (TR1_SLAY_DRAGON)) flg3 |= (RF3_DRAGON);
					if (f1 & (TR1_SLAY_SILVER)) flg3 |= (RF3_SILVER);
					if (f1 & (TR1_SLAY_BUG)) flg3 |= (RF3_BUG);
					if (f1 & (TR1_SLAY_WERE)) flg3 |= (RF3_HURT_SILV);
					if (f1 & (TR1_SLAY_LITE)) flg3 |= (RF3_HURT_DARK);
					if (f1 & (TR1_SLAY_TROLL)) flg3 |= (RF3_TROLL);
					if (f1 & (TR1_SLAY_GIANT)) flg3 |= (RF3_GIANT);
					/* if (f1 & (TR1_SLAY_ORC)) flg3 |= (RF3_ORC);  orcs kill each other often */
					if (f1 & (TR1_SLAY_DEMON)) flg3 |= (RF3_DEMON);
					if (f1 & (TR1_SLAY_UNDEAD)) flg3 |= (RF3_UNDEAD);
					if (f2 & (TR2_SLAY_ANIMAL)) flg3 |= (RF3_ANIMAL);
					/* if (f1 & (TR1_SLAY_EVIL)) flg3 |= (RF3_EVIL); */

					/* The object cannot be picked up by the monster */
					if (artifact_p(o_ptr) || (r_ptr->flags3 & flg3))
					{
						/* Only give a message for "take_item" */
						if (r_ptr->flags2 & (RF2_TAKE_ITEM))
						{
							/* Take note */
							did_take_item = TRUE;

							/* Describe observable situations */
							if (m_ptr->ml && player_has_los_bold(ny, nx) && !squelch_hide_item(o_ptr))
							{
								/* Dump a message */
								msg_format("%^s tries to pick up %s, but fails.",
								           m_name, o_name);
							}
						}
					}

					/* Pick up the item */
					else if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) && (!nopickup))
					{
						object_type *i_ptr;
						object_type object_type_body;

						/* Take note */
						did_take_item = TRUE;

						/* Describe observable situations */
						if (player_has_los_bold(ny, nx) && !squelch_hide_item(o_ptr))
						{
							/* Dump a message */
							msg_format("%^s picks up %s.", m_name, o_name);
						}

						/* item is not buried in rubble */
						o_ptr->hidden = 0;
						p_ptr->window |= PW_OBJLIST;

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
					else if (r_ptr->flags2 & (RF2_KILL_ITEM))
					{
						/* Take note */
						did_kill_item = TRUE;

						/* Describe observable situations */
						if (player_has_los_bold(ny, nx) && !squelch_hide_item(o_ptr))
						{
							/* Dump a message */
							message_format(MSG_DESTROY, 0, "%^s crushes %s.", m_name, o_name);
						}

						/* Delete the object */
						delete_object_idx(this_o_idx);
						p_ptr->window |= PW_OBJLIST;
					}
				}
			}
		}
		
		/* Daylight effect */
		if ((p_ptr->timed[TMD_DAYLIGHT]) && (m_ptr->cdis < 5))
        {
           /* true if monster dies */
           if (do_daylight(m_idx, ny, nx)) return;
        }

		/* Stop when done */
		if (do_turn) break;
	}

	/* fake flag for flexibility flapping froglegs freely */
	ifsmart = FALSE;
	if (adult_ai_smart) ifsmart = TRUE;
	if ((pause) && (r_ptr->flags2 & (RF2_SMART)) && (randint(100) < 85))
        ifsmart = TRUE;
	else if ((pause) && (randint(100) < 67)) ifsmart = TRUE;
	if ((m_ptr->stunned) || (m_ptr->confused)) ifsmart = FALSE;
	if ((m_ptr->roaming) && (m_ptr->roaming < 20)) ifsmart = FALSE;

	/* has given up its move but not its turn */
	if (pause) do_turn = FALSE;

	/* If we haven't done anything, try casting a spell again */
	if (((ifsmart) && !do_turn && !do_move))
	{
		/* Cast spell */
		if (make_attack_spell(m_idx)) return;
	}

	/* lasting repugnance (not sure if m_ptr->cdis is updated at this point..) */
	if ((do_turn) && (p_ptr->timed[TMD_STINKY]) && (!m_ptr->roaming))
	{
		int d = distance(ny, nx, p_ptr->py, p_ptr->px);
		if ((d <= 3) && (randint(100) < 35 + ((goodluck+1)/2)))
			(void)do_repugnance(m_idx);
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
		if (did_open_door) l_ptr->flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) l_ptr->flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) l_ptr->flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) l_ptr->flags2 |= (RF2_KILL_ITEM);

		/* Monster pushed past another monster */
		if (did_move_body) l_ptr->flags2 |= (RF2_MOVE_BODY);

		/* Monster ate another monster */
		if (did_kill_body) l_ptr->flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) l_ptr->flags2 |= (RF2_PASS_WALL);

		/* Monster passed through a door or rubble */
		if (did_pass_door) l_ptr->flags2 |= (RF2_PASS_DOOR);

		/* Monster destroyed a wall */
		if (did_kill_wall) l_ptr->flags2 |= (RF2_KILL_WALL);
	}


	/* Hack -- get "bold" if out of options */
	if (!do_turn && !do_move && m_ptr->monfear)
	{
		/* No longer afraid */
		m_ptr->monfear = 0;
		m_ptr->bold = TRUE;

		/* Message if seen */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);

			/* Dump a message */
			msg_format("%^s turns to fight!", m_name);
		}

		/* XXX XXX XXX Actually do something now (?) */
	}
}


#ifdef MONSTER_FLOW

static bool monster_can_flow(int m_idx)
{
	/* Hack -- Monsters can "smell" the player from far away */
	if (adult_ai_sound)
	{
		monster_type *m_ptr = &mon_list[m_idx];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Monster location */
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Check the flow (normal aaf is about 20) */
		if ((cave_when[fy][fx] == cave_when[p_ptr->py][p_ptr->px]) &&
		    (cave_cost[fy][fx] < MONSTER_FLOW_DEPTH) &&
		    (cave_cost[fy][fx] < r_ptr->aaf))
		{
			return TRUE;
		}
	}

	return FALSE;
}

#else /* MONSTER_FLOW */

static bool monster_can_flow(int m_idx)
{
	/* Unused parameter */
	(void)m_idx;

	return FALSE;
}

#endif /* MONSTER_FLOW */




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
 * Note the special "MFLAG_NICE" flag, which prevents "nasty" monsters from
 * using any of their spell attacks until the player gets a turn.  This flag
 * is optimized via the "repair_mflag_nice" flag.
 */
void process_monsters(byte minimum_energy)
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

		/* monster is temporarily dead */
        if (m_ptr->temp_death) continue;

		/* Not enough energy to move */
		if (m_ptr->energy < minimum_energy) continue;

		/* Use up "some" energy */
		m_ptr->energy -= 100;

		/* Heal monster? XXX XXX XXX */

		/* Get the race */
		r_ptr = &r_info[m_ptr->r_idx];
		
		/*
		 * Process the monster if the monster either:
		 * - can "sense" the player
		 * - is hurt
		 * - can "see" the player (checked backwards)
		 * - can "smell" the player from far away (flow)
		 */
		if ((m_ptr->cdis <= r_ptr->aaf) ||
		    (m_ptr->hp < m_ptr->maxhp) ||
		    player_has_los_bold(m_ptr->fy, m_ptr->fx) ||
		    monster_can_flow(i))
		{
			int zapped = 0;

			/* Process the monster */
			process_monster(i);
			
			/* Zapping effect */
			if ((p_ptr->timed[TMD_ZAPPING]) && (m_ptr->cdis < 4))
			{
				/* returns damage to do later */
				zapped = do_zapping(m_ptr, m_ptr->fy, m_ptr->fx);
			}
		}
	}
}
