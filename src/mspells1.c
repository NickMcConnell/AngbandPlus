/* CVS: Last edit by $Author: sfuerst $ on $Date: 2000/08/21 11:27:14 $ */
/* File: mspells1.c */

/* Purpose: Monster spells (attack player) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


#ifdef DRS_SMART_OPTIONS

/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Original idea and code by "DRS" (David Reeve Sward).
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

#define int_outof(dumb,prob) \
	(randint((dumb)?((prob) / 2):(prob)) < 100) 

/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);

	u32b smart = 0L;
	bool is_dumb = (!(r_ptr->flags2 & RF2_SMART));

	/* Too stupid to know anything */
	if (r_ptr->flags2 & RF2_STUPID) return;


	/* Must be cheating or learning */
	if (!smart_cheat && !smart_learn) return;


	/* Update acquired knowledge */
	if (smart_learn)
	{
		/* Hack -- Occasionally forget player status */
		if (m_ptr->smart && (rand_int(100) < 1)) m_ptr->smart = 0L;

		/* Use the memorized flags */
		smart = m_ptr->smart;
	}


	/* Cheat if requested */
	if (smart_cheat)
	{
		/* Know basic info */
		if (p_ptr->resist_acid) smart |= (SM_RES_ACID);
		if (p_ptr->oppose_acid) smart |= (SM_OPP_ACID);
		if (p_ptr->immune_acid) smart |= (SM_IMM_ACID);
		if (p_ptr->resist_elec) smart |= (SM_RES_ELEC);
		if (p_ptr->oppose_elec) smart |= (SM_OPP_ELEC);
		if (p_ptr->immune_elec) smart |= (SM_IMM_ELEC);
		if (p_ptr->resist_fire) smart |= (SM_RES_FIRE);
		if (p_ptr->oppose_fire) smart |= (SM_OPP_FIRE);
		if (p_ptr->immune_fire) smart |= (SM_IMM_FIRE);
		if (p_ptr->resist_cold) smart |= (SM_RES_COLD);
		if (p_ptr->oppose_cold) smart |= (SM_OPP_COLD);
		if (p_ptr->immune_cold) smart |= (SM_IMM_COLD);

		/* Know poison info */
		if (p_ptr->resist_pois) smart |= (SM_RES_POIS);
		if (p_ptr->oppose_pois) smart |= (SM_OPP_POIS);

		/* Know special resistances */
		if (p_ptr->resist_neth) smart |= (SM_RES_NETH);
		if (p_ptr->resist_lite) smart |= (SM_RES_LITE);
		if (p_ptr->resist_dark) smart |= (SM_RES_DARK);
		if (p_ptr->resist_fear) smart |= (SM_RES_FEAR);
		if (p_ptr->resist_conf) smart |= (SM_RES_CONF);
		if (p_ptr->resist_chaos) smart |= (SM_RES_CHAOS);
		if (p_ptr->resist_disen) smart |= (SM_RES_DISEN);
		if (p_ptr->resist_blind) smart |= (SM_RES_BLIND);
		if (p_ptr->resist_nexus) smart |= (SM_RES_NEXUS);
		if (p_ptr->resist_sound) smart |= (SM_RES_SOUND);
		if (p_ptr->resist_shard) smart |= (SM_RES_SHARD);
		if (p_ptr->reflect) smart |= (SM_IMM_REFLECT);

		/* Know bizarre "resistances" */
		if (p_ptr->free_act) smart |= (SM_IMM_FREE);
		if (!p_ptr->msp) smart |= (SM_IMM_MANA);
	}


	/* Nothing known */
	if (!smart) return;

	/* 
	 * Hack - some of the RNG calls have been removed from the
	 * earlier code.  This should speed it up.
	 */

	if ((smart & SM_IMM_ACID) && (int_outof(is_dumb, 100)))
	{
		f4 &= ~(RF4_BR_ACID);
		f5 &= ~(RF5_BA_ACID | RF5_BO_ACID);
	}
	else if ((smart & (SM_OPP_ACID)) && (smart & (SM_RES_ACID))
		 && (int_outof(is_dumb, 80)))
	{
		f4 &= ~(RF4_BR_ACID);
		f5 &= ~(RF5_BA_ACID | RF5_BO_ACID);
	}
	else if (((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID)))
		 && (int_outof(is_dumb, 30)))
	{
		f4 &= ~(RF4_BR_ACID);
		f5 &= ~(RF5_BA_ACID | RF5_BO_ACID);
	}


	if ((smart & (SM_IMM_ELEC)) && (int_outof(is_dumb, 100)))
	{
		f4 &= ~(RF4_BR_ELEC);
		f5 &= ~(RF5_BA_ELEC | RF5_BO_ELEC);
	}
	else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC))
		&& (int_outof(is_dumb, 80)))
	{
		f4 &= ~(RF4_BR_ELEC);
		f5 &= ~(RF5_BA_ELEC | RF5_BO_ELEC);
	}
	else if (((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC)))
		 && (int_outof(is_dumb, 30)))
	{
		f4 &= ~(RF4_BR_ELEC);
		f5 &= ~(RF5_BA_ELEC | RF5_BO_ELEC);
	}


	if ((smart & (SM_IMM_FIRE)) && (int_outof(is_dumb, 100)))
	{
		f4 &= ~(RF4_BR_FIRE);
		f5 &= ~(RF5_BA_FIRE | RF5_BO_FIRE);
	}
	else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE))
		 && (int_outof(is_dumb, 80)))
	{
		f4 &= ~(RF4_BR_FIRE);
		f5 &= ~(RF5_BA_FIRE | RF5_BO_FIRE);
	}
	else if (((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE)))
		 && (int_outof(is_dumb, 30)))
	{
		f4 &= ~(RF4_BR_FIRE);
		f5 &= ~(RF5_BA_FIRE | RF5_BO_FIRE);
	}


	if ((smart & (SM_IMM_COLD)) && (int_outof(is_dumb, 100)))
	{
		f4 &= ~(RF4_BR_COLD);
		f5 &= ~(RF5_BA_COLD | RF5_BO_COLD | RF5_BO_ICEE);
	}
	else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD))
		 && (int_outof(is_dumb, 80)))
	{
		f4 &= ~(RF4_BR_COLD);
		f5 &= ~(RF5_BA_COLD | RF5_BO_COLD | RF5_BO_ICEE);
	}
	else if (((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)))
		 && (int_outof(is_dumb, 30)))
	{
		f4 &= ~(RF4_BR_COLD);
		f5 &= ~(RF5_BA_COLD | RF5_BO_COLD | RF5_BO_ICEE);
	}


	if ((smart & (SM_OPP_POIS)) && (smart & (SM_RES_POIS))
		 && (int_outof(is_dumb, 80)))
	{
		f4 &= ~(RF4_BR_POIS);
		f5 &= ~(RF5_BA_POIS);
		
		if (rand_int(2))
		{
			f4 &= ~(RF4_BA_NUKE | RF4_BR_NUKE);
		}
	}
	else if (((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS)))
		 && (int_outof(is_dumb, 30)))
	{
		f4 &= ~(RF4_BR_POIS);
		f5 &= ~(RF5_BA_POIS);
	}


	if ((smart & (SM_RES_NETH)) && (int_outof(is_dumb, 50)))
	{
		f4 &= ~(RF4_BR_NETH);
		f5 &= ~(RF5_BA_NETH | RF5_BO_NETH);
	}

	if ((f4 & (RF4_BR_LITE)) && (smart & (SM_RES_LITE))
		 && (int_outof(is_dumb, 50)))
	{
		f4 &= ~(RF4_BR_LITE);
	}

	if ((smart & (SM_RES_DARK))&& (int_outof(is_dumb, 50)))
	{
		f4 &= ~(RF4_BR_DARK);
		f5 &= ~(RF5_BA_DARK);
	}

	if ((f5 & (RF5_SCARE)) && (smart & (SM_RES_FEAR))
		 && (int_outof(is_dumb, 100)))
	{
		f5 &= ~(RF5_SCARE);
	}

	if ((smart & (SM_RES_CONF)) && (int_outof(is_dumb, 100)))
	{
		f5 &= ~(RF5_CONF);
		
		if (rand_int(2))
		{
			f4 &= ~(RF4_BR_CONF);
		}
	}

	if ((smart & (SM_RES_CHAOS)) && (int_outof(is_dumb, 50)))
	{
		f4 &= ~(RF4_BR_CHAO | RF4_BA_CHAO);
	}

	if ((f4 & (RF4_BR_DISE)) && (smart & (SM_RES_DISEN))
		 && (int_outof(is_dumb, 50)))
	{
		f4 &= ~(RF4_BR_DISE);
	}

	if ((f5 & (RF5_BLIND)) && (smart & (SM_RES_BLIND))
		 && (int_outof(is_dumb, 100)))
	{
		f5 &= ~(RF5_BLIND);
	}

	if ((smart & (SM_RES_NEXUS)) && (int_outof(is_dumb, 50)))
	{
		f4 &= ~(RF4_BR_NEXU);
		f6 &= ~(RF6_TELE_LEVEL);
	}

	if ((f4 & (RF4_BR_SOUN)) && (smart & (SM_RES_SOUND))
		 && (int_outof(is_dumb, 50)))
	{
		f4 &= ~(RF4_BR_SOUN);
	}

	if ((smart & (SM_RES_SHARD)) && (int_outof(is_dumb, 50)))
	{
		f4 &= ~(RF4_BR_SHAR);
		
		if (rand_int(2))
		{
			f4 &= ~(RF4_ROCKET);
		}
	}

	if ((smart & (SM_IMM_REFLECT)) && (int_outof(is_dumb, 100)))
	{
		f5 &= ~(RF5_BO_COLD | RF5_BO_FIRE | RF5_BO_ACID
		 | RF5_BO_ELEC | RF5_BO_POIS | RF5_BO_NETH
		 | RF5_BO_WATE | RF5_BO_MANA | RF5_BO_PLAS
		 | RF5_BO_ICEE | RF5_MISSILE);
		f4 &= ~(RF4_ARROW_1 | RF4_ARROW_2 | RF4_ARROW_3 | RF4_ARROW_4);
	}

	if ((smart & (SM_IMM_FREE)) && (int_outof(is_dumb, 100)))
	{
		f5 &= ~(RF5_HOLD | RF5_SLOW);
	}

	if ((f5 & (RF5_DRAIN_MANA)) && (smart & (SM_IMM_MANA))
		 && (int_outof(is_dumb, 100)))
	{
		f5 &= ~(RF5_DRAIN_MANA);
	}

	/* XXX XXX XXX No spells left? */
	/* if (!f4 && !f5 && !f6) ... */

	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
}

#endif /* DRS_SMART_OPTIONS */


/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */
static bool summon_possible(int y1, int x1)
{
	int y, x;
	int dy, dx;

	cave_type *c_ptr;

	/* Start at the player's location, and check 2 grids in each dir */
	for (dy = -2; dy <= 2; dy++)
	{
		for (dx = -2; dx <= 2; dx++)
		{			
			/* Only check a circular area */
			if ((abs(dx) == 2) && (abs(dy) == 2)) continue;
			
			/* Get square */
			x = x1 + dx;
			y = y1 + dy;

			/* Ignore illegal locations */
			if (!in_bounds(y, x)) continue;
			
			/* Access Grid */
			c_ptr = &cave[y][x];

			/* Hack: no summon on glyph of warding */
			if (c_ptr->feat == FEAT_GLYPH) continue;
			if (c_ptr->feat == FEAT_MINOR_GLYPH) continue;

			/* ...nor on the Pattern */
			if ((c_ptr->feat >= FEAT_PATTERN_START) &&
			    (c_ptr->feat <= FEAT_PATTERN_XTRA2)) continue;

			/* Require empty floor grid in line of sight */
			if (cave_empty_bold(y,x) && los(y1, x1, y, x)) return (TRUE);
		}
	}

	return FALSE;
}


/*
 * Originally, it was possible for a friendly to shoot another friendly.
 * Change it so a "clean shot" means no equally friendly monster is
 * between the attacker and target.
 */
/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will
 * return FALSE if a monster is in the way.
 * no equally friendly monster is
 * between the attacker and target.
 *
 * This change has been implelemented via a flag - much quicker
 * and simpler than before.
 */
/* Must be the same as projectable() */
bool clean_shot(int y1, int x1, int y2, int x2, bool friend)
{
	int grid_n;
	coord grid_g[512];

	/* Check the projection path */
	if (friend)
	{
		grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, PROJECT_FRND);
	}
	else
	{
		grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, 0);
	}

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* May not end in an unrequested grid */
	if ((grid_g[grid_n-1].y != y2) ||
		 (grid_g[grid_n-1].x != x2)) return (FALSE);

	return (TRUE);
}

/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void bolt(int m_idx, int typ, int dam_hp)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, py, px, dam_hp, typ, flg);
}


/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void breath(int m_idx, int typ, int dam_hp, int rad, bool breath)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	if (rad < 1) rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

	/* Handle breath attacks */
	if (breath) rad = 0 - rad;

	/* Target the player with a ball attack */
	(void)project(m_idx, rad, py, px, dam_hp, typ, flg);
}


void curse_equipment(int chance, int heavy_chance)
{
	bool        changed = FALSE;
	u32b        o1, o2, o3;
	object_type *o_ptr = &inventory[INVEN_WIELD + rand_int(12)];

	if (randint(100) > chance) return;

	if (!o_ptr->k_idx) return;

	object_flags(o_ptr, &o1, &o2, &o3);


	/* Extra, biased saving throw for blessed items */
	if ((o3 & TR3_BLESSED) && (randint(888) > chance))
	{
		char o_name[256];
		object_desc(o_name, o_ptr, FALSE, 0);
		msg_format("Your %s resist%s cursing!", o_name,
			((o_ptr->number > 1) ? "" : "s"));
		/* Hmmm -- can we wear multiple items? If not, this is unnecessary */
		return;
	}

	if ((randint(100) <= heavy_chance) &&
		(o_ptr->name1 || o_ptr->name2 || o_ptr->art_name))
	{
		if (!(o3 & TR3_HEAVY_CURSE))
			changed = TRUE;
		o_ptr->art_flags3 |= TR3_HEAVY_CURSE;
		o_ptr->art_flags3 |= TR3_CURSED;
		o_ptr->ident |= IDENT_CURSED;
	}
	else
	{
		if (!(o_ptr->ident & IDENT_CURSED))
			changed = TRUE;
		o_ptr->art_flags3 |= TR3_CURSED;
		o_ptr->ident |= IDENT_CURSED;
	}

	if (changed)
	{
		msg_print("There is a malignant black aura surrounding you...");
		o_ptr->feeling = FEEL_NONE;
	}
}


/*
 * Have a monster choose a spell from a list of "useful" spells.
 *
 * Note that this list does NOT include spells that will just hit
 * other monsters, and the list is restricted when the monster is
 * "desperate".  Should that be the job of this function instead?
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * Use the helper functions above to put spells into categories.
 *
 * This function may well be an efficiency bottleneck.
 */
static int choose_attack_spell(int m_idx, u32b f4, u32b f5, u32b f6)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4_mask = 0L;
	u32b f5_mask = 0L;
	u32b f6_mask = 0L;

	bool has_escape, has_attack, has_summon, has_tactic;
	bool has_annoy, has_invul, has_haste, has_heal;
	
	int num = 0;
	byte spells[96];

	int i;

	/* Smart monsters restrict their spell choices. */
	if (!stupid_monsters && !(r_ptr->flags2 & (RF2_STUPID)))
	{
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
		has_invul = ((f4 & (RF4_INVULN_MASK)) ||
			     (f5 & (RF5_INVULN_MASK)) ||
			     (f6 & (RF6_INVULN_MASK)));
		has_haste = ((f4 & (RF4_HASTE_MASK)) ||
		             (f5 & (RF5_HASTE_MASK)) ||
		             (f6 & (RF6_HASTE_MASK)));
		has_heal = ((f4 & (RF4_HEAL_MASK)) ||
		            (f5 & (RF5_HEAL_MASK)) ||
		            (f6 & (RF6_HEAL_MASK)));
	
		/*** Try to pick an appropriate spell type ***/

		/* Hurt badly or afraid, attempt to flee */
		if (has_escape && ((m_ptr->hp < m_ptr->maxhp / 4) ||
			 m_ptr->monfear) && (!rand_int(2)))
		{
			/* Choose escape spell */
			f4_mask = (RF4_ESCAPE_MASK);
			f5_mask = (RF5_ESCAPE_MASK);
			f6_mask = (RF6_ESCAPE_MASK);
		}

		/* Still hurt badly, couldn't flee, attempt to heal */
		else if (has_heal && (m_ptr->hp < m_ptr->maxhp / 4) &&
			 (!rand_int(2)))
		{
			/* Choose heal spell */
			f4_mask = (RF4_HEAL_MASK);
			f5_mask = (RF5_HEAL_MASK);
			f6_mask = (RF6_HEAL_MASK);
		}

		/* Player is close and we have attack spells, blink away */
		else if (has_tactic && (m_ptr->cdis < 4) && has_attack &&
			 (rand_int(100) < 75))
		{
			/* Choose tactical spell */
			f4_mask = (RF4_TACTIC_MASK);
			f5_mask = (RF5_TACTIC_MASK);
			f6_mask = (RF6_TACTIC_MASK);
		}

		/* We're hurt (not badly), try to heal */
		else if ((m_ptr->hp < m_ptr->maxhp * 3 / 4)
			 && (rand_int(100) < 60))
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

		/* Cast globe of invulnerability if not already in effect */
		else if (has_invul && !(m_ptr->invulner)
			 && (rand_int(100) < 50))
		{
			/* Choose Globe of Invulnerability */
			f4_mask = (RF4_INVULN_MASK);
			f5_mask = (RF5_INVULN_MASK);
			f6_mask = (RF6_INVULN_MASK);
		}

		/* Haste self if we aren't already somewhat hasted (rarely) */
		else if (has_haste && (rand_int(100) < (20 + r_ptr->speed
			 - m_ptr->mspeed)))
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
	
	/* Extract the "innate" spells */
	for (i = 0; i < 32; i++)
	{
		if (f4 & (1L << i)) spells[num++] = i + 32 * 3;
	}

	/* Extract the "normal" spells */
	for (i = 0; i < 32; i++)
	{
		if (f5 & (1L << i)) spells[num++] = i + 32 * 4;
	}

	/* Extract the "bizarre" spells */
	for (i = 0; i < 32; i++)
	{
		if (f6 & (1L << i)) spells[num++] = i + 32 * 5;
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
 * if the player is blind.  See also "effects.c" for other "mistakes".
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Perhaps smart monsters should decline to use "bolt" spells if
 * there is a monster in the way, unless they wish to kill it.
 *
 * Note that, to allow the use of the "track_target" option at some
 * later time, certain non-optimal things are done in the code below,
 * including explicit checks against the "direct" variable, which is
 * currently always true by the time it is checked, but which should
 * really be set according to an explicit "projectable()" test, and
 * the use of generic "x,y" locations instead of the player location,
 * with those values being initialized with the player location.
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
 * prevent "blindness" attacks and such from bending around walls, etc,
 * and to allow the use of the "track_target" option in the future.
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
	int             k, chance, thrown_spell, rlev, failrate;
	u32b            f4, f5, f6;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	char            m_name[80];
	char            m_poss[80];
	char            ddesc[80];
	bool            no_inate = FALSE;

	/* Target location */
	int x = px;
	int y = py;

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
	if (m_ptr->mflag & MFLAG_NICE) return (FALSE);
	if (!is_hostile(m_ptr)) return (FALSE);

	/* Hack -- Extract the spell probability */
	chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance) return (FALSE);


	if (stupid_monsters)
	{
		/* Only do spells occasionally */
		if (rand_int(100) >= chance) return (FALSE);
	}
	else
	{
		if (rand_int(100) >= chance) return (FALSE);

		/* Sometimes forbid inate attacks (breaths) */
		if (rand_int(100) >= (chance * 2)) no_inate = TRUE;
	}

	/* XXX XXX XXX Handle "track_target" option (?) */


	/* Hack -- require projectable player */
	if (normal)
	{
		/* Check range */
		if (m_ptr->cdis > MAX_RANGE) return (FALSE);

		/* Check path */
		if (!projectable(m_ptr->fy, m_ptr->fx, py, px)) return (FALSE);
	}

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;

	if (!stupid_monsters)
	{
		/* Forbid inate attacks sometimes */
		if (no_inate) f4 = 0L;
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

	/* No spells left */
	if (!f4 && !f5 && !f6) return (FALSE);

#endif /* DRS_SMART_OPTIONS */

	if (!stupid_monsters)
	{
		/* Check for a clean bolt shot */
		if (((f4 & RF4_BOLT_MASK) ||
		     (f5 & RF5_BOLT_MASK) ||
		     (f6 & RF6_BOLT_MASK)) &&
		     !(r_ptr->flags2 & RF2_STUPID) &&
		     !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE))
		{
			/* Remove spells that will only hurt friends */
			f4 &= ~(RF4_BOLT_MASK);
			f5 &= ~(RF5_BOLT_MASK);
			f6 &= ~(RF6_BOLT_MASK);
		}

		/* Check for a possible summon */
		if (((f4 & RF4_SUMMON_MASK) ||
		     (f5 & RF5_SUMMON_MASK) ||
		     (f6 & RF6_SUMMON_MASK)) &&
		     !(r_ptr->flags2 & RF2_STUPID) &&
		     !(summon_possible(py, px)))
		{
			/* Remove summoning spells */
			f4 &= ~(RF4_SUMMON_MASK);
			f5 &= ~(RF5_SUMMON_MASK);
			f6 &= ~(RF6_SUMMON_MASK);
		}

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

	/* Stop if player is dead or gone */
	if (!alive || death) return (FALSE);

	/* Stop if player is leaving */
	if (p_ptr->leaving) return (FALSE);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, 0x88);

	thrown_spell = choose_attack_spell(m_idx, f4, f5, f6);

	/* Abort if no spell was chosen */
	if (!thrown_spell) return (FALSE);

	/* Calculate spell failure rate */
	failrate = 25 - (rlev + 3) / 4;

	/* Hack -- Stupid monsters will never fail (for jellies and such) */
	if (r_ptr->flags2 & RF2_STUPID) failrate = 0;

	/* Check for spell failure (inate attacks never fail) */
	if ((thrown_spell >= 128) && (rand_int(100) < failrate))
	{
		/* Message */
		msg_format("%^s tries to cast a spell, but fails.", m_name);

		return (TRUE);
	}

	/* Cast the spell. */
	switch (thrown_spell)
	{
		/* RF4_SHRIEK */
		case 96+0:
		{
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx);
			break;
		}

		/* RF4_XXX2X4 */
		case 96+1:
		{
			break;
		}

		/* RF4_XXX3X4 */
		case 96+2:
		{
			break;
		}

		/* RF4_XXX4X4 */
		case 96+3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s shoots something.", m_name);
			else msg_format("%^s fires a rocket.", m_name);
			breath(m_idx, GF_ROCKET,
				((m_ptr->hp / 4) > 600 ? 600 : (m_ptr->hp / 4)), 2, FALSE);
			update_smart_learn(m_idx, DRS_SHARD);
			break;
		}

		/* RF4_ARROW_1 */
		case 96+4:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires an arrow.", m_name);
			bolt(m_idx, GF_ARROW, damroll(1, 6));
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF4_ARROW_2 */
		case 96+5:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires an arrow!", m_name);
			bolt(m_idx, GF_ARROW, damroll(3, 6));
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF4_ARROW_3 */
		case 96+6:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires a bolt.", m_name);
			bolt(m_idx, GF_ARROW, damroll(5, 6));
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF4_ARROW_4 */
		case 96+7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires a bolt!", m_name);
			bolt(m_idx, GF_ARROW, damroll(7, 6));
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF4_BR_ACID */
		case 96+8:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes acid.", m_name);
			breath(m_idx, GF_ACID,
				((m_ptr->hp / 3) > 1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);
			update_smart_learn(m_idx, DRS_ACID);
			break;
		}

		/* RF4_BR_ELEC */
		case 96+9:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes lightning.", m_name);
			breath(m_idx, GF_ELEC,
				((m_ptr->hp / 3) > 1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);
			update_smart_learn(m_idx, DRS_ELEC);
			break;
		}

		/* RF4_BR_FIRE */
		case 96+10:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes fire.", m_name);
			breath(m_idx, GF_FIRE,
				((m_ptr->hp / 3) > 1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);
			update_smart_learn(m_idx, DRS_FIRE);
			break;
		}

		/* RF4_BR_COLD */
		case 96+11:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes frost.", m_name);
			breath(m_idx, GF_COLD,
				((m_ptr->hp / 3) > 1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);
			update_smart_learn(m_idx, DRS_COLD);
			break;
		}

		/* RF4_BR_POIS */
		case 96+12:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gas.", m_name);
			breath(m_idx, GF_POIS,
				((m_ptr->hp / 3) > 600 ? 600 : (m_ptr->hp / 3)), 0, TRUE);
			update_smart_learn(m_idx, DRS_POIS);
			break;
		}


		/* RF4_BR_NETH */
		case 96+13:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nether.", m_name);
			breath(m_idx, GF_NETHER,
				((m_ptr->hp / 6) > 450 ? 450 : (m_ptr->hp / 6)), 0, TRUE);
			update_smart_learn(m_idx, DRS_NETH);
			break;
		}

		/* RF4_BR_LITE */
		case 96+14:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes light.", m_name);
			breath(m_idx, GF_LITE,
				((m_ptr->hp / 4) > 350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);
			update_smart_learn(m_idx, DRS_LITE);
			break;
		}

		/* RF4_BR_DARK */
		case 96+15:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes darkness.", m_name);
			breath(m_idx, GF_DARK,
				((m_ptr->hp / 4) > 350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);
			update_smart_learn(m_idx, DRS_DARK);
			break;
		}

		/* RF4_BR_CONF */
		case 96+16:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes confusion.", m_name);
			breath(m_idx, GF_CONFUSION,
				((m_ptr->hp / 4) > 350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);
			update_smart_learn(m_idx, DRS_CONF);
			break;
		}

		/* RF4_BR_SOUN */
		case 96+17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes sound.", m_name);
			breath(m_idx, GF_SOUND,
				((m_ptr->hp / 4) > 350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);
			update_smart_learn(m_idx, DRS_SOUND);
			break;
		}

		/* RF4_BR_CHAO */
		case 96+18:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes chaos.", m_name);
			breath(m_idx, GF_CHAOS,
				((m_ptr->hp / 4) > 500 ? 500 : (m_ptr->hp / 4)), 0, TRUE);
			update_smart_learn(m_idx, DRS_CHAOS);
			break;
		}

		/* RF4_BR_DISE */
		case 96+19:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes disenchantment.", m_name);
			breath(m_idx, GF_DISENCHANT,
				((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)), 0, TRUE);
			update_smart_learn(m_idx, DRS_DISEN);
			break;
		}

		/* RF4_BR_NEXU */
		case 96+20:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nexus.", m_name);
			breath(m_idx, GF_NEXUS,
				((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)), 0, TRUE);
			update_smart_learn(m_idx, DRS_NEXUS);
			break;
		}

		/* RF4_BR_TIME */
		case 96+21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes time.", m_name);
			breath(m_idx, GF_TIME,
				((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3)), 0, TRUE);
			break;
		}

		/* RF4_BR_INER */
		case 96+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes inertia.", m_name);
			breath(m_idx, GF_INERTIA,
				((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)), 0, TRUE);
			break;
		}

		/* RF4_BR_GRAV */
		case 96+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gravity.", m_name);
			breath(m_idx, GF_GRAVITY,
				((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)), 0, TRUE);
			break;
		}

		/* RF4_BR_SHAR */
		case 96+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes shards.", m_name);
			breath(m_idx, GF_SHARDS,
				((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)), 0, TRUE);
			update_smart_learn(m_idx, DRS_SHARD);
			break;
		}

		/* RF4_BR_PLAS */
		case 96+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes plasma.", m_name);
			breath(m_idx, GF_PLASMA,
				((m_ptr->hp / 4) > 150 ? 150 : (m_ptr->hp / 4)), 0, TRUE);
			break;
		}

		/* RF4_BR_WALL */
		case 96+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes force.", m_name);
			breath(m_idx, GF_FORCE,
				((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)), 0, TRUE);
			break;
		}

		/* RF4_BR_MANA */
		case 96+27:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes mana.", m_name);
			breath(m_idx, GF_MANA,
				((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)), 0, TRUE);
			break;
		}

		/* RF4_BA_NUKE */
		case 96+28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a ball of radiation.", m_name);
			breath(m_idx, GF_NUKE, (rlev + damroll(10, 6)), 2, FALSE);
			update_smart_learn(m_idx, DRS_POIS);
			break;
		}

		/* RF4_BR_NUKE */
		case 96+29:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes toxic waste.", m_name);
			breath(m_idx, GF_NUKE,
				((m_ptr->hp / 2) > 600 ? 600 : (m_ptr->hp / 2)), 0, TRUE);
			update_smart_learn(m_idx, DRS_POIS);
			break;
		}

		/* RF4_BA_CHAO */
		case 96+30:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles frighteningly.", m_name);
			else msg_format("%^s invokes raw Logrus.", m_name);
			breath(m_idx, GF_CHAOS, (rlev * 2) + damroll(10, 10), 4, FALSE);
			update_smart_learn(m_idx, DRS_CHAOS);
			break;
		}

		/* RF4_BR_DISI */
		case 96+31:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes disintegration.", m_name);
			breath(m_idx, GF_DISINTEGRATE,
				((m_ptr->hp / 3) > 300 ? 300 : (m_ptr->hp / 3)), 0, TRUE);
			break;
		}



		/* RF5_BA_ACID */
		case 128+0:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts an acid ball.", m_name);
			breath(m_idx, GF_ACID,
				randint(rlev * 3) + 15, 2, FALSE);
			update_smart_learn(m_idx, DRS_ACID);
			break;
		}

		/* RF5_BA_ELEC */
		case 128+1:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a lightning ball.", m_name);
			breath(m_idx, GF_ELEC,
				randint(rlev * 3 / 2) + 8, 2, FALSE);
			update_smart_learn(m_idx, DRS_ELEC);
			break;
		}

		/* RF5_BA_FIRE */
		case 128+2:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a fire ball.", m_name);
			breath(m_idx, GF_FIRE,
				randint(rlev * 7 / 2) + 10, 2, FALSE);
			update_smart_learn(m_idx, DRS_FIRE);
			break;
		}

		/* RF5_BA_COLD */
		case 128+3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a frost ball.", m_name);
			breath(m_idx, GF_COLD,
				randint(rlev * 3 / 2) + 10, 2, FALSE);
			update_smart_learn(m_idx, DRS_COLD);
			break;
		}

		/* RF5_BA_POIS */
		case 128+4:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a stinking cloud.", m_name);
			breath(m_idx, GF_POIS,
				damroll(12, 2), 2, FALSE);
			update_smart_learn(m_idx, DRS_POIS);
			break;
		}

		/* RF5_BA_NETH */
		case 128+5:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a nether ball.", m_name);
			breath(m_idx, GF_NETHER,
				(50 + damroll(10, 10) + rlev), 2, FALSE);
			update_smart_learn(m_idx, DRS_NETH);
			break;
		}

		/* RF5_BA_WATE */
		case 128+6:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s gestures fluidly.", m_name);
			msg_print("You are engulfed in a whirlpool.");
			breath(m_idx, GF_WATER,
				randint(rlev * 5 / 2) + 50, 4, FALSE);
			break;
		}

		/* RF5_BA_MANA */
		case 128+7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
			else msg_format("%^s invokes a mana storm.", m_name);
			breath(m_idx, GF_MANA,
				(rlev * 4) + damroll(10, 10), 4, FALSE);
			break;
		}

		/* RF5_BA_DARK */
		case 128+8:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
			else msg_format("%^s invokes a darkness storm.", m_name);
			breath(m_idx, GF_DARK,
				(rlev * 4) + damroll(10, 10), 4, FALSE);
			update_smart_learn(m_idx, DRS_DARK);
			break;
		}

		/* RF5_DRAIN_MANA */
		case 128+9:
		{
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
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);

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
						msg_format("%^s appears healthier.", m_name);
					}
				}
			}
			update_smart_learn(m_idx, DRS_MANA);
			break;
		}

		/* RF5_MIND_BLAST */
		case 128+10:
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

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");

				if (!p_ptr->resist_conf)
				{
					(void)set_confused(p_ptr->confused + rand_int(4) + 4);
				}

				if (!p_ptr->resist_chaos && one_in_(3))
				{
					(void)set_image(p_ptr->image + rand_int(250) + 150);
				}

				take_hit(damroll(8, 8), ddesc);
			}
			break;
		}

		/* RF5_BRAIN_SMASH */
		case 128+11:
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
					(void)set_blind(p_ptr->blind + 8 + rand_int(8));
				}
				if (!p_ptr->resist_conf)
				{
					(void)set_confused(p_ptr->confused + rand_int(4) + 4);
				}
				if (!p_ptr->free_act)
				{
					(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
				}
				(void)set_slow(p_ptr->slow + rand_int(4) + 4);

				while (rand_int(100) > p_ptr->skill_sav)
					(void)do_dec_stat(A_INT);
				while (rand_int(100) > p_ptr->skill_sav)
					(void)do_dec_stat(A_WIS);

				if (!p_ptr->resist_chaos)
				{
					(void)set_image(p_ptr->image + rand_int(250) + 150);
				}
			}
			break;
		}

		/* RF5_CAUSE_1 */
		case 128+12:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s points at you and curses.", m_name);
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				curse_equipment(33, 0);
				take_hit(damroll(3, 8), ddesc);
			}
			break;
		}

		/* RF5_CAUSE_2 */
		case 128+13:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s points at you and curses horribly.", m_name);
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				curse_equipment(50, 5);
				take_hit(damroll(8, 8), ddesc);
			}
			break;
		}

		/* RF5_CAUSE_3 */
		case 128+14:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles loudly.", m_name);
			else msg_format("%^s points at you, incanting terribly!", m_name);
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				curse_equipment(80, 15);
				take_hit(damroll(10, 15), ddesc);
			}
			break;
		}

		/* RF5_CAUSE_4 */
		case 128+15:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s screams the word 'DIE!'", m_name);
			else msg_format("%^s points at you, screaming the word DIE!", m_name);
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				take_hit(damroll(15, 15), ddesc);
				(void)set_cut(p_ptr->cut + damroll(10, 10));
			}
			break;
		}

		/* RF5_BO_ACID */
		case 128+16:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a acid bolt.", m_name);
			bolt(m_idx, GF_ACID, damroll(7, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_ACID);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_BO_ELEC */
		case 128+17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a lightning bolt.", m_name);
			bolt(m_idx, GF_ELEC, damroll(4, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_ELEC);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_BO_FIRE */
		case 128+18:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a fire bolt.", m_name);
			bolt(m_idx, GF_FIRE, damroll(9, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_FIRE);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_BO_COLD */
		case 128+19:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a frost bolt.", m_name);
			bolt(m_idx, GF_COLD, damroll(6, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_COLD);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_BO_POIS */
		case 128+20:
		{
			/* XXX XXX XXX */
			break;
		}

		/* RF5_BO_NETH */
		case 128+21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a nether bolt.", m_name);
			bolt(m_idx, GF_NETHER, 30 + damroll(5, 5) + (rlev * 3) / 2);
			update_smart_learn(m_idx, DRS_NETH);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_BO_WATE */
		case 128+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a water bolt.", m_name);
			bolt(m_idx, GF_WATER, damroll(10, 10) + (rlev));
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_BO_MANA */
		case 128+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a mana bolt.", m_name);
			bolt(m_idx, GF_MANA, randint(rlev * 7 / 2) + 50);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_BO_PLAS */
		case 128+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a plasma bolt.", m_name);
			bolt(m_idx, GF_PLASMA, 10 + damroll(8, 7) + (rlev));
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_BO_ICEE */
		case 128+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts an ice bolt.", m_name);
			bolt(m_idx, GF_ICE, damroll(6, 6) + (rlev));
			update_smart_learn(m_idx, DRS_COLD);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_MISSILE */
		case 128+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a magic missile.", m_name);
			bolt(m_idx, GF_MISSILE, damroll(2, 6) + (rlev / 3));
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

		/* RF5_SCARE */
		case 128+27:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
			else msg_format("%^s casts a fearful illusion.", m_name);
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
				(void)set_afraid(p_ptr->afraid + rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FEAR);
			break;
		}

		/* RF5_BLIND */
		case 128+28:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a spell, burning your eyes!", m_name);
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
				(void)set_blind(12 + rand_int(4));
			}
			update_smart_learn(m_idx, DRS_BLIND);
			break;
		}

		/* RF5_CONF */
		case 128+29:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else msg_format("%^s creates a mesmerising illusion.", m_name);
			if (p_ptr->resist_conf)
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else
			{
				(void)set_confused(p_ptr->confused + rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_CONF);
			break;
		}

		/* RF5_SLOW */
		case 128+30:
		{
			if (!direct) break;
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
				(void)set_slow(p_ptr->slow + rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}

		/* RF5_HOLD */
		case 128+31:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s stares deep into your eyes!", m_name);
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
				(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}

		/* RF6_HASTE */
		case 160+0:
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
				msg_format("%^s starts moving faster.", m_name);
				m_ptr->mspeed += 2;
			}

			break;
		}

		/* RF6_HAND_DOOM */
		case 160+1:
		{
			disturb(1, 0);
			msg_format("%^s invokes the Hand of Doom!", m_name);
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_format("You resist the effects!");
			}
			else
			{
				int dummy = (((s32b) ((65 + randint(25)) * (p_ptr->chp))) / 100);
				msg_print("Your feel your life fade away!");
				take_hit(dummy, m_name);
				curse_equipment(100, 20);

				if (p_ptr->chp < 1) p_ptr->chp = 1;
			}
			break;
		}

		/* RF6_HEAL */
		case 160+2:
		{
			disturb(1, 0);

			/* Message */
			if (blind)
			{
				msg_format("%^s mumbles.", m_name);
			}
			else
			{
				msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
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
					msg_format("%^s looks completely healed!", m_name);
				}
				else
				{
					msg_format("%^s sounds completely healed!", m_name);
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
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

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

		/* RF6_INVULNER */
		case 160+3:
		{
			disturb(1, 0);

			/* Message */
			if (!seen)
			{
				msg_format("%^s mumbles powerfully.", m_name);
			}
			else
			{
				msg_format("%^s casts a Globe of Invulnerability.", m_name);
			}

			if (!(m_ptr->invulner))
				m_ptr->invulner = randint(4) + 4;

			break;
		}

		/* RF6_BLINK */
		case 160+4:
		{
			disturb(1, 0);
			msg_format("%^s blinks away.", m_name);
			teleport_away(m_idx, 10);
			break;
		}

		/* RF6_TPORT */
		case 160+5:
		{
			disturb(1, 0);
			msg_format("%^s teleports away.", m_name);
			teleport_away(m_idx, MAX_SIGHT * 2 + 5);
			break;
		}

		/* RF6_XXX3X6 */
		case 160+6:
		{
			break;
		}

		/* RF6_XXX4X6 */
		case 160+7:
		{
			break;
		}

		/* RF6_TELE_TO */
		case 160+8:
		{
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s commands you to return.", m_name);
			teleport_player_to(m_ptr->fy, m_ptr->fx);
			break;
		}

		/* RF6_TELE_AWAY */
		case 160+9:
		{
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s teleports you away.", m_name);
			teleport_player(100);
			break;
		}

		/* RF6_TELE_LEVEL */
		case 160+10:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles strangely.", m_name);
			else msg_format("%^s gestures at your feet.", m_name);
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
			update_smart_learn(m_idx, DRS_NEXUS);
			break;
		}

		/* RF6_XXX5 */
		case 160+11:
		{
			break;
		}

		/* RF6_DARKNESS */
		case 160+12:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s gestures in shadow.", m_name);
			(void)unlite_area(0, 3);
			break;
		}

		/* RF6_TRAPS */
		case 160+13:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and then cackles evilly.", m_name);
			else msg_format("%^s casts a spell and cackles evilly.", m_name);
			(void)trap_creation();
			break;
		}

		/* RF6_FORGET */
		case 160+14:
		{
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s tries to blank your mind.", m_name);

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else if (lose_all_info())
			{
				msg_print("Your memories fade away.");
			}
			break;
		}

		/* RF6_RAISE_DEAD */
		case 160+15:
		{
			disturb(1, 0);
			msg_format("%^s mutters quietly.", m_name);		
			
			/*raise_dead(m_ptr->fy, m_ptr->fx, FALSE);*/
			break;
		}

		/* RF6_SUMMON_KIN */
		case 160+16:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons %s %s.",
				m_name, m_poss,
				((r_ptr->flags1) & RF1_UNIQUE ?
				"minions" : "kin"));
			summon_kin_type = r_ptr->d_char; /* Big hack */

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_KIN, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");

			break;
		}

		/* RF6_S_CYBER */
		case 160+17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons Cyberdemons!", m_name);
			if (blind && count) msg_print("You hear heavy steps nearby.");
			summon_cyber(m_idx, y, x);
			break;
		}

		/* RF6_S_MONSTER */
		case 160+18:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons help!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, 0, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF6_S_MONSTERS */
		case 160+19:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons monsters!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, 0, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF6_S_ANT */
		case 160+20:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons ants.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_ANT, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF6_S_SPIDER */
		case 160+21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons spiders.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_SPIDER, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF6_S_HOUND */
		case 160+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hounds.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_HOUND, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF6_S_HYDRA */
		case 160+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hydras.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_HYDRA, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF6_S_ANGEL */
		case 160+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons an angel!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_ANGEL, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF6_S_DEMON */
		case 160+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a demon from the Courts of Chaos!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_DEMON, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF6_S_UNDEAD */
		case 160+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons an undead adversary!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_UNDEAD, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF6_S_DRAGON */
		case 160+27:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a dragon!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_DRAGON, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF6_S_HI_UNDEAD */
		case 160+28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater undead!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_UNDEAD, TRUE, FALSE, FALSE);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

		/* RF6_S_HI_DRAGON */
		case 160+29:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons ancient dragons!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_DRAGON, TRUE, FALSE, FALSE);
			}
			if (blind && count)
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}

		/* RF6_S_AMBERITES */
		case 160+30:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons Lords of Amber!", m_name);


			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_AMBERITES, TRUE, FALSE, FALSE);
			}
			if (blind && count)
			{
				msg_print("You hear immortal beings appear nearby.");
			}
			break;
		}

		/* RF6_S_UNIQUE */
		case 160+31:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons special opponents!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_UNIQUE, TRUE, FALSE, FALSE);
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_UNDEAD, TRUE, FALSE, FALSE);
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
		/* Inate spell */
		if (thrown_spell < 32 * 4)
		{
			r_ptr->r_flags4 |= (1L << (thrown_spell - 32 * 3));
			if (r_ptr->r_cast_inate < MAX_UCHAR) r_ptr->r_cast_inate++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32 * 5)
		{
			r_ptr->r_flags5 |= (1L << (thrown_spell - 32 * 4));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32 * 6)
		{
			r_ptr->r_flags6 |= (1L << (thrown_spell - 32 * 5));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}
	}


	/* Always take note of monsters that kill you */
	if (death && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
	}

	/* A spell was cast */
	return (TRUE);
}
