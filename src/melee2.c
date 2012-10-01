/* File: melee2.c */

/* Monster learning, monster distance attacks and spells, fear, flow/
 * movement, monster AI effecting movement and spells, process a monster 
 * (with spells and actions of all kinds, reproduction, effects of any 
 * terrain on monster movement, picking up and destroying objects), 
 * process all monsters.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


#define SPEAK_CHANCE 16

#ifdef DRS_SMART_OPTIONS


/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Original idea and code by "DRS" (David Reeves Sward).
 *
 * Major modifications by "BEN" (Ben Harrison).
 *
 * Includes Keldon Jones' Monster AI (version 0.1.1) -LM-
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
 * Internal probablility routine
 */
static bool int_outof(monster_race *r_ptr, int prob)
{
	/* Non-Smart monsters are half as "smart" */
	if (!(r_ptr->flags2 & (RF2_SMART))) prob = prob / 2;

	/* Roll the dice */
	return (rand_int(100) < prob);
}



/*
 * Remove the "bad" spells from a spell list.  From Keldon Jones' AI, 
 * modified in Oangband to make breathers breath slightly more often 
 * (a breather will seldom have better ranged damage attacks), and to 
 * make certain that a monster will never give up on attack spells 
 * entirely.
 */
static void remove_bad_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);

	u32b smart = 0L;


	/* Too stupid to know anything */
	if (r_ptr->flags2 & (RF2_STUPID)) return;


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


	/* Cheat if requested, or if a player ghost. -LM- */
	if ((smart_cheat) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
	{
		/* Know weirdness */
		if (p_ptr->free_act) smart |= (SM_IMM_FREE);
		if (!p_ptr->msp) smart |= (SM_IMM_MANA);

		/* Know immunities */
		if (p_ptr->immune_acid) smart |= (SM_IMM_ACID);
		if (p_ptr->immune_elec) smart |= (SM_IMM_ELEC);
		if (p_ptr->immune_fire) smart |= (SM_IMM_FIRE);
		if (p_ptr->immune_cold) smart |= (SM_IMM_COLD);

		/* Know oppositions */
		if (p_ptr->oppose_acid) smart |= (SM_OPP_ACID);
		if (p_ptr->oppose_elec) smart |= (SM_OPP_ELEC);
		if (p_ptr->oppose_fire) smart |= (SM_OPP_FIRE);
		if (p_ptr->oppose_cold) smart |= (SM_OPP_COLD);
		if (p_ptr->oppose_pois) smart |= (SM_OPP_POIS);

		/* Know resistances */
		if (p_ptr->resist_acid) smart |= (SM_RES_ACID);
		if (p_ptr->resist_elec) smart |= (SM_RES_ELEC);
		if (p_ptr->resist_fire) smart |= (SM_RES_FIRE);
		if (p_ptr->resist_cold) smart |= (SM_RES_COLD);
		if (p_ptr->resist_pois) smart |= (SM_RES_POIS);
		if (p_ptr->resist_fear) smart |= (SM_RES_FEAR);
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
	if (!smart) return;


	if (smart & (SM_IMM_ACID))
	{
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_ACID);
	}
	else if ((smart & (SM_OPP_ACID)) && (smart & (SM_RES_ACID)))
	{
		if (int_outof(r_ptr, 60)) f4 &= ~(RF4_BR_ACID);
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
		if (int_outof(r_ptr, 60)) f4 &= ~(RF4_BR_ELEC);
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
		if (int_outof(r_ptr, 60)) f4 &= ~(RF4_BR_FIRE);
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
		if (int_outof(r_ptr, 60)) f4 &= ~(RF4_BR_COLD);
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
		if (int_outof(r_ptr, 70)) f4 &= ~(RF4_BR_POIS);
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
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_DISE);
	}


	if (smart & (SM_IMM_FREE))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_HOLD);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_SLOW);
	}

	/* Once slowed, the player doesn't need to be slowed again. -LM- */
	if (p_ptr->slow) f5 &= ~(RF5_SLOW);

	if (smart & (SM_IMM_MANA))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_DRAIN_MANA);
	}


	/* If nothing seems to work, ignore the bad news. -LM- */
	if (!f4 && !f5 && !f6)
	{
		return;
	}
	/* Otherwise, modify the spell list. */
	else
	{
		(*f4p) = f4;
		(*f5p) = f5;
		(*f6p) = f6;
	}
}


#endif

/*
 * Determine if a bolt spell will hit the player.  From Keldon Jones' AI.
 *
 * This is exactly like "projectable", but it will return FALSE if a monster
 * is in the way.
 */
static bool clean_shot(int y1, int x1, int y2, int x2)
{
       int y, x;

       int grid_n = 0;
       u16b grid_g[512];

       /* Check the projection path */
       grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, PROJECT_STOP);

       /* Source and target the same */
       if (!grid_n) return (FALSE);

       /* Final grid */
       y = GRID_Y(grid_g[grid_n-1]);
       x = GRID_X(grid_g[grid_n-1]);

       /* May not end in a wall grid, unless trees or rubble. -LM- */
       if (!cave_passable_bold(y, x)) return (FALSE);

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY;

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, py, px, dam_hp, typ, flg, 0, 0);
}

/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and (specifically) the player
 */
static void breath(int m_idx, int typ, int dam_hp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int rad;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

	/* Target the player with a ball attack */
	(void)project(m_idx, rad, py, px, dam_hp, typ, flg, 0, 0);
}

/*
 * Return TRUE if a spell is good for hurting the player (directly).
 */
static bool spell_attack(byte spell)
{
       /* All RF4 spells hurt (except for shriek) */
       if (spell < 128 && spell > 96) return (TRUE);

       /* Various "ball" spells */
       if (spell >= 128 && spell <= 128 + 8) return (TRUE);

       /* "Cause wounds" and "bolt" spells */
       if (spell >= 128 + 12 && spell <= 128 + 27) return (TRUE);

       /* Doesn't hurt */
       return (FALSE);
}

/*
 * Return TRUE if a spell is good for escaping.
 */
static bool spell_escape(byte spell)
{
       /* Blink or Teleport */
       if (spell == 160 + 4 || spell == 160 + 5) return (TRUE);

       /* Teleport the player away */
       if (spell == 160 + 9 || spell == 160 + 10) return (TRUE);

       /* Isn't good for escaping */
       return (FALSE);
}

/*
 * Return TRUE if a spell is good for annoying the player.
 */
static bool spell_annoy(byte spell)
{
       /* Shriek */
       if (spell == 96 + 0) return (TRUE);

       /* Brain smash, et al */
       if (spell >= 128 + 9 && spell <= 128 + 11) return (TRUE);

       /* Scare, confuse, blind, slow, paralyze */
       if (spell >= 128 + 27 && spell <= 128 + 31) return (TRUE);

       /* Teleport to */
       if (spell == 160 + 8) return (TRUE);

       /* Darkness, make traps, cause amnesia */
       if (spell >= 160 + 12 && spell <= 160 + 14) return (TRUE);

       /* Doesn't annoy */
       return (FALSE);
}

/*
 * Return TRUE if a spell summons help.
 */
static bool spell_summon(byte spell)
{
       /* All summon spells */
       if (spell >= 160 + 18) return (TRUE);

       /* Doesn't summon */
       return (FALSE);
}

/*
 * Return TRUE if a spell is good in a tactical situation.
 */
static bool spell_tactic(byte spell)
{
       /* Blink */
       if (spell == 160 + 4) return (TRUE);

       /* Not good */
       return (FALSE);
}

/*
 * Return TRUE if a spell hastes.
 */
static bool spell_haste(byte spell)
{
       /* Haste self */
       if (spell == 160 + 0) return (TRUE);

       /* Not a haste spell */
       return (FALSE);
}

/*
 * Return TRUE if a spell is good for healing.
 */
static bool spell_heal(byte spell)
{
       /* Heal */
       if (spell == 160 + 2) return (TRUE);

       /* No healing */
       return (FALSE);
}

/*
 * Have a monster choose a spell from a list of "useful" spells.
 * From Keldon Jones' AI, slightly modified for Oangband use.
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
static int choose_attack_spell(int m_idx, byte spells[], byte num)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int summon_chance, attack_chance, annoy_chance;

	byte escape[96], escape_num = 0;
	byte attack[96], attack_num = 0;
	byte summon[96], summon_num = 0;
	byte tactic[96], tactic_num = 0;
	byte annoy[96], annoy_num = 0;
	byte haste[96], haste_num = 0;
	byte heal[96], heal_num = 0;

	int i, py = p_ptr->py, px = p_ptr->px;
	int rounds = 0;

	/* Stupid monsters choose randomly */
	if (r_ptr->flags2 & (RF2_STUPID))
	{
		/* Pick at random */
		return (spells[rand_int(num)]);
	}

	/* Categorize spells */
	for (i = 0; i < num; i++)
	{
		/* Escape spell? */
		if (spell_escape(spells[i])) escape[escape_num++] = spells[i];

		/* Attack spell? */
		if (spell_attack(spells[i])) attack[attack_num++] = spells[i];

		/* Summon spell? */
		if (spell_summon(spells[i])) summon[summon_num++] = spells[i];

		/* Tactical spell? */
		if (spell_tactic(spells[i])) tactic[tactic_num++] = spells[i];

		/* Annoyance spell? */
		if (spell_annoy(spells[i])) annoy[annoy_num++] = spells[i];

		/* Haste spell? */
		if (spell_haste(spells[i])) haste[haste_num++] = spells[i];

		/* Heal spell? */
		if (spell_heal(spells[i])) heal[heal_num++] = spells[i];
       }

       /*** Try to pick an appropriate spell type ***/

	/* See end of the spell selection choices for notes. -LM-*/ 
	try_again:


	/* Hurt badly or afraid, attempt to flee.  Chance greatly reduced in Oangband. */
	if (((m_ptr->hp < m_ptr->maxhp / 3) || (m_ptr->monfear)) && (rand_int(100) < 35))
	{
		/* Choose escape spell if possible */
		if (escape_num) return (escape[rand_int(escape_num)]);
	}

	/* Still hurt badly, couldn't flee, attempt to heal.  Chance reduced
	 * in Oangband to avoid annoyance. */
	if ((m_ptr->hp < m_ptr->maxhp / 4) && (rand_int(100) < 60))
	{
		/* Choose heal spell if possible */
		if (heal_num) return (heal[rand_int(heal_num)]);
	}


	/* Player is close and we have attack spells, blink away */
	if ((distance(py, px, m_ptr->fy, m_ptr->fx) < 3) && attack_num &&
		(rand_int(100) < 66))
	{
		/* Choose tactical spell */
		if (tactic_num) return (tactic[rand_int(tactic_num)]);
	}

	/* We're hurt (not badly), try to heal.  Chance reduced in Oangband
	 * to help avoid endless fights.
	 */
	if ((m_ptr->hp < m_ptr->maxhp * 3 / 4) && (rand_int(100) < 25))
	{
		/* Choose heal spell if possible */
		if (heal_num) return (heal[rand_int(heal_num)]);
	}

	/* Haste self if we aren't already greatly hasted.  Tweaked in 
	 * Oangband.
	 */
	if (haste_num && (rand_int(100) < 60))
	{
		/* Choose haste spell.  Higher priority if we aren't already hasted. */
		if ((20 + r_ptr->speed - m_ptr->mspeed) > rand_int(20)) 
			return (haste[rand_int(haste_num)]);
	}


	/* Use number of summon spells to help determine summon chance. -LM- */
	summon_chance = 20 + 10 * summon_num;
	if (summon_chance > 50) summon_chance = 50;

	/* Summon if possible (more often if has many summon spells). -LM- */
	if (summon_num && (rand_int(100) < summon_chance))
	{
		/* Choose summon spell */
		return (summon[rand_int(summon_num)]);
	}


	/* Use number of attack spells to help determine attack chance. -LM- */
	attack_chance = 30 + 10 * attack_num;
	if (attack_chance > 70) attack_chance = 70;

	/* Attack spell (more often if has many attack spells). -LM- */
	if (attack_num && (rand_int(100) < attack_chance))
	{
		/* Choose attack spell */
		return (attack[rand_int(attack_num)]);
	}


	/* Try another tactical spell (sometimes) */
	if (tactic_num && (rand_int(100) < 40))
	{
		/* Choose tactic spell */
		return (tactic[rand_int(tactic_num)]);
	}


	/* Use number of annoyance spells to help determine annoyance chance. -LM- */
	annoy_chance = 50 + 10 * annoy_num;
	if (annoy_chance > 90) annoy_chance = 90;

	/* Annoy player (most of the time, if no other spells take priority.) */
	if (annoy_num && (rand_int(100) < annoy_chance))
	{
		/* Choose annoyance spell */
		return (annoy[rand_int(annoy_num)]);
	}


	/* Line added in Oangband to make certain that monsters get the spells
	 * that they deserve, while never going into an infinite loop.
	 */
	if (rounds < 5)
	{
		rounds++;
		goto try_again;
	}

	/* Choose no spell.  This should seldom be activated. -LM- */
	return (0);
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
bool make_attack_spell(int m_idx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int k, chance, thrown_spell, rlev, failrate;

	byte spell[96], num = 0;

	u32b f4, f5, f6;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	bool no_inate = FALSE;

	/* Target player */
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
	if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);

	/* Hack -- Extract the spell probability */
	chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance) return (FALSE);

	/* Only do spells occasionally */
	if (rand_int(100) >= 2 * chance) return (FALSE);

	/* Sometimes forbid inate attacks (breaths) */
	if (rand_int(70) >= (chance + 10)) no_inate = TRUE;

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

	/* Forbid inate attacks sometimes */
	if (no_inate) f4 = 0L;

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

#endif

	/* Check for a clean bolt shot. */
	if (!(r_ptr->flags2 & (RF2_STUPID)) && 
		!clean_shot(m_ptr->fy, m_ptr->fx, py, px))
	{
		/* Remove spells that will only hurt friends */
		f4 &= ~(RF4_BOLT_MASK);
		f5 &= ~(RF5_BOLT_MASK);
		f6 &= ~(RF6_BOLT_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

	/* Extract the "inate" spells */
	for (k = 0; k < 32; k++)
	{
		if (f4 & (1L << k)) spell[num++] = k + 32 * 3;
	}

	/* Extract the "normal" spells */
	for (k = 0; k < 32; k++)
	{
		if (f5 & (1L << k)) spell[num++] = k + 32 * 4;
	}

	/* Extract the "bizarre" spells */
	for (k = 0; k < 32; k++)
	{
		if (f6 & (1L << k)) spell[num++] = k + 32 * 5;
	}

	/* No spells left */
	if (!num) return (FALSE);

	/* Handle "leaving" */
	if (p_ptr->leaving) return (FALSE);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, 0x88);

	/* Choose a spell to cast */
	thrown_spell = choose_attack_spell(m_idx, spell, num);

	/* Abort if no spell was chosen */
	if (!thrown_spell) return (FALSE);

	/* Calculate spell failure rate.  Lowered in Oangband <<cackle>> - but 
	 * stunned monsters are penalized.
	 */
	failrate = 25 - (rlev) / 5 + (m_ptr->stunned ? 20 : 0);

	/* Paranoia. */
	if (failrate < 0) failrate = 0;
 
	/* Hack -- Stupid monsters will never fail (for jellies and such) */
	if (r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

	/* Check for spell failure (inate attacks never fail) */
	if ((thrown_spell >= 128) && (rand_int(100) < failrate))
	{
		/* Message.  Currently not activated. */
		/* msg_format("%^s tries to cast a spell, but fails.", m_name); */

		return (TRUE);
	}

	/* Cast the spell. Breaths upgraded to compensate for lower monster HPs.  
	 * Boulders, arrows, and missiles sometimes miss (or hit to do no damage). 
	 * -LM-
	 */
	switch (thrown_spell)
	{
		/* RF4_SHRIEK */
		case 96+0:
		{
			if (!direct) break;
			disturb(1, 0);
			if (r_ptr->flags2 & (RF2_SMART))
				msg_format("%^s shouts for help.", m_name);
			else
				msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx, FALSE);
			break;
		}

		/* RF4_XXX2X4 */
		case 96+1:
		{
			break;
		}

		/* RF4_BOULDER */
		case 96+2:
		{
			disturb(1, 0);
			if (blind) msg_format("You hear something grunt with exertion.", m_name);
			else msg_format("%^s hurls a boulder at you!", m_name);
			bolt(m_idx, GF_ARROW, damroll(r_ptr->level / 7, 12));
			break;
		}

		/* RF4_ARROW_5 */
		case 96+3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a loud thwang.", m_name);
			else msg_format("%^s fires a seeker arrow!", m_name);
			bolt(m_idx, GF_ARROW, damroll(8, 10));
			break;
		}

		/* RF4_ARROW_1 */
		case 96+4:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a soft twang.", m_name);
			else msg_format("%^s fires a small arrow.", m_name);
			bolt(m_idx, GF_ARROW, damroll(2, 8));
			break;
		}

		/* RF4_ARROW_2 */
		case 96+5:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a twang.", m_name);
			else msg_format("%^s fires an arrow.", m_name);
			bolt(m_idx, GF_ARROW, damroll(4, 10));
			break;
		}

		/* RF4_ARROW_3 */
		case 96+6:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a 'twung' sound.", m_name);
			else msg_format("%^s fires a missile.", m_name);
			bolt(m_idx, GF_ARROW, damroll(5, 9));
			break;
		}

		/* RF4_ARROW_4 */
		case 96+7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a loud 'twung' sound.", m_name);
			else msg_format("%^s fires a missile!", m_name);
			bolt(m_idx, GF_ARROW, damroll(8, 9));
			break;
		}

		/* RF4_BR_ACID */
		case 96+8:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes acid.", m_name);
			breath(m_idx, GF_ACID,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* RF4_BR_ELEC */
		case 96+9:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes lightning.", m_name);
			breath(m_idx, GF_ELEC,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* RF4_BR_FIRE */
		case 96+10:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes fire.", m_name);
			breath(m_idx, GF_FIRE,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* RF4_BR_COLD */
		case 96+11:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes frost.", m_name);
			breath(m_idx, GF_COLD,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* RF4_BR_POIS */
		case 96+12:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gas.", m_name);
			breath(m_idx, GF_POIS,
			       ((2 * m_ptr->hp / 5) > 800 ? 800 : (2 * m_ptr->hp / 5)));
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

		/* RF4_BR_NETH */
		case 96+13:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nether.", m_name);
			breath(m_idx, GF_NETHER,
			       ((m_ptr->hp / 4) > 550 ? 550 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* RF4_BR_LITE */
		case 96+14:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes light.", m_name);
			breath(m_idx, GF_LITE,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_LITE);
			break;
		}

		/* RF4_BR_DARK */
		case 96+15:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes darkness.", m_name);
			breath(m_idx, GF_DARK,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_DARK);
			break;
		}

		/* RF4_BR_CONF */
		case 96+16:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes confusion.", m_name);
			breath(m_idx, GF_CONFUSION,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_CONFU);
			break;
		}

		/* RF4_BR_SOUN */
		case 96+17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes sound.", m_name);
			breath(m_idx, GF_SOUND,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_SOUND);
			break;
		}

		/* RF4_BR_CHAO.  Upgraded in Oangband. */
		case 96+18:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes chaos.", m_name);
			breath(m_idx, GF_CHAOS,
			       ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_CHAOS);
			break;
		}

		/* RF4_BR_DISE */
		case 96+19:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes disenchantment.", m_name);
			breath(m_idx, GF_DISENCHANT,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_DISEN);
			break;
		}

		/* RF4_BR_NEXU */
		case 96+20:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nexus.", m_name);
			breath(m_idx, GF_NEXUS,
			       ((m_ptr->hp / 4) > 300 ? 300 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_NEXUS);
			break;
		}

		/* RF4_BR_TIME */
		case 96+21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes time.", m_name);
			breath(m_idx, GF_TIME,
			       ((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)));
			break;
		}

		/* RF4_BR_INER */
		case 96+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes inertia.", m_name);
			breath(m_idx, GF_INERTIA,
			       ((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)));
			break;
		}

		/* RF4_BR_GRAV */
		case 96+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gravity.", m_name);
			breath(m_idx, GF_GRAVITY,
			       ((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)));
			break;
		}

		/* RF4_BR_SHAR */
		case 96+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes shards.", m_name);
			breath(m_idx, GF_SHARD,
			       ((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)));
			update_smart_learn(m_idx, DRS_RES_SHARD);
			break;
		}

		/* RF4_BR_PLAS */
		case 96+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes plasma.", m_name);
			breath(m_idx, GF_PLASMA,
			       ((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)));
			break;
		}

		/* RF4_BR_WALL */
		case 96+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes force.", m_name);
			breath(m_idx, GF_FORCE,
			       ((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)));
			break;
		}

		/* RF4_BR_MANA */
		case 96+27:
		{
			/* XXX XXX XXX */
			break;
		}

		/* RF4_XXX5X4 */
		case 96+28:
		{
			break;
		}

		/* RF4_XXX6X4 */
		case 96+29:
		{
			break;
		}

		/* RF4_XXX7X4 */
		case 96+30:
		{
			break;
		}

		/* RF4_XXX8X4 */
		case 96+31:
		{
			break;
		}

		/* RF5_BA_ACID */
		case 128+0:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts an acid ball.", m_name);
			breath(m_idx, GF_ACID,
			       randint(rlev * 3) + 15);
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* RF5_BA_ELEC */
		case 128+1:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a lightning ball.", m_name);
			breath(m_idx, GF_ELEC,
			       randint(rlev * 2) + 8);
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* RF5_BA_FIRE */
		case 128+2:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a fire ball.", m_name);
			breath(m_idx, GF_FIRE,
			       randint(rlev * 3) + 10);
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* RF5_BA_COLD */
		case 128+3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a frost ball.", m_name);
			breath(m_idx, GF_COLD,
			       randint(rlev * 2) + 10);
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}


		/* RF5_BA_POIS */
		case 128+4:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a stinking cloud.", m_name);
			breath(m_idx, GF_POIS,
			       damroll(12, 2));
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

		/* RF5_BA_NETH */
		case 128+5:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a nether ball.", m_name);
			breath(m_idx, GF_NETHER,
			       (50 + damroll(10, 10) + rlev));
			update_smart_learn(m_idx, DRS_RES_NETHR);
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
			       randint(rlev * 5 / 2) + 50);
			break;
		}

		/* RF5_BA_MANA */
		case 128+7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
			else msg_format("%^s invokes a mana storm.", m_name);
			breath(m_idx, GF_MANA,
			       (rlev * 4) + damroll(10, 10));
			break;
		}

		/* RF5_BA_DARK */
		case 128+8:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
			else msg_format("%^s invokes a darkness storm.", m_name);
			breath(m_idx, GF_DARK,
			       (rlev * 5) + damroll(10, 10));
			update_smart_learn(m_idx, DRS_RES_DARK);
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
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

				/* Heal the monster */
				if (m_ptr->hp < m_ptr->maxhp)
				{
					/* Heal */
					m_ptr->hp += (3 * r1) + 10;
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
				if (!p_ptr->resist_confu)
				{
					(void)set_confused(p_ptr->confused + rand_int(4) + 4);
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
				if (!p_ptr->resist_confu)
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
			bolt(m_idx, GF_ACID,
			     damroll(7, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* RF5_BO_ELEC */
		case 128+17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a lightning bolt.", m_name);
			bolt(m_idx, GF_ELEC,
			     damroll(4, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* RF5_BO_FIRE */
		case 128+18:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a fire bolt.", m_name);
			bolt(m_idx, GF_FIRE,
			     damroll(9, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* RF5_BO_COLD */
		case 128+19:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a frost bolt.", m_name);
			bolt(m_idx, GF_COLD,
			     damroll(6, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_COLD);
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
			bolt(m_idx, GF_NETHER,
			     30 + damroll(5, 5) + (rlev * 3) / 2);
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* RF5_BO_WATE */
		case 128+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a water bolt.", m_name);
			bolt(m_idx, GF_WATER,
			     damroll(10, 10) + (rlev));
			break;
		}

		/* RF5_BO_MANA */
		case 128+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a mana bolt.", m_name);
			bolt(m_idx, GF_MANA,
			     randint(rlev * 7 / 2) + 50);
			break;
		}

		/* RF5_BO_PLAS */
		case 128+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a plasma bolt.", m_name);
			bolt(m_idx, GF_PLASMA,
			     10 + damroll(8, 7) + (rlev));
			break;
		}

		/* RF5_BO_ICEE */
		case 128+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts an ice bolt.", m_name);
			bolt(m_idx, GF_ICE,
			     damroll(6, 6) + (rlev));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* RF5_MISSILE */
		case 128+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a magic missile.", m_name);
			bolt(m_idx, GF_MISSILE,
			     damroll(2, 6) + (rlev / 3));
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
				(void)set_afraid(p_ptr->afraid + rand_int(3) + 3);
			}
			update_smart_learn(m_idx, DRS_RES_FEAR);
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
			update_smart_learn(m_idx, DRS_RES_BLIND);
			break;
		}

		/* RF5_CONF */
		case 128+29:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else msg_format("%^s creates a mesmerising illusion.", m_name);
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
				(void)set_confused(p_ptr->confused + rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_RES_CONFU);
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
				(void)set_slow(p_ptr->slow + rand_int(6) + 6);
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

			/* Allow a quick speed increase to base+10 */
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

		/* RF6_XXX1X6 */
		case 160+1:
		{
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
			m_ptr->hp += (rlev * 4);

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

		/* RF6_XXX2X6 */
		case 160+3:
		{
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
			update_smart_learn(m_idx, DRS_RES_NEXUS);
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

		/* RF6_XXX6X6 */
		case 160+15:
		{
			break;
		}

		/* RF6_XXX7X6 */
		case 160+16:
		{
			break;
		}

		/* RF6_XXX8X6 */
		case 160+17:
		{
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
				count += summon_specific(y, x, FALSE, rlev, 0);
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
				count += summon_specific(y, x, FALSE, rlev, 0);
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_ANT);
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_SPIDER);
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_HOUND);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF6_XXX9X6 */
		case 160+23:
		{
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_ANGEL);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF6_S_DEMON.  Demons are better at summoning other demons. -LM- */
		case 160+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);

			if ((r_ptr->flags3 & (RF3_DEMON)) && (randint(rlev) > 40))
			{
				if (!(blind)) msg_format
					("%^s magically summons hellish adversaries!", m_name);
				for (k = 0; k < 1 + randint(2); k++)
				{
					count += summon_specific(y, x, FALSE, rlev, SUMMON_DEMON);
				}
				if (blind && count) msg_print
					("You hear several things appear nearby.");
			}

			else
			{
				if (!(blind)) msg_format
					("%^s magically summons a hellish adversary!", m_name);
				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, FALSE, rlev, SUMMON_DEMON);
				}
				if (blind && count) msg_print
					("You hear something appear nearby.");
			}
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_UNDEAD);
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_DRAGON);
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_HI_UNDEAD);
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_HI_DRAGON);
			}
			if (blind && count)
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}

		/* RF6_S_WRAITH */
		case 160+30:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons mighty undead opponents!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_WRAITH);
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
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
				count += summon_specific(y, x, FALSE, rlev, SUMMON_UNIQUE);
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, FALSE, rlev, SUMMON_HI_UNDEAD);
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
		if (thrown_spell < 32*4)
		{
			r_ptr->r_flags4 |= (1L << (thrown_spell - 32*3));
			if (r_ptr->r_cast_inate < MAX_UCHAR) r_ptr->r_cast_inate++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32*5)
		{
			r_ptr->r_flags5 |= (1L << (thrown_spell - 32*4));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32*6)
		{
			r_ptr->r_flags6 |= (1L << (thrown_spell - 32*5));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}
	}


	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
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
	monster_type *m_ptr = &m_list[m_idx];

#ifdef ALLOW_TERROR

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u16b p_lev, m_lev;
	u16b p_chp, p_mhp;
	u16b m_chp, m_mhp;
	u32b p_val, m_val;

#endif

	/* Keep monsters from running too far away */
	if (m_ptr->cdis > (p_ptr->themed_level ? 
		MAX_SIGHT / 2 : MAX_SIGHT) + 5) return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);

#ifdef ALLOW_TERROR

	/* Nearby monsters that cannot run away will not become terrified */
	if ((m_ptr->cdis < 4) && (m_ptr->mspeed < p_ptr->pspeed)) return (FALSE);

	/* Examine player power (level) */
	p_lev = p_ptr->lev;

	/* Examine monster power (level plus morale) */
	m_lev = r_ptr->level + (m_idx & 0x08) + 25;

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4) return (FALSE);
	if (m_lev + 3 < p_lev) return (TRUE);

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

#endif

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, y1, x1;

	int when = 0;
	int cost = 999;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster flowing disabled */
	if (!flow_by_sound) return (FALSE);

	/* Monster can go through rocks */
	if (r_ptr->flags2 & (RF2_PASS_WALL)) return (FALSE);
	if (r_ptr->flags2 & (RF2_KILL_WALL)) return (FALSE);

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* The player is not currently near the monster grid */
	if (cave_when[y1][x1] < cave_when[py][px])
	{
		/* The player has never been near the monster grid */
		if (cave_when[y1][x1] == 0) return (FALSE);

		/* The monster is not allowed to track the player */
		if (!flow_by_smell) return (FALSE);
	}

	/* Monster is too far away to notice the player. */
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
		(*yp) = py + 16 * ddy_ddd[i];
		(*xp) = px + 16 * ddx_ddd[i];
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

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster flowing disabled */
	if (!flow_by_sound) return (FALSE);

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
	 * on themed levels.
	 */
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


#endif

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
	monster_type *m_ptr = &m_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, d, dis;
	int gy = 0, gx = 0, gdis = 0;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Check nearby locations */
		for (y = fy - d; y <= fy + d; y++)
		{
			for (x = fx - d; x <= fx + d; x++)
			{
				/* Skip illegal locations */
 				if (!in_bounds_fully(y, x)) continue;

				/* Skip locations in a wall */
				if (!cave_passable_bold(y, x)) continue;

				/* Check distance */
				if (distance(y, x, fy, fx) != d) continue;

				/* Check for "availability" (if monsters can flow) */
				if (flow_by_sound)
				{
					/* Ignore grids very far from the player */
					if (cave_when[y][x] < cave_when[py][px]) continue;

					/* Ignore too-distant grids */
					if (cave_cost[y][x] > cave_cost[fy][fx] + 2 * d) continue;
				}

				/* Check for absence of shot */
				if (!projectable(y, x, py, px))
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

	int y, x, d, dis;
	int gy = 0, gx = 0, gdis = 999, min;

	/* Closest distance to get */
	min = distance(py, px, fy, fx) * 3 / 4 + 2;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Check nearby locations */
		for (y = fy - d; y <= fy + d; y++)
		{
			for (x = fx - d; x <= fx + d; x++)
			{
				/* Skip illegal locations */
				if (!in_bounds_fully(y, x)) continue;

				/* Skip locations in a wall */
				if (!cave_floor_bold(y, x)) continue;

				/* Check distance */
				if (distance(y, x, fy, fx) != d) continue;

				/* Check for hidden, available grid */
				if (!player_can_see_bold(y, x) && clean_shot(fy, fx, y, x))
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

	bool done = FALSE;
	bool fear;

#ifdef MONSTER_FLOW
	/* Flow towards the player */
	if (flow_by_sound)
	{
		/* Flow towards the player */
		(void)get_moves_aux(m_idx, &y2, &x2);
	}
#endif

	/* Extract the "pseudo-direction" */
	y = m_ptr->fy - y2;
	x = m_ptr->fx - x2;

	/* Is the monster frightened? */
	fear = mon_will_run(m_idx);


	/* Animal packs try to lure the player into the open. */
	if ((r_ptr->flags1 & RF1_FRIENDS) && (r_ptr->flags3 & RF3_ANIMAL) && !fear)
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

		/* Not in the open and strong player.  Tweaked in Oangband. */
		if ((passable < 5) && (p_ptr->chp > p_ptr->mhp / 2) && 
			(p_ptr->csp + 1 > (p_ptr->msp + 3) / 4))
		{
			/* Find hiding place */
			if (find_hiding(m_idx, &y, &x)) done = TRUE;
		}
	}

	/* Monster groups often try to surround the player, unless frightened. */
	if ((r_ptr->flags1 & RF1_FRIENDS) && !done && !fear && (randint(2) == 1))
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
			/* This is not a very "smart" method XXX XXX */
			y = (-y);
			x = (-x);
		}
			else
		{
			/* Attempt to avoid the player */
			if (flow_by_sound)
			{
				/* Adjust movement */
				(void)get_fear_moves_aux(m_idx, &y, &x);
			}
		}
	}


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
static int compare_monsters(monster_type *m_ptr, monster_type *n_ptr)
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

	int i, d, oy, ox, ny, nx;
	int chance = 0;

	int mm[5];

	/* Assume the monster is able to perceive the player. -LM- */
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


	/* Players hidden in shadow are almost imperceptable. -LM- */
	if ((p_ptr->superstealth) && (!p_ptr->aggravate))
	{
		add_wakeup_chance = 0;

		/* Monsters almost never wake up. */
		if (total_wakeup_chance > 100) total_wakeup_chance = 100;

		/* Low-level monsters will find it difficult to locate the player. */
		if (rand_int(p_ptr->lev * 2 - r_ptr->level) != 0) aware = FALSE;
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
			m_ptr->csleep = 0;

			/* Notice the "waking up" */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s wakes up.", m_name);
			}

			/* Efficiency XXX XXX */
			return;
		}

		/* Get the origin */
		oy = m_ptr->fy;
		ox = m_ptr->fx;

		/* Chance for extra noise to wake up monsters in LOS. -LM- */
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
						msg_format("%^s wakes up.", m_name);
				}
			}
		}


		/* Hack -- See if monster "notices" player.  Now a percentage chance. -LM- */
		if (total_wakeup_chance > rand_int(10000))
		{
			int d = 1;

			/* Wake up faster near the player */
			if (m_ptr->cdis < 50) d = (100 / (m_ptr->cdis + 1));

			/* Still asleep */
			if (m_ptr->csleep > d)
			{
				/* Monster wakes up "a little bit" */
				m_ptr->csleep -= d;

				/* Notice the "not waking up" */
				if (m_ptr->ml)
				{
					/* Hack -- Count the ignores */
					if (r_ptr->r_ignore < MAX_UCHAR)
					{
						r_ptr->r_ignore++;
					}
				}
			}

			/* Just woke up */
			else
			{
				/* Reset sleep counter */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);

					/* Hack -- Count the wakings */
					if (r_ptr->r_wake < MAX_UCHAR)
					{
						r_ptr->r_wake++;
					}
				}
			}
		}

		/* Still sleeping */
		if (m_ptr->csleep) return;
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
			}
		}
	}

	/* Handle stasis.  Fast monsters get no bonus (unfortunately, slow 
	 * one are penalized). -LM-
	 */
	if ((m_ptr->stasis) && 
		(rand_int(extract_energy[m_ptr->mspeed]) < 10))
	{
		int d = 1;

		/* Sudden emergence.  Uniques get a bonus. */
		if (50 + r_ptr->level + (r_ptr->flags1 & (RF1_UNIQUE) ? 
			r_ptr->level / 2 : 0) > randint(800)) m_ptr->stasis = 0;

		/* Gradual emergence. */
		else if (m_ptr->stasis > d) m_ptr->stasis -= d;
		else m_ptr->stasis = 0;

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
	if (m_ptr->stasis) return;


	/* Handle significantly hasted or slowed creatures.  Random variations 
	 * are not large enough to activate this code. -LM-
	 */
	if ((m_ptr->mspeed > r_ptr->speed + 4) || (m_ptr->mspeed < r_ptr->speed - 4))
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
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

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
					r_ptr->r_flags2 |= (RF2_MULTIPLY);
				}

				/* Multiplying takes energy */
				return;
			}
		}
	}

	/* This code is taken from Zangband, with some simplifications. -LM- */
	if ((r_ptr->flags2 & (RF2_SPEAKING)) && (rand_int(SPEAK_CHANCE) == 0) && 
		(!m_ptr->monfear))
	{
		/* Certain monsters in LOS can speak. */
		if (player_has_los_bold(oy, ox))
		{
			char m_name[80];
			char bravado[80];

			/* Acquire the monster name/poss */
			if (m_ptr->ml) monster_desc(m_name, m_ptr, 0);

			/* Default name */
			else strcpy(m_name, "It");

			get_rnd_line("bravado.txt", bravado);
			msg_format("%^s %s", m_name, bravado);
		}
	}

	/* Player ghosts may have a unique message they can say.  They will only 
	 * say it once (unless reloaded). -LM-
	 */
	else if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && 
		(player_has_los_bold(oy, ox)) && (ghost_string_type == 1) && 
		(ghost_has_spoken == FALSE) 
		&& (randint(3) == 1))
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
	if (aware && make_attack_spell(m_idx)) return;


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
				if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_25);

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
				if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_50);

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
				if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_50 | RF1_RAND_25);

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


	/* Process all four possible moves */
	for (i = 0; i < 5; i++)
	{
		/* Try again, if monster cannot deal with a feature it has encountered. -LM- */
		I_cannot_handle_feature:

		/* Get the direction (or stagger) */
		d = (stagger || stagger_temp ? ddd[rand_int(8)] : mm[i]);

		/* Clear a temporary stagger, and movement permission. -LM- */
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
					msg_print("You hear a door burst open!");

					/* Disturb (sometimes) */
					if (disturb_minor) disturb(0, 0);

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
				if (player_has_los_bold(ny, nx)) do_view = TRUE;
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
			/* Hack -- memorize lack of attacks after a while */
			if ((m_ptr->ml) && (randint(20) == 1))
				r_ptr->r_flags1 |= (RF1_NEVER_BLOW);

			/* Do not move */
			do_move = FALSE;
		}


		/* The player is in the way.  Attack him. */
		if (do_move && (cave_m_idx[ny][nx] < 0))
		{
			/* Do the attack */
			(void)make_attack_normal(m_idx, ny, nx);

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
				r_ptr->r_flags1 |= (RF1_NEVER_MOVE);

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
						(r_ptr->flags2 & (RF2_KILL_WALL))) break;

					if (r_ptr->flags2 & (RF2_SMART))
					{
						if (randint(2) == 1) do_move = FALSE;
					}
					else
					{
						if (randint(3) != 1) do_move = FALSE;
					}

					break;
				}
				/* Trees aren't easy either, but they can be passed 
				 * through, flown over, and animals can cross them 
				 * more readily. */
				case FEAT_TREE:
				{
					if (r_ptr->flags2 & (RF2_PASS_WALL)) break;

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
					if (r_ptr->flags3 & (RF3_IM_FIRE)) break;

					else if ((r_ptr->flags2 & (RF2_FLYING)) && 
						(m_ptr->hp > 49)) break;

					else if (randint(10) != 1)
					{
						stagger_temp = TRUE;
						goto I_cannot_handle_feature;
					}
					else do_move = FALSE;
					break;
				}
				/* Non-flying fire breathers and demons are stopped by 
				 * water.  They will normally look for other options.
				 */
				case FEAT_WATER:
				{
					if (((!(strchr("uU", r_ptr->d_char))) && 
						(!(r_ptr->flags4 & (RF4_BR_FIRE)))) || 
						(r_ptr->flags2 & (RF2_FLYING))) break;

					else if (randint(10) != 1)
					{
						stagger_temp = TRUE;
						goto I_cannot_handle_feature;
					}
					else do_move = FALSE;
					break;
				}

				/* Rogues may set traps for monsters.  They can be fairly 
				 * deadly, but monsters can also sometimes disarm or fly 
				 * over them. -LM-
				 */
				case FEAT_MONSTER_TRAP:
				{
					if ((r_ptr->flags2 & (RF2_SMART)) && (randint(3) == 1))
					{
						if (m_ptr->ml)
						{
							char m_name[80];

							/* Acquire the monster name/poss */
							monster_desc(m_name, m_ptr, 0);

							msg_format("%s finds your trap and disarms it.", m_name);
						}

						/* Kill the trap, decrement the monster trap count. */
						cave_feat[ny][nx] = FEAT_FLOOR;
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
							if (m_ptr->ml) monster_desc(m_name, m_ptr, 0);
							/* Default name */
							else strcpy(m_name, "Something");

							msg_format("%s sets off your cunning trap!", m_name);
						}

						else msg_print("You hear anguished yells in the distance.");

						/* Sometimes, the trap is destroyed. */
						if (randint(3) == 1)
						{
							cave_feat[ny][nx] = FEAT_FLOOR;
							num_trap_on_level--;
						}

						/* Hurt the monster.  Big bruisers fall hard. */
						mon_take_hit(cave_m_idx[oy][ox], 
							(1 + randint(p_ptr->lev * 3) + m_ptr->maxhp / 20), &fear, note_dies);
					}

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

			/* Assume no movement */
			do_move = FALSE;

			/* Kill weaker monsters */
			if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
			    (compare_monsters(m_ptr, n_ptr) > 0))
			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster ate another monster */
				did_kill_body = TRUE;

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

			/* Possible disturb */
			if (m_ptr->ml &&
			    (disturb_move ||
			     ((m_ptr->mflag & (MFLAG_VIEW)) &&
			      disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}


			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[ny][nx]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Acquire object */
				o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Skip gold */
				if (o_ptr->tval == TV_GOLD) continue;

				/* Take or Kill objects on the floor */
				if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
				    (r_ptr->flags2 & (RF2_KILL_ITEM)))
				{
					u32b f1, f2, f3;

					u32b flg3 = 0L;

					char m_name[80];
					char o_name[80];

					/* Extract some flags */
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Acquire the object name */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0x04);

					/* React to objects that hurt the monster */
					if (f1 & (TR1_SLAY_DRAGON)) flg3 |= (RF3_DRAGON);
					if (f1 & (TR1_SLAY_TROLL)) flg3 |= (RF3_TROLL);
					if (f1 & (TR1_SLAY_GIANT)) flg3 |= (RF3_GIANT);
					if (f1 & (TR1_SLAY_ORC)) flg3 |= (RF3_ORC);
					if (f1 & (TR1_SLAY_DEMON)) flg3 |= (RF3_DEMON);
					if (f1 & (TR1_SLAY_UNDEAD)) flg3 |= (RF3_UNDEAD);
					if (f1 & (TR1_SLAY_ANIMAL)) flg3 |= (RF3_ANIMAL);
					if (f1 & (TR1_SLAY_EVIL)) flg3 |= (RF3_EVIL);

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
						if (player_has_los_bold(ny, nx))
						{
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
							msg_format("%^s crushes %s.", m_name, o_name);
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
	if (!do_turn && !do_move && !m_ptr->monfear && aware)
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
		if (did_open_door) r_ptr->r_flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) r_ptr->r_flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) r_ptr->r_flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) r_ptr->r_flags2 |= (RF2_KILL_ITEM);

		/* Monster pushed past another monster */
		if (did_move_body) r_ptr->r_flags2 |= (RF2_MOVE_BODY);

		/* Monster ate another monster */
		if (did_kill_body) r_ptr->r_flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) r_ptr->r_flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall) r_ptr->r_flags2 |= (RF2_KILL_WALL);
	}


	/* Hack -- get "bold" if out of options */
	if (!do_turn && !do_move && m_ptr->monfear && aware)
	{
		/* No longer afraid */
		m_ptr->monfear = 0;

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
 */
void process_monsters(void)
{
	int i, e;
	int fy, fx, total_wakeup_chance;

	monster_type *m_ptr;
	monster_race *r_ptr;


	/* The likelyhood, in 1/100ths of a percent, that any monster
	 * will be disturbed. -LM-
	 */
	total_wakeup_chance = p_ptr->base_wakeup_chance + add_wakeup_chance;

	/* People don't make much noise when resting. */
	if (p_ptr->resting) total_wakeup_chance /= 2;


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


	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Access the monster */
		m_ptr = &m_list[i];


		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;


		/* Ignore "born" monsters XXX XXX */
		if (m_ptr->mflag & (MFLAG_BORN)) continue;


		/* Obtain the energy boost */
		e = extract_energy[m_ptr->mspeed];

		/* Give this monster some energy */
		m_ptr->energy += e;


		/* Not enough energy to move */
		if (m_ptr->energy < 100) continue;

		/* Use up "some" energy */
		m_ptr->energy -= 100;


		/* Heal monster? XXX XXX XXX */


		/* Access the race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Monsters can "sense" the player.  Sighting impaired on themed 
		 * levels.
		 */
		if (m_ptr->cdis <= (p_ptr->themed_level ? r_ptr->aaf / 2 : r_ptr->aaf))
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
			/* Hack -- Reduced sighting on themed levels. -LM- */
			if ((p_ptr->themed_level) && (m_ptr->cdis > MAX_SIGHT / 2))
				continue;

			/* Process the monster */
			process_monster(i, total_wakeup_chance);

			/* Continue */
			continue;
		}

#ifdef MONSTER_FLOW

		/* Hack -- Monsters can "smell" the player from far away */
		if (flow_by_sound)
		{
			int py = p_ptr->py;
			int px = p_ptr->px;

			/* Check the flow (normal aaf is about 20) */
			if ((cave_when[fy][fx] == cave_when[py][px]) &&
				(cave_cost[fy][fx] < MONSTER_FLOW_DEPTH) &&
				(cave_cost[fy][fx] < (p_ptr->themed_level ? r_ptr->aaf / 2 : r_ptr->aaf)))
			{
				/* Process the monster */
				process_monster(i, total_wakeup_chance);

				/* Continue */
				continue;
			}
		}
#endif
	}
}



