/* File: melee2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "melee.h"
#include "flow.h"
#include "keypad.h"

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
	monster_race *r_ptr = m_ptr->race();

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
		if (m_ptr->smart && one_in_(100)) m_ptr->smart = 0L;

		/* Use the memorized flags */
		smart = m_ptr->smart;
	}


	/* Cheat if requested */
	if (smart_cheat)
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


	/* XXX XXX XXX No spells left? */
	/* if (!f4 && !f5 && !f6) ... */


	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
}


/*
 * Determine if there is a space near the selected spot in which
 * a summoned creature can appear
 */
static bool summon_possible(coord g)
{
	coord_scan t;

	/* Start at the location, and check 2 grids in each dir */
	for (t.y = g.y - 2; t.y <= g.y + 2; t.y++)
	{
		for (t.x = g.x - 2; t.x <= g.x + 2; t.x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(t.y, t.x)) continue;

			/* Only check a circular area */
			if (distance(g.y, g.x, t.y, t.x) > 2) continue;

			/* Hack: no summon on glyph of warding */
			if (cave_feat[t.y][t.x] == FEAT_GLYPH) continue;

			/* Require empty floor grid in line of sight */
			if (cave_empty_bold(t.y, t.x) && los(g, t))
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
static bool clean_shot(coord g1, coord g2)
{
	coord grid_g[MAX(DUNGEON_HGT,DUNGEON_WID)];
	int grid_n = project_path(grid_g, MAX_RANGE, g1, g2, true, &wall_mon_stop);	/* Check the projection path */

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* May not end in an unrequested grid */
	if (grid_g[grid_n-1]!=g2) return (FALSE);

	/* May not end in a wall grid */
	if (!cave_floor_bold(g2.y, g2.x)) return (FALSE);

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
	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, p_ptr->loc, dam_hp, typ, PROJECT_STOP | PROJECT_KILL);
}


/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void breath(int m_idx, int typ, int dam_hp)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();
	int rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;	/* Determine the radius of the blast */

	/* Target the player with a ball attack */
	(void)project(m_idx, rad, p_ptr->loc, dam_hp, typ, PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL);
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
	monster_race *r_ptr = m_ptr->race();

	u32b f4_mask = 0L;
	u32b f5_mask = 0L;
	u32b f6_mask = 0L;

	int num = 0;
	byte spells[96];

	int i;
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	bool has_escape, has_attack, has_summon, has_tactic;
	bool has_annoy, has_haste, has_heal;


	/* Smart monsters restrict their spell choices. */
	if (smart_monsters && !(r_ptr->flags2 & (RF2_STUPID)))
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
		has_haste = ((f4 & (RF4_HASTE_MASK)) ||
		             (f5 & (RF5_HASTE_MASK)) ||
		             (f6 & (RF6_HASTE_MASK)));
		has_heal = ((f4 & (RF4_HEAL_MASK)) ||
		            (f5 & (RF5_HEAL_MASK)) ||
		            (f6 & (RF6_HEAL_MASK)));

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
		else if (has_tactic && (distance(py, px, m_ptr->loc.y, m_ptr->loc.x) < 4) &&
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
	int k, chance, thrown_spell, rlev;

	int failrate;

	u32b f4, f5, f6;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();
	monster_lore *l_ptr = m_ptr->lore();

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	int count = 0;				/* Summon count */
	bool blind = p_ptr->timed[TMD_BLIND] ;	/* Extract the blind-ness */
	bool seen = (!blind && m_ptr->ml);	/* Extract the "see-able-ness" */
	bool direct = TRUE;			/* Assume "projectable" */

	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	/* Cannot cast spells when nice */
	if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);

	/* Hack -- Extract the spell probability */
	chance = (r_ptr->freq_innate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance) return (FALSE);


	/* Only do spells occasionally */
	if (rand_int(100) >= chance) return (FALSE);


	/* Hack -- require projectable player */
	/* Check range */
	if (m_ptr->cdis > MAX_RANGE) return (FALSE);

	/* Check path */
	if (!projectable(m_ptr->loc, p_ptr->loc)) return (FALSE);


	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;



	/* Hack -- allow "desperate" spells */
	if ((r_ptr->flags2 & (RF2_SMART)) &&
	    (m_ptr->hp < m_ptr->maxhp / 10) &&
	    one_in_(2))
	{
		/* Require intelligent spells */
		f4 &= (RF4_INT_MASK);
		f5 &= (RF5_INT_MASK);
		f6 &= (RF6_INT_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}


	/* Remove the "ineffective" spells */
	remove_bad_spells(m_idx, &f4, &f5, &f6);

	/* No spells left */
	if (!f4 && !f5 && !f6) return (FALSE);

	/* Check whether summons and bolts are worth it. */
	if (!(r_ptr->flags2 & (RF2_STUPID)))
	{
		/* Check for a clean bolt shot */
		if ((f4 & (RF4_BOLT_MASK) ||
			 f5 & (RF5_BOLT_MASK) ||
			 f6 & (RF6_BOLT_MASK)) &&
			!clean_shot(m_ptr->loc, p_ptr->loc))
		{
			/* Remove spells that will only hurt friends */
			f4 &= ~(RF4_BOLT_MASK);
			f5 &= ~(RF5_BOLT_MASK);
			f6 &= ~(RF6_BOLT_MASK);
		}

		/* Check for a possible summon */
		if (!(summon_possible(m_ptr->loc)))
		{
			/* Remove summoning spells */
			f4 &= ~(RF4_SUMMON_MASK);
			f5 &= ~(RF5_SUMMON_MASK);
			f6 &= ~(RF6_SUMMON_MASK);
		}

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

	/* Handle "leaving" */
	if (p_ptr->leaving) return (FALSE);


	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, sizeof(m_poss), m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, 0x88);


	/* Choose a spell to cast */
	thrown_spell = choose_attack_spell(m_idx, f4, f5, f6);

	/* Abort if no spell was chosen */
	if (!thrown_spell) return (FALSE);

	/* Calculate spell failure rate */
	failrate = 25 - (rlev + 3) / 4;

	/* Hack -- Stupid monsters will never fail (for jellies and such) */
	if (!smart_monsters || r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

	/* Check for spell failure (innate attacks never fail) */
	if ((thrown_spell >= RF5_OFFSET) && (rand_int(100) < failrate))
	{
		/* Message */
		msg_format("%^s tries to cast a spell, but fails.", m_name);

		return (TRUE);
	}

	/* Cast the spell. */
	switch (thrown_spell)
	{
		/* RF4_SHRIEK */
		case RF4_OFFSET+0:
		{
			if (!direct) break;
			disturb(1, 0);
			sound(MSG_SHRIEK);
			msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx);
			break;
		}

		/* RF4_XXX2X4 */
		case RF4_OFFSET+1:
		{
			break;
		}

		/* RF4_XXX3X4 */
		case RF4_OFFSET+2:
		{
			break;
		}

		/* RF4_XXX4X4 */
		case RF4_OFFSET+3:
		{
			break;
		}

		/* RF4_ARROW_1 */
		case RF4_OFFSET+4:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires an arrow.", m_name);
			bolt(m_idx, GF_ARROW, damroll(1, 6));
			break;
		}

		/* RF4_ARROW_2 */
		case RF4_OFFSET+5:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires an arrow!", m_name);
			bolt(m_idx, GF_ARROW, damroll(3, 6));
			break;
		}

		/* RF4_ARROW_3 */
		case RF4_OFFSET+6:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires a missile.", m_name);
			bolt(m_idx, GF_ARROW, damroll(5, 6));
			break;
		}

		/* RF4_ARROW_4 */
		case RF4_OFFSET+7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a strange noise.", m_name);
			else msg_format("%^s fires a missile!", m_name);
			bolt(m_idx, GF_ARROW, damroll(7, 6));
			break;
		}

		/* RF4_BR_ACID */
		case RF4_OFFSET+8:
		{
			disturb(1, 0);
			sound(MSG_BR_ACID);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes acid.", m_name);
			breath(m_idx, GF_ACID,
			       ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

		/* RF4_BR_ELEC */
		case RF4_OFFSET+9:
		{
			disturb(1, 0);
			sound(MSG_BR_ELEC);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes lightning.", m_name);
			breath(m_idx, GF_ELEC,
			       ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* RF4_BR_FIRE */
		case RF4_OFFSET+10:
		{
			disturb(1, 0);
			sound(MSG_BR_FIRE);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes fire.", m_name);
			breath(m_idx, GF_FIRE,
			       ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* RF4_BR_COLD */
		case RF4_OFFSET+11:
		{
			disturb(1, 0);
			sound(MSG_BR_FROST);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes frost.", m_name);
			breath(m_idx, GF_COLD,
			       ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* RF4_BR_POIS */
		case RF4_OFFSET+12:
		{
			disturb(1, 0);
			sound(MSG_BR_GAS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gas.", m_name);
			breath(m_idx, GF_POIS,
			       ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

		/* RF4_BR_NETH */
		case RF4_OFFSET+13:
		{
			disturb(1, 0);
			sound(MSG_BR_NETHER);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nether.", m_name);
			breath(m_idx, GF_NETHER,
			       ((m_ptr->hp / 6) > 550 ? 550 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

		/* RF4_BR_LITE */
		case RF4_OFFSET+14:
		{
			disturb(1, 0);
			sound(MSG_BR_LIGHT);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes light.", m_name);
			breath(m_idx, GF_LITE,
			       ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_LITE);
			break;
		}

		/* RF4_BR_DARK */
		case RF4_OFFSET+15:
		{
			disturb(1, 0);
			sound(MSG_BR_DARK);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes darkness.", m_name);
			breath(m_idx, GF_DARK,
			       ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_DARK);
			break;
		}

		/* RF4_BR_CONF */
		case RF4_OFFSET+16:
		{
			disturb(1, 0);
			sound(MSG_BR_CONF);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes confusion.", m_name);
			breath(m_idx, GF_CONFUSION,
			       ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_CONFU);
			break;
		}

		/* RF4_BR_SOUN */
		case RF4_OFFSET+17:
		{
			disturb(1, 0);
			sound(MSG_BR_SOUND);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes sound.", m_name);
			breath(m_idx, GF_SOUND,
			       ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_SOUND);
			break;
		}

		/* RF4_BR_CHAO */
		case RF4_OFFSET+18:
		{
			disturb(1, 0);
			sound(MSG_BR_CHAOS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes chaos.", m_name);
			breath(m_idx, GF_CHAOS,
			       ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_CHAOS);
			break;
		}

		/* RF4_BR_DISE */
		case RF4_OFFSET+19:
		{
			disturb(1, 0);
			sound(MSG_BR_DISENCHANT);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes disenchantment.", m_name);
			breath(m_idx, GF_DISENCHANT,
			       ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_DISEN);
			break;
		}

		/* RF4_BR_NEXU */
		case RF4_OFFSET+20:
		{
			disturb(1, 0);
			sound(MSG_BR_NEXUS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nexus.", m_name);
			breath(m_idx, GF_NEXUS,
			       ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_NEXUS);
			break;
		}

		/* RF4_BR_TIME */
		case RF4_OFFSET+21:
		{
			disturb(1, 0);
			sound(MSG_BR_TIME);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes time.", m_name);
			breath(m_idx, GF_TIME,
			       ((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3)));
			break;
		}

		/* RF4_BR_INER */
		case RF4_OFFSET+22:
		{
			disturb(1, 0);
			sound(MSG_BR_INERTIA);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes inertia.", m_name);
			breath(m_idx, GF_INERTIA,
			       ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)));
			break;
		}

		/* RF4_BR_GRAV */
		case RF4_OFFSET+23:
		{
			disturb(1, 0);
			sound(MSG_BR_GRAVITY);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gravity.", m_name);
			breath(m_idx, GF_GRAVITY,
			       ((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)));
			break;
		}

		/* RF4_BR_SHAR */
		case RF4_OFFSET+24:
		{
			disturb(1, 0);
			sound(MSG_BR_SHARDS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes shards.", m_name);
			breath(m_idx, GF_SHARD,
			       ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_SHARD);
			break;
		}

		/* RF4_BR_PLAS */
		case RF4_OFFSET+25:
		{
			disturb(1, 0);
			sound(MSG_BR_PLASMA);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes plasma.", m_name);
			breath(m_idx, GF_PLASMA,
			       ((m_ptr->hp / 6) > 150 ? 150 : (m_ptr->hp / 6)));
			break;
		}

		/* RF4_BR_WALL */
		case RF4_OFFSET+26:
		{
			disturb(1, 0);
			sound(MSG_BR_FORCE);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes force.", m_name);
			breath(m_idx, GF_FORCE,
			       ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)));
			break;
		}

		/* RF4_BR_MANA */
		case RF4_OFFSET+27:
		{
			/* XXX XXX XXX */
			break;
		}

		/* RF4_XXX5X4 */
		case RF4_OFFSET+28:
		{
			break;
		}

		/* RF4_XXX6X4 */
		case RF4_OFFSET+29:
		{
			break;
		}

		/* RF4_XXX7X4 */
		case RF4_OFFSET+30:
		{
			break;
		}

		/* RF4_BOULDER */
		case RF4_OFFSET+31:
		{
			disturb(1, 0);
			if (blind) msg_format("You hear something grunt with exertion.", m_name);
			else msg_format("%^s hurls a boulder at you!", m_name);
			bolt(m_idx, GF_ARROW, damroll(1 + r_ptr->level / 7, 12));
			break;
		}


		/* RF5_BA_ACID */
		case RF5_OFFSET+0:
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
		case RF5_OFFSET+1:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a lightning ball.", m_name);
			breath(m_idx, GF_ELEC,
			       randint(rlev * 3 / 2) + 8);
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

		/* RF5_BA_FIRE */
		case RF5_OFFSET+2:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a fire ball.", m_name);
			breath(m_idx, GF_FIRE,
			       randint(rlev * 7 / 2) + 10);
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

		/* RF5_BA_COLD */
		case RF5_OFFSET+3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a frost ball.", m_name);
			breath(m_idx, GF_COLD,
			       randint(rlev * 3 / 2) + 10);
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

		/* RF5_BA_POIS */
		case RF5_OFFSET+4:
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
		case RF5_OFFSET+5:
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
		case RF5_OFFSET+6:
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
		case RF5_OFFSET+7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
			else msg_format("%^s invokes a mana storm.", m_name);
			breath(m_idx, GF_MANA,
			       (rlev * 5) + damroll(10, 10));
			break;
		}

		/* RF5_BA_DARK */
		case RF5_OFFSET+8:
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
		case RF5_OFFSET+9:
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

			if (p_ptr->std_save())
			{
				msg_print("You resist the effects!");
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");
				if (!p_ptr->resist_confu)
				{
					(void)p_ptr->inc_timed<TMD_CONFUSED>(rand_int(4) + 4);
				}
				take_hit(damroll(8, 8), ddesc);
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
			if (p_ptr->std_save())
			{
				msg_print("You resist the effects!");
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");
				take_hit(damroll(12, 15), ddesc);
				if (!p_ptr->resist_blind)
				{
					(void)p_ptr->inc_timed<TMD_BLIND>(8 + rand_int(8));
				}
				if (!p_ptr->resist_confu)
				{
					(void)p_ptr->inc_timed<TMD_CONFUSED>(rand_int(4) + 4);
				}
				if (!p_ptr->free_act)
				{
					(void)p_ptr->inc_timed<TMD_PARALYZED>(rand_int(4) + 4);
				}
				(void)p_ptr->inc_timed<TMD_SLOW>(rand_int(4) + 4);
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
			if (p_ptr->std_save())
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
			if (p_ptr->std_save())
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
		case RF5_OFFSET+14:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles loudly.", m_name);
			else msg_format("%^s points at you, incanting terribly!", m_name);
			if (p_ptr->std_save())
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
		case RF5_OFFSET+15:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s screams the word 'DIE!'", m_name);
			else msg_format("%^s points at you, screaming the word DIE!", m_name);
			if (p_ptr->std_save())
			{
				msg_print("You resist the effects!");
			}
			else
			{
				take_hit(damroll(15, 15), ddesc);
				(void)p_ptr->inc_timed<TMD_CUT>(damroll(10, 10));
			}
			break;
		}

		/* RF5_BO_ACID */
		case RF5_OFFSET+16:
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
		case RF5_OFFSET+17:
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
		case RF5_OFFSET+18:
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
		case RF5_OFFSET+19:
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
		case RF5_OFFSET+20:
		{
			/* XXX XXX XXX */
			break;
		}

		/* RF5_BO_NETH */
		case RF5_OFFSET+21:
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
		case RF5_OFFSET+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a water bolt.", m_name);
			bolt(m_idx, GF_WATER,
			     damroll(10, 10) + (rlev));
			break;
		}

		/* RF5_BO_MANA */
		case RF5_OFFSET+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a mana bolt.", m_name);
			bolt(m_idx, GF_MANA,
			     randint(rlev * 7 / 2) + 50);
			break;
		}

		/* RF5_BO_PLAS */
		case RF5_OFFSET+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s casts a plasma bolt.", m_name);
			bolt(m_idx, GF_PLASMA,
			     10 + damroll(8, 7) + (rlev));
			break;
		}

		/* RF5_BO_ICEE */
		case RF5_OFFSET+25:
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
			if (!direct) break;
			disturb(1, 0);
			sound(MSG_CAST_FEAR);
			if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
			else msg_format("%^s casts a fearful illusion.", m_name);
			if (p_ptr->resist_fear)
			{
				msg_print("You refuse to be frightened.");
			}
			else if (p_ptr->std_save())
			{
				msg_print("You refuse to be frightened.");
			}
			else
			{
				(void)p_ptr->inc_timed<TMD_AFRAID>(rand_int(4) + 4);
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
			else if (p_ptr->std_save())
			{
				msg_print("You resist the effects!");
			}
			else
			{
				(void)p_ptr->set_timed<TMD_BLIND>(12 + rand_int(4));
			}
			update_smart_learn(m_idx, DRS_RES_BLIND);
			break;
		}

		/* RF5_CONF */
		case RF5_OFFSET+29:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else msg_format("%^s creates a mesmerising illusion.", m_name);
			if (p_ptr->resist_confu)
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else if (p_ptr->std_save())
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else
			{
				(void)p_ptr->inc_timed<TMD_CONFUSED>(rand_int(4) + 4);
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
			if (p_ptr->free_act)
			{
				msg_print("You are unaffected!");
			}
			else if (p_ptr->std_save())
			{
				msg_print("You resist the effects!");
			}
			else
			{
				(void)p_ptr->inc_timed<TMD_SLOW>(rand_int(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}

		/* RF5_HOLD */
		case RF5_OFFSET+31:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s stares deep into your eyes!", m_name);
			if (p_ptr->free_act)
			{
				msg_print("You are unaffected!");
			}
			else if (p_ptr->std_save())
			{
				msg_format("You resist the effects!");
			}
			else
			{
				(void)p_ptr->inc_timed<TMD_PARALYZED>(rand_int(4) + 4);
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
				msg_format("%^s starts moving faster.", m_name);
				m_ptr->mspeed += 2;
			}

			break;
		}

		/* RF6_XXX1X6 */
		case RF6_OFFSET+1:
		{
			break;
		}

		/* RF6_HEAL */
		case RF6_OFFSET+2:
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
					msg_format("%^s looks REALLY healthy!", m_name);
				}
				else
				{
					msg_format("%^s sounds REALLY healthy!", m_name);
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
		case RF6_OFFSET+3:
		{
			break;
		}

		/* RF6_BLINK */
		case RF6_OFFSET+4:
		{
			disturb(1, 0);
			msg_format("%^s blinks away.", m_name);
			teleport_away(m_idx, 10);
			break;
		}

		/* RF6_TPORT */
		case RF6_OFFSET+5:
		{
			disturb(1, 0);
			msg_format("%^s teleports away.", m_name);
			teleport_away(m_idx, MAX_SIGHT * 2 + 5);
			break;
		}

		/* RF6_XXX3X6 */
		case RF6_OFFSET+6:
		{
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
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s commands you to return.", m_name);
			teleport_player_to(m_ptr->loc);
			break;
		}

		/* RF6_TELE_AWAY */
		case RF6_OFFSET+9:
		{
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s teleports you away.", m_name);
			teleport_player(100);
			break;
		}

		/* RF6_TELE_LEVEL */
		case RF6_OFFSET+10:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles strangely.", m_name);
			else msg_format("%^s gestures at your feet.", m_name);
			if (p_ptr->resist_nexus)
			{
				msg_print("You are unaffected!");
			}
			else if (p_ptr->std_save())
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
		case RF6_OFFSET+11:
		{
			break;
		}

		/* RF6_DARKNESS */
		case RF6_OFFSET+12:
		{
			if (!direct) break;
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s gestures in shadow.", m_name);
			(void)unlite_area(0, 3);
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

			if (p_ptr->std_save())
			{
				msg_print("You resist the effects!");
			}
			else if (lose_all_info())
			{
				msg_print("Your memories fade away.");
			}
			break;
		}

		/* RF6_ANIMAL */
		case RF6_OFFSET+15:
		{
			break;
		}

		/* RF6_S_KIN */
		case RF6_OFFSET+16:
		{
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
				count += summon_specific(m_ptr->loc, rlev, SUMMON_KIN);
			}
			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_HI_DEMON */
		case RF6_OFFSET+17:
		{
			disturb(1, 0);
			sound(MSG_SUM_HI_DEMON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater demons!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_HI_DEMON);
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
			disturb(1, 0);
			sound(MSG_SUM_MONSTER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons help!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, 0);
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
			disturb(1, 0);
			sound(MSG_SUM_MONSTER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons monsters!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, 0);
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
			disturb(1, 0);
			sound(MSG_SUM_ANIMAL);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons animals.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_ANIMAL);
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
			disturb(1, 0);
			sound(MSG_SUM_SPIDER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons spiders.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_SPIDER);
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
			disturb(1, 0);
			sound(MSG_SUM_HOUND);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hounds.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_HOUND);
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
			disturb(1, 0);
			sound(MSG_SUM_HYDRA);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hydras.", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_HYDRA);
			}
			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_ANGEL */
		case RF6_OFFSET+24:
		{
			disturb(1, 0);
			sound(MSG_SUM_ANGEL);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons an angel!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_ANGEL);
			}
			if (blind && count)
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_DEMON */
		case RF6_OFFSET+25:
		{
			disturb(1, 0);
			sound(MSG_SUM_DEMON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a hellish adversary!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_DEMON);
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
			disturb(1, 0);
			sound(MSG_SUM_UNDEAD);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons an undead adversary!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_UNDEAD);
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
			disturb(1, 0);
			sound(MSG_SUM_DRAGON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a dragon!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_DRAGON);
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
			disturb(1, 0);
			sound(MSG_SUM_HI_UNDEAD);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater undead!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

		/* RF6_S_HI_DRAGON */
		case RF6_OFFSET+29:
		{
			disturb(1, 0);
			sound(MSG_SUM_HI_DRAGON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons ancient dragons!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_HI_DRAGON);
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
			disturb(1, 0);
			sound(MSG_SUM_WRAITH);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons mighty undead opponents!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_WRAITH);
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

		/* RF6_S_UNIQUE */
		case RF6_OFFSET+31:
		{
			disturb(1, 0);
			sound(MSG_SUM_UNIQUE);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons special opponents!", m_name);
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_UNIQUE);
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(m_ptr->loc, rlev, SUMMON_HI_UNDEAD);
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
			if (l_ptr->cast_innate < UCHAR_MAX) l_ptr->cast_innate++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32*5)
		{
			l_ptr->flags5 |= (1L << (thrown_spell - 32*4));
			if (l_ptr->cast_spell < UCHAR_MAX) l_ptr->cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32*6)
		{
			l_ptr->flags6 |= (1L << (thrown_spell - 32*5));
			if (l_ptr->cast_spell < UCHAR_MAX) l_ptr->cast_spell++;
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

#if 0
/* 
 * initializes an estimate of how much "attack power" can be brought to bear on a grid.
 */
static void mon_attack_power(coord g,int& dam_min, int& dam_median, int& dam_max)
{
	/* base initialization */
	dam_min = 0;
	dam_median = 0;
	dam_max = 0;

	/*! \todo melee check */
	/*! \todo spells/innate check */
}
#endif


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
	monster_race *r_ptr = m_ptr->race();

	/* Keep monsters from running too far away */
	if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);

	/* Nearby monsters will not become terrified */
	if (m_ptr->cdis <= 5) return (FALSE);

#if 0
	{
	int m_min, m_median, m_max;
	int pmelee_min, pmelee_median, pmelee_max;
	int pmissile_min, pmissile_median, pmissile_max;
	int mhp = m_ptr->chp;
	int php = p_ptr->chp;

	m_ptr->melee_analyze(m_min,m_median,m_max,p_ptr->loc);
	p_ptr->melee_analyze(m_ptr,pmelee_min,pmelee_median,pmelee_max);
	p_ptr->missile_analyze(m_ptr,pmissile_min,pmissile_median,pmissile_max);

	if ((1 == m_ptr->cdis) && (m_max >= p_ptr->chp)) return FALSE;
	}
#else
	if (1 == m_ptr->cdis)
	{	/* check for melee instakill chance; do not run if we have it */
		int m_min, m_median, m_max;
		m_ptr->melee_analyze(m_min,m_median,m_max,p_ptr->loc);
		if (m_max >= p_ptr->chp) return FALSE;
	}
#endif

	{	/* C-ish blocking brace */
	u16b p_lev = p_ptr->lev;							/* Examine player power (level) */
	u16b m_lev = r_ptr->level + (m_idx & 0x08) + 25;	/* Examine monster power (level plus morale) */

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4) return (FALSE);
	if (m_lev + 4 <= p_lev) return (TRUE);

	{	/* C-ish blocking brace */
	/* Examine player health */
	u16b p_chp = p_ptr->chp;
	u16b p_mhp = p_ptr->mhp;

	/* Examine monster health */
	u16b m_chp = m_ptr->hp;
	u16b m_mhp = m_ptr->maxhp;

	/* Prepare to optimize the calculation */
	u16b p_val = (p_lev * p_mhp) + (p_chp << 2);	/* div p_mhp */
	u16b m_val = (m_lev * m_mhp) + (m_chp << 2);	/* div m_mhp */

	/* Strong players scare strong monsters */
	if (p_val * m_mhp > m_val * p_mhp) return (TRUE);
	}	/* end blocking brace */
	}	/* end blocking brace */

	/* Assume no terror */
	return (FALSE);
}


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
static int get_move_dir_aux(int m_idx)
{
	/* Monster flowing disabled */
	if (!flow_by_sound) return 0;

	{	/* C-style blocking braces */
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();
	
	int i;
	int best_i = 0;
	int when = 0;
	int cost = 999;

	/* Monster can go through rocks */
	if (r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) return 0;

	/* Monster never moves */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE)) return 0;

	/* The player is not currently near the monster grid */
	if (cave_when[m_ptr->loc.y][m_ptr->loc.x] < cave_when[p_ptr->loc.y][p_ptr->loc.x])
	{
		/* The player has never been near the monster grid */
		if (cave_when[m_ptr->loc.y][m_ptr->loc.x] == 0) return 0;

		/* The monster is not allowed to track the player */
		if (!flow_by_smell) return 0;
	}

	/* Monster is too far away to notice the player */
	if (cave_cost[m_ptr->loc.y][m_ptr->loc.x] > MONSTER_FLOW_DEPTH) return 0;
	if (cave_cost[m_ptr->loc.y][m_ptr->loc.x] > r_ptr->aaf) return 0;

	/* Hack -- Player can see us, run towards him */
	if (player_has_los_bold(m_ptr->loc.y, m_ptr->loc.x)) return 0;

	/* Check nearby grids, diagonals first */
	for (i = KEYPAD_DIR_MAX-1; i >= 0; i--)
	{
		/* Get the location */
		coord tmp(m_ptr->loc);
		tmp += dd_coord_ddd[i];

		/* Ignore illegal locations */
		if (cave_when[tmp.y][tmp.x] == 0) continue;

		/* Ignore ancient locations */
		if (cave_when[tmp.y][tmp.x] < when) continue;

		/* Ignore distant locations */
		if (cave_cost[tmp.y][tmp.x] > cost) continue;

		/* Save the cost and time */
		when = cave_when[tmp.y][tmp.x];
		cost = cave_cost[tmp.y][tmp.x];
		best_i = i;
	}

	/* No flowed move */
	if (!when) return 0;

	/* Success */
	return ddd[best_i];
	}	/* end blocking brace */
}

/*
 * Hack -- compare the "strength" of two monsters XXX XXX XXX
 */
static int compare_monsters(const monster_type *m_ptr, const monster_type *n_ptr)
{
	u32b mexp1 = m_ptr->race()->mexp;
	u32b mexp2 = n_ptr->race()->mexp;

	/* Compare */
	if (mexp1 < mexp2) return (-1);
	if (mexp1 > mexp2) return (1);

	/* Assume equal */
	return (0);
}

static bool nodoor_flee_move_legal(coord g)
{
	/* Skip illegal locations */
	if (!in_bounds_fully(g.y, g.x)) return FALSE;

	/* Skip locations in a wall */
	if (!cave_floor_bold(g.y, g.x)) return FALSE;

	/* can't handle doors */
	if (((cave_feat[g.y][g.x] >= FEAT_DOOR_HEAD) &&
	     (cave_feat[g.y][g.x] <= FEAT_DOOR_TAIL)) ||
	     (cave_feat[g.y][g.x] == FEAT_SECRET))
		return FALSE;

	/* Skip player, since we're fleeing */
	if (g==p_ptr->loc) return FALSE;

	return TRUE;
}

static bool normal_flee_move_legal(coord g)
{
	/* Skip illegal locations */
	if (!in_bounds_fully(g.y, g.x)) return FALSE;

	/* Skip locations in a wall */
	if (!cave_floor_bold(g.y, g.x)) return FALSE;

	/* Skip player, since we're fleeing */
	if (g==p_ptr->loc) return FALSE;

	return TRUE;
}

static bool wallpasser_flee_move_legal(coord g)
{
	/* Skip illegal locations */
	if (!in_bounds_fully(g.y, g.x)) return FALSE;

	/* Skip player, since we're fleeing */
	if (g==p_ptr->loc) return FALSE;

	return true;
}

static bool player_no_los(coord g)
{
	return !player_has_los_bold(g.y,g.x);
}

static int find_safety_dir(int m_idx)
{
	monster_type* m_ptr = &mon_list[m_idx];
	monster_race* r_ptr = m_ptr->race();

	/* Monster never moves */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE)) return 0;

	{	/* C-style blocking brace */
	int dir_stack[8];
	int dis_stack[8];
	size_t dir_stack_UB = 0;
	size_t i;

	byte access_map[(2*10+1)*(2*10+1)];

	flow_from(access_map,10,m_ptr->loc,((r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) ? wallpasser_flee_move_legal
									: ((r_ptr->flags2 & (RF2_OPEN_DOOR | RF2_BASH_DOOR)) ? normal_flee_move_legal : nodoor_flee_move_legal)),
						player_no_los,NULL);

	for(i = 0; i<KEYPAD_DIR_MAX; ++i)
	{
		if (0!=access_map[distance_square_idx(dd_coord_ddd[i],10)])
		{	/* check for ability to move through other monsters here */
			coord n = m_ptr->loc;
			n += dd_coord_ddd[i];
			if (cave_m_idx[n.y][n.x] > 0)
			{
				if (!(r_ptr->flags2 & (RF2_KILL_BODY))) continue;	/* oops */
				{	/* C-ish blocking brace */
				monster_type *n_ptr = &mon_list[cave_m_idx[n.y][n.x]];
				if (compare_monsters(m_ptr, n_ptr) <= 0) continue;	/* no-go */				
				}	/* end blocking brace */
			}
			dir_stack[dir_stack_UB++] = i;
		}
	}

	/* if did not find safety, fail now */
	if (0 == dir_stack_UB) return 0;
	if (1 == dir_stack_UB) return ddd[dir_stack[0]];

	/* maximize distance from player when fleeing */
	i = dir_stack_UB;
	do	{
		--i;
		dis_stack[i] = distance(m_ptr->loc.y+ddy[dir_stack[i]],m_ptr->loc.x+ddx[dir_stack[i]],p_ptr->loc.y,p_ptr->loc.x);
		while(i+1<dir_stack_UB && dis_stack[i]!=dis_stack[i+1])
		{
			if (dis_stack[i]<dis_stack[i+1])
			{
				memmove(dis_stack+i,dis_stack+i+1,(dir_stack_UB-(i+1))*sizeof(int));
				memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+1))*sizeof(int));
			}
			else if (i+2<dir_stack_UB)
			{
				memmove(dis_stack+i+1,dis_stack+i+2,(dir_stack_UB-(i+2))*sizeof(int));
				memmove(dir_stack+i+1,dir_stack+i+2,(dir_stack_UB-(i+2))*sizeof(int));
			}
			--dir_stack_UB;
		}
		}
	while(0<i);

	if (1 == dir_stack_UB) return ddd[dir_stack[0]];
	return ddd[dir_stack[rand_int(dir_stack_UB)]];
	}	/* end blocking brace */
}

static int find_hiding_dir(int m_idx)
{
	monster_type* m_ptr = &mon_list[m_idx];
	monster_race* r_ptr = m_ptr->race();

	/* Monster never moves */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE)) return 0;

	{	/* C-style blocking brace */
	int dir_stack[8];
	int dis_stack[8];
	size_t dir_stack_UB = 0;
	size_t i;

	byte access_map[(2*10+1)*(2*10+1)];

	flow_from(access_map,10,m_ptr->loc,((r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) ? wallpasser_flee_move_legal
									: ((r_ptr->flags2 & (RF2_OPEN_DOOR | RF2_BASH_DOOR)) ? normal_flee_move_legal : nodoor_flee_move_legal)),
						player_no_los,NULL);

	for(i = 0; i<KEYPAD_DIR_MAX; ++i)
	{
		if (0!=access_map[distance_square_idx(dd_coord_ddd[i],10)])
		{	/* check for ability to move through other monsters here */
			coord n = m_ptr->loc;
			n += dd_coord_ddd[i];
			if (cave_m_idx[n.y][n.x] > 0)
			{
				if (!(r_ptr->flags2 & (RF2_KILL_BODY))) continue;	/* oops */
				{	/* C-ish blocking brace */
				monster_type *n_ptr = &mon_list[cave_m_idx[n.y][n.x]];
				if (compare_monsters(m_ptr, n_ptr) <= 0) continue;	/* no-go */				
				}	/* end blocking brace */
			}
			dir_stack[dir_stack_UB++] = i;
		}
	}

	/* if did not find safety, fail now */
	if (0 == dir_stack_UB) return 0;
	if (1 == dir_stack_UB) return ddd[dir_stack[0]];

	/* minimize distance from player when hiding */
	i = dir_stack_UB;
	do	{
		--i;
		dis_stack[i] = distance(m_ptr->loc.y+ddy[dir_stack[i]],m_ptr->loc.x+ddx[dir_stack[i]],p_ptr->loc.y,p_ptr->loc.x);
		while(i+1<dir_stack_UB && dis_stack[i]!=dis_stack[i+1])
		{
			if (dis_stack[i]>dis_stack[i+1])
			{
				memmove(dis_stack+i,dis_stack+i+1,(dir_stack_UB-(i+1))*sizeof(int));
				memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+1))*sizeof(int));
			}
			else if (i+2<dir_stack_UB)
			{
				memmove(dis_stack+i+1,dis_stack+i+2,(dir_stack_UB-(i+2))*sizeof(int));
				memmove(dir_stack+i+1,dir_stack+i+2,(dir_stack_UB-(i+2))*sizeof(int));
			}
			--dir_stack_UB;
		}
		}
	while(0<i);

	if (1 == dir_stack_UB) return ddd[dir_stack[0]];
	return ddd[dir_stack[rand_int(dir_stack_UB)]];
	}	/* end blocking brace */
}

static int find_target_dir(int m_idx,coord g)
{
	monster_type* m_ptr = &mon_list[m_idx];
	monster_race* r_ptr = m_ptr->race();

	/* Monster never moves */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE)) return 0;

	{	/* C-style blocking brace */
	int dir_stack[8];
	int dis_stack[8];
	size_t dir_stack_UB = 0;
	size_t i;

	byte access_map[(2*10+1)*(2*10+1)];

	flow_from(access_map,10,m_ptr->loc,((r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) ? wallpasser_flee_move_legal
									: ((r_ptr->flags2 & (RF2_OPEN_DOOR | RF2_BASH_DOOR)) ? normal_flee_move_legal : nodoor_flee_move_legal)),
						NULL,&g);

	for(i = 0; i<KEYPAD_DIR_MAX; ++i)
	{
		if (0!=access_map[distance_square_idx(dd_coord_ddd[i],10)])
		{	/* check for ability to move through other monsters here */
			coord n = m_ptr->loc;
			n += dd_coord_ddd[i];
			if (cave_m_idx[n.y][n.x] > 0)
			{
				if (!(r_ptr->flags2 & (RF2_KILL_BODY))) continue;	/* oops */
				{	/* C-ish blocking brace */
				monster_type *n_ptr = &mon_list[cave_m_idx[n.y][n.x]];
				if (compare_monsters(m_ptr, n_ptr) <= 0) continue;	/* no-go */				
				}	/* end blocking brace */
			}
			dir_stack[dir_stack_UB++] = i;
		}
	}

	/* if did not find safety, fail now */
	if (0 == dir_stack_UB) return 0;
	if (1 == dir_stack_UB) return ddd[dir_stack[0]];

	/* maximize distance from player when closing in */
	i = dir_stack_UB;
	do	{
		--i;
		dis_stack[i] = distance(m_ptr->loc.y+ddy[dir_stack[i]],m_ptr->loc.x+ddx[dir_stack[i]],p_ptr->loc.y,p_ptr->loc.x);
		while(i+1<dir_stack_UB && dis_stack[i]!=dis_stack[i+1])
		{
			if (dis_stack[i]<dis_stack[i+1])
			{
				memmove(dis_stack+i,dis_stack+i+1,(dir_stack_UB-(i+1))*sizeof(int));
				memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+1))*sizeof(int));
			}
			else if (i+2<dir_stack_UB)
			{
				memmove(dis_stack+i+1,dis_stack+i+2,(dir_stack_UB-(i+2))*sizeof(int));
				memmove(dir_stack+i+1,dir_stack+i+2,(dir_stack_UB-(i+2))*sizeof(int));
			}
			--dir_stack_UB;
		}
		}
	while(0<i);

	if (1 == dir_stack_UB) return ddd[dir_stack[0]];
	return ddd[dir_stack[rand_int(dir_stack_UB)]];
	}	/* end blocking brace */
}

#if 0
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
#endif


/*
 * Choose "logical" directions for monster movement
 *
 * We store the directions in a special "mm" array
 */
static bool get_moves(int m_idx, int mm[5])
{
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();

	bool done = FALSE;

	int flow_dir = get_move_dir_aux(m_idx);

	if (!flow_dir)
	{
		flow_dir = 5;
		if (m_ptr->loc.x!=p_ptr->loc.x)
		{
			flow_dir += (m_ptr->loc.x<p_ptr->loc.x) ? 1 : -1;
		}
		if (m_ptr->loc.y!=p_ptr->loc.y)
		{
			flow_dir += (m_ptr->loc.y<p_ptr->loc.y) ? -3 : 3;
		}
	}

	/* Normal animal packs try to get the player out of corridors. */
	if (smart_packs &&
	    (r_ptr->flags1 & RF1_FRIENDS) && (r_ptr->flags3 & RF3_ANIMAL) &&
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
		if ((room < 8) && (p_ptr->chp > p_ptr->mhp / 2))
		{
			/* Find hiding place */
			int hide_dir = find_hiding_dir(m_idx);
			if (hide_dir)
			{
				flow_dir = hide_dir;
				done = TRUE;
			}
		}
	}

	/* Apply fear */
	if (!done && mon_will_run(m_idx))
	{
		/* Try to find safe place */
		int flee_dir = (smart_monsters) ? find_safety_dir(m_idx) : 0;
		if (flee_dir)
		{
			flow_dir = flee_dir;
		}
		else
		{
			flow_dir = 10-flow_dir;	/* inverts keypad direction */
		};

		done = TRUE;
	}

	/* Monster groups try to surround the player */
	if (!done && smart_packs && (r_ptr->flags1 & RF1_FRIENDS))
	{
		coord tmp;
		bool not_there = true;
		int i;

		/* Find an empty square near the player to fill */
		for (i = 0; i < KEYPAD_DIR_MAX; i++)
		{
			tmp = p_ptr->loc;
			tmp += dd_coord_ddd[(m_idx + i) & 7];

			if (m_ptr->loc == tmp)
			{
				not_there = false;				
				break;
			}

			/* Ignore filled grids */
			if (!cave_empty_bold(tmp.y, tmp.x)) continue;

			/* Try to fill this hole */
			break;
		}

		if (not_there)
		{
			int target_dir = find_target_dir(m_idx,tmp);
			if (target_dir)
			{
				flow_dir = target_dir;
			}
		}
	}

	if (!flow_dir) return FALSE;

	mm[0] = flow_dir;
	if (one_in_(2))
	{
		mm[1] = cycle[chome[mm[0]]+1];
		mm[2] = cycle[chome[mm[0]]-1];
		mm[3] = cycle[chome[mm[0]]+2];
		mm[4] = cycle[chome[mm[0]]-2];
	}
	else
	{
		mm[1] = cycle[chome[mm[0]]-1];
		mm[2] = cycle[chome[mm[0]]+1];
		mm[3] = cycle[chome[mm[0]]-2];
		mm[4] = cycle[chome[mm[0]]+2];
	}

	/* Want to move */
	return (TRUE);
}



/*
 * ZaiBand: Meta-code for enhanced jellies/molds
 * typedef for selective_destroy_ok must interoperate 
 * with set_acid_destroy in spells1.c
 */

typedef int selective_destroy_ok(const object_type *o_ptr);
typedef int selective_destroy_helper(object_type *o_ptr, monster_type *m_ptr);

static int eat_food_floor_ok(const object_type *o_ptr)
{
	if (   o_ptr->tval == TV_FOOD
		&& o_ptr->sval >= SV_FOOD_CURE_POISON)
		return (TRUE);
	return (FALSE);
}

static int non_money_ok(const object_type *o_ptr)
{
	if (o_ptr->tval != TV_GOLD)
		return (TRUE);
	return (FALSE);
}

static int spoil_food_ok(const object_type *o_ptr)
{
	if (o_ptr->tval == TV_FOOD)
		return (TRUE);
	return (FALSE);
}

static int eat_lite_ok(const object_type *o_ptr)
{
	if (   o_ptr->tval == TV_LITE
		&& 0<o_ptr->pval
		&& !o_ptr->is_artifact())
		return (TRUE);
	return (FALSE);
}

static int eat_food_floor_helper(object_type *o_ptr, monster_type *m_ptr)
{
	if 		(o_ptr->sval == SV_FOOD_CURE_PARANOIA && m_ptr->monfear)
		{	/* cure fear */
		m_ptr->monfear = 1;		/* XXX -- don't want to clone code */
		}
	else if (o_ptr->sval == SV_FOOD_CURE_CONFUSION && m_ptr->confused)
		{	/* cure confusion */
		m_ptr->confused = 1;	/* XXX -- don't want to clone code */
		}
	return TRUE;
}

static int take_item_helper(object_type *o_ptr, monster_type *m_ptr)
{
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;		/* Get local object */
	size_t m_idx = 0;
	while(&mon_list[m_idx]!=m_ptr) m_idx++;		/* DOS-safe recovery */

	COPY(i_ptr, o_ptr);							/* Obtain local object */	
	(void)monster_carry(m_idx, i_ptr);			/* Carry the object */
	return TRUE;
}

static int eat_lite_helper(object_type *o_ptr, monster_type *m_ptr)
{
	(void)(m_ptr);		/* unused parameter */
	o_ptr->pval = 0;	/* no light left! */
	return FALSE;
}

static bool dont_destroy_this(object_type *o_ptr, monster_type *m_ptr)
{
	if (o_ptr->is_artifact()) return (TRUE);	/* artifacts are indestructible */

	if (NULL!=m_ptr)	/* monster is considered relevant to this test */
		{
		u32b f1, f2, f3;
		u32b flg3 = 0L;
		monster_race *r_ptr = m_ptr->race();
		object_flags(o_ptr, &f1, &f2, &f3); /* extract some flags */

		if (0L!=r_ptr->flags3 && 0L!=f1)
			{	/* React to objects that hurt the monster */
			if (f1 & (TR1_KILL_DRAGON)) flg3 |= (RF3_DRAGON);
			if (f1 & (TR1_KILL_DEMON)) flg3 |= (RF3_DEMON);
			if (f1 & (TR1_KILL_UNDEAD)) flg3 |= (RF3_UNDEAD);
			if (f1 & (TR1_SLAY_DRAGON)) flg3 |= (RF3_DRAGON);
			if (f1 & (TR1_SLAY_TROLL)) flg3 |= (RF3_TROLL);
			if (f1 & (TR1_SLAY_GIANT)) flg3 |= (RF3_GIANT);
			if (f1 & (TR1_SLAY_ORC)) flg3 |= (RF3_ORC);
			if (f1 & (TR1_SLAY_DEMON)) flg3 |= (RF3_DEMON);
			if (f1 & (TR1_SLAY_UNDEAD)) flg3 |= (RF3_UNDEAD);
			if (f1 & (TR1_SLAY_ANIMAL)) flg3 |= (RF3_ANIMAL);
			if (f1 & (TR1_SLAY_EVIL)) flg3 |= (RF3_EVIL);

			if (r_ptr->flags3 & flg3) return (TRUE);
			}
		}
	return (FALSE);
}

/* indestructible message: e.g., "%^s tries to eat %s, but fails." */
/* destructible message: e.g., "%^s eats %s." */
static bool destroy_items_selectively(s16b this_o_idx, monster_type *m_ptr, bool has_los,
	cptr indestructible_message, cptr destructible_message, 
	selective_destroy_ok* test_func, selective_destroy_helper* test_helper)
{
	char m_name[80];
	bool did_something = (FALSE);
	if (!this_o_idx) return (FALSE);

	monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);		/* Get the monster name */

	do	{
		char o_name[80];
		object_type *o_ptr = &o_list[this_o_idx];
		s16b next_o_idx = o_ptr->next_o_idx;		/* Get the next object */

		if (NULL==test_func || test_func(o_ptr))
		{	/* approve item as target for selective destruction */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);	/* Get the object name */

			/* The object cannot be picked up by the monster */
			if (dont_destroy_this(o_ptr,m_ptr))
			{	/* Describe observable situations */
				if (m_ptr->ml && has_los && NULL!=indestructible_message)
				{	/* Dump a message */
					msg_format(indestructible_message,m_name, o_name);
					did_something = (TRUE);
				}
			}

			/* Eat the item */
			else
			{	/* Describe observable situations */
				if (has_los)
				{	/* Dump a message */
					msg_format(destructible_message,m_name, o_name);
					did_something = (TRUE);
				}

				/* item may affect monster */
				if (NULL==test_helper || test_helper(o_ptr,m_ptr))
					{
					delete_object_idx(this_o_idx);	/* delete the object */
					}
			}
		}

		this_o_idx = next_o_idx;
		}	
	while(this_o_idx);
	return did_something;
}

void
monster_type::wake_up()
{
	/* reality check */
	assert(0<csleep);

	/* Reset sleep counter */
	csleep = 0;

	/* Start with no energy */
	energy = 0;

	/* Notice the "waking up" */
	if (ml)
		{
		char m_name[80];

		/* Get the monster name */
		monster_desc(m_name, sizeof(m_name), this, 0);

		/* Dump a message */
		msg_format("%^s wakes up.", m_name);

		/* Hack -- Update the health bar */
		if (p_ptr->health_who == (this-mon_list)) p_ptr->redraw |= (PR_HEALTH);

		/* Hack -- Count the wakings */
		if (lore()->wake < UCHAR_MAX) lore()->wake++;
	}
}

void
monster_type::disturb(int d)
{
	/* reality check */
	assert(0<csleep);

	/* minimum disturbance level */
	if (0>=d) d = 1;

	/* Still asleep */
	if (csleep > d)
	{
		/* Monster wakes up "a little bit" */
		csleep -= d;

		/* Notice the "not waking up" */
		if (ml)
		{
			/* Hack -- Count the ignores */
			if (lore()->ignore < UCHAR_MAX) lore()->ignore++;
		}
	}

	/* Just woke up */
	else
	{
		wake_up();
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
 * will still stand there trying to open it...  XXX XXX XXX
 *
 * Technically, need to check for monster in the way combined
 * with that monster being in a wall (or door?) XXX
 */
static void process_monster(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();
	monster_lore *l_ptr = m_ptr->lore();

	int i, d;

	int mm[5];

	coord o,n;

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


	/* Handle "sleep" */
	if (m_ptr->csleep)
	{
		/* Aggravation */
		if (p_ptr->aggravate)
		{
			m_ptr->wake_up();
		}

		/* Zaiband: Player is in line-of-sight */
		else if ((m_ptr->cdis <= r_ptr->aaf) && los(m_ptr->loc,p_ptr->loc))
		{
			/* Wake up faster near the player */
			/* cf. noise code in dungeon.c; Intensity 1, distance is cached */
			m_ptr->disturb(100/m_ptr->cdis);
		}

		/* as the monster is either asleep or groggy, don't do anything more this turn */
		return;
	}


	/* Get the origin */
	o = m_ptr->loc;

	/* Attempt to "multiply" if able and allowed */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) && (num_repro < MAX_REPRO))
	{
		int k, y, x;

		/* Count the adjacent monsters */
		for (k = 0, y = o.y - 1; y <= o.y + 1; y++)
		{
			for (x = o.x - 1; x <= o.x + 1; x++)
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


	/* Attempt to cast a spell */
	if (make_attack_spell(m_idx)) return;


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


	/* Process moves */
	for (i = 0; i < 5; i++)
	{
		/* Get the direction (or stagger) */
		d = (stagger ? ddd[rand_int(KEYPAD_DIR_MAX)] : mm[i]);

		/* Get the destination */
		n = o + dd_coord[d];

		/* Floor is open? */
		if (cave_floor_bold(n.y, n.x))
		{
			/* Go ahead and move */
			do_move = TRUE;
		}

		/* Permanent wall */
		else if (cave_feat[n.y][n.x] >= FEAT_PERM_EXTRA)
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
			cave_info[n.y][n.x] &= ~(CAVE_MARK);

			/* Notice */
			cave_set_feat(n.y, n.x, FEAT_FLOOR);

			/* Note changes to viewable region */
			if (player_has_los_bold(n.y, n.x)) do_view = TRUE;
		}

		/* Handle doors and secret doors */
		else if (((cave_feat[n.y][n.x] >= FEAT_DOOR_HEAD) &&
		          (cave_feat[n.y][n.x] <= FEAT_DOOR_TAIL)) ||
		         (cave_feat[n.y][n.x] == FEAT_SECRET))
		{
			bool may_bash = TRUE;

			/* Take a turn */
			do_turn = TRUE;

			/* Creature can open doors. */
			if (r_ptr->flags2 & (RF2_OPEN_DOOR))
			{
				/* Closed doors and secret doors */
				if ((cave_feat[n.y][n.x] == FEAT_DOOR_HEAD) ||
				    (cave_feat[n.y][n.x] == FEAT_SECRET))
				{
					/* The door is open */
					did_open_door = TRUE;

					/* Do not bash the door */
					may_bash = FALSE;
				}

				/* Locked doors (not jammed) */
				else if (cave_feat[n.y][n.x] < FEAT_DOOR_HEAD + 0x08)
				{
					int k;

					/* Door power */
					k = ((cave_feat[n.y][n.x] - FEAT_DOOR_HEAD) & 0x07);

					/* Try to unlock it XXX XXX XXX */
					if (rand_int(m_ptr->hp / 10) > k)
					{
						/* Unlock the door */
						cave_set_feat(n.y, n.x, FEAT_DOOR_HEAD + 0x00);

						/* Do not bash the door */
						may_bash = FALSE;
					}
				}
			}

			/* Stuck doors -- attempt to bash them down if allowed */
			if (may_bash && (r_ptr->flags2 & (RF2_BASH_DOOR)))
			{
				int k;

				/* Door power */
				k = ((cave_feat[n.y][n.x] - FEAT_DOOR_HEAD) & 0x07);

				/* Attempt to Bash XXX XXX XXX */
				if (rand_int(m_ptr->hp / 10) > k)
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
				if (did_bash_door && one_in_(2))
				{
					cave_set_feat(n.y, n.x, FEAT_BROKEN);
				}

				/* Open the door */
				else
				{
					cave_set_feat(n.y, n.x, FEAT_OPEN);
				}

				/* Handle viewable doors */
				if (player_has_los_bold(n.y, n.x)) do_view = TRUE;
			}
		}


		/* Hack -- check for Glyph of Warding */
		if (do_move && (cave_feat[n.y][n.x] == FEAT_GLYPH))
		{
			/* Assume no move allowed */
			do_move = FALSE;

			/* Break the ward */
			if (randint(BREAK_GLYPH) < r_ptr->level)
			{
				/* Describe observable breakage */
				if (cave_info[n.y][n.x] & (CAVE_MARK))
				{
					msg_print("The rune of protection is broken!");
				}

				/* Forget the rune */
				cave_info[n.y][n.x] &= ~(CAVE_MARK);

				/* Break the rune */
				cave_set_feat(n.y, n.x, FEAT_FLOOR);

				/* Allow movement */
				do_move = TRUE;
			}
		}

		/* Some monsters never attack */
		if (do_move && (cave_m_idx[n.y][n.x] < 0) &&
		    (r_ptr->flags1 & (RF1_NEVER_BLOW)))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) l_ptr->flags1 |= (RF1_NEVER_BLOW);

			/* Do not move */
			do_move = FALSE;
		}

/* Zaiband modification target.  This is where immobile jellies and molds destroy things.
**	Silver jelly: ...:EAT_LITE:...; NEVER_MOVE; STUPID => reduces torches and lanterns to 0
turns of light
**	Blue jelly: ...:COLD:...; NEVER_MOVE; STUPID => destroys cold-hating items
**	Green jelly: ...:ACID:...; NEVER_MOVE; STUPID => destroys acid-hating items
**	Red mold: ...:FIRE:...; NEVER_MOVE; STUPID => destroys fire-hating items
**	Rot jelly: ...:EAT_FOOD:...; NEVER_MOVE; STUPID => destroys food, if not organic items
**	Shimmering mold: ...:ELEC:...; NEVER_MOVE; STUPID => destroys electricity-hating items
Range of effect: Objects in square are automatically blasted.
NOTE: Vulnerable items must be moved away in initial generation.
 */
		if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) && (r_ptr->flags2 & (RF2_STUPID)))
			{
			if (monster_has_attack(r_ptr,RBE_EAT_FOOD))
				{
				(void)destroy_items_selectively(cave_o_idx[o.y][o.x],m_ptr,player_has_los_bold(o.y, o.x),
										  NULL,
										  "%^s spoils %s.",
										  spoil_food_ok,NULL);
				}
			if (monster_has_attack(r_ptr,RBE_EAT_LITE))
				{
				(void)destroy_items_selectively(cave_o_idx[o.y][o.x],m_ptr,player_has_los_bold(o.y, o.x),
										  NULL,
										  "%^s unfuels %s.",
										  eat_lite_ok,eat_lite_helper);	/* code these two */
				}
			if (monster_has_attack(r_ptr,RBE_ACID))
				{
				(void)destroy_items_selectively(cave_o_idx[o.y][o.x],m_ptr,player_has_los_bold(o.y, o.x),
										  NULL,
										  "%^s dissolves %s.",
										  set_acid_destroy,NULL);
				}
			if (monster_has_attack(r_ptr,RBE_ELEC))
				{
				(void)destroy_items_selectively(cave_o_idx[o.y][o.x],m_ptr,player_has_los_bold(o.y, o.x),
										  NULL,
										  "%^s causes %s to explode.",
										  set_elec_destroy,NULL);
				}
			if (monster_has_attack(r_ptr,RBE_FIRE))
				{
				(void)destroy_items_selectively(cave_o_idx[o.y][o.x],m_ptr,player_has_los_bold(o.y, o.x),
										  NULL,
										  "%^s incinerates %s.",
										  set_fire_destroy,NULL);
				}
			if (monster_has_attack(r_ptr,RBE_COLD))
				{
				(void)destroy_items_selectively(cave_o_idx[o.y][o.x],m_ptr,player_has_los_bold(o.y, o.x),
										  NULL,
										  "%^s shatters %s.",
										  set_cold_destroy,NULL);
				}
			}

		/* The player is in the way.  Attack him. */
		if (do_move && (cave_m_idx[n.y][n.x] < 0))
		{
			/* Do the attack */
			(void)make_attack_normal(m_idx);

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

			/* Do physical ranged attacks */
			do_turn = make_attack_ranged_physical(m_idx);
		}

		/* A monster is in the way */
		if (do_move && (cave_m_idx[n.y][n.x] > 0))
		{
			monster_type *n_ptr = &mon_list[cave_m_idx[n.y][n.x]];

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

				/* Message XXX XXX XXX */

				/* Kill the monster */
				delete_monster(n.y, n.x);
			}
		}

		/* A monster is in the way */
		if (do_move && (cave_m_idx[n.y][n.x] > 0))
		{
			monster_type *n_ptr = &mon_list[cave_m_idx[n.y][n.x]];

			/* Assume no movement */
			do_move = FALSE;

			/* Push past weaker monsters (unless leaving a wall) */
			if ((r_ptr->flags2 & (RF2_MOVE_BODY)) &&
			    (compare_monsters(m_ptr, n_ptr) > 0) &&
			    (cave_floor_bold(m_ptr->loc.y, m_ptr->loc.x)))
			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster pushed past another monster */
				did_move_body = TRUE;

				/* XXX XXX XXX Message */
			}
		}


		/* Creature has been allowed move */
		if (do_move)
		{
			/* Take a turn */
			do_turn = TRUE;

			/* Move the monster */
			monster_swap(o, n);

			/* Possible disturb */
			if (m_ptr->ml &&
			    (disturb_move ||
			     ((m_ptr->mflag & (MFLAG_VIEW)) &&
			      disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}


			/* Start ZaiBand modification */
			/* If something attacks to eat food, it should eat food from the floor! */
			/* this condition must reflect the attack type's existence */
			if (monster_has_attack(r_ptr,RBE_EAT_FOOD))
				{	/* Scan all objects in the grid */
				(void)destroy_items_selectively(cave_o_idx[n.y][n.x],m_ptr,player_has_los_bold(n.y, n.x),
										  "%^s tries to eat %s, but fails.",
										  "%^s eats %s.",
										  eat_food_floor_ok,eat_food_floor_helper);
				}
			/* End ZaiBand modification */

			/* ZaiBand: no change to code effect, but restructured to take advantage 
			 * of meta-code.  [well...you *must* see it done before you know it.]
			 * META: there should be a reality check prohibiting both RF2_KILL_ITEM 
			 * and RF2_TAKE_ITEM in the same r_ptr->flags2
			 */
			if 		(r_ptr->flags2 & (RF2_KILL_ITEM))
				{
				did_kill_item = destroy_items_selectively(cave_o_idx[n.y][n.x],m_ptr,player_has_los_bold(n.y, n.x),
										  NULL,
										  "%^s crushes %s.",
										  non_money_ok,NULL);
				}
			else if (r_ptr->flags2 & (RF2_TAKE_ITEM))
				{
				did_take_item = destroy_items_selectively(cave_o_idx[n.y][n.x],m_ptr,player_has_los_bold(n.y, n.x),
										  "%^s tries to pick up %s, but fails.",
										  "%^s picks up %s.",
										  non_money_ok,take_item_helper);
				}
			/* End ZaiBand modification */
		}

		/* Stop when done */
		if (do_turn) break;
	}


	/* If we haven't done anything, try casting a spell again */
	if (smart_monsters && !do_turn && !do_move)
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

		/* Monster destroyed a wall */
		if (did_kill_wall) l_ptr->flags2 |= (RF2_KILL_WALL);
	}


	/* Hack -- get "bold" if out of options */
	if (!do_turn && !do_move && m_ptr->monfear)
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
			msg_format("%^s turns to fight!", m_name);
		}

		/* XXX XXX XXX Actually do something now (?) */
	}
}


static bool monster_can_flow(int m_idx)
{
	/* Hack -- Monsters can "smell" the player from far away */
	if (flow_by_sound)
	{
		monster_type *m_ptr = &mon_list[m_idx];
		monster_race *r_ptr = m_ptr->race();

		/* Monster location */
		int fy = m_ptr->loc.y;
		int fx = m_ptr->loc.x;

		/* Check the flow (normal aaf is about 20) */
		if ((cave_when[fy][fx] == cave_when[p_ptr->loc.y][p_ptr->loc.x]) &&
		    (cave_cost[fy][fx] < MONSTER_FLOW_DEPTH) &&
		    (cave_cost[fy][fx] < r_ptr->aaf))
		{
			return TRUE;
		}
	}

	return FALSE;
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
 * Note the special "MFLAG_NICE" flag, which prevents "nasty" monsters from
 * using any of their spell attacks until the player gets a turn.  This flag
 * is optimized via the "repair_mflag_nice" flag.
 */
void process_monsters(byte minimum_energy)
{
	int i;

	/* Process the monsters (backwards) */
	for (i = mon_max - 1; i >= 1; i--)
	{
		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		{ /* C-ish blocking brace */
		/* Get the monster */
		monster_type* m_ptr = &mon_list[i];

		if (!m_ptr->r_idx) continue;					/* Ignore "dead" monsters */
		if (m_ptr->energy < minimum_energy) continue;	/* Not enough energy to move */

		/* Use up "some" energy */
		m_ptr->energy -= 100;


		/* Heal monster? XXX XXX XXX */


		{ /* C-ish blocking brace */
		/* Get the race */
		monster_race* r_ptr = m_ptr->race();

		/*
		 * Process the monster if the monster either:
		 * - can "sense" the player
		 * - is hurt
		 * - can "see" the player (checked backwards)
		 * - is already awake and has either full gaze or clairvoyant gaze [ZaiBand]
		 * - can "smell" the player from far away (flow)
		 */
		if ((m_ptr->cdis <= r_ptr->aaf) ||
		    (m_ptr->hp < m_ptr->maxhp) ||
		    player_has_los_bold(m_ptr->loc.y, m_ptr->loc.x) ||
			(!m_ptr->csleep && (monster_has_attack(r_ptr,RBM_CLAIRVOYANT_GAZE) || monster_has_attack(r_ptr,RBM_RANGED_GAZE)) && MAX_RANGE<distance(m_ptr->loc.y,m_ptr->loc.x,p_ptr->loc.y,p_ptr->loc.x)) ||
		    monster_can_flow(i))
		{
			/* Process the monster */
			process_monster(i);
		}
		}	/* end blocking brace */
		}	/* end blocking brace */
	}
}
