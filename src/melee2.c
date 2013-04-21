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
 * Original idea and code by "DRS" (David Reeves Sward).
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
 * Internal probablility routine
 */
static bool int_outof(monster_race * r_ptr, int prob)
{
	/* Non-Smart monsters are half as "smart" */
	if (!(r_ptr->flags2 & (RF2_SMART)))
		prob = prob / 2;

	/* Roll the dice */
	return (rand_int(100) < prob);
}



/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(int m_idx, u32b * f4p, u32b * f5p,
	u32b * f6p)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);

	u32b smart = 0L;


	/* Too stupid to know anything */
	if (r_ptr->flags2 & (RF2_STUPID))
		return;

	/* Pets don't know about other monsters. */
	if (m_ptr->is_pet)
		return;

	/* Must be cheating or learning */
	if (!smart_cheat && !smart_learn)
		return;


	/* Update acquired knowledge */
	if (smart_learn)
	{
		/* Hack -- Occasionally forget player status */
		if (m_ptr->smart && (rand_int(100) < 1))
			m_ptr->smart = 0L;

		/* Use the memorized flags */
		smart = m_ptr->smart;
	}


	/* Cheat if requested */
	if (smart_cheat)
	{
		/* Know weirdness */
		if (p_ptr->free_act)
			smart |= (SM_IMM_FREE);
		if (!p_ptr->msp)
			smart |= (SM_IMM_MANA);

		/* Know immunities */
		if (p_ptr->immune_acid)
			smart |= (SM_IMM_ACID);
		if (p_ptr->immune_elec)
			smart |= (SM_IMM_ELEC);
		if (p_ptr->immune_fire)
			smart |= (SM_IMM_FIRE);
		if (p_ptr->immune_cold)
			smart |= (SM_IMM_COLD);

		/* Know oppositions */
		if (p_ptr->oppose_acid)
			smart |= (SM_OPP_ACID);
		if (p_ptr->oppose_elec)
			smart |= (SM_OPP_ELEC);
		if (p_ptr->oppose_fire)
			smart |= (SM_OPP_FIRE);
		if (p_ptr->oppose_cold)
			smart |= (SM_OPP_COLD);
		if (p_ptr->oppose_pois)
			smart |= (SM_OPP_POIS);

		/* Know resistances */
		if (p_ptr->resist_acid)
			smart |= (SM_RES_ACID);
		if (p_ptr->resist_elec)
			smart |= (SM_RES_ELEC);
		if (p_ptr->resist_fire)
			smart |= (SM_RES_FIRE);
		if (p_ptr->resist_cold)
			smart |= (SM_RES_COLD);
		if (p_ptr->resist_pois)
			smart |= (SM_RES_POIS);
		if (p_ptr->resist_fear)
			smart |= (SM_RES_FEAR);
		if (p_ptr->resist_lite)
			smart |= (SM_RES_LITE);
		if (p_ptr->resist_dark)
			smart |= (SM_RES_DARK);
		if (p_ptr->resist_blind)
			smart |= (SM_RES_BLIND);
		if (p_ptr->resist_confu)
			smart |= (SM_RES_CONFU);
		if (p_ptr->resist_sound)
			smart |= (SM_RES_SOUND);
		if (p_ptr->resist_shard)
			smart |= (SM_RES_SHARD);
		if (p_ptr->resist_nexus)
			smart |= (SM_RES_NEXUS);
		if (p_ptr->resist_nethr)
			smart |= (SM_RES_NETHR);
		if (p_ptr->resist_chaos)
			smart |= (SM_RES_CHAOS);
		if (p_ptr->resist_disen)
			smart |= (SM_RES_DISEN);
	}


	/* Nothing known */
	if (!smart)
		return;


	if (smart & (SM_IMM_ACID))
	{
		if (int_outof(r_ptr, 100))
			f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BO_ACID);
	}
	else if ((smart & (SM_OPP_ACID)) && (smart & (SM_RES_ACID)))
	{
		if (int_outof(r_ptr, 80))
			f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BO_ACID);
	}
	else if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID)))
	{
		if (int_outof(r_ptr, 30))
			f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BO_ACID);
	}


	if (smart & (SM_IMM_ELEC))
	{
		if (int_outof(r_ptr, 100))
			f4 &= ~(RF4_BR_ELEC);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BA_ELEC);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BO_ELEC);
	}
	else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC)))
	{
		if (int_outof(r_ptr, 80))
			f4 &= ~(RF4_BR_ELEC);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BA_ELEC);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BO_ELEC);
	}
	else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC)))
	{
		if (int_outof(r_ptr, 30))
			f4 &= ~(RF4_BR_ELEC);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BA_ELEC);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BO_ELEC);
	}


	if (smart & (SM_IMM_FIRE))
	{
		if (int_outof(r_ptr, 100))
			f4 &= ~(RF4_BR_FIRE);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BA_FIRE);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BO_FIRE);
	}
	else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE)))
	{
		if (int_outof(r_ptr, 80))
			f4 &= ~(RF4_BR_FIRE);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BA_FIRE);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BO_FIRE);
	}
	else if ((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE)))
	{
		if (int_outof(r_ptr, 30))
			f4 &= ~(RF4_BR_FIRE);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BA_FIRE);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BO_FIRE);
	}


	if (smart & (SM_IMM_COLD))
	{
		if (int_outof(r_ptr, 100))
			f4 &= ~(RF4_BR_COLD);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BA_COLD);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BO_ICEE);
	}
	else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD)))
	{
		if (int_outof(r_ptr, 80))
			f4 &= ~(RF4_BR_COLD);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BA_COLD);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BO_ICEE);
	}
	else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)))
	{
		if (int_outof(r_ptr, 30))
			f4 &= ~(RF4_BR_COLD);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BA_COLD);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BO_ICEE);
	}


	if ((smart & (SM_OPP_POIS)) && (smart & (SM_RES_POIS)))
	{
		if (int_outof(r_ptr, 80))
			f4 &= ~(RF4_BR_POIS);
		if (int_outof(r_ptr, 80))
			f5 &= ~(RF5_BA_POIS);
	}
	else if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS)))
	{
		if (int_outof(r_ptr, 30))
			f4 &= ~(RF4_BR_POIS);
		if (int_outof(r_ptr, 30))
			f5 &= ~(RF5_BA_POIS);
	}


	if (smart & (SM_RES_FEAR))
	{
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_SCARE);
	}

	if (smart & (SM_RES_LITE))
	{
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_LITE);
	}

	if (smart & (SM_RES_DARK))
	{
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_DARK);
		if (int_outof(r_ptr, 50))
			f5 &= ~(RF5_BA_DARK);
	}

	if (smart & (SM_RES_BLIND))
	{
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_BLIND);
	}

	if (smart & (SM_RES_CONFU))
	{
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_CONF);
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_CONF);
	}

	if (smart & (SM_RES_SOUND))
	{
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_SOUN);
	}

	if (smart & (SM_RES_SHARD))
	{
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_SHAR);
	}

	if (smart & (SM_RES_NEXUS))
	{
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_NEXU);
		if (int_outof(r_ptr, 50))
			f6 &= ~(RF6_TELE_LEVEL);
	}

	if (smart & (SM_RES_NETHR))
	{
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_NETH);
		if (int_outof(r_ptr, 50))
			f5 &= ~(RF5_BA_NETH);
		if (int_outof(r_ptr, 50))
			f5 &= ~(RF5_BO_NETH);
	}

	if (smart & (SM_RES_CHAOS))
	{
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_CONF);
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_CONF);
		if (int_outof(r_ptr, 50))
			f4 &= ~(RF4_BR_CHAO);
	}

	if (smart & (SM_RES_DISEN))
	{
		if (int_outof(r_ptr, 100))
			f4 &= ~(RF4_BR_DISE);
	}


	if (smart & (SM_IMM_FREE))
	{
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_HOLD);
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_SLOW);
	}

	if (smart & (SM_IMM_MANA))
	{
		if (int_outof(r_ptr, 100))
			f5 &= ~(RF5_DRAIN_MANA);
	}


	/* XXX XXX XXX No spells left? */
	/* if (!f4 && !f5 && !f6) ... */


	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
}


#endif


/*
 * Find closest target. This function looks through all the
 * monsters and tries to find the closest one that is of opposite
 * pet status. Note that `line-of-sight' between monsters is not checked.
 */

static void find_target_nearest(monster_type * m_ptr, monster_race * r_ptr,
	int *ry, int *rx)
{
	int i, j;
	int sx = m_ptr->fx;
	int sy = m_ptr->fy;

	monster_type *o_m_ptr;
	monster_race *o_r_ptr;
	monster_type *min_m_ptr = NULL;
	int min_dist = r_ptr->aaf;

	int gooddude = (p_ptr->sc >= 0) && (r_ptr->flags3 & RF3_POLICE);


	/* Default target */

	*ry = p_ptr->py;
	*rx = p_ptr->px;

	/* Either the player can be seen, or there are no pets to track */

	if (m_ptr->is_pet == FALSE && !gooddude &&
	    (player_has_los_bold(m_ptr->fy, m_ptr->fx) || m_pet_num == 0))
	{
		return;
	}

	/* Call pets. */
	if (m_ptr->is_pet == TRUE && p_ptr->pets_notice)
	{

		if (p_ptr->pets_notice == 2 && target_okay())
		{
			*ry = p_ptr->target_row;
			*rx = p_ptr->target_col;
		}

		return;
	}

	/* Paranoia */
	if (sy < 0 || sy >= DUNGEON_WID || sx < 0 || sy >= DUNGEON_HGT)
	{
		return;
	}

	/* Process the monsters, backwards */
	for (i = m_max - 1; i >= 1; i--)
	{
		o_m_ptr = &m_list[i];

		if (!o_m_ptr->r_idx)
			continue;

		o_r_ptr = &r_info[o_m_ptr->r_idx];

		if (o_m_ptr->is_pet != m_ptr->is_pet && 
		    !(m_ptr->is_pet &&
		      o_r_ptr->flags3 & RF3_FRIENDLY))
		{
			j = distance(o_m_ptr->fy, o_m_ptr->fx, sy, sx);

			if (j < min_dist)
			{
				min_dist = j;
				min_m_ptr = o_m_ptr;
			}
		}
	}

	/* Do we have a match? */

	if (min_m_ptr)
	{
		*ry = min_m_ptr->fy;
		*rx = min_m_ptr->fx;
	}

	/* HACK! Handle police. */
	if (gooddude && *rx == p_ptr->px && *ry == p_ptr->py) {

	  /* Track the player without damaging him. */
	  
	  while (1) {
	    *rx += rand_range(-9, 9);
	    *ry += rand_range(-9, 9);

	    if (*rx != p_ptr->px && *ry != p_ptr->py) {
	      break;
	    }
	  }
	}
}


/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 * Make sure any square can be targetted.
 * Don't fire if a monster is in the way.
 */
static void bolt(int m_idx, int py, int px, int typ, int dam_hp)
{

	int flg = PROJECT_STOP | PROJECT_KILL;

	/* Target the player with a bolt attack */
	(void) project(m_idx, 0, py, px, dam_hp, typ, flg);
}


/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 * Make sure any square can be targetted.
 * Don't fire if a monster is in the way.
 */
static void breath(int m_idx, int py, int px, int typ, int dam_hp)
{
	int rad;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_STOP;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

	/* Target the player with a ball attack */
	(void) project(m_idx, rad, py, px, dam_hp, typ, flg);
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
	int py;
	int px;

	int k, chance, thrown_spell, rlev;

	byte spell[96], num = 0;

	u32b f4, f5, f6;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char m_name[80];
	char m_poss[80];

	char ddesc[80];


	/* Target player */
	int x;
	int y;


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

	/* Flag to check wheather it's OK to shoot */
	bool clear_shot;

	/* General targetting code. */
	find_target_nearest(m_ptr, r_ptr, &py, &px);

	if (m_ptr->is_pet)
	{
		if (py == p_ptr->py && px == p_ptr->px)
			return FALSE;

		direct = FALSE;
	}
	else
	{
		if (py != p_ptr->py && px != p_ptr->px)
			direct = FALSE;
	}

	x = px;
	y = py;

	clear_shot = target_clear(m_ptr, py, px);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* ``Smart'' monsters speak insults. */
	if (direct && m_ptr->ml && magik(20) && monsters_speak)
	{
		cptr insult = get_monster_saying(r_ptr);

		if (insult)
		{
			msg_format("%^s %s", m_name, insult);
		}
	}

	/* Cannot cast spells when confused */
	if (m_ptr->confused)
		return (FALSE);

	/* Cannot cast spells when nice */
	if (m_ptr->mflag & (MFLAG_NICE))
		return (FALSE);

	/* Hack -- Extract the spell probability */
	chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance)
		return (FALSE);

	/* Only do spells occasionally */
	if (rand_int(100) >= chance)
		return (FALSE);


	/* XXX XXX XXX Handle "track_target" option (?) */


	/* Hack -- require projectable player */
	if (normal)
	{
		/* Check range */
		if (m_ptr->cdis > MAX_RANGE)
			return (FALSE);

		/* Check path */
		if (!projectable(m_ptr->fy, m_ptr->fx, py, px))
			return (FALSE);
	}


	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;


	/* Hack -- allow "desperate" spells */
	if ((r_ptr->flags2 & (RF2_SMART)) && (m_ptr->hp < m_ptr->maxhp / 10) &&
		(rand_int(100) < 50))
	{
		/* Require intelligent spells */
		f4 &= (RF4_INT_MASK);
		f5 &= (RF5_INT_MASK);
		f6 &= (RF6_INT_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6)
			return (FALSE);
	}


#ifdef DRS_SMART_OPTIONS

	/* Remove the "ineffective" spells */
	remove_bad_spells(m_idx, &f4, &f5, &f6);

	/* No spells left */
	if (!f4 && !f5 && !f6)
		return (FALSE);

#endif


	/* Extract the "inate" spells */
	for (k = 0; k < 32; k++)
	{
		if (f4 & (1L << k))
			spell[num++] = k + 32 * 3;
	}

	/* Extract the "normal" spells */
	for (k = 0; k < 32; k++)
	{
		if (f5 & (1L << k))
			spell[num++] = k + 32 * 4;
	}

	/* Extract the "bizarre" spells */
	for (k = 0; k < 32; k++)
	{
		if (f6 & (1L << k))
			spell[num++] = k + 32 * 5;
	}

	/* No spells left */
	if (!num)
		return (FALSE);


	/* Handle "leaving" */
	if (p_ptr->leaving)
		return (FALSE);


	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, 0x88);


	/* Choose a spell to cast */
	thrown_spell = spell[rand_int(num)];

	if (!clear_shot && thrown_spell < 32 * 5 &&
		!(r_ptr->flags2 & RF2_STUPID)) return FALSE;


	/* Cast the spell. */
	switch (thrown_spell)
	{
			/* RF4_SHRIEK */
		case 96 + 0:
		{
			if (!direct)
				break;
			disturb(1, 0);
			msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx);
			break;
		}

			/* RF4_XXX2X4 */
		case 96 + 1:
		{
			break;
		}

			/* RF4_XXX3X4 */
		case 96 + 2:
		{
			break;
		}

			/* RF4_XXX4X4 */
		case 96 + 3:
		{
			break;
		}

			/* RF4_ARROW_1 */
		case 96 + 4:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s makes a strange noise.", m_name);
			else
				msg_format("%^s fires an arrow.", m_name);
			bolt(m_idx, py, px, GF_ARROW, damroll(1, 6));
			break;
		}

			/* RF4_ARROW_2 */
		case 96 + 5:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s makes a strange noise.", m_name);
			else
				msg_format("%^s fires an arrow!", m_name);
			bolt(m_idx, py, px, GF_ARROW, damroll(3, 6));
			break;
		}

			/* RF4_ARROW_3 */
		case 96 + 6:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s makes a strange noise.", m_name);
			else
				msg_format("%^s fires a missile.", m_name);
			bolt(m_idx, py, px, GF_ARROW, damroll(5, 6));
			break;
		}

			/* RF4_ARROW_4 */
		case 96 + 7:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s makes a strange noise.", m_name);
			else
				msg_format("%^s fires a missile!", m_name);
			bolt(m_idx, py, px, GF_ARROW, damroll(7, 6));
			break;
		}

			/* RF4_BR_ACID */
		case 96 + 8:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes acid.", m_name);
			breath(m_idx, py, px, GF_ACID,
				((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

			/* RF4_BR_ELEC */
		case 96 + 9:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes lightning.", m_name);
			breath(m_idx, py, px, GF_ELEC,
				((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

			/* RF4_BR_FIRE */
		case 96 + 10:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes fire.", m_name);
			breath(m_idx, py, px, GF_FIRE,
				((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

			/* RF4_BR_COLD */
		case 96 + 11:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes frost.", m_name);
			breath(m_idx, py, px, GF_COLD,
				((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

			/* RF4_BR_POIS */
		case 96 + 12:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes gas.", m_name);
			breath(m_idx, py, px, GF_POIS,
				((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

			/* RF4_BR_NETH */
		case 96 + 13:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes nether.", m_name);
			breath(m_idx, py, px, GF_NETHER,
				((m_ptr->hp / 6) > 550 ? 550 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

			/* RF4_BR_LITE */
		case 96 + 14:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes light.", m_name);
			breath(m_idx, py, px, GF_LITE,
				((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_LITE);
			break;
		}

			/* RF4_BR_DARK */
		case 96 + 15:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes darkness.", m_name);
			breath(m_idx, py, px, GF_DARK,
				((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_DARK);
			break;
		}

			/* RF4_BR_CONF */
		case 96 + 16:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes confusion.", m_name);
			breath(m_idx, py, px, GF_CONFUSION,
				((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_CONFU);
			break;
		}

			/* RF4_BR_SOUN */
		case 96 + 17:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes sound.", m_name);
			breath(m_idx, py, px, GF_SOUND,
				((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_SOUND);
			break;
		}

			/* RF4_BR_CHAO */
		case 96 + 18:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes chaos.", m_name);
			breath(m_idx, py, px, GF_CHAOS,
				((m_ptr->hp / 6) > 600 ? 600 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_CHAOS);
			break;
		}

			/* RF4_BR_DISE */
		case 96 + 19:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes disenchantment.", m_name);
			breath(m_idx, py, px, GF_DISENCHANT,
				((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_DISEN);
			break;
		}

			/* RF4_BR_NEXU */
		case 96 + 20:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes nexus.", m_name);
			breath(m_idx, py, px, GF_NEXUS,
				((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)));
			update_smart_learn(m_idx, DRS_RES_NEXUS);
			break;
		}

			/* RF4_BR_TIME */
		case 96 + 21:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes time.", m_name);
			breath(m_idx, py, px, GF_TIME,
				((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3)));
			break;
		}

			/* RF4_BR_INER */
		case 96 + 22:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes inertia.", m_name);
			breath(m_idx, py, px, GF_INERTIA,
				((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)));
			break;
		}

			/* RF4_BR_GRAV */
		case 96 + 23:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes gravity.", m_name);
			breath(m_idx, py, px, GF_GRAVITY,
				((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)));
			break;
		}

			/* RF4_BR_SHAR */
		case 96 + 24:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes shards.", m_name);
			breath(m_idx, py, px, GF_SHARD,
				((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
			update_smart_learn(m_idx, DRS_RES_SHARD);
			break;
		}

			/* RF4_BR_PLAS */
		case 96 + 25:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes plasma.", m_name);
			breath(m_idx, py, px, GF_PLASMA,
				((m_ptr->hp / 6) > 150 ? 150 : (m_ptr->hp / 6)));
			break;
		}

			/* RF4_BR_WALL */
		case 96 + 26:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes force.", m_name);
			breath(m_idx, py, px, GF_FORCE,
				((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)));
			break;
		}

			/* RF4_BR_MANA */
		case 96 + 27:
		{
			/* XXX XXX XXX */
			break;
		}

			/* RF4_BR_QUAKE */
		case 96 + 28:
		{
			disturb(1, 0);
			msg_format("%^s warps space-time!", m_name);
			breath(m_idx, py, px, GF_QUAKE,
				((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)));
			break;
		}

			/* RF4_XXX6X4 */
		case 96 + 29:
		{
			break;
		}

			/* RF4_XXX7X4 */
		case 96 + 30:
		{
			break;
		}

			/* RF4_XXX8X4 */
		case 96 + 31:
		{
			break;
		}



			/* RF5_BA_ACID */
		case 128 + 0:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts an acid ball.", m_name);
			breath(m_idx, py, px, GF_ACID, randint(rlev * 3) + 15);
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

			/* RF5_BA_ELEC */
		case 128 + 1:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a lightning ball.", m_name);
			breath(m_idx, py, px, GF_ELEC, randint(rlev * 3 / 2) + 8);
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

			/* RF5_BA_FIRE */
		case 128 + 2:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a fire ball.", m_name);
			breath(m_idx, py, px, GF_FIRE, randint(rlev * 7 / 2) + 10);
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

			/* RF5_BA_COLD */
		case 128 + 3:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a frost ball.", m_name);
			breath(m_idx, py, px, GF_COLD, randint(rlev * 3 / 2) + 10);
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

			/* RF5_BA_POIS */
		case 128 + 4:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a stinking cloud.", m_name);
			breath(m_idx, py, px, GF_POIS, damroll(12, 2));
			update_smart_learn(m_idx, DRS_RES_POIS);
			break;
		}

			/* RF5_BA_NETH */
		case 128 + 5:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a nether ball.", m_name);
			breath(m_idx, py, px, GF_NETHER, (50 + damroll(10,
						10) + rlev));
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

			/* RF5_BA_WATE */
		case 128 + 6:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s gestures fluidly.", m_name);
			msg_format("%^s invokes a massive whirlpool.", m_name);
			breath(m_idx, py, px, GF_WATER, randint(rlev * 5 / 2) + 50);
			break;
		}

			/* RF5_BA_MANA */
		case 128 + 7:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles powerfully.", m_name);
			else
				msg_format("%^s invokes a mana storm.", m_name);
			breath(m_idx, py, px, GF_MANA, (rlev * 5) + damroll(10, 10));
			break;
		}

			/* RF5_BA_DARK */
		case 128 + 8:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles powerfully.", m_name);
			else
				msg_format("%^s invokes a darkness storm.", m_name);
			breath(m_idx, py, px, GF_DARK, (rlev * 5) + damroll(10, 10));
			update_smart_learn(m_idx, DRS_RES_DARK);
			break;
		}

			/* RF5_DRAIN_MANA */
		case 128 + 9:
		{
			if (!direct)
				break;

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
				p_ptr->window |= (PW_SPELL | PW_PLAYER);

				/* Heal the monster */
				if (m_ptr->hp < m_ptr->maxhp)
				{
					/* Heal */
					m_ptr->hp += (6 * r1);
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
			update_smart_learn(m_idx, DRS_MANA);
			break;
		}

			/* RF5_MIND_BLAST */
		case 128 + 10:
		{
			disturb(1, 0);
			if (blind)
				msg_format("You feel an intense psychic disturbance.",
					m_name);
			else
				msg_format("%^s lets out a blast of psionic energy!",
					m_name);
			bolt(m_idx, py, px, GF_MIND_BLAST, damroll(8, 8));
			break;
		}

			/* RF5_BRAIN_SMASH */
		case 128 + 11:
		{
			disturb(1, 0);
			if (blind)
				msg_format("You feel an intense psychic disturbance.",
					m_name);
			else
				msg_format("%^s lets loose a mind-melting blast!", m_name);
			bolt(m_idx, py, px, GF_BRAIN_SMASH, damroll(12, 15));
			break;
		}

			/* RF5_CAUSE_1 */
		case 128 + 12:
		{
			disturb(1, 0);
			if (blind)
				msg_format("Something mumbles.", m_name);
			else
				msg_format("%^s points and curses.", m_name);
			bolt(m_idx, py, px, GF_DISP_ALL, damroll(3, 8));
			break;
		}

			/* RF5_CAUSE_2 */

		case 128 + 13:
		{
			disturb(1, 0);
			if (blind)
				msg_format("Something mumbles.", m_name);
			else
				msg_format("%^s points and curses horribly!", m_name);
			bolt(m_idx, py, px, GF_DISP_ALL, damroll(8, 8));
			break;
		}

			/* RF5_CAUSE_3 */

		case 128 + 14:
		{
			disturb(1, 0);
			if (blind)
				msg_format("Something mumbles loudly.", m_name);
			else
				msg_format("%^s points, incanting terribly!", m_name);
			bolt(m_idx, py, px, GF_DISP_ALL, damroll(10, 15));
			break;
		}

			/* RF5_CAUSE_4 */
		case 128 + 15:
		{
			disturb(1, 0);
			if (blind)
				msg_format("Something screams the word 'DIE!'.", m_name);
			else
				msg_format("%^s points, screaming the word 'DIE'!",
					m_name);
			bolt(m_idx, py, px, GF_DISP_ALL, damroll(15, 15));
			break;
		}

			/* RF5_BO_ACID */
		case 128 + 16:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a acid bolt.", m_name);
			bolt(m_idx, py, px, GF_ACID, damroll(7, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_ACID);
			break;
		}

			/* RF5_BO_ELEC */
		case 128 + 17:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a lightning bolt.", m_name);
			bolt(m_idx, py, px, GF_ELEC, damroll(4, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_ELEC);
			break;
		}

			/* RF5_BO_FIRE */
		case 128 + 18:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a fire bolt.", m_name);
			bolt(m_idx, py, px, GF_FIRE, damroll(9, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_FIRE);
			break;
		}

			/* RF5_BO_COLD */
		case 128 + 19:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a frost bolt.", m_name);
			bolt(m_idx, py, px, GF_COLD, damroll(6, 8) + (rlev / 3));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

			/* RF5_BO_POIS */
		case 128 + 20:
		{
			/* XXX XXX XXX */
			break;
		}

			/* RF5_BO_NETH */
		case 128 + 21:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a nether bolt.", m_name);
			bolt(m_idx, py, px, GF_NETHER, 30 + damroll(5,
					5) + (rlev * 3) / 2);
			update_smart_learn(m_idx, DRS_RES_NETHR);
			break;
		}

			/* RF5_BO_WATE */
		case 128 + 22:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a water bolt.", m_name);
			bolt(m_idx, py, px, GF_WATER, damroll(10, 10) + (rlev));
			break;
		}

			/* RF5_BO_MANA */
		case 128 + 23:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a mana bolt.", m_name);
			bolt(m_idx, py, px, GF_MANA, randint(rlev * 7 / 2) + 50);
			break;
		}

			/* RF5_BO_PLAS */
		case 128 + 24:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a plasma bolt.", m_name);
			bolt(m_idx, py, px, GF_PLASMA, 10 + damroll(8, 7) + (rlev));
			break;
		}

			/* RF5_BO_ICEE */
		case 128 + 25:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts an ice bolt.", m_name);
			bolt(m_idx, py, px, GF_ICE, damroll(6, 6) + (rlev));
			update_smart_learn(m_idx, DRS_RES_COLD);
			break;
		}

			/* RF5_MISSILE */
		case 128 + 26:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s casts a magic missile.", m_name);
			bolt(m_idx, py, px, GF_MISSILE, damroll(2, 6) + (rlev / 3));
			break;
		}

			/* RF5_SCARE */
		case 128 + 27:
		{
			disturb(1, 0);

			if (blind)
				msg_format("%^s mumbles, and you hear scary noises.",
					m_name);
			else
				msg_format("%^s casts a fearful illusion.", m_name);

			bolt(m_idx, py, px, GF_TURN_ALL, rand_int(4) + 4);
			update_smart_learn(m_idx, DRS_RES_FEAR);
			break;
		}

			/* RF5_BLIND */
		case 128 + 28:
		{
			disturb(1, 0);

			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s lets loose a cloak of darkness!", m_name);
			bolt(m_idx, py, px, GF_DARK, 0);
			update_smart_learn(m_idx, DRS_RES_BLIND);
			break;
		}

			/* RF5_CONF */
		case 128 + 29:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles, and you hear puzzling noises.",
					m_name);
			else
				msg_format("%^s creates a mesmerising illusion.", m_name);
			bolt(m_idx, py, px, GF_CONFUSION, 0);
			update_smart_learn(m_idx, DRS_RES_CONFU);
			break;
		}

			/* RF5_SLOW */
		case 128 + 30:
		{
			disturb(1, 0);
			msg_format("%^s drains power from muscles!", m_name);
			bolt(m_idx, py, px, GF_SLOW, 5);
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}

			/* RF5_HOLD */
		case 128 + 31:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s chants a sonorific tune.", m_name);
			bolt(m_idx, py, px, GF_SLEEP, 0);
			update_smart_learn(m_idx, DRS_FREE);
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
		case 160 + 1:
		{
			break;
		}

			/* RF6_HEAL */
		case 160 + 2:
		{
			disturb(1, 0);

			/* Message */
			if (blind)
			{
				msg_format("%^s mumbles.", m_name);
			}
			else
			{
				msg_format("%^s concentrates on %s wounds.", m_name,
					m_poss);
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

			/* RF6_XXX2X6 */
		case 160 + 3:
		{
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
			break;
		}

			/* RF6_XXX3X6 */
		case 160 + 6:
		{
			break;
		}

			/* RF6_XXX4X6 */
		case 160 + 7:
		{
			break;
		}

			/* RF6_TELE_TO */
		case 160 + 8:
		{
			if (!direct)
				break;
			disturb(1, 0);
			msg_format("%^s commands you to return.", m_name);
			teleport_player_to(m_ptr->fy, m_ptr->fx);
			break;
		}

			/* RF6_TELE_AWAY */
		case 160 + 9:
		{
			if (!direct)
				break;
			disturb(1, 0);
			msg_format("%^s teleports you away.", m_name);
			teleport_player(100);
			break;
		}

			/* RF6_TELE_LEVEL */
		case 160 + 10:
		{
			if (!direct)
				break;
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
			update_smart_learn(m_idx, DRS_RES_NEXUS);
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
			if (!direct)
				break;
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s gestures in shadow.", m_name);
			breath(m_idx, py, px, GF_DARK_WEAK, 0);
			break;
		}

			/* RF6_TRAPS */
		case 160 + 13:
		{
			if (!direct)
				break;
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles, and then cackles evilly.",
					m_name);
			else
				msg_format("%^s casts a spell and cackles evilly.",
					m_name);
			breath(m_idx, py, px, GF_MAKE_TRAP, 0);
			break;
		}

			/* RF6_FORGET */
		case 160 + 14:
		{
			if (!direct)
				break;
			disturb(1, 0);
			msg_format("%^s tries to blank your mind.", m_name);

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else if (lose_all_info())
			{
				mprint(MSG_WARNING, "Your memories fade away.");
			}
			break;
		}

			/* RF6_XXX6X6 */
		case 160 + 15:
		{
		  disturb(1, 0);

		  if (blind) {
		    msg_format("%^s mumbles in the kobold tongue.", m_name);

		  } else {
		    msg_format("%^s summons undead kobolds!", m_name);
		  }

		  for (k = 0; k < 3; k++) {
		    if (m_ptr->is_pet)
		      count +=
			summon_specific_friendly(y, x, rlev, 
						 SUMMON_UNDEAD_KOBOLD);
		    else
		      count += summon_specific(y, x, rlev, 
					       SUMMON_UNDEAD_KOBOLD);
		  }

		  if (blind && count)
		    msg_print("You hear many kobolds appear nearby.");

		  break;
		}

			/* RF6_S_KOBOLD */
		case 160 + 16:
		{
		  disturb(1, 0);

		  if (blind) {
		    msg_format("%^s mumbles in the kobold tongue.", m_name);

		  } else {
		    msg_format("%^s summons kobolds!", m_name);
		  }

		  for (k = 0; k < 5; k++) {
		    if (m_ptr->is_pet)
		      count +=
			summon_specific_friendly(y, x, rlev, SUMMON_KOBOLD);
		    else
		      count += summon_specific(y, x, rlev, SUMMON_KOBOLD);
		  }

		  if (blind && count)
		    msg_print("You hear many kobolds appear nearby.");

		  break;
		}

			/* RF6_S_GOOD_UNIQUE */
		case 160 + 17:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s chants in a strong, clear voice.", m_name);
			else
				msg_format("%^s magically summons help!", m_name);

			for (k = 0; k < 8; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_GOOD_UNIQUE);
				else
					count +=
						summon_specific(y, x, rlev, SUMMON_GOOD_UNIQUE);
			}

			if (blind && count)
				msg_print("You hear many powerful "
					"beings appear nearby.");

			break;
		}

			/* RF6_S_MONSTER */
		case 160 + 18:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons help!", m_name);
			for (k = 0; k < 1; k++)
			{
				if (m_ptr->is_pet)
					count += summon_specific_friendly(y, x, rlev, 0);
				else
					count += summon_specific(y, x, rlev, 0);
			}
			if (blind && count)
				msg_print("You hear something appear nearby.");
			break;
		}

			/* RF6_S_MONSTERS */
		case 160 + 19:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons monsters!", m_name);
			for (k = 0; k < 8; k++)
			{
				if (m_ptr->is_pet)
					count += summon_specific_friendly(y, x, rlev, 0);
				else
					count += summon_specific(y, x, rlev, 0);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF6_S_ANT */
		case 160 + 20:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons ants.", m_name);
			for (k = 0; k < 6; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev, SUMMON_ANT);
				else
					count += summon_specific(y, x, rlev, SUMMON_ANT);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF6_S_SPIDER */
		case 160 + 21:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons spiders.", m_name);
			for (k = 0; k < 6; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_SPIDER);
				else
					count += summon_specific(y, x, rlev, SUMMON_SPIDER);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF6_S_HOUND */
		case 160 + 22:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons hounds.", m_name);
			for (k = 0; k < 6; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev, SUMMON_HOUND);
				else
					count += summon_specific(y, x, rlev, SUMMON_HOUND);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF6_S_HYDRA */
		case 160 + 23:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons hydras.", m_name);
			for (k = 0; k < 6; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev, SUMMON_HYDRA);
				else
					count += summon_specific(y, x, rlev, SUMMON_HYDRA);
			}
			if (blind && count)
				msg_print("You hear many things appear nearby.");
			break;
		}

			/* RF6_S_ANGEL */
		case 160 + 24:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons an angel!", m_name);
			for (k = 0; k < 1; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev, SUMMON_ANGEL);
				else
					count += summon_specific(y, x, rlev, SUMMON_ANGEL);
			}
			if (blind && count)
				msg_print("You hear something appear nearby.");
			break;
		}

			/* RF6_S_DEMON */
		case 160 + 25:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons a hellish adversary!",
					m_name);
			for (k = 0; k < 1; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev, SUMMON_DEMON);
				else
					count += summon_specific(y, x, rlev, SUMMON_DEMON);
			}
			if (blind && count)
				msg_print("You hear something appear nearby.");
			break;
		}

			/* RF6_S_UNDEAD */
		case 160 + 26:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons an undead adversary!",
					m_name);
			for (k = 0; k < 1; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_UNDEAD);
				else
					count += summon_specific(y, x, rlev, SUMMON_UNDEAD);
			}
			if (blind && count)
				msg_print("You hear something appear nearby.");
			break;
		}

			/* RF6_S_DRAGON */
		case 160 + 27:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons a dragon!", m_name);
			for (k = 0; k < 1; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_DRAGON);
				else
					count += summon_specific(y, x, rlev, SUMMON_DRAGON);
			}
			if (blind && count)
				msg_print("You hear something appear nearby.");
			break;
		}

			/* RF6_S_HI_UNDEAD */
		case 160 + 28:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons greater undead!",
					m_name);
			for (k = 0; k < 8; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_HI_UNDEAD);
				else
					count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

			/* RF6_S_HI_DRAGON */
		case 160 + 29:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons ancient dragons!",
					m_name);
			for (k = 0; k < 8; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_HI_DRAGON);
				else
					count += summon_specific(y, x, rlev, SUMMON_HI_DRAGON);
			}
			if (blind && count)
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}

			/* RF6_S_WRAITH */
		case 160 + 30:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format
					("%^s magically summons mighty undead opponents!",
					m_name);
			for (k = 0; k < 8; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_WRAITH);
				else
					count += summon_specific(y, x, rlev, SUMMON_WRAITH);
			}
			for (k = 0; k < 8; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_HI_UNDEAD);
				else
					count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

			/* RF6_S_UNIQUE */
		case 160 + 31:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons special opponents!",
					m_name);
			for (k = 0; k < 8; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_UNIQUE);
				else
					count += summon_specific(y, x, rlev, SUMMON_UNIQUE);
			}
			for (k = 0; k < 8; k++)
			{
				if (m_ptr->is_pet)
					count +=
						summon_specific_friendly(y, x, rlev,
						SUMMON_HI_UNDEAD);
				else
					count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
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
			if (r_ptr->r_cast_inate < MAX_UCHAR)
				r_ptr->r_cast_inate++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32 * 5)
		{
			r_ptr->r_flags5 |= (1L << (thrown_spell - 32 * 4));
			if (r_ptr->r_cast_spell < MAX_UCHAR)
				r_ptr->r_cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32 * 6)
		{
			r_ptr->r_flags6 |= (1L << (thrown_spell - 32 * 5));
			if (r_ptr->r_cast_spell < MAX_UCHAR)
				r_ptr->r_cast_spell++;
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
	/* Monsters in the magical arena. */
	if (p_ptr->inside_special == SPECIAL_MAGIC_ARENA)
		return (TRUE);

	/* Keep monsters from running too far away */
	if (m_ptr->cdis > MAX_SIGHT + 5)
		return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear)
		return (TRUE);

#ifdef ALLOW_TERROR

	/* Nearby monsters will not become terrified */
	if (m_ptr->cdis <= 5)
		return (FALSE);

	/* Examine player power (level) */
	p_lev = p_ptr->lev;

	/* Examine monster power (level plus morale) */
	m_lev = r_ptr->level + (m_idx & 0x08) + 25;

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4)
		return (FALSE);
	if (m_lev + 4 <= p_lev)
		return (TRUE);

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
		return (TRUE);

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
	if (!flow_by_sound)
		return (FALSE);

	/* Monster can go through rocks */
	if (r_ptr->flags2 & (RF2_PASS_WALL))
		return (FALSE);
	if (r_ptr->flags2 & (RF2_KILL_WALL))
		return (FALSE);

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* The player is not currently near the monster grid */
	if (cave_when[y1][x1] < cave_when[py][px])
	{
		/* The player has never been near the monster grid */
		if (cave_when[y1][x1] == 0)
			return (FALSE);

		/* The monster is not allowed to track the player */
		if (!flow_by_smell)
			return (FALSE);
	}

	/* Monster is too far away to notice the player */
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

#endif


/*
 * Choose "logical" directions for monster movement
 *
 * We store the directions in a special "mm" array
 */
static void get_moves(int m_idx, int mm[5])
{
	int py;
	int px;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int y, ay, x, ax;

	int move_val = 0;

	int y2;
	int x2;

	/* Choose the appropriate target. */
	find_target_nearest(m_ptr, r_ptr, &py, &px);

	y2 = py;
	x2 = px;

#ifdef MONSTER_FLOW
	/* Flow towards the player */
	if (flow_by_sound && py == p_ptr->py && px == p_ptr->px)
	{
		/* Flow towards the player */
		(void) get_moves_aux(m_idx, &y2, &x2);
	}
#endif

	/* Extract the "pseudo-direction" */
	y = m_ptr->fy - y2;
	x = m_ptr->fx - x2;


	/* Apply fear */
	/* Pets aren't scared of the player, but will run away. */

	if (!m_ptr->is_pet && mon_will_run(m_idx))
	{
		/* This is not a very "smart" method XXX XXX */
		y = (-y);
		x = (-x);
	}
	else if (m_ptr->is_pet && m_ptr->monfear)
	{
		y = (-y);
		x = (-x);
	}

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


static void punish_monster(monster_type * m_ptr)
{
	int roll = randint(100);
	char m_name[80];
	cptr god_name = deity_info[p_ptr->pgod - 1].name;

	monster_desc(m_name, m_ptr, 0);

	p_ptr->redraw |= (PR_HEALTH);

	/* Avatars don't get punished. */
	if (m_ptr->mflag & MFLAG_AVATAR)
		return;


	if (roll < 35)
	{
		m_ptr->monfear += 200;
		mformat(MSG_BONUS, "%^s strikes fear into %s!", god_name, m_name);
	}
	else if (roll < 65)
	{
		m_ptr->hp -= 200;
		mformat(MSG_BONUS, "%^s strikes at %s!", god_name, m_name);
	}
	else if (roll < 95)
	{
		m_ptr->is_pet = TRUE;
		mformat(MSG_BONUS, "%^s dooms %s to eternal slavery!", god_name,
			m_name);
	}
	else
	{
		m_ptr->hp -= 200;
		destroy_area(m_ptr->fy, m_ptr->fx, 7, TRUE);
		mformat(MSG_BONUS, "%^s is supremely angered at %s!", god_name,
			m_name);
	}
}


static void process_monster(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i, d, oy, ox, ny, nx;

	int mm[5];

	bool stagger;

	bool do_turn;
	bool do_move;
	bool do_view;

	bool player_is_sacred = sacred_monster(r_ptr);
	bool monster_is_smart = r_ptr->flags2 & RF2_SMART;

	bool did_open_door;
	bool did_bash_door;
	bool did_take_item;
	bool did_kill_item;
	bool did_move_body;
	bool did_kill_body;
	bool did_pass_wall;
	bool did_kill_wall;

	bool gooddude = (p_ptr->sc >= 0 && r_ptr->flags3 & RF3_POLICE);

	/* Handle "sleep" */
	if (m_ptr->csleep)
	{
		u32b notice;

		/* Aggravation */
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
				mformat(MSG_TEMP, "%^s wakes up.", m_name);
			}

			/* Efficiency XXX XXX */
			return;
		}

		/* Anti-stealth */
		notice = rand_int(1024);

		/* Hack -- See if monster "notices" player */
		if ((notice * notice * notice) <= p_ptr->noise)
		{
			int d = 1;

			/* Wake up faster near the player */
			if (m_ptr->cdis < 50)
				d = (100 / m_ptr->cdis);

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
					mformat(MSG_TEMP, "%^s wakes up.", m_name);

					/* Hack -- Count the wakings */
					if (r_ptr->r_wake < MAX_UCHAR)
					{
						r_ptr->r_wake++;
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

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				mformat(MSG_TEMP, "%^s is no longer stunned.", m_name);
			}
		}

		/* Still stunned -- but not helpless!! From GJW     -KMW- */
		if (m_ptr->stunned && (rand_int(100) < 40))
			return;
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
				mformat(MSG_TEMP, "%^s is no longer confused.", m_name);
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
			if (m_ptr->ml && !player_is_sacred)
			{
				char m_name[80];
				char m_poss[80];

				/* Acquire the monster name/poss */
				monster_desc(m_name, m_ptr, 0);
				monster_desc(m_poss, m_ptr, 0x22);

				/* Dump a message */
				mformat(MSG_TEMP, "%^s recovers %s courage.", m_name,
					m_poss);
			}
		}
	}


	/* Get the origin */
	oy = m_ptr->fy;
	ox = m_ptr->fx;


	/* Attempt to "mutiply" if able and allowed */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) && (num_repro < MAX_REPRO))
	{
		int k, y, x;

		/* Count the adjacent monsters 
		 * HACK Don't crowd the wilderness. */
		if (p_ptr->inside_special == SPECIAL_WILD) {
		  k = 3;

		} else {
		  for (k = 0, y = oy - 1; y <= oy + 1; y++) {
		    for (x = ox - 1; x <= ox + 1; x++) {
		      /* Count monsters */
		      if (cave_m_idx[y][x] > 0)
			k++;
		    }
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


	/* Attempt to cast a spell */
	/* Don't cast if player should be protected. */
	/* Smart monsters will be terrfied. (See the terror fns later.) */

	if (!m_ptr->is_pet)
	{
		if (player_is_sacred && monster_is_smart)
		{
			m_ptr->monfear += damroll(25, 25);
		}
		else if (player_is_sacred)
		{
			/* Hack -- punish monster only if player was hurt. */

			int old_hp = p_ptr->chp;
			bool spell_cast_ok = make_attack_spell(m_idx);

			if (spell_cast_ok && p_ptr->chp < old_hp)
			{
				punish_monster(m_ptr);
				return;
			}
		}
		else if (!gooddude)
		{
			if (make_attack_spell(m_idx))
				return;
		}
	}
	else
	{
		if (make_attack_spell(m_idx))
			return;
	}

	/* Reset */
	stagger = FALSE;


	/* Confused */
	if (m_ptr->confused || r_ptr->flags3 & RF3_FRIENDLY ||
	    m_ptr->mflag & MFLAG_PACIFIST) {

		/* Stagger */
		stagger = TRUE;

	/* Random movement */
	} else if (r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25))
	{
		/* Random movement (25%) */
		if (!(r_ptr->flags1 & (RF1_RAND_50)))
		{
			/* Random */
			if (rand_int(100) < 25)
			{
				/* Memorize flags */
				if (m_ptr->ml)
					r_ptr->r_flags1 |= (RF1_RAND_25);

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
					r_ptr->r_flags1 |= (RF1_RAND_50);

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
					r_ptr->r_flags1 |= (RF1_RAND_50 | RF1_RAND_25);

				/* Stagger */
				stagger = TRUE;
			}
		}
	}

	/* Normal movement */
	if (!stagger)
	{
		/* Logical moves */
		get_moves(m_idx, mm);
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
		d = (stagger ? ddd[rand_int(8)] : mm[i]);

		/* Get the destination */
		ny = oy + ddy[d];
		nx = ox + ddx[d];

		/* Empty space. */
		if (cave_feat[ny][nx] == FEAT_UNSEEN)
		{
			/* Skip */
		}

		/* Floor is open? */
		else if (cave_floor_bold(ny, nx))
		{
			/* Go ahead and move */
			do_move = TRUE;

			/* handle deep water -KMW- */
			if (cave_feat[ny][nx] == FEAT_DEEP_WATER &&
				!(r_ptr->flags2 & RF2_SWIM) &&
				!(r_ptr->flags2 & RF2_PASS_WALL) &&
				!(r_ptr->flags2 & RF2_FLY) &&
				!(r_ptr->flags2 & RF2_AQUATIC))
				do_move = FALSE;

			/* handle deep lava -KMW- */
			else if ((cave_feat[ny][nx] == FEAT_DEEP_LAVA) &&
				((!(r_ptr->flags2 & (RF2_DEEPLAVA))) &&
					(!(r_ptr->flags2 & (RF2_PASS_WALL))) &&
					(!(r_ptr->flags2 & (RF2_FLY)))))
				do_move = FALSE;

			/* handle shallow lava -KMW- */
			else if ((cave_feat[ny][nx] == FEAT_SHAL_LAVA) &&
				((!(r_ptr->flags3 & (RF3_IM_FIRE))) &&
					(!(r_ptr->flags2 & (RF2_PASS_WALL))) &&
					(!(r_ptr->flags2 & (RF2_FLY)))))
				do_move = FALSE;

			/* handle aquatic monsters -KMW- */
			else if (r_ptr->flags2 & RF2_AQUATIC &&
				(cave_feat[ny][nx] != FEAT_DEEP_WATER) &&
				(cave_feat[ny][nx] != FEAT_SHAL_WATER))
				do_move = FALSE;
		}

		/* Aquatic monsters never move through solid terrain. */
		else if (r_ptr->flags2 & RF2_AQUATIC)
		{
			/* Nothing. */
		}

		/* Permanent wall. Adjusted -KMW- */
		else if ((cave_feat[ny][nx] >= FEAT_PERM_EXTRA) &&
			(cave_feat[ny][nx] <= FEAT_PERM_SOLID))
		{
			/* Nothing */
		}

		/* Handle Chaos Fog */
		else if (cave_feat[ny][nx] == FEAT_CHAOS_FOG)
		{
			if (randint(5) == 1)
			{
				do_move = FALSE;
				if (player_has_los_bold(ny, nx))
				{
					mformat(MSG_TEMP, "The chaos fog blocks something.");
				}
			}
			else
			{
				do_move = TRUE;
			}
		}

		/* Handle trees */
		else if (cave_feat[ny][nx] == FEAT_TREES)
		{
			do_move = TRUE;
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
				}

				/* Locked doors (not jammed) */
				else if (cave_feat[ny][nx] < FEAT_DOOR_HEAD + 0x08)
				{
					int k;

					/* Door power */
					k = ((cave_feat[ny][nx] - FEAT_DOOR_HEAD) & 0x07);

#if 0
					/* XXX XXX XXX Old test (pval 10 to 20) */
					if (randint((m_ptr->hp + 1) * (50 + o_ptr->pval)) <
						40 * (m_ptr->hp - 10 - o_ptr->pval));
#endif

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
				int k;

				/* Door power */
				k = ((cave_feat[ny][nx] - FEAT_DOOR_HEAD) & 0x07);

#if 0
				/* XXX XXX XXX Old test (pval 10 to 20) */
				if (randint((m_ptr->hp + 1) * (50 + o_ptr->pval)) <
					40 * (m_ptr->hp - 10 - o_ptr->pval));
#endif

				/* Attempt to Bash XXX XXX XXX */
				if (rand_int(m_ptr->hp / 10) > k)
				{
					/* Message */
					mprint(MSG_TEMP, "You hear a door burst open!");

					/* Disturb (sometimes) */
					if (disturb_minor)
						disturb(0, 0);

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
				if (player_has_los_bold(ny, nx))
					do_view = TRUE;
			}
		}



		/* Set off traps. */
		if (do_move && (cave_feat[ny][nx] == FEAT_INVIS ||
			(cave_feat[ny][nx] >= FEAT_TRAP_HEAD &&
			cave_feat[ny][nx] <= FEAT_TRAP_TAIL)) &&
			!(r_ptr->flags2 & RF2_FLY))
		{
			if (m_ptr->ml)
			{
				char m_name[80];

				monster_desc(m_name, m_ptr, 0);
				msg_format("%^s has set off a trap!", m_name);
			}

			if (cave_feat[ny][nx] == FEAT_INVIS)
				pick_trap(ny, nx);

			/* Chance of ``disarming'' the trap if the monster is smart.
			 */
			if (!(r_ptr->flags2 & RF2_SMART) || !(magik(r_ptr->level)))
			{
				mon_hit_trap(m_idx, ny, nx);
			}

			/* Erase the trap -- no cheating! */
			cave_info[ny][nx] &= ~(CAVE_MARK);
			cave_set_feat(ny, nx, FEAT_FLOOR);
		}


		/* Hack -- check for Glyph of Warding */
		if (do_move && (cave_feat[ny][nx] == FEAT_GLYPH))
		{
			/* Assume no move allowed */
			do_move = FALSE;

			/* Break the ward */
			/* Don't break ward if it is part of vault -- */
			/* Monsters shouldn't escape from sealed vaults! */

			if ((randint(BREAK_GLYPH) < r_ptr->level) &&
				!(cave_info[ny][nx] & CAVE_ICKY))
			{
				/* Describe observable breakage */
				if (cave_info[ny][nx] & (CAVE_MARK))
				{
					mprint(MSG_WARNING,
						"The rune of protection is broken!");
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
		/* The player is in the way.  Attack him. */
		/* if (do_move && (cave_m_idx[ny][nx] < 0)) */
		if (cave_m_idx[ny][nx] < 0) {
		  /* Do the attack */
		  /* "Friendly" monsters don't attack. */
		  /* Punish monsters for attacking sacred player. */
		  /* No melee in the magical arena! */

		  if (r_ptr->flags1 & RF1_NEVER_BLOW ||
		      m_ptr->mflag & MFLAG_PACIFIST || 
		      gooddude) {

		    /* Nothing. */

		  } else if (!m_ptr->is_pet &&
			     p_ptr->inside_special != SPECIAL_MAGIC_ARENA) {

		    if (monster_is_smart && player_is_sacred) {
		      m_ptr->monfear += damroll(25, 25);

		    } else if (player_is_sacred) {
		      (void) make_attack_normal(m_idx);
		      punish_monster(m_ptr);

		    } else {
		      (void) make_attack_normal(m_idx);
		    }
		  }

		  /* Do not move */
		  do_move = FALSE;

		  /* Took a turn */
		  do_turn = TRUE;
		}


		/* Some monsters never move */
		if (do_move && (r_ptr->flags1 & (RF1_NEVER_MOVE)))
		{
			/* Hack -- memorize lack of attacks */
			/* if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_NEVER_MOVE); */

			/* Do not move */
			do_move = FALSE;
		}


		/* A monster is in the way */
		if (do_move && (cave_m_idx[ny][nx] > 0))
		{
			monster_type *n_ptr = &m_list[cave_m_idx[ny][nx]];
			bool fear = FALSE, award_exp;
			int dam, tmp_i;
			char m_name[80];
			char n_name[80];

			/* Assume no movement */
			do_move = FALSE;

			/* Kill weaker monsters */
			if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
				(compare_monsters(m_ptr, n_ptr) > 0) && !m_ptr->is_pet)
			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster ate another monster */
				did_kill_body = TRUE;

				/* Message XXX XXX XXX */

				/* Kill the monster */
				delete_monster(ny, nx);

				/* "Friendly" monsters. */

			}
			else if ((m_ptr->is_pet && !n_ptr->is_pet) || (!m_ptr->is_pet
					&& n_ptr->is_pet))
			{
				do_move = FALSE;

				for (tmp_i = 0; tmp_i < 4; tmp_i++)
				{
					if (!(r_ptr->blow[tmp_i].method))
						break;

					dam =
						damroll(r_ptr->blow[tmp_i].d_dice,
						r_ptr->blow[tmp_i].d_side);

					/* Acquire the monster names */
					/* Assume names are always known */

					if (m_ptr->is_pet)
					{
						monster_desc(m_name, m_ptr, 0x80);
						monster_desc(n_name, n_ptr, 0x88);
						award_exp = TRUE;
					}
					else
					{
						monster_desc(m_name, m_ptr, 0x88);
						monster_desc(n_name, n_ptr, 0x80);
						award_exp = FALSE;
					}

					if (show_pet_messages && (hidden_pet_messages || m_ptr->ml))
						msg_format("%^s hits %^s.", m_name, n_name);

					if (mon_take_hit(cave_m_idx[ny][nx], dam, &fear,
							" is killed.", award_exp, TRUE))
						break;
				}
			}
		}

		/* A monster is in the way */
		if (do_move && (cave_m_idx[ny][nx] > 0))
		{
			monster_type *n_ptr = &m_list[cave_m_idx[ny][nx]];

			/* Assume no movement */
			do_move = FALSE;

			/* Push past weaker monsters (unless leaving a wall) */
			if ((r_ptr->flags2 & (RF2_MOVE_BODY)) &&
				(compare_monsters(m_ptr, n_ptr) > 0) &&
				(cave_floor_bold(m_ptr->fy, m_ptr->fx)) && !m_ptr->is_pet)
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
			object_type *o_ptr;
			object_type *o_nxt;

			/* Take a turn */
			do_turn = TRUE;

			/* Move the monster */
			monster_swap(oy, ox, ny, nx);

			/* Possible disturb */
			if (m_ptr->ml && (disturb_move ||
					((m_ptr->mflag & (MFLAG_VIEW)) && disturb_near)) &&
				!m_ptr->is_pet)
			{
				/* Disturb */
				disturb(0, 0);
			}



			/* Scan all objects in the grid */
			o_ptr = cave_o_idx[ny][nx];

			while (TRUE)
			{

				if (o_ptr == NULL)
					break;

				/* Acquire next object, if this one gets moved. */
				o_nxt = o_ptr->next;

				/* Skip gold */
				if (o_ptr->tval == TV_GOLD)
				{
					o_ptr = o_nxt;
					continue;
				}

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
					if (f1 & (TR1_KILL_DRAGON))
						flg3 |= (RF3_DRAGON);
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
						/* Take note */
						did_take_item = TRUE;

						/* Describe observable situations */
						if (player_has_los_bold(ny, nx))
						{
							/* Dump a message */
							msg_format("%^s picks up %s.", m_name, o_name);
						}

						/* Carry the object */
						monster_inven_carry(m_ptr, o_ptr);
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
						remove_object(o_ptr);
					}
				}

				/* Advance to the next object. */
				o_ptr = o_nxt;

			}
		}

		/* Stop when done */
		if (do_turn)
			break;
	}


	/* Notice changes in view */
	if (do_view)
	{
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
	}


	/* Learn things from observable monster */
	if (m_ptr->ml)
	{
		/* Monster opened a door */
		if (did_open_door)
			r_ptr->r_flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door)
			r_ptr->r_flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item)
			r_ptr->r_flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item)
			r_ptr->r_flags2 |= (RF2_KILL_ITEM);

		/* Monster pushed past another monster */
		if (did_move_body)
			r_ptr->r_flags2 |= (RF2_MOVE_BODY);

		/* Monster ate another monster */
		if (did_kill_body)
			r_ptr->r_flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall)
			r_ptr->r_flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall)
			r_ptr->r_flags2 |= (RF2_KILL_WALL);
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
	int fy, fx;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Assume no pets are active */
	m_pet_num = 0;


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
		if (p_ptr->leaving)
			break;


		/* Access the monster */
		m_ptr = &m_list[i];


		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx)
			continue;

		if (m_ptr->is_pet)
		{
			m_pet_num++;
		}

		/* Ignore "born" monsters XXX XXX */
		if (m_ptr->mflag & (MFLAG_BORN))
			continue;

		/* Obtain the energy boost */
		e = extract_energy[m_ptr->mspeed];

		/* Give this monster some energy */
		m_ptr->energy += e;


		/* Not enough energy to move */
		if (m_ptr->energy < 100)
			continue;

		/* Use up "some" energy */
		m_ptr->energy -= 100;


		/* Heal monster? XXX XXX XXX */


		/* Access the race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Monsters can "sense" the player */

		if (m_ptr->cdis <= r_ptr->aaf)
		{
			/* Process the monster */
			process_monster(i);

			/* Continue */
			continue;
		}

		/* Access the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;

		/* Monsters can "see" the player (backwards) XXX XXX */
		if (player_has_los_bold(fy, fx))
		{
			/* Process the monster */
			process_monster(i);

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
				(cave_cost[fy][fx] < r_ptr->aaf))
			{
				/* Process the monster */
				process_monster(i);

				/* Continue */
				continue;
			}
		}

#endif

	}
}
