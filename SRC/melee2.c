/* File: melee2.c */

/* Purpose: Monster spells and movement */

/* Monster learning, monster distance attacks and spells, fear, flow/
 * movement, monster AI effecting movement and spells, process a monster
 * (with spells and actions of all kinds, reproduction, effects of any
 * terrain on monster movement, picking up and destroying objects),
 * process all monsters.
 *
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
* This file has several additions to it by Keldon Jones (keldon@umr.edu)
* to improve the general quality of the AI (version 0.1.1).
*/

#include "angband.h"

#define SPEAK_CHANCE 8
#define GRINDNOISE 20
#define CYBERNOISE 20

#define SURROUND_MAX 80

/*
 * Calculate the direction to the next enemy
 */
static bool get_enemy_dir(monster_type *m_ptr, int *mm)
{
	int i;
	int x, y;
	int t_idx;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_type *t_ptr;
	monster_race *tr_ptr;

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

		if (is_pet(m_ptr))
		{
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
		}

		/* Monster must be 'an enemy' */
		if (!are_enemies(m_ptr, t_ptr)) continue;

		/* Monster must be projectable if we can't pass through walls */
		if (!(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) &&
			!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx))
		{
			continue;
		}


		/* OK -- we've got a target */
		y = t_ptr->fy;
		x = t_ptr->fx;

		/* Extract the direction */
		x -= m_ptr->fx;
		y -= m_ptr->fy;

		/* North */
		if ((y < 0) && (x == 0))
		{
			mm[0] = 8;
			mm[1] = 7;
			mm[2] = 9;
		}
		/* South */
		else if ((y > 0) && (x == 0))
		{
			mm[0] = 2;
			mm[1] = 1;
			mm[2] = 3;
		}
		/* East */
		else if ((x > 0) && (y == 0))
		{
			mm[0] = 6;
			mm[1] = 9;
			mm[2] = 3;
		}
		/* West */
		else if ((x < 0) && (y == 0))
		{
			mm[0] = 4;
			mm[1] = 7;
			mm[2] = 1;
		}
		/* North-West */
		if ((y < 0) && (x < 0))
		{
			mm[0] = 7;
			mm[1] = 4;
			mm[2] = 8;
		}
		/* North-East */
		else if ((y < 0) && (x > 0))
		{
			mm[0] = 9;
			mm[1] = 6;
			mm[2] = 8;
		}
		/* South-West */
		else if ((y > 0) && (x < 0))
		{
			mm[0] = 1;
			mm[1] = 4;
			mm[2] = 2;
		}
		/* South-East */
		else if ((y > 0) && (x > 0))
		{
			mm[0] = 3;
			mm[1] = 6;
			mm[2] = 2;
		}

		/* Found a monster */
		return TRUE;
	}

	/* No monster found */
	return FALSE;
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
	if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

	/* Friends don't run away */
	if (is_pet(m_ptr)) return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);

#ifdef ALLOW_TERROR

	/* Nearby monsters will not become terrified */
	if (m_ptr->cdis <= 5) return (FALSE);

	/* Examine player power (level) */
	p_lev = p_ptr->lev;

	/* If monster cannot see player, don't panic */

	if ((p_invis) &&
	    (!(r_ptr->flags3 & (RF3_UNDEAD))) &&
	    (!(r_ptr->flags3 & (RF3_DEMON))) &&
	    (!(r_ptr->flags2 & (RF2_INVISIBLE))))
		return(FALSE);

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
	p_val = (p_lev * p_mhp) + (p_chp << 2); /* div p_mhp */
	m_val = (m_lev * m_mhp) + (m_chp << 2); /* div m_mhp */

	/* Strong players scare strong monsters */
	if (p_val * m_mhp > m_val * p_mhp) return (TRUE);

#endif

	/* Assume no terror */
	return (FALSE);
}



/*
* Hack, based on mon_take_hit... perhaps all monster attacks on
* other monsters should use this?
*/
static void mon_take_hit_mon(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type    *m_ptr = &m_list[m_idx];

	monster_race    *r_ptr = &r_info[m_ptr->r_idx];


	/* Redraw (later) if needed */
	if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now... or is it? */
	if (m_ptr->hp < 0)
	{
		if (((r_ptr->flags1 & RF1_UNIQUE) && (!(r_ptr->flags7 & RF7_PET))) ||
			(r_ptr->flags1 & RF1_QUESTOR))
		{
			m_ptr->hp = 1;
		}
		else
		{
			char m_name[80];

			/* Extract monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Make a sound */
			if (!monster_living(r_ptr))
			{
				sound(SOUND_N_KILL);
			}
			else
			{
				sound(SOUND_KILL);
			}

			/* Death by Missile/Spell attack */
			if (note)
			{
				msg_format("%^s%s", m_name, note);
			}
			/* Death by Physical attack -- living monster */
			else if (!m_ptr->ml)
			{
				/* Do nothing */
			}
			/* Death by Physical attack -- non-living monster */
			else if (!monster_living(r_ptr))
			{
				msg_format("%^s is destroyed.", m_name);
			}
			else
			{
				msg_format("%^s is killed.", m_name);
			}

			/* Generate treasure */
			monster_death(m_idx);

			/* Delete the monster */
			delete_monster_idx(m_idx);

			/* Not afraid */
			(*fear) = FALSE;

			/* Monster is dead */
			return;
		}

	}

#ifdef ALLOW_FEAR

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint(dam);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			/* Reduce fear */
			m_ptr->monfear -= tmp;
		}

		/* Cure all the fear */
		else
		{
			/* Cure fear */
			m_ptr->monfear = 0;

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage. Smart monsters
	   won't get scared while player is confused -GSN- */

	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR) ||
	    ((p_ptr->confused) && (!(r_ptr->flags2 & (RF2_SMART))))))
	{
		int             percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		* Run (sometimes) if at 10% or less of max hit points,
		* or (usually) when hit for half its current hit points
		*/
		if (((percentage <= 10) && (rand_int(10) < percentage)) ||
			((dam >= m_ptr->hp) && (rand_int(100) < 80)))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* XXX XXX XXX Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) +
				(((dam >= m_ptr->hp) && (percentage > 7)) ?
				20 : ((11 - percentage) * 5)));
		}
	}

#endif /* ALLOW_FEAR */

	/* Not dead yet */
	return;
}





#ifdef DRS_SMART_OPTIONS


/*
* And now for Intelligent monster attacks (including spells).
*
* Original idea and code by "DRS" (David Reeves Sward).
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
	/* Non-Smart monsters are half as "smart" */
	if (!(r_ptr->flags2 & (RF2_SMART))) prob = prob / 2;

	/* Roll the dice */
	return (rand_int(100) < prob);
}

/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(int m_idx, u64b *f4p, u64b *f5p, u64b *f6p)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u64b f4 = (*f4p);
	u64b f5 = (*f5p);
	u64b f6 = (*f6p);

	u32b smart = 0L;


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


	/* Cheat if requested  or a player ghost*/
	if (smart_cheat || r_ptr->flags2 & RF2_PLAYER_GHOST)
	{
		/* Know basic info */
		if (p_resist_acid) smart |= (SM_RES_ACID);
		if (p_ptr->oppose_acid) smart |= (SM_OPP_ACID);
		if (p_immune_acid) smart |= (SM_IMM_ACID);
		if (p_resist_elec) smart |= (SM_RES_ELEC);
		if (p_ptr->oppose_elec) smart |= (SM_OPP_ELEC);
		if (p_immune_elec) smart |= (SM_IMM_ELEC);
		if (p_resist_fire) smart |= (SM_RES_FIRE);
		if (p_ptr->oppose_fire) smart |= (SM_OPP_FIRE);
		if (p_immune_fire) smart |= (SM_IMM_FIRE);
		if (p_resist_cold) smart |= (SM_RES_COLD);
		if (p_ptr->oppose_cold) smart |= (SM_OPP_COLD);
		if (p_immune_cold) smart |= (SM_IMM_COLD);

		/* Know poison info */
		if (p_resist_pois) smart |= (SM_RES_POIS);
		if (p_ptr->oppose_pois) smart |= (SM_OPP_POIS);

		/* Know special resistances */
		if (p_resist_neth) smart |= (SM_RES_NETH);
		if (p_resist_lite) smart |= (SM_RES_LITE);
		if (p_resist_dark) smart |= (SM_RES_DARK);
		if (p_resist_fear) smart |= (SM_RES_FEAR);
		if (p_resist_conf) smart |= (SM_RES_CONF);
		if (p_resist_chaos) smart |= (SM_RES_CHAOS);
		if (p_resist_disen) smart |= (SM_RES_DISEN);
		if (p_resist_blind) smart |= (SM_RES_BLIND);
		if (p_resist_nexus) smart |= (SM_RES_NEXUS);
		if (p_resist_sound) smart |= (SM_RES_SOUND);
		if (p_resist_shard) smart |= (SM_RES_SHARD);
		if (p_reflect) smart |= (SM_IMM_REFLECT);

		/* Know bizarre "resistances" */
		if (p_free_act) smart |= (SM_IMM_FREE);
		if (!p_ptr->msp) smart |= (SM_IMM_MANA);
	}


	/* Nothing known */
	if (!smart) return;


	if (smart & SM_IMM_ACID)
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
		if (int_outof(r_ptr, 40)) f4 &= ~(RF4_BA_NUKE);
		if (int_outof(r_ptr, 40)) f4 &= ~(RF4_BR_NUKE);
	}
	else if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_POIS);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_POIS);
	}


	if (smart & (SM_RES_NETH))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_NETH);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BA_NETH);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BO_NETH);
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

	if (smart & (SM_RES_FEAR))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_SCARE);
	}

	if (smart & (SM_RES_CONF))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_CONF);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_CONF);
	}

	if (smart & (SM_RES_CHAOS))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_CONF);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_CONF);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_CHAO);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BA_CHAO);
	}

	if (smart & (SM_RES_DISEN))
	{
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_BR_DISE);
	}

	if (smart & (SM_RES_BLIND))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BLIND);
	}

	if (smart & (SM_RES_NEXUS))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_NEXU);
		if (int_outof(r_ptr, 50)) f6 &= ~(RF6_TELE_LEVEL);
	}

	if (smart & (SM_RES_SOUND))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_SOUN);
	}

	if (smart & (SM_RES_SHARD))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_SHAR);
		if (int_outof(r_ptr, 20)) f4 &= ~(RF4_ROCKET);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BL_SHAR);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_CH_SHAR);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BO_SHAR);
	}

	if (smart & (SM_IMM_REFLECT))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_FIRE);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_ACID);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_ELEC);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_POIS);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_NETH);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_WATE);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_MANA);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_PLAS);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_BO_ICEE);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_MISSILE);
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_ARROW_1);
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_ARROW_2);
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_ARROW_3);
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_ARROW_4);
		if (int_outof(r_ptr, 100)) f4 &= ~(RF4_CH_SHAR);
	}

	/* Once slowed, the player doesn't need to be slowed again. -LM- */
	if (p_ptr->slow) f5 &= ~(RF5_SLOW);

	if (smart & (SM_IMM_FREE))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_HOLD);
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_SLOW);
	}

	if (smart & (SM_IMM_MANA))
	{
		if (int_outof(r_ptr, 100)) f5 &= ~(RF5_DRAIN_MANA);
	}

	/* No spells left? */
	if (!f4 && !f5 && !f6)
	   return;
	else
	{
		(*f4p) = f4;
		(*f5p) = f5;
		(*f6p) = f6;
	}

}

#endif /* DRS_SMART_OPTIONS */


/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */
static bool summon_possible(int y1, int x1)
{
	int y, x;

	/* Start at the player's location, and check 2 grids in each dir */
	for (y= y1-2; y<= y1+2; y++)
	{
		for (x = x1-2; x<=x1+2; x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(y,x)) continue;

			/* Only check a circular area */
			if (distance(y1,x1,y,x)>2) continue;

			/* Hack: no summon on glyph of warding */
			if (cave[y][x].feat == FEAT_GLYPH) continue;
			if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

			/* ...nor on the Pattern */
			if ((cave[y][x].feat >= FEAT_PATTERN_START)
				&& (cave[y][x].feat <= FEAT_PATTERN_XTRA2)) continue;

			/* Require empty floor grid in line of sight */
			if (cave_empty_bold(y,x) && los(y1,x1,y,x)) return (TRUE);
		}
	}

	return FALSE;
}



/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will return FALSE if a monster
 * is in the way.
 */
static bool clean_shot(int y1, int x1, int y2, int x2, bool friend)
{
	/* Must be the same as projectable() */

	int i, y, x;

	int grid_n = 0;
	u16b grid_g[512];

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, 0);

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = GRID_Y(grid_g[grid_n-1]);
	x = GRID_X(grid_g[grid_n-1]);

	/* May not end in an unrequested grid */
	if ((y != y2) || (x != x2)) return (FALSE);

	for (i = 0; i < grid_n; i++)
	{
		y = GRID_Y(grid_g[i]);
		x = GRID_X(grid_g[i]);

		if (cave[y][x].m_idx > 0
		    && !(y==y1 && x==x1))
		{
			monster_type *m_ptr = &m_list[cave[y][x].m_idx];
			if (friend == is_pet(m_ptr))
			{
				return (FALSE);
			}

		/* Pets may not shoot through the character */
		if ((y == py) && (x == px))
		{
			if (friend)
				return (FALSE);
		}

		}
	}

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
	(void)project(m_idx, 0, py, px, dam_hp, typ, flg);
}


/*
 * Return TRUE if a spell is good for hurting the player (directly).
 */
static bool spell_attack(byte spell)
{
	/* All RF4 spells hurt (except for shriek) */
	if (spell < 65 && spell > 0) return (TRUE);

	/* Various "ball" spells */
	if (spell >= 65 && spell <= 65 + 8) return (TRUE);

	/* "Cause wounds" and "bolt" spells */
	if (spell >= 65 + 12 && spell <= 65 + 27) return (TRUE);

	/* Hand of Doom, Winds */
	if (spell == 129 + 1 || spell == 129 + 6 || spell == 129 + 7) return (TRUE);

	/* Doesn't hurt */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good for escaping.
 */
static bool spell_escape(byte spell)
{
	/* Blink or Teleport */
	if (spell == 129 + 4 || spell == 129 + 5) return (TRUE);

	/* Teleport the player away */
	if (spell == 129 + 9 || spell == 129 + 10) return (TRUE);

	/* Isn't good for escaping */
	return (FALSE);
}

/*
 * Return TRUE if a spell is good for getting closer.
 */
static bool spell_close(byte spell)
{
	/* Dimension door, teleport to */
	if ((spell == 129 + 11) || (spell == 129 + 8)) return (TRUE);

	/* Isn't good for escaping */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good for annoying the player.
 */
static bool spell_annoy(byte spell)
{
	/* Shriek */
	if (spell == 1 + 0) return (TRUE);

	/* Brain smash, et al (added curses) */
	if (spell >= 65 + 9 && spell <= 65 + 14) return (TRUE);

	/* Scare, confuse, blind, slow, paralyze */
	if (spell >= 65 + 27 && spell <= 65 + 31) return (TRUE);

	/* Teleport to */
	if (spell == 129 + 8) return (TRUE);

#if 0
	/* Hand of Doom */
	if (spell == 129 + 1) return (TRUE);
#endif

	/* Darkness, make traps, cause amnesia */
	if (spell >= 129 + 12 && spell <= 129 + 14) return (TRUE);

	/* Doesn't annoy */
	return (FALSE);
}

/*
 * Return TRUE if a spell summons help.
 */
static bool spell_summon(byte spell)
{
	/* All summon spells */
	if (spell >= 129 + 16) return (TRUE);

	/* Doesn't summon */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good in a tactical situation.
 */
static bool spell_tactic(byte spell)
{
	/* Blink */
	if (spell == 129 + 4) return (TRUE);

	/* Not good */
	return (FALSE);
}


/*
 * Return TRUE if a spell hastes.
 */
static bool spell_haste(byte spell)
{
	/* Haste self */
	if (spell == 129 + 0) return (TRUE);

	/* Not a haste spell */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good for healing.
 */
static bool spell_heal(byte spell)
{
	/* Heal */
	if (spell == 160 + 2 || spell == 160 + 3) return (TRUE);

	/* No healing */
	return (FALSE);
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
static int choose_attack_spell(int m_idx, byte spells[], byte num)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	byte escape[96], escape_num = 0;
	byte attack[96], attack_num = 0;
	byte summon[96], summon_num = 0;
	byte tactic[96], tactic_num = 0;
	byte annoy[96], annoy_num = 0;
	byte haste[96], haste_num = 0;
	byte heal[96], heal_num = 0;
	byte close[96], close_num = 0;

	/* Can the monster teleport to the player and blink away again? */
	bool hit_n_run = (r_ptr->flags6 & RF6_BLINK) && (r_ptr->flags6 & (RF6_DOOR | RF6_TELE_TO));

	int i;

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

		/* Heal spell? */
		if (spell_close(spells[i])) close[close_num++] = spells[i];

	}

	/*** Try to pick an appropriate spell type ***/

	/* Hurt badly or afraid, attempt to flee */
	if ((m_ptr->hp < m_ptr->maxhp / 3) || m_ptr->monfear)
	{
		/* Choose escape spell if possible */
		if (escape_num) return (escape[rand_int(escape_num)]);
	}

	/* Still hurt badly, couldn't flee, attempt to heal */
	if (m_ptr->hp < m_ptr->maxhp / 3)
	{
		/* Choose heal spell if possible */
		if (heal_num) return (heal[rand_int(heal_num)]);
	}

	/* Get close to the player so we can hit them */
	if (close_num && (distance(py, px, m_ptr->fy, m_ptr->fx) > 1) && !mon_will_run(m_idx))
	{
		/* Choose closing spell */
		return (close[rand_int(close_num)]);
	}

	/* Player is close and we have attack spells, blink away */
	if ((distance(py, px, m_ptr->fy, m_ptr->fx) < 4) && attack_num && (rand_int(100) < 75))
	{
		/* Choose tactical spell */
		if (tactic_num) return (tactic[rand_int(tactic_num)]);
	}

	/* We're hurt (not badly), try to heal */
	if ((m_ptr->hp < m_ptr->maxhp * 3 / 4) && (rand_int(100) < 75))
	{
		/* Choose heal spell if possible */
		if (heal_num) return (heal[rand_int(heal_num)]);
	}

	/* Summon if possible (sometimes) */
	if (summon_num && (rand_int(100) < 50))
	{
		/* Choose summon spell */
		return (summon[rand_int(summon_num)]);
	}

	/* Attack spell (most of the time) */
	if (attack_num && (rand_int(100) < 85))
	{
		/* Choose attack spell */
		return (attack[rand_int(attack_num)]);
	}

	/* Player is close and we want to run, have attack spells, or can teleport right back; blink */
	if (tactic_num && (mon_will_run(m_idx) ||
		 ((distance(py, px, m_ptr->fy, m_ptr->fx) < 4) && attack_num) ||
		 ((distance(py, px, m_ptr->fy, m_ptr->fx) == 1) && hit_n_run)))
	{
		/* Choose tactical spell */
		return (tactic[rand_int(tactic_num)]);
	}

	/* Haste self if we aren't already somewhat hasted (rarely) */
	if (haste_num && (rand_int(100) < (20 + r_ptr->speed - m_ptr->mspeed)))
	{
		/* Choose haste spell */
		return (haste[rand_int(haste_num)]);
	}

	/* Annoy player (most of the time) */
	if (annoy_num && (rand_int(100) < 85))
	{
		/* Choose annoyance spell */
		return (annoy[rand_int(annoy_num)]);
	}

	/* Choose no spell */
	return (0);
}


/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void breath(int m_idx, int typ, int dam_hp, int rad, bool breath)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	if (rad < 1) rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

 	if (breath) rad = 0 - rad;

	if ((p_ptr->mind & MIND_INERTIAL_BARRIER) && (typ != GF_PSI))
	   dam_hp /= 2;

	/* Target the player with a ball/cone attack */
	(void)project(m_idx, rad, py, px, dam_hp, typ, flg);
}

/*
 * Monster casts a volley of bolt spells at another monster
 * Scatter "bolts" around the target or first obstacle reached.
 * Affect grids, objects, and monsters
 *
 * dd - bolt damage dice
 * ds - bolt damage sides
 * num - number of bolts in volley
 * dev - maximum distance to spread
 */
static void blast(int m_idx, int typ, int dd, int ds, int num, int dev)
{
	int ly, lx, ld;
	int i;

	monster_type *m_ptr = &m_list[m_idx];
	int y = m_ptr->fy;
	int x = m_ptr->fx;

	int flg = PROJECT_FAST | PROJECT_STOP | PROJECT_KILL | PROJECT_GRID;

	lx = 20 * (px - x) + x;
	ly = 20 * (py - y) + y;
	ld = distance(y, x, ly, lx);

	/* Blast */
	for (i = 0; i < num; i++)
	{
		while (1)
		{
			/* Get targets for some bolts */
			y = rand_spread(ly, ld * dev / 20);
			x = rand_spread(lx, ld * dev / 20);

			if (distance(ly, lx, y, x) <= ld * dev / 20) break;
		}

		/* Analyze the "dir" and the "target". */
		(void)project(m_idx, 0, y, x, damroll(dd, ds), typ, flg);
	}
}



/*
 * Monster casts a breath (or ball) attack at another monster.
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void monst_breath_monst(int m_idx, int y, int x, int typ, int dam_hp, int rad, bool breath)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	if (rad < 1) rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

 	/* Handle breath attacks */
 	if (breath) rad = 0 - rad;

	(void)project(m_idx, rad, y, x, dam_hp, typ, flg);
}


/*
 * Monster casts a bolt at another monster
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void monst_bolt_monst(int m_idx, int y, int x, int typ, int dam_hp)
{
	int flg = PROJECT_STOP | PROJECT_KILL;

	(void)project(m_idx, 0, y, x, dam_hp, typ, flg);
}


/*
 * Monster casts a volley of bolt spells at another monster
 * Scatter "bolts" around the target or first obstacle reached.
 * Affect grids, objects, and monsters
 *
 * dd - bolt damage dice
 * ds - bolt damage sides
 * num - number of bolts in volley
 * dev - maximum distance to spread
 */
static void monst_blast_monst(int m_idx, int ty, int tx, int typ, int dd, int ds, int num, int dev)
{
	int ly, lx, ld;
	int i;

	monster_type *m_ptr = &m_list[m_idx];
	int y = m_ptr->fy;
	int x = m_ptr->fx;

	int flg = PROJECT_FAST | PROJECT_THRU | PROJECT_STOP | PROJECT_KILL | PROJECT_GRID;

	lx = 20 * (tx - x) + x;
	ly = 20 * (ty - y) + y;
	ld = distance(y, x, ly, lx);

	/* Blast */
	for (i = 0; i < num; i++)
	{
		while (1)
		{
			/* Get targets for some bolts */
			y = rand_spread(ly, ld * dev / 20);
			x = rand_spread(lx, ld * dev / 20);

			if (distance(ly, lx, y, x) <= ld * dev / 20) break;
		}

		/* Analyze the "dir" and the "target". */
		(void)project(m_idx, 0, y, x, damroll(dd, ds), typ, flg);
	}
}


/*
 * Monster tries to 'cast a spell' (or breath, etc)
 * at another monster.
 */
static bool monst_spell_monst(int m_idx)
{
	int y = 0, x = 0;
	int i, k, t_idx;
	int chance, thrown_spell, count = 0;
	byte spell[192], num = 0;
	char m_name[80], t_name[80];
	char m_poss[80];
	char ddesc[80];
	int rlev;                               /* monster level */
	monster_type *m_ptr = &m_list[m_idx];   /* Attacker */
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_type *t_ptr;                    /* Putative target */
	monster_race *tr_ptr;
	u32b  f4, f5, f6;                       /* racial spell flags */
	bool direct = TRUE;
	bool wake_up = FALSE;

	/* Extract the blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Extract the "see-able-ness" */
	bool seen = (!blind && m_ptr->ml);

	bool see_m;
	bool see_t;
	bool see_either;
	bool see_both;

	bool known;

	bool friendly = is_friendly(m_ptr);
	bool pet = is_pet(m_ptr);

	if (is_pet(m_ptr)) friendly = TRUE;

	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	/* Hack -- Extract the spell probability */
	chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance) return (FALSE);

	if (rand_int(100) >= chance) return (FALSE);


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

		/* Hack -- no fighting away from player */
		if (t_ptr->cdis > p_ptr->pet_follow_distance) continue;

		/* Monster must be 'an enemy' */
		if (!are_enemies(m_ptr, t_ptr)) continue;

		/* Hack -- no fighting >100 squares from player */
		if (t_ptr->cdis > MAX_RANGE) continue;

		/* Monster must be projectable */
		if (stupid_monsters)
		{
			if (!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx)) continue;
		}
		else
		{
			if (!clean_shot(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx, is_pet(m_ptr))) continue;
		}

		/* OK -- we-ve got a target */
		y = t_ptr->fy;
		x = t_ptr->fx;

		/* Extract the monster level */
		rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

		/* Extract the racial spell flags */
		f4 = r_ptr->flags4;
		f5 = r_ptr->flags5;
		f6 = r_ptr->flags6;

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

		/* Extract the "inate" spells */
		for (k = 0; k < 64; k++)
		{
			if (f4 & (1LL << k)) spell[num++] = k + 1;
		}

		/* Extract the "normal" spells */
		for (k = 0; k < 64; k++)
		{
			if (f5 & (1LL << k)) spell[num++] = k + 65;
		}

		/* Extract the "bizarre" spells */
		for (k = 0; k < 64; k++)
		{
			if (f6 & (1LL << k)) spell[num++] = k + 129;
		}

		/* No spells left */
		if (!num) return (FALSE);

		/* Stop if player is dead or gone */
		if (!alive || death) return (FALSE);

		/* Handle "leaving" */
		if (p_ptr->leaving) return (FALSE);

		/* Get the monster name (or "it") */
		monster_desc(m_name, m_ptr, 0x00);

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(m_poss, m_ptr, 0x22);

		/* Get the target's name (or "it") */
		monster_desc(t_name, t_ptr, 0x00);

		/* Hack -- Get the "died from" name */
		monster_desc(ddesc, m_ptr, 0x88);

		/* Choose a spell to cast */
		thrown_spell = spell[rand_int(num)];


		see_m = seen;
		see_t = (!blind && t_ptr->ml);
		see_either = (see_m || see_t);
		see_both = (see_m && see_t);

		known = (m_ptr->cdis <= MAX_SIGHT) || (t_ptr->cdis <= MAX_SIGHT);

		switch (thrown_spell)
		{
		/* RF4_SHRIEK */
		case 1:
			{
				if (!direct) break;
				disturb(1, 0);
				if (!see_m) msg_print("You hear a shriek.");
				else msg_format("%^s shrieks at %s.", m_name, t_name);
#if 0
				aggravate_monsters(m_idx);
#endif
				wake_up = TRUE;
				break;
			}

		/* RF4_BR_WATE */
		case 2:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes water at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_WATER,
					((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)), 0, TRUE);
				break;
			}
		/* RF4_ARROW_5 */
		case 3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a loud thwang.", m_name);
			else msg_format("%^s fires a seeker arrow!", m_name);
			bolt(m_idx, GF_ARROW, damroll(8, 8));
			break;
		}
			{
				break;
			}

			/* RF4_XXX4X4 */
		case 4:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear an explosion!");
				else if (blind) msg_format("%^s shoots something.", m_name);
				else msg_format("%^s fires a rocket at %s.", m_name, t_name);
				monst_breath_monst(m_idx, y, x, GF_ROCKET,
					((m_ptr->hp / 4) > 800 ? 800 : (m_ptr->hp / 4)), 2, FALSE);
				break;
			}

			/* RF4_ARROW_1 */
		case 5:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear a strange noise.");
				else if (blind) msg_format("%^s makes a strange noise.", m_name);
				else msg_format("%^s fires an arrow at %s.", m_name, t_name);
				sound(SOUND_SHOOT);
				monst_bolt_monst(m_idx, y, x, GF_ARROW, damroll(1, 6));
				break;
			}

			/* RF4_ARROW_2 */
		case 6:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear a strange noise.");
				else if (blind) msg_format("%^s makes a strange noise.", m_name);
				else msg_format("%^s fires an arrow at %s.", m_name, t_name);
				sound(SOUND_SHOOT);
				monst_bolt_monst(m_idx, y, x, GF_ARROW, damroll(3, 6));
				break;
			}

			/* RF4_ARROW_3 */
		case 7:
			{
				disturb(1, 0);

				if (!see_either) msg_print("You hear a strange noise.");
				else if (blind) msg_format("%^s makes a strange noise.", m_name);
				else msg_format("%^s fires a missile at %s.", m_name, t_name);
				sound(SOUND_SHOOT);
				monst_bolt_monst(m_idx, y, x, GF_ARROW, damroll(5, 6));
				break;
			}

			/* RF4_ARROW_4 */
		case 8:
			{
				if (!see_either) msg_print("You hear a strange noise.");
				else disturb(1, 0);
				if (blind) msg_format("%^s makes a strange noise.", m_name);
				else msg_format("%^s fires a missile at %s.", m_name, t_name);
				sound(SOUND_SHOOT);
				monst_bolt_monst(m_idx, y, x, GF_ARROW, damroll(7, 6));
				break;
			}

			/* RF4_BR_ACID */
		case 9:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes acid at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_ACID,
					((m_ptr->hp / 3) > 1000 ? 1000 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_BR_ELEC */
		case 10:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes lightning at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_ELEC,
					((m_ptr->hp / 3) > 1000 ? 1000 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_BR_FIRE */
		case 11:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes fire at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_FIRE,
					((m_ptr->hp / 3) > 1000 ? 1000 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_BR_COLD */
		case 12:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes frost at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_COLD,
					((m_ptr->hp / 3) > 1000 ? 1000 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_BR_POIS */
		case 13:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes gas at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_POIS,
					((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_BR_NETH */
		case 14:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes nether at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_NETHER,
					((m_ptr->hp / 6) > 550 ? 550 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_LITE */
		case 15:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes light at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_LITE,
					((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_DARK */
		case 16:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes darkness at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_DARK,
					((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_CONF */
		case 17:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes confusion at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_CONFUSION,
					((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_SOUN */
		case 18:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes sound at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_SOUND,
					((m_ptr->hp / 6) > 800 ? 400 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_CHAO */
		case 19:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes chaos at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_CHAOS,
					((m_ptr->hp / 6) > 800 ? 600 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_DISE */
		case 20:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes disenchantment at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_DISENCHANT,
					((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_NEXU */
		case 21:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes nexus at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_NEXUS,
					((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_BR_TIME */
		case 22:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes time at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_TIME,
					((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_BR_INER */
		case 23:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes inertia at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_INERTIA,
					((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_GRAV */
		case 24:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes gravity at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_GRAVITY,
					((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_BR_SHAR */
		case 25:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes shards at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_SHARDS,
					((m_ptr->hp / 6) > 600 ? 600 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_PLAS */
		case 26:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes plasma at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_PLASMA,
					((m_ptr->hp / 6) > 800 ? 800 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_WALL */
		case 27:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes force at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_FORCE,
					((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)),0, TRUE);
				break;
			}

			/* RF4_BR_MANA */
		case 28:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes magical energy at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_MANA,
					((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_XXX5X4 */
		case 29:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear someone mumble.");
				else if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a ball of radiation at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_NUKE,
					(rlev + damroll(10, 6)), 2, FALSE);
				break;
			}

			/* RF4_XXX6X4 */
		case 30:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes toxic waste at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_NUKE,
					((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

			/* RF4_XXX7X4 */
		case 31:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear someone mumble frighteningly.");
				else if (blind) msg_format("%^s mumbles frighteningly.", m_name);
				else msg_format("%^s invokes raw Logrus upon %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_CHAOS,
					(rlev * 2) + damroll(10, 10), 4, FALSE);
				break;
			}

			/* RF4_XXX8X4 -> Breathe Disintegration */
		case 32:
			{
				disturb(1, 0);
				if (!see_either) msg_print("You hear breathing noise.");
				else if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes disintegration at %s.", m_name, t_name);
				sound(SOUND_BREATH);
				monst_breath_monst(m_idx, y, x, GF_DISINTEGRATE,
					((m_ptr->hp / 3) > 300 ? 300 : (m_ptr->hp / 3)),0, TRUE);
				break;
			}

		/* RF4_BO_SHAR */
		case 33:
		{
			if (!see_either)
			{
				if (known) msg_print("You hear gunfire!");
			}
			else if (blind)
			{
				msg_format("%^s shoots something.", m_name);
			}
			else
			{
				msg_format("%^s fires a shard at %s.", m_name, t_name);
			}
			monst_bolt_monst(m_idx, y, x, GF_SHARDS,
				damroll(3 + ((rlev - 1) / 10), 5));
			break;
		}

		/* RF4_BL_SHAR */
		case 34:
		{
			if (!see_either)
			{
				if (known) msg_print("You hear gunfire!");
			}
			else if (blind)
			{
				msg_format("%^s shoots something.", m_name);
			}
			else
			{
				msg_format("%^s fires shards at %s.", m_name, t_name);
			}
			monst_blast_monst(m_idx, y, x, GF_SHARDS,
				2 + ((rlev - 1) / 10), 4, 5, 3);
			break;
		}

		/* RF5_CH_SHARD */
		case 35:
		{
			if (!see_either)
			{
				if (known) msg_print("You hear gunfire!");
			}
			else if (blind)
			{
				msg_format("%^s shoots something.", m_name);
			}
			else
			{
				msg_format("%^s fires shards at %s.", m_name, t_name);
			}
			monst_blast_monst(m_idx, y, x, GF_SHARDS,
				3 + ((rlev - 1) / 10), 5, 10, 2);
			break;
		}

		/* RF6_BR_SKULL */
		case 36:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s shrieks.", m_name);
			else msg_format("%^s spawns a skull!", m_name);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev + 5, SUMMON_SKULL, FALSE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

			/* RF5_BA_ACID */
		case 65+0:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble.");
				else if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts an acid ball at %s.", m_name, t_name);
				monst_breath_monst(m_idx, y, x, GF_ACID, randint(rlev * 3) + 15, 2, FALSE);
				break;
			}

			/* RF5_BA_ELEC */
		case 65+1:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble.");
				else
					if (blind) msg_format("%^s mumbles.", m_name);
					else msg_format("%^s casts a lightning ball at %s.", m_name, t_name);
					monst_breath_monst(m_idx, y, x, GF_ELEC, randint(rlev * 3 / 2) + 8, 2, FALSE);
					break;
			}

			/* RF5_BA_FIRE */
		case 65+2:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble.");
				else
					if (blind) msg_format("%^s mumbles.", m_name);
					else msg_format("%^s casts a fire ball at %s.", m_name, t_name);
					monst_breath_monst(m_idx, y, x, GF_FIRE, randint(rlev * 7 / 2) + 10, 2, FALSE);
					break;
			}

			/* RF5_BA_COLD */
		case 65+3:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble.");
				else
					if (blind) msg_format("%^s mumbles.", m_name);
					else msg_format("%^s casts a frost ball at %s.", m_name, t_name);
					monst_breath_monst(m_idx, y, x, GF_COLD, randint(rlev * 3 / 2) + 10, 2, FALSE);
					break;
			}

			/* RF5_BA_POIS */
		case 65+4:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble.");
				else
					if (blind) msg_format("%^s mumbles.", m_name);
					else msg_format("%^s casts a stinking cloud at %s.", m_name, t_name);
					monst_breath_monst(m_idx, y, x, GF_POIS, damroll(12, 2), 2, FALSE);
					break;
			}

			/* RF5_BA_NETH */
		case 65+5:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble.");
				else
					if (blind) msg_format("%^s mumbles.", m_name);
					else msg_format("%^s casts a nether ball at %s.", m_name, t_name);
					monst_breath_monst(m_idx, y, x, GF_NETHER, (50 + damroll(10, 10) + rlev), 2, FALSE);
					break;
			}

			/* RF5_BA_WATE */
		case 65+6:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble.");
				else
					if (blind) msg_format("%^s mumbles.", m_name);
					else msg_format("%^s gestures fluidly at %s.", m_name, t_name);
					msg_format("%^s is engulfed in a whirlpool.", t_name);
					monst_breath_monst(m_idx, y, x, GF_WATER, randint(rlev * 5 / 2) + 50, 4, FALSE);
					break;
			}

			/* RF5_BA_MANA */
		case 65+7:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble powerfully.");
				else
					if (blind) msg_format("%^s mumbles powerfully.", m_name);
					else msg_format("%^s invokes a mana storm upon %s.", m_name, t_name);
					monst_breath_monst(m_idx, y, x, GF_MANA, (rlev * 5) + damroll(10, 10), 4, FALSE);
					break;
			}

			/* RF5_BA_DARK */
		case 65+8:
			{
				disturb(1, 0);
				if (!see_either) msg_print ("You hear someone mumble powerfully.");
				else
					if (blind) msg_format("%^s mumbles powerfully.", m_name);
					else msg_format("%^s invokes a darkness storm upon %s.", m_name, t_name);
					monst_breath_monst(m_idx, y, x, GF_DARK, (rlev * 5) + damroll(10, 10), 4, FALSE);
					break;
			}

			/* RF5_DRAIN_MANA */
		case 65+9:
			{
				/* Attack power */
				int r1 = (randint(rlev) / 2) + 1;

				if (see_m)
				{
					/* Basic message */
					msg_format("%^s draws psychic energy from %s.", m_name, t_name);
				}

				/* Heal the monster */
				if (m_ptr->hp < m_ptr->maxhp)
				{
					if (!(tr_ptr->flags4 || tr_ptr->flags5 || tr_ptr->flags6))
					{
						if (see_both)
							msg_format("%^s is unaffected!", t_name);
					}
					else
					{
						/* Heal */
						m_ptr->hp += (6 * r1);
						if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

						/* Redraw (later) if needed */
						if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

						/* Special message */
						if (seen)
						{
							msg_format("%^s appears healthier.", m_name);
						}
					}
				}

				wake_up = TRUE;
				break;
			}

			/* RF5_MIND_BLAST */
		case 65+10:
			{
				if (!direct) break;

				disturb(1, 0);

				if (!seen)
				{
					/* */
				}
				else
				{
					msg_format("%^s gazes intently at %s.", m_name, t_name);
				}

				/* Attempt a saving throw */
				if ((tr_ptr->flags1 & (RF1_UNIQUE)) ||
					(tr_ptr->flags3 & (RF3_NO_CONF)) ||
					(tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10))
				{
					/* Memorize a flag */
					if (tr_ptr->flags3 & (RF3_NO_CONF))
					{
						if (seen) tr_ptr->r_flags3 |= (RF3_NO_CONF);
					}

					/* No obvious effect */
					if (see_t)
					{
						msg_format("%^s is unaffected!", t_name);
					}
				}
				else
				{
					bool fear;
					msg_format("%^s is blasted by psionic energy.", t_name);
					t_ptr->confused += rand_int(4) + 4;

					mon_take_hit_mon(t_idx, damroll(8, 8), &fear, " collapses, a mindless husk.");
				}

				wake_up = TRUE;
				break;
			}

			/* RF5_BRAIN_SMASH */
		case 65+11:
			{
				if (!direct) break;
				disturb(1, 0);
				if (!seen) {
					/* */
				} else {
					msg_format("%^s gazes intently at %s.", m_name, t_name);
				}

				/* Attempt a saving throw */
				if ((tr_ptr->flags1 & (RF1_UNIQUE)) ||
					(tr_ptr->flags3 & (RF3_NO_CONF)) ||
					(tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)) {
					/* Memorize a flag */
					if (tr_ptr->flags3 & (RF3_NO_CONF)) {
						if (seen) tr_ptr->r_flags3 |= (RF3_NO_CONF);
					}
					/* No obvious effect */
					if (see_t)
					{
						msg_format("%^s is unaffected!", t_name);
					}
				} else {
					bool fear;
					if (see_t)
					{
						msg_format("%^s is blasted by psionic energy.", t_name);
					}
					t_ptr->confused += rand_int(4) + 4;
					t_ptr->mspeed -= rand_int(4) + 4;
					t_ptr->stunned += rand_int(4) + 4;
					mon_take_hit_mon(t_idx, damroll(12, 15), &fear, " collapses, a mindless husk.");
				}
				wake_up = TRUE;
				break;
			}

			/* RF5_CAUSE_1 */
		case 65+12:
			{
				if (!direct) break;
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s points at %s and curses.", m_name, t_name);
				if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{

					if (see_t) msg_format("%^s resists!", t_name);
				}
				else
				{
					bool fear;
					mon_take_hit_mon(t_idx, damroll(3, 8), &fear, " is destroyed.");
				}
				wake_up = TRUE;
				break;
			}

			/* RF5_CAUSE_2 */
		case 65+13:
			{
				if (!direct) break;
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s points at %s and curses horribly.", m_name, t_name);
				if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s resists!", t_name);
				}
				else
				{
					bool fear;
					mon_take_hit_mon(t_idx, damroll(8, 8), &fear, " bleeds to death.");
				}
				wake_up = TRUE;
				break;
			}

			/* RF5_CAUSE_3 */
		case 65+14:
			{
				if (!direct) break;
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s points at %s, incanting terribly!", m_name, t_name);
				if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s resists!", t_name);
				}
				else
				{
					bool fear;
					mon_take_hit_mon(t_idx, damroll(10, 15), &fear, " bleeds to death.");
				}
				break;
				wake_up = TRUE;
			}

			/* RF5_CAUSE_4 */
		case 65+15:
			{
				if (!direct) break;
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s points at %s, screaming the word 'DIE!'", m_name, t_name);
				if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s resists!", t_name);
				}
				else
				{
					bool fear;
					mon_take_hit_mon(t_idx, damroll(15, 15), &fear, " bleeds to death.");
				}
				wake_up = TRUE;
				break;
			}

			/* RF5_BO_ACID */
		case 65+16:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a acid bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_ACID,
					damroll(7, 8) + (rlev / 3));
				break;
			}

			/* RF5_BO_ELEC */
		case 65+17:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a lightning bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_ELEC,
					damroll(4, 8) + (rlev / 3));
				break;
			}

			/* RF5_BO_FIRE */
		case 65+18:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a fire bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_FIRE,
					damroll(9, 8) + (rlev / 3));
				break;
			}

			/* RF5_BO_COLD */
		case 65+19:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a frost bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_COLD,
					damroll(6, 8) + (rlev / 3));
				break;
			}

			/* RF5_BO_POIS */
		case 65+20:
			{
				/* XXX XXX XXX */
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a poison bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_POIS,
					damroll(8, 8) + (rlev / 3));
				break;
			}

			/* RF5_BO_NETH */
		case 65+21:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a nether bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_NETHER,
					30 + damroll(5, 5) + (rlev * 3) / 2);
				break;
			}

			/* RF5_BO_WATE */
		case 65+22:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a water bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_WATER,
					damroll(10, 10) + (rlev));
				break;
			}

			/* RF5_BO_MANA */
		case 65+23:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a mana bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_MANA,
					randint(rlev * 7 / 2) + 50);
				break;
			}

			/* RF5_BO_PLAS */
		case 65+24:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a plasma bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_PLASMA,
					10 + damroll(8, 7) + (rlev));
				break;
			}

			/* RF5_BO_ICEE */
		case 65+25:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts an ice bolt at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_ICE,
					damroll(6, 6) + (rlev));
				break;
			}

			/* RF5_MISSILE */
		case 65+26:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a magic missile at %s.", m_name, t_name);
				monst_bolt_monst(m_idx, y, x, GF_MISSILE,
					damroll(2, 6) + (rlev / 3));
				break;
			}

			/* RF5_SCARE */
		case 65+27:
                        {
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a fearful illusion in front of %s.", m_name, t_name);
					}
					else
					{
						msg_format("%^s mumbles, and you hear scary noises.", m_name);
					}
				}

				if (tr_ptr->flags3 & RF3_NO_FEAR)
				{
					if (see_t) msg_format("%^s refuses to be frightened.", t_name);
				}
				else if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s refuses to be frightened.", t_name);
				}
				else
				{
					if (!t_ptr->monfear);
					t_ptr->monfear += rand_int(4) + 4;
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_BLIND */
		case 65+28:
			{
				if (!direct) break;
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a spell, burning %s%s eyes.", m_name, t_name,
					(!strcmp(t_name, "it") ? "s" : "'s"));
				if (tr_ptr->flags3 & RF3_NO_CONF)  /* Simulate blindness with confusion */
				{
					if (see_t) msg_format("%^s is unaffected.", t_name);
				}
				else if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s is unaffected.", t_name);
				}
				else
				{
					if (see_t)   msg_format("%^s is blinded!", t_name);
					t_ptr->confused += 12 + (byte)rand_int(4);
				}
				wake_up = TRUE;
				break;

			}

			/* RF5_CONF */
		case 65+29:
			{
				if (!direct) break;
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
				else msg_format("%^s creates a mesmerising illusion in front of %s.", m_name, t_name);
				if (tr_ptr->flags3 & RF3_NO_CONF)
				{
					if (see_t) msg_format("%^s disbelieves the feeble spell.", t_name);
				}
				else if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s disbelieves the feeble spell.", t_name);
				}
				else
				{
					if (see_t)    msg_format("%^s seems confused.", t_name);
					t_ptr->confused += 12 + (byte)rand_int(4);
				}
				wake_up = TRUE;
				break;
			}

			/* RF5_SLOW */
		case 65+30:
			{
				if (!direct) break;
				disturb(1, 0);
				if (!blind && see_either) msg_format("%^s drains power from %s%s muscles.", m_name, t_name,
					(!strcmp(t_name, "it") ? "s" : "'s"));
				if (tr_ptr->flags1 & RF1_UNIQUE)
				{
					if (see_t) msg_format("%^s is unaffected.", t_name);
				}
				else if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s is unaffected.", t_name);
				}
				else
				{
					t_ptr->mspeed -= 10;
					if (see_t) msg_format("%^s starts moving slower.", t_name);
				}
				wake_up = TRUE;
				break;
			}

			/* RF5_HOLD */
		case 65+31:
			{
				if (!direct) break;
				disturb(1, 0);
				if (!blind && see_m) msg_format("%^s stares intently at %s.", m_name, t_name);
				if ((tr_ptr->flags1 & RF1_UNIQUE) ||
					(tr_ptr->flags3 & RF3_NO_STUN))
				{
					if (see_t) msg_format("%^s is unaffected.", t_name);
				}
				else if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s is unaffected.", t_name);
				}
				else
				{
					t_ptr->stunned += randint(4) + 4;
					if (see_t) msg_format("%^s is paralyzed!", t_name);
				}
				wake_up = TRUE;
				break;
			}


			/* RF6_HASTE */
		case 129+0:
			{
				disturb(1, 0);
				if (blind || !see_m)
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
					if (see_m) msg_format("%^s starts moving faster.", m_name);
					m_ptr->mspeed += 10;
				}

				/* Allow small speed increases to base+20 */
				else if (m_ptr->mspeed < r_ptr->speed + 20)
				{
					if (see_m) msg_format("%^s starts moving faster.", m_name);
					m_ptr->mspeed += 2;
				}

				break;
			}

			/* RF6_HAND_DOOM */
		case 129+1:
			{
				if (!direct) break;
				disturb(1, 0);
				if (!see_m) msg_print("You hear someone invoke the Hand of Doom!");
				else if (!blind) msg_format("%^s invokes the Hand of Doom on %s.", m_name, t_name);
				else
					msg_print ("You hear someone invoke the Hand of Doom!");
				if (tr_ptr->flags1 & RF1_UNIQUE)
				{
					if(!blind && see_t) msg_format("^%s is unaffected!", t_name);
				}
				else
				{
					if (((r_ptr->level) + randint(20)) >
						((tr_ptr->level) + 10 + randint(20)))
					{
						t_ptr->hp = t_ptr->hp
							- (((s32b) ((65 + randint(25)) * (t_ptr->hp))) / 100);
						if (t_ptr->hp < 1) t_ptr->hp = 1;
					}
					else
					{
						if (see_t) msg_format("%^s resists!", t_name);
					}
				}

				wake_up = TRUE;
				break;
			}

			/* RF6_HEAL */
		case 129+2:
			{
				disturb(1, 0);

				/* Message */
				if (blind || !see_m)
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
				if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

				/* Cancel fear */
				if (m_ptr->monfear)
				{
					/* Cancel fear */
					m_ptr->monfear = 0;

					/* Message */
					if (see_m) msg_format("%^s recovers %s courage.", m_name, m_poss);
				}

				break;
			}

		 /* RF6_HEAL2 */
		case 129+3:
		{
			disturb(1, 0);

			/* Message */
			if (blind)
			{
				msg_format("%^s mumbles.", m_name);
			}
			else
			{
	   msg_format(
	   "%^s fades out of existence and rematerializes far healthier.",
	    m_name);
			}

			 /* Heal some (int avoids overflow) */
			 k = m_ptr->hp;
			 k += m_ptr->maxhp * rlev / (50 + randint(400));

			/* Fully healed */
			 if (k >= m_ptr->maxhp)
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
		  msg_format("%^s looks much healthier.", m_name);
				else
		  msg_format("%^s sounds much healthier.", m_name);
				 m_ptr->hp = k;
			}

			/* Redraw (later) if needed */
			if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

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

			/* RF6_BLINK */
		case 129+4:
			{
				disturb(1, 0);
				if (see_m) msg_format("%^s blinks away.", m_name);
				teleport_away(m_idx, 10);
				break;
			}

			/* RF6_TPORT */
		case 129+5:
			{
				disturb(1, 0);
				if (see_m) msg_format("%^s teleports away.", m_name);
				teleport_away(m_idx, MAX_SIGHT * 2 + 5);
				break;
			}

		/* RF6_BR_WIND */
		case 129+6:
		{
			disturb(1, 0);
			if (!see_either) msg_print ("You hear someone hissing.");
			else if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes at %s.", m_name, t_name);
			monst_breath_monst(m_idx, y, x, GF_WIND,
					damroll(7, 8) + (rlev / 3), 0, TRUE);
			break;
		}

		/* RF6_BA_WIND */
		case 129+7:
		{
			disturb(1, 0);
			if (!see_either) msg_print ("You hear calling out.");
			else if (blind) msg_format("%^s calls out.", m_name);
			else msg_format("%^s calls out at %s.", m_name, t_name);
			monst_breath_monst(m_idx, y, x, GF_WIND,
					 randint(rlev * 5 / 2) + 50, 0, FALSE);
			break;
		}

			/* RF6_TELE_TO */
		case 129+8:
			{
				/* Not implemented */
				break;
			}

			/* RF6_TELE_AWAY */
		case 129+9:
			{

				if (!direct) break;
				else
				{
					bool resists_tele = FALSE;
					disturb(1, 0);
					msg_format("%^s teleports %s away.", m_name, t_name);


					if (tr_ptr->flags3 & (RF3_RES_TELE))
					{
						if (tr_ptr->flags1 & (RF1_UNIQUE))
						{
							if (see_t)
							{
								tr_ptr->r_flags3 |= RF3_RES_TELE;
								msg_format("%^s is unaffected!", t_name);
							}
							resists_tele = TRUE;
						}
						else if (tr_ptr->level > randint(100))
						{
							if (see_t)
							{
								tr_ptr->r_flags3 |= RF3_RES_TELE;
								msg_format("%^s resists!", t_name);
							}
							resists_tele = TRUE;
						}
					}

					if (!resists_tele)
					{
						teleport_away(t_idx, MAX_SIGHT * 2 + 5);
					}
				}

				break;
			}

			/* RF6_TELE_LEVEL */
		case 129+10:
			{
				/* Not implemented */
				break;
			}

			/* RF6_DOOR */
		case 129+11:
			{
			    break;
			}

			/* RF6_DARKNESS */
		case 129+12:
			{
				if (!direct) break;
				disturb(1, 0);
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s gestures in shadow.", m_name);
				if (seen)
					msg_format("%^s is surrounded by darkness.", t_name);
				(void)project(m_idx, 3, y, x, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
				/* Lite up the room */
				unlite_room(y, x);
				break;
			}

			/* RF6_TRAPS */
		case 129+13:
			{
				/* Not implemented */
				break;
			}

			/* RF6_FORGET */
		case 129+14:
			{
				/* Not implemented */
				break;
			}

			/* RF6_XXX6X6 */
		case 129+15:
			{
				break;
			}

			/* RF6_SUMMON_KIN */
		case 129+16:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons %s %s.",
					m_name, m_poss,
					((r_ptr->flags1) & RF1_UNIQUE ?
					"minions" : "kin"));
				summon_kin_type = r_ptr->d_char; /* Big hack */
				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_KIN, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear many things appear nearby.");


				break;
			}

			/* RF6_S_CYBER */
		case 129+17:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons Cyberdemons!", m_name);
				if (blind && count) msg_print("You hear heavy steps nearby.");
				if (friendly)
					summon_specific(y, x, rlev, SUMMON_CYBER, TRUE, TRUE, pet);
				else
					summon_cyber();
				break;
			}

			/* RF6_S_MONSTER */
		case 129+18:
			{
				int type = (friendly ? SUMMON_NO_UNIQUES : 0);

				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons help!", m_name);

				count += summon_specific(y, x, rlev, type, FALSE, friendly, pet);

				if (blind && count) msg_print("You hear something appear nearby.");
				break;
			}

			/* RF6_S_MONSTERS */
		case 129+19:
			{
				int type = (friendly ? SUMMON_NO_UNIQUES : 0);

				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons monsters!", m_name);

				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, type, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear many things appear nearby.");
				break;
			}

			/* RF6_S_ANT */
		case 129+20:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons ants.", m_name);
				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_ANT, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear many things appear nearby.");
				break;
			}

			/* RF6_S_SPIDER */
		case 129+21:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons spiders.", m_name);
				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_SPIDER, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear many things appear nearby.");
				break;
			}

			/* RF6_S_HOUND */
		case 129+22:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons hounds.", m_name);
				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_HOUND, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear many things appear nearby.");
				break;
			}

			/* RF6_S_HYDRA */
		case 129+23:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons hydras.", m_name);
				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_HYDRA, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear many things appear nearby.");
				break;
			}

			/* RF6_S_ANGEL */
		case 129+24:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons an angel!", m_name);
				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_ANGEL, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear something appear nearby.");
				break;
			}

			/* RF6_S_DEMON */
		case 129+25:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons a demon!", m_name);
				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_DEMON, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear something appear nearby.");
				break;
			}

			/* RF6_S_UNDEAD */
		case 129+26:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons an undead adversary!", m_name);
				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_UNDEAD, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear something appear nearby.");
				break;
			}

			/* RF6_S_DRAGON */
		case 129+27:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons a dragon!", m_name);
				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_DRAGON, TRUE, friendly, pet);
				}
				if (blind && count) msg_print("You hear something appear nearby.");
				break;
			}

			/* RF6_S_HI_UNDEAD */
		case 129+28:
			{
				int type = (friendly ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_HI_UNDEAD);

				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons greater undead!", m_name);
				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, type, TRUE, friendly, pet);
				}
				if (blind && count)
				{
					msg_print("You hear many creepy things appear nearby.");
				}
				break;
			}

			/* RF6_S_HI_DRAGON */
		case 129+29:
			{
				int type = (friendly ? SUMMON_HI_DRAGON_NO_UNIQUES : SUMMON_HI_DRAGON);

				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons ancient dragons!", m_name);
				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, type, TRUE, friendly, pet);
				}
				if (blind && count)
				{
					msg_print("You hear many powerful things appear nearby.");
				}
				break;
			}

			/* RF6_S_WRAITH */
		case 129+30:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons wraiths!", m_name);

				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_WRAITH, TRUE, FALSE, FALSE);
				}
				if (blind && count)
				{
					msg_print("You hear powerful beings appear nearby.");
				}
				break;
			}

			/* RF6_S_UNIQUE */
		case 129+31:
			{
				disturb(1, 0);
				if (blind || !see_m) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s magically summons special opponents!", m_name);
				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_UNIQUE, TRUE, FALSE, FALSE);
				}
				if (blind && count)
				{
					msg_print("You hear many powerful things appear nearby.");
				}
				break;
			}

		/* RF6_S_DOOM */
		case 129+32:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a minion of Id!", m_name);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev + 5, SUMMON_DOOM, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF6_S_HI_DOOM */
		case 129+33:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons minions of Id!", m_name);

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, rlev + 5, SUMMON_HI_DOOM, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

   }

   if (wake_up)
   {
	   t_ptr->csleep = 0;
   }


   /* Remember what the monster did, if we saw it */
   if (seen)
   {
       /* Inate spell */
       if (thrown_spell < 65)
       {
	   r_ptr->r_flags4 |= (1LL << (thrown_spell - 1));
	   if (r_ptr->r_cast_inate < MAX_UCHAR) r_ptr->r_cast_inate++;
       }

       /* Bolt or Ball */
       else if (thrown_spell < 129)
       {
	   r_ptr->r_flags5 |= (1LL << (thrown_spell - 65));
	   if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
       }

       /* Special spell */
       else if (thrown_spell < 193)
       {
	   r_ptr->r_flags6 |= (1LL << (thrown_spell - 129));
	   if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
       }
   }

   /* Always take note of monsters that kill you ---
   * even accidentally */
   if (death && (r_ptr->r_deaths < MAX_SHORT)) {
       r_ptr->r_deaths++;
   }

   /* A spell was cast */
   return (TRUE);
     }

     /* No enemy found */
     return (FALSE);
 }


void curse_equipment(int chance, int heavy_chance)
{
	bool changed = FALSE;
	u64b    o1, o2, o3;
	object_type * o_ptr = &inventory[INVEN_WIELD + rand_int(12)];

	if (randint(100) > chance) return;

	if (!(o_ptr->k_idx)) return;

	object_flags(o_ptr, &o1, &o2, &o3);


	/* Extra, biased saving throw for blessed items */
	if ((o3 & (TR3_BLESSED)) && (randint(888) > chance))
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
		if (!(o_ptr->ident & (IDENT_CURSED)))
			changed = TRUE;
		o_ptr->art_flags3 |= TR3_CURSED;
		o_ptr->ident |= IDENT_CURSED;
	}

	if (changed)
	{
		msg_print("There is a malignant black aura surrounding you...");
		if (o_ptr->note)
		{
			if (streq(quark_str(o_ptr->note), "uncursed"))
			{
				o_ptr->note = 0;
			}
		}
	}
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
	byte            spell[192], num = 0;
	u64b            f4, f5, f6;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	char            m_name[80];
	char            m_poss[80];
	char            m_ref[80];
	char            ddesc[80];
	bool            no_inate = FALSE, prevented = FALSE;

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

	/* if player is invisible and monster can't see invisible, no spell
	    attack is made -KMW- */
	if ((p_invis) &&
	    (!(r_ptr->flags3 & (RF3_UNDEAD))) &&
	    (!(r_ptr->flags3 & (RF3_DEMON))) &&
	    (!(r_ptr->flags2 & (RF2_INVISIBLE))))
		return (FALSE);

	/* Cannot cast spells when nice */
	if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);
	if (is_pet(m_ptr) || is_friendly(m_ptr)) return (FALSE);

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
		if (rand_int(100) >=  chance) return (FALSE);

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

#if 0
	if (!stupid_monsters)
	{
		/* Forbid inate attacks sometimes */
		if (no_inate) f4 = 0L;
	}
#endif

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
		if ((f4&(RF4_BOLT_MASK) || f5 & (RF5_BOLT_MASK) ||
		     f6&(RF6_BOLT_MASK)) &&
		     !(r_ptr->flags2 & (RF2_STUPID)) &&
		     !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE))
		{
			/* Remove spells that will only hurt friends */
			f4 &= ~(RF4_BOLT_MASK);
			f5 &= ~(RF5_BOLT_MASK);
			f6 &= ~(RF6_BOLT_MASK);
		}

		/* Check for a possible summon */
		if ((f4 & (RF4_SUMMON_MASK) || f5 & (RF5_SUMMON_MASK) ||
		     f6 & (RF6_SUMMON_MASK)) &&
		     !(r_ptr->flags2 & (RF2_STUPID)) &&
		     !(summon_possible(py,px)))
		{
			/* Remove summoning spells */
			f4 &= ~(RF4_SUMMON_MASK);
			f5 &= ~(RF5_SUMMON_MASK);
			f6 &= ~(RF6_SUMMON_MASK);
		}

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

	/* Extract the "inate" spells */
	for (k = 0; k < 64; k++)
	{
		if (f4 & (1LL << k)) spell[num++] = k + 1;
	}

	/* Extract the "normal" spells */
	for (k = 0; k < 64; k++)
	{
		if (f5 & (1LL << k)) spell[num++] = k + 65;
	}

	/* Extract the "bizarre" spells */
	for (k = 0; k < 64; k++)
	{
		if (f6 & (1LL << k)) spell[num++] = k + 129;
	}

	/* No spells left */
	if (!num) return (FALSE);

	/* Stop if player is dead or gone */
	if (!alive || death) return (FALSE);

	/* Stop if player is leaving */
	if (p_ptr->leaving) return (FALSE);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, 0x22);

	/* Get the monster reflexive ("himself"/"herself"/"itself") */
	monster_desc(m_ref, m_ptr, 0x23);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, 0x88);

	if (stupid_monsters)
	{
		/* Choose a spell to cast */
		thrown_spell = spell[rand_int(num)];
	}
	else
	{
		thrown_spell = choose_attack_spell(m_idx, spell, num);

		/* Abort if no spell was chosen */
		if (!thrown_spell) return (FALSE);

		/* Calculate spell failure rate */
		failrate = 25 - (rlev + 3) / 4;

		/* Hack -- Stupid monsters will never fail (for jellies and such) */
		if (r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

		/* Antimagic amulets give radius 3 ''shell'' */
		if ((p_anti_magic) && (distance(py, px, m_ptr->fy, m_ptr->fx) <= 3))
		{
		    failrate = 100;
		    prevented = TRUE;
		}

		/* Check for spell failure (inate attacks never fail) */
		if ((thrown_spell >= 65) && (rand_int(100) < failrate))
		{
			/* Message */
			if (prevented)
			msg_format("%^s tries to cast a spell, but is hindered by your anti-magic field.", m_name);
			else
			msg_format("%^s tries to cast a spell, but fails.", m_name);

			return (TRUE);
		}
	}

	/* Cast the spell. */
	switch (thrown_spell)
	{
		 /* RF4_SHRIEK */
	         case 1:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 if (r_ptr->flags2 & (RF2_SMART))
				msg_format("%^s shouts for help.", m_name);
			 else
				msg_format("%^s makes a high pitched shriek.", m_name);
			 aggravate_monsters(m_idx);
			 break;
		 }

		 /* RF4_BR_WATE */
		case 2:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes water.", m_name);
			 breath(m_idx, GF_WATER,
				 ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)),0, TRUE);
			 break;
		 }

		/* RF4_ARROW_5 */
		case 3:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a loud thwang.", m_name);
			else msg_format("%^s fires a seeker arrow!", m_name);
			bolt(m_idx, GF_ARROW, damroll(8, 8));
			break;
		}

		 /* RF4_ROCKET */
        	 case 4:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s shoots something.", m_name);
			 else msg_format("%^s fires a rocket.", m_name);
			 breath(m_idx, GF_ROCKET,
				 ((m_ptr->hp / 4) > 800 ? 800 : (m_ptr->hp / 4)), 2, FALSE);
			 update_smart_learn(m_idx, DRS_SHARD);
			 break;
		 }

		 /* RF4_ARROW_1 */
        	 case 5:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s makes a strange noise.", m_name);
			 else msg_format("%^s fires an arrow.", m_name);
			 bolt(m_idx, GF_ARROW, damroll(1, 6));
			 update_smart_learn(m_idx, DRS_REFLECT);
			 break;
		 }

		 /* RF4_ARROW_2 */
	 case 6:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s makes a strange noise.", m_name);
			 else msg_format("%^s fires an arrow!", m_name);
			 bolt(m_idx, GF_ARROW, damroll(3, 6));
			 update_smart_learn(m_idx, DRS_REFLECT);
			 break;
		 }

		 /* RF4_ARROW_3 */
	 case 7:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s makes a strange noise.", m_name);
			 else msg_format("%^s fires a missile.", m_name);
			 bolt(m_idx, GF_ARROW, damroll(5, 6));
			 update_smart_learn(m_idx, DRS_REFLECT);
			 break;
		 }

		 /* RF4_ARROW_4 */
	 case 8:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s makes a strange noise.", m_name);
			 else msg_format("%^s fires a missile!", m_name);
			 bolt(m_idx, GF_ARROW, damroll(7, 6));
			 update_smart_learn(m_idx, DRS_REFLECT);
			 break;
		 }

		 /* RF4_BR_ACID */
	 case 9:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes acid.", m_name);
			 breath(m_idx, GF_ACID,
				 ((m_ptr->hp / 3) > 1000 ? 1000 : (m_ptr->hp / 3)), 0, TRUE);
			 update_smart_learn(m_idx, DRS_ACID);
			 break;
		 }

		 /* RF4_BR_ELEC */
	 case 10:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes lightning.", m_name);
			 breath(m_idx, GF_ELEC,
				 ((m_ptr->hp / 3) > 1000 ? 1000 : (m_ptr->hp / 3)),0, TRUE);
			 update_smart_learn(m_idx, DRS_ELEC);
			 break;
		 }

		 /* RF4_BR_FIRE */
	 case 11:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes fire.", m_name);
			 breath(m_idx, GF_FIRE,
				 ((m_ptr->hp / 3) > 1000 ? 1000 : (m_ptr->hp / 3)),0, TRUE);
			 update_smart_learn(m_idx, DRS_FIRE);
			 break;
		 }

		 /* RF4_BR_COLD */
	 case 12:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes frost.", m_name);
			 breath(m_idx, GF_COLD,
				 ((m_ptr->hp / 3) > 1000 ? 1000 : (m_ptr->hp / 3)),0, TRUE);
			 update_smart_learn(m_idx, DRS_COLD);
			 break;
		 }

		 /* RF4_BR_POIS */
	 case 13:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes gas.", m_name);
			 breath(m_idx, GF_POIS,
				 ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)),0, TRUE);
			 update_smart_learn(m_idx, DRS_POIS);
			 break;
		 }


		 /* RF4_BR_NETH */
	 case 14:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes nether.", m_name);
			 breath(m_idx, GF_NETHER,
				 ((m_ptr->hp / 6) > 550 ? 550 : (m_ptr->hp / 6)),0, TRUE);
			 update_smart_learn(m_idx, DRS_NETH);
			 break;
		 }

		 /* RF4_BR_LITE */
	 case 15:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes light.", m_name);
			 breath(m_idx, GF_LITE,
				 ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)),0, TRUE);
			 update_smart_learn(m_idx, DRS_LITE);
			 break;
		 }

		 /* RF4_BR_DARK */
	 case 16:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes darkness.", m_name);
			 breath(m_idx, GF_DARK,
				 ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)),0, TRUE);
			 update_smart_learn(m_idx, DRS_DARK);
			 break;
		 }

		 /* RF4_BR_CONF */
	 case 17:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes confusion.", m_name);
			 breath(m_idx, GF_CONFUSION,
				 ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)),0, TRUE);
			 update_smart_learn(m_idx, DRS_CONF);
			 break;
		 }

		 /* RF4_BR_SOUN */
	 case 18:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes sound.", m_name);
			 breath(m_idx, GF_SOUND,
				 ((m_ptr->hp / 6) > 800 ? 800 : (m_ptr->hp / 6)),0, TRUE);
			 update_smart_learn(m_idx, DRS_SOUND);
			 break;
		 }

		 /* RF4_BR_CHAO */
	 case 19:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes chaos.", m_name);
			 breath(m_idx, GF_CHAOS,
				 ((m_ptr->hp / 6) > 800 ? 800 : (m_ptr->hp / 6)),0, TRUE);
			 update_smart_learn(m_idx, DRS_CHAOS);
			 break;
		 }

		 /* RF4_BR_DISE */
	 case 20:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes disenchantment.", m_name);
			 breath(m_idx, GF_DISENCHANT,
				 ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6)),0, TRUE);
			 update_smart_learn(m_idx, DRS_DISEN);
			 break;
		 }

		 /* RF4_BR_NEXU */
	 case 21:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes nexus.", m_name);
			 breath(m_idx, GF_NEXUS,
				 ((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)),0, TRUE);
			 update_smart_learn(m_idx, DRS_NEXUS);
			 break;
		 }

		 /* RF4_BR_TIME */
	 case 22:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes time.", m_name);
			 breath(m_idx, GF_TIME,
				 ((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3)),0, TRUE);
			 break;
		 }

		 /* RF4_BR_INER */
	 case 23:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes inertia.", m_name);
			 breath(m_idx, GF_INERTIA,
				 ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)),0, TRUE);
			 break;
		 }

		 /* RF4_BR_GRAV */
	 case 24:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes gravity.", m_name);
			 breath(m_idx, GF_GRAVITY,
				 ((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)),0, TRUE);
			 break;
		 }

		 /* RF4_BR_SHAR */
	 case 25:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes shards.", m_name);
			 breath(m_idx, GF_SHARDS,
				 ((m_ptr->hp / 6) > 600 ? 400 : (m_ptr->hp / 6)),0, TRUE);
			 update_smart_learn(m_idx, DRS_SHARD);
			 break;
		 }

		 /* RF4_BR_PLAS */
	 case 26:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes plasma.", m_name);
			 breath(m_idx, GF_PLASMA,
				 ((m_ptr->hp / 6) > 800 ? 800 : (m_ptr->hp / 6)),0, TRUE);
			 break;
		 }

		 /* RF4_BR_WALL */
	 case 27:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes force.", m_name);
			 breath(m_idx, GF_FORCE,
				 ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)),0, TRUE);
			 break;
		 }

		 /* RF4_BR_MANA */
	 case 28:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes magical energy.", m_name);
			 breath(m_idx, GF_MANA,
				 ((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)),0, TRUE);
			 break;
		 }

		 /* RF4_XXX5X4 */
	 case 29:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s casts a ball of radiation.", m_name);
			 breath(m_idx, GF_NUKE, (rlev + damroll(10, 6)), 2, FALSE);
			 update_smart_learn(m_idx, DRS_POIS);
			 break;
		 }

		 /* RF4_XXX6X4 */
	 case 30:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes toxic waste.", m_name);
			 breath(m_idx, GF_NUKE,
				 ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)),0, TRUE);
			 update_smart_learn(m_idx, DRS_POIS);
			 break;
		 }

		 /* RF4_XXX7X4 */
	 case 31:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles frighteningly.", m_name);
			 else msg_format("%^s invokes raw Logrus.", m_name);
			 breath(m_idx, GF_CHAOS, (rlev * 2) + damroll(10, 10), 4, FALSE);
			 update_smart_learn(m_idx, DRS_CHAOS);
			 break;
		 }

		 /* RF4_XXX8X4 -> Disintegration breath! */
	 case 32:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s breathes.", m_name);
			 else msg_format("%^s breathes disintegration.", m_name);
			 breath(m_idx, GF_DISINTEGRATE,
				 ((m_ptr->hp / 3) > 300 ? 300 : (m_ptr->hp / 3)),0, TRUE);
			 break;
		 }
	case 33: /* RF4_BO_SHAR */
		{
			disturb(1, 0);
			if (blind) msg_format("%^s shoots something.", m_name);
			else msg_format("%^s fires a shard.", m_name);
			bolt(m_idx, GF_SHARDS, damroll(3 + ((rlev - 1) / 10), 5));
			update_smart_learn(m_idx, DRS_SHARD);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}
	case 34:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s shoots something.", m_name);
			else msg_format("%^s fires shards.", m_name);
			blast(m_idx, GF_SHARDS, 2 + ((rlev - 1) / 10), 4, 5, 3);
			update_smart_learn(m_idx, DRS_SHARD);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}
         case 35: /* RF4_CH_SHAR */
		{
			disturb(1, 0);
			if (blind) msg_format("%^s fires many things.", m_name);
			else msg_format("%^s fires shards!", m_name);
			blast(m_idx, GF_SHARDS, 3 + ((rlev - 1) / 10), 5, 10, 2);
			update_smart_learn(m_idx, DRS_SHARD);
			update_smart_learn(m_idx, DRS_REFLECT);
			break;
		}

	case 36:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s shrieks.", m_name);
			else msg_format("%^s spawns a skull!", m_name);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev + 5, SUMMON_SKULL, FALSE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}


		 /* RF5_BA_ACID */
	 case 65+0:
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
	 case 65+1:
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
	 case 65+2:
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
	 case 65+3:
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
	 case 65+4:
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
	 case 65+5:
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
	 case 65+6:
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
	 case 65+7:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles powerfully.", m_name);
			 else msg_format("%^s invokes a mana storm.", m_name);
			 breath(m_idx, GF_MANA,
				 (rlev * 5) + damroll(10, 10), 4, FALSE);
			 break;
		 }

		 /* RF5_BA_DARK */
	 case 65+8:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles powerfully.", m_name);
			 else msg_format("%^s invokes a darkness storm.", m_name);
			 breath(m_idx, GF_DARK,
				 (rlev * 5) + damroll(10, 10), 4, FALSE);
			 update_smart_learn(m_idx, DRS_DARK);
			 break;
		 }

		 /* RF5_DRAIN_MANA */
	 case 65+9:
		 {
			 if (!direct) break;
			 if (p_ptr->csp)
			 {
				 int r1;

				 /* Disturb if legal */
				 disturb(1, 0);

				 /* Resist */

				 if ((p_ptr->mind & MIND_MENTAL_BARRIER) &&
				    (randint(2) == 1))
				 {
				 msg_format("Your barrier prevents %^s from draining your psychic energy!", m_name);
				 break;
				 }

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
					 if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

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
	 case 65+10:
		 {
			 int dam = damroll (8,8);
			 if (p_ptr->sign == SIGN_CANCER) dam /= 2;

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
			 else if ((p_ptr->mind & MIND_MENTAL_BARRIER) && rand_int(3))
			      msg_print("Your mental barrier deflects the attack.");
			 else
			 {
				 msg_print("Your mind is blasted by psionic energy.");

				 if (!(p_resist_conf || p_ptr->tim_res_conf))
				 {
					 (void)set_confused(p_ptr->confused + rand_int(4) + 4);
				 }

				 if ((!p_resist_chaos) && (randint(3)==1))
				 {
					 (void) set_image(p_ptr->image + rand_int(250) + 150);
				 }

				 take_hit(dam, ddesc);
			 }
			 break;
		 }

		 /* RF5_BRAIN_SMASH */
	 case 65+11:
		 {
			 int dam = damroll (10,10);
			 if (p_ptr->sign == SIGN_CANCER) dam /= 2;

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
			 else if ((p_ptr->mind & MIND_MENTAL_BARRIER) && rand_int(3))
			      msg_print("Your mental barrier deflects the attack.");
			 else
			 {
				 msg_print("Your mind is blasted by psionic energy.");
				 take_sanity_hit(dam, "going insane");
				 if (!(p_resist_blind || p_ptr->tim_res_blind))
				 {
					 (void)set_blind(p_ptr->blind + 8 + rand_int(8));
				 }
				 if (!(p_resist_conf || p_ptr->tim_res_conf))
				 {
					 (void)set_confused(p_ptr->confused + rand_int(4) + 4);
				 }
				 if (!p_free_act)
				 {
					 (void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
				 }
				 (void)set_slow(p_ptr->slow + rand_int(4) + 4);

				 while (rand_int(100) > p_ptr->skill_sav)
					 (void)do_dec_stat(A_INT, STAT_DEC_NORMAL);
				 while (rand_int(100) > p_ptr->skill_sav)
					 (void)do_dec_stat(A_WIS, STAT_DEC_NORMAL);

				 if (!p_resist_chaos)
				 {
					 (void) set_image(p_ptr->image + rand_int(250) + 150);
				 }
			 }
			 break;
		 }

		 /* RF5_CAUSE_1 */
	 case 65+12:
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
	 case 65+13:
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
	 case 65+14:
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
	 case 65+15:
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
				 curse_equipment(90, 25);
				 take_hit(damroll(15, 15), ddesc);
				 (void)set_cut(p_ptr->cut + damroll(10, 10));
			 }
			 break;
		 }

		 /* RF5_BO_ACID */
	 case 65+16:
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
	 case 65+17:
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
	 case 65+18:
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
	 case 65+19:
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
	 case 65+20:
		 {
			 /* XXX XXX XXX */
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s casts a poison bolt.", m_name);
			 bolt(m_idx, GF_POIS, damroll(8, 8) + (rlev / 3));
			 update_smart_learn(m_idx, DRS_POIS);
			 update_smart_learn(m_idx, DRS_REFLECT);
			 break;
		 }

		 /* RF5_BO_NETH */
	 case 65+21:
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
	 case 65+22:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s casts a water bolt.", m_name);
			 bolt(m_idx, GF_WATER, damroll(10, 10) + (rlev));
			 update_smart_learn(m_idx, DRS_REFLECT);
			 break;
		 }

		 /* RF5_BO_MANA */
	 case 65+23:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s casts a mana bolt.", m_name);
			 bolt(m_idx, GF_MANA, randint(rlev * 7 / 2) + 50);
			 update_smart_learn(m_idx, DRS_REFLECT);
			 break;
		 }

		 /* RF5_BO_PLAS */
	 case 65+24:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s casts a plasma bolt.", m_name);
			 bolt(m_idx, GF_PLASMA, 10 + damroll(8, 7) + (rlev));
			 update_smart_learn(m_idx, DRS_REFLECT);
			 break;
		 }

		 /* RF5_BO_ICEE */
	 case 65+25:
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
	 case 65+26:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s casts a magic missile.", m_name);
			 if (p_anti_magic)
			 {
			   msg_print("The missile bounces");
			 update_smart_learn(m_idx, DRS_REFLECT);
			 }
			 else
			 bolt(m_idx, GF_MISSILE, damroll(2, 6) + (rlev / 3));
			 break;
		 }

		 /* RF5_SCARE */
	 case 65+27:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
			 else msg_format("%^s casts a fearful illusion.", m_name);
			 if (p_resist_fear)
			 {
				 msg_print("You are too brave to be frightened.");
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
	 case 65+28:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s casts a spell, burning your eyes!", m_name);
			 if (p_resist_blind || p_ptr->tim_res_blind)
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
	 case 65+29:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			 else msg_format("%^s creates a mesmerising illusion.", m_name);
			 if (p_resist_conf || p_ptr->tim_res_conf)
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
	 case 65+30:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 msg_format("%^s drains power from your muscles!", m_name);
			 if (p_free_act)
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
	 case 65+31:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s stares deep into your eyes!", m_name);
			 if (p_free_act)
			 {
				 msg_print("You are unaffected!");
			 }
			 else if (rand_int(100) < p_ptr->skill_sav +
				  ((p_ptr->sign == SIGN_SAGGITARIUS) ? 20 : 0))
			 {
				 msg_format("You resist the effects!");
			 }
			 else if ((p_ptr->mind & MIND_MENTAL_BARRIER) && rand_int(3))
			      msg_print("Your mental barrier deflects the attack.");
			 else
			 {
				 (void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
			 }
			 update_smart_learn(m_idx, DRS_FREE);
			 break;
		 }



		 /* RF6_HASTE */
	 case 129+0:
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
	 case 129+1:
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
	 case 129+2:
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
			 if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

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

		 /* RF6_HEAL2 */
		case 129+3:
		{
			disturb(1, 0);

			/* Message */
			if (blind)
			{
				msg_format("%^s mumbles.", m_name);
			}
			else
			{
	   msg_format("%^s fades out of existence and rematerializes far healthier.",
		       m_name);
			}

			 /* Heal some (int avoids overflow) */
			 k = m_ptr->hp;
			 k += m_ptr->maxhp * rlev / (50 + randint(400));

			/* Fully healed */
			 if (k >= m_ptr->maxhp)
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
		  msg_format("%^s looks much healthier.", m_name);
				else
		  msg_format("%^s sounds much healthier.", m_name);
				 m_ptr->hp = k;
			}

			/* Redraw (later) if needed */
			if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

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

		 /* RF6_BLINK */
	 case 129+4:
		 {
			 disturb(1, 0);
			 if (!blind)
			 msg_format("%^s blinks away.", m_name);
			 teleport_away(m_idx, 10);
			 break;
		 }

		 /* RF6_TPORT */
	 case 129+5:
		 {
			 disturb(1, 0);
			 if (!blind)
			 msg_format("%^s teleports away.", m_name);
			 teleport_away(m_idx, MAX_SIGHT * 2 + 5);
			 break;
		 }

		/* RF4_BR_WIND */
	 case 129+6:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s breathes.", m_name);
			else
				msg_format("%^s breathes wind.", m_name);
			breath(m_idx, GF_WIND,
				   ((m_ptr->hp / 3) > 300 ? 300 : (m_ptr->hp / 3)), 0, TRUE);
			break;
		}

		/* RF4_BA_WIND */
	case 129+7:
		{
			disturb(1, 0);
			if (blind)
				msg_format("%^s calls out.", m_name);
			else
				msg_format("%^s calls forth a whirlwind.", m_name);
			breath(m_idx, GF_WIND,
				   randint(rlev * 5 / 2) + 50, 0, FALSE);
			break;
		}

		 /* RF6_TELE_TO */
	 case 129+8:
		 {
			 if (!direct) break;
			 if (m_ptr->monfear) break;
			 disturb(1, 0);
			 msg_format("%^s commands you to return.", m_name);
			 teleport_player_to(m_ptr->fy, m_ptr->fx);
			 break;
		 }

		 /* RF6_TELE_AWAY */
	 case 129+9:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 msg_format("%^s teleports you away.", m_name);
			 teleport_player(100);
			 break;
		 }

		 /* RF6_TELE_LEVEL */
	 case 129+10:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles strangely.", m_name);
			 else msg_format("%^s gestures at your feet.", m_name);
			 if (p_resist_nexus)
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

		 /* RF6_DOOR */
		 case 129+11:
		 {
			if (!direct) break;
			disturb(1, 0);
			if (!blind) msg_format("%^s opens a door to you.", m_name);
			teleport_to_player(m_idx);
			break;
		 }

		 /* RF6_DARKNESS */
		 case 129+12:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s gestures in shadow.", m_name);
			 (void)unlite_area(0, 3);
			 break;
		 }

		 /* RF6_TRAPS */
	 case 129+13:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles, and then cackles evilly.", m_name);
			 else msg_format("%^s casts a spell and cackles evilly.", m_name);
			 (void)trap_creation();
			 break;
		 }

		 /* RF6_FORGET */
	 case 129+14:
		 {
			 if (!direct) break;
			 disturb(1, 0);
			 msg_format("%^s tries to blank your mind.", m_name);

			 if (rand_int(100) < p_ptr->skill_sav)
			 {
				 msg_print("You resist the effects!");
			 }
			 else if ((p_ptr->mind & MIND_MENTAL_BARRIER) && rand_int(3))
			      msg_print("Your mental barrier deflects the attack.");
			 else if (lose_all_info())
			 {
				 msg_print("Your memories fade away.");
			 }
			 break;
		 }

		 /* RF6_XXX6X6 */
	 case 129+15:
		 {
			 break;
		 }

		 /* RF6_SUMMON_KIN */
	 case 129+16:
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
				 count += summon_specific(y, x, rlev, SUMMON_KIN, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear many things appear nearby.");

			 break;
		 }

		 /* RF6_S_CYBER */
	 case 129+17:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons Cyberdemons!", m_name);
			 if (blind && count) msg_print("You hear heavy steps nearby.");
			 summon_cyber();
			 break;
		 }

		 /* RF6_S_MONSTER */
	 case 129+18:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons help!", m_name);
			 for (k = 0; k < 1; k++)
			 {
				 count += summon_specific(y, x, rlev, 0, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear something appear nearby.");
			 break;
		 }

		 /* RF6_S_MONSTERS */
	 case 129+19:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons monsters!", m_name);
			 for (k = 0; k < 8; k++)
			 {
				 count += summon_specific(y, x, rlev, 0, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear many things appear nearby.");
			 break;
		 }

		 /* RF6_S_ANT */
	 case 129+20:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons ants.", m_name);
			 for (k = 0; k < 6; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_ANT, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear many things appear nearby.");
			 break;
		 }

		 /* RF6_S_SPIDER */
	 case 129+21:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons spiders.", m_name);
			 for (k = 0; k < 6; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_SPIDER, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear many things appear nearby.");
			 break;
		 }

		 /* RF6_S_HOUND */
	 case 129+22:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons hounds.", m_name);
			 for (k = 0; k < 6; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_HOUND, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear many things appear nearby.");
			 break;
		 }

		 /* RF6_S_HYDRA */
	 case 129+23:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons hydras.", m_name);
			 for (k = 0; k < 6; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_HYDRA, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear many things appear nearby.");
			 break;
		 }

		 /* RF6_S_ANGEL */
	 case 129+24:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons an angel!", m_name);
			 for (k = 0; k < 1; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_ANGEL, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear something appear nearby.");
			 break;
		 }

		 /* RF6_S_DEMON */
	 case 129+25:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons a demon from the Courts of Chaos!", m_name);
			 for (k = 0; k < 1; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_DEMON, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear something appear nearby.");
			 break;
		 }

		 /* RF6_S_UNDEAD */
	 case 129+26:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons an undead adversary!", m_name);
			 for (k = 0; k < 1; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_UNDEAD, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear something appear nearby.");
			 break;
		 }

		 /* RF6_S_DRAGON */
	 case 129+27:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons a dragon!", m_name);
			 for (k = 0; k < 1; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_DRAGON, TRUE, FALSE, FALSE);
			 }
			 if (blind && count) msg_print("You hear something appear nearby.");
			 break;
		 }

		 /* RF6_S_HI_UNDEAD */
	 case 129+28:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons greater undead!", m_name);
			 for (k = 0; k < 8; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD, TRUE, FALSE, FALSE);
			 }
			 if (blind && count)
			 {
				 msg_print("You hear many creepy things appear nearby.");
			 }
			 break;
		 }

		 /* RF6_S_HI_DRAGON */
	 case 129+29:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons ancient dragons!", m_name);
			 for (k = 0; k < 8; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_HI_DRAGON, TRUE, FALSE, FALSE);
			 }
			 if (blind && count)
			 {
				 msg_print("You hear many powerful things appear nearby.");
			 }
			 break;
		 }

		 /* RF6_S_WRAITH */
	 case 129+30:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons wraiths", m_name);


			 for (k = 0; k < 8; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_WRAITH, TRUE, FALSE, FALSE);
			 }
			 if (blind && count)
			 {
				 msg_print("You hear powerful beings appear nearby.");
			 }
			 break;
		 }

		 /* RF6_S_UNIQUE */
	 case 129+31:
		 {
			 disturb(1, 0);
			 if (blind) msg_format("%^s mumbles.", m_name);
			 else msg_format("%^s magically summons special opponents!", m_name);
			 for (k = 0; k < 8; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_UNIQUE, TRUE, FALSE, FALSE);
			 }
			 for (k = 0; k < 8; k++)
			 {
				 count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD, TRUE, FALSE, FALSE);
			 }
			 if (blind && count)
			 {
				 msg_print("You hear many powerful things appear nearby.");
			 }
			 break;
		 }

		/* RF6_S_DOOM */
		case 129+32:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a minion of Id!", m_name);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev + 5, SUMMON_DOOM, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}
		/* RF6_S_HI_DOOM */
		case 129+33:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons minions of Id!", m_name);

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, rlev + 5, SUMMON_HI_DOOM, TRUE, FALSE, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
	}


	/* Remember what the monster did to us */
	if (seen)
	{
		/* Inate spell */
		if (thrown_spell < 65)
		{
			r_ptr->r_flags4 |= (1L << (thrown_spell - 1));
			if (r_ptr->r_cast_inate < MAX_UCHAR) r_ptr->r_cast_inate++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 129)
		{
			r_ptr->r_flags5 |= (1L << (thrown_spell - 65));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 193)
		{
			r_ptr->r_flags6 |= (1L << (thrown_spell - 129));
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




#ifdef MONSTER_FLOW

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
				if (!in_bounds(y, x)) continue;

				/* Skip locations in a wall */
				if (!cave_floor_bold(y, x)) continue;

				/* Check distance */
				if (distance(y, x, fy, fx) != d) continue;

				/* Check for hidden, available grid */
				if (!player_can_see_bold(y, x) && clean_shot(fy, fx, y, x, FALSE))
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
	int i, y, x, y1, x1, when = 0, cost = 999;

	cave_type *c_ptr;

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

	/* Monster grid */
	c_ptr = &cave[y1][x1];

	/* The player is not currently near the monster grid */
	if (c_ptr->when < cave[py][px].when)
	{
		/* The player has never been near the monster grid */
		if (!c_ptr->when) return (FALSE);

		/* The monster is not allowed to track the player */
		if (!flow_by_smell) return (FALSE);
	}

	/* Monster is too far away to notice the player */
	if (c_ptr->cost > MONSTER_FLOW_DEPTH) return (FALSE);
	if (c_ptr->cost > r_ptr->aaf) return (FALSE);

	/* Hack -- Player can see us, run towards him */
	if (player_has_los_bold(y1, x1)) return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		/* Get the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Ignore illegal locations */
		if (!cave[y][x].when) continue;

		/* Ignore ancient locations */
		if (cave[y][x].when < when) continue;

		/* Ignore distant locations */
		if (cave[y][x].cost > cost) continue;

		/* Save the cost and time */
		when = cave[y][x].when;
		cost = cave[y][x].cost;

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
	int y, x, y1, x1, fy, fx, gy = 0, gx = 0;
	int when = 0, score = -1;
	int i;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster flowing disabled */
	if (!flow_by_sound) return (FALSE);

	/* Monster location */
	fy = m_ptr->fy;
	fx = m_ptr->fx;

	/* Desired destination */
	y1 = fy - (*yp);
	x1 = fx - (*xp);

	/* The player is not currently near the monster grid */
	if (cave[fy][fx].when < cave[py][px].when)
	{
		/* No reason to attempt flowing */
		return (FALSE);
	}

	/* Monster is too far away to use flow information */
	if (cave[fy][fx].cost > MONSTER_FLOW_DEPTH) return (FALSE);
	if (cave[fy][fx].cost > r_ptr->aaf) return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		int dis, s;

		/* Get the location */
		y = fy + ddy_ddd[i];
		x = fx + ddx_ddd[i];

		/* Ignore illegal locations */
		if (cave[y][x].when == 0) continue;

		/* Ignore ancient locations */
		if (cave[y][x].when < when) continue;

		/* Calculate distance of this grid from our destination */
		dis = distance(y, x, y1, x1);

		/* Score this grid */
		s = 5000 / (dis + 3) - 500 / (cave[y][x].cost + 1);

		/* No negative scores */
		if (s < 0) s = 0;

		/* Ignore lower scores */
		if (s < score) continue;

		/* Save the score and time */
		when = cave[y][x].when;
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
				if (!in_bounds(y, x)) continue;

				/* Skip locations in a wall */
				if (!cave_floor_bold(y, x)) continue;

				/* Check distance */
				if (distance(y, x, fy, fx) != d) continue;

				/* Check for "availability" (if monsters can flow) */
				if (flow_by_sound)
				{
					/* Ignore grids very far from the player */
					if (cave[y][x].when < cave[py][px].when) continue;

					/* Ignore too-distant grids */
					if (cave[y][x].cost > cave[fy][fx].cost + 2 * d) continue;
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
 * Choose "logical" directions for monster movement
 */
static bool get_moves(int m_idx, int *mm)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int          y, ay, x, ax;
	int          move_val = 0;
	int          y2 = py;
	int          x2 = px;
	bool         done = FALSE;
	bool         will_run = mon_will_run(m_idx);


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

	if (!stupid_monsters && !will_run && is_hostile(m_ptr))
	{
	/*
	 * Animal packs try to get the player out of corridors
	 * (...unless they can move through walls -- TY)
	 */
		if ((r_ptr->flags1 & RF1_FRIENDS) &&
			(r_ptr->flags3 & RF3_ANIMAL) &&
			 !(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)))
		{
			int i, room = 0;

			/* Count room grids next to player */
			for (i = 0; i < 8; i++)
			{
				int x = px + ddx_ddd[i];
				int y = py + ddy_ddd[i];

				/* Check grid */
				if (monster_can_cross_terrain(cave[y][x].feat, r_ptr))
				{
					/* One more room grid */
					room++;
				}
			}

			/* Not in a room and strong player */
			if ((room <= (8 * (p_ptr->chp + p_ptr->csp)) /
			    (p_ptr->mhp + p_ptr->msp)) && (room != 7))
			{
				/* Find hiding place */
				if (find_hiding(m_idx, &y, &x)) done = TRUE;
			}
		}

		/* Monster groups try to surround the player */
		if (!done && (r_ptr->flags1 & RF1_FRIENDS))
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
				if (!cave_empty_bold(y2, x2)) continue;

				/* Try to fill this hole */
				break;
			}

			/* Extract the new "pseudo-direction" */
			y = m_ptr->fy - y2;
			x = m_ptr->fx - x2;

			/* Done */
			done = TRUE;
		}
	}

	/* Apply fear if possible and necessary */
	if ((stupid_monsters || is_pet(m_ptr)) && will_run)
	{
		/* XXX XXX Not very "smart" */
		y = (-y), x = (-x);
	}
	else
	{
		if (!done && will_run)
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
	}


	if (!stupid_monsters)
	{
		/* Check for no move */
		if (!x && !y) return (FALSE);
	}


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

	/* Extract some directions */
	switch (move_val)
	{
	case 0:
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
	case 1:
	case 9:
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
	case 2:
	case 6:
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
	case 4:
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
	case 5:
	case 13:
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
	case 8:
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
	case 10:
	case 14:
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
	case 12:
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

	/* Wants to move... */
	return (TRUE);
}


int check_hit2(int power, int level, int ac)
{
	int i, k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality" */
	i = (power + (level * 3));

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint(i) > ((ac * 3) / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/* Monster attacks monster */
static bool monst_attack_monst(int m_idx,int t_idx)
{
	monster_type    *m_ptr = &m_list[m_idx];
	monster_type    *t_ptr = &m_list[t_idx];

	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	monster_race    *tr_ptr = &r_info[t_ptr->r_idx];
	int             ap_cnt;
	int             ac, rlev,pt;
	char            m_name[80],t_name[80];
	char            ddesc[80],temp[80];
	bool            blinked = FALSE, touched = FALSE;
	bool                    explode = FALSE;
	bool                    fear = FALSE;
	byte            y_saver = t_ptr->fy;
	byte            x_saver = t_ptr->fx;

	/* Cannot attack self */
	if (m_idx == t_idx) return FALSE;

	/* Not allowed to attack */
	if (r_ptr->flags1 & RF1_NEVER_BLOW) return FALSE;

	/* Wake it up */
	t_ptr->csleep = 0;

	/* Total armor */
	ac = tr_ptr->ac;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the monster name (or "it") */
	monster_desc(t_name, t_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, m_ptr, 0x88);

	/* Assume no blink */
	blinked = FALSE;

	if (!(m_ptr->ml || t_ptr->ml))
	{
		msg_print("You hear noise.");
	}

	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;

		int power = 0;
		int damage = 0;

		cptr act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;

		if (t_ptr == m_ptr) /* Paranoia */
		{
			if (wizard)
				msg_print("Monster attacking self?");
			break;
		}

		/* Stop attacking if the target dies! */
		if (t_ptr->fx != x_saver || t_ptr->fy != y_saver)
			break;

		/* Hack -- no more attacks */
		if (!method) break;

		if (blinked) /* Stop! */
		{
			/* break; */
		}

		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* Extract the attack "power" */
		switch (effect)
		{
		case RBE_HURT:          power = 60; break;
		case RBE_POISON:        power =  5; break;
		case RBE_UN_BONUS:      power = 20; break;
		case RBE_UN_POWER:      power = 15; break;
		case RBE_EAT_GOLD:      power =  5; break;
		case RBE_EAT_ITEM:      power =  5; break;
		case RBE_EAT_FOOD:      power =  5; break;
		case RBE_EAT_LITE:      power =  5; break;
		case RBE_ACID:          power =  0; break;
		case RBE_ELEC:          power = 10; break;
		case RBE_FIRE:          power = 10; break;
		case RBE_COLD:          power = 10; break;
		case RBE_BLIND:         power =  2; break;
		case RBE_CONFUSE:       power = 10; break;
		case RBE_TERRIFY:       power = 10; break;
		case RBE_PARALYZE:      power =  2; break;
		case RBE_LOSE_STR:      power =  0; break;
		case RBE_LOSE_DEX:      power =  0; break;
		case RBE_LOSE_CON:      power =  0; break;
		case RBE_LOSE_INT:      power =  0; break;
		case RBE_LOSE_WIS:      power =  0; break;
		case RBE_LOSE_CHR:      power =  0; break;
		case RBE_LOSE_ALL:      power =  2; break;
		case RBE_SHATTER:       power = 60; break;
		case RBE_EXP_10:        power =  5; break;
		case RBE_EXP_20:        power =  5; break;
		case RBE_EXP_40:        power =  5; break;
		case RBE_EXP_80:        power =  5; break;
		case RBE_DISEASE:       power =  5; break;
		case RBE_TIME:          power =  5; break;
		case RBE_DEATH:         power =  5; break;
		case RBE_STONE:         power =  0; break;
		}


		/* Monster hits*/
		if (!effect || check_hit2(power, rlev, ac))
		{
			/* Always disturbing */
			disturb(1, 0);

			/* Describe the attack method */
			switch (method)
			{
			case RBM_HIT:
				{
					act = "hits %s.";
					touched = TRUE;
					break;
				}

			case RBM_TOUCH:
				{
					act = "touches %s.";
					touched = TRUE;
					break;
				}

			case RBM_PUNCH:
				{
					act = "punches %s.";
					touched = TRUE;
					break;
				}

			case RBM_KICK:
				{
					act = "kicks %s.";
					touched = TRUE;
					break;
				}

			case RBM_CLAW:
				{
					act = "claws %s.";
					touched = TRUE;
					break;
				}

			case RBM_BITE:
				{
					act = "bites %s.";
					touched = TRUE;
					break;
				}

			case RBM_STING:
				{
					act = "stings %s.";
					touched = TRUE;
					break;
				}

			case RBM_BEAK:
				{
					act = "beaks %s.";
					break;
				}

			case RBM_BUTT:
				{
					act = "butts %s.";
					touched = TRUE;
					break;
				}

			case RBM_CRUSH:
				{
					act = "crushes %s.";
					touched = TRUE;
					break;
				}

			case RBM_ENGULF:
				{
					act = "engulfs %s.";
					touched = TRUE;
					break;
				}

			case RBM_CHARGE:
				{
					act = "charges %s.";
					touched = TRUE;
					break;
				}

			case RBM_CRAWL:
				{
					act = "crawls on %s.";
					touched = TRUE;
					break;
				}

			case RBM_DROOL:
				{
					act = "drools on %s.";
					touched = FALSE;
					break;
				}

			case RBM_SPIT:
				{
					act = "spits on %s.";
					touched = FALSE;
					break;
				}

			case RBM_EXPLODE:
				{
					act = "explodes.";
					explode = TRUE;
					touched = FALSE;
					break;
				}

			case RBM_GAZE:
				{
					act = "gazes at %s.";
					touched = FALSE;
					break;
				}

			case RBM_WAIL:
				{
					act = "wails at %s.";
					touched = FALSE;
					break;
				}

			case RBM_SPORE:
				{
					act = "releases spores at %s.";
					touched = FALSE;
					break;
				}

			case RBM_XXX4:
				{
					act = "projects XXX4's at %s.";
					touched = FALSE;
					break;
				}

			case RBM_BEG:
				{
					act = "begs %s for money.";
					touched = FALSE;
					t_ptr->csleep = 0;
					break;
				}

			case RBM_INSULT:
				{
					act = "insults %s.";
					touched = FALSE;
					t_ptr->csleep = 0;
					break;
				}

			case RBM_MOAN:
				{
					act = "moans at %s.";
					touched = FALSE;
					t_ptr->csleep = 0;
					break;
				}

			case RBM_SHOW:
				{
					act = "sings to %s.";
					touched = FALSE;
					t_ptr->csleep = 0;
					break;
				}
			}

			/* Message */
			if (act)
			{
				(void)strfmt(temp,act,t_name);
				if (m_ptr->ml || t_ptr->ml)
					msg_format("%^s %s", m_name, temp);

			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);

			pt = GF_MISSILE;

			/* Apply appropriate damage */
			switch (effect)
			{
			case 0:
				{
					damage = 0;
					pt  = 0;
					break;
				}

			case RBE_HURT:
				{
					damage -= (damage * ((ac < 150) ? ac : 150) / 250);
					break;
				}

			case RBE_POISON:
			case RBE_DISEASE:
				{
					pt = GF_POIS;
					break;
				}

			case RBE_UN_BONUS:
			case RBE_UN_POWER:
				{
					pt = GF_DISENCHANT;
					break;
				}

			case RBE_EAT_FOOD:
			case RBE_EAT_LITE:
				{
					pt = damage = 0;
					break;
				}

			case RBE_EAT_ITEM:
			case RBE_EAT_GOLD:
				{
					pt = damage = 0;
					if (randint(2)==1) blinked = TRUE;
					break;
				}

			case RBE_ACID:
				{
					pt = GF_ACID;
					break;
				}

			case RBE_ELEC:
				{
					pt = GF_ELEC;
					break;
				}

			case RBE_FIRE:
				{
					pt = GF_FIRE;
					break;
				}

			case RBE_COLD:
				{
					pt = GF_COLD;
					break;
				}

			case RBE_BLIND:
				{
					break;
				}

			case RBE_CONFUSE:
				{
					pt = GF_CONFUSION;
					break;
				}

			case RBE_TERRIFY:
				{
					pt = GF_TURN_ALL;
					break;
				}

			case RBE_PARALYZE:
				{
					pt = GF_OLD_SLEEP; /* sort of close... */
					break;
				}

			case RBE_LOSE_STR:
			case RBE_LOSE_INT:
			case RBE_LOSE_WIS:
			case RBE_LOSE_DEX:
			case RBE_LOSE_CON:
			case RBE_LOSE_CHR:
			case RBE_LOSE_ALL:
				{
					break;
				}
			case RBE_SHATTER:
				{
					if (damage > 23)
					{
						/* Prevent destruction of quest levels and town */
						if (!is_quest(dun_level) && dun_level)
							earthquake(m_ptr->fy, m_ptr->fx, 8);
					}
					break;
				}
			case RBE_EXP_10:
			case RBE_EXP_20:
			case RBE_EXP_40:
			case RBE_EXP_80:
				{
					pt = GF_NETHER;
					break;
				}
			case RBE_TIME:
				{
					pt = GF_TIME;
					break;
				}
			case RBE_DEATH:
				{
					pt = GF_DEATH_RAY;
					break;
				}
			default:
				{
					pt = 0;
					break;
				}
			}

			if (pt)
			{
				/* Do damage if not exploding */
				if (!explode)
				{
					project(m_idx, 0, t_ptr->fy, t_ptr->fx,
						(pt == GF_OLD_SLEEP ? r_ptr->level : damage), pt, PROJECT_KILL | PROJECT_STOP);
				}

				if (touched)
				{
					/* Aura fire */
					if ((tr_ptr->flags2 & RF2_AURA_FIRE) &&
						!(r_ptr->flags3 & RF3_IM_FIRE))
					{
						if (m_ptr->ml || t_ptr->ml)
						{
							blinked = FALSE;
							msg_format("%^s is suddenly very hot!", m_name);
							if(t_ptr->ml)
								tr_ptr->r_flags2 |= RF2_AURA_FIRE;
						}
						project(t_idx, 0, m_ptr->fy, m_ptr->fx,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_FIRE, PROJECT_KILL | PROJECT_STOP);
					}

					/* Aura cold */
					if ((tr_ptr->flags3 & RF3_AURA_COLD) &&
						!(r_ptr->flags3 & RF3_IM_COLD))
					{
						if (m_ptr->ml || t_ptr->ml)
						{
							blinked = FALSE;
							msg_format("%^s is suddenly very cold!", m_name);
							if(t_ptr->ml)
								tr_ptr->r_flags3 |= RF3_AURA_COLD;
						}
						project(t_idx, 0, m_ptr->fy, m_ptr->fx,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_COLD, PROJECT_KILL | PROJECT_STOP);
					}

					/* Aura elec */
					if ((tr_ptr->flags2 & (RF2_AURA_ELEC)) && !(r_ptr->flags3 & (RF3_IM_ELEC)))
					{
						if (m_ptr->ml || t_ptr->ml)
						{
							blinked = FALSE;
							msg_format("%^s gets zapped!", m_name);
							if(t_ptr->ml)
								tr_ptr->r_flags2 |= RF2_AURA_ELEC;
						}
						project(t_idx, 0, m_ptr->fy, m_ptr->fx,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_ELEC, PROJECT_KILL | PROJECT_STOP);
					}

					/* Aura fear */
					if ((tr_ptr->flags2 & (RF2_AURA_FEAR)) && !(r_ptr->flags3 & (RF3_NO_FEAR)))
					{
						if (m_ptr->ml || t_ptr->ml)
						{
							blinked = FALSE;
							msg_format("%^s becomes afraid!", m_name);
							if(t_ptr->ml)
								tr_ptr->r_flags2 |= RF2_AURA_FEAR;
						}
						t_ptr->monfear += 
						damroll (1 + (tr_ptr->level) / 26,
							1 + (tr_ptr->level) / 17);
					}

				}
			}
		}

		/* Monster missed player */
		else
		{
			/* Analyze failed attacks */
			switch (method)
			{
			case RBM_HIT:
			case RBM_TOUCH:
			case RBM_PUNCH:
			case RBM_KICK:
			case RBM_CLAW:
			case RBM_BITE:
			case RBM_STING:
			case RBM_BEAK:
			case RBM_BUTT:
			case RBM_CRUSH:
			case RBM_ENGULF:
			case RBM_CHARGE:
				{
					/* Visible monsters */
					if (m_ptr->ml)
					{
						/* Disturbing */
						disturb(1, 0);

						/* Message */
						msg_format("%^s misses %s.", m_name,t_name);
					}

					break;
				}
			}
		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (r_ptr->r_blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (r_ptr->r_blows[ap_cnt] < MAX_UCHAR)
				{
					r_ptr->r_blows[ap_cnt]++;
				}
			}
		}
	}

	if (explode)
	{
		sound(SOUND_EXPLODE);
		mon_take_hit_mon(m_idx, m_ptr->hp + 1, &fear, " explodes into tiny shreds.");

		blinked = FALSE;
	}


	/* Blink away */
	if (blinked)
	{
		if (m_ptr->ml)
		{
			msg_print("The thief flees laughing!");
		}
		else
		{
			msg_print("You hear laughter!");
		}

		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}

	return TRUE;
}


/*
 * Hack -- local "player stealth" value (see below)
 */
static u32b noise = 0L;

/* Determine whether the player is invisible to a monster */
static bool player_invis(monster_type * m_ptr)
{
	s16b inv, mlv;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	inv = p_invis;

	mlv = (s16b) r_ptr->level;

	if (r_ptr->flags3 & RF3_NO_SLEEP)
		mlv += 10;
	if (r_ptr->flags3 & RF3_DRAGON)
		mlv += 20;
	if (r_ptr->flags3 & RF3_UNDEAD)
		mlv += 15;
	if (r_ptr->flags3 & RF3_DEMON)
		mlv += 15;
	if (r_ptr->flags3 & RF3_ANIMAL)
		mlv += 15;
	if (r_ptr->flags3 & RF3_ORC)
		mlv -= 15;
	if (r_ptr->flags3 & RF3_TROLL)
		mlv -= 10;
	if (r_ptr->flags2 & RF2_STUPID)
		mlv /= 2;
	if (r_ptr->flags2 & RF2_SMART)
		mlv = (mlv * 5) / 4;
	if (r_ptr->flags1 & RF1_QUESTOR)
		inv = 0;
	if (r_ptr->flags2 & RF2_INVISIBLE)
		inv = 0;
	if (mlv < 1)
		mlv = 1;
	return (inv >= randint(mlv*2));
}



/*
 * Process a monster
 *
 * The monster is known to be within 100 grids of the player
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
 * XXX XXX XXX In addition, monsters which *cannot* open or bash
 * down a door will still stand there trying to open it...
 *
 * XXX Technically, need to check for monster in the way
 * combined with that monster being in a wall (or door?)
 *
 * A "direction" of "5" means "pick a random direction".
 */
static void process_monster(int m_idx)
{
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	int             i, d, oy, ox, ny, nx;

	int             mm[8];

	cave_type       *c_ptr;

	monster_type    *y_ptr;

	bool            do_turn;
	bool            do_move;
	bool            do_view;

	bool            did_open_door;
	bool            did_bash_door;
	bool            did_take_item;
	bool            did_kill_item;
	bool            did_move_body;
	bool            did_pass_wall;
	bool            did_kill_wall;
	bool            gets_angry = FALSE;

	bool inv = player_invis(m_ptr);

	/* Handle "sleep" */
	if (m_ptr->csleep)
	{
		u32b notice = 0;

		/* Hack -- handle non-aggravation */
		if (!p_aggravate) notice = rand_int(1024);

		/* Hack -- See if monster "notices" player */
		if ((notice * notice * notice) <= noise)
		{
			/* Hack -- amount of "waking" */
			int d = 1;

			/* Wake up faster near the player */
			if (m_ptr->cdis < 50) d = (100 / m_ptr->cdis);

			/* Hack -- handle aggravation */
			if (p_aggravate) d = m_ptr->csleep;

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

					/* Hack -- Update the health bar */
					if (health_who == m_idx)
					  p_ptr->redraw |= (PR_HEALTH);

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

	/* Handle "fast" */
	if (m_ptr->mspeed > r_ptr->speed + 4)
	{
		if (rand_int(67) == 1)
		{
			m_ptr->mspeed = r_ptr->speed;

			/* Message if visible */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s slows down.", m_name);
			}
		}
	}

	/* Handle "slow" */
	if (m_ptr->mspeed < r_ptr->speed - 4)
	{
		if (rand_int(67) == 1)
		{
			m_ptr->mspeed = r_ptr->speed;

			/* Message if visible */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s speeds up.", m_name);
			}
		}
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
				msg_format("%^s is no longer stunned.", m_name);

				/* Hack -- Update the health bar */
				if (health_who == m_idx)
				  p_ptr->redraw |= (PR_HEALTH);
			}
		}

		/* Still stunned */
		if (m_ptr->stunned) return;
	}


	/* Handle confusion */
	if (m_ptr->confused)
	{
		/* Amount of "boldness" */
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
				if (health_who == m_idx)
				  p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}

	/* No one wants to be your friend if you're aggravating */
	if ((!is_hostile(m_ptr)) && (p_aggravate) && !(r_ptr->flags7 & RF7_PET))
		gets_angry = TRUE;

	/* Paranoia... no pet uniques outside wizard mode -- TY */
	if ((is_pet(m_ptr)) && !(wizard) &&
		(r_ptr->flags1 & (RF1_UNIQUE)) && !(r_ptr->flags7 & RF7_PET))
		gets_angry = TRUE;

	if (gets_angry)
	{
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);
		msg_format("%^s suddenly becomes hostile!", m_name);
		set_hostile(m_ptr);
	}

	/* Take drowning damage (experimental) */

	if ((cave[m_ptr->fy][m_ptr->fx].feat == FEAT_DEEP_WATER) &&
	   (!(monster_can_cross_terrain(FEAT_DEEP_WATER, r_ptr))))
	   {
	   char m_name[80];
	   bool fear;
	   monster_desc(m_name, m_ptr, 0);
	   if (m_ptr->ml) msg_format("%^s is drowning.", m_name);
	   mon_take_hit(m_idx, 1, &fear, " goes down.");
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

				/* Hack -- Update the health bar */
				if (health_who == m_idx)
				  p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}

	/* Get the origin */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Attempt to "multiply" if able and allowed */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) && (num_repro < MAX_REPRO))
	{
		int k, y, x;

		/* Count the adjacent monsters */
		for (k = 0, y = oy - 1; y <= oy + 1; y++)
		{
			for (x = ox - 1; x <= ox + 1; x++)
			{
				if (cave[y][x].m_idx) k++;
			}
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(m_idx, FALSE, is_friendly(m_ptr), is_pet(m_ptr)))
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

	/* Hack! "Cyber" monster makes noise... */
	if (strstr((r_name + r_ptr->name),"Cyberdemon"))
	{
		if (randint(CYBERNOISE) == 1)
		{
			if (!m_ptr->ml)
			{
				disturb(FALSE, FALSE);
				msg_print("You hear heavy steps.");
			}
		}
	}


	if (speak_unique)
	{
		if (randint(SPEAK_CHANCE) == 1)
		{
			if (player_has_los_bold(oy, ox) && (r_ptr->flags2 & RF2_CAN_SPEAK))
			{
				char m_name[80];
				char monmessage[80];
				char comeback[80];
				char verb[80];

				/* Acquire the monster name/poss */
				if (m_ptr->ml)
					monster_desc(m_name, m_ptr, 0);
				else
					strcpy(m_name, "It");

		/* xtra_line function by Matt Graham--allow uniques to */
		/* say "unique" things based on their monster index.   */
		/* Try for the unique's lines in "monspeak.txt" first. */
		/* 0 is SUCCESS, of course....                         */

		if (get_xtra_line ("monspeak.txt",m_ptr,monmessage) != 0)
			{
			/* Get a message from old defaults if new don't work */

			if (m_ptr->monfear)
				get_rnd_line("monfear.txt", monmessage);
			else if (!is_hostile)
				get_rnd_line("friendly.txt", monmessage);
			else
				get_rnd_line("bravado.txt", monmessage);
			}
			msg_format("%^s %s", m_name, monmessage);
			if (!p_ptr->afraid)
				  {
				    get_rnd_line("comeback.txt", comeback);
				    get_rnd_line("verbs.txt", verb);
				    msg_format("'%^s', you %s!", comeback, verb);
				  }

			}
		}
	}


	/* Attempt to cast a spell */
	if (make_attack_spell(m_idx)) return;

	/*
	 * Attempt to cast a spell at an enemy other than the player
	 * (may slow the game a smidgeon, but I haven't noticed.)
	 */
	if (monst_spell_monst(m_idx)) return;


	/* Hack -- Assume no movement */
	mm[0] = mm[1] = mm[2] = mm[3] = 0;
	mm[4] = mm[5] = mm[6] = mm[7] = 0;


	/* Confused, doesn't see us -- 100% random */
	if (m_ptr->confused || inv)
	{
		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

	/* 75% random movement */
	else if ((r_ptr->flags1 & (RF1_RAND_50)) &&
		(r_ptr->flags1 & (RF1_RAND_25)) &&
		(rand_int(100) < 75))
	{
		/* Memorize flags */
		if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_50);
		if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_25);

		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

	/* 50% random movement */
	else if ((r_ptr->flags1 & (RF1_RAND_50)) &&
		(rand_int(100) < 50))
	{
		/* Memorize flags */
		if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_50);

		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

	/* 25% random movement */
	else if ((r_ptr->flags1 & (RF1_RAND_25)) &&
		(rand_int(100) < 25))
	{
		/* Memorize flags */
		if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_25);

		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}
	/* Can't reach player - find something else to hit */
	else if ((r_ptr->flags1 & RF1_NEVER_MOVE) && (m_ptr->cdis > 1))
	{
		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;

		/* Look for an enemy */
		get_enemy_dir(m_ptr, mm);
	}
	/* Pets will follow the player */
	else if (is_pet(m_ptr))
	{
		/* Are we trying to avoid the player? */
		bool avoid = ((p_ptr->pet_follow_distance < 0) &&
						  (m_ptr->cdis <= (0 - p_ptr->pet_follow_distance)));

		/* Do we want to find the player? */
		bool lonely = (!avoid && (m_ptr->cdis > p_ptr->pet_follow_distance));

		/* Should we find the player if we can't find a monster? */
		bool distant = (m_ptr->cdis > PET_SEEK_DIST);

		/* by default, move randomly */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;

		/* Look for an enemy */
		if (!get_enemy_dir(m_ptr, mm))
		{
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
				get_moves(m_idx, mm);

				/* Restore the leash */
				p_ptr->pet_follow_distance = dis;
			}
		}
	}
	/* Friendly monster movement */
	else if (!is_hostile(m_ptr))
	{
		/* by default, move randomly */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;

		/* Look for an enemy */
		get_enemy_dir(m_ptr, mm);
	}
	/* Normal movement */
	else
	{
		if (stupid_monsters)
		{
			/* Logical moves */
			get_moves(m_idx, mm);
		}
		else
		{
			/* Logical moves, may do nothing */
			if (!get_moves(m_idx, mm)) return;
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
	did_pass_wall = FALSE;
	did_kill_wall = FALSE;


	/* Take a zero-terminated array of "directions" */
	for (i = 0; mm[i]; i++)
	{
		/* Get the direction */
		d = mm[i];

		/* Hack -- allow "randomized" motion */
		if (d == 5) d = ddd[rand_int(8)];

		/* Get the destination */
		ny = oy + ddy[d];
		nx = ox + ddx[d];

		/* Access that cave grid */
		c_ptr = &cave[ny][nx];

		/* Access that cave grid's contents */
		y_ptr = &m_list[c_ptr->m_idx];


		/* Floor is open? */
		if (cave_floor_bold(ny, nx))
		{
			/* Go ahead and move */
			do_move = TRUE;
		}

		else if (cave[ny][nx].feat == FEAT_CHAOS_FOG) {
		  if (randint(5) == 1)
		  {
		    do_move = FALSE;
		    if (player_has_los_bold(ny, nx))
		      {
		      msg_format("The chaos fog blocks something.");
		      }
		  }
		  else
		      {
		      do_move = TRUE;
		      }
		}

		/* Hack -- trees are no obstacle */
		else if (cave[ny][nx].feat == FEAT_TREES)
		{
			do_move = TRUE;
		}

		/* Hack -- player 'in' wall */
		else if ((ny == py) && (nx == px))
		{
			do_move = TRUE;
		}

		else if (c_ptr->m_idx)
		{
			/* Possibly a monster to attack */
			do_move = TRUE;
		}

		/* Permanent wall */
		else if (f_info[c_ptr->feat].flags1 & FF1_PERMANENT)
		{
			/* Nothing */
		}

		/* Some monsters can fly */
		else if ((f_info[c_ptr->feat].flags1 & FF1_CAN_LEVITATE) && (r_ptr->flags7 & (RF7_CAN_FLY)))
		{
			/* Pass through walls/doors/rubble */
			do_move = TRUE;
		}

		/* Monster moves through walls (and doors) */
		else if ((f_info[c_ptr->feat].flags1 & FF1_CAN_PASS) && (r_ptr->flags2 & (RF2_PASS_WALL)))
		{
			/* Pass through walls/doors/rubble */
			do_move = TRUE;

			/* Monster went through a wall */
			did_pass_wall = TRUE;
		}

		/* Monster destroys walls (and doors) */
		else if ((f_info[c_ptr->feat].flags1 & FF1_CAN_PASS) && (r_ptr->flags2 & (RF2_KILL_WALL)))
		{
			/* Eat through walls/doors/rubble */
			do_move = TRUE;

			/* Monster destroyed a wall */
			did_kill_wall = TRUE;

			if (randint(GRINDNOISE) == 1)
			{
				msg_print("There is a grinding sound.");
			}

			/* Forget the wall */
			c_ptr->info &= ~(CAVE_MARK);

			/* Notice */
			cave_set_feat(ny, nx, FEAT_FLOOR);

			/* Note changes to viewable region */
			if (player_has_los_bold(ny, nx)) do_view = TRUE;
		}

		/* Handle doors and secret doors */
		else if (((c_ptr->feat >= FEAT_DOOR_HEAD) &&
			  (c_ptr->feat <= FEAT_DOOR_TAIL)) ||
			  (c_ptr->feat == FEAT_SECRET))
		{
			bool may_bash = TRUE;

			/* Take a turn */
			do_turn = TRUE;

			/* Creature can open doors. */
			if ((r_ptr->flags2 & RF2_OPEN_DOOR) &&
			    (!is_pet(m_ptr) || p_ptr->pet_open_doors))
			{
					/* Closed doors and secret doors */
					if ((c_ptr->feat == FEAT_DOOR_HEAD) ||
						(c_ptr->feat == FEAT_SECRET))
					{
						/* The door is open */
						did_open_door = TRUE;

						/* Do not bash the door */
						may_bash = FALSE;
					}

					/* Locked doors (not jammed) */
					else if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08)
					{
						int k;

						/* Door power */
						k = ((c_ptr->feat - FEAT_DOOR_HEAD) & 0x07);

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
			if (may_bash && (r_ptr->flags2 & RF2_BASH_DOOR) &&
				(!is_pet(m_ptr) || p_ptr->pet_open_doors))
			{
					int k;

					/* Door power */
					k = ((c_ptr->feat - FEAT_DOOR_HEAD) & 0x07);

#if 0
					/* XXX XXX XXX Old test (pval 10 to 20) */
					if (randint((m_ptr->hp + 1) * (50 + o_ptr->pval)) <
						40 * (m_ptr->hp - 10 - o_ptr->pval));
#endif

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
		if (do_move && (c_ptr->feat == FEAT_GLYPH) &&
		    !(r_ptr->flags1 & RF1_NEVER_BLOW))
		{
			/* Assume no move allowed */
			do_move = FALSE;

			/* Break the ward */
			if (randint(BREAK_GLYPH) < r_ptr->level)
			{
				/* Describe observable breakage */
				if (c_ptr->info & CAVE_MARK)
				{
					msg_print("The rune of protection is broken!");
				}

				/* Forget the rune */
				c_ptr->info &= ~(CAVE_MARK);

				/* Break the rune */
				cave_set_feat(ny, nx, FEAT_FLOOR);

				/* Allow movement */
				do_move = TRUE;
			}
		}

		else if (do_move && (c_ptr->feat == FEAT_MINOR_GLYPH)
			&& !(r_ptr->flags1 & RF1_NEVER_BLOW))
		{
			/* Assume no move allowed */
			do_move = FALSE;

			/* Break the ward */
			if (randint(BREAK_MINOR_GLYPH) < r_ptr->level)
			{
				/* Describe observable breakage */
				if (c_ptr->info & CAVE_MARK)
				{
					if (ny == py && nx == px)
					{
						msg_print("The rune explodes!");
						fire_ball(GF_MANA, 0,
							2 * ((p_ptr->lev / 2) + damroll(7, 7)), 2);
					}
					else
						msg_print("An explosive rune was disarmed.");
				}

				/* Forget the rune */
				c_ptr->info &= ~(CAVE_MARK);

				/* Break the rune */
				cave_set_feat(ny, nx, FEAT_FLOOR);

				/* Allow movement */
				do_move = TRUE;
			}
		}

		/* Some monsters never attack */
		if (do_move && (ny == py) && (nx == px) &&
			(r_ptr->flags1 & RF1_NEVER_BLOW))
		{

			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_NEVER_BLOW);

			/* Do not move */
			do_move = FALSE;
		}

		/* The player is in the way.  Attack him. */
		if (do_move && (ny == py) && (nx == px))
		{
			/* Do the attack */
			(void)make_attack_normal(m_idx);

			/* Do not move */
			do_move = FALSE;

			/* Took a turn */
			do_turn = TRUE;
		}

		if ((cave[ny][nx].feat >= FEAT_PATTERN_START) &&
			(cave[ny][nx].feat <= FEAT_PATTERN_XTRA2) &&
			do_turn == FALSE)
		{
			do_move = FALSE;
		}


		/* A monster is in the way */
		if (do_move && c_ptr->m_idx)
		{
			monster_race *z_ptr = &r_info[y_ptr->r_idx];
			monster_type    *m2_ptr = &m_list[c_ptr->m_idx];

			/* Assume no movement */
			do_move = FALSE;

			/* Kill weaker monsters */
			/* Friends don't kill friends. */
			/* Questors never get killed. */
			/* Attack 'enemies' */
			if (((r_ptr->flags2 & (RF2_KILL_BODY)) &&
				  (r_ptr->mexp * r_ptr->level > z_ptr->mexp * z_ptr->level) &&
				  (cave_floor_bold(ny, nx))) ||
				 are_enemies(m_ptr, m2_ptr) || m_ptr->confused)
			{
				do_move = FALSE;

				if (r_ptr->flags2 & RF2_KILL_BODY) r_ptr->r_flags2 |= (RF2_KILL_BODY);

				/* attack */
				if ((m2_ptr->r_idx) && (m2_ptr->hp >= 0))
				{
					if (monst_attack_monst(m_idx, cave[ny][nx].m_idx))
					return;
				}
			}
			/* Push past weaker monsters (unless leaving a wall) */
			else if ((r_ptr->flags2 & RF2_MOVE_BODY) &&
				(r_ptr->mexp > z_ptr->mexp) && cave_floor_bold(ny,nx) &&
				(cave_floor_bold(m_ptr->fy, m_ptr->fx)))
			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster pushed past another monster */
				did_move_body = TRUE;

				/* XXX XXX XXX Message */
			}
		}

		/*
		 * Check if monster can cross terrain
		 * This is checked after the normal attacks
		 * to allow monsters to attack an enemy,
		 * even if it can't enter the terrain.
		 */
		if (do_move && !monster_can_cross_terrain(c_ptr->feat, r_ptr))
		{
			/* Assume no move allowed */
			do_move = FALSE;
		}

		/* Hack no. 2 - if the monster suddenly finds itself
		   drowning, it should try to escape */

		if  ((cave[m_ptr->fy][m_ptr->fx].feat == FEAT_DEEP_WATER) &&
			  (cave[ny][nx].feat == FEAT_DEEP_WATER))
		{
			do_move = TRUE;
		}

		/* Some monsters never move */
		if (do_move && (r_ptr->flags1 & RF1_NEVER_MOVE))
		{
			/* Hack -- memorize lack of attacks */
			/* if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_NEVER_MOVE); */

			/* Do not move */
			do_move = FALSE;
		}



		/* Creature has been allowed move */
		if (do_move)
		{
			s16b this_o_idx, next_o_idx = 0;

			/* Take a turn */
			do_turn = TRUE;

			/* Hack -- Update the old location */
			cave[oy][ox].m_idx = c_ptr->m_idx;

			/* Mega-Hack -- move the old monster, if any */
			if (c_ptr->m_idx)
			{
				/* Move the old monster */
				y_ptr->fy = oy;
				y_ptr->fx = ox;

				/* Update the old monster */
				update_mon(c_ptr->m_idx, TRUE);

				/* Wake up the moved monster */
				m_list[c_ptr->m_idx].csleep = 0;
			}

			/* Hack -- Update the new location */
			c_ptr->m_idx = m_idx;

			/* Move the monster */
			m_ptr->fy = ny;
			m_ptr->fx = nx;

			/* Update the monster */
			update_mon(m_idx, TRUE);

			/* Redraw the old grid */
			lite_spot(oy, ox);

			/* Redraw the new grid */
			lite_spot(ny, nx);

			/* Possible disturb */
			if (m_ptr->ml && (disturb_move ||
				((m_ptr->mflag & (MFLAG_VIEW)) &&
				disturb_near)))
			{
				/* Disturb */
				if (is_hostile(m_ptr) || disturb_pets)
					disturb(0, 0);
			}

			/* Scan all objects in the grid */
			for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Acquire object */
				o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Skip gold */
				if (o_ptr->tval == TV_GOLD) continue;

				/* Take or Kill objects on the floor */
				/* rr9: Pets will no longer pick up/destroy items */
				if ((r_ptr->flags2 & (RF2_TAKE_ITEM | RF2_KILL_ITEM)) &&
					 (!is_pet(m_ptr) || p_ptr->pet_pickup_items))
				{
					u64b f1, f2, f3;

					u64b flg3 = 0LL;

					char m_name[80];
					char o_name[80];

					/* Extract some flags */
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Acquire the object name */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0x04);

					/* React to objects that hurt the monster */
					if (f1 & (TR1_KILL_DRAGON)) flg3 |= (RF3_DRAGON);
					if (f1 & (TR1_SLAY_DRAGON)) flg3 |= (RF3_DRAGON);
					if (f1 & (TR1_SLAY_TROLL)) flg3 |= (RF3_TROLL);
					if (f1 & (TR1_SLAY_GIANT)) flg3 |= (RF3_GIANT);
					if (f1 & (TR1_SLAY_ORC)) flg3 |= (RF3_ORC);
					if (f1 & (TR1_SLAY_DEMON)) flg3 |= (RF3_DEMON);
					if (f1 & (TR1_SLAY_UNDEAD)) flg3 |= (RF3_UNDEAD);
					if (f1 & (TR1_SLAY_ANIMAL)) flg3 |= (RF3_ANIMAL);
					if (f1 & (TR1_SLAY_EVIL)) flg3 |= (RF3_EVIL);

					/* The object cannot be picked up by the monster */
					if (artifact_p(o_ptr) || (r_ptr->flags3 & flg3) || (o_ptr->art_name) ||
						((o_ptr->tval == TV_CORPSE) && (o_ptr->pval == m_ptr->r_idx)) ||
						((r_ptr->flags2 & RF2_SMART) && (object_value(o_ptr)) <= 0))
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
						/* Take note */
						did_take_item = TRUE;

						/* Describe observable situations */
						if (player_has_los_bold(ny, nx))
						{
							/* Dump a message */
							msg_format("%^s picks up %s.", m_name, o_name);
						}

						/* Option */
						if (testing_carry)
						{
							/* Excise the object */
							excise_object_idx(this_o_idx);

							/* Forget mark */
							o_ptr->marked = FALSE;

							/* Forget location */
							o_ptr->iy = o_ptr->ix = 0;

							/* Memorize monster */
							o_ptr->held_m_idx = m_idx;

							/* Build a stack */
							o_ptr->next_o_idx = m_ptr->hold_o_idx;

							/* Carry object */
							m_ptr->hold_o_idx = this_o_idx;
						}

						/* Nope */
						else
						{
							/* Delete the object */
							delete_object_idx(this_o_idx);
						}
					}

					/* Destroy the item if not a pet */
					else if (!is_pet(m_ptr))
					{
						/* Take note */
						did_kill_item = TRUE;

						/* Describe observable situations */
						if (player_has_los_bold(ny, nx))
						{
							/* Dump a message */
							msg_format("%^s destroys %s.", m_name, o_name);
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
	if (!do_turn && !do_move && !m_ptr->monfear && !stupid_monsters &&
		!make_attack_spell(m_idx))
	{
		/* Cast spell */
		if (make_attack_spell(m_idx)) return;
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
		if (did_open_door) r_ptr->r_flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) r_ptr->r_flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) r_ptr->r_flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) r_ptr->r_flags2 |= (RF2_KILL_ITEM);

		/* Monster pushed past another monster */
		if (did_move_body) r_ptr->r_flags2 |= (RF2_MOVE_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) r_ptr->r_flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall) r_ptr->r_flags2 |= (RF2_KILL_WALL);
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

	/* Handle "poison" */
	if (m_ptr->monpois)
	{
		int d = (r_ptr->flags1 & RF1_UNIQUE) ? randint(10) : 1;

		if (r_ptr->flags9 & RF9_HURT_POIS) d = 0; /* No recovery */

		/* Hack -- Recover from poison */
		if (m_ptr->monpois > d)
		{
			/* Recover somewhat */
			m_ptr->monpois -= d;
		}

		/* Fully recover */
		else
		{
			/* Recover fully */
			m_ptr->monpois = 0;

			/* Message if visible */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer feeling sick.", m_name);
			}
		}

		/* Still poisoned */
		if (m_ptr->monpois)
		{
		    bool fear = FALSE;
		    mon_take_hit(m_idx, poison_to_dam(m_ptr->monpois), &fear,
		     " dies from blood poisoning.");
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
 * in this function, and its children, "process_monster()" and "make_move()".
 *
 * Most of the rest of the time is spent in "update_view()" and "lite_spot()",
 * especially when the player is running.
 *
 * Note the special "MFLAG_BORN" flag, which allows us to ignore "fresh"
 * monsters while they are still being "born".  A monster is "fresh" only
 * during the turn in which it is created, and we use the "hack_m_idx" to
 * determine if the monster is yet to be processed during the current turn.
 *
 * Note the special "MFLAG_NICE" flag, which allows the player to get one
 * move before any "nasty" monsters get to use their spell attacks.
 *
 * Note that when the "knowledge" about the currently tracked monster
 * changes (flags, attacks, spells), we induce a redraw of the monster
 * recall window.
 */
void process_monsters(void)
{
	int             i, e;
	int             fx, fy;

	bool            test;

	monster_type    *m_ptr;
	monster_race    *r_ptr;

	int             old_monster_race_idx;

	u32b    old_r_flags1 = 0L;
	u32b    old_r_flags2 = 0L;
	u32b    old_r_flags3 = 0L;
	u32b    old_r_flags4 = 0L;
	u32b    old_r_flags5 = 0L;
	u32b    old_r_flags6 = 0L;

	byte    old_r_blows0 = 0;
	byte    old_r_blows1 = 0;
	byte    old_r_blows2 = 0;
	byte    old_r_blows3 = 0;

	byte    old_r_cast_inate = 0;
	byte    old_r_cast_spell = 0;


	/* Memorize old race */
	old_monster_race_idx = monster_race_idx;

	/* Acquire knowledge */
	if (monster_race_idx)
	{
		/* Acquire current monster */
		r_ptr = &r_info[monster_race_idx];

		/* Memorize flags */
		old_r_flags1 = r_ptr->r_flags1;
		old_r_flags2 = r_ptr->r_flags2;
		old_r_flags3 = r_ptr->r_flags3;
		old_r_flags4 = r_ptr->r_flags4;
		old_r_flags5 = r_ptr->r_flags5;
		old_r_flags6 = r_ptr->r_flags6;

		/* Memorize blows */
		old_r_blows0 = r_ptr->r_blows[0];
		old_r_blows1 = r_ptr->r_blows[1];
		old_r_blows2 = r_ptr->r_blows[2];
		old_r_blows3 = r_ptr->r_blows[3];

		/* Memorize castings */
		old_r_cast_inate = r_ptr->r_cast_inate;
		old_r_cast_spell = r_ptr->r_cast_spell;
	}


	/* Hack -- calculate the "player noise" */
	noise = (1L << (30 - p_ptr->skill_stl));


	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Calculate "upkeep" for pets */
		if (is_pet(m_ptr))
		{
			total_friends++;
			total_friend_levels += r_info[m_ptr->r_idx].level;
		}


		/* Handle "fresh" monsters */
		if (m_ptr->mflag & (MFLAG_BORN))
		{
			/* No longer "fresh" */
			m_ptr->mflag &= ~(MFLAG_BORN);

			/* Skip */
			continue;
		}


		/* Obtain the energy boost */
		e = extract_energy[m_ptr->mspeed];

		/* Give this monster some energy */
		m_ptr->energy += e;


		/* Not enough energy to move */
		if (m_ptr->energy < 100) continue;

		/* Use up "some" energy */
		m_ptr->energy -= 100;


		/* Hack -- Require proximity */
		if (m_ptr->cdis >= 100) continue;


		/* Access the race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Access the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;


		/* Assume no move */
		test = FALSE;

		/* Handle "sensing radius" */
		if (m_ptr->cdis <= r_ptr->aaf)
		{
			/* We can "sense" the player */
			test = TRUE;
		}

		/* Handle "sight" and "aggravation" */
		else if ((m_ptr->cdis <= MAX_SIGHT) &&
			(player_has_los_bold(fy, fx) ||
			p_aggravate))
		{
			/* We can "see" or "feel" the player */
			test = TRUE;
		}

#ifdef MONSTER_FLOW
		/* Hack -- Monsters can "smell" the player from far away */
		/* Note that most monsters have "aaf" of "20" or so */
		else if (flow_by_sound &&
			(cave[py][px].when == cave[fy][fx].when) &&
			(cave[fy][fx].cost < MONSTER_FLOW_DEPTH) &&
			(cave[fy][fx].cost < r_ptr->aaf))
		{
			/* We can "smell" the player */
			test = TRUE;
		}
#endif /* MONSTER_FLOW */


		/* Do nothing */
		if (!test) continue;

		/* Save global index */
		hack_m_idx = i;

		/* Process the monster */
		process_monster(i);

		/* Hack -- notice death or departure */
		if (!alive || death) break;

		/* Notice leaving */
		if (p_ptr->leaving) break;
	}

	/* Reset global index */
	hack_m_idx = 0;


	/* Tracking a monster race (the same one we were before) */
	if (monster_race_idx && (monster_race_idx == old_monster_race_idx))
	{
		/* Acquire monster race */
		r_ptr = &r_info[monster_race_idx];

		/* Check for knowledge change */
		if ((old_r_flags1 != r_ptr->r_flags1) ||
			(old_r_flags2 != r_ptr->r_flags2) ||
			(old_r_flags3 != r_ptr->r_flags3) ||
			(old_r_flags4 != r_ptr->r_flags4) ||
			(old_r_flags5 != r_ptr->r_flags5) ||
			(old_r_flags6 != r_ptr->r_flags6) ||
			(old_r_blows0 != r_ptr->r_blows[0]) ||
			(old_r_blows1 != r_ptr->r_blows[1]) ||
			(old_r_blows2 != r_ptr->r_blows[2]) ||
			(old_r_blows3 != r_ptr->r_blows[3]) ||
			(old_r_cast_inate != r_ptr->r_cast_inate) ||
			(old_r_cast_spell != r_ptr->r_cast_spell))
		{
			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);
		}
	}
}
