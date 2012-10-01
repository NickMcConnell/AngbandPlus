/* File: melee2.c */

/* Purpose: Monster spells and movement */

/*
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

#define FOLLOW_DISTANCE 6

#define SURROUND_MAX 80

static bool too_weak(monster_type *m_ptr)
{
	/*monster_race *r_ptr = &r_info[m_ptr->r_idx];*/
	
	/*if (p_ptr->lev > (r_ptr->level * 4)) return (TRUE);*/

	return (FALSE);
}

static int check_monsters[SURROUND_MAX][2] =
{
	{-1,  0},
	{ 0, -1},
	{ 1,  0},
	{ 0, +1},

	{-1, -1},
	{ 1, -1},
	{ 1,  1},
	{-1,  1},

	{-2,  0},
	{ 0, -2},
	{ 2,  0},
	{ 0,  2},

	{-2, -1},
	{ 1, -2},
	{ 2,  1},
	{-1,  2},

	{-1, -2},
	{ 2, -1},
	{ 1,  2},
	{-2,  1},

	{-2, -2},
	{ 2, -2},
	{ 2,  2},
	{-2,  2},

	{-3,  0},
	{ 0, -3},
	{ 3,  0},
	{ 0,  3},

	{-3, -1},
	{ 1, -3},
	{ 3,  1},
	{-1,  3},

	{-3,  1},
	{-1, -3},
	{ 3, -1},
	{ 1,  3},

	{-3, -2},
	{ 2, -3},
	{ 3,  2},
	{-2,  3},

	{-3,  2},
	{-2, -3},
	{ 3, -2},
	{ 2,  3},

	{-3, -3},
	{ 3, -3},
	{ 3,  3},
	{-3,  3},

	{-4,  0},
	{ 0, -4},
	{ 4,  0},
	{ 0,  4},

	{ 1, -4},
	{ 4,  1},
	{-1,  4},
	{-4, -1},

	{-4,  1},
	{-1, -4},
	{ 4, -1},
	{ 1,  4},

	{-4, -2},
	{ 2, -4},
	{ 4,  2},
	{-2,  4},

	{-4,  2},
	{-2, -4},
	{ 4, -2},
	{ 2,  4},

	{-4, -3},
	{ 3, -4},
	{ 4,  3},
	{-3,  4},

	{-4,  3},
	{-3, -4},
	{ 4, -3},
	{ 3,  4},

	{-4, -4},
	{ 4, -4},
	{ 4,  4},
	{-4,  4},
};


/*
 * Calculate the direction to the next enemy
 */
static void get_enemy_dir(monster_type *m_ptr, int *mm)
{
	int i;
	int x, y;

	monster_type *enemy_ptr;

	/* Scan the surounding fields for enemies */
	for (i = 0; i < SURROUND_MAX; i++)
	{
		y = m_ptr->fy + check_monsters[i][0];
		x = m_ptr->fx + check_monsters[i][1];

                if (!in_bounds(y, x))
                        continue;

		/* Hack -- no fighting away from player */
                if (m_list[cave[y][x].m_idx].cdis > p_ptr->pet_follow_distance) continue;

		if (in_bounds(y, x))
		{
			if (cave[y][x].m_idx)
			{
				monster_race *enemyr_ptr;
				monster_race *r_ptr;
				bool dontattack = FALSE;
				enemy_ptr = &m_list[cave[y][x].m_idx];

				enemyr_ptr = &r_info[enemy_ptr->r_idx];
				r_ptr = &r_info[m_ptr->r_idx];

				/* Guards don't attack friendly monsters */
				if (r_ptr->flags7 & (RF7_GUARD))
				{
					/* Don't attack if it's friendly, and not summoned. */
					if (is_pet(enemy_ptr) && (enemy_ptr->summoned == 0)) dontattack = TRUE;
				}

				/* Friendly monsters won't attack guards if they aren't summoned. */
				if (enemyr_ptr->flags7 & (RF7_GUARD))
				{
					if (is_pet(m_ptr) && (m_ptr->summoned == 0 && !(m_ptr->friend))) dontattack = TRUE;
				}

				/* Valid target if enemy and not asleep */
				if ((is_pet(enemy_ptr) != is_pet(m_ptr)) &&
					!enemy_ptr->csleep && !(enemyr_ptr->flags7 & (RF7_NEVER_ATTACKED)) && !(dontattack))
				{
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

					break;
				}
			}
		}
	}
}

/*
* Hack, based on mon_take_hit... perhaps all monster attacks on
* other monsters should use this?
*/
static void mon_take_hit_mon(int m_idx, s32b dam, bool *fear, cptr note)
{
	monster_type	*m_ptr = &m_list[m_idx];
	
	monster_race	*r_ptr = &r_info[m_ptr->r_idx];
	
	s32b            div, new_exp, new_exp_frac;
        int tempint = 1;
	
	/* Redraw (later) if needed */
	if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
	
	/* Wake it up */
	m_ptr->csleep = 0;
	
	/* Hurt it */
	m_ptr->hp -= dam;
	
	/* It is dead now... or is it? */
	if (m_ptr->hp < 0)
        {
                /* ((r_ptr->flags1 & RF1_UNIQUE)) */
                /* Useless part of code... It's here for HUGE HUGE HUGE historical reasons! ;) */
                if (tempint == 0)
		{
                        m_ptr->hp = -1;
		}
		else
		{
			char m_name[80];
			
			/* Extract monster name */
			monster_desc(m_name, m_ptr, 0);

			if (m_ptr->lives > 0)
			{
				sound(SOUND_FLEE);
				m_ptr->lives -= 1;
				msg_format("%^s has lost a life point.", m_name);
				m_ptr->hp = m_ptr->maxhp;
				return;
			}

			/* Immortality. */
			if ((r_ptr->flags7 & (RF7_IMMORTAL)) && (enemy_immortality))
			{
				msg_format("%^s has been knocked out.", m_name);
				m_ptr->hp = 1;
				m_ptr->seallight = 5;
				return;
			}

			/* Make a sound */
			if ((r_ptr->flags3 & RF3_DEMON) ||
			    (r_ptr->flags3 & RF3_UNDEAD) ||
			    (r_ptr->flags2 & RF2_STUPID) ||
			    (r_ptr->flags3 & RF3_NONLIVING) ||
				(strchr("Evg", r_ptr->d_char)))
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
			else if ((r_ptr->flags3 & (RF3_DEMON)) ||
				(r_ptr->flags3 & (RF3_UNDEAD)) ||
				(r_ptr->flags2 & (RF2_STUPID)) ||
				(r_ptr->flags3 & (RF3_NONLIVING)) ||
				(strchr("Evg", r_ptr->d_char)))
			{
				msg_format("%^s is destroyed.", m_name);
			}
			else
			{
				msg_format("%^s is killed.", m_name);
			}

#ifdef PET_GAIN_EXP
                {
		/* Maximum player level */
		div = p_ptr->max_plv;

		/* Give some experience for the kill */
		new_exp = ((long)r_ptr->mexp * r_ptr->level) / div;

		/* Handle fractional experience */
		new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % div)
		                * 0x10000L / div) + p_ptr->exp_frac;

		/* Keep track of experience */
		if (new_exp_frac >= 0x10000L)
		{
			new_exp++;
			p_ptr->exp_frac = new_exp_frac - 0x10000;
		}
		else
		{
			p_ptr->exp_frac = new_exp_frac;
		}

                /* New for 1.5.0...*/
                new_exp = new_exp / 10;

                /* Friends are worth no experience! */
                if (is_pet(m_ptr))
                {
                        new_exp = 0;
                }

		if (p_ptr->inside_quest || too_weak(m_ptr) || (m_ptr->no_experience)) new_exp = 0;

		/* You killed something! Now, check if you should */
                /* advance your class level... */
		if (!((p_ptr->inside_quest || too_weak(m_ptr) || (m_ptr->no_experience)) && !(r_ptr->flags1 & (RF1_UNIQUE)))) add_class_kill(m_ptr);

		/* Gain experience */
                gain_exp_kill(new_exp, m_ptr);
                }
#endif
			
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
	
	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)))
	{
		int		percentage;
		
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

                        /* Nor on the between */
                        if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

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
static bool clean_shot(int y1, int x1, int y2, int x2)
{
	int dist, y, x;
	
	/* Start at the initial location */
	y = y1, x = x1;
	
	/* See "project()" and "projectable()" */
	for (dist = 0; dist <= MAX_RANGE; dist++)
	{
		/* Never pass through walls */
		/* Although it can hit a player on a wall. */
		if (dist && (!cave_floor_bold_project(y, x) && py != y && px != x)) break;
		
		/* Never pass through monsters */
		if (dist && cave[y][x].m_idx > 0)
		{
			if (!is_pet(&m_list[cave[y][x].m_idx])) break;
		}
		
		/* Check for arrival at "final target" */
		if ((x == x2) && (y == y2)) return (TRUE);
		
		/* Calculate the new location */
		mmove2(&y, &x, y1, x1, y2, x2);
	}
	
	/* Assume obstruction */
	return (FALSE);
}


/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void bolt(int m_idx, int typ, s32b dam_hp)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
        bool absorb = FALSE;
        monster_type *m_ptr = &m_list[m_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        no_magic_return = TRUE;
        if (m_ptr->abilities & (CURSE_HALVE_MAGIC)) dam_hp -= dam_hp / 2;
        else if (m_ptr->abilities & (CURSE_LOWER_MAGIC)) dam_hp -= dam_hp / 4;

        /* Absorb the hp! :) */
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 8] >= 1)
        {
                int chance;
                chance = p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 8];
                if (typ == p_ptr->elemlord) chance *= 5;
                if (randint(100) <= chance)
                {
                        msg_print("You absorb the energy!");
                        p_ptr->chp += dam_hp;
                        if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
                        absorb = TRUE;
                        update_and_handle();
                }
        }

	
	/* Target the player with a bolt attack */
        if (!absorb) (void)project(m_idx, 0, py, px, dam_hp, typ, flg);

        no_magic_return = FALSE;
}

/* Similar to bolt, but no magic blocking tests. */
static void arrow(int m_idx, int typ, s32b dam_hp)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
        monster_type *m_ptr = &m_list[m_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        no_magic_return = TRUE;
	
	/* Target the player with a bolt attack */
        (void)project(m_idx, 0, py, px, dam_hp, typ, flg);

        no_magic_return = FALSE;
}

/* Radius ranged attack! */
static void canon(int m_idx, int typ, s32b dam_hp, int rad)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Target the player with a ball attack */
        (void)project(m_idx, 0, py, px, dam_hp, typ, flg);

        no_magic_return = FALSE;
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
	
	/* Hand of Doom */
	if (spell == 160 + 1) return (TRUE);
	
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
	
	/* Brain smash, et al (added curses) */
	if (spell >= 128 + 9 && spell <= 128 + 14) return (TRUE);
	
	/* Scare, confuse, blind, slow, paralyze */
	if (spell >= 128 + 27 && spell <= 128 + 31) return (TRUE);
	
	/* Teleport to */
	if (spell == 160 + 8) return (TRUE);
	
#if 0
	/* Hand of Doom */
	if (spell == 160 + 1) return (TRUE);
#endif
	
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
        if (spell >= 160 + 13) return (TRUE);
	
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
	}
	
	/*** Try to pick an appropriate spell type ***/
	
	/* Hurt badly or afraid, attempt to flee */
	if ((m_ptr->hp < m_ptr->maxhp / 3) || m_ptr->monfear)
	{
		/* Choose escape spell if possible */
		if (escape_num) return (escape[rand_int(escape_num)]);
	}
	
	/* Still hurt badly, couldn't flee, attempt to heal */
        if ((m_ptr->hp < m_ptr->maxhp / 3) || m_ptr->r_idx == 1125)
	{
		/* Choose heal spell if possible */
		if (heal_num) return (heal[rand_int(heal_num)]);
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
	
	/* Try another tactical spell (sometimes) */
	if (tactic_num && (rand_int(100) < 50))
	{
		/* Choose tactic spell */
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
static void breath(int m_idx, int typ, s32b dam_hp, int rad)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
        bool absorb = FALSE;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

        no_magic_return = TRUE;
	/* Determine the radius of the blast */
	if (rad < 1) rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;
        /* Lower Magic curse... */
        if (m_ptr->abilities & (CURSE_HALVE_MAGIC)) dam_hp -= dam_hp / 2;
        else if (m_ptr->abilities & (CURSE_LOWER_MAGIC)) dam_hp -= dam_hp / 4;

        /* Absorb the hp! :) */
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 8] >= 1)
        {
                int chance;
                chance = p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 8];
                if (typ == p_ptr->elemlord) chance *= 5;
                if (randint(100) <= chance)
                {
                        msg_print("You absorb all the energy!");
                        p_ptr->chp += dam_hp;
                        if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
                        absorb = TRUE;
                        update_and_handle();
                }
        }

	/* Target the player with a ball attack */
        if (absorb != TRUE) (void)project(m_idx, rad, py, px, dam_hp, typ, flg);

        no_magic_return = FALSE;
}


/*
 * Monster casts a breath (or ball) attack at another monster.
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void monst_breath_monst(int m_idx, int y, int x, int typ, s32b dam_hp, int rad)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

        no_magic_return = TRUE;
	/* Determine the radius of the blast */
	if (rad < 1) rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;
	
	(void)project(m_idx, rad, y, x, dam_hp, typ, flg);
        no_magic_return = FALSE;
}


/*
 * Monster casts a bolt at another monster
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void monst_bolt_monst(int m_idx, int y, int x, int typ, s32b dam_hp)
{
	int flg = PROJECT_STOP | PROJECT_KILL;

        monster_type *m_ptr = &m_list[m_idx];

        no_magic_return = TRUE;
	
	(void)project(m_idx, 0, y, x, dam_hp, typ, flg);
        no_magic_return = FALSE;
}


/*
 * Monster tries to 'cast a spell' (or breath, etc)
 * at another monster.
 */
static bool monst_spell_monst(int m_idx)
{
	int y = 0, x = 0;
	int i, k, j, t_idx;
	int chosenspell, numspells;
	int chance, thrown_spell, count = 0;
	byte spell[96], num = 0;
	char m_name[80], t_name[80];
	char m_poss[80];
	char ddesc[80];
	int rlev;				/* monster level */
	monster_type *m_ptr = &m_list[m_idx];	/* Attacker */
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_type *t_ptr;			/* Putative target */
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
	
	bool friendly = FALSE;
	
	if (is_pet(m_ptr)) friendly = TRUE;

	/* Locked monsters cannot cast spells! */
        if (m_ptr->abilities & (CURSE_LOCK)) return (FALSE);

	/* Taunted monsters cast spells only 50% of the time. */
        if (m_ptr->abilities & (TAUNTED))
	{
		int proll;
		int mroll;
		int chrbonus;

		proll = (p_ptr->abilities[(CLASS_FIGHTER * 10) + 2] * 20);
		mroll = m_ptr->level + m_ptr->mind;
		chrbonus = (p_ptr->stat_ind[A_CHR] - 5) * 5;

		if (chrbonus < 0) chrbonus = 0;
		if (chrbonus > (proll * 2)) chrbonus = proll;

		proll += chrbonus;

		if (lua_randint(proll) >= lua_randint(mroll)) return (FALSE);
	}
	
	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	/* Hack -- Extract the spell probability */
        chance = (r_ptr->spellchance);
	 
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
		
		/* Monster must be 'an enemy' */
		if ((is_pet(m_ptr)) == (is_pet(t_ptr))) continue;

		/* Can we attack the target? */
		if (tr_ptr->flags7 & (RF7_NEVER_ATTACKED)) continue;

		/* Guards don't attack friendly monsters */
		if (r_ptr->flags7 & (RF7_GUARD))
		{
			/* Don't attack if it's friendly, and not summoned. */
			if (is_pet(t_ptr) && (t_ptr->summoned == 0)) continue;
		}

		/* Friendly monsters won't attack guards if they aren't summoned. */
		if (tr_ptr->flags7 & (RF7_GUARD))
		{
			if (is_pet(m_ptr) && (m_ptr->summoned == 0 && !(m_ptr->friend))) continue;
		}
		
		/* Hack -- no fighting >100 squares from player */
		if (t_ptr->cdis > MAX_RANGE) continue;
		
		/* Monster must be projectable */
		if (!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx)) continue;
		
		/* OK -- we-ve got a target */
		y = t_ptr->fy;
		x = t_ptr->fx;
		
		/* Extract the monster level */
		rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
		
		/* Extract the racial spell flags */
		f4 = r_ptr->flags4;
		f5 = r_ptr->flags5;
		f6 = r_ptr->flags6;
		
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
		
		/* NewAngband 1.8.0 New monster spells system! :) */
		/* Monsters can cast more than one spell... */
		for (j = 0; j < r_ptr->spells; j++)
		{
		/* First, find the number of available spells */
		i = 0;
		numspells = 0;
		while (i < 20 && r_ptr->spell[i].type > 0) 
		{
			numspells++;
			i++;
		}
	
		chosenspell = -1;
		/* Enter AI code here */
	
		/* Monster is weak... */
		/* Use an healing or teleport spell if possible! */
		if (m_ptr->hp <= (m_ptr->maxhp / 4))
		{
			/* Teleport spells will usually work better. */
			for(i = 0; i < numspells; i++)
			{
				if (r_ptr->spell[i].type == 8 && m_ptr->mana >= r_ptr->spell[i].cost)
				{
					chosenspell = i;
				}
			}
			if (chosenspell = -1)
			{
				for(i = 0; i < numspells; i++)
				{
					if (r_ptr->spell[i].type == 3 && m_ptr->mana >= r_ptr->spell[i].cost)
					{
						chosenspell = i;
					}
				}
			}
		}
		/* All right, then look for an haste spell */
		if (m_ptr->hasted == 0 && chosenspell == -1)
		{
			for(i = 0; i < numspells; i++)
			{
				if (r_ptr->spell[i].type == 4 && m_ptr->mana >= r_ptr->spell[i].cost)
				{
					chosenspell = i;
				}
			}
		}
		/* If hasted and no need for healing, let's cast a boost! */
		if (m_ptr->boosted == 0 && chosenspell == -1)
		{
			for(i = 0; i < numspells; i++)
			{
				if (r_ptr->spell[i].type == 5 && m_ptr->mana >= r_ptr->spell[i].cost)
				{
					chosenspell = i;
				}
			}
		}
		/* Otherwise, choose a spell that is not 3, 4 or 5(if any) */
		/* Nothign to cast? Forget spellcasting then... */
		if (chosenspell == -1)
		{
			bool cancast = FALSE;
			/* First, check if we have ANY other spells */
			for(i = 0; i < numspells; i++)
			{
				if (r_ptr->spell[i].type != 3 && r_ptr->spell[i].type != 4 && r_ptr->spell[i].type != 5)
				{
					if (m_ptr->mana >= r_ptr->spell[i].cost) cancast = TRUE;
				}
			}
			/* Pick a random spell from available spells */
			if (cancast)
			{
				int tmpspell;
				while (chosenspell == -1)
				{
					tmpspell = randint(numspells) - 1;
					if (r_ptr->spell[tmpspell].type != 3 && r_ptr->spell[tmpspell].type != 4 && r_ptr->spell[tmpspell].type != 5)
					{
						if (m_ptr->mana >= r_ptr->spell[tmpspell].cost) chosenspell = tmpspell;
					}
				}
			}
			else return (FALSE);			
		}
                monster_cast_spell_monst(m_idx, m_ptr, y, x, t_ptr, chosenspell);        
   }

   /* Deactivate the HACK */
   summoner_monster = NULL;
 
   if (wake_up)
   {
	   t_ptr->csleep = 0;
   }
   
   
   /* Remember what the monster did, if we saw it */
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
        u32b    o1, o2, o3, o4;
	object_type * o_ptr = &inventory[INVEN_WIELD - 1 + randint(12)];

	if (randint(100) > chance) return;

	if (!(o_ptr->k_idx)) return;

        object_flags(o_ptr, &o1, &o2, &o3, &o4);


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

void curse_equipment_dg(int chance, int heavy_chance)
{
	bool changed = FALSE;
        u32b    o1, o2, o3, o4;
	object_type * o_ptr = &inventory[INVEN_WIELD - 1 + randint(12)];

	if (randint(100) > chance) return;

	if (!(o_ptr->k_idx)) return;

        object_flags(o_ptr, &o1, &o2, &o3, &o4);


	/* Extra, biased saving throw for blessed items */
	if ((o3 & (TR3_BLESSED)) && (randint(888) > chance))
	{   
		char o_name[256];
		object_desc(o_name, o_ptr, FALSE, 0);
		msg_format("Your %s resist%s cursing!", o_name,
			((o_ptr->number > 1) ? "" : "s"));
		/* Hmmm -- can we wear multiple items? If not, this is unnecessary */
                /* DG -- Yes we can, in the quiver */
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

/* Function updated for NewAngband 1.8.0! :) */
bool make_attack_spell(int m_idx)
{
	int             k, chance, thrown_spell, rlev, failrate, i, j;
	int		numspells, chosenspell;
	byte            spell[96], num = 0;
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

        /* Locked monsters cannot cast spells! */
        if (m_ptr->abilities & (CURSE_LOCK)) return (FALSE);

	/* Taunted monsters cast spells only 50% of the time. */
        if ((m_ptr->abilities & (TAUNTED)) && randint(100) >= 50) return (FALSE);

	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	/* Cannot cast spells when nice */
	if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);
	if (is_pet(m_ptr)) return (FALSE);

	/* Hack -- Extract the spell probability */
        chance = (r_ptr->spellchance);
	 
	/* Not allowed to cast spells */
	if (!chance) return (FALSE);

	/* We don't necessarely ALWAYS cast a spell. */
        if (rand_int(100) >  chance) return (FALSE);

	/* XXX XXX XXX Handle "track_target" option (?) */
	 
	 
	/* Hack -- require projectable player */
	if (normal)
	{
		/* Check range */
		if (m_ptr->cdis > MAX_RANGE) return (FALSE);

		/* Check path */
		if (!projectable(m_ptr->fy, m_ptr->fx, py, px)) return (FALSE);

		if (!clean_shot(m_ptr->fy, m_ptr->fx, py, px)) return (FALSE);

		/* Is the player invisible? */
		if (player_invis(m_ptr)) return (FALSE);
	}

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

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

	/* NewAngband 1.8.0 New monster spells system! :) */
	/* Monsters can cast more than one spell... */
	for (j = 0; j < r_ptr->spells; j++)
	{
	/* First, find the number of available spells */
	i = 0;
	numspells = 0;
	while (i < 20 && r_ptr->spell[i].type > 0) 
	{
		numspells++;
		i++;
	}
	
	chosenspell = -1;
	/* Enter AI code here */
	
	/* Monster is weak... */
	/* Use an healing spell if possible! */
	if (m_ptr->hp <= (m_ptr->maxhp / 4))
	{
		for(i = 0; i < numspells; i++)
		{
			if (r_ptr->spell[i].type == 3 && m_ptr->mana >= r_ptr->spell[i].cost)
			{
				chosenspell = i;
			}
		}
	}
	/* All right, then look for an haste spell */
	if (m_ptr->hasted == 0 && chosenspell == -1)
	{
		for(i = 0; i < numspells; i++)
		{
			if (r_ptr->spell[i].type == 4 && m_ptr->mana >= r_ptr->spell[i].cost)
			{
				chosenspell = i;
			}
		}
	}
	/* If hasted and no need for healing, let's cast a boost! */
	if (m_ptr->boosted == 0 && chosenspell == -1)
	{
		for(i = 0; i < numspells; i++)
		{
			if (r_ptr->spell[i].type == 5 && m_ptr->mana >= r_ptr->spell[i].cost)
			{
				chosenspell = i;
			}
		}
	}
	/* Otherwise, choose a spell that is not 3, 4 or 5(if any) */
	/* Nothign to cast? Forget spellcasting then... */
	if (chosenspell == -1)
	{
		bool cancast = FALSE;
		/* First, check if we have ANY other spells */
		for(i = 0; i < numspells; i++)
		{
			if (r_ptr->spell[i].type != 3 && r_ptr->spell[i].type != 4 && r_ptr->spell[i].type != 5)
			{
				if (m_ptr->mana >= r_ptr->spell[i].cost) cancast = TRUE;
			}
		}
		/* Pick a random spell from available spells */
		if (cancast)
		{
			int tmpspell;
			while (chosenspell == -1)
			{
				tmpspell = randint(numspells) - 1;
				if (r_ptr->spell[tmpspell].type != 3 && r_ptr->spell[tmpspell].type != 4 && r_ptr->spell[tmpspell].type != 5)
				{
					if (m_ptr->mana >= r_ptr->spell[tmpspell].cost) chosenspell = tmpspell;
				}
			}
		}
		else return (FALSE);
	}

	monster_cast_spell(m_idx, m_ptr, chosenspell);
	

        /* Deactivate the HACK */
        summoner_monster = NULL;
	
	/* Remember what the monster did to us */
	if (seen)
	{
		r_ptr->r_spells[chosenspell] = 1;
		
	}
	
	update_and_handle();
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
	
	/* Always take note of monsters that kill you */
	if (death && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
	}

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
	if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);
	
	/* Friends don't run away */
	if (is_pet(m_ptr)) return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);

        /* The monsters have no moral at all! ;) */
        return (FALSE);

        /* The next lines of code are useless... */
        /* Why would a monster run away from a pathetic player anyway? ;) */
	
#ifdef ALLOW_TERROR
	
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
	p_val = (p_lev * p_mhp) + (p_chp << 2); /* div p_mhp */
	m_val = (m_lev * m_mhp) + (m_chp << 2); /* div m_mhp */
	
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
 */
static bool get_moves(int m_idx, int *mm)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	int y, ay, x, ax;
	
	int move_val = 0;
	
	int y2 = py;
	int x2 = px;
	bool done = FALSE;

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
	
	if (!stupid_monsters && !is_pet(m_ptr))
	{
	/*
	 * Animal packs try to get the player out of corridors
	 * (...unless they can move through walls -- TY)
	 */
		if ((r_ptr->flags1 & RF1_FRIENDS) &&
			(r_ptr->flags3 & RF3_ANIMAL) &&
			!((r_ptr->flags2 & (RF2_PASS_WALL)) ||
			(r_ptr->flags2 & (RF2_KILL_WALL))))
		{
			int i, room = 0;
			
			/* Count room grids next to player */
			for (i = 0; i < 8; i++)
			{
				/* Check grid */
				if (cave[py + ddy_ddd[i]][px + ddx_ddd[i]].info & (CAVE_ROOM))
				{
					/* One more room grid */
					room++;
				}
			}

			/* Not in a room and strong player */
			if ((room < 8) && (p_ptr->chp > ((p_ptr->mhp * 3) / 4)))
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
	if ((stupid_monsters) || (is_pet(m_ptr)))
	{
		if (mon_will_run(m_idx))
		{
			/* XXX XXX Not very "smart" */
			y = (-y), x = (-x);
		}
	}
	else
	{
                if (mon_will_run(m_idx))
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
	monster_type    *m_ptr = &m_list[m_idx],*t_ptr = &m_list[t_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	monster_race	*tr_ptr = &r_info[t_ptr->r_idx];
	int             ap_cnt;
	int		i, j, flg;
	char            m_name[80],t_name[80];
	char            ddesc[80],temp[80];
	bool            blinked = FALSE, touched = FALSE;
	bool			explode = FALSE;
	bool			fear = FALSE;
	byte            y_saver = t_ptr->fy;
	byte            x_saver = t_ptr->fx;


	/* Not allowed to attack */
	if (r_ptr->flags1 & RF1_NEVER_BLOW) return FALSE;

	/* Guards don't attack friendly monsters */
	if (r_ptr->flags7 & (RF7_GUARD))
	{
		/* Don't attack if it's friendly, and not summoned. */
		if (is_pet(t_ptr) && (t_ptr->summoned == 0)) return FALSE;
	}

	/* Friendly monsters won't attack guards if they aren't summoned. */
	if (tr_ptr->flags7 & (RF7_GUARD))
	{
		if (is_pet(m_ptr) && (m_ptr->summoned == 0 && !(m_ptr->friend))) return FALSE;
	}

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

	/* Attack until we have no more blows! */
	for (i = 0; i < r_ptr->attacks; i++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;
		bool nothurt = FALSE;

		int power = 0;
		int numattacks = 0;
		int chosen = 0;
                s32b damage = 0;

		cptr act = NULL;

		/* Stop if player is dead or gone */
		if (!alive || death) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Make sure the monster is still alive */
		if (!m_ptr) break;

		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* We can have up to 50!! different attacks. */
		/* Let's choose one! */
		j = 0;
		while (r_ptr->attack[j].type > 0 && numattacks < 50) 
		{
			numattacks++;
			j++;
		}

		/* Now, choose an attack! */
		chosen = randint(numattacks) - 1;

		if (r_ptr->attack[chosen].name == "!") msg_format("%^s %s %^s!", m_name, r_ptr->attack[chosen].act, t_name);
		else msg_format("%^s %s %^s with %s!", m_name, r_ptr->attack[chosen].act, t_name, r_ptr->attack[chosen].name);
                
		/* What type of attack did the monster do? */
		switch (r_ptr->attack[chosen].type)
		{

			/* Normal attack */
			case 1:
			{
				/* Call a lua script. */
				if (r_ptr->event_before_melee > 0)
				{
					call_lua("monster_before_melee", "(dd)", "", m_idx, r_ptr->event_before_melee);
				}

				if (monster_hit_monster(m_ptr, t_ptr))
				{
					damage = damroll(r_ptr->attack[chosen].ddice, r_ptr->attack[chosen].dside);
					if (is_pet(m_ptr)) damage *= ((m_ptr->skill_attack + p_ptr->skill[9]) + 1);
					else damage *= (m_ptr->skill_attack + 1);
					if (is_pet(m_ptr))
					{
						damage += multiply_divide(damage, ((m_ptr->str - 5) + (p_ptr->stat_ind[A_CHR] - 5)) * 5, 100);
					}
					else damage += multiply_divide(damage, (m_ptr->str - 5) * 5, 100);
					damage += multiply_divide(damage, m_ptr->str, 100);
					/* Bosses may get higher damages! */
					if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) damage *= 2;
                        		if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) damage -= damage / 2;
                        		else if (m_ptr->abilities & (CURSE_LOWER_POWER)) damage -= damage / 4;
					

					if (!nothurt)
					{
						msg_format("%^s is hit!", t_name);
						flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL;
                				no_magic_return = TRUE;
                				(void)project(m_idx, 0, y_saver, x_saver, damage, r_ptr->attack[chosen].element, flg);
                				no_magic_return = FALSE;						
					}
				}
				else msg_format("%^s misses.", m_name);

				/* Call a lua script. */
				if (r_ptr->event_after_melee > 0)
				{
					call_lua("monster_after_melee", "(dd)", "", m_idx, r_ptr->event_after_melee);
				}
				break;
			}
			/* Animated monster attack */
			case 2:
			{
				/* Call a lua script. */
				if (r_ptr->event_before_melee > 0)
				{
					call_lua("monster_before_melee", "(dd)", "", m_idx, r_ptr->event_before_melee);
				}

				if (monster_hit_monster(m_ptr, t_ptr))
				{
					damage = damroll(m_ptr->animdam_d, m_ptr->animdam_s);
					if (is_pet(m_ptr)) damage *= ((m_ptr->skill_attack + p_ptr->skill[9]) + 1);
					else damage *= (m_ptr->skill_attack + 1);
					if (is_pet(m_ptr))
					{
						damage += multiply_divide(damage, ((m_ptr->str - 5) + (p_ptr->stat_ind[A_CHR] - 5)) * 5, 100);
					}
					else damage += multiply_divide(damage, (m_ptr->str - 5) * 5, 100);
					damage += multiply_divide(damage, m_ptr->str, 100);
					/* Bosses may get higher damages! */
					if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) damage *= 2;
                        		if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) damage -= damage / 2;
                        		else if (m_ptr->abilities & (CURSE_LOWER_POWER)) damage -= damage / 4;
					

					if (!nothurt)
					{
						msg_format("%^s is hit!", t_name);
						flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL;
                				no_magic_return = TRUE;
                				(void)project(m_idx, 0, y_saver, x_saver, damage, r_ptr->attack[chosen].element, flg);
                				no_magic_return = FALSE;						
					}
				}
				else msg_format("%^s misses.", m_name);

				/* Call a lua script. */
				if (r_ptr->event_after_melee > 0)
				{
					call_lua("monster_after_melee", "(dd)", "", m_idx, r_ptr->event_after_melee);
				}

				break;
			}
		}        
	}                                          

	/* Always notice cause of death */
	if (death && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
	}

	/* Assume we attacked */
	return (TRUE);

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
bool player_invis(monster_type * m_ptr)
{
	s16b inv;
	int ppower;
	int mpower;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

        ppower = p_ptr->invis + (p_ptr->skill[6] * 3);
	mpower = m_ptr->level + m_ptr->mind;

	/* Stealth doesn't work against Questors and Invisible enemies. */
	if ((r_ptr->flags1 & RF1_QUESTOR) || (r_ptr->flags2 & RF2_INVISIBLE)) return (FALSE);

	/* Your friends knows where you are! */
	if (is_pet(m_ptr)) return (FALSE);

	/* Don't bother rolling if power is 0. */
	if (ppower == 0) return (FALSE);

	/* Elites and Bosses gets double rolls. */
	if (m_ptr->boss >= 1) mpower *= 2;

	/* Uniques have much more power. */
	if (r_ptr->flags1 & RF1_UNIQUE) mpower *= 5;

	if (randint(ppower) >= randint(mpower)) return (TRUE);

	return (FALSE);
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
static void process_monster(int m_idx, bool is_friend)
{
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	
	int             i, d, oy, ox, ny, nx;
	
	int             mm[8];
	
	cave_type       *c_ptr;
	
	monster_type    *y_ptr;

        bool            field_dam = FALSE;
        int             fieldtype = 0;
        int             field_dam_amount = 0;

	
	bool            do_turn;
	bool            do_move;
	bool            do_view;
	
	bool            did_open_door;
	bool            did_bash_door;
	bool            did_take_item;
	bool            did_kill_item;
	bool            did_move_body;
	bool            did_kill_body;
	bool            did_pass_wall;
	bool            did_kill_wall;
	bool            gets_angry = FALSE;
	bool inv;

	inv = player_invis(m_ptr);

	/* Call a lua script. */
	if (r_ptr->event_passive > 0)
	{
		call_lua("monster_passive", "(dd)", "", m_idx, r_ptr->event_passive);
	}

	/* Monster is summoned. It will eventually fade... */
	if (m_ptr->summoned > 0)
        {
                m_ptr->summoned -= 1;
		if (m_ptr->summoned <= 0)
		{
			char m_name[80];
					
			/* Acquire the monster name */
			monster_desc(m_name, m_ptr, 0);
					
			/* Dump a message */
			msg_format("%^s disappear!", m_name);
			delete_monster_idx(m_idx);
			return;
		}
        }

        /* If the monster is sealed by Sealing Light, don't do anything */
        if (m_ptr->seallight > 0)
        {
                m_ptr->seallight -= 1;
                return;
        }

	/* Handle "sleep" */
	if (m_ptr->csleep)
	{
		u32b notice = 0;
		
		/* Hack -- handle non-aggravation */
		if (!p_ptr->aggravate) notice = rand_int(1024);
		
		/* Hack -- See if monster "notices" player */
		if ((notice * notice * notice) <= noise)
		{
			/* Hack -- amount of "waking" */
			int d = 1;
			
			/* Wake up faster near the player */
			if (m_ptr->cdis < 50) d = (100 / m_ptr->cdis);
			
			/* Hack -- handle aggravation */
			if (p_ptr->aggravate) d = m_ptr->csleep;
			
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
			}
		}
	}
	
	/* No one wants to be your friend if you're aggravating */
        if ((is_pet(m_ptr)) && (p_ptr->aggravate) && !(r_ptr->flags7 & RF7_PET))
		gets_angry = TRUE;
	
	/* Paranoia... no friendly uniques outside wizard mode -- TY */
	if ((is_pet(m_ptr)) && !(wizard) &&
                (r_ptr->flags1 & (RF1_QUESTOR)) && !(r_ptr->flags7 & RF7_PET))
		gets_angry = TRUE;
	
        if ((gets_angry && m_ptr->friend == 0)
        || (gets_angry && r_ptr->flags1 & (RF1_QUESTOR)))
	{
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);
                msg_format("%^s suddenly becomes hostile!", m_name);
                set_pet(m_ptr, FALSE);
                m_ptr->angered_pet = TRUE;
	}
	
	/* Handle "fear" */
	if (m_ptr->monfear)
	{
		/* Amount of "boldness" */
                /* int d = randint(r_ptr->level / 10 + 1); */
                int d = 1;

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
	
	/* Get the origin */
	oy = m_ptr->fy;
	ox = m_ptr->fx;
	
	
	/* Attempt to "multiply" if able and allowed */
        if ((r_ptr->flags2 & (RF2_MULTIPLY)) && (num_repro < MAX_REPRO) && !(is_pet(m_ptr)))
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
		
		if (is_pet(m_ptr))
		{
			is_friend = TRUE;
		}
		else
		{
			is_friend = FALSE;
		}
		
		
		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(m_idx, (is_friend), FALSE))
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
	
	/* The CAN_SPEAK flag has changed. */
	/* Monster will first initiate the dialog with the player, then attack. */
	/* If the monster is defeated, dialog will pop-up again. */
	/*if (player_has_los_bold(oy, ox) && (r_ptr->flags2 & RF2_CAN_SPEAK))
	{
		if (m_ptr->spoke == 0)
		{
			show_dialog(r_ptr->extra2);
			m_ptr->spoke = 1;
		}
	}*/
	
	
	/* Attempt to cast a spell */
	if (make_attack_spell(m_idx)) return;
	
	/*
	 * Attempt to cast a spell at an enemy other than the player
	 * (may slow the game a smidgeon, but I haven't noticed.)
	 */
	if (monst_spell_monst(m_idx)) return;

	/* No spells casted? Try shooting! */
	if (make_ranged_attack(m_idx)) return;

	/* Shoot a monster! */
	if (make_ranged_attack_monst(m_idx)) return;

	/* Hack -- Assume no movement */
	mm[0] = mm[1] = mm[2] = mm[3] = 0;
	mm[4] = mm[5] = mm[6] = mm[7] = 0;

        c_ptr = &cave[m_ptr->fy][m_ptr->fx];

	/* Confused -- 100% random */
        if ((m_ptr->confused || (inv==TRUE)))
	{
		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}
	/* TOWNSFOLK always moves randomly... if friendly. */
	else if (is_pet(m_ptr) && r_ptr->flags7 & (RF7_TOWNSFOLK))
	{
		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

        /* The Dark Mist */
        else if (c_ptr->feat == FEAT_DARK_MIST && randint(100) <= (25 + p_ptr->abilities[(CLASS_SHADOW * 10) + 6]) && m_ptr->boss < 1 && !(r_ptr->flags1 & RF1_UNIQUE))
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

	/* Pets will follow the player */
	else if (is_pet(m_ptr) &&
                 (m_ptr->cdis > p_ptr->pet_follow_distance))
	{
                get_moves(m_idx, mm); 

	}

	/* pet movement */
        else if (is_pet(m_ptr))
	{
                /* by default, move randomly */
                mm[0] = mm[1] = mm[2] = mm[3] = 5; 

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
	did_kill_body = FALSE;
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


		/* Hack -- check for Glyph of Warding */
                if ((c_ptr->feat == FEAT_GLYPH) &&
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
                
                /* Hack -- trees are obstacle */
                else if ((cave[ny][nx].feat == FEAT_TREES) && (r_ptr->flags9 & RF9_KILL_TREES))
		{
                                do_move = TRUE;

                                /* Forget the tree */
                                c_ptr->info &= ~(CAVE_MARK);

                                /* Notice */
                                cave_set_feat(ny, nx, FEAT_GRASS);
		}

                /* NEWANGBAND: Vines!! */
                else if (cave[ny][nx].feat == FEAT_VINE_FIELD)
		{
                        if (r_ptr->d_char == 'P' && r_ptr->d_char == 'D' && r_ptr->d_char == 'm')
                        do_move = TRUE;
                        else
                        {
                                int randchance = 0;
                                if ((r_ptr->flags1 & (RF1_UNIQUE)) || m_ptr->boss >= 1) randchance = 40;
                                else randchance = 75;
                                if (randint(100) > randchance) do_move = TRUE;
                        }
		}


                /* NEWANGBAND: FIELDS!!! */
                /* Are every monsters stupid enough to cross them or what? */
                else if ((cave[ny][nx].feat == FEAT_FIRE_FIELD) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
                                do_move = TRUE;

                                /* Notice there is a field... */
                                field_dam = TRUE;

                                /* Check the field type... */
                                fieldtype = GF_FIRE;

                                /* How much damages will the monster take? */
                                field_dam_amount = c_ptr->field_damage;

				/* Fields have 50% chance of disappearing. */
				if (randint(100) >= 50)
				{
					cave[ny][nx].feat = FEAT_FLOOR;
					update_and_handle();
				}
		}
                else if ((cave[ny][nx].feat == FEAT_COLD_FIELD) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
                                do_move = TRUE;

                                /* Notice there is a field... */
                                field_dam = TRUE;

                                /* Check the field type... */
                                fieldtype = GF_COLD;

                                /* How much damages will the monster take? */
                                field_dam_amount = c_ptr->field_damage;

				/* Fields have 50% chance of disappearing. */
				if (randint(100) >= 50)
				{
					cave[ny][nx].feat = FEAT_FLOOR;
					update_and_handle();
				}
		}
                else if ((cave[ny][nx].feat == FEAT_ELEC_FIELD) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
                                do_move = TRUE;

                                /* Notice there is a field... */
                                field_dam = TRUE;

                                /* Check the field type... */
                                fieldtype = GF_ELEC;

                                /* How much damages will the monster take? */
                                field_dam_amount = c_ptr->field_damage;

				/* Fields have 50% chance of disappearing. */
				if (randint(100) >= 50)
				{
					cave[ny][nx].feat = FEAT_FLOOR;
					update_and_handle();
				}
		}
                else if ((cave[ny][nx].feat == FEAT_SPIKE_TRAP) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
                                do_move = TRUE;

                                /* Notice there is a field... */
                                field_dam = TRUE;

                                /* Check the field type... */
                                fieldtype = GF_PHYSICAL;

                                /* How much damages will the monster take? */
                                field_dam_amount = c_ptr->field_damage;

                                /* The trap triggered, it disappear. */
                                cave[ny][nx].feat = FEAT_FLOOR;

                                update_and_handle();
		}
                else if ((cave[ny][nx].feat == FEAT_GAS_TRAP) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
                                do_move = TRUE;

                                /* Notice there is a field... */
                                field_dam = TRUE;

                                /* Check the field type... */
                                fieldtype = GF_SLEEP_GAS;

                                /* How much damages will the monster take? */
                                field_dam_amount = c_ptr->field_damage;

                                /* The trap triggered, it disappear. */
                                cave[ny][nx].feat = FEAT_FLOOR;

                                update_and_handle();
		}
                else if ((cave[ny][nx].feat == FEAT_POISON_TRAP) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
                                do_move = TRUE;

                                /* Notice there is a field... */
                                field_dam = TRUE;

                                /* Check the field type... */
                                fieldtype = GF_POIS;

                                /* How much damages will the monster take? */
                                field_dam_amount = c_ptr->field_damage;

                                /* The trap triggered, it disappear. */
                                cave[ny][nx].feat = FEAT_FLOOR;

                                update_and_handle();
		}

                else if ((cave[ny][nx].feat == FEAT_WEBS) && cave[ny][nx].owner != 1)
		{
                                /* The monster is stupid, and will walk trough! */
                                do_move = TRUE;

                                /* Slow it down...maybe. */
                                /* Unless it's a spider...or Variaz! */
                                if (m_ptr->mspeed > 0 && r_ptr->d_char != 'S')
				{
					if (randint((p_ptr->stat_ind[A_INT] + p_ptr->stat_ind[A_WIS] + (p_ptr->skill[25] * 3) + p_ptr->skill[1])) >= randint((m_ptr->level + m_ptr->dex + m_ptr->str)))
					{
                                		m_ptr->mspeed -= 2;
						/* Webs will vanish! */
                                		cave[ny][nx].feat = FEAT_FLOOR;
					}
				}
                                if (m_ptr->mspeed <= 0) m_ptr->mspeed = 0;

                                /* Update */
                                update_and_handle();
		}
                else if ((cave[ny][nx].feat == FEAT_THORNED_VINES) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
                                do_move = TRUE;

                                /* Notice there is a field... */
                                field_dam = TRUE;

                                /* Check the field type... */
                                fieldtype = GF_EARTH;

                                /* How much damages will the monster take? */
                                field_dam_amount = c_ptr->field_damage;

				/* Fields have 50% chance of disappearing. */
				if (randint(100) >= 50)
				{
					cave[ny][nx].feat = FEAT_FLOOR;
					update_and_handle();
				}
		}
                else if ((cave[ny][nx].feat == FEAT_STORMS) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
                                do_move = TRUE;

                                /* Notice there is a field... */
                                field_dam = TRUE;

                                /* Check the field type... */
                                fieldtype = GF_WIND;

                                /* How much damages will the monster take? */
                                field_dam_amount = c_ptr->field_damage;

				/* Fields have 50% chance of disappearing. */
				if (randint(100) >= 50)
				{
					cave[ny][nx].feat = FEAT_FLOOR;
					update_and_handle();
				}
		}
                else if ((cave[ny][nx].feat == FEAT_DARK_MIST) && cave[ny][nx].owner != 1)
		{
                                /* Cross the field... */
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
		
		/* Monster moves through walls (and doors) */
                else if ((f_info[c_ptr->feat].flags1 & FF1_CAN_PASS) && (r_ptr->flags2 & (RF2_PASS_WALL)))
		{
			/* Pass through walls/doors/rubble */
			do_move = TRUE;
			
			/* Monster went through a wall */
			did_pass_wall = TRUE;
		}
		
		/* Handle doors and secret doors */
		else if (((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		          (c_ptr->feat <= FEAT_DOOR_TAIL)) || ((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) &&
		          (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)) ||
		          (c_ptr->feat == FEAT_SECRET) || (c_ptr->feat == FEAT_ICE_SECRET))
		{
			bool may_bash = TRUE;

			/* Take a turn */
			do_turn = TRUE;
			if (c_ptr->event == 3)
			{
				may_bash = FALSE;
			}
			else
			{
                        if ((r_ptr->flags2 & (RF2_OPEN_DOOR)) &&
                            (!is_pet(m_ptr) || p_ptr->pet_open_doors))
				{
					/* Closed doors and secret doors */
					if ((c_ptr->feat == FEAT_DOOR_HEAD) || (c_ptr->feat == FEAT_ICE_DOOR_HEAD) ||
						(c_ptr->feat == FEAT_SECRET) || (c_ptr->feat == FEAT_ICE_SECRET))
					{
						/* The door is open */
						did_open_door = TRUE;
						
						/* Do not bash the door */
						may_bash = FALSE;
					}
					
					/* Locked doors (not jammed) */
					else if ((((c_ptr->feat >= FEAT_DOOR_HEAD) && (c_ptr->feat <= FEAT_DOOR_TAIL)) && (c_ptr->feat < FEAT_DOOR_HEAD + 0x08)) || (((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) && (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)) && (c_ptr->feat < FEAT_ICE_DOOR_HEAD + 0x08)))
					{
						int k;
						
						/* Door power */
						if (((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) && (c_ptr->feat <= FEAT_ICE_DOOR_TAIL))) k = ((c_ptr->feat - FEAT_ICE_DOOR_HEAD) & 0x07);
						else k = ((c_ptr->feat - FEAT_DOOR_HEAD) & 0x07);
						
#if 0
						/* XXX XXX XXX Old test (pval 10 to 20) */
						if (randint((m_ptr->hp + 1) * (50 + o_ptr->pval)) <
							40 * (m_ptr->hp - 10 - o_ptr->pval));
#endif
						
						/* Try to unlock it XXX XXX XXX */
						if (rand_int(m_ptr->hp / 10) > k)
						{
							/* Unlock the door */
							if ((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) && (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)) cave_set_feat(ny, nx, FEAT_ICE_DOOR_HEAD + 0x00);
							else cave_set_feat(ny, nx, FEAT_DOOR_HEAD + 0x00);
							
							/* Do not bash the door */
							may_bash = FALSE;
						}
					}
				}
				}
				
				/* Stuck doors -- attempt to bash them down if allowed */
                                if (may_bash && (r_ptr->flags2 & RF2_BASH_DOOR) &&
                                   (!is_pet(m_ptr) || p_ptr->pet_open_doors))
				{
					int k;
					
					/* Door power */
					if (((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) && (c_ptr->feat <= FEAT_ICE_DOOR_TAIL))) k = ((c_ptr->feat - FEAT_ICE_DOOR_HEAD) & 0x07);
					else k = ((c_ptr->feat - FEAT_DOOR_HEAD) & 0x07);
					
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
						if ((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) && (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)) cave_set_feat(ny, nx, FEAT_ICE_BROKEN);
						else cave_set_feat(ny, nx, FEAT_BROKEN);
					}
					
					/* Open the door */
					else
					{
						if ((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) && (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)) cave_set_feat(ny, nx, FEAT_ICE_OPEN);
						else cave_set_feat(ny, nx, FEAT_OPEN);
					}
					
					/* Handle viewable doors */
					if (player_has_los_bold(ny, nx)) do_view = TRUE;
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

                /* Hack -- the Between teleport the monsters too */
                else if (cave[ny][nx].feat == FEAT_BETWEEN)
		{
                        nx = cave[ny][nx].special & 255;
                        ny = cave[ny][nx].special >> 8;
                        get_pos_player(10, &ny, &nx);

                        /* Access that cave grid */
                        c_ptr = &cave[ny][nx];		
		
                        /* Access that cave grid's contents */
                        y_ptr = &m_list[c_ptr->m_idx];

                        if(!(r_ptr->flags3 & RF3_IM_COLD))
                        {
                                if((m_ptr->hp - distance(ny,nx,oy,ox)*2)<=0)
                                {
                                        ny = oy + ddy[d];
                                        nx = ox + ddx[d];
                                        do_move = FALSE;
                                }
                                else
                                {
                                       m_ptr->hp -= distance(ny,nx,oy,ox)*2;
                                       do_move = TRUE;
                                }
                        }
                        else
                        {
                                do_move = TRUE;
                        }
		}

		/* Some monsters never attack */
		if (do_move && (ny == py) && (nx == px) &&
			(r_ptr->flags1 & RF1_NEVER_BLOW))
		{
#if 0
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_NEVER_BLOW);
#endif

			/* Do not move */
			do_move = FALSE;
		}

		/* The player is in the way.  Attack him. */
		if (do_move && (ny == py) && (nx == px))
		{
			/* Do the attack */
                        (void)make_attack_normal(m_idx, 1);
			
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
			if ((r_ptr->flags2 & RF2_KILL_BODY) &&
				(r_ptr->mexp > z_ptr->mexp) && (cave_floor_bold(ny,nx)) &&
				!(is_pet(m_ptr) && is_pet(m2_ptr)))
				/* Friends don't kill friends... */
			{
				/* Allow movement */
				do_move = TRUE;
				
				/* Monster ate another monster */
				did_kill_body = TRUE;
				
				/* XXX XXX XXX Message */
				
				/* Kill the monster */
				delete_monster(ny, nx);
				
				/* Hack -- get the empty monster */
				y_ptr = &m_list[c_ptr->m_idx];
			}
			
			/* Attack 'enemies' */
			else if ((is_pet(m_ptr) != is_pet(m2_ptr)) || m_ptr->confused)
			{
				do_move = FALSE;
				/* attack */
				if (m2_ptr->r_idx && (m2_ptr->hp >= 0))
				{
					if (monst_attack_monst(m_idx, c_ptr->m_idx))
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

		/* Some monsters never move */
		if (do_move && (r_ptr->flags7 & RF7_NEVER_MOVE_FRIENDLY) && is_pet(m_ptr))
		{
			/* Hack -- memorize lack of attacks */
			/* if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_NEVER_MOVE); */
			
			/* Do not move */
			do_move = FALSE;
		}

		/* Some monsters never move */
		if (do_move && (r_ptr->flags1 & RF1_NEVER_MOVE))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_NEVER_MOVE); 
			
			/* Do not move */
			do_move = FALSE;
		}

                /* No legs, no moves! */
                if (m_ptr->abilities & (MUTILATE_LEGS)) do_move = FALSE;
		
		
		/* Creature has been allowed move */
		if (do_move)
		{
			s16b this_o_idx, next_o_idx = 0;

			/* Call a lua script. */
			if (r_ptr->event_before_move > 0)
			{
				call_lua("monster_before_move", "(dd)", "", m_idx, r_ptr->event_before_move);
			}
			
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

                        /* Was there a field here? */
                        if (field_dam)
                        {
                                /* There is a field? Hurt the monster then! */
                                no_magic_return = TRUE;
                                nevermiss = TRUE;
                                if (fieldtype == GF_SLEEP_GAS) corpse_explode(field_dam_amount, m_ptr->fx, m_ptr->fy, 3 + (p_ptr->abilities[(CLASS_ROGUE * 10) + 6] / 20), fieldtype);
                                else corpse_explode(field_dam_amount, m_ptr->fx, m_ptr->fy, 0, fieldtype);
                                no_magic_return = FALSE;
                                nevermiss = FALSE;
                        }

			/* Possible disturb */
			if (m_ptr->ml && (disturb_move ||
				((m_ptr->mflag & (MFLAG_VIEW)) &&
				disturb_near)))
			{
				/* Disturb */
				if (!is_pet(m_ptr) || disturb_pets)
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
                                if ((((r_ptr->flags2 & (RF2_TAKE_ITEM)) &&
				      (!is_pet(m_ptr) || p_ptr->pet_pickup_items)) ||
					(r_ptr->flags2 & (RF2_KILL_ITEM))) &&
					!is_pet(m_ptr))
				{
                                        u32b f1, f2, f3, f4;
					
					u32b flg3 = 0L;
					
					char m_name[80];
					char o_name[80];
					
					/* Extract some flags */
                                        object_flags(o_ptr, &f1, &f2, &f3, &f4);
					
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
					if (artifact_p(o_ptr) || (r_ptr->flags3 & flg3) ||
						(o_ptr->art_name))
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

			/* Call a lua script. */
			if (r_ptr->event_after_move > 0)
			{
				call_lua("monster_after_move", "(dd)", "", m_idx, r_ptr->event_after_move);
			}
		}
		
		/* Stop when done */
		if (do_turn) break;
	}
	
	
	/* If we haven't done anything, try casting a spell again */
	if (!do_turn && !do_move && !m_ptr->monfear &&
		!is_pet(m_ptr))
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
		
		/* Monster ate another monster */
		if (did_kill_body) r_ptr->r_flags2 |= (RF2_KILL_BODY);
		
		/* Monster passed through a wall */
		if (did_pass_wall) r_ptr->r_flags2 |= (RF2_PASS_WALL);
		
		/* Monster destroyed a wall */
		if (did_kill_wall) r_ptr->r_flags2 |= (RF2_KILL_WALL);

		/* The CAN_SPEAK flag has changed. */
		/* Monster will first initiate the dialog with the player, then attack. */
		/* If the monster is defeated, dialog will pop-up again. */
		if (player_has_los_bold(oy, ox) && (r_ptr->flags2 & RF2_CAN_SPEAK))
		{
			if (m_ptr->spoke == 0)
			{
				int tmpx, tmpy;
				tmpy = m_ptr->fy;
				tmpx = m_ptr->fx;
				show_dialog(r_ptr->extra2);
				m_ptr->spoke = 1;
				lite_spot(tmpy, tmpx);
				update_and_handle();
			}
		}
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
	bool            is_friend = FALSE;
	
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
        noise = (1L << (30 - (p_ptr->skill[6] / 3)));


	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Handle "leaving" */
		if (p_ptr->leaving) break;
		
		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;
		
		/* Calculate "upkeep" for friendly monsters */
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
			p_ptr->aggravate))
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
		
		if (is_pet(m_ptr)) is_friend = TRUE;
		
		/* Process the monster */
		process_monster(i, is_friend);
		
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

bool counterspell(monster_type *m_ptr)
{
        char ch;
        if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 4] >= 1)
        {

        	get_com("Attempt to counter spell? [y/n]", &ch);
        	if (ch == 'y' || ch == 'Y')
        	{
                	if (randint(p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 4] * 20) >= randint(m_ptr->level + m_ptr->mind))
                	{
                        	msg_print("You successfully countered the spell!");
                        	return (TRUE);
                	}
                	else
                	{
                        	msg_print("You failed to counter the spell.");
                        	return (FALSE);
                	}
        	}
        
        }
        return (FALSE);
}

/* Monster will cast a spell! */
void monster_cast_spell(int m_idx, monster_type *m_ptr, int spellnum)
{
	s32b dam, dambonus;
	int rad, i;
	monster_race	*r_ptr = &r_info[m_ptr->r_idx];
	char m_name[80];
	char kind;
	bool canlearn = TRUE;
	int mindstat;
		
	/* Extract monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Use monster's mind to determine damages. */
	mindstat = (m_ptr->mind - 5);
	if (mindstat < 0) mindstat = 0;

	/* Call a lua script. */
	if (r_ptr->event_before_magic > 0)
	{
		call_lua("monster_before_magic", "(dd)", "", m_idx, r_ptr->event_before_magic);
	}

	/* Let's cast the spell! :) */
	if (r_ptr->spell[spellnum].type != 999) msg_format("%^s %s %s!", m_name, r_ptr->spell[spellnum].act, r_ptr->spell[spellnum].name);

	/* Counter Spell can even counter scripted attacks! */
	if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 4] >= 1 && r_ptr->spell[spellnum].type == 999) msg_format("%^s attempts a strange power...", m_name);
	if (counterspell(m_ptr)) return;

	/* Let's see what type of spell it was... */
	switch (r_ptr->spell[spellnum].type)
	{
		/* Type 1: Bolt */
		/* Special1 is the element. */
		case 1:
		{
			if (r_ptr->spell[spellnum].special3 == 1) dam = r_ptr->spell[spellnum].power;
			else
			{
				dam = (r_ptr->spell[spellnum].power * mindstat);
				dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
				dam = dam + dambonus;
				if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
			}
			if (r_ptr->spell[spellnum].special1 == GF_LOSE_STR ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_INT ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_WIS ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_DEX ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_CON ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_CHR ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_ALL ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_EXP) canlearn = FALSE;
			bolt(m_idx, r_ptr->spell[spellnum].special1, dam);
			break;
		}
		/* Type 2: Ball/Breath */
		/* Special1 is the element. */
		case 2:
		{
			if (r_ptr->spell[spellnum].special3 == 1) dam = r_ptr->spell[spellnum].power;
			else
			{
				dam = (r_ptr->spell[spellnum].power * mindstat);
				dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
				dam = dam + dambonus;
				if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
			}
			if (r_ptr->spell[spellnum].special1 == GF_LOSE_STR ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_INT ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_WIS ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_DEX ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_CON ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_CHR ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_ALL ||
			r_ptr->spell[spellnum].special1 == GF_LOSE_EXP) canlearn = FALSE;
			rad = r_ptr->spell[spellnum].special2;
			rad += m_ptr->skill_magic / 30;
			breath(m_idx, r_ptr->spell[spellnum].special1, dam, rad);
			break;
		}
		/* Type 3: Healing Spell */
		case 3:
		{
			dam = (r_ptr->spell[spellnum].power * mindstat);
			dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
			dam = dam + dambonus;
			m_ptr->hp += dam;
			if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
			msg_format("%^s looks healtier.", m_name);
			break;
		}
		/* Type 4: Haste */
		case 4:
		{
			m_ptr->mspeed += r_ptr->spell[spellnum].power;
			if (m_ptr->mspeed > 180) m_ptr->mspeed = 180;
			m_ptr->hasted = 1;
			msg_format("%^s starts moving faster!", m_name);
			break;
		}
		/* Type 5: Boost */
		/* Special1 is the type of boost */
		/* 1. Strength
		 * 2. Dexterity
		 * 3. Mind
		 * 4. Attack skill
		 * 5. Magic skill
		 * 6. All stats
		 * 7. All skills
		 * 8. All stats and skills
		*/
		case 5:
		{
			if (r_ptr->spell[spellnum].special1 == 1) m_ptr->str += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 2) m_ptr->dex += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 3) m_ptr->mind += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 4) m_ptr->skill_attack += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 5) m_ptr->skill_magic += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 6) 
			{
				m_ptr->str += r_ptr->spell[spellnum].power;
				m_ptr->dex += r_ptr->spell[spellnum].power;
				m_ptr->mind += r_ptr->spell[spellnum].power;
			}
			if (r_ptr->spell[spellnum].special1 == 7) 
			{
				m_ptr->skill_attack += r_ptr->spell[spellnum].power;
				m_ptr->skill_ranged += r_ptr->spell[spellnum].power;
				m_ptr->skill_magic += r_ptr->spell[spellnum].power;
				canlearn = FALSE;
			}
			if (r_ptr->spell[spellnum].special1 == 8) 
			{
				m_ptr->str += r_ptr->spell[spellnum].power;
				m_ptr->dex += r_ptr->spell[spellnum].power;
				m_ptr->mind += r_ptr->spell[spellnum].power;
				m_ptr->skill_attack += r_ptr->spell[spellnum].power;
				m_ptr->skill_ranged += r_ptr->spell[spellnum].power;
				m_ptr->skill_magic += r_ptr->spell[spellnum].power;
			}
			if (r_ptr->spell[spellnum].special1 == 9) m_ptr->skill_ranged += r_ptr->spell[spellnum].power;
			m_ptr->boosted = 1;
			break;
		}
		/* Type 6: Summon Kind*/
		case 6:
		{
			for (i = 0; i < r_ptr->spell[spellnum].special1; i++)
			{
				if (is_pet(m_ptr)) summon_specific_kind(py, px, r_ptr->spell[spellnum].power, r_ptr->spell[spellnum].summchar, FALSE, TRUE, r_ptr->spell[spellnum].special2);
				else summon_specific_kind(py, px, r_ptr->spell[spellnum].power, r_ptr->spell[spellnum].summchar, FALSE, FALSE, r_ptr->spell[spellnum].special2);
			}
			break;
		}
		/* Type 7: Summon Specific*/
		case 7:
		{
			for (i = 0; i < r_ptr->spell[spellnum].special1; i++)
			{
				if (is_pet(m_ptr)) summon_specific_ridx(py, px, r_ptr->spell[spellnum].power, FALSE, TRUE, r_ptr->spell[spellnum].special2);
				else summon_specific_ridx(py, px, r_ptr->spell[spellnum].power, FALSE, FALSE, r_ptr->spell[spellnum].special2);
			}
			break;
		}
		/* Type 8: Phase door*/
		case 8:
		{
			disturb(1, 0);
			msg_format("%^s teleports!", m_name);
			teleport_away(m_idx, r_ptr->spell[spellnum].power);
			break;
		}
		case 999:
		{
			call_lua(r_ptr->spell[spellnum].name, "(d)", "", m_idx);
			canlearn = FALSE;
			break;
		}
	}
	/* The spell costs some mana. */
	if (r_ptr->spell[spellnum].cost > 0) m_ptr->mana -= r_ptr->spell[spellnum].cost;

	/* Spells with negative costs cannot be learned. */
	if (r_ptr->spell[spellnum].cost < 0) canlearn = FALSE;

	/* Call a lua script. */
	if (r_ptr->event_after_magic > 0)
	{
		call_lua("monster_after_magic", "(dd)", "", m_idx, r_ptr->event_after_magic);
	}

	/* Counter Shot ability. */
	if ((m_ptr->r_idx) && p_ptr->abilities[(CLASS_MARKSMAN * 10) + 3] > 0 && p_ptr->events[29045] == 1)
	{
		p_ptr->events[29046] = m_idx;
		call_lua("use_ability", "(d)", "", 120);
		p_ptr->events[29046] = 0;
	}
	
	/* So, we're alive and learning? */
	/* Let's learn the monster magic then! :) */
	if (p_ptr->learning && p_ptr->chp >= 0 && (canlearn))
	{
		int whichslot;
		char ch;
		monster_magics *mspell_ptr;
		get_com("Learn the spell? [y/n]", &ch);

                if (ch == 'y' || ch == 'Y')
                {
			if (randint((p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] * 3)) >= randint(r_ptr->spell[spellnum].cost))
			{
				msg_print("You successfully learned the spell!");
                        	whichslot = get_quantity("Put in which slot?", 15);
				whichslot -= 1;
				if (whichslot < 0) whichslot = 0;
				if (whichslot > 14) whichslot = 14;
        			mspell_ptr = &monster_magic[whichslot];
				/* Copy the spell to the slot! */
				strcpy(mspell_ptr->name, r_ptr->spell[spellnum].name);
				strcpy(mspell_ptr->act, r_ptr->spell[spellnum].act);
				mspell_ptr->type = r_ptr->spell[spellnum].type;
				mspell_ptr->power = r_ptr->spell[spellnum].power;
				mspell_ptr->special1 = r_ptr->spell[spellnum].special1;
				mspell_ptr->special2 = r_ptr->spell[spellnum].special2;
				mspell_ptr->special3 = r_ptr->spell[spellnum].special3;
				mspell_ptr->summchar = r_ptr->spell[spellnum].summchar;
				mspell_ptr->cost = r_ptr->spell[spellnum].cost;
			}
			else msg_print("You failed to learn the spell.");
                }
	}
}

/* Monster will cast a spell... at another monster! */
void monster_cast_spell_monst(int m_idx, monster_type *m_ptr, int y, int x, monster_type *t_ptr, int spellnum)
{
	s32b dam, dambonus;
	int rad, i;
	monster_race	*r_ptr = &r_info[m_ptr->r_idx];
	char m_name[80];
	char t_name[80];
	int mindstat;
			
	/* Extract monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Extract target name */
	monster_desc(t_name, t_ptr, 0);

	/* Use monster's mind to determine damages. */
	mindstat = (m_ptr->mind - 5);
	if (mindstat < 0) mindstat = 0;

	/* Call a lua script. */
	if (r_ptr->event_before_magic > 0)
	{
		call_lua("monster_before_magic", "(dd)", "", m_idx, r_ptr->event_before_magic);
	}

	/* Let's see what type of spell it was... */
	switch (r_ptr->spell[spellnum].type)
	{
		/* Type 1: Bolt */
		/* Special1 is the element. */
		case 1:
		{
			msg_format("%^s %s %s at %^s!", m_name, r_ptr->spell[spellnum].act, r_ptr->spell[spellnum].name, t_name);
			if (r_ptr->spell[spellnum].special3 == 1) dam = r_ptr->spell[spellnum].power;
			else
			{
				dam = (r_ptr->spell[spellnum].power * mindstat);
				dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
				dam = dam + dambonus;
				if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
				if (is_pet(m_ptr))
				{
					dam += ((dam * 10) * p_ptr->skill[9]) / 100;
					if (p_ptr->stat_ind[A_CHR] > 5) dam += ((dam * 3) * (p_ptr->stat_ind[A_CHR] - 5)) / 100;
				}
			}
			monst_bolt_monst(m_idx, y, x, r_ptr->spell[spellnum].special1, dam);
			break;
		}
		/* Type 2: Ball/Breath */
		/* Special1 is the element. */
		case 2:
		{
			msg_format("%^s %s %s at %^s!", m_name, r_ptr->spell[spellnum].act, r_ptr->spell[spellnum].name, t_name);
			if (r_ptr->spell[spellnum].special3 == 1) dam = r_ptr->spell[spellnum].power;
			else
			{
				dam = (r_ptr->spell[spellnum].power * mindstat);
				dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
				dam = dam + dambonus;
				if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
				if (is_pet(m_ptr))
				{
					dam += ((dam * 10) * p_ptr->skill[9]) / 100;
					if (p_ptr->stat_ind[A_CHR] > 5) dam += ((dam * 3) * (p_ptr->stat_ind[A_CHR] - 5)) / 100;
				}
			}
			rad = r_ptr->spell[spellnum].special2;
			rad += m_ptr->skill_magic / 30;
			monst_breath_monst(m_idx, y, x, r_ptr->spell[spellnum].special1, dam, rad);
			break;
		}
		/* Type 3: Healing Spell */
		case 3:
		{
			msg_format("%^s %s %s!", m_name, r_ptr->spell[spellnum].act, r_ptr->spell[spellnum].name);
			dam = (r_ptr->spell[spellnum].power * mindstat);
			dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
			dam = dam + dambonus;
			if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
			m_ptr->hp += dam;
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
			msg_format("%^s looks healtier.", m_name);
			break;
		}
		/* Type 4: Haste */
		case 4:
		{
			msg_format("%^s %s %s!", m_name, r_ptr->spell[spellnum].act, r_ptr->spell[spellnum].name);
			m_ptr->mspeed += r_ptr->spell[spellnum].power;
			if (m_ptr->mspeed > 180) m_ptr->mspeed = 180;
			m_ptr->hasted = 1;
			msg_format("%^s starts moving faster!", m_name);
			break;
		}
		/* Type 5: Boost */
		/* Special1 is the type of boost */
		/* 1. Strength
		 * 2. Dexterity
		 * 3. Mind
		 * 4. Attack skill
		 * 5. Magic skill
		 * 6. All stats
		 * 7. All skills
		 * 8. All stats and skills
		*/
		case 5:
		{
			msg_format("%^s %s %s!", m_name, r_ptr->spell[spellnum].act, r_ptr->spell[spellnum].name);
			if (r_ptr->spell[spellnum].special1 == 1) m_ptr->str += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 2) m_ptr->dex += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 3) m_ptr->mind += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 4) m_ptr->skill_attack += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 5) m_ptr->skill_magic += r_ptr->spell[spellnum].power;
			if (r_ptr->spell[spellnum].special1 == 6) 
			{
				m_ptr->str += r_ptr->spell[spellnum].power;
				m_ptr->dex += r_ptr->spell[spellnum].power;
				m_ptr->mind += r_ptr->spell[spellnum].power;
			}
			if (r_ptr->spell[spellnum].special1 == 7) 
			{
				m_ptr->skill_attack += r_ptr->spell[spellnum].power;
				m_ptr->skill_ranged += r_ptr->spell[spellnum].power;
				m_ptr->skill_magic += r_ptr->spell[spellnum].power;
			}
			if (r_ptr->spell[spellnum].special1 == 8) 
			{
				m_ptr->str += r_ptr->spell[spellnum].power;
				m_ptr->dex += r_ptr->spell[spellnum].power;
				m_ptr->mind += r_ptr->spell[spellnum].power;
				m_ptr->skill_attack += r_ptr->spell[spellnum].power;
				m_ptr->skill_ranged += r_ptr->spell[spellnum].power;
				m_ptr->skill_magic += r_ptr->spell[spellnum].power;
			}
			if (r_ptr->spell[spellnum].special1 == 9) m_ptr->skill_ranged += r_ptr->spell[spellnum].power;
			m_ptr->boosted = 1;
			break;
		}
		/* Type 6: Summon Kind*/
		case 6:
		{
			msg_format("%^s %s %s at %^s!", m_name, r_ptr->spell[spellnum].act, r_ptr->spell[spellnum].name, t_name);
			for (i = 0; i < r_ptr->spell[spellnum].special1; i++)
			{
				if (is_pet(m_ptr)) summon_specific_kind(t_ptr->fy, t_ptr->fx, r_ptr->spell[spellnum].power, r_ptr->spell[spellnum].summchar, FALSE, TRUE, r_ptr->spell[spellnum].special2);
				else summon_specific_kind(t_ptr->fy, t_ptr->fx, r_ptr->spell[spellnum].power, r_ptr->spell[spellnum].summchar, FALSE, FALSE, r_ptr->spell[spellnum].special2);
			}
			break;
		}
		/* Type 7: Summon Specific*/
		case 7:
		{
			msg_format("%^s %s %s at %^s!", m_name, r_ptr->spell[spellnum].act, r_ptr->spell[spellnum].name, t_name);
			for (i = 0; i < r_ptr->spell[spellnum].special1; i++)
			{
				if (is_pet(m_ptr)) summon_specific_ridx(t_ptr->fy, t_ptr->fx, r_ptr->spell[spellnum].power, FALSE, TRUE, r_ptr->spell[spellnum].special2);
				else summon_specific_ridx(t_ptr->fy, t_ptr->fx, r_ptr->spell[spellnum].power, FALSE, FALSE, r_ptr->spell[spellnum].special2);
			}
			break;
		}
		/* Type 8: Phase door*/
		case 8:
		{
			disturb(1, 0);
			msg_format("%^s teleports!", m_name);
			teleport_away(m_idx, r_ptr->spell[spellnum].power);
			break;
		}
		case 999:
		{
			call_lua(r_ptr->spell[spellnum].name, "(d)", "", m_idx);
			break;
		}
	}
	
	/* The spell costs some mana. */
	m_ptr->mana -= r_ptr->spell[spellnum].cost;

	/* Call a lua script. */
	if (r_ptr->event_after_magic > 0)
	{
		call_lua("monster_after_magic", "(dd)", "", m_idx, r_ptr->event_after_magic);
	}
}

/* Similar to make_attack_spell, but for ranged attacks! */
bool make_ranged_attack(int m_idx)
{
	int             k, chance, thrown_spell, rlev, failrate, i, j;
	int		numspells, chosenattack;
	byte            spell[96], num = 0;
        u32b            f4, f5, f6;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	object_type	*o_ptr;
	char            m_name[80];
	char            m_poss[80];
	char            ddesc[80];
	int		ranged[20]; /* Attacks that are ranged. */
	bool            no_inate = FALSE;
	bool		found_ranged = FALSE;
	bool 		visible = FALSE;
	bool 		obvious = FALSE;
	bool 		nothurt = FALSE;
	u32b f1, f2, f3;
	s32b damage;
	int flg;

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

	o_ptr = &inventory[INVEN_WIELD];

        object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Cannot shoot when confused */
	if (m_ptr->confused) return (FALSE);

	/* Cannot shoot when friendly */
	if (is_pet(m_ptr)) return (FALSE);	 
	 
	/* Hack -- require projectable player */
	if (normal)
	{
		/* Check range */
		if (m_ptr->cdis > MAX_RANGE) return (FALSE);

		/* Check path */
		if (!projectable(m_ptr->fy, m_ptr->fx, py, px)) return (FALSE);

		if (!clean_shot(m_ptr->fy, m_ptr->fx, py, px)) return (FALSE);

		/* Player is invisible? */
		if (player_invis(m_ptr)) return (FALSE);
	}

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

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

	/* Reset ranged count. */
	for (j = 0; j < 20; j++) ranged[j] = 0;

	/* Scan the various attacks. Try to find some type "3" attacks. */
	i = 1;
	for (j = 0; j < 20; j++)
	{
		if (r_ptr->attack[j].type == 3 || r_ptr->attack[j].type == 1000)
		{ 
			ranged[(i-1)] = j;
			i++;
			found_ranged = TRUE;
		}
	}
	
	/* If no ranged attacks were found, return. */
	if (!(found_ranged)) return (FALSE);

	/* Pick a random ranged attack! */
	chosenattack = (randint(i) - 1);

	if (r_ptr->attack[chosenattack].type == 1000)
	{
		/* Call a lua script. */
		if (r_ptr->event_before_ranged > 0)
		{
			call_lua("monster_before_ranged", "(dd)", "", m_idx, r_ptr->event_before_ranged);
		}

		call_lua(r_ptr->attack[chosenattack].name, "(d)", "", m_idx);

		/* Call a lua script. */
		if (r_ptr->event_after_ranged > 0)
		{
			call_lua("monster_after_ranged", "(dd)", "", m_idx, r_ptr->event_after_ranged);
		}

		/* Counter Shot ability. */
		if ((m_ptr->r_idx) && p_ptr->abilities[(CLASS_MARKSMAN * 10) + 3] > 0 && p_ptr->events[29045] == 1)
		{
			p_ptr->events[29046] = m_idx;
			call_lua("use_ability", "(d)", "", 120);
			p_ptr->events[29046] = 0;
		}
		return;
	}

	/* Now... shoot the poor player! */
	/* Special1 is the radius... */
	if (r_ptr->attack[chosenattack].special1 == 0)
	{
		/* We don't ALWAYS fire... */
		if (randint(100) <= r_ptr->attack[chosenattack].special2)
		{
			/* Call a lua script. */
			if (r_ptr->event_before_ranged > 0)
			{
				call_lua("monster_before_ranged", "(dd)", "", m_idx, r_ptr->event_before_ranged);
			}

			if (monster_hit_player(m_ptr, 0))
			{
				damage = damroll(r_ptr->attack[chosenattack].ddice, r_ptr->attack[chosenattack].dside);
				damage *= (m_ptr->skill_ranged + 1);
				damage += multiply_divide(damage, ((m_ptr->dex - 5) * 5), 100);
				damage += multiply_divide(damage, m_ptr->dex, 100);

				/* Bosses may get higher damages! */
				if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) damage *= 2;
                        	if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) damage -= damage / 2;
                        	else if (m_ptr->abilities & (CURSE_LOWER_POWER)) damage -= damage / 4;

				if (strstr(r_ptr->attack[chosenattack].name, "!")) msg_format("%^s %s you!", m_name, r_ptr->attack[chosenattack].act);
				else msg_format("%^s %s %s!", m_name, r_ptr->attack[chosenattack].act, r_ptr->attack[chosenattack].name);

                        	/* Resistances...and it apply AFTER the Damages Curse! :) */
				if (r_ptr->attack[chosenattack].element == GF_MISSILE)
				{
					if (p_ptr->pres_dur > 0)
                        		{
                                		s32b damagepercent;
						damagepercent = multiply_divide(damage, p_ptr->pres, 100);
                                		damage -= damagepercent; 
                        		}
					if (p_ptr->mres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = multiply_divide(damage, p_ptr->mres, 100);
                                		damage -= damagepercent; 
                        		}
				}
				else if (r_ptr->attack[chosenattack].element != GF_PHYSICAL)
				{
                        		if (p_ptr->mres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = multiply_divide(damage, p_ptr->mres, 100);
                                		damage -= damagepercent; 
                        		}
				}
				else
				{
					if (p_ptr->pres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = multiply_divide(damage, p_ptr->pres, 100);
                                		damage -= damagepercent; 
                        		}
				}

                        	/* Attempt to block the attack. */
                        	{
                                	int blockchance = 0;
					int x;

					/* Try to block with each of your weapons. */
					for (x = 0; x < 2; x++)
					{
						o_ptr = &inventory[INVEN_WIELD + x];
						blockchance = 0;

                                		if (o_ptr->tval != 0)
                                		{
                                        		/* Basic block chance is equal to item's base AC */
                                        		blockchance = o_ptr->ac;

							/* Swords skill allows you to parry with a sword. */
							if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 12 && p_ptr->skill[12] >= 10) blockchance += 10;

							/* Polearm skill allows you to parry with a spear. */
							if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 14 && p_ptr->skill[14] >= 25) blockchance += 20;

							/* If AC is 0, cannot block with this item. */
							if (blockchance > 0)
							{
                                        			/* Then, it's increased by dexterity and item's to_a */
                                        			blockchance += (p_ptr->stat_ind[A_DEX] + o_ptr->to_a);

								/* But is reduced by monster's dex. */
								blockchance -= m_ptr->dex;

                                        			/* Defender is the master of shields! */
                                        			if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 3] >= 1) blockchance += (p_ptr->abilities[(CLASS_DEFENDER * 10) + 3] * 5);
							}
                                		}
                                		else if (unarmed() && p_ptr->skill[18] >= 40 && x == 0 && !heavy_armor())
						{
							/* If unarmed, and not wearing heavy armor, you may get a block chance. */
							/* Gloves are used as the "shield". */
							object_type *g_ptr;
							g_ptr = &inventory[INVEN_HANDS];
							blockchance += 10 + p_ptr->stat_ind[A_DEX];
							if (g_ptr)
							{
								blockchance += g_ptr->ac;
								blockchance += g_ptr->to_a;
							}
						}
						
                                		/* Maximum blocking chance is 75% */
                                		if (blockchance > 75) blockchance = 75;

                                		/* Now, try to block */
                                		if (randint(100) < blockchance)
						{
							msg_print("You block!");
							nothurt = TRUE;
						}
					}
                        	}

				if (!nothurt)
				{
					flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL;
                			no_magic_return = TRUE;
					monster_ranged = TRUE;
                			arrow(m_idx, r_ptr->attack[chosenattack].element, damage);
					monster_ranged = FALSE;
                			no_magic_return = FALSE;
				}
			}
			else msg_format("%^s %s at you, but miss.", m_name, r_ptr->attack[chosenattack].act);

			/* Call a lua script. */
			if (r_ptr->event_after_ranged > 0)
			{
				call_lua("monster_after_ranged", "(dd)", "", m_idx, r_ptr->event_after_ranged);
			}

			/* Counter Shot ability. */
			if ((m_ptr->r_idx) && p_ptr->abilities[(CLASS_MARKSMAN * 10) + 3] > 0 && p_ptr->events[29045] == 1)
			{
				p_ptr->events[29046] = m_idx;
				call_lua("use_ability", "(d)", "", 120);
				p_ptr->events[29046] = 0;
			}
		}
		else return (FALSE);
	}
	else
	{
		/* We don't ALWAYS fire... */
		if (randint(100) <= r_ptr->attack[chosenattack].special2)
		{
			/* Call a lua script. */
			if (r_ptr->event_before_ranged > 0)
			{
				call_lua("monster_before_ranged", "(dd)", "", m_idx, r_ptr->event_before_ranged);
			}

			if (monster_hit_player(m_ptr, 0))
			{
				damage = damroll(r_ptr->attack[chosenattack].ddice, r_ptr->attack[chosenattack].dside);
				damage *= (m_ptr->skill_ranged + 1);
				damage += multiply_divide(damage, ((m_ptr->dex - 5) * 5), 100);
				damage += multiply_divide(damage, m_ptr->dex, 100);
				/* Bosses may get higher damages! */
				if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) damage *= 2;
                        	if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) damage -= damage / 2;
                        	else if (m_ptr->abilities & (CURSE_LOWER_POWER)) damage -= damage / 4;

				if (strstr(r_ptr->attack[chosenattack].name, "!")) msg_format("%^s %s you!", m_name, r_ptr->attack[chosenattack].act);
				else msg_format("%^s %s %s!", m_name, r_ptr->attack[chosenattack].act, r_ptr->attack[chosenattack].name);

                        	/* Resistances...and it apply AFTER the Damages Curse! :) */
				if (r_ptr->attack[chosenattack].element == GF_MISSILE)
				{
					if (p_ptr->pres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = multiply_divide(damage, p_ptr->pres, 100);
                                		damage -= damagepercent; 
                        		}
					if (p_ptr->mres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = multiply_divide(damage, p_ptr->mres, 100);
                                		damage -= damagepercent; 
                        		}
				}
				else if (r_ptr->attack[chosenattack].element != GF_PHYSICAL)
				{
                        		if (p_ptr->mres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = multiply_divide(damage, p_ptr->mres, 100);
                                		damage -= damagepercent; 
                        		}
				}
				else
				{
					if (p_ptr->pres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = multiply_divide(damage, p_ptr->pres, 100);
                                		damage -= damagepercent; 
                        		}
				}

                        	/* Attempt to block the attack. */
                        	{
                                	int blockchance = 0;
					int x;

					/* Try to block with each of your weapons. */
					for (x = 0; x < 2; x++)
					{
						o_ptr = &inventory[INVEN_WIELD + x];
						blockchance = 0;

                                		if (o_ptr->tval != 0)
                                		{
                                        		/* Basic block chance is equal to item's base AC */
                                        		blockchance = o_ptr->ac;

							/* Swords skill allows you to parry with a sword. */
							if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 12 && p_ptr->skill[12] >= 10) blockchance += 10;

							/* Polearm skill allows you to parry with a spear. */
							if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 14 && p_ptr->skill[14] >= 25) blockchance += 20;

							/* If AC is 0, cannot block with this item. */
							if (blockchance > 0)
							{
                                        			/* Then, it's increased by dexterity and item's to_a */
                                        			blockchance += (p_ptr->stat_ind[A_DEX] + o_ptr->to_a);

								/* But is reduced by monster's dex. */
								blockchance -= m_ptr->dex;

                                        			/* Defender is the master of shields! */
                                        			if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 3] >= 1) blockchance += (p_ptr->abilities[(CLASS_DEFENDER * 10) + 3] * 5);
							}
                                		}
                                		else if (unarmed() && p_ptr->skill[18] >= 40 && x == 0 && !heavy_armor())
						{
							/* If unarmed, and not wearing heavy armor, you may get a block chance. */
							/* Gloves are used as the "shield". */
							object_type *g_ptr;
							g_ptr = &inventory[INVEN_HANDS];
							blockchance += 10 + p_ptr->stat_ind[A_DEX];
							if (g_ptr)
							{
								blockchance += g_ptr->ac;
								blockchance += g_ptr->to_a;
							}
						}
						
                                		/* Maximum blocking chance is 75% */
                                		if (blockchance > 75) blockchance = 75;

                                		/* Now, try to block */
                                		if (randint(100) < blockchance)
						{
							msg_print("You block!");
							nothurt = TRUE;
						}
					}
                        	}

				if (!nothurt)
				{
					flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL;
                			no_magic_return = TRUE;
					monster_ranged = TRUE;
                			canon(m_idx, r_ptr->attack[chosenattack].element, damage, r_ptr->attack[chosenattack].special1);
					monster_ranged = FALSE;
                			no_magic_return = FALSE;
				}
			}
			else msg_format("%^s %s at you, but miss.", m_name, r_ptr->attack[chosenattack].act);

			/* Call a lua script. */
			if (r_ptr->event_after_ranged > 0)
			{
				call_lua("monster_after_ranged", "(dd)", "", m_idx, r_ptr->event_after_ranged);
			}

			/* Counter Shot ability. */
			if ((m_ptr->r_idx) && p_ptr->abilities[(CLASS_MARKSMAN * 10) + 3] > 0 && p_ptr->events[29045] == 1)
			{
				p_ptr->events[29046] = m_idx;
				call_lua("use_ability", "(d)", "", 120);
				p_ptr->events[29046] = 0;
			}
		}
		else return (FALSE);
	}

        /* Deactivate the HACK */
        summoner_monster = NULL;
	
	/* Remember what the monster did to us */
	if (seen)
	{
		r_ptr->r_blows[chosenattack] = 1;	
	}
	
	update_and_handle();
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
	
	/* Always take note of monsters that kill you */
	if (death && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
	}

	/* A missile was shot */
	return (TRUE);
}

bool make_ranged_attack_monst(int m_idx)
{
	int             k, chance, thrown_spell, rlev, failrate, i, j, w;
	int		numspells, chosenattack;
	int		t_idx;
	byte            spell[96], num = 0;
        u32b            f4, f5, f6;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	monster_type 	*t_ptr;			/* Putative target */
	monster_race 	*tr_ptr;
	object_type	*o_ptr;
	char            m_name[80];
	char            m_poss[80];
	char            ddesc[80];
	int		ranged[20]; /* Attacks that are ranged. */
	bool            no_inate = FALSE;
	bool		found_ranged = FALSE;
	bool 		visible = FALSE;
	bool 		obvious = FALSE;
	bool 		nothurt = FALSE;
	bool		wake_up = FALSE;
	bool		ammo_shot = FALSE;
	u32b f1, f2, f3;
	s32b damage;
	int flg;

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

	/* Cannot shoot when confused */
	if (m_ptr->confused) return (FALSE);

	/* Cannot shoot when friendly */
	if (is_pet(m_ptr)) return (FALSE);	 
	 
	/* Scan thru all monsters */
	for (w = 1; w < m_max; w++)
	{
		t_idx = w;
		t_ptr = &m_list[t_idx];
		tr_ptr = &r_info[t_ptr->r_idx];

		/* The monster itself isn't a target */
		if (t_ptr == m_ptr) continue;
		
		/* Paranoia -- Skip dead monsters */
		if (!t_ptr->r_idx) continue;
		
		/* Monster must be 'an enemy' */
		if ((is_pet(m_ptr)) == (is_pet(t_ptr))) continue;

		/* Can we attack the target? */
		if (tr_ptr->flags7 & (RF7_NEVER_ATTACKED)) continue;

		/* Guards don't attack friendly monsters */
		if (r_ptr->flags7 & (RF7_GUARD))
		{
			/* Don't attack if it's friendly, and not summoned. */
			if (is_pet(t_ptr) && (t_ptr->summoned == 0)) continue;
		}

		/* Friendly monsters won't attack guards if they aren't summoned. */
		if (tr_ptr->flags7 & (RF7_GUARD))
		{
			if (is_pet(m_ptr) && (m_ptr->summoned == 0 && !(m_ptr->friend))) continue;
		}
		
		/* Hack -- no fighting >100 squares from player */
		if (t_ptr->cdis > MAX_RANGE) continue;
		
		/* Monster must be projectable */
		if (!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx)) continue;
		
		/* OK -- we-ve got a target */
		y = t_ptr->fy;
		x = t_ptr->fx;
		
		/* Extract the monster level */
		rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
		
		/* Extract the racial spell flags */
		f4 = r_ptr->flags4;
		f5 = r_ptr->flags5;
		f6 = r_ptr->flags6;
		
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

		/* Reset ranged count. */
		for (j = 0; j < 20; j++) ranged[j] = 0;

		/* Scan the various attacks. Try to find some type "3" attacks. */
		i = 1;
		for (j = 0; j < 20; j++)
		{
			if (r_ptr->attack[j].type == 3 || r_ptr->attack[j].type == 1000)
			{ 
				ranged[(i-1)] = j;
				i++;
				found_ranged = TRUE;
			}
		}
		
		/* If no ranged attacks were found, return. */
		if (!(found_ranged)) return (FALSE);
		
		/* Pick a random ranged attack! */
		chosenattack = (randint(i) - 1);

		if (r_ptr->attack[chosenattack].type == 1000)
		{
			call_lua(r_ptr->attack[chosenattack].name, "(d)", "", m_idx);
			return;
		}

		/* Now... shoot the poor player! */
		/* Special1 is the radius... */
		if (r_ptr->attack[chosenattack].special1 == 0)
		{
			/* We don't ALWAYS fire... */
			if (randint(100) <= r_ptr->attack[chosenattack].special2)
			{
				/* Call a lua script. */
				if (r_ptr->event_before_ranged > 0)
				{
					call_lua("monster_before_ranged", "(dd)", "", m_idx, r_ptr->event_before_ranged);
				}

				if (monster_hit_monster(m_ptr, t_ptr))
				{
					damage = damroll(r_ptr->attack[chosenattack].ddice, r_ptr->attack[chosenattack].dside);
					damage *= (m_ptr->skill_ranged + 1);
					damage += multiply_divide(damage, ((m_ptr->dex - 5) * 5), 100);
					damage += multiply_divide(damage, m_ptr->dex, 100);
					/* Bosses may get higher damages! */
					if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) damage *= 2;
                        		if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) damage -= damage / 2;
                        		else if (m_ptr->abilities & (CURSE_LOWER_POWER)) damage -= damage / 4;

					if (strstr(r_ptr->attack[chosenattack].name, "!")) msg_format("%^s %s %^s!", m_name, r_ptr->attack[chosenattack].act, t_name);
					else msg_format("%^s %s %s at %^s!", m_name, r_ptr->attack[chosenattack].act, r_ptr->attack[chosenattack].name, t_name);

					flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL;
                			no_magic_return = TRUE;
					monster_ranged = TRUE;
					monst_bolt_monst(m_idx, y, x, r_ptr->attack[chosenattack].element, damage);
					monster_ranged = FALSE;
                			no_magic_return = FALSE;
					ammo_shot = TRUE;
				}
				else msg_format("%^s %s at %^s, but miss.", m_name, r_ptr->attack[chosenattack].act, t_name);

				/* Call a lua script. */
				if (r_ptr->event_after_ranged > 0)
				{
					call_lua("monster_after_ranged", "(dd)", "", m_idx, r_ptr->event_after_ranged);
				}
			}
			else return (FALSE);
		}
		else
		{
			/* We don't ALWAYS fire... */
			if (randint(100) <= r_ptr->attack[chosenattack].special2)
			{
				/* Call a lua script. */
				if (r_ptr->event_before_ranged > 0)
				{
					call_lua("monster_before_ranged", "(dd)", "", m_idx, r_ptr->event_before_ranged);
				}

				if (monster_hit_monster(m_ptr, t_ptr))
				{
					damage = damroll(r_ptr->attack[chosenattack].ddice, r_ptr->attack[chosenattack].dside);
					damage *= (m_ptr->skill_ranged + 1);
					damage += multiply_divide(damage, ((m_ptr->dex - 5) * 5), 100);
					damage += multiply_divide(damage, m_ptr->dex, 100);
					/* Bosses may get higher damages! */
					if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) damage *= 2;
                        		if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) damage -= damage / 2;
                        		else if (m_ptr->abilities & (CURSE_LOWER_POWER)) damage -= damage / 4;

					if (strstr(r_ptr->attack[chosenattack].name, "!")) msg_format("%^s %s %^s!", m_name, r_ptr->attack[chosenattack].act, t_name);
					else msg_format("%^s %s %s at %^s!", m_name, r_ptr->attack[chosenattack].act, r_ptr->attack[chosenattack].name, t_name);

					flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL;
                			no_magic_return = TRUE;
					monster_ranged = TRUE;
					monst_breath_monst(m_idx, y, x, r_ptr->attack[chosenattack].element, damage, r_ptr->attack[chosenattack].special1);
					monster_ranged = FALSE;
                			no_magic_return = FALSE;
					ammo_shot = TRUE;
				}
				else msg_format("%^s %s at %^s, but miss.", m_name, r_ptr->attack[chosenattack].act, t_name);

				/* Call a lua script. */
				if (r_ptr->event_after_ranged > 0)
				{
					call_lua("monster_after_ranged", "(dd)", "", m_idx, r_ptr->event_after_ranged);
				}
			}
			else return (FALSE);
		}
	}

        /* Deactivate the HACK */
        summoner_monster = NULL;
	
	if (wake_up)
   	{
		t_ptr->csleep = 0;
   	}
	
	update_and_handle();
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

	/* A missile was shot */
	if (ammo_shot) return (TRUE);
	else return (FALSE);
}

/* Calls the "bolt" function from a script. */
void lua_bolt(int m_idx, int typ, s32b dam_hp)
{
	bolt(m_idx, typ, dam_hp);
}

/* Same, but for balls. */
void lua_ball(int m_idx, int typ, s32b dam_hp, int rad)
{
	breath(m_idx, typ, dam_hp, rad);
}
