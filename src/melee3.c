/* File: melee3.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Hack, based on mon_take_hit... perhaps all monster attacks on
 * other monsters should use this?
 */
bool mon_take_hit_mon(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type	*m_ptr = &m_list[m_idx];

	monster_race	*r_ptr = &r_info[m_ptr->r_idx];

	char m_name[160];

	bool seen = m_ptr->ml;

	/* Can the player be aware of this attack? */
	bool known = (m_ptr->cdis <= MAX_SIGHT);

	/* Extract monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now... or is it? */
	if (m_ptr->hp < 0)
	{
		if ((r_ptr->flags1 & RF1_UNIQUE) ||
		    (r_ptr->flags1 & RF1_QUESTOR))
		{
			if (seen)
			{
				msg_format("%^s is unharmed.", m_name);
			}

			m_ptr->hp = 1;
		}
		else
		{
			/* if (known && seen) */
			if (known && seen)
			{
				/* Death by special attack */
				if (note)
				{
					msg_format("%^s%s", m_name, note);
				}
				/* Death by normal attack -- living monster */
				else
				{
					msg_format("%^s is killed.", m_name);
				}
			}

			/* Generate treasure */
			monster_death(m_idx);

			/* Delete the monster */
			delete_monster_idx(m_idx);

			/* Not afraid */
			(*fear) = FALSE;

			/* Monster is dead */
			return (TRUE);
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
		int percentage;

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

			/* Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) +
			                  (((dam >= m_ptr->hp) && (percentage > 7)) ?
			                   20 : ((11 - percentage) * 5)));
		}
	}

#endif /* ALLOW_FEAR */


	/* Not dead yet */
	return (FALSE);
}


/*
 * Monster casts a breath (or ball) attack at another monster.
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void monst_breath_monst(int m_idx, int y, int x, int typ, int dam_hp, int rad, bool breath)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	if (rad < 1) rad = (r_ptr->flags2 & RF2_POWERFUL) ? 3 : 2;

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
	u16b flg = PROJECT_STOP | PROJECT_KILL;

	(void)project(m_idx, 0, y, x, dam_hp, typ, flg);
}


/*
 * Monster tries to 'cast a spell' (or breath, etc)
 * at another monster.
 *
 * The player is only disturbed if able to be affected by the spell.
 */
bool monst_spell_monst(int m_idx)
{
	int y = 0, x = 0;
	int i, k, t_idx;
	int chance, thrown_spell, count = 0;
	int rlev;

	byte spell[96], num = 0;

	char m_name[160];
	char t_name[160];
	char m_poss[160];
	char ddesc[160];

	monster_type *m_ptr = &m_list[m_idx];
	monster_type *t_ptr;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_race *tr_ptr;
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	monster_lore *tl_ptr;

	u32b f4, f5, f6;

	/* Expected ball spell radius */
	int rad = (r_ptr->flags2 & RF2_POWERFUL) ? 3 : 2;

	bool wake_up = FALSE;
	bool fear = FALSE;

	bool blind = (p_ptr->blind ? TRUE : FALSE);

	bool see_m = m_ptr->ml;
	bool see_t;
	bool see_either;
	bool see_both;
	bool known;

	bool friendly = m_ptr->is_friendly;

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
		/* The monster itself isn't a target */
		if (i == m_idx) continue;

		t_idx = i;
		t_ptr = &m_list[t_idx];
		tr_ptr = &r_info[t_ptr->r_idx];
		tl_ptr = &l_list[t_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!t_ptr->r_idx) continue;

		/* Monster must be 'an enemy' */
		if (m_ptr->is_friendly == t_ptr->is_friendly) continue;

		/* Monster must be projectable */
		if (!projectable(t_ptr->fy, t_ptr->fx, m_ptr->fy, m_ptr->fx)) continue;

		/* OK -- we've got a target */
		y = t_ptr->fy;
		x = t_ptr->fx;

		/* Extract the monster level */
		rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

		/* Extract the racial spell flags */
		f4 = r_ptr->flags4;
		f5 = r_ptr->flags5;
		f6 = r_ptr->flags6;

		/* Disallow blink unless close */
		if ((distance(m_ptr->fy, m_ptr->fx, y, x) > 1) || !rand_int(3)) f6 &= ~(RF6_BLINK);

		/* Disallow teleport unless wounded */
		if (m_ptr->hp > m_ptr->maxhp / 2) f6 &= ~(RF6_TPORT);

		/* Disallow teleport away unless a friend is wounded */
		if (friendly && (p_ptr->chp > p_ptr->mhp / 4)) f6 &= ~(RF6_TELE_AWAY);

		/* Remove some spells if necessary */
		if (((f4 & RF4_BOLT_MASK) ||
			  (f5 & RF5_BOLT_MASK) ||
			  (f6 & RF6_BOLT_MASK)) &&
			 !(r_ptr->flags2 & RF2_STUPID) &&
			 !clean_shot(t_ptr->fy, t_ptr->fx, m_ptr->fy, m_ptr->fx))
		{
			f4 &= ~(RF4_BOLT_MASK);
			f5 &= ~(RF5_BOLT_MASK);
			f6 &= ~(RF6_BOLT_MASK);
		}

		/* Prevent collateral damage */
		if (friendly && (distance(p_ptr->py, p_ptr->px, y, x) <= rad))
		{
			f4 &= ~(RF4_BALL_MASK);
			f5 &= ~(RF5_BALL_MASK);
			f6 &= ~(RF6_BALL_MASK);
		}

		/* Find another target if no spells available */
		if (!f4 && !f5 && !f6)
		{
			f4 = r_ptr->flags4;
			f5 = r_ptr->flags5;
			f6 = r_ptr->flags6;

			continue;
		}

		/* Hack -- allow "desperate" spells */
		if ((r_ptr->flags2 & RF2_SMART) &&
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

		/* Stop if player is dead or gone */
		if (!p_ptr->playing || p_ptr->is_dead) return (FALSE);

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

		see_t = t_ptr->ml;
		see_either = (see_m || see_t);
		see_both = (see_m && see_t);

		/* Can the player be aware of this attack? */
		known = (m_ptr->cdis <= MAX_SIGHT) || (t_ptr->cdis <= MAX_SIGHT);

		switch (thrown_spell)
		{
			/* RF4_SHRIEK */
			case 96+0:
			{
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s shrieks at %s.", m_name, t_name);
					}
				}

				wake_up = TRUE;

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
				break;
			}

			/* RF4_ARROW_1 */
			case 96+4:
			{
				if (known)
				{
					if (see_either)
					{
						if (blind)
						{
							msg_format("%^s makes a strange noise.", m_name);
						}
						else
						{
							msg_format("%^s fires an arrow at %s.", m_name, t_name);
						}
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_ARROW, damroll(1, 6));

				break;
			}

			/* RF4_ARROW_2 */
			case 96+5:
			{
				if (known)
				{
					if (see_either)
					{
						if (blind)
						{
							msg_format("%^s makes a strange noise.", m_name);
						}
						else
						{
							msg_format("%^s fires an arrow at %s.", m_name, t_name);
						}
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_ARROW, damroll(3, 6));

				break;
			}

			/* RF4_ARROW_3 */
			case 96+6:
			{
				if (known)
				{
					if (see_either)
					{
						if (blind)
						{
							msg_format("%^s makes a strange noise.", m_name);
						}
						else
						{
							msg_format("%^s fires a bolt at %s.", m_name, t_name);
						}
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_ARROW, damroll(5, 6));

				break;
			}

			/* RF4_ARROW_4 */
			case 96+7:
			{
				if (known)
				{
					if (see_either)
					{
						if (blind)
						{
							msg_format("%^s makes a strange noise.", m_name);
						}
						else
						{
							msg_format("%^s fires a bolt at %s.", m_name, t_name);
						}
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_ARROW, damroll(7, 6));

				break;
			}

			/* RF4_BR_ACID */
			case 96+8:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes acid at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_ACID,
					((m_ptr->hp / 3) > 1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);

				break;
			}

			/* RF4_BR_ELEC */
			case 96+9:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes lightning at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_ELEC,
					((m_ptr->hp / 3) > 1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);

				break;
			}

			/* RF4_BR_FIRE */
			case 96+10:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes fire at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_FIRE,
					((m_ptr->hp / 3) > 1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);

				break;
			}

			/* RF4_BR_COLD */
			case 96+11:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes frost at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_COLD,
					((m_ptr->hp / 3) > 1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);
				break;
			}

			/* RF4_BR_POIS */
			case 96+12:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes gas at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_POIS,
					((m_ptr->hp / 3) > 600 ? 600 : (m_ptr->hp / 3)), 0, TRUE);

				break;
			}

			/* RF4_BR_NETH */
			case 96+13:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes nether at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_NETHER,
					((m_ptr->hp / 6) > 450 ? 450 : (m_ptr->hp / 6)), 0, TRUE);

				break;
			}

			/* RF4_BR_LITE */
			case 96+14:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes light at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_LITE,
					((m_ptr->hp / 4) > 350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_DARK */
			case 96+15:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes darkness at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_DARK,
					((m_ptr->hp / 4) > 350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_CONF */
			case 96+16:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes confusion at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_CONFUSION,
					((m_ptr->hp / 4) > 350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_SOUN */
			case 96+17:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes sound at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_SOUND,
					((m_ptr->hp / 4) > 350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_CHAO */
			case 96+18:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes chaos at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_CHAOS,
					((m_ptr->hp / 4) > 500 ? 500 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_DISE */
			case 96+19:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes disenchantment at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_DISENCHANT,
					((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_NEXU */
			case 96+20:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes nexus at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_NEXUS,
					((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)), 0, TRUE);

				break;
			}

			/* RF4_BR_TIME */
			case 96+21:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes time at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_TIME,
					((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3)), 0, TRUE);

				break;
			}

			/* RF4_BR_INER */
			case 96+22:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes inertia at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_INERTIA,
					((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_GRAV */
			case 96+23:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes gravity at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_GRAVITY,
					((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)), 0, TRUE);

				break;
			}

			/* RF4_BR_SHAR */
			case 96+24:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes shards at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_SHARD,
					((m_ptr->hp / 4) > 400 ? 400 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_PLAS */
			case 96+25:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes plasma at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_PLASMA,
					((m_ptr->hp / 4) > 150 ? 150 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_WALL */
			case 96+26:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes force at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_FORCE,
					((m_ptr->hp / 4) > 200 ? 200 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			/* RF4_BR_MANA */
			case 96+27:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s breathes.", m_name);
						}
						else
						{
							msg_format("%^s breathes mana at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_MANA,
					((m_ptr->hp / 4) > 250 ? 250 : (m_ptr->hp / 4)), 0, TRUE);

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
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles.", m_name);
						}
						else
						{
							msg_format("%^s casts an acid ball at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_ACID, randint(rlev * 3) + 15, 2, FALSE);

				break;
			}

			/* RF5_BA_ELEC */
			case 128+1:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles.", m_name);
						}
						else
						{
							msg_format("%^s casts a lightning ball at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_ELEC, randint(rlev * 3 / 2) + 8, 2, FALSE);

				break;
			}

			/* RF5_BA_FIRE */
			case 128+2:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles.", m_name);
						}
						else
						{
							msg_format("%^s casts a fire ball at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_FIRE, randint(rlev * 7 / 2) + 10, 2, FALSE);

				break;
			}

			/* RF5_BA_COLD */
			case 128+3:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles.", m_name);
						}
						else
						{
							msg_format("%^s casts a frost ball at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_COLD, randint(rlev * 3 / 2) + 10, 2, FALSE);

				break;
			}

			/* RF5_BA_POIS */
			case 128+4:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles.", m_name);
						}
						else
						{
							msg_format("%^s casts a stinking cloud at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_POIS, damroll(12, 2), 2, FALSE);

				break;
			}

			/* RF5_BA_NETH */
			case 128+5:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles.", m_name);
						}
						else
						{
							msg_format("%^s casts a nether ball at %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_NETHER, (50 + damroll(10, 10) + rlev), 2, FALSE);

				break;
			}

			/* RF5_BA_WATE */
			case 128+6:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles.", m_name);
						}
						else
						{
							msg_format("%^s gestures fluidly at %s.", m_name, t_name);
							msg_format("%^s is engulfed in a whirlpool.", t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_WATER, randint(rlev * 5 / 2) + 50, 4, FALSE);

				break;
			}

			/* RF5_BA_MANA */
			case 128+7:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles powerfully.", m_name);
						}
						else
						{
							msg_format("%^s invokes a mana storm upon %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_MANA, (rlev * 4) + damroll(10, 10), 4, FALSE);

				break;
			}

			/* RF5_BA_DARK */
			case 128+8:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
							msg_format("%^s mumbles powerfully.", m_name);
						}
						else
						{
							msg_format("%^s invokes a darkness storm upon %s.", m_name, t_name);
						}
					}
				}

				monst_breath_monst(m_idx, y, x, GF_DARK, (rlev * 4) + damroll(10, 10), 4, FALSE);

				break;
			}

			/* RF5_DRAIN_MANA */
			case 128+9:
			{
				/* Attack power */
				int power = (randint(rlev) / 2) + 1;

				if (see_m)
				{
					/* Basic message */
					msg_format("%^s draws psychic energy from %s.", m_name, t_name);
				}

				/* Heal the monster */
				if (m_ptr->hp < m_ptr->maxhp)
				{
					if (!tr_ptr->flags4 && !tr_ptr->flags5 && !tr_ptr->flags6)
					{
						if (see_both)
						{
							msg_format("%^s is unaffected!", t_name);
						}
					}
					else
					{
						/* Heal */
						m_ptr->hp += 6 * power;
						if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

						/* Redraw (later) if needed */
						if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

						/* Special message */
						if (see_m)
						{
							msg_format("%^s appears healthier.", m_name);
						}
					}
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_MIND_BLAST */
			case 128+10:
			{
				if (see_m)
				{
					msg_format("%^s gazes intently at %s.", m_name, t_name);
				}

				/* Attempt a saving throw */
				if ((tr_ptr->flags1 & RF1_UNIQUE) ||
					 (tr_ptr->flags3 & RF3_NO_CONF) ||
					 (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10))
				{
					/* No obvious effect */
					if (see_both)
					{
						/* Memorize a flag */
						if (tr_ptr->flags3 & (RF3_NO_CONF))
						{
							tl_ptr->r_flags3 |= (RF3_NO_CONF);
						}

						msg_format("%^s is unaffected!", t_name);
					}
				}
				else
				{
					if (see_t)
					{
						msg_format("%^s is blasted by psionic energy.", t_name);
					}

					t_ptr->confused += rand_int(4) + 4;

					mon_take_hit_mon(t_idx, damroll(8, 8), &fear, " collapses, a mindless husk.");
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_BRAIN_SMASH */
			case 128+11:
			{
				if (see_m)
				{
					msg_format("%^s gazes intently at %s.", m_name, t_name);
				}

				/* Attempt a saving throw */
				if ((tr_ptr->flags1 & RF1_UNIQUE) ||
					 (tr_ptr->flags3 & RF3_NO_CONF) ||
					 (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10))
				{
					/* No obvious effect */
					if (see_both)
					{
						/* Memorize a flag */
						if (tr_ptr->flags3 & (RF3_NO_CONF))
						{
							tl_ptr->r_flags3 |= (RF3_NO_CONF);
						}

						msg_format("%^s is unaffected!", t_name);
					}
				}
				else
				{
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
			case 128+12:
			{
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s points at %s and curses.", m_name, t_name);
					}
				}

				if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_both) msg_format("%^s resists!", t_name);
				}
				else
				{
					mon_take_hit_mon(t_idx, damroll(3, 8), &fear, " is destroyed.");
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_CAUSE_2 */
			case 128+13:
			{
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s points at %s and curses horribly.", m_name, t_name);
					}
				}

				if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_both) msg_format("%^s resists!", t_name);
				}
				else
				{
					mon_take_hit_mon(t_idx, damroll(8, 8), &fear, " is destroyed.");
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_CAUSE_3 */
			case 128+14:
			{
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s points at %s, incanting terribly!", m_name, t_name);
					}
				}

				if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_both) msg_format("%^s resists!", t_name);
				}
				else
				{
					mon_take_hit_mon(t_idx, damroll(10, 15), &fear, " is destroyed.");
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_CAUSE_4 */
			case 128+15:
			{
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s points at %s, screaming the word, 'DIE!'", m_name, t_name);
					}
				}

				if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_both) msg_format("%^s resists!", t_name);
				}
				else
				{
					mon_take_hit_mon(t_idx, damroll(15, 15), &fear, " is destroyed.");
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_BO_ACID */
			case 128+16:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts an acid bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_ACID,
					damroll(7, 8) + (rlev / 3));

				break;
			}

			/* RF5_BO_ELEC */
			case 128+17:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a lightning bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_ELEC,
					damroll(4, 8) + (rlev / 3));

				break;
			}

			/* RF5_BO_FIRE */
			case 128+18:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a fire bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_FIRE,
					damroll(9, 8) + (rlev / 3));

				break;
			}

			/* RF5_BO_COLD */
			case 128+19:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a frost bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_COLD,
					damroll(6, 8) + (rlev / 3));

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
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a nether bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_NETHER,
					30 + damroll(5, 5) + (rlev * 3) / 2);

				break;
			}

			/* RF5_BO_WATE */
			case 128+22:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a water bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_WATER,
					damroll(10, 10) + rlev);

				break;
			}

			/* RF5_BO_MANA */
			case 128+23:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a mana bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_MANA,
					randint(rlev * 7 / 2) + 50);

				break;
			}

			/* RF5_BO_PLAS */
			case 128+24:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a plasma bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_PLASMA,
					10 + damroll(8, 7) + rlev);

				break;
			}

			/* RF5_BO_ICEE */
			case 128+25:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts an ice bolt at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_ICE,
					damroll(6, 6) + rlev);

				break;
			}

			/* RF5_MISSILE */
			case 128+26:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a magic missile at %s.", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_MISSILE,
					damroll(2, 6) + (rlev / 3));

				break;
			}

			/* RF5_SCARE */
			case 128+27:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a fearful illusion in front of %s.", m_name, t_name);
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
					if (!t_ptr->monfear) fear = TRUE;

					t_ptr->monfear += rand_int(4) + 4;
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_BLIND */
			case 128+28:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a spell, burning %s%s eyes.", m_name, t_name,
									  (streq(t_name, "it") ? "s" : "'s"));
					}
				}

				/* Simulate blindness with confusion */
				if (tr_ptr->flags3 & RF3_NO_CONF)
				{
					if (see_t) msg_format("%^s is unaffected.", t_name);
				}
				else if (tr_ptr->level > randint((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
				{
					if (see_t) msg_format("%^s is unaffected.", t_name);
				}
				else
				{
					if (see_t) msg_format("%^s is blinded!", t_name);

					t_ptr->confused += 12 + (byte)rand_int(4);
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_CONF */
			case 128+29:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s casts a mesmerizing illusion in front of %s.", m_name, t_name);
					}
				}

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
					if (see_t) msg_format("%^s seems confused.", t_name);

					t_ptr->confused += 12 + (byte)rand_int(4);
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_SLOW */
			case 128+30:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s drains power from %s%s muscles.", m_name, t_name,
									  (streq(t_name, "it") ? "s" : "'s"));
					}
				}

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
					if (see_t) msg_format("%^s starts moving slower.", t_name);

					t_ptr->mspeed -= 10;
				}

				wake_up = TRUE;

				break;
			}

			/* RF5_HOLD */
			case 128+31:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s stares intently at %s.", m_name, t_name);
					}
				}

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
					if (see_t) msg_format("%^s is paralyzed!", t_name);

					t_ptr->stunned += randint(4) + 4;
				}

				wake_up = TRUE;

				break;
			}


			/* RF6_HASTE */
			case 160+0:
			{
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s concentrates on %s body.", m_name, m_poss);
					}
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
			case 160+1:
			{
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s invokes the Hand of Doom upon %s!", m_name, t_name);
					}
				}

				if (tr_ptr->flags1 & RF1_UNIQUE)
				{
					if (see_both) msg_format("^%s is unaffected!", t_name);
				}
				else
				{
					if ((r_ptr->level + randint(20)) >
						(tr_ptr->level + 10 + randint(20)))
					{
						t_ptr->hp = t_ptr->hp -
						  (((s32b)((65 + randint(25)) * t_ptr->hp)) / 100);

						if (t_ptr->hp < 1) t_ptr->hp = 1;
					}
					else
					{
						if (see_both) msg_format("%^s resists!", t_name);
					}
				}

				wake_up = TRUE;

				break;
			}

			/* RF6_HEAL */
			case 160+2:
			{
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
					}
				}

				/* Heal some */
				m_ptr->hp += (rlev * 6);

				/* Fully healed */
				if (m_ptr->hp >= m_ptr->maxhp)
				{
					/* Fully healed */
					m_ptr->hp = m_ptr->maxhp;

					if (known)
					{
						if (see_m)
						{
							msg_format("%^s looks completely healed!", m_name);
						}
					}
				}

				/* Partially healed */
				else if (known)
				{
					if (see_m)
					{
						msg_format("%^s looks healthier.", m_name);
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
					if (see_m) msg_format("%^s recovers %s courage.", m_name, m_poss);
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
				if (see_m)
				{
					msg_format("%^s blinks away.", m_name);
				}

				teleport_away(m_idx, 10);

				break;
			}

			/* RF6_TPORT */
			case 160+5:
			{
				if (see_m)
				{
					msg_format("%^s teleports away.", m_name);
				}

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
				/* Not implemented */
				break;
			}

			/* RF6_TELE_AWAY */
			case 160+9:
			{
				if (known)
				{
					if (see_either)
					{
						msg_format("%^s teleports %s away.", m_name, t_name);
					}
				}

				teleport_away(t_idx, MAX_SIGHT * 2 + 5);

				break;
			}

			/* RF6_TELE_LEVEL */
			case 160+10:
			{
				/* Not implemented */
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
				if (known)
				{
					if (see_m)
					{
						msg_format("%^s gestures in shadow.", m_name);

						if (see_t)
						{
							msg_format("%^s is surrounded by darkness.", t_name);
						}
					}
				}

				(void)project(m_idx, 3, y, x, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);

				unlite_room(y, x);

				break;
			}

			/* RF6_TRAPS */
			case 160+13:
			{
				/* Not implemented */
				break;
			}

			/* RF6_FORGET */
			case 160+14:
			{
				/* Not implemented */
				break;
			}

			/* RF6_XXX6X6 */
			case 160+15:
			{
				break;
			}

			/* RF6_SUMMON_KIN */
			case 160+16:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons %s %s.", m_name, m_poss,
									  ((r_ptr->flags1 & RF1_UNIQUE) ? "minions" : "kin"));
					}
				}

				summon_kin_type = r_ptr->d_char;

				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_KIN, friendly);
				}

				break;
			}

			/* RF6_S_HI_DEMON */
			case 160+17:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons demons!", m_name);
					}
				}

			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_DEMON, friendly);
			}

				break;
			}

			/* RF6_S_MONSTER */
			case 160+18:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons help!", m_name);
					}
				}

				count += summon_specific(y, x, rlev, 0, friendly);

				break;
			}

			/* RF6_S_MONSTERS */
			case 160+19:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons monsters!", m_name);
					}
				}

				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, 0, friendly);
				}

				break;
			}

			/* RF6_S_ANT */
			case 160+20:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons ants.", m_name);
					}
				}

				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_ANT, friendly);
				}

				break;
			}

			/* RF6_S_SPIDER */
			case 160+21:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons spiders.", m_name);
					}
				}

				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_SPIDER, friendly);
				}

				break;
			}

			/* RF6_S_HOUND */
			case 160+22:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons hounds.", m_name);
					}
				}

				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_HOUND, friendly);
				}

				break;
			}

			/* RF6_S_HYDRA */
			case 160+23:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons hydras.", m_name);
					}
				}

				for (k = 0; k < 6; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_HYDRA, friendly);
				}

				break;
			}

			/* RF6_S_ANGEL */
			case 160+24:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons an angel!", m_name);
					}
				}

				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_ANGEL, friendly);
				}

				break;
			}

			/* RF6_S_DEMON */
			case 160+25:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons a demon from the Courts of Chaos!", m_name);
					}
				}

				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_DEMON, friendly);
				}

				break;
			}

			/* RF6_S_UNDEAD */
			case 160+26:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons undead.", m_name);
					}
				}

				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_UNDEAD, friendly);
				}

				break;
			}

			/* RF6_S_DRAGON */
			case 160+27:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons a dragon!", m_name);
					}
				}

				for (k = 0; k < 1; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_DRAGON, friendly);
				}

				break;
			}

			/* RF6_S_HI_UNDEAD */
			case 160+28:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons undead.", m_name);
					}
				}

				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD, friendly);
				}

				break;
			}

			/* RF6_S_HI_DRAGON */
			case 160+29:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons ancient dragons!", m_name);
					}
				}

				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_HI_DRAGON, friendly);
				}

				break;
			}

			/* RF6_S_WRAITH */
			case 160+30:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons mighty undead opponents!", m_name);
					}
				}

				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_WRAITH, m_ptr->is_friendly);
				}
				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD, m_ptr->is_friendly);
				}

				break;
			}

			/* RF6_S_UNIQUE */
			case 160+31:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						msg_format("%^s magically summons special opponents!", m_name);
					}
				}

				for (k = 0; k < 8; k++)
				{
					count += summon_specific(y, x, rlev, SUMMON_UNIQUE, FALSE);
				}

				break;
			}
		}

		if (wake_up)
		{
			t_ptr->csleep = 0;
		}

		if (fear && see_t)
		{
			msg_format("%^s flees in terror!", t_name);
		}

		/* Remember what the monster did, if we saw it */
		if (see_m)
		{
			/* Inate spell */
			if (thrown_spell < 32*4)
			{
				l_ptr->r_flags4 |= (1L << (thrown_spell - 32*3));
				if (l_ptr->r_cast_inate < MAX_UCHAR) l_ptr->r_cast_inate++;
			}

			/* Bolt or Ball */
			else if (thrown_spell < 32*5)
			{
				l_ptr->r_flags5 |= (1L << (thrown_spell - 32*4));
				if (l_ptr->r_cast_spell < MAX_UCHAR) l_ptr->r_cast_spell++;
			}

			/* Special spell */
			else if (thrown_spell < 32*6)
			{
				l_ptr->r_flags6 |= (1L << (thrown_spell - 32*5));
				if (l_ptr->r_cast_spell < MAX_UCHAR) l_ptr->r_cast_spell++;
			}
		}

		/* Always take note of monsters that kill you */
		if (p_ptr->is_dead && (l_ptr->r_deaths < MAX_SHORT))
		{
			l_ptr->r_deaths++;
		}

		/* A spell was cast */
		return (TRUE);
	}

	/* No enemy found */
	return (FALSE);
}
