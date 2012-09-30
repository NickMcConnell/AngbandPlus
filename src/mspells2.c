/* File: mspells2.c */

/* Purpose: Monster spells (attack monster) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Monster casts a breath (or ball) attack at another monster.
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void monst_breath_monst(int m_idx, int x, int y, int typ, int dam_hp,
                               int rad, bool breath)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	if (rad < 1) rad = FLAG(r_ptr, RF_POWERFUL) ? 3 : 2;

	/* Handle breath attacks */
	if (breath) rad = 0 - rad;

	(void)project(m_idx, rad, x, y, dam_hp, typ, flg);
}


/*
 * Monster casts a bolt at another monster
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void monst_bolt_monst(int m_idx, int x, int y, int typ, int dam_hp)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;

	(void)project(m_idx, 0, x, y, dam_hp, typ, flg);
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

	u32b f4, f5, f6;

	/* Expected ball spell radius */
	int rad = FLAG(r_ptr, RF_POWERFUL) ? 3 : 2;

	bool wake_up = FALSE;
	bool fear = FALSE;

	bool blind = (p_ptr->tim.blind ? TRUE : FALSE);

	bool see_m = m_ptr->ml;
	bool see_t;
	bool see_either;
	bool see_both;
	bool known;

	bool friendly = is_friendly(m_ptr);
	bool pet = is_pet(m_ptr);

	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	/* Hack -- Extract the spell probability */
	chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance) return (FALSE);

	if (randint0(100) >= chance) return (FALSE);

	/* Stop if player is dead or gone */
	if (!p_ptr->state.playing || p_ptr->state.is_dead) return (FALSE);

	/* Handle "leaving" */
	if (p_ptr->state.leaving) return (FALSE);

	/* Scan thru all monsters */
	for (i = 1; i < m_max; i++)
	{
		/* The monster itself isn't a target */
		if (i == m_idx) continue;

		t_idx = i;
		t_ptr = &m_list[t_idx];
		tr_ptr = &r_info[t_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!t_ptr->r_idx) continue;

		/* Monster must be 'an enemy' */
		if (!are_enemies(m_ptr, t_ptr)) continue;

		/* Monster must be projectable */
		if (!projectable(m_ptr->fx, m_ptr->fy, t_ptr->fx, t_ptr->fy)) continue;

		/* OK -- we've got a target */
		y = t_ptr->fy;
		x = t_ptr->fx;

		/* Extract the monster level */
		rlev = ((r_ptr->hdice * 2 >= 1) ? r_ptr->hdice * 2 : 1);

		/* Extract the racial spell flags */
		f4 = r_ptr->flags[3];
		f5 = r_ptr->flags[4];
		f6 = r_ptr->flags[5];

		/* Disallow blink unless close */
		if ((distance(m_ptr->fx, m_ptr->fy, x, y) > 1)
			|| one_in_(3)) f6 &= ~(RF5_BLINK);

		/* Disallow teleport unless wounded */
		if (m_ptr->hp > m_ptr->maxhp / 2) f6 &= ~(RF5_TPORT);

		/* Disallow teleport away unless a friend is wounded */
		if (friendly && (p_ptr->chp > p_ptr->mhp / 4)) f6 &= ~(RF5_TELE_AWAY);

		/* Remove some spells if necessary */
		if (!stupid_monsters &&
			((f4 & RF3_BOLT_MASK) ||
			 (f5 & RF4_BOLT_MASK) ||
			 (f6 & RF5_BOLT_MASK)) &&
			!FLAG(r_ptr, RF_STUPID) &&
			!clean_shot(m_ptr->fx, m_ptr->fy, t_ptr->fx, t_ptr->fy,
						is_pet(m_ptr)))
		{
			f4 &= ~(RF3_BOLT_MASK);
			f5 &= ~(RF4_BOLT_MASK);
			f6 &= ~(RF5_BOLT_MASK);
		}

		/* Prevent collateral damage */
		if (friendly && (distance(p_ptr->px, p_ptr->py, x, y) <= rad))
		{
			f4 &= ~(RF3_BALL_MASK);
			f5 &= ~(RF4_BALL_MASK);
			f6 &= ~(RF5_BALL_MASK);
		}

		/* Find another target if no spells available */
		if (!f4 && !f5 && !f6)
		{
			f4 = r_ptr->flags[3];
			f5 = r_ptr->flags[4];
			f6 = r_ptr->flags[5];

			continue;
		}

		/* Hack -- allow "desperate" spells */
		if (FLAG(r_ptr, RF_SMART) &&
			(m_ptr->hp < m_ptr->maxhp / 10) && one_in_(2))
		{
			/* Require intelligent spells */
			f4 &= (RF3_INT_MASK);
			f5 &= (RF4_INT_MASK);
			f6 &= (RF5_INT_MASK);

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

		/* Get the monster name (or "it") */
		monster_desc(m_name, m_ptr, 0, 160);

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(m_poss, m_ptr, 0x22, 160);

		/* Get the target's name (or "it") */
		monster_desc(t_name, t_ptr, 0, 160);

		/* Hack -- Get the "died from" name */
		monster_desc(ddesc, m_ptr, 0x88, 160);

		/* Choose a spell to cast */
		thrown_spell = spell[randint0(num)];

		see_t = t_ptr->ml;
		see_either = (see_m || see_t);
		see_both = (see_m && see_t);

		/* Can the player be aware of this attack? */
		known = (m_ptr->cdis <= MAX_SIGHT) || (t_ptr->cdis <= MAX_SIGHT);

		switch (thrown_spell)
		{
			case 96 + 0:
			{
				/* RF3_SHRIEK */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s shrieks at %s.", m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				wake_up = TRUE;

				break;
			}

			case 96 + 1:
			{
				/* RF3_ELDRITCH_HORROR */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s stares at %s.", m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				wake_up = TRUE;

				break;
			}

			case 96 + 2:
			{
				/* RF3_XXX3X4 */
				break;
			}

			case 96 + 3:
			{
				/* RF3_ROCKET */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s shoots something.", m_name);
						}
						else
						{
							msgf("%^s fires a rocket at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_ROCKET,
								   ((m_ptr->hp / 4) >
									600 ? 600 : (m_ptr->hp / 4)), 2, FALSE);

				break;
			}

			case 96 + 4:
			{
				int dice = r_ptr->hdice < 4 ? 1 : r_ptr->hdice / 4;
				if (dice > 7) dice = 7;

				/* RF3_ARROW */
				if (known)
				{
					if (see_either)
					{
						if (blind)
						{
							msgf("%^s makes a strange noise.", m_name);
						}
						else
						{
							msgf("%^s fires an arrow at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_SHOOT);
				}

				monst_bolt_monst(m_idx, x, y, GF_ARROW, damroll(dice, 6));

				break;
			}

			case 96 + 5:
			{
				/* RF3_XXX6 */
				break;
			}

			case 96 + 6:
			{
				/* RF3_XXX7 */
				break;
			}

			case 96 + 7:
			{
				/* RF3_XXX8 */
				break;
			}

			case 96 + 8:
			{
				/* RF3_BR_ACID */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes acid at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_ACID,
								   ((m_ptr->hp / 3) >
									1200 ? 1200 : (m_ptr->hp / 3)), 0, TRUE);

				break;
			}

			case 96 + 9:
			{
				/* RF3_BR_ELEC */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes lightning at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_ELEC,
								   ((m_ptr->hp / 2) >
									1200 ? 1200 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 96 + 10:
			{
				/* RF3_BR_FIRE */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes fire at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_FIRE,
								   ((m_ptr->hp / 2) >
									1200 ? 1200 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 96 + 11:
			{
				/* RF3_BR_COLD */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes frost at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_COLD,
								   ((m_ptr->hp / 2) >
									1200 ? 1200 : (m_ptr->hp / 2)), 0, TRUE);
				break;
			}

			case 96 + 12:
			{
				/* RF3_BR_POIS */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes gas at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_POIS,
								   ((m_ptr->hp / 2) >
									600 ? 600 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 96 + 13:
			{
				/* RF3_BR_NETH */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes nether at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_NETHER,
								   ((m_ptr->hp / 4) >
									450 ? 450 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 14:
			{
				/* RF3_BR_LITE */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes light at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_LITE,
								   ((m_ptr->hp / 4) >
									350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 15:
			{
				/* RF3_BR_DARK */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes darkness at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_DARK,
								   ((m_ptr->hp / 4) >
									350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 16:
			{
				/* RF3_BR_CONF */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes confusion at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_CONFUSION,
								   ((m_ptr->hp / 4) >
									350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 17:
			{
				/* RF3_BR_SOUN */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes sound at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_SOUND,
								   ((m_ptr->hp / 4) >
									350 ? 350 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 18:
			{
				/* RF3_BR_CHAO */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes chaos at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_CHAOS,
								   ((m_ptr->hp / 4) >
									500 ? 500 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 19:
			{
				/* RF3_BR_DISE */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes disenchantment at %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_DISENCHANT,
								   ((m_ptr->hp / 4) >
									400 ? 400 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 20:
			{
				/* RF3_BR_NEXU */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes nexus at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_NEXUS,
								   ((m_ptr->hp / 2) >
									250 ? 250 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 96 + 21:
			{
				/* RF3_BR_TIME */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes time at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_TIME,
								   ((m_ptr->hp / 2) >
									150 ? 150 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 96 + 22:
			{
				/* RF3_BR_INER */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes inertia at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_INERTIA,
								   ((m_ptr->hp / 4) >
									200 ? 200 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 23:
			{
				/* RF3_BR_GRAV */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes gravity at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_GRAVITY,
								   ((m_ptr->hp / 2) >
									200 ? 200 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 96 + 24:
			{
				/* RF3_BR_SHAR */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes shards at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_SHARDS,
								   ((m_ptr->hp / 4) >
									400 ? 400 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 25:
			{
				/* RF3_BR_PLAS */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes plasma at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_PLASMA,
								   ((m_ptr->hp / 4) >
									150 ? 150 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 26:
			{
				/* RF3_BR_WALL */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes force at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_FORCE,
								   ((m_ptr->hp / 4) >
									200 ? 200 : (m_ptr->hp / 4)), 0, TRUE);

				break;
			}

			case 96 + 27:
			{
				/* RF3_BR_MANA */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes mana at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_MANA,
								   ((m_ptr->hp / 2) >
									200 ? 200 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 96 + 28:
			{
				/* RF3_BA_NUKE */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles.", m_name);
						}
						else
						{
							msgf("%^s casts a ball of radiation at %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_NUKE,
								   (rlev + damroll(10, 6)), 2, FALSE);

				break;
			}

			case 96 + 29:
			{
				/* RF3_BR_NUKE */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes toxic waste at %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_NUKE,
								   ((m_ptr->hp / 2) >
									600 ? 600 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 96 + 30:
			{
				/* RF3_BA_CHAO */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles frighteningly.", m_name);
						}
						else
						{
							msgf("%^s invokes raw Logrus upon %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_CHAOS,
								   (rlev * 2) + damroll(10, 10), 4, FALSE);

				break;
			}

			case 96 + 31:
			{
				/* RF3_BR_DISI */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s breathes.", m_name);
						}
						else
						{
							msgf("%^s breathes disintegration at %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				monst_breath_monst(m_idx, x, y, GF_DISINTEGRATE,
								   ((m_ptr->hp / 2) >
									300 ? 300 : (m_ptr->hp / 2)), 0, TRUE);

				break;
			}

			case 128 + 0:
			{
				/* RF4_BA_ACID */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles.", m_name);
						}
						else
						{
							msgf("%^s casts an acid ball at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_ACID,
					damroll(r_ptr->hdice, 6), 2, FALSE);

				break;
			}

			case 128 + 1:
			{
				/* RF4_BA_ELEC */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles.", m_name);
						}
						else
						{
							msgf("%^s casts a lightning ball at %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_ELEC,
					damroll(r_ptr->hdice, 6), 2, FALSE);

				break;
			}

			case 128 + 2:
			{
				/* RF4_BA_FIRE */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles.", m_name);
						}
						else
						{
							msgf("%^s casts a fire ball at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_FIRE,
					damroll(r_ptr->hdice, 6), 2, FALSE);

				break;
			}

			case 128 + 3:
			{
				/* RF4_BA_COLD */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles.", m_name);
						}
						else
						{
							msgf("%^s casts a frost ball at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_COLD,
					damroll(r_ptr->hdice, 6), 2, FALSE);

				break;
			}

			case 128 + 4:
			{
				/* RF4_BA_POIS */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles.", m_name);
						}
						else
						{
							msgf("%^s casts a stinking cloud at %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_POIS, damroll(12, 2), 2,
								   FALSE);

				break;
			}

			case 128 + 5:
			{
				/* RF4_BA_NETH */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles.", m_name);
						}
						else
						{
							msgf("%^s casts a nether ball at %s.", m_name,
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_NETHER,
								   (50 + damroll(10, 10) + rlev), 2, FALSE);

				break;
			}

			case 128 + 6:
			{
				/* RF4_BA_WATE */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles.", m_name);
						}
						else
						{
							msgf("%^s gestures fluidly at %s.", m_name,
									   t_name);
							msgf("%^s is engulfed in a whirlpool.",
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_WATER,
					damroll(r_ptr->hdice, 8), 4, FALSE);

				break;
			}

			case 128 + 7:
			{
				/* RF4_BA_MANA */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles powerfully.", m_name);
						}
						else
						{
							msgf("%^s invokes a mana storm upon %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_MANA,
								   (rlev * 4) + damroll(10, 10), 4, FALSE);

				break;
			}

			case 128 + 8:
			{
				/* RF4_BA_DARK */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						if (blind)
						{
							msgf("%^s mumbles powerfully.", m_name);
						}
						else
						{
							msgf("%^s invokes a darkness storm upon %s.",
									   m_name, t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_breath_monst(m_idx, x, y, GF_DARK,
								   (rlev * 4) + damroll(10, 10), 4, FALSE);

				break;
			}

			case 128 + 9:
			{
				/* RF4_DRAIN_MANA */

				/* Attack power */
				int power = (randint1(rlev) / 2) + 1;

				if (see_m)
				{
					/* Basic message */
					msgf("%^s draws psychic energy from %s.", m_name,
							   t_name);
				}

				/* Heal the monster */
				if (m_ptr->hp < m_ptr->maxhp)
				{
					if (!tr_ptr->flags[3] && !tr_ptr->flags[4] && !tr_ptr->flags[5])
					{
						if (see_both)
						{
							msgf("%^s is unaffected!", t_name);
						}
					}
					else
					{
						/* Heal */
						m_ptr->hp += 6 * power;
						if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

						/* Redraw (later) if needed */
						if (p_ptr->health_who == m_idx) p_ptr->redraw |=
								(PR_HEALTH);

						/* Special message */
						if (see_m)
						{
							msgf("%^s appears healthier.", m_name);
						}
					}
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 10:
			{
				/* RF4_MIND_BLAST */
				if (see_m)
				{
					msgf("%^s gazes intently at %s.", m_name, t_name);
				}

				/* Attempt a saving throw */
				if (FLAG(tr_ptr, RF_UNIQUE) ||
					FLAG(tr_ptr, RF_NO_CONF) ||
					(tr_ptr->hdice * 2 > randint1(rlev * 3) / 2))
				{
					/* No obvious effect */
					if (see_both)
					{
						/* Memorize a flag */
						if (FLAG(tr_ptr, RF_NO_CONF))
						{
							tr_ptr->r_flags[2] |= (RF2_NO_CONF);
						}

						msgf("%^s is unaffected!", t_name);
					}
				}
				else
				{
					if (see_t)
					{
						msgf("%^s is blasted by psionic energy.", t_name);
					}

					t_ptr->confused += (byte)rand_range(4, 8);

					mon_take_hit_mon(t_idx, damroll(8, 8), &fear,
									 " collapses, a mindless husk.");
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 11:
			{
				/* RF4_BRAIN_SMASH */
				if (see_m)
				{
					msgf("%^s gazes intently at %s.", m_name, t_name);
				}

				/* Attempt a saving throw */
				if (FLAG(tr_ptr, RF_UNIQUE) ||
					FLAG(tr_ptr, RF_NO_CONF) ||
					(tr_ptr->hdice * 2 > randint1(rlev * 3) / 2))
				{
					/* No obvious effect */
					if (see_both)
					{
						/* Memorize a flag */
						if (FLAG(tr_ptr, RF_NO_CONF))
						{
							tr_ptr->r_flags[2] |= (RF2_NO_CONF);
						}

						msgf("%^s is unaffected!", t_name);
					}
				}
				else
				{
					if (see_t)
					{
						msgf("%^s is blasted by psionic energy.", t_name);
					}

					t_ptr->confused += (byte)rand_range(4, 8);
					t_ptr->mspeed -= (byte)rand_range(4, 8);
					t_ptr->stunned += (byte)rand_range(4, 8);

					mon_take_hit_mon(t_idx, damroll(12, 15), &fear,
									 " collapses, a mindless husk.");
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 12:
			{
				/* RF4_CAUSE_1 */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s points at %s and curses.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_both) msgf("%^s resists!", t_name);
				}
				else
				{
					mon_take_hit_mon(t_idx, damroll(3, 8), &fear,
									 " is destroyed.");
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 13:
			{
				/* RF4_CAUSE_2 */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s points at %s and curses horribly.",
								   m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_both) msgf("%^s resists!", t_name);
				}
				else
				{
					mon_take_hit_mon(t_idx, damroll(8, 8), &fear,
									 " is destroyed.");
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 14:
			{
				/* RF4_CAUSE_3 */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s points at %s, incanting terribly!",
								   m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_both) msgf("%^s resists!", t_name);
				}
				else
				{
					mon_take_hit_mon(t_idx, damroll(10, 15), &fear,
									 " is destroyed.");
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 15:
			{
				/* RF4_CAUSE_4 */
				if (known)
				{
					if (see_m)
					{
						msgf
							("%^s points at %s, screaming the word, 'DIE!'",
							 m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_both) msgf("%^s resists!", t_name);
				}
				else
				{
					mon_take_hit_mon(t_idx, damroll(15, 15), &fear,
									 " is destroyed.");
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 16:
			{
				/* RF4_BO_ACID */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts an acid bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_ACID,
								 damroll(7, 8) + (rlev / 3));

				break;
			}

			case 128 + 17:
			{
				/* RF4_BO_ELEC */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a lightning bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_ELEC,
								 damroll(4, 8) + (rlev / 3));

				break;
			}

			case 128 + 18:
			{
				/* RF4_BO_FIRE */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a fire bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_FIRE,
								 damroll(9, 8) + (rlev / 3));

				break;
			}

			case 128 + 19:
			{
				/* RF4_BO_COLD */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a frost bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_COLD,
								 damroll(6, 8) + (rlev / 3));

				break;
			}

			case 128 + 20:
			{
				/* RF4_BO_POIS */

				/* XXX XXX XXX */
				break;
			}

			case 128 + 21:
			{
				/* RF4_BO_NETH */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a nether bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_NETHER,
								 30 + damroll(5, 5) + (rlev * 3) / 2);

				break;
			}

			case 128 + 22:
			{
				/* RF4_BO_WATE */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a water bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_WATER, damroll(10, 10) + rlev);

				break;
			}

			case 128 + 23:
			{
				/* RF4_BO_MANA */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a mana bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_MANA,
					damroll(r_ptr->hdice, 10));

				break;
			}

			case 128 + 24:
			{
				/* RF4_BO_PLAS */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a plasma bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_PLASMA,
								 10 + damroll(8, 7) + rlev);

				break;
			}

			case 128 + 25:
			{
				/* RF4_BO_ICEE */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts an ice bolt at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_ICE, damroll(6, 6) + rlev);

				break;
			}

			case 128 + 26:
			{
				/* RF4_MISSILE */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a magic missile at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				monst_bolt_monst(m_idx, x, y, GF_MISSILE,
								 damroll(2, 6) + (rlev / 3));

				break;
			}

			case 128 + 27:
			{
				/* RF4_SCARE */
				if (known)
				{
					if (see_either)
					{
						msgf
							("%^s casts a fearful illusion in front of %s.",
							 m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (FLAG(tr_ptr, RF_NO_FEAR))
				{
					if (see_t) msgf("%^s refuses to be frightened.",
										  t_name);
				}
				else if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_t) msgf("%^s refuses to be frightened.",
										  t_name);
				}
				else
				{
					if (!t_ptr->monfear) fear = TRUE;

					t_ptr->monfear += (byte)rand_range(4, 8);
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 28:
			{
				/* RF4_BLIND */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s casts a spell, burning %s%s eyes.",
								   m_name, t_name,
								   (streq(t_name, "it") ? "s" : "'s"));
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				/* Simulate blindness with confusion */
				if (FLAG(tr_ptr, RF_NO_CONF))
				{
					if (see_t) msgf("%^s is unaffected.", t_name);
				}
				else if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_t) msgf("%^s is unaffected.", t_name);
				}
				else
				{
					if (see_t) msgf("%^s is blinded!", t_name);

					t_ptr->confused += (byte)rand_range(12, 16);
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 29:
			{
				/* RF4_CONF */
				if (known)
				{
					if (see_either)
					{
						msgf
							("%^s casts a mesmerizing illusion in front of %s.",
							 m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (FLAG(tr_ptr, RF_NO_CONF))
				{
					if (see_t) msgf("%^s disbelieves the feeble spell.",
										  t_name);
				}
				else if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_t) msgf("%^s disbelieves the feeble spell.",
										  t_name);
				}
				else
				{
					if (see_t) msgf("%^s seems confused.", t_name);

					t_ptr->confused += (byte)rand_range(12, 16);
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 30:
			{
				/* RF4_SLOW */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s drains power from %s%s muscles.",
								   m_name, t_name,
								   (streq(t_name, "it") ? "s" : "'s"));
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (FLAG(tr_ptr, RF_UNIQUE))
				{
					if (see_t) msgf("%^s is unaffected.", t_name);
				}
				else if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_t) msgf("%^s is unaffected.", t_name);
				}
				else
				{
					if (see_t) msgf("%^s starts moving slower.", t_name);

					t_ptr->mspeed -= 10;
				}

				wake_up = TRUE;

				break;
			}

			case 128 + 31:
			{
				/* RF4_HOLD */
				if (known)
				{
					if (see_either)
					{
						msgf("%^s stares intently at %s.", m_name,
								   t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (FLAG(tr_ptr, RF_UNIQUE) ||
					FLAG(tr_ptr, RF_NO_STUN))
				{
					if (see_t) msgf("%^s is unaffected.", t_name);
				}
				else if (tr_ptr->hdice * 2 > randint1(rlev * 3) / 2)
				{
					if (see_t) msgf("%^s is unaffected.", t_name);
				}
				else
				{
					if (see_t) msgf("%^s is paralyzed!", t_name);

					t_ptr->stunned += (byte)rand_range(4, 8);
				}

				wake_up = TRUE;

				break;
			}

			case 160 + 0:
			{
				/* RF5_HASTE */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s concentrates on %s body.", m_name,
								   m_poss);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				/* Allow quick speed increases to base+10 */
				if (m_ptr->mspeed < r_ptr->speed + 10)
				{
					if (see_m) msgf("%^s starts moving faster.", m_name);

					m_ptr->mspeed += 10;
				}

				/* Allow small speed increases to base+20 */
				else if (m_ptr->mspeed < r_ptr->speed + 20)
				{
					if (see_m) msgf("%^s starts moving faster.", m_name);

					m_ptr->mspeed += 2;
				}

				break;
			}

			case 160 + 1:
			{
				/* RF5_HAND_DOOM */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s invokes the Hand of Doom upon %s!",
								   m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (FLAG(tr_ptr, RF_UNIQUE))
				{
					if (see_both) msgf("^%s is unaffected!", t_name);
				}
				else
				{
					if ((r_ptr->hdice * 2 + randint1(20)) >
						(tr_ptr->hdice * 2 + rand_range(10, 30)))
					{
						t_ptr->hp = t_ptr->hp -
							(((s32b)(rand_range(65, 90) * t_ptr->hp)) / 100);

						if (t_ptr->hp < 1) t_ptr->hp = 1;
					}
					else
					{
						if (see_both) msgf("%^s resists!", t_name);
					}
				}

				wake_up = TRUE;

				break;
			}

			case 160 + 2:
			{
				/* RF5_HEAL */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s concentrates on %s wounds.", m_name,
								   m_poss);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
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
							msgf("%^s looks completely healed!", m_name);
						}
						else
						{
							p_ptr->state.mon_fight = TRUE;
						}
					}
				}

				/* Partially healed */
				else if (known)
				{
					if (see_m)
					{
						msgf("%^s looks healthier.", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
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
					if (see_m) msgf("%^s recovers %s courage.", m_name,
										  m_poss);
				}

				break;
			}

			case 160 + 3:
			{
				/* RF5_INVULNER */
				if (known)
				{
					if (see_m)
					{
						disturb(TRUE);
						msgf("%^s casts a Globe of Invulnerability.",
								   m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (!m_ptr->invulner) m_ptr->invulner = (byte)rand_range(4, 8);

				break;
			}

			case 160 + 4:
			{
				/* RF5_BLINK */
				if (see_m)
				{
					msgf("%^s blinks away.", m_name);
				}

				(void)teleport_away(m_idx, 10);

				break;
			}

			case 160 + 5:
			{
				/* RF5_TPORT */
				if (see_m)
				{
					msgf("%^s teleports away.", m_name);
				}

				(void)teleport_away(m_idx, MAX_SIGHT * 2 + 5);

				break;
			}

			case 160 + 6:
			{
				/* RF5_XXX3X6 */
				break;
			}

			case 160 + 7:
			{
				/* RF5_XXX4X6 */
				break;
			}

			case 160 + 8:
			{
				/* RF5_TELE_TO */

				/* Not implemented */
				break;
			}

			case 160 + 9:
			{
				/* RF5_TELE_AWAY */
				bool resists_tele = FALSE;

				if (known)
				{
					if (see_either)
					{
						msgf("%^s teleports %s away.", m_name, t_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				if (FLAG(tr_ptr, RF_RES_TELE))
				{
					if (FLAG(tr_ptr, RF_UNIQUE))
					{
						if (see_t)
						{
							tr_ptr->r_flags[2] |= RF2_RES_TELE;
							msgf("%^s is unaffected!", t_name);
						}

						resists_tele = TRUE;
					}
					else if (tr_ptr->hdice * 2 > randint1(100))
					{
						if (see_t)
						{
							tr_ptr->r_flags[2] |= RF2_RES_TELE;
							msgf("%^s resists!", t_name);
						}

						resists_tele = TRUE;
					}
				}

				if (!resists_tele)
				{
					(void)teleport_away(t_idx, MAX_SIGHT * 2 + 5);
				}

				break;
			}

			case 160 + 10:
			{
				/* RF5_TELE_LEVEL */

				/* Not implemented */
				break;
			}

			case 160 + 11:
			{
				/* RF5_XXX5 */
				break;
			}

			case 160 + 12:
			{
				/* RF5_DARKNESS */
				if (known)
				{
					if (see_m)
					{
						msgf("%^s gestures in shadow.", m_name);

						if (see_t)
						{
							msgf("%^s is surrounded by darkness.",
									   t_name);
						}
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				(void)project(m_idx, 3, x, y, 0, GF_DARK_WEAK,
							  PROJECT_GRID | PROJECT_KILL);

				unlite_room(x, y);

				break;
			}

			case 160 + 13:
			{
				/* RF5_TRAPS */

				/* Not implemented */
				break;
			}

			case 160 + 14:
			{
				/* RF5_FORGET */

				/* Not implemented */
				break;
			}

			case 160 + 15:
			{
				/* RF5_RAISE_DEAD */
				if (raise_dead
					(m_ptr->fx, m_ptr->fy, (bool)(!is_hostile(m_ptr))) && known
					&& see_m)
				{
					msgf("%^s mutters quietly.", m_name);
				}
				break;
			}

			case 160 + 16:
			{
				/* RF5_SUMMON_KIN */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons %s %s.", m_name,
								   m_poss,
								   (FLAG(r_ptr, RF_UNIQUE)
									 ? "minions" : "kin"));
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				summon_kin_type = r_ptr->d_char;

				for (k = 0; k < 6; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_KIN, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 17:
			{
				/* RF5_S_CYBER */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons Cyberdemons!",
								   m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				count += summon_cyber(m_idx, x, y);

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 18:
			{
				/* RF5_S_MONSTER */
				int type = (friendly ? SUMMON_NO_UNIQUES : 0);

				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons help!", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				count +=
					summon_specific(m_idx, x, y, rlev, type, FALSE, friendly,
									pet);

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 19:
			{
				/* RF5_S_MONSTERS */
				int type = (friendly ? SUMMON_NO_UNIQUES : 0);

				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons monsters!", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 8; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, type, TRUE, friendly,
										pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 20:
			{
				/* RF5_S_ANT */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons ants.", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 6; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_ANT, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 21:
			{
				/* RF5_S_SPIDER */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons spiders.", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 6; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_SPIDER, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 22:
			{
				/* RF5_S_HOUND */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons hounds.", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 6; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_HOUND, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 23:
			{
				/* RF5_S_HYDRA */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons hydras.", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 6; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_HYDRA, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 24:
			{
				/* RF5_S_ANGEL */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons an angel!", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 1; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_ANGEL, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 25:
			{
				/* RF5_S_DEMON */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf
							("%^s magically summons a demon from the Courts of Chaos!",
							 m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 1; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_DEMON, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 26:
			{
				/* RF5_S_UNDEAD */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons undead.", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 1; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_UNDEAD, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 27:
			{
				/* RF5_S_DRAGON */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons a dragon!", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 1; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_DRAGON, TRUE,
										friendly, pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 28:
			{
				/* RF5_S_HI_UNDEAD */
				int type =
					(friendly ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_HI_UNDEAD);

				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons undead.", m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 8; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, type, TRUE, friendly,
										pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 29:
			{
				/* RF5_S_HI_DRAGON */
				int type =
					(friendly ? SUMMON_HI_DRAGON_NO_UNIQUES : SUMMON_HI_DRAGON);

				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons ancient dragons!",
								   m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 8; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, type, TRUE, friendly,
										pet);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 30:
			{
				/* RF5_S_AMBERITES */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons Lords of Amber!",
								   m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 8; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_AMBERITES,
										TRUE, FALSE, FALSE);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
				}

				break;
			}

			case 160 + 31:
			{
				/* RF5_S_UNIQUE */
				if (known)
				{
					if (see_either)
					{
						disturb(TRUE);

						msgf("%^s magically summons special opponents!",
								   m_name);
					}
					else
					{
						p_ptr->state.mon_fight = TRUE;
					}
				}

				for (k = 0; k < 8; k++)
				{
					count +=
						summon_specific(m_idx, x, y, rlev, SUMMON_UNIQUE, TRUE,
										FALSE, FALSE);
				}

				if (known && !see_t && count)
				{
					p_ptr->state.mon_fight = TRUE;
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
			flee_message(t_name, t_ptr->r_idx);
		}

		/* Remember what the monster did, if we saw it */
		if (m_ptr->ml)
		{
			/* Look to see if we've spotted a mimic */
			if (m_ptr->smart & SM_MIMIC)
			{
				/* Toggle flag */
				m_ptr->smart &= ~(SM_MIMIC);

				/* It is in the monster list now */
				update_mon_vis(m_ptr->r_idx, 1);

				/*Hack - no need for a message */
			}

			/* Inate spell */
			if (thrown_spell < 32 * 4)
			{
				r_ptr->r_flags[3] |= (1L << (thrown_spell - 32 * 3));
				if (r_ptr->r_cast_inate < MAX_UCHAR) r_ptr->r_cast_inate++;
			}

			/* Bolt or Ball */
			else if (thrown_spell < 32 * 5)
			{
				r_ptr->r_flags[4] |= (1L << (thrown_spell - 32 * 4));
				if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
			}

			/* Special spell */
			else if (thrown_spell < 32 * 6)
			{
				r_ptr->r_flags[5] |= (1L << (thrown_spell - 32 * 5));
				if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
			}
		}

		/* Always take note of monsters that kill you */
		if (p_ptr->state.is_dead && (r_ptr->r_deaths < MAX_SHORT))
		{
			r_ptr->r_deaths++;
		}

		/* A spell was cast */
		return (TRUE);
	}

	/* No enemy found */
	return (FALSE);
}
