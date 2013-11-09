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
static void monst_breath_monst(int m_idx, int y, int x, int typ, int dam_hp, int rad, bool breath, bool no_reduce)
{
	u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_MONSTER;
	int mod_elem_mode = MODIFY_ELEM_MODE_MAGIC;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	if ((rad < 1) && breath) rad = (r_ptr->flags2 & RF2_POWERFUL) ? 3 : 2;

	/* Handle breath attacks */
	if (breath) rad = 0 - rad;

	switch (typ)
	{
	case GF_ROCKET:
		flg |= (PROJECT_STOP | PROJECT_AVOIDABLE);
		mod_elem_mode = MODIFY_ELEM_MODE_FIRE;
		break;
	case GF_MIND_BLAST:
	case GF_BRAIN_SMASH:
	case GF_CAUSE_1:
	case GF_CAUSE_2:
	case GF_CAUSE_3:
	case GF_CAUSE_4:
	case GF_HAND_DOOM:
		flg |= (PROJECT_HIDE);
		break;
	}
	if (no_reduce) flg |= PROJECT_NO_REDUCE;

	(void)project(m_idx, rad, y, x, dam_hp, typ, flg, mod_elem_mode);
}


/*
 * Monster casts a bolt at another monster
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void monst_bolt_monst(int m_idx, int y, int x, int typ, int dam_hp)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_MONSTER | PROJECT_REFLECTABLE | PROJECT_AVOIDABLE;
	int mod_elem_mode = MODIFY_ELEM_MODE_MAGIC;

	switch (typ)
	{
	case GF_PHYSICAL:
	case GF_BLUNT:
	case GF_EDGED:
		flg &= ~(PROJECT_REFLECTABLE);
		mod_elem_mode = MODIFY_ELEM_MODE_FIRE;
		break;
	}

	(void)project(m_idx, 0, y, x, dam_hp, typ, flg, mod_elem_mode);
}

static void monst_beam_monst(int m_idx, int y, int x, int typ, int dam_hp)
{
	u32b flg = PROJECT_BEAM | PROJECT_KILL | PROJECT_THRU | PROJECT_MONSTER;
	int mod_elem_mode = MODIFY_ELEM_MODE_MAGIC;

	switch (typ)
	{
	case GF_PHYSICAL:
	case GF_BLUNT:
	case GF_EDGED:
		mod_elem_mode = MODIFY_ELEM_MODE_FIRE;
		break;
	}

	(void)project(m_idx, 0, y, x, dam_hp, typ, flg, mod_elem_mode);
}

static void monst_special_blow_monst(int y, int x, int m_idx, int typ, int dam_hp)
{
	u32b flg = PROJECT_HIDE | PROJECT_STOP | PROJECT_KILL | PROJECT_GRID | PROJECT_MONSTER;

	project_length = 2;

	/* Target the player with a special blow */
	(void)project(m_idx, 0, y, x, dam_hp, typ, flg, MODIFY_ELEM_MODE_MAGIC);
}

/*
 * Determine if a beam spell will hit the target.
 */
static bool direct_beam(int y1, int x1, int y2, int x2, monster_type *m_ptr)
{
	bool hit2 = FALSE;
	int i, y, x;

	int grid_n = 0;
	u16b grid_g[512];

	bool friend = is_pet(m_ptr);

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, PROJECT_THRU);

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	for (i = 0; i < grid_n; i++)
	{
		y = GRID_Y(grid_g[i]);
		x = GRID_X(grid_g[i]);

		if (y == y2 && x == x2)
			hit2 = TRUE;
		else if (friend && cave[y][x].m_idx > 0 &&
			 !are_enemies(m_ptr, &m_list[cave[y][x].m_idx]))
		{
			/* Friends don't shoot friends */
			return FALSE;
		}

		if (friend && y == py && x == px)
			return FALSE;
	}
	if (!hit2)
		return FALSE;
	return TRUE;
}

static bool breath_direct(int y1, int x1, int y2, int x2, int rad, bool disint_ball, bool friend)
{
	/* Must be the same as projectable() */

	int i, y, x;

	int grid_n = 0;
	u16b grid_g[512];

	int grids = 0;
	byte gx[2048], gy[2048];
	u16b gm[32];
	int gm_rad = rad;

	bool hit2 = FALSE;
	bool hityou = FALSE;

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, disint_ball ? PROJECT_DISI : 0);
	breath_shape(grid_g, grid_n, &grids, gx, gy, gm, &gm_rad, rad, y1, x1, y2, x2, disint_ball, FALSE);

	for (i = 0; i < grids; i++)
	{
		/* Extract the location */
		y = gy[i];
		x = gx[i];

		if (y == y2 && x == x2)
			hit2 = TRUE;
		if (y == py && x == px)
			hityou = TRUE;
	}
	if (!hit2)
		return FALSE;
	if (friend && hityou)
		return FALSE;
	return TRUE;
}

/*
 * Monster tries to 'cast a spell' (or breath, etc)
 * at another monster.
 *
 * The player is only disturbed if able to be affected by the spell.
 */
bool monst_spell_monst(int m_idx, bool target_is_decoy)
{
	int y = 0, x = 0;
	int fy = 0, fx = 0;
	int i, k, t_idx = 0;
	int thrown_spell, count = 0;
	int rlev;
	int dam = 0;
	int start;
	int plus = 1;
	u32b u_mode = 0L;
	int s_num_6 = (easy_band ? 2 : 6);
	int s_num_4 = (easy_band ? 1 : 4);

	u16b spell[96], num = 0;

	char m_name[160];
	char t_name[160];
	char m_poss[160];
	char ddesc[160];

	monster_type *m_ptr = &m_list[m_idx];
	monster_type *t_ptr = NULL;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_race *tr_ptr = NULL;

	u32b f4, f5, f6, fa;

	bool wake_up = FALSE;
	bool fear = FALSE;

	bool blind = (p_ptr->blind ? TRUE : FALSE);

	bool see_m = m_ptr->ml;
	bool see_t = FALSE;
	bool see_either;
	bool see_both;
	bool known = FALSE;

	bool pet = is_pet(m_ptr);

	bool mon_anti_magic = (m_ptr->silent || m_ptr->silent_song);

	int  do_disi_type = DO_DISI_TYPE_NONE;

	bool resists_tele = FALSE;

	/* Prepare flags for summoning */
	if (!pet) u_mode |= PM_ALLOW_UNIQUE;

	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	start = m_max + 1;

	/* Nearby anti-magic monsters */
	if (!mon_anti_magic)
	{
		mon_anti_magic = is_anti_magic_grid(m_idx, m_ptr->fy, m_ptr->fx);
	}

	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;
	fa = r_ptr->flagsa;

	if (target_is_decoy)
	{
		if (!p_ptr->use_decoy) return FALSE;
		if (!is_hostile(m_ptr)) return FALSE;

		/* Check range */
		if ((m_ptr->ddis > MAX_RANGE) && !m_ptr->target_y) return FALSE;

		y = fy = p_ptr->decoy_y;
		x = fx = p_ptr->decoy_x;

		if (!mspell_check_path(m_ptr, &y, &x, m_ptr->ddis, &do_disi_type, &f4, &f5, &f6, &fa)) return FALSE;

		if (!p_ptr->image)
#ifdef JP
			strcpy(t_name, "ダミー人形");
#else
			strcpy(t_name, "your decoy");
#endif
		/* Get the silly name */
		else monster_desc(t_name, m_ptr, 0);

		see_t = player_has_los_bold(fy, fx);
		known = (m_ptr->cdis <= MAX_SIGHT) || (distance(py, px, fy, fx) <= MAX_SIGHT);
	}
	else
	{
		/* Target is given for pet? */
		if (pet_t_m_idx && pet)
		{
			t_idx = pet_t_m_idx;
			t_ptr = &m_list[t_idx];

			/* Cancel if not projectable (for now) */
			if (!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx))
			{
				t_idx = 0;
			}
		}

		/* Is there counter attack target? */
		if (!t_idx && m_ptr->target_y)
		{
			t_idx = cave[m_ptr->target_y][m_ptr->target_x].m_idx;

			if (t_idx)
			{
				t_ptr = &m_list[t_idx];

				/* Cancel if neither enemy nor a given target */
				if (t_idx != pet_t_m_idx &&
				    !are_enemies(m_ptr, t_ptr))
				{
					t_idx = 0;
				}

				/* Allow only summoning etc.. if not projectable */
				else if (!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx))
				{
					f4 &= (RF4_INDIRECT_MASK);
					f5 &= (RF5_INDIRECT_MASK);
					f6 &= (RF6_INDIRECT_MASK);
					fa &= (RFA_INDIRECT_MASK);
				}
			}
		}

		/* Look for enemies normally */
		if (!t_idx)
		{
			/* Scan thru all monsters */
			for (i = start; ((i < start + m_max) && (i > start - m_max)); i+=plus, fy = fx = 0)
			{
				int t_dist;

				/* The monster itself isn't a target */
				int dummy = (i % m_max);
				if (!dummy) continue;

				t_idx = dummy;
				t_ptr = &m_list[t_idx];
				tr_ptr = &r_info[t_ptr->r_idx];
				y = fy = t_ptr->fy;
				x = fx = t_ptr->fx;

				/* Skip dead monsters */
				if (!t_ptr->r_idx) continue;

				/* Monster must be 'an enemy' */
				if (!are_enemies(m_ptr, t_ptr)) continue;

				t_dist = distance(m_ptr->fy, m_ptr->fx, y, x);

				/* Check range */
				if ((t_dist > MAX_RANGE) && !m_ptr->target_y) continue;

				do_disi_type = DO_DISI_TYPE_NONE;
				if (!mspell_check_path(m_ptr, &y, &x, t_dist, &do_disi_type, &f4, &f5, &f6, &fa)) continue;

				reset_target(m_ptr);

				/* Get the target's name (or "it") */
				monster_desc(t_name, t_ptr, 0x00);

				see_t = t_ptr->ml;

				/* Can the player be aware of this attack? */
				known = (m_ptr->cdis <= MAX_SIGHT) || (t_ptr->cdis <= MAX_SIGHT);

				break;
			}
		}
	}

	if (fy && fx)
	{
		/* Extract the monster level */
		rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

		/* Remove unimplemented spells */
		f4 &= ~(RF4_DISPEL);
		f6 &= ~(RF6_FORGET);

		/* Remove unimplemented special moves */
		if (f6 & RF6_SPECIAL)
		{
			if (r_ptr->d_char != 'B') f6 &= ~(RF6_SPECIAL);
		}

		if (pet)
		{
			f4 &= ~(RF4_SHRIEK);
			f6 &= ~(RF6_DARKNESS | RF6_TRAPS);

			if (!(p_ptr->pet_extra_flags & PF_TELEPORT))
			{
				f6 &= ~(RF6_BLINK | RF6_TPORT | RF6_TELE_TO | RF6_TELE_AWAY | RF6_TELE_LEVEL);
			}

			if (!(p_ptr->pet_extra_flags & PF_ATTACK_SPELL))
			{
				f4 &= ~(RF4_ATTACK_MASK);
				f5 &= ~(RF5_ATTACK_MASK);
				f6 &= ~(RF6_ATTACK_MASK);
				fa &= ~(RFA_ATTACK_MASK);
			}

			if (!(p_ptr->pet_extra_flags & PF_SUMMON_SPELL))
			{
				f4 &= ~(RF4_SUMMON_MASK);
				f5 &= ~(RF5_SUMMON_MASK);
				f6 &= ~(RF6_SUMMON_MASK);
				fa &= ~(RFA_SUMMON_MASK);
			}

			if (!(p_ptr->pet_extra_flags & PF_DISI_SPELL))
			{
				f4 &= ~(RF4_BR_DISI);
				fa &= ~(RFA_BA_DISI);
			}

			/* Prevent collateral damage */
			if (!(p_ptr->pet_extra_flags & PF_BALL_SPELL) && (m_idx != p_ptr->riding))
			{
				int real_y = y;
				int real_x = x;
				int dist;

				/* Expected breath radius */
				int breath_rad = (r_ptr->flags2 & RF2_POWERFUL) ? 3 : 2;

				/* Expected orb spell radius */
				int orb_rad = (rlev > 29) ? 3 : 2;

				get_mon_project_point(m_idx, &real_y, &real_x);

				dist = distance(real_y, real_x, py, px);

				if (los(real_y, real_x, py, px))
				{
					if (dist <= 2)
					{
						f4 &= ~(RF4_BALL_MASK);
						f5 &= ~(RF5_BALL_MASK);
						f6 &= ~(RF6_BALL_MASK);
						fa &= ~(RFA_BALL_MASK);
					}
					else if (dist <= 3)
					{
						f4 &= ~(RF4_RAD3_BALL_MASK);
						f5 &= ~(RF5_RAD3_BALL_MASK);
						f6 &= ~(RF6_RAD3_BALL_MASK);
						fa &= ~(RFA_RAD3_BALL_MASK);
					}
					else if (dist <= 4)
					{
						f4 &= ~(RF4_RAD4_BALL_MASK);
						f5 &= ~(RF5_RAD4_BALL_MASK);
						f6 &= ~(RF6_RAD4_BALL_MASK);
						fa &= ~(RFA_RAD4_BALL_MASK);
					}

					if (dist <= orb_rad)
					{
						f4 &= ~(RF4_ORB_MASK);
						f5 &= ~(RF5_ORB_MASK);
						f6 &= ~(RF6_ORB_MASK);
						fa &= ~(RFA_ORB_MASK);
					}
				}
				else if (fa & RFA_BA_DISI)
				{
					if ((dist <= 4) && in_disintegration_range(real_x, real_x, py, px))
						fa &= ~(RFA_BA_DISI);
				}

				if (!direct_beam(m_ptr->fy, m_ptr->fx, y, x, m_ptr))
				{
					f4 &= ~(RF4_BEAM_MASK | RF4_SHOOT_GUN);
					f5 &= ~(RF5_BEAM_MASK);
					f6 &= ~(RF6_BEAM_MASK);
					fa &= ~(RFA_BEAM_MASK);
				}

				if (!breath_direct(m_ptr->fy, m_ptr->fx, y, x, breath_rad, FALSE, TRUE))
				{
					f4 &= ~(RF4_BREATH_MASK);
					f5 &= ~(RF5_BREATH_MASK);
					f6 &= ~(RF6_BREATH_MASK);
					fa &= ~(RFA_BREATH_MASK);
				}
				else if (f4 & RF4_BR_DISI)
				{
					if (!breath_direct(m_ptr->fy, m_ptr->fx, y, x, breath_rad, TRUE, TRUE))
						f4 &= ~(RF4_BR_DISI);
				}

				if (fa & RFA_STONE_GAZE)
				{
					if (los(m_ptr->fy, m_ptr->fx, py, px) && los(py, px, m_ptr->fy, m_ptr->fx) &&
						(distance(m_ptr->fy, m_ptr->fx, py, px) <= MAX_SIGHT))
					{
						if (!p_ptr->resist_stone && !p_ptr->blind &&
						   !(inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].tval == TV_SHIELD)) &&
						   !(inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].tval == TV_SHIELD)))
							fa &= ~(RFA_STONE_GAZE);
					}
				}
			}

			/* Special moves restriction */
			if (f6 & RF6_SPECIAL)
			{
				switch (m_ptr->r_idx)
				{
				case MON_OZ:
				case MON_OZMA:
				case MON_VOLAC:
				case MON_MARTYM:
				case MON_BARBAS:
				case MON_BALZEPHO:
				case MON_ANDORAS:
				case MON_LANCELOT:
				case MON_DENIM:
					if (!(p_ptr->pet_extra_flags & PF_ATTACK_SPELL)) f6 &= ~(RF6_SPECIAL);
					break;

				default:
					if (r_ptr->d_char == 'B')
					{
						if (!(p_ptr->pet_extra_flags & PF_TELEPORT)) f6 &= ~(RF6_SPECIAL);
					}
					break;
				}
			}
		} /* if (pet) */

		if (mon_anti_magic && !(r_ptr->flags2 & RF2_STUPID))
		{
			f4 &= (RF4_NOMAGIC_MASK);
			f5 &= (RF5_NOMAGIC_MASK);
			f6 &= (RF6_NOMAGIC_MASK);
			fa &= (RFA_NOMAGIC_MASK);
		}

		if (p_ptr->inside_arena)
		{
			f4 &= ~(RF4_SUMMON_MASK);
			f5 &= ~(RF5_SUMMON_MASK);
			f6 &= ~(RF6_SUMMON_MASK);
			fa &= ~(RFA_SUMMON_MASK);
		}

		if (m_idx == p_ptr->riding)
		{
			f4 &= ~(RF4_RIDING_MASK);
			f5 &= ~(RF5_RIDING_MASK);
			f6 &= ~(RF6_RIDING_MASK);
			fa &= ~(RFA_RIDING_MASK);
		}

		/* Remove some spells if necessary */

		/* Check for a clean bolt shot */
		if (!(r_ptr->flags2 & RF2_STUPID) &&
		    !clean_shot(m_ptr->fy, m_ptr->fx, y, x, pet, FALSE))
		{
			f4 &= ~(RF4_BOLT_MASK);
			f5 &= ~(RF5_BOLT_MASK);
			f6 &= ~(RF6_BOLT_MASK);
			fa &= ~(RFA_BOLT_MASK);
		}

		/* Check for a possible summon */
		if (!(r_ptr->flags2 & RF2_STUPID) &&
		    !(summon_possible(m_idx, y, x)))
		{
			/* Remove summoning spells */
			f4 &= ~(RF4_SUMMON_MASK);
			f5 &= ~(RF5_SUMMON_MASK);
			f6 &= ~(RF6_SUMMON_MASK);
			fa &= ~(RFA_SUMMON_MASK);
		}

		/* Hack -- allow "desperate" spells */
		if ((r_ptr->flags2 & RF2_SMART) &&
			(m_ptr->hp < m_ptr->maxhp / 10) &&
			(randint0(100) < 50))
		{
			/* Require intelligent spells */
			f4 &= (RF4_INT_MASK);
			f5 &= (RF5_INT_MASK);
			f6 &= (RF6_INT_MASK);
			fa &= (RFA_INT_MASK);
		}

		/* No spells left */
		switch (do_disi_type)
		{
		case DO_DISI_TYPE_BREATH:
			if (!(f4 & RF4_BR_DISI)) return FALSE;
			break;
		case DO_DISI_TYPE_BALL:
			if (!(fa & RFA_BA_DISI)) return FALSE;
			break;
		default:
			if (!f4 && !f5 && !f6 && !fa) return FALSE;
			break;
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

		/* Extract the "additional" spells */
		for (k = 0; k < 32; k++)
		{
			if (fa & (1L << k)) spell[num++] = k + 32 * 9;
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

		/* Hack -- Get the "died from" name */
		monster_desc(ddesc, m_ptr, 0x88);

		/* Choose a spell to cast */
		switch (do_disi_type)
		{
		case DO_DISI_TYPE_BREATH:
			thrown_spell = 96+31;
			break;
		case DO_DISI_TYPE_BALL:
			thrown_spell = 288+23;
			break;
		default:
			thrown_spell = spell[randint0(num)];
			break;
		}

		see_either = (see_m || see_t);
		see_both = (see_m && see_t);

		if (p_ptr->riding && (m_idx == p_ptr->riding)) disturb(1, 0);

		/* Check for spell failure (inate attacks never fail) */
		if (!spell_is_inate(thrown_spell) && ((m_ptr->stunned && one_in_(2)) || mon_anti_magic))
		{
			disturb(1, 0);
			/* Message */
#ifdef JP
			if (see_m) msg_format("%^sは呪文を唱えようとしたが失敗した。", m_name);
#else
			if (see_m) msg_format("%^s tries to cast a spell, but fails.", m_name);
#endif

			return (TRUE);
		}

		switch (thrown_spell)
		{
			/* RF4_SHRIEK */
			case 96+0:
			{
				if (!target_is_decoy)
				{
					if (known)
					{
						if (see_m)
						{
#ifdef JP
							msg_format("%^sが%sに向かって叫んだ。", m_name, t_name);
#else
							msg_format("%^s shrieks at %s.", m_name, t_name);
#endif

						}
						else
						{
							mon_fight = TRUE;
						}
					}

					wake_up = TRUE;
				}
				else
				{
					disturb(1, 0);

					if (known)
					{
						if (see_m)
						{
#ifdef JP
							msg_format("%^sがかん高い金切り声をあげた。", m_name);
#else
							msg_format("%^s makes a high pitched shriek.", m_name);
#endif

						}
						else
						{
							mon_fight = TRUE;
						}
					}

					aggravate_monsters(m_idx);
				}

				break;
			}

			/* RF4_SHIFT_ELEM */
			case 96+1:
			{
				s16b old_elem = m_ptr->elem;

				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%sはシフト・エレメントの呪文を唱えた。", m_name);
#else
						msg_format("%^s casts shifting own element.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				do
				{
					m_ptr->elem = randint0(ELEM_NUM);
				}
				while (m_ptr->elem == old_elem);
				break;
			}

			/* RF4_DISPEL */
			case 96+2:
			{
				return FALSE;
			}

			/* RF4_ROCKET */
			case 96+3:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かを射った。", m_name);
#else
							msg_format("%^s shoots something.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sにロケットを発射した。", m_name, t_name);
#else
							msg_format("%^s fires a rocket at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_ROCKET);
				}

				dam = ((m_ptr->hp / 4) > 800 ? 800 : (m_ptr->hp / 4));
				monst_breath_monst(m_idx, y, x, GF_ROCKET, dam, 2, FALSE, FALSE);

				break;
			}

			/* RF4_SHOOT */
			case 96+4:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
						if (blind)
						{
#ifdef JP
							msg_format("%^sが奇妙な音を発した。", m_name);
#else
							msg_format("%^s makes a strange noise.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに矢を放った。", m_name, t_name);
#else
							msg_format("%^s fires an arrow at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_SHOOT);
				}

				dam = damroll(r_ptr->blow[0].d_dice, r_ptr->blow[0].d_side);
				monst_bolt_monst(m_idx, y, x, GF_EDGED, dam);

				break;
			}

			/* RF4_SHOOT_GUN */
			case 96+5:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
						if (blind)
						{
#ifdef JP
							msg_format("%^sが鋭い音を発した。", m_name);
#else
							msg_format("%^s makes a sharp noise.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに銃を撃った。", m_name, t_name);
#else
							msg_format("%^s shoots a gun at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_SHOOT_GUN);
				}

				dam = damroll(r_ptr->blow[0].d_dice, r_ptr->blow[0].d_side);
				monst_beam_monst(m_idx, y, x, GF_BLUNT, dam);

				break;
			}

			/* RF4_XXX3 */
			case 96+6:
			{
				/* XXX XXX XXX */
				return FALSE;
			}

			/* RF4_XXX4 */
			case 96+7:
			{
				/* XXX XXX XXX */
				return FALSE;
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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに酸のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes acid at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_ACID, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに稲妻のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes lightning at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_ELEC, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに火炎のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes fire at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_FIRE, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに冷気のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes frost at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_COLD, dam, 0, TRUE, FALSE);
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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sにガスのブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes gas at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_POIS, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに地獄のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes nether at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 550 ? 550 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_NETHER, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに閃光のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes light at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_LITE, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに暗黒のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes darkness at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_DARK, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに混乱のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes confusion at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 450 ? 450 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_CONFUSION, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに轟音のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes sound at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 450 ? 450 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_SOUND, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sにカオスのブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes chaos at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 600 ? 600 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_CHAOS, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに劣化のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes disenchantment at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_DISENCHANT, dam, 0, TRUE, FALSE);

				break;
			}

			/* RF4_BR_STON */
			case 96+20:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに石化のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes stone at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_STONE, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに時間逆転のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes time at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_TIME, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに遅鈍のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes inertia at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_INERTIA, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに重力のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes gravity at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_GRAVITY, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに破片のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes shards at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_SHARDS, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sにプラズマのブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes plasma at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 150 ? 150 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_PLASMA, dam, 0, TRUE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sにフォースのブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes force at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_FORCE, dam, 0, TRUE, FALSE);
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
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに魔力のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes mana at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_MANA, dam, 0, TRUE, FALSE);

				break;
			}

			/* RF4_BA_NUKE */
			case 96+28:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに放射能球を放った。", m_name, t_name);
#else
							msg_format("%^s casts a ball of radiation at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev + damroll(10, 6));
				monst_breath_monst(m_idx, y, x, GF_NUKE, dam, 2, FALSE, FALSE);

				break;
			}

			/* RF4_BR_NUKE */
			case 96+29:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに放射性廃棄物のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes toxic waste at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_NUKE, dam, 0, TRUE, FALSE);
				break;
			}

			/* RF4_BA_CHAO */
			case 96+30:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが恐ろしげにつぶやいた。", m_name);
#else
							msg_format("%^s mumbles frighteningly.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに純粋なカオスを放った。", m_name, t_name);
#else
							msg_format("%^s invokes a raw chaos upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 2) + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_CHAOS, dam, 4, FALSE, FALSE);

				break;
			}

			/* RF4_BR_DISI */
			case 96+31:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに分解のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes disintegration at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 6) > 150 ? 150 : (m_ptr->hp / 6));
				monst_breath_monst(m_idx, y, x, GF_DISINTEGRATE, dam, 0, TRUE, FALSE);
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
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かってアシッドクラウドの呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts an acid cloud at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (randint1(rlev * 3) + 15) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_breath_monst(m_idx, y, x, GF_ACID, dam, 2, FALSE, TRUE);

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
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かってサンダーフレアの呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts a thunder flare at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (randint1(rlev * 3 / 2) + 8) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_breath_monst(m_idx, y, x, GF_ELEC, dam, 2, FALSE, TRUE);

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
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かってファイアストームの呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts a fire storm at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (randint1(rlev * 7 / 2) + 10) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_breath_monst(m_idx, y, x, GF_FIRE, dam, 2, FALSE, TRUE);

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
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かってアイスブラストの呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts an ice blast at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (randint1(rlev * 3 / 2) + 10) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_breath_monst(m_idx, y, x, GF_COLD, dam, 2, FALSE, TRUE);

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
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かって悪臭雲の呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts a stinking cloud at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(12, 2) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_breath_monst(m_idx, y, x, GF_POIS, dam, 2, FALSE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かって地獄球の呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts a nether ball at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = 50 + damroll(10, 10) + (rlev * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1));
				monst_breath_monst(m_idx, y, x, GF_NETHER, dam, 2, FALSE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対して流れるような身振りをした。", m_name, t_name);
#else
							msg_format("%^s gestures fluidly at %s.", m_name, t_name);
#endif

#ifdef JP
							msg_format("%^sは渦巻に飲み込まれた。", t_name);
#else
							msg_format("%^s is engulfed in a whirlpool.", t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = ((r_ptr->flags2 & RF2_POWERFUL) ? randint1(rlev * 3) : randint1(rlev * 2)) + 50;
				monst_breath_monst(m_idx, y, x, GF_WATER, dam, 4, FALSE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
							msg_format("%^s mumbles powerfully.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対して魔力の嵐の呪文を念じた。", m_name, t_name);
#else
							msg_format("%^s invokes a mana storm upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 4) + 50 + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_MANA, dam, 4, FALSE, FALSE);

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
#ifdef JP
							msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
							msg_format("%^s mumbles powerfully.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対して暗黒の嵐の呪文を念じた。", m_name, t_name);
#else
							msg_format("%^s invokes a darkness storm upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 4) + 50 + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_DARK, dam, 4, FALSE, FALSE);

				break;
			}

			/* RF5_DRAIN_MANA */
			case 128+9:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (see_m)
				{
					/* Basic message */
#ifdef JP
					msg_format("%^sは精神エネルギーを%sから吸いとった。", m_name, t_name);
#else
					msg_format("%^s draws psychic energy from %s.", m_name, t_name);
#endif

				}

				if (!target_is_decoy)
				{
					/* Heal the monster */
					if (m_ptr->hp < m_ptr->maxhp)
					{
						if (!tr_ptr->flags4 && !tr_ptr->flags5 && !tr_ptr->flags6 && !tr_ptr->flagsa)
						{
							if (see_both)
							{
#ifdef JP
								msg_format("%^sには効果がなかった。", t_name);
#else
								msg_format("%^s is unaffected!", t_name);
#endif

							}
						}
						else
						{
							/* Attack power */
							int power = (randint1(rlev) / 2) + 1;

							/* Heal */
							m_ptr->hp += 6 * power;
							if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

							/* Redraw (later) if needed */
							if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
							if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

							/* Special message */
							if (see_m)
							{
#ifdef JP
								msg_format("%^sは気分が良さそうだ。", m_name);
#else
								msg_format("%^s appears healthier.", m_name);
#endif

							}
						}
					}

					wake_up = TRUE;
				}

				break;
			}

			/* RF5_MIND_BLAST */
			case 128+10:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (see_m)
				{
#ifdef JP
					msg_format("%^sは%sをじっと睨んだ。", m_name, t_name);
#else
					msg_format("%^s gazes intently at %s.", m_name, t_name);
#endif

				}

				dam = damroll(7, 7);
				monst_breath_monst(m_idx, y, x, GF_MIND_BLAST, dam, 0, FALSE, FALSE);

				break;
			}

			/* RF5_BRAIN_SMASH */
			case 128+11:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (see_m)
				{
#ifdef JP
					msg_format("%^sは%sをじっと睨んだ。", m_name, t_name);
#else
					msg_format("%^s gazes intently at %s.", m_name, t_name);
#endif

				}

				dam = damroll(12, 12);
				monst_breath_monst(m_idx, y, x, GF_BRAIN_SMASH, dam, 0, FALSE, FALSE);

				break;
			}

			/* RF5_CAUSE_1 */
			case 128+12:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sは%sを指さして呪いをかけた。", m_name, t_name);
#else
						msg_format("%^s points at %s and curses.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(3, 8);
				monst_breath_monst(m_idx, y, x, GF_CAUSE_1, dam, 0, FALSE, FALSE);

				break;
			}

			/* RF5_CAUSE_2 */
			case 128+13:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sは%sを指さして恐ろしげに呪いをかけた。", m_name, t_name);
#else
						msg_format("%^s points at %s and curses horribly.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(8, 8);
				monst_breath_monst(m_idx, y, x, GF_CAUSE_2, dam, 0, FALSE, FALSE);

				break;
			}

			/* RF5_CAUSE_3 */
			case 128+14:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sは%sを指さし、恐ろしげに呪文を唱えた！", m_name, t_name);
#else
						msg_format("%^s points at %s, incanting terribly!", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(10, 15);
				monst_breath_monst(m_idx, y, x, GF_CAUSE_3, dam, 0, FALSE, FALSE);

				break;
			}

			/* RF5_CAUSE_4 */
			case 128+15:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sが%sを指差し、「死ね！」と叫んだ。", m_name, t_name);
#else
						msg_format("%^s points at %s, screaming the word, 'DIE!'", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(15, 15);
				monst_breath_monst(m_idx, y, x, GF_CAUSE_4, dam, 0, FALSE, FALSE);

				break;
			}

			/* RF5_BO_ACID */
			case 128+16:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%sが%sに向かってアシッド・ボルトの呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts an acid bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (damroll(7, 8) + (rlev / 3)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_bolt_monst(m_idx, y, x, GF_ACID, dam);

				break;
			}

			/* RF5_BO_ELEC */
			case 128+17:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かってサンダー・ボルトの呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts a lightning bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (damroll(4, 8) + (rlev / 3)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_bolt_monst(m_idx, y, x, GF_ELEC, dam);

				break;
			}

			/* RF5_BO_FIRE */
			case 128+18:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かってファイア・ボルトの呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts a fire bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (damroll(9, 8) + (rlev / 3)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_bolt_monst(m_idx, y, x, GF_FIRE, dam);

				break;
			}

			/* RF5_BO_COLD */
			case 128+19:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かってアイス・ボルトの呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts a frost bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (damroll(6, 8) + (rlev / 3)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
				monst_bolt_monst(m_idx, y, x, GF_COLD, dam);

				break;
			}

			/* RF5_BA_LITE */
			case 128+20:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
							msg_format("%^s mumbles powerfully.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対してスターバーストの呪文を念じた。", m_name, t_name);
#else
							msg_format("%^s invokes a starburst upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 4) + 50 + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_LITE, dam, 4, FALSE, FALSE);

				break;
			}

			/* RF5_BO_NETH */
			case 128+21:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かって地獄の矢の呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts a nether bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = 30 + damroll(5, 5) + (rlev * 4) / ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 3);
				monst_bolt_monst(m_idx, y, x, GF_NETHER, dam);

				break;
			}

			/* RF5_BO_WATE */
			case 128+22:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かってウォーター・ボルトの呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts a water bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(10, 10) + (rlev * 3 / ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 3));
				monst_bolt_monst(m_idx, y, x, GF_WATER, dam);

				break;
			}

			/* RF5_BO_MANA */
			case 128+23:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かって魔力の矢の呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts a mana bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = randint1(rlev * 7 / 2) + 50;
				monst_bolt_monst(m_idx, y, x, GF_MANA, dam);

				break;
			}

			/* RF5_BO_PLAS */
			case 128+24:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かってプラズマ・ボルトの呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts a plasma bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = 10 + damroll(8, 7) + (rlev * 3 / ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 3));
				monst_bolt_monst(m_idx, y, x, GF_PLASMA, dam);

				break;
			}

			/* RF5_BO_ICEE */
			case 128+25:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かって極寒の矢の呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts an ice bolt at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(6, 6) + (rlev * 3 / ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 3));
				monst_bolt_monst(m_idx, y, x, GF_ICE, dam);

				break;
			}

			/* RF5_MISSILE */
			case 128+26:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かってマジック・ミサイルの呪文を唱えた。", m_name, t_name);
#else
						msg_format("%^s casts a magic missile at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(2, 6) + (rlev / 3);
				monst_bolt_monst(m_idx, y, x, GF_MISSILE, dam);

				break;
			}

			/* RF5_SCARE */
			case 128+27:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが恐ろしげな幻覚を作り出した。", m_name, t_name);
#else
						msg_format("%^s casts a fearful illusion in front of %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!target_is_decoy)
				{
					if (tr_ptr->flags3 & RF3_NO_FEAR)
					{
#ifdef JP
						if (see_t) msg_format("%^sは恐怖を感じない。", t_name);
#else
						if (see_t) msg_format("%^s refuses to be frightened.", t_name);
#endif

					}
					else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
					{
#ifdef JP
						if (see_t) msg_format("%^sは恐怖を感じない。", t_name);
#else
						if (see_t) msg_format("%^s refuses to be frightened.", t_name);
#endif

					}
					else
					{
						if (!t_ptr->monfear) fear = TRUE;

						t_ptr->monfear += randint0(4) + 4;
					}

					wake_up = TRUE;
				}

				break;
			}

			/* RF5_BLIND */
			case 128+28:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%sは呪文を唱えて%sの目を焼き付かせた。", m_name, t_name);
#else
						msg_format("%^s casts a spell, burning %s%s eyes.", m_name, t_name,
									  (streq(t_name, "it") ? "s" : "'s"));
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!target_is_decoy)
				{
					/* Simulate blindness with confusion */
					if (tr_ptr->flags3 & RF3_NO_CONF)
					{
#ifdef JP
						if (see_t) msg_format("%^sには効果がなかった。", t_name);
#else
						if (see_t) msg_format("%^s is unaffected.", t_name);
#endif

					}
					else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
					{
#ifdef JP
						if (see_t) msg_format("%^sには効果がなかった。", t_name);
#else
						if (see_t) msg_format("%^s is unaffected.", t_name);
#endif

					}
					else
					{
#ifdef JP
						if (see_t)   msg_format("%^sは目が見えなくなった！ ", t_name);
#else
						if (see_t) msg_format("%^s is blinded!", t_name);
#endif


						t_ptr->confused += 12 + (byte)randint0(4);
					}

					wake_up = TRUE;
				}

				break;
			}

			/* RF5_CONF */
			case 128+29:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sの前に幻惑的な幻をつくり出した。", m_name, t_name);
#else
						msg_format("%^s casts a mesmerizing illusion in front of %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!target_is_decoy)
				{
					if (tr_ptr->flags3 & RF3_NO_CONF)
					{
#ifdef JP
						if (see_t) msg_format("%^sは惑わされなかった。", t_name);
#else
						if (see_t) msg_format("%^s disbelieves the feeble spell.", t_name);
#endif

					}
					else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
					{
#ifdef JP
						if (see_t) msg_format("%^sは惑わされなかった。", t_name);
#else
						if (see_t) msg_format("%^s disbelieves the feeble spell.", t_name);
#endif

					}
					else
					{
#ifdef JP
						if (see_t) msg_format("%^sは混乱したようだ。", t_name);
#else
						if (see_t) msg_format("%^s seems confused.", t_name);
#endif


						t_ptr->confused += 12 + (byte)randint0(4);
					}

					wake_up = TRUE;
				}

				break;
			}

			/* RF5_SLOW */
			case 128+30:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%sが%sの筋肉から力を吸いとった。", m_name, t_name);
#else
						msg_format("%^s drains power from %s%s muscles.", m_name, t_name,
									  (streq(t_name, "it") ? "s" : "'s"));
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!target_is_decoy)
				{
					if (tr_ptr->flags1 & RF1_UNIQUE)
					{
#ifdef JP
						if (see_t) msg_format("%^sには効果がなかった。", t_name);
#else
						if (see_t) msg_format("%^s is unaffected.", t_name);
#endif

					}
					else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
					{
#ifdef JP
						if (see_t) msg_format("%^sには効果がなかった。", t_name);
#else
						if (see_t) msg_format("%^s is unaffected.", t_name);
#endif

					}
					else
					{
						if (!t_ptr->slow)
						{
#ifdef JP
						if (see_t) msg_format("%sの動きが遅くなった。", t_name);
#else
						if (see_t) msg_format("%^s starts moving slower.", t_name);
#endif
						}

						t_ptr->slow = MIN(200, t_ptr->slow + 50);
					}

					wake_up = TRUE;
				}

				break;
			}

			/* RF5_HOLD */
			case 128+31:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sは%sをじっと見つめた。", m_name, t_name);
#else
						msg_format("%^s stares intently at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!target_is_decoy)
				{
					if ((tr_ptr->flags1 & RF1_UNIQUE) ||
						 (tr_ptr->flags3 & RF3_NO_STUN))
					{
#ifdef JP
						if (see_t) msg_format("%^sには効果がなかった。", t_name);
#else
						if (see_t) msg_format("%^s is unaffected.", t_name);
#endif

					}
					else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
					{
#ifdef JP
						if (see_t) msg_format("%^sには効果がなかった。", t_name);
#else
						if (see_t) msg_format("%^s is unaffected.", t_name);
#endif

					}
					else
					{
#ifdef JP
						if (see_t) msg_format("%^sは麻痺した！", t_name);
#else
						if (see_t) msg_format("%^s is paralyzed!", t_name);
#endif


						t_ptr->stunned += randint1(4) + 4;
					}

					wake_up = TRUE;
				}

				break;
			}


			/* RF6_HASTE */
			case 160+0:
			{
				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sが自分の体に念を送った。", m_name, m_poss);
#else
						msg_format("%^s concentrates on %s body.", m_name, m_poss);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				/* Allow quick speed increases to base+10 */
				if (!m_ptr->fast)
				{
#ifdef JP
					if (see_m) msg_format("%^sの動きが速くなった。", m_name);
#else
					if (see_m) msg_format("%^s starts moving faster.", m_name);
#endif

				}
				m_ptr->fast = MIN(200, m_ptr->fast + 100);
				if (p_ptr->riding == m_idx) p_ptr->update |= PU_BONUS;
				break;
			}

			/* RF6_HAND_DOOM */
			case 160+1:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sが%sに<破滅の手>を放った！", m_name, t_name);
#else
						msg_format("%^s invokes the Hand of Doom upon %s!", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = rlev + randint1(20);
				monst_breath_monst(m_idx, y, x, GF_HAND_DOOM, dam, 0, FALSE, FALSE);

				break;
			}

			/* RF6_HEAL */
			case 160+2:
			{
				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sは自分の傷に念を集中した。", m_name);
#else
						msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
#endif

					}
					else
					{
						mon_fight = TRUE;
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
#ifdef JP
							msg_format("%^sは完全に治った！", m_name);
#else
							msg_format("%^s looks completely healed!", m_name);
#endif

						}
						else
						{
							mon_fight = TRUE;
						}
					}
				}

				/* Partially healed */
				else if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sは体力を回復したようだ。", m_name);
#else
						msg_format("%^s looks healthier.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				/* Redraw (later) if needed */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
				if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

				/* Cancel fear */
				if (m_ptr->monfear)
				{
					/* Cancel fear */
					m_ptr->monfear = 0;

					/* Message */
#ifdef JP
					if (see_m) msg_format("%^sは勇気を取り戻した。", m_name);
#else
					if (see_m) msg_format("%^s recovers %s courage.", m_name, m_poss);
#endif

				}

				break;
			}

			/* RF6_INVULNER */
			case 160+3:
			{
				if (known)
				{
					if (see_m)
					{
						disturb(1, 0);
#ifdef JP
						msg_format("%sは無傷の球の呪文を唱えた。", m_name);
#else
						msg_format("%^s casts a Globe of Invulnerability.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!m_ptr->invulner) m_ptr->invulner = randint1(4) + 4;

				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
				if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);
				break;
			}

			/* RF6_BLINK */
			case 160+4:
			{
				if (see_m)
				{
#ifdef JP
					msg_format("%^sが瞬時に消えた。", m_name);
#else
					msg_format("%^s blinks away.", m_name);
#endif

				}

				teleport_away(m_idx, 10);

				break;
			}

			/* RF6_TPORT */
			case 160+5:
			{
				if (see_m)
				{
#ifdef JP
					msg_format("%^sがテレポートした。", m_name);
#else
					msg_format("%^s teleports away.", m_name);
#endif

				}

				teleport_away(m_idx, MAX_SIGHT * 2 + 5);

				if (los(py, px, m_ptr->fy, m_ptr->fx) && !stop_the_time_monster && see_m)
				{
					for (i=INVEN_RARM;i<INVEN_TOTAL;i++)
					{
						u32b flgs[TR_FLAG_SIZE];
						object_type *o_ptr = &inventory[i];

						if (cursed_p(o_ptr)) continue;
						object_flags(o_ptr, flgs);

						if (have_flag(flgs, TR_TELEPORT) || (p_ptr->muta1 & MUT1_VTELEPORT))
						{
#ifdef JP
							cptr msg = "ついていきますか？";
#else
							cptr msg = "Do you follow it? ";
#endif
							if(get_check_strict(msg, CHECK_OKAY_CANCEL))
							{
								if (one_in_(3))
								{
									teleport_player(200);
#ifdef JP
									msg_print("失敗！");
#else
									msg_print("Failed!");
#endif
								}
								else teleport_player_to(m_ptr->fy, m_ptr->fx, TRUE, TRUE);
								p_ptr->energy_need = ENERGY_NEED();
							}
							break;
						}
					}
				}
				break;
			}

			/* RF6_STOP_TIME */
			case 160+6:
			{
				return FALSE;
			}

			/* RF6_SPECIAL */
			case 160+7:
			{
				cave_type *c_ptr = &cave[y][x];

				/* if (p_ptr->inside_arena) return FALSE; */
				switch (m_ptr->r_idx)
				{
				case MON_OZ:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「貴様のツラは見飽きたよ…。終わりにしようぜ！ブラックプリズン！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_DARK, dam);
					if (c_ptr->m_idx) monst_special_blow_monst(y, x, m_idx, GF_STASIS, rlev + 100);
					break;

				case MON_OZMA:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「根性だけは認めてあげるわ…。でも、そこまでよ、デーモンローズ！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_PURE_FIRE, dam);
					if (c_ptr->m_idx) monst_special_blow_monst(y, x, m_idx, GF_OLD_CONF, rlev + 100);
					break;

				case MON_VOLAC:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「ロスローリアンの力をなめてもらっては困るのだよ…。ライアットバーン！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_HOLY_FIRE, dam);
					break;

				case MON_MARTYM:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「このオレとサシで戦おうなんざ、10年早ぇんだよ！バカめッ！フローヴェノム！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_WATER, dam);
					break;

				case MON_BARBAS:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「このオレの奥義が見たいか！？いい度胸だ。デスアベンジャー！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_PURE_EARTH, dam);
					if (c_ptr->m_idx) knock_back(m_idx, y, x, 200);
					break;

				case MON_BALZEPHO:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「真の騎士の誇りを貴様に見せてやろう！くらえッ！フレイミングデス！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_PURE_FIRE, dam);
					break;

				case MON_ANDORAS:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「フン！命知らずの愚か者めッ！我が奥義を防げるかな？いくぞッ！サンダーブレイド！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_ELEC, dam);
					break;

				case MON_LANCELOT:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「ローディスに逆らう愚か者め…。我が奥義を受けてみよ！アポカリプス！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_OLD_DRAIN, dam);
					break;

				case MON_DENIM:
					if ((x != fx) || (y != fy)) return FALSE;
					if ((distance(m_ptr->fy, m_ptr->fx, fy, fx) > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, fy, fx, pet, TRUE)) return FALSE;
					if (known)
					{
						if (see_m)
						{
							msg_format("%s「僕は負けるわけにはいかないッ！くらえ！神鳴明王剣！」", m_name);
						}
						else
						{
							mon_fight = TRUE;
						}
					}
					dam = 3 * rlev + 100;
					monst_special_blow_monst(y, x, m_idx, GF_GODLY_SPEAR, dam);
					break;

				default:
					if (r_ptr->d_char == 'B')
					{
						if (one_in_(3) || (x != fx) || (y != fy))
						{
							if (see_m)
							{
#ifdef JP
								msg_format("%^sは突然急上昇して視界から消えた!", m_name);
#else
								msg_format("%^s suddenly go out of your sight!", m_name);
#endif
							}
							teleport_away(m_idx, 10);
							p_ptr->update |= (PU_MONSTERS);
						}
						else
						{
							if (known)
							{
								if (see_either)
								{
#ifdef JP
									msg_format("%^sが%sを掴んで空中から投げ落した。", m_name, t_name);
#else
									msg_format("%^s holds %s, and drops from the sky.", m_name, t_name);
#endif

								}
								else
								{
									mon_fight = TRUE;
								}
							}

							if (!target_is_decoy)
							{
								int dam1;

								dam = damroll(4, 8);

								if (t_idx == p_ptr->riding) teleport_player_to(m_ptr->fy, m_ptr->fx, FALSE, FALSE);
								else teleport_to(t_idx, m_ptr->fy, m_ptr->fx, 1, 100, FALSE);

								see_t = t_ptr->ml;

								if (!t_ptr->r_idx) break;

								if (!p_ptr->leaving)
								{
									sound(SOUND_FALL);

									if (cave[t_ptr->fy][t_ptr->fx].feat != FEAT_AIR)
									{
										if (tr_ptr->flags7 & RF7_CAN_FLY)
										{
#ifdef JP
											if (see_t) msg_format("%^sは静かに着地した。", t_name);
#else
											if (see_t) msg_format("%^s float gently down to the ground.", t_name);
#endif
										}
										else
										{
#ifdef JP
											if (see_t) msg_format("%^sは地面に叩きつけられた。", t_name);
#else
											if (see_t) msg_format("%^s crashed into the ground.", t_name);
#endif
											dam += damroll(6, 8);
										}
									}
								}

								dam1 = modify_dam_by_elem(m_idx, t_idx, dam, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);
								mon_take_hit_mon(FALSE, t_idx, dam1, &fear, extract_note_dies(tr_ptr), m_idx);

								if (t_idx == p_ptr->riding)
								{
									int dam2 = modify_dam_by_elem(m_idx, 0, dam, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);
									int get_damage = take_hit(DAMAGE_NOESCAPE, dam2, m_name);

									if (p_ptr->tim_eyeeye && get_damage > 0 && !p_ptr->is_dead && !p_ptr->leaving)
									{
#ifdef JP
										msg_format("攻撃が%s自身を傷つけた！", m_name);
#else
										char m_name_self[80];

										/* hisself */
										monster_desc(m_name_self, m_ptr, 0x23);

										msg_format("The attack of %s has wounded %s!", m_name, m_name_self);
#endif
										project(0, 0, m_ptr->fy, m_ptr->fx, get_damage, GF_MISSILE, PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
										set_tim_eyeeye(p_ptr->tim_eyeeye-5, TRUE);
									}
								}
							}
							else
							{
								sound(SOUND_FALL);
								break_decoy();
							}
						}
						break;
					}

					/* Something is wrong */
					else return FALSE;
				}

				break;
			}

			/* RF6_TELE_TO */
			case 160+8:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sを引き戻した。", m_name, t_name);
#else
						msg_format("%^s commands %s to return.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!target_is_decoy)
				{
					if (tr_ptr->flags3 & RF3_RES_TELE)
					{
						if (tr_ptr->flags1 & RF1_UNIQUE)
						{
							if (see_t)
							{
								tr_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
								msg_format("%^sには効果がなかった。", t_name);
#else
								msg_format("%^s is unaffected!", t_name);
#endif

							}

							resists_tele = TRUE;
						}
						else if (tr_ptr->level > randint1(100))
						{
							if (see_t)
							{
								tr_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
								msg_format("%^sは耐性を持っている！", t_name);
#else
								msg_format("%^s resists!", t_name);
#endif

							}

							resists_tele = TRUE;
						}
					}

					if (!resists_tele)
					{
						if (t_idx == p_ptr->riding)
						{
							if (p_ptr->earth_spike)
#ifdef JP
								msg_print("しかし効力を跳ね返した！");
#else
								msg_print("You resist the effects!");
#endif
							else teleport_player_to(m_ptr->fy, m_ptr->fx, TRUE, FALSE);
						}
						else teleport_to(t_idx, m_ptr->fy, m_ptr->fx, 1, 100, FALSE);
					}

					wake_up = TRUE;
				}

				break;
			}

			/* RF6_TELE_AWAY */
			case 160+9:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sは%sをテレポートさせた。", m_name, t_name);
#else
						msg_format("%^s teleports %s away.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!target_is_decoy)
				{
					if (tr_ptr->flags3 & RF3_RES_TELE)
					{
						if (tr_ptr->flags1 & RF1_UNIQUE)
						{
							if (see_t)
							{
								tr_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
								msg_format("%^sには効果がなかった。", t_name);
#else
								msg_format("%^s is unaffected!", t_name);
#endif

							}

							resists_tele = TRUE;
						}
						else if (tr_ptr->level > randint1(100))
						{
							if (see_t)
							{
								tr_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
								msg_format("%^sは耐性を持っている！", t_name);
#else
								msg_format("%^s resists!", t_name);
#endif

							}

							resists_tele = TRUE;
						}
					}

					if (!resists_tele)
					{
						if (t_idx == p_ptr->riding)
						{
							if (p_ptr->earth_spike)
#ifdef JP
								msg_print("しかし効力を跳ね返した！");
#else
								msg_print("You resist the effects!");
#endif
							else teleport_player(MAX_SIGHT * 2 + 5);
						}
						else teleport_away(t_idx, MAX_SIGHT * 2 + 5);
					}

					wake_up = TRUE;
				}

				break;
			}

			/* RF6_TELE_LEVEL */
			case 160+10:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sの足を指さした。", m_name, t_name);
#else
						msg_format("%^s gestures at %s's feet.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (!target_is_decoy)
				{
					if (tr_ptr->flags3 & RF3_RES_TELE)
					{
						if (tr_ptr->flags1 & RF1_UNIQUE)
						{
							if (see_t)
							{
								tr_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
								msg_format("%^sには効果がなかった。", t_name);
#else
								msg_format("%^s is unaffected!", t_name);
#endif

							}

							resists_tele = TRUE;
						}
						else if (tr_ptr->level > randint1(100))
						{
							if (see_t)
							{
								tr_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
								msg_format("%^sは耐性を持っている！", t_name);
#else
								msg_format("%^s resists!", t_name);
#endif

							}

							resists_tele = TRUE;
						}
					}

					if (!resists_tele)
					{
						if ((tr_ptr->flags1 & RF1_QUESTOR) ||
						    (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10) ||
						    ((t_idx == p_ptr->riding) && p_ptr->earth_spike))
						{
							if (see_t)
							{
#ifdef JP
								msg_format("%^sは効力を跳ね返した！", t_name);
#else
								msg_format("%^s resist the effects!", t_name);
#endif
							}
						}
						else teleport_level((t_idx == p_ptr->riding) ? 0 : t_idx);
					}

					wake_up = TRUE;
				}

				break;
			}

			/* RF6_GODLY_SPEAR */
			case 160+11:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かって神の槍を放った。", m_name, t_name);
#else
						msg_format("%^s throws a godly spear at %s.", m_name, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (r_ptr->flags2 & RF2_POWERFUL) ? (randint1(rlev * 2) + 180) : (randint1(rlev * 3 / 2) + 120);
				monst_beam_monst(m_idx, y, x, GF_GODLY_SPEAR, dam);
				break;
			}

			/* RF6_DARKNESS */
			case 160+12:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (!target_is_decoy || ((p_ptr->pclass != CLASS_NINJA) && (p_ptr->pclass != CLASS_NINJAMASTER)))
				{
					if (known)
					{
						if (see_m)
						{
#ifdef JP
							msg_format("%^sが暗闇の中で手を振った。", m_name);
#else
							msg_format("%^s gestures in shadow.", m_name);
#endif


							if (see_t)
							{
#ifdef JP
								msg_format("%^sは暗闇に包まれた。", t_name);
#else
								msg_format("%^s is surrounded by darkness.", t_name);
#endif

							}
						}
						else
						{
							mon_fight = TRUE;
						}
					}

					(void)project(m_idx, 3, y, x, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL | PROJECT_MONSTER, MODIFY_ELEM_MODE_MAGIC);

					unlite_room(y, x);
				}
				else /* Decoy of Ninja */
				{
					if (known)
					{
						if (see_m)
						{
#ifdef JP
							msg_format("%^sが辺りを明るく照らした。", m_name);
#else
							msg_format("%^s cast a spell to light up.", m_name);
#endif


							if (see_t)
							{
#ifdef JP
								msg_format("%^sは光に包まれた。", t_name);
#else
								msg_format("%^s is surrounded by light.", t_name);
#endif

							}
						}
						else
						{
							mon_fight = TRUE;
						}
					}

					(void)project(m_idx, 3, y, x, 0, GF_LITE_WEAK, PROJECT_GRID | PROJECT_KILL | PROJECT_MONSTER, MODIFY_ELEM_MODE_MAGIC);

					lite_room(y, x);
				}

				break;
			}

			/* RF6_TRAPS */
			case 160+13:
			{
#if 0
				if (known)
				{
					if (see_m)
					{
#ifdef JP
						msg_format("%^sが呪文を唱えて邪悪に微笑んだ。", m_name);
#else
						msg_format("%^s casts a spell and cackles evilly.", m_name);
#endif
					}
					else
					{
#ifdef JP
						msg_format("%^sが何かをつぶやいた。", m_name);
#else
						msg_format("%^s mumbles.", m_name);
#endif
					}
				}

				trap_creation(y, x);

				break;
#else
				return FALSE;
#endif
			}

			/* RF6_FORGET */
			case 160+14:
			{
				/* Not implemented */
				return FALSE;
			}

			/* RF6_RAISE_DEAD */
			case 160+15:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
#ifdef JP
						if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
						if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
						else msg_format("%^sが死者復活の呪文を唱えた。", m_name);
#else
						else msg_format("%^s casts a spell to revive corpses.", m_name);
#endif
					}
					else
					{
						mon_fight = TRUE;
					}
				}
				animate_dead(m_idx, m_ptr->fy, m_ptr->fx);
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

#ifdef JP
						msg_format("%sが魔法で%sを召喚した。", m_name,
								  ((r_ptr->flags1 & RF1_UNIQUE) ? "手下" : "仲間"));
#else
						msg_format("%^s magically summons %s %s.", m_name, m_poss,
								  ((r_ptr->flags1 & RF1_UNIQUE) ? "minions" : "kin"));
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				switch (m_ptr->r_idx)
				{
				case MON_THORONDOR:
				case MON_GWAIHIR:
				case MON_MENELDOR:
					{
						int num = 4 + randint1(3);
						for (k = 0; k < num; k++)
						{
							count += summon_specific(m_idx, y, x, rlev, SUMMON_EAGLES, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
						}
					}
					break;
				case MON_ELEM_L_FIRE:
					{
						int num = 4 + randint1(3);
						for (k = 0; k < num; k++)
						{
							count += summon_specific(m_idx, y, x, rlev, SUMMON_FIRE, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
						}
					}
					break;
				case MON_ELEM_L_AQUA:
					{
						int num = 4 + randint1(3);
						for (k = 0; k < num; k++)
						{
							count += summon_specific(m_idx, y, x, rlev, SUMMON_AQUA, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
						}
					}
					break;
				case MON_ELEM_L_EARTH:
					{
						int num = 4 + randint1(3);
						for (k = 0; k < num; k++)
						{
							count += summon_specific(m_idx, y, x, rlev, SUMMON_EARTH, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
						}
					}
					break;
				case MON_ELEM_L_WIND:
					{
						int num = 4 + randint1(3);
						for (k = 0; k < num; k++)
						{
							count += summon_specific(m_idx, y, x, rlev, SUMMON_WIND, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
						}
					}
					break;
				default:
					{
						summon_kin_type = r_ptr->d_char;

						for (k = 0; k < 4; k++)
						{
							count += summon_specific(m_idx, y, x, rlev, SUMMON_KIN, (PM_ALLOW_GROUP));
						}
					}
					break;
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
				}

				break;
			}

			/* RF6_S_CYBER */
			case 160+17:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

#ifdef JP
						msg_format("%^sがサイバーデーモンを召喚した！", m_name);
#else
						msg_format("%^s magically summons Cyberdemons!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (is_friendly(m_ptr))
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_CYBER, (PM_ALLOW_GROUP));
				}
				else
				{
					count += summon_cyber(m_idx, y, x);
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%^sが魔法で仲間を召喚した！", m_name);
#else
						msg_format("%^s magically summons help!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				count += summon_specific(m_idx, y, x, rlev, 0, (u_mode));

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
				}

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

#ifdef JP
						msg_format("%^sが魔法でモンスターを召喚した！", m_name);
#else
						msg_format("%^s magically summons monsters!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_6; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, 0, (PM_ALLOW_GROUP | u_mode));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%^sが魔法でアリを召喚した。", m_name);
#else
						msg_format("%^s magically summons ants.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_6; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_ANT, (PM_ALLOW_GROUP));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%^sが魔法でクモを召喚した。", m_name);
#else
						msg_format("%^s magically summons spiders.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_6; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_SPIDER, (PM_ALLOW_GROUP));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%^sが魔法でハウンドを召喚した。", m_name);
#else
						msg_format("%^s magically summons hounds.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_4; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_HOUND, (PM_ALLOW_GROUP));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
				}

				break;
			}

			/* RF6_S_BEAST */
			case 160+23:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (m_ptr->r_idx == MON_GAMP)
#ifdef JP
							msg_format("%^sが魔法で魔獣の親友を召喚した。", m_name);
#else
							msg_format("%^s magically summons friends of beasts.", m_name);
#endif
						else
#ifdef JP
							msg_format("%^sが魔法で魔獣を召喚した。", m_name);
#else
							msg_format("%^s magically summons beasts.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if (m_ptr->r_idx == MON_GAMP)
				{
					int attempt;
					s16b friend_beasts_r_idx[] =
					{
						MON_OBDA, MON_BELDA, MON_ZANGA, MON_BANGA,
						MON_OBDA_JR, MON_BELDA_JR,
					};

					for (k = 0; k < 2; k++)
					{
						attempt = 5000;
						while (attempt)
						{
							if (summon_named_creature(m_idx, y, x, friend_beasts_r_idx[randint0((sizeof friend_beasts_r_idx) / (sizeof (s16b)))], 0))
							{
								count++;
								break;
							}
							attempt--;
						}
					}
				}
				else
				{
					for (k = 0; k < s_num_4; k++)
					{
						count += summon_specific(m_idx, y, x, rlev, SUMMON_BEAST, (PM_ALLOW_GROUP));
					}
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
				}

				break;
			}

			/* RF6_S_ANGEL */
			case 160+24:
			{
				int num = 1;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

#ifdef JP
						msg_format("%^sが魔法で天使を召喚した！", m_name);
#else
						msg_format("%^s magically summons an angel!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				if ((r_ptr->flags1 & RF1_UNIQUE) && !easy_band)
				{
					num += rlev/40;
				}

				for (k = 0; k < num; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_ANGEL, (PM_ALLOW_GROUP));
				}

				if (known && !see_t && count)
				{
					 mon_fight = TRUE;
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

#ifdef JP
						msg_format("%^sが魔法で地獄の悪魔を召喚した！", m_name);
#else
						msg_format("%^s magically summons a hellish demon!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < 1; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_DEMON, (PM_ALLOW_GROUP));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%sが魔法でアンデッドを召喚した。", m_name);
#else
						msg_format("%^s magically summons undead.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < 1; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_UNDEAD, (PM_ALLOW_GROUP));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%^sが魔法でドラゴンを召喚した！", m_name);
#else
						msg_format("%^s magically summons a dragon!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < 1; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_DRAGON, (PM_ALLOW_GROUP));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%sが魔法でアンデッドを召喚した。", m_name);
#else
						msg_format("%^s magically summons undead.", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_6; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_UNDEAD, (PM_ALLOW_GROUP | u_mode));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%^sが魔法で古代ドラゴンを召喚した！", m_name);
#else
						msg_format("%^s magically summons ancient dragons!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_4; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_DRAGON, (PM_ALLOW_GROUP | u_mode));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
				}

				break;
			}

			/* RF6_S_TEMPLES */
			case 160+30:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

#ifdef JP
						msg_format("%^sが暗黒騎士を召喚した！", m_name);
#else
						msg_format("%^s magically summons Temple Knights!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_4; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_TEMPLES, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
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

#ifdef JP
						msg_format("%^sが魔法で特別な強敵を召喚した！", m_name);
#else
						msg_format("%^s magically summons special opponents!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_4; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_UNIQUE, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
				}
				for (k = count; k < s_num_4; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
				}

				break;
			}

			/* RFA_FIRE_STORM */
			case 288+0:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
							msg_format("%^s mumbles powerfully.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対して*火炎*の嵐の呪文を念じた。", m_name, t_name);
#else
							msg_format("%^s invokes a *fire* storm upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 4) + 50 + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_PURE_FIRE, dam, 4, FALSE, FALSE);

				break;
			}

			/* RFA_AQUA_STORM */
			case 288+1:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
							msg_format("%^s mumbles powerfully.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対して*水*の嵐の呪文を念じた。", m_name, t_name);
#else
							msg_format("%^s invokes a *aqua* storm upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 4) + 50 + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_PURE_AQUA, dam, 4, FALSE, FALSE);

				break;
			}

			/* RFA_EARTH_STORM */
			case 288+2:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
							msg_format("%^s mumbles powerfully.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対して*大地*の嵐の呪文を念じた。", m_name, t_name);
#else
							msg_format("%^s invokes a *earth* storm upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 4) + 50 + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_PURE_EARTH, dam, 4, FALSE, FALSE);

				break;
			}

			/* RFA_WIND_STORM */
			case 288+3:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
							msg_format("%^s mumbles powerfully.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対して*風*の嵐の呪文を念じた。", m_name, t_name);
#else
							msg_format("%^s invokes a *wind* storm upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 4) + 50 + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_PURE_WIND, dam, 4, FALSE, FALSE);

				break;
			}

			/* RFA_BR_PURE_FIRE */
			case 288+4:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに*火炎*のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes *fire* at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_PURE_FIRE, dam, 0, TRUE, FALSE);

				break;
			}

			/* RFA_BR_PURE_AQUA */
			case 288+5:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに*水*のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes *aqua* at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_PURE_AQUA, dam, 0, TRUE, FALSE);

				break;
			}

			/* RFA_BR_PURE_EARTH */
			case 288+6:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに*大地*のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes *earth* at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_PURE_EARTH, dam, 0, TRUE, FALSE);

				break;
			}

			/* RFA_BR_PURE_WIND */
			case 288+7:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
							msg_format("%^s breathes.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに*風*のブレスを吐いた。", m_name, t_name);
#else
							msg_format("%^s breathes *wind* at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}

					sound(SOUND_BREATH);
				}

				dam = ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3));
				monst_breath_monst(m_idx, y, x, GF_PURE_WIND, dam, 0, TRUE, FALSE);

				break;
			}

			/* RFA_PETRO_CLOUD */
			case 288+8:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かってペトロクラウドの呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts a petrocloud at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = 100 + rlev * 3;
				monst_breath_monst(m_idx, y, x, GF_STONE, dam, 3, FALSE, TRUE);

				break;
			}

			/* RFA_SAND_STORM */
			case 288+9:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
							msg_format("%^s mumbles powerfully.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに対して神砂嵐の呪文を念じた。", m_name, t_name);
#else
							msg_format("%^s invokes a godly sand storm upon %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (rlev * 4) + 50 + damroll(10, 10);
				monst_breath_monst(m_idx, y, x, GF_SHARDS, dam, 4, FALSE, FALSE);

				break;
			}

			/* RFA_ERASE_ELEM */
			case 288+10:
			{
				/* Not implemented */
				return FALSE;
			}

			/* RFA_CHANGE_ELEM */
			case 288+11:
			{
				/* Not implemented */
				return FALSE;
			}

			/* RFA_SALAMANDER */
			case 288+12:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かに命令した。", m_name);
#else
							msg_format("%^s orders someone.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sは%sを焼き払うように精霊サラマンダーに命じた！", m_name, t_name);
#else
							msg_format("%^s orders the Salamander to burn %s!.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				(void)mon_cast_call_the_elemental(m_idx, y, x, rlev, (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1, rlev, 5, GF_FIRE);

				break;
			}

			/* RFA_FENRER */
			case 288+13:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かに命令した。", m_name);
#else
							msg_format("%^s orders someone.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sは%sを氷漬けにするように精霊フェンリルに命じた！", m_name, t_name);
#else
							msg_format("%^s orders the Fenrer to freeze %s!.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				(void)mon_cast_call_the_elemental(m_idx, y, x, rlev, (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1, rlev, 5, GF_COLD);

				break;
			}

			/* RFA_GNOME */
			case 288+14:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かに命令した。", m_name);
#else
							msg_format("%^s orders someone.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sは%sを溶解させるように精霊ノームに命じた！", m_name, t_name);
#else
							msg_format("%^s orders the Gnome to melt %s!.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				(void)mon_cast_call_the_elemental(m_idx, y, x, rlev, (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1, rlev, 5, GF_ACID);

				break;
			}

			/* RFA_THUNDERBIRD */
			case 288+15:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かに命令した。", m_name);
#else
							msg_format("%^s orders someone.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sは%sに雷撃を落とすように精霊サンダーバードに命じた！", m_name, t_name);
#else
							msg_format("%^s orders the Thunderbird to hit lightning on %s!.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				(void)mon_cast_call_the_elemental(m_idx, y, x, rlev, (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1, rlev, 5, GF_ELEC);

				break;
			}

			/* RFA_IGNIS_FATUUS */
			case 288+16:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かに命令した。", m_name);
#else
							msg_format("%^s orders someone.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sは%sを神聖な力で消し去るように精霊イグニスファタスに命じた！", m_name, t_name);
#else
							msg_format("%^s orders the Ignis Fatuus to erase %s by holy force!.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				(void)mon_cast_call_the_elemental(m_idx, y, x, 0, (r_ptr->flags2 & (RF2_POWERFUL)) ? 5 : 4, rlev / 4, 3, GF_HOLY_FIRE);

				break;
			}

			/* RFA_DARK_LORE */
			case 288+17:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かに命令した。", m_name);
#else
							msg_format("%^s orders someone.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sは%sを邪悪な力で葬り去るように精霊ファントムに命じた！", m_name, t_name);
#else
							msg_format("%^s orders the Phantom to slay %s by evil force!.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				(void)mon_cast_call_the_elemental(m_idx, y, x, 0, (r_ptr->flags2 & (RF2_POWERFUL)) ? 5 : 4, rlev / 4, 3, GF_HELL_FIRE);

				break;
			}

			/* RFA_STONE_GAZE */
			case 288+18:
			{
				if ((x != fx) || (y != fy)) return FALSE;
				if (known)
				{
					if (see_m)
					{
						disturb(1, 0);
#ifdef JP
						msg_format("%^sが邪眼で周囲を見回した！", m_name);
#else
						msg_format("%^s looks around with stone gaze!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				stone_gaze(m_idx);

				break;
			}

			/* RFA_HOLY_ORB */
			case 288+19:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かって聖なる光球の呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts a holy orb at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(3, 6) + rlev + rlev / 2;
				monst_breath_monst(m_idx, y, x, GF_HOLY_FIRE, dam, (rlev > 29) ? 3 : 2, FALSE, FALSE);

				break;
			}

			/* RFA_DARK_FIRE */
			case 288+20:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かって闇の焔の呪文を唱えた。", m_name, t_name);
#else
							msg_format("%^s casts a petit hell fire at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = damroll(3, 6) + rlev + rlev / 2;
				monst_breath_monst(m_idx, y, x, GF_HELL_FIRE, dam, (rlev > 29) ? 3 : 2, FALSE, FALSE);

				break;
			}

			/* RFA_S_ZENOBIAN */
			case 288+21:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

#ifdef JP
						msg_format("%^sが神聖騎士を召喚した！", m_name);
#else
						msg_format("%^s magically summons White Knights!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_4; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_ZENOBIAN_FORCES, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
				}

				break;
			}

			/* RFA_S_HI_DEMON */
			case 288+22:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

#ifdef JP
						msg_format("%^sが魔法で上級デーモンを召喚した！", m_name);
#else
						msg_format("%^s magically summons major demons!", m_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				for (k = 0; k < s_num_4; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_DEMON, (PM_ALLOW_GROUP | u_mode));
				}

				if (known && !see_t && count)
				{
					mon_fight = TRUE;
				}

				break;
			}

			/* RFA_BA_DISI */
			case 288+23:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);

						if (blind)
						{
#ifdef JP
							msg_format("%^sが何かをつぶやいた。", m_name);
#else
							msg_format("%^s mumbles.", m_name);
#endif

						}
						else
						{
#ifdef JP
							msg_format("%^sが%sに向かって分解のエネルギーを放った。", m_name, t_name);
#else
							msg_format("%^s invokes disintegration energy at %s.", m_name, t_name);
#endif

						}
					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = rlev + 70;
				monst_breath_monst(m_idx, y, x, GF_DISINTEGRATE, dam, 4, FALSE, FALSE);

				break;
			}

			/* RFA_PURE_ELEM_BEAM */
			case 288+24:
			{
				int pure_elem_typ = GF_GODLY_SPEAR;
				cptr pure_elem_desc = "(?)";

				if ((x != fx) || (y != fy)) return FALSE;

				switch (get_cur_melem(m_ptr))
				{
				case ELEM_FIRE:
					pure_elem_typ = GF_PURE_FIRE;
#ifdef JP
					pure_elem_desc = "*火炎*";
#else
					pure_elem_desc = "*fire*";
#endif
					break;
				case ELEM_AQUA:
					pure_elem_typ = GF_PURE_AQUA;
#ifdef JP
					pure_elem_desc = "*水*";
#else
					pure_elem_desc = "*aqua*";
#endif
					break;
				case ELEM_EARTH:
					pure_elem_typ = GF_PURE_EARTH;
#ifdef JP
					pure_elem_desc = "*大地*";
#else
					pure_elem_desc = "*earth*";
#endif
					break;
				case ELEM_WIND:
					pure_elem_typ = GF_PURE_WIND;
#ifdef JP
					pure_elem_desc = "*風*";
#else
					pure_elem_desc = "*wind*";
#endif
					break;
				}

				if (known)
				{
					if (see_either)
					{
#ifdef JP
						msg_format("%^sが%sに向かって%sのビームを放った。", m_name, t_name, pure_elem_desc);
#else
						msg_format("%^s fires a %s beam at %s.", m_name, pure_elem_desc, t_name);
#endif

					}
					else
					{
						mon_fight = TRUE;
					}
				}

				dam = (r_ptr->flags2 & RF2_POWERFUL) ? (randint1(rlev * 2) + 180) : (randint1(rlev * 3 / 2) + 120);
				monst_beam_monst(m_idx, y, x, pure_elem_typ, dam);
				break;
			}
		}

		if (wake_up)
		{
			t_ptr->csleep = 0;
			if (tr_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);
		}

		if (fear && see_t)
		{
			/* Sound */
			sound(SOUND_FLEE);

#ifdef JP
			msg_format("%^sは恐怖して逃げ出した！", t_name);
#else
			msg_format("%^s flees in terror!", t_name);
#endif

		}

		/* Remember what the monster did, if we saw it */
		if (see_m)
		{
			/* Inate spell */
			if (thrown_spell < 32*4)
			{
				r_ptr->r_flags4 |= (1L << (thrown_spell - 32*3));
				if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
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

			/* Additional spell */
			else if (thrown_spell < 32*10)
			{
				r_ptr->r_flagsa |= (1L << (thrown_spell - 32*9));
				if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
			}
		}

		/* Always take note of monsters that kill you */
		if (p_ptr->is_dead && (r_ptr->r_deaths < MAX_SHORT) && !p_ptr->inside_arena)
		{
			r_ptr->r_deaths++;
		}

		if (p_ptr->action == ACTION_ELEMSCOPE) p_ptr->redraw |= (PR_MAP);

		/* A spell was cast */
		return TRUE;
	}
}