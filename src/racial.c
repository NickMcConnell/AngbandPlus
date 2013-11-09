/* File: racial.c */

/* Purpose: Racial powers (and mutations) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Hook to determine if an object is contertible in an arrow/bolt
 */
static bool item_tester_hook_convertible(object_type *o_ptr)
{
	if((o_ptr->tval==TV_JUNK) || (o_ptr->tval==TV_SKELETON)) return TRUE;

	if ((o_ptr->tval == TV_CORPSE) && (o_ptr->sval == SV_SKELETON)) return TRUE;
	/* Assume not */
	return (FALSE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'archer'.
 */
static bool do_cmd_archer(void)
{
	int ext=0;
	char ch;

	object_type	forge;
	object_type     *q_ptr;

	char com[80];
	char o_name[MAX_NLEN];

	q_ptr = &forge;

	if(p_ptr->lev >= 20)
#ifdef JP
		sprintf(com, "[A]矢, [B]クロスボウの矢 :");
#else
		sprintf(com, "Create [A]rrow or Create [B]olt ?");
#endif
	else
#ifdef JP
		sprintf(com, "[A]矢:");
#else
		sprintf(com, "Create [A]rrow ?");
#endif

	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱してる！");
#else
		msg_print("You are too confused!");
#endif
		return FALSE;
	}

	if (p_ptr->blind)
	{
#ifdef JP
		msg_print("目が見えない！");
#else
		msg_print("You are blind!");
#endif
		return FALSE;
	}

	while (TRUE)
	{
		if (!get_com(com, &ch, TRUE))
		{
			return FALSE;
		}
		if ((ch == 'A' || ch == 'a')&&(p_ptr->lev >= 10))
		{
			ext = 2;
			break;
		}
		if ((ch == 'B' || ch == 'b')&&(p_ptr->lev >= 20))
		{
			ext = 3;
			break;
		}
	}

	/**********Create arrows*********/
	if (ext == 2)
	{
		int item;

		cptr q, s;

		item_tester_hook = item_tester_hook_convertible;

		/* Get an item */
#ifdef JP
		q = "どのアイテムから作りますか？ ";
		s = "材料を持っていない。";
#else
		q = "Convert which item? ";
		s = "You have no item to convert.";
#endif
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

		/* Get the item (in the pack) */
		if (item >= 0)
		{
			q_ptr = &inventory[item];
		}

		/* Get the item (on the floor) */
		else
		{
			q_ptr = &o_list[0 - item];
		}

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player some small firestones */
		object_prep(q_ptr, lookup_kind(TV_ARROW, m_bonus(1, p_ptr->lev)+ 1));
		q_ptr->number = (byte)rand_range(5,10);
		object_aware(q_ptr);
		object_known(q_ptr);
		apply_magic(q_ptr, p_ptr->lev, 0L);

		q_ptr->discount = 99;

		object_desc(o_name, q_ptr, TRUE, 2);
#ifdef JP
		msg_format("%sを作った。", o_name);
#else
		msg_print("You make some ammo.");
#endif

		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
		(void)inven_carry(q_ptr);
	}
	/**********Create bolts*********/
	else if (ext == 3)
	{
		int item;

		cptr q, s;

		item_tester_hook = item_tester_hook_convertible;

		/* Get an item */
#ifdef JP
		q = "どのアイテムから作りますか？ ";
		s = "材料を持っていない。";
#else
		q = "Convert which item? ";
		s = "You have no item to convert.";
#endif
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

		/* Get the item (in the pack) */
		if (item >= 0)
		{
			q_ptr = &inventory[item];
		}

		/* Get the item (on the floor) */
		else
		{
			q_ptr = &o_list[0 - item];
		}

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player some small firestones */
		object_prep(q_ptr, lookup_kind(TV_BOLT, m_bonus(1, p_ptr->lev)+1));
		q_ptr->number = (byte)rand_range(4,8);
		object_aware(q_ptr);
		object_known(q_ptr);
		apply_magic(q_ptr, p_ptr->lev, 0L);

		q_ptr->discount = 99;

		object_desc(o_name, q_ptr, TRUE, 2);
#ifdef JP
		msg_format("%sを作った。", o_name);
#else
		msg_print("You make some ammo.");
#endif

		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}

		(void)inven_carry(q_ptr);
	}
	return TRUE;
}


static bool do_cmd_make_golem(void)
{
	int ext=0;
	char ch;
	int dir, item, i;
	int plev = p_ptr->lev;
	int summon_lev = 0;
	u32b mode = PM_FORCE_PET;
	int count = 0;
	int num = 1;

	object_type     *o_ptr;

	char com[80];

#ifdef JP
	sprintf(com, "[R]岩石から, [C]死体から :");
#else
	sprintf(com, "Create from [R]ubble or [C]orpse ?");
#endif

	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱してる！");
#else
		msg_print("You are too confused!");
#endif
		return FALSE;
	}

	if (p_ptr->blind)
	{
#ifdef JP
		msg_print("目が見えない！");
#else
		msg_print("You are blind!");
#endif
		return FALSE;
	}

	while (TRUE)
	{
		if (!get_com(com, &ch, TRUE))
		{
			return FALSE;
		}
		if ((ch == 'R') || (ch == 'r'))
		{
			ext = 1;
			break;
		}
		if ((ch == 'C') || (ch == 'c'))
		{
			ext = 2;
			break;
		}
	}

	/**********Create from rubbles*********/
	if (ext == 1)
	{
		int x, y;
		cave_type *c_ptr;

		if (!get_rep_dir(&dir, FALSE)) return FALSE;
		y = py + ddy[dir];
		x = px + ddx[dir];
		c_ptr = &cave[y][x];
		if (c_ptr->feat == FEAT_RUBBLE)
		{
			summon_lev = plev * 2 / 3 + randint1(plev / 2);
		}
		else
		{
#ifdef JP
			msg_print("そこには岩石がない。");
#else
			msg_print("You need pile of rubble.");
#endif
			return FALSE;
		}
	}
	/**********Create from corpses*********/
	else if (ext == 2)
	{
		cptr q, s;

		item_tester_hook = item_tester_hook_corpse;

		/* Get an item */
#ifdef JP
		q = "どの死体から作りますか？ ";
		s = "死体を持っていない。";
#else
		q = "Make from which corpse? ";
		s = "You have no corpse to make.";
#endif
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

		/* Get the item (in the pack) */
		if (item >= 0)
		{
			o_ptr = &inventory[item];
		}

		/* Get the item (on the floor) */
		else
		{
			o_ptr = &o_list[0 - item];
		}

		summon_lev = plev * 2 / 3 + r_info[o_ptr->pval].level;
	}

	if (plev > 39)
	{
		mode |=  PM_ALLOW_GROUP;
		num = (plev / 25) + randint1(2);
	}

	for (i = 0; i < num; i++)
	{
		if (summon_specific(-1, py, px, summon_lev, SUMMON_GOLEM, mode))
		{
			count++;
		}
	}

	if (count)
	{
#ifdef JP
		msg_print("ゴーレムを作った。");
#else
		msg_print("You make a golem.");
#endif
		if (ext == 1)
		{
			(void)wall_to_mud(dir);
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);
			p_ptr->window |= (PW_OVERHEAD);
		}
		else if (ext == 2)
		{
			if (item >= 0)
			{
				inven_item_increase(item, -1);
				inven_item_describe(item);
				inven_item_optimize(item);
			}
			else
			{
				floor_item_increase(0 - item, -1);
				floor_item_describe(0 - item);
				floor_item_optimize(0 - item);
			}
		}
	}
	else
	{
#ifdef JP
		msg_print("うまくゴーレムを作れなかった。");
#else
		msg_print("No Golems arrive.");
#endif
	}

	return TRUE;
}


typedef struct power_desc_type power_desc_type;

struct power_desc_type
{
	char name[40];
	int  level;
	int  cost;
	int  stat;
	int  fail;
	int  number;
};


/*
 * Returns the chance to activate a racial power/mutation
 */
static int racial_chance(power_desc_type *pd_ptr)
{
	s16b min_level  = pd_ptr->level;
	int  use_stat   = pd_ptr->stat;
	int  difficulty = pd_ptr->fail;

	int i;
	int val;
	int sum = 0;
	int stat = p_ptr->stat_cur[use_stat];

	/* No chance for success */
	if ((p_ptr->lev < min_level) || p_ptr->confused)
	{
		return (0);
	}

	if (difficulty == 0) return 100;

	/* Calculate difficulty */
	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun / 3;
	}
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level) / 3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* We only need halfs of the difficulty */
	difficulty = difficulty / 2;

	for (i = 1; i <= stat; i++)
	{
		val = i - difficulty;
		if (val > 0)
			sum += (val <= difficulty) ? val : difficulty;
	}

	if (difficulty == 0)
		return (100);
	else
		return (((sum * 100) / difficulty) / stat);
}


static int  racial_cost;
static bool racial_use_hp;

/*
 * Note: return value indicates that we have succesfully used the power
 * 1: Succeeded, 0: Cancelled, -1: Failed
 */

static int racial_aux(power_desc_type *pd_ptr)
{
	s16b min_level  = pd_ptr->level;
	int  use_stat   = pd_ptr->stat;
	int  difficulty = pd_ptr->fail;

	racial_cost   = pd_ptr->cost;
	racial_use_hp = FALSE;

	/* Not enough mana - use hp */
	if (p_ptr->csp < racial_cost) racial_use_hp = TRUE;

	/* Power is not available yet */
	if (p_ptr->lev < min_level)
	{
#ifdef JP
msg_format("この能力を使用するにはレベル %d に達していなければなりません。", min_level);
#else
		msg_format("You need to attain level %d to use this power.", min_level);
#endif

		energy_use = 0;
		return 0;
	}

	/* Too confused */
	else if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していてその能力は使えない。");
#else
		msg_print("You are too confused to use this power.");
#endif

		energy_use = 0;
		return 0;
	}

	/* Risk death? */
	else if (racial_use_hp && (p_ptr->chp < racial_cost))
	{
#ifdef JP
		if (!get_check("本当に今の衰弱した状態でこの能力を使いますか？"))
#else
		if (!get_check("Really use the power in your weakened state? "))
#endif

		{
			energy_use = 0;
			return 0;
		}
	}

	/* Else attempt to do it! */

	if (difficulty)
	{
		if (p_ptr->stun)
		{
			difficulty += p_ptr->stun;
		}
		else if (p_ptr->lev > min_level)
		{
			int lev_adj = ((p_ptr->lev - min_level) / 3);
			if (lev_adj > 10) lev_adj = 10;
			difficulty -= lev_adj;
		}

		if (difficulty < 5) difficulty = 5;
	}

	/* take time and pay the price */
	energy_use = 100;

	/* Success? */
	if (randint1(p_ptr->stat_cur[use_stat]) >=
	    ((difficulty / 2) + randint1(difficulty / 2)))
	{
		return 1;
	}

#ifdef JP
	msg_print("充分に集中できなかった。");
#else
	msg_print("You've failed to concentrate hard enough.");
#endif
	if (flush_failure) flush();

	return -1;
}


static bool do_cmd_racial_throwing(int fake_item)
{
	object_type  fake_bow;
	object_type *j_ptr = &fake_bow;
	s16b         old_to_h_b = p_ptr->to_h_b;
	s16b         old_num_fire = p_ptr->num_fire;
	byte         old_tval_ammo = p_ptr->tval_ammo;
	bool         old_xtra_might = p_ptr->xtra_might;
	bool         old_dis_xtra_might = p_ptr->dis_xtra_might;
	int          fake_sval;

	object_type *o_ptr;
	u32b         flgs[TR_FLAG_SIZE];
	int          extra_shots = 0;
	int          hold = adj_str_hold[p_ptr->stat_ind[A_STR]];
	int          i;

	bool done;

	switch (fake_item)
	{
	case INVEN_PEBBLE:
		fake_sval = SV_PISTOL;
		p_ptr->tval_ammo = TV_BULLET;
		break;

	case INVEN_GRAVE:
		fake_sval = SV_BOWGUN;
		p_ptr->tval_ammo = TV_BOLT;
		break;

	default:
		return FALSE;
	}

	/* Prepare the "fake bow" */
	object_prep(j_ptr, lookup_kind(TV_BOW, fake_sval));
	object_aware(j_ptr);
	object_known(j_ptr);
	j_ptr->to_h = (int)(adj_throwing[p_ptr->stat_ind[A_DEX]]) - 128;
	j_ptr->to_d = (int)(adj_throwing[p_ptr->stat_ind[A_STR]]) - 128;

	p_ptr->to_h_b = 0;
	p_ptr->num_fire = 100;
	p_ptr->tval_ammo = 0;
	p_ptr->dis_xtra_might = p_ptr->xtra_might = FALSE;

	/* Scan the usable inventory */
	for (i = INVEN_RARM; i < INVEN_TOTAL; i++)
	{
		int bonus_to_h;
		o_ptr = &inventory[i];

		/* Hack -- ignore "bow" */
		if (i == INVEN_BOW) continue;

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, flgs);

		/* Boost shots */
		if (have_flag(flgs, TR_XTRA_SHOTS)) extra_shots++;

		/* Various flags */
		if (have_flag(flgs, TR_XTRA_MIGHT))
		{
			p_ptr->xtra_might = TRUE;
			if (object_known_p(o_ptr)) p_ptr->dis_xtra_might = TRUE;
		}

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_RARM && buki_motteruka(i)) continue;
		if (i == INVEN_LARM && buki_motteruka(i)) continue;

		bonus_to_h = o_ptr->to_h;
		if ((p_ptr->pclass == CLASS_NINJA) && (o_ptr->to_h > 0))
			bonus_to_h = (o_ptr->to_h + 1) / 2;

		/* Apply the bonuses to hit */
		p_ptr->to_h_b += bonus_to_h;
	}

	/* Hack - Mighty throwing is converted to XTRA_SHOTS and XTRA_MIGHT */
	if (p_ptr->mighty_throw)
	{
		extra_shots++;
		p_ptr->xtra_might = TRUE;
		p_ptr->dis_xtra_might = p_ptr->dis_mighty_throw;
	}

	/* Apply temporary status */
	if (p_ptr->stun > 100) p_ptr->to_h_b -= 20;
	else if (p_ptr->stun) p_ptr->to_h_b -= 5;
	if (p_ptr->blessed) p_ptr->to_h_b  += 10;
	if (p_ptr->hero) p_ptr->to_h_b  += 12;
	if (p_ptr->shero) p_ptr->to_h_b  -= 12;

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_h_b += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h_b += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

	/* It is hard to carholdry a heavy bow */
	if (hold < j_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h_b  += 2 * (hold - j_ptr->weight / 10);
	}

	/* Apply special flags */
	else
	{
		/* Extra shots */
		p_ptr->num_fire += (extra_shots * 100);
		p_ptr->num_fire += skill_lev_var[p_ptr->misc_skill_lev[SKILL_THROWING]] * 50;
	}

	if (p_ptr->riding)
	{
		int penalty = 0;

		if ((p_ptr->pclass == CLASS_BEASTTAMER) || (p_ptr->pclass == CLASS_DRAGONTAMER))
		{
			if (p_ptr->tval_ammo != TV_ARROW) penalty = 5;
		}
		else
		{
			penalty = r_info[m_list[p_ptr->riding].r_idx].level - (skill_lev_var[p_ptr->misc_skill_lev[SKILL_RIDING]] * 1000) / 80;
			penalty += 30;
			if (penalty < 30) penalty = 30;
		}
		if (p_ptr->tval_ammo == TV_BOLT) penalty *= 2;
		p_ptr->to_h_b -= penalty;
	}

	/* Fire with fake bow, fake ammo and fake status */
	done = do_cmd_fire_aux(fake_item, j_ptr, FALSE);

	/* Restore player's status rerating shooting */
	p_ptr->to_h_b = old_to_h_b;
	p_ptr->num_fire = old_num_fire;
	p_ptr->tval_ammo = old_tval_ammo;
	p_ptr->xtra_might = old_xtra_might;
	p_ptr->dis_xtra_might = old_dis_xtra_might;

	return done;
}


static bool cmd_racial_power_aux(s32b command)
{
	s16b        plev = p_ptr->lev;
	s16b        chp = p_ptr->chp;
	int         dir = 0;
	bool        done;

	if (!(rp_ptr->r_flags & PRF_LARGE)
		&& (p_ptr->pclass != CLASS_TERRORKNIGHT)
		&& (p_ptr->pclass != CLASS_SWORDMASTER)
		&& (p_ptr->pclass != CLASS_NINJA)
		&& (command == -9))
	{
		if (!do_cmd_racial_throwing(INVEN_PEBBLE)) return FALSE;
	}
	else if (((p_ptr->pclass == CLASS_KNIGHT)
		|| (p_ptr->pclass == CLASS_VALKYRIE))
		&& (command == -10))
	{
		char m_name[80];
		monster_type *m_ptr;
		monster_race *r_ptr;
		int rlev;

		if (p_ptr->riding)
		{
#ifdef JP
			msg_print("今は乗馬中だ。");
#else
			msg_print("You ARE riding.");
#endif
			return FALSE;
		}
		if (!do_riding(TRUE)) return FALSE;
		m_ptr = &m_list[p_ptr->riding];
		r_ptr = &r_info[m_ptr->r_idx];
		monster_desc(m_name, m_ptr, 0);
#ifdef JP
		msg_format("%sに乗った。",m_name);
#else
		msg_format("You ride on %s.",m_name);
#endif
		if (is_pet(m_ptr)) return TRUE;
		rlev = r_ptr->level;
		if (r_ptr->flags1 & RF1_UNIQUE) rlev = rlev * 3 / 2;
		if (rlev > 60) rlev = 60+(rlev-60)/2;
		if ((randint1((skill_lev_var[p_ptr->misc_skill_lev[SKILL_RIDING]] * 1000)/120+p_ptr->lev*2/3) > rlev) && one_in_(2) && !p_ptr->inside_arena && !(r_ptr->flags7 & (RF7_GUARDIAN)) && !(r_ptr->flags1 & (RF1_QUESTOR)) && (rlev < p_ptr->lev*3/2+randint0(p_ptr->lev/5)))
		{
#ifdef JP
			msg_format("%sを手なずけた。",m_name);
#else
			msg_format("You tame %s.",m_name);
#endif
			set_pet(m_ptr);
		}
		else
		{
#ifdef JP
			msg_format("%sに振り落とされた！",m_name);
#else
			msg_format("You have thrown off by %s.",m_name);
#endif
			rakuba(1,TRUE);
		}
	}
	else if (((p_ptr->pclass == CLASS_WIZARD)
		|| (p_ptr->pclass == CLASS_SIRENE)
		|| (p_ptr->pclass == CLASS_LICH)
		|| (p_ptr->pclass == CLASS_HIGHWITCH))
		&& (command == -10))
	{
		if (!eat_magic(p_ptr->lev * 2)) return FALSE;
	}
	else if (command <= -9)
	{
		switch (p_ptr->pclass)
		{
		case CLASS_KNIGHT:
		{
			switch (command)
			{
			case -11:
				(void)detect_monsters_evil(DETECT_RAD_DEFAULT);
				break;
			case -12:
				{
					int tx, ty, nx, ny;
					int over_1 = 0, over_2 = 0;
					u16b path_g[16];
					int path_n, i;

					project_length = 5;
					if (!get_aim_dir(&dir)) return FALSE;

					/* Hack -- Use an actual "target" */
					if ((dir == 5) && target_okay())
					{
						tx = target_col;
						ty = target_row;
					}
					else
					{
						/* Use the given direction */
						ty = py + 99 * ddy[dir];
						tx = px + 99 * ddx[dir];

						/* If not in bounds... */
						if (!in_bounds2(ty, tx))
						{
							if (ty < 0) over_1 = -ty;
							else if (ty >= cur_hgt) over_1 = ty - cur_hgt + 1;
							if (tx < 0) over_2 = -tx;
							else if (tx >= cur_wid) over_2 = tx - cur_wid + 1;

							if (over_1 < over_2) over_1 = over_2;

							ty += over_1 * (-ddy[dir]);
							tx += over_1 * (-ddx[dir]);
						}
					}

					path_n = project_path(path_g, project_length, py, px, ty, tx, 0L);
					project_length = 0;
					ty = py;
					tx = px;

					/* Project along the path */
					for (i = 0; i < path_n; ++i)
					{
						ny = GRID_Y(path_g[i]);
						nx = GRID_X(path_g[i]);

						/* Max distance empty floor */
						if (player_has_los_bold(ny, nx) &&
							cave_empty_bold(ny, nx) &&
							player_can_enter(cave[ny][nx].feat))
						{
							/* Save the tmp location */
							ty = ny;
							tx = nx;
						}

						if (!p_ptr->ffall && (cave[ny][nx].feat == FEAT_AIR)) break;
					}

					if ((ty == py) && (tx == px))
					{
#ifdef JP
						msg_print("突撃できません。");
#else
						msg_print("You can't charge to that place.");
#endif
						return FALSE;
					}

					project(0, 0, ty, tx, PY_ATTACK_CHARGE, GF_ATTACK, PROJECT_BEAM | PROJECT_KILL, MODIFY_ELEM_MODE_MELEE);
					teleport_player_to(ty, tx, FALSE, FALSE);
				}
				break;
			case -13:
				if (p_ptr->lev > 44)
				{
					if (!identify_fully(TRUE)) return FALSE;
				}
				else
				{
					if (!ident_spell(TRUE)) return FALSE;
				}
				break;
			}
			break;
		}
		case CLASS_BERSERKER:
		{
			if (command == -10)
			{
				teleport_player(50 + plev * 2);
			}
			break;
		}
		case CLASS_TERRORKNIGHT:
		{
			switch (command)
			{
			case -9:
				(void)detect_monsters_living(DETECT_RAD_DEFAULT);
				break;
			case -10:
				if (!do_cmd_racial_throwing(INVEN_GRAVE)) return FALSE;
				break;
			case -11:
				{
					int i, count = 0;
					u32b mode = PM_NO_PET | PM_ALLOW_GROUP | PM_IGNORE_AMGRID;

					summon_kin_type = 'G';

					for (i = 0; i < 4; i++)
					{
						if (summon_specific(0, py, px, plev * 2 / 3 + randint1(plev/2), SUMMON_KIN, mode))
							count++;
					}
					if (count)
					{
#ifdef JP
						msg_print("殺してきた者達の霊が現れた。");
#else
						msg_print("You summon souls of your victims.");
#endif
					}
					else
					{
#ifdef JP
						msg_print("何も現れなかった。");
#else
						msg_print("No one arrive.");
#endif
					}
				}
				break;
			case -12:
				earthquake(py, px, 8 + randint0(5));
				break;
			case -13:
				{
					int              y, x;
					cave_type       *c_ptr;
					monster_type    *m_ptr;

					for (dir = 0; dir < 8; dir++)
					{
						y = py + ddy_ddd[dir];
						x = px + ddx_ddd[dir];
						c_ptr = &cave[y][x];

						/* Get the monster */
						m_ptr = &m_list[c_ptr->m_idx];

						/* Hack -- attack monsters */
						if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
							py_attack(y, x, 0);
					}
				}
				break;
			case -14:
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}
			break;
		}
		case CLASS_BEASTTAMER:
		{
			switch (command)
			{
			case -10:
				{
					u32b mode = PM_ALLOW_GROUP | PM_FORCE_PET;
					if (summon_specific(-1, py, px, plev * 2 / 3 + randint1(plev/2), SUMMON_SPIDER, mode))
					{
#ifdef JP
						msg_print("クモを召喚した。");
#else
						msg_print("You summon spiders.");
#endif
					}
					else
					{
#ifdef JP
						msg_print("クモは現れなかった。");
#else
						msg_print("No spider arrive.");
#endif
					}
				}
				break;
			case -11:
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_CONTROL_BEAST, dir, plev * 2, 0, FALSE);
				break;
			case -12:
				{
					int i, count = 0;
					u32b mode = PM_ALLOW_GROUP | PM_FORCE_PET;
					for (i = 0; i < 4; i++)
					{
						if (summon_specific(-1, py, px, plev * 2 / 3 + randint1(plev/2), SUMMON_BEAST, mode))
							count++;
					}
					if (count)
					{
#ifdef JP
						msg_print("魔獣を召喚した。");
#else
						msg_print("You summon beasts.");
#endif
					}
					else
					{
#ifdef JP
						msg_print("魔獣は現れなかった。");
#else
						msg_print("No beast arrive.");
#endif
					}
				}
				break;
			}
			break;
		}
		case CLASS_SWORDMASTER:
		{
			switch (command)
			{
			case -9:
				{
					int pstat = p_ptr->stat_use[A_STR];
					int attack_var = skill_lev_var[p_ptr->misc_skill_lev[SKILL_THROWING]];

					if (!get_aim_dir(&dir)) return FALSE;
					hack_elem_mod_mode = MODIFY_ELEM_MODE_THROW;
					fire_bolt(GF_BLUNT, dir,
						damroll((((plev - 10) > 0) ? (plev - 10) : 0) / ((pstat >= (18 + 150)) ? 4 : 5) + 3,
							((pstat >= (18 + 200)) ? 4 : 3)));

					/* Take partial turn */
					energy_use = 100 - attack_var * attack_var;
				}
				break;
			case -10:
				detect_monsters_mind(DETECT_RAD_DEFAULT);
				break;
			case -11:
				if (!buki_motteruka(INVEN_RARM))
				{
					if (flush_failure) flush();
#ifdef JP
					msg_print("武器を持たないと必殺技は使えない！");
#else
					msg_print("You need to wield a weapon!");
#endif
					return;
				}
				if (!do_cmd_throw_aux(1, PY_THROW_BOOMERANG, 0)) return FALSE;
				break;
			case -12:
				{
					int y, x;

					if (!get_rep_dir2(&dir)) return FALSE;
					if (dir == 5) return FALSE;
					y = py + ddy[dir];
					x = px + ddx[dir];
					if (cave[y][x].m_idx)
						py_attack(y, x, PY_ATTACK_MINEUCHI);
					else
					{
#ifdef JP
						msg_print("その方向にはモンスターはいません。");
#else
						msg_print("There is no monster.");
#endif
						return FALSE;
					}
				}
				break;
			case -13:
				{
					int y, x;

					if (p_ptr->riding)
					{
#ifdef JP
						msg_print("乗馬中には無理だ。");
#else
						msg_print("You cannot do it when riding.");
#endif
						return FALSE;
					}

					if (!get_rep_dir2(&dir)) return FALSE;

					if (dir == 5) return FALSE;
					y = py + ddy[dir];
					x = px + ddx[dir];

					if (!cave[y][x].m_idx)
					{
#ifdef JP
						msg_print("その方向にはモンスターはいません。");
#else
						msg_print("There is no monster.");
#endif
						return FALSE;
					}

					py_attack(y, x, 0);

					if (!player_can_enter(cave[y][x].feat) || is_trap(cave[y][x].feat))
						break;

					y += ddy[dir];
					x += ddx[dir];

					if (player_can_enter(cave[y][x].feat) && !is_trap(cave[y][x].feat) && !cave[y][x].m_idx)
					{
						int oy, ox;

						msg_print(NULL);

						/* Save the old location */
						oy = py;
						ox = px;

						/* Move the player */
						py = y;
						px = x;

						forget_flow();

						/* Redraw the old spot */
						lite_spot(oy, ox);

						/* Redraw the new spot */
						lite_spot(py, px);

						/* Check for new panel (redraw map) */
						verify_panel();

						set_aquatic_in_water();

						/* Update stuff */
						p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_BONUS);

						/* Update the monsters */
						p_ptr->update |= (PU_DISTANCE);

						/* Window stuff */
						p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

						/* Handle stuff XXX XXX XXX */
						handle_stuff();
					}
				}
				break;
			case -14:
				project_length = 5;
				if (!get_aim_dir(&dir)) return FALSE;
				project_hook(GF_ATTACK, dir, PY_ATTACK_NYUSIN, PROJECT_STOP | PROJECT_KILL);
				break;
			case -15:
				{
					int y, x, i;
					if (!get_rep_dir2(&dir)) return FALSE;
					if (dir == 5) return FALSE;
					for (i = 0; i < 3; i++)
					{
						int oy, ox;
						int ny, nx;
						int m_idx;
						monster_type *m_ptr;

						y = py + ddy[dir];
						x = px + ddx[dir];

						if (cave[y][x].m_idx)
							py_attack(y, x, PY_ATTACK_3DAN);
						else
						{
#ifdef JP
							msg_print("その方向にはモンスターはいません。");
#else
							msg_print("There is no monster.");
#endif
							return FALSE;
						}

						/* Monster is dead? */
						if (!cave[y][x].m_idx) break;

						ny = y + ddy[dir];
						nx = x + ddx[dir];
						m_idx = cave[y][x].m_idx;
						m_ptr = &m_list[m_idx];

						/* Monster cannot move back? */
						if (!monster_can_enter(ny, nx, &r_info[m_ptr->r_idx])) continue;

						cave[y][x].m_idx = 0;
						cave[ny][nx].m_idx = m_idx;
						m_ptr->fy = ny;
						m_ptr->fx = nx;

						update_mon(m_idx, TRUE);

						/* Player can move forward? */
						if (player_can_enter(cave[y][x].feat))
						{
							/* Save the old location */
							oy = py;
							ox = px;

							/* Move the player */
							py = y;
							px = x;

							if (p_ptr->riding)
							{
								int tmp;
								tmp = cave[py][px].m_idx;
								cave[py][px].m_idx = cave[oy][ox].m_idx;
								cave[oy][ox].m_idx = tmp;
								m_list[p_ptr->riding].fy = py;
								m_list[p_ptr->riding].fx = px;
							}

							forget_flow();

							/* Redraw the old spot */
							lite_spot(oy, ox);

							/* Redraw the new spot */
							lite_spot(py, px);
						}

						/* Redraw the old spot */
						lite_spot(y, x);

						/* Redraw the new spot */
						lite_spot(ny, nx);

						/* Check for new panel (redraw map) */
						verify_panel();

						set_aquatic_in_water();

						/* Update stuff */
						p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

						/* Update the monsters */
						p_ptr->update |= (PU_DISTANCE);

						/* Window stuff */
						p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

						/* Handle stuff */
						handle_stuff();

						/* -more- */
						if (i < 2) msg_print(NULL);
					}
				}
				break;
			}
			break;
		}
		case CLASS_NINJA:
		{
			switch (command)
			{
			case -9:
				{
					int pstat = p_ptr->stat_use[A_STR];
					int attack_var = skill_lev_var[p_ptr->misc_skill_lev[SKILL_THROWING]];

					if (!get_aim_dir(&dir)) return FALSE;
					hack_elem_mod_mode = MODIFY_ELEM_MODE_THROW;
					fire_bolt(GF_EDGED, dir,
						damroll((((plev - 10) > 0) ? (plev - 10) : 0) / ((pstat >= (18 + 150)) ? 4 : 5) + 3,
							((pstat >= (18 + 200)) ? 4 : 3)));

					/* Take partial turn */
					energy_use = 100 - attack_var * attack_var;
				}
				break;
			case -10:
				if (p_ptr->singing || p_ptr->restart_singing) stop_singing();

				if (p_ptr->action == ACTION_STEALTH) set_action(ACTION_NONE);
				else set_action(ACTION_STEALTH);
				energy_use = 0;
				break;
			case -11:
				{
					int y, x;

					if (!get_rep_dir(&dir, FALSE)) return FALSE;
					y = py + ddy[dir];
					x = px + ddx[dir];
					if (cave[y][x].m_idx)
					{
						py_attack(y, x, 0);
						if (randint0(p_ptr->skill_dis) < 7)
#ifdef JP
							msg_print("うまく逃げられなかった。");
#else
							msg_print("You failed to run away.");
#endif
						else
						{
							teleport_player(30);
						}
					}
					else
					{
#ifdef JP
						msg_print("その方向にはモンスターはいません。");
#else
						msg_print("You don't see any monster in this direction");
#endif

						msg_print(NULL);
					}
				}
				break;
			}
			break;
		}
		case CLASS_WARLOCK:
		{
			switch (command)
			{
			case -10:
				if (!identify_fully(FALSE)) return FALSE;
				break;
			case -11:
				if (!do_cmd_make_golem()) return FALSE;
				break;
			}
			break;
		}
		case CLASS_VALKYRIE:
		{
			if (command == -11)
			{
				{
					int y, x;
					bool do_attack = FALSE;

					if (!get_rep_dir2(&dir)) return FALSE;
					if (dir == 5) return FALSE;
					y = py + ddy[dir];
					x = px + ddx[dir];
					if (cave[y][x].m_idx)
					{
						py_attack(y, x, PY_ATTACK_PENET);
						do_attack = TRUE;
					}
					y += ddy[dir];
					x += ddx[dir];
					if (in_bounds(y, x))
					{
						if (cave[y][x].m_idx)
						{
							py_attack(y, x, 0);
							do_attack = TRUE;
						}
					}
					penet_ac = 0;

					if (!do_attack)
					{
#ifdef JP
						msg_print("その方向にはモンスターはいません。");
#else
						msg_print("There is no monster.");
#endif
						return FALSE;
					}
				}
			}
			break;
		}
		case CLASS_ARCHER:
		{
			switch (command)
			{
			case -10:
				if (!do_cmd_archer()) return FALSE;
				break;
			case -11:
				if (p_ptr->tval_ammo != TV_ARROW)
				{
#ifdef JP
					msg_print("矢を使う射撃武器が必要です。");
#else
					msg_print("You need a bow which use arrows.");
#endif
					return FALSE;
				}
				energy_use = 0;
				if (!do_cmd_fire(TRUE)) return FALSE;
				break;
			case -12:
				energy_use = 0;
				p_ptr->to_h_b += 30;
				done = do_cmd_fire(FALSE);
				p_ptr->to_h_b -= 30;
				if (!done) return FALSE;
				break;
			case -13:
				energy_use = 0;
				p_ptr->to_h_b -= 30;
				p_ptr->num_fire *= 2;
				done = do_cmd_fire(FALSE);

				p_ptr->to_h_b += 30;
				p_ptr->num_fire /= 2;
				if (!done) return FALSE;
				break;
			}
			break;
		}
		case CLASS_DRAGONTAMER:
		{
			switch (command)
			{
			case -10:
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_CONTROL_DRAGON, dir, plev * 2, 0, FALSE);
				break;
			case -11:
				if (summon_specific(-1, py, px, plev * 2 / 3 + randint1(plev/2), SUMMON_DRAGON, PM_FORCE_PET))
				{
#ifdef JP
					msg_print("ドラゴンを召喚した。");
#else
					msg_print("You summon a dragon.");
#endif
				}
				else
				{
#ifdef JP
					msg_print("ドラゴンは現れなかった。");
#else
					msg_print("No dragon arrive.");
#endif
				}
				break;
			case -12:
				if (summon_specific(-1, py, px, plev * 2 / 3 + randint1(plev/2), SUMMON_HI_DRAGON, PM_FORCE_PET))
				{
#ifdef JP
					msg_print("古代ドラゴンを召喚した。");
#else
					msg_print("You summon an ancient dragon.");
#endif
				}
				else
				{
#ifdef JP
					msg_print("古代ドラゴンは現れなかった。");
#else
					msg_print("No ancient dragon arrive.");
#endif
				}
				break;
			}
			break;
		}
		case CLASS_LICH:
		{
			if (command == -11)
			{
				if (summon_specific(-1, py, px, plev * 2 / 3 + randint1(plev/2), SUMMON_HI_UNDEAD, PM_FORCE_PET))
				{
#ifdef JP
					msg_print("上級アンデッドを召喚した。");
#else
					msg_print("You summon a greater undead being.");
#endif
				}
				else
				{
#ifdef JP
					msg_print("上級アンデッドは現れなかった。");
#else
					msg_print("No greater undead being arrive.");
#endif
				}
			}
			break;
		}
		case CLASS_ANGELKNIGHT:
		{
			if ((command == -11) || (command == -12))
			{
				if (p_ptr->anti_magic)
				{
#ifdef JP
					msg_print("反魔法バリアが歌を邪魔した！");
#else
					msg_print("An anti-magic shell disrupts your singing!");
#endif
					return FALSE;
				}
				if (is_anti_magic_grid(-1, py, px))
				{
#ifdef JP
					msg_print("反魔法フィールドが歌を邪魔した！");
#else
					msg_print("An anti-magic field disrupts your singing!");
#endif
					return TRUE;
				}
				else if (p_ptr->shero)
				{
#ifdef JP
					msg_format("狂戦士化していて頭が回らない！");
#else
					msg_format("You cannot think directly!");
#endif
					return FALSE;
				}
			}
			switch (command)
			{
			case -10:
				stop_singing();
				energy_use = 10;
				break;
			case -11:
				if (p_ptr->singing) stop_singing();
#ifdef JP
				msg_print("悲しげで安らかなメロディを奏で始めた...");
#else
				msg_print("You weave a slow, sad, soothing melody of imploration...");
#endif
				p_ptr->singing = MUSIC_SAD;
				p_ptr->song_start = MUSIC_SAD;
				set_action(ACTION_SING);
				break;
			case -12:
				if (p_ptr->singing) stop_singing();
#ifdef JP
				msg_print("静かな音楽が辺りの者をも静まり返らせる...");
#else
				msg_print("Your quiet music silences all listeners...");
#endif
				p_ptr->singing = MUSIC_SILENT;
				p_ptr->song_start = MUSIC_SILENT;
				set_action(ACTION_SING);
				break;
			}
			break;
		}
		case CLASS_HIGHWITCH:
		{
			if (command == -11)
			{
				{
					int i, count = 0;
					for (i = 0; i < 4; i++)
					{
						if (summon_named_creature(-1, py, px, MON_PUMPKIN_HEAD, PM_FORCE_PET))
							count++;
					}
					if (count)
					{
#ifdef JP
						msg_print("パンプキンヘッドを召喚した。");
#else
						msg_print("You summon Pumpkin-Heads.");
#endif
					}
					else
					{
#ifdef JP
						msg_print("パンプキンヘッドは現れなかった。");
#else
						msg_print("No Pumpkin-Head arrive.");
#endif
					}
				}
			}
			break;
		}
		case CLASS_GUNNER:
		{
			switch (command)
			{
			case -10:
				detect_monsters_thermal(DETECT_RAD_DEFAULT);
				break;
			case -11:
				map_area(DETECT_RAD_MAP);
				(void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_doors(DETECT_RAD_DEFAULT);
				(void)detect_stairs(DETECT_RAD_DEFAULT);
				(void)detect_objects_normal(DETECT_RAD_DEFAULT);
				(void)detect_treasure(DETECT_RAD_DEFAULT);
				(void)detect_objects_gold(DETECT_RAD_DEFAULT);
				break;
			case -12:
				{
					int tx, ty, nx, ny;
					int over_1 = 0, over_2 = 0;
					u16b path_g[16];
					int path_n, i;

					project_length = 5 + plev / 20;
					if (!get_aim_dir(&dir)) return FALSE;

					/* Hack -- Use an actual "target" */
					if ((dir == 5) && target_okay())
					{
						tx = target_col;
						ty = target_row;
					}
					else
					{
						/* Use the given direction */
						ty = py + 99 * ddy[dir];
						tx = px + 99 * ddx[dir];

						/* If not in bounds... */
						if (!in_bounds2(ty, tx))
						{
							if (ty < 0) over_1 = -ty;
							else if (ty >= cur_hgt) over_1 = ty - cur_hgt + 1;
							if (tx < 0) over_2 = -tx;
							else if (tx >= cur_wid) over_2 = tx - cur_wid + 1;

							if (over_1 < over_2) over_1 = over_2;

							ty += over_1 * (-ddy[dir]);
							tx += over_1 * (-ddx[dir]);
						}
					}

					path_n = project_path(path_g, project_length, py, px, ty, tx, 0L);
					project_length = 0;
					ty = py;
					tx = px;

					/* Project along the path */
					for (i = 0; i < path_n; ++i)
					{
						ny = GRID_Y(path_g[i]);
						nx = GRID_X(path_g[i]);

						/* Max distance empty floor */
						if (player_has_los_bold(ny, nx) &&
							cave_empty_bold(ny, nx) &&
							player_can_enter(cave[ny][nx].feat))
						{
							/* Save the tmp location */
							ty = ny;
							tx = nx;
						}

						if (!p_ptr->ffall && (cave[ny][nx].feat == FEAT_AIR)) break;
					}

					if ((ty == py) && (tx == px))
					{
#ifdef JP
						msg_print("移動できません。");
#else
						msg_print("You can't move to that place.");
#endif
						return FALSE;
					}

					/* Project along the path */
					for (i = 0; i < path_n; ++i)
					{
						ny = GRID_Y(path_g[i]);
						nx = GRID_X(path_g[i]);

						if ((ny == ty) && (nx == tx)) break;

						if (cave[ny][nx].m_idx)
						{
							monster_type *m_ptr = &m_list[cave[ny][nx].m_idx];
							if (m_ptr->csleep)
							{
								m_ptr->csleep = 0;
								if (m_ptr->ml && !p_ptr->blind)
								{
									char m_name[80];

									monster_desc(m_name, m_ptr, 0);
#ifdef JP
									msg_format("%^sが目を覚ました。", m_name);
#else
									msg_format("%^s wakes up.", m_name);
#endif
								}
							}
						}
					}

					teleport_player_to(ty, tx, FALSE, FALSE);
				}
				break;
			case -13:
				probing();
				break;
			case -14:
				energy_use = 0;
				p_ptr->to_h_b += 30;
				done = do_cmd_fire(FALSE);
				p_ptr->to_h_b -= 30;
				if (!done) return FALSE;
				break;
			case -15:
				if (!jump_wall()) return FALSE;
				break;
			}
			break;
		}
		}
	}

	else 
	{

	switch (p_ptr->prace)
	{
		case RACE_HAWKMAN:
			if (p_ptr->pelem == ELEM_WIND)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_SOUND, dir, plev / 4 + (damroll(((plev - 1) / 4), 4)));
			}
			break;

		case RACE_LIZARDMAN:
			{
				int Type = GF_MISSILE;
#ifdef JP
				cptr Type_desc = "エレメント";
#else
				cptr Type_desc = "the elements";
#endif

				switch (get_cur_pelem())
				{
				case ELEM_FIRE:
					{
						Type = GF_FIRE;
#ifdef JP
						Type_desc = "火炎";
#else
						Type_desc = "fire";
#endif
					}
					break;
				case ELEM_AQUA:
					{
						if ((plev > 39) && (one_in_(5)))
						{
							Type = GF_PURE_AQUA;
#ifdef JP
							Type_desc = "*水*";
#else
							Type_desc = "*aqua*";
#endif
						}
						else
						{
							Type = GF_COLD;
#ifdef JP
							Type_desc = "冷気";
#else
							Type_desc = "cold";
#endif
						}
					}
					break;
				case ELEM_EARTH:
					{
						Type = GF_ACID;
#ifdef JP
						Type_desc = "酸";
#else
						Type_desc = "acid";
#endif
					}
					break;
				case ELEM_WIND:
					{
						Type = GF_ELEC;
#ifdef JP
						Type_desc = "稲妻";
#else
						Type_desc = "lightning";
#endif
					}
					break;
				}

				if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
				msg_format("あなたは%sのブレスを吐いた。", Type_desc);
#else
				msg_format("You breathe %s.", Type_desc);
#endif

				fire_ball(Type, dir,
				    chp / ((get_cur_pelem() == ELEM_AQUA) ? 3 : 10) + randint1(chp/10),
				    -(plev / 15) - 1, FALSE);
			}
			break;

		case RACE_FAIRY:
#ifdef JP
msg_print("あなたは魔法の粉を投げつけた...");
#else
			msg_print("You throw some magic dust...");
#endif

			if (plev < 25)
				sleep_monsters_touch();
			else
				(void)sleep_monsters();
			break;

		case RACE_GREMLIN:
			{
				int y, x, dummy = 0;
				cave_type *c_ptr;

				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir, FALSE)) return FALSE;   /* was get_aim_dir */
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!c_ptr->m_idx)
				{
#ifdef JP
msg_print("何もない場所に口づけた。");
#else
					msg_print("You kiss into thin air.");
#endif

					break;
				}

#ifdef JP
msg_print("あなたは情熱的なキスをする...");
#else
				msg_print("You kiss passionately...");
#endif

				project_length = 1;
				dummy = plev * 3;
				if (fire_ball(GF_NEW_DRAIN, dir, dummy, 0, FALSE))
				{
					c_ptr = &cave[y][x];
					if (c_ptr->m_idx)
						project(0, 0, y, x, plev * 2, GF_OLD_STONE, PROJECT_KILL, MODIFY_ELEM_MODE_MELEE);
				}
			}
			break;

		case RACE_SKELETON:
#ifdef JP
msg_print("あなたは失ったエネルギーを取り戻そうと試みた。");
#else
			msg_print("You attempt to restore your lost energies.");
#endif

			(void)restore_level();
			break;

		case RACE_GHOST:
			if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
msg_print("あなたはおどろおどろしい叫び声をあげた！");
#else
			msg_print("You emit an eldritch howl!");
#endif
			(void)fear_monster(dir, plev);
			break;

		case RACE_PUMPKINHEAD:
			if (command == -1)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				msg_print("ちょっと腐った魔法のカボチャを投げつけた。");
				fire_ball(GF_PHYSICAL, dir, damroll(2 + plev / 8, 8),
					(p_ptr->stat_use[A_INT] >= (18 + 150)) ? 3 : 2, FALSE);
			}
			else if (command == -2)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball_hide(GF_HAND_DOOM, dir, plev + randint1(plev * 2), 0, FALSE);
			}
			else if (command == -3)
			{
				project(0, 4, py, px,
					p_ptr->mhp, GF_MISSILE, PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
				take_hit(DAMAGE_USELIFE, p_ptr->mhp / 4, "自殺的なパンプキンボム");
#else
				take_hit(DAMAGE_USELIFE, p_ptr->mhp / 4, "a suicidal Pumpkin Bomb");
#endif
			}
			break;

		case RACE_GORGON:
			if (p_ptr->blind)
			{
#ifdef JP
				msg_print("目が見えないので邪眼は使えない。");
#else
				msg_print("You cannot use the stone gaze because you are blind.");
#endif
				return FALSE;
			}
#ifdef JP
			msg_print("邪眼で周囲を見渡した。");
#else
			msg_print("You look around with stone gaze.");
#endif
			stone_gaze(0);
			break;

		case RACE_MERMAID:
			switch (command)
			{
			case -1:
				stop_singing();
				energy_use = 10;
				break;

			case -2:
				switch (cave[py][px].feat)
				{
				case FEAT_FLOOR:
				case FEAT_DARK_PIT:
				case FEAT_DIRT:
				case FEAT_GRASS:
				case FEAT_FLOWER:
				case FEAT_DEEP_GRASS:
				case FEAT_SWAMP:
				case FEAT_TUNDRA:
					cave_set_feat(py, px, FEAT_SHAL_WATER);
					set_aquatic_in_water();
					break;
				default:
					msg_print("ここは水溜まりに変えられない。");
					return FALSE;
				}
				break;

			case -3:
				if (p_ptr->anti_magic)
				{
#ifdef JP
					msg_print("反魔法バリアが歌を邪魔した！");
#else
					msg_print("An anti-magic shell disrupts your singing!");
#endif
					return FALSE;
				}
				if (is_anti_magic_grid(-1, py, px))
				{
#ifdef JP
					msg_print("反魔法フィールドが歌を邪魔した！");
#else
					msg_print("An anti-magic field disrupts your singing!");
#endif
					return TRUE;
				}
				else if (p_ptr->shero)
				{
#ifdef JP
					msg_format("狂戦士化していて頭が回らない！");
#else
					msg_format("You cannot think directly!");
#endif
					return FALSE;
				}

				if (p_ptr->singing) stop_singing();
				msg_print("誘惑のメロディを奏で始めた...");
				p_ptr->singing = MUSIC_TEMPTATION;
				p_ptr->song_start = MUSIC_TEMPTATION;
				set_action(ACTION_SING);
				break;

			case -4:
				mermaid_water_flow();
				break;
			}
			break;

		case RACE_OCTOPUS:
			switch (command)
			{
			case -1:
				switch (cave[py][px].feat)
				{
				case FEAT_FLOOR:
				case FEAT_DARK_PIT:
				case FEAT_DIRT:
				case FEAT_GRASS:
				case FEAT_FLOWER:
				case FEAT_DEEP_GRASS:
				case FEAT_SWAMP:
				case FEAT_TUNDRA:
					cave_set_feat(py, px, FEAT_SHAL_WATER);
					set_aquatic_in_water();
					break;
				default:
					msg_print("ここは水溜まりに変えられない。");
					return FALSE;
				}
				break;

			case -2:
				if (!get_aim_dir(&dir)) return FALSE;
				msg_print("墨を吐いた。");
				fire_ball(GF_CONFUSION, dir, damroll(2 + plev / 8, 8),
					(p_ptr->stat_use[A_CON] >= (18 + 150)) ? -3 : -2, FALSE);
				break;

			case -3:
				if (!word_of_recall()) return FALSE;
				break;

			case -4:
				if (!aqua_diving()) return FALSE;
				break;

			case -5:
				(void)set_tim_octopus_immunity(randint1(25) + 50, FALSE);
				break;

			case -6:
				{
					int y, x;
					cave_type *c_ptr;

					/* Only works on adjacent monsters */
					if (!get_rep_dir(&dir, FALSE)) return FALSE;
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];

					if (!c_ptr->m_idx)
					{
#ifdef JP
						msg_print("そこには誰もいません。");
#else
						msg_print("You see no one there.");
#endif
						break;
					}

					msg_print("吸盤のような口で吸い付いた。");

					project_length = 1;
					(void)fire_ball(GF_NEW_DRAIN, dir, plev * 3, 0, FALSE);
				}
				break;

			case -7:
				if (!alter_with_flood()) return FALSE;
				break;

			case -8:
				{
					int x, y;

					if (!get_rep_dir(&dir, FALSE)) return FALSE;
					y = py + ddy[dir];
					x = px + ddx[dir];
					if (cave[y][x].m_idx)
					{
						py_attack(y, x, 0);
						if (cave[y][x].m_idx)
						{
							handle_stuff();
							py_attack(y, x, 0);
						}
					}
					else
					{
#ifdef JP
						msg_print("その方向にはモンスターはいません。");
#else
						msg_print("You don't see any monster in this direction");
#endif
						return FALSE;
					}
				}
				break;
			}
			break;

		default:
#ifdef JP
msg_print("この種族は特殊な能力を持っていません。");
#else
			msg_print("This race has no bonus power.");
#endif
			return FALSE;
	}
	}
	return TRUE;
}


static bool special_blow_aux(s32b command)
{
	special_blow_type *sb_ptr;
	cave_type *c_ptr = NULL;
	monster_type *m_ptr;
	monster_race *r_ptr = NULL;
	char m_name[80];
	int dir = 0;
	int tx, ty;
	int i;
	char buf[160];
	u32b flg = PROJECT_HIDE | PROJECT_STOP | PROJECT_KILL | PROJECT_GRID;
	bool fear; /* Dummy */
	cptr note_dies;

	command = -32 - command;
	if ((command < 0) || (command >= (MAX_SB + MAX_TEMPLE_SB))) return FALSE;

	if (command >= MAX_SB) sb_ptr = &temple_blow_info[command - MAX_SB];
	else sb_ptr = &special_blow_info[command];

	if (sb_ptr->weapon_type & WT_BIT_BOW)
	{
		if (!(weapon_type_bit(get_weapon_type(&k_info[inventory[INVEN_BOW].k_idx])) & sb_ptr->weapon_type))
		{
#ifdef JP
			msg_print("この必殺技を使うには弓を装備しなければなりません。");
#else
			msg_print("You must equip a bow to use this special blow.");
#endif
			return FALSE;
		}
	}
	if (sb_ptr->weapon_type & ~(WT_BIT_GUN | WT_BIT_BOW))
	{
		if (!(weapon_type_bit(get_weapon_type(&k_info[inventory[INVEN_RARM].k_idx])) & sb_ptr->weapon_type))
		{
			strcpy(buf, "この必殺技を使うには");
			for (i = 1; i <= MAX_WT; i++)
			{
				if (weapon_type_bit(i) & (sb_ptr->weapon_type & ~(WT_BIT_GUN | WT_BIT_BOW)))
				{
					strcat(buf, wt_desc[i]);
					strcat(buf, "か");
				}
			}
			buf[strlen(buf) - 2] = '\0';
			strcat(buf, "を利き腕に装備しなければなりません。");
			msg_print(buf);

			return FALSE;
		}
	}

	if (!get_rep_dir(&dir, FALSE)) return FALSE;

	ty = py;
	tx = px;

	for (i = 1; i <= 2; i++)
	{
		ty += ddy[dir];
		tx += ddx[dir];

		if (!in_bounds(ty, tx)) break;

		if (!cave_floor_bold(ty, tx)) break;

		c_ptr = &cave[ty][tx];

		/* Found target */
		if (c_ptr->m_idx)
		{
			m_ptr = &m_list[c_ptr->m_idx];
			r_ptr = &r_info[m_ptr->r_idx];

			/* Get the monster name (BEFORE polymorphing) */
			monster_desc(m_name, m_ptr, 0);
			break;
		}
	}

	/*
	 *  0- 9: "Common" special blow
	 * 10-17: Only for Temple-Knights
	 */
	switch (command)
	{
	case 0:
		msg_print("わが魂を鬼神に捧げよう…いけーッ！！鬼哭血散斬ッ！！");
		project(0, 0, ty, tx, p_ptr->chp, GF_PHYSICAL, flg, MODIFY_ELEM_MODE_MAGIC);
		take_hit(DAMAGE_USELIFE, p_ptr->chp / 4, "自殺的な鬼哭血散斬");
		break;

	case 1:
		msg_print("わが力、炎に変えて刃に宿らさん…くらえーッ！覇王獄炎波ッ！！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_WIS] * 2, GF_PURE_FIRE, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 2:
		msg_print("わが修練の成果を見せてやろう…いくぞッ！！神鳴明王剣ッ！！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_DEX] * 2, GF_PHYSICAL, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 3:
		msg_print("わが刃に太古の雷獣を宿らそう…つらぬけーッ！天聖雷妙波ッ！！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_WIS] * 2, GF_PURE_WIND, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 4:
		msg_print("この拳にすべての力をそそぎ込もう…思い知れーッ！！怒号魔破拳ッ！！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_STR] * 2, GF_DISINTEGRATE, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 5:
		msg_print("わが剣は竜となり嵐を巻き起こす…、うけてみよッ！竜牙烈風剣ッ！！");
		{
			object_type *o_ptr = &inventory[INVEN_RARM];
			project(0, 0, ty, tx, o_ptr->dd * o_ptr->ds * 10 + o_ptr->to_d, GF_PHYSICAL, flg, MODIFY_ELEM_MODE_MAGIC);
		}
		break;

	case 6:
		msg_print("血の通わぬ石塊に変えてやろう…覚悟しろッ！月花地霊斬ッ！！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_STR], GF_SPECIAL_STONE, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 7:
		msg_print("風よ、わが刃に宿り、その力を示せ…うなれーッ！！風烈天破斬ッ！！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_CON], GF_PHYSICAL, flg, MODIFY_ELEM_MODE_MAGIC);
		if (c_ptr->m_idx) knock_back(0, ty, tx, 200);
		break;

	case 8:
		msg_print("次元の彼方へ吹き飛ばしてやろう…消えされーッ！波動次元斬ッ！！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_INT] * 2, GF_OLD_DRAIN, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 9:
		msg_print("わが苦痛の代償をいただこう…これでおわりだッ！双魔邪王剣ッ！！");
		/* Word of Pain, drain mode */
		project(0, 0, ty, tx, 1, GF_WORD_OF_PAIN, flg, MODIFY_ELEM_MODE_MELEE);
		break;

	case 10:
		msg_print("貴様のツラは見飽きたよ…。終わりにしようぜ！ブラックプリズン！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_STR] * 2, GF_DARK, flg, MODIFY_ELEM_MODE_MAGIC);
		if (c_ptr->m_idx) project(0, 0, ty, tx, p_ptr->lev + 100, GF_STASIS, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 11:
		msg_print("根性だけは認めてあげるわ…。でも、そこまでよ、デーモンローズ！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_CHR] * 2, GF_PURE_FIRE, flg, MODIFY_ELEM_MODE_MAGIC);
		if (c_ptr->m_idx) project(0, 0, ty, tx, p_ptr->lev + 100, GF_CHARM, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 12:
		msg_print("このオレの奥義が見たいか！？いい度胸だ。デスアベンジャー！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_CON] * 2, GF_PURE_EARTH, flg, MODIFY_ELEM_MODE_MAGIC);
		if (c_ptr->m_idx) knock_back(0, ty, tx, 200);
		break;

	case 13:
		msg_print("このオレとサシで戦おうなんざ、10年早ぇんだよ！バカめッ！フローヴェノム！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_DEX] * 2, GF_WATER, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 14:
		msg_print("フン！命知らずの愚か者めッ！我が奥義を防げるかな？いくぞッ！サンダーブレイド！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_STR] * 2, GF_ELEC, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 15:
		msg_print("真の騎士の誇りを貴様に見せてやろう！くらえッ！フレイミングデス！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_WIS] * 2, GF_PURE_FIRE, flg, MODIFY_ELEM_MODE_MAGIC);
		break;

	case 16:
		msg_print("ロスローリアンの力をなめてもらっては困るのだよ…。ライアットバーン！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_WIS] * 2, GF_HOLY_FIRE, flg, MODIFY_ELEM_MODE_MAGIC);
		if (c_ptr->m_idx)
		{
			if ((r_ptr->flags3 & RF3_UNDEAD) &&
			   !(r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) &&
			   !(r_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2)) &&
			   !p_ptr->inside_arena && !p_ptr->inside_quest)
			{
				delete_monster_idx(c_ptr->m_idx);
#ifdef JP
				msg_format("%sは消滅した！", m_name);
#else
				msg_format("%^s disappered!", m_name);
#endif
			}
		}
		break;

	case 17:
		msg_print("ローディスに逆らう愚か者め…。我が奥義を受けてみよ！アポカリプス！");
		project(0, 0, ty, tx, p_ptr->stat_use[A_INT] * 2, GF_OLD_DRAIN, flg, MODIFY_ELEM_MODE_MAGIC);
		break;
	}

	return TRUE;
}


/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	power_desc_type power_desc[36];
	int             num, i = 0;
	int             ask = TRUE;
	int             lvl = p_ptr->lev;
	int             mhp = p_ptr->mhp;
	int             chp = p_ptr->chp;
	bool            flag, redraw, cast = FALSE;
	char            choice;
	char            out_val[160];
	int menu_line = (use_menu ? 1 : 0);


	for (num = 0; num < 36; num++)
	{
		strcpy(power_desc[num].name, "");
		power_desc[num].number = 0;
	}

	num = 0;

	if (p_ptr->confused)
	{
#ifdef JP
msg_print("混乱していて特殊能力を使えません！");
#else
		msg_print("You are too confused to use any powers!");
#endif

		energy_use = 0;
		return;
	}

	if (!(rp_ptr->r_flags & PRF_LARGE)
		&&(p_ptr->pclass != CLASS_TERRORKNIGHT)
		&& (p_ptr->pclass != CLASS_SWORDMASTER)
		&& (p_ptr->pclass != CLASS_NINJA))
	{
#ifdef JP
		strcpy(power_desc[num].name, "投石");
#else
		strcpy(power_desc[num].name, "Throw Pebble");
#endif

		power_desc[num].level = 1;
		power_desc[num].cost = 0;
		power_desc[num].stat = A_STR;
		power_desc[num].fail = 0;
		power_desc[num++].number = -9;
	}

	if ((p_ptr->pclass == CLASS_KNIGHT)
		|| (p_ptr->pclass == CLASS_VALKYRIE))
	{
#ifdef JP
		strcpy(power_desc[num].name, "荒馬ならし");
#else
		strcpy(power_desc[num].name, "Rodeo");
#endif

		power_desc[num].level = 10;
		power_desc[num].cost = 0;
		power_desc[num].stat = A_STR;
		power_desc[num].fail = 10;
		power_desc[num++].number = -10;
	}

	if ((p_ptr->pclass == CLASS_WIZARD)
		|| (p_ptr->pclass == CLASS_SIRENE)
		|| (p_ptr->pclass == CLASS_LICH)
		|| (p_ptr->pclass == CLASS_HIGHWITCH))
	{
#ifdef JP
		strcpy(power_desc[num].name, "魔力食い");
#else
		strcpy(power_desc[num].name, "Eat Magic");
#endif

		power_desc[num].level = 25;
		power_desc[num].cost = 1;
		power_desc[num].stat = A_INT;
		power_desc[num].fail = 25;
		power_desc[num++].number = -10;
	}

	switch (p_ptr->pclass)
	{
	case CLASS_KNIGHT:
	{
#ifdef JP
		strcpy(power_desc[num].name, "邪悪存在感知");
#else
		strcpy(power_desc[num].name, "Detect Evil");
#endif

		power_desc[num].level = 10;
		power_desc[num].cost = 5;
		power_desc[num].stat = A_WIS;
		power_desc[num].fail = 20;
		power_desc[num++].number = -11;
#ifdef JP
		strcpy(power_desc[num].name, "突撃");
#else
		strcpy(power_desc[num].name, "Charge");
#endif

		power_desc[num].level = 20;
		power_desc[num].cost = 40;
		power_desc[num].stat = A_STR;
		power_desc[num].fail = 60;
		power_desc[num++].number = -12;
#ifdef JP
		strcpy(power_desc[num].name, "戦士の目");
#else
		strcpy(power_desc[num].name, "Warrior's Eye");
#endif

		power_desc[num].level = 30;
		power_desc[num].cost = 20;
		power_desc[num].stat = A_INT;
		power_desc[num].fail = 60;
		power_desc[num++].number = -13;
		break;
	}
	case CLASS_BERSERKER:
	{
#ifdef JP
		strcpy(power_desc[num].name, "とんずら");
#else
		strcpy(power_desc[num].name, "Escape");
#endif

		power_desc[num].level = 10;
		power_desc[num].cost = 5;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 30;
		power_desc[num++].number = -10;
		break;
	}
	case CLASS_TERRORKNIGHT:
	{
#ifdef JP
		strcpy(power_desc[num].name, "獲物感知");
#else
		strcpy(power_desc[num].name, "Detect Victim");
#endif

		power_desc[num].level = 5;
		power_desc[num].cost = 1;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 20;
		power_desc[num++].number = -9;
#ifdef JP
		strcpy(power_desc[num].name, "墓石投げ");
#else
		strcpy(power_desc[num].name, "Throw Grave");
#endif

		power_desc[num].level = 10;
		power_desc[num].cost = 2;
		power_desc[num].stat = A_STR;
		power_desc[num].fail = 20;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "殺しの勲章");
#else
		strcpy(power_desc[num].name, "Killing Award");
#endif

		power_desc[num].level = 25;
		power_desc[num].cost = 20;
		power_desc[num].stat = A_STR;
		power_desc[num].fail = 50;
		power_desc[num++].number = -11;
#ifdef JP
		strcpy(power_desc[num].name, "地震");
#else
		strcpy(power_desc[num].name, "Earthquake");
#endif

		power_desc[num].level = 30;
		power_desc[num].cost = 10;
		power_desc[num].stat = A_STR;
		power_desc[num].fail = 60;
		power_desc[num++].number = -12;
#ifdef JP
		strcpy(power_desc[num].name, "殺人ショー");
#else
		strcpy(power_desc[num].name, "Murder Show");
#endif

		power_desc[num].level = 40;
		power_desc[num].cost = 20;
		power_desc[num].stat = A_STR;
		power_desc[num].fail = 70;
		power_desc[num++].number = -13;
#ifdef JP
		strcpy(power_desc[num].name, "亡霊喰い");
#else
		strcpy(power_desc[num].name, "Phantom Eater");
#endif

		power_desc[num].level = 45;
		power_desc[num].cost = 6;
		power_desc[num].stat = A_CON;
		power_desc[num].fail = 30;
		power_desc[num++].number = -14;
		break;
	}
	case CLASS_BEASTTAMER:
	{
#ifdef JP
		strcpy(power_desc[num].name, "クモ召喚");
#else
		strcpy(power_desc[num].name, "Summon Spider");
#endif

		power_desc[num].level = 10;
		power_desc[num].cost = 10;
		power_desc[num].stat = A_CHR;
		power_desc[num].fail = 30;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "餌付け");
#else
		strcpy(power_desc[num].name, "Feeding");
#endif

		power_desc[num].level = 15;
		power_desc[num].cost = 8;
		power_desc[num].stat = A_CHR;
		power_desc[num].fail = 40;
		power_desc[num++].number = -11;
#ifdef JP
		strcpy(power_desc[num].name, "魔獣召喚");
#else
		strcpy(power_desc[num].name, "Summon Beasts");
#endif

		power_desc[num].level = 35;
		power_desc[num].cost = 30;
		power_desc[num].stat = A_CHR;
		power_desc[num].fail = 50;
		power_desc[num++].number = -12;
		break;
	}
	case CLASS_SWORDMASTER:
	{
#ifdef JP
		strcpy(power_desc[num].name, "指弾");
#else
		strcpy(power_desc[num].name, "Small Bullet");
#endif

		power_desc[num].level = 1;
		power_desc[num].cost = 0;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 0;
		power_desc[num++].number = -9;
#ifdef JP
		strcpy(power_desc[num].name, "殺気感知");
#else
		strcpy(power_desc[num].name, "Detect Ferocity");
#endif

		power_desc[num].level = 1;
		power_desc[num].cost = 1;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 10;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "ブーメラン");
#else
		strcpy(power_desc[num].name, "Boomerang");
#endif

		power_desc[num].level = 6;
		power_desc[num].cost = 15;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 30;
		power_desc[num++].number = -11;
#ifdef JP
		strcpy(power_desc[num].name, "みね打ち");
#else
		strcpy(power_desc[num].name, "Strike to Stun");
#endif

		power_desc[num].level = 12;
		power_desc[num].cost = 10;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 40;
		power_desc[num++].number = -12;
#ifdef JP
		strcpy(power_desc[num].name, "払い抜け");
#else
		strcpy(power_desc[num].name, "Harainuke");
#endif

		power_desc[num].level = 17;
		power_desc[num].cost = 20;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 50;
		power_desc[num++].number = -13;
#ifdef JP
		strcpy(power_desc[num].name, "入身");
#else
		strcpy(power_desc[num].name, "Rush Attack");
#endif

		power_desc[num].level = 24;
		power_desc[num].cost = 30;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 60;
		power_desc[num++].number = -14;
#ifdef JP
		strcpy(power_desc[num].name, "無双三段");
#else
		strcpy(power_desc[num].name, "Musou-Sandan");
#endif

		power_desc[num].level = 39;
		power_desc[num].cost = 80;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 80;
		power_desc[num++].number = -15;
		break;
	}
	case CLASS_NINJA:
	{
#ifdef JP
		strcpy(power_desc[num].name, "手裏剣");
#else
		strcpy(power_desc[num].name, "Syuriken");
#endif

		power_desc[num].level = 1;
		power_desc[num].cost = 0;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 0;
		power_desc[num++].number = -9;
#ifdef JP
		strcpy(power_desc[num].name, "忍び足");
#else
		strcpy(power_desc[num].name, "Stealth Walk");
#endif

		power_desc[num].level = 5;
		power_desc[num].cost = 7;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 10;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "ヒット&アウェイ");
#else
		strcpy(power_desc[num].name, "Hit and Away");
#endif

		power_desc[num].level = 8;
		power_desc[num].cost = 12;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 14;
		power_desc[num++].number = -11;
		break;
	}
	case CLASS_WARLOCK:
	{
#ifdef JP
		strcpy(power_desc[num].name, "*鑑定*");
#else
		strcpy(power_desc[num].name, "Identify True");
#endif

		power_desc[num].level = 30;
		power_desc[num].cost = 20;
		power_desc[num].stat = A_INT;
		power_desc[num].fail = 60;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "ゴーレム創造");
#else
		strcpy(power_desc[num].name, "Create Golem");
#endif

		power_desc[num].level = 30;
		power_desc[num].cost = 30;
		power_desc[num].stat = A_INT;
		power_desc[num].fail = 80;
		power_desc[num++].number = -11;
		break;
	}
	case CLASS_VALKYRIE:
	{
#ifdef JP
		strcpy(power_desc[num].name, "貫通");
#else
		strcpy(power_desc[num].name, "Penetration");
#endif

		power_desc[num].level = 9;
		power_desc[num].cost = 16;
		power_desc[num].stat = A_STR;
		power_desc[num].fail = 30;
		power_desc[num++].number = -11;
		break;
	}
	case CLASS_ARCHER:
	{
#ifdef JP
		strcpy(power_desc[num].name, "弾/矢の製造");
#else
		strcpy(power_desc[num].name, "Create Ammo");
#endif

		power_desc[num].level = 10;
		power_desc[num].cost = 0;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 0;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "狙い撃ち");
#else
		strcpy(power_desc[num].name, "Sniping (1)");
#endif

		power_desc[num].level = 10;
		power_desc[num].cost = 5;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 20;
		power_desc[num++].number = -11;
#ifdef JP
		strcpy(power_desc[num].name, "狙撃");
#else
		strcpy(power_desc[num].name, "Sniping (2)");
#endif

		power_desc[num].level = 28;
		power_desc[num].cost = 30;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 20;
		power_desc[num++].number = -12;
#ifdef JP
		strcpy(power_desc[num].name, "連射");
#else
		strcpy(power_desc[num].name, "Rapidfire");
#endif

		power_desc[num].level = 35;
		power_desc[num].cost = 50;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 50;
		power_desc[num++].number = -13;
		break;
	}
	case CLASS_DRAGONTAMER:
	{
#ifdef JP
		strcpy(power_desc[num].name, "竜の餌付け");
#else
		strcpy(power_desc[num].name, "Feed Dragon");
#endif

		power_desc[num].level = 25;
		power_desc[num].cost = 15;
		power_desc[num].stat = A_CHR;
		power_desc[num].fail = 40;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "ドラゴン召喚");
#else
		strcpy(power_desc[num].name, "Summon Dragon");
#endif

		power_desc[num].level = 30;
		power_desc[num].cost = 25;
		power_desc[num].stat = A_CHR;
		power_desc[num].fail = 50;
		power_desc[num++].number = -11;
#ifdef JP
		strcpy(power_desc[num].name, "古代ドラゴン召喚");
#else
		strcpy(power_desc[num].name, "Summon Ancient Dragon");
#endif

		power_desc[num].level = 40;
		power_desc[num].cost = 40;
		power_desc[num].stat = A_CHR;
		power_desc[num].fail = 60;
		power_desc[num++].number = -12;
		break;
	}
	case CLASS_LICH:
	{
#ifdef JP
		strcpy(power_desc[num].name, "サモンダークネス");
#else
		strcpy(power_desc[num].name, "Summon Darkness");
#endif

		power_desc[num].level = 40;
		power_desc[num].cost = 40;
		power_desc[num].stat = A_INT;
		power_desc[num].fail = 70;
		power_desc[num++].number = -11;
		break;
	}
	case CLASS_ANGELKNIGHT:
	{
#ifdef JP
		strcpy(power_desc[num].name, "歌を止める");
#else
		strcpy(power_desc[num].name, "Stop Singing");
#endif

		power_desc[num].level = 1;
		power_desc[num].cost = 0;
		power_desc[num].stat = A_CHR;
		power_desc[num].fail = 0;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "サッドソング");
#else
		strcpy(power_desc[num].name, "Sad Song");
#endif

		power_desc[num].level = 28;
		power_desc[num].cost = 20;
		power_desc[num].stat = A_WIS;
		power_desc[num].fail = 50;
		power_desc[num++].number = -11;
#ifdef JP
		strcpy(power_desc[num].name, "サイレントソング");
#else
		strcpy(power_desc[num].name, "Silent Song");
#endif

		power_desc[num].level = 44;
		power_desc[num].cost = 30;
		power_desc[num].stat = A_WIS;
		power_desc[num].fail = 70;
		power_desc[num++].number = -12;
		break;
	}
	case CLASS_HIGHWITCH:
	{
#ifdef JP
		strcpy(power_desc[num].name, "パンプキンヘッド召喚");
#else
		strcpy(power_desc[num].name, "Summon Pumpkin-Heads");
#endif

		power_desc[num].level = 30;
		power_desc[num].cost = 30;
		power_desc[num].stat = A_CHR;
		power_desc[num].fail = 60;
		power_desc[num++].number = -11;
		break;
	}
	case CLASS_GUNNER:
	{
#ifdef JP
		strcpy(power_desc[num].name, "熱源感知");
#else
		strcpy(power_desc[num].name, "Thermal Detection");
#endif

		power_desc[num].level = 1;
		power_desc[num].cost = 1;
		power_desc[num].stat = A_INT;
		power_desc[num].fail = 10;
		power_desc[num++].number = -10;
#ifdef JP
		strcpy(power_desc[num].name, "地形データ取得");
#else
		strcpy(power_desc[num].name, "Feature Data Correcting");
#endif

		power_desc[num].level = 10;
		power_desc[num].cost = 25;
		power_desc[num].stat = A_INT;
		power_desc[num].fail = 40;
		power_desc[num++].number = -11;
#ifdef JP
		strcpy(power_desc[num].name, "高速移動");
#else
		strcpy(power_desc[num].name, "Fast Move");
#endif

		power_desc[num].level = 20;
		power_desc[num].cost = 30;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 0;
		power_desc[num++].number = -12;
#ifdef JP
		strcpy(power_desc[num].name, "調査");
#else
		strcpy(power_desc[num].name, "Probing");
#endif

		power_desc[num].level = 25;
		power_desc[num].cost = 20;
		power_desc[num].stat = A_INT;
		power_desc[num].fail = 50;
		power_desc[num++].number = -13;
#ifdef JP
		strcpy(power_desc[num].name, "狙撃");
#else
		strcpy(power_desc[num].name, "Sniping");
#endif

		power_desc[num].level = 28;
		power_desc[num].cost = 30;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 20;
		power_desc[num++].number = -14;
#ifdef JP
		strcpy(power_desc[num].name, "ジャンプウォール");
#else
		strcpy(power_desc[num].name, "Jump Wall");
#endif

		power_desc[num].level = 30;
		power_desc[num].cost = 35;
		power_desc[num].stat = A_DEX;
		power_desc[num].fail = 40;
		power_desc[num++].number = -15;
	}
#if 0
	default:
#ifdef JP
strcpy(power_desc[0].name, "(なし)");
#else
		strcpy(power_desc[0].name, "(none)");
#endif
#endif

	}

	switch (p_ptr->prace)
	{
		case RACE_HAWKMAN:
			if (p_ptr->pelem == ELEM_WIND)
			{
#ifdef JP
strcpy(power_desc[num].name, "ウィンドショット");
#else
				strcpy(power_desc[num].name, "Wind Shot");
#endif
				power_desc[num].level = 6;
				power_desc[num].cost = 8;
				power_desc[num].stat = A_DEX;
				power_desc[num].fail = 25;
				power_desc[num++].number = -1;
			}
			break;
		case RACE_LIZARDMAN:
			sprintf(power_desc[num].name,
#ifdef JP
			        "ブレス (d. %d+d%d)",
#else
			        "Breath (d. %d+d%d)",
#endif
			        chp / ((p_ptr->pelem == ELEM_AQUA) ? 3 : 10), chp / 10);

			power_desc[num].level = 1;
			power_desc[num].cost = mhp / 10;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 12;
			power_desc[num++].number = -1;
			break;
		case RACE_FAIRY:
#ifdef JP
strcpy(power_desc[num].name, "眠り粉");
#else
			strcpy(power_desc[num].name, "Sleeping Dust");
#endif

			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 15;
			power_desc[num++].number = -1;
			break;
		case RACE_GREMLIN:
#ifdef JP
strcpy(power_desc[num].name, "ディープキッス");
#else
			strcpy(power_desc[num].name, "Deep Kiss");
#endif

			power_desc[num].level = 40;
			power_desc[num].cost = 20;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 70;
			power_desc[num++].number = -1;
			break;
		case RACE_SKELETON:
#ifdef JP
strcpy(power_desc[num].name, "経験値復活");
#else
			strcpy(power_desc[num].name, "Restore Life");
#endif

			power_desc[num].level = 30;
			power_desc[num].cost = 30;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 18;
			power_desc[num++].number = -1;
			break;
		case RACE_GHOST:
#ifdef JP
strcpy(power_desc[num].name, "モンスター恐慌");
#else
			strcpy(power_desc[num].name, "Scare Monster");
#endif

			power_desc[num].level = 4;
			power_desc[num].cost = 6;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 3;
			power_desc[num++].number = -1;
			break;
		case RACE_PUMPKINHEAD:
#ifdef JP
strcpy(power_desc[num].name, "かぼちゃうぉーず");
#else
			strcpy(power_desc[num].name, "Pumpkin Wars");
#endif

			power_desc[num].level = 2;
			power_desc[num].cost = 2;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 10;
			power_desc[num++].number = -1;
#ifdef JP
strcpy(power_desc[num].name, "ドラッグイーター");
#else
			strcpy(power_desc[num].name, "Drug Eater");
#endif

			power_desc[num].level = 40;
			power_desc[num].cost = 80;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 80;
			power_desc[num++].number = -2;
#ifdef JP
strcpy(power_desc[num].name, "パンプキンボム");
#else
			strcpy(power_desc[num].name, "Pumpkin Bomb");
#endif

			power_desc[num].level = 45;
			power_desc[num].cost = 90;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 75;
			power_desc[num++].number = -3;
			break;
		case RACE_GORGON:
#ifdef JP
strcpy(power_desc[num].name, "邪眼");
#else
			strcpy(power_desc[num].name, "Stone Gaze");
#endif

			power_desc[num].level = 25;
			power_desc[num].cost = 30;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 60;
			power_desc[num++].number = -1;
			break;
		case RACE_MERMAID:
#ifdef JP
			strcpy(power_desc[num].name, "歌を止める");
#else
			strcpy(power_desc[num].name, "Stop Singing");
#endif

			power_desc[num].level = 1;
			power_desc[num].cost = 0;
			power_desc[num].stat = A_CHR;
			power_desc[num].fail = 0;
			power_desc[num++].number = -1;
			strcpy(power_desc[num].name, "水溜まり");

			power_desc[num].level = 1;
			power_desc[num].cost = 1;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 0;
			power_desc[num++].number = -2;

			strcpy(power_desc[num].name, "魅惑の歌");

			power_desc[num].level = 14;
			power_desc[num].cost = 28;
			power_desc[num].stat = A_CHR;
			power_desc[num].fail = 25;
			power_desc[num++].number = -3;

			strcpy(power_desc[num].name, "浸水");

			power_desc[num].level = 26;
			power_desc[num].cost = 40;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 50;
			power_desc[num++].number = -4;
			break;
		case RACE_OCTOPUS:
			strcpy(power_desc[num].name, "水溜まり");

			power_desc[num].level = 1;
			power_desc[num].cost = 1;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 0;
			power_desc[num++].number = -1;
			strcpy(power_desc[num].name, "墨を吐く");

			power_desc[num].level = 2;
			power_desc[num].cost = 4;
			power_desc[num].stat = A_STR;
			power_desc[num].fail = 5;
			power_desc[num++].number = -2;
#ifdef JP
			strcpy(power_desc[num].name, "帰還");
#else
			strcpy(power_desc[num].name, "Recall");
#endif

			power_desc[num].level = 5;
			power_desc[num].cost = 10;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 10;
			power_desc[num++].number = -3;
			strcpy(power_desc[num].name, "潜水");

			power_desc[num].level = 26;
			power_desc[num].cost = 30;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 45;
			power_desc[num++].number = -4;
			strcpy(power_desc[num].name, "タコ壺");

			power_desc[num].level = 32;
			power_desc[num].cost = 70;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 55;
			power_desc[num++].number = -5;
			strcpy(power_desc[num].name, "タコちゅー");

			power_desc[num].level = 34;
			power_desc[num].cost = 25;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 60;
			power_desc[num++].number = -6;
			strcpy(power_desc[num].name, "大洪水");

			power_desc[num].level = 38;
			power_desc[num].cost = 100;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 60;
			power_desc[num++].number = -7;
			strcpy(power_desc[num].name, "タコ足千手");

			power_desc[num].level = 47;
			power_desc[num].cost = 200;
			power_desc[num].stat = A_STR;
			power_desc[num].fail = 70;
			power_desc[num++].number = -8;
			break;
		default:
		{
			break;
		}
	}

	if (p_ptr->special_blow)
	{
		special_blow_type *sb_ptr;

		for (i = 0; i < MAX_SB + MAX_TEMPLE_SB; i++)
		{
			if (p_ptr->special_blow & (0x00000001L << i))
			{
				if (i >= MAX_SB) sb_ptr = &temple_blow_info[i - MAX_SB];
				else sb_ptr = &special_blow_info[i];

				strcpy(power_desc[num].name, sb_ptr->name);

				power_desc[num].level = sb_ptr->level;
				power_desc[num].cost = sb_ptr->cost;
				power_desc[num].stat = sb_ptr->stat;
				power_desc[num].fail = sb_ptr->fail;
				power_desc[num++].number = -32 - i;
			}
		}
	}

	if (p_ptr->muta1)
	{
		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
#ifdef JP
strcpy(power_desc[num].name, "酸の唾");
#else
			strcpy(power_desc[num].name, "Spit Acid");
#endif

			power_desc[num].level = 9;
			power_desc[num].cost = 9;
			power_desc[num].stat = A_DEX;
			power_desc[num].fail = 15;
			power_desc[num++].number = MUT1_SPIT_ACID;
		}

		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
#ifdef JP
strcpy(power_desc[num].name, "炎のブレス");
#else
			strcpy(power_desc[num].name, "Fire Breath");
#endif

			power_desc[num].level = 20;
			power_desc[num].cost = lvl;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 18;
			power_desc[num++].number = MUT1_BR_FIRE;
		}

		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
#ifdef JP
strcpy(power_desc[num].name, "催眠睨み");
#else
			strcpy(power_desc[num].name, "Hypnotic Gaze");
#endif

			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].stat = A_CHR;
			power_desc[num].fail = 18;
			power_desc[num++].number = MUT1_HYPN_GAZE;
		}

		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
#ifdef JP
strcpy(power_desc[num].name, "念動力");
#else
			strcpy(power_desc[num].name, "Telekinesis");
#endif

			power_desc[num].level = 9;
			power_desc[num].cost = 9;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 14;
			power_desc[num++].number = MUT1_TELEKINES;
		}

		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
#ifdef JP
strcpy(power_desc[num].name, "テレポート");
#else
			strcpy(power_desc[num].name, "Teleport");
#endif

			power_desc[num].level = 7;
			power_desc[num].cost = 7;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 15;
			power_desc[num++].number = MUT1_VTELEPORT;
		}

		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
#ifdef JP
strcpy(power_desc[num].name, "精神攻撃");
#else
			strcpy(power_desc[num].name, "Mind Blast");
#endif

			power_desc[num].level = 5;
			power_desc[num].cost = 3;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 15;
			power_desc[num++].number = MUT1_MIND_BLST;
		}

		if (p_ptr->muta1 & MUT1_RADIATION)
		{
#ifdef JP
strcpy(power_desc[num].name, "放射能");
#else
			strcpy(power_desc[num].name, "Emit Radiation");
#endif

			power_desc[num].level = 15;
			power_desc[num].cost = 15;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 14;
			power_desc[num++].number = MUT1_RADIATION;
		}

		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
#ifdef JP
strcpy(power_desc[num].name, "吸血ドレイン");
#else
			strcpy(power_desc[num].name, "Vampiric Drain");
#endif

			power_desc[num].level = 2;
			power_desc[num].cost = (1 + (lvl / 3));
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 9;
			power_desc[num++].number = MUT1_VAMPIRISM;
		}

		if (p_ptr->muta1 & MUT1_SMELL_MET)
		{
#ifdef JP
strcpy(power_desc[num].name, "金属嗅覚");
#else
			strcpy(power_desc[num].name, "Smell Metal");
#endif

			power_desc[num].level = 3;
			power_desc[num].cost = 2;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 12;
			power_desc[num++].number = MUT1_SMELL_MET;
		}

		if (p_ptr->muta1 & MUT1_SMELL_MON)
		{
#ifdef JP
strcpy(power_desc[num].name, "敵臭嗅覚");
#else
			strcpy(power_desc[num].name, "Smell Monsters");
#endif

			power_desc[num].level = 5;
			power_desc[num].cost = 4;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 15;
			power_desc[num++].number = MUT1_SMELL_MON;
		}

		if (p_ptr->muta1 & MUT1_BLINK)
		{
#ifdef JP
strcpy(power_desc[num].name, "ショート・テレポート");
#else
			strcpy(power_desc[num].name, "Blink");
#endif

			power_desc[num].level = 3;
			power_desc[num].cost = 3;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 12;
			power_desc[num++].number = MUT1_BLINK;
		}

		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
#ifdef JP
strcpy(power_desc[num].name, "岩食い");
#else
			strcpy(power_desc[num].name, "Eat Rock");
#endif

			power_desc[num].level = 8;
			power_desc[num].cost = 12;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 18;
			power_desc[num++].number = MUT1_EAT_ROCK;
		}

		if (p_ptr->muta1 & MUT1_SWAP_POS)
		{
#ifdef JP
strcpy(power_desc[num].name, "位置交換");
#else
			strcpy(power_desc[num].name, "Swap Position");
#endif

			power_desc[num].level = 15;
			power_desc[num].cost = 12;
			power_desc[num].stat = A_DEX;
			power_desc[num].fail = 16;
			power_desc[num++].number = MUT1_SWAP_POS;
		}

		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
#ifdef JP
strcpy(power_desc[num].name, "叫び");
#else
			strcpy(power_desc[num].name, "Shriek");
#endif

			power_desc[num].level = 20;
			power_desc[num].cost = 14;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 16;
			power_desc[num++].number = MUT1_SHRIEK;
		}

		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
#ifdef JP
strcpy(power_desc[num].name, "照明");
#else
			strcpy(power_desc[num].name, "Illuminate");
#endif

			power_desc[num].level = 3;
			power_desc[num].cost = 2;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 10;
			power_desc[num++].number = MUT1_ILLUMINE;
		}

		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
#ifdef JP
strcpy(power_desc[num].name, "呪い感知");
#else
			strcpy(power_desc[num].name, "Detect Curses");
#endif

			power_desc[num].level = 7;
			power_desc[num].cost = 14;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 14;
			power_desc[num++].number = MUT1_DET_CURSE;
		}

		if (p_ptr->muta1 & MUT1_BERSERK)
		{
#ifdef JP
strcpy(power_desc[num].name, "狂戦士化");
#else
			strcpy(power_desc[num].name, "Berserk");
#endif

			power_desc[num].level = 8;
			power_desc[num].cost = 8;
			power_desc[num].stat = A_STR;
			power_desc[num].fail = 14;
			power_desc[num++].number = MUT1_BERSERK;
		}

		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
#ifdef JP
strcpy(power_desc[num].name, "変身");
#else
			strcpy(power_desc[num].name, "Polymorph");
#endif

			power_desc[num].level = 18;
			power_desc[num].cost = 20;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 18;
			power_desc[num++].number = MUT1_POLYMORPH;
		}

		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
#ifdef JP
strcpy(power_desc[num].name, "ミダスの手");
#else
			strcpy(power_desc[num].name, "Midas Touch");
#endif

			power_desc[num].level = 10;
			power_desc[num].cost = 5;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 12;
			power_desc[num++].number = MUT1_MIDAS_TCH;
		}

		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
#ifdef JP
strcpy(power_desc[num].name, "カビ発生");
#else
			strcpy(power_desc[num].name, "Grow Mold");
#endif

			power_desc[num].level = 1;
			power_desc[num].cost = 6;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 14;
			power_desc[num++].number = MUT1_GROW_MOLD;
		}

		if (p_ptr->muta1 & MUT1_RESIST)
		{
#ifdef JP
strcpy(power_desc[num].name, "エレメント耐性");
#else
			strcpy(power_desc[num].name, "Resist Elements");
#endif

			power_desc[num].level = 10;
			power_desc[num].cost = 12;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 12;
			power_desc[num++].number = MUT1_RESIST;
		}

		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
#ifdef JP
strcpy(power_desc[num].name, "地震");
#else
			strcpy(power_desc[num].name, "Earthquake");
#endif

			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].stat = A_STR;
			power_desc[num].fail = 16;
			power_desc[num++].number = MUT1_EARTHQUAKE;
		}

		if (p_ptr->muta1 & MUT1_EAT_MAGIC)
		{
#ifdef JP
strcpy(power_desc[num].name, "魔力食い");
#else
			strcpy(power_desc[num].name, "Eat Magic");
#endif

			power_desc[num].level = 17;
			power_desc[num].cost = 1;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 15;
			power_desc[num++].number = MUT1_EAT_MAGIC;
		}

		if (p_ptr->muta1 & MUT1_WEIGH_MAG)
		{
#ifdef JP
strcpy(power_desc[num].name, "魔力感知");
#else
			strcpy(power_desc[num].name, "Weigh Magic");
#endif

			power_desc[num].level = 6;
			power_desc[num].cost = 6;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 10;
			power_desc[num++].number = MUT1_WEIGH_MAG;
		}

		if (p_ptr->muta1 & MUT1_STERILITY)
		{
#ifdef JP
strcpy(power_desc[num].name, "増殖阻止");
#else
			strcpy(power_desc[num].name, "Sterilize");
#endif

			power_desc[num].level = 12;
			power_desc[num].cost = 23;
			power_desc[num].stat = A_CHR;
			power_desc[num].fail = 15;
			power_desc[num++].number = MUT1_STERILITY;
		}

		if (p_ptr->muta1 & MUT1_PANIC_HIT)
		{
#ifdef JP
strcpy(power_desc[num].name, "ヒット＆アウェイ");
#else
			strcpy(power_desc[num].name, "Panic Hit");
#endif

			power_desc[num].level = 10;
			power_desc[num].cost = 12;
			power_desc[num].stat = A_DEX;
			power_desc[num].fail = 14;
			power_desc[num++].number = MUT1_PANIC_HIT;
		}

		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
#ifdef JP
strcpy(power_desc[num].name, "眩惑");
#else
			strcpy(power_desc[num].name, "Dazzle");
#endif

			power_desc[num].level = 7;
			power_desc[num].cost = 15;
			power_desc[num].stat = A_CHR;
			power_desc[num].fail = 8;
			power_desc[num++].number = MUT1_DAZZLE;
		}

		if (p_ptr->muta1 & MUT1_LASER_EYE)
		{
#ifdef JP
strcpy(power_desc[num].name, "レーザー・アイ");
#else
			strcpy(power_desc[num].name, "Laser Eye");
#endif

			power_desc[num].level = 7;
			power_desc[num].cost = 10;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 9;
			power_desc[num++].number = MUT1_LASER_EYE;
		}

		if (p_ptr->muta1 & MUT1_RECALL)
		{
#ifdef JP
strcpy(power_desc[num].name, "帰還");
#else
			strcpy(power_desc[num].name, "Recall");
#endif

			power_desc[num].level = 17;
			power_desc[num].cost = 50;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 16;
			power_desc[num++].number = MUT1_RECALL;
		}

		if (p_ptr->muta1 & MUT1_BANISH)
		{
#ifdef JP
strcpy(power_desc[num].name, "邪悪消滅");
#else
			strcpy(power_desc[num].name, "Banish Evil");
#endif

			power_desc[num].level = 25;
			power_desc[num].cost = 25;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 18;
			power_desc[num++].number = MUT1_BANISH;
		}

		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
#ifdef JP
strcpy(power_desc[num].name, "凍結の手");
#else
			strcpy(power_desc[num].name, "Cold Touch");
#endif

			power_desc[num].level = 2;
			power_desc[num].cost = 2;
			power_desc[num].stat = A_CON;
			power_desc[num].fail = 11;
			power_desc[num++].number = MUT1_COLD_TOUCH;
		}

		if (p_ptr->muta1 & MUT1_LAUNCHER)
		{
#ifdef JP
strcpy(power_desc[num].name, "アイテム投げ");
#else
			strcpy(power_desc[num].name, "Throw Object");
#endif

			power_desc[num].level = 1;
			power_desc[num].cost = lvl;
			power_desc[num].stat = A_STR;
			power_desc[num].fail = 6;
			/* XXX_XXX_XXX Hack! MUT1_LAUNCHER counts as negative... */
			power_desc[num++].number = 3;
		}
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt */
#ifdef JP
(void) strnfmt(out_val, 78, "(特殊能力 %c-%c, *'で一覧, ESCで中断) どの特殊能力を使いますか？",
#else
	(void)strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
#endif

		I2A(0), (num <= 26) ? I2A(num - 1) : '0' + num - 27);

#ifdef ALLOW_REPEAT
if (!repeat_pull(&i) || i<0 || i>=num) {
#endif /* ALLOW_REPEAT */
	if (use_menu) screen_save();
	 /* Get a spell from the user */

	choice = (always_show_list || use_menu) ? ESCAPE:1;
	while (!flag)
	{
		if( choice==ESCAPE ) choice = ' '; 
		else if( !get_com(out_val, &choice, FALSE) )break; 

		if (use_menu && choice != ' ')
		{
			switch(choice)
			{
				case '0':
				{
					screen_load();
					energy_use = 0;
					return;
				}

				case '8':
				case 'k':
				case 'K':
				{
					menu_line += (num - 1);
					break;
				}

				case '2':
				case 'j':
				case 'J':
				{
					menu_line++;
					break;
				}

				case '6':
				case 'l':
				case 'L':
				case '4':
				case 'h':
				case 'H':
				{
					if (menu_line > 18)
						menu_line -= 18;
					else if (menu_line+18 <= num)
						menu_line += 18;
					break;
				}

				case 'x':
				case 'X':
				case '\r':
				{
					i = menu_line - 1;
					ask = FALSE;
					break;
				}
			}
			if (menu_line > num) menu_line -= num;
		}
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?') || (use_menu && ask))
		{
			/* Show the list */
			if (!redraw || use_menu)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];
				char letter;
				int x1, y1;

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				if (!use_menu) screen_save();

				/* Print header(s) */
				if (num < 18)
#ifdef JP
prt("                            Lv   MP 失率", y++, x);
#else
					prt("                            Lv Cost Fail", y++, x);
#endif

				else
#ifdef JP
prt("                            Lv   MP 失率                            Lv   MP 失率", y++, x);
#else
					prt("                            Lv Cost Fail                            Lv Cost Fail", y++, x);
#endif


				/* Print list */
				while (ctr < num)
				{
					x1 = ((ctr < 18) ? x : x + 40);
					y1 = ((ctr < 18) ? y + ctr : y + ctr - 18);

					if (use_menu)
					{
#ifdef JP
						if (ctr == (menu_line-1)) strcpy(dummy, " 》 ");
#else
						if (ctr == (menu_line-1)) strcpy(dummy, " >  ");
#endif
						else strcpy(dummy, "    ");
					}
					else
					{
						/* letter/number for power selection */
						if (ctr < 26)
							letter = I2A(ctr);
						else
							letter = '0' + ctr - 26;
						sprintf(dummy, " %c) ",letter);
					}
					strcat(dummy, format("%-23.23s %2d %4d %3d%%",
						power_desc[ctr].name, power_desc[ctr].level, power_desc[ctr].cost,
						100 - racial_chance(&power_desc[ctr])));
					prt(dummy, y1, x1);
					ctr++;
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}

		if (!use_menu)
		{
			if (choice == '\r' && num == 1)
			{
				choice = 'a';
			}

			if (isalpha(choice))
			{
				/* Note verify */
				ask = (isupper(choice));

				/* Lowercase */
				if (ask) choice = tolower(choice);

				/* Extract request */
				i = (islower(choice) ? A2I(choice) : -1);
			}
			else
			{
				ask = FALSE; /* Can't uppercase digits */

				i = choice - '0' + 26;
			}
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
#ifdef JP
(void) strnfmt(tmp_val, 78, "%sを使いますか？ ", power_desc[i].name);
#else
			(void)strnfmt(tmp_val, 78, "Use %s? ", power_desc[i].name);
#endif


			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) screen_load();

	/* Abort if needed */
	if (!flag)
	{
		energy_use = 0;
		return;
	}
#ifdef ALLOW_REPEAT
	repeat_push(i);
	} /*if (!repeat_pull(&i) || ...)*/
#endif /* ALLOW_REPEAT */
	switch (racial_aux(&power_desc[i]))
	{
	case 1:
		if (power_desc[i].number <= -32)
			cast = special_blow_aux(power_desc[i].number);
		else if (power_desc[i].number < 0)
			cast = cmd_racial_power_aux(power_desc[i].number);
		else
			cast = mutation_power_aux(power_desc[i].number);
		break;
	case 0:
		cast = FALSE;
		break;
	case -1:
		cast = TRUE;
		break;
	}

	if (cast)
	{
		if (racial_cost)
		{
			if (racial_use_hp)
			{
#ifdef JP
				take_hit(DAMAGE_USELIFE, (racial_cost / 2) + randint1(racial_cost / 2),
					 "過度の集中");
#else
				take_hit(DAMAGE_USELIFE, (racial_cost / 2) + randint1(racial_cost / 2),
					 "concentrating too hard");
#endif
			}
			else
			{
				p_ptr->csp -= (racial_cost / 2) + randint1(racial_cost / 2);
			}

			/* Redraw mana and hp */
			p_ptr->redraw |= (PR_HP | PR_MANA);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER | PW_SPELL);
		}
	}
	else energy_use = 0;

	/* Success */
	return;
}
