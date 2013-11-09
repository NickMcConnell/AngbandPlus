/* File: xtra2.c */

/* Purpose: effects of various "objects" */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Advance class experience levels and print class experience
 */
void check_class_experience(void)
{
	int  prev_lev;
	bool level_inc_stat = FALSE;
	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];
	s32b tmp32s;


	/* Note current level */
	prev_lev = cexp_ptr->clev;

	/* Hack -- lower limit */
	if (cexp_ptr->cexp < 0) cexp_ptr->cexp = 0;

	/* Hack -- lower limit */
	if (cexp_ptr->max_cexp < 0) cexp_ptr->max_cexp = 0;

	/* Hack -- upper limit */
	if (cexp_ptr->cexp > PY_MAX_EXP) cexp_ptr->cexp = PY_MAX_EXP;

	/* Hack -- upper limit */
	if (cexp_ptr->max_cexp > PY_MAX_EXP) cexp_ptr->max_cexp = PY_MAX_EXP;

	/* Hack -- maintain "max" experience */
	if (cexp_ptr->cexp > cexp_ptr->max_cexp) cexp_ptr->max_cexp = cexp_ptr->cexp;

	/* Hack -- maintain "max-max" experience */
	if (cexp_ptr->max_cexp > cexp_ptr->max_max_cexp) cexp_ptr->max_max_cexp = cexp_ptr->max_cexp;

	/* Redraw experience */
	p_ptr->redraw |= (PR_CEXP);

	/* Handle stuff */
	handle_stuff();


	/* Lose levels while possible */
	while ((cexp_ptr->clev > 1) &&
	       (cexp_ptr->cexp < (player_exp[cexp_ptr->clev - 2] * p_ptr->cexpfact / 100L)))
	{
		/* Lose a level */
		cexp_ptr->clev--;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_CLEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle stuff */
		handle_stuff();
	}


	/* Gain levels while possible */
	while ((cexp_ptr->clev < PY_MAX_LEVEL) &&
	       (cexp_ptr->cexp >= (player_exp[cexp_ptr->clev - 1] * p_ptr->cexpfact / 100L)))
	{
		/* Gain a level */
		cexp_ptr->clev++;

		/* Save the highest level */
		if (cexp_ptr->clev > cexp_ptr->max_clev)
		{
			int i, j, gfact;
			int lfact = skill_lev_var[cexp_ptr->clev];
			int total_max_clev = 0;


			cexp_ptr->max_clev = cexp_ptr->clev;
			if (cexp_ptr->max_clev > cexp_ptr->max_max_clev) cexp_ptr->max_max_clev = cexp_ptr->max_clev;

			for (i = 0; i < MAX_CLASS; i++)
				{
				if (p_ptr->cexp_info[i].max_clev > 0) total_max_clev += p_ptr->cexp_info[i].max_clev;
				}
			gfact = 2 + (total_max_clev *total_max_clev / cexp_ptr->clev / cexp_ptr->clev / 2);
/*			if (total_max_clev == cexp_ptr->clev) gfact = 2;
			else if ((total_max_clev > cexp_ptr->clev) && (total_max_clev < cexp_ptr->clev * 2)) gfact = 3;
			else gfact = 5;
 */			
			if ((total_max_clev < cexp_ptr->clev * 2) && 
				((p_ptr->pclass == CLASS_LICH) || (p_ptr->pclass == CLASS_ANGELKNIGHT) || (p_ptr->pclass == CLASS_GUNNER))) gfact = 1;

			/* Gain skills */
			p_ptr->gx_dis += cp_ptr->x_dis * lfact / gfact;
			p_ptr->gx_dev += cp_ptr->x_dev * lfact / gfact;
			p_ptr->gx_sav += cp_ptr->x_sav * lfact / gfact;
			p_ptr->gx_stl += cp_ptr->x_stl * lfact / gfact;
			p_ptr->gx_srh += cp_ptr->x_srh * lfact / gfact;
			p_ptr->gx_fos += cp_ptr->x_fos * lfact / gfact;
			p_ptr->gx_spd += cp_ptr->x_spd * lfact / gfact;
			p_ptr->gx_thn += cp_ptr->x_thn * lfact / gfact;
			p_ptr->gx_thb += cp_ptr->x_thb * lfact / gfact;

			/* Limit skills */
			if (p_ptr->gx_dis > 30000) p_ptr->gx_dis = 30000;
			if (p_ptr->gx_dev > 30000) p_ptr->gx_dev = 30000;
			if (p_ptr->gx_sav > 30000) p_ptr->gx_sav = 30000;
			if (p_ptr->gx_stl > 30000) p_ptr->gx_stl = 30000;
			if (p_ptr->gx_srh > 30000) p_ptr->gx_srh = 30000;
			if (p_ptr->gx_fos > 30000) p_ptr->gx_fos = 30000;
			if (p_ptr->gx_spd > 30000) p_ptr->gx_spd = 30000;
			if (p_ptr->gx_thn > 30000) p_ptr->gx_thn = 30000;
			if (p_ptr->gx_thb > 30000) p_ptr->gx_thb = 30000;


			for (i = 1; i < MAX_WT; i++)
				{
				int wlev = p_ptr->s_ptr->w_eff[i] + p_ptr->s_ptr->w_eff[i] * lfact / 15;
				if (p_ptr->weapon_exp[i] >= (cexp_ptr->clev - 1) * 20) wlev = 0;
				else if (p_ptr->weapon_exp[i] >= (cexp_ptr->clev - 1) * 40 / 3) wlev /= 2;
				p_ptr->weapon_exp[i] += wlev;

				if (p_ptr->weapon_exp[i] > 500) p_ptr->weapon_exp[i] = 500;
				}

			for (i = 0; i < 10; i++)
				{
				int mlev = p_ptr->s_ptr->m_eff[i] + p_ptr->s_ptr->m_eff[i] * lfact / 15;
				if (p_ptr->skill_exp[i] >= (cexp_ptr->clev - 1) * 20) mlev = 0;
				else if (p_ptr->skill_exp[i] >= (cexp_ptr->clev - 1) * 40 / 3) mlev /= 2;
				p_ptr->skill_exp[i] += mlev;

				if (p_ptr->skill_exp[i] > 500) p_ptr->skill_exp[i] = 500;
				}

			for (i = 0; i < MAX_REALM + 1; i++)
				{
				if (p_ptr->magic_exp[i] >= (cexp_ptr->clev - 1) * 20) p_ptr->magic_exp[i] += 0;
				else if (p_ptr->magic_exp[i] >= (cexp_ptr->clev - 1) * 30 / 2) p_ptr->magic_exp[i] += p_ptr->s_ptr->s_eff[i] / 3;
				else if (p_ptr->magic_exp[i] >= (cexp_ptr->clev - 1) * 40 / 3) p_ptr->magic_exp[i] += p_ptr->s_ptr->s_eff[i] * 2 / 3;
				else p_ptr->magic_exp[i] += p_ptr->s_ptr->s_eff[i];
				if (p_ptr->magic_exp[i] > 500) p_ptr->magic_exp[i] = 500;
				}

			
			
			if (!(cexp_ptr->max_clev % gfact)) level_inc_stat = TRUE;

			if (p_ptr->pclass != CLASS_GUNNER)
			{
				tmp32s = rand_spread(cp_ptr->c_msp, 1);
				p_ptr->race_sp[cexp_ptr->max_clev - 1] = p_ptr->race_sp[cexp_ptr->max_clev - 2] + MAX(tmp32s, 0);
			}
			else p_ptr->race_sp[cexp_ptr->max_clev - 1] = 0;
		}

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_CLEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_SPELL);

		/* Handle stuff */
		handle_stuff();

		if (level_inc_stat)
		{
			int stat, max_value, inc_value;

			for (stat = 0; stat < A_MAX; stat++)
			{
				max_value = p_ptr->stat_max[stat];
				inc_value = cp_ptr->c_gain[stat];
				if (!inc_value) continue;
				if (max_value < 18) max_value++;
				else
				{
					max_value += inc_value;
					if (max_value > STAT_MAX_MAX) max_value = STAT_MAX_MAX;
				}
				p_ptr->stat_max[stat] = max_value;
				p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];
			}
		}

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_CLEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_SPELL);

		/* Handle stuff */
		handle_stuff();
	}

	/* Load the "pref" files */
	if (prev_lev != cexp_ptr->clev) load_all_pref_files();
}


/*
 * Advance racial experience levels and print racial experience
 */
void check_racial_experience(void)
{
	int  prev_lev;
	bool level_reward = FALSE;
	bool level_inc_stat = FALSE;
	s32b tmp32s;


	/* Note current level */
	prev_lev = p_ptr->lev;

	/* Hack -- lower limit */
	if (p_ptr->exp < 0) p_ptr->exp = 0;

	/* Hack -- lower limit */
	if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

	/* Hack -- upper limit */
	if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

	/* Hack -- upper limit */
	if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;

	/* Hack -- maintain "max" experience */
	if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

	/* Hack -- maintain "max-max" experience */
	if (p_ptr->max_exp > p_ptr->max_max_exp) p_ptr->max_max_exp = p_ptr->max_exp;

	/* Redraw experience */
	p_ptr->redraw |= (PR_EXP);

	/* Handle stuff */
	handle_stuff();


	/* Lose levels while possible */
	while ((p_ptr->lev > 1) &&
	       (p_ptr->exp < (player_exp[p_ptr->lev - 2] * p_ptr->expfact / 100L)))
	{
		/* Lose a level */
		p_ptr->lev--;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_INVEN);

		/* Handle stuff */
		handle_stuff();
	}


	/* Gain levels while possible */
	while ((p_ptr->lev < PY_MAX_LEVEL) &&
	       (p_ptr->exp >= (player_exp[p_ptr->lev-1] * p_ptr->expfact / 100L)))
	{
		/* Gain a level */
		p_ptr->lev++;

		/* Save the highest level */
		if (p_ptr->lev > p_ptr->max_plv)
		{
			p_ptr->max_plv = p_ptr->lev;
			if (p_ptr->max_plv > p_ptr->max_max_plv) p_ptr->max_max_plv = p_ptr->max_plv;

			tmp32s = rand_spread(rp_ptr->r_mhp, 1);
			p_ptr->race_hp[p_ptr->max_plv - 1] = p_ptr->race_hp[p_ptr->max_plv - 2] + MAX(tmp32s, 0);

			/* Gain speed */
			p_ptr->gx_spd += rp_ptr->rx_spd;

			/* Limit skills */
			if (p_ptr->gx_spd > 30000) p_ptr->gx_spd = 30000;

			if (p_ptr->muta2 & MUT2_TAROT)
			{
				level_reward = TRUE;
			}
			level_inc_stat = TRUE;

			do_cmd_write_nikki(NIKKI_LEVELUP, p_ptr->lev, NULL);
		}

		/* Sound */
		sound(SOUND_LEVEL);

		/* Message */
#ifdef JP
		msg_format("レベル %d にようこそ。", p_ptr->lev);
#else
		msg_format("Welcome to level %d.", p_ptr->lev);
#endif

		if (level_inc_stat && !(p_ptr->max_plv % 5))
		{
			int choice;
			screen_save();
			while(1)
			{
				int n;
				char tmp[32];

#ifdef JP
				cnv_stat(p_ptr->stat_max[A_STR], tmp);
				prt(format("        a) 腕力 (現在値 %s)", tmp), 2, 14);
				cnv_stat(p_ptr->stat_max[A_INT], tmp);
				prt(format("        b) 知能 (現在値 %s)", tmp), 3, 14);
				cnv_stat(p_ptr->stat_max[A_WIS], tmp);
				prt(format("        c) 賢さ (現在値 %s)", tmp), 4, 14);
				cnv_stat(p_ptr->stat_max[A_DEX], tmp);
				prt(format("        d) 器用 (現在値 %s)", tmp), 5, 14);
				cnv_stat(p_ptr->stat_max[A_CON], tmp);
				prt(format("        e) 耐久 (現在値 %s)", tmp), 6, 14);
				cnv_stat(p_ptr->stat_max[A_CHR], tmp);
				prt(format("        f) 魅力 (現在値 %s)", tmp), 7, 14);
				prt("", 8, 14);
				prt("        どの能力値を上げますか？", 1, 14);
#else
				cnv_stat(p_ptr->stat_max[A_STR], tmp);
				prt(format("        a) Str (cur %s)", tmp), 2, 14);
				cnv_stat(p_ptr->stat_max[A_INT], tmp);
				prt(format("        b) Int (cur %s)", tmp), 3, 14);
				cnv_stat(p_ptr->stat_max[A_WIS], tmp);
				prt(format("        c) Wis (cur %s)", tmp), 4, 14);
				cnv_stat(p_ptr->stat_max[A_DEX], tmp);
				prt(format("        d) Dex (cur %s)", tmp), 5, 14);
				cnv_stat(p_ptr->stat_max[A_CON], tmp);
				prt(format("        e) Con (cur %s)", tmp), 6, 14);
				cnv_stat(p_ptr->stat_max[A_CHR], tmp);
				prt(format("        f) Chr (cur %s)", tmp), 7, 14);
				prt("", 8, 14);
				prt("        Which stat do you want to raise?", 1, 14);
#endif
				while(1)
				{
					choice = inkey();
					if ((choice >= 'a') && (choice <= 'f')) break;
				}
				for (n = 0; n < A_MAX; n++)
					if (n != choice - 'a')
						prt("", n + 2, 14);
#ifdef JP
				if (get_check("よろしいですか？")) break;
#else
				if (get_check("Are you sure? ")) break;
#endif
			}
			do_inc_stat(choice - 'a');
			screen_load();
		}
		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_SPELL | PW_INVEN);

		/* Handle stuff */
		handle_stuff();


		/*
		 * 報酬でレベルが上ると再帰的に check_racial_experience() が
		 * 呼ばれるので順番を最後にする。
		 */
		if (level_reward)
		{
			gain_level_reward(0);
			level_reward = FALSE;
		}

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_SPELL);

		/* Handle stuff */
		handle_stuff();
	}

	/* Load the "pref" files */
	if (prev_lev != p_ptr->lev) load_all_pref_files();
}


void check_experience(void)
{
	check_class_experience();
	check_racial_experience();
}


/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * XXX XXX XXX Note the use of actual "monster names"
 */
static int get_coin_type(int r_idx)
{
	switch (r_idx)
	{
	case MON_COPPER_COINS: return (2);
	case MON_SILVER_COINS: return (5);
	case MON_GOLD_COINS: return (10);
	case MON_MITHRIL_COINS: return (16);
	case MON_ADAMANT_COINS: return (17);
	case MON_MITHRIL_GOLEM: return (16);
	}

	/* Assume nothing */
	return (0);
}


/*
 * Hack -- determine if a template is Cloak
 */
static bool kind_is_cloak(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if (k_ptr->tval == TV_CLOAK)
	{
		return (TRUE);
	}

	/* Assume not good */
	return (FALSE);
}


/*
 * Hack -- determine if a template is Polearm
 */
static bool kind_is_polearm(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if (k_ptr->tval == TV_POLEARM)
	{
		return (TRUE);
	}

	/* Assume not good */
	return (FALSE);
}


/*
 * Hack -- determine if a template is Sword
 */
static bool kind_is_sword(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if (k_ptr->tval == TV_SWORD)
	{
		return (TRUE);
	}

	/* Assume not good */
	return (FALSE);
}


/*
 * Hack -- determine if a template is hafted weapon
 */
static bool kind_is_hafted(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if (k_ptr->tval == TV_HAFTED)
	{
		if (!((k_ptr->sval == SV_WHIP) || (k_ptr->sval == SV_QUARTERSTAFF) || (k_ptr->sval == SV_SCIPPLAYS_STAFF) || (k_ptr->sval == SV_FAN) || (k_ptr->sval == SV_WIZSTAFF)))
			return (TRUE);
		else
			return (FALSE);
	}

	/* Assume not good */
	return (FALSE);
}


/*
 * Hack -- determine if a template is Armor
 */
static bool kind_is_armor(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if (k_ptr->tval == TV_HARD_ARMOR)
	{
		return (TRUE);
	}

	/* Assume not good */
	return (FALSE);
}


/*
 * Check for "Quest" completion when a quest monster is killed or charmed.
 */
void check_quest_completion(monster_type *m_ptr)
{
	int i, j, y, x, ny, nx, i2, j2;

	int quest_num;

	bool create_stairs = FALSE;
	bool reward = FALSE;

	object_type forge;
	object_type *q_ptr;

	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Inside a quest */
	quest_num = p_ptr->inside_quest;

	/* Search for an active quest on this dungeon level */
	if (!quest_num)
	{
		for (i = max_quests - 1; i > 0; i--)
		{
			/* Quest is not active */
			if (quest[i].status != QUEST_STATUS_TAKEN)
				continue;

			/* Quest is not a dungeon quest */
			if (quest[i].flags & QUEST_FLAG_PRESET)
				continue;

			/* Quest is not on this level */
			if ((quest[i].level != dun_level) &&
			    (quest[i].type != QUEST_TYPE_KILL_ANY_LEVEL))
				continue;

			/* Not a "kill monster" quest */
			if ((quest[i].type == QUEST_TYPE_FIND_ARTIFACT) ||
			    (quest[i].type == QUEST_TYPE_FIND_EXIT))
				continue;

			/* Interesting quest */
			if ((quest[i].type == QUEST_TYPE_KILL_NUMBER) ||
			    (quest[i].type == QUEST_TYPE_KILL_ALL))
				break;

			/* Interesting quest */
			if (((quest[i].type == QUEST_TYPE_KILL_LEVEL) ||
			     (quest[i].type == QUEST_TYPE_KILL_ANY_LEVEL) ||
			     (quest[i].type == QUEST_TYPE_RANDOM)) &&
			     (quest[i].r_idx == m_ptr->r_idx))
				break;
		}

		quest_num = i;
	}

	/* Handle the current quest */
	if (quest_num && (quest[quest_num].status == QUEST_STATUS_TAKEN))
	{
		/* Current quest */
		i = quest_num;

		switch (quest[i].type)
		{
			case QUEST_TYPE_KILL_NUMBER:
			{
				quest[i].cur_num++;

				if (quest[i].cur_num >= quest[i].num_mon)
				{
					if (record_fix_quest) do_cmd_write_nikki(NIKKI_FIX_QUEST_C, i, NULL);
					/* completed quest */
					quest[i].status = QUEST_STATUS_COMPLETED;
					quest[i].complev = (byte)p_ptr->lev;

					if (!(quest[i].flags & QUEST_FLAG_SILENT))
					{
						/* Make a sound */
						sound(SOUND_QUEST);

#ifdef JP
msg_print("クエストを達成した！");
#else
						msg_print("You just completed your quest!");
#endif

						msg_print(NULL);
					}

					quest[i].cur_num = 0;
					if (quest_is_fixed(i)) change_your_alignment_lnc(10);
				}
				break;
			}
			case QUEST_TYPE_KILL_ALL:
			{
				int number_mon = 0;

				if (!is_hostile(m_ptr)) break;

				/* Count all hostile monsters */
				for (i2 = 0; i2 < cur_wid; ++i2)
					for (j2 = 0; j2 < cur_hgt; j2++)
						if (cave[j2][i2].m_idx > 0)
							if (is_hostile(&m_list[cave[j2][i2].m_idx])) 
								number_mon++;

				if ((number_mon - 1) == 0)
				{
					if (record_fix_quest) do_cmd_write_nikki(NIKKI_FIX_QUEST_C, i, NULL);
					quest[i].complev = (byte)p_ptr->lev;
					if ((i != QUEST_BARMAMUTHA_L) && (i != QUEST_BARMAMUTHA_C))
					{
						/* completed */
						if (quest[i].flags & QUEST_FLAG_SILENT)
						{
							quest[i].status = QUEST_STATUS_FINISHED;
						}
						else
						{
							quest[i].status = QUEST_STATUS_COMPLETED;

							/* Make a sound */
							sound(SOUND_QUEST);

#ifdef JP
msg_print("クエストを達成した！");
#else
							msg_print("You just completed your quest!");
#endif

							msg_print(NULL);
						}
						if (quest_is_fixed(i)) change_your_alignment_lnc(10);
					}

					/* Finish the two "genocide" quests without rewarding */
					else
					{
						quest[i].status = QUEST_STATUS_FINISHED;
						p_ptr->inside_quest = 0;
						dun_level = 0;

						if (i == QUEST_BARMAMUTHA_L)
						{
							msg_print("あなたは収容所の人々をすべて殺した。");
							misc_event_flags |= EVENT_CLOSE_BARMAMUTHA;
						}
						else if (i == QUEST_BARMAMUTHA_C)
							msg_print("あなたは敵を全滅させた。");
						msg_print(NULL);

						/* Force change to wild mode */
						energy_use = 1000;
						set_action(ACTION_NONE);
						p_ptr->wild_mode = TRUE;

						/* Leaving */
						p_ptr->leaving = TRUE;
					}
				}
				break;
			}
			case QUEST_TYPE_KILL_LEVEL:
			case QUEST_TYPE_RANDOM:
			{
				/* Only count valid monsters */
				if (quest[i].r_idx != m_ptr->r_idx)
					break;

				quest[i].cur_num++;

				if (quest[i].cur_num >= quest[i].max_num)
				{
					if (record_fix_quest && (quest[i].type == QUEST_TYPE_KILL_LEVEL)) do_cmd_write_nikki(NIKKI_FIX_QUEST_C, i, NULL);
					if (record_rand_quest && (quest[i].type == QUEST_TYPE_RANDOM))
					{
						r_info[quest[i].r_idx].flags1 &= ~(RF1_QUESTOR);
						do_cmd_write_nikki(NIKKI_RAND_QUEST_C, i, NULL);
					}
					/* completed quest */
					quest[i].status = QUEST_STATUS_COMPLETED;
					quest[i].complev = (byte)p_ptr->lev;
					if (!(quest[i].flags & QUEST_FLAG_PRESET))
					{
						create_stairs = (!(i == QUEST_ARMORICA) ? TRUE : FALSE);
						p_ptr->inside_quest = 0;
					}

					if (!(quest[i].flags & QUEST_FLAG_SILENT))
					{
						/* Make a sound */
						sound(SOUND_QUEST);

#ifdef JP
msg_print("クエストを達成した！");
#else
						msg_print("You just completed your quest!");
#endif

						msg_print(NULL);
					}

					/* Finish the main quests without rewarding */
					if (quest[i].flags & QUEST_FLAG_GUARDIAN)
					{
						quest[i].status = QUEST_STATUS_FINISHED;
						if (i == QUEST_FILARHH)
						{
							int k;
							create_stairs = FALSE;
							for (k = 0; k < INVEN_TOTAL; k++)
							{
								if (inventory[k].k_idx && (inventory[k].name1 == ART_BRUNHILD))
								{
									msg_print("ブリュンヒルドは力を取り戻したようだ。");
									break;
								}
							}
						}
					}

					if (quest[i].type == QUEST_TYPE_RANDOM)
					{
						reward = TRUE;
						quest[i].status = QUEST_STATUS_FINISHED;
					}
					if (quest_is_fixed(i)) change_your_alignment_lnc(10);
				}
				break;
			}
			case QUEST_TYPE_KILL_ANY_LEVEL:
			{
				quest[i].cur_num++;
				if (quest[i].cur_num >= quest[i].max_num)
				{
					if (record_fix_quest) do_cmd_write_nikki(NIKKI_FIX_QUEST_C, i, NULL);
					 /* completed quest */
					quest[i].status = QUEST_STATUS_COMPLETED;
					quest[i].complev = (byte)p_ptr->lev;

					if (!(quest[i].flags & QUEST_FLAG_SILENT))
					{
						/* Make a sound */
						sound(SOUND_QUEST);

#ifdef JP
msg_print("クエストを達成した！");
#else
						msg_print("You just completed your quest!");
#endif

						msg_print(NULL);
					}
					quest[i].cur_num = 0;
					if (quest_is_fixed(i)) change_your_alignment_lnc(10);
				}
				break;
			}
		}
	}

	/* Create a magical staircase */
	if (create_stairs)
	{
		/* Stagger around */
		while (cave_perma_bold(y, x) || cave[y][x].o_idx || (cave[y][x].info & CAVE_OBJECT))
		{
			/* Pick a location */
			scatter(&ny, &nx, y, x, 1, 0);

			/* Stagger */
			y = ny; x = nx;
		}

		/* Explain the staircase */
#ifdef JP
msg_print("魔法の階段が現れた...");
#else
		msg_print("A magical staircase appears...");
#endif


		/* Create stairs up or down */
		cave_set_feat(y, x, (((d_info[dungeon_type].flags1 & DF1_UPWARD) ? TRUE : FALSE) ^ astral_mode) ? FEAT_LESS : FEAT_MORE);

		/* Remember to update everything */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
	}

	/*
	 * Drop quest reward
	 */
	if (reward)
	{
		int old_dun_level = dun_level;

		if (astral_mode) dun_level = 100 - dun_level;

		for (j = 0; j < (dun_level / 15)+1; j++)
		{
			/* Get local object */
			q_ptr = &forge;

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Make a great object */
			make_object(q_ptr, AMF_OKAY | AMF_GOOD | AMF_GREAT);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);
		}

		if (astral_mode) dun_level = old_dun_level;
	}
}

static void prepare_tarot_card(object_type *o_ptr, int effect)
{
	if ((effect < 0) || (effect > 44)) effect = randint0(45);

	/* Prepare a tarot card */
	object_prep(o_ptr, lookup_kind(TV_TAROT, 0));
	o_ptr->number = 1;
	o_ptr->pval = effect;
	object_aware(o_ptr);
	object_known(o_ptr);
}

cptr extract_note_dies(monster_race *r_ptr)
{
	/* Assume a default death */
#ifdef JP
	cptr note_dies = "は死んだ。";
#else
	cptr note_dies = " dies.";
#endif

	/* Some monsters get "destroyed" */
	if (!monster_living(r_ptr))
	{
		int i;
		bool explode = FALSE;

		for (i = 0; i < 4; i++)
		{
			if (r_ptr->blow[i].method == RBM_EXPLODE) explode = TRUE;
		}

		/* Special note at death */
		if (explode)
#ifdef JP
			note_dies = "は爆発して粉々になった。";
#else
			note_dies = " explodes into tiny shreds.";
#endif
		else
#ifdef JP
			note_dies = "を倒した。";
#else
			note_dies = " is destroyed.";
#endif
	}

	return note_dies;
}

/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 * Thus (for now) all Quest monsters should be Uniques.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(int m_idx, bool drop_item, bool is_stoned)
{
	int i, j, y, x;

	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & RF1_UNIQUE));

	u32b am_flags = AMF_OKAY;

	bool do_gold = (!(r_ptr->flags1 & RF1_ONLY_ITEM));
	bool do_item = (!(r_ptr->flags1 & RF1_ONLY_GOLD));
	bool cloned = FALSE;
	int force_coin = get_coin_type(m_ptr->r_idx);

	object_type forge;
	object_type *q_ptr;


	if (r_ptr->flags1 & RF1_DROP_GOOD) am_flags |= AMF_GOOD;
	if (r_ptr->flags1 & RF1_DROP_GREAT) am_flags |= AMF_GREAT;
	if (r_ptr->flags1 & RF1_DROP_SPECIAL) am_flags |= AMF_SPECIAL;

	if (stop_the_time_monster) stop_the_time_monster = FALSE;

	/* Notice changes in view */
	if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2 | RF7_SELF_LITE_1 | RF7_SELF_LITE_2))
	{
		/* Update some things */
		p_ptr->update |= (PU_MON_LITE);
	}

	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	if (m_ptr->smart1 & SM1_CLONED)
		cloned = TRUE;

	if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
	{
		char m_name[80];

		monster_desc(m_name, m_ptr, 0x08);
		do_cmd_write_nikki(NIKKI_NAMED_PET, is_stoned ? 8 : 3, m_name);
	}

	/* Let monsters explode! */
	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method == RBM_EXPLODE)
		{
			u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
			int typ = GF_MISSILE;
			int d_dice = r_ptr->blow[i].d_dice;
			int d_side = r_ptr->blow[i].d_side;
			int damage = damroll(d_dice, d_side);

			switch (r_ptr->blow[i].effect)
			{
				case RBE_HURT:      typ = GF_MISSILE; break;
				case RBE_POISON:    typ = GF_POIS; break;
				case RBE_UN_BONUS:  typ = GF_DISENCHANT; break;
				case RBE_UN_POWER:  typ = GF_MISSILE; break; /* ToDo: Apply the correct effects */
				case RBE_EAT_GOLD:  typ = GF_MISSILE; break;
				case RBE_EAT_ITEM:  typ = GF_MISSILE; break;
				case RBE_EAT_FOOD:  typ = GF_MISSILE; break;
				case RBE_EAT_LITE:  typ = GF_MISSILE; break;
				case RBE_ACID:      typ = GF_ACID; break;
				case RBE_ELEC:      typ = GF_ELEC; break;
				case RBE_FIRE:      typ = GF_FIRE; break;
				case RBE_COLD:      typ = GF_COLD; break;
				case RBE_BLIND:     typ = GF_MISSILE; break;
				case RBE_CONFUSE:   typ = GF_CONFUSION; break;
				case RBE_TERRIFY:   typ = GF_MISSILE; break;
				case RBE_PARALYZE:  typ = GF_MISSILE; break;
				case RBE_LOSE_STR:  typ = GF_MISSILE; break;
				case RBE_LOSE_DEX:  typ = GF_MISSILE; break;
				case RBE_LOSE_CON:  typ = GF_MISSILE; break;
				case RBE_LOSE_INT:  typ = GF_MISSILE; break;
				case RBE_LOSE_WIS:  typ = GF_MISSILE; break;
				case RBE_LOSE_CHR:  typ = GF_MISSILE; break;
				case RBE_LOSE_ALL:  typ = GF_MISSILE; break;
				case RBE_SHATTER:   typ = GF_ROCKET; break;
				case RBE_EXP_10:    typ = GF_MISSILE; break;
				case RBE_EXP_20:    typ = GF_MISSILE; break;
				case RBE_EXP_40:    typ = GF_MISSILE; break;
				case RBE_EXP_80:    typ = GF_MISSILE; break;
				case RBE_DISEASE:   typ = GF_POIS; break;
				case RBE_TIME:      typ = GF_TIME; break;
				case RBE_EXP_VAMP:  typ = GF_MISSILE; break;
				case RBE_DR_MANA:   typ = GF_MANA; break;
				case RBE_SUPERHURT: typ = GF_MISSILE; break;
				case RBE_STONE:     typ = GF_STONE; break;
				case RBE_HOLY:      typ = GF_HOLY_FIRE; break;
				case RBE_HELL:      typ = GF_HELL_FIRE; break;
			}
			if (is_stoned)
			{
				char m_name[80];

				/* Extract monster name */
				monster_desc(m_name, m_ptr, 0);

				is_stoned = FALSE;
				if (typ != RBE_STONE) typ = GF_SHARDS;
#ifdef JP
				msg_format("石化した%sが爆発して砕け散った。", m_name);
#else
				msg_format("The stoned %s exploded into tiny shreds.", m_name);
#endif
			}

			project(m_idx, 3, y, x, damage, typ, flg, MODIFY_ELEM_MODE_MELEE);
			break;
		}
	}

	/* Check for quest completion */
	check_quest_completion(m_ptr);

	/* Handle the possibility of player vanquishing arena combatant -KMW- */
	if (p_ptr->inside_arena && !is_pet(m_ptr))
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

		p_ptr->exit_bldg = TRUE;

		if (p_ptr->arena_number > MAX_ARENA_MONS)
		{
#ifdef JP
msg_format("素晴らしい！君こそ%sの勝利者だ。", (p_ptr->arena_number == MAX_ARENA_MONS + 1) ? "至高" : "究極");
#else
			msg_format("You are a %s Champion!", (p_ptr->arena_number == MAX_ARENA_MONS + 1) ? "Higher" : "Ultimate");
#endif
		}
		else
		{
#ifdef JP
msg_print("勝利！チャンピオンへの道を進んでいる。");
#else
			msg_print("Victorious! You're on your way to becoming Champion.");
#endif
		}

		if (arena_object[p_ptr->arena_number][0] && arena_object[p_ptr->arena_number][1])
		{
			/* Get local object */
			q_ptr = &forge;

			/* Prepare to make a reward */
			object_prep(q_ptr, lookup_kind(arena_object[p_ptr->arena_number][0], arena_object[p_ptr->arena_number][1]));

			apply_magic(q_ptr, object_level, AMF_OKAY | AMF_GOOD);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);
		}

		if (p_ptr->arena_number > MAX_ARENA_MONS) p_ptr->arena_number++;
		p_ptr->arena_number++;
		if (record_arena)
		{
			if (p_ptr->arena_number > (MAX_ARENA_MONS + 3))
				do_cmd_write_nikki(NIKKI_ARENA, p_ptr->arena_number - 3, m_name);
			else if (p_ptr->arena_number > (MAX_ARENA_MONS + 1))
				do_cmd_write_nikki(NIKKI_ARENA, p_ptr->arena_number - 2, m_name);
			else
				do_cmd_write_nikki(NIKKI_ARENA, p_ptr->arena_number, m_name);
		}
	}

	if (m_idx == p_ptr->riding)
	{
		if (rakuba(-1, FALSE))
		{
#ifdef JP
msg_print("地面に落とされた。");
#else
			msg_print("You have fallen from your riding pet.");
#endif
		}
	}

	/* Drop a dead corpse? */
	if (one_in_(r_ptr->flags1 & RF1_UNIQUE ? 1 : 4) &&
	    ((r_ptr->flags9 & RF9_DROP_CORPSE) ||
	     (r_ptr->flags9 & RF9_DROP_SKELETON)) &&
	    !is_stoned && !(p_ptr->inside_arena || (m_ptr->smart1 & SM1_CLONED) || (!astral_mode && (m_ptr->r_idx == today_mon) && is_pet(m_ptr))))
	{
		/* Assume skeleton */
		bool corpse = FALSE;

		/*
		 * We cannot drop a skeleton? Note, if we are in this check,
		 * we *know* we can drop at least a corpse or a skeleton
		 */
		if (!(r_ptr->flags9 & RF9_DROP_SKELETON))
			corpse = TRUE;
		else if ((r_ptr->flags9 & RF9_DROP_CORPSE) && (r_ptr->flags1 && RF1_UNIQUE))
			corpse = TRUE;

		/* Else, a corpse is more likely unless we did a "lot" of damage */
		else if (r_ptr->flags9 & RF9_DROP_CORPSE)
		{
			/* Lots of damage in one blow */
			if ((0 - ((m_ptr->maxhp) / 4)) > m_ptr->hp)
			{
				if (one_in_(5)) corpse = TRUE;
			}
			else
			{
				if (!one_in_(5)) corpse = TRUE;
			}
		}

		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make an object */
		object_prep(q_ptr, lookup_kind(TV_CORPSE, (corpse ? SV_CORPSE : SV_SKELETON)));

		apply_magic(q_ptr, object_level, 0L);

		q_ptr->pval = m_ptr->r_idx;

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}
	/* A monster turns into a stone statue? */
	else if (one_in_(r_ptr->flags1 & RF1_UNIQUE ? 1 : 3) && is_stoned)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a stone statue */
		object_prep(q_ptr, lookup_kind(TV_STATUE, SV_STONE_STATUE));

		apply_magic(q_ptr, object_level, 0L);

		q_ptr->pval = m_ptr->r_idx;

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	/* Drop objects being carried */
	monster_drop_carried_objects(m_ptr);

	/* The Runeweapon is beaten */
	if (monster_is_runeweapon(m_ptr->r_idx) && !cloned)
	{
		if (astral_mode && (runeweapon_num_from(m_ptr->r_idx) == 1))
		{
			/* Nothing */
		}
		else
		{
			runeweapon_type *runeweapon = &runeweapon_list[runeweapon_num_from(m_ptr->r_idx)];

			/* Get local object */
			q_ptr = &forge;

			object_copy(q_ptr, &runeweapon->weapon);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);
		}
	}

	else if ((m_ptr->r_idx == MON_FOOD_DRAGON) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_INC_STR));

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((m_ptr->r_idx == MON_FOOD_CHIMERA) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_INC_INT));

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((m_ptr->r_idx == MON_FOOD_OCTOPUS) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_INC_WIS));

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((m_ptr->r_idx == MON_FOOD_SQUID) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_INC_DEX));

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((m_ptr->r_idx == MON_FOOD_BIRD) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_INC_CON));

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((m_ptr->r_idx == MON_FOOD_COOK) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_INC_CHR));

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((m_ptr->r_idx == MON_FOOD_MAEMAID) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_AUGMENTATION));

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((r_ptr->d_char == '\\') &&
	    !monster_is_runeweapon(m_ptr->r_idx) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Activate restriction */
		get_obj_num_hook = kind_is_hafted;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Make a object */
		make_object(q_ptr, AMF_OKAY);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((r_ptr->d_char == '|') &&
	    !monster_is_runeweapon(m_ptr->r_idx) && !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Activate restriction */
		get_obj_num_hook = kind_is_sword;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Make a object */
		make_object(q_ptr, AMF_OKAY);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((r_ptr->d_char == '(') && (dun_level > 0) &&
	    !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Activate restriction */
		get_obj_num_hook = kind_is_cloak;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Make a object */
		make_object(q_ptr, AMF_OKAY);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((r_ptr->d_char == '/') && (dun_level > 4) &&
	    !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Activate restriction */
		get_obj_num_hook = kind_is_polearm;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Make a object */
		make_object(q_ptr, AMF_OKAY);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	else if ((r_ptr->d_char == '[') && (dun_level > 19) &&
	    !p_ptr->inside_arena)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Activate restriction */
		get_obj_num_hook = kind_is_armor;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Make a object */
		make_object(q_ptr, AMF_OKAY);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	/* Mega-Hack -- drop "winner" treasures */
	else
	{
		if (m_ptr->r_idx == MON_DOLGARUA)
		{
			/* Get local object */
			q_ptr = &forge;

			/* Mega-Hack -- Prepare to make "Grond" */
			object_prep(q_ptr, lookup_kind(TV_HAFTED, SV_GROND));

			/* Mega-Hack -- Mark this item as "Grond" */
			q_ptr->name1 = ART_GROND;

			/* Mega-Hack -- Actually create "Grond" */
			apply_magic(q_ptr, -1, AMF_OKAY | AMF_GOOD | AMF_GREAT);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);

			/* Get local object */
			q_ptr = &forge;

			/* Mega-Hack -- Prepare to make "King" */
			object_prep(q_ptr, lookup_kind(TV_CROWN, SV_KING));

			/* Mega-Hack -- Mark this item as "King" */
			q_ptr->name1 = ART_KING;

			/* Mega-Hack -- Actually create "King" */
			apply_magic(q_ptr, -1, AMF_OKAY | AMF_GOOD | AMF_GREAT);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);
		}

		/* DROP_CHOSEN */
		if (!cloned)
		{
			int a_idx = 0;
			int chance = 0;

			switch (m_ptr->r_idx)
			{
			case MON_MAN_LOOK_SEA:
				switch (randint1(4))
				{
				case 1: case 2:
					a_idx = ART_LEGACY;
					chance = 100;
					break;
				case 3:
					a_idx = ART_ZENOBIA;
					chance = 10;
					break;
				default:
					a_idx = ART_LANCELOT_H;
					chance = 10;
					break;
				}
				break;

			case MON_CLARE:
				a_idx = ART_CLARE;
				chance = 100;
				break;

			case MON_BAPALU:
				a_idx = ART_BAPALU;
				chance = 50;
				break;

			case MON_SARA:
				a_idx = ART_SARA;
				chance = 40;
				break;

			case MON_VELDO:
				a_idx = ART_VELDO;
				chance = 35;
				break;

			case MON_POISON_DENIM:
				a_idx = ART_DENIM;
				chance = 100;
				break;

			case MON_ZILDOR:
				a_idx = ART_CALDIA;
				chance = 50;
				break;

			case MON_BAIAN:
				a_idx = ART_BAIAN;
				chance = 20;
				break;

			case MON_GENAUNES:
				a_idx = ART_GENAUNES;
				chance = 20;
				break;

			case MON_GUACHARO:
				a_idx = ART_GUACHARO;
				chance = 40;
				break;

			case MON_DEBARDES:
				a_idx = ART_DEBARDES;
				chance = 20;
				break;

			case MON_URAM:
				a_idx = ART_WATER;
				chance = 30;
				break;

			case MON_OZ:
				a_idx = ART_GRAMLOCK;
				chance = 20;
				break;

			case MON_MERCURY:
				a_idx = ART_PEREGRINE;
				chance = 20;
				break;

			case MON_PROKION:
				a_idx = ART_PROKION;
				chance = 10;
				break;

			case MON_BESTEAR:
				a_idx = ART_PHOENIX;
				chance = 20;
				break;

			case MON_FALFADET:
				a_idx = ART_BLACK;
				chance = 15;
				break;

			case MON_ERIG:
				a_idx = ART_ERIG;
				chance = 20;
				break;

			case MON_RENDAL:
				a_idx = ART_RENDAL;
				chance = 30;
				break;

			case MON_LAUAU:
				a_idx = ART_FLAME;
				chance = 50;
				break;

			case MON_GRANDE:
				a_idx = ART_BLACK_CAT;
				chance = 15;
				break;

			case MON_OXYONES:
				a_idx = ART_OXYONES;
				chance = 30;
				break;

			case MON_SISTEENA:
				a_idx = ART_SISTEENA;
				chance = 20;
				break;

			case MON_BARBATOS:
				a_idx = ART_BARBATOS;
				chance = 50;
				break;

			case MON_SELYE:
				a_idx = ART_VOLCAETUS;
				chance = 25;
				break;

			case MON_WARREN:
				/* Hack -- Warren has blank tarot card */

				/* Get local object */
				q_ptr = &forge;

				/* Prepare a blank tarot card */
				prepare_tarot_card(q_ptr, 0);

				/* Drop it in the dungeon */
				(void)drop_near(q_ptr, -1, y, x);

				a_idx = ART_WARREN;
				chance = 15;
				break;

			case MON_OZMA:
				a_idx = ART_RAPTURE_ROSE;
				chance = 30;
				break;

			case MON_LEONARD:
				a_idx = ART_LEONARD;
				chance = 25;
				break;

			case MON_HABORYM:
				a_idx = ART_HABORYM;
				chance = 25;
				break;

			case MON_MARTYM:
				if (one_in_(2))
				{
					a_idx = ART_NEPHRITE;
					chance = 15;
				}
				else
				{
					a_idx = ART_BRUNHILD;
					chance = 1;
				}
				break;

			case MON_BARBAS:
				switch (randint1(2))
				{
				case 1:
					a_idx = ART_SANSCION;
					chance = 15;
					break;
				default:
					a_idx = ART_RIMFIRE;
					chance = 50;
					break;
				}
				break;

			case MON_BRANTA:
				a_idx = ART_PENITENCE;
				chance = 15;
				break;

			case MON_BELZBUTE:
				a_idx = ART_BELZBUTE;
				chance = 25;
				break;

			case MON_OLIVIA:
				a_idx = ART_MAGI;
				chance = 15;
				break;

			case MON_RADLUM:
				a_idx = ART_ALBELEO;
				chance = 20;
				break;

			case MON_BALZEPHO:
				if (one_in_(2))
				{
					a_idx = ART_HABORYM_EYE;
					chance = 40;
				}
				else
				{
					a_idx = ART_VOLGRAS;
					chance = 20;
				}
				break;

			case MON_ZADOVA:
				a_idx = ART_ZADOVA;
				chance = 20;
				break;

			case MON_ANDORAS:
				if (one_in_(2))
				{
					a_idx = ART_TOUELNO;
					chance = 40;
				}
				else
				{
					a_idx = ART_NIRDAM;
					chance = 30;
				}
				break;

			case MON_YENDOR:
				a_idx = ART_YENDOR;
				chance = 4;
				break;

			case MON_FELION:
				a_idx = ART_SHOTGUN;
				chance = 50;
				break;

			case MON_DENEB:
				a_idx = ART_VOLUPTUOUS;
				chance = 100;
				break;

			case MON_FOGEL:
				a_idx = ART_ZANZIBAR;
				chance = 5;
				break;

			case MON_BLACMORE:
				a_idx = ART_LICH;
				chance = 50;
				break;

			case MON_ENBU:
				a_idx = ART_BOREAS;
				chance = 100;
				break;

			case MON_SEIGETSU:
				a_idx = ART_NOTOS;
				chance = 100;
				break;

			case MON_KINBU:
				a_idx = ART_EUROS;
				chance = 100;
				break;

			case MON_REKKUU:
				a_idx = ART_ZEPHYRUS;
				chance = 100;
				break;

			case MON_NYBBAS:
				a_idx = ART_NYBBAS;
				chance = 30;
				break;

			case MON_BERSALIA:
				a_idx = ART_RED;
				chance = 100;
				break;

			case MON_LANCELOT:
				if (one_in_(2))
				{
					a_idx = ART_LANCELOT_D;
					chance = 30;
				}
				else
				{
					a_idx = ART_ANBICION;
					chance = 15;
				}
				break;

			case MON_DENIM:
				a_idx = ART_BLUE;
				chance = 100;
				break;

			case MON_DOLGARUA:
				a_idx = ART_DOLGARUA;
				chance = 50;
				break;

			case MON_CANOPUS:
				a_idx = ART_SKYWALKER;
				chance = 100;
				break;

			case MON_OBDA:
				a_idx = ART_OBDA;
				chance = 50;
				break;

			case MON_BELDA:
				a_idx = ART_BELDA;
				chance = 50;
				break;

			case MON_FELLANA:
				a_idx = ART_OGRE_SHIELD;
				chance = 100;
				break;

			case MON_HOLP:
				a_idx = ART_OGRE_HELM;
				chance = 100;
				break;

			case MON_ISHTALLE:
				a_idx = ART_OGRE_BLADE;
				chance = 100;
				break;

			case MON_FILARHH:
				a_idx = ART_OGRE_ARMOR;
				chance = 100;
				break;

			case MON_DIVINE_DRAGON:
				a_idx = ART_FIRECREST;
				chance = 100;
				break;
			}

			if ((a_idx > 0) && ((randint0(100) < chance) || (p_ptr->wizard)))
			{
				artifact_type *a_ptr = &a_info[a_idx];

				if (a_ptr->cur_num == 0)
				{
					/* Create the artifact */
					if (create_named_art(a_idx, y, x))
					{
						a_ptr->cur_num = 1;

						/* Hack -- Memorize location of artifact in saved floors */
						if (character_dungeon) a_ptr->floor_id = p_ptr->floor_id;
					}
					else if (!preserve_mode)
						a_ptr->cur_num = 1;
				}
			}
		}
	}
	if ((r_ptr->flags7 & RF7_GUARDIAN) && (d_info[dungeon_type].final_guardian == m_ptr->r_idx))
	{
		int k_idx = 447; /* Acquirement */;

		if (d_info[dungeon_type].final_object)
			k_idx = d_info[dungeon_type].final_object;

		if (d_info[dungeon_type].final_artifact)
		{
			int a_idx = d_info[dungeon_type].final_artifact;
			artifact_type *a_ptr = &a_info[a_idx];

			if (a_ptr->cur_num == 0)
			{
				/* Create the artifact */
				if (create_named_art(a_idx, y, x))
				{
					a_ptr->cur_num = 1;
					k_idx = 0;

					/* Hack -- Memorize location of artifact in saved floors */
					if (character_dungeon) a_ptr->floor_id = p_ptr->floor_id;
				}
				else if (!preserve_mode)
				{
					a_ptr->cur_num = 1;
					k_idx = 0;
				}
			}
		}

		if (k_idx)
		{
			/* Get local object */
			q_ptr = &forge;

			/* Prepare to make a reward */
			object_prep(q_ptr, k_idx);

			apply_magic(q_ptr, object_level, AMF_GOOD);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);
		}

		/* Hack -- Guardian has random tarot card */

		/* Get local object */
		q_ptr = &forge;

		/* Prepare a random tarot card */
		prepare_tarot_card(q_ptr, -1);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);

#ifdef JP
		msg_format("あなたは%sを制覇した！",d_name+d_info[dungeon_type].name);
#else
		msg_format("You have conquered %s!",d_name+d_info[dungeon_type].name);
#endif
	}

	/* Determine how much we can drop */
	if ((r_ptr->flags1 & RF1_DROP_60) && (randint0(100) < 60)) number++;
	if ((r_ptr->flags1 & RF1_DROP_90) && (randint0(100) < 90)) number++;
	if  (r_ptr->flags1 & RF1_DROP_1D2) number += damroll(1, 2);
	if  (r_ptr->flags1 & RF1_DROP_2D2) number += damroll(2, 2);
	if  (r_ptr->flags1 & RF1_DROP_3D2) number += damroll(3, 2);
	if  (r_ptr->flags1 & RF1_DROP_4D2) number += damroll(4, 2);

	if (cloned && !(r_ptr->flags1 & RF1_UNIQUE))
		number = 0; /* Clones drop no stuff unless Cloning Pits */

	if (is_pet(m_ptr) || p_ptr->inside_arena)
		number = 0; /* Pets drop no stuff */
	if (!drop_item && (r_ptr->d_char != '$')) number = 0;

	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/* Average dungeon and monster levels */
	object_level = (dun_level + r_ptr->level) / 2;

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Make Gold */
		if (do_gold && (!do_item || (randint0(100) < 50)))
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;

			/* XXX XXX XXX */
			dump_gold++;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (!make_object(q_ptr, am_flags)) continue;

			/* XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = base_level;

	/* Reset "coin" type */
	coin_type = 0;


	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}

	/* Redraw the "title" */
	if (m_ptr->r_idx == MON_FILARHH) p_ptr->redraw |= (PR_TITLE);

	/* Only process "Quest Monsters" */
	if (!(r_ptr->flags1 & RF1_QUESTOR)) return;

	/* Winner? */
	if (m_ptr->r_idx == MON_DOLGARUA)
	{
		/* Total winner */
		p_ptr->total_winner = TRUE;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

#ifdef JP
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "見事にTObandの勝利者となった！");
#else
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "become *WINNER* of TOband finely!");
#endif

		/* Congratulations */
#ifdef JP
msg_print("*** おめでとう ***");
#else
		msg_print("*** CONGRATULATIONS ***");
#endif

#ifdef JP
msg_print("あなたはゲームをコンプリートしました。");
#else
		msg_print("You have won the game!");
#endif

#ifdef JP
msg_print("準備が整ったら引退(自殺コマンド)しても結構です。");
#else
		msg_print("You may retire (commit suicide) when you are ready.");
#endif

	}
}

/*
 * Modify the physical damage done to the monster.
 * (for example when it's invulnerable or shielded)
 *
 * ToDo: Accept a damage-type to calculate the modified damage from
 * things like fire, frost, lightning, poison, ... attacks.
 *
 * "type" is not yet used and should be 0.
 */
int mon_damage_mod(monster_type *m_ptr, int dam, bool force_damage)
{
	if (m_ptr->invulner)
	{
		if (force_damage)
		{
			if (!p_ptr->blind && m_ptr->ml)
			{
#ifdef JP
msg_print("バリアを切り裂いた！");
#else
				msg_print("The barrier is penetrated!");
#endif
			}
		}
		else if (!one_in_(PENETRATE_INVULNERABILITY))
		{
			return (0);
		}
	}
	return (dam);
}

static s32b get_exp_from_mon_aux(int dam, monster_type *m_ptr, s32b max_lev, u16b *exp_frac_ptr)
{
	s32b         div, new_exp, new_exp_frac;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int          monnum_penarty = 0;

	u32b m_exp;
	u32b m_exp_h, m_exp_l;
	u32b div_h, div_l;

	if (r_ptr->flags2 & RF2_MULTIPLY)
	{
		monnum_penarty = r_ptr->r_pkills / 400;
		if (monnum_penarty > 8) monnum_penarty = 8;
	}
	if (r_ptr->flags1 & RF1_UNIQUE)
	{
		m_exp = (long)r_ptr->mexp * r_ptr->level;
		div = (max_lev + 2);
	}
	else
	{
		m_exp = (long)r_ptr->mexp * r_ptr->level * extract_energy[m_ptr->mspeed];
		div = (max_lev + 2) * extract_energy[r_ptr->speed];
	}
	m_exp_h = m_exp/0x10000L;
	m_exp_l = m_exp%0x10000L;
	m_exp_h *= dam;
	m_exp_l *= dam;
	m_exp_h += m_exp_l / 0x10000L;
	m_exp_l %= 0x10000L;

	/* real monster maxhp have effect on EXP */
	if(!(r_ptr->flags1 & RF1_FORCE_MAXHP))
	{
	  u32b maxhp = m_ptr->max_maxhp*2;
	  m_exp_h *= maxhp;
	  m_exp_l *= maxhp;
	  m_exp_h += m_exp_l / 0x10000L;
	  m_exp_l %= 0x10000L;

	  div *= r_ptr->hdice * (r_ptr->hside + 1);
	}
	if (!dun_level && !ambush_flag && (!(r_ptr->flags8 & RF8_WILD_ONLY) || !(r_ptr->flags1 & RF1_UNIQUE))) div *= 4;
	div_h = div/0x10000L;
	div_l = div%0x10000L;
	div_h *= (m_ptr->max_maxhp*2);
	div_l *= (m_ptr->max_maxhp*2);
	div_h += div_l / 0x10000L;
	div_l %= 0x10000L;

	while (monnum_penarty)
	{
		div_h *= 4;
		div_l *= 4;
		div_h += div_l / 0x10000L;
		div_l %= 0x10000L;
		monnum_penarty--;
	}

	m_exp_l = (0x7fffffff & (m_exp_h << 16)) | m_exp_l;
	m_exp_h = m_exp_h >> 15;
	div_l = (0x7fffffff & (div_h << 16)) | div_l;
	div_h = div_h >> 15;

#define M_INT_GREATER63(h1,l1,h2,l2)  ( (h1>h2)||( (h1==h2)&&(l1>=l2)))
#define M_INT_SUB63(h1,l1, h2,l2) {h1-=h2;if(l1<l2){l1+=0x80000000;h1--;}l1-=l2;}
#define M_INT_LSHIFT63(h1,l1) {h1=(h1<<1)|(l1>>30);l1=(l1<<1)&0x7fffffff;}
#define M_INT_RSHIFT63(h1,l1) {l1=(l1>>1)|(h1<<30);h1>>=1;}
#define M_INT_DIV63(h1,l1,h2,l2,result) \
	do { \
		int bit = 1; \
		result = 0; \
		while (M_INT_GREATER63(h1,l1, h2, l2)) { M_INT_LSHIFT63(h2, l2); bit <<= 1; } \
		for (bit >>= 1; bit >= 1; bit >>= 1) { \
			M_INT_RSHIFT63(h2, l2); \
			if (M_INT_GREATER63(h1, l1, h2, l2)) \
			{ result |= bit; M_INT_SUB63(h1, l1, h2, l2); } \
		} \
	} while (0);

	/* Give some experience for the kill */
	M_INT_DIV63(m_exp_h, m_exp_l, div_h, div_l, new_exp);

	/* Handle fractional experience */
	/* multiply 0x10000L to remainder */
	m_exp_h = (m_exp_h<<16) | (m_exp_l>>15);
	m_exp_l <<= 16;
	M_INT_DIV63(m_exp_h, m_exp_l, div_h, div_l, new_exp_frac);
	new_exp_frac += *exp_frac_ptr;
	/* Keep track of experience */
	if (new_exp_frac >= 0x10000L)
	{
		new_exp++;
		*exp_frac_ptr = (u16b)(new_exp_frac - 0x10000L);
	}
	else
	{
		*exp_frac_ptr = (u16b)new_exp_frac;
	}

	return new_exp;
}

void get_exp_from_mon(int dam, monster_type *m_ptr)
{
	if (!m_ptr->r_idx) return;
	if (is_pet(m_ptr)) return;
	else
	{
		cexp_info_type *cexp_ptr;
		int total_max_clev = 0;
		int i;

		for (i = 0; i < MAX_CLASS; i++)
		{
			cexp_ptr = &p_ptr->cexp_info[i];
			if (cexp_ptr->max_clev > 0) total_max_clev += cexp_ptr->max_clev;
		}

		/* Gain experience */
		gain_class_exp(get_exp_from_mon_aux(dam, m_ptr, total_max_clev, &cexp_ptr->cexp_frac));
		gain_racial_exp(get_exp_from_mon_aux(dam, m_ptr, p_ptr->max_plv, &p_ptr->exp_frac));
	}
}


static void expire_current_class(void)
{
	char buf[80];
	byte old_pclass = p_ptr->pclass;
	cexp_info_type *cexp_ptr;

	switch (old_pclass)
	{
	case CLASS_TEMPLEKNIGHT:
		msg_print("ロスローリアン本隊から書状が届いた。");
		msg_print("「貴公の度重なる国家への反逆を、これ以上許すわけにはいかん。ゆえに貴公の騎士資格を剥奪する。」");
		misc_event_flags |= EVENT_CANNOT_BE_TEMPLEKNIGHT;
		break;

	case CLASS_WHITEKNIGHT:
		msg_print("新生ゼノビア王国から書状が届いた。");
		msg_print("「貴公の度重なる国家への反逆を、これ以上許すわけにはいかん。ゆえに貴公の騎士資格を剥奪する。」");
		misc_event_flags |= EVENT_CANNOT_BE_WHITEKNIGHT;
		break;

	default:
		return;
	}

	dispel_player();
	set_action(ACTION_NONE);

	/* Class reset */
	p_ptr->pclass = (p_ptr->psex == SEX_MALE) ? CLASS_SOLDIER : CLASS_AMAZONESS;
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &m_info[p_ptr->pclass];
	p_ptr->s_ptr = &s_info[p_ptr->pclass];
	cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

	if (old_pclass == CLASS_TEMPLEKNIGHT) change_level99_quest(FALSE);

	sprintf(buf, "%sの資格を剥奪され、%sになった。", class_info[old_pclass].title, cp_ptr->title);
	msg_print(buf);
	do_cmd_write_nikki(NIKKI_BUNSHOU, 0, buf);
	msg_print(NULL);

	if (cp_ptr->c_flags & PCF_NO_DIGEST) p_ptr->food = PY_FOOD_FULL - 1;

	if (!cexp_ptr->max_clev)
	{
		cexp_ptr->max_clev = cexp_ptr->clev = 1;
		if (!cexp_ptr->max_max_clev) cexp_ptr->max_max_clev = 1;
	}

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();

	/* Update stuff */
	update_stuff();

	if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
	if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;

	/* Update stuff */
	p_ptr->update |= (PU_HP | PU_MANA);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY | PR_MAP);

	redraw_stuff();

	/* Window stuff */
	p_ptr->window |= (PW_SPELL | PW_PLAYER);
}


/*
 * Decreases monsters hit points, handling monster death.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Only "physical attacks" can induce the "You have slain" message.
 * Missile and Spell attacks will induce the "dies" message, or
 * various "specialized" messages.  Note that "You have destroyed"
 * and "is destroyed" are synonyms for "You have slain" and "dies".
 *
 * Hack -- unseen monsters yield "You have killed it." message.
 *
 * Added fear (DGK) and check whether to print fear messages -CWS
 *
 * Made name, sex, and capitalization generic -BEN-
 *
 * As always, the "ghost" processing is a total hack.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * XXX XXX XXX Consider decreasing monster experience over time, say,
 * by using "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))"
 * instead of simply "(m_exp * m_lev) / (p_lev)", to make the first
 * monster worth more than subsequent monsters.  This would also need
 * to induce changes in the monster recall code.
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note, bool is_stoned)
{
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	monster_type    exp_mon;

	int         i;
	int         expdam;

	char m_name[80];

	COPY(&exp_mon, m_ptr, monster_type);
	if (!(r_ptr->flags7 & RF7_KILL_EXP))
	{
		expdam = (m_ptr->hp > dam) ? dam : m_ptr->hp;
		if (r_ptr->flags6 & RF6_HEAL) expdam = (expdam+1) * 2 / 3;

		get_exp_from_mon(expdam, &exp_mon);

		/* Hack -- Prevent bug */
		if (!m_ptr->r_idx) return TRUE;
	}

	/* Extract monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
	if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;

	if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

	if (p_ptr->action == ACTION_STEALTH)
	{
		set_action(ACTION_NONE);
	}

	/* Hurt it */
	m_ptr->hp -= dam;
	if (show_damage && m_ptr->ml && (dam > 0))
#ifdef JP
		msg_format("%^sに%dのダメージ。", m_name, dam);
#else
		msg_format("%^s takes %d damages.", m_name, dam);
#endif

	if (p_ptr->use_decoy) break_decoy();

	/* It is dead or stoned now */
	if ((m_ptr->hp < 0) || is_stoned)
	{
		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & RF1_UNIQUE && !(m_ptr->smart1 & SM1_CLONED))
			r_ptr->max_num = 0;

		/* When the player kills a Nazgul, it stays dead */
		if (r_ptr->flags7 & RF7_NAZGUL) r_ptr->max_num--;

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags1 & RF1_UNIQUE))
		{
			/* Count kills this life */
			if (r_ptr->r_pkills < MAX_SHORT) r_ptr->r_pkills++;

			/* Count kills in all lives */
			if (r_ptr->r_tkills < MAX_SHORT) r_ptr->r_tkills++;

			/* Hack -- Auto-recall */
			monster_race_track(m_ptr->ap_r_idx);
		}

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

		if (r_ptr->flags2 & RF2_CAN_SPEAK)
		{
			char line_got[1024];

			/* Dump a message */
#ifdef JP
			if (!get_rnd_line("mondeath_j.txt", m_ptr->r_idx, line_got))
#else
			if (!get_rnd_line("mondeath.txt", m_ptr->r_idx, line_got))
#endif

				msg_format("%^s %s", m_name, line_got);

			if (m_ptr->r_idx == MON_DOLGARUA)
			{
				/* Make screen dump */
				screen_dump = make_screen_dump();
			}
		}

		if ((r_ptr->flags1 & RF1_UNIQUE) && record_destroy_uniq)
			do_cmd_write_nikki(NIKKI_UNIQUE, 0, r_name + r_ptr->name);

		/* Make a sound */
		if (r_ptr->flags1 & RF1_MALE)
		{
			sound(SOUND_M_KILL);
		}
		else if (r_ptr->flags1 & RF1_FEMALE)
		{
			sound(SOUND_F_KILL);
		}
		else if (monster_living(r_ptr))
		{
			sound(SOUND_KILL);
		}
		else
		{
			sound(SOUND_N_KILL);
		}

		/* Stoned */
		if (is_stoned)
		{
#ifdef JP
			msg_format("%sは石像になった。", m_name);
#else
			msg_format("%s has become a stone statue.", m_name);
#endif
		}

		/* Death by Missile/Spell attack */
		else if (note)
		{
			msg_format("%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
#ifdef JP
			msg_format("%sを殺した。", m_name);
#else
			msg_format("You have killed %s.", m_name);
#endif

		}

		/* Death by Physical attack -- non-living monster */
		else if (!monster_living(r_ptr))
		{
			bool explode = FALSE;

			for (i = 0; i < 4; i++)
			{
				if (r_ptr->blow[i].method == RBM_EXPLODE) explode = TRUE;
			}

			/* Special note at death */
			if (explode)
#ifdef JP
				msg_format("%sは爆発して粉々になった。", m_name);
#else
				msg_format("%s explodes into tiny shreds.", m_name);
#endif
			else
			{
#ifdef JP
				msg_format("%sを倒した。", m_name);
#else
				msg_format("You have destroyed %s.", m_name);
#endif
			}
		}

		/* Death by Physical attack -- living monster */
		else
		{
#ifdef JP
			msg_format("%sを葬り去った。", m_name);
#else
			msg_format("You have slain %s.", m_name);
#endif

		}
		if ((r_ptr->flags1 & RF1_UNIQUE) && !(m_ptr->smart1 & SM1_CLONED) && !astral_mode)
		{
			for (i = 0; i < MAX_KUBI; i++)
			{
				if (kubi_r_idx[i] == m_ptr->r_idx)
				{
#ifdef JP
					msg_format("%sの首には賞金がかかっている。", m_name);
#else
					msg_format("There is a price on %s's head.", m_name);
#endif
					break;
				}
			}
		}

		if (!p_ptr->inside_arena && !(m_ptr->smart1 & SM1_CLONED))
		{
			int kill_temple = ((p_ptr->pclass == CLASS_TEMPLEKNIGHT) && (r_ptr->flags3 & RF3_TEMPLE)) ? 2 : 1;
			int kill_zenobian_forces = ((p_ptr->pclass == CLASS_WHITEKNIGHT) && (r_ptr->flags7 & RF7_ZENOBIAN_FORCES)) ? 2 : 1;

			/* Alignment change */
			if (r_ptr->d_char == 't')
			{
				if (!dun_level && p_ptr->town_num) change_chaos_frame(town[p_ptr->town_num].ethnic, -1);
			}
			if (r_ptr->flags7 & RF7_LAWFUL)
			{
				if (r_ptr->flags1 & RF1_UNIQUE)
					change_your_alignment_lnc(r_ptr->level >= p_ptr->lev ? -4 : -2);
				else if (one_in_((r_ptr->level >= p_ptr->lev) ? 5 : 10))
					change_your_alignment_lnc(-1);
			}
			if (r_ptr->flags7 & RF7_CHAOTIC)
			{
				if (r_ptr->flags1 & RF1_UNIQUE)
					change_your_alignment_lnc(r_ptr->level >= p_ptr->lev ? 4 : 2);
				else if (one_in_((r_ptr->level >= p_ptr->lev) ? 5 : 10))
					change_your_alignment_lnc(1);
			}

			/* Chaos frame change */
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				if (r_ptr->flags2 & RF2_WALSTANIAN) change_chaos_frame(ETHNICITY_WALSTANIAN, -10);
				if (r_ptr->flags2 & RF2_GARGASTAN) change_chaos_frame(ETHNICITY_GARGASTAN, -10);
				if (r_ptr->flags2 & RF2_BACRUM) change_chaos_frame(ETHNICITY_BACRUM, -10);
				if (r_ptr->flags2 & RF2_ZENOBIAN) change_chaos_frame(ETHNICITY_ZENOBIAN, -10 * kill_zenobian_forces);
				if (r_ptr->flags2 & RF2_LODIS) change_chaos_frame(ETHNICITY_LODIS, -10 * kill_temple);

				switch (m_ptr->r_idx)
				{
				case MON_MAN_LOOK_SEA:
					change_chaos_frame(ETHNICITY_WALSTANIAN, -10);
					change_chaos_frame(ETHNICITY_GARGASTAN, -10);
					change_chaos_frame(ETHNICITY_BACRUM, -10);
					change_chaos_frame(ETHNICITY_ZENOBIAN, -40 * kill_zenobian_forces);
					change_chaos_frame(ETHNICITY_LODIS, 40);
					if (p_ptr->pclass == CLASS_WHITEKNIGHT) expire_current_class();
					break;

				case MON_RONWE:
					change_chaos_frame(ETHNICITY_WALSTANIAN, -40);
					change_chaos_frame(ETHNICITY_GARGASTAN, 10);
					change_chaos_frame(ETHNICITY_BACRUM, 10);
					change_chaos_frame(ETHNICITY_ZENOBIAN, 10);
					change_chaos_frame(ETHNICITY_LODIS, 10);
					break;

				case MON_ZAEBOS:
					change_chaos_frame(ETHNICITY_WALSTANIAN, 5);
					change_chaos_frame(ETHNICITY_GARGASTAN, -20);
					change_chaos_frame(ETHNICITY_BACRUM, 5);
					change_chaos_frame(ETHNICITY_ZENOBIAN, 5);
					change_chaos_frame(ETHNICITY_LODIS, 5);
					break;

				case MON_BARBATOS:
					change_chaos_frame(ETHNICITY_WALSTANIAN, 10);
					change_chaos_frame(ETHNICITY_GARGASTAN, -40);
					change_chaos_frame(ETHNICITY_BACRUM, 10);
					change_chaos_frame(ETHNICITY_ZENOBIAN, 10);
					change_chaos_frame(ETHNICITY_LODIS, 10);
					break;

				case MON_WARREN:
					change_chaos_frame(ETHNICITY_WALSTANIAN, -5);
					change_chaos_frame(ETHNICITY_GARGASTAN, -5);
					change_chaos_frame(ETHNICITY_BACRUM, -5);
					change_chaos_frame(ETHNICITY_ZENOBIAN, -20 * kill_zenobian_forces);
					change_chaos_frame(ETHNICITY_LODIS, 20);
					break;

				case MON_LEONARD:
					change_chaos_frame(ETHNICITY_WALSTANIAN, -20);
					change_chaos_frame(ETHNICITY_GARGASTAN, 5);
					change_chaos_frame(ETHNICITY_BACRUM, 5);
					change_chaos_frame(ETHNICITY_ZENOBIAN, 5);
					change_chaos_frame(ETHNICITY_LODIS, 5);
					break;

				case MON_BRANTA:
					change_chaos_frame(ETHNICITY_WALSTANIAN, 10);
					change_chaos_frame(ETHNICITY_GARGASTAN, 10);
					change_chaos_frame(ETHNICITY_BACRUM, -40);
					change_chaos_frame(ETHNICITY_ZENOBIAN, 10);
					change_chaos_frame(ETHNICITY_LODIS, -10);
					break;

				case MON_BALZEPHO:
					change_chaos_frame(ETHNICITY_WALSTANIAN, 5);
					change_chaos_frame(ETHNICITY_GARGASTAN, 5);
					change_chaos_frame(ETHNICITY_BACRUM, -5);
					change_chaos_frame(ETHNICITY_ZENOBIAN, 20);
					change_chaos_frame(ETHNICITY_LODIS, -20 * kill_temple);
					break;

				case MON_BERSALIA:
					change_chaos_frame(ETHNICITY_WALSTANIAN, -50);
					change_chaos_frame(ETHNICITY_GARGASTAN, -50);
					change_chaos_frame(ETHNICITY_BACRUM, -50);
					break;

				case MON_LANCELOT:
					change_chaos_frame(ETHNICITY_WALSTANIAN, 10);
					change_chaos_frame(ETHNICITY_GARGASTAN, 10);
					change_chaos_frame(ETHNICITY_BACRUM, -10);
					change_chaos_frame(ETHNICITY_ZENOBIAN, 40);
					change_chaos_frame(ETHNICITY_LODIS, -40 * kill_temple);
					if (p_ptr->pclass == CLASS_TEMPLEKNIGHT) expire_current_class();
					break;

				case MON_DOLGARUA:
					change_chaos_frame(ETHNICITY_WALSTANIAN, 10);
					change_chaos_frame(ETHNICITY_GARGASTAN, 10);
					change_chaos_frame(ETHNICITY_BACRUM, 10);
					change_chaos_frame(ETHNICITY_ZENOBIAN, 10);
					change_chaos_frame(ETHNICITY_LODIS, 10);
					break;

				case MON_FILARHH:
					change_chaos_frame(ETHNICITY_WALSTANIAN, -600);
					change_chaos_frame(ETHNICITY_GARGASTAN, -600);
					change_chaos_frame(ETHNICITY_BACRUM, -600);
					change_chaos_frame(ETHNICITY_ZENOBIAN, -600);
					change_chaos_frame(ETHNICITY_LODIS, -600);
					break;
				}
			}
			else if (one_in_(5))
			{
				if (r_ptr->flags2 & RF2_WALSTANIAN) change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
				if (r_ptr->flags2 & RF2_GARGASTAN) change_chaos_frame(ETHNICITY_GARGASTAN, -1);
				if (r_ptr->flags2 & RF2_BACRUM) change_chaos_frame(ETHNICITY_BACRUM, -1);
				if (r_ptr->flags2 & RF2_ZENOBIAN) change_chaos_frame(ETHNICITY_ZENOBIAN, -1 * kill_zenobian_forces);
				if (r_ptr->flags2 & RF2_LODIS) change_chaos_frame(ETHNICITY_LODIS, -1 * kill_temple);
			}

			if ((p_ptr->pclass == CLASS_TEMPLEKNIGHT) && (r_ptr->flags7 & RF7_ZENOBIAN_FORCES))
			{
				change_chaos_frame(ETHNICITY_ZENOBIAN, -1);
				change_chaos_frame(ETHNICITY_LODIS, 1);
			}
			if ((p_ptr->pclass == CLASS_WHITEKNIGHT) && (r_ptr->flags3 & RF3_TEMPLE))
			{
				change_chaos_frame(ETHNICITY_ZENOBIAN, 1);
				change_chaos_frame(ETHNICITY_LODIS, -1);
			}
		}

		/* Generate treasure */
		monster_death(m_idx, TRUE, is_stoned);

		/* Delete the monster */
		delete_monster_idx(m_idx);

		if (r_ptr->flags7 & RF7_KILL_EXP)
			get_exp_from_mon(exp_mon.max_maxhp*2, &exp_mon);
		else
			get_exp_from_mon((exp_mon.max_maxhp+1L) * 9L / 10L, &exp_mon);

		/* Not afraid */
		(*fear) = FALSE;

		/* Monster is dead */
		return (TRUE);
	}


#ifdef ALLOW_FEAR

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint1(dam);

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
		if ((randint1(10) >= percentage) ||
		    ((dam >= m_ptr->hp) && (randint0(100) < 80)))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* XXX XXX XXX Hack -- Add some timed fear */
			m_ptr->monfear = (randint1(10) +
			                  (((dam >= m_ptr->hp) && (percentage > 7)) ?
			                   20 : ((11 - percentage) * 5)));
		}
	}

#endif

#if 0
	if (p_ptr->riding && (p_ptr->riding == m_idx) && (dam > 0))
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

		if (m_ptr->hp > m_ptr->maxhp/3) dam = (dam + 1) / 2;
		if (rakuba((dam > 200) ? 200 : dam, FALSE))
		{
#ifdef JP
msg_format("%^sに振り落とされた！", m_name);
#else
				msg_format("%^s has thrown you off!", m_name);
#endif
		}
	}
#endif

	/* Not dead yet */
	return (FALSE);
}


/*
 * Get term size and calculate screen size
 */
void get_screen_size(int *wid_p, int *hgt_p)
{
	Term_get_size(wid_p, hgt_p);
	*hgt_p -= ROW_MAP + 2;
	*wid_p -= COL_MAP + 2;
	if (use_bigtile) *wid_p /= 2;
}


/*
 * Calculates current boundaries
 * Called below and from "do_cmd_locate()".
 */
void panel_bounds_center(void)
{
	int wid, hgt;

	/* Get size */
	get_screen_size(&wid, &hgt);

	panel_row_max = panel_row_min + hgt - 1;
	panel_row_prt = panel_row_min - 1;
	panel_col_max = panel_col_min + wid - 1;
	panel_col_prt = panel_col_min - 13;
}


/*
 * Map resizing whenever the main term changes size
 */
void resize_map(void)
{
	/* Only if the dungeon exists */
	if (!character_dungeon) return;
	
	/* Mega-Hack -- no panel yet */
	panel_row_max = 0;
	panel_col_max = 0;

	/* Reset the panels */
	panel_row_min = cur_hgt;
	panel_col_min = cur_wid;
				
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_TORCH | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Forget lite/view */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update lite/view */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Hack -- update */
	handle_stuff();
	
	/* Redraw */
	Term_redraw();

	/*
	 * Waiting command;
	 * Place the cursor on the player
	 */
	if (can_save) move_cursor_relative(py, px);

	/* Refresh */
	Term_fresh();
}

/*
 * Redraw a term when it is resized
 */
void redraw_window(void)
{
	/* Only if the dungeon exists */
	if (!character_dungeon) return;
	
	/* Hack - Activate term zero for the redraw */
	Term_activate(&term_screen[0]);
	
	/* Hack -- react to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT);

	/* Hack -- update */
	handle_stuff();

	/* Redraw */
	Term_redraw();

	/* Refresh */
	Term_fresh();
}


/*
 * Handle a request to change the current panel
 *
 * Return TRUE if the panel was changed.
 *
 * Also used in do_cmd_locate
 */
bool change_panel(int dy, int dx)
{
	int y, x;
	int wid, hgt;

	/* Get size */
	get_screen_size(&wid, &hgt);

	/* Apply the motion */
	y = panel_row_min + dy * hgt / 2;
	x = panel_col_min + dx * wid / 2;

	/* Verify the row */
	if (y > cur_hgt - hgt) y = cur_hgt - hgt;
	if (y < 0) y = 0;

	/* Verify the col */
	if (x > cur_wid - wid) x = cur_wid - wid;
	if (x < 0) x = 0;

	/* Handle "changes" */
	if ((y != panel_row_min) || (x != panel_col_min))
	{
		/* Save the new panel info */
		panel_row_min = y;
		panel_col_min = x;

		/* Recalculate the boundaries */
		panel_bounds_center();

		/* Update stuff */
		p_ptr->update |= (PU_MONSTERS);

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Handle stuff */
		handle_stuff();

		/* Success */
		return (TRUE);
	}

	/* No change */
	return (FALSE);
}


/*
 * Given an row (y) and col (x), this routine detects when a move
 * off the screen has occurred and figures new borders. -RAK-
 *
 * "Update" forces a "full update" to take place.
 *
 * The map is reprinted if necessary, and "TRUE" is returned.
 */
void verify_panel(void)
{
	int y = py;
	int x = px;
	int wid, hgt;

	int prow_min;
	int pcol_min;
	int max_prow_min;
	int max_pcol_min;

	/* Get size */
	get_screen_size(&wid, &hgt);

	max_prow_min = cur_hgt - hgt;
	max_pcol_min = cur_wid - wid;

	/* Bounds checking */
	if (max_prow_min < 0) max_prow_min = 0;
	if (max_pcol_min < 0) max_pcol_min = 0;

		/* Center on player */
	if (center_player && (center_running || !running))
	{
		/* Center vertically */
		prow_min = y - hgt / 2;
		if (prow_min < 0) prow_min = 0;
		else if (prow_min > max_prow_min) prow_min = max_prow_min;

		/* Center horizontally */
		pcol_min = x - wid / 2;
		if (pcol_min < 0) pcol_min = 0;
		else if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;
	}
	else
	{
		prow_min = panel_row_min;
		pcol_min = panel_col_min;

		/* Scroll screen when 2 grids from top/bottom edge */
		if (y > panel_row_max - 2)
		{
			while (y > prow_min + hgt-1 - 2)
			{
				prow_min += (hgt / 2);
			}
		}

		if (y < panel_row_min + 2)
		{
			while (y < prow_min + 2)
			{
				prow_min -= (hgt / 2);
			}
		}

		if (prow_min > max_prow_min) prow_min = max_prow_min;
		if (prow_min < 0) prow_min = 0;

		/* Scroll screen when 4 grids from left/right edge */
		if (x > panel_col_max - 4)
		{
			while (x > pcol_min + wid-1 - 4)
			{
				pcol_min += (wid / 2);
			}
		}
		
		if (x < panel_col_min + 4)
		{
			while (x < pcol_min + 4)
			{
				pcol_min -= (wid / 2);
			}
		}

		if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;
		if (pcol_min < 0) pcol_min = 0;
	}

	/* Check for "no change" */
	if ((prow_min == panel_row_min) && (pcol_min == panel_col_min)) return;

	/* Save the new panel info */
	panel_row_min = prow_min;
	panel_col_min = pcol_min;

	/* Hack -- optional disturb on "panel change" */
	if (disturb_panel && !center_player) disturb(0, 0);

	/* Recalculate the boundaries */
	panel_bounds_center();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}


/*
 * Monster health description
 */
cptr look_mon_desc(int m_idx, u32b mode)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *ap_r_ptr = &r_info[m_ptr->ap_r_idx];
	bool         living;
	int          perc;
	cptr desc;
	cptr attitude;
	cptr clone;

	/* Determine if the monster is "living" */
	living = monster_living(ap_r_ptr);

	/* Calculate a health "percentage" */
	perc = 100L * m_ptr->hp / m_ptr->maxhp;

	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
#ifdef JP
		desc = living ? "無傷" : "無ダメージ";
#else
		desc = living ? "unhurt" : "undamaged";
#endif

	}

	else if (perc >= 60)
	{
#ifdef JP
		desc = living ? "軽傷" : "小ダメージ";
#else
		desc = living ? "somewhat wounded" : "somewhat damaged";
#endif

	}

	else if (perc >= 25)
	{
#ifdef JP
		desc = living ? "負傷" : "中ダメージ";
#else
		desc = living ? "wounded" : "damaged";
#endif

	}

	else if (perc >= 10)
	{
#ifdef JP
		desc = living ? "重傷" : "大ダメージ";
#else
		desc = living ? "badly wounded" : "badly damaged";
#endif

	}

	else 
	{
#ifdef JP
		desc = living ? "半死半生" : "倒れかけ";
#else
		desc = living ? "almost dead" : "almost destroyed";
#endif
	}


	/* Need attitude information? */
	if (!(mode & 0x01))
	{
		/* Full information is not needed */
		attitude = "";
	}
	else if (is_pet(m_ptr))
	{
#ifdef JP
		attitude = ", ペット";
#else
		attitude = ", pet";
#endif
	}
	else if (is_friendly(m_ptr))
	{
#ifdef JP
		attitude = ", 友好的";
#else
		attitude = ", friendly";
#endif
	}
	else
	{
#ifdef JP
		attitude = "";
#else
		attitude = "";
#endif
	}


	/* Clone monster? */
	if (m_ptr->smart1 & SM1_CLONED)
	{
		clone = ", clone";
	}
	else
	{
		clone = "";
	}

	/* Display monster's level --- idea borrowed from ToME */
	if (ap_r_ptr->r_tkills)
	{
#ifdef JP
		return format("レベル%d, %s%s%s", ap_r_ptr->level, desc, attitude, clone);
#else
		return format("Level %d, %s%s%s", ap_r_ptr->level, desc, attitude, clone);
#endif
	}
	else 
	{
#ifdef JP
		return format("レベル???, %s%s%s", desc, attitude, clone);
#else
		return format("Level ???, %s%s%s", desc, attitude, clone);
#endif
	}
}



/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(vptr u, vptr v, int p, int q)
{
	int z, a, b;

	/* Done sort */
	if (p >= q) return;

	/* Pivot */
	z = p;

	/* Begin */
	a = p;
	b = q;

	/* Partition */
	while (TRUE)
	{
		/* Slide i2 */
		while (!(*ang_sort_comp)(u, v, b, z)) b--;

		/* Slide i1 */
		while (!(*ang_sort_comp)(u, v, z, a)) a++;

		/* Done partition */
		if (a >= b) break;

		/* Swap */
		(*ang_sort_swap)(u, v, a, b);

		/* Advance */
		a++, b--;
	}

	/* Recurse left side */
	ang_sort_aux(u, v, p, b);

	/* Recurse right side */
	ang_sort_aux(u, v, b+1, q);
}


/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(vptr u, vptr v, int n)
{
	/* Sort the array */
	ang_sort_aux(u, v, 0, n-1);
}



/*** Targeting Code ***/


/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targeting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
bool target_able(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

	if (p_ptr->riding && (p_ptr->riding == m_idx)) return (TRUE);

	/* Monster must be projectable */
	if (!projectable(py, px, m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* XXX XXX XXX Hack -- Never target trappers */
	/* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

	/* Assume okay */
	return (TRUE);
}




/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(void)
{
	/* Accept stationary targets */
	if (target_who < 0) return (TRUE);

	/* Check moving targets */
	if (target_who > 0)
	{
		/* Accept reasonable targets */
		if (target_able(target_who))
		{
			monster_type *m_ptr = &m_list[target_who];

			/* Acquire monster location */
			target_row = m_ptr->fy;
			target_col = m_ptr->fx;

			/* Good target */
			return (TRUE);
		}
	}

	/* Assume no target */
	return (FALSE);
}



/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
static bool ang_sort_comp_distance(vptr u, vptr v, int a, int b)
{
	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	int da, db, kx, ky;

	/* Absolute distance components */
	kx = x[a]; kx -= px; kx = ABS(kx);
	ky = y[a]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Absolute distance components */
	kx = x[b]; kx -= px; kx = ABS(kx);
	ky = y[b]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Compare the distances */
	return (da <= db);
}


/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
static void ang_sort_swap_distance(vptr u, vptr v, int a, int b)
{
	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	byte temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;

	/* Swap "y" */
	temp = y[a];
	y[a] = y[b];
	y[b] = temp;
}



/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx)
{
	int i, v;

	int x2, y2, x3, y3, x4, y4;

	int b_i = -1, b_v = 9999;


	/* Scan the locations */
	for (i = 0; i < temp_n; i++)
	{
		/* Point 2 */
		x2 = temp_x[i];
		y2 = temp_y[i];

		/* Directed distance */
		x3 = (x2 - x1);
		y3 = (y2 - y1);

		/* Verify quadrant */
		if (dx && (x3 * dx <= 0)) continue;
		if (dy && (y3 * dy <= 0)) continue;

		/* Absolute distance */
		x4 = ABS(x3);
		y4 = ABS(y3);

		/* Verify quadrant */
		if (dy && !dx && (x4 > y4)) continue;
		if (dx && !dy && (y4 > x4)) continue;

		/* Approximate Double Distance */
		v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

		/* XXX XXX XXX Penalize location */

		/* Track best */
		if ((b_i >= 0) && (v >= b_v)) continue;

		/* Track best */
		b_i = i; b_v = v;
	}

	/* Result */
	return (b_i);
}


/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_accept(int y, int x)
{
	cave_type *c_ptr;

	s16b this_o_idx, next_o_idx = 0;

	/* Bounds */
	if (!(in_bounds(y, x))) return (FALSE);

	/* Player grid is always interesting */
	if ((y == py) && (x == px)) return (TRUE);


	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);


	/* Handle "player decoy" */
	if (p_ptr->use_decoy)
	{
		if ((y == p_ptr->decoy_y) && (x == p_ptr->decoy_x)) return TRUE;
	}

	/* Examine the grid */
	c_ptr = &cave[y][x];

	/* Visible monsters */
	if (c_ptr->m_idx)
	{
		monster_type *m_ptr = &m_list[c_ptr->m_idx];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized object */
		if (o_ptr->marked) return (TRUE);
	}

	/* Interesting memorized features */
	if (c_ptr->info & (CAVE_MARK))
	{
		byte feat;

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		switch (feat)
		{
		/* Notice glyphs */
		case FEAT_GLYPH:
		case FEAT_MINOR_GLYPH:

		/* Notice doors */
		case FEAT_OPEN:
		case FEAT_BROKEN:

		/* Notice stairs */
		case FEAT_LESS:
		case FEAT_MORE:
		case FEAT_LESS_LESS:
		case FEAT_MORE_MORE:
		case FEAT_BETWEEN:

		case FEAT_MUSEUM:

#if 0
		/* Notice rubble */
		/* I think FEAT_RUBBLEs should not be "interesting" */
		case FEAT_RUBBLE:

		/* Notice veins with treasure */
		/* Now veins with treasure are too many */
		case FEAT_MAGMA_K:
		case FEAT_QUARTZ_K:
#endif

		/* Notice quest features */
		case FEAT_QUEST_ENTER:
		case FEAT_QUEST_EXIT:
		case FEAT_QUEST_DOWN:
		case FEAT_QUEST_UP:
		case FEAT_TOWN:
		case FEAT_ENTRANCE:
		case FEAT_ENTRANCE_UPWARD:
			return TRUE;

		default:
			/* Notice shops */
			if ((feat >= FEAT_SHOP_HEAD) &&
			    (feat <= FEAT_SHOP_TAIL)) return TRUE;

			/* Notice buildings -KMW- */
			if ((feat >= FEAT_BLDG_HEAD) &&
			    (feat <= FEAT_BLDG_TAIL)) return TRUE;

			/* Notice objects */
			if (c_ptr->info & CAVE_OBJECT) return (TRUE);

			/* Notice traps */
			if (is_trap(feat)) return (TRUE);

			/* Notice doors */
			if ((feat >= FEAT_DOOR_HEAD) &&
			    (feat <= FEAT_DOOR_TAIL)) return TRUE;

			break;
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * Prepare the "temp" array for "target_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_prepare(int mode)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			cave_type *c_ptr;

			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_has_los_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_accept(y, x)) continue;

			c_ptr = &cave[y][x];

			/* Require target_able monsters for "TARGET_KILL" */
			if ((mode & (TARGET_KILL)) && !target_able(c_ptr->m_idx)) continue;

			if ((mode & (TARGET_KILL)) && !target_pet && is_pet(&m_list[c_ptr->m_idx])) continue;

			/* Save the location */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_distance;
	ang_sort_swap = ang_sort_swap_distance;

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n);

	if (p_ptr->riding && target_pet && (temp_n > 1) && (mode & (TARGET_KILL)))
	{
		byte tmp;

		tmp = temp_y[0];
		temp_y[0] = temp_y[1];
		temp_y[1] = tmp;
		tmp = temp_x[0];
		temp_x[0] = temp_x[1];
		temp_x[1] = tmp;
	}
}


static int prt_grid_elem(cave_type *c_ptr, int feat, int col)
{
	char buf[24] = "";
	s16b dominant_elem;
	int  prev_col, i;

	cptr short_elem_name[ELEM_NUM] =
	{
#ifdef JP
		"火", "水", "地", "風"
#else
		"F:", "A:", "E:", "W:"
#endif
	};

	if (feat == FEAT_NONE) return 0;
	if (p_ptr->wild_mode) return 0;

	dominant_elem = get_dominant_feature_elem(c_ptr);
	prev_col = col;

	prt("(", 0, col++);

	for (i = MIN_ELEM; i < ELEM_NUM; i++)
	{
		sprintf(buf, "%s%c%02d,", short_elem_name[i],
			(c_ptr->elem[i] < 0) ? '-' : '+', abs(c_ptr->elem[i]));
		c_prt((i == dominant_elem) ? elem_attr(i) : TERM_WHITE, buf, 0, col);
		col += strlen(buf);
	}

#ifdef JP
	sprintf(buf, "攻+%02d,防+%02d)", f_info[feat].to_offence, f_info[feat].to_defence);
#else
	sprintf(buf, "At:+%02d,Df:+%02d)", f_info[feat].to_offence, f_info[feat].to_defence);
#endif

	prt(buf, 0, col);
	col += strlen(buf);

	return col - prev_col;
}


/*
 * Evaluate number of kill needed to gain level
 */
static void evaluate_monster_exp(char *buf, monster_type *m_ptr)
{
#define M_INT_GREATER(h1,l1,h2,l2)  ( (h1>h2)||( (h1==h2)&&(l1>=l2)))
#define M_INT_SUB(h1,l1, h2,l2) {h1-=h2;if(l1<l2){l1+=0x10000;h1--;}l1-=l2;}
#define M_INT_ADD(h1,l1, h2,l2) {h1+=h2;l1+=l2;if(l1>=0x10000L){l1&=0xFFFF;h1++;}}
#define M_INT_LSHIFT(h1,l1) {h1=(h1<<1)|(l1>>15);l1=(l1<<1)&0xffff;}
#define M_INT_RSHIFT(h1,l1) {l1=(l1>>1)|((h1&1)<<15);h1>>=1;}
#define M_INT_MULT(h1,l1,mul,h2,l2) {l2=(l1*mul)&0xffff;h2=((l1*mul)>>16)+h1*mul;}

	monster_race *ap_r_ptr = &r_info[m_ptr->ap_r_idx];

	u32b tmp_h, tmp_l;
	int bit, result;
	u32b exp_mon = (ap_r_ptr->mexp) * (ap_r_ptr->level);
	u32b exp_mon_h = exp_mon / (p_ptr->max_plv + 2);
	u32b exp_mon_l = ((exp_mon % (p_ptr->max_plv + 2)) * 0x10000 / (p_ptr->max_plv + 2)) & 0xFFFF;

	u32b exp_adv_h = player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100;
	u32b exp_adv_l = ((player_exp[p_ptr->lev - 1] % 100) * p_ptr->expfact * 0x10000 / 100) & 0xFFFF;

	M_INT_SUB(exp_adv_h, exp_adv_l, p_ptr->exp, p_ptr->exp_frac);
	if (p_ptr->lev >= PY_MAX_LEVEL)
		sprintf(buf,"**");
	else if (!ap_r_ptr->r_tkills)
		sprintf(buf,"??");
	else if (M_INT_GREATER(exp_mon_h, exp_mon_l, exp_adv_h, exp_adv_l))
		sprintf(buf,"001");
	else
	{
		M_INT_MULT(exp_mon_h, exp_mon_l, 1000,tmp_h, tmp_l);
		if( M_INT_GREATER(exp_adv_h, exp_adv_l, tmp_h, tmp_l) )
			sprintf(buf,"999");
		else
		{
			bit=1; result=0;
			M_INT_ADD(exp_adv_h, exp_adv_l, exp_mon_h, exp_mon_l);
			M_INT_SUB(exp_adv_h, exp_adv_l, 0, 1);
			while(M_INT_GREATER(exp_adv_h, exp_adv_l, exp_mon_h,exp_mon_l))
			{
				M_INT_LSHIFT(exp_mon_h,exp_mon_l);
				bit <<= 1;
			}
			M_INT_RSHIFT(exp_mon_h,exp_mon_l);bit>>=1;
			for(;bit>=1;bit>>=1)
			{
				if(M_INT_GREATER(exp_adv_h,exp_adv_l,exp_mon_h,exp_mon_l))
				{
					result |= bit;
					M_INT_SUB(exp_adv_h,exp_adv_l,exp_mon_h,exp_mon_l);
				}
				M_INT_RSHIFT(exp_mon_h,exp_mon_l); 
			}
			sprintf(buf,"%03d",result);
		}
	}
}


/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately.  This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text.  This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * Eventually, we may allow multiple objects per grid, or objects
 * and terrain features in the same grid. XXX XXX XXX
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_aux(int y, int x, int mode, cptr info)
{
	cave_type *c_ptr = &cave[y][x];
	s16b this_o_idx, next_o_idx = 0;
	cptr s1 = "", s2 = "", s3 = "", x_info = "";
	bool boring = TRUE;
	byte feat;
	int query = '\001';
	char out_val[MAX_NLEN+80];
	int tmp_len;

#ifdef ALLOW_EASY_FLOOR
	int floor_list[23], floor_num = 0;

	/* Scan all objects in the grid */
	if (easy_floor)
	{
		floor_num = scan_floor(floor_list, y, x, 0x02);

		if (floor_num)
		{
#ifdef JP
			x_info = "x物 ";
#else
			x_info = "x,";
#endif
		}
	}

#endif /* ALLOW_EASY_FLOOR */

	/* Hack -- under the player */
	if ((y == py) && (x == px))
	{
		/* Description */
#ifdef JP
		s1 = "あなたは";
		s2 = "の上";
		s3 = "にいる";
#else
		s1 = "You are ";

		/* Preposition */
		s2 = "on ";
#endif
	}
	else
	{
#ifdef JP
		s1 = "ターゲット:";
#else
		s1 = "Target:";
#endif
	}

	/* Hack -- hallucination */
	if (p_ptr->image)
	{
#ifdef JP
		cptr name = "何か奇妙な物";
#else
		cptr name = "something strange";
#endif


		/* Display a message */
#ifdef JP
		sprintf(out_val, "%s%s%s%s [%s]", s1, name, s2, s3, info);
#else
		sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);
#endif

		prt(out_val, 0, 0);
		move_cursor_relative(y, x);
		query = inkey();

		/* Stop on everything but "return" */
		if ((query != '\r') && (query != '\n')) return query;

		/* Repeat forever */
		return 0;
	}


	/* Actual monsters */
	if (c_ptr->m_idx && m_list[c_ptr->m_idx].ml)
	{
		monster_type *m_ptr = &m_list[c_ptr->m_idx];
		monster_race *ap_r_ptr = &r_info[m_ptr->ap_r_idx];
		char m_name[80];
		bool recall = FALSE;

		/* Not boring */
		boring = FALSE;

		/* Get the monster name ("a kobold") */
		monster_desc(m_name, m_ptr, 0x08);

		/* Hack -- track this monster race */
		monster_race_track(m_ptr->ap_r_idx);

		/* Hack -- health bar for this monster */
		health_track(c_ptr->m_idx);

		/* Hack -- handle stuff */
		handle_stuff();

		/* Interact */
		while (1)
		{
			char acount[10];

			/* Recall */
			if (recall)
			{
				/* Save */
				screen_save();

				/* Recall on screen */
				screen_roff(m_ptr->ap_r_idx, 0);

				/* Hack -- Complete the prompt (again) */
#ifdef JP
				Term_addstr(-1, TERM_WHITE, format("  [r思 %s%s]", x_info, info));
#else
				Term_addstr(-1, TERM_WHITE, format("  [r,%s%s]", x_info, info));
#endif

				/* Command */
				query = inkey();

				/* Restore */
				screen_load();

				/* Normal commands */
				if (query != 'r') break;

				/* Toggle recall */
				recall = FALSE;

				/* Cleare recall text and repeat */
				continue;
			}

			/*** Normal ***/

			/* Describe, and prompt for recall */
			evaluate_monster_exp(acount, m_ptr);

#ifdef JP
			sprintf(out_val, "[%s]%s%s(%s, ", acount, s1, m_name, look_mon_desc(c_ptr->m_idx, 0x01));
#else
			sprintf(out_val, "[%s]%s%s%s%s(%s, ", acount, s1, s2, s3, m_name, look_mon_desc(c_ptr->m_idx, 0x01));
#endif
			tmp_len = strlen(out_val);
			prt(out_val, 0, 0);

			{
				s16b cur_elem = get_cur_melem(m_ptr);
				c_prt(elem_attr(cur_elem), elem_names[cur_elem], 0, tmp_len);
				tmp_len += strlen(elem_names[cur_elem]);
			}

#ifdef JP
			sprintf(out_val, ")%s%s [r思 %s%s]", s2, s3, x_info, info);
#else
			sprintf(out_val, ") [r, %s%s]", x_info, info);
#endif
			prt(out_val, 0, tmp_len);

			/* Place cursor */
			move_cursor_relative(y, x);

			/* Command */
			query = inkey();

			/* Normal commands */
			if (query != 'r') break;

			/* Toggle recall */
			recall = TRUE;
		}

		/* Always stop at "normal" keys */
		if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x')) return query;

		/* Sometimes stop at "space" key */
		if ((query == ' ') && !(mode & (TARGET_LOOK))) return query;

		/* Change the intro */
#ifdef JP
		s1 = "それは";
#else
		s1 = "It is ";
#endif


		/* Hack -- take account of gender */
#ifdef JP
		if (ap_r_ptr->flags1 & (RF1_FEMALE)) s1 = "彼女は";
#else
		if (ap_r_ptr->flags1 & (RF1_FEMALE)) s1 = "She is ";
#endif

#ifdef JP
		else if (ap_r_ptr->flags1 & (RF1_MALE)) s1 = "彼は";
#else
		else if (ap_r_ptr->flags1 & (RF1_MALE)) s1 = "He is ";
#endif


		/* Use a preposition */
#ifdef JP
		s2 = "を";
		s3 = "持っている";
#else
		s2 = "carrying ";
#endif


		/* Scan all objects being carried */
		for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			char o_name[MAX_NLEN];

			object_type *o_ptr;

			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Obtain an object description */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Describe the object */
#ifdef JP
			sprintf(out_val, "%s%s%s%s[%s]", s1, o_name, s2, s3, info);
#else
			sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
#endif

			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Always stop at "normal" keys */
			if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x')) return query;

			/* Sometimes stop at "space" key */
			if ((query == ' ') && !(mode & (TARGET_LOOK))) return query;

			/* Change the intro */
#ifdef JP
			s2 = "をまた";
#else
			s2 = "also carrying ";
#endif
		}

		/* Use a preposition */
#ifdef JP
		s2 = "の上";
		s3 = "にいる";
#else
		s2 = "on ";
#endif
	}


	/* Handle "player decoy" */
	if (p_ptr->use_decoy)
	{
		if ((y == p_ptr->decoy_y) && (x == p_ptr->decoy_x))
		{
			/* Not boring */
			boring = FALSE;

			/* Describe the object */
#ifdef JP
			sprintf(out_val, "%sダミー人形%s%s[%s]", s1, s2, s3, info);
#else
			sprintf(out_val, "%s%s%sYour Decoy [%s]", s1, s2, s3, info);
#endif

			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Always stop at "normal" keys */
			if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x')) return query;

			/* Sometimes stop at "space" key */
			if ((query == ' ') && !(mode & TARGET_LOOK)) return query;

			/* Change the intro */
#ifdef JP
			s1 = "それは";
#else
			s1 = "It is ";
#endif


			/* Preposition */
#ifdef JP
			s2 = "の上";
			s3 = "に見える";
#else
			s2 = "on ";
#endif
		}
	}


#ifdef ALLOW_EASY_FLOOR
	if (floor_num)
	{
		int min_width = 0;

		while (1)
		{
			if (floor_num == 1)
			{
				char o_name[MAX_NLEN];

				object_type *o_ptr;

				/* Acquire object */
				o_ptr = &o_list[floor_list[0]];

				/* Describe the object */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Message */
#ifdef JP
				sprintf(out_val, "%s%s%s%s[%s]",
					s1, o_name, s2, s3, info);
#else
				sprintf(out_val, "%s%s%s%s [%s]",
					s1, s2, s3, o_name, info);
#endif

				prt(out_val, 0, 0);
				move_cursor_relative(y, x);

				/* Command */
				query = inkey();

				/* End this grid */
				return query;
			}

			/* Provide one cushion before item listing  */
			if (boring)
			{
				/* Display rough information about items */
#ifdef JP
				sprintf(out_val, "%s %d個のアイテム%s%s ['x'で一覧, %s]",
					s1, floor_num, s2, s3, info);
#else
				sprintf(out_val, "%s%s%sa pile of %d items [x,%s]",
					s1, s2, s3, floor_num, info);
#endif

				prt(out_val, 0, 0);
				move_cursor_relative(y, x);

				/* Command */
				query = inkey();

				/* No request for listing */
				if (query != 'x' && query != ' ') return query;
			}


			/** Display list of items **/

			/* Continue scrolling list if requested */
			while (1)
			{
				int i, o_idx;
				cave_type *c_ptr;

				/* Save screen */
				screen_save();

				/* Display */
				(void)show_floor(0, y, x, &min_width);

				/* Prompt */
#ifdef JP
				sprintf(out_val, "%s %d個のアイテム%s%s [Enterで次へ, %s]",
					s1, floor_num, s2, s3, info);
#else
				sprintf(out_val, "%s%s%sa pile of %d items [Enter,%s]",
					s1, s2, s3, floor_num, info);
#endif
				prt(out_val, 0, 0);


				/* Wait */
				query = inkey();

				/* Load screen */
				screen_load();

				/* Exit unless 'Enter' */
				if (query != '\n' && query != '\r')
				{
					return query;
				}

				/* Get the object being moved. */
				c_ptr = &cave[y][x];
				o_idx =	c_ptr->o_idx;

				/* Only rotate a pile of two or more objects. */
				if (!(o_idx && o_list[o_idx].next_o_idx)) continue;

				/* Remove the first object from the list. */
				excise_object_idx(o_idx);

				/* Find end of the list. */
				i = c_ptr->o_idx;
				while (o_list[i].next_o_idx)
					i = o_list[i].next_o_idx;

				/* Add after the last object. */
				o_list[i].next_o_idx = o_idx;

				/* Loop and re-display the list */
			}
		}

		/* End this grid */
		/* return query; */
	}
#endif /* ALLOW_EASY_FLOOR */


	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Describe it */
		if (o_ptr->marked)
		{
			char o_name[MAX_NLEN];

			/* Not boring */
			boring = FALSE;

			/* Obtain an object description */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Describe the object */
#ifdef JP
			sprintf(out_val, "%s%s%s%s[%s]", s1, o_name, s2, s3, info);
#else
			sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
#endif

			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Always stop at "normal" keys */
			if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x')) return query;

			/* Sometimes stop at "space" key */
			if ((query == ' ') && !(mode & TARGET_LOOK)) return query;

			/* Change the intro */
#ifdef JP
			s1 = "それは";
#else
			s1 = "It is ";
#endif


			/* Plurals */
#ifdef JP
			if (o_ptr->number != 1) s1 = "それらは";
#else
			if (o_ptr->number != 1) s1 = "They are ";
#endif


			/* Preposition */
#ifdef JP
			s2 = "の上";
			s3 = "に見える";
#else
			s2 = "on ";
#endif

		}
	}


	/* Feature code (applying "mimic" field) */
	feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

	/* Require knowledge about grid, or ability to see grid */
	if (!(c_ptr->info & CAVE_MARK) && !player_can_see_bold(y, x))
	{
		/* Forget feature */
		feat = FEAT_NONE;
	}

	/* Terrain feature if needed */
	if (boring || (feat > FEAT_INVIS))
	{
		cptr name;

		/* Hack -- special handling for building doors */
		if ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL))
		{
			name = building[feat - FEAT_BLDG_HEAD].name;
		}
		else if ((feat == FEAT_ENTRANCE) || (feat == FEAT_ENTRANCE_UPWARD))
		{
#ifdef JP
			name = format("%s(%d階相当)", d_text + d_info[c_ptr->special].text, d_info[c_ptr->special].mindepth);
#else
			name = format("%s(level %d)", d_text + d_info[c_ptr->special].text, d_info[c_ptr->special].mindepth);
#endif
		}
		else if (feat == FEAT_TOWN)
		{
			name = town[c_ptr->special].name;
		}
		/* Hack -- special handling for quest entrances */
		else if (feat == FEAT_QUEST_ENTER)
		{
			/* Set the quest number temporary */
			int old_quest = p_ptr->inside_quest;
			p_ptr->inside_quest = c_ptr->special;

			/* Get the quest text */
			init_flags = INIT_SHOW_TEXT;
			quest_text_line = 0;

			process_dungeon_file("q_info.txt", 0, 0, 0, 0);

			name = quest[c_ptr->special].name;

			/* Reset the old quest number */
			p_ptr->inside_quest = old_quest;
		}
		else if (p_ptr->wild_mode && (feat == FEAT_FLOOR))
		{
#ifdef JP
			name = "道";
#else
			name = "road";
#endif
		}
		else
		{
			name = f_name + f_info[feat].name;
		}


		/* Pick a prefix */
		if (*s2 && (((feat >= FEAT_DOOR_HEAD) &&
		             (feat <= FEAT_PERM_SOLID)) ||
		             (feat == FEAT_TOWN)))
		{
#ifdef JP
			s2 = "の中";
#else
			s2 = "in ";
#endif

		}

		/* Hack -- special introduction for store & building doors -KMW- */
		if (((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL)) ||
		    ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL)) ||
		    (feat == FEAT_MUSEUM) ||
		    (feat == FEAT_ENTRANCE) ||
		    (feat == FEAT_ENTRANCE_UPWARD))
		{
#ifdef JP
			s2 = "の入口";
#else
			s3 = "";
#endif

		}
		else if (feat == FEAT_QUEST_ENTER)
		{
#ifdef JP
			s3 = "のクエスト入口";
#else
			s3 = "the quest-entrance to the ";
#endif

		}
		else if ((feat == FEAT_TOWN) || (feat == FEAT_FLOOR) || (feat == FEAT_DIRT) ||
		         (feat == FEAT_FLOWER) || (feat == FEAT_SWAMP) || (feat == FEAT_TUNDRA))
		{
#ifndef JP
			s3 ="";
#endif
		}
		else
		{
			/* Pick proper indefinite article */
#ifndef JP
			s3 = (is_a_vowel(name[0])) ? "an " : "a ";
#endif
		}

		/* Display a message */
#ifdef JP
		sprintf(out_val, "%s%s", s1, name);
#else
		sprintf(out_val, "%s%s%s%s", s1, s2, s3, name);
#endif
		prt(out_val, 0, 0);

		tmp_len = strlen(out_val);
		tmp_len += prt_grid_elem(c_ptr, feat, tmp_len);

		if (p_ptr->wizard)
#ifdef JP
			sprintf(out_val, "%s%s[%s] %x %d %d %d %d %d %d %d (%d,%d)", s2, s3, info,
#else
			sprintf(out_val, " [%s] %x %d %d %d %d %d %d %d (%d,%d)", info,
#endif
			        c_ptr->info, c_ptr->feat, c_ptr->dist, c_ptr->cost, c_ptr->when,
			        c_ptr->ddist, c_ptr->dcost, c_ptr->dwhen, x, y);
		else
#ifdef JP
			sprintf(out_val, "%s%s[%s]", s2, s3, info);
#else
			sprintf(out_val, " [%s]", info);
#endif
		prt(out_val, 0, tmp_len);

		move_cursor_relative(y, x);
		query = inkey();

		/* Always stop at "normal" keys */
		if ((query != '\r') && (query != '\n') && (query != ' ')) return query;
	}

	/* Stop on everything but "return" */
	if ((query != '\r') && (query != '\n')) return query;

	/* Repeat forever */
	return 0;
}


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * All locations must be on the current panel.  Consider the use of
 * "panel_bounds()" to allow "off-panel" targets, perhaps by using
 * some form of "scrolling" the map around the cursor.  XXX XXX XXX
 * That is, consider the possibility of "auto-scrolling" the screen
 * while the cursor moves around.  This may require changes in the
 * "update_mon()" code to allow "visibility" even if off panel, and
 * may require dynamic recalculation of the "temp" grid set.
 *
 * Hack -- targeting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 */
bool target_set(int mode)
{
	int		i, d, m, t, bd;
	int		y = py;
	int		x = px;

	bool	done = FALSE;

	bool	flag = TRUE;

	char	query;

	char	info[80];

	cave_type		*c_ptr;

	int wid, hgt;

	/* Get size */
	get_screen_size(&wid, &hgt);

	/* Cancel target */
	target_who = 0;


	/* Cancel tracking */
	/* health_track(0); */


	/* Prepare the "temp" array */
	target_set_prepare(mode);

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		/* Interesting grids */
		if (flag && temp_n)
		{
			y = temp_y[m];
			x = temp_x[m];

			if (!(mode & TARGET_LOOK)) prt_path(y, x);

			/* Access */
			c_ptr = &cave[y][x];

			/* Allow target */
			if (target_able(c_ptr->m_idx))
			{
#ifdef JP
strcpy(info, "q止 t決 p自 o現 +次 -前");
#else
				strcpy(info, "q,t,p,o,+,-,<dir>");
#endif

			}

			/* Dis-allow target */
			else
			{
#ifdef JP
strcpy(info, "q止 p自 o現 +次 -前");
#else
				strcpy(info, "q,p,o,+,-,<dir>");
#endif

			}

			/* Describe and Prompt */
			while (!(query = target_set_aux(y, x, mode, info))) /* loop */;

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no "direction" */
			d = 0;

			if (use_menu)
			{
				if (query == '\r') query = 't';
			}  

			/* Analyze */
			switch (query)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case 't':
				case '.':
				case '5':
				case '0':
				{
					if (target_able(c_ptr->m_idx))
					{
						health_track(c_ptr->m_idx);
						target_who = c_ptr->m_idx;
						target_row = y;
						target_col = x;
						done = TRUE;
					}
					else
					{
						bell();
					}
					break;
				}

				case ' ':
				case '*':
				case '+':
				{
					if (++m == temp_n)
					{
						m = 0;
						if (!expand_list) done = TRUE;
					}
					break;
				}

				case '-':
				{
					if (m-- == 0)
					{
						m = temp_n - 1;
						if (!expand_list) done = TRUE;
					}
					break;
				}

				case 'p':
				{
					/* Recenter the map around the player */
					verify_panel();

					/* Update stuff */
					p_ptr->update |= (PU_MONSTERS);

					/* Redraw map */
					p_ptr->redraw |= (PR_MAP);

					/* Window stuff */
					p_ptr->window |= (PW_OVERHEAD);

					/* Handle stuff */
					handle_stuff();

					/* Recalculate interesting grids */
					target_set_prepare(mode);

					y = py;
					x = px;
				}

				case 'o':
				{
					flag = FALSE;
					break;
				}

				case 'm':
				{
					break;
				}

				default:
				{
					/* Extract the action (if any) */
					d = get_keymap_dir(query);

					if (!d) bell();
					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				/* Modified to scroll to monster */
				int y2 = panel_row_min;
				int x2 = panel_col_min;

				/* Find a new monster */
				i = target_pick(temp_y[m], temp_x[m], ddy[d], ddx[d]);

				/* Request to target past last interesting grid */
				while (flag && (i < 0))
				{
					/* Note the change */
					if (change_panel(ddy[d], ddx[d]))
					{
						int v = temp_y[m];
						int u = temp_x[m];

						/* Recalculate interesting grids */
						target_set_prepare(mode);

						/* Look at interesting grids */
						flag = TRUE;

						/* Find a new monster */
						i = target_pick(v, u, ddy[d], ddx[d]);

						/* Use that grid */
						if (i >= 0) m = i;
					}

					/* Nothing interesting */
					else
					{
						int dx = ddx[d];
						int dy = ddy[d];

						/* Restore previous position */
						panel_row_min = y2;
						panel_col_min = x2;
						panel_bounds_center();

						/* Update stuff */
						p_ptr->update |= (PU_MONSTERS);

						/* Redraw map */
						p_ptr->redraw |= (PR_MAP);

						/* Window stuff */
						p_ptr->window |= (PW_OVERHEAD);

						/* Handle stuff */
						handle_stuff();

						/* Recalculate interesting grids */
						target_set_prepare(mode);

						/* Look at boring grids */
						flag = FALSE;

						/* Move */
						x += dx;
						y += dy;

						/* Do not move horizontally if unnecessary */
						if (((x < panel_col_min + wid / 2) && (dx > 0)) ||
							 ((x > panel_col_min + wid / 2) && (dx < 0)))
						{
							dx = 0;
						}

						/* Do not move vertically if unnecessary */
						if (((y < panel_row_min + hgt / 2) && (dy > 0)) ||
							 ((y > panel_row_min + hgt / 2) && (dy < 0)))
						{
							dy = 0;
						}

						/* Apply the motion */
						if ((y >= panel_row_min+hgt) || (y < panel_row_min) ||
						    (x >= panel_col_min+wid) || (x < panel_col_min))
						{
							if (change_panel(dy, dx)) target_set_prepare(mode);
						}

						/* Slide into legality */
						if (x >= cur_wid-1) x = cur_wid - 2;
						else if (x <= 0) x = 1;

						/* Slide into legality */
						if (y >= cur_hgt-1) y = cur_hgt- 2;
						else if (y <= 0) y = 1;
					}
				}

				/* Use that grid */
				m = i;
			}
		}

		/* Arbitrary grids */
		else
		{
			bool move_fast = FALSE;

			if (!(mode & TARGET_LOOK)) prt_path(y, x);

			/* Access */
			c_ptr = &cave[y][x];

			/* Default prompt */
#ifdef JP
strcpy(info, "q止 t決 p自 m近 +次 -前");
#else
			strcpy(info, "q,t,p,m,+,-,<dir>");
#endif


			/* Describe and Prompt (enable "TARGET_LOOK") */
			while (!(query = target_set_aux(y, x, mode | TARGET_LOOK, info))) /* loop */;

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no direction */
			d = 0;

			if (use_menu)
			{
				if (query == '\r') query = 't';
			}

			/* Analyze the keypress */
			switch (query)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case 't':
				case '.':
				case '5':
				case '0':
				{
					target_who = -1;
					target_row = y;
					target_col = x;
					done = TRUE;
					break;
				}

				case 'p':
				{
					/* Recenter the map around the player */
					verify_panel();

					/* Update stuff */
					p_ptr->update |= (PU_MONSTERS);

					/* Redraw map */
					p_ptr->redraw |= (PR_MAP);

					/* Window stuff */
					p_ptr->window |= (PW_OVERHEAD);

					/* Handle stuff */
					handle_stuff();

					/* Recalculate interesting grids */
					target_set_prepare(mode);

					y = py;
					x = px;
				}

				case 'o':
				{
					break;
				}

				case ' ':
				case '*':
				case '+':
				case '-':
				case 'm':
				{
					flag = TRUE;

					m = 0;
					bd = 999;

					/* Pick a nearby monster */
					for (i = 0; i < temp_n; i++)
					{
						t = distance(y, x, temp_y[i], temp_x[i]);

						/* Pick closest */
						if (t < bd)
						{
							m = i;
							bd = t;
						}
					}

					/* Nothing interesting */
					if (bd == 999) flag = FALSE;

					break;
				}

				default:
				{
					/* Extract the action (if any) */
					d = get_keymap_dir(query);

					/* XTRA HACK MOVEFAST */
					if (isupper(query)) move_fast = TRUE;

					if (!d) bell();
					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				int dx = ddx[d];
				int dy = ddy[d];

				/* XTRA HACK MOVEFAST */
				if (move_fast)
				{
					int mag = MIN(wid / 2, hgt / 2);
					x += dx * mag;
					y += dy * mag;
				}
				else
				{
					x += dx;
					y += dy;
				}

				/* Do not move horizontally if unnecessary */
				if (((x < panel_col_min + wid / 2) && (dx > 0)) ||
					 ((x > panel_col_min + wid / 2) && (dx < 0)))
				{
					dx = 0;
				}

				/* Do not move vertically if unnecessary */
				if (((y < panel_row_min + hgt / 2) && (dy > 0)) ||
					 ((y > panel_row_min + hgt / 2) && (dy < 0)))
				{
					dy = 0;
				}

				/* Apply the motion */
				if ((y >= panel_row_min + hgt) || (y < panel_row_min) ||
					 (x >= panel_col_min + wid) || (x < panel_col_min))
				{
					if (change_panel(dy, dx)) target_set_prepare(mode);
				}

				/* Slide into legality */
				if (x >= cur_wid-1) x = cur_wid - 2;
				else if (x <= 0) x = 1;

				/* Slide into legality */
				if (y >= cur_hgt-1) y = cur_hgt- 2;
				else if (y <= 0) y = 1;
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();

	/* Failure to set target */
	if (!target_who) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Get an "aiming direction" from the user.
 *
 * The "dir" is loaded with 1,2,3,4,6,7,8,9 for "actual direction", and
 * "0" for "current target", and "-1" for "entry aborted".
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Note that confusion over-rides any (explicit?) user choice.
 */
bool get_aim_dir(int *dp)
{
	int		dir;

	char	command;

	cptr	p;

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = command_dir;

	/* Hack -- auto-target if requested */
	if (use_old_target && target_okay()) dir = 5;

#ifdef ALLOW_REPEAT /* TNB */

	if (repeat_pull(dp))
	{
		/* Confusion? */

		/* Verify */
		if (!(*dp == 5 && !target_okay()))
		{
/*			return (TRUE); */
			dir = *dp;
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
#ifdef JP
p = "方向 ('*'でターゲット選択, ESCで中断)? ";
#else
			p = "Direction ('*' to choose a target, Escape to cancel)? ";
#endif

		}
		else
		{
#ifdef JP
p = "方向 ('5'でターゲットへ, '*'でターゲット再選択, ESCで中断)? ";
#else
			p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";
#endif

		}

		/* Get a command (or Cancel) */
		if (!get_com(p, &command, TRUE)) break;

		if (use_menu)
		{
			if (command == '\r') command = 't';
		}  

		/* Convert various keys to "standard" keys */
		switch (command)
		{
			/* Use current target */
			case 'T':
			case 't':
			case '.':
			case '5':
			case '0':
			{
				dir = 5;
				break;
			}

			/* Set new target */
			case '*':
			case ' ':
			case '\r':
			{
				if (target_set(TARGET_KILL)) dir = 5;
				break;
			}

			default:
			{
				/* Extract the action (if any) */
				dir = get_keymap_dir(command);

				break;
			}
		}

		/* Verify requested targets */
		if ((dir == 5) && !target_okay()) dir = 0;

		/* Error */
		if (!dir) bell();
	}

	/* No direction */
	if (!dir)
	{
		project_length = 0; /* reset to default */
		return (FALSE);
	}

	/* Save the direction */
	command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused || ((fool_effect_status & FOOL_STATUS_PLAYER) && one_in_(16)))
	{
		/* XXX XXX XXX */
		/* Random direction */
		dir = ddd[randint0(8)];
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
#ifdef JP
		if (p_ptr->confused) msg_print("あなたは混乱している。");
#else
		if (p_ptr->confused) msg_print("You are confused.");
#endif
		else msg_print("あなたは変な方向に歩いている。");

	}

	/* Save direction */
	(*dp) = dir;

#ifdef ALLOW_REPEAT /* TNB */

/*	repeat_push(dir); */
	repeat_push(command_dir);

#endif /* ALLOW_REPEAT -- TNB */

	/* A "valid" direction was entered */
	return (TRUE);
}



/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user,
 * and place it into "command_dir", unless we already have one.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player.  Note that,
 * for example, it is no longer possible to "disarm" or "open" chests
 * in the same grid as the player.
 *
 * Direction "5" is illegal and will (cleanly) abort the command.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", to which "confusion" is applied.
 */
bool get_rep_dir(int *dp, bool under)
{
	int dir;

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = command_dir;

#ifdef ALLOW_REPEAT /* TNB */

	if (repeat_pull(dp))
	{
		dir = *dp;
/*		return (TRUE); */
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Get a direction */
	while (!dir)
	{
		char ch;

		/* Get a command (or Cancel) */
#ifdef JP
if (!get_com("方向 (ESCで中断)? ", &ch, TRUE)) break;
#else
		if (!get_com("Direction (Escape to cancel)? ", &ch, TRUE)) break;
#endif


		/* Look up the direction */
		dir = get_keymap_dir(ch);

		/* Oops */
		if (!dir) bell();
	}

	/* Prevent weirdness */
	if ((dir == 5) && (!under)) dir = 0;

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	command_dir = dir;

	/* Apply "confusion" */
	if (p_ptr->confused || ((fool_effect_status & FOOL_STATUS_PLAYER) && one_in_(16)))
	{
		/* Standard confusion */
		if (randint0(100) < 75)
		{
			/* Random direction */
			dir = ddd[randint0(8)];
		}
	}
	else if (p_ptr->riding)
	{
		monster_type *m_ptr = &m_list[p_ptr->riding];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		int rf1_rand = 0;

		if (r_ptr->flags1 & RF1_RAND_50) rf1_rand += 50;
		if (r_ptr->flags1 & RF1_RAND_25) rf1_rand += 25;
		if (fool_effect_status & FOOL_STATUS_MONSTERS) rf1_rand += 25;
		rf1_rand -= 25;
		if (rf1_rand < 0) rf1_rand = 0;

		if (m_ptr->confused)
		{
			/* Standard confusion */
			if (randint0(100) < 75)
			{
				/* Random direction */
				dir = ddd[randint0(8)];
			}
		}
		else if (randint0(100) < rf1_rand)
		{
			/* Random direction */
			dir = ddd[randint0(8)];
		}
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		if (!p_ptr->riding)
		{
			/* Warn the user */
#ifdef JP
			if (p_ptr->confused) msg_print("あなたは混乱している。");
#else
			if (p_ptr->confused) msg_print("You are confused.");
#endif
			else msg_print("あなたは変な方向に歩いている。");
		}
		else
		{
			char m_name[80];
			monster_type *m_ptr = &m_list[p_ptr->riding];

			monster_desc(m_name, m_ptr, 0);
			if (m_ptr->confused)
			{
#ifdef JP
				msg_format("%sは混乱している。", m_name);
#else
				msg_format("%^s is confusing.", m_name);
#endif
			}
			else
			{
#ifdef JP
				msg_format("%sは思い通りに動いてくれない。", m_name);
#else
				msg_format("You cannot control %s.", m_name);
#endif
			}
		}
	}

	/* Save direction */
	(*dp) = dir;

#ifdef ALLOW_REPEAT /* TNB */

/*	repeat_push(dir); */
	repeat_push(command_dir);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


bool get_rep_dir2(int *dp)
{
	int dir;

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = command_dir;

#ifdef ALLOW_REPEAT /* TNB */

	if (repeat_pull(dp))
	{
		dir = *dp;
/*		return (TRUE); */
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Get a direction */
	while (!dir)
	{
		char ch;

		/* Get a command (or Cancel) */
#ifdef JP
if (!get_com("方向 (ESCで中断)? ", &ch, TRUE)) break;
#else
		if (!get_com("Direction (Escape to cancel)? ", &ch, TRUE)) break;
#endif


		/* Look up the direction */
		dir = get_keymap_dir(ch);

		/* Oops */
		if (!dir) bell();
	}

	/* Prevent weirdness */
	if (dir == 5) dir = 0;

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	command_dir = dir;

	/* Apply "confusion" */
	if (p_ptr->confused || ((fool_effect_status & FOOL_STATUS_PLAYER) && one_in_(16)))
	{
		/* Standard confusion */
		if (randint0(100) < 75)
		{
			/* Random direction */
			dir = ddd[randint0(8)];
		}
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
#ifdef JP
msg_print("あなたは混乱している。");
#else
		msg_print("You are confused.");
#endif

	}

	/* Save direction */
	(*dp) = dir;

#ifdef ALLOW_REPEAT /* TNB */

/*	repeat_push(dir); */
	repeat_push(command_dir);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


bool activate_tarot_power(int effect)
{
	int i;
	int s_num_6 = (easy_band ? 2 : 6);
	int summon_lev = MAX(p_ptr->lev * 2 / 3 + randint1(p_ptr->lev / 2), dun_level);
	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

	switch (effect)
	{
	case 0: /* The Blank Card */
		if (p_ptr->muta2 & MUT2_TAROT)
		{
#ifdef JP
			if (!get_check("タロットカードの力を感じるのをやめますか？ ")) return FALSE;
#else
			if (!get_check("Do you stop feeling the power of tarot cards? ")) return FALSE;
#endif
			/* Lose the mutation (Power of Tarot Cards) */
			lose_mutation(110);
		}
		else
		{
#ifdef JP
			if (!get_check("タロットカードの力を感じられる... 続けますか？ ")) return FALSE;
#else
			if (!get_check("You can feel the power of tarot cards... continue? ")) return FALSE;
#endif
			/* Gain the mutation (Power of Tarot Cards) */
			gain_random_mutation(110, FALSE);
		}
		break;

	case 1: /* The Magician */
		(void)do_inc_stat(A_INT);
		break;

	case 2: /* Reverted position of the Magician */
		if (dec_stat(A_INT, 10, TRUE))
		{
#ifdef JP
			msg_print("ひどく無知になった気がする。");
#else
			msg_print("You feel very stupid.");
#endif
		}
		break;

	case 3: /* The High Priestess */
#ifdef JP
		msg_print("体中に生命力が満ちあふれてきた！");
#else
		msg_print("You feel life flow through your body!");
#endif

		restore_level();
		(void)set_poisoned(0);
		(void)set_blind(0);
		(void)set_confused(0);
		(void)set_image(0);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_stoning(0);
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_CHR);
		(void)set_shero(0,TRUE);
		update_stuff();
		hp_player(p_ptr->mhp);
		break;

	case 4: /* Reverted position of the High Priestess */
#ifdef JP
		msg_print("体中の生命力が弱っていく...");
#else
		msg_print("You feel life flow weakened...");
#endif

		i = randint0(20) + 10;
		if (!p_ptr->hold_life && (p_ptr->exp > 0))
		{
			s32b ee;

			ee = (cexp_ptr->cexp / 2) + 10;
			if (ee > 100000L) ee = 100000L;
			lose_class_exp(ee);

			ee = (p_ptr->exp / 2) + 10;
			if (ee > 100000L) ee = 100000L;
			lose_racial_exp(ee);
		}
		if (!p_ptr->resist_pois && !p_ptr->oppose_pois) (void)set_poisoned(p_ptr->poisoned + i);
		if (!p_ptr->resist_blind) (void)set_blind(p_ptr->blind + i);
		if (!p_ptr->resist_conf) (void)set_confused(p_ptr->confused + i);
		if (!p_ptr->resist_chaos) (void)set_image(p_ptr->image + i);
		if (!p_ptr->resist_sound) (void)set_stun(p_ptr->stun + i);
		if (!p_ptr->resist_shard) (void)set_cut(p_ptr->cut + i);
		if (!p_ptr->resist_stone) (void)set_stoning(1);
		(void)do_dec_stat(A_DEX);
		(void)do_dec_stat(A_WIS);
		(void)do_dec_stat(A_CON);
		(void)do_dec_stat(A_STR);
		(void)do_dec_stat(A_CHR);
		(void)do_dec_stat(A_INT);
		update_stuff();
		take_hit(DAMAGE_LOSELIFE, p_ptr->chp / 2, tarot_info[effect].name);
		break;

	case 5: /* The Empress */
		(void)do_inc_stat(A_CHR);
		break;

	case 6: /* Reverted position of the Empress */
		if (dec_stat(A_CHR, 10, TRUE))
		{
#ifdef JP
			msg_print("ひどく醜くなった気がする。");
#else
			msg_print("You feel very ugly.");
#endif
		}
		break;

	case 7: /* The Emperor */
		(void)set_tim_inc_blow(randint1(40) + 40, FALSE);
		break;

	case 8: /* Reverted position of the Emperor */
		(void)set_tim_dec_blow(randint1(40) + 40, FALSE);
		break;

	case 9: /* The Hierophant */
		msg_print("あらゆる民の支持を得た気がする。");
		for (i = 0; i < ETHNICITY_NUM; i++) change_chaos_frame(i, 50);
		break;

	case 10: /* Reverted position of the Hierophant */
		msg_print("あらゆる民の支持を失った気がする。");
		for (i = 0; i < ETHNICITY_NUM; i++) change_chaos_frame(i, -50);
		break;

	case 11: /* The Lovers */
		{
			char m_name[80];
			monster_type *m_ptr;
			monster_race *r_ptr;

			for (i = 1; i < m_max; i++)
			{
				m_ptr = &m_list[i];

				/* Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				/* Require line of sight */
				if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;

				r_ptr = &r_info[m_ptr->r_idx];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				if ((r_ptr->flags1 & RF1_QUESTOR) || p_ptr->inside_arena)
				{
					if (m_ptr->ml)
#ifdef JP
						msg_format("%sには効果がなかった！", m_name);
#else
						msg_format("%s is unaffected!", m_name);
#endif
				}
				else
				{
					if (m_ptr->ml)
#ifdef JP
						msg_format("%sは突然友好的になったようだ！", m_name);
#else
						msg_format("%s suddenly seems friendly!", m_name);
#endif
					set_pet(m_ptr);
				}

				/* Wake the monster up */
				m_ptr->csleep = 0;

				/* Redraw (later) if needed */
				if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
				if (p_ptr->riding == i) p_ptr->redraw |= (PR_UHEALTH);
			}
		}
		break;

	case 12: /* Reverted position of the Lovers */
#ifdef JP
		msg_print("フロア中の全てのモンスターが目覚めた。");
#else
		msg_print("All monsters in this floor wake up...");
#endif
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];

			/* Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* Wake up */
			m_ptr->csleep = 0;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
			if (p_ptr->riding == i) p_ptr->redraw |= (PR_UHEALTH);
		}
		break;

	case 13: /* The Chariot */
		{
			bool arrived = FALSE;

			if (!summon_specific(-1, py, px, summon_lev, SUMMON_RIDING_UNIQUE, PM_ALLOW_UNIQUE | PM_FORCE_PET))
			{
				if (summon_specific(-1, py, px, summon_lev, SUMMON_RIDING, PM_ALLOW_UNIQUE | PM_FORCE_PET))
					arrived = TRUE;
			}
			else arrived = TRUE;

			if (arrived)
			{
				monster_race *r_ptr = &r_info[m_list[hack_m_idx_ii].r_idx];
				bool is_mighty = ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_UNIQUE2));

				msg_format("あなたの戦車として仕えるべく%sモンスターが現れた。", is_mighty ? "強力な" : "");
			}
			else
			{
#ifdef JP
				msg_print("何も現れなかった。");
#else
				msg_print("No one arrives.");
#endif
			}
		}
		break;

	case 14: /* Reverted position of the Chariot */
		{
			bool arrived = FALSE;

			if (!summon_specific(0, py, px, summon_lev, SUMMON_RIDING_UNIQUE, PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID))
			{
				if (summon_specific(0, py, px, summon_lev, SUMMON_RIDING, PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID))
					arrived = TRUE;
			}
			else arrived = TRUE;

			if (arrived)
			{
				monster_race *r_ptr = &r_info[m_list[hack_m_idx_ii].r_idx];
				bool is_mighty = ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_UNIQUE2));

				msg_format("%sモンスターが現れた。あなたを蹴散らすために！", is_mighty ? "強力な" : "");
			}
			else
			{
#ifdef JP
				msg_print("何も現れなかった。");
#else
				msg_print("No one arrives.");
#endif
			}
		}
		break;

	case 15: /* The Strength */
		(void)do_inc_stat(A_STR);
		break;

	case 16: /* Reverted position of the Strength */
		if (dec_stat(A_STR, 10, TRUE))
		{
#ifdef JP
			msg_print("ひどく弱くなった気がする。");
#else
			msg_print("You feel very weak.");
#endif
		}
		break;

	case 17: /* The Hermit */
		(void)do_inc_stat(A_WIS);
		break;

	case 18: /* Reverted position of the Hermit */
		if (dec_stat(A_WIS, 10, TRUE))
		{
#ifdef JP
			msg_print("ひどく愚かになった気がする。");
#else
			msg_print("You feel very naive.");
#endif
		}
		break;

	case 19: /* The Wheel of Fortune */
		alter_reality();
		break;

	case 20: /* Reverted position of the Wheel of Fortune */
		do_poly_self();
		break;

	case 21: /* The Justice */
		msg_print("あふれる正義感を感じる。");
		change_your_alignment_lnc(100);
		break;

	case 22: /* Reverted position of the Justice */
		msg_print("無秩序な気分になった。");
		change_your_alignment_lnc(-100);
		break;

	case 23: /* The Hanged Man */
		{
			int count = 0;
			(void)activate_ty_curse(FALSE, &count);
		}
		break;

	case 24: /* Reverted position of the Hanged Man */
		msg_print("体の動きがわずかに鈍くなった。");
		p_ptr->gx_spd -= 50;
		p_ptr->update |= (PU_BONUS);
		break;

	case 25: /* The Death */
		{
			int dam;
			bool fear;
			char m_name[80];

#ifdef JP
			msg_print("死神は苦痛の言葉を投げかけた。");
#else
			msg_print("The Death throws the Word of Pain.");
#endif
			if (p_ptr->chp > (p_ptr->mhp / 5))
			{
				take_hit(DAMAGE_LOSELIFE, p_ptr->chp - (p_ptr->mhp / 5), tarot_info[effect].name);
			}
			dam = p_ptr->mhp - p_ptr->chp;

			for (i = 1; i < m_max; i++)
			{
				monster_type *m_ptr = &m_list[i];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				fear = FALSE;
				mon_take_hit_mon(FALSE, i, dam, &fear, extract_note_dies(r_ptr), -1);

				/* Take note */
				if (fear && m_ptr->ml)
				{
					monster_desc(m_name, m_ptr, 0);

					/* Sound */
					sound(SOUND_FLEE);

					/* Message */
#ifdef JP
					msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
					msg_format("%^s flees in terror!", m_name);
#endif
				}
			}
		}
		break;

	case 26: /* Reverted position of the Death */
#ifdef JP
		if (!get_check("レベル1のキャラクタに転生します。よろしいですか？ ")) return FALSE;
#else
		if (!get_check("Reincarnate as level 1 character. Are you sure? ")) return FALSE;
#endif
		reincarnation();
		break;

	case 27: /* The Temperance */
		if (p_ptr->exp < PY_MAX_EXP)
		{
			s32b ee;

#ifdef JP
			msg_print("更に経験を積んだような気がする。");
#else
			msg_print("You feel more experienced.");
#endif
			/* Class */
			ee = (cexp_ptr->cexp / 2) + 10;
			if (ee > 100000L) ee = 100000L;
			gain_class_exp(ee);

			/* Racial */
			ee = (p_ptr->exp / 2) + 10;
			if (ee > 100000L) ee = 100000L;
			gain_racial_exp(ee);
		}
		break;

	case 28: /* Reverted position of the Temperance */
		if (p_ptr->exp > 0)
		{
			s32b ee;

#ifdef JP
			msg_print("経験が失われていく気がする。");
#else
			msg_print("You feel less experienced.");
#endif
			/* Class */
			ee = (cexp_ptr->cexp / 2) + 10;
			if (ee > 100000L) ee = 100000L;
			cexp_ptr->cexp -= ee;
			if (cexp_ptr->cexp < 0) cexp_ptr->cexp = 0;
			cexp_ptr->max_cexp -= ee;
			if (cexp_ptr->max_cexp < 0) cexp_ptr->max_cexp = 0;

			/* Racial */
			ee = (p_ptr->exp / 2) + 10;
			if (ee > 100000L) ee = 100000L;
			p_ptr->exp -= ee;
			if (p_ptr->exp < 0) p_ptr->exp = 0;
			p_ptr->max_exp -= ee;
			if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

			check_experience();
		}
		break;

	case 29: /* The Devil */
		{
			u32b mode = (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE);
			bool pet = one_in_(15);
			int count = 0;

			if (pet) mode |= (PM_FORCE_PET);
			else mode |= (PM_NO_PET | PM_IGNORE_AMGRID);

			for (i = 0; i < s_num_6; i++)
			{
				count += summon_specific((pet ? -1 : 0), py, px, summon_lev, SUMMON_DEMON, mode);
			}
			if (!count)
			{
#ifdef JP
				msg_print("悪魔は現れなかった。");
#else
				msg_print("No demons arrive.");
#endif
			}
		}
		break;

	case 30: /* Reverted position of the Devil */
		{
			monster_type *m_ptr;
			u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
			int y, x;

			for (i = 1; i < m_max; i++)
			{
				m_ptr = &m_list[i];

				/* Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				/* Location */
				y = m_ptr->fy;
				x = m_ptr->fx;

				/* Require line of sight */
				if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;

				/* Only affect demons */
				if (!(r_info[m_ptr->r_idx].flags3 & RF3_DEMON)) continue;

				/* Jump directly to the target monster */
				project(0, 0, y, x, 300, GF_DISP_ALL, flg, MODIFY_ELEM_MODE_MAGIC);
			}
		}
		break;

	case 31: /* The Tower */
		if (destroy_area(py, px, 127))
#ifdef JP
			msg_print("ダンジョンが崩壊した...");
#else
			msg_print("The dungeon collapses...");
#endif
		else
#ifdef JP
			msg_print("ダンジョンは大きく揺れた。");
#else
			msg_print("The dungeon trembles.");
#endif

		take_hit(DAMAGE_NOESCAPE, 250, tarot_info[effect].name);
		break;

	case 32: /* Reverted position of the Tower */
		destroy_area(py, px, 13+randint0(5));
		break;

	case 33: /* The Star */
		acquirement(py, px, randint1(2) + 1, TRUE, FALSE);
		break;

	case 34: /* Reverted position of the Star */
		curse_weapon(FALSE, INVEN_RARM);
		break;

	case 35: /* The Moon */
		msg_print("新たなモンスターが周囲に現れた気がする。");
		for (i = 0; i < 10; i++)
			(void)alloc_monster(5, 0);
		break;

	case 36: /* Reverted position of the Moon */
		{
			monster_type *m_ptr;
			s16b *delete_array;
			int delete_num = 0, cur_delete;

			C_MAKE(delete_array, m_max, s16b);

			for (i = 1; i < m_max; i++)
			{
				m_ptr = &m_list[i];

				if (!m_ptr->r_idx) continue;

				if (m_ptr->cdis < 5) continue;
				if (r_info[m_ptr->r_idx].flags1 & (RF1_UNIQUE | RF1_QUESTOR)) continue;

				delete_array[delete_num++] = i;
			}

			if (delete_num)
				msg_print("周囲のモンスターの数がいくらか減った気がする。");

			for (i = 0; (i < 10) && delete_num; i++)
			{
				cur_delete = randint0(delete_num);

				/* Check for quest completion */
				check_quest_completion(&m_list[delete_array[cur_delete]]);

				/* Delete the monster */
				delete_monster_idx(delete_array[cur_delete]);

				/* Pack the list */
				if (cur_delete < (delete_num - 1))
					delete_array[cur_delete] = delete_array[delete_num - 1];
				delete_num--;
			}

			C_KILL(delete_array, m_max, s16b);
		}
		break;

	case 37: /* The Sun */
		wiz_lite(FALSE);
		detect_all(DETECT_RAD_ALL * 3);
		break;

	case 38: /* Reverted position of the Sun */
		wiz_dark(TRUE);
		break;

	case 39: /* The Judgement */
		{
			char m_name[80];
			monster_type *m_ptr;
			monster_race *r_ptr;

#ifdef JP
			msg_print("裁きの刻は来た...");
#else
			msg_print("Time of Judgement is right now...");
#endif

			for (i = 1; i < m_max; i++)
			{
				m_ptr = &m_list[i];

				/* Paranoia -- Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				r_ptr = &r_info[m_ptr->r_idx];

				if ((r_ptr->flags3 & RF3_GOOD) && (r_ptr->flags7 & RF7_LAWFUL)) continue;
				if (is_pet(m_ptr)) continue;

				if ((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & RF7_UNIQUE2))
				{
					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					if (m_ptr->ml)
#ifdef JP
						msg_format("%sは裁きに抵抗した。", m_name);
#else
						msg_format("%s resists the Judgement.", m_name);
#endif
					m_ptr->csleep = 0;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
					if (p_ptr->riding == i) p_ptr->redraw |= (PR_UHEALTH);
				}
				else
				{
					/* Check for quest completion */
					check_quest_completion(m_ptr);

					/* Delete the monster */
					delete_monster_idx(i);
				}
			}
		}
		break;

	case 40: /* Reverted position of the Judgement */
		{
			int count = 0;

			for (i = 0; i < s_num_6; i++)
			{
				count += summon_specific(0, py, px, summon_lev, SUMMON_GOOD_LAW, PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID);
			}
			if (count)
			{
#ifdef JP
				msg_print("あなたを裁くために秩序の使者が現れた！");
#else
				msg_print("The agents of law arrive for judgement!");
#endif
			}
			else
			{
#ifdef JP
				msg_print("何も現れなかった。");
#else
				msg_print("No one arrives.");
#endif
			}
		}
		break;

	case 41: /* The World */
		return wish_object(NULL);

	case 42: /* Reverted position of the World */
		{
			int inven_num = 0;
			int *inven_ptr;
			object_type *o_ptr;

			C_MAKE(inven_ptr, INVEN_TOTAL, int);

			for (i = 0; i < INVEN_TOTAL; i++)
			{
				o_ptr = &inventory[i];

				if (!o_ptr->k_idx) continue;

				if (object_is_astral_runeweapon(o_ptr)) continue;

				if (dungeon_type == DUNGEON_HEAVEN)
				{
					if (o_ptr->name1 == ART_BRUNHILD) continue;
				}

				inven_ptr[inven_num++] = i;
			}

			if (inven_num)
			{
				int item = inven_ptr[randint0(inven_num)];
				char o_name[MAX_NLEN];

				o_ptr = &inventory[item];

				/* Get a description */
				object_desc(o_name, o_ptr, TRUE, 3);

#ifdef JP
				msg_format("%sが消えてしまった！", o_name);
#else
				msg_format("%s disappears!", o_name);
#endif

				inven_item_increase(item, -255);
				inven_item_describe(item);
				inven_item_optimize(item);
			}
			else
			{
#ifdef JP
				msg_print("何も起きなかった。");
#else
				msg_print("Nothing happens.");
#endif
			}

			C_KILL(inven_ptr, INVEN_TOTAL, int);
		}
		break;

	case 43: /* The Fool */
		if (!(fool_effect_status & FOOL_STATUS_PLAYER))
		{
			msg_print("まともに歩けなくなってしまった。頭も悪くなった気がする。");
			fool_effect_status |= FOOL_STATUS_PLAYER;
		}
		break;

	case 44: /* Reverted position of the Fool */
		if (!(fool_effect_status & FOOL_STATUS_MONSTERS))
		{
			msg_print("全てのモンスターの動きがおかしくなった。");
			fool_effect_status |= FOOL_STATUS_MONSTERS;
		}
		break;

	default:
#ifdef JP
		msg_print("何も起きなかった。");
#else
		msg_print("Nothing happens.");
#endif
		return FALSE;
	}

	return TRUE;
}

void gain_level_reward(int chosen_reward)
{
	int effect;
	bool allow_card = FALSE;

	if (!chosen_reward)
	{
		if (multi_rew) return;
		else multi_rew = TRUE;
	}

#ifdef JP
	msg_print("タロットカードの力が発動する...");
#else
	msg_print("Power of tarot cards is activated...");
#endif

	if ((chosen_reward < 0) || (chosen_reward > 44)) chosen_reward = 0;
	effect = chosen_reward ? chosen_reward : randint1(44);

#ifdef JP
	msg_format("%sだ。", tarot_info[effect].name);
#else
	msg_format("It's %s.", tarot_info[effect].name);
#endif

	if (tarot_info[effect].preserve)
	{
#ifdef JP
		if (!get_check("カード化せずこの場で使いますか？ ")) allow_card = TRUE;
#else
		if (!get_check("Do you use this without card now? ")) allow_card = TRUE;
#endif
	}

	if (!allow_card)
	{
		if (!activate_tarot_power(effect)) allow_card = TRUE;
	}

	/* Prepare a tarot card */
	if (allow_card)
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Prepare a tarot card */
		prepare_tarot_card(q_ptr, effect);

		(void)inven_carry(q_ptr);
	}
}


/*
 * old -- from PsiAngband.
 */
bool tgt_pt(int *x_ptr, int *y_ptr, bool allow_player)
{
	char ch = 0;
	int d, x, y;
	bool success = FALSE;

	int wid, hgt;

	/* Get size */
	get_screen_size(&wid, &hgt);

	x = px;
	y = py;

#ifdef JP
msg_print("場所を選んでスペースキーを押して下さい。");
#else
	msg_print("Select a point and press space.");
#endif
	msg_flag = FALSE; /* prevents "-more-" message. */

	while ((ch != ESCAPE) && !success)
	{
		bool move_fast = FALSE;

		move_cursor_relative(y, x);
		ch = inkey();
		switch (ch)
		{
		case ESCAPE:
			break;
		case ' ':
		case 't':
		case '.':
		case '5':
		case '0':
			/* illegal place */
			if (!allow_player && (x == px) && (y == py)) ch = 0;

			/* okay place */
			else success = TRUE;

			break;
		default:
			/* Look up the direction */
			d = get_keymap_dir(ch);

			/* XTRA HACK MOVEFAST */
			if (isupper(ch)) move_fast = TRUE;

			/* Handle "direction" */
			if (d)
			{
				int dx = ddx[d];
				int dy = ddy[d];

				/* XTRA HACK MOVEFAST */
				if (move_fast)
				{
					int mag = MIN(wid / 2, hgt / 2);
					x += dx * mag;
					y += dy * mag;
				}
				else
				{
					x += dx;
					y += dy;
				}

				/* Do not move horizontally if unnecessary */
				if (((x < panel_col_min + wid / 2) && (dx > 0)) ||
					 ((x > panel_col_min + wid / 2) && (dx < 0)))
				{
					dx = 0;
				}

				/* Do not move vertically if unnecessary */
				if (((y < panel_row_min + hgt / 2) && (dy > 0)) ||
					 ((y > panel_row_min + hgt / 2) && (dy < 0)))
				{
					dy = 0;
				}

				/* Apply the motion */
				if ((y >= panel_row_min + hgt) || (y < panel_row_min) ||
					 (x >= panel_col_min + wid) || (x < panel_col_min))
				{
					/* if (change_panel(dy, dx)) target_set_prepare(mode); */
					change_panel(dy, dx);
				}

				/* Slide into legality */
				if (x >= cur_wid-1) x = cur_wid - 2;
				else if (x <= 0) x = 1;

				/* Slide into legality */
				if (y >= cur_hgt-1) y = cur_hgt- 2;
				else if (y <= 0) y = 1;

			}
			break;
		}
	}

	/* Clear the top line */
	prt("", 0, 0);

	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();

	*x_ptr = x;
	*y_ptr = y;
	return success;
}


bool get_hack_dir(int *dp)
{
	int		dir;
	cptr    p;
	char    command;


	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = 0;

	/* (No auto-targeting) */

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
#ifdef JP
p = "方向 ('*'でターゲット選択, ESCで中断)? ";
#else
			p = "Direction ('*' to choose a target, Escape to cancel)? ";
#endif

		}
		else
		{
#ifdef JP
p = "方向 ('5'でターゲットへ, '*'でターゲット再選択, ESCで中断)? ";
#else
			p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";
#endif

		}

		/* Get a command (or Cancel) */
		if (!get_com(p, &command, TRUE)) break;

		if (use_menu)
		{
			if (command == '\r') command = 't';
		}  

		/* Convert various keys to "standard" keys */
		switch (command)
		{
			/* Use current target */
			case 'T':
			case 't':
			case '.':
			case '5':
			case '0':
			{
				dir = 5;
				break;
			}

			/* Set new target */
			case '*':
			case ' ':
			case '\r':
			{
				if (target_set(TARGET_KILL)) dir = 5;
				break;
			}

			default:
			{
				/* Look up the direction */
				dir = get_keymap_dir(command);

				break;
			}
		}

		/* Verify requested targets */
		if ((dir == 5) && !target_okay()) dir = 0;

		/* Error */
		if (!dir) bell();
	}

	/* No direction */
	if (!dir) return (FALSE);

	/* Save the direction */
	command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused || ((fool_effect_status & FOOL_STATUS_PLAYER) && one_in_(16)))
	{
		/* XXX XXX XXX */
		/* Random direction */
		dir = ddd[randint0(8)];
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
#ifdef JP
		if (p_ptr->confused) msg_print("あなたは混乱している。");
#else
		if (p_ptr->confused) msg_print("You are confused.");
#endif
		else msg_print("あなたは変な方向に歩いている。");

	}

	/* Save direction */
	(*dp) = dir;

	/* A "valid" direction was entered */
	return (TRUE);
}


/*
 * Return bow energy 
 */
s32b bow_energy(object_type *o_ptr)
{
	s32b energy = 100;

	/* Analyze the launcher */
	switch (o_ptr->sval)
	{
		/* Pistol and Bullet */
		case SV_PISTOL:
		{
			energy = 8000;
			break;
		}

		/* Assault Rifle and Rifle Round */
		case SV_ASSAULT_RIFLE:
		{
			energy = 12000;
			break;
		}

		/* Sniper Rifle and Rifle Round */
		case SV_SNIPER_RIFLE:
		{
			energy = 12000;
			break;
		}

		/* Shotgun and Shot Shell */
		case SV_SHOTGUN:
		{
			energy = 30000;
			break;
		}

		/* Rocket Launcher and Rocket */
		case SV_ROCKET_LAUNCHER:
		{
			energy = 15000;
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			energy = 10000;
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			energy = 10000;
			break;
		}

		/* Bowgun and Bolt */
		case SV_BOWGUN:
		{
			energy = 12000;
			break;
		}

		/* Crossbow and Bolt */
		case SV_CROSSBOW:
		{
			energy = 13333;
			break;
		}

		/* Runebow and Arrow */
		case SV_RUNEBOW:
		{
			energy = object_is_snapdragon_runeweapon(o_ptr) ? runeweapon_list[o_ptr->xtra3].bow_energy : 10000;
			break;
		}

		/* Runegun and Rifle Round */
		case SV_RUNEGUN:
		{
			energy = object_is_snapdragon_runeweapon(o_ptr) ? runeweapon_list[o_ptr->xtra3].bow_energy : 12000;
			break;
		}
	}

	return energy;
}


/*
 * Return bow tmul
 */
int bow_tmul(object_type *o_ptr)
{
	s32b tmul = 0;

	/* Analyze the launcher */
	switch (o_ptr->sval)
	{
		/* Pistol and Bullet */
		case SV_PISTOL:
		{
			tmul = 2;
			break;
		}

		/* Assault Rifle and Rifle Round */
		case SV_ASSAULT_RIFLE:
		{
			tmul = 2;
			break;
		}

		/* Sniper Rifle and Rifle Round */
		case SV_SNIPER_RIFLE:
		{
			tmul = 4;
			break;
		}

		/* Shotgun and Shot Shell */
		case SV_SHOTGUN:
		{
			tmul = 2;
			break;
		}

		/* Rocket Launcher and Rocket */
		case SV_ROCKET_LAUNCHER:
		{
			tmul = 1;
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			tmul = 2;
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			tmul = 3;
			break;
		}

		/* Bowgun and Bolt */
		case SV_BOWGUN:
		{
			tmul = 3;
			break;
		}

		/* Crossbow and Bolt */
		case SV_CROSSBOW:
		{
			tmul = 4;
			break;
		}

		/* Runebow and Arrow */
		case SV_RUNEBOW:
		{
			tmul = object_is_snapdragon_runeweapon(o_ptr) ? runeweapon_list[o_ptr->xtra3].bow_tmul : 3;
			break;
		}

		/* Runegun and Rifle Round */
		case SV_RUNEGUN:
		{
			tmul = object_is_snapdragon_runeweapon(o_ptr) ? runeweapon_list[o_ptr->xtra3].bow_tmul : 4;
			break;
		}
	}

	return tmul;
}


/*
 * Return rocket damage
 */
s32b rocket_damage(object_type *o_ptr, int to_d)
{
	s32b dam = 0;

	switch (o_ptr->xtra4)
	{
	case ROCKET_MATERIAL_BALDAR:
		dam = 150 + to_d * 13;
		break;

	case ROCKET_MATERIAL_MITHRIL:
		dam = 200 + to_d * 14;
		break;

	case ROCKET_MATERIAL_ADAMANTITE:
		dam = 250 + to_d * 16;
		break;

	case ROCKET_MATERIAL_NORMAL + ROCKET_ANTIGRAV:
		dam = (100 + to_d * 10) / 2;
		break;

	case ROCKET_MATERIAL_BALDAR + ROCKET_ANTIGRAV:
		dam = (150 + to_d * 13) / 2;
		break;

	case ROCKET_MATERIAL_MITHRIL + ROCKET_ANTIGRAV:
		dam = (200 + to_d * 14) / 2;
		break;

	case ROCKET_MATERIAL_ADAMANTITE + ROCKET_ANTIGRAV:
		dam = (250 + to_d * 16) / 2;
		break;

	default:
		dam = 100 + to_d * 10;
		break;
	}

	return dam;
}

/*
 * Return alignment title (GNE)
 */
cptr your_alignment_gne(void)
{
	switch (get_your_alignment_gne())
	{
	case ALIGN_GNE_GOOD:
#ifdef JP
		return "善良";
#else
		return "Good";
#endif

	case ALIGN_GNE_NEUTRAL:
#ifdef JP
		return "中立";
#else
		return "Neutral";
#endif

	case ALIGN_GNE_EVIL:
#ifdef JP
		return "邪悪";
#else
		return "Evil";
#endif

	default:
		return "";
	}
}

/*
 * Return alignment (GNE)
 */
int get_your_alignment_gne(void)
{
	int i;
	s32b align_gne = friend_align_gne;

	switch (p_ptr->pclass)
	{
	case CLASS_LICH:        return ALIGN_GNE_EVIL;
	case CLASS_ANGELKNIGHT: return ALIGN_GNE_GOOD;
	}

	for (i = 0; i < ETHNICITY_NUM; i++) align_gne += chaos_frame[i];

	if (align_gne > 299) return ALIGN_GNE_GOOD;
	else if (align_gne > -300) return ALIGN_GNE_NEUTRAL;
	else return ALIGN_GNE_EVIL;
}

/*
 * Return alignment title (LNC)
 */
cptr your_alignment_lnc(void)
{
	switch (get_your_alignment_lnc())
	{
	case ALIGN_LNC_LAWFUL:
#ifdef JP
		return "秩序";
#else
		return "Lawful";
#endif

	case ALIGN_LNC_NEUTRAL:
#ifdef JP
		return "中立";
#else
		return "Neutral";
#endif

	case ALIGN_LNC_CHAOTIC:
#ifdef JP
		return "混沌";
#else
		return "Chaotic";
#endif

	default:
		return "";
	}
}

/*
 * Return alignment (LNC)
 */
int get_your_alignment_lnc(void)
{
	if (p_ptr->align > 49) return ALIGN_LNC_LAWFUL;
	else if (p_ptr->align > -50) return ALIGN_LNC_NEUTRAL;
	else return ALIGN_LNC_CHAOTIC;
}

/*
 * Change alignment (LNC)
 */
void change_your_alignment_lnc(int amt)
{
	p_ptr->align_self += amt;
	if (p_ptr->align_self > 300) p_ptr->align_self = 300;
	if (p_ptr->align_self < -300) p_ptr->align_self = -300;
	p_ptr->update |= (PU_BONUS);
}

/*
 * Change the questor of dungeon level 99
 */
void change_level99_quest(bool flip)
{
	int old_quest = p_ptr->inside_quest;

	if (astral_mode) return;

	if (flip)
	{
		if (quest[QUEST_LANCELOT].status == QUEST_STATUS_TAKEN)
		{
			/* Re-init the quest (Lancelot -> Denim) */
			quest[QUEST_LANCELOT].status = QUEST_STATUS_UNTAKEN;
			quest[QUEST_LANCELOT].cur_num = 0;
			quest[QUEST_LANCELOT].max_num = 0;
			quest[QUEST_LANCELOT].type = 0;
			quest[QUEST_LANCELOT].level = 0;
			quest[QUEST_LANCELOT].r_idx = 0;
			quest[QUEST_LANCELOT].complev = 0;

			init_flags = INIT_ASSIGN;
			p_ptr->inside_quest = QUEST_DENIM;

			process_dungeon_file("q_info.txt", 0, 0, 0, 0);

			quest[QUEST_DENIM].status = QUEST_STATUS_TAKEN;

			p_ptr->inside_quest = old_quest;
		}
	}
	else
	{
		if (quest[QUEST_DENIM].status == QUEST_STATUS_TAKEN)
		{
			/* Re-init the quest (Denim -> Lancelot) */
			quest[QUEST_DENIM].status = QUEST_STATUS_UNTAKEN;
			quest[QUEST_DENIM].cur_num = 0;
			quest[QUEST_DENIM].max_num = 0;
			quest[QUEST_DENIM].type = 0;
			quest[QUEST_DENIM].level = 0;
			quest[QUEST_DENIM].r_idx = 0;
			quest[QUEST_DENIM].complev = 0;

			init_flags = INIT_ASSIGN;
			p_ptr->inside_quest = QUEST_LANCELOT;

			process_dungeon_file("q_info.txt", 0, 0, 0, 0);

			quest[QUEST_LANCELOT].status = QUEST_STATUS_TAKEN;

			p_ptr->inside_quest = old_quest;
		}
	}
}

void process_chaos_frame(int ethnic)
{
	switch (ethnic)
	{
	case ETHNICITY_GARGASTAN:
		if (misc_event_flags & EVENT_CLOSE_BARMAMUTHA)
		{
			if (chaos_frame[ethnic] == CFRAME_UPPER_LIMIT)
				misc_event_flags &= ~(EVENT_CLOSE_BARMAMUTHA);
		}
		break;

	case ETHNICITY_ZENOBIAN:
		if (!astral_mode)
		{
			if (p_ptr->pclass == CLASS_WHITEKNIGHT)
			{
				if (chaos_frame[ethnic] < 0) expire_current_class();
			}
		}
		break;

	case ETHNICITY_LODIS:
		if (!astral_mode)
		{
			if (p_ptr->pclass == CLASS_TEMPLEKNIGHT)
			{
				if (chaos_frame[ethnic] < 0) expire_current_class();
			}
		}
		break;
	}
}

/*
 * Change chaos frame
 */
void change_chaos_frame(int ethnic, int amt)
{
	/* Invalid chaos frame */
	if ((ethnic < 0) || (ethnic >= ETHNICITY_NUM)) return;

	/* Racial murderer!! */
	if (chaos_frame[ethnic] <= CFRAME_LOWER_LIMIT) return;

	chaos_frame[ethnic] += amt;
	if (chaos_frame[ethnic] > CFRAME_UPPER_LIMIT) chaos_frame[ethnic] = CFRAME_UPPER_LIMIT;
	if (chaos_frame[ethnic] < CFRAME_LOWER_LIMIT) chaos_frame[ethnic] = CFRAME_LOWER_LIMIT;

	process_chaos_frame(ethnic);
};

/*
 * Return proficiency level of weapons and misc. skills (except riding)
 */
int weapon_exp_level(int weapon_exp)
{
	if (weapon_exp < SKILL_EXP_NOVICE) return SKILL_LEVEL_BEGINNER;
	else if (weapon_exp < SKILL_EXP_AVERAGE) return SKILL_LEVEL_NOVICE;
	else if (weapon_exp < SKILL_EXP_SKILLED) return SKILL_LEVEL_AVERAGE;
	else if (weapon_exp < SKILL_EXP_EXPERT) return SKILL_LEVEL_SKILLED;
	else if (weapon_exp < SKILL_EXP_MASTER) return SKILL_LEVEL_EXPERT;
	else return SKILL_LEVEL_MASTER;
}

/*
 * Return proficiency level of weapons and misc. skills (except riding)
 */
int skill_exp_level(int skill_exp)
{
	if (skill_exp < SKILL_EXP_NOVICE) return SKILL_LEVEL_BEGINNER;
	else if (skill_exp < SKILL_EXP_AVERAGE) return SKILL_LEVEL_NOVICE;
	else if (skill_exp < SKILL_EXP_SKILLED) return SKILL_LEVEL_AVERAGE;
	else if (skill_exp < SKILL_EXP_EXPERT) return SKILL_LEVEL_SKILLED;
	else if (skill_exp < SKILL_EXP_MASTER) return SKILL_LEVEL_EXPERT;
	else return SKILL_LEVEL_MASTER;
}

/*
 * Get current element of player
 */

s16b get_cur_pelem(void)
{
	if (inventory[INVEN_OUTER].k_idx && (inventory[INVEN_OUTER].name2 == EGO_NO_ELEM)) return NO_ELEM;
	else if (p_ptr->opposite_pelem) return get_opposite_elem(p_ptr->pelem);
	else return p_ptr->pelem;
}

/*
 * Get current element of a moster
 */

s16b get_cur_melem(monster_type *m_ptr)
{
	if (!m_ptr) return NO_ELEM;

	return m_ptr->opposite_elem ? get_opposite_elem(m_ptr->elem) : m_ptr->elem;
}

/*
 * Get element type from attribute
 */
s16b get_elem_type(int typ)
{
	/* Analyze the element from type */
	switch (typ)
	{
	/* ELEM_FIRE */
	case GF_FIRE:
	case GF_PLASMA:
	case GF_PURE_FIRE:
	case GF_STRIKE_NOVA:
		return ELEM_FIRE;

	/* ELEM_AQUA */
	case GF_COLD:
	case GF_WATER:
	case GF_ICE:
	case GF_PURE_AQUA:
		return ELEM_AQUA;

	/* ELEM_EARTH */
	case GF_ACID:
	case GF_SHARDS:
	case GF_STONE:
	case GF_ROCKET:
	case GF_METEOR:
	case GF_PURE_EARTH:
	case GF_SPECIAL_STONE:
		return ELEM_EARTH;

	/* ELEM_WIND */
	case GF_ELEC:
	case GF_SOUND:
	case GF_INERTIA:
	case GF_GRAVITY:
	case GF_PURE_WIND:
		return ELEM_WIND;

	/* Don't affect them */
	case GF_ATTACK:
	case GF_WORD_OF_PAIN:
	case GF_WATER_FLOW:
		return ELEM_DONT_AFFECT;

	/* No element */
	default:
		return NO_ELEM;
	}
}

bool are_opposite_elems(s16b atk_elem, s16b def_elem)
{
	switch (atk_elem)
	{
	case ELEM_FIRE:
		if (def_elem == ELEM_AQUA) return TRUE;
		break;

	case ELEM_AQUA:
		if (def_elem == ELEM_FIRE) return TRUE;
		break;

	case ELEM_EARTH:
		if (def_elem == ELEM_WIND) return TRUE;
		break;

	case ELEM_WIND:
		if (def_elem == ELEM_EARTH) return TRUE;
		break;
	}

	return FALSE;
}

s16b get_opposite_elem(s16b elem)
{
	switch (elem)
	{
	case ELEM_FIRE:
		return ELEM_AQUA;

	case ELEM_AQUA:
		return ELEM_FIRE;

	case ELEM_EARTH:
		return ELEM_WIND;

	case ELEM_WIND:
		return ELEM_EARTH;
	}

	return NO_ELEM;
}

s16b get_cur_weather_elem(int pick)
{
	s16b tmp = 0;

	/* Paranoia */
	if ((pick < MIN_ELEM) || (pick >= ELEM_NUM)) return 0;

	tmp = weather_table[WEATHER_RAIN][weather_level(weather[WEATHER_RAIN])].elem[pick]
	    + weather_table[WEATHER_WIND][weather_level(weather[WEATHER_WIND])].elem[pick]
	    + weather_table[WEATHER_TEMP][weather_level(weather[WEATHER_TEMP])].elem[pick];

	return tmp;
}

/*
 * Set the new weather (incremental)
 */
void set_weather(int inc_rain, int inc_wind, int inc_temp)
{
	int tmp_val, i;
	int inc_val[WEATHER_TYPE_NUM];

	inc_val[MIN_WEATHER_TYPE] = inc_rain;
	inc_val[MIN_WEATHER_TYPE + 1] = inc_wind;
	inc_val[MIN_WEATHER_TYPE + 2] = inc_temp;

	for (i = MIN_WEATHER_TYPE; i < WEATHER_TYPE_NUM; i++)
	{
		prev_weather[i] = weather[i];
		tmp_val = weather[i] + inc_val[i];
		if (tmp_val > MAX_WEATHER_VAL) tmp_val = MAX_WEATHER_VAL;
		else if (tmp_val < MIN_WEATHER_VAL) tmp_val = MIN_WEATHER_VAL;
		weather[i] = tmp_val;
	}

	/* Apply weather effects for features in floor */
	apply_weather_effect(FALSE);

	p_ptr->redraw |= (PR_WEATHER);
	if (p_ptr->action == ACTION_ELEMSCOPE) p_ptr->redraw |= (PR_MAP);
}

void change_grid_elem(cave_type *c_ptr, s16b elem, s16b amount)
{
	int tmp_elem_val;

	if (elem == NO_ELEM) return;

	tmp_elem_val = c_ptr->elem[elem] + amount;

	if (tmp_elem_val < -99) tmp_elem_val = -99;
	else if (tmp_elem_val > 99) tmp_elem_val = 99;

	c_ptr->elem[elem] = tmp_elem_val;

	return;
}

/*
 * Modify spell damage by elements of player or monsters
 */
int modify_dam_by_elem(int a_who, int d_who, int dam, int typ, int mode)
{
	cave_type *ac_ptr;
	cave_type *dc_ptr;

	int ret_dam;
	bool see_a = TRUE, see_d;
	int ay = 0, ax = 0;

	/* Element type */
	s16b typ_elem = NO_ELEM, atk_elem = NO_ELEM, def_elem = NO_ELEM;

	/* Ra & Rd */
	int ra, rd;

	/*
	 * Table of weather effect to Ra
	   fire_effect_table[is bow][weather type][weather level]
	 */
	static s16b fire_effect_table[2][2][WEATHER_LEVEL_NUM] =
	{
		{
			{ 0,  0, -1, -2},
			{ 0, -1, -2, -4}
		},

		{
			{ 0,  0, -2, -4},
			{ 0, -2, -4, -8}
		}
	};

	if (mode == MODIFY_ELEM_MODE_NONE) return dam;

	typ_elem = get_elem_type(typ);
	if (typ_elem == ELEM_DONT_AFFECT) return dam;

	/* Attacker's variable */
	if (a_who > 0) /* From monster */
	{
		monster_type *m_ptr = &m_list[a_who];
		ay = m_ptr->fy;
		ax = m_ptr->fx;
		ac_ptr = &cave[ay][ax];
		atk_elem = get_cur_melem(m_ptr);
		see_a = m_ptr->ml;
	}
	else if (!a_who) /* From player */
	{
		ay = py;
		ax = px;
		ac_ptr = &cave[ay][ax];
		atk_elem = get_cur_pelem();
		see_a = TRUE;
	}

	/* Defender's variable */
	if (!d_who) /* To player */
	{
		dc_ptr = &cave[py][px];
		def_elem = get_cur_pelem();
		see_d = TRUE;
	}
	else /* To monster */
	{
		monster_type *m_ptr = &m_list[d_who];
		dc_ptr = &cave[m_ptr->fy][m_ptr->fx];
		def_elem = get_cur_melem(m_ptr);
		see_d = m_ptr->ml;
	}

	if (p_ptr->wizard && show_damage && see_a)
	{
		switch (mode)
		{
		case MODIFY_ELEM_MODE_MELEE:
			msg_print("(直接物理攻撃計算モード)");
			break;

		case MODIFY_ELEM_MODE_FIRE:
			msg_print("(射撃物理攻撃計算モード)");
			break;

		case MODIFY_ELEM_MODE_THROW:
			msg_print("(投擲物理攻撃計算モード)");
			break;

		case MODIFY_ELEM_MODE_MAGIC:
			msg_format("(魔法攻撃計算モード [魔法対応エレメント: %s])",
			           (typ_elem != NO_ELEM) ? elem_names[typ_elem] : "なし");
			break;

		default:
			msg_format("(不正なモード: %d)", mode);
			break;
		}
	}

	/********* 'Ra' *********/

	ra = 150;
	if (a_who >= 0)
	{
		ra += f_info[ac_ptr->feat].to_offence;
		if (ra > 200) ra = 200;

		if (mode == MODIFY_ELEM_MODE_MAGIC)
		{
			if (typ_elem != NO_ELEM) ra += (ac_ptr->elem[typ_elem] + dc_ptr->elem[typ_elem]) / 2;
		}
		else
		{
			if (atk_elem != NO_ELEM) ra += (ac_ptr->elem[atk_elem] + dc_ptr->elem[atk_elem]) / 2;
		}
		if (ra > 200) ra = 200; else if (ra < 0) ra = 0;
	}
	else
	{
		if ((mode == MODIFY_ELEM_MODE_MAGIC) && (typ_elem != NO_ELEM))
		{
			ra += dc_ptr->elem[typ_elem] / 2;
			if (ra > 200) ra = 200; else if (ra < 0) ra = 0;
		}
	}

	ra -= 100;
	if (ra < 0) ra = 0;

	switch (mode)
	{
	case MODIFY_ELEM_MODE_FIRE:
	case MODIFY_ELEM_MODE_THROW:
		{
			bool is_bow = FALSE;
			bool is_gun = FALSE;

			if (mode == MODIFY_ELEM_MODE_FIRE)
			{
				if (a_who)
				{
					switch (typ)
					{
					case GF_BLUNT:
						is_gun = TRUE;
						break;
					case GF_EDGED:
						is_bow = TRUE;
						break;
					}
				}
				else
				{
					object_type *o_ptr = &inventory[INVEN_BOW];

					if (o_ptr->k_idx && (o_ptr->tval == TV_BOW))
					{
						switch (get_weapon_type(&k_info[o_ptr->k_idx]))
						{
						case WT_BOW:
							switch (o_ptr->sval)
							{
							case SV_SHORT_BOW:
							case SV_LONG_BOW:
							case SV_RUNEBOW:
								is_bow = TRUE;
								break;
							}
							break;
						case WT_GUN:
							is_gun = TRUE;
							break;
						}
					}
				}
			}

			if (!is_gun)
			{
				ra += fire_effect_table[is_bow][0][weather_level(WEATHER_RAIN)];
				if (ra > 200) ra = 200; else if (ra < 0) ra = 0;

				ra += fire_effect_table[is_bow][1][weather_level(WEATHER_WIND)];
				if (ra > 200) ra = 200; else if (ra < 0) ra = 0;
			}
		}
		break;

	case MODIFY_ELEM_MODE_MAGIC:
		if ((atk_elem != NO_ELEM) && (typ_elem != NO_ELEM))
		{
			if (atk_elem == typ_elem) ra += 10;
			else if (are_opposite_elems(atk_elem, typ_elem)) ra -= 10;
			if (ra > 200) ra = 200; else if (ra < 0) ra = 0;
		}

		ra += get_cur_weather_elem(typ_elem);
		if (ra > 200) ra = 200; else if (ra < 0) ra = 0;
		break;
	}

	if (a_who >= 0)
	{
		ra -= 20 * is_fear_field_grid(a_who, ay, ax);
		if (ra < 0) ra = 0;
	}

	/********* 'Rd' *********/

	rd = 150 + f_info[dc_ptr->feat].to_defence;
	if (rd > 200) rd = 200;

	if (def_elem != NO_ELEM)
	{
		rd += dc_ptr->elem[def_elem];
		if (rd > 200) rd = 200; else if (rd < 0) rd = 0;
	}

	rd -= 100;
	if (rd < 0) rd = 0;

	if ((atk_elem != NO_ELEM) && (def_elem != NO_ELEM))
	{
		if (atk_elem == def_elem) rd += 40;
		else if (are_opposite_elems(atk_elem, def_elem)) rd -= 20;
		if (rd > 200) rd = 200; else if (rd < 0) rd = 0;
	}

	ret_dam = dam * (((100 + ra - rd) > 0) ? (100 + ra - rd) : 0) / 100;
	if (p_ptr->wizard && show_damage && see_a && see_d)
	{
		msg_format("(Ra: %d、Rd: %d、補正前の耐性軽減なし威力: %d) ", ra, rd, dam);
	}

	return ret_dam;
}

s16b choose_elem(void)
{
	int chosen_elem;

#ifdef ALLOW_REPEAT
	if (!(repeat_pull(&chosen_elem) && (chosen_elem >= MIN_ELEM) && (chosen_elem < ELEM_NUM)))
	{
#endif /* ALLOW_REPEAT */

		/* Save screen */
		screen_save();

#ifdef JP
		c_prt(elem_attr(ELEM_FIRE),  "        a) 火", 2, 14);
#else
		c_prt(elem_attr(ELEM_FIRE),  "        a) Fire", 2, 14);
#endif

#ifdef JP
		c_prt(elem_attr(ELEM_AQUA),  "        b) 水", 3, 14);
#else
		c_prt(elem_attr(ELEM_AQUA),  "        b) Aqua", 3, 14);
#endif

#ifdef JP
		c_prt(elem_attr(ELEM_EARTH), "        c) 地", 4, 14);
#else
		c_prt(elem_attr(ELEM_EARTH), "        c) Earth", 4, 14);
#endif

#ifdef JP
		c_prt(elem_attr(ELEM_WIND),  "        d) 風", 5, 14);
#else
		c_prt(elem_attr(ELEM_WIND),  "        d) Wind", 5, 14);
#endif


		prt("", 6, 14);
		prt("", 7, 14);
		prt("", 8, 14);
		prt("", 9, 14);

		prt("", 1, 0);
#ifdef JP
		prt("        エレメントを選んでください。", 1, 14);
#else
		prt("        Choose a element.", 1, 14);
#endif

		switch (inkey())
		{
		case 'A':
		case 'a':
			chosen_elem = ELEM_FIRE;
			break;

		case 'B':
		case 'b':
			chosen_elem = ELEM_AQUA;
			break;

		case 'C':
		case 'c':
			chosen_elem = ELEM_EARTH;
			break;

		case 'D':
		case 'd':
			chosen_elem = ELEM_WIND;
			break;

		default:
			chosen_elem = NO_ELEM;
			break;
		}

		/* Load screen */
		screen_load();

#ifdef ALLOW_REPEAT
		repeat_push(chosen_elem);
	}
#endif /* ALLOW_REPEAT */

	return (s16b)chosen_elem;
}

/*
 * Is this class is choosable?
 */
bool can_choose_class(byte new_class, byte mode)
{
	player_class *new_cp_ptr = &class_info[new_class];
	int i;

	/* Gender restriction */
	switch (p_ptr->psex)
	{
	case SEX_MALE:
		if (!(new_cp_ptr->c_flags & PCF_SEX_MALE)) return FALSE;
		break;

	case SEX_FEMALE:
		if (!(new_cp_ptr->c_flags & PCF_SEX_FEMALE)) return FALSE;
		break;

	default:
		return FALSE;
	}

	/* Mode-dependent process */
	switch (mode)
	{
	case CLASS_CHOOSE_MODE_NORMAL:
		/* Reincarnation classes are must not be chosen now */
		if (new_cp_ptr->c_flags & PCF_REINCARNATE) return FALSE;

		/* Astral mode */
		if (astral_mode && (new_class != p_ptr->pclass))
		{
			/* Several classes cannot change more */
			if (cp_ptr->c_flags & PCF_REINCARNATE) return FALSE;
			return TRUE;
		}

		/* Several classes cannot change more */
		if (cp_ptr->c_flags & PCF_NO_CHANGE) return FALSE;

		if (new_class == CLASS_TEMPLEKNIGHT)
		{
			if (misc_event_flags & EVENT_CANNOT_BE_TEMPLEKNIGHT) return FALSE;
			if (!r_info[MON_LANCELOT].max_num) return FALSE;
			if (chaos_frame[ETHNICITY_LODIS] < 100) return FALSE;
		}
		else if (new_class == CLASS_WHITEKNIGHT)
		{
			if (misc_event_flags & EVENT_CANNOT_BE_WHITEKNIGHT) return FALSE;
			if (!r_info[MON_MAN_LOOK_SEA].max_num) return FALSE;
			if (chaos_frame[ETHNICITY_ZENOBIAN] < 100) return FALSE;
		}
		break;

	case CLASS_CHOOSE_MODE_BIRTH:
		/* Choosable classes on birth */
		if (new_cp_ptr->c_flags & PCF_BIRTH) return TRUE;
		else return FALSE;

	case CLASS_CHOOSE_MODE_DEATH:
		/* Reincarnation classes only */
		if (!(new_cp_ptr->c_flags & PCF_REINCARNATE)) return FALSE;

		/* Reincarnated classes are no more reincarnated */
		if (cp_ptr->c_flags & PCF_REINCARNATE) return FALSE;

		/* Undeads are no more reincarnated */
		if ((rp_ptr->r_flags & PRF_UNDEAD) || (cp_ptr->c_flags & PCF_UNDEAD)) return FALSE;

		/* Class specific conditions */
		switch (new_class)
		{
		case CLASS_LICH:
			/* Magic-user classes only */
			if ((p_ptr->pclass != CLASS_WIZARD)
				&& (p_ptr->pclass != CLASS_WARLOCK)
				&& (p_ptr->pclass != CLASS_EXORCIST)
				&& (p_ptr->pclass != CLASS_SIRENE)
				&& (p_ptr->pclass != CLASS_WITCH))
				return FALSE;
			break;

		case CLASS_ANGELKNIGHT:
			/* Good player only */
			if (get_your_alignment_gne() != ALIGN_GNE_GOOD) return FALSE;

			/* Chance is dependent on CHR (Min 40%) */
			if (randint1(100) > ((MAX(p_ptr->stat_use[A_CHR] - 100, 0) / 2) + 40)) return FALSE;
			break;
		}
		break;

	default:
		return FALSE;
	}

	/* Same class cannot be chosen */
	if (new_class == p_ptr->pclass) return FALSE;

	/* Alignment restriction (LNC) */
	switch (get_your_alignment_lnc())
	{
	case ALIGN_LNC_LAWFUL:
		if (!(new_cp_ptr->c_flags & PCF_ALIGN_LAWFUL)) return FALSE;
		break;

	case ALIGN_LNC_NEUTRAL:
		if (!(new_cp_ptr->c_flags & PCF_ALIGN_NEUTRAL)) return FALSE;
		break;

	case ALIGN_LNC_CHAOTIC:
		if (!(new_cp_ptr->c_flags & PCF_ALIGN_CHAOTIC)) return FALSE;
		break;

	default:
		return FALSE;
	}

	/* Stat restriction */
	for (i = 0; i < A_MAX; i++)
		if (adjust_stat(p_ptr->stat_max[i], rp_ptr->r_adj[i]) < new_cp_ptr->c_need[i]) return FALSE;

	/* Assume okay */
	return TRUE;
}

int cut_level(int cut)
{
	/* Mortal wound */
	if (cut > 1000) return 7;

	/* Deep gash */
	else if (cut > 200) return 6;

	/* Severe cut */
	else if (cut > 100) return 5;

	/* Nasty cut */
	else if (cut > 50) return 4;

	/* Bad cut */
	else if (cut > 25) return 3;

	/* Light cut */
	else if (cut > 10) return 2;

	/* Graze */
	else if (cut > 0) return 1;

	/* None */
	else return 0;
}

int stun_level(int stun)
{
	/* Knocked out */
	if (stun > 300) return 4;

	/* Nearly faint */
	else if (stun > 200) return 3;

	/* Heavy stun */
	else if (stun > 100) return 2;

	/* Stun */
	else if (stun > 0) return 1;

	/* None */
	else return 0;
}
