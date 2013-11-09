/* File: mind.c */

/* Purpose: Mindcrafter code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "mindtips.h"


/* Mindcraft */
typedef struct mind_type mind_type;
struct mind_type
{
	int     lev;
	int     cost;
	int     fail;
	int     stat;
	cptr    name;
};

typedef struct mind_power mind_power;
struct mind_power
{
	mind_type info[MAX_MIND_POWERS];
};


mind_power mind_powers =
{
	{
		{  1,  5, 10, A_WIS, "ダウジング"},
		{  2, 50, 40, A_WIS, "予兆"},
		{  9, 28, 55, A_STR, "テレキネシス"},
		{ 11, 11, 30, A_WIS, "念動衝撃弾"},
		{ 18, 20, 35, A_WIS, "テレパシー"},
		{ 22, 15, 40, A_WIS, "引き寄せ"},
		{ 24, 26, 40, A_WIS, "ヴィジョン"},
		{ 25, 10, 45, A_WIS, "精神波動"},
		{ 30, 28, 50, A_WIS, "完全調査"},
		{ 32, 30, 60, A_WIS, "クレヤヴォヤンス"},
		{ 36, 50, 85, A_WIS, "運命"},
		{ 39, 75, 85, A_WIS, "サイコメトリー"},
		{ 40, 40, 80, A_WIS, "ラック"},
		{ 41, 50, 75, A_WIS, "マインド・ハック"},
		{ 45, 55, 85, A_WIS, "イミュニティ"},
	}
};


static void mindcraft_info(char *p, int power)
{
#ifdef JP
	cptr s_dam = "損傷:";
	cptr s_dur = "期間:";
#else
	cptr s_dam = "dam ";
	cptr s_dur = "dur ";
#endif
	int pwis = p_ptr->stat_use[A_WIS];

	strcpy(p, "");

	switch (power)
	{
	case  0: break;
	case  1: break;
	case  2: break;
	case  3: sprintf(p, " %s6d%d", s_dam, pwis / 4); break;
	case  4: sprintf(p, " %s50+d25", s_dur);  break;
	case  5: break;
	case  6: break;
	case  7: sprintf(p, " %sd%d", s_dam, pwis * 2 / 3); break;
	case  8: break;
	case  9: break;
	case 10: break;
	case 11: break;
	case 12: sprintf(p, " %s10d5", s_dur);  break;
	case 13: break;
	case 14: sprintf(p, " %s12+8d2", s_dur);  break;
	}
}

  /*
 * Allow user to choose a mindcraft power.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 *
 * nb: This function has a (trivial) display bug which will be obvious
 * when you run it. It's probably easy to fix but I haven't tried,
 * sorry.
 */
static int get_mind_power(int *sn, bool only_browse)
{
	int             i;
	int             num = MAX_MIND_POWERS;
	int             min_num = MAX_MIND_POWERS - 1;
	int             max_num = 0;
	int             y = 1;
	int             x = 10;
	int             minfail = 0;
	int             plev = p_ptr->lev;
	int             chance = 0;
	int             ask = TRUE;
	char            choice;
	char            out_val[160];
	char            comment[80];
	bool            unlearned[MAX_MIND_POWERS];
	cptr            p;

	mind_type       spell;
	mind_power      *mind_ptr;
	bool            flag, redraw;
	int menu_line = (use_menu ? 1 : 0);

#ifdef JP
	p = "超能力";
#else
	p = "mindcraft";
#endif
	mind_ptr = &mind_powers;

	/* Assume cancelled */
	*sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (p_ptr->mindcraft_learned & (0x00000001 << *sn))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	for (i = 0; i < MAX_MIND_POWERS; i++)
	{
		if (p_ptr->mindcraft_learned & (0x00000001 << i))
		{
			if (i < min_num) min_num = i;
			if (i > max_num) max_num = i;
			unlearned[i] = FALSE;
		}
		else unlearned[i] = TRUE;
	}

	/* Build a prompt (accept all spells) */
	if (only_browse)
	{
#ifdef JP
		(void) strnfmt(out_val, 78, "(%^s %c-%c, '*'で一覧, ESC) どの%sについて知りますか？",
#else
		(void) strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
#endif
				       p, I2A(min_num), I2A(max_num), p);
	}
	else
	{
#ifdef JP
		(void) strnfmt(out_val, 78, "(%^s %c-%c, '*'で一覧, ESC) どの%sを使いますか？",
#else
		(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
#endif
		p, I2A(min_num), I2A(max_num), p);
	}

	if (use_menu && !only_browse) screen_save();
	/* Get a spell from the user */

	choice= (always_show_list || use_menu) ? ESCAPE:1 ;
	while (!flag)
	{
		if(choice==ESCAPE) choice = ' '; 
		else if( !get_com(out_val, &choice, TRUE) )break;

		if (use_menu && choice != ' ')
		{
			switch(choice)
			{
				case '0':
				{
					if (!only_browse) screen_load();
					return (FALSE);
				}

				case '8':
				case 'k':
				case 'K':
				{
					do
					{
						menu_line += (num - 1);
						if (menu_line > num) menu_line -= num;
					}
					while (unlearned[menu_line - 1]);
					break;
				}

				case '2':
				case 'j':
				case 'J':
				{
					do
					{
						menu_line++;
						if (menu_line > num) menu_line -= num;
					}
					while (unlearned[menu_line - 1]);
					break;
				}

				case 'x':
				case 'X':
				case '\r':
				case '\n':
				{
					i = menu_line - 1;
					ask = FALSE;
					break;
				}
			}
		}
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?') || (use_menu && ask))
		{
			/* Show the list */
			if (!redraw || use_menu)
			{
				char psi_desc[80];

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				if (!only_browse && !use_menu) screen_save();

				/* Display a list of spells */
				prt("", y, x);
#ifdef JP
				put_str("名前", y, x + 5);
#else
				put_str("Name", y, x + 5);
#endif

#ifdef JP
				put_str("Lv   MP   失率 効果", y, x + 35);
#else
				put_str("Lv   MP   Fail Info", y, x + 35);
#endif

				/* Dump the spells */
				for (i = 0; i < MAX_MIND_POWERS; i++)
				{
					int cost;

					/* Access the spell */
					spell = mind_ptr->info[i];

					chance = spell.fail;

					cost = spell.cost;
					if (chance)
					{
						/* Reduce failure rate by "effective" level adjustment */
						chance -= 3 * (plev - spell.lev);

						/* Reduce failure rate by stat adjustment */
						chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[spell.stat]] - 1);

						/* Extract the minimum failure rate */
						minfail = adj_mag_fail[p_ptr->stat_ind[spell.stat]];

						/* Minimum failure rate */
						if (chance < minfail) chance = minfail;

						if (fool_effect_status & FOOL_STATUS_PLAYER) chance += 10;

						/* Stunning makes spells harder */
						if (p_ptr->stun > 200) chance += 40;
						else if (p_ptr->stun > 100) chance += 25;
						else if (p_ptr->stun) chance += 15;

						/* Always a 5 percent chance of working */
						if (chance > 95) chance = 95;
					}

					/* Get info */
					mindcraft_info(comment, i);

					if (use_menu)
					{
#ifdef JP
						if (i == (menu_line-1)) strcpy(psi_desc, "  》 ");
#else
						if (i == (menu_line-1)) strcpy(psi_desc, "  >  ");
#endif
						else strcpy(psi_desc, "     ");
					}
					else
					{
						if (unlearned[i]) strcpy(psi_desc, "     ");
						else sprintf(psi_desc, "  %c) ",I2A(i));
					}
					if (!unlearned[i])
					{
						/* Dump the spell --(-- */
						strcat(psi_desc,
						       format("%-30s%2d %4d   %3d%%%s",
							      spell.name, spell.lev, cost,
							      chance, comment));
					}
					prt(psi_desc, y + i + 1, x);
				}

				/* Clear the bottom line */
				prt("", y + i + 1, x);
			}

			/* Hide the list */
			else if (!only_browse)
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
			/* Note verify */
			ask = isupper(choice);

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}

		/* Totally Illegal */
		if ((i < min_num) || (i > max_num))
		{
			bell();
			continue;
		}
		if (unlearned[i])
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = mind_ptr->info[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
#ifdef JP
(void) strnfmt(tmp_val, 78, "%sを使いますか？", spell.name);
#else
			(void)strnfmt(tmp_val, 78, "Use %s? ", spell.name);
#endif


			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw && !only_browse) screen_load();

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


/*
 * do_cmd_cast calls this function
 */
static bool cast_mindcraft_spell(int spell)
{
	int dir;
	int pwis = p_ptr->stat_use[A_WIS];

	/* spell code */
	switch (spell)
	{
	case 0: /* Detect Objects and Treasure */
		(void)detect_objects_normal(DETECT_RAD_DEFAULT);
		(void)detect_treasure(DETECT_RAD_DEFAULT);
		(void)detect_objects_gold(DETECT_RAD_DEFAULT);
		break;
	case 1:
		{
			int min_random_quest = astral_mode ? MIN_RANDOM_QUEST_ASTRAL : MIN_RANDOM_QUEST;
			int tell_depth = astral_mode ? 0 : 100;
			int tell_q_idx = 0;
			int i;

			for (i = min_random_quest; i < min_random_quest + 10; i++)
			{
				if (quest[i].status == QUEST_STATUS_TAKEN)
				{
					if (astral_mode)
					{
						if (quest[i].level > tell_depth)
						{
							tell_depth = quest[i].level;
							tell_q_idx = i;
						}
					}
					else
					{
						if (quest[i].level < tell_depth)
						{
							tell_depth = quest[i].level;
							tell_q_idx = i;
						}
					}
				}
			}

			if (tell_q_idx)
			{
				quest_type *q_ptr = &quest[tell_q_idx];
				msg_format("%d階は…%sに守られているようだ。", q_ptr->level, r_name + r_info[q_ptr->r_idx].name);
			}
			else
			{
				msg_print("何も感じられなかった。");
			}
		}
		break;
	case 2:
		/* Telekinesis */
		if (!get_aim_dir(&dir)) return FALSE;

		fetch(dir, MAX_SHORT, FALSE);
		break;
	case 3:
		/* Fist of Force  ---  not 'true' TK  */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_TELEKINESIS, dir, damroll(6, pwis / 4), 0, FALSE);
		break;
	case 4: /* Sense Minds */
		(void)set_tim_esp(randint1(25) + 50, FALSE);
		break;
	case 5:
	{
		int tx, ty;
		monster_type *m_ptr;
		monster_race *r_ptr;
		char m_name[80];

		if (!get_aim_dir(&dir)) return FALSE;
		range_restricted_target(dir, MAX_RANGE, &ty, &tx, TRUE);

		if (!cave[ty][tx].m_idx) break;
		if (!player_has_los_bold(ty, tx)) break;
		m_ptr = &m_list[cave[ty][tx].m_idx];
		r_ptr = &r_info[m_ptr->r_idx];
		monster_desc(m_name, m_ptr, 0);
		m_ptr->csleep = 0;

		if (m_ptr->ml)
		{
			/* Hack -- auto-recall */
			monster_race_track(m_ptr->ap_r_idx);

			/* Hack - auto-track */
			health_track(cave[ty][tx].m_idx);
		}

		if (r_ptr->flags3 & (RF3_RES_TELE))
		{
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
				msg_format("%sには効果がなかった！", m_name);
#else
				msg_format("%s is unaffected!", m_name);
#endif

				break;
			}
			else if (r_ptr->level > randint1(100))
			{
				r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
				msg_format("%sには耐性がある！", m_name);
#else
				msg_format("%s resists!", m_name);
#endif

				break;
			}
		}
#ifdef JP
		msg_format("%sを引き戻した。", m_name);
#else
		msg_format("You command %s to return.", m_name);
#endif

		teleport_to(cave[ty][tx].m_idx, py, px, 2, 100, FALSE);
		break;
	}
	case 6:
	{
		monster_type *m_ptr;
		monster_race *r_ptr;
		int i;

		/* Process the monsters (backwards) */
		for (i = m_max - 1; i >= 1; i--)
		{
			/* Access the monster */
			m_ptr = &m_list[i];

			/* Ignore "dead" monsters */
			if (!m_ptr->r_idx) continue;

			r_ptr = &r_info[m_ptr->r_idx];

			if (r_ptr->flags1 & RF1_UNIQUE) msg_format("%s  ", r_name + r_ptr->name);
		}
		break;
	}
	case 7:
		/* Mindwave */
#ifdef JP
		msg_print("精神を捻じ曲げる波動を発生させた！");
#else
		msg_print("Mind-warping forces emanate from your brain!");
#endif

		(void)mindblast_monsters(randint1(pwis * 2 / 3));
		break;
	case 8:
	{
		int tx, ty;
		monster_type *m_ptr;
		monster_race *r_ptr;
		char m_name[80];

		if (!get_aim_dir(&dir)) return FALSE;
		range_restricted_target(dir, MAX_RANGE, &ty, &tx, TRUE);

		if (!cave[ty][tx].m_idx) break;
		if (!player_has_los_bold(ty, tx)) break;
		m_ptr = &m_list[cave[ty][tx].m_idx];
		r_ptr = &r_info[m_ptr->r_idx];
		monster_desc(m_name, m_ptr, 0);

		if (m_ptr->ml)
		{
			/* Hack -- auto-recall */
			monster_race_track(m_ptr->ap_r_idx);

			/* Hack - auto-track */
			health_track(cave[ty][tx].m_idx);
		}

		if (m_ptr->ap_r_idx != m_ptr->r_idx)
		{
			m_ptr->ap_r_idx = m_ptr->r_idx;
			lite_spot(m_ptr->fy, m_ptr->fx);
		}

		if (r_ptr->flags7 & RF7_EGG_ONLY)
		{
#ifdef JP
			msg_format("%sを完全調査することはできない。", m_name);
#else
			msg_format("You cannot research %s completely.", m_name);
#endif
		}
		else
		{
			/* Save the screen */
			screen_save();

			research_mon_aux(m_ptr->r_idx);
			inkey();

			/* Restore */
			screen_load();
		}

		break;
	}
	case 9:
	{
		object_type *o_ptr;
		char o_name[MAX_NLEN];
		int i;

		/* Scan objects */
		for (i = 1; i < o_max; i++)
		{
			object_type *o_ptr = &o_list[i];

			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;

			if (artifact_p(o_ptr) || o_ptr->art_name)
			{
				object_desc_store(o_name, o_ptr, FALSE, 0);
				msg_format("%s  ", o_name);
			}
		}
		break;
	}
	case 10:
		return alter_to_fate();
	case 11:
#ifdef JP
		msg_print("サイコメトリーを試みた。");
#else
		msg_print("You tried psychometry.");
#endif

		identify_pack(TRUE);
		(void)project(0, 1, py, px, 0, GF_STAR_IDENTIFY, PROJECT_ITEM, MODIFY_ELEM_MODE_NONE);
		break;
	case 12:
		(void)set_tim_immune_magic(damroll(10, 5), FALSE);
		break;
	case 13:
	{
		int tx, ty, rlev;
		int power = randint1(pwis);
		monster_type *m_ptr;
		monster_race *r_ptr;
		char m_name[80];

		if (!get_aim_dir(&dir)) return FALSE;
		range_restricted_target(dir, MAX_RANGE, &ty, &tx, TRUE);

		if (!cave[ty][tx].m_idx) break;
		if (!player_has_los_bold(ty, tx)) break;

		m_ptr = &m_list[cave[ty][tx].m_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		monster_desc(m_name, m_ptr, 0);
		m_ptr->csleep = 0;

		if (m_ptr->ml)
		{
			/* Hack -- auto-recall */
			monster_race_track(m_ptr->ap_r_idx);

			/* Hack - auto-track */
			health_track(cave[ty][tx].m_idx);
		}

		rlev = r_ptr->level;
		if (r_ptr->flags2 & RF2_SMART) rlev = rlev * 3 / 2;
		if (r_ptr->flags2 & RF2_STUPID) rlev /= 2;

		if (r_ptr->flags2 & RF2_EMPTY_MIND)
		{
			r_ptr->r_flags2 |= RF2_EMPTY_MIND;
#ifdef JP
			msg_format("%sには効果がなかった！", m_name);
#else
			msg_format("%s is unaffected!", m_name);
#endif
		}
		else if (!(r_ptr->flags2 & RF2_WEIRD_MIND) &&
		         (rlev > randint1(((power - 10) < 1) ? 1 : (power - 10)) + 10))
		{
#ifdef JP
			msg_format("%sはハックに抵抗した！", m_name);
#else
			msg_format("%s resists hacking!", m_name);
#endif
		}
		else
		{
			int times = randint1(10);
#ifdef JP
			msg_format("%sの精神をハックした。", m_name);
#else
			msg_format("You hacked the mind of %s.", m_name);
#endif
			for (; times; times--)
			{
				int energy = m_ptr->energy_need + ENERGY_NEED();
				if (energy > MAX_SHORT) energy = MAX_SHORT;
				m_ptr->energy_need = (s16b)energy;
			}
		}

		break;
	}
	case 14: /* Immune */
		return choose_the_immunity(12 + damroll(8, 2));
	default:
#ifdef JP
		msg_print("なに？");
#else
		msg_print("Zap?");
#endif
	}

	return TRUE;
}


/*
 * do_cmd_cast calls this function
 */
void do_cmd_mind(void)
{
	int             n = 0;
	int             chance;
	int             minfail = 0;
	int             plev = p_ptr->lev;
	mind_type       spell;
	bool            cast;
	int             hp_cost = 0, mana_cost = 0;

	/* not if confused */
	if (p_ptr->confused)
	{
#ifdef JP
msg_print("混乱していて集中できない！");
#else
		msg_print("You are too confused!");
#endif

		return;
	}

	/* get power */
	if (!get_mind_power(&n, FALSE)) return;

	spell = mind_powers.info[n];

	/* Spell failure chance */
	chance = spell.fail;

	mana_cost = spell.cost;

	if (mana_cost > p_ptr->csp)
	{
		mana_cost = p_ptr->csp;
		hp_cost = spell.cost - mana_cost;

		if (hp_cost > p_ptr->chp)
		{
#ifdef JP
			msg_print("ＨＰが足りません。");
#else
			msg_print("You do not have enough hp to use this power.");
#endif
			return;
		}
	}

	if (chance)
	{
		/* Reduce failure rate by "effective" level adjustment */
		chance -= 3 * (plev - spell.lev);

		/* Reduce failure rate by stat adjustment */
		chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[spell.stat]] - 1);

		/* Extract the minimum failure rate */
		minfail = adj_mag_fail[p_ptr->stat_ind[spell.stat]];

		/* Minimum failure rate */
		if (chance < minfail) chance = minfail;

		if (fool_effect_status & FOOL_STATUS_PLAYER) chance += 10;

		/* Stunning makes spells harder */
		if (p_ptr->stun > 200) chance += 40;
		else if (p_ptr->stun > 100) chance += 25;
		else if (p_ptr->stun) chance += 15;
	}

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("精神の集中に失敗した！");
#else
		msg_print("You failed to concentrate hard enough!");
#endif

		sound(SOUND_FAIL);
	}
	else
	{
		sound(SOUND_ZAP);

		cast = cast_mindcraft_spell(n);

		if (!cast) return;
	}


	/* Take a turn */
	energy_use = 100;

	/* Sufficient mana */
	if (mana_cost)
	{
		/* Use some mana */
		p_ptr->csp -= mana_cost;

		/* Limit */
		if (p_ptr->csp < 0) p_ptr->csp = 0;

		/* Redraw mana */
		p_ptr->redraw |= (PR_MANA);
	}

	if (hp_cost)
	{
#ifdef JP
		take_hit(DAMAGE_USELIFE, hp_cost, "過度の集中");
#else
		take_hit(DAMAGE_USELIFE, hp_cost, "concentrating too hard");
#endif
		/* Redraw hp */
		p_ptr->redraw |= (PR_HP);
	}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * do_cmd_cast calls this function
 */
void do_cmd_mind_browse(void)
{
	int n = 0;
	int j, line;
	char temp[62*5];

	screen_save();

	while(1)
	{
		/* get power */
		if (!get_mind_power(&n, TRUE))
		{
			screen_load();
			return;
		}

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(12, 22, 255);
		Term_erase(12, 21, 255);
		Term_erase(12, 20, 255);
		Term_erase(12, 19, 255);
		Term_erase(12, 18, 255);
		Term_erase(12, 17, 255);

		roff_to_buf(mind_tips[n], 62, temp, sizeof(temp));
		for(j=0, line = 18;temp[j];j+=(1+strlen(&temp[j])))
		{
			prt(&temp[j], line, 15);
			line++;
		}
	}
}


/*
 * Get new mindcraft powers.
 */
void learn_mindcraft_power(bool is_unlearned)
{
	int  learn_num = (is_unlearned ? 1 : 3);
	int  unlearned_num = 0;
	int  i;

	for (i = 0; i < MAX_MIND_POWERS; i++)
	{
		if (!(p_ptr->mindcraft_learned & (0x00000001 << i))) unlearned_num++;
	}
	if (!unlearned_num) return;
	if (learn_num > unlearned_num) learn_num = unlearned_num;

	/* Get 2 basic mindcraft powers */
	if (is_unlearned) p_ptr->mindcraft_learned |= (0x00000003);

	/* Get mindcraft powers */
	while (learn_num > 0)
	{
		for (i = 2; (i < MAX_MIND_POWERS) && (learn_num > 0); i++)
		{
			if (p_ptr->mindcraft_learned & (0x00000001 << i)) continue;
			if (one_in_(2)) continue;
			p_ptr->mindcraft_learned |= (0x00000001 << i);
			learn_num--;
		}
	}
}
