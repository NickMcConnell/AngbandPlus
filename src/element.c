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
#include "elementtips.h"

/* Mindcraft */
typedef struct element_type element_type;
struct element_type
{
	int     min_lev;
	int     mana_cost;
	int     elem_cost;
	int     fail;
	int     rad;
	u32b    use_element;
	u32b    weather_effect;
	cptr    name;
	cptr    elem_name[4];
};

typedef struct element_power element_power;
struct element_power
{
	element_type info[MAX_ELEMENT_POWERS];
};


element_power element_powers =
{
	{
		{  1,  1,  5, 10, 1, (ELEM_USE_SELF | ELEM_EFFECT_OPPOSE), 0, "%sの矢", {"火炎", "冷気", "酸", "電撃"}},
		{  2,  5, 15, 40, 1, (ELEM_USE_SELF | ELEM_EFFECT_OPPOSE), 0, "%sの球", {"火炎", "冷気", "酸", "電撃"}},
		{  7, 10, 20, 35, 1, (ELEM_USE_AQUA | ELEM_EFFECT_OPPOSE), (EFFECT_RAINY | EFFECT_CALM), "%sキュアミスト", {"", "", "", ""}},
		{  9, 10, 30, 55, 1, (ELEM_USE_SELF | ELEM_EFFECT_OPPOSE), 0, "%sの矢", {"プラズマ", "極寒", "石化", "遅鈍"}},
		{ 14, 12, 25, 40, 1, (ELEM_USE_EARTH | ELEM_EFFECT_OPPOSE), 0, "%sアースヒール", {"", "", "", ""}},
		{ 17, 15, 40, 30, 2, (ELEM_USE_SELF | ELEM_EFFECT_OPPOSE), 0, "%sのブレス", {"プラズマ", "極寒", "石化", "遅鈍"}},
		{ 24, 18, 35, 40, 2, (ELEM_USE_SELF | ELEM_EFFECT_OPPOSE), 0, "%sの精気の鎧", {"火", "水", "大地", "風"}},
		{ 25, 25, 50, 45, 2, (ELEM_USE_AQUA | ELEM_USE_WIND | ELEM_EFFECT_OPPOSE), (EFFECT_RAINY | EFFECT_WIND | EFFECT_COLD), "%sブリザード", {"", "", "", ""}},
		{ 30, 28, 50, 50, 3, (ELEM_USE_FIRE | ELEM_USE_WIND | ELEM_EFFECT_OPPOSE), (EFFECT_SUNNY | EFFECT_WIND), "%s太陽風", {"", "", "", ""}},
		{ 31, 30, 45, 40, 3, (ELEM_USE_AQUA | ELEM_USE_EARTH | ELEM_EFFECT_OPPOSE), 0, "%sエリクサー", {"", "", "", ""}},
		{ 32, 40, 55, 60, 3, (ELEM_USE_FIRE | ELEM_USE_EARTH | ELEM_EFFECT_OPPOSE), 0, "%sボルケーノ", {"", "", "", ""}},
		{ 36, 35, 60, 85, 4, (ELEM_USE_FIRE | ELEM_USE_AQUA | ELEM_USE_EARTH), 0, "%sメテオ", {"", "", "", ""}},
		{ 39, 60, 70, 85, 4, (ELEM_USE_FIRE | ELEM_USE_AQUA | ELEM_USE_EARTH | ELEM_USE_WIND), 0, "%sピュアエレメント", {"", "", "", ""}},
	}
};


static void element_info(char *p, int power)
{
	int plev = p_ptr->cexp_info[CLASS_ELEMENTALER].clev;

	strcpy(p, "");

	switch (power)
	{
	case  0: sprintf(p, " 損傷:%dd%d", 3 + ((plev - 1) / 5), 4); break;
	case  1: sprintf(p, " 損傷:%d", 55 + plev - 1); break;
	case  2: sprintf(p, " 回復:%dd%d", 2, 8); break;
	case  3: sprintf(p, " 損傷:%dd%d", 8 + ((plev - 5) / 4), 8); break;
	case  4: sprintf(p, " 回復:%d", 55 + plev); break;
	case  5: sprintf(p, " 損傷:%d", 100 + plev * 2); break;
	case  6: sprintf(p, " 期間:20 + d20"); break;
	case  7: sprintf(p, " 損傷:550"); break;
	case  8: sprintf(p, " 損傷:500"); break;
	case  9: sprintf(p, " 回復:500"); break;
	case 10: sprintf(p, " 損傷:%d", 55 + plev * 2); break;
	case 11: sprintf(p, " 損傷:%d", plev * 3); break;
	case 12: sprintf(p, " 損傷:%d", 300 + plev * 4); break;
	}
}

static s16b calc_mana_cost(int cost)
{
	int tmp_mana = cost;

	if (p_ptr->dec_mana) tmp_mana = tmp_mana * 3 / 4;
	if (tmp_mana < 1) tmp_mana = 1;

	return (tmp_mana);
}

/*
 * Drop 10+1d10 meteor ball at random places near the player
 */
static void cast_meteor(int dam, int rad)
{
	int i;
	int b = 10 + randint1(10);

	for (i = 0; i < b; i++)
	{
		int y, x;
		int count;

		for (count = 0; count <= 20; count++)
		{
			int dy, dx, d;

			x = px - 8 + randint0(17);
			y = py - 8 + randint0(17);

			dx = (px > x) ? (px - x) : (x - px);
			dy = (py > y) ? (py - y) : (y - py);

			/* Approximate distance */
			d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));

			if (d >= 9) continue;

			if (!in_bounds(y, x) || !projectable(py, px, y, x)) continue;

			/* Valid position */
			break;
		}

		if (count > 20) continue;

		project(0, rad, y, x, dam, GF_METEOR, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM, -1);
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
static int get_element_power(int *sn, bool only_browse)
{
	int             i;
	int             num = 0;
	int             y = 1;
	int             x = 10;
	int             minfail = 0;
	int             plev = p_ptr->cexp_info[CLASS_ELEMENTALER].clev;
	int             chance = 0;
	int             ask = TRUE;
	char            choice;
	char            out_val[160];
	char            spellname[80];
	char            comment[80];
	cptr            p;

	element_type       spell;
	element_power      *element_ptr;
	bool            flag, redraw;
	int menu_line = (use_menu ? 1 : 0);

#ifdef JP
	p = "祈り";
#else
	p = "pray";
#endif
	element_ptr = &element_powers;

	/* Assume cancelled */
	*sn = (-1);

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (element_ptr->info[*sn].min_lev <= plev)
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	for (i = 0; i < MAX_ELEMENT_POWERS; i++)
	{
		if (element_ptr->info[i].min_lev <= plev)
		{
	      num++;
		}
	}

	/* Build a prompt (accept all spells) */
	if (only_browse)
	{
#ifdef JP
		(void) strnfmt(out_val, 78, "(%^s %c-%c, '*'で一覧, ESC) どの%sについて知りますか？",
#else
		(void) strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
#endif
				       p, I2A(0), I2A(num - 1), p);
	}
	else
	{
#ifdef JP
		(void) strnfmt(out_val, 78, "(%^s %c-%c, '*'で一覧, ESC) どの%sを使いますか？",
#else
		(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
#endif
		p, I2A(0), I2A(num - 1), p);
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
				for (i = 0; i < MAX_ELEMENT_POWERS; i++)
				{
					int cost;

					/* Access the spell */
					spell = element_ptr->info[i];

					if (spell.min_lev > plev)   break;

					chance = spell.fail;

					cost = calc_mana_cost(spell.mana_cost);

					if (chance)
					{
						/* Reduce failure rate by "effective" level adjustment */
						chance -= 3 * (plev - spell.min_lev);

						/* Reduce failure rate by stat adjustment */
						chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

						/* Extract the minimum failure rate */
						minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

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
					element_info(comment, i);

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
						sprintf(psi_desc, "  %c) ",I2A(i));
					}

					/* Get spell name */
					sprintf(spellname, spell.name, spell.elem_name[get_cur_pelem()]);

					/* Dump the spell --(-- */
					strcat(psi_desc,
					       format("%-30s%2d %4d   %3d%%%s",
						      spellname, spell.min_lev, cost,
						      chance, comment));
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
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = element_ptr->info[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Get spell name */
			sprintf(spellname, spell.name, spell.elem_name[get_cur_pelem()]);

			/* Prompt */
#ifdef JP
			(void) strnfmt(tmp_val, 78, "%sを使いますか？", spellname);
#else
			(void)strnfmt(tmp_val, 78, "Use %s? ", spellname);
#endif


			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw && !only_browse) screen_load();

	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;

	repeat_push(*sn);

	/* Success */
	return (TRUE);
}


/*
 * do_cmd_cast calls this function
 */
static bool cast_element_spell(int spell)
{
	int dir;
	int tmp_dam, dam;
	int plev = p_ptr->cexp_info[CLASS_ELEMENTALER].clev;
	int rad = element_powers.info[spell].rad;
	u32b use_element = element_powers.info[spell].use_element;
	u32b weather_effect = element_powers.info[spell].weather_effect;
	int elem1, elem2;

	switch (get_cur_pelem())
	{
		case ELEM_FIRE:
			elem1 = GF_FIRE;
			elem2 = GF_PLASMA;
			break;
		case ELEM_AQUA:
			elem1 = GF_COLD;
			elem2 = GF_ICE;
			break;
		case ELEM_EARTH:
			elem1 = GF_ACID;
			elem2 = GF_STONE;
			break;
		case ELEM_WIND:
			elem1 = GF_ELEC;
			elem2 = GF_INERTIA;
			break;
	}

	/* spell code */
	switch (spell)
	{
	case 0: /* Detect Objects and Treasure */
		if (!get_aim_dir(&dir)) return FALSE;

		tmp_dam = damroll(3 + ((plev - 1) / 5), 4);
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		fire_bolt(elem1, dir, dam);  
		break;
	case 1:
		if (!get_aim_dir(&dir)) return FALSE;

		tmp_dam = 55 + plev;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		fire_ball(elem1, dir, dam, 2, FALSE);
		break;
	case 2:
		tmp_dam = damroll(2, 8);
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		(void)hp_player(dam);
		set_poisoned(0);
		set_cut(0);
		set_afraid(0);
		if (dam > 15) set_image(0);
		if (dam > 25) set_stoning(0);
		break;
	case 3:
		if (!get_aim_dir(&dir)) return FALSE;
		tmp_dam = damroll(8 + ((plev - 5) / 4), 8);
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		fire_bolt_or_beam(plev + 15, elem2, dir, dam);  
		break;
	case 4:
		tmp_dam = 55 + plev;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		(void)hp_player(dam);
		set_stun(0);
		set_cut(0);
		break;
	case 5:
		if (!get_aim_dir(&dir)) return FALSE;

		tmp_dam = 100 + plev * 2;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		fire_ball(elem2, dir, dam, -3, FALSE);
		break;
	case 6:
		tmp_dam = randint1(20) + 20;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		set_shield(dam, FALSE);
		if (plev > 29) set_magicdef(dam, FALSE);
		break;
	case 7:
		if (!get_aim_dir(&dir)) return FALSE;

		tmp_dam = 550;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		fire_ball(GF_ICE, dir, dam, -(plev / 15) - 3, TRUE);
		break;
	case 8:
		tmp_dam = 500;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		project_hack(GF_PLASMA, dam);
		confuse_monsters(50 + plev);
		stun_monsters(50 + plev);
		break;
	case 9:
		tmp_dam = 500;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		(void)hp_player(dam);
		set_stun(0);
		set_cut(0);
		set_poisoned(0);
		set_cut(0);
		set_afraid(0);
		set_image(0);
		set_stoning(0);
		break;
	case 10:
		if (!get_aim_dir(&dir)) return FALSE;

		tmp_dam = 55 + plev * 2;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		fire_ball_hide(GF_LAVA_FLOW, dir, 2 + randint1(2), 3, FALSE);
		fire_ball(GF_VOLCANIC_BOMB, dir, dam, 3, FALSE);
		break;
	case 11:
		tmp_dam = plev * 3;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		cast_meteor(dam, 2);
		break;
	case 12:
		if (!get_aim_dir(&dir)) return FALSE;

		tmp_dam = 300 + plev * 4;
		dam = mod_element_damage(tmp_dam, rad, use_element, weather_effect);
		fire_ball(GF_GODLY_SPEAR, dir, dam, 4, TRUE);
		break;
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
void do_cmd_element(void)
{
	int             n = 0;
	int             chance;
	int             minfail = 0;
	int             plev = p_ptr->cexp_info[CLASS_ELEMENTALER].clev;
	element_type    spell;
	bool            cast;
	int             mana_cost = 0, elem_cost = 0;

	if (get_cur_pelem() == NO_ELEM)
	{
#ifdef JP
		msg_print("エレメントの力が感じられない！");
#else
		msg_print("You feel no energy!");
#endif

		return;
	}

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
	if (!get_element_power(&n, FALSE)) return;

	spell = element_powers.info[n];

	/* Spell failure chance */
	chance = spell.fail;

	mana_cost = calc_mana_cost(spell.mana_cost);
	elem_cost = spell.elem_cost;

	if (mana_cost > p_ptr->csp)
	{
#ifdef JP
		msg_print("ＭＰが足りません。");
#else
		msg_print("You do not have enough mana to use this power.");
#endif
		if (!over_exert) return;

		/* Verify */
#ifdef JP
		if (!get_check("それでも挑戦しますか? ")) return;
#else
		if (!get_check("Attempt it anyway? ")) return;
#endif

	}

	if (chance)
	{
		/* Reduce failure rate by "effective" level adjustment */
		chance -= 3 * (plev - spell.min_lev);

		/* Reduce failure rate by stat adjustment */
		chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

		/* Not enough mana to cast */
		if (mana_cost > p_ptr->csp)
		{
			chance += 5 * (mana_cost - p_ptr->csp);
		}

		/* Extract the minimum failure rate */
		minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

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

		if (randint1(100) < (chance / 2))
		{
			/* Elemental storm */
#ifdef JP
			msg_print("元素の力が逆流してしまった！");
			take_hit(DAMAGE_LOSELIFE, damroll(1, elem_cost / 3), "元素エネルギーの逆流");
#else
			msg_print("Elemental power unleashes its power in an uncontrollable storm!");
			take_hit(DAMAGE_LOSELIFE, damroll(o_ptr->sval + 1, 6), "a miscast elemental spell");
#endif
			/* Redraw hp */
			p_ptr->redraw |= (PR_HP);
		}
	}
	else
	{
		sound(SOUND_ZAP);

		cast = cast_element_spell(n);

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

	if (elem_cost)
	{
		s16b pelem = get_cur_pelem();
		int rad = spell.rad;

		if (spell.use_element & ELEM_USE_SELF) inc_area_elem(0, pelem, -elem_cost, -rad, FALSE);
		if (spell.use_element & ELEM_USE_FIRE) inc_area_elem(0, ELEM_FIRE, -elem_cost, -rad, FALSE);
		if (spell.use_element & ELEM_USE_AQUA) inc_area_elem(0, ELEM_AQUA, -elem_cost, -rad, FALSE);
		if (spell.use_element & ELEM_USE_EARTH) inc_area_elem(0, ELEM_EARTH, -elem_cost, -rad, FALSE);
		if (spell.use_element & ELEM_USE_WIND) inc_area_elem(0, ELEM_WIND, -elem_cost, -rad, FALSE);

		if (p_ptr->action == ACTION_ELEMSCOPE) p_ptr->redraw |= (PR_MAP);
	}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * do_cmd_cast calls this function
 */
void do_cmd_element_browse(void)
{
	int n = 0;
	int j, line;
	char temp[62*5];

	screen_save();

	while(1)
	{
		/* get power */
		if (!get_element_power(&n, TRUE))
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

		roff_to_buf(element_tips[get_cur_pelem()][n], 62, temp, sizeof(temp));
		for(j=0, line = 18;temp[j];j+=(1+strlen(&temp[j])))
		{
			prt(&temp[j], line, 15);
			line++;
		}
	}
}


