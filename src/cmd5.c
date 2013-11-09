/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#include "spellstips.h"

cptr spell_categoly_name(int tval)
{
	switch (tval)
	{
#ifdef JP
	case TV_HOLY_BOOK:
		return "祈り";
	default:
		return "呪文";
#else
	case TV_HOLY_BOOK:
		return "prayer";
	default:
		return "spell";
#endif
	}
}

/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */

bool select_spellbook=FALSE;

static int get_spell(int *sn, cptr prompt, int sval, int use_realm)
{
	int         i;
	int         spell = -1;
	int         num = 0;
	int         ask = TRUE;
	int         use_mana;
	byte        spells[32];
	bool        flag, redraw, okay;
	char        choice;
	char        out_val[160];
	cptr        p;
#ifdef JP
	char jverb_buf[128];
#endif
	int menu_line = (use_menu ? 1 : 0);

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, use_realm))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	p = spell_categoly_name(mp_ptr->spell_book);

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[use_realm - 1][sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}

	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(spells[i], use_realm)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);
	if (!can_use_realm(use_realm)) return FALSE;
	if ((p_ptr->pclass == CLASS_DRAGOON) && ((use_realm) == REALM_WITCH) && (sval > 0)) return FALSE;
	if (((p_ptr->pclass == CLASS_WITCH) || (p_ptr->pclass == CLASS_ARCHMAGE)) && ((use_realm) == REALM_DEATH) && (sval > 0)) return FALSE;

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}

	/* Build a prompt (accept all spells) */
#ifdef JP
	jverb1( prompt, jverb_buf );
	(void) strnfmt(out_val, 78, "(%^s:%c-%c, '*'で一覧, ESCで中断) どの%sを%^sますか? ",
	        p, I2A(0), I2A(num - 1), p, jverb_buf );
#else
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
		p, I2A(0), I2A(num - 1), prompt, p);
#endif

	/* Get a spell from the user */

	choice = (always_show_list || use_menu) ? ESCAPE:1;
	while (!flag)
	{
		if( choice==ESCAPE ) choice = ' '; 
		else if( !get_com(out_val, &choice, TRUE) )break; 

		if (use_menu && choice != ' ')
		{
			switch(choice)
			{
				case '0':
				{
					screen_load();
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
			if (menu_line > num) menu_line -= num;
			/* Display a list of spells */
			print_spells(menu_line, spells, num, 1, 15, use_realm);
			if (ask) continue;
		}
		else
		{
			/* Request redraw */
			if ((choice == ' ') || (choice == '*') || (choice == '?'))
			{
				/* Show the list */
				if (!redraw)
				{
					/* Show list */
					redraw = TRUE;

					/* Save the screen */
					screen_save();

					/* Display a list of spells */
					print_spells(menu_line, spells, num, 1, 15, use_realm);
				}

				/* Hide the list */
				else
				{
					if (use_menu) continue;

					/* Hide list */
					redraw = FALSE;

					/* Restore the screen */
					screen_load();
				}

				/* Redo asking */
				continue;
			}


			/* Note verify */
			ask = (isupper(choice));

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
		spell = spells[i];

		/* Require "okay" spells */
		if (!spell_okay(spell, use_realm))
		{
			bell();
#ifdef JP
			msg_format("その%sを%sことはできません。", p, prompt);
#else
			msg_format("You may not %s that %s.", prompt, p);
#endif

			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			use_mana = calc_use_mana(spell, use_realm);

			/* Prompt */
#ifdef JP
			jverb1( prompt, jverb_buf );
			/* 英日切り替え機能に対応 */
			(void)strnfmt(tmp_val, 78, "%s(MP%d, 失敗率%d%%)を%sますか? ",
			              spell_names[use_realm-1][spell], use_mana,
			              spell_chance(spell, use_realm),jverb_buf);
#else
			(void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			              prompt, spell_names[use_realm-1][spell], use_mana,
			              spell_chance(spell, use_realm));
#endif


			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) screen_load();


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
	(*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


static bool item_tester_learn_spell(object_type *o_ptr)
{
	if ((o_ptr->tval < TV_MAGERY_BOOK) || (o_ptr->tval > (TV_MAGERY_BOOK + MAX_REALM - 1))) return (FALSE);
	if ((p_ptr->pclass == CLASS_DRAGOON) && (o_ptr->tval == TV_WITCH_BOOK))
		return (o_ptr->sval == 0);
	if (((p_ptr->pclass == CLASS_WITCH) || (p_ptr->pclass == CLASS_ARCHMAGE)) && (o_ptr->tval == TV_DEATH_BOOK))
		return (o_ptr->sval == 0);
	if (can_use_realm(tval2realm(o_ptr->tval))) return (TRUE);
	return (FALSE);
}


/*
 * Peruse the spells/prayers in a book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int		item, sval, use_realm = 0, j, line;
	int		spell = -1;
	int		num = 0;

	byte		spells[64];
	char		temp[62*4];

	object_type	*o_ptr;

	cptr q, s;

	/* Warriors are illiterate */
	if (!realm_choices[p_ptr->pclass])
	{
#ifdef JP
		msg_print("本を読むことができない！");
#else
		msg_print("You cannot read books!");
#endif

		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_learn_spell;

	/* Get an item */
#ifdef JP
	q = "どの本を読みますか? ";
#else
	q = "Browse which book? ";
#endif

#ifdef JP
	s = "読める本がない。";
#else
	s = "You have no books that you can read.";
#endif

	select_spellbook = TRUE;
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
	{
		select_spellbook = FALSE;
		return;
	}
	select_spellbook = FALSE;

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

	/* Access the item's sval */
	sval = o_ptr->sval;

	use_realm = tval2realm(o_ptr->tval);

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[use_realm - 1][sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save the screen */
	screen_save();

	/* Clear the top line */
	prt("", 0, 0);

	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
#ifdef JP
		if (!get_spell(&spell, "読む", o_ptr->sval, use_realm))
#else
		if (!get_spell(&spell, "browse", o_ptr->sval, use_realm))
#endif
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Display a list of spells */
			print_spells(0, spells, num, 1, 15, use_realm);

			/* Notify that there's nothing to see, and wait. */
#ifdef JP
			prt("読める呪文がない。", 0, 0);
#else
			prt("No spells to browse.", 0, 0);
#endif
			(void)inkey();
			

			/* Restore the screen */
			screen_load();

			return;
		}				  

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(14, 21, 255);
		Term_erase(14, 20, 255);
		Term_erase(14, 19, 255);
		Term_erase(14, 18, 255);

		roff_to_buf(spell_tips[use_realm-1][spell], 62, temp, sizeof temp);
		for(j=0, line = 18;temp[j];j+=(1+strlen(&temp[j])))
		{
			prt(&temp[j], line, 15);
			line++;
		}
	}

	/* Restore the screen */
	screen_load();
}


static bool cast_magery_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_MAGERY]/10;

	switch (spell)
	{
	case 0: /* Magic Missile */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_bolt(GF_MISSILE, dir, damroll(3 + ((clev - 1) / 5), 4));
		break;
	case 1: /* Detect Monsters */
		(void)detect_monsters_normal(DETECT_RAD_DEFAULT);
		break;
	case 2: /* Phase Door */
		teleport_player(10);
		break;
	case 3: /* Detect Doors and Traps */
		(void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
		(void)detect_doors(DETECT_RAD_DEFAULT);
		(void)detect_stairs(DETECT_RAD_DEFAULT);
		break;
	case 4: /* Light Area */
		(void)lite_area(damroll(2, (clev / 2)), (clev / 10) + 1);
		break;
	case 5: /* Trap & Door Destruction */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)destroy_door(dir);
		break;
	case 6: /* Teleport Self */
		teleport_player(clev * 5);
		break;
	case 7: /* Manaburst */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_MISSILE, dir,
			  (damroll(3, 5) + clev +
			   (clev / (((p_ptr->pclass == CLASS_WIZARD) || (p_ptr->pclass == CLASS_SIRENE) || (p_ptr->pclass == CLASS_LICH) ||(p_ptr->pclass == CLASS_ARCHMAGE)) ? 2 : 4))),
			   (clev < 30) ? 2 : 3, FALSE);
			/* Shouldn't actually use GF_MANA, as it will destroy all
			 * items on the floor */
		break;
	case 8: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wall_to_mud(dir);
		break;
	case 9: /* Magic Mapping */
		map_area(DETECT_RAD_MAP);
		break;
	case 10: /* Identify */
		if (clev < 35)
		{
			return ident_spell(FALSE);
		}
		else
		{
			return identify_fully(FALSE);
		}
	case 11: /* Sense Minds */
		(void)set_tim_esp(randint1(30) + 25, FALSE);
		break;
	case 12: /* Mana Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_bolt(GF_MANA, dir, damroll(11 + ((clev - 5) / 4), 8));
		break;
	case 13: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 14: /* Word of Recall */
		word_of_recall();
		break;
	case 15: /* Probing */
		probing();
		break;
	case 16: /* Recharging */
		if ((p_ptr->pclass == CLASS_WITCH) || (p_ptr->pclass == CLASS_HIGHWITCH)) return recharge(clev * 8);
		else return recharge(clev * 4);
	case 17: /* Teleport Level */
		(void)teleport_level(0);
		break;
	case 18: /* Teleport Other */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_AWAY_ALL, dir, clev);
		break;
	case 19: /* Detection */
		(void)detect_all(DETECT_RAD_DEFAULT);
		break;
	case 20: /* Magic Rocket */
		if (!get_aim_dir(&dir)) return FALSE;

#ifdef JP
		msg_print("ロケット発射！");
#else
		msg_print("You launch a rocket!");
#endif

		fire_rocket(GF_ROCKET, dir, 120 + clev, 2, FALSE);
		break;
	case 21: /* Clairvoyance */
		wiz_lite(FALSE);
		if (!(p_ptr->telepathy))
		{
			(void)set_tim_esp(randint1(30) + 25, FALSE);
		}
		break;
	case 22: /* Summon monster, demon */
		{
			u32b mode = 0L;
			bool pet = !one_in_(3);

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_NO_PET | PM_IGNORE_AMGRID);

			if (summon_specific((pet ? -1 : 0), py, px, (clev * 3) / 2, SUMMON_DEMON, mode))
			{
#ifdef JP
				msg_print("硫黄の悪臭が充満した。");
#else
				msg_print("The area fills with a stench of sulphur and brimstone.");
#endif


				if (pet)
#ifdef JP
					msg_print("「ご用でございますか、ご主人様」");
#else
					msg_print("'What is thy bidding... Master?'");
#endif

				else
#ifdef JP
					msg_print("「卑しき者よ、我は汝の下僕にあらず！ お前の魂を頂くぞ！」");
#else
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
#endif

			}
			break;
		}
	case 23: /* Teleport to town */
		return tele_town(TRUE);
	case 24: /* Summon Ancient Dragon */
		if (!summon_specific(-1, py, px, clev * 2 / 3 + randint1(clev/2), SUMMON_HI_DRAGON, PM_FORCE_PET))
		{
#ifdef JP
			msg_print("古代ドラゴンは現れなかった。");
#else
			msg_print("No ancient dragons arrive.");
#endif
		}
		break;
	case 25: /* Mana Storm */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_MANA, dir, 300 + (clev * 4), 4, FALSE);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な魔道の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Magery spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_fire_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_FIRE]/10;
	int pstat = p_ptr->stat_use[A_INT];
	int k;

	switch (spell)
	{
	case 0:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_beam(GF_PLASMA, dir, damroll(3 + ((clev - 1) / 5), 4));
		break;
	case 1:
		k = 2;
		if (pstat >= (18 + 100)) k++;
		if (pstat >= (18 + 150)) k++;
		if (pstat >= (18 + 200)) k++;
		inc_area_elem(0, ELEM_FIRE, k, (clev / 10) + 1, TRUE);
		break;
	case 2:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_bolt(GF_FIRE, dir, damroll(8 + ((clev - 5) / 4), 8));
		break;
	case 3:
		if (!get_aim_dir(&dir)) return FALSE;
		if (pstat >= (18 + 150)) fire_ball(GF_STASIS, dir, clev + 70, 3, FALSE);
		else if (pstat >= (18 + 100)) fire_ball(GF_STASIS, dir, clev + 60, 2, FALSE);
		else fire_ball(GF_STASIS, dir, clev + 50, 1, FALSE);
		break;
	case 4:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_FIRE, dir, 10 + clev * 2 + randint1(clev),
			(pstat >= (18 + 150)) ? 3 : 2, TRUE);
		break;
	case 5:
		msg_print("天候が穏やかになっていく...");
		k = -2;
		if (pstat >= (18 + 150)) k--;
		if (pstat >= (18 + 200)) k--;
		set_weather(k, k, k);
		break;
	case 6:
		brand_weapon(1);
		break;
	case 7:
		(void)set_tim_res_time(randint1(20)+20, FALSE);
		break;
	case 8:
		if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
		msg_format("精霊サラマンダーを召喚した。");
#else
		msg_format("You called the Salamander.");
#endif
		cast_call_the_elemental(GF_FIRE, dir, clev, 1, clev, 3, A_INT);
		break;
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_PLASMA, dir, MIN(p_ptr->chp / 2, 350), -2, FALSE);
		break;
	case 10:
		{
			int x, y, tx, ty;
			int dir, i;
			int b = 5 + randint1(10);

			if (!get_aim_dir(&dir)) return FALSE;
			range_restricted_target(dir, MAX_RANGE, &ty, &tx, TRUE);

			for (i = 0; i < b; i++)
			{
				int count = 20, d = 0;

				while (count--)
				{
					int dx, dy;

					x = tx - 5 + randint0(11);
					y = ty - 5 + randint0(11);

					dx = (tx > x) ? (tx - x) : (x - tx);
					dy = (ty > y) ? (ty - y) : (y - ty);

					/* Approximate distance */
					d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));
					/* Within the radius */
					if (d < 5) break;
				}

				if (count < 0) continue;

				/* Cannot penetrate walls */
				if (!in_bounds(y, x) || !los(ty, tx, y, x)) continue;

				project(0, 2, y, x, clev * 3 + 25, GF_PURE_FIRE, PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
			}
		}
		break;
	case 11:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_STRIKE_NOVA, dir, 999, 0, FALSE);
		break;
	case 12:
		set_zoshonel_protect(randint1(10) + 10, FALSE);
		break;
	case 13:
		project(0, MAX_SIGHT, py, px, clev * 8 + randint1(clev * 5), GF_PLASMA, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "スーパーノヴァの呪文の巻き添え");
#else
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "the strain of casting Super Nova");
#endif
		set_weather(-15, 0, -15);
		change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
		change_chaos_frame(ETHNICITY_GARGASTAN, -1);
		change_chaos_frame(ETHNICITY_BACRUM, -1);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な火炎の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Fire spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_aqua_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_AQUA]/10;
	int pstat = p_ptr->stat_use[A_INT];
	int k;

	switch (spell)
	{
	case 0:
		k = 2;
		if (pstat >= (18 + 100)) k++;
		if (pstat >= (18 + 150)) k++;
		if (pstat >= (18 + 200)) k++;
		inc_area_elem(0, ELEM_AQUA, k, (clev / 10) + 1, TRUE);
		break;
	case 1:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_bolt(GF_ICE, dir, damroll(6 + ((clev - 5) / 4), 8));
		break;
	case 2:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_COLD, dir, 10 + clev * 2 + randint1(clev),
			(pstat >= (18 + 150)) ? 3 : 2, TRUE);
		break;
	case 3:
		(void)set_poisoned(0);
		(void)set_cut(0);
		(void)hp_player(damroll(4, 8));
		break;
	case 4:
		if (!get_aim_dir(&dir)) return FALSE;
		(void)slow_monster(dir, clev);
		break;
	case 5:
		(void)set_tim_sh_cold(randint1(25 + clev) + clev, FALSE);
		break;
	case 6:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_WATER, dir, 100 + clev * 3 / 2, (clev / 12) + 1, FALSE);
		break;
	case 7:
		project(0, clev / 10, py, px, 15 + clev / 2, GF_WATER, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		project(0, clev / 10, py, px, 15 + clev / 2, GF_POIS, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		project(0, clev / 10, py, px, 15 + clev / 2, GF_CONFUSION, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		if (pstat >= (18 + 150))
			project(0, clev / 10, py, px, 15 + clev / 2, GF_DISENCHANT, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		if (pstat >= (18 + 200))
			project(0, clev / 10, py, px, 15 + clev / 2, GF_OLD_DRAIN, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		break;
	case 8:
		return aqua_diving(clev);
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
		msg_format("精霊フェンリルを召喚した。");
#else
		msg_format("You called the Fenrer.");
#endif
		cast_call_the_elemental(GF_COLD, dir, clev, 1, clev, 3, A_INT);
		break;
	case 10:
		(void)hp_player(500);
		project(0, 3, py, px, 500, GF_OLD_HEAL, PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_NONE);
		(void)set_poisoned(0);
		(void)set_confused(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 11:
		return alter_with_flood();
	case 12:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_WATER, dir, damroll(10, clev), 0, FALSE);
		fire_ball(GF_DRAIN_MANA, dir, damroll(2, clev), 0, FALSE);
		break;
	case 13:
		project(0, MAX_SIGHT, py, px, clev * 8 + randint1(clev * 5), GF_ICE, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "アイスレクイエムの呪文の巻き添え");
#else
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "the strain of casting Ice Requiem");
#endif
		set_weather(15, 0, 15);
		change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
		change_chaos_frame(ETHNICITY_GARGASTAN, -1);
		change_chaos_frame(ETHNICITY_BACRUM, -1);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な水の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Aqua spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_earth_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_EARTH]/10;
	int pstat = p_ptr->stat_use[A_INT];
	int k;

	switch (spell)
	{
	case 0:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_ACID, dir, damroll(3 + ((clev - 1) / 5), 4), 0, FALSE);
		if (one_in_(3)) fire_ball(GF_STASIS, dir, 50 + clev, 0, FALSE);
		break;
	case 1:
		k = 2;
		if (pstat >= (18 + 100)) k++;
		if (pstat >= (18 + 150)) k++;
		if (pstat >= (18 + 200)) k++;
		inc_area_elem(0, ELEM_EARTH, k, (clev / 10) + 1, TRUE);
		break;
	case 2:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_bolt(GF_ACID, dir, damroll(8 + ((clev - 5) / 4), 8));
		break;
	case 3:
		(void)tree_creation();
		break;
	case 4:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_ACID, dir, 10 + clev * 2 + randint1(clev),
			(pstat >= (18 + 150)) ? 3 : 2, TRUE);
		break;
	case 5:
		return jump_wall();
	case 6:
		(void)set_shield(clev / 2 + randint1(clev / 2), FALSE);
		break;
	case 7:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_beam(GF_ACID, dir, damroll(10, clev / 3));
		fire_beam(GF_SHARDS, dir, damroll(10, clev / 3));
		if (pstat >= (18 + 200)) fire_beam(GF_NEW_DRAIN, dir, damroll(2, clev));
		break;
	case 8:
		set_earth_spike(6 + randint1(6), FALSE);
		break;
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
		msg_format("精霊ノームを召喚した。");
#else
		msg_format("You called the Gnome.");
#endif
		cast_call_the_elemental(GF_ACID, dir, clev, 1, clev, 3, A_INT);
		break;
	case 10:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_METEOR, dir, 100 + clev + randint1(clev * 2), 2, TRUE);
		break;
	case 11:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_STONE, dir, MIN(p_ptr->chp / 2, 350), -2, FALSE);
		break;
	case 12:
		project(0, MAX_SIGHT, py, px, clev * 8 + randint1(clev * 5), GF_SHARDS, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "アースクェイクの呪文の巻き添え");
#else
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "the strain of casting Earthquake");
#endif
		change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
		change_chaos_frame(ETHNICITY_GARGASTAN, -1);
		change_chaos_frame(ETHNICITY_BACRUM, -1);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な大地の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Earth spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_wind_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_WIND]/10;
	int pstat = p_ptr->stat_use[A_INT];
	int k;

	switch (spell)
	{
	case 0:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_beam(GF_SOUND, dir, damroll(3 + ((clev - 1) / 5), 4));
		break;
	case 1:
		k = 2;
		if (pstat >= (18 + 100)) k++;
		if (pstat >= (18 + 150)) k++;
		if (pstat >= (18 + 200)) k++;
		inc_area_elem(0, ELEM_WIND, k, (clev / 10) + 1, TRUE);
		break;
	case 2:
		set_wind_guard(randint1(clev) + 20, FALSE);
		break;
	case 3:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_bolt(GF_ELEC, dir, damroll(8 + ((clev - 5) / 4), 8));
		break;
	case 4:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_ELEC, dir, 10 + clev * 2 + randint1(clev),
			(pstat >= (18 + 150)) ? 3 : 2, TRUE);
		break;
	case 5:
		(void)set_fast(randint1(25 + clev) + clev, FALSE);
		break;
	case 6:
		msg_print("天候が荒れていく...");
		k = 2;
		if (pstat >= (18 + 150)) k++;
		if (pstat >= (18 + 200)) k++;
		set_weather(k, k, k);
		break;
	case 7:
		project(0, clev / 10, py, px, 40 + clev / 2, GF_PURE_WIND, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM, MODIFY_ELEM_MODE_MAGIC);
		project(0, clev / 10, py, px, 20 + clev / 2, GF_SOUND, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM, MODIFY_ELEM_MODE_MAGIC);
		project(0, clev / 10, py, px, 20 + clev / 2, GF_GRAVITY, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM, MODIFY_ELEM_MODE_MAGIC);
		break;
	case 8:
		if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
		msg_format("精霊サンダーバードを召喚した。");
#else
		msg_format("You called the Thunderbird.");
#endif
		cast_call_the_elemental(GF_ELEC, dir, clev, 1, clev, 3, A_INT);
		break;
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_SOUND, dir, MIN(p_ptr->chp / 2, 350), -2, FALSE);
		break;
	case 10: /* Dimension Door */
#ifdef JP
		msg_print("次元の扉が開いた。目的地を選んで下さい。");
#else
		msg_print("You open a dimensional gate. Choose a destination.");
#endif
		return dimension_door(clev);
	case 11:
		project(0, MAX_SIGHT, py, px, clev * 8 + randint1(clev * 5), GF_SOUND, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "エアリアルクライの呪文の巻き添え");
#else
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "the strain of casting Aerial Cry");
#endif
		set_weather(0, 15, 0);
		change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
		change_chaos_frame(ETHNICITY_GARGASTAN, -1);
		change_chaos_frame(ETHNICITY_BACRUM, -1);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な風の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Wind spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_holy_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_HOLY]/10;

	switch (spell)
	{
	case 0: /* Detect Evil */
		(void)detect_monsters_evil(DETECT_RAD_DEFAULT);
		break;
	case 1: /* Bless */
		(void)set_blessed(randint1(12) + 12, FALSE);
		break;
	case 2: /* Call Light */
		(void)lite_area(damroll(2, (clev / 2)), (clev / 10) + 1);
		break;
	case 3: /* Cure Critical Wounds */
		(void)hp_player(damroll(8, 10));
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 4: /* Detect Traps + Secret Doors */
		(void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
		(void)detect_doors(DETECT_RAD_DEFAULT);
		(void)detect_stairs(DETECT_RAD_DEFAULT);
		break;
	case 5: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 6: /* Cure Poison */
		(void)set_poisoned(0);
		break;
	case 7: /* Remove Curse */
		if (clev < 40)
		{
			if (remove_curse())
			{
#ifdef JP
				msg_print("誰かに見守られているような気がする。");
#else
				msg_print("You feel as if someone is watching over you.");
#endif
			}
		}
		else
		{
			if (remove_all_curse())
			{
#ifdef JP
				msg_print("誰かに見守られているような気がする。");
#else
				msg_print("You feel as if someone is watching over you.");
#endif
			}
		}
		break;
	case 8: /* Nature Awareness -- downgraded */
		map_area(DETECT_RAD_MAP);
		(void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
		(void)detect_doors(DETECT_RAD_DEFAULT);
		(void)detect_stairs(DETECT_RAD_DEFAULT);
		(void)detect_monsters_normal(DETECT_RAD_DEFAULT);
		break;
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_beam(GF_GODLY_SPEAR, dir, 100 + randint1(clev * 2));
		break;
	case 10: /* Glyph of Warding */
		warding_glyph();
		break;
	case 11: /* Healing True */
		(void)hp_player(1000);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 12:
		(void)set_poisoned(0);
		(void)set_cut(0);
		(void)set_afraid(0);
		(void)set_image(0);
		(void)set_shero(0, TRUE);
		(void)set_stoning(0);
		break;
	case 13: /* Protection from Evil */
		(void)set_protevil(randint1(25) + 3 * clev, FALSE);
		break;
	case 14:
		(void)hp_player(300);
		project(0, 2, py, px, 300, GF_OLD_HEAL, PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_NONE);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 15: /* Resistance */
		(void)set_oppose_acid(randint1(20) + 20, FALSE);
		(void)set_oppose_elec(randint1(20) + 20, FALSE);
		(void)set_oppose_fire(randint1(20) + 20, FALSE);
		(void)set_oppose_cold(randint1(20) + 20, FALSE);
		(void)set_oppose_pois(randint1(20) + 20, FALSE);
		break;
	case 16: /* Charm Monsters */
		charm_monsters(clev + 50);
		break;
	case 17: /* Restoration */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		break;
	case 18: /* Clairvoyance */
		wiz_lite(FALSE);
		break;
	case 19:
		if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
		msg_format("精霊イグニスファタスを召喚した。");
#else
		msg_format("You called the Ignis Fatuus.");
#endif
		cast_call_the_elemental(GF_HOLY_FIRE, dir, clev, 1, clev, 1, A_WIS);
		break;
	case 20: /* Summon monster, angel */
		{
			bool pet = !one_in_(3);
			u32b mode = 0L;

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_NO_PET | PM_IGNORE_AMGRID);
			if (!(pet && (clev < 50))) mode |= PM_ALLOW_GROUP;

			if (summon_specific((pet ? -1 : 0), py, px, (clev * 3) / 2, SUMMON_ANGEL, mode))
			{
				if (pet)
#ifdef JP
					msg_print("「ご用でございますか、ご主人様」");
#else
					msg_print("'What is thy bidding... Master?'");
#endif

				else
#ifdef JP
					msg_print("「我は汝の下僕にあらず！ 悪行者よ、悔い改めよ！」");
#else
					msg_print("Mortal! Repent of thy impiousness.");
#endif

			}
			break;
		}
	case 21: /* Warding True */
		warding_glyph();
		glyph_creation();
		break;
	case 22:
		hp_player(5000);
		(void)set_poisoned(0);
		(void)set_cut(0);
		(void)set_afraid(0);
		(void)set_shero(0, TRUE);
		(void)set_image(0);
		(void)set_stoning(0);
		break;
	case 23:
		set_tim_resurrection(6 + randint1(6), FALSE);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な神聖の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Holy spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_death_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_DEATH]/10;
	int	dummy = 0;
	int pstat;
	int k;

	switch (spell)
	{
	case 0: /* Enslave Undead */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)control_one_undead(dir, clev + 50);
		break;
	case 1:
		(void)set_blind(0);
		(void)set_poisoned(0);
		(void)set_confused(0);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_image(0);
		break;
	case 2:
		return get_energy_from_corpse();
	case 3: /* Evil Orb */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_HELL_FIRE, dir, damroll(3, 6) + clev + clev / 2,
			(clev < 30) ? 2 : 3, FALSE);
		break;
	case 4:
		(void)set_chargespell(randint1(clev + 25) + clev, FALSE);
		break;
	case 5:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_DARK, dir, 30 + clev + damroll(4 + ((clev - 10) / 5), 4), 0, FALSE);
		if (!one_in_(3)) fire_ball_hide(GF_OLD_SLEEP, dir, clev + 50, 0, FALSE);
		break;
	case 6:
		project_hack(GF_GRAVITY, damroll(clev / 5 + 10, 10));
		break;
	case 7: /* Genocide One */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball_hide(GF_GENOCIDE, dir, clev + 50, 0, FALSE);
		break;
	case 8: /* Raise the Dead */
		{
			int type;
			bool pet = one_in_(3);
			u32b mode = 0L;

			type = (clev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);

			if (!pet || (pet && (clev >= 24) && one_in_(3)))
				mode |= PM_ALLOW_GROUP;

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID);

			if (summon_specific((pet ? -1 : 0), py, px, (clev * 3) / 2, type, mode))
			{
#ifdef JP
				msg_print("冷たい風があなたの周りに吹き始めた。それは腐敗臭を運んでいる...");
#else
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
#endif


				if (pet)
#ifdef JP
					msg_print("古えの死せる者共があなたに仕えるため土から甦った！");
#else
					msg_print("Ancient, long-dead forms arise from the ground to serve you!");
#endif

				else
#ifdef JP
					msg_print("死者が甦った。眠りを妨げるあなたを罰するために！");
#else
					msg_print("'The dead arise... to punish you for disturbing them!'");
#endif

			}

			break;
		}
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;
		/* Word of Pain, no-drain mode */
		fire_ball(GF_WORD_OF_PAIN, dir, 0, 0, FALSE);
		break;
	case 10: /* Word of Destruction */
		destroy_area(py, px, 13+randint0(5));
		break;
	case 11: /* Animate Dead */
		animate_dead(0, py, px);
		break;
	case 12: /* True Discharge Minion */
		discharge_minion();
		break;
	case 13: /* Vampiric Branding */
		brand_weapon(4);
		break;
	case 14:
		k = -4;
		pstat = p_ptr->stat_use[mp_ptr->spell_stat];
		if (pstat >= (18 + 100)) k -= 2;
		if (pstat >= (18 + 150)) k -= 2;
		if (pstat >= (18 + 200)) k -= 2;
		for (dummy = MIN_ELEM; dummy < ELEM_NUM; dummy++)
		{
			if (dummy == get_cur_pelem()) continue;
			inc_area_elem(0, dummy, k, -((clev / 10) + 1), FALSE);
		}
		break;
	case 15: /* Vampirism True */
		if (!get_aim_dir(&dir)) return FALSE;

		for (dummy = 0; dummy < 3; dummy++)
			fire_ball(GF_NEW_DRAIN, dir, 100, 0, FALSE);
		break;
	case 16:
		if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
		msg_format("精霊ファントムを召喚した。");
#else
		msg_format("You called the Phantom.");
#endif
		cast_call_the_elemental(GF_HELL_FIRE, dir, clev, 1, clev, 1, A_WIS);
		break;
	case 17:
		project_hack_living(GF_HELL_FIRE, clev * 4);
		project_hack_living(GF_OLD_SLEEP, clev + 50);
		break;
	case 18: /* Genocide */
		(void)symbol_genocide(clev+50, TRUE);
		break;
	case 19: /* Darkness Storm */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_DARK, dir, 100 + clev * 4, 4, FALSE);
		break;
	case 20:
		if (!buki_motteruka(INVEN_RARM))
		{
#ifdef JP
			msg_format("利き腕に武器を持たないと闇の破神剣は使えない。");
#else
			msg_format("You cannot use evil weapon with no main weapon.");
#endif
			return FALSE;
		}
		(void)set_evil_weapon(randint1(clev / 2) + (clev / 2), FALSE, INVEN_RARM, FALSE);
		break;
	case 21: /* Mass Genocide */
		(void)mass_genocide(clev+50, TRUE);
		break;
	case 22: /* Hellfire */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_HELL_FIRE, dir, 666, 3, FALSE);
#ifdef JP
take_hit(DAMAGE_USELIFE, 20 + randint1(30), "地獄の劫火の呪文を唱えた疲労");
#else
		take_hit(DAMAGE_USELIFE, 20 + randint1(30), "the strain of casting Hellfire");
#endif

		break;
	case 23:
		if (stop_the_time_player)
		{
#ifdef JP
			msg_print("既に時は止まっている。");
#else
			msg_print("Time is already stopped.");
#endif
			return (FALSE);
		}
		stop_the_time_player = TRUE;
#ifdef JP
		msg_print("時を止めた！");
#else
		msg_print("You stopped the time!");
#endif
		msg_print(NULL);

		/* Hack */
		p_ptr->energy_need -= 1000 + (100 * (4 + damroll(2, 2)))*TURNS_PER_TICK/10;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Update monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

		handle_stuff();
		break;
	case 24:
		project(0, MAX_SIGHT, py, px, clev * 8 + randint1(clev * 5), GF_DARK, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "デッドスクリームの呪文の巻き添え");
#else
		take_hit(DAMAGE_NOESCAPE, clev + randint1(clev), "the strain of casting Dead Scream");
#endif
		change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
		change_chaos_frame(ETHNICITY_GARGASTAN, -1);
		change_chaos_frame(ETHNICITY_BACRUM, -1);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な暗黒の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Death spell: %d.", spell);
#endif
		msg_print(NULL);
	}

	return TRUE;
}


static bool ang_sort_comp_pet(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	int w1 = who[a];
	int w2 = who[b];

	monster_type *m_ptr1 = &m_list[w1];
	monster_type *m_ptr2 = &m_list[w2];
	monster_race *r_ptr1 = &r_info[m_ptr1->r_idx];
	monster_race *r_ptr2 = &r_info[m_ptr2->r_idx];

	/* Unused */
	(void)v;

	if (m_ptr1->nickname && !m_ptr2->nickname) return TRUE;
	if (m_ptr2->nickname && !m_ptr1->nickname) return FALSE;

	if ((r_ptr1->flags1 & RF1_UNIQUE) && !(r_ptr2->flags1 & RF1_UNIQUE)) return TRUE;
	if ((r_ptr2->flags1 & RF1_UNIQUE) && !(r_ptr1->flags1 & RF1_UNIQUE)) return FALSE;

	if (r_ptr1->level > r_ptr2->level) return TRUE;
	if (r_ptr2->level > r_ptr1->level) return FALSE;

	if (m_ptr1->hp > m_ptr2->hp) return TRUE;
	if (m_ptr2->hp > m_ptr1->hp) return FALSE;
	
	return w1 <= w2;
}


static bool cast_symbiotic_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_SYMBIOTIC]/10;

	switch (spell)
	{
	case 0:
		(void)detect_monsters_normal(DETECT_RAD_DEFAULT);
		break;
	case 1: /* Animal Taming */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)charm_animal(dir, clev);
		break;
	case 2:
		{
			bool old_target_pet = target_pet;
			if (!get_aim_dir(&dir))
			{
				target_pet = old_target_pet;
				return FALSE;
			}
			target_pet = old_target_pet;
			fire_ball(GF_OLD_HEAL, dir, damroll(4, 6), 0, FALSE);
		}
		break;
	case 3:
		slow_monsters(clev);
		break;
	case 4:
		{
			object_type *q_ptr;
			object_type forge;

			/* Get local object */
			q_ptr = &forge;

			/* Create the food ration */
			object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));

			/* Drop the object from heaven */
			(void)drop_near(q_ptr, -1, py, px);
#ifdef JP
			msg_print("食料を生成した。");
#else
			msg_print("You make some food.");
#endif
		}
		break;
	case 5:
		{
			int pet_ctr, i;
			u16b *who;
			int max_pet = 0;
			u16b dummy_why;

			/* Allocate the "who" array */
			C_MAKE(who, max_m_idx, u16b);

			/* Process the monsters (backwards) */
			for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
			{
				if (is_pet(&m_list[pet_ctr]) && (p_ptr->riding != pet_ctr))
				  who[max_pet++] = pet_ctr;
			}

			/* Select the sort method */
			ang_sort_comp = ang_sort_comp_pet;
			ang_sort_swap = ang_sort_swap_hook;

			ang_sort(who, &dummy_why, max_pet);

			/* Process the monsters (backwards) */
			for (i = 0; i < max_pet; i++)
			{
				pet_ctr = who[i];
				teleport_to(pet_ctr, py, px, 2, 100, TRUE);
			}

			/* Free the "who" array */
			C_KILL(who, max_m_idx, u16b);
		}
		break;
	case 6:
		summon_kin_type = 'q';
		if (!summon_specific(-1, py, px, clev * 2 / 3 + randint1(clev/2), SUMMON_KIN, PM_FORCE_PET))
		{
#ifdef JP
			msg_print("動物は現れなかった。");
#else
			msg_print("No animals arrive.");
#endif
		}
		break;
	case 7:
		map_area(DETECT_RAD_MAP);
		(void)detect_all(DETECT_RAD_DEFAULT);
		break;
	case 8: /* Speed Monster */
		{
			bool old_target_pet = target_pet;
			target_pet = TRUE;
			if (!get_aim_dir(&dir))
			{
				target_pet = old_target_pet;
				return FALSE;
			}
			target_pet = old_target_pet;
			(void)speed_monster(dir, clev);
		}
		break;
	case 9: /* Animal Friendship */
		(void)charm_animals(clev + 50);
		break;
	case 10:
		num_repro += MAX_REPRO;
		break;
	case 11:
		{
			u32b mode = PM_ALLOW_GROUP | PM_FORCE_PET;
			if (!summon_specific(-1, py, px, clev * 2 / 3 + randint1(clev/2), SUMMON_HOUND, mode))
			{
#ifdef JP
				msg_print("ハウンドは現れなかった。");
#else
				msg_print("No hounds arrive.");
#endif
			}
		}
		break;
	case 12:
		{
			int     i, x, y;
			u32b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
			monster_type *m_ptr;

			/* Affect all (nearby) monsters */
			for (i = 1; i < m_max; i++)
			{
				m_ptr = &m_list[i];

				/* Paranoia -- Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				/* Location */
				y = m_ptr->fy;
				x = m_ptr->fx;

				/* Require line of sight */
				if (!player_has_los_bold(y, x)) continue;

				/* Skip distant monsters */
				if (distance(py, px, y, x) > 3) continue;

				/* Player's pets only */
				if (!is_pet(m_ptr)) continue;

				/* Jump directly to the target monster */
				project(0, 0, y, x, 200, GF_OLD_HEAL, flg, MODIFY_ELEM_MODE_NONE);
			}
		}
		break;
	case 13:
		{
			int  dummy;
			bool no_trump = TRUE;

			for (dummy = 0; dummy < 1 + ((clev - 15)/ 10); dummy++)
			{
				if (summon_specific(-1, py, px, clev, SUMMON_ANIMAL, (PM_ALLOW_GROUP | PM_FORCE_PET)))
					no_trump = FALSE;
			}

			if (no_trump)
#ifdef JP
				msg_print("動物は現れなかった。");
#else
				msg_print("No animals arrive.");
#endif
		}
		break;
	case 14:
		{
			bool old_target_pet = target_pet;
			if (!get_aim_dir(&dir))
			{
				target_pet = old_target_pet;
				return FALSE;
			}
			target_pet = old_target_pet;
			fire_ball(GF_STAR_HEAL, dir, 1000, 0, FALSE);
		}
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な共生の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Symbiotic spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_witch_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_WITCH]/10;
	int pstat;
	int k;
	s16b chosen_elem;

	switch (spell)
	{
	case 0: /* Infravision */
		set_tim_infra(100 + randint1(100), FALSE);
		break;
	case 1:
		pstat = p_ptr->stat_use[A_CHR];
		if (pstat < (18 + 180))
		{
			if (!get_aim_dir(&dir)) return FALSE;
			if (pstat >= (18 + 150)) fire_ball(GF_CHARM, dir, clev + 70, 4, FALSE);
			else if (pstat >= (18 + 100)) fire_ball(GF_CHARM, dir, clev + 60, 3, FALSE);
			else fire_ball(GF_CHARM, dir, clev + 50, 2, FALSE);
		}
		else charm_monsters(clev + 50);
		break;
	case 2:
		confuse_monsters(clev + 50);
		break;
	case 3: /* Heroism */
		(void)set_hero(randint1(25) + 25, FALSE);
		(void)hp_player(10);
		(void)set_afraid(0);
		break;
	case 4:
		chosen_elem = choose_elem();
		if (chosen_elem == NO_ELEM)
		{
#ifdef JP
			msg_print("エレメントを活性化させるのをやめた。");
#else
			msg_print("You cancel enhancing the elememt.");
#endif
			return FALSE;
		}
		k = 2;
		pstat = p_ptr->stat_use[A_INT];
		if (pstat >= (18 + 100)) k++;
		if (pstat >= (18 + 150)) k++;
		if (pstat >= (18 + 200)) k++;
		inc_area_elem(0, chosen_elem, k, (clev / 10) + 1, TRUE);
		break;
	case 5:
		set_wind_guard(randint1(clev) + 20, FALSE);
		break;
	case 6:
		(void)set_poisoned(0);
		(void)set_cut(0);
		(void)hp_player(damroll(4, 8));
		break;
	case 7: /* Haste */
		(void)set_fast(randint1(25 + clev) + clev, FALSE);
		break;
	case 8: /* Slow Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)slow_monster(dir, clev);
		break;
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;

		if (fire_ball(GF_OLD_DRAIN, dir, damroll(3, 5) + clev + clev / 2, 0, FALSE))
		{
			if (!one_in_(3)) fire_ball(GF_SILENT, dir, clev + 50, 0, FALSE);
		}
		break;
	case 10:
		if (!get_aim_dir(&dir)) return FALSE;
		pstat = p_ptr->stat_use[A_INT];
		if (pstat >= (18 + 150)) fire_ball(GF_STASIS, dir, clev + 70, 3, FALSE);
		else if (pstat >= (18 + 100)) fire_ball(GF_STASIS, dir, clev + 60, 2, FALSE);
		else fire_ball(GF_STASIS, dir, clev + 50, 1, FALSE);
		break;
	case 11: /* Explosive Rune */
		explosive_rune(clev);
		break;
	case 12:
		return jump_wall();
	case 13:
		if (!get_aim_dir(&dir)) return FALSE;
		pstat = p_ptr->stat_use[A_INT];
		fire_ball(GF_POIS, dir, 10 + clev * 2 + randint1(clev),
			(pstat >= (18 + 150)) ? 3 : 2, TRUE);
		break;
	case 14: /* Decoy */
		return set_decoy();
	case 15: /* Resistance */
		(void)set_oppose_acid(randint1(20) + 20, FALSE);
		(void)set_oppose_elec(randint1(20) + 20, FALSE);
		(void)set_oppose_fire(randint1(20) + 20, FALSE);
		(void)set_oppose_cold(randint1(20) + 20, FALSE);
		(void)set_oppose_pois(randint1(20) + 20, FALSE);
		break;
	case 16:
		return choose_magical_weapon();
	case 17:
		{
			int tx, ty;
			int power = clev * 2;

#ifdef JP
			msg_print("誰の攻撃力を弱めるか指定して下さい。");
#else
			msg_print("Choose the target to weaken.");
#endif
			if (!tgt_pt(&tx, &ty, FALSE)) return FALSE;
			if (!player_has_los_bold(ty, tx))
			{
#ifdef JP
				msg_print("その場所を指定することはできません。");
#else
				msg_print("You can't specify that place.");
#endif
				return FALSE;
			}

			if (!cave[ty][tx].m_idx)
			{
#ifdef JP
				msg_print("そこにはモンスターがいません。");
#else
				msg_print("There is no monster.");
#endif
				return FALSE;
			}

			if (cave[ty][tx].m_idx)
			{
				monster_type *m_ptr = &m_list[cave[ty][tx].m_idx];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];
				char m_name[80];

				monster_desc(m_name, m_ptr, 0);
				if (r_ptr->level > randint1((power - 10) < 1 ? 1 : (power - 10)) + 10)
				{
#ifdef JP
					msg_format("%^sには効果がなかった！", m_name);
#else
					msg_format("%^s is unaffected!", m_name);
#endif
				}
				else if (!m_ptr->melt_weapon)
				{
#ifdef JP
					msg_format("%^sの攻撃力が弱まったようだ。", m_name);
#else
					msg_format("Damage of %^s is seems to weakened.", m_name);
#endif
					m_ptr->melt_weapon = power;
				}
			}
		}
		break;
	case 18:
		chosen_elem = choose_elem();
		if (chosen_elem == NO_ELEM)
		{
#ifdef JP
			msg_print("エレメントを活性化させるのをやめた。");
#else
			msg_print("You cancel enhancing the elememt.");
#endif
			return FALSE;
		}
		k = 4;
		pstat = p_ptr->stat_use[A_INT];
		if (pstat >= (18 + 100)) k += 2;
		if (pstat >= (18 + 150)) k += 2;
		if (pstat >= (18 + 200)) k += 2;
		inc_area_elem(0, chosen_elem, k, (clev / 10) + 1, FALSE);
		break;
	case 19:
#ifdef JP
		msg_print("援軍を召喚した。");
#else
		msg_print("You summon minions.");
#endif
		summon_kin_player(clev * 2 / 3 + randint1(clev/2), py, px, PM_FORCE_PET);
		break;
	case 20:
		{
			int pure_elem_typ = GF_GODLY_SPEAR;

			if (!get_aim_dir(&dir)) return FALSE;

			switch (get_cur_pelem())
			{
			case NO_ELEM:
				pure_elem_typ = GF_MANA;
				break;
			case ELEM_FIRE:
				pure_elem_typ = GF_PURE_FIRE;
				break;
			case ELEM_AQUA:
				pure_elem_typ = GF_PURE_AQUA;
				break;
			case ELEM_EARTH:
				pure_elem_typ = GF_PURE_EARTH;
				break;
			case ELEM_WIND:
				pure_elem_typ = GF_PURE_WIND;
				break;
			}

			fire_beam(pure_elem_typ, dir, 100 + randint1(clev * 2));
		}
		break;
	case 21: /* Magic armor */
		(void)set_magicdef(randint1(20) + 20, FALSE);
		break;
	case 22: /* Alchemy */
		return alchemy();
	case 23:
		{
			int  dummy;
			bool no_trump = TRUE;

			for (dummy = 0; dummy < 1 + ((clev - 15)/ 10); dummy++)
			{
				if (summon_specific(-1, py, px, clev * 2 / 3 + randint1(clev/2), SUMMON_MOLD, (PM_ALLOW_GROUP | PM_FORCE_PET)))
					no_trump = FALSE;
			}

			if (no_trump)
#ifdef JP
				msg_print("モルドは現れなかった。");
#else
				msg_print("No molds arrive.");
#endif
		}
		break;
	case 24:
		msg_print("天候が荒れていく...");
		pstat = p_ptr->stat_use[A_INT];
		k = 2;
		if (pstat >= (18 + 150)) k++;
		if (pstat >= (18 + 200)) k++;
		set_weather(k, k, k);
		break;
	case 25:
		{
			int tx, ty;

#ifdef JP
			msg_print("誰の魔力を消去するか指定して下さい。");
#else
			msg_print("Choose the target to dispel.");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			range_restricted_target(dir, MAX_RANGE, &ty, &tx, FALSE);

			if (!((ty == py) && (tx == px)) && !cave[ty][tx].m_idx)
			{
#ifdef JP
				msg_print("そこには誰もいません。");
#else
				msg_print("There is no one.");
#endif
				return FALSE;
			}

			if ((ty == py) && (tx == px))
			{
				dispel_player();

				if (p_ptr->riding)
				{
					m_list[p_ptr->riding].invulner = 0;
					m_list[p_ptr->riding].fast = 0;
					m_list[p_ptr->riding].slow = 0;
					m_list[p_ptr->riding].melt_weapon = 0;
					m_list[p_ptr->riding].opposite_elem = 0;
					p_ptr->update |= PU_BONUS;
					if (p_ptr->health_who == p_ptr->riding) p_ptr->redraw |= PR_HEALTH;
					p_ptr->redraw |= (PR_UHEALTH);
				}
			}
			else if (cave[ty][tx].m_idx)
			{
				monster_type *m_ptr = &m_list[cave[ty][tx].m_idx];
				char m_name[80];

				monster_desc(m_name, m_ptr, 0);
				if (m_ptr->invulner)
				{
					m_ptr->invulner = 0;
#ifdef JP
					msg_format("%sはもう無敵ではない。", m_name);
#else
					msg_format("%^s is no longer invulnerable.", m_name);
#endif
					m_ptr->energy_need += ENERGY_NEED();
				}
				if (m_ptr->fast)
				{
					m_ptr->fast = 0;
#ifdef JP
					msg_format("%sはもう加速されていない。", m_name);
#else
					msg_format("%^s is no longer fast.", m_name);
#endif
				}
				if (m_ptr->slow)
				{
					m_ptr->slow = 0;
#ifdef JP
					msg_format("%sはもう減速されていない。", m_name);
#else
					msg_format("%^s is no longer slow.", m_name);
#endif
				}
				if (m_ptr->melt_weapon)
				{
					m_ptr->melt_weapon = 0;
#ifdef JP
					msg_format("%^sの攻撃力が元に戻ったようだ。", m_name);
#else
					msg_format("Damage of %^s is seems to restored.", m_name);
#endif
				}
				if (m_ptr->opposite_elem)
				{
					m_ptr->opposite_elem = 0;
#ifdef JP
					msg_format("%^sのエレメントの反転が消えた。", m_name);
#else
					msg_format("Elements of %^s are no longer reverted.", m_name);
#endif
					if (p_ptr->action == ACTION_ELEMSCOPE) lite_spot(m_ptr->fy, m_ptr->fx);
				}
				p_ptr->redraw |= (PR_HEALTH);
				if (p_ptr->riding == cave[ty][tx].m_idx) p_ptr->redraw |= (PR_UHEALTH);
			}
		}
		break;
	case 26:
		(void)hp_player(500);
		project(0, 3, py, px, 500, GF_OLD_HEAL, PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_NONE);
		(void)set_poisoned(0);
		(void)set_confused(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 27:
		if (!get_aim_dir(&dir)) return FALSE;
		pstat = p_ptr->stat_use[A_INT];
		fire_ball(GF_STONE, dir, 200 + clev * 3, (pstat >= (18 + 150)) ? 3 : 2, TRUE);
		break;
	case 28: /* Dimension Door */
#ifdef JP
		msg_print("次元の扉が開いた。目的地を選んで下さい。");
#else
		msg_print("You open a dimensional gate. Choose a destination.");
#endif
		return dimension_door(clev);
	case 29:
		{
			int tx, ty;
			int power = clev * 2;

#ifdef JP
			msg_print("誰のエレメントを反転させるか指定して下さい。");
#else
			msg_print("Choose the target to revert elements.");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			range_restricted_target(dir, MAX_RANGE, &ty, &tx, FALSE);

			if (!((ty == py) && (tx == px)) && !cave[ty][tx].m_idx)
			{
#ifdef JP
				msg_print("そこには誰もいません。");
#else
				msg_print("There is no one.");
#endif
				return FALSE;
			}

			if ((ty == py) && (tx == px))
			{
				if (!p_ptr->opposite_pelem) set_opposite_pelem(power);
			}
			else if (cave[ty][tx].m_idx)
			{
				monster_type *m_ptr = &m_list[cave[ty][tx].m_idx];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];
				char m_name[80];

				monster_desc(m_name, m_ptr, 0);
				if (r_ptr->level > randint1((power - 10) < 1 ? 1 : (power - 10)) + 10)
				{
#ifdef JP
					msg_format("%^sには効果がなかった！", m_name);
#else
					msg_format("%^s is unaffected!", m_name);
#endif
				}
				else if (!m_ptr->opposite_elem)
				{
#ifdef JP
					msg_format("%^sのエレメントが反転した。", m_name);
#else
					msg_format("Elements of %^s are reverted.", m_name);
#endif
					m_ptr->opposite_elem = power;
					if (p_ptr->action == ACTION_ELEMSCOPE) lite_spot(m_ptr->fy, m_ptr->fx);
				}
			}
		}
		break;
	case 30: /* Wraithform */
		set_wraith_form(randint1(clev / 2) + (clev / 2), FALSE);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明なウィッチの呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Witch spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_drakonite_spell(int spell)
{
	int	clev = p_ptr->magic_exp[REALM_DRAKONITE]/10;
	int i;

	switch (spell)
	{
	case 0:
		{
			int i;
			bool *no_revive;
			bool revived = FALSE;

			/* Allocate the "no_revive" array */
			C_MAKE(no_revive, max_r_idx, bool);

			/* Scan the random quests */
			for (i = 1; i < max_quests; i++)
			{
				if (quest[i].r_idx) no_revive[quest[i].r_idx] = TRUE;
			}

			/* Scan the arena */
			for (i = 0; i < ((sizeof arena_monsters) / sizeof (s16b)); i++)
			{
				no_revive[arena_monsters[i]] = TRUE;
			}

			/* Scan the monster races */
			for (i = 0; i < max_r_idx; i++)
			{
				monster_race *r_ptr = &r_info[i];

				if (!r_ptr->name) continue;

				/* Unique monsters only */
				if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

				/* Questors don't revive */
				if (r_ptr->flags1 & RF1_QUESTOR) continue;

				/* Unique monster is alive, no need to revive */
				if (r_ptr->max_num) continue;

				/* This unique is stayed dead... */
				if (no_revive[i]) continue;

				/* The dead unique monster is come back!! */
				r_ptr->max_num = 1;
				revived = TRUE;
			}

			/* Free the "no_revive" array */
			C_KILL(no_revive, max_r_idx, bool);

			if (revived)
			{
#ifdef JP
				msg_format("葬った仇敵達が帰ってくるのを感じる。");
#else
				msg_format("You feel slain foes coming back.");
#endif
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);

				if (p_ptr->materialize_cnt < MAX_SHORT) p_ptr->materialize_cnt++;
			}
		}
		break;
	case 1:
		project_hack(GF_NEW_SLOW, randint1(clev + 25) + clev);
		break;
	case 2:
		project(0, MAX_SIGHT, py, px, clev * 6 + randint1(clev * 5), GF_PURE_WIND, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		set_weather(8, 8, 8);
		break;
	case 3:
		project(0, MAX_SIGHT, py, px, clev * 6 + randint1(clev * 5), GF_PURE_FIRE, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		set_weather(-8, -8, -8);
		break;
	case 4:
		project(0, MAX_SIGHT, py, px, clev * 6 + randint1(clev * 5), GF_PURE_EARTH, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		confuse_monsters(clev + 50);
		break;
	case 5:
		project(0, MAX_SIGHT, py, px, clev * 6 + randint1(clev * 5), GF_PURE_AQUA, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
		stasis_monsters(clev + 50);
		break;
	case 6:
#ifdef JP
		if (!get_check("レベル1のキャラクタに転生します。よろしいですか？ ")) return FALSE;
#else
		if (!get_check("Reincarnate as level 1 character. Are you sure? ")) return FALSE;
#endif
		reincarnation();
		break;
	case 7:
#ifdef JP
		if (!get_check("本当に武器に変化しますか？")) return FALSE;
#else
		if (!get_check("Do you really want to commit change into a weapon? ")) return FALSE;
#endif
		/* Special Verification for Snap Dragon */
#ifdef JP
		prt("確認のため '@' を押して下さい。", 0, 0);
#else
		prt("Please verify CHANGE by typing the '@' sign: ", 0, 0);
#endif

		flush();
		i = inkey();
		prt("", 0, 0);
		if (i != '@') return FALSE;

		snap_dragon();
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な竜言語の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Drakonite spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_crusade_spell(int spell)
{
	int	dir;
	int	clev = p_ptr->magic_exp[REALM_CRUSADE]/10;
	int pstat;

	switch (spell)
	{
	case 0:
		(void)detect_monsters_evil(DETECT_RAD_DEFAULT);
		break;
	case 1:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_beam(GF_LITE, dir, damroll(3 + ((clev - 1) / 5), 4));
		break;
	case 2:
		(void)sleep_monsters_touch(clev);
		break;
	case 3:
		(void)set_cut(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		break;
	case 4: /* Sense Unseen */
		(void)set_tim_invis(randint1(24) + 24, FALSE);
		break;
	case 5:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_blast(GF_LITE, dir, 3+((clev-1)/9), 2, 10, 3);
		break;
	case 6:
		if (!get_aim_dir(&dir)) return FALSE;
		(void)stasis_evil(dir, clev * 2);
		break;
	case 7: /* Holy Orb */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_HOLY_FIRE, dir, damroll(3, 6) + clev + clev / 2,
			(clev < 30) ? 2 : 3, FALSE);
		break;
	case 8:
		dispel_undead(randint1(clev * 4));
		project_hack_undead(GF_AWAY_ALL, clev * 4);
		break;
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ELEC, dir, clev * 5);
		break;
	case 10:
		{
			int x, y, tx, ty;
			int dir, i;
			int b = 10 + randint1(10);

			if (!get_aim_dir(&dir)) return FALSE;
			range_restricted_target(dir, MAX_RANGE, &ty, &tx, TRUE);

			for (i = 0; i < b; i++)
			{
				int count = 20, d = 0;

				while (count--)
				{
					int dx, dy;

					x = tx - 5 + randint0(11);
					y = ty - 5 + randint0(11);

					dx = (tx > x) ? (tx - x) : (x - tx);
					dy = (ty > y) ? (ty - y) : (y - ty);

					/* Approximate distance */
					d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));
					/* Within the radius */
					if (d < 5) break;
				}

				if (count < 0) continue;

				/* Cannot penetrate perm walls */
				if (!in_bounds(y,x) ||
				    cave_stop_disintegration(y,x) ||
				    !in_disintegration_range(ty, tx, y, x))
					continue;

				project(0, 2, y, x, clev * 3 + 25, GF_DISINTEGRATE, PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
			}
		}
		break;
	case 11:
		project(0, 4, py, px, p_ptr->chp, GF_HOLY_FIRE, PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
		take_hit(DAMAGE_USELIFE, p_ptr->chp / 4, "自殺的なマジックボム");
#else
		take_hit(DAMAGE_USELIFE, p_ptr->chp / 4, "a suicidal Magic Bomb");
#endif
		break;
	case 12: /* Heroism */
		(void)set_hero(randint1(25) + 25, FALSE);
		(void)hp_player(10);
		(void)set_afraid(0);
		break;
	case 13:
		if (!get_aim_dir(&dir)) return FALSE;
		pstat = p_ptr->stat_use[A_WIS];
		{
			int dummy = (pstat >= (18 + 150)) ? 3 : 2;
			fire_ball(GF_GENOCIDE_UNDEAD, dir, clev * dummy, dummy, FALSE);
		}
		break;
	case 14: /* Berserk */
		(void)set_shero(randint1(25) + 25, FALSE);
		(void)hp_player(30);
		(void)set_afraid(0);
		break;
	case 15:
		set_tim_sh_holy(randint1(20)+20, FALSE);
		break;
	case 16:
		brand_weapon(13);
		break;
	case 17:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_SHINING, dir, clev + 60, 0, FALSE);
		break;
	case 18: /* Holy Word */
		(void)dispel_evil(randint1(clev * 6));
		(void)hp_player(100);
		(void)set_afraid(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 19: /* Star Burst */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_LITE, dir, 100+clev*2, 4, FALSE);
		break;
	case 20: /* Word of Destruction */
		destroy_area(py, px, 13+randint0(5));
		break;
	case 21: /* Eye for an Eye */
		set_tim_eyeeye(randint1(10)+10, FALSE);
		break;
	case 22:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_beam(GF_HOLY_FIRE, dir, 200 + clev * 2);
		break;
	case 23: /* Divine Intervention */
		project(0, 1, py, px, clev*11, GF_HOLY_FIRE, PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
		dispel_monsters(clev * 4);
		slow_monsters(clev);
		stun_monsters(clev * 4);
		confuse_monsters(clev * 4);
		turn_monsters(clev * 4);
		stasis_monsters(clev * 4);
		(void)hp_player(100);
		break;
	case 24:
		{
			int i;
			(void)crusade(clev * 4);
			for (i = 0; i < 12; i++)
			{
				int attempt = 10;
				int my, mx;

				while (attempt--)
				{
					scatter(&my, &mx, py, px, 4, 0);

					/* Require empty grids */
					if (cave_empty_bold2(my, mx)) break;
				}
				if (attempt < 0) continue;
				summon_specific(-1, my, mx, clev, SUMMON_HUMANS, (PM_ALLOW_GROUP | PM_FORCE_PET | PM_HASTE | PM_ALLOW_UNIQUE));
			}
			(void)set_hero(randint1(25) + 25, FALSE);
			(void)set_blessed(randint1(25) + 25, FALSE);
			(void)set_fast(randint1(20 + clev) + clev, FALSE);
			(void)set_protevil(randint1(25) + 25, FALSE);
			(void)set_afraid(0);
		}
		break;
	default:
#ifdef JP
		msg_format("あなたは不明な破邪の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown crusade spell: %d.", spell);
#endif

		msg_print(NULL);
	}

	return TRUE;
}


void stop_singing(void)
{
	if (!p_ptr->singing && !p_ptr->restart_singing) return;

	if (p_ptr->restart_singing)
	{
		p_ptr->restart_singing = 0;
		return;
	}
	if (!p_ptr->singing) return;

	/* Hack -- if called from set_action(), avoid recursive loop */
	if (p_ptr->action == ACTION_SING) set_action(ACTION_NONE);

	switch (p_ptr->singing)
	{
	case MUSIC_SILENT:
		song_of_silence(0);
		break;
	}

	p_ptr->singing = MUSIC_NONE;
	p_ptr->song_start = 0;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS | PU_HP);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int	item, sval, spell, realm;
	int	chance;
	int	use_realm;
	int	use_mana;
	bool cast = FALSE;

	cptr	prayer;

	object_type	*o_ptr;

	magic_type	*s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (!realm_choices[p_ptr->pclass])
	{
#ifdef JP
		msg_print("呪文を唱えられない！");
#else
		msg_print("You cannot cast spells!");
#endif

		return;
	}

	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
#ifdef JP
		msg_print("目が見えない！");
#else
		msg_print("You cannot see!");
#endif
		flush();
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて唱えられない！");
#else
		msg_print("You are too confused!");
#endif
		flush();
		return;
	}

	prayer = spell_categoly_name(mp_ptr->spell_book);

	/* Restrict choices to spell books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
#ifdef JP
	q = "どの呪文書を使いますか? ";
#else
	q = "Use which book? ";
#endif

#ifdef JP
	s = "呪文書がない！";
#else
	s = "You have no spell books!";
#endif

	select_spellbook = TRUE;
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
	{
		select_spellbook = FALSE;
		return;
	}
	select_spellbook = FALSE;

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

	/* Access the item's sval */
	sval = o_ptr->sval;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	realm = tval2realm(o_ptr->tval);

	/* Ask for a spell */
#ifdef JP
	if (!get_spell(&spell,
		           ((mp_ptr->spell_book == TV_HOLY_BOOK) ? "詠唱する" : "唱える"), 
		           sval, realm))
	{
		if (spell == -2) msg_format("その本には知っている%sがない。", prayer);
		return;
	}
#else
	if (!get_spell(&spell, ((mp_ptr->spell_book == TV_HOLY_BOOK) ? "recite" : "cast"),
		sval, realm))
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}
#endif


	use_realm = tval2realm(o_ptr->tval);

	s_ptr = &mp_ptr->info[realm - 1][spell];

	use_mana = calc_use_mana(spell, realm);

	/* Verify "dangerous" spells */
	if (use_mana > p_ptr->csp)
	{
		if (flush_failure) flush();

		/* Warning */
#ifdef JP
		msg_format("その%sを%sのに十分なマジックポイントがない。",prayer,
((mp_ptr->spell_book == TV_HOLY_BOOK) ? "詠唱する" : "唱える"));
#else
		msg_format("You do not have enough mana to %s this %s.",
			((mp_ptr->spell_book == TV_HOLY_BOOK) ? "recite" : "cast"),
			prayer);
#endif


		if (!over_exert) return;

		/* Verify */
#ifdef JP
		if (!get_check_strict("それでも挑戦しますか? ", CHECK_OKAY_CANCEL)) return;
#else
		if (!get_check_strict("Attempt it anyway? ", CHECK_OKAY_CANCEL)) return;
#endif

	}


	/* Spell failure chance */
	chance = spell_chance(spell, use_realm);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

#ifdef JP
		msg_format("%sをうまく唱えられなかった！", prayer);
#else
		msg_format("You failed to get the %s off!", prayer);
#endif

		sound(SOUND_FAIL);

		if ((o_ptr->tval == TV_DEATH_BOOK) && (randint1(100) < spell))
		{
			if ((sval == 1) && one_in_(2))
			{
				bool happened = FALSE;
				int power = 100;

				if (!character_dungeon) return;

				if (one_in_(2))
				{
#ifdef JP
					msg_print("暗黒の力が精神を蝕んだ！");
#else
					msg_print("Your sanity is shaken by reading the Necronomicon!");
#endif

					if (!saving_throw(p_ptr->skill_sav - power)) /* Mind blast */
					{
						if (!p_ptr->resist_conf)
						{
							(void)set_confused(p_ptr->confused + randint0(4) + 4);
						}
						if (!p_ptr->resist_chaos && one_in_(3))
						{
							(void)set_image(p_ptr->image + randint0(250) + 150);
						}
						return;
					}

					if (!saving_throw(p_ptr->skill_sav - power)) /* Lose int & wis */
					{
						do_dec_stat(A_INT);
						do_dec_stat(A_WIS);
						return;
					}

					if (!saving_throw(p_ptr->skill_sav - power)) /* Brain smash */
					{
						if (!p_ptr->resist_conf)
						{
							(void)set_confused(p_ptr->confused + randint0(4) + 4);
						}
						if (!p_ptr->free_act)
						{
							(void)set_paralyzed(p_ptr->paralyzed + randint0(4) + 4);
						}
						while (randint0(100) > p_ptr->skill_sav)
							(void)do_dec_stat(A_INT);
						while (randint0(100) > p_ptr->skill_sav)
							(void)do_dec_stat(A_WIS);
						if (!p_ptr->resist_chaos)
						{
							(void)set_image(p_ptr->image + randint0(250) + 150);
						}
						return;
					}

					if (!saving_throw(p_ptr->skill_sav - power)) /* Amnesia */
					{

						if (lose_all_info())
#ifdef JP
							msg_print("あまりの恐怖に全てのことを忘れてしまった！");
#else
							msg_print("You forget everything in your utmost terror!");
#endif

						return;
					}

					if (saving_throw(p_ptr->skill_sav - power))
					{
						return;
					}

					/* Else gain permanent insanity */
					if ((p_ptr->muta3 & MUT3_MORONIC) && /*(p_ptr->muta2 & MUT2_BERS_RAGE) &&*/
						((p_ptr->muta2 & MUT2_COWARDICE) || (p_ptr->resist_fear)) &&
						((p_ptr->muta2 & MUT2_HALLU) || (p_ptr->resist_chaos)))
					{
						/* The poor bastard already has all possible insanities! */
						return;
					}

					while (!happened)
					{
						switch (randint1(13))
						{
							case 1:
								if (!(p_ptr->muta3 & MUT3_MORONIC) && one_in_(5))
								{
									if ((p_ptr->stat_use[A_INT] < 4) && (p_ptr->stat_use[A_WIS] < 4))
									{
#ifdef JP
										msg_print("あなたは完璧な馬鹿になったような気がした。しかしそれは元々だった。");
#else
										msg_print("You turn into an utter moron!");
#endif
									}
									else
									{
#ifdef JP
										msg_print("あなたは完璧な馬鹿になった！");
#else
										msg_print("You turn into an utter moron!");
#endif
									}

									if (p_ptr->muta3 & MUT3_HYPER_INT)
									{
#ifdef JP
										msg_print("あなたの脳は生体コンピュータではなくなった。");
#else
										msg_print("Your brain is no longer a living computer.");
#endif

										p_ptr->muta3 &= ~(MUT3_HYPER_INT);
									}
									p_ptr->muta3 |= MUT3_MORONIC;
									happened = TRUE;
								}
								break;
							case 2:
							case 3:
							case 4:
							case 5:
							case 6:
							case 7:
								if (!(p_ptr->muta2 & MUT2_COWARDICE) && !p_ptr->resist_fear)
								{
#ifdef JP
									msg_print("あなたはパラノイアになった！");
#else
									msg_print("You become paranoid!");
#endif


									/* Duh, the following should never happen, but anyway... */
									if (p_ptr->muta3 & MUT3_FEARLESS)
									{
#ifdef JP
										msg_print("あなたはもう恐れ知らずではなくなった。");
#else
										msg_print("You are no longer fearless.");
#endif

										p_ptr->muta3 &= ~(MUT3_FEARLESS);
									}

									p_ptr->muta2 |= MUT2_COWARDICE;
									happened = TRUE;
								}
								break;
							default:
								if (!(p_ptr->muta2 & MUT2_HALLU) && !p_ptr->resist_chaos)
								{
#ifdef JP
									msg_print("幻覚をひき起こす精神錯乱に陥った！");
#else
									msg_print("You are afflicted by a hallucinatory insanity!");
#endif

									p_ptr->muta2 |= MUT2_HALLU;
									happened = TRUE;
								}
								break;
						}
					}

					p_ptr->update |= PU_BONUS;
					handle_stuff();
				}
				else
				{
#ifdef JP
					msg_print("暗黒の力が肉体を蝕んだ！");
#else
					msg_print("Your sanity is shaken by reading the Necronomicon!");
#endif

					if (!saving_throw(p_ptr->skill_sav - power)) /* Mind blast */
					{
						if (!one_in_(7))
						{
							(void)set_cut(p_ptr->cut + randint1(power));
						}
						if (!one_in_(7))
						{
							(void)set_stun(p_ptr->stun + randint1(power));
						}
						return;
					}

					if (!saving_throw(p_ptr->skill_sav - power)) /* Lose int & wis */
					{
						do_dec_stat(A_STR);
						do_dec_stat(A_DEX);
						do_dec_stat(A_CON);
						return;
					}

					if (!saving_throw(p_ptr->skill_sav - power))
					{
						if (!p_ptr->free_act)
						{
							(void)set_paralyzed(p_ptr->paralyzed + randint0(4) + 4);
						}
						while (randint0(100) > p_ptr->skill_sav)
							(void)do_dec_stat(A_STR);
						while (randint0(100) > p_ptr->skill_sav)
							(void)do_dec_stat(A_DEX);
						while (randint0(100) > p_ptr->skill_sav)
							(void)do_dec_stat(A_CON);
						return;
					}

					if (saving_throw(p_ptr->skill_sav - power))
					{
						return;
					}

					/* Else gain permanent insanity */
					if ((p_ptr->muta3 & MUT3_MORONIC) && /*(p_ptr->muta2 & MUT2_BERS_RAGE) &&*/
						((p_ptr->muta2 & MUT2_COWARDICE) || (p_ptr->resist_fear)) &&
						((p_ptr->muta2 & MUT2_HALLU) || (p_ptr->resist_chaos)))
					{
						/* The poor bastard already has all possible insanities! */
						return;
					}

					while (!happened)
					{
						switch (randint1(3))
						{
							case 1:
								if (!(p_ptr->muta2 & MUT2_WASTING))
								{
#ifdef JP
									msg_print("あなたの肉体はおぞましい衰弱病に冒された！");
#else
									msg_print("You suddenly contract a horrible wasting disease!");
#endif
									p_ptr->muta2 |= MUT2_WASTING;
									happened = TRUE;
								}
								break;
							case 2:
								if (!(p_ptr->muta3 & MUT3_FLESH_ROT))
								{
#ifdef JP
									msg_print("あなたの肉体は腐敗する病気に侵された！");
#else
									msg_print("Your flesh is afflicted by a rotting disease!");
#endif
									p_ptr->muta3 |= MUT3_FLESH_ROT;
									happened = TRUE;
								}
								break;
							default:
								if (!(p_ptr->muta3 & MUT3_ALBINO))
								{
#ifdef JP
									msg_print("アルビノになった！弱くなった気がする...");
#else
									msg_print("You turn into an albino! You feel frail...");
#endif

									p_ptr->muta3 |= MUT3_ALBINO;
									happened = TRUE;
								}
								break;
						}
					}

					p_ptr->update |= PU_BONUS;
					handle_stuff();
				}
			}
			else
			{
#ifdef JP
				msg_print("痛い！");
#else
				msg_print("It hurts!");
#endif

#ifdef JP
				take_hit(DAMAGE_LOSELIFE, damroll(o_ptr->sval + 1, 6), "暗黒魔法の逆流");
#else
				take_hit(DAMAGE_LOSELIFE, damroll(o_ptr->sval + 1, 6), "a miscast Death spell");
#endif

				if ((spell > 9) && one_in_(6) && !p_ptr->hold_life)
					lose_exp(spell * 250);
			}
		}
	}

	/* Process spell */
	else
	{
		if (p_ptr->singing) stop_singing();

		/* Spells.  */
		switch (realm)
		{
		case REALM_MAGERY: /* * MAGERY * */
			cast = cast_magery_spell(spell);
			break;
		case REALM_FIRE: /* * FIRE * */
			cast = cast_fire_spell(spell);
			break;
		case REALM_AQUA: /* * AQUA * */
			cast = cast_aqua_spell(spell);
			break;
		case REALM_EARTH: /* * EARTH * */
			cast = cast_earth_spell(spell);
			break;
		case REALM_WIND: /* * WIND * */
			cast = cast_wind_spell(spell);
			break;
		case REALM_HOLY: /* * HOLY * */
			cast = cast_holy_spell(spell);
			break;
		case REALM_DEATH: /* * DEATH * */
			cast = cast_death_spell(spell);
			break;
		case REALM_SYMBIOTIC: /* * SYMBIOTIC * */
			cast = cast_symbiotic_spell(spell);
			break;
		case REALM_WITCH: /* * WITCH * */
			cast = cast_witch_spell(spell);
			break;
		case REALM_DRAKONITE: /* * DRAKONITE * */
			cast = cast_drakonite_spell(spell);
			if (spell == 6) use_mana = 0; /* Reincarnated */
			break;
		case REALM_CRUSADE: /* * CRUSADE * */
			cast = cast_crusade_spell(spell);
			break;
		default:
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;

		switch (realm)
		{
		case REALM_HOLY: /* * HOLY * */
		case REALM_CRUSADE: /* * CRUSADE * */
			if (randint0(100) < s_ptr->slevel) change_your_alignment_lnc(1);
			break;
		case REALM_DEATH: /* * DEATH * */
		case REALM_SYMBIOTIC: /* * SYMBIOTIC * */
		case REALM_WITCH: /* * WITCH * */
			if (randint0(100) < s_ptr->slevel) change_your_alignment_lnc(-1);
			break;
		}
	}

	/* Take a turn */
	energy_use = 115 - skill_lev_var[(p_ptr->skill_exp[SKILL_SPELL_CAST]/10)] * 5;
	if (p_ptr->cexp_info[CLASS_HIGHWITCH].clev > 49) energy_use -= 50;
	else if ((p_ptr->cexp_info[CLASS_HIGHWITCH].clev > 29) || (p_ptr->cexp_info[CLASS_SIRENE].clev > 44) || (p_ptr->cexp_info[CLASS_WIZARD].clev > 44) || (p_ptr->cexp_info[CLASS_ARCHMAGE].clev > 44)) energy_use -= 25;

	/* Sufficient mana */
	if (use_mana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= use_mana;

		if (((realm == REALM_HOLY) && (spell == 24))
			|| ((realm == REALM_DEATH) && (spell == 23)))
		{
			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
		}
	}

	/* Over-exert the player */
	else
	{
		int oops = use_mana;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
#ifdef JP
		msg_print("精神を集中しすぎて気を失ってしまった！");
#else
		msg_print("You faint from the effort!");
#endif


		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (randint0(100) < 50)
		{
			bool perm = (randint0(100) < 25);

			/* Message */
#ifdef JP
			msg_print("体を悪くしてしまった！");
#else
			msg_print("You have damaged your health!");
#endif


			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint1(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * Pray a prayer -- Unused in TOband
 */
void do_cmd_pray(void)
{
	bool no_effect = TRUE;
	int  align_gne = 0, i;

	for (i = 0; i < ETHNICITY_NUM; i++) align_gne += chaos_frame[i];

	switch (get_your_alignment_gne())
	{
	case ALIGN_GNE_GOOD:
		if (!prace_is_(RACE_HUMAN)) break;
		if (get_cur_pelem() == NO_ELEM) break;
		if (cp_ptr->c_flags & PCF_REINCARNATE) break;

		switch (randint1(100))
		{
		case 1: case 2: case 3: case 4: case 5:
		case 6: case 7: case 8: case 9: case 10:
			{
				s16b pelem = get_cur_pelem();
				cptr god_name = "(?)";

				switch (pelem)
				{
				case ELEM_FIRE:
					god_name = "炎神ゾショネル";
					break;
				case ELEM_AQUA:
					god_name = "水神グルーザ";
					break;
				case ELEM_EARTH:
					god_name = "地神バーサ";
					break;
				case ELEM_WIND:
					god_name = "風神ハーネラ";
					break;
				}

				msg_format("祈りは%sに届いた。", god_name);
				inc_area_elem(0, pelem, 20, ((p_ptr->lev / 10) + 1), FALSE);
				no_effect = FALSE;
			}
			break;

		case 11:
			msg_print("祈りは太陽神フィラーハに届いた。");
			project_hack(GF_HOLY_FIRE, randint1(MAX(align_gne, 300)));
			no_effect = FALSE;
			break;
		}
		break;

	case ALIGN_GNE_EVIL:
		{
			int dam = 200 + randint1(MAX(0 - align_gne, 300) / 500);
			int pdam = dam * 2;

			msg_print("天罰が下った!");
			if (p_ptr->ogre_equip)
			{
#ifdef JP
				msg_print("ひどい痛手を受けた！");
#else
				msg_print("You are hit hard!");
#endif
				pdam *= 3;
			}
			(void)take_hit(DAMAGE_NOESCAPE, pdam, "天罰");
			project_hack(GF_ELEC, dam);
			no_effect = FALSE;
		}
		break;
	}

	if (no_effect)
	{
#ifdef JP
		msg_print("がんばって祈れば、祈りが届くこともある。");
#else
		msg_print("Pray hard enough and your prayers may be answered.");
#endif
	}

	energy_use = 100;
}

static bool ang_sort_comp_pet_dismiss(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	int w1 = who[a];
	int w2 = who[b];

	monster_type *m_ptr1 = &m_list[w1];
	monster_type *m_ptr2 = &m_list[w2];
	monster_race *r_ptr1 = &r_info[m_ptr1->r_idx];
	monster_race *r_ptr2 = &r_info[m_ptr2->r_idx];

	/* Unused */
	(void)v;

	if (w1 == p_ptr->riding) return TRUE;
	if (w2 == p_ptr->riding) return FALSE;

	if (m_ptr1->nickname && !m_ptr2->nickname) return TRUE;
	if (m_ptr2->nickname && !m_ptr1->nickname) return FALSE;

	if ((r_ptr1->flags1 & RF1_UNIQUE) && !(r_ptr2->flags1 & RF1_UNIQUE)) return TRUE;
	if ((r_ptr2->flags1 & RF1_UNIQUE) && !(r_ptr1->flags1 & RF1_UNIQUE)) return FALSE;

	if (r_ptr1->level > r_ptr2->level) return TRUE;
	if (r_ptr2->level > r_ptr1->level) return FALSE;

	if (m_ptr1->hp > m_ptr2->hp) return TRUE;
	if (m_ptr2->hp > m_ptr1->hp) return FALSE;
	
	return w1 <= w2;
}

int calculate_upkeep(void)
{
	s32b old_friend_align_lnc = friend_align_lnc;
	int m_idx;
	bool have_a_unique = FALSE;
	int total_friend_levels = 0;
	int temp_rlev;

	total_friends = 0;
	friend_align_gne = 0;
	friend_align_lnc = 0;

	for (m_idx = m_max - 1; m_idx >=1; m_idx--)
	{
		monster_type *m_ptr;
		monster_race *r_ptr;

		m_ptr = &m_list[m_idx];
		if (!m_ptr->r_idx) continue;
		r_ptr = &r_info[m_ptr->r_idx];

		if (is_pet(m_ptr))
		{
			total_friends++;
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				if (p_ptr->pclass == CLASS_LORD)
				{
					temp_rlev = r_ptr->level;
				}
				else if (p_ptr->pclass == CLASS_GENERAL)
				{
					if ((p_ptr->riding == m_idx) || !have_a_unique)
						temp_rlev = (r_ptr->level+5)*2;
					else
						temp_rlev = (r_ptr->level+5)*7/2;
					have_a_unique = TRUE;
				}
				else if (((p_ptr->pclass == CLASS_BEASTTAMER) || (p_ptr->pclass == CLASS_DRAGONTAMER)) || ((p_ptr->cexp_info[CLASS_BEASTTAMER].clev > 49) || (p_ptr->cexp_info[CLASS_DRAGONTAMER].clev > 49)))
				{
					if (p_ptr->riding == m_idx)
						temp_rlev = (r_ptr->level+5)*2;
					else if (!have_a_unique && (r_ptr->flags7 & RF7_RIDING))
						temp_rlev = (r_ptr->level+5)*7/2;
					else
						temp_rlev = (r_ptr->level+5)*10;
					have_a_unique = TRUE;
				}
				else
					temp_rlev = (r_ptr->level+5)*10;
			}
			else
				temp_rlev = r_ptr->level;

			if (((p_ptr->pclass == CLASS_LICH) || (p_ptr->pclass == CLASS_VAMPIRE)) && (r_ptr->flags3 & RF3_UNDEAD)) temp_rlev /= 2;

			if (r_ptr->flags7 & RF7_EGG_ONLY) temp_rlev /= 3;
			total_friend_levels += temp_rlev;

			/* Determine pet alignment */
			if (r_ptr->flags3 & RF3_GOOD) friend_align_gne += r_ptr->level / 5;
			if (r_ptr->flags3 & RF3_EVIL) friend_align_gne -= r_ptr->level / 5;
			if (r_ptr->flags7 & RF7_LAWFUL) friend_align_lnc += r_ptr->level / 5;
			if (r_ptr->flags7 & RF7_CHAOTIC) friend_align_lnc -= r_ptr->level / 5;
		}
	}
	if (old_friend_align_lnc != friend_align_lnc) p_ptr->update |= (PU_BONUS);
	if (total_friends)
	{
		int upkeep_factor;
		upkeep_factor = (total_friend_levels - (p_ptr->lev * 80 / ((6 - (int)skill_exp_level(p_ptr->skill_exp[SKILL_PET_UPKEEP]/10)) * 10)));
		if (upkeep_factor < 0) upkeep_factor = 0;
		if (upkeep_factor > 1000) upkeep_factor = 1000;
		return upkeep_factor;
	}
	else
		return 0;
}

void do_cmd_pet_dismiss(void)
{
	monster_type	*m_ptr;
	bool		all_pets = FALSE;
	int pet_ctr, i;
	int Dismissed = 0;

	u16b *who;
	u16b dummy_why;
	int max_pet = 0;
	int cu, cv;

	cu = Term->scr->cu;
	cv = Term->scr->cv;
	Term->scr->cu = 0;
	Term->scr->cv = 1;

	/* Allocate the "who" array */
	C_MAKE(who, max_m_idx, u16b);

	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		if (is_pet(&m_list[pet_ctr]))
			who[max_pet++] = pet_ctr;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_pet_dismiss;
	ang_sort_swap = ang_sort_swap_hook;

	ang_sort(who, &dummy_why, max_pet);

	/* Process the monsters (backwards) */
	for (i = 0; i < max_pet; i++)
	{
		bool delete_this;
		char friend_name[80];
		char buf[80];
		bool kakunin;

		/* Access the monster */
		pet_ctr = who[i];
		m_ptr = &m_list[pet_ctr];

		delete_this = FALSE;
		kakunin = ((pet_ctr == p_ptr->riding) || (m_ptr->nickname));
		monster_desc(friend_name, m_ptr, 0x80);
		
		if (!all_pets)
		{
			/* Hack -- health bar for this monster */
			health_track(pet_ctr);
			
			/* Hack -- handle stuff */
			handle_stuff();
			
#ifdef JP
			sprintf(buf, "%sを放しますか？ [Yes/No/Unnamed (%d匹)]", friend_name, max_pet-i);
#else
			sprintf(buf, "Dismiss %s? [Yes/No/Unnamed (%d remain)]", friend_name, max_pet-i);
#endif
			prt(buf, 0, 0);
			
			if (m_ptr->ml)
				move_cursor_relative(m_ptr->fy, m_ptr->fx);
			
			while (TRUE)
			{
				char ch = inkey();

				if (ch == 'Y' || ch == 'y')
				{
					delete_this = TRUE;
					
					if (kakunin)
					{
#ifdef JP
						sprintf(buf, "本当によろしいですか？ (%s) ", friend_name);
#else
						sprintf(buf, "Are you sure? (%s) ", friend_name);
#endif
						if (!get_check(buf))
							delete_this = FALSE;
					}
					break;
				}
				
				if (ch == 'U' || ch == 'u')
				{
					all_pets = TRUE;
					break;
				}
				
				if (ch == ESCAPE || ch == 'N' || ch == 'n')
					break;
				
				bell();
			}
		}
		
		if ((all_pets && !kakunin) || (!all_pets && delete_this))
		{
			if (record_named_pet && m_ptr->nickname)
			{
				char m_name[80];
				
				monster_desc(m_name, m_ptr, 0x08);
				do_cmd_write_nikki(NIKKI_NAMED_PET, 2, m_name);
			}
			
			if (pet_ctr == p_ptr->riding)
			{
#ifdef JP
				msg_format("%sから降りた。", friend_name);
#else
				msg_format("You have got off %s. ", friend_name);
#endif
				
				p_ptr->riding = 0;
				
				/* Update the monsters */
				p_ptr->update |= (PU_BONUS | PU_MONSTERS);
				p_ptr->redraw |= (PR_EXTRA);
			}

			/* HACK : Add the line to message buffer */
#ifdef JP
			sprintf(buf, "%sを放した。", friend_name);
#else
			sprintf(buf, "Dismissed %s.", friend_name);
#endif
			message_add(buf);
			p_ptr->window |= (PW_MESSAGE);
			window_stuff();

			delete_monster_idx(pet_ctr);
			Dismissed++;
		}
	}
	
	Term->scr->cu = cu;
	Term->scr->cv = cv;
	Term_fresh();

	C_KILL(who, max_m_idx, u16b);

#ifdef JP
	msg_format("%d 匹のペットを放しました。", Dismissed);
#else
	msg_format("You have dismissed %d pet%s.", Dismissed,
		   (Dismissed == 1 ? "" : "s"));
#endif
	if (Dismissed == 0 && all_pets)
#ifdef JP
		msg_print("'U'nnamed は、乗馬以外の名前のないペットだけを全て解放します。");
#else
		msg_print("'U'nnamed means all your pets except named pets and your mount.");
#endif

	p_ptr->update |= (PU_MON_LITE);
}

bool rakuba(int dam, bool force)
{
	int i, y, x, oy, ox;
	int sn = 0, sy = 0, sx = 0, riding_level = 0;
	char m_name[80];
	monster_type *m_ptr = &m_list[p_ptr->riding];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (!p_ptr->riding) return FALSE;
	if (p_ptr->wild_mode) return FALSE;

	if (p_ptr->psex == SEX_MALE) riding_level = p_ptr->cexp_info[CLASS_BEASTTAMER].clev + p_ptr->cexp_info[CLASS_GENERAL].clev;
	else riding_level = p_ptr->cexp_info[CLASS_DRAGONTAMER].clev + p_ptr->cexp_info[CLASS_FREYA].clev;

	if ((p_ptr->pclass == CLASS_BEASTTAMER) || (p_ptr->pclass == CLASS_DRAGONTAMER)) riding_level += 15;

	if (dam >= 0 || force)
	{
		if (!force)
		{
			int level = r_ptr->level;
			if (p_ptr->riding_ryoute) level += 20;
			if (randint0(dam/2 + level*2) < ((skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000)/30+10))
			{
				if ((!p_ptr->riding_ryoute) || !one_in_(riding_level * (p_ptr->riding_ryoute ? 2 : 3)+30))
				{
					return FALSE;
				}
			}
		}
		/* Check around the player */
		for (i = 0; i < 8; i++)
		{
			cave_type *c_ptr;

			/* Access the location */
			y = py + ddy_ddd[i];
			x = px + ddx_ddd[i];

			c_ptr = &cave[y][x];

			/* Skip non-empty grids */
			if (cave_perma_grid(c_ptr)) continue;
			if (!cave_empty_grid2(c_ptr)) continue;

			if (c_ptr->m_idx) continue;

			/* Count "safe" grids */
			sn++;

			/* Randomize choice */
			if (randint0(sn) > 0) continue;

			/* Save the safe location */
			sy = y; sx = x;
		}
		if (!sn)
		{
			monster_desc(m_name, m_ptr, 0);
#ifdef JP
			msg_format("%sから振り落とされそうになって、壁にぶつかった。",m_name);
			take_hit(DAMAGE_NOESCAPE, r_ptr->level+3, "壁への衝突");
#else
			msg_format("You have nearly fallen from %s, but bumped into wall.",m_name);
			take_hit(DAMAGE_NOESCAPE, r_ptr->level+3, "bumping into wall");
#endif
			return FALSE;
		}

		oy = py;
		ox = px;

		py = sy;
		px = sx;

		/* Redraw the old spot */
		lite_spot(oy, ox);

		/* Redraw the new spot */
		lite_spot(py, px);

		/* Check for new panel */
		verify_panel();
	}

	p_ptr->riding = 0;
	p_ptr->pet_extra_flags &= ~(PF_RYOUTE);
	p_ptr->riding_ryoute = p_ptr->old_riding_ryoute = FALSE;

	set_mermaid_in_water();

	calc_bonuses();

	p_ptr->update |= (PU_BONUS);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	p_ptr->redraw |= (PR_EXTRA);

	/* Update health track of mount */
	p_ptr->redraw |= (PR_UHEALTH);

	if (p_ptr->ffall && !force)
	{
		if (cave[py][px].feat != FEAT_AIR)
		{
			monster_desc(m_name, m_ptr, 0);
#ifdef JP
			msg_format("%sから落ちたが、空中でうまく体勢を立て直して着地した。",m_name);
#else
			msg_format("You are thrown from %s, but make a good landing.",m_name);
#endif
		}
		return FALSE;
	}

	if (cave[py][px].feat != FEAT_AIR)
	{
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, r_ptr->level+3, "落馬");
#else
		take_hit(DAMAGE_NOESCAPE, r_ptr->level+3, "Falling from riding");
#endif
	}

	return TRUE;
}

bool do_riding(bool force)
{
	int oy, ox, x, y, dir = 0;
	cave_type *c_ptr;
	monster_type *m_ptr;

	if (!get_rep_dir2(&dir)) return FALSE;
	y = py + ddy[dir];
	x = px + ddx[dir];
	c_ptr = &cave[y][x];

	if (p_ptr->riding)
	{
		/* Skip non-empty grids */
		if (!player_can_enter(c_ptr->feat) || c_ptr->m_idx)
		{
#ifdef JP
			msg_print("そちらには降りられません。");
#else
			msg_print("You cannot go to that direction.");
#endif
			return FALSE;
		}
		p_ptr->riding = 0;
		p_ptr->pet_extra_flags &= ~(PF_RYOUTE);
		p_ptr->riding_ryoute = p_ptr->old_riding_ryoute = FALSE;
	}
	else
	{
		if (p_ptr->confused)
		{
#ifdef JP
			msg_print("混乱していて乗れない！");
#else
			msg_print("You are too confused!");
#endif
			return FALSE;
		}
		if (!(c_ptr->m_idx))
		{
#ifdef JP
			msg_print("その場所にはモンスターはいません。");
#else
			msg_print("Here is no pet.");
#endif

			return FALSE;
		}

		m_ptr = &m_list[c_ptr->m_idx];

		if (!is_pet(m_ptr) && !force)
		{
#ifdef JP
			msg_print("そのモンスターはペットではありません。");
#else
			msg_print("That monster is no a pet.");
#endif

			return FALSE;
		}
		if (!(r_info[m_ptr->r_idx].flags7 & RF7_RIDING))
		{
#ifdef JP
			msg_print("そのモンスターには乗れなさそうだ。");
#else
			msg_print("This monster doesn't seem suitable for riding.");
#endif

			return FALSE;
		}
		if (!(p_ptr->pass_wall) && (c_ptr->feat >= FEAT_RUBBLE) && (c_ptr->feat <= FEAT_PERM_SOLID))
		{
#ifdef JP
			msg_print("そのモンスターは壁の中にいる。");
#else
			msg_print("This monster is in the wall.");
#endif

			return FALSE;
		}
		if (!m_ptr->ml)
		{
#ifdef JP
			msg_print("その場所にはモンスターはいません。");
#else
			msg_print("Here is no monster.");
#endif

			return FALSE;
		}
		if (r_info[m_ptr->r_idx].level > randint1(((skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000)/50 + p_ptr->lev/2 +20)))
		{
#ifdef JP
			msg_print("うまく乗れなかった。");
#else
			msg_print("You failed to ride.");
#endif

			energy_use = 100;

			return FALSE;
		}
		if (m_ptr->csleep)
		{
			char m_name[80];
			monster_desc(m_name, m_ptr, 0);
			m_ptr->csleep = 0;
#ifdef JP
			msg_format("%sを起こした。", m_name);
#else
			msg_format("You have waked %s up.", m_name);
#endif
		}

		p_ptr->riding = c_ptr->m_idx;

		/* Hack -- remove tracked monster */
		if (p_ptr->riding == p_ptr->health_who) health_track(0);
	}

	/* Save the old location */
	oy = py;
	ox = px;

	/* Move the player to the safe location */
	py = y;
	px = x;

	/* Redraw the old spot */
	lite_spot(oy, ox);

	/* Redraw the new spot */
	lite_spot(py, px);

	/* Check for new panel */
	verify_panel();

	set_mermaid_in_water();

	energy_use = 100;

	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Update the monsters */
	p_ptr->update |= (PU_BONUS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP | PR_EXTRA);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	p_ptr->redraw |= (PR_UHEALTH);

	handle_stuff();
	return TRUE;
}

static void do_name_pet(void)
{
	monster_type *m_ptr;
	char out_val[20];
	char m_name[80];
	bool old_name = FALSE;
	bool old_target_pet = target_pet;

	target_pet = TRUE;
	if (!target_set(TARGET_KILL))
	{
		target_pet = old_target_pet;
		return;
	}
	target_pet = old_target_pet;

	if (cave[target_row][target_col].m_idx)
	{
		m_ptr = &m_list[cave[target_row][target_col].m_idx];

		if (!is_pet(m_ptr))
		{
			/* Message */
#ifdef JP
			msg_print("そのモンスターはペットではない。");
#else
			msg_format("This monster is not a pet.");
#endif
			return;
		}
		if (r_info[m_ptr->r_idx].flags1 & RF1_UNIQUE)
		{
#ifdef JP
			msg_print("そのモンスターの名前は変えられない！");
#else
			msg_format("You cannot change name of this monster!");
#endif
			return;
		}
		monster_desc(m_name, m_ptr, 0);

		/* Message */
#ifdef JP
		msg_format("%sに名前をつける。", m_name);
#else
		msg_format("Name %s.", m_name);
#endif

		msg_print(NULL);

		/* Start with nothing */
		strcpy(out_val, "");

		/* Use old inscription */
		if (m_ptr->nickname)
		{
			/* Start with the old inscription */
			strcpy(out_val, quark_str(m_ptr->nickname));
			old_name = TRUE;
		}
		else
		{
#ifdef JP
			(void)get_rnd_line("petnam_j.txt", 0, out_val);
#else
			(void)get_rnd_line("petnam.txt", 0, out_val);
#endif
		}

		/* Get a new inscription (possibly empty) */
#ifdef JP
		if (get_string("名前: ", out_val, 15))
#else
		if (get_string("Name: ", out_val, 15))
#endif

		{
			if (out_val[0])
			{
				/* Save the inscription */
				m_ptr->nickname = quark_add(out_val);
				if (record_named_pet)
				{
					char m_name[80];

					monster_desc(m_name, m_ptr, 0x08);
					do_cmd_write_nikki(NIKKI_NAMED_PET, 0, m_name);
				}
			}
			else
			{
				if (record_named_pet && old_name)
				{
					char m_name[80];

					monster_desc(m_name, m_ptr, 0x08);
					do_cmd_write_nikki(NIKKI_NAMED_PET, 1, m_name);
				}
				m_ptr->nickname = 0;
			}
		}
	}
}

/*
 * Issue a pet command
 */
void do_cmd_pet(void)
{
	int			i = 0;
	int			num;
	int			powers[36];
	cptr			power_desc[36];
	bool			flag, redraw;
	int			ask;
	char			choice;
	char			out_val[160];
	int			pets = 0, pet_ctr;
	monster_type	*m_ptr;

	int mode = 0;

	byte y = 1, x = 0;
	int ctr = 0;
	char buf[160];
	char target_buf[160];

	num = 0;

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr)) pets++;
	}

#ifdef JP
	power_desc[num] = "ペットを放す";
#else
	power_desc[num] = "dismiss pets";
#endif

	powers[num++] = PET_DISMISS;

#ifdef JP
	sprintf(target_buf,"ペットのターゲットを指定 (現在：%s)",
		(pet_t_m_idx ? r_name + r_info[m_list[pet_t_m_idx].r_idx].name : "指定なし"));
#else
	sprintf(target_buf,"specify a targert of pet (now:%s)",
		(pet_t_m_idx ? r_name + r_info[m_list[pet_t_m_idx].r_idx].name : "nothing"));
#endif
	power_desc[num] = target_buf;

	powers[num++] = PET_TARGET;

#ifdef JP
power_desc[num] = "近くにいろ";
#else
	power_desc[num] = "stay close";
#endif

	if (p_ptr->pet_follow_distance == PET_CLOSE_DIST) mode = num;
	powers[num++] = PET_STAY_CLOSE;

#ifdef JP
	power_desc[num] = "ついて来い";
#else
	power_desc[num] = "follow me";
#endif

	if (p_ptr->pet_follow_distance == PET_FOLLOW_DIST) mode = num;
	powers[num++] = PET_FOLLOW_ME;

#ifdef JP
power_desc[num] = "敵を見つけて倒せ";
#else
	power_desc[num] = "seek and destroy";
#endif

	if (p_ptr->pet_follow_distance == PET_DESTROY_DIST) mode = num;
	powers[num++] = PET_SEEK_AND_DESTROY;

#ifdef JP
power_desc[num] = "少し離れていろ";
#else
	power_desc[num] = "give me space";
#endif

	if (p_ptr->pet_follow_distance == PET_SPACE_DIST) mode = num;
	powers[num++] = PET_ALLOW_SPACE;

#ifdef JP
power_desc[num] = "離れていろ";
#else
	power_desc[num] = "stay away";
#endif

	if (p_ptr->pet_follow_distance == PET_AWAY_DIST) mode = num;
	powers[num++] = PET_STAY_AWAY;

	if (p_ptr->pet_extra_flags & PF_OPEN_DOORS)
	{
#ifdef JP
		power_desc[num] = "ドアを開ける (現在:ON)";
#else
		power_desc[num] = "pets open doors (now On)";
#endif

	}
	else
	{
#ifdef JP
		power_desc[num] = "ドアを開ける (現在:OFF)";
#else
		power_desc[num] = "pets open doors (now Off)";
#endif

	}
	powers[num++] = PET_OPEN_DOORS;

	if (p_ptr->pet_extra_flags & PF_PICKUP_ITEMS)
	{
#ifdef JP
		power_desc[num] = "アイテムを拾う (現在:ON)";
#else
		power_desc[num] = "pets pick up items (now On)";
#endif

	}
	else
	{
#ifdef JP
		power_desc[num] = "アイテムを拾う (現在:OFF)";
#else
		power_desc[num] = "pets pick up items (now Off)";
#endif

	}
	powers[num++] = PET_TAKE_ITEMS;

	if (p_ptr->pet_extra_flags & PF_TELEPORT)
	{
#ifdef JP
		power_desc[num] = "テレポート系魔法を使う (現在:ON)";
#else
		power_desc[num] = "allow teleport (now On)";
#endif

	}
	else
	{
#ifdef JP
		power_desc[num] = "テレポート系魔法を使う (現在:OFF)";
#else
		power_desc[num] = "allow teleport (now Off)";
#endif

	}
	powers[num++] = PET_TELEPORT;

	if (p_ptr->pet_extra_flags & PF_ATTACK_SPELL)
	{
#ifdef JP
		power_desc[num] = "攻撃魔法を使う (現在:ON)";
#else
		power_desc[num] = "allow cast attack spell (now On)";
#endif

	}
	else
	{
#ifdef JP
		power_desc[num] = "攻撃魔法を使う (現在:OFF)";
#else
		power_desc[num] = "allow cast attack spell (now Off)";
#endif

	}
	powers[num++] = PET_ATTACK_SPELL;

	if (p_ptr->pet_extra_flags & PF_DISI_SPELL)
	{
#ifdef JP
		power_desc[num] = "分解魔法を使う (現在:ON)";
#else
		power_desc[num] = "allow cast disintegration spell (now On)";
#endif

	}
	else
	{
#ifdef JP
		power_desc[num] = "分解魔法を使う (現在:OFF)";
#else
		power_desc[num] = "allow cast disintegration spell (now Off)";
#endif

	}
	powers[num++] = PET_DISI_SPELL;

	if (p_ptr->pet_extra_flags & PF_SUMMON_SPELL)
	{
#ifdef JP
		power_desc[num] = "召喚魔法を使う (現在:ON)";
#else
		power_desc[num] = "allow cast summon spell (now On)";
#endif

	}
	else
	{
#ifdef JP
		power_desc[num] = "召喚魔法を使う (現在:OFF)";
#else
		power_desc[num] = "allow cast summon spell (now Off)";
#endif

	}
	powers[num++] = PET_SUMMON_SPELL;

	if (p_ptr->pet_extra_flags & PF_BALL_SPELL)
	{
#ifdef JP
		power_desc[num] = "プレイヤーを巻き込む範囲魔法を使う (現在:ON)";
#else
		power_desc[num] = "allow involve player in area spell (now On)";
#endif

	}
	else
	{
#ifdef JP
		power_desc[num] = "プレイヤーを巻き込む範囲魔法を使う (現在:OFF)";
#else
		power_desc[num] = "allow involve player in area spell (now Off)";
#endif

	}
	powers[num++] = PET_BALL_SPELL;

	if (p_ptr->riding)
	{
#ifdef JP
		power_desc[num] = "ペットから降りる";
#else
		power_desc[num] = "get off a pet";
#endif

	}
	else
	{
#ifdef JP
		power_desc[num] = "ペットに乗る";
#else
		power_desc[num] = "ride a pet";
#endif

	}
	powers[num++] = PET_RIDING;

#ifdef JP
	power_desc[num] = "ペットに名前をつける";
#else
	power_desc[num] = "name pets";
#endif

	powers[num++] = PET_NAME;

	if (p_ptr->riding && buki_motteruka(INVEN_RARM) && (empty_hands() & EMPTY_HAND_LARM) && ((inventory[INVEN_RARM].weight > 99) || (inventory[INVEN_RARM].tval == TV_POLEARM)))
	{
		if (p_ptr->pet_extra_flags & PF_RYOUTE)
		{
#ifdef JP
			power_desc[num] = "武器を片手で持つ";
#else
			power_desc[num] = "use one hand to control a riding pet";
#endif

		}
		else
		{
#ifdef JP
			power_desc[num] = "武器を両手で持つ";
#else
			power_desc[num] = "use both hands for a weapon.";
#endif

		}

		powers[num++] = PET_RYOUTE;
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
#ifdef JP
		strnfmt(out_val, 78, "(コマンド %c-%c、'*'=一覧、ESC=終了) コマンドを選んでください:",
#else
		strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
#endif

			I2A(0), I2A(num - 1));
	}
	else
	{
#ifdef JP
		strnfmt(out_val, 78, "(コマンド %c-%c、'*'=一覧、ESC=終了) コマンドを選んでください:",
#else
		strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
#endif

			I2A(0), '0' + num - 27);
	}

	/* Show list */
	redraw = TRUE;

	/* Save the screen */
	Term_save();

	prt("", y++, x);

	while (ctr < num)
	{
		prt(format("%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]), y + ctr, x);
		ctr++;
	}

	if (ctr <= PET_RYOUTE)
	{
		prt("", y + ctr, x);
	}
	else
	{
		prt("", y + PET_RYOUTE + 1, x);
	}

	/* Get a command from the user */
	while (!flag && get_com(out_val, &choice, TRUE))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				y = 1;
				x = 0;
				ctr = 0;

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt("", y++, x);

				while (ctr < num)
				{
					sprintf(buf, "%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]);
					prt(buf, y + ctr, x);
					ctr++;
				}

				if (ctr <= PET_RYOUTE)
				{
					prt("", y + ctr, x);
				}
				else
				{
					prt("", y + PET_RYOUTE + 1, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
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

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Verify it */
		if (ask)
		{
			/* Prompt */
#ifdef JP
			strnfmt(buf, 78, "%sを使いますか？ ", power_desc[i]);
#else
			strnfmt(buf, 78, "Use %s? ", power_desc[i]);
#endif


			/* Belay that order */
			if (!get_check(buf)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag)
	{
		energy_use = 0;
		return;
	}

	switch (powers[i])
	{
		case PET_DISMISS: /* Dismiss pets */
		{
			if (!pets)
			{
#ifdef JP
				msg_print("ペットがいない！");
#else
				msg_print("You have no pets!");
#endif
				break;
			}
			do_cmd_pet_dismiss();
			(void)calculate_upkeep();
			break;
		}
		case PET_TARGET:
		{
			project_length = -1;
			if (!target_set(TARGET_KILL)) pet_t_m_idx = 0;
			else
			{
				cave_type *c_ptr = &cave[target_row][target_col];
				if (c_ptr->m_idx && (m_list[c_ptr->m_idx].ml))
				{
					pet_t_m_idx = cave[target_row][target_col].m_idx;
					p_ptr->pet_follow_distance = PET_DESTROY_DIST;
				}
				else pet_t_m_idx = 0;
			}
			project_length = 0;

			break;
		}
		/* Call pets */
		case PET_STAY_CLOSE:
		{
			p_ptr->pet_follow_distance = PET_CLOSE_DIST;
			pet_t_m_idx = 0;
			break;
		}
		/* "Follow Me" */
		case PET_FOLLOW_ME:
		{
			p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
			pet_t_m_idx = 0;
			break;
		}
		/* "Seek and destoy" */
		case PET_SEEK_AND_DESTROY:
		{
			p_ptr->pet_follow_distance = PET_DESTROY_DIST;
			break;
		}
		/* "Give me space" */
		case PET_ALLOW_SPACE:
		{
			p_ptr->pet_follow_distance = PET_SPACE_DIST;
			break;
		}
		/* "Stay away" */
		case PET_STAY_AWAY:
		{
			p_ptr->pet_follow_distance = PET_AWAY_DIST;
			break;
		}
		/* flag - allow pets to open doors */
		case PET_OPEN_DOORS:
		{
			if (p_ptr->pet_extra_flags & PF_OPEN_DOORS) p_ptr->pet_extra_flags &= ~(PF_OPEN_DOORS);
			else p_ptr->pet_extra_flags |= (PF_OPEN_DOORS);
			break;
		}
		/* flag - allow pets to pickup items */
		case PET_TAKE_ITEMS:
		{
			if (p_ptr->pet_extra_flags & PF_PICKUP_ITEMS)
			{
				p_ptr->pet_extra_flags &= ~(PF_PICKUP_ITEMS);
				for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
				{
					/* Access the monster */
					m_ptr = &m_list[pet_ctr];

					if (is_pet(m_ptr))
					{
						monster_drop_carried_objects(m_ptr);
					}
				}
			}
			else p_ptr->pet_extra_flags |= (PF_PICKUP_ITEMS);

			break;
		}
		/* flag - allow pets to teleport */
		case PET_TELEPORT:
		{
			if (p_ptr->pet_extra_flags & PF_TELEPORT) p_ptr->pet_extra_flags &= ~(PF_TELEPORT);
			else p_ptr->pet_extra_flags |= (PF_TELEPORT);
			break;
		}
		/* flag - allow pets to cast attack spell */
		case PET_ATTACK_SPELL:
		{
			if (p_ptr->pet_extra_flags & PF_ATTACK_SPELL) p_ptr->pet_extra_flags &= ~(PF_ATTACK_SPELL);
			else p_ptr->pet_extra_flags |= (PF_ATTACK_SPELL);
			break;
		}
		/* flag - allow pets to cast disintegration spell */
		case PET_DISI_SPELL:
		{
			if (p_ptr->pet_extra_flags & PF_DISI_SPELL) p_ptr->pet_extra_flags &= ~(PF_DISI_SPELL);
			else p_ptr->pet_extra_flags |= (PF_DISI_SPELL);
			break;
		}
		/* flag - allow pets to cast attack spell */
		case PET_SUMMON_SPELL:
		{
			if (p_ptr->pet_extra_flags & PF_SUMMON_SPELL) p_ptr->pet_extra_flags &= ~(PF_SUMMON_SPELL);
			else p_ptr->pet_extra_flags |= (PF_SUMMON_SPELL);
			break;
		}
		/* flag - allow pets to cast attack spell */
		case PET_BALL_SPELL:
		{
			if (p_ptr->pet_extra_flags & PF_BALL_SPELL) p_ptr->pet_extra_flags &= ~(PF_BALL_SPELL);
			else p_ptr->pet_extra_flags |= (PF_BALL_SPELL);
			break;
		}

		case PET_RIDING:
		{
			do_riding(FALSE);
			break;
		}

		case PET_NAME:
		{
			do_name_pet();
			break;
		}

		case PET_RYOUTE:
		{
			if (p_ptr->pet_extra_flags & PF_RYOUTE) p_ptr->pet_extra_flags &= ~(PF_RYOUTE);
			else p_ptr->pet_extra_flags |= (PF_RYOUTE);
			p_ptr->update |= (PU_BONUS);
			handle_stuff();
			break;
		}
	}
}
