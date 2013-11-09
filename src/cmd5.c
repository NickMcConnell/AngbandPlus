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

	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();

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
			              do_spell(use_realm, spell, SPELL_NAME), use_mana,
			              spell_chance(spell, use_realm),jverb_buf);
#else
			(void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			              prompt, do_spell(use_realm, spell, SPELL_NAME), use_mana,
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


	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

	repeat_push(*sn);

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
	if (!class_info[p_ptr->pclass].realm_choices)
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

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
	{
		return;
	}

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

		roff_to_buf(do_spell(use_realm, spell, SPELL_DESC), 62, temp, sizeof temp);
		for(j=0, line = 18;temp[j];j+=(1+strlen(&temp[j])))
		{
			prt(&temp[j], line, 15);
			line++;
		}
	}

	/* Restore the screen */
	screen_load();
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

	cptr	prayer;

	object_type	*o_ptr;

	magic_type	*s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (!class_info[p_ptr->pclass].realm_choices)
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

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
	{
		return;
	}

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
		/* Canceled spells cost neither a turn nor mana */
		if (!do_spell(realm, spell, SPELL_CAST)) return;

		if (p_ptr->singing) stop_singing();

		switch (realm)
		{
		case REALM_HOLY: /* * HOLY * */
		case REALM_CRUSADE: /* * CRUSADE * */
			if (randint0(100) < s_ptr->slevel) change_your_alignment(ALI_GNE, 1);
			break;
		case REALM_SYMBIOTIC: /* * SYMBIOTIC * */
			if (randint0(100) < s_ptr->slevel) change_your_alignment(ALI_LNC, -1);
			break;
		case REALM_DEATH: /* * DEATH * */
		case REALM_WITCH: /* * WITCH * */
			if (randint0(100) < s_ptr->slevel) change_your_alignment(ALI_GNE, -1);
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
			project_hack(GF_HOLY_FIRE, randint1(MAX(p_ptr->align[ALI_GNE], 300)));
			no_effect = FALSE;
			break;
		}
		break;

	case ALIGN_GNE_EVIL:
		{
			int dam = 200 + randint1(MAX(0 - p_ptr->align[ALI_GNE], 300) / 500);
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

void check_pets_num_and_align(monster_type *m_ptr, bool inc)
{
	s32b old_friend_align_lnc = friend_align_lnc;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (inc)
	{
		total_friends++;
		if (r_ptr->flags3 & RF3_GOOD) friend_align_gne += r_ptr->level / 5;
		if (r_ptr->flags3 & RF3_EVIL) friend_align_gne -= r_ptr->level / 5;
		if (r_ptr->flags7 & RF7_LAWFUL) friend_align_lnc += r_ptr->level / 5;
		if (r_ptr->flags7 & RF7_CHAOTIC) friend_align_lnc -= r_ptr->level / 5;
	}
	else
	{
		total_friends--;
		if (r_ptr->flags3 & RF3_GOOD) friend_align_gne -= r_ptr->level / 5;
		if (r_ptr->flags3 & RF3_EVIL) friend_align_gne += r_ptr->level / 5;
		if (r_ptr->flags7 & RF7_LAWFUL) friend_align_lnc -= r_ptr->level / 5;
		if (r_ptr->flags7 & RF7_CHAOTIC) friend_align_lnc += r_ptr->level / 5;
	}

	if (old_friend_align_lnc != friend_align_lnc) p_ptr->update |= (PU_BONUS);
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
		monster_desc(friend_name, m_ptr, MD_ASSUME_VISIBLE);
		
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
				
				monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
				do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_DISMISS, m_name);
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
		if (MON_CSLEEP(m_ptr))
		{
			char m_name[80];
			monster_desc(m_name, m_ptr, 0);
			(void)set_monster_csleep(c_ptr->m_idx, 0);
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

					monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
					do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_NAME, m_name);
				}
			}
			else
			{
				if (record_named_pet && old_name)
				{
					char m_name[80];

					monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
					do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_UNNAME, m_name);
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
