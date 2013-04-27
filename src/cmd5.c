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

#define tval2realm(A) ((A) - TV_LIFE_BOOK + 1)

static const char *spell_tips[MAX_REALM][MAX_SPELLS] =
{
#ifdef JP
	{
		"近くの邪悪なモンスターを感知する。",
		"怪我と体力を少し回復させる。",
		"一定時間、命中率とACにボーナスを得る。",
		"光源が照らしている範囲か部屋全体を永久に明るくする。",
		"近くの全ての罠と扉と階段を感知する。",
		"満腹にする。",
		"聖なる力をもつ宝珠を放つ。邪悪なモンスターに対して大きなダメージを与えるが、善良なモンスターには効果がない。",
		"一定時間、炎、冷気に対する耐性を得る。装備による耐性に累積する。",

		"体力を大幅に回復させ、負傷と朦朧状態も全快する。",
		"アイテムにかかった弱い呪いを解除する。",
		"すべてのステータスと経験値を回復する。",
		"周辺の地形を感知する。",
		"視界内の全ての邪悪なモンスターにダメージを与える。",
		"極めて強力な回復呪文で、負傷と朦朧状態も全快する。",
		"武器を祝福する。強力な呪いのかかった武器には抵抗される。アーティファクトを祝福しようとして失敗すると劣化する。",
		"閃光の球を放つ。",
	},
	{
		"弱い魔法の矢を放つ。",
		"近距離のテレポートをする。",
		"近くの全ての見えるモンスターを感知する。",
		"光源が照らしている範囲か部屋全体を永久に明るくする。",
		"遠距離のテレポートをする。",
		"魔法の球を放つ。",
		"周辺の地形を感知する。",
		"モンスター1体をテレポートさせる。抵抗されると無効。",

		"壁を溶かして床にする。",
		"地上にいるときはダンジョンの最深階へ、ダンジョンにいるときは地上へと移動する。",
		"アイテムを1つ識別する。",
		"生物に有効な強力な矢を放つ。",
		"一定時間、テレパシー能力を得る。",
		"一定時間、加速する。",
		"近くの全てのモンスター、罠、扉、階段、財宝、そしてアイテムを感知する。",
		"巨大な地獄の炎の球を放つ。善良なモンスターに対してさらに大きなダメージを与える。",
	},
#else
	{
		"Detects all evil monsters in your vicinity.",
		"Heals cut and HP a little.",
		"Gives bonus to hit and AC for a few turns.",
		"Lights up nearby area and the inside of a room permanently.",
		"Detects traps, doors, and stairs in your vicinity.",
		"Satisfies hunger.",
		"Fires a beam of light which damages to light-sensitive monsters.",
		"Gives resistance to fire and cold. These resistances can be added to which from equipment for more powerful resistances.",

		"Heals cut, stun and HP greatly.",
		"Removes normal curses from equipped items.",
		"Restores all stats and experience.",
		"Maps nearby area.",
		"Damages all evil monsters in sight.",
		"Much powerful healing magic, and heals cut and stun completely.",
		"Blesses a weapon. Heavy cursed weapons resist it. Artifacts are disenchanted when blessing is failed.",
		"Fires a ball of light.",
	},
	{
		"Fires a weak bolt of magic.",
		"Teleport short distance.",
		"Detects all monsters in your vicinity unless invisible.",
		"Lights up nearby area and the inside of a room permanently.",
		"Teleport long distance.",
		"Fires a ball of magic.",
		"Maps nearby area.",
		"Teleports all monsters on the line away unless resisted.",

		"Turns one rock square to mud.",
		"Recalls player from dungeon to town, or from town to the deepest level of dungeon.",
		"Identifies an item.",
		"Fires a beam of drain life.",
		"Gives telepathy for a while.",
		"Hastes you for a while.",
		"Detects all monsters, traps, doors, stairs, treasures and items in your vicinity.",
		"Fires an extremely huge ball of hell fire.",
	},
#endif
};

byte get_modified_smana(magic_type *s_ptr)
{
	int tmp_mana;

	if (!s_ptr) return 0;
	tmp_mana = s_ptr->smana;

	if (p_ptr->dec_mana) tmp_mana = tmp_mana * 3 / 4;
	if (tmp_mana < 1) tmp_mana = 1;

	return (byte)tmp_mana;
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

static int get_spell(int *sn, cptr prompt, int sval, bool known, int use_realm)
{
	int         i;
	int         spell;
	int         num = 0;
	int         ask;
	byte        spells[32];
	bool        flag, redraw, okay;
	char        choice;
	magic_type  *s_ptr;
	char        out_val[160];
#ifdef JP
	char        jverb_buf[128];
	cptr        p = ((mp_ptr->spell_type == ST_PRAYER) ? "祈り" : "呪文");
#else
	cptr        p = ((mp_ptr->spell_type == ST_PRAYER) ? "prayer" : "spell");
#endif


	/* Repeat previous command */
	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known, use_realm - 1))
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
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
		if (spell_okay(spells[i], known, use_realm - 1)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

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
	jverb( prompt, jverb_buf, JVERB_AND );
	(void) strnfmt(out_val, 78, "(%^s:%c-%c, '*'で一覧, ESCで中断) どの%sを%^sますか? ",
		p, I2A(0), I2A(num - 1), p, jverb_buf );
#else
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
		p, I2A(0), I2A(num - 1), prompt, p);
#endif

	/* Get a spell from the user */
	choice = always_show_list ? ESCAPE : 1;
	while (!flag)
	{
		if( choice==ESCAPE ) choice = ' '; 
		else if( !get_com(out_val, &choice) )break; 

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
				print_spells(spells, num, 1, 20, use_realm - 1);
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


		/* Note verify */
		ask = (isupper(choice));

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = spells[i];

		/* Require "okay" spells */
		if (!spell_okay(spell, known, use_realm - 1))
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

			/* Access the spell */
			s_ptr = &mp_ptr->info[use_realm - 1][spell % 32];

			/* Prompt */
#ifdef JP
			jverb( prompt, jverb_buf, JVERB_AND);
			/* 英日切り替え機能に対応 */
			(void)strnfmt(tmp_val, 78, "%s(MP%d, 失敗率%d%%)を%sますか? ",
				spell_names[use_realm -1][spell % 32],
				get_modified_smana(s_ptr), spell_chance(spell, use_realm -1), jverb_buf);
#else
			(void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, spell_names[use_realm - 1][spell % 32],
				get_modified_smana(s_ptr), spell_chance(spell, use_realm - 1));
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

	/* Remember the command for repeating */
	repeat_push(*sn);

	/* Success */
	return (TRUE);
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
	int		item, sval, use_realm, j, line;
	int		spell = -1;
	int		num = 0;

	byte		spells[32];
	char		temp[62 * 4];

	object_type	*o_ptr;

	cptr q, s;

	/* Warriors are illiterate */
	if (!(p_ptr->realm1 || p_ptr->realm2))
	{
#ifdef JP
		msg_print("本を読むことができない！");
#else
		msg_print("You cannot read books!");
#endif
		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_tval = TV_LIFE_BOOK;

	/* Get an item */
#ifdef JP
	q = "どの本を読みますか? ";
	s = "読める本がない。";
#else
	q = "Browse which book? ";
	s = "You have no books that you can read.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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
		if ((fake_spell_flags[sval] & (1L << spell)))
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
		if (!get_spell(&spell, "読む", o_ptr->sval, TRUE, use_realm))
#else
		if (!get_spell(&spell, "browse", o_ptr->sval, TRUE, use_realm))
#endif
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Display a list of spells */
			print_spells(spells, num, 1, 15, use_realm - 1);

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
		Term_erase(14, 18, 255);
		Term_erase(14, 17, 255);
		Term_erase(14, 16, 255);
		Term_erase(14, 15, 255);

		roff_to_buf(spell_tips[use_realm - 1][spell], 62, temp, sizeof(temp));
		for(j = 0, line = 15; temp[j]; j += (1 + strlen(&temp[j])))
		{
			prt(&temp[j], line, 15);
			line++;
		}
	}

	/* Restore the screen */
	screen_load();
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int	i, item, sval;
	int	increment = 0;

	/* Spells of realm2 will have an increment of +32 */
	int	spell = -1;

#ifdef JP
	cptr p = ((mp_ptr->spell_type == ST_PRAYER) ? "祈り" : "呪文");
#else
	cptr p = ((mp_ptr->spell_type == ST_PRAYER) ? "prayer" : "spell");
#endif

	object_type *o_ptr;

	cptr q, s;

	if (!p_ptr->realm1)
	{
#ifdef JP
		msg_print("本を読むことができない！");
#else
		msg_print("You cannot read books!");
#endif
		return;
	}

	if (p_ptr->blind || no_lite())
	{
#ifdef JP
		msg_print("目が見えない！");
#else
		msg_print("You cannot see!");
#endif
		return;
	}

	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて読めない！");
#else
		msg_print("You are too confused!");
#endif
		return;
	}

	if (!(p_ptr->new_spells))
	{
#ifdef JP
		msg_format("新しい%sを覚えることはできない！", p);
#else
		msg_format("You cannot learn any new %ss!", p);
#endif
		return;
	}

#ifdef JP
	if( p_ptr->new_spells < 10 )
		msg_format("あと %d つの%sを学べる。", p_ptr->new_spells, p);
	else
		msg_format("あと %d 個の%sを学べる。", p_ptr->new_spells, p);
#else
	msg_format("You can learn %d new %s%s.", p_ptr->new_spells, p,
		(p_ptr->new_spells == 1?"":"s"));
#endif
	msg_print(NULL);


	/* Restrict choices to "useful" books */
	item_tester_tval = TV_LIFE_BOOK;

	/* Get an item */
#ifdef JP
	q = "どの本から学びますか? ";
	s = "読める本がない。";
#else
	q = "Study which book? ";
	s = "You have no books that you can read.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

#if 0
	if (o_ptr->tval == REALM2_BOOK) increment = 32;
#endif

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_type != ST_PRAYER)
	{
		/* Ask for a spell, allow cancel */
#ifdef JP
		if (!get_spell(&spell, "学ぶ", sval, FALSE, tval2realm(o_ptr->tval))
		    && (spell == -1)) return;
#else
		if (!get_spell(&spell, "study", sval, FALSE, tval2realm(o_ptr->tval))
		    && (spell == -1)) return;
#endif
	}

	/* Priest -- Learn a random prayer */
	else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < 32; spell++)
		{
			/* Check spells in the book */
			if ((fake_spell_flags[sval] & (1L << spell)))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(spell, FALSE,
					(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1))) continue;

				/* Hack -- Prepare the randomizer */
				k++;

				/* Hack -- Apply the randomizer */
				if (randint0(k) == 0) gift = spell;
			}
		}

		/* Accept gift */
		spell = gift;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
#ifdef JP
		msg_format("その本には学ぶべき%sがない。", p);
#else
		msg_format("You cannot learn any %ss in that book.", p);
#endif
		/* Abort */
		return;
	}


	/* Take a turn */
	energy_use = 100;

	if (increment) spell += increment;

	/* Learn the spell */
	if (spell < 32)
	{
		spell_learned1 |= (1L << spell);
	}
	else
	{
		spell_learned2 |= (1L << (spell - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 64; i++)
	{
		/* Stop at the first empty space */
		if (spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	spell_order[i] = spell;

	/* Mention the result */
#ifdef JP
	/* 英日切り替え機能に対応 */
		msg_format("%sの%sを学んだ。",
			    spell_names
		[(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)][spell % 32] ,p);
#else
	msg_format("You have learned the %s of %s.",
		p, spell_names
		[(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)][spell % 32]);
#endif

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
#ifdef JP
			if( p_ptr->new_spells < 10 )
				msg_format("あと %d つの%sを学べる。", p_ptr->new_spells, p);
			else
				msg_format("あと %d 個の%sを学べる。", p_ptr->new_spells, p);
#else
		msg_format("You can learn %d more %s%s.",
			p_ptr->new_spells, p,
			(p_ptr->new_spells != 1) ? "s" : "");
#endif
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}


static bool cast_life_spell(int spell)
{
	int	dir;
	int	plev = p_ptr->lev;

	switch (spell)
	{
		/*** First book ***/
	case 0: /* Detect Evil */
		(void)detect_monsters_evil(DETECT_RAD_DEFAULT);
		break;
	case 1: /* Cure Light Wounds */
		(void)hp_player(damroll(2, 10));
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 2: /* Bless */
		(void)set_blessed(p_ptr->blessed + randint1(12) + 12);
		break;
	case 3: /* Call Light */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 4: /* Detect Traps + Secret Doors */
		(void)detect_traps(DETECT_RAD_DEFAULT);
		(void)detect_doors(DETECT_RAD_DEFAULT);
		(void)detect_stairs(DETECT_RAD_DEFAULT);
		break;
	case 5: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 6: /* Holy Orb */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_HOLY_FIRE, dir,
			  (damroll(3, 6) + plev +
			  (plev / ((p_ptr->pclass == CLASS_PRIEST) ? 2 : 4))),
			  2);
		break;
	case 7: /* Resistance Heat & Cold */
		(void)set_oppose_fire(p_ptr->oppose_fire + randint1(20) + 20);
		(void)set_oppose_cold(p_ptr->oppose_cold + randint1(20) + 20);
		break;

		/*** Second Book ***/
	case 8: /* Cure Critical Wounds */
		(void)hp_player(damroll(8, 10));
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 9: /* Remove Curse */
		if (remove_curse())
		{
#ifdef JP
			msg_print("誰かに見守られているような気がする。");
#else
			msg_print("You feel as if someone is watching over you.");
#endif
		}
		break;
	case 10: /* Restoration */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		break;
	case 11: /* Sense Surrounding */
		map_area(DETECT_RAD_MAP);
		break;
	case 12: /* Dispel Evil */
		(void)dispel_evil(plev * 3);
		break;
	case 13: /* Healing */
		(void)hp_player(200);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 14: /* Bless Weapon */
		return bless_weapon();
		break;
	case 15: /* Star Burst */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_LITE, dir, (100 + plev * 4), 3);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明なライフの呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Life spell: %d.", spell);
#endif
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_sorcery_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else beam = plev / 2;

	switch (spell)
	{
		/*** First Book ***/
	case 0: /* Magic Missile */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
			damroll(3 + ((plev - 1) / 5), 4));
		break;
	case 1: /* Phase Door */
		teleport_player(10);
		break;
	case 2: /* Detect Monsters */
		(void)detect_monsters_normal(DETECT_RAD_DEFAULT);
		break;
	case 3: /* Light Area */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 4: /* Teleport Self */
		teleport_player(plev * 5);
		break;
	case 5: /* Manaburst */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_MISSILE, dir,
			  (damroll(3, 5) + plev +
			   (plev / ((p_ptr->pclass == CLASS_MAGE) ? 2 : 4))),
			   2);
			/* Shouldn't actually use GF_MANA, as it will destroy all
			 * items on the floor */
		break;
	case 6: /* Magic Mapping */
		map_area(DETECT_RAD_MAP);
		break;
	case 7: /* Teleport Other */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_AWAY_ALL, dir, plev);
		break;

		/*** Second Book ***/
	case 8: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)wall_to_mud(dir);
		break;
	case 9: /* Word of Recall */
		word_of_recall();
		break;
	case 10: /* Identify */
		return ident_spell();
	case 11: /* Bolt of Drain Life */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_bolt(GF_OLD_DRAIN, dir, 70 + p_ptr->lev * 2);
		break;
	case 12: /* Sense Minds */
		(void)set_tim_esp(p_ptr->tim_esp + randint1(30) + 25);
		break;
	case 13: /* Haste Self */
		(void)set_fast(randint1(20 + plev) + plev);
		break;
	case 14: /* Detection */
		(void)detect_all(DETECT_RAD_DEFAULT);
		break;
	case 15: /* Hell Fire */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_HELL_FIRE, dir, 111 + (plev * 6), 4);
		take_hit(7 + randint1(13), "呪文を唱えた疲労");
		break;
	default:
#ifdef JP
		msg_format("あなたは不明なソーサリーの呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Sorcery spell: %d.", spell);
#endif
		msg_print(NULL);
	}

	return TRUE;
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int	item, sval, spell, realm;
	int	chance;
	int	increment = 0;
	int	use_realm;
	bool cast;
	byte use_mana;

#ifdef JP
	const cptr prayer = ((mp_ptr->spell_type == ST_PRAYER) ? "祈り" : "呪文");
#else
	const cptr prayer = ((mp_ptr->spell_type == ST_PRAYER) ? "prayer" : "spell");
#endif

	object_type	*o_ptr;

	magic_type	*s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (!p_ptr->realm1)
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
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて学べない！");
#else
		msg_print("You are too confused!");
#endif
		return;
	}

	/* Restrict choices to spell books */
	item_tester_tval = TV_LIFE_BOOK;

	/* Get an item */
#ifdef JP
	q = "どの呪文書を使いますか? ";
	s = "呪文書がない！";
#else
	q = "Use which book? ";
	s = "You have no spell books!";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

#if 0
	if (o_ptr->tval == REALM2_BOOK) increment = 32;
#endif

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (increment) realm = p_ptr->realm2;
	else realm = p_ptr->realm1;

	/* Ask for a spell */
#ifdef JP
	if (!get_spell(&spell,  
		((mp_ptr->spell_type == ST_PRAYER) ? "詠唱する" : "唱える"),
		sval, TRUE, realm))
	{
		if (spell == -2) msg_format("その本には知っている%sがない。", prayer);
		return;
	}
#else
	if (!get_spell(&spell, ((mp_ptr->spell_type == ST_PRAYER) ? "recite" : "cast"),
		sval, TRUE, realm))
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}
#endif



	/* Access the spell */
	use_realm = (increment?p_ptr->realm2:p_ptr->realm1);

	s_ptr = &mp_ptr->info[use_realm - 1][spell];
	use_mana = get_modified_smana(s_ptr);

	/* Verify "dangerous" spells */
	if (use_mana > p_ptr->csp)
	{
		/* Warning */
#ifdef JP
		msg_format("その%sを%sのに十分なマジックポイントがない。",prayer,
			((mp_ptr->spell_type == ST_PRAYER) ? "詠唱する" : "唱える"));
#else
		msg_format("You do not have enough mana to %s this %s.",
			((mp_ptr->spell_type == ST_PRAYER) ? "recite" : "cast"),
			prayer);
#endif
		if (!over_exert) return;

		/* Verify */
#ifdef JP
		if (!get_check("それでも挑戦しますか? ")) return;
#else
		if (!get_check("Attempt it anyway? ")) return;
#endif
	}


	/* Spell failure chance */
	chance = spell_chance(spell, use_realm - 1);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

#ifdef JP
		msg_format("%sをうまく唱えられなかった！", prayer);
#else
		msg_format("You failed to get the %s off!", prayer);
#endif
 		sound(SOUND_STORE2);  /* (Sound substitute) HACK! No fail sound, use strore 2*/
	}

	/* Process spell */
	else
	{
		/* Spells.  */
		switch (realm)
		{
		case REALM_LIFE: /* * LIFE * */
			cast = cast_life_spell(spell);
			break;
		case REALM_SORCERY: /* * SORCERY * */
			cast = cast_sorcery_spell(spell);
			break;
		default:
			cast = FALSE;
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;

		/* A spell was cast */
		sound(SOUND_SPELL);
		if (!(increment ?
		    (spell_worked2 & (1L << spell)) :
		    (spell_worked1 & (1L << spell))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (realm == p_ptr->realm1)
			{
				spell_worked1 |= (1L << spell);
			}
			else
			{
				spell_worked2 |= (1L << spell);
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}

	/* Take a turn */
	energy_use = 100;

	/* Sufficient mana */
	if (use_mana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= use_mana;
	}

	/* Over-exert the player */
	else
	{
		int oops = use_mana - p_ptr->csp;

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
	p_ptr->window |= (PW_PLAYER | PW_STATS);
	p_ptr->window |= (PW_SPELL);
}


/*
 * Pray a prayer -- Unused in Zangband
 */
void do_cmd_pray(void)
{
	msg_print("Praying is not used in Zangband. Use magic spell casting instead.");
}



int calculate_upkeep(void)
{
	int i;

	monster_type    *m_ptr;
	monster_race    *r_ptr;

	/* int old_total_friends = total_friends; */
	s32b old_friend_align = friend_align;

	/* Clear some variables */
	total_friends = 0;
	total_friend_levels = 0;
	friend_align = 0;

	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Calculate "upkeep" for pets */
		if (is_pet(m_ptr))
		{
			total_friends++;
			if (r_ptr->flags1 & (RF1_UNIQUE))
				total_friend_levels += (r_ptr->level + 5) * 10;
			else
				total_friend_levels += r_ptr->level;

			/* Determine pet alignment */
			if (r_ptr->flags3 & RF3_GOOD)
			{
				friend_align += r_ptr->level;
			}
			else if (r_ptr->flags3 & RF3_EVIL)
			{
				friend_align -= r_ptr->level;
			}
		}
	}

	if (old_friend_align != friend_align) p_ptr->update |= (PU_BONUS);
	
	if (total_friends)
	{
#ifdef TRACK_FRIENDS
		if (wizard)
#ifdef JP
			msg_format("友好的モンスター: %d 体", total_friends);
#else
			msg_format("Total friends: %d.", total_friends);
#endif

#endif /* TRACK_FRIENDS */

		if (total_friends)
		{
			int upkeep_factor = (total_friend_levels - (p_ptr->lev * 80 / cp_ptr->pet_upkeep_div));

			if (upkeep_factor > 1000) upkeep_factor = 1000;
			else if (upkeep_factor < 0) upkeep_factor = 0;

#ifdef TRACK_FRIENDS
			if (wizard)
#ifdef JP
			msg_format("レベル %d, 維持ＭＰ %d", total_friend_levels, upkeep_factor);
#else
			msg_format("Levels %d, upkeep %d", total_friend_levels, upkeep_factor);
#endif

#endif /* TRACK_FRIENDS */

			return upkeep_factor;
		}
	}

	return 0;
}

/*
 * Dismiss pets
 */
void do_cmd_pet_dismiss(void)
{
	int pet_ctr;
	bool all_pets = FALSE;
	int Dismissed = 0;
	monster_type *m_ptr;

#ifdef JP
	if (get_check("すべてのペットを放しますか？")) all_pets = TRUE;
#else
	if (get_check("Dismiss all pets? ")) all_pets = TRUE;
#endif

	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr))
		{
			bool delete_this = FALSE;

			if (all_pets)
				delete_this = TRUE;
			else
			{
				char friend_name[80], check_friend[80];
				monster_desc(friend_name, m_ptr, 0x80);

				/* Hack -- health bar for this monster */
				health_track(pet_ctr);

				/* Hack -- handle stuff */
				handle_stuff();

#ifdef JP
				sprintf(check_friend, "%sを放しますか？ [Yes/No/All]", friend_name);
#else
				sprintf(check_friend, "Dismiss %s? [Yes/No/All]", friend_name);
#endif
				prt(check_friend, 0, 0);

				if (m_ptr->ml)
					move_cursor_relative(m_ptr->fy, m_ptr->fx);

				while (TRUE)
				{
					int ch = inkey();

					if ((ch == 'Y') || (ch == 'y'))
					{
						delete_this = TRUE;
						break;
					}
					else if ((ch == 'A') || (ch == 'a'))
					{
						delete_this = TRUE;
						all_pets = TRUE;
						break;
					}
					else if ((ch == ESCAPE) || (ch == 'N') || (ch == 'n'))
						break;

					bell();
				}
			}

			if (delete_this)
			{
				delete_monster_idx(pet_ctr);
				Dismissed++;
			}
		}
	}

	Term_erase(0, 0, 255);
#ifdef JP
	msg_format("%d 匹のペットを放しました。", Dismissed);
#else
	msg_format("You have dismissed %d pet%s.", Dismissed,
		(Dismissed == 1 ? "" : "s"));
#endif
	return;
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

	num = 0;

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr)) pets++;
	}

	if (pets)
	{
#ifdef JP
		power_desc[num] = "ペットを放す";
#else
		power_desc[num] = "dismiss pets";
#endif
		powers[num++] = PET_DISMISS;
	}

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

	if (p_ptr->pet_open_doors)
	{
#ifdef JP
		power_desc[num] = "ドアを開けさせる";
#else
		power_desc[num] = "pets may open doors";
#endif
	}
	else
	{
#ifdef JP
		power_desc[num] = "ドアを開けさせない";
#else
		power_desc[num] = "pets may not open doors";
#endif
	}
	powers[num++] = PET_OPEN_DOORS;

	if (p_ptr->pet_pickup_items)
	{
#ifdef JP
		power_desc[num] = "アイテムを拾わせる";
#else
		power_desc[num] = "pets may pick up items";
#endif
	}
	else
	{
#ifdef JP
		power_desc[num] = "アイテムを拾わせない";
#else
		power_desc[num] = "pets may not pick up items";
#endif

	}
	powers[num++] = PET_TAKE_ITEMS;

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
		sprintf(buf, "%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]);
		prt(buf, y + ctr, x);
		ctr++;
	}

	if (ctr < 17)
	{
		prt("", y + ctr, x);
	}
	else
	{
		prt("", y + 17, x);
	}

	/* Get a command from the user */
	while (!flag && get_com(out_val, &choice))
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

				if (ctr < 17)
				{
					prt("", y + ctr, x);
				}
				else
				{
					prt("", y + 17, x);
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
			do_cmd_pet_dismiss();
			break;
		}
		/* Call pets */
		case PET_STAY_CLOSE:
		{
			p_ptr->pet_follow_distance = PET_CLOSE_DIST;
			break;
		}
		/* "Follow Me" */
		case PET_FOLLOW_ME:
		{
			p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
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
			p_ptr->pet_open_doors = !p_ptr->pet_open_doors;
			break;
		}
		/* flag - allow pets to pickup items */
		case PET_TAKE_ITEMS:
		{
			p_ptr->pet_pickup_items = !p_ptr->pet_pickup_items;

			/* Drop objects being carried by pets */
			if (!p_ptr->pet_pickup_items)
			{
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

			break;
		}
	}
}
