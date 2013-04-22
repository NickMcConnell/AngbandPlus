/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



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
static int get_spell(int *sn, cptr prompt, int b_idx, bool known)
{
	int i;

	int spell = -1;
	int num = 0;

	byte spells[MAX_N_IDX];

	bool flag, redraw, okay, ask;
	char choice;

	spell_type *s_ptr;

	book_type *b_ptr = &b_info[b_idx];

	char out_val[160];

	cptr p = b_name + b_ptr->name;


	/* Extract spells */
	for (spell = 0; spell < MAX_N_IDX; spell++)
	{
		/* Check for this spell */
		if (b_ptr->avail[spell / 32] & (1L << (spell % 32)))
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
		if (spell_okay(spells[i], known)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);


	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

#if 0
	/* Show the list */
	if (redraw)
	{
		/* Save screen */
		screen_save();

		/* Display a list of spells */
		print_spells(spells, num, 1, 20);
	}

#endif


	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
	        p, I2A(0), I2A(num - 1), prompt, p);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Hide the list */
			if (redraw)
			{
				/* Load screen */
				screen_load();

				/* Hide list */
				redraw = FALSE;
			}

			/* Show the list */
			else
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of spells */
				print_spells(spells, num, 1, 20);
			}

			/* Ask again */
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
			bell("Illegal spell choice!");
			continue;
		}

		/* Save the spell index */
		spell = spells[i];

		/* Require "okay" spells */
		if (!spell_okay(spell, known))
		{
			bell("Illegal spell choice!");
			msg_format("You may not %s that %s.", prompt, p);
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Access the spell */
			s_ptr = &mp_ptr->spells[spell];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, n_name + n_info[spell].name,
			        s_ptr->smana, spell_chance(spell));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw)
	{
		/* Load screen */
		screen_load();

		/* Hack -- forget redraw */
		/* redraw = FALSE; */
	}


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

	/* Success */
	return (TRUE);
}


/*
 * Hook to specify "legal spell book"
 */
static bool item_tester_hook_book(object_type *o_ptr)
{
	/* Use "book_okay" function */
	return book_okay(o_ptr->tval, o_ptr->sval);
}


/*
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int item, b_idx;

	int spell = -1;
	int num = 0;

	byte spells[MAX_N_IDX];

	object_type *o_ptr;

	book_type *b_ptr;

	cptr q, s;


	/* Warriors are illiterate */
	if (mp_ptr->spell_first == 99)
	{
		msg_print("You cannot read books!");
		return;
	}

#if 0

	/* No lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

#endif

	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_hook_book;

	/* Get an item */
	q = "Browse which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	/* Access the book index */
	b_idx = lookup_book(o_ptr->tval, o_ptr->sval);

	/* Get book pointer */
	b_ptr = &b_info[b_idx];


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < MAX_N_IDX; spell++)
	{
		/* Check for this spell */
		if (b_ptr->avail[spell / 32] & (1L << (spell % 32)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save screen */
	screen_save();

	/* Display the spells */
	print_spells(spells, num, 1, 20);

	/* Prompt for a command */
	put_str("(Browsing) Command: ", 0, 0);

        /* Hack -- Get a new command */
        p_ptr->command_new = inkey();

	/* Load screen */
	screen_load();


	/* Hack -- Process "Escape" */
	if (p_ptr->command_new == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new = 0;
	}
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, item, b_idx;

	int spell = -1;

	cptr p;

	cptr q, s;

	object_type *o_ptr;

	book_type *b_ptr;


	if (mp_ptr->spell_first == 99)
	{
		msg_print("You cannot read books!");
		return;
	}

	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	if (!(p_ptr->new_spells))
	{
		msg_print("You cannot learn any new spells!");
		return;
	}


	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_hook_book;

	/* Get an item */
	q = "Study which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	/* Call script */
	if (perform_event(EVENT_STUDY, Py_BuildValue("(i)", item)))
	{
		/* Script calls for abort */
		return;
	}

	/* Access the book index */
	b_idx = lookup_book(o_ptr->tval, o_ptr->sval);

	/* Get book pointer */
	b_ptr = &b_info[b_idx];

	/* Get type of spell */
	p = b_name + b_ptr->name;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Hack -- Learn a random prayer */
	if (mp_ptr->spell_flags & PM_RANDOM)
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < MAX_N_IDX; spell++)
		{
			/* Check spells in the book */
			if (b_ptr->avail[spell / 32] & (1L << (spell % 32)))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(spell, FALSE)) continue;

				/* Apply the randomizer */
				if ((++k > 1) && (rand_int(k) != 0)) continue;

				/* Track it */
				gift = spell;
			}
		}

		/* Accept gift */
		spell = gift;
	}
	else
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", b_idx, FALSE) && (spell == -1)) return;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msg_format("You cannot learn any %ss in that book.", p);

		/* Abort */
		return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Learn the spell */
	p_ptr->spell_learned[spell / 32] |= (1L << (spell % 32));

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < MAX_N_IDX; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i] == 999) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i++] = spell;

	/* Mention the result */
	msg_format("You have learned the %s of %s.",
	           p, n_name + n_info[spell].name);

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more %s%s.",
		           p_ptr->new_spells, p,
		           (p_ptr->new_spells != 1) ? "s" : "");
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}


/*
 * Brand the current weapon
 */
static void brand_weapon(void)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr)))
	{
		cptr act = NULL;

		char o_name[80];

		if (rand_int(100) < 25)
		{
			act = "is covered in a fiery shield!";
			o_ptr->name2 = EGO_BRAND_FIRE;
		}

		else
		{
			act = "glows deep, icy blue!";
			o_ptr->name2 = EGO_BRAND_COLD;
		}

		object_desc(o_name, o_ptr, FALSE, 0);

		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();
		msg_print("The Branding failed.");
	}
}


/*
 * Actually produce the effects of a spell
 */
static void cast_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;
	int beam;

	int plev = p_ptr->lev;

	/* Hack -- chance of "beam" instead of "bolt" */
	beam = ((mp_ptr->spell_flags & PM_POWERFUL) ? plev : (plev / 2));

	/* Event */
	if (perform_event(EVENT_SPELL, Py_BuildValue("(i)", spell)))
	{
		/* Abort */
		return;
	}

	/* Spells */
	switch (spell)
	{
		case 0:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
					  damroll(3 + ((plev - 1) / 5), 4));
			break;
		}

		case 1:
		{
			(void)detect_monsters_normal();
			break;
		}

		case 2:
		{
			teleport_player(10);
			break;
		}

		case 3:
		{
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		}

		case 4:
		{
			(void)detect_treasure();
			(void)detect_objects_gold();
			break;
		}

		case 5:
		{
			(void)hp_player(damroll(2, 8));
			(void)set_cut(p_ptr->cut - 15);
			break;
		}

		case 6:
		{
			(void)detect_objects_normal();
			break;
		}

		case 7:
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}

		case 8:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
				  10 + (plev / 2), 2);
			break;
		}

		case 9:
		{
			if (!get_aim_dir(&dir)) return;
			(void)confuse_monster(dir, plev);
			break;
		}

		case 10:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_ELEC, dir,
					  damroll(3+((plev-5)/4), 8));
			break;
		}

		case 11:
		{
			(void)destroy_doors_touch();
			break;
		}

		case 12:
		{
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir);
			break;
		}

		case 13:
		{
			(void)set_poisoned(0);
			break;
		}

		case 14:
		{
			teleport_player(plev * 5);
			break;
		}

		case 15:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			break;
		}

		case 16:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
					  damroll(5+((plev-5)/4), 8));
			break;
		}

		case 17:
		{
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
			break;
		}

		case 18:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}

		case 19:
		{
			(void)recharge(5);
			break;
		}

		case 20:
		{
			(void)sleep_monsters_touch();
			break;
		}

		case 21:
		{
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
			break;
		}

		case 22:
		{
			(void)ident_spell();
			break;
		}

		case 23:
		{
			(void)sleep_monsters();
			break;
		}

		case 24:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
					  damroll(8+((plev-5)/4), 8));
			break;
		}

		case 25:
		{
			if (!get_aim_dir(&dir)) return;
			(void)slow_monster(dir);
			break;
		}

		case 26:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
				  30 + (plev), 2);
			break;
		}

		case 27:
		{
			(void)recharge(40);
			break;
		}

		case 28:
		{
			if (!get_aim_dir(&dir)) return;
			(void)teleport_monster(dir);
			break;
		}

		case 29:
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20) + plev);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(5));
			}
			break;
		}

		case 30:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir,
				  55 + (plev), 2);
			break;
		}

		case 31:
		{
			destroy_area(py, px, 15, TRUE);
			break;
		}

		case 32:
		{
			(void)genocide();
			break;
		}

		case 33:
		{
			(void)door_creation();
			break;
		}

		case 34:
		{
			(void)stair_creation();
			break;
		}

		case 35:
		{
			(void)teleport_player_level();
			break;
		}

		case 36:
		{
			earthquake(py, px, 10);
			break;
		}

		case 37:
		{
			if (!p_ptr->word_recall)
			{
				p_ptr->word_recall = rand_int(20) + 15;
				msg_print("The air about you becomes charged...");
			}
			else
			{
				p_ptr->word_recall = 0;
				msg_print("A tension leaves the air around you...");
			}
			break;
		}

		case 38:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_ACID, dir,
					  damroll(6+((plev-5)/4), 8));
			break;
		}

		case 39:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
				  20 + (plev / 2), 3);
			break;
		}

		case 40:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ACID, dir,
				  40 + (plev), 2);
			break;
		}

		case 41:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
				  70 + (plev), 3);
			break;
		}

		case 42:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_METEOR, dir,
				  65 + (plev), 3);
			break;
		}

		case 43:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_MANA, dir,
				  300 + (plev * 2), 3);
			break;
		}

		case 44:
		{
			(void)detect_monsters_evil();
			break;
		}

		case 45:
		{
			(void)detect_objects_magic();
			break;
		}

		case 46:
		{
			recharge(100);
			break;
		}

		case 47:
		{
			(void)genocide();
			break;
		}

		case 48:
		{
			(void)mass_genocide();
			break;
		}

		case 49:
		{
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			break;
		}

		case 50:
		{
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			break;
		}

		case 51:
		{
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
			break;
		}

		case 52:
		{
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
			break;
		}

		case 53:
		{
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
			break;
		}

		case 54:
		{
			(void)hp_player(10);
			(void)set_hero(p_ptr->hero + randint(25) + 25);
			(void)set_afraid(0);
			break;
		}

		case 55:
		{
			(void)set_shield(p_ptr->shield + randint(20) + 30);
			break;
		}

		case 56:
		{
			(void)hp_player(30);
			(void)set_shero(p_ptr->shero + randint(25) + 25);
			(void)set_afraid(0);
			break;
		}

		case 57:
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(30) + 30 + plev);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(10));
			}
			break;
		}

		case 58:
		{
			(void)set_invuln(p_ptr->invuln + randint(8) + 8);
			break;
		}

		case 64:
		{
			(void)detect_monsters_evil();
			break;
		}

		case 65:
		{
			(void)hp_player(damroll(2, 10));
			(void)set_cut(p_ptr->cut - 10);
			break;
		}

		case 66:
		{
			(void)set_blessed(p_ptr->blessed + randint(12) + 12);
			break;
		}

		case 67:
		{
			(void)set_afraid(0);
			break;
		}

		case 68:
		{
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		}

		case 69:
		{
			(void)detect_traps();
			break;
		}

		case 70:
		{
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}

		case 71:
		{
			(void)set_poisoned(p_ptr->poisoned / 2);
			break;
		}

		case 72:
		{
			if (!get_aim_dir(&dir)) return;
			(void)fear_monster(dir, plev);
			break;
		}

		case 73:
		{
			teleport_player(plev * 3);
			break;
		}

		case 74:
		{
			(void)hp_player(damroll(4, 10));
			(void)set_cut((p_ptr->cut / 2) - 20);
			break;
		}

		case 75:
		{
			(void)set_blessed(p_ptr->blessed + randint(24) + 24);
			break;
		}

		case 76:
		{
			(void)sleep_monsters_touch();
			break;
		}

		case 77:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}

		case 78:
		{
			remove_curse();
			break;
		}

		case 79:
		{
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
			break;
		}

		case 80:
		{
			(void)set_poisoned(0);
			break;
		}

		case 81:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_HOLY_ORB, dir,
				  (damroll(3, 6) + plev +
				   (plev / ((mp_ptr->spell_flags & PM_POWERFUL) ? 2 : 4))),
				  ((plev < 30) ? 2 : 3));
			break;
		}

		case 82:
		{
			(void)hp_player(damroll(6, 10));
			(void)set_cut(0);
			break;
		}

		case 83:
		{
			(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
			break;
		}

		case 84:
		{
			(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
			break;
		}

		case 85:
		{
			earthquake(py, px, 10);
			break;
		}

		case 86:
		{
			map_area();
			break;
		}

		case 87:
		{
			(void)hp_player(damroll(8, 10));
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}

		case 88:
		{
			(void)turn_undead();
			break;
		}

		case 89:
		{
			(void)set_blessed(p_ptr->blessed + randint(48) + 48);
			break;
		}

		case 90:
		{
			(void)dispel_undead(randint(plev * 3));
			break;
		}

		case 91:
		{
			(void)hp_player(300);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}

		case 92:
		{
			(void)dispel_evil(randint(plev * 3));
			break;
		}

		case 93:
		{
			warding_glyph();
			break;
		}

		case 94:
		{
			(void)dispel_evil(randint(plev * 4));
			(void)hp_player(1000);
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}

		case 95:
		{
			(void)detect_monsters_normal();
			break;
		}

		case 96:
		{
			(void)detect_all();
			break;
		}

		case 97:
		{
			(void)ident_spell();
			break;
		}

		case 98:
		{
			(void)probing();
			break;
		}

		case 99:
		{
			wiz_lite();
			break;
		}

		case 100:
		{
			(void)hp_player(damroll(4, 10));
			(void)set_cut(0);
			break;
		}

		case 101:
		{
			(void)hp_player(damroll(8, 10));
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}

		case 102:
		{
			(void)hp_player(2000);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}

		case 103:
		{
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			break;
		}

		case 104:
		{
			(void)restore_level();
			break;
		}

		case 105:
		{
			(void)dispel_undead(randint(plev * 4));
			break;
		}

		case 106:
		{
			(void)dispel_evil(randint(plev * 4));
			break;
		}

		case 107:
		{
			if (banish_evil(100))
			{
				msg_print("The power of your god banishes evil!");
			}
			break;
		}

		case 108:
		{
			destroy_area(py, px, 15, TRUE);
			break;
		}

		case 109:
		{
			if (!get_aim_dir(&dir)) return;
			drain_life(dir, 200);
			break;
		}

		case 110:
		{
			(void)destroy_doors_touch();
			break;
		}

		case 111:
		{
			(void)recharge(15);
			break;
		}

		case 112:
		{
			(void)remove_all_curse();
			break;
		}

		case 113:
		{
			(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
			break;
		}

		case 114:
		{
			(void)enchant_spell(0, 0, rand_int(3) + 2);
			break;
		}

		case 115:
		{
			brand_weapon();
			break;
		}

		case 116:
		{
			teleport_player(10);
			break;
		}

		case 117:
		{
			teleport_player(plev * 8);
			break;
		}

		case 118:
		{
			if (!get_aim_dir(&dir)) return;
			(void)teleport_monster(dir);
			break;
		}

		case 119:
		{
			(void)teleport_player_level();
			break;
		}

		case 120:
		{
			if (p_ptr->word_recall == 0)
			{
				p_ptr->word_recall = rand_int(20) + 15;
				msg_print("The air about you becomes charged...");
			}
			else
			{
				p_ptr->word_recall = 0;
				msg_print("A tension leaves the air around you...");
			}
			break;
		}

		case 121:
		{
			msg_print("The world changes!");

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}
	}
}

/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int item, b_idx;
	int chance;
	
	int spell;

	object_type *o_ptr;

	spell_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if (mp_ptr->spell_first == 99)
	{
		msg_print("You cannot cast spells!");
		return;
	}

	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Restrict choices to spell books */
	item_tester_hook = item_tester_hook_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	/* Access the book index */
	b_idx = lookup_book(o_ptr->tval, o_ptr->sval);


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Ask for a spell */
	if (!get_spell(&spell, "cast", b_idx, TRUE))
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return;
	}


	/* Access the spell */
	s_ptr = &mp_ptr->spells[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");
	}

	/* "Cast spell" event */
	else if (perform_event(EVENT_CAST, Py_BuildValue("(ii)", spell, 0)))
	{
		/* Abort */
		return;
	}

	/* Process spell */
	else
	{
		/* Spell effect */
		cast_spell(spell);

		/* A spell was cast */
		if (!(p_ptr->spell_worked[spell / 32] & (1L << (spell % 32))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_worked[spell / 32] |= (1L << (spell % 32));

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (s_ptr->smana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= s_ptr->smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = s_ptr->smana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_SPELL | PW_PLAYER);
}


