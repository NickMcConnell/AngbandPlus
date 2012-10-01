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
static int get_spell(int *sn, cptr prompt, int sval, bool known)
{
	int i;

	int spell;
	int num = 0;

	byte spells[64];

	bool verify;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];
	
	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" :
	((mp_ptr->spell_book == TV_PSI_BOOK) ? "power" : "prayer"));

#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
		verify = (isupper(choice) ? TRUE : FALSE);

		/* Lowercase */
		choice = tolower(choice);

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
		if (verify)
		{
			char tmp_val[160];

			/* Get the spell */
			s_ptr = &mp_ptr->info[spell];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, spell_names[mp_ptr->spell_type][spell],
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

#ifdef ALLOW_REPEAT

	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
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
	int item, sval;

	int spell;
	int num = 0;

	byte spells[64];

	object_type *o_ptr;

	cptr q, s;


	/* Warriors are illiterate */
	if (!mp_ptr->spell_book)
	{
		msg_print("You cannot read books!");
		return;
	}

#if 0

	/* No lite */
	if ((p_ptr->blind || no_lite()) && (mp_ptr->spell_book != TV_PSI_BOOK))
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
	item_tester_tval = mp_ptr->spell_book;

	/* Set the messages right */
	if (mp_ptr->spell_book == TV_PSI_BOOK)
	{
		q = "Examine which focus? ";
		s = "You have no foci to examine. ";
	}
	else
	{
		q = "Browse which book? ";
		s = "You have no books that you can read.";
	}

	/* Get an item */	
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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

	/* Get the item's sval */
	sval = o_ptr->sval;

	/* Psionic Foci */
	if (mp_ptr->spell_book == TV_PSI_BOOK) sval /= 3;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
	int i, item, sval;

	int spell = -1;

	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" :
	((mp_ptr->spell_book == TV_PSI_BOOK) ? "power" : "prayer"));

	cptr q, s;

	object_type *o_ptr;


	if (!mp_ptr->spell_book)
	{
		msg_print("You cannot read books!");
		return;
	}

	if ((p_ptr->blind || no_lite()) && (mp_ptr->spell_book != TV_PSI_BOOK))
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
		msg_format("You cannot learn any new %ss!", p);
		return;
	}


	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	if (mp_ptr->spell_book == TV_PSI_BOOK)
	{
		q = "Examine which focus? ";
		s = "You have no foci to examine. ";
	}
	else
	{
		q = "Study which book? ";
		s = "You have no books that you can read.";
	}
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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

	/* Get the item's sval */
	sval = o_ptr->sval;

        /* Psionicists books exist in different levels */
        if (mp_ptr->spell_book == TV_PSI_BOOK) sval /= 3;
	
	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Mage -- Learn a selected spell */
	if ((mp_ptr->spell_book == TV_MAGIC_BOOK) || (mp_ptr->spell_book == TV_PSI_BOOK))
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE) && (spell == -1)) return;
	}

	/* Priest -- Learn a random prayer */
	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < 64; spell++)
		{
			/* Check spells in the book */
			if ((spell < 32) ?
			    (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
			    (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msg_format("You cannot learn any %ss in that %s.", p,
		mp_ptr->spell_book == TV_PSI_BOOK ? "focus" : "book");

		/* Abort */
		return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Learn the spell */
	if (spell < 32)
	{
		p_ptr->spell_learned1 |= (1L << spell);
	}
	else
	{
		p_ptr->spell_learned2 |= (1L << (spell - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 64; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i] = spell;

	/* Mention the result */
	message_format(MSG_STUDY, 0, "You have learned the %s of %s.",
	           p, spell_names[mp_ptr->spell_type][spell]);

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

	/* Redraw object recall */
	p_ptr->window |= (PW_OBJECT);
}



/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir;
	int chance, beam;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if (mp_ptr->spell_book != TV_MAGIC_BOOK)
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
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";
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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Ask for a spell */
	if (!get_spell(&spell, "cast", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];


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

	/* Process spell */
	else
	{
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = ((p_ptr->pclass == CLASS_MAGE) ? plev : (plev / 2));

		/* Spells.  */
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
				set_recall();
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
				int time = randint(20) + 20;
				(void)set_oppose_acid(p_ptr->oppose_acid + time);
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				(void)set_oppose_fire(p_ptr->oppose_fire + time);
				(void)set_oppose_cold(p_ptr->oppose_cold + time);
				(void)set_oppose_pois(p_ptr->oppose_pois + time);
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
		}

		/* A spell was cast */
		if (!((spell < 32) ?
		      (p_ptr->spell_worked1 & (1L << spell)) :
		      (p_ptr->spell_worked2 & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1 |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2 |= (1L << (spell - 32));
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
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
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
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
		cptr act;

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
 * Pray a prayer
 */
void do_cmd_pray(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir, chance;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q, s;


	/* Must use prayer books */
	if (mp_ptr->spell_book != TV_PRAYER_BOOK)
	{
		msg_print("Pray hard enough and your prayers may be answered.");
		return;
	}

	/* Must have lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Must not be confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Restrict choices */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no prayer books!";
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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Choose a spell */
	if (!get_spell(&spell, "recite", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any prayers in that book.");
		return;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];


	/* Verify "dangerous" prayers */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to recite this prayer.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Check for failure */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to concentrate hard enough!");
	}

	/* Success */
	else
	{
		switch (spell)
		{
			case 0:
			{
				(void)detect_monsters_evil();
				break;
			}

			case 1:
			{
				(void)hp_player(damroll(2, 10));
				(void)set_cut(p_ptr->cut - 10);
				break;
			}

			case 2:
			{
				(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				break;
			}

			case 3:
			{
				(void)set_afraid(0);
				break;
			}

			case 4:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 5:
			{
				(void)detect_traps();
				break;
			}

			case 6:
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 7:
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 8:
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				break;
			}

			case 9:
			{
				teleport_player(plev * 3);
				break;
			}

			case 10:
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case 11:
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				break;
			}

			case 12:
			{
				(void)sleep_monsters_touch();
				break;
			}

			case 13:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 14:
			{
				remove_curse();
				break;
			}

			case 15:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

			case 16:
			{
				(void)set_poisoned(0);
				break;
			}

			case 17:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          (damroll(3, 6) + plev +
				           (plev / ((p_ptr->pclass == CLASS_PRIEST) ? 2 : 4))),
				          ((plev < 30) ? 2 : 3));
				break;
			}

			case 18:
			{
				(void)hp_player(damroll(6, 10));
				(void)set_cut(0);
				break;
			}

			case 19:
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				break;
			}

			case 20:
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			}

			case 21:
			{
				earthquake(py, px, 10);
				break;
			}

			case 22:
			{
				map_area();
				break;
			}

			case 23:
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 24:
			{
				(void)turn_undead();
				break;
			}

			case 25:
			{
				(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				break;
			}

			case 26:
			{
				(void)dispel_undead(randint(plev * 3));
				break;
			}

			case 27:
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 28:
			{
				(void)dispel_evil(randint(plev * 3));
				break;
			}

			case 29:
			{
				warding_glyph();
				break;
			}

			case 30:
			{
				(void)dispel_evil(randint(plev * 4));
				(void)hp_player(1000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 31:
			{
				(void)detect_monsters_normal();
				break;
			}

			case 32:
			{
				(void)detect_all();
				break;
			}

			case 33:
			{
				(void)ident_spell();
				break;
			}

			case 34:
			{
				(void)probing();
				break;
			}

			case 35:
			{
				wiz_lite();
				break;
			}

			case 36:
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut(0);
				break;
			}

			case 37:
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 38:
			{
				(void)hp_player(2000);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 39:
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 40:
			{
				(void)restore_level();
				break;
			}

			case 41:
			{
				(void)dispel_undead(randint(plev * 4));
				break;
			}

			case 42:
			{
				(void)dispel_evil(randint(plev * 4));
				break;
			}

			case 43:
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			}

			case 44:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 45:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 200);
				break;
			}

			case 46:
			{
				(void)destroy_doors_touch();
				break;
			}

			case 47:
			{
				(void)recharge(15);
				break;
			}

			case 48:
			{
				(void)remove_all_curse();
				break;
			}

			case 49:
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

			case 50:
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

			case 51:
			{
				brand_weapon();
				break;
			}

			case 52:
			{
				teleport_player(10);
				break;
			}

			case 53:
			{
				teleport_player(plev * 8);
				break;
			}

			case 54:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 55:
			{
				(void)teleport_player_level();
				break;
			}

			case 56:
			{
				set_recall();
				break;
			}

			case 57:
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}
		}

		/* A prayer was prayed */
		if (!((spell < 32) ?
		      (p_ptr->spell_worked1 & (1L << spell)) :
		      (p_ptr->spell_worked2 & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1 |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2 |= (1L << (spell - 32));
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
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
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * The cost of psionic spells can be amplified...
 */
int get_psi_cost(int spell)
{

	int cost = mp_ptr->info[spell].smana;
	
	switch (spell)
	{
		/* The non-intensifiable spells */
		case 15: /* teleport other */
		case 16: /* recall */
		case 24: /* satisfy hunger */
		case 31: /* complete healing */
		case 33: /* wall of force */
		case 44: /* cannibalize */
		case 45: /* intensify */
		case 46: /* receptacle */
		case 49: /* psychic surgery */
		case 50: /* splice */
		case 54: /* detect monsters */
		case 56: /* reveal secrets */
		case 57: /* clairvoyance */
		case 58: /* m. analysis */
		case 59: /* read object */
		case 60: /* read aura */
			return cost;
    		default:
    			return (cost * (2 + p_ptr->intensify)) / 2;
	}
}

int amplify_dam(int dam)
{
	return (dam * (3 + p_ptr->intensify)) / 3;
}

int inverse_amplify(int dam)
{	
	return (dam * 3) / (3 + p_ptr->intensify);
}

static bool item_tester_hook_drain(object_type *o_ptr)
{
	int t = o_ptr->tval;
	if ((t != TV_STAFF) && (t != TV_WAND)) return (FALSE);
	if (o_ptr->ident & IDENT_EMPTY) return (FALSE);
	return (TRUE);
}

#define req_focus(lvl) if (f_lev<lvl) {msg_print(\
"Your focus isn't strong enough to use that power."); return;}

int splice_val = 0,splice_n = 0;

#define A(x) amplify_dam(x)


/*
 * Use a psionic power
 */

void do_cmd_psi(void)
{
	int item, sval, spell, dir, f_lev;
	int chance, cost = 0;
	int plev = p_ptr->lev;

	object_type *o_ptr;

	magic_type *s_ptr;
	char temp[20];
	int i,j;

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}
	
	/* Note: psionics *are* possible when blind */

	/* Metapsionics allows use of any focus */
	if (p_ptr->meta_psi_lev)
	{
		/* Restrict choices to spell books */
		item_tester_tval = mp_ptr->spell_book;

		/* Get an item */
		if (!get_item(&item,"Use which discipline? ","You have no psionic foci!",
		USE_EQUIP | USE_INVEN | USE_FLOOR)) return;

		/* Get the item (in the pack/inven) */
		if (item >= 0) o_ptr = &inventory[item];

		/* Get the item (on the floor) */
		else o_ptr = &o_list[0 - item];

		/* Access the item's sval - extract power and type*/
		sval = o_ptr->sval;
		f_lev = sval % 3;
		sval /= 3;

		/* If we are using metapsionics limit focus power */
		if (o_ptr != &inventory[INVEN_NECK])
			f_lev = MIN(p_ptr->meta_psi_lev - 1,f_lev);
		
	}
	else
	{
		o_ptr = &inventory[INVEN_NECK];

		item = FALSE;
		if (o_ptr) item = (o_ptr->tval == TV_PSI_BOOK);
		if (!item)
		{
			msg_print("You aren't wearing a psionic focus!");
			return;
		}

		sval = o_ptr->sval;
		f_lev = sval % 3;
		sval /= 3;
	}

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Ask for a spell */
	if (!get_spell(&spell, "use", sval , TRUE))
	{
		if (spell == -2) msg_print("You don't know any powers in that discipline.");
		return;
	}


	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];
	cost = get_psi_cost(spell);

	/* Verify "dangerous" spells */
	if (cost > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use this power.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to concentrate hard enough!");

		if (splice_val)
		{
			splice_val -= MAX(damroll(2,8),splice_val / 2);
			if (splice_val <= 0) splice_val = 0;
		}
	}

	/* Process spell */
	else
	{

		/* Spells.  */
		switch (spell)
		{
			/* *** TELEPATHY *** */

			case 0: /* Mind Thrust */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PSI, dir, A(damroll(1 + (plev / 4), 4)),0);
				break;
			}

			case 1: /* Empathy */
			{
				if (plev < 25)
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_EMPATHY, dir, A(plev), MAX(2, plev / 5));
				}
				else
					project_hack(GF_EMPATHY,plev);
				break;
			}

			case 2: /* Mental Barrier */
			{
				(void)set_mental_barrier(p_ptr->mental_barrier + 10 + randint(A(plev / 2)));
				break;
			}

			case 3: /* Intimidate */
			{
				if (!get_aim_dir(&dir)) return;
				fear_monster(dir,A(plev * 3));
				break;
			}

			case 4: /* Sleep */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_OLD_SLEEP,dir,A(plev * 3),0);
				break;
			}

			case 5: /* Psionic Blast */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PSI, dir, A(plev * 5),2);
				break;
			}
			
			case 6: /* Psychic Crush */
			{
				req_focus(1);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PSI2, dir,A(damroll(plev , 9)),0);
				break;
			}

			case 7: /* Mind Wrack */
			{
				req_focus(1);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PSI3,dir,(damroll(3 , A(plev))),0);
				break;
			}

			case 8: /* Amnesia */
			{
				req_focus(2);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_AMNESIA,dir,A(plev + (rand_int(2) ? randint(plev * 3) : 0)),0);
				break;
			}
			
			case 9: /* Psychic Wave*/
			{
				req_focus(2);
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_PSI2,dir,damroll(6,A(plev)));
				break;
			}
	
			/* *** PSYCHOPORTATION *** */
	
			case 11: /* Blink */
			{
				teleport_player(A(10));
				break;
			}

			case 12: /* Teleport */
			{
				teleport_player(A(plev * 5));
				break;
			}

			case 13: /* Dimension Door */
			{
				msg_print("You open a dimensional door.");			
				if (dimension_door(A(plev)))
				{
					/* Require some time XXX */
					p_ptr->energy -= inverse_amplify(60 - plev);
				}
				break;
			}

			case 14: /* Probability Travel */
			{
				req_focus(1);
				set_prob_travel(p_ptr->prob_travel + (randint(A(5)) + A(plev / 10)));
				break;
			}

			case 15: /* Teleport Other */
			{
				req_focus(1);
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 16: /* Recall */
			{
				req_focus(2);
				set_recall();
				break;
			}

			case 17: /* Time Control */
			{
				req_focus(1);
				if (p_ptr->ts_anchor)
				{
					msg_print("You end your time/space anchor.");
					p_ptr->ts_anchor = 0;
				}

				(void)set_fast(p_ptr->fast + A(plev / 4 + randint(20)));
				
				if (p_ptr->fast)
				{
					set_fast(p_ptr->fast + randint(A(plev/4)));
				}
				else
				{
					set_fast(p_ptr->fast + 20 + randint(A(plev/4)));
				}
				break;
			}
			
			case 18: /* Time Bolt */
			{
				req_focus(1);
				if (p_ptr->ts_anchor)
				{
					if (!rand_int(3))
					{
						msg_print("The time/space anchor stops your time bolt!");
						break;
					}
					else if (!rand_int(3))
					{
						msg_print("Your time/space anchor collapses!");
						p_ptr->ts_anchor = 0;
					}
				}
				
				if (!get_aim_dir(&dir)) return;
				life_drained = 0;
				fire_bolt_or_beam(plev,GF_TIME,dir,(damroll(10,A(plev))));
				if (life_drained)
				{
					msg_print("You have stolen precious seconds of time!");
					p_ptr->energy += MIN(life_drained,damroll(2,plev));
				}
				break;
			}

			case 19: /* Time/Space Anchor */
			{
				(void)set_ts_anchor(10 + A(randint(plev/4)));
				break;
			}

			case 20: /* Time Shift */
			{
				req_focus(2);
				if (p_ptr->ts_anchor)
				{
					msg_print("You end your time/space anchor.");
					p_ptr->ts_anchor = 0;
				}
				msg_print("Time freezes around you!");
				p_ptr->energy += damroll(5,A(plev));
				break;
			}

			/* *** PSYCHOMETABOLISM *** */
			
			case 22: /* Cell Adjustment */
			{
				(void)hp_player((damroll(A(plev / 2), 6)));
				(void)set_cut(p_ptr->cut - A(15));
				break;
			}
			
			case 23: /* Light Control */
			{
				project(-1,(plev / 10) + 1,p_ptr->py,p_ptr->px,(damroll(3, A(plev))),
				GF_LITE_WEAK, PROJECT_GRID | PROJECT_KILL);
				lite_room(p_ptr->py,p_ptr->px);
				break;
			}

			case 24: /* Satisfy Hunger */
			{
				msg_print("You start converting your body's energy into food.");
				p_ptr->energy -= inverse_amplify(300 - 5 * plev);
				(void) set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 25: /* Adrenaline Control */
			{
				(void)set_adrenaline(p_ptr->adrenaline+ randint(20) + A(plev/2));
				break;
			}
		
			case 26: /* Biofeedback */
			{
				(void)set_biofeedback(p_ptr->biofeedback + randint(A(10 + plev)));
				break;
			}
#if 0		
			case 666: /* Shadowform */
			{
				req_focus(2);
				msg_print("You leave the physical world and become a living shadow!");
				p_ptr->shadow_form += randint(randint(A(plev + 12)));
				if (p_ptr->shadow_form > 30 + randint(plev * 2))
				{
					msg_print("You begin to fade into the netherworld!");
					i = randint(p_ptr->shadow_form);
					_killer = "shadows";
					project(-2,1,p_ptr->py,p_ptr->px,damroll(i,6),GF_NETHER,PROJECT_KILL);
					p_ptr->shadow_form -= i;
					if (p_ptr->shadow_form < 0) p_ptr->shadow_form = 0;
				}
				break;
			}
#endif
						
			case 27: /* Drain Life */
			{
				req_focus(1);
				if (!get_aim_dir(&dir)) return;
				life_drained = 0;
				fade_dam_on = TRUE;
				fire_bolt_or_beam(plev,GF_OLD_DRAIN,dir,A(damroll(plev,10)));
				fade_dam_on = FALSE;
				if (life_drained) hp_player(life_drained / 2);
				break;
			}
			
			case 28: /* Energy Contaiment */
			{
				int n = A(10 + randint(20));
				(void)set_oppose_acid(p_ptr->oppose_acid + n);
				(void)set_oppose_elec(p_ptr->oppose_elec + n);
				(void)set_oppose_fire(p_ptr->oppose_fire + n);
				(void)set_oppose_cold(p_ptr->oppose_cold + n);
				(void)set_oppose_pois(p_ptr->oppose_pois + n);
				break;
			}

			case 29: /* Death Field */
			{
				req_focus(1);
				sprintf(temp,"Intensity (1-%d): ",plev);
				i = get_quantity(temp,plev);
				i = damroll(6,i);
				msg_print("Waves of darkness begin emanating from your body.");
				take_hit(A(damroll(2,i)),"idiocy");
				project(-1,3,p_ptr->py, p_ptr->px, A(damroll(15,i)),GF_NETHER,PROJECT_KILL);
				break;
			}
			
			case 30: /* Double Pain */
			{
				req_focus(2);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PAIN,dir,randint(A(plev * 3 + 15)),0);
				break;
			}

			case 31: /* Complete Healing */
			{
				req_focus(2);
				msg_print("You enter a trance and your body begins to rapidly recover.");
				(void)hp_player(5000);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				p_ptr->energy -= 350 - plev * 2; /* intensify doesn't help */
				break;
			}

			
			/* *** PSYCHOKINESIS *** */

			
			case 33: /* Wall of Force */
			{
				if (no_fw == MAX_FORCE_WALLS)
				{
					msg_format("You already have %d force walls - that's the limit.", MAX_FORCE_WALLS);
					return;
				}
				msg_print("You create a barrier of pure force.");
				if (!tgt_pt(&i,&j)) return;
				if (!cave_empty_bold(j,i))
				{
					msg_print("There's something in the way!");
					break;
				}
				fw_x[no_fw] = i;
				fw_y[no_fw] = j;
				no_fw++;
				cave_set_feat(j, i, FEAT_FORCE_WALL);
				p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
				break;
			}

			case 34: /* Inertial Barrier */
			{
				(void)set_inertial_barrier(p_ptr->inertial_barrier + A(10 + randint(plev / 5)));
				break;
			}

			case 35: /* Project Force */
			{
				if (!get_aim_dir(&dir)) return;
				if (plev > 30) plev = 30 + (plev - 30) / 4;
				fade_dam_on = TRUE;
				fire_beam(GF_FORCE,dir,A(damroll(3,plev)));
				fade_dam_on = FALSE;
				break;
			}

			case 36: /* Sonic Boom */
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("You hear a roar of sound.");
				if (plev > 30) plev = 30 + (plev - 30) / 4;
				fire_ball(GF_SOUND, dir,A(30 + 2*plev),5);
				break;
			}

			case 37: /* Disintegrate */
			{
				req_focus(1);
				if (!get_aim_dir(&dir)) return;
				if (plev > 30) plev = 30 + (plev - 30) / 2;
				fire_ball(GF_DISINTEGRATE,dir,A(damroll(plev,9)),2);
				break;
			}
			
			case 38: /* Telekinesis */
			{
				req_focus(1);
				(void)telekinesis(A(plev));
				break;
			}

			case 39: /* Sphere of Cold */
			{
				req_focus(1);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD,dir,A(plev * 4),1);
				break;
			}

			case 40: /* Fire Eruption */
			{
				req_focus(1);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE,dir,A(plev * 7),0);
				break;
			}

			case 41: /* Detonate */
			{
				int radius = 0;
				req_focus(2);
				if (!get_aim_dir(&dir)) return;
				if (!rand_int(plev))
				{
					msg_print("Your imperfect concentration causes the power to backfire!");
					_killer = "detonate backfire";
					project(-2,2,p_ptr->py,p_ptr->px,A(damroll(15,MIN(plev,40))),GF_METEOR,
					PROJECT_GRID | PROJECT_KILL);
					do {radius += randint(6);} while (!rand_int(3));
					radius = (radius < 5) ? 10 : radius * 2;
				}
				else
				{
					fire_ball(GF_METEOR,dir,A(damroll(15,MIN(plev,40))),2);
					if (!rand_int(8))
					{
						msg_print("The dungeon collapses around the blast");
						do {radius += randint(6);} while (!rand_int(3));
					}
				}
				if (radius) earthquake(blast_center_y,blast_center_x,radius);
				break;
			}

			case 42: /* Balefire */
			{
				req_focus(2);
				if (!get_aim_dir(&dir)) return;
				balefire_done = FALSE;
				fire_beam(GF_BALEFIRE,dir,A(damroll(5,p_ptr->chp)));
				break;
			}

			
			/* *** METAPSIONICS *** */

			case 44: /* Cannibalize */
			{
				int m = 0,h;
				
				do
				{
					m = get_quantity("How much mana?",999);
					if (p_ptr->precognition)
						msg_print(format("That would probably leave you with %d hp.",
						p_ptr->chp - (m * 5) / 2));
					if (!m) return;
				} while (!get_check(format("Confirm %d mana?",m)));
				m += rand_int(6) - 2;
				if (m<0) return;
				p_ptr->csp += m;
				for (h=0;m;m--) h += rand_int(6);
				msg_print("You begin converting your life force into psionic power.");
				take_hit(h,"idiocy");
				break;
			}

			case 45: /* Intensify */
			{
				p_ptr->intensify = get_quantity(format("Intensify level? (max %d):",
				  (plev + 7) / 8),(plev + 7) / 8);
				break;
			}

			case 46: /* Receptacle */
			{	
				item_tester_hook = item_tester_hook_drain;
				if (!get_item(&item, "Drain what item?", "Nothing charged to drain",
				USE_EQUIP | USE_FLOOR | USE_INVEN)) break;
				if (item >= 0) o_ptr = &inventory[item];
				else o_ptr = &o_list[0 - item];

				msg_print("Energy drains from your pack!");

				i = 0;
				
				/* here check for empty staffs/wands and IDENT_EMPTY */
				
				while ((p_ptr->csp < p_ptr->msp) && (o_ptr->pval > 0) &&
				  (i < 6 + randint(60)))
				{
					p_ptr->csp += damroll(o_ptr->number,plev);
					j = randint((plev > 40) ? randint(6) : 6);
					o_ptr->pval -= j;
					i += j;
				}
				if ((j = o_ptr->pval) < 0) o_ptr->pval = 0;
				else if ((i = i * i - 40) > 0)
				if (rand_int(2)) /* Straight up 50% save */
				if (damroll(3,20) < i + damroll(j,6))
				{
					msg_print("There is a bright flash of light.");
					/* Reduce and describe inventory */
					if (item >= 0)
					{
						inven_item_increase(item, -999);
						inven_item_describe(item);
						inven_item_optimize(item);
					}
            	
					/* Reduce and describe floor item */
					else
					{
						floor_item_increase(0 - item, -999);
						floor_item_describe(0 - item);
						floor_item_optimize(0 - item);
					}
					i = damroll(j,(j>20 ? 16 : j));
					_killer = "a burst of magical energy";
					project(-2,i > 50 ? 3 : 2,p_ptr->py,p_ptr->px,i,GF_MANA,
					PROJECT_GRID | PROJECT_KILL | PROJECT_ITEM);
				}

				/* Combine / Reorder the pack */
				p_ptr->notice |= (PN_COMBINE | PN_REORDER);

				/* Window stuff */
				p_ptr->window |= (PW_INVEN);

				/* Done */
				break;
			}
			
			case 47: /* Empower */
			{
				recharge(rand_int(3)?100:100*randint(randint(A(plev))));
				break;
			}

			case 48: /* Psychic Drain */
			{
				req_focus(2);
				if (!get_aim_dir(&dir)) return;
				life_drained = 0;
				fade_dam_on = TRUE;
				fire_ball(GF_PSI_DRAIN,dir,A(damroll(plev/2,6)),1 + !rand_int(6) - !rand_int(5));
				fade_dam_on = FALSE;
				p_ptr->csp += damroll(5,life_drained) >> 2;
				p_ptr->energy -= randint(150);
				break;
			}

			case 49: /* Psychic Surgery */
			{
				restore_level();
				set_afraid(0);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_INT);
				break;
			}

			case 50: /* Splice */
			{
				req_focus(1);
				if (splice_val)
				{
					splice_val = 0;
					return;
				}
				splice_val = get_quantity(format(
				"Type in amount of mana to spend to begin splice (max %d).",plev),plev);
				splice_n = 0;
				p_ptr->csp -= splice_val;
				while (splice_val) do_cmd_psi();
				msg_print("Your splice unravels.");
				break;
			}

			case 51: /* Ultrablast */
			{
				req_focus(1);
				msg_print("You project waves of psychic energy with all your strength!");
				project_hack(GF_PSI,A(damroll(40,plev)));
				if (!rand_int(plev))
				{
					int temp = 0;
					
					msg_print("You lose control over the attack and your mana drains away.");
					do {temp += cost;} while (!rand_int(3));
					cost = temp;
					if (cost > p_ptr->csp) cost = p_ptr->csp;
				}
				break;
			}
  			
			case 52: /* *Ultrablast* */
			{
				req_focus(2);
				msg_print("You project deadly psychic energy with all your strength!");	
				project_hack(GF_PSI2,A(damroll(100,plev)));
				if (!rand_int(plev - 25))
				{
					int temp = 0;
					
					msg_print("You lose control over the attack and your mana drains away.");
					do {temp += cost;} while (!rand_int(5));
					cost = temp;
					if (cost > p_ptr->csp) cost = p_ptr->csp;
				}
				break;
			}

			/* *** CLAIRSENTIENCE *** */

			case 54: /* Detect Monsters */
			{
				(void)detect_monsters_normal();
				break;
			}

			case 55: /* Awareness */
			{
				(void)set_awareness(p_ptr->awareness + A(plev / 2 + randint(20)));
				break;
			}

			case 56: /* Reveal Secrets */
			{
				detect_doors();
				detect_traps();
				detect_stairs();
				break;
			}

			case 57: /* Clairvoyance */
			{
				msg_print("An image of your surroundings forms in your mind...");
				map_area();
				break;
			}

			case 58: /* Metaphysical analysis */
			{
				show_durations();
				break;
			}

			case 59: /* Read Object */
			{
				req_focus(1);
				(void)ident_spell();
				break;
			}

			case 60: /* Read Aura */
			{
				req_focus(2);
				(void)identify_fully();
				break;
			}

			case 61: /* Precognition */
			{
				req_focus(2);
				(void)set_precognition(p_ptr->precognition + A(5 + randint(plev / 5)));
				break;
			}

			case 62: /* Analyze Monster */
			{
				req_focus(2);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ANALYZE,dir,A(2 * plev),0);
				break;
			}

			default:
			{
				msg_format("%s not implemented",
				spell_names[mp_ptr->spell_type][spell]);
				break;
			}
		}

		/* A spell was cast */
		if (!((spell < 32) ?
		(p_ptr->spell_worked1 & (1L << spell)) :
		(p_ptr->spell_worked2 & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1 |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2 |= (1L << (spell - 32));
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
		if (splice_val)
		{
			p_ptr->command_dir = 0; /* change targets */
			splice_val -= cost / 3 + ++splice_n;
			if (splice_val < 0) splice_val = 0; else
			msg_format("%d splice points remaining.",splice_val);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (cost <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= cost;
	}

	/* Over-exert the player */
	else
	{
		int oops = cost - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your psyche!");

			(void)dec_stat(A_INT, 15 + randint(10), perm);
			(void)dec_stat(A_WIS, 15 + randint(10), perm);
		}
	}

	if (p_ptr->csp >= p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

#undef A


int get_balefire_effect()
{
	int result = 0;

	if (balefire_done) return 0;
	balefire_done = TRUE;
	if (rand_int(10)) return 0;
	msg_print("The balefire tears a hole in reality!");
	if (p_ptr->ts_anchor)
	{
		msg_print("Your time/space anchor is ripped apart!");
		p_ptr->ts_anchor = 0;
		if (!rand_int(6)) return BALE_ABORT;
	}
	if (!rand_int(8)) return BALE_ABORT;
	while (!result)
	{
		if (!rand_int(12)) result |= BALE_SUMMON;
		if (!rand_int(8)) result |= rand_int(3) ? (rand_int(2) ? BALE_KILL : BALE_CLONE) : BALE_HEAL;
		if (!rand_int(8)) result |= BALE_TIME_ATTACK;
		if (!rand_int(20)) result |= BALE_HISTORY;
		if (!rand_int(12)) result |= BALE_ALTER;
		if (!rand_int(40)) result |= BALE_SUMMON | BALE_CLONE | BALE_TIME_ATTACK;
	}
	return result;
}

void do_balefire_effects(int effect)
{
	if (effect & BALE_HISTORY)
	{
		seed_town = rand_int(0x10000000);
		/* other effects ? never born ? TOWN_WEIRD */
		msg_print("You have changed the entire course of human history!");
		msg_print(NULL);
		if (p_ptr->depth == 0)
		{
			p_ptr->leaving = TRUE;
			effect &= ~BALE_ALTER;
		}
	}
	if (effect & BALE_ALTER)
	{
		msg_print("The world changes!");
		msg_print(NULL);
		p_ptr->leaving = TRUE;
	}

	if (effect & BALE_TIME_ATTACK)
	{
		msg_print("Temporal rifts erupt all around you!");
		msg_print(NULL);
		_killer = " a rupture in space/time.";
		project(-2,rand_int(5),p_ptr->py,p_ptr->px,rand_int(rand_int(400)),
		GF_TIME,PROJECT_KILL);
	}

	if (effect & BALE_SUMMON)
	{
		int count = 0, n = 0;
		msg_print("Time vortices spin into existence all around you!");
		msg_print(NULL);
		while ((count < 10) && (n < 1000))
		{
			count += summon_specific(p_ptr->py,p_ptr->px,100,SUMMON_TIME);
			n++;
		}
	}

	if (effect & (BALE_CLONE | BALE_KILL | BALE_HEAL))
	; /* these effects aren't handled here */
}

/* this may need a "#ifdef DJGPP" */
#define print_dur(dur) \
do {if (p_ptr->dur) msg_format(#dur " %d",p_ptr->dur);} while (0)

#define print_dur2(dur) \
do {if (p_ptr->dur) msg_format(#dur " %d",p_ptr->dur);} while (0)

void show_durations()
{
  print_dur(fast);
  print_dur(slow);
  print_dur(blind);
  print_dur(paralyzed);
  print_dur(confused);
  print_dur(afraid);
  print_dur(image);
  print_dur(poisoned);
  print_dur(stun);
  print_dur(cut);
  print_dur(protevil);
  print_dur(invuln);
  print_dur(hero);
  print_dur(shero);
  print_dur(shield);
  print_dur(blessed);
  print_dur(tim_invis);
  print_dur(tim_infra);
  print_dur(oppose_acid);
  print_dur(oppose_elec);
  print_dur(oppose_fire);
  print_dur(oppose_cold);
  print_dur(oppose_pois);
  print_dur2(awareness);
  print_dur2(shadow_form);
  print_dur2(biofeedback);
  print_dur2(adrenaline);
  print_dur2(inertial_barrier);
  print_dur2(prob_travel);
  print_dur2(precognition);
  print_dur2(mental_barrier);
  print_dur2(ts_anchor);
  print_dur2(intensify);
  print_dur(word_recall);
}
