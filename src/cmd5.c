/* File: cmd5.c */

/* Selection, browsing, learning, and casting of spells and prayers.  
 * Includes definitions of all spells and prayers.  Weapon branding,
 * shapeshifting, and making Athelas.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/* Shapechanging code taken from Sangband. */
void shapechange(s16b shape)
{
	char *shapedesc = "";

	/* Wonder Twin powers -- Activate! */
	p_ptr->schange = shape;
	p_ptr->update |= PU_BONUS;

	switch (shape)
	{
		case SHAPE_MOUSE:
			shapedesc = "mouse";
			break;
		case SHAPE_FERRET:
			shapedesc = "ferret";
			break;
		case SHAPE_HOUND:
			shapedesc = "hound";
			break;
		case SHAPE_GAZELLE:
			shapedesc = "gazelle";
			break;
		case SHAPE_LION:
			shapedesc = "lion";
			break;
		case SHAPE_ENT:
			shapedesc = "Ent";
			break;
		case SHAPE_BAT:
			shapedesc = "bat";
			break;
		case SHAPE_WEREWOLF:
			shapedesc = "werewolf";
			break;
		case SHAPE_VAMPIRE:
			shapedesc = "vampire";
			break;
		default:
			msg_print("You return to your normal form.");
			break;
	}

	if (shape)
	{
		msg_format("You assume the form of a %s.", shapedesc);
		msg_print("Your equipment merges into your body.");
	}

	/* Recalculate mana. -LM- */
	p_ptr->update |= (PU_MANA);

	/* Show or hide shapechange on main window. -LM- */
	p_ptr->redraw |= (PR_SHAPE);
}

/* The Athelas-creation code. -LM- */
void create_athelas(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Make some Athelas, identify it, and drop it near the player. */
	object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_ATHELAS));
	object_aware(i_ptr);
	object_known(i_ptr);
	drop_near(i_ptr, -1, py, px);
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
static int get_spell(int *sn, cptr prompt, int sval, bool known)
{
	int i;

	int spell = -1;
	int num = 0;

	byte spells[64];

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

	cptr p = ((mp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");

#ifdef ALLOW_REPEAT /* TNB */

    /* Get the spell, if available */
    if (repeat_pull(sn)) {

        /* Verify the spell */
        if (spell_okay(*sn, known)) {

            /* Success */
            return (TRUE);
        }
    }

#endif

	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr->spell_book - TV_MAGIC_BOOK][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr->spell_book - TV_MAGIC_BOOK][sval][1] & (1L << (spell - 32))))
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
		ver = (isupper(choice));

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
		if (ver)
		{
			char tmp_val[160];

			/* Access the spell */
			s_ptr = &mp_ptr->info[spell];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, spell_names[mp_ptr->spell_book - TV_MAGIC_BOOK][spell],
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

#ifdef ALLOW_REPEAT /* TNB */
	repeat_push(*sn);
#endif

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

	int spell = -1;
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
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Browse which book? ";
	s = "You have no books that you can read.";
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


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr->spell_book - TV_MAGIC_BOOK][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr->spell_book - TV_MAGIC_BOOK][sval][1] & (1L << (spell - 32))))
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

	cptr p = ((mp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");

	cptr q, s;

	object_type *o_ptr;

	if (!mp_ptr->spell_book)
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
		msg_format("You cannot learn any new %ss!", p);
		return;
	}


	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Study which book? ";
	s = "You have no books that you can read.";
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


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* All but Priests -- Learn a selected spell */
	if (mp_ptr->spell_book != TV_PRAYER_BOOK)
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
			    (spell_flags[mp_ptr->spell_book - TV_MAGIC_BOOK][sval][0] & (1L << spell)) :
			    (spell_flags[mp_ptr->spell_book - TV_MAGIC_BOOK][sval][1] & (1L << (spell - 32))))
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
		msg_format("You cannot learn any %ss in that book.", p);

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
	p_ptr->spell_order[i++] = spell;

	/* Mention the result */
	msg_format("You have learned the %s of %s.",
	           p, spell_names[mp_ptr->spell_book - TV_MAGIC_BOOK][spell]);

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
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir;
	int chance, beam;
	int shape = 0;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability.  Must not be a Priest. */
	if ((mp_ptr->spell_book == TV_PRAYER_BOOK) || (mp_ptr->spell_book == 0))
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

	/* Access the item's sval */
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


	/* Access the spell */
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
		/* Hack -- higher chance of "beam" instead of "bolt" for mages and necros. */
		beam = (((p_ptr->pclass == MAGE) || (p_ptr->pclass == NECRO)) ? plev : (plev / 2));

		/* Spells.  */
		switch (spell + (mp_ptr->spell_book - TV_MAGIC_BOOK) * 64)
		{
			case 0:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir,
				                  damroll(2, 4 + plev / 10));
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
				(void)lite_area(damroll(2, 1 + (plev / 5)), (plev / 10) + 1);
				break;
			}

			case 4:
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 5:
			{
				(void)hp_player(damroll(2, plev / 4 + 5));
				(void)set_cut(p_ptr->cut - 15);
				break;
			}

			case 6:
			{
				(void)detect_traps();
				if (p_ptr->pclass == CLASS_ROGUE)
				{
					(void)detect_treasure();
					(void)detect_objects_gold();
				}
				break;
			}

			case 7:
			{
				(void)detect_doors();
				(void)detect_stairs();
				if (p_ptr->pclass == CLASS_ROGUE)
				{
					(void)detect_objects_normal();
				}
				break;
			}

			case 8:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          5 + plev / 3, 2);
				break;
			}

			case 9:
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev + 10);
				break;
			}

			case 10:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ELEC, dir,
				                  damroll(2+((plev-5)/5), 8));
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
				(void)sleep_monster(dir, plev + 10);
				break;
			}

			case 13:
			{
				(void)set_poisoned(0);
				break;
			}

			case 14:
			{
				teleport_player(50 + plev * 2);
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
				(void)recharge(85);
				break;
			}

			case 17:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_COLD, dir,
				                  damroll(4+((plev-5)/5), 8));
				break;

			}

			case 18:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 19:
			{
				if (!get_aim_dir(&dir)) return;
				(void)disarm_trap(dir);
				break;
			}

			case 20:
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}

			case 21:
			{
				(void)ident_spell();
				break;
			}

			case 22:
			{
				(void)sleep_monsters(plev + 10);
				break;
			}

			case 23:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(7+((plev-5)/5), 8));
				break;
			}

			case 24:
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir, plev + 10);
				break;
			}

			case 25:
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 26:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          30 + plev, 2);
				break;
			}

			case 27:
			{
				(void)recharge(150);
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
				          55 + plev, 2);
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

			case 37: /* Dimension Door.  A Zangband idea. */
			{
				int ny = 0;
				int nx = 0; /* target starts at player. */

				/* Use the safe, clean targeting function. This method,
				 * although very safe, provides a rather clunky interface.
				 * I may come up with a specialized function at some point.
				 */
				if (!target_set_interactive(TARGET_LOOK)) return;

				/* grab the target coords. */
				ny = p_ptr->target_row;
				nx = p_ptr->target_col;

				/* Test for empty floor, forbid vaults or too large a
				 * distance, and insure that this spell is never certain.
				 */
				if (!cave_empty_bold(ny,nx) || (cave_info[ny][nx] & CAVE_ICKY) ||
					(distance(ny,nx,py,px) > 25) || (rand_int(plev) == 0))
				{
					msg_print("You fail to exit the astral plane correctly!");
					p_ptr->energy -= 50;
					teleport_player(15);
             		}

				/* The simple act of controlled teleport. */
				else teleport_player_to(ny,nx);
				break;
			}

			case 38:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(5+((plev-5)/5), 8));
				break;
			}

			case 39:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + plev, 3);
				break;
			}

			case 40:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          2 * plev, 2);
				break;
			}

			case 41:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          (3 * plev), 3);
				break;
			}

			case 42:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
				          80 + (plev * 2), 1);
				break;
			}

			case 43:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          120 + (plev * 2), 4);
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
				earthquake(py, px, 10, FALSE);
				break;
			}

			case 47: /* Damage is high because ball is centered on player */
			{
				fire_ball(GF_LITE, 0,
				          plev * 6, plev / 10);
				break;
			}

			case 48:
			{
				(void)mass_genocide();
				break;
			}

			case 49: /* Now all four single resist spells are actually useful. */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(plev) + plev);
				break;
			}

			case 50:
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(plev) + plev);
				break;
			}

			case 51:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(plev) + plev);
				break;
			}

			case 52:
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(plev) + plev);
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
				if (p_ptr->csp < p_ptr->msp)
				{
					p_ptr->csp += 1 + plev / 12;
					p_ptr->csp_frac = 0;
					if (p_ptr->csp > p_ptr->msp) (p_ptr->csp = p_ptr->msp);
					msg_print("You feel your head clear a little.");
					p_ptr->redraw |= (PR_MANA);
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				}
				break;
			}

			case 55:
			{
				if (!p_ptr->shield)
				{
					(void)set_shield(p_ptr->shield + randint(20) + 30);
				}
				else
				{
					(void)set_shield(p_ptr->shield + randint(10) + 15);
				}
				break;
			}

			case 56:
			{
				recharge(220);
				break;
			}

			case 57:
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(30) + 10 + plev);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(10));
				}
				break;
			}

			case 58:
			{
				if (!p_ptr->magicdef)
				{
					(void)set_extra_defences(30);
				}
				else
				{
					(void)set_extra_defences(p_ptr->magicdef + randint(10));
				}

				break;
			}

			/* Druid Spells */

			case 128: /* detect life */
			{
				(void)detect_monsters_normal();
				break;
			}

			case 129:  /* call light */
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 130: /* foraging */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 131:  /* blink */
			{
				teleport_player(10);
				break;
			}

			case 132:  /* combat poison */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 133:  /* lightning bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
				                  damroll(2+(plev/7), 7));
				break;
			}

			case 134:  /* door destruction */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 135:  /* turn stone to mud */
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 136:  /* ray of sunlight */
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A ray of golden yellow light appears.");
				lite_line(dir);
				break;
			}

			case 137:  /* Cure poison */
			{
				(void)set_poisoned(0);
				break;
			}

			case 138:  /* sleep creature */
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monsters_touch(plev + 10);
				break;
			}

			case 139:  /* frighten creature */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev + 10);
				break;
			}

			case 140:  /* detect trap/doors */
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 141:  /* cease small life */
			{
				(void)dispel_monsters(1 + plev / 10);
				break;
			}

			case 142:  /* frost bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_COLD, dir,
				                  damroll(2+(plev/5), 8));
				break;
			}

			case 143:  /* fire bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_FIRE, dir,
				                  damroll(3+(plev/5), 8));
				break;
			}

			case 144:  /* heroism */
			{
				(void)hp_player(20);
				if (!p_ptr->hero)
				{
					(void)set_hero(p_ptr->hero + randint(20) + 20);
				}
				else
				{
					(void)set_hero(p_ptr->hero + randint(10) + 10);
				}
				(void)set_afraid(0);
				break;
			}

			case 145:  /* remove curse */
			{
				if (remove_curse()) msg_print ("You feel tender hands aiding you.");
				break;
			}

			case 146:  /* acid bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_ACID, dir,
				                  damroll(5+(plev/5), 8));
				break;
			}


			case 147:  /* teleport monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 148:  /* poison bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_POIS, dir,
				                  damroll(5+(plev/4), 8));
				break;
			}

			case 149:  /* resist poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 150:  /* earthquake */
			{
				earthquake(py, px, 10, FALSE);
				break;
			}


			case 151:  /* resist fire & cold */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case 152:  /* detect all */
			{
				(void)detect_monsters_normal();
				(void)detect_monsters_invis();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				(void)detect_treasure();
				(void)detect_objects_normal();
				break;
			}

			case 153:  /* natural vitality */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				(void)hp_player(damroll(2, plev / 5));
				(void)set_cut(p_ptr->cut - plev / 2);
				break;
			}

			case 154:  /* resist acid & lightning */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				break;
			}

			case 155:  /* wither foe */
			{
				if (!get_aim_dir(&dir)) return;			
				(void)confuse_monster(dir, plev + 10);
				(void)slow_monster(dir, plev + 10);
				fire_bolt(GF_MISSILE, dir,
					damroll(plev / 7, 8));
				break;
			}

			case 156:  /* disarm trap */
			{
				if (!get_aim_dir(&dir)) return;
				(void)disarm_trap(dir);
				break;
			}

			case 157:  /* identify */
			{
				(void)ident_spell();
				break;
			}

			case 158:  /* create athelas */
			{
				(void)create_athelas();
				break;
			}

			case 159:  /* raging storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir,
				          plev + randint(60 + plev * 2), (1 + plev / 15));
				break;
			}

			case 160:  /* thunderclap */
			{
				msg_print("Boom!");
				fire_ball(GF_SOUND, 0,
				          plev * 2 + randint(80 + plev * 4), plev / 8);
				break;
			}

			case 161:  /* detect evil */
			{
				(void)detect_monsters_evil();
				break;
			}

			case 162:  /* song of frightening */
			{
				(void)fear_monsters(3 * plev / 2 + 10);
				break;
			}

			case 163:  /* sense surroundings */
			{
				map_area();
				break;
			}

			case 164:  /* sight beyond sight */
			{
				(void)detect_monsters_normal();
				(void)detect_monsters_invis();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				if (!p_ptr->tim_invis)
				{
					(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				}
				else
				{
					(void)set_tim_invis(p_ptr->tim_invis + randint(12) + 12);
				}
				break;
			}

			case 165:  /* herbal healing */
			{
				(void)hp_player(damroll(25 + plev / 2, 12));
				(void)set_cut(0);
				(void)set_poisoned(0);
				break;
			}

			case 166:  /* time blast */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(plev * 2, GF_TIME, dir,
				                  damroll(plev / 6, 8));
				break;
			}

			case 167:  /* essence of speed */
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(10) + plev / 2);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(5));
				}
				break;
			}

			case 168:  /* genocide */
			{
				(void)genocide();
				break;
			}

			case 169:  /* become ent */
			{
				shape = SHAPE_ENT;
				break;
			}

			case 170:  /* regain life */
			{
				(void)restore_level();
				break;
			}

			case 171:  /* intervention of Yavanna */
			{
				(void)dispel_evil(100);
				(void)hp_player(500);
				(void)set_blessed(p_ptr->blessed + randint(100) + 100);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 172:  /* blizzard */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, plev + randint(50 + plev * 2), 1 + plev / 12);
				break;
			}

			case 173:  /* trigger tsunami */
			{
				msg_print("You hurl mighty waves at your foes!");
				fire_ball(GF_WATER, 0,
				          plev * 3 + randint(60 + plev * 4), plev / 7);
				break;
			}

			case 174:  /* volcanic eruption */
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("The earth convulses and spits forth fire!");
				fire_ball(GF_FIRE, dir, 3 * plev / 2 + randint(50 + plev * 3), 1 + plev / 15);
				earthquake(py, px, plev / 3, TRUE);
				break;
			}

			case 175:  /* molten lightning */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PLASMA, dir,
				          2 * plev + randint(50 + plev * 3), 1);
				break;
			}

			case 176:  /* starburst.  Damage is high because ball is centered on player */
			{
				msg_print("Light bright beyond enduring dazzles your foes!");
				fire_ball(GF_LITE, 0,
				          plev * 5 + randint(plev * 6), plev / 10);
				break;
			}

			case 177:  /* become mouse */
			{
				shape = SHAPE_MOUSE;
				break;
			}

			case 178:  /* become ferret */
			{
				shape = SHAPE_FERRET;
				break;
			}

			case 179:  /* become hound */
			{
				shape = SHAPE_HOUND;
				break;
			}

			case 180:  /* become gazelle */
			{
				shape = SHAPE_GAZELLE;
				break;
			}

			case 181:  /* become lion */
			{
				shape = SHAPE_LION;
				break;
			}

			case 182:  /* song of lulling */
			{
				msg_print("Your tranquil music enchants those nearby.");

				(void)slow_monsters(3 * plev / 2);
				(void)sleep_monsters(3 * plev / 2);
				break;
			}

			case 183:  /* song of protection */
			{
				msg_print("Your song creates a mystic shield.");
				if (!p_ptr->shield)
				{
					(void)set_shield(p_ptr->shield + randint(30) + plev / 2);
				}
				else
				{
					(void)set_shield(p_ptr->shield + randint(15) + plev / 4);
				}
				break;
			}

			case 184:  /* song of dispelling */
			{
				msg_print("An unbearable discord tortures your foes!");

				(void)dispel_monsters(randint(plev * 2));
				(void)dispel_evil(randint(plev * 2));
				break;
			}

			case 185:  /* song of warding */
			{
				msg_print("Your song creates a place of sanctuary.");

				warding_glyph();
				break;
			}

			case 186:  /* song of renewal */
			{
				msg_print("Amidst the gloom, you envoke light and beauty; your body regains its natural vitality.");

				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				(void)restore_level();
				break;
			}

			/* Necromantic Spells */
			case 192: /* magic bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MANA, dir, damroll(2, 5 + plev / 7));
				break;

			}
			case 193: /* detect evil */
			{
				(void)detect_monsters_evil();
				break;
			}

			case 194: /* enhanced infravision */
			{
				set_tim_infra(p_ptr->tim_infra + 100 + randint(100));
				break;
			}

			case 195: /* break curse */
			{
				if (remove_curse()) msg_print ("You feel mighty hands aiding you.");
				break;
			}

			case 196: /* slow monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir, plev + 5);
				break;
			}

			case 197: /* sleep monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir, plev + 5);
				break;
			}

			case 198: /* horrify */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev + 15);
				break;
			}

			case 199: /* become bat */
			{
				take_hit(damroll(2, 4), "shapeshifting stress");
				shape = SHAPE_BAT;
				break;
			}

			case 200: /* door destruction */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 201: /* dark bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_DARK, dir,
				                  damroll((3 + plev / 7), 8));
				break;
			}

			case 202: /* noxious fumes */
			{
				fire_ball(GF_POIS, 0,
				          12 + plev, 2 + plev / 12);
				if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
				{
					set_poisoned(p_ptr->poisoned + rand_int(2) + 2);
				}
				break;
			}

			case 203: /* turn undead */
			{
				(void)turn_undead(3 * plev / 2);
				break;
			}

			case 204: /* turn evil */
			{
				(void)turn_evil(5 * plev / 4);
				break;
			}

			case 205: /* cure poison */
			{
				(void)set_poisoned(0);
				break;
			}

			case 206: /* dispel undead */
			{
				(void)dispel_undead(plev + 15 + randint(3 * plev / 2));
				break;
			}

			case 207: /* dispel evil */
			{
				(void)dispel_evil(plev + randint(plev));
				break;
			}

			case 208: /* see invisible */
			{
				if (!p_ptr->tim_invis)
				{
					set_tim_invis(p_ptr->tim_invis + 20 + randint(plev / 2));
				}
				else
				{
					set_tim_invis(p_ptr->tim_invis + 10 + randint(plev / 4));
				}
				break;
			}

			case 209: /* shadow shifting */
			{
				take_hit(damroll(1, 4), "shadow dislocation");
				teleport_player(20);
				break;
			}

			case 210: /* detect traps */
			{
				(void)detect_traps();
				break;
			}

			case 211: /* detect doors/stairs */
			{

				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 212: /* sleep monsters */
			{
				(void)sleep_monsters(plev + 5);
				break;
			}
			case 213: /* slow monsters */
			{
				(void)slow_monsters(plev + 5);
				break;
			}

			case 214: /* detect magic */
			{
				(void)detect_objects_magic();
				break;
			}

			case 215: /* death bolt */
			{
				take_hit(damroll(1, 6), "the dark arts");
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_SPIRIT, dir,
				                  damroll(2 + plev / 3, 8));
				break;
			}

			case 216: /* resist poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + plev / 2);
				break;
			}

			case 217: /* Dispel Demons */
			{
				(void)dispel_demons(2 * plev + randint(2 * plev));
				break;
			}

			case 218: /* dark spear */
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_DARK, dir, 12 + plev);
				break;
			}

			case 219: /* mana bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_MANA, dir,
				                  damroll(1 + plev / 2, 8));
				break;
			}

			case 220: /* genocide */
			{
				(void)genocide();
				break;
			}

			case 221: /* dark ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir, 50 + plev * 2, 2);
				break;
			}

			case 222: /* stench of death */
			{
				take_hit(damroll(3, 8), "the stench of Death");
				(void)dispel_living(50 + randint(plev));
				confu_monsters(plev + 10);

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, plev * 2, 5 + plev / 11);
				break;
			}

			case 223: /* probing */
			{
				(void)probing();
				break;
			}

			case 224: /* shadow mapping */
			{
				map_area();
				break;
			}

			case 225: /* identify */
			{
				(void)ident_spell();
				break;
			}

			case 226: /* shadow warping */
			{
				take_hit(damroll(2, 6), "shadow dislocation");
				teleport_player(plev * 3);
				break;
			}

			case 227: /* poison ammo - for assassins only */
			{
				(void)brand_missile();
				break;
			}

			case 228: /* detect all monsters */
			{
				(void)detect_monsters_normal();
				(void)detect_monsters_invis();
				break;
			}

			case 229: /* strike at life */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_NETHER, dir,
				                  damroll(3 * plev / 5, 11));
				break;
			}

			case 230: /* orb of death */
			{
				take_hit(damroll(2, 8), "Death claiming his wages");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SPIRIT, dir, 25 + plev * 3, 2);
				break;
			}

			case 231: /* dispel life */
			{
				(void)dispel_living(60 + randint(plev * 2));
				break;
			}

			case 232: /* vampiric drain */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_SPIRIT, dir,
				                  damroll(plev / 3, 11));
				(void)hp_player(3 * plev);
				p_ptr->food += 1000;
				break;
			}

			case 233: /* word of destruction */
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 234: /* teleport away */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 235: /* smash undead */
			{
				(void)dispel_undead(plev * 3 + randint(50));
				break;
			}

			case 236: /* mass genocide */
			{
				(void)mass_genocide();
				break;
			}

			case 237: /* darkness storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir,
				          11 * plev / 2, plev / 7);
				break;
			}

			case 238: /* recharging */
			{
				(void)recharge(140);
				break;
			}

			case 239: /* become werewolf */
			{
				take_hit(damroll(2, 7), "shapeshifting stress");
				shape = SHAPE_WEREWOLF;
				break;
			}

			case 240: /* dispel curse */
			{
				if (remove_all_curse()) msg_print ("You feel mighty hands aiding you.");
				break;
			}

			case 241: /* become vampire */
			{
				take_hit(damroll(3, 6), "shapeshifting stress");
				shape = SHAPE_VAMPIRE;
				break;
			}

			case 242: /* haste self */
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(10 + randint(20));
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(5));
				}
				break;
			}

			case 243: /* prepare black breath */
			{
				msg_print("Your hands start to radiate Night.");
				p_ptr->confusing = 2;
				break;
			}

			case 244: /* resist acid and cold */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case 245: /* heal any wound */
			{
				(void)set_cut(0);
				(void)set_stun(0);
				break;
			}

			case 246: /* protection from evil */
			{
				(void)set_protevil(p_ptr->protevil + plev / 2 + randint(plev));
				break;
			}

			case 247: /* black blessing */
			{
				(void)set_blessed(p_ptr->blessed + randint(66));
				break;
			}

			case 248: /* banish evil */
			{
				(void)banish_evil(100);
				break;
			}

			case 249: /* shadow shield */
			{
				if (!p_ptr->shield)
				{
					(void)set_shield(p_ptr->shield + randint(20) + 10);
				}
				else
				{
					(void)set_shield(p_ptr->shield + randint(10) + 5);
				}
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

	/* Alter shape, if necessary. -LM- */
	if (shape) shapechange(shape);


	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

}


/*
 * Brand the current weapon
 */
void brand_weapon(void)
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
 * Pray a prayer
 */
void do_cmd_pray(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir, chance;

	int shape = 0;

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

	/* Access the item's sval */
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


	/* Access the spell */
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
			case 0: /* Detect Evil */
			{
				(void)detect_monsters_evil();
				break;
			}

			case 1: /* Bind Door */
			{
				(void)magic_spiking();
				break;
			}

			case 2: /* Bless */
			{
				if (!p_ptr->blessed)
				{
					(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				}
				else
				{
					(void)set_blessed(p_ptr->blessed + randint(4) + 4);
				}
				break;
			}

			case 3: /* Remove Fear */
			{
				(void)set_afraid(0);
				break;
			}

			case 4: /* Call Light */
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 5: /* Find Traps */
			{
				(void)detect_traps();
				break;
			}

			case 6: /* Detect Doors/Stairs */
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 7: /* Slow Poison */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 8: /* Cure Serious Wounds */
			{
				(void)hp_player(damroll(4, plev / 4 + 6));
				(void)set_cut((p_ptr->cut / 2) - 5);
				break;
			}

			case 9: /* Scare Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, (3 * plev / 2) + 10);
				break;
			}

			case 10: /* Portal */
			{
				teleport_player(2 * plev);
				break;
			}

			case 11: /* Chant */
			{
				if (!p_ptr->blessed)
				{
					(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				}
				else
				{
					(void)set_blessed(p_ptr->blessed + randint(8) + 8);
				}
				break;
			}

			case 12: /* Sanctuary */
			{
				(void)sleep_monsters_touch(plev + 15);
				break;
			}

			case 13: /* Satisfy Hunger */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 14: /* Remove Curse */
			{
				if (remove_curse()) msg_print ("You feel kindly hands aiding you.");
				break;
			}

			case 15: /* Resist Heat and Cold */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + plev / 2);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + plev / 2);
				break;
			}

			case 16: /* Neutralize Poison */
			{
				(void)set_poisoned(0);
				break;
			}

			case 17: /* Orb of Draining */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          (damroll(3, 6) + plev / 2 +
				           (plev / ((p_ptr->pclass == CLASS_PRIEST) ? 2 : 4))),
				          ((plev < 30) ? 2 : 3));
				break;
			}

			case 18: /* Sense Invisible */
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + plev);
				break;
			}

			case 19: /* Protection from Evil */
			{
				if (!p_ptr->protevil)
				{
					(void)set_protevil(p_ptr->protevil + randint(24) + 2 * plev);
				}
				else
				{
					(void)set_protevil(p_ptr->protevil + randint(30));
				}
				break;
			}

			case 20: /* Cure Mortal Wounds */
			{
				(void)hp_player(damroll(9, plev / 3 + 12));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 21: /* Earthquake */
			{
				earthquake(py, px, 10, FALSE);
				break;
			}

			case 22: /* Sense Surroundings */
			{
				map_area();
				break;
			}

			case 23: /* Turn Undead */
			{
				(void)turn_undead((3 * plev / 2) + 10);
				break;
			}

			case 24: /* Prayer */
			{
				if (!p_ptr->blessed)
				{
					(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				}
				else
				{
					(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				}
				break;
			}

			case 25: /* Dispel Undead */
			{
				(void)dispel_undead(randint(plev * 3));
				break;
			}

			case 26: /* Heal */
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 27: /* Dispel Evil */
			{
				(void)dispel_evil(randint(plev * 3));
				break;
			}

			case 28: /* Sacred Shield */
			{
				if (!p_ptr->shield)
				{
					(void)set_shield(p_ptr->shield + randint(20) + plev / 2);
				}
				else
				{
					(void)set_shield(p_ptr->shield + randint(10) + plev / 4);
				}
				break;
			}

			case 29: /* Glyph of Warding */
			{
				warding_glyph();
				break;
			}

			case 30: /* Holy Word */
			{
				(void)dispel_evil(randint(plev * 4));
				(void)hp_player(300);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 31: /* Detect Monsters */
			{
				(void)detect_monsters_normal();
				break;
			}

			case 32: /* Detection */
			{
				(void)detect_all();
				break;
			}

			case 33: /* Probing */
			{
				(void)probing();
				break;
			}

			case 34: /* Perception */
			{
				(void)ident_spell();
				break;
			}

			case 35: /* Clairvoyance */
			{
				wiz_lite(FALSE);
				break;
			}

			case 36: /* Banishment */
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			} 

			case 37: /* Healing */
			{
				(void)hp_player(700);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 38: /* Sacred Knowledge */
			{
				(void)identify_fully();
				break;
			}

			case 39: /* Restoration */
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 40: /* Remembrance */
			{
				(void)restore_level();
				break;
			}

			case 41: /* Ball of Light */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_LITE, dir, plev * 3, 3);
				break;
			}
			case 42: /* Holy Lance */
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_HOLY_ORB, dir, 3 * plev / 2);
				break;
			}
			case 43: /* Word of Destruction */
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 44: /* Annihilation */
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, plev * 3 + randint(100));
				break;
			}

			case 45: /* Call on Varda */
			{
				msg_print("Gilthoniel A Elbereth!");
				fire_ball(GF_LITE, 0,
				          plev * 10, plev / 7 + 2);
				(void)fear_monsters(plev * 2);
				(void)hp_player(500);
				break;
			}

			case 46: /* Unbarring Ways */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 47: /* Recharging */
			{
				(void)recharge(140);
				break;
			}

			case 48: /* Dispel Curse */
			{
				if (remove_all_curse()) msg_print("A beneficent force surrounds you for a moment.");
				break;
			}

			case 49: /* Disarm Trap */
			{
				if (!get_aim_dir(&dir)) return;
				(void)disarm_trap(dir);
				break;
			}

			case 50: /* Enchant Weapon */
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

			case 51: /* Enchant Armour */
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

			case 52: /* Blink */
			{
				teleport_player(10);
				break;
			}

			case 53: /* Teleport Self */
			{
				teleport_player(plev * 4);
				break;
			}

			case 54: /* Teleport Other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 55: /* Teleport Level */
			{
				(void)teleport_player_level();
				break;
			}

			case 56: /* Word of Recall */
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

			case 57: /* Alter Reality */
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

	/* Alter shape, if necessary. -LM- */
	if (shape) shapechange(shape);


	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}
