/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* from spells1.c */
extern int blast_center_x,blast_center_y;

static bool balefire_done = FALSE;

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

	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" :
	((mp_ptr->spell_book == TV_PSI_BOOK) ? "power" : "prayer"));


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
      if (mp_ptr->spell_book != TV_PSI_BOOK)
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

	/* Access the item's sval */
	sval = o_ptr->sval;
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

	if (p_ptr->blind || no_lite())
      if (mp_ptr->spell_book != TV_PSI_BOOK)
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

	/* Access the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK)
	{
		if (!get_spell(&spell, "study", sval, FALSE) && (spell == -1)) return;
		/* Ask for a spell, allow cancel */
	}

	if (mp_ptr->spell_book == TV_PSI_BOOK)
	  if (!get_spell(&spell,"study",sval/3,FALSE) && (spell == -1)) return;

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
		 (mp_ptr->spell_book == TV_PSI_BOOK) ? "focus" : "book");

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
	           p, spell_names[mp_ptr->spell_type][spell]);

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

	int plev = p_ptr->lev;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if (mp_ptr->spell_book != TV_MAGIC_BOOK)
	{
                 if (mp_ptr->spell_book == TV_PSI_BOOK)
                   do_cmd_psi();
                 else
                 if (mp_ptr->spell_book == TV_PRAYER_BOOK)
                   do_cmd_pray();
                 else
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
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = ((p_ptr->pclass == 1) ? plev : (plev / 2));

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

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

int get_psi_cost(int spell)
{

  int cost = mp_ptr->info[spell].smana;
  switch (spell)
    {
    case 14: /* teleport other */
    case 21: /* satisfy hunger */
    case 29: /* complete healing */
    case 31: /* wall of force */
    case 40: /* cannibalize */
    case 41: /* intensify */
    case 42: /* splice */
    case 43: /* receptacle */
    case 46: /* psychic surgery */
    case 49: /* detect monsters */
    case 51: /* reveal secrets */
    case 52: /* clairvoyance */
    case 53: /* m. analysis */
    case 54: case 55: /* read object/aura */
      return cost;
    }
  return (cost * (2 + pa_ptr->intensify)) / 2;
}

int amplify_dam(int dam)
{
  return (dam * (3 + pa_ptr->intensify)) / 3;
}

int inverse_amplify(int dam)
{
  return (dam * 3) / (3 + pa_ptr->intensify);
}

static bool item_tester_hook_drain(object_type *o_ptr)
{
   int t = o_ptr->tval;
   if ((t != TV_STAFF) && (t != TV_WAND)) return FALSE;
   return (o_ptr->pval != 0);
}

#define req_focus(lvl) if (f_lev<lvl) {msg_print(\
"Your focus isn't strong enough to use that power."); return;}

int min(int a,int b) {return (a<b) ? a : b;}

int max(int a,int b) {return (a>b) ? a : b;}

int splice_val = 0,splice_n = 0;

#define A(x) amplify_dam(x)

/*
 * use a psionic power
 */

void do_cmd_psi(void)
{
  int                     item, sval, spell, dir, f_lev;
  int                     chance,cost = 0;
  int                     plev = p_ptr->lev;

	object_type *o_ptr;

  magic_type              *s_ptr;
  char temp[20];
  int i,j;


  /* Require spell ability */
  if (mp_ptr->spell_book != TV_PSI_BOOK)
    {
      if (mp_ptr->spell_book == TV_MAGIC_BOOK)
	do_cmd_cast();
      else
	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
	  do_cmd_pray();
	else
	  msg_print("You cannot use psionics!");
      return;
    }

  /* Not when confused */
  if (p_ptr->confused)
		{
      msg_print("You are too confused!");
      return;
		}

  /* Note: psionics *are* possible when blind */

  /* Metapsionics allows use of any focus */
  if (meta_psi_lev)
    {
      /* Restrict choices to spell books */
      item_tester_tval = mp_ptr->spell_book;

      /* Get an item (from inven or floor) */
      if (!get_item(&item,"Use which discipline? ","You have no psionic foci!"
		    ,USE_EQUIP | USE_INVEN | USE_FLOOR)) return;

      /* Get the item (in the pack) */
      if (item >= 0)
	o_ptr = &inventory[item];

      /* Get the item (on the floor) */
      else
	o_ptr = &o_list[0 - item];

      /* Access the item's sval - extract power and type*/
      sval = o_ptr->sval;
      f_lev = sval % 3;

      /* If we are using metapsionics limit focus power */
      if (o_ptr != &inventory[INVEN_NECK]) f_lev = min(meta_psi_lev,f_lev);
      sval /= 3;
    }
		else
		{
      o_ptr = &inventory[INVEN_NECK];

      item = 0;
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
      p_ptr->csp += cost >> 1;
      if (splice_val)
	{
	  splice_val -= max(damroll(2,8),splice_val / 2);
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
		fire_ball(GF_EMPATHY,dir,A(plev),max(4,plev / 5));                      
	      }
	    else
	      project_los(GF_EMPATHY,plev);
	    break;
	  }

	case 2: /* Mental Barrier */
	  {
	    msg_print("You shield your psyche from mental attacks.");
	    p_ptr->oops = 1;
	    pa_ptr->mental_barrier += randint(A(20 + 2 * plev));
	    p_ptr->resist_confu = TRUE;
	    p_ptr->resist_fear = TRUE;
	    p_ptr->sustain_int = TRUE;
	    p_ptr->sustain_wis = TRUE;
	    break;
	  }

	case 3: /* Psychic Crush */
	  {
	    req_focus(1);
	    if (!get_aim_dir(&dir)) return;
	    fire_ball(GF_PSI2, dir,A(damroll(plev , 9)),0);
	    break;
	  }

	case 4: /* Intimidate */
	  {
	    if (!get_aim_dir(&dir)) return;
	    fear_monster(dir,A(plev * 3));
	    break;
	  }

	case 5: /* Sleep */
	  {
	    if (!get_aim_dir(&dir)) return;
	    fire_ball(GF_OLD_SLEEP,dir,A(plev * 3),0);
	    break;
	  }

	case 6: /* Psionic Blast */
	  {
	    if (!get_aim_dir(&dir)) return;
	    fire_ball(GF_PSI, dir, A(plev * 5),2);
	    break;
	  }

	case 7: /* Mind Wrack */
	  {
	    req_focus(1);
	    if (get_aim_dir(&dir)) return;
	    fire_ball(GF_PSI3,dir,A(damroll(2 , plev)),0);
	    break;
	  }

	case 8: /* Domination */
	  {
	    req_focus(2);
	    if (!get_aim_dir(&dir)) return;
	    fire_ball(GF_DOMINATE,dir,A(plev + randint(plev * 4)),0);
	    break;
	  }

#if 0
	case 9: /* Mass Domination */
	  {
	    req_focus(2);
	    project_los(GF_DOMINATE,A(damroll(3,plev)));
	    break;
	  }

	case 10: /* Suicide */
	  {
	    msg_print("You command your followers to kill themselves!");
	    break;
	  }
#endif 

	case 9: /* Amnesia */
	  {
	    req_focus(2);
	    if (!get_aim_dir(&dir)) return;
	    fire_ball(GF_AMNESIA,dir,A(plev + (rand_int(2) ? randint(plev * 3) : 0)),0);
	    break;
	  }
	  
	  /* *** PSYCHOPORTATION *** */
	  
	case 10: /* Blink */
	  {
	    teleport_player(A(10));
	    break;
	  }

	case 11: /* Teleport */
	  {
	    teleport_player(A(plev * 5));
	    break;
	  }

	case 12: /* Dimension Door */
	  {
	    msg_print("You open a dimensional gate. Choose a destination.");
	    if (!tgt_pt(&i,&j)) return;
	    p_ptr->energy -= inverse_amplify(60 - plev);
	    if (!cave_empty_bold(j,i) || (cave_info[j][i] & CAVE_ICKY) ||
		(distance(j,i,p_ptr->py,p_ptr->px) > A(plev + 2)) ||
		(!rand_int(A(plev * plev / 2))))
	      {
		msg_print("You fail to exit the astral plane correctly!");
		p_ptr->energy -= inverse_amplify(100);
		teleport_player(A(10));
	      }
	    else teleport_player_to(j,i);
	    break;
	  }

	case 13: /* Probability Travel */
	  {
	    req_focus(1);
	    p_ptr->oops = 1;
	    pa_ptr->prob_travel += A((plev / 8) - 3 + randint(5));
	    if (pa_ptr->prob_travel > 0)
	      msg_print("You enter the astral plane!");
	    else 
	      {
		msg_print("You fail to reach the astral plane.");
		pa_ptr->prob_travel = 0;
	      }
	    break;
	  }

	case 14: /* Teleport Other */
	  {
	    req_focus(1);
	    if (!get_aim_dir(&dir)) return;
	    (void)teleport_monster(dir);
	    break;
	  }

	case 15: /* Recall */
	  {
	    req_focus(2);
	    if (!p_ptr->word_recall)
	      {
		if (pa_ptr->ts_anchor)
		  {
		    msg_print("Your time/space anchor prevents you from recalling.");
		    break;
		  }
		p_ptr->word_recall = inverse_amplify(rand_int(30));
		msg_print("The air about you becomes charged...");
	      }
	    else
	{
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	      }
	    break;
	  }

	case 16: /* Time Control */
	  {
	    req_focus(1);
	    if (pa_ptr->ts_anchor)
	      {
		msg_print("You end your time/space anchor.");
		pa_ptr->ts_anchor = 0;
	      }
	    (void)set_fast(p_ptr->fast + A(plev + randint(20)));
	    while (p_ptr->fast > 100 + randint(plev * 10))
	      {
		msg_print("You accelerate time too much!");
		i = randint(randint(randint(p_ptr->fast * 3)));
		_killer = "a blast from the past.";
		project(-2,rand_int(5),p_ptr->py,p_ptr->px,i,GF_TIME,
			PROJECT_KILL);
		p_ptr->fast -= i;
		if (p_ptr->fast < 0) p_ptr->fast = 0;
	      }
	    break;
	  }
			  
	case 17: /* Time Bolt */
	  {
	    req_focus(1);
	    if (pa_ptr->ts_anchor)
	      if (!rand_int(3))
		{
		  msg_print("The time/space anchor stops your time bolt!");
		  break;
		}
	      else if (!rand_int(3))
		{
		  msg_print("Your time/space anchor collapses!");
		  pa_ptr->ts_anchor = 0;
		}
 	    if (!get_aim_dir(&dir)) return;
	    life_drained = 0;
	    fire_bolt_or_beam(plev,GF_TIME,dir,A(damroll(10,plev)));
	    if (life_drained)
	      {
		msg_print("You have stolen precious seconds of time!");
		p_ptr->energy += min(life_drained,damroll(2,plev));
	      }
	    break;
	  }

	case 18: /* Time/Space Anchor */
	  {
	    msg_print("With immense mental effort you temporarily stabilize local space.");
	    p_ptr->oops = 1;
	    pa_ptr->ts_anchor += randint(randint(A(100 + plev)));
	    break;
	  }

	case 19: /* Time Shift */
	  {
	    req_focus(2);
	    if (pa_ptr->ts_anchor)
	      {
		msg_print("You end your time/space anchor.");
		pa_ptr->ts_anchor = 0;
	      }
	    msg_print("Time freezes around you!");
	    p_ptr->energy += A(damroll(5,plev));
	    break;
	  }

	  /* *** PSYCHOMETABOLISM *** */

	case 20: /* Cell Adjustment */
	  {
	    (void)hp_player(A(damroll(plev / 2, 6)));
	    (void)set_cut(p_ptr->cut - A(15));
	    break;
	  }

	case 21: /* Satisfy Hunger */
	  {
	    msg_print("You start converting your body's energy into food.");
	    p_ptr->energy -= inverse_amplify(300 - 5 * plev);
	    (void)set_food(PY_FOOD_MAX - 1);
	    break;
	  }

	case 22: /* Adrenaline Control */
	  {
	    msg_print("Adrenaline surges through your veins!");
	    p_ptr->oops = 1;
	    pa_ptr->adrenaline += A(randint(20) + plev);
	    if (pa_ptr->biofeedback)
	      {
		msg_print("You lose control of your blood flow!");
		i = randint(randint(pa_ptr->adrenaline));
		take_hit(damroll(2,i),"adrenaline poisoning");
		pa_ptr->adrenaline -= i;
		pa_ptr->biofeedback = 0;
	      }
	    while (pa_ptr->adrenaline > 30 + randint(plev * 5))
	      {
		msg_print("Your body can't handle that much adrenaline!");
		i = randint(randint(pa_ptr->adrenaline));
		take_hit(damroll(3,i * 2),"adrenaline poisoning");
		pa_ptr->adrenaline -= i;
	      }
	    p_ptr->update |= PU_BONUS | PU_HP;
	    handle_stuff();
	    break;
	  }
		  
	case 23: /* Biofeedback */
	  {
	    msg_print("Your pulse slows and your body prepares to resist damage.");
	    p_ptr->oops = 1;
	    pa_ptr->biofeedback += randint(A(10 + plev));
	    if (pa_ptr->adrenaline)
	      {
		msg_print("The adrenaline drains out of your veins.");
		pa_ptr->adrenaline = 0;
		if (!rand_int(8))
		  {
		    msg_print("You start to tremble as your blood sugar crashes.");
		    set_slow(p_ptr->slow + rand_int(rand_int(16)));
		    if (!rand_int(5)) set_paralyzed(p_ptr->paralyzed + 1);
		    if (!rand_int(3)) set_stun(p_ptr->stun + rand_int(30));
		  }
	      }
	    while (pa_ptr->biofeedback > 25 + rand_int(rand_int(plev)))
	      {
		msg_print("You speed up your pulse to avoid fainting!");
		pa_ptr->biofeedback -= randint(20);
	      }
	    break;
	  }
		  
	case 24: /* Shadowform */
	  {
	    req_focus(2);
	    msg_print("You leave the physical world and become a living shadow!");
	    p_ptr->oops = 1;
	    pa_ptr->shadow_form += randint(randint(A(plev + 12)));
	    if (pa_ptr->shadow_form > 30 + randint(plev * 2))
	      {
		msg_print("You begin to fade into the netherworld!");
		i = randint(pa_ptr->shadow_form);
		_killer = "shadows";
		project(-2,1,p_ptr->py,p_ptr->px,damroll(i,6),GF_NETHER,PROJECT_KILL);
		pa_ptr->shadow_form -= i;
		if (pa_ptr->shadow_form < 0) pa_ptr->shadow_form = 0;
	      }
	    break;
	  }

	case 25: /* Drain Life */
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
			  
	case 26: /* Double Pain */
	  {
	    req_focus(2);
	    if (!get_aim_dir(&dir)) return;
	    fire_ball(GF_PAIN,dir,randint(A(plev * 3 + 15)),0);
	    break;
	  }

	case 27: /* Energy Contaiment */
	  {
	    int n = A(10 + randint(20));
	    (void)set_oppose_acid(p_ptr->oppose_acid + n);
	    (void)set_oppose_elec(p_ptr->oppose_elec + n);
	    (void)set_oppose_fire(p_ptr->oppose_fire + n);
	    (void)set_oppose_cold(p_ptr->oppose_cold + n);
	    (void)set_oppose_pois(p_ptr->oppose_pois + n);
	    break;
	  }

	case 28: /* Death Field */
	  {
	    req_focus(1);
	    sprintf(temp,"Intensity (1-%d): ",plev);
	    i = get_quantity(temp,plev);
	    i = damroll(6,i);
	    msg_print("Waves of darkness begin emanating from your body.");
	    take_hit(A(damroll(2,i)),"idiocy");
	    project(-1,3,p_ptr->py,p_ptr->px,A(damroll(15,i)),GF_NETHER,
		    PROJECT_KILL);
	    break;
	  }

	case 29: /* Complete Healing */
	  {
	    req_focus(1);
	    msg_print("You enter a trance and your body begins to rapidly recover.");
	    (void)hp_player(5000);
	    (void)set_poisoned(0);
	    (void)set_stun(0);
	    (void)set_cut(0);
	    p_ptr->energy -= 350 - plev * 2; /* intensify doesn't help */
	    break;
	  }

	  /* *** PSYCHOKINESIS *** */

	case 30: /* Light Control */
	  {
	    if (!p_ptr->blind) 
	      msg_print("Your infrared transforms into a brilliant burst of visible light.");
	    project(-1,(plev / 10) + 1,p_ptr->py,p_ptr->px,A(damroll(3, plev)),
		    GF_LITE_WEAK, PROJECT_GRID | PROJECT_KILL);
	    lite_room(p_ptr->py,p_ptr->px);
	    break;
	  }

	case 31: /* Wall of Force */
	  {
	    if (no_fw == 100)
	      {
		msg_print("You already have 100 force walls - that's the limit.");
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
	    cave_feat[j][i] = FEAT_FORCE_WALL;
	    lite_spot(j,i);
	    break;
	  }

	case 32: /* Inertial Barrier */
	  {
	    msg_print("The air around you solidifies into a protective shield.");
	    p_ptr->oops = 1;
	    pa_ptr->inertial_barrier += randint(A(20 + plev / 2));
	    while (pa_ptr->inertial_barrier > plev + randint(200))
	      {
		msg_print("The inertia makes it difficult to move!");
		_killer = "air pressure";
		i = randint(50);
		project(-2,randint(3),p_ptr->py,p_ptr->px,i * 2,GF_INERTIA,
			PROJECT_KILL);
		if ((pa_ptr->inertial_barrier -= i) < 0)
		  pa_ptr->inertial_barrier = 0;
	      }
	    break;
	  }

	case 33: /* Project Force */
	  {
	    if (!get_aim_dir(&dir)) return;
            if (plev > 30) plev = 30 + (plev - 30) / 4;
            fade_dam_on = TRUE;
	    fire_beam(GF_FORCE,dir,A(damroll(3,plev)));
            fade_dam_on = FALSE;
	    break;
	  }

	case 34: /* Sonic Boom */
	  {
	    if (!get_aim_dir(&dir)) return;
            msg_print("You hear a roar of sound.");
            if (plev > 30) plev = 30 + (plev - 30) / 4;
	    fire_ball(GF_SOUND, dir,A(30 + 2 * plev),5);
	    break;
	  }

	case 35: /* Disintegrate */
	  {
	    req_focus(1);
	    if (!get_aim_dir(&dir)) return;
            if (plev > 30) plev = 30 + (plev - 30) / 2;
	    fire_ball(GF_DISINTEGRATE,dir,A(damroll(plev,9)),2);
            break;
	  }

	case 36: /* Sphere of Cold */
	  {
	    req_focus(1);
	    if (!get_aim_dir(&dir)) return;
	    fire_ball(GF_COLD,dir,A(plev * 9 - 36),1);
	    break;
	  }

	case 37: /* Fire Eruption */
	  {
	    req_focus(1);
	    if (!get_aim_dir(&dir)) return;
	    fire_ball(GF_FIRE,dir,A(plev * 15 - 65),0);
	    break;
	  }

	case 38: /* Detonate */
	  {
	    int radius = 0;
 	    if (!get_aim_dir(&dir)) return;
	    if (!rand_int(plev))
	      {
		msg_print("Your imperfect concentration causes the power to backfire!");
		_killer = "detonate backfire";
		project(-2,2,p_ptr->py,p_ptr->px,A(damroll(15,min(plev,40))),GF_METEOR, 
			PROJECT_GRID | PROJECT_KILL);
		do {radius += randint(6);} while (!rand_int(3));
		radius = (radius < 5) ? 10 : radius * 2;
	      }
	    else
	      {
		fire_ball(GF_METEOR,dir,A(damroll(15,min(plev,40))),2);
		if (!rand_int(8))
		  {
		    msg_print("The dungeon collapses around the blast");
                    do {radius += randint(6);} while (!rand_int(3));
		  }
	      }
	    if (radius) earthquake(blast_center_y,blast_center_x,radius);
 	    break;
	  }

	case 39: /* Balefire */
	  {
	    req_focus(2);
	    if (!get_aim_dir(&dir)) return;
	    balefire_done = FALSE;
	    fire_beam(GF_BALEFIRE,dir,A(damroll(5,p_ptr->chp)));
	    break;
	  }

	  /* *** METAPSIONICS *** */

	case 40: /* Cannibalize */
	  {
	    int m = 0,h;
	    do
	      {
		m = get_quantity("How much mana?",999);
		if (pa_ptr->precognition)
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

        case 41: /* Intensify */
	  {
	    p_ptr->oops = 1;
	    pa_ptr->intensify = get_quantity(format("Intensify level? (max %d):",
						   (plev + 7) / 8),(plev + 7) / 8);
	    /* msg_print("x<=(L+7)/8;main x/L;power*=1+x/3;cost*=1+x/2"); */
	    break;
	  }

	case 42: /* Splice */
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

	case 43: /* Receptacle */
	  item_tester_hook = item_tester_hook_drain;
	  if (!get_item(&item, "Drain what item?", "Nothing charged to drain",
			USE_EQUIP | USE_FLOOR | USE_INVEN)) break;
	  if (item >= 0) o_ptr = &inventory[item];
	  else o_ptr = &o_list[0 - item];

	  msg_print("Energy drains from your pack!");

	  i = 0;
	  while ((p_ptr->csp < p_ptr->msp) && (o_ptr->pval > 0) &&
		 (i < 6 + randint(60)))
	    {
	      p_ptr->csp += damroll(o_ptr->number,plev);
	      j = randint((plev > 40) ? randint(6) : 6);
	      o_ptr->pval -= j;
	      i += j;
	    }

	  if ((j = o_ptr->pval) < 0) o_ptr->pval = 0;
	  else
	    if ((i = i * i - 40) > 0)
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

	case 44: /* Empower */
	  {
	    recharge(rand_int(3)?100:100*randint(randint(plev)));
	    break;
	  }

	case 45: /* Psychic Drain */
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

	case 46: /* Psychic Surgery */
	  {
	    restore_level();
	    set_afraid(0);
	    (void)do_res_stat(A_WIS);
	    (void)do_res_stat(A_INT);
	    break;
	  }
           
 	case 47: /* Ultrablast */
 	  {
 	    req_focus(1);
	    msg_print("You project waves of psychic energy with all your strength!");
 	    project_los(GF_PSI,A(damroll(40,plev)));
	    if (!rand_int(plev))
	      {
		int temp = 0;
		msg_print("You lose control over the attack and your mana drains away.");
		do {temp += cost;} while (!rand_int(3));
		cost = temp;
	      }
 	    break;
 	  }
  			       
 	case 48: /* *Ultrablast* */
 	  {
 	    req_focus(2);
	    msg_print("You project deadly psychic energy with all your strength!");	 
 	    project_los(GF_PSI2,A(damroll(100,plev)));
	    if (!rand_int(plev - 25))
	      {
		int temp = 0;
		msg_print("You lose control over the attack and your mana drains away.");
		do {temp += cost;} while (!rand_int(5));
		cost = temp;
	      }
 	    break;
 	  }

	  /* *** CLAIRSENTIENCE *** */

	case 49: /* Detect Monsters */
	  {
	    (void)detect_monsters_normal();
	    break;
	  }

	case 50: /* Awareness */
	  {
	    msg_print("You become aware of nearby intelligence!");
	    p_ptr->oops = 1;
	    pa_ptr->awareness += A(plev * plev / 5 + randint(80));
	    p_ptr->telepathy = TRUE;
	    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
	    p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);
	    handle_stuff();
	    break;
	  }

	case 51: /* Reveal Secrets */
	  {
	    detect_doors();
	    detect_traps();
	    detect_stairs();
	    break;
	  }

	case 52: /* Clairvoyance */
	  {
	    msg_print("An image of your surroundings forms in your mind...");
	    map_area();
	    break;
	  }

	case 53: /* Metaphysical analysis */
	  {
	    show_aura();
	    show_durations();
	    break;
	  }

	case 54: /* Read Object */
	  {
	    req_focus(1);
	    (void)ident_spell();
	    break;
	  }

	case 55: /* Read Aura */
	  {
	    req_focus(2);
	    (void)identify_fully();
	    break;
	  }

	case 56: /* Precognition */
	  {
	    req_focus(2);
	    msg_print("Images from the future begin to flash before your eyes.");
	    p_ptr->oops = 1;
	    pa_ptr->precognition += A(randint(plev));
	    break;
	  }

	case 57: /* Analyze Monster */
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
      if (mp_ptr->spell_book == TV_MAGIC_BOOK)
	do_cmd_cast();
      else
	if (mp_ptr->spell_book == TV_PSI_BOOK)
	  do_cmd_psi();
	else
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
				           (plev / ((p_ptr->pclass == 2) ? 2 : 4))),
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

int get_balefire_effect()
{
  int result = 0;
  if (balefire_done) return 0;
  balefire_done = TRUE;
  if (rand_int(10)) return 0;
  msg_print("The balefire tears a hole in reality!");
  if (pa_ptr->ts_anchor)
    {
      msg_print("Your time/space anchor is ripped apart!");
      pa_ptr->ts_anchor = 0;
      if (!rand_int(6)) return BALE_ABORT;
    }
  if (!rand_int(8)) return BALE_ABORT;
  while (!result)
    {
      if (!rand_int(12)) result |= BALE_HOLE;
      if (!rand_int(12)) result |= BALE_SUMMON;
      if (!rand_int(8)) result |= rand_int(3) ? (rand_int(2) ? BALE_KILL : BALE_CLONE) : BALE_HEAL;
      if (!rand_int(8)) result |= BALE_TIME_ATTACK;
      // if (!rand_int(20)) result |= BALE_TIME_TRAVEL;
      if (!rand_int(20)) result |= BALE_HISTORY;
      if (!rand_int(12)) result |= BALE_ALTER;
      if (!rand_int(30)) result |= BALE_ALTER | BALE_TIME_TRAVEL;
      if (!rand_int(40)) result |= BALE_HOLE | BALE_SUMMON | BALE_CLONE | BALE_TIME_ATTACK;
    }
  return result;
}

void time_travel(bool forward)
{
  msg_format("You are sent far into the %s...",forward ? "future" : "past");
  if ((town_special & ~TOWN_MESSAGE) == TOWN_DEAD_FUTURE)
    {
      if (forward)
	switch (rand_int(5))
	  {
	  case 1:
	    msg_print("You arrive in a world where Morgoth has achieved total domination.");
	    msg_print("Your quest hopeless, you are forced to surrender to his armies.");
	    msg_print("You live the rest of your life in slavery remembering your mistakes...");
	    p_ptr->chp = 0;
	    take_hit(1,"an orcish slavemaster");
	    break;
	  case 2:
	    msg_print("You arrive to find Morgoth's empire replaced by...");
	    msg_print("a republic of insects and grasses.");
	    msg_print("No enemy is in sight and the air is clean and fresh.");
	    msg_print("Unfortunately, your armor can't stop the hard radiation");
	    msg_print("suffusing the atmosphere.  You soon realize that the");
	    msg_print("price of total victory has been total war.");
	    p_ptr->chp = 0;
	    take_hit(1,"Uranium-238");
	    break;
	  default:
	    msg_print("You arrive even further in the future.");
	    msg_print("You feel vaguely relieved.");
	    /* empty_house() */
	    break;
	  }
      else
	{
	  msg_print("Hopefully, this time you're back in the present!");
	  town_special = TOWN_MESSAGE; /* optional with TOWN_WEIRD */
	}
    }
  else if ((town_special & ~TOWN_MESSAGE) == TOWN_DEAD_PAST)
    {
      if (forward)
	{
	  msg_print("Hopefully, this time you're back in the present!");
	  town_special = TOWN_MESSAGE; /* optional with TOWN_WEIRD */
	}
      else
	switch (rand_int(6))
	  {
	  case 1: 
	    msg_print("You arrive in an epoch without plants.");
	    msg_print("You never liked vegetables, but now, with no oxygen ");
	    msg_print("in the atmosphere, you start to appreciate them.");
	    p_ptr->chp = 0;
	    take_hit(1,"asphyxiation");
	    break;
	  case 2:
	    msg_print("You arrive in a prehistoric epoch.");
	    msg_print("You accidently step on a caterpillar...");
	    msg_print("The entire course of human history is changed and,");
	    msg_print("paradoxically, you are never born.");
	    p_ptr->chp = 0;
	    take_hit(1,"a bizarre paradox");
	    break;
	  default:
	    msg_print("You arrive even further in the past.");
	    msg_print("You feel vaguely relieved.");
	    /* empty_house() */
	    break;
	  }
    }
  else if (!(town_special & ~TOWN_MESSAGE))
    {
      town_special = TOWN_MESSAGE | (forward ?  TOWN_DEAD_FUTURE : TOWN_DEAD_PAST);
      msg_print("You feel lonely for a moment.");
    }
}

void do_balefire_effects(int effect)
{
  if (effect & BALE_TIME_TRAVEL)
    {
      time_travel(rand_int(2));
      if (!p_ptr->depth) effect |= BALE_ALTER;
    }
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
      int count = 0,n=0;
      msg_print("Time vortices spin into existence all around you!");
      msg_print(NULL);
      while ((count < 10) && (n < 1000))
	{
	  count += summon_specific(p_ptr->py,p_ptr->px,100,SUMMON_TIME);
	  n++;
	}
    }
  if (effect & BALE_HOLE)
    {
      int x,y,count=0;
      msg_print("The balefire has left a time/space anomaly!");
      msg_print("It seems to be expanding...");
      msg_print(NULL);
      do
	{
	  x = p_ptr->px + rand_int(9) - 4;
	  y = p_ptr->py + rand_int(9) - 4;
	  scatter(&y,&x,y,x,10,0);
	  count++;
	}
      while ((count < 1000) && (distance(x,y,p_ptr->px,p_ptr->py) < 3)); /* mercy */
      /* summon_specific(p_ptr->py,p_ptr->px,p_ptr->depth,SUMMON_HOLE); */
    }
  if (effect & (BALE_CLONE | BALE_KILL | BALE_HEAL))
      ; /* these effects aren't handled here */
}

/* this may need a "#ifdef DJGPP" */
#define print_dur(dur) \
do {if (p_ptr->dur) msg_format(#dur " %d",p_ptr->dur);} while (0)

#define print_dur2(dur) \
do {if (pa_ptr->dur) msg_format(#dur " %d",pa_ptr->dur);} while (0)

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

void show_aura()
{}
