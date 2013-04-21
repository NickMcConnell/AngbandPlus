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

	int spell = -1;
	int num = 0;

	byte spells[64];

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

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

	cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

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


	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK)
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
 * Brand some ammunition.  Used by Cubragol and a mage spell.  The spell was
 * moved here from cmd6.c where it used to be for Cubragol only.  I've also
 * expanded it to do either frost, fire or venom, at random. -GJW
 */
void brand_ammo (int brand_type, int bolts_only)
{
	int a;
	int allowable;

	if (bolts_only)
		allowable = TV_BOLT;
	else
		allowable = TV_BOLT | TV_ARROW | TV_SHOT;

	for (a = 0; a < INVEN_PACK; a++)
	{
		object_type *o_ptr = &inventory[a];

		if ((bolts_only) && (o_ptr->tval != TV_BOLT)) continue;
		if ((!bolts_only) && (o_ptr->tval != TV_BOLT) &&
		    (o_ptr->tval != TV_ARROW) && (o_ptr->tval != TV_SHOT))
		    	continue;
		if ((!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
		   (!cursed_p(o_ptr) && !broken_p(o_ptr)))
		   	break;
	}

	/* Enchant the ammo (or fail) */
	if ((a < INVEN_PACK) && (rand_int(100) < 50))
	{
		object_type *o_ptr = &inventory[a];
		char *ammo_name, *aura_name, msg[48];
		int aura_type, r;

		if (brand_type == 1) r = 0;		/* fire only */
		else if (brand_type == 2) r = 99;	/* poison only */
		else r = rand_int (100);

		if (r < 33)
		{
			aura_name = "fiery";
			aura_type = EGO_FLAME;
		}
		else if (r < 67)
		{
			aura_name = "frosty";
			aura_type = EGO_FROST;
		}
		else
		{
			aura_name = "sickly";
			aura_type = EGO_VENOM;
		}

		if (o_ptr->tval == TV_BOLT)
			ammo_name = (o_ptr->number > 1 ? "bolts" : "bolt");
		else if (o_ptr->tval == TV_ARROW)
			ammo_name = (o_ptr->number > 1 ? "arrows" : "arrow");
		else
			ammo_name = (o_ptr->number > 1 ? "shots" : "shot");

		sprintf (msg, "A %s aura surrounds your %s!",
			aura_name, ammo_name);
		msg_print (msg);
		o_ptr->name2 = aura_type;
		enchant (o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}
	else
	{
		if (flush_failure) flush();
		msg_print ("The enchantment failed.");
	}
}

/*
 * Brand the current weapon
 */
static void bless_weapon(void)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr)))
	{
		char o_name[80];

		o_ptr->name2 = EGO_BLESS_BLADE;

		/* Hack: (2*plev)% chance of getting WIS bonus. */
		if (rand_int(100) < 2 * p_ptr->lev)
		{
			/* Do we want a half bell curve instead? XXX */
			o_ptr->pval = randint(4);
		}

		/* Hack: plev% chance of getting bonus power. */
		if (rand_int(100) < p_ptr->lev)
		{
			o_ptr->xtra1 = OBJECT_XTRA_TYPE_POWER;
			o_ptr->xtra2 = rand_int(OBJECT_XTRA_SIZE_POWER);
		}
		object_desc(o_name, o_ptr, FALSE, 0);
		msg_format("Your %s has been consecrated!", o_name);
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();
		msg_print("The blessing failed.");
	}
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
		beam = ((p_ptr->pclass == CLASS_MAGE) ? plev : (plev / 2));

		/* Spells.  */
		switch (spell)
		{
			case 0: /* magic missile */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
				break;
			}

			case 1: /* detect monsters */
			{
				(void)detect_monsters_normal();
				break;
			}

			case 2: /* phase door */
			{
				teleport_player(10);
				break;
			}

			case 3: /* light area */
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 4: /* find hidden traps/doors */
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 5: /* cure light wounds */
			{
				(void)hp_player(damroll(2, 8));
				(void)set_cut(p_ptr->cut - 15);
				break;
			}

			case 6: /* detect treasure */
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case 7: /* detect objects */
			{
				(void)detect_objects_normal();
				break;
			}

			case 8: /* identify */
			{
				(void)ident_spell();
				break;
			}

			case 9: /* detect invisible */
			{
				(void)detect_monsters_invis();
				break;
			}

			case 10: /* detect enchantment */
			{
				(void)detect_objects_magic();
				break;
			}

			case 11: /* stinking cloud */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + (plev / 2), 2);
				break;
			}

			case 12: /* lightning bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_ELEC, dir,
					damroll(3+((plev-5)/6), 6));
				break;
			}

			case 13: /* confuse monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				break;
			}

			case 14: /* sleep monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			}

			case 15: /* wonder */
			{
			/* This spell should become more useful (more
			   controlled) as the player gains experience levels.
			   Thus, add 1/5 of the player's level to the die roll.
			   This eliminates the worst effects later on, while
			   keeping the results quite random.  It also allows
			   some potent effects only at high level. */

				int die = randint(100) + plev / 5;

				if (!get_aim_dir(&dir)) return;
				if (die > 100)
					msg_print ("You feel a surge of power!");
				if (die < 8) clone_monster (dir);
				else if (die < 14) speed_monster (dir);
				else if (die < 26) heal_monster (dir);
				else if (die < 31) poly_monster (dir);
				else if (die < 36)
					fire_bolt_or_beam (beam - 10,
					GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
				else if (die < 41) confuse_monster (dir, plev);
				else if (die < 46) fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
				else if (die < 51) lite_line (dir);
				else if (die < 56)
					fire_beam (GF_ELEC, dir,
					damroll(3+((plev-5)/6), 6));
				else if (die < 61)
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				else if (die < 66)
					fire_bolt_or_beam (beam, GF_ACID, dir,
					damroll(6+((plev-5)/4),8));
				else if (die < 71)
					fire_bolt_or_beam (beam, GF_FIRE, dir,
					damroll(8+((plev-5)/4),8));
				else if (die < 76) drain_life (dir, 75);
				else if (die < 81) fire_ball (GF_ELEC, dir, 30 + plev / 2, 2);
				else if (die < 86) fire_ball (GF_ACID, dir, 40 + plev, 2);
				else if (die < 91) fire_ball (GF_ICE, dir, 70 + plev, 3);
				else if (die < 96) fire_ball (GF_FIRE, dir, 80 + plev, 3);
				else if (die < 101) drain_life (dir, 100 + plev);
				else if (die < 104) earthquake (py, px, 12);
				else if (die < 106) destroy_area (py, px, 15, TRUE);
				else if (die < 108) genocide();
				else if (die < 110) dispel_monsters (120);
				else /* RARE */
				{
					dispel_monsters (150);
					slow_monsters();
					sleep_monsters();
					hp_player (300);
				}
				break;
			}

			case 16: /* frost bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				break;
			}

			case 17: /* acid bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case 18: /* fire bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case 19: /* trap/door destruction */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 20: /* spear of light */
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
				break;
			}

			case 21: /* turn stone to mud */
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 22: /* door creation */
			{
				(void)door_creation();
				break;
			}

			case 23: /* earthquake */
			{
				earthquake(py, px, 10);
				break;
			}

			case 24: /* stair creation */
			{
				(void)stair_creation();
				break;
			}

			case 25: /* cure poison */
			{
				(void)set_poisoned(0);
				break;
			}

			case 26: /* satisfy hunger */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 27: /* heroism */
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 28: /* berserker */
			{
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 29: /* haste self */
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

			case 30: /* teleport self */
			{
				teleport_player(plev * 5);
				break;
			}

			case 31: /* slow monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case 32: /* teleport other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 33: /* teleport level */
			{
				(void)teleport_player_level();
				break;
			}

			case 34: /* word of recall */
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

			case 35: /* polymorph other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}

			case 36: /* shock wave */
				if (!get_aim_dir(&dir)) return;
		    		fire_ball(GF_SOUND, dir, 10 + plev, 2);
				break;

			case 37: /* explosion */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SHARD, dir, 20 + (plev * 2), 2);
				break;

			case 38: /* cloudkill */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          40 + (plev / 2), 3);
				break;
			}

			case 39: /* mass sleep */
			{
				(void)sleep_monsters();
				break;
			}

			case 40: /* bedlam */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball (GF_OLD_CONF, dir, plev, 4);
				break;
			}

			case 41: /* rend soul */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam (beam / 4, GF_NETHER, dir,
					damroll (11, plev));
				break;
			}

			case 42: /* word of destruction */
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 43: /* chaos strike */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam (beam, GF_CHAOS, dir,
						damroll (13, plev));
				break;
			}

			case 44: /* resist cold */
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case 45: /* resist fire */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				break;
			}

			case 46: /* resist poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 47: /* resistance */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 48: /* shield */
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

			case 49: /* rune of protection */
			{
				(void)warding_glyph();
				break;
			}

			case 50: /* lesser recharging */
			{
				(void)recharge (2 + plev / 5);
				break;
			}

			case 51: /* enchant armor */
			{
				(void)enchant_spell (0, 0, 
					rand_int(3) + plev/20);
				break;
			}

			case 52: /* enchant weapon */
			{
				(void)enchant_spell(rand_int(4) + plev/20,
					rand_int(4) + plev/20, 0);
				break;
			}

			case 53: /* greater recharging */
			{
				recharge(50 + plev);
				break;
			}

			case 54: /* elemental brand */
			{
				brand_ammo (p_ptr->pclass==CLASS_ROGUE? 2 : 0,
					0);
				break;
			}

			case 55: /* frost ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          30 + (plev), 2);
				break;
			}

			case 56: /* acid ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          40 + (plev), 2);
				break;
			}

			case 57: /* fire ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          55 + (plev), 2);
				break;
			}

			case 58: /* ice storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ICE, dir,
				          50 + 2 * plev, 3);
				break;
			}

			case 59: /* genocide */
			{
				(void)genocide();
				break;
			}

			case 60: /* meteor swarm */
			{
				if (!get_aim_dir(&dir)) return;

				/* Fire 3 or more meteors. */
				fire_swarm(2 + plev / 20, GF_METEOR, dir,
					30 + plev / 2, 1);
				break;
			}

			case 61: /* mass genocide */
			{
				(void)mass_genocide();
				break;
			}

			case 62: /* rift */
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam (GF_GRAVITY, dir,
						40 + damroll (plev,7));
				break;
			}

			case 63: /* mana storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          300 + (plev * 2), 3);
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
			case 0: /* cure light */
			{
				(void)hp_player
					(damroll (2 + plev/10, 8 + plev/4));
				(void)set_cut(p_ptr->cut - 5 - plev/5);
				break;
			}

			case 1: /* remove fear */
			{
				(void)set_afraid(0);
				break;
			}

			case 2: /* neutralize poison */
			{
				(void)set_poisoned(0);
				break;
			}

			case 3: /* cure mortal */
			{
				(void)hp_player
					(damroll(8 + plev/6, 10 + plev/3));
				(void)set_cut(0);
				break;
			}

			case 4: /* regeneration */
			{
				(void)set_regen(p_ptr->regen + 5 +
						randint(20 + plev * 2));
				break;
			}

			case 5: /* healing */
			{
				(void)hp_player(500 + plev * 10);
				(void)set_cut(0);
				break;
			}

			case 6: /* purify body */
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)set_poisoned(0);
				(void)set_cut(0);
				break;
			}

			case 7: /* meditation */
			{
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_CHR);
				(void)set_afraid(0);
				(void)set_stun(0);
				break;
			}

			case 8: /* restoration */
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 9: /* remembrance */
			{
				(void)restore_level();
				break;
			}

			case 10: /* detect evil */
			{
				(void)detect_monsters_evil();
				break;
			}

			case 11: /* call light */
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 12: /* detect traps */
			{
				(void)detect_traps();
				break;
			}

			case 13: /* detect doors/stairs */
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 14: /* sense invisible */
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + plev);
				break;
			}

			case 15: /* detection */
			{
				(void)detect_all();
				break;
			}

			case 16: /* perception */
			{
				(void)ident_spell();
				break;
			}

			case 17: /* probing */
			{
				(void)probing();
				break;
			}

			case 18: /* clairvoyance */
			{
				wiz_lite();
				break;
			}

			case 19: /* bless */
			{
				(void)set_blessed(p_ptr->blessed +
						  randint(20) + plev * 2);
				break;
			}

			case 20: /* orb of draining */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          (damroll(3, 6) + plev +
				           (plev / ((p_ptr->pclass == CLASS_PRIEST) ? 2 : 4))),
				          ((plev < 30) ? 2 : 3));
				break;
			}

			case 21: /* radiant aura */
			{
				(void)set_radiant(p_ptr->radiant + plev * 3);
				break;
			}

			case 22: /* exorcise */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_EXORCISE, dir, plev, 0);
				break;
			}

			case 23: /* earthquake */
			{
				earthquake(py, px, 6 + randint(4) + plev / 10);
				break;
			}

			case 24: /* dispel undead */
			{
				(void)dispel_undead(damroll (2, plev * 3));
				break;
			}

			case 25: /* dispel demons */
			{
				(void)dispel_demons(damroll (2, plev * 3));
				break;
			}

			case 26: /* dispel evil */
			{
				(void)dispel_evil(damroll (2, plev * 3));
				break;
			}

			case 27: /* bless weapon */
			{
				bless_weapon();
				break;
			}

			case 28: /* holy word */
			{
				(void)dispel_evil(damroll(2, plev * 3));
				(void)hp_player(1000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 29: /* annihilation */
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 200);
				break;
			}

			case 30: /* word of destruction */
			{
				destroy_area(py, px,
					7 + randint(5) + plev / 5, TRUE);
				break;
			}

			case 31: /* portal */
			{
				teleport_player(20 + plev);
				break;
			}

			case 32: /* teleport self */
			{
				teleport_player(plev * 8);
				break;
			}

			case 33: /* teleport level */
			{
				(void)teleport_player_level();
				break;
			}

			case 34: /* blink */
			{
				teleport_player(10);
				break;
			}

			case 35: /* word of recall */
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

			case 36: /* alter reality */
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}

			case 37: /* sense surroundings */
			{
				map_area();
				break;
			}

			case 38: /* satisfy hunger */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 39: /* remove curse */
			{
				remove_curse();
				break;
			}

			case 40: /* unbarring ways */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 41: /* recharging */
			{
				(void)recharge(10 + plev/5);
				break;
			}

			case 42: /* dispel curse */
			{
				(void)remove_all_curse();
				break;
			}

			case 43: /* turn undead */
			{
				(void)turn_undead();
				break;
			}

			case 44: /* scare monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				break;
			}

			case 45: /* sanctuary */
			{
				if (plev < 30)
				{
					(void)sleep_monsters_touch();
				}
				else
				{
					(void)sleep_monsters();
				}
				break;
			}

			case 46: /* teleport other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 47: /* banishment */
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			}

			case 48: /* resist heat and cold */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

			case 49: /* protection from evil */
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			}

			case 50: /* resist poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois +
					randint(20) + plev);
				break;
			}

			case 51: /* holy sight */
			{
				(void)set_oppose_blind(p_ptr->oppose_blind +
					randint(20) + plev);
				break;
			}

			case 52: /* clear consciousness */
			{
				(void)set_oppose_conf(p_ptr->oppose_conf +
					randint(20) + plev);
				break;
			}

			case 53: /* resist elements */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid +
					randint(20) + plev);
				(void)set_oppose_elec(p_ptr->oppose_elec +
					randint(20) + plev);
				(void)set_oppose_fire(p_ptr->oppose_fire +
					randint(20) + plev);
				(void)set_oppose_cold(p_ptr->oppose_cold +
					randint(20) + plev);
				break;
			}

			case 54: /* sacred shield */
			{
				(void)set_shield(p_ptr->shield +
					randint(10) + plev / 2);
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

