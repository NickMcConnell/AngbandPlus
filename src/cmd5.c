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

	byte spells[PY_MAX_SPELLS];

	bool verify;

	bool flag, redraw, okay;
	char choice;

	const magic_type *s_ptr;

	char out_val[160];

	int priest = ((mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_PRAYER_BOOK || mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_DEATH_BOOK) ? 1 : 0);

	cptr p = ((priest) ? "prayer" : "spell");

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
	for (spell = 0; spell < PY_MAX_SPELLS; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][1] & (1L << (spell - 32))))
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
			s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, spell_names[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][spell],
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

	byte spells[PY_MAX_SPELLS];

	object_type *o_ptr;

	cptr q, s;


	/* Warriors are illiterate */
	if (!mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book)
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < PY_MAX_SPELLS; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
		    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][1] & (1L << (spell - 32))))
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

	cptr q, s;

	object_type *o_ptr;

	int priest = ((mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_PRAYER_BOOK || mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book == TV_DEATH_BOOK) ? 1 : 0);

	cptr p = ((priest) ? "prayer" : "spell");

	if (!mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book)
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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

	/* Get the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Mage -- Learn a selected spell */
	if (!priest)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE) && (spell == -1)) return;
	}

	/* Priest -- Learn a random prayer */
	if (priest)
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < PY_MAX_SPELLS; spell++)
		{
			/* Check spells in the book */
			if ((spell < 32) ?
			    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][0] & (1L << spell)) :
			    (spell_flags[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][sval][1] & (1L << (spell - 32))))
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
	  p_ptr->spell_learned1[(priest)] |= (1L << spell);
	}
	else
	{
	  p_ptr->spell_learned2[(priest)] |= (1L << (spell - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
	  /* Stop at the first empty space */
	  if (p_ptr->spell_order[(priest)][i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[(priest)][i] = spell;

	/* Mention the result */
	message_format(MSG_STUDY, 0, "You have learned the %s of %s.",
	           p, spell_names[mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_type][spell]);

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

	int plev = p_ptr->lev[p_ptr->current_class];

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if (mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book != TV_MAGIC_BOOK)
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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
	s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

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
		beam = ((p_ptr->pclass[p_ptr->current_class] == CLASS_MAGE) ? plev : (plev / 2));

		/* Spells.  */
		switch (spell)
		{
			case SPELL_MAGIC_MISSILE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
				effects[EFFECT_MAGIC_MISSILE] += 1;
				break;
			}

			case SPELL_DETECT_MONSTERS:
			{
				(void)detect_monsters_normal();
				effects[EFFECT_DETECT_MONSTERS]++;
				break;
			}

			case SPELL_PHASE_DOOR:
			{
				teleport_player(10);
				effects[EFFECT_BLINK]++;
				break;
			}

			case SPELL_LIGHT_AREA:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				effects[EFFECT_LIGHT_AREA]++;
				break;
			}

			case SPELL_TREASURE_DETECTION:
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
				effects[EFFECT_DETECT_TREASURE]++;
				break;
			}

			case SPELL_CURE_LIGHT_WOUNDS:
			{
				(void)hp_player(damroll(2, 8));
				(void)set_cut(p_ptr->cut - 15);
				effects[EFFECT_CURE_LIGHT]++;
				break;
			}

			case SPELL_OBJECT_DETECTION:
			{
				(void)detect_objects_normal();
				effects[EFFECT_DETECT_OBJECTS]++;
				break;
			}

			case SPELL_FIND_TRAPS_DOORS:
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				effects[EFFECT_DETECT_TRAP]++;
				effects[EFFECT_DETECT_DOOR]++;
				break;
			}

			case SPELL_STINKING_CLOUD:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + (plev / 2), 2);
				effects[EFFECT_STINKING_CLOUD]++;
				break;
			}

			case SPELL_CONFUSE_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				effects[EFFECT_CONFUSE_MONSTER]++;
				break;
			}

			case SPELL_LIGHTNING_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				effects[EFFECT_LIGHTNING_BOLT]++;
				break;
			}

			case SPELL_TRAP_DOOR_DESTRUCTION:
			{
				(void)destroy_doors_touch();
				effects[EFFECT_DOOR_DESTRUCT_TOUCH]++;
				break;
			}

			case SPELL_SLEEP_I:
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir, p_ptr->current_class);
				effects[EFFECT_SLEEP_MONSTER]++;
				break;
			}

			case SPELL_CURE_POISON:
			{
				(void)set_poisoned(0);
				effects[EFFECT_CURE_POISON]++;
				break;
			}

			case SPELL_TELEPORT_SELF:
			{
				teleport_player(plev * 5);
				effects[EFFECT_TELEPORT]++;
				break;
			}

			case SPELL_SPEAR_OF_LIGHT:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
				effects[EFFECT_SPEAR_LIGHT]++;
				break;
			}

			case SPELL_FROST_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				effects[EFFECT_COLD_BOLT]++;
				break;
			}

			case SPELL_TURN_STONE_TO_MUD:
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				effects[EFFECT_STONE_TO_MUD]++;
				break;
			}

			case SPELL_SATISFY_HUNGER:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				effects[EFFECT_SATISFY_HUNGER]++;
				break;
			}

			case SPELL_RECHARGE_ITEM_I:
			{
				(void)recharge(5);
				effects[EFFECT_RECHARGE_SMALL]++;
				break;
			}

			case SPELL_SLEEP_II:
			{
				(void)sleep_monsters_touch(p_ptr->current_class);
				effects[EFFECT_SLEEP_TOUCH]++;
				break;
			}

			case SPELL_POLYMORPH_OTHER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir, p_ptr->current_class);
				effects[EFFECT_POLY_OTHER]++;
				break;
			}

			case SPELL_IDENTIFY:
			{
				(void)ident_spell();
				effects[EFFECT_IDENTIFY]++;
				break;
			}

			case SPELL_SLEEP_III:
			{
				(void)sleep_monsters(p_ptr->current_class);
				effects[EFFECT_SLEEP_ALL]++;
				break;
			}

			case SPELL_FIRE_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				effects[EFFECT_FIRE_BOLT]++;
				break;
			}

			case SPELL_SLOW_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir, p_ptr->current_class);
				effects[EFFECT_SLOW_MONSTER]++;
				break;
			}

			case SPELL_FROST_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          30 + (plev), 2);
				effects[EFFECT_COLD_BALL]++;
				break;
			}

			case SPELL_RECHARGE_ITEM_II:
			{
				(void)recharge(40);
				effects[EFFECT_RECHARGE_MEDIUM]++;
				break;
			}

			case SPELL_TELEPORT_OTHER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				effects[EFFECT_TELEPORT_OTHER]++;
				break;
			}

			case SPELL_HASTE_SELF:
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + plev);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(5));
				}
				effects[EFFECT_HASTE]++;
				break;
			}

			case SPELL_FIRE_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          55 + (plev), 2);
				effects[EFFECT_FIRE_BALL]++;
				break;
			}

			case SPELL_WORD_OF_DESTRUCTION:
			{
				destroy_area(py, px, 15, TRUE);
				effects[EFFECT_WORD_OF_DESTRUCT]++;
				break;
			}

			case SPELL_GENOCIDE:
			{
				(void)genocide();
				effects[EFFECT_GENOCIDE]++;
				break;
			}

			case SPELL_DOOR_CREATION:
			{
				(void)door_creation();
				effects[EFFECT_DOOR_CREATION]++;
				break;
			}

			case SPELL_STAIR_CREATION:
			{
				(void)stair_creation();
				effects[EFFECT_STAIR_CREATION]++;
				break;
			}

			case SPELL_TELEPORT_LEVEL:
			{
				(void)teleport_player_level();
				effects[EFFECT_TELEPORT_LEVEL]++;
				break;
			}

			case SPELL_EARTHQUAKE:
			{
				earthquake(py, px, 10);
				effects[EFFECT_EARTHQUAKE]++;
				break;
			}

			case SPELL_WORD_OF_RECALL:
			{
			     if (p_ptr->astral)
			     {
				  msg_print("You feel a terrible sense of loss.");
				  break;
			     }
			     set_recall();
			     effects[EFFECT_RECALL]++;
			     break;
			}

			case SPELL_ACID_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				effects[EFFECT_ACID_BOLT]++;
				break;
			}

			case SPELL_CLOUD_KILL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          20 + (plev / 2), 3);
				effects[EFFECT_CLOUD_KILL]++;
				break;
			}

			case SPELL_ACID_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          40 + (plev), 2);
				effects[EFFECT_ACID_BALL]++;
				break;
			}

			case SPELL_ICE_STORM:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          70 + (plev), 3);
				effects[EFFECT_ICE_STORM]++;
				break;
			}

			case SPELL_METEOR_SWARM:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
				          65 + (plev), 3);
				effects[EFFECT_METEOR_SWARM]++;
				break;
			}

			case SPELL_MANA_STORM:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          300 + (plev * 2), 3);
				effects[EFFECT_MANA_STORM]++;
				break;
			}

			case SPELL_DETECT_EVIL:
			{
				(void)detect_monsters_evil();
				effects[EFFECT_DETECT_EVIL]++;
				break;
			}

			case SPELL_DETECT_ENCHANTMENT:
			{
				(void)detect_objects_magic();
				effects[EFFECT_DETECT_MAGIC]++;
				break;
			}

			case SPELL_RECHARGE_ITEM_III:
			{
				recharge(100);
				effects[EFFECT_RECHARGE_LARGE]++;
				break;
			}

			case SPELL_GENOCIDE2:
			{
				(void)genocide();
				effects[EFFECT_GENOCIDE]++;
				break;
			}

			case SPELL_MASS_GENOCIDE:
			{
				(void)mass_genocide();
				effects[EFFECT_MASS_GENOCIDE]++;
				break;
			}

			case SPELL_RESIST_FIRE:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				effects[EFFECT_RES_FIRE]++;
				break;
			}

			case SPELL_RESIST_COLD:
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				effects[EFFECT_RES_COLD]++;
				break;
			}

			case SPELL_RESIST_ACID:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				effects[EFFECT_RES_ACID]++;
				break;
			}

			case SPELL_RESIST_POISON:
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				effects[EFFECT_RES_POIS]++;
				break;
			}

			case SPELL_RESISTANCE:
			{
				int time = randint(20) + 20;
				(void)set_oppose_acid(p_ptr->oppose_acid + time);
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				(void)set_oppose_fire(p_ptr->oppose_fire + time);
				(void)set_oppose_cold(p_ptr->oppose_cold + time);
				(void)set_oppose_pois(p_ptr->oppose_pois + time);
				effects[EFFECT_RES_ACID]++;
				effects[EFFECT_RES_ELEC]++;
				effects[EFFECT_RES_FIRE]++;
				effects[EFFECT_RES_COLD]++;
				effects[EFFECT_RES_POIS]++;
				break;
			}

			case SPELL_HEROISM:
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + 
					       randint(25) + 25);
				(void)set_afraid(0);
				effects[EFFECT_HEROISM]++;
				effects[EFFECT_REMOVE_FEAR]++;
				break;
			}

			case SPELL_SHIELD:
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				effects[EFFECT_SHIELD]++;
				break;
			}

			case SPELL_BERSERKER:
			{
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + 
						randint(25) + 25);
				(void)set_afraid(0);
				effects[EFFECT_BERSERK]++;
				effects[EFFECT_REMOVE_FEAR]++;
				break;
			}

			case SPELL_ESSENCE_OF_SPEED:
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(30) + 30 + plev);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(10));
				}
				effects[EFFECT_HASTE]++;
				break;
			}

			case SPELL_GLOBE_OF_INVULNERABILITY:
			{
				(void)set_invuln(p_ptr->invuln + randint(8) + 8);
				effects[EFFECT_GLOBE_OF_INVULN]++;
				break;
			}
		}

		/* A spell was cast */
		if (!((spell < 32) ?
		      (p_ptr->spell_worked1[0] & (1L << spell)) :
		      (p_ptr->spell_worked2[0] & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1[0] |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2[0] |= (1L << (spell - 32));
			}

			/* Gain experience */
			if (player_has_class(CLASS_MAGE, 0))
			  gain_exp(e * s_ptr->slevel, index_of_class(CLASS_MAGE));
			if (player_has_class(CLASS_ROGUE, 0))
			  gain_exp(e * s_ptr->slevel, index_of_class(CLASS_ROGUE));
			if (player_has_class(CLASS_RANGER, 0))
			  gain_exp(e * s_ptr->slevel, index_of_class(CLASS_RANGER));

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

	int plev = p_ptr->lev[p_ptr->current_class];

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Must use prayer books */
	if (mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book != TV_PRAYER_BOOK)
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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
	s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];


	/* Verify "dangerous" prayers */
	if (s_ptr->smana > p_ptr->cpp)
	{
		/* Warning */
		msg_print("You do not have enough piety to recite this prayer.");

		/* Flush input */
		flush();

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
			case PRAYER_DETECT_EVIL:
			{
				(void)detect_monsters_evil();
				effects[EFFECT_DETECT_EVIL]++;
				break;
			}

			case PRAYER_CURE_LIGHT_WOUNDS:
			{
				(void)hp_player(damroll(2, 10));
				(void)set_cut(p_ptr->cut - 10);
				effects[EFFECT_CURE_LIGHT]++;
				break;
			}

			case PRAYER_BLESS:
			{
				(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				effects[EFFECT_BLESS]++;
				break;
			}

			case PRAYER_REMOVE_FEAR:
			{
				(void)set_afraid(0);
				effects[EFFECT_REMOVE_FEAR]++;
				break;
			}

			case PRAYER_CALL_LIGHT:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				effects[EFFECT_LIGHT_AREA]++;
				break;
			}

			case PRAYER_FIND_TRAPS:
			{
				(void)detect_traps();
				effects[EFFECT_DETECT_TRAP]++;
				break;
			}

			case PRAYER_DETECT_DOORS_STAIRS:
			{
				(void)detect_doors();
				(void)detect_stairs();
				effects[EFFECT_DETECT_DOOR]++;
				break;
			}

			case PRAYER_SLOW_POISON:
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				effects[EFFECT_SLOW_POISON]++;
				break;
			}

			case PRAYER_SCARE_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				effects[EFFECT_FEAR_MONSTER]++;
				break;
			}

			case PRAYER_PORTAL:
			{
				teleport_player(plev * 3);
				effects[EFFECT_TELEPORT]++;
				break;
			}

			case PRAYER_CURE_SERIOUS_WOUNDS:
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut((p_ptr->cut / 2) - 20);
				effects[EFFECT_CURE_SERIOUS]++;
				break;
			}

			case PRAYER_CHANT:
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				effects[EFFECT_CHANT]++;
				break;
			}

			case PRAYER_SANCTUARY:
			{
				(void)sleep_monsters_touch(p_ptr->current_class);
				effects[EFFECT_SLEEP_TOUCH]++;
				break;
			}

			case PRAYER_SATISFY_HUNGER:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				effects[EFFECT_SATISFY_HUNGER]++;
				break;
			}

			case PRAYER_REMOVE_CURSE:
			{
				remove_curse();
				effects[EFFECT_REMOVE_CURSE]++;
				break;
			}

			case PRAYER_RESIST_HEAT_COLD:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				effects[EFFECT_RES_FIRE]++;
				effects[EFFECT_RES_COLD]++;
				break;
			}

			case PRAYER_NEUTRALIZE_POISON:
			{
				(void)set_poisoned(0);
				effects[EFFECT_CURE_POISON]++;
				break;
			}

			case PRAYER_ORB_OF_DRAINING:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          (damroll(3, 6) + plev +
				           (plev / ((p_ptr->pclass[p_ptr->current_class] == CLASS_PRIEST) ? 2 : 4))),
				          ((plev < 30) ? 2 : 3));
				effects[EFFECT_ORB_DRAINING]++;
				break;
			}

			case PRAYER_CURE_CRITICAL_WOUNDS:
			{
				(void)hp_player(damroll(6, 10));
				(void)set_cut(0);
				effects[EFFECT_CURE_CRITICAL]++;
				break;
			}

			case PRAYER_SENSE_INVISIBLE:
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				effects[EFFECT_SEE_INVIS]++;
				break;
			}

			case PRAYER_PROTECTION_FROM_EVIL:
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev[p_ptr->current_class]);
				effects[EFFECT_PRO_EVIL]++;
				break;
			}

			case PRAYER_EARTHQUAKE:
			{
				earthquake(py, px, 10);
				effects[EFFECT_EARTHQUAKE]++;
				break;
			}

			case PRAYER_SENSE_SURROUNDINGS:
			{
				map_area();
				effects[EFFECT_MAPPING]++;
				break;
			}

			case PRAYER_CURE_MORTAL_WOUNDS:
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				effects[EFFECT_CURE_MORTAL]++;
				break;
			}

			case PRAYER_TURN_UNDEAD:
			{
				(void)turn_undead(p_ptr->current_class);
				effects[EFFECT_TURN_UNDEAD]++;
				break;
			}

			case PRAYER_PRAYER:
			{
				(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				effects[EFFECT_PRAYER]++;
				break;
			}

			case PRAYER_DISPEL_UNDEAD:
			{
				(void)dispel_undead(randint(plev * 3));
				effects[EFFECT_DISPEL_UNDEAD]++;
				break;
			}

			case PRAYER_HEAL:
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				effects[EFFECT_HEAL]++;
				break;
			}

			case PRAYER_DISPEL_EVIL:
			{
				(void)dispel_evil(randint(plev * 3));
				effects[EFFECT_DISPEL_EVIL]++;
				break;
			}

			case PRAYER_GLYPH_OF_WARDING:
			{
				warding_glyph();
				effects[EFFECT_GLYPH_WARDING]++;
				break;
			}

			case PRAYER_HOLY_WORD:
			{
				(void)dispel_evil(randint(plev * 4));
				(void)hp_player(1000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				effects[EFFECT_DISPEL_EVIL]++;
				effects[EFFECT_HEAL]++;
				effects[EFFECT_REMOVE_FEAR]++;
				effects[EFFECT_CURE_POISON]++;
				break;
			}

			case PRAYER_DETECT_MONSTERS:
			{
				(void)detect_monsters_normal();
				effects[EFFECT_DETECT_MONSTERS]++;
				break;
			}

			case PRAYER_DETECTION:
			{
				(void)detect_all();
				effects[EFFECT_DETECT_ALL]++;
				break;
			}

			case PRAYER_PERCEPTION:
			{
				(void)ident_spell();
				effects[EFFECT_IDENTIFY]++;
				break;
			}

			case PRAYER_PROBING:
			{
				(void)probing();
				effects[EFFECT_PROBING]++;
				break;
			}

			case PRAYER_CLAIRVOYANCE:
			{
				wiz_lite();
				effects[EFFECT_WIZ_LITE]++;
				break;
			}

			case PRAYER_CURE_SERIOUS_WOUNDS2:
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut(0);
				effects[EFFECT_CURE_SERIOUS]++;
				break;
			}

			case PRAYER_CURE_MORTAL_WOUNDS2:
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				effects[EFFECT_CURE_MORTAL]++;
				break;
			}

			case PRAYER_HEALING:
			{
				(void)hp_player(2000);
				(void)set_stun(0);
				(void)set_cut(0);
				effects[EFFECT_HEAL]++;
				break;
			}

			case PRAYER_RESTORATION:
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				effects[EFFECT_RESTORE_STR]++;
				effects[EFFECT_RESTORE_CON]++;
				effects[EFFECT_RESTORE_DEX]++;
				effects[EFFECT_RESTORE_INT]++;
				effects[EFFECT_RESTORE_WIS]++;
				effects[EFFECT_RESTORE_CHR]++;
				break;
			}

			case PRAYER_REMEMBRANCE:
			{
				(void)restore_level();
				effects[EFFECT_RESTORE_EXP]++;
				break;
			}

			case PRAYER_DISPEL_UNDEAD2:
			{
				(void)dispel_undead(randint(plev * 4));
				effects[EFFECT_DISPEL_UNDEAD]++;
				break;
			}

			case PRAYER_DISPEL_EVIL2:
			{
				(void)dispel_evil(randint(plev * 4));
				effects[EFFECT_DISPEL_EVIL]++;
				break;
			}

			case PRAYER_BANISHMENT:
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				effects[EFFECT_BANISH_EVIL]++;
				break;
			}

			case PRAYER_WORD_OF_DESTRUCTION:
			{
				destroy_area(py, px, 15, TRUE);
				effects[EFFECT_WORD_OF_DESTRUCT]++;
				break;
			}

			case PRAYER_ANNIHILATION:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 200);
				effects[EFFECT_DRAIN_LIFE]++;
				break;
			}

			case PRAYER_UNBARRING_WAYS:
			{
				(void)destroy_doors_touch();
				effects[EFFECT_DOOR_DESTRUCT_TOUCH]++;
				break;
			}

			case PRAYER_RECHARGING:
			{
				(void)recharge(15);
				effects[EFFECT_RECHARGE_SMALL]++;
				break;
			}

			case PRAYER_DISPEL_CURSE:
			{
				(void)remove_all_curse();
				effects[EFFECT_DISPEL_CURSE]++;
				break;
			}

			case PRAYER_ENCHANT_WEAPON:
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				effects[EFFECT_ENCHANT_WEAPON_HIT]++;
				effects[EFFECT_ENCHANT_WEAPON_DAM]++;
				break;
			}

			case PRAYER_ENCHANT_ARMOUR:
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				effects[EFFECT_ENCHANT_ARMOUR]++;
				break;
			}

			case PRAYER_ELEMENTAL_BRAND:
			{
				brand_weapon();
				effects[EFFECT_ELEMENTAL_BRAND]++;
				break;
			}

			case PRAYER_BLINK:
			{
				teleport_player(10);
				effects[EFFECT_BLINK]++;
				break;
			}

			case PRAYER_TELEPORT_SELF:
			{
				teleport_player(plev * 8);
				effects[EFFECT_TELEPORT]++;
				break;
			}

			case PRAYER_TELEPORT_OTHER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				effects[EFFECT_TELEPORT_OTHER]++;
				break;
			}

			case PRAYER_TELEPORT_LEVEL:
			{
				(void)teleport_player_level();
				effects[EFFECT_TELEPORT_LEVEL]++;
				break;
			}

			case PRAYER_WORD_OF_RECALL:
			{
			     if (p_ptr->astral)
			     {
				  msg_print("You feel a terrible sense of loss.");
				  break;
			     }
			     set_recall();
			     effects[EFFECT_RECALL]++;
			     break;
			}

			case PRAYER_ALTER_REALITY:
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				effects[EFFECT_ALTER_REALITY]++;
				break;
			}
		}

		/* A prayer was prayed */
		if (!((spell < 32) ?
		      (p_ptr->spell_worked1[1] & (1L << spell)) :
		      (p_ptr->spell_worked2[1] & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1[1] |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2[1] |= (1L << (spell - 32));
			}

			/* Gain experience */
			if (player_has_class(CLASS_PRIEST, 0))
			  gain_exp(e * s_ptr->slevel, index_of_class(CLASS_PRIEST));
			if (player_has_class(CLASS_PALADIN, 0))
			  gain_exp(e * s_ptr->slevel, index_of_class(CLASS_PALADIN));

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (s_ptr->smana <= p_ptr->cpp)
	{
		/* Use some mana */
		p_ptr->cpp -= s_ptr->smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = s_ptr->smana - p_ptr->cpp;

		/* No mana left */
		p_ptr->cpp = 0;
		p_ptr->cpp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage WIS (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You feel less wise!");

			/* Reduce wisdom */
			(void)dec_stat(A_WIS, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/*
* Brand some ammunition.  Used by an illusion spell. 
 */
void brand_ammo(int brand_type)
{
	int a;
	int allowable;

	allowable = TV_BOLT | TV_ARROW | TV_SHOT;

	for (a = 0; a < INVEN_PACK; a++)
	{
		object_type *o_ptr = &inventory[a];

		if ((o_ptr->tval != TV_BOLT) &&
		    (o_ptr->tval != TV_ARROW) && 
		    (o_ptr->tval != TV_SHOT))
		  continue;
		if ((!artifact_p(o_ptr)) && 
		    (!ego_item_p(o_ptr)) &&
		    (!cursed_p(o_ptr)))
		  break;
	}

	/* Enchant the ammo (or fail) */
	if ((a < INVEN_PACK) && (rand_int(100) < 50))
	{
		object_type *o_ptr = &inventory[a];
		char *ammo_name, *aura_name, msg[48];
		int aura_type;

		switch (brand_type)
		{
		case 0:
		     /* Random enchantment */
		        switch (rand_int(5))
			{
			case 0:
			  aura_name = "fiery";
			  aura_type = EGO_BRAND_FIRE;
			  break;
			case 1:
			  aura_name = "frosty";
			  aura_type = EGO_BRAND_COLD;
			  break;
			case 2:
			  aura_name = "sparkling";
			  aura_type = EGO_BRAND_ELEC;
			  break;
			case 3:
			  aura_name = "acidic";
			  aura_type = EGO_BRAND_ACID;
			  break;
			case 4:
			  aura_name = "sickly green";
			  aura_type = EGO_BRAND_POIS;
			  break;
			}
		        break;
		case 1:
			aura_name = "fiery";
			aura_type = EGO_BRAND_FIRE;
			break;
		case 2:
			aura_name = "frosty";
			aura_type = EGO_BRAND_COLD;
			break;
		case 3:
			aura_name = "sparkling";
			aura_type = EGO_BRAND_ELEC;
			break;
		case 4:
			aura_name = "acidic";
			aura_type = EGO_BRAND_ACID;
			break;
		case 5:
			aura_name = "sickly green";
			aura_type = EGO_BRAND_POIS;
			break;
		}
		
		if (o_ptr->tval == TV_BOLT)
			ammo_name = "bolts";
		else if (o_ptr->tval == TV_ARROW)
			ammo_name = "arrows";
		else
			ammo_name = "shots";

		sprintf (msg, "Your %s are covered in a %s aura!",
			ammo_name, aura_name);
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
 * Cast an illusion spell
 */
void do_cmd_cast_illusion(void)
{
        int dir;
        int beam;
	int plev = p_ptr->lev[p_ptr->current_class];

	int item, sval, spell;
	int chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Hack -- chance of "beam" instead of "bolt" */
	beam = ((p_ptr->pclass[p_ptr->current_class] == CLASS_ILLUSIONIST) ? plev : (plev / 2));

	/* Require spell ability */
	if (mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book != TV_ILLUSION_BOOK)
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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
	s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

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
		/* Spells.  */
		switch (spell)
		{
			case 0:
			{
			  /* confusion bolt -KMW- */
			  if (!get_aim_dir(&dir)) return;
			  fire_bolt_or_beam(beam-10, GF_CONFUSION, dir,
                                            damroll(2 + ((plev - 1) / 5), 4));
			  effects[EFFECT_CONFUSION_BOLT]++;
			  break;
			}

			case 1: /* detect monsters */
			{
			  (void)detect_monsters_normal();	
			  effects[EFFECT_DETECT_MONSTERS]++;
			  break;
			}

			case 2: /* phase door */
			{
			  teleport_player(10);
			  effects[EFFECT_BLINK]++;
			  break;
			}

			case 3: /* light area */
			{
			  (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			  effects[EFFECT_LIGHT_AREA]++;
			  break;
			}

			case 4: /* treasure detection */
			{
			  (void)detect_treasure();
			  (void)detect_objects_gold();
			  effects[EFFECT_DETECT_TREASURE]++;
			  break;
			}

			case 5:
			{
			  if (!get_aim_dir(&dir)) return;
			  (void)fear_monster(dir, plev);
			  effects[EFFECT_FEAR_MONSTER]++;
			  break;
			}

			case 6: /* object detection */
			{
			  (void)detect_objects_normal();
			  effects[EFFECT_DETECT_OBJECTS]++;
			  break;
			}

			case 7: /* find hidden traps/doors */
			{
			  (void)detect_traps();
			  (void)detect_doors();
			  (void)detect_stairs();
			  effects[EFFECT_DETECT_DOOR]++;
			  effects[EFFECT_DETECT_TRAP]++;
			  break;
			}

			case 8: /* stinking cloud */
			{
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_POIS, dir,
				    10 + (plev / 2), 2);
			  effects[EFFECT_STINKING_CLOUD]++;
			  break;
			}

			case 9:
			{
			  /* infravision */
			  if (p_ptr->tim_infra == 0)
			    set_tim_infra(p_ptr->tim_infra + 200 + randint(100));
			  effects[EFFECT_INFRA]++;
			  break;
			}

			case 10:
			{
			  if (!get_aim_dir(&dir)) return;
			  (void)sleep_monster(dir, p_ptr->current_class);
			  effects[EFFECT_SLEEP_MONSTER]++;
			  break;
			}

			case 11: /* trap/door destruction */
			{
			  (void)destroy_doors_touch();
			  effects[EFFECT_DOOR_DESTRUCT_TOUCH]++;
			  break;
			}

			case 12:
			{
			  /* fog cloud -KMW- */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_POIS, dir,
				    10 + (plev / 2), 3);
			  effects[EFFECT_FOG_CLOUD]++;
			  break;
			}

			case 13: /* cure poison */
			{
			  (void)set_poisoned(0);
			  effects[EFFECT_CURE_POISON]++;
			  break;
			}

			case 14:
			{
			  /* satisfy hunger */
			  (void)set_food(PY_FOOD_MAX - 1);
			  effects[EFFECT_SATISFY_HUNGER]++;
			  break;
			}

			case 15:
			{
			  /* shadow door */
			  (void)door_creation();
			  effects[EFFECT_DOOR_CREATION]++;
			  break;
			}

			case 16:
			{
			  /* shadow monster */
			  if (!get_aim_dir(&dir)) return;
			  fire_bolt(GF_FORCE, dir,
				    damroll(5+((plev-6)/4), 8));
			  effects[EFFECT_SHADOW_MONSTER]++;
			  break;
			}

			case 17: /* turn stone to mud */
			{
			  if (!get_aim_dir(&dir)) return;
			  (void)wall_to_mud(dir);
			  effects[EFFECT_STONE_TO_MUD]++;
			  break;
			}

			case 18:
			{
			  /* detect invisible */
			  (void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
			  effects[EFFECT_SEE_INVIS]++;
			  break;
			}

			case 19: /* recharge item */
			{
			  (void)recharge((plev * 2));
			  effects[EFFECT_RECHARGE_MEDIUM]++;
			  break;
			}

			case 20: /* brand ammo */
			{
			  /* Random enchantment */
			  (void)brand_ammo(0);
			  effects[EFFECT_ELEMENTAL_BRAND_AMMO]++;
			  break;
			}

			case 21:
			{
			  /* spear of light */
			  if (!get_aim_dir(&dir)) return;
			  msg_print("A line of blue shimmering light appears.");
			  fire_beam(GF_LITE, dir,
				    damroll(2+((plev-5)/4), 6));
			  lite_line(dir);
			  effects[EFFECT_SPEAR_LIGHT]++;
			  break;
			}

			case 22:
			{
			  /* chaos */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_CHAOS, dir,
				    25 + plev, 2);	
			  effects[EFFECT_CHAOS_BALL]++;
			  break;
			}

			case 23:
			{
			  /* mental barrier */
			  set_mental_barrier(p_ptr->mental_barrier + 100);
			  msg_print("Your wisdom and intelligence cannot be changed!");
			  effects[EFFECT_MENTAL_BARRIER]++;
			  break;
			}

			case 24:
			{
			  /* true sight */
			  map_area();
			  effects[EFFECT_MAPPING]++;
			  break;
			}

			case 25: /* slow monster */
			{
			  if (!get_aim_dir(&dir)) return;
			  (void)slow_monster(dir, p_ptr->current_class);
			  effects[EFFECT_SLOW_MONSTER]++;
			  break;
			}

			case 26:
			{
			  /* shadow ball */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_DARK, dir, 35 + (plev), 2);
			  effects[EFFECT_SHADOW_BALL]++;
			}

			case 27:
			{
			  /* bolt of darkness */
			  if (!get_aim_dir(&dir)) return;
			  fire_bolt_or_beam(beam, GF_DARK, dir,
                                            damroll(8+((plev-5)/4), 8));
			  effects[EFFECT_DARK_BOLT]++;
			  break;
			}

			case 28:
			{
			  /* shadow form */
			  /* (void)set_shadow(p_ptr->wraith_form + plev + randint(24)); */
			  msg_print("This spell not currently available");
			  break;
			}

			case 29: /* haste self */
			{
			  if (!p_ptr->fast)
			    (void)set_fast(randint(20) + plev);
			  else
			    (void)set_fast(p_ptr->fast + randint(5));
			  effects[EFFECT_HASTE]++;
			  break;
			}

			case 30:
			{
			  /* prismatic wall */
			  warding_glyph();
			  effects[EFFECT_GLYPH_WARDING]++;
			  break;
			}

			case 31:
			{
			  /* prismatic spray */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_LITE, 5, 40 + (plev), 2);
			  fire_beam(GF_LITE, dir,
				    damroll(8+((plev-5)/4), 8));
			  effects[EFFECT_PRISMATIC_SPRAY]++;
			  break;
			}

			case 32:
			{
			  /* chromatic shield */
			  (void)set_shield(p_ptr->shield + randint(30) + 30);
			  effects[EFFECT_SHIELD]++;
			  break;
			}

			case 33:
			{
			  /* wizard lock */
			  /* do_cmd_spike(); */
			  /* effects[EFFECT_LOCK]++; */
			  msg_print("This spell not currently available");
			  break;
			}

			case 34:
			{
			  /* bedlam */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_CHAOS, dir,
				    50 + plev, 10);
			  effects[EFFECT_BEDLAM]++;
			  break;
			}

			case 35:
			{
			     /* word of recall */
			     if (p_ptr->astral)
			     {
				  msg_print("You feel a terrible sense of loss.");
				  break;
			     }
			     set_recall();
			     effects[EFFECT_RECALL]++;
			     break;
			}

			case 36:
			{
			  /* detect enchantment */
			  (void)detect_objects_magic();
			  effects[EFFECT_DETECT_MAGIC]++;
			  break;
			}

			case 37:
			{
			  /* probing */
			  (void)probing();
			  effects[EFFECT_PROBING]++;
			  break;
			}

			case 38:
			{
			  /* sunfire */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_LITE, dir,
				    50 + plev, 10);
			  effects[EFFECT_SUNFIRE]++;
			  break;
			}

			case 39: /* the bigbys that are duplicates - warding, slow etc. shoudl act all aroundplayer */
			{
			  /* Bigby's Interposing Hand */
			  warding_glyph();
			  effects[EFFECT_GLYPH_WARDING]++;
			  break;
			}

			case 40:
			{
			  /* Bigby's Phantom Hand */
			  if (!get_aim_dir(&dir)) return;
			  fire_bolt_or_beam(beam, GF_CONFUSION, dir,
                                            damroll(2+((plev-5)/4), 8));
			  effects[EFFECT_PHANTOM_HAND]++;
			  break;
			}

			case 41:
			{
			  /* Bigby's Forceful Hand */
			  if (!get_aim_dir(&dir)) return;
			  fire_bolt_or_beam(beam, GF_FORCE, dir,
                                            damroll(6+((plev-5)/4), 8));
			  effects[EFFECT_FORCEFUL_HAND]++;
			  break;
			}

			case 42:
			{
			  /* Bigby's Grasping Hand */
			  if (!get_aim_dir(&dir)) return;
			  (void)slow_monster(dir, p_ptr->current_class);
			  effects[EFFECT_SLOW_MONSTER]++;
			  break;
			}

			case 43:
			{
			  /* Bigby's Clenched Fist */
			  if (!get_aim_dir(&dir)) return;
			  fire_beam(GF_FORCE, dir,
				    damroll(10+((plev-5)/4), 8));
			  effects[EFFECT_CLENCHED_FIST]++;
			  break;
			}

			case 44:
			{
			  /* Bigby's Crushing Hand */
			  /* want to have this last two turns */
			  if (!get_aim_dir(&dir)) return;
			  fire_bolt(GF_GRAVITY, dir,
				    damroll(12+((plev-5)/4), 8));
			  effects[EFFECT_CRUSHING_HAND]++;
			  break;
			}

			case 45:
			{
			  /* force blast */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_FORCE, 5,
				    300 + (plev * 2), 3);
			  effects[EFFECT_FORCE_BLAST]++;
			  break;
			}

			case 46:
			{
			  /* Sphere of Light */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_LITE, dir,
				    30 + (plev), 2);
			  effects[EFFECT_LIGHT_SPHERE]++;
			  break;
			}

			case 47:
			{
			  /* sphere of darkness */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_DARK, dir,
				    35 + (plev), 2);
			  effects[EFFECT_DARK_SPHERE]++;
			  break;
			}

			case 48:
			{
			  /* sphere of confusion */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_CONFUSION, dir,
				    40 + (plev), 2);
			  effects[EFFECT_CONF_SPHERE]++;
			  break;
			}

			case 49:
			{
			  /* sphere of chaos */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_CHAOS, dir,
				    45 + (plev), 2);
			  effects[EFFECT_CHAOS_SPHERE]++;
			  break;
			}

			case 50:
			{
			  /* sphere of sound */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_SOUND, dir,
				    50 + (plev), 2);
			  effects[EFFECT_SOUND_SPHERE]++;
			  break;
			}

			case 51:
			{
			  /* explosion */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_SHARD, dir,
				    80 + (plev), 2);
			  effects[EFFECT_EXPLOSION]++;
			  break;
			}

			case 52:
			{
			  /* remove fear */
			  (void)set_afraid(0);
			  effects[EFFECT_REMOVE_FEAR]++;
			  break;
			}

			case 53:
			{
			  /* resist light & dark */
			  (void)set_oppose_ld(p_ptr->oppose_ld + randint(20) + 20);
			  effects[EFFECT_OPPOSE_LD]++;
			  break;	
			}

			case 54:
			{
			  (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
			  effects[EFFECT_RES_POIS]++;
			  break;
			}

			case 55:
			{
			  /* resist chaos & confusion */
			  (void)set_oppose_cc(p_ptr->oppose_cc + randint(20) + 20);
			  effects[EFFECT_OPPOSE_CC]++;
			  break;
			}

			case 56:
			{
			  /* resist sound & shards */
			  (void)set_oppose_ss(p_ptr->oppose_ss + randint(20) + 20);
			  effects[EFFECT_OPPOSE_SS]++;
			  break;
			}

			case 57:
			{
			  /* resist nexus */
			  (void)set_oppose_nex(p_ptr->oppose_nex + randint(20) + 20);
			  effects[EFFECT_OPPOSE_NEX]++;
			  break;
			}

			case 58:
			{
			  /* invisibility */
			  /* (void)set_invis(p_ptr->tim_invis + randint(24) + 24, 30); */
			  msg_print("This spell not currently available");
			  break;	
			}

			case 59:
			{
			  /* shadow monsters */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_DARK, 5, 30 + (plev), 2);
			  fire_beam(GF_FORCE, dir,
				    damroll(6+((plev-5)/4), 8));
			  effects[EFFECT_SHADOW_MONSTERS]++;
			  break;
			}

			case 60:
			{
			  /* shadow ball */
			  if (!get_aim_dir(&dir)) return;
			  fire_ball(GF_DARK, dir,
				    40 + (plev), 2);
			  effects[EFFECT_SHADOW_BALL]++;
			  break;
			}

			case 61:
			{
			  /* life for mana */
			  int m = 0;
			  m = get_quantity("How much mana?",999);
			  if (!m) return;
			  if (m<0) return;
			  if (m > p_ptr->chp) {
			    msg_print("You don't have that much life!");
			    return;
			  }
			  if ((p_ptr->csp + m) > p_ptr->msp)
			    m = p_ptr->msp - p_ptr->csp;
			  p_ptr->csp += m;
			  take_hit(m,"spellcasting");
			  msg_print("You convert life into mana.");
			  effects[EFFECT_LIFE_FOR_MANA]++;
			  break;
			}

			case 62:
			{
			  /* shadow gate */
			  teleport_player(plev * 7);
			  effects[EFFECT_TELEPORT]++;
			  break;
			}

			case 63:
			{
			  /* summon shadows */
			  /* summon_monster(SUMMON_SHADOWS); */
			  msg_print("This spell not currently available");
			  break;
			}

		}

		/* A spell was cast */
		if (!((spell < 32) ?
		      (p_ptr->spell_worked1[0] & (1L << spell)) :
		      (p_ptr->spell_worked2[0] & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1[0] |= (1L << spell);
			}
			else
			{
			        p_ptr->spell_worked2[0] |= (1L << (spell - 32));
			}

			/* Gain experience */
			if (player_has_class(CLASS_ILLUSIONIST, 0))
			  gain_exp(e * s_ptr->slevel, 
				   index_of_class(CLASS_ILLUSIONIST));
			if (player_has_class(CLASS_TRICKSTER, 0))
			  gain_exp(e * s_ptr->slevel, 
				   index_of_class(CLASS_TRICKSTER));

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
 * Poison the current weapon
 */
static void poison_weapon(void)
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

		act = "is covered in a sickly green aura!";
		o_ptr->name2 = EGO_BRAND_POIS;

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
 * Cast a death spell
 */
void do_cmd_cast_death(void)
{
        int dir;
	int plev = p_ptr->lev[p_ptr->current_class];

	int item, sval, spell;
	int chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book != TV_DEATH_BOOK)
	{
		msg_print("Pray hard enough and your prayers may be answered.");
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
	item_tester_tval = mp_ptr[p_ptr->pclass[p_ptr->current_class]]->spell_book;

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


	/* Ask for a spell */
	if (!get_spell(&spell, "cast", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any prayers in that book.");
		return;
	}


	/* Get the spell */
	s_ptr = &mp_ptr[p_ptr->pclass[p_ptr->current_class]]->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->cpp)
	{
		/* Warning */
		msg_print("You do not have enough piety to recite this prayer.");

		/* Flush input */
		flush();

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
	}

	/* Process spell */
	else
	{
	     /* Spells.  */
	     switch (spell)
	     {
		       
	     case 0: /* Malediction */
		  {
		       if (!get_aim_dir(&dir)) return;
		       fire_bolt(GF_NETHER, dir,
					 damroll(3 + ((plev - 1) / 5), 4));
		       effects[EFFECT_MALEDICTION] += 1;
		       break;
		  }
		  
	     case 1: /* Detect Undead */
		  {
		       (void)detect_monsters_undead();
		       effects[EFFECT_DETECT_UNDEAD]++;
		       break;
		  }

	     case 2: /* Cure Light Wounds */
		  {
		       (void)hp_player(damroll(2, 8));
		       (void)set_cut(p_ptr->cut - 15);
		       effects[EFFECT_CURE_LIGHT]++;
		       break;
		  }

	     case 3: /* Bless */
		  {
		       (void)set_blessed(p_ptr->blessed + randint(12) + 12);
		       effects[EFFECT_BLESS]++;
		       break;
		  }

	     case 4: /* Remove Fear */
		  {
		       (void)set_afraid(0);
		       effects[EFFECT_REMOVE_FEAR]++;
		       break;
		  }

	     case 5: /* Infravision */
		  {
		       if (p_ptr->tim_infra == 0)
			    set_tim_infra(p_ptr->tim_infra + 200 + 
					  randint(100));
		       effects[EFFECT_INFRA]++;
		       break;
		  }

	     case 6: /* Horrify */
		  {
		       if (!get_aim_dir(&dir)) return;
		       (void)fear_monster(dir, plev);
		       effects[EFFECT_FEAR_MONSTER]++;
		       break;
		  }

	     case 7: /* Slow Poison */
		  {
		       (void)set_poisoned(p_ptr->poisoned / 2);
		       effects[EFFECT_SLOW_POISON]++;
		       break;
		  }

	     case 8: /* Stealth */
		  {
		       if (p_ptr->tim_stealth == 0)
			    set_tim_stealth(p_ptr->tim_stealth + 200 + 
					  randint(100));
		       effects[EFFECT_STEALTH]++;
		       break;
		  }

	     case 9: /* Call Light */
		  {
		       (void)lite_area(damroll(2, (plev / 2)), 
				       (plev / 10) + 1);
		       effects[EFFECT_LIGHT_AREA]++;
		       break;
		  }

	     case 10: /* Find Hidden Traps/Doors */
		  {
		       (void)detect_traps();
		       (void)detect_doors();
		       (void)detect_stairs();
		       effects[EFFECT_DETECT_TRAP]++;
		       effects[EFFECT_DETECT_DOOR]++;
		       break;
		  }

	     case 11: /* Cure Serious Wounds */
		  {
		       (void)hp_player(damroll(4, 10));
		       (void)set_cut((p_ptr->cut / 2) - 20);
		       effects[EFFECT_CURE_SERIOUS]++;
		       break;
		  }

	     case 12: /* Heroism */
		  {
		       int temp = 
			    25 * (player_has_class(CLASS_BERSERKER, 0) + 1);
		       (void)hp_player(10);
		       (void)set_hero(p_ptr->hero + randint(temp) + temp);
		       (void)set_afraid(0);
		       effects[EFFECT_HEROISM]++;
		       effects[EFFECT_REMOVE_FEAR]++;
		       break;
		  }

	     case 13: /* Sanctuary */
		  {
		       (void)sleep_monsters_touch(p_ptr->current_class);
		       effects[EFFECT_SLEEP_TOUCH]++;
		       break;
		  }

	     case 14: /* Res Cold */
		  {
		       (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
		       effects[EFFECT_RES_COLD]++;
		       break;
		  }

	     case 15: /* Noxious Fumes */
		  {
		       if (!get_aim_dir(&dir)) return;
		       fire_ball(GF_POIS, dir,
				 10 + (plev / 2), 3);
		       effects[EFFECT_STINKING_CLOUD]++;
		       break;
		  }

	     case 16: /* Break Curse */
		  {
		       remove_curse();
		       effects[EFFECT_REMOVE_CURSE]++;
		       break;
		  }

	     case 17: /* Sustenance */
		  {
		       (void)set_food(PY_FOOD_MAX - 1);
		       effects[EFFECT_SATISFY_HUNGER]++;
		       break;
		  }

	     case 18: /* Neutralize Poison */
		  {
		       (void)set_poisoned(0);
		       effects[EFFECT_CURE_POISON]++;
		       break;
		  }

	     case 19: /* Shield */
		  {
		       (void)set_shield(p_ptr->shield + randint(20) + 30);
		       effects[EFFECT_SHIELD]++;
		       break;
		  }

	     case 20: /* Orb of Enthropy */
		  {
		       if (!get_aim_dir(&dir)) return;
		       fire_ball(GF_NETHER, dir,
				 (damroll(3, 6) + plev +
				  (plev / 2)),
				 ((plev < 30) ? 2 : 3));
		       effects[EFFECT_ORB_ENTHROPY]++;
		       break;
		  }

	     case 21: /* Cure Critical Wounds */
		  {
		       (void)hp_player(damroll(6, 10));
		       (void)set_cut(0);
		       effects[EFFECT_CURE_CRITICAL]++;
		       break;
		  }

	     case 22: /* Protection from Undead */
		  {
		       (void)set_prot_undead(p_ptr->prot_undead + 
					randint(25) + 
					3 * p_ptr->lev[p_ptr->current_class]);
		       effects[EFFECT_PRO_UNDEAD]++;
		       break;
		  }

	     case 23: /* Sense Invisible */
		  {
		       (void)set_tim_invis(p_ptr->tim_invis + 
					   randint(24) + 24);
		       effects[EFFECT_SEE_INVIS]++;
		       break;
		  }

	     case 24: /* Turn Undead */
		  {
		       (void)turn_undead(p_ptr->current_class);
		       effects[EFFECT_TURN_UNDEAD]++;
		       break;
		  }

	     case 25: /* Drain Life */
		  {
		       if (!get_aim_dir(&dir)) return;
		       drain_life(dir, 50);
		       effects[EFFECT_DRAIN_LIFE]++;
		       break;
		  }

	     case 26: /* Sense Shadows */
		  {
		       map_area();
		       effects[EFFECT_MAPPING]++;
		       break;
		  }

	     case 27: /* Cure Mortal Wounds */
		  {
		       (void)hp_player(damroll(8, 10));
		       (void)set_stun(0);
		       (void)set_cut(0);
		       effects[EFFECT_CURE_MORTAL]++;
		       break;
		  }

	     case 28: /* Terror */
		  {
		       /* (void)scare_monsters(); */
		       /* effects[EFFECT_SCARE_ALL]++; */
		       msg_print("This prayer not currently available");
		       break;
		  }

	     case 29: /* Dispel Undead */
		  {
		       (void)dispel_undead(randint(plev * 3));
		       effects[EFFECT_DISPEL_UNDEAD]++;
		       break;
		  }

	     case 30: /* Heal */
		  {
		       (void)hp_player(300);
		       (void)set_stun(0);
		       (void)set_cut(0);
		       effects[EFFECT_HEAL]++;
		       break;
		  }

	     case 31: /* Darkness Storm */
		  {
		       if (!get_aim_dir(&dir)) return;
		       fire_ball(GF_DARK, dir, 35 + (plev), 2);
		       effects[EFFECT_DARKNESS_STORM]++;
		       break;
		  }

	     case 32: /* Glyph of Warding */
		  {
		       warding_glyph();
		       effects[EFFECT_GLYPH_WARDING]++;
		       break;
		  }

	     case 33: /* Cure Serious Wounds II */
		  {
		       (void)hp_player(damroll(4, 10));
		       (void)set_cut(0);
		       effects[EFFECT_CURE_SERIOUS]++;
		       break;
		  }

	     case 34: /* Cure Mortal Wounds II */
		  {
		       (void)hp_player(damroll(8, 10));
		       (void)set_stun(0);
		       (void)set_cut(0);
		       effects[EFFECT_CURE_MORTAL]++;
		       break;
		  }

	     case 35: /* Healing */
		  {
		       (void)hp_player(2000);
		       (void)set_stun(0);
		       (void)set_cut(0);
		       effects[EFFECT_HEAL]++;
		       break;
		  }

	     case 36: /* Restore Life */
		  {
		       (void)restore_level();
		       effects[EFFECT_RESTORE_EXP]++;
		       break;
		  }

	     case 37: /* Restoration */
		  {
		       (void)do_res_stat(A_STR);
		       (void)do_res_stat(A_INT);
		       (void)do_res_stat(A_WIS);
		       (void)do_res_stat(A_DEX);
		       (void)do_res_stat(A_CON);
		       (void)do_res_stat(A_CHR);
		       effects[EFFECT_RESTORE_STR]++;
		       effects[EFFECT_RESTORE_CON]++;
		       effects[EFFECT_RESTORE_DEX]++;
		       effects[EFFECT_RESTORE_INT]++;
		       effects[EFFECT_RESTORE_WIS]++;
		       effects[EFFECT_RESTORE_CHR]++;
		       break;
		  }

	     case 38: /* Dispel Undead II */
		  {
		       (void)dispel_undead(randint(plev * 4));
		       effects[EFFECT_DISPEL_UNDEAD]++;
		       break;
		  }

	     case 39: /* Banish Undead */
		  {
		       if (banish_undead(100))
		       {
			    msg_print("You banish the undead!");
		       }
		       effects[EFFECT_BANISH_UNDEAD]++;
		       break;
		  }

	     case 40: /* Genocide */
		  {
		       (void)genocide();
		       effects[EFFECT_GENOCIDE]++;
		       break;
		  }

	     case 41: /* Mass Genocide */
		  {
		       (void)mass_genocide();
		       effects[EFFECT_MASS_GENOCIDE]++;
		       break;
		  }

	     case 42: /* Annihilation */
		  {
		       if (!get_aim_dir(&dir)) return;
		       drain_life(dir, 200);
		       effects[EFFECT_DRAIN_LIFE]++;
		       break;
		  }

	     case 43: /* Res Lite/Dark */
		  {
		       (void)set_oppose_ld(p_ptr->oppose_ld + 
					   randint(20) + 20);
		       effects[EFFECT_OPPOSE_LD]++;
		       break;	
		  }

	     case 44: /* Invisibility */
		  {
		       /* (void)set_invis(p_ptr->tim_invis + randint(24) + 24, 30); */
		       /* effects[EFFECT_INVIS]++; */
		       msg_print("This prayer not currently available");
		       break;	
		  }

	     case 45: /* Regeneration */
		  {
		       (void)set_tim_regen(p_ptr->tim_regen + 
					   randint(20) + 20);
		       effects[EFFECT_REGEN]++;
		       break;			  
		  }

	     case 46: /* Res Nether */
		  {
		       (void)set_oppose_nether(p_ptr->oppose_nether + 
					       randint(20) + 20);
		       effects[EFFECT_OPPOSE_NETHER]++;
		       break;	
		  }

	     case 47: /* Wraithform */
		  {
		       /* (void)set_shadow(p_ptr->wraith_form + plev + randint(24)); */
		       /* effects[EFFECT_SHADOWFORM]++; */
		       msg_print("This prayer not currently available");
		       break;
		  }

	     case 48: /* Berserk Strength */
		  {
		       int temp = 
			    25 * (player_has_class(CLASS_BERSERKER, 0) + 1);
		       (void)hp_player(30);
		       (void)set_shero(p_ptr->shero + randint(temp) + temp);
		       (void)set_afraid(0);
		       effects[EFFECT_BERSERK]++;
		       effects[EFFECT_REMOVE_FEAR]++;
		       break;
		  }

	     case 49: /* Invulnerability */
		  {
		       (void)set_invuln(p_ptr->invuln + randint(8) + 8);
		       effects[EFFECT_GLOBE_OF_INVULN]++;
		       break;
		  }

	     case 50: /* Recharging */
		  {
		       (void)recharge(15);
		       effects[EFFECT_RECHARGE_SMALL]++;
		       break;
		  }

	     case 51: /* Dispel Curse */
		  {
		       (void)remove_all_curse();
		       effects[EFFECT_DISPEL_CURSE]++;
		       break;
		  }

	     case 52: /* Enchant Weapon */
		  {
		       (void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
		       effects[EFFECT_ENCHANT_WEAPON_HIT]++;
		       effects[EFFECT_ENCHANT_WEAPON_DAM]++;
		       break;
		  }

	     case 53: /* Enchant Armour */
		  {
		       (void)enchant_spell(0, 0, rand_int(3) + 2);
		       effects[EFFECT_ENCHANT_ARMOUR]++;
		       break;
		  }

	     case 54: /* Poison Weapon */
		  {
		       (void)poison_weapon();
		       effects[EFFECT_POISON_WEAPON]++;
		       break;
		  }

	     case 55: /* Shadow Door */
		  {
		       teleport_player(10);
		       effects[EFFECT_BLINK]++;
		       break;
		  }

	     case 56: /* Unbarring Ways */
		  {
		       (void)destroy_doors_touch();
		       effects[EFFECT_DOOR_DESTRUCT_TOUCH]++;
		       break;
		  }

	     case 57: /* Perception */
		  {
		       (void)ident_spell();
		       effects[EFFECT_IDENTIFY]++;
		       break;
		  }

	     case 58: /* Probing */
		  {
		       (void)probing();
		       effects[EFFECT_PROBING]++;
		       break;
		  }

	     case 59: /* Word of Recall */
		  {
		       if (p_ptr->astral)
		       {
			    msg_print("You feel a terrible sense of loss.");
			    break;
		       }
		       set_recall();
		       effects[EFFECT_RECALL]++;
		       break;
		  }
	     }
	     
	     /* A spell was cast */
	     if (!((spell < 32) ?
		   (p_ptr->spell_worked1[1] & (1L << spell)) :
		   (p_ptr->spell_worked2[1] & (1L << (spell - 32)))))
	     {
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1[1] |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2[1] |= (1L << (spell - 32));
			}

			/* Gain experience */
			if (player_has_class(CLASS_DEATH_PRIEST, 0))
			  gain_exp(e * s_ptr->slevel, 
				   index_of_class(CLASS_DEATH_PRIEST));

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
	     }
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (s_ptr->smana <= p_ptr->cpp)
	{
		/* Use some mana */
		p_ptr->cpp -= s_ptr->smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = s_ptr->smana - p_ptr->cpp;

		/* No mana left */
		p_ptr->cpp = 0;
		p_ptr->cpp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage WIS (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You feel less wise!");

			/* Reduce constitution */
			(void)dec_stat(A_WIS, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}
