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

	cptr p = ((cp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

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
		    (spell_flags[cp_ptr->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[cp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
			        prompt, spell_names[cp_ptr->spell_type][spell],
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
	if (!cp_ptr->spell_book)
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
	item_tester_tval = cp_ptr->spell_book;

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
		    (spell_flags[cp_ptr->spell_type][sval][0] & (1L << spell)) :
		    (spell_flags[cp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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

	cptr p = ((cp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

	cptr q, s;

	object_type *o_ptr;


	if (!cp_ptr->spell_book)
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
	item_tester_tval = cp_ptr->spell_book;

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
	if (cp_ptr->flags & CF_CHOOSE_SPELLS)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE) && (spell == -1)) return;
	}
	else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < PY_MAX_SPELLS; spell++)
		{
			/* Check spells in the book */
			if ((spell < 32) ?
			    (spell_flags[cp_ptr->spell_type][sval][0] & (1L << spell)) :
			    (spell_flags[cp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i] = spell;

	/* Mention the result */
	message_format(MSG_STUDY, 0, "You have learned the %s of %s.",
	           p, spell_names[cp_ptr->spell_type][spell]);

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
static void do_cmd_cast_spell(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir;
	int chance, beam;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


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
	item_tester_tval = cp_ptr->spell_book;

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
		beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

		/* Spells. */
		switch (spell)
		{
			case SPELL_MAGIC_MISSILE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
				break;
			}

			case SPELL_DETECT_MONSTERS:
			{
				(void)detect_monsters_normal();
				break;
			}

			case SPELL_PHASE_DOOR:
			{
				teleport_player(10);
				break;
			}

			case SPELL_LIGHT_AREA:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case SPELL_TREASURE_DETECTION:
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case SPELL_CURE_LIGHT_WOUNDS:
			{
 				(void)do_cure_wounds();
				break;
			}

			case SPELL_OBJECT_DETECTION:
			{
				(void)detect_objects_normal();
				break;
			}

			case SPELL_FIND_TRAPS_DOORS:
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case SPELL_STINKING_CLOUD:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + (plev / 2), 2);
				break;
			}

			case SPELL_CONFUSE_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				break;
			}

			case SPELL_LIGHTNING_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				break;
			}

			case SPELL_TRAP_DOOR_DESTRUCTION:
			{
				(void)destroy_doors_touch();
				break;
			}

			case SPELL_SLEEP_I:
			{
				if (!get_aim_dir(&dir)) return;
				(void)do_sleep_monster();
				break;
			}

			case SPELL_CURE_POISON:
			{
				(void)set_poisoned(0);
				break;
			}

			case SPELL_TELEPORT_SELF:
			{
				teleport_player(plev * 5);
				break;
			}

			case SPELL_SPEAR_OF_LIGHT:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				fire_beam(GF_LITE, dir,
				          damroll(2+((plev-5)/4), 6));
				lite_line(dir);
				break;
			}

			case SPELL_FROST_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				break;
			}

			case SPELL_TURN_STONE_TO_MUD:
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case SPELL_SATISFY_HUNGER:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case SPELL_RECHARGE_ITEM_I:
			{
				(void)recharge((plev * 2));
				break;
			}

			case SPELL_FETCH_ITEM:
			{
				if (!get_aim_dir(&dir)) return;
				fetch_item(dir, plev*15);
				break;
			}

			case SPELL_POLYMORPH_OTHER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}

			case SPELL_IDENTIFY:
			{
				(void)ident_spell();
				break;
			}

			case SPELL_LOWER_WATER:
			{
				alter_terrain(py, px, plev, 2);
				break;
			}

			case SPELL_FIRE_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case SPELL_SLOW_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case SPELL_FROST_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          30 + (plev), 2);
				break;
			}

			case SPELL_SUMMON_ELEMENTAL:
			{
				summon_monster(SUMMON_ELEMENTAL);
				break;
			}

			case SPELL_TELEPORT_OTHER:
			{
				if (p_ptr->ts_anchor)
				{
					msg_print("Your time/space anchor prevents it from leaving.");
					break;
				}
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
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
				break;
			}

			case SPELL_FIRE_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          55 + (plev), 2);
				break;
			}

			case SPELL_WORD_OF_DESTRUCTION:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case SPELL_GENOCIDE:
			{
				(void)genocide();
				break;
			}

			case SPELL_DOOR_CREATION:
			{
				(void)door_creation();
				break;
			}

			case SPELL_STAIR_CREATION:
			{
				(void)stair_creation();
				break;
			}

			case SPELL_TELEPORT_LEVEL:
			{
				if (p_ptr->ts_anchor)
				{
					msg_print("Your time/space anchor prevents you from leaving.");
					break;
				}
				(void)teleport_player_level();
				break;
			}

			case SPELL_EARTHQUAKE:
			{
				earthquake(py, px, 10);
				break;
			}

			case SPELL_WORD_OF_RECALL:
			{
				set_recall();
				break;
			}

			case SPELL_DIMENSION_DOOR:
			{
				teleport_specific();
				break;
			}

			case SPELL_ACID_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case SPELL_CLOUD_KILL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          20 + (plev / 2), 3);
				break;
			}

			case SPELL_ACID_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          40 + (plev), 2);
				break;
			}

			case SPELL_ICE_STORM:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          70 + (plev), 3);
				break;
			}

			case SPELL_METEOR_SWARM:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
				          65 + (plev), 3);
				break;
			}

			case SPELL_PLASMA_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_PLASMA, dir,
				          damroll(10+((plev-5)/4), 8));
				break;
			}

			case SPELL_MANA_STORM:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          300 + (plev * 2), 3);
				break;
			}

			case SPELL_WIZARD_LIGHT:
			{
				wiz_lite();
				break;
			}

			case SPELL_DETECT_ENCHANTMENT:
			{
				(void)detect_objects_magic();
				break;
			}

			case SPELL_TIME_SPACE_ANCHOR:
			{
				(void)set_tim_ts_anchor(p_ptr->ts_anchor + randint(100) + p_ptr->lev);
				break;
			}

			case SPELL_GENOCIDE2:
			{
				(void)genocide();
				break;
			}

			case SPELL_MASS_GENOCIDE:
			{
				(void)mass_genocide();
				break;
			}

			case SPELL_PROBING:
			{
				(void)probing();
				break;
			}

			case SPELL_RESIST_FIRE:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				break;
			}

			case SPELL_RESIST_COLD:
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case SPELL_RESIST_ACID:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				break;
			}

			case SPELL_RESIST_ELEC:
			{
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				break;
			}

			case SPELL_RUST_PROOF:
			{
				rustproof();
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
				break;
			}

			case SPELL_HEROISM:
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case SPELL_SHIELD:
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

			case SPELL_BERSERKER:
			{
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case SPELL_LIFE_FOR_MANA:
			{
				int m = 0;
				m = get_quantity("How much mana?",999);
				if (!m) return;
				if (m < 0) return;
				if (m > p_ptr->chp)
				{
					msg_print("You don't have that much life!");
					return;
				}
				if ((p_ptr->csp + m) > p_ptr->msp)
				{
					m = p_ptr->msp - p_ptr->csp;
				}
				p_ptr->csp += m;
				take_hit(m, "spellcasting");
				msg_print("You convert life into mana.");
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
				break;
			}

			case SPELL_GLOBE_OF_INVULNERABILITY:
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
static void do_cmd_pray_priest(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir, chance;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


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
	item_tester_tval = cp_ptr->spell_book;

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
				break;
			}

			case PRAYER_CURE_WOUNDS:
			{
				(void)do_cure_wounds();
				break;
			}

			case PRAYER_BLESS:
			{
				(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				break;
			}

			case PRAYER_REMOVE_FEAR:
			{
				(void)set_afraid(0);
				break;
			}

			case PRAYER_CALL_LIGHT:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case PRAYER_FIND_TRAPS:
			{
				(void)detect_traps();
				break;
			}

			case PRAYER_DETECT_DOORS_STAIRS:
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case PRAYER_SLOW_POISON:
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case PRAYER_SCARE_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)do_fear_monster();
				break;
			}

			case PRAYER_PORTAL:
			{
				teleport_player(plev * 3);
				break;
			}

			case PRAYER_DUSTSTORM:
			{
				fire_ball(GF_FORCE, 5, 3 + (plev / 4), 6);
				break;
			}

			case PRAYER_CHANT:
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				break;
			}

			case PRAYER_SANCTUARY:
			{
				(void)do_sleep_monster();
				break;
			}

			case PRAYER_SATISFY_HUNGER:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case PRAYER_REMOVE_CURSE:
			{
				remove_curse();
				break;
			}

			case PRAYER_RESIST_HEAT_COLD:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

			case PRAYER_LEVITATE:
			{
				(void)set_tim_levitate(p_ptr->tim_levitate + randint(50) + 50);
				break;
			}

			case PRAYER_ORB_OF_DRAINING:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir, (damroll(3, 6) + plev +
				     (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4))),
				     ((plev < 30) ? 2 : 3));
				break;
			}

			case PRAYER_DISINTEGRATE:
			{
				alter_terrain(py, px, plev, 3);
				break;
			}

			case PRAYER_SENSE_INVISIBLE:
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				break;
			}

			case PRAYER_PROTECTION_FROM_EVIL:
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			}

			case PRAYER_EARTHQUAKE:
			{
				earthquake(py, px, 10);
				break;
			}

			case PRAYER_SENSE_SURROUNDINGS:
			{
				map_area();
				break;
			}

			case PRAYER_CURE_MORTAL_WOUNDS:
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case PRAYER_TURN_UNDEAD:
			{
				(void)turn_undead();
				break;
			}

			case PRAYER_PRAYER:
			{
				(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				break;
			}

			case PRAYER_DISPEL_UNDEAD:
			{
				(void)dispel_undead(plev * 3);
				break;
			}

			case PRAYER_HEAL:
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case PRAYER_DISPEL_EVIL:
			{
				(void)dispel_evil(plev * 3);
				break;
			}

			case PRAYER_GLYPH_OF_WARDING:
			{
				warding_glyph();
				break;
			}

			case PRAYER_HOLY_WORD:
			{
				(void)dispel_evil(plev * 4);
				(void)hp_player(1000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case PRAYER_SUMMON_ANGEL:
			{
				summon_monster(SUMMON_ANGEL);
				break;
			}

			case PRAYER_DETECT_MONSTERS:
			{
				(void)detect_monsters_normal();
				break;
			}

			case PRAYER_DETECTION:
			{
				(void)detect_all();
				break;
			}

			case PRAYER_PERCEPTION:
			{
				(void)ident_spell();
				break;
			}

			case PRAYER_PROBING:
			{
				(void)probing();
				break;
			}

			case PRAYER_CLAIRVOYANCE:
			{
				wiz_lite();
				break;
			}

			case PRAYER_RESIST_NETHER:
			{
				(void)set_oppose_nethr(p_ptr->oppose_nethr + randint(20) + 20);
				break;
			}

			case PRAYER_CURE_WOUNDS2:
			{
 				(void)do_cure_wounds();
				break;
			}

			case PRAYER_CURE_MORTAL_WOUNDS2:
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case PRAYER_STAR_HEALING:
			{
				(void)hp_player(2000);
				(void)set_stun(0);
				(void)set_cut(0);
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
				break;
			}

			case PRAYER_REMEMBRANCE:
			{
				(void)restore_level();
				break;
			}

			case PRAYER_DISPEL_DEMONS:
			{
				(void)dispel_demons(plev * 4);
				break;
			}

			case PRAYER_DISPEL_UNDEAD2:
			{
				(void)dispel_undead(plev * 4);
				break;
			}

			case PRAYER_DISPEL_EVIL2:
			{
				(void)dispel_evil(plev * 4);
				break;
			}

			case PRAYER_BANISHMENT:
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			}

			case PRAYER_WORD_OF_DESTRUCTION:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case PRAYER_ANNIHILATION:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 200);
				break;
			}

			case PRAYER_BALEFIRE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_BALEFIRE, dir,
				          damroll(15+((plev-5)/3), 10));
				break;
			}

			case PRAYER_UNBARRING_WAYS:
			{
				(void)destroy_doors_touch();
				break;
			}

			case PRAYER_RECHARGING:
			{
				(void)recharge((plev * 2));
				break;
			}

			case PRAYER_DISPEL_CURSE:
			{
				(void)remove_all_curse();
				break;
			}

			case PRAYER_ENCHANT_WEAPON:
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

			case PRAYER_ENCHANT_ARMOUR:
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

			case PRAYER_ELEMENTAL_BRAND:
			{
				brand_weapon();
				break;
			}

			case PRAYER_IMPRISON:
			{
				if (!get_aim_dir(&dir)) return;
				imprision(FEAT_WALL_EXTRA, dir, p_ptr->lev/10);
				break;
			}

			case PRAYER_BLINK:
			{
				teleport_player(10);
				break;
			}

			case PRAYER_TELEPORT_SELF:
			{
				teleport_player(plev * 8);
				break;
			}

			case PRAYER_TELEPORT_OTHER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case PRAYER_TELEPORT_LEVEL:
			{
				(void)teleport_player_level();
				break;
			}

			case PRAYER_WORD_OF_RECALL:
			{
				set_recall();
				break;
			}

			case PRAYER_ALTER_REALITY:
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}

			case PRAYER_IMMOLATION:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				    100 + (plev * 2), 3);
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
 * Cast an illusion
 */
static void do_cmd_cast_illusion(void)
{
	int item, sval, spell, dir;
	int chance, beam;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


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
	item_tester_tval = cp_ptr->spell_book;

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
		beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

		/* Spells. */
		switch (spell)
		{
			case ILLUS_CONFUSION_BOLT:
			{
 				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_CONFUSION, dir,
				                  damroll(2 + ((plev - 1) / 5), 4));
				break;
			}

			case ILLUS_DETECT_MONSTERS:
			{
				(void)detect_monsters_normal();
				break;
			}

			case ILLUS_PHASE_DOOR:
			{
				teleport_player(10);
				break;
			}

			case ILLUS_LIGHT_AREA:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case ILLUS_TREASURE_DETECTION:
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case ILLUS_FEAR:
			{
				(void)do_fear_monster();
				break;
			}

			case ILLUS_OBJECT_DETECTION:
			{
				(void)detect_objects_normal();
				break;
			}

			case ILLUS_FIND_TRAPS_DOORS:
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case ILLUS_STINKING_CLOUD:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + (plev / 2), 2);
				break;
			}

			case ILLUS_INFRAVISION:
			{
				if (p_ptr->tim_infra == 0)
				set_tim_infra(p_ptr->tim_infra + 200 + randint(100));
				break;
			}

			case ILLUS_SLEEP:
			{
				(void)do_sleep_monster();
				break;
			}

			case ILLUS_TRAP_DOOR_DESTRUCTION:
			{
				(void)destroy_doors_touch();
				break;
			}

			case ILLUS_FOG_CLOUD:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FOG, dir,
				          10 + (plev / 2), 3);
				break;
			}

			case ILLUS_CURE_POISON:
			{
				(void)set_poisoned(0);
				break;
			}

			case ILLUS_SATISFY_HUNGER:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case ILLUS_SHADOW_DOOR:
			{
				(void)door_creation();
				break;
			}

			case ILLUS_SHADOW_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FORCE, dir,
				          damroll(5+((plev-6)/4), 8));
				break;
			}

			case ILLUS_TURN_STONE_TO_MUD:
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case ILLUS_DETECT_INVISIBLE:
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				break;
			}

			case ILLUS_RECHARGE_ITEM_I:
			{
				(void)recharge((plev * 2));
				break;
			}

			case ILLUS_BRAND_AMMO:
			{
				(void)brand_ammo(1,0);
				break;
			}

			case ILLUS_SPEAR_OF_LIGHT:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				fire_beam(GF_LITE, dir,
				          damroll(2+((plev-5)/4), 6));
				lite_line(dir);
				break;
			}

			case ILLUS_CHAOS:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_CHAOS, dir,
				          25 + plev, 2);
				break;
			}

			case ILLUS_MENTAL_BARRIER:
			{
				(void)set_tim_sus_int(p_ptr->tim_sus_int + randint(50) + 50);
				(void)set_tim_sus_wis(p_ptr->tim_sus_wis + randint(50) + 50);
				break;
			}

			case ILLUS_TRUE_SIGHT:
			{
				map_area();
				break;
			}

			case ILLUS_SLOW_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case ILLUS_SHADOW_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir,
                                  35 + (plev), 2);
				break;
			}

			case ILLUS_BOLT_OF_DARKNESS:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_DARK, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case ILLUS_SHADOW_FORM:
			{
				(void)set_tim_ghostly(p_ptr->tim_ghostly + plev + randint(24));
				(void)set_tim_invis(p_ptr->tim_invis + plev + randint(24));
				break;
			}

			case ILLUS_HASTE_SELF:
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

			case ILLUS_PRISMATIC_WALL:
			{
				warding_glyph();
				break;
			}

			case ILLUS_PRISMATIC_SPRAY:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_LITE, 5,
                                  40 + (plev), 2);
				fire_beam(GF_LITE, dir,
				          damroll(8+((plev-5)/4), 8));
				break;
			}

			case ILLUS_CHROMATIC_SHIELD:
			{
				(void)set_shield(p_ptr->shield + randint(30) + 30);
				break;
			}

			case ILLUS_WIZARD_LOCK:
			{
				do_cmd_spike(TRUE);
				break;
			}

			case ILLUS_BEDLAM:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_CHAOS, dir,
				          50 + plev, 10);
				break;
			}

			case ILLUS_WORD_OF_RECALL:
			{
				set_recall();
				break;
			}

			case ILLUS_DETECT_ENCHANTMENT:
			{
				(void)detect_objects_magic();
				break;
			}

			case ILLUS_PROBING:
			{
				(void)probing();
				break;
			}

			case ILLUS_SUNFIRE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_LITE, dir,
				          50 + plev, 10);
				break;
			}

			/*
			 * The bigby spells that are duplicates - warding,
			 * slow etc. shoudl act all aroundplayer
			 */
			case ILLUS_INTERPOSING_HAND:
			{
				warding_glyph();
				break;
			}

			case ILLUS_PHANTOM_HAND:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_CONFUSION, dir,
				                  damroll(2+((plev-5)/4), 8));
				break;
			}

			case ILLUS_FORCEFUL_HAND:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FORCE, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case ILLUS_GRASPING_HAND:
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case ILLUS_CLENCHED_FIST:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_FORCE, dir,
				          damroll(10+((plev-5)/4), 8));
				break;
			}

			case ILLUS_CRUSHING_HAND:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_GRAVITY, dir,
				          damroll(12+((plev-5)/4), 8));
				break;
			}

			case ILLUS_FORCE_BLAST:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FORCE, 5,
				          300 + (plev * 2), 3);
				break;
			}

			case ILLUS_SPHERE_OF_LIGHT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_LITE, dir,
				          30 + (plev), 2);
				break;
			}

			case ILLUS_SPHERE_OF_DARKNESS:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir,
				          35 + (plev), 2);
				break;
			}

			case ILLUS_SPHERE_OF_CONFUSION:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_CONFUSION, dir,
				          40 + (plev), 2);
				break;
			}

			case ILLUS_SPHERE_OF_CHAOS:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_CHAOS, dir,
				          45 + (plev), 2);
				break;
			}

			case ILLUS_SPHERE_OF_SOUND:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SOUND, dir,
				          50 + (plev), 2);
				break;
			}

			case ILLUS_EXPLOSION:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SHARD, dir,
				          80 + (plev), 2);
				break;
			}

			case ILLUS_RESIST_FEAR:
			{
				(void)set_afraid(0);
				break;
			}

			case ILLUS_RESIST_LITE_AND_DARK:
			{
				(void)set_oppose_ld(p_ptr->oppose_ld + randint(20) + 20);
				break;
			}

			case ILLUS_RESIST_POISON:
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case ILLUS_RESIST_CHAOS_AND_CONFU:
			{
				(void)set_oppose_cc(p_ptr->oppose_cc + randint(20) + 20);
				break;
			}

			case ILLUS_RESIST_SOUND_AND_SHARDS:
			{
				(void)set_oppose_ss(p_ptr->oppose_ss + randint(20) + 20);
				break;
			}

			case ILLUS_RESIST_NEXUS:
			{
				(void)set_oppose_nexus(p_ptr->oppose_nexus + randint(20) + 20);
				break;
			}

			case ILLUS_INVISIBILITY:
			{
				(void)set_tim_pl_invis(p_ptr->tim_pl_invis + randint(24));
				(void)set_tim_invis(p_ptr->tim_invis + randint(24));
				break;
			}

			case ILLUS_SHADOW_MONSTERS:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, 5, 30 + (plev), 2);
				fire_beam(GF_FORCE, dir,
				          damroll(6+((plev-5)/4), 8));
				break;
			}

			case ILLUS_SHADOW_BALL_II:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir,
				          40 + (plev), 2);
				break;
			}

			case ILLUS_LIFE_FOR_MANA:
			{
				int m = 0;
				m = get_quantity("How much mana?",999);
				if (!m) return;
				if (m < 0) return;
				if (m > p_ptr->chp)
				{
					msg_print("You don't have that much life!");
					return;
				}
				if ((p_ptr->csp + m) > p_ptr->msp)
				{
					m = p_ptr->msp - p_ptr->csp;
				}
				p_ptr->csp += m;
				take_hit(m, "spellcasting");
				msg_print("You convert life into mana.");
				break;
			}

			case ILLUS_SHADOW_GATE:
			{
				teleport_player(plev * 7);
				break;
			}

			case ILLUS_SUMMON_SHADOWS:
			{
				summon_monster(SUMMON_SHADOWS);
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
 * Pray a prayer
 */
static void do_cmd_pray_druid(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int y, x;

	int item, sval, spell, dir, chance;
	int beam;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


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
	item_tester_tval = cp_ptr->spell_book;

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
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

		switch (spell)
		{
			case DRUID_DETECT_EVIL:
			{
				(void)detect_monsters_evil();
				break;
			}

			case DRUID_CURE_WOUNDS:
			{
				(void)do_cure_wounds();
				break;
			}

			case DRUID_POISON_BOLT:
			{
 				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_POIS, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
				break;
			}

			case DRUID_REMOVE_FEAR:
			{
				(void)set_afraid(0);
				break;
			}

			case DRUID_CALL_LIGHT:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case DRUID_FIND_TRAPS:
			{
				(void)detect_traps();
				break;
			}

			case DRUID_DETECT_DOORS_STAIRS:
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case DRUID_SLOW_POISON:
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case DRUID_SLEEP:
			{
				(void)do_sleep_monster();
				break;
			}

			case DRUID_PORTAL:
			{
				teleport_player(plev * 3);
				break;
			}

			case DRUID_DUSTSTORM:
			{
				fire_ball(GF_FORCE, 5, 3 + (plev / 4), 6);
				break;
			}

			case DRUID_COOL_LAVA:
			{
				alter_terrain(py, px, plev, 1);
				break;
			}

			case DRUID_POISON_CLOUD:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 9 + (plev / 2), 3);
				break;
			}

			case DRUID_SATISFY_HUNGER:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case DRUID_REMOVE_CURSE:
			{
				remove_curse();
				break;
			}

			case DRUID_RESIST_HEAT_COLD:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

			case DRUID_LEVITATE:
			{
				(void)set_tim_levitate(p_ptr->tim_levitate + randint(50) + 50);
				break;
			}

			case DRUID_CHARM_MONSTER:
			{
				if (!get_aim_dir(&dir)) return;
				charm_monster(dir, plev);
				break;
			}

			case DRUID_DISINTEGRATE:
			{
				alter_terrain(py, px, plev, 3);
				break;
			}

			case DRUID_GROW_TREES:
			{
				alter_terrain(py, px, 1, 4);
				break;
			}

			case DRUID_PROTECTION_FROM_EVIL:
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			}

			case DRUID_SENSE_INVISIBLE:
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				break;
			}

			case DRUID_SENSE_SURROUNDINGS:
			{
				map_area();
				break;
			}

			case DRUID_LOWER_WATER:
			{
				alter_terrain(py, px, plev, 2);
				break;
			}

			case DRUID_TURN_ANIMALS:
			{
				(void)turn_animals();
				break;
			}

			case DRUID_FEAR:
			{
				(void)do_fear_monster();
				break;
			}

			case DRUID_CALL_WOLVES:
			{
				summon_monster(SUMMON_WOLVES);
				break;
			}

			case DRUID_HEAL:
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case DRUID_ANALYZE_MONSTER:
			{
				(void)probing();
				break;
			}

			case DRUID_GLYPH_OF_WARDING:
			{
				warding_glyph();
				break;
			}

			case DRUID_DETECTION:
			{
				(void)detect_all();
				break;
			}

			case DRUID_NEXUS_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_NEXUS, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case DRUID_DETECT_MONSTERS:
			{
				(void)detect_monsters_normal();
				break;
			}

			case DRUID_MAP_TERRAIN:
			{
				for(y = 0; y < DUNGEON_HGT; y++)
				{
					for(x = 0; x < DUNGEON_WID; x++)
					{
						if ((cave_feat[y][x] >= FEAT_DEEP_WATER) &&
						    (cave_feat[y][x] <= FEAT_MOUNTAIN))
						{
							cave_info[y][x] |= (CAVE_GLOW);
							cave_info[y][x] |= (CAVE_MARK);
						}
					}
				}
				/* Redraw map */
				p_ptr->redraw |= (PR_MAP);

				break;
			}

			case DRUID_PERCEPTION:
			{
				(void)ident_spell();
				break;
			}

			case DRUID_PROBING:
			{
				(void)probing();
				break;
			}

			case DRUID_CLAIRVOYANCE:
			{
				wiz_lite();
				break;
			}

			case DRUID_RESIST_NETHER:
			{
				(void)set_oppose_nethr(p_ptr->oppose_nethr + randint(20) + 20);
				break;
			}

			case DRUID_FEAR_II:
			{
				(void)do_fear_monster();
				break;
			}

			case DRUID_CURE_MORTAL_WOUNDS:
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case DRUID_FOREST:
			{
				flood(py, px, 20,2);
				break;
			}

			case DRUID_RESTORATION:
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case DRUID_FAITHFULNESS:
			{
				if (!get_aim_dir(&dir)) return;
				tame_monster(dir, plev);
				break;
			}

			case DRUID_FIRESTORM:
			{
				fire_ball(GF_FIRE, 5,
				    35 + (plev * 2), 10);
				break;
			}

			case DRUID_FISSURE:
			{
				(void) fissure(FEAT_WALL_EXTRA);
				(void) fissure(FEAT_WALL_EXTRA);
				break;
			}

			case DRUID_TORNADO:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FORCE, dir, 60 + (plev), 2);
				break;
			}

			case DRUID_EARTHQUAKE:
			{
				earthquake(py, px, 10);
				break;
			}

			case DRUID_FLOOD:
			{
				flood(py, px, 20,1);
				break;
			}

			case DRUID_NEXUS_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_NEXUS, dir,
				          70 + (plev), 2);
				break;
			}

			case DRUID_BALEFIRE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_BALEFIRE, dir,
				          damroll(15 + ((plev-5)/3), 10));
				break;
			}

			case DRUID_UNBARRING_WAYS:
			{
				(void)destroy_doors_touch();
				break;
			}

			case DRUID_RECHARGING:
			{
				(void)recharge((plev * 2));
				break;
			}

			case DRUID_ERRUPTIONS:
			{
				alter_terrain(py, px, plev, 5);
				break;
			}

			case DRUID_SUSTAIN_SELF:
			{
				(void)set_tim_sus_str(p_ptr->tim_sus_str + randint(50) + 50);
				(void)set_tim_sus_int(p_ptr->tim_sus_int + randint(50) + 50);
				(void)set_tim_sus_wis(p_ptr->tim_sus_wis + randint(50) + 50);
				(void)set_tim_sus_dex(p_ptr->tim_sus_dex + randint(50) + 50);
				(void)set_tim_sus_con(p_ptr->tim_sus_con + randint(50) + 50);
				(void)set_tim_sus_chr(p_ptr->tim_sus_chr + randint(50) + 50);
				break;
			}

			case DRUID_BRAND_AMMO:
			{
				(void)brand_ammo(2,0);
				break;
			}

			case DRUID_ELEMENTAL_BRAND:
			{
				brand_weapon();
				break;
			}

			case DRUID_IMPRISON:
			{
				if (!get_aim_dir(&dir)) return;
				imprision(FEAT_WALL_EXTRA, dir, p_ptr->lev/10);
				break;
			}

			case DRUID_BLINK:
			{
				teleport_player(10);
				break;
			}

			case DRUID_TELEPORT_SELF:
			{
				teleport_player(plev * 8);
				break;
			}

			case DRUID_TELEPORT_OTHER:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case DRUID_SUMMON_ANIMALS:
			{
				summon_monster(SUMMON_ANIMALS);
				summon_monster(SUMMON_ANIMALS);
				break;
			}

			case DRUID_WORD_OF_RECALL:
			{
				set_recall();
				break;
			}

			case DRUID_ALTER_REALITY:
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}

			case DRUID_DROWN:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_WATER, dir, 100 + (plev * 2), 3);
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
 * Cast a spell
 */
void do_cmd_cast(void)
{
	/* Require spell ability */
	if (cp_ptr->spell_book == TV_MAGIC_BOOK)
	{
		do_cmd_cast_spell();
		return;
	}
	else if (cp_ptr->spell_book == TV_ILLUSION_BOOK)
	{
		do_cmd_cast_illusion();
		return;
	}
	else
	{
		msg_print("You cannot cast spells!");
		return;
	}
}


/*
 * Pray a prayer
 */
void do_cmd_pray(void)
{
	/* Must use prayer books */
	if (cp_ptr->spell_book == TV_PRAYER_BOOK)
	{
		do_cmd_pray_priest();
		return;
	}
	else if (cp_ptr->spell_book == TV_NATURE_BOOK)
	{
		do_cmd_pray_druid();
		return;
	}
	else
	{
		msg_print("Pray hard enough and your prayers may be answered.");
		return;
	}
}


/*
 * Drop all items carried by a monster
 */
static void monster_drop_carried_objects(monster_type *m_ptr)
{
	s16b this_o_idx, next_o_idx = 0;
	object_type forge;
	object_type *o_ptr;
	object_type *q_ptr;


	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Paranoia */
		o_ptr->held_m_idx = 0;

		/* Get local object */
		q_ptr = &forge;

		/* Copy the object */
		object_copy(q_ptr, o_ptr);

		/* Delete the object */
		delete_object_idx(this_o_idx);

		/* Drop it */
		(void)drop_near(q_ptr, -1, m_ptr->fy, m_ptr->fx);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;
}


/*
 * Issue a pet command
 */
void do_cmd_pets(void)
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
	bool			all_pets = FALSE;
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

		if (m_ptr->is_friendly) pets++;
	}

	if (pets)
	{
		power_desc[num] = "dismiss pets";
		powers[num++] = PET_DISMISS;
	}

	power_desc[num] = "stay close";
	if (p_ptr->pet_follow_dist == PET_CLOSE_DIST) mode = num;
	powers[num++] = PET_STAY_CLOSE;

	power_desc[num] = "follow me";
	if (p_ptr->pet_follow_dist == PET_FOLLOW_DIST) mode = num;
	powers[num++] = PET_FOLLOW_ME;

	power_desc[num] = "seek and destroy";
	if (p_ptr->pet_follow_dist == PET_DESTROY_DIST) mode = num;
	powers[num++] = PET_SEEK_AND_DESTROY;

	power_desc[num] = "give me space";
	if (p_ptr->pet_follow_dist == PET_SPACE_DIST) mode = num;
	powers[num++] = PET_ALLOW_SPACE;

	power_desc[num] = "stay away";
	if (p_ptr->pet_follow_dist == PET_AWAY_DIST) mode = num;
	powers[num++] = PET_STAY_AWAY;

	if (p_ptr->pet_open_doors)
	{
		power_desc[num] = "pets may open doors";
	}
	else
	{
		power_desc[num] = "pets may not open doors";
	}
	powers[num++] = PET_OPEN_DOORS;

	if (p_ptr->pet_pickup_items)
	{
		power_desc[num] = "pets may pick up items";
	}
	else
	{
		power_desc[num] = "pets may not pick up items";
	}
	powers[num++] = PET_TAKE_ITEMS;

	/* Nothing chosen yet */
	flag = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
		strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
			I2A(0), I2A(num - 1));
	}
	else
	{
		strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
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
			bell(NULL);
			continue;
		}

		/* Verify it */
		if (ask)
		{
			/* Prompt */
			strnfmt(buf, 78, "Use %s? ", power_desc[i]);

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
		p_ptr->energy_use = 0;
		return;
	}

	switch (powers[i])
	{
		case PET_DISMISS: /* Dismiss pets */
		{
			int Dismissed = 0;

			if (get_check("Dismiss all pets? ")) all_pets = TRUE;

			/* Process the monsters (backwards) */
			for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
			{
				/* Access the monster */
				m_ptr = &m_list[pet_ctr];

				if (m_ptr->is_friendly)
				{
					bool delete_this = FALSE;

					if (all_pets)
						delete_this = TRUE;
					else
					{
						char friend_name[80], check_friend[80];
						monster_desc(friend_name, m_ptr, 0x80);
						sprintf(check_friend, "Dismiss %s? ", friend_name);

						if (get_check(check_friend))
							delete_this = TRUE;
					}

					if (delete_this)
					{
						delete_monster_idx(pet_ctr);
						Dismissed++;
					}
				}
			}

			msg_format("You have dismissed %d pet%s.", Dismissed,
				(Dismissed == 1 ? "" : "s"));
			break;
		}
		/* Call pets */
		case PET_STAY_CLOSE:
		{
			p_ptr->pet_follow_dist = PET_CLOSE_DIST;
			break;
		}
		/* "Follow Me" */
		case PET_FOLLOW_ME:
		{
			p_ptr->pet_follow_dist = PET_FOLLOW_DIST;
			break;
		}
		/* "Seek and destoy" */
		case PET_SEEK_AND_DESTROY:
		{
			p_ptr->pet_follow_dist = PET_DESTROY_DIST;
			break;
		}
		/* "Give me space" */
		case PET_ALLOW_SPACE:
		{
			p_ptr->pet_follow_dist = PET_SPACE_DIST;
			break;
		}
		/* "Stay away" */
		case PET_STAY_AWAY:
		{
			p_ptr->pet_follow_dist = PET_AWAY_DIST;
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

					if (m_ptr->is_friendly)
					{
						monster_drop_carried_objects(m_ptr);
					}
				}
			}

			break;
		}
	}
}

