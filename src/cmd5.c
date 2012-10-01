/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Large pieces of the code have been copied from Oangband by Leon Marrik */

#include "angband.h"

/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", "study", or "browse".
 * The "known" should be TRUE for cast/pray, FALSE for study
 */

static int get_spell(int *sn, cptr prompt, int book, bool known)
{
	int i;

	int spell = -1;

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn)) 
	{
		/* Verify the spell is okay */
		if (spell_okay(book, *sn, known)) 
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(book, i, known)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(spells %c-%c, *=List, ESC=exit) %^s which spell? ",
		I2A(0), I2A(count_spells(book)-1), prompt);

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
				print_spells(book, 1, 14);
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
		if ((i < 0) || (i >= MAX_BOOK_SPELLS))
		{
			bell("Illegal spell choice!");
			continue;
		}

		/* Convert spellbook number to spell index. */
		spell = i;

		/* Require "okay" spells */
		if (!spell_okay(book, spell, known))
		{
			bell("Illegal spell choice!");
			msg_format("You may not %s that spell.", prompt);
			continue;
		}

		/* Verify it */
		if (ver)
		{
			char tmp_val[160];

			/* Get the spell */
			s_ptr = &books[book].contents[i];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, s_ptr->sname,
			        spell_mana(book, spell), spell_chance(book, spell));

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
	}


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */
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
 *
 * Some code taken from Oangband 0.4.1 
 *
 */
void do_cmd_browse(void)
{
	int item, spell, lines;
	magic_type *s_ptr;
	object_type *o_ptr;

	cptr q = "";
	cptr s = "";

	/* Forbid illiterates to read spellbooks. */
	if (!literate())
	{
		msg_print("You cannot read books!");
		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_hook_spellbooks;

	/* Get an item */
	if (!get_item(&item, "Browse which book? ", "You have no books that you can read.", 
		(USE_INVEN | USE_FLOOR))) return;

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

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();

	/* Display the spells */
	print_spells(o_ptr->sval, 1, 14);

	/* Prompt for a command */
	put_str("(Browsing) Choose a spell, or ESC:", 0, 0);

	/* Hack - Determine how far from the top of the screen the spell list 
	 * extends by counting spells, and adding space for name, etc.
	 */
	lines = count_spells(o_ptr->sval);

	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "browse", o_ptr->sval, TRUE))
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Notify that there's nothing to see, and wait. */
			c_put_str(TERM_SLATE, "No spells to browse     ", 0, 11);

			/* Any key cancels if no spells are available. */
			if (inkey()) break;
		}				  

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(14, lines + 3, 255);

		/* Access the spell */
		s_ptr = &books[o_ptr->sval].contents[spell];

		/* Display that spell's information. */
		c_roff(TERM_L_BLUE, spell_tips[s_ptr->index]);
	}

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
	int i, item;

	magic_type *s_ptr;

	int spell = -1;

	object_type *o_ptr;

	/* Forbid illiterates to read spellbooks. */
	if (!literate())
	{
		msg_print("You cannot learn magic!");
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
		msg_print("You cannot learn any new spells.");
		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_hook_spellbooks;

	/* Get an item */
	if (!get_item(&item, "Study which book? ", "You have no books that you can read."
		, (USE_INVEN | USE_FLOOR))) return;

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

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* All but Priests -- Learn a selected spell */
	if (cp_ptr->flags & CF_CHOOSE_SPELLS)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", o_ptr->sval, FALSE) && (spell == -1)) return;
	}
	/* Priest -- Learn a random prayer */
	else
	{
		int k = 0;

		int gift = -1;

		/* Pick an legal, unknown prayer at random. */
		for (spell = 0; spell < MAX_BOOK_SPELLS; spell++)
		{
			/* Skip non "okay" prayers */
			if (!spell_okay(o_ptr->sval, spell, FALSE)) continue;

			/* Apply the randomizer */
			if ((++k > 1) && (rand_int(k) != 0)) continue;

			/* Track it */
			gift = spell;
		}

		/* Accept gift */
		spell = gift;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msg_print("You cannot learn any spells in that book.");

		/* Abort */
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Learn the spell */
	p_ptr->spell_learned[o_ptr->sval] |= (1L << spell);

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < (SV_MAX_BOOKS * MAX_BOOK_SPELLS); i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i][1] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i][0] = o_ptr->sval;
	p_ptr->spell_order[i][1] = spell;

	/* Access the spell */
	s_ptr = &books[o_ptr->sval].contents[spell];

	/* Mention the result */
	msg_format("You have learned the spell of %s.",
	           s_ptr->sname);

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more spell%s.", p_ptr->new_spells, 
			(p_ptr->new_spells != 1)  ? "s" : "");
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);

}


/*
 * Cast a spell or pray a prayer.
 */
void do_cmd_cast_or_pray(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, spell, dir;
	int chance, beam;
	int time;
	int shape = 0;

	int plev = ((cp_ptr->flags & CF_POWER) 
		? p_ptr->lev + (p_ptr->lev / 2) : p_ptr->lev);

	object_type *o_ptr;

	magic_type *s_ptr;

	if (!literate())
	{
		msg_print("You know no magic!");
		return;
	}

	if (!cp_ptr->flags & CF_MYSTIC_CAST)
	{
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
	}

	/* Restrict choices to spell books of the player's realm. */
	if ((cp_ptr->flags & CF_MYSTIC_CAST) && (p_ptr->blind || no_lite() || p_ptr->confused))
	{
		item_tester_hook = item_tester_hook_mysticbooks;
	}
	else item_tester_hook = item_tester_hook_spellbooks;

	/* Get an item */
	if (!get_item(&item, "Use which book? ", "You have no books that you can use.", 
		(USE_INVEN | USE_FLOOR))) return;

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

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Ask for a spell */
	if (!get_spell(&spell, "cast", o_ptr->sval, TRUE))
	{
		if (spell == -2) 
		{
			msg_print("You don't know any spells in that books.");
		}
		return;
	}

	/* Access the spell */
	s_ptr = &books[o_ptr->sval].contents[spell];

	/* Verify "dangerous" spells */
	if (spell_mana(o_ptr->sval, spell) > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(o_ptr->sval, spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		switch (books[o_ptr->sval].flags & SBF_TYPE_MASK) 
		{
			case SBF_MAGIC:
			{
				msg_print("You fail to tap onto the necessary magical forces!");
				break;
			}
			case SBF_PRAYER:
			{
				msg_print("Your prayer was left unanswered!");
				break;
			}
			case SBF_MYSTIC:
			{
				msg_print("You lose your concentration!");
				break;
			}
			case SBF_CODEX:
			{
				msg_print("Your mind is overwhelmed by the magnitute of ancient mystery!");
				/* Lose your spell-casting stats */
				if (rand_int(100) < chance) 
				{ 
					if (rand_int(2) == 0) dec_stat(cp_ptr->spell_stat1, rand_int(10)+20, TRUE);
					else dec_stat(cp_ptr->spell_stat2, rand_int(10)+20, TRUE);
				}
				/* Lose your memories */
				lose_all_info();
				break;
			}
			case SBF_NECRONOM:
			{
				msg_print("You lost your grasp on the evil powers that you had sought to control!");
				/* Summon some horrors */
				summon_specific(py, px, p_ptr->depth+10, SUMMON_HORROR);
				/* Darkness */
				if (!p_ptr->resist_blind)
				{
					set_blind(p_ptr->blind + 3 + randint(5));
				}
				unlite_area(10, 3);
				/* Lose EXP */
				if (p_ptr->hold_life && (rand_int(100) < (100-chance)))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(200 + (p_ptr->exp/500) * MON_DRAIN_LIFE);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(200 + (p_ptr->exp/50) * MON_DRAIN_LIFE);
				}				
				break;
			}
		}

	}

	/* Process spell */
	else
	{
		/* Hack -- higher chance of "beam" instead of "bolt" for mages */
		beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));



		/* Spell Effects.  Spells are mostly listed by realm, each using a 
		 * block of 64 spell slots, but there are a certain number of spells 
		 * that are used by more than one realm (this allows more neat class-
		 * specific magics)
		 */
		switch (s_ptr->index)
		{
			/* Sorcerous Spells */
		
			case 1: /* Magic Missile */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
				break;
			}

			case 2: /* Phase Door */
			{
				teleport_player(10);
				break;
			}

			case 3: /* Light Area */
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 4: /* Detect Monsters */
			{
				(void)detect_monsters_normal();
				break;
			}

			case 5: /* Stinking Cloud */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + (plev / 2), 2);
				break;
			}

			case 6: /* Confuse Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				break;
			}

			case 7: /* Find Hidden Traps/Doors */
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 8: /* Absorb Hit */
			{
				(void)set_absorb(p_ptr->absorb + randint(25) + plev);
				break;
			}

			case 9: /* Lightning Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				break;
			}

			case 10: /* Trap/Door Destruction */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 11: /* Sleep Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			}

			case 12: /* Frost Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				break;
			}

			case 13: /* Teleport Self */
			{
				teleport_player(plev * 5);
				break;
			}

			case 14: /* Spear of Light */
			{ 
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir,damroll(9,8));
				break;
			}

			case 15: /* Turn Stone to Mud */
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 16: /* Sleep All */
			{
				(void)sleep_monsters();
				break;
			}

			case 17: /* Satisfy Hunger */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 18: /* Lesser Recharge Item */
			{
				(void)recharge(5);
				break;
			}

			case 19: /* Acid Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case 20: /* Identify */
			{
				(void)ident_spell();
				break;
			}

			case 21: /* Door Creation */
			{
				(void)door_creation();
				break;
			}

			case 22: /* Detect Enchantment */
			{
				(void)detect_objects_magic();
				break;
			}

			case 23: /* Fire Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case 24: /* Slow Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case 25: /* Frost Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          30 + (plev), 2);
				break;
			}

			case 26: /* Recharge Item */
			{
				(void)recharge(40);
				break;
			}

			case 27: /* Teleport Other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 28: /* Fire Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          55 + (plev), 2);
				break;
			}

			case 29: /* Haste Self */
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

			case 30: /* Word of Destruction */
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 31: /* Genocide */
			{
				(void)genocide();
				break;
			}

			case 32: /* Resist Fire */
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case 33: /* Resist Cold */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				break;
			}

			case 34: /* Resist Acid & Electricity */
			{
				time = randint(20) + 20;
				(void)set_oppose_acid(p_ptr->oppose_acid + time);
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				break;
			}

			case 35: /* Resist Poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 36: /* Resistance */
			{
				time = randint(20) + 20;
				(void)set_oppose_acid(p_ptr->oppose_acid + time);
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				(void)set_oppose_fire(p_ptr->oppose_fire + time);
				(void)set_oppose_cold(p_ptr->oppose_cold + time);
				(void)set_oppose_pois(p_ptr->oppose_pois + time);
				break;
			}

			case 37: /* Stair Creation */
			{
				(void)stair_creation();
				break;
			}

			case 38: /* Teleport Level */
			{
				(void)teleport_player_level();
				break;
			}

			case 39: /* Dimension Door. */
			{
				msg_print("Choose a location to teleport to.");
				msg_print(NULL);
				dimen_door();
				break;
			}

			case 40: /* Earthquake */
			{
				earthquake(py, px, 10);
				break;
			}

			case 41: /* Word of Recall */
			{
				set_recall();
				break;
			}

			case 42: /* Detect Evil */
			{
				(void)detect_monsters_evil();
				break;
			}

			case 43: /* Polymorph Other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}

			case 44: /* Greater Recharge Item */
			{
				recharge(100);
				break;
			}

			case 45: /* Mass Genocide */
			{
				(void)mass_genocide();
				break;
			}

			case 46: /* Heroism */
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 47: /* Shield */
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

			case 48: /* Temporary Invisibility */
			{
				if (!p_ptr->tim_invis)
				{
					(void)set_tim_invis(randint(25) + 25 + plev);
				}
				else
				{
					(void)set_tim_invis(p_ptr->tim_invis + randint(20));
				}
				break;
				break;
			}

			case 49: /* Essence of Speed */
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

			case 50: /* Globe of Resilience */
			{
				(void)set_resilient(p_ptr->resilient + randint(8) + 8);
				break;
			}

			case 51: /* Mana Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_MANA, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case 52: /* Cloud Kill */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          20 + (plev / 2), 3);
				break;
			}

			case 53: /* Acid Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          40 + (plev), 2);
				break;
			}

			case 54: /* Ice Storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          70 + (plev), 3);
				break;
			}

			case 55: /* Meteor Swarm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
				          65 + (plev), 3);
				break;
			}

			case 56: /* Mana Storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          300 + (plev * 2), 3);
				break;
			}

			/* Priest spells */

			case 57: /* Cure Light Wounds */
			{
				(void)hp_player(damroll(2, 10));
				(void)set_cut(p_ptr->cut - 10);
				break;
			}

			case 58: /* Bless */
			{
				(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				break;
			}

			case 59: /* Remove Fear */
			{
				(void)set_afraid(0);
				break;
			}

			case 60: /* Find Traps */
			{
				(void)detect_traps();
				break;
			}

			case 61: /* Detect Doors/Stairs */
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 62: /* Slow Poison */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 63: /* Scare Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				break;
			}

			case 64: /* Portal */
			{
				teleport_player(plev * 3);
				break;
			}

			case 65: /* Cure Serious Wounds */
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case 66: /* Chant */
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				break;
			}

			case 67: /* Sanctuary */
			{
				(void)sleep_monsters_touch();
				break;
			}

			case 68: /* Remove Curse */
			{
				remove_curse();
				break;
			}

			case 69: /* Resist Heat and Cold */
			{
				time = randint(10) + 10;
				(void)set_oppose_fire(p_ptr->oppose_fire + time);
				(void)set_oppose_cold(p_ptr->oppose_cold + time);
				break;
			}

			case 70: /* Neutralize Poison */
			{
				(void)set_poisoned(0);
				break;
			}

			case 71: /* Orb of Draining */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          (damroll(3, 6) + plev +
				           (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4))),
				          (((plev >= 30) && (cp_ptr->flags & CF_BLESS_WEAPON)) ? 3 : 2));
				break;
			}

			case 72: /* Cure Critical Wounds */
			{
				(void)hp_player(damroll(6, 10));
				(void)set_cut(0);
				break;
			}

			case 73: /* Sense Invisible */
			{
				(void)set_tim_see_invis(p_ptr->tim_see_invis + randint(24) + 24);
				break;
			}

			case 74: /* Protection from Evil */
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * plev);
				break;
			}

			case 75: /* Sense Surroundings */
			{
				map_area();
				break;
			}

			case 76: /* Cure Mortal Wounds */
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 77: /* Turn Undead */
			{
				(void)turn_undead();
				break;
			}

			case 78: /* Prayer */
			{
				(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				break;
			}

			case 79: /* Dispel Undead */
			{
				(void)dispel_undead(randint(plev * 3));
				break;
			}

			case 80: /* Heal */
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 81: /* Dispel Evil */
			{
				(void)dispel_evil(randint(plev * 3));
				break;
			}

			case 82: /* Glyph of Warding */
			{
				warding_glyph();
				break;
			}

			case 83: /* Holy Word */
			{
				(void)dispel_evil(randint(plev * 4));
				(void)hp_player(1000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 84: /* Teleport Self */
			{
				teleport_player(plev * 8);
				break;
			}

			case 85: /* Alter Reality */
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}

			case 86: /* Detection */
			{
				(void)detect_all();
				break;
			}

			case 87: /* Probing */
			{
				(void)probing();
				break;
			}

			case 88: /* Clairvoyance */
			{
				wiz_lite();
				break;
			}

			case 89: /* Cure Serious Wounds */
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut(0);
				break;
			}

			case 90: /* Healing */
			{
				(void)hp_player(2000);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 91: /* Restoration */
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 92: /* Remeberance */
			{
				(void)restore_level();
				break;
			}

			case 93: /* Recharging */
			{
				(void)recharge(15);
				break;
			}

			case 94: /* Dispel Curse */
			{
				(void)remove_all_curse();
				break;
			}

			case 95: /* Enchant Weapon */
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

			case 96: /* Enchant Armour */
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

			case 97: /* Sanctify Shots */
			{
				(void)brand_weapon(TV_SHOT,EGO_HURT_EVIL,FALSE);
				break;
			}

			case 98: /* Dispel Undead */
			{
				(void)dispel_undead(randint(plev * 4));
				break;
			}

			case 99: /* Dispel Evil */
			{
				(void)dispel_evil(randint(plev * 4));
				break;
			}

			case 100: /* Banishment */
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			}

			case 101: /* Annihilation */
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 200);
				break;
			}

			/* Artifact spells */
 
			case 102: /* Self Knowledge */
			{
				msg_print("You begin to know yourself a little better...");
				msg_print(NULL);
				self_knowledge();
				break;
			}

			case 103: /* Revelation */
			{
				identify_fully();
				break;
			}

			case 104: /* Wave of Evil */
			{
				dispel_non_evil(randint(plev * 5));
				break;
			}

			case 105: /* Ray of Evil */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(100, GF_NETHER, dir,
				                  damroll((8 * plev), 4));				
				break;
			}

			/* Mystic spells */

			case 106: /* Clarity of Mind */
			{
				(void)set_stun(0);
				(void)set_blind(0);
				(void)set_afraid(0);
				(void)set_confused(0);
				break;
			}

			case 107: /* Mastery of Elements */
			{
				time = randint(plev/2) + plev/2;
				(void)set_oppose_acid(p_ptr->oppose_acid + time);
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				(void)set_oppose_fire(p_ptr->oppose_fire + time);
				(void)set_oppose_cold(p_ptr->oppose_cold + time);
				break;
			}

			case 108: /* Control of the body */
			{
				(void)set_poisoned(0);
				(void)set_food(PY_FOOD_MAX - 1);
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 109: /* One with the World */
			{
				(void)set_oppose_all(randint(plev/3) + plev/3);
				break;
			}

			case 110: /* Hunter's Arrows & Bolts */
			{	
				(void)brand_weapon(TV_ARROW,EGO_HURT_ANIMAL,TRUE);
				break;
			}
			case 111: /* Enchant Arrows & Bolts */
			{	
				(void)brand_weapon(TV_ARROW,EGO_WOUNDING,TRUE);
				break;
			}
			case 112: /* Elemental Arrows & Bolts */
			{	
				/* Hack - choose random brand (not really time but the variable's there)*/
				time = rand_int(4);
				(void)brand_weapon(TV_ARROW,EGO_AMMO_ACID+time,TRUE);
				break;
			}
		}


		/* A spell was cast or a prayer prayed */
		if (!(p_ptr->spell_worked[o_ptr->sval] & (1L << spell)))
		{
			int e = s_ptr->sexp;

			/* The spell or prayer worked */
			p_ptr->spell_worked[o_ptr->sval] |= (1L << spell);

			/* Gain experience */
			gain_exp(e * (s_ptr->slevel+(cp_ptr->spell_handicap[o_ptr->sval]-1)));
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (spell_mana(o_ptr->sval, spell) <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell_mana(o_ptr->sval, spell);
	}

	/* Over-exert the player */
	else
	{
		int oops = spell_mana(o_ptr->sval, spell) - p_ptr->csp;

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
 * Cast a spell or pray a prayer.
 */
void do_cmd_use_racial(void)
{

	int dir;

	if ((!rp_ptr->special) || (rsp_ptr[p_ptr->max_lev/5]->power == 0))
	{
		msg_print("You have no racial powers!");
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Not if still recharging */
	if (p_ptr->racial_power)
	{
		msg_print("You have to collect your energy");
		return;
	}

	switch (rsp_ptr[p_ptr->max_lev/5]->power)
	{
		/*Angel Powers*/
		case 1: /* Cherub - Detect Evil */
		{
			(void)detect_monsters_evil();
			p_ptr->racial_power=50;
			break;
		}			

		case 2: /* Seraph - Light area */
		{
			(void)lite_area(damroll(2,2),2);
			p_ptr->racial_power=50;
			break;
		}			

		case 3: /* Deva/Planeter - Spear of Light */
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir,damroll(6,8));
			p_ptr->racial_power=25;
			break;
		}			

		case 4: /* Archon - Orb of Draining */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_HOLY_ORB, dir, damroll(3, 6)+50, 3);
			p_ptr->racial_power=15;
			break;
		}			

		case 5: /* Angel/Archangel - Protection From Evil */
		{
			(void)set_protevil(p_ptr->protevil + randint(25) + 30);
			p_ptr->racial_power=150;
			break;
		}	
		
		/*Demon Powers*/

		case 6: /* Tengu - Blink */
		{
			teleport_player(10);
			p_ptr->racial_power=20;
			break;
		}

		case 7: /* Bodak/Vrock/Hezrou - Fire Bolt */
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(0, GF_FIRE, dir, damroll(10, 8));
			p_ptr->racial_power=20;
			break;
		}			

		case 8: /* Glabrezu/Nalfeshnee/Pit Fiend - Fire Ball */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir, 85, 2);
			p_ptr->racial_power=20;
			break;
		}			

		case 9: /* Balrog - Plasma ball */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_PLASMA, dir, damroll(4, 7)+80, 3);
			p_ptr->racial_power=20;
			break;
		}			

	}

}
