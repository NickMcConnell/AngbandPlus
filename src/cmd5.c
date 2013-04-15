/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "animeband.h"



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

	cptr ninja = "ninjutsu";

	cptr p = ((cp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

	if (cp_ptr->spell_book == TV_LARGE_SCROLL)
	{
		p = ninja;
	}

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
 * Study a parchment to gain a new spell/prayer
 */
void do_cmd_study_chi_warrior(void)
{
	int item, sval;

	cptr p = "power";

	cptr q, s;

	object_type *o_ptr;


	
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
	


	/* Restrict choices to "useful" books */
	item_tester_tval = TV_PARCHMENT;

	/* Get an item */
	q = "Study which parchment? ";
	s = "You have no parchments that you can read.";
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

	/* Did we learn this already? */
	if (p_ptr->chi_powers[sval] == TRUE)
	{
		msg_print("You have already learned this power.");
		return;
	}

	/* Otherwise - put it in the list */
	p_ptr->chi_powers[sval] = TRUE;
	msg_format("You have learned the power of %s.", chi_warrior_powers[sval]);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item was tried */
	object_tried(o_ptr);

	
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);


	
	/* Destroy a parchment in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a parchment on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}


	/* Take a turn */
	p_ptr->energy_use = 100;


	/* Hack -- Handle stuff */
	handle_stuff();

	
}

static char head[3] =
{ 'a', 'A', '0' };

void swap_which_power(int howmany)
{

	int i, num; 
	int col, row;
	int idx;
	
	char ch;


	if (howmany == 0){
		msg_print("You have no powers to swap in.");
		return;
	}
		

	/* Clear screen */
	Term_clear();
	

	/* Print all powers */
	
	for (num = 0; num < CHI_WARRIOR_MAX; num++)
	{
		if (p_ptr->chi_powers[num] == TRUE){
		row = 2 + (num % 20);
		col = 30 * (num / 20);
		ch = head[num/20] + (num%20);
		prt(format("[%c] %s", ch, chi_warrior_powers[num]), row, col);
		}
	
	}

	/* Choose! */
	if (!get_com("Swap in which power? ", &ch)) return;

	/* Do we have this power? */
	/* Analyze choice */
	num = -1;
	if ((ch >= head[0]) && (ch < head[0] + 20)) num = ch - head[0];
	if ((ch >= head[1]) && (ch < head[1] + 20)) num = ch - head[1] + 20;
	if ((ch >= head[2]) && (ch < head[2] + 17)) num = ch - head[2] + 40;

	/* Bail out if choice is illegal */
	if ((num < 0) || (num >= CHI_WARRIOR_MAX) || (!(p_ptr->chi_powers[num]))) return;

	/* Base case - No current powers */
	if (p_ptr->max_powers == 0)
	{
		p_ptr->max_powers++;
		p_ptr->player_powers[0] = num;
		msg_format("You have learned the technique of %s.", chi_warrior_powers[num]);
		return;
	}

	/* Second case -- do we have this power already? */
	
	for (i = 0; i < p_ptr->max_powers; i++)
	{
		if (p_ptr->player_powers[i] == num)
		{
			msg_print("You already have this power.");
			return;
		}
	}

	/* Third case -- is max_powers < STUDENT_MAX?) */
	if (p_ptr->max_powers < STUDENT_MAX)
	{
		/* Queue at end */
		p_ptr->player_powers[p_ptr->max_powers] = num;
		p_ptr->max_powers++;		
		msg_format("You have learned the technique of %s.", chi_warrior_powers[num]);
		return;
	}

	/* Else, gotta choose something */
	/* Clear screen */
	Term_clear();
	/* Print all powers */
	
	for (idx = 0; idx < p_ptr->max_powers; idx++)
	{
		row = 2 + (idx % 20);
		col = 30 * (idx / 20);
		ch = head[idx/20] + (idx%20);
		prt(format("[%c] %s", ch, chi_warrior_powers[p_ptr->player_powers[idx]]), row, col);
			
	}

	/* Choose! */
	if (!get_com("Swap for what? ", &ch)) return;
	/* Analyze choice */
	idx = -1;
	if ((ch >= head[0]) && (ch < head[0] + 20)) idx = ch - head[0];
	if ((ch >= head[1]) && (ch < head[1] + 20)) idx = ch - head[1] + 20;
	if ((ch >= head[2]) && (ch < head[2] + 17)) idx = ch - head[2] + 40;

	/* Bail out if choice is illegal */
	if ((idx < 0) || (idx >= p_ptr->max_powers)) return;

	msg_print("Swapped.");
	p_ptr->player_powers[idx] = num;
	
}


/*
 * Swap in a power
 */
void do_cmd_swap_chi_warrior(void)
{
	int howmany, counter;


	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* 1st case - Find out how many powers you have */
	howmany = 0;
	for (counter = 0; counter < CHI_WARRIOR_MAX; counter++)
	{
		if(p_ptr->chi_powers[counter])
			howmany++;
	}

	if (howmany == 0)
	{
		msg_print("You don't have any powers to swap in");
		return;
	}


	/* 2nd case - Choose a power to swap/queue in */
	/* Save screen */
	screen_save();

	/* Swap powers */
	swap_which_power(howmany);

	/* Load screen */
	screen_load();


		
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

	if (cp_ptr->spell_book == TV_LARGE_SCROLL)
	{
		p = "ninjutsu";

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
	if ((p_ptr->new_spells) && (!(cp_ptr->flags & CF_NO_STUDY)))
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
	int chance, beam, amt;
	int wrath;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if (cp_ptr->spell_book != TV_MAGIC_BOOK)
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
		if (spell == -2) msg_print("You don't know any spells from that book.");
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
				amt = damroll(2,8);

			if (amt < (p_ptr->mhp / 10))
			{
				amt = p_ptr->mhp / 10;
			}
				(void)hp_player(amt);
				(void)set_cut(p_ptr->cut - 15);
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
				(void)sleep_monster(dir);
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
				(void)recharge(5);
				break;
			}

			case SPELL_SLEEP_II:
			{
				(void)sleep_monsters_touch();
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

			case SPELL_SLEEP_III:
			{
				(void)sleep_monsters();
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

			case SPELL_SHADOW_FLARE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir,
				          30 + (plev), 2);
				break;
			}

			case SPELL_RECHARGE_ITEM_II:
			{
				(void)recharge(40);
				break;
			}

			case SPELL_TELEPORT_OTHER:
			{
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

			case SPELL_STAR_FLARE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_LITE, dir,
				          55 + (plev), 2);
				break;
			}

			case SPELL_WORD_OF_DESTRUCTION:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case SPELL_SOUTHERN_CROSS:
			{
				fire_beam(GF_METEOR, 2,
				          80 + (plev));
				fire_beam(GF_METEOR, 4,
				          80 + (plev));
				fire_beam(GF_METEOR, 6,
				          80 + (plev));
				fire_beam(GF_METEOR, 8,
				          80 + (plev));
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
				(void)teleport_player_level();
				break;
			}

			case SPELL_SMOKE_BOMB:
			{
				fire_ball(GF_CONFUSION, 0, 0, 10);
				fire_ball(GF_SOUND, 0, 0, 10);
				break;
			}

			case SPELL_WORD_OF_RECALL:
			{
				set_recall();
				break;
			}

			case SPELL_ACID_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case SPELL_FIRE_BALL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
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

			case SPELL_RAH_TILT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_MANA, dir,
				                  (plev) * 8);
				break;
			}

			case SPELL_DRAGON_SLAVE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
				          (plev) * 10, 3);
				break;
			}

			case SPELL_GIGA_SLAVE:
			{
				if (!get_aim_dir(&dir)) return;
				
				if (rand_int(100) < 10){ /* 10% of nuking the Universe */

				msg_print("You were unable to contain the Giga Slave!");
				take_hit(4500, "invoking a Giga Slave");

				}

				else fire_ball(GF_MANA, dir,
				          4000 + (plev * 10), 12);
				break;
			}

			case SPELL_DETECT_EVIL:
			{
				(void)detect_monsters_evil();
				break;
			}

			case SPELL_DETECT_ENCHANTMENT:
			{
				(void)detect_objects_magic();
				break;
			}

			case SPELL_RECHARGE_ITEM_III:
			{
				recharge(100);
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

			case SPELL_FOEHN:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FORCE, dir,
				          (plev * 2), 12);
				break;
			}

			case SPELL_NOAH:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_WATER, dir,
				          (plev * 2) + 20, 12);
				break;
			}

			case SPELL_PROTECTION:
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

			case SPELL_EARTHQUAKE:
			{
				earthquake(py, px, 12);
				break;
			}

			case SPELL_ANTI:
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

			case SPELL_KAMEHAMEHA:
			{
				msg_print("You scream out 'Kamehameha'!");
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_MANA, dir,
				          (plev * 3));
				break;
			}

			case SPELL_BERSERKER:
			{
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
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

			case SPELL_WRATH:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("You scream as you rapidly fling ki bolts!");
				for (wrath = 0; wrath < plev; wrath++)
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				                  damroll(3, 8));
				msg_print("You become tired from the effort.");
				(void)set_slow(p_ptr->slow + rand_int(10) + 4);
				/*(void)set_invuln(p_ptr->invuln + randint(8) + 8); Original GOI */
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
	/* Hack -- Sanjiyan consumes no mana during transform */
	if ((s_ptr->smana <= p_ptr->csp) && (!(p_ptr->wu_transform)))
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
 * Do a Ninjutsu
 */
void do_cmd_ninjutsu(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir;
	int chance, beam;


	int plev = p_ptr->lev;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if (cp_ptr->spell_book != TV_LARGE_SCROLL)
	{
		msg_print("You cannot use ninja techniques!");
		return;
	}

	/* Ninjas don't need light to form seals :) */
	/* Require lite 
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	} */

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Restrict choices to spell books */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Use which scroll? ";
	s = "You have no ninja scrolls!";
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
	if (!get_spell(&spell, "use", sval, TRUE))
	{
		if (spell == -2) msg_print("You don't know any ninjutsus from that scroll.");
		return;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough chakra to use this ninjutsu.");

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
		msg_print("You failed to get the ninjutsu off!");
	}

	/* Process spell */
	else
	{
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

		/* Spells. */
		switch (spell)
		{
			case NINJA_BASIC_REPLACEMENT:
			{
				msg_print("You get a log ready.");
				p_ptr->replacement = TRUE;
				break;
			}

			case NINJA_BASIC_BLINK:
			{
				teleport_player(10);
				break;
			}

			case NINJA_BASIC_TRANSFORM:
			{
				do_cmd_mimic();
				break;
			}
			

			case NINJA_BASIC_BUNSHIN:
			{
				(void)set_double_team(p_ptr->double_team + plev + 50);
				break;
			}

			case NINJA_BASIC_DISPEL:
			{
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
				break;
			}

			case NINJA_BASIC_SMOKE_BOMBS:
			{
				fire_ball(GF_CONFUSION, 0, 0, 10);
				fire_ball(GF_SOUND, 0, 0, 10);
				break;
			}

			case NINJA_BASIC_HASTE:
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

			case NINJA_BASIC_SHURIKEN_BUNSHIN:
			{
				do_cmd_shuriken_bunshin();
				break;
			}

			case NINJA_BASIC_KANCHO:
				{
					msg_print("Heh heh heh.....");
					p_ptr->kancho = TRUE;
					break;
				}

			case NINJA_KATON_RESIST_FIRE:
			{
			(void)set_oppose_fire(p_ptr->oppose_fire + 20 + rand_int(20));
				break;
			}

			case NINJA_KATON_FIRE_BOLT:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case NINJA_KATON_GOUKAKYUU:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Katon!  Goukakyuu no jutsu!");
				fire_ball(GF_FIRE, dir,
				          20 + (plev / 2), 3);
				break;
			}

			case NINJA_KATON_RYUUKA:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Katon!  Ryuuka no jutsu!");
				fire_beam(GF_FIRE, dir, 100 + (plev / 2));
				break;
			}

			case NINJA_KATON_KARYUU_ENDAN:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Katon!  Karyuu Endan!");
				fire_beam(GF_FIRE, dir, 100 + (plev / 2));
				fire_beam(GF_FIRE, dir, 150 + (plev / 2));
				fire_beam(GF_FIRE, dir, 200 + (plev / 2));
				break;
			}

			case NINJA_KATON_HELLFIRE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          400 + (plev / 2), 8);
				break;
			}

			case NINJA_SUITON_SUIKODAN:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Suiton!  Suikodan!");
				fire_bolt_or_beam(beam, GF_WATER, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case NINJA_SUITON_TEPPOUDAMA:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Suiton!  Teppoudama!");
				fire_ball(GF_WATER, dir,
				          20 + (plev / 2), 3);
				break;
			}

			case NINJA_SUITON_SUIRYUUDAN:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Suiton!  Suiryuudan no jutsu!");
				fire_bolt_or_beam(beam-10, GF_WATER, dir,
				                  damroll(16+((plev-5)/4), 8));
				break;
			}

			case NINJA_SUITON_SUIJINHEKI:
			{
				msg_print("Suiton!  Suijinheki!");
					fire_ball(GF_WATER, 0,
				          plev * 8, 1);
				break;
			}

			case NINJA_SUITON_DAIBAKUFU:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Suiton!  Daibakufu!");
				fire_beam(GF_WATER, dir, 100 + (plev / 2));
				fire_beam(GF_GRAVITY, dir, 0);
				break;
			}

			case NINJA_SUITON_SUISHOUHA:
			{
				msg_print("Suiton!  Suishouha!");
				for (dir = 1; dir < 10; dir++){
				fire_beam(GF_WATER, dir,
				           250 + plev);
				fire_beam(GF_GRAVITY, dir, damroll(plev, 2));
					}
				break;
			}

			case NINJA_DOTON_REPLACEMENT:
			{
				msg_print("Your get a replacement ready.");
				 p_ptr->replacement = 2; 
				break;
			}

			case NINJA_DOTON_YOMI_NUMA:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Doton!  Yomi Numa!");
				fire_ball(GF_YOMI_NUMA, dir,
				          0, 1 + (plev/10));
				break;
			}

			case NINJA_DOTON_DORYUU_TAIGA:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Doton!  Doryuu Taiga!");
				fire_beam(GF_GRAVITY, dir, 0);
				break;
			}

			case NINJA_DOTON_DORYUUDAN:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Doton!  Doryuudan!");
				fire_bolt_or_beam(beam, GF_MISSILE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case NINJA_DOTON_DORYUUHEKI:
			{
				msg_print("Doton!  Doryuuheki!");
				(void)wall_creation();
				break;
			}

			case NINJA_DOTON_DORYOU_DANGO:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("Doton!  Doryou dango!");
				fire_beam(GF_DASH, dir, 0);
				break;
			}

			case NINJA_FUUTON_DUST:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir,
				          30 + (plev), 2);
				break;
			}

			case NINJA_FUUTON_WIND_BLADE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_WATER, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case NINJA_FUUTON_TELEPORT:
			{
				teleport_player(plev * 8);
				break;
			}

			case NINJA_FUUTON_ZANKUUHA:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_FORCE, dir,
				          damroll(12+((plev-5)/4), 8));
				break;
			}

			case NINJA_FUUTON_KAMAITACHI:
			{
				if (!get_aim_dir(&dir)) return;
					fire_beam(GF_FORCE, dir,
				          damroll(14+((plev-5)/4), 8));
					fire_beam(GF_GRAVITY, dir, 0);
				break;
			}

			case NINJA_FUUTON_RENKYUUDAN:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FORCE, dir,
				          damroll(14+((plev-5)/4), 8), 8);
				break;
			}

			case NINJA_ADVANCED_FIND_TRAPS_DOORS:
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case NINJA_ADVANCED_HEROISM:
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case NINJA_ADVANCED_SATISFY_HUNGER:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case NINJA_ADVANCED_IDENTIFY:
			{
				(void)ident_spell();
				break;
			}

			case NINJA_ADVANCED_WORD_OF_RECALL:
			{
				set_recall();
				break;
			}

			case NINJA_ADVANCED_STONE_TO_MUD:
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case NINJA_ADVANCED_DETECT_EVIL:
			{
				(void)detect_monsters_evil();
				break;
			}

			case NINJA_BLOODLINE_SHARINGAN:
			{
				if (p_ptr->sharingan)
				{
					msg_print("You stop using the Sharingan.");
					p_ptr->sharingan = FALSE;
					return;
				}

				if (p_ptr->byakugan)
				{
					msg_print("You stop using the Byakugan.");
					p_ptr->byakugan = FALSE;
				}

				p_ptr->sharingan = TRUE;;
				msg_print("Sharingan!");

				break;
			}

			case NINJA_BLOODLINE_BYAKUGAN:
			{
				if (p_ptr->byakugan)
				{
					msg_print("You stop using the Byakugan.");
					p_ptr->byakugan = FALSE;
					return;
				}

				if (p_ptr->sharingan)
				{
					msg_print("You stop using the Sharingan.");
					p_ptr->sharingan = FALSE;
				}

				p_ptr->byakugan = TRUE;
				wiz_lite();
				msg_print("Byakugan!");

				break;
			}

			case NINJA_BLOODLINE_KAITEN:
			{

				if (!(p_ptr->byakugan))
				{
					msg_print("The Byakugan is required for this jutsu.");
					return;
				}
				
				msg_print("You start spinning!");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, 0 , plev * 8, (plev / 4));
				break;
			}

			case NINJA_BLOODLINE_HAKKE:
			{
				if (!(p_ptr->byakugan))
				{
					msg_print("The Byakugan is required for this jutsu.");
					return;
				}

				if (p_ptr->chidori == TRUE)
				{
					msg_print("You dissipate your Chidori.");
					p_ptr->chidori = FALSE;

				}

				if (p_ptr->jyuken == TRUE)
				{
					msg_print("You are already in a Jyuken stance!");
					return;
				}

				if (p_ptr->jyuken2 == TRUE)
				{
					msg_print("You change your stance.");
					p_ptr->jyuken2 = FALSE;
				}

				if (p_ptr->rasengan == TRUE)
				{
					msg_print("You disperse your Rasengan.");
					p_ptr->rasengan = FALSE;

				}

				msg_print("You assume a Jyuken stance.");
				p_ptr->jyuken = TRUE;
								
				break;
			}

			case NINJA_BLOODLINE_MANGEKYOU:
			{
				if (!(p_ptr->sharingan))
				{
					msg_print("The Sharingan is required for this jutsu.");
					return;
				}

				if (!get_aim_dir(&dir)) return;
				msg_print("You gaze deeply in that direction.");
				if (confuse_monster(dir, plev))
				{
					fire_beam(GF_MISSILE, dir,
				           damroll(8, 8));					
				}
				break;
			}

			case NINJA_BLOODLINE_TSUKUYOMI:
			{
				if (!(p_ptr->sharingan))
				{
					msg_print("The Sharingan is required for this jutsu.");
					return;
				}
				break;
			}

			case NINJA_BLOODLINE_AMATERASU:
			{
				if (!(p_ptr->sharingan))
				{
					msg_print("The Sharingan is required for this jutsu.");
					return;
				}

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_CHAOS, dir,
				          100 + (plev * 10), 12);
				
				break;
			}

			case NINJA_BLOODLINE_HAKKE2:
			{
				if (!(p_ptr->byakugan))
				{
					msg_print("The Byakugan is required for this jutsu.");
					return;
				}

				if (p_ptr->chidori == TRUE)
				{
					msg_print("You dissipate your Chidori.");
					p_ptr->chidori = FALSE;

				}

				if (p_ptr->jyuken == TRUE)
				{
					msg_print("You switch out of your Jyuken stance.");
					p_ptr->jyuken = FALSE;
				}

				if (p_ptr->jyuken2 == TRUE)
				{
					msg_print("You are already in a wide Jyuken stance!");
					return;
				}

				if (p_ptr->rasengan == TRUE)
				{
					msg_print("You dissipate your Rasengan.");
					p_ptr->rasengan = FALSE;

				}

				msg_print("You assume a wide Jyuken stance.");
				p_ptr->jyuken2 = TRUE;

				break;
			}

			case NINJA_NINPOU_KAGE_MANE:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("You extend your shadow.");
				fire_ball(GF_SHADOW, dir,
				          0, 1 + (plev / 10));
				
				break;
			}

			case NINJA_NINPOU_DOKUGIRI:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          20 + (plev / 2), 2);
				break;
			}

			case NINJA_NINPOU_JOUROU_SENBON:
			{
				msg_print("Needles start raining down from the sky.");
				fire_ball(GF_SHARD, 0,
				          80 + (plev / 2), 13);
				break;
			}

			case NINJA_NINPOU_KANASHIBARI:
			{
				if (!get_aim_dir(&dir)) return;
			msg_print("You gaze deeply in that direction.");
				if (confuse_monster(dir, plev))
				{
					fire_beam(GF_MISSILE, dir,
				           damroll(8, 8));					
				}
				break;
			}

			case NINJA_NINPOU_MIST_CONCEALMENT:
				{
					(void)set_defense(p_ptr->defense + plev + 50);
					break;

				}

			case NINJA_NINPOU_SAND_ARMOR:
			{
				(void)set_armor_of_sand(plev * 4);
				break;
			}

			case NINJA_KINJUTSU_CHIDORI:
			{
				if (p_ptr->chidori == TRUE)
				{
					msg_print("You already have a Chidori in your hand!");
					return;

				}

				if (p_ptr->jyuken == TRUE)
				{
					msg_print("You switch out of your Jyuken stance.");
					p_ptr->jyuken = FALSE;
				}

				if (p_ptr->jyuken2 == TRUE)
				{
					msg_print("You switch out of your Jyuken stance.");
					p_ptr->jyuken2 = FALSE;
				}

				if (p_ptr->rasengan == TRUE)
				{
					msg_print("You disperse your Rasengan.");
					p_ptr->rasengan = FALSE;

				}
				msg_print("Your hands start to glow...");
				p_ptr->chidori = TRUE;
				break;
			}

			case NINJA_KINJUTSU_OPEN_GATE:
			{
				
				if (p_ptr->chakra_gate_level == 7)
				{
					msg_print("You are about to open the Eighth gate, the Death Gate!");
					msg_print("There is a VERY good chance that you will die once you open it!");
					if (!get_check("Are you sure you want to open the Death Gate?"))
					{
						return;
					}

				}
				p_ptr->chakra_gate_level += 1;

				(void)set_chakra_gate(p_ptr->lev);

				if (p_ptr->chakra_gate_level > 8)
				{
				msg_print("You have already opened all the gates!");
				p_ptr->chakra_gate_level = 8;
				return;
				}

				
				msg_format("The %s Gate, The %s, has been opened!", numeric_description[p_ptr->chakra_gate_level], 
					gate_description[p_ptr->chakra_gate_level]);

				/* Recalculate bonuses */
				p_ptr->update |= (PU_BONUS);

				/* Handle stuff */
				handle_stuff();


				break;

			}

			case NINJA_KINJUTSU_RASENGAN:
			{

				if (p_ptr->chidori == TRUE)
				{
					msg_print("You dissipate your Chidori.");
					p_ptr->chidori = FALSE;

				}

				if (p_ptr->jyuken == TRUE)
				{
					msg_print("You switch out of your Jyuken stance.");
					p_ptr->jyuken = FALSE;
				}

				if (p_ptr->jyuken2 == TRUE)
				{
					msg_print("You switch out of your Jyuken stance.");
					p_ptr->jyuken2 = FALSE;
				}

				if (p_ptr->rasengan == TRUE)
				{
					msg_print("You already have a Rasengan in your hand!");
					return;

				}
			
				msg_print("You form a sphere of rotating chakra in one hand...");
				p_ptr->rasengan = TRUE;
				
				break;
			}

			case NINJA_KINJUTSU_SOUZOU_SAISEI:
			{
				msg_print("You open the seal!");
				(void)restore_level();
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);

			/* Recalculate max. hitpoints */
			update_stuff();

			(void)hp_player(5000);

		if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			}

			msg_print("The side effects start to kick in...");
			/* Permanently Reduce all stats */
			(void)dec_stat(A_STR, 15 + randint(10), TRUE);
			(void)dec_stat(A_CON, 15 + randint(10), TRUE);
			(void)dec_stat(A_DEX, 15 + randint(10), TRUE);
			(void)dec_stat(A_WIS, 15 + randint(10), TRUE);
			(void)dec_stat(A_INT, 15 + randint(10), TRUE);
			(void)dec_stat(A_CHR, 15 + randint(10), TRUE);

				
				break;
			}

			case NINJA_KINJUTSU_JUKAI_KOTAN:
			{
				msg_print("Mokuton Hijitsu...Jukai Koutan!");
				for (dir = 0; dir < 5; dir++)
				{
				fire_ball(GF_MISSILE, 0, 275, 13);	
				earthquake(p_ptr->py, p_ptr->px, 13);
				}
				break;
			}

			case NINJA_KINJUTSU_ROLLING_THE_DICE:
			{
				fire_ball(GF_MANA, 0, rand_int(plev * 20), 13);
					for (dir = 0; dir < randint(10); dir++)
				{
				summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, 0);
				
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

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	/* Hack -- Sanjiyan consumes no mana during transform */
	if ((s_ptr->smana <= p_ptr->csp) && (!(p_ptr->wu_transform)))
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

/* Doing a "Super" (Limit Break) */
void do_cmd_super(void)
{
 
int plev = p_ptr->lev;	
int dir;
int rand_limit = rand_int(LB_DANCE + 1);
int selector;

/*	#define LB_GLOBE					0   For my own benefit
	#define LB_FAIRY_HEAL               1   
	#define LB_SUPER_SAYIAN             2   
	#define LB_HYPER_ARMOR              3   
	#define LB_NUKE                     4   
	#define LB_WHIRLWIND				5   
	#define LB_AHVB						6   
	#define LB_POWER_BLAST				7   
	#define LB_KEKKAI					8
#define LB_AHVB					9   
#define LB_FAIL					10  
#define LB_RAGING_DEAMON			11  
#define LB_RANDOM					12  */

											
	

	/* Must have meter*/
	if (p_ptr->c_meter < p_ptr->m_meter)
	{
		msg_print("You have insufficient meter!");
		return;
	}

		/* Hack -- Verify command */
			if (!get_check("Do you wish to super? "))
			{
				return;
			}

	/* Remnant exception */
	if (p_ptr->l_break == LB_RANDOM)
		selector = rand_limit;
		/*selector = LB_GENEI_JIN;*/
	else
		selector = p_ptr->l_break;

	
	/* Determine what kind of limit break used*/
	switch (selector)
	{

	case LB_GLOBE:
		{

		msg_print("You start to laugh maniacally!");
		(void)set_invuln(p_ptr->invuln + plev + 10);
		break;

		}

	case LB_FAIRY_HEAL:
		{

		msg_print("Fairies dance around you!");
		
			(void)restore_level();
			(void)set_poisoned(0);
			(void)set_confused(0);
			(void)set_blind(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);

			/* Recalculate max. hitpoints */
			update_stuff();

			(void)hp_player(5000);

		if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			}
	break;
	}

	case LB_SUPER_SAYIAN:
		{
			(void)set_stun(0);
			(void)hp_player(200);
			(void)set_s_sayian(p_ptr->s_sayian + plev + 10);
			break;
		}

	case LB_HYPER_ARMOR:
		{
			(void)set_shield(p_ptr->shield + plev * 4);
			(void)set_protevil(p_ptr->protevil + plev * 4);
			break;
		}

	case LB_NUKE:
		{
			msg_print("You tiger knee into the air and drop a nuke!");
			fire_ball(GF_RADI, 0,
				          plev * 50, 13);
			take_hit(100, "Nuclear Explosion");
			break;
			
		}

	case LB_WHIRLWIND:
		{
			msg_print("You unleash a whirlwind of fury swipes!");
			fire_ball(GF_FORCE, 0, plev * 25, 1);
			teleport_player(15);
			break;
		}

	case LB_TENTACLE:
		{
			msg_print("You start molesting all the creatures around you!");
			fire_ball(GF_MISSILE, 0, plev * 75, 1);

			break;
		}

	case LB_POWER_BLAST: /*misnamed*/
		{

			(void)set_invuln(p_ptr->invuln + plev / 10 + 2);
			(void)set_wu_transform(p_ptr->wu_transform + plev / 10 + 2);
			/*
			if (!get_aim_dir(&dir)) return;
			raging_demon(dir);
			break;
			
		p_ptr->word_recall = 1;
		break;
		*/
			break;
		}


	case LB_KEKKAI:
		{
			msg_print("You clasp your hands together and form a Kekkai.");
			(void)set_kekkai(p_ptr->kekkai + plev);
			break;
		}

	case LB_AHVB:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You tiger knee into the air and unleash a beam!");
			fire_beam(GF_METEOR, dir,
				          plev * 100);
			break;
		}
	case LB_FAIL:
		{
			msg_print("Nothing happens.....");
			break;
		}
	case LB_GENEI_JIN:
		{
			(void)set_geneijin(p_ptr->geneijin + 10 + plev/10);
			break;


		}

	case LB_DANCE:
		{
			msg_print("You start dancing about!");
			moogle_dance();
			break;


		}

	
	default:
		{
			msg_print("Nothing happens...");
		}
	}

	/* consume all meter */
	p_ptr->c_meter = 0;
	p_ptr->redraw |= (PR_METER);

	/* Take a turn */
	p_ptr->energy_use = 100;

}

/* Moogle Limit break */
/* A ghetto, but perhaps amusing way of doing what was intended */

void moogle_dance(void)
{

	int blah = rand_int(SV_WAND_ANNIHILATION + 1);
	int dir;

	for (dir = 1; dir < 10; dir++)
	
	/* Analyze the wand */
	switch (blah)
	{
		case SV_WAND_HEAL_MONSTER:
		{
			(void)heal_monster(dir);
			break;
		}

		case SV_WAND_HASTE_MONSTER:
		{
			(void)speed_monster(dir);
			break;
		}

		case SV_WAND_CLONE_MONSTER:
		{
			(void)clone_monster(dir);
			break;
		}

		case SV_WAND_TELEPORT_AWAY:
		{
			(void)teleport_monster(dir);
			break;
		}

		case SV_WAND_DISARMING:
		{
			(void)disarm_trap(dir);
			break;
		}

		case SV_WAND_TRAP_DOOR_DEST:
		{
			(void)destroy_door(dir);
			break;
		}

		case SV_WAND_STONE_TO_MUD:
		{
			(void)wall_to_mud(dir);
			break;
		}

		case SV_WAND_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			break;
		}

		case SV_WAND_SLEEP_MONSTER:
		{
			(void)sleep_monster(dir);
			break;
		}

		case SV_WAND_SLOW_MONSTER:
		{
			(void)slow_monster(dir);
			break;
		}

		case SV_WAND_CONFUSE_MONSTER:
		{
			(void)confuse_monster(dir, p_ptr->lev);
			break;
		}

		case SV_WAND_FEAR_MONSTER:
		{
			(void)fear_monster(dir, p_ptr->lev);
			break;
		}

		case SV_WAND_DRAIN_LIFE:
		{
			(void)drain_life(dir, p_ptr->lev * 10);
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			(void)poly_monster(dir);
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, p_ptr->lev, 2);
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2, p_ptr->lev));
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(5, p_ptr->lev));
			break;
		}

		case SV_WAND_ELEC_BOLT:
		{
			fire_bolt_or_beam(20, GF_ELEC, dir, damroll(3, p_ptr->lev));
			break;
		}

		case SV_WAND_FIRE_BOLT:
		{
			fire_bolt_or_beam(20, GF_FIRE, dir, damroll(6, p_ptr->lev));
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			fire_bolt_or_beam(20, GF_COLD, dir, damroll(3, p_ptr->lev));
			break;
		}

		case SV_WAND_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 60 + p_ptr->lev, 2);
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 32 + p_ptr->lev, 2);
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 72 + p_ptr->lev, 2);
			break;
		}

		case SV_WAND_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 48 + p_ptr->lev, 2);
			break;
		}

		case SV_WAND_WONDER:
		{
			/* Change to heal self */
			(void)hp_player(5000);
			break;
		}

		case SV_WAND_DRAGON_FIRE:
		{
			fire_ball(GF_FIRE, dir, 100 + p_ptr->lev, 3);
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			fire_ball(GF_COLD, dir, 80 + p_ptr->lev, 3);
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			switch (randint(5))
			{
				case 1:
				{
					fire_ball(GF_ACID, dir, 100 + p_ptr->lev, 3);
					break;
				}

				case 2:
				{
					fire_ball(GF_ELEC, dir, 80 + p_ptr->lev, 3);
					break;
				}

				case 3:
				{
					fire_ball(GF_FIRE, dir, 100 + p_ptr->lev, 3);
					break;
				}

				case 4:
				{
					fire_ball(GF_COLD, dir, 80 + p_ptr->lev, 3);
					break;
				}

				default:
				{
					fire_ball(GF_POIS, dir, 60 + p_ptr->lev, 3);
					break;
				}
			}

			
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
			(void)drain_life(dir, 125 + p_ptr->lev * 10);
			break;
		}
	}

}


/*
 * Pray a prayer
 */
void do_cmd_pray(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir, chance, amt;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Must use prayer books */
	if (cp_ptr->spell_book != TV_PRAYER_BOOK)
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

			case PRAYER_CURE_LIGHT_WOUNDS:
			{
				amt = damroll(2,10);

			if (amt < (p_ptr->mhp / 9))
			{
				amt = p_ptr->mhp / 9;
			}
				(void)hp_player(amt);
				(void)set_cut(p_ptr->cut - 10);
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
				(void)fear_monster(dir, plev);
				break;
			}

			case PRAYER_PORTAL:
			{
				teleport_player(plev * 3);
				break;
			}

			case PRAYER_CURE_SERIOUS_WOUNDS:
			{
				amt = damroll(4,10);

				if (amt < (p_ptr->mhp / 5))
				{
					amt = p_ptr->mhp / 5;
				}

				(void)hp_player(amt);
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case PRAYER_CHANT:
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				break;
			}

			case PRAYER_SANCTUARY:
			{
				(void)sleep_monsters_touch();
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

			case PRAYER_NEUTRALIZE_POISON:
			{
				(void)set_poisoned(0);
				break;
			}

			case PRAYER_ORB_OF_DRAINING:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          (damroll(3, 6) + plev +
				           (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4))),
				          ((plev < 30) ? 2 : 3));
				break;
			}

			case PRAYER_CURE_CRITICAL_WOUNDS:
			{
				amt = damroll(6,10);

			if (amt < (p_ptr->mhp / 3))
			{
				amt = p_ptr->mhp / 3;
			}
				(void)hp_player(amt);
				(void)set_cut(0);
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
				amt = damroll(8,10);

			if (amt < (p_ptr->mhp / 2))
			{
				amt = p_ptr->mhp / 2;
			}
				(void)hp_player(amt);
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
				(void)dispel_undead(randint(plev * 3));
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
				(void)dispel_evil(randint(plev * 3));
				break;
			}

			case PRAYER_GLYPH_OF_WARDING:
			{
				warding_glyph();
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

			case PRAYER_CURE_SERIOUS_WOUNDS2:
			{
				amt = damroll(4,10);

			if (amt < (p_ptr->mhp / 5))
			{
				amt = p_ptr->mhp / 5;
			}
				(void)hp_player(amt);
				(void)set_cut(0);
				break;
			}

			case PRAYER_CURE_MORTAL_WOUNDS2:
			{
				amt = damroll(8,10);

			if (amt < (p_ptr->mhp / 2))
			{
				amt = p_ptr->mhp / 2;
			}
				(void)hp_player(amt);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case PRAYER_HEALING:
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

			case PRAYER_DISPEL_UNDEAD2:
			{
				(void)dispel_undead(randint(plev * 4));
				break;
			}

			case PRAYER_DISPEL_EVIL2:
			{
				(void)dispel_evil(randint(plev * 4));
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

			case PRAYER_ARMAGEDDON:
			{
				msg_print("You feel a searing heat!");
				fire_ball(GF_METEOR, 0,
				          plev * 5, 15);
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case PRAYER_NOAH:
				{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_WATER, dir,
				          (plev * 2) + 20, 12);
				break;
				}

			case PRAYER_JUDGEMENT_DAY:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 400);
				break;
			}

			case PRAYER_UNBARRING_WAYS:
			{
				(void)destroy_doors_touch();
				break;
			}

			case PRAYER_RECHARGING:
			{
				(void)recharge(15);
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

	/* Hack -- Intervention sometimes allows players to ignore mana cost */
	if ((rand_int(200) < plev) && (cp_ptr->flags & CF_INTERVENTION))
	{
		
		p_ptr->csp += s_ptr->smana;

	}

	/* Sufficient mana */
	/* Hack -- Sanjiyan consume no mana during Wu */
	if ((s_ptr->smana <= p_ptr->csp) && (!(p_ptr->wu_transform)))
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
 * Use an intrinsic power
 */
void do_cmd_intrinsic(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int i, spell;

	char out_val[160];
	char choice;

	
	byte line_attr = TERM_WHITE;

	
	
	bool flag = FALSE; 
	bool redraw = FALSE;
	

	int plev = p_ptr->lev;



	


	/* Must not be confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	
	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();

	/* Title the list */
	prt("", 1, 20);
	put_str("Name", 1, 25);
	put_str("Meter", 1, 55);

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, ESC=exit) Use which Power? ",
	        I2A(0), I2A(4));
	c_prt(line_attr, out_val, 0, 0);
	/* Dump the spells */
	for (i = 0; i < 5; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), intrinsic_desc[cp_ptr->c_p[p_ptr->pclass][i]], intrinsic_cost[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}

	/* Clear the bottom line */
	prt("", 1 + i + 1, 20);
		
	
	/* Choose */
	while (1)
	{
	

		choice = inkey();
		spell = (islower(choice) ? A2I(choice) : -1);
		if (choice == ESCAPE){
			screen_load();
			return;
		}
		if ((spell >= 0) && (spell < 5)) break;
		else bell("Illegal POWER!");
	}
	
	
	screen_load();
	/* Verify adequate mana */
	if (intrinsic_cost[spell] > p_ptr->c_meter)
	{
		/* Warning */
		msg_print("You do not have enough meter to use that power!.");
		
		return;
	}

	switch (cp_ptr->c_p[p_ptr->pclass][spell])
	{
	case INTRINSIC_SECOND_WIND:
		{
			(void)hp_player(p_ptr->lev * 2);
			break;
		}

	case INTRINSIC_SECRET_STRENGTH:
		{
				if (p_ptr->csp < p_ptr->msp)
				{
					p_ptr->csp += p_ptr->lev;
				
				if (p_ptr->csp >= p_ptr->msp){
					p_ptr->csp = p_ptr->msp;
					p_ptr->csp_frac = 0;
				}
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			}
				
			break;

		}
	case INTRINSIC_HEROISM:
		{
			(void)hp_player(10);
			(void)set_afraid(0);
			(void)set_hero(p_ptr->hero + randint(25) + 25);
			break;
		}

	case INTRINSIC_BERSERKER:
		{
			(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
		}

	case INTRINSIC_FAULTLESS_DEFENSE:
		{
			(void)set_invuln(p_ptr->invuln + 1);
			break;
		}

	case INTRINSIC_KAIOKEN:
		{
			(void)set_kaioken(p_ptr->kaioken + plev);
			break;
		}

	case INTRINSIC_REPLACEMENT:
		{
			msg_print("You get a log ready.");
			p_ptr->replacement = TRUE;
			break;
		}

	case INTRINSIC_STEALTH:
		{
			msg_print("You vanish in a trail of leaves.");
			p_ptr->findme = TRUE;
			break;
		}

	case INTRINSIC_HOLY_PRAYER:
		{
		(void)set_blessed(p_ptr->blessed + randint(48) + p_ptr->lev);
		break;
		}

	case INTRINSIC_BACKSTAB:
		{
		p_ptr->backstab = TRUE;
		break;

		}

	case INTRINSIC_IDENTIFY:
		{
				if (!ident_spell()) return;
			break;
		}

	case INTRINSIC_HASTE:
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
	case INTRINSIC_STRONG_BOW:
		{
			msg_print("Your arms feel limber.");
			p_ptr->strong_bow = TRUE;
			break;
		}

	case INTRINSIC_BANISH_EVIL:
		{
			msg_print("You get a warding scroll ready.");
			p_ptr->banish_evil = TRUE;
			break;
		}

	case INTRINSIC_CREATE_COSTUME:
		{

		if	(!do_cmd_create_costume())
		{
			return;
		}
			break;

		}

	case INTRINSIC_ANALYZE:
		{
			probing();
			break;
		}

		
	
	}


	

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Use meter */
	p_ptr->c_meter -= intrinsic_cost[spell];
	

	/* Redraw mana */
	p_ptr->redraw |= (PR_METER);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}


/* Taunt */
void do_cmd_taunt(void)
{

	int counter = randint(4);

	switch (counter)
	{
	case 1: { msg_print("You yell, 'Seibai!'"); break; }
	case 2: { msg_print("You yell, 'I will punish you all!'"); break; }
	case 3: { msg_print("You laugh out loud!"); break; }
	case 4: { msg_print("You yell, 'Let us dance, you and I!'"); break; }
	}
	aggravate_monsters(-2);

	
		/* Take a turn */
	p_ptr->energy_use = 300;


}

/* Pose */
void do_cmd_pose(void)
{

	int counter = randint(4);

	switch (counter)
	{
	case 1: { msg_print("You put your hands across your chest and yell, 'Change!  Switch on!  One, Two, Three!'"); break; }
	case 2: { msg_print("You do a crane kick pose!"); break; }
	case 3: { msg_print("You do a split and put up your hands in a fighting pose!"); break; }
	case 4: { msg_print("You backflip and go into a fighting stance!"); break; }
	}
	aggravate_monsters(-3);

	
		/* Take a turn */
	p_ptr->energy_use = 300;


}

/* Fire Weapons from Mech */
bool do_cmd_mechfire(void)
{
	int dir;
	int counter;
	int multiplier = 1;
	object_type *o_ptr;
	o_ptr = &inventory[INVEN_MECHA];

	if (!(o_ptr->k_idx)){

		msg_print("You are not riding on a mecha.");
		return (FALSE);

	}

	/* Damage Multiplier for those efficient with Mechas */
	if (cp_ptr->flags & CF_MECHA_SENSE) {
		multiplier = 4;
	}

	switch (o_ptr->sval){

	case SV_GUNDAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (o_ptr->tval == TV_TEMP_MECHA)
			{
				msg_format("You fire your %s!", p_ptr->temp_mecha_gun_name);
			}
			else{
			msg_print("You fire your machine gun.");
			}

			for (counter = 0; counter < 4; counter++){
			fire_bolt(GF_METEOR, dir, 15 * multiplier);
			}

			break;

		}
	case SV_EVA:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("You fire your laser cannon.");
			fire_beam(GF_METEOR, dir, 60 * multiplier);
			break;
		}

	case SV_RAYEARTH:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("You shoot a Fire Arrow!");
			fire_bolt(GF_FIRE, dir, 60 * multiplier);
			break;
		}

	case SV_SELECE:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("You shoot a Sapphire Whirlwind!");
			fire_ball(GF_WATER, dir, 60 * multiplier, 5);
			break;
		}

	case SV_WINDAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("You shoot a Emerald Typhoon!");
			fire_ball(GF_SHARD, dir, 60 * multiplier, 10);
			break;
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	
	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	return (TRUE);
}

/* Mimic something */
void do_cmd_mimic(void)
{
	
	monster_type *m_ptr;
	monster_race *r_ptr;
	char m_name[80];

		if (target_set_interactive(TARGET_KILL))
	{
		if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}

	}

		else
		{
			return;
		}

	
	
	
	/* Get monster */
	m_ptr = &m_list[p_ptr->target_who];
	r_ptr = &r_info[m_ptr->r_idx];
	p_ptr->mimic_idx = m_ptr->r_idx;

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Monster too strong? */
	if ((p_ptr->lev < r_ptr->level) && (!(p_ptr->sharingan)))
	{
		msg_format("You cannot mimic %s without a Sharingan.", m_name);
		return;
	}

	msg_format("You mimic %s.", m_name);

	/* Otherwise, we're mimicing something */
	extract_mimic_powers(r_ptr);

		/* Recalculate bonuses */
					p_ptr->update |= (PU_BONUS);
					/* Take a turn */
					p_ptr->energy_use = 100;

					handle_stuff();
	
					return;

}

void extract_mimic_powers(const monster_race *r_ptr)
{
		p_ptr->mimic = TRUE;
	p_ptr->max_powers = 0;

	
	

	/* Extract Powers */
	if (r_ptr->flags4 & (RF4_BR_ACID))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_ACID;
			p_ptr->max_powers++;
		}

	}

	if (r_ptr->flags4 & (RF4_BR_ELEC))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_ELEC;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_FIRE))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_FIRE;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_COLD))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_COLD;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_POIS))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_POIS;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_NETH))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_NETH;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_LITE))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_LITE;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_DARK))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_DARK;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_CONF))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_CONF;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_SOUN))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_SOUN;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_CHAO))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_CHAO;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_DISE))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_DISE;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_NEXU))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_NEXU;
			p_ptr->max_powers++;
		}
	}


	if (r_ptr->flags4 & (RF4_BR_TIME))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_TIME;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_INER))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_INER;
			p_ptr->max_powers++;
		}
	}

	if (r_ptr->flags4 & (RF4_BR_GRAV))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_GRAV;
			p_ptr->max_powers++;
		}
	}
		
	if (r_ptr->flags4 & (RF4_BR_SHAR))
	{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_SHAR;
			p_ptr->max_powers++;
		}
	}

		if (r_ptr->flags4 & (RF4_BR_PLAS))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_PLAS;
			p_ptr->max_powers++;
		}

		}

		if (r_ptr->flags4 & (RF4_BR_WALL))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_WALL;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags4 & (RF4_BR_MANA))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BR_MANA;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BA_ACID))
		{
		if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_ACID;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BA_ELEC))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_ELEC;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BA_FIRE))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_FIRE;
			p_ptr->max_powers++;
		}
		}
		if (r_ptr->flags5 & (RF5_BA_COLD))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_COLD;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BA_POIS))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_POIS;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BA_NETH))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_NETH;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BA_WATE))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_WATE;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BA_MANA))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_MANA;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BA_DARK))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BA_DARK;
			p_ptr->max_powers++;
		}
		}
/*
		if (r_ptr->flags5 & (RF5_DRAIN_MANA))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract 
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_DRAIN_MANA;
			p_ptr->max_powers++;
		}
		}
		*/

		if (r_ptr->flags5 & (RF5_MIND_BLAST))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_MIND_BLAST;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BRAIN_SMASH))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BRAIN_SMASH;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_CAUSE_1))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_CAUSE_1;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_CAUSE_2))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_CAUSE_2;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_CAUSE_3))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_CAUSE_3;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_CAUSE_4))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_CAUSE_4;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_ACID))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_ACID;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_ELEC))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_ELEC;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_FIRE))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_FIRE;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_COLD))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_COLD;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_POIS))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_POIS;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_NETH))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_NETH;
			p_ptr->max_powers++;
		}
		}
		if (r_ptr->flags5 & (RF5_BO_WATE))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_WATE;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_MANA))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_MANA;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_PLAS))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_PLAS;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BO_ICEE))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BO_ICEE;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_MISSILE))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_MISSILE;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_SCARE))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_SCARE;
			p_ptr->max_powers++;
		}
		}

		
		if (r_ptr->flags5 & (RF5_CONF))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_CONF;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_SLOW))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_SLOW;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_BLIND))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BLIND;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags5 & (RF5_HOLD))
		{
				if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_HOLD;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags6 & (RF6_HASTE))
		{
				if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_HASTE;
			p_ptr->max_powers++;
		}
		}
		
		if (r_ptr->flags6 & (RF6_HEAL))
		{
				if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_HEAL;
			p_ptr->max_powers++;
		}
		}
		
		if (r_ptr->flags6 & (RF6_BLINK))
		{
				if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_BLINK;
			p_ptr->max_powers++;
		}
		}

		if (r_ptr->flags6 & (RF6_TPORT))
		{
				if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_TPORT;
			p_ptr->max_powers++;
		}
		}
		
		
		if (r_ptr->flags6 & (RF6_TELE_AWAY))
		{
				if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_TELE_AWAY;
			p_ptr->max_powers++;
		}
		}


		if (r_ptr->flags6 & (RF6_DIVINE_COMEDY))
		{
				if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_DIVINE_COMEDY;
			p_ptr->max_powers++;
		}
		}

		
		if (r_ptr->flags6 & (RF6_S_DRAGON_SLAVE))
		{
				if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_S_DRAGON_SLAVE;
			p_ptr->max_powers++;
		}
		}

		
}

void umareru(void){

	s16b damage = 0;
	msg_print("You expel all current powers and ills into");
	msg_print("one giant energy ball.");

	/* Timed -- Fast */
	damage += p_ptr->fast / 5;
	(void)set_fast(0);

	/* Timed -- Slow */
	damage += p_ptr->slow / 10;
	(void)set_slow(0);
	
	/* Timed -- Blindness */
	damage += p_ptr->blind / 10;
	(void)set_blind(0);

	/* Timed -- Confusion */
	damage += p_ptr->confused / 10;
	(void)set_confused(0);
	
	/* Timed -- Fear */
	damage += p_ptr->afraid / 10;
	(void)set_afraid(0);
	
	/* Timed -- Hallucination */
	damage += p_ptr->image / 20;
	(void)set_image(0);

	/* Timed -- Poisoned */
	damage += p_ptr->poisoned / 10;
	(void)set_poisoned(0);
	
	/* Timed -- Cut */
	damage += p_ptr->cut / 10;		
	(void)set_cut(0);

	/* Timed -- Stun */
	damage += p_ptr->stun / 10;
	(void)set_stun(0);

	/* Timed -- Protection */
	damage += p_ptr->protevil / 5;
	(void)set_protevil(0);

	/* Timed -- Invulnerable */
	damage += p_ptr->invuln / 5;		
	(void)set_invuln(0);

	/* Timed -- Heroism */
	damage += p_ptr->hero / 5;
	(void)set_hero(0);

	/* Timed -- Super Heroism */
	damage += p_ptr->shero / 5;			
	(void)set_shero(0);

	/* Timed -- Super Saiyan */
	damage += p_ptr->s_sayian / 2;
	(void)set_s_sayian(0);

	/* Timed -- Wu Transform */
	damage += p_ptr->wu_transform * 10;
	(void)set_wu_transform(0);

	/* Timed -- Shield Spell */
	damage += p_ptr->shield / 2;
	(void)set_shield(0);

	/* Timed -- Blessed */
	damage += p_ptr->blessed / 5;
	(void)set_blessed(0);

	/* Timed -- See Invisible */
	damage += p_ptr->tim_invis / 5;		
	(void)set_tim_invis(0);

	/* Timed -- Infra Vision */
	damage += p_ptr->tim_infra / 5;
	(void)set_tim_infra(0);

	/* Timed -- Kekkai */
	damage += p_ptr->kekkai / 2;		
	(void)set_kekkai(0);

	/* Timed -- Genei Jin */
	damage += p_ptr->geneijin * 5;		
	(void)set_geneijin(0);

	/* Timed -- Ouroborous */
	damage += p_ptr->ouroborous / 2;
	(void)set_ouroborous(0);
	
	/* Timed -- oppose acid */
	damage += p_ptr->oppose_acid / 5;
	(void)set_oppose_acid(0);

	/* Timed -- oppose lightning */
	damage += p_ptr->oppose_elec / 5;
	(void)set_oppose_elec(0);

	/* Timed -- oppose heat */
	damage += p_ptr->oppose_fire / 5;
	(void)set_oppose_fire(0);

	/* Timed -- oppose cold */
	damage += p_ptr->oppose_cold / 5;
	(void)set_oppose_cold(0);

	/* Timed -- oppose poison */
	damage += p_ptr->oppose_pois / 5;
	(void)set_oppose_pois(0);

	/* Timed -- Kaioken */
	damage += p_ptr->kaioken / 5;
	(void)set_kaioken(0);

	/* Timed --  Double Team */
	damage += p_ptr->double_team / 5;
	(void)set_double_team(0);

	fire_ball(GF_MANA, 0, damage, 5);
	(void)set_paralyzed(4);
	

	
}


/* Use a Chi Warrior Power */
void do_cmd_chi_power(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int spell, dir, i;
	

	char tmp_val[160];

	long tmp_long;
	

	char choice;
	char m_name[80];
	char out_val[160];

	int plev = p_ptr->lev;
	int beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

	

	byte line_attr = TERM_WHITE;

	bool flag = FALSE; 
	bool redraw = FALSE;
	monster_type *m_ptr;

	/* Must have powers */
	if (p_ptr->max_powers == 0)
	{
		msg_print("You have learned no techniques.");
		return;
	}

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();

	/* Title the list */
	prt("", 1, 20);
	put_str("Name", 1, 25);
	put_str("Mana", 1, 55);

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, ESC=exit) Use which Technique? ",
	        I2A(0), I2A(p_ptr->max_powers - 1));
	c_prt(line_attr, out_val, 0, 0);
	/* Dump the spells */
	for (i = 0; i < p_ptr->max_powers; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), chi_warrior_powers[p_ptr->player_powers[i]], chi_warrior_cost[p_ptr->player_powers[i]]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}

	/* Clear the bottom line */
	prt("", 1 + i + 1, 20);

		
	/* Choose */
	while (1)
	{
	

		choice = inkey();
		spell = (islower(choice) ? A2I(choice) : -1);
		if (choice == ESCAPE){
			screen_load();
			return;
		}
		if ((spell >= 0) && (spell < p_ptr->max_powers)) break;
		else bell("Illegal Technique!");
	}
	
	
	screen_load();
	/* Verify adequate mana */
	if (chi_warrior_cost[p_ptr->player_powers[spell]] > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use that technique!.");
		
		return;
	}

	switch (p_ptr->player_powers[spell])
	{
	case CHI_WARRIOR_BLINK:
		{
			teleport_player(10);
			break;
		}

	case CHI_WARRIOR_HEROISM:
		{
			(void)hp_player(10);
			(void)set_hero(p_ptr->hero + randint(25) + 25);
			(void)set_afraid(0);
			break;
		}

	case CHI_WARRIOR_BERSERK:
		{
			(void)hp_player(30);
			(void)set_shero(p_ptr->shero + randint(25) + 25);
			(void)set_afraid(0);
			break;
		}

	case CHI_WARRIOR_MISSILE:
		{
			if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
				break;
		}

	case CHI_WARRIOR_UMARERU:
		{
			umareru();
			break;
		}

	case CHI_WARRIOR_KAMEHAMEHA:
		{
				if (!get_aim_dir(&dir)) return;
			msg_print("You scream out 'Kamehameha'!");
				fire_beam(GF_MANA, dir,
				          (plev * 3));
				break;
		}

	case CHI_WARRIOR_BURST:
		{
			msg_print("You jump up and flash gold.");
				fire_ball(GF_GRAVITY, 0,
				          0, 6);
				(void)set_meter(p_ptr->c_meter + p_ptr->lev * 2);
				break;
		}

	case CHI_WARRIOR_BAKUSAI_TENGETSU:
		{
				msg_print("You assume a Bakusai Tenketsu stance.");
				p_ptr->bakusai_tengetsu = TRUE;
			
			break;

		}

	case CHI_WARRIOR_HIRYU_SHOTEN_HA:
		{
				msg_print("A massive whirlwind spins around you!");
			fire_ball(GF_FORCE, 0,
				          (p_ptr->mhp / 3), 10);
			fire_ball(GF_GRAVITY, 0,
				          0, 10);
			break;
		}

	case CHI_WARRIOR_HASTE:
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

	case CHI_WARRIOR_GENEI_JIN:
		{
			(void)set_geneijin(p_ptr->geneijin + 15 + plev/10);
			break;
		}

	case CHI_WARRIOR_KAIOKEN:
		{
	(void)set_kaioken(p_ptr->kaioken + plev + 50);
			break;

		}

	case CHI_WARRIOR_DOUBLE_TEAM:
		{
		(void)set_double_team(p_ptr->double_team + plev + 50);
			break;
		}

	case CHI_WARRIOR_THE_WORLD:
		{
				if (target_set_interactive(TARGET_KILL))
		{
		
			if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}
				}
			else
		{
			return;
		}

	/* Require Adjacency */
	if ((p_ptr->target_row - py > 1) || (p_ptr->target_row - py < -1)
		|| (p_ptr->target_col - px > 1) || (p_ptr->target_col - px < -1))
	{
		msg_print("Target is not adjacent.");
		return;
	}
	m_ptr = &m_list[cave_m_idx[p_ptr->target_row][p_ptr->target_col]];
	/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);
			msg_print("You scream 'Muda da!'");
			msg_format("You scream 'ADUBAUDBAUDBAUDBADA!' as you pummel %s with flurry of punches!", m_name);
			msg_print("You scream 'ZA WORUDO!'  A black aura eminates from you with a huge sucking sound!");
			fire_ball(GF_CONFUSION, 0, 0, 10);
			fire_ball(GF_SOUND, 0, 0, 10);
			msg_format("You scream 'Stop time!' and then punch %s!", m_name);
			fire_bolt(GF_DASH, 5, 0);
			msg_format("You say 'Time resume!'");
			msg_format("You blink and smash %s with a steamroller and scream out 'WRYYYYYYYYYYYYYYYYYYYY!'", m_name);
			teleport_player_to(m_ptr->fy, m_ptr->fx);
			fire_ball(GF_MISSILE, 0, damroll(plev, 8), 1);
				msg_print("You become tired from the effort.");
				(void)set_slow(p_ptr->slow + rand_int(10) + 4);
				(void)set_meter(p_ptr->c_meter / 2);
				/*(void)set_invuln(p_ptr->invuln + randint(8) + 8); Original GOI */
				break;
		}

	case CHI_WARRIOR_GLITTERING_GOLD:
	{
		/* Query */
	if (!get_string("Gold: ", tmp_val, 10)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long <= 0) return;

	/* Cap */

	if (tmp_long > p_ptr->au)
	{
		tmp_long = p_ptr->au;		
	}

	if (tmp_long > 50000)
	{
		tmp_long = 50000;
	}

	if (!get_aim_dir(&dir)) return;

	if (tmp_long > 1)
	msg_format("You chuck %d gold coins!", tmp_long);

	else
	msg_format("You chuck %d gold coin!", tmp_long);

	p_ptr->au -= tmp_long;

	fire_beam(GF_MISSILE, dir, tmp_long / 250 + plev * 2);

	/* Redraw gold */
	p_ptr->redraw |= (PR_GOLD);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	
	break;
	}
  
	case CHI_WARRIOR_SUPERIOR_ATTACK:
		{
			for (i = 0; i < 10; i++)
			fire_beam(GF_PLASMA, i, plev * 5);
			break;
		}

	case CHI_WARRIOR_DETECTION:
		{
			(void)detect_all();
			break;
		}

	case CHI_WARRIOR_SUPER_PUNCH:
		{
			msg_print("You hear the sound of a train....");
			msg_print("number 235 will depart to Osaka shortly after 6:30...");
			p_ptr->super_punch = TRUE;
			break;
		}

	case CHI_WARRIOR_SMOKE_BOMBS:
		{
			fire_ball(GF_CONFUSION, 0, 0, 10);
			fire_ball(GF_SOUND, 0, 0, 10);
			break;
		}

	case CHI_WARRIOR_DEFENSE:
		{
			(void)set_defense(p_ptr->defense + plev + 50);
			break;
		}

	case CHI_WARRIOR_OUROBOROUS:
		{
			(void)set_ouroborous(p_ptr->ouroborous + 10 + plev/5);
			break;
		}

	case CHI_WARRIOR_DRUNK_FIST:
		{
				if (p_ptr->confusing == 0)
			{
				msg_print("Your hands begin to glow.");
				p_ptr->confusing = TRUE;
			}
			break;
		}

	case CHI_WARRIOR_ELEMENTAL_AURA:
		{
			msg_print("Lightning clouds hover about you.");
			p_ptr->elemental_aura = TRUE;
			break;
		}

	case CHI_WARRIOR_AMAGURI_KEN:
		{
			msg_print("You assume an Amaguri Ken stance.");
			p_ptr->amaguri_ken = TRUE;
			break;
		}

	case CHI_WARRIOR_DEKAKERU:
		{
			msg_print("You run away...");
			teleport_player(100);
			break;
		}

	case CHI_WARRIOR_CROSS_COUNTER:
		{
			msg_print("You assume a cross counter stance.");
			p_ptr->cross_counter = TRUE;
			break;
		}

	case CHI_WARRIOR_UME_SHORYU:
		{
			msg_print("You assume an Ume-Shoryu stance.");
			p_ptr->ume_shoryu = TRUE;
			break;
		}

	case CHI_WARRIOR_CARP_ON_CUTTING_BOARD:
		{
			msg_print("You flop down in a submissive stance.");

				/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + plev * 5);

		break;
		}

	case CHI_WARRIOR_RIDE_THE_LIGHTNING:
		{
			msg_print("You ride a huge lightning wave.");
			fire_ball(GF_ELEC, 0, plev * 5, 5);
			teleport_player(5);
			break;
		}

	case CHI_WARRIOR_NANI_GA_DERU_KA_NA:
		{
			i = rand_int(20);
			if (!get_aim_dir(&dir)) return;

			if (i < 3)
			{
				msg_print("You chuck a meteor.");
				fire_ball(GF_METEOR, 0, plev * 5, 5);
			}

			else if (i < 10)
			{
				msg_print("You chuck a candy bar.");
				(void)hp_player(plev * 2);
			}

			else if (i < 14)
			{
				msg_print("You chuck a hammer.");
				fire_bolt(GF_MISSILE, dir, damroll(1, plev));

			}

			else if (i < 17)
			{
				msg_print("You chuck a poison bottle.");
				fire_bolt(GF_POIS, dir, damroll(1, plev));
			}

			else {
				msg_print("You chuck a bomb.");
				fire_ball(GF_FIRE, 0, plev * 5, 5);
				take_hit(damroll(1, plev), "a bomb");

			}

			break;
		}


	}

	/* Use Mana */
	/* Hack -- Sanjiyans consume no mana during Wu */
	if (!(p_ptr->wu_transform))
	{
	p_ptr->csp -= chi_warrior_cost[p_ptr->player_powers[spell]];
	}

		/* Update stuff */
	p_ptr->update |= (PU_BONUS);

	/* Update Stuff */
	update_stuff();

		/* Take a turn */
	p_ptr->energy_use = 100;

	
	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);


}



/* Use a Mimic Power */
void do_cmd_mimic_cast(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int spell, dir, i;
	

	char choice;
	char out_val[160];

	int plev = p_ptr->lev;
	int beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

	

	byte line_attr = TERM_WHITE;

	bool flag = FALSE; 
	bool redraw = FALSE;

	/* Must be mimicing something */
	if (p_ptr->max_powers == 0)
	{
		msg_print("You must be mimicing something that can cast spells.");
		return;
	}

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();

	/* Title the list */
	prt("", 1, 20);
	put_str("Name", 1, 25);

	if (!(cp_ptr->flags & CF_FREE_MIMIC))
	{
	put_str("Mana", 1, 55);
	}

	else
	{
		put_str("Meter", 1, 55);
	}

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, ESC=exit) Use which Power? ",
	        I2A(0), I2A(p_ptr->max_powers - 1));
	c_prt(line_attr, out_val, 0, 0);

	if (!(cp_ptr->flags & CF_FREE_MIMIC))
	{
	/* Dump the spells */
	for (i = 0; i < p_ptr->max_powers; i++)
	{

		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), mimic_powers[p_ptr->player_powers[i]], mimic_powers_cost[p_ptr->player_powers[i]]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}

	}

	else
	{
		/* Dump the spells */
	for (i = 0; i < p_ptr->max_powers; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), mimic_powers[p_ptr->player_powers[i]], mimic_powers_cost[p_ptr->player_powers[i]] * 10);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}

	}

	/* Clear the bottom line */
	prt("", 1 + i + 1, 20);

		
	/* Choose */
	while (1)
	{
	

		choice = inkey();
		spell = (islower(choice) ? A2I(choice) : -1);
		if (choice == ESCAPE){
			screen_load();
			return;
		}
		if ((spell >= 0) && (spell < p_ptr->max_powers)) break;
		else bell("Illegal POWER!");
	}
	
	
	screen_load();
	/* Verify adequate mana */
	if (!(cp_ptr->flags & CF_FREE_MIMIC))
	{

	if (mimic_powers_cost[p_ptr->player_powers[spell]] > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use that power!.");
		
		return;
	}

	}

	else
	{
		if (mimic_powers_cost[p_ptr->player_powers[spell]] * 10 > p_ptr->c_meter)
	{
		/* Warning */
		msg_print("You do not have enough meter to use that power!.");
		
		return;
	}

	}

	
	switch (p_ptr->player_powers[spell])
	{
	
	case MIMIC_BR_ACID:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe acid.");
				fire_ball(GF_ACID, dir,
				          (p_ptr->mhp / 6), 2);
				break;						
		}

	case MIMIC_BR_ELEC:
		{
				if (!get_aim_dir(&dir)) return;
				msg_print("You breathe lightning.");
				fire_ball(GF_ELEC, dir,
				          (p_ptr->mhp / 6), 2);
				break;
		}

	case MIMIC_BR_FIRE:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe fire.");
				fire_ball(GF_FIRE, dir,
				          (p_ptr->mhp / 6), 2);
				break;
		}

 
	case MIMIC_BR_COLD:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe frost.");
				fire_ball(GF_COLD, dir,
				          (p_ptr->mhp / 6), 2);
				break;

		}
		
	case MIMIC_BR_POIS:
		{

			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe poison.");
				fire_ball(GF_POIS, dir,
				          (p_ptr->mhp / 6), 2);
				break;
		}

	case MIMIC_BR_NETH:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe nether.");
				fire_ball(GF_NETHER, dir,
				          (p_ptr->mhp / 5), 2);
				break;
		}

	case MIMIC_BR_LITE:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe light.");
				fire_ball(GF_LITE, dir,
				          (p_ptr->mhp / 5), 2);
				break;
		}

	case MIMIC_BR_DARK:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe dark.");
				fire_ball(GF_DARK, dir,
				          (p_ptr->mhp / 5), 2);
				break;
		}

	case MIMIC_BR_CONF:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe confusion.");
				fire_ball(GF_CONFUSION, dir,
				          (p_ptr->mhp / 5), 2);
				break;
		}

	case MIMIC_BR_SOUN:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe sound.");
				fire_ball(GF_SOUND, dir,
				          (p_ptr->mhp / 5), 2);
				break;
		}

	case MIMIC_BR_CHAO:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe chaos.");
				fire_ball(GF_CHAOS, dir,
				          (p_ptr->mhp / 4), 2);
				break;
		}

	case MIMIC_BR_DISE:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe disenchantment.");
				fire_ball(GF_DISENCHANT, dir,
				          (p_ptr->mhp / 4), 2);
				break;
		}

	case MIMIC_BR_NEXU:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe nexus.");
				fire_ball(GF_NEXUS, dir,
				          (p_ptr->mhp / 4), 2);
				break;
		}

	case MIMIC_BR_TIME:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe time.");
				fire_ball(GF_TIME, dir,
				          (p_ptr->mhp / 4), 2);
				break;
		}

	case MIMIC_BR_INER:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe inertia.");
				fire_ball(GF_INERTIA, dir,
				          (p_ptr->mhp / 4), 2);
				break;
		}

	case MIMIC_BR_GRAV:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe gravity.");
				fire_ball(GF_GRAVITY, dir,
				          (p_ptr->mhp / 3), 2);
				break;
		}

	case MIMIC_BR_SHAR:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe shards.");
				fire_ball(GF_SHARD, dir,
				          (p_ptr->mhp / 3), 2);
				break;
		}

	case MIMIC_BR_PLAS:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe plasma.");
				fire_ball(GF_PLASMA, dir,
				          (p_ptr->mhp / 3), 2);
				break;
		}

	case MIMIC_BR_WALL:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe force.");
				fire_ball(GF_FORCE, dir,
				          (p_ptr->mhp / 3), 2);
				break;
		}

	case MIMIC_BR_MANA:
		{
			if (!get_aim_dir(&dir)) return;
				msg_print("You breathe mana.");
				fire_ball(GF_MANA, dir,
				          (p_ptr->mhp / 3), 2);
				break;
		}

	case MIMIC_BA_ACID:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          40 + (plev), 2);
				break;
		}

	case MIMIC_BA_ELEC:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir,
				          30 + (plev), 2);
				break;
		}

	case MIMIC_BA_FIRE:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          20 + (plev), 2);
				break;
		}
	
	case MIMIC_BA_COLD:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          20 + (plev), 2);
				break;
		}

	case MIMIC_BA_POIS:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          40 + (plev), 2);
				break;
		}

	case MIMIC_BA_NETH:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_NETHER, dir,
				          50 + (plev), 2);
				break;
		}

	case MIMIC_BA_WATE:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_WATER, dir,
				          50 + (plev), 2);
				break;
		}

	case MIMIC_BA_MANA:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          60 + (plev), 2);
				break;
		}

	case MIMIC_BA_DARK:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir,
				          30 + (plev), 2);
				break;
		}
/*
	case MIMIC_DRAIN_MANA:
		{
			msg_print("I won't work until someone does Monster Mana!");
			msg_print("So...hah :P");
			break;

		}*/

	case MIMIC_MIND_BLAST:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You gaze deeply in that direction.");
				if (confuse_monster(dir, plev))
				{
					fire_beam(GF_MISSILE, dir,
				           damroll(8, 8));					
				}
				break;
		}

	case MIMIC_BRAIN_SMASH:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You concentrate your mind in that direction.");
				fire_beam(GF_MISSILE, dir, damroll(12, 15));	
				(void)confuse_monster(dir, plev);
				(void)slow_monster(dir);
				(void)sleep_monster(dir);
				break;
		}

	case MIMIC_CAUSE_1:
		{
			/* Ghetto for now...perhaps in the future
			 * this will be a projection spell */
			if (!get_aim_dir(&dir)) return;
			msg_print("You point and curse.");
			fire_bolt(GF_MISSILE, dir, damroll(3 , 8));
			break;
		}

	case MIMIC_CAUSE_2:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You point and curse horribly.");
			fire_bolt(GF_MISSILE, dir, damroll(8 , 8));
			break;
		}

	case MIMIC_CAUSE_3:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You point and incant terribly!");
			fire_bolt(GF_MISSILE, dir, damroll(10 , 15));
			break;
		}

	case MIMIC_CAUSE_4:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You point, screaming the word 'DIE!'");
			fire_bolt(GF_MISSILE, dir, damroll(15 , 15));
			break;
		}

	case MIMIC_BO_ACID:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
		}

	case MIMIC_BO_ELEC:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				break;
		}
		
	case MIMIC_BO_FIRE:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
		}

	case MIMIC_BO_COLD:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				break;
		}

	case MIMIC_BO_POIS:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_POIS, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
		}

	case MIMIC_BO_NETH:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_NETHER, dir,
				                  30 + damroll(6+((plev-5)/4), 8));
				break;
		}

	case MIMIC_BO_WATE:
		{	
			if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_WATER, dir,
				                  plev + damroll(10, 10));
				break;
		}

	case MIMIC_BO_MANA:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_MANA, dir,
				                  randint(plev * 7 / 2) + 50);
				break;
		}

	case MIMIC_BO_PLAS:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_PLASMA, dir,
				                  damroll(8, 7) + 10 + plev);
				break;
		}

	case MIMIC_BO_ICEE:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ICE, dir,
				                  damroll(6, 6) + plev);
				break;
		}

	case MIMIC_MISSILE:
		{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_MISSILE, dir,
				                  damroll(2, 6) + (plev / 3));
				break;
		}

	case MIMIC_SCARE:
		{
			if (!get_aim_dir(&dir)) return;
			(void)fear_monster(dir, plev);
			break;
		}

	case MIMIC_BLIND:
		{
			/* Something here for now...this should change */
			fire_ball(GF_SOUND, 0, 0, 10);
			break;
		}

	case MIMIC_CONF:
		{
			if (!get_aim_dir(&dir)) return;
			(void)confuse_monster(dir, plev);
			break;
		}

	case MIMIC_SLOW:
		{
			if (!get_aim_dir(&dir)) return;
			(void)slow_monster(dir);
			break;
		}

	case MIMIC_HOLD:
		{
			/* Can't think of something better */
			if (!get_aim_dir(&dir)) return;
			fire_beam(GF_ICE, dir, 0);
			break;
		}

	case MIMIC_HASTE:
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

	case MIMIC_HEAL:
		{
			(void)hp_player(300);
			(void)set_poisoned(0);
			(void)set_confused(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
		}

	case MIMIC_BLINK:
		{
			teleport_player(10);
			break;
		}

	case MIMIC_TPORT:
		{
			teleport_player(plev * 5);
			break;
		}

	case MIMIC_TELE_AWAY:
		{
			if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				break;
		}

	case MIMIC_DIVINE_COMEDY:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You scream, 'Swallow these pathetic fools!'");
				fire_ball(GF_MANA, dir,
				          (plev * 7) + damroll(15, 15), 2);
				break;
		}

	case MIMIC_S_DRAGON_SLAVE:
		{
			if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
				          (plev) * 10, 3);
				break;
		}

	default:
		{
			msg_print("Nothing happens");
			break;

		}
		
	}

	/* Update stuff */
	p_ptr->update |= (PU_BONUS);

	/* Update Stuff */
	update_stuff();

		/* Take a turn */
	p_ptr->energy_use = 100;

	/* Use mana */
	/* Hack -- Sanjiyan consume no mana during Wu */
	if (!(p_ptr->wu_transform) && (!(cp_ptr->flags & CF_FREE_MIMIC))){
	p_ptr->csp -= mimic_powers_cost[p_ptr->player_powers[spell]];
	}

	else if (cp_ptr->flags & CF_FREE_MIMIC)
	{
		set_meter(p_ptr->c_meter - mimic_powers_cost[p_ptr->player_powers[spell]] * 10);
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);


}

/* Use a Magic Knight power -- This is just as sad as the student code */
void do_cmd_mknight(void)
{

	int py = p_ptr->py;
	int px = p_ptr->px;
	int i, spell;

	char out_val[160];
	char choice;

	/* Magic Powers */
cptr fire_powers[3] =
{
	"Flame Arrow",
	"Flame Arrow 2",
 	"Ruby Lightning"
};

cptr water_powers[3] =
{
	"Magic Missile",
	"Water Dragon",
	"Sapphire Whirlwind"
 	
};

cptr wind_powers[3] =
{
	"Wind of Protection",
	"Emerald Typhoon",
	"Winds of Healing"
 	
};

cptr metal_powers[3] =
{
	"Electric Bolt",
	"Thunderstorm",
	"Magnetic Tempest"
 	
};

cptr drunk_powers[3] =
{
	"Drunken Flail",
	"Aura of Confusion",
	"Sake Dragon"
 	
};

cptr sand_powers[3] =
{
	"Desert Coffin",
	"Desert Funeral",
	"Sand Shield"

};

cptr mknight_powers[10] =
{
	"Nothing",
	"Nothing",
	"Nothing",
	"Phase Door",
	"Heroism",
	"Berserk Strength",
	"Haste",
	"Psych Burst",
	"Detection",
	"Alter Reality"
};

/* Cost */
int mana_cost[10] = {5, 10, 20, 5, 10, 20, 40, 80, 80, 160};
byte line_attr = TERM_WHITE;

/* Direction */
int dir;

/*	bool verify; */

	bool flag = FALSE; 
	bool redraw = FALSE;
	/* bool okay;*/

	int plev = p_ptr->lev;

/*	const power_type *s_ptr; */

	if (p_ptr->pclass != C_MAGIC_KNIGHT) {
		msg_print("You are not a magic knight!");
		return;

	}

	/* Must not be confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();

	/* Title the list */
	prt("", 1, 20);
	put_str("Name", 1, 25);
	put_str("Mana", 1, 55);

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, ESC=exit) Use which Power? ",
	        I2A(0), I2A(2));
	c_prt(line_attr, out_val, 0, 0);
	


	switch (p_ptr->pgroove)
	{
	case G_FIRE:
		{
	
	for (i = 0; i < 3; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), fire_powers[i], mana_cost[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}
	break;

		}

	case G_WATER:
		{
	
	for (i = 0; i < 3; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), water_powers[i], mana_cost[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}
	break;

		}

	case G_WIND:
		{
	
	for (i = 0; i < 3; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), wind_powers[i], mana_cost[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}
	break;

		}

	case G_METAL:
		{
	
	for (i = 0; i < 3; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), metal_powers[i], mana_cost[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}
	break;

		}

	case G_DRUNK:
		{
	
	for (i = 0; i < 3; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), drunk_powers[i], mana_cost[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}
	break;

		}

		
	case G_SAND:
		{
	
	for (i = 0; i < 3; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), sand_powers[i], mana_cost[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}
	break;

		}


	}
	/* New powers */
	for (i = 3; i < 10; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), mknight_powers[i], mana_cost[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}

	while (1)
	{
	

		choice = inkey();
		spell = (islower(choice) ? A2I(choice) : -1);
		if (choice == ESCAPE){
			screen_load();
			return;
		}
		if ((spell >= 0) && (spell < 10)) break;
		else bell("Illegal POWER!");
	}
	
	screen_load();
	/* Verify adequate mana */
	if (mana_cost[spell] > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use that power!.");
		
		return;
	}

	if (p_ptr->pgroove == G_FIRE){

		switch (spell){

		case FIRE_FLARE_ARROW:
			{

				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir,
				           damroll(1 + (plev  / 2), 6));
					break;
			}
		

		case FIRE_FLARE_ARROW_2:
			{

				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_FIRE, dir,
				           damroll(1 + (plev  / 2), 4));
					break;
			}

		case FIRE_RUBY_LIGHTNING:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				           damroll(1 + (plev  / 2), 6), 6);
					break;
			}
		}
	}

	else if (p_ptr->pgroove == G_WATER){
		switch (spell)
		{

		case WATER_MAGIC_MISSILE:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_WATER, dir,
				           damroll(1 + (plev  / 2), 6));
					break;
			}

		case WATER_WATER_DRAGON:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_WATER, dir,
				           damroll(1 + (plev  / 2), 4));
					break;

			}

		case WATER_SAPPHIRE_WHIRLWIND:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_WATER, dir,
				           damroll(1 + (plev  / 2), 4),6);
					break;

			}
		}
	}

	else if (p_ptr->pgroove == G_WIND){
		switch (spell)
		{
		
		case WIND_WIND_OF_PROTECTION:
			{
				msg_print("You create a protective aura!");

				(void)set_blessed(p_ptr->blessed + 5 + plev);
					break;

			}

		case WIND_EMERALD_TYPHOON:
			{

					if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FORCE, dir,
				           damroll(1 + (plev  / 2), 6),6);
					break;
			}

		case WIND_WINDS_OF_HEALING:
			{

				(void)hp_player(10 + plev);
				break;


			}
		}
	}

	else if (p_ptr->pgroove == G_METAL)
	{
		switch (spell)
		{

		case METAL_ELECTRIC_BOLT:
			{

				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir,
				           damroll(1 + (plev  / 2), 4));
					break;
			}

		case METAL_THUNDERSTORM:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_WATER, dir,
				           damroll(1 + (plev  / 2), 4),6);
					break;
			}

		case METAL_MAGNETIC_TEMPEST:
			{
				for (dir = 1; dir < 10; dir++){
				fire_bolt(GF_WATER, dir,
				           damroll(1 + (plev  / 2), 4));
					}
				break;

			}
		}
	}

		else if (p_ptr->pgroove == G_SAND)
	{
		switch (spell)
		{

		case SAND_DESERT_COFFIN:
			{

				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_SHADOW, dir,
				           damroll(1 + (plev  / 2), 4));
					break;
			}

		case SAND_DESERT_FUNERAL:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FORCE, dir,
				           damroll(1 + (plev  / 2), 4),6);
					break;
			}

		case SAND_SAND_SHIELD:
			{
				(void)set_armor_of_sand(plev * 4);
					
				break;

			}
		}
	}

	else {
		switch (spell)
		{
		case DRUNK_DRUNKEN_FLAIL:
			{
				msg_print("You flail about!");
				fire_ball(GF_MISSILE, 5,
				           damroll(1 + (plev  / 2), 4),1);
					break;
			}
		case DRUNK_AURA_OF_CONFUSION:
		{
			fire_ball(GF_CONFUSION, 5, 0, 10);
			break;


		}
		case DRUNK_SAKE_DRAGON:
			{

				for (dir = 1; dir < 10; dir++){
				fire_beam(GF_CONFUSION, dir,
				           damroll(1 + (plev  / 2), 4));
				
				}

				break;


			}
	}
	}

	/* Rest of spells */
	switch (spell)
	{
	case MKNIGHT_BLINK:
		{
			teleport_player(10);
			break;
		}
	
	case MKNIGHT_HEROISM:
		{
			(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
			break;
		}

	case MKNIGHT_BERSERK:
		{
			(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
			break;
		}

	case MKNIGHT_HASTE:
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

	case MKNIGHT_BURST:
		{
			msg_print("You jump up and flash gold.");
				fire_ball(GF_GRAVITY, 0,
				          0, 6);
				(void)set_meter(p_ptr->c_meter + p_ptr->lev * 2);
			break;
		}

	case MKNIGHT_DETECTION:
		{
			(void)detect_all();
			break;
		}

	case MKNIGHT_ALTER_REALITY:
		{

			msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

			break;
		}

	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Use mana */
	/* Hack -- Wu Transform */
	if (!(p_ptr->wu_transform)){
	p_ptr->csp -= mana_cost[spell];
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	



}

/*
 * Use a magical power  -- believe me, this will definitely be
 * rewritten.
 */
void do_cmd_sentai_power(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int i, spell;
	monster_race *r_ptr;
	monster_type *m_ptr;

	char out_val[160];
	char m_name[80];
	char choice;

	/* Names of Magical Student Powers */
cptr mag_powers[SENTAI_MAX] =
{
	"Taunt",
	"Pose",
	"Quick Return",
	"Pyrotechnics",
	"Heroism",
	"Dash",
	"Giant Swing!",
	"Double Chop!",
	"The END!",
	"Summon Mecha",
};

int power_mana[STUDENT_MAX] = {0,0,100,200,300,500,500,500,1,5000};
	byte line_attr = TERM_WHITE;

	/* int spell, dir;*/
	
	object_type *mech_ptr = &inventory[INVEN_MECHA];
	object_type *i_ptr;
	object_type object_type_body;

	



/*	bool verify; */

	bool flag = FALSE; 
	bool redraw = FALSE;
	/* bool okay;*/

	int plev = p_ptr->lev;

/*	const power_type *s_ptr; */

		/* Get local object */
	i_ptr = &object_type_body;


	/* Must not be confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	
	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();

	/* Title the list */
	prt("", 1, 20);
	put_str("Name", 1, 25);
	put_str("Meter", 1, 55);

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, ESC=exit) Use which Power? ",
	        I2A(0), I2A(STUDENT_MAX - 1));
	c_prt(line_attr, out_val, 0, 0);
	/* Dump the spells */
	for (i = 0; i < STUDENT_MAX; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), mag_powers[i], power_mana[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}

	/* Clear the bottom line */
	prt("", 1 + i + 1, 20);
		
	
	/* Choose */
	while (1)
	{
	

		choice = inkey();
		spell = (islower(choice) ? A2I(choice) : -1);
		if (choice == ESCAPE){
			screen_load();
			return;
		}
		if ((spell >= 0) && (spell < STUDENT_MAX)) break;
		else bell("Illegal POWER!");
	}
	
	
	screen_load();
	/* Verify adequate mana */
	if (power_mana[spell] > p_ptr->c_meter)
	{
		/* Warning */
		msg_print("You do not have enough meter to use that power!.");
		
		return;
	}

	switch (spell)
	{
	case SENTAI_TAUNT: {
			do_cmd_taunt();
			break;
		}

	case SENTAI_POSE:	{
		do_cmd_pose();
				break;
			}

	case SENTAI_QUICK_RETURN:			{

		if (p_ptr->location != W_TOWN)
		{
			msg_print("That command does not work here.");
			return;
		}
		else
		{
		p_ptr->word_recall = 1;
		}
				break;
		
			}

	case SENTAI_PYROTECHNICS:		{

		if (target_set_interactive(TARGET_KILL))
		{
		
			if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}

		}
			else
		{
			return;
		}

		msg_print("You jump over and throw some bombs!");
		teleport_player_to(p_ptr->target_row, p_ptr->target_col);
			fire_ball(GF_CONFUSION, 0, 0, 10);
			fire_ball(GF_SOUND, 0, 0, 10);
				break;
			}

	case SENTAI_HEROISM:	{
		(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
			break;
		}

	case SENTAI_GIANT_SWING:	  {
		if (target_set_interactive(TARGET_KILL))
		{
		
			if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}
		}

			else
		{
			return;
		}

	/* Require Adjacency */
	if ((p_ptr->target_row - py > 1) || (p_ptr->target_row - py < -1)
		|| (p_ptr->target_col - px > 1) || (p_ptr->target_col - px < -1))
	{
		msg_print("Target is not adjacent.");
		return;
	}

	m_ptr = &m_list[cave_m_idx[p_ptr->target_row][p_ptr->target_col]];
	/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

	msg_format("You yell, 'Giant Swing!' as you grab and swing %s away!", m_name);
	fire_bolt(GF_DASH, 5, p_ptr->lev);
		


			break;
		}

	case SENTAI_DOUBLE_CHOP:		{

		if (target_set_interactive(TARGET_KILL))
		{
		
			if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}
		}
		else
		{
			return;
		}

	/* Require Adjacency */
	if ((p_ptr->target_row - py > 1) || (p_ptr->target_row - py < -1)
		|| (p_ptr->target_col - px > 1) || (p_ptr->target_col - px < -1))
	{
		msg_print("Target is not adjacent.");
		return;
	}

	msg_print("You yell, 'Double Chop!'");
	fire_bolt(GF_MANA, 5, p_ptr->lev * 2);
	fire_bolt(GF_MANA, 5, p_ptr->lev * 2);
		
				break;

			}


	case SENTAI_THE_END:		{
		if (target_set_interactive(TARGET_KILL))
		{
		
			if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}
		}
		
		else
		{
			return;
		}

	/* Require Adjacency */
	if ((p_ptr->target_row - py > 1) || (p_ptr->target_row - py < -1)
		|| (p_ptr->target_col - px > 1) || (p_ptr->target_col - px < -1))
	{
		msg_print("Target is not adjacent.");
		return;
	}

	msg_print("You yell, 'The END!'");
	m_ptr = &m_list[cave_m_idx[p_ptr->target_row][p_ptr->target_col]];
	r_ptr = &r_info[m_ptr->r_idx];
		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);
	

	/* Check HP and Unique status */
	
	if ((m_ptr->hp > m_ptr->maxhp / 4) || (r_ptr->flags1 & (RF1_UNIQUE)))
	{
		msg_format("%s blocks your attack!", m_name);
	}

	else {
		message_format(MSG_HIT, m_ptr->r_idx, "%^s%s", m_name, " explodes!");
		fire_bolt(GF_MANA, 5, m_ptr->hp + 1);
	}
		
		
			
		break;
				}

	case SENTAI_DASH:	{

		if (target_set_interactive(TARGET_KILL))
		{
		
			if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}

		}

		else
		{
			return;
		}

		msg_print("You dash!");

		fire_beam(GF_DASH, 5, 0);
		teleport_player_to(p_ptr->target_row, p_ptr->target_col);

		break;

		}

	case SENTAI_SUMMON_MECHA:	
		{

				if (!(mech_ptr->k_idx))
				{
					/* Create Mecha */
				object_prep(i_ptr, lookup_kind(TV_TEMP_MECHA, SV_TEMP_MECHA));
				i_ptr->number = 1;
				
				object_aware(i_ptr);
				object_known(i_ptr);
				object_copy(&inventory[INVEN_MECHA], i_ptr);

				msg_format(mecha_summon_quotes[rand_int(5)], p_ptr->temp_mecha_name);
				p_ptr->total_weight += i_ptr->weight;
					
					/* Redraw */
				p_ptr->redraw |= (PR_HP);

				/* Redraw */
				p_ptr->redraw |= (PR_METER);

				/* Hack -- Ensure that the mechas has at least 2500 hp */
				p_ptr->c_meter += 2500;
				}
				
				else
				{
					msg_print("You already have a Mecha!");
					return;
				}
							break;
						}
	}


	

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Use meter */
	p_ptr->c_meter -= power_mana[spell];
	

	/* Redraw meter */
	p_ptr->redraw |= (PR_METER);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}


/*
 * Use a magical power  -- believe me, this will definitely be
 * rewritten.
 */
void do_cmd_power(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int i, spell;

	char out_val[160];
	char choice;

	/* Names of Magical Student Powers */
cptr mag_powers[STUDENT_MAX] =
{
	"Magic Missile",
	"Phase Door",
	"Heroism",
	"Haste",
	"Ouroborous",
	"Gaijin Smash!",
	"Teleport",
	"Ki Beam",
	"Ultimate Restoration",
	"Genei Jin",
};

int power_mana[STUDENT_MAX] = {1,5,15,20,25,30,40,50,70,100};
	byte line_attr = TERM_WHITE;

	/* int spell, dir;*/
	int dir;

/*	bool verify; */

	bool flag = FALSE; 
	bool redraw = FALSE;
	/* bool okay;*/

	int plev = p_ptr->lev;

/*	const power_type *s_ptr; */

	


	/* Must be a student */
	if ((cp_ptr->spell_book != TV_STUDENT_BOOK) || (!p_ptr->mag_student))
	{
		msg_print("You are not a magical student!");
		return;
	}

	/* Must not be confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	
	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();

	/* Title the list */
	prt("", 1, 20);
	put_str("Name", 1, 25);
	put_str("Mana", 1, 55);

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, ESC=exit) Use which Power? ",
	        I2A(0), I2A(STUDENT_MAX - 1));
	c_prt(line_attr, out_val, 0, 0);
	/* Dump the spells */
	for (i = 0; i < STUDENT_MAX; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), mag_powers[i], power_mana[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}

	/* Clear the bottom line */
	prt("", 1 + i + 1, 20);
		
	
	/* Choose */
	while (1)
	{
	

		choice = inkey();
		spell = (islower(choice) ? A2I(choice) : -1);
		if (choice == ESCAPE){
			screen_load();
			return;
		}
		if ((spell >= 0) && (spell < STUDENT_MAX)) break;
		else bell("Illegal POWER!");
	}
	
	
	screen_load();
	/* Verify adequate mana */
	if (power_mana[spell] > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use that power!.");
		
		return;
	}

	switch (spell)
	{
	case STUDENT_MAGIC_MISSILE: {
		if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir,
				           damroll(3 + ((plev - 1) / 5), 4));
			break;
		}

	case STUDENT_PHASE_DOOR:	{
		teleport_player(10);
				break;
			}

	case STUDENT_HASTE:			{

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

	case STUDENT_HEROISM:		{

				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

	case STUDENT_OUROBOROUS:	{
		(void)set_ouroborous(p_ptr->ouroborous + 10 + plev/5);
			break;
		}

	case STUDENT_GAIJIN_SMASH:	  {
		msg_print("GAIJIN SMASH!");
		fire_ball(GF_CONFUSION, 0, 0, 1);
		fire_ball(GF_SOUND, 0, 0, 1);
			break;
		}

	case STUDENT_TELEPORT:		{

		teleport_player(plev * 5);
				break;

			}
	case STUDENT_KI_BEAM:		{

			if (!get_aim_dir(&dir)) return;
				fire_beam(GF_MANA, dir,
				          (plev * 3));
					break;
				}

	case STUDENT_RESTORATION:	{
				(void)hp_player(2000);
				(void)set_stun(0);
				(void)set_cut(0);
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);

						break;

					}

				case STUDENT_GENEI_JIN:				{

				(void)set_geneijin(p_ptr->geneijin + 15 + plev/10);
							break;
						}
	}


	

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Use mana */
	/* Hack -- Wu Transform */
	if (!(p_ptr->wu_transform)){
	p_ptr->csp -= power_mana[spell];
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/* Mecha Commands */
/*
 * Use a magical power  -- believe me, this will definitely be
 * rewritten.
 */
void do_cmd_mecha(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int i, spell;

	char out_val[160];
	char choice;

	/* Names of Magical Student Powers */
cptr mag_powers[MECHA_MAX] =
{
	"Fire Weapons",
	"Scuttle",
	"Rename",
	
};

	byte line_attr = TERM_WHITE;

	
	bool flag = FALSE; 
	bool redraw = FALSE;


	int plev = p_ptr->lev;

	object_type *o_ptr;
	o_ptr = &inventory[INVEN_MECHA];

	/* Check for Mechas before continuing */
	
	if (!(o_ptr->k_idx)){

		msg_print("You are not riding on a mecha.");
		return;

	}

	
	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();

	/* Title the list */
	prt("", 1, 20);
	put_str("Name", 1, 25);
	

	/* Build a prompt (accept all powers) */
	strnfmt(out_val, 78, "(Commands %c-%c, ESC=exit) Use which Mecha Command? ",
	        I2A(0), I2A(MECHA_MAX - 1));
	c_prt(line_attr, out_val, 0, 0);
	/* Dump the spells */
	for (i = 0; i < MECHA_MAX; i++)
	{
		/* Dump the command --(-- */
		sprintf(out_val, "  %c) %-30s",
		        I2A(i), mag_powers[i]);
		c_prt(line_attr, out_val, 1 + i + 1, 20);
	}

	/* Clear the bottom line */
	prt("", 1 + i + 1, 20);
		
	
	/* Choose */
	while (1)
	{
	

		choice = inkey();
		spell = (islower(choice) ? A2I(choice) : -1);
		if (choice == ESCAPE){
			screen_load();
			return;
		}
		if ((spell >= 0) && (spell < MECHA_MAX)) break;
		else bell("Illegal POWER!");
	}
	
	
	screen_load();
	

	switch (spell)
	{
	case MECHA_FIRE: {
	
		if (!do_cmd_mechfire())
		{
			return;
		}

		break;
		}

	case MECHA_SCUTTLE:	{
		/* Destroy Mecha */
			msg_print("You scuttle your Mecha!");
			inven_item_increase(INVEN_MECHA, -999);
			inven_item_optimize(INVEN_MECHA);
				/* Redraw */
				p_ptr->redraw |= (PR_HP);
				break;
			}

	case MECHA_RENAME:			{

		if (!(o_ptr->k_idx))
		{
			msg_print("You are not riding a summoned mecha!");
			return;
		}

		else if (o_ptr->tval != TV_TEMP_MECHA)
		{
			msg_print("This mecha is not renamable.");
			return;
		}

		get_mecha_info();
			return;

			break;
		
			}

	}


	

	/* Take a turn */
	p_ptr->energy_use = 100;
	

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/* Give command */
void do_cmd_give(void)
{
	int item, amt;
	int old_number;

	object_type *o_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	char o_name[80];

	cptr q, s;

	int py = p_ptr->py;
	int px = p_ptr->px;

	
	int item_new;
	monster_type *m_ptr;
	char m_name[80];

	/* Get an item */
	q = "Give which item? ";
	s = "You have nothing to give.";
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

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;



	
	/* Which creature */
	if (target_set_interactive(TARGET_KILL))
	{
		if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}

	}

		else
		{
			return;
		}

	/* Require Adjacency */
	if ((p_ptr->target_row - py > 1) || (p_ptr->target_row - py < -1)
		|| (p_ptr->target_col - px > 1) || (p_ptr->target_col - px < -1))
	{
		msg_print("Target is not adjacent.");
		return;
	}

	
	/* Found someone to give to */
	if (cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)
	{
		/* Take a turn */
		p_ptr->energy_use = 100;

		m_ptr = &m_list[p_ptr->target_who];
		monster_desc(m_name, m_ptr, 0);

		switch (m_ptr->r_idx)
		{

			case GARDENER:
			{
			if ((o_ptr->tval == TV_QUEST_ITEM) && (o_ptr->sval == SV_BAG_OF_ACORNS))
			{
				/* Mark as complete */
				p_ptr->normal_quests[QUEST_FOREST] = STATUS_COMPLETE;
				msg_print("Thank you very much!  The forest is saved!");
				break;
			
			}
			else {
				
				msg_format("I doubt %s wants that.", m_name);
				return;
				break;
				}
			}

		case OISHI:
			{
			if ((o_ptr->tval == TV_QUEST_ITEM) && (o_ptr->sval == SV_KIRA_HEAD))
			{
				/* Mark as complete */
				p_ptr->normal_quests[QUEST_CHUSHINGURA] = STATUS_COMPLETE;
				msg_print("Thank you very much!  Now our lord can rest in peace.");
				break;
			
			}

		else {
				
				msg_format("I doubt %s wants that.", m_name);
				return;
				break;
				}

			}


		case TOTORO_LEAF:
			{
			
				if (m_ptr->ml)
				{

				if ((o_ptr->tval == TV_QUEST_ITEM) && (o_ptr->sval == SV_UMBRELLA))
				{
					/* Mark as complete */
				p_ptr->normal_quests[QUEST_TOTORO] = STATUS_COMPLETE;
				msg_print("The Totoro smiles maniacally and jumps for joy!");
				msg_print("The Totoro hands you a small bag.");

				/* Get local object */
				i_ptr = &object_type_body;

				/* Mega-Hack -- Prepare to make Bag of acorns */
				object_prep(i_ptr, lookup_kind(TV_QUEST_ITEM, SV_BAG_OF_ACORNS));
				
				/* Give it to the player */
				item_new = inven_carry(i_ptr);

				/* Describe the final result */
				object_desc(o_name, &inventory[item_new], TRUE, 3);

				/* Message */
				msg_format("You have %s (%c).",
				           o_name, index_to_label(item_new));

				/* Handle stuff */
				handle_stuff();
				break;
				}

				else {
				msg_format("I doubt %s wants that.", m_name);
				return;
				break;
				}


				}

				else{
					msg_print("There is nobody there.");
					return;
					break;
				}



			}
		

		default:
			{
			msg_format("I doubt %s wants that.", m_name);
			return;
			break;
			}


		}


	}

	else {
		msg_print("There is nobody there.");
		return;
	}

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
	
	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/* Talk command */
void do_cmd_talk(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	
	monster_type *m_ptr;
	char m_name[80];
	char o_name[80];
	int item_new;
	
/*
	object_type *i_ptr;
	object_type object_type_body;
	*/

	/* Which creature */
	if (target_set_interactive(TARGET_KILL))
	{
		if (!(cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)){

			msg_print("Target is not a valid monster.");
			return;
		}

	}

		else
		{
			return;
		}

	/* Require Adjacency */
	if ((p_ptr->target_row - py > 1) || (p_ptr->target_row - py < -1)
		|| (p_ptr->target_col - px > 1) || (p_ptr->target_col - px < -1))
	{
		msg_print("Target is not adjacent.");
		return;
	}
	

	/* Found someone to talk to */
	if (cave_m_idx[p_ptr->target_row][p_ptr->target_col] > 0)
	{
		m_ptr = &m_list[p_ptr->target_who];
		monster_desc(m_name, m_ptr, 0);

		switch (m_ptr->r_idx)
		{

		case TOTORO_LEAF:
			{
			
				if (m_ptr->ml)
				{

				if (p_ptr->normal_quests[QUEST_TOTORO] != STATUS_COMPLETE)
				msg_print("The Totoro sniffles as water drops on its nose.");

				else
				msg_print("The Totoro smiles rather eeriely...");

				break;

				}

				else{
					msg_print("All you hear are dripping sounds.");
					break;
				}

			}

		case OISHI:
			{

		if (p_ptr->normal_quests[QUEST_CHUSHINGURA] != STATUS_COMPLETE)
		{
			msg_print("Damn that Kira!");
			msg_print("Ever since that fateful day when our Lord Asano");
			msg_print("was punished by death for drawing his sword against");
			msg_print("Kira, the Ako have been disbanded.  We swore an");
			msg_print("unceasing vendetta against him.  Please help us!");
		}

		else
		{
			msg_print("Thank you!");
		}
			break;

			}

		case KIKI:
			{
				msg_print("Hello!");

				/*
			msg_print("Oh dear!  My broom is broken!");
			msg_print("Please deliver this for me.  Auhu needs her");
			msg_print("cake for her 16th birthday.");
			*/
			break;

			}

		case ARAIGUMA_RASCAL:
			{
			msg_print("Rascal drops a treasure and scuttles off!");
			acquirement(py, px, 1, TRUE);
		/* Delete the monster */
		delete_monster_idx(cave_m_idx[p_ptr->target_row][p_ptr->target_col]);
		break;

			}

		case DELIVERY_BOY:
			{
				msg_print("The delivery boy hands you your requested item.");

			
				/* Give it to the player */
				item_new = inven_carry(p_ptr->delivery_ptr);

				/* Describe the final result */
				object_desc(o_name, &inventory[item_new], TRUE, 3);

				/* Message */
				msg_format("You have %s (%c).",
				           o_name, index_to_label(item_new));
				delete_monster_idx(cave_m_idx[p_ptr->target_row][p_ptr->target_col]);
				object_wipe(p_ptr->delivery_ptr);
				
				break;
			}

		case GARDENER:
			{
				if (p_ptr->normal_quests[QUEST_FOREST] != STATUS_COMPLETE)
				{
					msg_print("I wish the Totoro were here.");
					msg_print("This forest is dying and their acorns");
					msg_print("would be of great use right now.");
					msg_print("Have you come to take on the pagoda of masters?");
					msg_print("I hope you're at least level 30.");
				}
				else{
				msg_print("Thank you!");
				msg_print("Have you come to take on the pagoda of masters?");
					msg_print("I hope you're at least level 30.");
				}
				break;
			}
		case INFO_BOY:
			{

			if (p_ptr->location == W_FUN_CITY)
			{

			msg_print("Welcome to Fun City!");
			msg_print("It is a carnival every day here!");
			msg_print("Our main attraction is the Dueling Arena.");
			msg_print("You can get challenged to duels or train your character there.");
			msg_print("Go check it out!");
			break;

			}

			else if (p_ptr->location == W_PUZZLE_LAND)
			{

			msg_print("Over there is Happy Fun Puzzle Land.");
			msg_print("Legend has it that there is a really awesome weapon somewhere in there.");
			msg_print("However....nobody has ever returned from entering Happy Fun Puzzle Land.");
			msg_print("Stay away, if you know what's best for you!");
			msg_print("It is rumored that many deep and rare creatures live there, so please, don't");
			msg_print("even think of entering unless you can deal with creatures that live deeper");
			msg_print("than 5000 feet.");
			break;

			}

			else if (p_ptr->location == W_PAGODA_ROOF)
			{

			msg_print("Congrats!  You have learned that the best fighting style ");
			msg_print("is no style at all.  You are truly a martial arts master.");
			msg_print("Go in peace.");
			break;

			}

			else if (p_ptr->location == W_TOKYO_TOWER)
			{

			if (p_ptr->normal_quests[QUEST_TOKYO_TOWER] != STATUS_COMPLETE)
			{
			msg_print("Please Help!");
			msg_print("This is the last point that the Land Dragons need ");
			msg_print("in order to destroy the world!  Fuma Monou is already ");
			msg_print("threatening it as we speak.  He's on the roof.  Please");
			msg_print("defend this place!");
			}

			else
			{
				msg_print("Thank you!");
			}
			break;

			}

			else
			{

				msg_print("Why...Why did the RNG place me here?!");
				break;

			}



			}

		default:
			{
			msg_format("Hmm.... %s does not like you very much.", m_name);
			break;
			}


		}


	}

	else {
		msg_print("You see nobody there to talk to.");
	}
	/* Take a turn */
	p_ptr->energy_use = 100;
	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

void raging_demon(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;
	int counter;
	int counter2;
	int starcounter;
	bool fear = FALSE;

	monster_type *m_ptr;
	
	char m_name[80];


	for (counter = 0; counter <10; counter++)
	{
	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
	/* Save screen */
	screen_save();

	/* Clear screen */
		Term_clear();

		/* Attack */
		m_ptr = &m_list[cave_m_idx[y][x]];
		monster_desc(m_name, m_ptr, 0);
		
		/* Auto-Recall if possible and visible */
		if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
		if (m_ptr->ml) health_track(cave_m_idx[y][x]);

		for (counter2 = 0; counter2 < 15; counter2++){
			message_format(MSG_HIT, m_ptr->r_idx, "You hit %s.", m_name);
			for (starcounter = 0; starcounter < 9; starcounter++){
				c_put_str((byte)rand_int(16), "*", rand_int(24), rand_int(70));
			}
			
		}
				
		
		/* Redraw Screen */
		screen_load();
		if(!(mon_take_hit(cave_m_idx[y][x], p_ptr->lev * 100, &fear, NULL))){
		message_format(MSG_FLEE, m_ptr->r_idx, "%^s crumples to the ground.", m_name);
		msg_print("Sauce......");
		/* Load screen */
		}
/*
		else
		{
			/* Draw Kanji for 'Ten' - For now I'll skip
		for (starcounter = 0; starcounter<15; starcounter++){
			c_put_str(TERM_RED, "*", 8,35 + starcounter);
			}
		for (starcounter = 0; starcounter < 11; starcounter++){
			c_put_str(TERM_RED, "*", 12,37 + starcounter);
			}
		for (starcounter = 0; starcounter<4; starcounter++){
			c_put_str(TERM_RED,"*",9 + starcounter,42);
			
		}
		for (starcounter = 0; starcounter<6; starcounter++){
			c_put_str(TERM_RED, "*", 13+starcounter, 42-starcounter);
			c_put_str(TERM_RED, "*", 13+starcounter, 42+starcounter);
		}
		
		

		}
		*/

		return;
	}


	/* Player can not walk through "walls" */
	else if (!cave_floor_bold(y, x))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Notice unknown obstacles */
		if (!(cave_info[y][x] & (CAVE_MARK)))
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "You feel a pile of rubble blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
				return;
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{
				message(MSG_HITWALL, 0, "You feel a door blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
				return;
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "You feel a wall blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
				return;
			}
		}

		/* Mention known obstacles */
		else
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "There is a pile of rubble blocking your way.");
				return;
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{
				message(MSG_HITWALL, 0, "There is a door blocking your way.");
				return;
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "There is a wall blocking your way.");
				return;
			}
		}
	}

	/* Normal movement */
	else
	{
		/* Sound XXX XXX XXX */
		/* sound(MSG_WALK); */

		/* Move player */
		monster_swap(py, px, y, x);

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;

				
		
		/* Discover invisible traps */
		if (cave_feat[y][x] == FEAT_INVIS)
		{
			/* Disturb */
			disturb(0, 0);

			/* Message */
			msg_print("You found a trap!");

			/* Pick a trap */
			pick_trap(y, x);

			/* Hit the trap */
			hit_trap(y, x);
			return;
		}

		/* Set off an visible trap */
		else if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		         (cave_feat[y][x] <= FEAT_TRAP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_trap(y, x);
			return;
		}

		/* Set off a teleproter */
		else if ((cave_feat[y][x] >= FEAT_TELE_1) &&
		         (cave_feat[y][x] <= FEAT_FUN_TELE))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_tele(y, x);
			return;
		}
		else if (cave_feat[y][x] == FEAT_TRIGGER)
		{
			disturb(0,0);
			if (p_ptr->location == W_MASTERMIND)
			{
			hit_trigger(y,x);
			}

			else{
				hit_trigger2(y,x);
			}

			return;
		}
	}
	}
return;
}


