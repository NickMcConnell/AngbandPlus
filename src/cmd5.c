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
void do_cmd_cast(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir;
	int chance, beam;
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
				(void)hp_player(damroll(2, 8));
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

/* Doing a "Super" (Limit Break) */
void do_cmd_super(void)
{
 
int plev = p_ptr->lev;	
int dir;
int rand_limit = rand_int(LB_RANDOM);
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
			fire_ball(GF_FIRE, 0,
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
			if (!get_aim_dir(&dir)) return;
			raging_demon(dir);
			break;
			/*
		p_ptr->word_recall = 1;
		break;
		*/


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
				(void)hp_player(damroll(2, 10));
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
				(void)hp_player(damroll(4, 10));
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
				(void)hp_player(damroll(6, 10));
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
				(void)hp_player(damroll(4, 10));
				(void)set_cut(0);
				break;
			}

			case PRAYER_CURE_MORTAL_WOUNDS2:
			{
				(void)hp_player(damroll(8, 10));
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

/* Taunt */
void do_cmd_taunt(void)
{

	int counter = randint(4);

	switch (counter)
	{
	case 1: { msg_print("You shout out a challenge."); break; }
	case 2: { msg_print("You yawn."); break; }
	case 3: { msg_print("You laugh out loud."); break; }
	case 4: { msg_print("You make a raspberry while pulling your lower eyelid down."); break; }
	}
	aggravate_monsters(-1);

	
		/* Take a turn */
	p_ptr->energy_use = 100;


}

/* Fire Weapons from Mech */
void do_cmd_mechfire(void)
{
	int dir;
	int counter;
	object_type *o_ptr;
	o_ptr = &inventory[INVEN_MECHA];

	if (!(o_ptr->k_idx)){

		msg_print("You are not riding on a mecha.");
		return;

	}

	switch (o_ptr->sval){

	case SV_GUNDAM:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You fire your machine gun.");

			for (counter = 0; counter < 4; counter++){
			fire_bolt(GF_METEOR, dir, 15);
			}

			break;

		}
	case SV_EVA:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You fire your laser cannon.");
			fire_beam(GF_METEOR, dir, 60);
			break;
		}

	case SV_RAYEARTH:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You shoot a Fire Arrow!");
			fire_bolt(GF_FIRE, dir, 60);
			break;
		}

	case SV_SELECE:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You shoot a Sapphire Whirlwind!");
			fire_ball(GF_WATER, dir, 60, 5);
			break;
		}

	case SV_WINDAM:
		{
			if (!get_aim_dir(&dir)) return;
			msg_print("You shoot a Emerald Typhoon!");
			fire_ball(GF_SHARD, dir, 60, 10);
			break;
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	
	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/* Mimic something */
void do_cmd_mimic(void)
{
	int x, y;

	int dir;
	monster_type *m_ptr;
	monster_race *r_ptr;
	char m_name[80];

	
	/* Get a direction */
	if (!get_aim_dir(&dir)) return;

	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Adjacent monster? */
	if (!(cave_m_idx[y][x] > 0))
	{
		msg_print("You see no monster to mimic.");
		return;
	}
	/* Otherwise, we're mimicing something */
	p_ptr->mimic = TRUE;
	p_ptr->max_powers = 0;

	/* Get monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	p_ptr->mimic_idx = m_ptr->r_idx;

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	message_format(MSG_HIT, m_ptr->r_idx, "You mimic %s.", m_name);
	

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

		if (r_ptr->flags5 & (RF5_DRAIN_MANA))
		{
			if (p_ptr->max_powers >= STUDENT_MAX)
		{
			/* Do not extract */
			
		}

		else {
			p_ptr->player_powers[p_ptr->max_powers] = MIMIC_DRAIN_MANA;
			p_ptr->max_powers++;
		}
		}

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
		
		p_ptr->mimic = TRUE;
					/* Recalculate bonuses */
					p_ptr->update |= (PU_BONUS);
					/* Take a turn */
					p_ptr->energy_use = 100;

					handle_stuff();
					return;

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
	int wrath;

	char tmp_val[160];

	long tmp_long;
	

	char choice;
	char out_val[160];

	int plev = p_ptr->lev;
	int beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

	

	byte line_attr = TERM_WHITE;

	bool flag = FALSE; 
	bool redraw = FALSE;

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
				break;
		}

	case CHI_WARRIOR_BAKUSAI_TENGETSU:
		{
				msg_print("You assume a Bakusai Tengetsu stance.");
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

	case CHI_WARRIOR_WRATH:
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
		/*	for (i = 0; i < 10; i++)
			fire_beam(rand_int(32), i, plev * 5);*/
			break;
		}

	case CHI_WARRIOR_DETECTION:
		{
			(void)detect_all();
			break;
		}

	case CHI_WARRIOR_SUPER_PUNCH:
		{
			msg_print("Your hands begin to glow brightly.");
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
			msg_print("You start to glow.");
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
	p_ptr->csp -= chi_warrior_cost[p_ptr->player_powers[spell]];

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
	put_str("Mana", 1, 55);

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, ESC=exit) Use which Power? ",
	        I2A(0), I2A(p_ptr->max_powers - 1));
	c_prt(line_attr, out_val, 0, 0);
	/* Dump the spells */
	for (i = 0; i < p_ptr->max_powers; i++)
	{
		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %4d",
		        I2A(i), mimic_powers[p_ptr->player_powers[i]], mimic_powers_cost[p_ptr->player_powers[i]]);
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
		else bell("Illegal POWER!");
	}
	
	
	screen_load();
	/* Verify adequate mana */
	if (mimic_powers_cost[p_ptr->player_powers[spell]] > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use that power!.");
		
		return;
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

	case MIMIC_DRAIN_MANA:
		{
			msg_print("I won't work until someone does Monster Mana!");
			msg_print("So...hah :P");
			break;

		}

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
	p_ptr->csp -= mimic_powers_cost[p_ptr->player_powers[spell]];

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

/* Cost */
int mana_cost[3] = {5, 10, 20};
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

	}

	while (1)
	{
	

		choice = inkey();
		spell = (islower(choice) ? A2I(choice) : -1);
		if (choice == ESCAPE){
			screen_load();
			return;
		}
		if ((spell >= 0) && (spell < 3)) break;
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

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Use mana */
	p_ptr->csp -= mana_cost[spell];

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

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
	"Terrible Engrish",
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

	case STUDENT_POOR_DUBBING:	  {
		msg_print("You start to speak in terrible Engrish!");
		for (dir = 1; dir < 10; dir++){
		(void)fear_monster(dir, plev);
		}
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
	p_ptr->csp -= power_mana[spell];

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

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

	int y, x;
	int dir;
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



	/* Which direction */
	if (!get_aim_dir(&dir)) return;

	/* Get direction results */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Found someone to give to */
	if (cave_m_idx[y][x] > 0)
	{
		/* Take a turn */
		p_ptr->energy_use = 100;

		m_ptr = &m_list[cave_m_idx[y][x]];
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
				
				message_format(MSG_HIT, m_ptr->r_idx, "I doubt %s wants that.", m_name);
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
				
				message_format(MSG_HIT, m_ptr->r_idx, "I doubt %s wants that.", m_name);
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
				message_format(MSG_HIT, m_ptr->r_idx, "I doubt %s wants that.", m_name);
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
			message_format(MSG_HIT, m_ptr->r_idx, "I doubt %s wants that.", m_name);
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

	int y, x;
	int dir;
	monster_type *m_ptr;
	char m_name[80];

	/* Which direction */
	if (!get_aim_dir(&dir)) return;

	/* Get direction results */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Found someone to talk to */
	if (cave_m_idx[y][x] > 0)
	{
		m_ptr = &m_list[cave_m_idx[y][x]];
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

		case GARDENER:
			{
				if (p_ptr->normal_quests[QUEST_FOREST] != STATUS_COMPLETE)
				{
					msg_print("I wish the Totoro were here.");
					msg_print("This forest is dying and their acorns");
					msg_print("would be of great use right now.");
				}
				else{
				msg_print("Thank you!");
				}
				break;
			}
		case INFO_BOY:
			{

			if (p_ptr->location == W_FUN_CITY)
			{

			msg_print("Welcome to Fun City!");
			msg_print("It is a carnival every day here!");
			msg_print("Our main attraction is the World Martial Arts Tourney.");
			msg_print("The winner gets to be known as the world martial arts champion.");
			msg_print("All you have to do is defeat the current world champ!");
			msg_print("Go check it out!");
			break;

			}

			else if (p_ptr->location == W_PUZZLE_LAND)
			{

			msg_print("Over there is Happy Fun Puzzle Land.");
			msg_print("Legend has it that there is a really awesome weapon somewhere in there.");
			msg_print("However....nobody has ever returned from entering Happy Fun Puzzle Land.");
			msg_print("Stay away, if you know what's best for you!");
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
			message_format(MSG_HIT, m_ptr->r_idx, "Hmm.... %s does not like you very much.", m_name);
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


