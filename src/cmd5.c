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

	cptr p = ((mp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");


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

	cptr p = ( ( mp_ptr->spell_book == TV_PRAYER_BOOK ) ? 
		"prayer" : "spell" );

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
	if ( ( mp_ptr->spell_book == TV_MAGIC_BOOK ) ||
		( mp_ptr->spell_book == TV_ELE_BOOK ) )
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
 * Generic "cast spell" function for all spellcasters
 */
void do_cmd_spell(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int item, sval, spell, dir;
	int chance, beam;

	int plev = p_ptr->lev;

	object_type* o_ptr;
	magic_type* s_ptr;

	cptr q, s;

	int pspellbook = mp_ptr->spell_book;

	/* Ensure player can cast spells */
	if ( !pspellbook )
	{
		msg_print("You cannot cast spells!");
		return;
	}

	/* Require light */
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
	item_tester_tval = pspellbook;

	/* Get an item */
	q = "Use which book? ";

	s = (pspellbook == TV_PRAYER_BOOK) ?
		"You have no prayer books" : "You have no spell books!";

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
	if (!get_spell(&spell, (pspellbook == TV_PRAYER_BOOK) ?
		"recite" : "cast", sval, TRUE))
	{
		if (spell == -2) msg_print(
			(pspellbook == TV_PRAYER_BOOK) ?
			"You don't know any prayers in that book." :
			"You don't know any spells in that book." );
		return;
	}


	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];


	if (pspellbook == TV_ELE_BOOK) {
		if (s_ptr->smana > p_ptr->chp) {
			msg_print( "Casting this spell would kill you!" );
			return;
		}
	} else {
		/* Verify "dangerous" spells */
		if (s_ptr->smana > p_ptr->csp)
		{
			/* Warning */
			msg_print( (pspellbook == TV_PRAYER_BOOK) ?
			"You do not have enough mana to recite this prayer." :
			"You do not have enough mana to cast this spell." );

			/* Verify */
			if (!get_check("Attempt it anyway? ")) return;
		}
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print( (pspellbook == TV_PRAYER_BOOK) ?
			"You failed to concentrate enough!" :
			"You failed to get the spell off!");
	}

	/* Process spell */
	else
	{
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = ( ( p_ptr->pclass == CLASS_MAGE ) ||
			( p_ptr->pclass == CLASS_DRUID ) ) ?
			plev : ( plev / 2 );

		/* Spells	0-99 for Mage/Rogue/Ranger,
				100-199 for Priest/Paladin
				200-299 for Druid */
		switch ( (pspellbook == TV_MAGIC_BOOK) ? spell :
			(pspellbook == TV_PRAYER_BOOK) ? spell+100 :
			spell+200 )
		{
			case 0:		/* Magic Missile */
			case 106:	/* Force Hammer */
			case 200:	/* Elemental Force */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
				break;
			}

			case 1:		/* Detect Monsters */
			case 131:	/* Detect Monsters */
			case 212:	/* Detect Monsters */
			{
				(void)detect_monsters_normal();
				break;
			}

			case 2:		/* Phase Door */
			case 152:	/* Blink */
			{
				teleport_player(10);
				break;
			}

			case 3:		/* Light Area */
			case 104:	/* Call Light */
			case 201:	/* Light Area */
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 4:		/* Treasure Detection */
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case 5:		/* Cure Light Wounds */
			{
				(void)hp_player(damroll(2, 8));
				(void)set_cut(p_ptr->cut - 15);
				break;
			}

			case 6:		/* Object Detection */
			{
				(void)detect_objects_normal();
				break;
			}

			case 7:		/* Find Hidden Traps/Doors */
			case 105:	/* Detect Traps/Doors */
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 8:		/* Stinking Cloud */
			case 207:	/* Poison Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + (plev / 2), 2);
				break;
			}

			case 9:		/* Confuse Monster */
			case 206:	/* Confuse Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				break;
			}

			case 10:	/* Lightning Bolt */
			case 214:	/* Lightning Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				break;
			}

			case 11:	/* Trap/Door Destruction */
			case 146:	/* Unbarring Ways */
			case 235:	/* Remove Traps & Doors */
			{
				(void)destroy_doors_touch();
				break;
			}

			case 12:	/* Sleep I */
			case 213:	/* Sleep I */
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			}

			case 13:	/* Cure Poison */
			case 116:	/* Neutralize Poison */
			case 215:	/* Cure Poison */
			{
				(void)set_poisoned(0);
				break;
			}

			case 14:	/* Teleport Self */
			{
				teleport_player(plev * 5);
				break;
			}

			case 15:	/* Spear of Light */
			case 217:	/* Light Spear */
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
				break;
			}

			case 16:	/* Frost Bolt */
			case 222:	/* Frost Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				break;
			}

			case 17:	/* Turn Stone to Mud */
			case 225:	/* Turn Stone to Mud */
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 18:	/* Satisfy Hunger */
			case 113:	/* Satisfy Hunger */
			case 223:	/* Satisfy Hunger */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 19:	/* Recharge Item I */
			{
				(void)recharge(5);
				break;
			}

			case 20:	/* Sleep II */
			case 221:	/* Sleep II */
			{
				(void)sleep_monsters_touch();
				break;
			}

			case 21:	/* Polymorph Other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}

			case 22:	/* Identify */
			case 133:	/* Perception */
			case 231:	/* Identify */
			{
				(void)ident_spell();
				break;
			}

			case 23:	/* Sleep III */
			case 229:	/* Sleep III */
			{
				(void)sleep_monsters();
				break;
			}

			case 24:	/* Fire Bolt */
			case 227:	/* Fire Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case 25:	/* Slow Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case 26:	/* Frost Ball */
			case 239:	/* Frost Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          30 + (plev), 2);
				break;
			}

			case 27:	/* Recharge Item II */
			{
				(void)recharge(40);
				break;
			}

			case 28:	/* Teleport Other */
			case 154:	/* Teleport Other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 29:	/* Haste Self */
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

			case 30:	/* Fire Ball */
			case 240:	/* Fire Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          55 + (plev), 2);
				break;
			}

			case 31:	/* Word of Destruction */
			case 144:	/* Word of Destruction */
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 32:	/* Genocide */
			{
				(void)genocide();
				break;
			}

			case 33:	/* Door Creation */
			{
				(void)door_creation();
				break;
			}

			case 34:	/* Stair Creation */
			{
				(void)stair_creation();
				break;
			}

			case 35:	/* Teleport Level */
			case 155:	/* Teleport Level */
			{
				(void)teleport_player_level();
				break;
			}

			case 36:	/* Earthquake */
			{
				earthquake(py, px, 10);
				break;
			}

			/* 37 blank */

			case 38:	/* Acid Bolt */
			case 230:	/* Acid Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case 39:	/* Cloud Kill */
			case 238:	/* Death Cloud */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          20 + (plev / 2), 3);
				break;
			}

			case 40:	/* Acid Ball */
			case 241:	/* Acid Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          40 + (plev), 2);
				break;
			}

			case 41:	/* Ice Storm */
			case 242:	/* Ice Storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          70 + (plev), 3);
				break;
			}

			case 42:	/* Meteor Swarm */
			case 243:	/* Meteor Swarm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
				          65 + (plev), 3);
				break;
			}

			case 43:	/* Mana Storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          300 + (plev * 2), 3);
				break;
			}

			case 45:	/* Detect Enchantment */
			case 226:	/* Detect Enchantment */
			{
				(void)detect_objects_magic();
				break;
			}

			case 46:	/* Recharge Item III */
			{
				recharge(100);
				break;
			}

			case 47:	/* Genocide */
			{
				(void)genocide();
				break;
			}

			case 48:	/* Mass Genocide */
			{
				(void)mass_genocide();
				break;
			}

			case 49:	/* Resist Fire */
			case 209:	/* Resist Fire */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				break;
			}

			case 50:	/* Resist Cold */
			case 210:	/* Resist Cold */
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case 51:	/* Resist Acid */
			case 211:	/* Resist Acid */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				break;
			}

			case 52:	/* Resist Poison */
			case 218:	/* Resist Poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 53:	/* Resistance */
			case 244:	/* Resist Elements */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 54:	/* Heroism */
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 55:	/* Shield */
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

			case 56:	/* Berserker */
			{
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 57:	/* Essence of Speed */
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

			case 58:	/* Globe of Invulnerability */
			{
				(void)set_invuln(p_ptr->invuln + randint(8) + 8);
				break;
			}

			case 44:	/* Detect Evil */
			case 100:	/* Detect Evil */
			case 204:	/* Detect Evil */
			{
				(void)detect_monsters_evil();
				break;
			}

			case 101:	/* Cure Light Wounds */
			{
				(void)hp_player(damroll(2, 10));
				(void)set_cut(p_ptr->cut - 10);
				break;
			}

			case 102:	/* Bless */
			{
				(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				break;
			}

			case 103:	/* Remove Fear */
			case 202:	/* Remove Fear */
			{
				(void)set_afraid(0);
				break;
			}

			case 107:	/* Slow Poison */
			case 203:	/* Slow Poison */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 108:	/* Scare Monster */
			case 216:	/* Scare Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				break;
			}

			case 109:	/* Portal */
			{
				teleport_player(plev * 3);
				break;
			}

			case 110:	/* Cure Serious Wounds */
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case 111:	/* Chant */
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				break;
			}

			case 112:	/* Sanctuary */
			{
				(void)sleep_monsters_touch();
				break;
			}

			case 114:	/* Remove Curse */
			case 219:	/* Remove Curse */
			{
				remove_curse();
				break;
			}

			case 115:	/* Resist Heat and Cold */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

			case 117:	/* Orb of Draining */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          (damroll(3, 6) + plev +
				           (plev / ((p_ptr->pclass == 2) ? 2 : 4))),
				          ((plev < 30) ? 2 : 3));
				break;
			}

			case 118:	/* Cure Critical Wounds */
			{
				(void)hp_player(damroll(6, 10));
				(void)set_cut(0);
				break;
			}

			case 119: /* Sense Invisible */
			case 205: /* Detect Invisible */
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				break;
			}

			case 120:	/* Protection from Evil */
			case 224:	/* Protection from Evil */
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			}

			case 121:	/* Earthquake */
			{
				earthquake(py, px, 10);
				break;
			}

			case 122:	/* Sense Surroundings */
			case 228:	/* Sense Surroundings */
			{
				map_area();
				break;
			}

			case 123:	/* Cure Mortal Wounds */
			case 137:	/* Cure Mortal Wounds */
			{
				(void)hp_player(damroll(8, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 124:	/* Turn Undead */
			case 220:	/* Turn Undead */
			{
				(void)turn_undead();
				break;
			}

			case 125:	/* Prayer */
			{
				(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				break;
			}

			case 126:	/* Dispel Undead */
			case 232:	/* Dispel Undead */
			{
				(void)dispel_undead(randint(plev * 3));
				break;
			}

			case 127:	/* Heal */
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 128:	/* Dispel Evil */
			case 233:	/* Dispel Evil */
			{
				(void)dispel_evil(randint(plev * 3));
				break;
			}

			case 129:	/* Glyph of Warding */
			{
				warding_glyph();
				break;
			}

			case 130:	/* Holy Word */
			{
				(void)dispel_evil(randint(plev * 4));
				(void)hp_player(1000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 132:	/* Detection */
			{
				(void)detect_all();
				break;
			}

			case 134:	/* Probing */
			{
				(void)probing();
				break;
			}

			case 135:	/* Clairvoyance */
			case 251:	/* Clairvoyance */
			{
				wiz_lite();
				break;
			}

			case 136:	/* Cure Serious Wounds */
			{
				(void)hp_player(damroll(4, 10));
				(void)set_cut(0);
				break;
			}

			case 138:	/* Healing */
			{
				(void)hp_player(2000);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 139:	/* Restoration */
			case 252:	/* Restoration */
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 140:	/* Remembrance */
			case 253:	/* Remembrance */
			{
				(void)restore_level();
				break;
			}

			case 141:	/* Dispel Undead */
			{
				(void)dispel_undead(randint(plev * 4));
				break;
			}

			case 142:	/* Dispel Evil */
			{
				(void)dispel_evil(randint(plev * 4));
				break;
			}

			case 143:	/* Banishment */
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			}

			case 145:	/* Annihilation */
			case 260:	/* Take Life */
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 200);
				break;
			}

			case 147:	/* Recharging */
			{
				(void)recharge(15);
				break;
			}

			case 148:	/* Dispel Curse */
			case 234:	/* Dispel Curse */
			{
				(void)remove_all_curse();
				break;
			}

			case 149:	/* Enchant Weapon */
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

			case 150:	/* Enchant Armour */
			case 258:	/* Enchant Armour */
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

			case 151:	/* Elemental Brand */
			case 208:	/* Elemental Brand */
			{
				brand_weapon();
				break;
			}

			case 153:	/* Teleport Self */
			{
				teleport_player(plev * 8);
				break;
			}

			case 245:	/* Resist Light */
			{
				(void)set_oppose(p_ptr->oppose_light + randint(20)+20, &(p_ptr->oppose_light), "light");
				break;
			}

			case 246:	/* Resist Dark */
			{
				(void)set_oppose(p_ptr->oppose_dark + randint(20)+20, &(p_ptr->oppose_dark), "darkness");
				break;
			}

			case 247:	/* Resist Confusion */
			{
				(void)set_oppose(p_ptr->oppose_conf + randint(20)+20, &(p_ptr->oppose_conf), "confusion");
				break;
			}

			case 248:	/* Resist Shards */
			{
				(void)set_oppose(p_ptr->oppose_shards + randint(20)+20, &(p_ptr->oppose_shards), "shards");
				break;
			}

			case 249:	/* Resist Time */
			{
				(void)set_oppose(p_ptr->oppose_time + randint(20)+20, &(p_ptr->oppose_time), "time");
				break;
			}

			case 250:	/* Cure Wounds */
			{
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 256:	/* Enchant Weapon To-Hit */
			{
				(void)enchant_spell( rand_int(3) + 2, 0, 0 );
				break;
			}

			case 257:	/* Enchant Weapon To-Dam */
			{
				(void)enchant_spell( 0, rand_int(3) + 2, 0 );
				break;
			}

			case 259:	/* Object Analysis */
			{
				(void)identify_fully();
				break;
			}

			default:
			{
				msg_print( "Warning: unimplemented spell." );
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

	if ( pspellbook == TV_ELE_BOOK ) {
		/* Use hp */
		p_ptr->chp -= s_ptr->smana;

		/* Redraw hp */
		p_ptr->redraw |= PR_HP;

	} else {

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
	}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}
