/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
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
static int get_spell(int *sn, cptr prompt, int sval, bool known, bool realm_2)
{
	int		i;
	int		spell = -1;
	int		num = 0;
	int		ask;

	byte		spells[64];

	bool		flag, redraw, okay;
	char		choice;

	magic_type	*s_ptr;

	char		out_val[160];

	int use_realm = (realm_2?p_ptr->realm2:p_ptr->realm1);
	cptr p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
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
		if (spell_okay(spells[i], known, use_realm-1)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	if (show_choices_main)
	{
		/* Show list */
		redraw = TRUE;
		Term_save();

		/* Display a list of spells */
		print_spells(spells, num, 1, 20, use_realm-1);
	}		
	else
	{
		/* No redraw yet */
		redraw = FALSE;
	}

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}


	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
		p, I2A(0), I2A(num - 1), prompt, p);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				/* Display a list of spells */
				print_spells(spells, num, 1, 20, use_realm-1);
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


		/* Note verify */
		ask = (isupper(choice));

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = spells[i];

		/* Require "okay" spells */
		if (!spell_okay(spell, known, use_realm-1))
		{
			bell();
			msg_format("You may not %s that %s.", prompt, p);
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Access the spell */
			s_ptr = &mp_ptr->info[use_realm-1][spell%32];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, spell_names[use_realm-1][spell%32],
				s_ptr->smana, spell_chance(spell,use_realm-1));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) Term_load();


	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
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
	int		item, sval;
	int		spell = -1;
	int		num = 0;
	byte		spells[64];
	object_type	*o_ptr;

	/* Warriors are illiterate */
	if (!(p_ptr->realm1 || p_ptr->realm2))
	{
		msg_print("You cannot read books!");
		return;
	}

	/* No lite */
	if (p_ptr->blind || (no_lite() && 
	    !((cave[py][px].feat >= FEAT_SHOP_HEAD) &&
	      (cave[py][px].feat <= FEAT_SHOP_TAIL))))
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

	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Browse which book? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no books that you can read.");
		return;
	}

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
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}

	/* Save the screen */
	Term_save();

	/* Display the spells */
	print_spells(spells, num, 1, 20, (o_ptr->tval-90));

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);

	/* Wait for key */
	(void)inkey();

	/* Restore the screen */
	Term_load();
}


/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int	i, item, sval;
	int	increment = 0;

	/* Spells of realm2 will have an increment of +32 */
	int	spell = -1;

	cptr p = ((mp_ptr->spell_book == TV_SORCERY_BOOK) ? "spell" : "prayer");

	object_type *o_ptr;


	if (!p_ptr->realm1)
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

	msg_format("You can learn %d new %s%s.", p_ptr->new_spells, p,
		(p_ptr->new_spells == 1?"":"s"));
	msg_print(NULL);


	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Study which book? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no books that you can read.");
		return;
	}

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

	if (o_ptr->tval==p_ptr->realm2+89) increment=32;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_book != TV_LIFE_BOOK)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE,(increment?TRUE:FALSE))
		&& (spell == -1)) return;
	}

	/* Priest -- Learn a random prayer */
	else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < 32; spell++)
		{
			/* Check spells in the book */
			if ((fake_spell_flags[sval] & (1L << spell)))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(spell, FALSE,
					(increment?(p_ptr->realm2)-1:(p_ptr->realm1)-1))) continue;

				/* Hack -- Prepare the randomizer */
				k++;

				/* Hack -- Apply the randomizer */
				if (rand_int(k) == 0) gift = spell;
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
	energy_use = 100;

	if (increment) spell += increment;

	/* Learn the spell */
	if (spell < 32)
	{
		spell_learned1 |= (1L << spell);
	}
	else
	{
		spell_learned2 |= (1L << (spell - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 64; i++)
	{
		/* Stop at the first empty space */
		if (spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	spell_order[i++] = spell;

	/* Mention the result */
	msg_format("You have learned the %s of %s.",
		p, spell_names
		[(increment?(p_ptr->realm2)-1:(p_ptr->realm1)-1)][spell%32]);

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
	int	item, sval, spell, dir, realm;
	int	chance, beam;
	int	plev = p_ptr->lev;
	int	increment = 0, dummy = 0;
	int	use_realm, i;
	int	ii = 0, ij = 0;
	bool	no_trump = FALSE;
	const cptr prayer = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");
	object_type	*o_ptr;
	magic_type	*s_ptr;

/*        char	ppp[80];      */
/*        char	tmp_val[160]; */

	/* Require spell ability */
	if (p_ptr->realm1 == REALM_NONE)
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

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Use which book? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_format("You have no %s books!", prayer);
		return;
	}

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

	if (o_ptr->tval == p_ptr->realm2+89) increment = 32;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (increment) realm = p_ptr->realm2-1;
	else realm = p_ptr->realm1-1;

	/* Ask for a spell */
	if (!get_spell(&spell, ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
		sval, TRUE, (increment?TRUE:FALSE)))
	{
		if (spell == -2)
		msg_format("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */
	use_realm = (increment?p_ptr->realm2:p_ptr->realm1);

	s_ptr = &mp_ptr->info[use_realm-1][spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.",
			((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
			prayer);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell,use_realm-1);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();

		msg_format("You failed to get the %s off!", prayer);

		if (o_ptr->tval == TV_CHAOS_BOOK && (randint(100)<spell))
		{
			msg_print("You produce a chaotic effect!");
			wild_magic(spell);
		}
		else if (o_ptr->tval == TV_DEATH_BOOK && (randint(100)<spell))
		{
			if ((sval == 3) && (randint(2)==1))
			{
				sanity_blast(0, TRUE);
			}
			else
			{
				msg_print("It hurts!");
				take_hit(damroll((o_ptr->sval)+1,6), "a miscast Death spell");
				if (spell>15 && randint(6)==1 && !(p_ptr->hold_life))
					lose_exp(spell * 250);
			}
		}
	}

	/* Process spell */
	else
	{

		if (p_ptr->pclass == CLASS_MAGE) beam = plev;
		else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 20;
		else beam = plev / 2;


	/* Spells.  */
	switch (realm)
	{
	case 0: /* * LIFE * */
	  switch (spell)
	  {
	   case 0: /* Detect Evil */
			(void)detect_monsters_evil();
		       break;
	   case 1: /* Cure Light Wounds */
			(void)hp_player(damroll(2 + ((plev - 2) / 6), 5));
			(void)set_cut(p_ptr->cut - 10);
		       break;
	   case 2: /* Bless */
			(void)set_blessed(p_ptr->blessed + 10 + plev);
		       break; 
	   case 3: /* Spiritual Hammer */
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam - 10, GF_HOLY_FIRE, dir,
		  damroll(3 + ((plev - 1) / 7), 4));
		break;
	   case 4: /* Call Light */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		       break;
	   case 5: /* Detect Traps + Secret Doors */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break;
	   case 6: /* Cure Medium Wounds */
			(void)hp_player(damroll(3 + ((plev - 2) / 6), 10));
			(void)set_cut((p_ptr->cut / 2) - 20);
		       break;
	   case 7: /* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
		       break;
	   case 8: /* Remove Curse */
			if (remove_curse())
				msg_print("A blue glow surrounds you.");
			break;
	   case 9: /* Turn Undead */
		(void)turn_undead();
		break;
	   case 10: /* Cure Critical Wounds */
		(void)hp_player(damroll(6 + ((plev - 5) / 3), 10));
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	   case 11: /* Sense Unseen */
		(void)set_tim_invis(p_ptr->tim_invis + (25 + ((3 * plev) / 2)));
		break;
	   case 12: /* Holy Orb */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_HOLY_FIRE, dir,
			25 + (plev + (plev / 2)), ((plev < 30) ? 2 : 3));
		break;
	   case 13: /* Protection from Evil */
		(void)set_protevil(p_ptr->protevil + randint(25) + (2 * p_ptr->lev));
		break;
	   case 14: /* Healing */
		(void)hp_player(300);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_image(0);
		break;
	   case 15: /* Rune of Protection */
		warding_glyph();
		break;
       case 16: /* Exorcism */
		(void) dispel_demons(plev * 6);
		break;
	   case 17: /* Dispel Curse */
		if (remove_all_curse())
			msg_print("A bright blue glow surrounds you.");
		break;
       case 18: /* Dispel Undead */
		(void)dispel_undead(plev * 6);
		break;
       case 19: /* 'Day of the Dove' */
		charm_monsters(plev * 2);
		break;
       case 20: /* Banish Evil */
		if (banish_evil(100))
			msg_print("The power of your god banishes evil!");
		break;
       case 21: /* Dispel Evil */
		(void)dispel_evil(plev * 5);
		break;
	   case 22: /* Holy Word */
		(void)dispel_evil(plev * 5);
		(void)hp_player(1000);
		(void)set_afraid(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_image(0);
		break;
	   case 23: /* Sea of Runes */
		warding_glyph();
		glyph_creation();
		break;
	   case 24: /* Heroism */
		(void)set_hero(p_ptr->hero + randint(25) + 25);
		(void)hp_player(10);
		(void)set_afraid(0);
		break;
	   case 25: /* Prayer */
		(void)set_blessed(p_ptr->blessed + 50 + plev);
		break;
	/* Summon Angel */
       case 26:
		if (randint(5) == 1)
		{
			if (summon_specific(py, px, plev, SUMMON_ANGEL))
				msg_print("You hear discord among the heavenly choir.");
        	}
		else
		{
			if (summon_specific_friendly(py, px, plev, SUMMON_ANGEL, TRUE))
				msg_print("You hear a heavenly choir singing joyously.");
		}
		break;
	   case 27: /* Restoration */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		break;
       case 28: /* Healing True */
		(void)hp_player(2000);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_image(0);
		break;
       case 29: /* Holy Vision */
		identify_fully();
		break;
       case 30: /* Divine Intervention */
		project(0, FALSE, 1, py, px, 777, GF_HOLY_FIRE, PROJECT_KILL, FALSE);
		dispel_monsters(plev * 5);
		slow_monsters();
		stun_monsters(plev*4);
		confuse_monsters(plev*4);
		turn_monsters(plev*4);
		stasis_monsters(plev*4);
		summon_specific_friendly(py, px, (plev * 2), SUMMON_ANGEL, TRUE);
		(void)set_shero(p_ptr->shero + randint(25) + 25);
		(void)hp_player(300);
		if (!p_ptr->fast)
		{
			(void)set_fast(randint(20 + (plev) ) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint(5));
		}
		(void)set_afraid(0);
		break;
	   case 31: /* Holy Resistance*/
		(void)set_invuln(p_ptr->invuln + randint(5) + 5);
		break;
	   default:
		 msg_format("You cast an unknown Life spell: %d.", spell);
		 msg_print(NULL);
	   }
	  break;
	
	case 1: /* * SORCERY * */
	  switch (spell)
	  {
	   case 0: /* Detect Monsters */
			(void)detect_monsters_normal();
		       break;
	   case 1: /* Phase Door */
			teleport_player(10);
		       break;
	   case 2: /* Detect Objects and Treasure*/
			(void)detect_objects_normal();
			(void)detect_treasure();
			(void)detect_objects_gold();
		       break;
	   case 3: /* Detect Doors and Traps */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break; 
       case 4: /* Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
            break;
	   case 5: /* Confuse Monster */
			if (!get_aim_dir(&dir)) return;
            (void)confuse_monster(dir, ( plev * 3) / 2 );
			break;
	   case 6: /* Teleport Self */
            teleport_player(plev * 5);
		       break;
	   case 7: /* Sleep Monster */
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir);
		       break;
	   case 8: /* Recharging */
               (void)recharge(50 + plev);
		       break;
	   case 9: /* Magic Mapping */
			map_area();
		       break;
	   case 10: /* Identify */
			(void)ident_spell();
		       break;
	   case 11: /* Detect Enchantment */
			(void)detect_objects_magic();
		       break;
	   case 12: /* Slow Monster */
			if (!get_aim_dir(&dir)) return;
			(void)slow_monster(dir);
		       break;
	   case 13: /* Mass Sleep */
			(void)sleep_monsters();
		       break;
	   case 14: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev);
		       break;
	   case 15: /* Haste Self */
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20 + (plev) ) + plev);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(plev));
			}
		       break;
       case 16: /* Charm Monster */
                 if (!get_aim_dir(&dir)) return;
                 (void) charm_monster(dir, plev);
               break;
       case 17: /* Dimension Door */
	{
		msg_print("You open a dimensional gate. Choose a destination.");
		if (!tgt_pt(&ii,&ij)) return;
		p_ptr->energy -= 60 - plev;
		if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
		    (distance(ij,ii,py,px) > plev + 2) ||
		    (!rand_int(plev * plev / 2)))
		{
			msg_print("You fail to exit the astral plane correctly!");
			p_ptr->energy -= 100;
			teleport_player(10);
		}
		else teleport_player_to(ij,ii);
		break;
	}
       case 18: /* Apport Arcane */
		if (!get_aim_dir(&dir)) return;
		fetch(dir, 200, FALSE);
		break;
       case 19: /* Self knowledge */
		(void)self_knowledge();
		(void)identify_pack();
		break;
	   case 20: /* Teleport Level */
		(void)teleport_player_level();
		break;
	   case 21: /* Word of Recall */
			(void) word_of_recall();
			break;
	   case 22: /* Detection */
		(void)detect_all();
		break;
	   case 23: /* Insight */
		identify_fully();
		break;
	case 24: /* Stasis */
		if (!get_aim_dir(&dir)) return;
		(void)stasis_monster(dir);
		break;
	case 25: /* Slow Monsters */
		(void)slow_monsters();
		break;
       case 26: /* Recharging True -- replaced by Explosive Rune */
               explosive_rune();
		break;
	   case 27: /* Clairvoyance */
		wiz_lite();
		if (!(p_ptr->telepathy))
		{
			(void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
		}
		break;
	   case 28: /* Enchant Weapon */
		(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
		break;
	   case 29: /* Enchant Armour */
		(void)enchant_spell(0, 0, rand_int(3) + 2);
		break;
	   case 30: /* Alchemy */
		(void)alchemy();
		break;
	   case 31: /* Globe of Resilience */
		(void)set_invuln(p_ptr->invuln + randint(5) + 5);
		break;
	   default:
		msg_format("You cast an unknown Sorcery spell: %d.", spell);
		msg_print(NULL);
	}
	break;

	case 2: /* * NATURE * */
	  switch (spell)
	  {
	case 0: /* Animal Detection */
		(void)detect_monsters_xxx(RF3_ANIMAL);
		break;
	case 1: /* Pesticide */
		msg_print("You spray a mild pesticide into the air...");
		dispel_animals(3 + ((plev - 1) / 7)); break;
	case 2: /* Detect Doors + Traps */
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		break; 
	case 3: /* Foraging */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 4: /* Daylight */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		if ((p_ptr->prace == RACE_VAMPIRE) && !(p_ptr->resist_lite))
		{
			msg_print("The daylight scorches your flesh!");
			take_hit(damroll(2,2), "daylight");
		}
		break;
	case 5: /* Animal Taming */
		if (!get_aim_dir(&dir)) return;
		(void) charm_animal(dir, plev);
		break;
	case 6: /* Resist Environment */
		(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
		(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
		(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
		break;
	case 7: /* Magic Poultice */
		(void)hp_player(damroll(6 , 8));
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 8: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return;
		(void)wall_to_mud(dir);
		break;
	case 9: /* Lightning Bolt */
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam-10, GF_ELEC, dir, damroll(5+((plev-5)/3), 8));
		break;
	case 10:
		/* Nature Awareness - *used* to detect monsters in general,
                 * but now detects only animals and elementals - Gumby
                 */
		map_area();
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		(void)detect_monsters_xxx(RF3_ANIMAL);
		if (plev > 19)
		{
			(void)detect_monsters_xxx(RF3_ELEMENTAL);
		}
		break;
	case 11: /* Frost Bolt */
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam-10, GF_COLD, dir, damroll(8+((plev-5)/3), 8));
		break;
	case 12: /* Ray of Sunlight */
		if (!get_aim_dir(&dir)) return;
		msg_print("A line of sunlight appears.");
		lite_line(dir);
		break;
	case 13: /* Elemental Brand */
		brand_weapon(0); break;
	case 14: /* Summon Animals */
		if (!(summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE)))
		no_trump = TRUE;
		break;
	case 15: /* Protection from Corrosion */
		rustproof(); break;
	case 16: /* Door Building */
		(void)door_creation(); break;
	case 17: /* Stair Building */
		(void)stair_creation(); break;
	case 18: /* Stone Skin */
		(void)set_shield(p_ptr->shield + randint(20) + 30); break;
	case 19: /* Resistance */
		(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
		(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
		(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
		(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
		(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
		break;
	case 20: /* Animal Friendship */
		(void)charm_animals(plev * 2); break;
	case 21: /* Stone Tell */
		identify_fully(); break;
	case 22: /* Wall of Stone */
		(void)wall_stone(); break;
	case 23: /* Herbal Healing */
		(void)hp_player(1000);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(0);
		(void)set_image(0);
		break;
	case 24: /* Venomous Thorns */
		if (get_aim_dir(&dir))
		{
			msg_print("Thorns spray from your fingers...");
			fire_blast(GF_POIS, dir, 5 + (plev/5), 4, 10, 3);
		}
		break;
	case 25: /* Earthquake */
		msg_print("The dungeon rumbles...");
		earthquake(py, px, 10);
		break;
	case 26: /* Whirlwind Attack */
		{
			int y = 0, x = 0;
			cave_type       *c_ptr;
			monster_type    *m_ptr;

			msg_print("You start to spin...");

			for (dir = 0; dir <= 9; dir++)
			{
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				/* Get the monster */
				m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- attack monsters */
				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
					py_attack(y, x);
			}
		}
		break;
	case 27: /* Blizzard */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_COLD, dir, 300 + (2 * plev), (plev/12)+1);
		break;
	case 28: /* Lightning Storm */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_ELEC, dir, 250 + (2 * plev), (plev/12)+1);
		break;
	case 29: /* Whirlpool */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_WATER, dir, 200 + (2 * plev), (plev/12)+1);
		break;
	case 30: /* Call Sunlight */
		fire_ball(GF_LITE, 0, 200, 8);
		wiz_lite();
		if ((p_ptr->prace == RACE_VAMPIRE) && !(p_ptr->resist_lite))
		{
			msg_print("The sunlight scorches your flesh!");
			take_hit(50, "sunlight");
		}
		break;
	case 31: /* Nature's Wrath */
		msg_print("You call upon Mother Nature to smite your enemies...");
		(void)dispel_monsters(plev * 6);
		earthquake(py, px, 20 + (plev / 2) );
		project(0, FALSE, 1+plev/12, py, px, 200 + plev, GF_DISINTEGRATE, PROJECT_KILL|PROJECT_ITEM, FALSE);
		break;
	default:
		msg_format("You cast an unknown Nature spell: %d.", spell);
		msg_print(NULL);
	}

	if (no_trump)
	msg_print("No animals arrive.");
	break;

	case 3: /* * CHAOS * */
	   switch (spell)
	   {
		case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
						  damroll(3 + ((plev - 1) / 3), 4));
                break;
        case 1: /* Trap/Door destruction */
			(void)destroy_doors_touch();
			break;
        case 2: /* Flash of Light */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
        case 3: /* Chaotic Sensing */
		(void)detect_random();
		break;
	case 4: /* Mana Burst */
		if (!get_aim_dir(&dir)) return;
		if (randint(100) <= 5)
		{
			fire_ball(GF_MANA, dir, 50 + (plev + (plev / 2)), ((plev < 30) ? 2 : 3));
		}
		else
		{
			fire_ball(GF_MISSILE, dir, 25 + (plev + (plev / 2)), ((plev < 30) ? 2 : 3));
		}
		break;
        case 5: /* Fire Bolt */
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam, GF_FIRE, dir, damroll(9+((plev-5)/3), 8));
		break;
	case 6: /* Fist of Force */
		if (!get_aim_dir(&dir)) return;
		fire_bolt(GF_FORCE, dir, damroll(9+((plev-5)/3), 8));
		break;
	case 7: /* Teleport Self */
		teleport_player(randint(150)+50);
		break;
        case 8: /* Chaos Branding */
		brand_weapon(1); break;
        case 9: /* Polymorph Other */
		if (!get_aim_dir(&dir)) return;
		(void)poly_monster(dir);
		break;
	case 10: /* Chaos Bolt */
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam, GF_CHAOS, dir,
					damroll(15 + ((plev - 5) / 3), 8));
		break;
        case 11: /* Sonic Boom */
		msg_print("BOOM! Shake the room!");
		project(0, FALSE, 2+plev/10, py, px, 65 + plev, GF_SOUND,
					PROJECT_KILL | PROJECT_ITEM, FALSE);
		break;
        case 12: /* Mana Bolt */
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam + 20, GF_MANA, dir, damroll(11 + ((plev - 5) / 3), 8));
		break;
	case 13: /* Fire Ball */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_FIRE, dir, 80 + (plev), 2);
		break;
	case 14: /* Word of Destruction */
		msg_print("The dungeon rumbles...");
		destroy_area(py, px, 15, TRUE);
		break;
	case 15: /* Invoke Chaos */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_CHAOS, dir, 100 + (plev), (plev / 5));
		break;
        case 16: /* Confusion Bolt (was Chain Lightning) */
#if 0
		for (dir = 0; dir <= 9; dir++)
		{
			fire_beam(GF_ELEC, dir, damroll(10+(plev/5), 8));
		}
		break;
#endif
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam, GF_CONFUSION, dir, damroll(10+((plev-5)/3), 8));
		break;
        case 17: /* Arcane Binding == Charging */
		(void)recharge(50 + plev);
		break;
        case 18: /* Disintegration */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_DISINTEGRATE, dir, 100 + (plev * 4), 3 + (plev/40));
		break;
	case 19: /* Demonic Consultation */
		if (randint(100) >= 50 + (p_ptr->lev - 5))
		{
			if (summon_specific(py, px, plev, SUMMON_DEMON))
			{
				msg_format("%^s says, 'You shall pay for annoying me!'", chaos_patrons[p_ptr->chaos_patron]);
			}
			else
			{
				msg_print("You were unable to contact Hell...");
				wild_magic(randint(20));
			}
		}
		else
		{
			if (randint(2)==1)	(void)identify_fully();
			else			(void)ident_spell();
		}
		break;
        case 20: /* Alter Reality */
		msg_print("The world changes!");
                if (autosave_l)
                {
                    is_autosave = TRUE;
		    msg_print("Autosaving the game...");
                    do_cmd_save_game();
                    is_autosave = FALSE;
                }
		new_level_flag = TRUE;
		break;
        case 21: /* Polymorph Self */
		do_poly_self();
		break;
	case 22: /* Mass Polymorph */
		poly_all();
		break;
        case 23: /* Summon monster, demon */
		if (randint(3) == 1)
		{
			if (summon_specific(py, px, (plev*3)/2, SUMMON_DEMON))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
        	}
		else
		{
			if (summon_specific_friendly(py, px, (plev*3)/2,
				SUMMON_DEMON, TRUE))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'What is thy bidding... Master?'");
			}
		}
		break;
        case 24: /* Beam of Gravity */
		if (!get_aim_dir(&dir)) return;
		fire_beam(GF_GRAVITY, dir, damroll(15+((plev-5)/3), 8));
		break;
        case 25: /* Meteor Swarm  */
		{
			int x, y, dx, dy, d, count = 0;
			int b = 10 + randint(10); 

			for (i = 0; i < b; i++)
			{
				do
				{
					count++;

					if (count > 1000)  break;

					x = px - 5 + randint(10);
					y = py - 5 + randint(10);
					dx = (px > x) ? (px - x) : (x - px);
					dy = (py > y) ? (py - y) : (y - py);

					/* Approximate distance */
					d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

				} while ((d > 5) || (!(player_has_los_bold(y, x))));

				if (count > 1000) break;
				count = 0;
				project(0, FALSE, 2, y, x, plev*4, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM, FALSE);
			}
		}
		break;
	case 26: /* Flame Strike */
		fire_ball(GF_FIRE, 0, 200 + (4 * plev), 8); break;
        case 27: /* Call Chaos */
		call_chaos(); break;
        case 28: /* Magic Rocket */
		if (!get_aim_dir(&dir)) return;
		msg_print("You launch a rocket!");
		fire_ball(GF_ROCKET, dir, 350 + (2 * plev), 2);
		break;
        case 29: /* Mana Storm */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_MANA, dir, 400 + (plev * 3), 4);
		break;
        case 30: /* Breathe Chaos */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_CHAOS, dir, p_ptr->chp, 3);
		break;
	case 31: /* Call the Void */
		call_the_(); break;
	default:
		msg_format("You cast an unknown Chaos spell: %d.", spell);
		msg_print(NULL);
	}
	break;
	
	case 4: /* * DEATH * */
	  switch (spell)
	  {
       case 0: /* Detect Undead & Demons -> Unlife*/
		(void) detect_monsters_nonliving();
		break;
       case 1: /* Malediction */
	if (!get_aim_dir(&dir)) return;

	/*
	 * A radius-0 ball may (1) be aimed at objects etc., and will
	 * affect them; (2) may be aimed at ANY visible monster, unlike a
	 * 'bolt' which must travel to the monster.
	 */
	fire_ball(GF_HELL_FIRE, dir, damroll(3 + ((plev - 1) / 3), 4), 0);

	if (randint(5)==1)	/* Special effect first */
	{
		dummy = randint(1000);

		if (dummy == 666)     fire_bolt(GF_DEATH_RAY, dir, plev);
		else if (dummy < 500) fire_bolt(GF_TURN_ALL, dir, plev);
		else if (dummy < 800) fire_bolt(GF_OLD_CONF, dir, plev);
		else                  fire_bolt(GF_STUN, dir, plev);
	}
	break;
       case 2: /* Detect Evil */
		(void)detect_monsters_evil();
		break; 
	   case 3: /* Stinking Cloud */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_POIS, dir, 15 + (plev / 2), 2);
		break;
	   case 4: /* Black Sleep */
		if (!get_aim_dir(&dir)) return;
		(void)sleep_monster(dir);
		break;
	case 5: /* Resist Poison */
		(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
		break;
	case 6: /* Horrify */
		if (!get_aim_dir(&dir)) return;
		(void)fear_monster(dir, plev);
		(void) stun_monster(dir, plev);
		break;
	case 7: /* Enslave the Undead */
		if (!get_aim_dir(&dir)) return;
		(void)control_one_undead(dir, plev);
		break;
	case 8: /* Infernal Orb */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_HELL_FIRE, dir, 25 + (plev + (plev / 2)), ((plev < 30) ? 2 : 3));
		break;
	case 9: /* Nether Bolt */
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam, GF_NETHER, dir, damroll(9+((plev-5)/3), 8));
		break;
       case 10: /* Poison Branding */
		brand_weapon(2); break;
       case 11: /* Terror */
		turn_monsters(30+plev); break;
	case 12: /* Vampiric Drain */
		if (!get_aim_dir(&dir)) return;
		dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
		if (drain_life(dir, dummy))
		{
			(void)hp_player(dummy);
			/* Gain nutritional sustenance: 150/hp drained */
			/* A Food ration gives 5000 food points (by contrast) */
			/* Don't ever get more than "Full" this way */
			/* But if we ARE Gorged,  it won't cure us */
			dummy = p_ptr->food + MIN(5000, 100 * dummy);
			if (p_ptr->food < PY_FOOD_MAX) /* Not gorged already */
			{
				(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
			}
		}
		break;
       case 13: /* Cloud Kill */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_POIS, dir, 200 + (plev * 2), (plev/10)+1);
		break;
	   case 14: /* Genocide */
		(void)genocide(TRUE);
		break;
	   case 15: /* Restore Life */
		(void)restore_level();
		break;
	   case 16: /* Berserk */
		(void)set_shero(p_ptr->shero + randint(25) + 25);
		(void)hp_player(30);
		(void)set_afraid(0);
		break;
       case 17: /* Punish Undead */
		(void)dispel_undead(plev * 5);
		(void)turn_undead();
		break;
	case 18: /* Dark Bolt */
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam, GF_DARK, dir, damroll(10+(plev/5), 8));
		break;
       case 19: /* Battle Frenzy */
		(void)set_shero(p_ptr->shero + randint(25) + 25);
		(void)hp_player(30);
		(void)set_afraid(0);

		if (!p_ptr->fast)
		{
			(void)set_fast(randint(20 + (plev / 2) ) + (plev / 2));
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint(5));
		}
		break;
	case 20: /* Vampirism True */
		if (!get_aim_dir(&dir)) return;
		for (dummy = 0; dummy < 3; dummy++)
		{
			if (drain_life(dir, 100))
			hp_player(100);
		}
		break;
	case 21: /* Vampiric Branding */
		brand_weapon(3);
		break;
	case 22: /* Darkness Storm */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_DARK, dir,	150 + (2 * plev), 4);
		break;
        case 23: /* Mass Genocide */
		(void)mass_genocide(TRUE);
		break;
       case 24: /* Death Ray */
		if (!get_aim_dir(&dir)) return;
		(void)death_ray(dir, plev * 2);
		break;
       case 25: /* Raise the Dead */
		if (randint(3) == 1)
		{
			if (summon_specific(py, px, (plev*3)/2, (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD)))
			{
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
				msg_print("'The dead arise... to punish you for disturbing them!'");
			}
		}
		else
		{
			if (summon_specific_friendly(py, px, (plev*3)/2, (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD), TRUE))
			{
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
				msg_print("Ancient, long-dead forms arise from the ground to serve you!");
			}
		}
		break;
       case 26: /* Esoteria */
		if (randint(50)>plev) (void) ident_spell();
		else identify_fully();
		break;
       case 27: /* Word of Death */
		(void)dispel_living(plev * 6); break;
       case 28: /* Evocation */
		(void)dispel_monsters(plev * 4);
		(void)turn_monsters(plev*4);
		break;
       case 29: /* Hellfire */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_HELL_FIRE, dir, 666, 3);
		take_hit(50+randint(50), "the strain of casting Hellfire");
		break;
        case 30: /* Summon Death */
		msg_print("The Grim Reaper appears and begins His harvest...");
		(void) deathray_monsters();

		/* Must punish the player severely for his presumption! */
		take_hit((p_ptr->chp / 2), "Death's cold touch");
		break;
        case 31: /* Wraithform */
		set_shadow(p_ptr->wraith_form + randint(plev) + (plev/2));
		break;
	default:
		msg_format("You cast an unknown Death spell: %d.", spell);
		msg_print(NULL);
	}
	break;

    case 5: /* TRUMP */
    switch (spell)
    {
        case 0: /* Phase Door */
		teleport_player(10);
		break;
        case 1: /* Mind Blast */
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_PSI, dir, damroll(3 + ((plev - 1) / 3), 4), 0);
		break;
        case 2: /* Shuffle */
           {
               int die = randint(120);

               if ((p_ptr->pclass == CLASS_ROGUE) ||
                   (p_ptr->pclass == CLASS_HIGH_MAGE))
                    die = (randint(110)) + plev / 5;
               /* Card sharks and high mages get a level bonus */

            msg_print("You shuffle the deck and draw a card...");

            if (die < 5)
            {
                msg_print("Oh no! It's Death!");
                for (dummy = 0; dummy < randint(3); dummy++)
                    (void)activate_hi_summon();
            }
            else if (die < 10)
            {
                msg_print("Oh no! It's the Devil!");
                (void) summon_specific(py, px, dun_level, SUMMON_DEMON);
            }
            else if (die < 18)
            {
		msg_print("It's the picture of a friendly monster.");
		if (!(summon_specific_friendly(py, px, plev, SUMMON_MINOR, FALSE)))
			no_trump = TRUE;
            }
            else if (die < 22)
            {
                msg_print("It's the swords of discord.");
                aggravate_monsters(1, FALSE);
            }
            else if (die < 26)
            {
                msg_print("It's the Fool.");
                (void) do_dec_stat(A_INT);
                (void) do_dec_stat(A_WIS);
            }
            else if (die < 30)
            {
                msg_print("It's the picture of a strange monster.");
                if (!(summon_specific(py, px, (dun_level * 3) / 2, 32 + randint(6))))
                    no_trump = TRUE;
            }
            else if (die < 33)
            {
                msg_print("It's the Moon.");
                unlite_area(10,3);
            }
            else if (die < 38)
            {
                msg_print("It's the Wheel of Fortune.");
                wild_magic((randint(32))-1);
            }
            else if (die < 40)
            {
                msg_print("It's a teleport trump card.");
                teleport_player(10);
            }
            else if (die <42)
            {
                msg_print("It's Justice.");
                set_blessed(p_ptr->blessed + p_ptr->lev);
            }
            else if (die <47)
            {
                msg_print("It's a teleport trump card.");
                teleport_player(100);
            }
            else if (die <52)
            {
                msg_print("It's a teleport trump card.");
                teleport_player(200);
            }
            else if (die <60)
            {
                msg_print("It's the Tower.");
                wall_breaker();
            }
            else if (die <72)
            {
                msg_print("It's Temperance.");
                sleep_monsters_touch();
            }
            else if (die <80)
            {
                msg_print("It's the Tower.");
                earthquake(py, px, 5);
            }
            else if (die<82)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, SUMMON_MOLD, TRUE)))
                    no_trump = TRUE;
            }
            else if (die<84)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, SUMMON_BAT, TRUE)))
                    no_trump = TRUE;
            }
            else if (die<86)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, SUMMON_VORTEX, FALSE)))
                    no_trump = TRUE;
            }
            else if (die<88)
            {
                msg_print("It's the picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, SUMMON_COIN, TRUE)))
                    no_trump = TRUE;
            }
            else if (die<96)
            {
                msg_print("It's the Lovers.");
                if (!get_aim_dir(&dir)) return;
                (void) charm_monster(dir, MIN(p_ptr->lev, 20));
            }
            else if (die<101)
            {
                msg_print("It's the Hermit.");
                wall_stone();
            }
            else if (die< 111)
            {
                msg_print("It's the Judgement.");
                do_cmd_rerate();
                if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
                {
                    msg_print("You are cured of all mutations.");
                    p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
                    p_ptr->update |= PU_BONUS;
                    handle_stuff();
                }
            }
            else if (die < 120)
            {
                msg_print("It's the Sun.");
                wiz_lite();
            }
            else
            {
                msg_print("It's the World.");
                if (p_ptr->exp < PY_MAX_EXP)
                {
                    s32b ee = (p_ptr->exp / 25) + 1;
                    if (ee > 5000) ee = 5000;
                    msg_print("You feel more experienced.");
                    gain_exp(ee);
                }
            }
           }
		break;
        case 3: /* Detect Minds */
		(void)detect_monsters_mental();
		break;
        case 4: /* Teleport Self */
		teleport_player(100 + (plev * 2));
		break;
        case 5: /* Dimension Door */
       {
             msg_print("You open a dimensional gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the astral plane correctly!");
                 p_ptr->energy -= 100;
                 teleport_player(10);
             }
             else teleport_player_to(ij,ii);
             break;
            }
        case 6: /* Minor Trump */
	{
		msg_print ("You concentrate on the lesser trumps...");
		if (randint(10) == 1)
		{
			if (summon_specific(py, px, plev, SUMMON_MINOR))
			{
				msg_print("The summoned creature is pissed!");
			}
			else
			{
				no_trump = TRUE;
			}
		}
		else
		{
			if (!(summon_specific_friendly(py, px, plev, SUMMON_MINOR, FALSE)))
				no_trump = TRUE;
		}
		break;
	}
        case 7: /* Teleport Away */
		if (!get_aim_dir(&dir)) return;
		(void)fire_beam(GF_AWAY_ALL, dir, plev);
		break;
	case 8: /* Trump Branding */
		brand_weapon(4); break;
        case 9: /* Teleport Level */
		(void)teleport_player_level(); break;
	case 10: /* Nexus Bolt */
		if (!get_aim_dir(&dir)) return;
		(void)fire_bolt_or_beam(beam + 20, GF_NEXUS, dir, damroll(9+((plev-5)/3), 8));
		break;
	case 11: /* Phantasmal Servant */
		if (summon_specific_friendly(py, px, (plev*3)/2, SUMMON_PHANTOM, FALSE))
		{
			msg_print ("'Your wish, master?'");
		}
		else
		{
			no_trump = TRUE;
		}
		break;
        case 12: /* Word of Recall */
		(void) word_of_recall();
		break;
        case 13: /* Trump Animal */
		{
			msg_print ("You concentrate on the trump of an animal...");
			if (randint(5) > 2)
			{
				if (!(summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, FALSE)))
				no_trump = TRUE;
			}
			else
			{
				if (summon_specific(py, px, plev, SUMMON_ANIMAL))
				{
					msg_print("The summoned animal gets angry!");
				}
				else
				{
					no_trump = TRUE;
				}
			}
		}
		break;
        case 14: /* Trump Monster */
        {
            msg_print ("You concentrate on the trump of a monster...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_NO_UNIQUES, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, 0))
                {
                    msg_print("The summoned creature gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 15: /* Conjure Elemental */
        {
            if (randint(6)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_ELEMENTAL, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_ELEMENTAL))
                {
                      msg_print("You fail to control the elemental creature!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 16: /* Apportation */
		if (!get_aim_dir(&dir)) return;
		fetch(dir, 200, FALSE);
		break;
        case 17: /* Trump Spiders */
        {
            msg_print ("You concentrate on the trump of a spider...");
            if (randint(5)>2)
            {
                if (!(summon_specific_friendly(py, px, plev, SUMMON_SPIDER, TRUE)))
                    no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_SPIDER))
                {
                    msg_print("The summoned spiders get angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 18: /* Trump Reptiles */
        {
            msg_print ("You concentrate on the trump of a reptile...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HYDRA, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HYDRA))
                {
                    msg_print("The summoned reptile gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 19: /* Trump Hounds */
        {
            msg_print ("You concentrate on the trump of a hound...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HOUND, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HOUND))
                {
                    msg_print("The summoned hounds get angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }

        break;
        case 20: /* Banish */
		banish_monsters(plev*4); break;
        case 21: /* Living Trump */
	        if (randint(2)==1) dummy = 12;
	        else dummy = 17;
	        if (gain_random_mutation(dummy))
	            msg_print("You have turned into a Living Trump.");
	        break;
        case 22: /* Death Dealing */
            (void)dispel_living(plev * 5);
        break;
        case 23: /* Trump Cyberdemon */
        {
            msg_print ("You concentrate on the trump of a Cyberdemon...");
            if (randint(10)>3)
            {
              if (!(summon_specific_friendly(py, px, plev, SUMMON_CYBER, FALSE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_CYBER))
                {
                    msg_print("The summoned Cyberdemon gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 24: /* Trump Divination */
			(void)detect_all();
        break;
        case 25: /* Trump Lore */
            identify_fully();
        break;
        case 26: /* Trump Undead */
        {
            msg_print ("You concentrate on the trump of an undead creature...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_UNDEAD, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_UNDEAD))
                {
                    msg_print("The summoned undead creature gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 27: /* Trump Dragon */
        {
            msg_print ("You concentrate on the trump of a dragon...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_DRAGON, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_DRAGON))
                {
                    msg_print("The summoned dragon gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }

        break;
        case 28: /* Mass Trump */
        {
            no_trump = TRUE;
            msg_print ("You concentrate on several trumps at once...");
            for (dummy = 0; dummy < 3 + (plev / 10); dummy++)
            {
                if (randint(10)>3)
                {
                 if (summon_specific_friendly(py, px, plev, SUMMON_NO_UNIQUES, TRUE))
                    no_trump = FALSE;
                }
                else
                {
                    if (summon_specific(py, px, plev, 0))
                    {
                        msg_print("A summoned creature gets angry!");
                        no_trump = FALSE;
                    }
                }
            }
        }
        break;
        case 29: /* Trump Demon */
        {
            msg_print ("You concentrate on the trump of a demon...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_DEMON, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_DEMON))
                {
                    msg_print("The summoned demon gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        case 30: /* Trump Ancient Dragon */
        {
            msg_print ("You concentrate on the trump of an ancient dragon...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HI_DRAGON))
                {
                    msg_print("The summoned ancient dragon gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }

        break;
        case 31: /* Trump Greater Undead */
        {
            msg_print ("You concentrate on the trump of a greater undead being...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE)))
                no_trump = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HI_UNDEAD))
                {
                    msg_print("The summoned greater undead creature gets angry!");
                }
                else
                {
                    no_trump = TRUE;
                }
            }
        }
        break;
        default:
        msg_format("You cast an unknown Trump spell: %d.", spell);
        msg_print(NULL);
    }
    if (no_trump)
    {
        msg_print("Nobody answers to your Trump call.");
    }
    break;

    /*
     * ARCANE - Re-written by Gumby, making it an amalgamation of spells
     *          appearing in the first two books of each of the other
     *          realms.
     */
    case 6:
	switch (spell)
	{
		/* Cantrips for Beginners */
		case 0: /* Static Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_ELEC, dir,
					damroll(3 + ((plev - 2) / 3), 4));
			break;
		case 1: /* Detect Monsters */
			(void)detect_monsters_normal(); break;
		case 2: /* Cure Light Wounds */
			(void)hp_player(damroll(3, 8));
			(void)set_cut(p_ptr->cut - 10);
			break;
		case 3: /* Phase Door */
			teleport_player(10); break;
		case 4: /* Detect Doors/Traps */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			break; 
		case 5: /* Phlogiston*/
			phlogiston(); break;
		case 6: /* Stinking Cloud */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir, 15 + (plev/2), 2);
			break;
		case 7: /* Confuse Monster */
			if (!get_aim_dir(&dir)) return;
			(void)confuse_monster(dir, (plev * 3) / 2);
			break;

		/* Minor Arcana */
		case 8: /* Resist Environment */
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
			break;
		case 9: /* Sleep Monster */
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir);
			break;
		case 10: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
			break;
		case 11: /* Lightning Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_ELEC, dir,
					damroll(5 + ((plev - 5) / 3), 8));
			break;
		case 12: /* Recharging */
			(void)recharge(50 + plev); break;
		case 13: /* Magic Mapping */
			map_area(); break;
		case 14: /* Cure Medium Wounds */
			(void)hp_player(damroll(6, 8));
			(void)set_cut((p_ptr->cut / 2) - 20);
			break;
		case 15: /* Identify */
			(void)ident_spell(); break;

		/* Major Arcana */
		case 16: /* Ray of Light */
			if (!get_aim_dir(&dir)) return;
			msg_print("A ray of bright light appears.");
			lite_line(dir);
			break;
		case 17: /* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1); break;
		case 18: /* Teleport Self */
			teleport_player(plev * 5); break;
		case 19: /* Remove Curse */
			if (remove_curse())
				msg_print("A blue glow surrounds you.");
			break;
		case 20: /* Cure Critical Wounds */
			(void)hp_player(damroll(12, 8));
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		case 21: /* Brand Weapon */
			brand_weapon(rand_int(5)); break;
		case 22: /* Teleport Other */
			if (!get_aim_dir(&dir)) return;
			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 23: /* Haste Self */
			if (!p_ptr->fast)
				(void)set_fast(randint(plev) + plev);
			else
				(void)set_fast(p_ptr->fast + randint(plev));
			break;

		/* Manual of Mastery */
 		case 24: /* Vampiric Drain */
			if (!get_aim_dir(&dir)) return;
			dummy = plev + randint(plev) * MAX(1, plev/10);
			if (drain_life(dir, dummy))
			{
				(void)hp_player(dummy);
				dummy = p_ptr->food + MIN(5000, 100 * dummy);
				if (p_ptr->food < PY_FOOD_MAX)
				{
					(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
				}
			}
			break;
		case 25: /* Ball Lightning */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ELEC, dir, 100 + (plev*2), 2);
			break;
		case 26: /* Healing */
			(void)hp_player(250);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_image(0);
			break;
		case 27: /* Summon Monster */
			if (!(summon_specific_friendly(py, px, plev, SUMMON_NO_UNIQUES, FALSE)))
				no_trump = TRUE;
			break;
		case 28: /* Poison Gas */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir, 200 + (plev*4), (plev/10)+1);
			break;
		case 29: /* Teleport Level */
			(void)teleport_player_level(); break;
		case 30: /* Word of Recall */
			(void) word_of_recall();
			break;
		case 31: /* Restoration */
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			(void)restore_level();
			break;

		default:
			msg_format("You cast an unknown Arcane spell: %d.", spell);
			msg_print(NULL);
	}
	break;

	default:
		  msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
		  msg_print(NULL);
	}
	
	/* A spell was cast */
	if (!((increment) ?
	      (spell_worked2 & (1L << spell)) :
	      (spell_worked1 & (1L << (spell)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
	    if (realm == p_ptr->realm1-1)
			{
				spell_worked1 |= (1L << spell);
			}
			else
			{
		spell_worked2 |= (1L << spell);
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}

	/* Take a turn */
	energy_use = 100;

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
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

