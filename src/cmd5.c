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


#if !defined(USE_NEW_MAGIC) || defined(SUPPORT_OLD_MAGIC)
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
static int get_spell_old(int *sn, cptr prompt, int sval, bool known, bool realm_2)
{
	int         i;
	int         spell;
	int         num = 0;
	int         ask;
	byte        spells[64];
	bool        flag, redraw, okay;
	char        choice;
	const magic_type  *s_ptr;
	char        out_val[160];
	int         use_realm = (realm_2 ? p_ptr->realm2 : p_ptr->realm1);
	cptr        p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known, use_realm - 1))
		{
			/* Success */
			return (TRUE);
		}
	}

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
		if (spell_okay(spells[i], known, use_realm - 1)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();


	/* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
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
				screen_save();

				/* Display a list of spells */
				print_spells_old(spells, num, 1, 20, use_realm - 1);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
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
		if (!spell_okay(spell, known, use_realm - 1))
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
			s_ptr = &mp_ptr->info[use_realm - 1][spell % 32];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, spell_names[use_realm - 1][spell % 32],
				s_ptr->smana, spell_chance(spell, use_realm - 1));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) screen_load();


	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

	repeat_push(*sn);

	/* Success */
	return (TRUE);
}
#endif /* !defined(USE_NEW_MAGIC) || defined(SUPPORT_OLD_MAGIC) */

#ifdef USE_NEW_MAGIC
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
static int get_spell_new(int *sn, cptr prompt, const object_type *o_ptr, bool known)
{
	int         i;
	int         spell;
	int         num = 0;
	int         ask;
	u16b        spells[MAX_SPELLS_CURRENT];
	bool        flag, redraw, okay;
	char        choice;
	char        out_val[160];
	cptr        p = "spell"; /* Used to choose between prayer and spell, repair this? */
	cptr        string = quarky_str(o_ptr->spell_list);

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay_new(*sn, known, FALSE))
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Check for blank spellbooks */
	if (!string) 
	{
		msg_print("That spellbook has no spells in it!");
		return FALSE;
	}

	/* Check how many spells we have */
	num = strlen(string) / 2;

	/* Sanity check */
	if (num < 0 || num > MAX_SPELLS_IN_BOOK)
	{
		msg_print("Error - Bad number of spells in spell book");
		return FALSE;
	}
	
	/* Extract spells */
	for (spell = 0; spell < num; ++spell)
	{
		spells[spell] = spell_number(string[spell * 2],string[(spell * 2) + 1]);
	}

	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay_new(spells[i], known, FALSE)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();


	/* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
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
				screen_save();

				/* Display a list of spells */
				print_spells_new(spells, num, 1, 20);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
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
		if (!spell_okay_new(spell, known, FALSE))
		{
			bell();
			msg_format("You may not %s that %s.", prompt, p);
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];


			/* Prompt */
			(void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, new_spell_name[spell],
				calculate_mana(spell), spell_chance_new(spell));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) screen_load();


	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

	repeat_push(*sn);

	/* Success */
	return (TRUE);
}


#endif /* USE_NEW_MAGIC */


		
#ifdef USE_NEW_MAGIC
/*
 * New version of do_cmd_browse_aux() for the new magic system.
 * 
 * Peruse the spells/prayers in a given book.  Note that we may
 * bypass do_cmd_browse by calling this function directly (as we
 * do from identify_fully_aux() which has the effect of allowing
 * any book to be browsed regardless of the player's realm choice.
 *
 * Note that *all* spells in the book are listed
 */
void do_cmd_browse_aux_new(const object_type *o_ptr)
{
	int spell;
	int num = 0;

	u16b spells[MAX_SPELLS_IN_BOOK];

	cptr string = quarky_str(o_ptr->spell_list);

	/* Check how many spells we have */
	num = string ? strlen(string) / 2 : 0;

	/* Is it empty? */
	if (!num)
	{
		msg_print("The spellbook is empty.");
		return;
	}
	
	/* Sanity check */
	if (num < 0 || num > MAX_SPELLS_IN_BOOK)
	{
		msg_print("Bad number of spells in spell book");
		return;
	}
	
	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < num; ++spell)
	{
		spells[spell] = spell_number(string[spell * 2],string[(spell * 2) + 1]);
	}

	/* Save the screen */
	screen_save();

	/* Display the spells */
	print_spells_new(spells, num, 1, 20);

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);

	/* Wait for key */
	(void)inkey();

	/* Restore the screen */
	screen_load();
}
#endif /* USE_NEW_MAGIC */



#if !defined(USE_NEW_MAGIC) || defined(SUPPORT_OLD_MAGIC)
/*
 * Peruse the spells/prayers in a given book.  Note that we may
 * bypass do_cmd_browse by calling this function directly (as we
 * do from identify_fully_aux() which has the effect of allowing
 * any book to be browsed regardless of the player's realm choice.
 *
 * Note that *all* spells in the book are listed
 */
void do_cmd_browse_aux_old(const object_type *o_ptr)
{
	int sval;
	int spell;
	int num = 0;

	byte spells[64];


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
	screen_save();

	/* Display the spells */
	print_spells_old(spells, num, 1, 20, (o_ptr->tval - TV_BOOKS_MIN));

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);

	/* Wait for key */
	(void)inkey();

	/* Restore the screen */
	screen_load();
}
#endif /* !defined(USE_NEW_MAGIC) || defined(SUPPORT_OLD_MAGIC) */


/*
 * Peruse the spells/prayers in a given book.
 * We need to know which magic system we're using.
 */
void do_cmd_browse_aux(const object_type *o_ptr)
{
#ifdef USE_NEW_MAGIC
 #ifdef SUPPORT_OLD_MAGIC
	if (old_magic_user) do_cmd_browse_aux_old(o_ptr);
	else do_cmd_browse_aux_new(o_ptr);
 #else /* SUPPORT_OLD_MAGIC */
	do_cmd_browse_aux_new(o_ptr);
 #endif /* SUPPORT_OLD_MAGIC */
#else /* USE_NEW_MAGIC */
	do_cmd_browse_aux_old(o_ptr);
#endif /* USE_NEW_MAGIC */
}
	
	
/*
 * Peruse the spells/prayers in a book
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int item;
	object_type	*o_ptr;
	cptr q, s;


	/* Warriors are illiterate */
	/*if (!(p_ptr->realm1 || p_ptr->realm2))*/
	/* With the new magic, looking at classes is easier */
	if (p_ptr->pclass == CLASS_WARRIOR || p_ptr->pclass == CLASS_MINDCRAFTER)
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
#ifdef USE_NEW_MAGIC
 #ifdef SUPPORT_OLD_MAGIC
	item_tester_tval = old_magic_user ? mp_ptr->spell_book : TV_SPELLBOOK;
 #else /* SUPPORT_OLD_MAGIC */
	item_tester_tval = TV_SPELLBOOK;
 #endif /* SUPPORT_OLD_MAGIC */
#else /* USE_NEW_MAGIC */
	item_tester_tval = mp_ptr->spell_book;
#endif /* USE_NEW_MAGIC */
	
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

	/* Print out the spells */
	do_cmd_browse_aux(o_ptr);
}


#if defined(SUPPORT_OLD_MAGIC) || !defined(USE_NEW_MAGIC)
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

	cptr q, s;

#ifdef USE_NEW_MAGIC
	/* New magic users need to be in their homes to study */
	if (!old_magic_user)
	{
		msg_print("You cannot study in this environment.");
		return;
	}
#endif /* USE_NEW_MAGIC */

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

	if (o_ptr->tval == REALM2_BOOK) increment = 32;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_book != TV_LIFE_BOOK)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell_old(&spell, "study", sval, FALSE,
		    (bool)(increment ? TRUE : FALSE)) && (spell == -1)) return;
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
					(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)))
					continue;

				/* Hack -- Prepare the randomizer */
				k++;

				/* Hack -- Apply the randomizer */
				if (one_in_(k)) gift = spell;
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

	if (increment) spell += increment;

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
		p, spell_names
		[(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)][spell % 32]);

	if (mp_ptr->spell_book == TV_LIFE_BOOK)
		chg_virtue(V_FAITH, 1);
	else
		chg_virtue(V_KNOWLEDGE, 1);

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

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}
#endif /* defined(SUPPORT_OLD_MAGIC) || !defined(USE_NEW_MAGIC) */

#ifdef USE_NEW_MAGIC
/*
 * Study a spell so it can be used - new magic version
 */
void do_cmd_study_new(void)
{
	int	i;

	int	spell = -1;

#ifdef SUPPORT_OLD_MAGIC
	/* Hack - Old magic users can't use this function */
	if (old_magic_user)
	{
		msg_print("That command does not work in stores.");
		return;
	}
#endif /* SUPPORT_OLD_MAGIC */
	
	if (p_ptr->pclass == CLASS_WARRIOR || p_ptr->pclass == CLASS_MINDCRAFTER)
	{
		msg_print("You cannot read books!");
		return;
	}

	if (!(p_ptr->new_spells))
	{
		msg_print("You cannot learn any new spells!");
		return;
	}

	msg_format("You can learn %d new spell%s.", p_ptr->new_spells,
		(p_ptr->new_spells == 1?"":"s"));
	msg_print(NULL);


	/* Is this necessary? */
	/* Hack -- Handle stuff */
	handle_stuff();

	/* Ask for a spell, allow cancel */
	if (!get_spell_from_grand_list(&spell, "study", FALSE, FALSE, NULL, 0)) return;

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msg_print("You do not have any more spells to study.");

		/* Abort */
		return;
	}

	
	/* Learn the spell */
	p_ptr->spell_learned[spell/32] |= (1L << (spell % 32));

	/* Find the next open entry in "spells[]" */
	for (i = 0; i < MAX_SPELLS_KNOWN_TOTAL; i++)
	{
		/* Stop at the first empty space */
		if (!p_ptr->spells[i]) break;
	}

	/* Add the spell to the known list */
	p_ptr->spells[i++] = spell;

	/* Mention the result */
	msg_format("You have learned the spell of %s.", new_spell_name[spell]);

	if (spell_stats[spell][6])
		chg_virtue(V_KNOWLEDGE, 1);
	else
		chg_virtue(V_FAITH, 1);

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more spell%s.",
			p_ptr->new_spells,
			(p_ptr->new_spells != 1) ? "s" : "");
	}

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}
#endif /* USE_NEW_MAGIC */


#define MAX_BIZARRE		6


static const int bizarre_num[MAX_BIZARRE] =
{
	SUMMON_BIZARRE1,
	SUMMON_BIZARRE2,
	SUMMON_BIZARRE3,
	SUMMON_BIZARRE4,
	SUMMON_BIZARRE5,
	SUMMON_BIZARRE6,
};


static void wild_magic(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	switch (randint0(spell) + randint0(9))
	{
		case 1:
		case 2:
		case 3:
		{
			teleport_player(10);
			break;
		}
		case 4:
		case 5:
		case 6:
		{
			teleport_player(100);
			break;
		}
		case 7:
		case 8:
		{
			teleport_player(200);
			break;
		}
		case 9:
		case 10:
		case 11:
		{
			(void)unlite_area(10, 3);
			break;
		}
		case 12:
		case 13:
		case 14:
		{
			(void)lite_area(damroll(2, 3), 2);
			break;
		}
		case 15:
		{
			(void)destroy_doors_touch();
			break;
		}
		case 16: case 17:
		{
			wall_breaker();
			break;
		}
		case 18:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case 19:
		case 20:
		{
			(void)trap_creation();
			break;
		}
		case 21:
		case 22:
		{
			(void)door_creation();
			break;
		}
		case 23:
		case 24:
		case 25:
		{
			aggravate_monsters(0);
			break;
		}
		case 26:
		{
			(void)earthquake(py, px, 5);
			break;
		}
		case 27:
		case 28:
		{
			(void)gain_mutation(0);
			break;
		}
		case 29:
		case 30:
		{
			(void)apply_disenchant();
			break;
		}
		case 31:
		{
			(void)lose_all_info();
			break;
		}
		case 32:
		{
			(void)fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));
			break;
		}
		case 33:
		{
			(void)wall_stone(-2);
			break;
		}
		case 34:
		case 35:
		{
			int i;
			int type = bizarre_num[randint0(6)];

			for (i = 0; i < 8; i++)
			{
				(void)summon_specific(0, py, px, (p_ptr->depth * 3) / 2, type, TRUE, 
						      FALSE, FALSE, GP_COPY, 0);
			}
			break;
		}
		case 36:
		case 37:
		{
			(void)activate_hi_summon();
			break;
		}
		case 38:
		{
			(void)summon_cyber(-1, py, px);
			break;
		}
		default:
		{
			int count = 0;

			(void)activate_ty_curse(FALSE, &count);

			break;
		}
	}

	return;
}


static bool cast_life_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int	dir;
	int	plev = p_ptr->lev;

	switch (spell)
	{
	case 0: /* Detect Evil */
		(void)detect_monsters_evil();
		break;
	case 1: /* Cure Light Wounds */
		(void)hp_player(damroll(2, 10));
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 2: /* Bless */
		(void)set_blessed(p_ptr->blessed + rand_range(12, 24));
		break;
	case 3: /* Remove Fear */
		(void)set_afraid(0);
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
		(void)hp_player(damroll(4, 10));
		(void)set_cut((p_ptr->cut / 2) - 20);
		break;
	case 7: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 8: /* Remove Curse */
		(void)remove_curse();
		break;
	case 9: /* Cure Poison */
		(void)set_poisoned(0);
		break;
	case 10: /* Cure Critical Wounds */
		(void)hp_player(damroll(8, 10));
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 11: /* Sense Unseen */
		(void)set_tim_invis(p_ptr->tim_invis + rand_range(24, 48));
		break;
	case 12: /* Orb or Draining */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_HOLY_FIRE, dir,
		          (damroll(3, 6) + plev +
		          (plev / ((p_ptr->pclass == CLASS_PRIEST ||
		             p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4))),
		          ((plev < 30) ? 2 : 3));

		break;
	case 13: /* Protection from Evil */
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * p_ptr->lev);
		break;
	case 14: /* Healing */
		(void)hp_player(300);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 15: /* Glyph of Warding */
		(void)warding_glyph();
		break;
	case 16: /* Exorcism */
		(void)dispel_undead(plev);
		(void)dispel_demons(plev);
		(void)turn_evil(plev);
		break;
	case 17: /* Dispel Curse */
		(void)remove_all_curse();
		break;
	case 18: /* Dispel Undead + Demons */
		(void)dispel_undead(plev * 3);
		(void)dispel_demons(plev * 3);
		break;
	case 19: /* 'Day of the Dove' */
		(void)charm_monsters(plev * 2);
		break;
	case 20: /* Dispel Evil */
		(void)dispel_evil(plev * 4);
		break;
	case 21: /* Banishment */
		if (banish_evil(100))
		{
			msg_print("The power of your god banishes evil!");
		}
		break;
	case 22: /* Holy Word */
		(void)dispel_evil(plev * 4);
		(void)hp_player(1000);
		(void)set_afraid(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 23: /* Warding True */
		(void)warding_glyph();
		(void)glyph_creation();
		break;
	case 24: /* Heroism */
		(void)set_hero(p_ptr->hero + rand_range(25, 50));
		(void)hp_player(10);
		(void)set_afraid(0);
		break;
	case 25: /* Prayer */
		(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
		break;
	case 26:
		return bless_weapon();
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
		(void)set_afraid(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 29: /* Holy Vision */
		return identify_fully();
	case 30: /* Divine Intervention */
		(void)project(0, 1, py, px, 777, GF_HOLY_FIRE, PROJECT_KILL);
		(void)dispel_monsters(plev * 4);
		(void)slow_monsters();
		(void)stun_monsters(plev * 4);
		(void)confuse_monsters(plev * 4);
		(void)turn_monsters(plev * 4);
		(void)stasis_monsters(plev * 4);
		(void)summon_specific(-1, py, px, plev, SUMMON_ANGEL, TRUE, TRUE, TRUE, GP_ALLY, 0);
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(300);

		/* Haste */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}

		(void)set_afraid(0);
		break;
	case 31: /* Holy Invulnerability */
		(void)set_invuln(p_ptr->invuln + rand_range(7, 14));
		break;
	default:
		msg_format("You cast an unknown Life spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}



static bool cast_sorcery_spell(int spell)
{
	int	dir;
	int	plev = p_ptr->lev;

	switch (spell)
	{
	case 0: /* Detect Monsters */
		(void)detect_monsters_normal();
		break;
	case 1: /* Phase Door */
		teleport_player(10);
		break;
	case 2: /* Detect Doors and Traps */
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		break;
	case 3: /* Light Area */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 4: /* Confuse Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)confuse_monster(dir, (plev * 3) / 2);
		break;
	case 5: /* Teleport Self */
		teleport_player(plev * 5);
		break;
	case 6: /* Sleep Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)sleep_monster(dir);
		break;
	case 7: /* Recharging */
		return recharge(plev * 4);
	case 8: /* Magic Mapping */
		map_area();
		break;
	case 9: /* Identify */
		return ident_spell();
	case 10: /* Slow Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)slow_monster(dir);
		break;
	case 11: /* Mass Sleep */
		(void)sleep_monsters();
		break;
	case 12: /* Teleport Away */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_AWAY_ALL, dir, plev);
		break;
	case 13: /* Haste Self */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 14: /* Detection True */
		(void)detect_all();
		break;
	case 15: /* Identify True */
		return identify_fully();
	case 16: /* Detect Objects and Treasure */
		(void)detect_objects_normal();
		(void)detect_treasure();
		(void)detect_objects_gold();
		break;
	case 17: /* Detect Enchantment */
		(void)detect_objects_magic();
		break;
	case 18: /* Charm Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)charm_monster(dir, plev);
		break;
	case 19: /* Dimension Door */
		msg_print("You open a dimensional gate. Choose a destination.");
		return dimension_door();
	case 20: /* Sense Minds */
		(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		break;
	case 21: /* Self knowledge */
		(void)self_knowledge();
		break;
	case 22: /* Teleport Level */
		(void)teleport_player_level();
		break;
	case 23: /* Word of Recall */
		word_of_recall();
		break;
	case 24: /* Stasis */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)stasis_monster(dir);
		break;
	case 25: /* Telekinesis */
		if (!get_aim_dir(&dir)) return FALSE;

		fetch(dir, plev * 15, FALSE);
		break;
	case 26: /* Explosive Rune */
		(void)explosive_rune();
		break;
	case 27: /* Clairvoyance */
		wiz_lite();
		if (!(p_ptr->telepathy))
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		}
		break;
	case 28: /* Enchant Weapon */
		return enchant_spell(randint1(4), randint1(4), 0);
	case 29: /* Enchant Armour */
		return enchant_spell(0, 0, rand_range(2, 5));
	case 30: /* Alchemy */
		return alchemy();
	case 31: /* Globe of Invulnerability */
		(void)set_invuln(p_ptr->invuln + rand_range(8, 16));
		break;
	default:
		msg_format("You cast an unknown Sorcery spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_nature_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int	    dir;
	int	    beam;
	int	    plev = p_ptr->lev;
	bool    no_trump = FALSE;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
	case 0: /* Detect Creatures */
		(void)detect_monsters_normal();
		break;
	case 1: /* First Aid */
		(void)hp_player(damroll(2, 8));
		(void)set_cut(p_ptr->cut - 15);
		break;
	case 2: /* Detect Doors + Traps */
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		break;
	case 3: /* Produce Food */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 4: /* Daylight */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		if ((p_ptr->prace == RACE_VAMPIRE) && !p_ptr->resist_lite)
		{
			msg_print("The daylight scorches your flesh!");
			take_hit(damroll(2, 2), "daylight");
		}
		break;
	case 5: /* Animal Taming */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)charm_animal(dir, plev);
		break;
	case 6: /* Resist Environment */
		{
			int dur = rand_range(20, 40);
			(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			(void)set_oppose_fire(p_ptr->oppose_fire + dur);
			(void)set_oppose_elec(p_ptr->oppose_elec + dur);
		}
		break;
	case 7: /* Cure Wounds + Poison */
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 8: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wall_to_mud(dir);
		break;
	case 9: /* Lightning Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
			damroll(3 + ((plev - 5) / 4), 8));
		break;
	case 10: /* Nature Awareness -- downgraded */
		map_area();
		/*(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		(void)detect_monsters_normal();*/
		(void)detect_all();
		break;
	case 11: /* Frost Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
			damroll(5 + ((plev - 5) / 4), 8));
		break;
	case 12: /* Ray of Sunlight */
		if (!get_aim_dir(&dir)) return FALSE;
		msg_print("A line of sunlight appears.");
		(void)lite_line(dir);
		break;
	case 13: /* Entangle */
		(void)slow_monsters();
		break;
	case 14: /* Summon Animals */
		if (!(summon_specific(-1, py, px, plev, SUMMON_ANIMAL_RANGER, TRUE, 
						TRUE, TRUE, GP_ALLY, 0)))
			no_trump = TRUE;
		break;
	case 15: /* Herbal Healing */
		(void)hp_player(1000);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 16: /* Door Building */
		(void)door_creation();
		break;
	case 17: /* Stair Building */
		(void)stair_creation();
		break;
	case 18: /* Stone Skin */
		(void)set_shield(p_ptr->shield + rand_range(30, 50));
		break;
	case 19: /* Resistance True */
		{
			int dur = rand_range(20, 40);
			(void)set_oppose_acid(p_ptr->oppose_acid + dur);
			(void)set_oppose_elec(p_ptr->oppose_elec + dur);
			(void)set_oppose_fire(p_ptr->oppose_fire + dur);
			(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			(void)set_oppose_pois(p_ptr->oppose_pois + dur);
		}
		break;
	case 20: /* Animal Friendship */
		(void)charm_animals(plev * 2);
		break;
	case 21: /* Stone Tell */
		return identify_fully();
	case 22: /* Wall of Stone */
		(void)wall_stone(-2);
		break;
	case 23: /* Nature's Boon */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		(void)remove_all_curse();
		(void)set_afraid(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 24: /* Earthquake */
		(void)earthquake(py, px, 10);
		break;
	case 25: /* Whirlwind Attack */
		{
			int y = 0, x = 0;
			cave_type *c_ptr;
			monster_type *m_ptr;

			for (dir = 0; dir <= 9; dir++)
			{
				y = py + ddy[dir];
				x = px + ddx[dir];

				/* paranoia */
				if (!in_bounds2(y, x)) continue;
				c_ptr = area(y, x);

				/* Get the monster */
				m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- attack monsters */
				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
					py_attack(y, x);
			}
		}
		break;
	case 26: /* Blizzard */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_COLD, dir, 70 + plev, (plev / 12) + 1);
		break;
	case 27: /* Nature's Doom */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_MISSILE, dir, 175 + (2 * plev), 0);
		break;
	case 28: /* Whirlpool */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_WATER, dir, 100 + plev, (plev / 12) + 1);
		break;
	case 29: /* Call Sunlight */
		(void)fire_ball(GF_LITE, 0, 150, 8);
		wiz_lite();
		if ((p_ptr->prace == RACE_VAMPIRE) && !p_ptr->resist_lite)
		{
			msg_print("The sunlight scorches your flesh!");
			take_hit(50, "sunlight");
		}
		break;
	case 30: /* Elemental Brand */
		brand_weapon(0);
		break;
	case 31: /* Nature's Wrath */
		(void)dispel_monsters(plev * 4);
		(void)earthquake(py, px, 20 + (plev / 2));
		(void)project(0, 1 + plev / 12, py, px,
			100 + plev, GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM);
		(void)set_hero(p_ptr->hero + rand_range(25, 50));
		(void)hp_player(10);
		(void)set_afraid(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	default:
		msg_format("You cast an unknown Nature spell: %d.", spell);
		msg_print(NULL);
	}

	/*if (no_trump)
		msg_print("No animals arrive.");*/

	return TRUE;
}


static bool cast_chaos_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int	dir, i, beam;
	int	plev = p_ptr->lev;
	cave_type *c_ptr;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
	case 0: /* Magic Missile */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
			damroll(3 + ((plev - 1) / 5), 4));
		break;
	case 1: /* Trap / Door destruction */
		(void)destroy_doors_touch();
		break;
	case 2: /* Flash of Light == Light Area */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 3: /* Touch of Confusion */
		if (!p_ptr->confusing)
		{
			msg_print("Your hands start glowing.");
			p_ptr->confusing = TRUE;
			p_ptr->redraw |= (PR_STATUS);
		}
		break;
	case 4: /* Manaburst */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_MISSILE, dir,
		    (damroll(3, 5) + plev +
		    (plev / (((p_ptr->pclass == CLASS_MAGE) ||
		    (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))),
		    ((plev < 30) ? 2 : 3));
			/* Shouldn't actually use GF_MANA, as it will destroy all
			 * items on the floor */
		break;
	case 5: /* Fire Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
			damroll(8 + ((plev - 5) / 4), 8));
		break;
	case 6: /* Fist of Force ("Fist of Fun") */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_DISINTEGRATE, dir,
			damroll(8 + ((plev - 5) / 4), 8), 0);
		break;
	case 7: /* Teleport Self */
		teleport_player(plev * 5);
		break;
	case 8: /* Wonder */
		{
		/*
		 * This spell should become more useful (more
		 * controlled) as the player gains experience levels.
		 * Thus, add 1/5 of the player's level to the die roll.
		 * This eliminates the worst effects later on, while
		 * keeping the results quite random.  It also allows
		 * some potent effects only at high level.
		 */
			int die = randint1(100) + plev / 5;

			if (die < 26)
				chg_virtue(V_CHANCE, 1);

			if (!get_aim_dir(&dir)) return FALSE;
			if (die > 100)
				msg_print("You feel a surge of power!");
			if (die < 8) (void)clone_monster(dir);
			else if (die < 14) (void)speed_monster(dir);
			else if (die < 26) (void)heal_monster(dir);
			else if (die < 31) (void)poly_monster(dir);
			else if (die < 36)
				(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
			else if (die < 41) (void)confuse_monster(dir, plev);
			else if (die < 46) (void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
			else if (die < 51) (void)lite_line(dir);
			else if (die < 56)
				(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
				                  damroll(3 + ((plev - 5) / 4), 8));
			else if (die < 61)
				(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
				                  damroll(5 + ((plev - 5) / 4), 8));
			else if (die < 66)
				(void)fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6 + ((plev - 5) / 4), 8));
			else if (die < 71)
				(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8 + ((plev - 5) / 4), 8));
			else if (die < 76) (void)drain_life(dir, 75);
			else if (die < 81) (void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
			else if (die < 86) (void)fire_ball(GF_ACID, dir, 40 + plev, 2);
			else if (die < 91) (void)fire_ball(GF_ICE, dir, 70 + plev, 3);
			else if (die < 96) (void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
			else if (die < 101) (void)drain_life(dir, 100 + plev);
			else if (die < 104)
			{
				(void)earthquake(py, px, 12);
			}
			else if (die < 106)
			{
				(void)destroy_area(py, px, 15);
			}
			else if (die < 108)
			{
				(void)genocide(TRUE);
			}
			else if (die < 110) (void)dispel_monsters(120);
			else /* RARE */
			{
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}
			break;
		}
	case 9: /* Chaos Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_CHAOS, dir,
		                  damroll(10 + ((plev - 5) / 4), 8));
		break;
	case 10: /* Sonic Boom */
		msg_print("BOOM! Shake the room!");
		(void)project(0, plev / 10 + 2, py, px,
			45 + plev, GF_SOUND, PROJECT_KILL | PROJECT_ITEM);
		break;
	case 11: /* Doom Bolt -- always beam in 2.0.7 or later */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_MANA, dir, damroll(11 + ((plev - 5) / 4), 8));
		break;
	case 12: /* Fire Ball */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_FIRE, dir, plev + 55, 2);
		break;
	case 13: /* Teleport Other */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_AWAY_ALL, dir, plev);
		break;
	case 14: /* Word of Destruction */
		(void)destroy_area(py, px, 15);
		break;
	case 15: /* Invoke Logrus */
		/* Power of Chaos */
		/*if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_CHAOS, dir, plev + 66, plev / 5);
		break;*/
		if (one_in_(2)) (void)dispel_monsters(150);
		if (one_in_(2)) (void)slow_monsters();
		if (one_in_(2)) (void)sleep_monsters();
		if (randint1(4) > 1) (void)explosive_rune();
		if (randint1(4) > 1) (void)hp_player(150);
		if (randint1(4) > 1)
		{
			(void)set_shero(p_ptr->shero + rand_range(25, 50));
			(void)set_afraid(0);
		}
		if (randint1(4) > 1)
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(rand_range(plev / 2, 20 + plev));
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint1(5));
			}
		}
		break;
	case 16: /* Polymorph Other */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)poly_monster(dir);
		break;
	case 17: /* Chain Lightning */
		for (dir = 0; dir <= 9; dir++)
			(void)fire_beam(GF_ELEC, dir, damroll(5 + (plev / 10), 8));
		break;
	case 18: /* Arcane Binding == Charging */
		return recharge(90);
	case 19: /* Disintegration */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_DISINTEGRATE, dir, plev + 80, 3 + plev / 40);
		break;
	case 20: /* Alter Reality */
		alter_reality();
		break;
	case 21: /* Polymorph Self */
		do_poly_self();
		break;
	case 22: /* Chaos Branding */
		brand_weapon(1);
		break;
	case 23: /* Summon monster, demon */
		{
			bool pet = TRUE; /*(one_in_(3));*/
			bool group = !(pet && (plev < 50));

			if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_DEMON, group, 
						FALSE, pet, GP_COPY, 0))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");

				if (pet)
					msg_print("'What is thy bidding... Master?'");
				else
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
			break;
		}
	case 24: /* Beam of Gravity */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_GRAVITY, dir, damroll(9 + ((plev - 5) / 4), 8));
		break;
	case 25: /* Mana Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_MISSILE, dir,
			75 + randint1(350 + (3*plev)));
		break;
	case 26: /* Meteor Swarm */
		{
			int x, y;
			int b = rand_range(10, 20);
			for (i = 0; i < b; i++)
			{
				int count = 0;

				while (count < 1000)
				{
					count++;
					
					x = px - 5 + randint1(10);
					y = py - 5 + randint1(10);

					/* paranoia */
					if (!in_bounds(y, x)) continue;

					c_ptr = area(y, x);

					/* keep going if not in LOS */
					if (!player_has_los_grid(c_ptr)) continue;

					/* if close enough - exit */
					if (distance(py, px, y, x) < 6) break;
				}

				if (count >= 1000) break;

				(void)project(0, 2, y, x, (plev * 3) / 2, GF_METEOR, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
			}
		}
		break;
	case 27: /* Flame Strike */
		(void)fire_ball(GF_FIRE, 0, 150 + (2 * plev), 8);
		break;
	case 28: /* Call Chaos */
		call_chaos();
		break;
	case 29: /* Mana Storm */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_MANA, dir, 300 + (plev * 2), 4);
		break;
	case 30: /* Breathe Logrus */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_CHAOS, dir, p_ptr->chp, 2);
		break;
	case 31: /* Call the Void */
		call_the_();
		break;
	default:
		msg_format("You cast an unknown Chaos spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_death_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;
	int	i;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
	case 0: /* Detect Undead & Demons -> Unlife */
		(void)detect_monsters_nonliving();
		break;
	case 1: /* Malediction */
		if (!get_aim_dir(&dir)) return FALSE;
		/* A radius-0 ball may (1) be aimed at objects etc.,
		 * and will affect them; (2) may be aimed at ANY
		 * visible monster, unlike a 'bolt' which must travel
		 * to the monster. */

		(void)fire_ball(GF_HELL_FIRE, dir,
			damroll(3 + ((plev - 1) / 5), 3), 0);

		if (one_in_(5))
		{
			/* Special effect first */
			dummy = randint1(1000);
			if (dummy == 666)
				(void)fire_bolt(GF_DEATH_RAY, dir, plev * 50);
			else if (dummy < 500)
				(void)fire_bolt(GF_TURN_ALL, dir, plev);
			else if (dummy < 800)
				(void)fire_bolt(GF_OLD_CONF, dir, plev);
			else
				(void)fire_bolt(GF_STUN, dir, plev);
		}
		break;
	case 2: /* Detect Evil */
		(void)detect_monsters_evil();
		break;
	case 3: /* Stinking Cloud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
		break;
	case 4: /* Black Sleep */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)sleep_monster(dir);
		break;
	case 5: /* Resist Poison */
		(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
		break;
	case 6: /* Horrify */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fear_monster(dir, plev);
		(void)stun_monster(dir, plev);
		break;
	case 7: /* Enslave the Undead */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)control_one_undead(dir, plev);
		break;
	case 8: /* Orb of Entropy */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_OLD_DRAIN, dir,
			(damroll(3, 6) + plev +
			(plev / (((p_ptr->pclass == CLASS_MAGE) ||
			(p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))),
			((plev < 30) ? 2 : 3));
		break;
	case 9: /* Nether Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_NETHER, dir,
		                  damroll(6 + ((plev - 5) / 4), 8));
		break;
	case 10: /* Terror */
		(void)turn_monsters(30 + plev);
		break;
	case 11: /* Vampiric Drain */
		if (!get_aim_dir(&dir)) return FALSE;

		dummy = plev + randint1(plev) * MAX(1, plev / 10);   /* Dmg */
		if (drain_gain_life(dir, dummy))
		{
			/*
			 * Hack - this only happens when monster is seen to
			 * be hit.
			 */
			chg_virtue(V_SACRIFICE, -1);
			chg_virtue(V_VITALITY, -1);

			/* Gain nutritional sustenance: 150/hp drained */
			/* A Food ration gives 5000 food points (by contrast) */
			/* Don't ever get more than "Full" this way */
			/* But if we ARE Gorged, it won't cure us */
			dummy = p_ptr->food + MIN(5000, 100 * dummy);
			if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
				(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
		}
		break;
	case 12: /* Poison Branding */
		brand_weapon(2);
		break;
	case 13: /* Dispel Evil */
		(void)dispel_evil(plev * 3);
		break;
	case 14: /* Genocide */
		(void)genocide(TRUE);
		break;
	case 15: /* Restore Life */
		(void)restore_level();
		break;
	case 16: /* Berserk */
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(30);
		(void)set_afraid(0);
		break;
	case 17: /* Invoke Spirits */
		{
			int die = randint1(100) + plev / 5;
			if (!get_aim_dir(&dir)) return FALSE;

			msg_print("You call on the power of the dead...");

			if (die < 26)
				chg_virtue(V_CHANCE, 1);

			if (die > 100)
				msg_print("You feel a surge of eldritch force!");

			if (die < 8)
			{
				msg_print("Oh no! Mouldering forms rise from the earth around you!");
				(void)summon_specific(0, py, px, p_ptr->depth, SUMMON_UNDEAD, TRUE, 
						      FALSE, FALSE, GP_FIXATED_ON_PLAYER, 0);

				chg_virtue(V_UNLIFE, 1);
			}
			else if (die < 14)
			{
				msg_print("An unnamable evil brushes against your mind...");
				(void)set_afraid(p_ptr->afraid + rand_range(4, 8));
			}
			else if (die < 26)
			{
				msg_print("Your head is invaded by a horde of gibbering spectral voices...");
				(void)set_confused(p_ptr->confused + rand_range(4, 8));
			}
			else if (die < 31)
			{
				(void)poly_monster(dir);
			}
			else if (die < 36)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
			}
			else if (die < 41)
			{
				(void)confuse_monster (dir, plev);
			}
			else if (die < 46)
			{
				(void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
			}
			else if (die < 51)
			{
				(void)lite_line(dir);
			}
			else if (die < 56)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
					damroll(3 + ((plev - 5) / 4), 8));
			}
			else if (die < 61)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
					damroll(5 + ((plev - 5) / 4), 8));
			}
			else if (die < 66)
			{
				(void)fire_bolt_or_beam(beam, GF_ACID, dir,
					damroll(6 + ((plev - 5) / 4), 8));
			}
			else if (die < 71)
			{
				(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
					damroll(8 + ((plev - 5) / 4), 8));
			}
			else if (die < 76)
			{
				(void)drain_life(dir, 75);
			}
			else if (die < 81)
			{
				(void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
			}
			else if (die < 86)
			{
				(void)fire_ball(GF_ACID, dir, 40 + plev, 2);
			}
			else if (die < 91)
			{
				(void)fire_ball(GF_ICE, dir, 70 + plev, 3);
			}
			else if (die < 96)
			{
				(void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
			}
			else if (die < 101)
			{
				(void)drain_life(dir, 100 + plev);
			}
			else if (die < 104)
			{
				(void)earthquake(py, px, 12);
			}
			else if (die < 106)
			{
				(void)destroy_area(py, px, 15);
			}
			else if (die < 108)
			{
				(void)genocide(TRUE);
			}
			else if (die < 110)
			{
				(void)dispel_monsters(120);
			}
			else
			{ /* RARE */
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}

			if (die < 31)
				msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
			break;
		}
	case 18: /* Dark Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_DARK, dir,
			damroll(4 + ((plev - 5) / 4), 8));
		break;
	case 19: /* Battle Frenzy */
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(30);
		(void)set_afraid(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(rand_range(plev / 2, 20 + plev));
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 20: /* Vampirism True */
		if (!get_aim_dir(&dir)) return FALSE;

		chg_virtue(V_SACRIFICE, -1);
		chg_virtue(V_VITALITY, -1);

		for (dummy = 0; dummy < 3; dummy++)
		{
			(void)drain_gain_life(dir, 100);
		}
		break;
	case 21: /* Vampiric Branding */
		brand_weapon(3);
		break;
	case 22: /* Darkness Storm */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_DARK, dir, 120, 4);
		break;
	case 23: /* Mass Genocide */
		(void)mass_genocide(TRUE);
		break;
	case 24: /* Death Ray */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)death_ray(dir, plev);
		break;
	case 25: /* Raise the Dead */
		{
			if (raise_dead(py, px, FALSE, (bool)(!one_in_(3)), GP_COPY, 0))
			{
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
				chg_virtue(V_UNLIFE, 1);
			}
			else
			{
				msg_print("Nothing happens.");
			}	
			break;
		}
	case 26: /* Esoteria */
		if (randint1(50) > plev)
			return ident_spell();
		else
			return identify_fully();
		break;
	case 27: /* Word of Death */
		(void)dispel_living(plev * 3);
		break;
	case 28: /* Evocation */
		(void)dispel_monsters(plev * 4);
		(void)turn_monsters(plev * 4);
		(void)banish_monsters(plev * 4);
		break;
	case 29: /* Hellfire */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_HELL_FIRE, dir, 666, 3);
		take_hit(rand_range(50, 100), "the strain of casting Hellfire");
		break;
	case 30: /* Omnicide */
		p_ptr->csp -= 100;
		
		/* Display doesn't show mana cost (100)
		 * as deleted until the spell has finished. This gives a
		 * false impression of how high your mana is climbing.
		 * Therefore, 'deduct' the cost temporarily before entering the
		 * loop, then add it back at the end so that the rest of the
		 * program can deduct it properly
		 */
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* Hack -- Skip Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

			/* Hack -- Skip Quest Monsters */
			if (r_ptr->flags1 & RF1_QUESTOR) continue;

			/* Notice changes in view */
			if (r_ptr->flags7 & (RF7_LITE_1 | RF7_LITE_2))
			{
				/* Update some things */
				p_ptr->update |= (PU_MON_LITE);
			}
			
			/* Delete the monster */
			delete_monster_idx(i);

			/* Take damage */
			take_hit(randint1(4), "the strain of casting Omnicide");

			/* Absorb power of dead soul - up to twice max. mana */
			if (p_ptr->csp < (p_ptr->msp * 2))
				p_ptr->csp++;

			/* Visual feedback */
			move_cursor_relative(py, px);

			/* Redraw */
			p_ptr->redraw |= (PR_HP | PR_MANA);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
			p_ptr->window |= (PW_SPELL);

			/* Handle */
			handle_stuff();

			/* Fresh */
			Term_fresh();

			/* Delay */
			Term_xtra(TERM_XTRA_DELAY,
				delay_factor * delay_factor * delay_factor);
		}

		/* Restore, ready to be deducted properly */
		p_ptr->csp += 100;

		break;
	case 31: /* Wraithform */
		(void)set_wraith_form(p_ptr->wraith_form + rand_range(plev / 2, plev));
		break;
	default:
		msg_format("You cast an unknown Death spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_trump_spell(int spell, bool success)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int     dir;
	int     beam;
	int     plev = p_ptr->lev;
	/*int     dummy = 0;
	bool	no_trump = FALSE;
	char	ppp[80];
	char	tmp_val[160];*/


	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;
int die;
	switch (spell)
	{
		case 0: /* Spit Poison */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_POIS, dir,
			damroll(3 + ((plev - 1) / 5), 3));
			(void)set_poisoned(p_ptr->poisoned > 10 ? p_ptr->poisoned - 10 : 0);
			break;
		case 1: /* Remove Fear */
			(void)set_afraid(0);
			break;
		case 2: /* Sixth Sense */
			(void)detect_traps();
			(void)detect_monsters_normal();
			if (!(p_ptr->telepathy) && (plev>35))
			{
				(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
			}
			break;
		case 3: /* Heroism */
			(void)set_hero(p_ptr->hero + rand_range(25, 50));
			(void)hp_player(10);
			(void)set_afraid(0);
			(void)set_poisoned(p_ptr->poisoned > 8 ? p_ptr->poisoned - 8 : 0);
			break;
		case 4: /* Resist Fire */
			(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(30, 60));
			break;
		case 5: /* Resist Acid */
			(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(30, 60));
			break;
		case 6: /* Create Nutrients */
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		case 7: /* Spit Acid */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_ACID, dir,
			damroll(5 + ((plev - 5) / 4), 8));
			break;
		case 8: /* Relieve Wounds */
			(void)hp_player(damroll(6, 10));
			(void)set_cut(p_ptr->cut - 30);
			break;
		case 9: /* Breathe Frost */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_COLD, dir, 15 + ((3 * plev) / 4), 2);
			break;
		case 10: /* Hands of Appraisal */
			if (plev < 25)
				return psychometry();
			else 
				return ident_spell();
			break;
		case 11: /* See Invisible */
			(void)set_tim_invis(p_ptr->tim_invis + rand_range(36, 72));
			break;
		case 12: /* Adrenalin */
			(void)set_shero(p_ptr->shero + rand_range(50, 75));
			(void)hp_player(30);
			(void)set_afraid(0);
			(void)set_poisoned(p_ptr->poisoned / 2);
			break;
		case 13: /* Cure */
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		case 14: /* Breathe Fire */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_FIRE, dir, 40 + ((3 * plev) / 4), 2);
			break;
		case 15: /* Breathe Plasma */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_PLASMA, dir, 150 + (3 * plev), 2);
			break;
		case 16: /* Resist Cold */
			(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(30, 60));
			break;
		case 17: /* Resist Electricity */
			(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(30, 60));
			break;
		case 18: /* Resist Poison */
			(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(30, 60));
			break;
		case 19: /* Ironskin */
			(void)set_shield(p_ptr->shield + rand_range(60, 100));
			break;
		case 20: /* Essence of Speed */
			if (!p_ptr->fast)
			{
				(void)set_fast(randint1(100) + (3 * plev));
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint1(plev));
			}
			break;
		case 21: /* Release Poison */
			(void)fire_ball(GF_POIS, 0, 75 + plev, 2 + (plev/8));
			break;
		case 22: /* Breathe Shards */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_SHARDS, dir, 60 + plev, 2);
			break;
		case 23: /* Electrocution */
			(void)project(0, 1, py, px, (125 + (2 * plev)), GF_ELEC, PROJECT_KILL);
			break;
		case 24: /* Resistance */
			{
				int dur = rand_range(30, 60);
				(void)set_oppose_acid(p_ptr->oppose_acid + dur);
				(void)set_oppose_elec(p_ptr->oppose_elec + dur);
				(void)set_oppose_fire(p_ptr->oppose_fire + dur);
				(void)set_oppose_cold(p_ptr->oppose_cold + dur);
				(void)set_oppose_pois(p_ptr->oppose_pois + dur);
			}
			break;
		case 25: /* Breathe Lightning */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_ELEC, dir, damroll(9 + ((plev - 5) / 4), 8));
			break;
		case 26: /* Breathe Power */
			/*int die;*/
			die = randint1(100);
			if (!get_aim_dir(&dir)) return FALSE;

			if (one_in_(2))
			{
			do
			{
			die = randint1(100);
			if (die < 5)
			{
				msg_print("You breathe light.");
				(void)fire_ball(GF_LITE, dir, 10 + (plev / 5), 2);
			}
			else if (die < 10)
			{
				msg_print("You breathe darkness.");
				(void)fire_ball(GF_DARK, dir, 15 + (plev / 5), 2);
			}
			else if (die < 14)
			{
				msg_print("You project a missile.");
				(void)fire_bolt_or_beam(beam, GF_MISSILE, dir,
			damroll(3 + ((plev - 1) / 5), 4));
			}
			else if (die < 19)
			{
				msg_print("You breathe nexus.");
				(void)fire_ball(GF_NEXUS, dir, 10 + (plev / 2), 2);
			}
			else if (die < 23)
			{
				msg_print("You breathe time.");
				(void)fire_ball(GF_TIME, dir, 15 + (plev / 2), 2);
			}
			else if (die < 28)
			{
				msg_print("You breathe inertia.");
				(void)fire_ball(GF_INERTIA, dir, 20 + (plev / 2), 2);
			}
			else if (die < 32)
			{
				msg_print("You breathe frost.");
				(void)fire_ball(GF_COLD, dir, 15 + ((3 * plev) / 4), 2);
			}
			else if (die < 37)
			{
				msg_print("You breathe acid.");
				(void)fire_beam(GF_ACID, dir, damroll(4 + ((plev - 5) / 4), 8));
			}
			else if (die < 41)
			{
				msg_print("You breathe fire.");
				(void)fire_ball(GF_FIRE, dir, 40 + ((3 * plev) / 4), 2);
			}
			else if (die < 45)
			{
				msg_print("You breathe lightning.");
				(void)fire_beam(GF_ELEC, dir, damroll(9 + ((plev - 5) / 4), 8));
			}
			else if (die < 49)
			{
				msg_print("You breathe confusion.");
				(void)fire_ball(GF_CONFUSION, dir, 50 + plev, 2);
			}
			else if (die < 53)
			{
				msg_print("You breathe disenchantment.");
				(void)fire_ball(GF_DISENCHANT, dir, 55 + plev, 2);
			}
			else if (die < 57)
			{
				msg_print("You breathe shards.");
				(void)fire_ball(GF_SHARDS, dir, 60 + plev, 2);
			}
			else if (die < 62)
			{
				msg_print("You breathe gravity.");
				(void)fire_ball(GF_GRAVITY, dir, 60 + (2 * plev), 2);
			}
			else if (die < 66)
			{
				msg_print("You breathe force.");
				(void)fire_ball(GF_FORCE, dir, 85 + (2 * plev), 2);
			}
			else if (die < 71)
			{
				msg_print("You breathe poison.");
				(void)fire_ball(GF_POIS, dir, 100 + (6 * plev), 2);
			}
			else if (die < 75)
			{
				msg_print("You breathe plasma.");
				(void)fire_ball(GF_PLASMA, dir, 150 + (6 * plev), 2);
			}
			else if (die < 79)
			{
				msg_print("You breathe toxic waste.");
				(void)fire_ball(GF_NUKE, dir, 200 + (4 * plev), 2);
			}
			else if (die < 83)
			{
				msg_print("You breathe sound.");
				(void)fire_ball(GF_SOUND, dir, 250 + (5 * plev), 2);
			}
			else if (die == 83)
			{
				msg_print("Everything comes out!!!");
				(void)fire_ball(GF_LITE, dir, 2, 2);
				(void)fire_ball(GF_DARK, dir, 2, 2);
				(void)fire_bolt_or_beam(beam, GF_MISSILE, dir, 3);
				(void)fire_ball(GF_NEXUS, dir, 3, 2);
				(void)fire_ball(GF_TIME, dir, 4, 2);
				(void)fire_ball(GF_INERTIA, dir, 4, 2);
				(void)fire_ball(GF_COLD, dir, 5, 2);
				(void)fire_beam(GF_ACID, dir, 7);
				(void)fire_ball(GF_FIRE, dir, 8, 2);
				(void)fire_beam(GF_ELEC, dir, 9);
				(void)fire_ball(GF_CONFUSION, dir, 5 + (plev / 10), 2);
				(void)fire_ball(GF_DISENCHANT, dir, 6 + (plev / 10), 2);
				(void)fire_ball(GF_SHARDS, dir, 6 + (plev / 10), 2);
				(void)fire_ball(GF_GRAVITY, dir, 60 + (plev / 5), 2);
				(void)fire_ball(GF_FORCE, dir, 9 + (plev / 5), 2);
				(void)fire_ball(GF_POIS, dir, 10 + (plev / 5), 2);
				(void)fire_ball(GF_PLASMA, dir, 20 + (plev / 5), 2);
				(void)fire_ball(GF_NUKE, dir, 15 + (plev / 2), 2);
				(void)fire_ball(GF_SOUND, dir, 25 + (plev / 2), 2);
				(void)fire_ball(GF_NETHER, dir, 35 + (plev / 2), 2);
				(void)fire_ball(GF_CHAOS, dir, 40 + (plev / 2), 2);
				(void)fire_ball(GF_DISINTEGRATE, dir, 45 + (plev / 2), 2);
				(void)fire_ball(GF_MANA, dir, 25 + plev, 2);
			}
			else if (die < 88)
			{
				msg_print("You breathe nether.");
				(void)fire_ball(GF_NETHER, dir, 300 + (12 * plev), 2);
			}
			else if (die < 92)
			{
				msg_print("You breathe chaos.");
				(void)fire_ball(GF_CHAOS, dir, 300 + (7 * plev), 2);
			}
			else if (die < 97)
			{
				msg_print("You breathe disintegration.");
				(void)fire_ball(GF_DISINTEGRATE, dir, 350 + (14 * plev), 2);
			}
			else
			{
				msg_print("You breathe mana.");
				(void)fire_ball(GF_MANA, dir, 350 + (15 * plev), 2);
			}
			}
			while (one_in_(2));
			}
			else msg_print("Nothing seems to come.");

			break;
		
		case 27: /* Remove Wounds */
			(void)hp_player(300 + randint1(400));
			(void)set_cut(0);
			break;
		case 28: /* Polymorph Self */
			do_poly_self();
			break;
		case 29: /* Polymorph Life */
			do_cmd_rerate();
			if (one_in_(2))
			{
				if (!lose_mutation(0))
					msg_print("You feel oddly normal.");
			}
			if (one_in_(12))
			{
				msg_print("You have a sudden feeling of doom...");
				(void)do_dec_stat(A_STR);
				(void)do_dec_stat(A_DEX);
				(void)do_dec_stat(A_CON);
				(void)do_dec_stat(A_INT);
				(void)do_dec_stat(A_WIS);
				(void)do_dec_stat(A_CHR);
				int count = 0;
				(void)activate_ty_curse(FALSE, &count);
				(void)lose_all_info();
			}
			break;
		case 30: /* Mutate */
			if (one_in_(12))
			{
				msg_print("You have a sudden feeling of doom...");
				(void)do_dec_stat(A_STR);
				(void)do_dec_stat(A_DEX);
				(void)do_dec_stat(A_CON);
				(void)do_dec_stat(A_INT);
				(void)do_dec_stat(A_WIS);
				(void)do_dec_stat(A_CHR);
				int count = 0;
				(void)activate_ty_curse(FALSE, &count);
				(void)lose_all_info();
			}
			(void)gain_mutation(0);
			break;
		case 31: /* Breathe Mana */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_MANA, dir, 300 + (5 * plev), 2);
			break;
		default:
			msg_format("You cast an unknown Morphic spell: %d.", spell);
			msg_print(NULL);
	}

	/*if (no_trump)
	{
		msg_print("Nobody answers to your Trump call.");
	}*/

	return TRUE;
}


static bool cast_arcane_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
	case 0: /* Zap */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
			damroll(3 + ((plev - 1) / 5), 3));
		break;
	case 1: /* Wizard Lock */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wizard_lock(dir);
		break;
	case 2: /* Detect Invisibility */
		(void)detect_monsters_invis();
		break;
	case 3: /* Detect Monsters */
		(void)detect_monsters_normal();
		break;
	case 4: /* Blink */
		teleport_player(10);
		break;
	case 5: /* Light Area */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 6: /* Trap & Door Destruction */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)destroy_door(dir);
		break;
	case 7: /* Cure Light Wounds */
		(void)hp_player(damroll(2, 8));
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 8: /* Detect Doors & Traps */
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		break;
	case 9: /* Phlogiston */
		phlogiston();
		break;
	case 10: /* Detect Treasure */
		(void)detect_treasure();
		(void)detect_objects_gold();
		break;
	case 11: /* Detect Enchantment */
		(void)detect_objects_magic();
		break;
	case 12: /* Detect Object */
		(void)detect_objects_normal();
		break;
	case 13: /* Cure Poison */
		(void)set_poisoned(0);
		break;
	case 14: /* Resist Cold */
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		break;
	case 15: /* Resist Fire */
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		break;
	case 16: /* Resist Lightning */
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		break;
	case 17: /* Resist Acid */
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		break;
	case 18: /* Cure Medium Wounds */
		(void)hp_player(damroll(4, 8));
		(void)set_cut((p_ptr->cut / 2) - 50);
		break;
	case 19: /* Teleport */
		teleport_player(plev * 5);
		break;
	case 20: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wall_to_mud(dir);
		break;
	case 21: /* Ray of Light */
		if (!get_aim_dir(&dir)) return FALSE;

		msg_print("A line of light appears.");
		(void)lite_line(dir);
		break;
	case 22: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 23: /* See Invisible */
		(void)set_tim_invis(p_ptr->tim_invis + rand_range(24, 48));
		break;
	case 24: /* Recharging */
		return recharge(plev * 4);
	case 25: /* Teleport Level */
		(void)teleport_player_level();
		break;
	case 26: /* Identify */
		return ident_spell();
	case 27: /* Teleport Away */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_AWAY_ALL, dir, plev);
		break;
	case 28: /* Elemental Ball */
		if (!get_aim_dir(&dir)) return FALSE;

		switch (randint1(4))
		{
			case 1:  dummy = GF_FIRE; break;
			case 2:  dummy = GF_ELEC; break;
			case 3:  dummy = GF_COLD; break;
			default: dummy = GF_ACID; break;
		}
		(void)fire_ball(dummy, dir, 75 + (plev), 2);
		break;
	case 29: /* Detection */
		(void)detect_all();
		break;
	case 30: /* Word of Recall */
		word_of_recall();
		break;
	case 31: /* Clairvoyance */
		wiz_lite();
		if (!p_ptr->telepathy)
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		}
		break;
	default:
		msg_format("You cast an unknown Arcane spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_wizardry_spell(int spell)
{

	int px = p_ptr->px;
	int py = p_ptr->py;

	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	i;
	cave_type *c_ptr;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;



	switch (spell)
	{
	case 0: /* Magic Missile */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
			damroll(3 + ((plev - 1) / 5), 4));
		break;
	case 1: /* Trap / Door destruction */
		(void)destroy_doors_touch();
		break;
	case 2: /* Detect Evil */
		(void)detect_monsters_evil();
		break;
	case 3: /* Cure Light Wounds */
		(void)hp_player(damroll(2, 10));
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 4: /* Stinking Cloud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
		break;
	case 5: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wall_to_mud(dir);
		break;
	case 6: /* Lightning Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
			damroll(3 + ((plev - 5) / 4), 8));
		break;
	case 7: /* Resist Poison */
		(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
		break;
	case 8: /* Spear of Light */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)lite_line(dir);
		break;
	case 9: /* Manaburst */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_MISSILE, dir,
		    (damroll(3, 6) + plev + (plev / 4)), ((plev < 45) ? 1 : 2));
			/* Shouldn't actually use GF_MANA, as it will destroy all
			 * items on the floor */
		break;
	case 10: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 11: /* Fire Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
			damroll(8 + ((plev - 5) / 4), 8));
		break;
	case 12: /* Cure Poison */
		(void)set_poisoned(0);
		break;
	case 13: /* Fire Ball */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_FIRE, dir, plev + 55, 2);
		break;
	case 14: /* Word of Destruction */
		(void)destroy_area(py, px, 15);
		break;
	case 15: /* Genocide */
		(void)genocide(TRUE);
		break;
	case 16: /* Resist Environment */
		{
			int dur = rand_range(20, 40);
			(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			(void)set_oppose_fire(p_ptr->oppose_fire + dur);
			(void)set_oppose_elec(p_ptr->oppose_elec + dur);
		}
		break;
	case 17: /* Door Building */
		(void)door_creation();
		break;
	case 18: /* Heroism */
		(void)set_hero(p_ptr->hero + rand_range(25, 50));
		(void)hp_player(10);
		(void)set_afraid(0);
		break;
	case 19: /* Stair Building */
		(void)stair_creation();
		break;
	case 20: /* Berserker */
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(30);
		(void)set_afraid(0);
		break;
	case 21: /* Shield */
		(void)set_shield(p_ptr->shield + rand_range(30, 50));
		break;
	case 22: /* Resistance */
		{
			int dur = rand_range(20, 40);
			(void)set_oppose_acid(p_ptr->oppose_acid + dur);
			(void)set_oppose_elec(p_ptr->oppose_elec + dur);
			(void)set_oppose_fire(p_ptr->oppose_fire + dur);
			(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			(void)set_oppose_pois(p_ptr->oppose_pois + dur);
		}
		break;
	case 23: /* Mass Genocide */
		(void)mass_genocide(TRUE);
		break;
	case 24: /* Frost Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
			damroll(5 + ((plev - 5) / 4), 8));
		break;
	case 25: /* Polymorph Other */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)poly_monster(dir);
		break;
	case 26: /* Earthquake */
		(void)earthquake(py, px, 10);
		break;
	case 27: /* Doom Bolt -- always beam in 2.0.7 or later */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_MANA, dir, damroll(11 + ((plev - 5) / 4), 8));
		break;
	case 28: /* Blizzard */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_COLD, dir, 70 + plev, (plev / 12) + 1);
		break;
	case 29: /* Whirlpool */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_WATER, dir, 100 + plev, (plev / 12) + 1);
		break;
	case 30: /* Meteor Swarm */
		{
			int x, y;
			int b = rand_range(10, 20);
			for (i = 0; i < b; i++)
			{
				int count = 0;

				while (count < 1000)
				{
					count++;

					x = px - 5 + randint1(10);
					y = py - 5 + randint1(10);

					/* paranoia */
					if (!in_bounds(y, x)) continue;

					c_ptr = area(y, x);

					/* keep going if not in LOS */
					if (!player_has_los_grid(c_ptr)) continue;

					/* if close enough - exit */
					if (distance(py, px, y, x) < 6) break;
				}

				if (count >= 1000) break;

				(void)project(0, 2, y, x, (plev * 3) / 2, GF_METEOR, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
			}
		}
		break;
	case 31: /* Mana Storm */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_MANA, dir, 300 + (plev * 2), 4);
		break;
	default:
		msg_format("You cast an unknown Wizardry spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}






#if defined(SUPPORT_OLD_MAGIC) || !defined(USE_NEW_MAGIC)
/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
#ifdef USE_SCRIPT
	use_skill_callback();
#else /* USE_SCRIPT */
	int	item, sval, spell, realm;
	int	chance;
	int	increment = 0;
	int	use_realm;
	bool cast;

	const cptr prayer = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	object_type	*o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (!p_ptr->realm1)
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

#ifdef USE_SCRIPT
	if (object_cast_callback(o_ptr)) return;
#endif /* USE_SCRIPT */

	/* Access the item's sval */
	sval = o_ptr->sval;

	if (o_ptr->tval == REALM2_BOOK) increment = 32;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (increment) realm = p_ptr->realm2;
	else realm = p_ptr->realm1;

	/* Ask for a spell */
	if (!get_spell_old(&spell, ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
	               sval, TRUE, (bool)(increment ? TRUE : FALSE)))
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */
	use_realm = (increment ? p_ptr->realm2 : p_ptr->realm1);

	s_ptr = &mp_ptr->info[use_realm - 1][spell];


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
	chance = spell_chance(spell, use_realm - 1);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

		msg_format("You failed to get the %s off!", prayer);
		sound(SOUND_FAIL);

		if ((randint1(100) < chance) && (mp_ptr->spell_book == TV_LIFE_BOOK))
			chg_virtue(V_FAITH, -1);
		else if (randint1(100) < chance)
			chg_virtue(V_KNOWLEDGE, -1);


		/*if (realm == REALM_TRUMP)
		{
			(void)cast_trump_spell(spell, FALSE);
		}*/
		else if ((o_ptr->tval == TV_CHAOS_BOOK) && (randint1(100) < spell))
		{
			msg_print("You produce a chaotic effect!");
			wild_magic(spell);
		}
		else if ((o_ptr->tval == TV_DEATH_BOOK) && (randint1(100) < spell))
		{
			if ((sval == 3) && one_in_(2))
			{
				msg_print("Your sanity is shaken by reading the Necronomicon!");
				
				/* Mind blast */
				if (!saving_throw(p_ptr->skill_sav * 100))
				{
					if (!p_ptr->resist_confu)
					{
						(void)set_confused(p_ptr->confused + rand_range(4, 8));
					}
					if (!p_ptr->resist_chaos && one_in_(3))
					{
						(void)set_image(p_ptr->image + rand_range(150, 400));
					}
				}

				/* Lose int & wis */
				else if (!saving_throw(p_ptr->skill_sav * 100))
				{
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
				}
			}
			else
			{
				msg_print("It hurts!");
				take_hit(damroll(o_ptr->sval + 1, 6), "a miscast Death spell");
				if ((spell > 15) && one_in_(6) && !p_ptr->hold_life)
					lose_exp(spell * 250);
			}
		}
	}

	/* Process spell */
	else
	{
		if ((randint1(100) < chance) && (chance < 50))
		{
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				chg_virtue(V_FAITH, 1);
			else
				chg_virtue(V_KNOWLEDGE, 1);
		}

		/* Spells. */
		switch (realm)
		{
		case REALM_LIFE: /* * LIFE * */
			cast = cast_life_spell(spell);
			break;
		case REALM_SORCERY: /* * SORCERY * */
			cast = cast_sorcery_spell(spell);
			break;
		case REALM_NATURE: /* * NATURE * */
			cast = cast_nature_spell(spell);
			break;
		case REALM_CHAOS: /* * CHAOS * */
			cast = cast_chaos_spell(spell);
			break;
		case REALM_DEATH: /* * DEATH * */
			cast = cast_death_spell(spell);
			break;
		case REALM_TRUMP: /* TRUMP */
			cast = cast_trump_spell(spell, TRUE);
			break;
		case REALM_ARCANE: /* ARCANE */
			cast = cast_arcane_spell(spell);
			break;
		case REALM_WIZARDRY: /* WIZARDRY */
			cast = cast_wizardry_spell(spell);
			break;
		default:
			cast = FALSE;
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;

		/* A spell was cast */
		if (!(increment ?
		    (p_ptr->spell_worked2 & (1L << spell)) :
		    (p_ptr->spell_worked1 & (1L << spell))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (realm == p_ptr->realm1)
			{
				p_ptr->spell_worked1 |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2 |= (1L << spell);
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				chg_virtue(V_FAITH, 1);
			else
				chg_virtue(V_KNOWLEDGE, 1);
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
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1));

		if (mp_ptr->spell_book == TV_LIFE_BOOK)
			chg_virtue(V_FAITH, -10);
		else
			chg_virtue(V_KNOWLEDGE, -10);

		/* Damage CON (possibly permanently) */
		if (one_in_(2))
		{
			bool perm = one_in_(4);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, rand_range(15, 25), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
#endif /* USE_SCRIPT */
}
#endif /* defined(SUPPORT_OLD_MAGIC) || !defined(USE_NEW_MAGIC) */


#ifdef USE_NEW_MAGIC
/*
 * Cast a spell - new system
 */
void do_cmd_cast_new(void)
{
#ifdef USE_SCRIPT
	use_skill_callback();
#else /* USE_SCRIPT */
	int	item, spell;
	int	chance;
	bool 	cast;
	int 	smana;
			
	object_type	*o_ptr;

	cptr q, s;

	/* Require spell ability */
	if (p_ptr->pclass == CLASS_WARRIOR || p_ptr->pclass == CLASS_MINDCRAFTER)
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
	item_tester_tval = TV_SPELLBOOK;

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

#ifdef USE_SCRIPT
	if (object_cast_callback(o_ptr)) return;
#endif /* USE_SCRIPT */

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Ask for a spell */
	if (!get_spell_new(&spell, "cast", o_ptr, TRUE))
	{
		if (spell == -2)
			msg_print("You don't know any spells in that book.");
		return;
	}


	/* Access the spell */
	smana = calculate_mana(spell);
	chance = spell_chance_new(spell);


	/* Verify "dangerous" spells */
	if (smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

		msg_print("You failed to get the spell off!");
		sound(SOUND_FAIL);
		
		if (randint1(100) < chance)
			chg_virtue(spell_stats[spell][6] ? V_KNOWLEDGE : V_FAITH, -1);
		
/* May do something like this, but would have to have a different implementation */
#if 0
		else if ((o_ptr->tval == TV_CHAOS_BOOK) && (randint1(100) < spell))
		{
			msg_print("You produce a chaotic effect!");
			wild_magic(spell);
		}
		else if ((o_ptr->tval == TV_DEATH_BOOK) && (randint1(100) < spell))
		{
			if ((sval == 3) && one_in_(2))
			{
				msg_print("Your sanity is shaken by reading the Necronomicon!");
				
				/* Mind blast */
				if (!saving_throw(p_ptr->skill_sav * 100))
				{
					if (!p_ptr->resist_confu)
					{
						(void)set_confused(p_ptr->confused + rand_range(4, 8));
					}
					if (!p_ptr->resist_chaos && one_in_(3))
					{
						(void)set_image(p_ptr->image + rand_range(150, 400));
					}
				}

				/* Lose int & wis */
				else if (!saving_throw(p_ptr->skill_sav * 100))
				{
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
				}
			}
			else
			{
				msg_print("It hurts!");
				take_hit(damroll(o_ptr->sval + 1, 6), "a miscast Death spell");
				if ((spell > 15) && one_in_(6) && !p_ptr->hold_life)
					lose_exp(spell * 250);
			}
		}
#endif /* 0 */
	}

	/* Process spell */
	else
	{
		if ((randint1(100) < chance) && (chance < 50))
		{
			chg_virtue(spell_stats[spell][6] ? V_KNOWLEDGE : V_FAITH, 1);
		}

		/* Spells. */
		cast = cast_spell_new(spell, TRUE);

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;

		/* A spell was cast */
		if (!(p_ptr->spell_worked[spell/32] & (1L << (spell % 32))))
		{
			/* The spell worked */
			p_ptr->spell_worked[spell/32] |= (1L << (spell % 32));

			/* Gain experience */
			gain_exp(spell_stats[spell][2] * spell_stats[spell][4]);

			chg_virtue(spell_stats[spell][6] ? V_KNOWLEDGE : V_FAITH, 1);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (smana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = smana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1));

		chg_virtue(spell_stats[spell][6] ? V_KNOWLEDGE : V_FAITH, -10);

		/* Damage CON (possibly permanently) */
		if (one_in_(2))
		{
			bool perm = one_in_(4);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, rand_range(15, 25), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
#endif /* USE_SCRIPT */
}
#endif /* USE_NEW_MAGIC */


/*
 * Pray a prayer -- Unused in Zangband
 */
void do_cmd_pray(void)
{
	msg_format("Praying is not used in %s. Use magic spell casting instead.",
	           VERSION_NAME);
}


/*
 * Issue a pet command
 */
void do_cmd_pet(void)
{
	int i = 0;
	int powers[36];
	cptr power_desc[36];
	bool flag, redraw;
	int ask;
	char choice;
	char out_val[160];
	int pets = 0;
	int pet_ctr;
	bool all_pets = FALSE;
	monster_type *m_ptr;
	int mode = 0;
	byte y = 1, x = 0;
	int ctr = 0;
	char buf[160];
	int num = 0;


	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr)) pets++;
	}

	if (pets)
	{
		power_desc[num] = "dismiss pets";
		powers[num++] = PET_DISMISS;
	}

	power_desc[num] = "stay close";
	if (p_ptr->pet_follow_distance == PET_CLOSE_DIST) mode = num;
	powers[num++] = PET_STAY_CLOSE;

	power_desc[num] = "follow me";
	if (p_ptr->pet_follow_distance == PET_FOLLOW_DIST) mode = num;
	powers[num++] = PET_FOLLOW_ME;

	power_desc[num] = "seek and destroy";
	if (p_ptr->pet_follow_distance == PET_DESTROY_DIST) mode = num;
	powers[num++] = PET_SEEK_AND_DESTROY;

	power_desc[num] = "give me space";
	if (p_ptr->pet_follow_distance == PET_SPACE_DIST) mode = num;
	powers[num++] = PET_ALLOW_SPACE;

	power_desc[num] = "stay away";
	if (p_ptr->pet_follow_distance == PET_AWAY_DIST) mode = num;
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
		(void)strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
			I2A(0), I2A(num - 1));
	}
	else
	{
		(void)strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
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
			bell();
			continue;
		}

		/* Verify it */
		if (ask)
		{
			/* Prompt */
			(void)strnfmt(buf, 78, "Use %s? ", power_desc[i]);

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

				if (is_pet(m_ptr))
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
						/* Notice changes in view */
						if (r_info[m_ptr->r_idx].flags7 &
							 (RF7_LITE_1 | RF7_LITE_2))
						{
							/* Update some things */
							p_ptr->update |= (PU_MON_LITE);
						}
												
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
			p_ptr->pet_follow_distance = PET_CLOSE_DIST;
			break;
		}
		/* "Follow Me" */
		case PET_FOLLOW_ME:
		{
			p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
			break;
		}
		/* "Seek and destoy" */
		case PET_SEEK_AND_DESTROY:
		{
			p_ptr->pet_follow_distance = PET_DESTROY_DIST;
			break;
		}
		/* "Give me space" */
		case PET_ALLOW_SPACE:
		{
			p_ptr->pet_follow_distance = PET_SPACE_DIST;
			break;
		}
		/* "Stay away" */
		case PET_STAY_AWAY:
		{
			p_ptr->pet_follow_distance = PET_AWAY_DIST;
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

					if (is_pet(m_ptr))
					{
						monster_drop_carried_objects(m_ptr);
					}
				}
			}

			break;
		}
	}
}

