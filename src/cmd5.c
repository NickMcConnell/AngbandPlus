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
static int get_spell(object_type *o_ptr, int *sn, cptr prompt, int sval, bool known, bool realm_2)
{
	int         i;
	int         spell;
	int         num = 0;
	int         ask;
	byte        spells[64];
	bool        flag, redraw, okay;
	char        choice;
	magic_type  *s_ptr;
	char        out_val[160];
	cptr        p = "spell";

/*   spell = 0;

   s_ptr = &s_info[spell];*/

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known, p_ptr->realm1))
		{
			/* Success */
			return (TRUE);
		}
	}

   if (p_ptr->realm2)
   {
   	/* Get the spell, if available */
	   if (repeat_pull(sn))
   	{
	   	/* Verify the spell */
		   if (spell_okay(*sn, known, p_ptr->realm2))
   		{
	   		/* Success */
		   	return (TRUE);
         }
		}
	}

	/* Extract spells */
	for (spell = 0; spell < 8; spell++)
	{
		/* Check for this spell */
		if (o_ptr->spellist[spell] != 0)
		{
 			/* Collect this spell */
     		spells[num++] = o_ptr->spellist[spell];
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
		if (spell_okay(spells[i], known, p_ptr->realm1)) okay = TRUE;
		if (spell_okay(spells[i], known, p_ptr->realm2)) okay = TRUE;
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
				print_spells(spells, num, 1, 20, 0);
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
		if (!spell_okay(spell, known, p_ptr->realm1) && !spell_okay(spell, known, p_ptr->realm2))
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
			s_ptr = &s_info[spell];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, "A nifty Spell",
				s_ptr->smana, spell_chance(spell, 0));

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


/*
 * Peruse the spells/prayers in a given book.  Note that we may
 * bypass do_cmd_browse by calling this function directly (as we
 * do from identify_fully_aux() which has the effect of allowing
 * any book to be browsed regardless of the player's realm choice.
 *
 * Note that *all* spells in the book are listed
 */
void do_cmd_browse_aux(const object_type *o_ptr)
{
	int sval;
	int spell;
	int num = 0;

	byte spells[8];


	/* Access the item's sval */
	sval = o_ptr->sval;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

   for (spell = 0; spell < 8; spell++)
   {
      spells[spell] = 0;
   }

	/* Extract spells */
	for (spell = 0; spell < 8; spell++)
	{
/*      if (o_ptr->spellist[spell] != 0)
      {*/
         spells[num++] = o_ptr->spellist[spell];
/*      }*/
		/* Check for this spell */
/*		if ((fake_spell_flags[sval] & (1L << spell)))
		{
			spells[num++] = spell;
		}*/
	}


	/* Save the screen */
	screen_save();

	/* Display the spells */
	print_spells(spells, num, 1, 20, 0);

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);

	/* Wait for key */
	(void)inkey();

	/* Restore the screen */
	screen_load();
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
	item_tester_hook = item_tester_hook_has_spell;

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


/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int	i, item, sval;
	int	increment = 0;

	/* Spells of realm2 will have an increment of +32 */
	int	spell = -1;

	cptr p = "spell";

	object_type *o_ptr;

	cptr q, s;

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
	item_tester_hook = item_tester_hook_has_spell;

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
/*	if (mp_ptr->spell_book != TV_SPELL_BOOK)
	{*/
		/* Ask for a spell, allow cancel */
		if (!get_spell(o_ptr, &spell, "study", sval, FALSE,
		    (bool)(increment ? TRUE : FALSE)) && (spell == -1)) return;
/*	}*/

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

/*
	if (mp_ptr->spell_book == TV_SPELL_BOOK)
		chg_virtue(V_FAITH, 1);
	else
		chg_virtue(V_KNOWLEDGE, 1);
*/
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
			(void)wall_stone();
			break;
		}
		case 34:
		case 35:
		{
			int i;
			int type = bizarre_num[randint0(6)];

			for (i = 0; i < 8; i++)
			{
				(void)summon_specific(0, py, px, (p_ptr->depth * 3) / 2, type, TRUE, FALSE, FALSE);
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
   int   bpct = 0;
	bool cast;

	const cptr prayer = "spell";

	object_type	*o_ptr;

	const magic_type *s_ptr;

	cptr q, s, tmp;

   realm = 0;

/*   spell = 0;

   s_ptr = &s_info[spell];*/

   use_realm = 0;

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
	item_tester_hook = item_tester_hook_has_spell;

	/* Get an item */
	q = "Use which item? ";
	s = "You have no spell items!";
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

/*	if (o_ptr->tval == REALM2_BOOK) increment = 32;*/


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

/*	if (increment) realm = p_ptr->realm2;
	else realm = p_ptr->realm1;*/

/*   spell = 0;*/

	/* Ask for a spell */
	if (!get_spell(o_ptr, &spell, "cast",
	               sval, TRUE, (bool)(increment ? TRUE : FALSE)))
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */
/*	use_realm = (increment ? p_ptr->realm2 : p_ptr->realm1);*/

	s_ptr = &s_info[spell];

   tmp = s_name + s_ptr->name;

	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s %s!", "cast", tmp);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell, 0);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

		msg_format("You failed to get the %s off!", prayer);
		sound(SOUND_FAIL);

		if (p_ptr->realm1 == REALM_TRUMP)
		{
			msg_print("You produce a chaotic effect!");
			wild_magic(spell);
		}
	}

	/* Process spell */
	else
	{
      bpct = 5;
      if (p_ptr->pclass == CLASS_HIGH_MAGE) bpct = p_ptr->lev + 10;
      if (p_ptr->pclass != CLASS_HIGH_MAGE) bpct = p_ptr->lev;

      tmp = s_name + s_ptr->name;

      msg_format("You cast %s!", tmp);

      cast = act_magic( p_ptr->px, p_ptr->py, s_ptr->activation, 0, 0,
                        1, (p_ptr->lev - s_ptr->slevel), s_ptr->eff1, s_ptr->eff2,
                        0, bpct);

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;
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

void do_cmd_master_book(void)
{
   cptr SpellName[64];
   int  SpellColor[64];
   cptr sName;

   char  cmd,cmd2;

   int   i,j,k,l,m;

   int   item;

   int   realmcnt, realm;

   int   room1,room2;

   int   add1,add2,add3;

   int   done;

   char  out_val[160];

   bool  redraw;

   int   has_slots;

   magic_type  *s_ptr;

   object_type *o_ptr;

   realmcnt = 1;

   done = 0;

   if (p_ptr->realm1 == 0) return;

   redraw = TRUE;

   i = 0; j = 0; k = 0; l = 0; m = 0; room1 = 0; room2 = 0; has_slots = 0;

	screen_save();

   realm = p_ptr->realm1;

   done = 0;

   while(!done)
   {
      i = 0; j = 0; k = 0; l = 0; room1 = 0; room2 = 0; has_slots = 0;
      add1 = 0; add2 = 0; item = 0; add3 = 0;

      realm = p_ptr->realm1;

      k = 0;

      for (i = 0; i < 32; i++)
      {
         if (realm == 1 ) j = mp_ptr->life[i];
         if (realm == 2 ) j = mp_ptr->sorcery[i];
         if (realm == 3 ) j = mp_ptr->nature[i];
         if (realm == 4 ) j = mp_ptr->chaos[i];
         if (realm == 5 ) j = mp_ptr->death[i];
         if (realm == 6 ) j = mp_ptr->trump[i];
         if (realm == 7 ) j = mp_ptr->arcane[i];
         if (realm == 8 ) j = mp_ptr->chi[i];
         if (realm == 9 ) j = mp_ptr->elemental[i];
         if (realm == 10) j = mp_ptr->general[i];

         if (j == 0)
         {
            SpellName[k] = "None";
            SpellColor[k] = TERM_L_DARK;
         }

         if (j != 0)
         {
            s_ptr = &s_info[j];
            SpellName[k] = s_name + s_ptr->name;
            SpellColor[k] = TERM_L_GREEN;
         }
         k++;
      }

      realm = p_ptr->realm2;

      for (i = 0; i < 32; i++)
      {
         if (realm == 1 ) j = mp_ptr->life[i];
         if (realm == 2 ) j = mp_ptr->sorcery[i];
         if (realm == 3 ) j = mp_ptr->nature[i];
         if (realm == 4 ) j = mp_ptr->chaos[i];
         if (realm == 5 ) j = mp_ptr->death[i];
         if (realm == 6 ) j = mp_ptr->trump[i];
         if (realm == 7 ) j = mp_ptr->arcane[i];
         if (realm == 8 ) j = mp_ptr->chi[i];
         if (realm == 9 ) j = mp_ptr->elemental[i];
         if (realm == 10) j = mp_ptr->general[i];

         if (j == 0)
         {
            SpellName[k] = "None";
            SpellColor[k] = TERM_L_DARK;
         }

         if (j != 0)
         {
            s_ptr = &s_info[j];
            SpellName[k] = s_name + s_ptr->name;
            SpellColor[k] = TERM_L_GREEN;
         }
         k++;
      }

      Term_clear();

      if (realmcnt == 1) realm = p_ptr->realm1;
      if (realmcnt == 2) realm = p_ptr->realm2;

      sprintf(out_val, "Master Spell List");
      c_put_str(TERM_WHITE, out_val, 1, 4);
      sprintf(out_val, "Realm : %s", realm_names[realm]);
      c_put_str(TERM_YELLOW, out_val, 2, 4);

      if (realm == p_ptr->realm1) k = 0;
      if (realm == p_ptr->realm2) k = 32;

      for (i = 0; i < 16; i++)
      {
         (void)sprintf(out_val, "%2d) %s", i, SpellName[k+i]);
			c_put_str(SpellColor[k+i], out_val, i+4, 4);
         (void)sprintf(out_val, "%2d) %s", i+16, SpellName[k+i+16]);
			c_put_str(SpellColor[k+i+16], out_val, i+4, 39);
      }

      sprintf(out_val, "a) Add    f) Filter    s) Switch realm    w) Write to book    ESC) Exit");
      c_put_str(TERM_SLATE, out_val, 21, 4);

      /* Wait for key */
	   cmd = inkey();

      if (cmd == 'a' || cmd == 'A')
      {
         room1  = 32; room2 = 32;
         realm = p_ptr->realm1;

         for (i = 0; i < 32; i++)
         {
            j = 0;

            if (realm == 1 ) j = mp_ptr->life[i];
            if (realm == 2 ) j = mp_ptr->sorcery[i];
            if (realm == 3 ) j = mp_ptr->nature[i];
            if (realm == 4 ) j = mp_ptr->chaos[i];
            if (realm == 5 ) j = mp_ptr->death[i];
            if (realm == 6 ) j = mp_ptr->trump[i];
            if (realm == 7 ) j = mp_ptr->arcane[i];
            if (realm == 8 ) j = mp_ptr->chi[i];
            if (realm == 9 ) j = mp_ptr->elemental[i];
            if (realm == 10) j = mp_ptr->general[i];

            if (j != 0) room1--;
         }

         realm = p_ptr->realm2;

         for (i = 0; i < 32; i++)
         {
            j = 0;

            if (realm == 1 ) j = mp_ptr->life[i];
            if (realm == 2 ) j = mp_ptr->sorcery[i];
            if (realm == 3 ) j = mp_ptr->nature[i];
            if (realm == 4 ) j = mp_ptr->chaos[i];
            if (realm == 5 ) j = mp_ptr->death[i];
            if (realm == 6 ) j = mp_ptr->trump[i];
            if (realm == 7 ) j = mp_ptr->arcane[i];
            if (realm == 8 ) j = mp_ptr->chi[i];
            if (realm == 9 ) j = mp_ptr->elemental[i];
            if (realm == 10) j = mp_ptr->general[i];

            if (j != 0) room2--;
         }

         if (p_ptr->realm1 != 0 && room1 > 0) has_slots = 1;
         if (p_ptr->realm2 != 0 && room2 > 0) has_slots = 1;

         if (has_slots)
         {
            for (i = 0; i < INVEN_TOTAL; i++)
         	{
               Term_clear();
		         o_ptr = &inventory[i];

	      	   /* Skip non-objects */
         		if (!o_ptr->k_idx) continue;

               object_desc(out_val, o_ptr, 0, 0);

               c_put_str(TERM_SLATE, out_val, 1, 4);

               (void)inkey();


               for (j = 0; j < 8; j++)
               {
                  if (o_ptr->spellist[j] != 0)
                  {
                     add1 = 0;
                     add2 = 0;

                     k = o_ptr->spellist[j];
                     s_ptr = &s_info[k];
                     sName = s_name + s_ptr->name;

                     if (p_ptr->realm1 && room1 > 0) add1 = check_realm2( s_ptr, p_ptr->realm1);
                     if (p_ptr->realm2 && room2 > 0) add2 = check_realm2( s_ptr, p_ptr->realm2);

                     if (add1 && check_spell(s_ptr)) add1 = 0;
                     if (add2 && check_spell(s_ptr)) add2 = 0;

                     if ((add1) || (add2))
                     {
                        cmd = '\0';

                        Term_clear();
                        sprintf(out_val, "Add spell %s to spell book?", sName);
               			c_put_str(TERM_SLATE, out_val, 2, 4);

                        if (add1)
                        {
                           sprintf(out_val, "1) Add to %s", realm_names[p_ptr->realm1]);
                           c_put_str(TERM_SLATE, out_val, 3, 4);
                        }

                        if (add2)
                        {
                           sprintf(out_val, "2) Add to %s", realm_names[p_ptr->realm2]);
                           c_put_str(TERM_SLATE, out_val, 4, 4);
                        }

                        cmd = inkey();

                        if (add1 && cmd == '1')
                        {
                           for (l = 0; l < 32; l++)
                           {
                              if (p_ptr->realm1 == 1 && mp_ptr->life[l] == 0)
                              {
                                 mp_ptr->life[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 2 && mp_ptr->sorcery[l] == 0)
                              {
                                 mp_ptr->sorcery[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 3 && mp_ptr->nature[l] == 0)
                              {
                                 mp_ptr->nature[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 4 && mp_ptr->chaos[l] == 0)
                              {
                                 mp_ptr->chaos[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 5 && mp_ptr->death[l] == 0)
                              {
                                 mp_ptr->death[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 6 && mp_ptr->trump[l] == 0)
                              {
                                 mp_ptr->trump[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 7 && mp_ptr->arcane[l] == 0)
                              {
                                 mp_ptr->arcane[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 8 && mp_ptr->chi[l] == 0)
                              {
                                 mp_ptr->chi[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 9 && mp_ptr->elemental[l] == 0)
                              {
                                 mp_ptr->elemental[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm1 == 10 && mp_ptr->general[l] == 0)
                              {
                                 mp_ptr->general[l] = s_ptr->index;
                                 l = 32;
                              }
                           }
                        }

                        if (add2 && cmd == '2')
                        {
                           for (l = 0; l < 32; l++)
                           {
                              if (p_ptr->realm2 == 1 && mp_ptr->life[l] == 0)
                              {
                                 mp_ptr->life[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 2 && mp_ptr->sorcery[l] == 0)
                              {
                                 mp_ptr->sorcery[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 3 && mp_ptr->nature[l] == 0)
                              {
                                 mp_ptr->nature[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 4 && mp_ptr->chaos[l] == 0)
                              {
                                 mp_ptr->chaos[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 5 && mp_ptr->death[l] == 0)
                              {
                                 mp_ptr->death[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 6 && mp_ptr->trump[l] == 0)
                              {
                                 mp_ptr->trump[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 7 && mp_ptr->arcane[l] == 0)
                              {
                                 mp_ptr->arcane[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 8 && mp_ptr->chi[l] == 0)
                              {
                                 mp_ptr->chi[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 9 && mp_ptr->elemental[l] == 0)
                              {
                                 mp_ptr->elemental[l] = s_ptr->index;
                                 l = 32;
                              }
                              if (p_ptr->realm2 == 10 && mp_ptr->general[l] == 0)
                              {
                                 mp_ptr->general[l] = s_ptr->index;
                                 l = 32;
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }

      if (cmd == 's' || cmd == 'S')
      {
         realmcnt++;
         if (realmcnt > 2) realmcnt = 1;
      }

      if (cmd == 'f' || cmd == 'F')
      {
         realm = p_ptr->realm1;

         for (i = 0; i < 31; i++)
         {
            if (realm == 1 ) j = mp_ptr->life[i];
            if (realm == 2 ) j = mp_ptr->sorcery[i];
            if (realm == 3 ) j = mp_ptr->nature[i];
            if (realm == 4 ) j = mp_ptr->chaos[i];
            if (realm == 5 ) j = mp_ptr->death[i];
            if (realm == 6 ) j = mp_ptr->trump[i];
            if (realm == 7 ) j = mp_ptr->arcane[i];
            if (realm == 8 ) j = mp_ptr->chi[i];
            if (realm == 9 ) j = mp_ptr->elemental[i];
            if (realm == 10) j = mp_ptr->general[i];

            if (j != 0)
            {
               for (k = i+1; k < 32; k++)
               {
                  if (realm == 1 ) l = mp_ptr->life[k];
                  if (realm == 2 ) l = mp_ptr->sorcery[k];
                  if (realm == 3 ) l = mp_ptr->nature[k];
                  if (realm == 4 ) l = mp_ptr->chaos[k];
                  if (realm == 5 ) l = mp_ptr->death[k];
                  if (realm == 6 ) l = mp_ptr->trump[k];
                  if (realm == 7 ) l = mp_ptr->arcane[k];
                  if (realm == 8 ) l = mp_ptr->chi[k];
                  if (realm == 9 ) l = mp_ptr->elemental[k];
                  if (realm == 10) l = mp_ptr->general[k];

                  if (l == j)
                  {
                     if (realm == 1 ) mp_ptr->life[k] = 0;
                     if (realm == 2 ) mp_ptr->sorcery[k] = 0;
                     if (realm == 3 ) mp_ptr->nature[k] = 0;
                     if (realm == 4 ) mp_ptr->chaos[k] = 0;
                     if (realm == 5 ) mp_ptr->death[k] = 0;
                     if (realm == 6 ) mp_ptr->trump[k] = 0;
                     if (realm == 7 ) mp_ptr->arcane[k] = 0;
                     if (realm == 8 ) mp_ptr->chi[k] = 0;
                     if (realm == 9 ) mp_ptr->elemental[k] = 0;
                     if (realm == 10) mp_ptr->general[k] = 0;
                  }
               }
            }
         }

         realm = p_ptr->realm2;

         for (i = 0; i < 31; i++)
         {
            if (realm == 1 ) j = mp_ptr->life[i];
            if (realm == 2 ) j = mp_ptr->sorcery[i];
            if (realm == 3 ) j = mp_ptr->nature[i];
            if (realm == 4 ) j = mp_ptr->chaos[i];
            if (realm == 5 ) j = mp_ptr->death[i];
            if (realm == 6 ) j = mp_ptr->trump[i];
            if (realm == 7 ) j = mp_ptr->arcane[i];
            if (realm == 8 ) j = mp_ptr->chi[i];
            if (realm == 9 ) j = mp_ptr->elemental[i];
            if (realm == 10) j = mp_ptr->general[i];

            if (j != 0)
            {
               for (k = i+1; k < 32; k++)
               {
                  if (realm == 1 ) l = mp_ptr->life[k];
                  if (realm == 2 ) l = mp_ptr->sorcery[k];
                  if (realm == 3 ) l = mp_ptr->nature[k];
                  if (realm == 4 ) l = mp_ptr->chaos[k];
                  if (realm == 5 ) l = mp_ptr->death[k];
                  if (realm == 6 ) l = mp_ptr->trump[k];
                  if (realm == 7 ) l = mp_ptr->arcane[k];
                  if (realm == 8 ) l = mp_ptr->chi[k];
                  if (realm == 9 ) l = mp_ptr->elemental[k];
                  if (realm == 10) l = mp_ptr->general[k];

                  if (l == j)
                  {
                     if (realm == 1 ) mp_ptr->life[k] = 0;
                     if (realm == 2 ) mp_ptr->sorcery[k] = 0;
                     if (realm == 3 ) mp_ptr->nature[k] = 0;
                     if (realm == 4 ) mp_ptr->chaos[k] = 0;
                     if (realm == 5 ) mp_ptr->death[k] = 0;
                     if (realm == 6 ) mp_ptr->trump[k] = 0;
                     if (realm == 7 ) mp_ptr->arcane[k] = 0;
                     if (realm == 8 ) mp_ptr->chi[k] = 0;
                     if (realm == 9 ) mp_ptr->elemental[k] = 0;
                     if (realm == 10) mp_ptr->general[k] = 0;
                  }
               }
            }
         }
      }

      if (cmd == 'w' || cmd == 'W')
      {
         item = 0;

      	item_tester_hook = item_tester_hook_is_book;
	      if (get_item(&item, "Copy to which book? ", "You have no book to copy to.", (USE_INVEN | USE_EQUIP)))
         {
      		o_ptr = &inventory[item];

            j = 0;

            for (i = 0; i < 8; i++)
            {
               if (o_ptr->spellist[i] == 0) j = 1;
            }

            l = 0; m = 0;

            add3 = 0;

/*            if (realm == p_ptr->realm1) add3 = 0;
            if (realm == p_ptr->realm2) add3 = 32;*/

            if (j == 1)
            {
               while(!l)
               {
                  Term_clear();

                  if (realmcnt == 1) realm = p_ptr->realm1;
                  if (realmcnt == 2) realm = p_ptr->realm2;

                  sprintf(out_val, "Master Spell List");
                  c_put_str(TERM_WHITE, out_val, 1, 4);
                  sprintf(out_val, "Realm : %s", realm_names[realm]);
                  c_put_str(TERM_YELLOW, out_val, 2, 4);

                  if (realm == p_ptr->realm1) k = 0;
                  if (realm == p_ptr->realm2) k = 32;

                  for (i = 0; i < 16; i++)
                  {
                     (void)sprintf(out_val, "%2d) %s", i, SpellName[k+i]);
                     add1 = TERM_SLATE;
                     if (add3 == i) add1 = TERM_YELLOW;
			            c_put_str(add1, out_val, i+4, 4);
                     (void)sprintf(out_val, "%2d) %s", i+16, SpellName[k+i+16]);
                     add1 = TERM_SLATE;
                     if (add3 == i+16) add1 = TERM_YELLOW;
			            c_put_str(add1, out_val, i+4, 39);
                  }

                  cmd2 = inkey();

                  if (isdigit(cmd2))
                  {
                     add2 = 0;

            			add2 = get_keymap_dir(cmd2);

                     if (add2 == 2 && add3 < 31) add3++;

                     if (add2 == 8 && add3 > 0) add3--;

                     if (add2 == 4 && add3 - 16 > -1) add3 -= 16;

                     if (add2 == 6 && add3 + 16 < 32) add3 += 16;
                  }

                  if (cmd2 == '\r')
                  {
                     l = 1;

                     k = add3;

                     if (realm == 1 ) m = mp_ptr->life[k];
                     if (realm == 2 ) m = mp_ptr->sorcery[k];
                     if (realm == 3 ) m = mp_ptr->nature[k];
                     if (realm == 4 ) m = mp_ptr->chaos[k];
                     if (realm == 5 ) m = mp_ptr->death[k];
                     if (realm == 6 ) m = mp_ptr->trump[k];
                     if (realm == 7 ) m = mp_ptr->arcane[k];
                     if (realm == 8 ) m = mp_ptr->chi[k];
                     if (realm == 9 ) m = mp_ptr->elemental[k];
                     if (realm == 10) m = mp_ptr->general[k];

                     if (m != 0)
                     {
                        msg_print("You copy the spell!");

                        for (i = 0; i < 8; i++)
                        {
                           if (o_ptr->spellist[i] == 0)
                           {
                              o_ptr->spellist[i] = m;
                              i = 9;
                           }
                        }
                     }
                  }
                  if (cmd2 == ESCAPE) l = 1;
               }
            }
         }
      }

      if (cmd == ESCAPE) done = 1;

      if (realmcnt == 1) realm = p_ptr->realm1;
      if (realmcnt == 2) realm = p_ptr->realm2;

   }

	/* Restore the screen */
	screen_load();

}
