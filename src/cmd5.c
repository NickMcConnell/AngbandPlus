/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "game-cmd.h"



/*
 * Returns chance of failure for a spell
 */
s16b spell_chance(int spell)
{
	int chance, minfail;

	const magic_type *s_ptr;


	/* Paranoia -- must be literate */
	if (!cp_ptr->spell_book) return (100);

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= adj_mag_stat[SPELL_STAT_SLOT];

	/* Not enough mana to cast */
	if (s_ptr->smana > p_ptr->csp)
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[SPELL_STAT_SLOT];

	/* Non mage/priest characters never get better than 5 percent */
	if (!(cp_ptr->flags & CF_ZERO_FAIL))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	if (p_ptr->icky_wield)
	{
		chance += 25;
	}

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder (after minfail) */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool known)
{
	const magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is ironman */
	if (p_ptr->spell_flags[spell] & PY_SPELL_IRONMAN)
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if (p_ptr->spell_flags[spell] & PY_SPELL_LEARNED)
	{

		/* Okay to cast, not to study */
		return (known);
	}

	/* Okay to study, not to cast */
	return (!known);
}

/*
 * Print a list of spells (for browsing or casting or viewing).
 */
void print_spells(const byte *spells, int num, int row, int col)
{
	int i, spell;

	const magic_type *s_ptr;

	cptr comment;

	char out_val[500];

	byte line_attr;

	int length;

	int wrap = Term->wid - col;

	/* The length of the spell name, level, and mana info*/
	int indent = 48;

	/* Title the list */
	prt("", row, col);
	put_str("Name", row, col + 5);
	put_str("Lv Mana Fail Info", row, col + 35);

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		int use_indent = 0;

		/* Get the spell index */
		spell = spells[i];

		/* Get the spell info */
		s_ptr = &mp_ptr->info[spell];

		/* Skip illegible spells */
		if (s_ptr->slevel >= 99)
		{
			strnfmt(out_val, sizeof(out_val), "  %c) %-30s", I2A(i), "(illegible)");
			c_prt(TERM_L_DARK, out_val, row + i + 1, col);
			continue;
		}

		/* Get extra info */
		switch (cp_ptr->spell_book)
		{
			case TV_MAGIC_BOOK:
			{
				comment = do_mage_spell(MODE_SPELL_DESC, spell, 0);
				break;
			}
			case TV_PRAYER_BOOK:
			{
				comment = do_priest_prayer(MODE_SPELL_DESC, spell, 0);
				break;
			}
			case TV_DRUID_BOOK:
			{
				comment = do_druid_spell(MODE_SPELL_DESC, spell, 0);
				break;
			}
			/*Oops*/
			default: comment = "Error!";
		}

		/* Assume spell is known and tried */
		line_attr = TERM_WHITE;

		/* Analyze the spell */
		if (p_ptr->spell_flags[spell] & PY_SPELL_IRONMAN)
		{
			comment = "Ironman Spell";
			line_attr = TERM_L_RED;
		}
		/* Analyze the spell */
		else if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
		{
			comment = "forgotten";
			line_attr = TERM_YELLOW;
		}
		else if (!(p_ptr->spell_flags[spell] & PY_SPELL_LEARNED))
		{
			if (s_ptr->slevel <= p_ptr->lev)
			{
				comment = "unknown";
				line_attr = TERM_L_BLUE;
			}
			else
			{
				comment = "difficult";
				line_attr = TERM_RED;
			}
		}
		else if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			comment = "untried";
			line_attr = TERM_L_GREEN;
		}

		/* Dump the spell --(-- */
		strnfmt(out_val, sizeof(out_val), "  %c) %-30s%2d %4d %3d%% %s",
		        I2A(i), (cast_spell(MODE_SPELL_NAME, cp_ptr->spell_book, spell, 0)),
		        s_ptr->slevel, s_ptr->smana, spell_chance(spell), comment);

		length = strlen(out_val);

		/* Break up long notes */
		if ((length > wrap) && (indent < wrap))
		{
			int startpoint = 0;
			int endpoint, n, x;

			while (TRUE)
			{
				char partial_out_val[200];

				/* Don't print more than the set linewrap amount */
				endpoint = startpoint + wrap - use_indent - 1;

				/* The remaining portion of the string fits in the screen */
				if (endpoint > (length - 1))
				{
					/* Print all */
				       	endpoint = (length - 1);
				}
				/* We are in the middle of a word */
				else if (out_val[endpoint + 1] != ' ')
				{
					/* Find a breaking point */
					while ((endpoint > startpoint) &&
						(out_val[endpoint] != ' ') &&
						(out_val[endpoint] != '-'))
					{
						--endpoint;
					}

					/* No spaces in the line, so break in the middle of text */
					if (endpoint == startpoint)
					{
						endpoint = startpoint + wrap - use_indent - 1;
					}
				}

				/* Write that line to file */
				for (x = 0; x < use_indent; x++)
				{
					/* Insert Blank Space */
					partial_out_val[x] = ' ';
				}

				/* Write that line to file */
				for (n = startpoint; n <= endpoint; x++, n++)
				{
					/* Ensure the character is printable */
					partial_out_val[x] = (isprint(out_val[n]) ? out_val[n] : ' ');

				}

				/* Terminate */
				partial_out_val[x] = '\0';

				c_prt(line_attr, partial_out_val, row + i + 1, col);

				/* Prepare for the next line */
				startpoint = endpoint + 1;

				/* Trim whitespace */
				while (out_val[startpoint] == ' ') ++startpoint;

				/* We reached the end of the string so this spell is done */
				if (!out_val[startpoint]) break;

				/* Indent from now on */
				use_indent = indent;

				/* Prepare for the next line */
				++row;
			}

		}

 	 	/* Add note to buffer */
 	 	else
		{
			c_prt(line_attr, out_val, row + i + 1, col);
		}

	}

	/* Clear the bottom line */
	prt("", row + i + 1, col);
}


/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readible books
 */
void display_koff(int k_idx)
{
	int y;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[80];


	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* No info */
	if (!k_idx) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Prepare the object */
	object_wipe(i_ptr);

	/* Prepare the object */
	object_prep(i_ptr, k_idx);


	/* Describe */
	object_desc_spoil(o_name, sizeof(o_name), i_ptr, FALSE, 0);

	/* Mention the object name */
	Term_putstr(0, 0, -1, TERM_WHITE, o_name);


	/* Warriors are illiterate */
	if (!cp_ptr->spell_book) return;

	/* Display spells in readible books */
	if (i_ptr->tval == cp_ptr->spell_book)
	{
		int i;
		int spell;
		int num = 0;

		byte spells[PY_MAX_SPELLS];


		/* Extract spells */
		for (i = 0; i < SPELLS_PER_BOOK; i++)
		{
			spell = get_spell_index(i_ptr, i);

			/* Collect this spell */
			if (spell >= 0) spells[num++] = spell;
		}

		/* Print spells */
		print_spells(spells, num, 2, 0);
	}
}


/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * Returns -1 if the user hits escape.
 * Returns -2 if there are no legal choices.
 * Returns a valid spell otherwise.
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */
int get_spell(const object_type *o_ptr, cptr prompt, bool known)
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

	cptr p = ((cp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		spell = get_spell_index(o_ptr, i);

		/* Collect this spell */
		if (spell != -1) spells[num++] = spell;
	}

	/* Assume no usable spells */
	okay = FALSE;

	/* Check for "okay" spells */
	for (i = 0; i < num; i++)
	{

		/* Look for "okay" spells */
		if (spell_okay(spells[i], known)) okay = TRUE;

	}

	/* No available spells */
	if (!okay) return (-2);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
	        p, I2A(0), I2A(num - 1), prompt, p);

	/* Option -- automatically show lists */
	if (auto_display_lists)
	{
		/* Show list */
		redraw = TRUE;

		/* Save screen */
		screen_save();

		/* Display a list of spells */
		print_spells(spells, num, 2, 2);
	}

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
				print_spells(spells, num, 2, 2);
			}

			/* Ask again */
			continue;
		}

		/* Note verify */
		verify = (isupper((unsigned char)choice) ? TRUE : FALSE);

		/* Lowercase */
		choice = tolower((unsigned char)choice);

		/* Extract request */
		i = (islower((unsigned char)choice) ? A2I(choice) : -1);

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
			        prompt, get_spell_name(cp_ptr->spell_book, spell),
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
	}

	/* Abort if needed */
	if (!flag) return (-1);

	/* Success */
	return (spell);

}



void do_cmd_browse_aux(const object_type *o_ptr)
{
	int i;
	int spell;
	int num = 0;
	cptr spell_type;

	byte spells[SPELLS_PER_BOOK];

	/* Track the object kind */
	track_object_kind(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (cp_ptr->spell_book == TV_PRAYER_BOOK) spell_type = "prayer";
	else spell_type = "spell";

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		spell = get_spell_index(o_ptr, i);

		/* Collect this spell */
		if (spell != -1) spells[num++] = spell;
	}

	/* Save screen */
	screen_save();

	/* Display the spells */
	print_spells(spells, num, 2, 2);

	/* Note */
	prt("Hit any key to continue...", 0, 0);

	/* Wait for the player to press any key*/
	inkey();

	/* Load screen */
	screen_load();

	/* Hack -- Process "Escape" */
	if (p_ptr->command_new == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new = 0;
	}
}




/* Is the player capable of casting a spell? */
bool player_can_cast(void)
{
	if (!cp_ptr->spell_book)
	{
		msg_print("You cannot cast spells!");
		return FALSE;
	}

	if (p_ptr->timed[TMD_BLIND] || no_light())
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return FALSE;
	}

	return TRUE;
}

/* Is the player capable of studying? */
bool player_can_study(void)
{
	if (!player_can_cast())
		return FALSE;

	if (!p_ptr->new_spells)
	{
		cptr p = ((cp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");
		msg_format("You cannot learn any new %ss!", p);
		return FALSE;
	}

	return TRUE;
}

/* Check if the given spell is in the given book. */
static bool spell_in_book(int spell, int book)
{
	int i;
	object_type *o_ptr = object_from_item_idx(book);

	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		if (spell == get_spell_index(o_ptr, i))
			return TRUE;
	}

	return FALSE;
}

/* Gain a specific spell, specified by spell number (for mages). */
void do_cmd_study_spell(cmd_code code, cmd_arg args[])
{
	int spell = args[0].choice;

	int item_list[INVEN_TOTAL + MAX_FLOOR_STACK];
	int item_num;
	int i;

	/* Check the player can study at all atm */
	if (!player_can_study())
		return;

	/* Check that the player can actually learn the nominated spell. */
	item_tester_hook = obj_can_browse;
	item_num = scan_items(item_list, N_ELEMENTS(item_list), (USE_INVEN | USE_FLOOR));

	/* Check through all available books */
	for (i = 0; i < item_num; i++)
	{
		if (spell_in_book(spell, item_list[i]))
		{
			if (spell_okay(spell, FALSE))
			{
				/* Spell is in an available book, and player is capable. */
				spell_learn(spell);
				p_ptr->p_energy_use = BASE_ENERGY_MOVE;
			}
			else
			{
				/* Spell is present, but player incapable. */
				msg_format("You cannot learn that spell.");
			}

			return;
		}
	}
}




/* Gain a random spell from the given book (for priests) */
void do_cmd_study_book(cmd_code code, cmd_arg args[])
{
	int book = args[0].item;
	object_type *o_ptr = object_from_item_idx(book);

	int spell = -1;
	int i, k = 0;

	cptr p = ((cp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

	/* Check the player can study at all atm */
	if (!player_can_study())
		return;

	/* Check that the player has access to the nominated spell book. */
	if (!item_is_available(book, obj_can_browse, (USE_INVEN | USE_FLOOR)))
	{
		msg_format("That item is not within your reach.");
		return;
	}

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		int s = get_spell_index(o_ptr, i);

		/* Skip non-OK spells */
		if (s == -1) continue;
		if (!spell_okay(s, FALSE)) continue;

		/* Apply the randomizer */
		if ((++k > 1) && (randint0(k) != 0)) continue;

		/* Track it */
		spell = s;
	}

	if (spell < 0)
	{
		msg_format("You cannot learn any %ss in that book.", p);
	}
	else
	{
		spell_learn(spell);
		p_ptr->p_energy_use = BASE_ENERGY_MOVE;
	}
}



/* Cast a spell from a book */
void do_cmd_cast(cmd_code code, cmd_arg args[])
{
	int spell = args[0].choice;
	int dir = args[1].direction;

	int chance;

	bool prayer = cp_ptr->spell_book == TV_PRAYER_BOOK ? TRUE : FALSE;

	const magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		if (prayer) msg_print("You do not have enough mana to cast this prayer.");
		else msg_print("You do not have enough mana to cast this spell.");

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
		/* Cast the spell */
		sound(MSG_SPELL);
		if (cast_spell(MODE_SPELL_CAST, cp_ptr->spell_book, spell, dir) == NULL) return;

		/* A spell was cast */
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->redraw |= (PR_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

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
		(void)inc_timed(TMD_PARALYZED, randint1(5 * oops + 1), TRUE);

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


/*
 * Learn the specified spell.
 */
void spell_learn(int spell)
{
	int i;
	cptr p = ((cp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

	/* Learn the spell */
	p_ptr->spell_flags[spell] |= PY_SPELL_LEARNED;

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
	           p, get_spell_name(cp_ptr->spell_book, spell));

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more %s%s.",
		           p_ptr->new_spells, p, PLURAL(p_ptr->new_spells));
	}

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY | PR_OBJECT);
}

