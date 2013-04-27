/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Andrew Sidwell
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

#include "option.h"
#include "tvalsval.h"
#include "wind_flg.h"

/**
 * \return chance of failure for a spell
 */
s16b spell_chance(int spell)
{
	int chance, minfail;

	const magic_type *s_ptr;


	/* Paranoia -- must be literate */
	if (!p_ptr->spell_book()) return (100);

	/* Get the spell */
	s_ptr = p_ptr->spell_info(spell);

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= adj_mag_stat[p_ptr->stat_ind[p_ptr->cp_ptr->spell_stat]];

	/* Not enough mana to cast */
	if (s_ptr->smana > p_ptr->csp)
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[p_ptr->cp_ptr->spell_stat]];

	/* Non mage/priest characters never get better than 5 percent */
	if (!(p_ptr->cp_ptr->flags & CF_ZERO_FAIL))
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
	unsigned int stunned = stun_level(p_ptr->timed[TMD_STUN]);
	if (stunned)
		{
		chance += (2<=stunned) ? 25 : 15;
		};

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}



/**
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool known)
{
	const magic_type* s_ptr = p_ptr->spell_info(spell);	/* Get the spell */

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
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


/**
 * Print a list of spells (for browsing or casting or viewing).
 */
void print_spells(const byte *spells, int num, int y, int x)
{
	int i, spell;

	const magic_type *s_ptr;

	const char* comment;

	char out_val[160];

	byte line_attr;

	/* Title the list */
	prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Lv Mana Fail Info", y, x + 35);

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		/* Get the spell index */
		spell = spells[i];

		/* Get the spell info */
		s_ptr = p_ptr->spell_info(spell);

		/* Skip illegible spells */
		if (s_ptr->slevel >= 99)
		{
			strnfmt(out_val, sizeof(out_val), "  %c) %-30s", I2A(i), "(illegible)");
			c_prt(TERM_L_DARK, out_val, y + i + 1, x);
			continue;
		}

		/* Get extra info */
		comment = get_spell_info(p_ptr->spell_book(), spell);

		/* Assume spell is known and tried */
		line_attr = TERM_WHITE;

		/* Analyze the spell */
		if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
		{
			comment = " forgotten";
			line_attr = TERM_YELLOW;
		}
		else if (!(p_ptr->spell_flags[spell] & PY_SPELL_LEARNED))
		{
			if (s_ptr->slevel <= p_ptr->lev)
			{
				comment = " unknown";
				line_attr = TERM_L_BLUE;
			}
			else
			{
				comment = " difficult";
				line_attr = TERM_RED;
			}
		}
		else if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			comment = " untried";
			line_attr = TERM_L_GREEN;
		}

		/* Dump the spell --(-- */
		strnfmt(out_val, sizeof(out_val), "  %c) %-30s%2d %4d %3d%%%s",
		        I2A(i), get_spell_name(p_ptr->spell_book(), spell),
		        s_ptr->slevel, s_ptr->smana, spell_chance(spell), comment);
		c_prt(line_attr, out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}



/**
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readable books
 */
void display_koff(int k_idx)
{
	int y;

	object_type object_type_body;
	object_type *i_ptr = &object_type_body;	/* Get local object */

	char o_name[80];


	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* No info */
	if (!k_idx) return;

	/* Prepare the object */
	WIPE(i_ptr);

	/* Prepare the object */
	object_prep(i_ptr, k_idx);


	/* Describe */
	object_desc_spoil(o_name, sizeof(o_name), i_ptr, FALSE, ODESC_BASE);

	/* Mention the object name */
	Term_putstr(0, 0, -1, TERM_WHITE, o_name);


	/* Warriors are illiterate */
	if (!p_ptr->spell_book()) return;

	/* Display spells in readible books */
	if (i_ptr->obj_id.tval == p_ptr->spell_book())
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



/**
 * Allow user to choose a spell/prayer from the given book.
 *
 * \return -1 if the user hits escape.
 * \return -2 if there are no legal choices.
 * \return a valid spell otherwise.
 *
 * \param o_ptr is the book
 * \param prompt should be "cast", "recite", or "study"
 * \param known should be TRUE for cast/pray, FALSE for study
 */
int get_spell(const object_type *o_ptr, const char* const prompt, bool known)
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

	const char* const p = ((p_ptr->spell_book() == TV_MAGIC_BOOK) ? "spell" : "prayer");

	int result;

	/* Get the spell, if available */
	if (repeat_pull(&result))
	{
		/* Verify the spell */
		if (spell_okay(result, known))
		{
			/* Success */
			return (result);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}

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
			s_ptr = p_ptr->spell_info(spell);

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, get_spell_name(p_ptr->spell_book(), spell),
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

	repeat_push(spell);

	/* Success */
	return (spell);
}


void do_cmd_browse_aux(const object_type *o_ptr)
{
	int i;
	int spell;
	int num = 0;

	byte spells[PY_MAX_SPELLS];


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

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


/**
 * Choose a new spell from the book.
 */
int spell_choose_new(const object_type *o_ptr)
{
	int i, k = 0;
	int gift = -1;
	
	const char* const p = ((p_ptr->spell_book() == TV_MAGIC_BOOK) ? "spell" : "prayer");

	/* Mage -- Learn a selected spell */
	if (p_ptr->cp_ptr->flags & CF_CHOOSE_SPELLS)
	{
		return get_spell(o_ptr, "study", FALSE);
	}

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		int spell = get_spell_index(o_ptr, i);

		/* Skip non-OK spells */
		if (spell == -1) continue;
		if (!spell_okay(spell, FALSE)) continue;

		/* Apply the randomizer */
		if ((++k > 1) && !one_in_(k)) continue;

		/* Track it */
		gift = spell;
	}

	/* Nothing to study */
	if (gift < 0)
		msg_format("You cannot learn any %ss in that book.", p);

	return gift;
}

/**
 * Learn the specified spell.
 */
void spell_learn(int spell)
{
	int i;
	const char* const p = ((p_ptr->spell_book() == TV_MAGIC_BOOK) ? "spell" : "prayer");

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
	           p, get_spell_name(p_ptr->spell_book(), spell));

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more %s%s.",
		           p_ptr->new_spells, p, PLURAL(p_ptr->new_spells));
	}

	/* Redraw Study Status, object recall */
	p_ptr->redraw |= (PR_STUDY | PR_OBJECT);
}


/** Cast the specified spell */
bool spell_cast(int spell)
{
	int chance;
    const char* const p = ((p_ptr->spell_book() == TV_MAGIC_BOOK) ?
			          "cast this spell" :
	    		      "recite this prayer");


	/* Get the spell */
	const magic_type* s_ptr = p_ptr->spell_info(spell);


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s.", p);

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return FALSE;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (OPTION(flush_failure)) flush();
		msg_print("You failed to concentrate hard enough!");
	}

	/* Process spell */
	else
	{
		/* Cast the spell */
		if (!cast_spell(p_ptr->spell_book(), spell)) return FALSE;

		/* A spell was cast */
		sound((p_ptr->spell_book() == TV_MAGIC_BOOK) ? MSG_SPELL : MSG_PRAYER);
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
		(void)p_ptr->inc_timed<TMD_PARALYZED>(randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (one_in_(2))
		{
			bool perm = one_in_(4);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	return TRUE;
}
