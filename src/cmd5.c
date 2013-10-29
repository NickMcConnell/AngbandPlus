/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "script.h"


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
	chance -= adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]];

	/* Not enough mana to cast */
	if ((s_ptr->smana > p_ptr->csp) && (!p_ptr->manafree))
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	}
	/* max fail rate before all modifiers (soft cap, if you have enough mana) */
	else if (chance > 75)
	{
		if (chance >=79) chance = 75 + (chance-75)/4;
		else chance = 75;
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

	/* Non primary spellcasters never get better than 5 percent */
	if (!(cp_ptr->flags & CF_ZERO_FAIL))
	{
		if (minfail < 5) minfail = 5;
	}
	
	/* spellcasting bonus */
	if (p_ptr->timed[TMD_BRAIL])
    {
       chance -= 5;
       if (goodluck > 5) minfail -= randint((goodluck/6) + 1); /* - 1 to 4 */
       else if (goodluck > 0) minfail -= (randint(3) - 1); /* - 1 or 2 */
       else minfail -= (randint(2) - 1); /* - 0 or 1 */
    }
    
	/* Priest prayer penalty for "edged" weapons (before minfail) */
	/* was +25 fail */
	if (p_ptr->icky_wield)
	{
		if (goodweap > badweap) chance += (20 - (goodweap*3));
		else if (badweap > goodweap) chance += 25;
		else chance += 20;
	}
	
	/* effect of sentient weapons: */
	if (cp_ptr->spell_book == TV_PRAYER_BOOK)
	{
	   if ((badweap > 0) && (goodweap > 0))
	   {
          /* wielding same amount of bad and good objects */
	      if (magicmod == 10) chance += 2; 
	      /* more good objects than bad */
	      if (magicmod == 7) chance -= (goodweap-badweap); 
	   /* magicmod == 11 bad and good completely cancels out each other's effects */
	      /* more bad objects than good ones */
	      if ((magicmod == 8) && (!p_ptr->icky_wield)) chance += 4 + ((badweap-goodweap) * 2); 
	      if ((magicmod == 8) && (p_ptr->icky_wield)) chance += 1 + ((badweap-goodweap) * 2); 
          if ((magicmod == 13) && (!p_ptr->icky_wield)) chance += 6 + ((badweap-goodweap) * 2); 
       }
       /* wielding 1 bad object (icky_wield is enough penalty) */
	   else if ((badweap == 1) && (!p_ptr->icky_wield)) chance += 5;
	   else if ((goodweap == 1) && (magicmod != 12)) chance -= 4; /* wielding good object(s) */
	   else if ((badweap > 1) && (!p_ptr->icky_wield)) chance += 5 + (badweap*2);
	   else if ((badweap > 1) && (p_ptr->icky_wield)) chance += (badweap*2);
	   else if (goodweap > 1) chance -= (3 + (goodweap*2));
    }
	/* War mages are considered evil aligned as well as those who use */
	/*  the black magic realm. */
	else if ((cp_ptr->spell_book == TV_DARK_BOOK) ||
             ((cp_ptr->flags & CF_POWER_SHIELD) && (cp_ptr->spell_book == TV_MAGIC_BOOK)))
	{
	   if ((badweap > 0) && (goodweap > 0))
	   {
          /* wielding same amount of bad and good objects */
	      if (magicmod == 1) chance += 2;
	      /* more bad objects than good ones */
	      if (magicmod == 0) chance -= 3 + (badweap-goodweap) * 2;
	      /* more good objects than bad */
	      if (magicmod == 3) chance += 5 + ((goodweap-badweap) * 2);
       }
	   if (goodweap == 1) chance += 5; /* wielding good object */
	   if (badweap == 1) chance -= 5; /* wielding bad object */
	   if (goodweap > 1) chance += 5 + (goodweap*2);
	   if (badweap > 1) chance -= (4 + (badweap*2));
    }
    else /* neutral class */
    {
         if (badweap > 0) chance -= 1;
         if ((badweap > 1) && (goodweap < badweap)) chance -= 1;
         /* penalty for using conflicting sentient weapons */
         else if ((badweap > 0) && (goodweap > 0)) chance += 2 + goodweap + badweap;
    }

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder (after minfail) */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;
	
	/* used to be no chance of casting while confused */
    if (p_ptr->timed[TMD_CONFUSED])
	{
		chance += 25;
		/* minimum fail chance while confused (should it be 50% ?) */
		if (chance < 35) chance = 35;
	}
    
    /* don't take other penalties when using manafree */
    if (p_ptr->manafree) return (chance);

	/* other effects which make spells harder */
	if ((p_ptr->timed[TMD_SHERO]) || (p_ptr->timed[TMD_FRENZY]) ||
	   (p_ptr->timed[TMD_CHARM]) || (p_ptr->timed[TMD_AFRAID]))
    {
       chance += 4 + randint(6);
    }
	/* being held by a monster makes it hard to cast */
	if (p_ptr->timed[TMD_BEAR_HOLD]) chance += 10;

    /* 1st sight strengthens natural sight and inhibits unnatural stuff like magic */
	if (p_ptr->timed[TMD_2ND_THOUGHT]) chance += 10;

	/* Amnesia makes spells fail half the time */
	if (p_ptr->timed[TMD_AMNESIA]) chance *= 2;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}



/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 * When browsing a book, all legible spells are okay.
 */
bool spell_okay(int spell, bool known, bool browse)
{
	const magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Spell is illegible */
	if (s_ptr->slevel >= 99) return (FALSE);

	/* Spell is too hard */
	if (s_ptr->slevel > p_ptr->lev) return (browse);

	/* Spell is forgotten */
	if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
	{
		/* Never okay */
		return (browse);
	}

	/* Spell is learned */
	if (p_ptr->spell_flags[spell] & PY_SPELL_LEARNED)
	{
		/* Okay to cast or browse, not to study */
		return (known || browse);
	}

	/* Okay to study or browse, not to cast */
	return (!known || browse);
}

/* figure the mana cost for the mimmic wand spell */
static s16b mimmic_mana(void)
{
	s16b usemana, sval;
	int kidx;
	bool mimmicrod;
	object_kind *k_ptr;

	if (!p_ptr->mimmic)
	{
		/* set amount for default effect */
		usemana = 6;
	}
	else
	{
		/* sval of the wand to mimmic */
		if (p_ptr->mimmic < 100)
		{
			sval = p_ptr->mimmic;
			mimmicrod = FALSE;
		}
		/* or -100 if it's a rod */
		else
		{
			sval = p_ptr->mimmic - 100;
			mimmicrod = TRUE;
		}

		/* get the object kind (without having a real object) */
		if (mimmicrod) kidx = lookup_kind(TV_ROD, sval);
		else kidx = lookup_kind(TV_WAND, sval);
		k_ptr = &k_info[kidx];

		/* mana used depends on difficulty of wand or rod mimmicked */
		usemana = ((k_ptr->extra+1) / 3) + 4;
	}

	return usemana;
}


/*
 * Print a list of spells (for browsing or casting or viewing).
 */
void print_spells(const byte *spells, int num, int y, int x)
{
	int i, spell;
	s16b manacost;

	const magic_type *s_ptr;

	char help[20];
	char out_val[160];

	const char *comment = help;

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
		s_ptr = &mp_ptr->info[spell];

		/* Skip illegible spells */
		if (s_ptr->slevel >= 99)
		{
			strnfmt(out_val, sizeof(out_val), "  %c) %-30s", I2A(i), "(illegible)");
			c_prt(TERM_L_DARK, out_val, y + i + 1, x);
			continue;
		}

		/* Get extra info */
		get_spell_info(cp_ptr->spell_book, spell, help, sizeof(help));

		/* Assume spell is known and tried */
		comment = help;
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

		/* mimmic wand spell uses a variable amount of mana */
		if ((cp_ptr->spell_book == TV_CHEM_BOOK) && (spell == 44))
		{
			/* mana used depends on difficulty of wand or rod mimmicked */
			manacost = mimmic_mana();
		}
		else
		{
			manacost = s_ptr->smana;
		}

		/* Dump the spell --(-- */
		strnfmt(out_val, sizeof(out_val), "  %c) %-30s%2d %4d %3d%%%s",
		        I2A(i), get_spell_name(cp_ptr->spell_book, spell),
		        s_ptr->slevel, manacost, spell_chance(spell), comment);
		c_prt(line_attr, out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
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
 * The "prompt" should be "cast", "recite", "study", or "browse"
 * The "known" should be TRUE for cast/pray, FALSE for study
 * The "browse" should be TRUE for browse, FALSE for cast/pray/study
 */
static int get_spell(const object_type *o_ptr, cptr prompt, bool known, bool browse)
{
	int i;

	int spell;
	int num = 0;
	int result;

	byte spells[PY_MAX_SPELLS];

	bool verify;

	bool flag, redraw, okay;
	char choice;

	const magic_type *s_ptr;

	char out_val[160];

	cptr p = ((cp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");
    if (cp_ptr->spell_book == TV_CHEM_BOOK) p = "formula";


	/* Get the spell, if available */
	if (repeat_pull(&result))
	{
		/* Verify the spell */
		if (spell_okay(result, known, browse))
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
		if (spell_okay(spells[i], known, browse)) okay = TRUE;
	}

	/* No available spells */
	if (!okay) return (-2);


	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Hack -- when browsing a book, start with list shown */
	if ((browse) || (show_lists))
	{
		/* Show list */
		redraw = TRUE;

		/* Save screen */
		screen_save();

		/* Display a list of spells */
		print_spells(spells, num, 1, 20);
	}

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c%s, ESC=exit) %^s which %s? ",
	        p, I2A(0), I2A(num - 1), ((show_lists) ? "" : ", *=List"), prompt, p);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((!show_lists) &&
           ((choice == ' ') || (choice == '*') || (choice == '?')))
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
		if (!spell_okay(spell, known, browse))
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

	repeat_push(spell);

	/* Success */
	return (spell);
}



/*
 * View the detailed description for a selected spell.
 */
static void browse_spell(int spell)
{
	const magic_type *s_ptr;
    int idx;

	char out_val[160];
	char help[20];

	const char *comment = help;

	byte line_attr;


	/* Redirect output to the screen */
	text_out_hook = text_out_to_screen;

	/* Save the screen */
	screen_save();

	/* Get the magic and spell info */
	s_ptr = &mp_ptr->info[spell];

	/* Get extra info */
	get_spell_info(cp_ptr->spell_book, spell, help, sizeof(help));

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

	/* Show spell name and comment (if any) on first line of screen */
	if (streq(comment, ""))
	{
		strnfmt(out_val, sizeof(out_val), "%^s",
	    	    get_spell_name(cp_ptr->spell_book, spell));
	}
	else
	{
		strnfmt(out_val, sizeof(out_val), "%^s (%s)",
	    	    get_spell_name(cp_ptr->spell_book, spell),
	    	    /* Hack -- skip leading space */
	    	    ++comment);
	}

	/* Print, in colour */
	text_out_c(line_attr, out_val);

	/* Display the spell description */
	text_out("\n\n   ");

    /* allow for more than 2 realms */
    /* (code thanks to Pete Mack) */
    idx = spell;
    if (cp_ptr->spell_book == TV_PRAYER_BOOK) idx = idx + PY_MAX_SPELLS;
    else if (cp_ptr->spell_book == TV_NEWM_BOOK) idx = idx + (2 * PY_MAX_SPELLS);
    else if (cp_ptr->spell_book == TV_LUCK_BOOK) idx = idx + (3 * PY_MAX_SPELLS);
    else if (cp_ptr->spell_book == TV_CHEM_BOOK) idx = idx + (4 * PY_MAX_SPELLS);
    else if (cp_ptr->spell_book == TV_DARK_BOOK) idx = idx + (5 * PY_MAX_SPELLS);
/*    else if (cp_ptr->spell_book == TV_MIND_BOOK) idx = idx + (6 * PY_MAX_SPELLS); */
    text_out(s_text + s_info[idx].text);
    
/* replaced old code:
 *	text_out(s_text + s_info[(cp_ptr->spell_book == TV_MAGIC_BOOK) ? spell : spell + PY_MAX_SPELLS].text);
 */
	text_out_c(TERM_L_BLUE, "\n\n[Press any key to continue]\n");

	/* Wait for input */
	(void)inkey();

	/* Load screen */
	screen_load();
}


void do_cmd_browse_aux(const object_type *o_ptr)
{
	int spell;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Continue to browse spells until player hits ESC */
	while (1)
	{
		/* Ask for a spell */
		spell = get_spell(o_ptr, "browse", TRUE, TRUE);
		if (spell < 0) break;

		/* Browse the spell */
		browse_spell(spell);
	}
}


/*
 * Peruse the spells/prayers in a Book
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;


	/* Warriors are illiterate */
	if (!cp_ptr->spell_book)
	{
		msg_print("You cannot read books!");
		return;
	}

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

	/* Browse the book */
	do_cmd_browse_aux(o_ptr);
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, item;
	int spell;
	cptr q, s;
	object_type *o_ptr;

	cptr p = ((cp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");
    if (cp_ptr->spell_book == TV_CHEM_BOOK) p = "formula";

	if (!cp_ptr->spell_book)
	{
		msg_print("You cannot read books!");
		return;
	}

	if ((p_ptr->timed[TMD_BLIND]) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return;
	}

	if ((no_lite()) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return;
	}

	/* TMD_BRAIL */
    if ((p_ptr->timed[TMD_BLIND]) || (no_lite()))
	{
		msg_print("You read the spell with your hands..");
	}

	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}

	if (p_ptr->timed[TMD_TERROR])
	{
		msg_print("You are too desperate to escape to stop and read!");
		return;
	}

	if (p_ptr->new_spells < 1)
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


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Choose a spell if allowed */
	if (cp_ptr->flags & CF_CHOOSE_SPELLS)
	{
		/* Ask for a spell */
		spell = get_spell(o_ptr, "study", FALSE, FALSE);

		/* Allow cancel */
		if (spell == -1) return;
	}
	else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (i = 0; i < SPELLS_PER_BOOK; i++)
		{
			spell = get_spell_index(o_ptr, i);

			/* Skip empty spells */
			if (spell == -1) continue;

			/* Skip non "okay" spells */
			if (!spell_okay(spell, FALSE, FALSE)) continue;

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
		msg_format("You cannot learn any %ss in that book.", p);

		/* Abort */
		return;
	}

	/* learn these spells at the same time (see below) */
	if ((cp_ptr->spell_book == TV_CHEM_BOOK) && (spell == 43)) spell += 1;

	/* Take a turn */
	p_ptr->energy_use = 100;

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

	/* learn two spells at the same time with mimmic wand */
	/* because they're really two parts of the same spell */
	if ((cp_ptr->spell_book == TV_CHEM_BOOK) && (spell == 44)) 
	{
		/* Mark as forgotten (can only learn part of the spell at this time) */
		if (p_ptr->new_spells == 1)
		{
			p_ptr->spell_flags[spell-1] |= PY_SPELL_FORGOTTEN;

			/* (only learning one spell) */
			p_ptr->new_spells--;
		}
		/* Learn the mimmic wand (mimmic) spell */
		else
		{
			p_ptr->spell_flags[spell-1] |= PY_SPELL_LEARNED;

			/* two less spells available */
			p_ptr->new_spells -= 2;
		}

		/* Find the next open entry in "spell_order[]" */
		for (i = 0; i < PY_MAX_SPELLS; i++)
		{
			/* Stop at the first empty space */
			if (p_ptr->spell_order[i] == 99) break;
		}
	
		/* Add mimmic wand (mimmic) to the known list */
		p_ptr->spell_order[i] = (spell - 1);
	}
	else /* normal spell */
	{
		/* One less spell available */
		p_ptr->new_spells--;
	}

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

	/* Redraw object recall */
	p_ptr->window |= (PW_OBJECT);
}



/*
 * Cast a spell
 * void do_cmd_cast(void)
 */
bool do_cmd_cast(void)
{
	int item, spell;
	int chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Require spell ability */
	if (cp_ptr->spell_book != TV_MAGIC_BOOK)
	{
		msg_print("You cannot cast spells in this school!");
		return FALSE;
	}

	if ((p_ptr->timed[TMD_BLIND]) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if ((no_lite()) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	/* TMD_BRAIL */
    if ((p_ptr->timed[TMD_BLIND]) || (no_lite()))
	{
		msg_print("You read the spell with your hands..");
	}

    /* Not when desperate to escape */
	if (p_ptr->timed[TMD_TERROR])
	{
		msg_print("You are too desperate to escape to cast spells!");
		return FALSE;
	}

	/* Restrict choices to spell books */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

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
	spell = get_spell(o_ptr, "cast", TRUE, FALSE);

	if (spell < 0)
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return FALSE;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

    /* no dangerous spells when not using mana */
    if (p_ptr->manafree)
    {
       /* nothing */
    }

    /* Verify "dangerous" spells */
	else if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return FALSE;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);
	
	/* If you can use the staff, then you can cast the spell */
	/* as long as your failure rate is less than 86 */
    if ((p_ptr->manafree) && (chance < 86)) chance = 0;
	
	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");

		if (p_ptr->manafree) return FALSE;
	}

	/* Process spell */
	else
	{
		/* Cast the spell */
		if (!cast_spell(cp_ptr->spell_book, spell)) return FALSE;

		/* A spell was cast */
		sound(MSG_SPELL);
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

    /* no mana use with staff of manafree */
    if (p_ptr->manafree)
    {
       /* save the manacost so you know how many staff charges to use */
       p_ptr->manafree = s_ptr->smana;
    }

	/* Sufficient mana */
	else if (s_ptr->smana <= p_ptr->csp)
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
		(void)inc_timed(TMD_PARALYZED, randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 5 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	return TRUE;
}


/*
 * Pray a prayer
 */
bool do_cmd_pray(void)
{
	int item, spell, chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Must use prayer books */
	if (cp_ptr->spell_book != TV_PRAYER_BOOK)
	{
		msg_print("Pray hard enough and your prayers may be answered.");
		return FALSE;
	}

	if ((p_ptr->timed[TMD_BLIND]) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if ((no_lite()) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	/* TMD_BRAIL */
    if ((p_ptr->timed[TMD_BLIND]) || (no_lite()))
	{
		msg_print("You read the spell with your hands..");
	}

    /* Not when desperate to escape */
	if (p_ptr->timed[TMD_TERROR])
	{
		msg_print("You are too desperate to escape to stop and pray!");
		return FALSE;
	}

	/* Restrict choices */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no prayer books!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

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


	/* Choose a spell */
	spell = get_spell(o_ptr, "recite", TRUE, FALSE);

	if (spell < 0)
	{
		if (spell == -2) msg_print("You don't know any prayers in that book.");
		return FALSE;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

    /* no dangerous spells when not using mana */
    if (p_ptr->manafree)
    {
       /* nothing */
    }

	/* Verify "dangerous" prayers */
	else if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to recite this prayer.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return FALSE;
	}


	/* Spell failure chance (higher chance = more likely to fail) */
	chance = spell_chance(spell);

	/* If you can use the staff, then you can cast the spell */
	/* as long as your failure rate is less than 86 */
    if ((p_ptr->manafree) && (chance < 86)) chance = 0;
	
	/* Check for failure */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to concentrate hard enough!");

		if (p_ptr->manafree) return FALSE;
	}

	/* Success */
	else
	{
		/* Cast the spell */
		if (!cast_spell(cp_ptr->spell_book, spell)) return FALSE;

		/* A prayer was prayed */
		sound(MSG_PRAYER);
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

    /* no mana use with staff of manafree */
    if (p_ptr->manafree)
    {
       /* save the manacost so you know how many staff charges to use */
       p_ptr->manafree = s_ptr->smana;
    }

	/* Sufficient mana */
	else if (s_ptr->smana <= p_ptr->csp)
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
		(void)inc_timed(TMD_PARALYZED, randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 5 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	
	return TRUE;
}

/*
 * Cast a spell of nature magic (copied from previous function and tweaked)
 */
bool do_cmd_castnew(void)
{
	int item, spell, chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Must use correct books */
	if (cp_ptr->spell_book != TV_NEWM_BOOK)
	{
		msg_print("You cannot cast spells in this school!");
		return FALSE;
	}

	if ((p_ptr->timed[TMD_BLIND]) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if ((no_lite()) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	/* TMD_BRAIL */
    if ((p_ptr->timed[TMD_BLIND]) || (no_lite()))
	{
		msg_print("You read the spell with your hands..");
	}

    /* Not when desperate to escape */
	if (p_ptr->timed[TMD_TERROR])
	{
		msg_print("You are too desperate to escape to cast spells!");
		return FALSE;
	}


	/* Restrict choices */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no books of nature magic!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

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


	/* Choose a spell */
	spell = get_spell(o_ptr, "invoke", TRUE, FALSE);

	if (spell < 0)
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return FALSE;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

    /* no dangerous spells when not using mana */
    if (p_ptr->manafree)
    {
       /* nothing */
    }

	/* Verify "dangerous" spells */
	else if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return FALSE;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* If you can use the staff, then you can cast the spell */
	/* as long as your failure rate is less than 86 */
    if ((p_ptr->manafree) && (chance < 86)) chance = 0;
	
	/* Check for failure */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");

		if (p_ptr->manafree) return FALSE;
	}

	/* Success */
	else
	{
		/* Cast the spell */
		if (!cast_spell(cp_ptr->spell_book, spell)) return FALSE;

		/* A spell was cast */
		sound(MSG_SPELL);
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

    /* no mana use with staff of manafree */
    if (p_ptr->manafree)
    {
       /* save the manacost so you know how many staff charges to use */
       p_ptr->manafree = s_ptr->smana;
    }

	/* Sufficient mana */
	else if (s_ptr->smana <= p_ptr->csp)
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
		(void)inc_timed(TMD_PARALYZED, randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 5 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	
	return TRUE;
}

/*
 * Cast a spell of chance magic
 */
bool do_cmd_castluck(void)
{
	int item, spell, chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Must use correct books */
	if (cp_ptr->spell_book != TV_LUCK_BOOK)
	{
		msg_print("You cannot cast spells in this school!");
		return FALSE;
	}

	if ((p_ptr->timed[TMD_BLIND]) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if ((no_lite()) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	/* TMD_BRAIL */
    if ((p_ptr->timed[TMD_BLIND]) || (no_lite()))
	{
		msg_print("You read the spell with your hands..");
	}

    /* Not when desperate to escape */
	if (p_ptr->timed[TMD_TERROR])
	{
        if (randint(100) < goodluck*2)
        {
		   msg_print("You manage to stop long enough to cast quickly.");
        }
        else
        {        
           msg_print("You are too desperate to escape to cast spells!");
		   return FALSE;
        }
	}

	/* Restrict choices */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no books of new magic!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

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


	/* Choose a spell */
	spell = get_spell(o_ptr, "invoke", TRUE, FALSE);

	if (spell < 0)
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return FALSE;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

    /* no dangerous spells when not using mana */
    if (p_ptr->manafree)
    {
       /* nothing */
    }

	/* Verify "dangerous" spells */
	else if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return FALSE;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* If you can use the staff, then you can cast the spell */
	/* as long as your failure rate is less than 86 */
    if ((p_ptr->manafree) && (chance < 86)) chance = 0;
	
	/* Check for failure */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");

		if (p_ptr->manafree) return FALSE;
	}

	/* Success */
	else
	{
		/* Cast the spell */
		if (!cast_spell(cp_ptr->spell_book, spell)) return FALSE;

		/* A spell was cast */
		sound(MSG_SPELL);
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

    /* no mana use with staff of manafree */
    if (p_ptr->manafree)
    {
       /* save the manacost so you know how many staff charges to use */
       p_ptr->manafree = s_ptr->smana;
       /* msg_print("manafree"); */
    }

	/* Sufficient mana */
	else if (s_ptr->smana <= p_ptr->csp)
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
		(void)inc_timed(TMD_PARALYZED, randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 5 + randint(5), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	
	return TRUE;
}

/*
 * Mix (cast) an alchemy formula (copied from previous function and tweaked)
 */
bool do_cmd_castchem(void)
{
	int item, spell, chance;
	bool toohard = FALSE;
	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;


	/* Must use correct books */
	if (cp_ptr->spell_book != TV_CHEM_BOOK)
	{
		msg_print("You cannot cast spells in this school!");
		return FALSE;
	}

	if ((p_ptr->timed[TMD_BLIND]) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if ((no_lite()) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	/* TMD_BRAIL */
    if ((p_ptr->timed[TMD_BLIND]) || (no_lite()))
	{
		msg_print("You read the spell with your hands..");
	}

    /* Not when desperate to escape */
	if (p_ptr->timed[TMD_TERROR])
	{
		msg_print("You are too desperate to escape to mix up a formula!");
		return FALSE;
	}

	/* Restrict choices */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no alchemy books!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

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


	/* Choose a spell */
	spell = get_spell(o_ptr, "mix up", TRUE, FALSE);

	if (spell < 0)
	{
		if (spell == -2) msg_print("You don't know any formulas in that book.");
		return FALSE;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

    /* no dangerous spells when not using mana */
    if (p_ptr->manafree)
    {
       /* nothing */
    }

	/* Verify "dangerous" spells */
	else if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to mix up this formula.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return FALSE;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* If you can use the staff, then you can cast the spell */
	/* as long as your failure rate is less than 86 */
    if ((p_ptr->manafree) && (chance < 86)) chance = 0;
	
	/* Check for failure */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");

		if (p_ptr->manafree) return FALSE;
	}

	/* Success */
	else
	{
		/* Cast the spell */
		if (!cast_spell(cp_ptr->spell_book, spell)) return FALSE;

		/* A spell was cast */
		sound(MSG_SPELL);
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

    /* no mana use with staff of manafree */
    if (p_ptr->manafree)
    {
       /* save the manacost so you know how many staff charges to use */
       p_ptr->manafree = s_ptr->smana;
    }

	/* mimmic wand spell uses a variable amount of mana */
	else if ((cp_ptr->spell_book == TV_CHEM_BOOK) && (spell == 44))
	{
		s16b usemana;

		/* mana used depends on difficulty of wand or rod mimmicked */
		usemana = mimmic_mana();

		/* use the mana, or overexert if you don't have enough */
		if (s_ptr->smana <= p_ptr->csp) p_ptr->csp -= usemana;
		else toohard = TRUE;
	}

	/* Sufficient mana */
	else if (s_ptr->smana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= s_ptr->smana;
	}
	else
	{
		toohard = TRUE;
	}

	/* Over-exert the player */
	if (toohard)
	{
		int oops = s_ptr->smana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)inc_timed(TMD_PARALYZED, randint(2 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 2 + randint(3), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	
	return TRUE;
}

/*
 * Cast a spell of witchcraft
 */
bool do_cmd_castblack(void)
{
	int item, spell;
	int chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (cp_ptr->spell_book != TV_DARK_BOOK)
	{
		msg_print("You cannot cast spells in this school!");
		return FALSE;
	}

	if ((p_ptr->timed[TMD_BLIND]) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if ((no_lite()) && (!p_ptr->timed[TMD_BRAIL]))
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	/* TMD_BRAIL */
    if ((p_ptr->timed[TMD_BLIND]) || (no_lite()))
	{
		msg_print("You read the spell with your hands..");
	}

    /* Not when desperate to escape */
	if (p_ptr->timed[TMD_TERROR])
	{
		msg_print("You are too desperate to escape to cast spells!");
		return FALSE;
	}

	/* Restrict choices to spell books */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

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
	spell = get_spell(o_ptr, "cast", TRUE, FALSE);

	if (spell < 0)
	{
		if (spell == -2) msg_print("You don't know any spells in that book.");
		return FALSE;
	}


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

    /* no dangerous spells when not using mana */
    if (p_ptr->manafree)
    {
       /* nothing */
    }

	/* Verify "dangerous" spells */
	else if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return FALSE;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);
	
	/* If you can use the staff, then you can cast the spell */
	/* as long as your failure rate is less than 86 */
    if ((p_ptr->manafree) && (chance < 86)) chance = 0;
	
	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");

		if (p_ptr->manafree) return FALSE;
	}

	/* Process spell */
	else
	{
		/* Cast the spell */
		if (!cast_spell(cp_ptr->spell_book, spell)) return FALSE;

		/* A spell was cast */
		sound(MSG_SPELL);
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

    /* no mana use with staff of manafree */
    if (p_ptr->manafree)
    {
       /* save the manacost so you know how many staff charges to use */
       p_ptr->manafree = s_ptr->smana;
    }

	/* Sufficient mana */
	else if (s_ptr->smana <= p_ptr->csp)
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
		(void)inc_timed(TMD_PARALYZED, randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 26);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 6 + randint(9), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	
	return TRUE;
}
