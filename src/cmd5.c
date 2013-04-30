/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
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
 * The "known" should be TRUE for cast/pray/sing, FALSE for study if
 * a book (eg magic book, prayer book, song book, runestone).
 * For all others, "known" should be TRUE if the player is to be
 * prompted to select an object power, or FALSE if a random power is
 * to be chosen.
 *
 * Now also allows scrolls, potions etc. We do not require mana or
 * ability to read to use any of these.
 */
int get_spell(int *sn, cptr prompt, object_type *o_ptr, bool known)
{
	int i;

	int num = 0;

	int spell = 0;

	int tval;

	s16b book[26];

	bool verify;


	int okay = 0;

	bool flag, redraw;
	key_event ke;

	spell_type *s_ptr;
	spell_cast *sc_ptr;

	char out_val[160];

	cptr p;

	bool cast = FALSE;

	/* Get fake tval */
	if (o_ptr->tval == TV_STUDY) tval = o_ptr->sval;
	else tval = o_ptr->tval;

	/* Spell */
	switch (tval)
	{
		case TV_PRAYER_BOOK:
			p="prayer";
			cast = TRUE;
			break;

		case TV_SONG_BOOK:
			p="song";
			cast = TRUE;
			break;

		case TV_MAGIC_BOOK:
			p="spell";
			cast = TRUE;
			break;

		case TV_RUNESTONE:
			p="pattern";
			cast = TRUE;
			break;

		default:
			p="power";
			break;
	}

	/* Cannot cast spells if illiterate */
	if ((cast) &&(c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL)
		&& (p_ptr->pstyle != WS_MAGIC_BOOK) && (p_ptr->pstyle != WS_PRAYER_BOOK) && (p_ptr->pstyle != WS_SONG_BOOK))
	{
		msg_print("You cannot read books or runestones.");

		return(-2);

	}

	/* Hack -- coated objects use coating instead */
	if (coated_p(o_ptr))
	{
		object_type object_type_body;
		object_type *j_ptr = &object_type_body;

		j_ptr->tval = o_ptr->xtra1;
		j_ptr->sval = o_ptr->xtra2;
		j_ptr->k_idx = lookup_kind(o_ptr->xtra1, o_ptr->xtra2);
		o_ptr = j_ptr;
	}

#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (!(cast) || (spell_okay(*sn, known)))
		{
			/* Success only if known */
			/* This is required to allow wands of wonder to repeat differently every time */
			if (known) return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	/* Fill the book with spells */
	fill_book(o_ptr,book,&num); 

	/* Assume no usable spells */
	okay = 0;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells if casting */
	if (cast) for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(book[i], known)) okay = TRUE;
	}

	/* Get a random spell/only one choice */
	else if ((!known) || (num == 1))
	{
		/* Get a random spell */
		*sn = book[rand_int(num)];

		/* Something happened */
		return (TRUE);
	}
	else okay = TRUE;

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Option -- automatically show lists */
	if (auto_display_lists)
	{
		/* Show list */
		redraw = TRUE;

		/* Save screen */
		screen_save();

		/* Display a list of spells */
		if (cast) print_spells(book, num, 1, 20);
		else print_powers(book, num, 1, 20);
	}

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
	p, I2A(0), I2A(num - 1), prompt, p);

	/* Get a spell from the user */
	while (!flag && get_com_ex(out_val, &ke))
	{
		char choice;

		if (ke.key == '\xff')
		{
			if (ke.mousebutton)
			{
				if (redraw) ke.key = 'a' + ke.mousey - 2;
				else ke.key = ' ';
			}
			else continue;
		}

		/* Request redraw */
		if ((ke.key == ' ') || (ke.key == '*') || (ke.key == '?'))
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
				if (cast) print_spells(book, num, 1, 20);
				else print_powers(book, num, 1, 20);
			}

			/* Ask again */
			continue;
		}

		choice = ke.key;

		/* Note verify */
		verify = (isupper(choice) ? TRUE : FALSE);

		/* Lowercase 1+*/
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
		spell = book[i];

		/* Require "okay" spells */
		if ((cast) && (!spell_okay(spell, known)))
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
			s_ptr = &s_info[spell];

			if (cast)
			{
				/* Get the casting details */
				sc_ptr = spell_cast_details(spell);
				
				/* Paranoia */
				if (!sc_ptr) return (FALSE);

				/* Prompt */
				strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, s_name + s_ptr->name,
				sc_ptr->mana, spell_chance(spell));
			}
			else
			{
				/* Prompt */
				strnfmt(tmp_val, 78, "%^s %s)? ",
				prompt, s_name + s_ptr->name);
			}

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
 * Is the book okay to select?
 * 
 * This requires that the player has spells in it that
 * they can 'read'.
 */
bool inven_book_okay(const object_type *o_ptr)
{
	if ((o_ptr->tval != TV_MAGIC_BOOK) &&
  	  (o_ptr->tval != TV_PRAYER_BOOK) &&
  	  (o_ptr->tval != TV_RUNESTONE) &&
  	  (o_ptr->tval != TV_SONG_BOOK) &&
	  (o_ptr->tval != TV_STUDY)) return (0);

	/* Study notes */
	if (o_ptr->tval == TV_STUDY)
	{
		return (spell_legible(o_ptr->pval));
	}

	/* Book / runestone */
	else
	{
		s16b book[26];
		int num;
		int i;
		
		fill_book(o_ptr, book, &num);
		
		for (i=0; i < num; i++)
		{
			if (spell_legible(book[i])) return (TRUE);
		}
	}			

	return (FALSE);
}


/*
 * Print a list of fields (for research).
 */
void print_fields(const s16b *sn, int num, int y, int x)
{
	int i;

	char out_val[160];
#if 0
	/* Title the list */
	prt("", y, x);
#endif
	/* Dump the fields */
	for (i = 0; i < num; i++)
	{
		/* Dump the spell -- skip 'of ' if required */
		sprintf(out_val, "  %c) %-75s ",
			I2A(i), k_name + k_info[sn[i]].name + (k_info[sn[i]].tval == TV_RUNESTONE ? 0 : 3));
		c_prt(TERM_WHITE, out_val, y + i, x);
	}

	/* Clear the bottom line */
	prt("", y + i, x);
}


/*
 * Persuse the spells/prayers in a Book.
 * 
 * Takes an object as a parameter
 */
bool do_cmd_browse_object(object_type *o_ptr)
{
	int num = 0;

	s16b book[26];

	cptr p, r;

	int spell=-1;

	int i;

	int tval;

	char choice = 0;

	char out_val[160];

	spell_type *s_ptr;

	object_type object_type_body;

	/* Get fake tval */
	if (o_ptr->tval == TV_STUDY) tval = o_ptr->sval;
	else tval = o_ptr->tval;

	/* Spell */
	switch (tval)
	{
		case TV_PRAYER_BOOK:
			p="prayer";
			r="Pray for which blessing";
			break;

		case TV_SONG_BOOK:
			p="song";
			r="Improvise which melody";
			break;

		case TV_MAGIC_BOOK:
			p="spell";
			r="Research which field";
			break;

		case TV_RUNESTONE:
			p="pattern";
			r="Engrave which pattern";
			break;

		default:
			p="power";
			r = "";
			break;
	}

	/* Study materials -- Browse spells in a book related to the current spell for magic books and runestones only */
	if ((o_ptr->tval == TV_STUDY) && ((o_ptr->sval == TV_MAGIC_BOOK) || (o_ptr->sval == TV_RUNESTONE)))
	{
		s16b field[MAX_SPELL_APPEARS];

		int num = 0;

		int selection = 0;

		/* Get the spell */
		s_ptr = &s_info[o_ptr->pval];

		/* Pick a new spell item */
		for (i = 0; i < MAX_SPELL_APPEARS; i++)
		{
			if (s_ptr->appears[i].tval == tval) field[num++] = lookup_kind(tval, s_ptr->appears[i].sval);
		}

		/* Paranoia */
		if (!num) return (FALSE);

		/* Display the list and get a selection */
		if (get_list(print_fields, field, num, format("%^ss",p), r, 1, 20, &selection))
		{
			/* Fake the o_ptr */
			o_ptr = &object_type_body;

			/* Set object details required */
			o_ptr->k_idx = selection;
			o_ptr->tval = k_info[selection].tval;
			o_ptr->sval = k_info[selection].sval;
			o_ptr->xtra1 = 0;
		}
		/* Did not choose something */
		else
		{
			return (FALSE);
		}
	}

	/* Fill book with spells */
	fill_book(o_ptr,book,&num);

	/* Paranoia */
	if (num == 0)
	{
		msg_format("There are no %ss to browse.",p);
		return (FALSE);
	}

	/* 'School' specialists cannot learn spells from basic 'school' books other than their school */
	if ((p_ptr->psval >= SV_BOOK_MAX_GOOD) && (o_ptr->sval >= SV_BOOK_MAX_GOOD))
	{
		/* Sval hackery */
		if (o_ptr->sval - (o_ptr->sval % SV_BOOK_SCHOOL) + SV_BOOK_SCHOOL - 1 != p_ptr->psval)
		{
			msg_format("You cannot read that %s.",p);
			switch(o_ptr->sval % 4)
			{
				case 0: msg_print("It shows a lack of grasp of simple theory."); break;
				case 1: msg_print("It could never work due to harmonic instability."); break;
				case 2: msg_print("It's the deranged scribblings from a lunatic asylum."); break;
				case 3: msg_print("The book snaps itself shut and jumps from your fingers."); break;
			}

			return (FALSE);
		}
	}
	
	/* Display the spells */
	print_spells(book, num, 1, 20);

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, ESC=exit) Browse which %s? ",
		p, I2A(0), I2A(num - 1), p);

	/* Get a spell from the user */
	while ((choice != ESCAPE) && get_com(out_val, &choice))
	{
		/* Lowercase 1+*/
		choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		if ((i >= 0) && (i < num))
		{
			spell_type *s_ptr;
			
			/* Save the spell index */
			spell = book[i];

			/* Load screen */
			screen_load();

			/* Save screen */
			screen_save();

			/* Display the spells */
			print_spells(book, num, 1, 20);

			/* Begin recall */
			Term_gotoxy(0, 1);

			/* Get the spell */
			s_ptr = &s_info[spell];
			
			/* Spell is illegible */
			if (!spell_legible(spell))
			{
				msg_format("You cannot read that %s.",p);
				
				/* Build a prompt (accept all spells) */
				strnfmt(out_val, 78, "(%^ss %c-%c, ESC=exit) Browse which %s? ",
					p, I2A(0), I2A(num - 1), p);
			}
			else
			{
				bool intro = FALSE;

				/* Set text_out hook */
				text_out_hook = text_out_to_screen;

				/* Hack -- 'wonder' spells */
				if (s_info[book[i]].type == SPELL_USE_OBJECT)
				{
					object_type object_type_body;
					object_type *j_ptr = &object_type_body;
						
					s16b book2[26];

					int num2, j;
					
					bool powers = FALSE;
						
					/* Prepare fake object */
					object_prep(j_ptr, s_info[book[i]].param);
						
					/* Fill the book */
					fill_book(j_ptr, book2, &num2);
						
					for (j = 0; j < num2; j++)
					{
						/* List powers */
						powers |= spell_desc(&s_info[book2[j]],(j==0) ? "When cast, it " : ", or ",spell_power(spell),TRUE, j);	
					}
				}

				/* Recall spell */
				if (spell_desc(&s_info[spell],"When cast, it ",spell_power(spell), TRUE, 1))
				{
					int y, x;

					/* End the sentence */
					text_out(".  ");

					/* Clear until the end of the line */
					/* XXX XXX Not sure why we have to do this.*/ 

					/* Obtain the cursor */
					(void)Term_locate(&x, &y);

					/* Clear line, move cursor */
					Term_erase(x, y, 255);
				}

				/* Display pre-requisites, unless specialist */
				if (!spell_match_style(spell))
				  for (i = 0; i < MAX_SPELL_PREREQUISITES; i++)
				  {
				    /* Check if pre-requisite spells */
				    if (s_info[spell].preq[i])
				    {
				      if (!intro) text_out_c(TERM_VIOLET,"You must learn ");
				      else text_out_c(TERM_VIOLET, " or ");

				      intro = TRUE;

				      text_out_c(TERM_VIOLET, s_name + s_info[s_info[spell].preq[i]].name);
				    }

				  }

				/* Terminate if required */
				if (intro) text_out_c(TERM_VIOLET, format(" before studying this %s.\n",p));

				/* Build a prompt (accept all spells) */
				strnfmt(out_val, 78, "The %s of %s. (%c-%c, ESC) Browse which %s:",
					p, s_name + s_info[spell].name,I2A(0), I2A(num - 1), p);
			}

			continue;
		}
	}
	
	return (TRUE);
}



/*
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while berserk or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int item, sval;

	object_type *o_ptr;

	cptr q, s;

	/* Cannot browse books if illiterate */
	if ((c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL)
		&& (p_ptr->pstyle != WS_MAGIC_BOOK) && (p_ptr->pstyle != WS_PRAYER_BOOK) && (p_ptr->pstyle != WS_SONG_BOOK))
	{
		msg_print("You cannot read books or runestones.");

		return;
	}

#if 0

	/* No lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Berserk */
	if (p_ptr->shero)
	{
		msg_print("You are too enraged!");
		return;
	}

	/* Amnesia */
	if (p_ptr->amnesia)
	{
		msg_print("You have forgotten all your spells!");
		return;
	}
#endif

	item_tester_hook = inven_book_okay;

	/* Get an item */
	q = "Browse which book or runestone? ";
	s = "You have no books or runestones that you can read.";
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

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Get the item's sval */
	sval = o_ptr->sval;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();
	
	/* Save screen */
	screen_save();

	/* Browse the object */
	if (do_cmd_browse_object(o_ptr))
	{
		/* Prompt for a command */
		put_str("(Browsing) Command: ", 0, 0);
	
		/* Hack -- Get a new command */
		p_ptr->command_new = inkey_ex();
	}
	/* Hack -- we shouldn't need this here. TODO */
	else if (easy_more)
	{
		msg_print(NULL);
		
		messages_easy(TRUE);
	}

	/* Load screen */
	screen_load();

	/* Hack -- Process "Escape" */
	if (p_ptr->command_new.key == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new.key = 0;
	}
}



/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, item;

	int spell = -1;

	cptr p, q, r, s;

	cptr u = " book";

	object_type *o_ptr;

	spell_type *s_ptr;

	int tval;

	int max_spells = PY_MAX_SPELLS;

	object_type object_type_body;

	bool study_item = FALSE;
	bool disdain = FALSE;

	/* Cannot cast spells if illiterate */
	if ((c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL)
		&& (p_ptr->pstyle != WS_MAGIC_BOOK) && (p_ptr->pstyle != WS_PRAYER_BOOK) && (p_ptr->pstyle != WS_SONG_BOOK))
	{
		msg_print("You cannot read books or runestones.");

		return;
	}

	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	if (p_ptr->shero)
	{
		msg_print("You are too enraged!");
		return;
	}

	/* Amnesia */
	if (p_ptr->amnesia)
	{
		msg_print("You have forgotten how to read!");
		return;
	}

	/* Cannot learn more spells */
	if (!(p_ptr->new_spells))
	{
		msg_format("You cannot learn anything more yet.");
		return;
	}

	/* Message if needed */
	else
	{
		/* Hack */
		p_ptr->old_spells = 0;

		/* Message */
		calc_spells();
	}

	/* Restrict choices to "useful" books */
	item_tester_hook = inven_book_okay;

	/* Get an item */
	q = "Study which book or runestone? ";
	s = "You have no books or runestones that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATU))) return;

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

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Generate study item from features */
	if (o_ptr->ident & (IDENT_STORE)) study_item = TRUE;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Get fake tval */
	if (o_ptr->tval == TV_STUDY)
	{
		tval = o_ptr->sval;
		u = "";
	}
	else tval = o_ptr->tval;

	/* Spell */
	switch (tval)
	{
		case TV_PRAYER_BOOK:
			p="prayer";
			r="Pray for which blessing";
			break;

		case TV_SONG_BOOK:
			p="song";
			r="Improvise which melody";
			break;

		case TV_MAGIC_BOOK:
			p="spell";
			r="Research which field";
			break;

		case TV_RUNESTONE:
			p="pattern";
			r="Engrave which pattern";
			u=" combination of runes";
			break;

		default:
			p="power";
			r = "";
			break;
	}

	/* Study materials -- Choose spells in a book related to the current spell */
	if (o_ptr->tval == TV_STUDY)
	{
		s16b field[MAX_SPELL_APPEARS];

		int num = 0;

		int selection = 0;

		/* Get the spell */
		s_ptr = &s_info[o_ptr->pval];

		/* Pick a new spell item */
		for (i = 0; i < MAX_SPELL_APPEARS; i++)
		{
			if (s_ptr->appears[i].tval == tval) field[num++] = lookup_kind(tval, s_ptr->appears[i].sval);
		}

		/* Paranoia */
		if (!num) return;

		/* Display the list and get a selection */
		if (get_list(print_fields, field, num, format("%^ss",p), r, 1, 20, &selection))
		{
			/* Fake the o_ptr */
			o_ptr = &object_type_body;

			/* Set object details required */
			o_ptr->k_idx = selection;
			o_ptr->tval = k_info[selection].tval;
			o_ptr->sval = k_info[selection].sval;
			o_ptr->xtra1 = 0;
		}
		/* Did not choose something */
		else
		{
			return;
		}
	}

	/* 'School' specialists cannot learn spells from basic 'school' books other than their school */
	if ((p_ptr->psval >= SV_BOOK_MAX_GOOD) && (o_ptr->sval >= SV_BOOK_MAX_GOOD))
	{
		/* Sval hackery */
		if (o_ptr->sval - (o_ptr->sval % SV_BOOK_SCHOOL) + SV_BOOK_SCHOOL - 1 != p_ptr->psval) disdain = TRUE;
	}
	
	/* Spell is illegible */
	if (disdain)
	{
		msg_format("You cannot study that %s.",p);

		switch(o_ptr->sval % 4)
		{
			case 0: msg_print("You lack the grasp of simple theory."); break;
			case 1: msg_print("You can't master it due to harmonic instability."); break;
			case 2: msg_print(format("Your %ss resemble deranged scribblings from a lunatic asylum.", p)); break;
			case 3: msg_print("The book burns red hot in your hands!"); break;
		}
		
		msg_print("You pass out from the strain!");
	
		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(o_ptr->sval % 4 + 1));
		
		return;
	}
	
	/* Prayer book -- Learn a random prayer */
	if (o_ptr->tval == TV_PRAYER_BOOK)
	{
		int ii;

		int k = 0;

		int gift = -1;

		for (i=0;i<z_info->s_max;i++)
		{
			s_ptr=&s_info[i];

			for (ii=0;ii<MAX_SPELL_APPEARS;ii++)
			{
				if ((s_ptr->appears[ii].tval == o_ptr->tval) &&
			    	(s_ptr->appears[ii].sval == o_ptr->sval) &&
				(spell_okay(i,FALSE)))
				{
					if ((++k == 1) || ((k > 1) &&
						(rand_int(k) ==0)))
					{
						gift = i;
					}
				}
			}
		}

		/* Accept gift */
		spell = gift;
	}

	/* Song book -- Learn a spell in order */
	else if (o_ptr->tval == TV_SONG_BOOK)
	{
		s16b book[26];

		int num = 0;

		int graft = -1;

		/* Fill the book with spells */
		fill_book(o_ptr,book,&num);

		/* Do the hard work */
		for(i=0;i<num;i++)
		{
			if (spell_okay(book[i],FALSE))
			{
				graft = book[i];
				break;
			}

		}

		/* Accept graft */
		spell = graft;
	}

	/* Magic book / runestone -- Learn a selected spell */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) || (o_ptr->tval == TV_RUNESTONE))
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", o_ptr, FALSE) && (spell == -1)) return;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msg_format("You cannot learn any %ss in that%s.", p, u);

		/* Abort */
		return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i] == 0) break;
	}

	/* Paranoia */
	if (i >= max_spells)
	{
		/* Message */
		msg_format("You cannot learn any more %ss.", p);

		return;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i] = spell;

	/* Learn the spell */
	if (i < 32)
	{
		p_ptr->spell_learned1 |= (1L << i);
	}
	else if (i < 64)
	{
		p_ptr->spell_learned2 |= (1L << (i - 32));
	}
	else if (i < 96)
	{
		p_ptr->spell_learned3 |= (1L << (i - 64));
	}
	else
	{
		p_ptr->spell_learned4 |= (1L << (i - 96));
	}

	/*Set to spell*/
	s_ptr = &(s_info[spell]);

	/* Mention the result */
	message_format(MSG_STUDY, 0, "You have learned the %s of %s.",
	   p, s_name + s_ptr->name);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Create study item if required */
	if (study_item)
	{
		/* Create a new study object */
		o_ptr = &object_type_body;

		/* Prepare object */
		object_prep(o_ptr,lookup_kind(TV_STUDY, tval));

		/* Set the spell */
		o_ptr->pval = spell;

		/* And carry it */
		item = inven_carry(o_ptr);
	}

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);

	/* Redraw object recall */
	p_ptr->window |= (PW_OBJECT);

}




bool inven_cast_okay(const object_type *o_ptr)
{
	int i,ii;

	spell_type *s_ptr;

	if ((o_ptr->tval != TV_MAGIC_BOOK) &&
	    (o_ptr->tval != TV_PRAYER_BOOK) &&
	    (o_ptr->tval != TV_RUNESTONE) &&
  	  (o_ptr->tval != TV_SONG_BOOK) &&
	  (o_ptr->tval != TV_STUDY)) return (0);

	/* Research materials */
	if (o_ptr->tval == TV_STUDY)
	{
		for (i=0;i<PY_MAX_SPELLS;i++)
		{
			if (p_ptr->spell_order[i] == o_ptr->pval) return(1);
		}
	}

	/* Book / runestone */
	else for (i=0;i<PY_MAX_SPELLS;i++)
	{
		if (p_ptr->spell_order[i] == 0) continue;

		s_ptr=&s_info[p_ptr->spell_order[i]];

		/* Book / runestone */
		for (ii=0;ii<MAX_SPELL_APPEARS;ii++)
		{
			if ((s_ptr->appears[ii].tval == o_ptr->tval) &&
			    (s_ptr->appears[ii].sval == o_ptr->sval))

			{
				return(1);
			}
		}
	}

	return (0);

}


/*
 * Cast a spell (once chosen); return FALSE if aborted
 */
bool do_cmd_cast_aux(int spell, int plev, cptr p, cptr t)
{
	int i;
	int chance;

	spell_type *s_ptr;
	spell_cast *sc_ptr;


	/* Get the spell */
	s_ptr = &s_info[spell];

	/* Get the cost */
	sc_ptr = spell_cast_details(spell);
	
	/* Paranoia */
	if (!sc_ptr) return (FALSE);

	/* Verify "dangerous" spells */
	if (sc_ptr->mana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.",p,t);
		
		/* No constitution to drain */
		if (!p_ptr->stat_ind[A_CON]) return FALSE;

		/* Verify */
		if (!get_check("Attempt it anyway? "))
		{
			if (p_ptr->held_song)
			{
				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);			

				p_ptr->held_song = 0;
			}

			return FALSE;
		}
	}

	/* Verify "warning" spells */
	else if ((verify_mana) &&
	 ((p_ptr->csp - sc_ptr->mana) < (p_ptr->msp * op_ptr->hitpoint_warn) / 10))
	{
		/* Warning */
		msg_format("You have limited mana to %s this %s.",p,t);

		/* Verify */
		if (!get_check("Attempt it anyway? "))
		{
			if (p_ptr->held_song)
			{
				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);			

				p_ptr->held_song = 0;
			}
			return FALSE;
		}
	}

	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Some items and some rooms silence the player */
	if ((p_ptr->cur_flags4 & (TR4_SILENT)) || (room_has_flag(p_ptr->py, p_ptr->px, ROOM_SILENT)))
	{
		/* Some items silence the player */
		chance = 99;

		/* Warn the player */
		msg_print("You are engulfed in magical silence.");

		/* Get the room */
		if (!(room_has_flag(p_ptr->py, p_ptr->px, ROOM_SILENT)))
		{
			/* Always notice */
			equip_can_flags(0x0L,0x0L,0x0L,TR4_SILENT);
		}
	}
	else
	{
		/* Always notice */
		equip_not_flags(0x0L,0x0L,0x0L,TR4_SILENT);
	}

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_format("You failed to %s the %s!",p,t);

		if (p_ptr->held_song)
		{
			/* Redraw the state */
			p_ptr->redraw |= (PR_STATE);			

			p_ptr->held_song = 0;
		}
	}

	/* Process spell */
	else
	{
		/* Must be true to let us abort */
		bool abort = TRUE;

		/* Always true */
		bool known = TRUE;

		/* Apply the spell effect */
		process_spell(SOURCE_PLAYER_CAST, spell, spell,plev,&abort,&known);

		/* Did we cancel? */
		if (abort) return FALSE;

		for (i=0;i<PY_MAX_SPELLS;i++)
		{
			if (p_ptr->spell_order[i] == spell) break;
		}

		/* Paranoia */
		if (i==PY_MAX_SPELLS) ;

		/* A spell was cast */ 
		else if (!((i < 32) ?
		      (p_ptr->spell_worked1 & (1L << i)) :
		      ((i < 64) ? (p_ptr->spell_worked2 & (1L << (i - 32))) :
		      ((i < 96) ? (p_ptr->spell_worked3 & (1L << (i - 64))) :
		      (p_ptr->spell_worked4 & (1L << (i - 96)))))))
		{
			/* The spell worked */
			if (i < 32)
			{
				p_ptr->spell_worked1 |= (1L << i);
			}
			else if (i < 64)
			{
				p_ptr->spell_worked2 |= (1L << (i - 32));
			}
			else if (i < 96)
			{
				p_ptr->spell_worked3 |= (1L << (i - 64));
			}
			else if (i < 128)
			{
				p_ptr->spell_worked2 |= (1L << (i - 96));
			}

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Sufficient mana */
	if (sc_ptr->mana <= p_ptr->csp)
	{
		/* Over-exert the player if casting in the 'red-zone' of their reserve.
		 * Note that if this occurs, the spell does not cost any mana.
		 */
		if ((p_ptr->reserves) && (p_ptr->csp < adj_con_reserve[p_ptr->stat_ind[A_CON]] / 2) &&
			(rand_int(100) < adj_con_reserve[p_ptr->stat_ind[A_CON]]) && (p_ptr->stat_ind[A_CON]))
		{
			/* Temporarily weaken the player */
			if (!p_ptr->stat_dec_tim[A_CON])
			{
				set_stat_dec_tim(rand_int(20) + 20, A_CON);
			}
			
			/* Weaken the player */
			else
			{
				/* Message */
				msg_print("You have damaged your health!");

				/* Reduce constitution */
				(void)dec_stat(A_CON, 15 + randint(10));
			}
		}
		else
		{
			/* Use some mana */
			p_ptr->csp -= sc_ptr->mana;

			/* Need reserves */
			if (!(p_ptr->reserves) && (p_ptr->csp < adj_con_reserve[p_ptr->stat_ind[A_CON]] / 2))
			{
				/* Give some mana */
				msg_print("You draw on your reserves.");
			
				p_ptr->reserves = TRUE;

				p_ptr->csp = (p_ptr->csp + adj_con_reserve[p_ptr->stat_ind[A_CON]]) / 2;
				
				/* Update mana */
				p_ptr->update |= (PU_MANA);
			}			
		}
	}

	/* Over-exert the player */
	else
	{
		int oops = sc_ptr->mana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage CON */
		if (rand_int(100) < 50)
		{
			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint(10));
			
			/* Add to the temporary drain */
			set_stat_dec_tim(p_ptr->stat_dec_tim[A_CON] + rand_int(20) + 20, A_CON);	
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	return TRUE;
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int item, spell, tval;

	object_type *o_ptr;

	cptr p, t;

	cptr q, s;

	cptr u = " book";

	/* Check if we are holding a song */
	if (p_ptr->held_song)
	{
		/* Verify */
		if (!get_check(format("Continue singing %s?", s_name + s_info[p_ptr->held_song].name)))
		{
			/* Redraw the state */
			p_ptr->redraw |= (PR_STATE);			

			p_ptr->held_song = 0;
		}
	}

	/* Cannot cast spells if illiterate */
	if ((c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL)
		&& (p_ptr->pstyle != WS_MAGIC_BOOK) && (p_ptr->pstyle != WS_PRAYER_BOOK) && (p_ptr->pstyle != WS_SONG_BOOK))
	{
		msg_print("You cannot read books or runestones.");
		return;
	}


	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Not when berserk */
	if (p_ptr->shero)
	{
		msg_print("You are too enraged!");
		return;
	}

	/* Amnesia */
	if (p_ptr->amnesia)
	{
		msg_print("You have forgotten how to read!");
		return;
	}

	/* Restrict choices to spells we can cast */
	item_tester_hook = inven_cast_okay;

	/* Get an item */
	q = "Use which book? ";
	s = "You have nothing you have studied!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATU))) return;

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

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Get fake tval */
	if (o_ptr->tval == TV_STUDY)
	{
		tval = o_ptr->sval;
		u = "";
	}
	else tval = o_ptr->tval;

	/* Cast, recite, sing or play */
	switch (tval)
	{
		case TV_MAGIC_BOOK:
		{
			p="cast";
			t="spell";
			
			break;
		}
		case TV_RUNESTONE:
		{
			p="draw";
			t="pattern";
			u=" combination of runes";
			
			break;
		}
		case TV_PRAYER_BOOK:
		{
       			p="recite";
			t="prayer";
			
			break;
		}
		case TV_SONG_BOOK:
		{
			if (p_ptr->pstyle == WS_INSTRUMENT)
			{
				p="play";
			}
			else
			{
				p="sing";
			}

			t = "song";
			break;
		}

		default:
		{
			p="use";
			t="power";
			break;		
		}
	}

	/* Ask for a spell */
	if (!get_spell(&spell, p, o_ptr, TRUE))
	{
		if (spell == -2) msg_format("You don't know any %ss in that%s.",t,u);
		return;
	}

	/* Hold a song if possible */
	if (s_info[spell].flags3 & (SF3_HOLD_SONG))
	{
		int i;

		for (i = 0;i< z_info->w_max;i++)
		{
			if (w_info[i].class != p_ptr->pclass) continue;

			if (w_info[i].level > p_ptr->lev) continue;

			if (w_info[i].benefit != WB_HOLD_SONG) continue;

			/* Check for styles */       
			if ((w_info[i].styles==0) || (w_info[i].styles & (p_ptr->cur_style & (1L << p_ptr->pstyle))))
			{
				/* Verify */
				if (get_check(format("Continue singing %s?", s_name + s_info[spell].name))) p_ptr->held_song = spell;
			}

			/* Hack - Cancel searching */
			/* Stop searching */
			if (p_ptr->searching)
			{
				/* Clear the searching flag */
				p_ptr->searching = FALSE;

				/* Clear the last disturb */
				p_ptr->last_disturb = turn;
			}

			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Redraw the state */
			p_ptr->redraw |= (PR_STATE);

		}
	}

	/* Cast the spell - held songs get cast later*/
	if (p_ptr->held_song != spell)
		if (do_cmd_cast_aux(spell,spell_power(spell),p,t))
			/* If not aborted, take a turn */
			p_ptr->energy_use = 100;
}

