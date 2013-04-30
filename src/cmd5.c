/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
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
int i,ii;

	int num = 0;

	int spell = 0;

	s16b book[26];

	bool verify;


	int okay = 0;

	bool flag, redraw;
	char choice;

	spell_type *s_ptr;
	spell_cast *sc_ptr = &(s_info[0].cast[0]);

	char out_val[160];

	cptr p;

	bool cast = FALSE;

	/* Spell */
	switch (o_ptr->tval)
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
			p="rune";
			cast = TRUE;
			break;

		default:
			p="power";
			break;

	}

	/* Cannot cast spells if illiterate */
	if ((cast) &&(c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL))
	{
		msg_print("You cannot read books.");

		return(-2);

	}

#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (!(cast) || (spell_okay(*sn, known)))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	/* Fill the book with spells */
	fill_book(o_ptr,book,&num);

	/* Assume no usable spells */
	okay = 0;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	if (cast) for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(book[i], known)) okay = TRUE;
	}
	/* Get a random spell */
	else if (!known)
	{
		/* Get a random spell */
		*sn = book[rand_int(num)];

#ifdef ALLOW_REPEAT
		repeat_push(*sn);
#endif /* ALLOW_REPEAT */

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

#if 0
	/* Show the list */
	if (redraw)
	{
		/* Save screen */
		screen_save();

		/* Display a list of spells */
		print_spells(book, num, 1, 20);
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
			else if (cast)
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of spells */
				print_spells(book, num, 1, 20);
			}
			
			/* Show the list */
			else
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of spells */
				print_powers(book, num, 1, 20);
			}

			/* Ask again */
			continue;
		}


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
				/* Get the spell details */
				for (ii=0;ii<MAX_SPELL_CASTERS;ii++)
				{
					if (s_ptr->cast[ii].class == p_ptr->pclass)
					{
						sc_ptr=&(s_ptr->cast[ii]);
					}
				}

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

/* Note this routine is simple, but horribly inefficient due 
   to the (1st iteration) design of the data structures */
bool inven_book_okay(const object_type *o_ptr)
{
	int i,ii,iii;

	spell_type *s_ptr;

	if ((o_ptr->tval != TV_MAGIC_BOOK) &&
  	  (o_ptr->tval != TV_PRAYER_BOOK) &&
  	  (o_ptr->tval != TV_RUNESTONE) &&
  	  (o_ptr->tval != TV_SONG_BOOK)) return (0);

	for (i=0;i<z_info->s_max;i++)
	{
		s_ptr=&s_info[i];

		for (ii=0;ii<MAX_SPELL_APPEARS;ii++)
		{
			if ((s_ptr->appears[ii].tval == o_ptr->tval) &&
			    (s_ptr->appears[ii].sval == o_ptr->sval))
			{
				for (iii=0;iii<MAX_SPELL_CASTERS;iii++)
				{

					if (s_ptr->cast[iii].class == p_ptr->pclass) return(1);
				}
			}
		}

	}			

	return (0);

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

	int num = 0;

	s16b book[26];

	object_type *o_ptr;

	cptr p, q, s;

	int spell=-1;

	int i;

	char choice = 0;

	char out_val[160];


	/* Cannot browse books if illiterate */
	if (c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL)
	{
		msg_print("You cannot read books.");

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

	item_tester_hook = inven_book_okay;

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

	/* Spell */
	switch (o_ptr->tval)
	{

		case TV_PRAYER_BOOK:
			p="prayer";
			break;

		case TV_SONG_BOOK:
			p="song";
			break;

		case TV_MAGIC_BOOK:
			p="spell";
			break;

		case TV_RUNESTONE:
			p="rune";
			break;

		default:
			p="power";
			break;
	}

	/* Fill book with spells */
	fill_book(o_ptr,book,&num);

	/* Paranoia */
	if (num == 0)
	{
		msg_format("There are no %ss to browse.",p);
		return;
	}

	/* Save screen */
	screen_save();

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
			int ii;
			bool legible = FALSE;

			spell_type *s_ptr;

			spell_cast *sc_ptr = &(s_info[0].cast[0]);

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

			/* Get casting information */
			for (ii=0;ii<MAX_SPELL_CASTERS;ii++)
			{
				if (s_ptr->cast[ii].class == p_ptr->pclass)
				{
					legible = TRUE;
					sc_ptr=&(s_ptr->cast[ii]);
				}
			}

			/* Spell is illegible */
			if (!legible)
			{
				msg_print("You cannot read that spell.");

				/* Build a prompt (accept all spells) */
				strnfmt(out_val, 78, "(%^ss %c-%c, ESC=exit) Browse which %s? ",
					p, I2A(0), I2A(num - 1), p);

			}
			else
			{

				/* Set text_out hook */
				text_out_hook = text_out_to_screen;

				/* Recall spell */
				spell_desc(&s_info[spell],"When cast, it ",spell_power(spell), TRUE, 1);

				text_out(".");

				/* Build a prompt (accept all spells) */
				strnfmt(out_val, 78, "The %s of %s. (%c-%c, ESC) Browse which %s:",
					p, s_name + s_info[spell].name,I2A(0), I2A(num - 1), p);
			}

			continue;
		}

	}

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
	int i, item;

	int spell = -1;

	cptr p, q, s;

	object_type *o_ptr;

	spell_type *s_ptr;

	int max_spells = PY_MAX_SPELLS;

	if (!variant_study_more) max_spells = 64;

	/* Cannot cast spells if illiterate */
	if (c_info[p_ptr->pclass].spell_first > 50)
	{
		msg_print("You cannot read books.");

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
		msg_format("You cannot learn anything new!");
		return;
	}


	/* Restrict choices to "useful" books */
	item_tester_hook = inven_book_okay;

	/* Get an item */
	q = "Study which book? ";
	s = "You have no books that you can read.";
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


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	switch (o_ptr->tval)
	{
		case TV_PRAYER_BOOK:
		p="prayer";
		break;

		case TV_SONG_BOOK:
		p = "song";
		break;

		case TV_MAGIC_BOOK:
		p = "spell";
		break;

		case TV_RUNESTONE:
		p = "rune";
		break;

		default:
		p="power";
		break;

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

	/* Magic book -- Learn a selected spell */
	/* Now only for basic magic books */
	else if (o_ptr->tval == TV_MAGIC_BOOK)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", o_ptr, FALSE) && (spell == -1)) return;
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


bool inven_cast_okay(const object_type *o_ptr)
{
int i,ii;

	spell_type *s_ptr;

	if ((o_ptr->tval != TV_MAGIC_BOOK) &&
	    (o_ptr->tval != TV_PRAYER_BOOK) &&
	    (o_ptr->tval != TV_RUNESTONE) &&
	    (o_ptr->tval != TV_SONG_BOOK)) return (0);

	for (i=0;i<PY_MAX_SPELLS;i++)
	{

		if (p_ptr->spell_order[i] == 0) continue;

		s_ptr=&s_info[p_ptr->spell_order[i]];

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
 * Cast a spell (once chosen)
 */
void do_cmd_cast_aux(int spell, int plev, cptr p, cptr t)
{
	int i;
	int chance;

	spell_type *s_ptr;
	spell_cast *sc_ptr = &(s_info[0].cast[0]);


	/* Get the spell */
	s_ptr = &s_info[spell];

	/* Get the spell details */
	for (i=0;i<MAX_SPELL_CASTERS;i++)
	{
		if (s_ptr->cast[i].class == p_ptr->pclass)
		{
			sc_ptr=&(s_ptr->cast[i]);
		}


	}

	/* Verify "dangerous" spells */
	if (sc_ptr->mana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.",p,t);

		/* Verify */
		if (!get_check("Attempt it anyway? "))
		{
			if (p_ptr->held_song)
			{
				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);			

				p_ptr->held_song = 0;
			}

			return;
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
			return;
		}
	}

	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Some rooms only give a (slight) chance */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		/* Special rooms affect some of this */
		int by = p_ptr->py/BLOCK_HGT;
		int bx = p_ptr->px/BLOCK_HGT;

		/* Get the room */
		if(room_info[dun_room[by][bx]].flags & (ROOM_SILENT))
		{
			chance = 99;

			/* Warn the player */
			msg_print("You are engulfed in magical silence.");

		}
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

		/* Apply the spell effect */
		process_spell(spell,plev,&abort);

		/* Did we cancel? */
		if (abort) return;

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
			int e = sc_ptr->level;

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

			/* Gain experience */
			gain_exp(e * sc_ptr->level);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Sufficient mana */
	if (sc_ptr->mana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= sc_ptr->mana;
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
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int item,spell;

	object_type *o_ptr;

	cptr p, t;

	cptr q, s;


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
	if (c_info[p_ptr->pclass].spell_first > 50)
	{
		msg_print("You cannot read books.");
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

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Cast, recite, sing or play */
	switch (o_ptr->tval)
	{

		case TV_MAGIC_BOOK:
		{
			p="cast";
			t="spell";
			
			break;
		}
		case TV_RUNESTONE:
		{
			p="apply";
			t="rune";
			
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
		if (spell == -2) msg_format("You don't know any %ss in that book.",t);
		return;
	}

	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

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
	if (p_ptr->held_song != spell) do_cmd_cast_aux(spell,spell_power(spell),p,t);
	
}

