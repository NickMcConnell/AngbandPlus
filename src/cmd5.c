/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 *
 *
 * James E. Wilson and Robert A. Koeneke released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
 * or under the terms of the traditional Angband license. 
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 * or under the terms of the traditional Angband license. 
 */ 



#include "angband.h"


extern void do_cmd_rerate(void);
extern void corruption_shuffle(void);
extern bool item_tester_hook_armour(object_type *o_ptr);

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
int get_spell(int *sn, cptr prompt, int use_realm , int sval, bool known )
{
	int		i;
	int		spell = -1;
	int		num = 0;
	int		ask;

	byte		spells[64];

	bool		flag, redraw, okay;
	char		choice;

	char		out_val[160];
    
    magic_type   s_magic;
    magic_type  *s_ptr = &s_magic;

	cptr p = ((mp_ptr->spell_book == TV_MIRACLES_BOOK) ? "prayer" : "spell");

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn)) {

		/* Verify the spell */
		if (spell_okay(*sn, known, use_realm - 1)) {

			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */


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

	/* No redraw yet */
	redraw = FALSE;

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
				print_spells(spells, num, 1, 15, use_realm-1);
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
			get_extended_spell_info( use_realm-1  , spell%32 , s_ptr );
			
			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, s_ptr->name,
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

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

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
void do_cmd_browse(int item)
{
	int		sval;
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
	/* Restrict choices to "useful" books */
	item_tester_tval = (byte)mp_ptr->spell_book;

	/* Get an item if we do not already have one */
	if(item < 0)
	{
		/* Get an item (from inven or floor) */
		if (!get_item(&item, "Browse which book? ","You have no books that you can read.", USE_FLOOR | USE_INVEN))
		{
			return;
		}
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

	item_tester_tval = (byte)mp_ptr->spell_book;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("You can't read that.");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

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
	print_spells(spells, num, 1, 13, (o_ptr->tval-TV_MIRACLES_BOOK));

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str("[(Browsing) Choose a spell or press Escape ]", 0, 13);
	
	/* Spoil the spells*/
	spoil_spells( o_ptr );

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

	object_type             *o_ptr;

	if (!p_ptr->realm1)
	{
		msg_print("You cannot read books!");
		return;
	}

	if (p_ptr->blind)
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
	item_tester_tval = (byte)mp_ptr->spell_book;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Study which book? ", "You have no books that you can read." , USE_INVEN ))
	{
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
	if (mp_ptr->spell_book != TV_MIRACLES_BOOK)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", o_ptr->tval-TV_MIRACLES_BOOK+1 , sval, (bool)FALSE)
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
		p, spells
		[(increment?(p_ptr->realm2)-1:(p_ptr->realm1)-1)][spell%32].name);

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
	p_ptr->window |= (PW_SPELL);
}

void do_poly_wounds(void)
{

	s16b wounds = p_ptr->cut, hit_p = (p_ptr->mhp - p_ptr->chp);
	s16b change = damroll(p_ptr->lev, 5);
	bool Nasty_effect = (randint(5)==1);

	if (!(wounds || hit_p || Nasty_effect)) return;

	if (Nasty_effect)
	{
		msg_print("A new wound was created!");
		take_hit(change, "a polymorphed wound");
		set_timed_effect( TIMED_CUT, change);
	}
	else
	{
		msg_print("Your wounds are polymorphed into less serious ones.");
		hp_player(change);
		set_timed_effect( TIMED_CUT, (p_ptr->cut)-(change/2));
	}
}

void do_poly_self(void)
{
	int effects = randint(2);
	int tmp = 0;
	int new_race;
	int more_effects = TRUE;
	char buf[1024];

	msg_print("You feel a change coming over you...");

	while (effects-- && more_effects)
	{
		switch (randint(12))
		{
		case 1: case 2:
			do_poly_wounds();
			break;
		case 3: case 4:
			(void) gain_corruption(0);
			break;
		case 5: case 6: case 7: /* Racial polymorph! Uh oh... */
			{
				do { new_race = randint(COUNT_RACES) -1; } while (new_race == p_ptr->prace);

				msg_format("You turn into a%s %s!",
					((new_race == ELF
					|| new_race == IMP)?"n":""),
					race_info[new_race].title);

				p_ptr->prace = new_race;
				/* rp_ptr = &race_info[p_ptr->prace]; */
				p_race = race_info[p_ptr->prace];

				/* Access the "race" pref file */
				sprintf(buf, "%s.prf", rp_ptr->title);

				/* Process that file */
				process_pref_file(buf);

				/* Access the "font" or "graf" pref file, based on "use_graphics" */
				sprintf(buf, "%s-%s.prf", (use_graphics ? "graf" : "font"), ANGBAND_SYS);

				/* Process that file */
				process_pref_file(buf);

				/* Experience factor */
				p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

				/* Calculate the height/weight for males */
				if (p_ptr->psex == SEX_MALE)
				{
					p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
					p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
				}

				/* Calculate the height/weight for females */
				else if (p_ptr->psex == SEX_FEMALE)
				{
					p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
					p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
				}

				check_experience();
				p_ptr->max_plv = p_ptr->lev;

				p_ptr->redraw |= (PR_BASIC);

				p_ptr->update |= (PU_BONUS);

				handle_stuff();
			}
			lite_spot(py, px);
			more_effects = FALSE; /* Stop here! */
			break;
		case 8: /* Purposedly "leaks" into default */
			msg_print("You polymorph into an abomination!");
			while (tmp < 6)
			{
				(void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
				tmp++;
			}
			if (randint(6)==1)
			{
				msg_print("You find living difficult in your present form!");
				take_hit(damroll(randint(p_ptr->lev),p_ptr->lev), "a lethal corruption");
			}
			/* No break; here! */
		default:
			corruption_shuffle();
		}
	}
}



/*
* Brand the current weapon
*/

/*
* Pray a prayer -- Unused in Hellband
*/
void do_cmd_pray(void)
{
	msg_print
		("Praying is not used in Hellband. Use magic spell casting instead.");
}


void mindcraft_info(char *p, int power)
{
	int plev = p_ptr->lev;

	strcpy(p, "");

	switch (power) {
	 case 0:  break;
	 case 1:  sprintf(p, " dam %dd%d", 3 + ((plev - 1) / 4), 3 + plev/15); break;
	 case 2:  sprintf(p, " range %d", (plev < 25 ? 10 : plev + 2)); break;
	 case 3:  sprintf(p, " range %d", plev * 5);  break;
	 case 4:  break;
	 case 5:  sprintf(p, " dam %dd8", 8+((plev-5)/4));  break;
	 case 6:  sprintf(p, " dur %d", plev);  break;
	 case 7:  break;
	 case 8:  sprintf(p, " dam %d", plev * ((plev-5) / 10 + 1)); break;
	 case 9:  sprintf(p, " dur 11-%d", plev + plev/2);  break;
	 case 10: sprintf(p, " dam %dd6", plev/2);  break;
	 case 11: sprintf(p, " dam %d", plev * (plev > 39 ? 4: 3)); break;
	}
}


/*
* Allow user to choose a orphic power.
*
* If a valid spell is chosen, saves it in '*sn' and returns TRUE
* If the user hits escape, returns FALSE, and set '*sn' to -1
* If there are no legal choices, returns FALSE, and sets '*sn' to -2
*
* The "prompt" should be "cast", "recite", or "study"
* The "known" should be TRUE for cast/pray, FALSE for study
*
* nb: This function has a (trivial) display bug which will be obvious
* when you run it. It's probably easy to fix but I haven't tried,
* sorry.
*/
static int get_mindcraft_power(int *sn)
{
	int                     i;

	int                     num = 0;
	int y = 1;
	int x = 20;
	int minfail = 0;

	int  plev = p_ptr->lev;
	int chance = 0;

	bool            flag, redraw;
	int             ask;
	char            choice;

	mindcraft_power spell;

	char            out_val[160];
	char            comment[80];

	cptr p = "power";

	/* Assume cancelled */
	*sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn)) {

		/* Verify the spell */
		if (mindcraft_powers[*sn].min_lev <= plev) {

			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */


	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
		if (mindcraft_powers[i].min_lev <= plev)
			num++;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
		p, I2A(0), I2A(num - 1), p);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				char psi_desc[80];

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				/* Display a list of spells */
				prt("", y, x);
				put_str("Name", y, x + 5);
				put_str("Lv Mana Fail Info", y, x + 35);

				/* Dump the spells */
				for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
				{
					/* Access the spell */
					spell = mindcraft_powers[i];
					if (spell.min_lev > plev)   break;

					chance = spell.fail;
					/* Reduce failure rate by "effective" level adjustment */
					chance -= 3 * (p_ptr->lev - spell.min_lev);

					/* Reduce failure rate by INT/WIS adjustment */
					chance -= 3 * (adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_INTWIS] - 1);

					/* Not enough mana to cast */
					if (spell.mana_cost > p_ptr->csp)
					{
						chance += 5 * (spell.mana_cost - p_ptr->csp);
					}

					/* Extract the minimum failure rate */
					minfail = adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_FAILURE];

					/* Minimum failure rate */
					if (chance < minfail) chance = minfail;

					/* Stunning makes spells harder */
					if (p_ptr->stun > 50) chance += 25;
					else if (p_ptr->stun) chance += 15;

					/* Always a 5 percent chance of working */
					if (chance > 95) chance = 95;

					/* Get info */
					mindcraft_info(comment, i);

					/* Dump the spell --(-- */
					sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
						I2A(i), spell.name,
						spell.min_lev, spell.mana_cost, chance, comment);
					prt(psi_desc, y + i + 1, x);
				}

				/* Clear the bottom line */
				prt("", y + i + 1, x);
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
		spell = mindcraft_powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", mindcraft_powers[i].name);

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
	(*sn) = i;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


/*
* do_cmd_cast calls this function if the player's class
* is 'orphic'.
*/
void do_cmd_mindcraft(void)
{
	int   n = 0,  b = 0;
	int chance;
	int dir;
	int minfail = 0;
	int plev = p_ptr->lev;

	mindcraft_power   spell;

	/* not if confused */
	if (p_ptr->confused) {
		msg_print("You are too confused!");
		return;
	}

	/* get power */
	if (!get_mindcraft_power(&n))  return;

	spell = mindcraft_powers[n];

	/* Verify "dangerous" spells */
	if (spell.mana_cost > p_ptr->csp) {
		/* Warning */
		msg_print("You do not have enough mana to use this power.");
		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell.fail;
	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - spell.min_lev);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_INTWIS] - 1);

	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_FAILURE];

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_format("You failed to concentrate hard enough!");

		if (randint(100) < (chance/2)) {    /* Backfire */
			b = randint(100);
			if (b < 5)   {
				msg_print("Oh, no! Your mind has gone blank!");
				lose_all_info();
			} else if (b < 15) {
				msg_print("Weird visions seem to dance before your eyes...");
				set_timed_effect( TIMED_IMAGE , p_ptr->image + 5 + randint(10));
			} else if (b < 45) {
				msg_print("Your brain is addled!");
				set_timed_effect( TIMED_CONFUSED , p_ptr->confused + randint(8));
			} else if (b < 90) {
				set_timed_effect( TIMED_STUN, p_ptr->stun + randint(8));
			} else {   /* Mana storm */
				msg_print("Your mind unleashes its power in an uncontrollable storm!");
				project(1, 2+plev/10, py, px,
					plev * 2, GF_MANA,PROJECT_JUMP|PROJECT_KILL|PROJECT_GRID|PROJECT_ITEM);
				p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev/10));
			}
		}
	}  else  {
		/* spell code */
		switch (n) {
		 case 0:   /* Precog */
			 if (plev > 44)
				 wiz_lite();
			 else if (plev > 19)
				 map_area();

			 if (plev < 30) {
				 b = detect_monsters_normal();
				 if (plev > 14)  b |=  detect_monsters_invis();
				 if (plev > 4)   b |=  detect_traps();
			 } else {
				 b = detect_all();
			 }

			 if ((plev > 24) && (plev < 40))
				 set_timed_effect( TIMED_ESP, p_ptr->tim_esp + plev);

			 if (!b)  msg_print("You feel safe.");
			 break;
		 case 1:   /* Mindblast */
			 if (!get_aim_dir(&dir)) return;
			 if (randint(100) < plev * 2)
				 fire_beam(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3+plev/15)));
			 else
				 fire_ball(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3+plev/15)), 0);
			 break;
		 case 2:   /* Minor displace */
			 if (plev < 25) {
				 teleport_player(10);
			 } else {
				if (!spell_dimensional_gate( plev )) return;
			 }
			 break;
		 case 3:   /* Major displace */
			 teleport_player(plev * 5);
			 if (plev > 29)
				 banish_monsters(plev);
			 break;
		 case 4:   /* Domination */
			 if (plev < 30) {
				 if (!get_aim_dir(&dir)) return;
				 fire_ball(GF_DOMINATION, dir, plev, 0);
			 } else {
				 charm_monsters(p_ptr->lev * 2);
			 }
			 break;
		 case 5:   /* Fist of Force  ---  not 'true' TK  */
			 if (!get_aim_dir(&dir)) return;
			 fire_ball(GF_SOUND, dir, damroll(8+((plev-5)/4), 8),
				 (plev > 20 ? (plev-20)/8 + 1 : 0));
			 break;
		 case 6:   /* Character Armour */
			 set_timed_effect( TIMED_SHIELD, p_ptr->shield + plev);
			 if (plev > 14)   set_timed_effect( TIMED_OPPOSE_ACID, p_ptr->oppose_acid + plev);
			 if (plev > 19)   set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + plev);
			 if (plev > 24)   set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + plev);
			 if (plev > 29)   set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + plev);
			 if (plev > 34)   set_timed_effect( TIMED_OPPOSE_POIS, p_ptr->oppose_pois + plev);
			 break;
		 case 7:   /* Psychometry */
			 if (plev < 40)
				 psychometry();
			 else
				 ident_spell();
			 break;
		 case 8:   /* Mindwave */
			 msg_print("Mind-warping forces emanate from your brain!");
			 if (plev < 25)
				 project(0, 2+plev/10, py, px,
				 (plev*3)/2, GF_PSI, PROJECT_KILL);
			 else
				 (void)mindblast_monsters(plev * ((plev-5) / 10 + 1));
			 break;
		 case 9:   /* Adrenaline */
			 set_timed_effect( TIMED_AFRAID , 0);
			 set_timed_effect( TIMED_STUN, 0);
			 hp_player(plev);
			 b = 10 + randint((plev*3)/2);
			 if (plev < 35)
				 set_timed_effect( TIMED_HERO, p_ptr->hero + b);
			 else
				 set_timed_effect( TIMED_SHERO, p_ptr->shero + b);

			 if (!p_ptr->fast)	{   /* Haste */
				 (void)set_timed_effect( TIMED_FAST, b);
			 } else {
				 (void)set_timed_effect( TIMED_FAST, p_ptr->fast + b);
			 }
			 break;
		 case 10:   /* Psychic Drain */
			 if (!get_aim_dir(&dir)) return;
			 b = damroll(plev/2, 6);
			 if (fire_ball(GF_PSI_DRAIN, dir, b,  0 +
				 (plev-25)/10))
				 p_ptr->energy -= (s16b)randint(150);
			 break;
		 case 11:   /* Telekinesis */
			 msg_print("A wave of pure physical force radiates out from your body!");
			 project(0, 3+plev/10, py, px,
				 plev * (plev > 39 ? 4 : 3), GF_TELEKINESIS, PROJECT_KILL|PROJECT_ITEM|PROJECT_GRID);
			 break;
		 default:
			 msg_print("Zap?");
		}
	}

	/* Take a turn */
	energy_use = 100-(5*(p_ptr->lev-spell.min_lev));
	if(energy_use < 10) energy_use = 10;

	/* Sufficient mana */
	if (spell.mana_cost <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell.mana_cost;
	}

	/* Over-exert the player */
	else
	{
		int oops = spell.mana_cost - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage WIS (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your mind!");

			/* Reduce constitution */
			(void)dec_stat(A_WIS, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);

}


