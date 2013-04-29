/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001 Andrew Doull. Modifications to the Angband 2.9.1
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
 * The "known" should be TRUE for cast/pray/sing, FALSE for study
 */
static int get_spell(int *sn, cptr prompt, object_type *o_ptr, bool known)
{
        int i,ii;

	int num = 0;

        int spell = 0;

	byte book[26];

	bool verify;

	bool flag, redraw, okay;
	char choice;

        spell_type *s_ptr;
        spell_cast *sc_ptr = &(s_info[0].cast[0]);

	char out_val[160];

        cptr p;


        /* Spell */
        switch (o_ptr->tval)
        {

                case TV_PRAYER_BOOK:
                        p="prayer";
                break;

                case TV_SONG_BOOK:
                        p="song";
                break;

                default:
                        p="spell";
                break;

        }


	/* Cannot cast spells if illiterate */
	if (c_info[p_ptr->pclass].sp_lvl > PY_MAX_LEVEL)
	{
		msg_print("You cannot read books.");

		return(-2);

	}


#ifdef ALLOW_REPEAT

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	/* Fill the book with spells */
	fill_book(o_ptr,book,&num);

	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(book[i], known)) okay = TRUE;
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
			else
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of spells */
				print_spells(book, num, 1, 20);
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
			s_ptr = &s_info[spell];

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


bool inven_book_okay(object_type *o_ptr)
{
/* Note this routine is simple, but horribly inefficient due 
   to the (1st iteration) design of the data structures */


	int i,ii,iii;

	spell_type *s_ptr;

        if ((o_ptr->tval != TV_MAGIC_BOOK) &&
            (o_ptr->tval != TV_PRAYER_BOOK) &&
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

	byte book[26];

	object_type *o_ptr;

	cptr q, s;

	/* Cannot browse books if illiterate */
	if (c_info[p_ptr->pclass].sp_lvl > PY_MAX_LEVEL)
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

	/* Fill book with spells */
	fill_book(o_ptr,book,&num);

	/* Save screen */
	screen_save();

	/* Display the spells */
        print_spells(book, num, 1, 20);

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

	/* Cannot cast spells if illiterate */
	if (c_info[p_ptr->pclass].sp_lvl > 50)
	{
		msg_print("You cannot read books.");
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

        switch (o_ptr->tval)
        {
                case TV_PRAYER_BOOK:
                        p="prayer";
                break;

                case TV_SONG_BOOK:
                        p = "song";
                break;

                default:
                        p="spell";
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
				(spell_okay(i,FALSE)) &&
				(++k > 1) &&
				(rand_int(k) ==0))
					{
						gift = i;
					}

			}

		}


		/* Accept gift */
		spell = gift;
	}

	/* Song book -- Learn a spell in order */
        else if (o_ptr->tval == TV_SONG_BOOK)
	{
		byte book[26];

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

	/* Add the spell to the known list */
	p_ptr->spell_order[i] = spell;

	/* Learn the spell */
	if (i < 32)
	{
		p_ptr->spell_learned1 |= (1L << i);
	}
	else
	{
		p_ptr->spell_learned2 |= (1L << (i - 32));
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


bool inven_cast_okay(object_type *o_ptr)
{
        int i,ii;

	spell_type *s_ptr;

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
 * Brand the current weapon
 */
static void brand_weapon(void)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	/* you can never modify non-weapons */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr))&&
	    ((o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		cptr act;

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
 * Bless the current weapon
 */
static void bless_weapon(void)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	/* you can never modify non-weapons */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr)) &&
	    ((o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		char o_name[80];

                o_ptr->name2 = EGO_BLESS_BLADE;

                /* Give special ability */
                o_ptr->xtra2 = (byte)rand_int(OBJECT_XTRA_SIZE_POWER);

		object_desc(o_name, o_ptr, FALSE, 0);

                msg_format("Your %s is blessed by the power of your god!", o_name);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();
		msg_print("The Blessing failed.");
	}
}


/*
 * Brand the current armor with resistance
 */
static void brand_armor(void)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_BODY];


	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	/* you can never modify non-armor */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr)) &&
            ((o_ptr->tval == TV_HARD_ARMOR) || (o_ptr->tval == TV_SOFT_ARMOR)))
	{
		cptr act;

		char o_name[80];

		if (rand_int(100) < 25)
		{
			act = "is covered in a fiery shield!";
			o_ptr->name2 = EGO_RESIST_FIRE;
		}
		else if (rand_int(100) < 50)
		{
			act = "glows deep, icy blue!";
			o_ptr->name2 = EGO_RESIST_COLD;
		} 
		else if (rand_int(100) < 75)
		{
			act = "drips with acid!";
			o_ptr->name2 = EGO_RESIST_ACID;
		} 
		else
		{
			act = "crackles with energy!";
			o_ptr->name2 = EGO_RESIST_ELEC;
		} 

		object_desc(o_name, o_ptr, FALSE, 0);

		msg_format("Your %s %s", o_name, act);

                enchant(o_ptr, rand_int(3) + 4, ENCH_TOAC);
	}

	else
	{
		if (flush_failure) flush();
		msg_print("The Branding failed.");
	}
}


/*
 * Create a (wielded) spell item
 */
static void wield_spell(int item, int sval, int time)
{

	object_type *i_ptr;
	object_type object_type_body;

        cptr act;

        char o_name[80];

	/* Get the wield slot */
        object_type *o_ptr = &inventory[item];


	/* Get local object */
	i_ptr = &object_type_body;

	/* Create the spell */
	object_prep(i_ptr, lookup_kind(TV_SPELL, sval));
	i_ptr->pval = time;
	object_aware(i_ptr);
	object_known(i_ptr);

	/* Take off existing item */
	if (o_ptr->k_idx)
	{

		/* Check if same spell */
		if ((o_ptr->tval == TV_SPELL) && (o_ptr->sval))
		{
			/* Reset duration */
			if (o_ptr->pval < time) o_ptr->pval = time;

			/* And done */
			return;

		}

		/* Take off existing item */
                (void)inven_takeoff(item, 255);
	}

	/* 'Wear' the spell */
	object_copy(o_ptr,i_ptr);

	/* Increment the equip counter by hand */
	p_ptr->equip_cnt++;

	/* Where is the item now */
        if (item == INVEN_WIELD)
	{
		act = "You are wielding";
	}
        else if (item == INVEN_BOW)
	{
		act = "You are shooting with";
	}
        else if (item == INVEN_LITE)
	{
		act = "Your light source is";
	}
	else
	{
		act = "You are wearing";
	}

	/* Describe the result */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
        msg_format("%s %s (%c).", act, o_name, index_to_label(item));

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

}


/*
 * Enchant item --- a big pile of (fun) hack
 *
 * Basically we select an adjacent item of the same tval,
 * compare the difference in levels between the two, and 
 * if randint(plev) > difference, polymorph the item.
 * Otherwise scan until we encounter a 0 value item
 * of the same tval and polymorph the item into that, or
 * generate some junk (50% chance of each).
 * We need to generate junk or caster could infinitely
 * polymorph their items until getting exceedingly good
 * stuff.
 * Note it is dangerous to allow enchant scroll...
 */
static void enchant_item(byte tval, int plev)
{

	object_type *o_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	cptr q,s;
	int dirs,kind,i;

        int item;

        /* Get the local item */
        i_ptr = &object_type_body;

	/* Set strings */
	switch (tval)
	{

		case TV_POTION:
			q = "Enchant which potion?";
			s = "You have no potions to enchant.";
			break;
		case TV_ROD:
                        q = "Enchant which rod?";
			s = "You have no rods to enchant.";
			break;
		case TV_WAND:
                        q = "Enchant which wand?";
			s = "You have no wands to enchant.";
			break;
		case TV_STAFF:
                        q = "Enchant which staff?";
			s = "You have no staves to enchant.";
			break;
		case TV_FOOD:
                        q = "Enchant which food?";
			s = "You have no food to enchant.";
			break;
                default:
                        q = "Enchant which item?";
                        s = "You have no items to enchant.";
	}

	/* Restrict choices */
	item_tester_tval = tval;

	/* Get an item */
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

	/* Start with same kind */
        i = o_ptr->k_idx;

	/* Randomly increase or decrease */
	if (rand_int(100)<50)
	{
		if (k_info[i-1].tval == tval) dirs = -1;
		else dirs = 1;
	}
	else
	{
		if (k_info[i-1].tval == tval) dirs = 1;
		else dirs = -1;
	}

	/* Check for improvement */
        if (randint(plev) > (k_info[i+dirs].level - k_info[i].level))
	{
		/*Success*/
		kind = i+dirs;
	}
        else if (randint(100) < 50)
	{
		/* Pick bad failure */
		while ((k_info[i].cost) && (k_info[i].tval == tval))
		{
			/* Keep trying */
			i+=dirs;
		}

		/* No bad items downwards */
		if (k_info[i].tval != tval)
		{
			/* Try other direction */
                        i = o_ptr->k_idx;
                        dirs = -dirs;

			/* Pick bad failure */
			while ((k_info[i].cost) && (k_info[i].tval == tval))
			{
				/* Keep trying */
				i+=dirs;
			}
		}
					
		/* No bad items at all */
		if (k_info[i].tval !=tval)
		{
                        i = o_ptr->k_idx;
		}

                /* No change */
		kind = i;
	}
        else
        {

        /* Pick some junk*/
                switch (tval)
                {
                        case TV_POTION:
                        {
                                kind = lookup_kind(TV_BOTTLE,1);
                                break;
                        }
                /* XXX Really need some junk for each of these */
                case TV_WAND:
                case TV_STAFF:
		case TV_ROD:
                        {
                                kind = lookup_kind(TV_JUNK,6);
                                break;
                        }
                /* XXX Odd junk, but can't be food */
		case TV_FOOD:
                        {
                                kind = lookup_kind(TV_JUNK,3);
                                break;
                        }
                /* XXX Pick meaningless junk */
                default:
                        {
                                kind = lookup_kind(TV_JUNK,3);
                                break;
                        }
                }
        }
        

	/* Ta da! */
	msg_print("There is a blinding flash!");

	/* Destroy an item in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a potion on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	/* Create the new object */
	object_prep(i_ptr, kind);

	/* Carry the object */
        item = inven_carry(i_ptr);

        /* Describe the new object */
        inven_item_describe(item);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

}

/*
 * Create gold
 */
static void create_gold(void)
{
	object_type *i_ptr;
	object_type object_type_body;

        int base;

	/* Get local object */
	i_ptr = &object_type_body;

        /* Create the gold */
        object_prep(i_ptr, lookup_kind(TV_GOLD, 10));

	/* Hack -- Base coin cost */
        base = k_info[i_ptr->k_idx].cost;

	/* Determine how much the treasure is "worth" */
        i_ptr->pval = (base + (8L * randint(base)) + randint(8));

        /* Floor carries the item */
        drop_near(i_ptr, 0, p_ptr->py, p_ptr->px);

        /* XXX To do thesis on inflation in Angband economies. */
}

/*
 * Cast a spell (once chosen)
 */
void do_cmd_cast_aux(int spell, int plev, cptr p, cptr t)
{

	int py = p_ptr->py;
	int px = p_ptr->px;

        int i;

	int dir;
	int chance, beam;

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
			p_ptr->held_song = 0;

			return;
		}
	}
	/* Verify "warning" spells */
        else if ((p_ptr->csp - sc_ptr->mana) < (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
	{
		/* Warning */
		msg_format("You have limited mana to %s this %s.",p,t);

		/* Verify */
		if (!get_check("Attempt it anyway? "))
		{
			p_ptr->held_song = 0;

			return;
		}
	}

	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
                msg_format("You failed to %s the %s!",p,t);

                p_ptr->held_song = 0;
	}

	/* Process spell */
	else
	{
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = (c_info[p_ptr->pclass].sp_pow ? plev : (plev / 2));

		/* Spells.  */
		switch (spell)
		{
			case 1+0:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
				break;
			}

			case 1+1:
			{
				(void)detect_monsters_normal();
				break;
			}

			case 1+2:
			{
				teleport_player(10);
				break;
			}

			case 1+3:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 1+4:
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case 1+5:
			{
				(void)hp_player(damroll(2, 8));
				(void)set_cut(p_ptr->cut - 15);
				break;
			}

			case 1+6:
			{
				(void)detect_objects_normal();
				break;
			}

			case 1+7:
			{
				(void)detect_doors();
				(void)detect_traps();
				(void)detect_stairs();
				break;
			}

			case 1+8:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          10 + (plev / 2), 2);
				break;
			}

			case 1+9:
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				break;
			}

			case 1+10:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				break;
			}

			case 1+11:
			{
				(void)destroy_doors_touch();
				break;
			}

			case 1+12:
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			}

			case 1+13:
			{
				(void)set_poisoned(0);
				break;
			}

			case 1+14:
			{
				teleport_player(plev * 5);
				break;
			}

			case 1+15:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
				break;
			}

			case 1+16:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				break;
			}

			case 1+17:
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 1+18:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 1+19:
			{
				(void)recharge(5);
				break;
			}

			case 1+20:
			{
				(void)sleep_monsters_touch();
				break;
			}

			case 1+21:
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}

			case 1+22:
			{
				(void)ident_spell();
				break;
			}

			case 1+23:
			{
				(void)sleep_monsters();
				break;
			}

			case 1+24:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case 1+25:
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case 1+26:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          30 + (plev), 2);
				break;
			}

			case 1+27:
			{
				(void)recharge(40);
				break;
			}

			case 1+28:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 1+29:
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

			case 1+30:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          55 + (plev), 2);
				break;
			}

			case 1+31:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 1+32:
			{
				(void)genocide();
				break;
			}

			case 1+33:
			{
				(void)door_creation();
				break;
			}

			case 1+34:
			{
				(void)stair_creation();
				break;
			}

			case 1+35:
			{
				(void)teleport_player_level();
				break;
			}

			case 1+36:
			{
				earthquake(py, px, 10);
				break;
			}

			case 1+37:
			{
				set_recall();
				break;
			}

			case 1+38:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case 1+39:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
				          20 + (plev / 2), 3);
				break;
			}

			case 1+40:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          40 + (plev), 2);
				break;
			}

			case 1+41:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          70 + (plev), 3);
				break;
			}

			case 1+42:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
				          65 + (plev), 3);
				break;
			}

			case 1+43:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
				          300 + (plev * 2), 3);
				break;
			}

			case 1+44:
			{
				(void)detect_monsters_evil();
				break;
			}

			case 1+45:
			{
				(void)detect_objects_magic();
				break;
			}

			case 1+46:
			{
				recharge(100);
				break;
			}

			case 1+47:
			{
				(void)genocide();
				break;
			}

			case 1+48:
			{
				(void)mass_genocide();
				break;
			}

			case 1+49:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				break;
			}

			case 1+50:
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

			case 1+51:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				break;
			}

			case 1+52:
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case 1+53:
			{
				int time = randint(20) + 20;
				(void)set_oppose_acid(p_ptr->oppose_acid + time);
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				(void)set_oppose_fire(p_ptr->oppose_fire + time);
				(void)set_oppose_cold(p_ptr->oppose_cold + time);
				(void)set_oppose_pois(p_ptr->oppose_pois + time);
				break;
			}

			case 1+54:
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 1+55:
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

			case 1+56:
			{
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 1+57:
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

			case 1+58:
			{
				(void)set_invuln(p_ptr->invuln + randint(8) + 8);
				break;
			}

			case 60+0:
			{
				(void)detect_monsters_evil();
				break;
			}

			case 60+1:
			{
				(void)hp_player(damroll(2, 10) + plev);
				(void)set_cut(p_ptr->cut - 10);
				break;
			}

			case 60+2:
			{
				(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				break;
			}

			case 60+3:
			{
				(void)set_afraid(0);
				break;
			}

			case 60+4:
			{
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 60+5:
			{
				(void)detect_traps();
				break;
			}

			case 60+6:
			{
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 60+7:
			{
				(void)set_poisoned(0);
				break;
			}

			case 60+8:
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				break;
			}

			case 60+9:
			{
				teleport_player(plev * 3);
				break;
			}

			case 60+10:
			{
				(void)hp_player(damroll(4, 10) + plev);
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case 60+11:
			{
				(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				break;
			}

			case 60+12:
			{
				(void)sleep_monsters_touch();
				break;
			}

			case 60+13:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 60+14:
			{
				remove_curse();
				break;
			}

			case 60+15:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

			case 60+16:
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;

			}

			case 60+17:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_ORB, dir,
				          (damroll(3, 6) + plev +
				           (plev / ((c_info[p_ptr->pclass].sp_pow) ? 2 : 4))),
				          ((plev < 30) ? 2 : 3));
				break;
			}

			case 60+18:
			{
				(void)hp_player(damroll(8, 10) + plev + (plev /2));
				(void)set_cut(0);
				break;
			}

			case 60+19:
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
				break;
			}

			case 60+20:
			{
				(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			}

			case 60+21:
			{
				earthquake(py, px, 10);
				break;
			}

			case 60+22:
			{
				map_area();
				break;
			}

			case 60+23:
			{
				(void)hp_player(damroll(10, 10) + 2 * plev);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 60+24:
			{
				(void)turn_undead();
				break;
			}

			case 60+25:
			{
				(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				break;
			}

			case 60+26:
			{
				(void)dispel_undead(randint(plev * 3));
				break;
			}

			case 60+27:
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 60+28:
			{
				(void)dispel_evil(randint(plev * 3));
				break;
			}

			case 60+29:
			{
				warding_glyph();
				break;
			}

			case 60+30:
			{
				(void)dispel_evil(randint(plev * 4));
				(void)hp_player(1000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 60+31:
			{
				(void)detect_monsters_normal();
				break;
			}

			case 60+32:
			{
				(void)detect_all();
				break;
			}

			case 60+33:
			{
				(void)ident_spell();
				break;
			}

			case 60+34:
			{
				(void)probing();
				break;
			}

			case 60+35:
			{
				wiz_lite();
				break;
			}

			case 60+36:
			{
				(void)hp_player(damroll(6, 10) + plev);
				(void)set_cut(0);
				break;
			}

			case 60+37:
			{
				(void)hp_player(damroll(10, 10) + 2*plev);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 60+38:
			{
				(void)hp_player(2000);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 60+39:
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 60+40:
			{
				(void)restore_level();
				break;
			}

			case 60+41:
			{
				(void)dispel_undead(randint(plev * 4));
				break;
			}

			case 60+42:
			{
				(void)dispel_evil(randint(plev * 4));
				break;
			}

			case 60+43:
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			}

			case 60+44:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 60+45:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 200 + 2 * plev);
				break;
			}

			case 60+46:
			{
				(void)destroy_doors_touch();
				break;
			}

			case 60+47:
			{
				(void)recharge(15);
				break;
			}

			case 60+48:
			{
				(void)remove_all_curse();
				break;
			}

			case 60+49:
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

			case 60+50:
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

			case 60+51:
			{
				brand_weapon();
				break;
			}

			case 60+52:
			{
				teleport_player(10);
				break;
			}

			case 60+53:
			{
				teleport_player(plev * 8);
				break;
			}

			case 60+54:
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 60+55:
			{
				(void)teleport_player_level();
				break;
			}

			case 60+56:
			{
				set_recall();
				break;
			}

			case 60+57:
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}

		/*** And the new spells ***/
		/*** Of course, they are mostly old spells ***/


			case 118:
			{
				(void)set_shield(p_ptr->shield + randint(30) + 20);
				break;
			}


			case 119:
			{
				int time = randint(48) + 48;
				(void)set_blessed(p_ptr->blessed + time);
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + time);
				(void)set_afraid(0);
				break;
			}


			case 120:
			{
				int time = randint(48) + 48;

				(void)set_blessed(p_ptr->blessed + time);
				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + time);
				(void)set_afraid(0);
				break;
			}


			case 121:
			{
				(void)set_invuln(p_ptr->invuln + randint(8) + 8);
				break;
			}


			case 122:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
				break;
			}

			case 123:
			{

				wield_spell(INVEN_WIELD, SV_HAMMER_UNDEAD, randint(48) + 48);
				break;

			}


			case 124:
			{

				wield_spell(INVEN_WIELD, SV_HAMMER_DEMON, randint(48) + 48);
				break;

			}

			case 125:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          55 + (plev), 2);
				break;
			}

			case 126:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}



			case 127:
			{

				detect_monsters_undead();
				break;

			}

			case 128:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 129:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}


			case 130:
			{

                                detect_objects_cursed();
				break;

			}


			case 131:
			{
				enchant_item(TV_FOOD,plev);
				break;
			}

			case 132:
			{

				bless_weapon();
				break;

			}



			case 133:
			{

                                brand_armor();
				break;

			}


			case 134:
			{
				(void)set_poisoned(p_ptr->poisoned/2);
				break;
			}

			case 135:
			{
				if (!get_aim_dir(&dir)) return;
                                water_to_air(dir);
				break;
			}

			case 136:
			{
				int time = randint(20) + 20;
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				break;
			}

			case 137:
			{
				int time = randint(20) + 20;
				(void)set_oppose_acid(p_ptr->oppose_acid + time);
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				(void)set_oppose_fire(p_ptr->oppose_fire + time);
				(void)set_oppose_cold(p_ptr->oppose_cold + time);
				(void)set_oppose_pois(p_ptr->oppose_pois + time);
				break;
			}

			case 138:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, damroll(2, 10) + plev);
				break;
			}


			case 139:
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev);
				break;
			}


			case 140:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, damroll(2, 10) + (plev/2));
				break;
			}


			case 141:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}
			
			case 142:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, damroll(3, 10) + ( plev/2));
				break;
			}

			case 143:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, damroll(4, 10) + plev);
				break;
			}

			case 144:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 145:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, damroll(5, 10) + plev);
				break;
			}


			case 146:
			{

				if (!get_rep_dir(&dir)) return;
				fire_touch(GF_FIRE, dir,
				                  damroll(3 + ((plev - 1) / 5), 8));
				break;
			}


                        case 147:
			{
				if (!get_rep_dir(&dir)) return;
                                warding_trap(22,dir);
				break;

			}

                        case 148:
			{

				wield_spell(INVEN_WIELD, SV_BLADE_FIRE, randint(48) + 48);
				break;
			}

			case 149:
			{

				/* To do summon friendly monster */
				break;

			}


			case 150:
			{

				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;

			}

			case 151:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}

			case 152:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}


			case 153:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          95 + (plev), 3);
				break;
			}

			case 154:
			{

				if (!get_rep_dir(&dir)) return;
				fire_touch(GF_ACID, dir,
				                  damroll(3 + ((plev - 1) / 5), 8));
				break;

			}

			case 155:
			{
				if (!get_rep_dir(&dir)) return;
                                warding_trap(23,dir);
				break;


			}

                        case 156:
			{

				wield_spell(INVEN_WIELD, SV_BLADE_ACID, randint(48) + 48);
				break;

			}

			case 157:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}



			case 158:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}


			case 159:
			{

				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}

			case 160:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          65 + (plev), 3);
				break;
			}

			case 161:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;
			}



			case 162:
			{

				if (!get_rep_dir(&dir)) return;
				fire_touch(GF_ELEC, dir,
				                  damroll(3 + ((plev - 1) / 5), 8));
				break;
			}


			case 163:
			{

				int time = randint(20) + 20;
				(void)set_oppose_elec(p_ptr->oppose_elec + time);
				break;

			}

			case 164:
			{
				if (!get_rep_dir(&dir)) return;
                                warding_trap(78,dir);
				break;

			}

			case 165:
			{

				wield_spell(INVEN_WIELD, SV_BLADE_ELEC, randint(48) + 48);
				break;

			}


			case 166:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				break;
			}


			case 167:
			{

				if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				break;

			}

			case 168:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir,
				          90 + (plev), 2);
				break;
			}

			case 169:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				                  damroll(3+((plev-5)/4), 8));
				break;
			}

			case 170:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir,
				          115 + (plev), 2);
				break;
			}


			case 171:
			{

				if (!get_rep_dir(&dir)) return;
				fire_touch(GF_COLD, dir,
				                  damroll(3 + ((plev - 1) / 5), 8));
                                break;

			}

			case 172:
			{
				if (!get_rep_dir(&dir)) return;
                                warding_trap(132,dir);
				break;

			}

			case 173:
			{

				wield_spell(INVEN_WIELD, SV_BLADE_COLD, randint(48) + 48);
				break;
			}

			case 174:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));
				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));

				break;
			}



			case 175:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}



			case 176:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));

				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));

				if (!get_aim_dir(&dir)) break;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
				                  damroll(5+((plev-5)/4), 8));

				break;
			}

			case 177:
			{

				if (!get_aim_dir(&dir)) break;
                                fire_beam(GF_ICE, dir,
				                  damroll(5+((plev-5)/4), 8));

				break;
			}

                        case 178:
			{

				map_area();
				break;

			}



			case 179:
			{
				if (!get_rep_dir(&dir)) return;
				fire_blast(GF_OLD_CONF,dir,plev);
				break;
			}


			case 180:
			{
				(void)hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 181:
			{
				(void)ident_spell_tval(TV_POTION);
				break;
			}


			case 182:
			{
				int time = randint(20) + 20;
				(void)set_oppose_fire(p_ptr->oppose_fire + time);
				(void)set_oppose_cold(p_ptr->oppose_cold + time);
				break;
			}

			case 183:
			{
				(void)set_poisoned(0);
				(void)hp_player(damroll(4, 10) + plev);
				(void)set_cut(0);
				break;
			}

			case 184:
			{

				if (!get_rep_dir(&dir)) return;
				fire_blast(GF_FIRE,dir,damroll(2, 10)+plev/2);
				fire_blast(GF_SOUND,dir,damroll(2, 10)+plev/2);
				break;

			}

			case 185:
			{

				(void)hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

			case 186:
			{
				wiz_lite();
				break;
			}

			case 187:
			{
				enchant_item(TV_POTION,plev);
				break;
			}

			case 188:
			{
                                create_gold();
                                break;
			}


			case 189:
			{

                                detect_monsters_undead();
				break;

			}

			case 190:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}


			case 191:
			{
				(void)turn_undead();
				break;
			}


			case 192:
			{

				(void)dispel_undead(randint(plev * 4));
				break;
			}


			case 193:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}


			case 194:
			{
				if (banish_evil(100))
				{
					msg_print("The power of your magic banishes evil!");
				}
				break;
			}

			case 195:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 196:
			{

				(void)detect_monsters_animal;
				break;

			}

			case 197:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 198:
			{

				/* XXX To do: Make monsters friendly */ break;

			}

			case 199:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 200:
			{
				wield_spell(INVEN_BODY, SV_DRAGONSCALES, randint(100) + 100);
				break;
			}

			case 201:
			{
				int choice = rand_int(100);

				if (!get_aim_dir(&dir)) return;
				if (choice < 20)
				{
					fire_ball(GF_FIRE, dir,
					          55 + (plev), 2);
				}
				else if (choice < 40)
				{
					fire_ball(GF_COLD, dir,
					          30 + (plev), 2);
				}
				else if (choice < 60)
				{
					fire_ball(GF_ELEC, dir,
					          45 + (plev), 2);
				}
				else if (choice < 80)
				{
					fire_ball(GF_ACID, dir,
					          40 + (plev), 2);

				}
				else
				{
					fire_ball(GF_POIS, dir,
					          35 + (plev), 2);

				}
				break;
			}

			case 202:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 203:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 204:
			{

				fear_monsters();
				break;

			}

			case 205:
			{

				confuse_monsters();
				break;

			}

			case 206:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 207:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 208:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 209:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 210:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 211:
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}

			case 212:
			{

				wield_spell(INVEN_LITE, SV_LITE_WIZARD, randint(200) + 200);
				break;

			}

			case 213:
			{
				int k;

				for (k = 0; k < 8; k++) lite_line(ddd[k]);

				break;
			}

			case 214:
			{
				(void)lite_area(damroll(4, (plev / 2)), (plev / 5) + 1);
				break;
			}

			case 215:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_METEOR, dir,
				                  damroll(10+((plev-5)/4), 8));
				break;
				
			}

			case 216:
			{

				wield_spell(INVEN_BODY, SV_STONESKIN, randint(100) + 100);
				break;

			}

			case 217:
			{

				if (!get_aim_dir(&dir)) return;
                                raise_bridge(dir);

				break;
			}

			case 218:
			{

				/* XXX To do: Summon friendly monsters */ break;

			}

			case 219:
			{
				if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_SHARD, dir,
				                  damroll(5+((plev-5)/4), 8));

				break;
			}

			case 220:
			{
				if (!get_aim_dir(&dir)) return;
                                water_to_air(dir);
				break;
			}


			case 221:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
				          10 + (plev / 2), (plev/10) + 2);
				break;

			}
			case 222:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
				          10 + (plev / 2), (plev/10) + 2);
				break;

			}

			case 223:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir,
				          10 + (plev / 2), (plev/10) + 2);
				break;

			}

			case 224:
			{
				/* XXX To do: Summon friendly monsters */ break;
				
			}

			case 225:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          10 + (plev / 2), (plev/10) + 2);
				break;
			}


			case 226:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
				          80+plev, (plev/10) + 2);
				break;
			}

			case 227:
			{
				detect_water();
				break;

			}

			case 228:
			{
				(void)lower_water(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 229:
			{
				(void)raise_water(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 230:
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_WATER, dir,
				                  damroll(5+((plev-5)/4), 8));
				break;
			
			}

			case 231:
			{
				/* XXX To do: Summon friendly monsters */ break;
				
			}



			case 232:
			{

				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_WATER, dir,
				                  damroll(6+((plev-5)/4), 8));
				break;

			}

			case 233:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_WATER, dir,
				          60+plev, 3);
				break;
			}

			case 234:
			{
				(void)destroy_doors_touch();
				break;
			}

			case 235:
			{
				sleep_monsters();
				break;
			}

			case 236:
			{
				dispel_monsters(10+plev);
				break;
			}

			case 237:
			{
                                wield_spell(INVEN_WIELD, SV_QUARTERSTAFF_RUNE, randint(200) + 200);
				break;
			}
			case 238:
			{
				dispel_monsters(20+2*plev);
				break;
			}

			case 239:
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				break;
			}


			case 240:
			{

				/* XXX To do: Make monsters friendly */ break;

			}

			case 241:
			{

				(void)do_inc_stat(A_CHR);
				break;

			}

			case 242:
			{

				/* XXX To do: Make monsters friendly */ break;

			}

			case 243:
			{
				(void)ident_spell_tval(TV_SCROLL);
				break;
			}

			case 244:
			{
				if (!identify_fully()) return;
				break;
			}

			case 245:
			{
				(void)hp_player(damroll(6, 10) + plev);
				(void)set_cut(0);
				break;
			}

			case 246:
			{
				(void)hp_player(damroll(10, 10) + 2*plev);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}


			case 247:
			{
				/* To do: Summon friendly monster */
				break;
			}
			case 248:
			{
				enchant_item(TV_WAND,plev);
				break;
			}

			case 249:
			{
				enchant_item(TV_STAFF,plev);
				break;
			}


			case 250:
			{
				enchant_item(TV_ROD,plev);
				break;
			}

			case 251:
			{
				if (!get_rep_dir(&dir)) return;
				warding_trap(21,dir);
				break;
			}

			case 252:
			{
				warding_glyph();
				break;
			}

			case 253:
			{
                                wield_spell(INVEN_HEAD, SV_PSYCHIC_CROWN, randint(48) + 48);
				break;
			}

			case 254:
			{

				if (!get_rep_dir(&dir)) return;
				fire_blast(GF_SOUND,dir,damroll(4, 10)+plev);
				break;

			}

			case 255:
			{

				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam+10, GF_SOUND, dir,
				                  damroll(8+((plev-5)/4), 8));
				break;
			}
		}


		for (i=0;i<PY_MAX_SPELLS;i++)
		{

			if (p_ptr->spell_order[i] == spell) break;

		}


		/* A spell was cast */ 
		if (!((i < 32) ?
		      (p_ptr->spell_worked1 & (1L << i)) :
		      (p_ptr->spell_worked2 & (1L << (i - 32)))))
		{
                        int e = sc_ptr->level;

			/* The spell worked */
			if (i < 32)
			{
				p_ptr->spell_worked1 |= (1L << i);
			}
			else
			{
				p_ptr->spell_worked2 |= (1L << (i - 32));
			}

			/* Gain experience */
			gain_exp(e * sc_ptr->level);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

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
        int i;

	int item,spell;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	cptr p, t;

	cptr q, s;


	/* Check if we are holding a song */
	if (p_ptr->held_song)
	{
		/* Verify */
		if (get_check("Continue singing?")) return;
	}

	/* Stop singing - if started */
	p_ptr->held_song = 0;

	/* Cannot cast spells if illiterate */
	if (c_info[p_ptr->pclass].sp_lvl > 50)
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

	/* Cast, recite, sing or play */
        switch (o_ptr->tval)
	{

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
       	                p="cast";
			t="spell";
			break;		
	        }
	}

	/* Ask for a spell */
	if (!get_spell(&spell, p, o_ptr, TRUE))
	{
		if (spell == -2) msg_format("You don't know any &ss in that book.",t);
		return;
	}

	/* Modify the spell power */
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if (w_info[i].styles & (1L<< p_ptr->pstyle)) continue;

		/* Line 1 --- casting from a spell book */
		/* Line 2 --- spell can be held */
		/* Line 3 --- capable of holding song */
                if ((o_ptr->tval == TV_SONG_BOOK) &&
			(s_info[spell].flags & (SF_HOLD_SONG)) &&
			 (w_info[i].benefit == WB_HOLD_SONG)) p_ptr->held_song = spell;

                if (w_info[i].benefit != WB_POWER) continue;

		switch (p_ptr->pstyle)
		{
			case WS_WAND:
			{
				if (!(o_ptr->tval == TV_WAND)) break;
				plev += (p_ptr->lev - w_info[i].level);
				break;
			}
			case WS_STAFF:
			{
				if (!(o_ptr->tval == TV_STAFF)) break;
				plev += (p_ptr->lev - w_info[i].level);
				break;
			}
			case WS_POTION:
			{
				if (!(o_ptr->tval == TV_POTION)) break;
				plev += (p_ptr->lev - w_info[i].level);
				break;
			}
			case WS_SCROLL:
			{
				if (!(o_ptr->tval == TV_SCROLL)) break;
				plev += (p_ptr->lev - w_info[i].level);
				break;
			}
			case WS_AMULET:
				if (!(o_ptr->tval == TV_AMULET)) break;
				plev += (p_ptr->lev - w_info[i].level);
				break;
			case WS_RING:
				if (!(o_ptr->tval == TV_RING)) break;
				plev += (p_ptr->lev - w_info[i].level);
				break;
			case WS_MAGIC_BOOK:
				if (!(o_ptr->tval == TV_MAGIC_BOOK)) break;
                                if (o_ptr->sval == p_ptr->psval) plev += (p_ptr->lev - w_info[i].level);
				break;
			case WS_PRAYER_BOOK:
				if (!(o_ptr->tval == TV_PRAYER_BOOK)) break;
                                if (o_ptr->sval == p_ptr->psval) plev += (p_ptr->lev - w_info[i].level);
				break;
			case WS_SONG_BOOK:
				if (!(o_ptr->tval == TV_SONG_BOOK)) break;
                                if (o_ptr->sval == p_ptr->psval) plev += (p_ptr->lev - w_info[i].level);
				break;
			case WS_INSTRUMENT:
				if (!(p_ptr->cur_style & (1L << WS_INSTRUMENT))) break;

                                /* Line 1 - has item wielded */
                                /* Line 2 - item is instrument */
                                /* Line 3 - instrument sval matches spellbook sval */
                                if ((inventory[INVEN_WIELD].k_idx) &&
                                (inventory[INVEN_WIELD].tval == TV_INSTRUMENT) &&
                                (o_ptr->sval == inventory[INVEN_WIELD].sval))
                                        plev += (p_ptr->lev - w_info[i].level);

                                /* Line 1 - has item in off-hand */
                                /* Line 2 - item is instrument */
                                /* Line 3 - instrument sval matches spellbook sval */
                                else if ((inventory[INVEN_ARM].k_idx) &&
                                (inventory[INVEN_ARM].tval == TV_INSTRUMENT) &&
                                (o_ptr->sval == inventory[INVEN_ARM].sval))
                                        plev += (p_ptr->lev - w_info[i].level);
				break;

			default:
                                if (w_info[i].styles & p_ptr->cur_style) plev += (p_ptr->lev - w_info[i].level);
				break;
		}

	}

	/* Cast the spell - held songs get cast later*/
        if (!(p_ptr->held_song)) do_cmd_cast_aux(spell,plev,p,t);
	
}

