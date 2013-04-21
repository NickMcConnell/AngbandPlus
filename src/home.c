/* File: home.c */

/*
 * Copyright (C)1997 elemental
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * We store the current "inventory page" here so everyone can access it
 */
static int store_top = 0;


/*
 * Check to see if the home will be carrying too many objects
 *
 * Note that the home, just like a player, will not accept things
 * it cannot hold.  Before, one could "nuke" objects this way, by
 * adding them to a pile which was already full.
 */
static bool home_check_num(object_type *o_ptr)
{
	int i;
	object_type *j_ptr;

	/* Free space is always usable */
	if (home.stock_num < home.stock_size) return TRUE;

	/* Check all the items */
	for (i = 0; i < home.stock_num; i++)
	{
		/* Get the existing item */
		j_ptr = &home.stock[i];

		/* Can the new object be combined with the old one? */
		if (object_similar(j_ptr, o_ptr)) return (TRUE);
	}

	/* But there was no room at the inn... */
	return (FALSE);
}

/*
 * Add the item "o_ptr" to the inventory of the "home"
 *
 * In all cases, return the slot (or -1) where the object was placed
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" becoming
 * known, the player may have to pick stuff up and drop it again.
 */
static int home_carry(object_type *o_ptr)
{
	int slot;
	s32b value, j_value;
	int i;
	object_type *j_ptr;


	/* Check each existing item (try to combine) */
	for (slot = 0; slot < home.stock_num; slot++)
	{
		/* Get the existing item */
		j_ptr = &home.stock[slot];

		/* The home acts just like the player */
		if (object_similar(j_ptr, o_ptr))
		{
			/* Save the new number of items */
			object_absorb(j_ptr, o_ptr);

			/* All done */
			return (slot);
		}
	}

	/* No space? */
	if (home.stock_num >= home.stock_size) return (-1);


	/* Determine the "value" of the item */
	value = object_value(o_ptr);

	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < home.stock_num; slot++)
	{
		/* Get that item */
		j_ptr = &home.stock[slot];

		/* Hack -- readable books always come first */
		if ((o_ptr->tval == mp_ptr->spell_book) &&
		    (j_ptr->tval != mp_ptr->spell_book)) break;
		if ((j_ptr->tval == mp_ptr->spell_book) &&
		    (o_ptr->tval != mp_ptr->spell_book)) continue;

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Can happen in the home */
		if (!object_aware_p(o_ptr)) continue;
		if (!object_aware_p(j_ptr)) break;

		/* Objects sort by increasing sval */
		if (o_ptr->sval < j_ptr->sval) break;
		if (o_ptr->sval > j_ptr->sval) continue;

		/* Objects in the home can be unknown */
		if (!object_known_p(o_ptr)) continue;
		if (!object_known_p(j_ptr)) break;

		/* Hack:  otherwise identical rods sort by
		   increasing recharge time --dsb */
		if (o_ptr->tval == TV_ROD) {
			if (o_ptr->pval < j_ptr->pval) break;
			if (o_ptr->pval > j_ptr->pval) continue;
		}

		/* Objects sort by decreasing value */
		j_value = object_value(j_ptr);
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = home.stock_num; i > slot; i--)
	{
		/* Hack -- slide the objects */
		object_copy(&home.stock[i], &home.stock[i-1]);
	}

	/* More stuff now */
	home.stock_num++;

	/* Hack -- Insert the new item */
	object_copy(&home.stock[slot], o_ptr);

	/* Return the location */
	return (slot);
}

/*
 * Remove a slot if it is empty
 */
static void home_item_optimize(int item)
{
	int j;
	object_type *o_ptr;

	/* Get the item */
	o_ptr = &home.stock[item];

	/* Must exist */
	if (!o_ptr->k_idx) return;

	/* Must have no items */
	if (o_ptr->number) return;

	/* One less item */
	home.stock_num--;

	/* Slide everyone */
	for (j = item; j < home.stock_num; j++)
	{
		home.stock[j] = home.stock[j + 1];
	}

	/* Nuke the final slot */
	object_wipe(&home.stock[j]);
}


/*
 * Re-displays a single home entry
 */
static void display_entry(int pos)
{
	int i;
	object_type *o_ptr;
	char o_name[80];
	char out_val[160];

	int maxwid = 75;
	byte attr;

	/* Get the item */
	o_ptr = &home.stock[pos];

	/* Get the "offset" */
	i = (pos % 12);

	/* Label it, clear the line --(-- */
	sprintf(out_val, "%c) ", I2A(i));
	prt(out_val, i+6, 0);

	/* Describe an item in the home */
	maxwid = 75; /* Leave room for weights, if necessary -DRS- */
	if (show_weights) maxwid -= 10;

	/* Describe the object */
	object_desc(o_name, o_ptr, TRUE, 3);
	o_name[maxwid] = '\0';

	/* Acquire inventory color */
	attr = tval_to_attr[o_ptr->tval & 0x7F];

	/* Disable inventory colors */
/*	if (!inventory_colors) attr = TERM_WHITE; */

	/* Display the object */
	c_put_str(attr, o_name, i+6, 3);

	/* Show weights */
	if (show_weights)
	{
		/* Only show the weight of an individual item */
		int wgt = o_ptr->weight;
		sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
		put_str(out_val, i+6, 68);
	}
}


/*
 * Display a home's inventory
 *
 * All prices are listed as "per individual object"
 */
static void display_inventory(void)
{
	int i, k;

	/* Display the next 12 items */
	for (k = 0; k < 12; k++)
	{
		/* Do not display "dead" items */
		if (store_top + k >= home.stock_num) break;

		/* Display that line */
		display_entry(store_top + k);
	}

	/* Erase the extra lines and the "more" prompt */
	for (i = k; i < 13; i++) prt("", i + 6, 0);

	/* Assume "no current page" */
	put_str("        ", 5, 20);

	/* Visual reminder of "more items" */
	if (home.stock_num > 12)
	{
		/* Show "more" reminder (after the last item) */
		prt("-more-", k + 6, 3);

		/* Indicate the "current page" */
		put_str(format("(Page %d)", store_top/12 + 1), 5, 20);
	}
}


/*
 * Display home (after clearing screen)
 */
static void display_home(void)
{
	/* Clear screen */
	Term_clear();

	/* Put the owner name */
	put_str("Your Home", 3, 30);

	/* Label the item descriptions */
	put_str("Item Description", 5, 3);

	/* If showing weights, show label */
	if (show_weights)
	{
		put_str("Weight", 5, 70);
	}

	/* Draw in the inventory */
	display_inventory();
}



/*
 * Get the index of a home item
 *
 * Return TRUE if an item was selected
 */
static bool get_stock(int *com_val, cptr pmt, int i, int j)
{
	char command;

	char out_val[160];


	/* Paranoia XXX XXX XXX */
	msg_print(NULL);


	/* Assume failure */
	*com_val = (-1);

	/* Build the prompt */
	sprintf(out_val, "(Items %c-%c, ESC to exit) %s",
	              I2A(i), I2A(j), pmt);

	/* Ask until done */
	while (TRUE)
	{
		int k;

		/* Escape */
		if (!get_com(out_val, &command)) break;

		/* Convert */
		k = (islower(command) ? A2I(command) : -1);

		/* Legal responses */
		if ((k >= i) && (k <= j))
		{
			*com_val = k;
			break;
		}

		/* Oops */
		bell("Illegal command.");
	}

	/* Clear the prompt */
	prt("", 0, 0);

	/* Cancel */
	if (command == ESCAPE) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Increase, by a given amount, the number of a certain item.
 * This can result in zero items.
 */
static void home_item_increase(int item, int num)
{
	int cnt;
	object_type *o_ptr;

	/* Get the item */
	o_ptr = &home.stock[item];

	/* Verify the number */
	cnt = o_ptr->number + num;
	if (cnt > 255) cnt = 255;
	else if (cnt < 0) cnt = 0;
	num = cnt - o_ptr->number;

	/* Save the new number */
	o_ptr->number += num;
}


/*
 * Get an item from a home
 */
static void home_get(void)
{
	int i, amt = 1;
	int item, item_new;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[80];

	char out_val[160];


	/* Empty? */
	if (home.stock_num <= 0)
	{
		msg_print("Your home is empty.");
		return;
	}


	/* Find the number of objects on this and following pages */
	i = (home.stock_num - store_top);

	/* And then restrict it to the current page */
	if (i > 12) i = 12;

	/* Prompt */
	sprintf(out_val, "Which item do you want to take? ");

	/* Get the item number to be bought */
	if (!get_stock(&item, out_val, 0, i-1)) return;

	/* Get the actual index */
	item = item + store_top;

	/* Get the actual item */
	o_ptr = &home.stock[item];

	/* prompt for quantity (or use p_ptr->command_arg) */
	if ( o_ptr->number > 1 ) {
		amt = get_quantity( NULL, o_ptr->number );
		if ( amt <= 0 ) return; /* allow abort */
	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Get desired object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(i_ptr))
	{
		msg_print("You cannot carry that many items.");
		return;
	}

	/* Give it to the player */
	item_new = inven_carry(i_ptr);

	/* Describe just the result */
	object_desc(o_name, &inventory[item_new], TRUE, 3);

	/* Message */
	msg_format("You have %s (%c).", o_name, index_to_label(item_new));

	/* Handle stuff */
	handle_stuff();

	/* Take note if we take the last one */
	i = home.stock_num;

	/* Remove the items from the home */
	home_item_increase(item, -amt);
	home_item_optimize(item);

	/* Hack -- Item is still here */
	if (i == home.stock_num)
	{
		/* Redraw the item */
		display_entry(item);
	}

	/* The item is gone */
	else
	{
		/* Nothing left */
		if (home.stock_num == 0) store_top = 0;

		/* Nothing left on that screen */
		else if (store_top >= home.stock_num) store_top -= 12;

		/* Redraw everything */
		display_inventory();
	}

	return;
}


/*
 * Put an item in a home
 */
static void home_put(void)
{
	int item, item_pos; int amt = 1;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	cptr q, s;

	char o_name[80];


	q = "Drop which item? ";

	/* Get an item */
	s = "You are not carrying anything you can drop!";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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


	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* prompt for quantity (or use p_ptr->command_arg) */
	if ( o_ptr->number > 1 ) {
		amt = get_quantity( NULL, o_ptr->number );
		if ( amt <= 0 ) return; /* allow abort */
	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Get a copy of the object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Get a full description */
	object_desc(o_name, i_ptr, TRUE, 3);

	/* Is there room in the home? */
	if (!home_check_num(i_ptr))
	{
		msg_print("Your home is full.");
		return;
	}

	/* Describe */
	msg_format("You drop %s (%c).", o_name, index_to_label(item));

	/* Take it from the players inventory */
	inven_item_increase(item, -amt);
	inven_item_describe(item);
	inven_item_optimize(item);

	/* Handle stuff */
	handle_stuff();

	/* Let the home carry it */
	item_pos = home_carry(i_ptr);

	/* Update home display */
	if (item_pos >= 0)
	{
		store_top = (item_pos / 12) * 12;
		display_inventory();
	}
}


static bool leave_home = FALSE;


/*
 * Process a command in a home
 *
 * Note that we must allow the use of a few "special" commands
 * in the home which are not allowed in the dungeon, and we
 * must disable some commands which are allowed in the dungeon
 * but not in the home, to prevent chaos.
 *
 * Hack -- note the bizarre code to handle the "=" command,
 * which is needed to prevent the "redraw" from affecting
 * the display of the home.  XXX XXX XXX
 */
static void home_process_command(void)
{
	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
		/* Leave */
		case ESCAPE:
		{
			leave_home = TRUE;
			break;
		}

		/* Browse */
		case ' ':
		{
			if (home.stock_num <= 12)
			{
				msg_print("Entire inventory is shown.");
			}
			else
			{
				store_top += 12;
				if (store_top >= home.stock_num) store_top = 0;
				display_inventory();
			}
			break;
		}

		/* Redraw */
		case KTRL('R'):
		{
			do_cmd_redraw();
			display_home();
			break;
		}

		/* Get */
		case 'g':
		{
			home_get();
			break;
		}

		/* Drop */
		case 'd':
		{
			home_put();
			break;
		}

		/* Ignore return */
		case '\r':
		{
			break;
		}



		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			do_cmd_wield();
			break;
		}

			/* Take off equipment */
		case 't':
		{
			do_cmd_takeoff();
			break;
		}

		/* Destroy an item */
		case 'k':
		{
			do_cmd_destroy();
			break;
		}

		/* Equipment list */
		case 'e':
		{
			do_cmd_equip();
			break;
		}

		/* Inventory list */
		case 'i':
		{
			do_cmd_inven();
			break;
		}


		/*** Various commands ***/

		/* Identify an object */
		case 'I':
		{
			do_cmd_observe();
			break;
		}

		/* Hack -- toggle windows */
		case KTRL('E'):
		{
			toggle_inven_equip();
			break;
		}



		/*** Use various objects ***/

		/* Browse a book */
		case 'b':
		{
			do_cmd_browse();
			break;
		}

		/* Inscribe an object */
		case '{':
		{
			do_cmd_inscribe();
			break;
		}

		/* Uninscribe an object */
		case '}':
		{
			do_cmd_uninscribe();
			break;
		}



		/*** Help and Such ***/

		/* Help */
		case '?':
		{
			do_cmd_help();
			break;
		}

		/* Identify symbol */
		case '/':
		{
			do_cmd_query_symbol();
			break;
		}

		/* Character description */
		case 'C':
		{
			do_cmd_change_name();
			break;
		}


		/*** System Commands ***/

		/* Hack -- User interface */
		case '!':
		{
			(void)Term_user(0);
			break;
		}

		/* Single line from a pref file */
		case '"':
		{
			do_cmd_pref();
			break;
		}

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals();
			break;
		}

		/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			break;
		}

		/* Interact with options */
		case '=':
		{
			do_cmd_options();
			character_icky = TRUE;
			do_cmd_redraw();
			display_home();
			break;
		}


		/*** Misc Commands ***/

		/* Take notes */
		case ':':
		{
			do_cmd_note();
			break;
		}

		/* Version info */
		case 'V':
		{
			do_cmd_version();
			break;
		}

		/* Repeat level feeling */
		case KTRL('F'):
		{
			do_cmd_feeling();
			break;
		}

		/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages();
			break;
		}

		/* Check knowledge */
		case '~':
		case '|':
		{
			do_cmd_knowledge();
			break;
		}

		/* Load "screen dump" */
		case '(':
		{
			do_cmd_load_screen();
			break;
		}

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}


		/* Unknown command */
		default:
		{
			msg_print("That command does not work in the home.");
			break;
		}
	}
}


/*
 * Enter a home, and interact with it.
 *
 * Note that we use the standard "request_command()" function
 * to get a command, allowing us to use "p_ptr->command_arg" and all
 * command macros and other nifty stuff.
 */
void do_cmd_home(void)
{
	int tmp_chr;


	/* Forget the view */
	forget_view();


	/* Hack -- Character is in "icky" mode */
	character_icky = TRUE;


	/* No command argument */
	p_ptr->command_arg = 0;

	/* No repeated command */
	p_ptr->command_rep = 0;

	/* No automatic command */
	p_ptr->command_new = 0;

	/* Start at the beginning */
	store_top = 0;

	/* Display the home */
	display_home();

	/* Do not leave */
	leave_home = FALSE;

	/* Interact with player */
	while (!leave_home)
	{
		/* Hack -- Clear line 1 */
		prt("", 1, 0);

		/* Hack -- Check the charisma */
		tmp_chr = p_ptr->stat_use[A_CHR];

		/* Clear */
		clear_from(21);

		/* Basic commands */
		prt(" ESC) Exit from Building.", 22, 0);

		/* Browse if necessary */
		if (home.stock_num > 12)
		{
			prt(" SPACE) Next page of stock", 23, 0);
		}

		/* Home commands */
		prt(" g) Get an item.", 22, 40);
		prt(" d) Drop an item.", 23, 40);

		/* Prompt */
		prt("You may: ", 21, 0);

		/* Get a command */
		request_command();

		/* Process the command */
		home_process_command();

		/* Hack -- Character is still in "icky" mode */
		character_icky = TRUE;

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* XXX XXX XXX Pack Overflow */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK;

			object_type *o_ptr = &inventory[item];

			if (!home_check_num(o_ptr))
			{
				/* Message */
				msg_print("Your pack is so full that you flee your home...");

				/* Leave */
				leave_home = TRUE;
			}

			/* Hack -- Drop items into the home */
			else
			{
				int item_pos;

				object_type *i_ptr;
				object_type object_type_body;

				char o_name[80];


				/* Give a message */
				msg_print("Your pack overflows!");

				/* Get local object */
				i_ptr = &object_type_body;

				/* Grab a copy of the item */
				object_copy(i_ptr, o_ptr);

				/* Describe it */
				object_desc(o_name, i_ptr, TRUE, 3);

				/* Message */
				msg_format("You drop %s (%c).", o_name, index_to_label(item));

				/* Remove it from the players inventory */
				inven_item_increase(item, -255);
				inven_item_describe(item);
				inven_item_optimize(item);

				/* Handle stuff */
				handle_stuff();

				/* Let the home carry it */
				item_pos = home_carry(i_ptr);

				/* Redraw the home */
				if (item_pos >= 0)
				{
					store_top = (item_pos / 12) * 12;
					display_inventory();
				}
			}
		}
	}


	/* Free turn */
	p_ptr->energy_use = 0;


	/* Hack -- Character is no longer in "icky" mode */
	character_icky = FALSE;


	/* Hack -- Cancel automatic command */
	p_ptr->command_new = 0;

	/* Hack -- Cancel "see" mode */
	p_ptr->command_see = FALSE;


	/* Flush messages XXX XXX XXX */
	msg_print(NULL);


	/* Clear the screen */
	Term_clear();


	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}

void home_generate_item( int tval, int sval, int l_range, int h_range )
{
	object_type *i_ptr;
	object_type object_type_body;

	i_ptr = &object_type_body;
	object_prep( i_ptr, lookup_kind( tval, sval ));
	i_ptr->number = rand_range( l_range, h_range );
	object_aware( i_ptr );
	object_known( i_ptr );
	home_carry( i_ptr );
}

/*
 * Initialize the home
 */
void home_init()
{
	int k;

	/* Initialize the home */
	home.stock_num = 0;

	/* Clear any old items */
	for (k = 0; k < home.stock_size; k++)
	{
		object_wipe(&home.stock[k]);
	}

	/* Stock up the home */

	/* Some food */
	switch (p_ptr->prace) {
		case RACE_HALF_ELF:
		case RACE_ELF:
		case RACE_HIGH_ELF:
			home_generate_item( TV_FOOD, SV_FOOD_RATION, 3, 7 );
			home_generate_item( TV_FOOD, SV_FOOD_WAYBREAD, 1, 2 );
			break;

		default:
			home_generate_item( TV_FOOD, SV_FOOD_RATION, 5, 10 );
	}

	if ( rand_range( 1, 100 ) >= 76 ) {
		/* A lantern and some flasks of oil */
		home_generate_item( TV_LITE, SV_LITE_LANTERN, 1, 1 );
		home_generate_item( TV_FLASK, 0, 1, 3 );
	} else {
		/* Some torches */
		home_generate_item( TV_LITE, SV_LITE_TORCH, 5, 10 );
	}
}

