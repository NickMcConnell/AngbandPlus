/*
 * File: obj-ui.c
 * Purpose: Mainly object descriptions and generic UI functions
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Diego Gonzalez, Jeff Greene
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
#include "ui-menu.h"
#include "game-event.h"


/*
 * Display a list of objects.  Each object may be prefixed with a label.
 * Used by show_inven(), show_equip(), and show_floor().  Mode flags are
 * documented in object.h
 */
static void show_obj_list(int num_obj, char labels[50][80], object_type *objects[50], byte mode)
{
	int i, row = 0, col = 0;
	size_t max_len = 0;
	int ex_width = 0, ex_offset, ex_offset_ctr;

	object_type *o_ptr;
	char o_name[50][80];
	char tmp_val[80];

	bool in_term;

	in_term = (mode & OLIST_WINDOW) ? TRUE : FALSE;

	if (in_term) max_len = 40;

	/* Calculate name offset and max name length */
	for (i = 0; i < num_obj; i++)
	{
		o_ptr = objects[i];

		/* Null objects are used to skip lines, or display only a label */
		if (o_ptr == NULL) continue;

		/* Max length of label + object name */
		object_desc(o_name[i], sizeof(o_name[i]), o_ptr, ODESC_PREFIX | ODESC_FULL);
		max_len = MAX(max_len, strlen(labels[i]) + strlen(o_name[i]));
	}

	/* Width of extra fields */
	if (mode & OLIST_WEIGHT) ex_width += 9;
	if (mode & OLIST_PRICE) ex_width += 9;
	if (mode & OLIST_FAIL) ex_width += 10;

	/* Determine beginning row and column */
	if (in_term)
	{
		/* Term window */
		row = 0;
		col = 0;
	}
	else
	{
		/* Main window */
		row = 1;
		col = Term->wid - 1 - max_len - ex_width;

		if (col < 3) col = 0;
	}

	/* Column offset of the first extra field */
	ex_offset = MIN(max_len, (size_t)(Term->wid - 1 - ex_width - col));

	/* Output the list */
	for (i = 0; i < num_obj; i++)
	{
		o_ptr = objects[i];

		/* Clear the line */
		prt("", row + i, MAX(col - 2, 0));

		/* Print the label */
		put_str(labels[i], row + i, col);

		/* Print the object */
		if (o_ptr != NULL)
		{
			/* Limit object name */
			if (strlen(labels[i]) + strlen(o_name[i]) > (size_t)ex_offset)
			{
				int truncate = ex_offset - strlen(labels[i]);

				if (truncate < 0) truncate = 0;
				if ((size_t)truncate > sizeof(o_name[i]) - 1) truncate = sizeof(o_name[i]) - 1;

				o_name[i][truncate] = '\0';
			}

			/* Object name */
			c_put_str(tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)], o_name[i],
			          row + i, col + strlen(labels[i]));

			/* Extra fields */
			ex_offset_ctr = ex_offset;

			if (mode & OLIST_PRICE)
			{
				int price = price_item(o_ptr, TRUE) * o_ptr->number;
				strnfmt(tmp_val, sizeof(tmp_val), "%6d au", price);
				put_str(tmp_val, row + i, col + ex_offset_ctr);
				ex_offset_ctr += 9;
			}

			if (mode & OLIST_FAIL)
			{
				int fail = (9 + get_use_device_chance(o_ptr)) / 10;
				if (o_ptr->ident & (IDENT_MENTAL))
					strnfmt(tmp_val, sizeof(tmp_val), "%4d%% fail", fail);
				else
					my_strcpy(tmp_val, "    ? fail", sizeof(tmp_val));
				put_str(tmp_val, row + i, col + ex_offset_ctr);
				ex_offset_ctr += 10;
			}

			if (mode & OLIST_WEIGHT)
			{
				int weight = o_ptr->weight * o_ptr->number;
				strnfmt(tmp_val, sizeof(tmp_val), "%4d.%1d lb", weight / 10, weight % 10);
				put_str(tmp_val, row + i, col + ex_offset_ctr);
				ex_offset_ctr += 9;
			}
		}
	}

	/* For the inventory: print the quiver count */
	if (mode & OLIST_QUIVER)
	{
		int count, j;

		/* Quiver may take multiple lines */
		for(j = 0; j < p_ptr->quiver_slots; j++, i++)
		{
			/* Number of missiles in this "slot" */
			if (j == p_ptr->quiver_slots - 1 && p_ptr->quiver_remainder > 0)
				count = p_ptr->quiver_remainder;
			else
				count = 99;

			/* Clear the line */
			prt("", row + i, MAX(col - 2, 0));

			/* Print the (disabled) label */
			strnfmt(tmp_val, sizeof(tmp_val), "%c) ", index_to_label(i));
			c_put_str(TERM_SLATE, tmp_val, row + i, col);

			/* Print the count */
			strnfmt(tmp_val, sizeof(tmp_val), "in Quiver: %d missile%s", count,
					count == 1 ? "" : "s");
			c_put_str(TERM_L_UMBER, tmp_val, row + i, col + 3);
		}
	}

	/* Clear term windows */
	if (in_term)
	{
		for (; i < Term->hgt; i++)
		{
			prt("", row + i, MAX(col - 2, 0));
		}
	}

	/* Print a drop shadow for the main window if necessary */
	else if (i > 0 && row + i < 24)
	{
		prt("", row + i, MAX(col - 2, 0));
	}
}

/*
 * Helper function for the get_item or the cmd_use_item functions.
 * Helps to ensure there is only one item marked.
 * Clear all objects of the in_use flag;
 */
static void clear_object_in_use(void)
{
	int i;
	object_type *o_ptr;

	/* Clear everything in the backpack, equipment, and quiver */
	for (i = 0; i < ALL_INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		o_ptr->obj_in_use = FALSE;
	}

	/* Now clear everything on the floor */
	for (i = 1; i < o_max; i++)
	{
		/* Get the object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;
		o_ptr->obj_in_use = FALSE;
	}
}


/*
 * Helper function for the get_item or the cmd_use_item functions.
 * Find the object in the inventory or on the floor that is in use by the get_item function.
 * Only one object should be in use at a time to avoid confusion.
 * Returns TRUE/FALSE, and uses a pointer to return the item in use
 */

bool find_object_in_use(int *item)
{
	int i;
	object_type *o_ptr;

	/* First look through the backpack, equipment, and quiver */
	for (i = 0; i < ALL_INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Found it */
		if (o_ptr->obj_in_use)
		{
			*item = i;
			return (TRUE);
		}
	}

	/* Now look on the floor */
	for (i = 1; i < o_max; i++)
	{
		/* Get the object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Found it */
		if (o_ptr->obj_in_use)
		{
			*item = -i;
			return (TRUE);
		}

	}

	return (FALSE);
}



/*
 * Display the inventory.  Builds a list of objects and passes them
 * off to show_obj_list() for display.  Mode flags documented in
 * object.h
 */
void show_inven(byte mode)
{
	int i, last_slot = 0;

	object_type *o_ptr;

	int num_obj = 0;
	char labels[50][80];
	object_type *objects[50];

	bool in_term = (mode & OLIST_WINDOW) ? TRUE : FALSE;

	/* Find the last occupied inventory slot */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];
		if (o_ptr->k_idx) last_slot = i;
	}

	/* Build the object list */
	for (i = 0; i <= last_slot; i++)
	{
		o_ptr = &inventory[i];

		/* Acceptable items get a label */
		if (item_tester_okay(o_ptr, i))
			strnfmt(labels[num_obj], sizeof(labels[num_obj]), "%c) ", index_to_label(i));

		/* Unacceptable items are still displayed in term windows */
		else if (in_term)
			my_strcpy(labels[num_obj], "   ", sizeof(labels[num_obj]));

		/* Unacceptable items are skipped in the main window */
		else continue;

		/* Save the object */
		objects[num_obj] = o_ptr;
		num_obj++;
	}

	/* Display the object list */
	show_obj_list(num_obj, labels, objects, mode);
}

/*
 * Find the "first" inventory object with the given "tag".
 *
 * A "tag" is a char "n" appearing as "@n" anywhere in the
 * inscription of an object.
 *
 * Also, the tag "@xn" will work as well, where "n" is a tag-char,
 * and "x" is the "current" p_ptr->command_cmd code.
 */
static int get_tag(int *cp, char tag)
{
	int i;
	cptr s;

	/*
	 * The 'f'ire and 't'hrow commands behave differently when we are using the
	 * equipment (quiver)
	 */
	if (((p_ptr->command_cmd == 'f') || (p_ptr->command_cmd == 'v')) && (p_ptr->command_wrk == USE_EQUIP))
	{
		/* The pseudo-tag */
		byte tag_num = 0;
		object_type *o_ptr;
		byte group;

		/* Get the proper quiver group to determine which objects can be selected */
		if (p_ptr->command_cmd == 'f')
		{
			/* Ammo groups are taken from the missile weapon */
			switch (p_ptr->state.ammo_tval)
			{
				case TV_BOLT:	group = QUIVER_GROUP_BOLTS;	break;
				case TV_ARROW:	group = QUIVER_GROUP_ARROWS;	break;
				default:		group = QUIVER_GROUP_SHOTS;	break;
			}
		}
		/* Hack - Everything else is a throwing weapon */
		else
		{
		 	group = QUIVER_GROUP_THROWING_WEAPONS;
		}

		/* Iterate over the quiver */
		for (i = QUIVER_START; i < QUIVER_END; i++)
		{
			o_ptr = &inventory[i];

			/* (Paranoia) Ignore empty slots */
			if (!o_ptr->k_idx) continue;

			/* Groups must be equal */
			if (quiver_get_group(o_ptr) != group) continue;

			/* Allow pseudo-tag override */
			(void)get_tag_num(i, quiver_group[group].cmd, &tag_num);

			/* We have a match? */
			if (I2D(tag_num) == tag)
			{
				*cp = i;
				return TRUE;
			}

			/* Try with the next pseudo-tag */
			++tag_num;
		}
	}

	/* Check every object */
	for (i = 0; i < INVEN_TOTAL; ++i)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Skip empty inscriptions */
		if (!o_ptr->obj_note) continue;

		/* Don't check the swap weapon */
		if (adult_swap_weapons && (i == INVEN_SWAP_WEAPON)) continue;

		/* Find a '@' */
		s = strchr(quark_str(o_ptr->obj_note), '@');

		/* Process all tags */
		while (s)
		{
			/* Check the normal tags */
			if (s[1] == tag)
			{
				/* Save the actual inventory ID */
				*cp = i;

				/* Success */
				return (TRUE);
			}

			/* Check the special tags */
			if ((s[1] == p_ptr->command_cmd) && (s[2] == tag))
			{
				/* Save the actual inventory ID */
				*cp = i;

				/* Success */
				return (TRUE);
			}

			/* Find another '@' */
			s = strchr(s + 1, '@');
		}
	}

	/* No such tag */
	return (FALSE);
}



/*
 * Get the string that represents the pseudo-tag of the given quiver slot.
 * The color of the pseudo-tag is also obtained.
 * Returns the length of the pseudo-tag (0 on error).
 */
static int get_pseudo_tag(int slot, char tag[], int max_len, byte *color)
{
	byte tag_num = 0;
	object_type *o_ptr, *i_ptr;
	bool locked;
	int i;
	byte o_group;

	/* Paranoia */
	if (!IS_QUIVER_SLOT(slot)) return 0;

	/* Get the object */
	o_ptr = &inventory[slot];

	/* Paranoia */
	if (!o_ptr->k_idx) return 0;

	/* Get the group of the object */
	o_group = quiver_get_group(o_ptr);

	/* Check if the ammo is locked */
	locked = get_tag_num(slot, quiver_group[o_group].cmd, &tag_num);

	/* We calculate the pseudo-tag if there is not a real one */
	if (!locked)
	{
		/* Search the slots of the given ammo type */
		for (i = QUIVER_START; i < slot; i++)
		{
			byte i_group;

			/* Get the object */
			i_ptr = &inventory[i];

			/* Paranoia */
			if (!i_ptr->k_idx) continue;

			/* Get the group of the object */
			i_group = quiver_get_group(i_ptr);

			/* The groups must be equal */
			if (i_group != o_group) continue;

			/*
			 * A real tag overrides the current pseudo-tag when
			 * we have many locked ammo with the same tag
			 */
			(void)get_tag_num(i, quiver_group[i_group].cmd, &tag_num);

			/* But we always increment the pseudo-tag */
			++tag_num;
		}
	}

	/* Format the pseudo-tag */
	strnfmt(tag, max_len, "%s%c%d", (locked ? "@": ""), quiver_group[o_group].cmd, tag_num);

	/* Get the color of the group */
	*color = quiver_group[o_group].color;

	return strlen(tag);
}



/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
	register int i, n;
	object_type *o_ptr;
	byte attr;

	char tmp_val[80];

	char o_name[80];

	char ptag_desc[QUIVER_SIZE][10];
	byte ptag_len[QUIVER_SIZE];
	byte ptag_color[QUIVER_SIZE];
	byte max_ptag_len = 0;
	byte ptag_space;

	/* Get the pseudo-tags of the quiver slots */
	/* Calculate the maximum length of the pseudo-tags (for alignment) */
	for (i = QUIVER_START; i < QUIVER_END; i++)
	{
		/* The index in the temporary arrays */
		int q = i - QUIVER_START;

		/* Paranoia */
		ptag_len[q] = 0;

		/* Get the object */
		o_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!o_ptr->k_idx) continue;

		/* Store pseudo-tag data in the arrays */
		ptag_len[q] = get_pseudo_tag(i, ptag_desc[q],
			sizeof(ptag_desc[q]), &ptag_color[q]);

		/* Update the maximum length if necessary */
		if (ptag_len[q] > max_ptag_len)
		{
			max_ptag_len = ptag_len[q];
		}
	}

	/* Display the equipment */
	for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
	{

		/* Examine the item */
		o_ptr = &inventory[i];

		/* Hack -- Never show empty quiver slots */
		if (!o_ptr->k_idx && IS_QUIVER_SLOT(i))
		{
			/* Clear that line */
			Term_erase(0, i - INVEN_WIELD, 255);
			continue;
		}

		/* Hack -- Never show the gap between equipment and quiver */
		if (i == INVEN_TOTAL)
		{
			/* Clear that line */
			Term_erase(0, i - INVEN_WIELD, 255);
			continue;
		}

		/* Start with an empty "index" */
		tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

		/* Is this item "acceptable"? */
		if (item_tester_okay(o_ptr, i))
		{
			/* Prepare an "index" */
			tmp_val[0] = index_to_label(i);

			/* Bracket the "index" --(-- */
			tmp_val[1] = ')';
		}

		/* Display the index (or blank space) */
		Term_putstr(0, i - INVEN_WIELD, 3, TERM_WHITE, tmp_val);

		/* Obtain an item description */
		object_desc(o_name, sizeof(o_name), o_ptr, (ODESC_PREFIX | ODESC_FULL));

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Regular slots don't have a pseudo-tag */
		ptag_space = 0;

		/* Show quiver slot pseudo-tag if needed */
		if (IS_QUIVER_SLOT(i) &&
			(ptag_len[i - QUIVER_START] > 0))
		{
			/* The index in the temporary arrays */
			int q = i - QUIVER_START;

			/* Reserve space for the pseudo-tag in this case */
			ptag_space = max_ptag_len + 1;

			/* Hack -- Clear that space first */
			Term_erase(3, i - INVEN_WIELD, ptag_space);

			/* Show the pseudo-tag */
			Term_putstr(3 + max_ptag_len - ptag_len[q],
				i - INVEN_WIELD, ptag_len[q],
				ptag_color[q], ptag_desc[q]);
		}

		/* Display the entry itself */
		Term_putstr(3 + ptag_space , i - INVEN_WIELD, n, attr, o_name);

		/* Erase the rest of the line */
		Term_erase(3 + ptag_space + n, i - INVEN_WIELD, 255);

		/* Display the slot description (if needed) */
		Term_putstr(61, i - INVEN_WIELD, -1, TERM_WHITE, "<-- ");

		Term_putstr(65, i - INVEN_WIELD, -1, TERM_WHITE, mention_use(i));

		/* Display the weight (if needed) */
		if (o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			int col = 52;
			sprintf(tmp_val, "%3d.%1d lb ", wgt / 10, wgt % 10);
			Term_putstr(col, i - INVEN_WIELD, -1, TERM_WHITE, tmp_val);
		}
	}

	/* Erase the rest of the window */
	for (i = ALL_INVEN_TOTAL - INVEN_WIELD; i < Term->hgt; i++)
	{
		/* Clear that line */
		Term_erase(0, i, 255);
	}
}


/*
 * Display the equipment.  Builds a list of objects and passes them
 * off to show_obj_list() for display.  Mode flags documented in
 * object.h
 */
void show_equip(byte mode)
{
	int i, last_slot = 0;

	object_type *o_ptr;

   int num_obj = 0;
   char labels[50][80];
   object_type *objects[50];

	char tmp_val[80];

   bool in_term = (mode & OLIST_WINDOW) ? TRUE : FALSE;

	/* Find the last equipment slot to display */
	for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];
		if (i < INVEN_TOTAL || o_ptr->k_idx) last_slot = i;
	}

	/* Build the object list */
	for (i = INVEN_WIELD; i <= last_slot; i++)
	{
		o_ptr = &inventory[i];

		/* May need a blank line to separate the quiver */
		if (i == INVEN_TOTAL)
		{
			int j;
			bool need_spacer = FALSE;

			/* Scan the rest of the items for acceptable entries */
			for (j = i; j < last_slot; j++)
			{
				o_ptr = &inventory[j];
				if (item_tester_okay(o_ptr, i)) need_spacer = TRUE;
			}

			/* Add a spacer between equipment and quiver */
			if (num_obj > 0 && need_spacer)
			{
				my_strcpy(labels[num_obj], "", sizeof(labels[num_obj]));
				objects[num_obj] = NULL;
				num_obj++;
			}

			continue;
		}

		/* Acceptable items get a label */
		if (item_tester_okay(o_ptr, i))
			strnfmt(labels[num_obj], sizeof(labels[num_obj]), "%c) ", index_to_label(i));

		/* Unacceptable items are still displayed in term windows */
		else if (in_term)
			my_strcpy(labels[num_obj], "   ", sizeof(labels[num_obj]));

		/* Unacceptable items are skipped in the main window */
		else continue;

		/* Show short quiver labels */
		if (i >= QUIVER_START)
		{
			strnfmt(tmp_val, sizeof(tmp_val), "[f%d]: ", i - QUIVER_START);
			my_strcat(labels[num_obj], tmp_val, sizeof(labels[num_obj]));
		}

		else
		{
			strnfmt(tmp_val, sizeof(tmp_val), "%-14s: ", mention_use(i));
			my_strcat(labels[num_obj], tmp_val, sizeof(labels[num_obj]));
		}

		/* Save the object */
		objects[num_obj] = o_ptr;
		num_obj++;
	}

	/* Display the object list */
	show_obj_list(num_obj, labels, objects, mode);
}


/*
 * Display the floor.  Builds a list of objects and passes them
 * off to show_obj_list() for display.  Mode flags documented in
 * object.h
 */
void show_floor(const int *floor_list, int floor_num, byte mode)
{
	int i;

	object_type *o_ptr;

   int num_obj = 0;
   char labels[50][80];
   object_type *objects[50];

	if (floor_num > MAX_FLOOR_STACK) floor_num = MAX_FLOOR_STACK;

	/* Build the object list */
	for (i = 0; i < floor_num; i++)
	{
		o_ptr = &o_list[floor_list[i]];

		/* Tester always skips gold. When gold should be displayed,
		 * only test items that are not gold.
		 */
		if ((o_ptr->tval != TV_GOLD || !(mode & OLIST_GOLD)) &&
		    !item_tester_okay(o_ptr, i))
			continue;

		strnfmt(labels[num_obj], sizeof(labels[num_obj]),
		        "%c) ", index_to_label(i));

		/* Save the object */
		objects[num_obj] = o_ptr;
		num_obj++;
	}

	/* Display the object list */
	show_obj_list(num_obj, labels, objects, mode);
}


/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify_item(int item)
{
	char o_name[80];
	cptr prompt;
	char out_val[160];

	object_type *o_ptr;

	/* Inventory */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Floor */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Get the possible command prompts */
	switch (p_ptr->command_cmd)
	{
		case 'a':	{prompt = "Really aim";		break;}
		case 'A':	{prompt = "Really activate";break;}
		case 'd':	{prompt = "Really drop";	break;}
		case 'D':	{prompt = "Really disarm";	break;}
		case 'E':	{prompt = "Really eat";		break;}
		case 'f':	{prompt = "Really fire";	break;}
		case 'F':	{prompt = "Really fuel";	break;}
		case 'h':	{prompt = "Really fire";	break;}
		case 'k':	{prompt = "Really destroy";	break;}
		case 'o':	{prompt = "Really open";	break;}
		case 'q':	{prompt = "Really quaff";	break;}
		case 'r':	{prompt = "Really read";	break;}
		case 't':	{prompt = "Really take off";break;}
		case 'u':	{prompt = "Really use";		break;}
		case 'v':	{prompt = "Really throw";	break;}
		case 'w':
		{
			int slot = wield_slot(o_ptr);

			/* Where would the item go? INVEN_MAIN_WEAPON */
			if (slot == INVEN_WIELD)
			{
				if (obj_is_bow(o_ptr)) prompt = "Really shoot with";
				else prompt = "Really wield";
			}
			else if (slot == INVEN_BOW) 	prompt = "Really shoot with";
			else if ((slot == INVEN_LIGHT)	|| (slot == INVEN_ARM))
			{
				prompt = "Really hold";
			}
			else if ((slot == INVEN_LEFT) || (slot == INVEN_RIGHT) || (slot == INVEN_NECK))
			{
				prompt = "Really put on";
			}
			else if (slot >= QUIVER_START) prompt = "Really place in quiver";
			else prompt = "Really wear";

			break;

		}
		case 'z':	{prompt = "Really zap";		break;}
		default: 	{prompt = "Really try"; 	break;}
	}

	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Prompt */
	strnfmt(out_val, sizeof(out_val), "%s %s? ", prompt, o_name);

	/* Query */
	return (get_check(out_val));
}



/*
 * Hack -- allow user to "prevent" certain choices.
 *
 * The item can be negative to mean "item on floor".
 */
static bool get_item_allow(int item, bool is_harmless)
{
	object_type *o_ptr;
	char verify_inscrip[] = "!*";

	unsigned n;

	/* Inventory or floor */
	if (item >= 0)
		o_ptr = &inventory[item];
	else
		o_ptr = &o_list[0 - item];

	/* Check for a "prevention" inscription */
	verify_inscrip[1] = p_ptr->command_cmd;

	/* Find both sets of inscriptions, add together, and prompt that number of times */
	n = check_for_inscrip(o_ptr, verify_inscrip);

	if (!is_harmless)
		n += check_for_inscrip(o_ptr, "!*");

	while (n--)
	{
		if (!verify_item(item))
			return (FALSE);
	}

	/* Allow it */
	return (TRUE);
}





/* Arrays for inventory, equipment and floor */
int i1, i2, e1, e2, f1, f2, q1, q2;
int inven_count = 0;
int equip_count = 0;
int floor_count = 0;
int quiver_count = 0;
int equip_items[INVEN_WIELD];
int	inven_items[ALL_INVEN_TOTAL - INVEN_WIELD];
int floor_items[MAX_FLOOR_STACK];
int quiver_items[QUIVER_SIZE];
char header_val[160];


/**
 * Make the correct prompt for items, handle mouse buttons
 */
static void item_prompt(menu_type *menu, int mode, cptr pmt)
{
	bool use_inven   = ((mode & (USE_INVEN))   ? TRUE : FALSE);
	bool use_equip   = ((mode & (USE_EQUIP))   ? TRUE : FALSE);

	char tmp_val[160];
	char out_val[160];

	/* Viewing inventory */
	if (p_ptr->command_wrk == (USE_INVEN))
	{
		/* Begin the prompt */
    	sprintf(out_val, "    Inven:");

    	/* Indicate lack of inventory choices. */
    	if (i1 > i2)
    	{
    		sprintf(tmp_val, " (none),");
    	}

    	/* List choices. */
    	else sprintf(tmp_val, " %c-%c,", index_to_label(i1), index_to_label(i2));

    	/* Append choices. */
    	strcat(out_val, tmp_val);

    	/* Indicate ability to "view" */
    	if (!p_ptr->command_see)
    	{
    		strcat(out_val, " * to see,");
    		button_add("[VIEW]", '*');
    	}
    	else
    	{
    		strcat(out_val, " * to hide,");
    		button_add("[HIDE]", '*');
    	}

    	/* Indicate that equipment items are available */
    	if (use_equip)
    	{
    		strcat(out_val, " / equip,");
    		button_add("[EQUIP]", '/');
    	}

    	/* Indicate that floor items are available */
    	if (f1 <= f2)
    	{
    		strcat(out_val, " - floor, . floor top,");
    		button_add("[FLOOR]", '-');
    		button_add(".", '.');
    	}
	}

	/* Viewing equipment */
	else if (p_ptr->command_wrk == (USE_EQUIP))
	{
		/* Begin the prompt */
		sprintf(out_val, "Equip:");

		/* Indicate lack of equipment choices. */
		if (e1 > e2)
		{
			sprintf(tmp_val, " (none),");
		}

		/* List choices. */
		else sprintf(tmp_val, " %c-%c,", index_to_label(e1), index_to_label(e2));

		/* Append choices. */
		strcat(out_val, tmp_val);

		/* Indicate ability to "view" */
		if (!p_ptr->command_see)
		{
			strcat(out_val, " * to see,");
			button_add("[VIEW]", '*');
		}
		else
		{
			strcat(out_val, " * to hide,");
			button_add("[HIDE]", '*');
		}

		/* Append */
		if (use_inven)
		{
			strcat(out_val, " / inven,");
			button_add("[INVEN]", '/');
		}

		/* Append */
		if (f1 <= f2)
		{
			strcat(out_val, " - floor, . floor top,");
			button_add("[FLOOR]", '-');
			button_add(".", '.');
		}
	}

	/* Viewing floor */
	else if (p_ptr->command_wrk == (USE_FLOOR))
	{
		/* Begin the prompt */
		sprintf(out_val, "Floor:");

		/* Indicate lack of floor choices. */
		if (f1 > f2) sprintf(tmp_val, " (none),");

		/* List choices. */
		else sprintf(tmp_val, " %c-%c,", I2A(f1-f1), I2A(f2-f1));

		/* Append */
		strcat(out_val, tmp_val);

		/* Indicate ability to "view" */
		if (!p_ptr->command_see)
		{
			strcat(out_val, " * to see,");
			button_add("[VIEW]", '*');
		}
		else
		{
			strcat(out_val, " * to hide,");
		    button_add("[HIDE]", '*');
		}

		/* Append */
		if (use_inven)
		{
			strcat(out_val, " / inven,");
			button_add("[INVEN]", '/');
		}
		else if (use_equip)
		{
			strcat(out_val, " / equip,");
			button_add("[EQUIP]", '/');
		}

	}

 	/* Do the buttons */
	event_signal(EVENT_MOUSEBUTTONS);

	/* Finish the prompt */
	strcat(out_val, " ESC");

	my_strcpy(header_val, out_val, sizeof(header_val));
}




/**
 * Item selection menus
 */
static char get_item_tag(menu_type *menu, int oid)
{
	const int *choice = menu->menu_data;
	int idx = choice[oid];

	if (p_ptr->command_wrk == USE_FLOOR)  return I2A(oid);

	else return index_to_label(idx);

}

#define POUND_LENGTH   15

static void get_item_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	const int *choice = menu->menu_data;
	int idx = choice[oid];
	char o_name[200];
	char label;
	int weight;
	char tmp_val[30];
	u16b i, length, o_length;

	byte attr = TERM_WHITE;

	object_type *o_ptr;

	/* Get the object - hack to abort if invalid */
	if (p_ptr->command_wrk == (USE_INVEN))
	{
		o_ptr = &inventory[idx];
		if (!get_item_okay(idx)) return;
	}
	else if (p_ptr->command_wrk == (USE_EQUIP))
	{
		o_ptr = &inventory[idx];
		if (!get_item_okay(idx)) return;

		if (idx >= QUIVER_START)
		{
			attr = TERM_L_UMBER;
		}
	}
	else
	{
		o_ptr = &o_list[idx];
		if (!get_item_okay(0 - idx)) return;
	}

	/* Get the color, unless it is the quiver (above).*/
	if (attr == TERM_WHITE) attr = tval_to_attr[o_ptr->tval & 0x7F];
	if (cursor) attr = TERM_NAVY_BLUE;


	/* Get the object description */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Boundary control.  Ensure no memory leaks below during the for loop*/
	length = strlen(o_name);
	o_length = width - POUND_LENGTH;

	/* Terminate the string */
	o_name[o_length] = '\0';

	/* add spaces to o_mname, ensuring memory leaks */
	for (i = length; i < o_length; i++)
	{
		my_strcat(o_name," ", sizeof(o_name));
	}

	weight = o_ptr->weight * o_ptr->number;
	strnfmt(tmp_val, sizeof(tmp_val), "%4d.%1d lb", weight / 10, weight % 10);
	my_strcat(o_name, tmp_val, sizeof(o_name));

	/* Hack - re-print the label with the right color, code taken from get_item_tag above*/
	label = get_item_tag(menu, oid);

	c_put_str(attr, format("%c)",label), row, (col-3));

	/* Now print the object  */
	c_put_str(attr, o_name, row, col);


}



/* Print out the item description of the highlighted object */
static void item_menu_hook(int oid, void *db, const region *loc)
{
	const int *choice = (const int *) db;
	int idx = choice[oid];
	char out_val[1024];
	int max_output;
	bool started = FALSE;

	/* Get the object */
	object_type *o_ptr;

	/* Make sure the player has drop down lists turned on */
	if (!p_ptr->command_see) return;

	/* Not displaying full menu */
	if (loc->page_rows == 1) return;

	/* Get the object, handle whether in Inventory or on floor */
	if (p_ptr->command_wrk == (USE_FLOOR)) o_ptr = &o_list[idx];
	else o_ptr = &inventory[idx];

	prt("", loc->row + loc->page_rows, loc->col);
	prt("", loc->row + loc->page_rows + 1, loc->col);
	prt("", loc->row + loc->page_rows + 2, loc->col);
	prt("", loc->row + loc->page_rows + 3, loc->col);

	/* No info until they know about the item */
	if (!object_is_known(o_ptr)) return;

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Indent output */
	text_out_indent = loc->col;
	text_out_wrap = loc->col + loc->width - 2;

	/* Display the known artifact description */
	if (!adult_rand_artifacts && o_ptr->art_num &&
		    object_known_p(o_ptr) && a_info[o_ptr->art_num].text)
	{
		my_strcpy(out_val, a_text + a_info[o_ptr->art_num].text, sizeof(out_val));
		started = TRUE;
	}
	/* Display the known object description */
	else if (object_aware_p(o_ptr) || object_known_p(o_ptr))
	{
		if (k_info[o_ptr->k_idx].text)
		{
			my_strcpy(out_val, k_text + k_info[o_ptr->k_idx].text, sizeof(out_val));
			started = TRUE;
		}

		/* Display an additional ego-item description */
		if (o_ptr->ego_num && object_known_p(o_ptr) && e_info[o_ptr->ego_num].text)
		{
			if (started)
			{
				my_strcat(out_val, "   ", sizeof(out_val));
				my_strcat(out_val, e_text + e_info[o_ptr->ego_num].text, sizeof(out_val));
			}
			else
			{
				my_strcpy(out_val, e_text + e_info[o_ptr->ego_num].text, sizeof(out_val));
				started = TRUE;
			}
		}
	}

	/* Now (crudely) terminate it so it doesn't go any longer than four lines, */
	max_output = ((text_out_wrap - text_out_indent) * 4) - 30;
	out_val[max_output] = '\0';

	Term_gotoxy(loc->col, loc->row + loc->page_rows);

	/* Nothing to print */
	if (!started) return;

	text_out_c(TERM_L_BLUE, out_val);
	text_out_indent = 0;
}


/**
 * Deal with events on the get_item menu
 */
static bool get_item_action(char cmd, void *db, int oid)
{
	return TRUE;
}


/**
 * Display list items to choose from
 */
bool item_menu(int *cp, cptr pmt, int mode, bool *oops, int sq_y, int sq_x)
{
	menu_type menu;
	menu_iter menu_f = {get_item_tag, NULL, get_item_display, get_item_action };
	region area = { 0, 0, -1, -1 };
	ui_event_data evt = { EVT_NONE, 0, 0, 0, 0 };
	int num_entries;
	bool done;

	int j, k = 0;

	bool refresh = TRUE;

	bool use_inven  = ((mode & (USE_INVEN)) ? TRUE : FALSE);
	bool use_equip  = ((mode & (USE_EQUIP)) ? TRUE : FALSE);
	bool use_floor  = ((mode & (USE_FLOOR)) ? TRUE : FALSE);
	bool use_quiver = ((mode & (USE_QUIVER)) ? TRUE : FALSE);
	bool allow_quiver = FALSE;
	bool allow_equip = FALSE;
	bool allow_inven = FALSE;
	bool allow_floor = FALSE;
	int cursor = 0;
	bool toggle = FALSE;

	int floor_list[24];
	int floor_num;

	/* FIX LATER */
	int len = 70;

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);

	/* Not done */
	done = FALSE;

	/* Mega-hack -- show lists */
	if ((auto_display_lists) || (mode & (NOUN_VERB))) p_ptr->command_see = TRUE;

	/* Full inventory */
	i1 = 0;
	i2 = INVEN_WIELD - 1;

	/* Forbid inventory */
	if (!use_inven) i2 = -1;

	/* Restrict inventory indexes */
	while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
	while ((i1 <= i2) && (!get_item_okay(i2))) i2--;

	/* Accept inventory */
	if (i1 <= i2)
	{
		allow_inven = TRUE;

		/* Record them */
		for (inven_count = 0, j = i1; j <= i2; j++)
		{
			if (get_item_okay(j)) inven_items[inven_count++] = j;
		}
	}

	/*
	 * equipment & quiver
	 * Figure out where we want to start
	 * can be equipment, quiver, both, or neither
	 */

	if (use_equip) e1 = INVEN_WIELD;
	else e1 = QUIVER_START;

	/* Now figure out where we want to end */
	if (use_quiver) e2 = ALL_INVEN_TOTAL - 1;
	else e2 = INVEN_TOTAL - 1;

	/* Neither equipment or quiver */
	if ((!use_equip) && (!use_quiver)) e2 = -1;

	/* Restrict equipment indexes */
	while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
	while ((e1 <= e2) && (!get_item_okay(e2))) e2--;

	/* Accept equipment */
	if (e1 <= e2)
	{
		allow_equip = TRUE;

		/* Record them */
		for (equip_count = 0, quiver_count = 0, j = e1; j <= e2; j++)
		{
			if (get_item_okay(j))
			{
				equip_items[equip_count++] = j;
				if (IS_QUIVER_SLOT(j))
				{
					allow_quiver = TRUE;
					quiver_items[quiver_count++] = j;
				}
			}
		}
	}

	/* Count "okay" floor items */
	floor_num = 0;

	/* Restrict floor usage */
	if (mode & (USE_FLOOR))
	{
		/* Scan all objects in the grid */
		floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, sq_y, sq_x, 0x03);
	}

	/* Full floor */
	f1 = 0;
	f2 = floor_num - 1;

	/* Forbid floor */
	if (!use_floor) f2 = -1;

	/* Restrict floor indexes */
	while ((f1 <= f2) && (!get_item_okay(0 - floor_list[f1]))) f1++;
	while ((f1 <= f2) && (!get_item_okay(0 - floor_list[f2]))) f2--;

	/* Accept floor */
	if (f1 <= f2)
	{
		allow_floor = TRUE;

		/* Record them */
		for (floor_count = 0, j = f1; j <= f2; j++)
		{
			if (get_item_okay(0 - floor_list[j])) floor_items[floor_count++] = floor_list[j];
		}
	}

	/* Require at least one legal choice */
	if (!allow_inven && !allow_equip && !allow_floor && !allow_quiver)
	{
		/* Cancel p_ptr->command_see */
		p_ptr->command_see = FALSE;

		/* Report failure */
		*oops = TRUE;

		/* Done here */
		return FALSE;
	}

	/* Assume we'll be looking at something*/
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- Start on quiver if shooting or throwing */
	if ((mode & (QUIVER_FIRST)) && use_quiver && allow_quiver)
	{
		p_ptr->command_wrk = (USE_EQUIP);
	}
	/* Hack -- Start on equipment if requested */
	else if ((mode == (USE_EQUIP)) && use_equip)
	{
		p_ptr->command_wrk = (USE_EQUIP);
	}

	/* Use inventory if allowed. */
	else if (allow_inven)
	{
		p_ptr->command_wrk = (USE_INVEN);
	}

	/* Use equipment if allowed */
	else if (allow_equip)
	{
		p_ptr->command_wrk = (USE_EQUIP);
	}

	/* Use floor if allowed */
	else if (allow_floor)
	{
		p_ptr->command_wrk = (USE_FLOOR);
	}
	/* Hack -- Use (empty) inventory if no other choices available. */
	else
	{
		p_ptr->command_wrk = (USE_INVEN);
	}

	/* Find the column to start in */
	if ((Term->wid - len + 1) < 12 ) area.col = 12;
	else area.col = Term->wid - (len + 1);

	/* Save the screen */
	screen_save();

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.browse_hook = item_menu_hook;
	menu.cmd_keys = "\n\r";

	/* Clear space */
	area.width = len;

	/* Play until item selected or refused */
	while (!done)
	{
		int ni = 0;
		int ne = 0;

		/* Scan windows */
		for (j = 0; j < ANGBAND_TERM_MAX; j++)
		{
			/* Unused */
			if (!angband_term[j]) continue;

			/* Count windows displaying inven */
			if (op_ptr->window_flag[j] & (PW_INVEN)) ni++;

			/* Count windows displaying equip */
			if (op_ptr->window_flag[j] & (PW_EQUIP)) ne++;
		}

		/* Toggle if needed */
		if ((((p_ptr->command_wrk == (USE_EQUIP)) && ni && !ne) ||
			 ((p_ptr->command_wrk == (USE_INVEN)) && !ni && ne)) &&
			  (p_ptr->command_see))
		{
			/* Toggle */
			toggle_inven_equip();

			/* Track toggles */
			toggle = !toggle;
		}

		/* Update */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

		/* Redraw windows */
		redraw_stuff();
		event_signal(EVENT_MOUSEBUTTONS);

		/* Change the display if needed */
		if (refresh)
		{
			/* Hack - load the screen and re-save */
			screen_load();
			screen_save();

			/* Pick the right menu */
			if (p_ptr->command_wrk == (USE_INVEN))
			{
				menu.menu_data = inven_items;
				num_entries = inven_count;
			}
			else if ((p_ptr->command_wrk == (USE_EQUIP)) && use_quiver && (!use_inven) && allow_quiver &&
					 (mode & (QUIVER_FIRST)))
			{
				menu.menu_data = quiver_items;
				num_entries = quiver_count;
			}
			else if (p_ptr->command_wrk == (USE_FLOOR))
			{
				menu.menu_data = floor_items;
				num_entries = floor_count;
			}
			else /* if (p_ptr->command_wrk == (USE_EQUIP)) */
			{
				menu.menu_data = equip_items;
				num_entries = equip_count;
			}

			/* Different menu sizes depending on if the objects are listed or not */
			if (!p_ptr->command_see)
			{
				area.page_rows = 1;
				menu.count = 1;
			}
			else
			{
				area.page_rows = num_entries + 3;
				menu.count = num_entries;
			}

			/* Set the prompt */
			item_prompt(&menu, mode, pmt);

			menu.title = header_val;

			menu_init(&menu, MN_SKIN_SCROLL, &menu_f, &area);

			refresh = FALSE;
		}

		evt = menu_select(&menu, &cursor, EVT_MOVE | EVT_KBRD);

		switch(evt.type)
		{
			case EVT_KBRD:
			{
				break;
			}

			case EVT_ESCAPE:
			{
				done = TRUE;
				continue;
			}

			case EVT_SELECT:
			{
				int *tmp = (int *) menu.menu_data;
				if (p_ptr->command_wrk == (USE_FLOOR))	k = 0 - tmp[evt.index];
				else k = tmp[evt.index];

				/* Paranoia */
				if (!get_item_okay(k))	continue;

				(*cp) = k;
				done = TRUE;
				continue;
			}

			case EVT_MOVE:
			{
				continue;
			}

			case EVT_BACK:
			{
				done = TRUE;
			}

			default:
			{
				continue;
			}
		}

		switch (evt.key)
		{
			case '*':case ' ':
			{
				/* Hide the list */
				if (p_ptr->command_see)
				{
					/* Flip flag */
					p_ptr->command_see = FALSE;
				}

				/* Show the list */
				else
				{
					/* Flip flag */
					p_ptr->command_see = TRUE;
				}
				refresh = TRUE;
				break;
			}
			case '-':
			{
				if (!allow_floor)
				{
					bell("Cannot select floor!");
					break;
				}

				/*
				 * If we aren't examining the floor and there is only
				 * one item, we will select it if floor_query_flag
				 * is FALSE.
				 */
				/* Hack -- Auto-Select */
				if ((!floor_query_flag) && (floor_num == 1))
				{
					/* Fall through */
				}
				else
				{
					p_ptr->command_wrk = (USE_FLOOR);
					refresh = TRUE;
					break;
				}
			}
			case '.':
			{
				/*
				 * If we are allow to use the floor, select
				 * the top item. -BR-
				 */
				if (allow_floor)
				{
					int k;

					/* Special index */
					k = 0 - floor_list[0];

					/* Allow player to "refuse" certain actions */
					if (!get_item_allow(k, FALSE))
					{
						done = TRUE;
						break;
					}

					/* Accept that choice */
					(*cp) = k;
					done = TRUE;
				}

				break;
			}
			case '/':
			{
				/* Toggle to inventory */
				if (allow_inven && (p_ptr->command_wrk != (USE_INVEN)))
				{
					p_ptr->command_wrk = (USE_INVEN);
					refresh = TRUE;
				}

				/* Toggle to equipment */
				else if (allow_equip && (p_ptr->command_wrk != (USE_EQUIP)))
				{
				p_ptr->command_wrk = (USE_EQUIP);
				refresh = TRUE;
				}

				/* No toggle allowed */
				else
				{
					bell("Cannot switch item selector!");
				}

				break;
			}
			case ESCAPE:
			{
				evt.type = EVT_ESCAPE;
				done = TRUE;
				break;
			}

			case '0':
			case '1': case '2': case '3':
			case '4': case '5': case '6':
			case '7': case '8': case '9':
			{
				/* Look up the tag */
				if (!get_tag(&k, evt.key))
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Hack -- Validate the item */
				if ((k < INVEN_WIELD) ? !allow_inven : !allow_equip)
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Forbid classic equipment if using the quiver */
				if (use_quiver && (k >= INVEN_WIELD) && !IS_QUIVER_SLOT(k))
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Validate the item */
				if (!get_item_okay(k))
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Accept that choice */
				(*cp) = k;
				done = TRUE;
				break;
			}

			default:
			{
				bool verify;

				/* Note verify */
				verify = (isupper(evt.key) ? TRUE : FALSE);

				/* Lowercase */
				evt.key = tolower(evt.key);

				/* Convert letter to inventory index */
				if (p_ptr->command_wrk == (USE_INVEN))
				{
					k = label_to_inven(evt.key);

					if (k < 0)
					{
						bell("Illegal object choice (inven)!");
						break;
					}
				}

				/* Convert letter to equipment index */
				else if (p_ptr->command_wrk == (USE_EQUIP))
				{
					k = label_to_equip(evt.key);

					if (k < 0)
					{
						bell("Illegal object choice (equip)!");
						break;
					}

					/* Forbid classic equipment if using the quiver */
					if (use_quiver && !IS_QUIVER_SLOT(k))
					{
						bell("Illegal object choice (equip)!");
						break;
					}
				}

				/* Convert letter to floor index */
				else
				{
					k = (islower((unsigned char)evt.key) ? A2I(evt.key) : -1);

					if (k < 0 || k >= floor_num)
					{
						bell("Illegal object choice (floor)!");
						break;
					}

					/* Special index */
					k = 0 - floor_list[k];
				}

				/* Validate the item */
				if (!get_item_okay(k))
				{
					bell("Illegal object choice (normal)!");
					break;
				}

				/* Verify the item */
				if (verify && !verify_item(k))
				{
					done = TRUE;
					evt.type = EVT_ESCAPE;
					break;
				}

				/* Accept that choice */
				(*cp) = k;
				done = TRUE;
				break;
			}
		}
	}

	/* Load screen */
	screen_load();

	/* Kill buttons */
	button_kill('*');
	button_kill('/');
	button_kill('-');
	button_kill('.');
	button_kill('!');
	event_signal(EVENT_MOUSEBUTTONS);

	/* Clean up */
	if ((auto_display_lists) || (mode & NOUN_VERB))
	{
		/* Toggle again if needed */
		if (toggle) toggle_inven_equip();

		/* Update */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Window stuff */
		redraw_stuff();
	}

	return ((evt.type != EVT_ESCAPE) && (evt.type != EVT_BACK));
}

/**
 *
 * This code was taken from FAAngband v1.6, Modified for NPP
 *
 * FAAngband notes are below:
 *
 * Let the user select an item, save its "index"
 *
 * Return TRUE only if an acceptable item was chosen by the user.
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.
 *
 * Any of these are displayed (even if no acceptable items are in that
 * location) if the proper flag was given.
 *
 * If there are no acceptable items available anywhere, and "str" is
 * not NULL, then it will be used as the text of a warning message
 * before the function returns.
 *
 * Note that the user must press "-" to specify the item on the floor.  The
 * use of "capital" letters will "examine" an inventory, equipment, or floor
 * item, and prompt for its use.
 *
 * If a legal item is selected from the inventory, we save it in "cp"
 * directly (0 to 35), and return TRUE.
 *
 * If a legal item is selected from the floor, we save it in "cp" as
 * a negative (-1 to -511), and return TRUE.
 *
 * If no item is available, we do nothing to "cp", and we display a
 * warning message, using "str" if available, and return FALSE.
 *
 * If no item is selected, we do nothing to "cp", and return FALSE.
 *
 * If 'all squelched items' are selected we set cp to ALL_SQUELCHED and return
 * TRUE.
 *
 * Global "p_ptr->command_new" is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * Global "p_ptr->command_see" may be set before calling this function to start
 * out in "browse" mode.  It is cleared before this function returns.
 *
 * Global "p_ptr->command_wrk" is used to choose between equip/inven/floor
 * listings.  It is equal to USE_INVEN or USE_EQUIP or USE_FLOOR, except
 * when this function is first called, when it is equal to zero, which will
 * cause it to be set to USE_INVEN.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 *
 * Note that only "acceptable" floor objects get indexes, so between two
 * commands, the indexes of floor objects may change.  XXX XXX XXX
 *
 * This function has been revised using code from Tim Baker's Easy Patch 1.2
 *
 * This function has been largely rewritten for FAangband 0.3.2 using
 * Pete Mack's menu code.
 *
 * Assumes the item is on the player square
 */
bool get_item(int *cp, cptr pmt, cptr str, int mode)
{
	bool item;

	bool oops = FALSE;

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);

	/* No item selected */
	item = FALSE;
	*cp = 0;

	/* Go to menu */
	item = item_menu(cp, pmt, mode, &oops, p_ptr->py, p_ptr->px);

	/* Check validity */
	if (item)
	{
		if (!get_item_allow(*cp, FALSE))
		{
			item = FALSE;
			msg_print(NULL);
		}
	}

	/* Hack -- Cancel "display" */
	p_ptr->command_see = FALSE;

	/* Forget the item_tester_tval restriction */
	item_tester_tval = 0;

	/* Forget the item_tester_hook restriction */
	item_tester_hook = NULL;

	/* Forget the item tester_swap restriction */
	item_tester_swap = FALSE;

	/* Make sure the equipment/inventory windows are up to date */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	/* Clear the prompt line */
	prt("", 0, 0);

	/* Warning if needed */
	if (oops && str) msg_print(str);

	/* Result */
	return (item);
}

/*
* Same notes as above for get_item, except this is used for squares other than the one the player is on.
* This can be used on any square on the map, but it is intended for
* disarming opening chests on adjacent squares.
*/
bool get_item_beside(int *cp, cptr pmt, cptr str, int sq_y, int sq_x)
{
	bool item;

	bool oops = FALSE;

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);

	/* No item selected */
	item = FALSE;
	*cp = 0;

	/* Paranoia */
	if (!in_bounds_fully(sq_y, sq_x)) oops = TRUE;

	/* Go to menu */
	else item = item_menu(cp, pmt, (USE_FLOOR), &oops, sq_y, sq_x);

	/* Check validity */
	if (item)
	{
		if (!get_item_allow(*cp, TRUE))
		{
			item = FALSE;
			msg_print(NULL);
		}
	}

	/* Hack -- Cancel "display" */
	p_ptr->command_see = FALSE;

	/* Forget the item_tester_tval restriction */
	item_tester_tval = 0;

	/* Forger the item tester_swap restriction */
	item_tester_swap = FALSE;

	/* Forget the item_tester_hook restriction */
	item_tester_hook = NULL;

	/* Clear the prompt line */
	prt("", 0, 0);

	/* Warning if needed */
	if (oops && str) msg_print(str);

	/* Result */
	return (item);
}


/**
 * Menu functions
 */
char comm[30];
cmd_code comm_command[30];
cptr comm_descr[30];
int poss;




/*
 * Holds a generic command - if cmd is set to other than CMD_NULL
 * it simply pushes that command to the game, otherwise the hook
 * function will be called.
 */
typedef struct
{
	cmd_code command;
	const char *desc;
	unsigned char key;
	bool aim;
	bool quantity;
} object_command;

/* General actions, note inscribe should come first, see comments in find_command_line */
static object_command command_actions[] =
{
	{CMD_INSCRIBE, 		"Inscribe ", 				'{', 	FALSE, 	FALSE},
	{CMD_UNINSCRIBE, 	"Uninscribe ", 				'}', 	FALSE, 	FALSE},
	{CMD_TAKEOFF, 		"Takeoff/unwield ", 		't', 	FALSE, 	FALSE},
	{CMD_WIELD, 		"Wear/wield ",      		'w', 	FALSE, 	FALSE},
	{CMD_DROP, 			"Drop ",            		'd', 	FALSE, 	TRUE},
	{CMD_STUDY_SPELL, 	"Gain new spell",			'G', 	FALSE, 	FALSE},
	{CMD_STUDY_BOOK, 	"Gain new prayer", 			'G', 	FALSE, 	FALSE},
	{CMD_STUDY_INCAN, 	"Gain new incantation",		'G', 	FALSE, 	FALSE},
	{CMD_BROWSE, 		"Browse ",            		'b', 	FALSE, 	FALSE},
	{CMD_CAST,  		"Cast a spell",        		'm', 	FALSE, 	FALSE},
	{CMD_CHANT,  		"Chant an incantation",		'm', 	FALSE, 	FALSE},
	{CMD_PRAY,  		"Pray a prayer",       		'p', 	FALSE, 	FALSE},
	{CMD_USE_STAFF, 	"Use ",              		'u', 	TRUE, 	FALSE},
	{CMD_USE_WAND, 		"Aim ",               		'a', 	TRUE, 	FALSE},
	{CMD_USE_ROD, 		"Zap ",                		'z', 	TRUE, 	FALSE},
	{CMD_ACTIVATE, 		"Activate ",  				'A', 	TRUE, 	FALSE},
	{CMD_EAT, 			"Eat ",            			'E', 	FALSE, 	FALSE},
	{CMD_QUAFF, 		"Quaff ",           		'q', 	FALSE, 	FALSE},
	{CMD_READ_SCROLL, 	"Read ",          			'r', 	TRUE, 	FALSE},
	{CMD_REFILL, 		"Fuel ",   					'F', 	FALSE, 	FALSE},
	{CMD_FIRE, 			"Fire ", 					'f', 	TRUE, 	FALSE},
	{CMD_FIRE_NEAR,		"Fire nearest target",		'h', 	FALSE, 	FALSE},
	{CMD_THROW, 		"Throw ",            		'v', 	TRUE, 	FALSE},
	{CMD_PICKUP,		"Pick up ", 				'g', 	FALSE, 	FALSE},
	{CMD_DESTROY, 		"Destroy ",   				'k',	FALSE, 	TRUE},
	{CMD_EXAMINE, 		"Examine ", 				'I',	FALSE, 	FALSE}
};

/* Helper function - returns the row# of a particular command from above */
static cmd_code find_command_line(cmd_code command)
{
	u16b i;

	for (i = 0; i < N_ELEMENTS(command_actions); i++)
	{
		if (command_actions[i].command == command) return i;
	}

	/*
	 * Failure should never happen, but if it is we are returning the
	 * harmless Inscribe command
	 */

	return (0);
}

/*
 * Handle a command made from the item_menu function.
 * This is a noun-verb command section, which makes it
 * different than game-comd.h, in that it assumes 100% success in command execution.
 * It assumes that add_command has performed all necessary checks to
 * ensure success before allowing the command to be added to the menu.
 * The screen should be cleared before this command is called, and
 * any menus re-loaded after it is done.
 */
static void handle_command(cmd_code command_line, int item)
{
	cmd_arg args[CMD_MAX_ARGS];

	cmd_code action = command_actions[command_line].command;

	object_type *o_ptr = object_from_item_idx(item);

	args[0].item = item;

	/* Set up for verification of inscriptions */
	p_ptr->command_cmd = command_actions[command_line].key;

	/* Allow player to "refuse" certain actions, then clear afterwards. */
	if (!get_item_allow(item, TRUE))
	{
		p_ptr->command_cmd = 0;
		return;
	}
	else p_ptr->command_cmd = 0;

	/* Mark it as the item to be used */
	o_ptr->obj_in_use = TRUE;

	/* Get a qualtity or direction if we need it */
	if (command_actions[command_line].quantity)
	{
		args[1].number = get_quantity(NULL, o_ptr->number);
	}
	if ((command_actions[command_line].aim) && (obj_needs_aim(o_ptr) ||
		(action  == CMD_FIRE) || (action  == CMD_THROW)))
	{
		get_aim_dir(&args[1].direction, FALSE);
	}

	switch(action)
	{
		case CMD_INSCRIBE:
		{
			obj_inscribe(o_ptr, item);
			break;
		}
		case CMD_UNINSCRIBE:
		{
			do_cmd_uninscribe(action, args);
			break;
		}
		case CMD_TAKEOFF:
		{
			do_cmd_takeoff(action, args);
			break;
		}
		case CMD_WIELD:
		{
			args[1].number = wield_slot(o_ptr);

			/* Oops - this shouldn't ever happen */
			if (args[1].number == -1) return;
			if (args[1].number == QUIVER_END) return;

			do_cmd_wield(action, args);
			break;
		}
		case CMD_SWAP:
		{
			do_cmd_takeoff(action, args);
			break;
		}
		case CMD_DROP:
		{
			do_cmd_swap_weapon(action, args);
			break;
		}
		case CMD_STUDY_SPELL:
		case CMD_STUDY_BOOK:
		case CMD_STUDY_INCAN:
		{
			obj_study(o_ptr, item);
			break;
		}
		case CMD_BROWSE:
		{
			obj_browse(o_ptr, item);
			break;
		}
		case CMD_CAST:
		case CMD_PRAY:
		case CMD_CHANT:
		{
			obj_cast(o_ptr, item);
			break;
		}
		case CMD_USE_STAFF:
		case CMD_USE_WAND:
		case CMD_USE_ROD:
		case CMD_ACTIVATE:
		case CMD_EAT:
		case CMD_QUAFF:
		case CMD_READ_SCROLL:
		{
			do_cmd_use(action, args);
			break;
		}
		case CMD_REFILL:
		{
			do_cmd_refill(action, args);
			break;
		}
		case CMD_FIRE:
		{
			do_cmd_fire(action, args);
			break;
		}
		case CMD_FIRE_NEAR:
		{
			/* Require foe */
			if (!target_set_closest(TARGET_KILL | TARGET_QUIET))
			{
				return;
			}

			/* Use the target */
			args[1].direction = 5;

			/* Check for confusion */
			if (p_ptr->timed[TMD_CONFUSED])
			{
				msg_print("You are confused.");
				args[1].direction = ddd[randint0(8)];
			}

			/* Fire! */
			do_cmd_fire(action, args);


			break;
		}
		case CMD_THROW:
		{
			do_cmd_throw(action, args);
			break;
		}
		case CMD_PICKUP:
		{
			if (put_object_in_inventory(o_ptr))
			{
				delete_object_idx(-item);
				p_ptr->p_energy_use = BASE_ENERGY_MOVE;
			}
			break;
		}

		case CMD_DESTROY:
		{
			destroy_item(item);
			break;
		}
		case CMD_EXAMINE:
		{
			obj_examine(o_ptr, item);
			break;
		}

		default:
		{
			/* Oops - this shouldn't ever happen */
			msg_print("invalid selection");
			break;
		}
	}

	/* We only want one at at time */
	clear_object_in_use();
}

static void add_command(cmd_code command)
{
	u16b j;

	for (j = 0; j < N_ELEMENTS(command_actions); j++)
	{
		/* Find the right command */
		if (command != command_actions[j].command) continue;

		/* Found the match */
		comm[poss] = command_actions[j].key;
		comm_descr[poss] = command_actions[j].desc;
		comm_command[poss++] = command_actions[j].command;
	}
}

#define MODE_FLOOR 		1
#define MODE_INVENTORY	2
#define MODE_EQUIPMENT	3
#define MODE_QUIVER		4

static void collect_commands(const object_type *o_ptr, int item)
{
	int mode;

	/* Start with no commands */
	poss = 0;

	button_kill_all();

	/* Items in the inventory */
	if (item < 0) 					mode = MODE_FLOOR;
	else if (item <= INVEN_PACK) 	mode = MODE_INVENTORY;
	else if (item <= INVEN_TOTAL)	mode = MODE_EQUIPMENT;
	else 	/* 	MODE_QUIVER */		mode = MODE_QUIVER;

	add_command(CMD_EXAMINE);
	button_add("EXAMINE", 'I' );

	/* Most anything can be destroyed */
	if (!o_ptr->art_num)
	{
		add_command(CMD_DESTROY);
		button_add("|DESTROY", 'k');
	}

	if (mode >= MODE_EQUIPMENT)
	{
		if (adult_swap_weapons)
		{
			if ((item == INVEN_MAIN_WEAPON) || (item == INVEN_SWAP_WEAPON))

			add_command(CMD_SWAP);
			button_add("|SWAP", 'x');
		}

		/* Can take off items unless they are cursed */
		if (obj_can_takeoff(o_ptr))
		{
			add_command(CMD_TAKEOFF);
			button_add("|TAKEOFF", 't');
		}

		/*  Check if the object can be activated and it isn't charging */
		if (obj_can_activate(o_ptr))
		{

			if ((!adult_swap_weapons) || (item != INVEN_SWAP_WEAPON))
			{
				add_command(CMD_ACTIVATE);
				button_add("|ACTIVATE", 'A');
			}
		}
	}

	/* Floor and backpack */
	else if (obj_can_wear(o_ptr))
	{
		/* Inscribed as a swap weapon */
		if ((!adult_swap_weapons) && (strstr(quark_str(o_ptr->obj_note), "@x")))
		{
			add_command(CMD_SWAP);
			button_add("|SWAP", 'x');
		}

		add_command(CMD_WIELD);
		button_add("|WIELD", 'w');
	}

	if ((obj_is_spellbook(o_ptr)) && (o_ptr->tval == cp_ptr->spell_book))
	{
		/* Can we study? */
		if (player_can_use_book(o_ptr, FALSE))
		{
			if (cp_ptr->spell_book == TV_PRAYER_BOOK)		add_command(CMD_STUDY_BOOK);
			else if  (cp_ptr->spell_book == TV_MAGIC_BOOK)	add_command(CMD_STUDY_SPELL);
			else /* TV_DRUID_BOOK */						add_command(CMD_STUDY_INCAN);
			button_add("|STUDY", 'G');
		}
		/* Can we cast? */
		if (player_can_use_book(o_ptr, TRUE))
		{
			if (cp_ptr->spell_book == TV_PRAYER_BOOK)		add_command(CMD_PRAY);
			else if  (cp_ptr->spell_book == TV_MAGIC_BOOK)	add_command(CMD_CAST);
			else /* TV_DRUID_BOOK */						add_command(CMD_CHANT);

			button_add("|CAST", 'm');
		}

		add_command(CMD_BROWSE);
		button_add("|BROWSE", 'b');
	}

	/* A staff with charges */
	if ((obj_is_staff(o_ptr)) && (obj_has_charges(o_ptr)))
	{
		add_command(CMD_USE_STAFF);
		button_add("|USE", 'u');
	}

	/* A staff with charges */
	if ((obj_is_wand(o_ptr)) && (obj_has_charges(o_ptr)))
	{
		add_command(CMD_USE_WAND);
		button_add("|AIM", 'a');
	}

	/* A staff with charges */
	if (rod_can_zap(o_ptr))
	{
		add_command(CMD_USE_ROD);
		button_add("|ZAP", 'z');
	}

	/* A staff with charges */
	if (obj_is_food(o_ptr))
	{
		add_command(CMD_EAT);
		button_add("|EAT", 'E');
	}

	/* A staff with charges */
	if (obj_is_potion(o_ptr))
	{
		add_command(CMD_QUAFF);
		button_add("|QUAFF", 'q');
	}

	/* A staff with charges */
	if (obj_is_scroll(o_ptr))
	{
		add_command(CMD_READ_SCROLL);
		button_add("|READ", 'r');
	}

	/* A staff with charges */
	if (obj_can_refill(o_ptr))
	{
		add_command(CMD_REFILL);
		button_add("|REFILL", 'F');
	}

	if (item < 0)
	{
		add_command(CMD_PICKUP);
		button_add("|PICKUP", 'g');
	}

	else
	{
		add_command(CMD_DROP);
		button_add("|DROP", 'd');
	}

	/* See if we can fire ammo */
	if (ammo_can_fire(o_ptr, item))
	{
		add_command(CMD_FIRE);
		button_add("|FIRE", 'f');

		if (valid_target_exists(TARGET_KILL | TARGET_QUIET))
		{
			add_command(CMD_FIRE_NEAR);
			button_add("|FIRE_NEAR", 'h');
		}
	}

	/* Some are automatic */
	add_command(CMD_INSCRIBE);
	button_add("|INSCR", '{' );

	if (obj_has_inscrip(o_ptr))
	{
		add_command(CMD_UNINSCRIBE);
		button_add("|UNINSC", '}' );
	}

	add_command(CMD_THROW);
	button_add("|THROW", 'v' );

	event_signal(EVENT_MOUSEBUTTONS);
}

/**
 * Item tag/command key
 */
static char command_tag(menu_type *menu, int oid)
{
	/* Caution - could be a problem here if KTRL commands were used */
	return comm[oid];
}

/**
 * Display an entry on a command menu
 */
static void command_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);

	/* Write the description */
	Term_putstr(col + 2, row, -1, attr, comm_descr[oid]);
}


/**
 * Handle user input from a command menu
 */
static bool command_action(char cmd, void *db, int oid)
{
	return TRUE;
}

#define MENU_QUIT		1
#define MENU_OBJECT 	2
#define MENU_COMMAND	3



/*
 * Come up with a list of commands for an object and display them
 * This is a "noun-verb" command list.
 *
 */
void cmd_use_item(void)
{
	int item;
	object_type *o_ptr;
	bool oops = FALSE;
	ui_event_data evt;
	byte menu_stage = MENU_OBJECT;

	/* Get item */
	cptr q = "Select an object.";
	cptr s = "There are no objects to select.";

	menu_type menu;
	menu_iter commands_menu = {command_tag, NULL, command_display, command_action };
	/* Note that the column needs to be at least 1 due to the title line below */
	region area = { 0, 0, -1, -1 };

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = "\x8B\x8C\n\r";
	menu.count = poss;
	menu.menu_data = comm;
	menu_init(&menu, MN_SKIN_SCROLL, &commands_menu, &area);

	/* We want to use all objects */
	item_tester_tval = 0;
	item_tester_hook = NULL;
	item_tester_swap = FALSE;

	/* Get item */
	q = "Select an item.";
	s = "You have no objects to select.";

	/* We already have an object selected */
	if (find_object_in_use(&item))
	{
		/* Skip the object manu */
		menu_stage = MENU_COMMAND;

		/* Clear the mark */
		clear_object_in_use();
	}

	else menu_stage = MENU_OBJECT;

	/* Make sure the player has seen everything */
	message_flush();

	while (menu_stage != MENU_QUIT)
	{
		int cursor = 0;
		char o_name[80];

		if (menu_stage == MENU_OBJECT)
		{

			message_flush();
			screen_save();

			if (!item_menu(&item, q, (USE_QUIVER | USE_INVEN | USE_EQUIP | USE_FLOOR | NOUN_VERB), &oops, p_ptr->py, p_ptr->px))
			{
				/* Total Redraw, print warning, then quit  */
				menu_stage = MENU_QUIT;
				p_ptr->noun_verb = FALSE;
				screen_load();
				break;
			}
			else
			{
				menu_stage = MENU_COMMAND;
			}

			/* No objects, */
			if (oops)
			{
				/* Total Redraw, print warning, then quit  */
				msg_print(s);
				p_ptr->noun_verb = FALSE;
				menu_stage = MENU_QUIT;
			}
			screen_load();

		}

		if (menu_stage >= MENU_COMMAND)
		{
			char title[120];
			int len = 70;

			/* Get the item */
			o_ptr = object_from_item_idx(item);

			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);

			collect_commands(o_ptr, item);
			/* Update the menu */
			menu.count = poss;
			menu.menu_data = comm;
			area.page_rows = poss + 3;

			/* Find the column to start in */
			if ((Term->wid - len + 1) < 12 ) area.col = 12;
			else area.col = Term->wid - (len + 1);
			area.width = len;

			message_flush();
			screen_save();

			my_strcpy(title, format(" Enter a command for %s", o_name), sizeof(title));

			menu.title = title;

			menu_init(&menu, MN_SKIN_SCROLL, &commands_menu, &area);

			/* Select an entry */
			evt = menu_select(&menu, &cursor, EVT_SELECT);

			screen_load();

			/* Go back to the object manu */
			if (evt.key == ESCAPE)
			{
				menu_stage = MENU_OBJECT;
				continue;
			}

			if (evt.type == EVT_SELECT)
			{
				cmd_code command_line = find_command_line(comm_command[cursor]);

				o_ptr->obj_in_use = TRUE;

				message_flush();

				handle_command(command_line, item);

				/* Go back one step if item has been consumed used, moved, etc.... */
				if (!find_object_in_use(&item)) menu_stage = MENU_OBJECT;

				/* We just used some energy, so we need to exit the menu and process the dungeon */
				if (p_ptr->p_energy_use)
				{
					menu_stage = MENU_QUIT;
					p_ptr->noun_verb = TRUE;
				}

				/* Place the cursor on the player */
				move_cursor_relative(p_ptr->py, p_ptr->px);

				process_command(CMD_GAME, TRUE);
			}
			handle_stuff();
		}
	}

	handle_stuff();
	basic_buttons();
	message_flush();

}



