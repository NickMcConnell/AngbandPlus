/* File: cmd-know.c */
/* Purpose: Information and Utility functions. */

/*
 * Contents of Crate, shipped across the atlantic, total sailing time 9 
 * weeks. Display inventory and equipment.  Wear and remove equipment.  Drop and
 * destroy.  Inspecting, inscribing, refueling objects.  Learn about a
 * symbol. 
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Display inventory
 */
void do_cmd_inven(void)
{
	/* added to make equipment weight display properly */
	int i;
	int statval = p_ptr->stat_use[A_MUS];
	char prompt[80];

	if (statval < 30) statval = 30;
	i = 900 + (((statval / 25) * 100) / 2);

	/* Hack -- Start in "inventory" mode */
	p_ptr->command_wrk = (USE_INVEN);

	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the inventory */
	show_inven();

	/* Hack -- hide empty slots */
	item_tester_full = FALSE;

	/* Prompt for a command */
	/*	prt("(Inventory) Command: ", 0, 0); */
	/* Build a prompt */
	sprintf(prompt,
		"(Inventory) burden %ld.%ld lb (%ld%% of capacity). Command: ",
		(long)p_ptr->total_weight / 10, (long)p_ptr->total_weight % 10,
		(long)(p_ptr->total_weight * 100) / i);

	/* print the prompt */
	prt(prompt, 0, 0);

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

	/* Hack -- Process normal keys */
	else
	{
		/* Hack -- Use "display" mode */
		p_ptr->command_see = TRUE;
	}
}


/*
 * Display equipment
 */
void do_cmd_equip(void)
{
	/* Hack -- Start in "equipment" mode */
	p_ptr->command_wrk = (USE_EQUIP);

	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	show_equip();

	/* Hack -- undo the hack above */
	item_tester_full = FALSE;

	/* Prompt for a command */
	prt("(Equipment) Command: ", 0, 0);

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

	/* Hack -- Process normal keys */
	else
	{
		/* Enter "display" mode */
		p_ptr->command_see = TRUE;
	}
}


/*
 * The "wearable" tester
 */
static bool item_tester_hook_wear(const object_type *o_ptr)
{
	/* Check for a usable slot */
	if (wield_slot(o_ptr) >= INVEN_WIELD) return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
	int item, slot, num, percent;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	cptr act;

	cptr q, s;

	char o_name[80];

	bool reload = FALSE;

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_wear;

	/* Get an item */
	q = "Wear/Wield which item? ";
	s = "You have nothing you can wear or wield.";
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


	/* Check the slot */
	slot = wield_slot(o_ptr);

	/* Ask for ring to replace */
	if ((o_ptr->tval == TV_RING) &&
		inventory[INVEN_LEFT].k_idx &&
		inventory[INVEN_RIGHT].k_idx)
	{
		/* Restrict the choices */
		item_tester_tval = TV_RING;
	
		/* Choose a ring from the equipment only */
		q = "Replace which ring? ";
		s = "Oops.";
		if (!get_item(&slot, q, s, USE_EQUIP)) return;
	}

	/* Prevent wielding into a cursed slot */
	if (slot < INVEN_LOADEDGUN && cursed_p(&inventory[slot]))
	{
		/* Describe it */
		object_desc(o_name, &inventory[slot], FALSE, 0);

		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
			o_name, describe_use(slot));

		/* Cancel the command */
		return;
	}
	if ((slot == INVEN_LOADEDGUN || slot == INVEN_GUN) && cursed_p(&inventory[INVEN_LOADEDGUN]))
	{
		/* Describe it */
		object_desc(o_name, &inventory[INVEN_LOADEDGUN], FALSE, 0);

		/* Message */
		msg_format("%s%s you have %s appear%s to be cursed.",
			o_name, (o_ptr->number > 1)?"s":"",describe_use(INVEN_LOADEDGUN), (o_ptr->number > 1)?"":"s");

		/* Cancel the command */
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

	/* Usually, we wear or wield only one item. */
	num = 1;

	/* Ammo goes in quiver slots, which have special rules. */
	if (slot == INVEN_LOADEDGUN)
	{
		/* initialisation */
		int number = 0, ammo_num = 0, swiftshot = 0;

		object_type *ammo_ptr = &inventory[INVEN_LOADEDGUN];
		object_type *gun_ptr = &inventory[INVEN_GUN];

		if (!gun_ptr->k_idx)
		{
			msg_print("You aren't wielding a firearm.");
			return;			
		}
		
		if (!(p_ptr->ammo_tval == o_ptr->tval))
		{
			msg_print("You can't load a gun with the wrong ammunition.");
			return;			
		}

		/* swiftshot is used to calculate how much energy is used for loading */
		if (p_ptr->skills[SK_SWIFT_SHOT].skill_max > 0)
			swiftshot = p_ptr->skills[SK_SWIFT_SHOT].skill_rank;
		
		if (object_similar(o_ptr, ammo_ptr))
		{
			reload = TRUE;
			/* reloading is applicable only when objects are similar is all aspects */
			number =  (p_ptr->num_fire - ammo_ptr->number > o_ptr->number) ? o_ptr->number:  p_ptr->num_fire - ammo_ptr->number;
			/* paranoia - no negative amount ammo loading */
			if (number < 0) number = 0;
		}
		else
		{
			/* We are changing ammunition from one type to another */
			number = ((o_ptr->number < p_ptr->num_fire) ? o_ptr->number : p_ptr->num_fire);
		}

		num = number;
		
		/* no need to reload - Cancel */
		if (!num) return;

		/* paranoia */
		percent = 100;
		
		/* determine how many bullets we load */
		percent = (number * 100) / p_ptr->num_fire;
		
		/* Take some time to reload, modified by the number of bullets you are loading */
		p_ptr->energy_use += ((200 * percent)/100) + randint(100) - (swiftshot * 6);

		/* How long did it take? */
		if (p_ptr->wizard) msg_format("You used %d energy to load the firearm! percent=%d swiftshot=%d", p_ptr->energy_use, percent, swiftshot);

		/* Quiver will be reorganized (again) later. */
		p_ptr->notice |= (PN_COMBINE);
	}

	/* Modify quantity */
	i_ptr->number = num;  

	/* Decrease the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -num);
		inven_item_optimize(item);
	}
	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -num);
		floor_item_optimize(0 - item);
	}

	if (reload)
	{
		inven_item_increase(INVEN_LOADEDGUN, num);
	}
	else
	{
		/* Get the wield slot */
		o_ptr = &inventory[slot];

		/* Take off existing item */
		if (o_ptr->k_idx)
		{
			/* Take off existing item */
			(void)inven_takeoff(slot, 255);
		}

		/* Wear the new stuff */
		object_copy(o_ptr, i_ptr);
	}

	if ((i_ptr->tval == TV_MECHA_TORSO) ||
	 	(i_ptr->tval == TV_MECHA_HEAD) ||
	 	(i_ptr->tval == TV_MECHA_ARMS) ||
	 	(i_ptr->tval == TV_MECHA_FEET))
	 {
	 	/* Nothing */
	 }
	
	/* Increase the weight */
	else p_ptr->total_weight += i_ptr->weight;

	/* Increment the equip counter by hand */
	p_ptr->equip_cnt++;

	/* Where is the item now */
	if (slot == INVEN_WIELD)
	{
		act = "You are wielding";
	}
	else if (slot == INVEN_GUN)
	{
		act = "You are shooting with";
	}
	else if (slot == INVEN_LITE)
	{
		act = "Your light source is";
	}
	else if (slot == INVEN_LOADEDGUN)
	{
		act = "You have loaded";
	}
	else
	{
		act = "You are wearing";
	}

		/* Describe the result */
	if (reload)
	{
		object_desc(o_name, i_ptr, TRUE, 3);
	}
	else
	{
		object_desc(o_name, o_ptr, TRUE, 3);
	}

	/* Message */
	msg_format("%s %s (%c).", act, o_name, index_to_label(slot));

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		msg_print("Oops! It feels deathly cold!");

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

		/* Sense the object if allowed */
		if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_CURSED;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate Hit points */
	p_ptr->update |= (PU_HP);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
}



/*
 * Take off an item
 */
void do_cmd_takeoff(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;


	/* Get an item */
	q = "Take off which item? ";
	s = "You are not wearing anything to take off.";
	if (!get_item(&item, q, s, (USE_EQUIP))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Item is cursed */
	if (cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}
	if ((item == INVEN_GUN) && cursed_p(&inventory[INVEN_LOADEDGUN]))
	{
		/* Oops */
		msg_print("Hmmm, your ammo seems to be cursed.");

		/* Nope */
		return;
	}
	
	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Take off the item */
	(void)inven_takeoff(item, 255);
}


/*
 * Drop an item
 */
void do_cmd_drop(void)
{
	int item, amt;

	object_type *o_ptr;

	cptr q, s;


	p_ptr->command_wrk = (USE_INVEN);
	
	/* Get an item */
	q = "Drop which item? ";
	s = "You have nothing to drop.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

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

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Drop (some of) the item */
	inven_drop(item, amt);
}



/*
 * Destroy an item
 */
void do_cmd_destroy(int item)
{
		int amt;
	int old_number;

	object_type *o_ptr;

	char o_name[80];

	char out_val[160];

	cptr q, s;

	if (!item)
	{
		/* Get an item */
		q = "Destroy which item? ";
		s = "You have nothing to destroy.";
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
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

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify destruction */
	if (verify_destroy)
	{
		sprintf(out_val, "Really destroy %s? ", o_name);
		if (!get_check(out_val)) return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Artifacts cannot be destroyed */
	if ((artifact_p(o_ptr)) || (o_ptr->ident & IDENT_QUEST))
	{
		/* Message */
		msg_format("You cannot destroy %s.", o_name);

		/* Don't mark id'ed objects */
		if (object_known_p(o_ptr)) return;

		/* It has already been sensed */
		if (o_ptr->ident & (IDENT_SENSE))
		{
			/* Already sensed objects always get improved feelings */
			if (cursed_p(o_ptr))
				o_ptr->discount = INSCRIP_TWISTED;
			else if (broken_p(o_ptr))
				o_ptr->discount = INSCRIP_SHATTERED;
			else
				o_ptr->discount = INSCRIP_SPECIAL;
		}
		else
		{
			/* Mark the object as indestructible */
			o_ptr->discount = INSCRIP_INDESTRUCTIBLE;
		}

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}

	/* Message */
	msg_format("You destroy %s.", o_name);

	/* Reduce the charges of rods/wands */
	reduce_charges(o_ptr, amt);


	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}



/*
 * Display specialized object information.  -LM-
 *
 * Unidentified:
 *	Weapons and armour -> description of specific object type (dagger,
 *	  etc.).
 *	Others -> description only of general object kind (scroll, etc.)
 * Identified or aware:
 *	Artifacts -> artifact-specific description.
 *	Most objects -> description of specific object type.
 *	Scrolls, potions, spellbooks, rings, and amulets -> description
 *	  only of general object kind.
 * *Identified*:
 *	All -> description of object type or artifact description,
 *	  complete listing of attributes and flags.
 *
 * Objects may also be members of a class with known effects.  If so,
 * extra information about effects when used will appear if the effects
 * can be quantified.
 *
 * Boolean value "in_store" only takes effect if player is in a store.
 */
void do_cmd_observe(object_type *o_ptr, bool in_store)
{
	int item;
	int y, x;
	int i;

	u16b aware, known, mental;

	object_kind *k_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	char o_name[120];

	char info_text[2048];
	char object_kind_info[400];

	cptr q, s;


	/* Initialize object description. */
	strcpy(info_text, "");

	/* If not called in a store, we must get an object to inspect. */
	if (!o_ptr)
	{
		/* Get an item */
		q = "Examine which item?";
		s = "You have nothing to examine.";
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
	}

	/* Automatically Know books */
	if (o_ptr->tval == TV_BOOK)
	{
		if (!(object_known_p(o_ptr)))
		{
			/* Know the book */
			object_known(o_ptr);
		}
	}

	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy the object */
	object_copy(i_ptr, o_ptr);

	/* Make singular */
	i_ptr->number = 1;

	/* Describe the object */
	if (in_store) object_desc_store(o_name, i_ptr, FALSE, 2);
	else object_desc(o_name, i_ptr, FALSE, 2);

	/* What is our level of knowledge about the object? */
	aware = object_aware_p(i_ptr);
	known = (object_known_p(i_ptr) || in_store);
	/* known_effects = k_ptr->special & (SPECIAL_KNOWN_EFFECT); */
	mental = ((i_ptr->ident & (IDENT_MENTAL)) || in_store);

	
	/*
	 * Hack - Avoid giving away too much info in normal stores about objects
	 * other than weapons and armour (no sneaky learning about wand damages!).
	 */
	if (in_store) /* && (!k_ptr->special & (SPECIAL_KNOWN_EFFECT)) &&(!is_wargear(k_ptr))) */
	{
		mental = FALSE;
	}

	/* Object is fully known - give maximal information. */
	if (mental)
	{
		/* Get the specific object type's information. */
		object_info(info_text, i_ptr);

		/* Get information about the general object kind. */
		strcpy(object_kind_info, format("%s", obj_class_info[i_ptr->tval]));
	}

	/* Object or object type is identified - show all basic information. */
	else if ((known) || (aware))
	{
		/* Get the specific object type's information, if any. */
		object_info(info_text, i_ptr);

		/* Get information about the general object kind. */
		strcpy(object_kind_info, format("%s", obj_class_info[i_ptr->tval]));
	}

	/*
	 * Nothing is known about the object or object type - show only
	 * information about the general object kind.
	 */
	else
	{

		/* Get information about the general object kind. */
		strcpy(object_kind_info, format("%s", obj_class_info[i_ptr->tval]));

		/* Do not display the name of an unaware object  XXX */
		if (!object_aware_p(i_ptr)) strcpy(o_name, "  ");
		
	}

	/* Save screen */
	screen_save();

	/* Clear the screen */
	Term_clear();

	/* Label the information. */
	roff(format("Item Information:  %s\n\n", o_name));

	/* Object type or artifact information. */
	c_roff(TERM_BLUE, info_text);

#if 0
	/*  Describe artifacts that belong to sets. */
	if ((known) && (i_ptr->name1))
	{
		artifact_type *a_ptr = &a_info[i_ptr->name1];


		/* Is it a set item? */
		if (a_ptr->set_index)
		{
			/* Advance two lines */
			for (i = 0; i < 2; i++) roff("\n", 0, 0);

			/* Set notification */
			c_roff(TERM_GREEN,"Set Item: ");

			/* Require full ID to describe the specific set */
			if (mental)
			{
				set_type *s_ptr = &s_info[a_ptr->set_index];
				c_roff(TERM_GREEN, s_ptr->set_desc, 3, 77);
			}

			/* Generic description */
			else c_roff(TERM_GREEN,"It gains power when combined with matching items", 3, 77);
			/* End sentence */
			c_roff(TERM_GREEN, ".", 3, 77);
		}
	}
#endif


	/* Clear some space */
	for (i = 0; i < 2; i++) roff("\n");

	/* Fully describe the object. */
	object_details(i_ptr, mental, known);

	/* Obtain the cursor location */
	(void)Term_locate(&x, &y);

	/* Spacing. */
	for (i = y; i < 18; i++) roff("\n");

	/* Object kind information. */
	c_roff(TERM_GREEN, object_kind_info);

	/* Hack -- attempt to stay on screen. */
	for (i = y; i < Term->hgt; i++)
	{
		/* No more space! */
		if (i > Term->hgt - 2) break;

		/* Advance one line. */
		roff("\n");

		/* Enough clear space.  Done. */
		if (i == (y + 2)) break;
	}


	/* The exit sign. */
	roff("(Press any key to continue.)");
	(void)inkey();


	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);


	/* Clear the screen */
	Term_clear();

	/* Load screen */
	screen_load();
}



/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;


	/* Get an item */
	q = "Un-inscribe which item? ";
	s = "You have nothing to un-inscribe.";
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

	/* Nothing to remove */
	if (!o_ptr->note)
	{
		msg_print("That item had no inscription to remove.");
		return;
	}

	/* Message */
	msg_print("Inscription removed.");

	/* Remove the incription */
	o_ptr->note = 0;

	/* Combine the pack */
	p_ptr->notice |= (PN_COMBINE);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	char tmp[80];

	cptr q, s;


	/* Get an item */
	q = "Inscribe which item? ";
	s = "You have nothing to inscribe.";
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

	/* Describe the activity */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	msg_format("Inscribing %s.", o_name);
	message_flush();

	/* Start with nothing */
	strcpy(tmp, "");

	/* Use old inscription */
	if (o_ptr->note)
	{
		/* Start with the old inscription */
		strnfmt(tmp, 80, "%s", quark_str(o_ptr->note));
	}

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", tmp, 80))
	{
		/* Save the inscription */
		o_ptr->note = quark_add(tmp);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}



/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(const object_type *o_ptr)
{
	/* Flasks of oil are okay */
	if (o_ptr->tval == TV_FLASK) return (TRUE);

	/* Non-empty lanterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_LANTERN) &&
	    (o_ptr->pval > 0))
	{
		return (TRUE);
	}

	/* Assume not okay */
	return (FALSE);
}


/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(void)
{
	int item;

	object_type *o_ptr;
	object_type *j_ptr;

	cptr q, s;


	/* Restrict the choices */
	item_tester_hook = item_tester_refill_lantern;

	/* Get an item */
	q = "Refill with which source of oil? ";
	s = "You have no sources of oil.";
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


	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Get the lantern */
	j_ptr = &inventory[INVEN_LITE];

	/* Refuel */
	j_ptr->pval += o_ptr->pval;

	/* Message */
	msg_print("You fuel your lamp.");

	/* Comment */
	if (j_ptr->pval >= FUEL_LAMP)
	{
		j_ptr->pval = FUEL_LAMP;
		msg_print("Your lamp is full.");
	}

	/* Use fuel from a lantern */
	if (o_ptr->sval == SV_LITE_LANTERN)
	{
		/* No more fuel */
		o_ptr->pval = 0;

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* Decrease the item (from the pack) */
	else if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP);
}



/*
 * An "item_tester_hook" for refilling torches
 */
static bool item_tester_refill_torch(const object_type *o_ptr)
{
	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_TORCH)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * Refuel the players torch (from the pack or floor)
 */
static void do_cmd_refill_torch(void)
{
	int item;

	object_type *o_ptr;
	object_type *j_ptr;

	cptr q, s;


	/* Restrict the choices */
	item_tester_hook = item_tester_refill_torch;

	/* Get an item */
	q = "Refuel with which torch? ";
	s = "You have no extra torches.";
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


	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Get the primary torch */
	j_ptr = &inventory[INVEN_LITE];

	/* Refuel */
	j_ptr->pval += o_ptr->pval + 5;

	/* Message */
	msg_print("You combine the torches.");

	/* Over-fuel message */
	if (j_ptr->pval >= FUEL_TORCH)
	{
		j_ptr->pval = FUEL_TORCH;
		msg_print("Your torch is fully fueled.");
	}

	/* Refuel message */
	else
	{
		msg_print("Your torch glows more brightly.");
	}

	/* Decrease the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP);
}




/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(void)
{
	object_type *o_ptr;

	/* Get the light */
	o_ptr = &inventory[INVEN_LITE];

	/* It is nothing */
	if (o_ptr->tval != TV_LITE)
	{
		msg_print("You are not wielding a light.");
	}

	/* It's a lamp */
	else if (o_ptr->sval == SV_LITE_LANTERN)
	{
		do_cmd_refill_lamp();
	}

	/* It's a torch */
	else if (o_ptr->sval == SV_LITE_TORCH)
	{
		do_cmd_refill_torch();
	}

	/* It's a candle or taper */
	else if ((o_ptr->sval == SV_LITE_TAPER) || 
			 (o_ptr->sval == SV_LITE_CANDLE_WAX) ||
			 (o_ptr->sval == SV_LITE_CANDLE_TALLOW))
	{
		msg_print("Your light source can't be refilled.");
	}

	/* No torch to refill */
	else
	{
		msg_print("Your light cannot be refilled.");
	}
}






/*
 * Target command
 */
void do_cmd_target(void)
{
	/* Target set */
	if (target_set_interactive(TARGET_KILL))
	{
		msg_print("Target Selected.");
	}

	/* Target aborted */
	else
	{
		msg_print("Target Aborted.");
	}
}



/*
 * Look command
 */
void do_cmd_look(void)
{
	/* Look around */
	if (target_set_interactive(TARGET_LOOK))
	{
		msg_print("Target Selected.");
	}
}



/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(void)
{
	int dir, y1, x1, y2, x2;

	char tmp_val[80];

	char out_val[160];


	/* Start at current panel */
	y2 = y1 = p_ptr->wy;
	x2 = x1 = p_ptr->wx;

	/* Show panels until done */
	while (1)
	{
		/* Describe the location */
		if ((y2 == y1) && (x2 == x1))
		{
			tmp_val[0] = '\0';
		}
		else
		{
			sprintf(tmp_val, "%s%s of",
			        ((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
			        ((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));
		}

		/* Prepare to ask which way to look */
		sprintf(out_val,
		        "Map sector [%d,%d], which is%s your sector.  Direction?",
		        (y2 / PANEL_HGT), (x2 / PANEL_WID), tmp_val);

		/* More detail */
		if (center_player)
		{
			sprintf(out_val,
		        	"Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
		        	(y2 / PANEL_HGT), (y2 % PANEL_HGT),
		        	(x2 / PANEL_WID), (x2 % PANEL_WID), tmp_val);
		}

		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			char command;

			/* Get a command (or Cancel) */
			if (!get_com(out_val, &command)) break;

			/* Extract direction */
			dir = target_dir(command);

			/* Error */
			if (!dir) bell("Illegal direction for locate!");
		}

		/* No direction */
		if (!dir) break;

		/* Apply the motion */
		y2 += (ddy[dir] * PANEL_HGT);
		x2 += (ddx[dir] * PANEL_WID);

		/* Verify the row */
		if (y2 < 0) y2 = 0;
		if (y2 > DUNGEON_HGT - SCREEN_HGT) y2 = DUNGEON_HGT - SCREEN_HGT;

		/* Verify the col */
		if (x2 < 0) x2 = 0;
		if (x2 > DUNGEON_WID - SCREEN_WID) x2 = DUNGEON_WID - SCREEN_WID;

		/* Handle "changes" */
		if ((p_ptr->wy != y2) || (p_ptr->wx != x2))
		{
			/* Update panel */
			p_ptr->wy = y2;
			p_ptr->wx = x2;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Handle stuff */
			handle_stuff();
		}
	}

	/* Verify panel */
	p_ptr->update |= (PU_PANEL);

	/* Handle stuff */
	handle_stuff();
}






/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static cptr ident_info[] =
{
	" :A dark grid",
	"!:An tonic (or oil)",
	"\":An amulet (or necklace)",
	"#:A wall (or secret door)",
	"$:Treasure (gold or gems)",
	"%:A vein (magma or quartz)",
	/* "&:unused", */
	"':An open door",
	"(:Soft armor",
	"):An item of clothing for the legs",
	"*:A vein with treasure",
	"+:A closed door",
	",:Food (or mushroom patch)",
	"-:A ray gun (or apparatus)",
	".:Floor",
	"/:A polearm (Axe/Pike/etc)",
	"0:Entrance to Steamware Vendor",
	"1:Entrance to General Store", /* Must correct these */
	"2:Entrance to Clothing Store",
	"3:Entrance to Gun Smith",
	"4:Entrance to Machinist",
	"5:Entrance to Alchemy shop",
	"6:Entrance to Magic store",
	"7:Entrance to Black Market",
	"8:Entrance to your home",
	"9:Entrance to the Library",
	"::Rubble",
	";:A glyph of warding",
	"<:An up staircase",
	"=:A ring",
	">:A down staircase",
	"?:A mechanism",
	"[:Hard armor",
	"\\:A hafted weapon (mace/whip/etc)",
	"]:Misc. armor",
	"^:A trap",
	"_:A tool",
	/* "`:unused", */
	"@:You",
	"A:Alien",									/* Alien! */
	"B:Large Beast-man",						/* Beast-man! */
	"C:Construct",								/* Construct */
	"D:Large Dinosaur",							/* Dinosaur */
	"E:Elemental",								/* Elemental */
	/* "F:Face Card (cards moved to c)", */
	"G:Ghost",									/* Undead */
	"H:Hybrid",									/* ??? */
	"I:Insect",									/* Animal */
	"J:Snake",									/* Animal */
	"K:Mechanical Chicken",							/* Animal */
	/* "L:unused (old Lich)", */		
	"M:Monkey",									/* Animal */
	/* "N:unused", */
	"O:Creatures from Oz", 						/* ??? */
	"P:Giant Humanoid",							/* Humanoid */
	/* "Q:Unused (old Quylthulg)", */
	"R:Reptile/Amphibian",						/* Animal */
	"S:Spider/Scorpion/Tick",					/* Animal */
	"T:Tree", 									/* Plant */
	"U:Major Demon",							/* Demon */
	"V:Vampire/Revenant",						/* Undead */
	"W:Wight/Wraith/etc",						/* Undead */
	/* "X: Unused (old Xorn/Xaren/etc)", */
	/* "Y: Unused (old Yeti)", */
	"Z:Hound or Canine",						/* Animal */
	"a:Automaton",								/* Automaton */
	"b:Small Beast-man",						/* Beast-man */
	"c:Card",									/* Card */
	"d:Dinosaur",								/* Dinosaur */
	"e:Equipment",								/* Automata */
	"f:Feline",									/* Animal */
	"g:Golem",									/* Construct */
	"h:Small humanoid",							/* Humanoid */
	"i:Bird",									/* Animal */
	/* "j: Unused (old Jelly)",	*/
	"k:Chicken",								/* Animal */
	"l:Louse",									/* Animal */
	"m:Mold",									/* Plant */
	"n:Plant",									/* Plant */
	"o:Object",							    /* Object */
	"p:Person/Human",							/* Humanoid */
	"q:Quadruped",								/* Animal */
	"r:Rodent",									/* Animal */
	/* "s: Unused", (old skeleton)	*/			/* Undead */
	"t:Townsperson",							/* Humanoid */
	"u:Minor Demon",							/* Demon */
	"v:Vortex",									/* Elemental */
	"w:Worm/Worm-Mass",							/* Animal */
	/* "x: Unused", */
	/* "y: Unused (old Yeek)", */
	"z:Zombie/Mummy",							/* Undead */
	"{:Firearm Ammunition (bullet/shot/ammo)",
	"|:An edged weapon (sword/dagger/etc)",
	"}:A firearm (rifle/shotgun/pistol)",
	"~:A tool (or miscellaneous item)",
	NULL
};



/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
bool ang_sort_comp_hook(const void *u, const void *v, int a, int b)
{
	u16b *who = (u16b*)(u);

	u16b *why = (u16b*)(v);

	int w1 = who[a];
	int w2 = who[b];

	int z1, z2;


	/* Sort by player kills */
	if (*why >= 4)
	{
		/* Extract player kills */
		z1 = l_list[w1].r_pkills;
		z2 = l_list[w2].r_pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = l_list[w1].r_tkills;
		z2 = l_list[w2].r_tkills;

		/* Compare total kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = r_info[w1].level;
		z2 = r_info[w2].level;

		/* Compare levels */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = r_info[w1].mexp;
		z2 = r_info[w2].mexp;

		/* Compare experience */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Compare indexes */
	return (w1 <= w2);
}


/*
 * Sorting hook -- Swap function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform.
 */
void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	u16b holder;

	/* Unused parameter */
	(void)v;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}



/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void roff_top(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	byte a1, a2;
	char c1, c2;


	/* Get the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Get the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;


	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	Term_addstr(-1, TERM_WHITE, "'):");
}


/*
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "multiple" monsters:
 *   ^A (all monsters)
 *   ^U (all unique monsters)
 *   ^N (all non-unique monsters)
 *   ^K (all monsters killed at least once this life)  -RML-
 *
 * The responses may be sorted in several ways, see below.
 *
 */
void do_cmd_query_symbol(void)
{
	int i, j, n, r_idx;
	int start = 0, last_level = 0;
	char sym, query;
	char buf[128];

	bool all = FALSE;
	bool uniq = FALSE;
	bool norm = FALSE;
	bool kill = FALSE;

	bool recall = FALSE;

	u16b why = 0;
	u16b *who;


	/* Interact */
	while (TRUE)
	{
		/* Get a character, or abort */
		if (!get_com("Enter character to be identified (RET for help, ESC to cancel):", &sym)) return;

		/* Describe */
		if (sym == KTRL('A'))
		{
			all = TRUE;
			strcpy(buf, "Full monster list.");
			break;
		}
		else if (sym == KTRL('U'))
		{
			all = uniq = TRUE;
			strcpy(buf, "Unique monster list.");
			break;
		}
		else if (sym == KTRL('N'))
		{
			all = norm = TRUE;
			strcpy(buf, "Non-unique monster list.");
			break;
		}
		else if (sym == KTRL('K'))
		{
			all = kill = TRUE;
			strcpy(buf, "Killed monster list.");
			break;
		}
		else if (sym == ESCAPE)
		{
			return;
		}
		else if ((sym == '\r') || (sym == '\n'))
		{
			screen_save();
			clear_from(0);

			move_cursor(4, 15);
			c_roff(TERM_WHITE, "Press:\nControl-A for the full monster list\nControl-U for the unique monster list\nControl-N for the non-unique monster list\nControl-K for the killed monster list\nESCAPE to cancel\nRETURN to show this help window, or\nAny other key to display information about it");

			(void)inkey();
			screen_load();
		}
		else
		{
			/* Try to match the key with a info entry */
			for (i = 0; ident_info[i]; ++i)
			{
				if (sym == ident_info[i][0]) break;
			}
			if (ident_info[i])
			{
				sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
				break;
			}
			else if (!isalpha(sym))
			{
				msg_print("This command was not understood.");
				message_flush();
			}
		}
	}

	/* Display the result */
	prt(buf, 0, 0);

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/*
		 * Nothing to recall.
		 * Even cheat_know does not allow the viewing of nonexistent
		 * player ghosts.
		 */
		if ((!cheat_know) && (!l_ptr->r_sights))
		{
			 continue;
		}

		/* Skip unused monsters */
		if (!r_ptr->name) continue;
		if (!r_ptr->rarity) continue;

		/* Require non-unique monsters if needed */
		if (norm && (r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Require unique monsters if needed */
		if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Require killed if needed */
		if (kill && !(l_ptr->r_pkills)) continue;

		/* Collect "appropriate" monsters */
		if (all || (r_ptr->d_char == sym)) who[n++] = i;
	}


	/* Nothing to recall */
	if (!n)
	{
		/* XXX XXX Free the "who" array */
		/* FREE(who); */
		/* XXX XXX Free the "who" array */
		C_FREE(who, z_info->r_max, u16b);

		return;
	}


	/* Prompt */
	put_str("Recall details? (y/n, (k)ills, (l)evel): ", 0, 40);

	/* Query */
	query = inkey();

	/* Restore */
	prt(buf, 0, 0);


	/* Sort by kills (and level) */
	if (query == 'k')
	{
		why = 4;
		query = 'y';
	}

	/* Sort by total kills (and level) */
	else if (query == 't')
	{
		why = 3;
		query = 'y';
	}

	/* Sort by level */
	else if (query == 'l')
	{
		why = 2;
		query = 'y';
	}


	/* Order cancelled */
	else if (!strchr("yYrR", query))
	{
		/* XXX XXX Free the "who" array */
		/* FREE(who); */
		/* XXX XXX Free the "who" array */
		C_FREE(who, z_info->r_max, u16b);

		return;
	}


	/* Sort if needed */
	if (why)
	{
		/* Select the sort method */
		ang_sort_comp = ang_sort_comp_hook;
		ang_sort_swap = ang_sort_swap_hook;

		/* Sort the array */
		ang_sort(who, &why, n);
	}


	/* Start at the current level */
	i = 0;


	/* We're using the stadard order, or sorting by level */
	if ((why == 0) || (why == 2))
	{
		/*
		 * Find the monster whose level is the closest to the
		 * current depth, without being greater than it.
		 */
		for (j = 0; j < n; j++)
		{
			monster_race *r_ptr = &r_info[who[j]];

			/* Skip uniques */
			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

			if ((r_ptr->level <= p_ptr->depth) && (r_ptr->level > last_level))
			{
				start = j;
				last_level = r_ptr->level;
			}
		}
	}

	/* Start at the chosen monster. */
	i = start;

	/* Scan the monster memory */
	while (TRUE)
	{
		/* Extract a race */
		r_idx = who[i];

		/* Hack -- Auto-recall */
		monster_race_track(r_idx);

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Hack -- Begin the prompt */
		roff_top(r_idx);

		/* Hack -- Complete the prompt */
		Term_addstr(-1, TERM_WHITE, "[(r)ecall, ESC]");
		prt(format("(#%d of %d)", i + 1, n), 0 , 65);

		/* Interact */
		while (TRUE)
		{
			/* Recall */
			if (recall)
			{
				/* Save screen */
				screen_save();

				while (TRUE)
				{
					/* Recall on screen */
					screen_roff(who[i], p_ptr->monster_mem_fmt);

					/* Hack -- Complete the prompt (again) */
					Term_addstr(-1, TERM_WHITE, "[un(r)ecall, ESC]");
					prt(format("(#%d of %d)", i + 1, n), 0 , 65);
					/* Command */
					query = inkey();

					/* Switch display mode */
					if (query == '\t') {
						if (p_ptr->monster_mem_fmt == FALSE) p_ptr->monster_mem_fmt = TRUE;
						else p_ptr->monster_mem_fmt = FALSE;
						/* Update recall window */
						p_ptr->window |= (PW_MONSTER);
					}
					else break;

				}
			} else {
				/* Command */
				query = inkey();
			}
			
			/* Unrecall */
			if (recall)
			{
				/* Load screen */
				screen_load();
			}

			/* Normal commands */
			if (query != 'r') break;

			/* Toggle recall */
			recall = !recall;
		}

		/* Stop scanning */
		if (query == ESCAPE) break;

		/* Move to "prev" monster */
		if ((query == '-') || (query == '4') ||
		    ((rogue_like_commands) && (query == 'h')))
		{
			if (i-- == 0)
			{
				i = n - 1;
			}
		}

		/* Move to "next" monster */
		else
		{
			if (++i == n)
			{
				i = 0;
			}
		}
	}


	/* Re-display the identity */
	prt(buf, 0, 0);


	/* Free the "who" array */
	/* FREE(who); */
	/* XXX XXX Free the "who" array */
	C_FREE(who, z_info->r_max, u16b);
}

