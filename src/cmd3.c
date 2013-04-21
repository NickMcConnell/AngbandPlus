/* File: cmd3.c */

/*
 * Copyright (c) 2001 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
	prt("(Inventory) Command: ", 0, 0);

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
	/*Hack - don't allow quest items to be worn*/
	if(o_ptr->ident & (IDENT_QUEST)) return (FALSE);

	/* Check for a usable slot */
	if (wield_slot(o_ptr) >= INVEN_WIELD) return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}


static int quiver_wield(int item, object_type *o_ptr)
{
	int slot = 0;
	bool use_new_slot = FALSE;
	int num;
	int i;

	object_type object_type_body;
	object_type *i_ptr;

	/* was: num = get_quantity(NULL, o_ptr->number);*/
	num = o_ptr->number;

	/* Cancel */
	if (num <= 0) return 0;

	/* Check free space */
	if (!quiver_carry_okay(o_ptr, num, item))
	{
		msg_print("Your quiver needs more backpack space.");
		return 0;
 	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy to the local object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = num;

	/* Search for available slots. */
	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* Get the item */
		o_ptr = &inventory[i];

		/* Accept empty slot, but keep searching for combining */
		if (!o_ptr->k_idx)
		{
			slot = i;
			use_new_slot = TRUE;
			continue;
		}

		/* Accept slot that has space to absorb more */
		if (object_similar(o_ptr, i_ptr))
		{
			slot = i;
			use_new_slot = FALSE;
			object_absorb(o_ptr, i_ptr);
			break;
		}
	}

	/* Use a new slot if needed */
	if (use_new_slot) object_copy(&inventory[slot], i_ptr);

	if (!slot)
	{
		/* No space. */
		msg_print("Your quiver is full.");
		return 0;
	}

	/* Increase the weight */
	p_ptr->total_weight += i_ptr->weight * num;

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

	/* Update "p_ptr->pack_size_reduce" */
	find_quiver_size();

	/* Reorder the quiver and return the perhaps modified slot */
	return reorder_quiver(slot);
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
	int item, slot;

	u32b f1, f2, f3, fn, pf1, pf2, pf3, pf4;

	object_type *o_ptr, *shield_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	cptr act;

	cptr q, s;

	char o_name[80];


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

	/* Hack - Throwing weapons can be wielded in the quiver too. Ask the player */
	if (is_throwing_weapon(o_ptr) && !IS_QUIVER_SLOT(slot) &&
		get_check("Do you want to put it in the quiver? ")) slot = INVEN_QUIVER;

	/* Prevent wielding into a cursed slot */
	if (!IS_QUIVER_SLOT(slot) && cursed_p(&inventory[slot]))
	{
		/* Describe it */
		object_desc(o_name, sizeof(o_name), &inventory[slot], FALSE, 0);

		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
		           o_name, describe_use(slot));

		/* Cancel the command */
		return;
	}


	if (p_ptr->twoh_weapon && slot==INVEN_ARM){
		msg_format("You cannot wear a shield when you are wielding a two-handed weapon.");
		return;
	} 

	if (slot==INVEN_WIELD){
		object_flags(o_ptr, &f1, &f2, &f3, &fn);
		if (f2 & (TR2_2H)){
			player_flags(&pf1, &pf2, &pf3, &pf4);
			shield_ptr = &inventory[INVEN_ARM];
			if (pf2 & (TR2_NO_2H)){
				msg_format("Embarrassingly, you are too short to wield such a long weapon.");
				return;	
			} else if (shield_ptr->k_idx){
				msg_format("You cannot wield a two-handed weapon when you are wearing a shield.");
				return;		
			}
		}
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Ammo goes in quiver slots, which have special rules. */
	if (IS_QUIVER_SLOT(slot))
	{
		/* Put the ammo in the quiver */
		slot = quiver_wield(item, o_ptr);

		/* Can't do it */
		if (!slot) return;

		/* Get the object again */
		o_ptr = &inventory[slot];

		o_ptr->ident |= (IDENT_WIELDED);
		if (o_ptr->ident & (IDENT_SENSE) && (o_ptr->discount==INSCRIP_AVERAGE || o_ptr->discount==INSCRIP_GOOD_STRONG)){
			object_known(o_ptr);
		}
	}
	else
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Decrease the item (from the pack) */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_optimize(item);
		}

		/* Decrease the item (from the floor) */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_optimize(0 - item);
		}

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

		o_ptr->ident |= (IDENT_WIELDED);
		if (o_ptr->ident & (IDENT_SENSE) && (o_ptr->discount==INSCRIP_AVERAGE || o_ptr->discount==INSCRIP_GOOD_STRONG)){
			object_known(o_ptr);
		}

		/* Increase the weight */
		p_ptr->total_weight += i_ptr->weight;

		/* Increment the equip counter by hand */
		p_ptr->equip_cnt++;

		/* Autoinscribe if the object was on the floor */
		if (item < 0) apply_autoinscription(o_ptr);
	}

	if (slot==INVEN_LITE || slot==INVEN_LEFT || slot==INVEN_RIGHT || slot==INVEN_NECK){
		do_ident_item(item, o_ptr, 1);
	}

	/* Where is the item now */
	if (slot == INVEN_WIELD)
	{
		act = "You are wielding";
	}
	else if (slot == INVEN_BOW)
	{
		act = "You are shooting with";
	}
	else if (slot == INVEN_LITE)
	{
		act = "Your light source is";
	}
	else if (!IS_QUIVER_SLOT(slot))
	{
		act = "You are wearing";
	}
	else
	{
		act = "You have readied";
	}

	/* Describe the result */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Message */
	sound(MSG_WIELD);
	msg_format("%s %s (%c).", act, o_name, index_to_label(slot));

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		sound(MSG_CURSED);
		if (o_ptr->tval==TV_BOLT || o_ptr->tval==TV_SHOT || o_ptr->tval==TV_ARROW){
			msg_print("Oops! It feels untrustworthy...");
		} else {
			msg_print("Oops! It feels deathly cold!");
		}

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

		/* Sense the object if allowed */
		if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_CURSED;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS | PU_NATIVE);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	p_ptr->redraw |= (PR_EQUIPPY | PR_RESIST | PR_EXP | PR_STATS);
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

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Item is cursed */
	if (cursed_p(o_ptr) && (!(IS_QUIVER_SLOT(item))))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Take a partial turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE / 2;

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


	/* Get an item */
	q = "Drop which item? ";
	s = "You have nothing to drop.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;

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
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr)  && (!(IS_QUIVER_SLOT(item))))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Take a partial turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE / 2;

	/* Drop (some of) the item */
	inven_drop(item, amt);
}

/*
 * An "item_tester_hook" for destroying objects
 */
static bool item_tester_hook_destroy(const object_type *o_ptr)
{

	/* Don't bother with known artifacts */
	return ( !artifact_known(o_ptr) );
}



/*
 * Handle the user command to destroy an item
 */
void do_cmd_destroy(void)
{
	int item;
	cptr q, s;

	item_tester_hook = item_tester_hook_destroy;

	/* Get an item */
	q = "Destroy which item? ";
	s = "You have nothing to destroy.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	destroy_item(item);
}


/*
 * Destroy an item
 */
void destroy_item(int item)
{
	int amt;
	int old_number;
	int old_charges = 0;

	object_type *o_ptr;

	char o_name[80];

	char out_val[160];

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

	/*hack -  make sure we get the right amount displayed*/
	o_ptr->number = amt;

	/*now describe with correct amount*/
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/*reverse the hack*/
	o_ptr->number = old_number;

	/* Verify destruction */
	if (verify_destroy)
	{
		int result;

		/* Check for known ego-items */
		strnfmt(out_val, sizeof(out_val), "Really Destroy %s? ", o_name);

		/* Check for known ego-items */
		if (ego_item_p(o_ptr) && object_known_p(o_ptr))
		{
			result = get_check_other(out_val, 'E');

			/* returned "no"*/
			if (!result) return;

			/*return of 2 sets ego item type to squelch*/
			else if (result == 2)
			{
				/* get the ego item type */
				ego_item_type *e_ptr = &e_info[o_ptr->ego_num];

				/* set to squelch */
				e_ptr->squelch = TRUE;

				/* message */
				msg_format("Ego-item type %s is now set to be squelched upon identification.", e_name + e_ptr->name);
			}
		}

		/* Check for aware objects */
		else if (object_aware_p(o_ptr) &&
		    	!(k_info[o_ptr->k_idx].k_flags3 & (TR3_INSTA_ART)))
		{

			result = get_check_other(out_val, 's');

			/* returned "no"*/
			if (!result) return;

			/*return of 2 sets item to squelch*/
			else if (result == 2)
			{
				object_kind *k_ptr = &k_info[o_ptr->k_idx];
				char o_name2[80];

				/*make a fake object so we can give a proper message*/
				object_type *i_ptr;
				object_type object_type_body;

				/* Get local object */
				i_ptr = &object_type_body;

				/* Wipe the object */
				object_wipe(i_ptr);

				/* Create the object */
				object_prep(i_ptr, o_ptr->k_idx);

				/*make it plural*/
				i_ptr->number = 2;

				/*now describe with correct amount*/
				object_desc(o_name2, sizeof(o_name2), i_ptr, FALSE, 0);

				/*set to squelch*/
				k_ptr->squelch = SQUELCH_ALWAYS;

				/* Message - no good routine for extracting the plain name*/
				msg_format("All %^s will always be squelched.", o_name2);

				/*Mark the view to be updated*/
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW);;
			}
		}

		/* Unaware object, simple yes/no prompt */
		else if (!get_check(out_val)) return;
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Artifacts cannot be destroyed */
	if (artifact_p(o_ptr))
	{
		/* Message */
		msg_format("You cannot destroy %s.", o_name);

		/* Don't mark id'ed objects */
		if (object_known_p(o_ptr)) return;

		/* It has already been sensed */
		if (o_ptr->ident & (IDENT_SENSE))
		{
			/* Already sensed objects always get improved feelings */
			o_ptr->discount = INSCRIP_SPECIAL;
		}
		else
		{
			/* Mark the object as indestructible */
			o_ptr->discount = INSCRIP_INDESTRUCTIBLE;
		}

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		p_ptr->update |= (PU_NATIVE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		p_ptr->redraw |= (PR_EQUIPPY | PR_RESIST | PR_EXP | PR_STATS | PU_NATIVE);

		/* Done */
		return;
	}

	/* Sometimes identify the object that is being destroyed */
	/* We try hard to prevent scumming (example: stacks of unknown ammo) */
//	if ((amt == o_ptr->number) && object_aware_p(o_ptr) && !object_known_p(o_ptr) && !ammo_p(o_ptr))
//	{
//		/* Identify the object */
//	       	object_known(o_ptr);
//
//		/* Describe the object again */
//		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
//	}

	/* Message */
	message_format(MSG_DESTROY, 0, "You destroy %s.", o_name);

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
 * Observe an item, displaying what is known about it
 */
void do_cmd_observe(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;


	/* Get an item */
	q = "Examine which item? ";
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

	/* Describe */
	object_info_screen(o_ptr);
}


/*
 * Find out if we have to show the prompts for auto(un)inscribe object kinds
 * based on an instance object inscription.
 * We show them if:
 *    The player enabled this feature
 *    The object is aware (to not reveal flavors) and
 *    The object isn't an special artifact template and
 *    The object isn't an identified artifact and
 *    The object isn't ammo.
 */
#define ACCESS_AUTOINSCRIPTIONS(o_ptr) \
(expand_inscribe && \
object_aware_p(o_ptr) && \
!(k_info[(o_ptr)->k_idx].k_flags3 & TR3_INSTA_ART) && \
!(artifact_p(o_ptr) && object_known_p(o_ptr)) && \
!ammo_p(o_ptr))


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
	if (!o_ptr->obj_note)
	{
		msg_print("That item had no inscription to remove.");
		return;
	}

	/* Remove the inscription */
	o_ptr->obj_note = 0;

	/*The object kind has an autoinscription*/
	if (ACCESS_AUTOINSCRIPTIONS(o_ptr) &&
	    (get_autoinscription_index(o_ptr->k_idx) != -1))
	{
		char tmp_val[160];
		char o_name2[80];

		/*make a fake object so we can give a proper message*/
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Create the object */
		object_prep(i_ptr, o_ptr->k_idx);

		/*make it plural*/
		i_ptr->number = 2;

		/*now describe with correct amount*/
		object_desc(o_name2, sizeof(o_name2), i_ptr, FALSE, 0);

		/* Prompt */
		strnfmt(tmp_val, sizeof(tmp_val),
			"Remove automatic inscription for %s?", o_name2);

		/* Auto-Inscribe if they want that */
		if (get_check(tmp_val)) remove_autoinscription(o_ptr->k_idx);
	}

	/* Message */
	msg_print("Inscription removed.");

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

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
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Message */
	msg_format("Inscribing %s.", o_name);
	message_flush();

	/* Start with nothing */
	strcpy(tmp, "");

	/* Use old inscription */
	if (o_ptr->obj_note)
	{
		/* Start with the old inscription */
		strnfmt(tmp, sizeof(tmp), "%s", quark_str(o_ptr->obj_note));
	}

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", tmp, sizeof(tmp)))
	{
		char tmp_val[160];
		char o_name2[80];

		/*make a fake object so we can give a proper message*/
		object_type *i_ptr;
		object_type object_type_body;

		/* Add an autoinscription? */
		if (ACCESS_AUTOINSCRIPTIONS(o_ptr))
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Create the object */
			object_prep(i_ptr, o_ptr->k_idx);

			/*make it plural*/
			i_ptr->number = 2;

			/*now describe with correct amount*/
			object_desc(o_name2, sizeof(o_name2), i_ptr, FALSE, 0);

			/* Prompt */
			strnfmt(tmp_val, sizeof(tmp_val),
				"Automatically inscribe all %s with %s?",
				o_name2, tmp);

			/* Auto-Inscribe if they want that */
			if (get_check(tmp_val)) add_autoinscription(o_ptr->k_idx, tmp);
		}

		/* Expand certain patterns in inscriptions */
		expand_inscription(o_ptr, tmp, tmp_val, sizeof(tmp_val));

		/* Save the inscription */
		o_ptr->obj_note = quark_add(tmp_val);

		/* Combine / Reorder the pack */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

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
	    (o_ptr->timeout > 0))
	{
		return (TRUE);
	}

	/* Assume not okay */
	return (FALSE);
}


/*
 * Lanterns can be filled with oil on the dungeon's floor.
 * Returns TRUE if the amount of fuel could be filled in this way.
 */
static bool do_cmd_refill_lamp_from_terrain(void)
{
	object_type *j_ptr;

	/* Get player coordinates */
	int y = p_ptr->py;
	int x = p_ptr->px;

	/* Assume that the oil can be dry */
	bool can_dry = TRUE;

	/* Check presence of oil */
	if (!(cave_ff3_match(y, x, FF3_OIL) && cave_passable_bold(y, x))) return (FALSE);

	/* Ask the user if he/she wants to use the oil on the grid */
	if (!get_check("Do you want to refill your lamp from the oil on the floor? ")) return (FALSE);

	/* Get the lantern */
	j_ptr = &inventory[INVEN_LITE];

	/* Increase fuel amount (not too much) */
	j_ptr->timeout += (200 + rand_int(3) * 50);

	/* Increase fuel amount even more if there is more oil on that grid */
	if (cave_ff2_match(y, x, FF2_DEEP)) j_ptr->timeout += (100 + rand_int(2) * 50);

	/* Message */
	msg_print("You fuel your lamp.");

	/* Comment */
	if (j_ptr->timeout >= FUEL_LAMP)
	{
		/* Some oil was unused */
		if (j_ptr->timeout > FUEL_LAMP)
		{
			/* Set oil to the max. value */
			j_ptr->timeout = FUEL_LAMP;

			/* Give the remaining oil to the grid */
			can_dry = FALSE;
		}

		/* Message */
		msg_print("Your lamp is full.");
	}

	/* Sometimes we remove the oil grid to stop oil-abuse */
	if (can_dry && f_info[FEAT_EARTH].name && one_in_(7))
	{
		/* Transform to earth */
		cave_set_feat(y, x, FEAT_EARTH);

		/* Message */
		msg_print("The oil patch dries.");
	}

	/* Take a partial turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE / 2;

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP);

	/* Success */
	return (TRUE);
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
	p_ptr->p_energy_use = BASE_ENERGY_MOVE / 2;

	/* Get the lantern */
	j_ptr = &inventory[INVEN_LITE];

	/* Refuel from a latern */
	if (o_ptr->sval == SV_LITE_LANTERN)
	{
		j_ptr->timeout += o_ptr->timeout;
	}
	/* Refuel from a flask */
	else
	{
		j_ptr->timeout += o_ptr->pval;
	}

	/* Message */
	msg_print("You fuel your lamp.");

	/* Comment */
	if (j_ptr->timeout >= FUEL_LAMP)
	{
		j_ptr->timeout = FUEL_LAMP;
		msg_print("Your lamp is full.");
	}

	/* Refilled from a latern */
	if (o_ptr->sval == SV_LITE_LANTERN)
	{
		/* Unstack if necessary */
		if (o_ptr->number > 1)
		{
			object_type *i_ptr;
			object_type object_type_body;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Obtain a local object */
			object_copy(i_ptr, o_ptr);

			/* Modify quantity */
			i_ptr->number = 1;

			/* Remove fuel */
			i_ptr->timeout = 0;

			/* Unstack the used item */
			o_ptr->number--;
			p_ptr->total_weight -= i_ptr->weight;

			/* Carry or drop */
			if (item >= 0)
				item = inven_carry(i_ptr);
			else
				drop_near(i_ptr, 0, p_ptr->py, p_ptr->px);
		}

		/* Empty a single latern */
		else
		{
			/* No more fuel */
			o_ptr->timeout = 0;
		}


		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* Refilled from a flask */
	else
	{
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
	    (o_ptr->sval == SV_LITE_TORCH || o_ptr->sval == SV_LITE_MAGELIGHT)) return (TRUE);

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
	p_ptr->p_energy_use = BASE_ENERGY_MOVE / 2;

	/* Get the primary torch */
	j_ptr = &inventory[INVEN_LITE];

	/* Refuel */
	j_ptr->timeout += o_ptr->timeout + 5;

	/* Message */
	msg_print("You combine the torches.");

	/* Over-fuel message */
	if (j_ptr->timeout >= FUEL_TORCH)
	{
		j_ptr->timeout = FUEL_TORCH;
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
		/* First we check if the lamp can be filled from the oil on the floor */
		if (!do_cmd_refill_lamp_from_terrain())
		{
			do_cmd_refill_lamp();
		}
	}

	/* It's a torch */
	else if (o_ptr->sval == SV_LITE_TORCH || o_ptr->sval == SV_LITE_MAGELIGHT)
	{
		do_cmd_refill_torch();
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
	y1 = Term->offset_y;
	x1 = Term->offset_x;

	/* Show panels until done */
	while (1)
	{
		/* Get the current panel */
		y2 = Term->offset_y;
		x2 = Term->offset_x;

		/* Describe the location */
		if ((y2 == y1) && (x2 == x1))
		{
			tmp_val[0] = '\0';
		}
		else
		{
			strnfmt(tmp_val, sizeof(tmp_val), "%s%s of",
			        ((y2 < y1) ? " north" : (y2 > y1) ? " south" : ""),
			        ((x2 < x1) ? " west" : (x2 > x1) ? " east" : ""));
		}

		/* Prepare to ask which way to look */
		strnfmt(out_val, sizeof(out_val),
		        "Map sector [%d,%d], which is%s your sector.  Direction?",
		        (y2 / PANEL_HGT), (x2 / PANEL_WID), tmp_val);

		/* More detail */
		if (center_player)
		{
			strnfmt(out_val, sizeof(out_val),
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
		change_panel(dir);

		/* Handle stuff */
		handle_stuff();
	}

	/* Verify panel */
	verify_panel();
}



/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static cptr ident_info[] =
{
	" :A dark grid",
	"!:A potion (or oil)",
	"\":An amulet (or necklace)",
	"#:A wall (or secret door)",
	"$:Treasure (gold or gems)",
	"%:A vein (magma or quartz)",
	/* "&:unused", */
	"':An open door",
	"(:Soft armor",
	"):A shield",
	"*:A vein with treasure",
	"+:A closed door",
	",:Food (or mushroom patch)",
	"-:A wand (or rod)",
	".:Floor",
	"/:A polearm (Axe/Pike/etc)",
	/* "0:unused", */
	"1:Entrance to General Store",
	"2:Entrance to Armory",
	"3:Entrance to Weaponsmith",
	"4:Entrance to Temple",
	"5:Entrance to Alchemy shop",
	"6:Entrance to Magic store",
	"7:Entrance to Black Market",
	"8:Entrance to your home",
	"9:The Adventurer's Guild",
	"::Rubble",
	";:A glyph of warding or glacier",
	"<:An up staircase",
	"=:A ring",
	">:A down staircase",
	"?:A scroll",
	"@:You",
	"A:Ainu/Maia",
	"B:Bird",
	"C:Canine",
	"D:Ancient Dragon/Wyrm",
	"E:Elemental",
	"F:Dragon Fly",
	"G:Ghost",
	"H:Hybrid",
	"I:Insect",
	"J:Snake",
	"K:Killer Beetle",
	"L:Lich",
	"M:Multi-Headed Reptile",
	/* "N:unused", */
	"O:Ogre",
	"P:Giant Humanoid",
	"Q:Quylthulg (Pulsing Flesh Mound)",
	"R:Reptile/Amphibian",
	"S:Spider/Scorpion/Tick",
	"T:Troll",
	"U:Major Demon",
	"V:Vampire",
	"W:Wight/Wraith/etc",
	"X:Xorn/Xaren/etc",
	"Y:Yeti",
	"Z:Zephyr Hound",
	"[:Hard armor",
	"\\:A hafted weapon (mace/whip/etc)",
	"]:Misc. armor",
	"^:A trap",
	"_:A talisman",
	/* "`:unused", */
	"a:Ant",
	"b:Bat",
	"c:Centipede",
	"d:Dragon",
	"e:Floating Eye",
	"f:Feline",
	"g:Golem",
	"h:Hobbit/Elf/Dwarf",
	"i:Icky Thing",
	"j:Jelly",
	"k:Kobold",
	"l:Louse",
	"m:Mold",
	"n:Naga",
	"o:Orc",
	"p:Person/Human",
	"q:Quadruped",
	"r:Rodent",
	"s:Skeleton",
	"t:Townsperson",
	"u:Minor Demon",
	"v:Vortex",
	"w:Worm/Worm-Mass",
	/* "x:unused", */
	"y:Yeek",
	"z:Zombie/Mummy",
	"{:A missile (arrow/bolt/shot)",
	"|:An edged weapon (sword/dagger/etc)",
	"}:A launcher (bow/crossbow/sling)",
	"~:A chest (or miscellaneous item)",
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
		z1 = l_list[w1].pkills;
		z2 = l_list[w2].pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = l_list[w1].tkills;
		z2 = l_list[w2].tkills;

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
void ang_sort_swap_hook(void *u, void *v, int a, int b)
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
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "multiple" monsters:
 *   ^A (all monsters)
 *   ^U (all unique monsters)
 *   ^N (all non-unique monsters)
 *
 * The responses may be sorted in several ways, see below.
 *
 *
 */
void do_cmd_query_symbol(void)
{
	int i, n, r_idx;
	char sym, query;
	char buf[128];

	bool all = FALSE;
	bool uniq = FALSE;
	bool norm = FALSE;

	bool recall = FALSE;

	u16b why = 0;
	u16b *who;


	/* Get a character, or abort */
	if (!get_com("Enter character to be identified: ", &sym)) return;

	/* Find that character info, and describe it */
	for (i = 0; ident_info[i]; ++i)
	{
		if (sym == ident_info[i][0]) break;
	}

	/* Describe */
	if (sym == KTRL('A'))
	{
		all = TRUE;
		strcpy(buf, "Full monster list.");
	}
	else if (sym == KTRL('U'))
	{
		all = uniq = TRUE;
		strcpy(buf, "Unique monster list.");
	}
	else if (sym == KTRL('N'))
	{
		all = norm = TRUE;
		strcpy(buf, "Non-unique monster list.");
	}
	else if (ident_info[i])
	{
		strnfmt(buf, sizeof(buf), "%c - %s.", sym, ident_info[i] + 2);
	}
	else
	{
		strnfmt(buf, sizeof(buf), "%c - %s.", sym, "Unknown Symbol");
	}

	/* Display the result */
	prt(buf, 0, 0);

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Nothing to recall */
		if ((!cheat_know || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
		    && !l_ptr->sights) continue;

		/* Require non-unique monsters if needed */
		if (norm && (r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Require unique monsters if needed */
		if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Collect "appropriate" monsters */
		if (all || (r_ptr->d_char == sym)) who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* XXX XXX Free the "who" array */
		FREE(who);

		return;
	}


	/* Prompt */
	put_str("Recall details? (k/p/y/n): ", 0, 40);

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

	/* Sort by level */
	if (query == 'p')
	{
		why = 2;
		query = 'y';
	}

	/* Catch "escape" */
	if (query != 'y')
	{
		/* XXX XXX Free the "who" array */
		FREE(who);

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


	/* Start at the end */
	i = n - 1;

	/* Scan the monster memory */
	while (1)
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
		Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");

		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Save screen */
				screen_save();

				/* Recall on screen */
				screen_roff(who[i]);

				/* Hack -- Complete the prompt (again) */
				Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
			}

			/* Command */
			query = inkey();

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
		if (query == '-')
		{
			if (++i == n)
			{
				i = 0;
				if (!expand_list) break;
			}
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
			{
				i = n - 1;
				if (!expand_list) break;
			}
		}
	}


	/* Re-display the identity */
	prt(buf, 0, 0);

	/* Free the "who" array */
	FREE(who);
}


/*
 * Rogues may steal from monsters.  The monster needs to have
 * something to steal (it must drop some form of loot).
 *  Fail to steal too often on a level, and monsters will be more wary,
 * and the hue and cry will be eventually be raised.  Having every
 * monster on the level awake and aggravated is not pleasant. -LM-
 */
void py_steal(int y, int x)
{
	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char m_name[80];

	int i;
	int effect, theft_protection;
	bool teststeal = FALSE;
	int filching_power = 0;
	int counter = 0;
	int counter2 = 0;

	bool thief = FALSE;

	/* Try all you like, but fail enough and the door is closed to you. */
	if (recent_failed_thefts > 35)
	{
		msg_print("Everyone is keeping a lookout for you.  You can steal nothing here.");

	}

	/*before we go through everything else,
	 *sometimes monsters aren't carry anything*/
	else if (r_ptr->flags1 & (RF1_DROP_60))
	{
    	if (rand_int(100) >= 60)
		{
			  /* Pockets are empty. */
			msg_print("You burgle only dust.");

		}

		/*try to steal something*/
		else teststeal = TRUE;
	}

	else if (r_ptr->flags1 & (RF1_DROP_90))
	{
    	if (rand_int(100) >= 90)
		{
			  /* Pockets are empty. */
			msg_print("You burgle only dust.");

		}

		/*try to steal something*/
		else teststeal = TRUE;
	}

	/*not all monsters have treasure drops*/
	else if (r_ptr->flags1 & (RF1_DROP_1D2))	teststeal = TRUE;
 	else if (r_ptr->flags1 & (RF1_DROP_2D2))	teststeal = TRUE;
	else if (r_ptr->flags1 & (RF1_DROP_3D2))	teststeal = TRUE;
	else if (r_ptr->flags1 & (RF1_DROP_4D2))	teststeal = TRUE;

	/* Pockets are empty. */
	else msg_print("You find nothing to steal");

	/* Determine the cunning of the thief, based on level & disarming skill. */
	filching_power = (2 * p_ptr->lev) +  (p_ptr->skill_dis / 2);

	/*dex bonus counts extra*/
	filching_power += (adj_dex_dis[p_ptr->stat_ind[A_DEX]]* 3);

	/*easier on a sleeping monster*/
	if (m_ptr->csleep)
	{
		/*easier with better stealth*/
		filching_power += (p_ptr->skill_stl * 2);

		/*extra credit for extreme stealth*/
		if (p_ptr->skill_stl > 10) filching_power += ((p_ptr->skill_stl - 10) * 3);
	}

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) filching_power = filching_power / 10;
	if (p_ptr->confused || p_ptr->image) filching_power = filching_power / 10;

	/* Determine how much protection the monster has. */
	theft_protection = ((r_ptr->level * 2) + 35);

	/*smarter monsters are harder to steal from*/
	if (r_ptr->flags2 & (RF2_SMART))
	{
		theft_protection *= 3;
		theft_protection /= 2;
	}

	/*speeling, stunned, or confused monsters are harder to steal from*/
	if (m_ptr->csleep) theft_protection /= 2;
	if (m_ptr->confused) theft_protection /= 3;
	if (m_ptr->stunned) theft_protection /= 5;

	/* now adjust for speed*/
	theft_protection += (m_ptr->mspeed - p_ptr->pspeed);

	/*enforce a minimum - should almost never be necessary*/
	if (theft_protection < 1) theft_protection = 1;

	/* Send a thief to catch a thief. */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
	{
		/* Extract infomation about the blow effect */
		effect = r_ptr->blow[i].effect;
		if (effect == RBE_EAT_GOLD) thief = TRUE;
		else if (effect == RBE_EAT_ITEM) thief = TRUE;
	}
	if (thief) theft_protection += 30;

	/*sleeping monsters are much easier*/
	if (m_ptr->csleep)
	{
		theft_protection /= 2;
	}

	/*creatures who have guaranteed good or great times are very protective
	 *creatures who hold chests are INCREDIBLY protective -JG
	 */
	if (r_ptr->flags1 & (RF1_DROP_CHEST)) theft_protection *= 2;
	else if (r_ptr->flags1 & (RF1_DROP_GREAT)) theft_protection = (theft_protection * 5 / 3);
	else if (r_ptr->flags1 & (RF1_DROP_GOOD)) theft_protection = (theft_protection * 3 / 2);

	/* The more you fail to steal, the more wary the monsters. */
	theft_protection += (recent_failed_thefts * 5) / 2;

	if (teststeal)
	{

		/* Did the theft succeed, give object to player.  */
		if (randint(filching_power) >= theft_protection)
		{
			steal_object_from_monster(y, x);

			/*lower theft protection to 90% of orig value for next check*/
			theft_protection *= 9; theft_protection /= 10;
		}

		else
		{
			/* Pockets are empty. */
			msg_print("You fail to steal.");

			/*increase theft protection to 150% of orig value for next check*/
			theft_protection *= 3; theft_protection /= 2;
		}
	}


	/* The victim normally, but not always, wakes up and is aggravated. */
	if ((m_ptr->csleep == 0) || (randint(filching_power) >= theft_protection))
	{
		m_ptr->csleep = 0;

		/* Give the player a message. */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);
		msg_format("You have irritated %s.", m_name);

		/*add to the haste counter and give message*/
		set_monster_haste(cave_m_idx[m_ptr->fy][m_ptr->fx],
						  m_ptr->hasted + 100 + rand_int(100), TRUE);

		/*possibly update the monster health bar*/
		if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])
				p_ptr->redraw |= (PR_HEALTH);
	}

	/* Hack - count all the wary monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		if (m_ptr->mflag & (MFLAG_WARY)) counter += 1;
	}

	/*make the monsters who saw wary*/
	(void) project_los(y, x, 0, GF_MAKE_WARY);

	/* Hack - count all the wary monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		if (m_ptr->mflag & (MFLAG_WARY)) counter2 += 1;
	}

	/*let the player know who saw, and aggravate the monsters*/
	if ((m_ptr->csleep == 0) || (counter2 > counter))
	{
		/*dungeon gets different treatment than town*/
		if (p_ptr->depth)
		{
			/*increase the recent thefts counter*/
			recent_failed_thefts += 10;

			/* Increment the number of thefts in the dungeon, and possibly raise the hue and cry. */

			if (recent_failed_thefts > 30)
			{
				/* Wake up and haste the whole dungeon. */
				mass_aggravate_monsters(SOURCE_PLAYER);

				/* Message */
				msg_print("The Pits of Angband roar with anger!");
			}

			else if ((recent_failed_thefts > 15) || (randint(8) == 1))
			{
				msg_print("You hear hunting parties scouring the area for a notorious burglar.");

				/* Aggravate monsters nearby. */
				aggravate_monsters(SOURCE_PLAYER);
			}

		}
		else
		{
			static store_type *st_ptr = NULL;

			int x;

			/*the town gets aggravated right away, and the shops close*/
			mass_aggravate_monsters(SOURCE_PLAYER);

			msg_print("The furious townspeople search for the notorious burglar!");

			/*close the shops*/
			for (x = 0; x < MAX_STORES; x++)
			{
				st_ptr = &store[x];

				/* Skip home */
				if ((x == STORE_HOME) || (STORE_GUILD)) continue;

				/*first close the shops*/
				if (st_ptr->store_open < turn) st_ptr->store_open = turn;

				/* Add several days to their opening*/
				st_ptr->store_open += 50000 + randint(25000);
			}

		}
	}

	/* The thief also speeds up, but only for just long enough to escape. */
	if (!p_ptr->fast) p_ptr->fast += 2;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

}





/*create a monster trap, currently used from a scroll*/

bool make_monster_trap(void)
{
	int y, x, dir;

	/* Get a direction */
	if (!get_rep_dir(&dir)) return (FALSE);

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/*check for an empty floor, monsters, and objects*/
	if (!cave_trappable_bold(y, x) || cave_m_idx[y][x])
	{
		msg_print ("You can only make a trap on an empty floor space.");

		return(FALSE);
	}

	/*set the trap*/
	py_set_trap(y, x);

	return(TRUE);
}

/*
 * Rogues may set traps, or they can be set from a scroll.
 * Only one such trap may exist at any one time,
 * but an old trap can be disarmed to free up equipment for a new trap.
 * -LM-
 */
void py_set_trap(int y, int x)
{

	if (p_ptr->blind || no_lite())
	{
		msg_print("You can not see to set a trap.");
		return;
	}

	if (p_ptr->confused || p_ptr->image)
	{
		msg_print("You are too confused.");
		return;
	}

	/* Set the trap, and draw it. */
	set_effect_trap_player(FEAT_MTRAP_BASE, y, x);

	/* Remember the location */
	cave_info[y][x] |= (CAVE_MARK);

	/* Notify the player. */
	msg_print("You set a monster trap.");

	lite_spot(y, x);

	/*make the monsters who saw wary*/
	(void)project_los(y, x, 0, GF_MAKE_WARY);

	/* Increment the number of monster traps. */
	num_trap_on_level++;

}

/*
 * Choose advanced monster trap type
 */
static bool choose_mtrap(int *choice)
{
	int num;

	char c;

	bool done=FALSE;

	/* Save screen */
	screen_save();

	prt("        Choose an advanced monster trap (ESC to cancel):", 1, 8);

	num = 1 + (p_ptr->lev / 6);

                  prt("    a) Sturdy Trap          - (less likely to break)", 2, 8);
	if (num >= 2) prt("    b) Slowing Trap         - (slows monsters)", 3, 8);
	if (num >= 3) prt("    c) Confusion Trap       - (confuses monsters)", 4, 8);
	if (num >= 4) prt("    d) Poison Gas Trap      - (creates a toxic cloud)", 5, 8);
	if (num >= 5) prt("    e) Life Draining Trap   - (Hurts living monsters)", 6, 8);
	if (num >= 6) prt("    f) Lightning Trap       - (shoots a lightning bolt)", 7, 8);
	if (num >= 7) prt("    g) Explosive Trap       - (causes area damage)", 8, 8);
	if (num >= 8) prt("    h) Portal Trap          - (teleports monsters)", 9, 8);
	if (num >= 9) prt("    i) Dispel Monsters Trap - (hurt all monsters in area)", 10, 8);

	while (!done)
	{
		c = inkey();

		/* Letters are used for selection */
		if (isalpha(c))
		{
			if (islower(c))
			{
				*choice = A2I(c);
			}
			else
			{
				*choice = c - 'A' + 26;
			}

			/* Validate input */
			if ((*choice > -1) && (*choice < num))
			{
				done = TRUE;
			}

			else
			{
				bell("Illegal response to question!");
			}
		}

		/* Allow user to exit the fuction */
        else if (c == ESCAPE)
        {
			/* Load screen */
			screen_load();

			return (FALSE);
        }

         /* Invalid input */
         else bell("Illegal response to question!");
	}

	/* Load screen */
	screen_load();

	/* Return */
	return (TRUE);
}

/*
 * Turn a basic monster trap into an advanced one -BR-
 */
bool py_modify_trap(int y, int x)
{
	int trap_choice = 0;

	if (p_ptr->blind || no_lite())
	{
		msg_print("You can not see to modify your trap.");
		return (FALSE);
	}

	/*no modifying traps on top of monsters*/
	if (cave_m_idx[y][x] > 0)
	{
		msg_print("There is a creature in the way.");
		return (FALSE);
	}

	if (!(choose_mtrap(&trap_choice))) return (FALSE);

	/* Set the trap, and draw it. */
	x_list[cave_x_idx[y][x]].x_f_idx = FEAT_MTRAP_BASE + 1 + trap_choice;

	/*check if player did not modify trap*/
	if (x_list[cave_x_idx[y][x]].x_f_idx == FEAT_MTRAP_BASE) return(FALSE);

	/*Mark it*/
	lite_spot(y,x);

	/* Notify the player. */
	msg_print("You modify the monster trap.");

	/*make the monsters who saw wary*/
	(void)project_los(y, x, 0, GF_MAKE_WARY);

	return (TRUE);
}


