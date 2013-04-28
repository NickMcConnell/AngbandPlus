
/* File: cmd3.c */

/*
 * Display inventory and equipment.  Wear and remove equipment.  Drop and
 * destroy.  Inspecting, inscribing, refueling objects.  Learn about a
 * symbol.  Stealing and trap setting.
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
	char string[80];

	/* Note that we are in "inventory" mode */
	p_ptr->command_wrk = (USE_INVEN);


	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the inventory */
	show_inven();

	/* Hack -- hide empty slots */
	item_tester_full = FALSE;


	/* Insert the total burden and character capacity into a string. */
	if (use_metric) sprintf(string,
		"(Inventory) burden %d.%d kg (%d%% of capacity). Command: ",
		make_metric(p_ptr->total_weight) / 10,
		make_metric(p_ptr->total_weight) % 10,
		p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);
	else sprintf(string,
		"(Inventory) burden %d.%d lb (%d%% of capacity). Command: ",
		p_ptr->total_weight / 10, p_ptr->total_weight % 10,
		p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);



	/* Output that string, and prompt for a command. */
	prt(string, 0, 0);

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
	char string[80];

	/* Note that we are in "equipment" mode */
	p_ptr->command_wrk = (USE_EQUIP);


	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	show_equip();

	/* Hack -- undo the hack above */
	item_tester_full = FALSE;


	/* Insert the total burden and character capacity into a string. */
	if (use_metric) sprintf(string,
		"(Equipment) burden %d.%d kg (%d%% of capacity). Command: ",
		make_metric(p_ptr->total_weight) / 10,
		make_metric(p_ptr->total_weight) % 10,
		p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);
	else sprintf(string,
		"(Equipment) burden %d.%d lb (%d%% of capacity). Command: ",
		p_ptr->total_weight / 10, p_ptr->total_weight % 10,
		p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);


	/* Output that string, and prompt for a command. */
	prt(string, 0, 0);


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
 * Allow either wield slot
 */
static bool slot_tester_hook_wieldable(const int slot)
{
	/* Melee weapon */
	if (slot == INVEN_WIELD) return (TRUE);

	/* Shield */
	if (slot == INVEN_ARM) return (TRUE);

	/* Neither */
	return (FALSE);
}


/*
 * Test to see if a weapon requires two hands to wield.
 *
 * Weapons with the flag "TR1_TWO_HANDED_REQ" always require two hands.
 *
 * Weapons with the flag "TR1_TWO_HANDED_DES" require two hands if the
 * character doesn't have 18/90 strength, plus /10 more for every five
 * pounds of weight.  For example, to wield a weapon flagged as two-
 * handed wield desired that weighs 25 pounds in one hand, the
 * character must have 18/140 strength.
 */
bool needs_two_hands(u32b f1, int weight)
{
	if (f1 & (TR1_TWO_HANDED_REQ)) return (TRUE);

	if (f1 & (TR1_TWO_HANDED_DES))
	{
		int str_required = 24 + weight / 50;

		/* Give characters with super strength a break */
		if (str_required > 36) str_required = 36;

		if (p_ptr->stat_ind[A_STR] < str_required)
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Wield or wear a single item from the pack or floor, if not shapechanged.
 *
 * Support for two-handed weapons, wielding two weapons, and quiver by -LM-.
 */
void do_cmd_wield(void)
{
	int item, slot, num;

	u32b f1, f2, f3;

	object_type *o_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	cptr act;

	cptr q, s;

	char o_name[120];

	bool remove_two_weapons = FALSE;


	/* Restrict the choices */
	item_tester_hook = item_tester_hook_wear;

	if (p_ptr->schange)
	{
		msg_print("You cannot wield new equipment while shapechanged.");
		msg_print("Use the ']' command to return to your normal form.");
		return;
	}

	/* Get an item */
	q = "Wear/Wield which item?";
	s = "You have nothing you can wear or wield.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
	item_to_object(o_ptr, item);

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

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
		q = "Replace which ring?";
		s = "Oops.";
		if (!get_item(&slot, q, s, USE_EQUIP)) return;
	}

	/* Wielding a weapon */
	if (is_melee_weapon(o_ptr))
	{
		/*
		 * New weapon requires two hands.
		 */
		if (needs_two_hands(f1, o_ptr->weight))
		{
			int num_weapons = 0;

			/* Count existing weapons. */
			i_ptr = &inventory[INVEN_WIELD];
			if (is_melee_weapon(i_ptr)) num_weapons++;

			i_ptr = &inventory[INVEN_ARM];
			if (is_melee_weapon(i_ptr)) num_weapons++;

			/*
			 * If we have two weapons, we confirm before replacing them
			 * both.  Otherwise, we avoid annoyance by just executing the
			 * command and outputting appropriate warning messages.
			 */
			if (num_weapons == 2)
			{
				if (!get_check("This weapon requires two hands.  Replace both your existing weapons?")) return;

				/* We will have to remove two weapons */
				remove_two_weapons = TRUE;
			}

			/*
			 * Note that weapons that need two hands must be wielded in the
			 * INVEN_WIELD slot, or there might be trouble with shields.
			 */
			slot = INVEN_WIELD;
		}

		/* Weapon needs only one hand. */
		else
		{
			/* Primary wield slot is occupied. */
			if (inventory[INVEN_WIELD].k_idx)
			{
				bool slot1_ok = FALSE;
				bool slot2_ok = FALSE;

				/* Get "dual wield" limit (half the heavy wield limit) */
				int hold = 5 * adj_str_hold[p_ptr->stat_ind[A_STR]];

				/* Get item in primary wield slot */
				i_ptr = &inventory[INVEN_WIELD];

				/* Weapon in the primary wield slot is light enough */
				if (o_ptr->weight + i_ptr->weight <= hold)
				{
					/* Hack -- to avoid annoyance, shields cancel dual wield */
					if (inventory[INVEN_ARM].tval != TV_SHIELD)
					{
						/* Weapon is of same type -- allow dual wield in slot 2 */
						if (i_ptr->tval == o_ptr->tval) slot2_ok = TRUE;
					}
				}

				/* Get item in secondary wield slot */
				i_ptr = &inventory[INVEN_ARM];

				/* Anything but a melee weapon allows wielding in slot 1 */
				if (!is_melee_weapon(i_ptr)) slot1_ok = TRUE;

				/* Weapon in the secondary wield slot is light enough */
				else if (o_ptr->weight + i_ptr->weight <= hold)
				{
					/* Weapon is of same type -- allow dual wield in slot 1 */
					if (i_ptr->tval == o_ptr->tval) slot1_ok = TRUE;
				}

				/* Weapon is too heavy to wield with either existing weapon. */
				if ((!slot1_ok) && (!slot2_ok))
				{
					msg_print("This weapon is too heavy to wield with either of your existing weapons.");

					/* Allow cancel */
					if (!get_check("Replace them both?")) return;

					/* We will have to remove two weapons */
					remove_two_weapons = TRUE;
				}

				/* Only weapon in primary wield slot is light enough */
				else if ((slot2_ok) && (!slot1_ok))
				{
					/* Automatically use secondary wield slot */
					slot = INVEN_ARM;
				}

				/* Only weapon in secondary wield slot is light enough */
				else if ((!slot2_ok) && (slot1_ok))
				{
					/* Automatically use primary wield slot */
					slot = INVEN_WIELD;
				}

				/* Both wield slots are available */
				else
				{
					/* Restrict the choices to "wieldable" slots */
					slot_tester_hook = slot_tester_hook_wieldable;

					/* Choose a slot */
					q = "Use which slot (a or i)?";
					s = "Oops.";
					if (!get_item(&slot, q, s, USE_EQUIP)) return;
				}
			}

			/*
			 * If the primary wield slot is not occupied, we assume the
			 * character is wielding no weapons (this assumption depends
			 * on code elsewhere).  Wield the new weapon into the primary
			 * wield slot.
			 */
			else slot = INVEN_WIELD;
		}
	}


	/* Prevent wielding into a cursed slot */
	if ((slot < INVEN_Q0) && cursed_p(&inventory[slot]))
	{
		/* Describe it */
		object_desc(o_name, &inventory[slot], FALSE, 0);

		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
			   o_name, describe_use(slot));

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
	if (slot == INVEN_Q0)
	{
		int ammo_num = 0;

		/* Get a quantity */
		num = get_quantity(NULL, o_ptr->number);

		/* Cancel */
		if (!num) return;

		/* Count number of missiles in the quiver slots. */
		ammo_num = quiver_count();

		/*
		 * If the ammo now being added will make the quiver take up another
		 * backpack slot, and there are none available, refuse to wield the
		 * new ammo.
		 */
		if ((ammo_num + num) > 99 * p_ptr->pack_size_reduce)
		{
			/* We have no more space. */
			if (p_ptr->inven_cnt >= INVEN_PACK - p_ptr->pack_size_reduce)
			{
				/* Unless we are emptying a slot in the process, we can't fit it */
				if (!((item >= 0) && (num == o_ptr->number)))
				{
					msg_print("Your quiver needs more backpack space.");
					return;
				}
			}
		}

		/* Find a slot that can hold more ammo. */
		slot = process_quiver(num, o_ptr);

		if (!slot)
		{
			/* No space. */
			msg_print("Your quiver is full.");
			return;
		}

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


	/* Get the item currently in the slot */
	o_ptr = &inventory[slot];


	/* Hack -- Removing two weapons at a time requires special-case code */
	if (remove_two_weapons)
	{
		/* Take off both existing weapons */
		(void)inven_takeoff(INVEN_WIELD, 255);
		(void)inven_takeoff(INVEN_ARM, 255);
	}

	/* Removal of one item */
	else
	{
		/* Handle existing item. */
		if (o_ptr->k_idx)
		{
			/* Take off existing item, unless in the quiver. */
			if ((slot < INVEN_Q0) || (slot > INVEN_Q9))
			{
				(void)inven_takeoff(slot, 255);
			}

			/* Combine existing ammo with new. */
			else
			{
				p_ptr->equip_cnt--;
				p_ptr->total_weight -= o_ptr->weight * o_ptr->number;
				i_ptr->number += o_ptr->number;

				/* Hack -- Blend "notes" */
				if (o_ptr->note != 0) i_ptr->note = o_ptr->note;
			}
		}
	}

	/* Wear the new stuff */
	object_copy(o_ptr, i_ptr);

	/* Increase the weight */
	p_ptr->total_weight += i_ptr->weight * i_ptr->number;

	/* Increment the equip counter by hand */
	p_ptr->equip_cnt++;


	/* Where is the item now? */
	if (slot == INVEN_WIELD)
	{
		act = "You are wielding";
	}
	else if (slot == INVEN_ARM)
	{
		if (i_ptr->tval == TV_SHIELD) act = "You are wearing";
		else act = "Your secondary weapon is";
	}
	else if (slot == INVEN_BOW)
	{
		act = "You are shooting with";
	}
	else if (slot == INVEN_LITE)
	{
		act = "Your light source is";
	}
	else if ((slot >= INVEN_Q0) && (slot <= INVEN_Q9))
	{
		act = "You have readied";
	}
	else
	{
		act = "You are wearing";
	}

	/* Describe the result */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	msg_format("%s %s (%c).", act, o_name, index_to_label(slot));


	/* Artifacts can belong to sets   -GS- */
	if (o_ptr->artifact_index)
	{
		/* Get this artifact */
		artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

		/* If it completes a set, apply the set bonuses */
		if (check_set(a_ptr->set_index)) apply_set(a_ptr->set_index);
	}

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		msg_print("Oops! It feels deathly cold!");

		/* Note the curse */
		o_ptr->ident |= (IDENT_SENSE);
		if ((o_ptr->inscrip != INSCRIP_VERY_CURSED) &&
		    (o_ptr->inscrip != INSCRIP_TERRIBLE))
		{
			o_ptr->inscrip = INSCRIP_CURSED;
		}
	}

	/* Not cursed */
	else
	{
		/* Item has never been worn -- sense it carefully */
		if (!(o_ptr->ident & (IDENT_WORN)))
		{
			/* We force inscripions of "average", if appropriate */
			if (!ego_item_p(o_ptr)) sense_object(o_ptr, slot, TRUE, TRUE);
			else                    sense_object(o_ptr, slot, TRUE, FALSE);
		}

		/*
		 * We do not sense the original pile, because we have not yet
		 * figured out how to sense it properly, avoid abuse, and avoid
		 * really ugly, trouble-causing hacks, all at the same time.
		 */
	}

	/* Item has now been worn */
	o_ptr->ident |= (IDENT_WORN);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Redraw equippy chars */
	p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Take off an item
 */
void do_cmd_takeoff(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;


	if (p_ptr->schange)
	{
		msg_print("You cannot take off equipment while shapechanged.");
		msg_print("Use the ']' command to return to your normal form.");
		return;
	}

	/* Get an item */
	q = "Take off which item?";
	s = "You are not wearing anything to take off.";
	if (!get_item(&item, q, s, (USE_EQUIP))) return;
	item_to_object(o_ptr, item);

	/* Item is cursed */
	if (cursed_p(o_ptr))
	{
		/* Special case -- the One Ring */
		if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_POWER))
		{
			msg_print("The One Ring resists all attempts to remove it!");
		}

		/* Oops */
		else msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* You cannot remove pouches XXX */
	if (item == INVEN_POUCH)
	{
		msg_print("You cannot remove your pouch.");
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

	/* Get an item */
	q = "Drop which item?";
	s = "You have nothing to drop.";
	if (p_ptr->schange)
	{
		if (!get_item(&item, q, s, (USE_INVEN))) return;
	}
	else
	{
		if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;
	}
	item_to_object(o_ptr, item);


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

	/* You cannot remove pouches XXX */
	if (item == INVEN_POUCH)
	{
		msg_print("You cannot remove your pouch.");
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
void do_cmd_destroy(void)
{
	int item, amt;
	int old_number;

	object_type *o_ptr;

	char o_name[80];

	char out_val[160];

	cptr q, s;


	/* Get an item */
	q = "Destroy which item?";
	s = "You have nothing to destroy.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
	item_to_object(o_ptr, item);

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;


	/* Describe the object (using the requested quantity) */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify destruction */
	if (verify_destroy)
	{
		sprintf(out_val, "Really destroy %s?", o_name);
		if (!get_check(out_val)) return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Artifacts cannot be destroyed */
	if (artifact_p(o_ptr))
	{
		/* Message */
		msg_format("You cannot destroy %s.", o_name);

		/* Don't mark id'ed objects */
		if (object_known_p(o_ptr)) return;

		/* It has already been sensed */
		if ((o_ptr->ident & (IDENT_SENSE)) &&
		    (o_ptr->inscrip != INSCRIP_UNCERTAIN))
		{
			/* Already sensed objects always get improved feelings */
			if (cursed_p(o_ptr) || broken_p(o_ptr))
				o_ptr->inscrip = INSCRIP_TERRIBLE;
			else
				o_ptr->inscrip = INSCRIP_SPECIAL;
		}
		else
		{
			/* Mark the object as indestructible */
			o_ptr->inscrip = INSCRIP_INDESTRUCTIBLE;
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

	/* Get some experience  -TY- */
	if (((p_ptr->realm == PRIEST) && (o_ptr->tval == TV_DARK_BOOK)) ||
	    ((p_ptr->realm == NECRO) && (o_ptr->tval == TV_PRAYER_BOOK)))
	{
		/* Must not have been bought in a store  XXX */
		if (!(o_ptr->ident & (IDENT_FIXED)))
		{
			if (p_ptr->realm == PRIEST)
				msg_print("The Almighty smiles upon you.");
			else
				msg_print("You feel a wicked, gleeful satisfaction.");

			gain_exp(o_ptr->b_cost * amt / 25, S_NOSKILL);
		}
	}

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

	u16b aware, known, known_effects, mental;

	object_kind *k_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	char o_name[120];

	char info_text[2048];
	char object_kind_info[400];

	cptr q, s;

	int old_rows = screen_rows;


	/* Initialize object description. */
	strcpy(info_text, "");

	/* If not called in a store, we must get an object to inspect. */
	if (!o_ptr)
	{
		/* Get an item */
		q = "Examine which item?";
		s = "You have nothing to examine.";
		if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
			return;
		item_to_object(o_ptr, item);
	}


	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy the object */
	object_copy(i_ptr, o_ptr);

	/* Make singular */
	i_ptr->number = 1;


	/* Hack -- observe the contents of the pouch */
	if ((i_ptr->tval == TV_POUCH) && (item >= 0))
	{
		(void)get_essence(TRUE);
		return;
	}

	/* Describe the object */
	if (in_store) object_desc_store(o_name, i_ptr, FALSE, 2);
	else object_desc(o_name, i_ptr, FALSE, 2);

	/* What is our level of knowledge about the object? */
	aware = object_aware_p(i_ptr);
	known = (object_known_p(i_ptr) || in_store);
	known_effects = k_ptr->special & (SPECIAL_KNOWN_EFFECT);
	mental = ((i_ptr->ident & (IDENT_MENTAL)) || in_store);

	/*
	 * Hack - Avoid giving away too much info in normal stores about objects
	 * other than weapons and armour (no sneaky learning about wand damages!).
	 */
	if ((in_store) && (!k_ptr->special & (SPECIAL_KNOWN_EFFECT)) &&
		(!is_wargear(k_ptr)))
	{
		mental = FALSE;
	}

	/* Object is fully known - give maximal information. */
	if (mental)
	{
		/* Get the specific object type's information. */
		object_info(info_text, i_ptr);

		/* No object kind info. */
		strcpy(object_kind_info, "");
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

	/* Set to 25 screen rows */
	Term_rows(FALSE);


	/* Label the information. */
	roff(format("Item Information:  %s\n\n", o_name), 3, 0);

	/* Object type or artifact information. */
	c_roff(TERM_L_BLUE, info_text, 3, 77);


	/*  Describe artifacts that belong to sets. */
	if ((known) && (i_ptr->artifact_index))
	{
		artifact_type *a_ptr = &a_info[i_ptr->artifact_index];

		/* Is it a set item? */
		if (a_ptr->set_index)
		{
			/* Advance two lines */
			for (i = 0; i < 2; i++) roff("\n", 0, 0);

			/* Set notification */
			c_roff(TERM_GREEN,"Set Item: ", 3, 77);

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


	/* Clear some space */
	for (i = 0; i < 3; i++) roff("\n", 0, 0);

	/* Fully describe the object. */
	object_details(i_ptr, mental, known);

	/* Obtain the cursor location */
	(void)Term_locate(&x, &y);

	/* Object is not fully identified */
	if (!mental)
	{
		/* Spacing. */
		for (i = y; i < 18; i++) roff("\n", 0, 0);

		/* Object kind information. */
		roff(object_kind_info, 3, 77);
	}

	/* Hack -- attempt to stay on screen. */
	for (i = y; i < Term->hgt; i++)
	{
		/* No more space! */
		if (i > Term->hgt - 2) break;

		/* Advance one line. */
		roff("\n", 0, 0);

		/* Enough clear space.  Done. */
		if (i == (y + 2)) break;
	}


	/* The exit sign. */
	roff("(Press any key to continue.)", 25, 0);
	(void)inkey();


	/* Clear the screen */
	Term_clear();

	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
	}


	/* Load screen */
	screen_load();
}


/*
 * Remove the inscription from an object
 */
void do_cmd_uninscribe(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;


	/* Get an item */
	q = "Un-inscribe which item?";
	s = "You have nothing to un-inscribe.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return;
	item_to_object(o_ptr, item);

	/* No user note */
	if (!o_ptr->note)
	{
		/* Also remove those annoying "uncursed" inscriptions  -clefs- */
		if (o_ptr->inscrip == INSCRIP_UNCURSED)
		{
			o_ptr->inscrip = INSCRIP_NULL;
		}
		else
		{
			msg_print("That item had no inscription to remove.");
			return;
		}
	}

	/* Message */
	msg_print("Inscription removed.");

	/* Remove the inscription */
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


	/* Allow help on inscriptions */
	p_ptr->get_help_index = HELP_CMD_INSCRIP;

	/* Get an item */
	q = "Inscribe which item?";
	s = "You have nothing to inscribe.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return;
	item_to_object(o_ptr, item);

	/* No special help */
	p_ptr->get_help_index = HELP_GENERAL;


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
	if (get_string("Inscription:", tmp, 80))
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
 * Refill the player's lamp (from the pack or floor)
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
	q = "Refill with which source of oil?";
	s = "You have no sources of oil.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
	item_to_object(o_ptr, item);

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

	/* Item is not another lantern */
	if ((o_ptr->tval != TV_LITE) || (o_ptr->sval != SV_LITE_LANTERN))
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

	/* Empty the other lantern  (this can cause incorrect results) */
	else
	{
		o_ptr->pval = 0;
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
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
 * Refuel the player's torch (from the pack or floor)
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
	q = "Refuel with which torch?";
	s = "You have no extra torches.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
	item_to_object(o_ptr, item);

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Get the primary torch */
	j_ptr = &inventory[INVEN_LITE];

	/* Refuel */
	j_ptr->pval += o_ptr->pval;

	/* Message */
	msg_print("You combine the torches.");

	/* Fully fueled message */
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
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
}




/*
 * Refill the player's lamp, or restock his torches
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
 *
 * Note that we can look around even when blind or hallucinating.  XXX XXX
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
 *
 * This command also replicates the old "center on player" command of
 * Moria.
 *
 * The rules for shifting panels are somewhat relaxed when looking;
 * we can usually guarantee full vision in the direction of travel,
 * but may wind up showing slightly fewer grids to the rear.
 * See "verify_panel()".
 */
void do_cmd_locate(void)
{
	int dir, y1, x1, y2, x2;
	int wy, wx;

	char tmp_val[80];

	char out_val[160];

	/* Immediately shift panel to give a better view up and down */
	if ((p_ptr->move_dir >= 1) && (p_ptr->move_dir <= 9) &&
	    (p_ptr->move_dir != 5))
	{
		verify_panel(p_ptr->move_dir, TRUE);

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* Handle stuff */
		handle_stuff();
	}

	/* Start at current panel */
	y1 = p_ptr->wy;
	x1 = p_ptr->wx;

	/* Show panels until done */
	while (TRUE)
	{
		/* Get the current panel */
		y2 = p_ptr->wy;
		x2 = p_ptr->wx;

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

		/* Note current panel */
		wy = p_ptr->wy;
		wx = p_ptr->wx;

		/* Move to next panel in our chosen direction */
		change_panel(dir);

		/* Note change */
		if ((wy != p_ptr->wy) || (wx != p_ptr->wx))
		{
			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Handle stuff */
			handle_stuff();
		}

	}

	/* Recenter the map on the character */
	verify_panel(0, TRUE);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

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
	"!:A potion (or oil)",
	"\":An amulet (or necklace)",
	"#:A wall (or secret door)",
	"$:Treasure (gold or gems)",
	"%:A vein (magma or quartz)",
	"':An open door",
	"(:Soft armor",
	"):A shield",
	"*:Vein with treasure, essence, storm",
	"+:A closed door, or a large gemstone",
	",:Food (or mushroom patch)",
	"-:A wand (or rod)",
	".:Floor",
	"/:A polearm",
	/* "0:unused", */
	"1:Entrance to General Store",
	"2:Entrance to Armory",
	"3:Entrance to Weaponsmith",
	"4:Entrance to Temple",
	"5:Entrance to Alchemy shop",
	"6:Entrance to Magic store",
	"7:Entrance to Bookseller",
	"8:Entrance to your home",
	"::Rubble",
	";:A glyph of warding or loose rock",
	"<:An up staircase",
	"=:A ring",
	">:A down staircase",
	"?:A scroll",
	"@:You",
	"A:Angel",
	"B:Bird",
	"C:Canine",
	"D:Ancient Dragon/Wyrm",
	"E:Elemental",
	"F:Fly/Dragon Fly",
	"G:Ghost",
	"H:Hybrid",
	"I:Minor Demon",
	"J:Snake",
	"K:Killer Beetle",
	"L:Lich",
	"M:Mummy",
	/* "N:unused", */
	"O:Ogre",
	"P:Giant Humanoid",
	"Q:Quylthulg (Pulsing Flesh Mound)",
	"R:Reptile/Amphibian",
	"S:Spider/Scorpion/Tick",
	"T:Troll",
	"V:Vampire",
	"W:Wight/Wraith/etc",
	"X:Xorn/Xaren/etc",
	"Y:Yeti",
	"Z:Zephyr Hound",
	"[:Hard armor",
	"\\:A hafted (blunt) weapon",
	"]:Misc. armor",
	"^:A trap",
	"_:A staff",
	"`:A tool or junk",
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
	"v:Vortex",
	"w:Worm/Worm-Mass",
	/* "x:unused", */
	"y:Yeek",
	"z:Zombie/Mummy",
	"{:A missile",
	"|:An edged weapon",
	"}:A missile launcher",
	"~:A chest or light source",
	"&:Major Demon",
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
	u16b *who = (u16b *) (u);

	u16b *why = (u16b *) (v);

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
		z1 = l_list[w1].pkills;
		z2 = l_list[w2].pkills;

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
			c_roff(TERM_WHITE, "Press:\nControl-A for the full monster list\nControl-U for the unique monster list\nControl-N for the non-unique monster list\nControl-K for the killed monster list\nESCAPE to cancel\nRETURN to show this help window, or\nAny other key to display information about it", 15, 65);

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
		if ((!cheat_know || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
		    && (!l_ptr->sights))
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
		if (kill && !(l_ptr->pkills)) continue;

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
		roff_top(r_idx, 0);

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

				/* Recall on screen */
				screen_roff(who[i]);

				/* Hack -- Complete the prompt (again) */
				Term_addstr(-1, TERM_WHITE, "[un(r)ecall, ESC]");
				prt(format("(#%d of %d)", i + 1, n), 0 , 65);

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
	FREE(who);
}


#define OUTCRY_MAX   20

/* Hack -- possible victim outcry. -LM- */
static cptr desc_victim_outcry[OUTCRY_MAX] =
{
	"'My money, where's my money?'",
	"'Thief! Thief! Thief! Baggins! We hates it forever!'",
	"'Tell me, have you seen a purse wandering around?'",
	"'Thieves, Fire, Murder!'",
	"''Ere, 'oo are you?'",
	"'Hey, look what I've copped!'",
	"'How dare you!'",
	"'Help yourself again, thief, there is plenty and to spare!'",
	"'All the world detests a thief.'",
	"'Catch me this thief!'",
	"'Hi! Ho! Robbery!'",
	"'My gold, my precious gold!'",
	"'My gold is costly, thief!'",
	"'Your blood for my gold?  Agreed!'",
	"'I scrimp, I save, and now it's gone!'",
	"'Robbers like you are part of the problem!'",
	"'Banditti!  This dungeon's just not safe anymore!'",
	"'Ruined!  I'm ruined!'",
	"'Where, where is the common decency?'",
	"'Your knavish tricks end here and now!'"
};



/*
 * Rogues may steal gold from monsters.  The monster needs to have
 * something to steal (it must drop some form of loot), and should
 * preferably be asleep.  Humanoids and dragons are a rogue's favorite
 * targets.  Steal too often on a level, and monsters will be more wary,
 * and the hue and cry will be eventually be raised.  Having every
 * monster on the level awake and aggravated is not pleasant.  -LM-
 */
void py_steal(int y, int x)
{
	char act[80];

	monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char m_name[80];

	int i;
	int effect, theft_protection;
	int filching_power = 0;
	int purse = 0;

	bool thief = FALSE;
	bool success = FALSE;

	/* Hard limit on theft. */
	if (num_theft_on_level > 4)
	{
		msg_print("Everyone is keeping a lookout for you.  You can steal nothing here.");
		return;
	}

	/* Determine the cunning of the thief. */
	filching_power = get_skill(S_BURGLARY, 0, 100);


	/* Determine how much protection the monster has. */
	theft_protection = (7 * (r_ptr->level + 2) / 4);
	theft_protection += (m_ptr->mspeed - p_ptr->pspeed);
	if (theft_protection < 1) theft_protection = 1;

	/* Send a thief to catch a thief. */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
	{
		/* Extract information about the blow effect */
		effect = r_ptr->blow[i].effect;
		if (effect == RBE_EAT_GOLD) thief = TRUE;
		if (effect == RBE_EAT_ITEM) thief = TRUE;
	}
	if (thief) theft_protection += 30;

	if (m_ptr->csleep) theft_protection = 3 * theft_protection / 5;

	/* Special player stealth magics aid stealing, but are lost in the process. */
	if (p_ptr->tim_invis)
	{
		theft_protection = 3 * theft_protection / 5;
		set_invis(0, 0);
	}

	/* The more you steal on a level, the more wary the monsters. */
	theft_protection += num_theft_on_level * 15;

	/* Did the theft succeed?  */
	if (randint(theft_protection) < filching_power) success = TRUE;


	/* If the theft succeeded, determine the value of the purse. */
	if (success)
	{
		purse = rand_range(r_ptr->level * 3, r_ptr->level * 7);

		/* Uniques are juicy targets. */
		if (r_ptr->flags1 & (RF1_UNIQUE)) purse *= 2;

		/* But some monsters are dirt poor. */
		if (!((r_ptr->flags1 & (RF1_DROP_60)) ||
			(r_ptr->flags1 & (RF1_DROP_90)) ||
			(r_ptr->flags1 & (RF1_DROP_1D2)) ||
			(r_ptr->flags1 & (RF1_DROP_2D2)) ||
			(r_ptr->flags1 & (RF1_DROP_3D2)) ||
			(r_ptr->flags1 & (RF1_DROP_4D2)))) purse = 0;

		/* Some monster races are far better to steal from than others. */
		if ((r_ptr->d_char == 'D') || (r_ptr->d_char == 'd') ||
			(r_ptr->d_char == 'p') || (r_ptr->d_char == 'h'))
		{
			purse *= 4;
		}
		else if ((r_ptr->d_char == 'P') || (r_ptr->d_char == 'o') ||
			(r_ptr->d_char == 'O') || (r_ptr->d_char == 'T') ||
			(r_ptr->d_char == 'n') || (r_ptr->d_char == 'W') ||
			(r_ptr->d_char == 'k') || (r_ptr->d_char == 'L') ||
			(r_ptr->d_char == 'V') || (r_ptr->d_char == 'y'))
		{
			purse *= 2;
		}

		/* Pickings are scarce in a land of many thieves. */
		purse = purse * (p_ptr->depth + 5) / (p_ptr->max_depth + 5);

		/* Increase player gold. */
		p_ptr->au += purse;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Announce the good news. */
		if (purse)
		{
			msg_format("You burgle %d gold.", purse);

			/* Learn something */
			gain_exp(3, S_BURGLARY);
		}

		/* Pockets are empty. */
		else msg_print("You burgle only dust.");
	}

	/* The victim normally, but not always, wakes up and is aggravated. */
	if (!one_in_(4))
	{
		m_ptr->csleep = 0;
		m_ptr->mflag |= (MFLAG_ACTV);
		if (m_ptr->mspeed < r_ptr->speed + 3) m_ptr->mspeed += 10;


		/* Occasionally, amuse the player with a message. */
		if ((!one_in_(5)) && (purse) && (r_ptr->flags2 & (RF2_SMART)))
		{
			monster_desc(m_name, m_ptr, 0);
			strcpy(act, desc_victim_outcry[rand_int(OUTCRY_MAX)]);
			msg_format("%^s cries out %s", m_name, act);
		}
		/* Otherwise, simply explain what happened. */
		else
		{
			monster_desc(m_name, m_ptr, 0);
			msg_format("You have angered %s.", m_name);
		}
	}

	/* The thief also speeds up, but only for just long enough to escape. */
	if (!p_ptr->fast) set_fast(2);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();


	/* Increment the number of thefts, and possibly raise the hue and cry. */
	num_theft_on_level++;

	if (num_theft_on_level > 4)
	{
		/* Aggravate and speed up all monsters on level. */
		aggravate_monsters(-1, TRUE,
			"All the level is in an uproar over your misdeeds!");
	}

	else if ((num_theft_on_level > 2) || (one_in_(8)))
	{
		/* Aggravate monsters nearby. */
		aggravate_monsters(-1, FALSE,
			"You hear hunting parties scouring the area for a notorious burglar.");
	}

	/* Rogue "Hit and Run" attack. */
	if (p_ptr->special_attack & (ATTACK_FLEE))
	{
		/* Cancel the fleeing spell */
		p_ptr->special_attack &= ~(ATTACK_FLEE);

		/* Message */
		msg_print("You escape into the shadows!");

		/* Teleport */
		teleport_player(get_skill(S_BURGLARY, 6, 12), TRUE);
	}
}


/*
 * Rogues may set traps.  Only a few such traps may exist at any one time,
 * but an old trap can be disarmed to free up equipment for a new trap.
 * -LM-
 */
bool py_set_trap(int y, int x, int dir)
{
	/* Limit traps. */
	if (num_trap_on_level >= get_skill(S_BURGLARY, 1, 6))
	{
		msg_print("You must disarm an existing trap to free up your equipment.");
		return (FALSE);
	}

	/* Paranoia -- many types of terrain cannot hold traps */
	if (!cave_trap_allowed(y, x))
	{
		msg_print("You cannot place a trap here.");
	}

	/* Check some conditions */
	if (p_ptr->blind)
	{
		msg_print("You can't see anything.");
		return (FALSE);
	}
	if (no_light())
	{
		msg_print("You don't dare to set a trap in the darkness.");
		return (FALSE);
	}
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return (FALSE);
	}

	/* No setting traps while shapeshifted */
	if (p_ptr->schange)
	{
		msg_print("You cannot set traps while shapechanged.");
		msg_print("Use the ']' command to return to your normal form.");
		return (FALSE);
	}

	/* This is the first attempt to make a trap at this location */
	if ((p_ptr->trap_set.y != y) || (p_ptr->trap_set.x != x))
	{
		/* Reset trap making variables */
		p_ptr->trap_set.time = 0;
		p_ptr->trap_set.y = y;
		p_ptr->trap_set.x = x;
	}

	/* Invest some time */
	p_ptr->trap_set.time++;

	/* We've invested enough time */
	if (p_ptr->trap_set.time == 3)
	{
		/* Set the trap and draw it. */
		if (!place_trap(y, x, TRAP_MONSTER, 0))
		{
			/* Couldn't set a trap  XXX XXX */
			msg_print("You cannot set a trap here.");
			return (FALSE);
		}

		/* Notify the player. */
		msg_print("You set a monster trap.");

		/* All done */
		p_ptr->trap_set.time = 0;
		p_ptr->trap_set.y = 0;
		p_ptr->trap_set.x = 0;

		/* Cancel automation */
		p_ptr->command_rep = 0;
		p_ptr->redraw |= (PR_STATE);
	}

	/* Try to automate the process */
	if (p_ptr->trap_set.time)
	{
		/* Notify the player. */
		msg_print("Setting a monster trap...");

		p_ptr->command_cmd = '+';
		p_ptr->command_rep = 99;
		p_ptr->command_dir = dir;
	}

	return (TRUE);
}
