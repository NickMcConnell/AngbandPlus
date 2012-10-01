/* File: cmd-item.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * This file includes code for handling items. It includes all inventory 
 * handling commands, plus commands for eating food, drinking potions,
 * reading scrolls, aiming wands, using staffs, zapping rods,
 * and activating artifacts.
 *
 * In all cases, if the player becomes "aware" of the item's use
 * by testing it, mark it as "aware" and reward some experience
 * based on the object's level, always rounding up.  If the player
 * remains "unaware", mark that object "kind" as "tried".
 *
 * This code now correctly handles the unstacking of wands, staffs,
 * and rods.  Note the overly paranoid warning about potential pack
 * overflow, which allows the player to use and drop a stacked item.
 *
 * In all "unstacking" scenarios, the "used" object is "carried" as if
 * the player had just picked it up.  In particular, this means that if
 * the use of an item induces pack overflow, that item will be dropped.
 *
 * For simplicity, these routines induce a full "pack reorganization"
 * which not only combines similar items, but also reorganizes various
 * items to obey the current "sorting" method.  This may require about
 * 400 item comparisons, but only occasionally.
 *
 * There may be a BIG problem with any "effect" that can cause "changes"
 * to the inventory.  For example, a "scroll of recharging" can cause
 * a wand/staff to "disappear", moving the inventory up.  Luckily, the
 * scrolls all appear BEFORE the staffs/wands, so this is not a problem.
 * But, for example, a "staff of recharging" could cause MAJOR problems.
 * In such a case, it will be best to either (1) "postpone" the effect
 * until the end of the function, or (2) "change" the effect, say, into
 * giving a staff "negative" charges, or "turning a staff into a stick".
 * It seems as though a "rod of recharging" might in fact cause problems.
 * The basic problem is that the act of recharging (and destroying) an
 * item causes the inducer of that action to "move", causing "o_ptr" to
 * no longer point at the correct item, with horrifying results.
 *
 */

#include "angband.h"

/*
 * Display inventory
 */
void do_cmd_inven(void)
{
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

	/* Hack -- Use equipment screen */
	p_ptr->command_wrk = USE_INVEN;

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

	/* Hack -- Use equipment screen */
	p_ptr->command_wrk = USE_EQUIP;

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
	/* Hack - Deal with music items */
	if ((o_ptr->tval == TV_MUSIC) && !(cp_ptr->flags & CF_MUSIC)) return (FALSE);

	/* return true if wearable */
	return (wearable_p(o_ptr));
}

/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
	int item, slot;

	object_type *o_ptr;

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
	if (item >= 0) o_ptr = &inventory[item];
	
	/* Get the item (on the floor) */
	else o_ptr = &o_list[0 - item];

	/* Assign slot */
	slot = wield_slot(o_ptr);

	/* Check the slot */
	if (!slot) return;

	/* Prevent wielding into a cursed slot */
	if (cursed_p(&inventory[slot]))
	{
		/* Describe it */
		object_desc(o_name, sizeof(o_name), &inventory[slot], FALSE, 0);

		/* Message */
		message_format(MSG_FAIL, 0, "The %s you are %s appears to be cursed.",
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

	/* Increase the weight */
	p_ptr->total_weight += object_weight(i_ptr);

	/* Increment the equip counter by hand */
	p_ptr->equip_cnt++;

	/* Where is the item now */
	if (slot == INVEN_WIELD) act = "You are wielding";
	else if (slot == INVEN_BOW) act = "You are shooting with";
	else if (slot == INVEN_LITE) act = "Your light source is";
	else act = "You are wearing";

	/* Describe the result */
	object_desc(o_name,  sizeof(o_name),o_ptr, TRUE, 3);

	/* Message */
	message_format(MSG_DESCRIBE, 0, "%s %s (%c).", act, o_name, index_to_label(slot));

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		message(MSG_CURSE, 0, "Oops! It feels deathly cold!");

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

		/* Sense the object if allowed */
		if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_CURSED;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

		/* Squelch */
		if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Redraw "equippy" */
	p_ptr->redraw |= (PR_EQUIPPY);

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
	if (item >= 0) o_ptr = &inventory[item];

	/* Get the item (on the floor) */
	else o_ptr = &o_list[0 - item];

	/* Item is cursed */
	if (cursed_p(o_ptr))
	{
		/* Oops */
		message(MSG_FAIL, 0, "Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Take off the item */
	(void)inven_takeoff(item, 255);

	/* Redraw "equippy" */
	p_ptr->redraw |= (PR_EQUIPPY);
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

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
		message(MSG_FAIL, 0, "Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Drop (some of) the item */
	inven_drop(item, amt);

	/* Redraw "equippy" */
	p_ptr->redraw |= (PR_EQUIPPY);
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
	q = "Destroy which item? ";
	s = "You have nothing to destroy.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | CAN_SQUELCH))) return;

	/* Try to destroy all items marked SQUELCH */
	if (item == ALL_SQUELCHED)
	{
		destroy_squelched_items();

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

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify destruction */
	if ((verify_destroy) && !((o_ptr->note) && (streq(quark_str(o_ptr->note), "squelch"))))
	{
		strnfmt(out_val, sizeof(out_val), "Really destroy %s? ", o_name);
		if (!get_check(out_val)) return;
	}
	/* Artifacts cannot be destroyed */
	if (!destroy_check(o_ptr))
	{
		/* Message */
		message_format(MSG_FAIL, 0, "You cannot destroy %s.", o_name);

		/* Done */
		return;
	}

	/* Message */
	message_format(MSG_SUCCEED, 0, "You destroy %s.", o_name);

	/* Reduce the charges of rods */
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
 * Observe an item which has been *identify*-ed
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

	/* Track the object kind */
	object_actual_track(o_ptr);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Actually display the object */
	screen_object(o_ptr, TRUE);
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
		message(MSG_FAIL, 0, "That item had no inscription to remove.");
		return;
	}

	/* Message */
	message(MSG_SUCCEED, 0, "Inscription removed.");

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
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Message */
	message_format(MSG_GENERIC, 0, "Inscribing %s.", o_name);;
	message_flush();

	/* Start with nothing */
	strcpy(tmp, "");

	/* Use old inscription */
	if (o_ptr->note)
	{
		/* Start with the old inscription */
		strnfmt(tmp, sizeof(tmp), "%s", quark_str(o_ptr->note));
	}

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", tmp, sizeof(tmp)))
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
	if (o_ptr->tval == TV_FLASK) return TRUE;

	/* Non-empty lanterns are okay */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval >= SV_LANTERN) && (o_ptr->timeout > 0))
		return TRUE;

	/* Assume not okay */
	return FALSE;
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

	/* Refuel - use pval for flask, timeout for other light sources*/
	if (o_ptr->tval == TV_FLASK) j_ptr->timeout += o_ptr->pval;
	else j_ptr->timeout += o_ptr->timeout;

	/* Message */
	message(MSG_FUEL, j_ptr->k_idx, "You fuel your lamp.");

	/* Comment */
	if (j_ptr->timeout >= FUEL_LAMP)
	{
		j_ptr->timeout = FUEL_LAMP;
		message(MSG_FUEL, -1, "Your lamp is full.");
	}

	/* Use fuel from a lantern */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval >= SV_LANTERN))
	{
		/* XXX Hack -- unstack if necessary */
		if ((item >= 0) && (o_ptr->number > 1))
		{
			object_type *i_ptr;
			object_type object_type_body;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Obtain a local object */
			object_copy(i_ptr, o_ptr);

			/* Modify quantity */
			i_ptr->number = 1;

			/* No more fuel */
			i_ptr->timeout = 0;

			/* Unstack the used item */
			o_ptr->number--;
			p_ptr->total_weight -= object_weight(i_ptr);
			item = inven_carry(i_ptr);

			/* Message */
			message(MSG_GENERIC, 0, "You unstack your lantern.");
		}

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

	/* Reclaculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP);
}

/*
 * An "item_tester_hook" for refilling torches
 */
static bool item_tester_refill_torch(const object_type *o_ptr)
{
	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_TORCH)) return TRUE;

	/* Assume not okay */
	return FALSE;
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
	j_ptr->timeout += o_ptr->timeout + 5;

	/* Message */
	message(MSG_FUEL, j_ptr->k_idx, "You combine the torches.");

	/* Over-fuel message */
	if (j_ptr->timeout >= FUEL_TORCH)
	{
		j_ptr->timeout = FUEL_TORCH;
		message(MSG_FUEL, -1, "Your torch is fully fueled.");
	}

	/* Refuel message */
	else
	{
		message(MSG_FUEL, -1, "Your torch glows more brightly.");
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
		message(MSG_FAIL, 0, "You are not wielding a refuelable light.");
	}

	/* It's a lamp */
	else if (o_ptr->sval >= SV_LANTERN)
	{
		do_cmd_refill_lamp();
	}

	/* It's a torch */
	else 
	{
		do_cmd_refill_torch();
	}

}

/*
 * Eat some food (from the pack or floor)
 */
static void do_cmd_eat_food_aux(int item)
{
	bool ident = FALSE;

	object_type *o_ptr;

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

	/* Sound */
	sound(MSG_EAT);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Check for activation */
	if (object_activation(o_ptr)) 
	{
		/* Actually use the power */
		if (!do_power(k_info[o_ptr->k_idx].activation, 0, 0, 0, 0, 0, 0, FALSE, &ident)) 
			return;
	}
	else
	{
		/* XXX XXX XXX Non-power foods */
		switch (o_ptr->sval)
		{
			case SV_FOOD_RATION:
			case SV_FOOD_JERKY:
			case SV_FOOD_APPLE:
			case SV_FOOD_SLIME_MOLD:
			{
				message(MSG_EFFECT, 0, "That tastes good.");
				ident = TRUE;
				break;
			}

			case SV_FOOD_WAYBREAD:
			{
				message(MSG_EFFECT, 0, "That tastes good.");
				(void)set_poisoned(0);
				(void)hp_player(damroll(4, 8));
				ident = TRUE;
				break;
			}

			case SV_FOOD_AMBROSIA:
			{
				message(MSG_EFFECT, 0, "That tastes good.");
				(void)set_poisoned(0);
				(void)set_diseased(p_ptr->diseased/2);
				(void)set_afraid(0);
				(void)hp_player(damroll(2, 4));
				ident = TRUE;
				break;
			}

			case SV_FOOD_MYSTERY_MEAT:
			{
				message(MSG_EFFECT, 0, "You immediately regret eating that.");
				(void)set_poisoned(100);
				(void)set_diseased(100);
				(void)set_blind(100);
				ident = TRUE;
				break;
			}
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* We have tried it */
	object_tried(o_ptr);

	/* The player is now aware of the object */
	if (ident && !object_aware_p(o_ptr))
	{
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

		object_aware(o_ptr);

		/* Squelch */
		if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Food can feed the player */
	(void)set_food(p_ptr->food + o_ptr->pval);

	/* Destroy a food in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a food on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}

/* Eat some food */
void do_cmd_eat_food(void)
{
	int	item;
	cptr	q, s;

	/* Restrict choices to food */
	item_tester_tval = TV_FOOD;

	/* Get an item */
	q = "Eat which item? ";
	s = "You have nothing to eat.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Eat the object */
	do_cmd_eat_food_aux(item);
}

/*
 * Quaff a potion (from the pack or the floor)
 */
static void do_cmd_quaff_potion_aux(int item)
{
	bool ident = FALSE;

	object_type *o_ptr;

	char line[80];

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

	/* Sound */
	sound(MSG_QUAFF);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Potions feed the player */
	(void)set_food(p_ptr->food + 75);

	/* Analyze the potion */
	switch (o_ptr->sval)
	{
		case SV_POTION_WATER:
		case SV_POTION_APPLE_JUICE:
		case SV_POTION_SLIME_MOLD:
		{
			message(MSG_EFFECT, 0, "You feel less thirsty.");
			ident = TRUE;
			break;
		}

		/* Mega-hack - potions of risk are hardcoded. */
		case SV_POTION_RISK:
		{
			if ((object_aware_p(o_ptr)) && (rand_int(100) < 50))
			{
				message(MSG_EFFECT, 0, "You took one risk too many.");
				damage_player(5000, "a potion of risk");
			}
			else
			{
				message(MSG_EFFECT, 0, "Great risks bring great rewards!");
				restore_exp();
				(void)set_poisoned(0);
				(void)set_blind(0);
				(void)set_confused(0);
				(void)set_diseased(0);
				(void)set_image(0);
				(void)set_stun(0);
				(void)set_cut(0);
				(void)do_inc_stat(A_STR);
				(void)do_inc_stat(A_INT);
				(void)do_inc_stat(A_WIS);
				(void)do_inc_stat(A_DEX);
				(void)do_inc_stat(A_CON);
				(void)do_inc_stat(A_CHR);
				(void)hp_player(5000);
				wiz_lite();
			}

			ident = TRUE;
			break;
		}

		case SV_POTION_RESISTANCE:
		case SV_POTION_STAR_RESISTANCE:
		{
			/* XXX XXX These potions are activated at a higher llev */
			if (!do_power(k_info[o_ptr->k_idx].activation, 0, 0, 0, 0, 40, 0, FALSE, &ident)) 
				return;
			break;
		}
		default:
		{
			/* Other potions */
			if (!do_power(k_info[o_ptr->k_idx].activation, 0, 0, 0, 0, 15, 0, FALSE, &ident)) 
				return;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item has been tried */
	object_tried(o_ptr);

	/* An identification was made */
	if ((ident) && (!object_aware_p(o_ptr)))
	{
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

		object_aware(o_ptr);

		/* Squelch */
		if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
	}
		
	/* Check if the alchemicl formula is learnt */
	if (!((potion_alch[o_ptr->sval].known1) && (potion_alch[o_ptr->sval].known2)) &&
		(rand_int(100) <= p_ptr->skill[SK_ALC] - 30))
	{
		object_kind *k_ptr;

		int k;
		bool item_known1 = FALSE;
		bool item_known2 = FALSE;

		bool learn = FALSE;

		/* Check if the components are known */
		for (k = 1; k < z_info->k_max; k++)
		{
			k_ptr = &k_info[k];

			/* Found a match */
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == potion_alch[o_ptr->sval].sval1)) 
				item_known1 = (k_ptr->tried || k_ptr->aware);
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == potion_alch[o_ptr->sval].sval2)) 
				item_known2 = (k_ptr->tried || k_ptr->aware);
		}

		/* 
		 * Learn, if you are aware of the component potion, but
		 * you are not yet aware it is part of the potion.
		 */
		if ((!(potion_alch[o_ptr->sval].known1)) && item_known1) 
		{
			potion_alch[o_ptr->sval].known1 = TRUE;
			learn = TRUE;
		}
		else if ((!(potion_alch[o_ptr->sval].known2)) && item_known2) 
		{
			potion_alch[o_ptr->sval].known2 = TRUE;
			learn = TRUE;
		}

		/* Message, if something new was learned */
		if (learn)
		{
			message(MSG_STUDY, 0, "You have gained alchemical knowledge!");
			alchemy_describe(line, sizeof(line), o_ptr->sval);
			message_format(MSG_STUDY, -1, "%s", line);
		}
	}
		
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Destroy a potion in the pack */
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
}

/* Drink a potion */
void do_cmd_quaff_potion(void)
{
	int  item;
	cptr q, s;

	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get an item */
	q = "Quaff which potion? ";
	s = "You have no potions to quaff.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Quaff the potion */
	do_cmd_quaff_potion_aux(item);
}

/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
static void do_cmd_read_scroll_aux(int item)
{
	bool ident, used_up;

	object_type *o_ptr;

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

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Assume the scroll will get used up */
	used_up = do_power(k_info[o_ptr->k_idx].activation, 0, 0, 0, 20, 30, 0, FALSE, &ident);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item was tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

		object_aware(o_ptr);

		/* Squelch */
		if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- allow certain scrolls to be "preserved" */
	if (!used_up) return;

	/* Destroy a scroll in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a scroll on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}

void do_cmd_read_scroll(void)
{
	int  item;
	cptr q, s;

	/* Check some conditions */
	if (p_ptr->blind)
	{
		message(MSG_FAIL, 0, "You can't see anything.");
		return;
	}
	if (!player_can_see_bold(p_ptr->py, p_ptr->px))
	{
		message(MSG_FAIL, 0, "You have no light to read by.");
		return;
	}
	if (p_ptr->confused)
	{
		message(MSG_FAIL, 0, "You are too confused!");
		return;
	}

	/* Restrict choices to scrolls */
	item_tester_tval = TV_SCROLL;

	/* Get an item */
	q = "Read which scroll? ";
	s = "You have no scrolls to read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Read the scroll */
	do_cmd_read_scroll_aux(item);
}

#define USAGE_WAND		4		/* Difficulty for aiming wands */
#define USAGE_STAFF		3		/* Difficulty for using staves */
#define USAGE_TALISMAN	5		/* Difficulty for zapping rods */
#define USAGE_ROD		3		/* Difficulty for invoking talismans */
#define USAGE_ARTIFACT	3		/* Difficulty for activating artifacts */

/* 
 * Check for item success
 * 
 * There are two kinds of check - binary, and non-binary. Binary checks return 
 * 100 if successful and 0 if not. Non-binary checks return a percentage between 100
 * and 0 reflecting success level.
 */
static int check_item_success(int diff, int lev, bool binary)
{
	int chance;
	
	int result, i;

	/* Base chance of success */
	chance = p_ptr->skill[SK_DEV];

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Stunning hurts skill */
	if (p_ptr->stun > PY_STUN_HEAVY) chance = chance / 2;
	else if (p_ptr->stun) chance = (chance * 2) / 3;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < diff) && (rand_int(diff - chance + 1) == 0))
	{
		chance = diff;
	}
	else if (chance < 0) return 0;

	/* Check for success */
	if (chance > 0) i = (randint(chance) - diff + 1);
	else i = (1 - diff);

	/* Check exact failure percentage */
	if (i < 0)
	{
		if (!binary)
		{
			int j = 0 - (10 * diff * i);

			result = ((0 - randint(j)) * 5) + 100;

			if (result < 0) result = 0;
		}
		/* If a binary check, ignore results below 0 */
		else result = 0;
	}
	else result = 100;

	return (result);
}

/*
 * Use a staff
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
static void do_cmd_use_staff_aux(int item)
{
	bool ident = FALSE;

	object_type *o_ptr;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge;

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

	/* Mega-Hack -- refuse to use a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		message(MSG_FAIL, 0, "You must first pick up the staffs.");
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Roll for usage */
	if (!check_item_success(USAGE_STAFF, k_info[o_ptr->k_idx].level, TRUE))
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "You failed to use the staff properly.");
		return;
	}

	/* Notice empty staffs */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "The staff has no charges left.");
		o_ptr->ident |= (IDENT_EMPTY);
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		return;
	}

	/* Sound */
	sound(MSG_ZAP);

	/* Actually use the power */
	use_charge = do_power(k_info[o_ptr->k_idx].activation, 0, 0, 0, 20, 20, 15 + rand_int(10), FALSE, &ident);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

		object_aware(o_ptr);

		/* Squelch */
		if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- some uses are "free" */
	if (!use_charge) return;

	/* Use a single charge */
	o_ptr->pval--;

	/* XXX Hack -- unstack if necessary */
	if ((item >= 0) && (o_ptr->number > 1))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Restore the charges */
		o_ptr->pval++;

		/* Unstack the used item */
		o_ptr->number--;
		p_ptr->total_weight -= object_weight(i_ptr);
		item = inven_carry(i_ptr);

		/* Message */
		message(MSG_GENERIC, 0, "You unstack your staff.");
	}

	/* Describe charges in the pack */
	if (item >= 0)
	{
		inven_item_charges(item);
	}

	/* Describe charges on the floor */
	else
	{
		floor_item_charges(0 - item);
	}
}

/* 
 * Use a staff
 */
void do_cmd_use_staff(void)
{
	int  item;
	cptr q, s;

	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item */
	q = "Use which staff? ";
	s = "You have no staff to use.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	do_cmd_use_staff_aux(item);
}

/*
 * Aim a wand (from the pack or floor).
 *
 * Use a single charge from a single item.
 * Handle "unstacking" in a logical manner.
 *
 * For simplicity, you cannot use a stack of items from the
 * ground.  This would require too much nasty code.
 *
 * There are no wands which can "destroy" themselves, in the inventory
 * or on the ground, so we can ignore this possibility.  Note that this
 * required giving "wand of wonder" the ability to ignore destruction
 * by electric balls.
 *
 * All wands can be "cancelled" at the "Direction?" prompt for free.
 *
 * Note that the basic "bolt" wands do slightly less damage than the
 * basic "bolt" rods, but the basic "ball" wands do the same damage
 * as the basic "ball" rods.
 */
static void do_cmd_aim_wand_aux(int item)
{
	bool ident = FALSE;
	int dir, power, check;
	int plev = p_ptr->lev;

	object_type *o_ptr;

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

	/* Mega-Hack -- refuse to aim a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		message(MSG_FAIL, 0, "You must first pick up the wands.");
		return;
	}

	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	check = check_item_success(USAGE_WAND, k_info[o_ptr->k_idx].level, FALSE);

	/* Roll for usage */
	if (!check)
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "You failed to use the wand properly.");
		return;
	}

	/* The wand is already empty! */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "The wand has no charges left.");
		o_ptr->ident |= (IDENT_EMPTY);
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	
		return;
	}

	/* Sound */
	sound(MSG_ZAP);

	/* XXX Hack - get power */
	power = k_info[o_ptr->k_idx].activation;

	/* XXX Hack -- Wand of wonder can do anything before it */
	if (o_ptr->sval == SV_WAND_WONDER)
	{
		int k = lookup_kind(TV_WAND, rand_int(SV_WAND_WONDER));

		power = k_info[k].activation;
	}

	/* Partial success */
	plev = ((plev - 1) * check) / 100 + 1;
	
	/* Actually use the power */
	if (!do_power(power, 0, dir, 20, plev, plev, 10 + plev/2, FALSE, &ident)) 
		return;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Mark it as tried */
	object_tried(o_ptr);

	/* Apply identification */
	if (ident && !object_aware_p(o_ptr))
	{
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

		object_aware(o_ptr);

		/* Squelch */
		if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Use a single charge */
	o_ptr->pval--;

	/* Hack -- unstack if necessary */
	if ((item >= 0) && (o_ptr->number > 1))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Restore the charges */
		o_ptr->pval++;

		/* Unstack the used item */
		o_ptr->number--;
		p_ptr->total_weight -= object_weight(i_ptr);
		item = inven_carry(i_ptr);

		/* Message */
		message(MSG_GENERIC, 0, "You unstack your wand.");
	}

	/* Describe the charges in the pack */
	if (item >= 0)
	{
		inven_item_charges(item);
	}

	/* Describe the charges on the floor */
	else
	{
		floor_item_charges(0 - item);
	}
}

/* Aim a wand */
void do_cmd_aim_wand(void)
{
	int item;
	cptr q, s;

	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Get an item */
	q = "Aim which wand? ";
	s = "You have no wand to aim.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Aim the wand */
	do_cmd_aim_wand_aux(item);
}

/*
 * Hook to determine if rechargeable object (rod, talisman) is ready for use
 */
bool item_tester_hook_recharged(const object_type *o_ptr)
{
	/* Talisman */
	if (o_ptr->tval == TV_TALISMAN || o_ptr->tval == TV_ROD)
	{
		/* Check if there is at least one fully recharged rod/talismans */
		if (o_ptr->timeout <= ((o_ptr->number - 1) * o_ptr->pval)) return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}

/*
 * Activate (zap) a Rod.    Rods may be fully identified through use.  
 * Rods now use timeouts to determine charging status, and pvals have 
 * become the cost of zapping a rod (how long it takes between zaps).  
 * Pvals are defined for each rod in k_info. -LM-
 */
static void do_cmd_zap_rod_aux(int item)
{
	bool ident;
	int chance;

	object_type *o_ptr;

	/* Hack -- let perception get aborted */
	bool use_charge;

	/* Get the item (in the pack) */
	if (item >= 0) o_ptr = &inventory[item];

	/* Get the item (on the floor) */
	else o_ptr = &o_list[0 - item];

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Base chance of success */
	chance = p_ptr->skill[SK_DEV];

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Stunning hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Roll for usage */
	if (!check_item_success(USAGE_ROD, k_info[o_ptr->k_idx].level, TRUE))
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "You failed to use the rod properly.");
		return;
	}

	/* Increase timeout */
	o_ptr->timeout += o_ptr->pval;

	/* Sound */
	sound(MSG_ZAP);

	/* Actually use the power */
	use_charge = do_power(k_info[o_ptr->k_idx].activation, 0, 0, 0, 10, 10, 10, FALSE, &ident);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

		object_aware(o_ptr);

		/* Squelch */
		if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- deal with cancelled zap */
	if (!use_charge)
	{
		o_ptr->timeout -= o_ptr->pval;
		return;
	}
}

/* Zap a rod */
void do_cmd_zap_rod(void)
{
	int item;
	cptr q, s;

	/* Restrict choices to rods */
	item_tester_tval = TV_ROD;

	/* Don't offer charging rods */
	item_tester_hook = item_tester_hook_recharged;

	/* Get an item */
	q = "Zap which rod? ";
	s = "You have no charged rod to zap.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Zap the rod */
	do_cmd_zap_rod_aux(item);
}

/*
 * Activate (invoke) a Talisman. They act identically to rods but are
 * directional.
 */
static void do_cmd_invoke_talisman_aux(int item)
{
	bool ident;
	int dir, check;
	int plev = p_ptr->lev;

	object_type *o_ptr;

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

	/* Get a direction, allow cancel */
	if (!get_aim_dir(&dir)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	check = check_item_success(USAGE_TALISMAN, k_info[o_ptr->k_idx].level, FALSE);

	/* Roll for usage */
	if (!check)
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "You failed to use the talisman properly.");
		return;
	}

	/* Increase timeout by full amount (from k_info) */
	o_ptr->timeout += o_ptr->pval;

	/* Partial success */
	plev = ((plev - 1) * check) / 100 + 1;

	/* Sound */
	sound(MSG_ZAP);

	/* Actually use the power */
	if (!do_power(k_info[o_ptr->k_idx].activation, 0, dir, 10, plev, plev, plev, FALSE, &ident)) 
		return;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

		object_aware(o_ptr);

		/* Squelch */
		if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
}

/* Invoke a Talisman */
void do_cmd_invoke_talisman(void)
{
	int item;
	cptr q, s;

	/* Restrict choices to talismans */
 	item_tester_tval = TV_TALISMAN;

	/* Don't offer charging talismans */
	item_tester_hook = item_tester_hook_recharged;

	/* Get an item */
	q = "Invoke which talisman? ";
	s = "You have no charged talisman to invoke.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Invoke the talisman */
	do_cmd_invoke_talisman_aux(item);
}

/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(const object_type *o_ptr)
{
	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* DSM */
	if (k_info[o_ptr->k_idx].tval == TV_DRAG_ARMOR) return (TRUE);

	/* Check activation flag */
	if (a_info[o_ptr->a_idx].activation) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
static void do_cmd_activate_aux(int item)
{
	int lev;

	object_type *o_ptr;

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

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use artifact level instead */
	if (o_ptr->a_idx) lev = a_info[o_ptr->a_idx].level;

	/* Roll for usage */
	if (!check_item_success(USAGE_ARTIFACT, k_info[o_ptr->k_idx].level, TRUE))
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "You failed to activate it properly.");
		return;
	}

	/* Check the recharge */
	if (o_ptr->timeout)
	{
		message(MSG_FAIL, 0, "It whines, glows and fades...");
		return;
	}

	/* Activate the artifact */
	message(MSG_ZAP, TRUE, "You activate it...");

	/* Artifacts */
	if (o_ptr->a_idx)
	{
		artifact_type *a_ptr = &a_info[o_ptr->a_idx];
		char o_name[80];
		bool ident = FALSE;

		/* Get the basic name of the object */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Give the appropriate message */
		message_format(MSG_EFFECT, a_ptr->activation, "Your %s glows...", o_name);

		/* Actually use the power */
		if (!do_power(a_ptr->activation, 0, 0, 0, p_ptr->lev, p_ptr->lev, p_ptr->lev, FALSE, &ident)) 
			return;

		/* Know the activation */
		if (ident) a_ptr->status |= A_STATUS_ACTIVATE;

		/* Set the recharge time */
		if (a_ptr->randtime) o_ptr->timeout = a_ptr->time + (byte)randint(a_ptr->randtime);
		else o_ptr->timeout = a_ptr->time;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}

	/* Hack -- Dragon Scale Mail can be activated as well */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		int j;
		bool ignore_me;

		/* Actually use the power */
		if (!do_power(object_activation(o_ptr), 0, 0, 0,
			p_ptr->lev, p_ptr->lev, p_ptr->lev, FALSE, &ignore_me)) return;
		
		/* XXX XXX XXX DSM recharge times */
		switch (o_ptr->sval)
		{
			case (SV_DRAGON_BLACK):		j = 131; break;
			case (SV_DRAGON_BLUE):		j = 131; break;
			case (SV_DRAGON_WHITE):		j = 131; break;
			case (SV_DRAGON_RED):		j = 131; break;
			case (SV_DRAGON_GREEN):		j = 137; break;
			case (SV_DRAGON_GOLD):		j = 125; break;
			case (SV_DRAGON_SILVER):	j = 125; break;
			case (SV_DRAGON_MULTIHUED): j = 162; break;
			case (SV_DRAGON_ETHEREAL):	j = 162; break;
			case (SV_DRAGON_SPIRIT):	j = 162; break;
			case (SV_DRAGON_SHADOW):	j = 162; break;
			case (SV_DRAGON_CHAOS):		j = 187; break;
			case (SV_DRAGON_TIME):		j = 187; break;
			case (SV_DRAGON_POWER):		j = 200; break;
		}

		/* Recharging */
		o_ptr->timeout = rand_int(j) + j;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	/* Mistake */
	message(MSG_FAIL, 0, "Oops.  That object cannot be activated.");
}

/* Activate something */
void do_cmd_activate(void)
{
	int item;
	cptr q, s;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
	q = "Activate which item? ";
	s = "You have nothing to activate.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_FLOOR))) return;

	/* Activate the item */
	do_cmd_activate_aux(item);
}

/*
 * Hook to determine if an object is useable
 */
static bool item_tester_hook_use(const object_type *o_ptr)
{
	/* Useable object */
	switch (o_ptr->tval)
	{
		case TV_STAFF:
		case TV_WAND:
		case TV_ROD:
		case TV_TALISMAN:
		case TV_SCROLL:
		case TV_POTION:
		case TV_FOOD:
		{
			return (TRUE);
		}

		default:
		{
			int i;

			/* Not known */
			if (!object_known_p(o_ptr)) return (FALSE);

			/* HACK - only items from the equipment can be activated */
			for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
			{
				if (&inventory[i] == o_ptr)
				{
					if (o_ptr->tval == TV_DRAG_ARMOR) return (TRUE);

					/* Check activation flag */
					if (a_info[o_ptr->a_idx].activation) return (TRUE);
				}
			}
		}
	}

	/* Assume not */
	return (FALSE);
}

/*
 * Unified use item - taken from Zangband.
 */
void do_cmd_use(void)
{
	int item;
	object_type *o_ptr;
	cptr q, s;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_use;

	/* Get an item */
	q = "Use which item? ";
	s = "You have nothing to use.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	switch (o_ptr->tval)
	{
		/* Eat some food */
		case TV_FOOD:
		{
			do_cmd_eat_food_aux(item);
			break;
		}

		/* Aim a wand */
		case TV_WAND:
		{
			do_cmd_aim_wand_aux(item);
			break;
		}

		/* Use a staff */
		case TV_STAFF:
		{
			do_cmd_use_staff_aux(item);
			break;
		}

		/* Zap a rod */
		case TV_ROD:
		{
			do_cmd_zap_rod_aux(item);
			break;
		}

		/* Invoke a talisman */
		case TV_TALISMAN:
		{
			do_cmd_invoke_talisman_aux(item);
			break;
		}

		/* Quaff a potion */
		case TV_POTION:
		{
			do_cmd_quaff_potion_aux(item);
			break;
		}

		/* Read a scroll */
		case TV_SCROLL:
		{
			/* Check some conditions */
			if (p_ptr->blind)
			{
				message(MSG_FAIL, 0, "You can't see anything.");
				return;
			}
			if (!player_can_see_bold(p_ptr->py, p_ptr->px))
			{
				message(MSG_FAIL, 0, "You have no light to read by.");
				return;
			}
			if (p_ptr->confused)
			{
				message(MSG_FAIL, 0, "You are too confused!");
				return;
			}

		  do_cmd_read_scroll_aux(item);
		  break;
		}

		/* Activate an artifact */
		default:
		{
			do_cmd_activate_aux(item);
			break;
		}
	}
}

/* 
 * Mix two potions to (maybe) create a third 
 */
void do_cmd_mix(void)
{
	int item1, item2, k_idx;
	int i1, i2, sv;
	int chance = 0;
	int penalty = 0;
	int roll;

	bool found = FALSE;

	cptr q, s, r;
	object_kind *k_ptr;
	object_type object_type_body;
	object_type *o_ptr1, *o_ptr2, *to_ptr;

	/* Get an item */
	q = "Mix which potion? ";
	s = "You don't have enough potions to mix.";
	r = "With which potion? ";
	item_tester_tval = TV_POTION;
	if (!get_item(&item1, q, s, (USE_INVEN | USE_FLOOR))) return;
	item_tester_tval = TV_POTION;
	if (!get_item(&item2, r, s, (USE_INVEN | USE_FLOOR))) return;

	if (item1 >= 0)
	{
		o_ptr1 = &inventory[item1];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr1 = &o_list[0 - item1];
	}

	if (item2 >= 0)
	{
		o_ptr2= &inventory[item2];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr2 = &o_list[0 - item2];
	}

	if (o_ptr1->sval==o_ptr2->sval)
	{
		message(MSG_FAIL, 0, "You must mix two different potions!");
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;
	
	/* Extract the tval/sval codes */
	for (sv = 0; sv < SV_POTION_MAX; sv++)
	{
		if (((potion_alch[sv].sval1 == o_ptr1->sval) && (potion_alch[sv].sval2 == o_ptr2->sval))
			|| ((potion_alch[sv].sval2 == o_ptr1->sval) && (potion_alch[sv].sval1 == o_ptr2->sval)))
			found = TRUE;
	
		if (found) break;
	}

	/* Found a potion? */
	if (found)
	{
		/* Look for it */
		for (k_idx = 1; k_idx < z_info->k_max; k_idx++)
		{
			k_ptr = &k_info[k_idx];

			/* Found a match */
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == sv)) break;
		}

		/* If there's no potion, always fail */
		if (k_idx != z_info->k_max) 
		{
			chance = 25 + (p_ptr->skill[SK_ALC]) - ((k_ptr->pval * k_ptr->pval * 3)/2);
			penalty = ((k_ptr->pval * k_ptr->pval) / 2);

			/* Always 5% chance of success or failure*/
			if (chance < 5) chance = 5;
			if (chance > 95) chance = 95;
		}
	}

	/*** Skill check ***/
	roll = rand_int(100);	

	if (roll < chance)
	{
		/*** Create new potion ****/

		/* Get local object */
		to_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(to_ptr);
	
		/* Prepare the ojbect */
		object_prep(to_ptr, k_idx);

		drop_near(to_ptr, -1, p_ptr->py, p_ptr->px); /* drop the object */
		potion_alch[sv].known1 = TRUE;
		potion_alch[sv].known2 = TRUE;
	}
	else if ((roll < chance + 30) && (roll < 99)) 
	{
		message(MSG_FAIL, 0, "You have wasted the potions.");
	}
	else 
	{
		message(MSG_FAIL, 0, "The potions explode in your hands!");
		take_hit(damroll(4,8) + penalty, "carelessly mixing potions");
	}

	/* Hack - make sure the potions are destroyed in order */
	i1= (item1 > item2) ? item1 : item2;
	i2= (item1 > item2) ? item2 : item1;

	/* Destroy a potion in the pack */
	if (i1 >= 0)
	{
		inven_item_increase(i1, -1);
		inven_item_describe(i1);
		inven_item_optimize(i1); 
	}

	/* Destroy a potion on the floor */
	else
	{
		floor_item_increase(0 - i1, -1);
		floor_item_describe(0 - i1);
		floor_item_optimize(0 - i1);
	}

	/* Destroy a potion in the pack */
	if (i2 >= 0)
	{
		inven_item_increase(i2, -1);
		inven_item_describe(i2);
		inven_item_optimize(i2);
	}

	/* Destroy a potion on the floor */
	else
	{
		floor_item_increase(0 - i2, -1);
		floor_item_describe(0 - i2);
		floor_item_optimize(0 - i2);
	}
}