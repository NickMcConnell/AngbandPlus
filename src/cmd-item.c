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
 * Determine which equipment slot (if any) an item likes
 */
static s16b wield_slot(object_type *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (INVEN_WIELD);
		}

		case TV_BOW:
		{
			return (INVEN_BOW);
		}

		case TV_RING:
		{
			int slot;

			cptr q, s;
			
			object_type *i_ptr = &inventory[INVEN_RIGHT];
			object_type *j_ptr = &inventory[INVEN_LEFT];
	
			/* Right hand is free - pick it first */
			if (!i_ptr->k_idx) return (INVEN_RIGHT);

			/* Left hand is free - pick it */
			if (!j_ptr->k_idx) return (INVEN_LEFT);

			/* 
			 * Both hands are full - time to see if we can decide where to put it ourselves 
			 * If not obvious, prefer left hand to right hand for swapping.
			 */

			/* Both rings are cursed, choose arbitrarily (will fail later anyway) */
			if (cursed_p(i_ptr) && cursed_p(j_ptr)) return (INVEN_LEFT);

			/* Left ring is cursed, but right one isn't */
			if (!cursed_p(i_ptr) && cursed_p(j_ptr)) return (INVEN_RIGHT);

			/* Right ring is cursed, but left one isn't */
			if (cursed_p(i_ptr) && !cursed_p(j_ptr)) return (INVEN_LEFT);

			/* Rings are of the same type, choice might be easy */
			if ((i_ptr->sval == j_ptr->sval) && object_known_p(i_ptr) && object_known_p(j_ptr))
			{
				switch (i_ptr->sval)
				{
					case SV_RING_PROTECTION:
					case SV_RING_LIGHTNING:
					case SV_RING_ACID:
					case SV_RING_FLAMES:
					case SV_RING_ICE:
					{
						/* Prefer the ring with lower ac bonus */
						if (i_ptr->to_a < j_ptr->to_a) return (INVEN_RIGHT);
						else return (INVEN_LEFT);
					}
					case SV_RING_ACCURACY:
					case SV_RING_DAMAGE:
					case SV_RING_SLAYING:
					{
						/* Don't auto-choose ambiguous rings (only applies to slaying) */
						if (((i_ptr->to_h < j_ptr->to_h) &&	(i_ptr->to_d > j_ptr->to_d)) ||
							((i_ptr->to_h > j_ptr->to_h) && (i_ptr->to_d < j_ptr->to_d)))
						{
							break;
						}
						else
						{
							/* Prefer the ring with lower bonuses */
							if ((i_ptr->to_h < j_ptr->to_h) || (i_ptr->to_d < j_ptr->to_d))
								return (INVEN_RIGHT);
							else return (INVEN_LEFT);
						}
					}
					default:
					{
						/* 
						 * For most cases, just prefer the ring with lower pval
						 * Note that this works fine for rings with no pval
						 */
						if (i_ptr->pval < j_ptr->pval) return (INVEN_RIGHT);
						else return (INVEN_LEFT);
					}
				}
			}
			
			/* Since we haven't chosen automatically, choose interactively */

			/* Restrict the choices */
			item_tester_tval = TV_RING;

			/* Choose a ring from the equipment only */
			q = "Replace which ring? ";
			s = "Oops.";
			if (!get_item(&slot, q, s, USE_EQUIP)) return (0);
			
			return (slot);
		}

		case TV_AMULET:
		{
			return (INVEN_NECK);
		}

		case TV_LITE:
		case TV_LITE_SPECIAL:
		{
			return (INVEN_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_BODY_ARMOR:
		{
			return (INVEN_BODY);
		}

		case TV_CLOAK:
		{
			return (INVEN_OUTER);
		}

		case TV_SHIELD:
		{
			return (INVEN_ARM);
		}

		case TV_HEADGEAR:
		{
			return (INVEN_HEAD);
		}

		case TV_GLOVES:
		{
			return (INVEN_HANDS);
		}

		case TV_BOOTS:
		{
			return (INVEN_FEET);
		}

		case TV_MUSIC:
		{
			if (cp_ptr->flags & CF_MUSIC) return (INVEN_MUSIC);
		}
	}

	/* No slot available */
	return (0);
}

/*
 * The "wearable" tester
 */
static bool item_tester_hook_wear(object_type *o_ptr)
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
	bool ring_auto_choose = FALSE;

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
	if (!(slot = wield_slot(o_ptr))) return;

	/* Prevent wielding into a cursed slot */
	if (cursed_p(&inventory[slot]))
	{
		/* Describe it */
		object_desc(o_name, &inventory[slot], FALSE, 0);

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
	p_ptr->total_weight += i_ptr->weight;

	/* Increment the equip counter by hand */
	p_ptr->equip_cnt++;

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
	else
	{
		act = "You are wearing";
	}

	/* Describe the result */
	object_desc(o_name, o_ptr, TRUE, 3);

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
	if (verify_destroy && 
		((object_value(o_ptr) >= 1) || verify_destroy_junk))
	{
		sprintf(out_val, "Really destroy %s? ", o_name);
		if (!get_check(out_val)) return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Artifacts cannot be destroyed */
	if (artifact_p(o_ptr))
	{
		/* Message */
		message_format(MSG_FAIL, 0, "You cannot destroy %s.", o_name);

		/* Remove special inscription, if any */
		if (!object_known_p(o_ptr)) switch (o_ptr->discount)
		{
			case 0:
			case INSCRIP_NULL:
			case INSCRIP_UNCURSED:
			case INSCRIP_INDESTRUCT:
			{
				o_ptr->discount = INSCRIP_INDESTRUCT;
				break;
			}
			case INSCRIP_TERRIBLE:
			case INSCRIP_CURSED:
			{
				o_ptr->discount = INSCRIP_TERRIBLE;
				break;
			}
			case INSCRIP_GOOD:
			case INSCRIP_SPECIAL:
			{
				o_ptr->discount = INSCRIP_SPECIAL;
				break;
			}
		}

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

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

	char o_name[80];

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

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	message_format(MSG_DESCRIBE, 0, "Examining %s...", o_name);

	/* Describe it fully */
	if (!identify_fully_aux(o_ptr)) message(MSG_DESCRIBE, 0, "You see nothing special.");
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
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	message_format(MSG_GENERIC, 0, "Inscribing %s.", o_name);;
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
static bool item_tester_refill_lantern(object_type *o_ptr)
{
	/* Flasks of oil are okay */
	if (o_ptr->tval == TV_FLASK) return (TRUE);

	/* Non-empty lanterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval >= SV_LANTERN) &&
	    (o_ptr->timeout > 0))
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
		/* No more fuel */
		o_ptr->timeout = 0;

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
static bool item_tester_refill_torch(object_type *o_ptr)
{
	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_TORCH)) return (TRUE);

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
	int ident, lev;

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

	/* Identity not known yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Analyze the food */
	switch (o_ptr->sval)
	{
		case SV_FOOD_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_poisoned(p_ptr->poisoned + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_BLINDNESS:
		{
			if (!p_ptr->no_blind)
			{
				if (set_blind(p_ptr->blind + rand_int(200) + 200))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARANOIA:
		{
			if (!p_ptr->bravery)
			{
				if (set_afraid(p_ptr->afraid + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_CONFUSION:
		{
			if (!p_ptr->resist_confu)
			{
				if (set_confused(p_ptr->confused + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_HALLUCINATION:
		{
			if (!p_ptr->resist_chaos)
			{
				if (set_image(p_ptr->image + rand_int(250) + 250))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARALYSIS:
		{
			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_WEAKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food");
			(void)do_dec_stat(A_STR,10,FALSE,TRUE);
			ident = TRUE;
			break;
		}

		case SV_FOOD_SICKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food");
			(void)do_dec_stat(A_CON,10,FALSE,TRUE);
			ident = TRUE;
			break;
		}

		case SV_FOOD_STUPIDITY:
		{
			take_hit(damroll(8, 8), "poisonous food");
			(void)do_dec_stat(A_INT,10,FALSE,TRUE);
			ident = TRUE;
			break;
		}

		case SV_FOOD_NAIVETY:
		{
			take_hit(damroll(8, 8), "poisonous food");
			(void)do_dec_stat(A_WIS,10,FALSE,TRUE);
			ident = TRUE;
			break;
		}

		case SV_FOOD_UNHEALTH:
		{
			if (!(p_ptr->resist_disease))
			{
				if(set_diseased(p_ptr->diseased + rand_int(15) + 50)) ident = TRUE;
			}
			break;
		}

		case SV_FOOD_DISEASE:
		{
			if (!(p_ptr->resist_disease))
			{
				if(set_diseased(p_ptr->diseased + rand_int(30) + 100)) ident = TRUE;
			}
			break;
		}

		case SV_FOOD_CURE_POISON:
		{
			if (set_poisoned(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_DISEASE:
		{
			if (set_diseased(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_BLINDNESS:
		{
			if (set_blind(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_PARANOIA:
		{
			if (set_afraid(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_CONFUSION:
		{
			if (set_confused(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_SERIOUS:
		{
			if (hp_player(damroll(4, 8))) ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_STR:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_CON:
		{
			if (do_res_stat(A_CON)) ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_DEX:
		{
			if (do_res_stat(A_DEX)) ident = TRUE;
			break;
		}
		case SV_FOOD_RESTORING:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_INT)) ident = TRUE;
			if (do_res_stat(A_WIS)) ident = TRUE;
			if (do_res_stat(A_DEX)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_FOOD_RATION:
		case SV_FOOD_JERKY:
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

		case SV_FOOD_PINT_OF_NECTAR:
		{
			message(MSG_EFFECT, 0, "That tastes good.");
			(void)set_poisoned(0);
			(void)set_diseased(p_ptr->diseased/2);
			(void)set_afraid(0);
			(void)hp_player(damroll(2, 4));
			ident = TRUE;
			break;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* We have tried it */
	object_tried(o_ptr);

	/* The player is now aware of the object */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
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
	int         item;
	cptr        q, s;

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
	int ident, lev, time;

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

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

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

		case SV_POTION_SLOWNESS:
		{
			if (set_slow(p_ptr->slow + randint(25) + 15)) ident = TRUE;
			break;
		}

		case SV_POTION_SALT_WATER:
		{
			message(MSG_EFFECT, 0, "The potion makes you vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)set_poisoned(0);
			(void)set_paralyzed(p_ptr->paralyzed + 4);
			ident = TRUE;
			break;
		}

		case SV_POTION_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_poisoned(p_ptr->poisoned + rand_int(15) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_BLINDNESS:
		{
			if (!p_ptr->no_blind)
			{
				if (set_blind(p_ptr->blind + rand_int(100) + 100))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_DISEASE:
		{
			if (!(p_ptr->resist_disease))
			{
				if(set_diseased(p_ptr->diseased + rand_int(20) + 75)) ident = TRUE;
			}
			break;
		}

		case SV_POTION_CONFUSION:
		{
			if (!p_ptr->resist_confu)
			{
				if (set_confused(p_ptr->confused + rand_int(20) + 15))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_SLEEP:
		{
			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_LOSE_MEMORIES:
		{
			if (!p_ptr->hold_life && (p_ptr->exp > 0))
			{
				message(MSG_EFFECT, 0, "You feel your memories fade.");
				lose_exp(p_ptr->exp / 4);
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RUINATION:
		{
			message(MSG_EFFECT, 0, "Your nerves and muscles feel weak and lifeless!");
			take_hit(damroll(10, 10), "a potion of Ruination");
			(void)do_dec_stat(A_STR, 25, TRUE, FALSE);
			(void)do_dec_stat(A_WIS, 25, TRUE, FALSE);
			(void)do_dec_stat(A_INT, 25, TRUE, FALSE);
			(void)do_dec_stat(A_DEX, 25, TRUE, FALSE);
			(void)do_dec_stat(A_CON, 25, TRUE, FALSE);
			(void)do_dec_stat(A_CHR, 25, TRUE, FALSE);
			ident = TRUE;
			break;
		}

		case SV_POTION_DEC_STR:
		{
			if (do_dec_stat(A_STR, 10, FALSE, TRUE)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_INT:
		{
			if (do_dec_stat(A_INT, 10, FALSE, TRUE)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_WIS:
		{
			if (do_dec_stat(A_WIS, 10, FALSE, TRUE)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_DEX:
		{
			if (do_dec_stat(A_DEX, 10, FALSE, TRUE)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CON:
		{
			if (do_dec_stat(A_CON, 10, FALSE, TRUE)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CHR:
		{
			if (do_dec_stat(A_CHR, 10, FALSE, TRUE)) ident = TRUE;
			break;
		}

		case SV_POTION_DETONATIONS:
		{
			message(MSG_EFFECT, 0, "Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "a potion of Detonation");
			(void)set_stun(p_ptr->stun + 75);
			(void)set_cut(p_ptr->cut + 5000);
			ident = TRUE;
			break;
		}

		case SV_POTION_DEATH:
		{
			message(MSG_EFFECT, 0, "A feeling of Death flows through your body.");
			take_hit(5000, "a potion of Death");
			ident = TRUE;
			break;
		}

		case SV_POTION_RISK:
		{
			if (rand_int(100)<=49) 
			{
				message(MSG_EFFECT, 0, "You took one risk too many.");
				take_hit(5000, "a potion of Risk");
				ident = TRUE;
				break;
			}
			else
			{
				message(MSG_EFFECT, 0, "Great risks bring great rewards - ");
				hp_player(damroll(4, 8));
				(void)do_inc_stat(A_STR);
				(void)do_inc_stat(A_INT);
				(void)do_inc_stat(A_WIS);
				(void)do_inc_stat(A_DEX);
				(void)do_inc_stat(A_CON);
				(void)do_inc_stat(A_CHR);
				wiz_lite();
				ident = TRUE;
			break;
			}
		}


		case SV_POTION_DEFORM:
		{
			message(MSG_EFFECT, 0, "You feel your flesh twist and contort");
			for (time = 0; time < 3; time++) scramble_stats();
			ident=TRUE;
			p_ptr->ht+=(rand_int(11)-5);
			p_ptr->wt+=(rand_int(21)-10);
			if (p_ptr->ht<20) p_ptr->ht=20;
			if (p_ptr->ht<20) p_ptr->ht=20;
			break;
		}

		case SV_POTION_INFRAVISION:
		{
			if (set_tim_infra(p_ptr->tim_infra + 50 + randint(50)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_DETECT_INVIS:
		{
			if (set_tim_see_invis(p_ptr->tim_see_invis + 12 + randint(12)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_INVISIBILITY:
		{
			if (set_tim_invis(p_ptr->tim_invis + 10 + randint(10)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_SLOW_POISON:
		{
			if (set_poisoned(p_ptr->poisoned / 2)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_POISON:
		{
			if (set_poisoned(0)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_DISEASE:
		{
			if (set_diseased(0)) ident = TRUE;
			break;
		}

		case SV_POTION_BOLDNESS:
		{
			if (set_afraid(0)) ident = TRUE;
			break;
		}

		case SV_POTION_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_fast(randint(25) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			break;
		}

		case SV_POTION_RESIST_HEAT:
		{
			if (set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_COLD:
		{
			if (set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_HEROISM:
		{
			if (hp_player(10)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (set_hero(p_ptr->hero + randint(25) + 25)) ident = TRUE;
			break;
		}

		case SV_POTION_BESERK_STRENGTH:
		{
			if (hp_player(30)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (set_rage(p_ptr->rage + randint(25) + 25)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			if (hp_player(damroll(2, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			if (hp_player(damroll(4, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			if (hp_player(damroll(6, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_diseased(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			if (hp_player(1200)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_diseased(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_LIFE:
		{
			message(MSG_EFFECT, 0, "You feel life flow through your body!");
			restore_exp();
			hp_player(5000);
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_diseased(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);
			ident = TRUE;
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				message(MSG_EFFECT, 0, "Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESTORE_EXP:
		{
			if (restore_exp()) ident = TRUE;
			break;
		}

		case SV_POTION_RES_STR:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_INT:
		{
			if (do_res_stat(A_INT)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_WIS:
		{
			if (do_res_stat(A_WIS)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_DEX:
		{
			if (do_res_stat(A_DEX)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_CON:
		{
			if (do_res_stat(A_CON)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_CHR:
		{
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_STR:
		{
			if (do_inc_stat(A_STR)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_INT:
		{
			if (do_inc_stat(A_INT)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_WIS:
		{
			if (do_inc_stat(A_WIS)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_DEX:
		{
			if (do_inc_stat(A_DEX)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_CON:
		{
			if (do_inc_stat(A_CON)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_CHR:
		{
			if (do_inc_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_AUGMENTATION:
		{
			if (do_inc_stat(A_STR)) ident = TRUE;
			if (do_inc_stat(A_INT)) ident = TRUE;
			if (do_inc_stat(A_WIS)) ident = TRUE;
			if (do_inc_stat(A_DEX)) ident = TRUE;
			if (do_inc_stat(A_CON)) ident = TRUE;
			if (do_inc_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_ENLIGHTENMENT:
		{
			message(MSG_EFFECT, 0, "An image of your surroundings forms in your mind...");
			wiz_lite();
			ident = TRUE;
			break;
		}

		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			message(MSG_EFFECT, 0, "You begin to feel more enlightened...");
			message_flush();
			wiz_lite();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			identify_pack();
			self_knowledge();
			ident = TRUE;
			break;
		}

		case SV_POTION_SELF_KNOWLEDGE:
		{
			message(MSG_EFFECT, 0, "You begin to know yourself a little better...");
			message_flush();
			self_knowledge();
			ident = TRUE;
			break;
		}

		case SV_POTION_EXPERIENCE:
		{
			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				message(MSG_EFFECT, 0, "You feel more experienced.");
				gain_exp(ee);
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESISTANCE:
		{
			time = randint(20) + 20;
			if (set_oppose_acid(p_ptr->oppose_acid + time)) ident = TRUE;
			if (set_oppose_elec(p_ptr->oppose_elec + time)) ident = TRUE;
			if (set_oppose_fire(p_ptr->oppose_fire + time)) ident = TRUE;
			if (set_oppose_cold(p_ptr->oppose_cold + time)) ident = TRUE;
			break;
		}

		case SV_POTION_STAR_RESISTANCE:
		{
			time = randint(15) + 10;
			if (set_oppose_acid(p_ptr->oppose_acid + time)) ident = TRUE;
			if (set_oppose_elec(p_ptr->oppose_elec + time)) ident = TRUE;
			if (set_oppose_fire(p_ptr->oppose_fire + time)) ident = TRUE;
			if (set_oppose_cold(p_ptr->oppose_cold + time)) ident = TRUE;
			if (set_oppose_pois(p_ptr->oppose_pois + time)) ident = TRUE;
			if (set_tim_res_lite(p_ptr->tim_res_lite + time)) ident = TRUE;
			if (set_tim_res_dark(p_ptr->tim_res_dark + time)) ident = TRUE;
			if (set_tim_res_confu(p_ptr->tim_res_confu + time)) ident = TRUE;
			if (set_tim_res_sound(p_ptr->tim_res_sound + time)) ident = TRUE;
			if (set_tim_res_shard(p_ptr->tim_res_shard + time)) ident = TRUE;
			if (set_tim_res_nexus(p_ptr->tim_res_nexus + time)) ident = TRUE;
			if (set_tim_res_nethr(p_ptr->tim_res_nethr + time)) ident = TRUE;
			if (set_tim_res_chaos(p_ptr->tim_res_chaos + time)) ident = TRUE;
			if (set_tim_res_disease(p_ptr->tim_res_disease + time)) ident = TRUE;
			if (set_tim_res_water(p_ptr->tim_res_water + time)) ident = TRUE;
			break;
		}

		case SV_POTION_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) ident = TRUE;
			break;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item has been tried */
	object_tried(o_ptr);

	/* An identification was made */
	if ((ident) && (!object_aware_p(o_ptr)))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}
		
	/* Check if the alchemicl formula is learnt */
	if (!((potion_alch[o_ptr->sval].known1) && (potion_alch[o_ptr->sval].known2)) &&
		(rand_int(100) <= p_ptr->skill[SK_ALC] - 30))
	{
		object_kind *k_ptr;

		int k;
		bool p1, p2;

		bool learn = FALSE;

		/* Check if the components are known */
		for (k = 1; k < z_info->k_max; k++)
		{
			k_ptr = &k_info[k];

			/* Found a match */
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == potion_alch[o_ptr->sval].sval1)) 
				p1 = k_ptr->aware;
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == potion_alch[o_ptr->sval].sval2)) 
				p2 = k_ptr->aware;
		}

		/* 
		 * Learn, if you are aware of the component potion, but
		 * you are not yet aware it is part of the potion.
		 */
		if ((!(potion_alch[o_ptr->sval].known1)) && p1) 
		{
			potion_alch[o_ptr->sval].known1 = TRUE;
			learn = TRUE;
		}
		else if ((!(potion_alch[o_ptr->sval].known2)) && p2) 
		{
			potion_alch[o_ptr->sval].known2 = TRUE;
			learn = TRUE;
		}

		/* Message, if something new was learned */
		if (learn)
		{
			message(MSG_STUDY, 0, "You have gained alchemical knowledge!");
			alchemy_describe(line, o_ptr->sval);
			message_format(MSG_STUDY, -1, "%s", line);
		}
	}
		
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Potions can feed the player */
	(void)set_food(p_ptr->food + o_ptr->pval);

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
 * Curse the players armor
 */
static bool curse_armor(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the body armor */
	o_ptr = &inventory[INVEN_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw for artifacts */
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
	{
		/* Cool */
		message_format(MSG_ITEM_RESIST, o_ptr->k_idx, 
			"A terrible balck aura tries to surround %s, but it resists the effects!", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		message_format(MSG_ITEM_DAMAGE, o_ptr->k_idx, 
			"A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->a_idx = 0;
		o_ptr->e_idx = EGO_BLASTED;
		o_ptr->to_a = 0 - randint(5) - randint(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	return (TRUE);
}

/*
 * Curse the players weapon
 */
static bool curse_weapon(void)
{
	object_type *o_ptr;

	char o_name[80];

	/* Curse the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);

	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw */
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
	{
		/* Cool */
		message_format(MSG_ITEM_RESIST, o_ptr->k_idx, 
			"A terrible balck aura tries to surround %s, but it resists the effects!", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		message_format(MSG_ITEM_DAMAGE, o_ptr->k_idx, 
			"A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->a_idx = 0;
		o_ptr->e_idx = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	/* Notice */
	return (TRUE);
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
	int k, used_up, ident, lev;

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

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Assume the scroll will get used up */
	used_up = TRUE;

	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
		case SV_SCROLL_DARKNESS:
		{
			if (!p_ptr->no_blind)
			{
				(void)set_blind(p_ptr->blind + 3 + randint(5));
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			message(MSG_EFFECT, 0, "There is a high pitched humming noise.");
			aggravate_monsters(0);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_ARMOR:
		{
			if (curse_armor()) ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_WEAPON:
		{
			if (curse_weapon()) ident = TRUE;
			break;
		}

		case SV_SCROLL_SUMMON_MONSTER:
		{
			for (k = 0; k < randint(3); k++)
			{
				if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, 0))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_UNDEAD:
		{
			for (k = 0; k < randint(3); k++)
			{
				if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_UNDEAD))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_DRAGON:
		{
			for (k = 0; k < randint(3); k++)
			{
				if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_DRAGON))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_TRAP_CREATION:
		{
			if (trap_creation()) ident = TRUE;
			break;
		}

		case SV_SCROLL_PHASE_DOOR:
		{
			teleport_player(10);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT:
		{
			teleport_player(100);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_WORD_OF_RECALL:
		{
			set_recall();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ALTER_REALITY:
		{
			message(MSG_EFFECT, 0, "The world changes!");
			/* Leaving */
			p_ptr->leaving = TRUE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_IDENTIFY:
		{
			ident = TRUE;
			if (!ident_spell()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_STAR_IDENTIFY:
		{
			ident = TRUE;
			if (!identify_fully()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				message(MSG_EFFECT, 0, "You feel as if someone is watching over you.");
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			remove_all_curse();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_ARMOR:
		{
			ident = TRUE;
			if (!enchant_spell(0, 0, 1)) used_up = FALSE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
			if (!enchant_spell(1, 0, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
			if (!enchant_spell(0, 1, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_ARMOR:
		{
			if (!enchant_spell(0, 0, randint(3) + 2)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
			if (!enchant_spell(randint(3), randint(3), 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_RECHARGING:
		{
			if (!recharge(60)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ELEMENTAL_BRAND:
		{
			/* Hack - choose random brand */
			k = rand_int(4);
			if (!brand_weapon(0, EGO_BRAND_ACID+k, TRUE)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_LIGHT:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

		case SV_SCROLL_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_GOLD:
		{
			if (detect_treasure()) ident = TRUE;
			if (detect_objects_gold()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_ITEM:
		{
			if (detect_objects_normal()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_INVIS:
		{
			if (detect_monsters_invis()) ident = TRUE;
			break;
		}

		case SV_SCROLL_ABSORB_HIT:
		{
			if (set_absorb(p_ptr->absorb + randint(20) + 25)) ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESSING:
		{
			if (set_blessed(p_ptr->blessed + randint(12) + 6)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_CHANT:
		{
			if (set_blessed(p_ptr->blessed + randint(24) + 12)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_PRAYER:
		{
			if (set_blessed(p_ptr->blessed + randint(48) + 24)) ident = TRUE;
			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (p_ptr->confusing == 0)
			{
				message(MSG_EFFECT, 0, "Your hands begin to glow.");
				p_ptr->confusing = TRUE;
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			k = 3 * p_ptr->lev;
			if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			warding_glyph();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			destroy_area(p_ptr->py, p_ptr->px, 15, TRUE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_DISPEL_UNDEAD:
		{
			if (dispel_undead(60)) ident = TRUE;
			break;
		}

		case SV_SCROLL_GENOCIDE:
		{
			(void)genocide();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_MASS_GENOCIDE:
		{
			(void)mass_genocide();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ACQUIREMENT:
		{
			acquirement(p_ptr->py, p_ptr->px, 1, TRUE, TRUE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ACQUIREMENT:
		{
			acquirement(p_ptr->py, p_ptr->px, randint(2) + 1, TRUE, TRUE);
			ident = TRUE;
			break;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item was tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
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
	if (no_lite())
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

static int check_item_success(int diff, int lev, bool binary)
{
	int chance;
	
	sint result, i;

	/* Base chance of success */
	chance = p_ptr->skill[SK_DEV];

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Stunning hurts skill */
	if (p_ptr->stun) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < diff) && (rand_int(diff - chance + 1) == 0))
	{
		chance = diff;
	}
	else if (chance < 0) return 0;

	/* Check for success */
	i = (randint(chance) - diff + 1);

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
	else if (i > 0) result = 100;

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
	int ident, k;

	object_type *o_ptr;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;

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

	/* Not identified yet */
	ident = FALSE;

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
		return;
	}

	/* Sound */
	sound(MSG_ZAP);

	/* Analyze the staff */
	switch (o_ptr->sval)
	{
		case SV_STAFF_DARKNESS:
		{
			if (!p_ptr->no_blind)
			{
				if (set_blind(p_ptr->blind + 3 + randint(5))) ident = TRUE;
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_STAFF_SLOWNESS:
		{
			if (set_slow(p_ptr->slow + randint(30) + 15)) ident = TRUE;
			break;
		}

		case SV_STAFF_HASTE_MONSTERS:
		{
			if (speed_monsters()) ident = TRUE;
			break;
		}

		case SV_STAFF_SUMMONING:
		{
			for (k = 0; k < randint(4); k++)
			{
				if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, 0))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_TELEPORTATION:
		{
			teleport_player(100);
			ident = TRUE;
			break;
		}

		case SV_STAFF_IDENTIFY:
		{
			if (!ident_spell()) use_charge = FALSE;
			ident = TRUE;
			break;
		}

		case SV_STAFF_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				if (!p_ptr->blind)
				{
					message(MSG_EFFECT, 0, "The staff glows blue for a moment...");
				}
				ident = TRUE;
			}
			break;
		}

		case SV_STAFF_STARLITE:
		{
			if (!p_ptr->blind)
			{
				message(MSG_EFFECT, 0, "The end of the staff glows brightly...");
			}
			for (k = 0; k < 8; k++) lite_line(ddd[k],damroll(6,8));
			ident = TRUE;
			break;
		}

		case SV_STAFF_LITE:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

		case SV_STAFF_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_GOLD:
		{
			if (detect_treasure()) ident = TRUE;
			if (detect_objects_gold()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_ITEM:
		{
			if (detect_objects_normal()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_INVIS:
		{
			if (detect_monsters_invis()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_EVIL:
		{
			if (detect_monsters_evil()) ident = TRUE;
			break;
		}

		case SV_STAFF_CURE_LIGHT:
		{
			if (hp_player(randint(8))) ident = TRUE;
			break;
		}

		case SV_STAFF_CURING:
		{
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_diseased(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_THE_MAGI:
		{
			if (do_res_stat(A_INT)) ident = TRUE;
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				ident = TRUE;
				message(MSG_EFFECT, 0, "Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			}
			break;
		}

		case SV_STAFF_SLEEP_MONSTERS:
		{
			if (sleep_monsters()) ident = TRUE;
			break;
		}

		case SV_STAFF_SLOW_MONSTERS:
		{
			if (slow_monsters()) ident = TRUE;
			break;
		}

		case SV_STAFF_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_fast(randint(30) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			break;
		}

		case SV_STAFF_PROBING:
		{
			probing();
			ident = TRUE;
			break;
		}

		case SV_STAFF_DISPEL_EVIL:
		{
			if (dispel_evil(60)) ident = TRUE;
			break;
		}

		case SV_STAFF_POWER:
		{
			if (dispel_monsters(120)) ident = TRUE;
			break;
		}

		case SV_STAFF_HOLINESS:
		{
			if (dispel_evil(120)) ident = TRUE;
			k = 3 * p_ptr->lev;
			if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_diseased(0)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (hp_player(50)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_GENOCIDE:
		{
			(void)genocide();
			ident = TRUE;
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			earthquake(p_ptr->py, p_ptr->px, 10);
			ident = TRUE;
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			destroy_area(p_ptr->py, p_ptr->px, 15, TRUE);
			ident = TRUE;
			break;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);
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
		p_ptr->total_weight -= i_ptr->weight;
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
	int ident, dir, sval, check;
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

	/* Not identified yet */
	ident = FALSE;

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
		return;
	}

	/* Sound */
	sound(MSG_ZAP);

	/* XXX Hack -- Extract the "sval" effect */
	sval = o_ptr->sval;

	/* XXX Hack -- Wand of wonder can do anything before it */
	if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);

	/* Partial success */
	plev = ((plev - 1) * check) / 100 + 1;
	
	/* Analyze the wand */
	switch (sval)
	{
		case SV_WAND_HEAL_MONSTER:
		{
			if (heal_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_HASTE_MONSTER:
		{
			if (speed_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_CLONE_MONSTER:
		{
			if (clone_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_TRAP_DOOR_DEST:
		{
			if (destroy_door(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_STONE_TO_MUD:
		{
			if (wall_to_mud(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_LITE:
		{
			message(MSG_EFFECT, 0, "A line of blue shimmering light appears.");
			lite_line(dir,damroll(6,8));
			ident = TRUE;
			break;
		}

		case SV_WAND_SLEEP_MONSTER:
		{
			if (sleep_monster(dir, 15)) ident = TRUE;
			break;
		}

		case SV_WAND_SLOW_MONSTER:
		{
			if (slow_monster(dir, 15)) ident = TRUE;
			break;
		}

		case SV_WAND_CONFUSE_MONSTER:
		{
			if (confuse_monster(dir, 10)) ident = TRUE;
			break;
		}

		case SV_WAND_FEAR_MONSTER:
		{
			if (fear_monster(dir, 10)) ident = TRUE;
			break;
		}

		case SV_WAND_DRAIN_LIFE:
		{
			if (drain_life(dir, 60 + (plev))) ident = TRUE;
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, 12, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2, 6));
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(3 + (plev / 5), 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BOLT:
		{
			fire_bolt_or_beam(20, GF_ELEC, dir, damroll(3 + (plev / 5), 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BOLT:
		{
			fire_bolt_or_beam(20, GF_FIRE, dir, damroll(4 + (plev / 5), 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			fire_bolt_or_beam(20, GF_COLD, dir, damroll(3 + (plev / 5), 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_FIRE:
		{
			fire_ball(GF_FIRE, dir, 80 + (plev * 2), 3);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			fire_ball(GF_COLD, dir, 80 + (plev * 2), 3);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			switch (randint(5))
			{
				case 1:
				{
					fire_ball(GF_ACID, dir, 80 + (plev * 2), 3);
					break;
				}

				case 2:
				{
					fire_ball(GF_ELEC, dir, 60 + (plev * 2), 3);
					break;
				}

				case 3:
				{
					fire_ball(GF_FIRE, dir, 80 + (plev * 2), 3);
					break;
				}

				case 4:
				{
					fire_ball(GF_COLD, dir, 60 + (plev * 2), 3);
					break;
				}

				default:
				{
					fire_ball(GF_POIS, dir, 50 + (plev * 2), 3);
					break;
				}
			}

			ident = TRUE;
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
			if (drain_life(dir, 100 + (plev * 2))) ident = TRUE;
			break;
		}

		case SV_WAND_CALL_MONSTER:
		{
			if (call_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_CALM_MONSTER:
		{
			if (calm_monster(dir, 15)) ident = TRUE;
			break;
		}

		case SV_WAND_BLIND_MONSTER:
		{
			if (blind_monster(dir, 15)) ident = TRUE;
			break;
		}

		case SV_WAND_WONDER:
		{
			message(MSG_EFFECT, 0, "Oops.  Wand of wonder activated.");
			break;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Mark it as tried */
	object_tried(o_ptr);

	/* Apply identification */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);
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
		p_ptr->total_weight -= i_ptr->weight;
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
	int     item;
	cptr    q, s;

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
 * Activate (zap) a Rod.    Rods may be fully identified through use 
 * (although it's not easy).  Rods now use timeouts to determine charging 
 * status, and pvals have become the cost of zapping a rod (how long it 
 * takes between zaps).  Pvals are defined for each rod in k_info. -LM-
 */
static void do_cmd_zap_rod_aux(int item)
{
	int ident, k, chance;
	int plev = p_ptr->lev;

	object_type *o_ptr;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;

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

	/* A single rod is still charging */
	if ((o_ptr->number == 1) && (o_ptr->timeout))
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "The rod is still charging.");
		return;
	}
	/* A stack of rods lacks enough energy. */
	else if ((o_ptr->number > 1) && (o_ptr->timeout > o_ptr->pval - k_info[o_ptr->k_idx].pval))
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "The rods are all still charging.");
		return;
	}

	o_ptr->timeout += k_info[o_ptr->k_idx].pval;

	/* Sound */
	sound(MSG_ZAP);

	/* Analyze the rod */
	switch (o_ptr->sval)
	{
		case SV_ROD_SUMMON:
		{
			for (k = 0; k < randint(5); k++)
			{
				if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, 0))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_ROD_DARKNESS:
		{
			if (!p_ptr->no_blind)
			{
				if (set_blind(p_ptr->blind + 3 + randint(5))) ident = TRUE;
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_ROD_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

		case SV_ROD_IDENTIFY:
		{
			ident = TRUE;
			if (!ident_spell()) use_charge = FALSE;
			break;
		}

		case SV_ROD_RECALL:
		{
			set_recall();
			ident = TRUE;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

		case SV_ROD_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

		case SV_ROD_DETECTION:
		{
			detect_all();
			ident = TRUE;
			break;
		}

		case SV_ROD_PROBING:
		{
			probing();
			ident = TRUE;
			break;
		}

		case SV_ROD_CURING:
		{
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_diseased(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_ROD_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) ident = TRUE;
			break;
		}


		case SV_ROD_HEALING:
		{
			if (hp_player(500)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_ROD_RESTORATION:
		{
			if (restore_exp()) ident = TRUE;
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_INT)) ident = TRUE;
			if (do_res_stat(A_WIS)) ident = TRUE;
			if (do_res_stat(A_DEX)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_ROD_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_fast(randint(30) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			break;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- deal with cancelled zap */
	if (!use_charge)
	{
		o_ptr->timeout += k_info[o_ptr->k_idx].pval;
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

	/* Get an item */
	q = "Zap which rod? ";
	s = "You have no rod to zap.";
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
	int ident, dir, check;
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

	/* A single talisman is still charging */
	if ((o_ptr->number == 1) && (o_ptr->timeout))
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "The talisman is still charging.");
		return;
	}

	/* A stack of talismans lacks enough energy. */
	else if ((o_ptr->number > 1) && (o_ptr->timeout > o_ptr->pval - k_info[o_ptr->k_idx].pval))
	{
		if (flush_failure) flush();
		message(MSG_FAIL, 0, "The talismans are all still charging.");
		return;
	}

	o_ptr->timeout += k_info[o_ptr->k_idx].pval;

	/* Partial success */
	plev = ((plev - 1) * check) / 100 + 1;

	/* Sound */
	sound(MSG_ZAP);

	/* Analyze the talisman */
	switch (o_ptr->sval)
	{
		case SV_TALIS_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) ident = TRUE;
			break;
		}

		case SV_TALIS_DISARMING:
		{
			if (disarm_trap(dir)) ident = TRUE;
			break;
		}

		case SV_TALIS_LITE:
		{
			message(MSG_EFFECT, 0, "A line of blue shimmering light appears.");
			lite_line(dir,damroll(6,8));
			ident = TRUE;
			break;
		}

		case SV_TALIS_DRAIN_LIFE:
		{
			if (drain_life(dir, 60 + (plev))) ident = TRUE;
			break;
		}

		case SV_TALIS_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			break;
		}

		case SV_TALIS_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(3 + (plev / 5), 8));
			ident = TRUE;
			break;
		}

		case SV_TALIS_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(3 + (plev / 5), 8));
			ident = TRUE;
			break;
		}

		case SV_TALIS_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(5 + (plev / 5), 8));
			ident = TRUE;
			break;
		}

		case SV_TALIS_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(3 + (plev / 5), 8));
			ident = TRUE;
			break;
		}

		case SV_TALIS_FORCE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FORCE, dir, damroll(3 + (plev / 5), 10));
			ident = TRUE;
			break;
		}

		case SV_TALIS_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 40 + (plev * 2), 2);
			ident = TRUE;
			break;
		}

		case SV_TALIS_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 30 + (plev * 2), 2);
			ident = TRUE;
			break;
		}

		case SV_TALIS_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 45 + (plev * 2), 2);
			ident = TRUE;
			break;
		}

		case SV_TALIS_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 35 + (plev * 2), 2);
			ident = TRUE;
			break;
		}

		case SV_TALIS_STONE_TO_MUD:
		{
			if (wall_to_mud(dir)) ident = TRUE;
			break;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
}

/* Invoke a Talisman */
void do_cmd_invoke_talisman(void)
{
	int item;
	cptr q, s;

	/* Restrict choices to rods */
	item_tester_tval = TV_TALISMAN;

	/* Get an item */
	q = "Invoke which talisman? ";
	s = "You have no talisman to invoke.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Invoke the talisman */
	do_cmd_invoke_talisman_aux(item);
}

/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type *o_ptr)
{
	u32b f1, f2, f3, f4;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Hack -- activate the ring of power
 */
static void ring_of_power(int dir)
{
	/* Pick a random effect */
	switch (randint(10))
	{
		case 1:
		case 2:
		{
			/* Message */
			message(MSG_EFFECT, 0, "You are surrounded by a malignant aura.");

			/* Decrease all stats (permanently) */
			(void)do_dec_stat(A_STR, 50, TRUE, FALSE);
			(void)do_dec_stat(A_INT, 50, TRUE, FALSE);
			(void)do_dec_stat(A_WIS, 50, TRUE, FALSE);
			(void)do_dec_stat(A_DEX, 50, TRUE, FALSE);
			(void)do_dec_stat(A_CON, 50, TRUE, FALSE);
			(void)do_dec_stat(A_CHR, 50, TRUE, FALSE);

			/* Lose some experience (permanently) */
			p_ptr->exp -= (p_ptr->exp / 4);
			p_ptr->max_exp -= (p_ptr->exp / 4);
			check_experience();

			break;
		}

		case 3:
		{
			/* Message */
			message(MSG_EFFECT, 0, "You are surrounded by a powerful aura.");

			/* Dispel monsters */
			dispel_monsters(1000);

			break;
		}

		case 4:
		case 5:
		case 6:
		{
			/* Mana Ball */
			fire_ball(GF_MANA, dir, 300, 3);

			break;
		}

		case 7:
		case 8:
		case 9:
		case 10:
		{
			/* Mana Bolt */
			fire_bolt(GF_MANA, dir, 250);

			break;
		}
	}
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
	int i, k, dir, lev, chance;

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
	if (artifact_p(o_ptr)) lev = a_info[o_ptr->a_idx].level;

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

		/* Get the basic name of the object */
		object_desc(o_name, o_ptr, FALSE, 0);

		switch (a_ptr->activation)
		{
			case ACT_ILLUMINATION:
			{
				message_format(MSG_EFFECT, 0, "The %s wells with clear light...", o_name);
				lite_area(damroll(2, 15), 3);
				break;
			}

			case ACT_MAGIC_MAP:
			{
				message_format(MSG_EFFECT, 0, "The %s shines brightly...", o_name);
				map_area();
				break;
			}

			case ACT_CLAIRVOYANCE:
			{
				message_format(MSG_EFFECT, 0, "The %s glows a deep green...", o_name);
				wiz_lite();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case ACT_PROT_EVIL:
			{
				message_format(MSG_EFFECT, 0, "The %s lets out a shrill wail...", o_name);
				k = 3 * p_ptr->lev;
				(void)set_protevil(p_ptr->protevil + randint(25) + k);
				break;
			}

			case ACT_DISP_EVIL:
			{
				message_format(MSG_EFFECT, 0, "The %s floods the area with goodness...", o_name);
				dispel_evil(p_ptr->lev * 5);
				break;
			}

			case ACT_HASTE2:
			{
				message_format(MSG_EFFECT, 0, "The %s glows brightly...", o_name);
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(75) + 75);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				break;
			}

			case ACT_FIRE3:
			{
				message_format(MSG_EFFECT, 0, "The %s glows deep red...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 120, 3);
				break;
			}

			case ACT_FROST5:
			{
				message_format(MSG_EFFECT, 0, "The %s glows bright white...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 200, 3);
				break;
			}

			case ACT_ELEC2:
			{
				message_format(MSG_EFFECT, 0, "The %s glows deep blue...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 250, 3);
				break;
			}

			case ACT_BIZZARE:
			{
				message_format(MSG_EFFECT, 0, "The %s glows intensely black...", o_name);
				if (!get_aim_dir(&dir)) return;
				ring_of_power(dir);
				break;
			}


			case ACT_STAR_BALL:
			{
				message_format(MSG_EFFECT, 0, "Your %s is surrounded by lightning...", o_name);
				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
				break;
			}

			case ACT_RAGE:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows blood-red...", o_name);
				(void)set_rage(p_ptr->rage + randint(50) + 50);
				break;
			}

			case ACT_RAGE_BLESS_RESIST:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows many colours...", o_name);
				(void)hp_player(30);
				(void)set_afraid(0);
				(void)set_rage(p_ptr->rage + randint(50) + 50);
				(void)set_blessed(p_ptr->blessed + randint(50) + 50);
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(50) + 50);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(50) + 50);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(50) + 50);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(50) + 50);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(50) + 50);
				break;
			}

			case ACT_HEAL2:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows a bright white...", o_name);
				message(MSG_EFFECT, 0, "You feel much better...");
				(void)hp_player(1000);
				(void)set_cut(0);
				break;
			}

			case ACT_PHASE:
			{
				message_format(MSG_EFFECT, 0, "Your %s twists space around you...", o_name);
				teleport_player(10);
				break;
			}

			case ACT_GENOCIDE:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows deep blue...", o_name);
				(void)genocide();
				break;
			}

			case ACT_TRAP_DOOR_DEST:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows bright red...", o_name);
				destroy_doors_touch();
				break;
			}

			case ACT_DETECT:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows bright white...", o_name);
				message(MSG_EFFECT, 0, "An image forms in your mind...");
				detect_all();
				break;
			}

			case ACT_HEAL1:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows deep blue...", o_name);
				message(MSG_EFFECT, 0, "You feel a warm tingling inside...");
				(void)hp_player(500);
				(void)set_cut(0);
				break;
			}

			case ACT_RESIST:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows many colours...", o_name);
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

			case ACT_SLEEP:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows deep blue...", o_name);
				sleep_monsters_touch();
				break;
			}

			case ACT_RECHARGE1:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows bright yellow...", o_name);
				recharge(60);
				break;
			}

			case ACT_TELEPORT:
			{
				message_format(MSG_EFFECT, 0, "Your %s twists space around you...", o_name);
				teleport_player(100);
				break;
			}

			case ACT_RESTORE_LIFE:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows a deep red...", o_name);
				restore_exp();
				break;
			}

			case ACT_RESTORE_MANA:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows in all the colours of the rainbow...", o_name);
				if (p_ptr->csp < p_ptr->msp)
				{
					p_ptr->csp = p_ptr->msp;
					p_ptr->csp_frac = 0;
					message(MSG_EFFECT, 0, "Your feel your head clear.");
					p_ptr->redraw |= (PR_MANA);
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				}
				break;
			}

			case ACT_MISSILE:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows extremely brightly...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir, damroll(2, 6));
				break;
			}

			case ACT_DIMENSION_DOOR:
			{
				message_format(MSG_EFFECT, 0, "Your %s opens a door in the fabric of reality...", o_name);
				message(MSG_EFFECT, 0, "Choose a location to teleport to.");
				message_flush();
				dimen_door();
				break;
			}

			case ACT_FIRE1:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in fire...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				break;
			}

			case ACT_FROST1:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in frost...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				break;
			}

			case ACT_LIGHTNING_BOLT:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in sparks...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				break;
			}

			case ACT_LITE_BOLT:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in light...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_LITE, dir, damroll(4, 8));
				break;
			}

			case ACT_DARK_BOLT:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in darkness...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_DARK, dir, damroll(4, 8));
				break;
			}

			case ACT_WATER_BOLT:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in water...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_WATER, dir, damroll(5, 7));
				break;
			}

			case ACT_MANA_BOLT:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in a purple aura...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MANA, dir, damroll(8, 8));
				break;
			}

			case ACT_ACID1:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in acid...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ACID, dir, damroll(5, 8));
				break;
			}

			case ACT_ARROW:
			{
				message_format(MSG_EFFECT, 0, "Your %s grows magical spikes...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ARROW, dir, 150);
				break;
			}

			case ACT_HASTE1:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows bright green...", o_name);
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + 20);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				break;
			}

			case ACT_REM_FEAR_POIS:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows deep blue...", o_name);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				break;
			}

			case ACT_STINKING_CLOUD:
			{
				message_format(MSG_EFFECT, 0, "Your %s throbs deep green...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 12, 3);
				break;
			}

			case ACT_FROST2:
			{
				message_format(MSG_EFFECT, 0, "Your %s is covered in frost...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 48, 2);
				break;
			}

			case ACT_FROST4:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows a pale blue...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(12, 8));
				break;
			}

			case ACT_FROST3:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows a intense blue...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 100, 2);
				break;
			}

			case ACT_FIRE2:
			{
				message_format(MSG_EFFECT, 0, "Your %s rages in fire...", o_name);
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 2);
				break;
			}

			case ACT_DRAIN_LIFE2:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows black...", o_name);
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 120);
				break;
			}

			case ACT_STONE_TO_MUD:
			{
				message_format(MSG_EFFECT, 0, "Your %s pulsates...", o_name);
				if (!get_aim_dir(&dir)) return;
				wall_to_mud(dir);
				break;
			}

			case ACT_MASS_GENOCIDE:
			{
				message_format(MSG_EFFECT, 0, "Your %s lets out a long, shrill note...", o_name);
				(void)mass_genocide();
				break;
			}

			case ACT_CURE_WOUNDS:
			{
				message_format(MSG_EFFECT, 0, "Your %s radiates deep purple...", o_name);
				hp_player(damroll(4, 8));
				(void)set_cut((p_ptr->cut / 2) - 50);
				break;
			}

			case ACT_TELE_AWAY:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows deep red...", o_name);
				if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				break;
			}

			case ACT_WOR:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows soft white...", o_name);
				set_recall();
				break;
			}

			case ACT_CONFUSE:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows in scintillating colours...", o_name);
				if (!get_aim_dir(&dir)) return;
				confuse_monster(dir, 20);
				break;
			}

			case ACT_IDENTIFY:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows yellow...", o_name);
				if (!ident_spell()) return;
				break;
			}

			case ACT_PROBE:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows brightly...", o_name);
				probing();
				break;
			}

			case ACT_DRAIN_LIFE1:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows white...", o_name);
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 90);
				break;
			}

			case ACT_FIREBRAND:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows deep red...", o_name);
				(void)brand_weapon(TV_BOLT,EGO_FLAME,TRUE);
				break;
			}

			case ACT_LITEBRAND:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows blinding white...", o_name);
				(void)brand_weapon(TV_BOLT,EGO_AMMO_LITE,TRUE);
				break;
			}

			case ACT_VENOMSHOT:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows sickly green...", o_name);
				(void)brand_weapon(TV_SHOT,EGO_POISON,TRUE);
				break;
			}

			case ACT_SATISFY_HUNGER:
			{
				message_format(MSG_EFFECT, 0, "Your %s glow a soft, gentle red...", o_name);
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case ACT_DETECT_ENCHANT:
			{
				message_format(MSG_EFFECT, 0, "Your %s emits a low frequancy ringing...", o_name);
				(void)detect_objects_magic();
			}

			case ACT_DETECT_TRAPS:
			{
				message_format(MSG_EFFECT, 0, "Your %s shimmers brightly...", o_name);
				(void)detect_traps();
			}

			case ACT_DETECT_TREASURE:
			{
				message_format(MSG_EFFECT, 0, "Your %s glows golden yellow...", o_name);
				(void)detect_treasure();
			}

			case ACT_CALM_NON_CHAOS:
			{
				message_format(MSG_EFFECT, 0, "Your %s resonates with the voice of law...", o_name);
				(void)calm_non_chaos();
			}

			case ACT_CALL_MONSTER:
			{
				message_format(MSG_EFFECT, 0, "Your %s vibrates slowly...", o_name);
				if (!get_aim_dir(&dir)) return;
				call_monster(dir);
			}
		}

		/* Set the recharge time */
		if (a_ptr->randtime)
			o_ptr->timeout = a_ptr->time + (byte)randint(a_ptr->randtime);
		else
			o_ptr->timeout = a_ptr->time;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}

	/* Hack -- Dragon Scale Mail can be activated as well */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return;

		/* Branch on the sub-type */
		switch (o_ptr->sval)
		{
			case SV_DRAGON_BLUE:
			{
				message(MSG_DSM, o_ptr->sval, "You breathe lightning.");
				fire_ball(GF_ELEC, dir, 100, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_WHITE:
			{
				message(MSG_DSM, o_ptr->sval, "You breathe frost.");
				fire_ball(GF_COLD, dir, 110, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_BLACK:
			{
				message(MSG_DSM, o_ptr->sval, "You breathe acid.");
				fire_ball(GF_ACID, dir, 130, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_GREEN:
			{
				message(MSG_DSM, o_ptr->sval, "You breathe poison gas.");
				fire_ball(GF_POIS, dir, 150, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_RED:
			{
				message(MSG_DSM, o_ptr->sval, "You breathe fire.");
				fire_ball(GF_FIRE, dir, 200, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_MULTIHUED:
			{
				chance = rand_int(5);
				message_format(MSG_DSM, o_ptr->sval, "You breathe %s.",
				           ((chance == 1) ? "lightning" :
				            ((chance == 2) ? "frost" :
				             ((chance == 3) ? "acid" :
				              ((chance == 4) ? "poison gas" : "fire")))));
				fire_ball(((chance == 1) ? GF_ELEC :
				           ((chance == 2) ? GF_COLD :
				            ((chance == 3) ? GF_ACID :
				             ((chance == 4) ? GF_POIS : GF_FIRE)))),
				          dir, 250, 2);
				o_ptr->timeout = rand_int(225) + 225;
				break;
			}

			case SV_DRAGON_BRONZE:
			{
				message(MSG_DSM, o_ptr->sval, "You breathe confusion.");
				fire_ball(GF_CONFUSION, dir, 120, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_GOLD:
			{
				message(MSG_DSM, o_ptr->sval, "You breathe sound.");
				fire_ball(GF_SOUND, dir, 130, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_CHAOS:
			{
				chance = rand_int(2);
				message_format(MSG_DSM, o_ptr->sval, "You breathe %s.",
				           ((chance == 1 ? "chaos" : "disenchantment")));
				fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
				          dir, 220, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_LAW:
			{
				chance = rand_int(2);
				message_format(MSG_DSM, o_ptr->sval, "You breathe %s.",
				           ((chance == 1 ? "sound" : "shards")));
				fire_ball((chance == 1 ? GF_SOUND : GF_SHARD),
				          dir, 230, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_ETHEREAL:
			{
				chance = rand_int(3);
				message_format(MSG_DSM, o_ptr->sval, "You breathe %s.",
				           ((chance == 1) ? "light" :
				            ((chance == 2) ? "darknesst" : "confusion")));
				fire_ball((chance == 1) ? GF_LITE :
				           ((chance == 2) ? GF_DARK : GF_CONFUSION),
				          dir, 250, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_BALANCE:
			{
				chance = rand_int(4);
				message_format(MSG_DSM, o_ptr->sval, "You breathe %s.",
				           ((chance == 1) ? "chaos" :
				            ((chance == 2) ? "disenchantment" :
				             ((chance == 3) ? "sound" : "shards"))));
				fire_ball(((chance == 1) ? GF_CHAOS :
				           ((chance == 2) ? GF_DISENCHANT :
				            ((chance == 3) ? GF_SOUND : GF_SHARD))),
				          dir, 250, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_SHINING:
			{
				chance = rand_int(2);
				message_format(MSG_DSM, o_ptr->sval, "You breathe %s.",
				           ((chance == 0 ? "light" : "darkness")));
				fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 200, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_POWER:
			{
				message(MSG_DSM, o_ptr->sval, "You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}
		}

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
	int     item;
	cptr    q, s;

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
static bool item_tester_hook_use(object_type *o_ptr)
{
	u32b f1, f2, f3, f4;

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
					/* Extract the flags */
					object_flags(o_ptr, &f1, &f2, &f3, &f4);

					/* Check activation flag */
					if (f3 & TR3_ACTIVATE) return (TRUE);
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
	int         item;
	object_type *o_ptr;
	cptr        q, s;

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
			if (no_lite())
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

/* Mix two potions to (maybe) create a third */
void do_cmd_mix(void)
{
	int item1, item2, k_idx, penalty;
	int i1, i2, sv;
	int chance, roll;

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

	if (found)
	{
		/* Look for it */
		for (k_idx = 1; k_idx < z_info->k_max; k_idx++)
		{
			k_ptr = &k_info[k_idx];

			/* Found a match */
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == sv)) break;
		}
		
		if (k_idx == z_info->k_max) found = FALSE;
	}
	
	/* Check again if found */
	if (found)
	{
		chance = 25+(p_ptr->skill[SK_ALC])-((k_ptr->cost)/250);
		penalty = (k_ptr->cost)/800;

		/* Always 5% chance of success or failure*/

		if (chance < 5) chance = 5;

		if (chance > 96) chance = 96;
	}

	/* If there's no potion, always fail */
	else 
	{
		chance = 0;
		penalty = 0;
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

		/* Mark the potion */
		to_ptr->ident |= IDENT_MODIFIED;

		drop_near(to_ptr, -1, p_ptr->py, p_ptr->px); /* drop the object */
		potion_alch[sv].known1 = TRUE;
		potion_alch[sv].known2 = TRUE;
	}

	else if ((roll < chance+30) && (roll < 99)) message(MSG_FAIL, 0, "You have wasted the potions.");
	else 
	{
		message(MSG_FAIL, 0, "The potions explode in your hands!");
		take_hit(damroll(4,8)+penalty, "carelessly mixing potions");
	}

	/* Hack - make sure the potions are destroyed in order */

	i1= (item1>item2) ? item1 : item2;
	i2= (item1>item2) ? item2 : item1;

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
