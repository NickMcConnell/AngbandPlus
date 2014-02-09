/* File: cmd3.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
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
#include "game-event.h"

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
	show_inven(OLIST_WEIGHT | OLIST_QUIVER);

	/* Hack -- hide empty slots */
	item_tester_full = FALSE;

	/* Prompt for a command */
	prt(format("(Inventory) Burden %d.%dlb (%d%% capacity). Command: ",
	    p_ptr->total_weight / 10, p_ptr->total_weight % 10,
	    (10 * p_ptr->total_weight) / (6 * adj_str_wgt[p_ptr->state.stat_ind[A_STR]])), 0, 0);

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
 * Display equipment
 */
void do_cmd_equip(void)
{
	/* Hack -- Start in "equipment" mode */
	p_ptr->command_wrk = (USE_EQUIP | USE_QUIVER);

	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	show_equip(OLIST_WEIGHT);

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
}

static int quiver_wield(int item, object_type *o_ptr)
{
	int slot = 0;
	int num;
	char o_name[80];

	object_type object_type_body;
	object_type *i_ptr;

	/* was: num = get_quantity(NULL, o_ptr->number);*/
	num = o_ptr->number;

	/* Cancel */
	if (num <= 0) return 0;

	/* Check free space */
	if (!quiver_carry_okay(o_ptr, num, item))
	{
		msg_print("Your quiver needs more space.");
		return 0;
 	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy to the local object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = num;

	/* No longer in use */
	i_ptr->obj_in_use = FALSE;

	/* Describe the result */
	object_desc(o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

	/*Mark it to go in the quiver */
	o_ptr->ident |= (IDENT_QUIVER);

	slot = wield_slot_ammo(o_ptr);

	/* Handle errors (paranoia) */
	if (slot == QUIVER_END)
	{
		/* No space. */
		msg_print("Your quiver is full.");
		return 0;
	}

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

	/* Get the wield slot */
	o_ptr = &inventory[slot];

	/* Describe the result */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Take off existing item */
	if (o_ptr->k_idx)
	{
		/* Take off existing item */
		o_ptr->number += i_ptr->number;
	}

	/* Wear the new stuff */
	else object_copy(o_ptr, i_ptr);

	/* Describe the result */
	object_desc(o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Message */
	sound(MSG_WIELD);
	msg_format("You have readied %s (%c).", o_name, index_to_label(slot));

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		sound(MSG_CURSED);
		msg_print("Oops! It feels deathly cold!");

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

		/* Sense the object if allowed */
		if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_CURSED;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);
	}

	slot = sort_quiver(slot);

	/* Save quiver size */
	save_quiver_size();

	/* See if we have to overflow the pack */
	pack_overflow();

	/* Recalculate bonuses */
	p_ptr->notice |= (PN_SORT_QUIVER);
	p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA | PU_NATIVE);
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);

	/* Reorder the quiver and return the perhaps modified slot */
	return (slot);
}


/*
 * Wield or wear a single item from the pack or floor
 */
void wield_item(object_type *o_ptr, int item, int slot)
{
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;

	cptr fmt;
	char o_name[80];

	/* Describe the result */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Ammo goes in quiver slots, which have special rules. */
	if (IS_QUIVER_SLOT(slot))
	{
		/* Put the ammo in the quiver */
		slot = quiver_wield(item, o_ptr);

		/* Can't do it */
		return;
	}

	/* Hack - Don't wield mimic objects */
	if (o_ptr->mimic_r_idx) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = 1;

	/* No longer in use */
	i_ptr->obj_in_use = FALSE;

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

	/* Increment the equip counter by hand */
	p_ptr->equip_cnt++;

	/* Autoinscribe if the object was on the floor */
	if (item < 0) apply_autoinscription(o_ptr);

	/* Describe the result */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Where is the item now */
	if ((slot == INVEN_WIELD) || (slot == INVEN_BOW))
	{
		if (obj_is_bow(o_ptr)) fmt = "You are shooting with %s (%c).";
		else fmt = "You are wielding %s (%c).";

	}
	else if (slot == INVEN_LIGHT)
		fmt = "Your light source is %s (%c).";

	else
		fmt = "You are wearing %s (%c).";

	/* Message */
	sound(MSG_WIELD);
	msg_format(fmt, o_name, index_to_label(slot));

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		sound(MSG_CURSED);
		msg_print("Oops! It feels deathly cold!");

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

		/* Sense the object if allowed */
		if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_CURSED;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);
	}

	/* Save quiver size */
	save_quiver_size();

	/* See if we have to overflow the pack */
	pack_overflow();

	/* Recalculate bonuses, torch, mana */
	p_ptr->notice |= (PN_SORT_QUIVER);
	p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA | PU_NATIVE);
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);
}


/*
 * Hook to determine if an object is activatable
 */
bool item_tester_hook_activate(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}


/*
 * Handle the user command to destroy an item
 */
void do_cmd_destroy(cmd_code code, cmd_arg args[])
{
	int item = args[0].item;

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

	/* Can't destroy cursed items we're wielding. */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		msg_print("You cannot destroy the cursed item.");
		return;
	}

	/* Cursed quiver */
	else if (IS_QUIVER_SLOT(item) && p_ptr->state.cursed_quiver)
	{
		/* Oops */
		msg_print("Your quiver is cursed!");

		/* Nope */
		return;
	}

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Describe the object */
	old_number = o_ptr->number;

	/*Hack, state the correct number of charges to be destroyed if wand, rod or staff*/
	if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF) || o_ptr->tval == TV_ROD)
	    && (amt < o_ptr->number))
	{
		/*save the number of charges*/
		old_charges = o_ptr->pval;

		/*distribute the charges*/
		o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;

		o_ptr->pval = old_charges - o_ptr->pval;
	}

	/*hack -  make sure we get the right amount displayed*/
	o_ptr->number = amt;

	/*now describe with correct amount*/
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/*reverse the hack*/
	o_ptr->number = old_number;

	/* Verify destruction */
	if (verify_destroy)
	{
		int result;
		char other;
		char explain[80];
		cptr other_text = "[SQUELCH_EGO]";

		/* Check for known ego-items */
		strnfmt(out_val, sizeof(out_val), "Really Destroy %s", o_name);

		/* Check for known ego-items */
		if (ego_item_p(o_ptr) && object_known_p(o_ptr))
		{
			/* get the ego item type */
			ego_item_type *e_ptr = &e_info[o_ptr->ego_num];

			other = 'e';

			/* Format the prompt */
			strnfmt(explain, sizeof(explain), "(%c to always squelch ego item type %s)? ", other, e_name + e_ptr->name);

			result = get_check_other(out_val, other_text, other, explain);

			/* returned "no"*/
			if (!result) return;

			/*return of 2 sets ego item type to squelch*/
			else if (result == 2)
			{
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

			object_kind *k_ptr = &k_info[o_ptr->k_idx];
			char i_name[80];

			/*make a fake object so we can give a proper message*/
			object_type *i_ptr;
			object_type object_type_body;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Create the object */
			object_prep(i_ptr, o_ptr->k_idx);

			/*now describe with correct amount*/
			object_desc(i_name, sizeof(i_name), i_ptr, ODESC_PLURAL | ODESC_FULL);

			other = 's';
			other_text = "[SQUELCH_TYPE]";

			/* Format the prompt */
			strnfmt(explain, sizeof(explain), "(%c to always squelch %s)?", other, i_name);

			result = get_check_other(out_val, other_text, other, explain);

			/* returned "no"*/
			if (!result) return;

			/*return of 2 sets item to squelch*/
			else if (result == 2)
			{
				/*set to squelch*/
				k_ptr->squelch = SQUELCH_ALWAYS;

				/* Message - no good routine for extracting the plain name*/
				msg_format("All %^s are set to always be squelched.", i_name);

				/*Mark the view to be updated*/
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW);
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
			if (cursed_p(o_ptr) || broken_p(o_ptr))
				o_ptr->discount = INSCRIP_TERRIBLE;
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

		p_ptr->update |= (PU_NATIVE);

		p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_RESIST | PR_EXP |
						  PR_STATS | PU_NATIVE | PR_ITEMLIST);

		/* Done */
		return;
	}

	/* Message */
	message_format(MSG_DESTROY, 0, "You destroy %s.", o_name);

	/*hack, restore the proper number of charges after the messages have printed
	 * so the proper number of charges are destroyed*/
	if (old_charges) o_ptr->pval = old_charges;

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


void textui_cmd_destroy(void)
{
	int item;

	cptr q, s;

	/* Get an item */
	q = "Destroy which item? ";
	s = "You have nothing to destroy.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR | USE_QUIVER))) return;

	destroy_item(item);
}


static void refill_lamp(object_type *j_ptr, object_type *o_ptr, int item)
{

	/* Refuel from a lantern */
	if (o_ptr->sval == SV_LIGHT_LANTERN)
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

	/* Refilled from a lantern */
	if (o_ptr->sval == SV_LIGHT_LANTERN)
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

			/* Carry or drop */
			if (item >= 0)
				item = inven_carry(i_ptr);
			else
				drop_near(i_ptr, 0, p_ptr->py, p_ptr->px);
		}

		/* Empty a single lantern */
		else
		{
			/* No more fuel */
			o_ptr->timeout = 0;
		}

		/* Combine / Reorder the pack (later) */
		p_ptr->update |= (PU_BONUS);
		p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_INVEN | PR_ITEMLIST);
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

	/* Redraw stuff */
	p_ptr->redraw |= (PR_EQUIP);
}


static void refuel_torch(object_type *j_ptr, object_type *o_ptr, int item)
{
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

	/* Redraw stuff */
	p_ptr->redraw |= (PR_EQUIP);
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
	j_ptr = &inventory[INVEN_LIGHT];

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
	p_ptr->redraw |= (PR_EQUIP);

	/* Success */
	return (TRUE);
}


/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(cmd_code code, cmd_arg args[])
{
	object_type *j_ptr = &inventory[INVEN_LIGHT];

	int item = args[0].item;
	object_type *o_ptr = object_from_item_idx(item);

	if (!item_is_available(item, NULL, USE_INVEN | USE_FLOOR))
	{
		msg_print("You do not have that item to refill with it.");
		return;
	}

	/* It is nothing */
	if (j_ptr->tval != TV_LIGHT)
	{
		msg_print("You are not wielding a light.");
		return;
	}

	/* It's a lamp */
	else if (j_ptr->sval == SV_LIGHT_LANTERN)
	{
		/* First we check if the lamp can be filled from the oil on the floor */
		if (!do_cmd_refill_lamp_from_terrain())
		{
			refill_lamp(j_ptr, o_ptr, item);
		}
	}

	/* It's a torch */
	else if (j_ptr->sval == SV_LIGHT_TORCH)
		refuel_torch(j_ptr, o_ptr, item);


	p_ptr->p_energy_use = BASE_ENERGY_MOVE / 2;

}


/*
 * Target command
 */
void do_cmd_target(void)
{
	/* Target set */
	if (target_set_interactive(TARGET_KILL, -1, -1))
	{
		msg_print("Target Selected.");
		p_ptr->redraw |= (PR_MONLIST);
		redraw_stuff();
	}

	/* Target aborted */
	else
	{
		msg_print("Target Aborted.");
	}
}

void do_cmd_target_closest(void)
{
	target_set_closest(TARGET_KILL);
}


/*
 * Look command
 */
void do_cmd_look(void)
{
	/* Look around */
	if (target_set_interactive(TARGET_LOOK, -1, -1))
	{
		msg_print("Target Selected.");
		p_ptr->redraw |= (PR_MONLIST);
		redraw_stuff();
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
static cptr ident_info_nppmoria[] =
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
	"::Rubble",
	";:A glyph of warding or glacier",
	"<:An up staircase",
	"=:A ring",
	">:A down staircase",
	"?:A scroll",
	"@:You",
	"A:Ant Lion",
	"B:Balrog",
	"C:Gelatinous Cube",
	"D:Ancient Dragon (Beware)",
	"E:Elemental/Spirit",
	"F:Fly/Dragon Fly/Insect",
	"G:Ghost",
	"H:Hobgoblin",
	/* "I:unused", */
	"J:Jelly",
	"K:Killer Beetle",
	"L:Lich",
	"M:Mummy",
	/* "N:unused", */
	"O:Ooze",
	"P:Giant",
	"Q:Quylthulg (Pulsing Flesh Mound)",
	"R:Snake",
	"S:Scorpions",
	"T:Troll",
	"U:Umber Hulk",
	"V:Vampire",
	"W:Wight/Wraith",
	"X:Xorn",
	"Y:Yeti",
	/* "Z:unused", */
	"[:Hard armor",
	"\\:A hafted weapon (mace/whip/etc)",
	"]:Misc. armor",
	"^:A trap",
	"_:A staff",
	/* "`:unused", */
	"a:Ant",
	"b:Bat",
	"c:Centipede",
	"d:Dragon",
	"e:Floating Eye",
	"f:Frogs",
	"g:Golem",
	"h:harpy",
	"i:Icky Thing",
	"j:Jackal",
	"k:Kobold",
	"l:Louse",
	"m:Mold",
	"n:Naga",
	"o:Orc",
	"p:Person/Humanoid",
	"q:Quasit",
	"r:Rodent",
	"s:Skeleton",
	"t:Tick",
	/* "u:unused", */
	/* "v:unused", */
	"w:Worm/Worm-Mass",
	/* "x:unused", */
	"y:Yeek",
	"z:Zombie",
	"{:A missile (arrow/bolt/shot)",
	"|:An edged weapon (sword/dagger/etc)",
	"}:A launcher (bow/crossbow/sling)",
	"~:A chest (or miscellaneous item)",
	NULL
};




/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static cptr ident_info_nppangband[] =
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
	"0:Entrance to the Book Shop",
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
	"_:A staff",
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
 */
void do_cmd_query_symbol(void)
{
	int i, n, r_idx;
	char sym;
	char buf[128];

	ui_event_data query;

	bool all = FALSE;
	bool uniq = FALSE;
	bool norm = FALSE;

	bool recall = FALSE;

	u16b why = 0;
	u16b *who;

	button_backup_all();
	button_kill_all();
	button_add("ALL MONSTERS|", KTRL('A'));
	button_add("ALL UNIQUES|", KTRL('U'));
	button_add("NON-UNIQUES|", KTRL('N'));

	/* Don't collide with the repeat button */
	button_add("QUIT", 'q');
	event_signal(EVENT_MOUSEBUTTONS);

	/* Get a character, or abort */
	if (!get_com("Enter character to be identified, or control+[ANU]: ", &sym)) return;

	if (game_mode == GAME_NPPMORIA)
	{
		/* Find that character info, and describe it */
		for (i = 0; ident_info_nppmoria[i]; ++i)
		{
			if (sym == ident_info_nppmoria[i][0]) break;
		}
	}

	/* Find that character info, and describe it */
	else for (i = 0; ident_info_nppangband[i]; ++i)
	{
		if (sym == ident_info_nppangband[i][0]) break;
	}

	/* Describe */
	if (sym == KTRL('A'))
	{
		all = TRUE;
		my_strcpy(buf, "Full monster list.", sizeof(buf));
	}
	else if (sym == KTRL('U'))
	{
		all = uniq = TRUE;
		my_strcpy(buf, "Unique monster list.", sizeof(buf));
	}
	else if (sym == KTRL('N'))
	{
		all = norm = TRUE;
		my_strcpy(buf, "Non-unique monster list.", sizeof(buf));
	}
	else if ((game_mode == GAME_NPPMORIA) ? ident_info_nppmoria[i] : ident_info_nppangband[i])
	{
		strnfmt(buf, sizeof(buf), "%c - %s.", sym, (game_mode == GAME_NPPMORIA) ? ident_info_nppmoria[i] + 2 : ident_info_nppangband[i] + 2);
	}
	else
	{
		strnfmt(buf, sizeof(buf), "%c - %s.", sym, "Unknown Symbol");
	}

	/* Display the result */
	prt(buf, 0, 0);


	/* Allocate the "who" array */
	who = C_ZNEW(z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Collect matching monsters */
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

	/* Buttons */
	button_kill_all();
	button_add("SORT_LEVELS|", 'y');
	button_add("SORT-KILLS|", 'k');
	/* Don't collide with the repeat button */
	button_add("QUIT", 'q');
	event_signal(EVENT_MOUSEBUTTONS);

	/* Prompt */
	put_str("Recall details? (y/k/n): ", 0, 40);

	/* Query */
	query = inkey_ex();

	/* Restore */
	prt(buf, 0, 0);

	/* Interpret the response */
	if (query.key == 'k')
	{
		/* Sort by kills (and level) */
		why = 4;
	}
	else if (query.key == 'y' || query.key == 'p')
	{
		/* Sort by level; accept 'p' as legacy */
		why = 2;
	}
	else
	{
		/* Any unsupported response is "nope, no history please" */

		/* XXX XXX Free the "who" array */
		FREE(who);

		return;
	}


	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array */
	ang_sort(who, &why, n);

	/* Start at the end */
	i = n - 1;

	/* Button */
	button_backup_all();
	button_kill_all();
	button_add("ESC|", ESCAPE);
	button_add("RECALL|", 'r');
	button_add("PREV|", '-');
	button_add("NEXT", '+');
	event_signal(EVENT_MOUSEBUTTONS);

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
			query = inkey_ex();

			/* Unrecall */
			if (recall)
			{
				/* Load screen */
				screen_load();
			}

			/* Normal commands */
			if (query.key != 'r') break;

			/* Toggle recall */
			recall = !recall;
		}

		/* Stop scanning */
		if (query.key == ESCAPE) break;

		/* Move to "prev" monster */
		if (query.key == '-')
		{
			if (++i == n)
				i = 0;
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
				i = n - 1;
		}
	}

	/* Button */
	button_kill_all();
	button_restore();
	event_signal(EVENT_MOUSEBUTTONS);

	/* Clear the top line */
	prt("", 0, 0);

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
	bool did_steal = FALSE;
	int filching_power = 0;
	int counter = 0;
	int counter2 = 0;

	bool thief = FALSE;

	/*before we go through everything else,
	 *sometimes monsters aren't carry anything*/
	if (r_ptr->flags1 & (RF1_DROP_60))
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
	filching_power = (2 * p_ptr->lev) +  (p_ptr->state.skills[SKILL_DISARM] / 2);

	/*dex bonus counts extra*/
	filching_power += (adj_dex_dis[p_ptr->state.stat_ind[A_DEX]]* 3);

	/*easier on a sleeping monster*/
	if (m_ptr->m_timed[MON_TMD_SLEEP])
	{
		/*easier with better stealth*/
		filching_power += (p_ptr->state.skills[SKILL_STEALTH] * 2);

		/*extra credit for extreme stealth*/
		if (p_ptr->state.skills[SKILL_STEALTH] > 10) filching_power += ((p_ptr->state.skills[SKILL_STEALTH] - 10) * 3);
	}

	/* Penalize some conditions */
	if (p_ptr->timed[TMD_BLIND] || no_light()) filching_power = filching_power / 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) filching_power = filching_power / 10;

	/* Determine how much protection the monster has. */
	theft_protection = ((r_ptr->level * 2) + 35);

	/*smarter monsters are harder to steal from*/
	if (r_ptr->flags2 & (RF2_SMART))
	{
		theft_protection *= 3;
		theft_protection /= 2;
	}

	/*sleeping, stunned, or confused monsters are easier to steal from*/
	if (m_ptr->m_timed[MON_TMD_SLEEP]) theft_protection /= 3;
	if (m_ptr->m_timed[MON_TMD_CONF]) theft_protection /= 3;
	if (m_ptr->m_timed[MON_TMD_STUN]) theft_protection /= 5;

	/* now adjust for speed*/
	theft_protection += (m_ptr->m_speed - p_ptr->state.p_speed) * (game_mode == GAME_NPPMORIA ? 10 : 1);

	/*enforce a minimum - should almost never be necessary*/
	if (theft_protection < 1) theft_protection = 1;

	/* Send a thief to catch a thief. */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
	{
		/* Extract information about the blow effect */
		effect = r_ptr->blow[i].effect;
		if (effect == RBE_EAT_GOLD) thief = TRUE;
		else if (effect == RBE_EAT_ITEM) thief = TRUE;
	}
	if (thief) theft_protection += 30;

	/*creatures who have guaranteed good or great times are very protective
	 *creatures who hold chests are INCREDIBLY protective -JG
	 */
	if (r_ptr->flags1 & (RF1_DROP_CHEST)) theft_protection *= 2;
	else if (r_ptr->flags1 & (RF1_DROP_GREAT)) theft_protection = (theft_protection * 5 / 3);
	else if (r_ptr->flags1 & (RF1_DROP_GOOD)) theft_protection = (theft_protection * 3 / 2);

	if (teststeal)
	{
		/* Did the theft succeed, give object to player.  */
		if (randint(filching_power) >= theft_protection)
		{
			steal_object_from_monster(y, x);

			/*lower theft protection to 90% of orig value for next check*/
			theft_protection *= 9; theft_protection /= 10;

			did_steal = TRUE;
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
	if ((!did_steal) && (randint(filching_power) < (theft_protection / 2)))
	{
		wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);

		/* Give the player a message. */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);
		msg_format("You have irritated %s.", m_name);

		/*add to the haste counter and give message*/
		mon_inc_timed(cave_m_idx[m_ptr->fy][m_ptr->fx], MON_TMD_FAST,
				(50 + rand_int(50)), MON_TMD_FLG_NOTIFY);

		/* Update the monster health bar*/
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
	if ((!m_ptr->m_timed[MON_TMD_SLEEP]) || (counter2 > counter))
	{
		/* Aggravate monsters nearby. */
		mass_aggravate_monsters(SOURCE_PLAYER);

		/*dungeon gets different treatment than town*/
		if (p_ptr->depth) msg_print("The Pits of Angband roar with anger!");
		else msg_print("The furious townspeople search for the notorious burglar!");
	}

	/* The thief also speeds up, but only for just long enough to escape. */
	if (!p_ptr->timed[TMD_FAST]) inc_timed(TMD_FAST, 2, TRUE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);
	p_ptr->redraw |= (PR_HEALTH);

	/* Handle stuff */
	handle_stuff();

}


/*
 * create a monster trap, currently used from a scroll
 */
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

	if (p_ptr->timed[TMD_BLIND] || no_light())
	{
		msg_print("You can not see to set a trap.");
		return;
	}

	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE])
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

	light_spot(y, x);

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

		/* Allow user to exit the function */
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

	if (p_ptr->timed[TMD_BLIND] || no_light())
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
	light_spot(y,x);

	/* Notify the player. */
	msg_print("You modify the monster trap.");

	/*make the monsters who saw wary*/
	(void)project_los(y, x, 0, GF_MAKE_WARY);

	return (TRUE);
}


/*
 * Centers the map on the player
 */
void do_cmd_center_map(void)
{
	center_panel();
}
