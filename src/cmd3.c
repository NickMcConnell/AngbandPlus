/* File: cmd3.c */

/* Purpose: Inventory commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Display inventory
 */
void do_cmd_inven(void)
{
	char out_val[160];

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

	sprintf(out_val, "Inventory: carrying %d.%d pounds (%d%% of capacity). Command: ",
	    p_ptr->total_weight / 10, p_ptr->total_weight % 10,
	    (p_ptr->total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2));

	/* Get a command */
	prt(out_val, 0, 0);

	/* Get a new command */
	p_ptr->command_new = inkey();

	/* Load screen */
	screen_load();


	/* Process "Escape" */
	if (p_ptr->command_new == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new = 0;
	}

	/* Process normal keys */
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
	char out_val[160];

	/* Note that we are in "equipment" mode */
	p_ptr->command_wrk = (USE_EQUIP);

	/* Save the screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	show_equip();

	/* Hack -- undo the hack above */
	item_tester_full = FALSE;

	/* Build a prompt */
	sprintf(out_val, "Equipment: carrying %d.%d pounds (%d%% of capacity). Command: ",
	    p_ptr->total_weight / 10, p_ptr->total_weight % 10,
	    (p_ptr->total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2));

	/* Get a command */
	prt(out_val, 0, 0);

	/* Get a new command */
	p_ptr->command_new = inkey();

	/* Restore the screen */
	screen_load();


	/* Process "Escape" */
	if (p_ptr->command_new == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new = 0;
	}
	/* Process normal keys */
	else
	{
		/* Enter "display" mode */
		p_ptr->command_see = TRUE;
	}
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
	int item, slot;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	cptr act;

	char o_name[80];

	cptr q, s;

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

	/* Prevent wielding into a cursed slot */
	if (cursed_p(&inventory[slot]))
	{
		/* Describe it */
		object_desc(o_name, &inventory[slot], FALSE, 0);

		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
		           o_name, describe_use(slot));

		/* Cancel the command */
		return;
	}

	if (cursed_p(o_ptr) && confirm_wear &&
	    (object_known_p(o_ptr) || (o_ptr->ident & IDENT_SENSE)))
	{
		char dummy[512];

		/* Describe it */
		object_desc(o_name, o_ptr, FALSE, 0);

		sprintf(dummy, "Really use the %s {cursed}? ", o_name);

		if (!get_check(dummy))
			return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Get local object */
	q_ptr = &forge;

	/* Obtain local object */
	object_copy(q_ptr, o_ptr);

	/* Modify quantity */
	q_ptr->number = 1;

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

	/* Access the wield slot */
	o_ptr = &inventory[slot];

	/* Take off existing item */
	if (o_ptr->k_idx)
	{
		/* Take off existing item */
		(void)inven_takeoff(slot, 255);
	}

	/* Wear the new stuff */
	object_copy(o_ptr, q_ptr);

	/* Forget stack */
	o_ptr->next_o_idx = 0;

	/* Forget location */
	o_ptr->iy = o_ptr->ix = 0;

	/* Increase the weight */
	p_ptr->total_weight += q_ptr->weight;

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
	msg_format("%s %s (%c).", act, o_name, index_to_label(slot));

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		msg_print("Oops! It feels deathly cold!");

		chg_virtue(V_HARMONY, -1);

		/* Note the curse */
		o_ptr->ident |= (IDENT_SENSE);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	p_ptr->redraw |= (PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
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
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}


	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Take off the item */
	(void)inven_takeoff(item, 255);

	p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Drop an item
 */
void do_cmd_drop(void)
{
	int item, amt = 1;

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
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}


	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Drop (some of) the item */
	inven_drop(item, amt);

	p_ptr->redraw |= (PR_EQUIPPY);
}


static bool high_level_book(const object_type *o_ptr)
{
	if ((o_ptr->tval == TV_LIFE_BOOK) ||
	    (o_ptr->tval == TV_SORCERY_BOOK) ||
	    (o_ptr->tval == TV_NATURE_BOOK) ||
	    (o_ptr->tval == TV_CHAOS_BOOK) ||
	    (o_ptr->tval == TV_DEATH_BOOK) ||
	    (o_ptr->tval == TV_TRUMP_BOOK))
	{
		if (o_ptr->sval > 1)
			return TRUE;
		else
			return FALSE;
	}

	return FALSE;
}


bool destroy_item_aux(object_type *o_ptr, int amt)
{
	char o_name[80];
	
	bool gain_expr = FALSE;
	
	object_desc(o_name, o_ptr, TRUE, 3);
	
#ifdef USE_SCRIPT

	if (destroy_object_callback(o_ptr, amt)) return (FALSE);

#endif /* USE_SCRIPT */

	
	/* Can the player destroy the object? */
	if (!can_player_destroy_object(o_ptr))
	{
		/* Message */
		msg_format("You cannot destroy %s.", o_name);

		/* Done */
		return (FALSE);
	}
	
	/* Take a turn */
	p_ptr->energy_use += 100;
	
	/* Describe the object (with {terrible/special}) */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	msg_format("You destroy %s.", o_name);
	sound(SOUND_DESTITEM);

	if (high_level_book(o_ptr))
	{
		if (p_ptr->pclass == CLASS_WARRIOR)
		{
			gain_expr = TRUE;
		}
		else if (p_ptr->pclass == CLASS_PALADIN)
		{
			if (p_ptr->realm1 == REALM_LIFE)
			{
				if (o_ptr->tval != TV_LIFE_BOOK) gain_expr = TRUE;
			}
			else
			{
				if (o_ptr->tval == TV_LIFE_BOOK) gain_expr = TRUE;
			}
		}

		if (gain_expr && (p_ptr->exp < PY_MAX_EXP))
		{
			s32b tester_exp = p_ptr->max_exp / 20;
			if (tester_exp > 10000) tester_exp = 10000;
			if (o_ptr->sval < 3) tester_exp /= 4;
			if (tester_exp < 1) tester_exp = 1;

			msg_print("You feel more experienced.");
			gain_exp(tester_exp * amt);
		}

		if (high_level_book(o_ptr) && o_ptr->tval == TV_LIFE_BOOK)
		{
			chg_virtue(V_UNLIFE, 1);
			chg_virtue(V_VITALITY, -1);
		}
		else if (high_level_book(o_ptr) && o_ptr->tval == TV_DEATH_BOOK)
		{
			chg_virtue(V_UNLIFE, -1);
			chg_virtue(V_VITALITY, 1);
		}

		if (o_ptr->to_a || o_ptr->to_h || o_ptr->to_d)
			chg_virtue(V_ENCHANT, -1);

		if (object_value_real(o_ptr) > 30000)
			chg_virtue(V_SACRIFICE, 2);

		else if (object_value_real(o_ptr) > 10000)
			chg_virtue(V_SACRIFICE, 1);
	}

	if (o_ptr->to_a != 0 || o_ptr->to_d != 0 || o_ptr->to_h != 0)
		chg_virtue(V_HARMONY, 1);
	
	/* We destroyed the item(s) */
	return (TRUE);
}


/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
	int			item, amt = 1;
	int			old_number;

	bool		force = FALSE;

	object_type		*o_ptr;

	char		o_name[80];

	char		out_val[160];

	cptr q, s;

	/* Hack -- force destruction */
	if (p_ptr->command_arg > 0) force = TRUE;


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


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}


	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify unless quantity given */
	if (!force)
	{
		if (!(auto_destroy && (object_value(o_ptr) < 1)))
		{
			/* Make a verification */
			sprintf(out_val, "Really destroy %s? ", o_name);
			if (!get_check(out_val)) return;
		}
	}

	/* No energy used yet */
	p_ptr->energy_use = 0;

	/* Physically try to destroy the item(s) */
	if (!destroy_item_aux(o_ptr, amt)) return;

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
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{
	int			item;

	object_type		*o_ptr;

	char		o_name[80];

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


	/* Require full knowledge */
	if (!(o_ptr->ident & IDENT_MENTAL))
	{
		msg_print("You have no special knowledge about that item.");
		return;
	}


	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	msg_format("Examining %s...", o_name);

	/* Describe it fully */
	if (!identify_fully_aux(o_ptr)) msg_print("You see nothing special.");
}



/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(void)
{
	int   item;

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
	if (!o_ptr->inscription)
	{
		msg_print("That item had no inscription to remove.");
		return;
	}

	/* Message */
	msg_print("Inscription removed.");

	/* Remove the incription */
	o_ptr->inscription = 0;

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
	int			item;

	object_type		*o_ptr;

	char		o_name[80];

	char		out_val[80];

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
	msg_print(NULL);

	/* Start with nothing */
	strcpy(out_val, "");

	/* Use old inscription */
	if (o_ptr->inscription)
	{
		/* Start with the old inscription */
		strcpy(out_val, quark_str(o_ptr->inscription));
	}

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", out_val, 80))
	{
		/* Save the inscription */
		o_ptr->inscription = quark_add(out_val);

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

	/* Laterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_LANTERN) &&
	    (o_ptr->timeout > 0)) return (TRUE);

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

	/* Access the lantern */
	j_ptr = &inventory[INVEN_LITE];

	/* Refuel */
	if (o_ptr->tval == TV_FLASK)
	{
		/* Flasks use the pval to store the fuel */
		j_ptr->timeout += o_ptr->pval;
	}
	else
	{
		/* Lanterns use the timeout to store the fuel */ 
		j_ptr->timeout += o_ptr->timeout;
	}
	
	/* Message */
	msg_print("You fuel your lamp.");

	/* Comment */
	if (j_ptr->timeout >= FUEL_LAMP)
	{
		j_ptr->timeout = FUEL_LAMP;
		msg_print("Your lamp is full.");
	}

	/* Decrease the item (from the pack) */
	if (o_ptr->tval == TV_FLASK)
	{
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
	else
	{
		/* The lantern is empty */
		o_ptr->timeout = 0;
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
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

	/* Access the primary torch */
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
	if (target_set(TARGET_KILL))
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
	if (target_set(TARGET_LOOK))
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
	int wid, hgt;
	char tmp_val[80];
	char out_val[160];


	/* Get size */
	Term_get_size(&wid, &hgt);
	
	/* Offset */
	wid -= COL_MAP + 1;
	hgt -= ROW_MAP + 1;

	/* Start at current panel */
	y2 = y1 = panel_row_min;
	x2 = x1 = panel_col_min;

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
		        "Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
		        y2 / (hgt / 2), y2 % (hgt / 2),
		        x2 / (wid / 2), x2 % (wid / 2), tmp_val);

		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			char command;

			/* Get a command (or Cancel) */
			if (!get_com(out_val, &command)) break;

			/* Extract the action (if any) */
			dir = get_keymap_dir(command);

			/* Error */
			if (!dir) bell();
		}

		/* No direction */
		if (!dir) break;

		/* Apply the motion */
		if (change_panel(ddy[dir], ddx[dir]))
		{
			y2 = panel_row_min;
			x2 = panel_col_min;
		}
	}


	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

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
	"%:Trees",
	"&:A chest",
	"':An open door",
	"(:Soft armor",
	"):A shield",
	"*:A vein with treasure or a ball monster",
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
	"9:Entrance to the bookstore",
	"::Rubble / Rock",
	";:Swamp / Rune",
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
	"`:A figurine or statue",
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
	"l:Aquatic monster",
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
	"~:Fluid terrain (or miscellaneous item)",
	NULL
};



/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
bool ang_sort_comp_hook(const vptr u, const vptr v, int a, int b)
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
		z1 = r_info[w1].r_pkills;
		z2 = r_info[w2].r_pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = r_info[w1].r_tkills;
		z2 = r_info[w2].r_tkills;

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
void ang_sort_swap_hook(const vptr u, const vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	u16b holder;

	/* Hack - ignore v */
	(void) v;
	
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
	monster_race	*r_ptr = &r_info[r_idx];

	byte		a1, a2;
	char		c1, c2;


	/* Access the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Access the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;

	/* Hack -- fake monochrome */
	if (!use_color || ironman_moria) a1 = TERM_WHITE;
	if (!use_color || ironman_moria) a2 = TERM_WHITE;


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
 *   ^M (case insensitive name search)
 *
 * The responses may be sorted in several ways, see below.
 *
 * Note that the player ghosts are ignored. XXX XXX XXX
 */
void do_cmd_query_symbol(void)
{
	int		i, n, r_idx;
	char	sym, query;
	char	buf[128];

	bool	all = FALSE;
	bool	uniq = FALSE;
	bool	norm = FALSE;

	bool	recall = FALSE;

	u16b	why = 0;
	u16b	*who;

	char    temp1[80] = "\0";
	char    temp2[80] = "\0";

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
        else if (sym == KTRL('M'))
        {
                all = TRUE;
                if (!get_string("Name:",temp1, 70)) temp1[0] = 0;
                else sprintf(buf, "Monsters with a name \"%s\"", temp1);
        }
	else if (ident_info[i])
	{
		sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
	}
	else
	{
		sprintf(buf, "%c - %s.", sym, "Unknown Symbol");
	}

	/* Display the result */
	prt(buf, 0, 0);

	/* Allocate the "who" array */
	C_MAKE(who, max_r_idx, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights) continue;

		/* Require non-unique monsters if needed */
		if (norm && (r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Require unique monsters if needed */
		if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Collect monsters with a name temp1 */
                if (temp1[0])
		{
		  int xx;

		  for (xx=0; temp1[xx] && xx<80; xx++)
		  {
		      if (isupper(temp1[xx])) temp1[xx]=tolower(temp1[xx]);
		  }
		  strcpy(temp2, r_name + r_ptr->name);

		  for (xx=0; temp2[xx] && xx<80; xx++)
		  {
                      if (isupper(temp2[xx])) temp2[xx]=tolower(temp2[xx]);
		  }
		  
		  if (strstr(temp2, temp1)) who[n++]=i;
                }

		/* Collect "appropriate" monsters */
		else if (all || (r_ptr->d_char == sym)) who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* XXX XXX Free the "who" array */
		C_KILL(who, max_r_idx, u16b);

		return;
	}

	/* Prompt XXX XXX XXX */
	put_str("Recall details? (k/y/n): ", 0, 40);

	/* Query */
	query = inkey();

	/* Restore */
	prt(buf, 0, 0);

	why = 2;

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array */
	ang_sort(who, &why, n);

	/* Sort by kills (and level) */
	if (query == 'k')
	{
		why = 4;
		query = 'y';
	}

	/* Catch "escape" */
	if (query != 'y')
	{
		/* XXX XXX Free the "who" array */
		C_KILL(who, max_r_idx, u16b);

		return;
	}


	/* Sort if needed */
	if (why == 4)
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
				/* Save the screen */
				screen_save();

				/* Recall on screen */
				screen_roff(who[i], 0);

				/* Hack -- Complete the prompt (again) */
				Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
			}

			/* Command */
			query = inkey();

			/* Unrecall */
			if (recall)
			{
				/* Restore */
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

	/* Free the "who" array */
	C_KILL(who, max_r_idx, u16b);

	/* Re-display the identity */
	prt(buf, 0, 0);
}


/*
 *  research_mon
 *  -KMW-
 */
bool research_mon(void)
{
	int i, n, r_idx;
	char sym, query;
	char buf[128];

	s16b oldkills;
	byte oldwake;
	bool oldcheat;

	bool notpicked;

	bool recall = FALSE;

	u16b why = 0;

	monster_race *r2_ptr;

	u16b	*who;

	oldcheat = cheat_know;


	/* Save the screen */
	screen_save();

	/* Get a character, or abort */
	if (!get_com("Enter character of monster: ", &sym))
	{
		/* Restore */
		screen_load();

		return (FALSE);
	}

	/* Allocate the "who" array */
	C_MAKE(who, max_r_idx, u16b);

	/* Find that character info, and describe it */
	for (i = 0; ident_info[i]; ++i)
	{
		if (sym == ident_info[i][0]) break;
	}

	if (ident_info[i])
	{
		sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
	}
	else
	{
		sprintf(buf, "%c - %s.", sym, "Unknown Symbol");
	}

	/* Display the result */
	prt(buf, 16, 10);


	/* Collect matching monsters */
	for (n = 0, i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		cheat_know = TRUE;

		/* Collect "appropriate" monsters */
		if (r_ptr->d_char == sym) who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		cheat_know = oldcheat;

		/* Free the "who" array */
		C_KILL(who, max_r_idx, u16b);

		/* Restore */
		screen_load();

		return (FALSE);
	}

	/* Sort by level */
	why = 2;
	query = 'y';

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

	notpicked = TRUE;

	/* Scan the monster memory */
	while (notpicked)
	{
		/* Extract a race */
		r_idx = who[i];

		/* Save this monster ID */
		p_ptr->monster_race_idx = r_idx;

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Hack -- Begin the prompt */
		roff_top(r_idx);

		/* Hack -- Complete the prompt */
		Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC, space to continue]");

		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Recall on screen */
				r2_ptr = &r_info[r_idx];

				oldkills = r2_ptr->r_tkills;
				oldwake = r2_ptr->r_wake;
				screen_roff(who[i], 1);
				r2_ptr->r_tkills = oldkills;
				r2_ptr->r_wake = oldwake;
				cheat_know = oldcheat;
				notpicked = FALSE;
			}

			/* Command */
			query = inkey();

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
	/* prt(buf, 5, 5);*/

	cheat_know = oldcheat;

	/* Free the "who" array */
	C_KILL(who, max_r_idx, u16b);

	/* Restore */
	screen_load();

	return (!notpicked);
}

