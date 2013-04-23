#define CMD3_C
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
 * Display inventory/equipment and wait for a command.
 */
void do_cmd_inven(bool equip)
{
	char out_val[160];
	cptr title = (equip) ? "Equipment" : "Inventory";


	/* Store the mode. */
	command_wrk = equip;


	/* Save the screen */
	Term_save();

	/* Display the inventory */
	show_inven(equip, TRUE);

	/* Build a prompt */
	sprintf(out_val, "%s: carrying %d.%d pounds (%d%% of capacity). Command: ",
		title, total_weight / 10, total_weight % 10,
		(total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2));

	/* Get a command */
	prt(out_val, 0, 0);

	/* Get a new command */
	command_new = inkey();

	/* Restore the screen */
	Term_load();


	/* Process "Escape" */
	if (command_new == ESCAPE)
	{
		/* Reset stuff */
		command_new = 0;
		command_gap = 50;
	}

	/* Process normal keys */
	else
	{
		/* Hack -- Use "display" mode */
		command_see = TRUE;
	}
}

/*
 * The "wearable" tester
 */
bool PURE item_tester_hook_wear(object_ctype *o_ptr)
{
	int i = wield_slot(o_ptr);

	/* Check for a usable slot */
	return (i >= INVEN_WIELD && i < INVEN_TOTAL);
}

/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(object_type *o_ptr)
{
	object_type forge;
	object_type *q_ptr, *j_ptr;

	cptr act;

	/* This should only be called with valid items. */
	assert(item_tester_hook_wear(o_ptr));

	/* Check the slot */
	j_ptr = inventory + wield_slot(o_ptr);

	/* Prevent wielding into a cursed slot */
	if (cursed_p(j_ptr))
	{
		/* Message */
		msg_format("The %v you are %s appears to be cursed.", object_desc_f3,
			j_ptr, FALSE, 0, describe_use(j_ptr));

		/* Cancel the command */
		return;
	}



	if ((cursed_p(o_ptr)) && (wear_confirm)
		&& (object_known_p(o_ptr) || (o_ptr->ident & (IDENT_SENSE_CURSED))))
	{
		if (!(get_check(format("Really use the %v {cursed}? ",
			object_desc_f3, o_ptr, FALSE, 0))))
			return;
	}
	/* confirm_wear_all is triggered whenever something may be cursed.
	 * Slots are excluded because items to be placed in them are always
	 * created uncursed. */
	else if (confirm_wear_all && ~o_ptr->ident & IDENT_SENSE_CURSED &&
		wield_slot(o_ptr) >= INVEN_WIELD && wield_slot(o_ptr) <= INVEN_FEET)
	{
		if (!(get_check(format("Really use the %v? ",
			object_desc_f3, o_ptr, FALSE, 3))))
			return;
	}

	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Get local object */
	q_ptr = &forge;

	/* Obtain local object */
	object_copy(q_ptr, o_ptr);

	/* Modify quantity */
	q_ptr->number = 1;

	/* Decrease the item. */
	item_increase(o_ptr, -1);
	item_optimize(o_ptr);

	/* Access the wield slot */
	o_ptr = j_ptr;

	/* Take off existing item */
	if (o_ptr->k_idx)
	{
		/* Take off existing item */
		(void)inven_takeoff(o_ptr, 255);
	}

	/* Wear the new stuff */
	object_copy(o_ptr, q_ptr);

	/* Increase the weight */
	total_weight += q_ptr->weight;

	/* Where is the item now */
	switch (j_ptr - inventory)
	{
		case INVEN_WIELD: act = "You are wielding"; break;
		case INVEN_BOW: act = "You are shooting with"; break;
		case INVEN_LITE: act = "Your light source is"; break;
		case INVEN_POUCH_1: case INVEN_POUCH_2: case INVEN_POUCH_3:
		case INVEN_POUCH_4: case INVEN_POUCH_5: case INVEN_POUCH_6:
			act = "You have readied"; break;
		default: act = "You are wearing";
	}

	/* Message */
	msg_format("%s %v (%c).", act,
		object_desc_f3, o_ptr, TRUE, 3, index_to_label(o_ptr));

	/* Auto-curse */
	{
		u32b f1, f2, f3;
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & TR3_AUTO_CURSE)
	{
			curse(o_ptr);
		}
	}

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		msg_print("Oops! It feels deathly cold!");

		/* Make a note of it (only useful for rings and amulets). */
		k_info[o_ptr->k_idx].tried = TRUE;
	}

	/* Display the object if required */
	object_track(o_ptr);

	/* Note whether it is cursed or not. */
	o_ptr->ident |= (IDENT_SENSE_CURSED);

	/* Note that it has been tried. */
	o_ptr->ident |= (IDENT_TRIED);

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
void do_cmd_takeoff(object_type *o_ptr)
{
	/* Item is cursed */
	if (cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}


	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Take off the item */
	(void)inven_takeoff(o_ptr, 255);

	p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Drop an item
 */
void do_cmd_drop(object_type *o_ptr)
{
	int  amt = 1;

	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number,TRUE);

		/* Allow user abort */
		if (amt <= 0) return;
	}


	/* Take a partial turn */
	energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

	/* Drop (some of) the item */
	inven_drop(o_ptr, amt);

	p_ptr->redraw |= (PR_EQUIPPY);
}

/*
 * Destroy an item
 * Return a POWER_ERROR if no transformation occurred, set *name and *value
 * and return SUCCESS otherwise.
 * verb and dative are used to construct various sentences below.
 * dative0 replaces dative in the final statement if the object was worthless.
 * *value is used to store the value of the object(s) destroyed.
 * q_ptr is a pointer to an object which is used to store the object destroyed
 * on successful completion. It is junk otherwise.
 */
errr do_cmd_destroy_aux(cptr verb, cptr dative, object_type *q_ptr, object_type *o_ptr)
{
	/* Hack -- force destruction */
	bool force = (command_arg > 0);

	/* Get an object with the number to destroy. */
	object_copy(q_ptr, o_ptr);

	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		q_ptr->number = get_quantity(NULL, o_ptr->number,TRUE);

		/* Allow user abort */
		if (q_ptr->number <= 0) return POWER_ERROR_ABORT;
	}


	/* Verify unless quantity given */
	if (!force)
	{
		if (!((auto_destroy) && (object_value(o_ptr, FALSE)<1)))
		{
			/* Make a verification */
			if (!get_check(format("Really %s %v%s? ", verb,
				object_desc_f3, q_ptr, TRUE, 3, dative)))
				return POWER_ERROR_ABORT;
		}
	}

	/* Artifacts cannot be destroyed */
	if (allart_p(o_ptr))
	{
		/* Message */
		msg_format("You cannot %s %v%s.",
			verb, object_desc_f3, q_ptr, TRUE, 3, dative);

		/* We know how valuable it might be. */
		o_ptr->ident |= (IDENT_SENSE_VALUE);

		/* We know that it's extraordinary. */
		o_ptr->ident |= (IDENT_SENSE_HEAVY);

		/* Recalculate/redraw stuff (later) */
		update_object(o_ptr);

		/* Done */
		return POWER_ERROR_FAIL;
	}

	/* Eliminate the item */
	item_increase(o_ptr, -q_ptr->number);
	item_describe(o_ptr);
	item_optimize(o_ptr);

	return SUCCESS;
}

/*
 * Destroy an item
 */
void do_cmd_destroy(object_type *o_ptr)
{
	object_type q_ptr[1];

	errr err = do_cmd_destroy_aux("destroy", "", q_ptr, o_ptr);

	if (err != POWER_ERROR_ABORT) energy_use = extract_energy[p_ptr->pspeed];

	if (!err)
		msg_format("You have destroyed %v.", object_desc_f3, q_ptr, TRUE, 3);
}


/*
 * Hide an object stack on the floor, and ask if its tval should be hidden.
 *
 * Only floor items are considered, as the player is assumed not to
 */
void do_cmd_hide_object(object_type *o_ptr)
{
	msg_format("You hide %v.", object_desc_f3, o_ptr, TRUE, 3);

	/* Hide the object. */
	object_hide(o_ptr);

	/* Stop tracking the object. */
	object_track(NULL);
}

/*
 * Reveal all hidden objects on the current level.
 */
void do_cmd_unhide_objects(void)
{
	object_type *o_ptr;
	int t = 0;

	for (o_ptr = o_list; o_ptr < o_list+MAX_O_IDX; o_ptr++)
	{
		if (hidden_p(o_ptr)) t++;
		o_ptr->ident &= ~(IDENT_HIDDEN);
	}

	for (o_ptr = inventory; o_ptr < inventory+INVEN_TOTAL; o_ptr++)
	{
		if (hidden_p(o_ptr)) t++;
		o_ptr->ident &= ~(IDENT_HIDDEN);
	}

	/* Display any newly visible things. */
	update_objects(OUP_ALL);

	/* Show more distant changes. */
	p_ptr->update |= PU_UN_VIEW | PU_VIEW;

	msg_format("You reveal %d hidden object%s.", t, (t == 1) ? "" : "s");
}

/*
 * Destroy whole pack (and equip)
 * This routine will keep artifacts if 'preserve' is on.
 * Dean Anderson
 */
void destroy_pack(void)
{
	int                 i,amt;

	/* Simply destroy every item */
	for (i = INVEN_TOTAL-1; i>=0; i--)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;
		amt = o_ptr->number;
		/* Mega-Hack -- preserve artifacts */
		if (preserve_mode)
		{
			/* Hack -- Preserve unknown artifacts */
			if (artifact_p(o_ptr))
			{
				/* Mega-Hack -- Preserve the artifact */
				a_info[o_ptr->name1].cur_num = 0;
			}
		}
	/* Eliminate the item (from the pack) */
		item_increase(o_ptr, -amt);
		item_optimize(o_ptr);
	}
}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(object_type *o_ptr)
{
	/* Describe */
	msg_format("Examining %v...", object_desc_f3, o_ptr, TRUE, 3);

	/* Describe it fully */
	if (!identify_fully_aux(o_ptr, FALSE)) msg_print("You see nothing special.");
}



/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(object_type *o_ptr)
{
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

	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr);
}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(object_type *o_ptr)
{
	char out_val[80];

	/* Message */
	msg_format("Inscribing %v.", object_desc_f3, o_ptr, TRUE, 3);
	msg_print(NULL);

	/* Start with the old inscription */
	sprintf(out_val, "%.79s", get_inscription(o_ptr));

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", out_val, 80))
	{
		/* Save the inscription */
		o_ptr->note = quark_add(out_val);

		/* Recalculate/redraw stuff (later) */
		update_object(o_ptr);

		/* Make a note of the change. */
		message_add(format("Inscribed %v as %s.",
			object_desc_f3, o_ptr, TRUE, 3, quark_str(o_ptr->note)));
	}
}


/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(object_type *o_ptr)
{
	/* Access the lantern */
	object_type *j_ptr = &inventory[INVEN_LITE];

	/* Refuel */
	j_ptr->pval += o_ptr->pval;

	/* No longer empty. */
	j_ptr->ident &= ~(IDENT_EMPTY);

	/* Take a partial turn */
	energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

	/* Message */
	msg_print("You fuel your lamp.");

	/* Comment */
	if (j_ptr->pval >= FUEL_LAMP)
	{
		j_ptr->pval = FUEL_LAMP;
		msg_print("Your lamp is full.");
	}

	/* Decrease the item */
	item_increase(o_ptr, -1);
	item_describe(o_ptr);
	item_optimize(o_ptr);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}


/*
 * Refuel the players torch (from the pack or floor)
 */
static void do_cmd_refill_torch(object_type *o_ptr)
{
	/* Access the primary torch */
	object_type *j_ptr = &inventory[INVEN_LITE];

	/* Take a partial turn */
	energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

	/* Refuel */
	j_ptr->pval += o_ptr->pval + 5;

	/* No longer empty. */
	j_ptr->ident &= ~(IDENT_EMPTY);

	/* Message */
	msg_print("You combine the torches.");

	/* Over-fuel message */
	if (j_ptr->pval >= FUEL_TORCH)
	{
		j_ptr->pval = FUEL_TORCH;
		msg_print("Your torch is fully fuelled.");
	}

	/* Refuel message */
	else
	{
		msg_print("Your torch glows more brightly.");
	}

	/* Decrease the item. */
	item_increase(o_ptr, -1);
	item_describe(o_ptr);
	item_optimize(o_ptr);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}




/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(object_type *j_ptr)
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
	else if (o_ptr->k_idx == OBJ_BRASS_LANTERN)
	{
		do_cmd_refill_lamp(j_ptr);
	}

	/* It's a torch */
	else if (o_ptr->k_idx == OBJ_WOODEN_TORCH)
	{
		do_cmd_refill_torch(j_ptr);
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

	/* Start at current panel */
	y2 = y1 = panel_row;
	x2 = x1 = panel_col;

	/* Show panels until done */
	while (1)
	{
		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			cptr yss[3] = {" North", "", " South"};
			cptr xss[3] = {" West", "", " East"};
			cptr ys = yss[SGN(y2-y1)+1], xs = xss[SGN(x2-x1)+1];
			cptr of = (*xs || *ys) ? " of" : "";

			char command;

			/* Get a command (or Cancel) */
			if (!get_com(&command,
				"Map sector [%d,%d], which is%s%s%s your sector.  Direction?",
					y2, x2, ys, xs, of)) break;

			/* Extract the action (if any) */
			dir = get_keymap_dir(command);

			/* Error */
			if (!dir) bell(0);
		}

		/* No direction */
		if (!dir) break;

		/* Apply the motion */
		y2 += ddy[dir];
		x2 += ddx[dir];

		/* Verify the row */
		if (y2 > max_panel_rows) y2 = max_panel_rows;
		else if (y2 < 0) y2 = 0;

		/* Verify the col */
		if (x2 > max_panel_cols) x2 = max_panel_cols;
		else if (x2 < 0) x2 = 0;

		/* Handle "changes" */
		if ((y2 != panel_row) || (x2 != panel_col) || centre_view)
		{
			/* Save the new panel info */
			panel_row = y2;
			panel_col = x2;

			/* Recalculate the boundaries */
			panel_bounds();

			/* Update stuff */
			p_ptr->update |= (PU_MONSTERS);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Handle stuff */
			handle_stuff();
		}
	}


	/* Recenter the map around the player */
	verify_panel(FALSE);

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Handle stuff */
	handle_stuff();
}







/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
static bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
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
static void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	u16b holder;

	/* XXX XXX */
	v = v ? v : 0;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}



/*
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "mulitple" monsters:
 *   ^A (all monsters)
 *   ^U (all unique monsters)
 *   ^N (all non-unique monsters)
 *
 * The responses may be sorted in several ways, see below.
 *
 * Note that the player ghosts are ignored. XXX XXX XXX
 */
static void do_cmd_query_symbol_aux(u16b *who)
{
	int i, n, r_idx;
	char sym, query;
	char buf[128];

	bool symbol = FALSE;
	bool uniq = FALSE;
	bool norm = FALSE;
	bool string = FALSE;

	bool recall = FALSE;

	u16b why = 0;

	name_centry *nam_ptr;

	/* Get a character, or abort */
	if (!get_com(&sym, "Enter character to be identified: ")) return;

	/* Find that character info, and describe it.
	 * This assumes that no monster uses \0 as its symbol. */
	for (nam_ptr = ident_info; nam_ptr->idx; nam_ptr++)
	{
		if (sym == nam_ptr->idx) break;
	}

	/* Describe */
	if (sym == KTRL('A'))
	{
		strcpy(buf, "Full monster list.");
	}
	else if (sym == KTRL('U'))
	{
		uniq = TRUE;
		strcpy(buf, "Unique monster list.");
	}
	else if (sym == KTRL('N'))
	{
		norm = TRUE;
		strcpy(buf, "Non-unique monster list.");
	}
	else if (sym == KTRL('S'))
	{
		char *s;

		/* No name. */
		strcpy(buf, "Name: ");
		if (!get_string("Enter the name: ", buf+6, sizeof(buf)-6)) return;
		string = TRUE;

		for (s = buf+6; *s; s++) if (ISUPPER(*s)) *s = TOLOWER(*s);
	}
	else
	{
		sprintf(buf, "%c - %s.", sym, nam_ptr->str);
		symbol = TRUE;
	}

	/* Display the result */
	prt(buf, 0, 0);


	/* Collect matching monsters */
	for (n = 0, i = 1; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip "fake" monsters. */
		if (is_fake_monster(r_ptr)) continue;

		/* Nothing to recall */
		if (!spoil_mon && !r_ptr->r_sights) continue;

		/* Require non-unique monsters if needed */
		if (norm && (r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Require unique monsters if needed */
		if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Require a substring if needed */
		if (string)
		{
			char *s;
			C_TNEW(name, MNAME_MAX, char);

			/* Obtain a lower case string. */
			strnfmt(name, MNAME_MAX, "%v", monster_desc_aux_f3, r_ptr, 1, 0);
			for (s = name; *s; s++) if (ISUPPER(*s)) *s = TOLOWER(*s);

			/* None found. */
			if (!strstr(name, buf+6)) continue;

			TFREE(name);
		}

		/* Require a base symbol if needed. */
		if (symbol && (r_ptr->gfx.dc != sym)) continue;

		/* Collect this monster. */
		who[n++] = i;
	}

	/* Nothing to recall */
	if (!n) return;

	/* Prompt XXX XXX XXX */
	put_str("Recall details? (k/p/y/n): ", 0, 40);

	/* Show help. */
	help_track("query 2");

	/* Query */
	query = inkey();

	/* Remove help */
	help_track(NULL);

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
	if (query != 'y') return;


	/* Sort if needed */
	if (why)
	{
		/* Sort the array */
		ang_sort(who, &why, n, ang_sort_comp_hook, ang_sort_swap_hook);
	}


	/* Show help. */
	help_track("query 3");

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
		mc_add_fmt(" [(r)ecall, ESC]");

		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Save the screen */
				Term_save();

				/* Recall on screen */
				screen_roff(who[i]);

				/* Hack -- Complete the prompt (again) */
				mc_add_fmt(" [(r)ecall, ESC]");
			}

			/* Command */
			query = inkey();

			/* Unrecall */
			if (recall)
			{
				/* Restore */
				Term_load();
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

	/* Done with help. */
	help_track(NULL);

	/* Re-display the identity */
	prt(buf, 0, 0);
}

/*
 * A wrapper around do_cmd_query_symbol_aux() to allocate the who array.
 */
void do_cmd_query_symbol(void)
{
	C_TNEW(who, MAX_R_IDX, u16b);
	do_cmd_query_symbol_aux(who);
	TFREE(who);
}


/*
 * 'Handle' an object, doing whatever seems the sensible thing to it...
 */
void do_cmd_handle(object_type *o_ptr)
{
	void (*funcs[])(object_type *) =
	{
		do_cmd_activate,
		do_cmd_use_staff,
		do_cmd_aim_wand,
		do_cmd_zap_rod,
		do_cmd_read_scroll,
		do_cmd_quaff_potion,
		do_cmd_refill,
		do_cmd_eat_food,
		do_cmd_browse,
	}, (**ptr)(object_type *);

	/* If one of the above is suitable for this object, carry it out. */
	FOR_ALL_IN(funcs, ptr)
	{
		if (item_tester_okay_cmd(*ptr, o_ptr))
		{
			(**ptr)(o_ptr);
			return;
		}
	}

	/* If none is, give an error message. */
	if (item_tester_okay_cmd(do_cmd_wield, o_ptr))
	{
		msg_print("That item must be wielded to be used.");
	}
	else
	{
		msg_print("That item can't be used directly.");
	}
}

