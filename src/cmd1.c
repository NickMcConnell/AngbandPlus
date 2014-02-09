/* File: cmd1.c */

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



/*
 * Search for hidden things
 */
void search(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, chance;

	object_type *o_ptr;


	/* Start with base search ability */
	chance = p_ptr->state.skills[SKILL_SEARCH];

	/* Penalize various conditions */
	if (p_ptr->timed[TMD_BLIND] || no_light()) chance = chance / 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) chance = chance / 10;

	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* Get the feature */
				int feat = cave_feat[y][x];

				/* Reveal interesting secret features */
				if (feat_ff1_match(feat, FF1_SECRET) &&
					(feat_ff3_match(feat, FF3_PICK_TRAP |
						FF3_PICK_DOOR) ||
					 !feat_ff1_match(feat, FF1_MOVE)))
				{
					find_secret(y, x);
				}

				/* Find hidden player traps */
				if (cave_player_trap_bold(y, x))
				{
					/* Get the trap */
					effect_type *x_ptr = &x_list[cave_x_idx[y][x]];

					/* Ignore known traps */
					if (x_ptr->x_flags & (EF1_HIDDEN))
					{
						/* Reveal the trap */
						x_ptr->x_flags &= ~(EF1_HIDDEN);

						/* Show the trap */
						note_spot(y, x);

						light_spot(y, x);

						/* Message */
						msg_print("You have found a trap!");

						/* Disturb the player */
						disturb(0, 0);
					}
				}

				/* Scan all objects in the grid */
				for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
				{
					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST) continue;

					/* Skip disarmed chests */
					if (o_ptr->pval <= 0) continue;

					/* Skip non-trapped chests */
					if (!chest_traps[o_ptr->pval]) continue;

					if (o_ptr->ident & (IDENT_QUEST)) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
						msg_print("You have discovered a trap on the chest!");

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0, 0);
					}
				}
			}
		}
	}
}


/*
 * Helper function for do_cmd_pickup_from_pile.
 * Counts the number of objects that could be picked up.
 *
 */
static int count_possible_pickups(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	int num_items = 0;
	int stackable_items = 0;
	int max_pickups;

	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Count the objects */
		num_items++;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Test for auto-pickup */
		if (!inven_stack_okay(o_ptr, INVEN_MAX_PACK)) continue;

		/*This item stacks */
		stackable_items ++;
	}

	/*Count open slots + stackable items */
	max_pickups = INVEN_MAX_PACK - p_ptr->inven_cnt + stackable_items;

	if (num_items <= max_pickups) return (num_items);
	/*else*/
	return (max_pickups);
}


/*
 * Determine if the object should be picked up -- "=g" or autopickup is set
 */
static bool auto_pickup_inscrip(const object_type *o_ptr)
{
	cptr s;

	/* No inscription */
	if (!o_ptr->obj_note) return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->obj_note), '=');

	/* Process inscription */
	while (s)
	{
		/* Auto-pickup on "=g" */
		if (s[1] == 'g') return (TRUE);

		/* Find another '=' */
		s = strchr(s + 1, '=');
	}

	/* Don't auto pickup */
	return (FALSE);
}


/*
 * Add an object to the inventory.
 * Assumes the decision to put in inventory or quiver has already been made.
 * Does NOT Delete the object afterwards.
 * Prints out the appropriate message.
 */
static bool put_object_in_quiver(object_type *o_ptr)
{
	char o_name[80];
	int slot;

	u16b msgt = MSG_GENERIC;

	/*hack - don't pickup &nothings*/
	if (!o_ptr->k_idx) return (FALSE);

	/* make sure this item won't cause overflow */
	if (pack_is_full())
	{
		if ((quiver_space_per_unit(o_ptr) * o_ptr->number) > p_ptr->quiver_remainder) return (FALSE);
	}

	slot = wield_slot_ammo(o_ptr);

	/* Handle errors (paranoia) */
	if (slot == QUIVER_END) return (FALSE);

	/* Try to put the quiver in the slot */
	slot = quiver_carry(o_ptr);

	/* Unsuccessful */
	if (slot == -1) return (FALSE);

	/* Get the object again */
	o_ptr = &inventory[slot];

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Message */
	msg_c_format(msgt, "You have readied %s (%c) in your quiver.", o_name, index_to_label(slot));

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

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	p_ptr->redraw |= (PR_ITEMLIST | PR_INVEN | PR_EQUIP);

	notice_stuff();
	handle_stuff();

	return (TRUE);
}


/*
 * Add an object to the inventory.
 * Assumes the decision to put in inventory or quiver has already been made.
 * Does NOT Delete the object afterwards.
 * Prints out a message.
 * Assumes a check for mimic objects has already been made, and they
 * have been revealed.
 */
bool put_object_in_inventory(object_type *o_ptr)
{
	char o_name[80];

	u16b msgt = MSG_GENERIC;

	int slot = inven_carry(o_ptr);

	/*hack - don't pickup &nothings*/
	if (!o_ptr->k_idx) return (FALSE);

	/* Handle errors (paranoia) */
	if (slot < 0) return (FALSE);

	/* Update the quest counter */
	if (o_ptr->ident & (IDENT_QUEST))
	{
		p_ptr->notice |= (PN_QUEST_REMAIN);
		p_ptr->redraw |= (PR_QUEST_ST);
	}

	/* Get the object again */
	o_ptr = &inventory[slot];

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Message */
	msg_c_format(msgt, "You have %s (%c).", o_name, index_to_label(slot));

	/* No longer marked "in use */
	o_ptr->obj_in_use = FALSE;

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	p_ptr->redraw |= (PR_ITEMLIST | PR_INVEN | PR_EQUIP);

	notice_stuff();
	handle_stuff();

	return (TRUE);
}


/* Helper function for do_cmd_pickup_from_pile */
static int get_first_item_for_pickup(void)
{
	int y = p_ptr->py;
	int x = p_ptr->px;

	s16b this_o_idx, next_o_idx = 0;
	object_type *o_ptr;

	/* First, find the item */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Test for auto-pickup */
		if (inven_carry_okay(o_ptr))
		{
			return (this_o_idx);
		}
	}

	/* Not found */
	return (-1);
}


/*
 * Allow the player to sort through items in a pile and
 * pickup what they want.  This command does not use
 * any energy because it costs a player no extra energy
 * to walk into a grid and automatically pick up items
 */
void do_cmd_pickup_from_pile(bool pickup, bool message)
{
	object_type *o_ptr;

	int py = p_ptr->py;
	int px = p_ptr->px;

	byte cycles = 0;

	/*
	 * Loop through and pick up objects until escape is hit or the backpack
	 * can't hold anything else.
	 */
	while (TRUE)
	{
		int item;
		cptr q, s;

		int floor_list[MAX_FLOOR_STACK];

		int floor_num;
		int pickup_num;
		int want_pickup_num;

		if (cycles < 2) cycles++;

		/*start with everything updated*/
		handle_stuff();

		/* Scan for floor objects */
		floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, py, px, 0x03);

		/* No pile */
		if (floor_num < 1) break;

		/* Restrict the choices */
		item_tester_hook = inven_carry_okay;

		/* re-test to see if we can pick any of them up */
		floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, py, px, 0x03);

		pickup_num = count_possible_pickups();

		/* Filter out items you don't want to pickup */
		want_pickup_num = count_floor_items(py, px, TRUE);

		/* Nothing the player wants */
		if ((!want_pickup_num) && (!pickup)) break;

		/* Can't pick anything up */
		if(!pickup_num)
		{
			if (!message) break;

			msg_print("You do not have any room for these items.");
			break;
		}

		/* Auto-pickup if only one item */
		else if ((!floor_query_flag) && (pickup) && (cycles < 2) &&
				(want_pickup_num == 1))
		{
			item = get_first_item_for_pickup();

			/* paranoia */
			if (item == -1) break;

			o_ptr = &o_list[item];

			/* Pick up the object */
			if (put_object_in_inventory(o_ptr))
			{
				/* Delete the gold */
				delete_object_idx(item);
				break;
			}
		}

		/*clear the restriction*/
		item_tester_hook = NULL;

		q = "Pick up which object? (ESC to cancel):";
		s = "THere are no objects to pick up!";

		if (!get_item(&item, q, s, (USE_FLOOR)))
		{
			/*player chose escape*/
			break;
		}

		/* FLoor items are returned as negative numbers */
		item = -item;

		o_ptr = &o_list[item];

		/* Pick up the object */
		if (put_object_in_inventory(o_ptr))
		{
			/* Delete the object */
			delete_object_idx(item);
		}

		/* Pickup failed.  Quit. */
		else break;
	}

	/*clear the restriction*/
	item_tester_hook = NULL;

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);
	p_ptr->redraw |= (PR_ITEMLIST | PR_INVEN | PR_EQUIP);

	/* Just be sure all inventory management is done. */
	notice_stuff();
	handle_stuff();
}


void py_pickup_gold(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	s16b this_o_idx, next_o_idx = 0;
	object_type *o_ptr;
	char o_name[80];

	/* First, pick up the gold automatically */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		long gold;
		int sound_msg;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		if (o_ptr->tval != TV_GOLD) continue;

		/* Never pick up mimics */
		if (o_ptr->mimic_r_idx) continue;

		gold = (long)o_ptr->pval * o_ptr->number;

		/* Determine which sound to play */
		if (gold < 200) sound_msg = MSG_MONEY1;
		else if (gold < 600) sound_msg = MSG_MONEY2;
		else sound_msg = MSG_MONEY3;

		/* Describe the gold */
		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

		/* Message */
		message_format(sound_msg, 0, "You have found %s.", o_name);

		/* Collect the gold */
		p_ptr->au += gold;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Delete the gold */
		delete_object_idx(this_o_idx);

		/* Check the next object */
		continue;
	}

	return;
}


/*
 * Handle picking up objects from a given square.
 * First anything marked always squelch is destroyed.
 * Next Several items are picked up automatically.
 * Gold, items marked "always pickup", items that go
 * in the quiver, and objects marked "always pickup".
 *
 * The function stops at that point if pickup is false.
 *
 * This function has been re-written to be more linear,
 * and it sacrifices efficiency for clarity and flexibility.
 *
 */
void py_pickup(bool pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	int objects_left = 0;

	char o_name[80];

	/* Are we allowed to pick up anything here? */
	if (!(f_info[cave_feat[py][px]].f_flags1 & (FF1_DROP))) return;

	/*
	 * As a precaution, first, check for mimics, and reveal them.
	 * This is important, as IDENT_QUEST can be either a quest mimic monster,
	 * or a quest object to be picked up.  o_ptr->mimic_r_idx distinguishes it
	 * as a mimic.
	 */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Only work with the mimic objects */
		if (!o_ptr->mimic_r_idx) continue;

		reveal_mimic(this_o_idx, TRUE);
	}

 	/* Automatically destroy squelched items in pile if necessary */
	do_squelch_pile(py, px);

	/* Nothing left */
	if (!cave_o_idx[py][px]) return;

	/* First, pick up gold */
	py_pickup_gold();

	/* Nothing left */
	if (!cave_o_idx[py][px]) return;

	/* Next, put items in the quiver that belong there.  */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		bool do_continue = TRUE;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		if ((!obj_is_ammo(o_ptr)) && (!is_throwing_weapon(o_ptr))) continue;

		/* Hack - Don't pick up mimic objects */
		if (o_ptr->mimic_r_idx) continue;

		/* Possibly pickup throwing weapons */
		if (weapon_inscribed_for_quiver(o_ptr)) do_continue = FALSE;

		/* Only quiver objects */
		else if (ammo_inscribed_for_quiver(o_ptr)) do_continue = FALSE;

		else if (quiver_stack_okay(o_ptr)) do_continue = FALSE;

		if (do_continue) continue;

		/* Put it in the quiver */
		if (put_object_in_quiver(o_ptr))
		{
			/* Delete the object */
			delete_object_idx(this_o_idx);
		}
	}

	/* Nothing left */
	if (!cave_o_idx[py][px]) return;

	/*
	 * Next, pick up items that can be combined with current inventory
	 * and are not specified to leave on the floor.
	 */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Test for auto-pickup */
		if (!inven_stack_okay(o_ptr, INVEN_MAX_PACK)) continue;

		if (k_info[o_ptr->k_idx].squelch == NO_SQUELCH_NEVER_PICKUP) continue;

		/* Hack - Don't pick up mimic objects */
		if (o_ptr->mimic_r_idx) continue;

		/* Put it in the quiver */
		if (put_object_in_inventory(o_ptr))
		{
			/* Delete the object */
			delete_object_idx(this_o_idx);
		}
	}

	/* Nothing left */
	if (!cave_o_idx[py][px]) return;

	/* Next, pick up items that are marked for auto-pickup.  */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		bool do_continue = TRUE;

		/* We are done */
		if (pack_is_full()) break;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Object is marked to always pickup */
		if ((k_info[o_ptr->k_idx].squelch == NO_SQUELCH_ALWAYS_PICKUP)  &&
			(k_info[o_ptr->k_idx].aware)) do_continue = FALSE;

		/* Item is marked for auto-pickup with =g */
		if (auto_pickup_inscrip(o_ptr)) do_continue = FALSE;

		if (do_continue) continue;

		/* Hack - Don't pick up mimic objects */
		if (o_ptr->mimic_r_idx) continue;

		/* Put it in the quiver */
		if (put_object_in_inventory(o_ptr))
		{
			/* Delete the object */
			delete_object_idx(this_o_idx);
		}
	}

	/* Nothing left */
	if (!cave_o_idx[py][px]) return;

	if (pickup)
	{
		/* Next, pick up items if option or command asks for auto-pickup.  */
		for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
		{
			char o_name[80];

			/* No more room */
			if (pack_is_full()) break;

			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Describe the object */
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

			if (k_info[o_ptr->k_idx].squelch == NO_SQUELCH_NEVER_PICKUP) continue;

			/* Hack - Don't pick up mimic objects */
			if (o_ptr->mimic_r_idx) continue;

			/* Put it in the quiver */
			if (put_object_in_inventory(o_ptr))
			{
				/* Delete the object */
				delete_object_idx(this_o_idx);
			}
		}
	}

	/* Nothing left */
	if (!cave_o_idx[py][px]) return;

	if ((carry_query_flag) && (!pack_is_full()))
	{
		do_cmd_pickup_from_pile(pickup, FALSE);
		return;
	}

	/* Count remaining objects.  */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Only marked objects */
		if (!o_ptr->marked) continue;

		objects_left++;
	}

	/* Nothing else to report */
	if (!objects_left) return;

	/* Only one object */
	if (objects_left == 1)
	{
		u16b msgt = MSG_GENERIC;

		/* Get the object */
		o_ptr = &o_list[cave_o_idx[py][px]];

		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

		if (p_ptr->timed[TMD_BLIND]) msg_c_format(msgt, "You are aware of %s.", o_name);

		else msg_c_format(msgt, "You see %s.", o_name);
	}

	/* Multiple objects */
	else
	{
		if (p_ptr->timed[TMD_BLIND]) msg_format("You are aware of a pile of %d objects.", objects_left);

		/* Message */
		else msg_format("You see a pile of %d objects.", objects_left);
	}

	/* Done */
}


/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
s16b move_player(int dir, int jumping)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u16b old_dtrap, new_dtrap;

	s16b used_energy = BASE_ENERGY_MOVE;

	int y, x;

	char name[80];

	feature_type *f_ptr;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Get the feature */
	f_ptr = &f_info[cave_feat[y][x]];

	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x);
	}

	/* Optionally alter known traps/doors on (non-jumping) movement */
	else if ((easy_alter) && (!jumping) && (cave_info[y][x] & (CAVE_MARK)) &&
		 	 (_feat_ff1_match(f_ptr, FF1_CAN_OPEN | FF1_CAN_DISARM) ||
			 (cave_player_trap_bold(y, x) &&
			!(x_list[cave_x_idx[y][x]].x_flags & (EF1_HIDDEN)))))

	{
		/* Auto-repeat if not already repeating */
		if (!p_ptr->command_rep && (p_ptr->command_arg <= 0))
		{
			p_ptr->command_rep = 99;

			/* Reset the command count */
			p_ptr->command_arg = 0;
		}

		/* Alter */
		do_cmd_alter_aux(dir);

	}

	/* Player can not walk through certain terrain */
	else if (!cave_ff1_match(y, x, FF1_MOVE))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Notice unknown obstacles */
		if (!(cave_info[y][x] & (CAVE_MARK)))
		{
			/* Get hit by traps */
			if (cave_passive_trap_bold(y, x))
			{
				/* Hit the trap */
				hit_trap(x_list[cave_x_idx[y][x]].x_f_idx, y, x, MODE_ACTION);
			}

			/* Get the feature name */
			feature_desc(name, sizeof(name), f_ptr - f_info, TRUE, TRUE);

			/* Tell the player */
			msg_format("You feel %s blocking your way.", name);

			cave_info[y][x] |= (CAVE_MARK);

			light_spot(y, x);

		}

		/* Mention known obstacles */
		else if (!hit_wall(y, x, TRUE))
		{
			/* Get the feature name */
			feature_desc(name, sizeof(name), f_ptr - f_info,
				TRUE, TRUE);

			/* Tell the player */
			msg_format("There is %s blocking your way.", name);
		}
	}


	/* There is some effect blocking movement */
	else if (!cave_passable_bold(y, x))
	{
		int x_idx;

		/* Discover unknown terrain */
		if (!cave_flag_bold(y, x, CAVE_MARK))
		{
			/* Remember */
			cave_info[y][x] |= (CAVE_MARK);

			/* Redraw */
			light_spot(y, x);
		}

		/* Get the first effect */
		x_idx = cave_x_idx[y][x];

		/* Find the effect that disables movement */
		while (x_idx)
		{
			u16b feat;

			/* Get the effect */
			effect_type *x_ptr = &x_list[x_idx];

			/* Get the associated feature */
			feat = x_ptr->x_f_idx;

			/* Check the FF1_MOVE flag */
			if (feat && !feat_ff1_match(feat, FF1_MOVE))
			{
				/* Get the feature name */
				feature_desc(name, sizeof(name), feat, TRUE, TRUE);

				/* Tell the player */
				msg_format("There is %s blocking your way.", name);

				/* Done */
				break;
			}

			/* Point to the next effect */
			x_idx = x_ptr->next_x_idx;
		}

		/* Paranoia */
		if (!x_idx) msg_print("You cannot move into this grid.");
	}

	/* Some terrain prevents flying. */
	else if (!(feat_ff2_match(cave_feat[y][x], FF2_CAN_FLY)) && (p_ptr->timed[TMD_FLYING]))
	{
		char out_val[80];
		int feat = cave_feat[y][x];

		/* Discover unknown terrain */
		if (!cave_flag_bold(y, x, CAVE_MARK))
		{
			/* Remember */
			cave_info[y][x] |= (CAVE_MARK);

			/* Redraw */
			light_spot(y, x);
		}

		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, TRUE, TRUE);

		/* Tell the player */
		msg_format("There is %s blocking your way.", name);

		/* Give the player the option to stop flying. */
		sprintf(out_val, "So you want to stop flying? ");
		if (get_check(out_val))
		{
			clear_timed(TMD_FLYING, FALSE);

			msg_print("You land.");

			/*Little energy used*/
			used_energy = BASE_ENERGY_MOVE / 10;
		}

	}

	/* Normal movement */
	else
	{
		feature_lore *f_l_ptr;

		/* Sound XXX XXX XXX */
		/* sound(MSG_WALK); */

		/* See if trap detection status will change */
		old_dtrap = ((cave_info[py][px] & (CAVE_DTRAP)) != 0);
		new_dtrap = ((cave_info[y][x] & (CAVE_DTRAP)) != 0);

		/* Note the change in the detect status */
		if (old_dtrap != new_dtrap) p_ptr->redraw |= (PR_DTRAP);

		/* Disturb player if the player is about to leave the area */
		if (disturb_detect &&
				p_ptr->running && old_dtrap && !new_dtrap)
		{
			disturb(0, 0);
			return (used_energy);
		}

		/* Move player */
		monster_swap(py, px, y, x);

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;

		/*Get ready to mark lore*/
		f_l_ptr = &f_l_list[cave_feat[p_ptr->py][p_ptr->px]];

		/*Check if the player is native*/
		if (is_player_native(y, x))
		{
			if (!p_ptr->timed[TMD_FLYING])
			{
				/*Mark the lore*/
				if (f_l_ptr->f_l_native_moves < MAX_UCHAR) f_l_ptr->f_l_native_moves ++;

				/*record the energy*/
				used_energy = f_info[cave_feat[p_ptr->py][p_ptr->px]].native_energy_move;
			}
		}
		else
		{
			if (!p_ptr->timed[TMD_FLYING])
			{
				/*Mark the lore*/
				if (f_l_ptr->f_l_non_native_moves < MAX_UCHAR) f_l_ptr->f_l_non_native_moves ++;

				/*record the energy*/
				used_energy = f_info[cave_feat[p_ptr->py][p_ptr->px]].non_native_energy_move;
			}
		}

		/* Spontaneous Searching */
		if ((p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] >= 50) ||
			(0 == rand_int(50 - p_ptr->state.skills[SKILL_SEARCH_FREQUENCY])))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle "objects" */
		py_pickup(jumping != always_pickup);

		/* Handle "store doors" */
		if (cave_shop_bold(y, x))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';

			/* Free turn XXX XXX XXX */
			p_ptr->p_energy_use = 0;
		}

		/* Hit a trap */
		else if (cave_passive_trap_bold(y, x))
		{
			/* Hit the trap */
 			hit_trap(x_list[cave_x_idx[p_ptr->py][p_ptr->px]].x_f_idx, y, x, MODE_ACTION);
		}

		/* Discover secrets */
		else if (_feat_ff1_match(f_ptr, FF1_SECRET) && !p_ptr->timed[TMD_FLYING])
		{
			/* Find the secret */
			find_secret(y, x);

			/* Get the feature again */
			f_ptr = &f_info[cave_feat[y][x]];
		}

		/* Record the energy for flying creatures.*/
		if (p_ptr->timed[TMD_FLYING])	used_energy = BASE_ENERGY_MOVE;

		/* Reveal when you are on shallow or deep  terrain */
		else if (!(cave_info[y][x] & (CAVE_MARK)) &&
				_feat_ff3_match(f_ptr, FF2_SHALLOW | FF2_DEEP))
		{
			/* Get the name */
			feature_desc(name, sizeof(name), f_ptr - f_info,
				FALSE, TRUE);

			/* Tell the player */
			msg_format("You feel you are in %s.", name);

			cave_info[y][x] |= (CAVE_MARK);

			light_spot(y, x);
		}

		/* Walk on a monster trap */
		else if (cave_monster_trap_bold(y,x))
		{
			msg_print("You inspect your cunning trap.");
		}

	}

	return (used_energy);
}

