/* File: cmd1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "npp.h"
#include "src/cmds.h"
#include "storedialog.h"
#include "src/object_settings.h"





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

    for (this_o_idx = dungeon_info[py][px].object_idx; this_o_idx; this_o_idx = next_o_idx)
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
 * Add an object to the inventory.
 * Assumes the decision to put in inventory or quiver has already been made.
 * Does NOT Delete the object afterwards.
 * Prints out the appropriate message.
 */
static bool put_object_in_quiver(object_type *o_ptr)
{
    QString o_name;
	int slot;

    u16b msgt = TERM_WHITE;

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
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Message */
    color_message(QString("You have readied %1 (%2) in your quiver.").arg(o_name).arg(index_to_label(slot)), msgt);

	/* Cursed! */
    if (o_ptr->is_cursed())
	{
		/* Warn the player */
        //sound(MSG_CURSED);
        message(QString("Oops! It feels deathly cold!"));

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

		/* Sense the object if allowed */
		if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_CURSED;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);
	}

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    p_ptr->redraw |= (PR_WIN_OBJLIST | PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

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
    QString o_name;

	int slot = inven_carry(o_ptr);

	/*hack - don't pickup &nothings*/
	if (!o_ptr->k_idx) return (FALSE);

	/* Handle errors (paranoia) */
	if (slot < 0) return (FALSE);

	/* Update the quest counter */
    if (o_ptr->is_quest_object())
	{
		p_ptr->notice |= (PN_QUEST_REMAIN);
        p_ptr->redraw |= (PR_SIDEBAR_PL);
	}

	/* Get the object again */
	o_ptr = &inventory[slot];

	/* Describe the object */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Message */
    message(QString("You have %1 (%2).").arg(o_name).arg(index_to_label(slot)));

	/* No longer marked "in use */
	o_ptr->obj_in_use = FALSE;

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    p_ptr->redraw |= (PR_WIN_OBJLIST | PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

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
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
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
void do_cmd_pickup_from_pile(bool pickup, bool msg)
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
        QString q, s;

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
            if (!msg) break;

            pop_up_message_box("You do not have any room for these items.");
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

            if (!get_item_allow(-item, VERIFY_PICKUP)) return;

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
        s = "There are no objects to pick up!";

        if (!get_item(&item, q, s, (USE_FLOOR)))
		{
			/*player chose escape*/
			break;
		}

        /* Floor items are returned as negative numbers */
		item = -item;

		o_ptr = &o_list[item];

        if (!get_item_allow(-item, VERIFY_PICKUP)) return;

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
    p_ptr->redraw |= (PR_WIN_OBJLIST | PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

	/* Just be sure all inventory management is done. */
	notice_stuff();
	handle_stuff();
}

void command_pickup(cmd_arg args)
{
    // Paranoia
    if (args.item >= 0) return;

    // Floor items are numbered negatively
    s16b item = -args.item;

    object_type *o_ptr = &o_list[item];

    if (!get_item_allow(-item, VERIFY_PICKUP)) return;

    /* Pick up the object */
    if (put_object_in_inventory(o_ptr))
    {
        /* Delete the object */
        delete_object_idx(item);
        process_player_energy(BASE_ENERGY_MOVE);
    }

}


void py_pickup_gold(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	s16b this_o_idx, next_o_idx = 0;
	object_type *o_ptr;
    QString o_name;

	/* First, pick up the gold automatically */
    for (this_o_idx = dungeon_info[py][px].object_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		long gold;
		int sound_msg;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;        

		if (o_ptr->tval != TV_GOLD) continue;

		/* Never pick up mimics */
        if (o_ptr->is_mimic()) continue;

		gold = (long)o_ptr->pval * o_ptr->number;

		/* Determine which sound to play */
		if (gold < 200) sound_msg = MSG_MONEY1;
		else if (gold < 600) sound_msg = MSG_MONEY2;
		else sound_msg = MSG_MONEY3;

		/* Describe the gold */
        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

		/* Message */
        sound(sound_msg);
        message(QString("You have found %1.").arg(o_name));

        o_ptr->has_been_seen();

		/* Collect the gold */
		p_ptr->au += gold;

        /* Redraw gold and update player score */
        p_ptr->redraw |= (PR_SIDEBAR_PL);

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

    QString o_name;

	/* Are we allowed to pick up anything here? */
    if (!(f_info[dungeon_info[py][px].feature_idx].f_flags1 & (FF1_DROP))) return;

	/*
	 * As a precaution, first, check for mimics, and reveal them.
	 * This is important, as IDENT_QUEST can be either a quest mimic monster,
     * or a quest object to be picked up.  o_ptr->is_mimic() distinguishes it
	 * as a mimic.
	 */
    for (this_o_idx = dungeon_info[py][px].object_idx; this_o_idx; this_o_idx = next_o_idx)
	{

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Only work with the mimic objects */
        if (!o_ptr->is_mimic()) continue;

		reveal_mimic(this_o_idx, TRUE);
	}

 	/* Automatically destroy squelched items in pile if necessary */
	do_squelch_pile(py, px);

	/* Nothing left */
    if (!dungeon_info[py][px].object_idx) return;

	/* First, pick up gold */
	py_pickup_gold();

	/* Nothing left */
    if (!dungeon_info[py][px].object_idx) return;

	/* Next, put items in the quiver that belong there.  */
    for (this_o_idx = dungeon_info[py][px].object_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		bool do_continue = TRUE;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

        if ((!o_ptr->is_ammo()) && (!is_throwing_weapon(o_ptr))) continue;

		/* Hack - Don't pick up mimic objects */
        if (o_ptr->is_mimic()) continue;

		/* Possibly pickup throwing weapons */
        if (o_ptr->use_verify[AUTO_WIELD_QUIVER]) do_continue = FALSE;
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
    if (!dungeon_info[py][px].object_idx) return;

	/*
	 * Next, pick up items that can be combined with current inventory
	 * and are not specified to leave on the floor.
	 */
    for (this_o_idx = dungeon_info[py][px].object_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Test for auto-pickup */
		if (!inven_stack_okay(o_ptr, INVEN_MAX_PACK)) continue;

		if (k_info[o_ptr->k_idx].squelch == NO_SQUELCH_NEVER_PICKUP) continue;

		/* Hack - Don't pick up mimic objects */
        if (o_ptr->is_mimic()) continue;

		/* Put it in the quiver */
		if (put_object_in_inventory(o_ptr))
		{
			/* Delete the object */
			delete_object_idx(this_o_idx);
		}
	}

	/* Nothing left */
    if (!dungeon_info[py][px].object_idx) return;

	/* Next, pick up items that are marked for auto-pickup.  */
    for (this_o_idx = dungeon_info[py][px].object_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* We are done */
		if (pack_is_full()) break;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

        if (k_info[o_ptr->k_idx].squelch != NO_SQUELCH_ALWAYS_PICKUP) continue;

		/* Object is marked to always pickup */
        if (!k_info[o_ptr->k_idx].aware) continue;

        /* Player doesn't want to pickup item  */
        if (!get_item_allow(-this_o_idx, VERIFY_PICKUP)) continue;

		/* Hack - Don't pick up mimic objects */
        if (o_ptr->is_mimic()) continue;

		/* Put it in the quiver */
		if (put_object_in_inventory(o_ptr))
		{
			/* Delete the object */
			delete_object_idx(this_o_idx);
		}
	}

	/* Nothing left */
    if (!dungeon_info[py][px].object_idx) return;

	if (pickup)
	{
		/* Next, pick up items if option or command asks for auto-pickup.  */
        for (this_o_idx = dungeon_info[py][px].object_idx; this_o_idx; this_o_idx = next_o_idx)
		{
            QString o_name;

			/* No more room */
			if (pack_is_full()) break;

			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Describe the object */
            o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

            if (k_info[o_ptr->k_idx].squelch == NO_SQUELCH_NEVER_PICKUP) continue;

			/* Hack - Don't pick up mimic objects */
            if (o_ptr->is_mimic()) continue;

            /* Player doesn't want to pickup item  */
            if (!get_item_allow(-this_o_idx, VERIFY_PICKUP)) continue;

			/* Put it in the quiver */
			if (put_object_in_inventory(o_ptr))
			{
				/* Delete the object */
				delete_object_idx(this_o_idx);
			}
		}
	}

	/* Nothing left */
    if (!dungeon_info[py][px].object_idx) return;

	if ((carry_query_flag) && (!pack_is_full()))
	{
		do_cmd_pickup_from_pile(pickup, FALSE);
		return;
	}

	/* Count remaining objects.  */
    for (this_o_idx = dungeon_info[py][px].object_idx; this_o_idx; this_o_idx = next_o_idx)
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
        u16b msgt = TERM_WHITE;

		/* Get the object */
        o_ptr = &o_list[dungeon_info[py][px].object_idx];

        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

        if (p_ptr->timed[TMD_BLIND]) color_message(QString("You are aware of %1.").arg(o_name), msgt);

        else color_message(QString("You see %1.").arg(o_name), msgt);
	}

	/* Multiple objects */
	else
	{
        if (p_ptr->timed[TMD_BLIND]) message(QString("You are aware of a pile of %1 objects.").arg(objects_left));

		/* Message */
        else message(QString("You see a pile of %1 objects.").arg(objects_left));
	}

	/* Done */
}

/*
 * Pick up objects on the floor beneath you.
 */
void do_cmd_pickup(void)
{

    // Paranoia
    if (!p_ptr->playing) return;

    do_cmd_pickup_from_pile(TRUE, TRUE);

    /*
     * Intentionally use some energy in case
     * the player is intentionally using a turn.
     */
    process_player_energy(BASE_ENERGY_MOVE);
}


/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
int move_player(int dir, int jumping)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u16b old_dtrap, new_dtrap;

	s16b used_energy = BASE_ENERGY_MOVE;

	int y, x;

    QString name;

	feature_type *f_ptr;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Get the feature */
    f_ptr = &f_info[dungeon_info[y][x].feature_idx];

	/* Hack -- attack monsters */
    if (dungeon_info[y][x].monster_idx > 0)
	{
		/* Attack */
        py_attack(y, x);
        // Energy expended by py_attack
        return (0);
	}

	/* Optionally alter known traps/doors on (non-jumping) movement */
    if ((easy_alter) && (!jumping) && (dungeon_info[y][x].cave_info & (CAVE_MARK)) &&
		 	 (_feat_ff1_match(f_ptr, FF1_CAN_OPEN | FF1_CAN_DISARM) ||
			 (cave_player_trap_bold(y, x) &&
            !(x_list[dungeon_info[y][x].effect_idx].x_flags & (EF1_HIDDEN)))))

    {
        cmd_arg args;
        args.wipe();
        args.direction = dir;

		/* Alter */
        command_alter(args);

        // Energy expended by command alter
        return(0);
	}

	/* Player can not walk through certain terrain */
    if (!cave_ff1_match(y, x, FF1_MOVE))
	{
		/* Disturb the player */
        disturb(FALSE, TRUE);

		/* Notice unknown obstacles */
        if (!(dungeon_info[y][x].cave_info & (CAVE_MARK)))
		{
			/* Get hit by traps */
			if (cave_passive_trap_bold(y, x))
			{
				/* Hit the trap */
                (void)hit_trap(x_list[dungeon_info[y][x].effect_idx].x_f_idx, y, x, MODE_ACTION);
			}

			/* Get the feature name */
            name = feature_desc(f_ptr - f_info, TRUE, TRUE);

			/* Tell the player */
            message(QString("You feel %1 blocking your way.").arg(name));

            dungeon_info[y][x].mark_square();

			light_spot(y, x);

		}

		/* Mention known obstacles */
		else if (!hit_wall(y, x, TRUE))
		{
			/* Get the feature name */
            name = feature_desc(f_ptr - f_info, TRUE, TRUE);

			/* Tell the player */
            message(QString("There is %1 blocking your way.").arg(name));
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
            dungeon_info[y][x].mark_square();

			/* Redraw */
			light_spot(y, x);
		}

		/* Get the first effect */
        x_idx = dungeon_info[y][x].effect_idx;

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
                name = feature_desc(feat, TRUE, TRUE);

				/* Tell the player */
                message(QString("There is %1 blocking your way.").arg(name));

				/* Done */
				break;
			}

			/* Point to the next effect */
			x_idx = x_ptr->next_x_idx;
		}

		/* Paranoia */
        if (!x_idx) message(QString("You cannot move into this grid."));
	}

	/* Some terrain prevents flying. */
    else if (!(feat_ff2_match(dungeon_info[y][x].feature_idx, FF2_CAN_FLY)) && (p_ptr->timed[TMD_FLYING]))
	{
        int feat = dungeon_info[y][x].feature_idx;

		/* Discover unknown terrain */
		if (!cave_flag_bold(y, x, CAVE_MARK))
		{
			/* Remember */
            dungeon_info[y][x].mark_square();

			/* Redraw */
			light_spot(y, x);
		}

		/* Get the feature name */
        name = feature_desc(feat, TRUE, TRUE);

		/* Tell the player */
        message(QString("There is %1 blocking your way.").arg(name));

		/* Give the player the option to stop flying. */
        if (get_check("So you want to stop flying? "))
		{
			clear_timed(TMD_FLYING, FALSE);

            message(QString("You land."));

			/*Little energy used*/
			used_energy = BASE_ENERGY_MOVE / 10;
		}
	}

    else if(cave_hidden_object_bold(y, x))
    {
        message(QString("You have found something."));
        delete_effect_idx(dungeon_info[y][x].effect_idx);
        place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
        used_energy = BASE_ENERGY_MOVE / 4;
    }

	/* Normal movement */
	else
	{
		feature_lore *f_l_ptr;

		/* Sound XXX XXX XXX */
		/* sound(MSG_WALK); */

		/* See if trap detection status will change */
        old_dtrap = ((dungeon_info[py][px].cave_info & (CAVE_DTRAP)) != 0);
        new_dtrap = ((dungeon_info[y][x].cave_info & (CAVE_DTRAP)) != 0);

		/* Note the change in the detect status */
        if (old_dtrap != new_dtrap) p_ptr->redraw |= (PR_STATUSBAR);

		/* Disturb player if the player is about to leave the area */
		if (disturb_detect &&
                p_ptr->is_running() && old_dtrap && !new_dtrap)
		{
            disturb(FALSE, TRUE);
            return 0;
		}

		/* Move player */
		monster_swap(py, px, y, x);

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;

		/*Get ready to mark lore*/
        f_l_ptr = &f_l_list[dungeon_info[p_ptr->py][p_ptr->px].feature_idx];

		/*Check if the player is native*/
		if (is_player_native(y, x))
		{
			if (!p_ptr->timed[TMD_FLYING])
			{
				/*Mark the lore*/
                if (f_l_ptr->f_l_native_moves < UCHAR_MAX ) f_l_ptr->f_l_native_moves ++;

				/*record the energy*/
                used_energy = f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx].native_energy_move;
			}
		}
		else
		{
			if (!p_ptr->timed[TMD_FLYING])
			{
				/*Mark the lore*/
                if (f_l_ptr->f_l_non_native_moves < UCHAR_MAX ) f_l_ptr->f_l_non_native_moves ++;

				/*record the energy*/
                used_energy = f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx].non_native_energy_move;
			}
		}

		/* Spontaneous Searching */
        if ((p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] >= SEARCH_CHANCE_MAX) ||
            (0 == rand_int(SEARCH_CHANCE_MAX - p_ptr->state.skills[SKILL_SEARCH_FREQUENCY])))
		{
            do_search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
            do_search();
		}

		/* Handle "objects" */
        py_pickup(jumping != always_pickup);

		/* Handle "store doors" */
        if (dungeon_info[y][x].is_store())
		{
			/* Disturb */
            disturb(TRUE, TRUE);

			/* Hack -- Enter store */
            //p_ptr->command_new = '_';
            int feat_idx = dungeon_info[y][x].feature_idx;
            int store_idx = f_info[feat_idx].f_power;
            launch_store(store_idx);
            return (0);
		}

		/* Hit a trap */
		else if (cave_passive_trap_bold(y, x))
		{
			/* Hit the trap */
            (void)hit_trap(x_list[dungeon_info[p_ptr->py][p_ptr->px].effect_idx].x_f_idx, y, x, MODE_ACTION);
		}

		/* Discover secrets */
		else if (_feat_ff1_match(f_ptr, FF1_SECRET) && !p_ptr->timed[TMD_FLYING])
		{
			/* Find the secret */
			find_secret(y, x);

			/* Get the feature again */
            f_ptr = &f_info[dungeon_info[y][x].feature_idx];
		}

		/* Record the energy for flying creatures.*/
        if (p_ptr->timed[TMD_FLYING]) used_energy = BASE_ENERGY_FLYING;

		/* Walk on a monster trap */
		else if (cave_monster_trap_bold(y,x))
		{
            message(QString("You inspect your cunning trap."));
		}

	}

    return used_energy;
}

