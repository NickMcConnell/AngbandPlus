/* File: cmd3.c */

/*
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
	/* Check for a usable slot */
	if (wield_slot(o_ptr) >= INVEN_WIELD) return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}


/* EFGchange notice obvious effects */
typedef struct {u32b flag; char *name;} flagname;
static flagname boostconv[] =
{
	{ TR1_STR,      "strength" },
	{ TR1_INT,      "intelligence" },
	{ TR1_WIS,      "wisdom" },
	{ TR1_DEX,      "dexterity" },
	{ TR1_CON,      "constitution" },
	{ TR1_CHR,      "charisma" },
	{ TR1_STEALTH,  "stealth" }, /* unused */
	{ TR1_SPEED,    "speed" },
	{ TR1_BLOWS,    "blows" },
	{ TR1_SHOTS,    "shots" },
	{ TR1_INFRA,    "alertness" },
};

/* perhaps this belongs in a different file */
bool obviously_excellent(const object_type *o_ptr, bool to_print, char *o_name)
{
	bool ret = FALSE;

	/* the player should be informed of items that obviously boost */
	/* ??? should check for tried, print this when "I"nspecting a tried object */

	int i;
	u32b f1, f2, f3, f4;
	char *desc;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	desc = (o_ptr->pval >= 0) ? "boosts" : "reduces";
	for (i = 0; i < sizeof(boostconv)/sizeof(flagname); i++)
	{
		if (f1 & boostconv[i].flag)
		{
			if (o_ptr->pval > 0) 
			{
				ret = TRUE;
			}
			if (to_print)
				msg_format("%s %s your %s by %d.", o_name, desc, boostconv[i].name, abs(o_ptr->pval));
		}
	}
	if (f3 & (TR3_ACTIVATE))
	{
		ret = TRUE;
		if (to_print)
			msg_format("%s can be activated.", o_name);
	}
	if (f3 & (TR3_DARKVIS))
	{
		ret = TRUE;
		if (to_print)
			msg_format("%s grants darkvision.", o_name);
	}
	if (f3 & (TR3_TELEPATHY))
	{
		ret = TRUE;
		if (to_print)
			msg_format("%s provides ESP.", o_name);
	}
	if (f3 & (TR3_LITE))
	{
		ret = TRUE;
		if (to_print)
			msg_format("%s provides permanent light.", o_name);
	}
	if (f3 & (TR3_DRAIN_EXP))
	{
		if (to_print)
			msg_format("%s drains experience.", o_name);
	}
	if (f3 & (TR3_STOPREGEN))
	{
		if (to_print)
			msg_format("%s prevents hit point regeneration.", o_name);
	}
	/* be sure it doesn't get splendid pseudo if it's cursed */
	if (cursed_p(o_ptr)) return FALSE;

	return ret;
}

/* */
static int quiver_wield(int item, object_type *o_ptr)
{
	int slot = 0;
	bool use_new_slot = FALSE;
	int num, i, rthis;

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

	/* reorder the quiver (before updating p_ptr->pack_size_reduce) */
	rthis = reorder_quiver(slot);

	/* Update "p_ptr->pack_size_reduce" */
	find_quiver_size();

	/* return the perhaps modified slot from the reorder quiver function */
	return rthis;
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
	int item, slot;
	bool is_splendid;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;
	u32b f1, f2, f3, f4;

	object_type *equip_o_ptr;

	cptr act;

	cptr q, s;

	char o_name[80];
	char out_val[160];


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

	/* check for WIELD_SHIELD and THROWN flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Check the slot */
	slot = wield_slot(o_ptr);
	
	/* off-hand weapon (not duel-wielding (yet), more like using a weapon as a shield) */
    if ((slot == INVEN_WIELD) && (f4 & TR4_WIELD_SHIELD))
	{
       if (get_check("Wield in off-hand for defence? "))
       {
          slot = INVEN_ARM;
       }
    }

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

	/* Get a pointer to the slot to be removed */
	equip_o_ptr = &inventory[slot];

	/* Prevent wielding into a cursed slot */
	if (!IS_QUIVER_SLOT(slot) && cursed_p(equip_o_ptr))
	{
		/* Describe it */
		object_desc(o_name, sizeof(o_name), equip_o_ptr, FALSE, 0);

		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
		           o_name, describe_use(slot));

		/* Cancel the command */
		return;
	}

	/* Double-check taking off an item with "!t" */
	if (equip_o_ptr->note)
	{
		/* Find a '!' */
		s = strchr(quark_str(equip_o_ptr->note), '!');

		/* Process preventions */
		/* XXX Perhaps this should be factored out to a separate function? */
		while (s)
		{
			/* Check the "restriction" */
			if (s[1] == 't')
			{
				/* Describe it */
				object_desc(o_name, sizeof(o_name), equip_o_ptr, TRUE, 3);

				/* Prompt */
				strnfmt(out_val, sizeof(out_val), "Really take off %s? ", o_name);

				/* Forget it */
				if (!get_check(out_val)) return;
			}

			/* Find another '!' */
			s = strchr(s + 1, '!');
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Ammo goes in quiver slots, which have special rules. */
	if (IS_QUIVER_SLOT(slot))
	{
		/* Put the ammo in the quiver */
		slot = quiver_wield(item, o_ptr);

		/* Can't do it */
		if (!slot) return;

		/* Get the object again */
		o_ptr = &inventory[slot];
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
	
		/* Increase the weight */
		p_ptr->total_weight += i_ptr->weight;
	
		/* Increment the equip counter by hand */
		p_ptr->equip_cnt++;
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

	/* use up a charge for constant activation when wielding */
	if (f2 & (TR2_CONSTANTA))
	{
		o_ptr->pval--;
		msg_print("a charge has been used.");

		/* This lets you be able to tell what it is */
		object_aware(o_ptr);
		object_known(o_ptr);
	}

	/* EFGchange notice obvious effects */
	is_splendid = FALSE;
	if (!object_known_p(o_ptr))
		is_splendid = obviously_excellent(o_ptr, TRUE, o_name);
	/* EFGchange some jewelry should self-id */
	/* ??? should this be in a table somewhere? */
	/* ??? should we id unaware weakness/stupidity for neg pval when
		matching strength/int is aware? */
	/* ??? should be a separate function, so can be called upon pseudo? */
	switch(o_ptr->tval)
	{
		case TV_AMULET:
			switch(o_ptr->sval)
			{
				case SV_AMULET_WISDOM:
				case SV_AMULET_CHARISMA:
				case SV_AMULET_DEVOTION:
				case SV_AMULET_TRICKERY:
				/* no weaponmastery until +hit/+dam is apparent */
					object_aware(o_ptr);
					object_known(o_ptr);
				break;
			}
			break;
		case TV_RING:
			switch(o_ptr->sval)
			{
				/* ??? MAGIC NUMBERS */
				case SV_RING_WEAKNESS:
					if (k_info[132].aware)
					{
						object_aware(o_ptr);
						object_known(o_ptr);
					}
					break;
				case SV_RING_STR:
					if ((o_ptr->pval > 0) || (k_info[145].aware))
					{
						object_aware(o_ptr);
						object_known(o_ptr);
					}
					break;
				case SV_RING_INT:
					if ((o_ptr->pval > 0) || (k_info[150].aware))
					{
						object_aware(o_ptr);
						object_known(o_ptr);
					}
					break;

				case SV_RING_WOE:
				case SV_RING_DEX:
				case SV_RING_CON:
				case SV_RING_SPEED:
					object_aware(o_ptr);
					object_known(o_ptr);
				break;
			}
			break;
	}
	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		sound(MSG_CURSED);
		msg_print("Oops! It feels deathly cold!");

		/* Sense the object */
		o_ptr->pseudo = INSCRIP_CURSED;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

#ifdef EFG
		/* EFGchange remove need for identify wrto preserving artifacts */
		/* since cannot sense for pseudo, learn artifact now */
		if (o_ptr->name1)
			/* ??? should print message, make a function */
			object_known(o_ptr);

/* EFGchange bugfix */
/* you cannot squelch what you are wearing! */
#else
		/* Set squelched status */
		p_ptr->notice |= PN_SQUELCH;
#endif
	}
	/* EFGchange notice obvious effects */
	else 
	{
		/* keep track if you have tried something on, know not cursed */
		if (!object_known_p(o_ptr) && !(o_ptr->ident & IDENT_SENSE))
		{
			/* ??? this is not pseudoed, inscrip might confuse that 
			if (is_splendid)
				o_ptr->pseudo = INSCRIP_SPLENDID;
			else
			*/
				o_ptr->pseudo = INSCRIP_TRIED;
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

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

	/* Cursed quiver */
	else if (IS_QUIVER_SLOT(item) && p_ptr->cursed_quiver)
	{
		/* Oops */
		msg_print("Your quiver is cursed!");

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
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Cursed quiver */
	else if (IS_QUIVER_SLOT(item) && p_ptr->cursed_quiver)
	{
		/* Oops */
		msg_print("Your quiver is cursed!");

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
void do_cmd_destroy(void)
{
	int item, amt;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[120];
	char out_val[160];

	cptr q, s;

	/* Get an item */
	q = "Destroy which item? ";
	s = "You have nothing to destroy.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | CAN_SQUELCH))) return;

	/* Deal with squelched items */
	if (item == ALL_SQUELCHED)
	{
		squelch_items();
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

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	if ((o_ptr->tval == TV_WAND) ||
	    (o_ptr->tval == TV_STAFF) ||
	    (o_ptr->tval == TV_ROD))
	{
		/* Calculate the amount of destroyed charges */
		i_ptr->pval = o_ptr->pval * amt / o_ptr->number;
	}

	/* Set quantity */
	i_ptr->number = amt;

	/* Describe the destroyed object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Verify destruction */
	strnfmt(out_val, sizeof(out_val), "Really destroy %s? ", o_name);
	if (!get_check(out_val)) return; 

	/* Artifacts cannot be destroyed */
#ifdef EFG
/* ??? why not?  do_cmd_destroy is about ignoring, not actual destruction */
#endif
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
				o_ptr->pseudo = INSCRIP_TERRIBLE;
			else
#ifdef EFG
				/* EFGchange failed destruction ids artifacts */
				object_known(o_ptr);
#else
				o_ptr->pseudo = INSCRIP_SPECIAL;
#endif
		}
		else
		{
			/* Mark the object as indestructible */
#ifdef EFG
			/* EFGchange failed destruction ids artifacts */
			object_known(o_ptr);
#else
			o_ptr->pseudo = INSCRIP_INDESTRUCTIBLE;
#endif
		}

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		p_ptr->redraw |= (PR_EQUIPPY);

		/* Done */
		return;
	}

	/* Message */
	message_format(MSG_DESTROY, 0, "You destroy %s.", o_name);

	/* Reduce the charges of rods/wands/staves */
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

#if 0
	/*
	 * We can only re-enable this when it can be made to interact well with
	 * the repeat code.
	 */

	/* We have destroyed a floor item, and the floor is not empty */
	if ((item < 0) && (cave_o_idx[p_ptr->py][p_ptr->px]))
	{
		/* Automatically repeat this command (unless disturbed) */
		p_ptr->command_cmd = 'k';
		p_ptr->command_rep = 2;
	}
#endif
}


/*
 * Hook to specify enchanted staff
 */
static bool item_tester_hook_enchstaff(const object_type *o_ptr)
{
	if (o_ptr->tval == TV_STAFF)
	{
		/* If there's nothing special about a staff return FALSE */
        if ((o_ptr->to_h == 0) && (o_ptr->to_d == 0) && (o_ptr->to_a == 0) && (!o_ptr->name2) && (!o_ptr->name1))
        {
            return (FALSE);
		}
		/* otherwise staffs return TRUE */
		else return (TRUE);
	}

	return (FALSE);
}


/*
 * Force Stacking of magic staffs
 * (this removes to-hit and to-dam bonuses)
 */
void do_cmd_fstack(void)
{
	int item;

	object_type *o_ptr;
	object_kind *k_ptr;

	cptr q, s;
	
	item_tester_hook = item_tester_hook_enchstaff;
	
	if (!get_check("This will remove all weapon bonuses, are you sure? "))
	{
        return;
    }

	/* Get an item */
	q = "Un-enchant which item? ";
	s = "You have no enchanted staffs.";
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

	k_ptr = &k_info[o_ptr->k_idx];
	
	/* Currently there is only one artifact magic staff, but that may change */
    if (o_ptr->name1)
	{
        msg_print("You cannot unenchant an artifact!");
        return;
    }
	else if (cursed_p(o_ptr))
	{
        msg_print("You must uncurse the staff first.");
        return;
	}

	/* Message */
	msg_print("Staff is now unenchanted.");

	/* Remove the enchantments */
	o_ptr->name2 = 0;
	o_ptr->to_h = 0;
	o_ptr->to_d = 0;
	o_ptr->to_a = 0;
	/* remove possible random stuff from egos like defender */
    o_ptr->xtra1 = 0;
    o_ptr->xtra2 = 0;
	/* remove bonus damage dice */
	if (o_ptr->dd > k_ptr->dd) o_ptr->dd = k_ptr->dd;

	/* you now know that it's average */
	object_known(o_ptr);

	/* Combine the pack */
	p_ptr->notice |= (PN_COMBINE);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
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

	/* Remove the inscription */
	o_ptr->note = 0;

	/* Combine the pack, check for squelchables */
	p_ptr->notice |= (PN_COMBINE | PN_SQUELCH);

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
	tmp[0] = '\0';

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

		/* Combine the pack, check for squelchables */
		p_ptr->notice |= (PN_COMBINE | PN_SQUELCH);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}



/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(const object_type *o_ptr)
{
	u32b f1, f2, f3, f4;

	/* Get flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Flasks of oil are okay */
	if (o_ptr->tval == TV_FLASK) return (TRUE);

	/* Non-empty, non-everburning lanterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_LANTERN) &&
	    (o_ptr->timeout > 0) &&
		!(f3 & TR3_NO_FUEL))
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
	j_ptr->timeout += o_ptr->timeout ? o_ptr->timeout : o_ptr->pval;

	/* Message */
	msg_print("You fuel your lamp.");

	/* Comment */
	if (j_ptr->timeout >= FUEL_LAMP)
	{
		j_ptr->timeout = FUEL_LAMP;
		msg_print("Your lamp is full.");
	}

	/* Refilled from a lantern */
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

		/* Empty a single lantern */
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
	u32b f1, f2, f3, f4;

	/* Get flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* do not use up an artifact torch */
	if (artifact_p(o_ptr)) return (FALSE);

	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_TORCH) &&
		!(f3 & TR3_NO_FUEL))
	{
		return (TRUE);
	}

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
	u32b f1, f2, f3, f4;

	/* Get the light */
	o_ptr = &inventory[INVEN_LITE];

	/* Get flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);


	/* It is nothing */
	if (o_ptr->tval != TV_LITE)
	{
		msg_print("You are not wielding a light.");
	}

	else if (!(f3 & TR3_NO_FUEL))
	{
		/* It's a lamp */
		if (o_ptr->sval == SV_LITE_LANTERN)
		{
			do_cmd_refill_lamp();
		}

		/* It's a torch */
		else if (o_ptr->sval == SV_LITE_TORCH)
		{
			do_cmd_refill_torch();
		}
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
	"%:An Elemental or magma or quartz vein",
	"&:Major Demon", /* "&:unused", */
	"':An open door",
	"(:Soft armor",
	"):A shield",
	"*:A vein with treasure",
	"+:A closed door",
	",:Food (or mushroom patch)",
	"-:A wand (or rod)",
	".:Floor (or dust bunny)",
	"/:A polearm (Axe/Pike/etc) or non-priestly hafted weapon (mace/flail)",
	/* "0:unused", */
	"1:Entrance to General Store",
	"2:Entrance to Armory",
	"3:Entrance to Weaponsmith",
	"4:Entrance to Temple",
	"5:Entrance to Alchemy shop",
	"6:Entrance to Magic store",
	"7:Entrance to Black Market",
	"8:Entrance to your home",
	/* "9:unused", */
	"::Rubble",
	";:A glyph of warding",
	"<:An up staircase",
	"=:A ring",
	">:A down staircase",
	"?:A scroll",
	"@:You or a Dark Elf",
	"A:Ape",
	"B:Bird",
	"C:Canine",
	"D:Ancient Dragon/Wyrm",
	"E:Ent or other Tree Monster",
	"F:Dragon Fly",
	"G:Ghost (or rare Ghost-like non-undead)",
	"H:Hybrid",
	"I:Flying Insect",
	"J:Snake",
	"K:Knight or Mystic Warrior",
	"L:Lich",
	"M:Multi-Headed Reptile",
	"N:Null (acidic tentacled swamp monster)", /* "N:unused", */
	"O:Ogre",
	"P:Giant Humanoid",
	"Q:Quylthulg (Pulsing Flesh Mound)",
	"R:Amphibian",
	"S:Spider/Scorpion/Tick",
	"T:Troll",
	"U:Devil",
	"V:Vampire",
	"W:Wight/Wraith/etc",
	"X:Xorn or other Minor Elemental",
	"Y:Unicorn or Centaur",
	"Z:Zephyr Hound",
	"[:Hard armor",
	"\\:A hafted weapon (quarterstaff/hammer/etc)",
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
	"h:Humanoid (mostly hobbit or dwarf)",
	"i:Implike demon or dark fairy",
	"j:Jelly",
	"k:Kobold",
	"l:Lizard",
	"m:Mold",
	"n:Naga",
	"o:Orc",
	"p:Person/Human",
	"q:Quadruped",
	"r:Rodent",
	"s:Skeleton",
	"t:Human (mostly Townspeople)",
	"u:Minor Demon",
	"v:Vortex",
	"w:Worm/Worm-Mass",
	"x:Gargoyle", /* "x:unused", */
	"y:Light Fairy",
	"z:Zombie/Mummy",
	"{:A missile (arrow/bolt/shot)",
	"|:An edged weapon (sword/dagger/etc)",
	"}:A launcher (bow/crossbow/sling)",
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
 * Note that the player ghosts are ignored, since they do not exist.
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
		if (!cheat_know && !l_ptr->sights) continue;

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
				screen_roff(who[i], 0);

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
				i = 0;
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
				i = n - 1;
		}
	}


	/* Re-display the identity */
	prt(buf, 0, 0);

	/* Free the "who" array */
	FREE(who);
}
