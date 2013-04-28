
/* File: cmd3.c */

/*
 * Display inventory and equipment.  Wear and remove equipment.  Drop and
 * destroy.  Looking around.  Inspecting, inscribing, refueling and lighting
 * objects.  Learn about a symbol.  Stealing and trap setting.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Display inventory
 */
void do_cmd_inven(void)
{
	char string[DESC_LEN];

	/* Note that we are in "inventory" mode */
	p_ptr->command_wrk = (USE_INVEN);


	/* Save screen */
	screen_save(FALSE);

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the inventory */
	show_inven();

	/* Hack -- hide empty slots */
	item_tester_full = FALSE;


	/* Insert the total burden and character capacity into a string. */
	if (use_metric) (void)strnfmt(string, sizeof(string),
		"(Inventory) burden %d.%d kg (%d%% of capacity). Command: ",
		make_metric(p_ptr->total_weight) / 10,
		make_metric(p_ptr->total_weight) % 10,
		p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);
	else (void)strnfmt(string, sizeof(string),
		"(Inventory) burden %d.%d lb (%d%% of capacity). Command: ",
		p_ptr->total_weight / 10, p_ptr->total_weight % 10,
		p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);


	/* Output that string, and prompt for a command. */
	prt(string, 0, 0);

	/* Hack -- Get a new command */
	p_ptr->command_new = inkey(FALSE);

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
	char string[DESC_LEN];

	/* Note that we are in "equipment" mode */
	p_ptr->command_wrk = (USE_EQUIP);


	/* Save screen */
	screen_save(FALSE);

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	show_equip();

	/* Hack -- undo the hack above */
	item_tester_full = FALSE;


	/* Insert the total burden and character capacity into a string. */
	if (use_metric)
	{
		(void)strnfmt(string, sizeof(string),
			"(Equipment) burden %d.%d kg (%d%% of capacity). Command: ",
			make_metric(p_ptr->total_weight) / 10,
			make_metric(p_ptr->total_weight) % 10,
			p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);
	}
	else
	{
		(void)strnfmt(string, sizeof(string),
			"(Equipment) burden %d.%d lb (%d%% of capacity). Command: ",
			p_ptr->total_weight / 10, p_ptr->total_weight % 10,
			p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);
	}


	/* Output that string, and prompt for a command. */
	prt(string, 0, 0);


	/* Hack -- Get a new command */
	p_ptr->command_new = inkey(FALSE);

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
 * The "wearable" tester when shapechanged
 */
bool item_tester_hook_wear_shapechange(const object_type *o_ptr)
{
	int slot = wield_slot(o_ptr);

	/* Check for a usable slot */
	if (slot >= INVEN_WIELD)
	{
		/* Allow various wield slots */
		if (slot == INVEN_WIELD) return (TRUE);
		if (slot == INVEN_Q1)    return (TRUE);
		if (slot == INVEN_BOW)   return (TRUE);

		/* Allow light sources */
		if (slot == INVEN_LITE)   return (TRUE);
	}

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
 * Activate hidden curse.  Return TRUE if equipping should continue,
 * FALSE if it should stop.  -LM-
 */
static bool activate_hidden_curse(object_type *o_ptr)
{
 	int chance;

	char o_name[DESC_LEN];
	cptr act;

	u32b f1, f2, f3;


	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Paranoia */
	if (!(f3 & (TR3_CURSE_HIDDEN))) return (TRUE);


	/*
	 * If the object is a weapon with slays or brands, it has a 50% chance
	 * of simply gaining an unannounced curse.  See "dungeon.c".
	 */
	if (is_any_weapon(o_ptr) && ((f1 & TR1_BRAND_MASK) || (f1 & TR1_SLAY_MASK)) && (one_in_(2)))
	{
		/* Remove the hidden curse */
		o_ptr->flags3 &= ~(TR3_CURSE_HIDDEN);

		/* Add a light curse (but do not mark the item as "cursed") */
		o_ptr->flags3 |= (TR3_LIGHT_CURSE);

		/* Done */
		return (TRUE);
	}

	/* 25% chance for weapons to gain a nasty flag */
	if (is_any_weapon(o_ptr) && one_in_(2))
	{
		if      (one_in_(3)) o_ptr->flags3 |= TR3_SOULSTEAL;
		else if (one_in_(2)) o_ptr->flags3 |= TR3_TELEPORT;
		else                 o_ptr->flags3 |= TR3_AGGRAVATE;
		return (TRUE);
	}

	/* 33% chance of armor to gain a nasty flag */
	if (is_any_armor(o_ptr) && one_in_(3))
	{
		if      (one_in_(3)) o_ptr->flags3 |= TR3_DRAIN_HP;
		else if (one_in_(2)) o_ptr->flags3 |= TR3_DRAIN_EXP;
		else if (one_in_(2)) o_ptr->flags3 |= TR3_NOMAGIC;
		else if (one_in_(2)) o_ptr->flags3 |= TR3_TELEPORT;
		else                 o_ptr->flags3 |= TR3_AGGRAVATE;

		return (TRUE);

	}
	/* The object horribly mutates!  (keep equipping on failure) */
	else if (!make_cursed_ego_item(o_ptr)) return (TRUE);


	/* Identify the object XXX XXX */
	object_aware(o_ptr);
	object_known(o_ptr);


 	/* We aren't paying attention */
	if ((p_ptr->image) || (p_ptr->confused) || (p_ptr->berserk))
	{
		/* Object has a perfect chance of avoiding detection */
		chance = 100;
	}

	/* We can use skills to save ourself */
	else
	{
		/* Chance of object avoiding detection ranges from 100% down to 25% */
		chance = 80;
		chance = div_round(100 * chance, get_skill(S_PIETY, 100, 200));
		chance = div_round(100 * chance, get_skill(S_PERCEPTION, 100, 200));
		chance = div_round(100 * chance, get_skill(S_SAVE, 100, 200));
		chance = div_round(100 * chance, get_skill(S_INFUSION, 100, 200));
		chance += 20;
	}


	/* Roll for detection */
	if ((chance < 100) && (chance < randint(100)))
	{
		int discover_skill;

		/* Character suddenly realizes his danger */

		/* Tally up and randomize total skill */
		discover_skill = randint(
			get_skill(S_PIETY, 0, 100) +
			get_skill(S_PERCEPTION, 0, 100) +
			get_skill(S_SAVE, 0, 100) +
			get_skill(S_INFUSION, 0, 100));

		/* Divine intuition */
		discover_skill -= get_skill(S_PIETY, 0, 100);
		if (discover_skill <= 0)
			message(MSG_L_RED, 0, "You sense something deeply unholy... ");

		/* 'Twas perception what done it */
		discover_skill -= get_skill(S_PERCEPTION, 0, 100);
		if (discover_skill <= 0)
			message(MSG_L_RED, 0, "Suddenly, you pause and stare closely... ");

		/* It was the saving throw */
		discover_skill -= get_skill(S_SAVE, 0, 100);
		if (discover_skill <= 0)
			message(MSG_L_RED, 0, "You sense danger... ");

		/* A keen nose for magic */
		else message(MSG_L_RED, 0, "You feel terrible magiks... ");

		/* Caught! */
		msg_print("Foul curses reveal themselves!  You have saved yourself just in time!");


		/* Describe the object (briefly) */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Describe action */
		if (is_any_weapon(o_ptr)) act = "wielded";
		else if (is_missile(o_ptr)) act = "loaded";
		else act = "put on";

		/* Whew */
		msg_format("You have NOT %s the %s.", act, o_name);

		/* Cancel the equipping */
		return (FALSE);
	}

	/* We failed to stop in time */
	else
	{
		/* Get name of object kind */
		strip_name(o_name, o_ptr->k_idx);

		/* Describe action */
		if (is_any_weapon(o_ptr)) act = "wield";
		else if (is_missile(o_ptr)) act = "load";
		else act = "put on";


		/* Oh, no. */
		message(MSG_L_RED, 200, format("NO!  As you %s the %s, it changes horribly!", act, o_name));
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1 | PW_OBJECT);


	/* AAARGH! */
	return (TRUE);
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

	char o_name[DESC_LEN];

	bool remove_two_weapons = FALSE;
	bool hidden_curse = FALSE;
	bool replace_primary_weapon = FALSE;

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_wear;

	/* Shapechanges forbid any armor wearing */
	if (p_ptr->schange)
	{
		/* Restrict the choices more rigorously */
		item_tester_hook = item_tester_hook_wear_shapechange;
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

	/* Ask for ring to replace  -TNB- */
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

	/* Ask for wield or quivering of special throwing weapons, default to yes */
	if ((is_melee_weapon(o_ptr)) && (f1 & (TR1_THROWING)))
	{
		if (!get_check_default("Put this throwing weapon in the quiver?", TRUE))
			slot = INVEN_WIELD;
	}


	/* Wielding something (usually a weapon) */
	if (slot == INVEN_WIELD)
	{
		/* New weapon requires two hands */
		if (needs_two_hands(f1, o_ptr->weight))
		{
			int num_weapons = 0;

			/* Count existing weapons */
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

		/* Weapon needs only one hand */
		else
		{
			/* Primary wield slot is occupied */
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
					slot2_ok = TRUE;
				}

				/* Get item in secondary wield slot */
				i_ptr = &inventory[INVEN_ARM];

				/* Anything but a melee weapon allows wielding in slot 1 */
				if (!is_melee_weapon(i_ptr)) slot1_ok = TRUE;

				/* Weapon in the secondary wield slot is light enough */
				if (o_ptr->weight + i_ptr->weight <= hold)
				{
					slot1_ok = TRUE;
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
					replace_primary_weapon = TRUE;
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
					if (slot == INVEN_WIELD) replace_primary_weapon = TRUE;
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

		/* Watch out for an offhand cursed weapon when removing both  -SKY- */
		if (remove_two_weapons && cursed_cling(&inventory[INVEN_ARM]))
		{
			slot = INVEN_ARM;
		}
	}


	/* Prevent wielding into a cursed slot (ignore the quiver) */
	if ((slot < INVEN_Q1) && (cursed_cling(&inventory[slot])))
	{
		/* Describe it */
		object_desc(o_name, sizeof(o_name), &inventory[slot], FALSE, 0);

		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
			   o_name, describe_use(slot));

		/* Cancel the command */
		return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Usually, we wear or wield only one item. */
	num = 1;

	/* Missiles go in quiver slots, which have special rules. */
	if (slot == INVEN_Q1)
	{
		int ammo_num, add_num;

		/* Get a quantity, save it */
		get_quantity_default = o_ptr->number;
		num = add_num = (int)get_quantity(NULL, 0, o_ptr->number);

		/* Allow cancel */
		if (!add_num) return;

		/* Adjust "number" for item size in quiver */
		add_num = quiver_count_item(o_ptr, num);

		/* Count number of missiles in the quiver slots. */
		ammo_num = quiver_count();

		/*
		 * If the ammo now being added will make the quiver take up another
		 * backpack slot, and there are none available, refuse to wield the
		 * new ammo.
		 */
		if ((ammo_num + add_num) > 99 * p_ptr->pack_size_reduce)
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

		/* Find a slot that can hold more items */
		slot = process_quiver(o_ptr->number, o_ptr);

		/* Note lack of space */
		if (!slot)
		{
			/* No space */
			msg_print("Your quiver is full.");
			return;
		}

		/* Quiver will be reorganized (again) later. */
		p_ptr->notice |= (PN_COMBINE);
	}

	/* Activate hidden curses, which may or may not be caught in time */
	if (o_ptr->flags3 & (TR3_CURSE_HIDDEN))
	{
		if (!activate_hidden_curse(o_ptr)) return;

		/* No further messages or effects */
		hidden_curse = TRUE;
	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

	if (slot >= INVEN_Q1 && slot <= INVEN_Q0)
	{
		/* Remember that the item is quivered */
		i_ptr->quivered = TRUE;
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
		(void)inven_takeoff(INVEN_ARM, 255);
		(void)switch_weapons(TRUE);   /* Hack -- delay removing second weapon to prevent object loss */
	}
	/* Hack -- Replacing the primary weapon sometimes moves the secondary weapon -- move it back */
	else if (replace_primary_weapon)
	{
	    (void)inven_takeoff(slot, 255);
	    if (is_melee_weapon(&inventory[INVEN_WIELD])) (void)switch_weapons(TRUE);
	}

	/* Removal of one item */
	else
	{
		/* Handle existing item. */
		if (o_ptr->k_idx)
		{
			/* Take off existing item, unless in the quiver. */
			if ((slot < INVEN_Q1) || (slot > INVEN_Q0))
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

	/* Object has no location */
	i_ptr->iy = i_ptr->ix = 0;

    /* Hack -- delay removing the second weapon */
    if (remove_two_weapons)
        (void)inven_takeoff(INVEN_ARM, 255);


	/* Increment the equip counter by hand */
	p_ptr->equip_cnt++;

	/* Automatically turn on light sources */
	if (o_ptr->tval == TV_LITE) o_ptr->flags3 |= (TR3_IS_LIT);


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
	else if ((slot >= INVEN_Q1) && (slot <= INVEN_Q0))
	{
		act = "You have readied";
	}
	else
	{
		act = "You are wearing";
	}

	/* Describe the result */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Message */
	sound(MSG_WIELD);
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
	if (cursed_cling(o_ptr))
	{
		sound(MSG_CURSED);

		/* Special case -- the One Ring */
		if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_POWER))
		{
			/* Message */
			message(MSG_L_PURPLE, 200,
				"The ring fuses to your finger!  You feel a terrible power...");

			/* The One Ring is a great burden  XXX XXX */
			o_ptr->weight = 100;
		}

		/* Handle other cursed objects */
		else if (!hidden_curse)
		{
			if (f1 & (TR3_HEAVY_CURSE | TR3_PERMA_CURSE))
			{
				message(MSG_L_RED, 200, "OH NO!  It feels deathly cold!");
			}
			else
			{
				message(MSG_YELLOW, 50, "It feels deathly cold!");
			}
		}

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
			/* We force inscriptions of "average", if appropriate */
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


	/* Shapechanges forbid any taking off of armor */
	if (p_ptr->schange)
	{
		/* Restrict the choices more rigorously */
		item_tester_hook = item_tester_hook_wear_shapechange;
	}

	/* Get an item */
	q = "Take off which item?";
	s = "You are not wearing anything to take off.";
	if (!get_item(&item, q, s, (USE_EQUIP))) return;
	item_to_object(o_ptr, item);

	/* Item is cursed */
	if (cursed_cling(o_ptr))
	{
		/* Special case -- the One Ring */
		if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_POWER))
		{
			msg_print("The One Ring resists all attempts to remove it!");
		}

		/* Oops */
		else msg_print("Hmmm, it seems to be cursed.");

		/* Note the curse */
		o_ptr->ident |= (IDENT_SENSE);
		if ((o_ptr->inscrip != INSCRIP_VERY_CURSED) &&
		    (o_ptr->inscrip != INSCRIP_TERRIBLE))
		{
			o_ptr->inscrip = INSCRIP_CURSED;
		}

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
	amt = (int)get_quantity(NULL, 0, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_WIELD) && cursed_cling(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Note the curse */
		o_ptr->ident |= (IDENT_SENSE);
		if ((o_ptr->inscrip != INSCRIP_VERY_CURSED) &&
		    (o_ptr->inscrip != INSCRIP_TERRIBLE))
		{
			o_ptr->inscrip = INSCRIP_CURSED;
		}

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
	int item, amt = 0;

	object_type *o_ptr = NULL;
	object_type *i_ptr;
	object_type object_type_body;

	char o_name[DESC_LEN];
	char out_val[DESC_LEN];

	cptr q, s;


	/* Get an item */
	q = "Destroy which item?";
	s = "You have nothing to destroy.";
	if (get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
	{
		/* Get the object */
		item_to_object(o_ptr, item);

		/* Get a quantity */
		amt = (int)get_quantity(NULL, 0, o_ptr->number);
	}

	/* Assume command is not repeated */
	p_ptr->command_rep = 0;

	/* Allow user abort */
	if ((amt <= 0) || (!o_ptr)) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Object is a magical device */
	if (is_magical_device(o_ptr))
	{
		/* Calculate the amount of destroyed charges */
		i_ptr->pval = o_ptr->pval * amt / o_ptr->number;
	}

	/* Set quantity */
	i_ptr->number = amt;

	/* Describe the destroyed object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Verify destruction */
	if (verify_destroy)
	{
		(void)strnfmt(out_val, sizeof(out_val), "Really destroy %s?", o_name);
		if (!get_check(out_val)) return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Artifacts cannot be destroyed */
	if (artifact_p(o_ptr))
	{
		/* Message */
		msg_format("You cannot destroy %s.", o_name);

		/* Don't mark id' ed objects */
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
	message_format(MSG_DESTROY, 0, "You destroy %s.", o_name);

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


	/* We have destroyed a floor item, and the floor is not empty */
	if ((item < 0) && (cave_o_idx[p_ptr->py][p_ptr->px]))
	{
		/* Automatically repeat this command (unless disturbed) */
		p_ptr->command_cmd = 'k';
		p_ptr->command_rep = 2;
	}
}

/*
 * Display specialized object information.  -LM-
 *
 * Unidentified:
 *	Weapons and Armor -> description of specific object type (dagger,
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
	int item = -1;

	u16b aware, known, mental;

	object_kind *k_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	char o_name[DESC_LEN];

	char info_text[1024];
	char object_kind_info[1024];

	cptr q, s;

	int old_cursor_vis;


	/* Initialize object description. */
	strcpy(info_text, "");

	/* Get an object to inspect if one is not already supplied */
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


	/* What is our level of knowledge about the object? */
	aware = object_aware_p(i_ptr);
	known = (object_known_p(i_ptr));
	mental = (i_ptr->ident & (IDENT_MENTAL));

	if (in_store) known = mental = TRUE;


	/* Describe the object */
	if ((in_store) || (known && !aware))
		object_desc_store(o_name, sizeof(o_name), i_ptr, FALSE, 2);
	else
		object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 2);


	/*
	 * Hack - Avoid giving away too much info in normal stores about objects
	 * other than weapons and armor (no sneaky learning about wand damages!).
	 */
	if ((in_store) && (!(k_ptr->special & (SPECIAL_KNOWN_EFFECT))) &&
		(!is_wargear(k_ptr)))
	{
		mental = FALSE;
	}

	/* Object is fully known - give maximal information. */
	if (mental)
	{
		/* Get the specific object type's information. */
		object_info(info_text, i_ptr, TRUE);

		/* No object kind info. */
		strcpy(object_kind_info, "");
	}

	/* Object or object type is identified - show all basic information. */
	else if ((known) || (aware))
	{
		/* Get the specific object type's information, if any. */
		object_info(info_text, i_ptr, object_aware_p(i_ptr));

		/* Get information about the general object kind. */
		(void)my_strcpy(object_kind_info, format("%s", obj_class_info[i_ptr->tval]),
			sizeof(object_kind_info));
	}

	/*
	 * Nothing is known about the object or object type - show only
	 * information about the general object kind.
	 */
	else
	{
		/* Get information about the general object kind. */
		(void)my_strcpy(object_kind_info, format("%s", obj_class_info[i_ptr->tval]),
			sizeof(object_kind_info));

		/* Do not display the name of an unaware object  XXX */
		if (!object_aware_p(i_ptr)) strcpy(o_name, "  ");
	}

	/* Save and center screen */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_CX, 80, 0);

	/* Set up a 1-column left border  XXX XXX */
	text_border_left = 1;

	/* Clear some space */
	clear_space(0, 0, 80);
	clear_space(1, 0, 80);
	clear_space(2, 0, 80);

	/* Move cursor to top-left corner */
	(void)Term_gotoxy(0, 0);

	/* Label the information. */
	c_roff_centered(TERM_WHITE, format("Item Information:  %s\n---\n", o_name), 0, 80);

	/* Object type or artifact information. */
	c_roff(TERM_L_BLUE, info_text, 0, 80);


	/* Object has (unusual) hidden qualities  XXX XXX */
	if ((!mental) &&
	   ((o_ptr->flags1 & ~(k_ptr->flags1)) ||
		(o_ptr->flags2 & ~(k_ptr->flags2)) ||
		((o_ptr->flags3 & ~(k_ptr->flags3)) &&
		 (((o_ptr->flags3 & ~(k_ptr->flags3)) != (TR3_LIGHT_CURSE))))))
	{
		char buf_sname[DESC_LEN];

		strip_name(buf_sname, o_ptr->k_idx);

		c_roff(TERM_L_BLUE, format("  This %s may possess hidden qualities.", buf_sname), 0, 80);
	}


	/*  Describe artifacts that belong to sets. */
	if ((known) && (i_ptr->artifact_index))
	{
		artifact_type *a_ptr = &a_info[i_ptr->artifact_index];

		/* Is it a set item? */
		if (a_ptr->set_index)
		{
			/* Advance two lines */
			roff("\n\n", 0, 80);

			/* Set notification */
			c_roff(TERM_GREEN,"Set Item: ", 0, 80);

			/* Require full ID to describe the specific set */
			if (mental)
			{
				set_type *s_ptr = &s_info[a_ptr->set_index];
				c_roff(TERM_GREEN, s_ptr->set_desc, 0, 80);
			}

			/* Generic description */
			else c_roff(TERM_GREEN,"It gains power when combined with matching items", 0, 80);

			c_roff(TERM_GREEN, ".", 0, 0);
		}
	}


	/* Spacing */
	roff("\n\n\n", 0, 80);

	/* Fully describe the object. */
	object_details(i_ptr, mental, known);

	/* Object is not fully identified */
	if (!mental)
	{
		/* Spacing */
		roff("\n\n", 0, 80);

		/* Object kind information. */
		roff(object_kind_info, 0, 80);
	}

	/* Spacing */
	roff("\n", 0, 80);

	/* Clear left border request */
	text_border_left = 0;


	/* Save the previous main screen cursor visibility */
	old_cursor_vis = inkey_cursor_hack[TERM_MAIN];

	/* Hide the cursor on the main screen  XXX XXX */
	inkey_cursor_hack[TERM_MAIN] = -1;

	/* Wait for it */
	(void)inkey(ALLOW_CLICK);

	/* Restore old cursor visibility */
	inkey_cursor_hack[TERM_MAIN] = old_cursor_vis;

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);
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

	char o_name[DESC_LEN];

	char tmp[DESC_LEN];

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
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Message */
	msg_format("Inscribing %s.", o_name);
	message_flush();

	/* Start with nothing */
	strcpy(tmp, "");

	/* Use old inscription */
	if (o_ptr->note)
	{
		/* Start with the old inscription */
		(void)strnfmt(tmp, sizeof(tmp), "%s", quark_str(o_ptr->note));
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
	u32b f1, f2, f3;

	/* Get the light */
	o_ptr = &inventory[INVEN_LITE];

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* It is nothing */
	if (o_ptr->tval != TV_LITE)
	{
		msg_print("You are not wielding a light.");
	}

	/* It doesn't need fuel */
	else if (f3 & (TR3_NOFUEL))
	{
		msg_print("Your light cannot be refilled.");
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
 * An "item_tester_hook" for light sources which can be lit and doused.
 *
 * We do not allow other shining objects to be doused.  A blazing sword
 * always blazes.
 */
bool item_tester_light_source(const object_type *o_ptr)
{
	/* Return "is a light source" */
	return (o_ptr->tval == TV_LITE);
}

/*
 * Light or douse a light source.
 */
void do_cmd_light_and_douse(void)
{
	int item;

	object_type *o_ptr;
	char o_name[DESC_LEN];

	cptr own_str = "";

	cptr q, s;


	/* Restrict the choices */
	item_tester_hook = item_tester_light_source;

	/* Get an item (not in the backpack) */
	q = "Light or douse which light source?";
	s = "You have no light sources.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_FLOOR | USE_AUTO))) return;
	item_to_object(o_ptr, item);

	/* Get an object description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);

	/* Get an ownership string */
	if (item >= 0) own_str = "your";
	else if (o_ptr->number != 1) own_str = "some";
	else own_str = "the";


	/* Take a partial turn */
	p_ptr->energy_use = 50;


	/* Light the light source */
	if (!(o_ptr->flags3 & (TR3_IS_LIT)))
	{
		msg_format("You light %s %s.", own_str, o_name);
		o_ptr->flags3 |= (TR3_IS_LIT);
	}

	/* Douse the light source */
	else
	{
		msg_format("You douse %s %s.", own_str, o_name);
		o_ptr->flags3 &= ~(TR3_IS_LIT);
	}

	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
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
void do_cmd_look(u16b mode)
{
	/* Look around */
	if (target_set_interactive(TARGET_LOOK | mode))
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

	char tmp_val[DESC_LEN];
	char out_val[DESC_LEN];

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
			(void)strnfmt(tmp_val, sizeof(tmp_val), "%s%s of",
				((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
				((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));
		}

		/* Prepare to ask which way to look */
		(void)strnfmt(out_val, sizeof(out_val),
			"Map sector [%d,%d], which is%s your sector.  Direction?",
			(y2 / get_panel_hgt()), (x2 / get_panel_wid()), tmp_val);

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
	char buf[DESC_LEN];

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
		/* We are showing a pop-up help window */
		if (screen_depth)
		{
			put_str(format(" %-46s", "Press:"), 1, 5);
			put_str(format(" %-46s", "Control-A for the full monster list"), 2, 5);
			put_str(format(" %-46s", "Control-U for the unique monster list"), 3, 5);
			put_str(format(" %-46s", "Control-N for the non-unique monster list"), 4, 5);
			put_str(format(" %-46s", "Control-K for the killed monster list"), 5, 5);
			put_str(format(" %-46s", "ESCAPE to cancel"), 6, 5);
			put_str(format(" %-46s", "RETURN to show this help window, or"), 7, 5);
			put_str(format(" %-46s", "Any other key to display information about it"), 8, 5);
			put_str(format(" %-46s", ""), 9, 5);
		}

		/* Get a character, or abort */
		if (!get_com("Enter character to be identified (RET for help, ESC to cancel):", &sym))
		{
			if (screen_depth) screen_load();
			return;
		}

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
			if (!screen_depth) screen_save(FALSE);
			else               screen_load();
		}
		else
		{
			/* Try to match the key with an ident_info entry */
			for (i = 0; ident_info[i]; ++i)
			{
				if (sym == ident_info[i][0]) break;
			}
			if (ident_info[i])
			{
				(void)strnfmt(buf, sizeof(buf), "%c - %s.", sym, ident_info[i] + 2);
				break;
			}
			else if (!isalpha(sym))
			{
				msg_print("This command was not understood.");
				message_flush();
			}
		}
	}

	/* Close pop-up window */
	if (screen_depth) screen_load();


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
	put_str("Recall details? (y/n, (k)ills, (l)evel)", 0, 40);

	/* Query */
	query = inkey(FALSE);

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


	/* We're using the standard order, or sorting by level */
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
		(void)Term_addstr(-1, TERM_WHITE, "[(r)ecall, ESC]");
		prt(format("(#%d of %d)", i + 1, n), 0 , 65);

		/* Interact */
		while (TRUE)
		{
			/* Recall */
			if (recall)
			{
				/* Save screen */
				screen_save(FALSE);

				/* Recall on screen */
				screen_roff(who[i]);

				/* Hack -- Complete the prompt (again) */
				(void)Term_addstr(-1, TERM_WHITE, "[un(r)ecall, ESC]");
				prt(format("(#%d of %d)", i + 1, n), 0 , 65);

			}

			/* Command */
			query = inkey(FALSE);

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
		    ((rogue_like_commands) && (query == 'j')))
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
	"'My treasure is costly, thief!'",
	"'Your blood for my gold?  Agreed!'",
	"'I scrimp, I save, and now it's gone!'",
	"'Robbers like you are part of the problem!'",
	"'Banditti!  This dungeon's just not safe anymore!'",
	"'Ruined!  I'm ruined!'",
	"'Where, where is the common decency?'",
	"'Your knavish tricks end here and now!'"
};


/*
 * Rogues may steal objects and gold from monsters.  -LM-
 *
 * Steal too often on a level, and monsters will be more wary,
 * and the hue and cry will be eventually be raised.  Having every
 * monster on the level awake and aggravated is not pleasant.
 */
void py_steal(int y, int x)
{
	monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char m_name[DESC_LEN];

	int i;
	int filching_power, theft_protection;

	bool success = FALSE;
	bool purse = FALSE;
	bool undetected = FALSE;


	/* Fair warning */
	if (num_recent_thefts > get_skill(S_STEALTH, 3, 5))
	{
		msg_print("You dare not steal anything with the dungeon in such an uproar.");
		p_ptr->energy_use = 0;
		return;
	}
	else if (num_recent_thefts == get_skill(S_STEALTH, 3, 5))
	{
		if (!get_check("Everyone is keeping a lookout for you.  Are you sure you want to steal again so soon?"))
		{
			p_ptr->energy_use = 0;
			return;
		}
	}


	/* Determine the cunning of the thief. */
	filching_power = get_skill(S_BURGLARY, 0, 130);

	/* Dexterity helps (total max of 150) */
	filching_power += (adj_dis[p_ptr->stat_ind[A_DEX]] * 2) - 256;


	/* Determine how much protection the monster has. */
	theft_protection = r_ptr->level + 5;

	/* Adjust for speed difference */
	theft_protection += (m_ptr->mspeed - p_ptr->pspeed) * r_ptr->level / 40;

	/* Send a thief to catch a thief. */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
	{
		/* Extract information about the blow effect */
		int effect = r_ptr->blow[i].effect;
		if ((effect == RBE_EAT_GOLD) || (effect == RBE_EAT_ITEM))
		{
			theft_protection += 30;
		}
	}

	/* Sleeping monsters are easier to steal from */
	if (m_ptr->csleep)
	{
		theft_protection = theft_protection * 2 / 3;
	}

	/* It is good to steal in the dark */
	if (no_light())
	{
		theft_protection = theft_protection * 2 / 3;
	}

	/* Invisibility helps. */
	if (player_invis(m_ptr, FALSE))
	{
		theft_protection = theft_protection * 2 / 3;
	}

	/* The more you steal on a level, the more alert the monsters. */
	theft_protection += num_recent_thefts * 10;

	/* Wary monsters are harder to steal from  -JG */
	if (monster_wary(m_ptr)) theft_protection = 3 * theft_protection / 2;

	/* Creatures with remarkable drops protect their items  -JG */
	if (r_ptr->flags1 & (RF1_DROP_CHEST))
		theft_protection = 20 + (theft_protection * 2);
	else if (r_ptr->flags1 & (RF1_DROP_GREAT))
		theft_protection = 5 + (theft_protection * 3 / 2);
	else if (r_ptr->flags1 & (RF1_DROP_GOOD))
		theft_protection = (theft_protection * 4 / 3);

	/* Did the theft succeed? */
	if (rand_range(filching_power / 3, filching_power) > theft_protection)
		success = TRUE;


	/* Success -- search for loot */
	if (success)
	{
		/* Attempt to take an object or gold from the monster */
		if (!monster_loot(1, TRUE, m_ptr))
		{
			/* Pockets are empty. */
			msg_print("You burgle only dust.");
		}
		else purse = TRUE;
	}


	/* The victim normally, but not always, wakes up and is aggravated. */
	if ((success && one_in_(2)) || (!success && !one_in_(4)))
	{
		/* Wake up and go active */
		m_ptr->csleep = 0;
		m_ptr->mflag |= (MFLAG_ACTV);

		/* Haste up */
		if (m_ptr->hasted < 5) m_ptr->hasted = 5;

		/* Become wary */
		mon_make_wary(m_ptr);

		/* Occasionally, amuse the player with a message. */
		if ((one_in_(4)) && (purse) && (r_ptr->flags2 & (RF2_SMART)))
		{
			char act[DESC_LEN];
			monster_desc(m_name, m_ptr, 0x40);
			strcpy(act, desc_victim_outcry[rand_int(OUTCRY_MAX)]);
			msg_format("%^s cries out %s", m_name, act);
		}

		/* Otherwise, simply explain what happened. */
		else
		{
			monster_desc(m_name, m_ptr, 0x40);
			if (success)
				message_format(MSG_YELLOW, 0, "You have angered %s.", m_name);
			else
				message_format(MSG_YELLOW, 0, "%^s reacts quickly, and you are caught red-handed!", m_name);
		}

		/* Increment the number of noticed thefts. */
		num_recent_thefts++;
	}

	/* Got clean away */
	else if (success && purse)
	{
		msg_print("Your theft goes undiscovered!");
		undetected = TRUE;
	}

	/* Failed to steal, but at least didn't get caught */
	else if (!success)
	{
		msg_print("You fail to steal anything, but luckily avoid detection.");
		undetected = TRUE;
	}

	/* Gain some practice in Burglary */
	if (purse)
	{
		practice_skill(5 + p_ptr->depth * p_ptr->depth / 3, S_BURGLARY);
	}


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();


	/* If detected, other monsters may notice too */
	if (undetected == FALSE)
	{
		/* Lots of stealing */
		if (num_recent_thefts > get_skill(S_STEALTH, 3, 5))
		{
			/* Aggravate and speed up all monsters on level. */
			aggravate_monsters(-1, TRUE,
				"The hue and cry sounds!  The entire dungeon is in an uproar!");

			/* Any monster on the level that can become wary does */
			(void)make_monsters_wary(y, x, FALSE, FALSE);
		}

		/* A moderate amount of stealing */
		else if ((num_recent_thefts > get_skill(S_STEALTH, 1, 3)) || (one_in_(8)))
		{
			/* Aggravate monsters nearby */
			aggravate_monsters(-1, FALSE,
				"You hear hunting parties scouring the area for a notorious burglar.");

			/* Monsters in LOS or in earshot may become wary */
			(void)make_monsters_wary(y, x, TRUE, FALSE);
		}
	}


	/* Rogue "Hit and Run" attack. */
	if (p_ptr->special_attack & (ATTACK_FLEE))
	{
		/* Cancel the fleeing spell */
		p_ptr->special_attack &= ~(ATTACK_FLEE);

		/* Message */
		msg_print("You escape into the shadows!");

		/* Teleport */
		teleport_player(get_skill(S_BURGLARY, 6, 12), TRUE, FALSE);

		/* Print "special attacks" */
		left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

		/* Redraw conditions status */
		p_ptr->redraw |= (PR_CONDITIONS);
	}

	/* An angel has been robbed by a priest (!) */
	if ((p_ptr->realm == PRIEST) && (r_ptr->d_char == 'A'))
	{
		/* This is a (sort of) bad thing */
		set_unsanctified(p_ptr->unsanctified + rand_range(25, 50));
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
	if (num_trap_on_level >= PLAYER_ALLOWED_TRAPS)
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
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return (FALSE);
	}
	if (p_ptr->image)
	{
		msg_print("You are hallucinating!");
		return (FALSE);
	}
	if (p_ptr->berserk)
	{
		msg_print("You are too berserk!");
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
