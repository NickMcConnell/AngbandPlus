#define CMD6_C
/* File: cmd6.c */

/* Purpose: Object commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * This file includes code for eating food, drinking potions,
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
 * Note that food/potions/scrolls no longer use bit-flags for effects,
 * but instead use the "sval" (which is also used to sort the objects).
 */


/*
 * Find the expected number of the power appropriate for an object.
 * As the appropriate power can be derived from one of several places, some
 * of these are offset to separate them. This gives the following ranges:
 *
 * -1023--769: spell power (byte)
 * -768--513: tval (byte, not determined here)
 * -511--257: name1 (byte)
 * -255--1: activation (byte)
 * 1-32767: k_idx (s16b)
 */
static int get_power(object_ctype *o_ptr)
{
	if (o_ptr->activation)
	{
		return PO_ACTIVATION + o_ptr->activation;
	}
	else if (o_ptr->name1)
	{
		return PO_NAME1 + o_ptr->name1;
	}
	else
	{
		return PO_K_IDX + o_ptr->k_idx;
	}
}

/*
 * Use an object of some kind, and become aware of it if it has demonstrated
 * what kind of object it is.
 * Return TRUE if it has been used, FALSE if it may not have been.
 */
static bool use_object(object_type *o_ptr, int dir)
{
	bool ident, use;
	int power = get_power(o_ptr);

	/* "detect traps" marks the area it has been used in if the effect is
	 * known. This is always true if the object was previously known to have
	 * that effect, so check this.
	 */
	object_type j_ptr[1];
	object_info_known(j_ptr, o_ptr);
	ident = (power == get_power(j_ptr));

	if (!use_object_power(power, dir, &ident, &use) &&
		!use_object_power(PO_TVAL+o_ptr->tval, dir, &ident, &use))
	{
		/* Paranoia - no known powers. */
		bell("Unknown power: %d", power);
		return FALSE;
	}

	/* Become aware of the object if it's now known. */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
	}

	return use;
}

cptr describe_object_power(object_ctype *o_ptr)
{
	int power = get_power(o_ptr);
	int plev = MAX(1, skill_set[SKILL_DEVICE].value/2);
	return describe_power(power, plev);
}

/*
 * Calculate the energy needed to use a magical device.
 */
s16b item_use_energy(object_ctype *o_ptr)
{
	if (is_worn_p(o_ptr))
		return TURN_ENERGY/10;
	else
		return extract_energy[p_ptr->pspeed];
}

/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(object_type *o_ptr)
{
	bool	normal_food = FALSE;



	/* Restrict choices to food */
	item_tester_tval = TV_FOOD;

	/* Get an item if we weren't passed one */
	if(!o_ptr)
	{
		errr err;
		/* Get an item (from inven or floor) */
		if (!((o_ptr = get_item(&err, "Eat which item? ", FALSE, TRUE, TRUE))))
		{
			if (err == -2) msg_print("You have nothing to eat.");
			return;
		}
	}

	item_tester_tval = TV_FOOD;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("You can't eat that!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Sound */
	sound(SOUND_EAT);


	/* Take a turn */
	energy_use = item_use_energy(o_ptr);

	/* Process the object's effects. */
	normal_food = !use_object(o_ptr, 0);

	/* We have tried it */
	object_tried(o_ptr);

	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr, 0);

	/* Food can feed the player */
	switch (p_ptr->prace)
	{
		case RACE_VAMPIRE:
		{
			add_flag(TIMED_FOOD, (o_ptr->pval / 10));
			msg_print("Mere victuals hold scant sustenance for a being such as yourself.");
			if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
				msg_print("Your hunger can only be satisfied with fresh blood!");
			break;
		}
		case RACE_SKELETON:
		{
	        if (normal_food)
	        {
				object_type q_ptr[1];

				msg_print("The food falls through your jaws!");

				/* Create the item (should this be object_copy()?) */
				object_copy(q_ptr, o_ptr);

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, py, px);
			}
			else
			{
				msg_print("The food falls through your jaws and vanishes!");
			}
			break;
		}
		case RACE_GOLEM:
		case RACE_ZOMBIE:
		case RACE_SPECTRE:
		{
			msg_print("The food of mortals is poor sustenance for you.");
			add_flag(TIMED_FOOD, ((o_ptr->pval) / 20));
			break;
		}
		default:
		{
			add_flag(TIMED_FOOD, o_ptr->pval);
		}
	}



	/* Destroy a food */
	item_increase(o_ptr, -1);
	item_describe(o_ptr);
	item_optimize(o_ptr);
}
    
/*
 * Quaff a potion (from the pack or the floor)
 */
void do_cmd_quaff_potion(object_type *o_ptr)
{
	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get an item if we weren't passed one */
	if(!o_ptr)
	{
		errr err;
		/* Get an item (from inven or floor) */
		if (!((o_ptr = get_item(&err, "Quaff which potion? ", TRUE, TRUE, TRUE))))
		{
			if (err == -2) msg_print("You have no potions to quaff.");
			return;
		}
	}

	item_tester_tval = TV_POTION;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a potion!");
		return;
	}
	item_tester_tval = 0;

	/* Sound */
	sound(SOUND_QUAFF);


	/* Take a turn */
	energy_use = item_use_energy(o_ptr);

	/* Analyze the potion (which is always used up). */
	use_object(o_ptr, 0);

    if ((p_ptr->prace == RACE_SKELETON) && (randint(12)==1))
    {
        msg_print("Some of the fluid falls through your jaws!");
        potion_smash_effect(0, py, px, o_ptr->k_idx);
    }

	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr, 0);

	/* The item has been tried */
	object_tried(o_ptr);


	/* Potions can feed the player (should this be altered for skeletons?) */
	(void)add_flag(TIMED_FOOD, o_ptr->pval);


	/* Destroy a potion */
	item_increase(o_ptr, -1);
	item_describe(o_ptr);
	item_optimize(o_ptr);
}


/*
 * Try to curse an object. Return TRUE if an effect was observed.
 */
static bool curse_object(int o_idx, int e_idx, cptr where)
{
	object_type *o_ptr = inventory+o_idx;

	if (!o_ptr->k_idx) return FALSE;

	if (allart_p(o_ptr) && one_in(2))
	{
		/* Cool */
		msg_format("A terrible black aura tries to surround your %s, "
			"but your %v resists the effects!",
			where, object_desc_f3, o_ptr, FALSE, 3);
		return TRUE;
	}

	/* Oops */
	msg_format("A terrible black aura blasts your %v!",
		object_desc_f3, o_ptr, FALSE, 3);

	/* Shatter the object. */
	o_ptr->name1 = 0;
	o_ptr->name2 = e_idx;

	apply_magic_2(o_ptr, dun_depth);

	/* Tell the player the bad news */
	o_ptr->ident |= (IDENT_SENSE);
		
	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	return TRUE;
}

/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	return curse_object(INVEN_BODY, EGO_BLASTED, "armour");
}

/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
	return curse_object(INVEN_WIELD, EGO_SHATTERED, "weapon");
}



/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
void do_cmd_read_scroll(object_type *o_ptr)
{
	int used_up;

	/* Check some conditions */
	if (p_ptr->blind)
	{
		msg_print("You can't see anything.");
		return;
	}
	if (no_lite())
	{
		msg_print("You have no light to read by.");
		return;
	}
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Restrict choices to scrolls */
	item_tester_tval = TV_SCROLL;

	/* Get an item if we weren't passed one */
	if(!o_ptr)
	{
		errr err;
		/* Get an item (from inven or floor) */
		if (!((o_ptr = get_item(&err, "Read which scroll? ", TRUE, TRUE, TRUE))))
		{
			if (err == -2) msg_print("You have no scrolls to read.");
			return;
		}
	}

	item_tester_tval = TV_SCROLL;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a scroll!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Take a turn */
	energy_use = item_use_energy(o_ptr);

	/* Analyze the scroll */
	used_up = use_object(o_ptr, 0);


	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr, 0);

	/* The item was tried */
	object_tried(o_ptr);

	/* Hack -- allow certain scrolls to be "preserved" */
	if (!used_up) return;


	/* Destroy a scroll */
	item_increase(o_ptr, -1);
	item_describe(o_ptr);
	item_optimize(o_ptr);
}



/*
 * Determine the chance of using a magical device as a/b (rand_int(b) < a).
 *
 * NB: This does not check that the device is suitable for this calculation.
 */
void get_device_chance(object_ctype *o_ptr, int *num, int *denom)
{
	/* Find the object level. */
	int lev;
	if (is_wand_p(k_info+o_ptr->k_idx))
	{
		lev = wand_power(k_info+o_ptr->k_idx);
	}
	else if (artifact_p(o_ptr))
	{
		/* Use the artefact level for normal artefacts. */
		lev = a_info[o_ptr->name1].level;
	}
	else
	{
		/* Extract the base item level otherwise. */
		lev = object_k_level(k_info+o_ptr->k_idx);
	}

	/* Hack - Limit the difficulty. */
	lev = MIN(lev, 50);

	/* Base chance of success */
	*num = USE_DEVICE;
	*denom = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) *denom /= 2;

	/* Higher level objects are harder */
	*denom -= lev;

	/* Give everyone a (slight) chance */
	if (*denom < USE_DEVICE)
	{
		int factor = USE_DEVICE - *denom + 1;
		*num *= factor;
		*denom = USE_DEVICE * factor;
	}

	/* Convert num to the desired form. */
	(*num) = 1+(*denom)-(*num);
}

/*
 * Determine if an attempt to use a magical device succeeded.
 */
static bool use_device_p(object_type *o_ptr)
{
	int num, denom;
	get_device_chance(o_ptr, &num, &denom);
	return (rand_int(denom) < num);
}

/*
 * Use a staff.			-RAK-
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
void do_cmd_use_staff(object_type *o_ptr)
{
	bool use_charge;


	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item if we weren't already passed one */
	if(!o_ptr)
	{
		errr err;
		/* Get an item (from inven or floor) */
		if (!((o_ptr = get_item(&err, "Use which staff? ", FALSE, TRUE, TRUE))))
		{
			if (err == -2) msg_print("You have no staff to use.");
			return;
		}
	}

	item_tester_tval = TV_STAFF;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a staff!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Mega-Hack -- refuse to use a pile from the ground */
	if ((!is_inventory_p(o_ptr)) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the staffs.");
		return;
	}


	/* Take a turn */
	energy_use = item_use_energy(o_ptr);

	/* Roll for failure. */
	if (!use_device_p(o_ptr))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the staff properly.");
		return;
	}

	/* Notice empty staffs */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msg_print("The staff has no charges left.");
		o_ptr->ident |= (IDENT_EMPTY);
		return;
	}


	/* Sound */
	sound(SOUND_ZAP);



	/* Analyze the staff */
	use_charge = use_object(o_ptr, 0);


	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr, 0);

	/* Tried the item */
	object_tried(o_ptr);


	/* Hack -- some uses are "free" */
	if (!use_charge) return;


	/* Use a single charge */
	o_ptr->pval--;

	/* Gain exp */
	skill_exp(SKILL_DEVICE);

	/* XXX Hack -- unstack if necessary */
	if ((is_inventory_p(o_ptr)) && (o_ptr->number > 1))
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		/* Modify quantity */
		q_ptr->number = 1;

		/* Restore the charges */
		o_ptr->pval++;

		/* Unstack the used item */
		o_ptr->number--;
		total_weight -= q_ptr->weight;
		inven_carry(q_ptr);

		/* Message */
		msg_print("You unstack your staff.");
	}

	/* Describe charges in the pack */
	item_charges(o_ptr);
}


/*
 * Aim a wand (from the pack, pouch or floor).
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
void do_cmd_aim_wand(object_type *o_ptr)
{
	int dir;


	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Get an item if we weren't passed one */
	if(!o_ptr)
	{
		errr err;
		/* Get an item (from inven or floor) */
		if (!((o_ptr = get_item(&err, "Aim which wand? ", TRUE, TRUE, TRUE))))
		{
			if (err == -2) msg_print("You have no wand to aim.");
			return;
		}
	}

	item_tester_tval = TV_WAND;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a wand!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Mega-Hack -- refuse to aim a pile from the ground */
	if ((!is_inventory_p(o_ptr)) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the wands.");
		return;
	}


	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return;


	/* Take a turn */
	energy_use = item_use_energy(o_ptr);;

	/* Roll for failure. */
	if (!use_device_p(o_ptr))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the wand properly.");
		return;
	}

	/* The wand is already empty! */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msg_print("The wand has no charges left.");
		o_ptr->ident |= (IDENT_EMPTY);
		return;
	}


	/* Sound */
	sound(SOUND_ZAP);

	/* Use the object (which always takes a charge). */
	use_object(o_ptr, dir);

	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr, 0);

	/* Mark it as tried */
	object_tried(o_ptr);

	/* Use a single charge */
	o_ptr->pval--;

	/* Gain exp */
	skill_exp(SKILL_DEVICE);

	/* Hack -- unstack if necessary */
	if ((is_inventory_p(o_ptr)) && (o_ptr->number > 1))
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		/* Modify quantity */
		q_ptr->number = 1;

		/* Restore the charges */
		o_ptr->pval++;

		/* Unstack the used item */
		o_ptr->number--;
		total_weight -= q_ptr->weight;
		inven_carry(q_ptr);

		/* Message */
		msg_print("You unstack your wand.");
	}

	/* Describe the charges in the pack */
	item_charges(o_ptr);
}


/*
 * Activate (zap) a Rod
 *
 * Unstack fully charged rods as needed.
 *
 * Hack -- rods of perception/genocide can be "cancelled"
 * All rods can be cancelled at the "Direction?" prompt
 */
void do_cmd_zap_rod(object_type *o_ptr)
{
	int dir;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;


	/* Restrict choices to rods */
	item_tester_tval = TV_ROD;

	/* Get an item if we do not already have one */
	if(!o_ptr)
	{
		errr err;
		/* Get an item (from inven or floor) */
		if (!((o_ptr = get_item(&err, "Zap which rod? ", FALSE, TRUE, TRUE))))
		{
			if (err == -2) msg_print("You have no rod to zap.");
			return;
		}
	}

	item_tester_tval = TV_ROD;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a rod!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Mega-Hack -- refuse to zap a pile from the ground */
	if ((!is_inventory_p(o_ptr)) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the rods.");
		return;
	}



	/* Take a turn */
	energy_use = item_use_energy(o_ptr);

	/* Still charging */
	if (o_ptr->timeout)
	{
		if (flush_failure) flush();
		msg_print("The rod is still charging.");
		return;
	}

	/* Roll for failure. */
	if (!use_device_p(o_ptr))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the rod properly.");
		return;
	}

	/* Sound */
	sound(SOUND_ZAP);

	/* Get a direction for any unknown rod. */
	if (!object_aware_p(o_ptr))
	{
		/* Get a direction, allow cancel */
		if (!get_aim_dir(&dir)) return;
	}
	else
	{
		dir = 0;
	}

	/* Analyze the rod */
	use_charge = use_object(o_ptr, dir);


	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr, 0);

	/* Tried the object */
	object_tried(o_ptr);

	/* Do nothing more if cancelled. */
	if (!use_charge) return;

	/* Most rods simply use the pval to decide their recharging rate. */
	if (o_ptr->pval >= 0) o_ptr->timeout = o_ptr->pval;

	/* A few rods use negative pvals to indicate a variable timeout. */
	else
	{
		int t = -o_ptr->pval;
		o_ptr->timeout = rand_range(t/2+1, t);
	}

	/* Gain exp */
	skill_exp(SKILL_DEVICE);

	/* XXX Hack -- unstack if necessary */
	if ((is_inventory_p(o_ptr)) && (o_ptr->number > 1))
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		/* Modify quantity */
		q_ptr->number = 1;

		/* Restore "charge" */
		o_ptr->timeout = 0;

		/* Unstack the used item */
		o_ptr->number--;
		total_weight -= q_ptr->weight;
		inven_carry(q_ptr);

		/* Message */
		msg_print("You unstack your rod.");
	}
}




/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_ctype *o_ptr)
{
	u32b f1, f2, f3;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}



typedef const struct activation_type activation_type;
struct activation_type
{
	int idx;
	s16b min;
	s16b max;
	cptr text;
};

static activation_type activation_info[] =
{
	{ACT_SUNLIGHT+PO_ACTIVATION, 10, 10, 0},
	{ACT_BO_MISS_1+PO_ACTIVATION, 2, 2, "It glows extremely brightly..."},
	{ACT_BA_POIS_1+PO_ACTIVATION, 4, 7, "It throbs deep green..."},
	{ACT_BO_ELEC_1+PO_ACTIVATION, 6, 11, "It is covered in sparks..."},
	{ACT_BO_ACID_1+PO_ACTIVATION, 5, 9, "It is covered in acid..."},
	{ACT_BO_COLD_1+PO_ACTIVATION, 7, 13, "It is covered in frost..."},
	{ACT_BO_FIRE_1+PO_ACTIVATION, 8, 15, "It is covered in fire..."},
	{ACT_BA_COLD_1+PO_ACTIVATION, 400, 400, "It is covered in frost..."},
	{ACT_BA_FIRE_1+PO_ACTIVATION, 400, 400, "It glows an intense red..."},
	{ACT_DRAIN_1+PO_ACTIVATION, 100, 200, "It glows black..."},
	{ACT_BA_COLD_2+PO_ACTIVATION, 300, 300, "It glows an intense blue..."},
	{ACT_BA_ELEC_2+PO_ACTIVATION, 500, 500, "It crackles with electricity..."},
	{ACT_DRAIN_2+PO_ACTIVATION, 400, 400, "It glows black..."},
	{ACT_VAMPIRE_1+PO_ACTIVATION, 400, 400, 0},
	{ACT_BO_MISS_2+PO_ACTIVATION, 90, 179, "It grows magical spikes..."},
	{ACT_BA_FIRE_2+PO_ACTIVATION, 225, 449, "It glows deep red..."},
	{ACT_BA_COLD_3+PO_ACTIVATION, 325, 649, "It glows bright white..."},
	{ACT_BA_ELEC_3+PO_ACTIVATION, 425, 849, "It glows deep blue..."},
	{ACT_WHIRLWIND+PO_ACTIVATION, 250, 250, 0},
	{ACT_VAMPIRE_2+PO_ACTIVATION, 400, 400, 0},
	{ACT_CALL_CHAOS+PO_ACTIVATION, 350, 350, "It glows in scintillating colours..."},
	{ACT_SHARD+PO_ACTIVATION, 400, 400, 0},
	{ACT_DISP_EVIL+PO_ACTIVATION, 300, 599, "It floods the area with goodness..."},
	{ACT_BA_MISS_3+PO_ACTIVATION, 500, 500, 0},
	{ACT_DISP_GOOD+PO_ACTIVATION, 300, 599, "It floods the area with evil..."},
	{ACT_CONFUSE+PO_ACTIVATION, 15, 15, "It glows in scintillating colours..."},
	{ACT_SLEEP+PO_ACTIVATION, 55, 55, "It glows deep blue..."},
	{ACT_QUAKE+PO_ACTIVATION, 50, 50, 0},
	{ACT_TERROR+PO_ACTIVATION, 0, 0, 0}, /* 3*plev+30*/
	{ACT_TELE_AWAY+PO_ACTIVATION, 200, 200, 0},
	{ACT_BANISH_EVIL+PO_ACTIVATION, 251, 500, 0},
	{ACT_GENOCIDE+PO_ACTIVATION, 500, 500, "It glows deep blue..."},
	{ACT_MASS_GENO+PO_ACTIVATION, 1000, 1000, "It lets out a long, shrill note..."},
	{ACT_CHARM_ANIMAL+PO_ACTIVATION, 300, 300, 0},
	{ACT_CHARM_UNDEAD+PO_ACTIVATION, 333, 333, 0},
	{ACT_CHARM_OTHER+PO_ACTIVATION, 400, 400, 0},
	{ACT_CHARM_ANIMALS+PO_ACTIVATION, 500, 500, 0},
	{ACT_CHARM_OTHERS+PO_ACTIVATION, 750, 750, 0},
	{ACT_SUMMON_ANIMAL+PO_ACTIVATION, 201, 500, 0},
	{ACT_SUMMON_PHANTOM+PO_ACTIVATION, 201, 400, 0},
	{ACT_SUMMON_ELEMENTAL+PO_ACTIVATION, 750, 750, 0},
	{ACT_SUMMON_DEMON+PO_ACTIVATION, 667, 999, 0},
	{ACT_SUMMON_UNDEAD+PO_ACTIVATION, 667, 999, 0},
	{ACT_CURE_LW+PO_ACTIVATION, 10, 10, 0},
	{ACT_CURE_MW+PO_ACTIVATION, 3, 5, "It radiates deep purple..."},
	{ACT_CURE_POISON+PO_ACTIVATION, 5, 5, "It glows deep blue..."},
	{ACT_REST_LIFE+PO_ACTIVATION, 450, 450, "It glows a deep red..."},
	{ACT_REST_ALL+PO_ACTIVATION, 750, 750, "It glows a deep green..."},
	{ACT_CURE_700+PO_ACTIVATION, 250, 250, "It glows deep blue..."},
	{ACT_CURE_1000+PO_ACTIVATION, 888, 888, "It glows a bright white..."},
	{ACT_ESP+PO_ACTIVATION, 200, 200, 0},
	{ACT_BERSERK+PO_ACTIVATION, 101, 200, 0},
	{ACT_PROT_EVIL+PO_ACTIVATION, 225, 449, "It lets out a shrill wail..."},
	{ACT_RESIST_ALL+PO_ACTIVATION, 200, 200, "It glows many colours..."},
	{ACT_SPEED+PO_ACTIVATION, 250, 250, "It glows bright green..."},
	{ACT_XTRA_SPEED+PO_ACTIVATION, 200, 399, "It glows brightly..."},
	{ACT_WRAITH+PO_ACTIVATION, 1000, 1000, 0},
	{ACT_INVULN+PO_ACTIVATION, 1000, 1000, 0},
	{ACT_LIGHT+PO_ACTIVATION, 10, 19, "It wells with clear light..."},
	{ACT_MAP_LIGHT+PO_ACTIVATION, 50, 99, "It shines brightly..."},
	{ACT_DETECT_ALL+PO_ACTIVATION, 55, 109, "It glows bright white..."},
	{ACT_DETECT_XTRA+PO_ACTIVATION, 1000, 1000, "It glows brightly..."},
	{ACT_ID_FULL+PO_ACTIVATION, 750, 750, "It glows yellow..."},
	{ACT_ID_PLAIN+PO_ACTIVATION, 10, 10, 0},
	{ACT_RUNE_EXPLO+PO_ACTIVATION, 200, 200, "It glows bright red..."},
	{ACT_RUNE_PROT+PO_ACTIVATION, 400, 400, "It glows light blue..."},
	{ACT_SATIATE+PO_ACTIVATION, 200, 200, 0},
	{ACT_DEST_DOOR+PO_ACTIVATION, 10, 10, "It glows bright red..."},
	{ACT_STONE_MUD+PO_ACTIVATION, 5, 5, "It pulsates..."},
	{ACT_RECHARGE+PO_ACTIVATION, 70, 70, 0},
	{ACT_ALCHEMY+PO_ACTIVATION, 500, 500, "It glows bright yellow..."},
	{ACT_DIM_DOOR+PO_ACTIVATION, 100, 100, 0},
	{ACT_TELEPORT+PO_ACTIVATION, 45, 45, "It twists space around you..."},
	{ACT_RECALL+PO_ACTIVATION, 200, 200, "It glows soft white..."},
	{ACT_TELEPORT_WAIT+PO_ACTIVATION, 51, 100, 0},
	{OBJ_RING_ACID+PO_K_IDX, 50, 99, 0},
	{OBJ_RING_ICE+PO_K_IDX, 50, 99, 0},
	{OBJ_RING_FIRE+PO_K_IDX, 50, 99, 0},
	{OBJ_DSM_BLUE+PO_K_IDX, 450, 899, 0},
	{OBJ_DSM_WHITE+PO_K_IDX, 450, 899, 0},
	{OBJ_DSM_BLACK+PO_K_IDX, 450, 899, 0},
	{OBJ_DSM_GREEN+PO_K_IDX, 450, 899, 0},
	{OBJ_DSM_RED+PO_K_IDX, 450, 899, 0},
	{OBJ_DSM_MULTI_HUED+PO_K_IDX, 225, 449, 0},
	{OBJ_DSM_BRONZE+PO_K_IDX, 450, 899, 0},
	{OBJ_DSM_GOLD+PO_K_IDX, 450, 899, 0},
	{OBJ_DSM_CHAOS+PO_K_IDX, 300, 599, 0},
	{OBJ_DSM_LAW+PO_K_IDX, 300, 599, 0},
	{OBJ_DSM_BALANCE+PO_K_IDX, 300, 599, 0},
	{OBJ_DSM_PSEUDO+PO_K_IDX, 300, 599, 0},
	{OBJ_DSM_POWER+PO_K_IDX, 300, 599, 0},
	{ART_POLARIS+PO_NAME1, 10, 19, "The essence wells with clear light..."},
	{ART_XOTH+PO_NAME1, 50, 99, "The essence shines brightly..."},
	{ART_TRAPEZOHEDRON+PO_NAME1, 20, 39, "The gemstone flashes bright red!"},
	{ART_LOBON+PO_NAME1, 225, 449, "The amulet lets out a shrill wail..."},
	{ART_ALHAZRED+PO_NAME1, 300, 599, "The amulet floods the area with goodness..."},
	{ART_MAGIC+PO_NAME1, 100, 199, "You order Frakir to strangle your opponent."},
	{ART_BAST+PO_NAME1, 150, 299, "The ring glows brightly..."},
	{ART_ELEMFIRE+PO_NAME1, 225, 449, "The ring glows deep red..."},
	{ART_ELEMICE+PO_NAME1, 325, 649, "The ring glows bright white..."},
	{ART_ELEMSTORM+PO_NAME1, 425, 849, "The ring glows deep blue..."},
	{ART_NYARLATHOTEP+PO_NAME1, 450, 899, "The ring glows intensely black..."},
	{ART_RAZORBACK+PO_NAME1, 1000, 1000, "Your armor is surrounded by lightning..."},
	{ART_BLADETURNER+PO_NAME1, 400, 400, 0},
	{ART_SOULKEEPER+PO_NAME1, 888, 888, "Your armor glows a bright white..."},
	{ART_VAMPLORD+PO_NAME1, 300, 300, "A heavenly choir sings..."},
	{ART_ORCS+PO_NAME1, 500, 500, "Your armor glows deep blue..."},
	{ART_OGRELORDS+PO_NAME1, 10, 10, "Your armor glows bright red..."},
	{ART_POWER+PO_NAME1, 0, 0, 0}, /* 3*plev+30 */
	{ART_MASK+PO_NAME1, 0, 0, 0}, /* 3*plev+30 */
	{ART_SKULLKEEPER+PO_NAME1, 55, 109, "Your helm glows bright white..."},
	{ART_SUN+PO_NAME1, 250, 250, "Your crown glows deep yellow..."},
	{ART_BARZAI+PO_NAME1, 111, 111, "Your cloak glows many colours..."},
	{ART_DARKNESS+PO_NAME1, 55, 55, "Your cloak glows deep blue..."},
	{ART_SWASHBUCKLER+PO_NAME1, 70, 70, "Your cloak glows bright yellow..."},
	{ART_SHIFTER+PO_NAME1, 45, 45, "Your cloak twists space around you..."},
	{ART_NYOGTHA+PO_NAME1, 450, 450, "Your cloak glows a deep red..."},
	{ART_LIGHT+PO_NAME1, 2, 2, "Your gloves glow extremely brightly..."},
	{ART_IRONFIST+PO_NAME1, 8, 15, "Your gauntlets are covered in fire..."},
	{ART_GHOULS+PO_NAME1, 7, 13, "Your gauntlets are covered in frost..."},
	{ART_WHITESPARK+PO_NAME1, 6, 11, "Your gauntlets are covered in sparks..."},
	{ART_DEAD+PO_NAME1, 5, 9, "Your gauntlets are covered in acid..."},
	{ART_COMBAT+PO_NAME1, 90, 179, "Your cesti grows magical spikes..."},
	{ART_ITHAQUA+PO_NAME1, 200, 200, "A wind swirls around your boots..."},
	{ART_DANCING+PO_NAME1, 5, 5, "Your boots glow deep blue..."},
	{ART_FAITH+PO_NAME1, 8, 15, "Your dagger is covered in fire..."},
	{ART_HOPE+PO_NAME1, 7, 13, "Your dagger is covered in frost..."},
	{ART_CHARITY+PO_NAME1, 6, 11, "Your dagger is covered in sparks..."},
	{ART_THOTH+PO_NAME1, 4, 7, "Your dagger throbs deep green..."},
	{ART_ICICLE+PO_NAME1, 5, 9, "Your dagger is covered in frost..."},
	{ART_KARAKAL+PO_NAME1, 35, 35, 0},
	{ART_STARLIGHT+PO_NAME1, 300, 300, "Your sword glows an intense blue..."},
	{ART_DAWN+PO_NAME1, 500, 999, "Your sword flickers black for a moment..."},
	{ART_EVERFLAME+PO_NAME1, 400, 400, "Your sword glows an intense red..."},
	{ART_THEODEN+PO_NAME1, 400, 400, "Your axe blade glows black..."},
	{ART_ODIN+PO_NAME1, 500, 500, "Your spear crackles with electricity..."},
	{ART_DESTINY+PO_NAME1, 5, 5, "Your spear pulsates..."},
	{ART_TROLLS+PO_NAME1, 1000, 1000, "Your axe lets out a long, shrill note..."},
	{ART_SPLEENSLICER+PO_NAME1, 3, 5, "Your battle axe radiates deep purple..."},
	{ART_GNORRI+PO_NAME1, 150, 150, "Your trident glows deep red..."},
	{ART_GHARNE+PO_NAME1, 200, 200, "Your scythe glows soft white..."},
	{ART_TOTILA+PO_NAME1, 15, 15, "Your flail glows in scintillating colours..."},
	{ART_FIRESTAR+PO_NAME1, 100, 100, "Your morning star rages in fire..."},
	{ART_THUNDER+PO_NAME1, 100, 199, "Your mace glows bright green..."},
	{ART_ERIRIL+PO_NAME1, 10, 10, "Your quarterstaff glows yellow..."},
	{ART_ATAL+PO_NAME1, 1000, 1000, "Your quarterstaff glows brightly..."},
	{ART_JUSTICE+PO_NAME1, 70, 70, "Your hammer glows white..."},
	{ART_DEATH+PO_NAME1, 999, 999, "Your crossbow glows deep red..."},
};

#ifdef CHECK_ARRAYS
void check_activation_info(void)
{
	activation_type *a_ptr, *b_ptr;
	FOR_ALL_IN(activation_info, a_ptr)
	{
		if (a_ptr->min > a_ptr->max)
			quit_fmt("min > max for timeout %d.", a_ptr->idx);

		FOR_ALL_IN(activation_info, b_ptr)
		{
			if ((a_ptr != b_ptr) && (a_ptr->idx == b_ptr->idx))
				quit_fmt("Timeout %d is listed twice.", a_ptr->idx);
		}
	}
}
#endif /* CHECK_ARRAYS */

/*
 * Find an activation in the above table by idx field.
 * Return 0 on errors.
 */
static PURE activation_type *get_activation(int idx)
{
	activation_type *ptr;
	FOR_ALL_IN(activation_info, ptr)
	{
		if (ptr->idx == idx) return ptr;
	}
	return 0;
}

/*
 * Add the appropriate timeout for a rod or activatable object.
 * Complain if an object has no listed timeout as a timeout of 0 should be
 * listed explicitly.
 */
static int activation_timeout(activation_type *ac_ptr)
{
	assert(ac_ptr); /*See caller.*/
	switch (ac_ptr->idx)
	{
		case ACT_TERROR+PO_ACTIVATION:
		{
			int plev = MAX(1, skill_set[SKILL_DEVICE].value/2);
			return 3*(plev+10);
		}
		case ART_POWER+PO_NAME1:
		case ART_MASK+PO_NAME1:
		{
			int plev = MAX(0, skill_set[SKILL_DEVICE].value/2);
			return 3*(plev+10);
		}
		default:
		{
			/* Return a possible activation timeout. */
			return rand_range(ac_ptr->min, ac_ptr->max);
		}
	}
}

/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
void do_cmd_activate(object_type *o_ptr)
{
	activation_type *ac_ptr;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item if we weren't passed one from the inventory*/
	if(!o_ptr)
	{
		errr err;

		/* Get an item (from equip) */
		if (!((o_ptr = get_item(&err, "Activate which item? ", TRUE, FALSE, FALSE))))
		{
			if (err == -2) msg_print("You have nothing to activate.");
			return;
		}
	}

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;
	/* Verify that the item can be used */
	if(!item_tester_okay(o_ptr))
	{
		msg_print("You can't activate that!");
		item_tester_hook = 0;
		return;
	}
	/* Clear the hook */
	item_tester_hook = 0;

	/* Take a turn */
	energy_use = item_use_energy(o_ptr);


	/* Roll for failure. */
	if (!use_device_p(o_ptr))
	{
		if (flush_failure) flush();
		msg_print("You failed to activate it properly.");
		return;
	}

	/* Check the recharge */
	if (o_ptr->timeout)
	{
		msg_print("It whines, glows and fades...");
		return;
	}


	/* Activate the artifact */
	msg_print("You activate it...");

	/* Sound */
	sound(SOUND_ZAP);

	ac_ptr = get_activation(get_power(o_ptr));

	if (!ac_ptr)
	{
		bell("Unknown activation: %d", get_power(o_ptr));
		return;
	}
	if (ac_ptr->text)
	{
		/* Print a special message at the start of the activation. */
		msg_print(ac_ptr->text);
	}

	if (use_object(o_ptr, 0))
	{
		/* Set the timeout if the object has been used. */
		o_ptr->timeout += activation_timeout(ac_ptr);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Gain exp */
		skill_exp(SKILL_DEVICE);
	}
}
