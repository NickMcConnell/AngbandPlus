/* File: cmd6.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
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
 * Hook to determine if an object is activatable and charged
 */
static bool item_tester_hook_food_edible(const object_type *o_ptr)
{
	/* Check based on tval */
	switch(o_ptr->tval)
	{
		/* Undead can't eat normal food */
		case TV_FOOD:
			/* Undead cannot eat food */
			if (!(p_ptr->cur_flags4 & (TR4_UNDEAD))) return (TRUE);

			break;

		/* Eggs can be eaten by animals and hungry people */
		case TV_EGG:
			/* Undead cannot eat eggs */
			if (p_ptr->cur_flags4 & (TR4_UNDEAD)) return (FALSE);

			/* Animals */
			if (p_ptr->cur_flags4 & (TR4_ANIMAL)) return (TRUE);

			/* Hungry */
			if (p_ptr->food < PY_FOOD_ALERT) return (TRUE);

			break;

		/* Some flasks count as bodies */
		case TV_FLASK:

			if (o_ptr->sval != SV_FLASK_BLOOD) return (FALSE);

			/* Fall through */

		/* Bodies can only be eaten by animals, undead and starving people */
		case TV_BODY:
		case TV_BONE:
			/* Undead and animals can eat bodies */
			if (p_ptr->cur_flags4 & (TR4_UNDEAD | TR4_ANIMAL)) return (TRUE);

			/* Orcs, trolls, giants have harder stomachs */
			if ((p_ptr->cur_flags4 & (TR4_ORC | TR4_TROLL | TR4_GIANT)) && (p_ptr->food < PY_FOOD_ALERT)) return (TRUE);

			/* Starving */
			if (p_ptr->food < PY_FOOD_ALERT) return (TRUE);

			break;
	}

	/* Assume not edible */
	return (FALSE);
}



/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
	int item, ident, lev;

	/* Must be true to let us cancel */
	bool cancel = TRUE;

	object_type *o_ptr;

	cptr q, s;

	int power;

	bool use_feat = FALSE;

	/* Restrict choices to food */
	item_tester_hook = item_tester_hook_food_edible;

#if 0
	/* Restrict choices to food */
	item_tester_tval = TV_FOOD;
#endif

	/* Get an item */
	q = "Eat which item? ";
	s = "You have nothing to eat.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATU))) return;

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

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Use feat */
	if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

	/* Sound */
	sound(MSG_EAT);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Identity not known yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Eating effects */
	switch (o_ptr->tval)
	{
		/* Normal food */
		case TV_FOOD:
		{
			/* Get food effect */
			get_spell(&power, "use", o_ptr, FALSE);

			/* Paranoia */
			if (power < 0) return;

			/* Apply food effect */
			if (process_spell_eaten(SOURCE_PLAYER_EAT, o_ptr->k_idx, power,0,&cancel)) ident = TRUE;

			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);

			/* We have tried it */
			object_tried(o_ptr);

			/* The player is now aware of the object */
			if (ident && !object_aware_p(o_ptr))
			{
				object_aware(o_ptr, item < 0);
				gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
			}

			if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;

			break;
		}

		default:
		{
			/* First, apply breath weapon attack */

			/* Then apply touch attack */
			mon_blow_ranged(SOURCE_PLAYER_EAT_MONSTER, o_ptr->name3, p_ptr->py, p_ptr->px, RBM_TOUCH, 0, PROJECT_HIDE | PROJECT_PLAY, NULL);

			/* Then apply spore attack */
			mon_blow_ranged(SOURCE_PLAYER_EAT_MONSTER, o_ptr->name3, p_ptr->py, p_ptr->px, RBM_SPORE, 0, PROJECT_HIDE | PROJECT_PLAY, NULL);

			break;
		}
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Food can feed the player */
	(void)set_food(p_ptr->food + o_ptr->charges * o_ptr->weight);

	/* Destroy a food in the pack */
	if (item >= 0)
	{
		if (o_ptr->number == 1) inven_drop_flags(o_ptr);

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
		if (use_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_USE_FEAT);
	}
}




/*
 * Quaff a potion (from the pack or the floor)
 */
void do_cmd_quaff_potion(void)
{
	int item, ident, lev, power;

	/* Must be true to let us cancel */
	bool cancel = TRUE;

	object_type *o_ptr;

	cptr q, s;

	bool use_feat = FALSE;

	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get an item */
	q = "Quaff which potion? ";
	s = "You have no potions to quaff.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATU))) return;

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

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

	/* Sound */
	sound(MSG_QUAFF);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Set style */
	p_ptr->cur_style |= (1L << WS_POTION);

	/* Get potion effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Apply food effect */
	if (power >= 0) ident = process_spell_eaten(SOURCE_PLAYER_QUAFF, o_ptr->k_idx, power,0,&cancel);
	else return;

	/* Clear styles */
	p_ptr->cur_style &= ~(1L << WS_POTION);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item has been tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr, item < 0);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Potions can feed the player */
	(void)set_food(p_ptr->food + o_ptr->charges);

	/* Destroy a potion in the pack */
	if (item >= 0)
	{
		if (o_ptr->number == 1) inven_drop_flags(o_ptr);

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
		if (use_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_USE_FEAT);
	}
}


/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
void do_cmd_read_scroll(void)
{
	int item, ident, lev,power;

	/* Must be true to let us cancel */
	bool cancel = TRUE;
	bool known = FALSE;

	object_type *o_ptr;

	cptr q, s;

	bool use_feat = FALSE;

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
	if (p_ptr->shero)
	{
		msg_print("You are too enraged!");
		return;
	}


	/* Restrict choices to scrolls */
	item_tester_tval = TV_SCROLL;

	/* Get an item */
	q = "Read which scroll? ";
	s = "You have no scrolls to read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATU))) return;

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

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Use feat */
	if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Set style */
	p_ptr->cur_style |= (1L << WS_SCROLL);

	/* Set if known */
	if (object_aware_p(o_ptr)) known = TRUE;

	/* Get scroll effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Apply scroll effect */
	if (power >= 0) ident = process_spell(SOURCE_PLAYER_READ, o_ptr->k_idx, power, 0, &cancel, &known);
	else return;

	/* Clear styles */
	p_ptr->cur_style &= ~(1L << WS_SCROLL);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item was tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr, item < 0);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- allow certain scrolls to be "preserved" */
	if (cancel) return;

	/* Destroy a scroll in the pack */
	if (item >= 0)
	{
		if (o_ptr->number == 1) inven_drop_flags(o_ptr);

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
		if (use_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_USE_FEAT);
	}
}



/*
 * Use a staff
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
void do_cmd_use_staff(void)
{
	int item, ident, chance, lev, power;

	/* Must be true to let us cancel */
	bool cancel = TRUE;
	bool known = FALSE;

	object_type *o_ptr;

	cptr q, s;

	int i;

	/* Berserk */
	if (p_ptr->shero)
	{
		msg_print("You are too enraged!");
		return;
	}

	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item */
	q = "Use which staff? ";
	s = "You have no staff to use.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_EQUIP))) return;

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

#if 0
	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}
#endif
	/* Mega-Hack -- refuse to use a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the staffs.");
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Check for speciality */
	if (p_ptr->pstyle == WS_STAFF)
	{
		for (i = 0;i< z_info->w_max;i++)
		{
			if (w_info[i].class != p_ptr->pclass) continue;

			if (w_info[i].level > p_ptr->lev) continue;

			if (w_info[i].benefit != WB_POWER) continue;

			/* Check for styles */
			if (w_info[i].styles==WS_STAFF) chance *= 2;
		}
	}

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Some items and some rooms interfere with this */
	if ((p_ptr->cur_flags4 & (TR4_STATIC)) || (room_has_flag(p_ptr->py, p_ptr->px, ROOM_STATIC)))
	{
		chance = USE_DEVICE;

		/* Warn the player */
		msg_print("There is a static feeling in the air.");

		/* Get the room */
		if (!(room_has_flag(p_ptr->py, p_ptr->px, ROOM_STATIC)))
		{
			/* Always notice */
			equip_can_flags(0x0L,0x0L,0x0L,TR4_STATIC);
		}
	}
	else
	{
		/* Always notice */
		equip_not_flags(0x0L,0x0L,0x0L,TR4_STATIC);
	}

	/* Roll for usage */
	if (chance < USE_DEVICE
	    || randint(chance) + chance/USE_DEVICE < USE_DEVICE)
	{
		if (flush_failure) flush();
		msg_print("You failed to use the staff properly.");
		return;
	}

	/* Notice empty staffs */
	if (o_ptr->charges <= 0)
	{
		if (flush_failure) flush();
		msg_print("The staff has no charges left.");
		o_ptr->ident |= (IDENT_CHARGES);
		return;
	}

	/* Sound */
	sound(MSG_USE_STAFF);

	/* Set styles */
	p_ptr->cur_style |= (1L << WS_STAFF);

	/* Set if known */
	if (object_aware_p(o_ptr)) known = TRUE;

	/* Get rod effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Apply staff effect */
	if (power >= 0) ident = process_spell(SOURCE_PLAYER_USE, o_ptr->k_idx, power, 0, &cancel, &known);
	else return;

	/* Clear styles */
	p_ptr->cur_style &= ~(1L << WS_STAFF);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr, item < 0);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- some uses are "free" */
	if (cancel) return;

	/* XXX Hack -- new unstacking code */
	o_ptr->stackc++;

	/* No spare charges */	
	if (o_ptr->stackc >= o_ptr->number)
	{
		/* Use a charge off the stack */
		o_ptr->charges--;

		/* Reset the stack count */
		o_ptr->stackc = 0;
	}

	/* XXX Hack -- unstack if necessary */
	if ((o_ptr->number > 1) && 
	((!object_charges_p(o_ptr) && (o_ptr->charges == 2) && (o_ptr->stackc > 1)) ||
	  (!object_charges_p(o_ptr) && (rand_int(o_ptr->number) <= o_ptr->stackc) &&
	  (o_ptr->stackc != 1) && (o_ptr->charges > 2))))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Reset stack counter */
		i_ptr->stackc = 0;
 
 		/* Unstack the used item */
 		o_ptr->number--;

		/* Reduce the charges on the new item */
		if (o_ptr->stackc > 1)
		{
			i_ptr->charges-=2;
			o_ptr->stackc--;
		}
		else if (!o_ptr->stackc)
		{
			i_ptr->charges--;
			o_ptr->charges++;
			o_ptr->stackc = o_ptr->number-1;
		}

		/* Adjust the weight and carry */
		if (item >= 0)
		{
			p_ptr->total_weight -= i_ptr->weight;
			item = inven_carry(i_ptr);
		}
		else
		{
			item = floor_carry(p_ptr->py,p_ptr->px,i_ptr);
			floor_item_describe(item);
		}

		/* Message */
		msg_print("You unstack your staff.");
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
void do_cmd_aim_wand(void)
{
	int item, lev, ident, chance, power;

	/* Must be true to let us cancel */
	bool cancel = TRUE;
	bool known = FALSE;

	object_type *o_ptr;

	cptr q, s;

	int i;

	/* Berserk */
	if (p_ptr->shero)
	{
		msg_print("You are too enraged!");
		return;
	}

	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Get an item */
	q = "Aim which wand? ";
	s = "You have no wand to aim.";
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

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Mega-Hack -- refuse to aim a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the wands.");
		return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Get the level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Check for speciality */
	if (p_ptr->pstyle == WS_WAND)
	{
		for (i = 0;i< z_info->w_max;i++)
		{
			if (w_info[i].class != p_ptr->pclass) continue;

			if (w_info[i].level > p_ptr->lev) continue;

			if (w_info[i].benefit != WB_POWER) continue;

			/* Check for styles */
			if (w_info[i].styles==WS_WAND) chance *= 2;
		}
	}

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Some items and some rooms interfere with this */
	if ((p_ptr->cur_flags4 & (TR4_STATIC)) || (room_has_flag(p_ptr->py, p_ptr->px, ROOM_STATIC)))
	{
		chance = USE_DEVICE;

		/* Warn the player */
		msg_print("There is a static feeling in the air.");

		/* Get the room */
		if (!(room_has_flag(p_ptr->py, p_ptr->px, ROOM_STATIC)))
		{
			/* Always notice */
			equip_can_flags(0x0L,0x0L,0x0L,TR4_STATIC);
		}
	}
	else
	{
		/* Always notice */
		equip_not_flags(0x0L,0x0L,0x0L,TR4_STATIC);
	}

	/* Roll for usage */
	if (chance < USE_DEVICE
	    || randint(chance) + chance/USE_DEVICE < USE_DEVICE)
	{
		if (flush_failure) flush();
		msg_print("You failed to use the wand properly.");
		return;
	}

	/* The wand is already empty! */
	if (o_ptr->charges <= 0)
	{
		if (flush_failure) flush();
		msg_print("The wand has no charges left.");
		o_ptr->ident |= (IDENT_CHARGES);
		if (object_aware_p(o_ptr)) object_known(o_ptr);
		return;
	}

	/* Sound */
	sound(MSG_ZAP_ROD);

	/* Set styles */
	p_ptr->cur_style |= (1L << WS_WAND);

	/* Get wand effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Set if known */
	if (object_aware_p(o_ptr)) known = TRUE;

	/* Apply wand effect */
	if (power >= 0) ident = process_spell(SOURCE_PLAYER_AIM, o_ptr->k_idx, power, 0, &cancel, &known);
	else return;

	/* Clear styles */
	p_ptr->cur_style &= ~(1L << WS_WAND);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Mark it as tried */
	object_tried(o_ptr);

	/* Apply identification */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr, item < 0);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- allow cancel */
	if (cancel) return;

	/* XXX Hack -- new unstacking code */
	o_ptr->stackc++;

	/* No spare charges */	
	if (o_ptr->stackc >= o_ptr->number)
	{
		/* Use a charge off the stack */
		o_ptr->charges--;

		/* Reset the stack count */
		o_ptr->stackc = 0;
	}

	/* XXX Hack -- unstack if necessary */
	if ((o_ptr->number > 1) && 
	((!object_charges_p(o_ptr) && (o_ptr->charges == 2) && (o_ptr->stackc > 1)) ||
	  (!object_charges_p(o_ptr) && (rand_int(o_ptr->number) <= o_ptr->stackc) &&
	  (o_ptr->stackc != 1) && (o_ptr->charges > 2))))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Reset stack counter */
		i_ptr->stackc = 0;
 
 		/* Unstack the used item */
 		o_ptr->number--;

		/* Reduce the charges on the new item */
		if (o_ptr->stackc > 1)
		{
			i_ptr->charges-=2;
			o_ptr->stackc--;
		}
		else if (!o_ptr->stackc)
		{
			i_ptr->charges--;
			o_ptr->charges++;
			o_ptr->stackc = o_ptr->number-1;
		}

		/* Adjust the weight and carry */
		if (item >= 0)
		{
			p_ptr->total_weight -= i_ptr->weight;
			item = inven_carry(i_ptr);
		}
		else
		{
			item = floor_carry(p_ptr->py,p_ptr->px,i_ptr);
			floor_item_describe(item);
		}

		/* Message */
		msg_print("You unstack your wand.");
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


/*
 * Hook to determine if an object is activatable and charged
 */
static bool item_tester_hook_rod_charged(const object_type *o_ptr)
{
	/* Confirm this is a rod */
	if (o_ptr->tval != TV_ROD) return(FALSE);

	/* Check the recharge */
      	if ((o_ptr->timeout) && ((!o_ptr->stackc) || (o_ptr->stackc >= o_ptr->number))) return (FALSE);

	/* Assume charged */
	return (TRUE);
}



/*
 * Activate (zap) a Rod
 *
 * Unstack fully charged rods as needed.
 *
 * Hack -- rods of perception/banishment can be "cancelled"
 * All rods can be cancelled at the "Direction?" prompt
 */
void do_cmd_zap_rod(void)
{
	int item, ident, chance, lev, power;

	/* Must be true to let us cancel */
	bool cancel = TRUE;
	bool known = FALSE;

	object_type *o_ptr;

	cptr q, s;

	int tmpval, dir;

	/* Berserk */
	if (p_ptr->shero)
	{
		msg_print("You are too enraged!");
		return;
	}

	/* Restrict choices to charged items */
	item_tester_hook = item_tester_hook_rod_charged;

	/* Get an item */
	q = "Zap which rod? ";
	s = "You have no rod to zap.";
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
#if 0
	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}
#endif
	/* Mega-Hack -- refuse to zap a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the rods.");
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Some items and some rooms interfere with this */
	if ((p_ptr->cur_flags4 & (TR4_STATIC)) || (room_has_flag(p_ptr->py, p_ptr->px, ROOM_STATIC)))
	{
		chance = USE_DEVICE;

		/* Warn the player */
		msg_print("There is a static feeling in the air.");

		/* Get the room */
		if (!(room_has_flag(p_ptr->py, p_ptr->px, ROOM_STATIC)))
		{
			/* Always notice */
			equip_can_flags(0x0L,0x0L,0x0L,TR4_STATIC);
		}
	}
	else
	{
		/* Always notice */
		equip_not_flags(0x0L,0x0L,0x0L,TR4_STATIC);
	}

	/* Roll for usage */
	if (chance < USE_DEVICE
	    || randint(chance) + chance/USE_DEVICE < USE_DEVICE)
	{
		if (flush_failure) flush();
		msg_print("You failed to use the rod properly.");
		return;
	}
#if 0
	/* Still charging */
	if ((o_ptr->timeout) && ((!o_ptr->stackc) || (o_ptr->stackc >= o_ptr->number)))
	{
		if (flush_failure) flush();
		msg_print("The rod is still charging.");
		return;
	}
#endif

	/* Store charges */
	tmpval = o_ptr->timeout;

	/* Sound */
	sound(MSG_ZAP_ROD);

	/* Hack -- get fake direction */
	if (!object_aware_p(o_ptr) && (o_ptr->sval < SV_ROD_MIN_DIRECTION)) get_aim_dir(&dir);

	/* Set if known */
	if (object_aware_p(o_ptr)) known = TRUE;

	/* Get rod effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Apply rod effect */
	/* Note we use two different sources to suppress messages from dispel evil, in the even the rod is known to be ineffective against non-evil monsters */
	if (power >= 0) ident = process_spell(known && (o_ptr->sval >= SV_ROD_MIN_DIRECTION) ? SOURCE_PLAYER_ZAP_NO_TARGET : SOURCE_PLAYER_ZAP, o_ptr->k_idx, power, 0, &cancel, &known);
	else return;

	/* Time rod out */
	o_ptr->timeout = o_ptr->charges;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr, item < 0);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- deal with cancelled zap */
	if (cancel)
	{
		/* Restore charge */
		o_ptr->timeout = tmpval;

		return;
	}

	/* Hack -- check if we are stacking rods */
	if ((o_ptr->timeout > 0) && (!(tmpval) || stack_force_times))
	{
		/* Hack -- one more rod charging */
		if (o_ptr->timeout) o_ptr->stackc++;

		/* Reset stack count */
		if (o_ptr->stackc == o_ptr->number) o_ptr->stackc = 0;

		/* Hack -- always use maximum timeout */
		if (tmpval > o_ptr->timeout) o_ptr->timeout = tmpval;

		return;
	}

	/* XXX Hack -- unstack if necessary */
	if ((o_ptr->number > 1) && (o_ptr->timeout > 0))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Clear stack counter */
		i_ptr->stackc = 0;

		/* Restore "charge" */
		o_ptr->timeout = tmpval;

		/* Unstack the used item */
		o_ptr->number--;

		/* Reset the stack if required */
		if (o_ptr->stackc == o_ptr->number) o_ptr->stackc = 0;

		/* Adjust the weight and carry */
		if (item >= 0)
		{
			p_ptr->total_weight -= i_ptr->weight;
			item = inven_carry(i_ptr);
		}
		else
		{
			item = floor_carry(p_ptr->py,p_ptr->px,i_ptr);
			floor_item_describe(item);
		}

		/* Message */
		msg_print("You unstack your rod.");
	}

}

/*
 * Procedure for assembling parts into a whole.
 */
static void assemble_parts(int *src_sval, int *tgt_sval, const int r_idx)
{
	/* Re-attached assemblies to assemblies */
	if ((*tgt_sval < SV_ASSEMBLY_FULL) && (*src_sval == *tgt_sval +1))
	{
		*tgt_sval += 2;
	}

	else switch (*src_sval)
	{
		case SV_ASSEMBLY_ARM_L:
			if (*tgt_sval == SV_ASSEMBLY_NONE) {*tgt_sval = SV_ASSEMBLY_ARM_L;}
			else if (*tgt_sval == SV_ASSEMBLY_ARM_R)  {*tgt_sval = SV_ASSEMBLY_ARMS;}
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARM_R)  {*tgt_sval = SV_ASSEMBLY_PART_ARMS;}
			else if (*tgt_sval == SV_ASSEMBLY_PART_HAND_R)  {*tgt_sval = SV_ASSEMBLY_MISS_HAND_L;}
			break;
		case SV_ASSEMBLY_ARM_R:
			if (*tgt_sval == SV_ASSEMBLY_NONE) {*tgt_sval = SV_ASSEMBLY_ARM_R;}
			else if (*tgt_sval == SV_ASSEMBLY_ARM_L)  {*tgt_sval = SV_ASSEMBLY_ARMS;}
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARM_L)  {*tgt_sval = SV_ASSEMBLY_PART_ARMS;}
			else if (*tgt_sval == SV_ASSEMBLY_PART_HAND_L)  {*tgt_sval = SV_ASSEMBLY_MISS_HAND_R;}
			break;
		case SV_ASSEMBLY_HAND_L:
			if (*tgt_sval == SV_ASSEMBLY_HAND_R) {*tgt_sval = SV_ASSEMBLY_HANDS;} 
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARM_L) {*tgt_sval = SV_ASSEMBLY_PART_HAND_L;} 
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARMS)  {*tgt_sval = SV_ASSEMBLY_MISS_HAND_R;}
			else if (*tgt_sval == SV_ASSEMBLY_MISS_HAND_L) {*tgt_sval = SV_ASSEMBLY_PART_HANDS;}
			else if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) &&!(r_info[r_idx].flags8 & (RF8_HAS_ARM))) {*tgt_sval = SV_ASSEMBLY_MISS_HAND_R;}
			break;
		case SV_ASSEMBLY_HAND_R:
			if (*tgt_sval == SV_ASSEMBLY_HAND_L) {*tgt_sval = SV_ASSEMBLY_HANDS;} 
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARM_R) {*tgt_sval = SV_ASSEMBLY_PART_HAND_R;} 
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARMS)  {*tgt_sval = SV_ASSEMBLY_MISS_HAND_L;}
			else if (*tgt_sval == SV_ASSEMBLY_MISS_HAND_R) {*tgt_sval = SV_ASSEMBLY_PART_HANDS;} 
			else if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_ARM))) {*tgt_sval = SV_ASSEMBLY_MISS_HAND_L;}
			break;
		case SV_ASSEMBLY_ARMS:
			if (*tgt_sval == SV_ASSEMBLY_NONE) {*tgt_sval = SV_ASSEMBLY_PART_ARMS;} 
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARM_R) {*tgt_sval = SV_ASSEMBLY_PART_ARMS; *src_sval = SV_ASSEMBLY_ARM_L; } 
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARM_L) {*tgt_sval = SV_ASSEMBLY_PART_ARMS; *src_sval = SV_ASSEMBLY_ARM_R; }
			break;
		case SV_ASSEMBLY_HANDS:
			if (*tgt_sval == SV_ASSEMBLY_PART_ARM_R) {*tgt_sval = SV_ASSEMBLY_PART_HAND_R; *src_sval = SV_ASSEMBLY_HAND_L;}
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARM_L) {*tgt_sval = SV_ASSEMBLY_PART_HAND_L; *src_sval = SV_ASSEMBLY_HAND_R;}
			else if (*tgt_sval == SV_ASSEMBLY_PART_ARMS) {*tgt_sval = SV_ASSEMBLY_PART_HANDS;}
			else if (*tgt_sval == SV_ASSEMBLY_MISS_HAND_R) {*tgt_sval = SV_ASSEMBLY_PART_HANDS; *src_sval = SV_ASSEMBLY_HAND_L;}
			else if (*tgt_sval == SV_ASSEMBLY_MISS_HAND_L) {*tgt_sval = SV_ASSEMBLY_PART_HANDS; *src_sval = SV_ASSEMBLY_HAND_R;}
			else if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_ARM))) {*tgt_sval = SV_ASSEMBLY_PART_HANDS;}
			else if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_ARM))) {*tgt_sval = SV_ASSEMBLY_PART_HAND_L; *src_sval = SV_ASSEMBLY_HAND_R;}
			else if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_ARM))) {*tgt_sval = SV_ASSEMBLY_PART_HAND_R; *src_sval = SV_ASSEMBLY_HAND_L;}
			break;
		case SV_ASSEMBLY_LEG_L:
			if (*tgt_sval == SV_ASSEMBLY_PART_HANDS) {*tgt_sval = SV_ASSEMBLY_PART_LEG_L;}
			else if (*tgt_sval == SV_ASSEMBLY_LEG_R) {*tgt_sval = SV_ASSEMBLY_LEGS;}
			else if (*tgt_sval == SV_ASSEMBLY_PART_LEG_R) {*tgt_sval = SV_ASSEMBLY_PART_LEGS;}
			else if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_ARM | RF8_HAS_HAND))) {*tgt_sval = SV_ASSEMBLY_PART_LEG_L;}
			else if ((*tgt_sval == SV_ASSEMBLY_PART_ARMS) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_HAND))) {*tgt_sval = SV_ASSEMBLY_PART_LEG_L;}
			break;
		case SV_ASSEMBLY_LEG_R:
			if (*tgt_sval == SV_ASSEMBLY_PART_HANDS) {*tgt_sval = SV_ASSEMBLY_PART_LEG_R;}
			else if (*tgt_sval == SV_ASSEMBLY_LEG_L) {*tgt_sval = SV_ASSEMBLY_LEGS;}
			else if (*tgt_sval == SV_ASSEMBLY_PART_LEG_L) {*tgt_sval = SV_ASSEMBLY_PART_LEGS;}
			else if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_ARM | RF8_HAS_HAND))) {*tgt_sval = SV_ASSEMBLY_PART_LEG_R;}
			else if ((*tgt_sval == SV_ASSEMBLY_PART_ARMS) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_HAND))) {*tgt_sval = SV_ASSEMBLY_PART_LEG_R;}
			break;
		case SV_ASSEMBLY_LEGS:
			if (*tgt_sval == SV_ASSEMBLY_PART_HANDS) {*tgt_sval = SV_ASSEMBLY_PART_LEGS;}
			else if (*tgt_sval == SV_ASSEMBLY_LEG_L) {*tgt_sval = SV_ASSEMBLY_LEGS; *src_sval = SV_ASSEMBLY_LEG_R; }
			else if (*tgt_sval == SV_ASSEMBLY_LEG_R) {*tgt_sval = SV_ASSEMBLY_LEGS; *src_sval = SV_ASSEMBLY_LEG_L; }
			else if (*tgt_sval == SV_ASSEMBLY_PART_LEG_L) {*tgt_sval = SV_ASSEMBLY_PART_LEGS; *src_sval = SV_ASSEMBLY_LEG_R; }
			else if (*tgt_sval == SV_ASSEMBLY_PART_LEG_R) {*tgt_sval = SV_ASSEMBLY_PART_LEGS; *src_sval = SV_ASSEMBLY_LEG_L; }
			else if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_ARM | RF8_HAS_HAND))) {*tgt_sval = SV_ASSEMBLY_PART_LEGS;}
			else if ((*tgt_sval == SV_ASSEMBLY_PART_ARMS) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_HAND))) {*tgt_sval = SV_ASSEMBLY_PART_LEGS;}
			break;
		case SV_ASSEMBLY_HEAD:
			if ((*tgt_sval == SV_ASSEMBLY_NONE) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_ARM | RF8_HAS_HAND | RF8_HAS_LEG))) {*tgt_sval = SV_ASSEMBLY_FULL;}
			else if ((*tgt_sval == SV_ASSEMBLY_PART_ARMS) && (r_idx) && !(r_info[r_idx].flags8 & (RF8_HAS_HAND | RF8_HAS_LEG))) {*tgt_sval = SV_ASSEMBLY_FULL;}
			break;
	}
}

static int assembly_sval;
static int assembly_name3;

/*
 * Hook to determine if an object is assembly and activatable
 */
static bool item_tester_hook_assemble(const object_type *o_ptr)
{
	u32b f1, f2, f3, f4;

	/* Not assembly */
	if (o_ptr->tval != TV_ASSEMBLY) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
 }


/*
 * Hook to determine if an object can be assembled to
 */
static bool item_tester_hook_assembly(const object_type *o_ptr)
{
	/* Make sure the same type */
	if (o_ptr->name3 != assembly_name3) return (FALSE);	

	/* Re-attached assemblies to assemblies */
	if (o_ptr->tval == TV_ASSEMBLY)
	{
		int tgt_sval = o_ptr->sval;
		int src_sval = assembly_sval;

		assemble_parts(&src_sval,&tgt_sval, assembly_name3);

		if (tgt_sval != o_ptr->sval) return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}


/*
 * Assemble an assembly.
 */
void do_cmd_assemble(void)
{
	int item, item2, lev, chance;

	object_type *o_ptr, *j_ptr;

	cptr q, s;

	int src_sval, tgt_sval = 0;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_assemble;

	/* Get an item */
	q = "Assemble which item? ";
	s = "You have nothing to assemble.";
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
#if 0
	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}
#endif
	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use monster level instead */
	if (o_ptr->name3) lev = r_info[o_ptr->name3].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if (chance < USE_DEVICE
	    || randint(chance) + chance/USE_DEVICE < USE_DEVICE)
	{
		if (flush_failure) flush();
		msg_print("You failed to understand it properly.");
		return;
	}

	/* Initialise static variables */
	assembly_name3 = o_ptr->name3;
	assembly_sval = o_ptr->sval;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_assembly;

	/* Get an item */
	q = "Assemble with which item? ";
	s = "You have nothing to assemble this with.";
	if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR))) return;
	
	/* Get the item (in the pack) */
	if (item2 >= 0)
	{
		j_ptr = &inventory[item2];
	}

	/* Get the item (on the floor) */
	else
	{
		j_ptr = &o_list[0 - item2];
	}
#if 0
	/* In a bag? */
	if (j_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, j_ptr)) return;

		/* Refer to the item */
		j_ptr = &inventory[item2];
	}
#endif

	/* Initialise svals */
	src_sval = o_ptr->sval;
	tgt_sval = j_ptr->sval;

	assemble_parts(&src_sval,&tgt_sval, o_ptr->name3);

	if (tgt_sval != j_ptr->sval)
	{
		object_type *i_ptr, *k_ptr;
		object_type object_type_body, object_type_body2;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, j_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Reset stack counter */
		i_ptr->stackc = 0;

		/* Get local object */
		k_ptr = &object_type_body2;

		/* Obtain a local object */
		object_copy(k_ptr, o_ptr);

		/* Modify quantity */
		k_ptr->number = 1;

		/* Reset stack counter */
		k_ptr->stackc = 0;

		/* Decrease the item (in the pack) */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);

			/* Hack -- handle deletion of item slot */
			if ((inventory[item].number == 0)
			   && (item < item2)
				&& (item2 < INVEN_WIELD)) item2--;

			inven_item_optimize(item);
		}
		/* Decrease the item (from the floor) */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}

		/* Decrease the item (in the pack) */
		if (item2 >= 0)
		{
			inven_item_increase(item2, -1);
			inven_item_optimize(item2);
		}
		/* Decrease the item (from the floor) */
		else
		{
			floor_item_increase(0 - item2, -1);
			floor_item_optimize(item2);
		}

		/* Modify the target item */
		i_ptr->k_idx = lookup_kind(i_ptr->tval, tgt_sval);
		i_ptr->sval = tgt_sval;
		if (src_sval != k_ptr->sval) i_ptr->weight += k_ptr->weight /2;
		else i_ptr->weight += k_ptr->weight;
		if (k_info[i_ptr->k_idx].charges) i_ptr->timeout = randint(k_info[i_ptr->k_idx].charges) + k_info[i_ptr->k_idx].charges;

		/* Adjust the weight and carry */
		if (item2 >= 0)
		{
			p_ptr->total_weight -= i_ptr->weight;
			item2 = inven_carry(i_ptr);
			inven_item_describe(item2);
		}
		else
		{
			item2 = floor_carry(p_ptr->py,p_ptr->px,i_ptr);
			floor_item_describe(item2);
		}

		if (src_sval != k_ptr->sval)
		{
			/* Modify the source item */
			k_ptr->k_idx = lookup_kind(k_ptr->tval, src_sval);
			k_ptr->sval = src_sval;
			k_ptr->weight /= 2;

			/* Adjust the weight and carry */
			if (item >= 0)
			{
				p_ptr->total_weight -= k_ptr->weight;
				item = inven_carry(k_ptr);
				inven_item_describe(item);
			}
			else
			{
				item = floor_carry(p_ptr->py,p_ptr->px,k_ptr);
				floor_item_describe(item);
			}
		}
	}
}



/*
 * Hook to determine if an object is activatable and charged
 */
static bool item_tester_hook_activate(const object_type *o_ptr)
{
	u32b f1, f2, f3, f4;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Hack -- for spells that can activate. They are always 'charging', so would never activate otherwise. */
	if ((o_ptr->tval == TV_SPELL) && (p_ptr->rest < PY_REST_FAINT)) return (FALSE);

	/* Check the recharge */
	else if ((o_ptr->timeout) && ((!o_ptr->stackc) || (o_ptr->stackc >= o_ptr->number))) return (FALSE);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Check uncontrolled flag and not cursed */
	if ((f3 & (TR3_UNCONTROLLED)) && !(cursed_p(o_ptr))) return (TRUE);

	/* Assume not */
	return (FALSE);
}



/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Not true anymore. Rings can be activated and stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
void do_cmd_activate(void)
{
	int item, power, lev, chance;

	/* Must be true to let us cancel */
	bool cancel= TRUE;
	bool known = TRUE;

	object_type *o_ptr;

	cptr q, s;

	int tmpval;

	int i;

	/* Berserk */
	if (p_ptr->shero)
	{
		msg_print("You are too enraged!");
		return;
	}

	/* Amnesia */
	if (p_ptr->amnesia)
	{
		msg_print("You have forgotten how!");
		return;
	}

	/* Hack -- prepare a fake item for innate racial abilities of the current shape */
	if (p_info[p_ptr->pshape].flags3 & (TR3_ACTIVATE))
	{
		/* Prepare a 'fake' object */
		object_prep(&inventory[INVEN_SELF], lookup_kind(TV_RACE, 0));

		/* Object is known */
		object_known(&inventory[INVEN_SELF]);

		/* Hack -- set sval */
		inventory[INVEN_SELF].sval = p_ptr->pshape;
	}

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
	q = "Activate which item? ";
	s = "You have nothing to activate.";
	if (!get_item(&item, q, s, (USE_EQUIP | (p_ptr->rest < PY_REST_FAINT ? 0 : USE_SELF)))) return;

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
#if 0
	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}
#endif
	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use artifact level instead */
	if (artifact_p(o_ptr)) lev = a_info[o_ptr->name1].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Assign speciality */
	switch (o_ptr->tval)
	{
/*		case TV_WAND:
			p_ptr->cur_style |= 1L << WS_WAND;
			break;
		case TV_STAFF:
			p_ptr->cur_style |= 1L << WS_STAFF;
			break;
*/
		case TV_AMULET:
		case TV_RING:
		  /* Already assigned, because wielded */
			break;
	}

	/* Hack -- Check for speciality */
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if (w_info[i].benefit != WB_POWER) continue;

		if ((w_info[i].styles==0) || (w_info[i].styles & (p_ptr->cur_style & (1L << p_ptr->pstyle))))
		switch (p_ptr->pstyle)
		{
/*			case WS_WAND:
			case WS_STAFF:
*/
			case WS_AMULET:
			case WS_RING:
				chance *= 2;
				break;
		}
	}

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if (chance < USE_DEVICE
	    || randint(chance) + chance/USE_DEVICE < USE_DEVICE)
	{
		if (flush_failure) flush();
		msg_print("You failed to activate it properly.");
		
		/* Clear styles */
		p_ptr->cur_style &= ~((1L << WS_WAND) | (1L << WS_STAFF));

		/* Clear racial activation */
		if (p_info[p_ptr->pshape].flags3 & (TR3_ACTIVATE)) object_wipe(&inventory[INVEN_SELF]);

		return;
	}

#if 0
	/* Still charging */
	if ((o_ptr->timeout) && ((!o_ptr->stackc) || (o_ptr->stackc >= o_ptr->number)))
	{
		if (flush_failure) flush();
		msg_print("It whines, glows and fades...");
		return;
	}
#endif


	/* Store charges */
	tmpval = o_ptr->timeout;

	/* Activate the artifact */
	message(MSG_ACT_ARTIFACT, 0, "You activate it...");

	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		if (a_ptr->activation)
		{
			/* Apply artifact effect */
			(void)process_spell(SOURCE_PLAYER_ACT_ARTIFACT, o_ptr->name1, a_ptr->activation, 0, &cancel, &known);
		}
		else
		{
			/* Get object effect --- choose if required */
			get_spell(&power, "use", o_ptr, FALSE);

			/* Paranoia */
			if (power < 0) return;

			/* Apply object effect */
			(void)process_spell(SOURCE_PLAYER_ACTIVATE, o_ptr->k_idx, power, 0, &cancel, &known);
		}

		/* Set the recharge time */
		if (a_ptr->randtime)
		{
			o_ptr->timeout = a_ptr->time + (byte)randint(a_ptr->randtime);
		}
		/* Set the recharge time */
		else
		{
			o_ptr->timeout = a_ptr->time;
		}

		/* We know it activates */
		object_can_flags(o_ptr,0x0L,0x0L,TR3_ACTIVATE,0x0L, item < 0);

		/* Count activations */
		if (a_ptr->activated < MAX_SHORT) a_ptr->activated++;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

	}

	/* Hack -- Other objects can be activated as well */
	else
	{
		int power;

		/* Get object effect --- choose if required */
		get_spell(&power, "use", o_ptr, TRUE);

		/* Clear racial activation */
		if (p_info[p_ptr->pshape].flags3 & (TR3_ACTIVATE)) object_wipe(&inventory[INVEN_SELF]);

		/* Paranoia */
		if (power < 0) return;

		/* Apply object effect */
		(void)process_spell(SOURCE_PLAYER_ACTIVATE, o_ptr->k_idx, power, 0, &cancel, &known);

		/* Used the object */
		if (k_info[o_ptr->k_idx].used < MAX_SHORT) k_info[o_ptr->k_idx].used++;

		/* Tire the player */
		if ((item == INVEN_SELF) || (o_ptr->tval == TV_SPELL))
		{
			/* Tire out the player */
			p_ptr->rest -= PY_REST_FAINT;

			/* Redraw stuff */
			p_ptr->redraw |= (PR_STATE);
		}

		/* Time object out */
		else if (o_ptr->charges) o_ptr->timeout = rand_int(o_ptr->charges)+o_ptr->charges;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}

	/* Hack -- check if we are stacking rods */
	if ((o_ptr->timeout > 0) && (!(tmpval) || stack_force_times))
	{
		/* Hack -- one more rod charging */
		if (o_ptr->timeout) o_ptr->stackc++;

		/* Reset stack count */
		if (o_ptr->stackc == o_ptr->number) o_ptr->stackc = 0;

		/* Hack -- always use maximum timeout */
		if (tmpval > o_ptr->timeout) o_ptr->timeout = tmpval;

		return;
	}

	/* XXX Hack -- unstack if necessary */
	if ((o_ptr->number > 1) && (o_ptr->timeout > 0))
	{
		object_type *i_ptr;
		object_type object_type_body;

		char o_name[80];

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Clear stack counter */
		i_ptr->stackc = 0;

		/* Restore "charge" */
		o_ptr->timeout = tmpval;

		/* Unstack the used item */
		o_ptr->number--;

		/* Reset the stack if required */
		if (o_ptr->stackc == o_ptr->number) o_ptr->stackc = 0;

		/* Adjust the weight and carry */
		if (item >= 0)
		{
			p_ptr->total_weight -= i_ptr->weight;
			item = inven_carry(i_ptr);
		}
		else
		{
			item = floor_carry(p_ptr->py,p_ptr->px,i_ptr);
			floor_item_describe(item);
		}

		/* Describe */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Message */
		msg_format("You unstack your %s.",o_name);
	}

	/* Clear styles */
	p_ptr->cur_style &= ~((1L << WS_WAND) | (1L << WS_STAFF));

}



/*
 * Hook to determine if an object can be 'applied' to another object.
 */
static bool item_tester_hook_apply(const object_type *o_ptr)
{
	switch(o_ptr->tval)
	{
		case TV_FOOD:
			if (o_ptr->sval < SV_FOOD_MIN_FOOD) return (TRUE);
			break;
		case TV_POTION:
		case TV_FLASK:
		case TV_RUNESTONE:
			return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}


/*
 * Hook to determine if an object can be coated with a potion, mushroom or flask.
 */
static bool item_tester_hook_coating(const object_type *o_ptr)
{
	switch(o_ptr->tval)
	{
		case TV_SWORD:
		case TV_POLEARM:
		case TV_ARROW:
		case TV_BOLT:
			if (o_ptr->weight < 1000) return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}



/*
 * Apply rune to an object. Destroys the selected runestone and (maybe)
 * give object powers or name2 or transform into another object of its kind.
 *
 * We now also allow potions, mushrooms and flasks to be applied to swords,
 * polearms, bolts and arrows for various damaging effects. Note that for
 * balance, these only do 1/5 of the damage that throwing them would apply.
 */
void do_cmd_apply_rune_or_coating(void)
{
	int item, item2;

	object_type *o_ptr;
	object_type *j_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	cptr q, s;

	int i,ii;

	int rune;
	int tval, sval;

	bool aware = FALSE;
	bool use_feat = FALSE;
	bool brand_ammo = FALSE;
	bool split = FALSE;

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_apply;

	/* Get an item */
	q = "Apply which runestone, potion, mushroom or flask? ";
	s = "You have no runestones, potions, mushrooms or flasks.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATU))) return;

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

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Use feat */
	if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

	if (o_ptr->tval == TV_RUNESTONE)
	{
		/* Get an item to rune */
		q = "Apply rune to which item? ";
		s = "You have nothing to apply it to.";
	}
	else
	{
		/* Get an item to coat */
		q = "Apply coating to which item? ";
		s = "You have nothing to apply it to.";

		item_tester_hook = item_tester_hook_coating;
	}

	/* Get item */
	if (!get_item(&item2, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item2 >= 0)
	{
		j_ptr = &inventory[item2];
	}

	/* Get the item (on the floor) */
	else
	{
		j_ptr = &o_list[0 - item2];
	}

	/* In a bag? */
	if (j_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item2, q, s, j_ptr)) return;

		/* Refer to the item */
		j_ptr = &inventory[item2];
	}

	/* Remove coating */
	if ((coated_p(j_ptr)) && ((j_ptr->xtra1 != o_ptr->tval) || (j_ptr->xtra2 != o_ptr->sval)))
	{
		msg_format("It is %scoated with something.", o_ptr->tval == TV_RUNESTONE ? "" : "already ");

		/* Verify */
		if (!get_check("Remove the coating? "))
		{
			return;
		}

		/* Clear feeling */
		if (o_ptr->feeling == INSCRIP_COATED) o_ptr->feeling = 0;

		/* Clear charges */
		o_ptr->charges = 0;
		o_ptr->stackc = 0;
	}

	/* Can't apply if artifact or other magical item */
	if ((artifact_p(j_ptr)) || ((j_ptr->xtra1) && (j_ptr->xtra1 < OBJECT_XTRA_MIN_RUNES)))
	{
		msg_print("It has hidden powers that prevent this.");

		/* Sense the item? */
		return;
	}

	/* Get rune */
	if (o_ptr->tval == TV_RUNESTONE)
	{
		rune = o_ptr->sval;
		tval = -1;
		sval = -1;
	}
	else
	{
		rune = -1;
		tval = o_ptr->tval;
		sval = o_ptr->sval;
		aware = k_info[o_ptr->k_idx].aware;
	}

	/* Overwrite runes */
	if ((j_ptr->xtra1 >= OBJECT_XTRA_MIN_RUNES)
		&& (j_ptr->xtra1 < OBJECT_XTRA_MIN_COATS)
		&& (j_ptr->xtra1 != OBJECT_XTRA_MIN_RUNES + rune))
	{
		/* Warning */
		msg_format("It has %srunes applied to it.", o_ptr->tval == TV_RUNESTONE ? "other " : "");

		/* Verify */
		if (!get_check("Overwrite them? "))
		{
			return;
		}
	}

	/* Hack -- check ammo */
	switch (j_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
			brand_ammo = TRUE;
		break;
	}

	/* Decrease the item (in the pack) */
	if (item >= 0)
	{
		if (o_ptr->number == 1) inven_drop_flags(o_ptr);

		inven_item_increase(item, -1);
		inven_item_describe(item);

		/* Hack -- handle deletion of item slot */
		if ((inventory[item].number == 0)
		   && (item < item2)
			&& (item2 < INVEN_WIELD) && (item2 >= 0))
		{
			item2--;

			/* Get the item (in the pack) */
			if (item2 >= 0)
			{
				j_ptr = &inventory[item2];
			}

			/* Get the item (on the floor) */
			else
			{
				j_ptr = &o_list[0 - item2];
			}
		}

		inven_item_optimize(item);
	}
	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
		if (use_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_USE_FEAT);
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Hack -- split stack only if required. This is dangerous otherwise as we may
	   be calling from a routine where we delete items later. XXX XXX */
	/* Mega-hack -- we allow 5 arrows/bolts to be coated per application */
	if ((j_ptr->number > 1) && ((!brand_ammo) || (j_ptr->number > 5)))
	{
		int qty = (brand_ammo) ? 5 : 1;
		split = TRUE;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, j_ptr);

		/* Modify quantity */
		i_ptr->number = qty;

		/* Reset stack counter */
		i_ptr->stackc = 0;

		/* Decrease the item (in the pack) */
		if (item2 >= 0)
		{
			/* Forget about item */
			if (j_ptr->number == qty) inven_drop_flags(j_ptr);

			inven_item_increase(item2, -qty);
			inven_item_describe(item2);
			inven_item_optimize(item2);
		}
		/* Decrease the item (from the floor) */
		else
		{
			floor_item_increase(0 - item2, -qty);
			floor_item_describe(0 - item2);
			floor_item_optimize(0 - item2);
		}

		/* Hack -- use new temporary item */
		j_ptr = i_ptr;
	}

        /*
         * Forget about it
         * Previously 'not' flags may be added.
         * Previously 'can'/'may' flags may have been removed.
         */
	j_ptr->ident &= ~(IDENT_MENTAL);
        drop_all_flags(j_ptr);

	/* Apply runestone */
	if (rune >= 0)
	{
		/* Apply runes */
		if (j_ptr->xtra1 == OBJECT_XTRA_MIN_RUNES + rune)
		{
			j_ptr->xtra2++;
		}
		else if ((j_ptr->name2) && (e_info[j_ptr->name2].runest == rune))
		{
			j_ptr->xtra1 = OBJECT_XTRA_MIN_RUNES + rune;
			j_ptr->xtra2 = e_info[j_ptr->name2].runesc + 1;
		}
		else if (k_info[j_ptr->k_idx].runest == rune)
		{
			j_ptr->xtra1 = OBJECT_XTRA_MIN_RUNES + rune;
			j_ptr->xtra2 = k_info[j_ptr->k_idx].runesc + 1;
		}
		else
		{
			j_ptr->xtra1 = OBJECT_XTRA_MIN_RUNES + rune;
			j_ptr->xtra2 = 1;
		}

		/* Update bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		for (i = 0; i<z_info->k_max; i++)
		{
			object_kind *k_ptr = &k_info[i];

			if ((k_ptr->tval == j_ptr->tval) && (k_ptr->runest == rune) && (k_ptr->runesc == j_ptr->xtra2))
			{
				msg_print("It glows brightly.");

				/* Polymorph the item */
				/* XXX We assume weight is the same ? */
				object_prep(j_ptr, i);

				/* Apply magic (allow artifacts) */
				apply_magic(j_ptr, object_level, TRUE, FALSE, FALSE);

				/* Hack -- Set the "runes" flag */
				j_ptr->ident |= (IDENT_RUNES);

				break;
			}
		}

		for (i= 0; i<z_info->e_max; i++)
		{
			ego_item_type *e_ptr = &e_info[i];

			if ((e_ptr->runest == rune) && (e_ptr->runesc == j_ptr->xtra2))
			{
				for (ii = 0;ii < 3; ii++)
				{
					if ((e_ptr->tval[ii] == j_ptr->tval)
					 && (e_ptr->min_sval[ii]<= j_ptr->sval)
					  && (e_ptr->max_sval[ii] >= j_ptr->sval))
					{
						msg_print("It glows brightly.");

						/* Ego-ize the item */
						j_ptr->name2 = i;

						/* Hack -- Set the "runes" flag */
						j_ptr->ident |= (IDENT_RUNES);

						/* Extra powers */
						if (e_ptr->xtra)
						{
							j_ptr->xtra1 = e_ptr->xtra;
							j_ptr->xtra2 = (byte)rand_int(object_xtra_size[e_ptr->xtra]);
						}
						else
						{
							/* Hack -- Clear the 'real' runes */
							j_ptr->xtra1 = 0;
							j_ptr->xtra2 = 0;
						}

						/* Forget about it */
						drop_all_flags(j_ptr);

						/* Hack -- acquire "broken" flag */
						if (!e_ptr->cost) j_ptr->ident |= (IDENT_BROKEN);

						/* Hack -- acquire "cursed" flag */
						if (e_ptr->flags3 & (TR3_LIGHT_CURSE)) j_ptr->ident |= (IDENT_CURSED);

						/* Hack -- apply extra penalties if needed */
						if (cursed_p(j_ptr) || broken_p(j_ptr))
						{
							/* Hack -- obtain penalties */
							if (e_ptr->max_to_h > 0) j_ptr->to_h = MIN(j_ptr->to_h,-randint(e_ptr->max_to_h));
							else j_ptr->to_h = MIN(j_ptr->to_h,0);

							if (e_ptr->max_to_d > 0) j_ptr->to_d = MIN(j_ptr->to_d,-randint(e_ptr->max_to_d));
							else j_ptr->to_d = MIN(j_ptr->to_d,0);

							if (e_ptr->max_to_a > 0) j_ptr->to_a = MIN(j_ptr->to_a,-randint(e_ptr->max_to_a));
							else j_ptr->to_a = MIN(j_ptr->to_a,0);

							/* Hack -- obtain charges */
							if (e_ptr->max_pval > 0) j_ptr->pval = MIN(j_ptr->pval,-randint(e_ptr->max_pval));
						}

						/* Hack -- apply extra bonuses if needed */
						else
						{
							/* Hack -- obtain bonuses */
							if (e_ptr->max_to_h > 0) j_ptr->to_h = MAX(j_ptr->to_h, randint(e_ptr->max_to_h));
							else j_ptr->to_h = MIN(j_ptr->to_h, 0);

							if (e_ptr->max_to_d > 0) j_ptr->to_d = MIN(MAX(j_ptr->to_d, randint(e_ptr->max_to_d)),
												   (j_ptr->tval == TV_BOW ? 15 : j_ptr->dd * j_ptr->ds + 5));
							else j_ptr->to_d = MIN(j_ptr->to_d, 0);

							if (e_ptr->max_to_a > 0) j_ptr->to_a = MIN(MAX(j_ptr->to_a, randint(e_ptr->max_to_a)),
												   j_ptr->ac + 5);
							else j_ptr->to_a = MIN(j_ptr->to_a, 0);

							/* Hack -- obtain pval */
							if (e_ptr->max_pval > 0) j_ptr->pval = MAX(1,MIN(j_ptr->pval, randint(e_ptr->max_pval)));
							else j_ptr->pval = MIN(j_ptr->pval, 0);
						}

						/* Remove special inscription, if any */
						j_ptr->feeling = 0;

						/* Hack -- Clear the "felt" flag */
						j_ptr->ident &= ~(IDENT_SENSE);

						/* Hack -- Clear the "bonus" flag */
						j_ptr->ident &= ~(IDENT_BONUS);

						/* Hack -- Clear the "known" flag */
						j_ptr->ident &= ~(IDENT_KNOWN);

						/* Hack -- Clear the "store" flag */
						j_ptr->ident &= ~(IDENT_STORE);

						break;
					}
				}
			}
		}
	}
	/* Coat weapon */
	else
	{
		int charges = j_ptr->charges * j_ptr->number + j_ptr->stackc;

		/* This is a lot simpler */
		j_ptr->xtra1 = tval;
		j_ptr->xtra2 = sval;

		if (!aware) j_ptr->feeling = INSCRIP_COATED;

		/* Based on the weight, determine charges */
		j_ptr->charges = (charges + 1000 / j_ptr->weight) / j_ptr->number;
		j_ptr->stackc = (charges + 1000 / j_ptr->weight) % j_ptr->number;

		if (j_ptr->stackc) j_ptr->charges++;
	}

	/* Notice obvious flags again */
	object_obvious_flags(j_ptr, item2 < 0);

	/* Need to carry the new object? */
	if (split)
	{
	  /* If taken from the quiver, try to combine or put in the quiver */
	  if (item2 >= INVEN_QUIVER)
	    if (quiver_carry(j_ptr, -1))
	      {
		/* FIXME: use a proper quiver_carry and do not wipe below */

		/* Wipe the object */
		object_wipe(j_ptr);

		return;
	      }

	  /* Carry the new item */
	  if (item2 >= 0)
	    {
	      item = inven_carry(j_ptr);
	      inven_item_describe(item);
	    }
	  else
	    {
	      item = floor_carry(p_ptr->py,p_ptr->px,j_ptr);
	      floor_item_describe(item);
	    }
	}
}
