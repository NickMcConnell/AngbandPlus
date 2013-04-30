/* File: cmd6.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
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
	item_tester_tval = TV_FOOD;

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

	/* Use feat */
	if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

	/* Sound */
	sound(MSG_EAT);


	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

	/* Identity not known yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Get food effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Paranoia */
	if (power < 0) return;

	/* Apply food effect */
	if (process_spell_eaten(power,0,&cancel)) ident = TRUE;

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

	if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;


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

	if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

	/* Sound */
	sound(MSG_QUAFF);


	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Get potion effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Paranoia */
	if (power < 0) return;

	/* Apply food effect */
	if (process_spell_eaten(power,0,&cancel)) ident = TRUE;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item has been tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;

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
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
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

	/* Use feat */
	if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Get scroll effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Paranoia */
	if (power < 0) return;

	/* Apply scroll effect */
	ident = process_spell(power, 0, &cancel);

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

	if ((ident) && (k_info[o_ptr->k_idx].used < MAX_SHORT)) k_info[o_ptr->k_idx].used++;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- allow certain scrolls to be "preserved" */
	if (cancel) return;

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

	object_type *o_ptr;

	cptr q, s;

	int i;

	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item */
	q = "Use which staff? ";
	s = "You have no staff to use.";
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


	/* Mega-Hack -- refuse to use a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the staffs.");
		return;
	}


	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Check for speciality */
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if (w_info[i].benefit != WB_ID) continue;

		/* Check for styles */
		if ((w_info[i].styles==WS_STAFF) && (p_ptr->pstyle == WS_STAFF))
		{
			chance *=2;
		}
	}

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Some rooms only give a (slight) chance */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		/* Special rooms affect some of this */
		int by = p_ptr->py/BLOCK_HGT;
		int bx = p_ptr->px/BLOCK_HGT;

		/* Get the room */
		if(room_info[dun_room[by][bx]].flags & (ROOM_STATIC))
		{
			chance = USE_DEVICE;

			/* Warn the player */
			msg_print("There is a static feeling in the air.");
		}
	}    

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
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
		o_ptr->ident |= (IDENT_SENSE);
		o_ptr->discount = (INSCRIP_EMPTY);
		return;
	}


	/* Sound */
	sound(MSG_ZAP);


	/* Get rod effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Paranoia */
	if (power < 0) return;

	/* Apply rod effect */
	ident = process_spell(power, 0, &cancel);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
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
		o_ptr->pval--;

		/* Reset the stack count */
		o_ptr->stackc = 0;
	}

	/* XXX Hack -- unstack if necessary */
	if ((o_ptr->number > 1) &&
	((!variant_pval_stacks) || 
	((!object_known_p(o_ptr) && (o_ptr->pval == 2) && (o_ptr->stackc > 1)) ||
	  (!object_known_p(o_ptr) && (rand_int(o_ptr->number) <= o_ptr->stackc) &&
	  (o_ptr->stackc != 1) && (o_ptr->pval > 2)))))
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
			i_ptr->pval-=2;
			o_ptr->stackc--;
		}
		else if (!o_ptr->stackc)
		{
			i_ptr->pval--;
			o_ptr->pval++;
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

	object_type *o_ptr;

	cptr q, s;

	int i;

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


	/* Mega-Hack -- refuse to aim a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the wands.");
		return;
	}


	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Get the level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Check for speciality */
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if (w_info[i].benefit != WB_ID) continue;

		/* Check for styles */
		if ((w_info[i].styles==WS_WAND) && (p_ptr->pstyle == WS_WAND))
		{
			chance *=2;
		}
	}


	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Some rooms only give a (slight) chance */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		/* Special rooms affect some of this */
		int by = p_ptr->py/BLOCK_HGT;
		int bx = p_ptr->px/BLOCK_HGT;

		/* Get the room */
		if(room_info[dun_room[by][bx]].flags & (ROOM_STATIC))
		{
			chance = USE_DEVICE;

			/* Warn the player */
			msg_print("There is a static feeling in the air.");
		}
	}    

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
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
		o_ptr->ident |= (IDENT_SENSE);
		o_ptr->discount = (INSCRIP_EMPTY);
		return;
	}


	/* Sound */
	sound(MSG_ZAP);

	/* Get wand effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Paranoia */
	if (power < 0) return;

	/* Apply wand effect */
	ident = process_spell(power, 0, &cancel);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Mark it as tried */
	object_tried(o_ptr);

	/* Apply identification */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
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
		o_ptr->pval--;

		/* Reset the stack count */
		o_ptr->stackc = 0;
	}

	/* XXX Hack -- unstack if necessary */
	if ((o_ptr->number > 1) &&
	((!variant_pval_stacks) || 
	((!object_known_p(o_ptr) && (o_ptr->pval == 2) && (o_ptr->stackc > 1)) ||
	  (!object_known_p(o_ptr) && (rand_int(o_ptr->number) <= o_ptr->stackc) &&
	  (o_ptr->stackc != 1) && (o_ptr->pval > 2)))))
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
			i_ptr->pval-=2;
			o_ptr->stackc--;
		}
		else if (!o_ptr->stackc)
		{
			i_ptr->pval--;
			o_ptr->pval++;
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
static bool item_tester_hook_charged(const object_type *o_ptr)
{
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

	object_type *o_ptr;

	cptr q, s;

	int tmpval, dir;

	/* Restrict choices to rods */
	item_tester_tval = TV_ROD;

	/* Restrict choices to charged items */
	item_tester_hook = item_tester_hook_charged;

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


	/* Mega-Hack -- refuse to zap a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the rods.");
		return;
	}

	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

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

	/* Some rooms only give a (slight) chance */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		/* Special rooms affect some of this */
		int by = p_ptr->py/BLOCK_HGT;
		int bx = p_ptr->px/BLOCK_HGT;

		/* Get the room */
		if (room_info[dun_room[by][bx]].flags & (ROOM_STATIC))
		{
			chance = USE_DEVICE;

			/* Warn the player */
			msg_print("There is a static feeling in the air.");
		}
	}    

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
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

	/* Store pval */
	tmpval = o_ptr->timeout;

	/* Sound */
	sound(MSG_ZAP);

	/* Hack -- get fake direction */
	if (!object_aware_p(o_ptr) && (o_ptr->sval < SV_ROD_MIN_DIRECTION)) get_aim_dir(&dir);

	/* Get rod effect */
	get_spell(&power, "use", o_ptr, FALSE);

	/* Paranoia */
	if (power < 0) return;

	/* Apply rod effect */
	ident = process_spell(power, 0, &cancel);

	/* Time rod out */
	o_ptr->timeout = o_ptr->pval;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
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
 * Hook to determine if an object is activatable and charged
 */
static bool item_tester_hook_activate(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Check the recharge */
      if ((o_ptr->timeout) && ((!o_ptr->stackc) || (o_ptr->stackc >= o_ptr->number))) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

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

	object_type *o_ptr;

	cptr q, s;

	int tmpval;

	int i;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
	q = "Activate which item? ";
	s = "You have nothing to activate.";
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


	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

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

	/* Hack -- Check for speciality */
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if (w_info[i].benefit != WB_ID) continue;

		/* Check for styles */
		if ((w_info[i].styles==WS_WAND) && (p_ptr->pstyle == WS_WAND) && (o_ptr->tval == TV_WAND))
		{
			chance *=2;
		}
		else if ((w_info[i].styles==WS_STAFF) && (p_ptr->pstyle == WS_STAFF) && (o_ptr->tval == TV_STAFF))
		{
			chance *=2;
		}
		else if ((w_info[i].styles==WS_AMULET) && (p_ptr->pstyle == WS_AMULET) && (o_ptr->tval == TV_AMULET))
		{
			chance *=2;
		}
		else if ((w_info[i].styles==WS_RING) && (p_ptr->pstyle == WS_RING)  && (o_ptr->tval == TV_RING))
		{
			chance *=2;
		}
	}

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to activate it properly.");
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


	/* Store pval */
	tmpval = o_ptr->timeout;

	/* Activate the artifact */
	message(MSG_ZAP, 0, "You activate it...");

	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		if (a_ptr->activation)
		{
			/* Apply artifact effect */
			(void)process_spell(a_ptr->activation, 0, &cancel);
		}
		else
		{
			/* Get object effect --- choose if required */
			get_spell(&power, "use", o_ptr, FALSE);

			/* Paranoia */
			if (power < 0) return;

			/* Apply object effect */
			(void)process_spell(power, 0, &cancel);
		}

		/* Set the recharge time */
		if (a_ptr->randtime)
		{
			o_ptr->timeout = a_ptr->time + (byte)randint(a_ptr->randtime);
		}
		else
		{
			o_ptr->timeout = a_ptr->time;
		}

		/* We know it activates */
		object_can_flags(o_ptr,0x0L,0x0L,TR3_ACTIVATE);

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

		/* Paranoia */
		if (power < 0) return;

		/* Apply object effect */
		(void)process_spell(power, 0, &cancel);

		if (k_info[o_ptr->k_idx].used < MAX_SHORT) k_info[o_ptr->k_idx].used++;

		/* Time object out */
		o_ptr->timeout = rand_int(o_ptr->pval)+o_ptr->pval;

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

}

/*
 * Apply rune to an object. Destroys the selected runestone and (maybe)
 * give object powers or name2 or transform into another object of its kind.
 */
void do_cmd_apply_rune(void)
{
	int item, item2;

	object_type *o_ptr;
	object_type *j_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	cptr q, s;

	int i,ii;

	int rune;

	bool use_feat = FALSE;

	/* Restrict the choices */
	item_tester_tval = TV_RUNESTONE;

	/* Get an item */
	q = "Apply which runestone? ";
	s = "You have no runestones.";
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

	/* Use feat */
	if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

	rune = o_ptr->sval;

	/* Get an item to fill */
	q = "Apply rune to which item? ";
	s = "You have nothing to apply it to.";

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

	if ((artifact_p(j_ptr)) || ((j_ptr->xtra1) && (j_ptr->xtra1 < OBJECT_XTRA_MIN_RUNES)))
	{
		msg_print("It has hidden powers that prevent this.");

		/* Sense the item */
		return;
	}

	if ((j_ptr->xtra1 > OBJECT_XTRA_MIN_RUNES) && (j_ptr->xtra1 != OBJECT_XTRA_MIN_RUNES + rune))
	{
		/* Warning */
		msg_print("It has other runes applied to it.");

		/* Verify */
		if (!get_check("Overwrite them? "))
		{
			return;
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, j_ptr);

	/* Modify quantity */
	i_ptr->number = 1;

	/* Reset stack counter */
	i_ptr->stackc = 0;

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
		if (use_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_USE_FEAT);
	}

	/* Decrease the item (in the pack) */
	if (item2 >= 0)
	{
                /* Forget guessed information */
                if (j_ptr->number == 1) inven_drop_flags(j_ptr);

		inven_item_increase(item2, -1);
		inven_item_optimize(item2);
	}
	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item2, -1);
		floor_item_optimize(item2);
	}

        /*
         * Forget about it
         * Previously 'not' flags may be added.
         * Previously 'can'/'may' flags may have been removed.
         */
	i_ptr->ident &= ~(IDENT_MENTAL);
        drop_all_flags(i_ptr);

	if (i_ptr->xtra1 == OBJECT_XTRA_MIN_RUNES + rune)
	{
		i_ptr->xtra2++;
	}
	else if ((i_ptr->name2) && (e_info[i_ptr->name2].runest == rune))
	{
		i_ptr->xtra1 = OBJECT_XTRA_MIN_RUNES + rune;
		i_ptr->xtra2 = e_info[i_ptr->name2].runesc + 1;
	}
	else if (k_info[i_ptr->k_idx].runest == rune)
	{
		i_ptr->xtra1 = OBJECT_XTRA_MIN_RUNES + rune;
		i_ptr->xtra2 = k_info[i_ptr->k_idx].runesc + 1;
	}
	else
	{
		i_ptr->xtra1 = OBJECT_XTRA_MIN_RUNES + rune;
		i_ptr->xtra2 = 1;
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

		if ((k_ptr->tval == i_ptr->tval) && (k_ptr->runest == rune) && (k_ptr->runesc == i_ptr->xtra2))
		{
			msg_print("It glows brightly.");

			/* Polymorph the item */
			/* XXX We assume weight is the same ? */
			object_prep(i_ptr, i);

			/* Apply magic (allow artifacts) */
			apply_magic(i_ptr, object_level, TRUE, FALSE, FALSE);

			/* Add runes */
			i_ptr->xtra1 = OBJECT_XTRA_MIN_RUNES + rune;
			i_ptr->xtra2 = k_ptr->runesc;

			/* Remove special inscription, if any */
			if (i_ptr->discount >= INSCRIP_NULL) i_ptr->discount = 0;

			/* Hack -- Clear the "felt" flag */
			i_ptr->ident &= ~(IDENT_SENSE);

			/* Hack -- Clear the "bonus" flag */
			i_ptr->ident &= ~(IDENT_BONUS);

			/* Hack -- Clear the "store" flag */
			i_ptr->ident &= ~(IDENT_STORE);

			/* Hack -- Clear the "known" flag */
			i_ptr->ident &= ~(IDENT_KNOWN);

			break;
		}
	}

	for (i= 0; i<z_info->e_max; i++)
	{
		ego_item_type *e_ptr = &e_info[i];

		if ((e_ptr->runest == rune) && (e_ptr->runesc == i_ptr->xtra2))
		{
			for (ii = 0;ii < 3; ii++)
			{
				if ((e_ptr->tval[ii] == i_ptr->tval)
				 && (e_ptr->min_sval[ii]<= i_ptr->sval)
				  && (e_ptr->max_sval[ii] >= i_ptr->sval))
				{
					msg_print("It glows brightly.");

					/* Ego-ize the item */
					i_ptr->name2 = i;

					/* Extra powers */
					if (e_ptr->xtra)
					{
						i_ptr->xtra1 = e_ptr->xtra;
						i_ptr->xtra2 = (byte)rand_int(object_xtra_size[e_ptr->xtra]);
					}

					/* Forget about it */
					drop_all_flags(o_ptr);

					/* Hack -- acquire "broken" flag */
					if (!e_ptr->cost) i_ptr->ident |= (IDENT_BROKEN);

					/* Hack -- acquire "cursed" flag */
					if (e_ptr->flags3 & (TR3_LIGHT_CURSE)) i_ptr->ident |= (IDENT_CURSED);

					/* Hack -- apply extra penalties if needed */
					if (cursed_p(i_ptr) || broken_p(i_ptr))
					{
						/* Hack -- obtain bonuses */
						if (e_ptr->max_to_h > 0) i_ptr->to_h = MIN(i_ptr->to_h,-randint(e_ptr->max_to_h));
						else i_ptr->to_h = MIN(i_ptr->to_h,0);

						if (e_ptr->max_to_d > 0) i_ptr->to_d = MIN(i_ptr->to_d,-randint(e_ptr->max_to_d));
						else i_ptr->to_d = MIN(i_ptr->to_d,0);

						if (e_ptr->max_to_a > 0) i_ptr->to_a = MIN(i_ptr->to_a,-randint(e_ptr->max_to_a));
						else i_ptr->to_a = MIN(i_ptr->to_a,0);

						/* Hack -- obtain pval */
						if (e_ptr->max_pval > 0) i_ptr->pval = MIN(i_ptr->pval,-randint(e_ptr->max_pval));
					}

					/* Hack -- apply extra bonuses if needed */
					else
					{
						/* Hack -- obtain bonuses */
						if (e_ptr->max_to_h > 0) i_ptr->to_h = MAX(i_ptr->to_h,randint(e_ptr->max_to_h));
						else i_ptr->to_h = MIN(i_ptr->to_h,0);

						if (e_ptr->max_to_d > 0) i_ptr->to_d = MAX(i_ptr->to_d,randint(e_ptr->max_to_d));
						else i_ptr->to_d = MIN(i_ptr->to_d,0);

						if (e_ptr->max_to_a > 0) i_ptr->to_a = MAX(i_ptr->to_a,randint(e_ptr->max_to_a));
						else i_ptr->to_a = MIN(i_ptr->to_a,0);

						/* Hack -- obtain pval */
						if (e_ptr->max_pval > 0) i_ptr->pval = MAX(1,MIN(i_ptr->pval,randint(e_ptr->max_pval)));
						else i_ptr->pval = MIN(i_ptr->pval,0);
					}

					/* Remove special inscription, if any */
					if (i_ptr->discount >= INSCRIP_NULL) i_ptr->discount = 0;

					/* Hack -- Clear the "felt" flag */
					i_ptr->ident &= ~(IDENT_SENSE);

					/* Hack -- Clear the "bonus" flag */
					i_ptr->ident &= ~(IDENT_BONUS);

					/* Hack -- Clear the "known" flag */
					i_ptr->ident &= ~(IDENT_KNOWN);

					/* Hack -- Clear the "store" flag */
					i_ptr->ident &= ~(IDENT_STORE);

					/* Hack -- Clear the "known" flag */
					i_ptr->ident &= ~(IDENT_KNOWN);

					break;
				}
			}
		}
	}

	/* Carry the new item */
	if (item2 >= 0)
	{
		item = inven_carry(i_ptr);
		inven_item_describe(item);
	}
	else
	{
		item = floor_carry(p_ptr->py,p_ptr->px,i_ptr);
		floor_item_describe(item);
	}

}
