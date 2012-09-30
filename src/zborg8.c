/* File: zborg8.c */
/* Purpose: High level functions for the Borg -BEN- */

#include "angband.h"

#ifdef ALLOW_BORG

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"
#include "zborg4.h"
#include "zborg5.h"
#include "zborg6.h"
#include "zborg7.h"
#include "zborg8.h"


/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow wands/staffs (if they are known to have equal
 * charges) and rods (if fully charged) to combine.
 *
 * Note that rods/staffs/wands are then unstacked when they are used.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests never stack (for various reasons).
 *
 * We do NOT allow activatable items (artifacts or dragon scale mail)
 * to stack, to keep the "activation" code clean.  Artifacts may stack,
 * but only with another identical artifact (which does not exist).
 *
 * Ego items may stack as long as they have the same ego-item type.
 * This is primarily to allow ego-missiles to stack.
 */
static bool borg_object_similar(list_item *l_ptr, list_item *q_ptr)
{
	/* Require identical object types */
	if (l_ptr->k_idx != q_ptr->k_idx) return (FALSE);

	/* Maximal "stacking" limit */
	if (q_ptr->number + 1 >= MAX_STACK_SIZE) return (FALSE);


	/* Analyze the items */
	switch (l_ptr->tval)
	{
		case TV_CHEST:
		{
			/* Chests */

			/* Never okay */
			return (FALSE);
		}

		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		case TV_ROD:
		{
			/* Food and Potions and Scrolls and Rods */

			/* Assume okay */
			break;
		}

		case TV_STAFF:
		{
			/* Staffs */

			/* Require knowledge */
			if (!borg_obj_known_p(l_ptr) ||
				!borg_obj_known_p(q_ptr)) return (FALSE);

			/* Require identical charges */
			if (l_ptr->pval != q_ptr->pval) return (FALSE);

			/* Probably okay */
			break;
		}
		case TV_WAND:
		{
			/* Wands */

			/* Require equal knowledge */
			if (borg_obj_known_p(l_ptr) !=
				borg_obj_known_p(q_ptr)) return (FALSE);

			/* Probably okay */
			break;
		}

		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			/* Weapons and Armor never stack */

			return (FALSE);
		}

		case TV_LITE:
		{
			/* Lights */

			/* Only Torches can stack */
			if (k_info[l_ptr->k_idx].sval != SV_LITE_TORCH) return (FALSE);

			/* Require identical charges */
			if (l_ptr->timeout != q_ptr->timeout) return (FALSE);

			/* Probably okay */
			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
			/* Rings, Amulets */

			/* Require full knowledge of both items */
			if (!borg_obj_known_p(l_ptr) ||
				!borg_obj_known_p(q_ptr)) return (FALSE);

			/* Fall through */
		}

		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Missiles */

			/* Require identical "bonuses" */
			if (l_ptr->to_h != q_ptr->to_h) return (FALSE);
			if (l_ptr->to_d != q_ptr->to_d) return (FALSE);
			if (l_ptr->to_a != q_ptr->to_a) return (FALSE);

			/* Require identical "pval" code */
			if (l_ptr->pval != q_ptr->pval) return (FALSE);

			/* Hack --  items with hidden flags don't stack */
			if (borg_obj_star_id_able(l_ptr)) return (FALSE);

			/* Hack -- Never stack "powerful" items */
			if (l_ptr->kn_flags[0] || q_ptr->kn_flags[0]) return (FALSE);
			if (l_ptr->kn_flags[1] || q_ptr->kn_flags[1]) return (FALSE);
			if (l_ptr->kn_flags[2] || q_ptr->kn_flags[2]) return (FALSE);

			/* Require identical "values" */
			if (l_ptr->ac != q_ptr->ac) return (FALSE);
			if (l_ptr->dd != q_ptr->dd) return (FALSE);
			if (l_ptr->ds != q_ptr->ds) return (FALSE);

			/* Probably okay */
			break;
		}

		default:
		{
			/* Various */

			/* Require knowledge */
			if ((!borg_obj_known_p(l_ptr) || !borg_obj_known_p(q_ptr)))
				return (FALSE);

			/* Probably okay */
			break;
		}
	}


	/* Hack -- Require identical "broken" status */
	if (borg_obj_known_full(l_ptr) != borg_obj_known_full(q_ptr))
	{
		return (FALSE);
	}

	/* They match, so they must be similar */
	return (TRUE);
}


/*
 * This file handles the highest level goals, and store interaction.
 *
 * Store interaction strategy
 *
 *   (1) Sell items to the home (for later use)
 *  optimize the stuff in the home... this involves buying and selling stuff
 *  not in the 'best' list.
 *       We sell anything we may need later (see step 4)
 *
 *   (2) Sell items to the shops (for money)
 *       We sell anything we do not actually need
 *
 *   (3) Buy items from the shops (for the player)
 *       We buy things that we actually need
 *
 *   (4) Buy items from the home (for the player)
 *       We buy things that we actually need (see step 1)
 *
 *   (5) Buy items from the shops (for the home)
 *       We buy things we may need later (see step 1)
 *
 *   (6) Buy items from the home (for the stores)
 *       We buy things we no longer need (see step 2)
 *
 *   The basic principle is that we should always act to improve our
 *   "status", and we should sometimes act to "maintain" our status,
 *   especially if there is a monetary reward.  But first we should
 *   attempt to use the home as a "stockpile", even though that is
 *   not worth any money, since it may save us money eventually.
 */


/*
 * Sell items to the current shop
 */
static void borg_think_shop_sell(int item, list_item *l_ptr)
{
	/* Log */
	borg_note("# Selling %s (%c)", l_ptr->o_name, I2A(item));

	/* One item */
	borg_keypress('0');
	borg_keypress('1');

	/* Sell an item */
	borg_keypress('s');

	/* Sell the desired item */
	borg_keypress(I2A(item));

	/* If the user likes this option */
	if (check_transaction) borg_keypress('y');

	/* Increment 'use' count */
	borg_shops[shop_num].u_count++;

	/* The purchase is complete */
	goal_shop = -1;
}


/*
 * Buy items from the current shop
 */
static void borg_think_shop_buy(int item)
{
	list_item *l_ptr = &cur_list[item];

	/* Is the borg on page 1 but wants to be one page 2? */
	if ((borg_term_text_comp(26, 5, "1") && item >= STORE_INVEN_MAX / 2) ||
		(borg_term_text_comp(26, 5, "2") && item <  STORE_INVEN_MAX / 2))
	{
		/* Goto the other page */
		borg_keypress(' ');
	}

	/* Log */
	borg_note("# Buying %s (%i gold).", l_ptr->o_name, l_ptr->cost);

	/* Buy one item */
	borg_keypress('0');
	borg_keypress('1');

	/* Buy an item */
	borg_keypress('p');

	/* Buy the desired item */
	borg_keypress(I2A(item % (STORE_INVEN_MAX / 2)));

	/* Increment 'use' count */
	borg_shops[shop_num].u_count++;

	/* The purchase is complete */
	goal_shop = -1;
}


/* Test to see if the item can be merged with anything in the home. */
static bool borg_can_merge_home(list_item *l_ptr)
{
	int i;

	/* Scan the home for matching items */
	for (i = 0; i < home_num; i++)
	{
		/* Can they stack? */
		if (borg_object_similar(l_ptr, &borg_home[i])) return (TRUE);
	}

	/* No match */
	return (FALSE);
}


/*
 * This will see what single addition is best for the home.
 */
static int borg_think_home_sell_aux2(void)
{
	list_item *l_ptr;
	int i, b_i = -1;

	s32b p, b_p, b_s = 0;

	/* Evaluate the home */
	b_p = g_power + g_power_home;

	/* If the home is almost full */
	if (home_num >= STORE_INVEN_MAX - 1)
	{
		/* Find out how much value will be lost if one item is taken from the home */
		for (i = 0; i < home_num; i++)
		{
			list_item *l_ptr = &borg_home[i];

			/* Remove the item */
			l_ptr->treat_as = TREAT_AS_GONE;

			/* Evaluate the home */
			p = borg_power() + borg_power_home();

			/* Restore item */
			l_ptr->treat_as = TREAT_AS_NORM;

			/* Ignore "bad" sales */
			if (p < b_s) continue;

			/* Maintain the "best" */
			b_s = p;
		}

		/* Up the minimum a bit */
		if (b_s < b_p) b_p += b_p - b_s;

	}

	/* Loop through all the items */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		/* Is there room for this item at home? */
		if (home_num >= STORE_INVEN_MAX && !borg_can_merge_home(l_ptr)) continue;

		/* Chuck item out of the inv */
		l_ptr->treat_as = TREAT_AS_LESS;

		/* Evaluate the new power */
		p = borg_power() + borg_power_home();

		/* Restore item */
		l_ptr->treat_as = TREAT_AS_NORM;

		/* Track best */
		if (b_p >= p) continue;

		/* Save the results */
		b_i = i;
		b_p = p;
	}

	/* Set the internally kept stats correctly */
	g_power = borg_power();
	g_power_home = borg_power_home();

	/* Item to give to home, if any. */
	return (b_i);
}


/*
 * Step 1 -- sell "useful" things to the home (for later)
 */
static bool borg_think_home_sell_aux(void)
{
	int index;

	/* Hack - we need to have visited the home before */
	if (home_shop == -1) return (FALSE);

	/* Find best item to give to home. */
	index = borg_think_home_sell_aux2();

	/* Do we have an item? */
	if (index != -1)
	{
		goal_shop = home_shop;

		borg_think_shop_sell(index, &inventory[index]);

		/* We have goal */
		return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}


/*
 * Determine if an item can be sold in the given store
 *
 * XXX XXX XXX Consider use of "icky" test on items
 */
static bool borg_good_sell(list_item *l_ptr)
{
	/* Just making sure */

	if (!l_ptr) return (FALSE);

	/* Never sell worthless items */
	if (l_ptr->cost <= 0) return (FALSE);

	/* Analyze the type */
	switch (l_ptr->tval)
	{
		case TV_POTION:
		case TV_SCROLL:
		{
			/* Never sell if not "known" and interesting */
			if (!borg_obj_known_p(l_ptr) &&
				bp_ptr->max_depth > 20) return (FALSE);

			break;
		}

		case TV_ROD:
		case TV_WAND:
		case TV_STAFF:
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		{
			/* Never sell if not "known" */
			if (!borg_obj_known_p(l_ptr)) return (FALSE);

			break;
		}

		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			/* Worthless items can be sold */
			if (borg_worthless_item(l_ptr)) return (TRUE);

			/* Unknown items should not be sold */
			if (!borg_obj_known_p(l_ptr)) return (FALSE);

			break;
		}
	}

	/* Do not sell an artifact that is not fully id'd and should be  */
	if (!borg_obj_known_full(l_ptr) && KN_FLAG(l_ptr, TR_INSTA_ART))
	{
		/* *identify* this item first */
		if (borg_gold < 300000) return (FALSE);
	}

	/* Assume we can */
	return (TRUE);
}


/*
 * Step 2 -- sell "useless" items to a shop (for cash)
 */
static bool borg_think_shop_sell_aux(int shop)
{
	int i, b_i = -1;
	s32b p, b_p = 0L;
	s32b c = 0L;
	s32b b_c = 30001L;

	/* Evaluate */
	b_p = g_power;

	/* Sell stuff */
	for (i = 0; i < inven_num; i++)
	{
		list_item *l_ptr = &inventory[i];

		/* Skip "bad" sales */
		if (!borg_good_sell(l_ptr)) continue;

		/* Give the item to the shop */
		l_ptr->treat_as = TREAT_AS_LESS;

		/* Evaluate the inventory with this item gone */
		p = borg_power();

		/* Hack - it is good to sell unknown stuff */
		if (!l_ptr->k_idx)
		{
			if (bp_ptr->lev < 10)
			{
				p += 100;
			}
		}

		/* Restore the item */
		l_ptr->treat_as = TREAT_AS_NORM;

		/* Ignore "bad" sales */
		if (p < b_p) continue;

		/* Extract the "price" */
		c = ((l_ptr->cost < 30000L) ? l_ptr->cost : 30000L);

		/*
		 * Sell cheap items first.
		 * This is done because we may have to buy the item back
		 * in some very strange circumstances.
		 */
		if (p == b_p && c >= b_c) continue;

		/* Maintain the "best" */
		b_i = i;
		b_p = p;
		b_c = c;
	}

	/* Set the internally kept stats correctly */
	g_power = borg_power();

	/* Sell nothing */
	if (b_i == -1) return (FALSE);

	/* Visit that shop */
	goal_shop = shop;

	/* Sell that item */
	borg_think_shop_sell(b_i, &inventory[b_i]);

	/* Success */
	return (TRUE);
}

/*
 * What is the power after wielding the item
 *  into the requested slot?
 */
static s32b borg_think_buy_slot(list_item *l_ptr, int slot, bool home)
{
	list_item *q_ptr = look_up_equip_slot(slot);

	s32b p;

	/* Paranoia */
	if (q_ptr &&
		(KN_FLAG(q_ptr, TR_CURSED) ||
		KN_FLAG(q_ptr, TR_HEAVY_CURSE) ||
		KN_FLAG(q_ptr, TR_PERMA_CURSE)))
	{
		/* Return 'bad' value */
		return (0);
	}

	/* Swap items */
	if (q_ptr) q_ptr->treat_as = TREAT_AS_SWAP;
	l_ptr->treat_as = TREAT_AS_SWAP;

	/* Evaluate the inventory */
	p = borg_power();

	/* Examine the home */
	if (home) p += borg_power_home();

	/* Fix items */
	if (q_ptr) q_ptr->treat_as = TREAT_AS_NORM;
	l_ptr->treat_as = TREAT_AS_NORM;

	/* Return power */
	return (p);
}


/* Make sure the borg keeps money back for food and light and recall */
static bool borg_spends_gold_okay(list_item *l_ptr)
{
	/* Always allow buying fuel */
	if (l_ptr->tval == TV_LITE ||
		l_ptr->tval == TV_FLASK) return (TRUE);

	/* If the borg is low on money and may need to refuel */
	if (borg_gold < 20 &&
		!KN_FLAG(&equipment[EQUIP_LITE], TR_INSTA_ART)) return (FALSE);

	/* Allow food to be bought when there is more than 20 gold */
	if (l_ptr->tval == TV_FOOD ||
		(l_ptr->tval == TV_SCROLL &&
		k_info[l_ptr->k_idx].sval == SV_SCROLL_SATISFY_HUNGER)) return (TRUE);

	/* If the borg is low on money and does not have a food spell */
	if (borg_gold < 80 &&
		!borg_spell_legal_fail(REALM_LIFE, 0, 7, 65) &&
		!borg_spell_legal_fail(REALM_ARCANE, 2, 6, 65) &&
		!borg_spell_legal_fail(REALM_NATURE, 0, 3, 65) &&
		!borg_racial_check(RACE_HOBBIT, TRUE)) return (FALSE);

	if (l_ptr->tval == TV_SCROLL &&
		k_info[l_ptr->k_idx].sval == SV_SCROLL_WORD_OF_RECALL) return (TRUE);

	/* If the borg is low on money and does not have a recall spell */
	if (borg_gold < 200 &&
		!borg_equips_rod(SV_ROD_RECALL) &&
		!borg_spell_legal_fail(REALM_SORCERY, 2, 7, 60) &&
		!borg_spell_legal_fail(REALM_ARCANE, 3, 6, 60) &&
		!borg_spell_legal_fail(REALM_TRUMP, 1, 6, 60) &&
		!borg_mutation_check(MUT1_RECALL, TRUE)) return (FALSE);

	/* Spend away */
	return (TRUE);
}

/* Step 3 -- buy "useful" things from a shop (to be used) */
static bool borg_think_shop_buy_aux(int shop)
{
	int slot;

	int n, b_n = -1;
	s32b p = 0L, b_p = 0L;
	s32b c, b_c = 0L;

	/* Require one empty slot */
	if (inven_num == INVEN_PACK) return (FALSE);

	/* Extract the "power" */
	b_p = g_power;

	/* Attempt to use shop items */
	use_shop = TRUE;

	/* Scan the wares */
	for (n = 0; n < cur_num; n++)
	{
		list_item *l_ptr = &cur_list[n];

		/* Reset value indicator */
		p = 0;

		/* second check on empty */
		if (!l_ptr->k_idx) continue;

		/* Hack - we cannot buy some items */
		if (!l_ptr->cost) continue;

		/* Hack -- Require "sufficient" cash */
		if (borg_gold < l_ptr->cost) continue;

		/* Obtain "slot" */
		slot = borg_wield_slot(l_ptr);

		/*
		 * Hack, we keep diggers as a back-up, not to
		 * replace our current weapon
		 */
		if (l_ptr->tval == TV_DIGGING) slot = -1;

		/* skip it if it has not been decursed */
		if (strstr(l_ptr->o_name, "{cursed") ||
			KN_FLAG(l_ptr, TR_CURSED) ||
			KN_FLAG(l_ptr, TR_HEAVY_CURSE)) continue;

		/* Consider new equipment */
		if (slot >= 0)
		{
			/* Get power for doing swap */
			p = borg_think_buy_slot(l_ptr, slot, FALSE);

			/* Also check if it is an item for the inventory */
		}

		/* Consider new inventory */
		if (p <= b_p)
		{
			/* Hack - use 'INVEN_LESS' to say we want it in the inventory */
			l_ptr->treat_as = TREAT_AS_LESS;

			/* Evaluate the equipment */
			p = borg_power();

			/* Fix item */
			l_ptr->treat_as = TREAT_AS_NORM;
		}

		/* Obtain the "cost" of the item */
		c = l_ptr->cost;

		/* Is it too costly? */
		if (!borg_spends_gold_okay(l_ptr)) continue;

		/* Penalize the cost of expensive items */
		if (c > borg_gold / 10) p -= c;

		/* Ignore "bad" purchases */
		if (p < b_p) continue;

		/* Ignore "expensive" purchases */
		if (p == b_p && c >= b_c) continue;

		/* Save the item and cost */
		b_n = n;
		b_p = p;
		b_c = c;
	}

	/* Use normal items */
	use_shop = FALSE;

	/* Set the internally kept stats correctly */
	g_power = borg_power();

	/* Buy something */
	if (b_n == -1) return (FALSE);

	/* Visit that shop */
	goal_shop = shop;

	/* Buy that item */
	borg_think_shop_buy(b_n);

	/* Success */
	return (TRUE);
}


/*
 * Step 4 -- buy "useful" things from the home (to be used)
 */
static bool borg_think_home_buy_aux(void)
{
	int slot;
	int n, b_n = -1;
	s32b p = 0L, b_p = 0L;

	/* Require one empty slot */
	if (inven_num == INVEN_PACK) return (FALSE);

	/* Extract the "power" */
	b_p = g_power + g_power_home;

	/* Scan the home */
	for (n = 0; n < home_num; n++)
	{
		list_item *l_ptr = &borg_home[n];

		/* Skip empty items */
		if (!l_ptr->number) continue;

		/* Obtain "slot" */
		slot = borg_wield_slot(l_ptr);

		/* Consider new equipment */
		if (slot >= 0)
		{
			/* Get power for doing swap */
			p = borg_think_buy_slot(l_ptr, slot, TRUE);

			/* Rings can be put into two slots */
			if (slot == EQUIP_LEFT)
			{
				/* Try this ring for the other finger too. */
				p = MAX(p, borg_think_buy_slot(l_ptr, EQUIP_RIGHT, TRUE));
			}
		}

		/* Consider new inventory */
		else
		{
			/* Try to get item */
			l_ptr->treat_as = TREAT_AS_LESS;

			/* Evaluate the equipment */
			p = borg_power() + borg_power_home();

			/* Restore item */
			l_ptr->treat_as = TREAT_AS_NORM;
		}

		/* Ignore "silly" purchases */
		if (p <= b_p) continue;

		/* Save the item and cost */
		b_n = n;
		b_p = p;
	}

	/* Set the internally kept stats correctly */
	g_power = borg_power();
	g_power_home = borg_power_home();

	/* Buy nothing */
	if (b_n == -1) return (FALSE);

	/* Go to the home */
	goal_shop = home_shop;

	/* Buy that item */
	borg_think_shop_buy(b_n);

	/* Success */
	return (TRUE);
}


/*
 * Step 5 -- buy "interesting" things from a shop (to be used later)
 *
 * It is highly likely this function is buggy.  We need to
 * remember that the item is destined for the home.  (Or check
 * for placing items in the home before selling them to a shop.)
 */
static bool borg_think_shop_grab_aux(int shop)
{
	int n, b_n = -1;

	s32b s, b_s = 0L;
	s32b c, b_c = 0L;

	/* Require two empty slots */
	if (inven_num >= INVEN_PACK - 2) return (FALSE);

	/* Evaluate the home */
	b_s = g_power_home;

	/* Use shops */
	use_shop = TRUE;

	/* Scan the wares */
	for (n = 0; n < cur_num; n++)
	{
		list_item *l_ptr = &cur_list[n];

		/* Obtain the "cost" of the item */
		c = l_ptr->cost;

		/* Hack - we cannot buy some items */
		if (c <= 0) continue;

		/* Ignore too expensive items */
		if (borg_gold < c) continue;

		/* Too heavy to carry now */
		if (2 * (bp_ptr->weight + l_ptr->weight) /
			adj_str_wgt[my_stat_ind[A_STR]] >= 120) continue;

		/* Get a single item */
		l_ptr->treat_as = TREAT_AS_LESS;

		/* Evaluate the home */
		s = borg_power_home();

		/* Restore the item */
		l_ptr->treat_as = TREAT_AS_NORM;

		/* Is it too costly? */
		if (!borg_spends_gold_okay(l_ptr)) continue;

		/* Penalize expensive items */
		if (c > borg_gold / 10) s -= c;

		/* Ignore "bad" sales */
		if (s < b_s) continue;

		/* Ignore "expensive" purchases */
		if (s == b_s && c >= b_c) continue;

		/* Save the item and cost */
		b_n = n;
		b_s = s;
		b_c = c;
	}

	/* Normal power calculation */
	use_shop = FALSE;

	/* Set the internally kept stats correctly */
	g_power_home = borg_power_home();

	/* Buy something */
	if (b_n == -1) return (FALSE);

	/* Visit that shop */
	goal_shop = shop;

	/* Buy that item */
	borg_think_shop_buy(b_n);

	/* Hack - get out of the store */
	borg_keypress(ESCAPE);

	/* Success */
	return (TRUE);
}


/*
 * Step 6 -- take "useless" things from the home (to be sold)
 */
static bool borg_think_home_grab_aux(void)
{
	int n, b_n = -1;
	s32b s, b_s = 0L;

	/* Require two empty slots */
	if (inven_num >= INVEN_PACK - 2) return (FALSE);

	/* Evaluate the home */
	b_s = g_power + g_power_home;

	/* If the home is full, force one item to be picked up */
	if (home_num == STORE_INVEN_MAX) b_s = 0;

	/* Scan the home */
	for (n = 0; n < home_num; n++)
	{
		list_item *l_ptr = &borg_home[n];

		/* If the home is full */
		if (home_num == STORE_INVEN_MAX)
		{
			/* Remove the pile */
			l_ptr->treat_as = TREAT_AS_GONE;
		}
		else
		{
			/* Remove one item */
			l_ptr->treat_as = TREAT_AS_LESS;
		}

		/* Evaluate the home */
		s = borg_power() + borg_power_home();

		/* Restore item */
		l_ptr->treat_as = TREAT_AS_NORM;

		/* Ignore "bad" sales */
		if (s < b_s) continue;

		/* Maintain the "best" */
		b_n = n;
		b_s = s;
	}

	/* Set the internally kept stats correctly */
	g_power = borg_power();
	g_power_home = borg_power_home();

	/* Do nothing */
	if (b_n == -1) return (FALSE);

	/* Visit the home */
	goal_shop = home_shop;

	/* Grab that item */
	borg_think_shop_buy(b_n);

	/* Success */
	return (TRUE);
}


/* Use the weaponmaster to *id* weapons */
static bool borg_build_weaponmaster(void)
{
	int i;
	list_item *l_ptr = NULL;

	/* Not when the borg is broke */
	if (borg_gold < 2000) return (FALSE);

	/* Loop through the inv */
	for (i = 0; i < inven_num + 1; i++)
	{
		if (i == 0)
		{
			/* First try the weapon that is used */
			l_ptr = look_up_equip_slot(EQUIP_WIELD);
		}
		else
		{
			/* Then check out the inventory */
			l_ptr = &inventory[i - 1];
		}

		/* There is no item */
		if (!l_ptr) return (FALSE);

		/* It has to be a weapon */
		if (l_ptr->tval < TV_DIGGING || l_ptr->tval > TV_SWORD) continue;

		/* Are there unid'd items? */
		if (borg_obj_known_p(l_ptr))
		{
			/* Does this weapon need a *id*? */
			if (borg_obj_known_full(l_ptr) ||
				!borg_obj_star_id_able(l_ptr)) continue;
		}
		else
		{
			/* If the sensing shows it is interesting */
			if (!strstr(l_ptr->o_name, "{excellent") ||
				!strstr(l_ptr->o_name, "{special")) continue;
		}

		/* We found a weapon that needs *id*, but is it in the inv? */
		if (i)
		{
			/* wield this weapon */
			borg_keypress('w');
			borg_keypress(I2A(i - 1));
		}

		/* We know enough */
		break;
	}

	/* nothing interesting in the inven */
	if (!l_ptr || (i == inven_num + 1)) return (FALSE);

	/* *id* the weapon */
	borg_keypress('E');

	/* Make a note */
	borg_note("# *Identifying* %s at the weaponmaster", l_ptr->o_name);

	return (TRUE);
}



/* How to handle the recharging building */
static bool borg_build_recharge(void)
{
	int i, count = 0, max_pval = 0;
	char buf[4];
	list_item *l_ptr = NULL;

	/* Does the borg have enough gold for an id? */
	if (borg_gold < 700) return (FALSE);

	/* Loop through the inv */
	for (i = 0; i < inven_num; i++)
	{
		/* Are there unid'd items? */
		if (!borg_obj_known_p(&inventory[i])) count++;
	}

	/* Is the id worth it? */
	if (count >= 4)
	{
		/* Identify the pack */
		borg_keypress('I');

		return (TRUE);
	}

	/* Does the borg have enough gold for a recharge? */
	if (borg_gold < 2000) return (FALSE);

	/* Find out if there are wands or staffs in the inv */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		if (l_ptr->tval == TV_WAND ||
			l_ptr->tval == TV_STAFF ||
			l_ptr->tval == TV_ROD) 
		{
			/* If the wand is not identified */
			if (!borg_obj_known_p(l_ptr))
			{
				/* Identify it */
				borg_keypress('R');
				borg_keypress(I2A(i));
				borg_keypress('y');
				borg_keypress(ESCAPE);

				borg_note("# Identifying %s at the zymurgist.", l_ptr->o_name);

				/* Success at doing something */
				return (TRUE);
			}

			/* Don't bother to recharge rods */
			if (l_ptr->tval == TV_ROD) continue;

			/* What is the maximum number of charges for this staff/wand */
			max_pval = k_info[l_ptr->k_idx].pval;

			/* Find out the max charges */
			if (l_ptr->tval == TV_STAFF)
				count = max_pval - l_ptr->pval;
			else
				count = l_ptr->number * max_pval - l_ptr->pval;

			if (count > 0) break;
		}
	}

	/* Nothing to charge */
	if (!l_ptr || (i == inven_num)) return (FALSE);

	/* Get string corresponding to number */
	(void)strnfmt(buf, 4, "%d\n", count);

	/* Say what you do */
	borg_note("# Recharging %s at the zymurgist.", l_ptr->o_name);

	/* Do what you say */
	borg_keypress('R');
	borg_keypress(I2A(i));
	borg_keypresses(buf);

	return (TRUE);
}


/* The borg may try to enchant his weapon in this building */
static bool borg_build_weapon(void)
{
	int i;
	list_item *l_ptr = look_up_equip_slot(EQUIP_WIELD);

	/* Does the borg have enough gold? */
	if (borg_gold < 2000) return (FALSE);

	/* Does the borg have a weapon at all? */
	if (!l_ptr) return (FALSE);

	/* Is there improvement for the to_hit and to_dam bonus */
	if (l_ptr->to_h >= bp_ptr->lev / 5 &&
		l_ptr->to_d >= bp_ptr->lev / 3) return (FALSE);

	/* Let's go for it */
	borg_keypress('E');

	/* Find out if there are weapons in the inv */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		if (l_ptr->tval >= TV_DIGGING && l_ptr->tval <= TV_SWORD)
		{
			/* skip the inv and go to equip */
			borg_keypress('/');

			break;
		}
	}

	/* And enchant the weapon */
	borg_keypress('a');

	/* Say so */
	borg_note("# Enchanting %s at the magesmith.", l_ptr->o_name);

	return (TRUE);
}


/* How to handle the enchant armour building */
static bool borg_build_armour(void)
{
	int i, slot = -1;
	list_item *l_ptr;

	/* Does the borg have enough gold? */
	if (borg_gold < 2000) return (FALSE);

	for (i = EQUIP_BODY; i < equip_num; i++)
	{
		l_ptr = look_up_equip_slot(i);

		/* Is there armour here? */
		if (!l_ptr) continue;

		/* Is there improvement for the to_hit and to_dam bonus */
		if (l_ptr->to_a >= bp_ptr->lev / 5) continue;

		/* Found an armour */
		slot = i;

		break;
	}

	/* Was an armour found? */
	if (slot == -1) return (FALSE);

	/* Let's go for it */
	borg_keypress('E');

	/* Find out if there are armour in the inv */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		if (l_ptr->tval >= TV_BOOTS && l_ptr->tval <= TV_DRAG_ARMOR)
		{
			/* skip the inv and go to equip */
			borg_keypress('/');

			break;
		}
	}

	/* And enchant the armour */
	borg_keypress(I2A(slot));

	/* Say so */
	borg_note("# Enchanting %s at the magesmith.", l_ptr->o_name);

	return (TRUE);
}


/* How to handle the mapmaker building */
static bool borg_build_map(void)
{
	int i, j;
	int x = c_x / WILD_BLOCK_SIZE, y = c_y / WILD_BLOCK_SIZE;
	int radius = 20, sum = 0, count = 0;

	/* Check the money */
	if (borg_gold < 600) return (FALSE);

	/* Map a rough circle around the target position in the wilderness */
	for (i = x - radius; i < x + radius + 1; i++)
	{
		for (j = y - radius; j < y + radius + 1; j++)
		{
			/* In bounds? */
			if (i >= 0 && i < max_wild - 1 &&
				j >= 0 && j < max_wild - 1)
			{
				if (distance(i, j, x, y) >= radius) continue;

				sum++;

				/* Hack!  Check this location */
				if (wild[j][i].done.info & WILD_INFO_SEEN) count++;
			}
		}
	}

	/* Does the borg already know its way around here? */
	if (count * 2 > sum) return (FALSE);

	/* Look at the map */
	borg_keypress('E');

	return (TRUE);
}


/* Even the borg can be irrational */
static bool borg_build_casino(void)
{
	int wager = MIN(borg_gold / 10, bp_ptr->lev * 1000);
	char buf[7];

	/* No gambling when almost broke */
	if (borg_gold < 500) return (FALSE);

	/* Get string corresponding to number */
	(void)strnfmt(buf, 7, "%d\n", wager);

	/* In Between */
	borg_keypress('I');
	borg_keypresses(buf);
	borg_keypress('n');

	/* Craps */
	borg_keypress('C');
	borg_keypresses(buf);
	borg_keypress('n');

	/* Spin the Wheel */
	borg_keypress('S');
	borg_keypresses(buf);
	borg_keypress((char)(randint1(10) + '0'));
	borg_keypress('\n');
	borg_keypress('n');

	/* Dice Slots */
	borg_keypress('D');
	borg_keypresses(buf);
	borg_keypress('n');

	/* One visit to this building is enough */
	return (FALSE);
}



/* Get food and rest at the inn */
static bool borg_build_inn(void)
{
	int i;
	list_item *l_ptr;
	bool rest = TRUE;

	/* Is it light outside */
	rest &= (bp_ptr->hour < 6 || bp_ptr->hour > 17);
	 
	/* This is a respectable place */
	rest &= !bp_ptr->status.cut && !bp_ptr->status.poisoned;

	/* Check the vampire flag */
	rest &= !FLAG(bp_ptr, TR_HURT_LITE);

	/* Loop through the equipment */
	for (i = 0; i < equip_num; i++)
	{
		l_ptr = look_up_equip_slot(i);

		/* Check the equipment for that flag */
		if (l_ptr) rest &= !KN_FLAG(l_ptr, TR_HURT_LITE);
	}

	/* If the borg wants a rest */
	if (rest && borg_gold > 25)
	{
		/* Wait for daybreak */
		borg_keypress('R');
	}

	/* Can the borg use more food? */
	if (!bp_ptr->status.full && borg_gold > 25)
	{
		/* Have dinner */
		borg_keypress('E');
	}

	/* One pass takes care of all needs */
	return (FALSE);
}


/* How to handle the healer building */
static bool borg_build_healer(void)
{
	int i, count = 0;

	/* Expensive here */
	if (borg_gold < 15000) return (FALSE);

	/* Check the stats (but not CHR) */
	for (i = 0; i < 5; i++)
	{
		/* Count how many stats need fixing */
		if (bp_ptr->status.fixstat[i]) count++;
	}

	/* Don't bother for just one stat */
	if (count < 2) return (FALSE);

	/* Heal me */
	borg_keypress('R');

	/* One visit is enough */
	return (FALSE);
}


/* What to do inside a magetower */
static bool borg_build_magetower(void)
{
	/* No need to be here when broke */
	if (borg_gold < 2000) return (FALSE);

	/* Is the borg registered here? */
	if (borg_term_text_comp(39, 18, "Record aura"))
	{
		/* Register */
		borg_keypress('R');

		return (TRUE);
	}

	return (FALSE);
}


/* What to do inside a castle? */
static bool borg_build_castle(bool large)
{
	/* Is the borg registered here? */
	if (borg_term_text_comp(35, 19, "Request Quest"))
	{
		/* Get a quest */
		borg_keypress('R');

		if (borg_gold < 30000)
		{
			/* Deliver a message */
			borg_keypress('b');
		}
		else
		{
			if (large)
			{
				/* Find an artifact */
				borg_keypress('d');
			}
			else
			{
				/* Kill monster quest */
				borg_keypress('a');
			}
		}

		/* Success */
		return (TRUE);
	}
	/* Try to get a reward */
	else
	{
		/* Get a quest */
		borg_keypress('R');

		/* To avoid pressing this button again just leave the building */
		return (FALSE);
	}
}


/* Handle the buildings that are not stores */
static bool borg_think_building(void)
{
	/* Only the funny buildings have a type */
	switch (borg_shops[shop_num].type)
	{
		case BUILD_WEAPONMASTER: return (borg_build_weaponmaster());

		case BUILD_RECHARGE: return (borg_build_recharge());

		case BUILD_PLUS_WEAPON: return (borg_build_weapon());

		case BUILD_PLUS_ARMOUR: return (borg_build_armour());

		case BUILD_MUTATE: return (FALSE);

		case BUILD_MAP: return (borg_build_map());

		case BUILD_LIBRARY: return (FALSE);

		case BUILD_CASINO: return (borg_build_casino());

		case BUILD_INN: return (borg_build_inn());

		case BUILD_HEALER: return (borg_build_healer());

		case BUILD_MAGETOWER0:
		case BUILD_MAGETOWER1: return (borg_build_magetower());

		case BUILD_CASTLE0: return (borg_build_castle(FALSE));

		case BUILD_CASTLE1: return (borg_build_castle(TRUE));

		default: return (FALSE);
	}
}

/*
 * Deal with being in a store
 */
bool borg_think_store(void)
{
	int i;

	/* Set the internally kept stats correctly */
	g_power = borg_power();
	g_power_home = borg_power_home();

	/* Clear goal */
	goal = GOAL_NONE;

	/* Retrieve the correct place */
	for (i = 0; i < borg_shop_num; i++)
	{
		/* Find the right coords */
		if (c_x == borg_shops[i].x && c_y == borg_shops[i].y)
		{
			/* Get the shop */
			shop_num = i;

			break;
		}
	}

	/* This shop is not known! */
	if (i == borg_shop_num)
	{
		borg_oops("Unknown shop entered");
		return (FALSE);
	}

	/* Stamp the shop with a time stamp */
	borg_shops[shop_num].when = borg_t;
	borg_shops[shop_num].visit = TRUE;
	borg_towns[borg_shops[shop_num].town_num].visit = TRUE;

	/* Increment 'been' count */
	borg_shops[shop_num].b_count++;

	if (borg_shops[shop_num].type)
	{
		/* Check out the funny shops */
		if (borg_think_building()) return (TRUE);
	}
	else
	{
		/* Remove "useless" equipment */
		if (borg_unwear_stuff()) return (TRUE);

		/* Wear good stuff */
		if (borg_wear_stuff()) return (TRUE);

		/* Select what we want to do */
		if (shop_num == home_shop)
		{
			/* Step 1 -- Sell items to the home */
			if (borg_think_home_sell_aux()) return (TRUE);

			/* Step 4 -- Buy items from the home (for the player) */
			if (borg_think_home_buy_aux()) return (TRUE);

			/* Step 5 -- Grab items from the home (for the shops) */
			if (borg_think_home_grab_aux()) return (TRUE);

			borg_note("# Nothing to do at home.");
		}
		else
		{
			/* Step 2 -- Sell items to the shops */
			if (borg_think_shop_sell_aux(shop_num)) return (TRUE);

			/* Step 3 -- Buy items from the shops (for the player) */
			if (borg_think_shop_buy_aux(shop_num)) return (TRUE);

			/* Step 6 -- Buy items from the shops (for the home) */
			if (borg_think_shop_grab_aux(shop_num)) return (TRUE);

			borg_note("# Nothing to do in the store.");
		}
	}

	/* Leave the store */
	borg_keypress(ESCAPE);

	/* Assume no important shop */
	goal_shop = -1;

	/* Choose a shop to visit */
	if (borg_choose_shop()) return (TRUE);

	/* No shop */
	shop_num = -1;

	/* Done */
	return (TRUE);
}



/*
 * Hack -- perform an action in the dungeon under boosted bravery
 *
 * This function is a sub-set of the standard dungeon goals, and is
 * only executed when all of the standard dungeon goals fail, because
 * of excessive danger, or because the level is "bizarre".
 */
static bool borg_think_dungeon_brave(void)
{
	int dir;
	/*** Local stuff ***/

	/* Attack monsters */
	if (borg_attack(TRUE)) return (TRUE);

	/* Test a light beam to remove fear of an area */
	if (borg_lite_beam(TRUE, &dir))
	{
		/* Cast that beam */
		(void)borg_lite_beam(FALSE, &dir);

		/* Ready */
		return (TRUE);
	}

	/*** Flee (or leave) the level ***/

	/* Take stairs down from town */
	if (bp_ptr->depth == 0)
	{
		/* Current grid */
		map_block *mb_ptr = map_loc(c_x, c_y);

		/* Usable stairs */
		if (mb_ptr->feat == FEAT_MORE)
		{
			/* Take the stairs */
			borg_note("# Fleeing town via Stairs.");
			borg_keypress('>');

			/* If the borg leaves the wilderness */
			if (!bp_ptr->depth) borg_leave_surface();

			/* Success */
			return (TRUE);
		}
	}

	/* Return to Stairs, but not use them */
	if (goal_less)
	{
		/* Continue fleeing to stair */
		if (borg_flow_old(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs */
		if (scaryguy_on_level && !bp_ptr->depth &&
			borg_flow_stair_both(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs up */
		if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);
	}


	/* Flee the level */
	if (goal_fleeing || goal_leaving || scaryguy_on_level)
	{
		/* Hack -- Take the next stairs */
		stair_less = goal_fleeing;

		/* Only go down if fleeing or prepared. */
		stair_more = goal_fleeing;
		if (!borg_prepared(bp_ptr->depth + 1))
			stair_more = TRUE;

		/* Continue fleeing the level */
		if (borg_flow_old(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs up */
		if (stair_less)
			if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs down */
		if (stair_more)
			if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);

		/* Try to hide on a glyph if no stairs */
		if (borg_flow_glyph(GOAL_FLEE)) return (TRUE);
	}

	/* Do short looks on special levels */
	if (vault_on_level)
	{
		/* Continue flowing towards monsters */
		if (borg_flow_old(GOAL_KILL)) return (TRUE);

		/* Find a (viewable) monster */
		if (borg_flow_kill(TRUE, 35)) return (TRUE);

		/* Continue flowing towards objects */
		if (borg_flow_old(GOAL_TAKE)) return (TRUE);

		/* Find a (viewable) object */
		if (borg_flow_take(TRUE, 35)) return (TRUE);
	}

	/* Continue flowing towards monsters */
	if (borg_flow_old(GOAL_KILL)) return (TRUE);

	/* Find a (viewable) monster */
	if (borg_flow_kill(TRUE, 250)) return (TRUE);

	/* Continue flowing towards objects */
	if (borg_flow_old(GOAL_TAKE)) return (TRUE);

	/* Find a (viewable) object */
	if (borg_flow_take(TRUE, 250)) return (TRUE);

	/*** Exploration ***/

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_SHOP)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_DARK)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_XTRA)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_BORE)) return (TRUE);


	/*** Explore the dungeon ***/

	/* Explore interesting grids */
	if (borg_flow_dark(TRUE)) return (TRUE);

	/* Explore interesting grids */
	if (borg_flow_dark(FALSE)) return (TRUE);

	/* Search for secret doors */
	if (borg_flow_spastic(FALSE)) return (TRUE);

	/*** Track down old stuff ***/

	/* Chase old objects */
	if (borg_flow_take(FALSE, 250)) return (TRUE);

	/* Chase old monsters */
	if (borg_flow_kill(FALSE, 250)) return (TRUE);

	/* Search for secret doors */
	if (borg_flow_spastic(TRUE)) return (TRUE);

	/* Nothing */
	return (FALSE);
}


/*
 * Perform an action in the dungeon
 *
 * Return TRUE if a "meaningful" action was performed
 * Otherwise, return FALSE so we will be called again
 *
 * Strategy:
 *   Make sure we are happy with our "status" (see above)
 *   Attack and kill visible monsters, if near enough
 *   Open doors, disarm traps, tunnel through rubble
 *   Pick up (or tunnel to) gold and useful objects
 *   Explore "interesting" grids, to expand the map
 *   Explore the dungeon and revisit old grids
 *
 * Fleeing:
 *   Use word of recall when level is "scary"
 *   Flee to stairs when there is a chance of death
 *   Avoid "stair bouncing" if at all possible
 *
 * Note that the various "flow" actions allow the Borg to flow
 * "through" closed doors, which will be opened when he attempts
 * to pass through them, so we do not have to pursue terrain until
 * all monsters and objects have been dealt with.
 *
 * XXX XXX XXX The poor Borg often kills a nasty monster, and
 * then takes a nap to recover from damage, but gets yanked
 * back to town before he can collect his reward.
 */
bool borg_think_dungeon(void)
{
	int i, j;

	int msec = (delay_factor * delay_factor * delay_factor);

	/* Hack -- prevent clock wrapping */
	if (borg_t >= 20000000)
	{
		/* Panic */
		borg_oops("clock overflow");

		/* Oops */
		return (TRUE);
	}

	/* Add a short pause to slow the borg down for viewing */
	Term_xtra(TERM_XTRA_DELAY, msec);

	/* Prevent clock overflow */
	if (borg_t - borg_began >= 100000)
	{
		/* Start leaving */
		if (!goal_leaving)
		{
			/* Note */
			borg_note("# Leaving (boredom)");

			/* Start leaving */
			goal_leaving = TRUE;
		}

		/* Start fleeing */
		if (!goal_fleeing)
		{
			/* Note */
			borg_note("# Fleeing (boredom)");

			/* Start fleeing */
			goal_fleeing = TRUE;
		}
	}

	/* Avoid the burning sun */
	if (FLAG(bp_ptr, TR_HURT_LITE) && !FLAG(bp_ptr, TR_RES_LITE) &&
		!bp_ptr->depth &&
		bp_ptr->hour > 5 && bp_ptr->hour < 18)
	{
		/* Get out of the Sun */
		if (!goal_fleeing)
		{
			/* Flee */
			borg_note("# Avoiding Sunlight.");

			/* Ignore multipliers */
			goal_fleeing = TRUE;
		}
	}

	/* Count the awake breeders */
	for (j = 0, i = 1; i < borg_kills_nxt; i++)
	{
		borg_kill *kill = &borg_kills[i];
		
		monster_race *r_ptr;

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Skip sleeping monsters */
		if (kill->m_flags & MONST_ASLEEP) continue;
		
		r_ptr = &r_info[kill->r_idx];

		/* Count the monsters which are "breeders" */
		if (FLAG(r_ptr, RF_MULTIPLY)) j++;
	}

	/* hack -- close doors on breeder levles */
	if (j >= 8)
	{
		/* set the flag to close doors */
		breeder_level = TRUE;
	}

	/* Hack -- caution from breeders */
	if ((j >= MIN(bp_ptr->lev, 5)) && (!bp_ptr->recall || (bp_ptr->lev < 35)))
	{
		/* Ignore monsters from caution */
		if (!goal_ignoring)
		{
			/* Flee */
			borg_note("# Ignoring breeders (no recall)");

			/* Ignore multipliers */
			goal_ignoring = TRUE;
		}

		/* Start leaving */
		if (!goal_leaving)
		{
			/* Note */
			borg_note("# Leaving (no recall)");

			/* Start leaving */
			goal_leaving = TRUE;
		}

		/* Start fleeing */
		if (!goal_fleeing)
		{
			/* Note */
			borg_note("# Fleeing (no recall)");

			/* Start fleeing */
			goal_fleeing = TRUE;
		}
	}

	/* Reset avoidance */
	if (avoidance != bp_ptr->chp)
	{
		/* Reset "avoidance" */
		avoidance = bp_ptr->chp;

		/* Re-calculate danger */
		borg_danger_wipe = TRUE;
	}

	/*** crucial goals ***/

	/* Set the internally kept stats correctly */
	g_power = borg_power();
	g_power_home = borg_power_home();

	/* require light-- */
	if (!bp_ptr->cur_lite && (bp_ptr->depth >= 1))
	{
		if (goal_recalling)
		{
			/* just wait */
			borg_keypress('R');
			borg_keypress('9');
			borg_keypress('\n');
			return (TRUE);
		}

		/* attempt to refuel */
		if (borg_refuel()) return (TRUE);

		/* wear stuff and see if it glows */
		if (borg_wear_stuff()) return (TRUE);

		/* Can I recall out with a rod */
		if (!goal_recalling && borg_recall()) return (TRUE);

		/* Test for stairs */
		if (map_loc(c_x, c_y)->feat == FEAT_LESS)
		{
			/* Take it */
			borg_keypress('<');

			return (TRUE);
		}

		/* Try to flow to a lite if I can recall */
		if (bp_ptr->recall)
		{
			/* Can I recall out with a spell */
			if (borg_flow_light(GOAL_FLEE)) return (TRUE);
		}
	}

	/* Decrease the amount of time not allowed to retreat */
	if (borg_no_retreat > 0) borg_no_retreat--;

	/*** Important goals ***/

	/* Try not to die */
	if (borg_caution()) return (TRUE);

	/* Get to a non-hurting feat */
	if (borg_flow_non_hurt()) return (TRUE);

	/*** if returning from dungeon in bad shape...***/
	if (!bp_ptr->cur_lite || bp_ptr->status.cut ||
		bp_ptr->status.poisoned || bp_ptr->status.weak)
	{
		/* First try to wear something */
		if (!bp_ptr->cur_lite)
		{
			/* attempt to refuel */
			if (borg_refuel()) return (TRUE);

			/* wear stuff and see if it glows */
			if (borg_wear_stuff()) return (TRUE);

		}

		/* Recover from damage */
		if (borg_recover()) return (TRUE);

		/* Continue flowing (see below) */
		if (borg_flow_old(GOAL_TOWN)) return (TRUE);

		/* Shop for something that will help us */
		if (borg_choose_shop())
		{
			/* Try and visit a shop, if so desired */
			if (borg_flow_shop_entry(goal_shop)) return (TRUE);
		}
	}

	/* Learn useful spells immediately */
	if (borg_play_magic(FALSE)) return (TRUE);

	/* If using a digger, Wear "useful" equipment before fighting monsters */
	if (equipment[EQUIP_WIELD].tval == TV_DIGGING && borg_wear_stuff())
	{
		return (TRUE);
	}

	/* Attack monsters */
	if (borg_attack(FALSE)) return (TRUE);

	/* Wear things that need to be worn */
	if (borg_wear_stuff()) return (TRUE);

	/* Take off things that have become useless */
	if (borg_unwear_stuff()) return (TRUE);

	/* Check the light */
	if (borg_check_lite()) return (TRUE);

	/* Recover from damage */
	if (borg_recover()) return (TRUE);

	/* Perform "cool" perma spells */
	if (borg_perma_spell()) return (TRUE);

	/*** Flee the level XXX XXX XXX ***/

	/* Return to Stairs, but not use them */
	if (goal_less)
	{
		/* Continue fleeing to stair */
		if (borg_flow_old(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs */
		if (scaryguy_on_level && !bp_ptr->depth &&
			borg_flow_stair_both(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs up */
		if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);
	}

	/* Flee the level */
	if (goal_fleeing && !goal_recalling)
	{
		/* Hack -- Take the next stairs */
		stair_less = stair_more = TRUE;

		/* Continue fleeing the level */
		if (borg_flow_old(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs */
		if (scaryguy_on_level && !bp_ptr->depth &&
			borg_flow_stair_both(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs up */
		if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs down */
		if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);

		/* Try to hide on a glyph if no stairs */
		if (borg_flow_glyph(GOAL_FLEE)) return (TRUE);
	}

	/* Continue flowing towards monsters */
	if (borg_flow_old(GOAL_KILL)) return (TRUE);

	/* Find a (viewable) monster */
	if (borg_flow_kill(TRUE, 250)) return (TRUE);

	/* Find a viewable monster and line up a shot on him */
	/* Disabled because it is buggy, causing live-lock.
	if (borg_flow_kill_aim(TRUE)) return (TRUE); */

	/* Dig an anti-summon corridor */
	if (borg_flow_kill_corridor(TRUE)) return (TRUE);

	/*** Deal with inventory objects ***/

	/* Use things */
	if (borg_use_things()) return (TRUE);

	/* Try to identify things */
	if (borg_id_meta()) return (TRUE);

	/* Enchant things */
	if (borg_enchanting()) return (TRUE);

	/* Recharge things */
	if (borg_recharging()) return (TRUE);

	/* Maybe destroy an item */
	if (borg_destroy()) return (TRUE);

	/*** Flow towards objects ***/

	/* Continue flowing towards objects */
	if (borg_flow_old(GOAL_TAKE)) return (TRUE);

	/* Find a (viewable) object */
	if (borg_flow_take(TRUE, 250)) return (TRUE);


	/*** Leave the level XXX XXX XXX ***/

	/* Leave the level */
	if (goal_leaving && !goal_recalling && !unique_on_level)
	{
		/* Only go down if fleeing or prepared. */
		if (!borg_prepared(bp_ptr->depth + 1))
			stair_more = TRUE;

		/* Continue leaving the level */
		if (borg_flow_old(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs up */
		if (stair_less)
			if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs down */
		if (stair_more)
			if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);
	}


	/*** Exploration ***/

	/* Continue flowing for low importance stuff */
	if (goal >= GOAL_SHOP &&
		goal < GOAL_MAX &&
		borg_flow_old(goal)) return (TRUE);

	/*** Explore the dungeon ***/

	/* Chase close monsters */
	if (borg_flow_kill(FALSE, 35)) return (TRUE);

	/* Chase close objects */
	if (borg_flow_take(FALSE, 35)) return (TRUE);

	/* Chase old monsters */
	if (borg_flow_kill(FALSE, 250)) return (TRUE);

	/* Chase old objects */
	if (borg_flow_take(FALSE, 250)) return (TRUE);

	/* Explore interesting grids */
	if (borg_flow_dark(TRUE)) return (TRUE);

	/* Possibly leave the level (not bored) */
	if (borg_leave_level(FALSE)) return (TRUE);

	/* Explore interesting grids */
	if (borg_flow_dark(FALSE)) return (TRUE);

	/*** Deal with shops ***/
	if (borg_find_shop()) return (TRUE);

	/*** Leave the Level ***/

	/* Study/Test boring spells/prayers */
	if (!goal_fleeing && borg_play_magic(TRUE)) return (TRUE);

	/* Search for secret doors */
	if (borg_flow_spastic(FALSE)) return (TRUE);

	/* Recharge items before leaving the level */
	if (borg_wait_recharge()) return (TRUE);

	/* Leave the level (bored) */
	if (borg_leave_level(TRUE)) return (TRUE);

	/* Search for secret doors */
	if (borg_flow_spastic(TRUE)) return (TRUE);


	/*** Wait for recall ***/

	/* Wait for recall, unless in danger */
	if (goal_recalling &&
		borg_on_safe_feat(map_loc(c_x, c_y)->feat) &&
		(borg_danger(c_x, c_y, 1, TRUE) <= 0))
	{
		/* Take note */
		borg_note("# Waiting for Recall...");

		/* Rest until done */
		borg_keypress('R');
		borg_keypress('\n');

		/* Done */
		return (TRUE);
	}

	/*** Nothing to do ***/

	/* Wait for daylight */
	if (borg_waits_daylight()) return (TRUE);

	/* Try to cross the wilderness to do some fun shopping */
	if (borg_find_town()) return (TRUE);

	/* Try to cross the wilderness to find a challenging dungeon */
	if (borg_find_dungeon()) return (TRUE);

	/* Explore the wilderness */
	if (borg_flow_dark_wild()) return (TRUE);

	/* Set a flag that the borg is  not allowed to retreat for 5 rounds */
	borg_no_retreat = 5;

	/* Boost slightly */
	if (avoidance < bp_ptr->chp * 2)
	{
		bool done = FALSE;

		/* Note */
		borg_note("# Boosting bravery (1) from %d to %d!",
					  avoidance, bp_ptr->chp * 2);

		/* Hack -- ignore some danger */
		avoidance = (bp_ptr->chp * 2);

		/* Forget the danger fields */
		borg_danger_wipe = TRUE;

		/* Try anything */
		if (borg_think_dungeon_brave()) done = TRUE;

		/* Reset "avoidance" */
		avoidance = bp_ptr->chp;

		/* Re-calculate danger */
		borg_danger_wipe = TRUE;

		/* Done */
		if (done) return (TRUE);
	}

	/* Try phase before boosting bravery further and acting goofy */
	borg_times_twitch++;

	/* Phase to get out of being twitchy up to 3 times per level. */
	if (bp_ptr->depth && borg_times_twitch < 3)
	{
		borg_note("# Considering Phase (twitchy)");

		/* Phase */
		if (bp_ptr->able.phase && borg_caution_phase(15, 2) &&
			(borg_spell(REALM_SORCERY, 0, 1) ||
			 borg_spell(REALM_TRUMP, 0, 0) ||
			 borg_spell(REALM_ARCANE, 0, 4) ||
			 borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
		{
			/* Success */
			return (TRUE);
		}

	}

	/* Set a flag that the borg is not allowed */
	/*  to retreat for 10 rounds */
	borg_no_retreat = 10;

	/* Boost some more */
	if (avoidance < bp_ptr->mhp * 4)
	{
		bool done = FALSE;

		/* Note */
		borg_note("# Boosting bravery (2) from %d to %d!",
					  avoidance, bp_ptr->mhp * 4);

		/* Hack -- ignore some danger */
		avoidance = (bp_ptr->mhp * 4);

		/* Forget the danger fields */
		borg_danger_wipe = TRUE;

		/* Try anything */
		if (borg_think_dungeon_brave()) done = TRUE;

		/* Reset "avoidance" */
		avoidance = bp_ptr->chp;

		/* Re-calculate danger */
		borg_danger_wipe = TRUE;

		/* Done */
		if (done) return (TRUE);
	}

	/* Boost a lot */
	if (avoidance < 30000)
	{
		bool done = FALSE;

		/* Note */
		borg_note("# Boosting bravery (3) from %d to %d!",
					  avoidance, 30000);

		/* Hack -- ignore some danger */
		avoidance = 30000;

		/* Forget the danger fields */
		borg_danger_wipe = TRUE;

		/* Try anything */
		if (borg_think_dungeon_brave()) done = TRUE;

		/* Reset "avoidance" */
		avoidance = bp_ptr->chp;

		/* Re-calculate danger */
		borg_danger_wipe = TRUE;

		/* Done */
		if (done) return (TRUE);
	}

	/* try teleporting before acting goofy */
	borg_times_twitch++;


	/* Teleport to get out of being twitchy up to 5 times per level. */
	if (bp_ptr->depth && borg_times_twitch < 5)
	{
		borg_note("# Teleport (twitchy)");

		/* Teleport */
		if (borg_activate(BORG_ACT_TELEPORT) ||
			borg_spell_fail(REALM_ARCANE, 2, 3, 45) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, 45) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, 45) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, 45) ||
			borg_use_staff(SV_STAFF_TELEPORTATION) ||
			borg_read_scroll(SV_SCROLL_TELEPORT))
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Recall to town */
	if (bp_ptr->depth && (borg_recall()))
	{
		/* Note */
		borg_note("# Recalling (twitchy)");

		/* Success */
		return (TRUE);
	}


	/* Twitch around */
	if (borg_twitchy()) return (TRUE);

	/* Oops */
	return (FALSE);
}




/*
 * Initialize this file
 */
void borg_init_8(void)
{
	/* Nothing */
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
