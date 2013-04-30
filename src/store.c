/* File: store.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#define MAX_COMMENT_1	6

static cptr comment_1[MAX_COMMENT_1] =
{
	"Okay.",
	"Fine.",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!"
};

/*
 * Successful purchase.
 */
static void say_comment_1(void)
{
	message(MSG_STORE, 0, comment_1[rand_int(MAX_COMMENT_1)]);
}

/*
 * Messages for reacting to purchase prices.
 */

#define MAX_COMMENT_2A	4

static cptr comment_2a[MAX_COMMENT_2A] =
{
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!"
};

#define MAX_COMMENT_2B	4

static cptr comment_2b[MAX_COMMENT_2B] =
{
	"Damn!",
	"You fiend!",
	"The shopkeeper curses at you.",
	"The shopkeeper glares at you."
};

#define MAX_COMMENT_2C	4

static cptr comment_2c[MAX_COMMENT_2C] =
{
	"Cool!",
	"You've made my day!",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly."
};

#define MAX_COMMENT_2D	4

static cptr comment_2d[MAX_COMMENT_2D] =
{
	"Yipee!",
	"I think I'll retire!",
	"The shopkeeper jumps for joy.",
	"The shopkeeper smiles gleefully."
};

/*
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(s32b price, s32b value, s32b guess)
{
	/* Item was worthless, but we bought it */
	if ((value <= 0) && (price > value))
	{
		/* Comment */
		message(MSG_STORE_ANGRY, 0, comment_2a[rand_int(MAX_COMMENT_2A)]);
	}

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
	{
		/* Comment */
		message(MSG_STORE_ANGRY, 0, comment_2b[rand_int(MAX_COMMENT_2B)]);
	}

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (value < (4 * guess)) && (price < value))
	{
		/* Comment */
		message(MSG_STORE_HAPPY, 0, comment_2c[rand_int(MAX_COMMENT_2C)]);
	}

	/* Item was a great bargain, and we got away with it */
	else if ((value > guess) && (price < value))
	{
		/* Comment */
		message(MSG_STORE_HAPPY, 0, comment_2d[rand_int(MAX_COMMENT_2D)]);
	}
}

/*
 * We store the current "store number" here so everyone can access it
 */
static int store_num = (MAX_STORES - 1);

/*
 * We store the current "store page" here so everyone can access it
 */
static int store_top = 0;

/*
 * We store the current "store pointer" here so everyone can access it
 */
static store_type *st_ptr = NULL;

/*
 * We store the current "owner type" here so everyone can access it
 */
static owner_type *ot_ptr = NULL;

/*
 * Determine the price of an object (qty one) in a store.
 *
 * This function takes into account the player's charisma, and the
 * shop-keepers friendliness, and the shop-keeper's base greed, but
 * never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * object, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- the black market always charges 50% more than it should.
 *
 * Charisma adjustment runs from 80 to 130
 * Racial adjustment runs from 95 to 130
 *
 * Since greed/charisma/racial adjustments are centered at 100, we need
 * to adjust (by 200) to extract a usable multiplier.  Note that the
 * "greed" value is always something (?).
 */
static s32b price_item(const object_type *o_ptr, bool flip)
{
	int factor;
	int adjust;
	s32b price;

	/* Get the value of one of the items */
	price = object_value(o_ptr);

	/* Worthless items */
	if (price <= 0) return (0L);

	/* Compute the racial factor */
	factor = g_info[(ot_ptr->owner_race * z_info->p_max) + p_ptr->prace];

	/* Add in the charisma factor */
	factor += adj_chr_gold[p_stat(A_CHR)];

	if (adult_easy_mode) factor = (factor * 4) / 5; 

	/* Shop is buying */
	if (flip)
	{
		/* Adjust for greed */
		adjust = 100 + (300 - (ot_ptr->inflate + factor));

		/* Never get "silly" */
		if (adjust > 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (store_num == STORE_B_MARKET) price = price / 1.5;
	}

	/* Shop is selling */
	else
	{
		/* Adjust for greed */
		adjust = 100 + ((ot_ptr->inflate + factor) - 300);

		/* Never get "silly" */
		if (adjust < 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (store_num == STORE_B_MARKET) price = price * 1.5;
	}

	/* Compute the final price (with rounding) */
	price = (price * adjust + 50L) / 100L;

	/* Note -- Never become "free" */
	if (price <= 0L) return (1L);

	/* Return the price */
	return (price);
}

/*
 * Special "mass production" computation.
 */
static int mass_roll(int num, int max)
{
	int i, t = 0;
	for (i = 0; i < num; i++)
	{
		t += ((max > 1) ? rand_int(max) : 1);
	}
	return (t);
}

/*
 * Certain "cheap" objects should be created in "piles".
 *
 * Some objects can be sold at a "discount" (in smaller piles).
 *
 * Standard percentage discounts include 10, 25, 50, 75, and 90.
 */
static void mass_produce(object_type *o_ptr)
{
	int size = 1;

	int discount = 0;

	s32b cost = object_value(o_ptr);

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		case TV_POTION:
		case TV_POWDER:
		case TV_SCROLL:
		{
			if (cost <= 60L) size += mass_roll(3, 5);
			if (cost <= 240L) size += mass_roll(1, 5);
			break;
		}

		case TV_MAGIC_BOOK:
		{
			if (cost <= 50L) size += mass_roll(2, 3);
			if (cost <= 500L) size += mass_roll(1, 3);
			break;
		}

		case TV_BODY_ARMOR:
		case TV_SHIELD:
		case TV_GLOVES:
		case TV_BOOTS:
		case TV_CLOAK:
		case TV_HEADGEAR:
		case TV_SWORD:
		case TV_POLEARM:
		case TV_BLUNT:
		case TV_DIGGING:
		case TV_BOW:
		{
			if (o_ptr->e_idx) break;
			if (cost <= 10L) size += mass_roll(3, 5);
			if (cost <= 100L) size += mass_roll(3, 5);
			break;
		}

		case TV_ARROW:
		{
			if (cost <= 5L) size += mass_roll(5, 5);
			if (cost <= 50L) size += mass_roll(5, 5);
			if (cost <= 500L) size += mass_roll(5, 5);
			break;
		}
	}

	/* Pick a discount */
	if (cost < 5) discount = 0;
	else if (rand_int(25) == 0)	discount = 10;
	else if (rand_int(50) == 0)	discount = 25;
	else if (rand_int(150) == 0) discount = 50;
	else if (rand_int(300) == 0) discount = 75;
	else if (rand_int(500) == 0) discount = 90;

	/* Save the discount */
	o_ptr->discount = discount;

	/* Save the total pile size */
	o_ptr->number = size - (size * discount / 100);
}

/*
 * Convert a store item index into a one character label
 *
 * We use labels "a"-"l" for page 1, and labels "m"-"x" for page 2.
 */
static s16b store_to_label(int i)
{
	/* Assume legal */
	return (I2A(i));
}

/*
 * Convert a one character label into a store item index.
 *
 * Return "-1" if the label does not indicate a real store item.
 */
static s16b label_to_store(int c)
{
	int i;

	/* Convert */
	i = (islower((unsigned char)c) ? A2I(c) : -1);

	/* Verify the index */
	if ((i < 0) || (i >= st_ptr->stock_num)) return (-1);

	/* Return the index */
	return (i);
}

/*
 * Determine if a store object can "absorb" another object.
 *
 * See "object_similar()" for the same function for the "player".
 *
 * This function can ignore many of the checks done for the player,
 * since stores (but not the home) only get objects under certain
 * restricted circumstances.
 */
static bool store_object_similar(const object_type *o_ptr, const object_type *j_ptr)
{
	/* Hack -- Identical items cannot be stacked */
	if (o_ptr == j_ptr) return FALSE;

	/* Different objects cannot be stacked */
	if (o_ptr->k_idx != j_ptr->k_idx) return FALSE;

	/* Some special rules apply to rods and talismans */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
	{
		/* Hack -- Never stack recharging rods/talismans */
		if (o_ptr->timeout || j_ptr->timeout) return FALSE;

		/* Require identical recharge times */
		if (o_ptr->pval != j_ptr->pval) return FALSE;
	}

	/* Different charges (etc) cannot be stacked, except for rods and talismans */
	else if (o_ptr->pval != j_ptr->pval) return FALSE;
	
	/* Require many identical values */
	if (o_ptr->to_h != j_ptr->to_h) return FALSE;
	if (o_ptr->to_a != j_ptr->to_a) return FALSE;

	/* Require identical "artifact" names */
	if (o_ptr->a_idx != j_ptr->a_idx) return FALSE;

	/* Require identical "ego-item" names */
	if (o_ptr->e_idx != j_ptr->e_idx) return FALSE;

	/* Require identical "prefix" names */
	if (o_ptr->pfx_idx != j_ptr->pfx_idx) return FALSE;

	/* Hack -- Never stack "powerful" items */
	if (o_ptr->xtra1 || j_ptr->xtra1) return FALSE;

	/* Hack -- different light durations cannot be stacked */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->timeout != j_ptr->timeout)) return FALSE;

	/* Require matching "discount" fields */
	if (o_ptr->discount != j_ptr->discount) return FALSE;

	/* They match, so they must be similar */
	return TRUE;
}

/*
 * Allow a store object to absorb another object
 */
static void store_object_absorb(object_type *o_ptr, const object_type *j_ptr, bool player)
{
	int total = o_ptr->number + j_ptr->number;
	int max_num = MAX_STACK_SIZE;

	/* Calculate rods and talismans */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
	{
		/* Find appropriate stack size */
		max_num = ((o_ptr->tval == TV_ROD) ? MAX_STACK_ROD : MAX_STACK_TALIS);
	}

	/* Calculate light items */
	if (o_ptr->tval == TV_LITE)
	{
		/* Find appropriate stack size */
		max_num = MAX_STACK_LITE;
	}

	/* Combine quantity, lose excess items */
	o_ptr->number = (total >= max_num) ? (max_num - 1) : total;

	/* Try to combine histories */
	stack_histories(o_ptr, j_ptr);

	if (player && (total >= max_num))
		message(MSG_STORE, 0, "Can't have too many of these on my shelves...");
}

/*
 * Check to see if the shop will be carrying too many objects
 *
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.  Before, one could "nuke" objects this way, by
 * adding them to a pile which was already full.
 */
static bool store_check_num(const object_type *o_ptr)
{
	int i;
	object_type *j_ptr;

	/* Free space is always usable */
	if (st_ptr->stock_num < st_ptr->stock_size) return TRUE;

	/* The "home" acts like the player */
	if (store_num == STORE_HOME)
	{
		/* Check all the objects */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Get the existing object */
			j_ptr = &st_ptr->stock[i];

			/* Can the new object be combined with the old one? */
			if (object_similar(j_ptr, o_ptr)) return (TRUE);
		}
	}

	/* Normal stores do special stuff */
	else
	{
		/* Check all the objects */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Get the existing object */
			j_ptr = &st_ptr->stock[i];

			/* Can the new object be combined with the old one? */
			if (store_object_similar(j_ptr, o_ptr)) return (TRUE);
		}
	}

	/* But there was no room at the inn... */
	return (FALSE);
}

/*
 * Determine if a weapon is 'blessed'
 */
static bool is_blessed(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Get the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Is the object blessed? */
	return ((f2 & TR2_BLESSED) ? TRUE : FALSE);
}

/*
 * Determine if the current store will purchase the given object
 *
 * Note that a shop-keeper must refuse to buy "worthless" objects
 */
static bool store_will_buy(const object_type *o_ptr)
{
	/* Hack -- The Home is simple */
	if (store_num == STORE_HOME) return (TRUE);

	/* Rogues can only sell items with evaluated price >= min_depth in feet */
	if (object_value(o_ptr) < (500 * p_ptr->min_depth)) return (FALSE);

	/* Switch on the store */
	switch (store_num)
	{
		/* General Store */
		case STORE_GENERAL:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_FOOD:
				case TV_LITE:
				case TV_LITE_SPECIAL:
				case TV_FLASK:
				case TV_ARROW:
				case TV_DIGGING:
				case TV_CLOAK:
				case TV_MUSIC:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Armoury */
		case STORE_ARMOR:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_BOOTS:
				case TV_GLOVES:
				case TV_HEADGEAR:
				case TV_SHIELD:
				case TV_CLOAK:
				case TV_BODY_ARMOR:
				case TV_DRAG_ARMOR:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Weapon Shop */
		case STORE_WEAPON:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_ARROW:
				case TV_BOW:
				case TV_DIGGING:
				case TV_BLUNT:
				case TV_POLEARM:
				case TV_SWORD:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Temple */
		case STORE_TEMPLE:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_SCROLL:
				case TV_POTION:
				case TV_BLUNT:
				break;
				case TV_POLEARM:
				case TV_SWORD:
				{
					/* Known blessed blades are accepted too */
					if (is_blessed(o_ptr) && object_known_p(o_ptr)) break;
				}
				default:
				return (FALSE);
			}
			break;
		}

		/* Alchemist */
		case STORE_ALCHEMY:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_POTION:
				case TV_POWDER:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Magic Shop */
		case STORE_MAGIC:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_AMULET:
				case TV_RING:
				case TV_STAFF:
				case TV_WAND:
				case TV_ROD:
				case TV_TALISMAN:
				case TV_SCROLL:
				case TV_POTION:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Book Shop */
		case STORE_BOOK:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_MAGIC_BOOK:
				case TV_SCROLL:
				break;
				default:
				return (FALSE);
			}
			break;
		}
	}

	/* Assume okay */
	return (TRUE);
}

/*
 * Add an object to the inventory of the "Home"
 *
 * In all cases, return the slot (or -1) where the object was placed.
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" bacoming
 * known, the player may have to pick stuff up and drop it again.
 */
static int home_carry(const object_type *o_ptr)
{
	int i, slot;
	s32b value, j_value;
	object_type *j_ptr;

	/* Check each existing object (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing object */
		j_ptr = &st_ptr->stock[slot];

		/* The home acts just like the player */
		if (object_similar(j_ptr, o_ptr))
		{
			/* Save the new number of items */
			object_absorb(j_ptr, o_ptr);

			/* All done */
			return (slot);
		}
	}

	/* No space? */
	if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);

	/* Determine the "value" of the object */
	value = object_value(o_ptr);

	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get that object */
		j_ptr = &st_ptr->stock[slot];

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Can happen in the home */
		if (!object_aware_p(o_ptr)) continue;
		if (!object_aware_p(j_ptr)) break;

		/* Objects sort by increasing sval */
		if (o_ptr->sval < j_ptr->sval) break;
		if (o_ptr->sval > j_ptr->sval) continue;

		/* Objects in the home can be unknown */
		if (!object_known_p(o_ptr)) continue;
		if (!object_known_p(j_ptr)) break;

		/* Objects sort by decreasing value */
		j_value = object_value(j_ptr);
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = st_ptr->stock_num; i > slot; i--)
	{
		/* Hack -- slide the objects */
		object_copy(&st_ptr->stock[i], &st_ptr->stock[i-1]);
	}


	/* More stuff now */
	st_ptr->stock_num++;

	/* Hack -- Insert the new object */
	object_copy(&st_ptr->stock[slot], o_ptr);

	/* Return the location */
	return (slot);
}

/*
 * Add an object to a real stores inventory.
 *
 * The "player" parameter determines whether the player can see this happening.
 *
 * If the object is "worthless", it is thrown away (except in the home).
 *
 * If the object cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.  Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an object to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or -1) where the object was placed
 */
static int store_carry(object_type *o_ptr, bool player)
{
	int i, slot;
	s32b value, j_value;
	object_type *j_ptr;

	/* Evaluate the object */
	value = object_value(o_ptr);

	/* Cursed/Worthless items "disappear" when sold */
	if (value <= 0) return (-1);

	/* Erase the inscription */
	o_ptr->note = 0;

	/* Remove special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

	/* Check each existing object (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing object */
		j_ptr = &st_ptr->stock[slot];

		/* Can the existing items be incremented? */
		if (store_object_similar(j_ptr, o_ptr))
		{
			/* Absorb (some of) the object */
			store_object_absorb(j_ptr, o_ptr, player);

			/* All done */
			return (slot);
		}
	}

	/* No space? */
	if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);

	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get that object */
		j_ptr = &st_ptr->stock[slot];

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Objects sort by increasing sval */
		if (o_ptr->sval < j_ptr->sval) break;
		if (o_ptr->sval > j_ptr->sval) continue;

		/* Evaluate that slot */
		j_value = object_value(j_ptr);

		/* Objects sort by decreasing value */
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = st_ptr->stock_num; i > slot; i--)
	{
		/* Hack -- slide the objects */
		object_copy(&st_ptr->stock[i], &st_ptr->stock[i-1]);
	}

	/* More stuff now */
	st_ptr->stock_num++;

	/* Hack -- Insert the new object */
	object_copy(&st_ptr->stock[slot], o_ptr);

	/* Return the location */
	return (slot);
}

/*
 * Increase, by a given amount, the number of a certain item
 * in a certain store.  This can result in zero items.
 */
static void store_item_increase(int item, int num)
{
	int cnt;
	object_type *o_ptr;

	/* Get the object */
	o_ptr = &st_ptr->stock[item];

	/* Verify the number */
	cnt = o_ptr->number + num;
	if (cnt > 255) cnt = 255;
	else if (cnt < 0) cnt = 0;
	num = cnt - o_ptr->number;

	/* Save the new number */
	o_ptr->number += num;
}

/*
 * Remove a slot if it is empty
 */
static void store_item_optimize(int item)
{
	int j;
	object_type *o_ptr;

	/* Get the object */
	o_ptr = &st_ptr->stock[item];

	/* Must exist */
	if (!o_ptr->k_idx) return;

	/* Must have no items */
	if (o_ptr->number) return;

	/* One less object */
	st_ptr->stock_num--;

	/* Slide everyone */
	for (j = item; j < st_ptr->stock_num; j++)
	{
		st_ptr->stock[j] = st_ptr->stock[j + 1];
	}

	/* Nuke the final slot */
	object_wipe(&st_ptr->stock[j]);
}

/*
 * This function will keep 'crap' out of the black market.
 * Crap is defined as any object that is "available" elsewhere
 * Based on a suggestion by "Lee Vogt" <lvogt@cig.mcel.mot.com>
 */
static bool black_market_crap(const object_type *o_ptr)
{
	int i, j;

	/* Ego items are never crap */
	if (o_ptr->e_idx) return (FALSE);

	/* Good items are never crap */
	if (o_ptr->to_a > 0) return (FALSE);
	if (o_ptr->to_h > 0) return (FALSE);

	/* Check the other stores */
	for (i = 0; i < MAX_STORES; i++)
	{
		/* Skip home and black market */
		if ((i == STORE_B_MARKET) || (i == STORE_HOME) || (i == STORE_GUILD))
		  continue;

		/* Check every object in the store */
		for (j = 0; j < store[i].stock_num; j++)
		{
			object_type *j_ptr = &store[i].stock[j];

			/* Duplicate object "type", assume crappy */
			if (o_ptr->k_idx == j_ptr->k_idx) return (TRUE);
		}
	}

	/* Assume okay */
	return (FALSE);
}

/*
 * Attempt to delete (some of) a random object from the store
 * Hack -- we attempt to "maintain" piles of items when possible.
 */
static void store_delete(void)
{
	int what, num;

	/* Paranoia */
	if (st_ptr->stock_num <= 0) return;

	/* Pick a random slot */
	what = rand_int(st_ptr->stock_num);

	/* Determine how many objects are in the slot */
	num = st_ptr->stock[what].number;

	/* Hack -- sometimes, only destroy half the objects */
	if (rand_int(100) < 50) num = (num + 1) / 2;

	/* Hack -- sometimes, only destroy a single object */
	if (rand_int(100) < 50) num = 1;

	/* Actually destroy (part of) the object */
	store_item_increase(what, -num);
	store_item_optimize(what);
}

/*
 * Creates a random object and gives it to a store
 * This algorithm needs to be rethought.  A lot.
 * Currently, "normal" stores use a pre-built array.
 *
 * Note -- the "level" given to "obj_get_num()" is a "favored"
 * level, that is, there is a much higher chance of getting
 * items with a level approaching that of the given level...
 *
 * Should we check for "permission" to have the given object?
 */
static void store_create(void)
{
	int k_idx, tries, level, temp;

	object_type *i_ptr;
	object_type object_type_body;

	/* Paranoia -- no room left */
	if (st_ptr->stock_num >= st_ptr->stock_size) return;

	/* Hack -- consider up to four items */
	for (tries = 0; tries < 4; tries++)
	{
		/* Black Market */
		if (store_num == STORE_B_MARKET) 
		{
			/* Pick a level for object/magic */
			if (rand_int(100) < STORE_BM_OLDDEPTH) level = 25 + rand_int(25);
			else level = p_ptr->max_depth + 5 + rand_int(5);
	
			/* Not too deep */
			if (level > 100) level = 100;

			/* Random object kind (usually of given level) */
			k_idx = get_obj_num(level);
		
			/* Handle failure */
			if (!k_idx) continue;
		}

		/* Normal Store */
		else
		{
			/* Hack -- Pick an object kind to sell */
			k_idx = st_ptr->table[rand_int(st_ptr->table_num)];

			/* Hack -- fake level for apply_magic() */
			level = rand_range(1, STORE_OBJ_LEVEL);
		}

		/* Get local object */
		i_ptr = &object_type_body;

		/* Create a new object of the chosen kind */
		object_prep(i_ptr, k_idx);

		/* Store object creation level */
		temp = object_level;
		object_level = level;

		/* Apply some "low-level" magic (no artifacts) */
		apply_magic(i_ptr, level, FALSE, FALSE, FALSE, TRUE);

		/* Hack -- some prefixes don't belong in regular stores */
		if ((store_num != STORE_B_MARKET) && i_ptr->pfx_idx)
		{
			if (weapon_p(i_ptr))
			{
				if (!(wpx_info[i_ptr->pfx_idx].flags & PXF_SHOP)) continue;
			}
			if (i_ptr->tval == TV_BODY_ARMOR)
			{
				if (!(apx_info[i_ptr->pfx_idx].flags & PXF_SHOP)) continue;
			}
		}

		/* Restore object creation level */
		object_level = temp;

		/* Hack -- Charge lite's */
		if (i_ptr->tval == TV_LITE)
		{
			if (i_ptr->sval == SV_TORCH) i_ptr->timeout = FUEL_TORCH;
			if (i_ptr->sval == SV_LANTERN) i_ptr->timeout = FUEL_ENCHANTED;
			if (i_ptr->sval > SV_LANTERN) i_ptr->timeout = FUEL_EGO;
		}

		/* The object is fully "known" */
		object_known(i_ptr);
		
		/* Hack - *ID* ego items */
		if (i_ptr->e_idx) i_ptr->ident |= (IDENT_MENTAL);

		/* Object history */
		i_ptr->origin_nature = ORIGIN_STORE;

		/* Prune the black market */
		if (store_num == STORE_B_MARKET)
		{
			/* Hack -- No "crappy" items */
			if (black_market_crap(i_ptr)) continue;

			/* Hack -- No "cheap" items */
			if (object_value(i_ptr) < 25) continue;

		}

		/* Prune normal stores */
		else
		{
			/* No "worthless" items */
			if (object_value(i_ptr) <= 0) continue;
		}

		/* Mass produce and/or Apply discount */
		mass_produce(i_ptr);

		/* Attempt to carry the (known) object */
		(void)store_carry(i_ptr, FALSE);

		/* Definitely done */
		break;
	}
}

/*
 * Redisplay a single store entry
 */
static void display_entry(int item)
{
	int y;
	object_type *o_ptr;
	s32b x;

	char o_name[80];
	char out_val[160];
	int maxwid;

	/* Must be on current "page" to get displayed */
	if (!((item >= store_top) && (item < store_top + 12))) return;

	/* Get the object */
	o_ptr = &st_ptr->stock[item];

	/* Get the row */
	y = (item % 12) + 6;

	/* Label it, clear the line --(-- */
	sprintf(out_val, "%c) ", store_to_label(item));
	prt(out_val, y, 0);

	/* Describe an object in the home */
	if (store_num == STORE_HOME)
	{
		byte attr;

		maxwid = 75;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 10;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
		o_name[maxwid] = '\0';

		/* Get inventory color */
		if (o_ptr->tval != TV_MAGIC_BOOK) attr = tval_to_attr[o_ptr->tval & 0x7F];
		else
		{
			if (cp_ptr->spell_book[o_ptr->sval]) attr = k_info[o_ptr->k_idx].d_attr;
			else attr = TERM_L_DARK;
		}

		/* Display the object */
		c_put_str(attr, o_name, y, 3);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of a single object */
			int wgt = object_weight(o_ptr);
			sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
			put_str(out_val, y, 68);
		}
	}

	/* Describe an object (fully) in a store */
	else
	{
		byte attr;

		/* Must leave room for the "price" */
		maxwid = 65;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 7;

		/* Describe the object (fully) */
		object_desc_store(o_name, sizeof(o_name), o_ptr, TRUE, 3);
		o_name[maxwid] = '\0';

		/* Get inventory color */
		if (o_ptr->tval != TV_MAGIC_BOOK) attr = tval_to_attr[o_ptr->tval & 0x7F];
		else
		{
			if (cp_ptr->spell_book[o_ptr->sval]) attr = k_info[o_ptr->k_idx].d_attr;
			else attr = TERM_L_DARK;
		}

		/* Display the object */
		c_put_str(attr, o_name, y, 3);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of a single object */
			int wgt = object_weight(o_ptr);
			sprintf(out_val, "%3d.%d", wgt / 10, wgt % 10);
			put_str(out_val, y, 61);
		}

		/* XXX XXX - Mark objects as "seen" (doesn't belong in this function) */
		k_info[o_ptr->k_idx].everseen = TRUE;

		/* Extract the "minimum" price */
		x = price_item(o_ptr, FALSE);

		/* Actually draw the price */
		attr = (((x + 9) / 10) <= p_ptr->au ? TERM_WHITE : TERM_L_DARK);
		if (x > 5) sprintf(out_val, "%9ld  ", (long)(x + 9) / 10);
		else sprintf(out_val, "%3ld for 1", (long)(10 / x));

		c_put_str(attr, out_val, y, 68);
	}
}

/*
 * Display a store's inventory
 *
 * All prices are listed as "per individual object"
 */
static void display_inventory(void)
{
	int i, k;

	/* Display the next 12 items */
	for (k = 0; k < 12; k++)
	{
		/* Stop when we run out of items */
		if (store_top + k >= st_ptr->stock_num) break;

		/* Display that line */
		display_entry(store_top + k);
	}

	/* Erase the extra lines and the "more" prompt */
	for (i = k; i < 13; i++) prt("", i + 6, 0);

	/* Assume "no current page" */
	put_str("        ", 5, 20);

	/* Visual reminder of "more items" */
	if (st_ptr->stock_num > 12)
	{
		/* Show "more" reminder (after the last object ) */
		prt("-more-", k + 6, 3);

		/* Indicate the "current page" */
		put_str(format("(Page %d)", store_top/12 + 1), 5, 20);
	}
}

/*
 * Display players gold
 */
static void store_prt_gold(void)
{
	char out_val[64];

	prt("Gold Remaining: ", 19, 53);

	sprintf(out_val, "%9ld", (long)p_ptr->au);
	prt(out_val, 19, 68);
}

/*
 * Display store (after clearing screen)
 */
static void display_store(void)
{
	char buf[80];

	/* Clear screen */
	Term_clear();

	/* The "Home" is special */
	if (store_num == STORE_HOME)
	{
		/* Put the owner name */
		put_str("Your Home", 3, 30);

		/* Label the object descriptions */
		put_str("Item Description", 5, 3);

		/* If showing weights, show label */
		if (show_weights)
		{
			put_str("Weight", 5, 70);
		}
	}

	/* The "guild is special */
	else if (store_num == STORE_GUILD)
	{
		display_guild();
		return;
	}

	/* Normal stores */
	else
	{
		cptr store_name = (f_name + f_info[FEAT_SHOP_HEAD + store_num].name);
		cptr owner_name = &(b_name[ot_ptr->owner_name]);
		cptr race_name = p_name + p_info[ot_ptr->owner_race].name;

		/* Put the owner name and race */
		strnfmt(buf, sizeof(buf), "%s (%s)", owner_name, race_name);
		put_str(buf, 3, 10);

		/* Show the max price in the store (above prices) */
		strnfmt(buf, sizeof(buf), "%s (%ld)", store_name, (long)(ot_ptr->max_cost));
		prt(buf, 3, 50);

		/* Label the object descriptions */
		put_str("Item Description", 5, 3);

		/* If showing weights, show label */
		if (show_weights)
		{
			put_str("Weight", 5, 60);
		}

		/* Label the asking price (in stores) */
		put_str("Price", 5, 72);
	}

	/* Display the current gold */
	store_prt_gold();

	/* Draw in the inventory */
	display_inventory();
}

/*
 * Get the index of a store object
 *
 * Return TRUE if an object was selected
 */
static bool get_stock(int *com_val, cptr pmt)
{
	int item;

	char which;

	char buf[160];
	char o_name[80];
	char out_val[160];

	object_type *o_ptr;

	/* Get the item index */
	if (repeat_pull(com_val))
	{
		/* Verify the item */
		if ((*com_val >= 0) && (*com_val <= (st_ptr->stock_num - 1)))
		{
			/* Success */
			return (TRUE);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}

	/* Assume failure */
	*com_val = (-1);

	/* Build the prompt */
	strnfmt(buf, sizeof(buf), "(Items %c-%c, ESC to exit) %s",
	        store_to_label(0), store_to_label(st_ptr->stock_num - 1),
	        pmt);

	/* Ask until done */
	while (TRUE)
	{
		bool verify;

		/* Escape */
		if (!get_com(buf, &which)) return (FALSE);

		/* Note verify */
		verify = (isupper((unsigned char)which) ? TRUE : FALSE);

		/* Lowercase */
		which = tolower((unsigned char)which);

		/* Convert response to item */
		item = label_to_store(which);

		/* Oops */
		if (item < 0)
		{
			/* Oops */
			bell("Illegal store object choice!");

			continue;
		}

		/* No verification */
		if (!verify) break;

		/* Object */
		o_ptr = &st_ptr->stock[item];

		/* Home */
		if (store_num == STORE_HOME)
		{
			/* Describe */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
		}

		/* Shop */
		else
		{
			/* Describe */
			object_desc_store(o_name, sizeof(o_name), o_ptr, TRUE, 3);
		}

		/* Prompt */
		strnfmt(out_val, sizeof(out_val), "Try %s? ", o_name);

		/* Query */
		if (!get_check(out_val)) return (FALSE);

		/* Done */
		break;
	}

	/* Save item */
	(*com_val) = item;

	repeat_push(*com_val);


	/* Success */
	return (TRUE);
}

/*
 * Confirm a purchase
 */
static int confirm_trade(void)
{
	char out_val[80];

	/* Paranoia */
	message_flush();
	
	/* Hack - no text */
	strcpy(out_val, "");

	/* Ask the user for a response */
	/* return (get_string("Accept? ", out_val, sizeof(out_val))); */
	return (get_check("Accept? "));
}

/*
 * Purchase an object
 */
static bool do_purchase(const object_type *o_ptr, s32b *price)
{
	s32b value = *price;

	/* Extract the starting offer and final offer */
	value = ((price_item(o_ptr, FALSE) * o_ptr->number) + 9) / 10;

	/* Display Offer */
	put_str(format("Price :  %ld", (long)value), 1, 0);

	*price = value;

	/* Return success */
	return (confirm_trade());
}

/*
 * Sell an object to the store
 */
static bool do_sell(const object_type *o_ptr, s32b *price)
{
	s32b value = *price;

	/* Get the owner's payout limit */
	s32b purse = (s32b)(ot_ptr->max_cost * 10);

	/* Obtain the offer */
	value = price_item(o_ptr, TRUE);	

	if (value >= purse) value = purse;

	/* Sell the whole pile */
	value *= o_ptr->number;

	value /= 10;

	/* Always at least one item */
	if (value < 1) value = 1;

	/* Display Offer */
	put_str(format("Offer :  %ld", (long)value), 1, 0);

	*price = value;

	/* Return success */
	return (confirm_trade());
}

/*
 * Buy an object from a store
 */
static void store_purchase(void)
{
	int n, amt;
	int item, item_new;

	s32b price, price_one;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[80];

	char out_val[160];

	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		if (store_num == STORE_HOME)
		{
			message(MSG_FAIL, 0, "Your home is empty.");
		}
		else
		{
			message(MSG_STORE, 0, "I am currently out of stock.");
		}
		return;
	}

	/* Prompt */
	if (store_num == STORE_HOME)
	{
		sprintf(out_val, "Which item do you want to take? ");
	}
	else
	{
		sprintf(out_val, "Which item are you interested in? ");
	}

	/* Get the object number to be bought */
	if (!get_stock(&item, out_val)) return;

	/* Get the actual object */
	o_ptr = &st_ptr->stock[item];

	/* Prepare price of one item */
	price_one = price_item(o_ptr, FALSE);

	if (store_num == STORE_HOME)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);
	}
	else
	{
		/* Can't afford any */
		if (price_one / 10 > p_ptr->au)
		{
			message(MSG_FAIL, 0, "You do not have enough gold.");
			return;
		}

		/* Offer only what I can afford */
		if (((o_ptr->number * price_one) + 9) / 10 > p_ptr->au)
		{
			amt = get_quantity(NULL, (int)(p_ptr->au * 10 / price_one));
		}
		else
		{
			amt = get_quantity(NULL, o_ptr->number);
		}
	}

	/* Allow user abort */
	if (amt <= 0) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Get desired object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(i_ptr))
	{
		message(MSG_FAIL, 0, "You cannot carry that many items.");
		return;
	}

	/* Attempt to buy it */
	if (store_num != STORE_HOME)
	{
		/* Describe the object (fully) */
		object_desc_store(o_name, sizeof(o_name), i_ptr, TRUE, 3);

		/* Message */
		message_format(MSG_STORE, 0, "Buying %s (%c).",
		           o_name, store_to_label(item));
		message_flush();

		/* Player wants it */
		if (do_purchase(i_ptr, &price))

		{
			/* Player can afford it */
			if (p_ptr->au >= price)
			{
				/* Say "okay" */
				say_comment_1();
	
				/* Spend the money */
				p_ptr->au -= price;

				/* Update the display */
				store_prt_gold();

				/* Buying an object makes you aware of it */
				object_aware(i_ptr);

				/* Combine / Reorder the pack (later) */
				p_ptr->notice |= (PN_COMBINE | PN_REORDER);

				/* Describe the transaction */
				object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

				/* Message */
				message_format(MSG_STORE, 0, "You bought %s (%c) for %ld gold.",
				           o_name, store_to_label(item), (long)price);

				/* Erase the inscription */
				i_ptr->note = 0;

				/* Remove special inscription, if any */
				if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

				/* Give it to the player */
				item_new = inven_carry(i_ptr);

				/* Describe the final result */
				object_desc(o_name, sizeof(o_name), &inventory[item_new], TRUE, 3);

				/* Message */
				message_format(MSG_STORE, 0, "You have %s (%c).",
				           o_name, index_to_label(item_new));

				/* Handle stuff */
				handle_stuff();

				/* Note how many slots the store used to have */
				n = st_ptr->stock_num;

				/* Remove the bought objects from the store */
				store_item_increase(item, -amt);
				store_item_optimize(item);

				/* Store is empty */
				if (st_ptr->stock_num == 0)
				{
					int i;

					/* Shuffle */
					if (rand_int(STORE_SHUFFLE) == 0)
					{
						/* Message */
						message(MSG_STORE, 0, "The shopkeeper retires.");

						/* Shuffle the store */
						store_shuffle(store_num);
					}

					/* Maintain */
					else
					{
						/* Message */
						message(MSG_STORE, 0, "The shopkeeper brings out some new stock.");
					}

					/* New inventory */
					for (i = 0; i < 10; ++i)
					{
						/* Maintain the store */
						store_maint(store_num);
					}

					/* Start over */
					store_top = 0;
				}

				/* The object is gone */
				else if (st_ptr->stock_num != n)
				{
					/* Only one screen left */
					if (st_ptr->stock_num <= 12)
					{
						store_top = 0;
					}
				}

				/* Redraw everything */
				display_inventory();
			}

			/* Player cannot afford it */
			else
			{
				/* Simple message (no insult) */
				message(MSG_FAIL, 0, "You do not have enough gold.");
			}
		}
	}

	/* Home is much easier */
	else
	{

		/* Give it to the player */
		item_new = inven_carry(i_ptr);

		/* Describe just the result */
		object_desc(o_name, sizeof(o_name), &inventory[item_new], TRUE, 3);

		/* Message */
		message_format(MSG_STORE, 0, "You have %s (%c).", o_name, index_to_label(item_new));

		/* Handle stuff */
		handle_stuff();

		/* Take note if we take the last one */
		n = st_ptr->stock_num;

		/* Remove the items from the home */
		store_item_increase(item, -amt);
		store_item_optimize(item);

		/* The object is gone */
		if (st_ptr->stock_num != n)
		{
			/* Only one screen left */
			if (st_ptr->stock_num <= 12)
			{
				store_top = 0;
			}

			/* Redraw everything */
			display_inventory();
		}

		/* The object is still here */
		else
		{
			/* Redraw the object */
			display_entry(item);
		}
	}

	/* Not kicked out */
	return;
}

/*
 * Sell an object to the store (or home)
 */
static void store_sell(void)
{
	int item, item_pos;
	int amt;

	s32b price, value, dummy;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	cptr q, s;

	char o_name[80];

	/* Home */
	q = "Drop which item? ";

	/* Real store */
	if (store_num != STORE_HOME)
	{
		/* New prompt */
		q = "Sell which item? ";

		/* Only allow items the store will buy */
		item_tester_hook = store_will_buy;
	}

	/* Only rogues can sell items */


	if (!(store_num == STORE_HOME) && (!(cp_ptr->flags & CF_APPRAISE)))
	{
		message_format(MSG_STORE, 0, "Only rogues can sell items.");
		return;
	}

	/* Get an item */
	s = "You have nothing that I want.";
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

	/* Hack -- Cannot remove cursed objects */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
		message(MSG_FAIL, 0, "Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Get a copy of the object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Get a full description */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Is there room in the store (or the home?) */
	if (!store_check_num(i_ptr))
	{
		if (store_num == STORE_HOME)
		{
			message(MSG_FAIL, 0, "Your home is full.");
		}
		else
		{
			message(MSG_STORE, 0, "I have not the room in my store to keep it.");
		}
		return;
	}

	/* Real store */
	if (store_num != STORE_HOME)
	{
		/* Describe the transaction */
		message_format(MSG_STORE, 0, "Selling %s (%c).", o_name, index_to_label(item));
		message_flush();

		/* Sold... */
		if (do_sell(i_ptr, &price))

		{
			/* Say "okay" */
			say_comment_1();

			/* Get some money */
			p_ptr->au += price;

			/* Update the display */
			store_prt_gold();

			/* Get the "apparent" value */
			dummy = object_value(i_ptr) * i_ptr->number;

			/* Erase the inscription */
			i_ptr->note = 0;

			/* Remove special inscription, if any */
			if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

			/* Identify original object */
			object_aware(o_ptr);
			object_known(o_ptr);

			/* Test for squelch */
			if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);

			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

			/* Get local object */
			i_ptr = &object_type_body;

			/* Get a copy of the object */
			object_copy(i_ptr, o_ptr);

			/* Modify quantity */
			i_ptr->number = amt;

			/* Distribute charges of wands or rods */
			distribute_charges(o_ptr, i_ptr, amt);

			/* Get the "actual" value */
			value = object_value(i_ptr) * i_ptr->number;

			/* Get the description all over again */
			object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

			/* Describe the result (in message buffer) */
			message_format(MSG_STORE, 0, "You sold %s (%c) for %ld gold.",
			           o_name, index_to_label(item), (long)price);

			/* Analyze the prices (and comment verbally) */
			purchase_analyze(price, value, dummy);

			/* Take the object from the player */
			inven_item_increase(item, -amt);
			inven_item_describe(item);
			inven_item_optimize(item);

			/* Handle stuff */
			handle_stuff();

			/* The store gets that (known) object */
			item_pos = store_carry(i_ptr, TRUE);

			/* Update the display */
			if (item_pos >= 0)
			{
				/* Redisplay wares */
				display_inventory();
			}
		}
	}

	/* Player is at home */
	else
	{
		/* Distribute charges of wands or rods */
		distribute_charges(o_ptr, i_ptr, amt);

		/* Describe */
		message_format(MSG_STORE, 0, "You drop %s (%c).", o_name, index_to_label(item));

		/* Take it from the players inventory */
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);

		/* Handle stuff */
		handle_stuff();

		/* Let the home carry it */
		item_pos = home_carry(i_ptr);

		/* Update store display */
		if (item_pos >= 0)
		{
			/* Redisplay wares */
			display_inventory();
		}
	}
}

/*
 * Examine an item in a store
 */
static void store_examine(void)
{
	int item;
	object_type *o_ptr;
	char o_name[80];
	char out_val[160];

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		if (store_num == STORE_HOME) message(MSG_FAIL, 0, "Your home is empty.");
		else message(MSG_STORE, 0, "I am currently out of stock.");

		return;
	}

	/* Prompt */
	if (rogue_like_commands) sprintf(out_val, "Which item do you want to examine? ");
	else sprintf(out_val, "Which item do you want to look at? ");

	/* Get the item number to be examined */
	if (!get_stock(&item, out_val)) return;

	/* Get the actual object */
	o_ptr = &st_ptr->stock[item];

	/* save screen */
	screen_save();

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Actually display the item */
	list_object(o_ptr, OBJECT_INFO_KNOWN);

	/* Analyze the weapon */
	if (weapon_p(o_ptr) || (o_ptr->tval == TV_DIGGING)) analyze_weapon(o_ptr);
	if (o_ptr->tval == TV_ARROW) analyze_ammo(o_ptr);
	if ((o_ptr->tval == TV_BODY_ARMOR) || (o_ptr->tval == TV_DRAG_ARMOR)) analyze_armor(o_ptr);

	/* History and description */
	if (store_num == STORE_HOME)
	{
		display_object_history(o_ptr);
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
	}
	else
	{
		/* Don't display history for store-generated items */
		if (o_ptr->origin_nature != ORIGIN_STORE) display_object_history(o_ptr);
		object_desc_store(o_name, sizeof(o_name), o_ptr, TRUE, 3);
	}

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* Dump the name */
	Term_addstr(-1, TERM_L_BLUE, o_name);

	(void) inkey();
	
	/* Load screen */
	screen_load();

	return;
}

/*
 * Hack -- set this to leave the store
 */
static bool leave_store = FALSE;

/*
 * Process a command in a store
 *
 * Note that we must allow the use of a few "special" commands
 * in the stores which are not allowed in the dungeon, and we
 * must disable some commands which are allowed in the dungeon
 * but not in the stores, to prevent chaos.
 *
 * Hack -- note the bizarre code to handle the "=" command,
 * which is needed to prevent the "redraw" from affecting
 * the display of the store.  XXX XXX XXX
 *
 * Hack -- also note that the commands change for the guild.
 * If more "special" buildings are added, a store_num check might be 
 * better.
 */
static void store_process_command(bool guild_cmd)
{
	bool legal;

	/* Handle repeating the last command */
	repeat_check();

	legal = TRUE;

	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
		/* Leave */
		case ESCAPE:
		{
			leave_store = TRUE;
			break;
		}

		/* Browse */
		case ' ':
		{
			if (guild_cmd)
			{
				legal = FALSE;
				break;
			}

			if (st_ptr->stock_num <= 12)
			{
				/* Nothing to see */
				message(MSG_DESCRIBE, 0, "Entire inventory is shown.");
			}

			else if (store_top == 0)
			{
				/* Page 2 */
				store_top = 12;

				/* Redisplay wares */
				display_inventory();
			}

			else
			{
				/* Page 1 */
				store_top = 0;

				/* Redisplay wares */
				display_inventory();
			}

			break;
		}

		/* Ignore */
		case '\n':
		case '\r':
		{
			break;
		}

		/*** Store Commands ***/

		/* Get (purchase) */
		case 'g':
		{
			if (!guild_cmd) store_purchase();
			else guild_purchase();
			break;
		}

		/* Drop (Sell) */
		case 'd':
		{
			if (!guild_cmd) store_sell();
			else legal = FALSE;
			break;
		}

		/* Examine */
		case 'l':
		{
			if (!guild_cmd) store_examine();
			else legal = FALSE;
			break;
		}

		/*** Inventory Commands ***/

		case 'w': do_cmd_wield(); break;				/* Wear/wield equipment */
		case 't': do_cmd_takeoff();	break;				/* Take off equipment */
		case 'k': do_cmd_destroy();	break;				/* Destroy an item */
		case 'e': do_cmd_equip(); break;				/* Equipment list */
		case 'i': do_cmd_inven(); break;				/* Inventory list */
		case KTRL('E'):	toggle_inven_equip(); break;	/* Hack -- toggle windows */
		case 'I': do_cmd_observe(); break;				/* Identify an object */		
		case '{': do_cmd_inscribe(); break;				/* Inscribe an object */
		case '}': do_cmd_uninscribe(); break;			/* Uninscribe an object */

		/*** Help and Such ***/

		case '?': do_cmd_help(); break;					/* Help */
		case '/': do_cmd_query_symbol(); break;			/* Identify symbol */
		case 'C': do_cmd_display_character(); break;	/* Character description */

		/*** System Commands ***/

		case '!': (void)Term_user(0); break;			/* Hack -- User interface */
		case '"': do_cmd_pref(); break;					/* Single line from a pref file */
		case '@': do_cmd_macros(); break;				/* Interact with macros */
		case '%': do_cmd_visuals();	break;				/* Interact with visuals */
		case '&': do_cmd_colors(); break;				/* Interact with colors */
		/* Interact with options */
		case '=':
		{
			do_cmd_options();
			display_store();
			break;
		}

		/*** Misc Commands ***/

		case ':': do_cmd_note(); break;					/* Take notes */
		case 'V': do_cmd_version();	break;				/* Version info */		
		case KTRL('O'):	do_cmd_message_one(); break;	/* Show previous message */
		case KTRL('P'):	do_cmd_messages(); break;		/* Show previous messages */
		case '~': case '|':	do_cmd_knowledge();	break; 	/* Check knowledge */
		case '(': do_cmd_save_screen_text(); break; 	/* Text "screen dump" */
		case ')': do_cmd_save_screen_html(); break;		/* HTML "screen dump" */
		/* Redraw */
		case KTRL('R'):
		{
			do_cmd_redraw();
			if (!guild_cmd) display_store();
			else display_guild();
			break;
		}

		/*** Hack -- Unknown command ***/

		default:
		{
			legal = FALSE;
			break;
		}
	}

	if (!legal)
	{
		if (!guild_cmd) message(MSG_FAIL, 0, "That command does not work in stores.");
		else message(MSG_FAIL, 0, "That command does not work in the adventurer's guild.");
	}
}

/*
 * Enter a store, and interact with it.
 *
 * Note that we use the standard "request_command()" function
 * to get a command, allowing us to use "p_ptr->command_arg" and all

 * command macros and other nifty stuff, but we use the special
 * "shopping" argument, to force certain commands to be converted
 * into other commands, normally, we convert "p" (pray) and "m"
 * (cast magic) into "g" (get), and "s" (search) into "d" (drop).
 */
void do_cmd_store(void)
{
	int which;

	byte tmp_chr;

	/* Verify a store */
	if (!((cave_feat[p_ptr->py][p_ptr->px] >= FEAT_SHOP_HEAD) &&
	      (cave_feat[p_ptr->py][p_ptr->px] <= FEAT_SHOP_TAIL)))
	{
		message(MSG_FAIL, 0, "You see no store here.");
		return;
	}

	/* Hack -- Extract the store code */
	which = (cave_feat[p_ptr->py][p_ptr->px] - FEAT_SHOP_HEAD);

	/* Hack -- Check the "locked doors" */
	if (adult_no_stores)
	{
		message(MSG_FAIL, 0, "The doors are locked.");
		return;
	}

	/* Forget the view */
	forget_view();

	/* Hack -- Increase "icky" depth */
	character_icky++;

	/* No command argument */
	p_ptr->command_arg = 0;

	/* No repeated command */
	p_ptr->command_rep = 0;

	/* No automatic command */
	p_ptr->command_new = 0;

	/* Save the store number */
	store_num = which;

	/* Save the store and owner pointers */
	st_ptr = &store[store_num];
	ot_ptr = &b_info[(store_num * z_info->b_max) + st_ptr->owner];

	/* Start at the beginning */
	store_top = 0;

	/* Display the store */
	display_store();

	/* Do not leave */
	leave_store = FALSE;

	/* Interact with player */
	while (!leave_store)
	{
		/* Hack -- Clear line 1 */
		prt("", 1, 0);

		/* Hack -- Check the charisma */
		tmp_chr = p_ptr->stat_use[A_CHR];

		/* Clear */
		clear_from(21);

		/* Basic commands */
		prt(" ESC) Exit from Building.", 22, 0);

		/* Browse if necessary */
		if (st_ptr->stock_num > 12)
		{
			prt(" SPACE) Next page of stock.", 23, 0);
		}

		/* Commands */
		if (store_num != STORE_GUILD)
		{	
			prt(" g) Get/Purchase an item.", 22, 29);
			prt(" d) Drop/Sell an item.", 23, 29);

			/* Add in the eXamine option */
			if (rogue_like_commands)
				prt(" x) eXamine an item.", 22, 56);
			else
				prt(" l) Look at an item.", 22, 56);
		}
		else
		{
			prt(" g) Get a quest.", 22, 31);
		}

		/* Prompt */
		prt("You may: ", 21, 0);

		/* Get a command */
		request_command(TRUE);

		/* Process the command */
		if (store_num != STORE_GUILD) store_process_command(FALSE);
		else store_process_command(TRUE);

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Pack Overflow XXX XXX XXX */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK;

			object_type *o_ptr = &inventory[item];

			/* Hack -- Flee from the store */
			if (store_num != STORE_HOME)
			{
				/* Message */
				message(MSG_GENERIC, 0, "Your pack is so full that you flee the store...");

				/* Leave */
				leave_store = TRUE;
			}

			/* Hack -- Flee from the home */
			else if (!store_check_num(o_ptr))
			{
				/* Message */
				message(MSG_GENERIC, 0, "Your pack is so full that you flee your home...");

				/* Leave */
				leave_store = TRUE;
			}

			/* Hack -- Drop items into the home */
			else
			{
				int item_pos;

				object_type *i_ptr;
				object_type object_type_body;

				char o_name[80];

				/* Give a message */
				message(MSG_GENERIC, 0, "Your pack overflows!");

				/* Get local object */
				i_ptr = &object_type_body;

				/* Grab a copy of the object */
				object_copy(i_ptr, o_ptr);

				/* Describe it */
				object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

				/* Message */
				message_format(MSG_DROP, 0, "You drop %s (%c).", o_name, index_to_label(item));

				/* Remove it from the players inventory */
				inven_item_increase(item, -255);
				inven_item_describe(item);
				inven_item_optimize(item);

				/* Handle stuff */
				handle_stuff();

				/* Let the home carry it */
				item_pos = home_carry(i_ptr);

				/* Redraw the home */
				if (item_pos >= 0)
				{
					/* Redisplay wares */
					display_inventory();
				}
			}
		}

		/* Hack -- Handle charisma changes */
		if (tmp_chr != p_ptr->stat_use[A_CHR])
		{
			/* Redisplay wares */
			display_inventory();
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Hack -- Cancel automatic command */
	p_ptr->command_new = 0;

	/* Flush messages XXX XXX XXX */
	message_flush();

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Clear the screen */
	Term_clear();

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}

/*
 * Shuffle one of the stores.
 */
void store_shuffle(int which)
{
	int i, j;

	/* Ignore home */
	if ((which == STORE_HOME) || (which == STORE_GUILD)) return;

	/* Save the store index */
	store_num = which;

	/* Activate that store */
	st_ptr = &store[store_num];

	/* Pick a new owner */
	for (j = st_ptr->owner; j == st_ptr->owner; )
	{
		st_ptr->owner = (byte)rand_int(z_info->b_max);
	}

	/* Activate the new owner */
	ot_ptr = &b_info[(store_num * z_info->b_max) + st_ptr->owner];

	/* Discount all the items */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &st_ptr->stock[i];

		/* Discount non-discounted items by 40 percent */
		if (o_ptr->discount == 0) o_ptr->discount = 40;
	}
}

/*
 * Maintain the inventory at the stores.
 */
void store_maint(int which)
{
	int j;

	int old_rating = rating;

	/* Ignore home and guild*/
	if ((which == STORE_HOME) || (which == STORE_GUILD)) return;

	/* Save the store index */
	store_num = which;

	/* Activate that store */
	st_ptr = &store[store_num];

	/* Activate the owner */
	ot_ptr = &b_info[(store_num * z_info->b_max) + st_ptr->owner];

	/* Mega-Hack -- prune the black market */
	if (store_num == STORE_B_MARKET)
	{
		/* Destroy crappy black market items */
		for (j = st_ptr->stock_num - 1; j >= 0; j--)
		{
			object_type *o_ptr = &st_ptr->stock[j];

			/* Destroy crappy items */
			if (black_market_crap(o_ptr))
			{
				/* Destroy the object */
				store_item_increase(j, 0 - o_ptr->number);
				store_item_optimize(j);
			}
		}
	}

	/* Choose the number of slots to keep */
	j = st_ptr->stock_num;

	/* Sell a few items */
	j = j - randint(STORE_TURNOVER);

	/* Never keep more than "STORE_MAX_KEEP" slots */
	if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;

	/* Always "keep" at least "STORE_MIN_KEEP" items */
	if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;

	/* Hack -- prevent "underflow" */
	if (j < 0) j = 0;

	/* Destroy objects until only "j" slots are left */
	while (st_ptr->stock_num > j) store_delete();

	/* Choose the number of slots to fill */
	j = st_ptr->stock_num;

	/* Buy some more items */
	j = j + randint(STORE_TURNOVER);

	/* Never keep more than "STORE_MAX_KEEP" slots */
	if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;

	/* Always "keep" at least "STORE_MIN_KEEP" items */
	if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;

	/* Hack -- prevent "overflow" */
	if (j >= st_ptr->stock_size) j = st_ptr->stock_size - 1;

	/* Create some new items */
	while (st_ptr->stock_num < j) store_create();

	/* Hack -- Restore the rating */
	rating = old_rating;
}

/*
 * Initialize the stores
 */
void store_init(int which)
{
	int k;

	/* Save the store index */
	store_num = which;

	/* Activate that store */
	st_ptr = &store[store_num];

	/* Pick an owner */
	st_ptr->owner = (byte)rand_int(z_info->b_max);

	/* Activate the new owner */
	ot_ptr = &b_info[(store_num * z_info->b_max) + st_ptr->owner];

	/* Nothing in stock */
	st_ptr->stock_num = 0;

	/* Clear any old items */
	for (k = 0; k < st_ptr->stock_size; k++)
	{
		object_wipe(&st_ptr->stock[k]);
	}
}
