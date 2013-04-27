/* File: store.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cmds.h"
#include "option.h"
#include "store.h"
#include "tvalsval.h"
#include "x-spell.h"

/*
 * Store constants.
 */
#define STORE_OBJ_LEVEL	5		/**< Magic Level for normal stores */
#define STORE_TURNOVER	9		/**< Normal shop turnover, per day */
#define STORE_MIN_KEEP	6		/**< Min slots to "always" keep full */
#define STORE_MAX_KEEP	18		/**< Max slots to "always" keep full */

/*
 * Store reference tables.
 */
static const char* const comment_accept[] =
{
	"Okay.",
	"Fine.",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!"
};

static const char* const comment_insult[] =
{
	"Try again.",
	"Ridiculous!",
	"You will have to do better than that!",
	"Do you wish to do business or not?",
	"You've got to be kidding!",
	"You'd better be kidding!",
	"You try my patience.",
	"Hmmm, nice weather we're having."
};


static const char* const comment_nonsense[] =
{
	"I must have heard you wrong.",
	"I'm sorry, I missed that.",
	"I'm sorry, what was that?",
	"Sorry, what was that again?"
};



/*
 * Successful haggle.
 */
static void say_comment_accept(void)
{
	message(MSG_STORE5, 0, comment_accept[rand_int(N_ELEMENTS(comment_accept))]);
}


/*
 * You are insulting me
 */
static void say_comment_insult(void)
{
	msg_print(comment_insult[rand_int(N_ELEMENTS(comment_insult))]);
}


/*
 * You are making no sense.
 */
static void say_comment_nonsense(void)
{
	msg_print(comment_nonsense[rand_int(N_ELEMENTS(comment_nonsense))]);
}



/*
 * Messages for reacting to purchase prices.
 */

static const char* const comment_worthless[] =
{
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!"
};

static const char* const comment_bad[] =
{
	"Damn!",
	"You fiend!",
	"The shopkeeper curses at you.",
	"The shopkeeper glares at you."
};

static const char* const comment_good[] =
{
	"Cool!",
	"You've made my day!",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly."
};

static const char* const comment_great[] =
{
	"Yipee!",
	"I think I'll retire!",
	"The shopkeeper jumps for joy.",
	"The shopkeeper smiles gleefully."
};

static int get_store_choice(int store_num)
{
	return store[store_num].table[rand_int(store[store_num].table_num)];
}

/*
 * Determine if the current store will purchase the given object
 *
 * Note that a shop-keeper must refuse to buy "worthless" objects
 */
static bool store_will_buy(int store_num, const object_type *o_ptr)
{
	/* Hack -- The Home is simple */
	if (store_num == STORE_HOME) return TRUE;

	/* Ignore "worthless" items XXX XXX XXX */
	if (object_value(o_ptr) <= 0) return FALSE;

	/* Switch on the store */
	switch (store_num)
	{
		/* General Store */
		case STORE_GENERAL:
		{
			/* Analyze the type */
			switch (o_ptr->obj_id.tval)
			{
				case TV_FOOD:
				case TV_LITE:
				case TV_FLASK:
				case TV_SPIKE:
				case TV_SHOT:
				case TV_ARROW:
				case TV_BOLT:
				case TV_DIGGING:
				case TV_CLOAK:		return TRUE;
				default:			return FALSE;
			}
			break;
		}

		/* Armoury */
		case STORE_ARMOR:
		{
			/* Analyze the type */
			switch (o_ptr->obj_id.tval)
			{
				case TV_BOOTS:
				case TV_GLOVES:
				case TV_CROWN:
				case TV_HELM:
				case TV_SHIELD:
				case TV_CLOAK:
				case TV_SOFT_ARMOR:
				case TV_HARD_ARMOR:
				case TV_DRAG_ARMOR:	return TRUE;
				default:			return FALSE;
			}
			break;
		}

		/* Weapon Shop */
		case STORE_WEAPON:
		{
			/* Analyze the type */
			switch (o_ptr->obj_id.tval)
			{
				case TV_SHOT:
				case TV_BOLT:
				case TV_ARROW:
				case TV_BOW:
				case TV_DIGGING:
				case TV_HAFTED:
				case TV_POLEARM:
				case TV_SWORD:		return TRUE;
				default:			return FALSE;
			}
			break;
		}

		/* Temple */
		case STORE_TEMPLE:
		{
			/* Analyze the type */
			switch (o_ptr->obj_id.tval)
			{
				case TV_PRAYER_BOOK:
				case TV_SCROLL:
				case TV_POTION:
				case TV_HAFTED:		return TRUE;
				case TV_POLEARM:
				case TV_SWORD:		/* Known blessed blades are accepted too */
									if (is_blessed(o_ptr) && o_ptr->known()) return TRUE;
				default:			return FALSE;
			}
			break;
		}

		/* Alchemist */
		case STORE_ALCHEMY:
		{
			/* Analyze the type */
			switch (o_ptr->obj_id.tval)
			{
				case TV_SCROLL:
				case TV_POTION:		return TRUE;
				default:			return FALSE;
			}
			break;
		}

		/* Magic Shop */
		case STORE_MAGIC:
		{
			/* Analyze the type */
			switch (o_ptr->obj_id.tval)
			{
				case TV_MAGIC_BOOK:
				case TV_AMULET:
				case TV_RING:
				case TV_STAFF:
				case TV_WAND:
				case TV_ROD:
				case TV_SCROLL:
				case TV_POTION:		return TRUE;
				default:			return FALSE;
			}
			break;
		}
	}

	return TRUE;	/* Assume okay */
}


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
		message(MSG_STORE1, 0, comment_worthless[rand_int(N_ELEMENTS(comment_worthless))]);
	}

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
	{
		/* Comment */
		message(MSG_STORE2, 0, comment_bad[rand_int(N_ELEMENTS(comment_bad))]);
	}

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (value < (4 * guess)) && (price < value))
	{
		/* Comment */
		message(MSG_STORE3, 0, comment_good[rand_int(N_ELEMENTS(comment_good))]);
	}

	/* Item was a great bargain, and we got away with it */
	else if ((value > guess) && (price < value))
	{
		/* Comment */
		message(MSG_STORE4, 0, comment_great[rand_int(N_ELEMENTS(comment_great))]);
	}
}





/*
 * We store the current "store number" here so everyone can access it
 */
static store_indexes store_num = STORE_HOME;

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
 * We store the player net worth here so everyone can access it
 */
static s32b pc_net_worth = 0;

/**
 *  Determine player net worth
 */
static s32b player_net_worth(void)
{
	object_type o;
	s32b net_worth = p_ptr->au;	/* start with actual gold */
	int i;

	/* inventory */
	for (i=0; i < INVEN_TOTAL; ++i)
	{
		s32b item_worth;
		if (!p_ptr->inventory[i].k_idx) continue;
		o = p_ptr->inventory[i];
		object_known(&o);
		item_worth = object_value(&o);
		if (0 >= item_worth) continue;
		if (MAX_S32B-item_worth <= net_worth) return MAX_S32B;
		net_worth += item_worth;
	}

	/* home */
	for (i = 0; i < store[STORE_HOME].stock_num; ++i)
	{
		s32b item_worth;
		if (!store[STORE_HOME].stock[i].k_idx) continue;
		o = store[STORE_HOME].stock[i];
		object_known(&o);
		item_worth = object_value(&o);
		if (0 >= item_worth) continue;
		if (MAX_S32B-item_worth <= net_worth) return MAX_S32B;
		net_worth += item_worth;
	}

	return net_worth;
}

static void activate_store(store_indexes which)
{
	store_num = which;	/* Save the store index */
	st_ptr = &store[store_num];	/* Activate that store */

	/* Activate the owner */
	ot_ptr = &b_info[(store_num * z_info->b_max) + st_ptr->owner];
	pc_net_worth = player_net_worth();
}



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
 * Hack -- the black market always charges twice as much as it should.
 *
 * Charisma adjustment runs from 80 to 130
 * Racial adjustment runs from 95 to 130
 *
 * Since greed/charisma/racial adjustments are centered at 100, we need
 * to adjust (by 200) to extract a usable multiplier.  Note that the
 * "greed" value is always something (?).
 */
static s32b price_item(const object_type *o_ptr, int greed, bool store_buy)
{
	int factor;
	int adjust;

	/* Get the value of one of the items */
	s32b price = object_value(o_ptr);

	/* Worthless items */
	if (price <= 0) return 0L;


	/* Compute the racial factor */
	factor = g_info[(ot_ptr->owner_race * z_info->p_max) + p_ptr->prace];

	/* Add in the charisma factor */
	factor += adj_chr_gold[p_ptr->stat_ind[A_CHR]];


	/* Shop is buying */
	if (store_buy)
	{
		/* Adjust for greed */
		adjust = 100 + (300 - (greed + factor));

		/* Never get "silly" */
		if (adjust > 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (store_num == STORE_B_MARKET) price /= 2;
	}

	/* Shop is selling */
	else
	{
		/* Adjust for greed */
		adjust = 100 + ((greed + factor) - 300);

		/* Never get "silly" */
		if (adjust < 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (store_num == STORE_B_MARKET) price *= 2;
	}

	/* Compute the final price (with rounding) */
	price = (price * adjust + 50L) / 100L;

	/* Note -- Never become "free" */
	if (price <= 0L) return 1L;

	/* Return the price */
	return price;
}


/*
 * Certain "cheap" objects should be created in "piles".
 */
static void mass_produce(object_type *o_ptr)
{
	int size = 1;
	s32b cost = object_value(o_ptr);

	/* Analyze the type */
	switch (o_ptr->obj_id.tval)
	{
		/* Food, Flasks, and Lites */
		case TV_FOOD:
		case TV_FLASK:
		case TV_LITE:
		{
			const dice_sides aux = {3, 5};

			if (cost <= 5L) size += aux.damroll();
			if (cost <= 20L) size += aux.damroll();
			break;
		}

		case TV_POTION:
		case TV_SCROLL:
		{
			const dice_sides aux = {3, 5};
			if (cost <= 60L) size += aux.damroll();
			if (cost <= 240L) size += rand_int(5);
			break;
		}

		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		{
			const dice_sides aux = {2, 3};
			if (cost <= 50L) size += aux.damroll();
			if (cost <= 500L) size += rand_int(3);
			break;
		}

		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SHIELD:
		case TV_GLOVES:
		case TV_BOOTS:
		case TV_CLOAK:
		case TV_HELM:
		case TV_CROWN:
		case TV_SWORD:
		case TV_POLEARM:
		case TV_HAFTED:
		case TV_DIGGING:
		case TV_BOW:
		{
			const dice_sides aux = {3, 5};
			if (o_ptr->name2) break;
			if (cost <= 10L) size += aux.damroll();
			if (cost <= 100L) size += aux.damroll();
			break;
		}

		case TV_SPIKE:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			const dice_sides aux = {5, 5};
			if (cost <= 5L) size += aux.damroll();
			if (cost <= 50L) size += aux.damroll();
			if (cost <= 500L) size += aux.damroll();
			break;
		}
	}

	o_ptr->pseudo = 0;	/* wipe special inscription */
	o_ptr->number = size;	/* Save the total pile size */

	/* Hack -- rods need to increase PVAL if stacked */
	if (o_ptr->obj_id.tval == TV_ROD)
	{
		o_ptr->pval = o_ptr->number * object_type::k_info[o_ptr->k_idx].pval;
	}
}

/*
 * Convert a store index from visual to internal
 */
static s16b visual_to_internal_index(int i)
{
	int j;
	if ((0 > i) || (i >= st_ptr->stock_num)) return -1;

	for(j = 0; j <= i; ++j)
		if (st_ptr->stock[j].ident & IDENT_HIDE)
			if (++i >= st_ptr->stock_num) return -1;
	return i;
}

static s16b internal_to_visual_index(int i)
{
	int j;
	if ((0 > i) || (i >= st_ptr->stock_num)) return -1;

	for(j = i; 0 <= j; --j)
		if (st_ptr->stock[j].ident & IDENT_HIDE)
			if (0 > --i) return -1;
	return i;
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
	/* Convert */
	int i = (islower((unsigned char)c) ? A2I(c) : -1);

	/* Verify the index */
	if ((0 > i) || (i >= st_ptr->stock_num)) return (-1);

	/* Return the index */
	return visual_to_internal_index(i);
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
	if (o_ptr == j_ptr) return (0);

	/* Different objects cannot be stacked */
	if (o_ptr->k_idx != j_ptr->k_idx) return (0);

	/* Different pvals cannot be stacked, except for wands, staves, or rods */
	if ((o_ptr->pval != j_ptr->pval) &&
	    (o_ptr->obj_id.tval != TV_WAND) &&
	    (o_ptr->obj_id.tval != TV_STAFF) &&
	    (o_ptr->obj_id.tval != TV_ROD)) return (0);

	/* Require many identical values */
	if (o_ptr->to_h != j_ptr->to_h) return (0);
	if (o_ptr->to_d != j_ptr->to_d) return (0);
	if (o_ptr->to_a != j_ptr->to_a) return (0);

	/* Require identical "artifact" names */
	if (o_ptr->name1 != j_ptr->name1) return (0);

	/* Require identical "ego-item" names */
	if (o_ptr->name2 != j_ptr->name2) return (0);

	/* Hack -- Never stack "powerful" items */
	if (o_ptr->xtra1 || j_ptr->xtra1) return (0);

	/* Hack -- Never stack recharging items */
	if (o_ptr->timeout || j_ptr->timeout) return (0);

	/* Require many identical values */
	if (o_ptr->ac != j_ptr->ac) return (0);
	if (o_ptr->d != j_ptr->d) return (0);

	/* Hack -- Never stack chests */
	if (o_ptr->obj_id.tval == TV_CHEST) return (0);

	/* Require matching pseudo fields */
	if (o_ptr->pseudo != j_ptr->pseudo) return (0);

	/* They match, so they must be similar */
	return (TRUE);
}


/*
 * Allow a store object to absorb another object
 */
static void store_object_absorb(object_type *o_ptr, object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Combine quantity, lose excess items */
	o_ptr->number = (total > 99) ? 99 : total;

	/*
	 * Hack -- if rods are stacking, add the pvals (maximum timeouts)
	 * and any charging timeouts together
	 */
	if (o_ptr->obj_id.tval == TV_ROD)
	{
		o_ptr->pval += j_ptr->pval;
		o_ptr->timeout += j_ptr->timeout;
	}

	/* Hack -- if wands/staves are stacking, combine the charges */
	if ((o_ptr->obj_id.tval == TV_WAND) || (o_ptr->obj_id.tval == TV_STAFF))
	{
		o_ptr->pval += j_ptr->pval;
	}
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

	/* Free space is always usable */
	if (st_ptr->stock_num < st_ptr->stock_size) return TRUE;

	/* The "home" acts like the player */
	if (store_num == STORE_HOME)
	{
		/* Check all the objects */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Can the new object be combined with the old one? */
			if (object_similar(st_ptr->stock+i, o_ptr)) return TRUE;
		}
	}

	/* Normal stores do special stuff */
	else
	{
		/* Check all the objects */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Can the new object be combined with the old one? */
			if (store_object_similar(st_ptr->stock+i, o_ptr)) return TRUE;
		}
	}

	/* But there was no room at the inn... */
	return FALSE;
}


/*
 * Determine if the current store will purchase the given object
 */
static bool store_will_buy_tester(const object_type *o_ptr)
{
	return store_will_buy(store_num, o_ptr);
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
static int home_carry(object_type *o_ptr)
{
	int slot;
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

		/* Hack -- readable books always come first */
		if (o_ptr->obj_id.tval == p_ptr->spell_book())
			{
			if (j_ptr->obj_id.tval != p_ptr->spell_book()) break;
			}
		else{
			if (j_ptr->obj_id.tval == p_ptr->spell_book()) continue;
			}

		/* Objects sort by decreasing type */
		if (o_ptr->obj_id.tval > j_ptr->obj_id.tval) break;
		if (o_ptr->obj_id.tval < j_ptr->obj_id.tval) continue;

		/* Can happen in the home */
		if (!o_ptr->aware()) continue;
		if (!j_ptr->aware()) break;

		/* Objects sort by increasing sval */
		if (o_ptr->obj_id.sval < j_ptr->obj_id.sval) break;
		if (o_ptr->obj_id.sval > j_ptr->obj_id.sval) continue;

		/* Objects in the home can be unknown */
		if (!o_ptr->known()) continue;
		if (!j_ptr->known()) break;

		/* Objects sort by decreasing value */
		j_value = object_value(j_ptr);
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	if (st_ptr->stock_num > slot)
		C_COPY(st_ptr->stock+slot+1, st_ptr->stock+slot, st_ptr->stock_num-slot);

	st_ptr->stock_num++;			/* More stuff now */
	st_ptr->stock[slot] = *o_ptr;	/* Hack -- Insert the new object */
	return (slot);					/* Return the location */
}


/*
 * Add an object to a real stores inventory.
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
static int store_carry(object_type *o_ptr)
{
	int slot;
	s32b j_value;
	object_type *j_ptr;
	s32b value = object_value(o_ptr);	/* Evaluate the object */

	/* Cursed/Worthless items "disappear" when sold */
	if (value <= 0) return (-1);

	o_ptr->note = 0;	/* Erase the inscription */
	o_ptr->pseudo = 0;	/* Remove special inscription, if any */

	/* Check each existing object (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing object */
		j_ptr = &st_ptr->stock[slot];

		/* Can the existing items be incremented? */
		if (store_object_similar(j_ptr, o_ptr))
		{
			/* Absorb (some of) the object */
			store_object_absorb(j_ptr, o_ptr);

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
		if (o_ptr->obj_id.tval > j_ptr->obj_id.tval) break;
		if (o_ptr->obj_id.tval < j_ptr->obj_id.tval) continue;

		/* Objects sort by increasing sval */
		if (o_ptr->obj_id.sval < j_ptr->obj_id.sval) break;
		if (o_ptr->obj_id.sval > j_ptr->obj_id.sval) continue;

		/* Evaluate that slot */
		j_value = object_value(j_ptr);

		/* Objects sort by decreasing value */
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	if (st_ptr->stock_num > slot)
		C_COPY(st_ptr->stock+slot+1, st_ptr->stock+slot, st_ptr->stock_num-slot);

	st_ptr->stock_num++;			/* More stuff now */
	st_ptr->stock[slot] = *o_ptr;	/* Hack -- Insert the new object */
	return (slot);					/* Return the location */
}


/*
 * Increase, by a given amount, the number of a certain item
 * in a certain store.  This can result in zero items.
 */
static void store_item_increase(int item, int num)
{
	object_type *o_ptr = &st_ptr->stock[item];	/* Get the object */
	int cnt = o_ptr->number + num;

	/* Verify the number */
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
	object_type *o_ptr = &st_ptr->stock[item];	/* Get the object */

	if (!o_ptr->k_idx) return;	/* Must exist */
	if (o_ptr->number) return;	/* Must have no items */
	st_ptr->stock_num--;		/* One less object */

	/* Slide everyone */
	if (item < st_ptr->stock_num)
		C_COPY(st_ptr->stock + item, st_ptr->stock + item + 1, st_ptr->stock_num - item);

	/* Nuke the final slot */
	WIPE(st_ptr->stock + st_ptr->stock_num);
}

static bool store_has_item_type(s16b k_idx, int i)
{
	int j;

	/* Check every object in the store */
	for (j = 0; j < store[i].stock_num; j++)
	{
		/* Duplicate object "type", assume boring */
		if (k_idx == store[i].stock[j].k_idx) return TRUE;
	}
	return FALSE;
}

static bool some_legal_store_has_item_type(s16b k_idx)
{
	int i;

	/* Check the stores */
	for (i = 0; i < MAX_STORES; i++)
	{
		/* Skip home and black market */
		if (i == STORE_B_MARKET || i == STORE_HOME)
		  continue;

		if (store_has_item_type(k_idx,i)) return TRUE;
	}
	return FALSE;
}

static bool player_owns_item_type(s16b k_idx)
{
	int i;

	/* Check every object in the home */
	for (i = 0; i < store[STORE_HOME].stock_num; ++i)
	{
		/* Duplicate object "type", assume boring */
		if (k_idx == store[STORE_HOME].stock[i].k_idx) return TRUE;
	}

	/* Check every object equipped or in inventory */
	for (i = 0; i < INVEN_TOTAL; ++i)
	{
		/* Duplicate object "type", assume boring */
		if (k_idx == p_ptr->inventory[i].k_idx) return TRUE;
	}

	return FALSE;
}

static bool object_is_artifact_with_activation(const object_type* o_ptr,int activation)
{
	if (!o_ptr->k_idx) return FALSE;
	if (!o_ptr->is_artifact()) return FALSE;
	return activation==object_type::a_info[o_ptr->name1].activation;	
}

static bool player_owns_artifact_with_activation(int activation)
{
	int i;

	/* Check every object in the home */
	for (i = 0; i < store[STORE_HOME].stock_num; ++i)
		if (object_is_artifact_with_activation(store[STORE_HOME].stock+i,activation)) return TRUE;

	/* Check every object equipped or in inventory */
	for (i = 0; i < INVEN_TOTAL; ++i)
		if (object_is_artifact_with_activation(p_ptr->inventory+i,activation)) return TRUE;

	return FALSE;
}

static bool black_market_has_item_type(s16b k_idx)
{
	int i;

	/* Check every object in the home */
	for (i = 0; i < store[STORE_B_MARKET].stock_num; ++i)
	{
		/* Duplicate object "type", assume boring */
		if (k_idx == store[STORE_B_MARKET].stock[i].k_idx) return TRUE;
	}

	return FALSE;
}

/*
 * This function will keep 'crap' out of the black market.
 * Crap is defined as any object that is "available" elsewhere
 * Based on a suggestion by "Lee Vogt" <lvogt@cig.mcel.mot.com>
 */
static bool black_market_boring(const object_type *o_ptr)
{
	/* Ego items are never crap */
	if (o_ptr->name2) return FALSE;

	/* Good items are never crap */
	if (o_ptr->to_a > 0) return FALSE;
	if (o_ptr->to_h > 0) return FALSE;
	if (o_ptr->to_d > 0) return FALSE;

	
	/* Check the other stores; if they have it, it's boring */
	return some_legal_store_has_item_type(o_ptr->k_idx);
}

static bool player_armor_boring(const object_type* o_ptr, const u32b* f_ref, s16b total_ac, const object_type* j_ptr)
{
	u32b f[OBJECT_FLAG_STRICT_UB];
	u32b f2[OBJECT_FLAG_STRICT_UB];
	u32b f_mask[OBJECT_FLAG_STRICT_UB];
	int i;

	s16b total_ac2 = j_ptr->ac + j_ptr->to_a;
	bool has_flags = FALSE;
	bool has_flags2 = FALSE;

	object_flags(j_ptr,f2);

	/* mask out bad flags, and boring flags */
	f2[2] &= ~(TR3_IMPACT | TR3_TELEPORT | TR3_AGGRAVATE | TR3_DRAIN_EXP | TR3_LIGHT_CURSE | TR3_HEAVY_CURSE | TR3_PERMA_CURSE | TR3_INSTA_ART | TR3_EASY_KNOW | TR3_HIDE_TYPE | TR3_SHOW_MODS);

	/* mirror object flags */
	C_COPY(f,f_ref,OBJECT_FLAG_STRICT_UB);

	/* build mask */
	for(i = 0; i<OBJECT_FLAG_STRICT_UB; ++i)
		f_mask[i] = f_ref[i] & f2[i];

	/* apply easy masks */
	f[1] &= ~f_mask[1];
	f[1] &= ~f_mask[1];
	f2[2] &= ~f_mask[2];
	f2[2] &= ~f_mask[2];

	/* if they have a decent pval, mask out pval-driven flags we already have as well */
	if (o_ptr->pval <= j_ptr->pval)
		f[0] &= ~f_mask[0];						
	else
		/* mask out binary flags that we already have */
		f[0] &= ~(f_mask[0] & (TR1_SLAY_ANIMAL | TR1_SLAY_EVIL | TR1_SLAY_UNDEAD | TR1_SLAY_DEMON | TR1_SLAY_ORC | TR1_SLAY_TROLL | TR1_SLAY_GIANT | TR1_SLAY_DRAGON | TR1_KILL_DRAGON | TR1_KILL_DEMON | TR1_KILL_UNDEAD | TR1_BRAND_POIS | TR1_BRAND_ACID | TR1_BRAND_ELEC | TR1_BRAND_FIRE | TR1_BRAND_COLD));

	/* if we have a decent pval, mask out pval-driven flags we already have as well */
	if (o_ptr->pval >= j_ptr->pval)
		f2[0] &= ~f_mask[0];						
	else
		/* mask out binary flags that we already have */
		f2[0] &= ~(f_mask[0] & (TR1_SLAY_ANIMAL | TR1_SLAY_EVIL | TR1_SLAY_UNDEAD | TR1_SLAY_DEMON | TR1_SLAY_ORC | TR1_SLAY_TROLL | TR1_SLAY_GIANT | TR1_SLAY_DRAGON | TR1_KILL_DRAGON | TR1_KILL_DEMON | TR1_KILL_UNDEAD | TR1_BRAND_POIS | TR1_BRAND_ACID | TR1_BRAND_ELEC | TR1_BRAND_FIRE | TR1_BRAND_COLD));

	for(i = 0; i<OBJECT_FLAG_STRICT_UB; ++i)
		{
		if (f[i]) has_flags = TRUE;
		if (f2[i]) has_flags2 = TRUE;
		}

	if (has_flags) return FALSE;	/* had something the other item didn't */
	if (has_flags2)
	{	/* only chance is better AC */
		if (total_ac<=total_ac2) return TRUE;	/* oops */
	}
	else
	{	/* no worse AC, or same-type better AC */
		if (total_ac<total_ac2 || o_ptr->obj_id==j_ptr->obj_id) return TRUE;
	}
	return FALSE;
}

static bool player_bow_boring(const object_type* o_ptr, const u32b* f_ref, s32b base_dam_rate1, s32b ideal_dam_rate1, int speed1, int canonical_speed, const object_type* j_ptr)
{
	u32b f[OBJECT_FLAG_STRICT_UB];
	u32b f2[OBJECT_FLAG_STRICT_UB];
	u32b f_mask[OBJECT_FLAG_STRICT_UB];
	s32b base_dam_rate2 = -1000;
	s32b ideal_dam_rate2 = -1000;
	int i;
	s16b speed2 = canonical_speed;
	s16b num_attacks2;
	byte ammo_tval2;
	byte ammo_mult2;

	bool has_flags = FALSE;
/*	bool has_flags2 = FALSE;	*/

	player_missile_shot_analysis(*j_ptr, p_ptr->stat_use[A_STR], num_attacks2, ammo_tval2, ammo_mult2);

	/* adjust working speed if this provides speed */
	if (j_ptr->pval && (object_type::k_info[j_ptr->k_idx].flags[0] & TR1_SPEED))
		speed2 += j_ptr->pval;

	/* consider basic attack power per standard 200 energy */
	/* XXX should pull minroll+maxroll from data files */
	switch(ammo_tval2)
	{
	case TV_SHOT:	{	/* 1d4 Iron Shot */
					base_dam_rate2 = (5+2*j_ptr->to_d)*ammo_mult2*num_attacks2;
					ideal_dam_rate2 = (5+2*(MAX(j_ptr->to_d,9)+9))*ammo_mult2*num_attacks2;
					break;
					};
	case TV_ARROW:	{	/* 1d4 Arrow */
					base_dam_rate2 = (5+2*j_ptr->to_d)*ammo_mult2*num_attacks2;
					ideal_dam_rate2 = (5+2*(MAX(j_ptr->to_d,9)+9))*ammo_mult2*num_attacks2;
					break;
					};
	case TV_BOLT:	{	/* 1d5 Bolt */
					base_dam_rate2 = (6+2*j_ptr->to_d)*ammo_mult2*num_attacks2;
					ideal_dam_rate2 = (6+2*(MAX(j_ptr->to_d,9)+9))*ammo_mult2*num_attacks2;
					break;
					};
	};

	object_flags(j_ptr,f2);

	/* mask out extra shots and might, as they're already factored in */
	f2[0] &= ~(TR1_SHOTS | TR1_MIGHT);

	/* mask out bad flags, and boring flags */
	f2[2] &= ~(TR3_IMPACT | TR3_TELEPORT | TR3_AGGRAVATE | TR3_DRAIN_EXP | TR3_LIGHT_CURSE | TR3_HEAVY_CURSE | TR3_PERMA_CURSE | TR3_INSTA_ART | TR3_EASY_KNOW | TR3_HIDE_TYPE | TR3_SHOW_MODS);

	/* mirror object flags */
	C_COPY(f,f_ref,OBJECT_FLAG_STRICT_UB);

	/* build mask */
	for(i = 0; i<OBJECT_FLAG_STRICT_UB; ++i)
		f_mask[i] = f_ref[i] & f2[i];

	/* apply easy masks */
	f[1] &= ~f_mask[1];
	f[1] &= ~f_mask[1];
	f2[2] &= ~f_mask[2];
	f2[2] &= ~f_mask[2];

	/* if they have a decent pval, mask out pval-driven flags we already have as well */
	if (o_ptr->pval <= j_ptr->pval)
		f[0] &= ~f_mask[0];						
	else
		/* mask out binary flags that we already have */
		f[0] &= ~(f_mask[0] & (TR1_SLAY_ANIMAL | TR1_SLAY_EVIL | TR1_SLAY_UNDEAD | TR1_SLAY_DEMON | TR1_SLAY_ORC | TR1_SLAY_TROLL | TR1_SLAY_GIANT | TR1_SLAY_DRAGON | TR1_KILL_DRAGON | TR1_KILL_DEMON | TR1_KILL_UNDEAD | TR1_BRAND_POIS | TR1_BRAND_ACID | TR1_BRAND_ELEC | TR1_BRAND_FIRE | TR1_BRAND_COLD));

	/* if we have a decent pval, mask out pval-driven flags we already have as well */
	if (o_ptr->pval >= j_ptr->pval)
		f2[0] &= ~f_mask[0];						
	else
		/* mask out binary flags that we already have */
		f2[0] &= ~(f_mask[0] & (TR1_SLAY_ANIMAL | TR1_SLAY_EVIL | TR1_SLAY_UNDEAD | TR1_SLAY_DEMON | TR1_SLAY_ORC | TR1_SLAY_TROLL | TR1_SLAY_GIANT | TR1_SLAY_DRAGON | TR1_KILL_DRAGON | TR1_KILL_DEMON | TR1_KILL_UNDEAD | TR1_BRAND_POIS | TR1_BRAND_ACID | TR1_BRAND_ELEC | TR1_BRAND_FIRE | TR1_BRAND_COLD));

	for(i = 0; i<OBJECT_FLAG_STRICT_UB; ++i)
		{
		if (f[i]) has_flags = TRUE;
/*		if (f2[i]) has_flags2 = TRUE;	*/
		}

	/* consider basic attack power */
	if (has_flags) return FALSE;					/* had something the other item didn't */
	if (   base_dam_rate2*extract_energy[speed1]<base_dam_rate1*extract_energy[speed2]
		&& ideal_dam_rate2*extract_energy[speed1]<ideal_dam_rate1*extract_energy[speed2]) return TRUE;	/* dominated : no flags, worse base and ideal damage */
	return FALSE;
}

static bool player_weapon_boring(const object_type* o_ptr, const u32b* f_ref, s32b base_dam_rate1, s32b ideal_dam_rate1, int speed1, int canonical_speed, const object_type* j_ptr)
{
	u32b f[OBJECT_FLAG_STRICT_UB];
	u32b f2[OBJECT_FLAG_STRICT_UB];
	u32b f_mask[OBJECT_FLAG_STRICT_UB];
	s32b base_dam_rate2 = -1000;
	s32b ideal_dam_rate2 = -1000;
	int i;
	s16b speed2 = canonical_speed;
	s16b num_attacks2;

	bool has_flags = FALSE;
/*	bool has_flags2 = FALSE;	*/

	player_melee_blow_analysis(*j_ptr, p_ptr->stat_use[A_STR], p_ptr->stat_use[A_DEX], num_attacks2);

	/* adjust working speed if this provides speed */
	if (j_ptr->pval && (object_type::k_info[j_ptr->k_idx].flags[0] & TR1_SPEED))
		speed2 += j_ptr->pval;

	/* consider basic attack power per standard 200 energy */
	base_dam_rate2 = (j_ptr->d.minroll()+j_ptr->d.maxroll()+2*o_ptr->to_d)*num_attacks2;
	ideal_dam_rate2 = (j_ptr->d.minroll()+j_ptr->d.maxroll()+2*(MAX(o_ptr->to_d,9)+9))*num_attacks2;

	object_flags(j_ptr,f2);

	/* mask out extra blows, as they're already factored in */
	f2[0] &= ~TR1_BLOWS;

	/* mask out bad flags, and boring flags */
	f2[2] &= ~(TR3_IMPACT | TR3_TELEPORT | TR3_AGGRAVATE | TR3_DRAIN_EXP | TR3_LIGHT_CURSE | TR3_HEAVY_CURSE | TR3_PERMA_CURSE | TR3_INSTA_ART | TR3_EASY_KNOW | TR3_HIDE_TYPE | TR3_SHOW_MODS);

	/* mirror object flags */
	C_COPY(f,f_ref,OBJECT_FLAG_STRICT_UB);

	/* build mask */
	for(i = 0; i<OBJECT_FLAG_STRICT_UB; ++i)
		f_mask[i] = f_ref[i] & f2[i];

	/* apply easy masks */
	f[1] &= ~f_mask[1];
	f[1] &= ~f_mask[1];
	f2[2] &= ~f_mask[2];
	f2[2] &= ~f_mask[2];

	/* if they have a decent pval, mask out pval-driven flags we already have as well */
	if (o_ptr->pval <= j_ptr->pval)
		f[0] &= ~f_mask[0];						
	else
		/* mask out binary flags that we already have */
		f[0] &= ~(f_mask[0] & (TR1_SLAY_ANIMAL | TR1_SLAY_EVIL | TR1_SLAY_UNDEAD | TR1_SLAY_DEMON | TR1_SLAY_ORC | TR1_SLAY_TROLL | TR1_SLAY_GIANT | TR1_SLAY_DRAGON | TR1_KILL_DRAGON | TR1_KILL_DEMON | TR1_KILL_UNDEAD | TR1_BRAND_POIS | TR1_BRAND_ACID | TR1_BRAND_ELEC | TR1_BRAND_FIRE | TR1_BRAND_COLD));

	/* if we have a decent pval, mask out pval-driven flags we already have as well */
	if (o_ptr->pval >= j_ptr->pval)
		f2[0] &= ~f_mask[0];						
	else
		/* mask out binary flags that we already have */
		f2[0] &= ~(f_mask[0] & (TR1_SLAY_ANIMAL | TR1_SLAY_EVIL | TR1_SLAY_UNDEAD | TR1_SLAY_DEMON | TR1_SLAY_ORC | TR1_SLAY_TROLL | TR1_SLAY_GIANT | TR1_SLAY_DRAGON | TR1_KILL_DRAGON | TR1_KILL_DEMON | TR1_KILL_UNDEAD | TR1_BRAND_POIS | TR1_BRAND_ACID | TR1_BRAND_ELEC | TR1_BRAND_FIRE | TR1_BRAND_COLD));

	for(i = 0; i<OBJECT_FLAG_STRICT_UB; ++i)
		{
		if (f[i]) has_flags = TRUE;
/*		if (f2[i]) has_flags2 = TRUE;	*/
		}

	/* consider basic attack power */
	if (has_flags) { return FALSE; } /* had something the other item didn't */
	if (   base_dam_rate2*extract_energy[speed1]<base_dam_rate1*extract_energy[speed2]
		&& ideal_dam_rate2*extract_energy[speed1]<ideal_dam_rate1*extract_energy[speed2]) return TRUE;	/* dominated : no flags, worse base and ideal damage */
	return FALSE;
}

/*
 * This function guesses whether the player would be interested in the item.
 */
static bool item_is_boring_to_player(const object_type *o_ptr)
{
	u32b f_ref[OBJECT_FLAG_STRICT_UB];
	int i;

	object_flags(o_ptr,f_ref);

	/* mask out bad flags, and boring flags */
	f_ref[2] &= ~(TR3_IMPACT | TR3_TELEPORT | TR3_AGGRAVATE | TR3_DRAIN_EXP | TR3_LIGHT_CURSE | TR3_HEAVY_CURSE | TR3_PERMA_CURSE | TR3_INSTA_ART | TR3_EASY_KNOW | TR3_HIDE_TYPE | TR3_SHOW_MODS);

	switch(o_ptr->obj_id.tval)
	{
	case TV_BOOTS:
	case TV_GLOVES:
	case TV_SHIELD:
	case TV_CLOAK:		/* armors that are only used for slot */
		{
		s16b total_ac = o_ptr->ac+o_ptr->to_a;

		/* Check every object in the home */
		for (i = 0; i < store[STORE_HOME].stock_num; ++i)
		{
			const object_type* const j_ptr = store[STORE_HOME].stock+i;
			if (!j_ptr->k_idx) continue;
			if (o_ptr->obj_id.tval!=j_ptr->obj_id.tval) continue;
			if (player_armor_boring(o_ptr,f_ref,total_ac,j_ptr)) return TRUE;
		}

		/* Check every object equipped or in inventory */
		for (i = 0; i < INVEN_TOTAL; ++i)
		{
			const object_type* const j_ptr = p_ptr->inventory+i;
			if (!j_ptr->k_idx) continue;
			if (o_ptr->obj_id.tval!=j_ptr->obj_id.tval) continue;
			if (player_armor_boring(o_ptr,f_ref,total_ac,j_ptr)) return TRUE;
		}
		break;
		};
	case TV_HELM:
	case TV_CROWN:	/* helms and crowns */
		{
		s16b total_ac = o_ptr->ac+o_ptr->to_a;

		/* Check every object in the home */
		for (i = 0; i < store[STORE_HOME].stock_num; ++i)
		{
			const object_type* const j_ptr = store[STORE_HOME].stock+i;
			if (!j_ptr->k_idx) continue;
			if (TV_HELM!=j_ptr->obj_id.tval && TV_CROWN!=j_ptr->obj_id.tval) continue;
			if (player_armor_boring(o_ptr,f_ref,total_ac,j_ptr)) return TRUE;
		}

		/* Check every object equipped or in inventory */
		for (i = 0; i < INVEN_TOTAL; ++i)
		{
			const object_type* const j_ptr = p_ptr->inventory+i;
			if (!j_ptr->k_idx) continue;
			if (TV_HELM!=j_ptr->obj_id.tval && TV_CROWN!=j_ptr->obj_id.tval) continue;
			if (player_armor_boring(o_ptr,f_ref,total_ac,j_ptr)) return TRUE;
		}
		break;
		};
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_DRAG_ARMOR:
		{
		s16b total_ac = o_ptr->ac+o_ptr->to_a;

		/* Check every object in the home */
		for (i = 0; i < store[STORE_HOME].stock_num; ++i)
		{
			const object_type* const j_ptr = store[STORE_HOME].stock+i;
			if (!j_ptr->k_idx) continue;
			if (TV_SOFT_ARMOR!=j_ptr->obj_id.tval && TV_HARD_ARMOR!=j_ptr->obj_id.tval && TV_DRAG_ARMOR!=j_ptr->obj_id.tval) continue;
			if (player_armor_boring(o_ptr,f_ref,total_ac,j_ptr)) return TRUE;
		}

		/* Check every object equipped or in inventory */
		for (i = 0; i < INVEN_TOTAL; ++i)
		{
			const object_type* const j_ptr = p_ptr->inventory+i;
			if (!j_ptr->k_idx) continue;
			if (TV_SOFT_ARMOR!=j_ptr->obj_id.tval && TV_HARD_ARMOR!=j_ptr->obj_id.tval && TV_DRAG_ARMOR!=j_ptr->obj_id.tval) continue;
			if (player_armor_boring(o_ptr,f_ref,total_ac,j_ptr)) return TRUE;
		}
		break;
		};
	case TV_BOW:
		{
		s32b base_dam_rate = -1000;
		s32b ideal_dam_rate = -1000;
		s16b num_attacks = 0;
		s16b canonical_speed = p_ptr->speed;
		s16b working_speed;
		byte ammo_tval;
		byte ammo_mult;

		player_missile_shot_analysis(*o_ptr, p_ptr->stat_use[A_STR], num_attacks, ammo_tval, ammo_mult);

		/* mask out extra shots and extra might, as they're already factored in */
		f_ref[0] &= ~(TR1_SHOTS | TR1_MIGHT);

		/* XXX also pay attention to STR, in case of heavy-bow syndrome */
		/* adjust canonical speed if player's bow is providing speed */
		if (p_ptr->inventory[INVEN_BOW].k_idx && p_ptr->inventory[INVEN_BOW].pval && (object_type::k_info[p_ptr->inventory[INVEN_BOW].k_idx].flags[0] & TR1_SPEED))
			canonical_speed -= p_ptr->inventory[INVEN_BOW].pval;

		working_speed = canonical_speed;

		/* adjust working speed if this provides speed */
		if (o_ptr->pval && (object_type::k_info[o_ptr->k_idx].flags[0] & TR1_SPEED))
			working_speed += o_ptr->pval;

		/* consider basic attack power per standard 200 energy */
		/* XXX should pull minroll+maxroll from data files */
		switch(ammo_tval)
		{
		case TV_SHOT:	{	/* 1d4 Iron Shot */
						base_dam_rate = (5+2*o_ptr->to_d)*ammo_mult*num_attacks;
						ideal_dam_rate = (5+2*(MAX(o_ptr->to_d,9)+9))*ammo_mult*num_attacks;
						break;
						};
		case TV_ARROW:	{	/* 1d4 Arrow */
						base_dam_rate = (5+2*o_ptr->to_d)*ammo_mult*num_attacks;
						ideal_dam_rate = (5+2*(MAX(o_ptr->to_d,9)+9))*ammo_mult*num_attacks;
						break;
						};
		case TV_BOLT:	{	/* 1d5 Bolt */
						base_dam_rate = (6+2*o_ptr->to_d)*ammo_mult*num_attacks;
						ideal_dam_rate = (6+2*(MAX(o_ptr->to_d,9)+9))*ammo_mult*num_attacks;
						break;
						};
		};

		/* Check every object in the home */
		for (i = 0; i < store[STORE_HOME].stock_num; ++i)
		{
			const object_type* const j_ptr = store[STORE_HOME].stock+i;
			if (!j_ptr->k_idx) continue;
			if (TV_BOW!=j_ptr->obj_id.tval) continue;
			if (player_bow_boring(o_ptr,f_ref,base_dam_rate,ideal_dam_rate,working_speed,canonical_speed,j_ptr)) return TRUE;
		}

		/* Check every object equipped or in inventory */
		for (i = 0; i < INVEN_TOTAL; ++i)
		{
			const object_type* const j_ptr = p_ptr->inventory+i;
			if (!j_ptr->k_idx) continue;
			if (TV_BOW!=j_ptr->obj_id.tval) continue;
			if (player_bow_boring(o_ptr,f_ref,base_dam_rate,ideal_dam_rate,working_speed,canonical_speed,j_ptr)) return TRUE;
		}
		break;
		};
	case TV_DIGGING:
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
		{
		s32b base_dam_rate = -1000;
		s32b ideal_dam_rate = -1000;
		s16b num_attacks = 0;
		s16b canonical_speed = p_ptr->speed;
		s16b working_speed;

		player_melee_blow_analysis(*o_ptr, p_ptr->stat_use[A_STR], p_ptr->stat_use[A_DEX], num_attacks);

		/* mask out extra blows, as they're already factored in */
		f_ref[0] &= ~TR1_BLOWS;

		/* XXX also pay attention to STR, in case of heavy-bow syndrome */
		/* XXX also pay attention to DEX, in case of inagility syndrome */
		/* adjust canonical speed if player's bow is providing speed */
		if (p_ptr->inventory[INVEN_BOW].k_idx && p_ptr->inventory[INVEN_BOW].pval && (object_type::k_info[p_ptr->inventory[INVEN_BOW].k_idx].flags[0] & TR1_SPEED))
			canonical_speed -= p_ptr->inventory[INVEN_BOW].pval;

		working_speed = canonical_speed;

		/* adjust working speed if this provides speed */
		if (o_ptr->pval && (object_type::k_info[o_ptr->k_idx].flags[0] & TR1_SPEED))
			working_speed += o_ptr->pval;

		/* consider basic attack power per standard 200 energy */
		base_dam_rate = (o_ptr->d.minroll()+o_ptr->d.maxroll()+2*o_ptr->to_d)*num_attacks;
		ideal_dam_rate = (o_ptr->d.minroll()+o_ptr->d.maxroll()+2*(MAX(o_ptr->to_d,9)+9))*num_attacks;

		/* Check every object in the home */
		for (i = 0; i < store[STORE_HOME].stock_num; ++i)
		{
			const object_type* const j_ptr = store[STORE_HOME].stock+i;
			if (!j_ptr->k_idx) continue;
			if (TV_DIGGING!=j_ptr->obj_id.tval && TV_HAFTED!=j_ptr->obj_id.tval && TV_POLEARM!=j_ptr->obj_id.tval || TV_SWORD!=j_ptr->obj_id.tval) continue;
			if (player_weapon_boring(o_ptr,f_ref,base_dam_rate,ideal_dam_rate,working_speed,canonical_speed,j_ptr)) return TRUE;
		}

		/* Check every object equipped or in inventory */
		for (i = 0; i < INVEN_TOTAL; ++i)
		{
			const object_type* const j_ptr = p_ptr->inventory+i;
			if (!j_ptr->k_idx) continue;
			if (TV_DIGGING!=j_ptr->obj_id.tval && TV_HAFTED!=j_ptr->obj_id.tval && TV_POLEARM!=j_ptr->obj_id.tval || TV_SWORD!=j_ptr->obj_id.tval) continue;
			if (player_weapon_boring(o_ptr,f_ref,base_dam_rate,ideal_dam_rate,working_speed,canonical_speed,j_ptr)) return TRUE;
		}
		break;
		};
	}
	return FALSE;
}

static bool outrageous_item_price(const object_type *o_ptr)
{
	/* No unaffordable items */
	s32b item_worth = price_item(o_ptr, ot_ptr->inflate, FALSE);
	if (item_worth > pc_net_worth) return TRUE;

	/* No absurdly inexpensive items */
	if (item_worth <= (STORE_B_MARKET == store_num ? 10 : 0)) return TRUE;

	return FALSE;	/* We could sell this */
} 

/*
 * Attempt to delete (some of) a random object from the store
 * Hack -- we attempt to "maintain" piles of items when possible.
 */
static void store_delete(void)
{
	if (0 >= st_ptr->stock_num) return;	/* Paranoia */

	int what = rand_int(st_ptr->stock_num);		/* Pick a random slot */
	object_type* o_ptr = &st_ptr->stock[what];	/* Get the object */
	int num = o_ptr->number;					/* Determine how many objects are in the slot */

	/* Hack -- sometimes, only destroy half the objects */
	if (one_in_(2)) num = (num + 1) / 2;

	/* Hack -- sometimes, only destroy a single object */
	if (one_in_(2)) num = 1;

	/* Hack -- decrement the maximum timeouts and total charges of rods and wands. */
	if ((o_ptr->obj_id.tval == TV_ROD) ||
	    (o_ptr->obj_id.tval == TV_STAFF) ||
	    (o_ptr->obj_id.tval == TV_WAND))
	{
		o_ptr->pval -= num * o_ptr->pval / o_ptr->number;
	}

	/* Actually destroy (part of) the object */
	store_item_increase(what, -num);
	store_item_optimize(what);
}

/*
 * The Black Market makes a point of having what you need when no-one else does.
 */
static bool black_market_emergency_stock(void)
{
	object_type o;
	int i;
	s16b kind_stack[8];
	s16b kinds_used = 0;
	bool need_this = TRUE;

	/* Paranoia -- no room left */
	if (store[STORE_B_MARKET].stock_num >= store[STORE_B_MARKET].stock_size) return FALSE;

	/* Word Of Recall : critical */
	if (!player_owns_artifact_with_activation(ACT_WOR))
	{	/* no artifact providing this */
		kind_stack[kinds_used++] = lookup_kind2(TV_SCROLL,SV_SCROLL_WORD_OF_RECALL);	/* stock this */
		assert(0<kind_stack[kinds_used-1]);
	
		kind_stack[kinds_used++] = lookup_kind2(TV_ROD,SV_ROD_RECALL);				/* this also provides */
		assert(0<kind_stack[kinds_used-1]);

		switch(p_ptr->spell_book())
		{
		case TV_PRAYER_BOOK:	{	/* Sync with x-spell.c: spell provides if can be cast */
								const magic_type* s_info = p_ptr->spell_info(PRAYER_WORD_OF_RECALL);
								if (s_info->slevel <= p_ptr->lev && s_info->smana <= p_ptr->msp)
									{
									kind_stack[kinds_used++] = lookup_kind2(TV_PRAYER_BOOK,get_spell_book(PRAYER_WORD_OF_RECALL));
									assert(0<kind_stack[kinds_used-1]);
									}
								break;
								}
		case TV_MAGIC_BOOK:		{	/* Sync with x-spell.c: spell provides if can be cast */
								const magic_type* s_info = p_ptr->spell_info(SPELL_WORD_OF_RECALL);
								if (s_info->slevel <= p_ptr->lev && s_info->smana <= p_ptr->msp)
									{
									kind_stack[kinds_used++] = lookup_kind2(TV_MAGIC_BOOK,get_spell_book(SPELL_WORD_OF_RECALL));
									assert(0<kind_stack[kinds_used-1]);
									}
								break;
								}
		};

		/* does @ need this */
		while(0<kinds_used)
			if (some_legal_store_has_item_type(kind_stack[--kinds_used]) || player_owns_item_type(kind_stack[kinds_used]) || black_market_has_item_type(kind_stack[kinds_used]))
				need_this = FALSE;

		if (need_this)
		{
			WIPE(&o);	/* clear temporary object */

			/* Create a new object of the chosen kind */
			object_prep(&o, kind_stack[0]);

			object_known(&o);		/* The object is "known" */
			o.ident |= IDENT_STORE;	/* Item belongs to a store */

			/* No unaffordable or absurdly inexpensive items */
			if (!outrageous_item_price(&o))
			{
				mass_produce(&o);	/* Mass produce */
				store_carry(&o);	/* Attempt to carry the (known) object */
				return TRUE;		/* did an emergency stock */
			}
		}


		/* reset */
		need_this = TRUE;
		kinds_used = 0;
	};

	/* stat restore is critical */
	for(i = A_STR; i<A_MAX; ++i)
		if (p_ptr->stat_cur[i]<p_ptr->stat_max[i])
		{
			kind_stack[kinds_used++] = lookup_kind2(TV_POTION,SV_POTION_RES_STR+i);	/* stock this */
			assert(0<kind_stack[kinds_used-1]);

			kind_stack[kinds_used++] = lookup_kind2(TV_POTION,SV_POTION_INC_STR+i);	/* this also provides */
			assert(0<kind_stack[kinds_used-1]);

			kind_stack[kinds_used++] = lookup_kind2(TV_POTION,SV_POTION_AUGMENTATION);	/* XXX optimization target XXX */
			assert(0<kind_stack[kinds_used-1]);

			kind_stack[kinds_used++] = lookup_kind2(TV_POTION,SV_POTION_LIFE);	/* XXX optimization target XXX */
			assert(0<kind_stack[kinds_used-1]);

			kind_stack[kinds_used++] = lookup_kind2(TV_FOOD,SV_FOOD_RESTORING);	/* XXX optimization target XXX */
			assert(0<kind_stack[kinds_used-1]);

			kind_stack[kinds_used++] = lookup_kind2(TV_ROD,SV_ROD_RESTORATION);	/* XXX optimization target XXX */
			assert(0<kind_stack[kinds_used-1]);
			
			switch(i)
			{
			case A_STR:	{
						kind_stack[kinds_used++] = lookup_kind2(TV_FOOD,SV_FOOD_RESTORE_STR);
						assert(0<kind_stack[kinds_used-1]);
						break;
						}
			case A_INT:
			case A_WIS:	{
						kind_stack[kinds_used++] = lookup_kind2(TV_POTION,SV_POTION_STAR_ENLIGHTENMENT);
						assert(0<kind_stack[kinds_used-1]);
						break;
						}
			case A_CON:	{
						kind_stack[kinds_used++] = lookup_kind2(TV_FOOD,SV_FOOD_RESTORE_CON);
						assert(0<kind_stack[kinds_used-1]);
						break;
						}
			};

			if (TV_PRAYER_BOOK == p_ptr->spell_book())
			{
				const magic_type* s_info = p_ptr->spell_info(PRAYER_RESTORATION);
				if (s_info->slevel <= p_ptr->lev && s_info->smana <= p_ptr->msp)
					{
					kind_stack[kinds_used++] = lookup_kind2(TV_PRAYER_BOOK,get_spell_book(PRAYER_RESTORATION));
					assert(0<kind_stack[kinds_used-1]);
					}
			};

			/* does @ need this */
			while(0<kinds_used)
				if (some_legal_store_has_item_type(kind_stack[--kinds_used]) || player_owns_item_type(kind_stack[kinds_used]) || black_market_has_item_type(kind_stack[kinds_used]))
					need_this = FALSE;

			if (need_this)
			{
				WIPE(&o);	/* clear temporary object */

				/* Create a new object of the chosen kind */
				object_prep(&o, kind_stack[0]);

				object_known(&o);		/* The object is "known" */
				o.ident |= IDENT_STORE;	/* Item belongs to a store */

				/* No unaffordable or absurdly inexpensive items */
				if (!outrageous_item_price(&o))
				{
					mass_produce(&o);	/* Mass produce */
					store_carry(&o);	/* Attempt to carry the (known) object */
					return TRUE;		/* did an emergency stock */
				}
			}

			/* reset */
			need_this = TRUE;
			kinds_used = 0;
		}

	/* Restore Experience */
	if (p_ptr->max_exp<p_ptr->max_exp && !player_owns_artifact_with_activation(ACT_RESTORE_LIFE))
	{	/* no artifact providing this */
		kind_stack[kinds_used++] = lookup_kind2(TV_POTION,SV_POTION_RESTORE_EXP);	/* stock this */
		assert(0<kind_stack[kinds_used-1]);
	
		kind_stack[kinds_used++] = lookup_kind2(TV_POTION,SV_POTION_LIFE);
		assert(0<kind_stack[kinds_used-1]);

		kind_stack[kinds_used++] = lookup_kind2(TV_POTION,SV_POTION_EXPERIENCE);
		assert(0<kind_stack[kinds_used-1]);

		kind_stack[kinds_used++] = lookup_kind2(TV_ROD,SV_ROD_RESTORATION);	/* XXX optimization target XXX */
		assert(0<kind_stack[kinds_used-1]);

		if (TV_PRAYER_BOOK == p_ptr->spell_book())
		{
			const magic_type* s_info = p_ptr->spell_info(PRAYER_REMEMBRANCE);
			if (s_info->slevel <= p_ptr->lev && s_info->smana <= p_ptr->msp)
				{
				kind_stack[kinds_used++] = lookup_kind2(TV_PRAYER_BOOK,get_spell_book(PRAYER_REMEMBRANCE));
				assert(0<kind_stack[kinds_used-1]);
				}
		};

		/* does @ need this */
		while(0<kinds_used)
			if (some_legal_store_has_item_type(kind_stack[--kinds_used]) || player_owns_item_type(kind_stack[kinds_used]) || black_market_has_item_type(kind_stack[kinds_used]))
				need_this = FALSE;

		if (need_this)
		{
			WIPE(&o);	/* clear temporary object */

			/* Create a new object of the chosen kind */
			object_prep(&o, kind_stack[0]);

			object_known(&o);		/* The object is "known" */
			o.ident |= IDENT_STORE;	/* Item belongs to a store */

			/* No unaffordable or absurdly inexpensive items */
			if (!outrageous_item_price(&o))
			{
				mass_produce(&o);	/* Mass produce */
				store_carry(&o);	/* Attempt to carry the (known) object */
				return TRUE;		/* did an emergency stock */
			}
		}


		/* reset */
		need_this = TRUE;
		kinds_used = 0;
	};

	return FALSE;	/* no emergency stocking, proceed normally */
}

/*
 * Creates a random object and gives it to a store
 * This algorithm needs to be rethought.  A lot.
 *
 * Note -- the "level" given to "obj_get_num()" is a "favored"
 * level, that is, there is a much higher chance of getting
 * items with a level approaching that of the given level...
 *
 * Should we check for "permission" to have the given object?
 */
static void store_create(void)
{
	int item_create_tries = 0;	/* XXX give up on item filtration if it's causing a spinlock */

	int k_idx, tries, level;
	object_type o;	/* local object */

	/* Paranoia -- no room left */
	if (st_ptr->stock_num >= st_ptr->stock_size) return;


	/* Hack -- consider up to four items */
	for (tries = 0; tries < 4; tries++)
	{
		/* Black Market */
		if (store_num == STORE_B_MARKET)
		{
			/* emergency stocking counts */
			if (black_market_emergency_stock()) break;

			/* Pick a level for object/magic */
			level = 25 + rand_int(25);

			/* Random object kind (usually of given level) */
			k_idx = get_obj_num(level);

			/* Handle failure */
			if (!k_idx) continue;
		}

		/* Normal Store */
		else
		{
			/* Hack -- Pick an object kind to sell */
			k_idx = get_store_choice(store_num);

			/* Hack -- fake level for apply_magic() */
			level = rand_range(1, STORE_OBJ_LEVEL);
		}

		/* Create a new object of the chosen kind */
		object_prep(&o, k_idx);

		/* Apply some "low-level" magic (no artifacts) */
		apply_magic(&o, level, FALSE, FALSE, FALSE);

		/* Mega-Hack -- no chests in stores */
		if (o.obj_id.tval == TV_CHEST) continue;

		/* Hack -- Charge lite's */
		if (o.obj_id.tval == TV_LITE)
		{
			if (o.obj_id.sval == SV_LITE_TORCH) o.pval = (7*FUEL_TORCH) / 10;
			if (o.obj_id.sval == SV_LITE_LANTERN) o.pval = FUEL_LAMP / 2;
		}

		object_known(&o);		/* The object is "known" */
		o.ident |= IDENT_STORE;	/* Item belongs to a store */

		/* Prune the black market */
		if (store_num == STORE_B_MARKET)
		{
			/* Black market doesn't like to carry anything legal and available */
			if (black_market_boring(&o)) continue;
		};

		/* XXX drop retry filter after too many items XXX */
		if (20 >= ++item_create_tries)
		{
			/* No unaffordable or absurdly inexpensive items */
			if (outrageous_item_price(&o)) {--tries; continue; }

			/* No items that bore the player */
			if (item_is_boring_to_player(&o)) {--tries; continue; }

			/* No items we already have in stock */
			if (store_has_item_type(k_idx,store_num)) {--tries; continue; };
		};

		mass_produce(&o);		/* Mass produce */
		store_carry(&o);		/* Attempt to carry the (known) object */
		break;					/* Definitely done */
	}
}

/*
 *  Report whether an item has its quantity display-clamped
 */
static bool display_clamped(const object_type* o_ptr)
{
		/* XXX armors and main weapons should display only one XXX */
		switch(o_ptr->obj_id.tval)
		{
		case TV_RING:		/* Can only wear two rings, but the interesting ones won't be mass-produced */
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
		case TV_AMULET:		/* can only wear one of these at a time */
							return TRUE;
		default:			return FALSE;
		}
}

/*
 * Redisplay a single store entry
 */
static bool display_entry(int item)
{
	char o_name[80];
	char out_val[160];
	object_type *o_ptr = &st_ptr->stock[item];			/* Get the object */
	s32b x;
	int y;
	int maxwid;

	if (o_ptr->ident & IDENT_HIDE) return FALSE;	/* hide if requested */

	/* convert to visual index */
	item = internal_to_visual_index(item);

	/* Must be on current "page" to get displayed */
	if (!((item >= store_top) && (item < store_top + 12))) return FALSE;

	y = (item % 12) + 6;			/* Get the row */

	/* Label it, clear the line --(-- */
	sprintf(out_val, "%c) ", store_to_label(item));
	prt(out_val, y, 0);

	/* Describe an object in the home */
	if (store_num == STORE_HOME)
	{
		byte attr;

		maxwid = 75;

		/* Leave room for weights, if necessary -DRS- */
		if (OPTION(show_weights)) maxwid -= 10;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);
		o_name[maxwid] = '\0';

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->obj_id.tval & 0x7F];

		/* Display the object */
		c_put_str(attr, o_name, y, 3);

		/* Show weights */
		if (OPTION(show_weights))
		{
			/* Only show the weight of a single object */
			int wgt = o_ptr->weight;
			sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
			put_str(out_val, y, 68);
		}
	}

	/* Describe an object (fully) in a store */
	else
	{
		byte attr;
		const byte backup_quantity = o_ptr->number;

		/* Zaiband: Viewing an object in a store makes you aware of it */
		object_aware(o_ptr);

		/* Must leave room for the "price" */
		maxwid = 65;

		/* Leave room for weights, if necessary -DRS- */
		if (OPTION(show_weights)) maxwid -= 7;

		/* XXX armors and main weapons should display only one XXX */
		if (display_clamped(o_ptr)) o_ptr->number = 1;

		/* Describe the object (fully) */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);
		o_name[maxwid] = '\0';

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->obj_id.tval & 0x7F];

		/* Display the object */
		c_put_str(attr, o_name, y, 3);

		/* Show weights */
		if (OPTION(show_weights))
		{			/* Only show the weight of a single object */
			int wgt = o_ptr->weight;
			sprintf(out_val, "%3d.%d", wgt / 10, wgt % 10);
			put_str(out_val, y, 61);
		}

		/* Extract the "minimum" price */
		x = price_item(o_ptr, ot_ptr->inflate, FALSE);

		/* Actually draw the price (with tax) */
		if (((o_ptr->obj_id.tval == TV_WAND) || (o_ptr->obj_id.tval == TV_STAFF)) &&
		    ((o_ptr->pval % o_ptr->number) > 0))
			sprintf(out_val, "%9ld avg", (long)x);
		else
			sprintf(out_val, "%9ld  ", (long)x);

		put_str(out_val, y, 68);
	
		o_ptr->number = backup_quantity;
	}
	return TRUE;
}


/*
 * Display a store's inventory
 *
 * All prices are listed as "per individual object"
 */
static void display_inventory(void)
{
	int i;
	int k = 0;

	/* Display the next 12 items */
	for (i = 0; i < st_ptr->stock_size; ++i)
	{
		/* Stop when we run out of items */
		if (i >= st_ptr->stock_num) break;

		/* Display that line */
		if (display_entry(i)) ++k;
		if (12 == k) break;
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
		if (OPTION(show_weights)) put_str("Weight", 5, 70);
	}

	/* Normal stores */
	else
	{
		const char* const store_name = feature_type::f_info[FEAT_SHOP_HEAD + store_num].name();
		const char* const owner_name = &(b_name[ot_ptr->owner_name]);
		const char* const race_name = player_type::race_name(ot_ptr->owner_race);

		/* Put the owner name and race */
		strnfmt(buf, sizeof(buf), "%s (%s)", owner_name, race_name);
		put_str(buf, 3, 10);

		/* Show the max price in the store (above prices) */
		strnfmt(buf, sizeof(buf), "%s (%ld)", store_name, (long)(ot_ptr->max_cost));
		prt(buf, 3, 50);

		/* Label the object descriptions */
		put_str("Item Description", 5, 3);

		/* If showing weights, show label */
		if (OPTION(show_weights)) put_str("Weight", 5, 60);

		/* Label the asking price (in stores) */
		put_str("Price", 5, 72);
	}

	store_prt_gold();		/* Display the current gold */
	display_inventory();	/* Draw in the inventory */
}



/*
 * Get the index of a store object
 *
 * Return TRUE if an object was selected
 */
static bool get_stock(int *com_val, const char* const pmt)
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
	        store_to_label(0), store_to_label(internal_to_visual_index(st_ptr->stock_num - 1)),
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
		{	/* Oops */
			bell("Illegal store object choice!");
			continue;
		}

		if (!verify) break;				/* No verification */
		o_ptr = &st_ptr->stock[item];	/* Object */

		/* Describe */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

		/* Prompt */
		strnfmt(out_val, sizeof(out_val), "Try %s? ", o_name);

		/* Query */
		if (!get_check(out_val)) return (FALSE);

		break;							/* Done */
	}

	/* Save item */
	(*com_val) = item;

	repeat_push(*com_val);

	/* Success */
	return (TRUE);
}


/*
 * Get a haggle
 */
static int get_haggle(const char* const pmt, s32b *poffer, s32b price)
{
	/* Paranoia XXX XXX XXX */
	message_flush();


	/* Ask until done */
	while (TRUE)
	{
		const char* p;

		char out_val[80];

		/* Default */
		out_val[0] = '\x00';

		/* Ask the user for a response */
		if (!get_string(pmt, out_val, sizeof(out_val))) return (FALSE);

		/* Skip leading spaces */
		for (p = out_val; *p == ' '; p++) /* loop */;

		/* Empty response */
		if (*p == '\0')
		{
			/* Accept current price */
			*poffer = price;
			break;
		}

		/* Normal response */
		else
		{
			s32b i;
			char *err_ptr;

			/* Extract a number */
			i = strtol(p, &err_ptr, 10);

			/* Valid number? */
			if (*err_ptr == '\0')
			{
				/* Use the given "number" */
				*poffer = i;
				break;
			}
		}

		/* Warning */
		msg_print("Invalid response.");
		message_flush();
	}

	/* Success */
	return (TRUE);
}


/*
 * Receive an offer (from the player)
 *
 * Return TRUE if offer is NOT okay
 */
static bool receive_offer(const char* const pmt, s32b *poffer,
                          int factor, s32b price)
{
	/* Haggle till done */
	while (TRUE)
	{
		/* Get a haggle (or cancel) */
		if (!get_haggle(pmt, poffer, price)) return (TRUE);

		/* Acceptable offer */
		if (((*poffer) * factor) >= (price * factor)) break;

		/* Display and flush insult */
		say_comment_insult();

		/* Reject offer (correctly) */
		(*poffer) = price;
	}

	/* Success */
	return (FALSE);
}


/*
 * Haggling routine
 *
 * Return TRUE if purchase is successful
 */
static bool purchase_haggle(object_type *o_ptr, s32b *price)
{
	s32b ask_price;
	s32b offer;
	int flag, loop_flag;

	bool cancel = FALSE;

	const char* const pmt = "Final Offer";

	char out_val[160];


	*price = 0;


	/* Extract the asking price */
	ask_price = price_item(o_ptr, ot_ptr->inflate, FALSE);

	/* Message summary */
	msg_print("You quickly agree upon the price.");
	message_flush();

	/* Haggle for the whole pile */
	ask_price *= o_ptr->number;


	/* No offer yet */
	offer = 0;

	/* Haggle until done */
	for (flag = FALSE; !flag; )
	{
		loop_flag = TRUE;

		while (!flag && loop_flag)
		{
			strnfmt(out_val, sizeof(out_val), "%s :  %ld", pmt, (long)ask_price);
			put_str(out_val, 1, 0);
			cancel = receive_offer("What do you offer? [accept] ",
			                       &offer, 1, ask_price);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer > ask_price)
			{
				say_comment_nonsense();
			}
			else if (offer == ask_price)
			{
				flag = TRUE;
				*price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}
		}

		if (!flag)
		{
			if (offer >= ask_price)
			{
				flag = TRUE;
				*price = offer;
			}
		}
	}

	/* Cancel */
	if (cancel) return (FALSE);

	/* Do not cancel */
	return (TRUE);
}


/*
 * Haggling routine
 *
 * Return TRUE if purchase is successful
 */
static bool sell_haggle(object_type *o_ptr, s32b *price)
{
	s32b purse, ask_price;
	s32b offer = 0;

	int flag, loop_flag;

	bool cancel = FALSE;

	const char* const pmt = "Final Offer";

	char out_val[160];


	*price = 0;


	/* Obtain the asking price */
	ask_price = price_item(o_ptr, ot_ptr->inflate, TRUE);

	/* Get the owner's payout limit */
	purse = ot_ptr->max_cost;

	/* No reason to haggle */
	if (ask_price >= purse)
	{
		/* Message */
		msg_print("You instantly agree upon the price.");
		message_flush();

		/* Offer full purse */
		ask_price = purse;
	}

	/* No haggle option */
	else
	{
		/* Message summary */
		msg_print("You quickly agree upon the price.");
		message_flush();
	}

	/* Haggle for the whole pile */
	ask_price *= o_ptr->number;


	/* Display commands XXX XXX XXX */

	/* No offer yet */
	offer = 0;

	/* Haggle */
	for (flag = FALSE; !flag; )
	{
		while (1)
		{
			loop_flag = TRUE;

			strnfmt(out_val, sizeof(out_val), "%s :  %ld", pmt, (long)ask_price);
			put_str(out_val, 1, 0);
			cancel = receive_offer("What price do you ask? ",
			                       &offer, -1, ask_price);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer < ask_price)
			{
				say_comment_nonsense();
			}
			else if (offer == ask_price)
			{
				flag = TRUE;
				*price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}

			/* Stop */
			if (flag || !loop_flag) break;
		}

		if (!flag)
		{
			if (offer <= ask_price)
			{
				flag = TRUE;
				*price = offer;
			}
		}
	}

	/* Cancel */
	if (cancel) return (FALSE);

	/* Do not cancel */
	return (TRUE);
}





/*
 * Buy an object from a store
 */
static void store_purchase(void)
{
	int n;
	int amt;
	int item, item_new;

	s32b price;

	object_type *o_ptr;

	object_type object_type_body;
	object_type *i_ptr = &object_type_body;	/* Get local object */

	char o_name[80];

	char out_val[160];
	bool display_lockdown;


	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		msg_print((STORE_HOME == store_num)	? "Your home is empty."
											: "I am currently out of stock.");
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
	display_lockdown = display_clamped(o_ptr);

	/* Get a quantity */
	amt = get_quantity(NULL, (display_lockdown) ? 1 : o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Get desired object */
	*i_ptr = *o_ptr;

	/*
	 * Hack -- If a rod or wand, allocate total maximum timeouts or charges
	 * between those purchased and left on the shelf.
	 */
	reduce_charges(i_ptr, i_ptr->number - amt);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(i_ptr))
	{
		msg_print("You cannot carry that many items.");
		return;
	}

	/* Attempt to buy it */
	if (store_num != STORE_HOME)
	{
		/* Describe the object (fully) */
		object_desc(o_name, sizeof(o_name), i_ptr, TRUE, ODESC_FULL);

		/* Message */
		msg_format("Buying %s (%c).",
		           o_name, store_to_label(item));
		message_flush();

		/* Haggle for a final price */
		if (purchase_haggle(i_ptr, &price))
		{
			/* Player can afford it */
			if (p_ptr->au >= price)
			{
				/* Say "okay" */
				say_comment_accept();

				/* Spend the money */
				p_ptr->au -= price;

				/* Update the display */
				store_prt_gold();

				/* Combine / Reorder the pack (later) */
				p_ptr->notice |= (PN_COMBINE | PN_REORDER);

				/* The object no longer belongs to the store */
				i_ptr->ident &= ~(IDENT_STORE);

				/* Describe the transaction */
				object_desc(o_name, sizeof(o_name), i_ptr, TRUE, ODESC_FULL);

				/* Message */
				msg_format("You bought %s (%c) for %ld gold.",
				           o_name, store_to_label(item),
				           (long)price);

				/* Erase the inscription */
				i_ptr->note = 0;

				/* Remove special inscription, if any */
				o_ptr->pseudo = 0;

				/* Give it to the player */
				item_new = inven_carry(i_ptr);

				/* Describe the final result */
				object_desc(o_name, sizeof(o_name), &p_ptr->inventory[item_new], TRUE, ODESC_FULL);

				/* Message */
				msg_format("You have %s (%c).",
				           o_name, index_to_label(item_new));

				/* Now, reduce the original stack's pval */
				if ((o_ptr->obj_id.tval == TV_ROD) ||
				    (o_ptr->obj_id.tval == TV_WAND) ||
				    (o_ptr->obj_id.tval == TV_STAFF))
				{
					o_ptr->pval -= i_ptr->pval;
				}

				/* XXX hide item if display quantity is clamped XXX */
				if (display_lockdown) o_ptr->ident |= IDENT_HIDE;

				/* Handle stuff */
				handle_stuff();

				/* Note how many slots the store used to have */
				n = st_ptr->stock_num;

				/* Remove the bought objects from the store */
				store_item_increase(item, -amt);
				store_item_optimize(item);

				/* Store is empty */
				if (st_ptr->stock_num == 0 || -1==visual_to_internal_index(0))
				{
					int i;

					/* Shuffle */
					if (one_in_(STORE_SHUFFLE))
					{
						/* Message */
						msg_print("The shopkeeper retires.");

						/* Shuffle the store */
						store_shuffle(store_num);
					}

					/* Maintain */
					else
					{
						/* Message */
						msg_print("The shopkeeper brings out some new stock.");
					}

					/* New inventory */
					for (i = 0; i < 10; ++i)
					{
						/* Maintain the store */
						store_maint(store_num);
					}

					/* Start over */
					store_top = 0;

					/* Redraw everything */
					display_inventory();
				}

				/* The object is gone */
				else if (st_ptr->stock_num != n || display_lockdown)
				{
					/* Only one screen left */
					if (internal_to_visual_index(st_ptr->stock_num-1) <= 12) store_top = 0;

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

			/* Player cannot afford it */
			else
			{
				/* Simple message (no insult) */
				msg_print("You do not have enough gold.");
			}
		}
	}

	/* Home is much easier */
	else
	{
		/* Distribute charges of wands, staves, or rods */
		distribute_charges(o_ptr, i_ptr, amt);

		/* Give it to the player */
		item_new = inven_carry(i_ptr);

		/* Describe just the result */
		object_desc(o_name, sizeof(o_name), &p_ptr->inventory[item_new], TRUE, ODESC_FULL);

		/* Message */
		msg_format("You have %s (%c).", o_name, index_to_label(item_new));

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
			if (st_ptr->stock_num <= 12) store_top = 0;

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

	object_type object_type_body;
	object_type *o_ptr;
	object_type *i_ptr = &object_type_body;	/* Get local object */

	const char* q = "Drop which item? ";	/* Home */
	const char* s = "You have nothing that I want.";

	char o_name[120];


	/* Real store */
	if (store_num != STORE_HOME)
	{
		/* New prompt */
		q = "Sell which item? ";

		/* Only allow items the store will buy */
		item_tester_hook = store_will_buy_tester;
	}

	/* Get an item */
	p_ptr->command_wrk = (USE_INVEN);
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	/* Hack -- Cannot remove cursed objects */
	if ((item >= INVEN_WIELD) && o_ptr->is_cursed())
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Get a copy of the object */
	*i_ptr = *o_ptr;

	/* Modify quantity */
	i_ptr->number = amt;

	/* Hack -- If a rod, wand, or staff, allocate total maximum
	 * timeouts or charges to those being sold.
	 */
	if ((o_ptr->obj_id.tval == TV_ROD) ||
	    (o_ptr->obj_id.tval == TV_WAND) ||
	    (o_ptr->obj_id.tval == TV_STAFF))
	{
		i_ptr->pval = o_ptr->pval * amt / o_ptr->number;
	}

	/* Get a full description */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, ODESC_FULL);


	/* Is there room in the store (or the home?) */
	if (!store_check_num(i_ptr))
	{
		msg_print((STORE_HOME == store_num)	? "Your home is full."
											: "I have not the room in my store to keep it.");
		return;
	}


	/* Real store */
	if (store_num != STORE_HOME)
	{
		/* Describe the transaction */
		msg_format("Selling %s (%c).", o_name, index_to_label(item));
		message_flush();

		/* Haggle for it */
		if (sell_haggle(i_ptr, &price))
		{
			/* Say "okay" */
			say_comment_accept();

			/* Get some money */
			p_ptr->au += price;

			/* Update the display */
			store_prt_gold();

			/* Get the "apparent" value */
			dummy = object_value(i_ptr) * i_ptr->number;

			/* Erase the inscription */
			i_ptr->note = 0;

			/* Remove special inscription, if any */
			o_ptr->pseudo = 0;

			/* Identify original object */
			object_aware(o_ptr);
			object_known(o_ptr);

			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);

			/* Window stuff */
			p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

			/* Get a copy of the object */
			*i_ptr = *o_ptr;

			/* Modify quantity */
			i_ptr->number = amt;

			/* The object belongs to the store now */
			i_ptr->ident |= IDENT_STORE;

			/*
			 * Hack -- Allocate charges between those wands, staves, or rods
			 * sold and retained, unless all are being sold.
			 */
			distribute_charges(o_ptr, i_ptr, amt);

			/* Get the "actual" value */
			value = object_value(i_ptr) * i_ptr->number;

			/* Get the description all over again */
			object_desc(o_name, sizeof(o_name), i_ptr, TRUE, ODESC_FULL);

			/* Describe the result (in message buffer) */
			msg_format("You sold %s (%c) for %ld gold.",
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
			item_pos = store_carry(i_ptr);

			/* Redisplay wares, if needed */
			if (item_pos >= 0) display_inventory();
		}
	}

	/* Player is at home */
	else
	{
		/* Distribute charges of wands/staves/rods */
		distribute_charges(o_ptr, i_ptr, amt);

		/* Describe */
		msg_format("You drop %s (%c).", o_name, index_to_label(item));

		/* Take it from the players inventory */
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);

		/* Handle stuff */
		handle_stuff();

		/* Let the home carry it */
		item_pos = home_carry(i_ptr);

		/* Redisplay wares, if needed */
		if (item_pos >= 0) display_inventory();
	}
}


/*
 * Examine an item in a store
 */
static void store_examine(void)
{
	int         item;

	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		msg_print((STORE_HOME == store_num) ? "Your home is empty."
											: "I am currently out of stock.");
		return;
	}

	/* Get the item number to be examined */
	if (!get_stock(&item, ((OPTION(rogue_like_commands)) ? "Which item do you want to examine? "
												 : "Which item do you want to look at? ")))
		return;

	/* Describe it fully */
	object_info_screen(&st_ptr->stock[item]);
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
 */
static void store_process_command(void)
{

	/* Handle repeating the last command */
	repeat_check();

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
			if (st_ptr->stock_num <= 12)
			{
				/* Nothing to see */
				msg_print("Entire inventory is shown.");
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


		/* Redraw */
		case KTRL('R'):
		{
			do_cmd_redraw();
			display_store();
			break;
		}

		/* Get (purchase) */
		case 'g':
		{
			store_purchase();
			break;
		}

		/* Drop (Sell) */
		case 'd':
		{
			store_sell();
			break;
		}

		/* Examine */
		case 'l':
		{
			store_examine();
			break;
		}


		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			do_cmd_wield();
			break;
		}

		/* Take off equipment */
		case 't':
		{
			do_cmd_takeoff();
			break;
		}

		/* Destroy an item */
		case 'k':
		{
			do_cmd_destroy();
			break;
		}

		/* Equipment list */
		case 'e':
		{
			do_cmd_equip();
			break;
		}

		/* Inventory list */
		case 'i':
		{
			do_cmd_inven();
			break;
		}


		/*** Various commands ***/

		/* Identify an object */
		case 'I':
		{
			do_cmd_observe();
			break;
		}

		/* Hack -- toggle windows */
		case KTRL('E'):
		{
			toggle_inven_equip();
			break;
		}



		/*** Use various objects ***/

		/* Browse a book */
		case 'b':
		{
			do_cmd_browse();
			break;
		}

		/* Inscribe an object */
		case '{':
		{
			do_cmd_inscribe();
			break;
		}

		/* Uninscribe an object */
		case '}':
		{
			do_cmd_uninscribe();
			break;
		}



		/*** Help and Such ***/

		/* Help */
		case '?':
		{
			do_cmd_help();
			break;
		}

		/* Identify symbol */
		case '/':
		{
			do_cmd_query_symbol();
			break;
		}

		/* Character description */
		case 'C':
		{
			do_cmd_change_name();
			break;
		}


		/*** System Commands ***/

		/* Hack -- User interface */
		case '!':
		{
			(void)Term_user(0);
			break;
		}

		/* Single line from a pref file */
		case '"':
		{
			do_cmd_pref();
			break;
		}

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals();
			break;
		}

		/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			break;
		}

		/* Interact with options */
		case '=':
		{
			do_cmd_options();
			do_cmd_redraw();
			display_store();
			break;
		}


		/*** Misc Commands ***/

		/* Take notes */
		case ':':
		{
			do_cmd_note();
			break;
		}

		/* Version info */
		case 'V':
		{
			do_cmd_version();
			break;
		}

		/* Repeat level feeling */
		case KTRL('F'):
		{
			do_cmd_feeling();
			break;
		}

		/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages();
			break;
		}

		/* Check knowledge */
		case '~':
		case '|':
		{
			do_cmd_knowledge();
			break;
		}

		/* Load "screen dump" */
		case '(':
		{
			do_cmd_load_screen();
			break;
		}

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}


		/* Hack -- Unknown command */
		default:
		{
			msg_print("That command does not work in stores.");
			break;
		}
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
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	int tmp_chr;


	/* Verify a store */
	ZAIBAND_STATIC_ASSERT((FEAT_SHOP_TAIL-FEAT_SHOP_HEAD+1)==MAX_STORES);
	if (!cave_feat_in_range(py,px,FEAT_SHOP_HEAD,FEAT_SHOP_TAIL))
	{
		msg_print("You see no store here.");
		return;
	}

	/* The doors are locked on ironmen */
	if (OPTION(adult_no_stores))
	{
		msg_print("The doors are locked.");
		return;
	}


	forget_view();		/* Forget the view */
	character_icky++;	/* Hack -- Increase "icky" depth */

	p_ptr->command_arg = 0;	/* No command argument */
	p_ptr->command_rep = 0;	/* No repeated command */
	p_ptr->command_new = 0;	/* No automatic command */


	activate_store((store_indexes)(cave_feat[py][px] - FEAT_SHOP_HEAD));

	store_top = 0;			/* Start at the beginning */
	display_store();		/* Display the store */
	leave_store = FALSE;	/* Do not leave */

	/*
	 * Play a special sound if entering the 
	 * home, otherwise play a generic store sound.
	 */
	sound((STORE_HOME == store_num) ? MSG_STORE_HOME : MSG_STORE_ENTER);

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
		prt(" g) Get/Purchase an item.", 22, 29);
		prt(" d) Drop/Sell an item.", 23, 29);

		/* Add in the eXamine option */
		if (OPTION(rogue_like_commands))
			prt(" x) eXamine an item.", 22, 56);
		else
			prt(" l) Look at an item.", 22, 56);

		/* Prompt */
		prt("You may: ", 21, 0);

		/* Get a command */
		request_command(TRUE);

		/* Process the command */
		store_process_command();

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Pack Overflow XXX XXX XXX */
		if (p_ptr->inventory[INVEN_PACK].k_idx)
		{
			const object_type* const o_ptr = &p_ptr->inventory[INVEN_PACK];

			/* Hack -- Flee from the store */
			if (store_num != STORE_HOME)
			{
				/* Message */
				msg_print("Your pack is so full that you flee the store...");

				/* Leave */
				leave_store = TRUE;
			}

			/* Hack -- Flee from the home */
			else if (!store_check_num(o_ptr))
			{
				/* Message */
				msg_print("Your pack is so full that you flee your home...");

				/* Leave */
				leave_store = TRUE;
			}

			/* Hack -- Drop items into the home */
			else
			{
				int item_pos;

				object_type object_type_body;
				object_type *i_ptr = &object_type_body;	/* Get local object */

				char o_name[80];


				/* Give a message */
				msg_print("Your pack overflows!");

				/* Grab a copy of the object */
				*i_ptr = *o_ptr;

				/* Describe it */
				object_desc(o_name, sizeof(o_name), i_ptr, TRUE, ODESC_FULL);

				/* Message */
				msg_format("You drop %s (%c).", o_name, index_to_label(INVEN_PACK));

				/* Remove it from the players inventory */
				inven_item_increase(INVEN_PACK, -255);
				inven_item_describe(INVEN_PACK);
				inven_item_optimize(INVEN_PACK);

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

	/* Leave the store sound */
	sound(MSG_STORE_LEAVE);


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

	/* Redraw entire screen, map */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);
}



/*
 * Shuffle one of the stores.
 */
void store_shuffle(store_indexes which)
{
	/* Ignore home */
	if (which == STORE_HOME) return;

	{
	/* Activate that store */
	store_type* st_ptr = &store[which];

	/* Pick a new owner */
	byte j = st_ptr->owner;
	st_ptr->owner = (byte)rand_int(z_info->b_max-1);
	if (j <= st_ptr->owner) st_ptr->owner += 1;
	}
}


/*
 * Maintain the inventory at the stores.
 */
void store_maint(store_indexes which)
{
	int j;

	int old_rating = rating;


	/* Ignore home */
	if (which == STORE_HOME) return;

	activate_store(which);

	/* unhide everything */
	for (j = st_ptr->stock_num - 1; j >= 0; j--)
		st_ptr->stock[j].ident &= ~(IDENT_HIDE);

	/* Mega-Hack -- prune the black market */
	if (store_num == STORE_B_MARKET)
	{
		/* Destroy crappy black market items */
		for (j = st_ptr->stock_num - 1; j >= 0; j--)
		{
			object_type* const o_ptr = &st_ptr->stock[j];
			if (!o_ptr->k_idx) continue;

			/* Destroy boring items */
			if (black_market_boring(o_ptr))
			{
				/* Destroy the object */
				store_item_increase(j, 0 - o_ptr->number);
				store_item_optimize(j);
			}
		}
	}

	/* Mega-Hack -- do not offer unaffordable items for sale */
	/* Magic shop can get infinite-looped from this for mages, priests have no trouble */
	if (STORE_MAGIC != store_num || 500 <= pc_net_worth)
	{
		for (j = st_ptr->stock_num - 1; j >= 0; j--)
		{
			object_type* const o_ptr = &st_ptr->stock[j];
			if (!o_ptr->k_idx) continue;

			/* Destroy expensive items */
			if (object_value(o_ptr)>pc_net_worth)
			{
				/* Destroy the object */
				store_item_increase(j, 0 - o_ptr->number);
				store_item_optimize(j);
			}
		}
	}

	/* Mega-Hack -- do not offer boring items for sale */
	for (j = st_ptr->stock_num - 1; j >= 0; j--)
	{
		object_type* const o_ptr = &st_ptr->stock[j];
		if (!o_ptr->k_idx) continue;

		/* Destroy boring items */
		if (item_is_boring_to_player(o_ptr))
		{
			/* Destroy the object */
			store_item_increase(j, 0 - o_ptr->number);
			store_item_optimize(j);
		}
	}
	

	j = st_ptr->stock_num;			/* Choose the number of slots to keep */
	j =- randint(STORE_TURNOVER);

	/* ensure store has between STORE_MIN_KEEP and STORE_MAX_KEEP items */
	if (STORE_MAX_KEEP < j) j = STORE_MAX_KEEP;
	if (STORE_MIN_KEEP > j) j = STORE_MIN_KEEP;

	/* Prevent underflow. */
	ZAIBAND_STATIC_ASSERT(0 <= STORE_MIN_KEEP);

	/* Sell items until only "j" slots are left */
	while (st_ptr->stock_num > j) store_delete();

	j = st_ptr->stock_num;			/* Choose the number of slots to fill */
	j += randint(STORE_TURNOVER);

	/* ensure store has between STORE_MIN_KEEP and STORE_MAX_KEEP items */
	if (STORE_MAX_KEEP < j) j = STORE_MAX_KEEP;
	if (STORE_MIN_KEEP > j) j = STORE_MIN_KEEP;

	/* Hack -- prevent "overflow" */
	if (j >= st_ptr->stock_size) j = st_ptr->stock_size - 1;

	/* Buy some new items */
	while (st_ptr->stock_num < j) store_create();


	/* Hack -- Restore the rating */
	rating = old_rating;
}


/*
 * Initialize the stores
 */
void store_init(store_indexes which)
{
	store_type* st_ptr = &store[which];			/* Activate that store */
	st_ptr->owner = (byte)rand_int(z_info->b_max);	/* Pick an owner */
	st_ptr->stock_num = 0;						/* Nothing in stock */
	C_WIPE(st_ptr->stock,st_ptr->stock_size);	/* Clear any old items */
}
