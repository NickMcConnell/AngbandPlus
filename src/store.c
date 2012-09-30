/* File: store.c */

/* Purpose: Store commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
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
 * Successful haggle.
 */
static void say_comment_1(void)
{
	msgf(comment_1[randint0(MAX_COMMENT_1)]);
}


/*
 * Messages for reacting to purchase prices.
 */

#define MAX_COMMENT_7A	4

static cptr comment_7a[MAX_COMMENT_7A] =
{
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!"
};

#define MAX_COMMENT_7B	4

static cptr comment_7b[MAX_COMMENT_7B] =
{
	"Damn!",
	"You fiend!",
	"The shopkeeper curses at you.",
	"The shopkeeper glares at you."
};

#define MAX_COMMENT_7C	4

static cptr comment_7c[MAX_COMMENT_7C] =
{
	"Cool!",
	"You've made my day!",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly."
};

#define MAX_COMMENT_7D	4

static cptr comment_7d[MAX_COMMENT_7D] =
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
		msgf(MSGT_STORE1, comment_7a[randint0(MAX_COMMENT_7A)]);

		chg_virtue(V_HONOUR, -1);
		chg_virtue(V_JUSTICE, -1);

		/* Sound */
		sound(SOUND_STORE1);
	}

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
	{
		/* Comment */
		msgf(MSGT_STORE2, comment_7b[randint0(MAX_COMMENT_7B)]);

		chg_virtue(V_JUSTICE, -1);
		if (one_in_(4)) chg_virtue(V_HONOUR, -1);

		/* Sound */
		sound(SOUND_STORE2);
	}

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (value < (4 * guess)) && (price < value))
	{
		/* Comment */
		msgf(MSGT_STORE3, comment_7c[randint0(MAX_COMMENT_7C)]);

		if (one_in_(4))
			chg_virtue(V_HONOUR, -1);
		else if (one_in_(4))
			chg_virtue(V_HONOUR, 1);

		/* Sound */
		sound(SOUND_STORE3);
	}

	/* Item was a great bargain, and we got away with it */
	else if ((value > guess) && (price < value))
	{
		/* Comment */
		msgf(MSGT_STORE4, comment_7d[randint0(MAX_COMMENT_7D)]);

		if (one_in_(2))
			chg_virtue(V_HONOUR, -1);
		if (one_in_(4))
			chg_virtue(V_HONOUR, 1);

		if (10 * price < value)
			chg_virtue(V_SACRIFICE, 1);

		/* Sound */
		sound(SOUND_STORE4);
	}
}


/*
 * We store the current "store pointer" here so everyone can access it
 */
static store_type *st_ptr = NULL;

/*
 * We store the current field here so that it can be accessed everywhere
 */
static const field_type *f_ptr = NULL;

/* Save info flags for store */
static byte info_flags;



/*
 * Determine the price of an item (qty one) in a store.
 *
 * This function takes into account the player's charisma, and the
 * shop-keepers friendliness, and the shop-keeper's base greed, but
 * never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * item, and should be less than 100 when the player is "selling" it.
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
s32b price_item(object_type *o_ptr, bool flip)
{
	int factor;
	int adjust;
	s32b price;

	int greed = st_ptr->greed;

	/* Get the value of one of the items */
	price = object_value(o_ptr);

	/* Worthless items */
	if (price <= 0) return (0L);


	/* The charisma factor */
	factor = adj_chr_gold[p_ptr->stat[A_CHR].ind];


	/* Shop is buying */
	if (flip)
	{
		/* Adjust for greed */
		adjust = 100 + (200 - (greed + factor));

		/* Never get "silly" */
		if (adjust > 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if ((info_flags & ST_GREED) || (info_flags & ST_ULTRA_GREED))
		{
			price = price / 2;
        }
	}

	/* Shop is selling */
	else
	{
		/* Adjust for greed */
		adjust = 100 + ((greed + factor) - 200);

		/* Never get "silly" */
		if (adjust < 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (info_flags & ST_GREED)
			price = price * 2;

		if (info_flags & ST_ULTRA_GREED)
			price = price * 4;
	}

	/* Compute the final price (with rounding) */
	price = (price * adjust + 50L) / 100L;

    /* Cap the price */
    if (flip && price > st_ptr->max_cost * 100L)
    {
        price = st_ptr->max_cost * 100L;
    }

	/* Note -- Never become "free" */
	if (price <= 0L) return (1L);

	/* Hack - save price for object list code */
	o_ptr->temp_cost = price;

	/* Return the price */
	return (price);
}


/*
 * Certain "cheap" objects should be created in "piles"
 * Some objects can be sold at a "discount" (in small piles)
 */
static void mass_produce(object_type *o_ptr)
{
	int size = 1;
	int discount = 0;

	s32b cost = object_value(o_ptr);

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		case TV_FOOD:
		case TV_FLASK:
		case TV_LITE:
		{
			/* Food, Flasks, and Lites */
			if (cost <= 5L) size += damroll(3, 5);
			if (cost <= 20L) size += damroll(3, 5);
			break;
		}

		case TV_POTION:
		case TV_SCROLL:
		{
			if (cost <= 60L) size += damroll(3, 5);
			if (cost <= 240L) size += damroll(1, 5);
			break;
		}

		case TV_LIFE_BOOK:
		case TV_SORCERY_BOOK:
		case TV_NATURE_BOOK:
		case TV_CHAOS_BOOK:
		case TV_DEATH_BOOK:
		case TV_TRUMP_BOOK:
		case TV_ARCANE_BOOK:
		{
			if (cost <= 50L) size += damroll(2, 3);
			if (cost <= 500L) size += damroll(1, 3);
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
			if (o_ptr->xtra_name) break;
			if (cost <= 10L) size += damroll(3, 5);
			if (cost <= 100L) size += damroll(3, 5);
			break;
		}

		case TV_SPIKE:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			if (cost <= 5L) size += damroll(5, 5);
			if (cost <= 50L) size += damroll(5, 5);
			if (cost <= 500L) size += damroll(5, 5);
			break;
		}

		case TV_FIGURINE:
		case TV_STATUE:
		{
			if (cost <= 100L) size += damroll(2, 2);
			if (cost <= 1000L) size += damroll(2, 2);
			break;
		}

		case TV_ROD:
		case TV_WAND:
		case TV_STAFF:
		{
			if (one_in_(3))
			{
				if (cost < 1601L) size += damroll(1, 5);
				else if (cost < 3201L) size += damroll(1, 3);
			}

			/* 
			 * Ensure that mass-produced rods and wands
			 * get the correct pvals.
			 */
			if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
			{
				o_ptr->pval *= size;
			}
			break;
		}
	}


	/* Pick a discount */
	if (cost < 5)
	{
		discount = 0;
	}
	else if (one_in_(25))
	{
		discount = 25;
	}
	else if (one_in_(150))
	{
		discount = 50;
	}
	else if (one_in_(300))
	{
		discount = 75;
	}
	else if (one_in_(500))
	{
		discount = 90;
	}


	if (o_ptr->xtra_name)
	{
		if (cheat_peek && discount)
		{
			msgf("No discount on powerful items.");
		}
		discount = 0;
	}

	/* Save the discount */
	o_ptr->discount = discount;

	/* Save the total pile size */
	o_ptr->number = size - (size * discount / 100);
}



/*
 * Determine if a store item can "absorb" another item
 *
 * See "object_similar()" for the same function for the "player"
 */
static bool store_object_similar(const object_type *o_ptr,
                                 const object_type *j_ptr)
{
	/* Hack -- Identical items cannot be stacked */
	if (o_ptr == j_ptr) return (FALSE);

	/* Different objects cannot be stacked */
	if (o_ptr->k_idx != j_ptr->k_idx) return (FALSE);

	/* Different charges (etc) cannot be stacked, unless wands or rods. */
	if ((o_ptr->pval != j_ptr->pval) &&
		(o_ptr->tval != TV_WAND) && (o_ptr->tval != TV_ROD)) return (FALSE);

	/* Require many identical values */
	if (o_ptr->to_h != j_ptr->to_h) return (FALSE);
	if (o_ptr->to_d != j_ptr->to_d) return (FALSE);
	if (o_ptr->to_a != j_ptr->to_a) return (FALSE);

	/* Artifacts and ego items don't stack ! */
	if (o_ptr->xtra_name || j_ptr->xtra_name) return (FALSE);

	/* Hack -- Identical flags! */
	if ((o_ptr->flags[0] != j_ptr->flags[0]) ||
		(o_ptr->flags[1] != j_ptr->flags[1]) || 
		(o_ptr->flags[2] != j_ptr->flags[2]) ||
		(o_ptr->flags[3] != j_ptr->flags[3]))
		return (FALSE);

	/* Require identical recharge times / fuel level */
	if (o_ptr->timeout != j_ptr->timeout) return (FALSE);

	/* Require many identical values */
	if (o_ptr->ac != j_ptr->ac) return (FALSE);
	if (o_ptr->dd != j_ptr->dd) return (FALSE);
	if (o_ptr->ds != j_ptr->ds) return (FALSE);

	/* Hack -- Never stack chests */
	if (o_ptr->tval == TV_CHEST) return (FALSE);

	/* Require matching discounts */
	if (o_ptr->discount != j_ptr->discount) return (FALSE);

	/* They match, so they must be similar */
	return (TRUE);
}


/*
 * Allow a store item to absorb another item
 */
static void store_object_absorb(object_type *o_ptr, const object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Combine quantity, lose excess items */
	o_ptr->number = (total > 99) ? 99 : total;

	/* 
	 * Hack -- if rods are stacking, add the pvals
	 * (maximum timeouts) together. -LM-
	 */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval += j_ptr->pval;
	}

	/* Hack -- if wands are stacking, combine the charges. -LM- */
	if (o_ptr->tval == TV_WAND)
	{
		o_ptr->pval += j_ptr->pval;

		/* No "used" charges in store stock */
		o_ptr->ac = 0;
	}
}


/*
 * Check to see if the shop will be carrying too many objects
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.	Before, one could "nuke" potions this way.
 */
static bool store_check_num(const object_type *o_ptr)
{
	object_type *j_ptr;

	/* Free space is always usable */
	if (get_list_length(st_ptr->stock) < st_ptr->max_stock) return TRUE;

	/* The "home" acts like the player */
	if (st_ptr->type == BUILD_STORE_HOME)
	{
		/* Check all the items */
		OBJ_ITT_START (st_ptr->stock, j_ptr)
		{
			/* Can the new object be combined with the old one? */
			if (object_similar(j_ptr, o_ptr)) return (TRUE);
		}
		OBJ_ITT_END;
	}

	/* Normal stores do special stuff */
	else
	{
		/* Check all the items */
		OBJ_ITT_START (st_ptr->stock, j_ptr)
		{
			/* Can the new object be combined with the old one? */
			if (store_object_similar(j_ptr, o_ptr)) return (TRUE);
		}
		OBJ_ITT_END;
	}

	/* But there was no room at the inn... */
	return (FALSE);
}


/*
 * Determine if the current store will purchase the given item
 * (Check restriction flags, and object theme)
 */
static bool store_will_buy(const object_type *o_ptr)
{
	obj_theme theme;

	/* Check restriction flags */

	/* Blessed items only */
	if (info_flags & ST_REST_BLESSED)
	{
		if (!item_tester_hook_is_blessed(o_ptr)) return (FALSE);
	}

	/* Good items only */
	if ((info_flags & ST_REST_GOOD) || (info_flags & ST_REST_GREAT))
	{
		if (!item_tester_hook_is_good(o_ptr)) return (FALSE);
	}

	/* Ignore "worthless" items XXX XXX XXX */
	if (object_value(o_ptr) <= 0) return (FALSE);

	/* Final stage, check the theme */

	/* Set theme */
	theme.treasure = f_ptr->data[3];
	theme.combat = f_ptr->data[4];
	theme.magic = f_ptr->data[5];
	theme.tools = f_ptr->data[6];

	/* Initialise the theme tester */
	init_match_theme(theme);

	/*
	 * Final check: 
	 * Does the object have a chance of being made?
	 */
	return (kind_is_theme(o_ptr->k_idx));
}


/*
 * The player wants to sell something to the store.
 * This is a much more restrictive case than the store_will_buy()
 * function below.
 * Only objects that pass the field hooks will be accepted.
 * (As well as only selecting store_will_buy() objects.
 *
 * Two field action functions are called for a store.
 * The first checks to see if a store will not buy something.
 * The second checks the opposite.
 * By combining different action functions, lots of different
 * types of store can be made.
 */
static bool store_will_stock(const object_type *o_ptr)
{
	/* Default is to reject this rejection */
	bool result = FALSE;

	/* Will the store !not! buy this item? */
	field_script_const(f_ptr, FIELD_ACT_STORE_ACT1, "p:b", LUA_OBJECT(o_ptr), LUA_RETURN(result));

	/* We don't want this item type? */
	if (result == TRUE) return (FALSE);

	/* Change the default to acceptance */
	result = TRUE;

	/* Will the store buy this item? */
	field_script_const(f_ptr, FIELD_ACT_STORE_ACT2, "p:b", LUA_OBJECT(o_ptr), LUA_RETURN(result));

	/* Finally check to see if we will buy the item */
	return (result && store_will_buy(o_ptr));
}


/*
 * Compare two items to see if they are in store-order.
 */
static bool reorder_store_comp(const object_type *o1_ptr,
                               const object_type *o2_ptr)
{
	/* Hack -- readable books always come first */
	if ((o1_ptr->tval == mp_ptr->spell_book) &&
		(o2_ptr->tval != mp_ptr->spell_book)) return (TRUE);
	if ((o1_ptr->tval == mp_ptr->spell_book) &&
		(o2_ptr->tval != mp_ptr->spell_book)) return (FALSE);

	/* Objects sort by decreasing type */
	if (o1_ptr->tval > o2_ptr->tval) return (TRUE);
	if (o1_ptr->tval < o2_ptr->tval) return (FALSE);

	/* Can happen in the home */
	if (!object_aware_p(o2_ptr)) return (TRUE);
	if (!object_aware_p(o1_ptr)) return (FALSE);

	/* Objects sort by increasing sval */
	if (o1_ptr->sval < o2_ptr->sval) return (TRUE);
	if (o1_ptr->sval > o2_ptr->sval) return (FALSE);

	/* Objects in the home can be unknown */
	if (!object_known_p(o2_ptr)) return (TRUE);
	if (!object_known_p(o1_ptr)) return (FALSE);

	/* Objects sort by decreasing value */
	if (object_value(o1_ptr) > object_value(o2_ptr)) return (TRUE);

	return (FALSE);
}


/*
 * Add the item "o_ptr" to the inventory of the "Home"
 *
 * In all cases, return the slot (or -1) where the object was placed
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" becoming
 * known, the player may have to pick stuff up and drop it again.
 */
static object_type *home_carry(object_type *o_ptr)
{
	object_type *j_ptr;

	/* Check each existing item (try to combine) */
	OBJ_ITT_START (st_ptr->stock, j_ptr)
	{
		/* The home acts just like the player */
		if (object_similar(j_ptr, o_ptr))
		{
			/* Save the new number of items */
			object_absorb(j_ptr, o_ptr);

			/* All done */
			return (j_ptr);
		}
	}
	OBJ_ITT_END;

	/* No space? */
	if (get_list_length(st_ptr->stock) >= st_ptr->max_stock) return (NULL);

	/* Add the item to the store */
	o_ptr = add_object_list(&st_ptr->stock, o_ptr);

	/* Paranoia */
	if (!o_ptr) return (NULL);

	/* Forget location */
	o_ptr->iy = o_ptr->ix = 0;

	/* Forget Region */
	o_ptr->region = 0;

	/* No longer marked */
	o_ptr->info &= ~(OB_SEEN);

	/* Reorder the items */
	o_ptr = reorder_objects_aux(o_ptr, reorder_store_comp, st_ptr->stock);

	chg_virtue(V_SACRIFICE, -1);

	/* Return the location */
	return (o_ptr);
}


/*
 * Add the item "o_ptr" to a real stores inventory.
 *
 * If the item is "worthless", it is thrown away (except in the home).
 *
 * If the item cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.	Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an item to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or NULL) where the object was placed
 */
static object_type *store_carry(object_type *o_ptr)
{
	object_type *j_ptr;

	/* Evaluate the object */
	s32b value = object_value(o_ptr);

	/* Cursed/Worthless items "disappear" when sold */
	if (value <= 0) return (NULL);

	/* Identify it fully */
	object_known(o_ptr);
	object_mental(o_ptr);

	/* Save all the known flags */
	o_ptr->kn_flags[0] = o_ptr->flags[0];
	o_ptr->kn_flags[1] = o_ptr->flags[1];
	o_ptr->kn_flags[2] = o_ptr->flags[2];
	o_ptr->kn_flags[3] = o_ptr->flags[3];

    /* Erase the inscription */
    quark_remove(&o_ptr->inscription);

	/* Erase the "feeling" */
	o_ptr->feeling = FEEL_NONE;

	/* Check each existing item (try to combine) */
	OBJ_ITT_START (st_ptr->stock, j_ptr)
	{
		/* Can the existing items be incremented? */
		if (store_object_similar(j_ptr, o_ptr))
		{
			/* Hack -- extra items disappear */
			store_object_absorb(j_ptr, o_ptr);

			/* All done */
			return (j_ptr);
		}
	}
	OBJ_ITT_END;

	/* No space? */
	if (get_list_length(st_ptr->stock) >= st_ptr->max_stock) return (NULL);

	/* Add the item to the store */
	o_ptr = add_object_list(&st_ptr->stock, o_ptr);

	/* Paranoia */
	if (!o_ptr) return (NULL);

	/* Forget location */
	o_ptr->iy = o_ptr->ix = 0;

	/* Forget Region */
	o_ptr->region = 0;

	/* No longer marked */
	o_ptr->info &= ~(OB_SEEN);

	/* Reorder the items */
	o_ptr = reorder_objects_aux(o_ptr, reorder_store_comp, st_ptr->stock);

	/* Return the location */
	return (o_ptr);
}


/*
 * Attempt to delete (some of) a random item from the store
 * Hack -- we attempt to "maintain" piles of items when possible.
 */
static void store_delete(void)
{
	int what, num;
	object_type *o_ptr;

	/* Pick a random slot */
	what = randint0(get_list_length(st_ptr->stock));

	/* Get the item */
	o_ptr = get_list_item(st_ptr->stock, what);

	/* Determine how many items are here */
	num = o_ptr->number;

	/* Hack -- sometimes, only destroy half the items */
	if (one_in_(2)) num = (num + 1) / 2;

	/* Hack -- sometimes, only destroy a single item */
	if (one_in_(2)) num = 1;

	/* 
	 * Hack -- decrement the maximum timeouts and
	 * total charges of rods and wands. -LM-
	 */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
	{
		o_ptr->pval -= num * o_ptr->pval / o_ptr->number;
	}

	/* Actually destroy (part of) the item */
	item_increase_silent(o_ptr, -num);
}


/*
 * Creates a random item and gives it to a store
 */
static void store_create(void)
{
    int kind, tries, level, delta;
    byte flags;

	object_type *q_ptr;

	obj_theme theme;

	byte restricted = f_ptr->data[7];

	/* Paranoia -- no room left */
	if (get_list_length(st_ptr->stock) >= st_ptr->max_stock) return;

	/* Set theme */
	theme.treasure = f_ptr->data[3];
	theme.combat = f_ptr->data[4];
	theme.magic = f_ptr->data[5];
	theme.tools = f_ptr->data[6];

	/* Select items based on "theme" */
	init_match_theme(theme);

	/* Prepare allocation table */
	get_obj_num_prep(kind_is_theme);

	/* Hack -- consider up to fifty items */
	for (tries = 0; tries < 50; tries++)
    {
        /* Assume no flags */
        flags = OC_NORMAL;

        /* Assume no bonus */
        delta = 0;

		/* Get level to use */
		level = rand_range(f_ptr->data[1], f_ptr->data[2]);

		/* Get an item */
		kind = get_obj_num(level, 0);

		/* Handle failure */
		if (!kind) continue;

		/* Create a new object of the chosen kind */
        q_ptr = object_prep(kind);

		/* Create object based on restrictions */
		if (restricted & ST_REST_GREAT)
        {
            /* Apply "great" magic */
            delta = 30;
            flags = OC_FORCE_GOOD;
		}
		else if (restricted & ST_REST_GOOD)
        {
            /* Apply "good" magic */
            delta = 15;
        }

        /* Occasionally generate unusually good items */
        while (one_in_(30))
        {
            delta += rand_range(5, 15);
        }

        /* Apply some magic */
        apply_magic(q_ptr, level, delta, flags);

		/* Mega-Hack -- no chests in stores */
		if (q_ptr->tval == TV_CHEST) continue;

		/* The item is "known" */
		object_known(q_ptr);

		/* Mark it storebought */
		q_ptr->info |= OB_STOREB;
		q_ptr->info |= OB_NO_EXP;

		/* Require valid object */
		if (!store_will_stock(q_ptr)) continue;

		/* Hack -- Charge lite's */
		if (q_ptr->tval == TV_LITE)
		{
			if (q_ptr->sval == SV_LITE_TORCH) q_ptr->timeout = FUEL_TORCH / 2;
			if (q_ptr->sval == SV_LITE_LANTERN) q_ptr->timeout = FUEL_LAMP / 2;
		}

		/* Mass produce and/or Apply discount */
		mass_produce(q_ptr);

		/* Attempt to carry the (known) item */
		(void)store_carry(q_ptr);

		/* Definitely done */
		break;
	}
}

/*
 * Re-displays a single store entry
 */
static void display_entry(int pos)
{
	int i;

	/* Get the object */
	object_type *o_ptr = get_list_item(st_ptr->stock, pos);

	s32b x;

	byte a;
	char c;

	int maxwid;

	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Get the "offset" */
	i = (pos % 12);

	/* Label it, clear the line --(-- */
	prtf(0, i + 6, "%c) ", I2A(i));

	/* Show_store_graph perm on. */
	a = object_attr(o_ptr);
	c = object_char(o_ptr);

	/* Hack -- fake monochrome */
	if (!use_color)
    {
    	a = TERM_WHITE;
    	c = ' ';
    }
    
    if (object_aware_p(o_ptr))
        Term_draw(3, i + 6, a, c);

	/* Describe an item in the home */
	if (st_ptr->type == BUILD_STORE_HOME)
	{
		maxwid = wid - 4;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 10;

		/* Describe the object */
		put_fstr(5, i + 6, "%s" CLR_SET_DEFAULT "%v",
					color_seq[tval_to_attr[o_ptr->tval]],
					OBJECT_FMT(o_ptr, TRUE, 3));

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of an individual item */
			int wgt = o_ptr->weight;
			put_fstr(wid - 12, i + 6, "%3d.%d lb", wgt / 10, wgt % 10);
		}
	}

	/* Describe an item (fully) in a store */
	else
	{
		/* Must leave room for the "price" */
		maxwid = wid - 14;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 7;

		/* Describe the object (fully) */
		put_fstr(5, i + 6, "%s" CLR_SET_DEFAULT "%v",
					color_seq[tval_to_attr[o_ptr->tval]],
					OBJECT_STORE_FMT(o_ptr, TRUE, 3));

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of an individual item */
			int wgt = o_ptr->weight;
			put_fstr(wid - 19, i + 6, "%3d.%d", wgt / 10, wgt % 10);
		}

		/* Extract the "minimum" price */
		x = price_item(o_ptr, FALSE);

		/* Actually draw the price */
		put_fstr(wid - 12, i + 6, "%9ld  ", (long)x);
	}
}


/*
 * Displays a store's inventory
 * All prices are listed as "per individual object".  -BEN-
 */
static void display_inventory(void)
{
	int k;

	int stocknum = get_list_length(st_ptr->stock);

	/* Display the next 12 items */
	for (k = 0; k < 12; k++)
	{
		/* Do not display "dead" items */
		if (p_ptr->state.store_top + k >= stocknum) break;

		/* Display that line */
		display_entry(p_ptr->state.store_top + k);
	}

	/* Erase the extra lines and the "more" prompt */
	clear_region(0, k + 6, 18);

	/* Assume "no current page" */
	put_fstr(20, 5, "        ");

	/* Visual reminder of "more items" */
	if (stocknum > 12)
	{
		/* Show "more" reminder (after the last item) */
		prtf(3, k + 6, "-more-");

		/* Indicate the "current page" */
		put_fstr(20, 5, "(Page %d)", p_ptr->state.store_top / 12 + 1);
	}
}


/*
 * Displays players gold					-RAK-
 */
static void store_prt_gold(void)
{
	prtf(53, 19, "Gold Remaining: ");
	prtf(68, 19, "%9ld", (long)p_ptr->au);
}


/*
 * Displays store (after clearing screen)		-RAK-
 */
static void display_store(void)
{
	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Clear screen */
	Term_clear();

	/* The "Home" is special */
	if (st_ptr->type == BUILD_STORE_HOME)
	{
		/* Put the owner name */
		put_fstr(30, 3, "Your Home");

		/* Label the item descriptions */
		put_fstr(3, 5, "Item Description");

		/* If showing weights, show label */
		if (show_weights)
		{
			put_fstr(70, 5, "Weight");
		}
	}

	/* Normal stores */
	else
	{
		cptr store_name = field_name(f_ptr);
		cptr owner_name = quark_str(st_ptr->owner_name);

		/* Put the owner name */
		put_fstr(5, 3, "%s", owner_name);

		/* Show the max price in the store (above prices) */
		put_fstr(45, 3, "%s (%ld)", store_name, (long)(st_ptr->max_cost) * 100);

		/* Label the item descriptions */
		put_fstr(3, 5, "Item Description");

		/* If showing weights, show label */
		if (show_weights)
		{
			put_fstr(wid - 20, 5, "Weight");
		}

		/* Label the asking price (in stores) */
		put_fstr(wid - 8, 5, "Price");
	}

	/* Display the current gold */
	store_prt_gold();

	/* Draw in the inventory */
	display_inventory();
	
	/* Basic commands */
	prtf(0, 22, " ESC) Exit from Building.");

	/* Browse if necessary */
	if (get_list_length(st_ptr->stock) > 12)
	{
		prtf(0, 23, " SPACE) Next page of stock");
	}

	/* Home commands */
	if (st_ptr->type == BUILD_STORE_HOME)
	{
		prtf(31, 22, " g) Get an item.");
		prtf(31, 23, " d) Drop an item.");
	}

	/* Shop commands XXX XXX XXX */
	else
	{
		prtf(31, 22, " p) Purchase an item.");
		prtf(31, 23, " s) Sell an item.");
	}

	/* Add in the eXamine option */
	prtf(56, 22, " x) eXamine an item.");

	/* Prompt */
	prtf(0, 21, "You may: ");

	/* Refresh */
	Term_fresh();
}


/*
 * Maintain the inventory at the stores.
 */
static void store_maint(void)
{
	int i = 0, j;

	/* Ignore home + locker */
	if (st_ptr->type == BUILD_STORE_HOME) return;

	/* Choose the number of slots to keep */
	j = get_list_length(st_ptr->stock);

	/* Sell a few items */
	j = j - randint1(STORE_TURNOVER);

	if (st_ptr->max_stock == STORE_INVEN_MAX)
	{
		/* Never keep more than "STORE_MAX_KEEP" slots */
		if (j > STORE_MAX_KEEP1) j = STORE_MAX_KEEP1;

		/* Always "keep" at least "STORE_MIN_KEEP" items */
		if (j < STORE_MIN_KEEP1) j = STORE_MIN_KEEP1;
	}
	else
	{
		/* The store has half the normal inventory space */

		/* Never keep more than "STORE_MAX_KEEP" slots */
		if (j > STORE_MAX_KEEP2) j = STORE_MAX_KEEP2;

		/* Always "keep" at least "STORE_MIN_KEEP" items */
		if (j < STORE_MIN_KEEP2) j = STORE_MIN_KEEP2;
	}

	/* Hack -- prevent "underflow" (This should never happen anyway.) */
	if (j < 0) j = 0;

	/* Destroy objects until only "j" slots are left */
	while (get_list_length(st_ptr->stock) > j) store_delete();


	/* Choose the number of slots to fill */
	j = get_list_length(st_ptr->stock);

	/* Buy some more items */
	j = j + randint1(STORE_TURNOVER);

	if (st_ptr->max_stock == STORE_INVEN_MAX)
	{
		/* Never keep more than "STORE_MAX_KEEP" slots */
		if (j > STORE_MAX_KEEP1) j = STORE_MAX_KEEP1;

		/* Always "keep" at least "STORE_MIN_KEEP" items */
		if (j < STORE_MIN_KEEP1) j = STORE_MIN_KEEP1;
	}
	else
	{
		/* The store has half the normal inventory space */

		/* Never keep more than "STORE_MAX_KEEP" slots */
		if (j > STORE_MAX_KEEP2) j = STORE_MAX_KEEP2;

		/* Always "keep" at least "STORE_MIN_KEEP" items */
		if (j < STORE_MIN_KEEP2) j = STORE_MIN_KEEP2;
	}

	/* Hack -- prevent "overflow" (This shouldn't do anything) */
	if (j >= st_ptr->max_stock) j = st_ptr->max_stock - 1;

	/* Acquire some new items */
	while ((get_list_length(st_ptr->stock) < j) && (i < 30))
	{
		/* Increment counter so we avoid taking too long */
		i++;

		/* Try to allocate some items */
		store_create();
	}
}


/*
 * Shuffle one of the stores.
 */
static void store_shuffle(store_type *st_ptr)
{
	cptr own_name = owner_names[randint0(owner_names_max)];
	cptr own_suffix = owner_suffix[randint0(owner_suffix_max)];

	object_type *o_ptr;

	/* Ignore home + locker */
	if (st_ptr->type == BUILD_STORE_HOME) return;

	/* Pick a new owner */
	st_ptr->owner_name = quark_fmt("%s %s", own_name, own_suffix);
	
	/* These are set in place_sb() via the lua hook below */
	st_ptr->greed = 0;
	st_ptr->max_cost = 0;
		
	/*
	 * Hack - Init store
	 *
	 * Note that this assumes the player is in this store
	 */
	field_script_const(f_ptr, FIELD_ACT_SB_INIT, "");

	/* Reset the owner data */
	st_ptr->data = 0;

	/* Hack -- discount all the items */
	OBJ_ITT_START (st_ptr->stock, o_ptr)
	{
		/* Hack -- Sell all old items for "half price" */
		if (!(o_ptr->xtra_name))
		{
			o_ptr->discount = 50;
		}

		/* Mega-Hack -- Note that the item is "on sale" */
		o_ptr->inscription = quark_add("on sale");
	}
	OBJ_ITT_END;
}


/*
 * Get the ID of a store item and return its value
 */
static int get_stock(int *com_val, cptr pmt, int maxobj)
{
	char command;

	char out_val[160];

	/* Get the item index */
	if (repeat_pull(com_val))
	{
		/* Verify the item */
		if ((*com_val >= 0) && (*com_val < maxobj))
		{
			/* Success */
			return (TRUE);
		}
	}
	else
	{
		/* Invalid repeat - reset it */
		repeat_clear();
	}

	/* Paranoia XXX XXX XXX */
	message_flush();


	/* Assume failure */
	*com_val = (-1);

	/* Build the prompt */
	strnfmt(out_val, 160, "(Items a-%c, ESC to exit) %s",
				  I2A(maxobj - 1), pmt);

	/* Ask until done */
	while (TRUE)
	{
		int k;

		/* Escape */
		if (!get_com(out_val, &command)) break;

		/* Convert */
		k = (islower(command) ? A2I(command) : -1);

		/* Legal responses */
		if ((k >= 0) && (k < maxobj))
		{
			*com_val = k;
			break;
		}

		/* Oops */
		bell("Illegal store object choice!");
	}

	/* Clear the prompt */
	clear_msg();

	/* Cancel */
	if (command == ESCAPE) return (FALSE);

	repeat_push(*com_val);

	/* Success */
	return (TRUE);
}

static bool store_access_item(const object_type *o_ptr, s32b price, bool buy)
{
	if (buy)
	{
		/* Describe the object (fully) */
		put_fstr(0, 1, "%s %v", (buy) ? "Buying" : "Selling",
					OBJECT_STORE_FMT(o_ptr, TRUE, 3));
	}
	else
	{
		/* Describe the object (only what we know) */
		put_fstr(0, 1, "%s %v", (buy) ? "Buying" : "Selling",
					OBJECT_FMT(o_ptr, TRUE, 3));
	}

	put_fstr(0, 2, "Offer :  %ld", (long)price);

	/* Ask the user for a response */
	if (check_transaction && !get_check_ext(TRUE, FALSE, buy ? "Buy? ": "Sell? "))
	{
		return (FALSE);
	}

	/* Chose to make transaction */
	return (TRUE);
}

/*
 * Buy an item from a store
 */
static void store_purchase(void)
{
	int i, amt;
	int item, item_new;

	s32b price, best;

	object_type *j_ptr;

	object_type *o_ptr;

	char out_val[160];

	/* Empty? */
	if (!st_ptr->stock)
	{
		if (st_ptr->type == BUILD_STORE_HOME)
			msgf("Your home is empty.");
		else
			msgf("I am currently out of stock.");
		return;
	}

	/* Find the number of objects on this and following pages */
	i = (get_list_length(st_ptr->stock) - p_ptr->state.store_top);

	/* And then restrict it to the current page */
	if (i > 12) i = 12;

	/* Prompt */
	if (st_ptr->type == BUILD_STORE_HOME)
	{
		strnfmt(out_val, 160, "Which item do you want to take? ");
	}
	else
	{
		strnfmt(out_val, 160, "Which item are you interested in? ");
	}

	/* Get the item number to be bought */
	if (!get_stock(&item, out_val, i)) return;

	/* Get the actual index */
	item = item + p_ptr->state.store_top;

	/* Get the actual item */
	o_ptr = get_list_item(st_ptr->stock, item);

	/* Assume the player wants just one of them */
	amt = 1;

	/* Get a duplicate of the object */
	j_ptr = object_dup(o_ptr);

	/* Recalculate charges for a single wand/rod */
	reduce_charges(j_ptr, j_ptr->number - 1);

	/* Modify quantity */
	j_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(j_ptr))
	{
		msgf("You cannot carry that many different items.");
		return;
	}

	/* Determine the "best" price (per item) */
	best = price_item(j_ptr, FALSE);
	
	/*
	 * Paranoia - you can only buy one weapon / armour item at a time
	 *
	 * This prevents the player getting stacks of weapons etc. in
	 * his pack.  I suppose we could make an extension to
	 * inven_carry_okay() to do this properly.
	 */
	if ((j_ptr->tval < TV_BOW) || (j_ptr->tval > TV_DRAG_ARMOR))
	{
		/* Find out how many the player wants */
		if (o_ptr->number > 1)
		{
			/* Describe the object (fully) */
			put_fstr(0, 1, "%s %v", "Buying",
						OBJECT_STORE_FMT(o_ptr, TRUE, 3));

			/* Get a quantity */
			amt = get_quantity(NULL, o_ptr->number);

			/* Allow user abort */
			if (amt <= 0) return;
		}
	}

	/* Get desired object */
	j_ptr = object_dup(o_ptr);

	/*
	 * If a rod or wand, allocate total maximum timeouts or charges
	 * between those purchased and left on the shelf.
	 */
	reduce_charges(j_ptr, j_ptr->number - amt);

	/* Modify quantity */
	j_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(j_ptr))
	{
		msgf("You cannot carry that many items.");
		return;
	}

	/* Attempt to buy it */
	if (!(st_ptr->type == BUILD_STORE_HOME))
	{
		/* Get price */
		price = price_item(j_ptr, FALSE) * amt;

		/* Player can afford it */
		if (p_ptr->au < price)
		{
			/* Simple message (no insult) */
			msgf("You do not have enough gold.");

		}

		/* Player wants it? */
		else if (store_access_item(j_ptr, price, TRUE))
		{
			/* Say "okay" */
			say_comment_1();

			/* Make a sound */
			sound(SOUND_BUY);

			/* Spend the money */
			p_ptr->au -= price;

			/* Update the display */
			store_prt_gold();

			/* Hack -- buying an item makes you aware of it */
			object_aware(j_ptr);
			object_mental(j_ptr);
			j_ptr->info &= ~(OB_STOREB);

			/* Describe the transaction */
			msgf("You bought %v for %ld gold.",
				 OBJECT_FMT(j_ptr, TRUE, 3), (long)price);
			
			/* Now, reduce the original stack's pval. */
			if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
			{
				o_ptr->pval -= j_ptr->pval;

				/* No used charges in store stock */
				o_ptr->ac = 0;
			}

            /* Erase the inscription */
            quark_remove(&j_ptr->inscription);

			/* Erase the "feeling" */
			j_ptr->feeling = FEEL_NONE;
			
			/* Give it to the player */
			j_ptr = inven_carry(j_ptr);
			
			/* Paranoia */
			if (!j_ptr)
			{
				msgf("Too many allocated objects!");
				return;
			}

			/* Get slot */
			item_new = get_item_position(p_ptr->inventory, j_ptr);

			/* Describe the final result */
			msgf("You have %v (%c).", OBJECT_FMT(j_ptr, TRUE, 3), I2A(item_new));

			/* Handle stuff */
			handle_stuff();

			/* Note how many slots the store used to have */
			i = get_list_length(st_ptr->stock);

			/* Remove the bought items from the store */
			item_increase_silent(o_ptr, -amt);

			/* Store is empty */
			if (!st_ptr->stock)
			{
				/* Shuffle */
				if (one_in_(STORE_SHUFFLE))
				{
					/* Message */
					msgf("The shopkeeper retires.");

					/* Shuffle the store */
					store_shuffle(st_ptr);
				}

				/* Maintain */
				else
				{
					/* Message */
					msgf("The shopkeeper brings out some new stock.");
				}

				/* New inventory */
				for (i = 0; i < 10; i++)
				{
					/* Maintain the store */
					store_maint();
				}

				/* Start over */
				p_ptr->state.store_top = 0;
			}

			/* The item is gone */
			else if (get_list_length(st_ptr->stock) != i)
			{
				/* Pick the correct screen */
				if (p_ptr->state.store_top >= get_list_length(st_ptr->stock))
				{
					p_ptr->state.store_top -= 12;
				}
			}
			
			/* Redraw everything */
			display_inventory();
		}
	}

	/* Home is much easier */
	else
	{
		/* Distribute charges of wands/rods */
		distribute_charges(o_ptr, j_ptr, amt);

		/* Give it to the player */
		j_ptr = inven_carry(j_ptr);

		/* Paranoia */
		if (!j_ptr)
		{
			msgf("Too many allocated objects!");
			return;
		}

		/* Get slot */
		item_new = get_item_position(p_ptr->inventory, j_ptr);

		/* Describe just the result */
		msgf("You have %v (%c).", OBJECT_FMT(j_ptr, TRUE, 3), I2A(item_new));

		/* Handle stuff */
		handle_stuff();

		/* Take note if we take the last one */
		i = get_list_length(st_ptr->stock);

		/* Remove the items from the home */
		item_increase(o_ptr, -amt);

		/* Hack -- Item is still here */
		if (i == get_list_length(st_ptr->stock))
		{
			/* Redraw the item */
			display_entry(item);
		}

		/* The item is gone */
		else
		{
			/* Nothing left */
			if (!st_ptr->stock) p_ptr->state.store_top = 0;

			/* Nothing left on that screen */
			else if (p_ptr->state.store_top >= get_list_length(st_ptr->stock))
			{
				p_ptr->state.store_top -= 12;
			}

			/* Redraw everything */
			display_inventory();

			chg_virtue(V_SACRIFICE, 1);
		}
	}
}


/*
 * Sell an item to the store (or home)
 */
static void store_sell(void)
{
	int item_pos;
	int amt;

	s32b price, value, dummy;

	object_type *q_ptr;

	object_type *o_ptr;

	cptr q, s;
	
	s16b *list;

	/* Get an item */
	s = "You have nothing that I want.";

	/* Prepare a prompt */
	if (st_ptr->type == BUILD_STORE_HOME)
	{
		q = "Drop which item? ";

		/* Home takes anything */
		item_tester_hook = NULL;
		
		/* Get an item */
		o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN));
	}
	else
	{
		q = "Sell which item? ";

		/* Only allow items the store will buy */
		item_tester_hook = store_will_stock;
		
		/* Get an item */
		o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_STORE));
	}

	/* Not a valid item */
	if (!o_ptr) return;

	/* Hack -- Cannot remove cursed items */
	if ((!o_ptr->allocated) && cursed_p(o_ptr))
	{
		/* Oops */
		msgf("Hmmm, it seems to be cursed.");

		/* Set the knowledge flag for the player */
		o_ptr->kn_flags[2] |= TR2_CURSED;

		/* Nope */
		return;
	}

	/* Assume one item */
	amt = 1;

	/* Find out how many the player wants to sell */
	if (o_ptr->number > 1)
	{
		/* Describe the object (only what we know) */
		put_fstr(0, 1, "%s %v", "Selling",
					OBJECT_FMT(o_ptr, TRUE, 3));

		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}

	/* Duplicate the object */
	q_ptr = object_dup(o_ptr);

	/* Modify quantity */
	q_ptr->number = amt;

	/*
	 * Hack -- If a rod or wand, allocate total maximum
	 * timeouts or charges to those being sold. -LM-
	 */
	if (o_ptr->tval == TV_WAND)
	{
		q_ptr->pval = (o_ptr->pval + o_ptr->ac) * amt / o_ptr->number;

		/* Remove "used" charges */
		if (q_ptr->pval < o_ptr->ac)
		{
			q_ptr->pval = 0;
		}
		else
		{
			q_ptr->pval -= o_ptr->ac;
		}
	}

	if (o_ptr->tval == TV_ROD)
	{
		q_ptr->pval = o_ptr->pval * amt / o_ptr->number;
	}

	/* Remove any inscription, feeling for stores */
	if (!(st_ptr->type == BUILD_STORE_HOME))
    {
        quark_remove(&q_ptr->inscription);
		q_ptr->feeling = FEEL_NONE;
	}

	/* Is there room in the store (or the home?) */
	if (!store_check_num(q_ptr))
	{
		if (st_ptr->type == BUILD_STORE_HOME)
			msgf("Your home is full.");
		else
			msgf("I have not the room in my store to keep it.");
		return;
	}

	/* Get list to ensure that the object is in the inv */
	list = look_up_list(o_ptr);

	/* Take off equipment */
	if (!list)
	{
		/* Take off first */
		o_ptr = inven_takeoff(o_ptr);

		/* Paranoia */
		if (!o_ptr) return;
	}

	/* Real store */
	if (!(st_ptr->type == BUILD_STORE_HOME))
	{
		/* Get price */
		price = price_item(q_ptr, TRUE) * amt;

		/* Sold... */
		if (store_access_item(q_ptr, price, FALSE))
		{
			/* Say "okay" */
			say_comment_1();

			/* Make a sound */
			sound(SOUND_SELL);

			/* Get some money */
			p_ptr->au += price;

			/* Update the display */
			store_prt_gold();

			/* Get the "apparent" value */
			dummy = object_value(q_ptr) * q_ptr->number;

			/* Duplicate the object */
			q_ptr = object_dup(o_ptr);
			
			/* Identify sold item */
			identify_item(q_ptr);
			
			/* Don't want to let out how many charges on wands */
			if (o_ptr->tval != TV_WAND)
			{
				/* Identify pack item */
				identify_item(o_ptr);
			}

			/* Modify quantity */
			q_ptr->number = amt;

			/*
			 * Hack -- Allocate charges between those wands or rods sold
			 * and retained, unless all are being sold. -LM-
			 */
			distribute_charges(o_ptr, q_ptr, amt);

			/* Get the "actual" value */
			value = object_value(q_ptr) * q_ptr->number;

			/* Describe the result (in message buffer) */
			msgf("You sold %v for %ld gold.", OBJECT_FMT(q_ptr, TRUE, 3), (long)price);

			if (!((q_ptr->tval == TV_FIGURINE) && (value > 0)))
			{
				/* 
				 * Analyze the prices (and comment verbally)
				 * unless object is a figurine
				 */
				purchase_analyze(price, value, dummy);
			}

			if (q_ptr->tval != TV_LITE)
			{
				/* Reset timeouts of the sold items */
				q_ptr->timeout = 0;
			}

			if (q_ptr->tval == TV_WAND)
			{
				/* Reset the "used" charges. */
				q_ptr->ac = 0;
			}

			/* Take the item from the player, describe the result */
			item_increase(o_ptr, -amt);

			/* Handle stuff */
			handle_stuff();

			/* The store gets that (known) item */
			q_ptr = store_carry(q_ptr);

			/* Get position */
			item_pos = get_item_position(st_ptr->stock, q_ptr);

			/* Re-display if item is now in store */
			if (item_pos >= 0)
			{
				p_ptr->state.store_top = (item_pos / 12) * 12;
				display_inventory();
			}
		}
	}

	/* Player is at home */
	else
	{
		/* Distribute charges of wands/rods */
		distribute_charges(o_ptr, q_ptr, amt);
		
		/* Describe */
		msgf("You drop %v.", OBJECT_FMT(q_ptr, TRUE, 3));

		/* Take it from the players inventory */
		item_increase(o_ptr, -amt);

		/* Handle stuff */
		handle_stuff();

		/* Let the home carry it */
		q_ptr = home_carry(q_ptr);

		/* Get position */
		item_pos = get_item_position(st_ptr->stock, q_ptr);

		/* Update store display */
		if (item_pos >= 0)
		{
			p_ptr->state.store_top = (item_pos / 12) * 12;
			display_inventory();
		}
	}
}


/*
 * Examine an item in a store			   -JDL-
 */
static void store_examine(void)
{
	int i;
	int item;
	object_type *o_ptr;
	char out_val[160];


	/* Empty? */
	if (!st_ptr->stock)
	{
		if (st_ptr->type == BUILD_STORE_HOME)
			msgf("Your home is empty.");
		else
			msgf("I am currently out of stock.");
		return;
	}


	/* Find the number of objects on this and following pages */
	i = (get_list_length(st_ptr->stock) - p_ptr->state.store_top);

	/* And then restrict it to the current page */
	if (i > 12) i = 12;

	/* Prompt */
	strnfmt(out_val, 160, "Which item do you want to examine? ");

	/* Get the item number to be examined */
	if (!get_stock(&item, out_val, i)) return;

	/* Get the actual index */
	item = item + p_ptr->state.store_top;

	/* Get the actual item */
	o_ptr = get_list_item(st_ptr->stock, item);

	/* Describe it fully */
	identify_fully_aux(o_ptr);

	return;
}


/*
 * Hack -- set this to leave the store
 */
static bool leave_store = FALSE;


/* These commands are available inside stores and buildings both. */
bool do_standard_command(s16b c)
{
	/* Is this the right sort of command? */
	switch (c)
	{
			/*** Inventory Commands ***/

		case 'w':
		{
			/* Wear/wield equipment */
			do_cmd_wield();
			return (TRUE);
		}

		case 't':
		{
			/* Take off equipment */
			do_cmd_takeoff();
			return (TRUE);
		}

		case 'k':
		{
			/* Destroy an item */
			do_cmd_destroy();
			return (TRUE);
		}

		case 'e':
		{
			/* Equipment list */
			do_cmd_equip();
			return (TRUE);
		}

		case 'i':
		{
			/* Inventory list */
			do_cmd_inven();
			return (TRUE);
		}


			/*** Various commands ***/

		case 'M':
		{
			/* Full dungeon map */
			do_cmd_view_map();
			return (TRUE);
		}

		case 'I':
		{
			/* Identify an object */
			do_cmd_observe();
			return (TRUE);
		}

		case KTRL('I'):
		{
			/* Hack -- toggle windows */
			toggle_inven_equip();
			return (TRUE);
		}


			/*** Use various objects ***/

		case 'b':
		{
			/* Browse a book */
			do_cmd_browse();
			return (TRUE);
		}

		case '{':
		{
			/* Inscribe an object */
			do_cmd_inscribe();
			return (TRUE);
		}

		case '}':
		{
			/* Uninscribe an object */
			do_cmd_uninscribe();
			return (TRUE);
		}



		/*** Help and Such ***/

		case '?':
		{
			/* Help */
			do_cmd_help();
			return (TRUE);
		}

		case '/':
		{
			/* Identify symbol */
			do_cmd_query_symbol();
			return (TRUE);
		}

		case 'C':
		{
			/* Character description */
			do_cmd_character();
			return (TRUE);
		}


		/*** System Commands ***/

		case '!':
		{
			/* Hack -- User interface */
			(void)Term_user(0);
			return (TRUE);
		}

		case '"':
		{
			/* Single line from a pref file */
			do_cmd_pref();
			return (TRUE);
		}

		case '@':
		{
			/* Interact with macros */
			do_cmd_macros();
			return (TRUE);
		}

		case '%':
		{
			/* Interact with visuals */
			do_cmd_visuals();
			return (TRUE);
		}

		case '&':
		{
			/* Interact with colors */
			do_cmd_colors();
			return (TRUE);
		}

		case '=':
		{
			/* Interact with options */
			do_cmd_options(OPT_FLAG_SERVER | OPT_FLAG_PLAYER);
			return (TRUE);
		}

			/*** Misc Commands ***/

		case ':':
		{
			/* Take notes */
			do_cmd_note();
			return (TRUE);
		}

		case 'V':
		{
			/* Version info */
			do_cmd_version();
			return (TRUE);
		}

		case KTRL('F'):
		{
			/* Repeat level feeling */
			do_cmd_feeling();
			return (TRUE);
		}

		case KTRL('P'):
		{
			/* Show previous messages */
			do_cmd_messages();
			return (TRUE);
		}

		case KTRL('Q'):
		{
			/* Show quest status -KMW- */
			do_cmd_checkquest();
			return (TRUE);
		}

		case KTRL('T'):
		{
			/* Get the time of day */
			do_cmd_time();
			return (TRUE);
		}

		case '~':
		case '|':
		{
			/* Check artifacts, uniques, quests etc. */
			do_cmd_knowledge();
			return (TRUE);
		}

		case '(':
		{
			/* Load "screen dump" */
			do_cmd_load_screen();
			return (TRUE);
		}

		case ')':
		{
			/* Save "screen dump" */
			do_cmd_save_screen();
			return (TRUE);
		}
	}

	/* So the commands didn't match */
	return (FALSE);
}

/*
 * Process a command in a store
 *
 * Note that we must allow the use of a few "special" commands
 * in the stores which are not allowed in the dungeon, and we
 * must disable some commands which are allowed in the dungeon
 * but not in the stores, to prevent chaos.
 */
static void store_process_command(void)
{
	int stocknum = get_list_length(st_ptr->stock);

	/* Handle repeating the last command */
	repeat_check();

	if (rogue_like_commands && p_ptr->cmd.cmd == 'l')
	{
		p_ptr->cmd.cmd = 'x';	/* hack! */
	}

	/* Parse the command */
	switch (p_ptr->cmd.cmd)
	{
		case '\r':
		{
			/* Ignore return */
			break;
		}

		case ESCAPE:
		{
			/* Leave */
			leave_store = TRUE;
			break;
		}

		case ' ':
		{
			/* Browse */
			if (stocknum <= 12)
			{
				msgf("Entire inventory is shown.");
			}
			else
			{
				p_ptr->state.store_top += 12;
				if (p_ptr->state.store_top >= stocknum) p_ptr->state.store_top = 0;
				display_inventory();
			}
			break;
		}

		case KTRL('R'):
		{
			/* Redraw */
			do_cmd_redraw();
			display_store();
			break;
		}

		case 'g':
		{
			/* Get (purchase) */
			store_purchase();
			break;
		}

		case 'd':
		{
			/* Drop (Sell) */
			store_sell();
			break;
		}


		case 'x':
		{
			/* Examine */
			store_examine();
			break;
		}

		default:
		{
			/* Is it a standard command? */
			if (!do_standard_command(p_ptr->cmd.cmd))
			{
				/* Hack -- Unknown command */
				msgf("That command does not work in stores.");
				break;
			}
		}
	}
}


/*
 * Deallocate stores stock.
 *
 * This routine is used to deallocate the first store in the
 * store stock cache.  This is done to save memory.
 */
static void deallocate_store(void)
{
	int i;
	store_type *home;

	/* Return if there are no stores with stock */
	if (store_cache_num == 0) return;

	/* Do not deallocate homes or lockers */
	while (store_cache[0]->type == BUILD_STORE_HOME)
	{
		/* Hack - move home to end of cache */

		/* Keep track of stuff in home */
		home = store_cache[0];

		/* Resort the rest of the stores */
		for (i = 1; i < store_cache_num; i++)
		{
			store_cache[i - 1] = store_cache[i];
		}

		/* Move home to the end */
		store_cache[store_cache_num - 1] = home;
	}

	/* Delete store least used. */
	delete_object_list(&store_cache[0]->stock);

	/* Shift all other stores down the cache to fill the gap */
	for (i = 1; i < store_cache_num; i++)
	{
		store_cache[i - 1] = store_cache[i];
	}

	/* Decrease number of stores with stock */
	store_cache_num--;
}


/*
 * Allocate memory for a stores stock.
 *
 * This routine is used to save memory.  It is a waste to record
 * what is in every store in every town in the wilderness.  This
 * allocates the required array if the stockpointer is NULL.
 */
bool allocate_store(store_type *st_ptr)
{
	int i, n = -1;

	/* Find the location in the cache */
	for (i = 0; i < store_cache_num; i++)
	{
		/* See if cache location matches */
		if (st_ptr == store_cache[i])
		{
			/* Resort order based on last_visit */
			for (n = i + 1; n < store_cache_num; n++)
			{
				store_cache[n - 1] = store_cache[n];
			}

			/* Move current one to end */
			store_cache[store_cache_num - 1] = st_ptr;

			/* (No need to maintain store) */
			return FALSE;
		}
	}

	/* Store does not have stock - so need to allocate. */

	/* See if cache is full */
	if (store_cache_num == STORE_CACHE_AMNT)
	{
		/* Delete least used store */
		deallocate_store();
	}

	/* Add store to end of cache */
	store_cache[store_cache_num] = st_ptr;

	/* The number in the cache has increased */
	store_cache_num++;

	/* (Need to maintain stores) */
	return TRUE;
}

store_type *get_current_store(void)
{
	place_type *pl_ptr = &place[p_ptr->place_num];

	int i, which = -1;

	/* Get the building the player is on */
	for (i = 0; i < pl_ptr->numstores; i++)
	{
		if ((p_ptr->py - pl_ptr->y * 16 == pl_ptr->store[i].y) &&
			(p_ptr->px - pl_ptr->x * 16 == pl_ptr->store[i].x))
		{
			which = i;
		}
	}

	/* Paranoia */
	if (which == -1)
	{
		msgf("Could not locate building!");
		return (NULL);
	}

	/* Return a pointer to the store */
	return (&pl_ptr->store[which]);
}


/*
 * Enter a store, and interact with it.
 *
 * Note that we use the standard "request_command()" function
 * to get a command, allowing us to use "cmd.arg" and all
 * command macros and other nifty stuff, but we use the special
 * "shopping" argument, to force certain commands to be converted
 * into other commands, normally, we convert "p" (pray) and "m"
 * (cast magic) into "g" (get), and "s" (search) into "d" (drop).
 */
void do_cmd_store(const field_type *f1_ptr)
{
	int maintain_num;
	int tmp_chr;
	int i;

	object_type *o_ptr;
	
	/* Disturb */
	disturb(FALSE);

	/* Hack - save f1_ptr for later */
	f_ptr = f1_ptr;

	/* Save the store pointer */
	st_ptr = get_current_store();

	/* Paranoia */
	if (!st_ptr) return;
	
	/* Init store if required */
	field_script_const(f1_ptr, FIELD_ACT_SB_INIT, "");
	
	/* Some quests are finished by finding a shop */
	trigger_quest_complete(QX_FIND_SHOP, (vptr)st_ptr);

	/* Hack - save interesting flags for later */
	info_flags = f_ptr->data[7];

	/* Hack -- Check the "locked doors" */
	if (ironman_shops)
	{
		msgf("The doors are locked.");
		return;
	}
		
	/* Calculate the number of store maintainances since the last visit */
	maintain_num = (turn - st_ptr->last_visit) / (10L * STORE_TURNS);

	/* Recalculate maximum number of items in store */
	if (f_ptr->data[7] & ST_HALF_INVEN)
	{
		st_ptr->max_stock = STORE_INVEN_MAX / 2;
	}
	else
	{
		st_ptr->max_stock = STORE_INVEN_MAX;
	}

	/* Allocate object storage if required */
	if (allocate_store(st_ptr))
	{
		/* Hack - Maintain store if it is just allocated. */
		maintain_num++;
	}

	/* Maintain the store max. 20 times */
	if (maintain_num > 20) maintain_num = 20;

	if (maintain_num)
	{
		/* Maintain the store */
		for (i = 0; i < maintain_num; i++)
		{
			store_maint();
		}

		/* Save the visit */
		st_ptr->last_visit = turn;
	}

	/* Forget the view */
	forget_view();


	/* Hack -- Character is in "icky" mode */
	screen_save();

	/* No command argument */
	p_ptr->cmd.arg = 0;

	/* No repeated command */
	p_ptr->cmd.rep = 0;

	/* No automatic command */
	p_ptr->cmd.new = 0;

	/* Start at the beginning */
	p_ptr->state.store_top = 0;

	/* Display the store */
	display_store();

	/* Hack - change the redraw hook so bigscreen works */
	angband_term[0]->resize_hook = display_store;

	/* Do not leave */
	leave_store = FALSE;

	/* Interact with player */
	while (!leave_store)
	{
		/* Hack -- Clear lines 1 and 2 */
		clear_region(0, 1, 2);

		/* Hack -- Check the charisma */
		tmp_chr = p_ptr->stat[A_CHR].use;

		/* Clear */
		clear_from(21);

		/* Update store inventory information */
		if (st_ptr->type == BUILD_STORE_HOME)
		{
			Term_write_list(st_ptr->stock, LIST_HOME);
		}
		else
		{
			Term_write_list(st_ptr->stock, LIST_STORE);
		}

		/* What can we sell? */
		if (st_ptr->type == BUILD_STORE_HOME)
		{
			/* Home takes anything */
			item_tester_hook = NULL;
		}
		else
		{
			/* Only allow items the store will buy */
			item_tester_hook = store_will_stock;
		}

		/* Update player inventory information */
		OBJ_ITT_START (p_ptr->inventory, o_ptr)
		{
			/* Not right type of item? */
			if (item_tester_hook && !item_tester_hook(o_ptr))
			{
				/* Hack - cannot sell item */
				o_ptr->temp_cost = 0;

				continue;
			}

			/* Not enough room? */
			if (!store_check_num(o_ptr))
			{
				/* Hack - cannot sell item */
				o_ptr->temp_cost = 0;

				continue;
			}

			if (st_ptr->type == BUILD_STORE_HOME)
			{
				/*
				 * Hack - you can 'sell' anything to your home
				 * if there is room
				 */
				o_ptr->temp_cost = 1;
			}
			else
			{
				/*
				 * Hack - Otherwise, get store price
				 * for one item into o_ptr->temp_cost.
				 * (This is set inside price_item().)
				 */
				(void)price_item(o_ptr, TRUE);
			}
		}
		OBJ_ITT_END;

		/* Send information */
		Term_write_list(p_ptr->inventory, LIST_INVEN);

		/* Reset tester hook */
		item_tester_hook = NULL;


		/* Basic commands */
		prtf(0, 22, " ESC) Exit from Building.");

		/* Browse if necessary */
		if (get_list_length(st_ptr->stock) > 12)
		{
			prtf(0, 23, " SPACE) Next page of stock");
		}

		/* Home commands */
		if (st_ptr->type == BUILD_STORE_HOME)
		{
			prtf(31, 22, " g) Get an item.");
			prtf(31, 23, " d) Drop an item.");
		}

		/* Shop commands XXX XXX XXX */
		else
		{
			prtf(31, 22, " p) Purchase an item.");
			prtf(31, 23, " s) Sell an item.");
		}

		/* Add in the eXamine option */
		prtf(56, 22, " x) eXamine an item.");

		/* Prompt */
		prtf(0, 21, "You may: ");

		/* Get a command */
		request_command(TRUE);

		/* Process the command */
		store_process_command();

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* XXX XXX XXX Pack Overflow */
		if (get_list_length(p_ptr->inventory) > INVEN_PACK)
		{
			/* Message */
			msgf("Your pack is so full that you flee outside...");

			/* Leave */
			leave_store = TRUE;
		}

		/* Hack -- Redisplay store prices if charisma changes */
		if (tmp_chr != p_ptr->stat[A_CHR].use)
		{
			display_inventory();
		}
	}


	/* Free turn XXX XXX XXX */
	p_ptr->state.energy_use = 0;

	/* Hack -- Character is no longer in "icky" mode */
	screen_load();

	/* Hack -- Cancel automatic command */
	p_ptr->cmd.new = 0;

	/* Flush messages XXX XXX XXX */
	message_flush();
	
	/* Hack - reset the redraw hook */
	angband_term[0]->resize_hook = resize_map;

	/* Clear the screen */
	Term_clear();
	
	/* Update for the changed screen size */
	resize_map();

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}


/*
 * Initialize a store
 */
void store_init(int town_num, int store_num, byte store)
{
	cptr own_name = owner_names[randint0(owner_names_max)];
	cptr own_suffix = owner_suffix[randint0(owner_suffix_max)];

	/* Activate that building */
	store_type *st_ptr = &place[town_num].store[store_num];

	/* Pick an owner */
	st_ptr->owner_name = quark_fmt("%s %s", own_name, own_suffix);
	
	/* These are set in place_sb() via lua hooks */
	st_ptr->greed = 0;
	st_ptr->max_cost = 0;

	/* Do not allocate the stock yet. */
	st_ptr->stock = 0;

	/* Set the store type */
	st_ptr->type = store;

	/* Initialize the store */
	st_ptr->data = 0;

	/*
	 * Hack - maximum items in stock
	 * (This number may be changed when the store
	 *  is actually created.)
	 * The reason we do it later, is because we do not
	 * have the field data at this stage.  This line
	 * perhaps can be removed, but beware of strange bugs
	 * popping up in other code.
	 */
	st_ptr->max_stock = STORE_INVEN_MAX;

	/*
	 * MEGA-HACK - Last visit to store is
	 * BEFORE player birth to enable store restocking
	 */
	st_ptr->last_visit = -100L * STORE_TURNS;
}

void place_sb(int greed, int max_cost)
{
	store_type *st_ptr = get_current_store();

	/* Set greed / max_cost values if unset */
	if (!st_ptr->greed)
	{
		st_ptr->greed = greed;
		st_ptr->max_cost = max_cost;
	}
}
