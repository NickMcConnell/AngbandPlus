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

static cptr comment_1[MAX_COMMENT_1] = {
	"Okay.",
	"Fine.",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!"
};

#define MAX_COMMENT_2A	2

static cptr comment_2a[MAX_COMMENT_2A] = {
	"You try my patience.  %s is final.",
	"My patience grows thin.  %s is final."
};

#define MAX_COMMENT_2B	12

static cptr comment_2b[MAX_COMMENT_2B] = {
	"I can take no less than %s gold pieces.",
	"I will accept no less than %s gold pieces.",
	"Ha!  No less than %s gold pieces.",
	"You knave!  No less than %s gold pieces.",
	"That's a pittance!  I want %s gold pieces.",
	"That's an insult!  I want %s gold pieces.",
	"As if!  How about %s gold pieces?",
	"My arse!  How about %s gold pieces?",
	"May the fleas of 1000 orcs molest you!  Try %s gold pieces.",
	"May your most favourite parts go moldy!  Try %s gold pieces.",
	"May Morgoth find you tasty!  Perhaps %s gold pieces?",
	"Your mother was an Ogre!  Perhaps %s gold pieces?"
};

#define MAX_COMMENT_3A	2

static cptr comment_3a[MAX_COMMENT_3A] = {
	"You try my patience.  %s is final.",
	"My patience grows thin.  %s is final."
};


#define MAX_COMMENT_3B	12

static cptr comment_3b[MAX_COMMENT_3B] = {
	"Perhaps %s gold pieces?",
	"How about %s gold pieces?",
	"I will pay no more than %s gold pieces.",
	"I can afford no more than %s gold pieces.",
	"Be reasonable.  How about %s gold pieces?",
	"I'll buy it as scrap for %s gold pieces.",
	"That is too much!  How about %s gold pieces?",
	"That looks war surplus!  Say %s gold pieces?",
	"Never!  %s is more like it.",
	"That's an insult!  %s is more like it.",
	"%s gold pieces and be thankful for it!",
	"%s gold pieces and not a copper more!"
};

#define MAX_COMMENT_4A	4

static cptr comment_4a[MAX_COMMENT_4A] = {
	"Enough!  You have abused me once too often!",
	"Arghhh!  I have had enough abuse for one day!",
	"That does it!  You shall waste my time no more!",
	"This is getting nowhere!  I'm going to Londis!"
};

#define MAX_COMMENT_4B	4

static cptr comment_4b[MAX_COMMENT_4B] = {
	"Leave my store!",
	"Get out of my sight!",
	"Begone, you scoundrel!",
	"Out, out, out!"
};

#define MAX_COMMENT_5	8

static cptr comment_5[MAX_COMMENT_5] = {
	"Try again.",
	"Ridiculous!",
	"You will have to do better than that!",
	"Do you wish to do business or not?",
	"You've got to be kidding!",
	"You'd better be kidding!",
	"You try my patience.",
	"Hmmm, nice weather we're having."
};

#define MAX_COMMENT_6	4

static cptr comment_6[MAX_COMMENT_6] = {
	"I must have heard you wrong.",
	"I'm sorry, I missed that.",
	"I'm sorry, what was that?",
	"Sorry, what was that again?"
};



/*
 * Successful haggle.
 */
static void say_comment_1(void)
{
	mprint(MSG_TEMP, comment_1[rand_int(MAX_COMMENT_1)]);
	msg_print(NULL);
}


/*
 * Continue haggling (player is buying)
 */
static void say_comment_2(s32b value, int annoyed)
{
	char tmp_val[80];

	/* Prepare a string to insert */
	sprintf(tmp_val, "%ld", (long) value);

	/* Final offer */
	if (annoyed > 0)
	{
		/* Formatted message */
		mformat(MSG_TEMP, comment_2a[rand_int(MAX_COMMENT_2A)], tmp_val);
	}

	/* Normal offer */
	else
	{
		/* Formatted message */
		mformat(MSG_TEMP, comment_2b[rand_int(MAX_COMMENT_2B)], tmp_val);
	}
	msg_print(NULL);
}


/*
 * Continue haggling (player is selling)
 */
static void say_comment_3(s32b value, int annoyed)
{
	char tmp_val[80];

	/* Prepare a string to insert */
	sprintf(tmp_val, "%ld", (long) value);

	/* Final offer */
	if (annoyed > 0)
	{
		/* Formatted message */
		mformat(MSG_TEMP, comment_3a[rand_int(MAX_COMMENT_3A)], tmp_val);
	}

	/* Normal offer */
	else
	{
		/* Formatted message */
		mformat(MSG_TEMP, comment_3b[rand_int(MAX_COMMENT_3B)], tmp_val);
	}
	msg_print(NULL);
}


/*
 * You must leave my store
 */
static void say_comment_4(void)
{
	mprint(MSG_TEMP, comment_4a[rand_int(MAX_COMMENT_4A)]);
	mprint(MSG_TEMP, comment_4b[rand_int(MAX_COMMENT_4B)]);
	msg_print(NULL);
}


/*
 * You are insulting me
 */
static void say_comment_5(void)
{
	mprint(MSG_TEMP, comment_5[rand_int(MAX_COMMENT_5)]);
	msg_print(NULL);
}


/*
 * You are making no sense.
 */
static void say_comment_6(void)
{
	mprint(MSG_TEMP, comment_6[rand_int(MAX_COMMENT_6)]);
	msg_print(NULL);
}



/*
 * Messages for reacting to purchase prices.
 */

#define MAX_COMMENT_7A	4

static cptr comment_7a[MAX_COMMENT_7A] = {
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!"
};

#define MAX_COMMENT_7B	4

static cptr comment_7b[MAX_COMMENT_7B] = {
	"Damn!",
	"You fiend!",
	"The shopkeeper curses at you.",
	"The shopkeeper glares at you."
};

#define MAX_COMMENT_7C	4

static cptr comment_7c[MAX_COMMENT_7C] = {
	"Cool!",
	"You've made my day!",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly."
};

#define MAX_COMMENT_7D	4

static cptr comment_7d[MAX_COMMENT_7D] = {
	"Yipee!",
	"I think I'll retire!",
	"The shopkeeper jumps for joy.",
	"The shopkeeper smiles gleefully."
};


/*
 * Info about the current store.
 */
struct store_info
{
	store_type *st_ptr;	/* Current store */
	owner_type *ot_ptr;	/* Current owner */
	int per_page; /* Number of items per page */
	int page_cur; /* Current page */
	int page_cur_cnt; /* Number of items on displayed page */
	int page_cnt; /* Number of pages */
	object_type *pages[MAX_STACK_PAGES]; /* First object in each page */
	bool leave;	/* TRUE when leaving */
};
static struct store_info store_info_body, *st_cur = &store_info_body;


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
		mprint(MSG_TEMP, comment_7a[rand_int(MAX_COMMENT_7A)]);

		/* Sound */
		sound(SOUND_STORE1);
	}

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
	{
		/* Comment */
		mprint(MSG_TEMP, comment_7b[rand_int(MAX_COMMENT_7B)]);

		/* Sound */
		sound(SOUND_STORE2);
	}

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (value < (4 * guess)) && (price < value))
	{
		/* Comment */
		mprint(MSG_TEMP, comment_7c[rand_int(MAX_COMMENT_7C)]);

		/* Sound */
		sound(SOUND_STORE3);
	}

	/* Item was a great bargain, and we got away with it */
	else if ((value > guess) && (price < value))
	{
		/* Comment */
		mprint(MSG_TEMP, comment_7d[rand_int(MAX_COMMENT_7D)]);

		/* Sound */
		sound(SOUND_STORE4);
	}
	msg_print(NULL);
}



#define MAX_COMMENT_8	8

static cptr comment_8[MAX_COMMENT_8] = {
	"Hey, jerk, don't get your grubby fingers on my stuff!",
	"That's it! You just ruined my favorite item!",
	"Thief! Thief! Call the guards!",
	"Damn shoplifters, keep your dirty hands off my stuff!",
	"Hey, that was mine! You're going to pay for ruining my stuff!",
	"Nobody ruins my wares and lives to tell about it! Nobody!!",
	"You think destroying my best items is just fun and games, eh?!",
	"It'll take me years to find a item as good as the one you ruined!"
};


/*
 * Player ruined an item -- and it wasn't his! Punish the player accordingly.
 */
void player_theft(void)
{
	mformat(MSG_URGENT, "The shopkeeper shouts: ``%s''",
		comment_8[rand_int(MAX_COMMENT_8)]);

	/* Mark the player as a criminal. */
	if (p_ptr->sc > -100)
	{
		p_ptr->sc = -100;
	}

	/* Summon some guards. */
	activate_generators();
}


/*
 * Buying and selling adjustments for race combinations.
 * Entry[owner][player] gives the basic "cost inflation".
 */
static byte rgold_adj[MAX_RACES][MAX_RACES] = {
	/* Kobold added by GJW -KMW- */
	/*Hum, HfE, Elf, Hal, Gno, Dwa, HfO, HfT, Dun, HiE, Kob, Mut, Gst,
	 *Mch, Glm, Lep, Mol, Vor */

	/* Human */
	{100, 105, 105, 110, 113, 115, 120, 125, 100, 105, 120, 120, 130,
		90, 105, 115, 130, 130},

	/* Half-Elf */
	{110, 100, 100, 105, 110, 120, 125, 130, 110, 100, 115, 120, 110,
		90, 105, 120, 130, 120},

	/* Elf */
	{110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 120, 120, 110,
		90, 105, 120, 130, 110},

	/* Halfling */
	{115, 110, 105, 95, 105, 110, 115, 130, 115, 105, 115, 120, 130,
		90, 105, 115, 130, 130},

	/* Gnome */
	{115, 115, 110, 105, 95, 110, 115, 130, 115, 110, 110, 110, 130,
		90, 95, 105, 130, 130},

	/* Dwarf */
	{115, 120, 120, 110, 110, 95, 125, 135, 115, 120, 115, 120, 120,
		90, 95, 130, 120, 130},

	/* Half-Orc */
	{115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115, 120, 105,
		90, 115, 115, 95, 130},

	/* Half-Troll */
	{110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 115, 120, 105,
		90, 115, 115, 95, 130},

	/* Dunedain  */
	{100, 105, 105, 110, 113, 115, 120, 125, 100, 105, 120, 120, 105,
		90, 105, 115, 130, 115},

	/* High_Elf */
	{110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 125, 120, 110,
		90, 105, 120, 130, 110},

	/* Kobold - Added by GJW -KMW- */
	{110, 115, 120, 110, 105, 110, 110, 120, 110, 125, 90, 110, 120,
		90, 120, 105, 110, 130},

	/* Mutant */
	{110, 110, 110, 105, 105, 110, 120, 130, 110, 115, 105, 100, 130,
		90, 120, 105, 120, 100},

	/* Ghost */
	{110, 110, 110, 110, 110, 110, 120, 120, 100, 110, 110, 110, 100,
		90, 120, 105, 100, 90},

	/* Munchkin */
	{150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150,
		150, 150, 150, 150, 150},

	/* Golem */
	{110, 110, 110, 110, 100, 105, 120, 120, 110, 105, 120, 115, 120,
		90, 100, 105, 120, 90},

	/* Leprechaun */
	{110, 110, 110, 105, 105, 115, 120, 120, 110, 110, 110, 105, 110,
		90, 110, 100, 120, 110},

	/* Death Mold */
	{115, 115, 115, 115, 115, 115, 105, 105, 120, 130, 110, 110, 105,
		90, 115, 115, 100, 100},

	/* Vortex */
	{130, 120, 110, 130, 120, 130, 130, 130, 110, 105, 130, 90, 100,
		90, 90, 110, 100, 80},
};



/*
 * ST_INFO.TXT: Some of the new store code.
 */

/*
 * Variables
 */

struct str_buy_sell_info
{
	bool buy[MAX_K_IDX];
	int sell[MAX_K_IDX];
};


#define ST_INFO_BLACK_MARKET    6
#define ST_INFO_HOME            7


static struct str_buy_sell_info buy_sell_info[MAX_STORES];

static int st_info_shop = -1;


/*
 * Function to check that a store will buy an item.
 */
static bool store_will_buy(object_type * o_ptr)
{
	/* Home accepts everything */
	if (p_ptr->s_idx == ST_INFO_HOME)
		return TRUE;

	/* Don't buy anything worthless */
	if (object_value(o_ptr) <= 0)
		return FALSE;

	/* Will the store buy item? */
	return (buy_sell_info[p_ptr->s_idx].buy[o_ptr->k_idx]);
}

/*
 * Maintain any old store.
 */
static void store_maint_any(int which)
{
	store_type *st_ptr = &store[which];
	object_type *o_ptr;
	object_type *o_nxt;

	int k_idx, i_val;

	/*
	 * Remove any 'common', 'cheap' or irrelevant items
	 */
	o_ptr = st_ptr->stock;

	while (o_ptr)
	{
		/* Preload the next object. */
		o_nxt = o_ptr->next_global;

		if (buy_sell_info[which].sell[o_ptr->k_idx] > 100 ||
			buy_sell_info[which].buy[o_ptr->k_idx] == FALSE ||
			object_value(o_ptr) < 1)
		{
			/* Remove this item */
			remove_from_global_list(o_ptr, &(st_ptr->stock));

			KILL(o_ptr, object_type);
		}

		/* Next item */
		o_ptr = o_nxt;
	}

	/*
	 * Now remove 20% of stock
	 */
	o_ptr = st_ptr->stock;

	while (o_ptr)
	{
		/* Preload the next object. */
		o_nxt = o_ptr->next_global;

		if (randint(100) < 20)
		{
			/* Remove this item */
			remove_from_global_list(o_ptr, &(st_ptr->stock));

			KILL(o_ptr, object_type);
		}

		/* Next item */
		o_ptr = o_nxt;
	}

	/*
	 * Now add any common items
	 */
	for (k_idx = 0; k_idx < MAX_K_IDX; k_idx++)
	{
		/* Calculate i_val */
		i_val = buy_sell_info[which].sell[k_idx];
		i_val = randint(i_val << 1) / 100;

		/* Create an item? */
		if (i_val < 1)
			continue;

		/* Now create the item */
		MAKE(o_ptr, object_type);
		object_prep(o_ptr, k_idx);
		o_ptr->number = i_val;
		o_ptr->weight *= i_val;

		/* Single items may be transformed into ego items */
		/* GREP if(o_ptr->number == 1 && 
		 * (i_val = 127 - buy_sell_info[which].sell[k_idx]) > 0 &&
		 * magik(20)) */

		i_val = 127 - buy_sell_info[which].sell[k_idx];

		if (i_val > 0)
		{
			apply_magic(o_ptr, i_val, FALSE, FALSE, FALSE);
		}

		/* Store-made items are stronger. */
		o_ptr->chp = (o_ptr->chp * rand_range(10, 20)) / 10;

		/* The item is "known" */
		object_known(o_ptr);

		/* Remove worthless items */
		if (object_value(o_ptr) <= 0)
		{
			KILL(o_ptr, object_type);
		}

		/* Add to store */
		else
		{
			/* Attempt to carry the (known) item */
			insert_to_global_list(o_ptr, &(st_ptr->stock), WORLD_STORE);
		}
	}
}


/*
 * Maintain the black market.
 */
static void store_maint_bm(void)
{
	store_type *st_ptr = &store[ST_INFO_BLACK_MARKET];
	object_type *o_ptr;
	object_type *o_nxt;

	s32b k_idx = 0, i_val, new_amount = 0;

	/*
	 * Remove any cheap items
	 */
	o_ptr = st_ptr->stock;

	while (o_ptr)
	{
		/* Preload the next object. */
		o_nxt = o_ptr->next_global;

		if (object_value(o_ptr) < 150 || o_ptr->tval == TV_GOLD ||
			o_ptr->tval == TV_CHEST)
		{
			/* Remove this item */
			remove_from_global_list(o_ptr, &(st_ptr->stock));

			KILL(o_ptr, object_type);
		}

		/* Next item */
		o_ptr = o_nxt;
	}

	/*
	 * Now remove 50% of remaining stock
	 */
	o_ptr = st_ptr->stock;

	while (o_ptr)
	{
		/* Preload the next object. */
		o_nxt = o_ptr->next_global;

		if (randint(100) < 50)
		{
			/* Remove this item */
			remove_from_global_list(o_ptr, &(st_ptr->stock));

			KILL(o_ptr, object_type);
		}

		/* Next item */
		o_ptr = o_nxt;
	}

	/*
	 * Now add some items (maximum amount of 35).
	 */

	/* Count current items */
	for (o_ptr = st_ptr->stock; o_ptr; o_ptr = o_ptr->next_global)
		new_amount++;

	/* Too many? */
	if (new_amount > 25)
		return;

	/* Amount to create */
	new_amount = 25 + randint(10) - new_amount;

	/* Begin creating items */
	while (new_amount--)
	{
		/* Get a random item */
		k_idx = randint(MAX_K_IDX);

		/* Mega-hack -- fobidden range. */
		if ((k_idx >= 480 && k_idx <= 512) || (k_idx >= 585 &&
				k_idx <= 599) || (k_idx >= 338 && k_idx <= 344) ||
			(k_idx >= MAX_K_IDX))
		{
			continue;
		}

		/* Set it up */
		MAKE(o_ptr, object_type);
		object_prep(o_ptr, k_idx);

		/* The item is "known" */
		object_known(o_ptr);

		/* Is it a worthy item? */
		if (object_value(o_ptr) <= 100)
		{

			KILL(o_ptr, object_type);
			continue;
		}

		/* Number to create */
		i_val = ((10000 / object_value(o_ptr)) * (10 + randint(5))) / 100;

		if (i_val <= 0)
			i_val = 1;

		/* More setup */
		o_ptr->number = i_val;
		o_ptr->weight *= i_val;

		apply_magic(o_ptr, randint(50) + 25, FALSE, FALSE, FALSE);

		/* Is it a worthy item? */
		if (object_value(o_ptr) <= 100)
		{

			KILL(o_ptr, object_type);
			continue;
		}

		/* Add to store */
		insert_to_global_list(o_ptr, &(st_ptr->stock), WORLD_STORE);
	}
}

/*
 * Function to maintain stores.
 */
void store_maint(int which)
{
	/* Home is not maintained */
	if (which == ST_INFO_HOME)
		return;

	/* Black market has special rules */
	if (which == ST_INFO_BLACK_MARKET)
		store_maint_bm();

	/* Other store */
	else
		store_maint_any(which);
}


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
static s32b price_item(owner_type * ot_ptr, object_type * o_ptr, int greed,
	bool flip)
{
	int factor;
	int adjust;
	s32b price;


	/* Get the value of one of the items */
	price = object_value(o_ptr);

	/* Worthless items */
	if (price <= 0)
		return (0L);


	/* Compute the racial factor */
	factor = rgold_adj[ot_ptr->owner_race][p_ptr->prace];

	/* Add in the charisma factor */
	factor += adj_chr_gold[p_ptr->stat_ind[A_CHR]];


	/* Shop is buying */
	if (flip)
	{
		/* Adjust for greed */
		adjust = 100 + (300 - (greed + factor));

		/* Never get "silly" */
		if (adjust > 100)
			adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (p_ptr->s_idx == 6)
			price = price / 2;
	}

	/* Shop is selling */
	else
	{
		/* Adjust for greed */
		adjust = 100 + ((greed + factor) - 300);

		/* Never get "silly" */
		if (adjust < 100)
			adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (p_ptr->s_idx == 6)
			price = price * 2;
	}

	/* Compute the final price (with rounding) */
	price = (price * adjust + 50L) / 100L;

	/* Note -- Never become "free" */
	if (price <= 0L)
		return (1L);

	/* Return the price */
	return (price);
}


/*
 * Determine if an item can "absorb" a second item
 *
 * See "store_object_absorb()" for the actual "absorption" code.
 *
 * All identical items will stack in a store. Some fields, such as
 * the inscription field, are ignored.
 */
bool store_object_similar(object_type * o_ptr, object_type * j_ptr)
{
	/* Hack -- Identical items cannot be stacked */
	if (o_ptr == j_ptr)
		return (FALSE);

	/* Don't combine dead items. */
	if (!o_ptr->k_idx || !j_ptr->k_idx)
		return (FALSE);

	/* Different objects cannot be stacked */
	if (o_ptr->k_idx != j_ptr->k_idx)
		return (FALSE);

	/* Different materials cannot be stacked */
	if (o_ptr->stuff != j_ptr->stuff)
		return (FALSE);

	/* Different charges (etc) cannot be stacked */
	if (o_ptr->pval != j_ptr->pval)
		return (FALSE);

	/* Require identical "bonuses" */
	if (o_ptr->to_h != j_ptr->to_h)
		return (FALSE);
	if (o_ptr->to_d != j_ptr->to_d)
		return (FALSE);
	if (o_ptr->to_a != j_ptr->to_a)
		return (FALSE);

	/* Require identical "artifact" names */
	if (o_ptr->name1 != j_ptr->name1)
		return (FALSE);

	/* Require identical "ego-item" names */
	if (o_ptr->name2 != j_ptr->name2)
		return (FALSE);

	/* Hack -- Never stack recharging items */
	if (o_ptr->timeout || j_ptr->timeout)
		return (FALSE);

	/* Require identical health. */
	if (o_ptr->chp != j_ptr->chp)
		return (FALSE);

	/* Require identical "values" */
	if (o_ptr->ac != j_ptr->ac)
		return (FALSE);
	if (o_ptr->dd != j_ptr->dd)
		return (FALSE);
	if (o_ptr->ds != j_ptr->ds)
		return (FALSE);

	/* Hack -- Never stack chests */
	if (o_ptr->tval == TV_CHEST)
		return (FALSE);

	/* Rrequire matching "discounts" */
	if (o_ptr->discount != j_ptr->discount)
		return (FALSE);

	/* Maximal "stacking" limit */
	if (o_ptr->number + j_ptr->number >= MAX_STACK_SIZE)
		return (FALSE);

	/* They match, so they must be similar */
	return (TRUE);
}


/*
 * Allow a store object to absorb another object
 */
void store_object_absorb(object_type *o_ptr, object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;
	int wgt = o_ptr->weight / o_ptr->number;
	
	/* Combine quantity, lose excess items */
	o_ptr->number = (total > MAX_STACK_SIZE) ? MAX_STACK_SIZE : total;

	/* Combine weight */
	o_ptr->weight = o_ptr->number * wgt;
}


/*
 * Combine wares
 */
void store_combine(store_type *st_ptr)
{
	object_type *iter1;
	object_type *iter2;
	object_type *tmp;


	for (iter1 = st_ptr->stock; iter1 != NULL; iter1 = iter1->next_global)
	{
		for (iter2 = iter1; iter2->next_global != NULL; iter2 = iter2->next_global)
		{
			tmp = iter2->next_global;

			if (p_ptr->s_idx == ST_INFO_HOME)
			{
				/* Don't combine worlds ??? */
				if (iter1->world != tmp->world) continue;
		
				/* Compare the objects */
				if (object_similar(iter1, tmp))
				{
					/* Combine them. */
					object_absorb(iter1, tmp);
	
					/* Destroy object */
					remove_object(tmp);
				}
			}
			else
			{
				/* Compare objects */
				if (store_object_similar(iter1, tmp))
				{
					/* Combine them. */
					store_object_absorb(iter1, tmp);

					/* Destroy object */
					remove_object(tmp);
				}
			}
		}
	}
}


/*
 * Eliminate need to bargain if player has haggled well in the past
 */
static bool noneedtobargain(store_type * st_ptr, s32b minprice)
{
	s32b good = st_ptr->good_buy;
	s32b bad = st_ptr->bad_buy;

	/* Cheap items are "boring" */
	if (minprice < 10L)
		return (TRUE);

	/* Perfect haggling */
	if (good == MAX_SHORT)
		return (TRUE);

	/* Reward good haggles, punish bad haggles, notice price */
	if (good > ((3 * bad) + (5 + (minprice / 50))))
		return (TRUE);

	/* Return the flag */
	return (FALSE);
}


/*
 * Update the bargain info
 */
static void updatebargain(store_type * st_ptr, s32b price, s32b minprice)
{
	/* Hack -- auto-haggle */
	if (auto_haggle)
		return;

	/* Cheap items are "boring" */
	if (minprice < 10L)
		return;

	/* Count the successful haggles */
	if (price == minprice)
	{
		/* Just count the good haggles */
		if (st_ptr->good_buy < MAX_SHORT)
		{
			st_ptr->good_buy++;
		}
	}

	/* Count the failed haggles */
	else
	{
		/* Just count the bad haggles */
		if (st_ptr->bad_buy < MAX_SHORT)
		{
			st_ptr->bad_buy++;
		}
	}
}


/*
 * Get a store object
 */
static object_type *get_stock(cptr pmt)
{
	int item;
	char which;
	char buf[160];
	char o_name[80];
	char out_val[160];
	object_type *o_ptr = NULL;


	/* Build the prompt */
	sprintf(buf, "(Store: %c-%c, ESC to exit) %s", I2A(0),
		I2A(st_cur->page_cur_cnt - 1), pmt);

	/* Ask until done */
	while (TRUE)
	{
		int i = 0, ver;

		/* Escape */
		if (!get_com(buf, &which))
			return (NULL);

		/* Extract "query" setting */
		ver = isupper(which);
		which = tolower(which);

		/* Convert response to item */
		item = A2I(which);

		/* Oops */
		if (item < 0 || item >= st_cur->page_cur_cnt)
		{
			/* Oops */
			bell();

			/* Ask again */
			continue;
		}

		/* Object */
		for (o_ptr = st_cur->pages[st_cur->page_cur]; o_ptr != NULL;
			o_ptr = o_ptr->next_global)
		{
			if (i++ == item)
				break;
		}

		/* No verification */
		if (!ver)
			break;

		/* Home */
		if (p_ptr->s_idx == ST_INFO_HOME)
		{
			/* Describe */
			object_desc(o_name, o_ptr, TRUE, 3);
		}

		/* Shop */
		else
		{
			/* Describe */
			object_desc(o_name, o_ptr, TRUE, 3);
		}

		/* Prompt */
		sprintf(out_val, "Try %s? ", o_name);

		/* Query */
		if (!get_check(out_val))
			return (NULL);

		/* Done */
		break;
	}

	/* Success */
	return (o_ptr);
}


/*
 * Increase the insult counter and get angry if necessary
 */
static int increase_insults(owner_type * ot_ptr, store_type * st_ptr)
{
	/* Increase insults */
	st_ptr->insult_cur++;

	/* Become insulted */
	if (st_ptr->insult_cur > ot_ptr->insult_max)
	{
		/* Complain */
		say_comment_4();

		/* Reset insults */
		st_ptr->insult_cur = 0;
		st_ptr->good_buy = 0;
		st_ptr->bad_buy = 0;

		/* Open tomorrow */
		st_ptr->store_open = turn + 25000 + randint(25000);

		/* Closed */
		return (TRUE);
	}

	/* Not closed */
	return (FALSE);
}


/*
 * Decrease the insult counter
 */
static void decrease_insults(store_type * st_ptr)
{
	/* Decrease insults */
	if (st_ptr->insult_cur)
		st_ptr->insult_cur--;
}


/*
 * The shop-keeper has been insulted
 */
static int haggle_insults(owner_type * ot_ptr, store_type * st_ptr)
{
	/* Increase insults */
	if (increase_insults(ot_ptr, st_ptr))
		return (TRUE);

	/* Display and flush insult */
	say_comment_5();

	/* Still okay */
	return (FALSE);
}


/*
 * Mega-Hack -- Enable "increments"
 */
static bool allow_inc = FALSE;

/*
 * Mega-Hack -- Last "increment" during haggling
 */
static s32b last_inc = 0L;


/*
 * Get a haggle
 */
static int get_haggle(cptr pmt, s32b * poffer, s32b price, int final)
{
	s32b i;

	cptr p;

	char buf[128];
	char out_val[160];


	/* Clear old increment if necessary */
	if (!allow_inc)
		last_inc = 0L;


	/* Final offer */
	if (final)
	{
		sprintf(buf, "%s [accept] ", pmt);
	}

	/* Old (negative) increment, and not final */
	else if (last_inc < 0)
	{
		sprintf(buf, "%s [-%ld] ", pmt, (long) (ABS(last_inc)));
	}

	/* Old (positive) increment, and not final */
	else if (last_inc > 0)
	{
		sprintf(buf, "%s [+%ld] ", pmt, (long) (ABS(last_inc)));
	}

	/* Normal haggle */
	else
	{
		sprintf(buf, "%s ", pmt);
	}


	/* Paranoia XXX XXX XXX */
	msg_print(NULL);


	/* Ask until done */
	while (TRUE)
	{
		/* Default */
		strcpy(out_val, "");

		/* Ask the user for a response */
		if (!get_string(buf, out_val, 32))
			return (FALSE);

		/* Skip leading spaces */
		for (p = out_val; *p == ' '; p++) /* loop */ ;

		/* Empty response */
		if (*p == '\0')
		{
			/* Accept current price */
			if (final)
			{
				*poffer = price;
				last_inc = 0L;
				break;
			}

			/* Use previous increment */
			if (allow_inc && last_inc)
			{
				*poffer += last_inc;
				break;
			}
		}

		/* Normal response */
		else
		{
			/* Extract a number */
			i = atol(p);

			/* Handle "incremental" number */
			if ((*p == '+' || *p == '-'))
			{
				/* Allow increments */
				if (allow_inc)
				{
					/* Use the given "increment" */
					*poffer += i;
					last_inc = i;
					break;
				}
			}

			/* Handle normal number */
			else
			{
				/* Use the given "number" */
				*poffer = i;
				last_inc = 0L;
				break;
			}
		}

		/* Warning */
		mprint(MSG_TEMP, "Invalid response.");
		msg_print(NULL);
	}

	/* Success */
	return (TRUE);
}


/*
 * Receive an offer (from the player)
 *
 * Return TRUE if offer is NOT okay
 */
static bool receive_offer(owner_type * ot_ptr, store_type * st_ptr,
	cptr pmt, s32b * poffer, s32b last_offer, int factor, s32b price,
	int final)
{
	/* Haggle till done */
	while (TRUE)
	{
		/* Get a haggle (or cancel) */
		if (!get_haggle(pmt, poffer, price, final))
			return (TRUE);

		/* Acceptable offer */
		if (((*poffer) * factor) >= (last_offer * factor))
			break;

		/* Insult, and check for kicked out */
		if (haggle_insults(ot_ptr, st_ptr))
			return (TRUE);

		/* Reject offer (correctly) */
		(*poffer) = last_offer;
	}

	/* Success */
	return (FALSE);
}



/*
 * Shuffle one of the stores.
 */
void store_shuffle(int which)
{
	int j;

	store_type *st_ptr;
	object_type *o_ptr;

	/* Ignore home */
	if (which == ST_INFO_HOME)
		return;

	/* Activate that store */
	st_ptr = &store[which];

	/* Pick a new owner */
	for (j = st_ptr->owner; j == st_ptr->owner;)
	{
		st_ptr->owner = rand_int(MAX_OWNERS);
	}

	/* Reset the owner data */
	st_ptr->insult_cur = 0;
	st_ptr->store_open = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;


	/* Hack -- discount all the items */
	for (o_ptr = st_ptr->stock; o_ptr != NULL; o_ptr = o_ptr->next_global)
	{

		/* Hack -- Sell all old items for "half price" */
		o_ptr->discount = 50;

		/* Hack -- Items are no longer "fixed price" */
		o_ptr->ident &= ~(IDENT_FIXED);

		/* Mega-Hack -- Note that the item is "on sale" */
		o_ptr->note = quark_add("on sale");
	}
}


/*
 * Initialize the stores
 */
void store_init(int which)
{
	/* Activate that store */
	store_type *st_ptr = &store[which];

	/* Pick an owner */
	st_ptr->owner = rand_int(MAX_OWNERS);

	/* Initialize the store */
	st_ptr->store_open = 0;
	st_ptr->insult_cur = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;

	/* Nothing in stock */
	st_ptr->stock = NULL;
}


/*
 * Display a single store item
 */
static void display_entry(object_type * o_ptr, int row)
{
	store_type *st_ptr = st_cur->st_ptr;
	owner_type *ot_ptr = st_cur->ot_ptr;

	s32b x;

	char o_name[80];
	char out_val[160];

	int maxwid;


	/* Label it, clear the line --(-- */
	sprintf(out_val, "%c) ", I2A(row - 6));
	prt(out_val, row, 0);

	/* Describe an object in the home */
	if (p_ptr->s_idx == ST_INFO_HOME)
	{
		byte attr;

		maxwid = 75;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights)
			maxwid -= 10;

		/* XXX Hack -- Do not display gold in description */
		object_desc_mode |= ODESC_GOLD;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);
		o_name[maxwid] = '\0';

		/* Acquire inventory color */
		attr = tval_to_attr[o_ptr->tval & 0x7F];

		/* Display the object */
		c_put_str(attr, o_name, row, 3);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of a single object */
			int wgt = o_ptr->weight / o_ptr->number;
			sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
			put_str(out_val, row, screen_x - 12);
		}
	}

	/* Describe an object (fully) in a store */
	else
	{
		byte attr;

		/* Must leave room for the "price" */
		maxwid = 65;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights)
			maxwid -= 7;

		/* XXX Hack -- Do not display gold in description */
		object_desc_mode |= ODESC_GOLD;

		/* Describe the object (fully) */
		object_desc(o_name, o_ptr, TRUE, 3);
		o_name[maxwid] = '\0';

		/* Acquire inventory color */
		attr = tval_to_attr[o_ptr->tval & 0x7F];

		/* Display the object */
		c_put_str(attr, o_name, row, 3);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of a single object */
			int wgt = o_ptr->weight / o_ptr->number;
			sprintf(out_val, "%3d.%d", wgt / 10, wgt % 10);
			put_str(out_val, row, screen_x - 19);
		}

		/* Display the cost */
		if (o_ptr->ident & (IDENT_FIXED))
		{
			/* Extract the "minimum" price */
			x = price_item(ot_ptr, o_ptr, ot_ptr->min_inflate, FALSE);

			/* Actually draw the price (not fixed) */
			sprintf(out_val, "%9ld F", (long) x);
			put_str(out_val, row, screen_x - 12);
		}

		/* Display a "taxed" cost */
		else if (auto_haggle)
		{
			/* Extract the "minimum" price */
			x = price_item(ot_ptr, o_ptr, ot_ptr->min_inflate, FALSE);

			/* Hack -- Apply Sales Tax if needed */
			if (!noneedtobargain(st_ptr, x))
				x += x / 10;

			/* Actually draw the price (with tax) */
			sprintf(out_val, "%9ld  ", (long) x);
			put_str(out_val, row, screen_x - 12);
		}

		/* Display a "haggle" cost */
		else
		{
			/* Extrect the "maximum" price */
			x = price_item(ot_ptr, o_ptr, ot_ptr->max_inflate, FALSE);

			/* Actually draw the price (not fixed) */
			sprintf(out_val, "%9ld  ", (long) x);
			put_str(out_val, row, screen_x - 12);
		}
	}
}

/*
 * Display a store's inventory
 */
static void display_inventory(void)
{
	int i, k = 0;
	object_type *o_ptr;

	/* Display the current page */
	for (o_ptr = st_cur->pages[st_cur->page_cur]; o_ptr != NULL;
		o_ptr = o_ptr->next_global)
	{
		/* Stop when we run out of items */
		if (k >= st_cur->per_page)
			break;

		/* Display that line */
		display_entry(o_ptr, 6 + k);

		/* Count items */
		++k;
	}

	/* Remember number of displayed items */
	st_cur->page_cur_cnt = k;

	/* Erase the extra lines and the "more" prompt */
	for (i = k; i < st_cur->per_page + 1; i++)
		prt("", i + 6, 0);

	/* Assume "no current page" */
	put_str("           ", 5, 20);

	/* Visual reminder of "more items" */
	if (st_cur->page_cnt > 1)
	{
		char s[10] = "  more  ";

		if (st_cur->page_cur > 0)
			s[0] = '-';
		if (st_cur->page_cur < st_cur->page_cnt - 1)
			s[7] = '+';

		/* Show "more" reminder (after the last object) */
		prt(s, k + 6, 3);

		/* Indicate the "current page" */
		put_str(format("(Page %d/%d)", st_cur->page_cur + 1,
			st_cur->page_cnt), 5, 20);
	}
}

/*
 * Display players gold
 */
static void store_prt_gold(void)
{
	char out_val[64];

	prt("Gold Remaining: ", screen_y - 5, screen_x - 27);

	sprintf(out_val, "%9ld", (long) p_ptr->au);
	prt(out_val, screen_y - 5, screen_x - 12);
}

/*
 * Display store (after clearing screen)
 */
static void display_store(void)
{
	owner_type *ot_ptr = st_cur->ot_ptr;
	char buf[80];

	/* Clear screen */
	Term_clear();

	/* The "Home" is special */
	if (p_ptr->s_idx == ST_INFO_HOME)
	{
		/* Put the owner name */
		put_str("Your Home", 3, 30);

		/* Label the object descriptions */
		put_str("Item Description", 5, 3);

		/* If showing weights, show label */
		if (show_weights)
		{
			put_str("Weight", 5, screen_x - 10);
		}
	}

	/* Normal stores */
	else
	{
		cptr store_name =
			(f_name + f_info[FEAT_SHOP_HEAD + p_ptr->s_idx].name);
		cptr owner_name = (ot_ptr->owner_name);
		cptr race_name = race_info[ot_ptr->owner_race].title;

		/* Put the owner name and race */
		sprintf(buf, "%s (%s)", owner_name, race_name);
		put_str(buf, 3, 10);

		/* Show the max price in the store (above prices) */
		sprintf(buf, "%s (%ld)", store_name, (long) (ot_ptr->max_cost));
		prt(buf, 3, 50);

		/* Label the object descriptions */
		put_str("Item Description", 5, 3);

		/* If showing weights, show label */
		if (show_weights)
		{
			put_str("Weight", 5, screen_x - 20);
		}

		/* Label the asking price (in stores) */
		put_str("Price", 5, screen_x - 8);
	}

	/* Display the current gold */
	store_prt_gold();

	/* Draw in the inventory */
	display_inventory();
}


static void get_store_item_aux(object_type * o_ptr, store_type * st_ptr)
{
	object_type *ret = NULL;
	int num = 1;

	/* Paranoia. */
	if (o_ptr == NULL)
		return;

	prt("", 0, 0);

	if ((o_ptr->number > 1) && tval_can_stack(o_ptr->tval))
	{
		num = get_quantity("How many? ", o_ptr->number);

		if (!num)
			return;
	}

	if (num < o_ptr->number)
	{
		/* XXX Hack -- object_unabsorb() should not combine items */
		store_combine_flag = FALSE;

		/* Split the object */
		ret = object_unabsorb(o_ptr, num);

		/* XXX Hack -- undo the hack above */
		store_combine_flag = TRUE;
	}
	else
	{
		ret = o_ptr;
	}

	(void) inven_carry(ret);

	/* Split the stack into screenfuls */
	st_cur->page_cnt =
		make_stack_pages(st_ptr->stock, st_cur->pages,
		st_cur->per_page, TRUE);

	/* Check the displayed page */
	if (st_cur->page_cnt < 1)
	{
		st_cur->page_cur = 0;
	}
	else if (st_cur->page_cur >= st_cur->page_cnt)
	{
		--st_cur->page_cur;
	}

	/* Redisplay wares */
	display_inventory();
}


static bool item_tester_hook_sell(object_type * o_ptr)
{
	int i;

	for (i = 0; i < EQUIP_MAX; i++)
	{
		if ((equipment[i] == o_ptr) && (cursed_p(o_ptr) ||
				show_inven_equip == FALSE))
		{
			return FALSE;
		}
	}

	return store_will_buy(o_ptr);
}


/*
 *
 */
static void clean_floor_items(void)
{
	int y, x;

	/* Remove items on the floor. */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			cave_o_idx[y][x] = NULL;
		}
	}
}


/*
 * Haggling routine XXX XXX
 *
 * Return TRUE if purchase is NOT successful
 */
static s32b sell_haggle(store_type * st_ptr, object_type * o_ptr)
{
	s32b purse, cur_ask, final_ask;
	s32b last_offer = 0, offer = 0;
	s32b x1, x2, x3;
	s32b min_per, max_per, price;

	int flag, loop_flag, noneed;
	int annoyed = 0, final = FALSE;

	bool ignore = FALSE;

	bool cancel = FALSE;

	cptr pmt = "Offer";

	char out_val[160];

	owner_type *ot_ptr = &owners[p_ptr->s_idx][st_ptr->owner];


	price = 0;


	/* Obtain the starting offer and the final offer */
	cur_ask = price_item(ot_ptr, o_ptr, ot_ptr->max_inflate, TRUE);
	final_ask = price_item(ot_ptr, o_ptr, ot_ptr->min_inflate, TRUE);

	/* Determine if haggling is necessary */
	noneed = noneedtobargain(st_ptr, final_ask);

	/* Get the owner's payout limit */
	purse = (s32b) (ot_ptr->max_cost);

	/* No need to haggle */
	if (noneed || auto_haggle || (final_ask >= purse))
	{
		/* No reason to haggle */
		if (final_ask >= purse)
		{
			/* Message */
			mprint(MSG_TEMP, "You instantly agree upon the price.");
			msg_print(NULL);

			/* Ignore haggling */
			ignore = TRUE;

			/* Offer full purse */
			final_ask = purse;
		}

		/* No need to haggle */
		else if (noneed)
		{
			/* Message */
			mprint(MSG_TEMP, "You eventually agree upon the price.");
			msg_print(NULL);
		}

		/* No haggle option */
		else
		{
			/* Message summary */
			mprint(MSG_TEMP, "You quickly agree upon the price.");
			msg_print(NULL);

			/* Ignore haggling */
			ignore = TRUE;

			/* Apply Sales Tax */
			final_ask -= final_ask / 10;
		}

		/* Final price */
		cur_ask = final_ask;

		/* Final offer */
		final = TRUE;
		pmt = "Final Offer";
	}

	/* Haggle for the whole pile */
	cur_ask *= o_ptr->number;
	final_ask *= o_ptr->number;


	/* XXX XXX XXX Display commands */

	/* Haggling parameters */
	min_per = ot_ptr->haggle_per;
	max_per = min_per * 3;

	/* Mega-Hack -- artificial "last offer" value */
	last_offer = object_value(o_ptr) * o_ptr->number;
	last_offer = last_offer * ot_ptr->max_inflate / 100L;

	/* No offer yet */
	offer = 0;

	/* No incremental haggling yet */
	allow_inc = FALSE;

	/* Haggle */
	for (flag = FALSE; !flag;)
	{
		while (1)
		{
			loop_flag = TRUE;

			sprintf(out_val, "(%s: %ld) What price do you ask? ", pmt,
				(long) cur_ask);
			cancel =
				receive_offer(ot_ptr, st_ptr, out_val, &offer, last_offer,
				-1, cur_ask, final);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer < cur_ask)
			{
				say_comment_6();
				/* rejected, reset offer for incremental haggling */
				offer = last_offer;
			}
			else if (offer == cur_ask)
			{
				flag = TRUE;
				price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}

			/* Stop */
			if (flag || !loop_flag)
				break;
		}

		if (!flag)
		{
			x1 = 100 * (last_offer - offer) / (last_offer - cur_ask);
			if (x1 < min_per)
			{
				if (haggle_insults(ot_ptr, st_ptr))
				{
					flag = TRUE;
					cancel = TRUE;
				}
			}
			else if (x1 > max_per)
			{
				x1 = x1 * 3 / 4;
				if (x1 < max_per)
					x1 = max_per;
			}

			x2 = rand_range(x1 - 2, x1 + 2);
			x3 = ((offer - cur_ask) * x2 / 100L) + 1;
			/* don't let the price go down */
			if (x3 < 0)
				x3 = 0;
			cur_ask += x3;

			if (cur_ask > final_ask)
			{
				cur_ask = final_ask;
				final = TRUE;
				pmt = "Final Offer";
				annoyed++;
				if (annoyed > 3)
				{
					flag = TRUE;
					increase_insults(ot_ptr, st_ptr);
				}
			}
			else if (offer <= cur_ask)
			{
				flag = TRUE;
				price = offer;
			}

			if (!flag)
			{
				last_offer = offer;
				allow_inc = TRUE;
				say_comment_3(cur_ask, annoyed);
			}
		}
	}

	/* Cancel */
	if (cancel)
		return 0;

	/* Update haggling info */
	if (!ignore)
	{
		/* Update haggling info */
		updatebargain(st_ptr, price, final_ask);
	}

	/* Do not cancel */
	return price;
}


/*
 * Buy an object from a store
 */
static void store_purchase(void)
{
	object_type *o_ptr;
	char out_val[160];

	/* Empty? */
	if (st_cur->st_ptr->stock == NULL)
	{
		if (p_ptr->s_idx == ST_INFO_HOME)
		{
			msg_print("Your home is empty.");
		}
		else
		{
			msg_print("I am currently out of stock.");
		}
		return;
	}

	/* Prompt */
	if (p_ptr->s_idx == ST_INFO_HOME)
	{
		sprintf(out_val, "Which item do you want to take? ");
	}
	else
	{
		sprintf(out_val, "Which item are you interested in? ");
	}

	/* Get the object to be bought */
	o_ptr = get_stock(out_val);

	/* Cancel */
	if (o_ptr == NULL)
		return;

	/* Haggle, etc */
	get_store_item_aux(o_ptr, st_cur->st_ptr);
}


/*
 * Sell an object to the store (or home)
 */
static void store_sell(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Home */
	if (p_ptr->s_idx == ST_INFO_HOME)
	{
		/* Prompt */
		q = "Drop which item";
		s = "You have no inventory.";
	}

	/* Real store */
	else
	{
		/* Prompt */
		q = "Sell which item";
		s = "You have nothing that I want.";

		/* Only allow items the store will buy */
		item_tester_hook = item_tester_hook_sell;
	}

	/* Get an item */
	o_ptr =
		get_item(q, s, p_ptr->py, p_ptr->px,
		(USE_INVEN | USE_REMOVE | USE_BY_PARTS));

	/* Cancel */
	if (!o_ptr)
		return;

	/* Sell it. */
	if (store_sell_item(o_ptr, st_cur->st_ptr))
	{
		/* Worthless object */
		if (object_value(o_ptr) <= 0)
		{
			/* Not in the Home */
			if (p_ptr->s_idx != ST_INFO_HOME)
			{
				/* Destroy the object */
				remove_object(o_ptr);

				/* Done */
				return;
			}
		}

		/* Remove from list of dungeon objects */
		remove_from_global_list(o_ptr, &(o_list));

		/* Add to store inventory */
		insert_to_global_list(o_ptr, &(st_cur->st_ptr->stock),
			(p_ptr->s_idx == ST_INFO_HOME) ? WORLD_HOME : WORLD_STORE);

		/* Split the stack into screenfuls */
		st_cur->page_cnt =
			make_stack_pages(st_cur->st_ptr->stock, st_cur->pages,
			st_cur->per_page, TRUE);

		/* Check the displayed page */
		if (st_cur->page_cnt < 1)
		{
			st_cur->page_cur = 0;
		}
		else if (st_cur->page_cur >= st_cur->page_cnt)
		{
			--st_cur->page_cur;
		}

		/* Redisplay the wares */
		display_inventory();
	}

	/* Put it back in the inventory */
	else
	{
		inven_carry(o_ptr);
	}
}


/*
 * Haggling routine XXX XXX
 */
static s32b purchase_haggle(store_type * st_ptr, object_type * o_ptr)
{
	s32b cur_ask, final_ask;
	s32b last_offer, offer;
	s32b x1, x2, x3;
	s32b min_per, max_per, price;
	int flag, loop_flag, noneed;
	int annoyed = 0, final = FALSE;

	bool ignore = FALSE;

	bool cancel = FALSE;

	cptr pmt = "Asking";

	char out_val[160];

	owner_type *ot_ptr = &owners[p_ptr->s_idx][st_ptr->owner];

	price = 0;


	/* Extract the starting offer and final offer */
	cur_ask = price_item(ot_ptr, o_ptr, ot_ptr->max_inflate, FALSE);
	final_ask = price_item(ot_ptr, o_ptr, ot_ptr->min_inflate, FALSE);

	/* Determine if haggling is necessary */
	noneed = noneedtobargain(st_ptr, final_ask);

	/* No need to haggle */
	if (auto_haggle || noneed || (o_ptr->ident & (IDENT_FIXED)))
	{
		/* Already haggled */
		if (o_ptr->ident & (IDENT_FIXED))
		{
			/* Message summary */
			mprint(MSG_TEMP, "You instantly agree upon the price.");
			msg_print(NULL);
		}

		/* No need to haggle */
		else if (noneed)
		{
			/* Message summary */
			mprint(MSG_TEMP, "You eventually agree upon the price.");
			msg_print(NULL);
		}

		/* Auto-haggle */
		else
		{
			/* Message summary */
			mprint(MSG_TEMP, "You quickly agree upon the price.");
			msg_print(NULL);

			/* Ignore haggling */
			ignore = TRUE;

			/* Apply sales tax */
			final_ask += (final_ask / 10);
		}

		/* Jump to final price */
		cur_ask = final_ask;

		/* Go to final offer */
		pmt = "Final Offer";
		final = TRUE;
	}


	/* Haggle for the whole pile */
	cur_ask *= o_ptr->number;
	final_ask *= o_ptr->number;


	/* Haggle parameters */
	min_per = ot_ptr->haggle_per;
	max_per = min_per * 3;

	/* Mega-Hack -- artificial "last offer" value */
	last_offer = object_value(o_ptr) * o_ptr->number;
	last_offer = last_offer * (200 - (int) (ot_ptr->max_inflate)) / 100L;
	if (last_offer <= 0)
		last_offer = 1;

	/* No offer yet */
	offer = 0;

	/* No incremental haggling yet */
	allow_inc = FALSE;

	/* Haggle until done */
	for (flag = FALSE; !flag;)
	{
		loop_flag = TRUE;

		while (!flag && loop_flag)
		{
			sprintf(out_val, "(%s: %ld) What do you offer? ", pmt,
				(long) cur_ask);
			cancel =
				receive_offer(ot_ptr, st_ptr, out_val, &offer, last_offer,
				1, cur_ask, final);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer > cur_ask)
			{
				say_comment_6();
				offer = last_offer;
			}
			else if (offer == cur_ask)
			{
				flag = TRUE;
				price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}
		}

		if (!flag)
		{
			x1 = 100 * (offer - last_offer) / (cur_ask - last_offer);

			if (x1 < min_per)
			{
				if (haggle_insults(ot_ptr, st_ptr))
				{
					flag = TRUE;
					cancel = TRUE;
				}
			}
			else if (x1 > max_per)
			{
				x1 = x1 * 3 / 4;
				if (x1 < max_per)
					x1 = max_per;
			}

			x2 = rand_range(x1 - 2, x1 + 2);
			x3 = ((cur_ask - offer) * x2 / 100L) + 1;
			/* don't let the price go up */
			if (x3 < 0)
				x3 = 0;
			cur_ask -= x3;

			/* Too little */
			if (cur_ask < final_ask)
			{
				final = TRUE;
				cur_ask = final_ask;
				pmt = "Final Offer";
				annoyed++;

				if (annoyed > 3)
				{
					increase_insults(ot_ptr, st_ptr);
					cancel = TRUE;
					flag = TRUE;
				}

			}
			else if (offer >= cur_ask)
			{
				flag = TRUE;
				price = offer;
			}

			if (!flag)
			{
				last_offer = offer;
				allow_inc = TRUE;
				say_comment_2(cur_ask, annoyed);
			}
		}
	}

	/* Mark price as fixed */
	if (!ignore && (price == final_ask))
	{
		/* Mark as fixed price */
		o_ptr->ident |= (IDENT_FIXED);
	}

	/* Cancel */
	if (cancel)
		return 0;

	/* Update haggling info */
	if (!ignore)
	{

		/* Update haggling info */
		updatebargain(st_ptr, price, final_ask);
	}

	/* Do not cancel */
	return price;
}


/*
 * Sell an item to the store.
 */
bool store_sell_item(object_type * o_ptr, store_type * st_ptr)
{
	s32b price, value, dummy;
	char o_name[80];

	/* No money for the home. */
	if (p_ptr->s_idx == ST_INFO_HOME)
	{
		return TRUE;
	}

	/* Crappy item. */
	if (!store_will_buy(o_ptr))
	{
		mprint(MSG_TEMP, "This item is worthless.");
		msg_print(NULL);
		return FALSE;
	}

	/* Describe the object (fully) */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	mformat(MSG_TEMP, "Selling %s.", o_name);
	msg_print(NULL);

	/* Haggle for a final price */
	price = sell_haggle(st_ptr, o_ptr);

	/* Hack -- Got kicked out */
	if (st_ptr->store_open >= turn)
		return FALSE;

	/* Player cancelled purchase. */
	if (price == 0)
	{
		return FALSE;
	}

	/* Say "okay" */
	say_comment_1();

	/* Be happy */
	decrease_insults(st_ptr);

	/* Get some money */
	p_ptr->au += price;

	/* Update the display */
	store_prt_gold();

	/* Get the "apparent" value */
	dummy = object_value(o_ptr) * o_ptr->number;

	/* Hack -- selling an item makes you aware of it */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Get the "actual" value */
	value = object_value(o_ptr) * o_ptr->number;

	/* Describe the transaction */
	object_desc_store(o_name, o_ptr, TRUE, 3);

	/* Message */
	mformat(MSG_TEMP, "You sold %s for %ld gold.", o_name, (long) price);

	/* Analyze the prices (and comment verbally) */
	purchase_analyze(price, value, dummy);

	/* Erase the inscription */
	o_ptr->note = 0;

	/* Handle stuff */
	handle_stuff();

	return TRUE;
}


/*
 * Sell the item to the player.
 */
bool store_buy_item(object_type * o_ptr, store_type * st_ptr)
{
	s32b price;
	char o_name[80];

	/* Stuff in the home is always free. */
	if (p_ptr->s_idx == ST_INFO_HOME)
	{
		return TRUE;
	}

	/* Describe the object (fully) */
	object_desc_store(o_name, o_ptr, TRUE, 3);

	/* Message */
	mformat(MSG_TEMP, "Buying %s.", o_name);
	msg_print(NULL);

	/* Haggle for a final price */
	price = purchase_haggle(st_ptr, o_ptr);

	/* Hack -- Got kicked out */
	if (st_ptr->store_open >= turn)
		return FALSE;

	/* Player cancelled purchase. */
	if (price == 0)
	{
		return FALSE;
	}

	/* Player can't afford it */
	if (p_ptr->au < price)
	{
		mprint(MSG_TEMP, "You cannot afford this.");
		msg_print(NULL);
		return FALSE;
	}

	/* Say "okay" */
	say_comment_1();

	/* Be happy */
	decrease_insults(st_ptr);

	/* Spend the money */
	p_ptr->au -= price;

	/* Update the display */
	store_prt_gold();

	/* Hack -- buying an item makes you aware of it */
	object_aware(o_ptr);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Hack -- clear the "fixed" flag from the item */
	o_ptr->ident &= ~(IDENT_FIXED);

	/* Describe the transaction */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	mformat(MSG_TEMP, "You bought %s for %ld gold.", o_name, (long) price);

	/* Erase the inscription */
	o_ptr->note = 0;

	/* Handle stuff */
	handle_stuff();

	return TRUE;
}


/*
 * Return the price of an item, including special handling when the player is 
 * in a store.
 */
s32b object_store_value(object_type * o_ptr)
{
	/* Hack -- value of gold. */
	if (o_ptr->tval == TV_GOLD)
	{
		return o_ptr->pval;
	}

	if (p_ptr->s_idx < 0 || p_ptr->s_idx == ST_INFO_HOME)
	{
		return object_value(o_ptr);

	}
	else
	{
		s32b x;

		store_type *st_ptr = &store[p_ptr->s_idx];
		owner_type *ot_ptr = &owners[p_ptr->s_idx][st_ptr->owner];

		/* Figure out the cost in a shop. */

		if (o_ptr->ident & (IDENT_FIXED))
		{
			/* Extract the "minimum" price */
			x = price_item(ot_ptr, o_ptr, ot_ptr->min_inflate, FALSE);

		}
		else if (auto_haggle)
		{
			/* Extract the "minimum" price */
			x = price_item(ot_ptr, o_ptr, ot_ptr->min_inflate, FALSE);

			/* Hack -- Apply Sales Tax if needed */
			if (!noneedtobargain(st_ptr, x))
				x += x / 10;
		}
		else
		{
			/* Extrect the "maximum" price */
			x = price_item(ot_ptr, o_ptr, ot_ptr->max_inflate, FALSE);

		}

		return x;
	}
}


/*
 * Observe an store item
 */
static void store_observe(void)
{
	object_type *o_ptr;
	char o_name[80];
	char out_val[160];

	/* Prompt */
	sprintf(out_val, "Examine which item? ");

	/* Choose an item */
	o_ptr = get_stock("Examine which item? ");

	/* Cancel */
	if (!o_ptr) return;

	/* Hack -- Examine parchment */
	if (o_ptr->tval == TV_TEXT)
	{
		show_book_number(o_ptr->sval);
		return;
	}

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	msg_format("Examining %s...", o_name);

	/* Describe it fully */
	identify_fully_aux(o_ptr);
}


/*
 * Process a command in a store
 */
static void store_process_command(void)
{
	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
			/* Leave */
		case ESCAPE:
		{
			st_cur->leave = TRUE;
			break;
		}

		/* Browse (with wrap) */
		case ' ':
		{
			if (st_cur->page_cnt == 1)
			{
				/* Nothing to see */
				mprint(MSG_TEMP, "Entire inventory is shown.");
			}

			else
			{
				/* Advance, wrap if needed */
				if (++st_cur->page_cur >= st_cur->page_cnt)
					st_cur->page_cur = 0;
					
				/* Redisplay wares */
				display_inventory();
			}

			break;
		}

		/* Previous page */
		case '-':
		{
			if (st_cur->page_cur > 0)
			{
				/* Go back a page */
				--st_cur->page_cur;

				/* Redisplay wares */
				display_inventory();
			}
			else
			{
				bell();
			}
			break;
		}

		/* Next page */
		case '+':
		case '=':
		{
			if (st_cur->page_cur < st_cur->page_cnt - 1)
			{
				/* Go forward a page */
				++st_cur->page_cur;

				/* Redisplay wares */
				display_inventory();
			}
			else
			{
				bell();
			}
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

			/* Ignore return */
		case '\r':
		{
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

#if 0

			/* Drop an item */
		case 'd':
		{
			do_cmd_drop();
			break;
		}

#endif

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
			store_observe();
			break;
		}

#if 0
			/* Hack -- toggle windows */
		case KTRL('E'):
		{
			toggle_inven_equip();
			break;
		}
#endif


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
			(void) Term_user(0);
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

#if 0
			/* Interact with options */
		case '=':
		{
			do_cmd_options();
			do_cmd_redraw();
			display_store();
			break;
		}
#endif

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
 */
void do_cmd_store(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int which;
	store_type *st_ptr;
	s16b tmp_chr;


	/* Leave the shop. */
	if (vault_shops && (p_ptr->inside_special == SPECIAL_STORE))
	{
		p_ptr->load_dungeon = MAX_DEPTH + 1;
		p_ptr->leaving = TRUE;

		/* Remove the junk we scattered. */
		clean_floor_items();

		return;
	}

	/* Verify a store */
	if (!((cave_feat[py][px] >= FEAT_SHOP_HEAD) &&
			(cave_feat[py][px] <= FEAT_SHOP_TAIL)))
	{
		mprint(MSG_TEMP, "You see no store here.");
		return;
	}

	/* Hack -- Extract the store code */
	which = (cave_feat[py][px] - FEAT_SHOP_HEAD);

	/* Save the store index for other functions. */
	p_ptr->s_idx = which;

	/* Hack -- Check the "locked doors" */
	if (store[which].store_open >= turn)
	{
		mprint(MSG_TEMP, "The doors are locked.");
		return;
	}

	/* Generate a shop vault. */
	if (vault_shops)
	{
		/* Save the level. */
		if (save_dungeon(MAX_DEPTH))
		{
			mprint(MSG_ERROR, "Could not save temporary dungeon!");
		}

		p_ptr->inside_special = SPECIAL_STORE;
		p_ptr->leaving = TRUE;

		/* Done */
		return;
	}

	/* Hack -- Increase "icky" depth */
	character_icky++;

	/* Access the store */
	st_ptr = &store[which];

	/* Current store */
	st_cur->st_ptr = st_ptr;

	/* Current owner */
	st_cur->ot_ptr = &owners[which][st_ptr->owner];

	/* Number of items per page */
	st_cur->per_page = screen_y - 12;

	/* XXX Hack -- Only a-z possible */
	if (st_cur->per_page > 26)
		st_cur->per_page = 26;

	/* Display the first page */
	st_cur->page_cur = 0;

	/* We aren't leaving */
	st_cur->leave = FALSE;

	/* Split the stack into screenfuls */
	st_cur->page_cnt =
		make_stack_pages(st_ptr->stock, st_cur->pages, st_cur->per_page,
		TRUE);

	/* Display the store */
	display_store();

	/* Interact with player */
	while (!st_cur->leave)
	{
		/* Hack -- Check the charisma */
		tmp_chr = p_ptr->stat_use[A_CHR];

		/* Clear */
		clear_from(screen_y - 3);

		/* Basic commands */
		prt(" ESC) Exit from Building.", screen_y - 2, 0);

		/* Browse if necessary */
		if (st_cur->page_cnt > 1)
		{
			prt(" -/+/space) Browse stock.", screen_y - 1, 0);
		}

		/* Commands */
		prt(" g) Get/Purchase an item.", screen_y - 2, screen_x / 3);
		prt(" d) Drop/Sell an item.", screen_y - 1, screen_x / 3);

		prt(" I) Inspect an item.", screen_y - 2, screen_x / 3 * 2);

		/* Prompt */
		prt("You may: ", screen_y - 3, 0);

		/* Get a command */
		request_command(TRUE);

		/* Process the command */
		store_process_command();

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Hack -- Handle charisma changes */
		if (tmp_chr != p_ptr->stat_use[A_CHR])
		{
			/* Redisplay wares */
			display_inventory();
		}

		/* Hack -- get kicked out of the store */
		if (st_ptr->store_open >= turn)
			st_cur->leave = TRUE;
	}

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Redraw */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP);
}


/*
 * ST_INFO.TXT code: initialisation and usage
 */

/*
 * Read in a range of numbers
 */
static cptr read_number_range(cptr buf, int *from, int *to)
{
	/* Clear variables */
	*from = 0;
	*to = 0;

	/* Read in 'from' */
	while (*buf >= '0' && *buf <= '9')
	{
		*from = ((*from) * 10) + D2I(*buf);
		buf++;
	}

	/* Is this a range or just a number? */
	if (*buf != '-')
	{
		*to = *from;
		return buf;
	}

	buf++;

	/* Read in 'to' */
	while (*buf >= '0' && *buf <= '9')
	{
		*to = ((*to) * 10) + D2I(*buf);
		buf++;
	}

	/* Sanity check */
	if (*from > *to)
	{
		int tmp;

		tmp = *from;
		*from = *to;
		*to = tmp;
	}

	/* Return new string position */
	return buf;
}

/*
 * Check there is a current (valid) shop
 */
static void check_shop_range(int line_num)
{
	if (st_info_shop < 0 || st_info_shop >= MAX_STORES)
	{
		msg_format
			("'st_info.txt' error: Shop out of range or no shop on line %d.",
			line_num);
		msg_print(NULL);
		quit("Error in 'st_info.txt' file.");
	}
}

/*
 * Process a 'buying information' line
 */
static void process_buy_line(cptr line, int line_num)
{
	int from, to, i;

	/* Check shop is in range */
	check_shop_range(line_num);

	while (*line)
	{
		/* Read in the number range */
		line = read_number_range(line, &from, &to);

		/* Check this is in range */
		if (from > MAX_K_IDX || to > MAX_K_IDX)
		{
			msg_format
				("'st_info.txt' error: Range (%d-%d) is not an object (line %d).",
				from, to, line_num);
			msg_print(NULL);
			quit("Error in 'st_info.txt' file.");
		}

		/* Apply */
		for (i = from; i <= to; i++)
			buy_sell_info[st_info_shop].buy[i] = TRUE;

		/* Next number? */
		if (*line)
			line++;
	}
}

/*
 * Process a 'selling information' line
 */
static void process_sell_line(cptr line, int line_num)
{
	int from, to, i, i_val;

	/* Check shop is in range */
	check_shop_range(line_num);

	while (*line)
	{
		/* Read in the number range */
		line = read_number_range(line, &from, &to);

		/* Check this is in range */
		if (from > MAX_K_IDX || to > MAX_K_IDX)
		{
			msg_format
				("'st_info.txt' error: Range (%d-%d) is not an object (line %d).",
				from, to, line_num);
			msg_print(NULL);
			quit("Error in 'st_info.txt' file.");
		}

		/* Check format */
		if (*line != ':')
		{
			msg_format
				("'st_info.txt' error: malformed expression '%s' (line %d).",
				line, line_num);
			msg_print(NULL);
			quit("Error in 'st_info.txt' file.");
		}

		/* Read in i_val */
		i_val = 0;
		line++;
		while (*line >= '0' && *line <= '9')
		{
			i_val = (i_val * 10) + D2I(*line);
			line++;
		}

		/* Apply */
		for (i = from; i <= to; i++)
			buy_sell_info[st_info_shop].sell[i] = i_val;

		/* Next expression */
		while (*line && (*line < '0' || *line > '9'))
			line++;
	}
}

/*
 * Process a 'vault' line
 */
static void process_vault_line(cptr line, int line_num)
{
	int vault_idx;

	check_shop_range(line_num);

	/* This should simply be a number, so extract it */
	vault_idx = atoi(line);

	/* Check validity */
	if (vault_idx < 0 || vault_idx >= MAX_V_IDX)
	{
		msg_format
			("'st_info.txt': Error in line %d, '%s' isn't a valid vault.",
			line_num, line);
		msg_print(NULL);
		quit("Error in 'st_info.txt' file.");
	}

	/* Set */
	store[st_info_shop].vault = vault_idx;
}

/*
 * Process an 'owner information' line
 */
static void process_owner_line(cptr line, int line_num)
{
	/* &FIXME& Owners not implemented */
	return;
}

/*
 * Process a 'next shop' line
 */
static void process_new_line(cptr line, int line_num)
{
	int new_shop;

	/* This should simply be a number, so extract it */
	new_shop = atoi(line) - 1;

	/* Check validity */
	if (new_shop < 0 || new_shop >= MAX_STORES)
	{
		msg_format
			("'st_info.txt': Error in line %d, '%s' is not a valid shop.",
			line_num, line);
		msg_print(NULL);
		quit("Error in 'st_info.txt' file.");
	}

	/* Set */
	st_info_shop = new_shop;
}

/*
 * Function to strip whitespace and comments from a line
 */
static char *strip_whitespace(char *target)
{
	char *pos = NULL;
	int i;

	/* Remove comments */
	for (i = 0; target[i]; i++)
	{
		if (target[i] == '#')
		{
			target[i] = 0;
			target[i + 1] = 0;
		}
	}

	/* Remove whitespace at the start */
	for (i = 0; target[i]; i++)
		if (target[i] != ' ' && target[i] != '\t')
			break;

	/* Update start of string */
	pos = target + i;

	/* Remove whitespace at end */
	for (i = strlen(target) - 1; i >= 0; i--)
	{
		if (target[i] != ' ' && target[i] != '\t')
			break;

		target[i] = 0;
	}

	/* Return start position */
	return pos;
}

/*
 * Main initialisation function.
 */
void init_st_info(void)
{
	FILE *fp;
	char buf[1024], *buf_ptr;
	int line_num = 0, i, j;

	/* Clean array */
	for (i = 0; i < MAX_STORES; i++)
		for (j = 0; j < MAX_K_IDX; j++)
			buy_sell_info[i].buy[j] = buy_sell_info[i].sell[j] = 0;

	/* Get filename, open file */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "st_info.txt");
	fp = my_fopen(buf, "r");
	if (!fp)
	{
		msg_format("'st_info.txt' error: could not open file.");
		msg_print(NULL);
		quit("Could not open 'st_info.txt' file.");
	}

	/* Process lines */
	while (my_fgets(fp, buf, 1024) == 0)
	{
		/* Next line */
		line_num++;

		/* Remove whitespace and comments */
		buf_ptr = strip_whitespace(buf);

		/* Skip blank lines */
		if (*buf_ptr == 0)
			continue;

		/* Check line format */
		if (*(buf_ptr + 1) != ':')
		{
			msg_format
				("'st_info.txt' error: unrecognised line format (line %d), '%s'.",
				line_num, buf);
			msg_print(NULL);
			quit("Error in 'st_info.txt' file.");
		}

		/* Process line contents */
		switch (*buf_ptr)
		{
				/* New shop */
			case 'N':
				process_new_line(buf_ptr + 2, line_num);
				break;

				/* Buying information */
			case 'B':
				process_buy_line(buf_ptr + 2, line_num);
				break;

				/* Selling information */
			case 'S':
				process_sell_line(buf_ptr + 2, line_num);
				break;

				/* Vault information (header) */
			case 'V':
				process_vault_line(buf_ptr + 2, line_num);
				break;

				/* Owner information */
			case 'O':
				process_owner_line(buf_ptr + 2, line_num);
				break;

				/* Unknown */
			default:
				msg_format
					("'st_info.txt' error: unknown line token '%c' (line %d).",
					buf[0], line_num);
				msg_print(NULL);
				quit("Error in 'st_info.txt' file.");
		}
	}

	my_fclose(fp);
}
