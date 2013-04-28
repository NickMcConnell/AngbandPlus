/* File: store.c */

/*
 * Store tables (see also "lib/edit/store.txt").  Store owner speech.
 * React to a purchase, price stock, determine if stock items are similar
 * and combine them.  Determine if a store will buy something, manage
 * store stock.  Create, discount, tweak, and delete stock items.  Get
 * markup, display a store and its stock.  Choose an item in stock.  The
 * haggling code.  Buy and sell stuff, interact with a store, process
 * commands while in a store.  Initialize and maintain stores.
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
 * We store the current "store number" here so everyone can access it
 */
static int store_num = 7;

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
 * Number of stock items shown on screen at any one time.
 */
static int stock_displayed = (STORE_INVEN_MAX + 1) / 2;

/*
 * Stock index and quantity of item being purchased.
 */
static int item_buy_idx = -1;
static int item_buy_num =  0;

/*
 * Hack -- set this to leave the store
 */
static bool leave_store = FALSE;


/*
 * The higher this number is, the easier it is to bargain.  Values much
 * below 100 will usually make it impossible to bargain successfully.
 *
 * Note that this number and the tables below are overwritten by the
 * values in "lib/edit/store.txt".
 */
byte bargain_difficulty = 108;


/*
 * Range of shopkeeper attitudes (affects haggling text).  If tolerance
 * is at least always_polite, the shopkeeper is always relatively polite.
 */
#define ALWAYS_POLITE  25
#define ALWAYS_RUDE     5


/*
 * Store arrays.  Filled in by /lib/edit/store.txt.
 */
owner_type owners[MAX_STORES][MAX_OWNERS];
stock_type store_stock[STORE_STOCK_SIZE];
byte tval_sell[MAX_STORES][10];
byte rgold_adj[MAX_RACES][MAX_RACES];


/*
 * Get the short name of a shopkeeper.
 */
static void shopkeeper_short_name(char *short_name, cptr full_name)
{
	int i;

	/* Get the full shopkeeper name */
	strcpy(short_name, full_name);

	/* Scan to the end of the name or a space */
	for (i = 1; short_name[i] && short_name[i] != ' '; i++);

	/* Truncate the name */
	short_name[i] = '\0';
}


/*
 * Shopkeeper comment (avoids the "(+)"s from appearing)
 *
 * See "msg_format()".
 */
static void comment(cptr fmt, ...)
{
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Hack -- Flush any pending messages first  XXX */
	message_flush();

	/* Display some text */
	prt("", 0, 0);
	put_str(buf, 0, 0);
	(void)inkey(ALLOW_CLICK);
	prt("", 0, 0);
}



#define MAX_COMMENT_WELCOME	11


/*
 * Shopkeeper welcome messages.
 *
 * The shopkeeper's name must come first, then the character's name.
 */
static cptr comment_welcome[MAX_COMMENT_WELCOME] =
{
	"",
	"%s nods to you.",
	"%s says hello.",
	"%s: \"See anything you like, adventurer?\"",
	"%s: \"How may I help you, %s?\"",
	"%s: \"%s.  Good to see you.\"",
	"%s: \"Welcome back, %s.\"",
	"%s: \"A pleasure to see you again, %s.\"",
	"%s: \"How may I be of assistance, good %s?\"",
	"%s: \"You do honour to my humble store, noble %s.\"",
	"%s: \"I and my family are entirely at your service, glorious %s.\""
};


#define MAX_COMMENT_1A	6

static cptr comment_1a[MAX_COMMENT_1A] =
{
	"Works for me!",
	"Deal!",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!"
};


#define MAX_COMMENT_1B	10

static cptr comment_1b[MAX_COMMENT_1B] =
{
  "You drive a hard bargain, but taken.",
  "You'll force me bankrupt, but it's a deal.",
  "Sigh.  I'll take it.",
  "My poor sick children may starve, but done!",
  "I'm never going to make a profit off of you, I can see that.",
  "Agreed, but I would *never* have accepted this price from anyone else.",
  "I'm cutting me own throat, but OK.",   /* Apologies to T. Prachett */
  "You're a prince of hagglers, and I accept your price.",
  "I'll end my days a beggar if I keep dealing with you.  Done.",
  "My spouse will skin me, but accepted."
};



#define MAX_COMMENT_2A	5

static cptr comment_2a[MAX_COMMENT_2A] =
{
	"I'll let this go for %s; take it or leave it.",
	"%s is as low as I go.",
	"%s and that's final.",
	"Final price:  %s - I just can't go lower.",
	"Final price:  %s, and you can't do better anywhere."
};


#define MAX_COMMENT_2B	31

/*
 * Ordered by "rudeness".
 */
static cptr comment_2b[MAX_COMMENT_2B] =
{
	"Hmm.  I'm prepared to accept %s gold.",
	"%s gold, and I am almost giving it away.",
	"You just can't get this quality for that money.  %s gold.",
	"%s gold, and that only because you are my friend.",
	"I can take no less than %s gold pieces.",
	"%s gold, and it's worth twice that.",
	"I paid more than that for it myself, try %s.",
	"Far, far too little.  How about %s?",
	"Sell this for such a pittance?  Give me %s gold.",
	"I will accept no less than %s gold pieces.",
	"Why, I'll take no less than %s gold pieces.",
	"Work with me a little.  How about %s gold pieces?",
	"As scrap this would bring that.  Try %s in gold.",
	"That's a pittance.  Try %s.",
	"Huh!  %s gold pieces.",
	"You know full well it's worth at least %s gold pieces.",
	"%s gold; tomorrow it will be higher.",
	"The good jest.  A more serious price would be %s gold.",
	"I paid more than that!  Why, %s is too little already!",
	"My poor, sick, starving children need %s at least!",
	"I'm selling below cost here!  How about %s gold pieces?",
	"I have five customers eyeing this.  %s gold, and you pay the tax.",
	"Shop around.  Come back to me when you're ready to pay %s.",
	"Why do you even waste my time?  %s gold, and I am generous.",
	"Do you realize how hard it is to find this?  %s gold.",
	"Your mother was an Ogre!  %s or I'll tell.",
	"%s gold, and be grateful it's even available!",
	"May the fleas of a thousand orcs molest you!  I want %s.",
	"I'm losing money just talking to you!  %s gold.",
	"May your favourite bits and pieces go mouldy!  I want %s in gold!",
	"May Morgoth find you tasty!  I want %s, and no clipped coins!"
};



#define MAX_COMMENT_3A	5

static cptr comment_3a[MAX_COMMENT_3A] =
{
	"%s is my final offer; take it or leave it.",
	"I'll give you no more than %s.",
	"I'll pay no more than %s; take it or leave it.",
	"You'll get no more than %s from me.",
	"%s and that's final."
};


#define MAX_COMMENT_3B	31

/*
 * Ordered by "rudeness".
 */
static cptr comment_3b[MAX_COMMENT_3B] =
{
	"How about %s?",
	"Marry, %s gold is an honest price.",
	"%s gold is more than fair.",
	"Try %s gold pieces.",
	"Perhaps %s gold pieces?",
	"Friend, it's just not worth that much.  Accept %s gold.",
	"Be reasonable.  How about %s.",
	"I can afford no more than %s gold.",
	"Yes it's nice, but you're asking too much.  %s gold?",
	"Hmm.  For the sake of your future business, %s in gold.",
	"I offer you %s gold, and that only because we've done business before.",
	"%s gold pieces and not a copper more.",
	"Come now.  Even %s gold would be generous.",
	"What you have, I don't want for more than %s gold.",
	"That looks war surplus!  Say %s gold.",
	"It's pretty, but nobody wants it.  %s gold.",
	"Hah!  %s gold is already too high!",
	"NEVER!  %s is more like it.",
	"I'll buy it as scrap for %s.",
	"Only in your dreams.  %s.",
	"Shall I parade my sick, starving children before you?  %s gold.",
	"Let us end this farce, and say %s gold.",
	"*CHOKE* For that!?  Let's say %s.",
	"For what you ask I could own ten of those.  %s gold.",
	"%s gold pieces and be thankful for it!",
	"For that piece of junk?  No more than %s.",
	"What kobold-warren did you find this in?  %s gold pieces.",
	"Where you get your prices from, I will never know.  %s gold.",
	"I want some of whatever you're smoking.  %s gold.",
    "I wouldn't pay that for your tongue on a platter.  Try %s.",
	"I look at that dreck, and I sniff and sneer.  %s gold."
};



#define MAX_COMMENT_4A	6

static cptr comment_4a[MAX_COMMENT_4A] =
{
	"Enough!  You have abused me once too often!",
	"You insult me, you ignore my warnings, I've had enough!",
	"Arghhh!  I have had enough abuse for one day!",
	"That does it!  You shall waste my time no more!",
	"This is getting nowhere!",
	"BAH!  No more shall you insult me!"
};


#define MAX_COMMENT_4B	6

static cptr comment_4b[MAX_COMMENT_4B] =
{
	"Leave, and don't hurry back!",
	"Out of my place!",
	"Get out of my sight!",
	"Begone, you scoundrel!",
	"Disappear, and take your junk with you!",
	"out... Out... OUT!!!"
};


#define MAX_COMMENT_5	18

/*
 * In order of impatience.
 */
static cptr comment_5[MAX_COMMENT_5] =
{
	"Let me humbly propose a different price.",
	"Please try again.",
	"I can't hear you...",
	"I really cannot accept that price.  Really.",
	"Can we please be reasonable?",
	"You need to work with me here...",
	"*Grumblegrumblegrumble*",
	"Do you wish to do business or not?",
	"That's a joke, right?",
	"You've got to be kidding!",
	"I warn you, that price was insulting.",
	"You will have to do better than that!",
	"Improve your price, choose something else, or leave.",
	"You try my patience sorely.",
	"Come back to me with a reasonable price!",
	"That's an insult!",
	"My patience is almost exhausted!",
	"Much more of this nonsense, and we're done!"
};


#define MAX_COMMENT_6A	4

static cptr comment_6a[MAX_COMMENT_6A] =
{
	"I must have heard you wrong.",
	"I'm sorry, I missed that.",
	"I'm sorry, what was that?",
	"Sorry, what was that again?"
};


#define MAX_COMMENT_6B	6

static cptr comment_6b[MAX_COMMENT_6B] =
{
	"Hey, I thought we had a deal!",
	"We've agreed upon a price.  Do you want to close the deal or not?",
	"If you don't like the price, maybe you should show me something else.",
	"Dunno about you, but I like this price, and I'm not changing it!",
	"What do you think this is, an Orcish bazaar?",
	"Accept this price, or show me something else."
};



/*
 * Messages for reacting to purchase prices.
 */

#define MAX_COMMENT_7A	11

static cptr comment_7a[MAX_COMMENT_7A] =
{
	"%s beats his head against the counter.  AARRGH!",
	"%s wails in anguish!",
	"%s:  Curse it and crush it!",
	"%s:  How did I let myself get robbed like this?",
	"%s:  Identify your crap before you sell it, bastard!",
	"%s breaks down, weeping piteously about imminent bankruptcy.",
	"%s:  Sell the dreck you find on the floor to someone else!",
	"%s:  NO!  This can't be happening to me!",
	"%s:  Aii!  I'm ruined, ruined!",
	"%s screams and throws his purchase across the store.",
	"%s howls in agony!"
};

#define MAX_COMMENT_7B	10

static cptr comment_7b[MAX_COMMENT_7B] =
{
	"%s:  Phooie!",
	"%s:  You fiend!",
	"%s:  Oh, the pain!",
	"%s:  Outsmarted, and in my own store too!",
	"%s:  Send me back to bargaining school!",
	"%s:  Ratsafrazer!",
	"%s:  Can we undo that last deal, friend?",
	"%s:  Robbed again.  When will I learn?",
	"%s glares at what he just bought.",
	"%s:  Oh poot."   /* A tribute to the Penance comic */
};

#define MAX_COMMENT_7C	6

static cptr comment_7c[MAX_COMMENT_7C] =
{
	"How truly acceptable.",
	"You've made my day!",
	"%s giggles.",
	"%s sniggers.",
	"%s smiles gleefully.",
	"%s laughs loudly."
};

#define MAX_COMMENT_7D	9

static cptr comment_7d[MAX_COMMENT_7D] =
{
	"Yippee!",
	"I'm rich, rich, rich!",
	"%s jumps for joy.",
	"%s does a victory dance.",
	"Oh, the gold I will be able to sell this for!",
	"Wow.  I'm going to name my new villa in your honor.",
	"I've got it MADE!",
	"Let me do ALL your identifying for you, friend!",
	"Yes!  Yes!  Yes!"
};



/*
 * Close the deal.
 */
static void say_comment_1(bool perfect_haggle)
{
	if (perfect_haggle) comment(comment_1b[rand_int(MAX_COMMENT_1B)]);
	else                comment(comment_1a[rand_int(MAX_COMMENT_1A)]);
}


/*
 * Determine haggle message based on politeness.
 */
static int comment_choice(bool buying)
{
	int rudeness, this_comment;

	/* Get average tolerance/politeness */
	int toler_range = ALWAYS_POLITE - ALWAYS_RUDE;

	/* Get maximum allowable comment index */
	int max = (buying ? MAX_COMMENT_2B : MAX_COMMENT_3B);


	/* Rudeness ranges from 0 to tolerance range */
	rudeness = ALWAYS_POLITE - ot_ptr->insult_max;
	if (rudeness < 0) rudeness = 0;
	if (rudeness > toler_range) rudeness = toler_range;


	/* Start out one-sixth of the way down the comment list */
	this_comment = div_round(max, 6);

	/* Can go to five-sixths down the list if maximally rude */
	this_comment += (rudeness * 4 * max) / (6 * toler_range);

	/* Randomize (by a sixth) */
	this_comment = rand_spread(this_comment, div_round(max, 6));

	/* Set bounds */
	if (this_comment < 0) this_comment = 0;
	if (this_comment >= max) this_comment = max - 1;

	/* Return comment */
	return (this_comment);
}



/*
 * Continue haggling (player is buying)
 */
static void say_comment_2(s32b value, bool final)
{
	char tmp_val[DESC_LEN];

	/* Prepare a string to insert */
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%ld", (long)value);

	/* Final offer */
	if (final)
	{
		/* Formatted message */
		comment(comment_2a[rand_int(MAX_COMMENT_2A)], tmp_val);
	}

	/* Normal offer */
	else
	{
		/* Formatted message */
		comment(comment_2b[comment_choice(TRUE)], tmp_val);
	}
}


/*
 * Continue haggling (player is selling)
 */
static void say_comment_3(s32b value, bool final)
{
	char tmp_val[DESC_LEN];

	/* Prepare a string to insert */
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%ld", (long)value);

	/* Final offer */
	if (final)
	{
		/* Formatted message */
		comment(comment_3a[rand_int(MAX_COMMENT_3A)], tmp_val);
	}

	/* Normal offer */
	else
	{
		/* Formatted message */
		comment(comment_3b[comment_choice(FALSE)], tmp_val);
	}
}


/*
 * Kick 'da bum out.                  -RAK-
 */
static void say_comment_4(void)
{
	comment(comment_4a[rand_int(MAX_COMMENT_4A)]);
	comment(comment_4b[rand_int(MAX_COMMENT_4B)]);
}


/*
 * You are insulting me
 */
static void say_comment_5(void)
{
	/* Get remaining tolerance */
	int tolerance = MIN(30, ot_ptr->insult_max - st_ptr->insult_cur);

	/* Base comment (from 2 to MAX_COMMENT_5 - 3) */
	int base = 2 + (MAX_COMMENT_5 - 5) * (30 - tolerance) / 30;

	/* Randomize comment */
	int tmp = Rand_normal(base, MAX_COMMENT_5 / 8);

	/* Verify comment */
	if (tmp < 0) tmp = 0;
	if (tmp >= MAX_COMMENT_5) tmp = MAX_COMMENT_5 - 1;

	/* Display warning (increasingly impatient) */
	comment(comment_5[tmp]);
}


/*
 * You are making no sense, or are trying to change an agreed-upon price.
 */
static void say_comment_6(bool no_sense)
{
	if (no_sense) comment(comment_6a[rand_int(MAX_COMMENT_6A)]);
	else          comment(comment_6b[rand_int(MAX_COMMENT_6B)]);
}


/*
 * React to a purchase
 */
static void say_comment_7(int num)
{
	char short_name[DESC_LEN];

	/* Get the short shopkeeper name */
	shopkeeper_short_name(short_name, ot_ptr->owner_name);

	/* ARRGH! */
	if (num == 1)
	{
		comment(comment_7a[rand_int(MAX_COMMENT_7A)], short_name);
	}

	/* Uncool */
	else if (num == 2)
	{
		comment(comment_7b[rand_int(MAX_COMMENT_7B)], short_name);
	}

	/* Nice */
	else if (num == 3)
	{
		comment(comment_7c[rand_int(MAX_COMMENT_7C)], short_name);
	}

	/* Yes!  Yes!  Yes! */
	else if (num == 4)
	{
		comment(comment_7d[rand_int(MAX_COMMENT_7D)], short_name);
	}
}


/*
 * Let a shop-keeper react to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(s32b price, s32b value, s32b guess)
{
	/* No need to comment about purchases */
	if (birth_stores_only_sell) return;

	/* Item was worthless, and we paid a lot for it */
	if ((value <= 0L) && (price >= 25L * (p_ptr->max_depth + 10)))
	{
		/* Comment */
		say_comment_7(1);
	}

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
	{
		/* Comment */
		say_comment_7(2);
	}

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (price < value))
	{
		/* Great bargain */
		if (value >= 350L + (3 * guess / 2)) say_comment_7(4);

		/* Merely good bargain */
		else say_comment_7(3);
	}
}



/*
 * Determine the price of an object (qty one) in a store.
 *
 * Adjustments to price include charisma, race, shopkeeper greed, and a
 * price-dependant markup.  Shopkeepers never buy for more than they sell.
 *
 * Object price adjustments range from  10 to 200+. (see "object_value()")
 * Charisma adjustments     range from  80 to 150.
 * Racial adjustments       range from  85 to 130.
 * Greed adjustments        range from 105 to 210.
 * Markups                  range from   0 to  33.
 */
static s32b price_item(object_type *o_ptr, int greed, bool buying, int markup)
{
	int racial_factor, charisma_factor;
	int offer_adjust;
	s32b price;

	/* Get the value of one of the items (including price adjustment) */
	price = object_value(o_ptr);

	/* Worthless items */
	if (price <= 0L) return (0L);

	/* Get the racial factor */
	racial_factor = rgold_adj[ot_ptr->owner_race][p_ptr->prace];

	/* Get the charisma factor */
	charisma_factor = adj_chr_gold[p_ptr->stat_ind[A_CHR]];

	/* Add the factors together */
	offer_adjust = racial_factor + charisma_factor + greed + markup - 200;

	/* Shopkeepers never buy for more than they sell */
	if (offer_adjust < 100) offer_adjust = 100;

	/* Shopkeepers never get totally unreasonable */
	if (offer_adjust > 300) offer_adjust = 300;


	/* Shop is buying */
	if (buying)
	{
		/* Compute the final price (with rounding) */
		price = ((price * 100L) + 50L) / offer_adjust;
	}

	/* Shop is selling */
	else
	{
		/* Compute the final price (with rounding) */
		price = ((price * offer_adjust) + 50L) / 100L;
	}

	/* Never become "free" */
	if (price <= 0L) return (1L);

	/* Return the price */
	return (price);
}



/*
 * Convert a store item index into a one-character label
 *
 * We use labels "a"-"l" for page 1, and labels "m"-"x" for page 2.
 */
static s16b store_to_label(int i)
{
	/* Assume legal */
	return (I2A(i));
}


/*
 * Convert a one-character label into a store item index.
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
 *
 * It also allows a few items (staffs, for example) to stack in
 * stores even if they don't elsewhere.
 */
static bool store_object_similar(const object_type *o_ptr,
	const object_type *j_ptr)
{
	/* Hack -- Identical items cannot be stacked */
	if (o_ptr == j_ptr) return (0);

	/* Different object kinds cannot be stacked */
	if (o_ptr->k_idx != j_ptr->k_idx) return (0);

	/* Different charges (etc) cannot be stacked, unless wands or rods. */
	if ((o_ptr->pval != j_ptr->pval) &&
		(o_ptr->tval != TV_WAND) && (o_ptr->tval != TV_ROD)) return (0);

	/* Require many identical values */
	if (o_ptr->to_h != j_ptr->to_h) return (0);
	if (o_ptr->to_d != j_ptr->to_d) return (0);
	if (o_ptr->to_a != j_ptr->to_a) return (0);

	/* Require identical "artifact" names */
	if (o_ptr->artifact_index != j_ptr->artifact_index) return (0);

	/* Require identical "ego-item" names */
	if (o_ptr->ego_item_index != j_ptr->ego_item_index) return (0);

	/* Must have the same flags */
	if ((o_ptr->flags1 != j_ptr->flags1) ||
	    (o_ptr->flags2 != j_ptr->flags2) ||
	    (o_ptr->flags3 != j_ptr->flags3)) return (0);

	/* Hack -- Never stack recharging items */
	if (o_ptr->timeout || j_ptr->timeout) return (0);

	/* Require many identical values */
	if (o_ptr->ac != j_ptr->ac) return (0);
	if (o_ptr->dd != j_ptr->dd) return (0);
	if (o_ptr->ds != j_ptr->ds) return (0);

	/* Hack -- Never stack chests */
	if (o_ptr->tval == TV_CHEST) return (0);

	/* We do not require matching cost adjustments */

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

	/* Shopkeeper is greedy -- always use higher cost */
	if (o_ptr->cost_adjust < j_ptr->cost_adjust)
	    o_ptr->cost_adjust = j_ptr->cost_adjust;

	/*
	 * Hack -- if rods are stacking, add the pvals (maximum timeouts) and
	 * current timeouts together.
	 */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval += j_ptr->pval;
		o_ptr->timeout += j_ptr->timeout;
	}

	/* Hack -- if wands/staves are stacking, combine the charges */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
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
	object_type *j_ptr;

	/* Free space is always usable */
	if (st_ptr->stock_num < st_ptr->stock_size) return (TRUE);

	/* The "home" acts like the player */
	if (store_num == STORE_HOME)
	{
		/* Check all the object */
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
 * Determine if the current store will purchase the given object
 *
 * Note that a shop-keeper must refuse to buy "worthless" objects
 */
static bool store_will_buy(const object_type *o_ptr)
{
	/* Hack -- The Home is simple */
	if (store_num == STORE_HOME) return (TRUE);

	/* Switch on the store */
	switch (store_num)
	{
		/* General Store */
		case STORE_GENERAL:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_SKELETON:
				case TV_JUNK:
				case TV_COMPONENT:
				case TV_FOOD:
				case TV_LITE:
				case TV_FLASK:
				case TV_SPIKE:
				case TV_SHOT:
				case TV_ARROW:
				case TV_BOLT:
				case TV_DIGGING:
				case TV_CLOAK:

				break;
				case TV_SOFT_ARMOR:
				{
					if (o_ptr->sval == SV_ROBE) break;
					return (FALSE);
				}

				default:
				return (FALSE);
			}
			break;
		}

		/* Armory */
		case STORE_ARMOR:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_BOOTS:
				case TV_GLOVES:
				case TV_CROWN:
				case TV_HELM:
				case TV_SHIELD:
				case TV_CLOAK:
				case TV_SOFT_ARMOR:
				case TV_HARD_ARMOR:
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
				case TV_SHOT:
				case TV_BOLT:
				case TV_ARROW:
				case TV_SLING:
				case TV_BOW:
				case TV_CROSSBOW:
				case TV_DIGGING:
				case TV_HAFTED:
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
				case TV_HAFTED:
				break;
				case TV_POLEARM:
				case TV_SWORD:
				{
					u32b f1, f2, f3;

					/* Get object attributes */
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Known blessed blades are accepted too */
					if ((f3 & (TR3_BLESSED)) && object_known_p(o_ptr))
						break;
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
				case TV_SCROLL:
				case TV_POTION:
				case TV_BOTTLE:
				case TV_PARCHMENT:
				case TV_COMPONENT:
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
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Bookstore */
		case STORE_SPELLBOOK:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_MAGIC_BOOK:
				case TV_PRAYER_BOOK:
				case TV_NATURE_BOOK:
				case TV_DARK_BOOK:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		default:  return (FALSE);
	}

	/* Ignore "worthless" items XXX XXX XXX */
	if (object_value(o_ptr) <= 0L) return (FALSE);

	/* Assume okay */
	return (TRUE);
}


/*
 * Add an object to a real store's inventory and reorder the stock.
 *
 * If the object is "worthless", it is thrown away (except in the home).
 *
 * In all cases, return the slot (or -1) where the object was placed.
 */
static int store_carry(object_type *o_ptr)
{
	int i, slot;
	object_type *j_ptr;

	/* Standard store */
	if (store_num != STORE_HOME)
	{
		/* Evaluate the object */
		s32b value = object_value(o_ptr);

		/* Cursed/Worthless items "disappear" when sold */
		if (value <= 0L) return (-1);

		/* Erase the inscription */
		o_ptr->note = 0;

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
	}

	/* The home uses rules similar to the backpack */
	else
	{
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
	}


	/* No space? */
	if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


	/* Find an empty slot */
	for (i = 0; i < st_ptr->stock_size; i++)
	{
		/* Use it if found */
		if (!st_ptr->stock[i].k_idx) break;
	}

	/* Copy the item */
	object_copy(&st_ptr->stock[i], o_ptr);

	/* Reorder store inventory, track change in inventory slot */
	slot = reorder_pack(i, store_num, FALSE);

	/* More stuff now  XXX */
	st_ptr->stock_num++;

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

	/* Hack -- adjust the maximum timeouts and total charges of rods and wands. */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
	{
		o_ptr->pval += num * o_ptr->pval / o_ptr->number;
	}

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
 * Attempt to delete (some of) a random object from the store
 */
static void store_delete(void)
{
	int what, num;
	int tries, i;
	object_type *o_ptr;
	object_kind *k_ptr;

	/* Paranoia */
	if (st_ptr->stock_num <= 0) return;

	/* Try to remove something */
	for (tries = 20; tries; tries--)
	{
		bool safe = FALSE;

		/* Pick a random slot */
		what = rand_int(st_ptr->stock_num);

		/* Get object */
		o_ptr = &st_ptr->stock[what];

		/* Paranoia -- Ignore illegal objects */
		if (o_ptr->k_idx <= 0) continue;

		/* Get object kind */
		k_ptr = &k_info[o_ptr->k_idx];

		/* Usually do not delete standard stock items */
		if ((tries > 3) || (st_ptr->stock_num <= STORE_MAX_KEEP))
		{
			for (i = st_ptr->stock_start; i <= st_ptr->stock_end; i++)
			{
				/* Non-ego-item object with a standard index */
				if ((o_ptr->k_idx == store_stock[i].k_idx) &&
				    (!o_ptr->ego_item_index))
				{
					safe = TRUE;
					break;
				}
			}
		}

		/* We found something we can kill */
		if (!safe) break;
	}

	/* Didn't find anything to delete */
	if (tries == 0) return;

	/* Determine how many objects are in the slot */
	num = st_ptr->stock[what].number;

	/* Hack -- sometimes, only destroy some of the objects */
	if ((num > 1) && (one_in_(3))) num = randint(num - 1);

	/* Actually destroy (some of) the object(s) */
	store_item_increase(what, -num);
	store_item_optimize(what);
}





/*
 * Create an item and try to stock it.
 */
static void store_item_create(int k_idx, int level, bool special, int num)
{
	int tries;

	object_type *i_ptr;
	object_type object_type_body;

	int max_purse;
	int discount, odds;

	s32b cost;
	bool missile;



	/* Try up to ten times */
	for (tries = 0; tries < 10; tries++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Create a new object of the chosen kind */
		object_prep(i_ptr, k_idx);

		/* Balance -- Stores sell weak ammo early on  XXX XXX */
		if ((is_missile(i_ptr)) && (p_ptr->power < 15))
		{
			k_idx--;
			object_prep(i_ptr, k_idx);
		}

		/* Balance -- Stores do not sell Javelins early on  XXX XXX */
		if ((i_ptr->tval == TV_POLEARM) && (i_ptr->sval == SV_JAVELIN) && (p_ptr->power < 15))
		{
			/* Do not make the item */
			return;
		}


		/* We're creating an object in a store */
		obj_gen_flags |= (OBJ_GEN_STORE);

		/* Normally make mundane items */
		if (!special)
		{
			/* Can create "good", but never "great" items */
			apply_magic(i_ptr, level, -1, FALSE, FALSE);
		}

		/* If special, make good items (but never artifacts) */
		else
		{
			/* 1 in 10 -> 1 in 3 great */
			odds = 10 - MIN(7, rsqrt(st_ptr->total_buy / 25000));

			/* Make object */
			apply_magic(i_ptr, level, FALSE, TRUE, (one_in_(odds)));
		}

		/* Clear quality-control variable */
		obj_gen_flags &= ~(OBJ_GEN_STORE);


		/* Hack -- Standard charges for light sources */
		if ((i_ptr->tval == TV_LITE) && (i_ptr->pval))
		{
			if (i_ptr->sval == SV_LITE_TORCH)   i_ptr->pval = FUEL_TORCH / 2;
			if (i_ptr->sval == SV_LITE_LANTERN) i_ptr->pval = FUEL_LAMP / 2;
		}

		/* The object is "known" */
		object_known(i_ptr);

		/* No "worthless" items  XXX XXX */
		if (object_value(i_ptr) <= 0) continue;

		/* Limit the quantity of some objects */
		if ((k_info[i_ptr->k_idx].gen_dice *
		     k_info[i_ptr->k_idx].gen_side <= 1))
		{
			/* Get object kind */
			object_kind *k_ptr = &k_info[i_ptr->k_idx];

			/* Limit numbers */
			if (i_ptr->ego_item_index)
			{
				num = 1;
			}
			else if ((i_ptr->to_a + i_ptr->to_h + i_ptr->to_d) >
			         (k_ptr->to_a + k_ptr->to_h + k_ptr->to_d))
			{
				num = rsqrt(randint(num));
			}
		}

		/* Adjust quantity */
		i_ptr->number = num;


		/* Is this item a missile? */
		missile = (bool)is_missile(i_ptr);

		/* Get the value of this item */
		cost = object_value(i_ptr);


		/*
		 * Determine the discount probability modifier.   The purpose of
		 * this is to make shopkeepers with low purses offer more sales,
		 * as one might expect.  -LM-
		 */

		/* Hack -- determine the maximum purse at this type of store. */
		if (store_num == STORE_GENERAL) max_purse = 12000;
		if (store_num == STORE_ALCHEMY) max_purse = 15000;
		else max_purse = 30000;

		/* Determine discount probability but stay reasonable. */
		odds = div_round(30 * ot_ptr->max_cost, max_purse);


		/* Assume no discount */
		discount = 0;

		/* Sometimes discount non-special items */
		if ((cost >= 5) && (one_in_(odds)))
		{
			int tmp = randint(100);

			/* 75% chance of a 25% discount */
			if      (tmp <= 75)
			{
				discount = 25;
			}

			/* 12% chance of a 50% discount */
			else if (tmp <= 87)
			{
				discount = 50;
				if (!missile) i_ptr->number =
					rand_range((i_ptr->number + 1) / 2,
					i_ptr->number);
			}

			/* 8% chance of a 75% discount */
			else if (tmp <= 95)
			{
				discount = 75;
				if (!missile) i_ptr->number =
					rand_range((i_ptr->number + 3) / 4,
					(i_ptr->number + 1) / 2);
			}

			/* 5% chance of a 90% discount */
			else
			{
				discount = 90;
				if (!missile) i_ptr->number =
					rand_range(1, (i_ptr->number + 1) / 2);
			}
		}

		/* Save the discount */
		i_ptr->cost_adjust = 100 - discount;


		/* Special items, especially valuable ones, cost more */
		if (special && cost > 1000L)
		{
			/* Calculate cost increase */
			long k = div_round(cost - 1000L, 100L);

			/* Never add more than 120% to price */
			if (k > 120L) k = 120L;

			/* Apply cost adjustment */
			i_ptr->cost_adjust += (byte)k;

			/* Hack -- neaten (and reduce) discounts */
			if (i_ptr->cost_adjust < 100)
			{
				/* No 75% and 90% discounts, but some 10% ones */
				if      (i_ptr->cost_adjust <= 50) i_ptr->cost_adjust = 50;
				else if (i_ptr->cost_adjust <= 75) i_ptr->cost_adjust = 75;
				else if (i_ptr->cost_adjust <= 90) i_ptr->cost_adjust = 90;
				else                               i_ptr->cost_adjust = 100;
			}
		}

		/* Fix wand and rod pvals */
		if ((i_ptr->tval == TV_ROD) || (i_ptr->tval == TV_WAND))
		{
			i_ptr->pval *= i_ptr->number;
		}

		/* Attempt to carry the (known) object */
		(void)store_carry(i_ptr);

		/* Done */
		break;
	}
}


/*
 * Delete a random item if the store is getting full, then verify one
 * standard stock item, and - in certain cases - create a special item.
 */
static void store_adjust(int table_idx)
{
	int i, j, level;
	int stock_idx;
	int tval = 0;

	bool sell_it = FALSE;
	bool upscale = FALSE;

	int k_idx = 0;

	int divisor;


	/* Sometimes sell something */
	if (st_ptr->stock_num > rand_range(12, STORE_INVEN_MAX)) store_delete();

	/* We have not been supplied with a valid stock index */
	if ((table_idx < st_ptr->stock_start) || (table_idx > st_ptr->stock_end))
	{
		/* Choose a standard store item */
		table_idx = rand_range(st_ptr->stock_start, st_ptr->stock_end);
	}

	/* Determine if we wish to get rid of it */
	if (store_stock[table_idx].prob < randint(100)) sell_it = TRUE;

	/* Determine if the store is currently stocking this item */
	for (stock_idx = -1, i = 0; i < st_ptr->stock_num; i++)
	{
		/* Matching object kind indexes */
		if (st_ptr->stock[i].k_idx == store_stock[table_idx].k_idx)
		{
			stock_idx = i;
			break;
		}
	}

	/* We already have it in stock */
	if (stock_idx >= 0)
	{
		/* Destroy it */
		store_item_increase(stock_idx, -999);
		store_item_optimize(stock_idx);
	}

	/* We need to (re)create it */
	if (!sell_it)
	{
		/* Get desired quantity */
		int num = Rand_normal(store_stock[table_idx].num,
			div_round(store_stock[table_idx].num, 4));

		/* We always want at least one */
		if (num < 1) num = 1;

		/* We never want more than (MAX_STACK_SIZE - 1) items */
		if (num >= MAX_STACK_SIZE) num = MAX_STACK_SIZE - 1;

		/* Apply low-level magic */
		level = rand_range(1, STORE_OBJ_LEVEL);

		/* Create the item */
		store_item_create(store_stock[table_idx].k_idx, level, FALSE, num);
	}


	/* Adjust inherent probability of good items  XXX XXX */
	switch (store_num)
	{
		case STORE_GENERAL:    divisor =  900;  break;
		case STORE_TEMPLE:     divisor = 1700;  break;
		case STORE_ALCHEMY:    divisor = 1800;  break;
		case STORE_SPELLBOOK:  divisor = 2800;  break;
		default:               divisor = 1400;  break;
	}

	/* Sometimes a store owner will have special items in stock */
	if (rsqrt(st_ptr->total_buy) > rand_int(divisor))
	{
		/* Scan to the end of the list of tvals this store sells */
		for (i = 0; i < 10; i++)
		{
			if (!tval_sell[store_num][i]) break;
		}

		/* Pick one of these tvals */
		if (i) tval = tval_sell[store_num][rand_int(i)];

		/* We have a reasonable tval */
		if ((tval > 0) && (tval < TV_MAX)) upscale = TRUE;
	}

	/* Go upscale (add +10 to level for "good" quality) */
	if (upscale)
	{
		/* Stock is too large -- sell something */
		if (st_ptr->stock_num > STORE_MAX_KEEP) store_delete();

		/* Allow fairly high object levels (plus 10 for "good" quality) */
		if (store_num != STORE_SPELLBOOK)
		{
			/* Level 80 (some chance of higher) with 2m invested */
			level = MIN(rsqrt(st_ptr->total_buy / 785), 50) + 20;
		}

		/* Special case -- the bookstore */
		else
		{
			/* Harder to get high, but can reach any level */
			level = rsqrt(st_ptr->total_buy / 370);
		}

		/* Restrict to our chosen tval */
		required_tval = tval;

		/* Activate restriction */
		get_obj_num_hook = kind_fits_tval;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Usually get an object that we don't already stock */
		for (i = 0; i < 25; i++)
		{
			bool okay = TRUE;

			/* Get an object index (quickly) */
			k_idx = get_obj_num(level);

			/* Check existing items */
			for (j = 0; j < st_ptr->stock_num; j++)
			{
				if (k_idx == st_ptr->stock[j].k_idx)
				{
					okay = FALSE;
					break;
				}
			}

			/* Accept this item */
			if (okay) break;
		}

		/* No longer restricting to our chosen tval */
		required_tval = 0;

		/* Clear restriction */
		get_obj_num_hook = NULL;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* We have a valid index */
		if (k_idx)
		{
			object_kind *k_ptr = &k_info[k_idx];

			/* Basic number */
			int num = damroll(k_ptr->gen_dice, k_ptr->gen_side);
			if (num < 1) num = 1;

			/* Inexpensive non-wearable items can be more common (up to +15) */
			if ((!is_wearable(k_ptr)) && (k_ptr->cost < 1000L))
			{
				num += randint(div_round(1000 - k_ptr->cost, 65));
			}

			/* Make the item */
			store_item_create(k_idx, level, TRUE, num);
		}
	}
}



/*
 * Determine the price markup on an object, which depends on price and
 * bargaining record.  Items with no markup never need to be haggled
 * for.
 *
 * Note that the "fool's tax" on pricey items can be considerable, and
 * those who never bargain are stuck with paying it...
 */
static int price_markup(s32b price)
{
	/* Markup depends on price */
	int markup = (2 + rsqrt(price / 150L)) * 2;

	/* Never a need to haggle for small stuff (ignore any bad haggles) */
	if (price < 250L) return (0);

	/* Good hagglers are rewarded, bad ones are penalized */
	markup -= (st_ptr->good_buy * st_ptr->good_buy) -
	          (4 * st_ptr->bad_buy);

	/* We never get too nasty */
	if (markup > 33) markup = 33;

	/* If the markup is low, we cancel it entirely */
	if (markup <= 4) markup = 0;

	/* Return */
	return (markup);
}


/*
 * Redisplay a single store entry
 */
static void display_entry(int item)
{
	int y;
	object_type *o_ptr;
	s32b x;
	int markup = 0;

	char o_name[DESC_LEN];
	char out_val[160];
	int maxwid;
	int lines = stock_displayed;
	int label_attr = TERM_WHITE;


	/* Must be on current "page" to get displayed */
	if (!((item >= store_top) && (item < store_top + lines))) return;

	/* Highlight the object if necessary */
	if (item == item_buy_idx) label_attr = TERM_YELLOW;


	/* Get the object */
	o_ptr = &st_ptr->stock[item];

	/* Get the row */
	y = (item % lines) + 6;

	/* Label it */
	(void)strnfmt(out_val, sizeof(out_val), "%c) ", store_to_label(item));

	/* Describe an item in the home */
	if (store_num == STORE_HOME)
	{
		byte attr;

		/* Print label, clear the line --(-- */
		if (o_ptr->ident & (IDENT_MENTAL))
		{
			c_prt(TERM_L_BLUE, out_val, y, 0);
		}
		else if ((object_known_p(o_ptr)) || (object_aware_p(o_ptr)))
		{
			prt(out_val, y, 0);
		}
		else
		{
			c_prt(TERM_L_WHITE, out_val, y, 0);
		}

		maxwid = 75;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 10;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
		o_name[maxwid] = '\0';

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Display the object */
		c_put_str(attr, o_name, y, 3);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of a single object */
			int wgt = use_metric ? make_metric(o_ptr->weight) : o_ptr->weight;

			(void)strnfmt(out_val, sizeof(out_val), "%3d.%d", wgt / 10, wgt % 10);
			put_str(out_val, y, 72);
		}
	}

	/* Describe an object (fully) in a store */
	else
	{
		byte attr;

		/* Clear the line --(-- */
		prt(out_val, y, 0);

		/* Must leave room for the "price" */
		maxwid = 65;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 7;

		/* Describe the object (fully) */
		object_desc_store(o_name, sizeof(o_name), o_ptr, TRUE, 3);
		o_name[maxwid] = '\0';

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Display the object */
		c_put_str(attr, o_name, y, 3);

		/* Note that we are buying it */
		if (item == item_buy_idx)
		{
			c_put_str(label_attr, format("(buying %d)", item_buy_num),
				y, 4 + strlen(o_name));
		}

		/* Show weights (one space before, for neatness) */
		if (show_weights)
		{
			/* Only show the weight of a single object */
			int wgt = use_metric ? make_metric(o_ptr->weight) : o_ptr->weight;

			(void)strnfmt(out_val, sizeof(out_val), " %3d.%d", wgt / 10, wgt % 10);
			c_prt(label_attr, out_val, y, 61);
		}


		/* XXX XXX - Mark objects as "seen" (doesn't belong in this function) */
		k_info[o_ptr->k_idx].special |= (SPECIAL_EVER_SEEN);


		/* Determine markup */
		if (!(o_ptr->ident & (IDENT_FIXED)))
		{
			markup = price_markup(object_value(o_ptr));
		}

		/* Extract the "minimum" price */
		x = price_item(o_ptr, ot_ptr->min_inflate, FALSE, markup);

		/* Item has no markup */
		if (!markup)
		{
			/* Display the final price */
			(void)strnfmt(out_val, sizeof(out_val), "%8ld", (long)x);
			c_prt(label_attr, out_val, y, 69);
		}

		/* Display a "haggle" cost */
		else
		{
			/* Not auto-haggling */
			if (!never_haggle)
			{
				/* Calculate the initial asking price */
				x = price_item(o_ptr, ot_ptr->max_inflate, FALSE, markup);
			}

			/* Display the price (with markup) */
			(void)strnfmt(out_val, sizeof(out_val), "%8ld", (long)x);
			if (item == item_buy_idx)
				c_put_str(TERM_ORANGE, out_val, y, 69);
			else
				c_put_str(TERM_L_WHITE, out_val, y, 69);
		}
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
	int lines = stock_displayed;

	/* Display the next page of stock */
	for (k = 0; k < lines; k++)
	{
		/* Stop when we run out of items */
		if (store_top + k >= st_ptr->stock_num) break;

		/* Display that line */
		display_entry(store_top + k);
	}

	/* Erase the extra lines and the "more" prompt */
	for (i = k; i < lines + 1; i++) prt("", i + 6, 0);

	/* Assume "no current page" */
	put_str("        ", 5, 20);

	/* Visual reminder of "more items" */
	if (st_ptr->stock_num > lines)
	{
		/* Show "more" reminder (after the last object) */
		prt("-more-", k + 6, 3);

		/* Indicate the "current page" */
		put_str(format("(Page %d)", store_top / lines + 1), 5, 20);
	}
}

/*
 * Display player's gold
 */
static void store_prt_gold(void)
{
	char out_val[64];

	prt("Gold Remaining: ", Term->rows - 5, 52);

	(void)strnfmt(out_val, sizeof(out_val), "%9ld", (long)p_ptr->au);
	prt(out_val, Term->rows - 5, 68);
}

static void store_prt_invest(void)
{
	char out_val[64];

	prt("Total Investment: ", Term->rows - 1, 50);

	sprintf(out_val, "%9ld", (long)st_ptr->total_buy);
	prt(out_val, Term->rows - 1, 68);
}


/*
 * Display store (after clearing screen)
 */
static void display_store(void)
{
	char buf[DESC_LEN];


	/* Clear screen */
	(void)Term_clear();

	/* The "Home" is special */
	if (store_num == STORE_HOME)
	{
		/* Put the owner name */
		put_str("Your Home", 3, 30);

		/* Label the item descriptions */
		put_str("Item Description", 5, 3);

		/* If showing weights, show label */
		if (show_weights)
		{
			put_str("Weight", 5, 71);
		}
	}

	/* The Inn is special */
	else if (store_num == STORE_INN)
	{
		display_inn();
		return;
	}

	/* Normal stores */
	else
	{
		cptr store_name = (f_name + f_info[FEAT_SHOP_HEAD + store_num].name);
		cptr race_name = race_info[ot_ptr->owner_race].title;

		/* Put the owner name and race */
		(void)strnfmt(buf, sizeof(buf), "%s (%s)", ot_ptr->owner_name, race_name);
		put_str(buf, 3, 10);

		/* Show the max price in the store (above prices) */
		(void)strnfmt(buf, sizeof(buf), "%^s (%ld)", store_name, (long)(ot_ptr->max_cost));
		prt(buf, 3, 50);

		/* Label the object descriptions */
		put_str("Item Description", 5, 3);

		/* If showing weights, show label */
		if (show_weights)
		{
			put_str("Weight", 5, 61);
		}

		/* Label the asking price (in stores) */
		put_str("Price", 5, 72);
	}

	/* Draw in the inventory */
	display_inventory();

	/* Display the current gold */
	store_prt_gold();
}



/*
 * Helper function to browse store inventory
 */
static void store_browse_aux(void)
{
	int lines = stock_displayed;

	if (st_ptr->stock_num <= lines)
	{
		/* Nothing to see */
		msg_print("Entire inventory is shown.");
	}

	else if (store_top == 0)
	{
		/* Page 2 */
		store_top = lines;

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

	char buf[DESC_LEN];
	char o_name[DESC_LEN];
	char out_val[DESC_LEN];

	object_type *o_ptr;

	/* Repeated command -- Get the item index */
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
	(void)strnfmt(buf, sizeof(buf), "(Items %c-%c, ESC to exit) %s",
		store_to_label(0), store_to_label(st_ptr->stock_num - 1),
		pmt);

	/* Ask until done */
	while (TRUE)
	{
		bool verify;

		/* Escape */
		if (!get_com(buf, &which)) return (FALSE);

		/* Hack -- Browse the store inventory */
		if (which == ' ')
		{
			store_browse_aux();
			continue;
		}

		/* Note verify */
		verify = (my_isupper((unsigned char)which) ? TRUE : FALSE);

		/* Lowercase */
		which = my_tolower((unsigned char)which);

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
		(void)strnfmt(out_val, sizeof(out_val), "Try %s?", o_name);

		/* Query */
		if (!get_check(out_val)) return (FALSE);

		/* Done */
		break;
	}

	/* Save item */
	(*com_val) = item;

	/* Save this command */
	repeat_push(*com_val);

	/* Success */
	return (TRUE);
}

/*
 * The greeting a shopkeeper gives the character says a lot about his
 * general attitude.
 */
static void prt_welcome(void)
{
	int attitude = rsqrt(st_ptr->total_buy);
	int i = 0;

	char short_name[DESC_LEN];
	char buf[DESC_LEN];
	char welcome[DESC_LEN];

	if      (attitude >= 1400) i = 10;
	else if (attitude >= 1050) i = 9;
	else if (attitude >=  800) i = 8;
	else if (attitude >=  575) i = 7;
	else if (attitude >=  425) i = 6;
	else if (attitude >=  300) i = 5;
	else if (attitude >=  200) i = 4;
	else if (attitude >=  125) i = 3;
	else if (attitude >=   75) i = 2;
	else if (attitude >=   33) i = 1;

	/* Welcome the character */
	if (i)
	{
		/* Get the short shopkeeper name */
		shopkeeper_short_name(short_name, ot_ptr->owner_name);

		/* Get either the character title or name */
		if (i == 4) strcpy(buf, get_title(40, FALSE, TRUE));
		else        strcpy(buf, op_ptr->full_name);

		/* Get the comment */
		strcpy(welcome, comment_welcome[i]);

		/* Balthazar says "Welcome" */
		prt(format(welcome, short_name, buf), 0, 0);
	}
}


/*
 * Increase the insult counter and get angry if necessary
 */
static int increase_insults(void)
{
	/* Increase insults */
	st_ptr->insult_cur++;

	/* Become insulted */
	if (st_ptr->insult_cur > ot_ptr->insult_max)
	{
		/* Throw the character out of the store */
		say_comment_4();

		/* Reset insults */
		st_ptr->insult_cur = 0;

		/* Reopen a few days from now */
		st_ptr->store_open = turn + rand_range(20000L, 40000L);

		/* Closed */
		return (TRUE);
	}

	/* Not closed */
	return (FALSE);
}


/*
 * Decrease the insult counter
 */
static void decrease_insults(void)
{
	/* Decrease insults */
	if (st_ptr->insult_cur) st_ptr->insult_cur--;
}


/*
 * The shop-keeper has been insulted
 */
static int haggle_insults(void)
{
	/* Increase insults */
	if (increase_insults()) return (TRUE);

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
static int get_haggle(cptr pmt, s32b *poffer, s32b last_offer, s32b price,
	int final)
{
	char buf[DESC_LEN];

	/* Clear old increment if necessary */
	if (!allow_inc) last_inc = 0L;

	/* Agreed-upon price */
	if ((last_offer == price) || (final))
	{
		(void)strnfmt(buf, sizeof(buf), "%s", pmt);
	}

	/* Old (negative) increment, and not final */
	else if (last_inc < 0)
	{
		(void)strnfmt(buf, sizeof(buf), "%s [-%ld]", pmt, (long)(ABS(last_inc)));
	}

	/* Old (positive) increment, and not final */
	else if (last_inc > 0)
	{
		(void)strnfmt(buf, sizeof(buf), "%s [+%ld]", pmt, (long)(ABS(last_inc)));
	}

	/* Normal haggle */
	else
	{
		(void)strnfmt(buf, sizeof(buf), "%s", pmt);
	}

	/* Ask until done */
	while (TRUE)
	{
		cptr p;

		char out_val[DESC_LEN];

		/* Default */
		strcpy(out_val, "");

		/* Move cursor */
		move_cursor(0, 0);

		/* Ask the user for a response */
		if (!get_string(buf, out_val, sizeof(out_val))) return (FALSE);

		/* Skip leading spaces */
		for (p = out_val; *p == ' '; p++) /* loop */ ;

		/* Empty response */
		if (*p == '\0')
		{
			/* Accept current price */
			if ((final) || (last_offer == price))
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
			s32b i;

			/* Extract a number */
			i = atol(p);

			/* Handle "incremental" number */
			if ((*p == '+' || *p == '-'))
			{
				/* Allow increments */
				if ((allow_inc) && (i))
				{
					/* Use the given "increment" */
					*poffer += i;
					last_inc = i;
					break;
				}
			}

			/* Handle normal number (if valid) */
			else if (i > 0L)
			{
				/* Use the given "number" */
				*poffer = i;
				last_inc = 0L;
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
static bool receive_offer(cptr pmt, s32b *poffer, s32b last_offer,
	bool buying, s32b price, int final)
{
	int factor = 1;
	if (buying) factor = -1;

	/* Haggle till done */
	while (TRUE)
	{
		/* Get a haggle (or cancel) */
		if (!get_haggle(pmt, poffer, last_offer, price, final)) return (FALSE);

		/* Ignore attempts to revise an agreed price */
		if ((last_offer == price) &&
		    (((*poffer) * factor) > (last_offer * factor)))
		{
			/* "Hey, I thought we had a deal" */
			say_comment_6(FALSE);
			continue;
		}

		/* Acceptable offer */
		if (((*poffer) * factor) >= (last_offer * factor)) break;

		/* Insult, and check for kicked out */
		if (haggle_insults()) return (FALSE);

		/* Reject offer (correctly) */
		(*poffer) = last_offer;
	}

	/* Success */
	return (TRUE);
}



/*
 * Haggle for an item.  -RAK-, -LM-
 *
 * "buying" is TRUE if the store is purchasing something, FALSE otherwise.
 *
 * Return TRUE if an agreement was reached.
 */
static bool haggle(s32b *price, s32b final_price, bool buying,
	bool *perfect_haggle)
{
	s32b last_offer, offer = 0;
	s32b diff, percent, percent2;
	s32b min_per, max_per;

	s32b cur_price = *price;

	bool done = FALSE;
	bool final = (*price == final_price);
	bool bad_offer = FALSE;

	char prompt[DESC_LEN];
	char offer_desc[DESC_LEN];



	/* Minimum and maximum effective bargaining percentages */
	min_per =     ot_ptr->haggle_per / 2;
	max_per = 3 * ot_ptr->haggle_per / 2;


	/* We need to haggle some */
	if (!final)
	{
		/* "expected offer" differs as much from final_price as cur_price does. */
		diff = final_price - cur_price;
		last_offer = final_price + diff;

		/* Adjust expected offer slightly to make small items easier */
		if (buying) last_offer += 15;
		else        last_offer -= 5;

		/* Adjust expected offer by a "bargain difficulty factor". */
		last_offer += (diff * (bargain_difficulty - 100L) / 100L);
	}
	else
	{
		last_offer = final_price;
	}


	/* No offer yet */
	offer = 0;

	/* No incremental haggling yet */
	allow_inc = FALSE;

	/* Haggle until done */
	while (TRUE)
	{
		/* Build a price description and a prompt */
		if (final)
		{
			if (buying) strcpy(offer_desc, "Final offer");
			else        strcpy(offer_desc, "Final price");
			strcpy(prompt, "Press RETURN to accept this price, or ESC to cancel.");
		}
		else if (last_offer == cur_price)
		{
			if (buying) strcpy(offer_desc, "Offer");
			else        strcpy(offer_desc, "Price");
			strcpy(prompt, "Press RETURN to accept this price, or ESC to cancel.");

			bad_offer = TRUE;
		}
		else
		{
			if (buying) strcpy(offer_desc, "Offer");
			else        strcpy(offer_desc, "Asking");
			if (buying) strcpy(prompt, "What price do you ask?");
			else        strcpy(prompt, "What do you offer?");
		}

		/* Get a legal offer, or cancel */
		while (TRUE)
		{
			/* Display the price */
			put_str(format("%s : %ld    ", offer_desc, (long)cur_price), 1, 0);

			/* Display a prompt, get an offer, allow cancel */
			if (!receive_offer(prompt, &offer, last_offer, buying,
			                   cur_price, final))
			{
				return (FALSE);
			}

			/* Player is offering to rob himself */
			if (( buying && offer < cur_price) ||
			    (!buying && offer > cur_price))
			{
				/* "I must have heard you wrong" */
				say_comment_6(TRUE);

				/* Ignore that offer */
				offer = last_offer;
			}

			/* Player accepts the current price */
			else if (offer == cur_price)
			{
				/* Stop haggling (if final, or price has changed) */
				if (final || cur_price != *price) done = TRUE;

				/* Stop haggling if we foolishly accept a bad offer */
				else if (bad_offer) done = TRUE;

				break;
			}

			/* No agreement yet -- haggle some more */
			else break;
		}

		/* Stop haggling */
		if (final || done) break;


		/*
		 * Calculate the percentage of the remaining price difference the
		 * player is surrendering (minimum of 1).
		 */
		percent2 = percent = MAX(1, 100L * ABS(last_offer - offer) /
		                     MAX(1, ABS(last_offer - cur_price)));

		/* Character is not being reasonable enough */
		if (percent < min_per)
		{
			/* We're insulted */
			if (haggle_insults())
			{
				/* Our tolerance is exhausted */
				return (FALSE);
			}
		}

		/*
		 * Character is being too generous.  Special case -- if "expected
		 * offer" is negative, applying this rule is inappropriate.
		 */
		else if ((percent > max_per) && (last_offer >= 0L))
		{
			/* Sensing weakness, we don't adjust our price as much */
			percent2 = (percent + max_per) / 2;
		}

		/* If not the first offer... */
		if (cur_price != *price)
		{
			/* ... randomize our generosity a little */
			percent2 = rand_range(percent2 - 2, percent2 + 2);
		}

		/* Improve our price (by at least 1) */
		if (buying)
		{
			diff = MAX(1, (last_offer - offer) * percent2 / percent);
		}
		else
		{
			diff = MIN(-1, (last_offer - offer) * percent2 / percent);
		}

		/* Improve price, but don't give anything away */
		cur_price += diff;
		if      ( buying && cur_price > offer) cur_price = offer;
		else if (!buying && cur_price < offer) cur_price = offer;


		/* We've been haggled to our final price */
		if (( buying && cur_price >= final_price) ||
		    (!buying && cur_price <= final_price))
		{
			/* Jump to the final price */
			cur_price = final_price;
			final = TRUE;
		}

		/* Player offers a price that we're happy to take */
		else if (( buying && offer < (final_price + *price) / 2) ||
		         (!buying && offer > (final_price + *price) / 2))
		{
			/* Accept it gleefully */
			cur_price = offer;
		}

		/* Save this offer */
		last_offer = offer;

		/* Hack -- allow incremental haggling */
		allow_inc = TRUE;

		/* Clear and update the price information line */
		prt(format("                               Your last offer : %ld",
			(long)last_offer), 1, 0);

		/* Gleefully accept */
		if ((cur_price == offer) && (!final)) say_comment_1(FALSE);

		/* Haggle */
		else
		{
			if (buying) say_comment_3(cur_price, final);
			else        say_comment_2(cur_price, final);
		}
	}


	/* We had to haggle */
	if (*price != final_price)
	{
		/* Successful haggle */
		if (cur_price == final_price)
		{
			/* Shopkeeper respects the character more */
			if (st_ptr->good_buy < MAX_SHORT) st_ptr->good_buy++;

			/* Often forget a bad buy */
			if ((st_ptr->bad_buy > 0) && (rand_int(5 + st_ptr->bad_buy) >= 5))
			   st_ptr->bad_buy--;

			/* Note haggling success */
			*perfect_haggle = TRUE;
		}

		/* Failed haggle */
		else
		{
			/* Shopkeeper respects the character less */
			if (st_ptr->bad_buy  < MAX_SHORT) st_ptr->bad_buy++;
			if (st_ptr->good_buy >         0) st_ptr->good_buy--;
		}

		/* Redisplay wares */
		display_inventory();
	}

	/* Adjust the price */
	*price = cur_price;


	/* Success */
	return (TRUE);
}


/*
 * Buying and selling -- interact with the shopkeeper.
 *
 * Return TRUE if purchase is successful.
 */
static int buy_and_sell(object_type *o_ptr, s32b *price, bool buying,
	bool *perfect_haggle)
{
	s32b purse, cur_price, final_price;
	int markup = 0;

	bool final  = FALSE;


    /* For some, the stores don't want your items */
	if (birth_stores_only_sell && buying)
	{
	    *price = 0;
	    return 2; /* Fixed price */
	}

	/* Determine markup */
	if (o_ptr->ident & (IDENT_FIXED)) markup = 0;
	else markup = price_markup(object_value(o_ptr));

	/* Calculate starting and final price */
	cur_price   = price_item(o_ptr, ot_ptr->max_inflate, buying, markup);
	final_price = price_item(o_ptr, ot_ptr->min_inflate, buying, 0);


	/* Get the owner's payout limit */
	purse = (s32b) (ot_ptr->max_cost);

	/* Store owner figures he's probably going to have to pay full purse */
	if ((buying) && ((final_price + cur_price) / 2 >= purse) &&
	    (final_price >= purse))
	{
		/* Message */
		msg_print("I wish I could offer more, but - alas! - my funds are limited.");
		message_flush();

		/* Offer full purse */
		final_price = purse;

		final = TRUE;
	}

	/* Already haggled */
	else if ((!buying) && (o_ptr->ident & (IDENT_FIXED)))
	{
		prt("You instantly agree upon the price.", 1, 25);
		final = TRUE;
	}

	/* Item's price is well-known */
	else if (!markup)
	{
		prt("You quickly agree upon the price.", 1, 25);
		final = TRUE;
	}

	/* No haggle option */
	else if (never_haggle)
	{
		/* Final price now includes a markup */
		final_price = price_item(o_ptr, ot_ptr->min_inflate, buying, markup);

		/* Lambs to the slaughter */
		if (buying)
		{
			char short_name[DESC_LEN];

			/* Get the short shopkeeper name */
			shopkeeper_short_name(short_name, ot_ptr->owner_name);

			prt(format("You accept %s's offer.", short_name), 1, 25);
		}
		else
		{
			prt("You accept the marked price.", 1, 25);
		}

		final = TRUE;
	}


	/* When store is buying, the final price cannot exceed purse */
	if ((buying) && (final_price > purse)) final_price = purse;

	/* Jumped straight to final price */
	if (final)
	{
		/* Final price */
		cur_price = final_price;
	}

	/* Haggle for the whole pile */
	cur_price   *= o_ptr->number;
	final_price *= o_ptr->number;

	/* Save the initial price */
	*price = cur_price;

	/* Haggle */
	if (haggle(price, final_price, buying, perfect_haggle))
	{
		/* Return 2 if fixed price, 1 if haggled */
		return (final ? 2 : 1);
	}

	/* Cancelled */
	return (0);
}

static void store_retire()
{
	long retire_cost;
	char short_name[DESC_LEN];
	cptr s = "";


	/* Show that the shelves are empty */
	display_inventory();

	/* Get the short shopkeeper name */
	shopkeeper_short_name(short_name, ot_ptr->owner_name);

	/* Get the retirement cost */
	retire_cost = 50000L + 10L * ot_ptr->max_cost;

	/* Character can offer it */
	if (p_ptr->au >= retire_cost)
	{
		/* The shopkeeper may be willing to retire */
		msg_format("%s may be willing to retire if you offer enough incentive:  %ld in gold should be sufficient.", short_name, retire_cost);

		/* See if the player is up for it */
		if (get_check(format("Offer %ld gold?", retire_cost)))
		{
			/* Shopkeeper decides not to retire */
			if (one_in_(2))
			{
				msg_format("%s thanks you for the offer, but declines to retire.",
					short_name);
				msg_format("It'll take a while until I can replenish my stores.");
			}

			/* Retire */
			else
			{
				/* Message */
				msg_format("%s retires.", short_name);

				/* Give the gold */
				p_ptr->au -= retire_cost;

				/* Shuffle the store */
				store_shuffle(store_num);

				/* Get the new short shopkeeper name */
				shopkeeper_short_name(short_name, ot_ptr->owner_name);

				/* Has things to sell */
				s = ", the new shopkeeper, brings in things to sell";

				/* Rebuild the stock */
				store_maint(store_num, TRUE);

				/* Flush any pending messages */
				message_flush();

				/* Start over */
				store_top = 0;

				/* Re-display the store */
				display_store();

				/* Message */
				msg_format("%s%s.", short_name, s);
			}
		}
	}

}



/*
 * Buy an object from a store
 */
static void store_purchase(void)
{
	int n;
	int amt;
	int markup;
	bool accept;
	int item, item_new;
	bool perfect_haggle = FALSE;

	s32b price;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[DESC_LEN];

	cptr prompt;
	int lines = stock_displayed;


	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		if (store_num == STORE_HOME)
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
	if (store_num == STORE_HOME)
	{
		prompt = "Which item do you want to take?";
	}
	else
	{
		prompt = "Which item are you interested in?";
	}

	/* Get the item index */
	if (!get_stock(&item, prompt)) return;

	/* Get the actual object */
	o_ptr = &st_ptr->stock[item];


	/* Determine markup */
	if (o_ptr->ident & (IDENT_FIXED)) markup = 0;
	else markup = price_markup(object_value(o_ptr));

	/* Determine price */
	price = price_item(o_ptr, ot_ptr->min_inflate, FALSE, markup);
	if (price < 1) price = 1;

	/* Check gold (but only if item is fixed price) */
	if ((p_ptr->au < price) && (!markup) && (store_num != STORE_HOME))
	{
		msg_print("You do not have the gold to buy it.");
		return;
	}


	/* You can get multiples of anything, except staffs */
	if (o_ptr->tval != TV_STAFF)
	{
		/* Assume we can buy (or get) it all */
		s32b tmp_amt = o_ptr->number;

		/* Object does not need to be haggled for */
		if (((!markup) || (never_haggle)) && (store_num != STORE_HOME))
		{
			/* Restrict maximum quantity based on price */
			if (tmp_amt > p_ptr->au / price) tmp_amt = p_ptr->au / price;
		}

		/* Get a quantity */
		amt = (int)get_quantity(NULL, 0, tmp_amt);
	}
	else
	{
		amt = 1;
	}

	/* Allow user abort */
	if (amt <= 0) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Get desired object */
	object_copy(i_ptr, o_ptr);

	/*
	 * Hack -- If a rod or wand, allocate a portion of the total maximum
	 * timeouts or charges.
	 */
	if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD)) &&
	     (amt < o_ptr->number))
	{
		i_ptr->pval = o_ptr->pval * amt / o_ptr->number;
	}

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
		/* Output a message if item is not displayed */
		if (!((item >= store_top) && (item < store_top + lines)))
		{
			/* Describe the object (fully) */
			object_desc_store(o_name, sizeof(o_name), i_ptr, TRUE, 3);

			/* Message */
			msg_format("Buying %s (%c).", o_name, store_to_label(item));
			message_flush();
		}

		/* If item is displayed, just highlight it */
		else
		{
			item_buy_idx = item;
			item_buy_num = amt;
			display_entry(item);
		}

		/* Haggle for a final price */
		accept = buy_and_sell(i_ptr, &price, FALSE, &perfect_haggle);

		/* Cancel highlight */
		item_buy_idx = -1;

		/* Clear the highlight if no purchase */
		if (!accept) display_entry(item);


		/* Hack -- Got kicked out */
		if (st_ptr->store_open >= turn) return;

		/* Player wants it */
		if (accept)
		{
			/* Player can afford it */
			if (p_ptr->au >= price)
			{
				/* Say "okay" (when haggled to final price) */
				if (accept < 2 && perfect_haggle)
					say_comment_1(perfect_haggle);

				/* Be happy */
				decrease_insults();

				/*
				 * If object just sold was not part of a store-bought stack,
				 * give credit.  XXX XXX
				 */
				if (!(o_ptr->ident & (IDENT_FIXED))) st_ptr->total_buy += price;

				/* Spend the money */
				p_ptr->au -= price;

				/* Update the display */
				store_prt_gold();

				/* Buying an object makes you aware of it */
				object_aware(i_ptr);

				/* Perfect haggles fix price of the entire pile */
				if (perfect_haggle) o_ptr->ident |= (IDENT_FIXED);

				/* Everything sold has a fixed price  XXX XXX */
				i_ptr->ident |= (IDENT_FIXED);


				/* Combine / Reorder the pack (later) */
				p_ptr->notice |= (PN_COMBINE | PN_REORDER);

				/* Object is never more valuable than normal */
				if (i_ptr->cost_adjust > 100) i_ptr->cost_adjust = 100;

				/* Describe the transaction */
				object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

				/* Message */
				msg_format("You bought %s (%c) for %ld gold.",
					o_name, store_to_label(item),
					(long)price);

				/* Give it to the player */
				item_new = inven_carry(i_ptr);

				/* Paranoia -- handle errors */
				if (item_new < 0) return;

				/* Describe the final result */
				object_desc(o_name, sizeof(o_name), &inventory[item_new], TRUE, 3);

				/* Message */
				msg_format("You have %s (%c).",
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
					store_retire();
				}

				/* The object is gone */
				else if (st_ptr->stock_num != n)
				{
					/* Only one screen left */
					if (st_ptr->stock_num <= lines)
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

			/* Player cannot afford it */
			else
			{
				/* Unhighlight the item */
				display_entry(item);

				/* Simple message (no insult) */
				msg_print("You do not have enough gold.");
			}
		}
	}

	/* Home is much easier */
	else
	{
		/* Give it to the player */
		item_new = inven_carry(i_ptr);

		/* Paranoia -- handle errors */
		if (item_new < 0) return;

		/* Describe just the result */
		object_desc(o_name, sizeof(o_name), &inventory[item_new], TRUE, 3);

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
			if (st_ptr->stock_num <= lines)
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
	bool accept;
	int item, item_pos;
	int amt;

	s32b price, value, guess;
	bool perfect_haggle = FALSE;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	cptr q, s;

	char o_name[DESC_LEN];


	/* Home */
	q = "Drop which item?";

	/* Real store */
	if (store_num != STORE_HOME)
	{
		/* New prompt */
		q = "Sell which item?";

		/* Only allow items the store will buy */
		item_tester_hook = store_will_buy;
	}

	/* Get an item */
	if (store_num == STORE_HOME) s = "You have nothing to leave at home.";
	else                         s = "You have nothing that I want.";
	if (p_ptr->schange)
	{
		if (!get_item(&item, q, s, (USE_INVEN))) return;
	}
	else
	{
		if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;
	}
	item_to_object(o_ptr, item);


	/* Hack -- Cannot remove cursed objects */
	if ((item >= INVEN_WIELD) && cursed_cling(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Get a quantity */
	amt = (int)get_quantity(NULL, 0, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Get a copy of the object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/*
	 * Hack -- If a rod or wand, allocate some of the total maximum
	 * timeouts or charges to those being sold.
	 */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
	{
		i_ptr->pval = o_ptr->pval * amt / o_ptr->number;
	}

	/* Get a full description */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Is there room in the store (or the home?) */
	if (!store_check_num(i_ptr))
	{
		if (store_num == STORE_HOME)
		{
			msg_print("Your home is full.");
		}
		else
		{
			msg_print("I have not the room in my store to keep it.");
		}
		return;
	}


	/* Real store */
	if (store_num != STORE_HOME)
	{
		/* Describe the transaction */
		msg_format("Selling %s (%c).", o_name, index_to_label(item));
		message_flush();


		/* The shopkeeper has a 50% chance of discovering hidden curses */
		if ((i_ptr->flags3 & (TR3_CURSE_HIDDEN)) && (one_in_(2)) &&
		    (make_cursed_ego_item(i_ptr)))
		{
			char buf[DESC_LEN];

			int attitude = rsqrt(st_ptr->total_buy);

			int quantity = o_ptr->number;

			/* Identify the object XXX XXX */
			object_aware(i_ptr);
			object_known(i_ptr);

			/* Get name of object kind */
			strip_name(buf, i_ptr->k_idx);

			/* Describe the object (briefly) */
			object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 0);


			/* What is this?!?! */
			msg_format("The shopkeeper stares hard at your %s...   ", buf);
			if (!p_ptr->image)
				msg_format("Suddenly, it mutates!  It is, in fact, an evilly cursed %s!", o_name);

			/* This doesn't belong in my store! */
			if (attitude < 50)
			{
				msg_print("How dare you peddle this hideous crap here?  GET OUT OF MY STORE!");

				/* Reopen a few hours from now */
				st_ptr->store_open = turn + rand_range(2000L, 4000L);

				/* Leave */
				leave_store = TRUE;

			}
			else if (attitude < 150)
			{
				msg_print("AAARGH!  Don't show this sort of thing to me!  It's an insult!");

				/* Become more insulted */
				increase_insults();
				if (attitude < 100) increase_insults();
			}
			else
			{
				msg_print("*Scowls*  Now this I just CANNOT buy.  Please don't let me see it again!");
			}

			/* Hack -- copy changes back to original object  XXX XXX */
			object_copy(o_ptr, i_ptr);

			/* Restore old quantity */
			o_ptr->number = quantity;

			/* Cancel action */
			return;
		}


		/* Haggle for it */
		accept = buy_and_sell(i_ptr, &price, TRUE, &perfect_haggle);

		/* Kicked out */
		if (st_ptr->store_open >= turn) return;

		/* Sold... */
		if (accept)
		{
			/* When we haggle well, the whole pile becomes fixed-price */
			if (perfect_haggle) o_ptr->ident |= (IDENT_FIXED);

			/* Say "okay" (when haggled to final price) */
			if (accept < 2 && perfect_haggle) say_comment_1(perfect_haggle);

			/* Be happy */
			decrease_insults();

			/* Get some money */
			p_ptr->au += price;

			/* Update the display */
			store_prt_gold();

			/* Get the "apparent" value */
			guess = object_value(i_ptr) * i_ptr->number;

			/* Identify original object */
			object_aware(o_ptr);
			object_known(o_ptr);


			/* Discover hidden curses */
			if ((i_ptr->flags3 & (TR3_CURSE_HIDDEN)) &&
			    (make_cursed_ego_item(i_ptr)))
			{
				/* Save old quantity */
				int quantity = o_ptr->number;

				/* Identify sold object */
				object_aware(i_ptr);
				object_known(i_ptr);

				/* Original object also changes  XXX XXX */
				object_copy(o_ptr, i_ptr);

				/* Restore old quantity */
				o_ptr->number = quantity;

				/* Note change */
				msg_print("A hidden curse triggers; the object is not what it appeared to be!");
			}

			/* Douse (legal) lights */
			if (item_tester_light_source(o_ptr))
			{
				o_ptr->flags3 &= ~(TR3_IS_LIT);
			}

			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

			/* Equippy chars */
			p_ptr->redraw |= (PR_EQUIPPY);

			/* Get local object */
			i_ptr = &object_type_body;

			/* Get a copy of the object */
			object_copy(i_ptr, o_ptr);

			/* Modify quantity */
			i_ptr->number = amt;

			/* Erase the inscription */
			i_ptr->note = 0;

			/* Remove special inscription, if any */
			i_ptr->inscrip = 0;

			/* Allocate charges of rods/wands */
			distribute_charges(o_ptr, i_ptr, amt);

			/* Price becomes fixed */
			i_ptr->ident |= (IDENT_FIXED);

			/* Get the "actual" value */
			value = object_value(i_ptr) * i_ptr->number;

			/* Get the description all over again */
			object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 2);

			/* Describe the result (in message buffer) */
			msg_format("You sold %s (%c) for %ld gold.",
				o_name, index_to_label(item), (long)price);

			/* Analyze the prices (and comment verbally) */
			purchase_analyze(price, value, guess);

			/* Take the object from the player */
			inven_item_increase(item, -amt);
			inven_item_describe(item);
			inven_item_optimize(item);

			/* Store grows richer from good deals  -JM */
			if (price < value) st_ptr->total_buy += value - price;

			/* For niceness, the store doesn't get poorer from bad deals. */

			/* Handle stuff */
			handle_stuff();

			/* The store gets that (known) object */
			item_pos = store_carry(i_ptr);

			/* Update the display */
			if (item_pos >= 0)
			{
				/* Redisplay wares */
				display_inventory();
			}
		}
	}

	/* The home */
	else
	{
		/* Distribute charges of wands/rods */
		distribute_charges(o_ptr, i_ptr, amt);

		/* Describe */
		msg_format("You drop %s (%c).", o_name, index_to_label(item));

		/* Take it from the player's inventory */
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);

		/* Handle stuff */
		handle_stuff();

		/* Let the home carry it */
		item_pos = store_carry(i_ptr);

		/* Update store display */
		if (item_pos >= 0)
		{
			/* Redisplay wares */
			display_inventory();
		}
	}
}


/*
 * Get an object in a store to inspect.   Original code by -JDL-
 * (from Zangband).
 */
static void store_inspect(void)
{
	int item;

	object_type *o_ptr;

	cptr prompt = "Examine which item?";


	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		if (store_num == STORE_HOME) msg_print("Your home is empty.");
		else                         msg_print("I am currently out of stock.");
		return;
	}

	/* Get the item index */
	if (!get_stock(&item, prompt)) return;

	/* Get the actual item */
	o_ptr = &st_ptr->stock[item];

	/* Hack -- if it's a book, browse it  -clefs- */
	if (o_ptr->tval == mp_ptr->spell_book)
	{
		do_cmd_browse_aux(o_ptr);
	}

	/* Examine the item. */
	do_cmd_observe(o_ptr, (store_num == STORE_HOME ? FALSE : TRUE));
}



/*
 * Process a command in a store
 *
 * Note that we must allow the use of a few "special" commands in the
 * stores which are not allowed in the dungeon, and we must disable
 * some commands which are allowed in the dungeon but not in the
 * stores, to prevent chaos.
 *
 * WARNING:
 * Be sure to check your new command in both the original and the
 * roguelike keysets!
 */
static void store_process_command(bool inn_cmd)
{
	/* Assume command is legal */
	bool legal = TRUE;

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

		/* Ignore */
		case '\n':
		case '\r':
		{
			break;
		}

		/* Browse */
		case ' ':
		{
			store_browse_aux();
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
			if (!inn_cmd) store_purchase();
			else inn_purchase(0);
			break;
		}

		/* Drop (Sell) */
		case 'd':
		{
			if (!inn_cmd) store_sell();
			else legal = FALSE;
			break;
		}

		/* Learn about quest monster */
		case 'r':
		{
			/* We're in the inn, have a un-researched quest, and have enough money */
			if ((inn_cmd) && (p_ptr->cur_quest) &&
			    !(q_info[p_ptr->cur_quest].flags & (0x01)) &&
			    (p_ptr->au >= (1L + p_ptr->power + p_ptr->max_depth) * 20L))
			{
				int r_idx = q_info[quest_num(p_ptr->cur_quest)].r_idx;
				monster_race *r_ptr = &r_info[r_idx];
				monster_lore *l_ptr = &l_list[r_idx];

				char buf[2048];
				u32b race_flags = (RF3_ANIMAL | RF3_EVIL |
				                   RF3_UNDEAD | RF3_DRAGON |
				                   RF3_DEMON | RF3_GIANT |
				                   RF3_TROLL | RF3_ORC);

				/* Mark the quest (always) */
				q_info[p_ptr->cur_quest].flags |= (0x01);

				/* Must be questing for a real monster */
				if (!r_idx) break;

				/* Take some money */
				p_ptr->au -= MAX(2L, p_ptr->power + p_ptr->max_depth) * 20L;

				/* We have now "seen" this monster */
				if (!l_ptr->sights) l_ptr->sights = 1;

				/* We now know about the monster's native depth */
				l_ptr->flags |= (LORE_KNOWN_DEPTH);

				/* Print name and characters */
				roff_top(r_idx, 7);

				/* Get the monster description */
				(void)my_strcpy(buf, r_text + r_ptr->text, sizeof(buf));

				/* Print description */
				c_roff(TERM_L_BLUE, format("\n%s\n\n", buf), 0, 0);


				/* Learn basic facts */
				l_ptr->flags3 |= (r_ptr->flags3 & (race_flags));

				/* Rumour has it... */
				roff("Rumour has it that this", 0, 0);

				/* Racial information */
				if (r_ptr->flags3 & (RF3_ANIMAL))     roff(" natural", 0, 0);
				if (r_ptr->flags3 & (RF3_EVIL))       roff(" evil", 0, 0);
				if (r_ptr->flags3 & (RF3_UNDEAD))     roff(" undead", 0, 0);

				if (r_ptr->flags3 & (RF3_DRAGON))     roff(" dragon", 0, 0);
				else if (r_ptr->flags3 & (RF3_DEMON)) roff(" demon", 0, 0);
				else if (r_ptr->flags3 & (RF3_GIANT)) roff(" giant", 0, 0);
				else if (r_ptr->flags3 & (RF3_TROLL)) roff(" troll", 0, 0);
				else if (r_ptr->flags3 & (RF3_ORC))   roff(" orc", 0, 0);
				else roff(" creature", 0, 0);

				/* Depth */
				roff(" is normally found ", 0, 0);

				if ((depth_in_feet) && (use_metric))
				{
					roff(format("at depths of %d meters.",
						r_ptr->level * 15), 0, 0);
				}
				else if (depth_in_feet)
				{
					roff(format("at depths of %d feet.",
						r_ptr->level * 50), 0, 0);
				}
				else
				{
					roff(format("on dungeon level %d.",
						r_ptr->level), 0, 0);
				}
			}
			else legal = FALSE;
			break;
		}

		/* Examine */
		case 'I':
		{
			if (!inn_cmd) store_inspect();
			else legal = FALSE;
			break;
		}


		/* Use special commands */
		case '*':
		{
			/* Only in normal stores */
			if ((!inn_cmd) && (store_num != STORE_HOME))
			{
				char ch;

				/* Display investment */
				store_prt_invest();

				/* Prompt */
				prt("Command (I to invest money, C to cycle inventory, ESC to return)?", 0, 0);

				ch = inkey(FALSE);

				/* Invest money */
				if ((ch == 'I') || (ch == 'i'))
				{
					s32b invest, tmp32s;
					char short_name[DESC_LEN];

					/* Clear prompt, move cursor */
					prt("", 0, 0);

					invest = get_quantity("Invest how much of your money in the store?", 0, p_ptr->au);

					/* Stay legal */
					if (invest < 1) break;

					prt("", 0, 0);

					/* Remove money */
					p_ptr->au -= invest;

					/* Make store richer */
					st_ptr->total_buy += invest;

					/* Shopkeeper improves his opinion of the character */
					st_ptr->good_buy += div_round(invest, 10000L);
					st_ptr->bad_buy  -= div_round(invest, 30000L);

					/* Reduce insults */
					tmp32s = div_round(invest, 1000L);
					st_ptr->insult_cur -=
						MIN(tmp32s, st_ptr->insult_cur);

					/* Get the short shopkeeper name */
					shopkeeper_short_name(short_name, ot_ptr->owner_name);

					/* Shopkeeper is happy. */
					if (invest < 100)
					{
						msg_format("%s says \"Well, every little bit helps.\".", short_name);
					}
					else if (invest < 5000)
					{
						msg_format("%s thanks you for your investment.", short_name);
					}
					else if (invest < 25000)
					{
						msg_format("%s is grateful to you for your investment.", short_name);
						msg_print("This store may be able to buy higher quality goods.");
					}
					else if (invest < 100000L)
					{
						msg_format("%s is very grateful!", short_name);
						msg_print("This store will certainly be able to buy higher quality goods.");
					}
					else
					{
						msg_format("%s is exceedingly grateful!", short_name);
						msg_print("This store will stock some of the finest goods available!");
					}

					message_flush();

					/* Re-display the store */
					do_cmd_redraw();
					display_store();
				}
				else if (ch == 'c' || ch == 'C')  /* Clear store goods, for a price */
				{
					int cost = 0, i, num, price, markup;
					char prompt[80];
					object_type *o_ptr;

					/* Determine total cost of items in store */
					for (i = 0; i < st_ptr->stock_num; i++)
					{
						o_ptr = &st_ptr->stock[i];

						if (!o_ptr) continue;

						markup = price_markup(object_value(o_ptr));  /* Give price assuming good bargaining */

						/* Get the listed price */
						price = price_item(o_ptr, ot_ptr->min_inflate, FALSE, markup);

						cost +=  price * o_ptr->number;
					}

					/* Make sure there's a large cost associated with cycling inventory */
					cost += 100000;

					/* Prompt player with total cost */
					if (cost > p_ptr->au)
					{
						prt("You don't have that kind of money.", 1, 0);
						ch = inkey(TRUE);
						break;
					}

					/* Verify player wants to spend the money */
					sprintf(prompt, "Cycling the inventory will cost %d gold.  Are you sure?", cost);
					if (!get_check(prompt))  break;

					/* Reduce gold */
					p_ptr->au -= cost;

					/* Destroy all inventory */
					for (i = st_ptr->stock_num; i >= 0; i--)
					{
						num = st_ptr->stock[i].number;
						if (num > 0)
						{
							store_item_increase(i, -num);
							store_item_optimize(i);
						}
					}

					/* Update goods */
					store_maint(store_num, TRUE);

					/* Credit store with some investment */
					st_ptr->total_buy += cost / 2;

					/* Refresh store */
					display_store();

				}

				/* Clear the message line, wait for other commands */
				else
				{
					prt("", 0, 0);
				}
			}
			break;
		}

		/* Get quests (stop changing these to a, b, c - it breaks roguelike keys) */
		case '1':
		{
			if (inn_cmd) inn_purchase(1);
			break;
		}
		case '2':
		{
			if (inn_cmd) inn_purchase(2);
			break;
		}
		case '3':
		{
			if (inn_cmd) inn_purchase(3);
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

		/* Hack -- toggle windows */
		case KTRL('E'):
		{
			toggle_inven_equip();
			break;
		}


		/*** Use various objects ***/

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
			if (inn_cmd) p_ptr->get_help_index = HELP_QUEST;
			else         p_ptr->get_help_index = HELP_STORE;

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

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals('\0');
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

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}


		/* Hack -- Unknown command */
		default:
		{
			legal = FALSE;
			break;
		}
	}

	/* Not a legal command */
	if (!legal)
	{
		if (!inn_cmd)
		{
			if (store_num != STORE_HOME)
			     msg_print("That command does not work in stores.");
			else msg_print("That command does not work in your home.");
		}
		else msg_print("That command does not work in the Inn.");
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int which;

	int tmp_chr;


	/* Disturb */
	disturb(0, 0);

	/* Verify a store */
	if (!cave_shop_bold(py, px))
	{
		msg_print("You see no store here.");
		return;
	}

	/* Hack -- Extract the store code */
	which = (cave_feat[py][px] - FEAT_SHOP_HEAD);


	/* Hack -- Check the "locked doors" */
	if ((birth_no_stores) || (store[which].store_open >= turn))
	{
		msg_print("The doors are locked.");
		return;
	}


	/* Forget the view */
	forget_view();


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
	ot_ptr = &owners[store_num][st_ptr->owner];

	/* Start at the beginning */
	store_top = 0;


	/* Turn off the main view and center the display */
	display_change(DSP_REMEMBER | DSP_LOCK | DSP_CX, 80, 0);


	/* Sort the inventory */
	(void)reorder_pack(-1, store_num, FALSE);

	/* We don't have enough space to show the full inventory */
	if (Term->rows < st_ptr->stock_num + 12)
	{
		/* If switching to the tall display will solve the problem, do so */
		if (term_size_min[WINDOW_DISPLAY][1] >= st_ptr->stock_num + 12)
		{
			display_change(DSP_TALL, 0, 0);
		}
	}

	/* Either show all the merchandise, */
	if (Term->rows - 12 >= st_ptr->stock_num) stock_displayed = Term->rows - 12;

	/* Or whatever we can fit, up to half the total */
	else stock_displayed = MIN(Term->rows - 12, (STORE_INVEN_MAX + 1) / 2);

	/* Display the store */
	display_store();

	/* "Welcome" */
	prt_welcome();

	/* Do not leave */
	leave_store = FALSE;

	/*
	 * Play a special sound if entering the
	 * home, otherwise play a generic store sound.
	 */
	if (store_num == STORE_HOME) sound(MSG_STORE_HOME);
	else                         sound(MSG_STORE_ENTER);


	/* Interact with player */
	while (!leave_store)
	{
		/* Hack -- Clear line 1 */
		prt("", 1, 0);

		/* Hack -- Check the charisma */
		tmp_chr = p_ptr->stat_use[A_CHR];

		/* Clear */
		if (store_num != STORE_INN)
		{
			clear_from(Term->rows - 4);
		}
		else
		{
			clear_from(Term->rows - 1);
		}

		/* Basic commands  XXX */
		put_str(" ESC)   Exit from building", Term->rows - 2, 0);


		/* Browse if necessary */
		if (st_ptr->stock_num > stock_displayed)
		{
			prt(" SPACE) Show more stock", Term->rows - 3, 0);
		}

		/* Commands */
		if (store_num != STORE_INN)
		{
			if (store_num != STORE_HOME)
			{
				prt(" g) Purchase an item", Term->rows - 3, 31);
				prt(" d) Sell an item", Term->rows - 2, 31);
				prt(" *) Use special commands", Term->rows - 1, 0);
			}
			else
			{
				prt(" g) Get an item", Term->rows - 3, 31);
				prt(" d) Drop an item", Term->rows - 2, 31);
			}

			prt("   I) Inspect an item", Term->rows - 3, 56);
		}
		else
		{
			/* We have an unresearched quest, and have the gold to get information */
			if ((p_ptr->cur_quest) &&  !(q_info[p_ptr->cur_quest].flags & (0x01)) &&
				(p_ptr->au >= (1L + p_ptr->power + p_ptr->max_depth) * 20L))
			{
				put_str(format("   r)   Learn about quest monster (price %ld gold)",
					(1L + p_ptr->power + p_ptr->max_depth) * 20L), Term->rows - 3, 0);
			}
		}
		prt("   ?) Get help.", Term->rows - 2, 56);

		/* Prompt */
		prt("You may: ", Term->rows - 4, 0);

		/* Get a command */
		request_command(TRUE);

		/* Process the command */
		store_process_command(store_num == STORE_INN);

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Pack Overflow XXX XXX XXX */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK - p_ptr->pack_size_reduce;

			object_type *o_ptr = &inventory[item];

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

				object_type *i_ptr;
				object_type object_type_body;

				char o_name[DESC_LEN];


				/* Give a message */
				msg_print("Your pack overflows!");

				/* Get local object */
				i_ptr = &object_type_body;

				/* Grab a copy of the object */
				object_copy(i_ptr, o_ptr);

				/* Describe it */
				object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

				/* Message */
				msg_format("You drop %s (%c).", o_name, index_to_label(item));

				/* Remove it from the player's inventory */
				inven_item_increase(item, -255);
				inven_item_describe(item);
				inven_item_optimize(item);

				/* Handle stuff */
				handle_stuff();

				/* Let the home carry it */
				item_pos = store_carry(i_ptr);

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

		/* Hack -- get kicked out of the store */
		if (st_ptr->store_open >= turn) leave_store = TRUE;
	}

	/* Leave the store sound */
	sound(MSG_STORE_LEAVE);

	/* Take a turn */
	p_ptr->energy_use = 100;


	/* Hack -- Cancel automatic command */
	p_ptr->command_new = 0;

	/* Hack -- Cancel "see" mode */
	p_ptr->command_see = FALSE;


	/* Turn on the main view and restore the previous display settings */
	display_change(DSP_RESTORE | DSP_UNLOCK, 0, 0);
}



/*
 * Shuffle one of the stores.
 */
void store_shuffle(int which)
{
	int i, j;


	/* Ignore home and inn */
	if ((which == STORE_HOME) || (which == STORE_INN)) return;


	/* Save the store index */
	store_num = which;

	/* Activate that store */
	st_ptr = &store[store_num];

	/* Pick a new owner */
	for (j = st_ptr->owner; j == st_ptr->owner; )
	{
		st_ptr->owner = (byte)rand_int(MAX_OWNERS);
	}

	/* Activate the new owner */
	ot_ptr = &owners[store_num][st_ptr->owner];


	/* Reset the owner data */
	st_ptr->insult_cur = 0;
	st_ptr->store_open = 0;

	/* New owner has some idea of what kind of haggler the player is */
	st_ptr->good_buy = 2 * st_ptr->good_buy / 3;
	st_ptr->bad_buy  = 2 * st_ptr->bad_buy / 3;


	/* Discount all the items */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &st_ptr->stock[i];

		/* Sell all old items for "half price" */
		o_ptr->cost_adjust = 50;

		/* Clear the "fixed price" flag */
		o_ptr->ident &= ~(IDENT_FIXED);

		/* Inscribe the object as "on sale" */
		o_ptr->note = quark_add("on sale");
	}
}


/*
 * Maintain the inventory at the stores.
 */
void store_maint(int num, bool full)
{
	int i;

	/* Save the level rating */
	int old_rating = level_rating;

	/* Activate the store */
	store_num = num;

	/* Get the store information */
	st_ptr = &store[store_num];

	/* Get the owner information */
	ot_ptr = &owners[store_num][st_ptr->owner];

	/* Ignore any store that sells nothing */
	if ((st_ptr->stock_start < 0) || (st_ptr->stock_end < 0) ||
	    (st_ptr->stock_end - st_ptr->stock_start < 0))
	{
		return;
	}

	/* We're rebuilding the stock */
	if (full)
	{
		/* Total forgiveness */
		st_ptr->insult_cur = 0;

		/* Adjust all the stock items */
		for (i = st_ptr->stock_start; i <= st_ptr->stock_end; i++)
		{
			/* Adjust this stock item */
			store_adjust(i);
		}
	}

	/* We're just updating the stock */
	else
	{
		/* Store keeper slowly forgives the player over time */
		if ((st_ptr->insult_cur) && (one_in_(4))) st_ptr->insult_cur--;

		/* Adjust a random stock item */
		store_adjust(0);
	}

	/* During gameplay, handle other effects */
	if (!full)
	{
		/* Shopkeepers talk to each other about big spenders */
		for (i = 0; i < MAX_STORES; i++)
		{
			/* Credit at all stores is at least 1/9th that at the best */
			if (st_ptr->total_buy < store[i].total_buy / 9)
			{
				st_ptr->total_buy = store[i].total_buy / 9;
			}
		}

		/* Shopkeepers talk to each other about good hagglers */
		for (i = 0; i < MAX_STORES; i++)
		{
			if (st_ptr->good_buy < store[i].good_buy / 3)
			{
				st_ptr->good_buy = store[i].good_buy / 3;
			}
		}


	}

	/* Hack -- Restore the level rating */
	level_rating = old_rating;
}


/*
 * Initialize a store
 */
void store_init(int which)
{
	int k;

	/* Save the store index */
	store_num = which;

	/* Activate that store */
	st_ptr = &store[store_num];


	/* Pick an owner */
	st_ptr->owner = (byte)rand_int(MAX_OWNERS);

	/* Activate the new owner */
	ot_ptr = &owners[store_num][st_ptr->owner];


	/* Initialize the store */
	st_ptr->insult_cur = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;
	st_ptr->store_open = 0L;
	st_ptr->total_buy = 0L;

	/* Nothing in stock */
	st_ptr->stock_num = 0;

	/* Clear any old items */
	for (k = 0; k < st_ptr->stock_size; k++)
	{
		object_wipe(&st_ptr->stock[k]);
	}
}


/*
 * Handle what happens in the home over time
 *
 * We should probably worry about the player knowing what is going on in the house while away, but we don't.
 */
void process_world_aux_home(void)
{
	object_type *o_ptr;
	int i;

	st_ptr = &store[STORE_HOME];

	for (i = 0; i < st_ptr->stock_num; i++)
	{
		o_ptr = &st_ptr->stock[i];

		/* Handle athelas spoilage */
		if (o_ptr->tval == TV_FOOD && o_ptr->sval == SV_FOOD_ATHELAS)
		{
			/* Should wilt in around 1 day */
			if (one_in_(10000 / o_ptr->number))
			{
				store_item_increase(i, -1);
				store_item_optimize(i);
			}
		}
	}
}
