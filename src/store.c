/**
 * \file store.c
 * \brief Store stocking
 *
 * Copyright (c) 1997 Robert A. Koeneke, James E. Wilson, Ben Harrison
 * Copyright (c) 2007 Andi Sidwell
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cave.h"
#include "cmds.h"
#include "debug.h"
#include "game-event.h"
#include "game-world.h"
#include "hint.h"
#include "init.h"
#include "monster.h"
#include "obj-desc.h"
#include "obj-gear.h"
#include "obj-ignore.h"
#include "obj-info.h"
#include "obj-knowledge.h"
#include "obj-make.h"
#include "obj-pile.h"
#include "obj-power.h"
#include "obj-slays.h"
#include "obj-tval.h"
#include "obj-util.h"
#include "player-calcs.h"
#include "player-history.h"
#include "player-spell.h"
#include "store.h"
#include "target.h"
#include "ui-display.h"
#include "world.h"

/**
 * ------------------------------------------------------------------------
 * Constants and definitions
 * ------------------------------------------------------------------------ */


/**
 * Array[MAX_STORES] of stores in the current town
 */
struct store *stores;

/**
 * Array[MAX_STORES] of stores as originally read from the config file
 */
struct store *stores_init;

/**
 * The hints array
 */
struct hint *hints;

/**
 * The lies array
 */
struct hint *lies;


void do_store_maint(struct store *s, bool init);


static const char *obj_flags[] = {
	"NONE",
	#define OF(a) #a,
	#include "list-object-flags.h"
	#undef OF
	NULL
};

/**
 * Return the store instance at the given location
 */
struct store *store_at(struct chunk *c, struct loc grid)
{
	if (square_isshop(c, grid))
		return &stores[square_shopnum(cave, grid)];

	return NULL;
}


/**
 * Create a new store.
 */
static struct store *store_new(int idx) {
	struct store *s = mem_zalloc(sizeof *s);
	s->sidx = idx;
	s->stock_size = z_info->store_inven_max;
	return s;
}

/**
 * Return the store with the given index, or NULL if there is none
 */
struct store *get_store_by_idx(int idx) {
	for(int i=0; i< MAX_STORES; i++) {
		struct store *store = stores + i;
		if ((int)store->sidx == idx)
			return store;
	};
	return NULL;
}

/**
 * Return the store with the given name, or NULL if there is none
 */
struct store *get_store_by_name(const char *name) {
	if (!name)
		return NULL;
	for(int i=0; i< MAX_STORES; i++) {
		struct store *store = stores + i;
		if (streq(store->name, name))
			return store;
	};
	return NULL;
}

/**
 * Get rid of stores at cleanup. Gets rid of everything.
 */
void cleanup_stores(void)
{
	int i;

	for (int t=0; t<z_info->town_max; t++) {
		/* Free the store inventories */
		for (i = 0; i < MAX_STORES; i++) {
			/* Get the store */
			struct store *store = &t_info[t].stores[i];

			/* Free the store inventory */
			object_pile_free(store->stock_k);
			object_pile_free(store->stock);
			if (store->always_table) {
				mem_free(store->always_table);
				store->always_table = NULL;
			}
			if (store->normal_table) {
				mem_free(store->normal_table);
				store->always_table = NULL;
			}
			string_free((void *)store->name);
			store->name = NULL;
		}

		mem_free(t_info[t].stores);
		t_info[t].stores = NULL;
	}
	stores = NULL;
}


/**
 * ------------------------------------------------------------------------
 * Edit file parsing
 * ------------------------------------------------------------------------ */


/** store.txt **/

static enum parser_error parse_store(struct parser *p) {
	struct store *h = parser_priv(p);
	struct store *s;
	unsigned int idx = parser_getuint(p, "index") - 1;

	if (idx >= MAX_STORES)
		return PARSE_ERROR_OUT_OF_BOUNDS;

	s = store_new(parser_getuint(p, "index") - 1);
	s->name = string_make(parser_getstr(p, "name"));
	s->next = h;
	s->open = true;
	parser_setpriv(p, s);
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_slots(struct parser *p) {
	struct store *s = parser_priv(p);
	if (!s)
		return PARSE_ERROR_MISSING_RECORD_HEADER;

	s->normal_stock_min = parser_getuint(p, "min");
	s->normal_stock_max = parser_getuint(p, "max");

	return PARSE_ERROR_NONE;
}

static enum parser_error parse_danger(struct parser *p) {
	struct store *s = parser_priv(p);
	if (!s)
		return PARSE_ERROR_MISSING_RECORD_HEADER;

	s->low_danger = parser_getuint(p, "low");
	s->high_danger = parser_getuint(p, "high");

	return PARSE_ERROR_NONE;
}

static enum parser_error parse_turnover(struct parser *p) {
	struct store *s = parser_priv(p);
	s->turnover = parser_getuint(p, "turnover");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_max(struct parser *p) {
	struct store *s = parser_priv(p);
	s->stock_size = parser_getuint(p, "max");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_closed(struct parser *p) {
	struct store *s = parser_priv(p);
	s->open = false;
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_item_table(struct parser *p, size_t *num, size_t *size, struct store_entry **table) {
	int tval = tval_find_idx(parser_getsym(p, "tval"));
	struct object_kind *kind = NULL;
	int sval = -1;

	if (parser_hasval(p, "sval")) {
		sval = lookup_sval(tval, parser_getsym(p, "sval"));
		kind = lookup_kind(tval, sval);
		if (!kind)
			return PARSE_ERROR_UNRECOGNISED_SVAL;
	}

	random_value rarity = { 10000, 0, 0, 0 };


	/* Expand if necessary */
	if (!(*num)) {
		*size = 16;
		*table = mem_zalloc(*size * sizeof **table);
	} else if (num >= size) {
		*size += 8; 
		*table = mem_realloc(*table, *size * sizeof **table);
	}

	/* Read rarity is present */
	if (parser_hasval(p, "rarity")) {
		rarity = parser_getrand(p, "rarity");
		/* Negative base in this case doesn't mean that the number is negative, only that the base is.
		 */
		if (rarity.base < 0)
			rarity.base += (rarity.m_bonus + (rarity.dice * (rarity.sides + 1)));
	}

	(*table)[*num].rarity = rarity;
	(*table)[*num].kind = kind;
	(*table)[*num].tval = tval;
	(*num)++;

	return PARSE_ERROR_NONE;
}

static enum parser_error parse_always(struct parser *p) {
	struct store *s = parser_priv(p);
	return parse_item_table(p, &s->always_num, &s->always_size, &s->always_table);
}

static enum parser_error parse_normal(struct parser *p) {
	struct store *s = parser_priv(p);
	return parse_item_table(p, &s->normal_num, &s->normal_size, &s->normal_table);
}

static enum parser_error parse_owner(struct parser *p) {
	struct store *s = parser_priv(p);
	unsigned int maxcost = parser_getuint(p, "purse");
	unsigned int greed = parser_getuint(p, "greed");
	char *name = string_make(parser_getstr(p, "name"));
	struct owner *o;

	if (!s)
		return PARSE_ERROR_MISSING_RECORD_HEADER;

	o = mem_zalloc(sizeof *o);
	o->oidx = (s->owners ? s->owners->oidx + 1 : 1);
	o->next = s->owners;
	o->name = name;
	o->max_cost = maxcost;
	o->greed = greed;
	s->owners = o;
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_buy(struct parser *p) {
	struct store *s = parser_priv(p);
	struct object_buy *buy;

	if (!s)
		return PARSE_ERROR_MISSING_RECORD_HEADER;

	buy = mem_zalloc(sizeof(*buy));
	buy->tval = tval_find_idx(parser_getstr(p, "base"));
	buy->next = s->buy;
	s->buy = buy;
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_buy_flag(struct parser *p) {
	struct store *s = parser_priv(p);
	int flag;

	if (!s)
		return PARSE_ERROR_MISSING_RECORD_HEADER;

	flag = lookup_flag(obj_flags, parser_getsym(p, "flag"));

	if (flag == FLAG_END) {
		return PARSE_ERROR_INVALID_FLAG;
	} else {
		struct object_buy *buy = mem_zalloc(sizeof(*buy));

		buy->flag = flag;
		buy->tval = tval_find_idx(parser_getstr(p, "base"));
		buy->next = s->buy;
		s->buy = buy;

		return PARSE_ERROR_NONE;
	}
}

struct parser *init_parse_stores(void) {
	struct parser *p = parser_new();
	parser_setpriv(p, NULL);
	parser_reg(p, "store uint index str name", parse_store);
	parser_reg(p, "owner uint greed uint purse str name", parse_owner);
	parser_reg(p, "slots uint min uint max", parse_slots);
	parser_reg(p, "turnover uint turnover", parse_turnover);
	parser_reg(p, "size uint max", parse_max);
	parser_reg(p, "closed", parse_closed);
	parser_reg(p, "normal sym tval ?sym sval ?rand rarity", parse_normal);
	parser_reg(p, "always sym tval ?sym sval ?rand rarity", parse_always);
	parser_reg(p, "buy str base", parse_buy);
	parser_reg(p, "buy-flag sym flag str base", parse_buy_flag);
	parser_reg(p, "danger uint low uint high", parse_danger);
	return p;
}

static errr run_parse_stores(struct parser *p) {
	return parse_file_quit_not_found(p, "store");
}

static errr finish_parse_stores(struct parser *p) {
	stores = parser_priv(p);
	parser_destroy(p);
	return 0;
}

static struct file_parser store_parser = {
	"store",
	init_parse_stores,
	run_parse_stores,
	finish_parse_stores,
	NULL
};

/**
 * ------------------------------------------------------------------------
 * Other init stuff
 * ------------------------------------------------------------------------ */


static struct store *flatten_stores(struct store *store_list) {
	struct store *s;
	struct store *stores_local = mem_zalloc(MAX_STORES * sizeof(*stores_local));

	for (s = store_list; s; s = s->next) {
		if (s->sidx < MAX_STORES)
			memcpy(&stores_local[s->sidx], s, sizeof(*s));
	}

	while (store_list) {
		s = store_list->next;
		/* No need to free the sub-allocated memory, as this is passed on
		 * to the array of stores */
		mem_free(store_list);
		store_list = s;
	}

	return stores_local;
}

void store_init(void)
{
	event_signal_message(EVENT_INITSTATUS, 0, "Initializing stores...");
	if (run_parser(&store_parser)) quit("Can't initialize stores");
	stores_init = flatten_stores(stores);
}

void stores_copy(struct store *src)
{
	for (int t = 0; t<z_info->town_max; t++) {
		struct store *stores = t_info[t].stores;
		for (int i = 0; i < MAX_STORES; i++) {
			struct store *s;
			s = &stores[i];

			s->name = string_make(src[i].name);

			s->stock = NULL;
			s->stock_k = NULL;

			struct store_entry *always_table = mem_alloc(sizeof(struct store_entry) * src[i].always_size);
			memcpy(always_table, src[i].always_table, (sizeof(struct store_entry) * src[i].always_size));
			s->always_table = always_table;
			struct store_entry *normal_table = mem_alloc(sizeof(struct store_entry) * src[i].normal_size);
			memcpy(normal_table, src[i].normal_table, (sizeof(struct store_entry) * src[i].normal_size));
			s->normal_table = normal_table;
		}
	}
}

/* Reset the stores to the initial state.
 * The stores_init array contains one entry per store, as read from store.txt.
 * This must be duplicated per town.
 */
void store_reset(void) {
	int i;
	struct store *s;

	assert(t_info);
	assert(z_info->town_max);
	for (int t = 0; t<z_info->town_max; t++)
	{
		if (!t_info[t].stores)
			t_info[t].stores = mem_alloc(MAX_STORES * sizeof(*stores));
		stores = t_info[t].stores;
		memcpy(stores, stores_init, MAX_STORES * sizeof(*stores));
	}

	stores_copy(stores_init);

	for (int t = 0; t<z_info->town_max; t++) {
		stores = t_info[t].stores;
		for (i = 0; i < MAX_STORES; i++) {
			s = &stores[i];
			s->banreason = "";
			s->layaway_idx = -1;

			s->max_danger = s->low_danger + randint0(1 + s->high_danger - s->low_danger);
			store_shuffle(s);
			object_pile_free(s->stock);
			s->stock = NULL;
			if (i == STORE_HOME)
				continue;
			do_store_maint(s, true);
		}
	}
}

struct init_module store_module = {
	.name = "store",
	.init = store_init,
	.cleanup = cleanup_stores
};





/**
 * Check if a given item kind is an always-stocked item.
 */
static bool store_is_staple(struct store *s, struct object_kind *k) {
	size_t i;

	assert(s);
	assert(k);

	for (i = 0; i < s->always_num; i++) {
		struct object_kind *l = s->always_table[i].kind;
		if (k == l)
			return true;
	}

	return false;
}

/**
 * Check if a given item kind is an always-stocked or sometimes-stocked item.
 */
static bool store_can_carry(struct store *store, struct object_kind *kind) {
	size_t i;

	for (i = 0; i < store->normal_num; i++) {
		if (store->normal_table[i].kind == kind)
			return true;
	}

	return store_is_staple(store, kind);
}




/**
 * ------------------------------------------------------------------------
 * Utilities
 * ------------------------------------------------------------------------ */


/* Randomly select one of the entries in an array */
#define ONE_OF(x)	x[randint0(N_ELEMENTS(x))]


/**
 * ------------------------------------------------------------------------
 * Flavour text stuff
 * ------------------------------------------------------------------------ */


/**
 * Messages for reacting to purchase prices.
 */
static const char *comment_worthless[] =
{
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!",
	"The shopkeeper wails in anguish!",
	"The shopkeeper beats his head against the counter."
};

static const char *comment_bad[] =
{
	"Damn!",
	"You fiend!",
	"The shopkeeper curses at you.",
	"The shopkeeper glares at you."
};

static const char *comment_accept[] =
{
	"Okay.",
	"Fine.",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!"
};

static const char *comment_good[] =
{
	"Cool!",
	"You've made my day!",
	"The shopkeeper sniggers.",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly."
};

static const char *comment_great[] =
{
	"Yipee!",
	"I think I'll retire!",
	"The shopkeeper jumps for joy.",
	"The shopkeeper smiles gleefully.",
	"Wow.  I'm going to name my new villa in your honour."
};






/**
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(int price, int value, int guess)
{
	/* Item was worthless, but we bought it */
	if ((value <= 0) && (price > value))
		msgt(MSG_STORE1, "%s", ONE_OF(comment_worthless));

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
		msgt(MSG_STORE2, "%s", ONE_OF(comment_bad));

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (value < (4 * guess)) && (price < value))
		msgt(MSG_STORE3, "%s", ONE_OF(comment_good));

	/* Item was a great bargain, and we got away with it */
	else if ((value > guess) && (price < value))
		msgt(MSG_STORE4, "%s", ONE_OF(comment_great));
}




/**
 * ------------------------------------------------------------------------
 * Check if a store will buy an object
 * ------------------------------------------------------------------------ */


/**
 * Determine if the current store will purchase the given object
 *
 * Note that a shop-keeper must refuse to buy "worthless" objects
 */
static bool store_will_buy(struct store *store, const struct object *obj)
{
	struct object_buy *buy;

	/* Home accepts anything */
	if (store->sidx == STORE_HOME) return true;

	/* Ignore apparently worthless items, except no-selling {??} items */
	if (object_value(obj, 1) <= 0 && !(OPT(player, birth_no_selling) &&
									   tval_has_variable_power(obj) &&
									   !object_icons_known(obj))) {
		return false;
	}

	/* Don't buy quest items */
	if (of_has(obj->flags, OF_QUEST_SPECIAL))
		return false;

	/* No buy list means we buy anything */
	if (!store->buy) return true;

	/* Run through the buy list */
	for (buy = store->buy; buy; buy = buy->next) {
		/* Wrong tval */
		if (buy->tval != obj->tval) continue;

		/* No flag means we're good */
		if (!buy->flag) return true;

		/* OK if the object is known to have the flag */
		if (of_has(obj->flags, buy->flag) &&
			object_flag_is_known(obj, buy->flag))
			return true;
	}

	/* Not on the list */
	return false;
}


/**
 * ------------------------------------------------------------------------
 * Basics: pricing, generation, etc.
 * ------------------------------------------------------------------------ */

/* Returns true if you own the store (= owner is the first in the list) */
bool you_own(struct store *store)
{
	return (store->owner == store->owners);
}


/**
 * Determine the price of an object (qty one) in a store.
 *
 *  store_buying == true  means the shop is buying, player selling
 *               == false means the shop is selling, player buying
 *
 * This function never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * object, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- the black market always charges more than it should.
 */
int price_item(struct store *store, const struct object *obj,
			   bool store_buying, int qty)
{
	int adjust = 100;
	int price;
	struct owner *proprietor;

	if (!store) {
		return 0;
	}

	proprietor = store->owner;

	/* Get the value of the stack of wands, or a single item */
	if (tval_can_have_charges(obj)) {
		price = MIN(object_value_real(obj, qty), object_value(obj, qty));
	} else {
		price = MIN(object_value_real(obj, 1), object_value(obj, 1));
	}

	/* Worthless items */
	if (price <= 0) {
		return 0;
	}

	if (store->sidx == STORE_HQ) {
		adjust = 110;
	} else if (!you_own(store)) {
		/* This adjusts the price (in either direction) based on CHA
		 * and level, with CHA being more important at low level and
		 * the proportion that is dependent on level increasing at high
		 * level (that is, at level 25 less than half of the advantage
		 * of high level has occurred).
		 */
		adjust += ((3000 - (player->lev * player->lev)) /
			((3 + player->state.stat_ind[STAT_CHR]) * 10)) + store->owner->greed;

		/* The black market is always a worse deal (for the rest of them) */
		if (store->sidx == STORE_B_MARKET) {
			int bmf = (player->bm_faction >= 0) ? (20 / (player->bm_faction + 1)) : 50;
			adjust = (adjust * 2) + 30 + bmf;
		} else {
			/* Expensive stuff gets a further hike.
			 * 'Expensive' is based on the max cost.
			 * It's still always better than the BM, though
			 **/
			int value = object_value_real(obj, 1);
			int costly = proprietor->max_cost / 30;
			int scale = proprietor->max_cost / 250;
			int chscale = proprietor->max_cost / 7500;
			if (value > costly) {
				int div = (scale + (chscale * player->state.stat_ind[STAT_CHR]));
				if (div) {
					int markup = adjust + ((object_value_real(obj, 1) - costly) / (scale + (chscale * player->state.stat_ind[STAT_CHR])));
					adjust = MIN(((adjust * 2) + 30), markup);
				}
			}
		}
	}

	/* Shop is buying */
	if (store_buying) {
		/* Set the factor */
		adjust = 10000 / adjust;

		/* Check for no_selling option */
		if (OPT(player, birth_no_selling))
			return 0;

	} else {
		/* Re-evaluate if we're selling */
		if (tval_can_have_charges(obj)) {
			price = object_value_real(obj, qty);
		} else {
			price = object_value_real(obj, 1);
		}
	}

	/* Compute the final price (with rounding) */
	price = (price * adjust + 50L) / 100L;

	/* Now convert price to total price for non-wands */
	if (!tval_can_have_charges(obj)) {
		price *= qty;
	}

	/* Layaway - later, to avoid rounding errors. 90% same day, +25% per day */
	if (obj == &store->stock[store->layaway_idx]) {
		int base = price- (price/10);
		int today = turn / (10L * z_info->day_length);
		int days = today - store->layaway_day;
		price = base + (days * (price/4));
	}

	/* Now limit the price to the purse limit */
	if (store_buying && proprietor->max_cost && (price > proprietor->max_cost * qty)) {
		price = proprietor->max_cost * qty;
	}

	/* Note -- Never become "free" */
	if (price <= 0) {
		return qty;
	}

	/* Return the price */
	return price;
}


/**
 * Special "mass production" computation.
 */
static int mass_roll(int times, int max)
{
	int i, t = 0;

	assert(max > 1);

	for (i = 0; i < times; i++)
		t += randint0(max);

	return (t);
}


/**
 * Some cheap objects should be created in piles.
 */
static void mass_produce(struct object *obj)
{
	int size = 1;
	int cost = object_value_real(obj, 1);

	/* Analyze the type */
	switch (obj->tval)
	{
		/* Food, batteries, and lights */
		case TV_FOOD:
		case TV_MUSHROOM:
		case TV_BATTERY:
		case TV_LIGHT:
		{
			if (cost <= 5L) size += mass_roll(3, 5);
			if (cost <= 20L) size += mass_roll(3, 5);
			break;
		}

		case TV_PILL:
		case TV_CARD:
		{
			if (cost <= 60L) size += mass_roll(3, 5);
			if (cost <= 240L) size += mass_roll(1, 5);
			break;
		}

		case TV_AMMO_6:
		case TV_AMMO_9:
		case TV_AMMO_12:
		{
			if (cost <= 5L)
				size = randint1(2) * 20;         /* 20-40 in 20s */
			else if (cost > 5L && cost <= 50L)
				size = randint1(4) * 10;         /* 10-40 in 10s */
			else if (cost > 50 && cost <= 500L)
				size = randint1(4) * 5;          /* 5-20 in 5s */
			else
				size = 1;

			break;
		}
	}

	/* Save the total pile size */
	obj->number = MIN(size, obj->kind->base->max_stack);
}


/**
 * Sort the store inventory into an ordered array.
 */
void store_stock_list(struct store *store, struct object **list, int n)
{
	bool home = (store->sidx != STORE_HOME);
	int list_num;
	int num = 0;

	for (list_num = 0; list_num < n; list_num++) {
		struct object *current, *first = NULL;
		for (current = store->stock; current; current = current->next) {
			int i;
			bool possible = true;

			/* Skip objects already allocated */
			for (i = 0; i < num; i++)
				if (list[i] == current)
					possible = false;

			/* If still possible, choose the first in order */
			if (!possible)
				continue;
			else if (earlier_object(first, current, home))
				first = current;
		}

		/* Allocate and count the stock */
		list[list_num] = first;
		if (first)
			num++;
	}
}

/**
 * Allow a store object to absorb another object
 */
static void store_object_absorb(struct object *old, struct object *new)
{
	int total = old->number + new->number;

	/* Combine quantity, lose excess items */
	old->number = MIN(total, old->kind->base->max_stack);

	/* If rods are stacking, add the charging timeouts */
	if (tval_can_have_timeout(old) && (!tval_is_light(old)))
		old->timeout += new->timeout;

	/* If wands/devices are stacking, combine the charges */
	if (tval_can_have_charges(old))
		old->pval += new->pval;

	object_origin_combine(old, new);

	/* Fully absorbed */
	object_delete(&new);
}


/**
 * Check to see if the shop will be carrying too many objects
 *
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.  Before, one could "nuke" objects this way, by
 * adding them to a pile which was already full.
 */
bool store_check_num(struct store *store, const struct object *obj)
{
	struct object *stock_obj;

	/* Free space is always usable */
	if (store->stock_num < store->stock_size) return true;

	/* The "home" acts like the player */
	if (store->sidx == STORE_HOME) {
		for (stock_obj = store->stock; stock_obj; stock_obj = stock_obj->next) {
			/* Can the new object be combined with the old one? */
			if (object_similar(stock_obj, obj, OSTACK_PACK))
				return true;
		}
	} else {
		/* Normal stores do special stuff */
		for (stock_obj = store->stock; stock_obj; stock_obj = stock_obj->next) {
			/* Can the new object be combined with the old one? */
			if (object_similar(stock_obj, obj, OSTACK_STORE))
				return true;
		}
	}

	/* But there was no room at the inn... */
	return false;
}


/**
 * Add an object to the inventory of the Home.
 *
 * Also note that it may not correctly "adapt" to "knowledge" becoming
 * known: the player may have to pick stuff up and drop it again.
 */
void home_carry(struct object *obj)
{
	struct object *temp_obj;
	struct store *store = &stores[STORE_HOME];

	/* Check each existing object (try to combine) */
	for (temp_obj = store->stock; temp_obj; temp_obj = temp_obj->next) {
		/* The home acts just like the player */
		if (object_similar(temp_obj, obj, OSTACK_PACK)) {
			/* Save the new number of items */
			object_absorb(temp_obj, obj);
			return;
		}
	}

	/* No space? */
	if (store->stock_num >= store->stock_size) return;

	/* Insert the new object */
	pile_insert(&store->stock, obj);
	pile_insert(&store->stock_k, obj->known);
	store->stock_num++;
}


/**
 * Add an object to a real stores inventory.
 *
 * If the object is "worthless", it is thrown away (except in the home).
 *
 * If the object cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.  Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an object to a "fixed" price stack will not change the fixed price.
 *
 * Returns the object inserted (for ease of use) or NULL if it disappears
 */
struct object *store_carry(struct store *store, struct object *obj, bool maintain)
{
	unsigned int i;
	u32b value;
	struct object *temp_obj, *known_obj = obj->known;

	struct object_kind *kind = obj->kind;

	/* Evaluate the object */
	if (object_is_carried(player, obj))
		value = object_value(obj, 1);
	else
		value = object_value_real(obj, 1);

	/* Faulty/Worthless items "disappear" when sold */
	if (value <= 0)
		return NULL;

	/* Erase the inscription */
	obj->note = 0;
	known_obj->note = 0;

	/* Some item types require maintenance */
	if (tval_is_light(obj)) {
		/* If timeout == pval then it's at full power and can be stocked as is.
		 * Lights that can be stopped but not refueled can also be stocked as is.
		 * Lights that don't require charging can also be stocked as is.
		 * 
		 * Otherwise, if it's a light that can't be stopped once lit, just discard
		 * it to avoid turn-by-turn shop item maintenance (or 'magically' stopping
		 * the fuze without explanation). These are the ones with "time" set.
		 * 
		 * The remainder can be recharged, so do so (set timeout to pval).
		 */
		if ((of_has(obj->flags, OF_NO_FUEL)) || (obj->timeout == obj->pval) || (!of_has(obj->flags, OF_TAKES_FUEL))) {
			; // as is
		} else if (randcalc(obj->time, 0, AVERAGE) > 0) {
			return NULL; // discard
		} else {
			obj->timeout = obj->pval; // charge
		}
	} else if (tval_can_have_timeout(obj)) {
		obj->timeout = 0;
	} else if (tval_is_launcher(obj)) {
		obj->known->pval = obj->pval;
	} else if (tval_can_have_charges(obj)) {
		/* If the store can stock this item kind, we recharge */
		if (store_can_carry(store, obj->kind)) {
			int charges = 0;

			/* Calculate the recharged number of charges */
			for (i = 0; i < obj->number; i++)
				charges += randcalc(kind->charge, 0, RANDOMISE);

			/* Use recharged value only if greater */
			if (charges > obj->pval)
				obj->pval = charges;
		}
	}

	for (temp_obj = store->stock; temp_obj; temp_obj = temp_obj->next) {
		/* Can the existing items be incremented? */
		if (object_similar(temp_obj, obj, OSTACK_STORE)) {

			/* If this is 'maintenance' (rather than selling an item), discard some duplicates */
			if (maintain) {
				if (!(one_in_(1+temp_obj->number))) {
					return NULL;
				}
			}

			/* Absorb (some of) the object */
			store_object_absorb(temp_obj->known, known_obj);
			obj->known = NULL;
			store_object_absorb(temp_obj, obj);

			/* All done */
			return temp_obj;
		}
	}

	/* No space? */
	if (store->stock_num >= store->stock_size)
		return NULL;

	/* Insert the new object */
	pile_insert(&store->stock, obj);
	pile_insert(&store->stock_k, known_obj);
	store->stock_num++;
	return obj;
}


void store_delete(struct store *s, struct object *obj, int amt)
{
	struct object *known_obj = obj->known;

	if (obj->number > amt) {
		obj->number -= amt;
		known_obj->number -= amt;
	} else {
		pile_excise(&s->stock, obj);
		object_delete(&obj);
		pile_excise(&s->stock_k, known_obj);
		object_delete(&known_obj);
		assert(s->stock_num);
		s->stock_num--;
	}
}


/**
 * Find a given object kind in the store.
 */
static struct object *store_find_kind(struct store *s, struct object_kind *k) {
	struct object *obj;

	assert(s);
	assert(k);

	/* Check if it's already in stock */
	for (obj = s->stock; obj; obj = obj->next) {
		if (obj->kind == k && !obj->ego)
			return obj;
	}

	return NULL;
}


/**
 * Delete an object from store 'store', or, if it is a stack, perhaps only
 * partially delete it.
 *
 * This function is used when store maintainance occurs, and is designed to
 * imitate non-PC purchasers making purchases from the store.
 *
 * The reason this doesn't check for "staple" items and refuse to
 * delete them is that a store could conceviably have two stacks of a
 * single staple item, in which case, you could have a store which had
 * more stacks than staple items, but all stacks are staple items.
 */
static void store_delete_random(struct store *store)
{
	int what;
	int num;
	struct object *obj;

	assert(store->stock_num > 0);

	/* Pick a random slot */
	what = randint0(store->stock_num);

	/* Walk through list until we find our item */
	obj = store->stock;
	while (what--) {
		assert(obj);
		obj = obj->next;
	}

	/* Determine how many objects are in the slot */
	num = obj->number;

	/* Deal with stacks */
	if (num > 1) {
		/* Special behaviour for arrows, bolts &tc. */
		if (tval_is_ammo(obj)) {
			/* 50% of the time, destroy the entire stack */
			if (randint0(100) < 50 || num < 10)
				num = obj->number;

			/* 50% of the time, reduce the size to a multiple of 5 */
			else
				num = randint1(num / 5) * 5 + (num % 5);
		} else {
			/* 50% of the time, destroy a single object */
			if (randint0(100) < 50) num = 1;

			/* 25% of the time, destroy half the objects */
			else if (randint0(100) < 50) num = (num + 1) / 2;

			/* 25% of the time, destroy all objects */
			else num = obj->number;

			/* Hack -- decrement the total charges of devices and wands. */
			if (tval_can_have_charges(obj))
				obj->pval -= num * obj->pval / obj->number;
		}
	}

	assert (num <= obj->number);

	if (obj->artifact) {
		history_lose_artifact(player, obj->artifact);
	}

	/* If you own the store, ka-ching.
	 * This at first sight is an infinite money source - but in fact it is limited by
	 * the limited amount of time you have with the town available.
	 **/
	if (you_own(store)) {
		store->income += 1 + (price_item(store, obj, false, num) / 4);
	}

	/* Delete the item, wholly or in part */
	store_delete(store, obj, num);
}


/**
 * The HQ only stocks some items.
 * - Melee weapons, ranged weapons, ammo.
 * - Offensive, healing devices (TODO)
 * - Thrown weapons (TODO)
 * - Most armor (avoid items that aren't really armor)
 * - Most lights, food rations
 * - Some pills, cards? (TODO)
 */
static bool hq_ok(const struct object *obj)
{
	if (tval_is_weapon(obj))	/* includes ammo */
		return true;
	if (tval_is_armor(obj)) {
		if ((randcalc(obj->kind->to_h, 0, AVERAGE) < 0) && (obj->kind->ac <= 1))
			return false;	/* high heels */
		if ((obj->kind->ac <= 0) && (randcalc(obj->kind->to_a, 0, AVERAGE) < 5))
			return false;	/* sun hat, but not belt */
		return true;
	}
	switch(obj->tval) {
		case TV_LIGHT:
			if (!my_stristr(obj->kind->name, "firecracker"))
				return true;
			return false;
		case TV_BATTERY:
			return true;
		case TV_FOOD:
			if (my_stristr(obj->kind->name, "dried"))
				return true;
			if (my_stristr(obj->kind->name, "ation"))
				return true;
			if (my_stristr(obj->kind->name, "cheese"))
				return true;
			return false;
	}
	return false;
}

/**
 * This makes sure that the black market doesn't stock any object that other
 * stores have, unless it is an ego-item or has various bonuses.
 *
 * Based on a suggestion by Lee Vogt <lvogt@cig.mcel.mot.com>.
 */
static bool black_market_ok(const struct object *obj)
{
	int i;

	/* Ego items are always fine */
	if (obj->ego) return true;

	/* Good items are normally fine */
	if (obj->to_a > 2) return true;
	if (obj->to_h > 1) return true;
	if (obj->to_d > 2) return true;

	/* No cheap items */
	if (object_value_real(obj, 1) < 10) return (false);

	/* Check the other stores */
	for (i = 0; i < MAX_STORES; i++) {
		struct object *stock_obj;

		/* Skip home and black market */
		if (i == STORE_B_MARKET || i == STORE_HOME)
			continue;

		/* Check every object in the store */
		for (stock_obj = stores[i].stock; stock_obj; stock_obj = stock_obj->next) {
			/* Compare object kinds */
			if (obj->kind == stock_obj->kind)
				return false;
		}
	}

	/* Otherwise fine */
	return true;
}



/**
 * Get a choice from the store allocation table, in tables.c
 */
static struct object_kind *store_get_choice(struct store *store, int level)
{
	/* Choose a random entry from the store's table */
	do {
		struct store_entry *item = store->normal_table + randint0(store->normal_num);

		if (!item)
			return NULL;

		/* Produce a randomized value - parts in 10000 - based on the maximum dungeon level */
		int rarity = randcalc(item->rarity, player->max_depth, RANDOMISE);
		if (rarity != 10000) {
			if (randint0(10000) >= rarity)
				continue;
		}

		/* Specified directly, return it */
		if (item->kind)
			return item->kind;

		/* Only a tval is given, generate a random item from the tval */
		return get_obj_num(level, false, item->tval);

	} while (true);
	return NULL;
}

static void store_level_limits(struct store *store, int *min_level, int *max_level)
{
	/* Decide min/max levels */
	if (store->sidx == STORE_HQ) {
		*min_level = 1;
		*max_level = 5 + (title_idx(player->lev) * 6);
	} else {
		if (store->sidx == STORE_B_MARKET) {
			if (player->bm_faction <= 0) {
				*min_level = MIN(40, player->max_depth / 2);
				*max_level = MIN(55, (player->max_depth / 2) + 15);
			} else {
				*min_level = MIN(75, player->max_depth + (player->bm_faction * 5));
				*max_level = MIN(90, player->max_depth + 10 + (player->bm_faction * 10));
			}
		} else {
			*min_level = 1;
			*max_level = z_info->store_magic_level + MAX(player->max_depth - 20, 0);
			if (*min_level > 55) *min_level = 55;
			if (*max_level > 70) *max_level = 70;
		}
	}
}

/**
 * Creates a random object and gives it to store 'store'
 */
static bool store_create_random(struct store *store, int min_level, int max_level, bool good, bool great)
{
	struct object_kind *kind;
	struct object *obj, *known_obj;

	/* Work out the level for objects to be generated at */
	int level = rand_range(min_level, max_level);

	/* Black Markets have a random object, of a given level */
	if (store->sidx == STORE_HQ)
		kind = get_obj_num(level, false, 0);
	else if (store->sidx == STORE_B_MARKET)
		kind = get_obj_num(level, false, 0);
	else
		kind = store_get_choice(store, level);

	if (!kind)
		return false;

	/*** Pre-generation filters ***/

	/* No chests in stores XXX */
	if (kind->tval == TV_CHEST) return false;

	/*** Generate the item ***/

	/* Create a new object of the chosen kind */
	obj = object_new();
	object_prep(obj, kind, level, RANDOMISE);

	/* Apply some "low-level" magic (no artifacts) */
	apply_magic(obj, level, false, good, great, false);

	/* Reject if item is 'damaged' (faults).
	 * The BM doesn't care about ripping you off, though.
	 * And don't reject negative combat bonuses for their own sake, as there are some nice items
	 * with -ve to hit, and anything actually worthless will be caught later by the value check.
	 **/
	if ((store->sidx != STORE_B_MARKET) && (obj->faults)) {
		object_delete(&obj);
		return false;
	}

	/*** Post-generation filters ***/

	/* Make a known object */
	known_obj = object_new();
	obj->known = known_obj;

	/* Know everything the player knows, no origin yet */
	obj->known->notice |= OBJ_NOTICE_ASSESSED;
	object_set_base_known(obj);
	obj->known->notice |= OBJ_NOTICE_ASSESSED;
	player_know_object(player, obj);
	obj->origin = ORIGIN_NONE;

	/* Black markets have expensive tastes */
	if ((store->sidx == STORE_B_MARKET) && !black_market_ok(obj)) {
		object_delete(&known_obj);
		obj->known = NULL;
		object_delete(&obj);
		return false;
	}

	/* HQ only stocks some items */
	if ((store->sidx == STORE_HQ) && !hq_ok(obj)) {
		object_delete(&known_obj);
		obj->known = NULL;
		object_delete(&obj);
		return false;
	}

	/* No "worthless" items */
	if (object_value_real(obj, 1) < 1)  {
		object_delete(&known_obj);
		obj->known = NULL;
		object_delete(&obj);
		return false;
	}

	/* Mass produce and/or apply discount */
	mass_produce(obj);

	/* Attempt to carry the object */
	if (!store_carry(store, obj, true)) {
		object_delete(&known_obj);
		obj->known = NULL;
		object_delete(&obj);
		return false;
	}

	/* Definitely done */
	return true;
}


/**
 * Helper function: create an item with the given kind/tval, add it to the
 * store st.  Return the item in the inventory.
 */
static struct object *store_create_item(int level, struct store *store, struct object_kind *kind, int tval)
{
	struct object *obj = object_new();
	struct object *known_obj = object_new();

	if (!kind)
		kind = get_obj_num(level, false, tval);

	/* Create a new object of the chosen kind */
	object_prep(obj, kind, 0, RANDOMISE);

	/* Know everything the player knows, no origin yet */
	obj->known = known_obj;
	obj->known->notice |= OBJ_NOTICE_ASSESSED;
	object_set_base_known(obj);
	obj->known->notice |= OBJ_NOTICE_ASSESSED;
	player_know_object(player, obj);
	obj->origin = ORIGIN_NONE;

	/* Attempt to carry the object */
	return store_carry(store, obj, true);
}

/**
 * Maintain the inventory at the stores.
 */
void do_store_maint(struct store *s, bool init)
{
	/* Ignore home */
	if (s->sidx == STORE_HOME)
		return;

	if (!init) {
		/* Destroy crappy black market items */
		if (s->sidx == STORE_B_MARKET) {
			struct object *obj = s->stock;
			while (obj) {
				struct object *next = obj->next;
				if (!black_market_ok(obj))
					store_delete(s, obj, obj->number);
				obj = next;
			}
		}

		/* We want to make sure stores have staple items. If there's
		 * turnover, we also want to delete a few items, and add a few
		 * items.
		 *
		 * If we create staple items, then delete items, then create new
		 * items, we are stuck with one of three choices:
		 * 1. We can risk deleting staple items, and not having any left.
		 * 2. We can refuse to delete staple items, and risk having that
		 * become an infinite loop.
		 * 3. We can do a ton of extra bookkeeping to make sure we delete
		 * staple items only if there's duplicates of them.
		 *
		 * What if we change the order? First sell a handful of random items,
		 * then create any missing staples, then create new items. This
		 * has two tests for s->turnover, but simplifies everything else
		 * dramatically.
		 */

		if (s->turnover) {
			int restock_attempts = 100000;
			int stock = s->stock_num - randint1(s->turnover);

			/* We'll end up adding staples for sure, maybe plus other
			 * items. It's fine if we sell out completely, though, if
			 * turnover is high. The cap doesn't include always_num,
			 * because otherwise the addition of missing staples could
			 * put us over (if the store was full of player-sold loot).
			 */
			int min = 0;
			int max = s->normal_stock_max;

			/* More faction gives more items */
			if ((s->sidx == STORE_B_MARKET) && (player->bm_faction > 0)) {
				max *= (player->bm_faction + 2);
				max /= 2;
			}

			if (stock < min) stock = min;
			if (stock > max) stock = max;

			/* Destroy random objects until only "stock" slots are left */
			while (s->stock_num > stock && --restock_attempts)
				store_delete_random(s);

			if (!restock_attempts)
				quit_fmt("Unable to (de-)stock store %d. Please report this bug",
						 s->sidx + 1);
		} else {
			if (s->always_num && s->stock_num) {
				int sales = randint1(s->stock_num);
				while (sales--) {
					store_delete_random(s);
				}
			}
		}
	}

	/* Ensure staples are created */
	if (s->always_num) {
		size_t i;
		for (i = 0; i < s->always_num; i++) {
			struct store_entry *item = s->always_table + i;
			struct object_kind *kind = item->kind;
			int tval = item->tval;
			struct object *obj = store_find_kind(s, kind);
			int min_level, max_level;
			store_level_limits(s, &min_level, &max_level);

			/* Create the item if it doesn't exist */
			if (!obj) {
				/* Produce a randomized value - parts in 10000 - based on the maximum dungeon level */
				int rarity = randcalc(item->rarity, player->max_depth, RANDOMISE);
				if (rarity != 10000) {
					if (randint0(10000) >= rarity)
						continue;
				}
				int level = rand_range(min_level, max_level);
				obj = store_create_item(level, s, kind, tval);
				mass_produce(obj);
			}
		}
	}

	if (s->turnover) {

		/* Now that the staples exist, we want to add more
		 * items, at least enough to get us to normal_stock_min
		 * items that aren't necessarily staples.
		 */

		int min = s->normal_stock_min + s->always_num;
		int max = s->normal_stock_max + s->always_num;

		/* For the rest, we just choose items randomlyish */
		/* The restock_attempts will probably only go to zero (otherwise
		 * infinite loop) if stores don't have enough items they can stock,
		 * so instead decrement the number of items demanded.
		 * Only give up if no items can be generated.
		 **/
		int stock = s->stock_num;
		if (init)
			stock += s->turnover;
		else
			stock += randint1(s->turnover);

		/* Keep stock between specified min and max slots */
		if (stock > max) stock = max;
		if (stock < min) stock = min;


		/* Try to generate enough items to fill stock.
		 * If this is taking too long, start to increase the level
		 * and produce more unusual items (as a good item won't merge
		 * with the existing +0,+0 item).
		 * If it's taking far too long (1000 tries) then reduce the
		 * number of items required.
		 */
		while (stock && (s->stock_num < stock))
		{
			int min_level, max_level;
			store_level_limits(s, &min_level, &max_level);

			int restock_attempts = 1000;
			bool good = false;
			bool great = false;

			while (s->stock_num < stock && --restock_attempts) {
				store_create_random(s, min_level, max_level, good, great);
				if (restock_attempts < (1000 - stock))
					good = one_in_(2);
				if (restock_attempts < (1000 - (stock * 10)))
					great = one_in_(2);
				if (restock_attempts < (1000 - (stock * 2))) {
					if ((restock_attempts % stock) == 0) {
						if (max_level < 100)
							max_level++;
					}
				}
			}
			stock--;
		};

		if (s->stock_num < stock)
			quit_fmt("Unable to (re-)stock store %d. Please report this bug",
					 s->sidx + 1);
	}
}

/**
 * Maintain the inventory at the stores.
 */
void store_maint(struct store *s)
{
	do_store_maint(s, false);
}

/**
 * Update the stores on the return to town.
 */
void store_update(void)
{
	if (OPT(player, cheat_xtra)) msg("Updating Shops...");
	while (daycount--) {
		int n;

		for (int t=0; t<z_info->town_max; t++) {
			struct store *stores = t_info[t].stores;

			/* Maintain each shop (except home) */
			for (n = 0; n < MAX_STORES; n++) {
				/* Skip the home */
				if (n == STORE_HOME) continue;

				if (stores[n].bandays > 0) {
					stores[n].bandays--;
					if (stores[n].bandays == 0) {
						free((void *)stores[n].banreason);
						stores[n].banreason = NULL;
					}
				}

				/* Maintain */
				store_maint(&stores[n]);
			}

			/* Sometimes, shuffle the shop-keepers */
			if (one_in_(z_info->store_shuffle)) {
				/* Message */
				if (OPT(player, cheat_xtra)) msg("Shuffling a Shopkeeper...");

				/* Pick a random shop (except home) */
				while (1) {
					n = randint0(MAX_STORES);
					if (n != STORE_HOME) break;
				}

				/* Shuffle it */
				store_shuffle(&stores[n]);
			}
		}
	}
	daycount = 0;
	if (OPT(player, cheat_xtra)) msg("Done.");
}

/** Owner stuff **/

struct owner *store_ownerbyidx(struct store *s, unsigned int idx) {
	struct owner *o;
	for (o = s->owners; o; o = o->next) {
		if (o->oidx == idx)
			return o;
	}

	quit_fmt("Bad call to store_ownerbyidx: idx is %d\n", idx);
	return 0; /* Needed to avoid Windows compiler warning */
}

/* Return the store (in any town) whose current owner is o, or NULL if none are */
static struct store *store_by_owner(struct owner *o) {
	for(int t=0; t<z_info->town_max; t++) {
		for(int s=0; s<MAX_STORES; s++) {
			if (t_info[t].stores[s].owner == o) {
				return &t_info[t].stores[s];
			}
		}
	}
	return NULL;
}

/* Choose an owner that is not used by any shop, in any town */
static struct owner *store_choose_owner(struct store *s) {
	struct owner *o;

	do {
		unsigned int n = 0;
		for (o = s->owners; o; o = o->next) {
			n++;
		}

		n = randint1(n-1);
		o = store_ownerbyidx(s, n);
	} while (store_by_owner(o));
	return o;
}

/**
 * Shuffle one of the stores.
 */
void store_shuffle(struct store *store)
{
	struct owner *o = store->owner;

	while (o == store->owner)
	    o = store_choose_owner(store);

	store->owner = o;
	store->bandays = 0;
}




/**
 * ------------------------------------------------------------------------
 * Higher-level code
 * ------------------------------------------------------------------------ */

/**
 * Return the quantity of a given item in the pack (include quiver).
 */
int find_inven(const struct object *obj)
{
	int i;
	struct object *gear_obj;
	int num = 0;

	/* Similar slot? */
	for (gear_obj = player->gear; gear_obj; gear_obj = gear_obj->next) {
		/* Check only the inventory and the quiver */
		if (object_is_equipped(player->body, gear_obj))
			continue;

		/* Require identical object types */
		if (obj->kind != gear_obj->kind)
			continue;

		/* Analyze the items */
		switch (obj->tval)
		{
			/* Chests */
			case TV_CHEST:
			{
				/* Never okay */
				return 0;
			}

			/* Food and Pills and Cards */
			case TV_FOOD:
			case TV_MUSHROOM:
			case TV_PILL:
			case TV_CARD:
			{
				/* Assume okay */
				break;
			}

			/* Devices and Wands */
			case TV_DEVICE:
			case TV_WAND:
			{
				/* Assume okay */
				break;
			}

			/* Rods */
			case TV_GADGET:
			{
				/* Assume okay */
				break;
			}

			/* Wearables */
			case TV_GUN:
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_BELT:
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			case TV_LIGHT:
			case TV_AMMO_12:
			case TV_AMMO_9:
			case TV_AMMO_6:
			{
				/* Require identical "bonuses" */
				if (obj->to_h != gear_obj->to_h)
					continue;
				if (obj->to_d != gear_obj->to_d)
					continue;
				if (obj->to_a != gear_obj->to_a)
					continue;

				/* Require identical modifiers */
				for (i = 0; i < OBJ_MOD_MAX; i++)
					if (obj->modifiers[i] != gear_obj->modifiers[i])
						continue;

				/* Require identical "artifact" names */
				if (obj->artifact != gear_obj->artifact)
					continue;

				/* Require identical "ego-item" names */
				if (obj->ego != gear_obj->ego)
					continue;

				/* Lights must have same amount of fuel */
				else if (obj->timeout != gear_obj->timeout &&
						 tval_is_light(obj))
					continue;

				/* Require identical "values" */
				if (obj->ac != gear_obj->ac)
					continue;
				if (obj->dd != gear_obj->dd)
					continue;
				if (obj->ds != gear_obj->ds)
					continue;

				/* Probably okay */
				break;
			}

			/* Various */
			default:
			{
				/* Probably okay */
				break;
			}
		}


		/* Different flags */
		if (!of_is_equal(obj->flags, gear_obj->flags))
			continue;

		/* They match, so add up */
		num += gear_obj->number;
	}

	return num;
}


/**
 * Buy the item with the given index from the current store's inventory.
 */
void do_cmd_buy(struct command *cmd)
{
	int amt;

	struct object *obj, *bought, *known_obj;

	char o_name[80];
	int price;

	struct store *store = store_at(cave, player->grid);

	if (!store) {
		msg("You cannot purchase items when not in a store.");
		return;
	}

	/* Get arguments */
	/* XXX-AS fill this out, split into cmd-store.c */
	if (cmd_get_arg_item(cmd, "item", &obj) != CMD_OK)
		return;

	if (!pile_contains(store->stock, obj)) {
		msg("You cannot buy that item because it's not in the store.");
		return;
	}

	if (cmd_get_arg_number(cmd, "quantity", &amt) != CMD_OK)
		return;

	/* Get desired object */
	bought = object_new();
	object_copy_amt(bought, obj, amt);

	/* Ensure we have room */
	if (bought->number > inven_carry_num(bought, false)) {
		msg("You cannot carry that many items.");
		object_delete(&bought);
		return;
	}

	/* Describe the object (fully) */
	object_desc(o_name, sizeof(o_name), bought, ODESC_PREFIX | ODESC_FULL);

	/* Extract the price for the entire stack */
	price = price_item(store, bought, false, bought->number);

	if (price > player->au) {
		msg("You cannot afford that purchase.");
		object_delete(&bought);
		return;
	}

	/* Spend the money */
	player->au -= price;

	/* Update the gear */
	player->upkeep->update |= (PU_INVEN);

	/* Combine the pack (later) */
	player->upkeep->notice |= (PN_COMBINE | PN_IGNORE);

	/* Describe the object (fully) again for the message */
	object_desc(o_name, sizeof(o_name), bought, ODESC_PREFIX | ODESC_FULL);

	/* Message */
	bool cyber = (store->sidx == STORE_CYBER);

	/* Cyberware is usually installed when bought.
	 * Exceptions are made for buying a stack of >1, and for buying one that
	 * would merge with an item in your pack and so become a stack of >1 by
	 * the time it reaches do_inven_wield().
	 */
	bool install;
	if (amt != 1)
		install = false;
	else
		install = ((inven_carry_num(bought, true) <= 0) && (wield_slot(bought) != player->body.count));

	if (cyber && install) {
		msg("You have %s installed for $%d.", o_name, price);
	} else {
		if (one_in_(3)) msgt(MSG_STORE5, "%s", ONE_OF(comment_accept));
		msg("You bought %s for $%d.", o_name, price);
	}

	/* Erase the inscription */
	bought->note = 0;

	/* Give it an origin if it doesn't have one */
	if (bought->origin == ORIGIN_NONE)
		bought->origin = ORIGIN_STORE;

	/* Hack - Reduce the number of charges in the original stack */
	if (tval_can_have_charges(obj))
		obj->pval -= bought->pval;

	/* Make a known object */
	known_obj = object_new();
	object_copy(known_obj, obj->known);
	bought->known = known_obj;

	/* Learn flavor, any effect and all the icons */
	object_flavor_aware(bought);
	bought->known->effect = bought->effect;
	while (!object_fully_known(bought)) {
		object_learn_unknown_icon(player, bought);
		player_know_object(player, bought);
	}

	/* Give it to the player */
	inven_carry(player, bought, true, !cyber);

	/* Handle stuff */
	handle_stuff(player);

	/* Remove the bought objects from the store if it's not a staple */
	if (!store_is_staple(store, obj->kind)) {
		/* Reduce or remove the item */
		store_delete(store, obj, amt);

		/* Store is empty */
		if (store->stock_num == 0) {
			int i;

			/* Sometimes shuffle the shopkeeper */
			if (one_in_(z_info->store_shuffle)) {
				/* Shuffle */
				msg("The shopkeeper retires.");
				store_shuffle(store);
			} else
				/* Maintain */
				msg("The shopkeeper brings out some new stock.");

			/* New inventory */
			for (i = 0; i < 10; ++i)
				store_maint(store);
		}
	}

	event_signal(EVENT_STORECHANGED);
	event_signal(EVENT_INVENTORY);
	event_signal(EVENT_EQUIPMENT);

	if (cyber && install) {
		do_inven_wield(bought, wield_slot(bought), false, true);
	}
}

/**
 * Retrieve the item with the given index from the home's inventory.
 */
void do_cmd_retrieve(struct command *cmd)
{
	int amt;

	struct object *obj, *known_obj, *picked_item;

	struct store *store = store_at(cave, player->grid);
	if (!store) return;

	if (store->sidx != STORE_HOME) {
		msg("You are not currently at home.");
		return;
	}

	/* Get arguments */
	if (cmd_get_arg_item(cmd, "item", &obj) != CMD_OK)
		return;

	if (!pile_contains(store->stock, obj)) {
		msg("You cannot retrieve that item because it's not in the home.");
		return;
	}

	if (cmd_get_arg_number(cmd, "quantity", &amt) != CMD_OK)
		return;

	/* Get desired object */
	picked_item = object_new();
	object_copy_amt(picked_item, obj, amt);

	/* Ensure we have room */
	if (picked_item->number > inven_carry_num(picked_item, false)) {
		msg("You cannot carry that many items.");
		object_delete(&picked_item);
		return;
	}

	/* Distribute charges of wands, devices, or rods */
	distribute_charges(obj, picked_item, amt);

	/* Make a known object */
	known_obj = object_new();
	object_copy(known_obj, obj->known);
	picked_item->known = known_obj;

	/* Give it to the player */
	inven_carry(player, picked_item, true, true);

	/* Handle stuff */
	handle_stuff(player);
	
	/* Reduce or remove the item */
	store_delete(store, obj, amt);

	event_signal(EVENT_STORECHANGED);
	event_signal(EVENT_INVENTORY);
	event_signal(EVENT_EQUIPMENT);
}


/**
 * Determine if the current store will purchase the given object
 */
bool store_will_buy_tester(const struct object *obj)
{
	struct store *store = store_at(cave, player->grid);
	if (!store) return false;

	return store_will_buy(store, obj);
}

/**
 * Install (or uninstall) an item (equip/unequip it)
 */
void do_cmd_install(struct command *cmd)
{
	int amt;
	struct object *obj;

	/* Get arguments */
	/* XXX-AS fill this out, split into cmd-store.c */
	if (cmd_get_arg_item(cmd, "item", &obj) != CMD_OK)
		return;

	if (cmd_get_quantity(cmd, "quantity", &amt, obj->number) != CMD_OK)
		return;

	if (amt != 1)
		return;

	if (wield_slot(obj) == player->body.count)
		return;

	if (object_is_equipped(player->body, obj)) {
		/* Unequip */
		do_inven_takeoff(obj, true, true);
	} else {
		/* Equip */
		do_inven_wield(obj, wield_slot(obj), true, true);
	}
}

/**
 * Sell an item to the current store.
 */
void do_cmd_sell(struct command *cmd)
{
	int amt;
	struct object dummy_item;
	struct store *store = store_at(cave, player->grid);
	int price, dummy, value;
	char o_name[120];
	char label;

	struct object *obj, *sold_item;
	bool none_left = false;

	/* Get arguments */
	/* XXX-AS fill this out, split into cmd-store.c */
	if (cmd_get_arg_item(cmd, "item", &obj) != CMD_OK)
		return;

	if (cmd_get_quantity(cmd, "quantity", &amt, obj->number) != CMD_OK)
		return;

	bool cyber = (store->sidx == STORE_CYBER);

	/* Cannot remove stickied objects */
	if (object_is_equipped(player->body, obj)) {
		bool stuck = (cyber ? (!obj_cyber_can_takeoff(obj)) : (!obj_can_takeoff(obj)));
		if (stuck) {
			msg("Hmmm, it seems to be stuck.");
			return;
		}
	}

	/* Check we are somewhere we can sell the items. */
	if (!store) {
		msg("You cannot sell items when not in a store.");
		return;
	}

	/* Check the store wants the items being sold */
	if (!store_will_buy(store, obj)) {
		msg("I do not wish to purchase this item.");
		return;
	}

	/* Get a copy of the object representing the number being sold */
	object_copy_amt(&dummy_item, obj, amt);

	/* Check if the store has space for the items */
	if (!store_check_num(store, &dummy_item)) {
		object_wipe(&dummy_item);
		msg("I don't have the room in my store to keep it.");
		return;
	}

	/* Get the label */
	label = gear_to_label(obj);

	price = price_item(store, &dummy_item, true, amt);

	/* Get some money */
	player->au += price;

	/* Update the auto-history if selling an artifact that was previously
	 * un-IDed. (Ouch!) */
	if (obj->artifact)
		history_find_artifact(player, obj->artifact);

	/* Update the gear */
	player->upkeep->update |= (PU_INVEN);

	/* Combine the pack (later) */
	player->upkeep->notice |= (PN_COMBINE);

	/* Redraw stuff */
	player->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

	/* Get the "apparent" value */
	dummy = object_value(&dummy_item, amt);
	/*
	 * Do not need the dummy any more so release the memory allocated
	 * within it.
	 */
	object_wipe(&dummy_item);

	/* Know flavor and unknown icons */
	object_know_all(obj);

	/* Take a proper copy of the now known-about object. */
	sold_item = gear_object_for_use(obj, amt, false, &none_left);

	/* Get the "actual" value */
	value = object_value_real(sold_item, amt);

	/* Get the description all over again */
	object_desc(o_name, sizeof(o_name), sold_item, ODESC_PREFIX | ODESC_FULL);

	/* Describe the result (in message buffer) */
	if (OPT(player, birth_no_selling)) {
		msg("You had %s (%c).", o_name, label);
	} else {
		msg("You sold %s (%c) for $%d.", o_name, label, price);

		/* Analyze the prices (and comment verbally) */
		purchase_analyze(price, value, dummy);
	}

	/* Autoinscribe if we still have any */
	if (!none_left)
		apply_autoinscription(obj);

	/* Set ignore flag */
	player->upkeep->notice |= PN_IGNORE;

	/* Notice if pack items need to be combined or reordered */
	notice_stuff(player);

	/* Handle stuff */
	handle_stuff(player);

	/* The store gets that (known) object */
	if (! store_carry(store, sold_item, false)) {
		/* The store rejected it; delete. */
		if (sold_item->known) {
			object_delete(&sold_item->known);
			sold_item->known = NULL;
		}
		object_delete(&sold_item);
	}

	event_signal(EVENT_STORECHANGED);
	event_signal(EVENT_INVENTORY);
	event_signal(EVENT_EQUIPMENT);
}

/**
 * Stash an item in the home.
 */
void do_cmd_stash(struct command *cmd)
{
	int amt;
	struct object dummy;
	struct store *store = store_at(cave, player->grid);
	char o_name[120];
	char label;

	struct object *obj, *dropped;
	bool none_left = false;
	bool no_room;

	if (cmd_get_arg_item(cmd, "item", &obj))
		return;

	if (cmd_get_quantity(cmd, "quantity", &amt, obj->number) != CMD_OK)
		return;

	/* Check we are somewhere we can stash items. */
	if (store->sidx != STORE_HOME) {
		msg("You are not in your home.");
		return;
	}

	/* Cannot remove stickied objects */
	if (object_is_equipped(player->body, obj) && !obj_can_takeoff(obj)) {
		msg("Hmmm, it seems to be stuck.");
		return;
	}	

	/* Get a copy of the object representing the number being sold */
	object_copy_amt(&dummy, obj, amt);

	no_room = !store_check_num(store, &dummy);
	/*
	 * Do not need the dummy any more so release the memory allocated
	 * within it.
	 */
	object_wipe(&dummy);
	if (no_room) {
		msg("Your home is full.");
		return;
	}

	/* Get where the object is now */
	label = gear_to_label(obj);

	/* Now get the real item */
	dropped = gear_object_for_use(obj, amt, false, &none_left);

	/* Describe */
	object_desc(o_name, sizeof(o_name), dropped, ODESC_PREFIX | ODESC_FULL);

	/* Message */
	msg("You drop %s (%c).", o_name, label);

	/* Handle stuff */
	handle_stuff(player);

	/* Let the home carry it */
	home_carry(dropped);

	event_signal(EVENT_STORECHANGED);
	event_signal(EVENT_INVENTORY);
	event_signal(EVENT_EQUIPMENT);
}
