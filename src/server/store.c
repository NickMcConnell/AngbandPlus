/*
 * File: store.c
 * Purpose: Store stocking
 *
 * Copyright (c) 1997 Robert A. Koeneke, James E. Wilson, Ben Harrison
 * Copyright (c) 2007 Andi Sidwell
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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


#include "s-angband.h"
#include <math.h>


static void store_maint(struct store *s, bool force);


/*
 * Constants and definitions
 */


/*
 * Array[MAX_STORES] of stores
 */
struct store *stores;


/*
 * The hints array
 */
struct hint *hints;


/* Black market */
#define store_black_market(st) \
    (((st) == STORE_B_MARKET) || ((st) == STORE_XBM))


/* Store orders */
char store_orders[STORE_ORDERS][NORMAL_WID];


/*
 * Return the store instance at the given location
 */
struct store *store_at(struct player *p)
{
    if (p->store_num != -1)
        return &stores[p->store_num];

    return NULL;
}


/*
 * Create a new store.
 */
static struct store *store_new(int idx)
{
    struct store *s = mem_zalloc(sizeof(*s));

    s->sidx = idx;
    s->stock_size = z_info->store_inven_max;

    return s;
}


/*
 * Get rid of stores at cleanup. Gets rid of everything.
 */
static void cleanup_stores(void)
{
    struct owner *o, *o_next;
    struct object_buy *buy, *buy_next;
    int i;

    if (!stores) return;

    /* Free the store inventories */
    for (i = 0; i < MAX_STORES; i++)
    {
        /* Get the store */
        struct store *store = &stores[i];

        string_free(store->name);

        /* Free the store inventory */
        object_pile_free(store->stock);
        mem_free(store->always_table);
        mem_free(store->normal_table);

        for (o = store->owners; o; o = o_next)
        {
            o_next = o->next;
            string_free(o->name);
            mem_free(o);
        }

        for (buy = store->buy; buy; buy = buy_next)
        {
            buy_next = buy->next;
            mem_free(buy);
        }
    }

    mem_free(stores);
}


/*
 * Edit file parsing
 */


/** store.txt **/


static enum parser_error parse_store(struct parser *p)
{
    struct store *h = parser_priv(p);
    struct store *s;
    unsigned int idx = parser_getuint(p, "index") - 1;

    if (idx >= MAX_STORES) return PARSE_ERROR_OUT_OF_BOUNDS;

    s = store_new(parser_getuint(p, "index") - 1);
    s->name = string_make(parser_getstr(p, "name"));
    s->next = h;
    parser_setpriv(p, s);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_slots(struct parser *p)
{
    struct store *s = parser_priv(p);

    s->normal_stock_min = parser_getuint(p, "min");
    s->normal_stock_max = parser_getuint(p, "max");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_turnover(struct parser *p)
{
    struct store *s = parser_priv(p);

    s->turnover = parser_getuint(p, "turnover");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_normal(struct parser *p)
{
    struct store *s = parser_priv(p);
    int tval = tval_find_idx(parser_getsym(p, "tval"));
    int sval = lookup_sval(tval, parser_getsym(p, "sval"));
    struct object_kind *kind = lookup_kind(tval, sval);

    if (!kind) return PARSE_ERROR_UNRECOGNISED_SVAL;

    /* Expand if necessary */
    if (!s->normal_num)
    {
        s->normal_size = 16;
        s->normal_table = mem_zalloc(s->normal_size * sizeof(*s->normal_table));
    }
    else if (s->normal_num >= s->normal_size)
    {
        s->normal_size += 8;
        s->normal_table = mem_realloc(s->normal_table, s->normal_size * sizeof(*s->normal_table));
    }

    s->normal_table[s->normal_num++] = kind;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_always(struct parser *p)
{
    struct store *s = parser_priv(p);
    int tval = tval_find_idx(parser_getsym(p, "tval"));
    int sval = lookup_sval(tval, parser_getsym(p, "sval"));
    struct object_kind *kind = lookup_kind(tval, sval);

    if (!kind) return PARSE_ERROR_UNRECOGNISED_SVAL;

    /* Expand if necessary */
    if (!s->always_num)
    {
        s->always_size = 8;
        s->always_table = mem_zalloc(s->always_size * sizeof(*s->always_table));
    }
    else if (s->always_num >= s->always_size)
    {
        s->always_size += 8;
        s->always_table = mem_realloc(s->always_table, s->always_size * sizeof(*s->always_table));
    }

    s->always_table[s->always_num++] = kind;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_owner(struct parser *p)
{
    struct store *s = parser_priv(p);
    unsigned int maxcost = parser_getuint(p, "purse");
    char *name = string_make(parser_getstr(p, "name"));
    struct owner *o;

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    o = mem_zalloc(sizeof(*o));
    o->oidx = (s->owners? s->owners->oidx + 1: 0);
    o->next = s->owners;
    o->name = name;
    o->max_cost = maxcost;
    s->owners = o;

    /* Extended store purse */
    if (cfg_double_purse) o->max_cost *= 2;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_buy(struct parser *p)
{
    struct store *s = parser_priv(p);
    struct object_buy *buy;

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    buy = mem_zalloc(sizeof(*buy));
    buy->tval = tval_find_idx(parser_getstr(p, "base"));
    buy->next = s->buy;
    s->buy = buy;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_buy_flag(struct parser *p)
{
    struct store *s = parser_priv(p);
    int flag;
    struct object_buy *buy;

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    flag = lookup_flag(list_obj_flag_names, parser_getsym(p, "flag"));

    if (flag == FLAG_END) return PARSE_ERROR_INVALID_FLAG;

    buy = mem_zalloc(sizeof(*buy));
    buy->flag = flag;
    buy->tval = tval_find_idx(parser_getstr(p, "base"));
    buy->next = s->buy;
    s->buy = buy;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_stores(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "store uint index str name", parse_store);
    parser_reg(p, "owner uint purse str name", parse_owner);
    parser_reg(p, "slots uint min uint max", parse_slots);
    parser_reg(p, "turnover uint turnover", parse_turnover);
    parser_reg(p, "normal sym tval sym sval", parse_normal);
    parser_reg(p, "always sym tval sym sval", parse_always);
    parser_reg(p, "buy str base", parse_buy);
    parser_reg(p, "buy-flag sym flag str base", parse_buy_flag);

    return p;
}


static errr run_parse_stores(struct parser *p)
{
    return parse_file_quit_not_found(p, "store");
}


static errr finish_parse_stores(struct parser *p)
{
    stores = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static struct file_parser store_parser =
{
    "store",
    init_parse_stores,
    run_parse_stores,
    finish_parse_stores,
    NULL
};


/*
 * Other init stuff
 */


static struct store *flatten_stores(struct store *store_list)
{
    struct store *s;
    struct store *stores_local = mem_zalloc(MAX_STORES * sizeof(*stores_local));

    for (s = store_list; s; s = s->next)
    {
        if (s->sidx >= MAX_STORES) continue;
        memcpy(&stores_local[s->sidx], s, sizeof(*s));
        stores_local[s->sidx].next = NULL;
    }

    while (store_list)
    {
        s = store_list->next;
        mem_free(store_list);
        store_list = s;
    }

    return stores_local;
}


static void store_init(void)
{
    if (run_parser(&store_parser)) quit("Cannot initialize stores");
    stores = flatten_stores(stores);
}


void store_reset(void)
{
    int i, j;
    struct store *s;

    for (i = 0; i < MAX_STORES; i++)
    {
        s = &stores[i];
        s->stock_num = 0;
        store_shuffle(s, true);
        object_pile_free(s->stock);
        s->stock = NULL;
        if (i >= STORE_TAVERN) continue;
        for (j = 0; j < 10; j++) store_maint(s, true);
    }

    memset(store_orders, 0, sizeof(store_orders));
}


struct init_module store_module =
{
    "store",
    store_init,
    cleanup_stores
};


/*
 * Check if a given item kind is an always-stocked item.
 */
static bool store_is_staple(struct store *s, struct object_kind *k)
{
    size_t i;

    my_assert(s);
    my_assert(k);

    for (i = 0; i < s->always_num; i++)
    {
        struct object_kind *l = s->always_table[i];

        if (k == l)
            return true;
    }

    return false;
}


/*
 * Check if a given item kind is an always-stocked or sometimes-stocked item.
 */
static bool store_can_carry(struct store *s, struct object_kind *kind)
{
    size_t i;

    for (i = 0; i < s->normal_num; i++)
    {
        if (s->normal_table[i] == kind)
            return true;
    }

    return store_is_staple(s, kind);
}


/*
 * Flavour text stuff
 */


/*
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
    "Wow. I'm going to name my new villa in your honour."
};


/*
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(struct player *p, int price, int value, int guess)
{
    /* Item was worthless, but we bought it */
    if ((value <= 0) && (price > value))
        msgt(p, MSG_STORE1, ONE_OF(comment_worthless));

    /* Item was cheaper than we thought, and we paid more than necessary */
    else if ((value < guess) && (price > value))
        msgt(p, MSG_STORE2, ONE_OF(comment_bad));

    /* Item was a good bargain, and we got away with it */
    else if ((value > guess) && (value < (4 * guess)) && (price < value))
        msgt(p, MSG_STORE3, ONE_OF(comment_good));

    /* Item was a great bargain, and we got away with it */
    else if ((value > guess) && (price < value))
        msgt(p, MSG_STORE4, ONE_OF(comment_great));
}


/*
 * Check if a store will buy an object
 */


/*
 * Determine if the current store will purchase the given object
 *
 * Note that a shop-keeper must refuse to buy "worthless" objects
 */
static bool store_will_buy(struct player *p, int sidx, const struct object *obj)
{
    struct object_buy *buy;
    struct store *store = &stores[sidx];

    /* Home accepts anything */
    if (store->sidx == STORE_HOME) return true;

    /* PWMAngband: don't accept objects that are not fully known in the General Store */
    if ((sidx == STORE_GENERAL) && !object_fully_known(p, obj)) return false;

    /* Ignore "worthless" items */
    if (!object_value(p, obj, 1)) return false;

    /* No buy list means we buy anything */
    if (!store->buy) return true;

    /* Run through the buy list */
    for (buy = store->buy; buy; buy = buy->next)
    {
        bitflag obj_flags[OF_SIZE];

        /* Wrong tval */
        if (buy->tval != obj->tval) continue;

        /* No flag means we're good */
        if (!buy->flag) return true;

        /* Get object flags */
        of_wipe(obj_flags);
        object_flags_aux(obj, obj_flags);

        /* OK if the object is known to have the flag */
        if (of_has(obj_flags, buy->flag) && object_flag_is_known(p, obj, buy->flag))
            return true;
    }

    /* Not on the list */
    return false;
}


/*
 * Basics: pricing, generation, etc.
 */


/*
 * Determine the price of an object in a store.
 *
 * store_buying == true means the shop is buying, player selling
 * false means the shop is selling, player buying
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * object, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- black markets always charge 2x and 5x/10x the normal price.
 */
s32b price_item(struct player *p, struct object *obj, bool store_buying, int qty)
{
    int adjust = 100;
    double price;
    struct owner *proprietor = store_at(p)->owner;
    int factor;

    /* Player owned shops */
    if (p->store_num == STORE_PLAYER)
    {
        double maxprice, askprice;
        struct house_type *house = house_get(p->player_store_num);

        /* Check for no_selling option */
        if (store_buying && my_stristr(quark_str(obj->note), "*for sale")) return (0L);

        /* Disable selling true artifacts */
        if (true_artifact_p(obj)) return (0L);

        /* Get the value of the given quantity of items */
        price = (double)object_value(p, obj, qty);

        /* Worthless items */
        if (price <= 0) return (0L);

        /* Get the desired value of the given quantity of items */
        askprice = (double)obj->askprice * qty;

        /* Allow items to be "shown" without being "for sale" */
        if (askprice <= 0) return (0L);

        /* Never get too silly: 2x the expensive black market price is enough! */
        maxprice = price * 20;

        /* Black markets suck */
        if (house->color == PLAYER_STORE_BM) price = price * 2;
        if (house->color == PLAYER_STORE_XBM) price = price * 10;

        /* Use sellers asking price as base price */
        if (askprice > price) price = askprice;
        if (price > maxprice) price = maxprice;

        /* Paranoia */
        if (price > PY_MAX_GOLD) return PY_MAX_GOLD;

        /* Return the price */
        return (s32b)price;
    }

    /* Get the value of the stack of wands, or a single item */
    if (tval_can_have_charges(obj))
        price = (double)object_value(p, obj, qty);
    else
        price = (double)object_value(p, obj, 1);

    /* Worthless items */
    if (price <= 0) return (0L);

    /* The black market is always a worse deal */
    if (store_black_market(p->store_num)) adjust = 150;

    /* Hack -- expensive BM factor */
    if (cfg_diving_mode == 2) factor = 5;
    else factor = 10;

    /* Shop is buying */
    if (store_buying)
    {
        /* Set the factor */
        adjust = 100 + (100 - adjust);

        /* Shops now pay 2/3 of true value */
        price = price * 2 / 3;

        /* Black markets suck */
        if (p->store_num == STORE_B_MARKET) price = floor(price / 2);
        if (p->store_num == STORE_XBM) price = floor(price / factor);

        /* Check for no_selling option */
        if (OPT(p, birth_no_selling)) return (0L);
    }

    /* Shop is selling */
    else
    {
        /* Black markets suck */
        if (p->store_num == STORE_B_MARKET) price = price * 2;
        if (p->store_num == STORE_XBM) price = price * factor;
    }

    /* Compute the final price (with rounding) */
    price = floor((price * adjust + 50L) / 100L);

    /* Now convert price to total price for non-wands */
    if (!tval_can_have_charges(obj))
        price *= qty;

    /* Now limit the price to the purse limit */
    if (store_buying && (price > proprietor->max_cost * qty))
        price = proprietor->max_cost * qty;

    /* Note -- never become "free" */
    if (price <= 0) return (qty);

    /* Paranoia */
    if (price > PY_MAX_GOLD) return PY_MAX_GOLD;

    /* Return the price */
    return (s32b)price;
}


/*
 * Special "mass production" computation
 */
static int mass_roll(int num, int max)
{
    int i, t = 0;
    for (i = 0; i < num; i++) t += randint0(max);
    return (t);
}


/*
 * Some cheap objects should be created in piles.
 */
static void mass_produce(struct object *obj)
{
    int size = 1;
    int cost = object_value(NULL, obj, 1);

    /* Analyze the type */
    switch (obj->tval)
    {
        /* Food, Flasks, and Lights */
        case TV_FOOD:
        case TV_MUSHROOM:
        case TV_CROP:
        case TV_FLASK:
        case TV_LIGHT:
        {
            if (cost <= 5) size += mass_roll(3, 5);
            if (cost <= 20) size += mass_roll(3, 5);
            break;
        }

        case TV_POTION:
        case TV_SCROLL:
        {
            if (cost <= 60) size += mass_roll(3, 5);
            if (cost <= 240) size += mass_roll(1, 5);
            break;
        }

        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_SORCERY_BOOK:
        case TV_SHADOW_BOOK:
        case TV_HUNT_BOOK:
        case TV_PSI_BOOK:
        case TV_DEATH_BOOK:
        case TV_ELEM_BOOK:
        case TV_SUMMON_BOOK:
        {
            if (cost <= 50) size += mass_roll(2, 3);
            if (cost <= 500) size += mass_roll(1, 3);
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
        case TV_MSTAFF:
        case TV_DIGGING:
        case TV_BOW:
        {
            if (obj->ego) break;
            if (cost <= 10) size += mass_roll(3, 5);
            if (cost <= 100) size += mass_roll(3, 5);
            break;
        }

        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            if (of_has(obj->flags, OF_AMMO_MAGIC)) break;
            if (cost <= 5)
                size = randint1(2) * 20;         /* 20-40 in 20s */
            else if ((cost > 5) && (cost <= 50))
                size = randint1(4) * 10;         /* 10-40 in 10s */
            else if ((cost > 50) && (cost <= 500))
                size = randint1(4) * 5;          /* 5-20 in 5s */
            else
                size = 1;
            break;
        }
    }

    /* Save the total pile size */
    obj->number = size;
}


/*
 * Sort the store inventory into an ordered array.
 */
void store_stock_list(struct player *p, struct store *store, struct object **list, int n)
{
    int list_num;
    int num = 0;
    bool home = ((store->sidx == STORE_HOME)? true: false);

    for (list_num = 0; list_num < n; list_num++)
    {
        struct object *current, *first = NULL;

        for (current = store->stock; current; current = current->next)
        {
            int i;
            bool possible = true;

            /* Skip objects already allocated */
            for (i = 0; i < num; i++)
            {
                if (list[i] == current) possible = false;
            }

            /* If still possible, choose the first in order */
            if (!possible) continue;
            if (earlier_object(home? p: NULL, first, current, !home))
                first = current;
        }

        /* Allocate and count the stock */
        list[list_num] = first;
        if (first) num++;
    }
}


/*
 * Allow a store item to absorb another item
 */
static void store_object_absorb(struct object *obj, struct object *new_obj)
{
    int total = obj->number + new_obj->number;

    /* Combine quantity, lose excess items */
    obj->number = MIN(total, obj->kind->base->max_stack);

    /* Hack -- if wands/staves are stacking, combine the charges */
    if (tval_can_have_charges(obj))
        obj->pval += new_obj->pval;

    object_origin_combine(obj, new_obj);

    /* Fully absorbed */
    object_delete(&new_obj);
}


/*
 * Check to see if the shop will be carrying too many objects
 * Note that the shop, just like a player, will not accept things
 * it cannot hold. Before, one could "nuke" potions this way.
 */
static bool store_check_num(struct player *p, struct store *store, struct object *obj)
{
    struct object *stock_obj;
    bool home = ((store->sidx == STORE_HOME)? true: false);
    object_stack_t mode = ((store->sidx == STORE_HOME)? OSTACK_PACK: OSTACK_STORE);

    /* Free space is always usable */
    if (store->stock_num < store->stock_size) return true;

    /* The "home" acts like the player */
    /* Normal stores do special stuff */
    for (stock_obj = store->stock; stock_obj; stock_obj = stock_obj->next)
    {
        /* Can the new object be combined with the old one? */
        if (object_similar(home? p: NULL, stock_obj, obj, mode)) return true;
    }

    /* But there was no room at the inn... */
    return false;
}


/*
 * Add an object to the inventory of the Home.
 *
 * Also note that it may not correctly "adapt" to "knowledge" becoming
 * known: the player may have to pick stuff up and drop it again.
 */
struct object *home_carry(struct player *p, struct store *store, struct object *obj)
{
    struct object *temp_obj;

    /* Check each existing object (try to combine) */
    for (temp_obj = store->stock; temp_obj; temp_obj = temp_obj->next)
    {
        /* The home acts just like the player */
        if (object_similar(p, temp_obj, obj, OSTACK_PACK))
        {
            /* Save the new number of items */
            object_absorb(temp_obj, obj);
            return temp_obj;
        }
    }

    /* No space? */
    if (store->stock_num >= store->stock_size) return NULL;

    /* Insert the new object */
    pile_insert(&store->stock, obj);
    store->stock_num++;
    return obj;
}


static bool str_contains(const char *str, const char *substr)
{
    bool found = false;
    char *s, *t;

    s = string_make(substr);
    t = strtok(s, "|");
    while (t)
    {
        /* Check loosely */
        if (!strstr(str, t))
        {
            found = false;
            break;
        }

        found = true;
        t = strtok(NULL, "|");
    }
    string_free(s);

    return found;
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
 * Returns the object inserted (for ease of use) or NULL if it disappears
 */
struct object *store_carry(struct player *p, struct store *store, struct object *obj)
{
    unsigned int i;
    s32b value;
    struct object *temp_obj;

    /* Evaluate the object */
    value = (s32b)object_value(p, obj, 1);

    /* Cursed/Worthless items "disappear" when sold */
    if (!value) return NULL;

    /* Erase the inscription */
    obj->note = 0;

    /* Some item types require maintenance */
    if (tval_is_light(obj))
        fuel_default(obj);
    else if (tval_can_have_timeout(obj))
        obj->timeout = 0;
    else if (tval_can_have_charges(obj))
    {
        /* If the store can stock this item kind, we recharge */
        if (store_can_carry(store, obj->kind))
        {
            int charges = 0;

            /* Calculate the recharged number of charges */
            for (i = 0; i < (unsigned int)obj->number; i++)
                charges += randcalc(obj->kind->charge, 0, RANDOMISE);

            /* Use recharged value only if greater */
            if (charges > obj->pval)
                obj->pval = charges;
        }
    }

    /* Check each existing object (try to combine) */
    for (temp_obj = store->stock; temp_obj; temp_obj = temp_obj->next)
    {
        /* Can the existing items be incremented? */
        if (object_similar(p, temp_obj, obj, OSTACK_STORE))
        {
            /* Absorb (some of) the object */
            store_object_absorb(temp_obj, obj);

            /* All done */
            return temp_obj;
        }
    }

    /* No space? */
    if (store->stock_num >= store->stock_size) return NULL;

    /* Check for orders */
    if ((store->sidx == STORE_XBM) && !obj->ordered)
    {
        char o_name[NORMAL_WID];
        char *str;

        /* Describe the object and lowercase the result */
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
        for (str = (char*)o_name; *str; str++) *str = tolower((unsigned char)*str);

        for (i = 0; i < STORE_ORDERS; i++)
        {
            if (STRZERO(store_orders[i])) continue;

            /* Check loosely */
            if (str_contains(o_name, store_orders[i]))
            {
                /* Flag the item as "ordered" */
                obj->ordered = 1 + i;
                my_strcpy(store_orders[i], "{ordered}", sizeof(store_orders[0]));

                break;
            }
        }
    }

    /* Insert the new object */
    pile_insert(&store->stock, obj);
    store->stock_num++;

    return obj;
}


/*
 * Remove an object from a store's stock, leaving it unattached
 */
static void store_delete(struct store *s, struct object *obj, int amt)
{
    if (obj->number > amt)
        obj->number -= amt;
    else
    {
        pile_excise(&s->stock, obj);

        /* Hack -- excise object index */
        obj->oidx = 0;

        /* Remove the corresponding order */
        if (obj->ordered)
            my_strcpy(store_orders[obj->ordered - 1], "", sizeof(store_orders[0]));

        object_delete(&obj);
        s->stock_num--;
    }
}


/*
 * Find a given object kind in the store.
 */
static struct object *store_find_kind(struct store *s, struct object_kind *k)
{
    struct object *obj;

    my_assert(s);
    my_assert(k);

    /* Check if it's already in stock */
    for (obj = s->stock; obj; obj = obj->next)
    {
        if ((obj->kind == k) && !obj->ego)
            return obj;
    }

    return NULL;
}


/*
 * Delete a random object from store, or, if it is a stack, perhaps only
 * partially delete it.
 *
 * This function is used when store maintainance occurs, and is designed to
 * imitate non-PC purchasers making purchases from the store.
 *
 * The reason this doesn't check for "staple" items and refuse to
 * delete them is that a store could conceivably have two stacks of a
 * single staple item, in which case, you could have a store which had
 * more stacks than staple items, but all stacks are staple items.
 */
static void store_delete_random(struct store *store)
{
    int what;
    int num;
    struct object *obj;

    /* Paranoia */
    if (store->stock_num <= 0) return;

    /* Pick a random slot */
    what = randint0(store->stock_num);

    /* Walk through list until we find our item */
    obj = store->stock;
    while (what--) obj = obj->next;

    /* Hack -- ordered items stay in the shop until bought */
    if (obj->ordered) return;

    /* Determine how many objects are in the slot */
    num = obj->number;

    /* Deal with stacks */
    if (num > 1)
    {
        /* Special behaviour for arrows, bolts, etc. */
        if (tval_is_ammo(obj) && !of_has(obj->flags, OF_AMMO_MAGIC))
        {
            /* 50% of the time, destroy the entire stack */
            if (magik(50) || (num < 10)) num = obj->number;

            /* 50% of the time, reduce the size to a multiple of 5 */
            else num = randint1(num / 5) * 5 + (num % 5);
        }
        else
        {
            /* 50% of the time, destroy a single object */
            if (magik(50)) num = 1;

            /* 25% of the time, destroy half the objects */
            else if (magik(50)) num = (num + 1) / 2;

            /* 25% of the time, destroy all objects */
            else num = obj->number;

            /* Hack -- decrement the total charges of staves and wands. */
            if (tval_can_have_charges(obj))
                obj->pval -= num * obj->pval / obj->number;
        }
    }

    my_assert(num <= obj->number);

    /* Delete the item, wholly or in part */
    store_delete(store, obj, num);
}


/*
 * This makes sure that the black market doesn't stock any object that other
 * stores have, unless it is an ego-item or has various bonuses.
 *
 * Based on a suggestion by Lee Vogt <lvogt@cig.mcel.mot.com>.
 */
static bool black_market_ok(struct object *obj)
{
    int i;

    /* Ego items are always fine */
    if (obj->ego) return true;

    /* Good items are normally fine */
    if (obj->to_a > 2) return true;
    if (obj->to_h > 1) return true;
    if (obj->to_d > 2) return true;

    /* No cheap items */
    if (object_value(NULL, obj, 1) < 10) return false;

    /* Check the other "normal" stores */
    for (i = 0; i < STORE_B_MARKET; i++)
    {
        struct object *stock_obj;

        /* Check every object in the store */
        for (stock_obj = stores[i].stock; stock_obj; stock_obj = stock_obj->next)
        {
            /* Compare object kinds */
            if (obj->kind == stock_obj->kind) return false;
        }
    }

    /* Otherwise fine */
    return true;
}


/*
 * Get a choice from the store allocation table, in tables.c
 */
static struct object_kind *store_get_choice(struct store *store)
{
    /* Choose a random entry from the store's table */
    return store->normal_table[randint0(store->normal_num)];
}


/*
 * Creates a random object and gives it to store
 */
static bool store_create_random(struct store *store)
{
    int tries, level;
    int min_level, max_level;

    /* Paranoia -- no room left */
    if (store->stock_num >= store->stock_size) return false;

    /* Decide min/max levels */
    if (store->sidx == STORE_B_MARKET)
    {
        min_level = MIN(store->max_depth + 5, 55);
        max_level = MIN(store->max_depth + 20, 70);
    }
    else if (store->sidx == STORE_XBM)
    {
        min_level = 55;
        max_level = 100;
    }
    else
    {
        min_level = 1;
        max_level = MIN(z_info->store_magic_level + MAX(store->max_depth - 20, 0), 70);
    }

    /* Consider up to six items */
    for (tries = 0; tries < 6; tries++)
    {
        struct object_kind *kind;
        struct object *obj;

        /* Work out the level for objects to be generated at */
        level = rand_range(min_level, max_level);

        /* Black Markets have a random object, of a given level */
        if (store_black_market(store->sidx))
            kind = get_obj_num(level, false, 0);
        else
            kind = store_get_choice(store);

        /*** Pre-generation filters ***/

        /* No chests in stores XXX */
        if (tval_is_chest_k(kind)) continue;

        /*** Generate the item ***/

        /* Create a new object of the chosen kind */
        obj = object_new();
        object_prep(NULL, obj, kind, level, RANDOMISE);

        /* Apply some "low-level" magic (no artifacts) */
        apply_magic(NULL, chunk_get(base_wpos()), obj, level, false, false, false, false);

        /* Reject if item is 'damaged' (negative combat mods, curses) */
        if ((tval_is_enchantable_weapon(obj) && ((obj->to_h < 0) || (obj->to_d < 0))) ||
            (tval_is_armor(obj) && (obj->to_a < 0)) || obj->curses)
        {
            object_delete(&obj);
            continue;
        }

        /*** Post-generation filters ***/

        /* Know everything but flavor, no origin yet */
        object_notice_everything_aux(NULL, obj, true, false);

        /* Black markets have expensive tastes */
        if (store_black_market(store->sidx) && !black_market_ok(obj))
        {
            object_delete(&obj);
            continue;
        }

        /* No "worthless" items */
        if (object_value(NULL, obj, 1) < 1)
        {
            object_delete(&obj);
            continue;
        }

        /* Mass produce */
        mass_produce(obj);

        /* Attempt to carry the object */
        if (!store_carry(NULL, store, obj))
        {
            object_delete(&obj);
            continue;
        }

        /* Definitely done */
        return true;
    }

    return false;
}


/*
 * Helper function: create an item with the given (tval,sval) pair, add it to the
 * store store_num. Return the item in the inventory.
 */
static struct object *store_create_item(struct store *store, struct object_kind *kind)
{
    struct object *obj = object_new();

    /* Create a new object of the chosen kind */
    object_prep(NULL, obj, kind, 0, RANDOMISE);

    /* Know everything but flavor, no origin yet */
    object_notice_everything_aux(NULL, obj, true, false);

    /* Attempt to carry the object */
    return store_carry(NULL, store, obj);
}


/*
 * Maintain the inventory at the stores.
 */
static void store_maint(struct store *s, bool force)
{
    int j, n = 0;

    /* Ignore tavern, home and player shops */
    if (s->sidx >= STORE_TAVERN) return;

    /* Make sure no one is in the store */
    if (!force)
    {
        for (j = 1; j <= NumPlayers; j++)
        {
            /* Check this player */
            if (player_get(j)->store_num == (int)s->sidx) return;
        }
    }

    /* Destroy crappy black market items */
    if (store_black_market(s->sidx))
    {
        struct object *obj = s->stock, *next;

        while (obj)
        {
            next = obj->next;
            if (!black_market_ok(obj))
                store_delete(s, obj, obj->number);
            obj = next;
        }
    }

    /* Check for orders */
    for (j = 0; ((s->sidx == STORE_XBM) && (j < STORE_ORDERS)); j++)
    {
        if (!STRZERO(store_orders[j])) n++;
    }

    /*
     * We want to make sure stores have staple items. If there's
     * turnover, we also want to delete a few items, and add a few
     * items.
     *
     * If we create staple items, then delete items, then create new
     * items, we are stuck with one of three choices:
     *   1. We can risk deleting staple items, and not having any left.
     *   2. We can refuse to delete staple items, and risk having that
     *      become an infinite loop.
     *   3. We can do a ton of extra bookkeeping to make sure we delete
     *      staple items only if there's duplicates of them.
     *
     * What if we change the order? First sell a handful of random items,
     * then create any missing staples, then create new items. This
     * has two tests for s->turnover, but simplifies everything else
     * dramatically.
     */
    if (s->turnover)
    {
        int restock_attempts = 100000;
        int stock = s->stock_num - randint1(s->turnover);

        /*
         * We'll end up adding staples for sure, maybe plus other
         * items. It's fine if we sell out completely, though, if
         * turnover is high. The cap doesn't include always_num,
         * because otherwise the addition of missing staples could
         * put us over (if the store was full of player-sold loot).
         *
         * PWMAngband -- check for orders to prevent endless loop
         */
        int min = n;
        int max = s->normal_stock_max;

        /* Keep stock between specified min and max slots */
        if (stock > max) stock = max;
        if (stock < min) stock = min;

        /* Destroy random objects until only "stock" slots are left */
        while ((s->stock_num > stock) && --restock_attempts)
            store_delete_random(s);

        if (!restock_attempts)
            quit_fmt("Unable to (de-)stock store %d. Please report this bug.", s->sidx + 1);
    }

    /* Ensure staples are created */
    if (s->always_num)
    {
        size_t i;

        for (i = 0; i < s->always_num; i++)
        {
            struct object_kind *kind = s->always_table[i];
            struct object *obj = store_find_kind(s, kind);

            /* Create the item if it doesn't exist */
            if (!obj) obj = store_create_item(s, kind);

            /* Ensure a full stack */
            obj->number = obj->kind->base->max_stack;
        }
    }

    if (s->turnover)
    {
        int restock_attempts = 100000;
        int stock = s->stock_num + randint1(s->turnover);

        /*
         * Now that the staples exist, we want to add more
         * items, at least enough to get us to normal_stock_min
         * items that aren't necessarily staples.
         */
        int min = s->normal_stock_min + s->always_num;
        int max = s->normal_stock_max + s->always_num;

        /* Keep stock between specified min and max slots */
        if (stock > max) stock = max;
        if (stock < min) stock = min;

        /*
         * The (huge) restock_attempts will only go to zero (otherwise
         * infinite loop) if stores don't have enough items they can stock!
         */
        while ((s->stock_num < stock) && --restock_attempts) store_create_random(s);

        if (!restock_attempts)
            quit_fmt("Unable to (re-)stock store %d. Please report this bug.", s->sidx + 1);
    }
}


/*
 * Update the stores.
 */
void store_update(void)
{
    if (!(turn.turn % (10L * z_info->store_turns)))
    {
        int n;

        /* Maintain each shop (except home) */
        for (n = 0; n < MAX_STORES; n++)
        {
            /* Skip the home */
            if (n == STORE_HOME) continue;

            /* Maintain */
            store_maint(&stores[n], false);
        }

        /* Sometimes, shuffle the shopkeepers */
        if (one_in_(z_info->store_shuffle))
        {
            /* Pick a random shop (except tavern, home and player store) */
            n = randint0(MAX_STORES - 3);

            /* Shuffle it */
            store_shuffle(&stores[n], false);
        }
    }
}


/** Owner stuff **/


struct owner *store_ownerbyidx(struct store *s, unsigned int idx)
{
    struct owner *o;
    for (o = s->owners; o; o = o->next)
    {
        if (o->oidx == idx) return o;
    }

    quit_fmt("Bad call to store_ownerbyidx: idx is %d", idx);
    return NULL;
}


static struct owner *store_choose_owner(struct store *s)
{
    struct owner *o;
    unsigned int n = 0;

    for (o = s->owners; o; o = o->next) n++;

    n = randint0(n);
    return store_ownerbyidx(s, n);
}


/*
 * Shuffle one of the stores.
 */
void store_shuffle(struct store *store, bool force)
{
    struct owner *o = store->owner;

    /* Make sure no one is in the store (ignore tavern and player shops) */
    if ((store->sidx < STORE_TAVERN) && !force)
    {
        int i;

        for (i = 1; i <= NumPlayers; i++)
        {
            /* Check this player */
            if (player_get(i)->store_num == (int)store->sidx) return;
        }
    }

    while (o == store->owner) o = store_choose_owner(store);
    store->owner = o;
}


/*
 * Display code
 */


/*
 * Return the quantity of a given item in the pack (include quiver).
 */
static byte find_inven(struct player *p, struct object *obj)
{
    int i;
    struct object *gear_obj;
    int num = 0;

    /* Similar slot? */
    for (gear_obj = p->gear; gear_obj; gear_obj = gear_obj->next)
    {
        /* Check only the inventory and the quiver */
        if (object_is_equipped(p->body, gear_obj)) continue;

        /* Require identical object types */
        if (obj->kind != gear_obj->kind) continue;

        /* Analyze the items */
        switch (obj->tval)
        {
            /* Chests */
            case TV_CHEST:
            {
                /* Never okay */
                return 0;
            }

            /* Food and Potions and Scrolls */
            case TV_FOOD:
            case TV_MUSHROOM:
            case TV_CROP:
            case TV_POTION:
            case TV_SCROLL:
            {
                /* Assume okay */
                break;
            }

            /* Staffs and Wands */
            case TV_STAFF:
            case TV_WAND:
            {
                /* Assume okay */
                break;
            }

            /* Rods */
            case TV_ROD:
            {
                /* Assume okay */
                break;
            }

            /* Weapons, Armor, Tools */
            case TV_BOW:
            case TV_DIGGING:
            case TV_HORN:
            case TV_HAFTED:
            case TV_POLEARM:
            case TV_SWORD:
            case TV_MSTAFF:
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
                /* Fall through */
            }

            /* Rings, Amulets, Lights */
            case TV_RING:
            case TV_AMULET:
            case TV_LIGHT:
            {
                /* Require both items to be known */
                if (!object_is_known(p, obj) || !object_is_known(p, gear_obj))
                    continue;

                /* Require identical curses */
                if (!curses_are_equal(obj, gear_obj))
                    continue;

                /* Fall through */
            }

            /* Missiles */
            case TV_ROCK:
            case TV_BOLT:
            case TV_ARROW:
            case TV_SHOT:
            {
                /* Require identical knowledge of both items */
                if (object_is_known(p, obj) != object_is_known(p, gear_obj))
                    continue;

                /* Require identical "bonuses" */
                if (obj->to_h != gear_obj->to_h) continue;
                if (obj->to_d != gear_obj->to_d) continue;
                if (obj->to_a != gear_obj->to_a) continue;

                /* Require identical modifiers */
                for (i = 0; i < OBJ_MOD_MAX; i++)
                {
                    if (obj->modifiers[i] != gear_obj->modifiers[i]) continue;
                }

                /* Require identical "artifact" names */
                if (obj->artifact != gear_obj->artifact) continue;

                /* Require identical "ego-item" names */
                if (obj->ego != gear_obj->ego) continue;

                /* Require identical "random artifact" names */
                if (obj->randart_seed != gear_obj->randart_seed) continue;

                /* Lights must have same amount of fuel */
                if ((obj->timeout != gear_obj->timeout) && tval_is_light(obj))
                    continue;

                /* Require identical "values" */
                if (obj->ac != gear_obj->ac) continue;
                if (obj->dd != gear_obj->dd) continue;
                if (obj->ds != gear_obj->ds) continue;

                /* Probably okay */
                break;
            }

            /* Corpses */
            case TV_CORPSE:
            {
                /* Require identical monster type and timeout */
                if (obj->pval != gear_obj->pval) continue;
                if (obj->decay != gear_obj->decay) continue;

                /* Probably okay */
                break;
            }

            /* Various */
            default:
            {
                /* Require knowledge */
                if (!object_is_known(p, obj) || !object_is_known(p, gear_obj))
                    continue;

                /* Probably okay */
                break;
            }
        }

        /* Different flags */
        if (!of_is_equal(obj->flags, gear_obj->flags)) continue;

        /* They match, so add up */
        num += gear_obj->number;
    }

    return num;
}


/*
 * Send a single store entry
 */
static void display_entry(struct player *p, struct object *obj, bool home)
{
    s32b price = -1, amt = 0;
    char o_name[NORMAL_WID];
    byte attr;
    s16b wgt;
    byte num;

    /* Describe the object - preserving inscriptions in the home */
    if (home)
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
    else
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL | ODESC_STORE);

    /* Mark ordered objects */
    if ((p->store_num != STORE_PLAYER) && obj->ordered)
        my_strcat(o_name, " [*]", sizeof(o_name));

    attr = obj->kind->base->attr;

    /* Only show the weight of an individual item */
    wgt = obj->weight;

    /* Normal stores */
    if (p->store_num != STORE_PLAYER)
    {
        if (home)
            amt = obj->number;
        else
        {
            /* Price of one */
            price = price_item(p, obj, false, 1);

            /* Work out how many the player can afford */
            if (price == 0) amt = obj->number;
            else amt = p->au / price;

            if (amt > obj->number) amt = obj->number;

            /* Double check for wands/staves */
            if ((p->au >= price_item(p, obj, false, amt + 1)) && (amt < obj->number))
                amt++;
        }

        /* Limit to the number that can be carried */
        amt = MIN(amt, inven_carry_num(p, obj, false));
    }

    /* Player owned stores */
    else
    {
        /* Price of one */
        price = price_item(p, obj, false, 1);

        /* Viewing our own shop - the price we will get */
        if (house_owned_by(p, p->player_store_num))
            price = price * 9 / 10;

        /* Viewing someone else's shop - the price we will pay */
        else if (price)
        {
            amt = p->au / price;

            if (amt > obj->number) amt = obj->number;

            /* Double check for wands/staves */
            if ((p->au >= price_item(p, obj, false, amt + 1)) && (amt < obj->number))
                amt++;

            /* Limit to the number that can be carried */
            amt = MIN(amt, inven_carry_num(p, obj, false));
        }
    }

    /* Find the number of this item in the inventory */
    num = find_inven(p, obj);

    /* Send the info */
    Send_store(p, obj->oidx, attr, wgt, obj->number, num, price, obj->tval, (byte)amt, o_name);
}


static bool set_askprice(struct object *obj)
{
    const char *c;

    c = my_stristr(quark_str(obj->note), "for sale");
    if (c)
    {
        /* Get ask price, skip "for sale" */
        obj->askprice = 1;
        c += 8;
        if (*c == ' ') obj->askprice = atoi(c);

        return true;
    }

    return false;
}


/*
 * Send a store's inventory.
 *
 * Returns the number of items listed.
 */
static int display_inventory(struct player *p)
{
    struct store *store = store_at(p);
    bool home = ((store->sidx == STORE_HOME)? true: false);
    int i;

    /* Stock -- sorted array of stock items */
    struct object **stock_list = mem_zalloc(sizeof(struct object *) * z_info->store_inven_max);

    /* Hack -- map the Home to each player */
    if (home) store = p->home;

    store_stock_list(p, store, stock_list, z_info->store_inven_max);

    /* Display the items */
    for (i = 0; i < z_info->store_inven_max; i++)
    {
        struct object *obj = stock_list[i];

        /* Do not display "dead" items */
        if (!obj) break;

        /* Hack -- set index */
        obj->oidx = i;

        /* Display that line */
        display_entry(p, obj, home);
    }

    mem_free(stock_list);
    return (store->stock_num);
}


/*
 * Send a player store's inventory.
 *
 * Returns the number of items listed.
 */
static int display_live_inventory(struct player *p)
{
    int x, y, stocked;

    /* Send a "live" inventory */
    struct house_type *h_ptr = house_get(p->player_store_num);
    struct chunk *c = chunk_get(&h_ptr->wpos);

    /* Scan house */
    stocked = 0;
    for (y = h_ptr->y_1; y <= h_ptr->y_2; y++)
    {
        for (x = h_ptr->x_1; x <= h_ptr->x_2; x++)
        {
            struct object *obj, *copy;

            /* Scan all objects in the grid */
            for (obj = square_object(c, y, x); obj; obj = obj->next)
            {
                /* Must be for sale */
                if (!obj->note) continue;

                /* Get a copy of the object */
                copy = object_new();
                object_copy(copy, obj);

                /* Set ask price */
                copy->askprice = 0;
                if (set_askprice(copy))
                {
                    /* Know everything but flavor, no origin yet */
                    object_notice_everything_aux(p, copy, true, false);

                    /* Hack -- set index */
                    copy->oidx = stocked;

                    /* Remove any inscription */
                    copy->note = 0;

                    /* Display that line */
                    display_entry(p, copy, false);
                    stocked++;

                    /* Limited space available */
                    if (stocked == z_info->store_inven_max)
                    {
                        object_delete(&copy);
                        return stocked;
                    }
                }

                object_delete(&copy);
            }
        }
    }

    return stocked;
}


/*
 * Send player's gold
 */
static void store_prt_gold(struct player *p)
{
    Send_gold(p, p->au);
}


/*
 * Send store (after clearing screen)
 */
static void display_store(struct player *p)
{
    int stockcount;
    char store_name[NORMAL_WID];
    char store_owner_name[NORMAL_WID];
    s32b purse;

    /* Send the inventory */
    if (p->store_num != STORE_PLAYER)
        stockcount = display_inventory(p);
    else
        stockcount = display_live_inventory(p);

    /* Get the store info for normal stores */
    if (p->store_num != STORE_PLAYER)
    {
        struct owner *proprietor = store_at(p)->owner;
        const char *owner_name = proprietor->name;

        purse = proprietor->max_cost;

        /* Get the store name */
        strnfmt(store_name, sizeof(store_name), "%s", store_at(p)->name);

        /* Put the owner name and race */
        strnfmt(store_owner_name, sizeof(store_owner_name), "%s", owner_name);
    }

    /* Player owned stores */
    else
    {
        purse = 0;

        /* Get the store name */
        get_player_store_name(p->player_store_num, store_name, sizeof(store_name));

        /* Get the owner name */
        strnfmt(store_owner_name, sizeof(store_owner_name), "%s",
            house_get(p->player_store_num)->ownername);
    }

    /* Send the store info */
    Send_store_info(p, p->store_num, store_name, store_owner_name, stockcount, purse);
}


/*
 * Higher-level code
 */


/*
 * Look for an item in a player store
 * Return a sellable copy of that item
 */
static struct object *player_store_object(struct player *p, int item, struct object **original)
{
    int stocked = 0;
    int y, x;
    struct house_type *h_ptr = house_get(p->player_store_num);
    struct chunk *c = chunk_get(&h_ptr->wpos);

    /* Scan the store to find the item */
    for (y = h_ptr->y_1; y <= h_ptr->y_2; y++)
    {
        for (x = h_ptr->x_1; x <= h_ptr->x_2; x++)
        {
            /* Scan all objects in the grid */
            for (*original = square_object(c, y, x); *original; *original = (*original)->next)
            {
                struct object *obj;

                /* Must be for sale */
                if (!(*original)->note) continue;

                /* Get a copy of the object */
                obj = object_new();
                object_copy(obj, *original);

                /* Set ask price */
                obj->askprice = 0;
                if (set_askprice(obj))
                {
                    /* Is this the item we are looking for? */
                    if (item == stocked) return obj;

                    /* Keep looking */
                    stocked++;
                }

                object_delete(&obj);
            }
        }
    }

    /* If we didn't find this item, something has gone badly wrong */
    msg(p, "Sorry, this item is reserved.");

    return NULL;
}


/*  
 * Remove the given item from the players house who owns it and credit
 * this player with some gold for the transaction.
 */
static void sell_player_item(struct player *p, struct object *original, struct object *bought)
{
    s32b price;
    struct house_type *h_ptr = house_get(p->player_store_num);
    int y, x;
    int space_y, space_x;
    bool space_ok = false;
    struct chunk *c = chunk_get(&h_ptr->wpos);

    /* Full purchase */
    if (bought->number == original->number)
    {
        square_excise_object(c, original->iy, original->ix, original);
        object_delete(&original);
    }

    /* Partial purchase */
    else
    {
        /* Hack -- reduce the number of charges in the original stack */
        if (tval_can_have_charges(original))
            original->pval -= bought->pval;

        /* Reduce the pile of items */
        original->number -= bought->number;
    }

    /* Extract the price for the stack that has been sold */
    price = price_item(p, bought, true, bought->number);
    if (!price) return;

    /* Small sales tax */
    price = price * 9 / 10;

    /* Scan the store to find space for payment */
    for (y = h_ptr->y_1; y <= h_ptr->y_2; y++)
    {
        for (x = h_ptr->x_1; x <= h_ptr->x_2; x++)
        {
            struct object *obj = square_object(c, y, x);

            /* Find a pile of gold suitable for payment */
            if (obj)
            {
                if (tval_is_money(obj) && !obj->next)
                {
                    obj->pval += price;

                    /* Done */
                    return;
                }
            }

            /* Remember the first empty space */
            else if (!space_ok)
            {
                space_y = y;
                space_x = x;
                space_ok = true;
            }
        }
    }

    /* No pile of gold suitable for payment */
    /* The seller should ensure available space for gold deposit! */
    if (space_ok)
    {
        struct object *gold_obj = object_new();

        /* Make some gold */
        object_prep(p, gold_obj, money_kind("gold", price), 0, MINIMISE);

        /* How much gold to leave */
        gold_obj->pval = price;

        /* Put it in the house */
        drop_near(p, c, &gold_obj, 0, space_y, space_x, false, DROP_FADE);
    }
}


/*
 * Buy the item with the given index from the current store's inventory.
 */
void do_cmd_buy(struct player *p, int item, int amt)
{
    struct object *obj, *original, *bought;
    char o_name[NORMAL_WID];
    s32b price;
    struct store *store = store_at(p);
    byte origin = ((p->store_num == STORE_PLAYER)? ORIGIN_PLAYER: ORIGIN_STORE);

    /* Paranoia */
    if (item < 0) return;

    /* Player cannot buy from own store */
    if ((p->store_num == STORE_PLAYER) && house_owned_by(p, p->player_store_num))
    {
        msg(p, "You cannot buy from yourself.");
        return;
    }

    /* Don't sell if someone has just entered the house (anti-exploit) */
    if (p->store_num == STORE_PLAYER)
    {
        int i;

        for (i = 1; i <= NumPlayers; i++)
        {
            if (house_inside(player_get(i), p->player_store_num))
            {
                /* Eject any shopper */
                msg(p, "The shopkeeper is currently restocking.");
                Send_store_leave(p);
                return;
            }
        }
    }

    /* Player owned stores */
    if (p->store_num == STORE_PLAYER)
    {
        /* Scan the store to find the item */
        obj = player_store_object(p, item, &original);
        if (!obj) return;

        /* Know everything but flavor, no origin yet */
        object_notice_everything_aux(p, obj, true, false);
    }

    /* Normal stores */
    else
    {
        /* Get the actual object */
        for (obj = store->stock; obj; obj = obj->next)
        {
            if (obj->oidx == item) break;
        }
        if (!obj) return;
    }

    /* Check "shown" items */
    if ((price_item(p, obj, false, 1) == 0) ||
        object_prevent_inscription(p, obj, INSCRIPTION_PURCHASE, false))
    {
        msg(p, "Sorry, this item is not for sale.");
        if (p->store_num == STORE_PLAYER) object_delete(&obj);
        return;
    }

    /* Sanity check the number of items */
    if (amt < 1) amt = 1;
    if (amt > obj->number) amt = obj->number;

    /* Get desired object */
    bought = object_new();
    object_copy_amt(bought, obj, amt);

    /* Ensure we have room */
    if (bought->number > inven_carry_num(p, bought, false))
    {
        msg(p, "You cannot carry that many items.");
        object_delete(&bought);
        if (p->store_num == STORE_PLAYER) object_delete(&obj);
        return;
    }

    /* Note that the pack is too heavy */
    if (!weight_okay(p, bought))
    {
        msg(p, "You are already too burdened to carry another object.");
        object_delete(&bought);
        if (p->store_num == STORE_PLAYER) object_delete(&obj);
        return;
    }

    /* Describe the object (fully) */
    object_desc(p, o_name, sizeof(o_name), bought, ODESC_PREFIX | ODESC_FULL | ODESC_STORE);

    /* Extract the price for the entire stack */
    price = price_item(p, bought, false, bought->number);

    /* Paranoia */
    if (price > p->au)
    {
        msg(p, "You cannot afford that purchase.");
        object_delete(&bought);
        if (p->store_num == STORE_PLAYER) object_delete(&obj);
        return;
    }

    /* If this is a player shop we have sold a real item */
    if (p->store_num == STORE_PLAYER)
        sell_player_item(p, original, bought);

    /* Spend the money */
    p->au -= price;

    /* Bypass auto-ignore */
    bought->ignore_protect = 1;

    /* Know objects on buy */
    object_notice_everything(p, bought);

    /* Update the gear */
    p->upkeep->update |= (PU_INVEN);

    /* Combine the pack (later) */
    p->upkeep->notice |= (PN_COMBINE | PN_IGNORE);

    /* The object no longer belongs to the store */
    bought->bypass_aware = false;

    /* Message */
    if ((p->store_num != STORE_PLAYER) && one_in_(3))
        msgt(p, MSG_STORE5, ONE_OF(comment_accept));
    msg(p, "You bought %s for %d gold.", o_name, price);

    /* Erase the inscription */
    bought->note = 0;

    /* Erase the "ordered" flag */
    bought->ordered = 0;

    /* Give it an origin if it doesn't have one */
    if (bought->origin == ORIGIN_NONE)
        set_origin(bought, origin, p->wpos.depth, NULL);

    /* Ensure item owner = store owner */
    if (p->store_num == STORE_PLAYER)
    {
        const char *name = house_get(p->player_store_num)->ownername;
        hash_entry *ptr = lookup_player_by_name(name);

        bought->owner = ((ptr && ht_zero(&ptr->death_turn))? ptr->id: 0);

        /* Hack -- use o_name for audit :/ */
        strnfmt(o_name, sizeof(o_name), "PS %s-%d | %s-%d $ %d", p->name, (int)p->id, name,
            (int)bought->owner, price);
        audit(o_name);
        audit("PS+gold");
    }

    /* Hack -- reduce the number of charges in the original stack */
    if ((p->store_num != STORE_PLAYER) && tval_can_have_charges(obj))
        obj->pval -= bought->pval;

    /* Give it to the player */
    inven_carry(p, bought, true, true);

    /* Handle stuff */
    handle_stuff(p);

    /* Remove the bought objects from the store if it's not a staple */
    if ((p->store_num != STORE_PLAYER) && !store_is_staple(store, obj->kind))
    {
        /* Reduce or remove the item */
        store_delete(store, obj, amt);

        /* Store is empty */
        if (store->stock_num == 0)
        {
            int i;

            /* Sometimes shuffle the shopkeeper */
            if (one_in_(z_info->store_shuffle))
            {
                /* Shuffle */
                msg(p, "The shopkeeper retires.");
                store_shuffle(store, true);
            }

            /* Maintain */
            else
                msg(p, "The shopkeeper brings out some new stock.");

            /* New inventory */
            for (i = 0; i < 10; i++)
                store_maint(store, true);
        }
    }

    /* Resend the basic store info */
    display_store(p);
    store_prt_gold(p);

    if (p->store_num == STORE_PLAYER) object_delete(&obj);
}


/*
 * Retrieve the item with the given index from the home's inventory.
 */
void do_cmd_retrieve(struct player *p, int item, int amt)
{
    struct object *obj, *picked_item;
    struct store *store = store_at(p);

    /* Paranoia */
    if (item < 0) return;

    if (store->sidx != STORE_HOME)
    {
        msg(p, "You are not currently at home.");
        return;
    }

    /* Hack -- map the Home to each player */
    store = p->home;

    /* Get the actual object */
    for (obj = store->stock; obj; obj = obj->next)
    {
        if (obj->oidx == item) break;
    }
    if (!obj) return;

    /* Sanity check the number of items */
    if (amt < 1) amt = 1;
    if (amt > obj->number) amt = obj->number;

    /* Get desired object */
    picked_item = object_new();
    object_copy_amt(picked_item, obj, amt);

    /* Ensure we have room */
    if (picked_item->number > inven_carry_num(p, picked_item, false))
    {
        msg(p, "You cannot carry that many items.");
        object_delete(&picked_item);
        return;
    }

    /* Note that the pack is too heavy */
    if (!weight_okay(p, picked_item))
    {
        msg(p, "You are already too burdened to carry another object.");
        object_delete(&picked_item);
        return;
    }

    /* Distribute charges of wands, staves, or rods */
    distribute_charges(obj, picked_item, amt);

    /* Give it to the player */
    inven_carry(p, picked_item, true, true);

    /* Handle stuff */
    handle_stuff(p);

    /* Reduce or remove the item */
    store_delete(store, obj, amt);

    /* Resend the basic store info */
    display_store(p);
}


/*
 * Determine if the current store will purchase the given object
 */
bool store_will_buy_tester(struct player *p, const struct object *obj)
{
    struct store *store = store_at(p);

    if (!store) return false;

    return store_will_buy(p, p->store_num, obj);
}


/*
 * Sell an item to the current store.
 */
void do_cmd_sell(struct player *p, int item, int amt)
{
    struct store *store = store_at(p);
    s32b price;
    struct object *obj, *dummy;

    /* Paranoia */
    if (item < 0)
    {
        Send_store_sell(p, -1, false);
        return;
    }
    if (amt <= 0)
    {
        Send_store_sell(p, -1, false);
        return;
    }

    obj = object_from_index(p, item, true, true);
    if (!obj)
    {
        Send_store_sell(p, -1, false);
        return;
    }

    /* Cannot remove stuck objects */
    if (object_is_equipped(p->body, obj) && !obj_can_takeoff(obj))
    {
        msg(p, "Hmmm, it seems to be stuck.");
        Send_store_sell(p, -1, false);
        return;
    }

    /* Check the store wants the items being sold */
    if (!store_will_buy_tester(p, obj))
    {
        msg(p, "I do not wish to purchase this item.");
        Send_store_sell(p, -1, false);
        return;
    }

    /* Check preventive inscription '!s' */
    /* Check preventive inscription '!d' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_SELL, false) ||
        object_prevent_inscription(p, obj, INSCRIPTION_DROP, false))
    {
        msg(p, "The item's inscription prevents it.");
        Send_store_sell(p, -1, false);
        return;
    }

    /* Work out how many the player can sell */
    if (amt > obj->number) amt = obj->number;

    /* Get a copy of the object representing the number being sold */
    dummy = object_new();
    object_copy_amt(dummy, obj, amt);

    /* Check if the store has space for the items */
    if (!store_check_num(p, store, dummy))
    {
        msg(p, "I have not the room in my store to keep it.");
        object_delete(&dummy);
        Send_store_sell(p, -1, false);
        return;
    }

    /* Remove any inscription for stores */
    dummy->note = 0;

    /* Extract the value of the items */
    price = price_item(p, dummy, true, amt);
    object_delete(&dummy);

    /* Tell the client about the price */
    Send_store_sell(p, price, false);

    /* Save the info for the confirmation */
    p->current_selling = item;
    p->current_sell_amt = amt;
    p->current_sell_price = price;

    /* Wait for confirmation before actually selling */
}


/*
 * Stash an item in the home.
 */
void do_cmd_stash(struct player *p, int item, int amt)
{
    struct object *dummy;
    struct store *store = store_at(p);
    char o_name[NORMAL_WID];
    char label;
    struct object *obj, *dropped;
    bool none_left = false;

    /* Check we are somewhere we can stash items. */
    if (store->sidx != STORE_HOME)
    {
        msg(p, "You are not in your home.");
        return;
    }

    /* Hack -- map the Home to each player */
    store = p->home;

    /* Paranoia */
    if (item < 0) return;
    if (amt <= 0) return;

    obj = object_from_index(p, item, true, true);
    if (!obj) return;

    /* Cannot remove stuck objects */
    if (object_is_equipped(p->body, obj) && !obj_can_takeoff(obj))
    {
        msg(p, "Hmmm, it seems to be stuck.");
        return;
    }

    /* Check preventive inscription '!s' */
    /* Check preventive inscription '!d' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_SELL, false) ||
        object_prevent_inscription(p, obj, INSCRIPTION_DROP, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* True artifacts cannot be stashed at home except the Crown and Grond */
    if (true_artifact_p(obj) && !kf_has(obj->kind->kind_flags, KF_QUEST_ART))
    {
        msg(p, "You cannot drop this here.");
        return;
    }

    /* Work out how many the player can sell */
    if (amt > obj->number) amt = obj->number;

    /* Get a copy of the object representing the number being sold */
    dummy = object_new();
    object_copy_amt(dummy, obj, amt);

    /* Get the label */
    label = gear_to_label(p, obj);

    /* Check if the store has space for the items */
    if (!store_check_num(p, store, dummy))
    {
        msg(p, "Your home is full.");
        object_delete(&dummy);
        return;
    }

    object_delete(&dummy);

    /* Now get the real item */
    dropped = gear_object_for_use(p, obj, amt, false, &none_left);

    /* Describe */
    object_desc(p, o_name, sizeof(o_name), dropped, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    msg(p, "You drop %s (%c).", o_name, label);

    /* Handle stuff */
    handle_stuff(p);

    /* Let the home carry it */
    home_carry(p, store, dropped);

    /* Resend the basic store info */
    display_store(p);
}


/*
 * Sell an item to the store (part 2)
 */
void store_confirm(struct player *p)
{
    int amt;
    struct object *dummy_item;
    struct store *store = store_at(p);
    int price, dummy, value;
    char o_name[NORMAL_WID];
    char label;
    struct object *obj, *sold_item;
    bool none_left = false;

    /* Abort if we shouldn't be getting called */
    if (p->current_selling == -1) return;

    /* Get the inventory item */
    obj = object_from_index(p, p->current_selling, true, true);
    if (!obj) return;

    amt = p->current_sell_amt;

    /* Get a copy of the object representing the number being sold */
    dummy_item = object_new();
    object_copy_amt(dummy_item, obj, amt);

    /* Get the label */
    label = gear_to_label(p, obj);

    price = p->current_sell_price;

    /* Trash the saved variables */
    p->current_selling = -1;
    p->current_sell_amt = -1;
    p->current_sell_price = -1;

    /* Get some money */
    p->au += price;

    /* Mark artifact as sold */
    set_artifact_info(p, dummy_item, ARTS_SOLD);

    /* Update the gear */
    p->upkeep->update |= (PU_INVEN);

    /* Combine the pack (later) */
    p->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

    /* Get the "apparent" value */
    dummy = object_value(p, dummy_item, amt);
    object_delete(&dummy_item);

    /* Know original object */
    object_notice_everything(p, obj);

    /* Take a proper copy of the now known-about object. */
    sold_item = gear_object_for_use(p, obj, amt, false, &none_left);

    /* The item belongs to the store now */
    sold_item->bypass_aware = true;

    /* Get the "actual" value */
    value = object_value(p, sold_item, amt);

    /* Get the description all over again */
    object_desc(p, o_name, sizeof(o_name), sold_item, ODESC_PREFIX | ODESC_FULL);

    /* Describe the result (in message buffer) */
    if (OPT(p, birth_no_selling))
        msg(p, "You had %s (%c).", o_name, label);
    else
    {
        msg(p, "You sold %s (%c) for %d gold.", o_name, label, price);

        /* Analyze the prices (and comment verbally) */
        purchase_analyze(p, price, value, dummy);
    }

    /* Handle stuff */
    handle_stuff(p);

    /* Artifacts "disappear" when sold */
    if (sold_item->artifact)
    {
        /* Preserve any artifact */
        preserve_artifact_aux(sold_item);

        object_delete(&sold_item);
        store_prt_gold(p);
        return;
    }

    /* The store gets that (known) item */
    store_carry(NULL, store, sold_item);

    /* Resend the basic store info */
    display_store(p);
    store_prt_gold(p);
}


/*
 * Examine an item in a store
 */
void store_examine(struct player *p, int item, bool describe)
{
    struct store *store = store_at(p);
    struct object *obj;
    char header[NORMAL_WID];
    int odesc_flags = ODESC_PREFIX | ODESC_FULL;

    /* Items in the home get less description */
    if (store->sidx != STORE_HOME) odesc_flags |= ODESC_STORE;

    /* Player owned stores */
    if (p->store_num == STORE_PLAYER)
    {
        struct object *dummy;

        /* Scan the store to find the item */
        obj = player_store_object(p, item, &dummy);
        if (!obj) return;

        /* Know everything but flavor, no origin yet */
        object_notice_everything_aux(p, obj, true, false);
    }

    /* Normal stores */
    else
    {
        /* Hack -- map the Home to each player */
        if (store->sidx == STORE_HOME) store = p->home;

        /* Get the actual item */
        for (obj = store->stock; obj; obj = obj->next)
        {
            if (obj->oidx == item) break;
        }
        if (!obj) return;
    }

    /* Show full info in most stores, but normal info in player home */
    object_desc(p, header, sizeof(header), obj, odesc_flags);

    /* Describe object */
    if (describe)
    {
        char message[NORMAL_WID];
        char store_name[NORMAL_WID];
        int price;

        /* Get the store info for normal stores */
        if (p->store_num != STORE_PLAYER)
            strnfmt(store_name, sizeof(store_name), "%s", store_at(p)->name);

        /* Player owned stores */
        else
            get_player_store_name(p->player_store_num, store_name, sizeof(store_name));

        if (p->wpos.depth > 0)
        {
            struct worldpos wpos;

            COORDS_SET(&wpos, p->wpos.wy, p->wpos.wx, 0);
            strnfmt(message, sizeof(message), "%s: %s (%s at %d')", p->name, store_name,
                p->wpos.depth * 50, get_dungeon(&wpos)->name);
        }
        else
        {
            struct location *town = get_town(&p->wpos);

            if (town)
                strnfmt(message, sizeof(message), "%s: %s (%s)", p->name, store_name, town->name);
            else
            {
                strnfmt(message, sizeof(message), "%s: %s (%d, %d)", p->name, store_name,
                    p->wpos.wx, p->wpos.wy);
            }
        }
        msg_all(p, message, MSG_BROADCAST_STORE);

        price = ((store->sidx == STORE_HOME)? 0: price_item(p, obj, false, 1));
        if (price > 0)
            strnfmt(message, sizeof(message), "%s: %s (%d au)", p->name, header, price);
        else
            strnfmt(message, sizeof(message), "%s: %s", p->name, header);
        msg_all(p, message, MSG_BROADCAST_STORE);
    }

    /* Display object recall modally and wait for a keypress */
    else display_object_recall_interactive(p, obj, header);

    /* Handle stuff */
    handle_stuff(p);

    if (p->store_num == STORE_PLAYER) object_delete(&obj);
}


/*
 * Order an item
 */
void store_order(struct player *p, const char *buf)
{
    struct store *store = store_at(p);
    int i, idx = -1;
    struct object *obj;
    char o_name[NORMAL_WID];
    char *str;

    /* Paranoia */
    if (p->store_num != STORE_XBM)
    {
        msg(p, "You cannot order from this store.");
        return;
    }

    /* Check for space */
    for (i = 0; i < STORE_ORDERS; i++)
    {
        if (STRZERO(store_orders[i]))
        {
            idx = i;
            break;
        }
    }
    if (i == STORE_ORDERS)
    {
        msg(p, "Sorry, no more orders can be accepted at this time.");
        return;
    }

    /* Lowercase our search string */
    for (str = (char*)buf; *str; str++) *str = tolower((unsigned char)*str);

    /* Check if such item is already in stock */
    for (obj = store->stock; obj; obj = obj->next)
    {
        /* Discard if already ordered */
        if (obj->ordered) continue;

        /* Describe the object and lowercase the result */
        object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
        for (str = (char*)o_name; *str; str++) *str = tolower((unsigned char)*str);

        /* Check loosely */
        if (str_contains(o_name, buf))
        {
            /* Flag the item as "ordered" */
            obj->ordered = 1 + idx;

            /* Accept the order */
            msg(p, "Order accepted.");
            my_strcpy(store_orders[idx], "{ordered}", sizeof(store_orders[0]));

            return;
        }
    }

    /* Not in stock: place an order */
    msg(p, "Order accepted.");
    my_strcpy(store_orders[idx], buf, sizeof(store_orders[0]));
}


/*
 * Enter a store, and interact with it.
 *
 * pstore is -1 for normal stores or house index for player owned store
 */
void do_cmd_store(struct player *p, int pstore)
{
    int which, i;
    struct store *store;
    struct chunk *c = chunk_get(&p->wpos);

    /* Normal store */
    if (pstore < 0)
    {
        /* Verify a store */
        if (!square_isshop(c, p->py, p->px))
        {
            msg(p, "You see no store here.");
            return;
        }

        /* Extract the store code */
        which = square_shopnum(c, p->py, p->px);

        /* Hack -- ignore the tavern */
        if (which == STORE_TAVERN) return;

        /* Check if we can enter the store */
        if (OPT(p, birth_no_stores) || ((which == STORE_HOME) && (cfg_diving_mode != 1)))
        {
            msg(p, "The doors are locked.");
            return;
        }

        /* Store is closed if someone is already in the shop */
        for (i = 1; i <= NumPlayers; i++)
        {
            int which_player;

            /* Get this player */
            struct player *player = player_get(i);
            struct chunk *cave = chunk_get(&player->wpos);

            if (player == p) continue;

            /* Verify a store */
            if (!square_isshop(cave, player->py, player->px)) continue;

            /* Extract the store code */
            which_player = square_shopnum(cave, player->py, player->px);

            /* Hack -- ignore the tavern */
            if (which_player == STORE_TAVERN) continue;

            /* Hack -- ignore the Home */
            if (which_player == STORE_HOME) continue;

            /* Store is closed if someone is already in the shop */
            if (which_player == which)
            {
                msg(p, "The doors are locked.");
                return;
            }
        }

        /* Save the store number */
        p->store_num = which;

        /* Save the max level of this customer */
        store = store_at(p);
        store->max_depth = p->max_depth;

        /* Redraw (add selling prices) */
        p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
        handle_stuff(p);
    }

    /* Player owned store */
    else
    {
        /* Check if we can enter the store */
        if (OPT(p, birth_no_stores))
        {
            msg(p, "The doors are locked.");
            return;
        }

        /* Store is closed if someone is restocking (anti-exploit) */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *q = player_get(i);

            if ((p != q) && house_inside(q, pstore))
            {
                msg(p, "The doors are locked.");
                return;
            }
        }

        p->store_num = STORE_PLAYER;
        p->player_store_num = pstore;
    }

    sound(p, MSG_STORE_ENTER);

    /* Display the store */
    display_store(p);
}


bool check_store_drop_color(struct player *p, struct object *obj, byte color)
{
    /* Check color */
    switch (color)
    {
        case PLAYER_STORE_GENERAL: return store_will_buy(p, STORE_GENERAL, obj);
        case PLAYER_STORE_ARMOURY: return store_will_buy(p, STORE_ARMOR, obj);
        case PLAYER_STORE_SMITH: return store_will_buy(p, STORE_WEAPON, obj);
        case PLAYER_STORE_TEMPLE: return store_will_buy(p, STORE_TEMPLE, obj);
        case PLAYER_STORE_ALCHEMIST: return store_will_buy(p, STORE_ALCHEMY, obj);
        case PLAYER_STORE_MAGIC: return store_will_buy(p, STORE_MAGIC, obj);
        case PLAYER_STORE_LIBRARY: return store_will_buy(p, STORE_LIBRARY, obj);
        case PLAYER_STORE_BM: return store_will_buy(p, STORE_B_MARKET, obj);
        case PLAYER_STORE_XBM: return store_will_buy(p, STORE_XBM, obj);
        case PLAYER_STORE_TAVERN: return true;
        case PLAYER_STORE_HOME: return true;
    }

    /* Paranoia */
    return false;
}


bool check_store_drop(struct player *p, struct object *obj)
{
    int i;
    char store_name[NORMAL_WID];

    /* Check houses */
    for (i = 0; i < houses_count(); i++)
    {
        /* Are we inside this house? */
        if (!house_inside(p, i)) continue;

        /* If we don't own it, we can't drop anything inside! */
        if (!house_owned_by(p, i)) return false;

        /* Custom store: allow everything */
        if (get_player_store_name(i, store_name, sizeof(store_name)))
            return true;

        /* Check this house */
        return check_store_drop_color(p, obj, house_get(i)->color);
    }

    /* Not in a house */
    return true;
}


/*
 * Determine the price of an item for direct sale
 */
s32b player_price_item(struct player *p, struct object *obj)
{
    double price, maxprice, askprice;

    /* Is this item for sale? */
    if (!obj->note) return -1;
    if (!set_askprice(obj)) return -1;

    /* Get the value of all items */
    price = (double)object_value(p, obj, obj->number);

    /* Worthless items */
    if (price <= 0) return (0L);

    /* Get the desired value of all items */
    askprice = (double)obj->askprice * obj->number;

    /* Never get too silly: 2x the expensive black market price is enough! */
    maxprice = price * 20;

    /* BM Prices */
    price = price * 2;

    /* Use sellers asking price as base price */
    if (askprice > price) price = askprice;
    if (price > maxprice) price = maxprice;

    /* Paranoia */
    if (price > PY_MAX_GOLD) return PY_MAX_GOLD;

    /* Done */
    return (s32b)price;
}


static struct object *store_get_order_item(int order)
{
    struct store *store = &stores[STORE_XBM];
    struct object *obj;

    /* Iterate over stock items */
    for (obj = store->stock; obj; obj = obj->next)
    {
        /* Cancel the order */
        if (obj->ordered == 1 + order) return obj;
    }

    return NULL;
}


void store_cancel_order(int order)
{
    struct object *obj = store_get_order_item(order);

    /* Cancel the order */
    if (obj) obj->ordered = 0;
}


void store_get_order(int order, char *desc, int len)
{
    struct object *obj = store_get_order_item(order);

    /* Describe the object */
    if (obj)
        object_desc(NULL, desc, len, obj, ODESC_PREFIX | ODESC_FULL);
    else
        my_strcpy(desc, "cannot find the item!", len);
}
