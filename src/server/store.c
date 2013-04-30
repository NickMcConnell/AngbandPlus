/*
 * File: store.c
 * Purpose: Store stocking and UI (server side)
 *
 * Copyright (c) 1997 Robert A. Koeneke, James E. Wilson, Ben Harrison
 * Copyright (c) 2007 Andrew Sidwell, who rewrote a fair portion
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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
#include "../common/parser.h"
#include "../common/tvalsval.h"
#include "netserver.h"
#include "object/inventory.h"
#include "object/pval.h"
#include "party.h"
#include <math.h>


/*** Constants and definitions ***/


/* Some local constants */
#define STORE_TURNOVER  9   /* Normal shop turnover, per day */
#define STORE_OBJ_LEVEL 5   /* Magic Level for normal stores */

/* Black market */
#define store_black_market(st) \
    (((st) == STORE_B_MARKET) || ((st) == STORE_XBM))

/* Store orders */
char store_orders[STORE_MIN_KEEP][NORMAL_WID];


/*** Flavour text stuff ***/

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
 * Staple definitions.
 */
typedef enum
{
    MAKE_SINGLE, MAKE_NORMAL, MAKE_MAX
} create_mode;

static struct staple_type
{
    int tval, sval;
    create_mode mode;
} staples[] =
{
    {TV_FOOD, SV_FOOD_RATION, MAKE_NORMAL},
    {TV_LIGHT, SV_LIGHT_TORCH, MAKE_NORMAL},
    {TV_SCROLL, SV_SCROLL_WORD_OF_RECALL, MAKE_NORMAL},
    {TV_SCROLL, SV_SCROLL_PHASE_DOOR, MAKE_NORMAL},
    {TV_FLASK, 0, MAKE_NORMAL},
    {TV_SPIKE, 0, MAKE_NORMAL},
    {TV_SHOT, SV_AMMO_NORMAL, MAKE_MAX},
    {TV_ARROW, SV_AMMO_NORMAL, MAKE_MAX},
    {TV_BOLT, SV_AMMO_NORMAL, MAKE_MAX},
    {TV_DIGGING, SV_SHOVEL, MAKE_SINGLE},
    {TV_DIGGING, SV_PICK, MAKE_SINGLE},
    {TV_CLOAK, SV_CLOAK, MAKE_SINGLE},

    /* PWMAngband items */
    {TV_FOOD, SV_FOOD_PINT_OF_ALE, MAKE_NORMAL},
    {TV_ROCK, SV_AMMO_LIGHT, MAKE_MAX},
    {TV_SCROLL, SV_SCROLL_LIFE, MAKE_NORMAL},
    {TV_LIGHT, SV_LIGHT_LANTERN, MAKE_SINGLE}
};


static struct store *store_new(int idx)
{
    struct store *s = mem_zalloc(sizeof(*s));

    s->sidx = idx;
    s->stock = mem_zalloc(sizeof(*s->stock) * STORE_INVEN_MAX);
    s->stock_size = STORE_INVEN_MAX;

    return s;
}


/*
 * Get rid of stores at cleanup. Gets rid of everything.
 */
void free_stores(void)
{
    struct owner *o;
    struct owner *next;
    int i;

    /* Free the store inventories */
    for (i = 0; i < MAX_STORES; i++)
    {
        /* Get the store */
        struct store *store = &stores[i];

        /* Free the store inventory */
        mem_free(store->table);
        mem_free(store->stock);

        for (o = store->owners; o; o = next)
        {
            next = o->next;
            string_free(o->name);
            mem_free(o);
        }
    }

    mem_free(stores);
}


static enum parser_error parse_s(struct parser *p)
{
    struct store *h = parser_priv(p);
    struct store *s;
    unsigned int idx = parser_getuint(p, "index") - 1;
    unsigned int slots = parser_getuint(p, "slots");

    if ((idx < STORE_ARMOR) || (idx > STORE_LIBRARY))
        return PARSE_ERROR_OUT_OF_BOUNDS;

    s = store_new(parser_getuint(p, "index") - 1);
    s->table = mem_zalloc(sizeof(*s->table) * slots);
    s->table_size = slots;
    s->next = h;
    parser_setpriv(p, s);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_i(struct parser *p)
{
    struct store *s = parser_priv(p);
    unsigned int slots = parser_getuint(p, "slots");
    int tval = tval_find_idx(parser_getsym(p, "tval"));
    int sval = lookup_sval(tval, parser_getsym(p, "sval"));
    object_kind *kind = lookup_kind(tval, sval);

    if (!kind) return PARSE_ERROR_UNRECOGNISED_SVAL;

    if (s->table_num + slots > s->table_size)
        return PARSE_ERROR_TOO_MANY_ENTRIES;
    while (slots--)
        s->table[s->table_num++] = kind;

    return PARSE_ERROR_NONE;
}


static struct parser *store_parser_new(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "S uint index uint slots", parse_s);
    parser_reg(p, "I uint slots sym tval sym sval", parse_i);

    return p;
}


struct owner_parser_state
{
    struct store *stores;
    struct store *cur;
};


static enum parser_error parse_own_n(struct parser *p)
{
    struct owner_parser_state *s = parser_priv(p);
    unsigned int index = parser_getuint(p, "index");
    struct store *st;

    for (st = s->stores; st; st = st->next)
    {
        if (st->sidx == index)
        {
            s->cur = st;
            break;
        }
    }

    return (st? PARSE_ERROR_NONE: PARSE_ERROR_OUT_OF_BOUNDS);
}


static enum parser_error parse_own_s(struct parser *p)
{
    struct owner_parser_state *s = parser_priv(p);
    unsigned int maxcost = parser_getuint(p, "maxcost");
    char *name = string_make(parser_getstr(p, "name"));
    struct owner *o;

    if (!s->cur) return PARSE_ERROR_MISSING_RECORD_HEADER;

    o = mem_zalloc(sizeof(*o));
    o->oidx = (s->cur->owners ? s->cur->owners->oidx + 1 : 0);
    o->next = s->cur->owners;
    o->name = name;
    o->max_cost = maxcost;
    s->cur->owners = o;

    return PARSE_ERROR_NONE;
}


static struct parser *store_owner_parser_new(struct store *stores)
{
    struct parser *p = parser_new();
    struct owner_parser_state *s = mem_zalloc(sizeof(*s));

    s->stores = stores;
    s->cur = NULL;
    parser_setpriv(p, s);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint index", parse_own_n);
    parser_reg(p, "S uint maxcost str name", parse_own_s);

    return p;
}


/*
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(int Ind, s32b price, s32b value, s32b guess)
{
    player_type *p_ptr = player_get(Ind);

    /* Item was worthless, but we bought it */
    if ((value <= 0) && (price > value))
        msgt(p_ptr, MSG_STORE1, ONE_OF(comment_worthless));

    /* Item was cheaper than we thought, and we paid more than necessary */
    else if ((value < guess) && (price > value))
        msgt(p_ptr, MSG_STORE2, ONE_OF(comment_bad));

    /* Item was a good bargain, and we got away with it */
    else if ((value > guess) && (value < (4 * guess)) && (price < value))
        msgt(p_ptr, MSG_STORE3, ONE_OF(comment_good));

    /* Item was a great bargain, and we got away with it */
    else if ((value > guess) && (price < value))
        msgt(p_ptr, MSG_STORE4, ONE_OF(comment_great));
}


/*** Check if a store will buy an object ***/


/*
 * Determine if the current store will purchase the given item
 *
 * Note that a shop-keeper must refuse to buy "worthless" items
 */
static bool store_will_buy(struct player *p, int sidx, const object_type *o_ptr)
{
    /* Switch on the store */
    switch (sidx)
    {
        /* General Store */
        case STORE_GENERAL:
        {
            /* PWMAngband: don't accept unIDed objects */
            if (!object_is_known(p, o_ptr)) return FALSE;

            /* Accept lights (inc. oil), spikes and food */
            if ((o_ptr->tval == TV_LIGHT) || (o_ptr->tval == TV_FOOD) ||
                (o_ptr->tval == TV_FLASK) || (o_ptr->tval == TV_SPIKE))
            {
                break;
            }

            /* PWMAngband: accept tools */
            if ((o_ptr->tval == TV_DIGGING) || (o_ptr->tval == TV_HORN))
                 break;

            return FALSE;
        }

        /* Armoury */
        case STORE_ARMOR:
        {
            if (!armor_p(o_ptr)) return FALSE;
            break;
        }

        /* Weapon Shop */
        case STORE_WEAPON:
        {
            if (!wieldable_p(o_ptr) && (o_ptr->tval != TV_MSTAFF)) return FALSE;
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
                case TV_PRAYER_BOOK:
                    break;
                case TV_POLEARM:
                case TV_SWORD:
                {
                    /* Known blessed blades are accepted too */
                    if (object_is_known_blessed(p, o_ptr)) break;
                }
                default:
                    return FALSE;
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
                    break;
                default:
                    return FALSE;
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
                case TV_SCROLL:
                case TV_POTION:
                    break;
                default:
                    return FALSE;
            }
            break;
        }

        /* House of Arcanes */
        case STORE_LIBRARY:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_MAGIC_BOOK:
                case TV_SORCERY_BOOK:
                case TV_SHADOW_BOOK:
                case TV_HUNT_BOOK:
                case TV_PSI_BOOK:
                case TV_DEATH_BOOK:
                case TV_ELEM_BOOK:
                case TV_SUMMON_BOOK:
                    break;
                default:
                    return FALSE;
            }
            break;
        }
    }

    /* Ignore "worthless" items */
    if (!object_value(p, o_ptr, 1)) return FALSE;

    /* Assume okay */
    return TRUE;
}


/*** Basics: pricing, generation, etc. ***/


/*
 * Determine the price of an object in a store.
 *
 * store_buying == TRUE means the shop is buying, player selling
 * FALSE means the shop is selling, player buying
 *
 * This function takes into account the player's charisma, but
 * never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * object, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- black markets always charge 2x and 5x/10x the normal price.
 */
s32b price_item(struct player *p, object_type *o_ptr, bool store_buying, int qty)
{
    owner_type *ot_ptr = stores[p->store_num].owner;
    int adjust, factor;
    double price;

    /* Player owned shops */
    if (p->store_num == STORE_PLAYER)
    {
        double maxprice, askprice;

        /* Check for no_selling option */
        if (store_buying && my_stristr(quark_str(o_ptr->note), "*for sale")) return (0L);

        /* Disable selling true artifacts */
        if (true_artifact_p(o_ptr)) return (0L);

        /* Get the value of the given quantity of items */
        price = (double)object_value(p, o_ptr, qty);

        /* Worthless items */
        if (price <= 0) return (0L);

        /* Get the desired value of the given quantity of items */
        askprice = (double)o_ptr->askprice * qty;

        /* Allow items to be "shown" without being "for sale" */
        if (askprice <= 0) return (0L);

        /* Never get too silly: 2x the expensive black market price is enough! */
        maxprice = price * 20;

        /* Black markets suck */
        if (houses[p->player_store_num].color == PLAYER_STORE_BM)
            price = price * 2;
        if (houses[p->player_store_num].color == PLAYER_STORE_XBM)
            price = price * 10;

        /* Use sellers asking price as base price */
        if (askprice > price) price = askprice;
        if (price > maxprice) price = maxprice;

        /* Paranoia */
        if (price > PY_MAX_GOLD) return PY_MAX_GOLD;

        /* Return the price */
        return (s32b)price;
    }

    /* Get the value of the stack of wands, or a single item */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
        price = (double)object_value(p, o_ptr, qty);
    else
        price = (double)object_value(p, o_ptr, 1);

    /* Worthless items */
    if (price <= 0) return (0L);

    /* Add in the charisma factor */
    if (store_black_market(p->store_num))
        adjust = 150;
    else
        adjust = adj_chr_gold[p->state.stat_ind[A_CHR]];

    /* Hack -- Expensive BM factor */
    if (cfg_no_recall) factor = 5;
    else factor = 10;

    /* Shop is buying */
    if (store_buying)
    {
        /* Set the factor */
        adjust = 100 + (100 - adjust);
        if (adjust > 100) adjust = 100;

        /* Shops now pay 2/3 of true value */
        price = price * 2 / 3;

        /* Black markets suck */
        if (p->store_num == STORE_B_MARKET) price = floor(price / 2);
        if (p->store_num == STORE_XBM) price = floor(price / factor);

        /* Check for no_selling option */
        if (OPT_P(p, birth_no_selling)) return (0L);
    }

    /* Shop is selling */
    else
    {
        /* Fix the factor */
        if (adjust < 100) adjust = 100;

        /* Black markets suck */
        if (p->store_num == STORE_B_MARKET) price = price * 2;
        if (p->store_num == STORE_XBM) price = price * factor;
    }

    /* Compute the final price (with rounding) */
    price = floor((price * adjust + 50L) / 100L);

    /* Now convert price to total price for non-wands */
    if ((o_ptr->tval != TV_WAND) && (o_ptr->tval != TV_STAFF))
        price *= qty;

    /* Now limit the price to the purse limit */
    if (store_buying && (price > ot_ptr->max_cost * qty))
        price = ot_ptr->max_cost * qty;

    /* Note -- Never become "free" */
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
static void mass_produce(object_type *o_ptr)
{
    int size = 1;
    s32b cost = object_value(NULL, o_ptr, 1);

    /* Analyze the type */
    switch (o_ptr->tval)
    {
        /* Food, Flasks, and Lights */
        case TV_FOOD:
        case TV_CROP:
        case TV_FLASK:
        case TV_LIGHT:
        {
            if (cost <= 5L) size += mass_roll(3, 5);
            if (cost <= 20L) size += mass_roll(3, 5);
            break;
        }

        case TV_POTION:
        case TV_SCROLL:
        {
            if (cost <= 60L) size += mass_roll(3, 5);
            if (cost <= 240L) size += mass_roll(1, 5);
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
            if (cost <= 50L) size += mass_roll(2, 3);
            if (cost <= 500L) size += mass_roll(1, 3);
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
            if (o_ptr->ego) break;
            if (cost <= 10L) size += mass_roll(3, 5);
            if (cost <= 100L) size += mass_roll(3, 5);
            break;
        }

        case TV_SPIKE:
        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            if (o_ptr->sval == SV_AMMO_MAGIC) break;
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
    o_ptr->number = size;
}


/*
 * Allow a store item to absorb another item
 */
static void store_object_absorb(object_type *o_ptr, object_type *j_ptr)
{
    int total = o_ptr->number + j_ptr->number;

    /* Combine quantity, lose excess items */
    o_ptr->number = ((total >= MAX_STACK_SIZE)? MAX_STACK_SIZE - 1: total);

    /* Hack -- if wands/staves are stacking, combine the charges */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
        o_ptr->pval[DEFAULT_PVAL] += j_ptr->pval[DEFAULT_PVAL];

    /* Hack -- Blend "origins" */
    object_absorb_origin(o_ptr, j_ptr);
}


/*
 * Check to see if the shop will be carrying too many objects
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.  Before, one could "nuke" potions this way.
 */
static bool store_check_num(struct store *store, object_type *o_ptr)
{
    int i;
    object_type *j_ptr;

    /* Free space is always usable */
    if (store->stock_num < store->stock_size) return TRUE;

    /* Normal stores do special stuff */
    else
    {
        /* Check all the items */
        for (i = 0; i < store->stock_num; i++)
        {
            /* Get the existing item */
            j_ptr = &store->stock[i];

            /* Can the new object be combined with the old one? */
            if (object_similar(j_ptr, o_ptr, OSTACK_STORE)) return (TRUE);
        }
    }

    /* But there was no room at the inn... */
    return (FALSE);
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
static int store_carry(struct store *store, object_type *o_ptr)
{
    unsigned int i, slot;
    s32b value, j_value;
    object_type *j_ptr;
    char o_name[NORMAL_WID];
    char* str;

    /* Evaluate the object */
    value = object_value(NULL, o_ptr, 1);

    /* Cursed/Worthless items "disappear" when sold */
    if (!value) return (-1);

    /* Erase the inscription */
    o_ptr->note = 0;

    /* Some item types require maintenance */
    switch (o_ptr->tval)
    {
        /* Refuel lights to the standard amount */
        case TV_LIGHT: fuel_default(o_ptr); break;

        /* Recharge rods */
        case TV_ROD: o_ptr->timeout = 0; break;

        /* Possibly recharge wands and staves */
        case TV_STAFF:
        case TV_WAND:
        {
            bool recharge = FALSE;

            /* Recharge without fail if the store normally carries that type */
            for (i = 0; i < store->table_num; i++)
            {
                if (store->table[i] == o_ptr->kind)
                    recharge = TRUE;
            }

            if (recharge)
            {
                int charges = 0;

                /* Calculate the recharged number of charges */
                for (i = 0; i < o_ptr->number; i++)
                    charges += randcalc(o_ptr->kind->charge, 0, RANDOMISE);

                /* Use recharged value only if greater */
                if (charges > o_ptr->pval[DEFAULT_PVAL])
                    o_ptr->pval[DEFAULT_PVAL] = charges;
            }

            break;
        }
    }

    /* Check each existing object (try to combine) */
    for (slot = 0; slot < store->stock_num; slot++)
    {
        /* Get the existing object */
        j_ptr = &store->stock[slot];

        /* Can the existing items be incremented? */
        if (object_similar(j_ptr, o_ptr, OSTACK_STORE))
        {
            /* Absorb (some of) the object */
            store_object_absorb(j_ptr, o_ptr);

            /* All done */
            return (slot);
        }
    }

    /* No space? */
    if (store->stock_num >= store->stock_size) return (-1);

    /* Check existing slots to see if we must "slide" */
    for (slot = 0; slot < store->stock_num; slot++)
    {
        /* Get that object */
        j_ptr = &store->stock[slot];

        /* Objects sort by decreasing type */
        if (o_ptr->tval > j_ptr->tval) break;
        if (o_ptr->tval < j_ptr->tval) continue;

        /* Objects sort by increasing sval */
        if (o_ptr->sval < j_ptr->sval) break;
        if (o_ptr->sval > j_ptr->sval) continue;

        /* Evaluate that slot */
        j_value = object_value(NULL, j_ptr, 1);

        /* Objects sort by decreasing value */
        if (value > j_value) break;
        if (value < j_value) continue;
    }

    /* Slide the others up */
    for (i = store->stock_num; i > slot; i--)
    {
        /* Hack -- slide the objects */
        object_copy(&store->stock[i], &store->stock[i-1]);
    }

    /* More stuff now */
    store->stock_num++;

    /* Describe the object and lowercase the result */
    object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);
    for (str = (char*)o_name; *str; str++) *str = tolower((unsigned char)*str);

    /* Check for orders */
    for (i = 0; ((store->sidx == STORE_XBM) && (i < STORE_MIN_KEEP)); i++)
    {
        if (!STRZERO(store_orders[i]))
        {
            /* Check loosely */
            if (strstr(o_name, store_orders[i]))
            {
                /* Flag the item as "ordered" */
                o_ptr->ordered = 1 + i;
                my_strcpy(store_orders[i], "{ordered}", sizeof(store_orders[0]));

                break;
            }
        }
    }

    /* Hack -- Insert the new object */
    object_copy(&store->stock[slot], o_ptr);

    /* Return the location */
    return (slot);
}


/*
 * Increase, by a given amount, the number of a certain item
 * in a certain store.  This can result in zero items.
 */
static void store_item_increase(struct store *store, int item, int num)
{
    int cnt;
    object_type *o_ptr;

    /* Get the item */
    o_ptr = &store->stock[item];

    /* Verify the number */
    cnt = o_ptr->number + num;
    if (cnt > 255) cnt = 255;
    else if (cnt < 0) cnt = 0;

    /* Save the new number */
    o_ptr->number = cnt;
}


/*
 * Remove a slot if it is empty
 */
static void store_item_optimize(struct store *store, int item)
{
    int j;
    object_type *o_ptr;

    /* Get the item */
    o_ptr = &store->stock[item];

    /* Must exist */
    if (!o_ptr->kind) return;

    /* Must have no items */
    if (o_ptr->number) return;

    /* Remove the corresponding order */
    if (o_ptr->ordered)
        my_strcpy(store_orders[o_ptr->ordered - 1], "", sizeof(store_orders[0]));

    /* One less item */
    store->stock_num--;

    /* Slide everyone */
    for (j = item; j < store->stock_num; j++)
        store->stock[j] = store->stock[j + 1];

    /* Nuke the final slot */
    object_wipe(&store->stock[j]);
}


/*
 * Delete an object from store, or, if it is a stack, perhaps only
 * partially delete it.
 */
static void store_delete_index(struct store *store, int what)
{
    int num;
    object_type *o_ptr;

    /* Paranoia */
    if (store->stock_num <= 0) return;

    /* Get the object */
    o_ptr = &store->stock[what];

    /* Determine how many objects are in the slot */
    num = o_ptr->number;

    /* Deal with stacks */
    if (num > 1)
    {
        /* Special behaviour for arrows, bolts, etc. */
        if ((o_ptr->tval == TV_SPIKE) || (obj_is_ammo(NULL, o_ptr) &&
            (o_ptr->sval != SV_AMMO_MAGIC)))
        {
            /* 50% of the time, destroy the entire stack */
            if (magik(50) || (num < 10)) num = o_ptr->number;

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
            else num = o_ptr->number;

            /* Hack -- Decrement the total charges of staves and wands. */
            if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
                o_ptr->pval[DEFAULT_PVAL] -= num * o_ptr->pval[DEFAULT_PVAL] / o_ptr->number;
        }
    }

    /* Delete the item */
    store_item_increase(store, what, 0 - num);
    store_item_optimize(store, what);
}


/*
 * Delete a random object from store, or, if it is a stack, perhaps only
 * partially delete it.
 *
 * This function is used when store maintainance occurs, and is designed to
 * imitate non-PC purchasers making purchases from the store.
 */
static void store_delete_random(struct store *store)
{
    int what;

    /* Paranoia */
    if (store->stock_num <= 0) return;

    /* Pick a random slot */
    what = randint0(store->stock_num);

    /* Hack -- Ordered items stay in the shop until bought */
    if (store->stock[what].ordered) return;

    store_delete_index(store, what);
}


/*
 * Delete a percentage of a store's inventory
 */
static void store_prune(struct store *store, int chance_in_1000)
{
    int i;

    for (i = 0; i < store->stock_num; i++)
    {
        if (CHANCE(chance_in_1000, 1000)) store_delete_index(store, i);
    }
}


/*
 * This makes sure that the black market doesn't stock any object that other
 * stores have, unless it is an ego-item or has various bonuses.
 *
 * Based on a suggestion by Lee Vogt <lvogt@cig.mcel.mot.com>.
 */
static bool black_market_ok(object_type *o_ptr)
{
    int i, j;

    /* Ego items are always fine */
    if (o_ptr->ego) return (TRUE);

    /* Good items are normally fine */
    if (o_ptr->to_a > 2) return (TRUE);
    if (o_ptr->to_h > 1) return (TRUE);
    if (o_ptr->to_d > 2) return (TRUE);

    /* No cheap items */
    if (object_value(NULL, o_ptr, 1) < 10) return (FALSE);

    /* Check the other "normal" stores */
    for (i = 0; i < STORE_B_MARKET; i++)
    {
        /* Check every object in the store */
        for (j = 0; j < stores[i].stock_num; j++)
        {
            object_type *j_ptr = &stores[i].stock[j];

            /* Compare object kinds */
            if (o_ptr->kind == j_ptr->kind) return (FALSE);
        }
    }

    /* Otherwise fine */
    return (TRUE);
}


/*
 * Get a choice from the store allocation table, in tables.c
 */
static object_kind *store_get_choice(struct store *store)
{
    int r;

    /* Choose a random entry from the store's table */
    r = randint0(store->table_num);

    /* Return it */
    return store->table[r];
}


/*
 * Creates a random object and gives it to store
 */
static bool store_create_random(struct store *store)
{
    int tries, level;
    object_type *i_ptr;
    object_type object_type_body;
    int min_level, max_level;

    /* Paranoia -- No room left */
    if (store->stock_num >= store->stock_size) return FALSE;

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
        max_level = MIN(STORE_OBJ_LEVEL + MAX(store->max_depth - 20, 0), 70);
    }

    /* Consider up to six items */
    for (tries = 0; tries < 6; tries++)
    {
        object_kind *kind;

        /* Work out the level for objects to be generated at */
        level = rand_range(min_level, max_level);

        /* Black Markets have a random object, of a given level */
        if (store_black_market(store->sidx))
            kind = get_obj_num(level, FALSE);
        else
            kind = store_get_choice(store);

        /*** Pre-generation filters ***/

        /* No chests in stores XXX */
        if (kind->tval == TV_CHEST) continue;

        /*** Generate the item ***/

        /* Get local object */
        i_ptr = &object_type_body;

        /* Create a new object of the chosen kind */
        object_prep(i_ptr, kind, level, RANDOMISE);

        /* Apply some "low-level" magic (no artifacts) */
        apply_magic(NULL, 0, i_ptr, level, FALSE, FALSE, FALSE);

        /* Reject if item is 'damaged' (i.e. negative mods) */
        if (wieldable_p(i_ptr) && ((i_ptr->to_h < 0) || (i_ptr->to_d < 0))) continue;
        if (armor_p(i_ptr) && (i_ptr->to_a < 0)) continue;

        /* The object is "known" and belongs to a store */
        object_notice_everything(NULL, i_ptr, TRUE);

        /*** Post-generation filters ***/

        /* Black markets have expensive tastes */
        if (store_black_market(store->sidx) && !black_market_ok(i_ptr)) continue;

        /* No "worthless" items */
        if (object_value(NULL, i_ptr, 1) < 1) continue;

        /* Mass produce */
        mass_produce(i_ptr);

        /* Attempt to carry the object */
        store_carry(store, i_ptr);

        /* Definitely done */
        return TRUE;
    }

    return FALSE;
}


/*
 * Helper function: create an item with the given (tval,sval) pair, add it to the
 * store store_num.  Return the slot in the inventory.
 */
static int store_create_item(struct store *store, object_kind *kind)
{
    object_type object;

    /* Create a new object of the chosen kind */
    object_prep(&object, kind, 0, RANDOMISE);

    /* The object is "known" and belongs to a store */
    object_notice_everything(NULL, &object, TRUE);

    /* Attempt to carry the object */
    return store_carry(store, &object);
}


/*
 * Create all staple items.
 */
static void store_create_staples(void)
{
    struct store *store = &stores[STORE_GENERAL];
    size_t i;

    /* Make sure there's enough room for staples */
    while (store->stock_num >= STORE_INVEN_MAX - N_ELEMENTS(staples))
        store_delete_random(store);

    /* Iterate through staples */
    for (i = 0; i < N_ELEMENTS(staples); i++)
    {
        struct staple_type *staple = &staples[i];

        /* Create the staple and combine it into the store inventory */
        int idx = store_create_item(store, lookup_kind(staple->tval, staple->sval));
        object_type *o_ptr = &store->stock[idx];

        my_assert(o_ptr);

        /* Tweak the quantities */
        switch (staple->mode)
        {
            case MAKE_SINGLE:
                o_ptr->number = 1;
                break;
            case MAKE_NORMAL:
                mass_produce(o_ptr);
                break;
            case MAKE_MAX:
                o_ptr->number = MAX_STACK_SIZE - 1;
                break;
        }
    }
}


/*
 * Maintain the inventory at the stores.
 */
void store_maint(struct store *store, bool force)
{
    int j;
    unsigned int stock;
    int restock_attempts = 100000;

    /* Ignore tavern and player shops */
    if (store->sidx >= STORE_TAVERN) return;

    /* Make sure no one is in the store */
    if (!force)
    {
        for (j = 1; j <= NumPlayers; j++)
        {
            /* Check this player */
            if (player_get(j)->store_num == store->sidx) return;
        }
    }

    /* General Store gets special treatment */
    if (store->sidx == STORE_GENERAL)
    {
        /* Sell off 30% of the inventory */
        store_prune(store, 300);
        store_create_staples();
        return;
    }

    /* Prune black markets */
    if (store_black_market(store->sidx))
    {
        /* Destroy crappy black market items */
        for (j = store->stock_num - 1; j >= 0; j--)
        {
            object_type *o_ptr = &store->stock[j];

            /* Destroy crappy items */
            if (!black_market_ok(o_ptr))
            {
                /* Destroy the item */
                store_item_increase(store, j, 0 - o_ptr->number);
                store_item_optimize(store, j);
            }
        }
    }

    /*** "Sell" various items */

    /* Sell a few items */
    stock = store->stock_num;
    stock -= randint1(STORE_TURNOVER);

    /* Keep stock between specified min and max slots */
    if (stock > STORE_MAX_KEEP) stock = STORE_MAX_KEEP;
    if (stock < STORE_MIN_KEEP) stock = STORE_MIN_KEEP;

    /* Destroy objects until only "j" slots are left */
    while (store->stock_num > stock) store_delete_random(store);

    /*** "Buy in" various items */

    /* Buy a few items */
    stock = store->stock_num;
    stock += randint1(STORE_TURNOVER);

    /* Keep stock between specified min and max slots */
    if (stock > STORE_MAX_KEEP) stock = STORE_MAX_KEEP;
    if (stock < STORE_MIN_KEEP) stock = STORE_MIN_KEEP;

    /*
     * The (huge) restock_attempts will only go to zero (otherwise
     * infinite loop) if stores don't have enough items they can stock!
     */
    while ((store->stock_num < stock) && --restock_attempts) store_create_random(store);

    if (!restock_attempts)
        quit_fmt("Unable to (re-)stock store %d. Please report this bug", store->sidx);
}


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


static struct store *parse_stores(void)
{
    struct parser *p = store_parser_new();
    struct store *stores;

    parse_file(p, "store");
    stores = parser_priv(p);
    parser_destroy(p);

    return stores;
}


static struct store *add_builtin_stores(struct store *stores)
{
    struct store *s0, *s1, *s2, *s3, *s4;

    s0 = store_new(STORE_GENERAL);
    s1 = store_new(STORE_B_MARKET);
    s2 = store_new(STORE_XBM);
    s3 = store_new(STORE_TAVERN);
    s4 = store_new(STORE_PLAYER);

    s0->next = stores;
    s1->next = s0;
    s2->next = s1;
    s3->next = s2;
    s4->next = s3;

    return s4;
}


static void parse_owners(struct store *stores)
{
    struct parser *p = store_owner_parser_new(stores);

    parse_file(p, "shop_own");
    mem_free(parser_priv(p));
    parser_destroy(p);
}


static struct store *flatten_stores(struct store *store_list)
{
    struct store *s;
    struct store *stores = mem_zalloc(MAX_STORES * sizeof(*stores));

    for (s = store_list; s; s = s->next)
        memcpy(&stores[s->sidx], s, sizeof(*s));

    while (store_list)
    {
        s = store_list->next;
        mem_free(store_list);
        store_list = s;
    }

    return stores;
}


void store_init(void)
{
    struct store *store_list;

    store_list = parse_stores();
    store_list = add_builtin_stores(store_list);
    parse_owners(store_list);
    stores = flatten_stores(store_list);
}


void store_reset(void)
{
    int i, j;
    struct store *s;

    for (i = 0; i < MAX_STORES; i++)
    {
        s = &stores[i];
        s->stock_num = 0;
        store_shuffle(s, TRUE);
        for (j = 0; j < s->stock_size; j++) object_wipe(&s->stock[j]);
        if (i >= STORE_TAVERN) continue;
        for (j = 0; j < 10; j++) store_maint(s, TRUE);
    }

    memset(store_orders, 0, STORE_MIN_KEEP * NORMAL_WID);
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
            if (player_get(i)->store_num == store->sidx) return;
        }
    }

    while (o == store->owner) o = store_choose_owner(store);
    store->owner = o;
}


/*** Display code ***/


/*
 * Return the quantity of a given item in the pack (include quiver).
 */
static byte find_inven(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);
    int i, j;
    int num = 0;
    bitflag oflags[MAX_PVALS][OF_SIZE], jflags[MAX_PVALS][OF_SIZE];

    /* Similar slot? */
    for (j = 0; j < QUIVER_END; j++)
    {
        object_type *j_ptr = &p_ptr->inventory[j];

        /* Check only the inventory and the quiver */
        if ((j >= INVEN_WIELD) && (j < QUIVER_START)) continue;

        /* Require identical object types */
        if (!j_ptr->kind || (o_ptr->kind != j_ptr->kind)) continue;

        /* Analyze the items */
        switch (o_ptr->tval)
        {
            /* Chests */
            case TV_CHEST:
            {
                /* Never okay */
                return 0;
            }

            /* Food and Potions and Scrolls */
            case TV_FOOD:
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
                if (!object_is_known(p_ptr, o_ptr) || !object_is_known(p_ptr, j_ptr))
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
                if (object_is_known(p_ptr, o_ptr) != object_is_known(p_ptr, j_ptr))
                    continue;

                /* Require identical "bonuses" */
                if (o_ptr->to_h != j_ptr->to_h) continue;
                if (o_ptr->to_d != j_ptr->to_d) continue;
                if (o_ptr->to_a != j_ptr->to_a) continue;

                object_pval_flags(o_ptr, oflags);
                object_pval_flags(j_ptr, jflags);

                /* Require identical "pval" codes */
                for (i = 0; i < MAX_PVALS; i++)
                {
                    if (o_ptr->pval[i] != j_ptr->pval[i]) continue;
                    if (!of_is_equal(oflags[i], jflags[i])) continue;
                }

                /* Require identical "artifact" names */
                if (o_ptr->artifact != j_ptr->artifact) continue;

                /* Require identical "ego-item" names */
                if (o_ptr->ego != j_ptr->ego) continue;

                /* Require identical "random artifact" names */
                if (o_ptr->randart_seed != j_ptr->randart_seed) continue;

                /* Lights must have same amount of fuel */
                if ((o_ptr->timeout != j_ptr->timeout) && (o_ptr->tval == TV_LIGHT))
                    continue;

                /* Require identical "values" */
                if (o_ptr->ac != j_ptr->ac) continue;
                if (o_ptr->dd != j_ptr->dd) continue;
                if (o_ptr->ds != j_ptr->ds) continue;

                /* Probably okay */
                break;
            }

            /* Corpses */
            case TV_CORPSE:
            {
                /* Require identical monster type and timeout */
                if (o_ptr->pval[DEFAULT_PVAL] != j_ptr->pval[DEFAULT_PVAL]) continue;
                if (o_ptr->pval[DEFAULT_PVAL + 1] != j_ptr->pval[DEFAULT_PVAL + 1]) continue;

                /* Probably okay */
                break;
            }

            /* Various */
            default:
            {
                /* Require knowledge */
                if (!object_is_known(p_ptr, o_ptr) || !object_is_known(p_ptr, j_ptr))
                    continue;

                /* Probably okay */
                break;
            }
        }

        /* Different flags */
        if (!of_is_equal(o_ptr->flags, j_ptr->flags)) continue;

        /* They match, so add up */
        num += j_ptr->number;
    }

    return num;
}


/*
 * Send a single store entry
 */
static void display_entry(int Ind, int pos)
{
    player_type *p_ptr = player_get(Ind);
    struct store *store = &stores[p_ptr->store_num];
    object_type *o_ptr;
    s32b price, amt;
    char o_name[NORMAL_WID];
    byte attr;
    s16b wgt;
    byte num;

    /* Get the item */
    o_ptr = &store->stock[pos];

    /* Describe the object (fully) */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_STORE);

    /* Mark ordered objects */
    if (o_ptr->ordered)
        my_strcat(o_name, " [*]", sizeof(o_name));

    attr = p_ptr->tval_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

    /* Only show the weight of an individual item */
    wgt = o_ptr->weight;

    /* Price of one */
    price = price_item(p_ptr, o_ptr, FALSE, 1);

    /* Work out how many the player can afford */
    amt = p_ptr->au / price;
    if (amt > o_ptr->number) amt = o_ptr->number;

    /* Double check for wands/staves */
    if ((p_ptr->au >= price_item(p_ptr, o_ptr, FALSE, amt + 1)) && (amt < o_ptr->number))
        amt++;

    /* Find the number of this item in the inventory */
    num = find_inven(Ind, o_ptr);

    /* Send the info */
    Send_store(Ind, pos, attr, wgt, o_ptr->number, num, price, o_ptr->tval, (byte)amt, o_name);
}


/*
 * Send a player owned store entry
 */
static void display_entry_live(int Ind, int pos, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);
    s32b price, amt = 0;
    char o_name[NORMAL_WID];
    byte attr;
    s16b wgt;
    byte num;

    /* Describe the object (fully) */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_STORE);

    attr = p_ptr->tval_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

    /* Only show the weight of an individual item */
    wgt = o_ptr->weight;

    /* Price of one */
    price = price_item(p_ptr, o_ptr, FALSE, 1);

    /* Viewing our own shop - the price we will get */
    if (house_owned_by(p_ptr, p_ptr->player_store_num))
        price = price * 9 / 10;

    /* Viewing someone else's shop - the price we will pay */
    else if (price)
    {
        /* Work out how many the player can afford */
        amt = p_ptr->au / price;
        if (amt > o_ptr->number) amt = o_ptr->number;

        /* Double check for wands/staves */
        if ((p_ptr->au >= price_item(p_ptr, o_ptr, FALSE, amt + 1)) && (amt < o_ptr->number))
            amt++;
    }

    /* Find the number of this item in the inventory */
    num = find_inven(Ind, o_ptr);

    /* Send the info */
    Send_store(Ind, pos, attr, wgt, o_ptr->number, num, price, o_ptr->tval, (byte)amt, o_name);
}


static bool set_askprice(object_type *o_ptr)
{
    char *c;

    c = my_stristr(quark_str(o_ptr->note), "for sale");
    if (c)
    {
        /* Get ask price, skip "for sale" */
        o_ptr->askprice = 1;
        c += 8;
        if (*c == ' ') o_ptr->askprice = atoi(c);

        return TRUE;
    }

    return FALSE;
}


/*
 * Send a store's inventory.
 */
static int display_inventory(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    struct store *store = &stores[p_ptr->store_num];
    int k, x, y;
    object_type *i_ptr;
    object_type object_type_body;

    /* Normal stores */
    if (p_ptr->store_num != STORE_PLAYER)
    {
        /* Display the items */
        for (k = 0; k < STORE_INVEN_MAX; k++)
        {
            /* Do not display "dead" items */
            if (k >= store->stock_num) break;

            /* Display that line */
            display_entry(Ind, k);
        }
        return (store->stock_num);
    }

    /* Player owned stores */
    else
    {
        /* Send a "live" inventory */
        house_type *h_ptr = &houses[p_ptr->player_store_num];

        /* Scan house */
        store->stock_num = 0;
        for (y = h_ptr->y_1; y <= h_ptr->y_2; y++)
        {
            for (x = h_ptr->x_1; x <= h_ptr->x_2; x++)
            {
                object_type *o_ptr;

                /* Scan all objects in the grid */
                for (o_ptr = get_first_object(h_ptr->depth, y, x); o_ptr;
                    o_ptr = get_next_object(o_ptr))
                {
                    /* Get local object */
                    i_ptr = &object_type_body;

                    /* Get a copy of the object */
                    object_copy(i_ptr, o_ptr);

                    /* If there was an object, is it for sale? */
                    if (i_ptr->note)
                    {
                        /* Set ask price */
                        i_ptr->askprice = 0;
                        if (set_askprice(i_ptr))
                        {
                            /* The object is "known" and belongs to a store */
                            object_notice_everything(p_ptr, i_ptr, TRUE);

                            /* Remove any inscription */
                            i_ptr->note = 0;
                            display_entry_live(Ind, store->stock_num, i_ptr);
                            store->stock_num++;

                            /* Limited space available */
                            if (store->stock_num == STORE_INVEN_MAX)
                                return (store->stock_num);
                        }
                    }
                }
            }
        }
    }

    return (store->stock_num);
}


/*
 * Send player's gold
 */
static void store_prt_gold(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    Send_gold(p_ptr, p_ptr->au);
}


/* Returns the name of a player owned store */
bool get_player_store_name(int num, char *name, int len)
{
    int x, y;
    char *c;

    /* Default title */
    switch (houses[num].color)
    {
        case PLAYER_STORE_BM: my_strcpy(name, "Black Market", len); break;
        case PLAYER_STORE_TAVERN: my_strcpy(name, "Home", len); break;
        case PLAYER_STORE_XBM: my_strcpy(name, "Expensive Black Market", len); break;
        case PLAYER_STORE_ALCHEMIST: my_strcpy(name, "Alchemy Shop", len); break;
        case PLAYER_STORE_TEMPLE: my_strcpy(name, "Ecclesial Shop", len); break;
        case PLAYER_STORE_MAGIC: my_strcpy(name, "Magic Shop", len); break;
        case PLAYER_STORE_LIBRARY: my_strcpy(name, "House of Arcanes", len); break;
        case PLAYER_STORE_ARMOURY: my_strcpy(name, "Armoury", len); break;
        case PLAYER_STORE_SMITH: my_strcpy(name, "Weapon Shop", len); break;
        case PLAYER_STORE_GENERAL: my_strcpy(name, "General Store", len); break;
    }

    /* Scan house */
    for (y = houses[num].y_1; y <= houses[num].y_2; y++)
    {
        for (x = houses[num].x_1; x <= houses[num].x_2; x++)
        {
            object_type *o_ptr;

            /* Scan all objects in the grid */
            for (o_ptr = get_first_object(houses[num].depth, y, x); o_ptr;
                o_ptr = get_next_object(o_ptr))
            {
                /* If there was an object, does it have a store name? */
                if (o_ptr->note)
                {
                    c = my_stristr(quark_str(o_ptr->note), "store name");
                    if (c)
                    {
                        /* Get name */
                        c += 10; /* skip "store name" */
                        if (*c++ == ' ')
                        {
                            my_strcpy(name, c, len);
                            return TRUE;
                        }
                    }
                }
            }
        }
    }
    return FALSE;
}


/*
 * Send store (after clearing screen)
 */
static void display_store(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int stockcount;
    char store_name[NORMAL_WID];
    char store_owner_name[NORMAL_WID];
    s32b purse;

    /* Send the inventory */
    stockcount = display_inventory(Ind);

    /* Get the store info for normal stores */
    if (p_ptr->store_num != STORE_PLAYER)
    {
        owner_type *ot_ptr = stores[p_ptr->store_num].owner;
        const char *owner_name = ot_ptr->name;

        purse = ot_ptr->max_cost;

        /* Get the store name */
        strnfmt(store_name, sizeof(store_name), "%s", store_names[p_ptr->store_num]);

        /* Put the owner name and race */
        strnfmt(store_owner_name, sizeof(store_owner_name), "%s", owner_name);
    }

    /* Player owned stores */
    else
    {
        purse = 0;

        /* Get the store name */
        get_player_store_name(p_ptr->player_store_num, store_name, sizeof(store_name));

        /* Get the owner name */
        strnfmt(store_owner_name, sizeof(store_owner_name), "%s",
            houses[p_ptr->player_store_num].ownername);
    }

    /* Send the store info */
    Send_store_info(Ind, p_ptr->store_num, store_name, store_owner_name, stockcount, purse);
}


/*** Higher-level code ***/


/*
 * Look for an item in a player store and get a sellable copy of that item
 * Return the index of the original item
 */
static s16b player_store_o_ptr(int Ind, int item, object_type *store_o_ptr)
{
    player_type *p_ptr = player_get(Ind);
    int stocked = 0;
    int y, x;
    house_type *h_ptr = &houses[p_ptr->player_store_num];

    /* Scan the store to find the item */
    for (y = h_ptr->y_1; y <= h_ptr->y_2; y++)
    {
        for (x = h_ptr->x_1; x <= h_ptr->x_2; x++)
        {
            s16b this_o_idx, next_o_idx = 0;
            object_type *o_ptr;

            /* Scan all objects in the grid */
            for (this_o_idx = cave_get(h_ptr->depth)->o_idx[y][x]; this_o_idx;
                this_o_idx = next_o_idx)
            {
                /* Get the object */
                o_ptr = object_byid(this_o_idx);

                /* Get the next object */
                next_o_idx = o_ptr->next_o_idx;

                /* Get a copy of the object */
                object_copy(store_o_ptr, o_ptr);

                /* If there was an object, is it for sale? */
                if (store_o_ptr->note)
                {
                    /* Set ask price */
                    store_o_ptr->askprice = 0;
                    if (set_askprice(store_o_ptr))
                    {
                        /* Is this the item we are looking for? */
                        if (item == stocked) return this_o_idx;

                        /* Keep looking */
                        stocked++;
                    }
                }
            }
        }
    }

    /* If we didn't find this item, something has gone badly wrong */
    msg(p_ptr, "Sorry, this item is reserved.");

    return 0;
}


/*  
 * Remove the given item from the players house who owns it and credit
 * this player with some gold for the transaction.
 */
static void sell_player_item(int Ind, s16b o_idx, object_type *i_ptr)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    s32b price;
    house_type *h_ptr = &houses[p_ptr->player_store_num];
    int y, x;
    int space_y, space_x;
    bool space_ok = FALSE;

    /* Get the original item */
    o_ptr = object_byid(o_idx);

    /* Full purchase */
    if (i_ptr->number == o_ptr->number)
        delete_object_idx(o_idx);

    /* Partial purchase */
    else
    {
        /* Hack - Reduce the number of charges in the original stack */
        if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
            o_ptr->pval[DEFAULT_PVAL] -= i_ptr->pval[DEFAULT_PVAL];

        /* Reduce the pile of items */
        o_ptr->number -= i_ptr->number;
    }

    /* Extract the price for the stack that has been sold */
    price = price_item(p_ptr, i_ptr, TRUE, i_ptr->number);
    if (!price) return;

    /* Small sales tax */
    price = price * 9 / 10;

    /* Scan the store to find space for payment */
    for (y = h_ptr->y_1; y <= h_ptr->y_2; y++)
    {
        for (x = h_ptr->x_1; x <= h_ptr->x_2; x++)
        {
            /* Find a pile of gold suitable for payment */
            if (cave_get(h_ptr->depth)->o_idx[y][x])
            {
                /* Get the object */
                o_ptr = object_byid(cave_get(h_ptr->depth)->o_idx[y][x]);

                /* Add some gold to the pile */
                if ((o_ptr->tval == TV_GOLD) && !o_ptr->next_o_idx)
                {
                    o_ptr->pval[DEFAULT_PVAL] += price;

                    /* Done */
                    return;
                }
            }

            /* Remember the first empty space */
            else if (!space_ok)
            {
                space_y = y;
                space_x = x;
                space_ok = TRUE;
            }
        }
    }

    /* No pile of gold suitable for payment */
    /* The seller should ensure available space for gold deposit! */
    if (space_ok)
    {
        object_type gold_obj;

        /* Make some gold */
        object_prep(&gold_obj, lookup_kind(TV_GOLD, SV_GOLD), 0, MINIMISE);

        /* How much gold to leave */
        gold_obj.pval[DEFAULT_PVAL] = price;

        /* Put it in the house */
        drop_near(p_ptr, cave_get(h_ptr->depth), &gold_obj, 0, space_y, space_x, FALSE);
    }
}


/*
 * Buy an object from a store
 */
void store_purchase(int Ind, int item, int amt)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;
    char o_name[NORMAL_WID];
    s32b price;
    int item_new;
    struct store *store = &stores[p_ptr->store_num];
    s16b o_idx;

    /* Paranoia */
    if (item < 0) return;

    /* Player cannot buy from own store */
    if ((p_ptr->store_num == STORE_PLAYER) &&
        house_owned_by(p_ptr, p_ptr->player_store_num))
    {
        msg(p_ptr, "You cannot buy from yourself.");
        return;
    }

    /* Don't sell if someone has just entered the house (anti-exploit) */
    if (p_ptr->store_num == STORE_PLAYER)
    {
        int i;

        for (i = 1; i <= NumPlayers; i++)
        {
            if (house_inside(player_get(i), p_ptr->player_store_num))
            {
                /* Eject any shopper */
                msg(p_ptr, "The shopkeeper is currently restocking.");
                Send_store_leave(p_ptr);
                return;
            }
        }
    }

    /* Player owned stores */
    if (p_ptr->store_num == STORE_PLAYER)
    {
        /* Scan the store to find the item */
        o_idx = player_store_o_ptr(Ind, item, o_ptr);
        if (!o_idx) return;

        /* The object is "known" and belongs to a store */
        object_notice_everything(p_ptr, o_ptr, TRUE);
    }

    /* Normal stores */
    else
    {
        /* Get the actual object */
        o_ptr = &store->stock[item];
    }

    /* Check "shown" items */
    if ((price_item(p_ptr, o_ptr, FALSE, 1) == 0) || CGI(o_ptr, 'p', FALSE))
    {
        msg(p_ptr, "Sorry, this item is not for sale.");
        return;
    }

    /* Sanity check the number of items */
    if (amt < 1) amt = 1;
    if (amt > o_ptr->number) amt = o_ptr->number;

    /* Get desired object */
    object_copy_amt(i_ptr, o_ptr, amt);

    /* Ensure we have room */
    if (!inven_carry_okay(p_ptr, i_ptr))
    {
        msg(p_ptr, "You cannot carry that many items.");
        return;
    }

    /* Note that the pack is too heavy */
    if (!weight_okay(p_ptr, i_ptr))
    {
        msg(p_ptr, "You are already too burdened to carry another object.");
        return;
    }

    /* Describe the object (fully) */
    object_desc(p_ptr, o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_STORE);

    /* Extract the price for the entire stack */
    price = price_item(p_ptr, i_ptr, FALSE, i_ptr->number);

    /* Paranoia */
    if (price > p_ptr->au)
    {
        msg(p_ptr, "You cannot afford that purchase.");
        return;
    }

    /* If this is a player shop we have sold a real item */
    if (p_ptr->store_num == STORE_PLAYER)
        sell_player_item(Ind, o_idx, i_ptr);

    /* Spend the money */
    p_ptr->au -= price;

    /* Bypass auto-squelch */
    i_ptr->squelch = SQUELCH_PROTECT;

    /* ID objects on buy */
    object_notice_everything(p_ptr, i_ptr, FALSE);

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER | PN_SQUELCH);

    /* The object no longer belongs to the store */
    i_ptr->ident &= ~(IDENT_AWARE);

    /* Message */
    if ((p_ptr->store_num != STORE_PLAYER) && one_in_(3))
        msgt(p_ptr, MSG_STORE5, ONE_OF(comment_accept));
    msg(p_ptr, "You bought %s for %ld gold.", o_name, (long)price);

    /* Erase the inscription */
    i_ptr->note = 0;

    /* Erase the "ordered" flag */
    i_ptr->ordered = 0;

    /* Set origin */
    set_origin(i_ptr, ((p_ptr->store_num == STORE_PLAYER)? ORIGIN_PLAYER: ORIGIN_STORE),
        p_ptr->depth, 0);

    /* Ensure item owner = store owner */
    if (p_ptr->store_num == STORE_PLAYER)
    {
        const char *name = houses[p_ptr->player_store_num].ownername;
        hash_entry *ptr = lookup_player_by_name(name);

        i_ptr->owner = ((ptr && ht_zero(&ptr->death_turn))? ptr->id: 0);

        /* Hack -- Use o_name for audit :/ */
        strnfmt(o_name, sizeof(o_name), "PS %s-%d | %s-%d $ %ld", p_ptr->name, (int)p_ptr->id, name,
            (int)i_ptr->owner, (long)price);
        audit(o_name);
        audit("PS+gold");
    }

    /* Give it to the player */
    item_new = inven_carry(p_ptr, i_ptr, TRUE);

    /* Message */
    object_desc(p_ptr, o_name, sizeof(o_name), &p_ptr->inventory[item_new],
        ODESC_PREFIX | ODESC_FULL);
    msg(p_ptr, "You have %s (%c).", o_name, index_to_label(item_new));

    /* Player owned stores */
    if (p_ptr->store_num == STORE_PLAYER)
    {
        handle_stuff(p_ptr);
        display_store(Ind);
        store_prt_gold(Ind);
        return;
    }

    /* Hack - Reduce the number of charges in the original stack */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
        o_ptr->pval[DEFAULT_PVAL] -= i_ptr->pval[DEFAULT_PVAL];

    /* Handle stuff */
    handle_stuff(p_ptr);

    /* Remove the bought objects from the store */
    store_item_increase(store, item, 0 - amt);
    store_item_optimize(store, item);

    /* Store is empty */
    if (store->stock_num == 0)
    {
        int i;

        /* Shuffle */
        if (one_in_(STORE_SHUFFLE))
        {
            /* Message */
            msg(p_ptr, "The shopkeeper retires.");

            /* Shuffle the store */
            store_shuffle(store, TRUE);
        }

        /* Maintain */
        else
        {
            /* Message */
            msg(p_ptr, "The shopkeeper brings out some new stock.");
        }

        /* New inventory */
        for (i = 0; i < 10; i++)
        {
            /* Maintain the store */
            store_maint(store, TRUE);
        }
    }

    /* Resend the basic store info */
    display_store(Ind);
    store_prt_gold(Ind);
}


/*
 * Sell an item to the store
 */
void store_sell(int Ind, int item, int amt)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;
    char o_name[NORMAL_WID];
    s32b price;

    /* Paranoia */
    if (item < 0)
    {
        Send_store_sell(Ind, -1, FALSE);
        return;
    }

    /* No store */
    if (!in_store(p_ptr))
    {
        Send_store_sell(Ind, -1, FALSE);
        return;
    }

    /* You can't sell 0 of something. */
    if (amt <= 0)
    {
        Send_store_sell(Ind, -1, FALSE);
        return;
    }

    /* Get the item */
    o_ptr = &p_ptr->inventory[item];

    /* Check for validity of sale */
    if (!store_will_buy(p_ptr, p_ptr->store_num, o_ptr))
    {
        msg(p_ptr, "I don't want that!");
        Send_store_sell(Ind, -1, FALSE);
        return;
    }

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, 's', FALSE))
    __trap(p_ptr, CGI(o_ptr, 'd', FALSE))

    /* Cannot remove cursed items */
    if ((item >= INVEN_WIELD) && cursed_p(o_ptr->flags))
    {
        /* Oops */
        msg(p_ptr, "Hmmm, it seems to be cursed.");

        /* Nope */
        Send_store_sell(Ind, -1, FALSE);
        return;
    }

    /* Work out how many the player can sell */
    if (amt > o_ptr->number) amt = o_ptr->number;

    /* Get a copy of the object representing the number being sold */
    object_copy_amt(i_ptr, o_ptr, amt);

    /* Get a full description */
    object_desc(p_ptr, o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Remove any inscription for stores */
    i_ptr->note = 0;

    /* Is there room in the store (or the home?) */
    if (!store_check_num(&stores[p_ptr->store_num], i_ptr))
    {
        msg(p_ptr, "I have not the room in my store to keep it.");
        Send_store_sell(Ind, -1, FALSE);
        return;
    }

    /* Extract the value of the items */
    price = price_item(p_ptr, i_ptr, TRUE, i_ptr->number);

    /* Tell the client about the price */
    Send_store_sell(Ind, price, FALSE);

    /* Save the info for the confirmation */
    p_ptr->current_selling = item;
    p_ptr->current_sell_amt = amt;
    p_ptr->current_sell_price = price;

    /* Wait for confirmation before actually selling */
}


/*
 * Sell an item to the store (part 2)
 */
void store_confirm(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int item, amt, price, value, guess;
    object_type *o_ptr;
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;
    char o_name[NORMAL_WID];

    /* Abort if we shouldn't be getting called */
    if (p_ptr->current_selling == -1) return;

    /* Get the inventory item */
    item = p_ptr->current_selling;
    o_ptr = &p_ptr->inventory[item];

    /* Get a copy of the object representing the number being sold */
    amt = p_ptr->current_sell_amt;
    object_copy_amt(i_ptr, o_ptr, amt);

    /* Extract the value of the items */
    price = p_ptr->current_sell_price;

    /* Trash the saved variables */
    p_ptr->current_selling = -1;
    p_ptr->current_sell_amt = -1;
    p_ptr->current_sell_price = -1;

    /* Get some money */
    p_ptr->au += price;

    /* Mark artifact as sold */
    set_artifact_info(p_ptr, i_ptr, ARTS_SOLD);

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw */
    p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

    /* Get the "apparent" value */
    guess = object_value(p_ptr, i_ptr, i_ptr->number);

    /* Identify original object */
    object_notice_everything(p_ptr, o_ptr, FALSE);

    /* Take a new copy of the now known-about object. */
    object_distribute_amt(i_ptr, o_ptr, amt);

    /* The item belongs to the store now */
    i_ptr->ident |= IDENT_AWARE;

    /* Get the "actual" value */
    value = object_value(p_ptr, i_ptr, i_ptr->number);

    /* Get the description all over again */
    object_desc(p_ptr, o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Describe the result (in message buffer) */
    msg(p_ptr, "You sold %s for %ld gold.", o_name, (long)price);

    /* Analyze the prices (and comment verbally) */
    purchase_analyze(Ind, price, value, guess);

    /* Take the object from the player */
    inven_item_increase(p_ptr, item, 0 - amt);
    inven_item_optimize(p_ptr, item);

    /* Handle stuff */
    handle_stuff(p_ptr);

    /* Artifacts "disappear" when sold */
    if (i_ptr->artifact)
    {
        /* Preserve any artifact */
        preserve_artifact_aux(i_ptr);

        return;
    }

    /* The store gets that (known) item */
    store_carry(&stores[p_ptr->store_num], i_ptr);

    /* Resend the basic store info */
    display_store(Ind);
    store_prt_gold(Ind);
}


/*
 * Examine an item in a store
 */
void store_examine(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    struct store *store = &stores[p_ptr->store_num];
    object_type tmp_obj;
    object_type *o_ptr = &tmp_obj;
    char o_name[NORMAL_WID];

    /* Player owned stores */
    if (p_ptr->store_num == STORE_PLAYER)
    {
        /* Scan the store to find the item */
        if (!player_store_o_ptr(Ind, item, o_ptr)) return;

        /* The object is "known" and belongs to a store */
        object_notice_everything(p_ptr, o_ptr, TRUE);
    }

    /* Normal stores */
    else
    {
        /* Get the actual item */
        o_ptr = &store->stock[item];
    }

    /* Description (bypass the "aware" flag) */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_STORE);

    /* Let the player scroll through this info */
    p_ptr->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p_ptr);

    /* Dump info into player */
    object_info(p_ptr, o_ptr, (p_ptr->store_num == STORE_PLAYER)? OINFO_NONE: OINFO_FULL);

    /* Restore height and width of current dungeon level */
    text_out_done(p_ptr);

    /* Notify player */
    notify_player(Ind, o_name, NTERM_WIN_OBJECT, FALSE);

    /* Handle stuff */
    handle_stuff(p_ptr);
}


/*
 * Order an item
 */
void store_order(int Ind, const char *buf)
{
    player_type *p_ptr = player_get(Ind);
    struct store *store = &stores[p_ptr->store_num];
    int i, idx = -1;
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    char* str;

    /* Paranoia */
    if (p_ptr->store_num != STORE_XBM)
    {
        msg(p_ptr, "You cannot order from this store.");
        return;
    }

    /* Check for space */
    for (i = 0; i < STORE_MIN_KEEP; i++)
    {
        if (STRZERO(store_orders[i]))
        {
            idx = i;
            break;
        }
    }
    if (i == STORE_MIN_KEEP)
    {
        msg(p_ptr, "Sorry, no more orders can be accepted at this time.");
        return;
    }

    /* Lowercase our search string */
    for (str = (char*)buf; *str; str++) *str = tolower((unsigned char)*str);

    /* Check if such item is already in stock */
    for (i = 0; i < store->stock_num; i++)
    {
        o_ptr = &store->stock[i];

        /* Describe the object and lowercase the result */
        object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);
        for (str = (char*)o_name; *str; str++) *str = tolower((unsigned char)*str);

        /* Check loosely */
        if (strstr(o_name, buf))
        {
            /* Flag the item as "ordered" */
            o_ptr->ordered = 1 + idx;

            /* Accept the order */
            msg(p_ptr, "Order accepted.");
            my_strcpy(store_orders[idx], "{ordered}", sizeof(store_orders[0]));

            return;
        }
    }

    /* Not in stock: place an order */
    msg(p_ptr, "Order accepted.");
    my_strcpy(store_orders[idx], buf, sizeof(store_orders[0]));
}


/*
 * Enter a store, and interact with it.
 *
 * pstore is -1 for normal stores or house index for player owned store
 */
void do_cmd_store(int Ind, int pstore)
{
    player_type *p_ptr = player_get(Ind);
    int which, i;
    struct store *store;

    /* Normal store */
    if (pstore < 0)
    {
        /* Verify a store */
        if (!cave_isshop(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px))
        {
            msg(p_ptr, "You see no store here.");
            return;
        }

        /* Extract the store code */
        which = (cave_get(p_ptr->depth)->feat[p_ptr->py][p_ptr->px] - FEAT_SHOP_HEAD);

        /* Hack -- Ignore the tavern */
        if (which == STORE_TAVERN) return;

        /* Check if we can enter the store */
        if (OPT_P(p_ptr, birth_no_stores))
        {
            msg(p_ptr, "The doors are locked.");
            return;
        }

        /* Store is closed if someone is already in the shop */
        for (i = 1; i <= NumPlayers; i++)
        {
            player_type *q_ptr;
            int which_q_ptr;

            if (Ind == i) continue;

            /* Get this player */
            q_ptr = player_get(i);

            /* Verify a store */
            if (!cave_isshop(cave_get(q_ptr->depth), q_ptr->py, q_ptr->px)) continue;

            /* Extract the store code */
            which_q_ptr = (cave_get(q_ptr->depth)->feat[q_ptr->py][q_ptr->px] - FEAT_SHOP_HEAD);

            /* Hack -- Ignore the tavern */
            if (which_q_ptr == STORE_TAVERN) continue;

            /* Store is closed if someone is already in the shop */
            if (which_q_ptr == which)
            {
                msg(p_ptr, "The doors are locked.");
                return;
            }
        }

        /* Save the store number */
        p_ptr->store_num = which;

        /* Save the max level of this customer */
        store = &stores[p_ptr->store_num];
        store->max_depth = p_ptr->max_depth;

        /* Redraw (add selling prices) */
        p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
    }

    /* Player owned store */
    else
    {
        /* Check if we can enter the store */
        if (OPT_P(p_ptr, birth_no_stores))
        {
            msg(p_ptr, "The doors are locked.");
            return;
        }

        /* Store is closed if someone is restocking (anti-exploit) */
        for (i = 1; i <= NumPlayers; i++)
        {
            if ((Ind != i) && house_inside(player_get(i), pstore))
            {
                msg(p_ptr, "The doors are locked.");
                return;
            }
        }

        p_ptr->store_num = STORE_PLAYER;
        p_ptr->player_store_num = pstore;
    }

    /* Display the store */
    display_store(Ind);
}


bool check_store_drop_color(struct player *p, object_type *o_ptr, byte color)
{
    /* Check color */
    switch (color)
    {
        case PLAYER_STORE_GENERAL: return store_will_buy(p, STORE_GENERAL, o_ptr);
        case PLAYER_STORE_ARMOURY: return store_will_buy(p, STORE_ARMOR, o_ptr);
        case PLAYER_STORE_SMITH: return store_will_buy(p, STORE_WEAPON, o_ptr);
        case PLAYER_STORE_TEMPLE: return store_will_buy(p, STORE_TEMPLE, o_ptr);
        case PLAYER_STORE_ALCHEMIST: return store_will_buy(p, STORE_ALCHEMY, o_ptr);
        case PLAYER_STORE_MAGIC: return store_will_buy(p, STORE_MAGIC, o_ptr);
        case PLAYER_STORE_LIBRARY: return store_will_buy(p, STORE_LIBRARY, o_ptr);
        case PLAYER_STORE_BM: return store_will_buy(p, STORE_B_MARKET, o_ptr);
        case PLAYER_STORE_XBM: return store_will_buy(p, STORE_XBM, o_ptr);
        case PLAYER_STORE_TAVERN: return TRUE;
    }

    /* Paranoia */
    return FALSE;
}


bool check_store_drop(struct player *p, object_type *o_ptr)
{
    int i;
    char store_name[NORMAL_WID];

    /* Check houses */
    for (i = 0; i < num_houses; i++)
    {
        /* Are we inside this house? */
        if (house_inside(p, i))
        {
            /* If we don't own it, we can't drop anything inside! */
            if (!house_owned_by(p, i)) return FALSE;

            /* Custom store: allow everything */
            if (get_player_store_name(i, store_name, sizeof(store_name)))
                return TRUE;

            /* Check this house */
            return check_store_drop_color(p, o_ptr, houses[i].color);
        }
    }

    /* Not in a house */
    return TRUE;
}


/*
 * Determine the price of an item for direct sale
 */
s32b player_price_item(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);
    double price, maxprice, askprice;

    /* Is this item for sale? */
    if (!o_ptr->note) return -1;
    if (!set_askprice(o_ptr)) return -1;

    /* Get the value of all items */
    price = (double)object_value(p_ptr, o_ptr, o_ptr->number);

    /* Worthless items */
    if (price <= 0) return (0L);

    /* Get the desired value of all items */
    askprice = (double)o_ptr->askprice * o_ptr->number;

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


static object_type *store_get_order_item(int order)
{
    struct store *store = &stores[STORE_XBM];
    int i;
    object_type *o_ptr;

    /* Iterate over stock items */
    for (i = 0; i < store->stock_num; i++)
    {
        o_ptr = &store->stock[i];

        /* Cancel the order */
        if (o_ptr->ordered == 1 + order) return o_ptr;
    }

    return NULL;
}


void store_cancel_order(int order)
{
    object_type *o_ptr = store_get_order_item(order);

    /* Cancel the order */
    if (o_ptr) o_ptr->ordered = 0;
}


void store_get_order(int order, char *desc, int len)
{
    object_type *o_ptr = store_get_order_item(order);

    /* Describe the object */
    if (o_ptr) object_desc(NULL, desc, len, o_ptr, ODESC_PREFIX | ODESC_FULL);
}
