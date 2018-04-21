/* File: store.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Store commands */

#include "angband.h"

#include <assert.h>

#define MIN_STOCK 12

static int cur_store_num = 0;
static int store_top = 0;
static int store_bottom = 0;
static int xtra_stock = 0;
static store_type *st_ptr = NULL;
static owner_type *ot_ptr = NULL;
static s16b old_town_num = 0;
static s16b inner_town_num = 0;
bool store_hack = FALSE;
static int cur_store_feat;

static void _buyout(void);
static int _cull(store_type *st_ptr, bool all);
static void _restock(store_type *st_ptr, bool all);

int store_calc_price_factor(int greed)
{
    int factor;

    /* Compute adjustment based on race, wisdom, fame and shopkeeper greed.
       Effects are now multiplicative. Also, a shopkeeper that doubles the
       price on sale pays half when purchasing. A shopkeeper that triples
       on sale pays a third when purchasing. Etc. */
    factor = get_race()->shop_adjust;
    if (factor == 0)
        factor = 110;

    factor = (factor * adj_gold[p_ptr->stat_ind[A_CHR]] + 50) / 100;
    factor = (factor * (135 - MIN(200, p_ptr->fame)/4) + 50) / 100;
    factor = (factor * greed + 50) / 100;

    if (ot_ptr && prace_is_(ot_ptr->owner_race))
        factor = factor * 90 / 100;

    return factor;
}

int store_calc_sell_price(int price, int factor)
{
    if (factor < 100)
        factor = 100;

    if (price > 1000*1000)
        price = (price / 100) * factor;
    else
        price = (price * factor + 50) / 100;

    if (price > 1000)
        price = big_num_round(price, 3);

    return price;
}

int store_calc_purchase_price(int price, int factor)
{
    if (factor < 105)
        factor = 105;

    if (price > 1000*1000)
        price = (price / factor) * 100;
    else
        price = (price * 100) / factor;

    if (price > 1000)
        price = big_num_round(price, 3);

    return price;
}

/*
 * Determine the price of an item (qty one) in a store.
 *
 * This function takes into account the player's charisma, and the
 * shop-keepers friendliness, and the shop-keeper's base greed, but
 * never lets a shop-keeper lose money in a transaction.
 *
 */
static s32b price_item(object_type *o_ptr, int greed, bool flip)
{
    int     factor = store_calc_price_factor(greed);
    s32b    price;

    /* Get the value of one of the items */
    price = obj_value(o_ptr);

    /* Worthless items */
    if (price <= 0) return 0;


    /* Shop is buying */
    if (flip)
    {
        if (cur_store_num == STORE_BLACK && p_ptr->realm1 != REALM_BURGLARY && !mut_present(MUT_BLACK_MARKETEER))
            price = price / 2;

        if (cur_store_num == STORE_JEWELER)
            price = price / 2;

        if (cur_store_num == STORE_BLACK)
            price = price * (625 - virtue_current(VIRTUE_JUSTICE)) / 625;

        price = store_calc_purchase_price(price, factor);
    }
    /* Shop is selling */
    else
    {
        if (cur_store_num == STORE_BLACK && p_ptr->realm1 != REALM_BURGLARY && !mut_present(MUT_BLACK_MARKETEER))
            price = price * 2;

        if (cur_store_num == STORE_JEWELER)
            price = price * 2;

        if (cur_store_num == STORE_BLACK)
            price = price * (625 + virtue_current(VIRTUE_JUSTICE)) / 625;

        price = store_calc_sell_price(price, factor);
    }

    /* Note -- Never become "free" */
    if (price <= 0) return (1);

    /* Return the price */
    return price;
}


/*
 * Certain "cheap" objects should be created in "piles"
 * Some objects can be sold at a "discount" (in small piles)
 */
void discount(object_type *o_ptr)
{
    int discount = 0;
    s32b cost = obj_value(o_ptr);

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


    if (object_is_artifact(o_ptr))
        discount = 0;

    o_ptr->discount = discount;

}

/* Note: make_object() now uses this for dungeon drops!!!!
   Note: This is now controlled by the M:P:XdY line in k_info.txt
*/
void mass_produce(object_type *o_ptr)
{
    int          size = 1;
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    if (object_is_artifact(o_ptr)) return;
    if (object_is_ego(o_ptr) && !object_is_ammo(o_ptr)) return;
    if (!k_ptr->stack_chance) return;
    if (randint1(100) > k_ptr->stack_chance) return;

    assert(k_ptr->stack_dice);
    assert(k_ptr->stack_sides);
    size = damroll(k_ptr->stack_dice, k_ptr->stack_sides);

    if (size <= 1) return;

    /* Save the total pile size (Call discount() before mass_produce() for shops)*/
    o_ptr->number = size;
    if (!store_hack)
    {
        k_ptr->counts.generated += size - 1;
        if (o_ptr->name2)
            e_info[o_ptr->name2].counts.generated += size - 1;
    }

    if (!dun_level)
        o_ptr->number -= (size * o_ptr->discount / 100);
}

/*
 * Determine if a store item can "absorb" another item
 *
 * See "object_similar()" for the same function for the "player"
 */
static bool store_object_similar(object_type *o_ptr, object_type *j_ptr)
{
    int i;

    /* Hack -- Identical items cannot be stacked */
    if (o_ptr == j_ptr) return (0);

    /* Different objects cannot be stacked */
    if (o_ptr->k_idx != j_ptr->k_idx) return (0);

    switch (o_ptr->tval)
    {
    case TV_WAND: case TV_ROD: case TV_STAFF:
        return 0;
    }

    if (o_ptr->pval != j_ptr->pval) return (0);

    /* Require many identical values */
    if (o_ptr->to_h != j_ptr->to_h) return (0);
    if (o_ptr->to_d != j_ptr->to_d) return (0);
    if (o_ptr->to_a != j_ptr->to_a) return (0);

    /* Require identical "ego-item" names */
    if (o_ptr->name2 != j_ptr->name2) return (0);

    /* Artifacts don't stack! */
    if (object_is_artifact(o_ptr) || object_is_artifact(j_ptr)) return (0);

    /* Hack -- Identical art_flags! */
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        if (o_ptr->flags[i] != j_ptr->flags[i]) return (0);

    /* Hack -- Never stack "powerful" items */
    if (o_ptr->xtra1 || j_ptr->xtra1) return (0);

    /* Hack -- Never stack recharging items */
    if (o_ptr->timeout || j_ptr->timeout) return (0);

    /* Require many identical values */
    if (o_ptr->ac != j_ptr->ac)   return (0);
    if (o_ptr->dd != j_ptr->dd)   return (0);
    if (o_ptr->ds != j_ptr->ds)   return (0);

    /* Hack -- Never stack chests */
    if (o_ptr->tval == TV_CHEST) return (0);
    if (o_ptr->tval == TV_STATUE) return (0);
    if (o_ptr->tval == TV_CAPTURE) return (0);

    /* Require matching discounts */
    if (o_ptr->discount != j_ptr->discount) return (0);

    /* Require matching activations */
    if ( o_ptr->activation.type != j_ptr->activation.type
      || o_ptr->activation.power != j_ptr->activation.power
      || o_ptr->activation.difficulty != j_ptr->activation.difficulty
      || o_ptr->activation.cost != j_ptr->activation.cost
      || o_ptr->activation.extra != j_ptr->activation.extra )
    {
        return 0;
    }

    /* They match, so they must be similar */
    return (TRUE);
}


/*
 * Allow a store item to absorb another item
 */
static void store_object_absorb(object_type *o_ptr, object_type *j_ptr)
{
    int max_num = 99;
    int total = o_ptr->number + j_ptr->number;

    /* Combine quantity, lose excess items */
    o_ptr->number = (total > max_num) ? max_num : total;
}


/*
 * Check to see if the shop will be carrying too many objects    -RAK-
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.   Before, one could "nuke" potions this way.
 *
 * Return value is now int:
 *  0 : No space
 * -1 : Can be combined to existing slot.
 *  1 : Cannot be combined but there are empty spaces.
 */
static int store_check_num(object_type *o_ptr)
{
    int        i;
    object_type *j_ptr;

    /* The "home" acts like the player */
    if ((cur_store_num == STORE_HOME) || (cur_store_num == STORE_MUSEUM))
    {
        bool old_stack_force_notes = stack_force_notes;
        bool old_stack_force_costs = stack_force_costs;

        if (cur_store_num != STORE_HOME)
        {
            stack_force_notes = FALSE;
            stack_force_costs = FALSE;
        }

        /* Check all the items */
        for (i = 0; i < st_ptr->stock_num; i++)
        {
            /* Get the existing item */
            j_ptr = &st_ptr->stock[i];

            /* Can the new object be combined with the old one? */
            if (object_similar(j_ptr, o_ptr))
            {
                if (cur_store_num != STORE_HOME)
                {
                    stack_force_notes = old_stack_force_notes;
                    stack_force_costs = old_stack_force_costs;
                }

                return -1;
            }
        }

        if (cur_store_num != STORE_HOME)
        {
            stack_force_notes = old_stack_force_notes;
            stack_force_costs = old_stack_force_costs;
        }
    }

    /* Normal stores do special stuff */
    else
    {
        /* Check all the items */
        for (i = 0; i < st_ptr->stock_num; i++)
        {
            /* Get the existing item */
            j_ptr = &st_ptr->stock[i];

            /* Can the new object be combined with the old one? */
            if (store_object_similar(j_ptr, o_ptr)) return -1;
        }
    }

    /* Free space is always usable */
    if ((cur_store_num == STORE_HOME) && ( powerup_home == FALSE )) {
        if (st_ptr->stock_num < ((st_ptr->stock_size) / 10)) {
            return 1;
        }
    }
    else{
        if (st_ptr->stock_num < st_ptr->stock_size) {
            return 1;
        }
    }

    /* But there was no room at the inn... */
    return 0;
}


static bool is_blessed(object_type *o_ptr)
{
    u32b flgs[OF_ARRAY_SIZE];
    obj_flags(o_ptr, flgs);
    if (have_flag(flgs, OF_BLESSED)) return (TRUE);
    else return (FALSE);
}



/*
 * Determine if the current store will purchase the given item
 *
 * Note that a shop-keeper must refuse to buy "worthless" items
 */
static bool store_will_buy(object_type *o_ptr)
{
    /* Hack -- The Home is simple */
    if ((cur_store_num == STORE_HOME) || (cur_store_num == STORE_MUSEUM)) return (TRUE);

    /* Switch on the store */
    switch (cur_store_num)
    {
        /* General Store */
        case STORE_GENERAL:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_POTION:
                    if (o_ptr->sval != SV_POTION_WATER) return FALSE;

                case TV_WHISTLE:
                case TV_FOOD:
                case TV_LITE:
                case TV_FLASK:
                case TV_SPIKE:
                case TV_SHOT:
                case TV_ARROW:
                case TV_BOLT:
                case TV_DIGGING:
                case TV_CLOAK:
                case TV_BOTTLE: /* 'Green', recycling Angband */
                case TV_FIGURINE:
                case TV_STATUE:
                case TV_CAPTURE:
                case TV_CARD:
                break;
                default:
                return (FALSE);
            }
            break;
        }

        /* Armoury */
        case STORE_ARMOURY:
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
                case TV_BOW:
                case TV_DIGGING:
                case TV_POLEARM:
                case TV_SWORD:
                case TV_HISSATSU_BOOK:
                case TV_RAGE_BOOK:
                break;
                case TV_HAFTED:
                {
                    if(o_ptr->sval == SV_WIZSTAFF) return (FALSE);
                }
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
                case TV_LIFE_BOOK:
                case TV_CRUSADE_BOOK:
                case TV_SCROLL:
                case TV_POTION:
                case TV_HAFTED:
                {
                    break;
                }
                case TV_FIGURINE:
                case TV_STATUE:
                {
                    monster_race *r_ptr = &r_info[o_ptr->pval];

                    /* Decline evil */
                    if (!(r_ptr->flags3 & RF3_EVIL))
                    {
                        /* Accept good */
                        if (r_ptr->flags3 & RF3_GOOD) break;

                        /* Accept animals */
                        if (r_ptr->flags3 & RF3_ANIMAL) break;

                        /* Accept mimics */
                        if (my_strchr("?!", r_ptr->d_char)) break;
                    }
                }
                case TV_POLEARM:
                case TV_SWORD:
                {
                    if (is_blessed(o_ptr)) break;
                }
                default:
                return (FALSE);
            }
            break;
        }

        /* Alchemist */
        case STORE_ALCHEMIST:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_SCROLL:
                case TV_POTION:
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
                case TV_SORCERY_BOOK:
                case TV_NATURE_BOOK:
                case TV_CHAOS_BOOK:
                case TV_ARMAGEDDON_BOOK:
                case TV_DEATH_BOOK:
                case TV_TRUMP_BOOK:
                case TV_ARCANE_BOOK:
                case TV_CRAFT_BOOK:
                case TV_DAEMON_BOOK:
                case TV_MUSIC_BOOK:
                case TV_HEX_BOOK:
                case TV_AMULET:
                case TV_RING:
                case TV_STAFF:
                case TV_WAND:
                case TV_ROD:
                case TV_SCROLL:
                case TV_POTION:
                case TV_FIGURINE:
                break;
                case TV_HAFTED:
                {
                    if(o_ptr->sval == SV_WIZSTAFF) break;
                    else return (FALSE);
                }
                default:
                return (FALSE);
            }
            break;
        }
        /* Bookstore Shop */
        case STORE_BOOK:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_SORCERY_BOOK:
                case TV_NATURE_BOOK:
                case TV_CHAOS_BOOK:
                case TV_DEATH_BOOK:
                case TV_LIFE_BOOK:
                case TV_TRUMP_BOOK:
                case TV_ARCANE_BOOK:
                case TV_CRAFT_BOOK:
                case TV_DAEMON_BOOK:
                case TV_CRUSADE_BOOK:
                case TV_NECROMANCY_BOOK:
                case TV_ARMAGEDDON_BOOK:
                case TV_MUSIC_BOOK:
                case TV_HEX_BOOK:
                    break;
                default:
                    return (FALSE);
            }
            break;
        }
    }

    /* XXX XXX XXX Ignore "worthless" items */
    if (obj_value(o_ptr) <= 0) return (FALSE);

    /* Assume okay */
    return (TRUE);
}


/*
 * Combine and reorder items in the home
 */
bool combine_and_reorder_home(int store_num)
{
    int         i, j, k;
    s32b        o_value;
    object_type forge, *o_ptr, *j_ptr;
    bool        flag = FALSE, combined;
    store_type  *old_st_ptr = st_ptr;
    bool        old_stack_force_notes = stack_force_notes;
    bool        old_stack_force_costs = stack_force_costs;

    st_ptr = &town[1].store[store_num];
    if (store_num != STORE_HOME)
    {
        stack_force_notes = FALSE;
        stack_force_costs = FALSE;
    }

    do
    {
        combined = FALSE;

        /* Combine the items in the home (backwards) */
        for (i = st_ptr->stock_num - 1; i > 0; i--)
        {
            /* Get the item */
            o_ptr = &st_ptr->stock[i];

            /* Skip empty items */
            if (!o_ptr->k_idx) continue;

            /* Scan the items above that item */
            for (j = 0; j < i; j++)
            {
                int max_num;

                /* Get the item */
                j_ptr = &st_ptr->stock[j];

                /* Skip empty items */
                if (!j_ptr->k_idx) continue;

                /*
                 * Get maximum number of the stack if these
                 * are similar, get zero otherwise.
                 */
                max_num = object_similar_part(j_ptr, o_ptr);

                /* Can we (partialy) drop "o_ptr" onto "j_ptr"? */
                if (max_num && j_ptr->number < max_num)
                {
                    if (o_ptr->number + j_ptr->number <= max_num)
                    {
                        /* Add together the item counts */
                        object_absorb(j_ptr, o_ptr);

                        /* One object is gone */
                        st_ptr->stock_num--;

                        /* Slide everything down */
                        for (k = i; k < st_ptr->stock_num; k++)
                        {
                            /* Structure copy */
                            st_ptr->stock[k] = st_ptr->stock[k + 1];
                        }

                        /* Erase the "final" slot */
                        object_wipe(&st_ptr->stock[k]);
                    }
                    else
                    {
                        int remain = j_ptr->number + o_ptr->number - max_num;

                        /* Add together the item counts */
                        object_absorb(j_ptr, o_ptr);

                        o_ptr->number = remain;
                    }

                    /* Take note */
                    combined = TRUE;

                    /* Done */
                    break;
                }
            }
        }

        flag |= combined;
    }
    while (combined);

    /* Re-order the items in the home (forwards) */
    for (i = 0; i < st_ptr->stock_num; i++)
    {
        /* Get the item */
        o_ptr = &st_ptr->stock[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        /* Get the "value" of the item */
        o_value = obj_value(o_ptr);

        /* Scan every occupied slot */
        for (j = 0; j < st_ptr->stock_num; j++)
        {
            if (object_sort_comp(o_ptr, o_value, &st_ptr->stock[j])) break;
        }

        /* Never move down */
        if (j >= i) continue;

        /* Take note */
        flag = TRUE;

        /* Get local object */
        j_ptr = &forge;

        /* Save a copy of the moving item */
        object_copy(j_ptr, &st_ptr->stock[i]);

        /* Slide the objects */
        for (k = i; k > j; k--)
        {
            /* Slide the item */
            object_copy(&st_ptr->stock[k], &st_ptr->stock[k - 1]);
        }

        /* Insert the moving item */
        object_copy(&st_ptr->stock[j], j_ptr);
    }

    st_ptr = old_st_ptr;
    if (store_num != STORE_HOME)
    {
        stack_force_notes = old_stack_force_notes;
        stack_force_costs = old_stack_force_costs;
    }

    return flag;
}


/*
 * Add the item "o_ptr" to the inventory of the "Home"
 *
 * In all cases, return the slot (or -1) where the object was placed
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" bacoming
 * known, the player may have to pick stuff up and drop it again.
 */
static int home_carry(object_type *o_ptr)
{
    int                 slot;
    s32b               value;
    int     i;
    object_type *j_ptr;
    bool old_stack_force_notes = stack_force_notes;
    bool old_stack_force_costs = stack_force_costs;

    if (cur_store_num != STORE_HOME)
    {
        stack_force_notes = FALSE;
        stack_force_costs = FALSE;
    }

    /* Check each existing item (try to combine) */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get the existing item */
        j_ptr = &st_ptr->stock[slot];

        /* The home acts just like the player */
        if (object_similar(j_ptr, o_ptr))
        {
            /* Save the new number of items */
            object_absorb(j_ptr, o_ptr);

            if (cur_store_num != STORE_HOME)
            {
                stack_force_notes = old_stack_force_notes;
                stack_force_costs = old_stack_force_costs;
            }

            /* All done */
            return (slot);
        }
    }

    if (cur_store_num != STORE_HOME)
    {
        stack_force_notes = old_stack_force_notes;
        stack_force_costs = old_stack_force_costs;
    }

    /* No space? */
    if ((cur_store_num != STORE_HOME) || (powerup_home == TRUE)) {
        if (st_ptr->stock_num >= st_ptr->stock_size) {
            return (-1);
        }
    }
    else{
        if (st_ptr->stock_num >= ((st_ptr->stock_size) / 10)) {
            return (-1);
        }
    }


    /* Determine the "value" of the item */
    value = obj_value(o_ptr);

    /* Check existing slots to see if we must "slide" */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        if (object_sort_comp(o_ptr, value, &st_ptr->stock[slot])) break;
    }

    /* Slide the others up */
    for (i = st_ptr->stock_num; i > slot; i--)
    {
        st_ptr->stock[i] = st_ptr->stock[i-1];
    }

    /* More stuff now */
    st_ptr->stock_num++;

    /* Insert the new item */
    st_ptr->stock[slot] = *o_ptr;

    if (cur_store_num == STORE_MUSEUM)
        virtue_add(VIRTUE_SACRIFICE, 1);

    (void)combine_and_reorder_home(cur_store_num);

    /* Return the location */
    return (slot);
}


/*
 * Add the item "o_ptr" to a real stores inventory.
 *
 * If the item is "worthless", it is thrown away (except in the home).
 *
 * If the item cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.   Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an item to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or -1) where the object was placed
 */
static int store_carry(object_type *o_ptr)
{
    int     i, slot;
    s32b    value, j_value;
    object_type *j_ptr;


    /* Evaluate the object */
    value = obj_value(o_ptr);

    /* Cursed/Worthless items "disappear" when sold */
    if (value <= 0) return (-1);

    /* All store items are fully *identified* */
    o_ptr->ident |= IDENT_STORE;

    /* Erase the inscription */
    o_ptr->inscription = 0;

    /* Erase the "feeling" */
    o_ptr->feeling = FEEL_NONE;

    /* Check each existing item (try to combine) */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get the existing item */
        j_ptr = &st_ptr->stock[slot];

        /* Can the existing items be incremented? */
        if (store_object_similar(j_ptr, o_ptr))
        {
            /* Hack -- extra items disappear */
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
        /* Get that item */
        j_ptr = &st_ptr->stock[slot];

        /* Objects sort by decreasing type */
        if (o_ptr->tval > j_ptr->tval) break;
        if (o_ptr->tval < j_ptr->tval) continue;

        /* Objects sort by increasing sval */
        if (o_ptr->sval < j_ptr->sval) break;
        if (o_ptr->sval > j_ptr->sval) continue;

        /* Devices sort by increasing level of effect */
        if (o_ptr->tval == TV_WAND || o_ptr->tval == TV_ROD || o_ptr->tval == TV_STAFF)
        {
            if (o_ptr->activation.difficulty < j_ptr->activation.difficulty) break;
            if (o_ptr->activation.difficulty > j_ptr->activation.difficulty) continue;
        }

        /* Evaluate that slot */
        j_value = obj_value(j_ptr);

        /* Objects sort by decreasing value */
        if (value > j_value) break;
        if (value < j_value) continue;
    }

    /* Slide the others up */
    for (i = st_ptr->stock_num; i > slot; i--)
    {
        st_ptr->stock[i] = st_ptr->stock[i-1];
    }

    /* More stuff now */
    st_ptr->stock_num++;

    /* Insert the new item */
    st_ptr->stock[slot] = *o_ptr;

    /* Return the location */
    return (slot);
}


/*
 * Increase, by a given amount, the number of a certain item
 * in a certain store.   This can result in zero items.
 */
static void store_item_increase(int item, int num)
{
    int         cnt;
    object_type *o_ptr;

    /* Get the item */
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
    int         j;
    object_type *o_ptr;

    /* Get the item */
    o_ptr = &st_ptr->stock[item];

    /* Must exist */
    if (!o_ptr->k_idx) return;

    /* Must have no items */
    if (o_ptr->number) return;

    /* One less item */
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
 * Crap is defined as any item that is "available" elsewhere
 * Based on a suggestion by "Lee Vogt" <lvogt@cig.mcel.mot.com>
 */
static bool black_market_crap(object_type *o_ptr)
{
    /* Ego items are never crap */
    if (object_is_ego(o_ptr)) return (FALSE);
    if (o_ptr->marked & OM_RESERVED) return FALSE;

    /* Good items are never crap */
    if (o_ptr->to_a > 0) return (FALSE);
    if (o_ptr->to_h > 0) return (FALSE);
    if (o_ptr->to_d > 0) return (FALSE);

    /* All devices now share a common k_idx! */
    if (o_ptr->tval == TV_WAND || o_ptr->tval == TV_STAFF || o_ptr->tval == TV_ROD) return FALSE;

    if (k_info[o_ptr->k_idx].gen_flags & OFG_TOWN)
        return TRUE;

    /* Assume okay */
    return (FALSE);
}


/*
 * Attempt to delete (some of) a random item from the store
 * Hack -- we attempt to "maintain" piles of items when possible.
 */
static void store_delete(void)
{
    int what, num;

    /* Pick a random slot */
    what = randint0(st_ptr->stock_num);
    if (st_ptr->stock[what].marked & OM_RESERVED) return;

    /* Determine how many items are here */
    num = st_ptr->stock[what].number;

    /* Hack -- sometimes, only destroy half the items */
    if (randint0(100) < 50) num = (num + 1) / 2;

    /* Hack -- sometimes, only destroy a single item */
    if (randint0(100) < 50) num = 1;

    /* Actually destroy (part of) the item */
    store_item_increase(what, -num);
    store_item_optimize(what);
}

/* Store Stocking:
   [1] We use normal object allocation to determine object frequencies.
   [2] k_info.txt uses the TOWN attribute to determine allowable objects (except BM)
   [3] Object quantities (i.e., stacking) is specified by k_info's M:P:XdY line. (Idea from Vanilla).
   [4] Most stores won't auto restock just by resting. Instead, the player needs to
       actually play a bit as well (We use experience as a proxy to test this).
 */
static bool _town_accept_aux(int k_idx)
{
    if (k_info[k_idx].gen_flags & OFG_INSTA_ART)
        return FALSE;

    if (p_ptr->town_num != SECRET_TOWN)
    {
        if (!(k_info[k_idx].gen_flags & OFG_TOWN))
            return FALSE;
    }
    return TRUE;
}

static bool _general_store_accept(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_FLASK:
    case TV_SPIKE:
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
    case TV_CAPTURE:
    case TV_FIGURINE:
    case TV_CLOAK:
    case TV_LITE:
    case TV_FOOD:
    case TV_DIGGING:
        return TRUE;
    }
    return FALSE;
}
static bool _armoury_accept(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_HARD_ARMOR:
    case TV_SOFT_ARMOR:
    case TV_GLOVES:
    case TV_HELM:
    case TV_BOOTS:
    case TV_SHIELD:
        return TRUE;
    }
    return FALSE;
}
static bool _weapon_accept(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_POLEARM:
    case TV_SWORD:
        return TRUE;

    case TV_HISSATSU_BOOK:
    case TV_RAGE_BOOK:
        if (p_ptr->town_num == SECRET_TOWN && !one_in_(20))
        {
            if (!(k_info[k_idx].gen_flags & OFG_TOWN))
                return FALSE;
        }
        return TRUE;
    }
    return FALSE;
}
static bool _weapon_accept_shooter(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;
    switch (k_info[k_idx].tval)
    {
    case TV_BOW:
        return TRUE;
    }
    return FALSE;
}
static bool _weapon_accept_ammo(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;
    switch (k_info[k_idx].tval)
    {
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
        return TRUE;
    }
    return FALSE;
}
static bool _temple_accept(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_LIFE_BOOK:
    case TV_CRUSADE_BOOK:
        if (p_ptr->town_num == SECRET_TOWN && !one_in_(20))
        {
            if (!(k_info[k_idx].gen_flags & OFG_TOWN))
                return FALSE;
        }
        return TRUE;

    case TV_HAFTED:
        return TRUE;

    /* Scrolls and Potions are also stocked by the Alchemist */
    case TV_SCROLL:
        switch (k_info[k_idx].sval)
        {
        case SV_SCROLL_REMOVE_CURSE:
        case SV_SCROLL_BLESSING:
        case SV_SCROLL_HOLY_CHANT:
        case SV_SCROLL_WORD_OF_RECALL:
        case SV_SCROLL_STAR_REMOVE_CURSE:
        case SV_SCROLL_RUNE_OF_PROTECTION:
            return TRUE;
        }
        return FALSE;

    case TV_POTION:
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_RESIST_HEAT:
        case SV_POTION_RESIST_COLD:
        case SV_POTION_RESTORE_EXP:
        case SV_POTION_CURE_CRITICAL:
        case SV_POTION_CURE_SERIOUS:
        case SV_POTION_CURE_LIGHT:
        case SV_POTION_BOLDNESS:
        case SV_POTION_HEROISM:
            return TRUE;
        }
        return FALSE;
    }
    return FALSE;
}

static bool _alchemist_accept(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;

    if (p_ptr->town_num == SECRET_TOWN && !one_in_(20))
    {
        if (!(k_info[k_idx].gen_flags & OFG_TOWN))
            return FALSE;
    }

    switch (k_info[k_idx].tval)
    {
    /* Scrolls and Potions are also stocked by the Temple. */
    case TV_SCROLL:
    case TV_POTION:
        if (!_temple_accept(k_idx))
            return TRUE;
        break;
    }
    return FALSE;
}

static bool _magic_accept(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_WAND:
    case TV_STAFF:
        if (p_ptr->town_num == SECRET_TOWN && !one_in_(10))
        {
            if (!(k_info[k_idx].gen_flags & OFG_TOWN))
                return FALSE;
        }
        return TRUE;

    case TV_FIGURINE:
    case TV_ARCANE_BOOK:
        return TRUE;

    case TV_SORCERY_BOOK:
        if (p_ptr->town_num == SECRET_TOWN && !one_in_(20))
        {
            if (!(k_info[k_idx].gen_flags & OFG_TOWN))
                return FALSE;
        }
        return TRUE;
    }
    return FALSE;
}

static bool _book_accept(int k_idx)
{
    if (!_town_accept_aux(k_idx))
        return FALSE;

    if (p_ptr->town_num == SECRET_TOWN && !one_in_(10))
    {
        if (!(k_info[k_idx].gen_flags & OFG_TOWN))
            return FALSE;
    }

    switch (k_info[k_idx].tval)
    {
    case TV_ARCANE_BOOK:
    case TV_SORCERY_BOOK:
    case TV_NATURE_BOOK:
    case TV_CHAOS_BOOK:
    case TV_DEATH_BOOK:
    case TV_TRUMP_BOOK:
    case TV_CRAFT_BOOK:
    case TV_DAEMON_BOOK:
    case TV_MUSIC_BOOK:
    case TV_HEX_BOOK:
    case TV_NECROMANCY_BOOK:
    case TV_ARMAGEDDON_BOOK:
        return TRUE;
    }
    return FALSE;
}

static bool _jeweler_accept(int k_idx)
{
    if (k_info[k_idx].gen_flags & OFG_INSTA_ART)
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_RING:
    case TV_AMULET:
        return TRUE;
    }
    return FALSE;
}

static bool _get_store_obj(object_type *o_ptr)
{
    int level1 = 20; /* Level of get_obj_num ... Second books are L20 */
    int level2 = rand_range(1, STORE_OBJ_LEVEL); /* Level of apply_magic */
    int k_idx = 0;
    int mode = AM_NO_FIXED_ART;

    if (p_ptr->town_num != SECRET_TOWN && cur_store_num != STORE_BLACK)
    {
        mode |=  AM_STOCK_TOWN;
    }

    if (cur_store_num == STORE_BLACK)
        mode |= AM_STOCK_BM;

    switch (cur_store_num)
    {
    case STORE_GENERAL:
        level1 = level2;
        get_obj_num_hook = _general_store_accept;
        break;
    case STORE_ARMOURY:
        get_obj_num_hook = _armoury_accept;
        break;
    case STORE_WEAPON:
        /* Hack: Try to make sure archery is not swamped by melee weapons ... */
        if (one_in_(4))
            get_obj_num_hook = _weapon_accept_shooter;
        else if (one_in_(4))
            get_obj_num_hook = _weapon_accept_ammo;
        else
            get_obj_num_hook = _weapon_accept;
        break;
    case STORE_TEMPLE:
        get_obj_num_hook = _temple_accept;
        break;
    case STORE_ALCHEMIST:
        get_obj_num_hook = _alchemist_accept;
        break;
    case STORE_MAGIC:
        level2 = 15; /* cf the effect tables in devices.c */
        get_obj_num_hook = _magic_accept;
        break;
    case STORE_BOOK:
        get_obj_num_hook = _book_accept;
        break;
    case STORE_BLACK:
        choose_obj_kind(0);
        if (dun_level)
        {
            level1 = dun_level;
            level2 = dun_level;
        }
        else
        {
            level1 = 25 + randint0(25);
            level2 = 25 + randint0(25);
        }
        break;
    case STORE_JEWELER:
        get_obj_num_hook = _jeweler_accept;
        if (dun_level)
        {
            level1 = dun_level;
            level2 = dun_level;
        }
        else
        {
            level1 = 25 + randint0(25);
            level2 = 25 + randint0(25);
        }
        break;
    }

    if (cur_store_num == STORE_BLACK && one_in_(9))
    {
        k_idx = lookup_kind(TV_BURGLARY_BOOK, randint0(2));
    }
    else if (cur_store_num == STORE_TEMPLE)
    {
        if (one_in_(3))
            k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL);
        else if (one_in_(7))
            k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_REMOVE_CURSE);
        else if (one_in_(20))
            k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_STAR_REMOVE_CURSE);
    }
    else if (cur_store_num == STORE_ALCHEMIST)
    {
        if (one_in_(3))
            k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL);
        else if (one_in_(5))
            k_idx = lookup_kind(TV_POTION, SV_POTION_RES_STR + randint0(6));
        else if (one_in_(7))
            k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_IDENTIFY);
        else if (one_in_(10))
            k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_TELEPORT);
        else if (!easy_id && one_in_(20))
            k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_STAR_IDENTIFY);
    }
    else if (cur_store_num == STORE_MAGIC && one_in_(20))
    {
        /* Hack: Early resists are hard to find, and Archviles are so damn nasty!
           BTW, since we are cheating and not using normal ego generation code, we'll
           need to manually add to ego_type.xtra_flags. This will improve the
           player's lore experience should they purchase or examine this item
           of stock. */
        if (one_in_(5))
        {
            object_prep(o_ptr, lookup_kind(TV_AMULET, 0));
            o_ptr->name2 = EGO_JEWELRY_ELEMENTAL;
        }
        else
        {
            object_prep(o_ptr, lookup_kind(TV_RING, 0));
            o_ptr->name2 = EGO_JEWELRY_ELEMENTAL;
        }
        switch (randint1(5))
        {
        case 1: case 2:
            add_flag(o_ptr->flags, OF_RES_COLD);
            add_flag(e_info[EGO_JEWELRY_ELEMENTAL].xtra_flags, OF_RES_COLD);
            break;
        case 3: case 4:
            add_flag(o_ptr->flags, OF_RES_FIRE);
            add_flag(e_info[EGO_JEWELRY_ELEMENTAL].xtra_flags, OF_RES_FIRE);
            break;
        case 5:
            add_flag(o_ptr->flags, OF_RES_ACID);
            add_flag(e_info[EGO_JEWELRY_ELEMENTAL].xtra_flags, OF_RES_ACID);
            break;
        }
        return TRUE;
    }
    else if (cur_store_num == STORE_GENERAL)
    {
        if (one_in_(50))
            k_idx = lookup_kind(TV_CAPTURE, 0);
        else if (one_in_(3))
            k_idx = lookup_kind(TV_FOOD, SV_FOOD_RATION);
        else if (one_in_(3))
            k_idx = lookup_kind(TV_POTION, SV_POTION_WATER);
        else if (one_in_(3))
            k_idx = lookup_kind(TV_FLASK, SV_FLASK_OIL);
        else if (one_in_(3))
            k_idx = lookup_kind(TV_LITE, SV_LITE_LANTERN);
        else if (one_in_(3))
            k_idx = lookup_kind(TV_DIGGING, SV_SHOVEL);
        else if (one_in_(5))
            k_idx = lookup_kind(TV_DIGGING, SV_PICK);
    }

    if (!k_idx)
    {
        if (get_obj_num_hook) get_obj_num_prep();
        k_idx = get_obj_num(level1);
        if (get_obj_num_hook)
        {
            get_obj_num_hook = NULL;
            get_obj_num_prep();
        }
    }
    else
        get_obj_num_hook = NULL;

    object_prep(o_ptr, k_idx);
    apply_magic(o_ptr, level2, mode);
    if (o_ptr->tval == TV_LITE)
    {
        if (o_ptr->sval == SV_LITE_TORCH) o_ptr->xtra4 = FUEL_TORCH / 2;
        if (o_ptr->sval == SV_LITE_LANTERN) o_ptr->xtra4 = FUEL_LAMP / 2;
    }

    return TRUE;
}

static void store_create(void)
{
    int tries;

    object_type forge;

    /* Paranoia -- no room left */
    if (st_ptr->stock_num >= st_ptr->stock_size) return;

    /* Hack -- consider up to four items */
    for (tries = 0; tries < 4; tries++)
    {
        if (!_get_store_obj(&forge)) continue;

        /* The item is "known" */
        obj_identify(&forge);

        /* Mark it storebought */
        forge.ident |= IDENT_STORE;

        /* Mega-Hack -- no chests in stores */
        if (forge.tval == TV_CHEST) continue;

        /* Prune the black market */
        if (cur_store_num == STORE_BLACK)
        {
            /* Hack -- No "crappy" items */
            if (black_market_crap(&forge)) continue;

            /* Hack -- No "cheap" items */
            if (obj_value(&forge) < 10) continue;

            /* No "worthless" items */
            /* if (object_value(q_ptr) <= 0) continue; */
            if (object_is_cursed(&forge)) continue;
        }

        /* Prune normal stores */
        else
        {
            /* No "worthless" items */
            if (obj_value(&forge) <= 0) continue;
            if (object_is_cursed(&forge)) continue;
        }

        /* Mass produce and/or Apply discount (mass_produce needs to know about the discount) */
        discount(&forge);
        mass_produce(&forge);

        /* Attempt to carry the (known) item */
        (void)store_carry(&forge);

        /* Definitely done */
        break;
    }
}

/*
 * Re-displays a single store entry
 */
static void display_entry(int pos)
{
    int         i, cur_col;
    object_type     *o_ptr;
    s32b        x;

    char        o_name[MAX_NLEN];
    char        out_val[160];


    int maxwid = 75;

    /* Get the item */
    o_ptr = &st_ptr->stock[pos];

    /* Get the "offset" */
    i = (pos % store_bottom);

    /* Label it, clear the line --(-- */
    (void)sprintf(out_val, "%c) ", ((i > 25) ? toupper(I2A(i - 26)) : I2A(i)));
    prt(out_val, i+6, 0);

    cur_col = 3;
    if (show_item_graph)
    {
        byte a = object_attr(o_ptr);
        char c = object_char(o_ptr);

#ifdef AMIGA
        if (a & 0x80)
            a |= 0x40;
#endif

        Term_queue_bigchar(cur_col, i + 6, a, c, 0, 0);
        if (use_bigtile) cur_col++;

        cur_col += 2;
    }

    /* Describe an item in the home */
    if ((cur_store_num == STORE_HOME) || (cur_store_num == STORE_MUSEUM))
    {
        maxwid = 75;

        /* Leave room for weights, if necessary -DRS- */
        if (show_weights) maxwid -= 10;

        /* Describe the object */
        object_desc(o_name, o_ptr, 0);
        o_name[maxwid] = '\0';
        c_put_str(tval_to_attr[o_ptr->tval], o_name, i+6, cur_col);

        /* Show weights */
        if (show_weights)
        {
            /* Only show the weight of an individual item */
            int wgt = o_ptr->weight;
            (void)sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
            put_str(out_val, i+6, 68);

        }
        if (cur_store_num == STORE_HOME)
        {
            int score = obj_value(o_ptr);
            if (score)
            {
                (void)sprintf(out_val, "%9d ", score);
                put_str(out_val, i+6, 76);
            }
        }
    }

    /* Describe an item (fully) in a store */
    else
    {
        /* Must leave room for the "price" */
        maxwid = 65;

        /* Leave room for weights, if necessary -DRS- */
        if (show_weights) maxwid -= 7;

        /* Describe the object (fully) */
        object_desc(o_name, o_ptr, 0);
        o_name[maxwid] = '\0';
        c_put_str(tval_to_attr[o_ptr->tval], o_name, i+6, cur_col);

        /* Show weights */
        if (show_weights)
        {
            /* Only show the weight of an individual item */
            int wgt = o_ptr->weight;
            (void)sprintf(out_val, "%3d.%d", wgt / 10, wgt % 10);
            put_str(out_val, i+6, 61);

        }

        /* Display a "fixed" cost */
        if (o_ptr->ident & (IDENT_FIXED))
        {
            byte attr = TERM_WHITE;

            /* Extract the "minimum" price */
            x = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

            if (x > p_ptr->au)
                attr = TERM_L_DARK;

            /* Actually draw the price (not fixed) */
            (void)sprintf(out_val, "%9d F", x);

            c_put_str(attr, out_val, i+6, 68);
        }

        else
        {
            byte attr = TERM_WHITE;

            /* Extract the "minimum" price */
            x = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

            if (x > p_ptr->au)
                attr = TERM_L_DARK;

            /* Actually draw the price (with tax) */
            if (p_ptr->wizard)
            {
                 int score = new_object_cost(o_ptr, COST_REAL);
                 (void)sprintf(out_val, "%9d %9d ", x, score);
            }
            else
                (void)sprintf(out_val, "%9d  ", x);
            c_put_str(attr, out_val, i+6, 68);
        }
    }
}


/*
 * Displays a store's inventory         -RAK-
 * All prices are listed as "per individual object". -BEN-
 */
static void display_inventory(void)
{
    int i, k;

    /*msg_print(NULL);*/

    /* Display the next 12 items */
    for (k = 0; k < store_bottom; k++)
    {
        /* Do not display "dead" items */
        if (store_top + k >= st_ptr->stock_num) break;

        /* Display that line */
        display_entry(store_top + k);
    }

    /* Erase the extra lines and the "more" prompt */
    for (i = k; i < store_bottom + 1; i++) prt("", i + 6, 0);

    /* Assume "no current page" */
    put_str("        ", 5, 20);


    /* Visual reminder of "more items" */
    if (st_ptr->stock_num > store_bottom)
    {
        /* Show "more" reminder (after the last item) */
        prt("-more-", k + 6, 3);


        /* Indicate the "current page" */
        /* Trailing spaces are to display (Page xx) and (Page x) */
        put_str(format("(Page %d)  ", store_top/store_bottom + 1), 5, 20);

    }

    if (cur_store_num == STORE_HOME || cur_store_num == STORE_MUSEUM)
    {
        k = st_ptr->stock_size;

        if (cur_store_num == STORE_HOME && !powerup_home) k /= 10;
        put_str(format("Objects:  %4d/%4d", st_ptr->stock_num, k), 19 + xtra_stock, 30);
    }
}


/*
 * Displays players gold                    -RAK-
 */
static void store_prt_gold(void)
{
    char tmp[10];
    char out_val[64];

    prt("Gold Remaining: ", 19 + xtra_stock, 53);

    big_num_display(p_ptr->au, tmp);
    sprintf(out_val, "%6.6s", tmp);
    prt(out_val, 19 + xtra_stock, 68);
}


/*
 * Displays store (after clearing screen)        -RAK-
 */
static void display_store(void)
{
    char buf[80];


    /* Clear screen */
    Term_clear();

    /* The "Home" is special */
    if (cur_store_num == STORE_HOME)
    {
        /* Put the owner name */
        put_str("Your Home", 3, 30);


        /* Label the item descriptions */
        put_str("Item Description", 5, 3);


        /* If showing weights, show label */
        if (show_weights)
        {
            put_str("Weight", 5, 70);

        }

        put_str("Score", 5, 80);
    }

    /* The "Home" is special */
    else if (cur_store_num == STORE_MUSEUM)
    {
        /* Put the owner name */
        put_str("Museum", 3, 30);


        /* Label the item descriptions */
        put_str("Item Description", 5, 3);


        /* If showing weights, show label */
        if (show_weights)
        {
            put_str("Weight", 5, 70);

        }
        if (p_ptr->wizard)
            put_str("Score", 5, 80);
    }

    /* Normal stores */
    else
    {
        cptr store_name = (f_name + f_info[cur_store_feat].name);
        cptr owner_name = (ot_ptr->owner_name);
        cptr race_name = get_race_aux(ot_ptr->owner_race, 0)->name;

        /* Put the owner name and race */
        sprintf(buf, "%s (%s)", owner_name, race_name);
        put_str(buf, 3, 10);

        /* Show the max price in the store (above prices) */
        sprintf(buf, "%s (%d)", store_name, (int)ot_ptr->max_cost);
        prt(buf, 3, 50);

        /* Label the item descriptions */
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
 * Get the ID of a store item and return its value    -RAK-
 */
static int get_stock(int *com_val, cptr pmt, int i, int j)
{
    char    command;
    char    out_val[160];
    char    lo, hi;

#ifdef ALLOW_REPEAT /* TNB */

    /* Get the item index */
    if (repeat_pull(com_val))
    {
        /* Verify the item */
        if ((*com_val >= i) && (*com_val <= j))
        {
            /* Success */
            return (TRUE);
        }
    }

#endif /* ALLOW_REPEAT -- TNB */

    msg_line_clear();

    /* Assume failure */
    *com_val = (-1);

    /* Build the prompt */
    lo = I2A(i);
    hi = (j > 25) ? toupper(I2A(j - 26)) : I2A(j);
    (void)sprintf(out_val, "(Items %c-%c, ESC to exit) %s",
                  lo, hi, pmt);


    /* Ask until done */
    while (TRUE)
    {
        int k;

        /* Escape */
        if (!get_com(out_val, &command, FALSE)) break;

        /* Convert */
        if (islower(command))
            k = A2I(command);
        else if (isupper(command))
            k = A2I(tolower(command)) + 26;
        else
            k = -1;

        /* Legal responses */
        if ((k >= i) && (k <= j))
        {
            *com_val = k;
            break;
        }

        /* Oops */
        bell();
    }

    /* Clear the prompt */
    prt("", 0, 0);

    /* Cancel */
    if (command == ESCAPE) return (FALSE);

#ifdef ALLOW_REPEAT /* TNB */

    repeat_push(*com_val);

#endif /* ALLOW_REPEAT -- TNB */

    /* Success */
    return (TRUE);
}

/*
 * Buy an item from a store             -RAK-
 */
static void store_purchase(void)
{
    int i, amt;
    int item, item_new;

    s32b price, best;

    object_type forge;
    object_type *j_ptr;

    object_type *o_ptr;

    char o_name[MAX_NLEN];

    char out_val[160];

    if (cur_store_num == STORE_MUSEUM)
        return;

    /* Empty? */
    if (st_ptr->stock_num <= 0)
    {
        if (cur_store_num == STORE_HOME)
            msg_print("Your home is empty.");
        else
            msg_print("I am currently out of stock.");

        return;
    }


    /* Find the number of objects on this and following pages */
    i = (st_ptr->stock_num - store_top);

    /* And then restrict it to the current page */
    if (i > store_bottom) i = store_bottom;

    /* Prompt */
    if (cur_store_num == STORE_HOME)
    {
        sprintf(out_val, "Which item do you want to take? ");
    }
    else
    {
        sprintf(out_val, "Which item are you interested in? ");
    }

    /* Get the item number to be bought */
    if (!get_stock(&item, out_val, 0, i - 1)) return;

    /* Get the actual index */
    item = item + store_top;

    /* Get the actual item */
    o_ptr = &st_ptr->stock[item];

    /* Assume the player wants just one of them */
    amt = 1;

    /* Get local object */
    j_ptr = &forge;

    /* Get a copy of the object */
    object_copy(j_ptr, o_ptr);

    /*
     * If a rod or wand, allocate total maximum timeouts or charges
     * between those purchased and left on the shelf.
     */
    reduce_charges(j_ptr, o_ptr->number - amt);

    /* Modify quantity */
    j_ptr->number = amt;

    /* Hack -- require room in pack */
    if (!inven_carry_okay(j_ptr))
    {
        msg_print("You cannot carry that many different items.");
        return;
    }

    /* Determine the "best" price (per item) */
    best = price_item(j_ptr, ot_ptr->min_inflate, FALSE);

    /* Find out how many the player wants */
    if (o_ptr->number > 1)
    {
        /* Hack -- note cost of "fixed" items */
        if ((cur_store_num != STORE_HOME) &&
            (o_ptr->ident & IDENT_FIXED))
        {
            msg_format("That costs %d gold per item.", best);
        }

        /* Get a quantity */
        amt = get_quantity(NULL, o_ptr->number);

        /* Allow user abort */
        if (amt <= 0) return;
    }

    /* Get local object */
    j_ptr = &forge;

    /* Get desired object */
    object_copy(j_ptr, o_ptr);

    /*
     * If a rod or wand, allocate total maximum timeouts or charges
     * between those purchased and left on the shelf.
     */
    reduce_charges(j_ptr, o_ptr->number - amt);

    /* Modify quantity */
    j_ptr->number = amt;

    /* Hack -- require room in pack */
    if (!inven_carry_okay(j_ptr))
    {
        msg_print("You cannot carry that many items.");
        return;
    }

    /* Attempt to buy it */
    if (cur_store_num != STORE_HOME)
    {
        char prompt[255];
        price = (best * j_ptr->number);
        object_desc(o_name, j_ptr, OD_COLOR_CODED);
        sprintf(prompt, "Really purchase %s (%c) for <color:R>%d</color> gp? <color:y>[y/n]</color>", o_name, I2A(item), price);
        if (msg_prompt(prompt, "ny", PROMPT_YES_NO) == 'y')
        {
            if (p_ptr->au >= price)
            {
                if (cur_store_num == STORE_BLACK) /* The black market is illegal! */
                    virtue_add(VIRTUE_JUSTICE, -1);
                if((o_ptr->tval == TV_BOTTLE) && (cur_store_num != STORE_HOME))
                    virtue_add(VIRTUE_NATURE, -1);

                /* Make a sound */
                sound(SOUND_BUY);

                /* Spend the money */
                p_ptr->au -= price;
                stats_on_gold_buying(price);
                p_ptr->redraw |= PR_GOLD;
                if (prace_is_(RACE_MON_LEPRECHAUN))
                    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

                /* Update the display */
                store_prt_gold();

                /* Hack -- clear the "fixed" flag from the item */
                j_ptr->ident &= ~(IDENT_FIXED);

                /* Erase the inscription */
                j_ptr->inscription = 0;

                /* Erase the "feeling" */
                j_ptr->feeling = FEEL_NONE;
                j_ptr->ident &= ~(IDENT_STORE);
                j_ptr->marked &= ~(OM_RESERVED);

                /* Hack -- buying an item makes you aware of it */
                obj_identify_fully(j_ptr);

                /* Give it to the player */
                stats_on_purchase(j_ptr);
                item_new = inven_carry(j_ptr);

                /* Describe the final result */
                object_desc(o_name, &inventory[item_new], OD_COLOR_CODED);

                /* Message */
                msg_format("You have %s (%c).",
                           o_name, index_to_label(item_new));

                /* Auto-inscription */
                autopick_alter_item(item_new, FALSE);

                /* Handle stuff */
                handle_stuff();

                /* Note how many slots the store used to have */
                i = st_ptr->stock_num;

                /* Remove the bought items from the store */
                store_item_increase(item, -amt);
                store_item_optimize(item);

                /* Store is empty */
                if (st_ptr->stock_num == 0)
                {
                    /* Shuffle */
                    if (one_in_(STORE_SHUFFLE))
                    {
                        char buf[80];
                        /* Message */
                        msg_print("The shopkeeper retires.");

                        /* Shuffle the store */
                        store_shuffle(cur_store_num);

                        prt("",3,0);
                        sprintf(buf, "%s (%s)",
                            ot_ptr->owner_name, get_race_aux(ot_ptr->owner_race, 0)->name);
                        put_str(buf, 3, 10);
                        sprintf(buf, "%s (%d)",
                            (f_name + f_info[cur_store_feat].name), (int)ot_ptr->max_cost);
                        prt(buf, 3, 50);
                    }

                    /* Maintain */
                    else
                    {
                        /* Message */
                        msg_print("The shopkeeper brings out some new stock.");
                    }

                    /* New inventory */
                    for (i = 0; i < 10; i++)
                    {
                        /* Maintain the store */
                        store_maint(p_ptr->town_num, cur_store_num, STORE_MAINT_NORMAL);
                    }

                    /* Start over */
                    store_top = 0;

                    /* Redraw everything */
                    display_inventory();
                }

                /* The item is gone */
                else if (st_ptr->stock_num != i)
                {
                    /* Pick the correct screen */
                    if (store_top >= st_ptr->stock_num) store_top -= store_bottom;

                    /* Redraw everything */
                    display_inventory();
                }

                /* Item is still here */
                else
                {
                    /* Redraw the item */
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
        bool combined_or_reordered;

        /* Distribute charges of wands/rods */
        distribute_charges(o_ptr, j_ptr, amt);

        /* Give it to the player */
        item_new = inven_carry(j_ptr);

        /* Describe just the result */
        object_desc(o_name, &inventory[item_new], OD_COLOR_CODED);

        /* Message */
        msg_format("You have %s (%c).",  o_name, index_to_label(item_new));

        /* Handle stuff */
        handle_stuff();

        /* Take note if we take the last one */
        i = st_ptr->stock_num;

        /* Remove the items from the home */
        store_item_increase(item, -amt);
        store_item_optimize(item);

        combined_or_reordered = combine_and_reorder_home(STORE_HOME);

        /* Hack -- Item is still here */
        if (i == st_ptr->stock_num)
        {
            /* Redraw everything */
            if (combined_or_reordered) display_inventory();

            /* Redraw the item */
            else display_entry(item);
        }

        /* The item is gone */
        else
        {
            /* Nothing left */
            if (st_ptr->stock_num == 0) store_top = 0;

            /* Nothing left on that screen */
            else if (store_top >= st_ptr->stock_num) store_top -= store_bottom;

            /* Redraw everything */
            display_inventory();

            /* ??? virtue_add(VIRTUE_SACRIFICE, 1); */
        }
    }

    /* Not kicked out */
    return;
}


/*
 * Sell an item to the store (or home)
 */
static void store_sell(void)
{
    int item, item_pos;
    int amt;

    s32b price;

    object_type forge;
    object_type *q_ptr;

    object_type *o_ptr;

    cptr q, s;

    char o_name[MAX_NLEN];


    /* Prepare a prompt */
    if (cur_store_num == STORE_HOME)
        q = "Drop which item? ";

    else if (cur_store_num == STORE_MUSEUM)
        q = "Give which item? ";

    else
        q = "Sell which item? ";


    item_tester_no_ryoute = TRUE;
    /* Only allow items the store will buy */
    item_tester_hook = store_will_buy;

    /* Get an item */
    if (cur_store_num == STORE_HOME)
    {
        s = "You don't have any item to drop.";
    }
    else if (cur_store_num == STORE_MUSEUM)
    {
        s = "You don't have any item to give.";
    }
    else
    {
        s = "You have nothing that I want.";
    }

    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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


    /* Hack -- Cannot remove cursed items */
    if (equip_is_valid_slot(item))
    {
        if (object_is_cursed(o_ptr))
        {
            msg_print("Hmmm, it seems to be cursed.");
            return;
        }
        if (have_flag(o_ptr->flags, OF_NO_REMOVE))
        {
            msg_print("You can't sell yourself, silly!");
            return;
        }
    }

    if (o_ptr->tval == TV_POTION && o_ptr->sval == SV_POTION_BLOOD)
    {
        msg_print("You can't do that!  Your blood will go sour!");
        return;
    }

    /* Assume one item */
    amt = 1;

    /* Find out how many the player wants (letter means "all") */
    if (o_ptr->number > 1)
    {
        /* Get a quantity */
        amt = get_quantity(NULL, o_ptr->number);

        /* Allow user abort */
        if (amt <= 0) return;
    }

    /* Get local object */
    q_ptr = &forge;

    /* Get a copy of the object */
    object_copy(q_ptr, o_ptr);

    /* Modify quantity */
    q_ptr->number = amt;
    q_ptr->marked &= ~OM_WORN;

    /* Get a full description */
    object_desc(o_name, q_ptr, OD_COLOR_CODED);

    /* Remove any inscription, feeling for stores */
    if ((cur_store_num != STORE_HOME) && (cur_store_num != STORE_MUSEUM))
    {
        q_ptr->inscription = 0;
        q_ptr->feeling = FEEL_NONE;
    }

    /* Is there room in the store (or the home?) */
    if (!store_check_num(q_ptr))
    {
        if (cur_store_num == STORE_HOME)
            msg_print("Your home is full.");

        else if (cur_store_num == STORE_MUSEUM)
            msg_print("Museum is full.");

        else
            msg_print("I have not the room in my store to keep it.");

        return;
    }


    /* Real store */
    if ((cur_store_num != STORE_HOME) && (cur_store_num != STORE_MUSEUM))
    {
        char prompt[255];

        price = price_item(q_ptr, ot_ptr->min_inflate, TRUE);
        if (price > ot_ptr->max_cost)
            price = ot_ptr->max_cost;
        price *= q_ptr->number;

        sprintf(prompt, "Really sell %s (%c) for <color:R>%d</color> gp? <color:y>[y/n]</color>", o_name, index_to_label(item), price);
        if (msg_prompt(prompt, "ny", PROMPT_YES_NO) == 'y')
        {
            if (cur_store_num == STORE_BLACK) /* The black market is illegal! */
                virtue_add(VIRTUE_JUSTICE, -1);

            if((o_ptr->tval == TV_BOTTLE) && (cur_store_num != STORE_HOME))
                virtue_add(VIRTUE_NATURE, 1);

            /* Get some money */
            p_ptr->au += price;
            stats_on_gold_selling(price);
            p_ptr->redraw |= PR_GOLD;
            if (prace_is_(RACE_MON_LEPRECHAUN))
                p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

            /* Update the display */
            store_prt_gold();

            /* Identify it */
            stats_on_sell(o_ptr);
            obj_identify_fully(o_ptr);

            /* Get local object */
            q_ptr = &forge;

            /* Get a copy of the object */
            object_copy(q_ptr, o_ptr);

            /* Modify quantity */
            q_ptr->number = amt;

            /* Make it look like to be known */
            q_ptr->ident |= IDENT_STORE;

            /* Get the description all over again since it might not
               have been identified before. */
            object_desc(o_name, q_ptr, OD_COLOR_CODED);

            /* Describe the result (in message buffer) */
            msg_format("You sold %s for <color:R>%d</color> gold.", o_name, price);

            /*
             * Hack -- Allocate charges between those wands or rods sold
             * and retained, unless all are being sold. -LM-
             */
            distribute_charges(o_ptr, q_ptr, amt);

            /* Reset timeouts of the sold items */
            q_ptr->timeout = 0;

            /* Take the item from the player, describe the result */
            inven_item_increase(item, -amt);
            /*inven_item_describe(item);*/

            /* If items remain, auto-inscribe before optimizing */
            if (o_ptr->number > 0)
                autopick_alter_item(item, FALSE);

            inven_item_optimize(item);

            /* Handle stuff */
            handle_stuff();

            /* The store gets that item */
            item_pos = store_carry(q_ptr);

            /* Re-display if item is now in store */
            if (item_pos >= 0)
            {
                store_top = (item_pos / store_bottom) * store_bottom;
                display_inventory();
            }
        }
    }

    /* Player is at museum */
    else if (cur_store_num == STORE_MUSEUM)
    {
        char o2_name[MAX_NLEN];
        object_desc(o2_name, q_ptr, OD_NAME_ONLY);

        if (-1 == store_check_num(q_ptr))
        {
            msg_print("The same object as it is already in the Museum.");
        }
        else
        {
            msg_print("You cannot take items which is given to the Museum back!!");
        }
        if (!get_check(format("Really give %s to the Museum? ", o2_name))) return;

        /* Identify it */
        stats_on_sell(o_ptr); /* before identify, please! ... Why? */
        obj_identify_fully(o_ptr);

        /* Distribute charges of wands/rods */
        distribute_charges(o_ptr, q_ptr, amt);

        /* Describe */
        msg_format("You drop %s (%c).", o_name, index_to_label(item));

        /* Take it from the players inventory */
        inven_item_increase(item, -amt);
        /*inven_item_describe(item);*/
        inven_item_optimize(item);

        /* Handle stuff */
        handle_stuff();

        /* Let the home carry it */
        item_pos = home_carry(q_ptr);

        /* Update store display */
        if (item_pos >= 0)
        {
            store_top = (item_pos / store_bottom) * store_bottom;
            display_inventory();
        }
    }
    /* Player is at home */
    else
    {
        /* Distribute charges of wands/rods */
        distribute_charges(o_ptr, q_ptr, amt);

        /* Describe */
        msg_format("You drop %s (%c).", o_name, index_to_label(item));

        /* Take it from the players inventory */
        inven_item_increase(item, -amt);
        /*inven_item_describe(item);*/
        inven_item_optimize(item);

        /* Handle stuff */
        handle_stuff();

        /* Let the home carry it */
        item_pos = home_carry(q_ptr);

        /* Update store display */
        if (item_pos >= 0)
        {
            store_top = (item_pos / store_bottom) * store_bottom;
            display_inventory();
        }
    }

    if (equip_is_valid_slot(item))
        android_calc_exp();
}


/*
 * Examine an item in a store               -JDL-
 */
static void store_examine(void)
{
    int         i;
    int         item;
    object_type *o_ptr;
    char        out_val[160];


    /* Empty? */
    if (st_ptr->stock_num <= 0)
    {
        if (cur_store_num == STORE_HOME)
            msg_print("Your home is empty.");

        else if (cur_store_num == STORE_MUSEUM)
            msg_print("Museum is empty.");

        else
            msg_print("I am currently out of stock.");

        return;
    }


    /* Find the number of objects on this and following pages */
    i = (st_ptr->stock_num - store_top);

    /* And then restrict it to the current page */
    if (i > store_bottom) i = store_bottom;

    /* Prompt */
    sprintf(out_val, "Which item do you want to examine? ");

    /* Get the item number to be examined */
    if (!get_stock(&item, out_val, 0, i - 1)) return;

    /* Get the actual index */
    item = item + store_top;

    /* Get the actual item */
    o_ptr = &st_ptr->stock[item];

    if (cur_store_num != STORE_HOME)
        obj_identify_fully(o_ptr);
    obj_display(o_ptr);
    return;
}

/*
 * Remove an item from museum (Originally from TOband)
 */
static void museum_remove_object(void)
{
    int         i;
    int         item;
    object_type *o_ptr;
    char        o_name[MAX_NLEN];
    char        out_val[160];

    /* Empty? */
    if (st_ptr->stock_num <= 0)
    {
        msg_print("Museum is empty.");

        return;
    }

    /* Find the number of objects on this and following pages */
    i = st_ptr->stock_num - store_top;

    /* And then restrict it to the current page */
    if (i > store_bottom) i = store_bottom;

    /* Prompt */
    sprintf(out_val, "Which item do you want to order to remove? ");

    /* Get the item number to be removed */
    if (!get_stock(&item, out_val, 0, i - 1)) return;

    /* Get the actual index */
    item = item + store_top;

    /* Get the actual item */
    o_ptr = &st_ptr->stock[item];

    /* Description */
    object_desc(o_name, o_ptr, 0);

    msg_print("You cannot see items which is removed from the Museum!");
    if (!get_check(format("Really order to remove %s from the Museum? ", o_name))) return;

    /* Message */
    msg_format("You ordered to remove %s.", o_name);

    /* Remove the items from the home */
    store_item_increase(item, -o_ptr->number);
    store_item_optimize(item);

    (void)combine_and_reorder_home(STORE_MUSEUM);

    /* The item is gone */

    /* Nothing left */
    if (st_ptr->stock_num == 0) store_top = 0;

    /* Nothing left on that screen */
    else if (store_top >= st_ptr->stock_num) store_top -= store_bottom;

    /* Redraw everything */
    display_inventory();

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
 */
static void store_process_command(void)
{
#ifdef ALLOW_REPEAT /* TNB */

    /* Handle repeating the last command */
    repeat_check();

#endif /* ALLOW_REPEAT -- TNB */

    if (rogue_like_commands && command_cmd == 'l')
    {
        command_cmd = 'x';    /* hack! */
    }

    /* Parse the command */
    switch (command_cmd)
    {
        case 'b':
            if (cur_store_num != STORE_HOME && cur_store_num != STORE_MUSEUM)
                _buyout();
            break;

        /* Leave */
        case ESCAPE:
        {
            leave_store = TRUE;
            break;
        }

        case '-':
        {
            if (st_ptr->stock_num <= store_bottom) {
                msg_print("Entire inventory is shown.");
            }
            else{
                store_top -= store_bottom;
                if ( store_top < 0 )
                    store_top = ((st_ptr->stock_num - 1 )/store_bottom) * store_bottom;
                if ( (cur_store_num == STORE_HOME) && (powerup_home == FALSE) )
                    if ( store_top >= store_bottom ) store_top = store_bottom;
                display_inventory();
            }
            break;
        }

        /* Browse */
        case ' ':
        {
            if (st_ptr->stock_num <= store_bottom)
            {
                msg_print("Entire inventory is shown.");

            }
            else
            {
                store_top += store_bottom;
                if ((cur_store_num == STORE_HOME) &&
                    (powerup_home == FALSE) &&
                    (st_ptr->stock_num >= STORE_INVEN_MAX))
                {
                    if (store_top >= (STORE_INVEN_MAX - 1))
                    {
                        store_top = 0;
                    }
                }
                else
                {
                    if (store_top >= st_ptr->stock_num) store_top = 0;
                }

                display_inventory();
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

        /* Examine */
        case 'x':
        {
            store_examine();
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
            equip_wield();
            break;
        }

        /* Take off equipment */
        case 't':
        {
            equip_takeoff();
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
            do_cmd_inspect();
            break;
        }

        /* Hack -- toggle windows */
        case KTRL('I'):
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
            py_display();
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
            p_ptr->town_num = old_town_num;
            do_cmd_pref();
            p_ptr->town_num = inner_town_num;
            break;
        }

        /* Interact with macros */
        case '@':
        {
            p_ptr->town_num = old_town_num;
            do_cmd_macros();
            p_ptr->town_num = inner_town_num;
            break;
        }

        /* Interact with visuals */
        case '%':
        {
            p_ptr->town_num = old_town_num;
            do_cmd_visuals();
            p_ptr->town_num = inner_town_num;
            break;
        }

        /* Interact with colors */
        case '&':
        {
            p_ptr->town_num = old_town_num;
            do_cmd_colors();
            p_ptr->town_num = inner_town_num;
            break;
        }

        /* Interact with options */
        case '=':
        {
            do_cmd_options();
            (void)combine_and_reorder_home(STORE_HOME);
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

        /* Show previous messages */
        case KTRL('P'):
        {
            do_cmd_messages(0);
            break;
        }

        /* Check artifacts, uniques etc. */
        case '~':
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
            if ((cur_store_num == STORE_MUSEUM) && (command_cmd == 'h'))
            {
                museum_remove_object();
            }
            else
            {
                msg_print("That command does not work in stores.");
            }

            break;
        }
    }
}


/*
 * Enter a store, and interact with it.
 *
 * Note that we use the standard "request_command()" function
 * to get a command, allowing us to use "command_arg" and all
 * command macros and other nifty stuff, but we use the special
 * "shopping" argument, to force certain commands to be converted
 * into other commands, normally, we convert "p" (pray) and "m"
 * (cast magic) into "g" (get), and "s" (search) into "d" (drop).
 */
void do_cmd_store(void)
{
    int         which;
    int         maintain_num;
    int         i;
    cave_type   *c_ptr;
    bool        need_redraw_store_inv = FALSE; /* To redraw missiles damage and prices in store */
    int         w, h;
    bool        friend_hack = FALSE;
    int         options = STORE_MAINT_NORMAL;
    rect_t      msg_display_rect;


    /* Get term size */
    Term_get_size(&w, &h);

    /* Calculate stocks per 1 page */
    xtra_stock = MIN(14+26, ((h > 24) ? (h - 24) : 0));
    store_bottom = MIN_STOCK + xtra_stock;

    /* Access the player grid */
    c_ptr = &cave[py][px];

    /* Verify a store */
    if (!cave_have_flag_grid(c_ptr, FF_STORE))
    {
        msg_print("You see no store here.");
        return;
    }

    /* Extract the store code */
    which = f_info[c_ptr->feat].subtype;

    old_town_num = p_ptr->town_num;
    if ((which == STORE_HOME) || (which == STORE_MUSEUM)) p_ptr->town_num = 1;
    if (dun_level || !p_ptr->town_num) p_ptr->town_num = NO_TOWN;
    inner_town_num = p_ptr->town_num;

    if ( (mut_present(MUT_MERCHANTS_FRIEND) || p_ptr->wizard)
      && which != STORE_HOME
      && which != STORE_MUSEUM )
    {
        friend_hack = TRUE;
        --xtra_stock;
        --store_bottom;
    }

    /* Hack -- Check the "locked doors" */
    if (which == STORE_HOME)
    {
        /* New: Home is always open! */
    }
    else if (town[p_ptr->town_num].store[which].store_open >= game_turn || ironman_shops)
    {
        msg_print("The doors are locked.");
        p_ptr->town_num = old_town_num;
        return;
    }

    msg_display_rect = rect_create(0, 0, 80, 3);
    msg_line_init(&msg_display_rect);
    store_hack = TRUE;

    /* Calculate the number of store maintainances since the last visit */
    maintain_num = (game_turn - town[p_ptr->town_num].store[which].last_visit) / (TURNS_PER_TICK * STORE_TICKS);

    /* Maintain the store max. 10 times */
    if (maintain_num > 10) maintain_num = 10;

    if (maintain_num && (which == STORE_BLACK || which == STORE_JEWELER))
    {
        int xp = town[p_ptr->town_num].store[which].last_exp;
        xp += MIN(MAX(xp / 20, 1000), 100000);
        if ( !ironman_downward
          && p_ptr->max_plv <= town[p_ptr->town_num].store[which].last_lev
          && p_ptr->max_exp <= xp
          && p_ptr->prace != RACE_ANDROID )
        {
            options = STORE_MAINT_CULL;
        }
    }

    if (maintain_num)
    {
        /* Maintain the store */
        for (i = 0; i < maintain_num; i++)
            store_maint(p_ptr->town_num, which, options);

        /* Save the visit if we actually restocked any */
        if (options == STORE_MAINT_NORMAL)
        {
            town[p_ptr->town_num].store[which].last_visit = game_turn;
            town[p_ptr->town_num].store[which].last_lev = p_ptr->max_plv;
            town[p_ptr->town_num].store[which].last_exp = p_ptr->max_exp;
        }
    }

    /* Forget the lite */
    forget_lite();

    /* Forget the view */
    forget_view();


    /* Hack -- Character is in "icky" mode */
    character_icky = TRUE;


    /* No command argument */
    command_arg = 0;

    /* No repeated command */
    command_rep = 0;

    /* No automatic command */
    command_new = 0;

    /* Do not expand macros */
    get_com_no_macros = TRUE;

    /* Save the store number */
    cur_store_num = which;

    /* Hack -- save the store feature */
    cur_store_feat = c_ptr->feat;

    /* Save the store and owner pointers */
    st_ptr = &town[p_ptr->town_num].store[cur_store_num];
    ot_ptr = &owners[cur_store_num][st_ptr->owner];


    /* Start at the beginning */
    store_top = 0;

    /* Display the store */
    display_store();

    /* Do not leave */
    leave_store = FALSE;

    /* Interact with player */
    while (!leave_store)
    {
        /* Clear */
        clear_from(20 + xtra_stock);


        /* Basic commands */
        prt(" ESC) Exit from Building.", 21 + xtra_stock, 0);

        /* Browse if necessary */
        if (st_ptr->stock_num > store_bottom)
        {
            prt(" -) Previous page", 22 + xtra_stock, 0);
            prt(" SPACE) Next page", 23 + xtra_stock, 0);
        }

        if (friend_hack)
        {
            prt(" 1) Shuffle Stock (5000gp)", 24 + xtra_stock, 0);
            prt("   2) Hold Item (10000gp)", 24 + xtra_stock, 27);
        }

        /* Home commands */
        if (cur_store_num == STORE_HOME)
        {
            prt("g) Get an item", 21 + xtra_stock, 27);
            prt("d) Drop an item", 22 + xtra_stock, 27);
            prt("x) eXamine an item", 23 + xtra_stock, 27);
        }

        /* Museum commands */
        else if (cur_store_num == STORE_MUSEUM)
        {
            prt("d) Drop an item", 21 + xtra_stock, 27);
            prt("h) Hide an item", 22 + xtra_stock, 27);
            prt("x) eXamine an item", 23 + xtra_stock, 27);
        }

        /* Shop commands XXX XXX XXX */
        else
        {
            prt("p) Purchase an item", 21 + xtra_stock, 30);
            prt("s) Sell an item", 22 + xtra_stock, 30);
            prt("x) eXamine an item", 23 + xtra_stock,30);
            if (rogue_like_commands) /* P -> b coincidentally and 'P'urchase entire stock works */
                prt("  P) Purchase entire stock", 23 + xtra_stock, 56);
            else
                prt("  b) Buyout store", 23 + xtra_stock, 56);
        }

        prt("i/e) Inventry/Equipment list", 21 + xtra_stock, 56);
        prt("w/t) Wear/Take off equipment", 22 + xtra_stock, 56);
        prt("You may: ", 20 + xtra_stock, 0);

        /* Get a command */
        request_command(TRUE);

        if (friend_hack)
        {
            switch (command_cmd)
            {
            case '1':
                if (5000 > p_ptr->au)
                    msg_print("You do not have the gold!");
                else
                {
                    _restock(st_ptr, TRUE);
                    need_redraw_store_inv = TRUE;
                    p_ptr->au -= 5000;
                    stats_on_gold_services(5000);
                    p_ptr->redraw |= PR_GOLD;
                    store_prt_gold();
                }
                break;
            case '2':
                if (10000 > p_ptr->au)
                    msg_print("You do not have the gold!");
                else
                {
                    int item, i;
                    object_type *o_ptr;

                    i = (st_ptr->stock_num - store_top);
                    if (i > store_bottom) i = store_bottom;
                    if (get_stock(&item, "Which item shall I hold for you? ", 0, i - 1))
                    {
                        item = item + store_top;
                        o_ptr = &st_ptr->stock[item];
                        if (o_ptr->marked & OM_RESERVED)
                        {
                            msg_print("You already reserved that item!");
                        }
                        else
                        {
                            o_ptr->marked |= OM_RESERVED;
                            p_ptr->au -= 10000;
                            stats_on_gold_services(10000);
                            p_ptr->redraw |= PR_GOLD;
                            store_prt_gold();

                            need_redraw_store_inv = TRUE;
                            msg_print("Done! Come back later when you have more gold, OK?");
                        }
                    }
                }
                break;
            default:
                store_process_command();
                break;
            }
        }
        else
            store_process_command();

        /*
         * Hack -- To redraw missiles damage and prices in store
         * If player's charisma changes, or if player changes a bow, PU_BONUS is set
         */
         if (p_ptr->update & PU_BONUS)
            need_redraw_store_inv = TRUE;

         if (p_ptr->redraw & PR_GOLD)
            need_redraw_store_inv = TRUE;

        /* Hack -- Character is still in "icky" mode */
        character_icky = TRUE;

        /* Notice stuff */
        notice_stuff();

        /* Handle stuff */
        handle_stuff();

        /* XXX XXX XXX Pack Overflow */
        if (inventory[INVEN_PACK].k_idx)
        {
            int item = INVEN_PACK;

            object_type *o_ptr = &inventory[item];

            /* Hack -- Flee from the store */
            if (cur_store_num != STORE_HOME)
            {
                /* Message */
                if (cur_store_num == STORE_MUSEUM)
                    msg_print("Your pack is so full that you flee the Museum...");
                else
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

                object_type forge;
                object_type *q_ptr;

                char o_name[MAX_NLEN];


                /* Give a message */
                msg_print("Your pack overflows!");


                /* Get local object */
                q_ptr = &forge;

                /* Grab a copy of the item */
                object_copy(q_ptr, o_ptr);

                /* Describe it */
                object_desc(o_name, q_ptr, 0);

                /* Message */
                msg_format("You drop %s (%c).", o_name, index_to_label(item));


                /* Remove it from the players inventory */
                inven_item_increase(item, -255);
                inven_item_describe(item);
                inven_item_optimize(item);

                /* Handle stuff */
                handle_stuff();

                /* Let the home carry it */
                item_pos = home_carry(q_ptr);

                /* Redraw the home */
                if (item_pos >= 0)
                {
                    store_top = (item_pos / store_bottom) * store_bottom;
                    display_inventory();
                }
            }
        }

        /* Hack -- Redisplay store prices if charisma changes */
        /* Hack -- Redraw missiles damage if player changes bow */
        if (need_redraw_store_inv) display_inventory();

        /* Hack -- get kicked out of the store */
        if (st_ptr->store_open >= game_turn) leave_store = TRUE;
    }

    p_ptr->town_num = old_town_num;

    /* Free turn XXX XXX XXX */
    energy_use = 100;


    /* Hack -- Character is no longer in "icky" mode */
    character_icky = FALSE;


    /* Hack -- Cancel automatic command */
    command_new = 0;

    /* Hack -- Cancel "see" mode */
    command_see = FALSE;

    /* Allow expanding macros */
    get_com_no_macros = FALSE;

    msg_line_clear();

    /* Clear the screen */
    Term_clear();


    /* Update everything */
    p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);
    p_ptr->update |= (PU_MONSTERS);

    /* Redraw entire screen */
    p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_EQUIPPY);

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP);

    /* Window stuff */
    p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

    store_hack = FALSE;
    msg_line_init(NULL);
}



/*
 * Shuffle one of the stores.
 */
void store_shuffle(int which)
{
    int i, j;


    /* Ignore home */
    if (which == STORE_HOME) return;
    if (which == STORE_MUSEUM) return;


    /* Save the store index */
    cur_store_num = which;

    /* Activate that store */
    st_ptr = &town[p_ptr->town_num].store[cur_store_num];

    j = st_ptr->owner;
    /* Pick a new owner */
    while(1)
    {
        st_ptr->owner = (byte)randint0(MAX_OWNERS);
        if (j == st_ptr->owner) continue;
        for (i = 1;i < max_towns; i++)
        {
            if (i == p_ptr->town_num) continue;
            if (st_ptr->owner == town[i].store[cur_store_num].owner) break;
        }
        if (i == max_towns) break;
    }

    /* Activate the new owner */
    ot_ptr = &owners[cur_store_num][st_ptr->owner];


    /* Reset the owner data */
    st_ptr->insult_cur = 0;
    st_ptr->store_open = 0;
    st_ptr->good_buy = 0;
    st_ptr->bad_buy = 0;


    /* Hack -- discount all the items */
    for (i = 0; i < st_ptr->stock_num; i++)
    {
        object_type *o_ptr;

        /* Get the item */
        o_ptr = &st_ptr->stock[i];

        if (!object_is_artifact(o_ptr))
        {
            /* Hack -- Sell all non-artifact old items for "half price" */
            o_ptr->discount = 50;

            /* Hack -- Items are no longer "fixed price" */
            o_ptr->ident &= ~(IDENT_FIXED);

            /* Mega-Hack -- Note that the item is "on sale" */
            o_ptr->inscription = quark_add("on sale");
        }
    }
}

/*
 * Maintain the inventory at the stores.
 */
void store_maint(int town_num, int store_num, int options)
{
    int j;

    cur_store_num = store_num;

    /* Ignore home */
    if (store_num == STORE_HOME) return;
    if (store_num == STORE_MUSEUM) return;

    /* Activate that store */
    st_ptr = &town[town_num].store[store_num];

    /* Activate the owner */
    ot_ptr = &owners[store_num][st_ptr->owner];

    /* Store keeper forgives the player */
    st_ptr->insult_cur = 0;

    /* Mega-Hack -- prune the black market */
    if (store_num == STORE_BLACK)
    {
        /* Destroy crappy black market items */
        for (j = st_ptr->stock_num - 1; j >= 0; j--)
        {
            object_type *o_ptr = &st_ptr->stock[j];

            /* Destroy crappy items */
            if (black_market_crap(o_ptr))
            {
                /* Destroy the item */
                store_item_increase(j, 0 - o_ptr->number);
                store_item_optimize(j);
            }
        }
    }

    if (options == STORE_MAINT_NORMAL)
        _restock(st_ptr, FALSE);
    else if (options == STORE_MAINT_CULL)
        _cull(st_ptr, FALSE);
}


/*
 * Initialize the stores
 */
void store_init(int town_num, int store_num)
{
    int         k;

    cur_store_num = store_num;

    /* Activate that store */
    st_ptr = &town[town_num].store[store_num];


    /* Pick an owner */
    while(1)
    {
        int i;

        st_ptr->owner = (byte)randint0(MAX_OWNERS);
        for (i = 1;i < max_towns; i++)
        {
            if (i == town_num) continue;
            if (st_ptr->owner == town[i].store[store_num].owner) break;
        }
        if (i == max_towns) break;
    }

    /* Activate the new owner */
    ot_ptr = &owners[store_num][st_ptr->owner];


    /* Initialize the store */
    st_ptr->store_open = 0;
    st_ptr->insult_cur = 0;
    st_ptr->good_buy = 0;
    st_ptr->bad_buy = 0;

    /* Nothing in stock */
    st_ptr->stock_num = 0;

    /*
     * MEGA-HACK - Last visit to store is
     * BEFORE player birth to enable store restocking
     */
    st_ptr->last_visit = -10L * TURNS_PER_TICK * STORE_TICKS;
    st_ptr->last_lev = 0;
    st_ptr->last_exp = 0;

    /* Clear any old items */
    for (k = 0; k < st_ptr->stock_size; k++)
    {
        object_wipe(&st_ptr->stock[k]);
    }
}


void move_to_black_market(object_type *o_ptr)
{
    /* Not in town */
    if (!p_ptr->town_num) return;

    st_ptr = &town[p_ptr->town_num].store[STORE_BLACK];

    o_ptr->ident |= IDENT_STORE;

    (void)store_carry(o_ptr);

    object_wipe(o_ptr); /* Don't leave a bogus object behind... */
}

static void _restock(store_type *st_ptr, bool all)
{
    int j;
    j = _cull(st_ptr, all);
    while (st_ptr->stock_num < j)
        store_create();
}

static int _cull(store_type *st_ptr, bool all)
{
    int j, attempt;

    j = st_ptr->stock_num;

    if (all)
        j = 0;
    else
    {
        j = j - randint1(STORE_TURNOVER);
        if (j > STORE_MAX_KEEP)
            j = STORE_MAX_KEEP;
        if (j < STORE_MIN_KEEP)
            j = STORE_MIN_KEEP;
        if (j < 0)
            j = 0;
    }

    attempt = 1;
    while (st_ptr->stock_num > j)
    {
        store_delete(); /* Players may reserve items, so this might fail! */
        attempt++;
        if (attempt > 50)
            break;
    }

    j = st_ptr->stock_num;

    if (all)
        j = STORE_MAX_KEEP;
    else
    {
        j = j + randint1(STORE_TURNOVER);
        if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;
        if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;
    }

    if (j >= st_ptr->stock_size)
        j = st_ptr->stock_size - 1;

    return j;
}

/* Buyout inventory with a minimum of "noise". Apply automizer
   to determine what goes in the player's pack. Stop on completion,
   run out of gold, or run out of inventory slots. */
static void _buyout(void)
{
    int i, old_stock_num, amt;
    s32b price, best, total_price = 0;
    object_type forge;
    object_type *j_ptr;
    object_type *o_ptr;

    if (cur_store_num == STORE_MUSEUM || cur_store_num == STORE_HOME)
        return;

    if (!get_check("Are you sure you want to buy the entire inventory of this store? "))
        return;

    for (i = st_ptr->stock_num - 1; i >= 0; i--)
    {
        o_ptr = &st_ptr->stock[i];
        amt = o_ptr->number;

        j_ptr = &forge;
        object_copy(j_ptr, o_ptr);
        reduce_charges(j_ptr, o_ptr->number - amt);
        j_ptr->number = amt;

        best = price_item(j_ptr, ot_ptr->min_inflate, FALSE);
        price = (best * j_ptr->number);

        if (p_ptr->au >= price)
        {
            bool destroy = FALSE;
            int auto_pick_idx = is_autopick(j_ptr);

            if (auto_pick_idx >= 0 && autopick_list[auto_pick_idx].action & DO_AUTODESTROY)
                destroy = TRUE;

            if (!destroy && !inven_carry_okay(o_ptr))
            {
                msg_print("Your pack is full.");
                continue;
            }

            if (cur_store_num == STORE_BLACK) /* The black market is illegal! */
                virtue_add(VIRTUE_JUSTICE, -1);
            if((o_ptr->tval == TV_BOTTLE) && (cur_store_num != STORE_HOME))
                virtue_add(VIRTUE_NATURE, -1);

            sound(SOUND_BUY);
            p_ptr->au -= price;
            stats_on_gold_buying(price);
            p_ptr->redraw |= PR_GOLD;
            total_price += price;
            store_prt_gold();

            j_ptr->ident &= ~(IDENT_FIXED);
            j_ptr->inscription = 0;
            j_ptr->feeling = FEEL_NONE;
            j_ptr->ident &= ~(IDENT_STORE);
            j_ptr->marked &= ~(OM_RESERVED);
            obj_identify_fully(j_ptr);

            if (!destroy)
            {
                int slot = inven_carry(j_ptr);
                handle_stuff();
                autopick_alter_item(slot, FALSE);
            }

            old_stock_num = st_ptr->stock_num;
            store_item_increase(i, -amt);
            store_item_optimize(i);

            if (st_ptr->stock_num == 0)
            {
                if (one_in_(STORE_SHUFFLE))
                {
                    char buf[80];
                    msg_print("The shopkeeper retires.");
                    store_shuffle(cur_store_num);
                    prt("",3,0);
                    sprintf(buf, "%s (%s)",
                        ot_ptr->owner_name, get_race_aux(ot_ptr->owner_race, 0)->name);
                    put_str(buf, 3, 10);
                    sprintf(buf, "%s (%d)",
                        (f_name + f_info[cur_store_feat].name), (int)(ot_ptr->max_cost));
                    prt(buf, 3, 50);
                }
                else
                {
                    msg_print("The shopkeeper brings out some new stock.");
                }

                for (i = 0; i < 10; i++)
                    store_maint(p_ptr->town_num, cur_store_num, STORE_MAINT_NORMAL);

                store_top = 0;
                display_inventory();
                break;
            }
            else if (st_ptr->stock_num != old_stock_num)
            {
                if (store_top >= st_ptr->stock_num) store_top -= store_bottom;
                display_inventory();
            }
            else
            {
                display_entry(i);
            }
        }
        else
        {
            msg_print("You do not have enough gold.");
            break;
        }
    }
    msg_format("You spent %d gp.", total_price);
    if (prace_is_(RACE_MON_LEPRECHAUN))
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);
}

