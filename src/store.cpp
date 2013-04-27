// File: store.cpp
// Purpose: Store commands

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Show a message on the top line.
 */
static void msg_p_old(char *msg)
{
    box(0, 0, 639, 15, COLOR_BLACK);
    put_string(0, 0, msg, COLOR_WHITE);
    put_string(8*(strlen(msg)+1), 0, "--More--", COLOR_LT_BLUE);
    screen_refresh();
    wait_for_key();
    box(0, 0, 639, 15, COLOR_BLACK);
}


/*
 * Display a formatted message, using "vstrnfmt()" and "msg_p_old()".
 */
static void msg_f_old(char *fmt, ...)
{
    va_list vp;
    char buf[1024];

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Format the args, save the length */
    vstrnfmt(buf, 1024, fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Display */
    msg_p_old(buf);
}


/*
 * Messages for reacting to purchase prices.
 */

const int MAX_COMMENT_REACT = 4;

static char *comment_react_very_bad[MAX_COMMENT_REACT] = {
    "\"Arrgghh!\"",
    "\"You bastard!\"",
    "You hear someone sobbing...",
    "The shopkeeper howls in agony!"
};

static char *comment_react_bad[MAX_COMMENT_REACT] = {
    "\"Damn!\"",
    "\"You fiend!\"",
    "The shopkeeper curses at you.",
    "The shopkeeper glares at you."
};

static char *comment_react_good[MAX_COMMENT_REACT] = {
    "\"Cool!\"",
    "\"You've made my day!\"",
    "The shopkeeper giggles.",
    "The shopkeeper laughs loudly."
};

static char *comment_react_great[MAX_COMMENT_REACT] = {
    "\"Yipee!\"",
    "\"I think I'll retire!\"",
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
    if ((value <= 0) && (price > value)) {
        msg_p_old(comment_react_very_bad[rand_int(MAX_COMMENT_REACT)]);
    }

    /* Item was cheaper than we thought, and we paid more than necessary */
    else if ((value < guess) && (price > value)) {
        msg_p_old(comment_react_bad[rand_int(MAX_COMMENT_REACT)]);
    }

    /* Item was a good bargain, and we got away with it */
    else if ((value > guess) && (value < (4 * guess)) && (price < value)) {
        msg_p_old(comment_react_good[rand_int(MAX_COMMENT_REACT)]);
    }

    /* Item was a great bargain, and we got away with it */
    else if ((value > guess) && (price < value)) {
        msg_p_old(comment_react_great[rand_int(MAX_COMMENT_REACT)]);
    }
}





// We store the current store number here so everyone can access it
static int store_num = 0;

// We store the current store page here so everyone can access it
static int store_top = 0;

// We store the current store pointer here so everyone can access it
static store_type *st_ptr = NULL;

// We store the current owner here so everyone can access it
static owner_type *ot_ptr = NULL;






/*
 * Buying and selling adjustments for race combinations.
 * Entry[owner][player] gives the basic "cost inflation".
 */
static byte rgold_adj[MAX_RACES][MAX_RACES] = {
                   // Hum  HfE  Elf  Hal  Gno  Dwa  HfO  HfT  Dun  HiE
/* Human        */  { 100, 105, 105, 110, 113, 115, 120, 125, 100, 105},
/* Half-Elf     */  { 110, 100, 100, 105, 110, 120, 125, 130, 110, 100},
/* Elf          */  { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100},
/* Halfling     */  { 115, 110, 105,  95, 105, 110, 115, 130, 115, 105},
/* Gnome        */  { 115, 115, 110, 105,  95, 110, 115, 130, 115, 110},
/* Dwarf        */  { 115, 120, 120, 110, 110,  95, 125, 135, 115, 120},
/* Half-Orc     */  { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125},
/* Half-Troll   */  { 110, 115, 115, 110, 110, 130, 110, 110, 110, 115},
/* Dunedain     */  { 100, 105, 105, 110, 113, 115, 120, 125, 100, 105},
/* High_Elf     */  { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100}
};




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
static s32b price_item(CItem *i_ptr, int greed, bool flip)
{
    int     factor;
    int     adjust;
    s32b    price;


    // Get the value of one of the items
    price = i_ptr->GetValue();

    // Worthless items
    if (price <= 0) return 0L;


    // Compute the racial factor
    factor = rgold_adj[ot_ptr->owner_race][p_ptr->GetRace()];

    // Add in the charisma factor
    factor += adj_chr_gold[p_ptr->GetStatInd(STAT_CHR)];


    // Shop is buying
    if (flip) {
        // Adjust for greed
        adjust = 100 + (300 - (greed + factor));

        // Never get "silly"
        if (adjust > 100) adjust = 100;
    }

    // Shop is selling
    else {
        // Adjust for greed
        adjust = 100 + ((greed + factor) - 300);

        /* Never get "silly" */
        if (adjust < 100) adjust = 100;

        /* Black market sucks */
        if (store_num == 6) price *= 2;
    }

    /* Compute the final price (with rounding) */
    price = (price * adjust + 50L) / 100L;

    /* Note -- Never become "free" */
    if (price <= 0L) return 1L;

    /* Return the price */
    return price;
}


/*
 * Certain "cheap" objects should be created in "piles"
 * Some objects can be sold at a "discount" (in small piles)
 */
static void mass_produce(CItem *i_ptr)
{
    int size = 1;
    int discount = 0;

    s32b cost = i_ptr->GetValue();


    // Analyze the type
    switch (i_ptr->GetTval()) {
        // Food, Flasks, and Lites
        case TV_FOOD:
        case TV_FLASK:
        case TV_LITE:
            if (cost <= 5L) size += damroll(3, 5);
            if (cost <= 20L) size += damroll(3, 5);
            break;

        case TV_POTION:
        case TV_SCROLL:
            if (cost <= 60L) size += damroll(3, 5);
            if (cost <= 240L) size += damroll(1, 5);
            break;

        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
            if (cost <= 50L) size += damroll(2, 3);
            if (cost <= 500L) size += damroll(1, 3);
            break;

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
            if (i_ptr->isEgoItem()) break;
            if (cost <= 10L) size += damroll(3, 5);
            if (cost <= 100L) size += damroll(3, 5);
            break;

        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
            if (cost <= 5L) size += damroll(5, 5);
            if (cost <= 50L) size += damroll(5, 5);
            if (cost <= 500L) size += damroll(5, 5);
            break;
    }


    /* Pick a discount */
    if (cost < 5) {
        discount = 0;
    }
    else if (one_in(25)) {
        discount = 25;
    }
    else if (one_in(150)) {
        discount = 50;
    }
    else if (one_in(300)) {
        discount = 75;
    }
    else if (one_in(500)) {
        discount = 90;
    }


    // Save the discount
    i_ptr->SetDiscount(discount);

    // Save the total pile size
    i_ptr->SetNumber(size - (size * discount / 100));
}








/*
 * Determine if a store item can "absorb" another item
 *
 * See "object_similar()" for the same function for the "player"
 */
static bool store_object_similar(CItem *i_ptr, CItem *j_ptr)
{
    /* Hack -- Identical items cannot be stacked */
    if (i_ptr == j_ptr) return FALSE;

    /* Different objects cannot be stacked */
    if (i_ptr->GetKIdx() != j_ptr->GetKIdx()) return FALSE;

    /* Different charges (etc) cannot be stacked */
    if (i_ptr->GetPval() != j_ptr->GetPval()) return FALSE;

    /* Require many identical values */
    if (i_ptr->GetToH() != j_ptr->GetToH()) return FALSE;
    if (i_ptr->GetToD() != j_ptr->GetToD()) return FALSE;
    if (i_ptr->GetToA() != j_ptr->GetToA()) return FALSE;

    /* Require identical "artifact" names */
    if (i_ptr->GetName1() != j_ptr->GetName1()) return FALSE;

    /* Require identical "ego-item" names */
    if (i_ptr->GetName2() != j_ptr->GetName2()) return FALSE;

    /* Hack -- Never stack "powerful" items */
    if (i_ptr->GetXtra1() || j_ptr->GetXtra1()) return FALSE;

    /* Hack -- Never stack recharging items */
    if (i_ptr->GetTimeout() || j_ptr->GetTimeout()) return FALSE;

    /* Require many identical values */
    if (i_ptr->GetAC() != j_ptr->GetAC()) return FALSE;
    if (i_ptr->GetDD() != j_ptr->GetDD()) return FALSE;
    if (i_ptr->GetDS() != j_ptr->GetDS()) return FALSE;

    /* Hack -- Never stack chests */
    if (i_ptr->GetTval() == TV_CHEST) return (0);

    /* Require matching discounts */
    if (i_ptr->GetDiscount() != j_ptr->GetDiscount()) return (0);

    /* They match, so they must be similar */
    return TRUE;
}


/*
 * Allow a store item to absorb another item
 */
static void store_object_absorb(CItem *i_ptr, CItem *j_ptr)
{
    int total = i_ptr->GetNumber() + j_ptr->GetNumber();

    /* Combine quantity, lose excess items */
    i_ptr->SetNumber((total > 99) ? 99 : total);
}


/*
 * Check to see if the shop will be carrying too many objects   -RAK-   
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.
 */
static bool store_check_num(CItem *i_ptr)
{
    int i;
    CItem *j_ptr;

    /* Free space is always usable */
    if (st_ptr->stock_num < st_ptr->stock_size) return TRUE;

    /* The "home" acts like the player */
    if (store_num == 7) {
        /* Check all the items */
        for (i = 0; i < st_ptr->stock_num; i++) {
            /* Get the existing item */
            j_ptr = &st_ptr->stock[i];

            /* Can the new object be combined with the old one? */
            if (object_similar(j_ptr, i_ptr)) return TRUE;
        }
    }

    /* Normal stores do special stuff */
    else {
        /* Check all the items */
        for (i = 0; i < st_ptr->stock_num; i++) {
            /* Get the existing item */
            j_ptr = &st_ptr->stock[i];

            /* Can the new object be combined with the old one? */
            if (store_object_similar(j_ptr, i_ptr)) return (TRUE);
        }
    }

    /* But there was no room at the inn... */
    return FALSE;
}




/*
 * Determine if the current store will purchase the given item
 *
 * Note that a shop-keeper must refuse to buy "worthless" items
 */
static bool store_will_buy(CItem *i_ptr)
{
    /* Hack -- The Home is simple */
    if (store_num == 7) return TRUE;

    /* Switch on the store */
    switch (store_num) {
        /* General Store */
        case 0:

            /* Analyze the type */
            switch (i_ptr->GetTval()) {
                case TV_DIGGING:
                case TV_CLOAK:
                case TV_FOOD:
                case TV_FLASK:
                case TV_LITE:
                    break;
                default:
                    return FALSE;
            }
            break;

        /* Armorer */
        case 1:

            /* Analyze the type */
            switch (i_ptr->GetTval()) {
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
                    return FALSE;
            }
            break;

        /* Weapon Shop */
        case 2:

            /* Analyze the type */
            switch (i_ptr->GetTval()) {
                case TV_SHOT:
                case TV_BOLT:
                case TV_ARROW:
                case TV_BOW:
                case TV_DIGGING:
                case TV_HAFTED:
                case TV_POLEARM:
                case TV_SWORD:
                    break;
                default:
                    return FALSE;
            }
            break;

        /* Temple */
        case 3:

            /* Analyze the type */
            switch (i_ptr->GetTval()) {
                case TV_PRAYER_BOOK:
                case TV_SCROLL:
                case TV_POTION:
                case TV_HAFTED:
                    break;
                default:
                    return FALSE;
            }
            break;

        /* Alchemist */
        case 4:

            /* Analyze the type */
            switch (i_ptr->GetTval()) {
                case TV_SCROLL:
                case TV_POTION:
                    break;
                default:
                    return (FALSE);
            }
            break;

        /* Magic Shop */
        case 5:

            /* Analyze the type */
            switch (i_ptr->GetTval()) {
                case TV_MAGIC_BOOK:
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

    /* XXX XXX XXX Ignore "worthless" items */
    if (i_ptr->GetValue() <= 0) return FALSE;

    /* Assume okay */
    return TRUE;
}



/*
 * Add the item "i_ptr" to the inventory of the "Home"
 *
 * In all cases, return the slot (or -1) where the object was placed
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" bacoming
 * known, the player may have to pick stuff up and drop it again.
 */
static int home_carry(CItem *i_ptr)
{
    int slot, i;
    s32b value, j_value;
    CItem *j_ptr;


    /* Check each existing item (try to combine) */
    for (slot = 0; slot < st_ptr->stock_num; slot++) {
        /* Get the existing item */
        j_ptr = &st_ptr->stock[slot];

        /* The home acts just like the player */
        if (object_similar(j_ptr, i_ptr)) {
            /* Save the new number of items */
            object_absorb(j_ptr, i_ptr);

            /* All done */
            return (slot);
        }
    }

    /* No space? */
    if (st_ptr->stock_num >= st_ptr->stock_size) return -1;


    /* Determine the "value" of the item */
    value = i_ptr->GetValue();

    /* Check existing slots to see if we must "slide" */
    for (slot = 0; slot < st_ptr->stock_num; slot++) {
        /* Get that item */
        j_ptr = &st_ptr->stock[slot];

        /* Hack -- readable books always come first */
        if ((i_ptr->GetTval() == mp_ptr->spell_book) &&
            (j_ptr->GetTval() != mp_ptr->spell_book)) break;
        if ((j_ptr->GetTval() == mp_ptr->spell_book) &&
            (i_ptr->GetTval() != mp_ptr->spell_book)) continue;

        /* Items sort by decreasing type */
        if (i_ptr->GetTval() > j_ptr->GetTval()) break;
        if (i_ptr->GetTval() < j_ptr->GetTval()) continue;

        /* Can happen in the home */
        if (!i_ptr->isAware()) continue;
        if (!j_ptr->isAware()) break;

        /* Items sort by increasing sval */
        if (i_ptr->GetSval() < j_ptr->GetSval()) break;
        if (i_ptr->GetSval() > j_ptr->GetSval()) continue;

        /* Items in the home can be unknown */
        if (!i_ptr->isKnown()) continue;
        if (!j_ptr->isKnown()) break;

        /* Hack -- otherwise identical rods sort by increasing
           recharge time  -- dsb */
        if (i_ptr->GetTval() == TV_ROD) {
            if (i_ptr->GetPval() < j_ptr->GetPval()) break;
            if (i_ptr->GetPval() > j_ptr->GetPval()) continue;
        }

        /* Items sort by decreasing value */
        j_value = j_ptr->GetValue();
        if (value > j_value) break;
        if (value < j_value) continue;
    }

    // Slide the others up
    for (i = st_ptr->stock_num; i > slot; i--) {
        st_ptr->stock[i] = st_ptr->stock[i-1];
    }

    // More stuff now
    st_ptr->stock_num++;

    // Insert the new item
    st_ptr->stock[slot] = *i_ptr;

    // Return the location
    return slot;
}


/*
 * Add the item "i_ptr" to a real stores inventory.
 *
 * If the item is "worthless", it is thrown away (except in the home).
 *
 * If the item cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.  Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an item to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or -1) where the object was placed
 */
static int store_carry(CItem *i_ptr)
{
    int i, slot;
    s32b value, j_value;
    CItem *j_ptr;


    /* Evaluate the object */
    value = i_ptr->GetValue();

    /* Cursed/Worthless items "disappear" when sold */
    if (value <= 0) return -1;


    /* Erase the inscription */
    i_ptr->SetNote(NULL);

    /* Check each existing item (try to combine) */
    for (slot = 0; slot < st_ptr->stock_num; slot++) {
        /* Get the existing item */
        j_ptr = &st_ptr->stock[slot];

        /* Can the existing items be incremented? */
        if (store_object_similar(j_ptr, i_ptr)) {
            /* Hack -- extra items disappear */
            store_object_absorb(j_ptr, i_ptr);

            /* All done */
            return (slot);
        }
    }

    /* No space? */
    if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


    /* Check existing slots to see if we must "slide" */
    for (slot = 0; slot < st_ptr->stock_num; slot++) {
        /* Get that item */
        j_ptr = &st_ptr->stock[slot];

        /* Items sort by decreasing type */
        if (i_ptr->GetTval() > j_ptr->GetTval()) break;
        if (i_ptr->GetTval() < j_ptr->GetTval()) continue;

        /* Items sort by increasing sval */
        if (i_ptr->GetSval() < j_ptr->GetSval()) break;
        if (i_ptr->GetSval() > j_ptr->GetSval()) continue;

        /* Hack -- otherwise identical rods sort by increasing
           recharge time  -- dsb */
        if (i_ptr->GetTval() == TV_ROD) {
            if (i_ptr->GetPval() < j_ptr->GetPval()) break;
            if (i_ptr->GetPval() > j_ptr->GetPval()) continue;
        }

        /* Evaluate that slot */
        j_value = j_ptr->GetValue();

        /* Items sort by decreasing value */
        if (value > j_value) break;
        if (value < j_value) continue;
    }

    /* Slide the others up */
    for (i = st_ptr->stock_num; i > slot; i--) {
        st_ptr->stock[i] = st_ptr->stock[i-1];
    }

    /* More stuff now */
    st_ptr->stock_num++;

    /* Insert the new item */
    st_ptr->stock[slot] = *i_ptr;

    /* Return the location */
    return slot;
}


/*
 * Increase, by a given amount, the number of a certain item
 * in a certain store.  This can result in zero items.
 */
static void store_item_increase(int item, int num)
{
    int         cnt;
    CItem *i_ptr;

    /* Get the item */
    i_ptr = &st_ptr->stock[item];

    /* Verify the number */
    cnt = i_ptr->GetNumber() + num;
    if (cnt > 255) cnt = 255;
    else if (cnt < 0) cnt = 0;
    num = cnt - i_ptr->GetNumber();

    /* Save the new number */
    i_ptr->SetNumber(i_ptr->GetNumber() + num);
}


/*
 * Remove a slot if it is empty
 */
static void store_item_optimize(int item)
{
    int         j;
    CItem *i_ptr;

    /* Get the item */
    i_ptr = &st_ptr->stock[item];

    /* Must exist */
    if (!i_ptr->exists()) return;

    /* Must have no items */
    if (i_ptr->GetNumber()) return;

    /* One less item */
    st_ptr->stock_num--;

    /* Slide everyone */
    for (j = item; j < st_ptr->stock_num; j++) {
        st_ptr->stock[j] = st_ptr->stock[j + 1];
    }

    /* Nuke the final slot */
    st_ptr->stock[st_ptr->stock_num].wipe();
}


/*
 * This function will keep some crap out of the black market.
 * Crap is defined as any item that is possibly available elsewhere
 * Based on a suggestion by "Lee Vogt" <lvogt@cig.mcel.mot.com>
 * Somewhat modified by MJC
 */
static bool black_market_crap(CItem *i_ptr)
{
    int i, j;
    store_type *st_ptr;

    /* Ego items are never crap */
    if (i_ptr->isEgoItem()) return FALSE;

    /* Good items are never crap */
    if (i_ptr->GetToA() > 0) return FALSE;
    if (i_ptr->GetToH() > 0) return FALSE;
    if (i_ptr->GetToD() > 0) return FALSE;

    /* Check the "normal" stores */
    for (i = 0; i < 6; i++) {
        // Get the store pointer
        st_ptr = &store[i];

        // Look through the table
        for (j = 0; j < st_ptr->table_num; j++) {
            if (i_ptr->GetKIdx() == st_ptr->table[j]) return TRUE;
        }
    }

    /* Assume okay */
    return FALSE;
}


/*
 * Attempt to delete (some of) a random item from the store
 * Hack -- we attempt to "maintain" piles of items when possible.
 */
static void store_delete(void)
{
    int what, num;

    /* Pick a random slot */
    what = rand_int(st_ptr->stock_num);

    /* Determine how many items are here */
    num = st_ptr->stock[what].GetNumber();

    /* Hack -- sometimes, only destroy half the items */
    if (percent(50)) num = (num + 1) / 2;

    /* Hack -- sometimes, only destroy a single item */
    if (percent(50)) num = 1;

    /* Actually destroy (part of) the item */
    store_item_increase(what, -num);
    store_item_optimize(what);
}


/*
 * Creates a random item and gives it to a store
 * This algorithm needs to be rethought.  A lot.
 * Currently, "normal" stores use a pre-built array.
 *
 * Note -- the "level" given to "obj_get_num()" is a "favored"
 * level, that is, there is a much higher chance of getting
 * items with a level approaching that of the given level...
 *
 * Should we check for "permission" to have the given item?
 */
static void store_create(void)
{
    int i, tries, level;
    CItem tmp_obj, *i_ptr = &tmp_obj;

    /* Paranoia -- no room left */
    if (st_ptr->stock_num >= st_ptr->stock_size) return;

    /* Hack -- consider up to four items */
    for (tries = 0; tries < 4; tries++) {
        /* Black Market */
        if (store_num == 6) {
            /* Pick a level for object/magic */
            level = 25 + rand_int(25);

            /* Random item (usually of given level) */
            i = get_obj_num(level);

            /* Handle failure */
            if (!i) continue;
        }

        /* Normal Store */
        else {
            /* Hack -- Pick an item to sell */
            i = st_ptr->table[rand_int(st_ptr->table_num)];

            /* Hack -- fake level for apply_magic() */
            level = rand_range(1, STORE_OBJ_LEVEL);
        }


        /* Create a new object of the chosen kind */
        i_ptr->invcopy(i);

        // Apply some "low-level" magic (no artifacts, force neither good/great)
        i_ptr->apply_magic(level, 0);

        /* Hack -- Charge lite's */
        if (i_ptr->GetTval() == TV_LITE) {
            if (i_ptr->GetSval() == SV_LITE_TORCH) {
                i_ptr->SetPval(FUEL_TORCH / 2);
            }
            if (i_ptr->GetSval() == SV_LITE_LANTERN) {
                i_ptr->SetPval(FUEL_LAMP / 2);
            }
        }


        /* The item is "known" */
        i_ptr->MakeKnown();

        /* Mega-Hack -- no chests in stores */
        if (i_ptr->GetTval() == TV_CHEST) continue;

        /* Prune the black market */
        if (store_num == 6) {
            /* Hack -- No "crappy" items */
            if (black_market_crap(i_ptr)) continue;

            /* Hack -- No "cheap" items */
            if (i_ptr->GetValue() < 10) continue;
        }

        /* Prune normal stores */
        else {
            /* No "worthless" items */
            if (i_ptr->GetValue() <= 0) continue;
        }


        /* Mass produce and/or Apply discount */
        mass_produce(i_ptr);

        /* Attempt to carry the (known) item */
        store_carry(i_ptr);

        /* Definitely done */
        break;
    }
}



/*
 * Re-displays a single store entry
 */
static void display_entry(int pos)
{
    int i, maxwid = 75;
    CItem *i_ptr;
    s32b x;
    char i_name[80], out_val[160];

    /* Get the item */
    i_ptr = &st_ptr->stock[pos];

    /* Get the "offset" */
    i = pos - store_top;

    // Label it --(--
    sprintf(out_val, "%c)", I2A(i));
    put_string(0, (i+6)*16, out_val, COLOR_WHITE);

    /* Describe an item in the home */
    if (store_num == 7) {
        maxwid = 75;

        /* Leave room for weights, if necessary -DRS- */
        if (show_inven_weight) maxwid -= 10;

        /* Describe the object */
        i_ptr->object_desc(i_name, TRUE, 3);
        i_name[maxwid] = '\0';
        put_text(3*8, (i+6)*16, i_name, i_ptr->get_attr(), FONT_BOLD);
        
        // Show weights, if turned on -DRS-
        if (show_inven_weight) {
            // Only show the weight of an individual item
            int wgt = i_ptr->GetWeight();
            sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
            put_string(68*8, (i+6)*16, out_val, COLOR_WHITE);
        }
    }

    // Describe an item (fully) in a store
    else {
        // Must leave room for the "price"
        maxwid = 65;

        // Leave room for weights, if necessary -DRS-
        if (show_store_weight) maxwid -= 7;

        // Describe the object (fully)
        i_ptr->object_desc_store(i_name, TRUE, 3);
        i_name[maxwid] = '\0';
        put_text(3*8, (i+6)*16, i_name, i_ptr->get_attr(), FONT_BOLD);

        /* Show weights, if turned on -DRS- */
        if (show_store_weight) {
            /* Only show the weight of an individual item */
            int wgt = i_ptr->GetWeight();
            sprintf(out_val, "%3d.%d", wgt / 10, wgt % 10);
            put_string(61*8, (i+6)*16, out_val, COLOR_WHITE);
        }

        /* Extract the "minimum" price */
        x = price_item(i_ptr, ot_ptr->min_inflate, FALSE);

        /* Actually draw the price */
        sprintf(out_val, "%9ld", (long)x);
        put_string(68*8, (i+6)*16, out_val, COLOR_WHITE);
    }
}


/*
 * Displays a store's inventory                 -RAK-   
 * All prices are listed as "per individual object".  -BEN-
 */
static void display_inventory(void)
{
    int k;

    /* Display the next 12 items */
    for (k = 0; k < 12; k++) {
        /* Do not display "dead" items */
        if (store_top + k >= st_ptr->stock_num) break;

        /* Display that line */
        display_entry(store_top + k);
    }

    /* Visual reminder of "more items" */
    if (st_ptr->stock_num > 12) {
        /* Show "more" reminder (after the last item) */
        put_text(3*8, (k+6)*16, "-more-", COLOR_WHITE, FONT_BOLD);
    }
}


/*
 * Displays store (after clearing screen)               -RAK-   
 */
static void redraw_store(void)
{
    char buf[80];


    /* The "Home" is special */
    if (store_num == 7) {
        /* Put the owner name */
        put_text(30*8, 3*16, "Your Home", COLOR_WHITE, FONT_BOLD);

        /* Label the item descriptions */
        put_text(3*8, 5*16, "Item Descriptions", COLOR_WHITE, FONT_BOLD);

        /* If showing weights, show label */
        if (show_inven_weight) {
            put_string(70*8, 5*16, "Weight", COLOR_WHITE);
        }
    }

    /* Normal stores */
    else {
        char *store_name = (f_name + f_info[0x08 + store_num].name);
        char *owner_name = (ot_ptr->owner_name);
        char *race_name = race_info[ot_ptr->owner_race].title;

        /* Put the owner name and race */
        sprintf(buf, "%s (%s)", owner_name, race_name);
        put_text(10*8, 3*16, buf, COLOR_WHITE, FONT_BOLD);

        /* Show the max price in the store (above prices) */
        sprintf(buf, "%s (%ld)", store_name, (long)(ot_ptr->max_cost));
        put_text(50*8, 3*16, buf, COLOR_WHITE, FONT_BOLD);

        /* Label the item descriptions */
        put_text(3*8, 5*16, "Item Descriptions", COLOR_WHITE, FONT_BOLD);

        /* If showing weights, show label */
        if (show_store_weight) {
            put_string(60*8, 5*16, "Weight", COLOR_WHITE);
        }

        /* Label the asking price (in stores) */
        put_text(77*8-string_width("Price", FONT_BOLD), 5*16, "Price", COLOR_WHITE, FONT_BOLD);

        /* Display the current gold */
        put_string(53*8, 19*16, "Gold Remaining: ", COLOR_WHITE);
        sprintf(buf, "%9ld", (long)p_ptr->GetGold());
        put_string(68*8, 19*16, buf, COLOR_WHITE);
    }

    /* Draw in the inventory */
    display_inventory();

    // Basic commands
    put_text(1*8, 22*16, "ESC) Exit from Building.", COLOR_WHITE, FONT_BOLD);

    // Browse if necessary
    if (st_ptr->stock_num > 12) {
        put_text(1*8, 23*16, "Up, Down) Browse", COLOR_WHITE, FONT_BOLD);
    }

    // Home commands
    if (store_num == 7) {
        put_text(41*8, 22*16, "g) Get an item.", COLOR_WHITE, FONT_BOLD);
        put_text(41*8, 23*16, "d) Drop an item.", COLOR_WHITE, FONT_BOLD);
    }

    // Shop commands XXX XXX XXX
    else {
        put_text(41*8, 22*16, "p) Purchase an item.", COLOR_WHITE, FONT_BOLD);
        put_text(41*8, 23*16, "s) Sell an item.", COLOR_WHITE, FONT_BOLD);
    }
}



/*
 * Get the ID of a store item and return its value      -RAK-   
 */
static int get_stock(int *com_val, char *pmt, int i, int j)
{
    char command;
    char out_val[160];


    // Assume failure
    *com_val = (-1);

    // Build the prompt
    sprintf(out_val, "(Items %c-%c, ESC to exit) %s", I2A(i), I2A(j), pmt);

    // Ask until done
    while (TRUE) {
        int k;

        // Escape
        if (!get_com(out_val, &command)) break;

        // Convert
        k = (islower(command) ? A2I(command) : -1);

        // Legal responses
        if ((k >= i) && (k <= j)) {
            *com_val = k;
            break;
        }

        // Oops
        bell();
    }

    /* Clear the prompt */
    box(0, 0, 639, 15, COLOR_BLACK);

    /* Cancel */
    if (command == ESCAPE) return (FALSE);

    /* Success */
    return (TRUE);
}


/*
 * Buy an item from a store                             -RAK-   
 */
static void store_purchase(void)
{
    int i, amt, item, item_new;
    s32b price, best;
    CItem sell_obj, *i_ptr;
    char i_name[80], out_val[160];


    // Empty?
    if (st_ptr->stock_num <= 0) {
        if (store_num == 7) msg_p_old("Your home is empty.");
        else msg_p_old("I am currently out of stock.");
        return;
    }


    /* Find the number of objects on this and following pages */
    i = st_ptr->stock_num - store_top;

    /* And then restrict it to the current page */
    if (i > 12) i = 12;

    /* Prompt */
    if (store_num == 7) {
        sprintf(out_val, "Which item do you want to take?");
    }
    else {
        sprintf(out_val, "Which item are you interested in?");
    }

    /* Get the item number to be bought */
    if (!get_stock(&item, out_val, 0, i-1)) return;

    /* Get the actual index */
    item = item + store_top;

    /* Get the actual item */
    i_ptr = &st_ptr->stock[item];

    /* Assume the player wants just one of them */
    amt = 1;

    /* Hack -- get a "sample" object */
    sell_obj = *i_ptr;
    sell_obj.SetNumber(amt);

    /* Hack -- require room in pack */
    if (!inven_carry_okay(&sell_obj)) {
        msg_p_old("You cannot carry that many different items.");
        return;
    }

    /* Determine the "best" price (per item) */
    best = price_item(&sell_obj, ot_ptr->min_inflate, FALSE);

    /* Find out how many the player wants */
    if (i_ptr->isPlural()) {
        /* Get a quantity */
        amt = get_quantity(NULL, i_ptr->GetNumber());

        /* Allow user abort */
        if (amt <= 0) return;
    }

    /* Create the object to be sold (structure copy) */
    sell_obj = *i_ptr;
    sell_obj.SetNumber(amt);

    /* Hack -- require room in pack */
    if (!inven_carry_okay(&sell_obj)) {
        msg_p_old("You cannot carry that many items.");
        return;
    }

    // Attempt to buy it
    if (store_num != 7) {
        // Go directly to the "best" deal
        price = best * sell_obj.GetNumber();

        // Player wants it
        if (TRUE) {
            // Player can afford it
            if (p_ptr->GetGold() >= price) {
                // Spend the money
                p_ptr->SetGold(p_ptr->GetGold() - price);

                // Hack -- buying an item makes you aware of it
                sell_obj.object_aware();

                // Describe the transaction
                sell_obj.object_desc(i_name, TRUE, 3);

                // Message
                msg_f_old("You bought %s for %ld gold.", i_name, (long)price);

                /* Let the player carry it (as if he picked it up) */
                item_new = inven_carry(&sell_obj);

                // Describe the final result
                inventory[item_new].object_desc(i_name, TRUE, 3);

                // Message
                msg_f_old("You have %s (%c).",
                    i_name, index_to_label(item_new));

                // Update stuff
                update_stuff();

                // Note how many slots the store used to have
                i = st_ptr->stock_num;

                // Remove the bought items from the store
                store_item_increase(item, -amt);
                store_item_optimize(item);

                // Item is still here
                if (i == st_ptr->stock_num) {
                }

                /* The item is gone */
                else {
                    // Nothing left
                    if (st_ptr->stock_num == 0) store_top = 0;

                    // Nothing left on that screen
                    else if (store_top >= st_ptr->stock_num) store_top -= 12;
                }
            }

            /* Player cannot afford it */
            else {
                /* Simple message (no insult) */
                msg_p_old("You do not have enough gold.");
            }
        }
    }

    /* Home is much easier */
    else {
        /* Carry the item */
        item_new = inven_carry(&sell_obj);

        /* Describe just the result */
        inventory[item_new].object_desc(i_name, TRUE, 3);

        /* Message */
        msg_f_old("You have %s (%c).", i_name, index_to_label(item_new));

        /* Update stuff */
        update_stuff();

        /* Take note if we take the last one */
        i = st_ptr->stock_num;

        /* Remove the items from the home */
        store_item_increase(item, -amt);
        store_item_optimize(item);

        /* Hack -- Item is still here */
        if (i == st_ptr->stock_num) {
        }

        /* The item is gone */
        else {
            /* Nothing left */
            if (st_ptr->stock_num == 0) store_top = 0;

            /* Nothing left on that screen */
            else if (store_top >= st_ptr->stock_num) store_top -= 12;
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
    int item, item_pos, amt;
    s32b price, value, dummy;
    CItem sold_obj, *i_ptr;
    char *pmt = "Sell which item? ";
    char i_name[80];

    /* Prepare a prompt */
    if (store_num == 7) pmt = "Drop which item? ";

    /* Only allow items the store will buy */
    item_tester_hook = store_will_buy;

    /* Get an item (from equip or inven) */
    if (!get_item(&item, pmt, GI_EQUIP | GI_INVEN)) {
        if (item == -2) msg_p_old("You have nothing that I want.");
        return;
    }

    /* Get the item (in the pack) */
    i_ptr = gi_i_ptr;


    /* Assume one item */
    amt = 1;

    /* Find out how many the player wants (letter means "all") */
    if (i_ptr->isPlural()) {
        /* Get a quantity */
        amt = get_quantity(NULL, i_ptr->GetNumber());

        /* Allow user abort */
        if (amt <= 0) return;
    }

    // Check for curse
    if ((item >= INVEN_WIELD) && i_ptr->isCursed()) {
        msg_p_old("Hmmm, it seems to be cursed.");
        return;
    }

    // Create the object to be sold (structure copy)
    sold_obj = *i_ptr;
    sold_obj.SetNumber(amt);

    // Get a full description
    sold_obj.object_desc(i_name, TRUE, 3);

    // Remove any inscription for stores
    if (store_num != 7) sold_obj.SetNote(NULL);

    /* Is there room in the store (or the home?) */
    if (!store_check_num(&sold_obj)) {
        if (store_num == 7) msg_p_old("Your home is full.");
        else msg_p_old("I have not the room in my store to keep it.");
        return;
    }


    /* Real store */
    if (store_num != 7) {
        char out_val[120];

        // Price it
        price = price_item(&sold_obj, ot_ptr->min_inflate, TRUE);
        if (price > ot_ptr->max_cost) price = ot_ptr->max_cost;
        price *= sold_obj.GetNumber();

        // Describe the transaction
        sprintf(out_val, "Sell %s (%c) for %d gold?", i_name,
            index_to_label(item), price);

        // Sell it
        if (get_check(out_val)) {
            // Get some money
            p_ptr->SetGold(p_ptr->GetGold() + price);

            // Get the inventory item
            i_ptr = &inventory[item];

            // Get the "apparent" value
            dummy = sold_obj.GetValue() * sold_obj.GetNumber();

            // Become "aware" of the item
            i_ptr->object_aware();

            // Know the item fully
            i_ptr->MakeKnown();

            // Re-Create the now-identified object that was sold
            sold_obj = *i_ptr;
            sold_obj.SetNumber(amt);

            /* Get the "actual" value */
            value = sold_obj.GetValue() * sold_obj.GetNumber();

            /* Get the description all over again */
            sold_obj.object_desc(i_name, TRUE, 3);

            /* Describe the result (in message buffer) */
            msg_f_old("You sold %s for %ld gold.", i_name, (long)price);

            /* Analyze the prices (and comment verbally) */
            purchase_analyze(price, value, dummy);

            // Take the item from the player, describe the result
            inven_item_increase(item, -amt);
            inven_item_describe(item);
            inven_item_optimize(item);

            // Update stuff
            update_stuff();

            // The store gets that (known) item
            item_pos = store_carry(&sold_obj);

            // Re-display if item is now in store
            if (item_pos >= 0) {
                store_top = (item_pos / 12) * 12;
            }
        }
    }

    /* Player is at home */
    else {
        /* Describe */
        msg_f_old("You drop %s.", i_name);

        /* Take it from the players inventory */
        inven_item_increase(item, -amt);
        inven_item_describe(item);
        inven_item_optimize(item);

        /* Update stuff */
        update_stuff();

        /* Let the store (home) carry it */
        item_pos = home_carry(&sold_obj);

        /* Update store display */
        if (item_pos >= 0) {
            store_top = (item_pos / 12) * 12;
        }
    }
}



/*
 * Hack -- set this to leave the store
 */
static bool leave_store = FALSE;


/*
 * Process a command in a store
 */
static void store_process_command(void)
{
    char command_cmd = scan_inkey_scan();

    // Parse the command
    switch (command_cmd) {
        // Leave
        case KEY_ESCAPE: leave_store = TRUE; break;

        // Up
        case KEY_UP:
            if (store_top > 0) store_top--;
            break;

        // Down
        case KEY_DOWN:
            if (store_top < st_ptr->stock_num-12) store_top++;
            break;

        // Get/purchase
        case KEY_G:
        case KEY_P:
            store_purchase();
            break;

        // Drop (Sell)
        case KEY_D:
        case KEY_S:
            store_sell();
            break;



        /*** Inventory Commands ***/

        // Equipment list 
        case KEY_E: draw_equip_stuff(); screen_refresh(); wait_for_key(); break;

        // Inventory list
        case KEY_I: draw_inven_stuff(); screen_refresh(); wait_for_key(); break;
    }
}


/*
 * Enter a store, and interact with it.
 */
void do_cmd_store(void)
{
    int which;
    CGrid *g_ptr;


    // Access the player grid
    g_ptr = p_ptr->get_g_ptr();

    // Verify a store
    if (!g_ptr->is_store_door()) {
        msg_print("You see no store here.");
        return;
    }

    // Extract the store type
    which = g_ptr->get_store_type();

    /* Hack -- Check the "locked doors" */
    if (store[which].store_open >= game_turn) {
        msg_print("The doors are locked.");
        return;
    }


    /* Hack -- Character is in "icky" mode */
    character_icky = TRUE;


    // Save the store number
    store_num = which;

    // Save the store and owner pointers
    st_ptr = &store[store_num];
    ot_ptr = &owners[store_num][st_ptr->owner];


    // Start at the beginning
    store_top = 0;

    // Do not leave
    leave_store = FALSE;

    // Interact with player
    while (!leave_store) {
        // Blank the screen
        blank_screen(COLOR_BLACK);

        // Draw everything
        redraw_store();

        // Update screen
        mouse_refresh();

        // Process commands
        store_process_command();

        /* Hack -- Character is still in "icky" mode */
        character_icky = TRUE;

        /* Notice stuff */
        notice_stuff();

        /* Update stuff */
        update_stuff();

        /* XXX XXX XXX Pack Overflow */
        if (inventory[INVEN_PACK].exists()) {
            /* Hack -- Flee from the store */
            if (store_num != 7) {
                /* Message */
                mini_message_box("Note", "Your pack is so full that you flee the store...");

                /* Leave */
                leave_store = TRUE;
            }

            /* Hack -- Flee from the home */
            else if (!store_check_num(&inventory[INVEN_PACK])) {
                /* Message */
                mini_message_box("Note", "Your pack is so full that you flee your home...");

                /* Leave */
                leave_store = TRUE;
            }

            /* Hack -- Drop items into the home */
            else {
                int item_pos;
                CItem sold_obj;
                char i_name[80];

                // Grab a copy of the item
                sold_obj = inventory[INVEN_PACK];

                // Give a message
                msg_p_old("Your pack overflows!");

                // Describe it
                sold_obj.object_desc(i_name, TRUE, 3);

                // Message
                msg_f_old("You drop %s.", i_name);

                // Remove it from the players inventory
                inven_item_increase(INVEN_PACK, -999);
                inven_item_describe(INVEN_PACK);
                inven_item_optimize(INVEN_PACK);

                // Update stuff
                update_stuff();

                // Let the store (home) carry it
                item_pos = home_carry(&sold_obj);
            }
        }

        // Hack -- get kicked out of the store
        if (st_ptr->store_open >= game_turn) leave_store = TRUE;
    }


    // Forget the store number, etc
    store_num = 0;
    st_ptr = NULL;
    ot_ptr = NULL;


    // Hack -- Character is no longer in "icky" mode
    character_icky = FALSE;


    // Update stuff
    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_MONSTERS);
}



/*
 * Shuffle one of the stores.
 */
void store_shuffle(void)
{
    int i, j, n;

    /* Pick a real store to shuffle */
    n = rand_int(MAX_STORES - 1);

    /* Save the store index */
    store_num = n;

    /* Activate that store */
    st_ptr = &store[store_num];

    /* Pick a new owner */
    for (j = st_ptr->owner; j == st_ptr->owner; ) {
        st_ptr->owner = rand_int(MAX_OWNERS);
    }

    /* Activate the new owner */
    ot_ptr = &owners[store_num][st_ptr->owner];

    /* Reset the owner data */
    st_ptr->insult_cur = 0;
    st_ptr->store_open = 0;
    st_ptr->good_buy = 0;
    st_ptr->bad_buy = 0;

    /* Hack -- discount all the items */
    for (i = 0; i < st_ptr->stock_num; i++) {
        CItem *i_ptr;

        /* Get the item */
        i_ptr = &st_ptr->stock[i];

        /* Hack -- Sell all old items for "half price" */
        i_ptr->SetDiscount(50);

        /* Hack -- Items are no longer "fixed price" */
        i_ptr->ClearIdentFlag(ID_FIXED);

        /* Mega-Hack -- Note that the item is "on sale" */
        i_ptr->SetNote("on sale");
    }

    /* Turn it all off */
    store_num = 0;
    st_ptr = NULL;
    ot_ptr = NULL;
}


/*
 * Maintain the inventory at the stores.
 */
void store_maint(void)
{
    int i, j;

    /* Maintain every store (except the home) */
    for (i = 0; i < (MAX_STORES - 1); i++) {
        /* Save the store index */
        store_num = i;

        /* Activate that store */
        st_ptr = &store[store_num];

        /* Activate the new owner */
        ot_ptr = &owners[store_num][st_ptr->owner];


        /* Store keeper forgives the player */
        st_ptr->insult_cur = 0;


        /* Mega-Hack -- prune the black market */
        if (store_num == 6) {
            /* Destroy crappy black market items */
            for (j = st_ptr->stock_num - 1; j >= 0; j--) {
                CItem *i_ptr = &st_ptr->stock[j];

                /* Destroy crappy items */
                if (black_market_crap(i_ptr)) {
                    /* Destroy the item */
                    store_item_increase(j, -i_ptr->GetNumber());
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

        /* Acquire some new items */
        while (st_ptr->stock_num < j) store_create();
    }


    /* Turn it all off */
    store_num = 0;
    st_ptr = NULL;
    ot_ptr = NULL;
}


/*
 * Initialize the stores
 */
void store_init(void)
{
    int j, k;


    /* Build each store */
    for (j = 0; j < MAX_STORES; j++) {
        /* Save the store index */
        store_num = j;

        /* Activate that store */
        st_ptr = &store[store_num];


        /* Pick an owner */
        st_ptr->owner = rand_int(MAX_OWNERS);

        /* Activate the new owner */
        ot_ptr = &owners[store_num][st_ptr->owner];


        /* Initialize the store */
        st_ptr->store_open = 0;
        st_ptr->insult_cur = 0;
        st_ptr->good_buy = 0;
        st_ptr->bad_buy = 0;

        /* Nothing in stock */
        st_ptr->stock_num = 0;

        /* Clear any old items */
        for (k = 0; k < st_ptr->stock_size; k++) {
            st_ptr->stock[k].wipe();
        }
    }


    /* Turn it all off */
    store_num = 0;
    st_ptr = NULL;
    ot_ptr = NULL;
}

