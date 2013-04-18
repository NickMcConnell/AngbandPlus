/* File: borg8.c */
/* Purpose: High level functions for the Borg -BEN- */

#include "angband.h"

#ifdef ALLOW_BORG

#include "borg1.h"
#include "borg2.h"
#include "borg3.h"
#include "borg4.h"
#include "borg5.h"
#include "borg6.h"
#include "borg7.h"
#include "borg8.h"

byte *test;
byte *best;
s32b *b_home_power;


/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow wands/staffs (if they are known to have equal
 * charges) and rods (if fully charged) to combine.
 *
 * Note that rods/staffs/wands are then unstacked when they are used.
 *
 * If permitted, we allow weapons/armor to stack, if they both known.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests never stack (for various reasons).
 *
 * We do NOT allow activatable items (artifacts or dragon scale mail)
 * to stack, to keep the "activation" code clean.  Artifacts may stack,
 * but only with another identical artifact (which does not exist).
 *
 * Ego items may stack as long as they have the same ego-item type.
 * This is primarily to allow ego-missiles to stack.
 */
static bool borg_object_similar(auto_item  *o_ptr, auto_item  *j_ptr)
{
    /* NOTE: This assumes the giving of one item at a time */
    int total = o_ptr->iqty + 1;


    /* Require identical object types */
    if (o_ptr->kind != j_ptr->kind) return (0);


    /* Analyze the items */
    switch (o_ptr->tval)
    {
        /* Chests */
        case TV_CHEST:
        {
            /* Never okay */
            return (0);
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
            /* Require knowledge */
            if ((!o_ptr->able) || (!j_ptr->able)) return (0);

            /* Fall through */
        }

        /* Staffs and Wands and Rods */
        case TV_ROD:
        {
            /* Require permission */
/*            if (!testing_stack) return (0);*/

            /* Require identical charges */
            if (o_ptr->pval != j_ptr->pval) return (0);

            /* Probably okay */
            break;
        }

        /* Weapons and Armor */
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
        {
            /* Require permission */
/*            if (!testing_stack) return (0);*/

            /* XXX XXX XXX Require identical "sense" status */
            /* if ((o_ptr->ident & ID_SENSE) != */
            /*     (j_ptr->ident & ID_SENSE)) return (0); */

            /* Fall through */
        }

        /* Rings, Amulets, Lites */
        case TV_RING:
        case TV_AMULET:
        case TV_LITE:
        {
            /* Require full knowledge of both items */
            if ((!o_ptr->able) || (!j_ptr->able)) return (0);

            /* Fall through */
        }

        /* Missiles */
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            /* Require identical "bonuses" */
            if (o_ptr->to_h != j_ptr->to_h) return (FALSE);
            if (o_ptr->to_d != j_ptr->to_d) return (FALSE);
            if (o_ptr->to_a != j_ptr->to_a) return (FALSE);

            /* Require identical "pval" code */
            if (o_ptr->pval != j_ptr->pval) return (FALSE);

            /* Require identical "artifact" names */
            if (o_ptr->name1 != j_ptr->name1) return (FALSE);

            /* Require identical "ego-item" names */
            if (o_ptr->name2 != j_ptr->name2) return (FALSE);

            /* Hack -- Never stack "powerful" items */
            if (o_ptr->flags1 || j_ptr->flags1) return (FALSE);
            if (o_ptr->flags2 || j_ptr->flags2) return (FALSE);
            if (o_ptr->flags3 || j_ptr->flags3) return (FALSE);

            /* Hack -- Never stack recharging items */
            if (o_ptr->timeout || j_ptr->timeout) return (FALSE);

            /* Require identical "values" */
            if (o_ptr->ac != j_ptr->ac) return (FALSE);
            if (o_ptr->dd != j_ptr->dd) return (FALSE);
            if (o_ptr->ds != j_ptr->ds) return (FALSE);

            /* Probably okay */
            break;
        }

        /* Various */
        default:
        {
            /* Require knowledge */
            if ((!o_ptr->able) || (!j_ptr->able)) return (0);

            /* Probably okay */
            break;
        }
    }


    /* Hack -- Require identical "broken" status */
    if ((o_ptr->fully_identified) != (j_ptr->fully_identified)) return (0);

    /* The stuff with 'note' is not right but it is close.  I think it */
    /* has him assuming that he can't stack sometimes when he can.  This */
    /* is alright, it just causes him to take a bit more time to do */
    /* some exchanges. */
    /* Hack -- require semi-matching "inscriptions" */
    if (o_ptr->note[0] && j_ptr->note[0] &&
        (!streq(o_ptr->note, j_ptr->note)))
        return (0);

    /* Hack -- normally require matching "inscriptions" */
    if (!stack_force_notes && (!streq(o_ptr->note, j_ptr->note))) return (0);

    /* Hack -- normally require matching "discounts" */
    if (!stack_force_costs && (o_ptr->discount != j_ptr->discount)) return (0);


    /* Maximal "stacking" limit */
    if (total >= MAX_STACK_SIZE) return (0);


    /* They match, so they must be similar */
    return (TRUE);
}

/*
 * Find the mininum amount of some item to buy/sell. For most
 * items this is 1, but for certain items (such as ammunition)
 * it may be higher.  -- RML
 */
static int borg_min_item_quantity(auto_item *item)
{
    /* Only trade in bunches if sufficient cash */
    if (auto_gold < 250) return (1);

    /* Don't trade expensive items in bunches */
    if (item->cost > 2) return (1);

    /* Don't trade non-known items in bunches */
    if (!item->able) return (1);

    /* Only allow some types */
    switch (item->tval)
    {
    case TV_SPIKE:
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
        /* Maximum number of items */
        if (item->iqty < 5)
            return (item->iqty);
        return (5);
#if 0
    case TV_FOOD:
        if (item->iqty < 3)
            return (item->iqty);
        return (3);
    case TV_POTION:
    case TV_SCROLL:
        if (item->iqty < 2)
            return (item->iqty);
        return (2);
#endif

    default:
        return (1);
    }
}

/*
 * This file handles the highest level goals, and store interaction.
 *
 * Store interaction strategy
 *
 *   (1) Sell items to the home (for later use)
 ** optimize the stuff in the home... this involves buying and selling stuff
 ** not in the 'best' list.
 *       We sell anything we may need later (see step 4)
 *
 *   (2) Sell items to the shops (for money)
 *       We sell anything we do not actually need
 *
 *   (3) Buy items from the shops (for the player)
 *       We buy things that we actually need
 *
 *   (4) Buy items from the home (for the player)
 *       We buy things that we actually need (see step 1)
 *
 *   (5) Buy items from the shops (for the home)
 *       We buy things we may need later (see step 1)
 *
 *   (6) Buy items from the home (for the stores)
 *       We buy things we no longer need (see step 2)
 *
 *   The basic principle is that we should always act to improve our
 *   "status", and we should sometimes act to "maintain" our status,
 *   especially if there is a monetary reward.  But first we should
 *   attempt to use the home as a "stockpile", even though that is
 *   not worth any money, since it may save us money eventually.
 */
#if 0
/* this optimized the home storage by trying every combination... it was too slow.*/
/* put this code back when running this on a Cray. */
static void borg_think_home_sell_aux2(  int n, int start_i )
{
    int i;

    /* All done */
    if (n == STORE_INVEN_MAX)
    {
        s32b home_power;

        /* Examine the home  */
        borg_notice_home(NULL, FALSE);

        /* Evaluate the home */
        home_power = borg_power_home();

        /* Track best */
        if (home_power > *b_home_power)
        {
            /* Save the results */
            for (i = 0; i < STORE_INVEN_MAX; i++) best[i] = test[i];

#if 0
            /* dump, for debugging */
            borg_note(format("Trying Combo (best home power %ld)",
                              *b_home_power));
            borg_note(format("             (test home power %ld)",home_power));
            for (i = 0; i < STORE_INVEN_MAX; i++)
            {
                if (auto_shops[7].ware[i].iqty)
                    borg_note(format("store %d %s (qty-%d).",  i,
                                       auto_shops[7].ware[i].desc,
                                       auto_shops[7].ware[i].iqty ));
                else
                    borg_note(format("store %d (empty).",  i));
            }
            borg_note(" "); /* add a blank line */
#endif

            /* Use it */
            *b_home_power = home_power;
        }

        /* Success */
        return;
    }

    /* Note the attempt */
    test[n] = n;

    /* Evaluate the default item */
    borg_think_home_sell_aux2(n + 1, start_i );

    /* if this slot and the previous slot is empty, move on to previous slot*/
    /* this will prevent trying a thing in all the empty slots to see if */
    /* empty slot b is better than empty slot a.*/
    if ((n != 0) && !auto_shops[7].ware[n].iqty && !auto_shops[7].ware[n-1].iqty)
        return;

    /* try other combinations */
    for (i = start_i; i < INVEN_PACK; i++)
    {
        auto_item *item;
        auto_item *item2;
        bool stacked = FALSE;

        item = &auto_items[i];
        item2= &auto_shops[7].ware[n];

        /* Skip empty items */
        /* Require "aware" */
        /* Require "known" */
        if (!item->iqty || !item->kind || !item->able)
            continue;

        /* Hack -- ignore "worthless" items */
        if (!item->value) continue;
        if (i==weapon_swap && weapon_swap !=0) continue;
        if (i==armour_swap && armour_swap !=0) continue;

        /* stacking? */
        if (borg_object_similar(item2, item))
        {
            item2->iqty++;
            item->iqty--;
            stacked = TRUE;
        }
        else
        {
            int k;
            bool found_match = FALSE;

            /* eliminate items that would stack else where in the list. */
            for (k = 0; k < STORE_INVEN_MAX; k++)
            {
                if (borg_object_similar(&safe_home[k], item))
                {
                    found_match = TRUE;
                    break;
                }
            }
            if (found_match)
                continue;

            /* replace current item with this item */
            COPY(item2, item, auto_item);

            /* only move one into a non-stack slot */
            item2->iqty = 1;

            /* remove item from pack */
            item->iqty--;
        }

        /* Note the attempt */
        test[n] = i + STORE_INVEN_MAX;

        /* Evaluate the possible item */
        borg_think_home_sell_aux2( n + 1, i+1 );

        /* restore stuff */
        COPY(item2, &safe_home[n], auto_item);

        /* put item back into pack */
        item->iqty++;
    }
}

#endif
/*
 * this will see what single addition/substitution is best for the home.
 * The formula is not as nice as the one above because it will
 * not check all possible combinations of items. but it is MUCH faster.
 */
static void borg_think_home_sell_aux2(  int n, int start_i )
{
    auto_item *item;
    auto_item *item2;
    s32b home_power;
    int i, k;
    bool stacked = FALSE;

    /* get the starting best (current) */
    /* Examine the home  */
    borg_notice_home(NULL, FALSE);

    /* Evaluate the home  */
    *b_home_power = borg_power_home();

    /* try individual substitutions/additions.   */
    for (n = 0; n < STORE_INVEN_MAX; n++)
    {
        item2 = &auto_shops[7].ware[n];
        for (i = 0; i < INVEN_PACK; i++)
        {
            item = &auto_items[i];

            /* Skip empty items */
            /* Require "aware" */
            /* Require "known" */

            if (!item->iqty || !item->kind || !item->able)
                continue;
            if (i==weapon_swap && weapon_swap !=0) continue;
            if (i==armour_swap && armour_swap !=0) continue;

            /* Do not dump stuff at home that is not fully id'd and should be  */
            /* this is good with random artifacts. */
            if (!item->fully_identified && item->name1) continue;

            /* Hack -- ignore "worthless" items */
            if (!item->value) continue;

            /* Dragons should not store some things in the home */
            if (auto_race >= RACE_MIN_DRAGON &&
                 ((item->tval == TV_HAFTED) ||
                  (item->tval == TV_POLEARM) ||
                  (item->tval == TV_SWORD) ||
                  (item->tval == TV_DIGGING) ||
                  (item->tval == TV_BOOTS) ||
                  (item->tval == TV_GLOVES) ||
                  (item->tval == TV_DRAG_ARMOR) ||
                  (item->tval == TV_HARD_ARMOR) ||
                  (item->tval == TV_SOFT_ARMOR) ||
                  (item->tval == TV_SHIELD) ||
                  (item->tval == TV_BOW) ||
                  (item->tval == TV_HELM) ))
                  {
                    continue;
                  }

            /* stacking? */
            if (borg_object_similar(item2, item))
            {
                /* if this stacks with what was previously here */
                item2->iqty++;
                stacked = TRUE;
            }
            else
            {
                bool found_match = FALSE;

                /* eliminate items that would stack else where in the list. */
                for (k = 0; k < STORE_INVEN_MAX; k++)
                {
                    if (borg_object_similar(&safe_home[k], item))
                    {
                        found_match = TRUE;
                        break;
                    }
                }
                if (found_match)
                    continue;

                /* replace current item with this item */
                COPY(item2, item, auto_item);

                /* only move one into a non-stack slot */
                item2->iqty = 1;
            }

            /* remove item from pack */
            item->iqty--;

            /* Note the attempt */
            test[n] = i + STORE_INVEN_MAX;

            /* Test to see if this is a good substitution. */
            /* Examine the home  */
            borg_notice_home(NULL, FALSE);

            /* Evaluate the home  */
            home_power = borg_power_home();

            /* Track best */
            if (home_power > *b_home_power)
            {
                /* Save the results */
                for (k = 0; k < STORE_INVEN_MAX; k++) best[k] = test[k];

#if 0
                /* dump, for debugging */
                borg_note(format("Trying Combo (best home power %ld)",
                                    *b_home_power));
                borg_note(format("             (test home power %ld)",
                                    home_power));
                for (i = 0; i < STORE_INVEN_MAX; i++)
                    if (auto_shops[7].ware[i].iqty)
                        borg_note(format("store %d %s (qty-%d).",  i,
                                           auto_shops[7].ware[i].desc,
                                           auto_shops[7].ware[i].iqty ));
                    else
                    borg_note(format("store %d (empty).",  i));

                borg_note(" "); /* add a blank line */
#endif

                /* Use it */
                *b_home_power = home_power;
            }

            /* restore stuff */
            COPY(item2, &safe_home[n], auto_item);

            /* put item back into pack */
            item->iqty++;

            /* put the item back in the test array */
            test[n] = n;
        }
    }
}

static void borg_think_home_sell_aux3( )
{
    int     i;
    s32b    borg_empty_home_power;
    s32b    power;

    /* get the starting power */
    borg_notice(TRUE);
    power = borg_power();

    /* get what an empty home would have for power */
    borg_notice_home( NULL, TRUE );
    borg_empty_home_power = borg_power_home();

    /* go through the inventory and eliminate items that either  */
    /* 1) will not increase the power of an empty house. */
    /* 2) will reduce borg_power if given to home */
    for (i = 0; i < INVEN_PACK; i++)
    {
        int num_items_given;
        num_items_given = 0;

        /* if there is no item here, go to next slot */
        if (!auto_items[i].iqty)
            continue;


        /* 1) eliminate garbage items (items that add nothing to an */
        /*     empty house) */
        borg_notice_home( &auto_items[i], FALSE );
        if (borg_power_home() <= borg_empty_home_power)
        {
            safe_items[i].iqty = 0;
            continue;
        }

        /* 2) will reduce borg_power if given to home */
        while (auto_items[i].iqty)
        {
            /* reduce inventory by this item */
            num_items_given++;
            auto_items[i].iqty--;

            /* Examine borg */
            borg_notice(FALSE);

            /* done if this reduces the borgs power */
            if (borg_power() < power)
            {
                /* we gave up one to many items */
                num_items_given--;
                break;
            }
        }

        /* restore the qty */
        auto_items[i].iqty = safe_items[i].iqty;

        /* set the qty to number given without reducing borg power */
        safe_items[i].iqty = num_items_given;
    }
}

/*
 * Step 1 -- sell "useful" things to the home (for later)
 */
static bool borg_think_home_sell_aux( bool save_best )
{
    int icky = STORE_INVEN_MAX - 1;

    s32b home_power = -1L;

    int i = -1;

    byte test_a[STORE_INVEN_MAX];
    byte best_a[STORE_INVEN_MAX];

    /* if the best is being saved (see borg_think_shop_grab_aux) */
    /* !FIX THIS NEEDS TO BE COMMENTED BETTER */
    if (!save_best)
        b_home_power = &home_power;
    test = test_a;
    best = best_a;

    /* if I have not been to home, do not try this yet. */
    if (!auto_shops[7].when) return FALSE;

    /* Hack -- the home is full */
    /* and pack is full */
    if (auto_shops[7].ware[icky].iqty &&
        auto_items[INVEN_PACK-1].iqty)
        return (FALSE);

    /* Copy all the store slots */
    for (i = 0; i < STORE_INVEN_MAX; i++)
    {
        /* Save the item */
        COPY(&safe_home[i], &auto_shops[7].ware[i], auto_item);

        /* clear test arrays (test[i] == i is no change) */
        best[i] = test[i] = i;
    }

    /* Hack -- Copy all the slots */
    for (i = 0; i < INVEN_PACK; i++)
    {
        /* Save the item */
        if (i==weapon_swap && weapon_swap !=0) continue;
        if (i==armour_swap && armour_swap !=0) continue;
        COPY(&safe_items[i], &auto_items[i], auto_item);
    }

    /* get rid of useless items */
    borg_think_home_sell_aux3();

    /* Examine the borg once more with full inventory then swap in the */
    /* safe_items for the home optimization */
    borg_notice(FALSE);

    /* swap quantities (this should be all that is different) */
    for (i = 0; i < INVEN_PACK; i++)
    {
        byte save_qty;
        if (i==weapon_swap && weapon_swap !=0) continue;
        if (i==armour_swap && armour_swap !=0) continue;

        save_qty = safe_items[i].iqty;
        safe_items[i].iqty = auto_items[i].iqty;
        auto_items[i].iqty = save_qty;
    }

    *b_home_power = -1;

    /* find best combo for home. */
    borg_think_home_sell_aux2( 0, 0 );

    /* restore bonuses and such */
    for (i = 0; i < STORE_INVEN_MAX; i++)
    {
        COPY(&auto_shops[7].ware[i], &safe_home[i], auto_item);
    }

    for (i = 0; i < INVEN_TOTAL; i++)
    {
        if (i==weapon_swap && weapon_swap !=0) continue;
        if (i==armour_swap && armour_swap !=0) continue;
        COPY(&auto_items[i], &safe_items[i], auto_item);
    }

    borg_notice(FALSE);
    borg_notice_home(NULL, FALSE);

    /* Drop stuff that will stack in the home */
    for (i = 0; i < STORE_INVEN_MAX; i++)
    {
        /* if this is not the item that was there, */
        /* drop off the item that replaces it. */
        if (best[i] != i && best[i] != 255)
        {
            auto_item *item = &auto_items[best[i]-STORE_INVEN_MAX];
            auto_item *item2 = &auto_shops[7].ware[i];

            /* if this item is not the same as what was */
            /* there before take it. */
            if (!borg_object_similar(item2, item))
                continue;

            goal_shop = 7;
            goal_item = best[i] - STORE_INVEN_MAX;

            return (TRUE);
        }
    }

    /* Get rid of stuff in house but not in 'best' house if  */
    /* pack is not full */
    if (!auto_items[INVEN_PACK-1].iqty)
    {
        for (i = 0; i < STORE_INVEN_MAX; i++)
        {
            /* if this is not the item that was there, */
            /* get rid of the item that was there */
            if ((best[i] != i) &&
                (auto_shops[7].ware[i].iqty))
            {
                auto_item *item = &auto_items[best[i]-STORE_INVEN_MAX];
                auto_item *item2 = &auto_shops[7].ware[i];

                /* if this item is not the same as what was */
                /* there before take it. */
                if (borg_object_similar(item, item2))
                    continue;

                /* skip stuff if we sold bought it */
                if (sold_item_tval == item2->tval && sold_item_sval == item2->sval &&
                sold_item_pval == item2->pval && sold_item_store == 7) return (FALSE);

                goal_shop = 7;
                goal_ware = i;

                return TRUE;
            }
        }
    }

    /* Drop stuff that is in best house but currently in inventory */
    for (i = 0; i < STORE_INVEN_MAX; i++)
    {
        /* if this is not the item that was there,  */
        /* drop off the item that replaces it. */
        if (best[i] != i && best[i] != 255)
        {
            /* hack dont sell DVE */
            if (!auto_items[best[i]-STORE_INVEN_MAX].iqty) return (FALSE);

            goal_shop = 7;
            goal_item = best[i] - STORE_INVEN_MAX;

            return (TRUE);
        }
    }

    /* Assume not */
    return (FALSE);
}


/*
 * Determine if an item can be sold in the given store
 *
 * XXX XXX XXX Consider use of "icky" test on items
 */
static bool borg_good_sell(auto_item *item, int who)
{
    /* Never sell worthless items */
    if (item->value <= 0) return (FALSE);

    /* Analyze the type */
    switch (item->tval)
    {
        case TV_POTION:
        case TV_SCROLL:

        /* Never sell if not "known" and interesting */
        if (!item->able && (auto_max_depth > 5)) return (FALSE);
        break;

        case TV_FOOD:
        case TV_ROD:
        case TV_WAND:
        case TV_STAFF:
        case TV_RING:
        case TV_AMULET:
        case TV_LITE:

        /* Never sell if not "known" */
        if (!item->able) return (FALSE);

        break;

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


        /* Only sell "known" (or "average") items (unless "icky") */
        if (!item->able && !streq(item->note, "{average}") &&
            !borg_item_icky(item)) return (FALSE);
    }


    /* Do not sell artifacts that is not fully id'd and should be  */
    if (!item->fully_identified && item->name1)
    {
        /* If Dragons can use it, dont sell it */
        if (auto_race >= RACE_MIN_DRAGON)
        {
            if (borg_wield_slot(item) != -1) return (FALSE);
        }
        else
        {
            /* CHECK ALL THE ARTIFACTS (for now)*/
            return (FALSE);
        }
    }
    /* Do not sell ego stuff that is not fully id'd and should be  */
    if (!item->fully_identified && (item->name2 || (item->tval == TV_AMULET &&
        item->sval == SV_AMULET_THE_MAGI) || streq(item->note, "{excellent}")))
    {
        /* If Dragons can use it, dont sell it */
        if (auto_race >= RACE_MIN_DRAGON && (borg_wield_slot(item) != -1))
        {
           /* dont sell amgi amulets */
           if (item->tval == TV_AMULET && item->sval == SV_AMULET_THE_MAGI) return (FALSE);

           switch (item->name2)
           {
            /* Robe of Permanance */
            case EGO_PERMANENCE:
            /* Cloak of Elvenkind */
            case EGO_ELVENKIND:         /* he may store up armours too */
            /* Crown of the Magi */
            case EGO_MAGI:
            /* Cloak of Aman */
            case EGO_AMAN:
                return (FALSE);
            default:
                break;
            }
        }


else  /* consider non dragons */
        {
           /* dont sell amgi amulets */
           if (item->tval == TV_AMULET && item->sval == SV_AMULET_THE_MAGI) return (FALSE);

           switch (item->name2)
           {
            /* Weapon (Blessed) */
            case EGO_BLESS_BLADE:
            /* Robe of Permanance */
            case EGO_PERMANENCE:
            /* Armor of Elvenkind */
            case EGO_ELVENKIND:
            /* Crown of the Magi */
            case EGO_MAGI:
            /* Cloak of Aman */
            case EGO_AMAN:
            /* Weapon (Holy Avenger) */
            case EGO_HA:
                return (FALSE);

            /* Weapon (Defender) */
            case EGO_DF:
            /* anything else */
            default:
                break;
           }
        }
    }

    /* Switch on the store */
    switch (who + 1)
    {
        /* General Store */
        case 1:

            /* Analyze the type */
            switch (item->tval)
            {
                case TV_DIGGING:
                case TV_CLOAK:
                case TV_FOOD:
                case TV_FLASK:
                case TV_LITE:
                case TV_SPIKE:
                return (TRUE);
            }
            break;

        /* Armoury */
        case 2:

            /* Analyze the type */
            switch (item->tval)
            {
                case TV_BOOTS:
                case TV_GLOVES:
                case TV_HELM:
                case TV_CROWN:
                case TV_SHIELD:
                case TV_SOFT_ARMOR:
                case TV_HARD_ARMOR:
                case TV_DRAG_ARMOR:
                return (TRUE);
            }
            break;

        /* Weapon Shop */
        case 3:

            /* Analyze the type */
            switch (item->tval)
            {
                case TV_SHOT:
                case TV_BOLT:
                case TV_ARROW:
                case TV_BOW:
                case TV_DIGGING:
                case TV_HAFTED:
                case TV_POLEARM:
                case TV_SWORD:
                return (TRUE);
            }
            break;

        /* Temple */
        case 4:

            /* Analyze the type */
            switch (item->tval)
            {
                case TV_HAFTED:
                case TV_PRAYER_BOOK:
                case TV_SCROLL:
                case TV_POTION:
                return (TRUE);
            }
            break;

        /* book store --Alchemist */
        case 5:

            /* Analyze the type */
            switch (item->tval)
            {
                case TV_SCROLL:
                case TV_POTION:
                return (TRUE);
            }
            break;

        /* Magic Shop */
        case 6:
            /* Analyze the type */
            switch (item->tval)
            {
                case TV_AMULET:
                case TV_RING:
                case TV_SCROLL:
                case TV_POTION:
                case TV_STAFF:
                case TV_WAND:
                case TV_ROD:
                case TV_MAGIC_BOOK:
                return (TRUE);
            }
            break;

        /* Black Market */
        case 7:
        if (auto_level == 50) return (TRUE);
        break;

        /* Home */
        case 8:
            /* Analyze the type */
            switch (item->tval)
            {
                case TV_CROWN:
                case TV_CLOAK:
                case TV_LITE:
                case TV_AMULET:
                case TV_RING:
                case TV_STAFF:
                case TV_WAND:
                case TV_ROD:
                case TV_SCROLL:
                case TV_POTION:
                case TV_FLASK:
                case TV_FOOD:
                case TV_MAGIC_BOOK:
                case TV_PRAYER_BOOK:
                return (TRUE);
            }
            break;
    }

    /* Assume not */
    return (FALSE);
}



/*
 * Step 2 -- sell "useless" items to a shop (for cash)
 */
static bool borg_think_shop_sell_aux(void)
{
    int icky = STORE_INVEN_MAX - 1;

    int k, b_k = -1;
    int i, b_i = -1;
    int qty = 1;
    s32b p, b_p = 0L;
    s32b c = 0L;
    s32b b_c = 30001L;

    bool fix = FALSE;


    /* Evaluate */
    b_p = my_power;


    /* Check each shop */
    for (k = 0; k < (MAX_STORES -1) ; k++)
    {
        /* Hack -- Skip "full" shops */
        if (auto_shops[k].ware[icky].iqty) continue;

        /* Save the store hole */
        COPY(&safe_shops[k].ware[icky], &auto_shops[k].ware[icky], auto_item);

        /* Sell stuff */
        for (i = 0; i < INVEN_PACK; i++)
        {
            auto_item *item = &auto_items[i];

            /* Skip empty items */
            if (!item->iqty) continue;

            /* Skip some important type items */
            if ((item->tval == my_ammo_tval) && (amt_missile < 45)) continue;
            if ((auto_class == CLASS_WARRIOR || auto_class == CLASS_ROGUE) &&
                item->tval == TV_ROD && item->sval == SV_ROD_HEALING &&
                amt_rod_heal <= 3) continue;

            if (auto_class == CLASS_WARRIOR &&
                item->tval == TV_ROD && item->sval == SV_ROD_MAPPING &&
                item->iqty <= 2) continue;

            /* Try not to sell cool_staffs - Destruction*/
            if (item->tval == TV_STAFF && item->sval == SV_STAFF_DESTRUCTION &&
                item->iqty < 3) continue;

            /* Try not to sell cool_staffs - Power */
            if (item->tval == TV_STAFF && item->sval == SV_STAFF_POWER &&
                item->iqty < 3) continue;

            /* Try not to sell cool_staffs - Holiness*/
            if (item->tval == TV_STAFF && item->sval == SV_STAFF_HOLINESS &&
                item->iqty < 3) continue;

            if (i==weapon_swap && weapon_swap !=0) continue;
            if (i==armour_swap && armour_swap !=0) continue;

            /* Skip "bad" sales */
            if (!borg_good_sell(item, k)) continue;

            /* Save the item */
            COPY(&safe_items[i], &auto_items[i], auto_item);

            /* Give the item to the shop */
            COPY(&auto_shops[k].ware[icky], &safe_items[i], auto_item);

            /* get the quantity */
            qty = borg_min_item_quantity(item);

            /* Give a single item */
            auto_shops[k].ware[icky].iqty = qty;

            /* Lose a single item */
            auto_items[i].iqty -=qty;

            /* Fix later */
            fix = TRUE;

            /* Examine the inventory */
            borg_notice(FALSE);

            /* Evaluate the inventory */
            p = borg_power();

            /* Restore the item */
            COPY(&auto_items[i], &safe_items[i], auto_item);

            /* Ignore "bad" sales */
            if (p < b_p) continue;

            /* Extract the "price" */
            c = ((item->value < 30000L) ? item->value : 30000L);

            /* sell cheap items first.  This is done because we may have to */
            /* buy the item back in some very strange curcumstances. */
            if ((p == b_p) && (c >= b_c)) continue;
            /* Maintain the "best" */
            b_k = k; b_i = i; b_p = p; b_c = c;
        }

        /* Restore the store hole */
        COPY(&auto_shops[k].ware[icky], &safe_shops[k].ware[icky], auto_item);
    }

    /* Examine the inventory */
    if (fix) borg_notice(TRUE);

    /* Sell something (if useless) */
    if ((b_k >= 0) && (b_i >= 0))
    {
        /* Visit that shop */
        goal_shop = b_k;

        /* Sell that item */
        goal_item = b_i;

        /* Success */
        return (TRUE);
    }

    /* Assume not */
    return (FALSE);
}



/*
 * Help decide if an item should be bought from a real store
 *
 * We prevent the purchase of enchanted (or expensive) ammo,
 * so we do not spend all our money on temporary power.
 * if below level 35, who needs cash?  buy the expecive ammo!
 *
 * We prevent the purchase of low level discounted books,
 * so we will not waste slots on cheap books.
 *
 * We prevent the purchase of items from the black market
 * which are often available at normal stores, currently,
 * this includes low level books, and all wands and staffs.
 */
static bool borg_good_buy(auto_item *item, int who)
{

    /* Don't buy from BM until we are rich */
    if ((who == 6) && (auto_level < 35) ) return (FALSE);

    /* do not buy the item if I just sold it. */
    if (sold_item_tval == item->tval && sold_item_sval == item->sval &&
        sold_item_pval == item->pval && sold_item_store == who)
    {
        borg_note(format("# Choosing not to buy back %s",item->desc));
        return (FALSE);
    }

    /* Check the object */
    switch (item->tval)
    {
        case TV_DIGGING:
        return (FALSE);

        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        if (auto_level < 35)
        {
            if (item->to_h) return (FALSE);
            if (item->to_d) return (FALSE);
        }
        break;

        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        if (item->sval >= 4) break;
        if (item->discount) return (FALSE);
        break;

        case TV_WAND:
        case TV_STAFF:
        break;
    }


    /* Okay */
    return (TRUE);
}

/*
 * Step 3 -- buy "useful" things from a shop (to be used)
 */
static bool borg_think_shop_buy_aux(void)
{
    int hole = INVEN_PACK - 1;

    int slot;
    int qty =1;

    int k, b_k = -1;
    int n, b_n = -1;
    int ii, bb_p = -1;

    s32b p, b_p = 0L;
    s32b c, b_c = 0L;

    bool fix = FALSE;


    /* Require one empty slot */
    if (auto_items[hole].iqty) return (FALSE);


    /* Extract the "power" */
    b_p = my_power;


    /* Check the shops */
    for (k = 0; k < (MAX_STORES -1); k++)
    {
        /* Scan the wares */
        for (n = 0; n < STORE_INVEN_MAX; n++)
        {
            auto_item *item = &auto_shops[k].ware[n];

            /* Skip empty items */
            if (!item->iqty) continue;

            /* Skip "bad" buys */
            if (!borg_good_buy(item, k)) continue;

            /* Hack -- Require "sufficient" cash */
            if (auto_gold < item->cost * 12 / 10) continue;

            /* Skip it if I just sold this item. XXX XXX*/

            /* Save shop item */
            COPY(&safe_shops[k].ware[n], &auto_shops[k].ware[n], auto_item);

            /* Save hole */
            COPY(&safe_items[hole], &auto_items[hole], auto_item);

            /* Save the number to trade */
            qty = borg_min_item_quantity(item);

            /* Remove one item from shop (sometimes) */
            auto_shops[k].ware[n].iqty -= qty;

            /* Obtain "slot" */
            slot = borg_wield_slot(item);

            /* Consider new equipment */
           if (slot >= 0)
           {
                /* process for non dragons */
                if ((auto_race < RACE_MIN_DRAGON) ||
                    (auto_race >= RACE_MIN_DRAGON && slot != 99))
                {
                    /* Save old item */
                    COPY(&safe_items[slot], &auto_items[slot], auto_item);

                    /* Move equipment into inventory */
                    COPY(&auto_items[hole], &safe_items[slot], auto_item);

                    /* Move new item into equipment */
                    COPY(&auto_items[slot], &safe_shops[k].ware[n], auto_item);

                    /* Only a single item */
                    auto_items[slot].iqty = qty;

                    /* Fix later */
                    fix = TRUE;

                    /* Examine the inventory */
                    borg_notice(FALSE);

                    /* Evaluate the inventory */
                    p = borg_power();

                    /* Restore old item */
                    COPY(&auto_items[slot], &safe_items[slot], auto_item);
                }  /* non dragons */
                else /* process the full dragons */
                {
                    /* Compare this shop ring to all my current rings */
                    for (ii = INVEN_D_RING1; ii < INVEN_TOTAL; ii++)
                    {
                        slot = ii;

                     /* Save old item */
                    COPY(&safe_items[slot], &auto_items[slot], auto_item);

                    /* Move equipment into inventory */
                    COPY(&auto_items[hole], &safe_items[slot], auto_item);

                    /* Move new item into equipment */
                    COPY(&auto_items[slot], &safe_shops[k].ware[n], auto_item);

                    /* Only a single item */
                    auto_items[slot].iqty = 1;

                    /* Fix later */
                    fix = TRUE;

                    /* Examine the inventory */
                    borg_notice(FALSE);

                    /* Evaluate the inventory */
                    p = borg_power();

                    /* Restore old item */
                    COPY(&auto_items[slot], &safe_items[slot], auto_item);

                    /* Ignore "bad" or "equal" swaps */
                    if (p <= bb_p) continue;

                    /* Maintain the "best" */
                    bb_p = p;
                    } /*  dragon ring slots */

                /* reset our P counter for later */
                if (bb_p) p = bb_p;

                } /*  full dragons */
            } /* equipment */

            /* Consider new inventory */
            if (slot < 0)
            {
                /* Move new item into inventory */
                COPY(&auto_items[hole], &safe_shops[k].ware[n], auto_item);

                /* Only a single item */
                auto_items[hole].iqty = qty;

                /* Fix later */
                fix = TRUE;

                /* Examine the inventory */
                borg_notice(FALSE);

                /* Evaluate the equipment */
                p = borg_power();
            }

            /* Restore hole */
            COPY(&auto_items[hole], &safe_items[hole], auto_item);

            /* Restore shop item */
            COPY(&auto_shops[k].ware[n], &safe_shops[k].ware[n], auto_item);

            /* Obtain the "cost" of the item */
            c = item->cost * qty;

            /* Penalize the cost of expensive items */
            if (c > auto_gold / 10) p -= c;

            /* Ignore "bad" purchases */
            if (p < b_p) continue;

            /* Ignore "expensive" purchases */
            if ((p == b_p) && (c >= b_c)) continue;

            /* Save the item and cost */
            b_k = k; b_n = n; b_p = p; b_c = c;
        }
    }

    /* Examine the inventory */
    if (fix) borg_notice(TRUE);

    /* Buy something */
    if ((b_k >= 0) && (b_n >= 0))
    {
        /* Visit that shop */
        goal_shop = b_k;

        /* Buy that item */
        goal_ware = b_n;

        /* Success */
        return (TRUE);
    }

    /* Nope */
    return (FALSE);
}


/*
 * Step 4 -- buy "useful" things from the home (to be used)
 */
static bool borg_think_home_buy_aux(void)
{
    int hole = INVEN_PACK - 1;

    int slot;
    int qty=1;
    int n, b_n = -1;
    s32b p, b_p = 0L;
    int ii, bb_p = -1;

    bool fix = FALSE;


    /* Require one empty slot */
    if (auto_items[hole].iqty) return (FALSE);


    /* Extract the "power" */
    b_p = my_power;


    /* Scan the home */
    for (n = 0; n < STORE_INVEN_MAX; n++)
    {
        auto_item *item = &auto_shops[7].ware[n];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* do not buy the item if I just sold it. */
        if (sold_item_tval == item->tval && sold_item_sval == item->sval &&
            sold_item_pval == item->pval && sold_item_store == 7)
       {
            borg_note(format("# Choosing not to buy back %s",item->desc));
            return (FALSE);
       }

        /* Save shop item */
        COPY(&safe_shops[7].ware[n], &auto_shops[7].ware[n], auto_item);

        /* Save hole */
        COPY(&safe_items[hole], &auto_items[hole], auto_item);

        /* Save the number */
        qty = borg_min_item_quantity(item);

        /* Remove one item from shop (sometimes) */
        auto_shops[7].ware[n].iqty -= qty;

        /* Obtain "slot" */
        slot = borg_wield_slot(item);

            /* Consider new equipment */
           if (slot >= 0)
           {
                /* process for non dragons */
                if ((auto_race < RACE_MIN_DRAGON) ||
                    (auto_race >= RACE_MIN_DRAGON && slot != 99))
                {
                    /* Save old item */
                    COPY(&safe_items[slot], &auto_items[slot], auto_item);

                    /* Move equipment into inventory */
                    COPY(&auto_items[hole], &safe_items[slot], auto_item);

                    /* Move new item into equipment */
                    COPY(&auto_items[slot], &safe_shops[7].ware[n], auto_item);

                    /* Only a single item */
                    auto_items[slot].iqty = qty;

                    /* Fix later */
                    fix = TRUE;

                    /* Examine the inventory */
                    borg_notice(FALSE);

                    /* Evaluate the inventory */
                    p = borg_power();

                    /* Restore old item */
                    COPY(&auto_items[slot], &safe_items[slot], auto_item);
                }  /* non dragons */
                else /* process the full dragons */
                {
                    /* Compare this shop ring to all my current rings */
                    for (ii = INVEN_D_RING1; ii < INVEN_TOTAL; ii++)
                    {
                        slot = ii;

                     /* Save old item */
                    COPY(&safe_items[slot], &auto_items[slot], auto_item);

                    /* Move equipment into inventory */
                    COPY(&auto_items[hole], &safe_items[slot], auto_item);

                    /* Move new item into equipment */
                    COPY(&auto_items[slot], &safe_shops[7].ware[n], auto_item);

                    /* Only a single item */
                    auto_items[slot].iqty = 1;

                    /* Fix later */
                    fix = TRUE;

                    /* Examine the inventory */
                    borg_notice(FALSE);

                    /* Evaluate the inventory */
                    p = borg_power();

                    /* Restore old item */
                    COPY(&auto_items[slot], &safe_items[slot], auto_item);

                    /* Ignore "bad" or "equal" swaps */
                    if (p <= bb_p) continue;

                    /* Maintain the "best" */
                    bb_p = p;
                    } /* dragon ring slots */

                /* reset our P counter for later */
                if (bb_p) p = bb_p;

                } /* full dragons */
            } /* equipment */

            /* Consider new inventory */
            if (slot < 0)
            {
                /* Move new item into inventory */
                COPY(&auto_items[hole], &safe_shops[7].ware[n], auto_item);

                /* Only a single item */
                auto_items[hole].iqty = qty;

                /* Fix later */
                fix = TRUE;

                /* Examine the inventory */
                borg_notice(FALSE);

                /* Evaluate the equipment */
                p = borg_power();
            }

        /* Restore hole */
        COPY(&auto_items[hole], &safe_items[hole], auto_item);

        /* Restore shop item */
        COPY(&auto_shops[7].ware[n], &safe_shops[7].ware[n], auto_item);

        /* Ignore "silly" purchases */
        if (p <= b_p) continue;

        /* Save the item and cost */
        b_n = n; b_p = p;
    }

    /* Examine the inventory */
    if (fix) borg_notice(TRUE);

    /* Buy something */
    if ((b_n >= 0) && (b_p > my_power))
    {
        /* Go to the home */
        goal_shop = 7;

        /* Buy that item */
        goal_ware = b_n;

        /* Success */
        return (TRUE);
    }

    /* Nope */
    return (FALSE);
}



/*
 * Step 5 -- buy "interesting" things from a shop (to be used later)
 */
static bool borg_think_shop_grab_aux(void)
{

    int k, b_k = -1;
    int n, b_n = -1;
    int qty=1;

    s32b s, b_s = 0L;
    s32b c, b_c = 0L;
    s32b borg_empty_home_power;

    /* get what an empty home would have for power */
    borg_notice_home( NULL, TRUE );
    borg_empty_home_power = borg_power_home();

    b_home_power = &s;

    /* Require two empty slots */
    if (auto_items[INVEN_PACK-1].iqty) return (FALSE);
    if (auto_items[INVEN_PACK-2].iqty) return (FALSE);

    /* Examine the home */
    borg_notice_home(NULL, FALSE);

    /* Evaluate the home */
    b_s = borg_power_home();

    /* Check the shops */
    for (k = 0; k < (MAX_STORES-1); k++)
    {
        /* Scan the wares */
        for (n = 0; n < STORE_INVEN_MAX; n++)
        {
            auto_item *item = &auto_shops[k].ware[n];

            /* Skip empty items */
            if (!item->iqty) continue;

            /* Skip "bad" buys */
            if (!borg_good_buy(item, k)) continue;

            /* dont buy weapons or armour, I'll get those in dungeon */
            if (item->tval <= TV_ROD && item->tval >=TV_BOW) continue;

            /* Hack -- Require some "extra" cash */
            if (auto_gold < 1000L + item->cost * 5) continue;

            /* make this the next to last item that the player has */
            /* (can't make it the last or it thinks that both player and */
            /*  home are full) */
            COPY(&auto_items[INVEN_PACK-2], &auto_shops[k].ware[n], auto_item);

            /* Save the number */
            qty = borg_min_item_quantity(item);

            /* Give a single item */
            auto_items[INVEN_PACK-2].iqty = qty;

            /* make sure this item would help an empty home */
            borg_notice_home( &auto_shops[k].ware[n], FALSE );
            if (borg_empty_home_power >= borg_power_home()) continue;

            /* optimize the home inventory */
            if (!borg_think_home_sell_aux( TRUE )) continue;

            /* Obtain the "cost" of the item */
            c = item->cost * qty;

            /* Penalize expensive items */
            if (c > auto_gold / 10) s -= c;

            /* Ignore "bad" sales */
            if (s < b_s) continue;

            /* Ignore "expensive" purchases */
            if ((s == b_s) && (c >= b_c)) continue;

            /* Save the item and cost */
            b_k = k; b_n = n; b_s = s; b_c = c;
        }
    }

    /* restore inventory hole (just make sure the last slot goes back to */
    /* empty) */
    auto_items[INVEN_PACK-2].iqty = 0;

    /* Examine the home */
    borg_notice_home(NULL, FALSE);

    /* Evaluate the home */
    s = borg_power_home();

    /* remove the target that optimizing the home gave */
    goal_shop = goal_ware = goal_item = -1;

    /* Buy something */
    if ((b_k >= 0) && (b_n >= 0))
    {
        /* Visit that shop */
        goal_shop = b_k;

        /* Buy that item */
        goal_ware = b_n;

        /* Success */
        return (TRUE);
    }

    /* Nope */
    return (FALSE);
}


/*
 * Step 6 -- take "useless" things from the home (to be sold)
 */
static bool borg_think_home_grab_aux(void)
{
    int n, b_n = -1;
    s32b s, b_s = 0L;
    int qty=1;


    /* Require two empty slots */
    if (auto_items[INVEN_PACK-1].iqty) return (FALSE);
    if (auto_items[INVEN_PACK-2].iqty) return (FALSE);


    /* Examine the home */
    borg_notice_home(NULL, FALSE);

    /* Evaluate the home */
    b_s = borg_power_home();


    /* Scan the home */
    for (n = 0; n < STORE_INVEN_MAX; n++)
    {
        auto_item *item = &auto_shops[7].ware[n];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* skip stuff if we sold bought it */
        if (sold_item_tval == item->tval && sold_item_sval == item->sval &&
        sold_item_pval == item->pval && sold_item_store == 7) continue;

        /* Save shop item */
        COPY(&safe_shops[7].ware[n], &auto_shops[7].ware[n], auto_item);

        /* Save the number */
        qty = borg_min_item_quantity(item);

        /* Remove one item from shop */
        auto_shops[7].ware[n].iqty -= qty;

        /* Examine the home */
        borg_notice_home(NULL, FALSE);

        /* Evaluate the home */
        s = borg_power_home();

        /* Restore shop item */
        COPY(&auto_shops[7].ware[n], &safe_shops[7].ware[n], auto_item);

        /* Ignore "bad" sales */
        if (s < b_s) continue;

        /* Maintain the "best" */
        b_n = n; b_s = s;
    }

    /* Examine the home */
    borg_notice_home(NULL, FALSE);

    /* Evaluate the home */
    s = borg_power_home();

    /* Stockpile */
    if (b_n >= 0)
    {
        /* Visit the home */
        goal_shop = 7;

        /* Grab that item */
        goal_ware = b_n;

        /* Success */
        return (TRUE);
    }

    /* Assume not */
    return (FALSE);
}

/*
 * Step 7A -- buy "useful" weapons from the home (to be used as a swap)
 */
static bool borg_think_home_buy_swap_weapon(void)
{
    int hole;

    int slot;
    int old_weapon_swap;
    s32b old_weapon_swap_value;
    int old_armour_swap;
    s32b old_armour_swap_value;
    int n, b_n = -1;
    s32b p, b_p = 0L;

    bool fix = FALSE;

    /* save the current values */
    old_weapon_swap = weapon_swap;
    old_weapon_swap_value =  weapon_swap_value;
    old_armour_swap = armour_swap;
    old_armour_swap_value =  armour_swap_value;

    if (weapon_swap <= 0 || weapon_swap_value <=0)
    {
        hole = INVEN_PACK - 1;
        weapon_swap_value = -1L;
    }
    else
    {
        hole = weapon_swap;
    }

    /* Extract the "power" */
    b_p = weapon_swap_value;

    /* Scan the home */
    for (n = 0; n < STORE_INVEN_MAX; n++)
    {
        auto_item *item = &auto_shops[7].ware[n];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Obtain "slot" make sure its a weapon */
        slot = borg_wield_slot(item);
        if (slot != INVEN_WIELD) continue;

        /* Save shop item */
        COPY(&safe_shops[7].ware[n], &auto_shops[7].ware[n], auto_item);

        /* Save hole */
        COPY(&safe_items[hole], &auto_items[hole], auto_item);

        /* Remove one item from shop */
        auto_shops[7].ware[n].iqty--;


        /* Consider new equipment */
        if (slot == INVEN_WIELD)
        {
            /* Move new item into inventory */
            COPY(&auto_items[hole], &safe_shops[7].ware[n], auto_item);

            /* Only a single item */
            auto_items[hole].iqty = 1;

            /* Fix later */
            fix = TRUE;

            /* Examine the iventory and swap value*/
            borg_notice(TRUE);

            /* Evaluate the new equipment */
            p = weapon_swap_value;
        }

        /* Restore hole */
        COPY(&auto_items[hole], &safe_items[hole], auto_item);

        /* Restore shop item */
        COPY(&auto_shops[7].ware[n], &safe_shops[7].ware[n], auto_item);

        /* Ignore "silly" purchases */
        if (p <= b_p) continue;

        /* Save the item and value */
        b_n = n; b_p = p;
    }

    /* Examine the inventory */
    if (fix) borg_notice(TRUE);

    /* Buy something */
    if ((b_n >= 0) && (b_p > weapon_swap_value))
    {
        /* Go to the home */
        goal_shop = 7;

        /* Buy that item */
        goal_ware = b_n;

        /* Restore the values */
        weapon_swap = old_weapon_swap;
        weapon_swap_value =  old_weapon_swap_value;
        armour_swap = old_armour_swap;
        armour_swap_value =  old_armour_swap_value;

        /* Success */
        return (TRUE);
    }

    /* Restore the values */
        weapon_swap = old_weapon_swap;
        weapon_swap_value =  old_weapon_swap_value;
        armour_swap = old_armour_swap;
        armour_swap_value =  old_armour_swap_value;

    /* Nope */
    return (FALSE);
}
/*
 * Step 7B -- buy "useful" armour from the home (to be used as a swap)
 */
static bool borg_think_home_buy_swap_armour(void)
{
    int hole;

    int slot;

    int n, b_n = -1;
    s32b p, b_p = 0L;
    bool fix = FALSE;
    int old_weapon_swap;
    s32b old_weapon_swap_value;
    int old_armour_swap;
    s32b old_armour_swap_value;

    /* save the current values */
    old_weapon_swap = weapon_swap;
    old_weapon_swap_value =  weapon_swap_value;
    old_armour_swap = armour_swap;
    old_armour_swap_value =  armour_swap_value;

    if (armour_swap <= 1 || armour_swap_value <=0 )
    {
        hole = INVEN_PACK - 1;
        armour_swap_value = -1L;
    }
    else
    {
        hole = armour_swap;
    }


    /* Extract the "power" */
    b_p = armour_swap_value;


    /* Scan the home */
    for (n = 0; n < STORE_INVEN_MAX; n++)
    {
        auto_item *item = &auto_shops[7].ware[n];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Obtain "slot".  Elimination of non armours in borg4.c*/
        slot = borg_wield_slot(item);


        /* Save shop item */
        COPY(&safe_shops[7].ware[n], &auto_shops[7].ware[n], auto_item);

        /* Save hole */
        COPY(&safe_items[hole], &auto_items[hole], auto_item);

        /* Remove one item from shop */
        auto_shops[7].ware[n].iqty--;

        /* Move new item into inventory */
        COPY(&auto_items[hole], &safe_shops[7].ware[n], auto_item);

        /* Only a single item */
        auto_items[hole].iqty = 1;

        /* Fix later */
        fix = TRUE;

        /* Examine the inventory (false)*/
        borg_notice(TRUE);

        /* Evaluate the new equipment */
        p = armour_swap_value;

        /* Restore hole */
        COPY(&auto_items[hole], &safe_items[hole], auto_item);

        /* Restore shop item */
        COPY(&auto_shops[7].ware[n], &safe_shops[7].ware[n], auto_item);

        /* Ignore "silly" purchases */
        if (p <= b_p) continue;

        /* Save the item and value */
        b_n = n; b_p = p;
    }

    /* Examine the inventory */
    if (fix) borg_notice(TRUE);

    /* Buy something */
    if ((b_n >= 0) && (b_p > armour_swap_value))
    {
        /* Go to the home */
        goal_shop = 7;

        /* Buy that item */
        goal_ware = b_n;

        /* Restore the values */
        weapon_swap = old_weapon_swap;
        weapon_swap_value =  old_weapon_swap_value;
        armour_swap = old_armour_swap;
        armour_swap_value =  old_armour_swap_value;

        /* Success */
        return (TRUE);
    }
    /* Restore the values */
    weapon_swap = old_weapon_swap;
    weapon_swap_value =  old_weapon_swap_value;
    armour_swap = old_armour_swap;
    armour_swap_value =  old_armour_swap_value;

    /* Nope */
    return (FALSE);
}




/*
 * Choose a shop to visit (see above)
 */
static bool borg_choose_shop(void)
{
    int i;

    /* Must be in town */
    if (auto_depth) return (FALSE);

    /* Forbid if been sitting on level forever */
    /*    Just come back and work through the loop later */
    if (c_t - auto_began > 2000) return (FALSE);
    if (time_this_panel > 250) return (FALSE);

    /* If poisoned or bleeding -- flow to temple */
    if (do_cut || do_poisoned) goal_shop = 3;

    /* If Starving  -- flow to general store */
    if (do_weak || (!my_cur_lite && auto_level >= 2)) goal_shop = 0;

    /* If poisoned or bleeding -- Buy items from temple */
    if (!my_cur_lite || do_weak || do_cut || do_poisoned)
    {
       if (borg_think_shop_buy_aux())
       {
            /* Message */
            borg_note(format("# Buying '%s' at '%s' (for player)",
                         auto_shops[goal_shop].ware[goal_ware].desc,
                         (f_name + f_info[0x08+goal_shop].name)));

            /* Success */
            return (TRUE);
        }

        /* if temple is out of healing stuff, try the house */
        if (borg_think_home_buy_aux())
        {
            /* Message */
            borg_note(format("# Buying '%s' from the home",
                         auto_shops[goal_shop].ware[goal_ware].desc));

            /* Success */
            return (TRUE);
        }

    }

    /* Must have visited all shops first---complete information */
    for (i = 0; i < (MAX_STORES); i++)
    {
        auto_shop *shop = &auto_shops[i];

        /* Skip "visited" shops */
        if (!shop->when) return (FALSE);
    }

    /* if we are already flowing toward a shop do not check again... */
    if (goal_shop !=  -1)
        return TRUE;

    /* Assume no important shop */
    goal_shop = goal_ware = goal_item = -1;


    /* Step 1 -- Sell items to the home */
    if (borg_think_home_sell_aux( FALSE ))
    {
        /* Message */
        if (goal_item != -1)
            borg_note(format("# Selling '%s' to the home",
                             auto_items[goal_item].desc));
        else
            borg_note(format("# Buying '%s' from the home",
                             auto_shops[goal_shop].ware[goal_ware].desc));

        /* Success */
        return (TRUE);
    }


    /* Step 2 -- Sell items to the shops */
    if (borg_think_shop_sell_aux())
    {
        /* Message */
        borg_note(format("# Selling '%s' at '%s'",
                         auto_items[goal_item].desc,
                         (f_name + f_info[0x08+goal_shop].name)));

        /* Success */
        return (TRUE);
    }

    /* Step 3 -- Buy items from the shops (for the player) */
    if (borg_think_shop_buy_aux())
    {
        /* Message */
        borg_note(format("# Buying '%s' at '%s' (for player)",
                         auto_shops[goal_shop].ware[goal_ware].desc,
                         (f_name + f_info[0x08+goal_shop].name)));

        /* Success */
        return (TRUE);
    }


    /* Step 4 -- Buy items from the home (for the player) */
    if (borg_think_home_buy_aux())
    {
        /* Message */
        borg_note(format("# Buying '%s' from the home",
                         auto_shops[goal_shop].ware[goal_ware].desc));

        /* Success */
        return (TRUE);
    }

    /* get rid of junk from home first.  That way the home is 'uncluttered' */
    /* before you buy stuff for it.  This will prevent the problem where an */
    /* item has become a negative value and swapping in a '0' gain item */
    /* (like pottery) is better. */

    /* Step 5 -- Grab items from the home (for the shops) */
    if (borg_think_home_grab_aux())
    {
        /* Message */
        borg_note(format("# Grabbing (to sell) '%s' from the home",
                         auto_shops[goal_shop].ware[goal_ware].desc));

        /* Success */
        return (TRUE);
    }

    /* Step 6 -- Buy items from the shops (for the home) */
    if (borg_think_shop_grab_aux())
    {
        /* Message */
        borg_note(format("# Grabbing (for home) '%s' at '%s'",
                         auto_shops[goal_shop].ware[goal_ware].desc,
                         (f_name + f_info[FEAT_SHOP_HEAD+goal_shop].name)));

        /* Success */
        return (TRUE);
    }
    /* Step 7A -- Buy weapons from the home (as a backup item) */
    if (borg_think_home_buy_swap_weapon())
    {
        /* Message */
        borg_note(format("# Buying '%s' from the home as a backup",
                         auto_shops[goal_shop].ware[goal_ware].desc));

        /* Success */
        return (TRUE);
    }
    /* Step 7B -- Buy armour from the home (as a backup item) */
    if (borg_think_home_buy_swap_armour())
    {
        /* Message */
        borg_note(format("# Buying '%s' from the home as a backup",
                         auto_shops[goal_shop].ware[goal_ware].desc));

        /* Success */
        return (TRUE);
    }


    /* Failure */
    return (FALSE);

}




/*
 * Sell items to the current shop, if desired
 */
static bool borg_think_shop_sell(void)
{
    int qty= 1;
    /* Sell something if requested */
    if ((goal_shop == shop_num) && (goal_item >= 0))
    {

        auto_item *item = &auto_items[goal_item];

        qty = borg_min_item_quantity(item);

        /* Log */
        borg_note(format("# Selling %s (%d)", item->desc, goal_item));

        /* Buy an item */
        borg_keypress('s');

        /* Buy the desired item */
        borg_keypress(I2A(goal_item));

        /* Hack -- Sell a single item */
        if (qty >= 2)
        {
            if (qty == 5) borg_keypress('5');
            if (qty == 4) borg_keypress('4');
            if (qty == 3) borg_keypress('3');
            if (qty == 2) borg_keypress('2');
        }

        /* Mega-Hack -- Accept the price */
        borg_keypress('\n');
        borg_keypress('\n');
        borg_keypress('\n');
        borg_keypress('\n');

        /* Mark our last item sold */
        sold_item_pval = item->pval;
        sold_item_tval = item->tval;
        sold_item_sval = item->sval;
        sold_item_store = goal_shop;

        /* The purchase is complete */
        goal_shop = goal_item = -1;
        time_this_panel ++;

        /* leave the store */
        borg_keypress(ESCAPE);

        /* Exit the store & rebrowse this store AJG*/
        auto_do_browse_what = 100;


        /* Success */
        return (TRUE);
    }

    /* Nope */
    return (FALSE);
}


/*
 * Buy items from the current shop, if desired
 */
static bool borg_think_shop_buy(void)
{
    int qty =1;

    /* Buy something if requested */
    if ((goal_shop == shop_num) && (goal_ware >= 0))
    {
        auto_shop *shop = &auto_shops[goal_shop];

        auto_item *item = &shop->ware[goal_ware];

        /* Sometimes the borgs may buy a missing item */
        if (!item->kind) return (FALSE);

        qty = borg_min_item_quantity(item);

        /* Log */
        borg_note(format("# Buying %s (%d).", item->desc, goal_ware));

        /* Buy an item */
        borg_keypress('p');

        /* tick the anti-loop clock */
        time_this_panel ++;

        /* Buy the desired item */
        borg_keypress(I2A(goal_ware));

        /* Hack -- Buy a single item (sometimes)*/
        if (qty >= 2)
        {
            if (qty == 5) borg_keypress('5');
            if (qty == 4) borg_keypress('4');
            if (qty == 3) borg_keypress('3');
            if (qty == 2) borg_keypress('2');
        }

        /* Mega-Hack -- Accept the price */
        borg_keypress('\n');
        borg_keypress('\n');
        borg_keypress('\n');
        borg_keypress('\n');

        /* Mark it for 'no buy backs' */
        sold_item_store = goal_shop;

        /* The purchase is complete */
        goal_shop = goal_ware = -1;

        /* Increment our clock to avoid loops */
        time_this_panel ++;

        /* Dragons need to leave the shop after buying equipment */
        if (auto_race >=RACE_MIN_DRAGON || time_this_panel > 100 )
            {
                /* leave the store */
                borg_keypress(ESCAPE);

                /* Exit the store & rebrowse this store AJG*/
                auto_do_browse_what = 100;

            }

        /* Success */
        return (TRUE);
    }

    /* Nothing to buy */
    return (FALSE);
}


/*
 * Deal with being in a store
 */
bool borg_think_store(void)
{
    /* Hack -- prevent clock wrapping */
    if (c_t >= 28000)
    {
        /* Clear Possible errors */
        borg_keypress(ESCAPE);
        borg_keypress(ESCAPE);
        borg_keypress(ESCAPE);
        borg_keypress(ESCAPE);

        /* Re-examine inven and equip */
        borg_cheat_inven();
        borg_cheat_equip();
    }

    /* update all my equipment and swap items */
    borg_notice(TRUE);

    /* Stamp the shop with a time stamp */
    auto_shops[shop_num].when = c_t;

    /* Dragons dont need to swap, they handle it in wear_stuff */
    if (auto_race < RACE_MIN_DRAGON)
    {
        /* Remove "backwards" rings */
        if (borg_swap_rings()) return (TRUE);

        /* Repair "backwards" rings */
        if (borg_wear_rings()) return (TRUE);

        /* Wear "optimal" equipment */
        if (borg_best_stuff()) return (TRUE);
    }

    /* Wear "useful" equipment */
    /* if (borg_wear_stuff()) return (TRUE); */

    /* Remove "useless" equipment */
    if (borg_remove_stuff()) return (TRUE);

    /* Choose a shop to visit */
    if (borg_choose_shop())
    {
        /* Try to sell stuff */
        if (borg_think_shop_sell()) return (TRUE);

        /* Try to buy stuff */
        if (borg_think_shop_buy()) return (TRUE);
    }

    /* No shop */
    shop_num = -1;


    /* Leave the store */
    borg_keypress(ESCAPE);

    /* Done */
    return (TRUE);
}



/*
 * Hack -- perform an action in the dungeon under boosted bravery
 *
 * This function is a sub-set of the standard dungeon goals, and is
 * only executed when all of the standard dungeon goals fail, because
 * of excessive danger, or because the level is "bizarre".
 */
static bool borg_think_dungeon_brave(void)
{
    /*** Local stuff ***/

    /* Attack monsters */
    if (borg_attack()) return (TRUE);

    /* Cast a light beam to remove fear of an area */
    if (borg_lite_beam(FALSE)) return (TRUE);

    /* Continue flowing towards monsters */
    if (borg_flow_old(GOAL_KILL)) return (TRUE);

    /* Find a (viewable) monster */
    if (borg_flow_kill(TRUE)) return (TRUE);

    /* Continue flowing towards objects */
    if (borg_flow_old(GOAL_TAKE)) return (TRUE);

    /* Find a (viewable) object */
    if (borg_flow_take(TRUE)) return (TRUE);


    /*** Flee (or leave) the level ***/

    /* Flee the level */
    if (goal_fleeing || goal_leaving)
    {
        /* Hack -- Take the next stairs */
        stair_less = goal_fleeing;
        if (borg_ready_morgoth == 0)
            stair_less = TRUE;

        /* Only go down if fleeing or prepared. */
        stair_more = goal_fleeing;
        if ((cptr)NULL == borg_prepared(auto_depth+1))
            stair_more = TRUE;

        /* Continue fleeing the level */
        if (borg_flow_old(GOAL_FLEE)) return (TRUE);

        /* Try to find some stairs up */
        if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

        /* Try to find some stairs down */
        if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);

        /* Try to hide on a glyph if no stairs */
        if (borg_flow_glyph(GOAL_FLEE)) return (TRUE);

    }

    /*** Exploration ***/

    /* Continue flowing (see below) */
    if (borg_flow_old(GOAL_MISC)) return (TRUE);

    /* Continue flowing (see below) */
    if (borg_flow_old(GOAL_DARK)) return (TRUE);

    /* Continue flowing (see below) */
    if (borg_flow_old(GOAL_XTRA)) return (TRUE);

    /* Continue flowing (see below) */
    if (borg_flow_old(GOAL_BORE)) return (TRUE);


    /*** Explore the dungeon ***/

    /* Explore interesting grids */
    if (borg_flow_dark(TRUE)) return (TRUE);

    /* Explore interesting grids */
    if (borg_flow_dark(FALSE)) return (TRUE);

    /* Search for secret doors */
    if (borg_flow_spastic(FALSE)) return (TRUE);


    /*** Track down old stuff ***/

    /* Chase old objects */
    if (borg_flow_take(FALSE)) return (TRUE);

    /* Chase old monsters */
    if (borg_flow_kill(FALSE)) return (TRUE);

    /* Search for secret doors */
    if (borg_flow_spastic(TRUE)) return (TRUE);


    /* Nothing */
    return (FALSE);
}


/*
 * Perform an action in the dungeon
 *
 * Return TRUE if a "meaningful" action was performed
 * Otherwise, return FALSE so we will be called again
 *
 * Strategy:
 *   Make sure we are happy with our "status" (see above)
 *   Attack and kill visible monsters, if near enough
 *   Open doors, disarm traps, tunnel through rubble
 *   Pick up (or tunnel to) gold and useful objects
 *   Explore "interesting" grids, to expand the map
 *   Explore the dungeon and revisit old grids
 *
 * Fleeing:
 *   Use word of recall when level is "scary"
 *   Flee to stairs when there is a chance of death
 *   Avoid "stair bouncing" if at all possible
 *
 * Note that the various "flow" actions allow the Borg to flow
 * "through" closed doors, which will be opened when he attempts
 * to pass through them, so we do not have to pursue terrain until
 * all monsters and objects have been dealt with.
 *
 * XXX XXX XXX The poor Borg often kills a nasty monster, and
 * then takes a nap to recover from damage, but gets yanked
 * back to town before he can collect his reward.
 */
bool borg_think_dungeon(void)
{
    int i, j;
    int b_j = -1;

    int         msec = op_ptr->delay_factor * op_ptr->delay_factor;

    /* Hack -- Allow the user to stop the borg at certain levels */
    if (auto_depth == auto_stop_dlevel) borg_oops("Auto-stop at DLevel.");
    if (auto_level == auto_stop_clevel) borg_oops("Auto-stop at CLevel.");

    /* Hack -- prevent clock wrapping Step 1*/
    if (c_t >= 28000)
    {
        /* Clear Possible errors */
        borg_keypress(ESCAPE);
        borg_keypress(ESCAPE);
        borg_keypress(ESCAPE);
        borg_keypress(ESCAPE);

        /* Re-examine inven and equip */
        borg_cheat_inven();
        borg_cheat_equip();

        /* Continue on */
        return (TRUE);
    }

    /* Hack -- prevent clock wrapping step 2*/
    if (c_t >= 30000)
    {
        /* Panic */
        borg_oops("clock overflow");

        /* Oops */
        return (TRUE);
    }

    /* Idle for a moment on respawning borgs */
    if (borg_respawn >1)
    {
        borg_keypress(' ');
        borg_keypress(ESCAPE);
        borg_respawn --;
        return (TRUE);
    }

    /* add a short pause to slow the borg down for viewing */
    Term_xtra(TERM_XTRA_DELAY, msec);

    /* redraw the screen if we need to */
    if (my_need_redraw)
    {
        borg_note(format("#  Redrawing screen."));
        do_cmd_redraw();
        my_need_redraw = FALSE;
    }
    /* Prevent clock overflow */
    if (c_t - auto_began >= 10000)
    {
        /* Start leaving */
        if (!goal_leaving)
        {
            /* Note */
            borg_note("# Leaving (boredom)");

            /* Start leaving */
            goal_leaving = TRUE;
        }

        /* Start fleeing */
        if (!goal_fleeing)
        {
            /* Note */
            borg_note("# Fleeing (boredom)");

            /* Start fleeing */
            goal_fleeing = TRUE;
        }
    }

    /* Prevent a "bouncing Borg" bug. Where borg with telepathy
     * will sit in a narrow area bouncing between 2 or 3 places
     * tracking and flowing to a bouncing monster behind a wall.
     */
    if (auto_depth && (time_this_panel >= 500))
    {

        /* Start leaving */
        if (!goal_leaving)
        {
            /* Note */
            borg_note("# Leaving (bouncing-borg)");

            /* Start leaving */
            goal_leaving = TRUE;
        }

        /* Start fleeing */
        if (!goal_fleeing)
        {
            /* Note */
            borg_note("# Fleeing (bouncing-borg)");

            /* Start fleeing */
            goal_fleeing = TRUE;
        }

    }

    /* Avoid annoying farming */
    if (c_t - auto_began >= 2000)
    {
        /* Ignore monsters from boredom */
        if (!goal_ignoring)
        {
            /* Flee */
            borg_note("# Ignoring breeders (boredom)");

            /* Ignore multipliers */
            goal_ignoring = TRUE;
        }
    }


    /* Count the awake breeders */
    for (j = 0, i = 1; i < auto_kills_nxt; i++)
    {
        auto_kill *kill = &auto_kills[i];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Skip sleeping monsters */
        if (!kill->awake) continue;

        /* Count the monsters which are "breeders" */
        if (r_info[kill->r_idx].flags2 & RF2_MULTIPLY) j++;
    }

    /* hack -- close doors */
    if (j >=4)
    {
        /* set the flag to close doors */
        breeder_level = TRUE;
    }


    /* Hack -- caution */
    if (j >= MIN(auto_level,4) && (amt_recall <= 0 || auto_level < 35))
    {
        /* Ignore monsters from caution */
        if (!goal_ignoring)
        {
            /* Flee */
            borg_note("# Ignoring breeders (no recall)");

            /* Ignore multipliers */
            goal_ignoring = TRUE;
        }

        /* Start leaving */
        if (!goal_leaving)
        {
            /* Note */
            borg_note("# Leaving (no recall)");

            /* Start leaving */
            goal_leaving = TRUE;
        }

        /* Start fleeing */
        if (!goal_fleeing)
        {
            /* Note */
            borg_note("# Fleeing (no recall)");

            /* Start fleeing */
            goal_fleeing = TRUE;
        }


    }

    /* Reset avoidance */
    if (avoidance != auto_chp)
    {
        /* Reset "avoidance" */
        avoidance = auto_chp;

        /* Re-calculate danger */
        auto_danger_wipe = TRUE;

        /* Forget goals */
        goal = 0;
    }

    /* Keep Mages on a very short leaseh */
    if (track_less_num && auto_level < 5 && !goal_fleeing)
    {
        int y, x;

        /* Check for an existing "up stairs" */
        for (i = 0; i < track_less_num; i++)
        {
            x = track_less_x[i];
            y = track_less_y[i];

            /* How far is the nearest up stairs */
            j = distance(c_y, c_x, y, x);

            /* skip the closer ones */
            if (b_j >= j) continue;

            /* track it */
            b_j =j;
        }

        /* is the upstair too far away? */
        if (b_j > 50)
        {
            /* Start leaving */
            if (!goal_leaving)
            {
                /* Note */
                borg_note("# Leaving (wandered too far)");

                /* Start leaving */
                goal_leaving = TRUE;
            }

            /* Start fleeing */
            if (!goal_fleeing)
            {
                /* Note */
                borg_note("# Fleeing (wandered too far)");

                /* Start fleeing */
                goal_fleeing = TRUE;
            }
        }
    }


    /*** crucial goals ***/

    /* examine equipment and swaps */
    borg_notice(TRUE);

    /* require light-- */
    if (my_cur_lite <= 0 && auto_depth >= 1)
    {
        if (goal_recalling)
        {
            /* just wait */
            borg_keypress('R');
            borg_keypress('9');
            borg_keypress('\n');
            return (TRUE);
        }

        /* wear stuff and see if it glows */
        if (borg_wear_stuff()) return (TRUE);

        /* attempt to refuel */
        if (borg_refuel_torch() || borg_refuel_lantern()) return (TRUE);


        /* Can I recall out with a rod */
        if (!goal_recalling && borg_zap_rod(SV_ROD_RECALL)) return (TRUE);

        /* Test for stairs */
        borg_keypress('<');

        /* Try to flow to upstairs if on level one */
        if (borg_flow_stair_less(GOAL_FLEE))
        {
            /* Take the stairs */
            borg_keypress('<');
            return (TRUE);
        }

        /* Try to flow to a lite */
        if (amt_recall && borg_flow_light(GOAL_FLEE))
        {
            /* Can I recall out with a spell */
            if (!goal_recalling && borg_recall()) return (TRUE);
            return (TRUE);
        }
    }


    /*** Important goals ***/

    /* Try not to die */
    if (borg_caution()) return (TRUE);

    /*** if returning from dungeon in bad shape...***/
    if (!my_cur_lite || do_cut || do_poisoned || do_weak)
    {

        /* try to wear before moving */
        if (!my_cur_lite)
        {
            /* wear stuff and see if it glows */
            if (borg_wear_stuff()) return (TRUE);
        }

        if (borg_flow_shop_visit()) return (TRUE);
        if (borg_choose_shop())
        {
            /* Try and visit a shop, if so desired */
            if (borg_flow_shop_entry(goal_shop)) return (TRUE);
        }
    }

    /* Learn useful spells immediately */
    if (borg_play_magic(FALSE)) return (TRUE);

    /* Attack monsters */
    if (borg_attack()) return (TRUE);

    /* Wear things that need to be worn */
    if (borg_wear_stuff()) return (TRUE);

    /* Take off "Useless" things */
    if (borg_remove_stuff()) return (TRUE);

    /* Check the light */
    if (borg_check_lite()) return (TRUE);

    /* Recover from damage */
    if (borg_recover()) return (TRUE);

    /* Perform perma-spells */
    if (borg_perma_spell()) return (TRUE);

    /* Continue flowing towards monsters */
    if (borg_flow_old(GOAL_KILL)) return (TRUE);

    /* Find a (viewable) monster */
    if (borg_flow_kill(TRUE))
    {
         return (TRUE);
    }
    /*** Deal with inventory objects ***/

    /* Use things */
    if (borg_use_things()) return (TRUE);

    /* Identify unknown things */
    if (borg_test_stuff(FALSE)) return (TRUE);

    /* *Id* unknown things */
    if (borg_test_stuff(TRUE)) return (TRUE);

    /* Enchant things */
    if (borg_enchanting()) return (TRUE);

    /* Recharge things */
    if (borg_recharging()) return (TRUE);

    /* Destroy junk */
    if (borg_crush_junk()) return (TRUE);

    /* Destroy items to make space */
    if (borg_crush_hole()) return (TRUE);

    /* Destroy items if we are slow */
    if (borg_crush_slow()) return (TRUE);


    /*** Flee the level XXX XXX XXX ***/

    /* Flee the level */
    if (goal_fleeing && !goal_recalling)
    {
        /* Hack -- Take the next stairs */
        stair_less = stair_more = TRUE;

        /* Continue fleeing the level */
        if (borg_flow_old(GOAL_FLEE)) return (TRUE);

        /* Try to find some stairs up */
        if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

        /* Try to find some stairs down */
        if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);

        /* Try to hide on a glyph if no stairs */
        if (borg_flow_glyph(GOAL_FLEE)) return (TRUE);
    }


    /*** Flow towards objects ***/

    /* Continue flowing towards objects */
    if (borg_flow_old(GOAL_TAKE)) return (TRUE);

    /* Find a (viewable) object */
    if (borg_flow_take(TRUE)) return (TRUE);


    /*** Leave the level XXX XXX XXX ***/

    /* Leave the level */
    if (goal_leaving && !goal_recalling)
    {
        /* Hack -- Take the next stairs */
        if (borg_ready_morgoth == 0)
            stair_less = TRUE;

        /* Only go down if fleeing or prepared. */
        if ((cptr)NULL == borg_prepared(auto_depth+1))
            stair_more = TRUE;

        /* Continue leaving the level */
        if (borg_flow_old(GOAL_FLEE)) return (TRUE);

        /* Try to find some stairs up */
        if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

        /* Try to find some stairs down */
        if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);
    }


    /*** Exploration ***/

    /* Continue flowing (see below) */
    if (borg_flow_old(GOAL_MISC)) return (TRUE);

    /* Continue flowing (see below) */
    if (borg_flow_old(GOAL_DARK)) return (TRUE);

    /* Continue flowing (see below) */
    if (borg_flow_old(GOAL_XTRA)) return (TRUE);

    /* Continue flowing (see below) */
    if (borg_flow_old(GOAL_BORE)) return (TRUE);


    /*** Explore the dungeon ***/

    /* Chase old monsters */
    if (borg_flow_kill(FALSE)) return (TRUE);

    /* Chase old objects */
    if (borg_flow_take(FALSE)) return (TRUE);

    /* Explore interesting grids */
    if (borg_flow_dark(TRUE)) return (TRUE);

    /* Leave the level (if needed) */
    if (borg_leave_level(FALSE)) return (TRUE);

    /* Explore interesting grids */
    if (borg_flow_dark(FALSE)) return (TRUE);


    /*** Deal with shops ***/

    /* Hack -- visit all the shops */
    if (borg_flow_shop_visit()) return (TRUE);

    /* Hack -- Visit the shops */
    if (borg_choose_shop())
    {
        /* Try and visit a shop, if so desired */
        if (borg_flow_shop_entry(goal_shop)) return (TRUE);
    }



    /*** Leave the Level ***/
    /* Study/Test boring spells/prayers */
    if (borg_play_magic(TRUE)) return (TRUE);

    /* Search for secret doors */
    if (borg_flow_spastic(FALSE)) return (TRUE);

    /* Leave the level (if possible) */
    if (borg_leave_level(TRUE)) return (TRUE);

    /* Search for secret doors */
    if (borg_flow_spastic(TRUE)) return (TRUE);


    /*** Wait for recall ***/

    /* Wait for recall, unless in danger */
    if (goal_recalling && (borg_danger(c_y, c_x, 1) <= 0))
    {
        /* Take note */
        borg_note("# Waiting for Recall...");

        /* Rest until done */
        borg_keypress('R');
        borg_keypress('&');
        borg_keypress('\n');

        /* Done */
        return (TRUE);
    }


    /*** Nothing to do ***/

    /* The borg should never become twitchy in town.  If he does, then there
     * is a big problem which may lead to his demise from starvation.  If he
     * does twitch in town, he needs to reset that level and start shopping
     * all over again or run to the stairs quick.
     */
     /* HACK --reset the level on the next round */
     if (auto_depth == 0) old_depth = 5;

    /* Boost slightly */
    if (avoidance < auto_chp * 2)
    {
        bool done = FALSE;

        /* Note */
        borg_note(format("# Boosting bravery (1) from %d to %d!",
                         avoidance, auto_chp * 2));

        /* Hack -- ignore some danger */
        avoidance = (auto_chp * 2);

        /* Forget the danger fields */
        auto_danger_wipe = TRUE;

        /* Try anything */
        if (borg_think_dungeon_brave()) done = TRUE;

        /* Reset "avoidance" */
        avoidance = auto_chp;

        /* Re-calculate danger */
        auto_danger_wipe = TRUE;

        /* Forget goals */
        goal = 0;

        /* Done */
        if (done) return (TRUE);
    }

    /* try phase before boosting bravery further and acting goofy */
    borg_times_twitch++;

    /* Phase to get out of being twitchy up to 3 times per level. */
    if (borg_times_twitch < 3)
    {
        borg_note("# Considering Phase (twitchy)");

        /* Phase */
        if ( amt_phase && borg_caution_phase(15) &&
            (borg_spell(1, 4)  ||
             borg_prayer(4, 0) ||
             borg_activate_artifact(ART_BELEGENNON,INVEN_BODY)||
             borg_read_scroll(SV_SCROLL_PHASE_DOOR) ))
        {
            /* Success */
            return (TRUE);
        }

    }

    /* Start leaving */
    if (!auto_depth && !goal_leaving)
    {
        /* Note */
        borg_note("# Leaving (twitchy)");

        /* Start leaving */
        goal_leaving = TRUE;
    }

    /* Start fleeing */
    if (!auto_depth && !goal_fleeing)
    {
        /* Note */
        borg_note("# Fleeing (twitchy)");

        /* Start fleeing */
        goal_fleeing = TRUE;
    }

    /* Boost some more */
    if (avoidance < auto_mhp * 4)
    {
        bool done = FALSE;

        /* Note */
        borg_note(format("# Boosting bravery (2) from %d to %d!",
                         avoidance, auto_mhp * 4));

        /* Hack -- ignore some danger */
        avoidance = (auto_mhp * 4);

        /* Forget the danger fields */
        auto_danger_wipe = TRUE;

        /* Try anything */
        if (borg_think_dungeon_brave()) done = TRUE;

        /* Reset "avoidance" */
        avoidance = auto_chp;

        /* Re-calculate danger */
        auto_danger_wipe = TRUE;

        /* Forget goals */
        goal = 0;

        /* Done */
        if (done) return (TRUE);
    }

    /* Boost a lot */
    if (avoidance < 30000)
    {
        bool done = FALSE;

        /* Note */
        borg_note(format("# Boosting bravery (3) from %d to %d!",
                         avoidance, 30000));

        /* Hack -- ignore some danger */
        avoidance = 30000;

        /* Forget the danger fields */
        auto_danger_wipe = TRUE;

        /* Try anything */
        if (borg_think_dungeon_brave()) done = TRUE;

        /* Reset "avoidance" */
        avoidance = auto_chp;

        /* Re-calculate danger */
        auto_danger_wipe = TRUE;

        /* Forget goals */
        goal = 0;

        /* Done */
        if (done) return (TRUE);
    }

    /* try teleporting before acting goofy */
    borg_times_twitch++;


    /* Teleport to get out of being twitchy up to 5 times per level. */
    if (borg_times_twitch < 5)
    {
        borg_note("# Teleport (twitchy)");

        /* Teleport */
        if ( borg_spell(2, 6) ||
             borg_prayer(4, 1) ||
             borg_prayer(1, 1) ||
             borg_use_staff(SV_STAFF_TELEPORTATION) ||
             borg_read_scroll(SV_SCROLL_TELEPORT) )
        {
            /* Success */
            return (TRUE);
        }
    }

    /* Recall to town */
    if (auto_depth && (borg_recall()))
    {
        /* Note */
        borg_note("# Recalling (twitchy)");

        /* Success */
        return (TRUE);
    }


    /* Twitch around */
    if (borg_twitchy()) return (TRUE);

    /* Oops */
    return (FALSE);
}




/*
 * Initialize this file
 */
void borg_init_8(void)
{
    /* Nothing */
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
