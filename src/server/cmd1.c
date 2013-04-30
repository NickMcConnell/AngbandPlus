/*
 * File: cmd1.c
 * Purpose: Searching, movement, and pickup
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Leon Marrick
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
#include "../common/tvalsval.h"
#include "attack.h"
#include "cmds.h"
#include "generate.h"
#include "monster/mon-timed.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "object/inventory.h"
#include "s-spells.h"
#include "squelch.h"
#include "wilderness.h"


/*
 * Search for hidden things.  Returns true if a search was attempted, returns
 * false when the player has a 0% chance of finding anything.  Prints messages
 * for negative confirmation when verbose mode is requested.
 */
bool search(int Ind, bool verbose)
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    int y, x, chance;
    bool found = FALSE;
    object_type *o_ptr;

    /* Start with base search ability */
    chance = p_ptr->state.skills[SKILL_SEARCH];

    /* Penalize various conditions */
    if (p_ptr->timed[TMD_BLIND] || no_light(p_ptr)) chance = chance / 10;
    if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) chance = chance / 10;

    /* Prevent fruitless searches */
    if (chance <= 0)
    {
        if (verbose)
        {
            msg(p_ptr, "You can't make out your surroundings well enough to search.");

            /* Cancel repeat */
            disturb(p_ptr, 0, 0);
        }

        return FALSE;
    }

    /* Search the nearby grids, which are always in bounds */
    for (y = (py - 1); y <= (py + 1); y++)
    {
        for (x = (px - 1); x <= (px + 1); x++)
        {
            /* Sometimes, notice things */
            if (magik(chance))
            {
                /* Invisible trap */
                if (cave_issecrettrap(cave_get(p_ptr->depth), y, x))
                {
                    found = TRUE;

                    /* Pick a trap */
                    pick_trap(p_ptr->depth, y, x);

                    /* Message */
                    msg(p_ptr, "You have found a trap.");

                    /* Disturb */
                    disturb(p_ptr, 0, 0);
                }

                /* Secret door */
                if (cave_issecretdoor(cave_get(p_ptr->depth), y, x))
                {
                    found = TRUE;

                    /* Message */
                    msg(p_ptr, "You have found a secret door.");

                    /* Pick a door */
                    place_closed_door(cave_get(p_ptr->depth), y, x);

                    /* Disturb */
                    disturb(p_ptr, 0, 0);

                    /* Give the player an EXP bump */
                    p_ptr->exp += 1;
                }

                /* Scan all objects in the grid */
                for (o_ptr = get_first_object(p_ptr->depth, y, x); o_ptr;
                    o_ptr = get_next_object(o_ptr))
                {
                    /* Skip if not a trapped chest */
                    if (!is_trapped_chest(o_ptr)) continue;

                    /* Identify once */
                    if (!object_is_known(p_ptr, o_ptr))
                    {
                        found = TRUE;

                        /* Message */
                        msg(p_ptr, "You have discovered a trap on the chest!");

                        /* Know the trap */
                        object_notice_everything(p_ptr, o_ptr, TRUE);

                        /* Notice it */
                        disturb(p_ptr, 0, 0);
                    }
                }
            }
        }
    }

    if (verbose && !found)
    {
        if (chance >= 100) msg(p_ptr, "There are no secrets here.");
        else msg(p_ptr, "You found nothing.");
    }

    return TRUE;
}


/*** Pickup ***/


/*
 * Pickup all gold at the player's current location.
 */
static void py_pickup_gold(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    s32b total_gold = 0L;
    byte *treasure;
    s16b this_o_idx = 0;
    s16b next_o_idx = 0;
    object_type *o_ptr;
    int sound_msg;

    /* Allocate an array of ordinary gold objects */
    treasure = C_ZNEW(SV_GOLD_MAX, byte);

    /* Pick up all the ordinary gold objects */
    for (this_o_idx = cave_get(p_ptr->depth)->o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
    {
        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Ignore if not legal treasure */
        if ((o_ptr->tval != TV_GOLD) || (o_ptr->sval >= SV_GOLD_MAX))
            continue;

        /* Cannot carry infinite gold */
        if ((p_ptr->au + total_gold) > PY_MAX_GOLD)
        {
            msg(p_ptr, "Your purse is full!");
            continue;
        }

        /* Restricted by choice */
        if (!is_owner(p_ptr, o_ptr))
        {
            msg(p_ptr, "This pile of gold belongs to someone else!");
            continue;
        }

        /* Note that we have this kind of treasure */
        treasure[o_ptr->sval]++;

        /* Increment total value */
        total_gold += (s32b)o_ptr->pval[DEFAULT_PVAL];

        /* Pile of gold is now owned */
        object_own(p_ptr, o_ptr);

        /* Delete the gold */
        delete_object_idx(this_o_idx);
    }

    /* Pick up the gold, if present */
    if (total_gold)
    {
        char buf[MSG_LEN];
        int i, count, total;
        object_kind *kind;

        /* Disturb */
        disturb(p_ptr, 0, 0);

        /* Build a message */
        strnfmt(buf, sizeof(buf), "You have found %ld gold pieces worth of ",
            (long)total_gold);

        /* Count the types of treasure present */
        for (total = 0, i = 0; i < SV_GOLD_MAX; i++)
        {
            if (treasure[i]) total++;
        }

        /* List the treasure types */
        for (count = 0, i = 0; i < SV_GOLD_MAX; i++)
        {
            /* Skip if no treasure of this type */
            if (!treasure[i]) continue;

            /* Get this object index */
            kind = lookup_kind(TV_GOLD, i);
            if (!kind) continue;

            /* Build up the pickup string */
            my_strcat(buf, kind->name, sizeof(buf));

            /* Added another kind of treasure */
            count++;

            /* Add a comma if necessary */
            if ((total > 2) && (count < total)) my_strcat(buf, ",", sizeof(buf));

            /* Add an "and" if necessary */
            if ((total >= 2) && (count == total - 1))
                my_strcat(buf, " and", sizeof(buf));

            /* Add a space or period if necessary */
            if (count < total) my_strcat(buf, " ", sizeof(buf));
            else my_strcat(buf, ".", sizeof(buf));
        }

        /* Determine which sound to play */
        if (total_gold < 200) sound_msg = MSG_MONEY1;
        else if (total_gold < 600) sound_msg = MSG_MONEY2;
        else sound_msg = MSG_MONEY3;

        /* Display the message */
        msgt(p_ptr, sound_msg, buf);

        /* Add gold to purse */
        p_ptr->au += total_gold;

        /* Redraw gold */
        p_ptr->redraw |= (PR_GOLD);
    }

    /* Free the gold array */
    mem_free(treasure);
}


/*
 * Determine if the object can be picked up automatically.
 */
static bool auto_pickup_okay(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);
    const char *s;

    /*** Negative checks ***/

    /* Winners cannot pickup artifacts except the Crown and Grond */
    if (true_artifact_p(o_ptr) && restrict_winner(p_ptr, o_ptr))
        return (FALSE);

    /* Restricted by choice */
    if (true_artifact_p(o_ptr) && restrict_artifacts(p_ptr, o_ptr))
        return (FALSE);

    /* It can't be carried */
    if (!inven_carry_okay(p_ptr, o_ptr)) return (FALSE);

    /* Note that the pack is too heavy */
    if (!weight_okay(p_ptr, o_ptr)) return (FALSE);

    /* Restricted by choice */
    if (!is_owner(p_ptr, o_ptr)) return (FALSE);

    /* Ignore squelched items */
    if (squelch_item_ok(p_ptr, o_ptr)) return (FALSE);

    /* The inscription prevents it */
    if (protected_p(p_ptr, o_ptr, 'g', FALSE)) return (FALSE);

    /*** Positive checks ***/

    /* Vacuum up everything if requested */
    if (OPT_P(p_ptr, pickup_always)) return (TRUE);

    /* Check inscription */
    if (o_ptr->note)
    {
        /* Find a '=' */
        s = strchr(quark_str(o_ptr->note), '=');

        /* Process permissions */
        while (s)
        {
            /* =g ('g'et) means auto pickup */
            if (s[1] == 'g') return (TRUE);

            /* Find another '=' */
            s = strchr(s + 1, '=');
        }
    }

    /* Pickup if it matches the inventory */
    if (OPT_P(p_ptr, pickup_inven) && inven_stack_okay(p_ptr, o_ptr)) return (TRUE);

    /* Don't auto pickup */
    return (FALSE);
}


/*
 * Carry an object and delete it.
 */
static void py_pickup_aux(int Ind, int o_idx, bool domsg)
{
    player_type *p_ptr = player_get(Ind);
    int slot, quiver_slot = 0;
    char o_name[NORMAL_WID];
    object_type *o_ptr = object_byid(o_idx);

    /* Auto-squelch */
    if (p_ptr->squelch)
    {
        /* Set squelch flag as appropriate */
        p_ptr->notice |= PN_SQUELCH;

        /* Mark item as squelchable */
        o_ptr->squelch = SQUELCH_ALLOW;
    }
    else
    {
        /* Bypass auto-squelch */
        o_ptr->squelch = SQUELCH_PROTECT;
    }

    /* Carry the object */
    slot = inven_carry(p_ptr, o_ptr, TRUE);

    /* Handle errors (paranoia) */
    if (slot < 0) return;

    /*
     * If we have picked up ammo which matches something in the quiver, note
     * that it so that we can wield it later (and suppress pick up message)
     */
    if (obj_is_ammo(p_ptr, o_ptr))
    {
        int i;

        for (i = QUIVER_START; i < QUIVER_END; i++)
        {
            if (!p_ptr->inventory[i].kind) continue;
            if (!object_similar(&p_ptr->inventory[i], o_ptr, OSTACK_QUIVER)) continue;
            quiver_slot = i;
            break;
        }
    }

    /* Get the new object */
    o_ptr = &p_ptr->inventory[slot];

    /* Automatically sense artifacts */
    object_sense_artifact(p_ptr, o_ptr);

    /* Optionally, display a message */
    if (domsg && !quiver_slot)
    {
        /* Describe the object */
        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

        /* Message */
        msg(p_ptr, "You have %s (%c).", o_name, index_to_label(slot));
    }

    /* Delete the object */
    delete_object_idx(o_idx);

    /* If we have a quiver slot that this ammo matches, use it */
    if (quiver_slot) wield_item(Ind, o_ptr, slot, quiver_slot, FALSE);
}


static bool allow_pickup_object(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    /* Winners cannot pick up artifacts except the Crown and Grond */
    if (true_artifact_p(o_ptr) && restrict_winner(p_ptr, o_ptr))
        return FALSE;

    /* Restricted by choice */
    if (true_artifact_p(o_ptr) && restrict_artifacts(p_ptr, o_ptr))
        return FALSE;

    /* Restricted by choice */
    if (!is_owner(p_ptr, o_ptr)) return FALSE;

    return TRUE;
}


byte do_autopickup(int Ind, int pickup)
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    s16b this_o_idx, next_o_idx;
    object_type *o_ptr;
    byte objs_picked_up = 0;

    /* Nothing to pick up -- Return */
    if (!cave_get(p_ptr->depth)->o_idx[py][px]) return 0;

    /* Normal ghosts cannot pick things up */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY)) return 0;

    /* Always pickup gold, effortlessly */
    /* Hack -- Ghosts don't pick up gold automatically */
    if (!(p_ptr->ghost && (pickup < 2))) py_pickup_gold(Ind);

    /* Scan the remaining objects */
    for (this_o_idx = cave_get(p_ptr->depth)->o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
    {
        bool auto_pickup;

        /* Get the object and the next object */
        o_ptr = object_byid(this_o_idx);
        next_o_idx = o_ptr->next_o_idx;

        /* Ignore all non-objects */
        if (!o_ptr->kind) continue;

        /* Ignore all hidden objects */
        if (!object_marked(p_ptr, this_o_idx) || squelch_item_ok(p_ptr, o_ptr)) continue;

        /* Hack -- Disturb */
        if (!p_ptr->ghost) disturb(p_ptr, 0, 0);

        /* Auto-id */
        if (check_state(p_ptr, OF_KNOWLEDGE))
            object_notice_everything(p_ptr, o_ptr, FALSE);

        /* Hack -- Ghosts don't pick up gold automatically */
        auto_pickup = (pickup? TRUE: FALSE);
        if ((o_ptr->tval == TV_GOLD) && p_ptr->ghost && (pickup < 2))
            auto_pickup = FALSE;

        /* Automatically pick up items into the backpack */
        if (auto_pickup && auto_pickup_okay(Ind, o_ptr))
        {
            /* Pick up the object with message */
            py_pickup_aux(Ind, this_o_idx, TRUE);
            objs_picked_up++;
        }
    }

    return objs_picked_up;
}


static int see_floor_items(int Ind, int pickup, int *items, int max_size)
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    bool blind = (p_ptr->timed[TMD_BLIND] || no_light(p_ptr));
    int can_pickup = 0, can_lift = 0, allow_pickup = 0;
    s16b this_o_idx, next_o_idx;
    int num = 0;
    object_type *o_ptr;
    char o_name[NORMAL_WID];

    /* Scan the remaining objects */
    for (this_o_idx = cave_get(p_ptr->depth)->o_idx[py][px]; this_o_idx;
        this_o_idx = next_o_idx)
    {
        /* XXX Hack -- Enforce limit */
        if (num >= max_size) break;

        /* Get the object and the next object */
        o_ptr = object_byid(this_o_idx);
        next_o_idx = o_ptr->next_o_idx;

        /* Ignore all non-objects */
        if (!o_ptr->kind) continue;

        /* Ignore all hidden objects */
        if (squelch_item_ok(p_ptr, o_ptr)) continue;

        /* Tally objects and store them in an array. */

        /* Remember this object index */
        items[num] = this_o_idx;

        /* Count non-gold objects that remain on the floor. */
        num++;

        /* Tally objects that can be picked up. */
        if (inven_carry_okay(p_ptr, o_ptr)) can_pickup++;
        if (weight_okay(p_ptr, o_ptr)) can_lift++;
        if (allow_pickup_object(Ind, o_ptr)) allow_pickup++;
    }

    /* There are no objects left */
    if (!num) return 0;

    /* Mention the objects if player is not picking them up. */
    if ((pickup < 2) || !can_pickup || !can_lift || !allow_pickup)
    {
        const char *p = "see";

        /* One object */
        if (num == 1)
        {
            if (pickup < 2) {if (blind) p = "feel";}
            else if (!can_pickup) p = "have no room for";
            else if (!can_lift) p = "are already too burdened to pick up";
            else if (!allow_pickup) p = "are not allowed to pick up";

            /* Get the object */
            o_ptr = object_byid(items[0]);

            /* Describe the object. Less detail if blind. */
            if (blind)
                object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);
            else
                object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

            /* Message */
            msg(p_ptr, "You %s %s.", p, o_name);
        }

        /* Several items */
        else
        {
            /* Display more information about floor items */
            if (pickup < 2) {if (blind) p = "feel something on the floor";}
            else if (!can_pickup)
                p = "have no room for the following objects";
            else if (!can_lift)
                p = "are already too burdened to pick up the following objects";
            else if (!allow_pickup)
                p = "are not allowed to pick up the following objects";

            /* Display objects on the floor */
            display_floor(p_ptr, items, num);
            show_floor(Ind, OLIST_WEIGHT);

            /* Display prompt */
            msg(p_ptr, "You %s: ", p);
        }

        /* Done */
        return 0;
    }

    return num;
}


static bool floor_purchase(int Ind, int pickup, s16b this_o_idx)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr = object_byid(this_o_idx);
    char o_name[NORMAL_WID];

    /* Hack -- allow purchase from floor */
    if (protected_p(p_ptr, o_ptr, 'g', FALSE))
    {
        int i;
        player_type *q_ptr;
        bool okay = FALSE;
        object_type *i_ptr;
        object_type object_type_body;
        s32b price;
        char buf[NORMAL_WID];

        /* Get item owner */
        for (i = 1; i < NumPlayers + 1; i++)
        {
            q_ptr = player_get(i);
            if (q_ptr->id == o_ptr->owner)
            {
                okay = p_ptr->play_los[i];
                break;
            }
        }
        if (!okay)
        {
            msg(p_ptr, "Item owner must be nearby!");
            return FALSE;
        }

        /* Get local object */
        i_ptr = &object_type_body;

        /* Get a copy of the object */
        object_copy(i_ptr, o_ptr);

        /* Identify object to get real price */
        object_notice_everything(p_ptr, i_ptr, TRUE);

        /* Get the price */
        price = player_price_item(Ind, i_ptr);
        __trapR(p_ptr, price < 0, 0);
        if (p_ptr->au < price)
        {
            msg(p_ptr, "You do not have enough gold.");
            return FALSE;
        }

        /* Tell the client about the price */
        if (pickup < 4)
        {
            p_ptr->current_value = 0 - this_o_idx;
            Send_store_sell(Ind, price, FALSE);
            return FALSE;
        }

        /* Bypass auto-squelch */
        o_ptr->squelch = SQUELCH_PROTECT;

        /* Perform the transaction */
        p_ptr->au -= price;
        p_ptr->redraw |= PR_GOLD;
        if (!OPT_P(q_ptr, birth_no_selling))
        {
            q_ptr->au += price;
            q_ptr->redraw |= PR_GOLD;
        }

        /* Identify original object */
        object_notice_everything(p_ptr, o_ptr, FALSE);

        /* Describe the object (short name) */
        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_SALE);

        /* Combine / Reorder the pack (later) */
        p_ptr->notice |= (PN_COMBINE | PN_REORDER);

        /* Message */
        msg(p_ptr, "You bought %s for %ld gold.", o_name, (long)price);
        if (!OPT_P(q_ptr, birth_no_selling))
            msg(q_ptr, "You sold %s for %ld gold.", o_name, (long)price);

        /* Erase the inscription */
        o_ptr->note = 0;

        /* Set origin */
        set_origin(o_ptr, ORIGIN_PLAYER, p_ptr->depth, 0);

        /* Mark artifact as sold */
        set_artifact_info(q_ptr, o_ptr, ARTS_SOLD);

        /* Audit */
        strnfmt(buf, sizeof(buf), "PS %s-%d | %s-%d $ %ld", p_ptr->name,
            (int)p_ptr->id, q_ptr->name, (int)q_ptr->id, (long)price);
        audit(buf);
        audit("PS+gold");
    }

    return TRUE;
}


/*
 * Pick up objects and treasure on the floor.
 *
 * Called with pickup:
 * 0 to pickup gold and describe other objects (when running)
 * 1 to act according to the player's settings (auto-pickup check when walking)
 * 2 to quickly pickup single objects or present a menu for more (pickup command)
 * 3 to force a menu for any number of objects (recursive call)
 * 4 to purchase an item from the floor
 *
 * Scan the list of objects in that floor grid. Pick up gold automatically
 * Pick up objects automatically until backpack space is full if
 * auto-pickup option is on, Otherwise, store objects on
 * floor in an array, and tally both how many there are and can be picked up.
 *
 * If not picking up anything, indicate objects on the floor.
 *
 * If we are picking up objects automatically, and have room for at least
 * one, display information about objects and prompt the player.  Otherwise,
 * automatically pick up a single object or use a menu for more than one.
 *
 * Pick up multiple objects using Tim Baker's menu system. Recursively
 * call this function (forcing menus for any number of objects) until
 * objects are gone, backpack is full, or player is satisfied.
 *
 * We keep track of number of objects picked up to calculate time spent.
 * This tally is incremented even for automatic pickup, so we are careful
 * (in "dungeon.c" and elsewhere) to handle pickup as either a separate
 * automated move or a no-cost part of the stay still or 'g'et command.
 *
 * Note the lack of chance for the character to be disturbed by unmarked
 * objects. They are truly "unknown".
 */
byte py_pickup(int Ind, int pickup)
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    s16b this_o_idx = 0;
    size_t floor_num = 0;
    int floor_list[MAX_FLOOR_STACK];
    bool call_function_again = FALSE;
    bool domsg = TRUE;
    byte objs_picked_up = 0;

    /* Nothing to pick up -- Return */
    if (!cave_get(p_ptr->depth)->o_idx[py][px]) return 0;

    /* Normal ghosts cannot pick things up */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY)) return 0;

    /* Mention the objects if player is not picking them up. */
    floor_num = see_floor_items(Ind, pickup, floor_list, N_ELEMENTS(floor_list));
    if (!floor_num) return objs_picked_up;

    /* We can pick up objects. Menus are not requested (yet). */
    if (pickup == 2)
    {
        /* Use a menu interface for multiple objects, or pickup single objects */
        if (floor_num > 1) pickup = 3;
        else this_o_idx = floor_list[0];
    }

    /* Display a list if requested. */
    if (pickup == 3)
    {
        int item;
        object_type *o_ptr;

        /* No item -> get one */
        if (p_ptr->current_value == ITEM_REQUEST)
        {
            /* Update the floor on the client */
            display_floor(p_ptr, floor_list, floor_num);

            get_item(p_ptr, 0, HOOK_CARRY);
            return objs_picked_up;
        }

        /* Use current */
        item = p_ptr->current_value;

        /* Get the item */
        o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

        /* Paranoia: requires an item */
        if (!o_ptr || !o_ptr->kind) return objs_picked_up;

        /* Some checks */
        if (item < 0)
        {
            /* Winners cannot pick up artifacts except the Crown and Grond */
            if (true_artifact_p(o_ptr) && restrict_winner(p_ptr, o_ptr))
            {
                msg(p_ptr, "You cannot pick up that item anymore.");
                return objs_picked_up;
            }

            /* Restricted by choice */
            if (true_artifact_p(o_ptr) && restrict_artifacts(p_ptr, o_ptr))
            {
                msg(p_ptr, "You cannot pick up that item.");
                return objs_picked_up;
            }

            /* Restricted by choice */
            if (!is_owner(p_ptr, o_ptr))
            {
                msg(p_ptr, "This item belongs to someone else!");
                return objs_picked_up;
            }
        }

        /* Use current */
        this_o_idx = 0 - item;
        call_function_again = TRUE;

        /* With a list, we do not need explicit pickup messages */
        domsg = FALSE;
    }

    /* Hack -- Allow purchase from floor */
    if (pickup == 4)
    {
        int item;
        object_type *o_ptr;

        /* Use current */
        item = p_ptr->current_value;

        /* Get the item */
        o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

        /* Paranoia: requires an item */
        if (!o_ptr || !o_ptr->kind) return objs_picked_up;

        /* Use current */
        this_o_idx = 0 - item;
        call_function_again = TRUE;

        /* No explicit pickup message */
        domsg = FALSE;
    }

    /* Pick up object, if legal */
    if (this_o_idx)
    {
        /* Hack -- allow purchase from floor */
        if (!floor_purchase(Ind, pickup, this_o_idx)) return objs_picked_up;

        /* Pick up the object */
        py_pickup_aux(Ind, this_o_idx, domsg);

        /* Indicate an object picked up. */
        objs_picked_up = 1;
    }

    /*
     * If requested, call this function recursively. Count objects picked up.
     * Force the display of a menu in all cases.
     */
    if (call_function_again)
    {
        current_clear(p_ptr);
        objs_picked_up += py_pickup(Ind, 3);
    }

    /* Indicate how many objects have been picked up. */
    return objs_picked_up;
}


static const char *comment_ironman[] =
{
    "You don't feel like going to pick flowers right now.",
    "Where do you think you are going?",
    "Morgoth the potato farmer? - get real!",
    "Morgoth awaits you in the depths not in the fields.",
    "Something draws your attention back to the stairs."
};


/* Do a probability travel in a wall */
static void do_prob_travel(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int x = p_ptr->px, y = p_ptr->py;
    bool do_move = TRUE;

    /* Paranoia */
    if ((dir == 5) || !dir) return;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    x += ddx[dir];
    y += ddy[dir];

    while (TRUE)
    {
        /* Do not get out of the level */
        if (!in_bounds_fully(y, x))
        {
            do_move = FALSE;
            break;
        }

        /* Require an "empty" floor grid */
        if (!cave_empty_bold(p_ptr->depth, y, x) || is_icky(p_ptr->depth, y, x))
        {
            y += ddy[dir];
            x += ddx[dir];
            continue;
        }

        /* Everything is ok */
        do_move = TRUE;
        break;
    }

    if (do_move) monster_swap(p_ptr->depth, p_ptr->py, p_ptr->px, y, x);
}


/*
 * Move player in the given direction.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
void move_player(int Ind, int dir, bool disarm, bool check_pickup, bool force)
{
    player_type *p_ptr = player_get(Ind);
    int y, x, m_idx;
    bool old_dtrap, new_dtrap;
    bool do_move = TRUE;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];
    m_idx = cave_get(p_ptr->depth)->m_idx[y][x];

    /* Handle polymorphed players */
    if (p_ptr->r_idx)
    {
        monster_race *r_ptr = &r_info[p_ptr->r_idx];

        if (rf_has(r_ptr->flags, RF_NEVER_MOVE)) do_move = FALSE;

        /* Unaware players trying to move reveal themselves */
        if (p_ptr->k_idx) aware_player(p_ptr, p_ptr);
    }

    /* New player location on the world map */
    if ((p_ptr->depth <= 0) && !in_bounds_fully(y, x))
    {
        int new_world_x = p_ptr->world_x, new_world_y = p_ptr->world_y;
        int new_x = p_ptr->px, new_y = p_ptr->py;
        int new_world_index;

        /* Handle polymorphed players */
        if (!do_move)
        {
            msg(p_ptr, "You cannot move!");
            return;
        }

        /* Leaving town */
        if (!p_ptr->depth)
        {
            /* Forbid */
            if (cfg_no_recall || cfg_town_wall || OPT_P(p_ptr, birth_ironman))
            {
                if (!cfg_town_wall)
                    msg(p_ptr, ONE_OF(comment_ironman));
                else
                    msg(p_ptr, "There is a wall blocking your way.");
                disturb(p_ptr, 1, 0);
                return;
            }

            /* Warn */
            if (p_ptr->lev == 1)
                msg(p_ptr, "Really enter the wilderness? The dungeon entrance is in the town!");
        }

        /* Find his new location */
        if (y <= 0)
        {
            new_world_y++;
            new_y = DUNGEON_HGT - 2;
        }
        if (y >= DUNGEON_HGT - 1)
        {
            new_world_y--;
            new_y = 1;
        }
        if (x <= 0)
        {
            new_world_x--;
            new_x = DUNGEON_WID - 2;
        }
        if (x >= DUNGEON_WID - 1)
        {
            new_world_x++;
            new_x = 1;
        }

        /* New location */
        new_world_index = world_index(new_world_x, new_world_y);

        /* Check to make sure he hasnt hit the edge of the world */
        if (new_world_index < 0 - MAX_WILD) return;

        /* Hack -- DM redesigning the level */
        if (players_on_depth[new_world_index] == INHIBIT_DEPTH)
        {
            msg(p_ptr, "Something prevents you from going this way...");
            return;
        }

        /* Change location */
        dungeon_change_level(p_ptr, new_world_index, LEVEL_OUTSIDE);

        /* Hack -- Replace the player */
        p_ptr->world_x = new_world_x;
        p_ptr->world_y = new_world_y;
        p_ptr->old_px = p_ptr->px = new_x;
        p_ptr->old_py = p_ptr->py = new_y;

        /* Update the wilderness map */
        wild_set_explored(p_ptr, 0 - p_ptr->depth);

        /* Disturb if necessary */
        if (OPT_P(p_ptr, disturb_panel)) disturb(p_ptr, 0, 0);

        return;
    }

    /* Save "last direction moved" */
    p_ptr->last_dir = dir;

    /* Bump into other players */
    if (m_idx < 0)
    {
        int Ind2 = 0 - m_idx;
        player_type *q_ptr = player_get(Ind2);

        /* Don't bump into self! */
        if (Ind2 != Ind)
        {
            /* Reveal mimics */
            if (q_ptr->k_idx)
                aware_player(p_ptr, q_ptr);

            /* Check for an attack */
            if (pvp_check(p_ptr, q_ptr, PVP_DIRECT, TRUE, cave_get(p_ptr->depth)->feat[y][x]))
                py_attack(Ind, y, x);

            /* Handle polymorphed players */
            else if (!do_move)
                msg(p_ptr, "You cannot move!");

            /* Switch places */
            else if ((!player_passwall(p_ptr) && !player_passwall(q_ptr) &&
                (ddy[q_ptr->last_dir] == (0 - ddy[dir])) &&
                (ddx[q_ptr->last_dir] == (0 - ddx[dir]))) ||
                (q_ptr->dm_flags & DM_SECRET_PRESENCE))
            {
                monster_swap(p_ptr->depth, p_ptr->py, p_ptr->px, q_ptr->py, q_ptr->px);

                /* Don't tell people they bumped into the Dungeon Master */
                if (!(q_ptr->dm_flags & DM_SECRET_PRESENCE))
                {
                    char p_name[NORMAL_WID], q_name[NORMAL_WID];

                    player_desc(p_ptr, q_name, sizeof(q_name), q_ptr, FALSE);
                    player_desc(q_ptr, p_name, sizeof(p_name), p_ptr, FALSE);

                    /* Tell both of them */
                    msg(p_ptr, "You switch places with %s.", q_name);
                    msg(q_ptr, "You switch places with %s.", p_name);

                    /* Disturb both of them */
                    disturb(p_ptr, 1, 0);
                    disturb(q_ptr, 1, 0);
                }

                /* Unhack both of them */
                q_ptr->last_dir = p_ptr->last_dir = 5;
            }

            /* Bump into other players */
            else if (!(p_ptr->dm_flags & DM_SECRET_PRESENCE))
            {
                char p_name[NORMAL_WID], q_name[NORMAL_WID];

                player_desc(p_ptr, q_name, sizeof(q_name), q_ptr, FALSE);
                player_desc(q_ptr, p_name, sizeof(p_name), p_ptr, TRUE);

                /* Tell both about it */
                msg(p_ptr, "You bump into %s.", q_name);
                msg(q_ptr, "%s bumps into you.", p_name);

                /* Disturb both parties */
                disturb(p_ptr, 1, 0);
                disturb(q_ptr, 1, 0);
            }
        }

        return;
    }

    /* Bump into monsters */
    if (m_idx > 0)
    {
        monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

        /* Check for an attack */
        if (pvm_check(Ind, m_idx))
        {
            /* Mimics surprise the player */
            if (is_mimicking(m_ptr))
            {
                become_aware(p_ptr, m_ptr);

                /* Mimic wakes up */
                mon_clear_timed(p_ptr, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);
            }
            else
                py_attack(Ind, y, x);
        }

        /* Handle polymorphed players */
        else if (!do_move)
            msg(p_ptr, "You cannot move!");

        /* Reveal mimics */
        else if (is_mimicking(m_ptr))
            become_aware(p_ptr, m_ptr);

        /* Switch places */
        else
            monster_swap(p_ptr->depth, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx);

        return;
    }

    /* Arena */
    if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_PERM_ARENA)
    {
        access_arena(Ind, y, x);
        return;
    }

    /* Prob travel */
    if (p_ptr->timed[TMD_PROBTRAVEL] && !cave_floor_bold(p_ptr->depth, y, x))
    {
        do_prob_travel(Ind, dir);
        return;
    }

    /* Optionally alter traps/doors on movement */
    if (disarm && (p_ptr->cave->info[y][x] & CAVE_MARK) &&
        (cave_isbasicdoor(cave_get(p_ptr->depth), y, x) ||
        cave_isknowntrap(cave_get(p_ptr->depth), y, x)))
    {
        do_cmd_alter(Ind, dir);
        return;
    }

    /* Normal players can not walk through "walls" */
    if (!player_passwall(p_ptr) && !cave_floor_bold(p_ptr->depth, y, x))
    {
        /* Disturb the player */
        disturb(p_ptr, 0, 0);

        /* Notice unknown obstacles */
        if (!(p_ptr->cave->info[y][x] & CAVE_MARK))
        {
            /* Rubble */
            if (cave_isrubble(cave_get(p_ptr->depth), y, x))
            {
                msgt(p_ptr, MSG_HITWALL, "You feel a pile of rubble blocking your way.");
                p_ptr->cave->info[y][x] |= CAVE_MARK;
                cave_light_spot_aux(p_ptr, cave_get(p_ptr->depth), y, x);
            }

            /* Closed door */
            else if (cave_isbasicdoor(cave_get(p_ptr->depth), y, x))
            {
                msgt(p_ptr, MSG_HITWALL, "You feel a door blocking your way.");
                p_ptr->cave->info[y][x] |= CAVE_MARK;
                cave_light_spot_aux(p_ptr, cave_get(p_ptr->depth), y, x);
            }

            /* Tree */
            else if (cave_tree_basic(cave_get(p_ptr->depth)->feat[y][x]))
            {
                msgt(p_ptr, MSG_HITWALL, "You feel a tree blocking your way.");
                p_ptr->cave->info[y][x] |= CAVE_MARK;
                cave_light_spot_aux(p_ptr, cave_get(p_ptr->depth), y, x);
            }

            /* Wall (or secret door) */
            else
            {
                msgt(p_ptr, MSG_HITWALL, "You feel a wall blocking your way.");
                p_ptr->cave->info[y][x] |= CAVE_MARK;
                cave_light_spot_aux(p_ptr, cave_get(p_ptr->depth), y, x);
            }
        }

        /* Mention known obstacles */
        else
        {
            /* Rubble */
            if (cave_isrubble(cave_get(p_ptr->depth), y, x))
                msgt(p_ptr, MSG_HITWALL, "There is a pile of rubble blocking your way.");

            /* Closed doors */
            else if (cave_isbasicdoor(cave_get(p_ptr->depth), y, x))
                msgt(p_ptr, MSG_HITWALL, "There is a door blocking your way.");

            /* Tree */
            else if (cave_tree_basic(cave_get(p_ptr->depth)->feat[y][x]))
                msgt(p_ptr, MSG_HITWALL, "There is a tree blocking your way.");

            /* Wall (or secret door) */
            else
                msgt(p_ptr, MSG_HITWALL, "There is a wall blocking your way.");
        }

        return;
    }

    /* Permanent walls */
    if (player_passwall(p_ptr) && cave_isperm(cave_get(p_ptr->depth), y, x))
    {
        /* Forbid in most cases */
        if (p_ptr->timed[TMD_WRAITH] || player_can_undead(p_ptr) ||
            (cave_get(p_ptr->depth)->feat[y][x] == FEAT_PERM_SOLID))
        {
            /* Message */
            msg(p_ptr, "The wall blocks your movement.");

            disturb(p_ptr, 0, 0);
            return;
        }
    }

    /* Wraith trying to run inside a house */
    if (p_ptr->timed[TMD_WRAITH] && cave_ishomedoor(cave_get(p_ptr->depth), y, x))
    {
        do_cmd_open(Ind, dir, FALSE);
        return;
    }

    /* Handle polymorphed players */
    if (!do_move && !force)
    {
        msg(p_ptr, "You cannot move!");
        return;
    }

    /* See if trap detection status will change */
    old_dtrap = ((p_ptr->cave->info[p_ptr->py][p_ptr->px] & CAVE_DTRAP)? TRUE: FALSE);
    new_dtrap = ((p_ptr->cave->info[y][x] & CAVE_DTRAP)? TRUE: FALSE);

    /* Note the change in the detect status */
    if (old_dtrap != new_dtrap) p_ptr->redraw |= (PR_DTRAP);

    /* Disturb if the player is about to leave the area */
    if (OPT_P(p_ptr, disturb_detect) && p_ptr->running && !p_ptr->running_firststep &&
        old_dtrap && !new_dtrap && random_level(p_ptr->depth))
    {
        disturb(p_ptr, 0, 0);
        return;
    }

    /* Move player */
    monster_swap(p_ptr->depth, p_ptr->py, p_ptr->px, y, x);

    /* Searching */
    if (p_ptr->searching || (p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] >= 50) ||
        one_in_(50 - p_ptr->state.skills[SKILL_SEARCH_FREQUENCY]))
    {
        search(Ind, FALSE);
    }

    /* Handle "objects" */
    if (cave_get(p_ptr->depth)->o_idx[y][x])
    {
        p_ptr->squelch = 1;
        do_autopickup(Ind, check_pickup);
        current_clear(p_ptr);
        py_pickup(Ind, check_pickup);
    }

    /* Handle "store doors" */
    if (!p_ptr->ghost && cave_isshop(cave_get(p_ptr->depth), y, x))
    {
        /* Disturb */
        disturb(p_ptr, 0, 0);

        /* Hack -- Enter store */
        do_cmd_store(Ind, -1);
    }

    /* Handle resurrection */
    else if (p_ptr->ghost &&
        (cave_get(p_ptr->depth)->feat[y][x] == (FEAT_SHOP_HEAD + STORE_TEMPLE)))
    {
        /* Resurrect him */
        resurrect_player(Ind);

        /* Give him some gold */
        if (!is_dm_p(p_ptr) && !player_can_undead(p_ptr) && (p_ptr->lev >= 5))
            p_ptr->au = 100 * (p_ptr->lev - 4) / p_ptr->lives;
    }

    /* Discover invisible traps */
    else if (cave_issecrettrap(cave_get(p_ptr->depth), y, x))
    {
        /* Disturb */
        disturb(p_ptr, 0, 0);

        /* Message */
        msg(p_ptr, "You found a trap!");

        /* Pick a trap */
        pick_trap(p_ptr->depth, p_ptr->py, p_ptr->px);

        /* Hit the trap */
        hit_trap(Ind);
    }

    /* Set off a visible trap */
    else if (cave_isknowntrap(cave_get(p_ptr->depth), y, x))
    {
        /* Disturb */
        disturb(p_ptr, 0, 0);

        /* Hit the trap */
        hit_trap(Ind);
    }

    /* Mention fountains */
    else if (cave_fountain_basic(cave_get(p_ptr->depth)->feat[y][x]))
    {
        /* Disturb */
        disturb(p_ptr, 0, 0);

        /* Message */
        msg(p_ptr, "A fountain is located at this place.");
    }

    /*
     * Mega-hack -- If we are the dungeon master, and our movement hook
     * is set, call it.  This is used to make things like building walls
     * and summoning monster armies easier.
     */
    if (is_dm_p(p_ptr) && master_move_hook)
        master_move_hook(Ind, NULL);

    p_ptr->running_firststep = FALSE;
}


/* Check if basic quests have been completed */
bool quest_done(struct player *p, int depth)
{
    s16b r_idx;

    for (r_idx = 1; r_idx < z_info->r_max; r_idx++)
    {
        monster_race *r_ptr = &r_info[r_idx];
        monster_lore *l_ptr = &p->lore[r_idx];

        if (!r_ptr->name) continue;
        if (strcmp(r_ptr->name, "Sauron, the Sorcerer") &&
            strcmp(r_ptr->name, "Morgoth, Lord of Darkness") &&
            strcmp(r_ptr->name, "Senyakaze, Sorceress of the Nether Realm") &&
            strcmp(r_ptr->name, "Xakaze, Father of Abominations"))
        {
            continue;
        }

        /* Check depth and kill */
        if ((depth == r_ptr->level) && !l_ptr->pkills) return FALSE;
    }

    /* Ok */
    return TRUE;
}


void leave_depth(struct player *p)
{
    int i;
    monster_type *m_ptr;

    /* One less player here */
    if (players_on_depth[p->depth]) players_on_depth[p->depth]--;

    /* Free monsters from slavery */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        m_ptr = cave_monster(cave_get(p->depth), i);

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip non slaves */
        if (p->id != m_ptr->master) continue;

        /* Free monster from slavery */
        monster_set_master(m_ptr, NULL, MSTATUS_HOSTILE);
    }
    p->slaves = 0;
}


void use_energy(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Take a turn */
    p_ptr->energy -= level_speed(p_ptr->depth);

    /* Paranoia */
    if (p_ptr->energy < 0) p_ptr->energy = 0;
}


bool has_energy(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Check if we have enough energy */
    return (p_ptr->energy >= level_speed(p_ptr->depth));
}


bool weight_okay(struct player *p, object_type *o_ptr)
{
    int wgt;

    wgt = p->total_weight + o_ptr->weight * o_ptr->number;
    return (wgt <= weight_limit(&p->state) * 6);
}
