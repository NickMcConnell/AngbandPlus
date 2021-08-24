/*
 * File: cmd-pickup.c
 * Purpose: Pickup code
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Leon Marrick
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


/*
 * Pick up all gold at the player's current location.
 */
static void player_pickup_gold(struct player *p, struct chunk *c)
{
    s32b total_gold = 0L;
    char name[30] = "";
    int sound_msg;
    bool at_most_one = true;
    struct object *obj = square_object(c, &p->grid), *next;

    /* Pick up all the ordinary gold objects */
    while (obj)
    {
        struct object_kind *kind = NULL;

        /* Get next object */
        next = obj->next;

        /* Ignore if not legal treasure */
        kind = lookup_kind(obj->tval, obj->sval);
        if (!tval_is_money(obj) || !kind)
        {
            obj = next;
            continue;
        }

        /* Cannot carry infinite gold */
        if ((p->au + total_gold) > PY_MAX_GOLD)
        {
            msg(p, "Your purse is full!");
            obj = next;
            continue;
        }

        /* Restricted by choice */
        if (!is_owner(p, obj))
        {
            msg(p, "This pile of gold belongs to someone else!");
            obj = next;
            continue;
        }

        /* Multiple types if we have a second name, otherwise record the name */
        if (total_gold && !streq(kind->name, name))
            at_most_one = false;
        else
            my_strcpy(name, kind->name, sizeof(name));

        /* Increment total value */
        total_gold += (s32b)obj->pval;

        /* Pile of gold is now owned */
        object_own(p, obj);

        /* Delete the gold */
        square_excise_object(c, &p->grid, obj);
        object_delete(&obj);
        obj = next;
    }

    /* Pick up the gold, if present */
    if (total_gold)
    {
        char buf[100];

        /* Disturb */
        disturb(p, 0);

        /* Build a message */
        strnfmt(buf, sizeof(buf), "You have found %d gold piece%s worth of ", total_gold,
            PLURAL(total_gold));

        /* One treasure type.. */
        if (at_most_one) my_strcat(buf, name, sizeof(buf));

        /* ... or more */
        else my_strcat(buf, "treasures", sizeof(buf));

        my_strcat(buf, ".", sizeof(buf));

        /* Determine which sound to play */
        if (total_gold < 200) sound_msg = MSG_MONEY1;
        else if (total_gold < 600) sound_msg = MSG_MONEY2;
        else sound_msg = MSG_MONEY3;

        /* Display the message */
        msgt(p, sound_msg, buf);

        /* Add gold to purse */
        p->au += total_gold;

        /* Redraw gold */
        p->upkeep->redraw |= (PR_GOLD);
    }
}


/*
 * Looks if "inscrip" is present on the given object.
 */
static unsigned check_for_inscrip(const struct object *obj, const char *inscrip)
{
    unsigned i = 0;
    const char *s;

    if (!obj->note) return 0;

    s = quark_str(obj->note);
    if (!s) return 0;

    do
    {
        s = strstr(s, inscrip);
        if (!s) break;

        i++;
        s++;
    }
    while (s);

    return i;
}


/*
 * Find the specified object in the inventory (not equipment)
 */
static struct object *find_stack_object_in_inventory(struct player *p, const struct object *obj)
{
    struct object *gear_obj;

    for (gear_obj = p->gear; gear_obj; gear_obj = gear_obj->next)
    {
        if (!object_is_equipped(p->body, gear_obj) &&
            object_stackable(p, gear_obj, obj, OSTACK_PACK))
        {
            /* We found the object */
            return gear_obj;
        }
    }

    return NULL;
}


/*
 * Determine if an object can be picked up automatically.
 */
static bool auto_pickup_okay(struct player *p, struct object *obj)
{
    /*** Negative checks ***/

    /* Winners cannot pickup artifacts except the Crown and Grond */
    if (true_artifact_p(obj) && restrict_winner(p, obj))
        return false;

    /* Restricted by choice */
    if (obj->artifact && (cfg_no_artifacts || OPT(p, birth_no_artifacts)))
        return false;

    /* It can't be carried */
    if (!inven_carry_okay(p, obj)) return false;

    /* Note that the pack is too heavy */
    if (!weight_okay(p, obj)) return false;

    /* Restricted by choice */
    if (!is_owner(p, obj)) return false;

    /* Must meet level requirement */
    if (!has_level_req(p, obj)) return false;

    /* Ignore ignored items */
    if (ignore_item_ok(p, obj)) return false;

    /* Check preventive inscription '!g' */
    if (protected_p(p, obj, INSCRIPTION_PICKUP, false)) return false;

    /*** Positive checks ***/

    /* Vacuum up everything if requested */
    if (OPT(p, pickup_always)) return true;

    /* Check inscription */
    if (check_for_inscrip(obj, "=g")) return true;

    /* Pickup if it matches the inventory */
    if (OPT(p, pickup_inven))
    {
        struct object *gear_obj = find_stack_object_in_inventory(p, obj);

        if (inven_carry_num(p, obj, true) && !check_for_inscrip(gear_obj, "!g")) return true;
    }

    /* Don't auto pickup */
    return false;
}


/*
 * Move an object from a floor pile to the player's gear, checking first
 * whether partial pickup is needed
 */
static void player_pickup_aux(struct player *p, struct chunk *c, struct object *obj, int auto_max,
    bool domsg)
{
    int max = inven_carry_num(p, obj, false);

    /* Confirm at least some of the object can be picked up */
    if (!max) quit_fmt("Failed pickup of %s", obj->kind->name);

    /* Auto-ignore */
    if (p->ignore)
    {
        /* Set ignore status as appropriate */
        p->upkeep->notice |= PN_IGNORE;
    }
    else
    {
        /* Bypass auto-ignore */
        obj->ignore_protect = 1;
    }

    /* Carry the object, prompting for number if necessary */
    if (max == obj->number)
    {
        square_excise_object(c, &p->grid, obj);
        inven_carry(p, obj, true, domsg);
    }
    else
    {
        int num;
        bool dummy;
        struct object *picked_up;

        if (auto_max) num = auto_max;
        else num = max;
        if (!num) return;
        picked_up = floor_object_for_use(p, c, obj, num, false, &dummy);
        inven_carry(p, picked_up, true, domsg);
    }
}


static bool allow_pickup_object(struct player *p, struct object *obj)
{
    /* Winners cannot pick up artifacts except the Crown and Grond */
    if (true_artifact_p(obj) && restrict_winner(p, obj))
        return false;

    /* Restricted by choice */
    if (obj->artifact && (cfg_no_artifacts || OPT(p, birth_no_artifacts)))
        return false;

    /* Restricted by choice */
    if (!is_owner(p, obj)) return false;

    /* Must meet level requirement */
    if (!has_level_req(p, obj)) return false;

    return true;
}


static int see_floor_items(struct player *p, struct chunk *c, int pickup,
    struct object **floor_list, int floor_max)
{
    size_t floor_num = 0;
    bool blind = (p->timed[TMD_BLIND] || no_light(p));
    const char *prompt = "see";
    bool can_pickup = false, can_lift = false, allow_pickup = false;
    size_t i;
    char o_name[NORMAL_WID];

    /* Scan all visible, sensed objects in the grid */
    floor_num = scan_floor(p, c, floor_list, floor_max, OFLOOR_VISIBLE, NULL);
    if (floor_num == 0) return 0;

    /* Can we pick any up? */
    for (i = 0; i < floor_num; i++)
    {
        if (inven_carry_okay(p, floor_list[i])) can_pickup = true;
        if (weight_okay(p, floor_list[i])) can_lift = true;
        if (allow_pickup_object(p, floor_list[i])) allow_pickup = true;
    }

    /* Mention the objects if player is not picking them up. */
    if ((pickup >= 2) && can_pickup && can_lift && allow_pickup) return floor_num;

    if (pickup < 2) {if (blind) prompt = "feel";}
    else if (!can_pickup) prompt = "have no room for";
    else if (!can_lift) prompt = "are already too burdened to pick up";
    else if (!allow_pickup) prompt = "are not allowed to pick up";

    /* Describe the top object. Less detail if blind. */
    if (blind)
        object_desc(p, o_name, sizeof(o_name), floor_list[0], ODESC_PREFIX | ODESC_BASE);
    else
        object_desc(p, o_name, sizeof(o_name), floor_list[0], ODESC_PREFIX | ODESC_FULL);

    /* Message */
    if (floor_num == 1)
        msg(p, "You %s %s.", prompt, o_name);
    else
        msg(p, "You %s %s (on a pile).", prompt, o_name);

    /* Done */
    return 0;
}


static bool floor_purchase(struct player *p, struct chunk *c, int pickup, struct object *obj)
{
    char o_name[NORMAL_WID];

    /* Hack -- allow purchase from floor */
    if (protected_p(p, obj, INSCRIPTION_PICKUP, false))
    {
        int i;
        struct player *q;
        bool okay = false;
        struct object *full = object_new();
        s32b price;
        char buf[NORMAL_WID];

        /* Get item owner */
        for (i = 1; i <= NumPlayers; i++)
        {
            q = player_get(i);
            if (q->id == obj->owner)
            {
                okay = player_is_in_view(p, i);
                break;
            }
        }
        if (!okay)
        {
            msg(p, "Item owner must be nearby!");
            return false;
        }

        /* Get a copy of the object */
        object_copy(full, obj);

        /* Identify object to get real price */
        object_notice_everything_aux(p, full, true, false);

        /* Get the price */
        price = player_price_item(p, full);
        object_delete(&full);
        if (price < 0)
        {
            msg(p, "The item's inscription prevents it.");
            return false;
        }
        if (p->au < price)
        {
            msg(p, "You do not have enough gold.");
            return false;
        }

        /* Tell the client about the price */
        if (pickup < 4)
        {
            p->current_value = obj->oidx;
            Send_store_sell(p, price, false);
            return false;
        }

        /* Bypass auto-ignore */
        obj->ignore_protect = 1;

        /* Perform the transaction */
        p->au -= price;
        p->upkeep->redraw |= PR_GOLD;
        q->au += price;
        q->upkeep->redraw |= PR_GOLD;

        /* Know original object */
        object_notice_everything(p, obj);

        /* Describe the object (short name) */
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL | ODESC_SALE);

        /* Combine the pack (later) and update gear */
        p->upkeep->notice |= (PN_COMBINE);
        p->upkeep->update |= (PU_INVEN);

        /* Message */
        msg(p, "You bought %s for %d gold.", o_name, price);
        msg(q, "You sold %s for %d gold.", o_name, price);

        /* Erase the inscription */
        obj->note = 0;

        /* Set origin */
        set_origin(obj, ORIGIN_PLAYER, p->wpos.depth, NULL);

        /* Mark artifact as sold */
        set_artifact_info(q, obj, ARTS_SOLD);

        /* Audit */
        strnfmt(buf, sizeof(buf), "PS %s-%d | %s-%d $ %d", p->name, (int)p->id,
            q->name, (int)q->id, price);
        audit(buf);
        audit("PS+gold");
    }

    return true;
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
 * auto-pickup option is on, otherwise, store objects on
 * floor in an array, and tally both how many there are and can be picked up.
 *
 * If not picking up anything, indicate objects on the floor. Do the same
 * thing if we don't have room for anything.
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
 *
 * obj is the floor item to pick up.
 */
byte player_pickup_item(struct player *p, struct chunk *c, int pickup, struct object *o)
{
    struct object *current = NULL;
    int floor_max = z_info->floor_size;
    struct object **floor_list = mem_zalloc(floor_max * sizeof(*floor_list));
    int floor_num = 0;
    bool call_function_again = false;
    bool domsg = true;

    /* Objects picked up */
    byte objs_picked_up = 0;

    /* Nothing else to pick up -- return */
    if (!square_object(c, &p->grid))
    {
        mem_free(floor_list);
        return 0;
    }

    /* Normal ghosts cannot pick things up */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        mem_free(floor_list);
        return 0;
    }

    /* Tally objects that can be at least partially picked up.*/
    floor_num = see_floor_items(p, c, pickup, floor_list, floor_max);
    if (!floor_num)
    {
        mem_free(floor_list);
        return objs_picked_up;
    }

    /* Use the item that we are given, if defined (pickup command from context menu) */
    if (o) current = o;

    /* Use a menu interface for multiple objects, or pickup single objects */
    if ((pickup == 2) && !current)
    {
        if (floor_num > 1) pickup = 3;
        else current = floor_list[0];
    }

    /* Display a list if requested. */
    if ((pickup == 3) && !current)
    {
        struct object *obj;

        /* No item -> get one */
        if (p->current_value == ITEM_REQUEST)
        {
            /* Update the floor on the client */
            display_floor(p, c, floor_list, floor_num);

            p->current_action = ACTION_PICKUP;
            get_item(p, HOOK_CARRY, "");
            mem_free(floor_list);
            return objs_picked_up;
        }

        /* Use current */
        obj = object_from_index(p, p->current_value, true, true);

        /* Paranoia: requires an item */
        if (!obj)
        {
            mem_free(floor_list);
            return objs_picked_up;
        }

        /* Some checks */
        if (!object_is_carried(p, obj))
        {
            /* Winners cannot pick up artifacts except the Crown and Grond */
            if (true_artifact_p(obj) && restrict_winner(p, obj))
            {
                msg(p, "You cannot pick up that item anymore.");
                mem_free(floor_list);
                return objs_picked_up;
            }

            /* Restricted by choice */
            if (obj->artifact && (cfg_no_artifacts || OPT(p, birth_no_artifacts)))
            {
                msg(p, "You cannot pick up that item.");
                mem_free(floor_list);
                return objs_picked_up;
            }

            /* Restricted by choice */
            if (!is_owner(p, obj))
            {
                msg(p, "This item belongs to someone else!");
                mem_free(floor_list);
                return objs_picked_up;
            }

            /* Must meet level requirement */
            if (!has_level_req(p, obj))
            {
                msg(p, "You don't have the required level!");
                mem_free(floor_list);
                return objs_picked_up;
            }
        }

        /* Use current */
        current = obj;
        call_function_again = true;

        /* With a list, we do not need explicit pickup messages */
        domsg = false;
    }

    /* Hack -- allow purchase from floor */
    if (pickup == 4)
    {
        /* Use current */
        struct object *obj = object_from_index(p, p->current_value, true, true);

        /* Paranoia: requires an item */
        if (!obj)
        {
            mem_free(floor_list);
            return objs_picked_up;
        }

        /* Use current */
        current = obj;
        call_function_again = true;

        /* No explicit pickup message */
        domsg = false;
    }

    /* Pick up object, if legal */
    if (current)
    {
        /* Hack -- allow purchase from floor */
        if (!floor_purchase(p, c, pickup, current))
        {
            mem_free(floor_list);
            return objs_picked_up;
        }

        /* Pick up the object */
        player_pickup_aux(p, c, current, 0, domsg);

        /* Indicate an object picked up. */
        objs_picked_up = 1;
    }

    /*
     * If requested, call this function recursively. Count objects picked up.
     * Force the display of a menu in all cases.
     */
    if (call_function_again)
    {
        current_clear(p);
        objs_picked_up += player_pickup_item(p, c, 3, NULL);
    }

    mem_free(floor_list);

    /* Indicate how many objects have been picked up. */
    return objs_picked_up;
}


/*
 * Pick up everything on the floor that requires no player action
 */
byte do_autopickup(struct player *p, struct chunk *c, int pickup)
{
    struct object *obj, *next;
    byte objs_picked_up = 0;

    /* Nothing to pick up -- return */
    if (!square_object(c, &p->grid)) return 0;

    /* Normal ghosts cannot pick things up */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY)) return 0;

    /* Always pickup gold, effortlessly */
    /* Hack -- ghosts don't pick up gold automatically */
    if (!(p->ghost && (pickup < 2))) player_pickup_gold(p, c);

    /* Scan the remaining objects */
    obj = square_object(c, &p->grid);
    while (obj)
    {
        next = obj->next;

        /* Ignore all hidden objects */
        if (!ignore_item_ok(p, obj))
        {
            bool auto_pickup;

            /* Hack -- disturb */
            if (!p->ghost) disturb(p, 0);

            /* Hack -- ghosts don't pick up gold automatically */
            auto_pickup = (pickup? true: false);
            if (tval_is_money(obj) && p->ghost && (pickup < 2))
                auto_pickup = false;

            /* Automatically pick up items into the backpack */
            if (auto_pickup && auto_pickup_okay(p, obj))
            {
                /* Pick up the object (as much as possible) with message */
                player_pickup_aux(p, c, obj, inven_carry_num(p, obj, true), true);
                objs_picked_up++;
            }
        }

        obj = next;
    }

    return objs_picked_up;
}


/*
 * Pick up objects at the player's request
 */
void do_cmd_pickup(struct player *p, int item)
{
    struct chunk *c = chunk_get(&p->wpos);
    struct object *obj;

    /* Check item (on the floor) */
    for (obj = square_object(c, &p->grid); obj; obj = obj->next)
    {
        if (obj->oidx == item) break;
    }

    /* Autopickup first */
    do_autopickup(p, c, 2);

    /* Pick up floor objects with a menu for multiple objects */
    current_clear(p);
    player_pickup_item(p, c, 2, obj);
}


/*
 * Pick up or look at objects on a square when the player steps onto it
 */
void do_cmd_autopickup(struct player *p)
{
    do_autopickup(p, chunk_get(&p->wpos), 2);
}


void leave_depth(struct player *p, struct chunk *c)
{
    int i;
    struct monster *mon;

    /* One less player here */
    chunk_decrease_player_count(&p->wpos);

    /* Free monsters from slavery */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        mon = cave_monster(c, i);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Skip non slaves */
        if (p->id != mon->master) continue;

        /* Free monster from slavery */
        monster_set_master(mon, NULL, MSTATUS_HOSTILE);
    }
    p->slaves = 0;
}


bool weight_okay(struct player *p, struct object *obj)
{
    int wgt;

    wgt = p->upkeep->total_weight + obj->weight * obj->number;
    return (wgt <= weight_limit(&p->state) * 6);
}


/*
 * Stay still. Search. Pick up.
 */
void do_cmd_hold(struct player *p, int item)
{
    /* Take a turn */
    use_energy(p);

    /* Searching XXX */
    search(p, chunk_get(&p->wpos));

    /* Pick things up, not using extra energy */
    do_cmd_pickup(p, item);
}
