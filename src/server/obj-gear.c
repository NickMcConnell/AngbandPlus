/*
 * File: obj-gear.c
 * Purpose: Management of inventory, equipment and quiver
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2014 Nick McConnell
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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
 * Gets a slot of the given type, preferentially empty unless full is true
 */
static int slot_by_type(struct player *p, int type, bool full)
{
    struct player_body body = (p? p->body: bodies[0]);
    int i, fallback = body.count;

    /* Look for a correct slot type */
    for (i = 0; i < body.count; i++)
    {
        if (type == body.slots[i].type)
        {
            /* Found a full slot */
            if (full)
            {
                if (body.slots[i].obj != NULL) break;
            }

            /* Found an empty slot */
            else
            {
                if (body.slots[i].obj == NULL) break;
            }

            /* Not right for full/empty, but still the right type */
            if (fallback == body.count) fallback = i;
        }
    }

    /* Index for the best slot we found, or p->body.count if none found  */
    return ((i != body.count)? i :fallback);
}


bool slot_type_is(struct player *p, int slot, int type)
{
    /* Assume default body if no player */
    struct player_body body = (p? p->body: bodies[0]);

    return ((body.slots[slot].type == type)? true: false);
}


bool object_is_carried(struct player *p, const struct object *obj)
{
    return pile_contains(p->gear, obj);
}


/*
 * Check if an object is in the quiver
 */
static bool object_is_in_quiver(struct player *p, const struct object *obj)
{
    int i;

    for (i = 0; i < z_info->quiver_size; i++)
    {
        if (obj == p->upkeep->quiver[i]) return true;
    }

    return false;
}


/*
 * Calculate the number of pack slots used by the current gear.
 *
 * Note that this function does not check that there are adequate slots in the
 * quiver, just the total quantity of missiles.
 */
static int pack_slots_used(struct player *p)
{
    struct object *obj;
    int i, pack_slots = 0;
    int quiver_ammo = 0;

    for (obj = p->gear; obj; obj = obj->next)
    {
        bool found = false;

        /* Equipment doesn't count */
        if (!object_is_equipped(p->body, obj))
        {
            /* Check if it is in the quiver */
            if (tval_is_ammo(obj))
            {
                for (i = 0; i < z_info->quiver_size; i++)
                {
                    if (p->upkeep->quiver[i] == obj)
                    {
                        quiver_ammo += obj->number;
                        found = true;
                        break;
                    }
                }
            }

            /* Count regular slots */
            if (!found) pack_slots++;
        }
    }

    /* Full slots */
    pack_slots += quiver_ammo / z_info->quiver_slot_size;

    /* Plus one for any remainder */
    if (quiver_ammo % z_info->quiver_slot_size) pack_slots++;

    return pack_slots;
}


/*
 * Determine which equipment slot (if any) an item likes. The slot might (or
 * might not) be open, but it is a slot which the object could be equipped in.
 *
 * For items where multiple slots could work (e.g. rings), the function
 * will try to a return an open slot if possible.
 */
s16b wield_slot(struct player *p, const struct object *obj)
{
    /* Slot for equipment */
    switch (obj->tval)
    {
        case TV_MSTAFF: return slot_by_type(p, EQUIP_WEAPON, false);
        case TV_BOW: return slot_by_type(p, EQUIP_BOW, false);
        case TV_AMULET: return slot_by_type(p, EQUIP_AMULET, false);
        case TV_CLOAK: return slot_by_type(p, EQUIP_CLOAK, false);
        case TV_SHIELD: return slot_by_type(p, EQUIP_SHIELD, false);
        case TV_GLOVES: return slot_by_type(p, EQUIP_GLOVES, false);
        case TV_BOOTS: return slot_by_type(p, EQUIP_BOOTS, false);
        case TV_DIGGING:
        case TV_HORN: return slot_by_type(p, EQUIP_TOOL, false);
    }

    if (tval_is_melee_weapon(obj))
        return slot_by_type(p, EQUIP_WEAPON, false);
    if (tval_is_ring(obj))
        return slot_by_type(p, EQUIP_RING, false);
    if (tval_is_light(obj))
        return slot_by_type(p, EQUIP_LIGHT, false);
    if (tval_is_body_armor(obj))
        return slot_by_type(p, EQUIP_BODY_ARMOR, false);
    if (tval_is_head_armor(obj))
        return slot_by_type(p, EQUIP_HAT, false);

    /* No slot available */
    return -1;
}


/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
bool minus_ac(struct player *p)
{
    int i, count = 0;
    struct object *obj = NULL;

    /* Avoid crash during monster power calculations */
    if (!p->gear) return false;

    /* Count the armor slots */
    for (i = 0; i < p->body.count; i++)
    {
        /* Ignore non-armor */
        if (slot_type_is(p, i, EQUIP_WEAPON)) continue;
        if (slot_type_is(p, i, EQUIP_BOW)) continue;
        if (slot_type_is(p, i, EQUIP_RING)) continue;
        if (slot_type_is(p, i, EQUIP_AMULET)) continue;
        if (slot_type_is(p, i, EQUIP_LIGHT)) continue;
        if (slot_type_is(p, i, EQUIP_TOOL)) continue;

        /* Add */
        count++;
    }

    /* Pick one at random */
    for (i = p->body.count - 1; i >= 0; i--)
    {
        /* Ignore non-armor */
        if (slot_type_is(p, i, EQUIP_WEAPON)) continue;
        if (slot_type_is(p, i, EQUIP_BOW)) continue;
        if (slot_type_is(p, i, EQUIP_RING)) continue;
        if (slot_type_is(p, i, EQUIP_AMULET)) continue;
        if (slot_type_is(p, i, EQUIP_LIGHT)) continue;
        if (slot_type_is(p, i, EQUIP_TOOL)) continue;

        if (one_in_(count--)) break;
    }

    /* Get the item */
    obj = slot_object(p, i);

    /* If we can still damage the item */
    if (obj && (obj->ac + obj->to_a > 0))
    {
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

        /* Object resists */
        if (obj->el_info[ELEM_ACID].flags & EL_INFO_IGNORE)
            msg(p, "Your %s is unaffected!", o_name);
        else
        {
            msg(p, "Your %s is damaged!", o_name);

            /* Damage the item */
            obj->to_a--;

            p->upkeep->update |= (PU_BONUS);
            p->upkeep->redraw |= (PR_EQUIP);
        }

        /* There was an effect */
        return true;
    }

    /* No damage or effect */
    return false;
}


/*
 * Remove an object from the gear list, leaving it unattached
 */
void gear_excise_object(struct player *p, struct object *obj)
{
    int i;

    pile_excise(&p->gear, obj);

    /* Change the weight */
    p->upkeep->total_weight -= (obj->number * obj->weight);

    /* Hack -- excise object index */
    obj->oidx = 0;

    /* Make sure it isn't still equipped */
    for (i = 0; i < p->body.count; i++)
    {
        if (slot_object(p, i) == obj)
        {
            p->body.slots[i].obj = NULL;
            p->upkeep->equip_cnt--;
        }
    }

    /* Update the gear */
    calc_inventory(p);

    /* Housekeeping */
    p->upkeep->update |= (PU_BONUS);
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
}


struct object *gear_last_item(struct player *p)
{
    return pile_last_item(p->gear);
}


void gear_insert_end(struct player *p, struct object *obj)
{
    pile_insert_end(&p->gear, obj);
}


/*
 * Remove an amount of an object from the inventory or quiver, returning
 * a detached object which can be used.
 *
 * Optionally describe what remains.
 */
struct object *gear_object_for_use(struct player *p, struct object *obj, int num, bool message,
    bool *none_left)
{
    struct object *usable;
    char name[NORMAL_WID];
    char label = gear_to_label(p, obj);

    /* Bounds check */
    num = MIN(num, obj->number);

    /* Split off a usable object if necessary */
    if (obj->number > num)
    {
        usable = object_split(obj, num);

        /* Change the weight */
        p->upkeep->total_weight -= (num * obj->weight);

        if (message) object_desc(p, name, sizeof(name), obj, ODESC_PREFIX | ODESC_FULL);
    }
    else
    {
        if (message)
        {
            /* Describe zero amount */
            obj->number = 0;
            object_desc(p, name, sizeof(name), obj, ODESC_PREFIX | ODESC_FULL);
            obj->number = num;
        }

        /* We're using the entire stack */
        usable = obj;
        gear_excise_object(p, usable);
        *none_left = true;

        /* Stop tracking item */
        if (tracked_object_is(p->upkeep, obj)) track_object(p->upkeep, NULL);
    }

    /* Housekeeping */
    p->upkeep->update |= (PU_BONUS);
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

    /* Print a message if desired */
    if (message)
        msg(p, "You have %s (%c).", name, label);

    return usable;
}


/*
 * Check how many missiles can be put in the quiver without increasing the
 * number of pack slots used.
 *
 * Returns the quantity from a given stack of missiles that can be added.
 */
static int quiver_absorb_num(struct player *p, const struct object *obj)
{
    /* Must be ammo */
    if (tval_is_ammo(obj))
    {
        int i, quiver_count = 0, space_free = 0;

        /* Count the current space this object could go into */
        for (i = 0; i < z_info->quiver_size; i++)
        {
            struct object *quiver_obj = p->upkeep->quiver[i];

            if (quiver_obj)
            {
                quiver_count += quiver_obj->number;
                if (object_stackable(p, quiver_obj, obj, OSTACK_PACK))
                    space_free += z_info->quiver_slot_size - quiver_obj->number;
            }
            else
                space_free += z_info->quiver_slot_size;
        }

        if (space_free)
        {
            /* Check we won't need another pack slot */
            quiver_count += z_info->quiver_slot_size;
            while (quiver_count > z_info->quiver_slot_size)
                quiver_count -= z_info->quiver_slot_size;

            /* Return the number, or the number that will fit */
            space_free = MIN(space_free, z_info->quiver_slot_size - quiver_count);
            return MIN(obj->number, space_free);
        }
    }

    /* No ammo or no space */
    return 0;
}


/*
 * Calculate how much of an item can be carried in the inventory or quiver.
 *
 * Optionally only return a positive value if there is already a similar object.
 */
int inven_carry_num(struct player *p, struct object *obj, bool stack)
{
    int i, num_left;

    /* Check for similarity */
    if (stack)
    {
        struct object *gear_obj;

        for (gear_obj = p->gear; gear_obj; gear_obj = gear_obj->next)
        {
            if (!object_is_equipped(p->body, gear_obj) &&
                object_stackable(p, gear_obj, obj, OSTACK_PACK))
            {
                break;
            }
        }

        /* No similar object, so no stacking */
        if (!gear_obj) return 0;
    }

    /* Free inventory slots, so there is definitely room */
    if (pack_slots_used(p) < z_info->pack_size) return obj->number;

    /* Absorb as many as we can in the quiver */
    num_left = obj->number - quiver_absorb_num(p, obj);

    /* See if we can add to a part full inventory slot */
    for (i = 0; i < z_info->pack_size; i++)
    {
        struct object *inven_obj = p->upkeep->inven[i];

        if (inven_obj && object_stackable(p, inven_obj, obj, OSTACK_PACK))
            num_left -= inven_obj->kind->base->max_stack - inven_obj->number;
    }

    /* Return the number we can absorb */
    return obj->number - MAX(num_left, 0);
}


/*
 * Check if we're allowed to get rid of an item easily.
 */
bool inven_drop_okay(struct player *p, struct object *obj)
{
    /* Never drop true artifacts above their base depth except the Crown and Grond */
    if (!cfg_artifact_drop_shallow && true_artifact_p(obj) &&
        (p->wpos.depth < obj->artifact->level) && !kf_has(obj->kind->kind_flags, KF_QUEST_ART))
    {
        /* Do not apply this rule to no_recall characters and DMs */
        if ((cfg_diving_mode < 3) && !is_dm_p(p)) return false;
    }

    return true;
}


/*
 * Check if we have space for some of an item in the pack, optionally requiring stacking
 */
bool inven_carry_okay(struct player *p, struct object *obj)
{
    return ((inven_carry_num(p, obj, false) > 0)? true: false);
}


/*
 * Describe the charges on an item in the inventory.
 */
void inven_item_charges(struct player *p, struct object *obj)
{
    /* Require staff/wand */
    if (tval_can_have_charges(obj) && object_is_known(p, obj))
        msg(p, "You have %d charge%s remaining.", obj->pval, PLURAL(obj->pval));
}


/*
 * Add an item to the players inventory.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using object_similar() and object_absorb(), otherwise,
 * the item will be placed into the first available gear array index.
 *
 * This function can be used to "over-fill" the player's pack, but only
 * once, and such an action must trigger the "overflow" code immediately.
 * Note that when the pack is being "over-filled", the new item must be
 * placed into the "overflow" slot, and the "overflow" must take place
 * before the pack is reordered, but (optionally) after the pack is
 * combined. This may be tricky. See "dungeon.c" for info.
 *
 * Note that this code removes any location information from the object once
 * it is placed into the inventory, but takes no responsibility for removing
 * the object from any other pile it was in.
 */
void inven_carry(struct player *p, struct object *obj, bool absorb, bool message)
{
    bool combining = false;
    struct object *local_obj = obj;

    /* Object is now owned */
    object_own(p, obj);

    /* Check for combining, if appropriate */
    if (absorb)
    {
        struct object *combine_item = p->gear;

        while (combine_item)
        {
            if (!object_is_equipped(p->body, combine_item) &&
                object_similar(p, combine_item, obj, OSTACK_PACK))
            {
                break;
            }

            combine_item = combine_item->next;
        }

        if (combine_item)
        {
            /* Increase the weight */
            p->upkeep->total_weight += (obj->number * obj->weight);

            /* Combine the items, and their known versions */
            object_absorb(combine_item, obj);

            local_obj = combine_item;
            combining = true;
        }
    }

    /* We didn't manage the find an object to combine with */
    if (!combining)
    {
        /* Paranoia */
        my_assert(pack_slots_used(p) <= z_info->pack_size);

        gear_insert_end(p, obj);
        apply_autoinscription(p, obj);

        /* Remove cave object details */
        obj->held_m_idx = 0;
        loc_init(&obj->grid, 0, 0);
        memset(&obj->wpos, 0, sizeof(struct worldpos));

        /* Update the inventory */
        p->upkeep->total_weight += (obj->number * obj->weight);
        p->upkeep->notice |= (PN_COMBINE);

        /* Hobbits ID mushrooms on pickup, gnomes ID wands and staffs on pickup */
        /* PWMAngband: Dragons and Monks cannot use weapons, so they need to learn "on wield" */
        if (!object_is_known(p, obj))
        {
            if (player_has(p, PF_KNOW_MUSHROOM) && tval_is_mushroom(obj))
            {
                object_know_everything(p, obj);
                msg(p, "Mushrooms for breakfast!");
            }

            else if (player_has(p, PF_KNOW_ZAPPER) && tval_is_zapper(obj))
                object_know_everything(p, obj);

            else if (player_has(p, PF_DRAGON) || player_has(p, PF_HYDRA) ||
                player_has(p, PF_MARTIAL_ARTS))
            {
                weapon_learn_on_carry(p, obj);
            }
        }
    }

    p->upkeep->update |= (PU_BONUS | PU_INVEN);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_SPELL | PR_STUDY);
    update_stuff(p, chunk_get(&p->wpos));

    if (message)
    {
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), local_obj, ODESC_PREFIX | ODESC_FULL);
        msg(p, "You have %s (%c).", o_name, gear_to_label(p, local_obj));
    }

    if (object_is_in_quiver(p, local_obj)) sound(p, MSG_QUIVER);
}


static void know_everything(struct player *p, struct chunk *c)
{
    struct object *obj;

    /* Know all objects under the player */
    for (obj = square_object(c, &p->grid); obj; obj = obj->next)
    {
        if (object_is_known(p, obj)) continue;
        object_know_everything(p, obj);
    }

    /* Know all carried objects */
    for (obj = p->gear; obj; obj = obj->next)
    {
        if (object_is_known(p, obj)) continue;
        object_know_everything(p, obj);
    }
}


/*
 * Wield or wear a single item from the pack or floor
 */
void inven_wield(struct player *p, struct object *obj, int slot)
{
    struct object *wielded, *old = p->body.slots[slot].obj;
    const char *fmt;
    char o_name[NORMAL_WID];
    bool dummy = false;
    struct chunk *c = chunk_get(&p->wpos);

    /* Increase equipment counter if empty slot */
    if (old == NULL) p->upkeep->equip_cnt++;

    /* Take a turn */
    use_energy(p);

    /* It's either a gear object or a floor object */
    if (object_is_carried(p, obj))
    {
        /* Split off a new object if necessary */
        if (obj->number > 1)
        {
            wielded = gear_object_for_use(p, obj, 1, false, &dummy);

            /* The new item needs new gear and known gear entries */
            wielded->next = obj->next;
            obj->next = wielded;
            wielded->prev = obj;
            if (wielded->next) (wielded->next)->prev = wielded;
        }

        /* Just use the object directly */
        else
            wielded = obj;

        p->upkeep->notice |= (PN_COMBINE);
    }
    else
    {
        /* Get a floor item and carry it */
        wielded = floor_object_for_use(p, c, obj, 1, false, &dummy);
        inven_carry(p, wielded, false, false);
    }

    /* Wear the new stuff */
    wielded->oidx = z_info->pack_size + slot;
    p->body.slots[slot].obj = wielded;

    /* Object is now owned */
    object_own(p, wielded);

    /* Bypass auto-ignore */
    wielded->ignore_protect = 1;

    /* Do any ID-on-wield */
    object_learn_on_wield(p, wielded);

    /* Auto-id */
    if (of_has(wielded->flags, OF_KNOWLEDGE)) know_everything(p, c);

    /* Where is the item now */
    if (tval_is_melee_weapon(wielded) || tval_is_mstaff(wielded))
        fmt = "You are wielding %s (%c).";
    else if (tval_is_launcher(wielded))
        fmt = "You are shooting with %s (%c).";
    else if (tval_is_light(wielded))
        fmt = "Your light source is %s (%c).";
    else if (tval_is_tool(wielded))
        fmt = "You are using %s (%c).";
    else
        fmt = "You are wearing %s (%c).";

    /* Describe the result */
    object_desc(p, o_name, sizeof(o_name), wielded, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    msgt(p, MSG_WIELD, fmt, o_name, I2A(slot));

    /* Sticky flag gets a special mention */
    if (of_has(wielded->flags, OF_STICKY))
    {
        /* Warn the player */
        msgt(p, MSG_CURSED, "Oops! It feels deathly cold!");
    }

    /* See if we have to overflow the pack */
    combine_pack(p);
    pack_overflow(p, c, old);

    /* Recalculate bonuses, torch, mana, gear */
    p->upkeep->notice |= (PN_IGNORE);
    p->upkeep->update |= (PU_BONUS | PU_INVEN | PU_UPDATE_VIEW);
    p->upkeep->redraw |= (PR_PLUSSES | PR_INVEN | PR_EQUIP | PR_BASIC);
    update_stuff(p, c);
}


/*
 * Take off a non-cursed equipment item
 *
 * Note that taking off an item when "full" may cause that item
 * to fall to the ground.
 *
 * Note also that this function does not try to combine the taken off item
 * with other inventory items - that must be done by the calling function.
 */
void inven_takeoff(struct player *p, struct object *obj)
{
    int slot = equipped_item_slot(p->body, obj);
    const char *act;
    char o_name[NORMAL_WID];

    /* Paranoia */
    if (slot == p->body.count) return;

    /* Check preventive inscription '!t' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_TAKEOFF, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Describe the object */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

    /* Describe removal by slot */
    if (slot_type_is(p, slot, EQUIP_WEAPON))
        act = "You were wielding";
    else if (slot_type_is(p, slot, EQUIP_BOW) || slot_type_is(p, slot, EQUIP_LIGHT))
        act = "You were holding";
    else if (slot_type_is(p, slot, EQUIP_TOOL))
        act = "You were using";
    else
        act = "You were wearing";

    /* De-equip the object */
    p->body.slots[slot].obj = NULL;
    p->upkeep->equip_cnt--;

    p->upkeep->update |= (PU_BONUS | PU_INVEN | PU_UPDATE_VIEW);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_PLUSSES);
    p->upkeep->notice |= (PN_IGNORE);
    update_stuff(p, chunk_get(&p->wpos));

    /* Message */
    msgt(p, MSG_WIELD, "%s %s (%c).", act, o_name, gear_to_label(p, obj));
}


/*
 * Drop (some of) a non-cursed inventory/equipment item "near" the current
 * location
 *
 * There are two cases here - a single object or entire stack is being dropped,
 * or part of a stack is being split off and dropped
 */
bool inven_drop(struct player *p, struct object *obj, int amt, bool bypass_inscr)
{
    struct object *dropped;
    bool none_left = false;
    bool quiver = false;
    char name[NORMAL_WID];
    char label;

    /* Error check */
    if (amt <= 0) return true;

    /*
     * Check it is still held, in case there were two drop commands queued
     * for this item. This is in theory not ideal, but in practice should
     * be safe.
     */
    if (!object_is_carried(p, obj)) return true;

    /* Get where the object is now */
    label = gear_to_label(p, obj);

    /* Is it in the quiver? */
    if (object_is_in_quiver(p, obj)) quiver = true;

    /* Not too many */
    if (amt > obj->number) amt = obj->number;

    /* Check preventive inscription '!d' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_DROP, false) && !bypass_inscr)
    {
        msg(p, "The item's inscription prevents it.");
        return true;
    }

    /* Never drop true artifacts above their base depth except the Crown and Grond */
    if (!inven_drop_okay(p, obj))
    {
        if (!bypass_inscr) msg(p, "You cannot drop this here.");
        return false;
    }

    /* Never drop deeds of property */
    if (tval_is_deed(obj))
    {
        if (!bypass_inscr) msg(p, "You cannot drop this.");
        return false;
    }

    /* Never drop items in wrong house */
    if (!check_store_drop(p))
    {
        if (!bypass_inscr) msg(p, "You cannot drop this here.");
        return false;
    }

    /* Take off equipment, don't combine */
    if (object_is_equipped(p->body, obj))
        inven_takeoff(p, obj);

    /* Get the object */
    dropped = gear_object_for_use(p, obj, amt, false, &none_left);

    /* Describe the dropped object */
    object_desc(p, name, sizeof(name), dropped, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    msg(p, "You drop %s (%c).", name, label);

    /* Describe what's left */
    if (none_left)
    {
        /* Play silly games to get the right description */
        int number = dropped->number;

        dropped->number = 0;
        object_desc(p, name, sizeof(name), dropped, ODESC_PREFIX | ODESC_FULL);
        dropped->number = number;
    }
    else
        object_desc(p, name, sizeof(name), obj, ODESC_PREFIX | ODESC_FULL);
    msg(p, "You have %s (%c).", name, label);

    /* Drop it (carefully) near the player */
    drop_near(p, chunk_get(&p->wpos), &dropped, 0, &p->grid, false,
        (bypass_inscr? DROP_SILENT: DROP_FORBID), true);

    /* Sound for quiver objects */
    if (quiver) sound(p, MSG_QUIVER);

    return true;
}


/*
 * Return whether each stack of objects can be merged into two uneven stacks.
 */
static bool inventory_can_stack_partial(struct player *p, const struct object *obj1,
    const struct object *obj2, object_stack_t mode)
{
    if (!(mode & OSTACK_STORE))
    {
        int total = obj1->number + obj2->number;
        int remainder = total - obj1->kind->base->max_stack;

        if (remainder > obj1->kind->base->max_stack) return false;
    }

    return object_stackable(p, obj1, obj2, mode);
}


/*
 * Combine items in the pack, confirming no blank objects or gold
 */
void combine_pack(struct player *p)
{
    struct object *obj1, *obj2, *prev;
    bool display_message = false;
    bool redraw = false;

    /* Combine the pack (backwards) */
    obj1 = gear_last_item(p);
    while (obj1)
    {
        my_assert(!tval_is_money(obj1));
        prev = obj1->prev;

        /* Scan the items above that item */
        for (obj2 = p->gear; obj2 && (obj2 != obj1); obj2 = obj2->next)
        {
            /* Can we drop "obj1" onto "obj2"? */
            if (object_similar(p, obj2, obj1, OSTACK_PACK))
            {
                display_message = true;

                redraw = true;
                object_absorb(obj2, obj1);
                break;
            }
            else if (inventory_can_stack_partial(p, obj2, obj1, OSTACK_PACK))
            {
                /* Setting this to true spams the combine message. */
                display_message = false;

                redraw = true;
                object_absorb_partial(obj2, obj1);
                break;
            }
        }
        obj1 = prev;
    }

    calc_inventory(p);

    /* Redraw */
    if (redraw)
        p->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_SPELL | PR_STUDY);

    /* Message */
    if (display_message) msg(p, "You combine some items in your pack.");
}


/*
 * Returns whether the pack is holding the more than the maximum number of
 * items. If this is true, calling pack_overflow() will trigger a pack overflow.
 */
static bool pack_is_overfull(struct player *p)
{
    return ((pack_slots_used(p) > z_info->pack_size)? true: false);
}


/*
 * Overflow an item from the pack, if it is overfull.
 */
void pack_overflow(struct player *p, struct chunk *c, struct object *obj)
{
    int i;
    char o_name[NORMAL_WID];

    if (!pack_is_overfull(p)) return;

    /* Disturbing */
    disturb(p);

    /* Warning */
    msg(p, "Your pack overflows!");

    /* Get the last proper item */
    for (i = 1; i <= z_info->pack_size; i++)
    {
        if (!p->upkeep->inven[i]) break;
    }

    /* Drop the last inventory item unless requested otherwise */
    if (!obj) obj = p->upkeep->inven[i - 1];

    /* Describe */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    msg(p, "You drop %s.", o_name);

    /* Excise the object and drop it (carefully) near the player */
    gear_excise_object(p, obj);
    drop_near(p, c, &obj, 0, &p->grid, false, DROP_FADE, true);

    /* Describe */
    msg(p, "You no longer have %s.", o_name);

    /* Notice, update, redraw */
    if (p->upkeep->notice) notice_stuff(p);
    if (p->upkeep->update) update_stuff(p, c);
    if (p->upkeep->redraw) redraw_stuff(p);
}


/* Can only put on wieldable items */
bool item_tester_hook_wear(struct player *p, const struct object *obj)
{
    /* Check for a usable slot */
    int slot = wield_slot(p, obj);

    if ((slot < 0) || (slot >= p->body.count)) return false;

    /* Dragons, Hydras and Monks cannot use weapons */
    if ((player_has(p, PF_DRAGON) || player_has(p, PF_HYDRA) || player_has(p, PF_MARTIAL_ARTS)) &&
        ((slot == slot_by_name(p, "weapon")) || (slot == slot_by_name(p, "shooting"))))
    {
        return false;
    }

    /* Assume wearable */
    return true;
}
