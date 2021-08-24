/*
 * File: obj-gear-common.c
 * Purpose: Management of inventory, equipment and quiver
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2014 Nick McConnell
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


#include "angband.h"


static const struct slot_info
{
    int index;
    bool acid_vuln;
    bool name_in_desc;
    const char *mention;
    const char *heavy_describe;
    const char *describe;
} slot_table[] =
{
    #define EQUIP(a, b, c, d, e, f) {EQUIP_##a, b, c, d, e, f},
    #include "list-equip-slots.h"
    #undef EQUIP
    {EQUIP_MAX, false, false, NULL, NULL, NULL}
};


int slot_by_name(struct player *p, const char *name)
{
    struct player_body body = (p? p->body: bodies[0]);
    int i;

    /* Look for the correctly named slot */
    for (i = 0; i < body.count; i++)
    {
        if (streq(name, body.slots[i].name)) break;
    }

    /* Index for that slot */
    return i;
}


struct object *slot_object(struct player *p, int slot)
{
    /* Ensure a valid body */
    if (p->body.slots) return p->body.slots[slot].obj;

    return NULL;
}


struct object *equipped_item_by_slot_name(struct player *p, const char *name)
{
    /* Ensure a valid body */
    if (p->body.slots) return slot_object(p, slot_by_name(p, name));

    return NULL;
}


bool object_is_equipped(struct player_body body, const struct object *obj)
{
    return ((equipped_item_slot(body, obj) < body.count)? true: false);
}


/*
 * Return a string mentioning how a given item is carried.
 */
const char *equip_mention(struct player *p, int slot)
{
    int type = p->body.slots[slot].type;

    /* Heavy */
    if (((type == EQUIP_WEAPON) && p->state.heavy_wield) ||
        ((type == EQUIP_BOW) && p->state.heavy_shoot))
    {
        return slot_table[type].heavy_describe;
    }

    if (slot_table[type].name_in_desc)
        return format(slot_table[type].mention, p->body.slots[slot].name);
    return slot_table[type].mention;
}


/*
 * Return a string describing how a given item is being worn.
 */
const char *equip_describe(struct player *p, int slot)
{
    int type = p->body.slots[slot].type;

    /* Heavy */
    if (((type == EQUIP_WEAPON) && p->state.heavy_wield) ||
        ((type == EQUIP_BOW) && p->state.heavy_shoot))
    {
        return slot_table[type].heavy_describe;
    }

    if (slot_table[type].name_in_desc)
        return format(slot_table[type].describe, p->body.slots[slot].name);
    return slot_table[type].describe;
}


/*
 * Convert a gear object into a one character label
 */
char gear_to_label(struct player *p, struct object *obj)
{
    int i;

    /* Equipment is easy */
    if (object_is_equipped(p->body, obj))
        return I2A(equipped_item_slot(p->body, obj));

    /* Check the quiver */
    for (i = 0; i < z_info->quiver_size; i++)
    {
        if (p->upkeep->quiver[i] == obj) return I2D(i);
    }

    /* Check the inventory */
    for (i = 0; i < z_info->pack_size; i++)
    {
        if (p->upkeep->inven[i] == obj) return I2A(i);
    }

    return '\0';
}


int equipped_item_slot(struct player_body body, const struct object *item)
{
    int i;

    if (item == NULL) return body.count;

    /* Look for an equipment slot with this item */
    for (i = 0; i < body.count; i++)
    {
        if (item == body.slots[i].obj) break;
    }

    /* Correct slot, or body.count if not equipped */
    return i;
}


/* Can only put on wieldable items */
bool obj_can_wear(struct player *p, const struct object *obj)
{
    /* Check for a usable slot */
    switch (obj->tval)
    {
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_MSTAFF:
        case TV_BOW:
        {
            /* Dragons and Monks cannot use weapons */
            if (player_has(p, PF_DRAGON) || player_has(p, PF_MARTIAL_ARTS))
                return false;

            return true;
        }

        case TV_RING:
        case TV_AMULET:
        case TV_LIGHT:
        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_CLOAK:
        case TV_SHIELD:
        case TV_CROWN:
        case TV_HELM:
        case TV_GLOVES:
        case TV_BOOTS:
        case TV_DIGGING:
        case TV_HORN:
        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT: return true;
    }

    /* Assume not wearable */
    return false;
}
