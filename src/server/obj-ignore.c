/*
 * File: obj-ignore.c
 * Purpose: Item ignoring
 *
 * Copyright (c) 2007 David T. Blackston, Iain McFall, DarkGod, Jeff Greene,
 * David Vestal, Pete Mack, Andi Sidwell.
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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
 * Determine the ignore level of an object, which is similar to its pseudo.
 *
 * The main point is when the value is undetermined given current info,
 * return the maximum possible value.
 */
byte ignore_level_of(struct player *p, const struct object *obj)
{
    bool fullid = object_is_known(p, obj);
    obj_pseudo_t pseudo;

    /* Don't check pseudo-ID for nonsensed things */
    if (!(object_was_sensed(obj) && obj->allow_ignore) && !fullid)
    {
        /* Object would be sensed if it were splendid */
        if (!object_was_sensed(obj))
        {
            if (object_was_worn(obj)) return IGNORE_EXCELLENT_NO_SPL;
            if (object_is_known_not_artifact(obj)) return IGNORE_ALL;
        }
        return IGNORE_MAX;
    }

    /* Get pseudo-ID */
    pseudo = object_pseudo(p, obj, object_flavor_is_aware(p, obj), fullid);

    /* Never ignore "special" items */
    if (pseudo == INSCRIP_SPECIAL) return IGNORE_MAX;

    /* Other worthless items */
    if ((obj->origin == ORIGIN_WORTHLESS) || (fullid && worthless_p(obj)))
        return IGNORE_WORTHLESS;

    /* Deal with jewelry specially. */
    if (tval_is_jewelry(obj))
    {
        if (!kind_is_good_other(obj->kind)) return IGNORE_AVERAGE;
        return IGNORE_GOOD;
    }

    /* Get result based on the pseudo */
    switch (pseudo)
    {
        case INSCRIP_NULL: return IGNORE_MAX;
        case INSCRIP_AVERAGE:
        {
            if (!kind_is_good_other(obj->kind)) return IGNORE_AVERAGE;
            return IGNORE_GOOD;
        }
        case INSCRIP_MAGICAL_BAD:
        {
            if (fullid) return IGNORE_WORTHLESS;
            return IGNORE_GOOD;
        }
        case INSCRIP_STRANGE: return IGNORE_GOOD;
        case INSCRIP_MAGICAL_GOOD: return IGNORE_GOOD;
        case INSCRIP_SPLENDID: return IGNORE_ALL;
        case INSCRIP_EXCELLENT:
        {
            /* Have to assume splendid until you have tested it */
            if (object_was_worn(obj) || fullid)
            {
                if (object_high_resist_is_possible(obj))
                    return IGNORE_EXCELLENT_NO_SPL;
                return IGNORE_EXCELLENT_NO_HI;
            }
            return IGNORE_ALL;
        }
    }

    /* Default to not ignoring */
    return IGNORE_MAX;
}


/*
 * Determines if an object is already ignored.
 */
bool object_is_ignored(struct player *p, const struct object *obj)
{
    byte type;

    /* Don't ignore artifacts */
    if (obj->artifact) return false;

    /* Don't ignore stuff inscribed not to be destroyed (!k) */
    if (object_prevent_inscription(p, obj, INSCRIPTION_DESTROY, false)) return false;

    /* Don't ignore protected items */
    if (obj->allow_ignore == IGNORE_PROTECT) return false;

    /* Do ignore individual objects that marked ignore */
    if (obj->known->notice & OBJ_NOTICE_IGNORE) return true;

    /* Do ignoring by kind */
    if (object_flavor_is_aware(p, obj) && p->kind_ignore[obj->kind->kidx]) return true;

    type = ignore_type_of(obj);

    /* Ignore ego items if known */
    if (object_ego_is_visible(obj) && p->ego_ignore_types[obj->ego->eidx][type])
        return true;

    if (type == ITYPE_MAX) return false;

    /* Ignore items known not to be special */
    if (object_is_known_not_artifact(obj) && (p->other.ignore_lvl[type] == IGNORE_ALL))
        return true;

    /* Get result based on the feeling and the ignore_level */
    return (ignore_level_of(p, obj) <= p->other.ignore_lvl[type]);
}


/*
 * Determines if an object is eligible for ignoring.
 */
bool ignore_item_ok(struct player *p, const struct object *obj)
{
    /* Player doesn't hide ignored items */
    if (p->unignoring) return false;

    return object_is_ignored(p, obj);
}


/*
 * Drop all ignored items.
 */
void ignore_drop(struct player *p)
{
    struct object *obj;

    /* Scan through the slots backwards */
    for (obj = gear_last_item(p); obj; obj = obj->prev)
    {
        /* Skip non-objects and unignoreable objects */
        if (!ignore_item_ok(p, obj)) continue;

        /* Check for !d (no drop) inscription */
        if (!object_prevent_inscription(p, obj, INSCRIPTION_DROP, false))
        {
            /* We're allowed to drop it. */
            do_cmd_drop(p, obj->oidx, obj->number);
            cmd_ignore_drop(p);
            break;
        }
    }
}


/*
 * Initialize the ignore package
 */
void init_ignore(void)
{
}


/*
 * Clean up the ignore package
 */
void cleanup_ignore(void)
{
}


struct init_module ignore_module =
{
    "ignore",
    init_ignore,
    cleanup_ignore
};
