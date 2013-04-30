/*
 * File: squelch.c
 * Purpose: Item destruction
 *
 * Copyright (c) 2007 David T. Blackston, Iain McFall, DarkGod, Jeff Greene,
 * David Vestal, Pete Mack, Andrew Sidwell.
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
#include "squelch.h"


/*
 * The squelch code has a long history.  Originally it started out as a simple
 * sval-dependent item destroyer, but then ego-item and quality squelch was
 * added too, and then squelched items on the dungeon floor were marked by
 * purple dots, and by this time the code was quite unmaintainable and pretty
 * much impossible to work with.
 *
 * Luckily, though, it's been cleaned up.  There is now only sval-dependent
 * squelch and quality-based squelch, and the two don't interact -- quality-based
 * is for items that get pseudo-id'd and sval-dependent is for potions and the
 * like.
 *
 * The squelch code figures most things out itself.  Simply do:
 *     p_ptr->notice |= PN_SQUELCH;
 * whenever you want to make the game check for squelched items.
 *
 * The quality-dependent squelch is much reduced in scope from how it used to
 * be.
 */


/*
 * Find the squelch type of the object, or TYPE_MAX if none
 */
static squelch_type_t squelch_type_of(const object_type *o_ptr)
{
    if (object_is_jewelry(o_ptr)) return TYPE_JEWELRY;
    if (o_ptr->tval == TV_DRAG_ARMOR) return TYPE_DRAG_ARMOR;
    if (wearable_p(o_ptr)) return TYPE_WEARABLE;
    switch (o_ptr->tval)
    {
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_SORCERY_BOOK:
        case TV_SHADOW_BOOK:
        case TV_HUNT_BOOK:
        case TV_PSI_BOOK:
        case TV_DEATH_BOOK:
        case TV_ELEM_BOOK:
        case TV_SUMMON_BOOK: return TYPE_BOOK;
    }
    if (o_ptr->tval) return TYPE_CONSUMABLE;
    return TYPE_MAX;
}


/*
 * Determine the squelch level of an object, which is similar to its pseudo.
 *
 * The main point is when the value is undetermined given current info,
 * return the maximum possible value.
 */
static byte squelch_level_of(struct player *p, const object_type *o_ptr)
{
    bool fullid = object_is_known(p, o_ptr);
    obj_pseudo_t pseudo;

    /* Don't check pseudo-ID for nonsensed things */
    if (!(object_was_sensed(o_ptr) && o_ptr->squelch) && !fullid)
    {
        /* Object would be sensed if it were splendid */
        if (!object_was_sensed(o_ptr))
        {
            if (object_was_worn(o_ptr)) return SQUELCH_EXCELLENT_NO_SPL;
            if (object_is_known_not_artifact(o_ptr)) return SQUELCH_ALL;
        }
        return SQUELCH_MAX;
    }

    /* Get pseudo-ID */
    pseudo = object_pseudo(p, o_ptr, object_flavor_is_aware(p, o_ptr), fullid);

    /* Never squelch "special" items */
    if (pseudo == INSCRIP_SPECIAL) return SQUELCH_MAX;

    /* Other worthless items */
    if ((o_ptr->ident & IDENT_WORTHLESS) || (fullid && worthless_p(o_ptr))) return SQUELCH_WORTHLESS;

    /* Deal with jewelry specially. */
    if (object_is_jewelry(o_ptr))
    {
        if (!kind_is_good_other(o_ptr->kind)) return SQUELCH_AVERAGE;
        return SQUELCH_GOOD;
    }

    /* Get result based on the pseudo */
    switch (pseudo)
    {
        case INSCRIP_NULL: return SQUELCH_MAX;
        case INSCRIP_AVERAGE:
        {
            if (!kind_is_good_other(o_ptr->kind)) return SQUELCH_AVERAGE;
            return SQUELCH_GOOD;
        }
        case INSCRIP_MAGICAL_BAD:
        {
            if (fullid) return SQUELCH_WORTHLESS;
            return SQUELCH_GOOD;
        }
        case INSCRIP_STRANGE: return SQUELCH_GOOD;
        case INSCRIP_MAGICAL_GOOD: return SQUELCH_GOOD;
        case INSCRIP_SPLENDID: return SQUELCH_ALL;
        case INSCRIP_EXCELLENT:
        {
            /* Have to assume splendid until you have tested it */
            if (object_was_worn(o_ptr))
            {
                if (object_high_resist_is_possible(o_ptr))
                    return SQUELCH_EXCELLENT_NO_SPL;
                return SQUELCH_EXCELLENT_NO_HI;
            }
            return SQUELCH_ALL;
        }
    }

    /* Default to not squelching */
    return SQUELCH_MAX;
}


/*
 * Determines if an object is eligible for squelching.
 */
bool squelch_item_ok(struct player *p, const object_type *o_ptr)
{
    byte type;

    /* Player doesn't hide squelchable items */
    if (p->unignoring) return FALSE;

    /* Don't squelch artifacts */
    if (o_ptr->artifact) return FALSE;

    /* Don't squelch stuff inscribed not to be destroyed (!k) */
    if (CGI(o_ptr, 'k', FALSE)) return FALSE;

    /* Don't squelch protected items */
    if (o_ptr->squelch == SQUELCH_PROTECT) return FALSE;

    /* Do squelch individual objects that marked ignore */
    if (o_ptr->ignore) return TRUE;

    type = squelch_type_of(o_ptr);
    if (type == TYPE_MAX) return FALSE;

    /* Squelch items known not to be special */
    if (object_is_known_not_artifact(o_ptr) && (p->other.squelch_lvl[type] == SQUELCH_ALL))
        return TRUE;

    /* Get result based on the feeling and the squelch_level */
    return (squelch_level_of(p, o_ptr) <= p->other.squelch_lvl[type]);
}


/*
 * Drop all {squelch}able items.
 */
void squelch_drop(struct player *p)
{
    int n;

    /* Scan through the slots backwards */
    for (n = INVEN_PACK - 1; n >= 0; n--)
    {
        object_type *o_ptr = &p->inventory[n];

        /* Skip non-objects and unsquelchable objects */
        if (!o_ptr->kind) continue;
        if (!squelch_item_ok(p, o_ptr)) continue;

        /* Check for !d (no drop) inscription */
        if (!CGI(o_ptr, 'd', FALSE))
        {
            /* We're allowed to drop it. */
            inven_drop(p, n, o_ptr->number, FALSE);
        }
    }

    /* Combine/reorder the pack */
    p->notice |= (PN_COMBINE | PN_REORDER);
}
