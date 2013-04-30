/*
 * File: cmd-obj.c
 * Purpose: Handle objects in various ways
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007-9 Andrew Sidwell, Chris Carr, Ed Graham, Erik Osheim
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
#include "cmds.h"
#include "effects.h"
#include "netserver.h"
#include "object/inventory.h"
#include "s-spells.h"


/*** Utility bits and bobs ***/


/*
 * Check to see if the player can use a rod/wand/staff/activatable object.
 */
static bool check_devices(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);
    int fail;
    const char *action;

    /* Horns are not magical and therefore never fail */
    if (o_ptr->tval == TV_HORN) return TRUE;

    /* Get the right string */
    switch (o_ptr->tval)
    {
        case TV_ROD: action = "zap the rod"; break;
        case TV_WAND: action = "use the wand"; break;
        case TV_STAFF: action = "use the staff"; break;
        default: action = "activate it"; break;
    }

    /* Figure out how hard the item is to use */
    fail = get_use_device_chance(p_ptr, o_ptr);

    /* Roll for usage */
    if (CHANCE(fail, 1000))
    {
        msg(p_ptr, "You failed to %s properly.", action);
        return FALSE;
    }

    return TRUE;
}


/*
 * Return the chance of an effect beaming, given a tval.
 */
static int beam_chance(int tval)
{
    switch (tval)
    {
        case TV_WAND: return 20;
        case TV_ROD:  return 10;
    }

    return 0;
}


typedef enum
{
    ART_TAG_NONE,
    ART_TAG_NAME,
    ART_TAG_KIND,
    ART_TAG_VERB,
    ART_TAG_VERB_IS
} art_tag_t;


static art_tag_t art_tag_lookup(const char *tag)
{
    if (strncmp(tag, "name", 4) == 0) return ART_TAG_NAME;
    if (strncmp(tag, "kind", 4) == 0) return ART_TAG_KIND;
    if (strncmp(tag, "s", 1) == 0) return ART_TAG_VERB;
    if (strncmp(tag, "is", 2) == 0) return ART_TAG_VERB_IS;
    return ART_TAG_NONE;
}


/*
 * Print an artifact activation message.
 *
 * In order to support randarts, with scrambled names, we re-write
 * the message to replace instances of {name} with the artifact name
 * and instances of {kind} with the type of object.
 *
 * This code deals with plural and singular forms of verbs correctly
 * when encountering {s}, though in fact both names and kinds are
 * always singular in the current code (gloves are "Set of" and boots
 * are "Pair of")
 */
static void activation_message(int Ind, object_type *o_ptr, const char *message)
{
    player_type *p_ptr = player_get(Ind);
    char buf[MSG_LEN] = "\0";
    const char *next;
    const char *s;
    const char *tag;
    const char *in_cursor;
    size_t end = 0;

    in_cursor = message;

    next = strchr(in_cursor, '{');
    while (next)
    {
        /* Copy the text leading up to this { */
        strnfcat(buf, MSG_LEN, &end, "%.*s", next - in_cursor, in_cursor);

        s = next + 1;
        while (*s && isalpha((unsigned char)*s)) s++;

        /* Valid tag */
        if (*s == '}')
        {
            /* Start the tag after the { */
            tag = next + 1;
            in_cursor = s + 1;

            switch (art_tag_lookup(tag))
            {
                case ART_TAG_NAME:
                    end += object_desc(p_ptr, buf, MSG_LEN, o_ptr, ODESC_PREFIX | ODESC_BASE);
                    break;
                case ART_TAG_KIND:
                    object_kind_name(&buf[end], MSG_LEN - end, o_ptr->kind,
                        p_ptr->obj_aware[o_ptr->kind->kidx]);
                    end += strlen(&buf[end]);
                    break;
                case ART_TAG_VERB:
                    strnfcat(buf, MSG_LEN, &end, "s");
                    break;
                case ART_TAG_VERB_IS:
                    if ((end > 2) && (buf[end - 2] == 's'))
                        strnfcat(buf, MSG_LEN, &end, "are");
                    else
                        strnfcat(buf, MSG_LEN, &end, "is");
                default:
                    break;
            }
        }

        /* An invalid tag, skip it */
        else
            in_cursor = next + 1;

        next = strchr(in_cursor, '{');
    }
    strnfcat(buf, MSG_LEN, &end, in_cursor);

    msg(p_ptr, buf);
}


/*** Inscriptions ***/


/* Remove inscription */
void do_cmd_uninscribe(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        msg(p_ptr, "You cannot uninscribe items!");
        return;
    }

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* The inscription prevents it */
    if (item < 0)
    {
        __trap(p_ptr, CGI(o_ptr, 'g', FALSE))
    }

    /* Nothing to remove */
    if (!o_ptr->note)
    {
        msg(p_ptr, "That item had no inscription to remove.");
        return;
    }

    /* The inscription prevents it */
    __trap(p_ptr, protected_p(p_ptr, o_ptr, '}', FALSE))

    /* Message */
    msg(p_ptr, "Inscription removed.");

    /* Remove the incription */
    o_ptr->note = 0;

    /* Update global "preventive inscriptions" */
    update_prevent_inscriptions(p_ptr);

    /* Combine the pack */
    p_ptr->notice |= (PN_COMBINE | PN_SORT_QUIVER);

    /* Redraw */
    p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
    if (item < 0) p_ptr->redraw |= (PR_FLOOR | PR_ITEMLIST);
}


/* Add inscription */
void do_cmd_inscribe(int Ind, int item, const char *inscription)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    s32b price;
    char *c;

    /* Empty inscription: uninscribe the item instead */
    if (STRZERO(inscription))
    {
        do_cmd_uninscribe(Ind, item);
        return;
    }

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        msg(p_ptr, "You cannot inscribe items!");
        return;
    }

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* The inscription prevents it */
    if (item < 0)
    {
        __trap(p_ptr, CGI(o_ptr, 'g', FALSE))
    }

    /* The inscription prevents it */
    __trap(p_ptr, protected_p(p_ptr, o_ptr, '{', TRUE))

    /* Check ownership */
    if (strstr(inscription, "!g") && (p_ptr->id != o_ptr->owner))
    {
        msg(p_ptr, "You must own this item first.");
        return;
    }

    /* Don't allow certain inscriptions when selling */
    c = my_stristr(inscription, "for sale");
    if (c)
    {
        /* Can't sell unindentified items */
        if (!object_is_known(p_ptr, o_ptr))
        {
            msg(p_ptr, "You must identify this item first.");
            return;
        }

        /* Can't sell overpriced items */
        c += 8; /* skip "for sale" */
        if (*c == ' ')
        {
            price = atoi(c);
            if (price > PY_MAX_GOLD)
            {
                msg(p_ptr, "Your price is too high!");
                return;
            }
        }
    }

    /* Describe the activity */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    msg(p_ptr, "Inscribing %s.", o_name);
    message_flush(p_ptr);

    /* Save the inscription */
    if (OPT_P(p_ptr, birth_no_selling))
        o_ptr->note = quark_add(format("*%s", inscription));
    else
        o_ptr->note = quark_add(inscription);

    /* Update global "preventive inscriptions" */
    update_prevent_inscriptions(p_ptr);

    /* Combine the pack */
    p_ptr->notice |= (PN_COMBINE | PN_SORT_QUIVER);

    /* Redraw */
    p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
    if (item < 0) p_ptr->redraw |= (PR_FLOOR | PR_ITEMLIST);
}


/*** Examination ***/


void do_cmd_observe(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    char o_name[NORMAL_WID];

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Get name */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Inform */
    msg(p_ptr, "Examining %s...", o_name);

    /* Capitalize object name for header */
    my_strcap(o_name);

    /* Let the player scroll through this info */
    p_ptr->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p_ptr);

    /* Dump info into player */
    object_info(p_ptr, o_ptr, OINFO_NONE);

    /* Restore height and width of current dungeon level */
    text_out_done(p_ptr);

    /* Notify player */
    notify_player(Ind, o_name, NTERM_WIN_OBJECT, FALSE);
}


/*** Taking off/putting on ***/


/* Take off an item */
void do_cmd_takeoff(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to remove items!");
        return;
    }

    /* Paranoia */
    if (item < 0) return;

    /* Get the item */
    o_ptr = &p_ptr->inventory[item];

    /* Verify potential overflow */
    if (!inven_carry_okay(p_ptr, o_ptr))
    {
        msg(p_ptr, "Your pack is full and would overflow!");
        return;
    }

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, 't', FALSE))

    /* Item is cursed */
    if (cursed_p(o_ptr->flags))
    {
        /* Oops */
        msg(p_ptr, "Hmmm, it seems to be cursed.");

        /* Nope */
        return;
    }       

    /* Take a turn */
    use_energy(Ind);

    /* Take off the item */
    inven_takeoff(p_ptr, item, 255);

    /* Redraw */
    p_ptr->redraw |= (PR_EQUIP);
}


/*
 * Wield or wear a single item from the pack or floor
 */
void wield_item(int Ind, object_type *o_ptr, int item, int slot, bool take_turn)
{
    player_type *p_ptr = player_get(Ind);
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;
    const char *fmt;
    char o_name[NORMAL_WID];
    bool combined_ammo = FALSE;
    int num = 1;

    /* If we are stacking ammo in the quiver */
    if (obj_is_ammo(p_ptr, o_ptr))
    {
        num = o_ptr->number;
        combined_ammo = object_similar(o_ptr, &p_ptr->inventory[slot], OSTACK_QUIVER);
    }

    /* Take a turn */
    if (take_turn) use_energy(Ind);

    /* Obtain local object */
    object_copy(i_ptr, o_ptr);

    /* Modify quantity */
    i_ptr->number = num;

    /* Decrease the item */
    item_decrease(p_ptr, item, num, FALSE);

    /* Get the wield slot */
    o_ptr = &p_ptr->inventory[slot];

    if (combined_ammo)
    {
        /* Add the new ammo to the already-quiver-ed ammo */
        object_absorb(o_ptr, i_ptr);
    }
    else
    {
        /* Take off existing item */
        if (o_ptr->kind) inven_takeoff(p_ptr, slot, 255);

        /*
         * If we are wielding ammo we may need to "open" the slot by shifting
         * later ammo up the quiver; this is because we already called the
         * inven_item_optimize() function.
         */
        if (slot >= QUIVER_START) open_quiver_slot(Ind, slot);

        /* Wear the new stuff */
        object_copy(o_ptr, i_ptr);
    }

    /* Object is now owned */
    object_own(p_ptr, o_ptr);

    /* Bypass auto-squelch */
    o_ptr->squelch = SQUELCH_PROTECT;

    /* Increase the weight */
    p_ptr->total_weight += i_ptr->weight * num;

    /* Do any ID-on-wield */
    object_notice_on_wield(Ind, o_ptr);

    /* Where is the item now */
    if (slot == INVEN_WIELD)
        fmt = "You are wielding %s (%c).";
    else if (slot == INVEN_BOW)
        fmt = "You are shooting with %s (%c).";
    else if (slot == INVEN_LIGHT)
        fmt = "Your light source is %s (%c).";
    else if (combined_ammo)
        fmt = "You combine %s in your quiver (%c).";
    else if ((slot >= QUIVER_START) && (slot < QUIVER_END))
        fmt = "You add %s to your quiver (%c).";
    else
        fmt = "You are wearing %s (%c).";

    /* Describe the result */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    msgt(p_ptr, MSG_WIELD, fmt, o_name, index_to_label(slot));

    /* Cursed! */
    if (cursed_p(o_ptr->flags))
    {
        /* Warn the player */
        msgt(p_ptr, MSG_CURSED, "Oops! It feels deathly cold!");

        /* Sense the object */
        object_notice_curses(p_ptr, o_ptr);
    }

    /* Save quiver size */
    save_quiver_size(p_ptr, TRUE);

    /* Recalculate bonuses, torch, mana */
    p_ptr->notice |= PN_SORT_QUIVER;
    p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);
    p_ptr->redraw |= (PR_PLUSSES | PR_INVEN | PR_EQUIP);
}


/* Can only put on wieldable items */
static bool item_tester_hook_wear(int Ind, object_type *o_ptr)
{
    int slot;
    player_type *p_ptr = player_get(Ind);

    /* Check for a usable slot */
    /* Dragons and Monks cannot use weapons */
    slot = wield_slot(p_ptr, o_ptr);
    if ((player_has(p_ptr, PF_DRAGON) || player_has(p_ptr, PF_MARTIAL_ARTS)) &&
        ((slot == INVEN_WIELD) || (slot == INVEN_BOW)))
    {
        return FALSE;
    }
    if (slot >= INVEN_WIELD) return (TRUE);

    /* Assume not wearable */
    return (FALSE);
}


/* Wield or wear an item */
void do_cmd_wield(int Ind, int item, int slot)
{
    player_type *p_ptr = player_get(Ind);
    object_type *equip_o_ptr;
    char o_name[NORMAL_WID];
    object_type *o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to wield items!");
        return;
    }

    /* Some checks */
    if (item < 0)
    {
        int num, wgt;

        /* Winners cannot pickup artifacts except the Crown and Grond */
        if (true_artifact_p(o_ptr) && restrict_winner(p_ptr, o_ptr))
        {
            msg(p_ptr, "You cannot wield that item anymore.");
            return;
        }

        /* Restricted by choice */
        if (true_artifact_p(o_ptr) && restrict_artifacts(p_ptr, o_ptr))
        {
            msg(p_ptr, "You cannot wield that item.");
            return;
        }

        /* Make a check against max weight */
        num = (obj_is_ammo(p_ptr, o_ptr)? o_ptr->number: 1);
        wgt = p_ptr->total_weight + o_ptr->weight * num;
        if (wgt > weight_limit(&p_ptr->state) * 6)
        {
            msg(p_ptr, "You are already too burdened to wield that item.");
            return;
        }

        /* Restricted by choice */
        if (!is_owner(p_ptr, o_ptr))
        {
            msg(p_ptr, "This item belongs to someone else!");
            return;
        }

        /* The inscription prevents it */
        __trap(p_ptr, CGI(o_ptr, 'g', FALSE))
    }

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, 'w', FALSE))

    /* Paranoia: requires proper item */
    if (!item_tester_hook_wear(Ind, o_ptr)) return;

    /* Check the slot */
    slot = slot_can_wield_item(Ind, slot, o_ptr);

    /* Paranoia */
    if (slot == -1) return;

    /* Get a pointer to the slot to be removed */
    equip_o_ptr = &p_ptr->inventory[slot];

    /* If the slot is open, wield and be done */
    if (!equip_o_ptr->kind)
    {
        wield_item(Ind, o_ptr, item, slot, TRUE);
        return;
    }

    /* If the slot is in the quiver and objects can be combined */
    if (obj_is_ammo(p_ptr, equip_o_ptr) && object_similar(equip_o_ptr, o_ptr, OSTACK_QUIVER))
    {
        wield_item(Ind, o_ptr, item, slot, TRUE);
        return;
    }

    /* Prevent wielding into a cursed slot */
    if (cursed_p(equip_o_ptr->flags))
    {
        object_desc(p_ptr, o_name, sizeof(o_name), equip_o_ptr, ODESC_BASE);
        msg(p_ptr, "The %s you are %s appears to be cursed.", o_name,
            describe_use(p_ptr, slot));
        return;
    }

    /* The inscription prevents it */
    __trap(p_ptr, CGI(equip_o_ptr, 't', FALSE))

    /* Never drop true artifacts above their base depth except the Crown and Grond */
    if ((item < 0) && !inven_carry_okay(p_ptr, equip_o_ptr) &&
        !cfg_artifact_drop_shallow && true_artifact_p(equip_o_ptr) &&
        (p_ptr->depth < equip_o_ptr->artifact->level) &&
        (equip_o_ptr->artifact->aidx != ART_MORGOTH) &&
        (equip_o_ptr->artifact->aidx != ART_GROND))
    {
        object_desc(p_ptr, o_name, sizeof(o_name), equip_o_ptr, ODESC_BASE);
        msg(p_ptr, "Your pack is full and you can't drop the %s here.", o_name);
        return;
    }

    /*
     * Hack -- prevent anyone but total winners from wielding the Massive Iron
     * Crown of Morgoth or the Mighty Hammer 'Grond'.
     */
    if (!p_ptr->total_winner)
    {
        /*
         * Attempting to wear the crown if you are not a winner is a very,
         * very bad thing to do.
         */
        if (o_ptr->artifact && (o_ptr->artifact->aidx == ART_MORGOTH))
        {
            msg(p_ptr, "You are blasted by the Crown's power!");

            /* This should pierce invulnerability */
            my_strcpy(p_ptr->died_flavor,
                "was blasted by the Massive Iron Crown of Morgoth",
                sizeof(p_ptr->died_flavor));
            take_hit(p_ptr, 10000, "the Massive Iron Crown of Morgoth", FALSE);
            return;
        }

        /* Attempting to wield Grond isn't so bad. */
        if (o_ptr->artifact && (o_ptr->artifact->aidx == ART_GROND))
        {
            msg(p_ptr, "You are far too weak to wield the mighty Grond.");
            return;
        }
    }

    wield_item(Ind, o_ptr, item, slot, TRUE);
}


/* Drop an item */
void do_cmd_drop(int Ind, int item, int quantity)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to drop items!");
        return;
    }

    /* Handle the newbies_cannot_drop option */
    if (newbies_cannot_drop(p_ptr))
    {
        msg(p_ptr, "You are not experienced enough to drop items.");
        return;
    }

    /* Check preventive inscription '^d' */
    __trap(p_ptr, CPI(p_ptr, 'd'))

    /* Paranoia */
    if (item < 0) return;

    /* Get the item */
    o_ptr = &p_ptr->inventory[item];

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, 'd', FALSE))

    /* Cannot remove cursed items */
    if ((item >= INVEN_WIELD) && cursed_p(o_ptr->flags))
    {
        /* Oops */
        msg(p_ptr, "Hmmm, it seems to be cursed.");

        /* Nope */
        return;
    }        

    /* Take a turn */
    use_energy(Ind);

    /* Drop (some of) the item */
    inven_drop(p_ptr, item, quantity, FALSE);
}


/*
 * Destroy an item
 */
void do_cmd_destroy(int Ind, int item, bool des)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    char o_name[NORMAL_WID];

    /* Restrict ghosts */
    if (des && p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to destroy items!");
        return;
    }

    /* Check preventive inscription '^k' */
    __trap(p_ptr, CPI(p_ptr, 'k'))

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Some checks */
    if (item < 0)
    {
        /* Restricted by choice */
        if (!is_owner(p_ptr, o_ptr))
        {
            msg(p_ptr, "This item belongs to someone else!");
            return;
        }

        /* The inscription prevents it */
        __trap(p_ptr, CGI(o_ptr, 'g', FALSE))
    }

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, 'k', FALSE))

    /* Can't ignore or destroy cursed items we're wielding. */
    if ((item >= INVEN_WIELD) && cursed_p(o_ptr->flags))
    {
        /* Message */
        if (des) msg(p_ptr, "You cannot destroy the cursed item.");
        else msg(p_ptr, "You cannot ignore cursed items.");

        /* Done */
        return;
    }

    /* Describe */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Artifacts cannot be destroyed */
    if (des && o_ptr->artifact)
    {
        /* Message */
        msg(p_ptr, "You cannot destroy %s.", o_name);

        /* Mark the object as indestructible */
        object_notice_indestructible(Ind, o_ptr);

        /* Combine the pack */
        p_ptr->notice |= (PN_COMBINE);

        /* Redraw */
        p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

        /* Done */
        return;
    }

    /* Destroying or squelching from equipment? Update object flags! */
    if (item >= INVEN_WIELD) p_ptr->redraw |= (PR_EQUIP);

    /* Destroy */
    if (des)
    {
        /* Message */
        msgt(p_ptr, MSG_DESTROY, "You destroy %s.", o_name);

        /* Reduce the charges of rods/wands/staves */
        reduce_charges(o_ptr, o_ptr->number);

        /* Eliminate the item */
        item_decrease(p_ptr, item, o_ptr->number, TRUE);
    }

    /* Ignore */
    else
    {
        /* Message */
        if (o_ptr->ignore)
            msgt(p_ptr, MSG_DESTROY, "Showing %s again.", o_name);
        else
            msgt(p_ptr, MSG_DESTROY, "Ignoring %s.", o_name);

        /* Set squelch flag as appropriate */
        p_ptr->notice |= PN_SQUELCH;

        /* Toggle ignore */
        o_ptr->ignore = !o_ptr->ignore;
    }
}


/*** Casting and browsing ***/


/*
 * Stat Table (INT/WIS) -- Minimum failure rate (percentage)
 */
static const byte adj_mag_fail[] =
{
    99  /* 3 */,
    99  /* 4 */,
    99  /* 5 */,
    99  /* 6 */,
    99  /* 7 */,
    50  /* 8 */,
    30  /* 9 */,
    20  /* 10 */,
    15  /* 11 */,
    12  /* 12 */,
    11  /* 13 */,
    10  /* 14 */,
    9   /* 15 */,
    8   /* 16 */,
    7   /* 17 */,
    6   /* 18/00-18/09 */,
    6   /* 18/10-18/19 */,
    5   /* 18/20-18/29 */,
    5   /* 18/30-18/39 */,
    5   /* 18/40-18/49 */,
    4   /* 18/50-18/59 */,
    4   /* 18/60-18/69 */,
    4   /* 18/70-18/79 */,
    4   /* 18/80-18/89 */,
    3   /* 18/90-18/99 */,
    3   /* 18/100-18/109 */,
    2   /* 18/110-18/119 */,
    2   /* 18/120-18/129 */,
    2   /* 18/130-18/139 */,
    2   /* 18/140-18/149 */,
    1   /* 18/150-18/159 */,
    1   /* 18/160-18/169 */,
    1   /* 18/170-18/179 */,
    1   /* 18/180-18/189 */,
    1   /* 18/190-18/199 */,
    0   /* 18/200-18/209 */,
    0   /* 18/210-18/219 */,
    0   /* 18/220+ */
};


/*
 * Returns chance of failure for a spell
 */
static s16b spell_chance(struct player *p, int spell)
{
    int chance, minfail;
    const magic_type *s_ptr;

    /* Paranoia -- must be literate */
    if (!p->clazz->spell_book) return (100);

    /* Get the spell */
    s_ptr = &p->clazz->spells.info[spell];

    /* Extract the base spell failure rate */
    chance = s_ptr->sfail;

    /* Reduce failure rate by "effective" level adjustment */
    chance -= 3 * (p->lev - s_ptr->slevel);

    /* Reduce failure rate by INT/WIS adjustment */
    chance -= adj_mag_stat[p->state.stat_ind[p->clazz->spell_stat]];

    /* Extract the minimum failure rate */
    minfail = adj_mag_fail[p->state.stat_ind[p->clazz->spell_stat]];

    /* Non mage/priest characters never get better than 5 percent */
    if (!player_has(p, PF_ZERO_FAIL) && (minfail < 5)) minfail = 5;

    /* Priest prayer penalty for "edged" weapons (before minfail) */
    if (p->state.icky_wield) chance += 25;

    /* Fear makes spells harder (before minfail) */
    if (check_state(p, OF_AFRAID)) chance += 20;

    /* Minimal and maximal failure rate */
    if (chance < minfail) chance = minfail;
    if (chance > 50) chance = 50;

    /* Stunning makes spells harder */
    if (p->timed[TMD_STUN] > 50) chance += 25;
    else if (p->timed[TMD_STUN]) chance += 15;

    /* Amnesia doubles failure change */
    if (p->timed[TMD_AMNESIA]) chance = 50 + chance / 2;

    /* Always a 5 percent chance of working */
    if (chance > 95) chance = 95;

    /* Return the chance */
    return (chance);
}


/*
 * Print a list of spells (for browsing or casting)
 */
static void print_spells(struct player *p, int book, byte *spell, int num)
{
    int i, j;
    const magic_type *s_ptr;
    char out_val[NORMAL_WID];
    char out_desc[MSG_LEN];
    byte line_attr;
    char help[20];
    const char *comment = help;
    spell_flags flags;
    char spell_name[31];

    /* Dump the spells */
    for (i = 0; i < num; i++)
    {
        /* Get the spell index */
        j = spell[i];

        /* Get the spell info */
        s_ptr = &p->clazz->spells.info[j];

        /* Skip illegible spells */
        if (s_ptr->slevel >= 99)
        {
            flags.line_attr = TERM_L_DARK;
            flags.flag = RSF_NONE;
            flags.dir_attr = 0;
            flags.proj_attr = 0;

            my_strcpy(out_val, "(illegible)", sizeof(out_val));
            my_strcpy(out_desc, "", sizeof(out_desc));

            Send_spell_info(p, book, i, out_val, &flags);
            Send_spell_desc(p, book, i, out_desc);
            continue;
        }

        /* Get extra info */
        get_spell_info(p, j, help, sizeof(help));

        /* Assume spell is known and tried */
        comment = help;
        line_attr = TERM_WHITE;

        /* Analyze the spell */
        if (p->spell_flags[j] & PY_SPELL_FORGOTTEN)
        {
            comment = " forgotten";
            line_attr = TERM_YELLOW;
        }
        else if (!(p->spell_flags[j] & PY_SPELL_LEARNED))
        {
            if (s_ptr->slevel <= p->lev)
            {
                comment = " unknown";
                line_attr = TERM_L_BLUE;
            }
            else
            {
                comment = " difficult";
                line_attr = TERM_RED;
            }
        }
        else if (!(p->spell_flags[j] & PY_SPELL_WORKED))
        {
            comment = " untried";
            line_attr = TERM_L_GREEN;
        }

        flags.line_attr = line_attr;
        flags.flag = RSF_NONE;
        flags.dir_attr = get_spell_dir(p->clazz->spell_book, j);
        flags.proj_attr = get_spell_proj(p->clazz->spell_book, j);

        /* Dump the spell --(-- */
        if (p->clazz->spell_book == TV_ELEM_BOOK)
        {
            strnfmt(spell_name, sizeof(spell_name), "%s (%d)",
                get_spell_name(p->clazz->spell_book, j), p->spell_power[j]);
        }
        else
            my_strcpy(spell_name, get_spell_name(p->clazz->spell_book, j), sizeof(spell_name));
        strnfmt(out_val, sizeof(out_val), "%-30s%2d %4d %3d%%%s", spell_name,
            s_ptr->slevel, s_ptr->smana, spell_chance(p, j), comment);
        my_strcpy(out_desc, get_spell_desc(p->clazz->spell_book, j), sizeof(out_desc));

        Send_spell_info(p, book, i, out_val, &flags);
        Send_spell_desc(p, book, i, out_desc);
    }
}


/* Peruse spells in a book */
void do_cmd_browse(struct player *p, int book)
{
    int item;
    struct spell *sp;
    byte spell[PY_MAX_SPELLS], num = 0;
    object_type *o_ptr;

    /* Hack -- Must be literate */
    if (!p->clazz->spell_book)
    {
        msg(p, "You cannot read books!");
        return;
    }

    /* Restrict ghosts */
    /* One exception: players in undead form can read books (from pack only) */
    if (p->ghost && !is_dm_p(p) && !(player_undead(p) && (book >= 0)))
    {
        msg(p, "You cannot read books!");
        return;
    }

    item = book;

    /* Get the item */
    o_ptr = object_from_item_idx(p, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Paranoia: requires a spellbook */
    if (o_ptr->tval != p->clazz->spell_book) return;

    /* Extract spells */
    for (sp = o_ptr->kind->spells; sp; sp = sp->next)
    {
        /* Collect this spell */
        spell[num++] = sp->spell_index;
    }

    /* Display the spells */
    print_spells(p, book, spell, num);
}


/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
static bool spell_okay(int Ind, int j, bool known)
{
    player_type *p_ptr = player_get(Ind);
    const magic_type *s_ptr;

    /* Get the spell */
    s_ptr = &p_ptr->clazz->spells.info[j];

    /* Spell is illegible - never ok */
    if (s_ptr->slevel > p_ptr->lev) return (FALSE);

    /* Spell is forgotten - never ok */
    if (p_ptr->spell_flags[j] & PY_SPELL_FORGOTTEN) return (FALSE);

    /* Spell is learned - cast ok, no study */
    if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED) return (known);

    /* Spell has never been learned - study ok, no cast */
    return (!known);
}


/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * Returns -1 if the user hits escape.
 * Returns -2 if there are no legal choices.
 * Returns a valid spell otherwise.
 *
 * The "prompt" should be "cast", "recite", "study", or "use"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */
static int get_spell(int Ind, object_type *o_ptr, int spell, const char *prompt, bool known)
{
    int j;
    struct spell *sp;
    byte spells[PY_MAX_SPELLS], num = 0;
    player_type *p_ptr = player_get(Ind);

    /* Extract spells */
    for (sp = o_ptr->kind->spells; sp; sp = sp->next)
    {
        /* Collect this spell */
        spells[num++] = sp->spell_index;
    }

    /* Set the spell number */
    if (spell < PY_MAX_SPELLS)
        j = spells[spell];
    else
    {
        j = spells[spell - PY_MAX_SPELLS];

        /* Projected spells */
        if (!get_spell_proj(p_ptr->clazz->spell_book, j))
        {
            msg(p_ptr, "You cannot project that spell.");
            return -1;
        }
    }

    /* Verify the spell */
    if (!spell_okay(Ind, j, known))
    {
        if (prompt) msg(p_ptr, prompt);
        return -1;
    }

    return j;
}


/* Study a book to gain a new spell */
void do_cmd_study(int Ind, int book, int spell)
{
    player_type *p_ptr = player_get(Ind);
    int i;
    int j = -1;
    struct spell *sp;
    object_type *o_ptr;
    const char *p;
    const char *prompt;

    /* Restrict ghosts */
    /* One exception: players in undead form can read books (from pack only) */
    if (p_ptr->ghost && !is_dm_p(p_ptr) && !(player_undead(p_ptr) && (book >= 0)))
    {
        msg(p_ptr, "You cannot read books!");
        return;
    }

    if (p_ptr->clazz->spell_book == TV_PRAYER_BOOK)
    {
        p = "prayer";
        prompt = "You cannot learn that prayer!";
    }
    else
    {
        p = "spell";
        prompt = "You cannot learn that spell!";
    }

    if (!player_can_cast_msg(p_ptr)) return;

    /* Check preventive inscription '^G' */
    __trap(p_ptr, CPI(p_ptr, 'G'))

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, book, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Restricted by choice */
    if ((book < 0) && !is_owner(p_ptr, o_ptr))
    {
        msg(p_ptr, "This item belongs to someone else!");
        return;
    }

    /* Paranoia: requires a spellbook */
    if (o_ptr->tval != p_ptr->clazz->spell_book) return;

    /* Elementalists can increase the power of their spells */
    if (p_ptr->clazz->spell_book == TV_ELEM_BOOK)
    {
        /* Check if spell is learned */
        j = get_spell(Ind, o_ptr, spell, NULL, TRUE);
        if (j != -1)
        {
            int max_power = ((o_ptr->sval == SV_ELEM_BOOK)? 10: 5);
            const magic_type *s_ptr = &p_ptr->clazz->spells.info[j];

            /* Check max spellpower */
            if (p_ptr->spell_power[j] == max_power)
            {
                msg(p_ptr, "You already know everything about this spell.");
                return;
            }

            /* Check level */
            if (s_ptr->slevel + p_ptr->spell_power[j] > p_ptr->lev)
            {
                msg(p_ptr, "You are too low level to improve this spell.");
                return;
            }

            /* Check allocated points */
            max_power = 0;
            for (i = 0; i < PY_MAX_SPELLS; i++)
                max_power += p_ptr->spell_power[i];
            if (max_power >= p_ptr->lev * 2)
            {
                msg(p_ptr, "You are too low level to improve this spell.");
                return;
            }

            /* Take a turn */
            use_energy(Ind);

            /* Improve spellpower */
            p_ptr->spell_power[j]++;

            /* Mention the result */
            msgt(p_ptr, MSG_STUDY, "You improve your knowledge of the %s spell.",
                get_spell_name(p_ptr->clazz->spell_book, j));

            /* Redraw */
            p_ptr->redraw |= PR_SPELL;

            return;
        }
    }

    if (!p_ptr->new_spells)
    {
        msg(p_ptr, "You cannot learn any new %ss!", p);
        return;
    }

    /* Spellcaster -- Learn a selected spell */
    if (player_has(p_ptr, PF_CHOOSE_SPELLS))
    {
        /* Ask for a spell */
        j = get_spell(Ind, o_ptr, spell, prompt, FALSE);

        /* Allow cancel */
        if (j == -1) return;
    }

    /* Cleric -- Learn a random prayer */
    else
    {
        int k = 0;
        int gift = -1;

        /* Extract prayers */
        for (sp = o_ptr->kind->spells; sp; sp = sp->next)
        {
            /* Skip non "okay" prayers */
            if (!spell_okay(Ind, sp->spell_index, FALSE)) continue;

            /* Apply the randomizer */
            if ((++k > 1) && randint0(k)) continue;

            /* Track it */
            gift = sp->spell_index;
        }

        /* Accept gift */
        j = gift;
    }

    /* Nothing to study */
    if (j < 0)
    {
        /* Message */
        msg(p_ptr, "You cannot learn any %ss in that book.", p);

        /* Abort */
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Learn the spell */
    p_ptr->spell_flags[j] |= PY_SPELL_LEARNED;
    p_ptr->spell_power[j]++;

    /* Find the next open entry in "spell_order[]" */
    for (i = 0; i < PY_MAX_SPELLS; i++)
    {
        /* Stop at the first empty space */
        if (p_ptr->spell_order[i] == 99) break;
    }

    /* Add the spell to the known list */
    p_ptr->spell_order[i++] = j;

    /* Mention the result */
    msgt(p_ptr, MSG_STUDY, "You have learned the %s of %s.",
        p, get_spell_name(p_ptr->clazz->spell_book, j));

    /* One less spell available */
    p_ptr->new_spells--;

    /* Message if needed */
    if (p_ptr->new_spells)
    {
        /* Message */
        msg(p_ptr, "You can learn %d more %s%s.", p_ptr->new_spells, p,
            PLURAL(p_ptr->new_spells));
    }

    /* Redraw */
    p_ptr->redraw |= (PR_STUDY | PR_SPELL);
}


/* Cast the specified spell */
static bool spell_cast(int Ind, int spell, int dir, quark_t note, bool projected)
{
    player_type *p_ptr = player_get(Ind);
    int chance;
    int old_num = get_player_num(p_ptr);

    /* Spell failure chance */
    chance = spell_chance(p_ptr, spell);

    /* Failed spell */
    if (magik(chance))
        msg(p_ptr, "You failed to concentrate hard enough!");

    /* Process spell */
    else
    {
        /* Affect other spell */
        if (projected) spell += PY_MAX_SPELLS;

        /* Set current spell */
        p_ptr->current_spell = spell;

        /* Save current inscription */
        p_ptr->current_item = (s16b)note;

        /* Cast the spell */
        if (!cast_spell(p_ptr, spell, note, dir)) return FALSE;

        /* A spell was cast */
        sound(p_ptr, ((p_ptr->clazz->spell_book == TV_PRAYER_BOOK)? MSG_PRAYER: MSG_SPELL));

        cast_spell_end(Ind);
    }

    /* Use some mana */
    p_ptr->csp -= p_ptr->spell_cost;

    /* Hack -- Redraw picture */
    redraw_picture(p_ptr, old_num);

    /* Redraw mana */
    p_ptr->redraw |= (PR_MANA);

    return TRUE;
}


/* Cast a spell from a book */
void do_cmd_cast(int Ind, int book, int spell, int dir)
{
    player_type *p_ptr = player_get(Ind);
    byte spell_book = p_ptr->clazz->spell_book;
    const char *verb, *noun, *prompt;
    char what = ((spell_book == TV_PRAYER_BOOK)? 'p': 'm');
    object_type *o_ptr;
    int j;
    const magic_type *s_ptr;

    /* Clear current */
    current_clear(p_ptr);

    if (spell_book == TV_PRAYER_BOOK)
    {
        verb = "recite";
        noun = "prayer";
    }
    else if (spell_book == TV_PSI_BOOK)
    {
        verb = "use";
        noun = "psi power";
    }
    else
    {
        verb = "cast";
        noun = "spell";
    }

    /* Restrict ghosts */
    /* One exception: players in undead form can cast spells (from pack only) */
    if (p_ptr->ghost && !is_dm_p(p_ptr) && !(player_undead(p_ptr) && (book >= 0)))
    {
        msg(p_ptr, "You cannot %s that %s.", verb, noun);
        return;
    }

    if (!player_can_cast_msg(p_ptr)) return;

    /* Check preventive inscription '^m' */
    /* Check preventive inscription '^p' */
    __trap(p_ptr, CPI(p_ptr, what));

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, book, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Restricted by choice */
    if ((book < 0) && !is_owner(p_ptr, o_ptr))
    {
        msg(p_ptr, "This item belongs to someone else!");
        return;
    }

    /* Paranoia: requires a spellbook */
    if (o_ptr->tval != p_ptr->clazz->spell_book) return;

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, what, FALSE));

    /* Ask for a spell */
    prompt = format("You cannot %s that %s.", verb, noun);
    j = get_spell(Ind, o_ptr, spell, prompt, TRUE);
    if (j == -1) return;

    /* Access the spell */
    s_ptr = &p_ptr->clazz->spells.info[j];

    /* Check mana */
    if (s_ptr->smana > p_ptr->csp)
    {
        msg(p_ptr, "You do not have enough mana.");
        return;
    }

    /* Antimagic field (no effect on psi powers which are not "magical") */
    if ((spell_book != TV_PSI_BOOK) && check_antimagic(p_ptr, NULL)) return;

    /* Spell cost */
    p_ptr->spell_cost = s_ptr->smana;

    /* Cast a spell */
    if (spell_cast(Ind, j, dir, o_ptr->note, (spell >= PY_MAX_SPELLS)? TRUE: FALSE))
        use_energy(Ind);
}


/*** Using items the traditional way ***/


/* Basic tval testers */
static bool item_tester_hook_use(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    /* Non-staves are out */
    if (o_ptr->tval != TV_STAFF) return FALSE;

    /* Notice empty staves */
    if (o_ptr->pval[DEFAULT_PVAL] <= 0)
    {
        msg(p_ptr, "The staff has no charges left.");
        o_ptr->ident |= (IDENT_EMPTY);
        return FALSE;
    }

    /* Otherwise OK */
    return TRUE;
}


static bool item_tester_hook_aim(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    /* Non-wands are out */
    if (o_ptr->tval != TV_WAND) return FALSE;

    /* Notice empty wands */
    if (o_ptr->pval[DEFAULT_PVAL] <= 0)
    {
        msg(p_ptr, "The wand has no charges left.");
        o_ptr->ident |= (IDENT_EMPTY);
        return FALSE;
    }

    /* Otherwise OK */
    return TRUE;
}


static bool item_tester_hook_eat(int Ind, object_type *o_ptr)
{
    return is_food(o_ptr);
}


static bool item_tester_hook_quaff(int Ind, object_type *o_ptr)
{
    return (o_ptr->tval == TV_POTION);
}


static bool item_tester_hook_read(int Ind, object_type *o_ptr)
{
    return (o_ptr->tval == TV_SCROLL);
}


/* Determine if an object is zappable */
static bool item_tester_hook_zap(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    /* Non-rods are out */
    if (o_ptr->tval != TV_ROD) return FALSE;

    /* All still charging? */
    if (number_charging(o_ptr) == o_ptr->number)
    {
        msg(p_ptr, "The rod is still charging.");
        return FALSE;
    }

    /* Otherwise OK */
    return TRUE;
}


/* Determine if an object is activatable */
static bool item_tester_hook_activate(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    /* Check the recharge */
    if (o_ptr->timeout)
    {
        msg(p_ptr, "The item is still charging.");
        return (FALSE);
    }

    /* Check effect */
    if (o_ptr->effect) return (TRUE);

    /* Assume not */
    return (FALSE);
}


/* List of commands */
enum
{
    CMD_EAT = 0,
    CMD_QUAFF,
    CMD_READ,
    CMD_USE,
    CMD_AIM,
    CMD_ZAP,
    CMD_ACTIVATE
};


/* List of command parameters */
static cmd_param cmd_params[] =
{
    {DM_GHOST_BODY, FALSE, "You need a tangible body to eat food!",
        'E', 0, 'E', FALSE, USE_SINGLE, MSG_EAT, item_tester_hook_eat},
    {DM_GHOST_BODY, FALSE, "You need a tangible body to quaff potions!",
        'q', 0, 'q', FALSE, USE_SINGLE, MSG_QUAFF, item_tester_hook_quaff},
    {DM_GHOST_HANDS, TRUE, "You cannot read scrolls!",
        'r', 0, 'r', FALSE, USE_SINGLE, MSG_GENERIC, item_tester_hook_read},
    {DM_GHOST_HANDS, TRUE, "You cannot use staves!",
        'u', 0, 'u', TRUE, USE_CHARGE, MSG_USE_STAFF, item_tester_hook_use},
    {DM_GHOST_HANDS, TRUE, "You cannot aim wands!",
        'a', 0, 'a', TRUE, USE_CHARGE, MSG_GENERIC, item_tester_hook_aim},
    {DM_GHOST_HANDS, TRUE, "You cannot zap rods!",
        'z', 0, 'z', TRUE, USE_TIMEOUT, MSG_ZAP_ROD, item_tester_hook_zap},
    {DM_GHOST_BODY, TRUE, "You need a tangible body to activate items!",
        0, INVEN_WIELD, 'A', TRUE, USE_TIMEOUT, MSG_ACT_ARTIFACT, item_tester_hook_activate}
};


/* Get an o_ptr from an item number (bypassing squelching) */
static object_type *obj_from_item_idx(struct player *p, int item)
{
    s16b this_o_idx, next_o_idx;

    /* Get the item (from pack or equipment) */
    if (item >= 0) return &p->inventory[item];

    /* Get the item (on the floor) */
    for (this_o_idx = cave_get(p->depth)->o_idx[p->py][p->px]; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Ignore all non-objects */
        if (!o_ptr->kind) continue;

        /* Ignore all hidden objects */
        if (!object_marked(p, this_o_idx)) continue;

        /* Check item (on the floor) */
        if (this_o_idx == 0 - item) return object_byid(this_o_idx);
    }

    /* Nothing */
    return NULL;
}


static void do_cmd_use_end(int Ind, bool ident, bool used, use_type use)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    int item = p_ptr->current_item;

    /* Get the item */
    o_ptr = obj_from_item_idx(p_ptr, item);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Mark as tried and redisplay */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER);
    p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

    /*
     * If the player becomes aware of the item's function, then mark it as
     * aware and reward the player with some experience. Otherwise, mark
     * it as "tried".
     */
    object_aware_tried(Ind, o_ptr, ident, used);

    /* Hack -- Delay pack updates when an item request is pending */
    if (p_ptr->current_value == ITEM_PENDING) p_ptr->notice |= PN_WAIT;
    else p_ptr->notice &= ~(PN_WAIT);

    /* Some uses are "free" */
    if (!used) return;

    /* Chargeables act differently to single-used items when not used up */
    if (use == USE_CHARGE)
    {
        /* Use a single charge */
        o_ptr->pval[DEFAULT_PVAL]--;

        /* Describe charges */
        if (item >= 0)
            inven_item_charges(Ind, item);

        /* Redraw */
        else
            p_ptr->redraw |= (PR_FLOOR | PR_ITEMLIST);
    }

    else if (use == USE_TIMEOUT)
    {
        random_value *timeout = &o_ptr->time;

        /* Rods: drain the charge */
        if (o_ptr->tval == TV_ROD)
        {
            o_ptr->timeout += randcalc(*timeout, 0, RANDOMISE);

            /* Redraw */
            if (item < 0) p_ptr->redraw |= (PR_FLOOR | PR_ITEMLIST);
        }

        /* Other activatable items */
        else
            o_ptr->timeout = randcalc(*timeout, 0, RANDOMISE);
    }

    else if (use == USE_SINGLE)
    {
        /* Destroy an item */
        item_decrease(p_ptr, item, 1, TRUE);
    }
}


/*
 * Use an object the right way.
 *
 * There may be a BIG problem with any "effect" that can cause "changes"
 * to the inventory.  For example, a "scroll of recharging" can cause
 * a wand/staff to "disappear", moving the inventory up.  Luckily, the
 * scrolls all appear BEFORE the staffs/wands, so this is not a problem.
 * But, for example, a "staff of recharging" could cause MAJOR problems.
 * In such a case, it will be best to either (1) "postpone" the effect
 * until the end of the function, or (2) "change" the effect, say, into
 * giving a staff "negative" charges, or "turning a staff into a stick".
 * It seems as though a "rod of recharging" might in fact cause problems.
 * The basic problem is that the act of recharging (and destroying) an
 * item causes the inducer of that action to "move", causing "o_ptr" to
 * no longer point at the correct item, with horrifying results.
 */
static void do_cmd_use(int Ind, int item, int dir, cmd_param *p_cmd)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    bool ident = FALSE, used = FALSE;

    /* Clear current */
    current_clear(p_ptr);

    /* Set current item */
    p_ptr->current_item = item;

    /* Restrict ghosts */
    /* Sometimes players in undead form can use items (from pack only) */
    if (p_ptr->ghost && !(p_ptr->dm_flags & p_cmd->dm_flag) &&
        !(p_cmd->player_undead && player_undead(p_ptr) && (item >= 0)))
    {
        msg(p_ptr, p_cmd->msg_ghost);
        return;
    }

    /* Check preventive inscription */
    if (p_cmd->p_note)
    {
        __trap(p_ptr, CPI(p_ptr, p_cmd->p_note))
    }

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, item, p_cmd->inv_start, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Some checks */
    if (item < 0)
    {
        /* Restricted by choice */
        if (!is_owner(p_ptr, o_ptr))
        {
            msg(p_ptr, "This item belongs to someone else!");
            return;
        }

        /* The inscription prevents it */
        __trap(p_ptr, CGI(o_ptr, 'g', FALSE))
    }

    /* Paranoia: requires a proper object */
    if (!p_cmd->item_tester_hook(Ind, o_ptr)) return;

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, p_cmd->g_note, FALSE))

    /* Antimagic field (except horns which are not magical) */
    if (p_cmd->check_antimagic && (o_ptr->tval != TV_HORN) && check_antimagic(p_ptr, NULL))
        return;

    /* Apply confusion */
    player_confuse_dir(p_ptr, &dir);

    /* The player is aware of the object's flavour */
    p_ptr->was_aware = object_flavor_is_aware(p_ptr, o_ptr);

    /* Check for use if necessary, and execute the effect */
    if (((p_cmd->use != USE_CHARGE) && (p_cmd->use != USE_TIMEOUT)) || check_devices(Ind, o_ptr))
    {
        /* Sound */
        sound(p_ptr, p_cmd->snd);

        /* Special message for artifacts */
        if (o_ptr->artifact)
        {
            artifact_type *a_ptr = get_artifact(o_ptr);

            if (a_ptr->effect_msg) activation_message(Ind, o_ptr, a_ptr->effect_msg);
        }

        /* Cloak of lordly resistance can be activated for resistance */
        else if (o_ptr->ego && (o_ptr->ego->eidx == EGO_CLOAK_LORDLY_RES))
            msg(p_ptr, "Your cloak flashes many colors...");

        /* Use the object */
        used = use_object(Ind, o_ptr, &ident, dir);

        /* Quit if the item wasn't used and no knowledge was gained */
        if (!used && (p_ptr->was_aware || !ident)) return;
    }

    /* Take a turn */
    use_energy(Ind);

    do_cmd_use_end(Ind, ident, used, p_cmd->use);

    /* Hack -- Rings of Polymorphing get destroyed when activated */
    if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_POLYMORPHING) && used)
    {
        msg(p_ptr, "Your ring explodes in a bright flash of light!");
        item_decrease(p_ptr, item, 1, TRUE);
    }
}


/* Use a staff */
void do_cmd_use_staff(int Ind, int item)
{
    do_cmd_use(Ind, item, 0, &cmd_params[CMD_USE]);
}


/* Aim a wand */
void do_cmd_aim_wand(int Ind, int item, int dir)
{
    /* Use the object */
    do_cmd_use(Ind, item, dir, &cmd_params[CMD_AIM]);
}


/* Zap a rod */
void do_cmd_zap_rod(int Ind, int item, int dir)
{
    /* Use the object */
    do_cmd_use(Ind, item, dir, &cmd_params[CMD_ZAP]);
}


/* Activate a wielded object */
void do_cmd_activate(int Ind, int item, int dir)
{
    /* Use the object */
    do_cmd_use(Ind, item, dir, &cmd_params[CMD_ACTIVATE]);
}


/* Eat some food */
void do_cmd_eat_food(int Ind, int item)
{
    /* Use the object */
    do_cmd_use(Ind, item, 0, &cmd_params[CMD_EAT]);
}


/* Quaff a potion (from the pack or the floor) */
void do_cmd_quaff_potion(int Ind, int item)
{
    /* Use the object */
    do_cmd_use(Ind, item, 0, &cmd_params[CMD_QUAFF]);
}


/* Determine if the player can read scrolls. */
static bool can_read_scroll(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    if (p_ptr->timed[TMD_BLIND])
    {
        msg(p_ptr, "You can't see anything.");
        return FALSE;
    }

    if (no_light(p_ptr))
    {
        msg(p_ptr, "You have no light to read by.");
        return FALSE;
    }

    if (p_ptr->timed[TMD_CONFUSED])
    {
        msg(p_ptr, "You are too confused to read!");
        return FALSE;
    }

    if (one_in_(2) && p_ptr->timed[TMD_AMNESIA])
    {
        msg(p_ptr, "You can't remember how to read!");
        return FALSE;
    }

    return TRUE;
}


/* Read a scroll (from the pack or floor) */
void do_cmd_read_scroll(int Ind, int item)
{
    /* Check some conditions */
    if (!can_read_scroll(Ind)) return;

    /* Use the object */
    do_cmd_use(Ind, item, 0, &cmd_params[CMD_READ]);
}


/* Use an item */
void do_cmd_use_any(int Ind, int item, int dir)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Fire a missile */
    if (o_ptr->tval == p_ptr->state.ammo_tval) do_cmd_fire(Ind, dir, item);

    /* Eat some food */
    else if (item_tester_hook_eat(Ind, o_ptr)) do_cmd_eat_food(Ind, item);

    /* Quaff a potion */
    else if (item_tester_hook_quaff(Ind, o_ptr)) do_cmd_quaff_potion(Ind, item);

    /* Read a scroll */
    else if (item_tester_hook_read(Ind, o_ptr)) do_cmd_read_scroll(Ind, item);

    /* Use a staff */
    else if (item_tester_hook_use(Ind, o_ptr)) do_cmd_use_staff(Ind, item);

    /* Aim a wand */
    else if (item_tester_hook_aim(Ind, o_ptr)) do_cmd_aim_wand(Ind, item, dir);

    /* Zap a rod */
    else if (item_tester_hook_zap(Ind, o_ptr)) do_cmd_zap_rod(Ind, item, dir);

    /* Activate a wielded object */
    else
    {
        /* Get the item (again, restrict equipment) */
        o_ptr = object_from_item_idx(p_ptr, item, INVEN_WIELD, TRUE);

        /* Paranoia: requires an item */
        if (!o_ptr || !o_ptr->kind) return;

        /* Activate */
        if (item_tester_hook_activate(Ind, o_ptr)) do_cmd_activate(Ind, item, dir);

        /* Oops */
        else msg(p_ptr, "You cannot use that!");
    }
}


/*** Refuelling ***/


/*
 * Hook for refilling lamps
 */
static bool item_tester_refill_lamp(object_type *o_ptr)
{
    bitflag f[OF_SIZE];

    /* Get flags */
    object_flags(o_ptr, f);

    /* Flasks of oil are okay */
    if (o_ptr->tval == TV_FLASK) return (TRUE);

    /* Non-empty, non-everburning lamps are okay */
    if ((o_ptr->tval == TV_LIGHT) && is_lamp(o_ptr) && (o_ptr->timeout > 0) && !of_has(f, OF_NO_FUEL))
        return (TRUE);

    /* Assume not okay */
    return (FALSE);
}


/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    object_type *j_ptr;

    /* Get the item */
    o_ptr = object_from_item_idx(p_ptr, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return;

    /* Some checks */
    if (item < 0)
    {
        /* Restricted by choice */
        if (!is_owner(p_ptr, o_ptr))
        {
            msg(p_ptr, "This item belongs to someone else!");
            return;
        }

        /* The inscription prevents it */
        __trap(p_ptr, CGI(o_ptr, 'g', FALSE))
    }

    /* Paranoia: requires a refill */
    if (!item_tester_refill_lamp(o_ptr)) return;

    /* Take a turn */
    use_energy(Ind);

    /* Get the lamp */
    j_ptr = &p_ptr->inventory[INVEN_LIGHT];

    /* Refuel */
    j_ptr->timeout += (o_ptr->timeout? o_ptr->timeout: o_ptr->pval[DEFAULT_PVAL]);

    /* Message */
    msg(p_ptr, "You fuel your lamp.");

    /* Comment */
    if (j_ptr->timeout >= FUEL_LAMP)
    {
        j_ptr->timeout = FUEL_LAMP;
        msg(p_ptr, "Your lamp is full.");
    }

    /* Refilled from a lamp */
    if (is_lamp(o_ptr))
    {
        /* Unstack if necessary */
        if (o_ptr->number > 1)
        {
            object_type *i_ptr;
            object_type object_type_body;

            /* Get local object */
            i_ptr = &object_type_body;

            /* Obtain a local object */
            object_copy(i_ptr, o_ptr);

            /* Modify quantity */
            i_ptr->number = 1;

            /* Remove fuel */
            i_ptr->timeout = 0;

            /* Unstack the used item */
            o_ptr->number--;
            p_ptr->total_weight -= i_ptr->weight;

            /* Carry or drop */
            if (item >= 0)
                inven_carry(p_ptr, i_ptr, TRUE);
            else
                drop_near(p_ptr, cave_get(p_ptr->depth), i_ptr, 0, p_ptr->py, p_ptr->px, FALSE);
        }

        /* Empty a single lamp */
        else
        {
            /* No more fuel */
            o_ptr->timeout = 0;
        }

        /* Combine / Reorder the pack (later) */
        p_ptr->notice |= (PN_COMBINE | PN_REORDER);

        /* Redraw */
        p_ptr->redraw |= (PR_INVEN);
    }

    /* Refilled from a flask */
    else
    {
        /* Decrease the item */
        item_decrease(p_ptr, item, 1, TRUE);
    }

    /* Recalculate torch */
    p_ptr->update |= (PU_TORCH);

    /* Redraw */
    p_ptr->redraw |= (PR_EQUIP);
}


/*
 * Refill the players light source
 */
void do_cmd_refill(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr;
    bitflag f[OF_SIZE];

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to refill light sources!");
        return;
    }

    /* Get the light */
    o_ptr = &p_ptr->inventory[INVEN_LIGHT];

    /* Get flags */
    object_flags(o_ptr, f);

    /* The inscription prevents it */
    __trap(p_ptr, CGI(o_ptr, 'F', FALSE))

    if ((o_ptr->tval == TV_LIGHT) && is_lamp(o_ptr) && !of_has(f, OF_NO_FUEL))
        do_cmd_refill_lamp(Ind, item);
}


void do_cmd_read_scroll_end(int Ind, bool ident, bool used)
{
    do_cmd_use_end(Ind, ident, used, USE_SINGLE);
}


void do_cmd_use_staff_discharge(int Ind, bool ident, bool used)
{
    do_cmd_use_end(Ind, ident, used, USE_CHARGE);
}


void do_cmd_zap_rod_end(int Ind, bool ident, bool used)
{
    do_cmd_use_end(Ind, ident, used, USE_TIMEOUT);
}


bool use_object(int Ind, object_type *o_ptr, bool *ident, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int effect, beam = beam_chance(o_ptr->tval);
    bool used, no_ident = FALSE;
    int boost, level;
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;

    /* Food and potions can feed the player */
    if (is_food(o_ptr) || (o_ptr->tval == TV_POTION))
        player_set_food(p_ptr, p_ptr->food + o_ptr->pval[DEFAULT_PVAL]);

    /* Figure out effect to use */
    effect = o_ptr->effect;

    /* Do effect */
    if (effect)
    {
        if (o_ptr->artifact)
        {
            artifact_type *a_ptr = get_artifact(o_ptr);

            level = a_ptr->level;
        }
        else
            level = o_ptr->kind->level;

        /* Check for "obvious" effects beforehand */
        if (effect_obvious(effect)) object_flavor_aware(p_ptr, o_ptr);

        /* Boost damage effects if skill > difficulty */
        boost = MAX(p_ptr->state.skills[SKILL_DEVICE] - level, 0);

        /* Various hacks */
        object_copy(i_ptr, o_ptr);
        switch (effect)
        {
            case EF_TELE_LONG:
            {
                /* Use up one charge before teleporting the player */
                if (o_ptr->tval == TV_STAFF)
                    do_cmd_use_staff_discharge(Ind, TRUE, TRUE);

                /* Fall through */
            }

            case EF_RUNE:
            case EF_TELE_LEVEL:
            case EF_TELE_PHASE:
            {
                /* Use up the scroll first */
                if (o_ptr->tval == TV_SCROLL)
                    do_cmd_read_scroll_end(Ind, TRUE, TRUE);

                /* Already used up, don't call do_cmd_use_end again */
                no_ident = TRUE;

                break;
            }

            case EF_ANNOY_MON:
            {
                /* Message */
                if ((o_ptr->tval == TV_HORN) && !o_ptr->artifact)
                    msg(p_ptr, "Your horn sounds loud and clear...");
                break;
            }

            case EF_RECALL:
            {
                /* Recall depth */
                beam = (int)o_ptr->note;
                break;
            }

            case EF_GAIN_EXP:
            {
                /* Limit the effect of the Potion of Experience */
                if (!obj_own_p(p_ptr, o_ptr) || (o_ptr->origin == ORIGIN_STORE) ||
                    (o_ptr->askprice == 1))
                {
                    effect = EF_GAIN_EXP_LTD;
                }
                break;
            }

            case EF_DRINK_BREATH:
            {
                /* Use up the potion first, since breathing frost could destroy it */
                do_cmd_use_end(Ind, TRUE, TRUE, USE_SINGLE);

                /* Already used up, don't call do_cmd_use_end again */
                no_ident = TRUE;

                break;
            }

            case EF_RESIST_ACID:
            case EF_RESIST_ELEC:
            {
                /* Duration */
                beam = randint1(10) + 10;
                break;
            }

            case EF_POLY_RACE:
            {
                /* Monster race */
                beam = o_ptr->pval[which_pval(o_ptr, OF_POLY_RACE)];
                break;
            }
        }

        /* Do effect */
        used = effect_do(p_ptr, effect, ident, p_ptr->was_aware, dir, beam, boost);
        if (*ident)
        {
            /* Hack -- If the object has already been used up, use a copy for ID purposes */
            if (o_ptr && o_ptr->kind)
                object_notice_effect(Ind, o_ptr);
            else
                object_notice_effect(Ind, i_ptr);
        }
        if (no_ident) *ident = FALSE;
        return used;
    }

    /* Done */
    return FALSE;
}
