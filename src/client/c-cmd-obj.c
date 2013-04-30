/*
 * File: c-cmd-obj.c
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


#include "c-angband.h"
#include "../common/tvalsval.h"
#include "c-cmds.h"
#include "netclient.h"


/*** Taking off/putting on ***/


/* Can only put on wieldable items */
bool obj_can_wear(struct player *p, const object_type *o_ptr)
{
    /* Check for a usable slot */
    switch (o_ptr->tval)
    {
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_MSTAFF:
        case TV_BOW:
        {
            /* Dragons and Monks cannot use weapons */
            if (player_has(p, PF_DRAGON) || player_has(p, PF_MARTIAL_ARTS))
                return FALSE;

            return TRUE;
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
        case TV_BOLT: return TRUE;
    }

    /* Assume not wearable */
    return FALSE;
}


static bool obj_is_ring(struct player *p, const object_type *o_ptr)
{
    return (o_ptr->tval == TV_RING);
}


/*
 * Check if the object is inscribed @0 or @w0.
 */
static bool has_swap_tag(int slot)
{
    const char *s;
    char *buf = inventory_name[slot];
    char *buf2;

    /* Skip empty objects */
    if (!buf[0]) return (FALSE);

    /* Skip empty inscriptions */
    buf2 = strchr(buf, '{');
    if (!buf2) return (FALSE);

    /* Find a '@' */
    s = strchr(buf2, '@');

    /* Process all tags */
    while (s)
    {
        /* Check the normal tags */
        if (s[1] == '0')
        {
            /* Success */
            return (TRUE);
        }

        /* Check the special tags */
        if ((s[1] == 'w') && (s[2] == '0'))
        {
            /* Success */
            return (TRUE);
        }

        /* Find another '@' */
        s = strchr(s + 1, '@');
    }

    /* No such tag */
    return (FALSE);
}


/*
 * Wield or wear an item
 * Return -2 for abort
 */
int textui_obj_wield(int item)
{
    int slot = p_ptr->inventory[item].info_xtra.slot;

    /*
     * Usually if the slot is taken we'll just replace the item in the slot,
     * but in some cases we need to ask the user which slot they actually
     * want to replace
     */
    if (slot == inven_left)
    {
        /* Look up the tag */
        if (has_swap_tag(slot + 1)) slot++;
        else if (!has_swap_tag(slot))
        {
            const char *q = "Replace which ring? ";
            const char *s = "Error in textui_obj_wield, please report";

            item_tester_hook = obj_is_ring;
            if (!get_item(&slot, q, s, CMD_WIELD, USE_EQUIP)) return -2;
        }
    }
    if (slot == quiver_start)
    {
        /* Get item */
        const char *q = "Replace which ammunition? ";
        const char *s = "Error in textui_obj_wield, please report";

        item_tester_hook = obj_is_ammo;
        if (!get_item(&slot, q, s, CMD_WIELD, USE_EQUIP)) return -2;
    }

    return slot;
}


/* Drop an item */
int textui_cmd_drop(int item)
{
    int amt = 1;

    /* Get an amount */
    if (p_ptr->inventory[item].number > 1)
        amt = get_quantity("How many? ", p_ptr->inventory[item].number);

    return amt;
}


/*** Casting and browsing ***/


/*
 * Return the number of castable spells in the spellbook 'o_ptr'.
 */
static int spell_book_count_spells(const object_type *o_ptr,
    bool (*tester)(int book, int spell))
{
    int i;
    int n_spells = 0;

    for (i = 0; i < SPELLS_PER_BOOK; i++)
    {
        /* Check for end of the book */
        if (spell_info[o_ptr->info_xtra.index][i][0] == '\0') break;

        /* Spell is available */
        if (tester(o_ptr->info_xtra.index, i)) n_spells++;
    }

    return n_spells;
}


bool obj_can_browse(struct player *p, const object_type *o_ptr)
{
    return (o_ptr->tval == p->clazz->spell_book);
}


/* A prerequisite to browsing */
bool obj_browse_pre(void)
{
    if (p_ptr->ghost && !player_can_undead(p_ptr))
    {
        textui_spell_browse(0);
        return FALSE;
    }

    if (player_has(p_ptr, PF_MONSTER_SPELLS))
    {
        int page = 0;
        int i, num;
        char tmp[NORMAL_WID];

        /* Number of pages */
        do
        {
            /* Check for available spells */
            for (i = 0, num = 0; i < SPELLS_PER_BOOK; i++)
            {
                /* Check for end of the book */
                if (spell_info[page][i][0] == '\0') break;

                /* Spell is available */
                num++;
            }
            if (num > 0) page++;
        }
        while ((num > 0) && (page < BOOKS_PER_REALM));

        /* Forms with no spells */
        if (!page)
        {
            c_msg_print("You don't know any monster spells.");
            return FALSE;
        }

        /* Pick a page and display it */
        strnfmt(tmp, sizeof(tmp), "Select a page (1-%d): ", page);
        page = get_quantity(tmp, page);
        if (page) textui_spell_browse(page - 1);
        return FALSE;
    }

    if (!p_ptr->clazz->spell_book)
    {
        c_msg_print("You cannot read books!");
        return FALSE;
    }

    return TRUE;
}


bool obj_can_study(struct player *p, const object_type *o_ptr)
{
    return (obj_can_browse(p, o_ptr) &&
        (spell_book_count_spells(o_ptr, spell_okay_to_study) > 0));
}


/* A prerequisite to studying */
bool obj_study_pre(void)
{
    if (!p_ptr->clazz->spell_book)
    {
        c_msg_print("You cannot gain spells!");
        return FALSE;
    }

    if (p_ptr->ghost && !player_can_undead(p_ptr))
    {
        c_msg_print("You cannot gain spells!");
        return FALSE;
    }

    return TRUE;
}


bool obj_can_cast_from(struct player *p, const object_type *o_ptr)
{
    return (obj_can_browse(p, o_ptr) &&
        (spell_book_count_spells(o_ptr, spell_okay_to_cast) > 0));
}


/* A prerequisite to casting */
bool obj_cast_pre(void)
{
    /* Use a ghost ability */
    if (p_ptr->ghost && !player_can_undead(p_ptr))
    {
        textui_obj_cast(0);
        return FALSE;
    }

    /* Cast a monster spell */
    if (player_has(p_ptr, PF_MONSTER_SPELLS))
    {
        int page = 0, chosen_page;
        int i, num;
        char tmp[NORMAL_WID];

        /* Number of pages */
        do
        {
            /* Check for available spells */
            for (i = 0, num = 0; i < SPELLS_PER_BOOK; i++)
            {
                /* Check for end of the book */
                if (spell_info[page][i][0] == '\0') break;

                /* Spell is available */
                num++;
            }
            if (num > 0) page++;
        }
        while ((num > 0) && (page < BOOKS_PER_REALM));

        /* Forms with no spells */
        if (!page)
        {
            c_msg_print("You don't know any monster spells.");
            return FALSE;
        }

        /* Pick a page */
        strnfmt(tmp, sizeof(tmp), "Select a page (1-%d, *=don't select): ", page);
        chosen_page = get_quantity(tmp, page + 1);

        /* Cast a spell directly by using spell flag */
        if (chosen_page == page + 1) textui_obj_cast(-1);

        /* Cast a spell by using page/spell number */
        else if (chosen_page) textui_obj_cast(chosen_page - 1);

        return FALSE;
    }

    /* Require spell ability */
    if (!p_ptr->clazz->spell_book)
    {
        c_msg_print("You cannot pray or produce magics.");
        return FALSE;
    }

    return TRUE;
}


/*** Using items the traditional way ***/


/* Basic tval testers */
bool obj_is_staff(struct player *p, const object_type *o_ptr)
{
    return (o_ptr->tval == TV_STAFF);
}


bool obj_is_wand(struct player *p, const object_type *o_ptr)
{
    return (o_ptr->tval == TV_WAND);
}

bool obj_is_rod(struct player *p, const object_type *o_ptr)
{
    return (o_ptr->tval == TV_ROD);
}


bool obj_is_food(struct player *p, const object_type *o_ptr)
{
    return is_food(o_ptr);
}


bool obj_is_potion(struct player *p, const object_type *o_ptr)
{
    return (o_ptr->tval == TV_POTION);
}


bool obj_is_scroll(struct player *p, const object_type *o_ptr)
{
    return (o_ptr->tval == TV_SCROLL);
}


/* Determine if an object is activatable */
bool obj_can_activate(struct player *p, const object_type *o_ptr)
{
    /* Check activation flag */
    return (o_ptr->info_xtra.act > 0);
}


/*
 * Hook to determine if an object can be fired
 */
bool item_tester_hook_fire(struct player *p, const object_type *o_ptr)
{
    object_type *j_ptr;

    /* Examine the "current bow" */
    j_ptr = &p->inventory[inven_bow];

    /* Handle current shooter */
    if (j_ptr->kind)
    {
        /* Take note of required "tval" for missiles */
        switch (j_ptr->sval)
        {
            case SV_SLING: return (o_ptr->tval == TV_SHOT);

            case SV_SHORT_BOW:
            case SV_LONG_BOW: return (o_ptr->tval == TV_ARROW);

            case SV_LIGHT_XBOW:
            case SV_HEAVY_XBOW: return (o_ptr->tval == TV_BOLT);
        }
    }

    /* No shooter */
    return (o_ptr->tval == TV_ROCK);
}


bool obj_is_useable(struct player *p, const object_type *o_ptr)
{
    if (item_tester_hook_fire(p, o_ptr)) return TRUE;
    if (obj_is_food(p, o_ptr)) return TRUE;
    if (obj_is_potion(p, o_ptr)) return TRUE;
    if (obj_is_scroll(p, o_ptr)) return TRUE;
    if (obj_is_staff(p, o_ptr)) return TRUE;
    if (obj_is_wand(p, o_ptr)) return TRUE;
    if (obj_is_rod(p, o_ptr)) return TRUE;
    if (obj_can_activate(p, o_ptr)) return TRUE;
    return FALSE;
}


/* Zap a rod */
int textui_cmd_zap_rod(int item)
{
    char dummy[NORMAL_WID];
    object_type *o_ptr = object_from_item_idx(item, dummy, sizeof(dummy));

    /* Needs a direction */
    if (o_ptr->info_xtra.act) return DIR_UNKNOWN;

    return DIR_SKIP;
}


/* Activate a wielded object */
int textui_cmd_activate(int item)
{
    char dummy[NORMAL_WID];
    object_type *o_ptr = object_from_item_idx(item, dummy, sizeof(dummy));

    /* Needs a direction */
    if (o_ptr->info_xtra.act == 2) return DIR_UNKNOWN;

    return DIR_SKIP;
}


/* Use an item */
int textui_cmd_use_any(int item)
{
    char dummy[NORMAL_WID];
    object_type *o_ptr = object_from_item_idx(item, dummy, sizeof(dummy));

    /* Check for direction */
    if (item_tester_hook_fire(p_ptr, o_ptr)) return DIR_UNKNOWN;
    if (obj_is_food(p_ptr, o_ptr)) return DIR_SKIP;
    if (obj_is_potion(p_ptr, o_ptr)) return DIR_SKIP;
    if (obj_is_scroll(p_ptr, o_ptr)) return DIR_SKIP;
    if (obj_is_staff(p_ptr, o_ptr)) return DIR_SKIP;
    if (obj_is_wand(p_ptr, o_ptr)) return DIR_UNKNOWN;
    if (obj_is_rod(p_ptr, o_ptr)) return textui_cmd_zap_rod(item);
    if (obj_can_activate(p_ptr, o_ptr)) return textui_cmd_activate(item);

    return DIR_SKIP;
}


/*** Refuelling ***/


/*
 * Check if an object can be used to refuel other objects.
 */
bool obj_can_refill(struct player *p, const object_type *o_ptr)
{
    const object_type *light = &p->inventory[inven_light];

    /* A lamp can be refueled from a flask or another lamp */
    if (is_lamp(light))
    {
        if (o_ptr->tval == TV_FLASK) return TRUE;

        /* Non-empty, non-everburning lamps are okay */
        if ((o_ptr->tval == TV_LIGHT) && is_lamp(o_ptr) && o_ptr->info_xtra.fuel) return TRUE;
    }

    return FALSE;
}


bool obj_refill_pre(void)
{
    object_type *o_ptr;

    /* Get the light */
    o_ptr = &p_ptr->inventory[inven_light];

    /* It is nothing */
    if (o_ptr->tval != TV_LIGHT)
    {
        c_msg_print("You are not wielding a light.");
        return FALSE;
    }

    /* No light to refill */
    if (!o_ptr->info_xtra.fuel)
    {
        c_msg_print("Your light cannot be refilled.");
        return FALSE;
    }

    return TRUE;
}
