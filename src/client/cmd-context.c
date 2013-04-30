/*
 * File: cmd-context.c
 * Purpose: Show player and terrain context menus
 *
 * Copyright (c) 2011 Brett Reid
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
#include "c-cmds.h"
#include "netclient.h"
#include "ui-menu.h"


/*
 * Pick the context menu options appropriate for the item
 */
int context_menu_object(const object_type *o_ptr, const int slot, const char *desc)
{
    menu_type *m;
    region r;
    int selected;
    char *labels;
    char header[160];
    cmd_arg arg[CMD_MAX_ARGS];

    m = menu_dynamic_new();
    if (!m || !o_ptr) return 0;

    strnfmt(header, sizeof(header), "(Enter to select, ESC) Command for %s:", desc);

    labels = string_make(lower_case);
    m->selections = labels;

    menu_dynamic_add_label(m, "Inspect", 'I', 1, labels);
    if ((slot < inven_wield) && obj_can_wear(p_ptr, o_ptr))
        menu_dynamic_add_label(m, "Equip", 'w', 2, labels);
    if (slot >= inven_wield) menu_dynamic_add_label(m, "Take off", 't', 3, labels);
    menu_dynamic_add_label(m, "Inscribe", '{', 4, labels);
    menu_dynamic_add_label(m, "Uninscribe", '}', 5, labels);
    if (slot >= 0)
    {
        menu_dynamic_add_label(m, "Drop", 'd', 6, labels);
        if (o_ptr->number > 1) menu_dynamic_add_label(m, "Drop All", 'D', 13, labels);
    }
    else menu_dynamic_add_label(m, "Pickup", 'g', 7, labels);
    if (obj_can_browse(p_ptr, o_ptr))
    {
        if (obj_can_cast_from(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Cast", 'm', 8, labels);
        menu_dynamic_add_label(m, "Browse", 'b', 9, labels);
        if (obj_can_study(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Study", 'G', 10, labels);
    }
    else if (obj_is_useable(p_ptr, o_ptr))
    {
        if (obj_is_wand(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Aim", 'a', 8, labels);
        if (obj_is_rod(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Zap", 'z', 8, labels);
        if (obj_is_staff(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Use", 'u', 8, labels);
        if (obj_is_scroll(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Read", 'r', 8, labels);
        if (obj_is_potion(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Quaff", 'q', 8, labels);
        if (obj_is_food(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Eat", 'E', 8, labels);
        if (obj_can_activate(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Activate", 'A', 8, labels);
        if (item_tester_hook_fire(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Fire", 'f', 8, labels);
    }
    if (obj_can_refill(p_ptr, o_ptr)) menu_dynamic_add_label(m, "Refill", 'F', 11, labels);
    menu_dynamic_add_label(m, "Throw", 'v', 12, labels);
    menu_dynamic_add_label(m, "Ignore", 'k', 14, labels);

    /* Work out display region */
    r.width = menu_dynamic_longest_entry(m) + 3 + 2;
    if (full_icky_screen && (r.width < NORMAL_WID - COL_MAP - 1))
        r.width = NORMAL_WID - COL_MAP - 1;
    r.col = NORMAL_WID - r.width;
    r.row = 1;
    r.page_rows = m->count;
    if (full_icky_screen && (r.row + r.page_rows < NORMAL_HGT)) r.page_rows = NORMAL_HGT - r.row;

    screen_save();
    menu_layout(m, &r);
    region_erase_bordered(&r);

    prt(header, 0, 0);
    selected = menu_dynamic_select(m);

    menu_dynamic_free(m);
    string_free(labels);

    screen_load(FALSE);
    switch (selected)
    {
        case 1:
        {
            /* Inspect the item */
            arg[0].item = slot;
            Send_observe(arg);
            break;
        }
        case 2:
        {
            /* Wield the item */
            arg[0].item = slot;
            arg[1].number = textui_obj_wield(arg[0].item);
            if (arg[1].number == -2) return 0;
            Send_wield(arg);
            break;
        }
        case 3:
        {
            /* Take the item off */
            arg[0].item = slot;
            Send_take_off(arg);
            break;
        }
        case 4:
        {
            char buf[60];

            /* Inscribe the item */
            arg[0].item = slot;
            buf[0] = '\0';
            if (!get_string("Inscription: ", buf, sizeof(buf))) return 0;
            arg[1].string = string_make(buf);
            Send_inscribe(arg);
            break;
        }
        case 5:
        {
            /* Uninscribe the item */
            arg[0].item = slot;
            Send_uninscribe(arg);
            break;
        }
        case 6:
        {
            /* Drop the item */
            arg[0].item = slot;
            arg[1].number = textui_cmd_drop(arg[0].item);
            if (!arg[1].number) return 0;
            Send_drop(arg);
            break;
        }
        case 7:
        {
            /* Pick the item up */
            arg[0].number = 1;
            Send_pickup(arg);
            break;
        }
        case 8:
        {
            /* Cast a spell from the book */
            if (obj_can_browse(p_ptr, o_ptr))
            {
                if (!obj_cast_pre()) return 0;
                textui_obj_cast(slot);
            }

            /* Use the item */
            else
            {
                int dir;

                arg[0].item = slot;
                dir = textui_cmd_use_any(arg[0].item);
                if (dir == DIR_SKIP) dir = DIR_UNKNOWN;
                else if (!get_aim_dir(&dir)) return 0;
                arg[1].direction = dir;
                Send_use_any(arg);
            }

            break;
        }
        case 9:
        {
            /* Browse a spellbook */
            if (!obj_browse_pre()) return 0;
            textui_spell_browse(slot);
            return 2;
        }
        case 10:
        {
            /* Study a spellbook */
            if (!obj_study_pre()) return 0;
            arg[0].item = slot;
            arg[1].number = textui_obj_study(arg[0].item);
            if (arg[1].number == -1) return 0;
            Send_gain(arg);
        }
        case 11:
        {
            /* Use the item to refill a light source */
            if (!obj_refill_pre()) return 0;
            arg[0].item = slot;
            Send_fill(arg);
            break;
        }
        case 12:
        {
            int dir;

            /* Throw the item */
            if ((slot >= inven_wield) && (slot < quiver_start))
            {
                c_msg_print("You cannot throw wielded items.");
                return 0;
            }
            if (!get_aim_dir(&dir)) return 0;
            Send_throw(slot, dir);
            break;
        }
        case 13:
        {
            /* Drop all of the item stack */
            arg[0].item = slot;
            arg[1].number = o_ptr->number;
            Send_drop(arg);
            break;
        }
        case 14:
        {
            /* Squelch or unsquelch the item */
            textui_cmd_destroy_menu(slot);
            break;
        }
        case -1:
        {
            /* This menu was canceled, tell whatever called us to display its menu again */
            return 3;
        }
    }

    return 1;
}
