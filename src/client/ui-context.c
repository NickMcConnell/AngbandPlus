/*
 * File: ui-context.c
 * Purpose: Show player and terrain context menus
 *
 * Copyright (c) 2011 Brett Reid
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


#include "c-angband.h"


#define ADD_LABEL(text, cmd, valid) \
    cmdkey = cmd_lookup_key_unktrl((cmd), mode); \
    menu_dynamic_add_label_valid(m, (text), cmdkey, (cmd), labels, (valid))


/*
 * Additional constants for menu item values in context_menu_object(). The values must not collide
 * with the cmd_code enum, since those are the main values for these menu items.
 */
enum context_menu_object_value_e
{
    MENU_VALUE_INSPECT = CMD_MAX,
    MENU_VALUE_DROP_ALL
};


static int cmd_execute(int selected, struct object *obj)
{
    struct command cmd;

    memset(&cmd, 0, sizeof(cmd));
    cmd.code = (cmd_code)selected;

    switch (selected)
    {
        /* Wield the item */
        case CMD_WIELD:
        {
            cmd_set_arg_item(&cmd, "item", obj);
            return Send_wield(&cmd);
        }

        /* Take the item off */
        case CMD_TAKEOFF:
        {
            cmd_set_arg_item(&cmd, "item", obj);
            return Send_take_off(&cmd);
        }

        /* Inscribe the item */
        case CMD_INSCRIBE:
        {
            cmd_set_arg_item(&cmd, "item", obj);
            return Send_inscribe(&cmd);
        }

        /* Uninscribe the item */
        case CMD_UNINSCRIBE:
        {
            cmd_set_arg_item(&cmd, "item", obj);
            return Send_uninscribe(&cmd);
        }

        /* Pick the item up */
        case CMD_PICKUP:
        {
            cmd_set_arg_number(&cmd, "number", 1);
            cmd_set_arg_item(&cmd, "item", obj);
            Send_pickup(&cmd);
            break;
        }

        /* Drop the item */
        case CMD_DROP:
        {
            cmd_set_arg_item(&cmd, "item", obj);
            return Send_drop(&cmd);
        }

        /* Use the item to refill a light source */
        case CMD_REFILL:
        {
            if (!obj_refill_pre()) return 0;
            cmd_set_arg_item(&cmd, "item", obj);
            return Send_fill(&cmd);
        }

        /* Throw the item */
        case CMD_THROW:
        {
            cmd_set_arg_item(&cmd, "item", obj);
            return Send_throw(&cmd);
        }

        /* Use the item */
        case CMD_USE_WAND:
        case CMD_USE_ROD:
        case CMD_USE_STAFF:
        case CMD_READ_SCROLL:
        case CMD_QUAFF:
        case CMD_EAT:
        case CMD_ACTIVATE:
        case CMD_FIRE:
        case CMD_USE:
        {
            cmd_set_arg_item(&cmd, "item", obj);
            return Send_use_any(&cmd);
        }
    }

    return 1;
}


static bool object_is_carried(const struct object *obj)
{
    int i, size = z_info->pack_size + player->body.count + z_info->quiver_size;

    for (i = 0; i < size; i++)
    {
        struct object *pile_obj = &player->gear[i];

        if (obj == pile_obj) return true;
    }

    return false;
}


/*
 * Pick the context menu options appropriate for the item
 */
int context_menu_object(struct object *obj)
{
    struct menu *m;
    int selected;
    char *labels;
    char header[120];
    bool allowed = true;
    int mode = (OPT(player, rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    unsigned char cmdkey;

    m = menu_dynamic_new();
    if (!m || !obj) return 0;

    strnfmt(header, sizeof(header), "(Enter to select, ESC) Command for %s:", obj->info_xtra.name);

    labels = string_make(lower_case);
    m->selections = labels;

    /* 'I' is used for inspect in both keymaps. */
    menu_dynamic_add_label(m, "Inspect", 'I', MENU_VALUE_INSPECT, labels);

    if (obj_can_browse(player, obj))
    {
        if (obj_can_cast_from(player, obj))
        {
            ADD_LABEL("Cast", CMD_CAST, MN_ROW_VALID);
        }
        if (obj_can_study(player, obj))
        {
            ADD_LABEL("Study", CMD_STUDY, MN_ROW_VALID);
        }
        ADD_LABEL("Browse", CMD_BROWSE_SPELL, MN_ROW_VALID);
    }
    else if (obj_is_useable(player, obj))
    {
        if (tval_is_wand(obj))
        {
            menu_row_validity_t valid = (obj_has_charges(obj)? MN_ROW_VALID: MN_ROW_INVALID);

            ADD_LABEL("Aim", CMD_USE_WAND, valid);
        }
        else if (tval_is_rod(obj))
        {
            menu_row_validity_t valid = (obj_can_zap(obj)? MN_ROW_VALID: MN_ROW_INVALID);

            ADD_LABEL("Zap", CMD_USE_ROD, valid);
        }
        else if (tval_is_staff(obj))
        {
            menu_row_validity_t valid = (obj_has_charges(obj)? MN_ROW_VALID: MN_ROW_INVALID);

            ADD_LABEL("Use", CMD_USE_STAFF, valid);
        }
        else if (tval_is_scroll(obj))
        {
            ADD_LABEL("Read", CMD_READ_SCROLL, MN_ROW_VALID);
        }
        else if (tval_is_potion(obj))
        {
            ADD_LABEL("Quaff", CMD_QUAFF, MN_ROW_VALID);
        }
        else if (tval_is_edible(obj))
        {
            ADD_LABEL("Eat", CMD_EAT, MN_ROW_VALID);
        }
        else if (obj_is_activatable(player, obj))
        {
            menu_row_validity_t valid = ((object_is_equipped(player->body, obj) &&
                obj_can_activate(obj))? MN_ROW_VALID: MN_ROW_INVALID);

            ADD_LABEL("Activate", CMD_ACTIVATE, valid);
        }
        else if (item_tester_hook_fire(player, obj))
        {
            ADD_LABEL("Fire", CMD_FIRE, MN_ROW_VALID);
        }
        else
        {
            ADD_LABEL("Use", CMD_USE, MN_ROW_VALID);
        }
    }
    if (obj_can_refill(player, obj))
    {
        ADD_LABEL("Refill", CMD_REFILL, MN_ROW_VALID);
    }
    if (object_is_equipped(player->body, obj) && obj_can_takeoff(player, obj))
    {
        ADD_LABEL("Take off", CMD_TAKEOFF, MN_ROW_VALID);
    }
    else if (!object_is_equipped(player->body, obj) && obj_can_wear(player, obj))
    {
        ADD_LABEL("Equip", CMD_WIELD, MN_ROW_VALID);
    }
    if (object_is_carried(obj))
    {
        ADD_LABEL("Drop", CMD_DROP, MN_ROW_VALID);
        if (obj->number > 1)
        {
            /* 'D' is used for ignore in rogue keymap, so we'll just swap letters. */
            cmdkey = ((mode == KEYMAP_MODE_ORIG)? 'D': 'k');
            menu_dynamic_add_label(m, "Drop All", cmdkey, MENU_VALUE_DROP_ALL, labels);
        }
    }
    else
    {
        menu_row_validity_t valid = (inven_carry_okay(obj)? MN_ROW_VALID: MN_ROW_INVALID);

        ADD_LABEL("Pick up", CMD_PICKUP, valid);
    }
    ADD_LABEL("Throw", CMD_THROW, MN_ROW_VALID);
    ADD_LABEL("Inscribe", CMD_INSCRIBE, MN_ROW_VALID);
    if (obj_has_inscrip(player, obj))
    {
        ADD_LABEL("Uninscribe", CMD_UNINSCRIBE, MN_ROW_VALID);
    }
    ADD_LABEL((obj->info_xtra.ignored? "Unignore": "Ignore"), CMD_IGNORE, MN_ROW_VALID);

    menu_dynamic_calc_location(m);

    prt(header, 0, 0);
    selected = menu_dynamic_select(m);

    menu_dynamic_free(m);
    string_free(labels);

    screen_load(false);

    switch (selected)
    {
        case -1:
            /* User cancelled the menu. */
            return 3;

        case MENU_VALUE_INSPECT:
        {
            struct command cmd;

            memset(&cmd, 0, sizeof(cmd));
            cmd.code = CMD_EXAMINE;

            /* Inspect the item */
            cmd_set_arg_item(&cmd, "item", obj);
            Send_observe(&cmd);
            return 1;
        }

        case MENU_VALUE_DROP_ALL:
        {
            struct command cmd;

            memset(&cmd, 0, sizeof(cmd));
            cmd.code = CMD_DROP;

            /* Drop entire stack with confirmation. */
            cmd_set_arg_item(&cmd, "item", obj);
            cmd_set_arg_number(&cmd, "quantity", obj->number);
            Send_drop(&cmd);
            return 1;
        }

        case CMD_BROWSE_SPELL:
        case CMD_STUDY:
        case CMD_CAST:
        case CMD_IGNORE:
        case CMD_WIELD:
        case CMD_TAKEOFF:
        case CMD_INSCRIBE:
        case CMD_UNINSCRIBE:
        case CMD_PICKUP:
        case CMD_DROP:
        case CMD_REFILL:
        case CMD_THROW:
        case CMD_USE_WAND:
        case CMD_USE_ROD:
        case CMD_USE_STAFF:
        case CMD_READ_SCROLL:
        case CMD_QUAFF:
        case CMD_EAT:
        case CMD_ACTIVATE:
        case CMD_FIRE:
        case CMD_USE:
            /* PWMAngband: inscriptions are checked on the server */
            break;
        default:
        {
            /* Invalid command; prevent anything from happening. */
            bell("Invalid context menu command.");
            allowed = false;
            break;
        }
    }

    if (!allowed) return 1;

    if (selected == CMD_IGNORE)
    {
        /* Ignore or unignore the item */
        textui_cmd_ignore_menu(obj);
    }
    else if (selected == CMD_BROWSE_SPELL)
    {
        /* Browse a spellbook */
        if (!obj_browse_pre()) return 0;
        Send_track_object(obj->oidx);
        textui_book_browse(obj->info_xtra.bidx);
        return 2;
    }
    else if (selected == CMD_STUDY)
    {
        struct command cmd;

        memset(&cmd, 0, sizeof(cmd));
        cmd.code = CMD_STUDY;

        /* Study a spellbook */
        if (!obj_study_pre()) return 0;
        cmd_set_arg_item(&cmd, "item", obj);
        return Send_gain(&cmd);
    }
    else if (selected == CMD_CAST)
    {
        /* Cast a spell from the book */
        if (obj_can_browse(player, obj))
        {
            struct command cmd;

            memset(&cmd, 0, sizeof(cmd));
            cmd.code = CMD_CAST;

            if (!obj_cast_pre()) return 0;
            cmd_set_arg_item(&cmd, "item", obj);
            cmd_cast(&cmd);
        }
    }
    else
    {
        /* PWMAngband: replaces cmd_insert + cmd_set_arg_item */
        return cmd_execute(selected, obj);
    }

    return 1;
}


/*
 * Menu functions
 */


/*
 * Display an entry on a command menu
 */
static void cmd_sub_entry(struct menu *menu, int oid, bool cursor, int row, int col, int width)
{
    byte attr = (cursor? COLOUR_L_BLUE: COLOUR_WHITE);
    const struct cmd_info *commands = menu_priv(menu);
    int mode = (OPT(player, rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    struct keypress kp;
    char buf[16];

    kp.type = EVT_KBRD;
    kp.code = commands[oid].key[mode];
    kp.mods = 0;

    /* Write the description */
    Term_putstr(col, row, -1, attr, commands[oid].desc);

    /* Include keypress */
    Term_addch(attr, ' ');
    Term_addch(attr, '(');

    /* Get readable version */
    keypress_to_readable(buf, sizeof(buf), kp);
    Term_addstr(-1, attr, buf);

    Term_addch(attr, ')');
}


/*
 * Display a list of commands.
 */
static bool cmd_menu(struct command_list *list, void *selection_p)
{
    struct menu menu;
    menu_iter commands_menu = {NULL, NULL, cmd_sub_entry, NULL, NULL};
    region area = {23, 4, 37, 13};
    ui_event evt;
    struct cmd_info **selection = selection_p;
    ui_event ea = EVENT_ABORT;

    /* Set up the menu */
    menu_init(&menu, MN_SKIN_SCROLL, &commands_menu);
    menu_setpriv(&menu, list->len, list->list);
    menu_layout(&menu, &area);

    /* Set up the screen */
    screen_save();
    window_make(21, 3, 62, 17);

    /* Select an entry */
    evt = menu_select(&menu, 0, true);

    /* Load the screen */
    screen_load(false);

    if (evt.type == EVT_SELECT) *selection = &list->list[menu.cursor];
    else if (is_abort(evt)) Term_event_push(&ea);

    return false;
}


static bool cmd_list_action(struct menu *m, const ui_event *event, int oid)
{
    if (event->type == EVT_SELECT)
        return cmd_menu(&cmds_all[oid], menu_priv(m));
    return false;
}


static void cmd_list_entry(struct menu *menu, int oid, bool cursor, int row,
    int col, int width)
{
    byte attr = (cursor? COLOUR_L_BLUE: COLOUR_WHITE);

    Term_putstr(col, row, -1, attr, cmds_all[oid].name);
}


static struct menu *command_menu;


static menu_iter command_menu_iter =
{
    NULL,
    NULL,
    cmd_list_entry,
    cmd_list_action,
    NULL
};


/*
 * Display a list of command types, allowing the user to select one.
 */
struct cmd_info *textui_action_menu_choose(void)
{
    region area = {21, 5, 37, 6};
    struct cmd_info *chosen_command = NULL;

    if (!command_menu) command_menu = menu_new(MN_SKIN_SCROLL, &command_menu_iter);

    menu_setpriv(command_menu, MAX_COMMAND_LIST - 1, &chosen_command);
    menu_layout(command_menu, &area);

    /* Set up the screen */
    screen_save();
    window_make(19, 4, 58, 11);

    /* Select an entry */
    menu_select(command_menu, 0, true);

    /* Load the screen */
    screen_load(true);

    return chosen_command;
}


void free_command_menu(void)
{
    mem_free(command_menu);
}
