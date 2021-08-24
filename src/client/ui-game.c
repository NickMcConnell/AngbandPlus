/*
 * File: ui-game.c
 * Purpose: Game management for the traditional text UI
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2015 Nick McConnell
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


/*
 * Here are lists of commands, stored in this format so that they can be
 * easily maniuplated for e.g. help displays, or if a port wants to provide a
 * native menu containing a command list.
 *
 * Consider a two-paned layout for the command menus. XXX
 */


/*
 * Magic use
 */
struct cmd_info cmd_magic[] =
{
    {"Use polymorphing ability", {'V'}, CMD_POLY, textui_cmd_poly, NULL},
    {"Use breath attack", {'y', 'f'}, CMD_BREATH, NULL, NULL}
};


/*
 * Item commands
 */
struct cmd_info cmd_item[] =
{
    {"Inscribe an object", {'{'}, CMD_INSCRIBE, NULL, NULL},
    {"Uninscribe an object", {'}'}, CMD_UNINSCRIBE, NULL, NULL},
    {"Wear/wield an item", {'w'}, CMD_WIELD, NULL, NULL},
    {"Take off/unwield an item", {'t', 'T'}, CMD_TAKEOFF, NULL, NULL},
    {"Examine an item", {'I'}, CMD_EXAMINE, NULL, NULL},
    {"Drop an item", {'d'}, CMD_DROP, NULL, NULL},
    {"Fire your missile weapon", {'f', 't'}, CMD_FIRE, NULL, NULL},
    {"Use a staff", {'u', 'Z'}, CMD_USE_STAFF, NULL, NULL},
    {"Aim a wand", {'a', 'z'}, CMD_USE_WAND, NULL, NULL},
    {"Zap a rod", {'z', 'a'}, CMD_USE_ROD, NULL, NULL},
    {"Activate an object", {'A'}, CMD_ACTIVATE, NULL, NULL},
    {"Eat some food", {'E'}, CMD_EAT, NULL, NULL},
    {"Quaff a potion", {'q'}, CMD_QUAFF, NULL, NULL},
    {"Read a scroll", {'r'}, CMD_READ_SCROLL, NULL, NULL},
    {"Fuel your light source", {'F'}, CMD_REFILL, NULL, obj_refill_pre},
    {"Use an item", {'U', 'X'}, CMD_USE, NULL, NULL}
};


/*
 * General actions
 */
struct cmd_info cmd_action[] =
{
    {"Disarm a trap or chest", {'D'}, CMD_DISARM, NULL, NULL},
    {"Rest for a while", {'R'}, CMD_NULL, textui_cmd_rest, NULL},
    {"Look around", {'l', 'x'}, CMD_NULL, do_cmd_look, NULL},
    {"Target monster or location", {'*'}, CMD_NULL, do_cmd_target, NULL},
    {"Target non hostile player", {'('}, CMD_NULL, do_cmd_target_friendly, NULL},
    {"Target closest monster", {'\''}, CMD_NULL, do_cmd_target_closest, NULL},
    {"Dig a tunnel", {'T', KTRL('T')}, CMD_TUNNEL, NULL, NULL},
    {"Go up staircase", {'<'}, CMD_GO_UP, NULL, NULL},
    {"Go down staircase", {'>'}, CMD_GO_DOWN, NULL, NULL},
    {"Toggle stealth mode", {'S', '#'}, CMD_TOGGLE_STEALTH, NULL, NULL},
    {"Open a door or a chest", {'o'}, CMD_OPEN, NULL, NULL},
    {"Close a door", {'c'}, CMD_CLOSE, NULL, NULL},
    {"Fire at nearest target", {'h', KC_TAB}, CMD_NULL, do_cmd_fire_at_nearest, NULL},
    {"Throw an item", {'v'}, CMD_THROW, NULL, NULL},
    {"Walk into a trap", {'W', '-'}, CMD_JUMP, NULL, NULL},
    {"Interact with a fountain", {'_'}, CMD_FOUNTAIN, do_cmd_fountain, NULL}
};


/*
 * Item management commands
 */
struct cmd_info cmd_item_manage[]  =
{
    {"Display equipment listing", {'e'}, CMD_NULL, do_cmd_equip, NULL},
    {"Display inventory listing", {'i'}, CMD_NULL, do_cmd_inven, NULL},
    {"Display quiver listing", {'|'}, CMD_NULL, do_cmd_quiver, NULL},
    {"Pick up objects", {'g'}, CMD_PICKUP, NULL, NULL},
    {"Ignore an item", {'k', KTRL('D')}, CMD_IGNORE, textui_cmd_ignore, NULL},
    {"Drop gold", {'$'}, CMD_DROP_GOLD, textui_cmd_drop_gold, NULL},
    {"Steal an item", {'J', 'S'}, CMD_STEAL, NULL, NULL}
};


/*
 * Information access commands
 */
struct cmd_info cmd_info[] =
{
    {"Browse a book", {'b', 'P'}, CMD_BROWSE_SPELL, NULL, obj_browse_pre},
    {"Gain new spells or prayers", {'G'}, CMD_STUDY, NULL, obj_study_pre},
    {"Cast a spell", {'m'}, CMD_CAST, NULL, obj_cast_pre},
    {"Project a spell", {'p'}, CMD_PROJECT, NULL, obj_cast_pre},
    {"Full dungeon map", {'M'}, CMD_NULL, do_cmd_view_map, NULL},
    {"Toggle ignoring of items", {'K', 'O'}, CMD_NULL, textui_cmd_toggle_ignore, NULL},
    {"Display visible item list", {']'}, CMD_NULL, do_cmd_itemlist, NULL},
    {"Display visible monster list", {'['}, CMD_NULL, do_cmd_monlist, NULL},
    {"Locate player on map", {'L', 'W'}, CMD_NULL, do_cmd_locate, NULL},
    {"Help", {'?'}, CMD_NULL, do_cmd_help, NULL},
    {"Identify symbol", {'/'}, CMD_NULL, do_cmd_query_symbol, NULL},
    {"Character description", {'C'}, CMD_NULL, do_cmd_change_name, NULL},
    {"Check knowledge", {'~'}, CMD_NULL, textui_browse_knowledge, NULL},
    {"Repeat level feeling", {KTRL('F')}, CMD_NULL, do_cmd_feeling, NULL},
    {"Show previous message", {KTRL('O')}, CMD_NULL, do_cmd_message_one, NULL},
    {"Show previous messages", {KTRL('P')}, CMD_NULL, do_cmd_messages, NULL},
    {"Show chat messages", {':'}, CMD_NULL, do_cmd_message, NULL},
    {"Chat commands", {KTRL('Z')}, CMD_NULL, do_cmd_chat, NULL},
    {"Access party menu", {'P', '!'}, CMD_NULL, do_cmd_party, NULL},
    {"Display connected players", {'@', KTRL('V')}, CMD_NULL, do_cmd_players, NULL},
    {"Describe an item in chat window", {KTRL('D'), KC_BACKSPACE}, CMD_NULL, do_cmd_describe, NULL},
    {"Full wilderness map", {KTRL('W')}, CMD_NULL, do_cmd_wild_map, NULL},
    {"Display current time", {'%'}, CMD_NULL, do_cmd_time, NULL}
};


/*
 * Utility/assorted commands
 */
struct cmd_info cmd_util[] =
{
    {"Interact with options", {'='}, CMD_NULL, do_cmd_xxx_options, NULL},
    {"Save and quit", {KTRL('X')}, CMD_NULL, textui_quit, NULL},
    {"Kill character and quit", {'Q'}, CMD_NULL, textui_cmd_suicide, NULL},
    {"Redraw the screen", {KTRL('R')}, CMD_NULL, do_cmd_redraw, NULL},
    {"Save \"screen dump\"", {')'}, CMD_NULL, do_cmd_save_screen, NULL},
    {"Purchase a house", {KTRL('E')}, CMD_NULL, do_cmd_purchase_house, NULL},
    {"Take a quest", {KTRL('Q')}, CMD_NULL, do_cmd_quest, NULL},
    {"Socials", {KTRL('S')}, CMD_NULL, do_cmd_social, NULL}
};


/*
 * Commands that shouldn't be shown to the user
 */
struct cmd_info cmd_hidden[] =
{
    {"Load a single pref line", {'"'}, CMD_NULL, do_cmd_pref, NULL},
    {"Alter a grid", {'+'}, CMD_ALTER, NULL, NULL},
    {"Walk", {';'}, CMD_WALK, NULL, NULL},
    {"Start running", {'.', ','}, CMD_RUN, NULL, NULL},
    {"Stand still", {',', '.'}, CMD_HOLD, NULL, NULL},
    {"Center map", {KTRL('L'), '@'}, CMD_NULL, do_cmd_center_map, NULL},
    {"Do autopickup", {KTRL('A')}, CMD_AUTOPICKUP, NULL, NULL},
    {"Access dungeon master menu", {'&'}, CMD_NULL, do_cmd_master, NULL}
};


/*
 * List of command lists
 */
struct command_list cmds_all[MAX_COMMAND_LIST] =
{
    {"Misc magic", cmd_magic, N_ELEMENTS(cmd_magic)},
    {"Items", cmd_item, N_ELEMENTS(cmd_item)},
    {"Action commands", cmd_action, N_ELEMENTS(cmd_action)},
    {"Manage items", cmd_item_manage, N_ELEMENTS(cmd_item_manage)},
    {"Information", cmd_info, N_ELEMENTS(cmd_info)},
    {"Utility", cmd_util, N_ELEMENTS(cmd_util)},
    {"Hidden", cmd_hidden, N_ELEMENTS(cmd_hidden)}
};


/*** Exported functions ***/


/* List indexed by char */
static struct cmd_info *converted_list[2][UCHAR_MAX + 1];


/*
 * Initialize the command list.
 */
void cmd_init(void)
{
    size_t i, j;

    memset(converted_list, 0, 2 * (UCHAR_MAX + 1) * sizeof(struct cmd_info *));

    /* Go through all generic commands */
    for (j = 0; j < MAX_COMMAND_LIST; j++)
    {
        struct cmd_info *commands = cmds_all[j].list;

        /* Fill everything in */
        for (i = 0; i < cmds_all[j].len; i++)
        {
            /* If a roguelike key isn't set, use default */
            if (!commands[i].key[1]) commands[i].key[1] = commands[i].key[0];

            converted_list[0][commands[i].key[0]] = &commands[i];
            converted_list[1][commands[i].key[1]] = &commands[i];
        }
    }
}


unsigned char cmd_lookup_key(cmd_code lookup_cmd, int mode)
{
    unsigned int i;

    assert((mode == KEYMAP_MODE_ROGUE) || (mode == KEYMAP_MODE_ORIG));

    for (i = 0; i < N_ELEMENTS(converted_list[mode]); i++)
    {
        struct cmd_info *cmd = converted_list[mode][i];

        if (cmd && (cmd->cmd == lookup_cmd)) return cmd->key[mode];
    }

    return 0;
}


unsigned char cmd_lookup_key_unktrl(cmd_code lookup_cmd, int mode)
{
    unsigned char c = cmd_lookup_key(lookup_cmd, mode);

    if (c < 0x20) c = UN_KTRL(c);

    return c;
}


cmd_code cmd_lookup(unsigned char key, int mode)
{
    assert((mode == KEYMAP_MODE_ROGUE) || (mode == KEYMAP_MODE_ORIG));

    if (!converted_list[mode][key]) return CMD_NULL;
    return converted_list[mode][key]->cmd;
}


/*
 * Process the current command
 */
static void textui_process_command_aux(ui_event e)
{
    bool done = true;
    struct cmd_info *cmd = NULL;
    unsigned char key = '\0';
    int mode = (OPT(player, rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);

    if (e.type == EVT_KBRD) done = textui_process_key(e.key, &key);

    /* Null command */
    if (!key && done)
    {
        if (first_escape) Send_clear();
        first_escape = false;
        return;
    }

    /* Use command menus */
    if (key == KC_ENTER)
        cmd = textui_action_menu_choose();

    /* Command key */
    else
        cmd = converted_list[mode][key];

    if (cmd && done)
    {
        /* Check command prereqs */
        if (!cmd->prereq || cmd->prereq())
        {
            /* UI command */
            if (cmd->hook)
                cmd->hook();

            /* Game command */
            else if (cmd->cmd)
                process_command(cmd->cmd);
        }
    }

    /* Error */
    else
        do_cmd_unknown();
}


/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
void textui_process_command()
{
    ui_event e;

    /* See if we have a command waiting */
    e = textui_get_command();

    /* Process any commands we got */
    while (e.type != EVT_NONE)
    {
        /* Process it */
        textui_process_command_aux(e);

        /* Ask for another command */
        e = textui_get_command();
    }
}