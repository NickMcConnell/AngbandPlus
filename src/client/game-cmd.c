/*
 * File: game-cmd.c
 * Purpose: Handles the queueing of game commands
 *
 * Copyright (c) 2008-9 Antony Sidwell
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


/*
 * Command handlers will take a pointer to the command structure
 * so that they can access any arguments supplied.
 */
typedef int (*cmd_handler_fn)(cmd_arg args[]);


/* A simple list of commands and their handling functions. */
struct command_info
{
    cmd_code cmd;
    const char *verb;
    enum cmd_arg_type arg_type[CMD_MAX_ARGS];
    cmd_handler_fn fn;
};


static const struct command_info game_cmds[] =
{
    {CMD_GO_UP, NULL, {arg_NONE}, Send_go_up},
    {CMD_GO_DOWN, NULL, {arg_NONE}, Send_go_down},
    {CMD_SEARCH, NULL, {arg_NONE}, Send_search},
    {CMD_TOGGLE_SEARCH, NULL, {arg_NONE}, Send_toggle_search},
    {CMD_WALK, NULL, {arg_DIRECTION, arg_NONE}, Send_walk},
    {CMD_JUMP, NULL, {arg_DIRECTION, arg_NONE}, Send_jump},
    {CMD_INSCRIBE, "inscribe", {arg_ITEM, arg_STRING}, Send_inscribe},
    {CMD_UNINSCRIBE, "un-inscribe", {arg_ITEM, arg_NONE}, Send_uninscribe},
    {CMD_TAKEOFF, "take off", {arg_ITEM, arg_NONE}, Send_take_off},
    {CMD_WIELD, "wear or wield", {arg_ITEM, arg_NUMBER}, Send_wield},
    {CMD_DROP, "drop", {arg_ITEM, arg_NUMBER}, Send_drop},
    {CMD_BROWSE_SPELL, "browse", {arg_ITEM, arg_NONE}, NULL},
    {CMD_STUDY_BOOK, "study", {arg_ITEM, arg_NUMBER}, Send_gain},
    {CMD_CAST, "use", {arg_ITEM, arg_NONE}, NULL},
    {CMD_USE_STAFF, "use", {arg_ITEM, arg_NONE}, Send_use},
    {CMD_USE_WAND, "aim", {arg_ITEM, arg_DIRECTION}, Send_aim},
    {CMD_USE_ROD, "zap", {arg_ITEM, arg_DIRECTION}, Send_zap},
    {CMD_ACTIVATE, "activate", {arg_ITEM, arg_DIRECTION}, Send_activate},
    {CMD_EAT, "eat", {arg_ITEM, arg_NONE}, Send_eat},
    {CMD_QUAFF, "quaff", {arg_ITEM, arg_NONE}, Send_quaff},
    {CMD_READ_SCROLL, "read", {arg_ITEM, arg_NONE}, Send_read},
    {CMD_REFILL, "refuel with", {arg_ITEM, arg_NONE}, Send_fill},
    {CMD_FIRE, "fire", {arg_ITEM, arg_DIRECTION}, Send_fire},
    {CMD_PICKUP, NULL, {arg_NUMBER, arg_NONE}, Send_pickup},
    {CMD_AUTOPICKUP, NULL, {arg_NUMBER, arg_NONE}, Send_pickup},
    {CMD_DISARM, NULL, {arg_DIRECTION, arg_NONE}, Send_disarm},
    {CMD_TUNNEL, NULL, {arg_DIRECTION, arg_NONE}, Send_tunnel},
    {CMD_OPEN, NULL, {arg_DIRECTION, arg_NONE}, Send_open},
    {CMD_CLOSE, NULL, {arg_DIRECTION, arg_NONE}, Send_close},
    {CMD_JAM, NULL, {arg_DIRECTION, arg_NONE}, Send_spike},
    {CMD_BASH, NULL, {arg_DIRECTION, arg_NONE}, Send_bash},
    {CMD_HOLD, NULL, {arg_NUMBER, arg_NONE}, Send_pickup},
    {CMD_RUN, NULL, {arg_DIRECTION, arg_NONE}, Send_run},
    {CMD_ALTER, NULL, {arg_DIRECTION, arg_NONE}, Send_alter},
    {CMD_USE_ANY, "use", {arg_ITEM, arg_DIRECTION}, Send_use_any},
    {CMD_BREATH, NULL, {arg_DIRECTION, arg_NONE}, Send_breath},
    {CMD_PROJECT, "use", {arg_ITEM, arg_NONE}, NULL},
    {CMD_STEAL, NULL, {arg_DIRECTION, arg_NONE}, Send_steal},
    {CMD_EXAMINE, "examine", {arg_ITEM, arg_NONE}, Send_observe}
};


/* Item selector type (everything required for get_item()) */
struct item_selector
{
    cmd_code command;
    const char *type;
    bool (*filter)(struct player *p, const object_type *o_ptr);
    int mode;
};


/* List of requirements for various commands' objects */
static struct item_selector item_selector[] =
{
    {CMD_INSCRIBE, NULL, NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR)},
    {CMD_UNINSCRIBE, NULL, NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR)},
    {CMD_TAKEOFF, NULL, NULL, USE_EQUIP},
    {CMD_WIELD, NULL, obj_can_wear, (USE_INVEN | USE_FLOOR)},
    {CMD_DROP, NULL, NULL, (USE_EQUIP | USE_INVEN)},
    {CMD_BROWSE_SPELL, "book", obj_can_browse, (USE_INVEN)},
    {CMD_STUDY_BOOK, "book", obj_can_study, (USE_INVEN)},
    {CMD_CAST, "book", obj_can_cast_from, (USE_INVEN)},
    {CMD_USE_STAFF, "staff", obj_is_staff, (USE_INVEN | USE_FLOOR | SHOW_FAIL)},
    {CMD_USE_WAND, "wand", obj_is_wand, (USE_INVEN | USE_FLOOR | SHOW_FAIL)},
    {CMD_USE_ROD, "rod", obj_is_rod, (USE_INVEN | USE_FLOOR | SHOW_FAIL)},
    {CMD_ACTIVATE, NULL, obj_can_activate, (USE_EQUIP | SHOW_FAIL)},
    {CMD_EAT, NULL, obj_is_food, (USE_INVEN | USE_FLOOR)},
    {CMD_QUAFF, "potion", obj_is_potion, (USE_INVEN | USE_FLOOR)},
    {CMD_READ_SCROLL, "scroll", obj_is_scroll, (USE_INVEN | USE_FLOOR)},
    {CMD_REFILL, "fuel source", obj_can_refill, (USE_INVEN | USE_FLOOR)},
    {CMD_FIRE, NULL, item_tester_hook_fire, (USE_INVEN | USE_EQUIP | USE_FLOOR | QUIVER_TAGS)},
    {CMD_USE_ANY, NULL, obj_is_useable, (USE_EQUIP | USE_INVEN | USE_FLOOR | SHOW_FAIL | QUIVER_TAGS)},
    {CMD_PROJECT, "book", obj_can_cast_from, (USE_INVEN)},
    {CMD_EXAMINE, NULL, NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR)}
};


/* Return the index of the given command in the command array. */
static int cmd_idx(cmd_code code)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(game_cmds); i++)
    {
        if (game_cmds[i].cmd == code) return i;
    }

    return -1;
}


/* Return the index of the given item selector in the item selector array. */
static int selector_idx(cmd_code code)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(item_selector); i++)
    {
        if (item_selector[i].command == code) return i;
    }

    return -1;
}


/*
 * Request a game command from the UI and carry out whatever actions
 * go along with it.
 */
void process_command(cmd_code cmd)
{
    cmd_arg arg[CMD_MAX_ARGS];
    int dir = DIR_UNKNOWN;
    size_t i;
    int sidx;
    int number = 0;
    char inscript[60], buf[60];

    /* If we've got a command to process, do it. */
    int idx = cmd_idx(cmd);

    if (idx == -1) return;

    /* Preprocess the command */
    switch (cmd)
    {
        case CMD_INSCRIBE:
        {
            my_strcpy(inscript, "Inscription: ", sizeof(inscript));
            break;
        }

        case CMD_PICKUP:
        {
            number = 1;
            break;
        }

        case CMD_AUTOPICKUP:
        {
            number = 2;
            break;
        }

        case CMD_DISARM:
        case CMD_OPEN:
        case CMD_CLOSE:
        {
            if (OPT(easy_open)) dir = DIR_SKIP;
            break;
        }
    }

    sidx = selector_idx(cmd);

    /* Build the arguments */
    for (i = 0; i < CMD_MAX_ARGS; i++)
    {
        enum cmd_arg_type arg_type = game_cmds[idx].arg_type[i];

        if (arg_type == arg_NONE) break;

        switch (arg_type)
        {
            case arg_STRING:
            {
                buf[0] = '\0';
                if (!get_string(inscript, buf, sizeof(buf))) return;

                arg[i].string = string_make(buf);
                break;
            }

            case arg_NUMBER:
            {
                arg[i].number = number;
                break;
            }

            case arg_ITEM:
            {
                struct item_selector *is;
                int item;
                const char *verb = game_cmds[idx].verb;
                const char *type;
                char prompt[60], none[60];

                if (sidx == -1) return;

                is = &item_selector[sidx];
                type = is->type;

                if (!type)
                {
                    strnfmt(prompt, sizeof(prompt), "%s which item? ", verb);
                    my_strcap(prompt);
                    strnfmt(none, sizeof(none), "You have nothing you can %s.", verb);
                }
                else
                {
                    strnfmt(prompt, sizeof(prompt), "%s which %s? ", verb, type);
                    my_strcap(prompt);
                    strnfmt(none, sizeof(none), "You have no %s you can %s.", type, verb);
                }

                item_tester_hook = is->filter;
                if (!get_item(&item, prompt, none, cmd, is->mode)) return;

                arg[i].item = item;

                /* Postprocess the command */
                switch (cmd)
                {
                    case CMD_WIELD:
                    {
                        number = textui_obj_wield(item);
                        if (number == -2) return;
                        break;
                    }

                    case CMD_DROP:
                    {
                        number = textui_cmd_drop(item);
                        if (!number) return;
                        break;
                    }

                    case CMD_BROWSE_SPELL:
                    {
                        textui_spell_browse(item);
                        break;
                    }

                    case CMD_STUDY_BOOK:
                    {
                        number = textui_obj_study(item);
                        if (number == -1) return;
                        break;
                    }

                    case CMD_CAST:
                    {
                        textui_obj_cast(item);
                        break;
                    }

                    case CMD_USE_ROD:
                    {
                        dir = textui_cmd_zap_rod(item);
                        break;
                    }

                    case CMD_ACTIVATE:
                    {
                        dir = textui_cmd_activate(item);
                        break;
                    }

                    case CMD_USE_ANY:
                    {
                        dir = textui_cmd_use_any(item);
                        break;
                    }

                    case CMD_PROJECT:
                    {
                        textui_obj_project(item);
                        break;
                    }
                }

                break;
            }

            case arg_DIRECTION:
            {
                if (dir == DIR_SKIP) dir = DIR_UNKNOWN;
                else if (!get_aim_dir(&dir)) return;

                arg[i].direction = dir;
                break;
            }
        }
    }

    /* Process the command */
    if (game_cmds[idx].fn) game_cmds[idx].fn(arg);
}
