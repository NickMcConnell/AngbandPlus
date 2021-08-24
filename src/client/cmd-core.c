/*
 * File: cmd-core.c
 * Purpose: Handles the queueing of game commands
 *
 * Copyright (c) 2008-9 Antony Sidwell
 * Copyright (c) 2014 Andi Sidwell
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
 * Command handlers will take a pointer to the command structure
 * so that they can access any arguments supplied.
 */
typedef int (*cmd_handler_fn)(struct command *cmd);


/*
 * A simple list of commands and their handling functions.
 */
struct command_info
{
    cmd_code cmd;
    const char *verb;
    cmd_handler_fn fn;
};


static const struct command_info game_cmds[] =
{
    {CMD_GO_UP, "go up stairs", Send_go_up},
    {CMD_GO_DOWN, "go down stairs", Send_go_down},
    {CMD_TOGGLE_STEALTH, "toggle stealth mode", Send_toggle_stealth},
    {CMD_WALK, "walk", Send_walk},
    {CMD_JUMP, "jump", Send_jump},
    {CMD_INSCRIBE, "inscribe", Send_inscribe},
    {CMD_UNINSCRIBE, "un-inscribe", Send_uninscribe},
    {CMD_TAKEOFF, "take off", Send_take_off},
    {CMD_WIELD, "wear or wield", Send_wield},
    {CMD_DROP, "drop", Send_drop},
    {CMD_BROWSE_SPELL, "browse", textui_spell_browse},
    {CMD_STUDY, "study", Send_gain},
    {CMD_CAST, "cast", cmd_cast},
    {CMD_USE_STAFF, "use", Send_use},
    {CMD_USE_WAND, "aim", Send_aim},
    {CMD_USE_ROD, "zap", Send_zap},
    {CMD_ACTIVATE, "activate", Send_activate},
    {CMD_EAT, "eat", Send_eat},
    {CMD_QUAFF, "quaff", Send_quaff},
    {CMD_READ_SCROLL, "read", Send_read},
    {CMD_REFILL, "refuel with", Send_fill},
    {CMD_USE, "use", Send_use_any},
    {CMD_FIRE, "fire", Send_fire},
    {CMD_THROW, "throw", Send_throw},
    {CMD_PICKUP, "pickup", Send_pickup},
    {CMD_AUTOPICKUP, "autopickup", Send_autopickup},
    {CMD_DISARM, "disarm", Send_disarm},
    {CMD_TUNNEL, "tunnel", Send_tunnel},
    {CMD_OPEN, "open", Send_open},
    {CMD_CLOSE, "close", Send_close},
    {CMD_HOLD, "stay still", Send_hold},
    {CMD_RUN, "run", Send_run},
    {CMD_ALTER, "alter", Send_alter},
    {CMD_BREATH, "breathe", Send_breath},
    {CMD_PROJECT, "project", cmd_project},
    {CMD_STEAL, "steal", Send_steal},
    {CMD_EXAMINE, "examine", Send_observe}
};


/*
 * Return the index of the given command in the command array.
 */
static int cmd_idx(cmd_code code)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(game_cmds); i++)
    {
        if (game_cmds[i].cmd == code) return i;
    }

    return CMD_ARG_NOT_PRESENT;
}


/*
 * Process a game command from the UI or the command queue and carry out
 * whatever actions go along with it.
 */
void process_command(cmd_code ctx)
{
    struct command cmd;

    /* If we've got a command to process, do it. */
    int idx = cmd_idx(ctx);

    if (idx == -1) return;

    memset(&cmd, 0, sizeof(cmd));
    cmd.code = ctx;

    /* Process the command */
    if (game_cmds[idx].fn) game_cmds[idx].fn(&cmd);
}


/*
 * Argument setting/getting generics
 */


/*
 * Set an argument of name 'arg' to data 'data'
 */
static void cmd_set_arg(struct command *cmd, const char *name, enum cmd_arg_type type,
    union cmd_arg_data data)
{
    size_t i;
    int first_empty = -1;
    int idx = -1;

    assert(name);
    assert(name[0]);

    /* Find an arg that either... */
    for (i = 0; i < CMD_MAX_ARGS; i++)
    {
        struct cmd_arg *arg = &cmd->arg[i];

        if (!arg->name[0] && first_empty == -1)
            first_empty = i;

        if (streq(arg->name, name))
        {
            idx = i;
            break;
        }
    }

    assert(first_empty != -1 || idx != -1);

    if (idx == -1)
        idx = first_empty;

    cmd->arg[idx].type = type;
    cmd->arg[idx].data = data;
    my_strcpy(cmd->arg[idx].name, name, sizeof(cmd->arg[0].name));
}


/*
 * Get an argument with name 'arg'
 */
static int cmd_get_arg(struct command *cmd, const char *arg, enum cmd_arg_type type,
    union cmd_arg_data *data)
{
    size_t i;

    for (i = 0; i < CMD_MAX_ARGS; i++)
    {
        if (streq(cmd->arg[i].name, arg))
        {
            if (cmd->arg[i].type != type)
                return CMD_ARG_WRONG_TYPE;

            *data = cmd->arg[i].data;
            return CMD_OK;
        }
    }

    return CMD_ARG_NOT_PRESENT;
}


/*
 * Strings
 */


/*
 * Set arg 'n' to given string
 */
void cmd_set_arg_string(struct command *cmd, const char *arg, const char *str)
{
    union cmd_arg_data data;

    data.string = string_make(str);
    cmd_set_arg(cmd, arg, arg_STRING, data);
}


/*
 * Retrieve arg 'n' if a string
 */
int cmd_get_arg_string(struct command *cmd, const char *arg, const char **str)
{
    union cmd_arg_data data;
    int err;

    if ((err = cmd_get_arg(cmd, arg, arg_STRING, &data)) == CMD_OK)
        *str = data.string;

    return err;
}


/*
 * Get a string, first from the command or failing that prompt the user
 */
int cmd_get_string(struct command *cmd, const char *arg, const char **str, const char *initial,
    const char *title, const char *prompt)
{
    char tmp[NORMAL_WID] = "";

    if (cmd_get_arg_string(cmd, arg, str) == CMD_OK)
        return CMD_OK;

    /* Introduce */
    if (title) c_msg_print(title);

    /* Prompt properly */
    if (initial)
        my_strcpy(tmp, initial, sizeof(tmp));

    if (get_string(prompt, tmp, sizeof(tmp)))
    {
        cmd_set_arg_string(cmd, arg, tmp);
        if (cmd_get_arg_string(cmd, arg, str) == CMD_OK)
            return CMD_OK;
    }

    return CMD_ARG_ABORTED;
}


/*
 * Numbers, quantities
 */


/*
 * Set argument 'n' to 'number'
 */
void cmd_set_arg_number(struct command *cmd, const char *arg, int amt)
{
    union cmd_arg_data data;

    data.number = amt;
    cmd_set_arg(cmd, arg, arg_NUMBER, data);
}


/*
 * Get argument 'n' as a number
 */
int cmd_get_arg_number(struct command *cmd, const char *arg, int *amt)
{
    union cmd_arg_data data;
    int err;

    if ((err = cmd_get_arg(cmd, arg, arg_NUMBER, &data)) == CMD_OK)
        *amt = data.number;

    return err;
}


/*
 * Get argument 'n' as a number; failing that, prompt for input
 */
int cmd_get_quantity(struct command *cmd, const char *arg, int *amt, int max, bool plural)
{
    if (cmd_get_arg_number(cmd, arg, amt) == CMD_OK)
        return CMD_OK;

    *amt = 1;

    if (!plural || (max > 1))
    {
        *amt = get_quantity(NULL, max);
        if (*amt > 0)
        {
            cmd_set_arg_number(cmd, arg, *amt);
            return CMD_OK;
        }

        return CMD_ARG_ABORTED;
    }

    return CMD_OK;
}


/*
 * Item arguments
 */


/*
 * Set argument 'n' to 'item'
 */
void cmd_set_arg_item(struct command *cmd, const char *arg, struct object *obj)
{
    union cmd_arg_data data;

    data.obj = obj;
    cmd_set_arg(cmd, arg, arg_ITEM, data);
}


/*
 * Retrieve argument 'n' as an item
 */
int cmd_get_arg_item(struct command *cmd, const char *arg, struct object **obj)
{
    union cmd_arg_data data;
    int err;

    if ((err = cmd_get_arg(cmd, arg, arg_ITEM, &data)) == CMD_OK)
        *obj = data.obj;

    return err;
}


/*
 * Get an item, first from the command or try the UI otherwise
 */
int cmd_get_item(struct command *cmd, const char *arg, struct object **obj, const char *prompt,
    const char *reject, item_tester filter, int mode)
{
    if (cmd_get_arg_item(cmd, arg, obj) == CMD_OK)
        return CMD_OK;

    if (get_item(obj, prompt, reject, cmd->code, filter, mode))
    {
        cmd_set_arg_item(cmd, arg, *obj);
        return CMD_OK;
    }

    return CMD_ARG_ABORTED;
}


/*
 * Targets
 */


/*
 * Set arg 'n' to target
 */
void cmd_set_arg_target(struct command *cmd, const char *arg, int target)
{
    union cmd_arg_data data;

    data.direction = target;
    cmd_set_arg(cmd, arg, arg_DIRECTION, data);
}


/*
 * Retrieve arg 'n' if it's a target
 */
int cmd_get_arg_target(struct command *cmd, const char *arg, int *target)
{
    union cmd_arg_data data;
    int err;

    if ((err = cmd_get_arg(cmd, arg, arg_DIRECTION, &data)) == CMD_OK)
        *target = data.direction;

    return err;
}


/*
 * Get a target, first from command or prompt otherwise
 */
int cmd_get_target(struct command *cmd, const char *arg, int *target)
{
    if (cmd_get_arg_target(cmd, arg, target) == CMD_OK)
        return CMD_OK;

    if (*target == DIR_SKIP)
    {
        *target = DIR_UNKNOWN;
        cmd_set_arg_target(cmd, arg, *target);
        return CMD_OK;
    }

    if (get_aim_dir(target))
    {
        cmd_set_arg_target(cmd, arg, *target);
        return CMD_OK;
    }

    return CMD_ARG_ABORTED;
}
