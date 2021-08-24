/* File: player_command.cpp */

/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include <src/npp.h>
#include <src/cmds.h>
#include "player_command.h"
#include "nppdialog.h"

// Check if we have a completed command
bool command_type::repeated_command_completed(void)
{
    if (!repeat_allowed) return (TRUE);
    if (p_ptr->is_resting()) return (FALSE);
    if (!p_ptr->player_args.repeats) return (TRUE);
    return (FALSE);
}

// See if we should re-use the old direction on a repeated command
bool command_type::keep_direction(void)
{
    if (!needs_direction()) return (FALSE);
    if (!use_old_target) return (FALSE);
    if (repeat_num) return (TRUE);
    if (!target_okay()) return (FALSE);
    return(TRUE);
}

// Check if we have a completed command
bool command_type::needs_direction(void)
{
    if(cmd_needs & (ARG_DIRECTION)) return (TRUE);
    return (FALSE);
}

// Check if we have a completed command
bool command_type::needs_item()
{
    if(cmd_needs & (ARG_ITEM)) return (TRUE);
    return (FALSE);
}

// Check if we have a completed command
bool command_type::needs_quantity()
{
    if (!needs_item()) return (FALSE);
    if(cmd_needs & (ARG_NUMBER)) return (TRUE);
    return (FALSE);
}

// Check if we have a completed command
bool command_type::needs_slot(void)
{
    if (!needs_item()) return (FALSE);
    if(cmd_needs & (ARG_SLOT)) return (TRUE);
    return (FALSE);
}

// Resting needs special handling
bool command_type::is_special(void)
{
    if(cmd_needs & (ARG_SPECIAL)) return (TRUE);
    return (FALSE);
}

// Update needed every time ARG_NUMBER is added to table below.
QString command_type::prompt(int command)
{
    QString return_string = QString("Please enter an amount ");
    if (command == CMD_DROP) return_string.append("to drop.");
    else if (command == CMD_DESTROY) return_string.append("to destroy.");
    else return_string.append(".");

    return(return_string);
}

cmd_arg command_type::find_slot(object_type *o_ptr, cmd_arg args, int command)
{
    if (command == CMD_WIELD)
    {
        args.slot = wield_slot(o_ptr);
    }

    return (args);
}

// Targeting for spells and items are handled separately
command_type command_info[] =
{   // CMD_NONE
    {0L,            NULL, FALSE, FALSE, 0},
    // CMD_RESTING
    {ARG_SPECIAL,   command_rest,   TRUE,  TRUE,  0},
    // CMD_RUNNING
    {ARG_DIRECTION, command_run,    TRUE,  TRUE,  9999},
    // CMD_WALK
    {ARG_DIRECTION, command_walk,   FALSE,  FALSE,  0},
    // CMD_JUMP
    {ARG_DIRECTION, command_walk,   TRUE,  FALSE,  0},
    // CMD_OPEN
    {ARG_DIRECTION, command_open,   TRUE,  TRUE,  99},
    // CMD_CLOSE
    {ARG_DIRECTION, command_close,  TRUE,  FALSE, 0},
    // CMD_SPIKE
    {ARG_DIRECTION, command_spike,  TRUE,  FALSE, 0},
    // CMD_DISARM
    {ARG_DIRECTION, command_disarm, TRUE,  TRUE,  99},
    // CMD_BASH
    {ARG_DIRECTION, command_bash,   TRUE,  TRUE,  99},
    // CMD_TUNNEL
    {ARG_DIRECTION, command_tunnel, TRUE,  TRUE,  99},
    // CMD_ALTER
    {ARG_DIRECTION, command_alter,  TRUE,  TRUE,  99},
    // CMD_SEARCH
    {ARG_DIRECTION, command_search, TRUE,  FALSE,  0},
    // CMD_MAKE_TRAP
    {ARG_DIRECTION, command_make_trap, TRUE,  FALSE, 0},
    // CMD_HOLD
    {0L,            command_hold,   TRUE,  FALSE, 0},
    // CMD_TAKEOFF
    {ARG_ITEM,      command_takeoff, TRUE,  FALSE, 0},
    // CMD_WIELD
    {ARG_ITEM | ARG_SLOT, command_wield, TRUE,  FALSE, 0},
    // CMD_SWAP
    {0L,            command_swap, TRUE,  FALSE, 0},
    // CMD_ITEM_USE
    {ARG_ITEM,      command_use,    TRUE,  FALSE, 0},
    // CMD_REFUEL
    {ARG_ITEM,      command_refuel, TRUE,  FALSE, 0},
    // CMD_FIRE
    {ARG_ITEM | ARG_DIRECTION,command_fire,TRUE,  FALSE, 0},
    // CMD_FIRE_NEAR
    {ARG_ITEM,      command_fire_nearest,TRUE,  FALSE, 0},
    // CMD_DROP
    {ARG_ITEM | ARG_NUMBER,  command_drop,   TRUE,  FALSE, 0},
    // CMD_PICKUP
    {ARG_ITEM,  command_pickup,   TRUE,  FALSE, 0},
    // CMD_BROWSE
    {ARG_ITEM,      command_browse,   TRUE,  FALSE, 0},
    // CMD_STUDY
    {ARG_ITEM,      command_study,   TRUE,  FALSE, 0},
    // CMD_CAST
    {0L, command_cast,   TRUE,  FALSE, 0},
    // CMD_DESTROY
    {ARG_ITEM | ARG_NUMBER, command_destroy,   TRUE,  FALSE, 0},
    // CMD_EXAMINE
    {ARG_ITEM,      command_examine,   TRUE,  FALSE, 0},
    // CMD_INSCRIBE
    {ARG_ITEM,      command_inscribe,   TRUE,  FALSE, 0},
    // CMD_UNINSCRIBE
    {ARG_ITEM,      command_uninscribe,   TRUE,  FALSE, 0},
    // CMD_ACTIVATE
    {ARG_ITEM, command_use, TRUE,  FALSE, 0},
    // CMD_THROW
    {ARG_ITEM, command_throw, TRUE,  FALSE, 0},
    // CMD_SETTINGS - not handled here
    {ARG_SPECIAL, NULL, FALSE,  FALSE, 0},
};

// Prepare a command for processing.
// First we must make sure the cmd_arg has all the necessary information
void process_command(int item, s16b command)
{
    // Paranoia
    if (command == CMD_MAX) return;

    // Don't process for dead characters
    if (p_ptr->is_dead)
    {
        if ((command != CMD_EXAMINE) && (command != CMD_SETTINGS)) return;
    }

    // Now that we have a match, process the command.
    command_type *command_ptr = &command_info[command];
    cmd_arg args;
    args.wipe();
    object_type *o_ptr;

    /*
     * Note if the command doesn't call for an item,
     * this will point to the first item on the floor.
     */
    o_ptr = object_from_item_idx(item);
    args.item = item;

    if (!use_old_target) args.direction = DIR_UNKNOWN;

    // Get the direction, if necessary
    if (command_ptr->needs_direction())
    {
        if (!get_aim_dir(&args.direction, FALSE)) return;
    }

    // Only for objects
    if(command_ptr->needs_quantity())
    {
        args.number = get_quantity(command_ptr->prompt(command), o_ptr->number, o_ptr->number, FALSE);
    }

    // Only for objects
    if (command_ptr->needs_slot())
    {
        args = command_ptr->find_slot(o_ptr, args, command);
    }

    // resting needs special handling
    if (command_ptr->is_special())
    {
        if (command == CMD_RESTING)
        {
            RestDialog(&args.choice);
            //no choice made
            if (!args.choice) return;
            if (args.choice == REST_TURNCOUNT)
            {
                args.repeats = get_quantity(QString("Enter rest turncount"), 9999, 0, FALSE);
            }
        }
    }

    else if (command_ptr->repeat_allowed) args.repeats = command_ptr->repeat_num;

    args.verify = command_ptr->default_verify;

    // Finally, process the command
    command_ptr->command_function(args);

}
