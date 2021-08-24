/* File: cmd4.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
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
#include "src/player_command.h"
#include <QFile>


/*
 * Array of feeling strings
 */
static QString do_cmd_feeling_text[LEV_THEME_HEAD] =
{
    "Looks like any other level.",
    "You feel there is something special about this level.",
    "You have a superb feeling about this level.",
    "You have an excellent feeling...",
    "You have a very good feeling...",
    "You have a good feeling...",
    "You feel strangely lucky...",
    "You feel your luck is turning...",
    "You like the look of this place...",
    "This level can't be all bad...",
    "What a boring place..."
};


/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling(void)
{
    bool is_quest_level = quest_check(p_ptr->depth);

    /* No sensing things in Moria */
    if (game_mode == GAME_NPPMORIA) return;

    /* No useful feeling in town */
    if (!p_ptr->depth)
    {
        message(QString("Looks like a typical town."));
        return;
    }

    /* No useful feelings until enough time has passed */
    if (!do_feeling)
    {
        message(QString("You are still uncertain about this level..."));
        return;
    }

    if (p_ptr->dungeon_type == DUNGEON_TYPE_WILDERNESS)
    {
        if (is_quest_level) 	message(QString("You have entered a wilderness level on the verge of destruction!."));
        else 					message(QString("You have entered an area of near pristine wilderness."));
    }

    else if (p_ptr->dungeon_type == DUNGEON_TYPE_GREATER_VAULT)
    {
        message(QString("You have discovered a gigantic vault of great treasures guarded by dangerous creatures."));
    }

    else if (p_ptr->dungeon_type == DUNGEON_TYPE_LABYRINTH)
    {
        if (is_quest_level) message(QString("You have entered a tiny, closely guarded labyrinth."));
        else message(QString("You have entered a complex labyrinth of dungeon hallways."));
    }

    else if (p_ptr->dungeon_type == DUNGEON_TYPE_ARENA)
    {
        message(QString("You are in an arena fighting for your life."));
    }

    /* Verify the feeling */
    else if (feeling >= LEV_THEME_HEAD)
    {

        /*print out a message about a themed level*/
        QString note;
        QString mon_theme;


        note = (QString("You have entered "));
        mon_theme = (QString(feeling_themed_level[feeling - LEV_THEME_HEAD]));
        if (begins_with_vowel(mon_theme)) note.append(QString("an "));
        else note.append(QString("a "));

        note.append(QString("%1 stronghold.") .arg(mon_theme));

        message(note);
    }

    /* Display the feeling */
    else message(QString(do_cmd_feeling_text[feeling]));

    /* Redraw the feeling indicator */
    p_ptr->redraw |= (PR_SIDEBAR_PL);
}




// Repeat the previous command
// Assumes the command and args were properly saved
void do_cmd_repeat(void)
{
    if (!character_dungeon) return;
    if (!p_ptr->command_previous) return;

    // repeat the previous command
    command_type *command_ptr = &command_info[p_ptr->command_previous];

    // Make sure we are dealing with the same item
    if (command_ptr->cmd_needs & (ARG_ITEM))
    {
        object_type *o_ptr = object_from_item_idx(p_ptr->command_previous_args.item);

        if (o_ptr->k_idx != p_ptr->command_previous_args.k_idx)
        {
            pop_up_message_box("Unable to repeat command.<br>Item has been moved or changed.");
            p_ptr->player_previous_command_wipe();
            return;
        }
    }

    if (!command_ptr->keep_direction())
    {
        p_ptr->command_previous_args.direction = DIR_UNKNOWN;
    }

    // Get the direction, if necessary
    if (command_ptr->needs_direction())
    {
        if (!get_aim_dir(&p_ptr->command_previous_args.direction, FALSE)) return;
    }

    command_ptr->command_function(p_ptr->command_previous_args);
}

/*
 * Look command
 */
void do_cmd_look(void)
{
    if (!character_dungeon) return;

    /* Look around */
    if (target_set_interactive(TARGET_LOOK, -1, -1))
    {
        message("Target Selected.");
        p_ptr->redraw |= (PR_SIDEBAR_MON);
        redraw_stuff();
    }
}


//Allow the player to manually record a note
void do_cmd_write_note(void)
{
    if (!character_dungeon) return;

    QString note = get_string("Please enter note you wish to record.", "Enter Note.  The '<' and '>' characters are reserved and should not be used", NULL);

    write_note(note, p_ptr->depth);
}


void do_cmd_suicide(void)
{
    /* Verify Retirement */
    if (p_ptr->total_winner)
    {
        /* Verify */
        if (!get_check(QString("Do you want to retire? "))) return;
    }

    else
    {
        /* Verify */
        if (!get_check("Do you really want to terminate this character? ")) return;

        QString answer = get_string("Character Termnation Verification", "Please verify by typing the '@' sign:", NULL);
        if (!answer.contains("@")) return;
    }

    /* Commit suicide */
    p_ptr->is_dead = TRUE;
    p_ptr->terminated = TRUE;

    /* Stop playing */
    p_ptr->playing = FALSE;

    /* Leaving */
    p_ptr->leaving_level = TRUE;

    /* Cause of death */
    if (p_ptr->total_winner) p_ptr->died_from = QString("Ripe Old Age");
    else p_ptr->died_from = QString("Quitting");

    // To get the game to notice
    player_death_close_game();

}
