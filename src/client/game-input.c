/*
 * File: game-input.c
 * Purpose: Ask for non-command input from the UI.
 *
 * Copyright (c) 2014 Nick McConnell
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


bool (*get_string_hook)(const char *prompt, char *buf, int len);
int (*get_string_ex_hook)(const char *prompt, char *buf, int len, bool priv);
s32b (*get_quantity_hook)(const char *prompt, s32b max);
s32b (*get_quantity_ex_hook)(const char *prompt, s32b max);
bool (*get_check_hook)(const char *prompt);
int (*get_check_ex_hook)(const char *prompt);
bool (*get_com_hook)(const char *prompt, struct keypress *command);
bool (*get_com_ex_hook)(const char *prompt, ui_event *command);
bool (*get_aim_dir_hook)(int *dir);
int (*get_aim_dir_ex_hook)(int *dir);
int (*get_spell_hook)(int book, const char *verb, bool (*spell_test)(int, int));
bool (*get_item_hook)(struct object **choice, const char *pmt, const char *str, cmd_code cmd,
    item_tester tester, int mode);
bool (*get_curse_hook)(int *choice, struct object *obj, char *dice_string);


/*
 * Prompt for a string from the user.
 *
 * prompt is the prompt to the user, and should take the form "Prompt: "
 * buf is the user string, and the value passed in is the default
 * len is the length of buf
 *
 * Returns whether the user accepted the entered value or escaped
 */
bool get_string(const char *prompt, char *buf, int len)
{
    /* Ask the UI for it */
    if (get_string_hook) return get_string_hook(prompt, buf, len);
    return false;
}


/*
 * Get a string from the user
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
int get_string_ex(const char *prompt, char *buf, int len, bool priv)
{
    /* Ask the UI for it */
    if (get_string_ex_hook) return get_string_ex_hook(prompt, buf, len, priv);
    return 0;
}


/*
 * Request a quantity from the user
 *
 * prompt is the prompt to the user, and should take the form "Prompt: "
 * max is the maximum value to accept
 *
 * Returns the quantity
 */
s32b get_quantity(const char *prompt, s32b max)
{
    /* Ask the UI for it */
    if (get_quantity_hook) return get_quantity_hook(prompt, max);
    return 0;
}


/*
 * Request a "quantity" from the user
 *
 * Return -1 on abort, 0 on escape
 */
s32b get_quantity_ex(const char *prompt, s32b max)
{
    /* Ask the UI for it */
    if (get_quantity_ex_hook) return get_quantity_ex_hook(prompt, max);
    return 0;
}


/*
 * Verify something with the user
 *
 * prompt is the prompt to the user, and should take the form "Query? "
 * Return whether the user answered "y"
 *
 * get_check_hook() should be set to a function which asks the user for a
 * "y/n" answer
 */
bool get_check(const char *prompt)
{
    /* Ask the UI for it */
    if (get_check_hook) return get_check_hook(prompt);
    return false;
}


/*
 * Verify something with the user
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
int get_check_ex(const char *prompt)
{
    /* Ask the UI for it */
    if (get_check_ex_hook) return get_check_ex_hook(prompt);
    return 0;
}


/*
 * Prompts for a keypress
 *
 * prompt is the prompt to the user, and should take the form "Command: "
 * command stores the keypress
 *
 * Returns whether the user accepted the entered value or escaped
 */
bool get_com(const char *prompt, struct keypress *command)
{
    /* Ask the UI for it */
    if (get_com_hook) return get_com_hook(prompt, command);
    return false;
}


bool get_com_ex(const char *prompt, ui_event *command)
{
    /* Ask the UI for it */
    if (get_com_ex_hook) return get_com_ex_hook(prompt, command);
    return false;
}


/*
 * Get an "aiming" direction from the user.
 *
 * dir is a pointer to an integer representing the chosen direction
 *
 * Return true if a direction was chosen, otherwise return false.
 */
bool get_aim_dir(int *dir)
{
    /* Ask the UI for it */
    if (get_aim_dir_hook) return get_aim_dir_hook(dir);
    return false;
}


int get_aim_dir_ex(int *dir)
{
    /* Ask the UI for it */
    if (get_aim_dir_ex_hook) return get_aim_dir_ex_hook(dir);
    return 0;
}


/*
 * Get a spell from the player.
 */
int get_spell(int book, const char *verb, bool (*spell_filter)(int, int))
{
    /* Ask the UI for it */
    if (get_spell_hook) return get_spell_hook(book, verb, spell_filter);
    return -1;
}


/*
 * Let the user select an object, save its address
 *
 * choice is the chosen object
 * pmt is the prompt to the player
 * str is the message if no valid item is available
 * cmd is the command (if any) the request is called from
 * tester is the function (if any) used to test for valid objects
 * mode gives more information on where the object can be chosen from
 *
 * If a legal item is selected , we save it in "obj" and return true.
 * If no item is available, we do nothing to "obj", and we display a
 * warning message, using "str" if available, and return false.
 * If no item is selected, we do nothing to "obj", and return false.
 */
bool get_item(struct object **choice, const char *pmt, const char *str, cmd_code cmd,
    item_tester tester, int mode)
{
    /* Ask the UI for it */
    if (get_item_hook) return get_item_hook(choice, pmt, str, cmd, tester, mode);
    return false;
}


/*
 * Get a curse from an object
 */
bool get_curse(int *choice, struct object *obj, char *dice_string)
{
    /* Ask the UI for it */
    if (get_curse_hook) return get_curse_hook(choice, obj, dice_string);
    return false;
}
