/*
 * File: variable.c
 * Purpose: Various global variables
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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

#include "angband.h"

/*
 * Array[RANDNAME_NUM_TYPES][num_names] of random names
 */
u32b *num_names;
const char ***name_sections;

/*  
 * Structure (not array) of size limits
 */
maxima *z_info;

/*
 * The info arrays
 */
object_kind *k_info;
struct player_race *races;
struct player_class *classes;
social_type *soc_info;
struct hint *hints;
monster_race *r_info;

/*
 * FPS
 */
s16b cfg_fps = 1;

/*
 * Total Hack -- allow all items to be listed (even empty ones)
 * This is only used by "do_cmd_inven_e()" and is cleared there.
 */
bool item_tester_full;

/*
 * Here is a "pseudo-hook" used during calls to "get_item()" and
 * "show_inven()" and "show_equip()", and the choice window routines.
 */
byte item_tester_tval;

/*
 * Here is a "hook" used during calls to "get_item()" and
 * "show_inven()" and "show_equip()", and the choice window routines.
 */
bool (*item_tester_hook)(struct player *p, const object_type *o_ptr);

/*
 * Abbreviations of healthy stats
 */
const char *stat_names[A_MAX] =
{
    "STR: ", "INT: ", "WIS: ", "DEX: ", "CON: ", "CHR: "
};

/*
 * Abbreviations of damaged stats
 */
const char *stat_names_reduced[A_MAX] =
{
    "Str: ", "Int: ", "Wis: ", "Dex: ", "Con: ", "Chr: "
};

/*
 * Global array of color names and translations.
 */
color_type color_table[MAX_COLORS] =
{
    /* full mono vga blind lighter darker highlight metallic misc door */
    {'d', "Dark", {0, 0, 0, TERM_DARK, TERM_L_DARK, TERM_DARK,
        TERM_L_DARK, TERM_L_DARK, TERM_DARK, TERM_DARK}},
    {'w', "White", {1, 1, 1, TERM_WHITE, TERM_YELLOW, TERM_SLATE,
        TERM_L_BLUE, TERM_YELLOW, TERM_WHITE, TERM_WHITE}},
    {'s', "Slate", {2, 1, 2, TERM_SLATE, TERM_L_WHITE, TERM_L_DARK,
        TERM_L_WHITE, TERM_L_WHITE, TERM_SLATE, TERM_SLATE}},
    {'o', "Orange", {3, 1, 3, TERM_L_WHITE, TERM_YELLOW, TERM_SLATE,
        TERM_YELLOW, TERM_YELLOW, TERM_ORANGE, TERM_ORANGE}},
    {'r', "Red", {4, 1, 4, TERM_SLATE, TERM_L_RED, TERM_SLATE,
        TERM_L_RED, TERM_L_RED, TERM_RED, TERM_RED}},
    {'g', "Green", {5, 1, 5, TERM_SLATE, TERM_L_GREEN, TERM_SLATE,
        TERM_L_GREEN, TERM_L_GREEN, TERM_GREEN, TERM_GREEN}},
    {'b', "Blue", {6, 1, 6, TERM_SLATE, TERM_L_BLUE, TERM_SLATE,
        TERM_L_BLUE, TERM_L_BLUE, TERM_BLUE, TERM_BLUE}},
    {'u', "Umber", {7, 1, 7, TERM_L_DARK, TERM_L_UMBER, TERM_L_DARK,
        TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER, TERM_UMBER}},
    {'D', "Light Dark", {8, 1, 8, TERM_L_DARK, TERM_SLATE, TERM_L_DARK,
        TERM_SLATE, TERM_SLATE, TERM_L_DARK, TERM_L_DARK}},
    {'W', "Light Slate", {9, 1, 9, TERM_L_WHITE, TERM_WHITE, TERM_SLATE,
        TERM_WHITE, TERM_WHITE, TERM_SLATE, TERM_WHITE}},
    {'P', "Light Purple", {10, 1, 10, TERM_SLATE, TERM_YELLOW, TERM_SLATE,
        TERM_YELLOW, TERM_YELLOW, TERM_L_PURPLE, TERM_VIOLET}},
    {'y', "Yellow", {11, 1, 11, TERM_L_WHITE, TERM_L_YELLOW, TERM_L_WHITE,
        TERM_WHITE, TERM_WHITE, TERM_YELLOW, TERM_YELLOW}},
    {'R', "Light Red", {12, 1, 12, TERM_L_WHITE, TERM_YELLOW, TERM_RED,
        TERM_YELLOW, TERM_YELLOW, TERM_L_RED, TERM_RED}},
    {'G', "Light Green", {13, 1, 13, TERM_L_WHITE, TERM_YELLOW, TERM_GREEN,
        TERM_YELLOW, TERM_YELLOW, TERM_L_GREEN, TERM_GREEN}},
    {'B', "Light Blue", {14, 1, 14, TERM_L_WHITE, TERM_YELLOW, TERM_BLUE,
        TERM_YELLOW, TERM_YELLOW, TERM_L_BLUE, TERM_BLUE}},
    {'U', "Light Umber", {15, 1, 15, TERM_L_WHITE, TERM_YELLOW, TERM_UMBER,
        TERM_YELLOW, TERM_YELLOW, TERM_L_UMBER, TERM_UMBER}},

    /* "new" colors */
    {'p', "Purple", {16, 1, 10, TERM_SLATE, TERM_L_PURPLE, TERM_SLATE,
        TERM_L_PURPLE, TERM_L_PURPLE, TERM_L_PURPLE, TERM_VIOLET}},
    {'v', "Violet", {17, 1, 10, TERM_SLATE, TERM_L_PURPLE, TERM_SLATE,
        TERM_L_PURPLE, TERM_L_PURPLE, TERM_L_PURPLE, TERM_VIOLET}},
    {'t', "Teal", {18, 1, 6, TERM_SLATE, TERM_L_TEAL, TERM_SLATE,
        TERM_L_TEAL, TERM_L_TEAL, TERM_L_BLUE, TERM_BLUE}},
    {'m', "Mud", {19, 1, 5, TERM_SLATE, TERM_MUSTARD, TERM_SLATE,
        TERM_MUSTARD, TERM_MUSTARD, TERM_UMBER, TERM_UMBER}},
    {'Y', "Light Yellow", {20, 1, 11, TERM_WHITE, TERM_WHITE, TERM_YELLOW,
        TERM_WHITE, TERM_WHITE, TERM_L_YELLOW, TERM_YELLOW}},
    {'i', "Magenta-Pink", {21, 1, 12, TERM_SLATE, TERM_L_PINK, TERM_RED,
        TERM_L_PINK, TERM_L_PINK, TERM_L_PURPLE, TERM_RED}},
    {'T', "Light Teal", {22, 1, 14, TERM_L_WHITE, TERM_YELLOW, TERM_TEAL,
        TERM_YELLOW, TERM_YELLOW, TERM_L_BLUE, TERM_GREEN}},
    {'V', "Light Violet", {23, 1, 10, TERM_L_WHITE, TERM_YELLOW, TERM_VIOLET,
        TERM_YELLOW, TERM_YELLOW, TERM_L_PURPLE, TERM_VIOLET}},
    {'I', "Light Pink", {24, 1, 12, TERM_L_WHITE, TERM_YELLOW, TERM_MAGENTA,
        TERM_YELLOW, TERM_YELLOW, TERM_L_PURPLE, TERM_RED}},
    {'M', "Mustard", {25, 1, 11, TERM_SLATE, TERM_YELLOW, TERM_SLATE,
        TERM_YELLOW, TERM_YELLOW, TERM_YELLOW, TERM_YELLOW}},
    {'z', "Blue Slate",  {26, 1, 9, TERM_SLATE, TERM_DEEP_L_BLUE, TERM_SLATE,
        TERM_DEEP_L_BLUE, TERM_DEEP_L_BLUE, TERM_L_WHITE, TERM_BLUE}},
    {'Z', "Deep Light Blue", {27, 1, 14, TERM_L_WHITE, TERM_L_BLUE, TERM_BLUE_SLATE,
        TERM_L_BLUE, TERM_L_BLUE, TERM_L_BLUE, TERM_BLUE}},

    /* Rest to be filled in when the game loads */
};


/*
 * Player Sexes
 *
 *      Title,
 *      Winner
 */
player_sex sex_info[MAX_SEXES] =
{
    {
        "Female",
        "Queen"
    },
    {
        "Male",
        "King"
    },
    {
        "Neuter",
        "Regent"
    }
};


/*
 * Various directories. These are no longer necessarily all subdirs of "lib"
 */
char *ANGBAND_DIR_EDIT;
char *ANGBAND_DIR_FILE;
char *ANGBAND_DIR_PREF;
char *ANGBAND_DIR_USER;
