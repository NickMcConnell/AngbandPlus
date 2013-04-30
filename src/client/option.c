/*
 * File: option.c
 * Purpose: Options table and definitions.
 *
 * Copyright (c) 1997 Ben Harrison
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


/*
 * Option screen interface
 */
const int option_page[OPT_PAGE_MAX][OPT_PAGE_PER] =
{
    /* Interface */
    {
        OPT_use_sound,
        OPT_rogue_like_commands,
        OPT_use_old_target,
        OPT_pickup_always,
        OPT_pickup_inven,
        OPT_easy_open,
        OPT_active_auto_retaliator,
        OPT_pause_after_detect,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE
    },

    /* Display */
    {
        OPT_hp_changes_color,
        OPT_center_player,
        OPT_show_flavors,
        OPT_view_yellow_light,
        OPT_animate_flicker,
        OPT_purple_uniques,
        OPT_view_orange_light,
        OPT_highlight_leader,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE
    },

    /* Warning */
    {
        OPT_disturb_move,
        OPT_disturb_near,
        OPT_disturb_detect,
        OPT_disturb_state,
        OPT_notify_recharge,
        OPT_disturb_panel,
        OPT_auto_accept,
        OPT_disturb_icky,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE
    },

    /*** Birth/Difficulty ***/
    {
        OPT_birth_ironman,
        OPT_birth_no_stores,
        OPT_birth_no_artifacts,
        OPT_birth_no_feelings,
        OPT_birth_no_selling,
        OPT_birth_no_ghost,
        OPT_birth_fruit_bat,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE,
        OPT_NONE
    }
};


struct option
{
    const char *name;
    const char *description;
    bool normal;
};


static const struct option options[OPT_MAX] =
{
    {"use_sound",               "Use sound",                                    FALSE},
    {"rogue_like_commands",     "Use the roguelike command keyset",             FALSE},
    {"use_old_target",          "Use old target by default",                    FALSE},
    {"pickup_always",           "Always pickup items",                          FALSE},
    {"pickup_inven",            "Always pickup items matching inventory",       TRUE},
    {"easy_open",               "Open/disarm/close without direction",          TRUE},
    {"active_auto_retaliator",  "Active auto-retaliator",                       TRUE},
    {"pause_after_detect",      "Freeze screen after detecting monsters",       TRUE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},

    {"hp_changes_color",        "Color: Player color indicates % hit points",   FALSE},
    {"center_player",           "Center map continuously",                      FALSE},
    {"show_flavors",            "Show flavors in object descriptions",          FALSE},
    {"view_yellow_light",       "Color: Illuminate torchlight in yellow",       FALSE},
    {"animate_flicker",         "Color: Shimmer multi-colored things",          FALSE},
    {"purple_uniques",          "Color: Show unique monsters in purple",        FALSE},
    {"view_orange_light",       "Color: Illuminate torchlight in orange",       TRUE},
    {"highlight_leader",        "Use special color for party leader",           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},

    {"disturb_move",            "Disturb whenever any monster moves",           FALSE},
    {"disturb_near",            "Disturb whenever viewable monster moves",      TRUE},
    {"disturb_detect",          "Disturb when leaving trap detected area",      TRUE},
    {"disturb_state",           "Disturb whenever player state changes",        TRUE},
    {"notify_recharge",         "Notify on object recharge",                    FALSE},
    {"disturb_panel",           "Disturb whenever map panel changes",           TRUE},
    {"auto_accept",             "Always say Yes to Yes/No prompts",             FALSE},
    {"disturb_icky",            "Get out of icky screens when disturbed",       FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},

    {"birth_ironman",           "Restrict the use of stairs/recall",            FALSE},
    {"birth_no_stores",         "Restrict the use of stores/home",              FALSE},
    {"birth_no_artifacts",      "Restrict creation of artifacts",               FALSE},
    {"birth_no_feelings",       "Don't show level feelings",                    FALSE},
    {"birth_no_selling",        "Items always sell for 0 gold",                 FALSE},
    {"birth_no_ghost",          "Death is permanent",                           FALSE},
    {"birth_fruit_bat",         "Play as a fruit bat",                          FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE},
    {NULL,                      NULL,                                           FALSE}
};


/* Accessor functions */
const char *option_name(int opt)
{
    if (opt >= OPT_MAX) return NULL;
    return options[opt].name;
}

const char *option_desc(int opt)
{
    if (opt >= OPT_MAX) return NULL;
    return options[opt].description;
}

/* Setup functions */
bool option_set(bool *opts, const char *name, bool on)
{
    size_t opt;

    for (opt = 0; opt < OPT_MAX; opt++)
    {
        if (!options[opt].name || !streq(options[opt].name, name)) continue;
        opts[opt] = on;

        return TRUE;
    }

    return FALSE;
}

void option_set_defaults(bool *opts)
{
    size_t opt;

    for (opt = 0; opt < OPT_MAX; opt++)
        opts[opt] = options[opt].normal;
}
