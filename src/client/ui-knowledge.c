/*
 * File: ui-knowledge.c
 * Purpose: Knowledge screen
 *
 * Copyright (c) 2000-2007 Eytan Zweig, Andrew Doull, Pete Mack.
 * Copyright (c) 2010 Peter Denison, Chris Carr.
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


static void do_cmd_knowledge_aux(int hook, const char *header, bool abort)
{
    ui_event ea = EVENT_ABORT;

    /* Set the hook */
    special_line_type = hook;

    /* Set the header */
    my_strcpy(special_line_header[NTERM_WIN_OVERHEAD], header, sizeof(special_line_header[0]));

    /* Call the file perusal */
    if (!peruse_file() && abort) Term_event_push(&ea);
}


/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_OBJECT, "Known Objects", TRUE);
}


/*
 * Display known artifacts
 */
static void do_cmd_knowledge_artifacts(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_ARTIFACT, "Artifacts", TRUE);
}


/*
 * Display known ego items
 */
static void do_cmd_knowledge_ego_items(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_EGO, "Ego Items", TRUE);
}


/*
 * Display known monsters
 */
static void do_cmd_knowledge_monsters(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_KILL, "Monsters", TRUE);
}


/*
 * Interact with feature visuals
 */
static void do_cmd_knowledge_features(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_FEATURE, "Features", TRUE);
}


/*
 * Display hall of fame
 */
static void do_cmd_knowledge_scores(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_SCORES, "Hall of Fame", TRUE);
}


/*
 * Display character history
 */
static void do_cmd_knowledge_history(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_HISTORY, "Character History", TRUE);
}


/*
 * Display known uniques
 */
static void do_cmd_knowledge_uniques(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_UNIQUE, "Known Uniques", TRUE);
}


/*
 * Display party gear
 */
static void do_cmd_knowledge_gear(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_GEAR, "Party Gear", TRUE);
}


/*
 * Display owned houses
 */
static void do_cmd_knowledge_houses(const char *title, int row)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_HOUSES, "Owned Houses", TRUE);
}


/*
 * Definition of the "player knowledge" menu.
 */
static menu_action knowledge_actions[] =
{
    {0, 0, "Display object knowledge", do_cmd_knowledge_objects},
    {0, 0, "Display artifact knowledge", do_cmd_knowledge_artifacts},
    {0, 0, "Display ego item knowledge", do_cmd_knowledge_ego_items},
    {0, 0, "Display monster knowledge", do_cmd_knowledge_monsters},
    {0, 0, "Display feature knowledge", do_cmd_knowledge_features},
    {0, 0, "Display hall of fame", do_cmd_knowledge_scores},
    {0, 0, "Display character history", do_cmd_knowledge_history},
    {0, 0, "Display known uniques", do_cmd_knowledge_uniques},
    {0, 0, "Display party gear", do_cmd_knowledge_gear},
    {0, 0, "Display owned houses", do_cmd_knowledge_houses}
};


static menu_type knowledge_menu;


/*
 * Display the "player knowledge" menu.
 */
void textui_browse_knowledge(void)
{
    menu_type *menu;
    region knowledge_region = {0, 0, -1, 12};

    /* Initialize the knowledge menu */
    menu = &knowledge_menu;
    menu_init(menu, MN_SKIN_SCROLL, menu_find_iter(MN_ITER_ACTIONS));
    menu_setpriv(menu, N_ELEMENTS(knowledge_actions), knowledge_actions);

    menu->title = "Display current knowledge";
    menu->selections = lower_case;

    screen_save();
    menu_layout(menu, &knowledge_region);

    clear_from(0);
    menu_select(menu, 0, FALSE);

    screen_load(TRUE);
}


void do_cmd_players(void)
{
    do_cmd_knowledge_aux(SPECIAL_FILE_PLAYER, "Players", FALSE);
}