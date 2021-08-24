/*
 * File: ui-curse.c
 * Purpose: Curse selection menu
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2016 Nick McConnell
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


static int selection;


struct curse_menu_data
{
    int index;
    int power;
};


/*
 * Display an entry on the item menu
 */
static void get_curse_display(struct menu *menu, int oid, bool cursor, int row, int col, int width)
{
    struct curse_menu_data *choice = menu_priv(menu);
    byte attr = (cursor? COLOUR_L_BLUE: COLOUR_WHITE);
    char buf[NORMAL_WID];
    int power = choice[oid].power;
    char *name = curses[choice[oid].index].name;

    strnfmt(buf, sizeof(buf), " %s (curse strength %d)", name, power);
    c_put_str(attr, buf, row, col);
}


/*
 * Deal with events on the get_curse menu
 */
static bool get_curse_action(struct menu *menu, const ui_event *event, int oid)
{
    struct curse_menu_data *choice = menu_priv(menu);

    if (event->type == EVT_SELECT) selection = choice[oid].index;

    return false;
}


/*
 * Show curse long description when browsing
 */
static void curse_menu_browser(int oid, void *data, const region *loc)
{
    struct curse_menu_data *choice = data;
    char desc[MSG_LEN];

    /* Redirect output to the screen */
    Term_gotoxy(loc->col, loc->row + loc->page_rows);
    strnfmt(desc, sizeof(desc), "%s.\n", curses[choice[oid].index].desc);
    text_out_to_screen(COLOUR_WHITE, desc);
}


/*
 * Display list of curses to choose from
 */
static int curse_menu(struct object *obj, char *dice_string)
{
    menu_iter menu_f = {NULL, NULL, get_curse_display, get_curse_action, NULL};
    struct menu *m = menu_new(MN_SKIN_SCROLL, &menu_f);
    int row;
    unsigned int length = 0;
    int count = 0;
    struct curse_menu_data *available;
    static region area = { 20, 1, -1, -2 };
    char *s, *t;

    /* Count and then list the curses */
    s = string_make(obj->info_xtra.name_curse);
    t = strtok(s, "|");
    while (t)
    {
        count++;
        t = strtok(NULL, "|");
    }
    string_free(s);
    if (!count) return -1;
    available = mem_zalloc(count * sizeof(struct curse_menu_data));
    count = 0;
    s = string_make(obj->info_xtra.name_curse);
    t = strtok(s, "|");
    while (t)
    {
        int index = atoi(t);

        available[count++].index = index;
        length = MAX(length, strlen(curses[index].name) + 13);
        t = strtok(NULL, "|");
    }
    string_free(s);
    count = 0;
    s = string_make(obj->info_xtra.name_power);
    t = strtok(s, "|");
    while (t)
    {
        int power = atoi(t);

        available[count++].power = power;
        t = strtok(NULL, "|");
    }
    string_free(s);

    screen_save();

    /* Set up the menu */
    menu_setpriv(m, count, available);
    m->header = format("Remove which curse (spell strength %s)?", dice_string);
    m->selections = lower_case;
    m->flags = MN_PVT_TAGS;
    m->browse_hook = curse_menu_browser;

    /* Set up the item list variables */
    selection = -1;

    /* Set up the menu region */
    area.page_rows = m->count + 2;
    area.row = 1;
    area.col = (Term->wid - 1 - length) / 2;
    if (area.col <= 3) area.col = 0;
    area.width = MAX(length + 1, strlen(m->header));

    for (row = area.row; row <= area.row + area.page_rows; row++)
        prt("", row, MAX(0, area.col - 1));

    menu_layout(m, &area);

    /* Choose */
    menu_select(m, 0, true);

    screen_load(true);

    /* Clean up */
    mem_free(available);
    mem_free(m);

    /* Result */
    return selection;
}


bool textui_get_curse(int *choice, struct object *obj, char *dice_string)
{
    int curse = curse_menu(obj, dice_string);

    if (curse != -1)
    {
        *choice = curse;
        return true;
    }
    return false;
}
