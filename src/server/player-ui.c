/*
 * File: player-ui.c
 * Purpose: Character screens and dumps
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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


#include "s-angband.h"


/*** Status line display functions ***/


/*
 * Print the status line.
 */
static void update_statusline(struct player *p)
{
    int row = 1;
    int col = 0;

    /* Set the hooks */
    put_str_hook = buffer_put_str;

    /* Clear the buffer */
    buffer_clear();

    col += display_depth(p, row, col);

    /* Print the status line */
    display_statusline(p, row, col);
}


/*** Utility functions for character dumps ***/


/*
 * Display the character in a file (three different modes)
 *
 * The top two lines, and the bottom line (or two) are left blank.
 *
 * Mode 0 = standard display with skills/history
 * Mode 1 = special display with equipment flags
 * Mode 2 = special display with equipment flags (ESP flags)
 *
 * Similar to the function in "ui-display.c" but modified to work server-side.
 * This is used for server-side character dumps.
 */
static void display_player_file(struct player *p, byte mode)
{
    /* Set the hooks */
    clear_hook = buffer_clear;
    region_erase_hook = NULL;
    put_ch_hook = buffer_put_ch;
    put_str_hook = buffer_put_str;
    use_bigtile_hook = false;

    /* Display the character in a file */
    display_player(p, mode);
}


static void dump_buffer(ang_file *fff, int y1, int y2, int length, bool skip_empty)
{
    int y;
    char buf[100];
    char *s;

    for (y = y1; y <= y2; y++)
    {
        /* Dump each row */
        if (length > 0)
            my_strcpy(buf, buffer_line(y), length);
        else if (length <= 0)
            my_strcpy(buf, buffer_line(y) - length, sizeof(buf));
        s = buf + strlen(buf);

        /* Back up over spaces */
        while ((s > buf) && (s[-1] == ' ')) --s;

        /* Terminate */
        *s = '\0';

        /* End the row */
        if (!(skip_empty && STRZERO(buf))) file_putf(fff, "%s\n", buf);
    }
}


/*
 * Write a character dump
 */
static void write_character_dump(ang_file *fff, void *data)
{
    struct player *p = (struct player *)data;
    int i;
    u16b a;
    char c;
    struct store *home = p->home;
    struct object **home_list = mem_zalloc(sizeof(struct object *) * z_info->store_inven_max);
    char o_name[NORMAL_WID];
    time_t ct = time((time_t*)0);
    char today[10];
    int x1, x2, y1, y2;
    char attr;
    struct grid_data g;
    char sx;
    bool victory = streq(p->death_info.died_from, "winner");
    bool final = (p->is_dead || !p->alive || victory);
    struct chunk *cv = chunk_get(&p->wpos);
    struct loc grid;

    switch (p->psex)
    {
        case SEX_FEMALE: sx = 'f'; break;
        case SEX_MALE: sx = 'm'; break;
        default: sx = 'n'; break;
    }

    /*
     * Add ladder information, this line is used by the online ladder and
     * not displayed when viewing a character dump online.
     */
    if (p->ladder)
    {
        strftime(today, 9, "%m/%d/%y", localtime(&ct));
        file_putf(fff,
            "# %u|%u|%-.8s|%-.25s|%c|%2d|%2d|%3d|%3d|%3d|%3d|%-.31s|%s\n",
            total_points(p, p->max_exp, p->max_depth), p->au, today,
            p->name, sx, p->race->ridx, p->clazz->cidx, p->lev, p->wpos.depth,
            p->max_lev, p->max_depth, p->death_info.died_from, version_build(NULL, false));

        /* Leave it at that for characters lower than level 20 */
        if (p->lev < 20) return;
    }

    /* Begin dump */
    file_putf(fff, "  [%s Character Dump]\n\n", version_build(cfg_chardump_label, false));

    /* Display player */
    display_player_file(p, 0);

    /* Dump part of the screen */
    dump_buffer(fff, 0, 17, 0, false);
    dump_buffer(fff, 18, 20, 0, true);

    /* Skip a line */
    file_put(fff, "\n");

    /* Display player */
    display_player_file(p, 1);

    /* Dump part of the screen */
    dump_buffer(fff, 10, 18, 39, false);

    /* Skip a line */
    file_put(fff, "\n");

    /* Dump part of the screen */
    dump_buffer(fff, 10, 18, -38, false);

    /* Skip a line */
    file_put(fff, "\n");

    /* Display player */
    display_player_file(p, 2);

    /* Dump part of the screen */
    dump_buffer(fff, 10, 18, 0, false);

    /* Skip some lines */
    file_put(fff, "\n\n");

    /* If dead, dump last messages */
    if (final)
    {
        /* Display the last 15 messages */
        i = p->msg_hist_ptr - 15;
        if (i < 0) i += MAX_MSG_HIST;

        file_put(fff, "  [Last Messages]\n\n");
        while (i != p->msg_hist_ptr)
        {
            if (!STRZERO(p->msg_log[i])) file_putf(fff, "> %s\n", p->msg_log[i]);
            i++;
            if (i == MAX_MSG_HIST) i = 0;
        }
        if (victory) file_putf(fff, "\nAll Hail the Mighty %s!\n\n\n", get_title(p));
        else file_putf(fff, "\nKilled by %s.\n\n\n", p->death_info.died_from);
    }

    /* Dump the equipment */
    file_put(fff, "  [Character Equipment]\n\n");
    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (!obj) continue;

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
        file_putf(fff, "%c) %s\n", I2A(i), o_name);
        object_info_chardump(p, fff, obj);
    }

    /* Dump the quiver */
    file_put(fff, "\n  [Character Quiver]\n\n");
    for (i = 0; i < z_info->quiver_size; i++)
    {
        struct object *obj = p->upkeep->quiver[i];

        if (!obj) continue;

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
        file_putf(fff, "%c) %s\n", I2A(i), o_name);
        object_info_chardump(p, fff, obj);
    }

    /* Dump the inventory */
    file_put(fff, "\n  [Character Inventory]\n\n");
    for (i = 0; i < z_info->pack_size; i++)
    {
        struct object *obj = p->upkeep->inven[i];

        if (!obj) continue;

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
        file_putf(fff, "%c) %s\n", I2A(i), o_name);
        object_info_chardump(p, fff, obj);
    }
    file_put(fff, "\n");

    /* Dump the Home -- if anything there */
    store_stock_list(p, home, home_list, z_info->store_inven_max);
    if (home->stock_num)
    {
        /* Header */
        file_put(fff, "  [Home Inventory]\n\n");

        /* Dump all available items */
        for (i = 0; i < z_info->store_inven_max; i++)
        {
            struct object *obj = home_list[i];

            if (!obj) break;
            object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
            file_putf(fff, "%c) %s\n", I2A(i), o_name);
            object_info_chardump(p, fff, obj);
        }

        /* Add an empty line */
        file_put(fff, "\n");
    }

    if (has_home_inventory(p)) house_dump(p, fff);

    /* Dump character history */
    file_put(fff, "  [Character History]\n\n");
    dump_history(p, fff);

    /* Dump options */
    file_put(fff, "  [Options]\n\n");

    /* Dump options */
    for (i = 0; i < OP_MAX; i++)
    {
        int opt;
        const char *title = "";

        switch (i)
        {
            case OP_INTERFACE: title = "User interface"; break;
            case OP_MANGBAND: title = "MAngband"; break;
            case OP_BIRTH: title = "Birth"; break;
        }

        file_putf(fff, "  [%s]\n\n", title);
        for (opt = 0; opt < OPT_MAX; opt++)
        {
            if (option_type(opt) != i) continue;

            /* Hack -- only display server options */
            if (!option_server(opt)) continue;

            file_putf(fff, "%-45s: %s (%s)\n", option_desc(opt), p->opts.opt[opt]? "yes": "no ",
                option_name(opt));
        }

        /* Skip some lines */
        file_put(fff, "\n");
    }

    /* Dump the scene of death */
    if (final)
    {
        if (victory) file_put(fff, "\n  [Scene of Victory]\n\n");
        else file_put(fff, "\n  [Scene of Death]\n\n");

        /* Get an in bounds area */
        x1 = p->grid.x - 39;
        x2 = p->grid.x + 39;
        y1 = p->grid.y - 10;
        y2 = p->grid.y + 10;
        if (y1 < 0)
        {
            y2 = y2 - y1;
            y1 = 0;
        }
        if (x1 < 0)
        {
            x2 = x2 - x1;
            x1 = 0;
        }
        if (y2 > cv->height - 1)
        {
            y1 = y1 - (y2 - (cv->height - 1));
            y2 = cv->height - 1;
        }
        if (x2 > cv->width - 1)
        {
            x1 = x1 - (x2 - (cv->width - 1));
            x2 = cv->width - 1;
        }

        /* Describe each row */
        for (grid.y = y1; grid.y <= y2; grid.y++)
        {
            for (grid.x = x1; grid.x <= x2; grid.x++)
            {
                /* Get the features */
                map_info(p, cv, &grid, &g);
                grid_data_as_text(p, cv, true, &g, &a, &c, &a, &c);

                /* Hack for the player who is already dead and gone */
                if (player_is_at(p, &grid))
                {
                    c = (victory? '@': '†');
                    a = COLOUR_WHITE;
                }

                /* Translate the attr */
                attr = color_attr_to_char(a % MAX_COLORS);

                /* Config file controls if we output with color codes */
                if (cfg_chardump_color)
                {
                    /* Output with attr colour code */
                    file_putf(fff, "%c%c", attr, c);
                }
                else
                {
                    /* Output plain ASCII */
                    file_putf(fff, "%c", c);
                }
            }
            file_put(fff, "\n");
        }

        /* Prepare status line */
        update_statusline(p);

        /* Dump status line */
        file_putf(fff, "\n%s\n\n", buffer_line(0));
    }

    mem_free(home_list);
}


/*
 * Write a character dump
 * This is for server-side character dumps
 */
bool dump_save(struct player *p, const char *path)
{
    char buf[MSG_LEN];

    /* Build the filename */
    if (p->ladder)
        path_build(buf, sizeof(buf), ANGBAND_DIR_USER, path);
    else
        path_build(buf, sizeof(buf), ANGBAND_DIR_SCORES, path);

    if (text_lines_to_file(buf, write_character_dump, (void *)p))
    {
        plog_fmt("Failed to create file %s.new", buf);
        return false;
    }

    return true;
}
