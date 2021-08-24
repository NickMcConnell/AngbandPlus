/*
 * File: ui-death.c
 * Purpose: Handle the UI bits that happen after the character dies.
 *
 * Copyright (c) 1987 - 2007 Angband contributors
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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
 * Write formatted string `fmt` on line `y`, centred between points x1 and x2.
 */
static void put_str_centred(int y, int x1, int x2, const char *fmt, ...)
{
    va_list vp;
    char *tmp;
    size_t len;
    int x;

    /* Format into the (growable) tmp */
    va_start(vp, fmt);
    tmp = vformat(fmt, vp);
    va_end(vp);

    /* Centre now */
    len = strlen(tmp);
    x = x1 + ((x2 - x1) / 2 - len / 2);

    put_str(tmp, y, x);
}


/*
 * Display the tombstone
 */
void print_tomb(void)
{
    int i, line = 7;

    /* Clear the screen */
    Term_clear();

    for (i = 0; i < TEXTFILE__HGT; i++)
    {
        /* Show each line */
        text_out_e(&Setup.text_screen[TEXTFILE_TOMB][i * TEXTFILE__WID], i, 0);
    }

    put_str_centred(line++, 8, 8+31, "%s", player->name);
    put_str_centred(line++, 8, 8+31, "the");
    put_str_centred(line++, 8, 8+31, "%s", player->death_info.title);

    line++;

    put_str_centred(line++, 8, 8+31, "%s", player->clazz->name);
    put_str_centred(line++, 8, 8+31, "Level: %d", (int)player->death_info.lev);
    put_str_centred(line++, 8, 8+31, "Exp: %d", (int)player->death_info.exp);
    put_str_centred(line++, 8, 8+31, "AU: %d", (int)player->death_info.au);
    put_str_centred(line++, 8, 8+31, "Killed on Level %d (%d, %d)", player->death_info.wpos.depth,
        player->death_info.wpos.grid.x, player->death_info.wpos.grid.y);
    put_str_centred(line++, 8, 8+31, "by %s.", player->death_info.died_from);

    line++;

    put_str_centred(line, 8, 8+31, "on %-.24s", player->death_info.ctime);

    /* Show it */
    Term_fresh();

    /* Disable updates (this is the final screen) */
    player->screen_save_depth++;
}


/*
 * Display the winner crown
 */
void display_winner(void)
{
    int i, wid, hgt;

    Term_clear();
	Term_get_size(&wid, &hgt);

    for (i = 0; i < TEXTFILE__HGT; i++)
    {
        /* Show each line */
        text_out_e(&Setup.text_screen[TEXTFILE_CRWN][i * TEXTFILE__WID], i, 0);
    }

    put_str_centred(i, 0, wid, "All Hail the Mighty %s!", title);

    /* Show it */
    Term_fresh();

    /* Disable updates (this is the final screen) */
    player->screen_save_depth++;
}