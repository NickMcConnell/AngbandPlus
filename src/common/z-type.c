/*
 * File: z-type.c
 * Purpose: Helper classes for the display of typed data
 *
 * Copyright (c) 2007 Angband Developers
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


#include "angband.h"


/*
 * Hack -- the special Angband "System Suffix"
 * This variable is used to choose an appropriate "pref-xxx" file
 */
const char *ANGBAND_SYS;


/*
 * Various directories. These are no longer necessarily all subdirs of "lib"
 */
char *ANGBAND_DIR_GAMEDATA;
char *ANGBAND_DIR_CUSTOMIZE;
char *ANGBAND_DIR_HELP;
char *ANGBAND_DIR_SCREENS;
char *ANGBAND_DIR_FONTS;
char *ANGBAND_DIR_TILES;
char *ANGBAND_DIR_SOUNDS;
char *ANGBAND_DIR_ICONS;
char *ANGBAND_DIR_USER;
char *ANGBAND_DIR_SAVE;
char *ANGBAND_DIR_SCORES;


/*
 * Utility functions to work with point_sets
 */
struct point_set *point_set_new(int initial_size)
{
    struct point_set *ps = mem_alloc(sizeof(struct point_set));

    ps->n = 0;
    ps->allocated = initial_size;
    ps->pts = mem_zalloc(sizeof(*(ps->pts)) * ps->allocated);
    return ps;
}


void point_set_dispose(struct point_set *ps)
{
    mem_free(ps->pts);
    mem_free(ps);
}


/*
 * Add the point to the given point set, making more space if there is
 * no more space left.
 */
void add_to_point_set(struct point_set *ps, void *data, int y, int x)
{
    ps->pts[ps->n].data = data;
    ps->pts[ps->n].x = x;
    ps->pts[ps->n].y = y;
    ps->n++;
    if (ps->n >= ps->allocated)
    {
        ps->allocated *= 2;
        ps->pts = mem_realloc(ps->pts, sizeof(*(ps->pts)) * ps->allocated);
    }
}


int point_set_size(struct point_set *ps)
{
    return ps->n;
}


int point_set_contains(struct point_set *ps, int y, int x)
{
    int i;

    for (i = 0; i < ps->n; i++)
    {
        if ((ps->pts[i].x == x) && (ps->pts[i].y == y)) return 1;
    }

    return 0;
}


/**** MAngband specific ****/


/* The information given to us by the server */
server_setup_t Setup;


/* The information we give to the server */
client_setup_t Client_setup;


/* Chat channels */
channel_type channels[MAX_CHANNELS];


/*
 * Structure (not array) of game constants
 */
struct angband_constants *z_info;


/*
 * Socials
 */
struct social *soc_info;
