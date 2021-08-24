/*
 * File: z-type.c
 * Purpose: Helper classes for the display of typed data
 *
 * Copyright (c) 2007 Angband Developers
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
 * Determine if a grid location is the (0, 0) location
 */
bool loc_is_zero(struct loc *grid)
{
    return ((grid->x == 0) && (grid->y == 0));
}


void loc_init(struct loc *grid, int x, int y)
{
    grid->x = x;
    grid->y = y;
}


/*
 * Set one grid location equal to another
 */
void loc_copy(struct loc *dest, struct loc *src)
{
    dest->x = src->x;
    dest->y = src->y;
}


/*
 * Determine if two grid locations are equal
 */
bool loc_eq(struct loc *grid1, struct loc *grid2)
{
    return ((grid1->x == grid2->x) && (grid1->y == grid2->y));
}


/*
 * Sum two grid locations
 */
void loc_sum(struct loc *sum, struct loc *grid1, struct loc *grid2)
{
    loc_init(sum, grid1->x + grid2->x, grid1->y + grid2->y);
}


/*
 * Take the difference of two grid locations
 */
void loc_diff(struct loc *sum, struct loc *grid1, struct loc *grid2)
{
    loc_init(sum, grid1->x - grid2->x, grid1->y - grid2->y);
}


/*
 * Get a random location with the given x and y centres and spread
 */
void rand_loc(struct loc *rand, struct loc *grid, int x_spread, int y_spread)
{
    rand->x = rand_spread(grid->x, x_spread);
    rand->y = rand_spread(grid->y, y_spread);
}


/*
 * Location iterator functions
 */


/*
 * Initializes a location iterator
 */
void loc_iterator_first(struct loc_iterator *iter, struct loc *begin, struct loc *end)
{
    loc_copy(&iter->begin, begin);
    loc_copy(&iter->end, end);
    loc_copy(&iter->cur, begin);
}


/*
 * Advances a location iterator
 *
 * This is equivalent to the following loop:
 *
 * for (cur.y = begin.y; cur.y <= end.y; cur.y++)
 *   for (cur.x = begin.x; cur.x <= end.x; cur.x++)
 */
bool loc_iterator_next(struct loc_iterator *iter)
{
    iter->cur.x++;
    if (iter->cur.x == iter->end.x + 1)
    {
        iter->cur.x = iter->begin.x;
        iter->cur.y++;
        if (iter->cur.y == iter->end.y + 1) return false;
    }
    return true;
}


/*
 * Advances a location iterator
 *
 * This is equivalent to the following loop:
 *
 * for (cur.y = begin.y; cur.y < end.y; cur.y++)
 *   for (cur.x = begin.x; cur.x < end.x; cur.x++)
 */
bool loc_iterator_next_strict(struct loc_iterator *iter)
{
    iter->cur.x++;
    if (iter->cur.x == iter->end.x)
    {
        iter->cur.x = iter->begin.x;
        iter->cur.y++;
        if (iter->cur.y == iter->end.y) return false;
    }
    return true;
}


/*
 * Determine if a grid is between two grids
 */
bool loc_between(struct loc *grid, struct loc *grid1, struct loc *grid2)
{
    return ((grid->x >= grid1->x) && (grid->x <= grid2->x) &&
        (grid->y >= grid1->y) && (grid->y <= grid2->y));
}


bool wpos_null(struct worldpos *wpos)
{
    return (loc_is_zero(&wpos->grid) && (wpos->depth == 0));
}


void wpos_init(struct worldpos *wpos, struct loc *grid, int depth)
{
    loc_copy(&wpos->grid, grid);
    wpos->depth = depth;
}


bool wpos_eq(struct worldpos *wpos1, struct worldpos *wpos2)
{
    return (loc_eq(&wpos1->grid, &wpos2->grid) && (wpos1->depth == wpos2->depth));
}


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
void add_to_point_set(struct point_set *ps, void *data, struct loc *grid)
{
    ps->pts[ps->n].data = data;
    loc_copy(&ps->pts[ps->n].grid, grid);
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


int point_set_contains(struct point_set *ps, struct loc *grid)
{
    int i;

    for (i = 0; i < ps->n; i++)
    {
        if (loc_eq(&ps->pts[i].grid, grid)) return 1;
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
