/*
 * File: gen-chunk.c
 * Purpose: Handling of chunks of cave
 *
 * Copyright (c) 2014 Nick McConnell
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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


/* List of pointers to saved chunks */
static struct chunk **chunk_list = NULL;


/* How many players are at each depth */
static s16b *players_on_depth = NULL;


void chunk_list_new(void)
{
    chunk_list = mem_zalloc((z_info->max_depth + MAX_WILD) * sizeof(struct chunk *));
    players_on_depth = mem_zalloc((z_info->max_depth + MAX_WILD) * sizeof(s16b));
}


void chunk_list_free(void)
{
    mem_free(chunk_list);
    chunk_list = NULL;
    mem_free(players_on_depth);
    players_on_depth = NULL;
}


/*
 * Add an entry to the chunk list.
 *
 * c the chunk being added to the list
 */
void chunk_list_add(struct chunk *c)
{
    /* Depth MUST be valid */
    my_assert((c->depth >= 0 - MAX_WILD) && (c->depth < z_info->max_depth));

    /* Paranoia */
    my_assert(chunk_list);

    chunk_list[c->depth + MAX_WILD] = c;
}


/*
 * Remove an entry from the chunk list.
 *
 * depth the depth of the chunk being removed from the list
 */
void chunk_list_remove(s16b depth)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < z_info->max_depth));

    /* Paranoia */
    my_assert(chunk_list);

    chunk_list[depth + MAX_WILD] = NULL;
}


/*
 * Get an entry from the chunk list.
 */
struct chunk *chunk_get(s16b depth)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < z_info->max_depth));

    /* Paranoia */
    if (!chunk_list) return NULL;

    return chunk_list[depth + MAX_WILD];
}


/*
 * Validate that the chunk contains no NULL objects.
 * Only checks for nonzero tval.
 *
 * c is the chunk to validate.
 */
void chunk_validate_objects(struct chunk *c)
{
    int x, y;
    struct object *obj;

    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            for (obj = square_object(c, y, x); obj; obj = obj->next) {my_assert(obj->tval != 0);}
            if (c->squares[y][x].mon > 0)
            {
                struct monster *mon = square_monster(c, y, x);

                if (mon->held_obj)
                {
                    for (obj = mon->held_obj; obj; obj = obj->next) {my_assert(obj->tval != 0);}
                }
            }
        }
    }
}


bool chunk_inhibit_players(s16b depth)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < z_info->max_depth));

    /* Paranoia */
    my_assert(players_on_depth);

    return (players_on_depth[depth + MAX_WILD] == INHIBIT_DEPTH);
}


void chunk_decrease_player_count(s16b depth)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < z_info->max_depth));

    /* Paranoia */
    my_assert(players_on_depth);

    if (players_on_depth[depth + MAX_WILD]) players_on_depth[depth + MAX_WILD]--;
}


void chunk_set_player_count(s16b depth, s16b value)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < z_info->max_depth));

    /* Paranoia */
    my_assert(players_on_depth);

    players_on_depth[depth + MAX_WILD] = value;
}


void chunk_increase_player_count(s16b depth)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < z_info->max_depth));

    /* Paranoia */
    my_assert(players_on_depth);

    players_on_depth[depth + MAX_WILD]++;
}


bool chunk_has_players(s16b depth)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < z_info->max_depth));

    /* Paranoia */
    my_assert(players_on_depth);

    /* Note that there is actually 1 player on the level (the DM) when INHIBIT_DEPTH is set */
    return (players_on_depth[depth + MAX_WILD] != 0);
}


s16b chunk_get_player_count(s16b depth)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < z_info->max_depth));

    /* Paranoia */
    my_assert(players_on_depth);

    return players_on_depth[depth + MAX_WILD];
}
