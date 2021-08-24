/*
 * File: gen-chunk.c
 * Purpose: Handling of chunks of cave
 *
 * Copyright (c) 2014 Nick McConnell
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


#include "s-angband.h"


/*
 * Get the index of an entry in the chunk list corresponding to the given depth.
 */
static int chunk_index(struct wild_type *w_ptr, int depth)
{
    /* Paranoia */
    my_assert(w_ptr);
    my_assert(w_ptr->chunk_list);
    my_assert(depth >= 0);

    if (!depth) return 0;

    my_assert((depth >= w_ptr->min_depth) && (depth < w_ptr->max_depth));
    return (depth - w_ptr->min_depth + 1);
}


/*
 * Add an entry to the chunk list.
 *
 * c the chunk being added to the list
 */
void chunk_list_add(struct chunk *c)
{
    struct wild_type *w_ptr = get_wt_info_at(&c->wpos.grid);

    w_ptr->chunk_list[chunk_index(w_ptr, c->wpos.depth)] = c;
}


/*
 * Remove an entry from the chunk list.
 *
 * c the chunk being removed from the list
 */
void chunk_list_remove(struct chunk *c)
{
    struct wild_type *w_ptr = get_wt_info_at(&c->wpos.grid);

    w_ptr->chunk_list[chunk_index(w_ptr, c->wpos.depth)] = NULL;
}


/*
 * Validate that the chunk contains no NULL objects.
 * Only checks for nonzero tval.
 *
 * c is the chunk to validate.
 */
void chunk_validate_objects(struct chunk *c)
{
    struct loc begin, end;
    struct loc_iterator iter;
    struct object *obj;

    loc_init(&begin, 0, 0);
    loc_init(&end, c->width, c->height);
    loc_iterator_first(&iter, &begin, &end);

    do
    {
        for (obj = square_object(c, &iter.cur); obj; obj = obj->next) {my_assert(obj->tval != 0);}
        if (square(c, &iter.cur)->mon > 0)
        {
            struct monster *mon = square_monster(c, &iter.cur);

            if (mon->held_obj)
            {
                for (obj = mon->held_obj; obj; obj = obj->next) {my_assert(obj->tval != 0);}
            }
        }
    }
    while (loc_iterator_next_strict(&iter));
}


/*
 * Get an entry from the chunk list.
 */
struct chunk *chunk_get(struct worldpos *wpos)
{
    struct wild_type *w_ptr = get_wt_info_at(&wpos->grid);

    return w_ptr->chunk_list[chunk_index(w_ptr, wpos->depth)];
}


/*
 * Get the index of an entry in the players_on_depth array corresponding to the given depth.
 */
static int players_on_depth_index(struct wild_type *w_ptr, int depth)
{
    /* Paranoia */
    my_assert(w_ptr);
    my_assert(w_ptr->players_on_depth);
    my_assert(depth >= 0);

    if (!depth) return 0;

    my_assert((depth >= w_ptr->min_depth) && (depth < w_ptr->max_depth));
    return (depth - w_ptr->min_depth + 1);
}


bool chunk_inhibit_players(struct worldpos *wpos)
{
    struct wild_type *w_ptr = get_wt_info_at(&wpos->grid);

    return (w_ptr->players_on_depth[players_on_depth_index(w_ptr, wpos->depth)] == INHIBIT_DEPTH);
}


void chunk_decrease_player_count(struct worldpos *wpos)
{
    struct wild_type *w_ptr = get_wt_info_at(&wpos->grid);
    int index = players_on_depth_index(w_ptr, wpos->depth);

    if (w_ptr->players_on_depth[index]) w_ptr->players_on_depth[index]--;
}


void chunk_set_player_count(struct worldpos *wpos, s16b value)
{
    struct wild_type *w_ptr = get_wt_info_at(&wpos->grid);

    w_ptr->players_on_depth[players_on_depth_index(w_ptr, wpos->depth)] = value;
}


void chunk_increase_player_count(struct worldpos *wpos)
{
    struct wild_type *w_ptr = get_wt_info_at(&wpos->grid);

    w_ptr->players_on_depth[players_on_depth_index(w_ptr, wpos->depth)]++;
}


bool chunk_has_players(struct worldpos *wpos)
{
    struct wild_type *w_ptr = get_wt_info_at(&wpos->grid);

    /* Note that there is actually 1 player on the level (the DM) when INHIBIT_DEPTH is set */
    return (w_ptr->players_on_depth[players_on_depth_index(w_ptr, wpos->depth)] != 0);
}


s16b chunk_get_player_count(struct worldpos *wpos)
{
    struct wild_type *w_ptr = get_wt_info_at(&wpos->grid);

    return w_ptr->players_on_depth[players_on_depth_index(w_ptr, wpos->depth)];
}
