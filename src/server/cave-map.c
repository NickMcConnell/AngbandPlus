/*
 * File: cave-map.c
 * Purpose: Lighting and map management functions
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
 * This function takes a grid location and extracts information the
 * player is allowed to know about it, filling in the grid_data structure
 * passed in 'g'.
 *
 * The information filled in is as follows:
 *  - g->f_idx is filled in with the terrain's feature type, or FEAT_NONE
 *    if the player doesn't know anything about the grid.  The function
 *    makes use of the "mimic" field in terrain in order to allow one
 *    feature to look like another (hiding secret doors, invisible traps,
 *    etc).  This will return the terrain type the player "Knows" about,
 *    not necessarily the real terrain.
 *  - g->m_idx is set to the monster index, or 0 if there is none (or the
 *    player doesn't know it).
 *  - g->first_obj is set to the first object in a grid
 *    that the player knows about, or NULL for no objects.
 *  - g->multiple_objects is true if there is more than one object in the
 *    grid that the player knows and cares about (to facilitate any special
 *    floor stack symbol that might be used).
 *  - g->in_view is true if the player can currently see the grid - this can
 *    be used to indicate field-of-view.
 *  - g->lighting is set to indicate the lighting level for the grid:
 *    LIGHTING_DARK for unlit grids, LIGHTING_LIT for inherently light
 *    grids (lit rooms, etc), LIGHTING_TORCH for grids lit by the player's
 *    light source, and LIGHTING_LOS for grids in the player's line of sight.
 *    Note that lighting is always LIGHTING_LIT for known "interesting" grids
 *    like walls.
 *  - g->is_player is true if the player is on the given grid.
 *  - g->hallucinate is true if the player is hallucinating something "strange"
 *    for this grid - this should pick a random monster to show if the m_idx
 *    is non-zero, and a random object if first_obj is not NULL.
 *
 * NOTES:
 * This is called pretty frequently, whenever a grid on the map display
 * needs updating, so don't overcomplicate it.
 *
 * Terrain is remembered separately from objects and monsters, so can be
 * shown even when the player can't "see" it.  This leads to things like
 * doors out of the player's view still change from closed to open and so on.
 */
void map_info(struct player *p, struct chunk *c, struct loc *grid, struct grid_data *g)
{
    struct object *obj;
    struct player *q;

    my_assert(grid->x < c->width);
    my_assert(grid->y < c->height);

    /* Default "clear" values, others will be set later where appropriate. */
    g->first_obj = NULL;
    g->multiple_objects = false;
    g->lighting = LIGHTING_LIT;
    g->unseen_object = false;
    g->unseen_money = false;

    q = player_get(0 - square(c, grid)->mon);

    g->in_view = square_isseen(p, grid);
    g->is_player = ((q == p)? true: false);
    g->m_idx = ((g->is_player)? 0: square(c, grid)->mon);
    g->hallucinate = (p->timed[TMD_IMAGE]? true: false);

    if (g->in_view)
    {
        g->lighting = LIGHTING_LOS;

        /* Torchlight */
        if (!square_isglow(c, grid))
        {
            if (OPT(p, view_yellow_light)) g->lighting = LIGHTING_TORCH;
        }

        /* Remember seen grid */
        square_memorize(p, c, grid);
        square_know_pile(p, c, grid);
        square_memorize_trap(p, c, grid);
    }

    /* Use known feature */
    g->f_idx = square_apparent_feat(p, c, grid);

    /* Use known trap */
    g->trap = square_known_trap(p, c, grid);

    /* Objects */
    for (obj = square_known_pile(p, c, grid); obj; obj = obj->next)
    {
        if (obj->kind == unknown_gold_kind)
            g->unseen_money = true;
        else if (obj->kind == unknown_item_kind)
            g->unseen_object = true;
        else if (ignore_item_ok(p, obj))
        {
            /* Item stays hidden */
        }
        else if (!g->first_obj)
            g->first_obj = obj;
        else
        {
            g->multiple_objects = true;
            break;
        }
    }

    /* Monsters */
    if (g->m_idx > 0)
    {
        /* If the monster isn't "visible", make sure we don't list it.*/
        if (!monster_is_visible(p, g->m_idx)) g->m_idx = 0;
    }

    /* Players */
    else if (g->m_idx < 0)
    {
        /* If the player isn't "visible", make sure we don't list it.*/
        if (!player_is_visible(p, 0 - g->m_idx)) g->m_idx = 0;
    }

    /* Rare random hallucination on non-permanent walls */
    if (g->hallucinate && !g->m_idx && !g->first_obj)
    {
        if (one_in_(128) && !feat_isperm(g->f_idx)) g->m_idx = 1;

        /* If hallucinating, we just need (the unused) first_obj to not be NULL */
        else if (one_in_(128) && !feat_isperm(g->f_idx)) g->first_obj = (struct object *)1;

        else g->hallucinate = false;
    }

    my_assert(g->f_idx < z_info->f_max);
    if (!g->hallucinate) {my_assert((int)g->m_idx < c->mon_max);}
}


/*
 * Memorize interesting viewable object/features in the given grid
 *
 * This function should only be called on "legal" grids.
 *
 * This function will memorize the object and/or feature in the given grid,
 * if they are (1) see-able and (2) interesting.  Note that all objects are
 * interesting, all terrain features except floors (and invisible traps) are
 * interesting, and floors (and invisible traps) are interesting sometimes
 * (depending on various options involving the illumination of floor grids).
 *
 * The automatic memorization of all objects and non-floor terrain features
 * as soon as they are displayed allows incredible amounts of optimization
 * in various places, especially "map_info()" and this function itself.
 *
 * Note that the memorization of objects is completely separate from the
 * memorization of terrain features, preventing annoying floor memorization
 * when a detected object is picked up from a dark floor, and object
 * memorization when an object is dropped into a floor grid which is
 * memorized but out-of-sight.
 *
 * This function should be called every time the "memorization" of a grid
 * (or the object in a grid) is called into question, such as when an object
 * is created in a grid, when a terrain feature "changes" from "floor" to
 * "non-floor", and when any grid becomes "see-able" for any reason.
 *
 * This function is called primarily from the "update_view()" function, for
 * each grid which becomes newly "see-able".
 */
void square_note_spot_aux(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Require "seen" flag and the current level */
    if (!wpos_eq(&p->wpos, &c->wpos)) return;
    if (!square_isseen(p, grid))
    {
        /* PWMAngband: redraw changes occured on floor items */
        if (player_is_at(p, grid))
        {
            square_know_pile(p, c, grid);
            p->upkeep->redraw |= (PR_ITEMLIST | PR_FLOOR);
        }

        return;
    }

    /* Make the player know precisely what is on this grid */
    square_know_pile(p, c, grid);

    /* Notice traps */
    if (square_issecrettrap(c, grid))
        square_reveal_trap(p, grid, false, true);

    /* Redraw */
    p->upkeep->redraw |= PR_ITEMLIST;

    /* Memorize this grid */
    square_memorize(p, c, grid);
    square_memorize_trap(p, c, grid);
}


void square_note_spot(struct chunk *c, struct loc *grid)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Memorize interesting viewable object/features in the given grid for that player */
        square_note_spot_aux(p, c, grid);
    }
}


/*
 * Redraw (on the screen) a given map location
 *
 * This function should only be called on "legal" grids.
 */
void square_light_spot_aux(struct player *p, struct chunk *cv, struct loc *grid)
{
    struct loc disp;

    /* Paranoia (to avoid division by zero) */
    if (!p->tile_hgt || !p->tile_wid) return;

    /* Redraw if on screen */
    if (panel_contains(p, grid))
    {
        u16b a, ta;
        char c, tc;
        struct grid_data g;

        /* Examine the grid */
        map_info(p, cv, grid, &g);
        grid_data_as_text(p, cv, false, &g, &a, &c, &ta, &tc);

        loc_init(&disp, grid->x - p->offset_grid.x, grid->y - p->offset_grid.y + 1);

        /* Only draw if different than buffered */
        if ((p->scr_info[disp.y][disp.x].c != c) || (p->scr_info[disp.y][disp.x].a != a) ||
            (p->trn_info[disp.y][disp.x].a != ta) || (p->trn_info[disp.y][disp.x].c != tc) ||
            player_is_at(p, grid))
        {
            /* Modify internal buffer */
            p->scr_info[disp.y][disp.x].c = c;
            p->scr_info[disp.y][disp.x].a = a;
            p->trn_info[disp.y][disp.x].c = tc;
            p->trn_info[disp.y][disp.x].a = ta;

            /* Tell client to redraw this grid */
            Send_char(p, &disp, a, c, ta, tc);
        }
    }
}


void square_light_spot(struct chunk *c, struct loc *grid)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!wpos_eq(&p->wpos, &c->wpos)) continue;

        /* Actually light that spot for that player */
        square_light_spot_aux(p, c, grid);
    }
}


/*
 * This routine will Perma-Light all grids in the set passed in.
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_light(struct player *p, struct chunk *c, struct point_set *ps)
{
    int i;

    /* Apply flag changes */
    for (i = 0; i < ps->n; i++)
    {
        /* Paranoia */
        if (!square_in_bounds(c, &ps->pts[i].grid)) continue;

        /* Perma-Light */
        sqinfo_on(square(c, &ps->pts[i].grid)->info, SQUARE_GLOW);
    }

    /* Process the grids */
    for (i = 0; i < ps->n; i++)
    {
        /* Paranoia */
        if (!square_in_bounds(c, &ps->pts[i].grid)) continue;

        /* Redraw the grid */
        square_light_spot(c, &ps->pts[i].grid);

        /* Process affected monsters */
        if (square(c, &ps->pts[i].grid)->mon > 0)
        {
            int chance = 25;
            struct monster *mon = square_monster(c, &ps->pts[i].grid);

            /* Stupid monsters rarely wake up */
            if (monster_is_stupid(mon->race)) chance = 10;

            /* Smart monsters always wake up */
            if (monster_is_smart(mon)) chance = 100;

            /* Sometimes monsters wake up, and become aware if they do */
            if (mon->m_timed[MON_TMD_SLEEP] && magik(chance))
                monster_wake(p, mon, true, 100);
        }
    }
}


/*
 * This routine will "darken" all grids in the set passed in.
 *
 * In addition, some of these grids will be "unmarked".
 */
static void cave_unlight(struct chunk *c, struct point_set *ps)
{
    int i, j;

    /* Check everyone */
    for (j = 1; j <= NumPlayers; j++)
    {
        struct player *p = player_get(j);

        /* If he's not here, skip him */
        if (!wpos_eq(&p->wpos, &c->wpos)) continue;

        /* Apply flag changes */
        for (i = 0; i < ps->n; i++)
        {
            struct loc grid = ps->pts[i].grid;

            /* Darken the grid */
            if (!square_isbright(c, &grid))
            {
                sqinfo_off(square(c, &grid)->info, SQUARE_GLOW);

                /* ...but dark-loving characters remember them */
                if (player_has(p, PF_UNLIGHT))
                    square_memorize(p, c, &grid);
            }

            /* Hack -- forget "boring" grids */
            if (square_isview(p, &grid) && !square_isnormal(c, &grid))
                square_forget(p, &grid);
        }

        /* Process the grids */
        for (i = 0; i < ps->n; i++)
        {
            /* Redraw the grid */
            square_light_spot_aux(p, c, &ps->pts[i].grid);
        }
    }
}


/*
 * Aux function -- see below
 */
static void cave_room_aux(struct chunk *c, struct point_set *seen, struct loc *grid)
{
    if (point_set_contains(seen, grid)) return;
    if (!square_in_bounds(c, grid)) return;
    if (!square_isroom(c, grid)) return;

    /* Add it to the "seen" set */
    add_to_point_set(seen, NULL, grid);
}


void light_room(struct player *p, struct chunk *c, struct loc *grid, bool light)
{
    int i, d;
    struct point_set *ps;

    ps = point_set_new(200);

    /* Add the initial grid */
    cave_room_aux(c, ps, grid);

    /* While grids are in the queue, add their neighbors */
    for (i = 0; i < ps->n; i++)
    {
        /* Walls get lit, but stop light */
        if (!square_isprojectable(c, &ps->pts[i].grid)) continue;

        /* Spread to the adjacent grids */
        for (d = 0; d < 8; d++)
        {
            struct loc adjacent;

            loc_sum(&adjacent, &ps->pts[i].grid, &ddgrid_ddd[d]);
            cave_room_aux(c, ps, &adjacent);
        }
    }

    /* Now, lighten or darken them all at once */
    if (light) cave_light(p, c, ps);
    else cave_unlight(c, ps);

    point_set_dispose(ps);

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* If he's not here, skip him */
        if (!wpos_eq(&q->wpos, &p->wpos)) continue;

        /* Fully update the visuals */
        q->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

        /* Update stuff */
        update_stuff(q, c);
    }
}


/*
 * Light up the dungeon using "clairvoyance"
 *
 * This function "illuminates" every grid in the dungeon, memorizes all
 * "objects", and memorizes all grids as with magic mapping.
 */
void wiz_light(struct player *p, struct chunk *c, bool full)
{
    int i;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Hack -- DM has full detection */
    if (p->dm_flags & DM_SEE_LEVEL) full = true;

    loc_init(&begin, 1, 1);
    loc_init(&end, c->width - 1, c->height - 1);
    loc_iterator_first(&iter, &begin, &end);

    /* Scan all grids */
    do
    {
        /* Process all non-walls */
        if (!square_seemslikewall(c, &iter.cur))
        {
            /* Perma-light the grid */
            sqinfo_on(square(c, &iter.cur)->info, SQUARE_GLOW);

            /* Memorize normal features, mark grids as processed */
            if (square_isnormal(c, &iter.cur))
            {
                square_memorize(p, c, &iter.cur);
                square_mark(p, &iter.cur);
            }

            /* Memorize known walls */
            for (i = 0; i < 8; i++)
            {
                struct loc a_grid;

                loc_sum(&a_grid, &iter.cur, &ddgrid_ddd[i]);

                /* Memorize walls (etc), mark grids as processed */
                if (square_seemslikewall(c, &a_grid))
                {
                    square_memorize(p, c, &a_grid);
                    square_mark(p, &a_grid);
                }
            }
        }

        /* Memorize objects */
        if (full) square_know_pile(p, c, &iter.cur);
        else square_sense_pile(p, c, &iter.cur);

        /* Forget unprocessed, unknown grids in the mapping area */
        if (!square_ismark(p, &iter.cur) && square_isnotknown(p, c, &iter.cur))
            square_forget(p, &iter.cur);
    }
    while (loc_iterator_next_strict(&iter));

    loc_init(&begin, 0, 0);
    loc_init(&end, c->width, c->height);
    loc_iterator_first(&iter, &begin, &end);

    /* Unmark grids */
    do
    {
        square_unmark(p, &iter.cur);
    }
    while (loc_iterator_next_strict(&iter));

    /* Fully update the visuals */
    p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    p->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);
}


/*
 * Completely darken the level, and know all objects
 *
 * This function darkens every grid in the dungeon, memorizes all
 * "objects" (or notes the existence of an object "if" full is true),
 * and memorizes all grids as with magic mapping.
 */
void wiz_dark(struct player *p, struct chunk *c, bool full)
{
    int i;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Hack -- DM has full detection */
    if (p->dm_flags & DM_SEE_LEVEL) full = true;

    loc_init(&begin, 1, 1);
    loc_init(&end, c->width - 1, c->height - 1);
    loc_iterator_first(&iter, &begin, &end);

    /* Scan all grids */
    do
    {
        /* Process all non-walls */
        if (!square_seemslikewall(c, &iter.cur))
        {
            /* PWMAngband: unlight the grid */
            square_unglow(c, &iter.cur);

            /* Memorize normal features, mark grids as processed */
            if (square_isnormal(c, &iter.cur))
            {
                square_memorize(p, c, &iter.cur);
                square_mark(p, &iter.cur);
            }

            /* Memorize known walls */
            for (i = 0; i < 8; i++)
            {
                struct loc a_grid;

                loc_sum(&a_grid, &iter.cur, &ddgrid_ddd[i]);

                /* Memorize walls (etc), mark grids as processed */
                if (square_seemslikewall(c, &a_grid))
                {
                    square_memorize(p, c, &a_grid);
                    square_mark(p, &a_grid);
                }
            }
        }

        /* Memorize objects */
        if (full) square_know_pile(p, c, &iter.cur);
        else square_sense_pile(p, c, &iter.cur);

        /* Forget unprocessed, unknown grids in the mapping area */
        if (!square_ismark(p, &iter.cur) && square_isnotknown(p, c, &iter.cur))
            square_forget(p, &iter.cur);
    }
    while (loc_iterator_next_strict(&iter));

    loc_init(&begin, 0, 0);
    loc_init(&end, c->width, c->height);
    loc_iterator_first(&iter, &begin, &end);

    /* Unmark grids */
    do
    {
        square_unmark(p, &iter.cur);
    }
    while (loc_iterator_next_strict(&iter));

    /* Fully update the visuals */
    p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    p->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);
}


/*
 * Light or darken the towns.
 * Also applied for wilderness and special levels.
 */
void cave_illuminate(struct player *p, struct chunk *c, bool daytime)
{
    int i;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Not on random levels */
    if (random_level(&c->wpos)) return;

    /* Make sure we're not in a store */
    if (p && in_store(p)) return;

    loc_init(&begin, 0, 0);
    loc_init(&end, c->width, c->height);
    loc_iterator_first(&iter, &begin, &end);

    /* Apply light or darkness */
    do
    {
        bool light = true;

        if (square_ispermstatic(c, &iter.cur) || square_isbright(c, &iter.cur))
        {
            int d;

            light = false;

            /* Skip static dungeon town walls/lava squares with no surrounding floors or stairs */
            for (d = 0; d < 9; d++)
            {
                /* Extract adjacent (legal) location */
                struct loc a_grid;

                loc_sum(&a_grid, &iter.cur, &ddgrid_ddd[d]);

                /* Paranoia */
                if (!square_in_bounds_fully(c, &a_grid)) continue;

                /* Test */
                if (square_isanyfloor(c, &a_grid) || square_isstairs(c, &a_grid))
                    light = true;
            }
        }

        /* Only interesting grids at night */
        square_illuminate(p, c, &iter.cur, daytime, light);
    }
    while (loc_iterator_next_strict(&iter));

    loc_iterator_first(&iter, &begin, &end);

    /* Light shop doorways */
    do
    {
        if (!square_isshop(c, &iter.cur)) continue;

        for (i = 0; i < 8; i++)
        {
            struct loc a_grid;

            loc_sum(&a_grid, &iter.cur, &ddgrid_ddd[i]);
            sqinfo_on(square(c, &a_grid)->info, SQUARE_GLOW);
            if (p) square_memorize(p, c, &a_grid);
        }
    }
    while (loc_iterator_next_strict(&iter));

    /* Fully update the visuals */
    if (p) p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw */
    if (p) p->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);
}


void square_forget_all(struct chunk *c, struct loc *grid)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!wpos_eq(&p->wpos, &c->wpos)) continue;

        sqinfo_off(square_p(p, grid)->info, SQUARE_SEEN);
        square_forget(p, grid);
        square_forget_trap(p, grid);
    }
}


void square_forget_pile_all(struct chunk *c, struct loc *grid)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!wpos_eq(&p->wpos, &c->wpos)) continue;

        square_forget_pile(p, grid);
    }
}
