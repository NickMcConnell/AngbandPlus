/*
 * File: cave-map.c
 * Purpose: Lighting and map management functions
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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


#include "s-angband.h"


/*
 * This function takes a grid location (x, y) and extracts information the
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
void map_info(struct player *p, struct chunk *c, unsigned y, unsigned x, struct grid_data *g)
{
    struct object *obj;
    struct player *q;

    my_assert(x < (unsigned)c->width);
    my_assert(y < (unsigned)c->height);

    /* Default "clear" values, others will be set later where appropriate. */
    g->first_obj = NULL;
    g->multiple_objects = false;
    g->lighting = LIGHTING_DARK;
    g->unseen_object = false;
    g->unseen_money = false;

    q = player_get(0 - c->squares[y][x].mon);

    g->in_view = square_isseen(p, y, x);
    g->is_player = ((q == p)? true: false);
    g->m_idx = ((g->is_player)? 0: c->squares[y][x].mon);
    g->hallucinate = (p->timed[TMD_IMAGE]? true: false);

    if (g->in_view)
    {
        g->lighting = LIGHTING_LOS;

        if (!square_isglow(c, y, x) && OPT(p, view_yellow_light))
            g->lighting = LIGHTING_TORCH;

        /* Remember seen grid */
        square_memorize(p, c, y, x);
        square_know_pile(p, c, y, x);
        square_memorize_trap(p, c, y, x);
    }
    else if (square_isknown(p, y, x) && square_isglow(c, y, x))
        g->lighting = LIGHTING_LIT;

    /* Use known feature */
    g->f_idx = square_known_feat(p, c, y, x);
    if (f_info[g->f_idx].mimic) g->f_idx = lookup_feat(f_info[g->f_idx].mimic);

    /* Use known trap */
    g->trap = square_known_trap(p, c, y, x);

    /* Objects */
    for (obj = square_known_pile(p, c, y, x); obj; obj = obj->next)
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
void square_note_spot_aux(struct player *p, struct chunk *c, int y, int x)
{
    /* Require "seen" flag and the current level */
    if (!COORDS_EQUAL(&p->wpos, &c->wpos)) return;
    if (!square_isseen(p, y, x))
    {
        /* PWMAngband: redraw changes occured on floor items */
        if (player_is_at(p, y, x))
        {
            square_know_pile(p, c, y, x);
            p->upkeep->redraw |= (PR_ITEMLIST | PR_FLOOR);
        }

        return;
    }

    /* Make the player know precisely what is on this grid */
    square_know_pile(p, c, y, x);

    /* Notice traps */
    if (square_issecrettrap(c, y, x))
        square_reveal_trap(p, y, x, false, true);

    /* Redraw */
    p->upkeep->redraw |= PR_ITEMLIST;

    /* Memorize this grid */
    square_memorize(p, c, y, x);
    square_memorize_trap(p, c, y, x);
}


void square_note_spot(struct chunk *c, int y, int x)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Memorize interesting viewable object/features in the given grid for that player */
        square_note_spot_aux(p, c, y, x);
    }
}


/*
 * Redraw (on the screen) a given map location
 *
 * This function should only be called on "legal" grids.
 */
void square_light_spot_aux(struct player *p, struct chunk *cv, int y, int x)
{
    int dispx, dispy;

    /* Paranoia (to avoid division by zero) */
    if (!p->tile_hgt || !p->tile_wid) return;

    /* Redraw if on screen */
    if (panel_contains(p, y, x))
    {
        u16b a, ta;
        char c, tc;
        struct grid_data g;

        /* Examine the grid */
        map_info(p, cv, y, x, &g);
        grid_data_as_text(p, cv, false, &g, &a, &c, &ta, &tc);

        dispx = x - p->offset_x;
        dispy = y - p->offset_y + 1;

        /* Only draw if different than buffered */
        if ((p->scr_info[dispy][dispx].c != c) || (p->scr_info[dispy][dispx].a != a) ||
            (p->trn_info[dispy][dispx].a != ta) || (p->trn_info[dispy][dispx].c != tc) ||
            player_is_at(p, y, x))
        {
            /* Modify internal buffer */
            p->scr_info[dispy][dispx].c = c;
            p->scr_info[dispy][dispx].a = a;
            p->trn_info[dispy][dispx].c = tc;
            p->trn_info[dispy][dispx].a = ta;

            /* Tell client to redraw this grid */
            Send_char(p, dispx, dispy, a, c, ta, tc);
        }
    }
}


void square_light_spot(struct chunk *c, int y, int x)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!COORDS_EQUAL(&p->wpos, &c->wpos)) continue;

        /* Actually light that spot for that player */
        square_light_spot_aux(p, c, y, x);
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
        int y = ps->pts[i].y;
        int x = ps->pts[i].x;

        /* Paranoia */
        if (!square_in_bounds(c, y, x)) continue;

        /* Perma-Light */
        sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
    }

    /* Process the grids */
    for (i = 0; i < ps->n; i++)
    {
        int y = ps->pts[i].y;
        int x = ps->pts[i].x;

        /* Paranoia */
        if (!square_in_bounds(c, y, x)) continue;

        /* Redraw the grid */
        square_light_spot(c, y, x);

        /* Process affected monsters */
        if (c->squares[y][x].mon > 0)
        {
            int chance = 25;
            struct monster *mon = square_monster(c, y, x);

            /* Stupid monsters rarely wake up */
            if (monster_is_stupid(mon->race)) chance = 10;

            /* Smart monsters always wake up */
            if (monster_is_smart(mon->race)) chance = 100;

            /* Sometimes monsters wake up */
            if (mon->m_timed[MON_TMD_SLEEP] && magik(chance))
                mon_clear_timed(p, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOTIFY);
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
        if (!COORDS_EQUAL(&p->wpos, &c->wpos)) continue;

        /* Apply flag changes */
        for (i = 0; i < ps->n; i++)
        {
            int y = ps->pts[i].y;
            int x = ps->pts[i].x;

            /* Darken the grid */
            square_unglow(c, y, x);

            /* Hack -- forget "boring" grids */
            if (square_isview(p, y, x) && !square_isnormal(c, y, x))
                square_forget(p, y, x);
        }

        /* Process the grids */
        for (i = 0; i < ps->n; i++)
        {
            int y = ps->pts[i].y;
            int x = ps->pts[i].x;

            /* Redraw the grid */
            square_light_spot_aux(p, c, y, x);
        }
    }
}


/*
 * Aux function -- see below
 */
static void cave_room_aux(struct chunk *c, struct point_set *seen, int y, int x)
{
    if (point_set_contains(seen, y, x)) return;
    if (!square_in_bounds(c, y, x)) return;
    if (!square_isroom(c, y, x)) return;

    /* Add it to the "seen" set */
    add_to_point_set(seen, NULL, y, x);
}


void light_room(struct player *p, struct chunk *c, int y1, int x1, bool light)
{
    int i, x, y;
    struct point_set *ps;

    ps = point_set_new(200);

    /* Add the initial grid */
    cave_room_aux(c, ps, y1, x1);

    /* While grids are in the queue, add their neighbors */
    for (i = 0; i < ps->n; i++)
    {
        x = ps->pts[i].x, y = ps->pts[i].y;

        /* Walls get lit, but stop light */
        if (!square_isprojectable(c, y, x)) continue;

        /* Spread adjacent */
        cave_room_aux(c, ps, y + 1, x);
        cave_room_aux(c, ps, y - 1, x);
        cave_room_aux(c, ps, y, x + 1);
        cave_room_aux(c, ps, y, x - 1);

        /* Spread diagonal */
        cave_room_aux(c, ps, y + 1, x + 1);
        cave_room_aux(c, ps, y - 1, x - 1);
        cave_room_aux(c, ps, y - 1, x + 1);
        cave_room_aux(c, ps, y + 1, x - 1);
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
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

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
    int i, y, x;

    /* Hack -- DM has full detection */
    if (p->dm_flags & DM_SEE_LEVEL) full = true;

    /* Scan all grids */
    for (y = 1; y < c->height - 1; y++)
    {
        for (x = 1; x < c->width - 1; x++)
        {
            /* Process all non-walls */
            if (!square_seemslikewall(c, y, x))
            {
                /* Scan all neighbors */
                for (i = 0; i < 9; i++)
                {
                    int yy = y + ddy_ddd[i];
                    int xx = x + ddx_ddd[i];

                    /* Perma-light the grid */
                    sqinfo_on(c->squares[yy][xx].info, SQUARE_GLOW);

                    /* Memorize normal features, mark grids as processed */
                    if (square_isnormal(c, yy, xx))
                    {
                        square_memorize(p, c, yy, xx);
                        square_mark(p, yy, xx);
                    }
                }
            }

            /* Memorize objects */
            if (full) square_know_pile(p, c, y, x);
            else square_sense_pile(p, c, y, x);

            /* Forget unprocessed, unknown grids in the mapping area */
            if (!square_ismark(p, y, x) && square_isnotknown(p, c, y, x))
                square_forget(p, y, x);
        }
    }

    /* Unmark grids */
    for (y = 0; y < c->height; y++)
        for (x = 0; x < c->width; x++)
            square_unmark(p, y, x);

    /* Fully update the visuals */
    p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    p->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);
}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void wiz_dark(struct player *p)
{
    int y, x;
    struct chunk *c = chunk_get(&p->wpos);

    /* Forget every grid */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            /* Process the grid */
            square_forget(p, y, x);
            square_forget_trap(p, y, x);
            sqinfo_off(p->cave->squares[y][x].info, SQUARE_DTRAP);

            /* PWMAngband: unlight all permalit grids */
            square_unglow(c, y, x);

            /* Forget all objects */
            square_forget_pile(p, y, x);
        }
    }

    /* Memorize the content of owned houses */
    memorize_houses(p);

    /* Fully update the visuals */
    p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw */
    p->upkeep->redraw |= (PR_MAP | PR_DTRAP | PR_MONLIST | PR_ITEMLIST);
}


/*
 * Light or darken the towns.
 * Also applied for wilderness and special levels.
 */
void cave_illuminate(struct player *p, struct chunk *c, bool daytime)
{
    int y, x, i;

    /* Not on random levels */
    if (random_level(&c->wpos)) return;

    /* Make sure we're not in a store */
    if (p && in_store(p)) return;

    /* Apply light or darkness */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            if (square_ispermstatic(c, y, x) || square_isbright(c, y, x))
            {
                int d;
                bool light = false;

                /* Skip static dungeon town walls/lava squares with no surrounding floors or stairs */
                for (d = 0; d < 9; d++)
                {
                    /* Extract adjacent (legal) location */
                    int yy = y + ddy_ddd[d];
                    int xx = x + ddx_ddd[d];

                    /* Paranoia */
                    if (!square_in_bounds_fully(c, yy, xx)) continue;

                    /* Test */
                    if (square_isanyfloor(c, yy, xx) || square_isstairs(c, yy, xx))
                        light = true;
                }

                if (!light) continue;
            }

            /* Only interesting grids at night */
            square_illuminate(p, c, y, x, daytime);
        }
    }

    /* Light shop doorways */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            if (!square_isshop(c, y, x)) continue;

            for (i = 0; i < 8; i++)
            {
                int yy = y + ddy_ddd[i];
                int xx = x + ddx_ddd[i];

                sqinfo_on(c->squares[yy][xx].info, SQUARE_GLOW);
                if (p) square_memorize(p, c, yy, xx);
            }
        }
    }

    /* Fully update the visuals */
    if (p) p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw */
    if (p) p->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);
}


void square_forget_all(struct chunk *c, int y, int x)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!COORDS_EQUAL(&p->wpos, &c->wpos)) continue;

        sqinfo_off(p->cave->squares[y][x].info, SQUARE_SEEN);
        square_forget(p, y, x);
        square_forget_trap(p, y, x);
    }
}


void square_forget_pile_all(struct chunk *c, int y, int x)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!COORDS_EQUAL(&p->wpos, &c->wpos)) continue;

        square_forget_pile(p, y, x);
    }
}
