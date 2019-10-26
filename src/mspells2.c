/* File: mspells2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Monster spells (attack monster) */

#include "angband.h"

/*
 * Determine if a beam spell will hit the target.
 */
bool direct_beam(int y1, int x1, int y2, int x2, monster_type *m_ptr)
{
    bool hit2 = FALSE;
    int i;
    point_t p1 = point_create(x1, y1);
    point_t p2 = point_create(x2, y2);

    int path_n = 0;
    point_t path[512];

    bool friend = is_pet(m_ptr);

    /* Check the projection path */
    path_n = project_path(path, MAX_RANGE, p1, p2, PROJECT_THRU);

    /* No grid is ever projectable from itself */
    if (!path_n) return FALSE;

    for (i = 0; i < path_n; i++)
    {
        point_t p = path[i];
        mon_ptr mon = mon_at(p);

        if (point_equals(p, p2))
            hit2 = TRUE;
        else if (friend && mon && !are_enemies(m_ptr, mon))
        {
            /* Friends don't shoot friends */
            return FALSE;
        }

        if (friend && plr_at(p))
            return FALSE;
    }
    if (!hit2)
        return FALSE;
    return TRUE;
}

bool breath_direct(int y1, int x1, int y2, int x2, int rad, int typ, bool friend)
{
    /* Must be the same as projectable() */

    int i;

    /* Initial grid */
    point_t p1 = point_create(x1, y1);
    point_t p2 = point_create(x2, y2);
    point_t cur = p1;

    int path_n = 0;
    point_t path[512];

    int grids = 0;
    point_t gp[1024];
    byte gm[32];
    int gm_rad = rad;

    bool hit2 = FALSE;
    bool hityou = FALSE;

    int flg;

    switch (typ)
    {
    case GF_LITE:
    case GF_LITE_WEAK:
        flg = PROJECT_LOS;
        break;
    case GF_DISINTEGRATE:
        flg = PROJECT_DISI;
        break;
    default:
        flg = 0;
        break;
    }

    /* Check the projection path */
    path_n = project_path(path, MAX_RANGE, p1, p2, flg);

    /* Project along the path */
    for (i = 0; i < path_n; ++i)
    {
        point_t next = path[i];

        if (flg & PROJECT_DISI)
        {
            /* Hack -- Balls explode before reaching walls */
            if (cave_stop_disintegration(next)) break;
        }
        else if (flg & PROJECT_LOS)
        {
            /* Hack -- Balls explode before reaching walls */
            if (!cave_have_flag_at(next, FF_LOS)) break;
        }
        else
        {
            /* Hack -- Balls explode before reaching walls */
            if (!cave_have_flag_at(next, FF_PROJECT)) break;
        }

        /* Save the "blast epicenter" */
        cur = next;
    }

    path_n = i;

    if (!path_n)
    {
        if (flg & PROJECT_DISI)
        {
            if (in_disintegration_range(y1, x1, y2, x2) && (distance(y1, x1, y2, x2) <= rad)) hit2 = TRUE;
            if (in_disintegration_range(y1, x1, p_ptr->pos.y, p_ptr->pos.x) && (distance(y1, x1, p_ptr->pos.y, p_ptr->pos.x) <= rad)) hityou = TRUE;
        }
        else if (flg & PROJECT_LOS)
        {
            if (los(y1, x1, y2, x2) && (distance(y1, x1, y2, x2) <= rad)) hit2 = TRUE;
            if (los(y1, x1, p_ptr->pos.y, p_ptr->pos.x) && (distance(y1, x1, p_ptr->pos.y, p_ptr->pos.x) <= rad)) hityou = TRUE;
        }
        else
        {
            if (projectable(y1, x1, y2, x2) && (distance(y1, x1, y2, x2) <= rad)) hit2 = TRUE;
            if (projectable(y1, x1, p_ptr->pos.y, p_ptr->pos.x) && (distance(y1, x1, p_ptr->pos.y, p_ptr->pos.x) <= rad)) hityou = TRUE;
        }
    }
    else
    {
        breath_shape(path, path_n, &grids, gp, gm, &gm_rad, rad, y1, x1, cur.y, cur.x, typ);

        for (i = 0; i < grids; i++)
        {
            point_t p = gp[i];
            if (point_equals(p, p2)) hit2 = TRUE;
            if (plr_at(p)) hityou = TRUE;
        }
    }

    if (!hit2) return FALSE;
    if (friend && hityou) return FALSE;

    return TRUE;
}

/*
 * Get the actual center point of ball spells (rad > 1) (originally from TOband)
 */
void get_project_point(int sy, int sx, int *ty, int *tx, int flg)
{
    point_t path[128];
    int  path_n, i;

    path_n = project_path(path, MAX_RANGE, point_create(sx, sy), point_create(*tx, *ty), flg);

    *ty = sy;
    *tx = sx;

    /* Project along the path */
    for (i = 0; i < path_n; i++)
    {
        sy = path[i].y;
        sx = path[i].x;

        /* Hack -- Balls explode before reaching walls */
        if (!cave_have_flag_bold(sy, sx, FF_PROJECT)) break;

        *ty = sy;
        *tx = sx;
    }
}


