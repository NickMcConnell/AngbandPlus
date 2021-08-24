/*
 * File: mon-move.c
 * Purpose: Monster movement
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke (attacking code)
 * Copyright (c) 1997 Ben Harrison, David Reeve Sward, Keldon Jones (AI routines).
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


/*
 * Routines to enable decisions on monster behaviour
 */


/*
 * Find whether a PASS_WALL or KILL_WALL monster has a clear path to the player.
 *
 * This is a modified/simplified version of project_path().
 */
static bool can_path_player(struct player *p, const struct monster *mon, struct chunk *c)
{
    int y1 = mon->grid.y;
    int x1 = mon->grid.x;
    int y2 = p->grid.y;
    int x2 = p->grid.x;
    struct loc grid;

    /* Absolute */
    int ay, ax;

    /* Offsets */
    int sy, sx;

    /* Fractions */
    int frac;

    /* Scale factors */
    int full, half;

    /* Slope */
    int m;

    /* If player is in LOS, there's no need to go around walls */
    if (projectable(c, &((struct monster *)mon)->grid, &p->grid, PROJECT_NONE, false)) return true;

    /* Analyze "dy" */
    if (y2 < y1)
    {
        ay = (y1 - y2);
        sy = -1;
    }
    else
    {
        ay = (y2 - y1);
        sy = 1;
    }

    /* Analyze "dx" */
    if (x2 < x1)
    {
        ax = (x1 - x2);
        sx = -1;
    }
    else
    {
        ax = (x2 - x1);
        sx = 1;
    }

    /* Number of "units" in one "half" grid */
    half = (ay * ax);

    /* Number of "units" in one "full" grid */
    full = half << 1;

    /* Vertical */
    if (ay > ax)
    {
        /* Start at tile edge */
        frac = ax * ax;

        /* Let m = ((dx/dy) * full) = (dx * dx * 2) = (frac * 2) */
        m = frac << 1;

        /* Start */
        loc_init(&grid, x1, y1 + sy);

        /* Never go back from whence we came */
        if (loc_eq(&grid, &((struct monster *)mon)->old_grid)) return false;

        /* Create the projection path */
        while (1)
        {
            /* Stop at destination grid */
            if ((grid.x == x2) && (grid.y == y2)) return true;

            /* Stop at permawall grids */
            if (square_isperm(c, &grid)) return false;

            /* Slant */
            if (m)
            {
                /* Advance (X) part 1 */
                frac += m;

                /* Horizontal change */
                if (frac >= half)
                {
                    /* Advance (X) part 2 */
                    grid.x += sx;

                    /* Advance (X) part 3 */
                    frac -= full;
                }
            }

            /* Advance (Y) */
            grid.y += sy;
        }
    }

    /* Horizontal */
    else if (ax > ay)
    {
        /* Start at tile edge */
        frac = ay * ay;

        /* Let m = ((dy/dx) * full) = (dy * dy * 2) = (frac * 2) */
        m = frac << 1;

        /* Start */
        loc_init(&grid, x1 + sx, y1);

        /* Never go back from whence we came */
        if (loc_eq(&grid, &((struct monster *)mon)->old_grid)) return false;

        /* Create the projection path */
        while (1)
        {
            /* Stop at destination grid */
            if ((grid.x == x2) && (grid.y == y2)) return true;

            /* Stop at permawall grids */
            if (square_isperm(c, &grid)) return false;

            /* Slant */
            if (m)
            {
                /* Advance (Y) part 1 */
                frac += m;

                /* Vertical change */
                if (frac >= half)
                {
                    /* Advance (Y) part 2 */
                    grid.y += sy;

                    /* Advance (Y) part 3 */
                    frac -= full;
                }
            }

            /* Advance (X) */
            grid.x += sx;
        }
    }

    /* Diagonal */
    else
    {
        /* Start */
        loc_init(&grid, x1 + sx, y1 + sy);

        /* Never go back from whence we came */
        if (loc_eq(&grid, &((struct monster *)mon)->old_grid)) return false;

        /* Create the projection path */
        while (1)
        {
            /* Stop at destination grid */
            if ((grid.x == x2) && (grid.y == y2)) return true;

            /* Stop at permawall grids */
            if (square_isperm(c, &grid)) return false;

            /* Advance (Y) */
            grid.y += sy;

            /* Advance (X) */
            grid.x += sx;
        }
    }
}


/*
 * Check if the monster can hear anything
 */
static bool monster_can_hear(struct player *p, struct monster *mon)
{
    int base_hearing = mon->race->hearing - p->state.skills[SKILL_STEALTH] / 3;

    if (p->cave->noise.grids[mon->grid.y][mon->grid.x] == 0) return false;
    return ((base_hearing > p->cave->noise.grids[mon->grid.y][mon->grid.x])? true: false);
}


/*
 * Check if the monster can smell anything
 */
static bool monster_can_smell(struct player *p, struct monster *mon)
{
    if (p->cave->scent.grids[mon->grid.y][mon->grid.x] == 0) return false;
    return ((mon->race->smell > p->cave->scent.grids[mon->grid.y][mon->grid.x])? true: false);
}


/*
 * Compare the "strength" of two monsters XXX XXX XXX
 */
static int compare_monsters(const struct monster *mon1, const struct monster *mon2)
{
    u32b mexp1 = mon1->race->mexp;
    u32b mexp2 = mon2->race->mexp;

    /* Compare */
    if (mexp1 < mexp2) return (-1);
    if (mexp1 > mexp2) return (1);

    /* Assume equal */
    return (0);
}


/*
 * Check if the monster can kill any monster on the relevant grid
 */
static bool monster_can_kill(struct chunk *c, struct monster *mon, struct loc *grid)
{
    struct monster *mon1 = square_monster(c, grid);

    /* Kill weaker monsters */
    int kill_ok = rf_has(mon->race->flags, RF_KILL_BODY);

    /* No monster */
    if (!mon1) return true;

    if (kill_ok && (compare_monsters(mon, mon1) > 0)) return true;

    return false;
}


/*
 * Check if the monster can move any monster on the relevant grid
 */
static bool monster_can_move(struct chunk *c, struct monster *mon, struct loc *grid)
{
    struct monster *mon1 = square_monster(c, grid);

    /* Push past weaker monsters (unless leaving a wall) */
    int move_ok = (rf_has(mon->race->flags, RF_MOVE_BODY) && square_ispassable(c, &mon->grid));

    /* No monster */
    if (!mon1) return true;

    if (move_ok && (compare_monsters(mon, mon1) > 0)) return true;

    return false;
}


/*
 * Check if the monster can occupy a grid safely
 */
bool monster_hates_grid(struct chunk *c, struct monster *mon, struct loc *grid)
{
    /* Only some creatures can handle damaging terrain */
    if (square_isdamaging(c, grid) && !rf_has(mon->race->flags, square_feat(c, grid)->resist_flag))
    {
        /* Hack -- passwall creatures can cross any damaging terrain */
        if (rf_has(mon->race->flags, RF_PASS_WALL)) return false;

        /* Hack -- levitating creatures can cross water */
        if (square_iswater(c, grid) && rf_has(mon->race->flags, RF_LEVITATE)) return false;

        return true;
    }

    return false;
}


/*
 * Monster movement routines
 * These routines, culminating in get_move(), choose if and where a monster
 * will move on its turn
 */


/*
 * Calculate minimum and desired combat ranges
 */
static void get_move_find_range(struct player *p, struct monster *mon)
{
    u16b p_lev, m_lev;
    u16b p_chp, p_mhp;
    u32b m_chp, m_mhp;
    u32b p_val, m_val;

    /* Monsters will run up to z_info->flee_range grids out of sight */
    int flee_range = z_info->max_sight + z_info->flee_range;

    bool breathes = flags_test(mon->race->spell_flags, RSF_SIZE, RSF_BREATH_MASK, FLAG_END);

    /* Controlled monsters won't run away */
    if (master_in_party(mon->master, p->id))
        mon->min_range = 1;

    /* All "afraid" monsters will run away */
    else if (mon->m_timed[MON_TMD_FEAR])
        mon->min_range = flee_range;

    /* All "cautious" monsters will run away */
    else if (rf_has(mon->race->flags, RF_CAUTIOUS))
        mon->min_range = flee_range;

    else
    {
        /* Minimum distance - stay at least this far if possible */
        mon->min_range = 1;

        /* Examine player power (level) */
        p_lev = p->lev;

        /* Examine monster power (level plus morale) */
        m_lev = mon->level + (mon->midx & 0x08) + 25;

        /* Simple cases first */
        if (m_lev + 3 < p_lev)
            mon->min_range = flee_range;
        else if (m_lev - 5 < p_lev)
        {
            /* Examine player health */
            p_chp = p->chp;
            p_mhp = p->mhp;

            /* Examine monster health */
            m_chp = mon->hp;
            m_mhp = mon->maxhp;

            /* Prepare to optimize the calculation */
            p_val = (p_lev * p_mhp) + (p_chp << 2); /* div p_mhp */
            m_val = (m_lev * m_mhp) + (m_chp << 2); /* div m_mhp */

            /* Strong players scare strong monsters */
            if (p_val * m_mhp > m_val * p_mhp)
                mon->min_range = flee_range;
        }
    }

    if (mon->min_range < flee_range)
    {
        /* Creatures that don't move never like to get too close */
        if (rf_has(mon->race->flags, RF_NEVER_MOVE))
            mon->min_range += 3;

        /* Spellcasters that don't strike never like to get too close */
        if (rf_has(mon->race->flags, RF_NEVER_BLOW))
            mon->min_range += 3;
    }

    /* Maximum range to flee to */
    if (!(mon->min_range < flee_range))
        mon->min_range = flee_range;

    /* Nearby monsters won't run away */
    else if (mon->cdis < z_info->turn_range)
        mon->min_range = 1;

    /* Now find preferred range */
    mon->best_range = mon->min_range;

    if (mon->race->freq_spell > 24)
    {
        /* Breathers like point blank range */
        if (breathes && (mon->best_range < 6) && (mon->hp > mon->maxhp / 2))
            mon->best_range = 6;

        /* Other spell casters will sit back and cast */
        else
            mon->best_range += 3;
    }
}


static int get_best_noise(struct player *p, struct chunk *c, struct monster *mon, struct loc *grid)
{
    int i;
    int base_hearing = mon->race->hearing - p->state.skills[SKILL_STEALTH] / 3;
    int best_noise = base_hearing - p->cave->noise.grids[grid->y][grid->x];

    /* Check nearby sound, giving preference to the cardinal directions */
    for (i = 0; i < 8; i++)
    {
        /* Get the location */
        struct loc a_grid;
        int heard_noise;

        loc_sum(&a_grid, grid, &ddgrid_ddd[i]);

        /* Bounds check */
        if (!square_in_bounds(c, &a_grid)) continue;

        heard_noise = base_hearing - p->cave->noise.grids[a_grid.y][a_grid.x];

        /* Must be some noise */
        if (p->cave->noise.grids[a_grid.y][a_grid.x] == 0) continue;

        /* There's a monster blocking that we can't deal with */
        if (!monster_can_kill(c, mon, &a_grid) && !monster_can_move(c, mon, &a_grid))
            continue;

        /* There's damaging terrain */
        if (monster_hates_grid(c, mon, &a_grid)) continue;

        /* If it's better than the current noise, choose this direction */
        if (heard_noise > best_noise) best_noise = heard_noise;
    }

    return best_noise;
}


static int get_max_noise(struct player *p, struct chunk *c, struct monster *mon, int best_noise)
{
    int i;
    int base_hearing = mon->race->hearing - p->state.skills[SKILL_STEALTH] / 3;
    int max_noise = 0;

    /* Check nearby sound, giving preference to the cardinal directions */
    for (i = 0; i < 8; i++)
    {
        /* Get the location */
        struct loc grid;
        int heard_noise;

        loc_sum(&grid, &mon->grid, &ddgrid_ddd[i]);

        /* Bounds check */
        if (!square_in_bounds(c, &grid)) continue;

        heard_noise = base_hearing - p->cave->noise.grids[grid.y][grid.x];

        /* Must be some noise */
        if (p->cave->noise.grids[grid.y][grid.x] == 0) continue;

        /* There's a monster blocking that we can't deal with */
        if (!monster_can_kill(c, mon, &grid) && !monster_can_move(c, mon, &grid))
            continue;

        /* There's damaging terrain */
        if (monster_hates_grid(c, mon, &grid)) continue;

        /* Possible move if we can't actually get closer */
        if (heard_noise == best_noise)
        {
            /* Check nearby grids for max noise */
            int noise = get_best_noise(p, c, mon, &grid);

            if (noise > max_noise) max_noise = noise;
        }
    }

    return max_noise;
}


static int get_best_scent(struct player *p, struct chunk *c, struct monster *mon, struct loc *grid)
{
    int i;
    int best_scent = mon->race->smell - p->cave->scent.grids[grid->y][grid->x];

    /* Check nearby scent, giving preference to the cardinal directions */
    for (i = 0; i < 8; i++)
    {
        /* Get the location */
        struct loc a_grid;
        int smelled_scent;

        loc_sum(&a_grid, grid, &ddgrid_ddd[i]);

        /* Bounds check */
        if (!square_in_bounds(c, &a_grid)) continue;

        smelled_scent = mon->race->smell - p->cave->scent.grids[a_grid.y][a_grid.x];

        /* Must be some scent */
        if (p->cave->scent.grids[a_grid.y][a_grid.x] == 0) continue;

        /* There's a monster blocking that we can't deal with */
        if (!monster_can_kill(c, mon, &a_grid) && !monster_can_move(c, mon, &a_grid))
            continue;

        /* There's damaging terrain */
        if (monster_hates_grid(c, mon, &a_grid)) continue;

        /* If it's better than the current scent, choose this direction */
        if (smelled_scent > best_scent) best_scent = smelled_scent;
    }

    return best_scent;
}


static int get_max_scent(struct player *p, struct chunk *c, struct monster *mon, int best_scent)
{
    int i;
    int max_scent = 0;

    /* Check nearby scent, giving preference to the cardinal directions */
    for (i = 0; i < 8; i++)
    {
        /* Get the location */
        struct loc grid;
        int smelled_scent;

        loc_sum(&grid, &mon->grid, &ddgrid_ddd[i]);

        /* Bounds check */
        if (!square_in_bounds(c, &grid)) continue;

        smelled_scent = mon->race->smell - p->cave->scent.grids[grid.y][grid.x];

        /* Must be some scent */
        if (p->cave->scent.grids[grid.y][grid.x] == 0) continue;

        /* There's a monster blocking that we can't deal with */
        if (!monster_can_kill(c, mon, &grid) && !monster_can_move(c, mon, &grid))
            continue;

        /* There's damaging terrain */
        if (monster_hates_grid(c, mon, &grid)) continue;

        /* Possible move if we can't actually get closer */
        if (smelled_scent == best_scent)
        {
            /* Check nearby grids for max scent */
            int scent = get_best_scent(p, c, mon, &grid);

            if (scent > max_scent) max_scent = scent;
        }
    }

    return max_scent;
}


static bool player_walled(struct player *p, struct chunk *c)
{
    int d, n = 0;

    for (d = 0; d < 9; d++)
    {
        struct loc grid;

        loc_sum(&grid, &p->grid, &ddgrid_ddd[d]);
        if (square_iswall(c, &grid)) n++;
    }

    return (n == 9);
}


/*
 * Choose the best direction to advance toward the player, using sound or scent.
 *
 * Note that ghosts and rock-eaters generally just head straight for the player.
 *
 * Monsters first try to use current sound information as saved in
 * cave->noise.grids[y][x]. Failing that, they'll try using scent, saved in
 * cave->scent.grids[y][x].
 *
 * Note that this function assumes the monster is moving to an adjacent grid,
 * and so the noise can be louder by at most 1.
 *
 * Tracking by 'scent' means that monsters end up near enough the player to
 * switch to 'sound' (noise), or they end up somewhere the player left via
 * teleport. Teleporting away from a location will cause the monsters who
 * were chasing the player to converge on that location as long as the player
 * is still near enough to "annoy" them without being close enough to chase
 * directly.
 */
static bool get_move_advance(struct player *p, struct chunk *c, struct monster *mon)
{
    int i, n = 0;
    struct loc *decoy = cave_find_decoy(c), target;
    int base_hearing = mon->race->hearing - p->state.skills[SKILL_STEALTH] / 3;
    int best_scent, max_scent;
    int best_noise, max_noise;
    struct loc best_grid[8];

    /* PWMAngband: not if the player is wraithed in walls */
    if (player_walled(p, c)) return false;

    if (!loc_is_zero(decoy))
        loc_copy(&target, decoy);
    else
        loc_copy(&target, &p->grid);

    /* If the monster can pass through nearby walls, do that */
    if (monster_passes_walls(mon->race) && can_path_player(p, mon, c))
    {
        loc_copy(&mon->target.grid, &target);
        return true;
    }

    /* If the player can see monster, set target and run towards them */
    if (square_isview(p, &mon->grid))
    {
        loc_copy(&mon->target.grid, &target);
        return true;
    }

    for (i = 0; i < 8; i++) loc_init(&best_grid[i], 0, 0);

    /* Get nearby grids with best noise, break ties with max noise */
    best_noise = get_best_noise(p, c, mon, &mon->grid);
    max_noise = get_max_noise(p, c, mon, best_noise);
    for (i = 0; i < 8; i++)
    {
        /* Get the location */
        struct loc grid;
        int heard_noise;

        loc_sum(&grid, &mon->grid, &ddgrid_ddd[i]);

        /* Bounds check */
        if (!square_in_bounds(c, &grid)) continue;

        heard_noise = base_hearing - p->cave->noise.grids[grid.y][grid.x];

        /* Must be some noise */
        if (p->cave->noise.grids[grid.y][grid.x] == 0) continue;

        /* There's a monster blocking that we can't deal with */
        if (!monster_can_kill(c, mon, &grid) && !monster_can_move(c, mon, &grid))
            continue;

        /* There's damaging terrain */
        if (monster_hates_grid(c, mon, &grid)) continue;

        /* If it's better than the current noise, choose this direction */
        /* Possible move if we can't actually get closer */
        if (heard_noise == best_noise)
        {
            /* Check nearby grids for best noise again (in case we have multiple valid grids) */
            int noise = get_best_noise(p, c, mon, &grid);

            if (noise == max_noise)
            {
                loc_copy(&best_grid[n], &grid);
                n++;
            }
        }
    }

    /* Set the target */
    if (n)
    {
        /* If we have multiple valid directions, choose one at random */
        i = randint0(n);

        loc_copy(&mon->target.grid, &best_grid[i]);
        return true;
    }

    /* If no good sound, use scent */
    best_scent = get_best_scent(p, c, mon, &mon->grid);
    max_scent = get_max_scent(p, c, mon, best_scent);
    for (i = 0; i < 8; i++)
    {
        /* Get the location */
        struct loc grid;
        int smelled_scent;

        loc_sum(&grid, &mon->grid, &ddgrid_ddd[i]);

        /* Bounds check */
        if (!square_in_bounds(c, &grid)) continue;

        smelled_scent = mon->race->smell - p->cave->scent.grids[grid.y][grid.x];

        /* Must be some scent */
        if (p->cave->scent.grids[grid.y][grid.x] == 0) continue;

        /* There's a monster blocking that we can't deal with */
        if (!monster_can_kill(c, mon, &grid) && !monster_can_move(c, mon, &grid))
            continue;

        /* There's damaging terrain */
        if (monster_hates_grid(c, mon, &grid)) continue;

        /* If it's better than the current scent, choose this direction */
        /* Possible move if we can't actually get closer */
        if (smelled_scent == best_scent)
        {
            /* Check nearby grids for best scent again (in case we have multiple valid grids) */
            int scent = get_best_scent(p, c, mon, &grid);

            if (scent == max_scent)
            {
                loc_copy(&best_grid[n], &grid);
                n++;
            }
        }
    }

    /* Set the target */
    if (n)
    {
        /* If we have multiple valid directions, choose one at random */
        i = randint0(n);

        loc_copy(&mon->target.grid, &best_grid[i]);
        return true;
    }

    /* No reason to advance */
    return false;
}


/*
 * Choose a "safe" location near a monster for it to run toward.
 *
 * A location is "safe" if it can be reached quickly and the player
 * is not able to fire into it (it isn't a "clean shot"). So, this will
 * cause monsters to "duck" behind walls. Hopefully, monsters will also
 * try to run towards corridor openings if they are in a room.
 *
 * This function may take lots of CPU time if lots of monsters are fleeing.
 *
 * Return true if a safe location is available.
 */
static bool get_move_find_safety(struct player *p, struct chunk *c, struct monster *mon)
{
    int i, d, dis, gdis = 0;
    const int *y_offsets;
    const int *x_offsets;
    struct loc delta;

    /* Start with adjacent locations, spread further */
    for (d = 1; d < 10; d++)
    {
        struct loc best;

        loc_init(&best, 0, 0);

        /* Get the lists of points with a distance d from (fx, fy) */
        y_offsets = dist_offsets_y[d];
        x_offsets = dist_offsets_x[d];

        /* Check the locations */
        for (i = 0, delta.x = x_offsets[0], delta.y = y_offsets[0]; !loc_is_zero(&delta);
             i++, delta.x = x_offsets[i], delta.y = y_offsets[i])
        {
            struct loc grid;

            loc_sum(&grid, &mon->grid, &delta);

            /* Skip illegal locations */
            if (!square_in_bounds_fully(c, &grid)) continue;

            /* Skip locations in a wall */
            if (!square_ispassable(c, &grid)) continue;

            /* Ignore too-distant grids */
            if (p->cave->noise.grids[grid.y][grid.x] >
                p->cave->noise.grids[mon->grid.y][mon->grid.x] + 2 * d)
            {
                continue;
            }

            /* Ignore damaging terrain if they can't handle it */
            if (monster_hates_grid(c, mon, &grid)) continue;

            /* Check for absence of shot (more or less) */
            if (!square_isview(p, &grid))
            {
                /* Calculate distance from player */
                dis = distance(&grid, &p->grid);

                /* Remember if further than previous */
                if (dis > gdis)
                {
                    loc_copy(&best, &grid);
                    gdis = dis;
                }
            }
        }

        /* Check for success */
        if (gdis > 0)
        {
            /* Good location */
            loc_copy(&mon->target.grid, &best);

            /* Found safe place */
            return true;
        }
    }

    /* No safe place */
    return false;
}


/*
 * Choose a good hiding place near a monster for it to run toward.
 *
 * Pack monsters will use this to "ambush" the player and lure him out
 * of corridors into open space so they can swarm him.
 *
 * Return true if a good location is available.
 */
static bool get_move_find_hiding(struct player *p, struct chunk *c, struct monster *mon)
{
    int i, d, dis, gdis = 999, min;
    const int *y_offsets, *x_offsets;

    /* Closest distance to get */
    min = distance(&p->grid, &mon->grid) * 3 / 4 + 2;

    /* Start with adjacent locations, spread further */
    for (d = 1; d < 10; d++)
    {
        struct loc best, delta;

        loc_init(&best, 0, 0);

        /* Get the lists of points with a distance d from monster */
        y_offsets = dist_offsets_y[d];
        x_offsets = dist_offsets_x[d];

        /* Check the locations */
        for (i = 0, delta.x = x_offsets[0], delta.y = y_offsets[0]; !loc_is_zero(&delta);
             i++, delta.x = x_offsets[i], delta.y = y_offsets[i])
        {
            struct loc grid;

            loc_sum(&grid, &mon->grid, &delta);

            /* Skip illegal locations */
            if (!square_in_bounds_fully(c, &grid)) continue;

            /* Skip occupied locations */
            if (!square_isemptyfloor(c, &grid)) continue;

            /* Check for hidden, available grid */
            if (!square_isview(p, &grid) && projectable(c, &mon->grid, &grid, PROJECT_STOP, true))
            {
                /* Calculate distance from player */
                dis = distance(&grid, &p->grid);

                /* Remember if closer than previous */
                if ((dis < gdis) && (dis >= min))
                {
                    loc_copy(&best, &grid);
                    gdis = dis;
                }
            }
        }

        /* Check for success */
        if (gdis < 999)
        {
            /* Good location */
            loc_copy(&mon->target.grid, &best);

            /* Found good place */
            return true;
        }
    }

    /* No good place */
    return false;
}


/*
 * Provide a location to flee to, but give the player a wide berth.
 *
 * A monster may wish to flee to a location that is behind the player,
 * but instead of heading directly for it, the monster should "swerve"
 * around the player so that it has a smaller chance of getting hit.
 */
static bool get_move_flee(struct player *p, struct monster *mon)
{
    int i;
    int best_score = -1;

    /* If the player is not currently near the monster, no reason to flow */
    if (mon->cdis >= mon->best_range) return false;

    /* Monster is too far away to use sound or scent */
    if (!monster_can_hear(p, mon) && !monster_can_smell(p, mon)) return false;

    /* Check nearby grids, diagonals first */
    for (i = 7; i >= 0; i--)
    {
        int dis, score;
        struct loc grid;

        /* Get the location */
        loc_sum(&grid, &mon->grid, &ddgrid_ddd[i]);

        /* Bounds check */
        if (!square_in_bounds(chunk_get(&p->wpos), &grid)) continue;

        /* Calculate distance of this grid from our target */
        dis = distance(&grid, &mon->target.grid);

        /*
         * Score this grid
         * First half of calculation is inversely proportional to distance
         * Second half is inversely proportional to grid's distance from player
         */
        score = 5000 / (dis + 3) - 500 / (p->cave->noise.grids[grid.y][grid.x] + 1);

        /* No negative scores */
        if (score < 0) score = 0;

        /* Ignore lower scores */
        if (score < best_score) continue;

        /* Save the score */
        best_score = score;

        /* Save the location */
        loc_copy(&mon->target.grid, &grid);
    }

    /* Success */
    return true;
}


/*
 * Choose the basic direction of movement, and whether to bias left or right
 * if the main direction is blocked.
 *
 * Note that the input is an offset to the monster's current position, and
 * the output direction is intended as an index into the side_dirs array.
 */
static int get_move_choose_direction(struct loc *offset)
{
    int dir = 0;
    int dx = offset->x, dy = offset->y;

    /* Extract the "absolute distances" */
    int ay = ABS(dy);
    int ax = ABS(dx);

    /* We mostly want to move vertically */
    if (ay > (ax * 2))
    {
        /* Choose between directions '8' and '2' */
        if (dy > 0)
        {
            /* We're heading down */
            dir = 2;
            if ((dx > 0) || (dx == 0 && turn.turn % 2 == 0))
                dir += 10;
        }
        else
        {
            /* We're heading up */
            dir = 8;
            if ((dx < 0) || (dx == 0 && turn.turn % 2 == 0))
                dir += 10;
        }
    }

    /* We mostly want to move horizontally */
    else if (ax > (ay * 2))
    {
        /* Choose between directions '4' and '6' */
        if (dx > 0)
        {
            /* We're heading right */
            dir = 6;
            if ((dy < 0) || (dy == 0 && turn.turn % 2 == 0))
                dir += 10;
        }
        else
        {
            /* We're heading left */
            dir = 4;
            if ((dy > 0) || (dy == 0 && turn.turn % 2 == 0))
                dir += 10;
        }
    }

    /* We want to move down and sideways */
    else if (dy > 0)
    {
        /* Choose between directions '1' and '3' */
        if (dx > 0)
        {
            /* We're heading down and right */
            dir = 3;
            if ((ay < ax) || (ay == ax && turn.turn % 2 == 0))
                dir += 10;
        }
        else
        {
            /* We're heading down and left */
            dir = 1;
            if ((ay > ax) || (ay == ax && turn.turn % 2 == 0))
                dir += 10;
        }
    }

    /* We want to move up and sideways */
    else
    {
        /* Choose between directions '7' and '9' */
        if (dx > 0)
        {
            /* We're heading up and right */
            dir = 9;
            if ((ay > ax) || (ay == ax && turn.turn % 2 == 0))
                dir += 10;
        }
        else
        {
            /* We're heading up and left */
            dir = 7;
            if ((ay < ax) || (ay == ax && turn.turn % 2 == 0))
                dir += 10;
        }
    }

    return dir;
}


/*
 * Choose "logical" directions for monster movement
 */
static bool get_move(struct source *who, struct chunk *c, struct monster *mon, int *dir, bool *good)
{
    struct loc *decoy = cave_find_decoy(c), target;

    /* Offset to current position to move toward */
    struct loc grid;

    /* Monsters will run up to z_info->flee_range grids out of sight */
    int flee_range = z_info->max_sight + z_info->flee_range;

    bool done = false;

    if (!loc_is_zero(decoy))
        loc_copy(&target, decoy);
    else
        loc_copy(&target, &who->player->grid);

    /* Calculate range */
    if (!who->monster) get_move_find_range(who->player, mon);

    /* Assume we're heading towards the player */
    if (!who->monster)
    {
        /* Extract the "pseudo-direction" */
        if (get_move_advance(who->player, c, mon))
        {
            loc_diff(&grid, &mon->target.grid, &mon->grid);
            *good = true;
        }

        /* Head blindly straight for the "player" if there's no better idea */
        else
            loc_diff(&grid, &target, &mon->grid);
    }
    else
    {
        /* Head straight for the monster */
        loc_diff(&grid, &who->monster->grid, &mon->grid);
    }

    /* Player */
    if (!who->monster)
    {
        bool group_ai = (rf_has(mon->race->flags, RF_GROUP_AI) &&
            !flags_test(mon->race->flags, RF_SIZE, RF_KILL_WALL, RF_PASS_WALL, FLAG_END));

        /* Normal animal packs try to get the player out of corridors. */
        if (group_ai)
        {
            int i, open = 0;

            /*
             * Check grid around the player for room interior (room walls count)
             * or other empty space
             */
            for (i = 0; i < 8; i++)
            {
                struct loc test;

                loc_sum(&test, &target, &ddgrid_ddd[i]);

                /* Check grid */
                if (square_ispassable(c, &test) || square_isroom(c, &test))
                {
                    /* One more open grid */
                    open++;
                }
            }

            /* Not in an empty space and strong player */
            if ((open < 5) && (who->player->chp > who->player->mhp / 2))
            {
                /* Find hiding place */
                if (get_move_find_hiding(who->player, c, mon))
                {
                    done = true;
                    loc_diff(&grid, &mon->target.grid, &mon->grid);
                }
            }
        }

        /* Apply fear */
        if (!done && (mon->min_range == flee_range))
        {
            /* Try to find safe place */
            if (!get_move_find_safety(who->player, c, mon))
            {
                /* Just leg it away from the player */
                grid.y = 0 - grid.y;
                grid.x = 0 - grid.x;
            }
            else
            {
                /* Set a course for the safe place */
                get_move_flee(who->player, mon);
                loc_diff(&grid, &mon->target.grid, &mon->grid);
            }

            done = true;
        }

        /* Monster groups try to surround the player */
        if (!done && group_ai && square_isview(who->player, &mon->grid))
        {
            int i;
            struct loc grid1;

            loc_copy(&grid1, &mon->target.grid);

            /* If we are not already adjacent */
            if (mon->cdis > 1)
            {
                int tmp = randint0(8);

                /* Find an empty square near the player to fill */
                for (i = 0; i < 8; i++)
                {
                    /* Pick squares near player (pseudo-randomly) */
                    loc_sum(&grid1, &target, &ddgrid_ddd[(tmp + i) & 7]);

                    /* Ignore filled grids */
                    if (!square_isemptyfloor(c, &grid1)) continue;

                    /* Try to fill this hole */
                    break;
                }
            }

            /* Head in the direction of the chosen grid */
            loc_diff(&grid, &grid1, &mon->grid);
        }
    }

    /* Check if the monster has already reached its target */
    if (loc_is_zero(&grid)) return false;

    /* Pick the correct direction */
    *dir = get_move_choose_direction(&grid);

    /* Want to move */
    return true;
}


/*
 * Monster turn routines
 * These routines, culminating in monster_turn(), decide how a monster uses
 * its turn
 */


/*
 * Lets the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 *
 * Returns true if the monster successfully reproduced.
 */
bool multiply_monster(struct player *p, struct chunk *c, struct monster *mon)
{
    int i;
    bool result = false;

    my_assert(mon);

    /* Paranoia */
    if (!p) return false;

    /* Only on random levels */
    if (!random_level(&p->wpos)) return false;

    /* No uniques */
    if (monster_is_unique(mon->race)) return false;

    /* Limit number of clones */
    if (c->num_clones == z_info->repro_monster_max) return false;

    /* Try up to 18 times */
    for (i = 0; i < 18; i++)
    {
        int d = 1;
        struct loc grid;

        /* Pick a location */
        if (!scatter(c, &grid, &mon->grid, d, true)) continue;

        /* Require an "empty" floor grid */
        if (!square_isemptyfloor(c, &grid)) continue;

        /* Create a new monster (awake, no groups) */
        result = place_new_monster(p, c, &grid, mon->race, MON_CLONE, ORIGIN_DROP_BREED);

        /* Done */
        break;
    }

    /* Result */
    return (result);
}


/*
 * Attempt to reproduce, if possible. All monsters are checked here for
 * lore purposes, the unfit fail.
 */
static bool monster_turn_multiply(struct chunk *c, struct monster *mon)
{
    struct player *p = mon->closest_player;
    int k = 0;
    struct monster_lore *lore = get_lore(p, mon->race);
    bool allow_breed = false;
    struct loc begin, end;
    struct loc_iterator iter;

    loc_init(&begin, mon->grid.x - 1, mon->grid.y - 1);
    loc_init(&end, mon->grid.x + 1, mon->grid.y + 1);
    loc_iterator_first(&iter, &begin, &end);

    /* Count the adjacent monsters */
    do
    {
        if (square(c, &iter.cur)->mon > 0) k++;
    }
    while (loc_iterator_next(&iter));

    /* Multiply slower in crowded areas */
    /* Hack -- multiply even more slowly on no_recall servers */
    if (cfg_diving_mode == 3)
        allow_breed = ((k < 4) && one_in_((k + 1) * z_info->repro_monster_rate * 2));
    else
        allow_breed = ((k < 4) && (!k || one_in_(k * z_info->repro_monster_rate)));
    if (allow_breed)
    {
        /* Successful breeding attempt, learn about that now */
        if (monster_is_visible(p, mon->midx)) rf_on(lore->flags, RF_MULTIPLY);

        /* Leave now if not a breeder */
        if (!rf_has(mon->race->flags, RF_MULTIPLY)) return false;

        /* Try to multiply */
        if (multiply_monster(p, c, mon))
        {
            /* Make a sound */
            if (monster_is_visible(p, mon->midx)) sound(p, MSG_MULTIPLY);

            /* Multiplying takes energy */
            return true;
        }
    }

    return false;
}


/*
 * Check if a monster should stagger or not. Always stagger when confused,
 * but also deal with random movement for RAND_25 and _50 monsters.
 */
static bool monster_turn_should_stagger(struct player *p, struct monster *mon)
{
    struct monster_lore *lore = get_lore(p, mon->race);
    int chance, accuracy;

    /* Blind */
    if (mon->m_timed[MON_TMD_BLIND]) return true;

    /* Increase chance of being erratic for every level of confusion */
    accuracy = monster_effect_accuracy(mon, MON_TMD_CONF, CONF_ERRATIC_CHANCE);
    chance = 100 - accuracy;

    /* RAND_25 and RAND_50 are cumulative */
    if (rf_has(mon->race->flags, RF_RAND_25))
    {
        chance += 25;
        if (monster_is_visible(p, mon->midx)) rf_on(lore->flags, RF_RAND_25);
    }

    if (rf_has(mon->race->flags, RF_RAND_50))
    {
        chance += 50;
        if (monster_is_visible(p, mon->midx)) rf_on(lore->flags, RF_RAND_50);
    }

    return magik(chance);
}


/*
 * Work out if a monster can move through the grid, if necessary bashing
 * down doors in the way.
 *
 * Returns true if the monster is able to move through the grid.
 */
static bool monster_turn_can_move(struct source *who, struct chunk *c, struct monster *mon,
    struct loc *grid, bool *did_something)
{
    struct monster_lore *lore = get_lore(who->player, mon->race);
    struct monster *square_mon = NULL;

    /* Dangerous terrain in the way */
    if (monster_hates_grid(c, mon, grid)) return false;

    /* Safe floor */
    if (square_issafefloor(c, grid)) return false;

    /* Floor is open? */
    if (square_ispassable(c, grid)) return true;

    /* Permanent wall in the way */
    if (square_isperm(c, grid) || square_isborder(c, grid)) return false;

    /* Normal wall, door, or secret door in the way */

    /*
     * There's some kind of feature in the way, so learn about
     * kill-wall and pass-wall now
     */
    if (monster_is_visible(who->player, mon->midx))
    {
        rf_on(lore->flags, RF_PASS_WALL);
        rf_on(lore->flags, RF_KILL_WALL);
    }

    /* Reveal (door) mimics */
    if (square_monster(c, grid)) square_mon = cave_monster(c, square(c, grid)->mon);
    if (square_mon && monster_is_camouflaged(square_mon))
    {
        become_aware(who->player, c, square_mon);

        return true;
    }

    /* Monster may be able to deal with walls and doors */
    if (rf_has(mon->race->flags, RF_PASS_WALL)) return true;
    if (rf_has(mon->race->flags, RF_KILL_WALL))
    {
        /* Remove the wall */
        square_destroy_wall(c, grid);

        /* Note changes to viewable region */
        note_viewable_changes(&c->wpos, grid);

        /* Fully update the flow since terrain changed */
        fully_update_flow(&c->wpos);

        return true;
    }
    if (square_basic_iscloseddoor(c, grid) || square_issecretdoor(c, grid))
    {
        bool can_open = rf_has(mon->race->flags, RF_OPEN_DOOR);
        bool can_bash = rf_has(mon->race->flags, RF_BASH_DOOR);
        bool will_bash = false;

        /* Take a turn */
        *did_something = true;

        /* Learn about door abilities */
        if (monster_is_visible(who->player, mon->midx))
        {
            rf_on(lore->flags, RF_OPEN_DOOR);
            rf_on(lore->flags, RF_BASH_DOOR);
        }

        /* If creature can open or bash doors, make a choice */
        if (can_open)
        {
            /* Sometimes bash anyway (impatient) */
            if (can_bash) will_bash = (one_in_(2)? true: false);
        }

        /* Only choice */
        else if (can_bash)
            will_bash = true;

        /* Door is an insurmountable obstacle */
        else
            return false;

        /* Now outcome depends on type of door */
        if (square_islockeddoor(c, grid))
        {
            /* Locked door -- test monster strength against door strength */
            int k = square_door_power(c, grid);

            if (!CHANCE(k, mon->hp / 10))
            {
                char m_name[NORMAL_WID];
                const char *article = "a";

                if (monster_is_visible(who->player, mon->midx)) article = "the";
                monster_desc(who->player, m_name, sizeof(m_name), mon, MDESC_CAPITAL);

                /* Print a message */
                if (will_bash)
                    msg(who->player, "%s slams against %s door.", m_name, article);
                else
                    msg(who->player, "%s fiddles with %s lock.", m_name, article);

                /* Reduce the power of the door by one */
                square_set_door_lock(c, grid, k - 1);
            }
        }
        else
        {
            /* Closed or secret door -- always open or bash */
            if (will_bash)
            {
                square_smash_door(c, grid);

                msg(who->player, "You hear a door burst open!");

                /* Disturb if necessary */
                if (!who->monster && OPT(who->player, disturb_bash)) disturb(who->player, 0);

                /* Note changes to viewable region */
                note_viewable_changes(&c->wpos, grid);

                /* Fall into doorway */
                return true;
            }

            square_open_door(c, grid);

            /* Note changes to viewable region */
            note_viewable_changes(&c->wpos, grid);
        }
    }

    return false;
}


/*
 * Try to break a glyph.
 */
static bool monster_turn_glyph(struct player *p, struct monster *mon, struct loc *grid)
{
    /* Break the ward */
    if (CHANCE(mon->level, z_info->glyph_hardness))
    {
        /* Describe observable breakage */
        if (square_isseen(p, grid))
            msg(p, "The rune of protection is broken!");

        /* Break the rune */
        square_remove_all_traps(chunk_get(&p->wpos), grid);

        return true;
    }

    /* Unbroken ward - can't move */
    return false;
}


/*
 * Try to push past / kill another monster. Returns true on success.
 */
static bool monster_turn_try_push(struct source *who, struct chunk *c, struct monster *mon,
    const char *m_name, struct loc *grid, bool *did_something)
{
    struct monster_lore *lore = get_lore(who->player, mon->race);
    struct source target_body;
    struct source *target = &target_body;
    struct monster *mon1 = square_monster(c, grid);

    /* Kill weaker monsters */
    int kill_ok = rf_has(mon->race->flags, RF_KILL_BODY);

    /* Push past weaker monsters (unless leaving a wall) */
    int move_ok = (rf_has(mon->race->flags, RF_MOVE_BODY) && square_ispassable(c, &mon->grid));

    source_both(target, who->player, mon1);

    /* Always attack if this is our target */
    if (who->monster && (who->monster == mon1) && !master_in_party(mon->master, mon1->master))
    {
        /* Do the attack */
        make_attack_normal(mon, target);

        /* Took a turn */
        *did_something = true;

        return false;
    }

    if (compare_monsters(mon, mon1) > 0)
    {
        /* Learn about pushing and shoving */
        if (monster_is_visible(who->player, mon->midx))
        {
            rf_on(lore->flags, RF_KILL_BODY);
            rf_on(lore->flags, RF_MOVE_BODY);
        }

        if (kill_ok || move_ok)
        {
            char n_name[NORMAL_WID];

            /* Get the names of the monsters involved */
            monster_desc(who->player, n_name, sizeof(n_name), mon1, MDESC_IND_HID);

            /* Reveal mimics */
            if (monster_is_camouflaged(mon1)) become_aware(who->player, c, mon1);

            /* Note if visible */
            if (monster_is_visible(who->player, mon->midx) &&
                monster_is_in_view(who->player, mon->midx))
            {
                msg(who->player, "%s %s %s.", m_name, (kill_ok? "tramples over": "pushes past"),
                    n_name);
            }

            /* Monster ate another monster */
            if (kill_ok) delete_monster(c, grid);

            /* Move the monster */
            monster_swap(c, &mon->grid, grid);
            *did_something = true;

            return true;
        }

        /* Otherwise, attack the monster (skip if non hostile) */
        if (who->monster && !master_in_party(mon->master, mon1->master))
        {
            /* Do the attack */
            make_attack_normal(mon, target);

            /* Took a turn */
            *did_something = true;

            return false;
        }

        /* If the target is a player and there's a controlled monster on the way, try to retaliate */
        if (!who->monster && master_in_party(mon1->master, who->player->id))
        {
            int chance = 25;

            /* Stupid monsters rarely retaliate */
            if (monster_is_stupid(mon->race)) chance = 10;

            /* Smart monsters always retaliate */
            if (monster_is_smart(mon->race)) chance = 100;

            /* Sometimes monsters retaliate */
            if (magik(chance))
            {
                /* Do the attack */
                make_attack_normal(mon, target);

                /* Took a turn */
                *did_something = true;
            }
        }

        return false;
    }

    /* Always attack stronger monsters */
    if (who->monster && !master_in_party(mon->master, mon1->master))
    {
        /* Do the attack */
        make_attack_normal(mon, target);

        /* Took a turn */
        *did_something = true;

        return false;
    }

    /* If the target is a player and there's a controlled monster on the way, try to retaliate */
    if (!who->monster && master_in_party(mon1->master, who->player->id))
    {
        int chance = 25;

        /* Stupid monsters rarely retaliate */
        if (monster_is_stupid(mon->race)) chance = 10;

        /* Smart monsters always retaliate */
        if (monster_is_smart(mon->race)) chance = 100;

        /* Sometimes monsters retaliate */
        if (magik(chance))
        {
            /* Do the attack */
            make_attack_normal(mon, target);

            /* Took a turn */
            *did_something = true;
        }
    }

    return false;
}


/*
 * Grab all objects from the grid.
 */
static void monster_turn_grab_objects(struct player *p, struct chunk *c, struct monster *mon,
    const char *m_name, struct loc *grid)
{
    struct monster_lore *lore = get_lore(p, mon->race);
    struct object *obj, *next;
    bool visible = monster_is_visible(p, mon->midx);

    /* Learn about item pickup behavior */
    for (obj = square_object(c, grid); obj; obj = obj->next)
    {
        /* Skip gold and mimicked objects */
        if (tval_is_money(obj) || obj->mimicking_m_idx) continue;

        if (visible)
        {
            rf_on(lore->flags, RF_TAKE_ITEM);
            rf_on(lore->flags, RF_KILL_ITEM);
        }

        break;
    }

    /* Abort if can't pickup/kill */
    if (!rf_has(mon->race->flags, RF_TAKE_ITEM) && !rf_has(mon->race->flags, RF_KILL_ITEM))
        return;

    /* Take or kill objects on the floor */
    obj = square_object(c, grid);
    while (obj)
    {
        char o_name[NORMAL_WID];
        bool safe = (obj->artifact? true: false);

        next = obj->next;

        /* Skip gold */
        if (tval_is_money(obj))
        {
            obj = next;
            continue;
        }

        /* Skip mimicked objects */
        if (obj->mimicking_m_idx)
        {
            obj = next;
            continue;
        }

        /* Get the object name */
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

        /* React to objects that hurt the monster */
        if (react_to_slay(obj, mon)) safe = true;

        /* Try to pick up, or crush */
        if (safe)
        {
            /* Only give a message for "take_item" */
            if (rf_has(mon->race->flags, RF_TAKE_ITEM) && visible && square_isview(p, grid) &&
                !ignore_item_ok(p, obj))
            {
                /* Dump a message */
                msg(p, "%s tries to pick up %s, but fails.", m_name, o_name);
            }
        }
        else if (rf_has(mon->race->flags, RF_TAKE_ITEM))
        {
            /* Controlled monsters don't take objects */
            if (!mon->master)
            {
                /* Describe observable situations */
                if (square_isseen(p, grid) && !ignore_item_ok(p, obj))
                    msg(p, "%s picks up %s.", m_name, o_name);

                /* Carry the object */
                square_excise_object(c, grid, obj);
                if (!monster_carry(mon, obj, false))
                    object_delete(&obj);

                square_note_spot(c, grid);
                square_light_spot(c, grid);
            }
        }
        else
        {
            /* Describe observable situations */
            if (square_isseen(p, grid) && !ignore_item_ok(p, obj))
                msgt(p, MSG_DESTROY, "%s crushes %s.", m_name, o_name);

            /* Delete the object */
            square_excise_object(c, grid, obj);
            object_delete(&obj);

            square_note_spot(c, grid);
            square_light_spot(c, grid);
        }

        /* Next object */
        obj = next;
    }
}


/*
 * Monster movement
 */
static void monster_turn_move(struct source *who, struct chunk *c, struct monster *mon,
    const char *m_name, int dir, bool stagger, bool tracking, bool *did_something)
{
    struct monster_lore *lore = get_lore(who->player, mon->race);
    int i;
    struct loc grid;

    /* Target info */
    if (!who->monster)
        loc_copy(&grid, &who->player->grid);
    else
        loc_copy(&grid, &who->monster->grid);

    /* Process moves */
    for (i = 0; ((i < 5) && !(*did_something)); i++)
    {
        bool move;
        struct loc ngrid;

        /* Get the direction (or stagger) */
        int d = (stagger? ddd[randint0(8)]: side_dirs[dir][i]);

        /* Get the grid to step to or attack */
        next_grid(&ngrid, &mon->grid, d);

        /* Tracking monsters have their best direction, don't change */
        if ((i > 0) && !stagger && tracking) break;

        /* Check if we can move */
        move = monster_turn_can_move(who, c, mon, &ngrid, did_something);

        /* Hack -- a player is in the way */
        if (square_isplayer(c, &ngrid)) move = true;

        if (!move) continue;

        /* Try to break the glyph if there is one */
        if (square_iswarded(c, &ngrid) && !monster_turn_glyph(who->player, mon, &ngrid))
            continue;

        /* Break a decoy if there is one */
        if (square_isdecoyed(c, &ngrid))
            square_destroy_decoy(who->player, c, &ngrid);

        /* A player is in the way. */
        if (square_isplayer(c, &ngrid))
        {
            struct player *q = player_get(0 - square(c, &ngrid)->mon);

            /* Learn about if the monster attacks */
            if (monster_is_visible(who->player, mon->midx))
                rf_on(lore->flags, RF_NEVER_BLOW);

            /* Some monsters never attack */
            if (loc_eq(&ngrid, &grid) && rf_has(mon->race->flags, RF_NEVER_BLOW))
                continue;

            /* Otherwise, attack the player (skip if non hostile) */
            if (pvm_check(q, mon))
            {
                struct source target_body;
                struct source *target = &target_body;

                /* Do the attack */
                source_player(target, get_player_index(get_connection(q->conn)), q);
                make_attack_normal(mon, target);

                *did_something = true;
            }

            continue;
        }

        /* Some monsters never move */
        if (rf_has(mon->race->flags, RF_NEVER_MOVE))
        {
            /* Learn about lack of movement */
            if (monster_is_visible(who->player, mon->midx))
                rf_on(lore->flags, RF_NEVER_MOVE);

            continue;
        }

        /* A monster is in the way */
        if (square_monster(c, &ngrid))
        {
            if (!monster_turn_try_push(who, c, mon, m_name, &ngrid, did_something))
                continue;
        }

        /* Otherwise we can just move */
        else
        {
            monster_swap(c, &mon->grid, &ngrid);
            *did_something = true;
        }

        /* Scan all objects in the grid, if we reached it */
        if (mon == square_monster(c, &ngrid))
            monster_turn_grab_objects(who->player, c, mon, m_name, &ngrid);
    }

    if (*did_something)
    {
        /* Learn about no lack of movement */
        if (monster_is_visible(who->player, mon->midx))
            rf_on(lore->flags, RF_NEVER_MOVE);

        /* Possible disturb */
        if (!who->monster && monster_is_visible(who->player, mon->midx) &&
            monster_is_in_view(who->player, mon->midx) && OPT(who->player, disturb_near))
        {
            /* Disturb (except townies, friendlies and hidden mimics) */
            if ((mon->level > 0) && pvm_check(who->player, mon) && !monster_is_camouflaged(mon))
                disturb(who->player, (who->player->firing_request? 2: 0));
        }
    }
}


/*
 * Process a monster's turn
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level.  This should prevent the level from being "swamped" by
 * reproducing monsters.  It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it.  Actually,
 * the same thing happens to normal monsters when they hit a door.
 *
 * In addition, monsters which *cannot* open or bash down a door
 * will still stand there trying to open it...  XXX XXX XXX
 *
 * Technically, need to check for monster in the way combined
 * with that monster being in a wall (or door?) XXX
 */
static void monster_turn(struct source *who, struct chunk *c, struct monster *mon,
    int target_m_dis)
{
    bool did_something = false;
    int dir = 0;
    bool stagger = false;
    bool tracking = false;
    char m_name[NORMAL_WID];

    /* Get the monster name */
    monster_desc(who->player, m_name, sizeof(m_name), mon, MDESC_CAPITAL | MDESC_IND_HID);

    /* Generate monster drops */
    if (!who->monster) mon_create_drops(who->player, c, mon);

    /* Try to multiply - this can use up a turn */
    if (!who->monster && monster_turn_multiply(c, mon)) return;

    /* Attempt to cast a spell (skip if non hostile) */
    if (who->monster || pvm_check(who->player, mon))
    {
        if (make_attack_spell(who, c, mon, target_m_dis)) return;
    }

    /* Work out what kind of movement to use - AI or staggered movement */
    if (!monster_turn_should_stagger(who->player, mon))
    {
        if (!get_move(who, c, mon, &dir, &tracking)) return;
    }
    else
        stagger = true;

    /* Movement */
    monster_turn_move(who, c, mon, m_name, dir, stagger, tracking, &did_something);

    if (rf_has(mon->race->flags, RF_HAS_LIGHT))
        who->player->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Hack -- get "bold" if out of options */
    if (!did_something && mon->m_timed[MON_TMD_FEAR])
        mon_clear_timed(who->player, mon, MON_TMD_FEAR, MON_TMD_FLG_NOTIFY);

    /* If we see a hidden monster do something, become aware of it */
    if (did_something && monster_is_camouflaged(mon)) become_aware(who->player, c, mon);
}


/*
 * Processing routines that happen to a monster regardless of whether it
 * gets a turn, and/or to decide whether it gets a turn
 */


/*
 * Find the closest target
 */
static struct monster *get_closest_target(struct chunk *c, struct monster *mon, int *target_dis,
    bool *target_los)
{
    int i, j;
    struct monster *target_mon = NULL;
    int target_m_dis = 9999, target_m_hp = 99999;
    bool target_m_los = false, new_los;

    /* Process the monsters */
    for (i = cave_monster_max(c) - 1; i >= 1; i--)
    {
        /* Access the monster */
        struct monster *current_m_ptr = cave_monster(c, i);

        /* Skip "dead" monsters */
        if (!current_m_ptr->race) continue;

        /* Skip the origin */
        if (current_m_ptr == mon) continue;

        /* Skip controlled monsters */
        if (master_in_party(current_m_ptr->master, mon->master)) continue;

        /* Compute distance */
        j = distance(&current_m_ptr->grid, &mon->grid);

        /* Check if monster has LOS to the target */
        new_los = los(c, &mon->grid, &current_m_ptr->grid);

        /* Check that the closest VISIBLE target gets selected */
        /* If no visible one available just take the closest */
        if (((target_m_los >= new_los) && (j > target_m_dis)) || (target_m_los > new_los))
            continue;

        /* Skip if same distance and stronger and same visibility */
        if ((j == target_m_dis) && (current_m_ptr->hp > target_m_hp) && (target_m_los == new_los))
            continue;

        /* Remember this target */
        target_m_los = new_los;
        target_m_dis = j;
        target_mon = current_m_ptr;
        target_m_hp = current_m_ptr->hp;
    }

    /* Forget player status */
    of_wipe(mon->known_pstate.flags);
    pf_wipe(mon->known_pstate.pflags);
    for (i = 0; i < ELEM_MAX; i++)
        mon->known_pstate.el_info[i].res_level = 0;

    /* Always track closest target */
    (*target_dis) = target_m_dis;
    (*target_los) = target_m_los;
    return target_mon;
}


/*
 * Determine whether a monster is active or passive
 */
static bool monster_check_active(struct chunk *c, struct monster *mon, int *target_m_dis, bool *mvm,
    struct source *who)
{
    struct player *p = mon->closest_player;
    bool target_m_los, is_hurt = false, can_hear = false, can_smell = false;

    /* Hack -- MvM */
    if (mon->status == MSTATUS_ATTACK)
    {
        /* Find the closest monster */
        struct monster *target_mon = get_closest_target(c, mon, target_m_dis, &target_m_los);

        /* Paranoia -- make sure we found a closest monster */
        if (target_mon)
        {
            /* Bypass MvM if a player is closest */
            if (master_in_party(mon->master, p->id) || (mon->cdis >= *target_m_dis))
            {
                *mvm = true;
                source_both(who, p, target_mon);
            }
        }
    }
    if (!(*mvm))
    {
        *target_m_dis = mon->cdis;
        target_m_los = square_isview(p, &mon->grid);
        is_hurt = ((mon->hp < mon->maxhp)? true: false);
        can_hear = monster_can_hear(p, mon);
        can_smell = monster_can_smell(p, mon);
        source_player(who, get_player_index(get_connection(p->conn)), p);
    }

    /* Character is inside scanning range, monster can go straight there */
    if (*target_m_dis <= mon->race->hearing) return true;

    /* Monster is hurt */
    if (is_hurt) return true;

    /* Monster can "see" the player (checked backwards) */
    if (target_m_los) return true;

    /* Monster can hear the player */
    if (can_hear) return true;

    /* Monster can smell the player */
    if (can_smell) return true;

    /* Otherwise go passive */
    return false;
}


/*
 * Handle fear, poison, bleeding
 */
static void monster_effects(struct player *p, struct monster *mon)
{
    struct source who_body;
    struct source *who = &who_body;

    source_monster(who, mon);

    /* Handle "fear" */
    if (mon->m_timed[MON_TMD_FEAR])
    {
        /* Amount of "boldness" */
        int d = randint1(mon->level / 10 + 1);

        mon_dec_timed(p, mon, MON_TMD_FEAR, d, MON_TMD_FLG_NOTIFY);
    }

    /* Handle poison */
    if (mon->m_timed[MON_TMD_POIS])
    {
        /* Amount of "boldness" */
        int d = randint1(mon->level / 10 + 1);

        if (mon->m_timed[MON_TMD_POIS] > d)
        {
            /* Reduce the poison */
            mon_dec_timed(p, mon, MON_TMD_POIS, d, MON_TMD_FLG_NOMESSAGE);

            /* Take damage */
            if (mon->hp > 0)
            {
                mon->hp--;

                /* Unconscious state - message if visible */
                if ((mon->hp == 0) && monster_is_visible(p, mon->midx))
                {
                    /* Dump a message */
                    add_monster_message(p, mon, MON_MSG_CROAK, true);
                }

                /* Hack -- update the health bar */
                if (monster_is_visible(p, mon->midx)) update_health(who);
            }
        }
        else
            mon_clear_timed(p, mon, MON_TMD_POIS, MON_TMD_FLG_NOTIFY);
    }

    /* Handle bleeding */
    if (mon->m_timed[MON_TMD_CUT])
    {
        /* Amount of "boldness" */
        int d = randint1(mon->level / 10 + 1);

        if (mon->m_timed[MON_TMD_CUT] > d)
        {
            /* Reduce the bleeding */
            mon_dec_timed(p, mon, MON_TMD_CUT, d, MON_TMD_FLG_NOMESSAGE);

            /* Take damage */
            if (mon->hp > 0)
            {
                mon->hp--;

                /* Unconscious state - message if visible */
                if ((mon->hp == 0) && monster_is_visible(p, mon->midx))
                {
                    /* Dump a message */
                    add_monster_message(p, mon, MON_MSG_CROAK, true);
                }

                /* Hack -- update the health bar */
                if (monster_is_visible(p, mon->midx)) update_health(who);
            }
        }
        else
            mon_clear_timed(p, mon, MON_TMD_CUT, MON_TMD_FLG_NOTIFY);
    }
}


/*
 * Wake a monster or reduce its depth of sleep
 *
 * Chance of waking up is dependent only on the player's stealth, but the
 * amount of sleep reduction takes into account the monster's distance from
 * the player. Currently straight line distance is used; possibly this
 * should take into account dungeon structure.
 */
static void monster_reduce_sleep(struct monster *mon, bool mvm)
{
    struct player *p = mon->closest_player;
    bool woke_up = false;
    int stealth = p->state.skills[SKILL_STEALTH];
    int player_noise;
    int notice = randint0(1024);
    struct monster_lore *lore = get_lore(p, mon->race);

    /* PWMAngband: idle players are less susceptible to get noticed */
    if (has_energy(p, false)) stealth += 3;
    stealth = MIN(stealth, 30);
    player_noise = 1 << (30 - stealth);

    /* MvM or aggravation */
    if (mvm || player_of_has(p, OF_AGGRAVATE))
    {
        /* Wake the monster */
        mon_clear_timed(p, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOTIFY);

        woke_up = true;
    }

    /* Hack -- see if monster "notices" player */
    else if ((notice * notice * notice) <= player_noise)
    {
        int sleep_reduction = 1;
        int local_noise = p->cave->noise.grids[mon->grid.y][mon->grid.x];

        /* Wake up faster in hearing distance of the player */
        if ((local_noise > 0) && (local_noise < 50)) sleep_reduction = (100 / local_noise);

        /* Note a complete wakeup */
        if (mon->m_timed[MON_TMD_SLEEP] <= sleep_reduction) woke_up = true;

        /* Monster wakes up a bit */
        mon_dec_timed(p, mon, MON_TMD_SLEEP, sleep_reduction, MON_TMD_FLG_NOTIFY);

        /* Update knowledge */
        if (monster_is_obvious(p, mon->midx, mon))
        {
            if (!woke_up && (lore->ignore < UCHAR_MAX)) lore->ignore++;
            else if (woke_up && (lore->wake < UCHAR_MAX)) lore->wake++;
            lore_update(mon->race, lore);
        }
    }
}


/*
 * Process a monster's timed effects, e.g. decrease them.
 *
 * Returns true if the monster is skipping its turn.
 */
static bool process_monster_timed(struct monster *mon, bool mvm)
{
    struct player *p = mon->closest_player;

    /* If the monster is asleep or just woke up, then it doesn't act */
    if (mon->m_timed[MON_TMD_SLEEP])
    {
        monster_reduce_sleep(mon, mvm);
        return true;
    }

    if (mon->m_timed[MON_TMD_FAST])
        mon_dec_timed(p, mon, MON_TMD_FAST, 1, 0);

    if (mon->m_timed[MON_TMD_SLOW])
        mon_dec_timed(p, mon, MON_TMD_SLOW, 1, 0);

    if (mon->m_timed[MON_TMD_HOLD])
        mon_dec_timed(p, mon, MON_TMD_HOLD, 1, 0);

    if (mon->m_timed[MON_TMD_DISEN])
        mon_dec_timed(p, mon, MON_TMD_DISEN, 1, 0);

    if (mon->m_timed[MON_TMD_STUN])
        mon_dec_timed(p, mon, MON_TMD_STUN, 1, MON_TMD_FLG_NOTIFY);

    if (mon->m_timed[MON_TMD_CONF])
        mon_dec_timed(p, mon, MON_TMD_CONF, 1, MON_TMD_FLG_NOTIFY);

    if (mon->m_timed[MON_TMD_BLIND])
        mon_dec_timed(p, mon, MON_TMD_BLIND, 1, MON_TMD_FLG_NOTIFY);

    /* Handle fear, poison, bleeding */
    monster_effects(p, mon);

    /* One in __ chance of missing turn if stunned; always miss if held */
    if (mon->m_timed[MON_TMD_STUN] && one_in_(STUN_MISS_CHANCE)) return true;
    if (mon->m_timed[MON_TMD_HOLD]) return true;

    return false;
}


/*
 * Monster regeneration of HPs.
 */
static void regen_monster(struct monster *mon)
{
    /* Skip poisoned and bleeding monsters! */
    if (mon->m_timed[MON_TMD_POIS] || mon->m_timed[MON_TMD_CUT]) return;

    /* Regenerate (if needed) */
    if (mon->hp < mon->maxhp)
    {
        /* Base regeneration */
        int frac = mon->maxhp / 100;

        struct source who_body;
        struct source *who = &who_body;

        /* Minimal regeneration rate */
        if (!frac) frac = 1;

        /* Some monsters regenerate quickly */
        if (rf_has(mon->race->flags, RF_REGENERATE)) frac *= 2;

        /* Regenerate */
        mon->hp += frac;

        /* Do not over-regenerate */
        if (mon->hp > mon->maxhp) mon->hp = mon->maxhp;

        /* Update health bars */
        source_monster(who, mon);
        update_health(who);
    }
}


/*
 * Monster processing routines to be called by the main game loop
 */


/*
 * Get the player closest to a monster and update the distance to that player.
 *
 * Code from update_mon()... maybe we should simply call update_mon() instead.
 *
 * Note that this is necessary because process_monsters() is called after
 * process_players() which sets upkeep->new_level_method for players leaving a level...
 * but before generate_new_level() which actually creates the new level for the
 * players, thus making closest_player obsolete in the meantime.
 */
static void get_closest_player(struct chunk *c, struct monster *mon)
{
    int i;
    struct player *closest = NULL;
    int dis_to_closest = 9999, lowhp = 9999;
    bool blos = false, new_los;

    /* Check for each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);
        int d;

        /* Make sure he's on the same dungeon level */
        if (!wpos_eq(&p->wpos, &mon->wpos)) continue;

        /* Hack -- skip him if he's shopping */
        if (in_store(p)) continue;

        /* Hack -- make the dungeon master invisible to monsters */
        if (p->dm_flags & DM_MONSTER_FRIEND) continue;

        /* Skip player if dead or gone */
        if (!p->alive || p->is_dead || p->upkeep->new_level_method) continue;

        /* Wanderers ignore level 1 players unless hurt or aggravated */
        if (rf_has(mon->race->flags, RF_WANDERER) && (p->lev == 1) &&
            !player_of_has(p, OF_AGGRAVATE) && (mon->hp == mon->maxhp))
        {
            continue;
        }

        /* Compute distance */
        d = distance(&p->grid, &mon->grid);

        /* Restrict distance */
        if (d > 255) d = 255;

        /* Check if monster has LOS to the player */
        new_los = los(c, &mon->grid, &p->grid);

        /* Remember this player if closest */
        if (is_closest(p, mon, blos, new_los, d, dis_to_closest, lowhp))
        {
            blos = new_los;
            dis_to_closest = d;
            closest = p;
            lowhp = p->chp;
        }
    }

    /* Forget player status */
    if (closest != mon->closest_player)
    {
        of_wipe(mon->known_pstate.flags);
        pf_wipe(mon->known_pstate.pflags);
        for (i = 0; i < ELEM_MAX; i++)
            mon->known_pstate.el_info[i].res_level = 0;
    }

    /* Always track closest player */
    mon->closest_player = closest;

    /* Paranoia -- make sure we found a closest player */
    if (closest) mon->cdis = dis_to_closest;
}


/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), allowing fully
 * energized monsters to move, attack, pass, etc.
 *
 * This function and its children are responsible for a considerable fraction
 * of the processor time in normal situations, greater if the character is
 * resting.
 */
void process_monsters(struct chunk *c, bool more_energy)
{
    int i, j, time;

    /* Only process some things every so often */
    bool regen;

    /* Process the monsters (backwards) */
    for (i = cave_monster_max(c) - 1; i >= 1; i--)
    {
        struct monster *mon;
        int target_m_dis;
        bool mvm = false;
        struct source who_body;
        struct source *who = &who_body;

        /* Get a 'live' monster */
        mon = cave_monster(c, i);
        if (!mon->race) continue;

        /* Ignore monsters that have already been handled */
        if (mon->handled) continue;

        /* Skip "unconscious" monsters */
        if (mon->hp == 0) continue;

        /* Not enough energy to move yet */
        if (more_energy && mon->closest_player && (mon->energy <= mon->closest_player->energy))
            continue;

        /* Prevent reprocessing */
        mon->handled = true;

        /* Regenerate hitpoints and mana every 100 "scaled" turns */
        regen = false;
        time = move_energy(c->wpos.depth) / time_factor(mon->closest_player, c);
        if (!(turn.turn % time)) regen = true;

        /* Handle monster regeneration if requested */
        if (regen) regen_monster(mon);

        /* End the turn of monsters without enough energy to move */
        if (mon->energy < move_energy(mon->wpos.depth)) continue;

        /* Use up "some" energy */
        mon->energy -= move_energy(mon->wpos.depth);

        /* Hack -- controlled monsters have a limited lifespan */
        if (mon->master && mon->lifespan)
        {
            mon->lifespan--;

            /* Delete the monster */
            if (!mon->lifespan)
            {
                update_monlist(mon);
                delete_monster_idx(c, i);
                continue;
            }
        }

        /* Hack -- controlled monsters need to "sense" their master */
        if (mon->master)
        {
            /* Check everyone */
            for (j = 1; j <= NumPlayers; j++)
            {
                struct player *q = player_get(j);
                int d;

                /* Find the master */
                if (q->id != mon->master) continue;

                /* Compute distance */
                d = distance(&q->grid, &mon->grid);

                /* Player is inside scanning range */
                if (d <= mon->race->hearing) break;

                /* The monster is visible */
                if (monster_is_visible(q, mon->midx)) break;

                /* Sometimes free monster from slavery */
                if (one_in_(100))
                {
                    msg(q, "You feel a change of heart in a being close to you.");
                    monster_set_master(mon, NULL, MSTATUS_HOSTILE);
                }
            }
        }

        /* Get closest player */
        get_closest_player(c, mon);

        /* Paranoia -- make sure we have a closest player */
        if (!mon->closest_player) continue;

        /* Mimics lie in wait */
        if (monster_is_camouflaged(mon)) continue;

        /* Check if the monster is active */
        if (monster_check_active(c, mon, &target_m_dis, &mvm, who))
        {
            /* Process timed effects - skip turn if necessary */
            if (process_monster_timed(mon, mvm)) continue;

            /* The monster takes its turn */
            monster_turn(who, c, mon, target_m_dis);
        }
    }

    /* Efficiency */
    if (!c->scan_monsters) return;

    /* Every 5 game turns */
    if (turn.turn % 5) return;

    /* Shimmer multi-hued monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);

        if (!mon->race) continue;
        if (!monster_shimmer(mon->race)) continue;

        /* Check everyone */
        for (j = 1; j <= NumPlayers; j++)
        {
            struct player *q = player_get(j);

            /* If he's not here, skip him */
            if (!wpos_eq(&q->wpos, &mon->wpos)) continue;

            /* Actually light that spot for that player */
            if (allow_shimmer(q)) square_light_spot_aux(q, c, &mon->grid);
        }
    }
}


void reset_monsters(struct chunk *c)
{
    int i;
    struct monster *mon;

    /* Process the monsters (backwards) */
    for (i = cave_monster_max(c) - 1; i >= 1; i--)
    {
        /* Access the monster */
        mon = cave_monster(c, i);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Dungeon hurts monsters */
        monster_take_terrain_damage(c, mon);

        /* Monster is ready to go again */
        mon->handled = false;
    }
}


/*
 * Determine whether the player is invisible to a monster
 */
static bool player_invis(struct player *p, struct monster *mon)
{
    s16b mlv;

    /* Player should be invisible */
    if (!p->timed[TMD_INVIS]) return false;

    /* Aggravation breaks invisibility */
    if (player_of_has(p, OF_AGGRAVATE)) return false;

    /* Can't fool a monster once injured */
    if (mon->hp < mon->maxhp) return false;

    /* Questor and very powerful monsters see invisible */
    if (rf_has(mon->race->flags, RF_QUESTOR)) return false;
    if (rf_has(mon->race->flags, RF_PWMANG_FIXED)) return false;

    /* Invisible monsters see invisible */
    if (monster_is_invisible(mon)) return false;

    mlv = (s16b)mon->level;
    if (rf_has(mon->race->flags, RF_NO_SLEEP)) mlv += 10;
    if (rf_has(mon->race->flags, RF_DRAGON)) mlv += 20;
    if (rf_has(mon->race->flags, RF_UNDEAD)) mlv += 15;
    if (rf_has(mon->race->flags, RF_DEMON)) mlv += 15;
    if (rf_has(mon->race->flags, RF_ANIMAL)) mlv += 15;
    if (rf_has(mon->race->flags, RF_ORC)) mlv -= 15;
    if (rf_has(mon->race->flags, RF_TROLL)) mlv -= 10;
    if (monster_is_stupid(mon->race)) mlv /= 2;
    if (monster_is_smart(mon->race)) mlv = (mlv * 5) / 4;
    if (monster_is_unique(mon->race)) mlv *= 2;
    if ((p->timed[TMD_INVIS] == -1) && !p->ghost) mlv = (mlv * 7) / 10;
    if (mlv < 0) mlv = 0;

    /* High level monsters can't be fooled */
    if (mlv > p->lev) return false;

    /* Player is invisible */
    return true;
}


/*
 * Determine whether the player is closest to a monster
 */
bool is_closest(struct player *p, struct monster *mon, bool blos, bool new_los, int j,
    int dis_to_closest, int lowhp)
{
    /* Followers will try to reach their master */
    if ((mon->status == MSTATUS_FOLLOW) && (mon->master != p->id)) return false;

    /* Skip guards */
    if (master_in_party(mon->master, p->id) && (mon->status == MSTATUS_GUARD))
        return false;

    /* Skip if the monster can't see the player */
    if (player_invis(p, mon)) return false;

    /* Check that the closest VISIBLE target gets selected */
    /* If no visible one available just take the closest */
    if (((blos >= new_los) && (j > dis_to_closest)) || (blos > new_los))
        return false;

    /* Skip if same distance and stronger and same visibility */
    if ((j == dis_to_closest) && (p->chp > lowhp) && (blos == new_los))
        return false;

    return true;
}
