/*
 * File: mon-move.c
 * Purpose: Monster movement
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke (attacking code)
 * Copyright (c) 1997 Ben Harrison, David Reeve Sward, Keldon Jones (AI routines).
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
 * Routines to enable decisions on monster behaviour
 */


/*
 * Find whether a monster is near a permanent wall
 *
 * This decides whether PASS_WALL & KILL_WALL monsters use the monster flow code
 */
static bool monster_near_permwall(struct player *p, const struct monster *mon, struct chunk *c)
{
    struct loc gp[512];
    int path_grids, j;

    /* If player is in LOS, there's no need to go around walls */
    if (projectable(p, c, &((struct monster *)mon)->grid, &p->grid, PROJECT_SHORT, false))
        return false;

    /* Find the shortest path */
    path_grids = project_path(p, gp, z_info->max_sight, c, &((struct monster *)mon)->grid, &p->grid,
        PROJECT_ROCK);

    /* See if we can "see" the player without hitting permanent wall */
    for (j = 0; j < path_grids; j++)
    {
        if (square_isunpassable(c, &gp[j])) return true;
        if (loc_eq(&gp[j], &((struct monster *)mon)->old_grid)) return true;
        if (loc_eq(&gp[j], &p->grid)) return false;
    }

    return false;
}


/*
 * Check if the monster can see the player
 */
static bool monster_can_see_player(struct player *p, struct monster *mon)
{
    if (!square_isview(p, &mon->grid)) return false;
    if (p->timed[TMD_COVERTRACKS] && (mon->cdis > z_info->max_sight / 4)) return false;
    return true;
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
    u32b mexp1 = (mon1->original_race? mon1->original_race->mexp: mon1->race->mexp);
    u32b mexp2 = (mon2->original_race? mon2->original_race->mexp: mon2->race->mexp);

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

    /* No monster */
    if (!mon1) return true;

    /* No trampling uniques */
    if (rf_has(mon1->race->flags, RF_UNIQUE)) return false;

    if (rf_has(mon->race->flags, RF_KILL_BODY) && (compare_monsters(mon, mon1) > 0)) return true;

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
 * Check if the monster (defined by race) can occupy a grid safely
 */
bool race_hates_grid(struct chunk *c, struct monster_race *race, struct loc *grid)
{
    /* Only some creatures can handle damaging terrain */
    if (square_isdamaging(c, grid) && !rf_has(race->flags, square_feat(c, grid)->resist_flag))
    {
        /* Hack -- passwall creatures can cross any damaging terrain */
        if (rf_has(race->flags, RF_PASS_WALL)) return false;

        /* Hack -- levitating creatures can cross water */
        if (square_iswater(c, grid) && rf_has(race->flags, RF_LEVITATE)) return false;

        return true;
    }

    /* Aquatic monsters suffocate if not in water */
    if (!square_iswater(c, grid) && rf_has(race->flags, RF_AQUATIC)) return true;

    return false;
}


/*
 * Check if the monster can occupy a grid safely
 */
bool monster_hates_grid(struct chunk *c, struct monster *mon, struct loc *grid)
{
    return race_hates_grid(c, mon->race, grid);
}


/*
 * Monster movement routines
 * These routines, culminating in get_move(), choose if and where a monster
 * will move on its turn
 */


/*
 * Calculate minimum and desired combat ranges
 *
 * Afraid monsters will set this to their maximum flight distance.
 * Currently this is recalculated every turn - if it becomes a significant
 * overhead it could be calculated only when something has changed (monster HP,
 * chance of escaping, etc)
 */
static void get_move_find_range(struct player *p, struct monster *mon)
{
    u16b p_lev, m_lev;
    u16b p_chp, p_mhp;
    u32b m_chp, m_mhp;
    u32b p_val, m_val;

    /* Monsters will run up to z_info->flee_range grids out of sight */
    int flee_range = z_info->max_sight + z_info->flee_range;

    /* Controlled monsters won't run away */
    if (master_in_party(mon->master, p->id))
        mon->min_range = 1;

    /* All "afraid" monsters will run away */
    else if (mon->m_timed[MON_TMD_FEAR] || rf_has(mon->race->flags, RF_FRIGHTENED))
        mon->min_range = flee_range;

    /* Bodyguards don't flee */
    else if (mon->group_info[PRIMARY_GROUP].role == MON_GROUP_BODYGUARD)
        mon->min_range = 1;

    else
    {
        /* Minimum distance - stay at least this far if possible */
        mon->min_range = 1;

        /* Taunted monsters just want to get in your face */
        if (p->timed[TMD_TAUNT]) return;

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

    /* Archers are quite happy at a good distance */
    if (monster_loves_archery(mon)) mon->best_range += 3;

    if (mon->race->freq_spell > 24)
    {
        /* Breathers like point blank range */
        if (monster_breathes(mon) && (mon->best_range < 6) && (mon->hp > mon->maxhp / 2))
            mon->best_range = MAX(6, mon->best_range);

        /* Other spell casters will sit back and cast */
        else
            mon->best_range += 3;
    }
}


/*
 * Choose the best direction for a bodyguard.
 *
 * The idea is to stay close to the group leader, but attack the player if the
 * chance arises
 */
static bool get_move_bodyguard(struct player *p, struct chunk *c, struct monster *mon)
{
    int i;
    struct monster *leader = monster_group_leader(c, mon);
    int dist;
    struct loc best;
    bool found = false;

    if (!leader) return false;

    /* Get distance */
    dist = distance(&mon->grid, &leader->grid);

    /* If currently adjacent to the leader, we can afford a move */
    if (dist <= 1) return false;

    /* If the leader's too out of sight and far away, save yourself */
    if (!los(c, &mon->grid, &leader->grid) && (dist > 10)) return false;

    /* Check nearby adjacent grids and assess */
    for (i = 0; i < 8; i++)
    {
        /* Get the location */
        struct loc grid;
        int new_dist;
        int char_dist;

        loc_sum(&grid, &mon->grid, &ddgrid_ddd[i]);

        /* Bounds check */
        if (!square_in_bounds(c, &grid)) continue;

        /* There's a monster blocking that we can't deal with */
        if (!monster_can_kill(c, mon, &grid) && !monster_can_move(c, mon, &grid))
            continue;

        /* There's damaging terrain */
        if (monster_hates_grid(c, mon, &grid)) continue;

        new_dist = distance(&grid, &leader->grid);
        char_dist = distance(&grid, &p->grid);

        /* Closer to the leader is always better */
        if (new_dist < dist)
        {
            loc_copy(&best, &grid);
            found = true;

            /* If there's a grid that's also closer to the player, that wins */
            if (char_dist < mon->cdis) break;
        }
    }

    /* If we found one, set the target */
    if (found)
    {
        loc_copy(&mon->target.grid, &best);
        return true;
    }

    return false;
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
 * Ghosts and rock-eaters generally just head straight for the player. Other
 * monsters try sight, then current sound as saved in c->noise.grids[y][x],
 * then current scent as saved in c->scent.grids[y][x].
 *
 * This function assumes the monster is moving to an adjacent grid, and so the
 * noise can be louder by at most 1. The monster target grid set by sound or
 * scent tracking in this function will be a grid they can step to in one turn,
 * so is the preferred option for get_move() unless there's some reason
 * not to use it.
 *
 * Tracking by 'scent' means that monsters end up near enough the player to
 * switch to 'sound' (noise), or they end up somewhere the player left via
 * teleport. Teleporting away from a location will cause the monsters who
 * were chasing the player to converge on that location as long as the player
 * is still near enough to "annoy" them without being close enough to chase
 * directly.
 */
static bool get_move_advance(struct player *p, struct chunk *c, struct monster *mon, bool *track)
{
    int i, n = 0;
    struct loc *decoy = cave_find_decoy(c), target;
    int base_hearing = mon->race->hearing - p->state.skills[SKILL_STEALTH] / 3;
    int best_scent, max_scent;
    int best_noise, max_noise;
    struct loc best_grid[8];

    if (!loc_is_zero(decoy))
        loc_copy(&target, decoy);
    else
    {
        loc_copy(&target, &p->grid);

        /* PWMAngband: head straight for the player if wraithed in walls */
        if (player_walled(p, c))
        {
            loc_copy(&mon->target.grid, &target);
            *track = true;
            return true;
        }
    }

    /* Bodyguards are special */
    if ((mon->group_info[PRIMARY_GROUP].role == MON_GROUP_BODYGUARD) && get_move_bodyguard(p, c, mon))
        return true;

    /* If the monster can pass through nearby walls, do that */
    if (monster_passes_walls(mon->race) && !monster_near_permwall(p, mon, c))
    {
        loc_copy(&mon->target.grid, &target);
        *track = true;
        return true;
    }

    /* If the player can see monster, set target and run towards them */
    if (monster_can_see_player(p, mon))
    {
        int num_path_grids;
        struct loc path_grid[512];

        /* Check path for damaging grid */
        num_path_grids = project_path(NULL, path_grid, z_info->max_range, c, &mon->grid, &target, 0);
        if ((num_path_grids > 0) && !monster_hates_grid(c, mon, &path_grid[0]))
        {
          loc_copy(&mon->target.grid, &target);
          *track = true;
          return true;
        }
    }

    for (i = 0; i < 8; i++) loc_init(&best_grid[i], 0, 0);

    /* Try to use sound */
    if (monster_can_hear(p, mon))
    {
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
            *track = true;
            return true;
        }
    }

    /* If both vision and sound are no good, use scent */
    if (monster_can_smell(p, mon))
    {
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
            *track = true;
            return true;
        }
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
            if (!square_isview(p, &grid) && projectable(p, c, &mon->grid, &grid, PROJECT_STOP, true))
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
    struct loc best;
    int best_score = -1;

    /* Taking damage from terrain makes moving vital */
    if (!monster_taking_terrain_damage(chunk_get(&p->wpos), mon))
    {
        /* If the player is not currently near the monster, no reason to flow */
        if (mon->cdis >= mon->best_range) return false;

        /* Monster is too far away to use sound or scent */
        if (!monster_can_hear(p, mon) && !monster_can_smell(p, mon)) return false;
    }

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
        loc_copy(&best, &grid);
    }

    /* Set the immediate target */
    loc_copy(&mon->target.grid, &best);

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
 * Choose a random passable grid adjacent to the monster since is has no better
 * strategy.
 */
static void get_move_random(struct chunk *c, struct monster *mon, struct loc *grid)
{
    int attempts[8] = {0, 1, 2, 3, 4, 5, 6, 7};
    int nleft = 8;

    while (nleft > 0)
    {
        int itry = randint0(nleft);
        int tmp = attempts[itry];
        struct loc trygrid;

        loc_sum(&trygrid, &mon->grid, &ddgrid_ddd[tmp]);
        if (square_is_monster_walkable(c, &trygrid) && !monster_hates_grid(c, mon, &trygrid))
        {
            loc_copy(grid, &ddgrid_ddd[tmp]);
            return;
        }

        --nleft;
        attempts[itry] = attempts[nleft];
        attempts[nleft] = tmp;
    }
}


/*
 * Choose "logical" directions for monster movement
 *
 * This function is responsible for deciding where the monster wants to move,
 * and so is the core of monster "AI".
 *
 * First, we work out how best to advance toward the player:
 * - Try to head toward the player directly if we can pass through walls or
 *   if we can see them
 * - Failing that follow the player by sound, or failing that by scent
 * - If none of that works, just head in the general direction
 * Then we look at possible reasons not to just advance:
 * - If we're part of a pack, try to lure the player into the open
 * - If we're afraid, try to find a safe place to run to, and if no safe place
 *   just run in the opposite direction to the advance move
 * - If we can see the player and we're part of a group, try and surround them
 *
 * The function then returns false if we're already where we want to be, and
 * otherwise sets the chosen direction to step and returns true.
 */
static bool get_move(struct source *who, struct chunk *c, struct monster *mon, int *dir, bool *good)
{
    struct loc *decoy = cave_find_decoy(c), target;

    /* Offset to current position to move toward */
    struct loc grid;

    /* Monsters will run up to z_info->flee_range grids out of sight */
    int flee_range = z_info->max_sight + z_info->flee_range;

    bool done = false;

    loc_init(&grid, 0, 0);

    if (!loc_is_zero(decoy))
        loc_copy(&target, decoy);
    else
        loc_copy(&target, &who->player->grid);

    /* Calculate range */
    if (!who->monster) get_move_find_range(who->player, mon);

    /* Assume we're heading towards the player */
    if (!who->monster)
    {
        /* We have a good move, use it */
        if (get_move_advance(who->player, c, mon, good))
        {
            loc_diff(&grid, &mon->target.grid, &mon->grid);
            mflag_on(mon->mflag, MFLAG_TRACKING);
        }

        /* Try to follow someone who knows where they're going */
        else
        {
            struct monster *tracker = group_monster_tracking(c, mon);

            /* Need los? */
            if (tracker && los(c, &mon->grid, &tracker->grid))
            {
                loc_diff(&grid, &tracker->grid, &mon->grid);

                /* No longer tracking */
                mflag_off(mon->mflag, MFLAG_TRACKING);
            }
            else
            {
                /* Keep heading to the most recent goal. */
                if (mflag_has(mon->mflag, MFLAG_TRACKING))
                    loc_diff(&grid, &mon->target.grid, &mon->grid);

                /* Try a random move and no longer track. */
                if (loc_is_zero(&grid))
                {
                    get_move_random(c, mon, &grid);
                    mflag_off(mon->mflag, MFLAG_TRACKING);
                }
            }
        }
    }
    else
    {
        /* Head straight for the monster */
        loc_diff(&grid, &who->monster->grid, &mon->grid);
    }

    /* Player */
    if (!who->monster)
    {
        bool group_ai = (rf_has(mon->race->flags, RF_GROUP_AI) && !monster_passes_walls(mon->race));

        /* Monster is taking damage from terrain */
        if (monster_taking_terrain_damage(c, mon))
        {
            /* Try to find safe place */
            if (get_move_find_safety(who->player, c, mon))
            {
                /* Set a course for the safe place */
                get_move_flee(who->player, mon);
                loc_diff(&grid, &mon->target.grid, &mon->grid);
                done = true;
            }
        }

        /* Normal animal packs try to get the player out of corridors. */
        if (!done && group_ai)
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
                /* Find hiding place for an ambush */
                if (get_move_find_hiding(who->player, c, mon))
                {
                    done = true;
                    loc_diff(&grid, &mon->target.grid, &mon->grid);

                    /* No longer tracking */
                    mflag_off(mon->mflag, MFLAG_TRACKING);
                }
            }
        }

        /* Not hiding and monster is afraid */
        if (!done && (mon->min_range == flee_range))
        {
            /* Try to find safe place */
            if (get_move_find_safety(who->player, c, mon))
            {
                /* Set a course for the safe place */
                get_move_flee(who->player, mon);
                loc_diff(&grid, &mon->target.grid, &mon->grid);
            }
            else
            {
                /* Just leg it away from the player */
                grid.y = 0 - grid.y;
                grid.x = 0 - grid.x;
            }

            /* No longer tracking */
            mflag_off(mon->mflag, MFLAG_TRACKING);

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
    struct monster_group_info info = {0, 0};

    my_assert(mon);

    /* Paranoia */
    if (!p) return false;

    /* Only on random levels */
    if (!random_level(&p->wpos)) return false;

    /* No uniques */
    if (monster_is_unique(mon->race)) return false;

    /* Limit number of clones */
    if (c->num_repro == z_info->repro_monster_max) return false;

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
        result = place_new_monster(p, c, &grid, mon->race, MON_CLONE, &info, ORIGIN_DROP_BREED);

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


enum monster_stagger
{
    NO_STAGGER = 0,
    CONFUSED_STAGGER = 1,
    INNATE_STAGGER = 2
};


/*
 * Check if a monster should stagger (that is, step at random) or not.
 * Always stagger when confused, but also deal with random movement for
 * RAND_25 and RAND_50 monsters.
 */
static enum monster_stagger monster_turn_should_stagger(struct player *p, struct monster *mon)
{
    struct monster_lore *lore = get_lore(p, mon->race);
    int chance, accuracy, confused_chance, roll;

    /* Blind */
    if (mon->m_timed[MON_TMD_BLIND]) return true;

    /* Increase chance of being erratic for every level of confusion */
    accuracy = monster_effect_accuracy(mon, MON_TMD_CONF, CONF_ERRATIC_CHANCE);
    chance = 100 - accuracy;
    confused_chance = chance;

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

    roll = randint0(100);
    return ((roll < confused_chance)? CONFUSED_STAGGER: ((roll < chance)? INNATE_STAGGER: NO_STAGGER));
}


/*
 * Helper function for monster_turn_can_move() to display a message for a
 * confused monster moving into non-passable terrain.
 */
static void monster_display_confused_move_msg(struct player *p, struct monster *mon,
    const char *m_name, struct chunk *c, struct loc *grid)
{
    if (monster_is_visible(p, mon->midx) && monster_is_in_view(p, mon->midx))
    {
        const char *m = square_feat(c, grid)->confused_msg;

        msg(p, "%s %s.", m_name, (m? m: "stumbles"));
    }
}


/*
 * Helper function for monster_turn_can_move() to slightly stun a monster
 * on occasion due to bumbling into something.
 */
static void monster_slightly_stun_by_move(struct player *p, struct monster *mon)
{
    if ((mon->m_timed[MON_TMD_STUN] < 5) && one_in_(3))
        mon_inc_timed(p, mon, MON_TMD_STUN, 3, 0);
}


/*
 * Work out if a monster can move through the grid, if necessary bashing
 * down doors in the way.
 *
 * Returns true if the monster is able to move through the grid.
 */
static bool monster_turn_can_move(struct source *who, struct chunk *c, struct monster *mon,
    const char *m_name, struct loc *grid, bool confused, bool *did_something)
{
    struct monster_lore *lore = get_lore(who->player, mon->race);
    struct monster *square_mon = NULL;

    /* Always allow an attack upon the player or decoy. */
    if (square_isplayer(c, grid) || square_isdecoyed(c, grid)) return true;

    /* Dangerous terrain in the way */
    if (!confused && monster_hates_grid(c, mon, grid)) return false;

    /* Safe floor */
    if (square_issafefloor(c, grid)) return false;

    /* Floor is open? */
    if (square_ispassable(c, grid)) return true;

    /* Permanent wall in the way */
    if (square_ispermborder(c, grid))
    {
        if (confused)
        {
            *did_something = true;
            monster_display_confused_move_msg(who->player, mon, m_name, c, grid);
            monster_slightly_stun_by_move(who->player, mon);
        }
        return false;
    }

    /* Normal wall, door, or secret door in the way */

    /*
     * There's some kind of feature in the way, so learn about
     * kill-wall and pass-wall now
     */
    if (monster_is_visible(who->player, mon->midx))
    {
        rf_on(lore->flags, RF_PASS_WALL);
        rf_on(lore->flags, RF_KILL_WALL);
        rf_on(lore->flags, RF_SMASH_WALL);
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
    if (rf_has(mon->race->flags, RF_SMASH_WALL))
    {
        /* Remove the wall and much of what's nearby */
        square_smash_wall(c, grid);

        /* Note changes to viewable region */
        note_viewable_changes(&c->wpos, grid);

        /* Fully update the flow since terrain changed */
        fully_update_flow(&c->wpos);

        return true;
    }
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
        /* Don't allow a confused move to open a door. */
        bool can_open = (rf_has(mon->race->flags, RF_OPEN_DOOR) && !confused);

        /* During a confused move, a monster only bashes sometimes. */
        bool can_bash = (rf_has(mon->race->flags, RF_BASH_DOOR) && (!confused || one_in_(3)));

        bool will_bash = false;

        /* Take a turn */
        if (can_open || can_bash) *did_something = true;

        /* Learn about door abilities */
        if (!confused && monster_is_visible(who->player, mon->midx))
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
        {
            if (confused)
            {
                *did_something = true;
                monster_display_confused_move_msg(who->player, mon, m_name, c, grid);
                monster_slightly_stun_by_move(who->player, mon);
            }
            return false;
        }

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

            if (confused)
            {
                /* Didn't learn above; apply now since attempted to bash. */
                if (monster_is_visible(who->player, mon->midx)) rf_on(lore->flags, RF_BASH_DOOR);

                /* When confused, can stun itself while bashing. */
                monster_slightly_stun_by_move(who->player, mon);
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
                if (!who->monster && OPT(who->player, disturb_bash)) disturb(who->player);

                /* Note changes to viewable region */
                note_viewable_changes(&c->wpos, grid);

                if (confused)
                {
                    /* Didn't learn above; apply now since attempted to bash. */
                    if (monster_is_visible(who->player, mon->midx)) rf_on(lore->flags, RF_BASH_DOOR);

                    /* When confused, can stun itself while bashing. */
                    monster_slightly_stun_by_move(who->player, mon);
                }

                /* Fall into doorway */
                return true;
            }

            square_open_door(c, grid);

            /* Note changes to viewable region */
            note_viewable_changes(&c->wpos, grid);
        }
    }
    else if (confused)
    {
        *did_something = true;
        monster_display_confused_move_msg(who->player, mon, m_name, c, grid);
        monster_slightly_stun_by_move(who->player, mon);
    }

    return false;
}


/*
 * Try to break a glyph.
 */
static bool monster_turn_attack_glyph(struct player *p, struct monster *mon, struct loc *grid)
{
    /* Break the ward */
    if (CHANCE(mon->level, z_info->glyph_hardness))
    {
        /* Describe observable breakage */
        if (square_isseen(p, grid))
            msg(p, "The rune of protection is broken!");

        /* Break the rune */
        square_destroy_trap(chunk_get(&p->wpos), grid);

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
            if (monster_is_smart(mon)) chance = 100;

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
        if (monster_is_smart(mon)) chance = 100;

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
            square_delete_object(c, grid, obj, true, true);
        }

        /* Next object */
        obj = next;
    }
}


/*
 * Monster movement
 */
static void monster_turn_move(struct source *who, struct chunk *c, struct monster *mon,
    const char *m_name, int dir, enum monster_stagger stagger, bool tracking, bool *did_something)
{
    struct monster_lore *lore = get_lore(who->player, mon->race);
    int i;
    struct loc grid;

    /* Target info */
    if (!who->monster)
        loc_copy(&grid, &who->player->grid);
    else
        loc_copy(&grid, &who->monster->grid);

    /*
     * Try to move first in the chosen direction, or next either side of the
     * chosen direction, or next at right angles to the chosen direction.
     * Monsters which are tracking by sound or scent will not move if they
     * can't move in their chosen direction.
     */
    for (i = 0; ((i < 5) && !(*did_something)); i++)
    {
        bool move;
        struct loc ngrid;

        /* Get the direction (or stagger) */
        int d = ((stagger != NO_STAGGER)? ddd[randint0(8)]: side_dirs[dir][i]);

        /* Get the grid to step to or attack */
        next_grid(&ngrid, &mon->grid, d);

        /* Tracking monsters have their best direction, don't change */
        if ((i > 0) && (stagger == NO_STAGGER) && tracking) break;

        /* Check if we can move */
        move = monster_turn_can_move(who, c, mon, m_name, &ngrid, (stagger == CONFUSED_STAGGER),
            did_something);

        if (!move) continue;

        /* Try to break the glyph if there is one */
        if (square_iswarded(c, &ngrid) && !monster_turn_attack_glyph(who->player, mon, &ngrid))
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
            {
                /* Hack -- do not cancel fire_till_kill on movement */
                if (who->player->firing_request) who->player->cancel_firing = false;

                disturb(who->player);
            }
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
    enum monster_stagger stagger;
    bool tracking = false;
    char m_name[NORMAL_WID];
    struct monster_lore *lore = get_lore(who->player, mon->race);

    /* Get the monster name */
    monster_desc(who->player, m_name, sizeof(m_name), mon, MDESC_CAPITAL | MDESC_IND_HID);

    /* If we're in a web, deal with that */
    if (square_iswebbed(c, &mon->grid))
    {
        /* Learn web behaviour */
        if (monster_is_visible(who->player, mon->midx))
        {
            rf_on(lore->flags, RF_CLEAR_WEB);
            rf_on(lore->flags, RF_PASS_WEB);
        }

        /* If we can pass, no need to clear */
        if (!rf_has(mon->race->flags, RF_PASS_WEB))
        {
            /* Learn wall behaviour */
            if (monster_is_visible(who->player, mon->midx))
            {
                rf_on(lore->flags, RF_PASS_WALL);
                rf_on(lore->flags, RF_KILL_WALL);
                rf_on(lore->flags, RF_SMASH_WALL);
            }

            /* Insubstantial monsters go right through */
            if (rf_has(mon->race->flags, RF_PASS_WALL)) {}

            /* If you can pass through walls, you can destroy a web */
            else if (monster_passes_walls(mon->race))
            {
                square_clear_feat(c, &mon->grid);
                update_visuals(&who->player->wpos);
                fully_update_flow(&who->player->wpos);
                return;
            }

            /* Clearing costs a turn */
            else if (rf_has(mon->race->flags, RF_CLEAR_WEB))
            {
                square_clear_feat(c, &mon->grid);
                update_visuals(&who->player->wpos);
                fully_update_flow(&who->player->wpos);
                return;
            }

            /* Stuck */
            else return;
        }
    }

    /* Generate monster drops */
    if (!who->monster) mon_create_drops(who->player, c, mon);

    /* Let other group monsters know about the player */
    if (!who->monster) monster_group_rouse(who->player, c, mon);

    /* Try to multiply - this can use up a turn */
    if (!who->monster && monster_turn_multiply(c, mon)) return;

    /* Attempt a ranged attack (skip if non hostile) */
    if (who->monster || pvm_check(who->player, mon))
    {
        if (make_ranged_attack(who, c, mon, target_m_dis)) return;
    }

    /* Work out what kind of movement to use - random movement or AI */
    stagger = monster_turn_should_stagger(who->player, mon);

    /* If there's no sensible move, we're done */
    if ((stagger == NO_STAGGER) && !get_move(who, c, mon, &dir, &tracking)) return;

    /* Movement */
    monster_turn_move(who, c, mon, m_name, dir, stagger, tracking, &did_something);

    if (mon->race->light != 0)
        who->player->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Out of options - monster is paralyzed by fear (unless attacked) */
    if (!did_something && mon->m_timed[MON_TMD_FEAR])
    {
        int amount = mon->m_timed[MON_TMD_FEAR];

        mon_clear_timed(who->player, mon, MON_TMD_FEAR, MON_TMD_FLG_NOMESSAGE);
        mon_inc_timed(who->player, mon, MON_TMD_HOLD, amount, MON_TMD_FLG_NOTIFY);
    }

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
    if (*target_m_dis <= mon->race->hearing) mflag_on(mon->mflag, MFLAG_ACTIVE);

    /* Monster is hurt */
    else if (is_hurt) mflag_on(mon->mflag, MFLAG_ACTIVE);

    /* Monster can "see" the player (checked backwards) */
    else if (target_m_los) mflag_on(mon->mflag, MFLAG_ACTIVE);

    /* Monster can hear the player */
    else if (can_hear) mflag_on(mon->mflag, MFLAG_ACTIVE);

    /* Monster can smell the player */
    else if (can_smell) mflag_on(mon->mflag, MFLAG_ACTIVE);

    /* Monster is taking damage from the terrain */
    else if (monster_taking_terrain_damage(c, mon)) mflag_on(mon->mflag, MFLAG_ACTIVE);

    /* Otherwise go passive */
    else mflag_off(mon->mflag, MFLAG_ACTIVE);

    return (mflag_has(mon->mflag, MFLAG_ACTIVE)? true: false);
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
        /* Wake the monster, make it aware */
        monster_wake(p, mon, true, 100);

        /* Notify the player if aware */
        if (player_of_has(p, OF_AGGRAVATE) && monster_is_obvious(p, mon->midx, mon))
            equip_learn_flag(p, OF_AGGRAVATE);
    }

    /* Hack -- see if monster "notices" player */
    else if ((notice * notice * notice) <= player_noise)
    {
        int sleep_reduction = 1;
        int local_noise = p->cave->noise.grids[mon->grid.y][mon->grid.x];
        bool woke_up = false;

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

    /* Awake, active monsters may become aware */
    if (one_in_(10) && mflag_has(mon->mflag, MFLAG_ACTIVE)) mflag_on(mon->mflag, MFLAG_AWARE);

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

    if (mon->m_timed[MON_TMD_CHANGED])
        mon_dec_timed(p, mon, MON_TMD_CHANGED, 1, MON_TMD_FLG_NOTIFY);

    if (mon->m_timed[MON_TMD_BLIND])
        mon_dec_timed(p, mon, MON_TMD_BLIND, 1, MON_TMD_FLG_NOTIFY);

    /* Handle fear, poison, bleeding */
    monster_effects(p, mon);

    /* Always miss turn if held, one in STUN_MISS_CHANCE chance of missing if stunned */
    if (mon->m_timed[MON_TMD_HOLD]) return true;
    if (mon->m_timed[MON_TMD_STUN]) return one_in_(STUN_MISS_CHANCE);

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
        if (mflag_has(mon->mflag, MFLAG_HANDLED)) continue;

        /* Skip "unconscious" monsters */
        if (mon->hp == 0) continue;

        /* Not enough energy to move yet */
        if (more_energy && mon->closest_player && (mon->energy <= mon->closest_player->energy))
            continue;

        /* Prevent reprocessing */
        mflag_on(mon->mflag, MFLAG_HANDLED);

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
            if (monster_allow_shimmer(q)) square_light_spot_aux(q, c, &mon->grid);
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
        mflag_off(mon->mflag, MFLAG_HANDLED);
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
    if (monster_is_smart(mon)) mlv = (mlv * 5) / 4;
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
