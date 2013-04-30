/*
 * File: pathfind.c
 * Purpose: Pathfinding and running code.
 *
 * Copyright (c) 1988 Christopher J Stuart (running code)
 * Copyright (c) 2004-2007 Christophe Cavalaria, Leon Marrick (pathfinding)
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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
#include "squelch.h"


/****** Pathfinding code ******/


/****** Running code ******/


/*
 * Basically, once you start running, you keep moving until something
 * interesting happens.  In an enclosed space, you run straight, but
 * you follow corners as needed (i.e. hallways).  In an open space,
 * you run straight, but you stop before entering an enclosed space
 * (i.e. a room with a doorway).  In a semi-open space (with walls on
 * one side only), you run straight, but you stop before entering an
 * enclosed space or an open space (i.e. running along side a wall).
 *
 * All discussions below refer to what the player can see, that is,
 * an unknown wall is just like a normal floor.  This means that we
 * must be careful when dealing with "illegal" grids.
 *
 * No assumptions are made about the layout of the dungeon, so this
 * algorithm works in hallways, rooms, town, destroyed areas, etc.
 *
 * In the diagrams below, the player has just arrived in the grid
 * marked as '@', and he has just come from a grid marked as 'o',
 * and he is about to enter the grid marked as 'x'.
 *
 * Running while confused is not allowed, and so running into a wall
 * is only possible when the wall is not seen by the player.  This
 * will take a turn and stop the running.
 *
 * Several conditions are tracked by the running variables.
 *
 *   p_ptr->run_open_area (in the open on at least one side)
 *   p_ptr->run_break_left (wall on the left, stop if it opens)
 *   p_ptr->run_break_right (wall on the right, stop if it opens)
 *
 * When running begins, these conditions are initialized by examining
 * the grids adjacent to the requested destination grid (marked 'x'),
 * two on each side (marked 'L' and 'R').  If either one of the two
 * grids on a given side is a wall, then that side is considered to
 * be "closed".  Both sides enclosed yields a hallway.
 *
 *    LL                     @L
 *    @x      (normal)       RxL   (diagonal)
 *    RR      (east)          R    (south-east)
 *
 * In the diagram below, in which the player is running east along a
 * hallway, he will stop as indicated before attempting to enter the
 * intersection (marked 'x').  Starting a new run in any direction
 * will begin a new hallway run.
 *
 * #.#
 * ##.##
 * o@x..
 * ##.##
 * #.#
 *
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * In the diagram below, the player is running east down a hallway,
 * and will stop in the grid (marked '1') before the intersection.
 * Continuing the run to the south-east would result in a long run
 * stopping at the end of the hallway (marked '2').
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After each step, the surroundings are examined to determine if
 * the running should stop, and to determine if the running should
 * change direction.  We examine the new current player location
 * (at which the runner has just arrived) and the direction from
 * which the runner is considered to have come.
 *
 * Moving one grid in some direction places you adjacent to three
 * or five new grids (for straight and diagonal moves respectively)
 * to which you were not previously adjacent (marked as '!').
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * If any of the newly adjacent grids are "interesting" (monsters,
 * objects, some terrain features) then running stops.
 *
 * If any of the newly adjacent grids seem to be open, and you are
 * looking for a break on that side, then running stops.
 *
 * If any of the newly adjacent grids do not seem to be open, and
 * you are in an open area, and the non-open side was previously
 * entirely open, then running stops.
 *
 * If you are in a hallway, then the algorithm must determine if
 * the running should continue, turn, or stop.  If only one of the
 * newly adjacent grids appears to be open, then running continues
 * in that direction, turning if necessary.  If there are more than
 * two possible choices, then running stops.  If there are exactly
 * two possible choices, separated by a grid which does not seem
 * to be open, then running stops.  Otherwise, as shown below, the
 * player has probably reached a "corner".
 *
 *    ###             o##
 *    o@x  (normal)   #@!   (diagonal)
 *    ##!  (east)     ##x   (south east)
 *
 * In this situation, there will be two newly adjacent open grids,
 * one touching the player on a diagonal, and one directly adjacent.
 * We must consider the two "option" grids further out (marked '?').
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid.
 *
 *    ###s
 *    o@x?   (may be incorrect diagram!)
 *    ##!?
 *
 * If both "option" grids are closed, then there is no reason to enter
 * the corner, and so we can cut the corner, by moving into the other
 * grid (diagonally).  If we choose not to cut the corner, then we may
 * go straight, but we pretend that we got there by moving diagonally.
 * Below, we avoid the obvious grid (marked 'x') and cut the corner
 * instead (marked 'n').
 *
 *    ###:               o##
 *    o@x#   (normal)    #@n    (maybe?)
 *    ##n#   (east)      ##x#
 *                       ####
 *
 * If one of the "option" grids is open, then we may have a choice, so
 * we check to see whether it is a potential corner or an intersection
 * (or room entrance).  If the grid two spaces straight ahead, and the
 * space marked with 's' are both open, then it is a potential corner
 * and we enter it if requested.  Otherwise, we stop, because it is
 * not a corner, and is instead an intersection or a room entrance.
 *
 *    ###
 *    o@x
 *    ##!#
 *
 * I do not think this documentation is correct.
 */


/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static const byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };


/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static const byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };


/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int Ind, int dir, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return FALSE;

    /* Get the new location */
    y += ddy[dir];
    x += ddx[dir];

    /* Ghosts run right through everything */
    if (player_passwall(p_ptr)) return FALSE;

    /* Do wilderness hack, keep running from one outside level to another */
    if (!in_bounds_fully(y, x) && (p_ptr->depth <= 0)) return FALSE;

    /* Illegal grids are not known walls XXX XXX XXX */
    if (!in_bounds(y, x)) return FALSE;

    /* Non-wall grids are not known walls */
    if (cave_floor_bold(p_ptr->depth, y, x)) return FALSE;

    /* Unknown walls are not known walls */
    if (!(p_ptr->cave->info[y][x] & CAVE_MARK)) return FALSE;

    /* Default */
    return TRUE;
}


/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diagonal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int row, col, deepleft, deepright;
    int i, shortleft, shortright;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    /* Mark that we're starting a run */
    p_ptr->running_firststep = TRUE;

    /* Save the direction */
    p_ptr->run_cur_dir = dir;

    /* Assume running straight */
    p_ptr->run_old_dir = dir;

    /* Assume looking for open area */
    p_ptr->run_open_area = TRUE;

    /* Assume not looking for breaks */
    p_ptr->run_break_right = p_ptr->run_break_left = FALSE;

    /* Assume no nearby walls */
    deepleft = deepright = FALSE;
    shortright = shortleft = FALSE;

    /* Find the destination grid */
    row = p_ptr->py + ddy[dir];
    col = p_ptr->px + ddx[dir];

    /* Extract cycle index */
    i = chome[dir];

    /* Check for nearby wall */
    if (see_wall(Ind, cycle[i+1], p_ptr->py, p_ptr->px))
    {
        /* When in the town/wilderness, don't break left/right. */
        if (p_ptr->depth > 0)
        {
            p_ptr->run_break_left = TRUE;
            shortleft = TRUE;
        }
    }

    /* Check for distant wall */
    else if (see_wall(Ind, cycle[i+1], row, col))
    {
        /* When in the town/wilderness, don't break left/right. */
        if (p_ptr->depth > 0)
        {
            p_ptr->run_break_left = TRUE;
            deepleft = TRUE;
        }
    }

    /* Check for nearby wall */
    if (see_wall(Ind, cycle[i-1], p_ptr->py, p_ptr->px))
    {
        /* When in the town/wilderness, don't break left/right. */
        if (p_ptr->depth > 0)
        {
            p_ptr->run_break_right = TRUE;
            shortright = TRUE;
        }
    }

    /* Check for distant wall */
    else if (see_wall(Ind, cycle[i-1], row, col))
    {
        /* When in the town/wilderness, don't break left/right. */
        if (p_ptr->depth > 0)
        {
            p_ptr->run_break_right = TRUE;
            deepright = TRUE;
        }
    }

    /* Looking for a break */
    if (p_ptr->run_break_left && p_ptr->run_break_right)
    {
        /* Not looking for open area */
        /* In the town/wilderness, always in an open area */
        if (p_ptr->depth > 0)
            p_ptr->run_open_area = FALSE;

        /* Hack -- allow angled corridor entry */
        if (dir & 0x01)
        {
            if (deepleft && !deepright)
                p_ptr->run_old_dir = cycle[i - 1];
            else if (deepright && !deepleft)
                p_ptr->run_old_dir = cycle[i + 1];
        }

        /* Hack -- allow blunt corridor entry */
        else if (see_wall(Ind, cycle[i], row, col))
        {
            if (shortleft && !shortright)
                p_ptr->run_old_dir = cycle[i - 2];
            else if (shortright && !shortleft)
                p_ptr->run_old_dir = cycle[i + 2];
        }
    }
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    int prev_dir;
    int new_dir;
    int row, col;
    int i, max, inv;
    int option, option2;

    /* Ghosts never stop running */
    if (player_passwall(p_ptr)) return (FALSE);

    /* No options yet */
    option = 0;
    option2 = 0;

    /* Where we came from */
    prev_dir = p_ptr->run_old_dir;

    /* Range of newly adjacent grids */
    max = (prev_dir & 0x01) + 1;

    /* Look at every newly adjacent square. */
    for (i = -max; i <= max; i++)
    {
        object_type *o_ptr;
        s16b this_o_idx, next_o_idx = 0;
        int m_idx;

        /* New direction */
        new_dir = cycle[chome[prev_dir] + i];

        /* New location */
        row = py + ddy[new_dir];
        col = px + ddx[new_dir];

        /* Paranoia: ignore "illegal" locations */
        if (!in_bounds(row, col)) continue;

        m_idx = cave_get(p_ptr->depth)->m_idx[row][col];

        /* Visible hostile monsters abort running */
        if (m_idx > 0)
        {
            /* Visible hostile monster */
            if (pvm_check(Ind, m_idx) && p_ptr->mon_vis[m_idx])
                return (TRUE);
        }

        /* Visible hostile players abort running */
        if (m_idx < 0)
        {
            /* Visible hostile player */
            if (pvp_check(p_ptr, player_get(0 - m_idx), PVP_CHECK_BOTH, TRUE,
                cave_get(p_ptr->depth)->feat[row][col]) && p_ptr->play_vis[0 - m_idx])
            {
                return (TRUE);
            }
        }

        /* Visible objects abort running */
        for (this_o_idx = cave_get(p_ptr->depth)->o_idx[row][col]; this_o_idx;
            this_o_idx = next_o_idx)
        {
            /* Get the object */
            o_ptr = object_byid(this_o_idx);

            /* Get the next object */
            next_o_idx = o_ptr->next_o_idx;

            /* Visible object */
            if (object_marked(p_ptr, this_o_idx) && !squelch_item_ok(p_ptr, o_ptr))
                return (TRUE);
        }

        /* Hack -- Always stop in water and lava */
        if (cave_get(p_ptr->depth)->feat[row][col] == FEAT_WATER) return TRUE;
        if (cave_get(p_ptr->depth)->feat[row][col] == FEAT_LAVA) return TRUE;

        /* Assume unknown */
        inv = TRUE;

        /* Check memorized grids */
        if (p_ptr->cave->info[row][col] & CAVE_MARK)
        {
            bool notice = TRUE;

            /* Examine the terrain */
            switch (cave_get(p_ptr->depth)->feat[row][col])
            {
                /* Floors */
                case FEAT_FLOOR:
                case FEAT_FLOOR_SAFE:
                case FEAT_STREET:

                /* Invis traps */
                case FEAT_INVIS:

                /* Secret doors */
                case FEAT_SECRET:

                /* Normal veins */
                case FEAT_MAGMA:
                case FEAT_QUARTZ:

                /* Hidden treasure */
                case FEAT_MAGMA_H:
                case FEAT_QUARTZ_H:

                /* Grass, trees, mountains, logs, and dirt */
                case FEAT_GRASS:
                case FEAT_TREE:
                case FEAT_EVIL_TREE:
                case FEAT_MOUNTAIN:
                case FEAT_LOGS:
                case FEAT_DIRT:

                /* Walls */
                case FEAT_WALL_EXTRA:
                case FEAT_WALL_INNER:
                case FEAT_WALL_OUTER:
                case FEAT_WALL_SOLID:
                case FEAT_PERM_EXTRA:
                case FEAT_PERM_BASIC:
                case FEAT_PERM_FAKE:
                case FEAT_PERM_SOLID:
                case FEAT_PERM_CLEAR:
                case FEAT_PERM_ARENA:
                {
                    /* Ignore */
                    notice = FALSE;

                    /* Done */
                    break;
                }
            }

            /* Interesting feature */
            if (notice) return (TRUE);

            /* The grid is "visible" */
            inv = FALSE;
        }

        /* Analyze unknown grids and floors */
        /* Wilderness hack to run from one level to the next */
        if (inv || cave_floor_bold(p_ptr->depth, row, col) ||
            (!in_bounds_fully(row, col) && (p_ptr->depth <= 0)))
        {
            /* Looking for open area */
            if (p_ptr->run_open_area)
            {
                /* Nothing */
            }

            /* The first new direction. */
            else if (!option)
                option = new_dir;

            /* Three new directions. Stop running. */
            else if (option2)
                return (TRUE);

            /* Two non-adjacent new directions.  Stop running. */
            else if (option != cycle[chome[prev_dir] + i - 1])
                return (TRUE);

            /* Two new (adjacent) directions (case 1) */
            else if (new_dir & 0x01)
                option2 = new_dir;

            /* Two new (adjacent) directions (case 2) */
            else
            {
                option2 = option;
                option = new_dir;
            }
        }

        /* Obstacle, while looking for open area */
        /* When in the town/wilderness, don't break left/right. */
        else
        {
            if (p_ptr->run_open_area)
            {
                if (i < 0)
                {
                    /* Break to the right */
                    if (p_ptr->depth > 0)
                        p_ptr->run_break_right = (TRUE);
                }

                else if (i > 0)
                {
                    /* Break to the left */
                    if (p_ptr->depth > 0)
                        p_ptr->run_break_left = (TRUE);
                }
            }
        }
    }

    /* Look at every soon to be newly adjacent square. */
    for (i = -max; i <= max; i++)
    {
        int m_idx;

        /* New direction */
        new_dir = cycle[chome[prev_dir] + i];

        /* New location */
        row = py + ddy[prev_dir] + ddy[new_dir];
        col = px + ddx[prev_dir] + ddx[new_dir];

        /* Paranoia */
        if (!in_bounds_fully(row, col)) continue;

        m_idx = cave_get(p_ptr->depth)->m_idx[row][col];

        /* Visible hostile monsters abort running */
        if (m_idx > 0)
        {
            /* Visible hostile monster (except unaware mimics) */
            if (pvm_check(Ind, m_idx) && p_ptr->mon_vis[m_idx] &&
                !is_mimicking(cave_monster(cave_get(p_ptr->depth), m_idx)))
            {
                return (TRUE);
            }
        }

        /* Visible hostile players abort running */
        if (m_idx < 0)
        {
            /* Visible hostile player (except unaware mimics) */
            if (pvp_check(p_ptr, player_get(0 - m_idx), PVP_CHECK_BOTH, TRUE,
                cave_get(p_ptr->depth)->feat[row][col]) && p_ptr->play_vis[0 - m_idx] &&
                !player_get(0 - m_idx)->k_idx)
            {
                return (TRUE);
            }
        }
    }

    /* Looking for open area */
    if (p_ptr->run_open_area)
    {
        /* Hack -- look again */
        for (i = -max; i < 0; i++)
        {
            /* New direction */
            new_dir = cycle[chome[prev_dir] + i];

            /* New location */
            row = py + ddy[new_dir];
            col = px + ddx[new_dir];

            /* Unknown grid or floor */
            if (!(p_ptr->cave->info[row][col] & CAVE_MARK) ||
                cave_floor_bold(p_ptr->depth, row, col))
            {
                /* Looking to break right */
                if (p_ptr->run_break_right) return (TRUE);
            }

            /* Obstacle */
            else
            {
                /* Looking to break left */
                if (p_ptr->run_break_left) return (TRUE);
            }
        }

        /* Hack -- look again */
        for (i = max; i > 0; i--)
        {
            new_dir = cycle[chome[prev_dir] + i];

            row = py + ddy[new_dir];
            col = px + ddx[new_dir];

            /* Unknown grid or floor */
            if (!(p_ptr->cave->info[row][col] & CAVE_MARK) ||
                cave_floor_bold(p_ptr->depth, row, col))
            {
                /* Looking to break left */
                if (p_ptr->run_break_left) return (TRUE);
            }

            /* Obstacle */
            else
            {
                /* Looking to break right */
                if (p_ptr->run_break_right) return (TRUE);
            }
        }
    }

    /* Not looking for open area */
    else
    {
        /* No options */
        if (!option) return (TRUE);

        /* One option */
        else if (!option2)
        {
            /* Primary option */
            p_ptr->run_cur_dir = option;

            /* No other options */
            p_ptr->run_old_dir = option;
        }

        /* Two options, examining corners */
        else
        {
            /* Primary option */
            p_ptr->run_cur_dir = option;

            /* Hack -- allow curving */
            p_ptr->run_old_dir = option2;
        }
    }

    /* About to hit a known wall, stop */
    if (see_wall(Ind, p_ptr->run_cur_dir, p_ptr->py, p_ptr->px))
        return (TRUE);

    /* Failure */
    return (FALSE);
}


/*
 * Take one step along the current "run" path
 */
void run_step(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Start running */
    if (p_ptr->run_request)
    {
        /* Calculate torch radius */
        p_ptr->update |= (PU_TORCH);

        /* Initialize */
        run_init(Ind, p_ptr->run_request);

        /* We are running */
        p_ptr->run_request = 0;
    }

    /* Keep running */
    else
    {
        /* Update run */
        if (run_test(Ind))
        {
            /* Disturb */
            disturb(p_ptr, 0, 0);

            /* Done */
            return;
        }
    }

    /* Take a turn */
    use_energy(Ind);

    /* Move the player */
    move_player(Ind, p_ptr->run_cur_dir, TRUE, FALSE, FALSE);
}
