/*
 * File: player-path.c
 * Purpose: Pathfinding and running code.
 *
 * Copyright (c) 1988 Christopher J Stuart (running code)
 * Copyright (c) 2004-2007 Christophe Cavalaria, Leon Marrick (pathfinding)
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


/*
 * Running code
 */


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
 * Hack -- check for a "known wall" (see below)
 */
static int see_wall(struct player *p, struct chunk *c, int dir, int y, int x)
{
    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return false;

    /* Get the new location */
    y += ddy[dir];
    x += ddx[dir];

    /* Ghosts run right through everything */
    if (player_passwall(p)) return false;

    /* Do wilderness hack, keep running from one outside level to another */
    if (!square_in_bounds_fully(c, y, x) && (p->depth <= 0)) return false;

    /* Illegal grids are not known walls XXX XXX XXX */
    if (!square_in_bounds(c, y, x)) return false;

    /* Non-wall grids are not known walls */
    if (square_ispassable(c, y, x)) return false;

    /* Unknown walls are not known walls */
    if (!square_isknown(p, y, x)) return false;

    /* Default */
    return true;
}


/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diagonal entry into corridors.
 *
 * Blunt Corridor -- if there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(struct player *p, struct chunk *c, int dir)
{
    int row, col, deepleft, deepright;
    int i, shortleft, shortright;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    /* Mark that we're starting a run */
    p->upkeep->running_firststep = true;

    /* Save the direction */
    p->run_cur_dir = dir;

    /* Assume running straight */
    p->run_old_dir = dir;

    /* Assume looking for open area */
    p->run_open_area = true;

    /* Assume not looking for breaks */
    p->run_break_right = p->run_break_left = false;

    /* Assume no nearby walls */
    deepleft = deepright = false;
    shortright = shortleft = false;

    /* Find the destination grid */
    row = p->py + ddy[dir];
    col = p->px + ddx[dir];

    /* Extract cycle index */
    i = chome[dir];

    /* Check for nearby or distant wall */
    if (see_wall(p, c, cycle[i + 1], p->py, p->px))
    {
        /* When in the town/wilderness, don't break left/right. */
        if (p->depth > 0)
        {
            p->run_break_left = true;
            shortleft = true;
        }
    }
    else if (see_wall(p, c, cycle[i + 1], row, col))
    {
        /* When in the town/wilderness, don't break left/right. */
        if (p->depth > 0)
        {
            p->run_break_left = true;
            deepleft = true;
        }
    }

    /* Check for nearby or distant wall */
    if (see_wall(p, c, cycle[i - 1], p->py, p->px))
    {
        /* When in the town/wilderness, don't break left/right. */
        if (p->depth > 0)
        {
            p->run_break_right = true;
            shortright = true;
        }
    }
    else if (see_wall(p, c, cycle[i - 1], row, col))
    {
        /* When in the town/wilderness, don't break left/right. */
        if (p->depth > 0)
        {
            p->run_break_right = true;
            deepright = true;
        }
    }

    /* Looking for a break */
    if (p->run_break_left && p->run_break_right)
    {
        /* Not looking for open area */
        /* In the town/wilderness, always in an open area */
        if (p->depth > 0)
            p->run_open_area = false;

        /* Angled or blunt corridor entry */
        if (dir & 0x01)
        {
            if (deepleft && !deepright)
                p->run_old_dir = cycle[i - 1];
            else if (deepright && !deepleft)
                p->run_old_dir = cycle[i + 1];
        }
        else if (see_wall(p, c, cycle[i], row, col))
        {
            if (shortleft && !shortright)
                p->run_old_dir = cycle[i - 2];
            else if (shortright && !shortleft)
                p->run_old_dir = cycle[i + 2];
        }
    }
}


/*
 * Update the current "run" path
 *
 * Return true if the running should be stopped
 */
static bool run_test(struct player *p, struct chunk *c)
{
    int py = p->py;
    int px = p->px;
    int prev_dir;
    int new_dir;
    int row, col;
    int i, max, inv;
    int option, option2;
    struct actor who_body;
    struct actor *who = &who_body;

    /* Ghosts never stop running */
    if (player_passwall(p)) return false;

    /* No options yet */
    option = 0;
    option2 = 0;

    /* Where we came from */
    prev_dir = p->run_old_dir;

    /* Range of newly adjacent grids */
    max = (prev_dir & 0x01) + 1;

    /* Look at every newly adjacent square. */
    for (i = -max; i <= max; i++)
    {
        struct object *obj;
        int m_idx, feat;

        /* New direction */
        new_dir = cycle[chome[prev_dir] + i];

        /* New location */
        row = py + ddy[new_dir];
        col = px + ddx[new_dir];

        /* Paranoia: ignore "illegal" locations */
        if (!square_in_bounds(c, row, col)) continue;

        feat = c->squares[row][col].feat;
        m_idx = c->squares[row][col].mon;
        ACTOR_WHO(who, m_idx, player_get(0 - m_idx), cave_monster(c, m_idx));

        /* Visible hostile monsters abort running */
        if (who->mon)
        {
            /* Visible hostile monster */
            if (pvm_check(p, who->mon) && mflag_has(p->mflag[who->idx], MFLAG_VISIBLE))
                return true;
        }

        /* Visible hostile players abort running */
        if (who->player)
        {
            /* Visible hostile player */
            if (pvp_check(p, who->player, PVP_CHECK_BOTH, true, feat) &&
                mflag_has(p->pflag[who->idx], MFLAG_VISIBLE))
            {
                return true;
            }
        }

        /* Visible objects abort running */
        for (obj = floor_pile_known(p, c, row, col); obj; obj = obj->next)
        {
            /* Visible object */
            if (!ignore_item_ok(p, obj)) return true;
        }

        /* Hack -- always stop in water and lava */
        if (square_iswater(c, row, col)) return true;
        if (square_islava(c, row, col)) return true;

        /* Assume unknown */
        inv = true;

        /* Check memorized grids */
        if (square_isknown(p, row, col))
        {
            bool notice = square_noticeable(c, row, col);

            /* Interesting feature */
            if (notice) return true;

            /* The grid is "visible" */
            inv = false;
        }

        /* Analyze unknown grids and floors */
        /* Wilderness hack to run from one level to the next */
        if (inv || square_ispassable(c, row, col) ||
            (!square_in_bounds_fully(c, row, col) && (p->depth <= 0)))
        {
            /* Looking for open area */
            if (p->run_open_area)
            {
                /* Nothing */
            }

            /* The first new direction. */
            else if (!option)
                option = new_dir;

            /* Three new directions. Stop running. */
            else if (option2)
                return true;

            /* Two non-adjacent new directions.  Stop running. */
            else if (option != cycle[chome[prev_dir] + i - 1])
                return true;

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
            if (p->run_open_area)
            {
                if (i < 0)
                {
                    /* Break to the right */
                    if (p->depth > 0)
                        p->run_break_right = (true);
                }

                else if (i > 0)
                {
                    /* Break to the left */
                    if (p->depth > 0)
                        p->run_break_left = (true);
                }
            }
        }
    }

    /* Look at every soon to be newly adjacent square. */
    for (i = -max; i <= max; i++)
    {
        int m_idx, feat;

        /* New direction */
        new_dir = cycle[chome[prev_dir] + i];

        /* New location */
        row = py + ddy[prev_dir] + ddy[new_dir];
        col = px + ddx[prev_dir] + ddx[new_dir];

        /* Paranoia */
        if (!square_in_bounds_fully(c, row, col)) continue;

        feat = c->squares[row][col].feat;
        m_idx = c->squares[row][col].mon;
        ACTOR_WHO(who, m_idx, player_get(0 - m_idx), cave_monster(c, m_idx));

        /* Visible hostile monsters abort running */
        if (who->mon)
        {
            /* Visible hostile monster (except unaware mimics) */
            if (pvm_check(p, who->mon) && mflag_has(p->mflag[who->idx], MFLAG_VISIBLE) &&
                !is_mimicking(who->mon))
            {
                return true;
            }
        }

        /* Visible hostile players abort running */
        if (who->player)
        {
            /* Visible hostile player (except unaware mimics) */
            if (pvp_check(p, who->player, PVP_CHECK_BOTH, true, feat) &&
                mflag_has(p->pflag[who->idx], MFLAG_VISIBLE) && !who->player->k_idx)
            {
                return true;
            }
        }
    }

    /* Looking for open area */
    if (p->run_open_area)
    {
        /* Hack -- look again */
        for (i = -max; i < 0; i++)
        {
            /* New direction */
            new_dir = cycle[chome[prev_dir] + i];

            /* New location */
            row = py + ddy[new_dir];
            col = px + ddx[new_dir];

            /* Unknown grid or non-wall */
            if (!square_isknown(p, row, col) || square_ispassable(c, row, col))
            {
                /* Looking to break right */
                if (p->run_break_right) return true;
            }

            /* Obstacle */
            else
            {
                /* Looking to break left */
                if (p->run_break_left) return true;
            }
        }

        /* Hack -- look again */
        for (i = max; i > 0; i--)
        {
            new_dir = cycle[chome[prev_dir] + i];

            row = py + ddy[new_dir];
            col = px + ddx[new_dir];

            /* Unknown grid or non-wall */
            if (!square_isknown(p, row, col) || square_ispassable(c, row, col))
            {
                /* Looking to break left */
                if (p->run_break_left) return true;
            }

            /* Obstacle */
            else
            {
                /* Looking to break right */
                if (p->run_break_right) return true;
            }
        }
    }

    /* Not looking for open area */
    else
    {
        /* No options */
        if (!option) return true;

        /* One option */
        else if (!option2)
        {
            /* Primary option */
            p->run_cur_dir = option;

            /* No other options */
            p->run_old_dir = option;
        }

        /* Two options, examining corners */
        else
        {
            /* Primary option */
            p->run_cur_dir = option;

            /* Hack -- allow curving */
            p->run_old_dir = option2;
        }
    }

    /* About to hit a known wall, stop */
    if (see_wall(p, c, p->run_cur_dir, p->py, p->px))
        return true;

    /* Failure */
    return false;
}


/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
void run_step(struct player *p, int dir)
{
    struct chunk *c = chunk_get(p->depth);

    /* Start or continue run */
    if (dir)
    {
        /* Initialize */
        run_init(p, c, dir);

        /* Hack -- set the run counter */
        p->upkeep->running = true;

        /* Calculate torch radius */
        p->upkeep->update |= (PU_BONUS);
    }
    else
    {
        /* Update regular running */
        if (run_test(p, c))
        {
            /* Disturb */
            disturb(p, 0);
            return;
        }
    }

    /* Take a turn */
    use_energy(p);

    /* Move the player, attempts to disarm if running straight at a trap */
    move_player(p, c, p->run_cur_dir, (dir? true: false), false, false);

    /* Prepare the next step */
    if (p->upkeep->running) cmd_run(p, 0);
}
