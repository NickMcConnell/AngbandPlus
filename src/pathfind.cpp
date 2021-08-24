#include "src/npp.h"
#include <src/cmds.h>


/****** Pathfinding code ******/
/*
 * Build a path from the target location to the
 * player location.  Borrow the flow code so
 * terrain can be taken into account.
 */


static void clear_path(void)
{
    for (int y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (int x = 0; x < p_ptr->cur_map_wid; x++)
        {
            dungeon_info[y][x].clear_path_flow();
        }
    }
}

byte energy_to_move(int y, int x)
{
    /*Quick pointer*/
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /*We have not done this flow yet*/
    if (!dungeon_info[y][x].path_cost)
    {
        /* Can't move here */
        if (!cave_passable_bold(y, x)) return 0;
    }

    /*If a match, return the native movement energy*/
    if (_feat_ff3_match(f_ptr, p_ptr->state.p_flags_native_with_temp))
    {
        return (f_ptr->native_energy_move);
    }

    /*Not native*/
    /*don't want to suffer damage while running*/
    if (f_ptr->dam_non_native) return (0);

    /*Use non-native energy movement*/
    return(f_ptr->non_native_energy_move);

}

static bool path_accept_square(int y, int x)
{
    if (!in_bounds(y, x)) return (FALSE);

    // Can't get there
    if (p_ptr->depth)
    {
        if (!cave_passable_bold(y, x)) return FALSE;
        //Can't see it
        if (!(dungeon_info[y][x].cave_info & (CAVE_EXPLORED))) return (FALSE);
    }
    else if (!cave_ff1_match(y, x, FF1_MOVE)) return (FALSE);

    return (TRUE);
}

bool buildpath(int y, int x)
{
    if (!path_accept_square(y, x)) return (FALSE);

    //clear the path
    clear_path();
    QVector<byte> y_coords;
    QVector<byte> x_coords;
    y_coords.clear();
    x_coords.clear();

    // Start with the final spot.  Try to find the player.
    y_coords.append(y);
    x_coords.append(x);

    dungeon_type *dun_ptr_target = &dungeon_info[y][x];

    dun_ptr_target->path_cost = BASE_ENERGY_MOVE - 25;

    for (int i = 0; i < 100; i++)
    {
        QVector<byte> this_y_coords = y_coords;
        QVector<byte> this_x_coords= x_coords;

        y_coords.clear();
        x_coords.clear();

        for (int z = 0; z < this_y_coords.size(); z++)
        {
            int xx = this_x_coords.at(z);
            int yy = this_y_coords.at(z);

            dungeon_type *dun_ptr_square = &dungeon_info[yy][xx];

            int this_cost = dun_ptr_square->path_cost;

            /* Look at all adjacent grids */
            for (int d = 0; d < 8; d++)
            {
                int xxx = xx + ddx_ddd[d];
                int yyy = yy + ddy_ddd[d];

                dungeon_type *dun_ptr_adj = &dungeon_info[yyy][xxx];

                if (!path_accept_square(yyy, xxx)) continue;

                int new_energy = energy_to_move(yyy, xxx);
                if (!new_energy) continue;
                int new_cost = this_cost + new_energy;

                // Check if this is a longer path
                if (dun_ptr_adj->path_cost)
                {
                    if (dun_ptr_adj->path_cost <= new_cost) continue;
                }

                // record the new cost
                dun_ptr_adj->path_cost = new_cost;

                /*Don't store the same grid twice*/
                if (dun_ptr_adj->path_flow) continue;

                dun_ptr_adj->path_flow = TRUE;

                y_coords.append(yyy);
                x_coords.append(xxx);
            }
        }

        // Clear the flagged squares
        for (int z = 0; z < y_coords.size(); z++)
        {
            dungeon_info[y_coords.at(z)][x_coords.at(z)].path_flow = FALSE;
        }

        // We are done
        if (!y_coords.size()) break;
    }

    //Never found the player
    if (!dungeon_info[p_ptr->py][p_ptr->px].path_cost) return (FALSE);

    return (TRUE);
}

/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
    /* Get the new location */
    y += ddy[dir];
    x += ddx[dir];

    if (!in_bounds(y, x)) return (FALSE);

    /* Unknown walls are not known walls */
    if (!(dungeon_info[y][x].cave_info & (CAVE_MARK))) return (FALSE);

    /* Non-wall grids are not known walls */
    /* Was !cave_ff1_match(y, x, FF1_WALL) -DG- */
    if (cave_ff1_match(y, x, FF1_MOVE)) return (FALSE);

    /* Default */
    return (TRUE);
}

/*
 * The running algorithm  -CJS-
 *
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
 * diagonal grid.  For some unknown reason, we assign "check_dir" to
 * the grid marked 's', which may be incorrectly labelled.
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
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. (?)
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int i, row, col;

    bool deepleft, deepright;
    bool shortleft, shortright;

    /* Save the direction */
    p_ptr->run_cur_dir = dir;

    /* Assume running straight */
    p_ptr->run_old_dir = dir;

    /* Assume looking for open area */
    p_ptr->run_open_area = TRUE;

    /* Assume not looking for breaks */
    p_ptr->run_break_right = FALSE;
    p_ptr->run_break_left = FALSE;

    /* Assume no nearby walls */
    deepleft = deepright = FALSE;
    shortright = shortleft = FALSE;

    /* Find the destination grid */
    row = py + ddy[dir];
    col = px + ddx[dir];

    /* Extract cycle index */
    i = chome[dir];

    /* Check for nearby wall */
    if (see_wall(cycle[i+1], py, px))
    {
        p_ptr->run_break_left = TRUE;
        shortleft = TRUE;
    }

    /* Check for distant wall */
    else if (see_wall(cycle[i+1], row, col))
    {
        p_ptr->run_break_left = TRUE;
        deepleft = TRUE;
    }

    /* Check for nearby wall */
    if (see_wall(cycle[i-1], py, px))
    {
        p_ptr->run_break_right = TRUE;
        shortright = TRUE;
    }

    /* Check for distant wall */
    else if (see_wall(cycle[i-1], row, col))
    {
        p_ptr->run_break_right = TRUE;
        deepright = TRUE;
    }

    /* Looking for a break */
    if (p_ptr->run_break_left && p_ptr->run_break_right)
    {
        /* Not looking for open area */
        p_ptr->run_open_area = FALSE;

        /* Hack -- allow angled corridor entry */
        if (dir & 0x01)
        {
            if (deepleft && !deepright)
            {
                p_ptr->run_old_dir = cycle[i - 1];
            }
            else if (deepright && !deepleft)
            {
                p_ptr->run_old_dir = cycle[i + 1];
            }
        }

        /* Hack -- allow blunt corridor entry */
        else if (see_wall(cycle[i], row, col))
        {
            if (shortleft && !shortright)
            {
                p_ptr->run_old_dir = cycle[i - 2];
            }
            else if (shortright && !shortleft)
            {
                p_ptr->run_old_dir = cycle[i + 2];
            }
        }
    }
}

/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int prev_dir;
    int new_dir;

    int row, col;
    int i, max, inv;
    int option, option2;

    u16b feat;

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

        /* New direction */
        new_dir = cycle[chome[prev_dir] + i];

        /* New location */
        row = py + ddy[new_dir];
        col = px + ddx[new_dir];

        /* Visible monsters abort running */
        if (dungeon_info[row][col].monster_idx > 0)
        {
            monster_type *m_ptr = &mon_list[dungeon_info[row][col].monster_idx];

            /* Visible monsters disturb run */
            if (m_ptr->ml) return (TRUE);

        }

        /* Visible objects abort running */
        for (o_ptr = get_first_object(row, col); o_ptr; o_ptr = get_next_object(o_ptr))
        {
            /* Visible object */
            if (o_ptr->marked) return (TRUE);
        }

        /* Assume unknown */
        inv = TRUE;

        /* Check memorized grids */
        if (dungeon_info[row][col].cave_info & (CAVE_MARK))
        {
            feature_type *f_ptr;

            s16b x_idx;

            /* Get feature */
            feat = dungeon_info[row][col].feature_idx;

            /* Get mimiced feature */
            feat = f_info[feat].f_mimic;

            /* Get fast pointer */
            f_ptr = &f_info[feat];

            /* Feature doesn't support running. Stop */
            if (!_feat_ff1_match(f_ptr, FF1_RUN)) return (TRUE);

            /* Feature is a door */
            if (_feat_ff1_match(f_ptr, FF1_DOOR))
            {

                /* Ignore some doors. Closed doors are always noticeable */
                if (!_feat_ff3_match(f_ptr, FF3_DOOR_OPEN | FF3_DOOR_BROKEN)) return (TRUE);
            }

            /* Check if running should be stopped when reaching stairs */
            if (_feat_ff1_match(f_ptr, FF1_STAIRS)) return (TRUE);

            /* Check visible effects */
            x_idx = dungeon_info[row][col].effect_idx;

            /* Scan effects on that grid */
            while (x_idx)
            {
                /* Get the effect */
                effect_type *x_ptr = &x_list[x_idx];

                /* Found a visible effect, stop */
                if (!(x_ptr->x_flags & (EF1_HIDDEN))) return (TRUE);

                /* Get the index of the next effect */
                x_idx = x_ptr->next_x_idx;
            }

            /* The grid is "visible" */
            inv = FALSE;
        }

        /* Analyze unknown grids and floors */
        if (inv || cave_passable_bold(row, col))
        {
            /* Looking for open area */
            if (p_ptr->run_open_area)
            {
                /* Nothing */
            }

            /* The first new direction. */
            else if (!option)
            {
                option = new_dir;
            }

            /* Three new directions. Stop running. */
            else if (option2)
            {
                return (TRUE);
            }

            /* Two non-adjacent new directions.  Stop running. */
            else if (option != cycle[chome[prev_dir] + i - 1])
            {
                return (TRUE);
            }

            /* Two new (adjacent) directions (case 1) */
            else if (new_dir & 0x01)
            {
                option2 = new_dir;
            }

            /* Two new (adjacent) directions (case 2) */
            else
            {
                option2 = option;
                option = new_dir;
            }
        }

        /* Obstacle, while looking for open area */
        else
        {
            if (p_ptr->run_open_area)
            {
                if (i < 0)
                {
                    /* Break to the right */
                    p_ptr->run_break_right = TRUE;
                }

                else if (i > 0)
                {
                    /* Break to the left */
                    p_ptr->run_break_left = TRUE;
                }
            }
        }
    }

    /* Looking for open area */
    if (p_ptr->run_open_area)
    {
        /* Hack -- look again */
        for (i = -max; i < 0; i++)
        {
            new_dir = cycle[chome[prev_dir] + i];

            row = py + ddy[new_dir];
            col = px + ddx[new_dir];

            /* Get feature */
            feat = dungeon_info[row][col].feature_idx;

            /* Get mimiced feature */
            feat = f_info[feat].f_mimic;

            /* Unknown grid or non-wall */
            /* Was: cave_floor_bold(row, col) */
            if (!(dungeon_info[row][col].cave_info & (CAVE_MARK)) || feat_ff1_match(feat, FF1_MOVE))
            {
                /* Looking to break right */
                if (p_ptr->run_break_right)
                {
                    return (TRUE);
                }
            }

            /* Obstacle */
            else
            {
                /* Looking to break left */
                if (p_ptr->run_break_left)
                {
                    return (TRUE);
                }
            }
        }

        /* Hack -- look again */
        for (i = max; i > 0; i--)
        {
            new_dir = cycle[chome[prev_dir] + i];

            row = py + ddy[new_dir];
            col = px + ddx[new_dir];

            /* Get feature */
            feat = dungeon_info[row][col].feature_idx;

            /* Get mimiced feature */
            feat = f_info[feat].f_mimic;

            /* Unknown grid or non-wall */
            /* Was: cave_floor_bold(row, col) */
            if (!(dungeon_info[row][col].cave_info & (CAVE_MARK)) || feat_ff1_match(feat, FF1_MOVE))
            {
                /* Looking to break left */
                if (p_ptr->run_break_left)
                {
                    return (TRUE);
                }
            }

            /* Obstacle */
            else
            {
                /* Looking to break right */
                if (p_ptr->run_break_right)
                {
                    return (TRUE);
                }
            }
        }
    }


    /* Not looking for open area */
    else
    {
        /* No options */
        if (!option)
        {
            return (TRUE);
        }

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
    if (see_wall(p_ptr->run_cur_dir, py, px))
    {
        return (TRUE);
    }

    /* Failure */
    return (FALSE);
}

static int run_with_pathfind(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int dir = 5;

    // Current player value
    int lowest_cost = dungeon_info[py][px].path_cost;

    // We found where we wanted to be.
    if (lowest_cost < BASE_ENERGY_MOVE)
    {
        disturb(FALSE, FALSE);
        return(0);
    }

    /*
     * Find the fastest square....  Check nearby grids,prefer straight directions.
     */
    for (int i = 0; i < 8; i++)
    {

        /* Get the location */
        int y = py + ddy_ddd[i];
        int x = px + ddx_ddd[i];

        /* Check Bounds */
        if (!in_bounds(y, x)) continue;

        int this_cost = dungeon_info[y][x].path_cost;

        /*Is there a move here?*/
        if (!this_cost) continue;

        /*Not a better route?*/
        if (this_cost >= lowest_cost) continue;

        // This is a better route
        lowest_cost = this_cost;

        dir = ddd[i];
    }

    //Paranoia - we didn't find a better spot?
    if (dir == 5)
    {
        disturb(FALSE, FALSE);
        return(0);
    }

    return (move_player(dir, FALSE));
}

/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
int run_step(int dir)
{
    if (p_ptr->running_withpathfind)
    {
        return (run_with_pathfind());
    }

    /* Start run */
    else if (dir)
    {
        /* Initialize */
        run_init(dir);

        /* Calculate torch radius */
        p_ptr->update |= (PU_TORCH);
    }

    /* Continue run */
    else
    {
        /* Update run */
        if (run_test())
        {
            /* Disturb */
            disturb(FALSE, FALSE);

            /* Done */
            return 0;
        }
    }

    return (move_player(p_ptr->run_cur_dir, FALSE));
}

