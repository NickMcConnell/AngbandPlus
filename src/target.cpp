/*
 * File: target.c
 * Purpose: Targeting code
 *
 * Copyright (c) 1997-2007 Angband contributors, Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "griddialog.h"
#include "src/help.h"
#include "src/command_list.h"
#include <QObject>

/*
 * Determine if a trap makes a reasonable target
 */
static bool target_able_trap(int y, int x)
{
    /* Must be on line of fire */
    if (!player_can_fire_bold(y, x)) return (FALSE);

    /* Only player traps allowed. Ignore monster traps and glyphs */
    if (!cave_player_trap_bold(y, x)) return (FALSE);

    /* Ignore hidden traps */
    if (x_list[dungeon_info[y][x].effect_idx].x_flags & (EF1_HIDDEN)) return (FALSE);

    /* Known player traps are okay */
    return (TRUE);
}

/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targeting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
bool target_able(int m_idx, bool probing)
{

    monster_type *m_ptr;

    /* No monster */
    if (m_idx <= 0) return (FALSE);

    /* Get monster */
    m_ptr = &mon_list[m_idx];

    /* Monster must be alive */
    if (!m_ptr->r_idx) return (FALSE);

    /* Monster must be visible */
    if (!m_ptr->ml) return (FALSE);

    /* Monster must be projectable, except when probing */
    if (!probing)
    {
        if (!m_ptr->project) return (FALSE);
    }

    /* Walls protect monsters */
    if (!cave_project_bold(m_ptr->fy, m_ptr->fx) &&
        !cave_passable_bold(m_ptr->fy, m_ptr->fx)) return (FALSE);

    /* Hack -- no targeting hallucinations */
    if (p_ptr->timed[TMD_IMAGE]) return (FALSE);

    /* Hack -- Never target trappers XXX XXX XXX */
    /* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

    /* Hidden monsters cannot be targets */
    if (m_ptr->mflag & (MFLAG_HIDE)) return (FALSE);

    /* Assume okay */
    return (TRUE);
}

// See if there is a single targetable monster
bool monster_target_exists()
{
    /* Scan the monster list */
    for (int i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr = &mon_list[i];

        /* Don't bother with empty slots */
        if (!m_ptr->r_idx) continue;

        if (target_able(i, FALSE)) return (TRUE);
    }

    return (FALSE);
}

/*
 * Sorting hook srt squares by "distance to player"
 *
 * We use pythagoreans theorum to calculate the distance
 */
static bool coords_sort_distance(coord c1, coord c2)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int dist1 = GET_SQUARE(py - c1.y) + GET_SQUARE(px - c1.x);
    int dist2 = GET_SQUARE(py - c2.y) + GET_SQUARE(px - c2.x);

    /* Compare the distances */
    return (dist1 <= dist2);
}

/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_interactive_accept(int mode, int y, int x)
{
    if (mode & (TARGET_TRAP))
    {
        if (!cave_player_trap_bold(y, x)) return (FALSE);
        u16b x_idx = dungeon_info[y][x].effect_idx;
        effect_type *x_ptr = &x_list[x_idx];
        if (x_ptr->x_flags & (EF1_HIDDEN)) return (FALSE);
        return (TRUE);
    }

    object_type *o_ptr;

    /* Player grids are always interesting */
    if (dungeon_info[y][x].monster_idx < 0) return (TRUE);

    /* Handle hallucination */
    if (p_ptr->timed[TMD_IMAGE]) return (FALSE);

    /* Visible monsters */
    if (dungeon_info[y][x].monster_idx > 0)
    {
        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

        /* Visible monsters */
        if (m_ptr->ml) return (TRUE);
    }

    /* Scan all objects in the grid */
    for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
    {
        /* Memorized object */
        if (o_ptr->marked) return (TRUE);
    }

    /* Interesting memorized features */
    /* Ignore unknown features */
    if (!(dungeon_info[y][x].cave_info & (CAVE_MARK))) return (FALSE);

    /* Find interesting effects */
    if (dungeon_info[y][x].effect_idx > 0)
    {
        /* Get the first effect */
        u16b x_idx = dungeon_info[y][x].effect_idx;

        /* Scan the effects on that grid */
        while (x_idx)
        {
            /* Get the effect data */
            effect_type *x_ptr = &x_list[x_idx];

            /* Point to the next effect */
            x_idx = x_ptr->next_x_idx;

            /* Ignore hidden effects */
            if (!(x_ptr->x_f_idx) ||
                (x_ptr->x_flags & (EF1_HIDDEN))) continue;

            /* We have an interesting effect */
            return (TRUE);
        }
    }

    /* Check grid type with dungeon capabilities */
    return ((*dun_cap->can_target_feature)(dungeon_info[y][x].feature_idx));
}

/*
 * Prepare the "temp" array for "target_interactive_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_interactive_prepare(int mode)
{
    int y, x;

    bool expand_look = (mode & (TARGET_LOOK)) ? TRUE : FALSE;

    /* Reset "temp" array */
    target_grids.clear();

    // Not needed.
    if (mode & (TARGET_GRID)) return;

    QRect vis = visible_dungeon();

    /* Scan the current panel */
    for (y = vis.y(); y <= vis.y() + vis.height(); y++)
    {
        for (x = vis.x(); x <= vis.x() + vis.width(); x++)
        {
            bool do_continue = FALSE;

            /* Check bounds */
            if (!in_bounds_fully(y, x)) continue;

            /* Require line of sight, unless "look" is "expanded" */
            if (!player_has_los_bold(y, x) && (!expand_look)) continue;

            /* Require "interesting" contents */
            if (!target_set_interactive_accept(mode, y, x)) continue;

            /* Special mode */
            if (mode & (TARGET_KILL))
            {
                /* Must contain a monster */
                if (!dungeon_info[y][x].has_monster()) do_continue = TRUE;

                /* Must be a targetable monster */
                if (!target_able(dungeon_info[y][x].monster_idx, FALSE)) do_continue = TRUE;
            }

            /* Don't continue on the trap exception, or if probing. */
            if ((mode & (TARGET_TRAP)) && target_able_trap(y, x)) do_continue = FALSE;
            else if (mode & (TARGET_PROBE)) do_continue = FALSE;

            if (do_continue) continue;

            /*
             * Hack - don't go over redundant elemental terrain \
             * (since we have large lakes and pools of the same terrain)
             */
            if ((p_ptr->target_row > 0) || (p_ptr->target_col > 0))
            {
                if (dungeon_info[p_ptr->target_row][p_ptr->target_col].feature_idx == dungeon_info[y][x].feature_idx)
                {
                    if (cave_ff3_match(y, x, TERRAIN_MASK)) continue;
                }
            }

            /* Save the location */
            target_grids.append(make_coords(y, x));
        }
    }

    // Sort by distance
    qSort(target_grids.begin(), target_grids.end(), coords_sort_distance);
}

/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx)
{
    int i, v;

    int x2, y2, x3, y3, x4, y4;

    int b_i = -1, b_v = 9999;

    /* Scan the locations */
    for (i = 0; i < target_grids.size(); i++)
    {
        /* Point 2 */
        x2 = target_grids[i].x;
        y2 = target_grids[i].y;

        /* Directed distance */
        x3 = (x2 - x1);
        y3 = (y2 - y1);

        /* Verify quadrant */
        if (dx && (x3 * dx <= 0)) continue;
        if (dy && (y3 * dy <= 0)) continue;

        /* Absolute distance */
        x4 = ABS(x3);
        y4 = ABS(y3);

        /* Verify quadrant */
        if (dy && !dx && (x4 > y4)) continue;
        if (dx && !dy && (y4 > x4)) continue;

        /* Approximate Double Distance */
        v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

        /* Penalize location XXX XXX XXX */

        /* Track best */
        if ((b_i >= 0) && (v >= b_v)) continue;

        /* Track best */
        b_i = i; b_v = v;
    }

    /* Result */
    return (b_i);
}

static void describe_grid_brief(int y, int x)
{
    if (y < 0) return;
    if (y >= p_ptr->cur_map_hgt) return;
    if (x < 0) return;
    if (x >= p_ptr->cur_map_wid) return;

    dungeon_type *d_ptr = &dungeon_info[y][x];
    int m_idx = d_ptr->monster_idx;
    if (m_idx > 0 && mon_list[m_idx].ml) {
        monster_type *m_ptr = mon_list + m_idx;
        QString name = monster_desc(m_ptr, 0x08);
        message("You see " + name + ".");
        return;
    }

    int saved_o_idx = -1;
    int n = 0;
    int o_idx = d_ptr->object_idx;
    while (o_idx)
    {
        object_type *o_ptr = o_list + o_idx;
        if (o_ptr->marked)
        {
            saved_o_idx = o_idx;
            ++n;
        }
        o_idx = o_ptr->next_o_idx;
    }

    if (n > 1)
    {
        message("You see a pile of objects.");
        return;
    }

    if (n == 1)
    {
        QString name = object_desc(o_list + saved_o_idx, ODESC_PREFIX | ODESC_FULL);
        message("You see " + name + ".");
        return;
    }

    if (d_ptr->cave_info & (CAVE_MARK | CAVE_SEEN))
    {
        QString x_name;
        int x_idx = d_ptr->effect_idx;
        while (x_idx) {
            effect_type *x_ptr = x_list + x_idx;
            x_idx = x_ptr->next_x_idx;
            if (x_ptr->x_flags & EF1_HIDDEN) continue;
            int feat = x_ptr->x_f_idx;
            x_name = feature_desc(feat, true, false);
            x_name += " over ";
            break;
        }

        QString f_name;
        int feat = d_ptr->feature_idx;
        feat = f_info[feat].f_mimic;
        f_name = feature_desc(feat, true, false);
        QString msg = "You see ";
        msg += x_name;
        msg += f_name;
        msg += ".";
        message(msg);
    }
}


// Try to pick the most sensible target mode based on the mode
static bool set_selected_target(int mode, int y, int x)
{
    int m_idx = dungeon_info[y][x].monster_idx;

    if (mode & (TARGET_KILL | TARGET_PROBE))
    {
        bool probing = (mode & (TARGET_PROBE));

        if ((m_idx > 0) && target_able(m_idx, probing))
        {
            health_track(m_idx);
            target_set_monster(m_idx, probing);

        }
        else target_set_location(y, x);
        return (TRUE);
    }

    if ((mode & (TARGET_TRAP)) && target_able_trap(y, x))
    {
        target_set_location(y, x);
        return (TRUE);
    }

    // Always set location for target grid
    if (mode & (TARGET_GRID))
    {
        target_set_location(y, x);
        return (TRUE);
    }
    if (!(mode & (TARGET_QUIET)))
    {
        message(QString("Illegal target!"));
    }

    return (FALSE);
}


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * Currently, when "flag" is true, that is, when
 * "interesting" grids are being used, and a directional key is used, we
 * only scroll by a single panel, in the direction requested, and check
 * for any interesting grids on that panel.  The "correct" solution would
 * actually involve scanning a larger set of grids, including ones in
 * panels which are adjacent to the one currently scanned, but this is
 * overkill for this function.  XXX XXX
 *
 * Hack -- targeting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 *
 * 'mode' is one of TARGET_LOOK or TARGET_KILL.
 * 'x' and 'y' are the initial position of the target to be highlighted,
 * or -1 if no location is specified.
 * Returns TRUE if a target has been successfully set, FALSE otherwise.
 */
bool target_set_interactive(int mode, int x, int y)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int i, d, target_count;

    bool done = FALSE;
    bool interactive = TRUE;

    u16b path_n;
    u16b path_g[PATH_SIZE];
    u16b path_gx[PATH_SIZE];

    /* Cancel target */
    target_set_monster(0, FALSE);

      /* All grids are selectable */
    if (mode & (TARGET_GRID))
    {
        /* Disable other modes */
        mode &= ~(TARGET_LOOK | TARGET_KILL | TARGET_TRAP);

        /* Disable interesting grids */
       interactive = FALSE;
    }

    /* Prepare the "temp" array */
    target_set_interactive_prepare(mode);

    /* If we haven't been given an initial location, start on the
       player. */
    if ((x == -1 || y == -1) && target_grids.size())
    {
        x = p_ptr->px;
        y = p_ptr->py;
        ui_targeting_show(MODE_TARGETING_INTERACTIVE);
        ui_update_message_label(color_string("Interactive Target Mode", TERM_L_RED));
    }
    /*
     * If we /have/ been given an initial location, make sure we
     * honour it by going into "free targeting" mode.
     */
    else
    {
        if (x == -1 || y == -1)
        {
            x = p_ptr->px;
            y = p_ptr->py;
        }

        interactive = FALSE;
        ui_targeting_show(MODE_TARGETING_MANUAL);
        ui_update_message_label(color_string("Manual Target Mode", TERM_L_RED));
    }


    /* Start near the player */
    target_count = 0;

    /* Interact */
    while (!done)
    {
        /* Interesting grids */
        if (interactive && target_grids.size())
        {
            bool path_drawn = FALSE;
            int yy, xx;

            y = target_grids[target_count].y;
            x = target_grids[target_count].x;

            /* Dummy pointers to send to project_path */
            yy = y;
            xx = x;

            /* Adjust panel if needed */
            ui_ensure(y, x);

            /* Find the path. */
            path_n = project_path(path_g, path_gx, MAX_RANGE, py, px, &yy, &xx, PROJECT_THRU);

            /* Draw the path in "target" mode. If there is one */
            if ((mode & (TARGET_KILL)) && (dungeon_info[y][x].projectable()))
            {
                path_drawn = ui_draw_path(path_n, path_g, y, x);
            }

            ui_show_cursor(y, x);

            /* Describe and Prompt */
            describe_grid_brief(y, x);

            UserInput input = ui_get_input();

            /* Remove the path */
            if (path_drawn) ui_destroy_path();

            ui_show_cursor(-1, -1);

            /* Assume no "direction" */
            d = 0;

            // Use the mouse wheel to go through targets
            if (input.mode == INPUT_MODE_MOUSE_WHEEL)
            {
                if (input.key == Qt::Key_Plus)
                {
                    if (++target_count == target_grids.size()) target_count = 0;
                }
                else if (input.key == Qt::Key_Minus)
                {
                    if (target_count-- == 0)  target_count = target_grids.size() - 1;
                }
                continue;
            }

            // double-click - automatically target if appropriate
            if (input.mode == INPUT_MODE_MOUSE_DOUBLE_CLICK)
            {
                if (!set_selected_target(mode, y, x))
                {
                    target_set_location(y, x);
                }

                done = TRUE;
                continue;

            }

            /*
             * If we click, move the target location to the click and
             * switch to "free targeting" mode by unsetting 'flag'.
             * This means we get some info about wherever we've picked.
             */
            if (input.mode == INPUT_MODE_MOUSE_SINGLE_CLICK)
            {
                // If clicking twice on the same square, accept
                if (input.x == x && input.y == y)
                {
                    if (set_selected_target(mode, y, x)) done = TRUE;
                    continue;
                }

                x = input.x;
                y = input.y;
                ui_update_message_label(color_string("Interactive Target Mode", TERM_L_RED));
                ui_targeting_show(MODE_TARGETING_MANUAL);
                interactive = FALSE;
                continue;
            }

            /* Analyze */
            switch (input.key)
            {
                case Qt::Key_Escape:
                case Qt::Key_X:
                {
                    done = TRUE;
                    break;
                }
                case Qt::Key_C:
                case Qt::Key_Comma:
                {
                    /* Set to closest target */
                    if (target_set_closest(TARGET_KILL)) done = TRUE;
                    break;
                }
                case Qt::Key_Space:
                case Qt::Key_Plus:
                {
                    if (++target_count == target_grids.size()) target_count = 0;
                    break;
                }

                case Qt::Key_Minus:
                {
                    if (target_count-- == 0)  target_count = target_grids.size() - 1;
                    break;
                }
                case Qt::Key_Exclam:
                case Qt::Key_L:
                {
                    GridDialog(y, x);
                    break;
                }
                case Qt::Key_Asterisk:
                case Qt::Key_M:
                {
                    ui_update_message_label(color_string("Manual Target Mode", TERM_L_RED));
                    ui_targeting_show(MODE_TARGETING_MANUAL);
                    interactive = FALSE;
                    break;
                }
                case Qt::Key_Question:
                {
                    do_cmd_list_targeting_commands();
                    break;
                }
                case Qt::Key_H:
                case Qt::Key_5:
                case Qt::Key_Period:
                case Qt::Key_Clear:
                {
                    if (set_selected_target(mode, y, x)) done = TRUE;
                    break;
                }

                default:
                {
                    /* Extract direction */
                    d = target_dir(input);

                    /* Oops */
                    if (!d) message("Illegal command for target mode!");

                    break;
                }
            }

            /* Hack -- move around */
            if (d)
            {
                int old_y = target_grids[target_count].y;
                int old_x = target_grids[target_count].x;

                /* Find a new monster */
                i = target_pick(old_y, old_x, ddy[d], ddx[d]);

                /* Scroll to find interesting grid */
                if (i < 0)
                {
                    QRect vis = visible_dungeon();

                    int old_wy = vis.y();
                    int old_wx = vis.x();

                    /* Change if legal */
                    if (ui_change_panel(d))
                    {
                        /* Recalculate interesting grids */
                        target_set_interactive_prepare(mode);

                        /* Find a new monster */
                        i = target_pick(old_y, old_x, ddy[d], ddx[d]);

                        /* Restore panel if needed */
                        if ((i < 0) && ui_modify_panel(old_wy, old_wx))
                        {
                            /* Recalculate interesting grids */
                            target_set_interactive_prepare(mode);
                        }
                    }
                }

                /* Use interesting grid if found */
                if (i >= 0) target_count = i;
            }
        }

        /* Arbitrary grids */
        else
        {
            bool path_drawn = FALSE;

            /* Dummy pointers to send to project_path */
            int yy = y;
            int xx = x;

            /* Find the path. */
            path_n = project_path(path_g, path_gx, MAX_RANGE, py, px, &yy, &xx, PROJECT_THRU);

            /* Draw the path in "target" mode. If there is one */
            if ((mode & (TARGET_KILL)) && (dungeon_info[y][x].projectable()))
            {
                /* Save target info */
                path_drawn = ui_draw_path(path_n, path_g, y, x);
            }

            describe_grid_brief(y, x);

            ui_show_cursor(y, x);

            UserInput input = ui_get_input();

            /* Remove the path */
            if (path_drawn) ui_destroy_path();

            ui_show_cursor(y, x);

            /* Assume no direction */
            d = 0;

             if (input.mode == INPUT_MODE_MOUSE_WHEEL) continue;

            // double-click - automatically target if appropriate
            if (input.mode == INPUT_MODE_MOUSE_DOUBLE_CLICK)
            {

                if (set_selected_target(mode, y, x)) done = TRUE;
                else
                {
                    message(QString("Illegal target!"));
                }
                break;
            }

            if (input.mode == INPUT_MODE_MOUSE_SINGLE_CLICK)
            {
                /* We only target if we click somewhere where the cursor
                   is already (i.e. a double-click without a time limit) */
                if (input.x == x && input.y == y)
                {
                    target_set_location(y, x);
                    done = TRUE;
                }
                else
                {
                    /* Just move the cursor for now - another click will
                       target. */
                    x = input.x;
                    y = input.y;
                }
                continue;
            }

            /* Analyze the keypress */
            switch (input.key)
            {
                case Qt::Key_Escape:
                case Qt::Key_X:
                {
                    done = TRUE;
                    continue;
                }
                case Qt::Key_Asterisk:
                case Qt::Key_M:
                {
                    if (((mode & (TARGET_GRID)) != TARGET_GRID) && target_grids.size())
                    {
                        ui_update_message_label(color_string("Interactive Target Mode", TERM_L_RED));
                        ui_targeting_show(MODE_TARGETING_INTERACTIVE);
                        interactive = TRUE;
                    }
                    break;
                }
                case Qt::Key_copyright:
                case Qt::Key_C:
                case Qt::Key_Comma:
                {
                    /* Set to closest target */
                    if (target_set_closest(TARGET_KILL)) done = TRUE;
                    break;
                }
                case Qt::Key_Exclam:
                case Qt::Key_L:
                {
                    GridDialog(y, x);
                    break;
                }

                case Qt::Key_Ampersand:
                case Qt::Key_P:
                {
                    /* Recenter around player */
                    ui_center(py, px);

                    y = py;
                    x = px;

                    break;
                }

                case Qt::Key_H:
                case Qt::Key_5:
                case Qt::Key_Period:
                case Qt::Key_Clear:
                {
                    target_set_location(y, x);
                    done = TRUE;
                    break;
                }

                case Qt::Key_Question:
                {
                    do_cmd_list_targeting_commands();
                    break;
                }
                default:
                {
                    /* Extract a direction */
                    d = target_dir(input);

                    /* Oops */
                    if (!d) message("Illegal command for target mode!");

                    break;
                }
            }

            /* Handle "direction" */
            if (d)
            {
                int dungeon_hgt = p_ptr->cur_map_hgt;
                int dungeon_wid = p_ptr->cur_map_wid;

                /* Move */
                x += ddx[d];
                y += ddy[d];

                /* Slide into legality */
                if (x >= dungeon_wid - 1) x--;
                else if (x <= 0) x++;

                /* Slide into legality */
                if (y >= dungeon_hgt - 1) y--;
                else if (y <= 0) y++;

                /* Adjust panel if needed */
                if (ui_adjust_panel(y, x))
                {
                    /* Recalculate interesting grids */
                    target_set_interactive_prepare(mode);
                }
            }
        }
    }

    /* Forget */
    target_grids.clear();

    ui_targeting_hide();

    /* Recenter around player */
    ui_ensure(py, px);

    /* Failure to set target */
    if (!p_ptr->target_set) return (FALSE);

    /* Success */
    return (TRUE);
}

/*
 * Set the target to a monster (or nobody)
 */
void target_set_monster(int m_idx, bool probing)
{
    /* Acceptable target */
    if ((m_idx > 0) && target_able(m_idx, probing))
    {
        monster_type *m_ptr = &mon_list[m_idx];

        /* Save target info */
        p_ptr->target_set = TRUE;
        p_ptr->target_who = m_idx;
        p_ptr->target_row = m_ptr->fy;
        p_ptr->target_col = m_ptr->fx;
    }

    /* Clear target */
    else
    {
        /* Reset target info */
        p_ptr->target_set = FALSE;
        p_ptr->target_who = 0;
        p_ptr->target_row = 0;
        p_ptr->target_col = 0;
    }

    p_ptr->redraw |= (PR_SIDEBAR_MON);
}


/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(void)
{
    /* No target */
    if (!p_ptr->target_set) return (FALSE);

    /* Accept "location" targets */
    if (p_ptr->target_who == 0) return (TRUE);

    /* Check "monster" targets */
    if (p_ptr->target_who > 0)
    {
        int m_idx = p_ptr->target_who;

        /* Accept reasonable targets */
        if (target_able(m_idx, FALSE))
        {
            monster_type *m_ptr = &mon_list[m_idx];

            /* Get the monster location */
            p_ptr->target_row = m_ptr->fy;
            p_ptr->target_col = m_ptr->fx;

            /* Good target */
            return (TRUE);
        }
    }

    /* Assume no target */
    return (FALSE);
}



/*
 * Set the target to a location
 */
void target_set_location(int y, int x)
{
    /* Legal target */
    if (in_bounds_fully(y, x))
    {
        /* Save target info */
        p_ptr->target_set = TRUE;
        p_ptr->target_who = 0;
        p_ptr->target_row = y;
        p_ptr->target_col = x;
    }

    /* Clear target */
    else
    {
        /* Reset target info */
        p_ptr->target_set = FALSE;
        p_ptr->target_who = 0;
        p_ptr->target_row = 0;
        p_ptr->target_col = 0;
    }

    p_ptr->redraw |= (PR_SIDEBAR_MON);
}



/*
 * Extract a direction (or zero) from a character
 */
int target_dir(UserInput input)
{
    int d = 0;

    if (input.mode != INPUT_MODE_KEY) return 0;


    /* Already a direction? */
    if (!input.text.isEmpty() && input.text.at(0).isDigit())
    {
        d = input.text.mid(0, 1).toInt();
    }
    // handle arrow directions
    else switch (input.key)
    {
        // Left
        case Qt::Key_Left:
        case Qt::Key_4:
        case Qt::Key_G:
        {
            d = DIR_WEST;
            break;
        }
        // Right
        case Qt::Key_Right:
        case Qt::Key_6:
        case Qt::Key_J:
        {
            d = DIR_EAST;
            break;
        }
        // Up
        case Qt::Key_Up:
        case Qt::Key_8:
        case Qt::Key_Y:
        {
            d = DIR_NORTH;
            break;
        }
        // Down
        case Qt::Key_Down:
        case Qt::Key_2:
        case Qt::Key_B:
        {
            d = DIR_SOUTH;
            break;
        }
        // left and up
        case Qt::Key_7:
        case Qt::Key_T:
        case Qt::Key_Home:
        {
            d = DIR_NORTHWEST;
            break;
        }
        // right and up
        case Qt::Key_9:
        case Qt::Key_U:
        case Qt::Key_PageUp:
        {
            d = DIR_NORTHEAST;
            break;
        }
        // left and down
        case Qt::Key_1:
        case Qt::Key_V:
        case Qt::Key_End:
        {
            d = DIR_SOUTHWEST;
            break;
        }
        // right and down
        case Qt::Key_3:
        case Qt::Key_N:
        case Qt::Key_PageDown:
        {
            d = DIR_SOUTHEAST;
            break;
        }
        // Illegal direction
        default:
        {
            d = DIR_UNKNOWN;
            break;
        }
    }

    /* Return direction */
    return (d);
}



int dir_transitions[10][10] =
{
    /* 0-> */ { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
    /* 1-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    /* 2-> */ { 0, 0, 2, 0, 1, 0, 3, 0, 5, 0 },
    /* 3-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    /* 4-> */ { 0, 0, 1, 0, 4, 0, 5, 0, 7, 0 },
    /* 5-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    /* 6-> */ { 0, 0, 3, 0, 5, 0, 6, 0, 9, 0 },
    /* 7-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    /* 8-> */ { 0, 0, 5, 0, 7, 0, 9, 0, 8, 0 },
    /* 9-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
};




/*
 * Get an "aiming direction" (1,2,3,4,6,7,8,9 or 5) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * The direction "5" is special, and means "use current target". Also DIR_TARGET
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Currently this function applies confusion directly.
 */
bool get_aim_dir(int *dp, bool target_trap)
{
    /* Global direction */
    int dir = 0;
    int old_dir;

    bool done = FALSE;

    int mode = TARGET_QUIET;

    if (target_trap) mode |= TARGET_KILL;
    else mode |= TARGET_TRAP;

    if (*dp == DIR_CLOSEST)
    {
        if (target_set_closest(mode))
        {
            return(TRUE);
        }
    }

    /* Initialize */
    (*dp) = 0;

    /* Hack -- auto-target if requested */
    if (use_old_target && target_okay() && !dir) dir = DIR_TARGET;

    else ui_update_message_label(color_string("Please select a target.", TERM_L_RED));

    ui_targeting_show(MODE_TARGETING_AIMING);

    /* Ask until satisfied */
    while (!dir && !done)
    {
        ui_show_cursor(p_ptr->py, p_ptr->px);        

        /* Get a command (or Cancel) */
        UserInput input = ui_get_input();

        // Paranoia
        if (input.mode == INPUT_MODE_NONE) break;

        if ((input.key == Qt::Key_Escape) || (input.key == Qt::Key_X))
        {
            break;
        }
        // Do nothing
        if (input.mode == INPUT_MODE_MOUSE_WHEEL)
        {
            continue;
        }

        // Skip interactive mode and directly choose target.
        if (input.mode == INPUT_MODE_MOUSE_DOUBLE_CLICK)
        {
            if (set_selected_target(mode, input.y, input.x)) dir = DIR_TARGET;
            else set_selected_target(TARGET_GRID, input.y, input.x);
            dir = DIR_TARGET;
            continue;
        }

        if (input.mode == INPUT_MODE_MOUSE_SINGLE_CLICK)
        {
            /* Calculate approximate angle */
            if (target_set_interactive(mode, input.x, input.y)) dir = DIR_TARGET;
            else done = TRUE;
            continue;
        }

        /* Analyze */
        switch (input.key)
        {
            case Qt::Key_M:
            case Qt::Key_Asterisk:
            {
                /* Set new target, use target if legal */
                int mode = TARGET_KILL;
                if (target_trap) mode |= TARGET_TRAP;
                if (target_set_interactive(mode, -1, -1)) dir = DIR_TARGET;
                else done = TRUE;
                continue;
            }
            case Qt::Key_C:
            case Qt::Key_Comma:
            {
                /* Set to closest target */
                if (target_set_closest(TARGET_KILL))
                {
                    dir = DIR_CLOSEST;
                    continue;
                }
                break;
            }
            case Qt::Key_Question:
            {
                do_cmd_list_targeting_commands();
                continue;
            }
            case Qt::Key_H:
            case Qt::Key_5:
            case Qt::Key_Period:
            case Qt::Key_Clear:
            {
                /* Use current target, if set and legal */
                if (target_okay()) dir = DIR_TARGET;
                break;
            }
            default:
            {
                /* Possible direction */
                dir = target_dir(input);
                break;
            }
        }

        /* Error */
        if (!dir) color_message("Illegal aim direction!", TERM_ORANGE);
    }

    ui_targeting_hide();

    ui_show_cursor(-1, -1);

    ui_clear_message_label();

    /* No direction */
    if (!dir) return (FALSE);

    /* Save the direction */
    old_dir = dir;

    /* Check for confusion */
    if (p_ptr->timed[TMD_CONFUSED])
    {
        /* Random direction */
        dir = ddd[randint0(8)];
    }

    /* Notice confusion */
    if (old_dir != dir)
    {
        /* Warn the user */
        message(QString("You are confused."));
    }

    /* Save direction */
    (*dp) = dir;

    /* A "valid" direction was entered */
    return (TRUE);
}


bool target_set_closest(int mode)
{
    /* Cancel old target */
    target_set_monster(0, FALSE);

    target_grids.clear();

    //
    if (mode & (TARGET_KILL | TARGET_PROBE))
    {
        monster_type *m_ptr;
        int m_idx;

        bool probing = (mode & (TARGET_PROBE));

        for (int i = 1; i < mon_max; i++)
        {
            if (!target_able(i, probing)) continue;
            m_ptr = &mon_list[i];
            target_grids.append(make_coords(m_ptr->fy, m_ptr->fx));
        }

        if (!target_grids.size())
        {
            if (!(mode & TARGET_QUIET)) message(QString("No Available Target."));
            return FALSE;
        }

        // Sort by distance
        qSort(target_grids.begin(), target_grids.end(), coords_sort_distance);


        /* Find the first monster in the queue */
        int y = target_grids.at(0).y;
        int x = target_grids.at(0).x;
        m_idx = dungeon_info[y][x].monster_idx;

        /* Target the monster */
        m_ptr = &mon_list[m_idx];

        if (!(mode & TARGET_QUIET))
        {
            QString m_name = monster_desc(m_ptr, 0x00);
            message(QString("%1 is targeted.").arg(capitalize_first(m_name)));
        }

        /* Set up target  */
        monster_race_track(m_ptr->r_idx);
        health_track(m_idx);
        target_set_monster(m_idx, probing);
        return (TRUE);
    }
    else if (mode & (TARGET_TRAP))
    {
        // GO through all effects
        for (int i = x_max - 1; i >= 1; i--)
        {
            effect_type *x_ptr = &x_list[i];

            /* Skip dead effects */
            if (!x_ptr->x_type) continue;

            // Use only the targetable traps
            if (!target_able_trap(x_ptr->x_cur_y, x_ptr->x_cur_x)) continue;

            target_grids.append(make_coords(x_ptr->x_cur_y, x_ptr->x_cur_x));
        }

        // Sort by distance
        qSort(target_grids.begin(), target_grids.end(), coords_sort_distance);

        if (!target_grids.size())
        {
            if (!(mode & TARGET_QUIET)) message(QString("No Available Target."));
            return FALSE;
        }

        /* Find the first monster in the queue */
        int y = target_grids.at(0).y;
        int x = target_grids.at(0).x;
        {
            // Use this location
            target_set_location(y, x);
            return (TRUE);
        }
    }

    return (TRUE);
}

/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player.
 *
 * Directions "5" and "0" are illegal and will not be accepted.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 */
bool get_rep_dir(int *dp)
{
    int dir = 0;

    /* Initialize */
    (*dp) = 0;


    if (!dir)
    {
        ui_update_message_label(color_string("Please select a direction.", TERM_L_RED));

        ui_targeting_show(MODE_TARGETING_DIRECTION);
    }

    /* Get a direction */
    while (!dir)
    {
        UserInput input = ui_get_input();

        if (input.mode == INPUT_MODE_KEY)
        {
            if ((input.key == Qt::Key_Escape) || (input.key == Qt::Key_X)) dir = DIR_TARGET;

            else if (input.key == Qt::Key_Question)
            {
                do_cmd_list_targeting_commands();
                continue;
            }

            else dir = target_dir(input);
        }

        // Do nothing
        else  if (input.mode == INPUT_MODE_MOUSE_WHEEL)
        {
            continue;
        }
        /* Check mouse coordinates */
        else if ((input.mode == INPUT_MODE_MOUSE_SINGLE_CLICK) ||
                 (input.mode == INPUT_MODE_MOUSE_DOUBLE_CLICK))
        {
            /* Calculate approximate angle */
            dir = ui_get_dir_from_slope(p_ptr->py, p_ptr->px, input.y, input.x);
        }
    }

    ui_targeting_hide();

    if (!dir) color_message("Illegal direction", TERM_ORANGE);

    ui_clear_message_label();

    if (dir == DIR_TARGET) return false;

    /* Save desired direction */
    p_ptr->player_args.direction = dir;

    /* Save direction */
    (*dp) = dir;

    /* Success */
    return (TRUE);
}

/*
 * Apply confusion, if needed, to a direction
 *
 * Display a message and return TRUE if direction changes.
 */
bool confuse_dir(int *dp)
{
    int dir;

    /* Default */
    dir = (*dp);

    /* Apply "confusion" */
    if (p_ptr->timed[TMD_CONFUSED])
    {
        /* Apply confusion XXX XXX XXX */
        if ((dir == DIR_TARGET) || (rand_int(100) < 75))
        {
            /* Random direction */
            dir = ddd[rand_int(8)];
        }
    }

    /* Notice confusion */
    if ((*dp) != dir)
    {
        /* Warn the user */
        message("You are confused.");

        /* Save direction */
        (*dp) = dir;

        /* Confused */
        return (TRUE);
    }

    /* Not confused */
    return (FALSE);
}
