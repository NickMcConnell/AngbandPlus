/* File: generate.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/dun_generate.h"
#include "src/store.h"

// This file has most of the code to create a standard, simple dungeon.


/*
 *
 * Consider the "vault.txt" file for vault generation.
 *
 * In this file, we use the "special" granite and perma-wall sub-types,
 * where "basic" is normal, "inner" is inside a room, "outer" is the
 * outer wall of a room, and "solid" is the outer wall of the dungeon
 * or any walls that may not be pierced by corridors.  Thus the only
 * wall type that may be pierced by a corridor is the "outer granite"
 * type.  The "basic granite" type yields the "actual" corridors.
 *
 * Note that we use the special "solid" granite wall type to prevent
 * multiple corridors from piercing a wall in two adjacent locations,
 * which would be messy, and we use the special "outer" granite wall
 * to indicate which walls "surround" rooms, and may thus be "pierced"
 * by corridors entering or leaving the room.
 *
 * Note that a tunnel which attempts to leave a room near the "edge"
 * of the dungeon in a direction toward that edge will cause "silly"
 * wall piercings, but will have no permanently incorrect effects,
 * as long as the tunnel can *eventually* exit from another side.
 * And note that the wall may not come back into the room by the
 * hole it left through, so it must bend to the left or right and
 * then optionally re-enter the room (at least 2 grids away).  This
 * is not a problem since every room that is large enough to block
 * the passage of tunnels is also large enough to allow the tunnel
 * to pierce the room itself several times.
 *
 * Note that no two corridors may enter a room through adjacent grids,
 * they must either share an entryway or else use entryways at least
 * two grids apart.  This prevents "large" (or "silly") doorways.
 *
 * To create rooms in the dungeon, we first divide the dungeon up
 * into "blocks" of 11x11 grids each, and require that all rooms
 * occupy a rectangular group of blocks.  As long as each room type
 * reserves a sufficient number of blocks, the room building routines
 * will not need to check bounds.  Note that most of the normal rooms
 * actually only use 23x11 grids, and so reserve 33x11 grids.
 *
 * Note that the use of 11x11 blocks (instead of the 33x11 panels)
 * allows more variability in the horizontal placement of rooms, and
 * at the same time has the disadvantage that some rooms (two thirds
 * of the normal rooms) may be "split" by panel boundaries.  This can
 * induce a situation where a player is in a room and part of the room
 * is off the screen.  This can be so annoying that the player must set
 * a special option to enable "non-aligned" room generation.
 *
 * Note that the dungeon generation routines are much different (2.7.5)
 * and perhaps "DUN_ROOMS" should be less than 50.
 *
 * XXX XXX XXX Note that it is possible to create a room which is only
 * connected to itself, because the "tunnel generation" code allows a
 * tunnel to leave a room, wander around, and then re-enter the room.
 *
 * XXX XXX XXX Note that it is possible to create a set of rooms which
 * are only connected to other rooms in that set, since there is nothing
 * explicit in the code to prevent this from happening.  But this is less
 * likely than the "isolated room" problem, because each room attempts to
 * connect to another room, in a giant cycle, thus requiring at least two
 * bizarre occurances to create an isolated section of the dungeon.

 *
 */



/*
 * Always picks a correct direction
 */
static void correct_dir(int *rdir, int *cdir, int y1, int x1, int y2, int x2)
{
    /* Extract vertical and horizontal directions */
    *rdir = (y1 == y2) ? 0 : (y1 < y2) ? 1 : -1;
    *cdir = (x1 == x2) ? 0 : (x1 < x2) ? 1 : -1;

    /* Never move diagonally */
    if (*rdir && *cdir)
    {
        if (one_in_(2))
        {
            *rdir = 0;
        }
        else
        {
            *cdir = 0;
        }
    }
}


/*
 * Returns true if you are next to stairs.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 */
static int next_to_stairs(int y, int x)
{
    int k = 0;

    if (cave_ff1_match(y + 1, x, FF1_STAIRS)) k++;
    if (cave_ff1_match(y - 1, x, FF1_STAIRS)) k++;
    if (cave_ff1_match(y, x + 1, FF1_STAIRS)) k++;
    if (cave_ff1_match(y, x - 1, FF1_STAIRS)) k++;

    return (k);
}


/*
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 *
 * We count only stairs, granite walls and permanent walls.
 */
int next_to_walls(int y, int x)
{
    int k = 0;

    if (cave_ff1_match(y + 1, x, (FF1_WALL | FF1_STAIRS))) k++;
    if (cave_ff1_match(y - 1, x, (FF1_WALL | FF1_STAIRS))) k++;
    if (cave_ff1_match(y, x + 1, (FF1_WALL | FF1_STAIRS))) k++;
    if (cave_ff1_match(y, x - 1, (FF1_WALL | FF1_STAIRS))) k++;

    return (k);
}


/*
 * Pick a random direction
 */
static void rand_dir(int *rdir, int *cdir)
{
    /* Pick a random direction */
    int i = rand_int(4);

    /* Extract the dy/dx components */
    *rdir = ddy_ddd[i];
    *cdir = ddx_ddd[i];
}


/*
 * Returns random co-ordinates for player/monster/object
 * currently only used for player.
 */
bool new_player_spot_old(void)
{
    int yy, xx;

    QVector<coord> locations;
    locations.clear();

    /* Find a spot for the player */
    for (yy = 0; yy < p_ptr->cur_map_hgt; yy++)
    {
        for (xx = 0; xx < p_ptr->cur_map_wid; xx++)
        {
            /* Must be a "start" floor grid */
            if (!cave_start_bold(yy, xx)) continue;

            /* Refuse to start on anti-teleport grids */
            if (dungeon_info[yy][xx].cave_info & (CAVE_ICKY)) continue;

            /* Refuse to start on room grids if necessary */
            if (!(*dun_cap->can_place_player_in_rooms)() &&
                (dungeon_info[yy][xx].cave_info & (CAVE_ROOM))) continue;

            /* We like to be next to walls */
            if (next_to_walls(yy, xx) < 3) continue;

            locations.append(make_coords(yy, xx));
        }
    }

    /* Paranoia */
    if (!locations.size()) return (FALSE);

    /*
     * Try to place the player "i" times to reduce the number of failed
     * levels. -DG-
     */
    for (int tries = 0; tries < locations.size(); tries++)
    {
        /* Select a location */
        int rand_spot = randint0(locations.size());

        /* Place the player, check for failure */
        if (player_place(locations.at(rand_spot).y, locations.at(rand_spot).x))
        {
            return (TRUE);
        }
    }

    return (FALSE);
}

/*
 * Convert existing terrain type to rubble
 */
static void place_rubble(int y, int x)
{
    /* Create rubble */
    int effect = FEAT_RUBBLE;
    if (one_in_(3)) effect = FEAT_RUBBLE_HIDDEN_OBJECT;

    set_effect_rocks(effect, y, x);
}


/*
 * Pick either an ordinary up staircase or an up shaft.
 */
static int pick_up_stairs(void)
{
    /* No shafts in Moria */
    if (game_mode == GAME_NPPMORIA) return (FEAT_STAIRS_UP);

    if (p_ptr->depth >= 2)
    {
        if (one_in_(2)) return (FEAT_SHAFT_UP);
    }

    return (FEAT_STAIRS_UP);
}


/*
 * Pick either an ordinary down staircase or an down shaft.
 */
static int pick_down_stairs(void)
{
    /* No shafts in Moria */
    if (game_mode == GAME_NPPMORIA) return (FEAT_STAIRS_DOWN);

    if ((p_ptr->depth < MAX_DEPTH - 2) &&
        (!quest_check(p_ptr->depth + 1)))
    {
        if (one_in_(2)) return (FEAT_SHAFT_DOWN);
    }

    return (FEAT_STAIRS_DOWN);
}


/*
 * Place an up/down staircase at given location
 */
void place_random_stairs(int y, int x)
{
    /* Paranoia */
    if (!cave_clean_bold(y, x)) return;

    /* Create a staircase */
    if (!p_ptr->depth)
    {
        cave_set_feat(y, x, FEAT_STAIRS_DOWN);
    }
    else if ((quest_check(p_ptr->depth)) || (p_ptr->depth >= MAX_DEPTH-1))
    {
        if ((p_ptr->depth < 2) || one_in_(2))	cave_set_feat(y, x, FEAT_STAIRS_UP);
        else cave_set_feat(y, x, FEAT_SHAFT_UP);
    }
    else if (one_in_(2))
    {
        /* No shafts in Moria */
        if (game_mode == GAME_NPPMORIA) cave_set_feat(y, x, FEAT_STAIRS_DOWN);
        else if ((quest_check(p_ptr->depth + 1)) || (p_ptr->depth <= 1))
            cave_set_feat(y, x, FEAT_STAIRS_DOWN);
        else if (one_in_(2)) cave_set_feat(y, x, FEAT_STAIRS_DOWN);
        else cave_set_feat(y, x, FEAT_SHAFT_DOWN);
    }
    else
    {
        if (game_mode == GAME_NPPMORIA) cave_set_feat(y, x, FEAT_STAIRS_UP);
        else if ((one_in_(2)) || (p_ptr->depth == 1)) cave_set_feat(y, x, FEAT_STAIRS_UP);
        else cave_set_feat(y, x, FEAT_SHAFT_UP);
    }
}




/*
 * Allocates some objects (using "place" and "type")
 */
void alloc_object(int set, int typ, int num)
{
    int y, x, k, i;

    /* Place some objects */
    for (k = 0; k < num; k++)
    {
        /* Pick a "legal" spot */
        for (i = 0; i < 10000; i++)
        {
            bool is_room;

            /* Location */
            y = rand_int(p_ptr->cur_map_hgt);
            x = rand_int(p_ptr->cur_map_wid);

            switch (typ)
            {
                /* Some objects need specific terrain types */
                case ALLOC_TYP_GOLD:
                case ALLOC_TYP_OBJECT:
                case ALLOC_TYP_CHEST:
                case ALLOC_TYP_PARCHMENT:
                {
                    /* Require "clean" grid */
                    if (!cave_clean_bold(y, x)) continue;

                    /* Hack -- Ensure object/gold creation */
                    if (f_info[dungeon_info[y][x].feature_idx].dam_non_native > 0) continue;

                    break;
                }
                /* Traps */
                case ALLOC_TYP_TRAP:
                {
                    /* Require "trappable" floor grid */
                    if (!cave_trappable_bold(y, x)) continue;

                    /* Require "empty" grid */
                    if (dungeon_info[y][x].monster_idx) continue;

                    break;
                }

                /* ALLOC_TYP_RUBBLE */
                default:
                {
                    /* Require "naked" floor grid */
                    if (!cave_naked_bold(y, x)) continue;

                    break;
                }
            }

            /* Check for "room" */
            is_room = ((dungeon_info[y][x].cave_info & (CAVE_ROOM)) != 0);

            /* Require corridor? */
            if ((set == ALLOC_SET_CORR) && is_room) continue;

            /* Require room? */
            if ((set == ALLOC_SET_ROOM) && !is_room) continue;

            /* Accept it */
            break;
        }

        /* No point found */
        if (i == 10000) return;

        /* Place something */
        switch (typ)
        {
            case ALLOC_TYP_RUBBLE:
            {
                place_rubble(y, x);
                break;
            }

            case ALLOC_TYP_TRAP:
            {
                place_trap(y, x, 0);
                break;
            }

            case ALLOC_TYP_GOLD:
            {
                place_gold(y, x);
                break;
            }

            case ALLOC_TYP_OBJECT:
            {
                place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
                break;
            }
            case ALLOC_TYP_PARCHMENT:
            {
                object_type object_type_body;
                object_type *i_ptr = &object_type_body;

                /* Use a small wooden chest */
                int k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);

                /* Get a random chest */
                i_ptr->object_wipe();
                object_prep(i_ptr, k_idx);
                apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);

                /*Don't let the player see what the object it, and make it a quest item*/
                i_ptr->ident |= (IDENT_HIDE_CARRY | IDENT_QUEST);

                i_ptr->mark_known(TRUE);

                /* Give it to the floor */
                (void)floor_carry(y, x, i_ptr);
                break;
            }
            default: break;
        }
    }
}



/*
 * Places "streamers" of rock through dungeon
 *
 * Note that their are actually six different terrain features used
 * to represent streamers.  Three each of magma and quartz, one for
 * basic vein, one with hidden gold, and one with known gold.  The
 * hidden gold types are currently unused.
 */
static bool build_streamer(u16b feat, int chance)
{
    int i, tx, ty;
    int y, x, dir;
    int tries1 = 0;
    int tries2 = 0;
    u16b new_feat;

    /* Hack -- Choose starting point */
    y = rand_spread(p_ptr->cur_map_hgt / 2, 10);
    x = rand_spread(p_ptr->cur_map_wid / 2, 15);

    /* Choose a random compass direction */
    dir = ddd[rand_int(8)];

    /* Place streamer into dungeon */
    while (TRUE)
    {
        tries1++;

        if (tries1 > 2500) return(FALSE);

        /* One grid per density */
        for (i = 0; i < DUN_STR_DEN; i++)
        {
            int d = DUN_STR_RNG;

            /* Pick a nearby grid */
            while (TRUE)
            {
                tries2++;
                if (tries2 > 2500) return (FALSE);
                ty = rand_spread(y, d);
                tx = rand_spread(x, d);
                if (!in_bounds(ty, tx)) continue;
                break;
            }

            /* Only convert "granite" walls */
            if (!f_info[dungeon_info[ty][tx].feature_idx].f_name.contains("granite")) continue;

            /* We'll add the given vein type */
            new_feat = feat;

            /* Hack -- Add some (known) treasure */
            if (one_in_(chance))
            {
                /* Quartz with treasure? */
                if (feat == FEAT_QUARTZ_VEIN)
                {
                    new_feat = FEAT_QUARTZ_VEIN_TREASURE;
                }
                /* Magma with treasure? */
                else if (feat == FEAT_MAGMA_VEIN)
                {
                    new_feat = FEAT_MAGMA_VEIN_TREASURE;
                }
            }

            /* Clear previous contents, add proper vein type */
            cave_set_feat(ty, tx, new_feat);
        }

        /* Advance the streamer */
        y += ddy[dir];
        x += ddx[dir];

        /* Stop at dungeon edge */
        if (!in_bounds(y, x)) break;
    }

    return (TRUE);
}


/*
 * Build a destroyed level
 */
static void destroy_level(void)
{
    int y1, x1, y, x, k, t, n;


    /* Note destroyed levels */
    if (cheat_room)
    {
        message(QString("Destroyed Level"));
    }

    /* Drop a few epi-centers (usually about two) */
    for (n = 0; n < randint(5); n++)
    {
        /* Pick an epi-center */
        x1 = rand_range(5, p_ptr->cur_map_hgt-1 - 5);
        y1 = rand_range(5, p_ptr->cur_map_wid-1 - 5);

        /* Big area of affect */
        for (y = (y1 - 15); y <= (y1 + 15); y++)
        {
            for (x = (x1 - 15); x <= (x1 + 15); x++)
            {
                /* Skip illegal grids */
                if (!in_bounds_fully(y, x)) continue;

                /* Extract the distance */
                k = distance(y1, x1, y, x);

                /* Stay in the circle of death */
                if (k >= 16) continue;

                /* Delete the monster (if any) */
                delete_monster(y, x);

                /* Destroy valid grids */
                if (cave_valid_bold(y, x))
                {
                    /* Delete objects */
                    delete_object(y, x);

                    /* Wall (or floor) type */
                    t = rand_int(200);

                    /* Burn stuff */
                    if (cave_ff2_match(y, x, FF2_HURT_FIRE))
                    {
                        cave_alter_feat(y, x, FS_HURT_FIRE);
                    }

                    /* Granite */
                    else if (t < 20)
                    {
                        /* Create granite wall */
                        cave_set_feat(y, x, FEAT_WALL_GRANITE);
                    }

                    /* Quartz */
                    else if (t < 70)
                    {
                        /* Create quartz vein */
                        cave_set_feat(y, x, FEAT_QUARTZ_VEIN);
                    }

                    /* Magma */
                    else if (t < 100)
                    {
                        /* Create magma vein */
                        cave_set_feat(y, x, FEAT_MAGMA_VEIN);
                    }

                    /* Rubble */
                    else if (t < 130)
                    {
                        /* Create rubble */
                        cave_set_feat(y, x, FEAT_FLOOR);
                        place_rubble(y, x);
                    }

                    /* Floor */
                    else
                    {
                        /* Create floor */
                        cave_set_feat(y, x, FEAT_FLOOR);
                    }

                    /* No longer part of a room or vault */
                    dungeon_info[y][x].cave_info &= ~(CAVE_ROOM | CAVE_ICKY);

                    /* No longer illuminated */
                    dungeon_info[y][x].cave_info &= ~(CAVE_GLOW);
                }
            }
        }
    }
}



/*
 * Constructs a tunnel between two points
 *
 * This function must be called BEFORE any streamers are created,
 * since we use the special "granite wall" sub-types to keep track
 * of legal places for corridors to pierce rooms.
 *
 * We use "door_flag" to prevent excessive construction of doors
 * along overlapping corridors.
 *
 * We queue the tunnel grids to prevent door creation along a corridor
 * which intersects itself.
 *
 * We queue the wall piercing grids to prevent a corridor from leaving
 * a room and then coming back in through the same entrance.
 *
 * We "pierce" grids which are "outer" walls of rooms, and when we
 * do so, we change all adjacent "outer" walls of rooms into "solid"
 * walls so that no two corridors may use adjacent grids for exits.
 *
 * The "solid" wall check prevents corridors from "chopping" the
 * corners of rooms off, as well as "silly" door placement, and
 * "excessively wide" room entrances.
 *
 * Useful "feat" values:
 *   FEAT_WALL_GRANITE -- granite walls
 *   FEAT_WALL_GRANITE_INNER -- inner room walls
 *   FEAT_WALL_GRANITE_OUTER -- outer room walls
 *   FEAT_WALL_GRANITE_SOLID -- solid room walls
 *   FEAT_PERM_EXTRA -- shop walls (perma)
 *   FEAT_WALL_PERM_INNER -- inner room walls (perma)
 *   FEAT_WALL_PERM_OUTER -- outer room walls (perma)
 *   FEAT_WALL_PERM_SOLID -- dungeon border (perma)
 */
void build_tunnel(int row1, int col1, int row2, int col2)
{
    int i, y, x;
    int tmp_row, tmp_col;
    int row_dir, col_dir;
    int start_row, start_col;
    int main_loop_count = 0;

    bool door_flag = FALSE;

    /* Reset the arrays */
    dun->tunn_n = 0;
    dun->wall_n = 0;

    /* Save the starting location */
    start_row = row1;
    start_col = col1;

    /* Start out in the correct direction */
    correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

    /* Keep going until done (or bored) */
    while ((row1 != row2) || (col1 != col2))
    {
        /* Mega-Hack -- Paranoia -- prevent infinite loops */
        if (main_loop_count++ > 2000) break;

        /* Allow bends in the tunnel */
        if (rand_int(100) < DUN_TUN_CHG)
        {
            /* Get the correct direction */
            correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

            /* Random direction */
            if (rand_int(100) < DUN_TUN_RND)
            {
                rand_dir(&row_dir, &col_dir);
            }
        }

        /* Get the next location */
        tmp_row = row1 + row_dir;
        tmp_col = col1 + col_dir;

        /* Do not leave the dungeon!!! XXX XXX */
        while (!in_bounds_fully(tmp_row, tmp_col))
        {
            /* Get the correct direction */
            correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

            /* Random direction */
            if (rand_int(100) < DUN_TUN_RND)
            {
                rand_dir(&row_dir, &col_dir);
            }

            /* Get the next location */
            tmp_row = row1 + row_dir;
            tmp_col = col1 + col_dir;
        }

        /* Avoid the edge of the dungeon */
        if (dungeon_info[tmp_row][tmp_col].feature_idx == FEAT_WALL_PERM_SOLID) continue;

        /* Avoid the edge of vaults */
        if (dungeon_info[tmp_row][tmp_col].feature_idx == FEAT_WALL_PERM_OUTER) continue;

        /* Avoid "solid" granite walls */
        if (dungeon_info[tmp_row][tmp_col].feature_idx == FEAT_WALL_GRANITE_SOLID) continue;

        /* Pierce "outer" walls of rooms */
        if (dungeon_info[tmp_row][tmp_col].feature_idx == FEAT_WALL_GRANITE_OUTER)
        {
            /* We can pierce 2 outer walls at the same time */
            int outer_x[2], outer_y[2], num_outer = 0;

            /* Get the "next" location */
            y = tmp_row + row_dir;
            x = tmp_col + col_dir;

            /* Hack -- Avoid outer/solid permanent walls */
            if (dungeon_info[y][x].feature_idx == FEAT_WALL_PERM_SOLID) continue;
            if (dungeon_info[y][x].feature_idx == FEAT_WALL_PERM_OUTER) continue;

            /* Hack -- Avoid solid granite walls */
            if (dungeon_info[y][x].feature_idx == FEAT_WALL_GRANITE_SOLID) continue;

            /* Check if we have only 2 consecutive outer walls */
            /* This is important to avoid disconnected rooms */
            if (dungeon_info[y][x].feature_idx == FEAT_WALL_GRANITE_OUTER)
            {
                /* Get the "next" location (again) */
                int yy = y + row_dir;
                int xx = x + col_dir;

                /* There is some other wall there, ignore */
                if (!cave_ff1_match(yy, xx, FF1_MOVE)) continue;

                /* Remember the first wall */
                outer_y[num_outer] = tmp_row;
                outer_x[num_outer] = tmp_col;
                ++num_outer;

                /* Hack -- Go to the second wall */
                tmp_row = y;
                tmp_col = x;
            }

            /* Remember the current wall */
            outer_y[num_outer] = tmp_row;
            outer_x[num_outer] = tmp_col;
            ++num_outer;

            /* Process the stored outer walls */
            for (i = 0; i < num_outer; i++)
            {
                /* Get the wall location */
                row1 = outer_y[i];
                col1 = outer_x[i];

                /* Save the wall location */
                if (dun->wall_n < WALL_MAX)
                {
                    dun->wall[dun->wall_n].y = row1;
                    dun->wall[dun->wall_n].x = col1;
                    dun->wall_n++;
                }

                /* Forbid re-entry near this piercing */
                for (y = row1 - 1; y <= row1 + 1; y++)
                {
                    for (x = col1 - 1; x <= col1 + 1; x++)
                    {
                        /* Convert adjacent "outer" walls as "solid" walls */
                        if (dungeon_info[y][x].feature_idx == FEAT_WALL_GRANITE_OUTER)
                        {
                            /* Change the wall to a "solid" wall */
                            cave_set_feat(y, x, FEAT_WALL_GRANITE_SOLID);
                        }
                    }
                }
            }
        }

        /* Feature can be bridged */
        else if (cave_ff2_match(tmp_row, tmp_col, FF2_BRIDGE))
        {
            /* Accept this location */
            row1 = tmp_row;
            col1 = tmp_col;

            /* Save the bridge location */
            if (dun->tunn_n < TUNN_MAX)
            {
                dun->tunn[dun->tunn_n].y = row1;
                dun->tunn[dun->tunn_n].x = col1;
                dun->tunn_n++;
            }
        }

        /* Feature can be tunneled. We ignore doors and inner walls */
        else if (cave_ff1_match(tmp_row, tmp_col,
            FF1_CAN_TUNNEL | FF1_DOOR | FF1_INNER) ==
                (FF1_CAN_TUNNEL))
        {
            /* Accept this location */
            row1 = tmp_row;
            col1 = tmp_col;

            /* Save the tunnel location */
            if (dun->tunn_n < TUNN_MAX)
            {
                dun->tunn[dun->tunn_n].y = row1;
                dun->tunn[dun->tunn_n].x = col1;
                dun->tunn_n++;
            }

            /* Allow door in next grid */
            door_flag = FALSE;
        }

        /* Travel quickly through rooms */
        else if (dungeon_info[tmp_row][tmp_col].cave_info & (CAVE_ROOM))
        {
            /* Accept the location */
            row1 = tmp_row;
            col1 = tmp_col;
        }

        /* Handle corridor intersections or overlaps */
        else
        {
            /* Accept the location */
            row1 = tmp_row;
            col1 = tmp_col;

            /* Collect legal door locations */
            if (!door_flag)
            {
                /* Save the door location */
                if (dun->door_n < DOOR_MAX)
                {
                    dun->door[dun->door_n].y = row1;
                    dun->door[dun->door_n].x = col1;
                    dun->door_n++;
                }

                /* No door in next grid */
                door_flag = TRUE;
            }

            /* Hack -- allow pre-emptive tunnel termination */
            if (rand_int(100) >= DUN_TUN_CON)
            {
                /* Distance between row1 and start_row */
                tmp_row = row1 - start_row;
                if (tmp_row < 0) tmp_row = (-tmp_row);

                /* Distance between col1 and start_col */
                tmp_col = col1 - start_col;
                if (tmp_col < 0) tmp_col = (-tmp_col);

                /* Terminate the tunnel */
                if ((tmp_row > 10) || (tmp_col > 10)) break;
            }
        }
    }

    /* Turn the tunnel into corridor */
    for (i = 0; i < dun->tunn_n; i++)
    {
        /* Get the grid */
        y = dun->tunn[i].y;
        x = dun->tunn[i].x;

        if (cave_ff2_match(y, x, FF2_BRIDGE))
        {
            /* Clear previous contents, bridge it */
            cave_alter_feat(y, x, FS_BRIDGE);
        }
        else
        {
            /* Clear previous contents, tunnel it */
            cave_alter_feat(y, x, FS_TUNNEL);
        }
    }

    /* Apply the piercings that we found */
    for (i = 0; i < dun->wall_n; i++)
    {
        /* Get the grid */
        y = dun->wall[i].y;
        x = dun->wall[i].x;

        /* Convert to floor grid */
        cave_set_feat(y, x, FEAT_FLOOR);

        /* Occasional doorway */
        if (rand_int(100) < DUN_TUN_PEN)
        {
            /* Check for unusual entry points to starburst/fractal rooms */
            if ((cave_wall_bold(y-1, x) && cave_wall_bold(y+1, x)) ||
                (cave_wall_bold(y, x-1) && cave_wall_bold(y, x+1)))
            {
                /* Place a random door */
                place_random_door(y, x);
            }
        }
    }
}


/*
 * Place the player outside all rooms and corridors. Build a tunnel to the
 * closest room.
 */
static bool new_player_spot_safe(void)
{
    int i;
    int y_location_tables[40];
    int x_location_tables[40];
    int rand_spot;
    int x, y;
    int close_room = 0, room_dist = 1000;
    int tries = 0;

    /* Find unused blocks */
    for (i = 0, y = 0; y < dun->row_rooms; y++)
    {
        for (x = 0; x < dun->col_rooms; x++)
        {
            /* Ignore used blocks */
            if (dun->room_map[y][x]) continue;

            /* We have space in the arrays */
            if (i < 40)
            {
                y_location_tables[i] = y;
                x_location_tables[i] = x;
                ++i;
            }
            /* Put it in a random place */
            else
            {
                rand_spot = rand_int(i);
                y_location_tables[rand_spot] = y;
                x_location_tables[rand_spot] = x;
            }
        }
    }

    /* We need at least one unused block */
    if (i == 0)
    {
        if (cheat_room)
        {
            message(QString("No unused block for the player."));
        }

        return (FALSE);
    }

    /* Find a safe spot */
    while (TRUE)
    {
        int d;

        feature_type *f_ptr;

        /* Hack - Avoid infinite loops */
        if (++tries >= 1000)
        {
            if (cheat_room)
            {
                message(QString("No safe spot for the player."));
            }

            return (FALSE);
        }

        /* Pick a random block */
        rand_spot = rand_int(i);

        /* Pick a random spot inside that block */
        y = y_location_tables[rand_spot] * BLOCK_HGT + rand_int(BLOCK_HGT);
        x = x_location_tables[rand_spot] * BLOCK_WID + rand_int(BLOCK_WID);

        /* Too close to a vault/room */
        if (dungeon_info[y][x].cave_info & (CAVE_ICKY | CAVE_ROOM)) continue;

        /* Get the feature */
        f_ptr = &f_info[dungeon_info[y][x].feature_idx];

        /*
         * The spot must be a wall. Note that we do not need to call
         * in_bounds_fully
         */
        if (!_feat_ff1_match(f_ptr, FF1_WALL)) continue;

        /* But we don't want certain walls */
        if (_feat_ff1_match(f_ptr, FF1_INNER | FF1_OUTER | FF1_SOLID |
            FF1_PERMANENT)) continue;

        /* The spot must be surrounded by walls */
        for (d = 0; d < 8; d++)
        {
            int yy = y + ddy_ddd[d];
            int xx = x + ddx_ddd[d];

            /* Too close to a vault/room */
            if (dungeon_info[yy][xx].cave_info & (CAVE_ICKY | CAVE_ROOM)) break;

            /* Get the feature */
            f_ptr = &f_info[dungeon_info[yy][xx].feature_idx];

            /* We need walls */
            if (!_feat_ff1_match(f_ptr, FF1_WALL)) break;

            /* We don't want certain walls around us */
            if (_feat_ff1_match(f_ptr, FF1_INNER | FF1_OUTER |
                FF1_SOLID)) break;
        }

        /* We found a non-wall grid */
        if (d < 8) continue;

        /* We found the safe spot */
        break;
    }

    /* Find the closest room */
    for (i = 0; i < dun->cent_n; i++)
    {
        int dist = distance(y, x, dun->cent[i].y, dun->cent[i].x);

        /* Ignore dangerous rooms */
        if (dungeon_info[dun->cent[i].y][dun->cent[i].x].cave_info & (CAVE_ICKY)) continue;

        /* We found a closer room */
        if (dist < room_dist)
        {
            room_dist = dist;
            close_room = i;
        }
    }

    /* Connect the safe spot to the closest room */
    build_tunnel(y, x, dun->cent[close_room].y, dun->cent[close_room].x);

    /* build_tunnel goes crazy sometimes */
    if (next_to_walls(y, x) < 3) return (FALSE);

    /* Hack - build tunnel ignores the safe spot */
    if (cave_ff1_match(y, x, FF1_CAN_TUNNEL))
    {
        cave_alter_feat(y, x, FS_TUNNEL);
    }
    else
    {
        cave_set_feat(y, x, FEAT_FLOOR);
    }

    /* Actually place the player */
    return player_place(y, x);
}


/*
 * Places some staircases near walls
 */
bool alloc_stairs(u16b feat, int num)
{
    int x;

    /* Limit the number of stairs */
    num = (*dun_cap->adjust_stairs_number)(num);

    /* Place "num" stairs */
    for (x = 0; x < num; x++)
    {
        int i = 0;

        int yy, xx;

        int x_location_tables [40];
        int y_location_tables [40];
        int rand_spot;

        /* Collect suitable grids */
        for (yy = 0; yy < p_ptr->cur_map_hgt; yy++)
        {
            for (xx = 0; xx < p_ptr->cur_map_wid; xx++)
            {
                /* Check allowance with dungeon capabilities */
                if (!(*dun_cap->can_place_stairs)(yy, xx)) continue;

                /* We like to be next to walls, but not other stairs */
                if (next_to_walls(yy, xx) < 3) continue;
                if (next_to_stairs(yy, xx) > 0) continue;

                /* Hack - not too many stairs in pillar rooms */
                if (dungeon_info[yy][xx].cave_info & (CAVE_ROOM))
                {
                    if (!(one_in_(10))) continue;
                }

                /* No stairs in greater vaults */
                if (dungeon_info[yy][xx].cave_info & (CAVE_G_VAULT))	continue;

                /*don't go over size of array*/
                if (i < 40)
                {
                    x_location_tables[i] = xx;
                    y_location_tables[i] = yy;

                    /*increase the counter*/
                    i++;
                }

                /*put it in a random slot*/
                else
                {
                    rand_spot = rand_int(40);

                    x_location_tables[rand_spot] = xx;
                    y_location_tables[rand_spot] = yy;
                }
            }
        }

        /*paranoia*/
        if (i == 0) return (FALSE);

        /*select a location*/
        rand_spot = rand_int (i);

        /*get the coordinates*/
        yy = y_location_tables[rand_spot];
        xx = x_location_tables[rand_spot];

        /* Town -- must go down */
        if (!p_ptr->depth)
        {
            /* Clear previous contents, add down stairs */
            cave_set_feat(yy, xx, FEAT_STAIRS_DOWN);
        }

        /* Quest -- must go up */
        else if ((no_down_stairs(p_ptr->depth)) ||
                 (p_ptr->depth >= MAX_DEPTH-1))
        {
            /* Clear previous contents, add up stairs */
            if (x == 0) cave_set_feat(yy, xx, FEAT_STAIRS_UP);
            else cave_set_feat(yy, xx, pick_up_stairs());
        }

        /* Requested type */
        else
        {
            /* Allow shafts, but guarantee the first one is an ordinary stair */
            if (x != 0)
            {
                if      (feat == FEAT_STAIRS_UP) feat = pick_up_stairs();
                else if (feat == FEAT_STAIRS_DOWN) feat = pick_down_stairs();
            }

            /* Clear previous contents, add stairs */
            cave_set_feat(yy, xx, feat);
        }
    }

    return (TRUE);
}


/*
 * Count the number of "corridor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y1, x1)"
 *
 * This routine currently only counts actual "empty floor" grids
 * which are not in rooms.  We might want to also count stairs,
 * open doors, closed doors, etc.  XXX XXX
 */
static int next_to_corr(int y1, int x1)
{
    int i, y, x, k = 0;

    /* Scan adjacent grids */
    for (i = 0; i < 4; i++)
    {
        /* Extract the location */
        y = y1 + ddy_ddd[i];
        x = x1 + ddx_ddd[i];

        /* Skip non floors */
        if (!cave_ff1_match(y, x, FF1_FLOOR)) continue;

        /* Skip grids inside rooms */
        if (dungeon_info[y][x].cave_info & (CAVE_ROOM)) continue;

        /* Count these grids */
        k++;
    }

    /* Return the number of corridors */
    return (k);
}


/*
 * Determine if the given location is "between" two walls,
 * and "next to" two corridor spaces.  XXX XXX XXX
 *
 * Assumes "in_bounds_fully(y,x)"
 */
static bool possible_doorway(int y, int x)
{
    /* Count the adjacent corridors */
    if (next_to_corr(y, x) >= 2)
    {
        /* Check Vertical */
        if (cave_wall_bold(y-1, x) && cave_wall_bold(y+1, x))
        {
            return (TRUE);
        }

        /* Check Horizontal */
        if (cave_wall_bold(y, x-1) && cave_wall_bold(y, x+1))
        {
            return (TRUE);
        }
    }

    /* No doorway */
    return (FALSE);
}


/*
 * Places door at y, x position if at least 2 walls found
 */
static void try_door(int y, int x)
{
    /* Paranoia */
    if (!in_bounds(y, x)) return;

    /* Ignore walls */
    if (cave_wall_bold(y, x)) return;

    /* Ignore room grids */
    if (dungeon_info[y][x].cave_info & (CAVE_ROOM)) return;

    /* Occasional door (if allowed) */
    if ((rand_int(100) < DUN_TUN_JCT) && possible_doorway(y, x))
    {
        /* Place a door */
        place_random_door(y, x);
    }
}


/*
 * Hack - mark some squares in the dungeon, that are counted when the player
 * walks over them.  This is a painful, awful hack used to occasionally allow the
 * stores sell things outside their pre-determined inventory without allowing
 * the player to townscum and pick up items in the stores.  Players need to
 * explore the dungeon in order to occasionally walk over the squares, increasing
 * the chance of stores selling other items. -JG
 */
static void place_marked_squares(void)
{
    int y, x, k, i;
    s32b j;

    /* No secret quuares in Moria */
    if (game_mode == GAME_NPPMORIA) return;

    /*
     * First factor in the number of "secret squares" based on size of dungeon
     * Start with 100 since we are dealing with int variables.
     * This will be reduced later
     */
    j = 0;


    /*factor in dungeon size*/
    j += ((100 * p_ptr->cur_map_hgt) / MAX_DUNGEON_HGT);
    j += ((100 * p_ptr->cur_map_wid) / MAX_DUNGEON_WID);

    /*divide by 10*/
    j /= 10;

    /* Mark some squares */
    for (k = 0; k < j; k++)
    {
        bool is_room;

        /*put half of them in corridors, half in rooms*/
        if (one_in_(2)) is_room = TRUE;
        else is_room = FALSE;

        /* Pick a "legal" spot */
        for (i = 0; i < 10000; i++)
        {

            /* Location */
            y = rand_int(p_ptr->cur_map_hgt);
            x = rand_int(p_ptr->cur_map_wid);

            /* Require "plain" floor grid */
            if (!cave_plain_bold(y, x)) continue;

            /* Already Marked */
            if (dungeon_info[y][x].cave_info & (CAVE_SPECIAL)) continue;

            /* Check if it should be a room or not "room" */
            if (((dungeon_info[y][x].cave_info & (CAVE_ROOM)) ? TRUE : FALSE) != is_room)
            {
                continue;
            }

            break;
        }

        /* No point found */
        if (i == 10000) return;

        /*mark it*/
        dungeon_info[y][x].cave_info |= (CAVE_SPECIAL);
    }

}


void basic_granite(void)
{
    int y, x;

    /* Hack -- Start with basic granite */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Create granite wall */
            cave_set_feat(y, x, FEAT_WALL_GRANITE);
        }
    }
}


bool place_traps_rubble_player(void)
{
    int k;

    /* Basic "amount" */
    k = (p_ptr->depth / 3);
    if (k > 10) k = 10;
    if (k < 2) k = 2;

    /* Put some rubble in corridors */
    if (dun->tunn_n > 0) alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

    /* Place some traps in the dungeon */
    alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));

    /* Determine the character location, if it is needed */
    if ((p_ptr->px + p_ptr->py == 0) && !new_player_spot_old()) return (FALSE);

    /*
     * Hack - place up to 10 "marked squares" in the dungeon if
     * player is close to max_depth
     */
    if ((p_ptr->depth + 3) > p_ptr->max_depth) place_marked_squares();

    return (TRUE);

}


bool scramble_and_connect_rooms_stairs(void)
{
    int i, y1, x1, y, x;

    /* Start with no tunnel doors */
    dun->door_n = 0;

    /* Hack -- Scramble the room order */
    for (i = 0; i < dun->cent_n; i++)
    {
        int pick1 = rand_int(dun->cent_n);
        int pick2 = rand_int(dun->cent_n);
        y1 = dun->cent[pick1].y;
        x1 = dun->cent[pick1].x;
        dun->cent[pick1].y = dun->cent[pick2].y;
        dun->cent[pick1].x = dun->cent[pick2].x;
        dun->cent[pick2].y = y1;
        dun->cent[pick2].x = x1;
    }

    /* Hack -- connect the first room to the last room */
    y = dun->cent[dun->cent_n-1].y;
    x = dun->cent[dun->cent_n-1].x;

    /* Connect all the rooms together */
    for (i = 0; i < dun->cent_n; i++)
    {
        /* Connect the room to the previous room */
        build_tunnel(dun->cent[i].y, dun->cent[i].x, y, x);

        /* Remember the "previous" room */
        y = dun->cent[i].y;
        x = dun->cent[i].x;
    }

    /* Attempt to place the player in a safe spot */
    /* Note that this code must be executed before placing intersection
     * doors */
    (void)new_player_spot_safe();

    /* Place intersection doors */
    for (i = 0; i < dun->door_n; i++)
    {
        /* Extract junction location */
        y = dun->door[i].y;
        x = dun->door[i].x;

        /* Try placing doors */
        try_door(y, x - 1);
        try_door(y, x + 1);
        try_door(y - 1, x);
        try_door(y + 1, x);
    }

    /* Place 3 or 5 down stairs near some walls */
    if (!alloc_stairs(FEAT_STAIRS_DOWN, (3 + randint0(3))))
    {
        if (cheat_room)
        {
            message(QString("failed to place down stairs"));
        }

        return (FALSE);
    }

    /* Place 1 or 3 up stairs near some walls */
    if (!alloc_stairs(FEAT_STAIRS_UP, (1 + randint0(3))))
    {
        if (cheat_room)
        {
            message(QString("failed to place up stairs"));
        }

        return (FALSE);
    }

    /* Hack -- Sandstone streamers are shallow */
    if (rand_int(DUN_STR_SLV) > p_ptr->depth)
    {

        /* Hack -- Add some magma streamers */
        for (i = 0; i < DUN_STR_MAG; i++)
        {
            /*if we can't build streamers, something is wrong with level*/
            if (!build_streamer(FEAT_MAGMA_VEIN, DUN_STR_MC)) return (FALSE);
        }
    }

    else
    {
        /* Hack -- Add some quartz streamers */
        for (i = 0; i < DUN_STR_QUA; i++)
        {
            /*if we can't build streamers, something is wrong with level*/
            if (!build_streamer(FEAT_QUARTZ_VEIN, DUN_STR_QC)) return (FALSE);
        }
    }

    /* Hack -- Add a rich mineral vein very rarely */
    if (one_in_(DUN_STR_GOL))
    {
        if (!build_streamer(FEAT_QUARTZ_VEIN, DUN_STR_GC)) return (FALSE);
    }

    return (TRUE);

}


void set_perm_boundry(void)
{
    int y, x;

    /* Special boundary walls -- Top */
    for (x = 0; x < p_ptr->cur_map_wid; x++)
    {
        y = 0;

        /* Clear previous contents, add "solid" perma-wall */
        cave_set_feat(y, x, FEAT_WALL_PERM_SOLID);
    }

    /* Special boundary walls -- Bottom */
    for (x = 0; x < p_ptr->cur_map_wid; x++)
    {
        y = p_ptr->cur_map_hgt-1;

        /* Clear previous contents, add "solid" perma-wall */
        cave_set_feat(y, x, FEAT_WALL_PERM_SOLID);
    }

    /* Special boundary walls -- Left */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        x = 0;

        /* Clear previous contents, add "solid" perma-wall */
        cave_set_feat(y, x, FEAT_WALL_PERM_SOLID);
    }

    /* Special boundary walls -- Right */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        x = p_ptr->cur_map_wid-1;

        /* Clear previous contents, add "solid" perma-wall */
        cave_set_feat(y, x, FEAT_WALL_PERM_SOLID);
    }
}

/*
 * Find a random location in the dungeon for a monster of the given race and store it
 * in py and px.
 * Return TRUE if succeeds, FALSE if fails.
 * Sometimes we try to place the monster in native terrain first.
 */
static bool pick_monster_location(monster_race *r_ptr, int *py, int *px)
{
    int max = 300;
    int cur = 0;
    coord *grids;
    bool found = FALSE;
    int tries, k;
    int x = 0;
    int y = 0;

    /* Get the LF1_* flags of the monster */
    u32b flag = get_level_flag_from_race(r_ptr);

    /* Search a native grid if necessary. Note that we always try if the monster is unique. */
    if (flag && (level_flag & flag) && ((r_ptr->flags1 & RF1_UNIQUE) || one_in_(2)))
    {
        /* Allocate storage for candidate grids */
        grids = C_ZNEW(max, coord);

        /* Scan the map */
        for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
        {
            for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
            {
                /* Found a suitable grid? */
                if (!cave_empty_bold(y, x) ||
                        !cave_ff3_match(y, x, TERRAIN_MASK) ||
                        !is_monster_native(y, x, r_ptr)) continue;

                /* Put it on the grid list */
                if (cur < max)
                {
                    k = cur++;
                }
                /* Overwrite the list if there isn't more space */
                else
                {
                    k = rand_int(max);
                }

                /* Save the location */
                grids[k].y = y;
                grids[k].x = x;
            }
        }

        /* Pick one of the candidates */
        if (cur > 0)
        {
            /* Pick a random index */
            k = rand_int(cur);
            /* Get the location */
            y = grids[k].y;
            x = grids[k].x;
            /* Remember the event */
            found = TRUE;
        }

        /* Free dynamic storage */
        FREE_ARRAY(grids);

        /* Found the grid? */
        if (found)
        {
            /* Debug message */
            if (cheat_room)
            {
                color_message(QString("Found a NATIVE location for \"%1\".") .arg(r_ptr->r_name_full), TERM_WHITE);
            }

            /* Return the location */
            *py = y;
            *px = x;
            return (TRUE);
        }
    }

    /* Find a random location */
    for (tries = 0; tries < 1000; tries++)
    {
        /* Get random coordinates */
        y = randint(p_ptr->cur_map_hgt - 2);
        x = randint(p_ptr->cur_map_wid - 2);

        /* Suitable grid? */
        if (cave_empty_bold(y, x) && cave_no_dam_for_mon(y, x, r_ptr))
        {
            /* Debug message */
            if (cheat_room)
            {
                color_message(QString("Found a random location for \"%1\".") .arg(r_ptr->r_name_full), TERM_WHITE);
            }

            /* Return the location */
            *py = y;
            *px = x;
            return (TRUE);
        }
    }

    /* Failure */
    return (FALSE);
}


/*
 * Place monsters and objects on a level
 * Returns TRUE on success
 */
bool place_monsters_objects(void)
{
    int i;
    /* Hack - variables for allocations */
    s16b mon_gen, obj_gen;

    /* Reset generation variables */
    mon_gen = (*dun_cap->get_monster_count)();

    /* To make small levels a bit more playable */
    if (p_ptr->cur_map_hgt < MAX_DUNGEON_HGT || p_ptr->cur_map_wid < MAX_DUNGEON_WID)
    {
        int small_tester = mon_gen;

        mon_gen = ((mon_gen * p_ptr->cur_map_hgt) / MAX_DUNGEON_HGT) + 1;
        mon_gen = ((mon_gen * p_ptr->cur_map_wid) / MAX_DUNGEON_WID) + 1;

        if (mon_gen > small_tester) mon_gen = small_tester;
        else if (cheat_hear)
        {
            message(QString("Reduced monsters base from %1 to %2") .arg(small_tester) .arg(mon_gen));
        }
    }

    /* Paranoia */
    if (mon_gen < 1) mon_gen = 1;

    mon_gen += randint(8);

    /* Put some monsters in the dungeon */
    for (i = mon_gen; i > 0; i--)
    {
        (void)alloc_monster(0, (MPLACE_SLEEP | MPLACE_GROUP));
    }

    /*
     * Ensure quest monsters in Moria.
     *
     * The purpose of this function is to, when at level 50 or below, to place either
     * vil Iggy or The Balrog of Moria.
     */
    if (game_mode == GAME_NPPMORIA)
    {
        if (p_ptr->depth >= MORIA_QUEST_DEPTH)
        {
            int mon_count = 0;
            int mon_choice;

            /* Count the special monsters */
            for (i = 1; i < z_info->r_max; i++)
            {
                monster_race *r_ptr = &r_info[i];

                if (!(r_ptr->flags2 & (RF2_SPECIAL))) continue;

                if (r_ptr->max_num == 0) continue;

                mon_count++;
            }

            /* Paranoia */
            if (mon_count)
            {
                int y, x;
                monster_race *r_ptr;

                mon_choice = randint1(mon_count);
                mon_count = 0;

                /* Find the special monster of choice */
                for (i = 1; i < z_info->r_max; i++)
                {
                    monster_race *r_ptr = &r_info[i];

                    if (!(r_ptr->flags2 & (RF2_SPECIAL))) continue;
                    if (r_ptr->max_num == 0) continue;

                    mon_count++;

                    /* Found it */
                    if (mon_choice == mon_count) break;
                }

                r_ptr = &r_info[i];

                /* No big deal if it fails. We just place it one the next time a level is generated. */
                if (pick_monster_location(r_ptr, &y, &x))
                {
                    place_monster_aux(y, x, i, MPLACE_OVERRIDE);
                }
            }
        }
    }

    /* Ensure quest monsters Angband */
    else for (i = 0; i < z_info->q_max; i++)
    {
        quest_type *q_ptr = &q_info[i];

        /* Quest levels */
        if ((q_ptr->base_level == p_ptr->depth) && !is_quest_complete(i))
        {
            monster_race *r_ptr = &r_info[q_ptr->mon_idx];
            int y, x;

            if ((quest_fixed(q_ptr)) || (quest_single_r_idx(q_ptr)))
            {
                int j;

                /* A certain number of questors */
                s16b num_questors = q_ptr->q_max_num - q_ptr->q_num_killed;

                /* Ensure quest monsters */
                while (r_ptr->cur_num < num_questors)
                {
                    /* Pick a location */
                    if (!pick_monster_location(r_ptr, &y, &x)) return FALSE;

                    /* Place the questor */
                    place_monster_aux(y, x, q_ptr->mon_idx, (MPLACE_SLEEP | MPLACE_GROUP));
                }

                /* Process the monsters (backwards) */
                for (j = mon_max - 1; j >= 1; j--)
                {
                    /* Access the monster */
                    monster_type *m_ptr = &mon_list[j];

                    /*mark it as a quest monster if applicable*/
                    if (q_ptr->mon_idx == m_ptr->r_idx) m_ptr->mflag |= (MFLAG_QUEST);
                }
            }
        }
    }

    /* Put some objects in rooms */
    obj_gen = (*dun_cap->get_object_count)();
    if (obj_gen > 0) alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, obj_gen);

    /* Put some objects/gold in the dungeon */
    obj_gen = (*dun_cap->get_extra_object_count)();
    if (obj_gen > 0) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, obj_gen);

    obj_gen = (*dun_cap->get_gold_count)();
    if (obj_gen > 0) alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, obj_gen);

    return (TRUE);
}


/*
 * Generate an unthemed new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(void)
{
    int i, k;

    int by, bx;

    bool destroyed = FALSE;
    bool need_quest_room = FALSE;
    bool greater_vault = FALSE;
    bool fractal_level = FALSE;

    /*Hack - get the quest type*/
    byte quest_on_level = quest_check(p_ptr->depth);

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    dun_data dun_body;

    /* Global data */
    dun = &dun_body;

    /* Set level type */
    set_dungeon_type(DUNGEON_TYPE_DEFAULT);

    /* Possible "destroyed" level, but not in Moria  */
    if (game_mode == GAME_NPPMORIA) destroyed = FALSE;
    else if ((p_ptr->depth > 10) && (one_in_(DUN_DEST))) destroyed = TRUE;

    /* Possible "fractal" level */
    if (!destroyed && (p_ptr->depth >= 15) && one_in_(DUN_FRACTAL) && (!birth_classic_dungeons)) fractal_level = TRUE;

    /*Clear the level flag*/
    level_flag = 0;

    /*
     * If we have visited the quest level, and found the
     * artifact, don't re-create the vault.
     */
    if (quest_on_level == QUEST_VAULT)
    {
        /*
         * We already started the quest. Perhaps we have the object but
         * we haven't gone back to the guild yet
         */
        if (q_ptr->q_flags & (QFLAG_STARTED)) quest_on_level = 0;
    }

    /*see if we need a quest room*/
    if (quest_on_level)
    {
        /* Hack -- No destroyed "quest" levels */
        destroyed = FALSE;

        switch (q_ptr->q_type)
        {
            case QUEST_VAULT:
            case QUEST_PIT:
            case QUEST_NEST:
            {
                need_quest_room = TRUE;
                break;
            }
            default: break;
        }
    }

    if (((birth_force_small_lev) || (one_in_(SMALL_LEVEL)) ||
        (quest_on_level == QUEST_VAULT)) && (game_mode != GAME_NPPMORIA))
    {
        int l, m;

        while (TRUE)
        {
            /*
             * Note: Panel height and width is 1/6 of max
             * height/width
             */
            l = randint(MAX_DUNGEON_HGT / (PANEL_HGT));
            m = randint(MAX_DUNGEON_WID / (PANEL_WID));

            /* Make 2 panels the minimum size */
            if (l < 2) l = 2;
            if (m < 2) m = 2;

            /* Not too small for quest levels */
            if (quest_on_level)
            {
                if (l < 4) l = 4;
                if (m < 4) m = 4;
            }

            /*
             * Make the dungeon height & width a multiple
             * of 2 to 6 of panel hgt & width
             */
            p_ptr->cur_map_hgt = l * (PANEL_HGT);
            p_ptr->cur_map_wid = m * (PANEL_WID);

            /* Exit if less than normal dungeon */
            if ((p_ptr->cur_map_hgt < MAX_DUNGEON_HGT) ||
                (p_ptr->cur_map_wid < MAX_DUNGEON_WID)) break;
        }

        if ((cheat_room) && (!birth_force_small_lev))
        {
            message(QString("A 'small' dungeon level (%1x%2).") .arg(m) .arg(l));
        }

        if (!birth_force_small_lev) rating += ((m + l <= 3) ? 15 : 10);
    }
    else
    {
    /* Full-sized dungeon */
        p_ptr->cur_map_hgt = MAX_DUNGEON_HGT;
        p_ptr->cur_map_wid = MAX_DUNGEON_WID;
    }

    /*start with basic granite*/
    basic_granite();

    /* Actual maximum number of rooms on this level */
    dun->row_rooms = p_ptr->cur_map_hgt / BLOCK_HGT;
    dun->col_rooms = p_ptr->cur_map_wid / BLOCK_WID;

    /* Initialize the room table */
    for (by = 0; by < dun->row_rooms; by++)
    {
        for (bx = 0; bx < dun->col_rooms; bx++)
        {
            dun->room_map[by][bx] = FALSE;
        }
    }

    /* No "crowded" rooms yet */
    dun->crowded = FALSE;

    /* No rooms yet */
    dun->cent_n = 0;

    /* No features on destroyed level, or in Moria */
    if ((!destroyed) && one_in_(2) && (game_mode != GAME_NPPMORIA) && randint0(100) < p_ptr->depth)
    {
        /* Build lakes and rivers */
        build_nature();
    }

    /* Build some rooms */
    for (i = 0; i < DUN_ROOMS; i++)
    {
        /* Pick a block for the room */
        by = rand_int(dun->row_rooms);
        bx = rand_int(dun->col_rooms);

        if (game_mode == GAME_NPPMORIA)
        {
            if (rand_int(DUN_UNUSUAL_MORIA) < p_ptr->depth)
            {
                k = randint(3);

                if (k == 1)
                {
                    if (room_build(by, bx, 2)) continue;
                }
                else if (k == 2)
                {
                    if (room_build(by, bx, 3)) continue;
                }
                else if (room_build(by, bx, 4)) continue;

            }

            if (room_build(by, bx, 1)) continue;
        }

        /* Destroyed levels are boring */
        if (destroyed)
        {
            /* Attempt a "trivial" room */
            if (room_build(by, bx, 1)) continue;

            /* Never mind */
            continue;
        }
        /*first make a necessary quest room*/
        if (need_quest_room)
        {
            if (quest_on_level == QUEST_VAULT)
            {
                if (room_build(by, bx, 9)) need_quest_room = FALSE;

                /* Ensure the quest artifact was generated properly */
                if (a_info[QUEST_ART_SLOT].a_cur_num != 1)
                {
                    if (cheat_room)
                    {
                        message(QString("quest artifact not generated properly"));
                    }
                    return (FALSE);
                }

                continue;
            }
            else if (quest_on_level == QUEST_PIT)
            {
                if (room_build(by, bx, 6)) need_quest_room = FALSE;
                continue;
            }
            else if (quest_on_level == QUEST_NEST)
            {
                if (room_build(by, bx, 5)) need_quest_room = FALSE;
                continue;
            }
        }

        /* Fractal rooms in fractal levels */
        if (fractal_level)
        {
            int room_idx = (one_in_(4) ? 14: one_in_(2) ? 13: 12);

            if (room_build(by, bx, room_idx)) continue;

            /* We want mostly fractal rooms */
            if (rand_int(100) < 90) continue;
        }



        /* Attempt an "unusual" room */
        else if (rand_int(DUN_UNUSUAL) < p_ptr->depth )
        {

            /* Roll for room type */
            k = rand_int(100);

            /* Attempt a very unusual room */
            if (rand_int(DUN_UNUSUAL) < p_ptr->depth)
            {
                /* Type 8 -- Greater vault (10%) */
                if ((k < 10) && !greater_vault && room_build(by, bx, 8))
                {
                    greater_vault = TRUE;
                    continue;
                }

                /* Type 7 -- Lesser vault (15%) */
                if ((k < 25) && room_build(by, bx, 7)) continue;

                /* Type 6 -- Monster pit (15%) */
                if ((k < 40) && room_build(by, bx, 6)) continue;

                /* Type 5 -- Monster nest (10%) */
                if ((k < 50) && room_build(by, bx, 5)) continue;
            }

            /* Type 4 -- Large room (25%) */
            if ((k < 25) && room_build(by, bx, 4)) continue;

            /* Type 3 -- Cross room (25%) */
            if ((k < 50) && room_build(by, bx, 3)) continue;

            /* Type 2 -- Overlapping (50%) */
            if ((k < 100) && room_build(by, bx, 2)) continue;
        }

        /* Occasionally attempt a starburst room */
        /* Maximum chance: one in 20 */
        if ((randint(800) <= MIN(p_ptr->depth, 40)) && (!birth_classic_dungeons))
        {
            int room_idx = (one_in_(10) ? 11 : 10);

            if (room_build(by, bx, room_idx)) continue;
        }

        /* Occasionally attempt a fractal room */
        /* Maximum chance: one in 7 */
        if ((randint(490) <= MIN(p_ptr->depth, 70)) && (!birth_classic_dungeons))
        {
            if (one_in_(8) && room_build(by, bx, 14)) continue;

            if (!one_in_(3) && room_build(by, bx, 13)) continue;

            if (room_build(by, bx, 12)) continue;
        }

        /* Attempt a trivial room */
        if (room_build(by, bx, 1)) continue;
    }

    /* Set the permanent walls */
    set_perm_boundry();

    /*start over on all levels with less than two rooms due to inevitable crash*/
    if (dun->cent_n < ROOM_MIN)
    {
        if (cheat_room)
        {
            message(QString("not enough rooms"));
        }
        return (FALSE);
    }

    /*make the tunnels*/
    if (!scramble_and_connect_rooms_stairs())
    {
        if (cheat_room)
        {
            message(QString("couldn't connect the rooms"));
        }
        return (FALSE);
    }

    /*don't destroy really small levels*/
    if (dun->cent_n < 10) destroyed = FALSE;

    /* Destroy the level if necessary */
    if (destroyed) destroy_level();

    /*place the stairs, traps, rubble, player, secret stairs*/
    if (!place_traps_rubble_player())
    {
        if (cheat_room)
        {
            message(QString("couldn't place traps, rubble, or player"));
        }
        return (FALSE);
    }

    /* Place flavor features (fog, regions, walls, etc.) */
    build_misc_features();

    /* Place monsters and objects */
    if (!place_monsters_objects()) return (FALSE);

    return (TRUE);

}


/*
 * Builds a store at a given pseudo-location
 *
 * As of 2.7.4 (?) the stores are placed in a more "user friendly"
 * configuration, such that the four "center" buildings always
 * have at least four grids between them, to allow easy running,
 * and the store doors tend to face the middle of town.
 *
 * The stores now lie inside boxes from 3-9 and 12-18 vertically,
 * and from 7-17, 21-31, 35-45, 49-59.  Note that there are thus
 * always at least 2 open grids between any disconnected walls.
 *
 * Note the use of "town_illuminate()" to handle all "illumination"
 * and "memorization" issues.
 */
static void build_store(u16b feat, int yy, int xx)
{
    int y, x, y0, x0, y1, x1, y2, x2, tmp;

    if (game_mode == GAME_NPPMORIA)
    {
        /* Find the "center" of the store */
        y0 = yy * 10 + 5;
        x0 = xx * 16 + 16;

        /* Determine the store boundaries */
        y1 = y0 - randint1(3);
        y2 = y0 + randint1(4);
        x1 = x0 - randint1(5);
        x2 = x0 + randint1(6);
    }
    else
    {
        /* Find the "center" of the store */
        y0 = yy * 9 + 6;
        x0 = xx * 11 + 11;

        /* Determine the store boundaries */
        y1 = y0 - randint((yy == 0) ? 2 : 1) - 1;
        y2 = y0 + randint((yy == 1) ? 2 : 1) + 1;
        x1 = x0 - randint(2) - 1;
        x2 = x0 + randint(2) + 1;
    }

    /* Build an invulnerable rectangular building */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Create the building */
            cave_set_feat(y, x, FEAT_WALL_PERM);
        }
    }

    /* Pick a door direction (S,N,E,W) */
    tmp = rand_int(4);

    /* Re-roll "annoying" doors */
    if (((tmp == 0) && (yy == 1)) ||
        ((tmp == 1) && (yy == 0)) ||
        ((tmp == 2) && (xx == 3)) ||
        ((tmp == 3) && (xx == 0)))
    {
        /* Pick a new direction */
        tmp = rand_int(4);
    }

    /* Extract a "door location" */
    switch (tmp)
    {
        /* Bottom side */
        case 0:
        {
            y = y2;
            x = rand_range(x1, x2);
            break;
        }

        /* Top side */
        case 1:
        {
            y = y1;
            x = rand_range(x1, x2);
            break;
        }

        /* Right side */
        case 2:
        {
            y = rand_range(y1, y2);
            x = x2;
            break;
        }

        /* Left side */
        default:
        {
            y = rand_range(y1, y2);
            x = x1;
            break;
        }
    }

    /* Clear previous contents, add a store door (add base store number) */
    cave_set_feat(y, x, feat);
}


/*
 * Generate the "consistent" town features, and place the player
 *
 * Hack -- play with the R.N.G. to always yield the same town
 * layout, including the size and shape of the buildings, the
 * locations of the doorways, and the location of the stairs.
 */
static void town_gen_hack(void)
{
    int y, x, k, n;

    int border = (PANEL_HGT / 2 + 1);

    u16b i;

    u16b rooms[MAX_STORES];

    u16b max_stores = MAX_STORES;

    if (game_mode == GAME_NPPMORIA) max_stores = 6;

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Hack -- Induce consistant town layout */
    Rand_value = seed_town;

    /* Prepare an Array of "remaining stores" */
    for (n = 0; n < MAX_STORES; n++) rooms[n] = 0;

    /* hack - make sure the right number of stores is generated in the moria town */
    if (game_mode == GAME_NPPMORIA) n = 6;

    /* Scan the table */
    for (i = 0; i < z_info->f_max; i++)
    {
        /* Get the feature */
        feature_type *f_ptr = &f_info[i];

        /*We are looking for the shops*/
        if (!f_ptr->is_store()) continue;

        /*paranoia*/
        if (f_ptr->f_power >= max_stores) continue;

        /*We found a shop*/
        rooms[f_ptr->f_power] = i;
    }

    /* Place two rows of stores */
    for (y = 0; y < 2; y++)
    {
        int stores_row = max_stores / 2;

        /* Place five stores per row, but leave one space for stairs */
        for (x = 0; x < stores_row; x++)
        {
            /* Pick a random unplaced store */
            k = ((n <= 1) ? 0 : rand_int(n));

            /* Build that store at the proper location */
            build_store(rooms[k], y, x);

            /* Shift the stores down, remove one store */
            rooms[k] = rooms[--n];
        }
    }

    /* Place the stairs */
    while (TRUE)
    {
        bool found_spot = TRUE;

        /* Find the "center" of the empty store space, in the empty gap */
        int y1 = rand_range(border, (p_ptr->cur_map_hgt - border));
        int x1 = rand_range(border, (p_ptr->cur_map_wid - border));

        /* Require a "naked" floor grid */
        if (!cave_naked_bold(y1, x1)) found_spot = FALSE;

        /*Require it to be surrounded by empty squares, so it is not right up against a store*/
        else for (i = 0; i < 8; i++)
        {
            y = y1 + ddy_ddd[i];
            x = x1 + ddx_ddd[i];

            if (!cave_naked_bold(y, x))
            {
                /*No need to look any further, start over*/
                found_spot = FALSE;
                break;
            }
        }

        /*We have a spot*/
        if (found_spot)
        {
            y = y1;
            x = x1;
            break;
        }
    }

    /* Clear previous contents, add down stairs */
    cave_set_feat(y, x, FEAT_STAIRS_DOWN);

    /* Place the player */
    player_place(y, x);

    /* Hack -- use the "complex" RNG */
    Rand_quick = FALSE;
}


/*
 * Town logic flow for generation of new town
 *
 * We start with a fully wiped cave of normal floors.
 *
 * Note that town_gen_hack() plays games with the R.N.G.
 *
 * This function does NOT do anything about the owners of the stores,
 * nor the contents thereof.  It only handles the physical layout.
 *
 * We place the player on the stairs at the same time we make them.
 *
 * Hack -- since the player always leaves the dungeon by the stairs,
 * he is always placed on the stairs, even if he left the dungeon via
 * word of recall or teleport level.
 */
static void town_gen(void)
{
    int i, y, x;
    int residents;
    bool daytime;

    /* Set level type */
    set_dungeon_type(DUNGEON_TYPE_TOWN);

    /* Restrict to single-screen size */
    p_ptr->cur_map_hgt = (2 * PANEL_HGT);
    p_ptr->cur_map_wid = (2 * PANEL_WID);

    /* Day time */
    if ((p_ptr->game_turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
    {
        /* Day time */
        daytime = TRUE;

        /* Number of residents */
        residents = MIN_M_ALLOC_TD;
    }

    /* Night time */
    else
    {
        /* Night time */
        daytime = FALSE;

        /* Number of residents */
        residents = MIN_M_ALLOC_TN;
    }

    /* Start with solid walls */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Create "solid" perma-wall */
            cave_set_feat(y, x, FEAT_WALL_PERM_SOLID);
        }
    }

    /* Then place some floors */
    for (y = 1; y < p_ptr->cur_map_hgt-1; y++)
    {
        for (x = 1; x < p_ptr->cur_map_wid-1; x++)
        {
            /* Create empty floor */
            cave_set_feat(y, x, FEAT_COBBLESTONE_FLOOR);
        }
    }

    /* Build stuff */
    town_gen_hack();

    /* Apply illumination */
    town_illuminate(daytime);

    /* Make some residents */
    for (i = 0; i < residents; i++)
    {
        /* Make a resident */
        (void)alloc_monster(3, (MPLACE_SLEEP | MPLACE_GROUP));
    }
}


/*
 * Select and return one of the DUNGEON_TYPE_* constants
 * The selection is restricted by a number of things like depth and quests.
 */
static int pick_dungeon_type(void)
{
    /* Town */
    if (p_ptr->depth == 0)
    {
        return DUNGEON_TYPE_TOWN;
    }

    /* Classic moria level */
    if (game_mode == GAME_NPPMORIA)	return DUNGEON_TYPE_DEFAULT;

    /* Themed level quest */
    if (quest_check(p_ptr->depth) == QUEST_THEMED_LEVEL)
    {
        return DUNGEON_TYPE_THEMED_LEVEL;
    }

    /* Themed level quest */
    if (quest_check(p_ptr->depth) == QUEST_ARENA_LEVEL)
    {
        return DUNGEON_TYPE_ARENA;
    }

    /* Themed level quest */
    if (quest_check(p_ptr->depth) == QUEST_WILDERNESS)
    {
        return DUNGEON_TYPE_WILDERNESS;
    }
    /* Labyrinth level quest */
    if (quest_check(p_ptr->depth) == QUEST_LABYRINTH)
    {
        return DUNGEON_TYPE_LABYRINTH;
    }
    if (quest_check(p_ptr->depth) == QUEST_GREATER_VAULT)
    {
        return DUNGEON_TYPE_GREATER_VAULT;
    }

    if (!quest_check(p_ptr->depth))
    {
        /* Random themed level */
        if (allow_themed_levels && (p_ptr->depth >= 10) && (!birth_classic_dungeons) &&
            one_in_(THEMED_LEVEL_CHANCE))
        {
            return DUNGEON_TYPE_THEMED_LEVEL;
        }

        /* Random wilderness level */
        if ((p_ptr->depth > 10) && (!birth_classic_dungeons) &&
            one_in_(WILDERNESS_LEVEL_CHANCE))
        {
            return DUNGEON_TYPE_WILDERNESS;
        }

        /* Random labyrinth level */
        if ((p_ptr->depth > 15) && one_in_(LABYRINTH_LEVEL_CHANCE))
        {
            return DUNGEON_TYPE_LABYRINTH;
        }

        /* Random labyrinth level */
        if ((p_ptr->depth > 30) && one_in_(GREATER_VAULT_LEVEL_CHANCE))
        {
            return DUNGEON_TYPE_GREATER_VAULT;
        }
    }

    /* Classic level */
    return DUNGEON_TYPE_DEFAULT;
}


/*
 * Generate a random dungeon level
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Hack -- allow auto-scumming via a gameplay option.
 *
 * Note that this function resets "cave_feat" and "cave_info" directly.
 */
void generate_cave(void)
{
    int num;
    int dungeon_type = DUNGEON_TYPE_DEFAULT;
    /* The time to live of a dungeon type */
    /* This is used to prevent problems with different failure chances in level generation */
    int dungeon_type_ttl = 0;

    /* The dungeon is not ready */
    character_dungeon = FALSE;

    /* Don't know feeling yet */
    do_feeling = FALSE;

    /*allow uniques to be generated everywhere but in nests/pits*/
    allow_uniques = TRUE;

    /* Generate num is increased below*/
    for (num = 0; TRUE;)
    {
        bool okay = TRUE;
        QString why;
        why.clear();

        /* Reset */
        o_max = 1;
        mon_max = 1;
        x_max = 1;
        feeling = 0;

        o_cnt = 0;
        x_cnt = 0;
        mon_cnt = 0;

        /* Remove all dynamic features */
        dyna_grids.clear();

        /* Paranoia. Clear the current elemental flags */
        level_flag = 0;

        // Wipe the whole dungeon
        reset_dungeon_info();

        /* Mega-Hack -- no player yet */
        p_ptr->px = p_ptr->py = 0;

        /* Reset the monster generation level */
        monster_level = p_ptr->depth;

        /* Reset the object generation level */
        object_level = p_ptr->depth;

        /* Nothing special here yet */
        good_item_flag = FALSE;

        /* Nothing good here yet */
        rating = 0;

        /* The current dungeon type is still valid */
        if (dungeon_type_ttl > 0)
        {
            --dungeon_type_ttl;
        }
        /* We must reset the dungeon type */
        else
        {
            /* Select a new level type */
            dungeon_type = pick_dungeon_type();

            /* Set the time to live */
            dungeon_type_ttl = 200;
        }

        switch (dungeon_type)
        {
            case DUNGEON_TYPE_TOWN:
            {
                /* Make a town */
                town_gen();

                /* Hack -- Clear stairs request */
                p_ptr->create_stair = 0;

                /* Always okay */
                okay = TRUE;

                break;
            }
            case DUNGEON_TYPE_THEMED_LEVEL:
            {
                /* Make a themed level */
                okay = build_themed_level();

                break;
            }
            case DUNGEON_TYPE_WILDERNESS:
            {
                /* Make a wilderness level */
                okay = build_wilderness_level();

                break;
            }
            case DUNGEON_TYPE_ARENA:
            {
                /* Make a wilderness level */
                okay = build_arena_level();

                break;
            }
            case DUNGEON_TYPE_LABYRINTH:
            {
                /* Make a wilderness level */
                okay = build_labyrinth_level();

                break;
            }
            case DUNGEON_TYPE_GREATER_VAULT:
            {
                /* Make a wilderness level */
                okay = build_greater_vault_level();

                break;
            }
            default:
            {
                /* Make a classic level */
                okay = cave_gen();

                break;
            }
        }

        /*message*/
        if(!okay)
        {
            if (cheat_room || cheat_hear || cheat_peek || cheat_xtra)
            {
                why = "defective level";
            }
        }
        else
        {

            /*themed levels already have thier feeling*/
            /* Extract the feeling */
            if (!feeling)
            {
                if (rating > 100) feeling = 2;
                else if (rating > 80) feeling = 3;
                else if (rating > 60) feeling = 4;
                else if (rating > 40) feeling = 5;
                else if (rating > 30) feeling = 6;
                else if (rating > 20) feeling = 7;
                else if (rating > 10) feeling = 8;
                else if (rating > 0) feeling = 9;
                else feeling = 10;

                /* Hack -- Have a special feeling sometimes */
                if (good_item_flag && !birth_preserve) feeling = 1;

                /* Hack -- no feeling in the town */
                if (!p_ptr->depth) feeling = 0;
            }

            /* Prevent object over-flow */
            if (o_max >= z_info->o_max)
            {
                /* Message */
                why = "too many objects";

                /* Message */
                okay = FALSE;
            }

            /* Prevent monster over-flow */
            if (mon_max >= z_info->m_max)
            {
                /* Message */
                why = "too many monsters";

                /* Message */
                okay = FALSE;
            }

            /* Mega-Hack -- "auto-scum" */
            if (auto_scum && (num < 100))
            {
                /*count this level*/
                num++;

                /* Require "goodness", but always accept themed levels */
                if ((feeling < LEV_THEME_HEAD) &&
                    ((feeling > 9) ||
                     ((p_ptr->depth >= 5) && (feeling > 8)) ||
                     ((p_ptr->depth >= 10) && (feeling > 7)) ||
                     ((p_ptr->depth >= 20) && (feeling > 6)) ||
                     ((p_ptr->depth >= 40) && (feeling > 5))))
                {
                    /* Give message to cheaters */
                    if (cheat_room || cheat_hear || cheat_peek || cheat_xtra)
                    {
                        /* Message */
                        why = "boring level";
                    }

                    /* Try again */
                    okay = FALSE;
                }
            }
        }

        /* Accept */
        if (okay) break;

        /* Message */
        if (!why.isEmpty()) message(QString("Generation restarted (%1)") .arg(why));

        /* Wipe the objects */
        wipe_o_list();

        /* Wipe the monsters */
        wipe_mon_list();

        /* Wipe the monsters */
        wipe_x_list();
    }

    /* The dungeon is ready */
    character_dungeon = TRUE;

    /* Reset the number of traps on the level. */
    num_trap_on_level = 0;

    /* Clear the summon mask */
    dungeon_summon_mask_f7 = 0L;
}

