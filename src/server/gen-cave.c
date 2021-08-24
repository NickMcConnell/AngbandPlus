/*
 * File: gen-cave.c
 * Purpose: Generation of dungeon levels
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2013 Erik Osheim, Nick McConnell
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
#include <math.h>


/*
 * In this file, we use the SQUARE_WALL flags to the info field in
 * cave->squares, which should only be applied to granite. SQUARE_WALL_SOLID
 * indicates the wall should not be tunnelled; SQUARE_WALL_INNER is the
 * inward-facing wall of a room; SQUARE_WALL_OUTER is the outer wall of a room.
 *
 * We use SQUARE_WALL_SOLID to prevent multiple corridors from piercing a wall
 * in two adjacent locations, which would be messy, and SQUARE_WALL_OUTER
 * to indicate which walls surround rooms, and may thus be pierced by corridors
 * entering or leaving the room.
 *
 * Note that a tunnel which attempts to leave a room near the edge of the
 * dungeon in a direction toward that edge will cause "silly" wall piercings,
 * but will have no permanently incorrect effects, as long as the tunnel can
 * eventually exit from another side. And note that the wall may not come back
 * into the room by the hole it left through, so it must bend to the left or
 * right and then optionally re-enter the room (at least 2 grids away). This is
 * not a problem since every room that is large enough to block the passage of
 * tunnels is also large enough to allow the tunnel to pierce the room itself
 * several times.
 *
 * Note that no two corridors may enter a room through adjacent grids, they
 * must either share an entryway or else use entryways at least two grids
 * apart. This prevents large (or "silly") doorways.
 *
 * Traditionally, to create rooms in the dungeon, it was divided up into
 * "blocks" of 11x11 grids each, and all rooms were required to occupy a
 * rectangular group of blocks. As long as each room type reserved a
 * sufficient number of blocks, the room building routines would not need to
 * check bounds. Note that in classic generation most of the normal rooms
 * actually only use 23x11 grids, and so reserve 33x11 grids.
 *
 * Note that a lot of the original motivation for the block system was the
 * fact that there was only one size of map available, 22x66 grids, and the
 * dungeon level was divided up into nine of these in three rows of three.
 * Now that the map can be resized and enlarged, and dungeon levels themselves
 * can be different sizes, much of this original motivation has gone. Blocks
 * can still be used, but different cave profiles can set their own block
 * sizes. The classic generation method still uses the traditional blocks; the
 * main motivation for using blocks now is for the aesthetic effect of placing
 * rooms on a grid.
 */


/*
 * Check whether a square has one of the tunnelling helper flags
 *
 * c is the current chunk
 * (y, x) are the co-ordinates
 * flag is the relevant flag
 */
static bool square_is_granite_with_flag(struct chunk *c, int y, int x, int flag)
{
    if (c->squares[y][x].feat != FEAT_GRANITE) return false;
    if (!sqinfo_has(c->squares[y][x].info, flag)) return false;

    return true;
}


/*
 * Places a streamer of rock through dungeon.
 *
 * c is the current chunk
 * feat is the base feature (FEAT_MAGMA or FEAT_QUARTZ)
 * chance is the number of regular features per one gold
 *
 * Note that their are actually six different terrain features used to
 * represent streamers. Three each of magma and quartz, one for basic vein, one
 * with hidden gold, and one with known gold. The hidden gold types are
 * currently unused.
 */
static void build_streamer(struct chunk *c, int feat, int chance)
{
    int i, tx, ty;
    int y, x, dir;

    /* Hack -- choose starting point */
    y = rand_spread(c->height / 2, 10);
    x = rand_spread(c->width / 2, 15);

    /* Choose a random direction */
    dir = ddd[randint0(8)];

    /* Place streamer into dungeon */
    while (true)
    {
        /* One grid per density */
        for (i = 0; i < dun->profile->str.den; i++)
        {
            int d = dun->profile->str.rng;

            /* Pick a nearby grid */
            find_nearby_grid(c, &ty, y, d, &tx, x, d);

            /* Only convert walls */
            if (square_isrock(c, ty, tx))
            {
                /* Turn the rock into the vein type */
                square_set_feat(c, ty, tx, feat);

                /* Sometimes add known treasure */
                if (one_in_(chance)) square_upgrade_mineral(c, ty, tx);
            }
        }

        /* Advance the streamer */
        y += ddy[dir];
        x += ddx[dir];

        /* Stop at dungeon edge */
        if (!square_in_bounds(c, y, x)) break;
    }
}


static bool square_isperm_outer(struct chunk *c, int y, int x)
{
    return (square_isperm(c, y, x) && !square_iswall_inner(c, y, x));
}


static bool pierce_outer_locate(struct chunk *c, int tmp_row, int tmp_col, int row_dir, int col_dir,
    int *prow, int *pcol)
{
    int y, x;

    /* Get the "next" location */
    y = tmp_row + row_dir;
    x = tmp_col + col_dir;

    /* Stay in bounds */
    if (!square_in_bounds(c, y, x)) return false;

    /* Hack -- avoid solid permanent walls */
    if (square_isperm_outer(c, y, x)) return false;

    /* Hack -- avoid outer/solid granite walls */
    if (square_is_granite_with_flag(c, y, x, SQUARE_WALL_OUTER)) return false;
    if (square_is_granite_with_flag(c, y, x, SQUARE_WALL_SOLID)) return false;

    /* Accept this location */
    if (prow) *prow = tmp_row;
    if (pcol) *pcol = tmp_col;
    return true;
}


static void pierce_outer_save(struct chunk *c, int row1, int col1)
{
    int y, x;

    /* Save the wall location */
    if (dun->wall_n < z_info->wall_pierce_max)
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
            /* Be sure we are "in bounds" */
            if (!square_in_bounds_fully(c, y, x)) continue;

            /* Convert adjacent "outer" walls as "solid" walls */
            if (square_is_granite_with_flag(c, y, x, SQUARE_WALL_OUTER))
                set_marked_granite(c, y, x, SQUARE_WALL_SOLID);
        }
    }
}


static bool pierce_outer_wide(struct chunk *c, int row1, int col1, int row_dir, int col_dir, int sign)
{
    int y, x;

    /* Get an adjacent location */
    y = row1 + sign * col_dir;
    x = col1 + sign * row_dir;

    /* Must be a valid "outer" wall */
    if (!square_in_bounds_fully(c, y, x)) return false;
    if (square_is_granite_with_flag(c, y, x, SQUARE_WALL_SOLID)) return false;
    if (!square_is_granite_with_flag(c, y, x, SQUARE_WALL_OUTER)) return false;

    /* Get the "next" location */
    y += sign * col_dir;
    x += sign * row_dir;

    /* Must be a valid location inside the room (to avoid piercing corners) */
    if (!square_in_bounds_fully(c, y, x)) return false;
    if (!square_isroom(c, y, x)) return false;

    /* Accept this location */
    return pierce_outer_locate(c, row1 + sign * col_dir, col1 + sign * row_dir, row_dir, col_dir,
        NULL, NULL);
}


static bool possible_wide_tunnel(struct chunk *c, int row1, int col1, int row_dir, int col_dir,
    int sign)
{
    int y, x;

    /* Get adjacent location */
    y = row1 + sign * col_dir;
    x = col1 + sign * row_dir;

    /* Must be a valid granite wall */
    if (!square_in_bounds_fully(c, y, x)) return false;
    if (!square_isrock(c, y, x)) return false;

    /* Hack -- avoid outer/solid granite walls */
    if (square_is_granite_with_flag(c, y, x, SQUARE_WALL_OUTER)) return false;
    if (square_is_granite_with_flag(c, y, x, SQUARE_WALL_SOLID)) return false;

    /* Accept this location */
    return true;
}


/*
 * Constructs a tunnel between two points
 *
 * c is the current chunk
 * (row1, col1) are the co-ordinates of the first point
 * (row2, col2) are the co-ordinates of the second point
 *
 * This function must be called BEFORE any streamers are created, since we use
 * granite with the special SQUARE_WALL flags to keep track of legal places for
 * corridors to pierce rooms.
 *
 * We queue the tunnel grids to prevent door creation along a corridor which
 * intersects itself.
 *
 * We queue the wall piercing grids to prevent a corridor from leaving
 * a room and then coming back in through the same entrance.
 *
 * We pierce grids which are outer walls of rooms, and when we do so, we change
 * all adjacent outer walls of rooms into solid walls so that no two corridors
 * may use adjacent grids for exits.
 *
 * The solid wall check prevents corridors from chopping the corners of rooms
 * off, as well as silly door placement, and excessively wide room entrances.
 */
static void build_tunnel(struct chunk *c, int row1, int col1, int row2, int col2)
{
    int i, y, x;
    int tmp_row, tmp_col;
    int row_dir, col_dir;
    int start_row, start_col;
    int main_loop_count = 0;
    int sign = 1, feat;

    /* Used to prevent excessive door creation along overlapping corridors. */
    bool door_flag = false;

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
        /* Hack -- paranoia -- prevent infinite loops */
        if (main_loop_count++ > 2000) break;

        /* Allow bends in the tunnel */
        if (magik(dun->profile->tun.chg))
        {
            /* Get the correct direction */
            correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

            /* Random direction */
            if (magik(dun->profile->tun.rnd))
                rand_dir(&row_dir, &col_dir);
        }

        /* Get the next location */
        tmp_row = row1 + row_dir;
        tmp_col = col1 + col_dir;

        /* Be sure we are "in bounds" */
        while (!square_in_bounds(c, tmp_row, tmp_col))
        {
            /* Get the correct direction */
            correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

            /* Random direction */
            if (magik(dun->profile->tun.rnd)) rand_dir(&row_dir, &col_dir);

            /* Get the next location */
            tmp_row = row1 + row_dir;
            tmp_col = col1 + col_dir;
        }

        /* Avoid the edge of the dungeon */
        if (square_isperm_outer(c, tmp_row, tmp_col)) continue;

        /* Avoid "solid" granite walls */
        if (square_is_granite_with_flag(c, tmp_row, tmp_col, SQUARE_WALL_SOLID)) continue;

        /* Pierce "outer" walls of rooms */
        if (square_is_granite_with_flag(c, tmp_row, tmp_col, SQUARE_WALL_OUTER))
        {
            if (!pierce_outer_locate(c, tmp_row, tmp_col, row_dir, col_dir, &row1, &col1)) continue;

            /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
            if (cfg_turn_based && (NumPlayers == 1))
                pierce_outer_save(c, row1, col1);

            /* PWMAngband: try to create wide openings */
            else if (pierce_outer_wide(c, row1, col1, row_dir, col_dir, sign))
            {
                pierce_outer_save(c, row1, col1);

                /* Current adjacent location accepted */
                pierce_outer_save(c, row1 + sign * col_dir, col1 + sign * row_dir);
            }
            else if (pierce_outer_wide(c, row1, col1, row_dir, col_dir, -sign))
            {
                pierce_outer_save(c, row1, col1);

                /* Other adjacent location accepted */
                sign = -sign;
                pierce_outer_save(c, row1 + sign * col_dir, col1 + sign * row_dir);
            }
            else
            {
                pierce_outer_save(c, row1, col1);

                /* No adjacent location accepted: duplicate the entry for later */
                pierce_outer_save(c, row1, col1);
            }
        }

        /* Travel quickly through rooms */
        else if (square_isroom(c, tmp_row, tmp_col))
        {
            /* Accept the location */
            row1 = tmp_row;
            col1 = tmp_col;
        }

        /* Tunnel through all other walls */
        else if (square_isrock(c, tmp_row, tmp_col))
        {
            /* Accept this location */
            row1 = tmp_row;
            col1 = tmp_col;

            /* Save the tunnel location */
            if (dun->tunn_n < z_info->tunn_grid_max)
            {
                dun->tunn[dun->tunn_n].y = row1;
                dun->tunn[dun->tunn_n].x = col1;
                dun->tunn_n++;
            }

            /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
            if (cfg_turn_based && (NumPlayers == 1)) {}

            /* PWMAngband: try to create wide tunnels */
            else if ((dun->tunn_n < z_info->tunn_grid_max) &&
                possible_wide_tunnel(c, row1, col1, row_dir, col_dir, sign))
            {
                dun->tunn[dun->tunn_n].y = row1 + sign * col_dir;
                dun->tunn[dun->tunn_n].x = col1 + sign * row_dir;
                dun->tunn_n++;
            }

            /* Allow door in next grid */
            door_flag = false;
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
                if (dun->door_n < z_info->level_door_max)
                {
                    dun->door[dun->door_n].y = row1;
                    dun->door[dun->door_n].x = col1;
                    dun->door_n++;
                }

                /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
                if (cfg_turn_based && (NumPlayers == 1)) {}

                /* PWMAngband: try to create wide intersections */
                else
                {
                    y = row1 + sign * col_dir;
                    x = col1 + sign * row_dir;
                    if (square_in_bounds_fully(c, y, x) && (dun->door_n < z_info->level_door_max))
                    {
                        dun->door[dun->door_n].y = y;
                        dun->door[dun->door_n].x = x;
                        dun->door_n++;
                    }
                }

                /* No door in next grid */
                door_flag = true;
            }

            /* Hack -- allow pre-emptive tunnel termination */
            if (!magik(dun->profile->tun.con))
            {
                /* Distance between row1 and start_row */
                tmp_row = row1 - start_row;
                if (tmp_row < 0) tmp_row = 0 - tmp_row;

                /* Distance between col1 and start_col */
                tmp_col = col1 - start_col;
                if (tmp_col < 0) tmp_col = 0 - tmp_col;

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

        /* Clear previous contents, add a floor */
        square_set_feat(c, y, x, FEAT_FLOOR);
    }

    /* Apply the piercings that we found */
    for (i = 0; i < dun->wall_n; i++)
    {
        /* Get the grid */
        y = dun->wall[i].y;
        x = dun->wall[i].x;

        /* Convert to floor grid */
        square_set_feat(c, y, x, FEAT_FLOOR);

        /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
        if (cfg_turn_based && (NumPlayers == 1)) {}

        /* PWMAngband: for wide openings, duplicate the door feature */
        else if (i % 2)
        {
            if (feat) square_set_feat(c, y, x, feat);
            feat = 0;
            continue;
        }

        /* Place a random door */
        if (magik(dun->profile->tun.pen))
        {
            place_random_door(c, y, x);
            feat = c->squares[y][x].feat;
        }
        else
            feat = 0;
    }
}


/*
 * Count the number of corridor grids adjacent to the given grid.
 *
 * This routine currently only counts actual "empty floor" grids which are not
 * in rooms.
 *
 * c is the current chunk
 * (y1, x1) are the co-ordinates
 *
 * TODO: count stairs, open doors, closed doors?
 */
static int next_to_corr(struct chunk *c, int y1, int x1)
{
    int i, k = 0;

    my_assert(square_in_bounds(c, y1, x1));

    /* Scan adjacent grids */
    for (i = 0; i < 4; i++)
    {
        /* Extract the location */
        int y = y1 + ddy_ddd[i];
        int x = x1 + ddx_ddd[i];

        /* Count only floors which aren't part of rooms */
        if (square_isfloor(c, y, x) && !square_isroom(c, y, x)) k++;
    }

    /* Return the number of corridors */
    return k;
}


/*
 * Returns whether a doorway can be built in a space.
 *
 * c is the current chunk
 * (y, x) are the co-ordinates
 *
 * To have a doorway, a space must be adjacent to at least two corridors and be
 * between two walls.
 */
static bool possible_doorway(struct chunk *c, int y, int x)
{
    my_assert(square_in_bounds(c, y, x));

    if (next_to_corr(c, y, x) < 2) return false;

    if (square_isstrongwall(c, y - 1, x) && square_isstrongwall(c, y + 1, x))
        return true;

    if (square_isstrongwall(c, y, x - 1) && square_isstrongwall(c, y, x + 1))
        return true;

    return false;
}


/*
 * Returns whether a wide doorway can be built in a space.
 *
 * To have a wide doorway, a space must be adjacent to three corridors and a wall.
 */
static bool possible_wide_doorway(struct chunk *c, int y, int x, int *py, int *px)
{
    my_assert(square_in_bounds(c, y, x));

    if (next_to_corr(c, y, x) != 3) return false;
    if (square_isstrongwall(c, y - 1, x))
    {
        *py = y + 1;
        *px = x;
        return true;
    }
    if (square_isstrongwall(c, y + 1, x))
    {
        *py = y - 1;
        *px = x;
        return true;
    }
    if (square_isstrongwall(c, y, x - 1))
    {
        *py = y;
        *px = x + 1;
        return true;
    }
    if (square_isstrongwall(c, y, x + 1))
    {
        *py = y;
        *px = x - 1;
        return true;
    }
    return false;
}


/*
 * Places door at y, x position if at least 2 walls found
 *
 * c is the current chunk
 * (y, x) are the co-ordinates
 */
static void try_door(struct chunk *c, int y, int x)
{
    int y1, x1, y2, x2;

    my_assert(square_in_bounds(c, y, x));

    if (square_isstrongwall(c, y, x)) return;
    if (square_isroom(c, y, x)) return;

    if (magik(dun->profile->tun.jct))
    {
        if (possible_doorway(c, y, x))
            place_random_door(c, y, x);

        /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
        else if (cfg_turn_based && (NumPlayers == 1)) {}

        /* PWMAngband: for wide intersections, we need two valid adjacent spaces that face each other */
        else if (possible_wide_doorway(c, y, x, &y1, &x1) &&
            possible_wide_doorway(c, y1, x1, &y2, &x2) && (y2 == y) && (x2 == x))
        {
            place_random_door(c, y, x);
            square_set_feat(c, y1, x1, c->squares[y][x].feat);
        }
    }
}


/*
 * Generate a new dungeon level
 *
 * p is the player
 */
struct chunk *classic_gen(struct player *p)
{
    int i, j, k, y, x, y1, x1;
    int by, bx = 0, tby, tbx, key, rarity, built;
    int num_rooms, size_percent;
    int dun_unusual = dun->profile->dun_unusual;
    bool **blocks_tried;
    struct chunk *c;

    /*
     * Possibly generate fewer rooms in a smaller area via a scaling factor.
     * Since we scale row_blocks and col_blocks by the same amount, dun->profile->dun_rooms
     * gives the same "room density" no matter what size the level turns out
     * to be.
     */
    i = randint1(10) + p->depth / 24;
    if (is_quest(p->depth)) size_percent = 100;
    else if (i < 2) size_percent = 75;
    else if (i < 3) size_percent = 80;
    else if (i < 4) size_percent = 85;
    else if (i < 5) size_percent = 90;
    else if (i < 6) size_percent = 95;
    else size_percent = 100;

    /* Scale the various generation variables */
    num_rooms = dun->profile->dun_rooms * size_percent / 100;
    dun->block_hgt = dun->profile->block_size;
    dun->block_wid = dun->profile->block_size;
    c = cave_new(z_info->dungeon_hgt, z_info->dungeon_wid);
    c->depth = p->depth;
    player_cave_new(p, z_info->dungeon_hgt, z_info->dungeon_wid);

    /* Fill cave area with basic granite */
    fill_rectangle(c, 0, 0, c->height - 1, c->width - 1, FEAT_GRANITE, SQUARE_NONE);

    /* Actual maximum number of rooms on this level */
    dun->row_blocks = c->height / dun->block_hgt;
    dun->col_blocks = c->width / dun->block_wid;

    /* Initialize the room table */
    dun->room_map = mem_zalloc(dun->row_blocks * sizeof(bool*));
    for (i = 0; i < dun->row_blocks; i++)
        dun->room_map[i] = mem_zalloc(dun->col_blocks * sizeof(bool));

    /* Initialize the block table */
    blocks_tried = mem_zalloc(dun->row_blocks * sizeof(bool*));
    for (i = 0; i < dun->row_blocks; i++)
        blocks_tried[i] = mem_zalloc(dun->col_blocks * sizeof(bool));

    /* No rooms yet, pits or otherwise. */
    dun->pit_num = 0;
    dun->cent_n = 0;

    /*
     * Build some rooms. Note that the theoretical maximum number of rooms
     * in this profile is currently 36, so built never reaches num_rooms,
     * and room generation is always terminated by having tried all blocks
     */
    built = 0;
    while (built < num_rooms)
    {
        /* Count the room blocks we haven't tried yet. */
        j = 0;
        tby = 0;
        tbx = 0;
        for (by = 0; by < dun->row_blocks; by++)
        {
            for (bx = 0; bx < dun->col_blocks; bx++)
            {
                if (blocks_tried[by][bx]) continue;
                j++;
                if (one_in_(j))
                {
                    tby = by;
                    tbx = bx;
                }
            }
        }
        bx = tbx;
        by = tby;

        /* If we've tried all blocks we're done. */
        if (j == 0) break;

        if (blocks_tried[by][bx]) quit_fmt("generation: inconsistent blocks");

        /* Mark that we are trying this block. */
        blocks_tried[by][bx] = true;

        /* Roll for random key (to be compared against a profile's cutoff) */
        key = randint0(100);

        /*
         * We generate a rarity number to figure out how exotic to make the
         * room. This number has a depth/dun_unusual chance of being > 0,
         * a depth^2/dun_unusual^2 chance of being > 1, up to dun->profile->max_rarity.
         */
        i = 0;
        rarity = 0;
        while ((i == rarity) && (i < dun->profile->max_rarity))
        {
            if (randint0(dun_unusual) < 50 + c->depth / 2) rarity++;
            i++;
        }

        /*
         * Once we have a key and a rarity, we iterate through out list of
         * room profiles looking for a match (whose cutoff > key and whose
         * rarity > this rarity). We try building the room, and if it works
         * then we are done with this iteration. We keep going until we find
         * a room that we can build successfully or we exhaust the profiles.
         */
        for (i = 0; i < dun->profile->n_room_profiles; i++)
        {
            struct room_profile profile = dun->profile->room_profiles[i];

            if (profile.rarity > rarity) continue;
            if (profile.cutoff <= key) continue;

            if (room_build(p, c, by, bx, profile, false))
            {
                built++;
                break;
            }
        }
    }

    for (i = 0; i < dun->row_blocks; i++)
    {
        mem_free(blocks_tried[i]);
        mem_free(dun->room_map[i]);
    }
    mem_free(blocks_tried);
    mem_free(dun->room_map);

    /* Generate permanent walls around the edge of the generated area */
    draw_rectangle(c, 0, 0, c->height - 1, c->width - 1, FEAT_PERM, SQUARE_NONE);

    /* Hack -- scramble the room order */
    for (i = 0; i < dun->cent_n; i++)
    {
        int pick1 = randint0(dun->cent_n);
        int pick2 = randint0(dun->cent_n);

        y1 = dun->cent[pick1].y;
        x1 = dun->cent[pick1].x;
        dun->cent[pick1].y = dun->cent[pick2].y;
        dun->cent[pick1].x = dun->cent[pick2].x;
        dun->cent[pick2].y = y1;
        dun->cent[pick2].x = x1;
    }

    /* Start with no tunnel doors */
    dun->door_n = 0;

    /* Hack -- connect the first room to the last room */
    y = dun->cent[dun->cent_n - 1].y;
    x = dun->cent[dun->cent_n - 1].x;

    /* Connect all the rooms together */
    for (i = 0; i < dun->cent_n; i++)
    {
        /* Connect the room to the previous room */
        build_tunnel(c, dun->cent[i].y, dun->cent[i].x, y, x);

        /* Remember the "previous" room */
        y = dun->cent[i].y;
        x = dun->cent[i].x;
    }

    /* Place intersection doors */
    for (i = 0; i < dun->door_n; i++)
    {
        /* Extract junction location */
        y = dun->door[i].y;
        x = dun->door[i].x;

        /* Try placing doors */
        try_door(c, y, x - 1);
        try_door(c, y, x + 1);
        try_door(c, y - 1, x);
        try_door(c, y + 1, x);
    }

    ensure_connectedness(c);

    /* Add some magma streamers */
    for (i = 0; i < dun->profile->str.mag; i++)
        build_streamer(c, FEAT_MAGMA, dun->profile->str.mc);

    /* Add some quartz streamers */
    for (i = 0; i < dun->profile->str.qua; i++)
        build_streamer(c, FEAT_QUARTZ, dun->profile->str.qc);

    /* Place 3 or 4 down stairs near some walls */
    alloc_stairs(c, FEAT_MORE, rand_range(3, 4), 3);

    /* Place 1 or 2 up stairs near some walls */
    alloc_stairs(c, FEAT_LESS, rand_range(1, 2), 3);

    /* General amount of rubble, traps and monsters */
    k = MAX(MIN(c->depth / 3, 10), 2);

    /* Put some rubble in corridors */
    alloc_objects(p, c, SET_CORR, TYP_RUBBLE, randint1(k), object_level(c->depth), 0);

    /* Place some traps in the dungeon */
    alloc_objects(p, c, SET_BOTH, TYP_TRAP, randint1(k), object_level(c->depth), 0);

    /* Place some fountains in rooms */
    alloc_objects(p, c, SET_ROOM, TYP_FOUNTAIN, randint0(1 + k / 2), object_level(c->depth), 0);

    /* Determine the character location */
    new_player_spot(c, p);

    /* Pick a base number of monsters */
    i = z_info->level_monster_min + randint1(8) + k;

    /* Put some monsters in the dungeon */
    for (; i > 0; i--)
        pick_and_place_distant_monster(p, c, 0, MON_SLEEP);

    /* Put some objects in rooms */
    alloc_objects(p, c, SET_ROOM, TYP_OBJECT, Rand_normal(z_info->room_item_av, 3),
        object_level(c->depth), ORIGIN_FLOOR);

    /* Put some objects/gold in the dungeon */
    alloc_objects(p, c, SET_BOTH, TYP_OBJECT, Rand_normal(z_info->both_item_av, 3),
        object_level(c->depth), ORIGIN_FLOOR);
    alloc_objects(p, c, SET_BOTH, TYP_GOLD, Rand_normal(z_info->both_gold_av, 3),
        object_level(c->depth), ORIGIN_FLOOR);

    /* Apply illumination */
    player_cave_clear(p, true);
    cave_illuminate(p, c, true);

    return c;
}


/* ------------------ LABYRINTH ---------------- */


/*
 * Given an adjoining wall (a wall which separates two labyrinth cells)
 * set a and b to point to the cell indices which are separated. Used by
 * labyrinth_gen().
 *
 * i is the wall index
 * w is the width of the labyrinth
 * a, b are the two cell indices
 */
static void lab_get_adjoin(int i, int w, int *a, int *b)
{
    int y, x;

    i_to_yx(i, w, &y, &x);
    if (x % 2 == 0)
    {
        *a = yx_to_i(y - 1, x, w);
        *b = yx_to_i(y + 1, x, w);
    }
    else
    {
        *a = yx_to_i(y, x - 1, w);
        *b = yx_to_i(y, x + 1, w);
    }
}


/*
 * Return whether (x, y) is in a tunnel.
 *
 * c is the current chunk
 * (y, x) are the co-ordinates
 *
 * For our purposes a tunnel is a horizontal or vertical path, not an
 * intersection. Thus, we want the squares on either side to walls in one
 * case (e.g. up/down) and open in the other case (e.g. left/right). We don't
 * want a square that represents an intersection point.
 *
 * The high-level idea is that these are squares which can't be avoided (by
 * walking diagonally around them).
 */
static bool lab_is_tunnel(struct chunk *c, int y, int x)
{
    bool west = square_isopen(c, y, x - 1);
    bool east = square_isopen(c, y, x + 1);
    bool north = square_isopen(c, y - 1, x);
    bool south = square_isopen(c, y + 1, x);

    return ((north == south) && (west == east) && (north != west));
}


/*
 * Helper function for lab_is_wide_tunnel.
 */
static bool lab_is_wide_tunnel_aux(struct chunk *c, int y, int x, bool recursive, int *dy, int *dx)
{
    bool west = square_isopen(c, y, x - 1);
    bool east = square_isopen(c, y, x + 1);
    bool north = square_isopen(c, y - 1, x);
    bool south = square_isopen(c, y + 1, x);

    if (west && east && north && !south)
    {
        if (recursive)
        {
            *dy = -1;
            *dx = 0;
            return lab_is_wide_tunnel_aux(c, y - 1, x, false, dy, dx);
        }
        return true;
    }
    if (west && east && !north && south)
    {
        if (recursive)
        {
            *dy = 1;
            *dx = 0;
            return lab_is_wide_tunnel_aux(c, y + 1, x, false, dy, dx);
        }
        return true;
    }
    if (west && !east && north && south)
    {
        if (recursive)
        {
            *dy = 0;
            *dx = -1;
            return lab_is_wide_tunnel_aux(c, y, x - 1, false, dy, dx);
        }
        return true;
    }
    if (!west && east && north && south)
    {
        if (recursive)
        {
            *dy = 0;
            *dx = 1;
            return lab_is_wide_tunnel_aux(c, y, x + 1, false, dy, dx);
        }
        return true;
    }
    return false;
}


/*
 * Return whether (x, y) is in a wide tunnel.
 */
static bool lab_is_wide_tunnel(struct chunk *c, int y, int x, int *dy, int *dx)
{
    return lab_is_wide_tunnel_aux(c, y, x, true, dy, dx);
}


/*
 * Build a labyrinth chunk of a given height and width
 *
 * p is the player
 * (h, w) are the dimensions of the chunk
 * lit is whether the labyrinth is lit
 * soft is true if we use regular walls, false if permanent walls
 * wide is true if the labyrinth has wide corridors
 */
static struct chunk *labyrinth_chunk(struct player *p, int h, int w, bool lit, bool soft,
    bool wide)
{
    int i, j, k, y, x;

    /* This is the number of squares in the labyrinth */
    int n = h * w;

    /*
     * NOTE: 'sets' and 'walls' are too large... we only need to use about
     * 1/4 as much memory. However, in that case, the addressing math becomes
     * a lot more complicated, so let's just stick with this because it's
     * easier to read.
     */

    /*
     * 'sets' tracks connectedness; if sets[i] == sets[j] then cells i and j
     * are connected to each other in the maze.
     */
    int *sets;

    /* 'walls' is a list of wall coordinates which we will randomize */
    int *walls;

    /* The labyrinth chunk */
    struct chunk *c = cave_new((wide? h * 2: h) + 2, (wide? w * 2: w) + 2);

    c->depth = p->depth;
    player_cave_new(p, (wide? h * 2: h) + 2, (wide? w * 2: w) + 2);

    /* Allocate our arrays */
    sets = mem_zalloc(n * sizeof(int));
    walls = mem_zalloc(n * sizeof(int));

    /* Bound with perma-rock */
    draw_rectangle(c, 0, 0, (wide? h * 2: h) + 1, (wide? w * 2: w) + 1, FEAT_PERM, SQUARE_NONE);

    /* Fill the labyrinth area with rock */
    if (soft)
        fill_rectangle(c, 1, 1, h, w, FEAT_GRANITE, SQUARE_WALL_SOLID);
    else
        fill_rectangle(c, 1, 1, h, w, FEAT_PERM, SQUARE_NONE);

    /* Initialize each wall. */
    for (i = 0; i < n; i++)
    {
        walls[i] = i;
        sets[i] = -1;
    }

    /* Cut out a grid of 1x1 rooms which we will call "cells" */
    for (y = 0; y < h; y += 2)
    {
        for (x = 0; x < w; x += 2)
        {
            k = yx_to_i(y, x, w);
            sets[k] = k;
            square_set_feat(c, y + 1, x + 1, FEAT_FLOOR);
            if (lit) sqinfo_on(c->squares[y + 1][x + 1].info, SQUARE_GLOW);
        }
    }

    /* Shuffle the walls, using Knuth's shuffle. */
    shuffle(walls, n);

    /*
     * For each adjoining wall, look at the cells it divides. If they aren't
     * in the same set, remove the wall and join their sets.
     *
     * This is a randomized version of Kruskal's algorithm.
     */
    for (i = 0; i < n; i++)
    {
        int a, b;

        j = walls[i];

        /* If this cell isn't an adjoining wall, skip it */
        i_to_yx(j, w, &y, &x);
        if ((x < 1 && y < 1) || (x > w - 2 && y > h - 2)) continue;
        if (x % 2 == y % 2) continue;

        /* Figure out which cells are separated by this wall */
        lab_get_adjoin(j, w, &a, &b);

        /* If the cells aren't connected, kill the wall and join the sets */
        if (sets[a] != sets[b])
        {
            int sa = sets[a];
            int sb = sets[b];

            square_set_feat(c, y + 1, x + 1, FEAT_FLOOR);
            if (lit) sqinfo_on(c->squares[y + 1][x + 1].info, SQUARE_GLOW);

            for (k = 0; k < n; k++)
            {
                if (sets[k] == sb) sets[k] = sa;
            }
        }
    }

    /* Hack -- allow wide corridors */
    if (wide)
    {
        /* Simply stretch the original labyrinth area */
        for (y = h; y >= 1; y--)
        {
            for (x = w; x >= 1; x--)
            {
                c->squares[y * 2][x * 2].feat = c->squares[y][x].feat;
                sqinfo_wipe(c->squares[y * 2][x * 2].info);
                sqinfo_copy(c->squares[y * 2][x * 2].info, c->squares[y][x].info);
                c->squares[y * 2][x * 2 - 1].feat = c->squares[y][x].feat;
                sqinfo_wipe(c->squares[y * 2][x * 2 - 1].info);
                sqinfo_copy(c->squares[y * 2][x * 2 - 1].info, c->squares[y][x].info);
                c->squares[y * 2 - 1][x * 2].feat = c->squares[y][x].feat;
                sqinfo_wipe(c->squares[y * 2 - 1][x * 2].info);
                sqinfo_copy(c->squares[y * 2 - 1][x * 2].info, c->squares[y][x].info);
                c->squares[y * 2 - 1][x * 2 - 1].feat = c->squares[y][x].feat;
                sqinfo_wipe(c->squares[y * 2 - 1][x * 2 - 1].info);
                sqinfo_copy(c->squares[y * 2 - 1][x * 2 - 1].info, c->squares[y][x].info);
            }
        }
    }

    /* Generate a door for every 100 squares in the labyrinth */
    for (i = n / 100; i > 0; i--)
    {
        /* Try 10 times to find a useful place for a door, then place it */
        for (j = 0; j < 10; j++)
        {
            find_empty(c, &y, &x);

            /* Hack -- for wide corridors, place two doors */
            if (wide)
            {
                int dy, dx;

                if (lab_is_wide_tunnel(c, y, x, &dy, &dx))
                {
                    place_closed_door(c, y, x);
                    place_closed_door(c, y + dy, x + dx);
                    break;
                }
                continue;
            }

            if (lab_is_tunnel(c, y, x))
            {
                place_closed_door(c, y, x);
                break;
            }
        }
    }

    /* Unlit labyrinths will have some good items */
    if (!lit)
    {
        alloc_objects(p, c, SET_BOTH, TYP_GOOD, Rand_normal(3, 2), object_level(c->depth),
            ORIGIN_LABYRINTH);
    }

    /* Hard (non-diggable) labyrinths will have some great items */
    if (!soft)
    {
        alloc_objects(p, c, SET_BOTH, TYP_GREAT, Rand_normal(2, 1), object_level(c->depth),
            ORIGIN_LABYRINTH);
    }

    /* Deallocate our lists */
    mem_free(sets);
    mem_free(walls);

    return c;
}


/*
 * Build a labyrinth level.
 *
 * p is the player
 *
 * Note that if the function returns false, a level wasn't generated.
 * Labyrinths use the dungeon level's number to determine whether to generate
 * themselves (which means certain level numbers are more likely to generate
 * labyrinths than others).
 */
struct chunk *labyrinth_gen(struct player *p)
{
    int i, k;
    struct chunk *c;

    /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
    bool turn_based = (cfg_turn_based && (NumPlayers == 1));

    /* Most labyrinths have wide corridors */
    bool wide = (turn_based? false: magik(90));
    int hmax = (wide? z_info->dungeon_hgt / 2 - 2: z_info->dungeon_hgt - 3);

    /*
     * Size of the actual labyrinth part must be odd.
     *
     * NOTE: these are not the actual dungeon size, but rather the size of the
     * area we're generating a labyrinth in (which doesn't count the enclosing
     * outer walls.
     */
    int h = MIN(15 + randint0(p->depth / 10) * 2, hmax);
    int w = 51 + randint0(p->depth / 10) * 2;

    /* Most labyrinths are lit */
    bool lit = ((randint0(p->depth) < 25) || (randint0(2) < 1));

    /* Many labyrinths are known */
    bool known = (lit && (randint0(p->depth) < 25));

    /* Most labyrinths have soft (diggable) walls */
    bool soft = ((randint0(p->depth) < 35) || (randint0(3) < 2));

    /* Generate the actual labyrinth */
    c = labyrinth_chunk(p, h, w, lit, soft, wide);

    /* Hack -- allow wide corridors */
    if (wide)
    {
        h *= 2;
        w *= 2;
    }

    /* The level should have exactly one down and one up staircase */
    alloc_stairs(c, FEAT_MORE, 1, 3);
    alloc_stairs(c, FEAT_LESS, 1, 3);

    /* General some rubble, traps and monsters */
    k = MAX(MIN(c->depth / 3, 10), 2);

    /* Scale number of monsters items by labyrinth size */
    k = (3 * k * (h * w)) / (z_info->dungeon_hgt * z_info->dungeon_wid);

    /* Put some rubble in corridors */
    alloc_objects(p, c, SET_BOTH, TYP_RUBBLE, randint1(k), object_level(c->depth), 0);

    /* Place some traps in the dungeon */
    alloc_objects(p, c, SET_BOTH, TYP_TRAP, randint1(k), object_level(c->depth), 0);

    /* Determine the character location */
    new_player_spot(c, p);

    /* Put some monsters in the dungeon */
    for (i = z_info->level_monster_min + randint1(8) + k; i > 0; i--)
        pick_and_place_distant_monster(p, c, 0, MON_SLEEP);

    /* Put some objects/gold in the dungeon */
    alloc_objects(p, c, SET_BOTH, TYP_OBJECT, Rand_normal(k * 6, 2), object_level(c->depth),
        ORIGIN_LABYRINTH);
    alloc_objects(p, c, SET_BOTH, TYP_GOLD, Rand_normal(k * 3, 2), object_level(c->depth),
        ORIGIN_LABYRINTH);
    alloc_objects(p, c, SET_BOTH, TYP_GOOD, randint1(2), object_level(c->depth),
        ORIGIN_LABYRINTH);

    /* If we want the players to see the maze layout, do that now */
    player_cave_clear(p, true);
    if (known) wiz_light(p, c, false);

    return c;
}


/* ---------------- CAVERNS ---------------------- */


/*
 * Initialize the dungeon array, with a random percentage of squares open.
 *
 * c is the current chunk
 * density is the percentage of floors we are aiming for
 */
static void init_cavern(struct chunk *c, int density)
{
    int h = c->height;
    int w = c->width;
    int size = h * w;
    int count = (size * density) / 100;

    /* Fill the entire chunk with rock */
    fill_rectangle(c, 0, 0, h - 1, w - 1, FEAT_GRANITE, SQUARE_WALL_SOLID);

    while (count > 0)
    {
        int y = randint1(h - 2);
        int x = randint1(w - 2);

        if (square_isrock(c, y, x))
        {
            square_set_feat(c, y, x, FEAT_FLOOR);
            count--;
        }
    }
}


/*
 * Return the number of walls (0-8) adjacent to this square.
 *
 * c is the current chunk
 * (y, x) are the co-ordinates
 */
static int count_adj_walls(struct chunk *c, int y, int x)
{
    int yd, xd;
    int count = 0;

    for (yd = -1; yd <= 1; yd++)
    {
        for (xd = -1; xd <= 1; xd++)
        {
            if ((yd == 0) && (xd == 0)) continue;
            if (square_isfloor(c, y + yd, x + xd)) continue;
            count++;
        }
    }

    return count;
}


/*
 * Run a single pass of the cellular automata rules (4,5) on the dungeon.
 *
 * c is the chunk being mutated
 */
static void mutate_cavern(struct chunk *c)
{
    int y, x;
    int h = c->height;
    int w = c->width;
    int *temp = mem_zalloc(h * w * sizeof(int));

    for (y = 1; y < h - 1; y++)
    {
        for (x = 1; x < w - 1; x++)
        {
            int count = count_adj_walls(c, y, x);

            if (count > 5)
                temp[y * w + x] = FEAT_GRANITE;
            else if (count < 4)
                temp[y * w + x] = FEAT_FLOOR;
            else
                temp[y * w + x] = c->squares[y][x].feat;
        }
    }

    for (y = 1; y < h - 1; y++)
    {
        for (x = 1; x < w - 1; x++)
        {
            if (temp[y * w + x] == FEAT_GRANITE)
                set_marked_granite(c, y, x, SQUARE_WALL_SOLID);
            else
                square_set_feat(c, y, x, temp[y * w + x]);
        }
    }

    mem_free(temp);
}


/*
 * Fill an int[] with a single value.
 *
 * data is the array
 * value is what it's being filled with
 * size is the array length
 */
static void array_filler(int *data, int value, int size)
{
    int i;

    for (i = 0; i < size; i++) data[i] = value;
}


/*
 * Determine if we need to worry about coloring a point, or can ignore it.
 *
 * c is the current chunk
 * colors is the array of current point colors
 * (y, x) are the co-ordinates
 */
static int ignore_point(struct chunk *c, int *colors, int y, int x)
{
    int h = c->height;
    int w = c->width;
    int n = yx_to_i(y, x, w);

    if ((y < 0) || (x < 0) || (y >= h) || (x >= w)) return true;
    if (colors[n]) return true;
    if (square_ispassable(c, y, x)) return false;
    if (square_isdoor(c, y, x)) return false;
    return true;
}


static int xds[] = {0, 0, 1, -1, -1, -1, 1, 1};
static int yds[] = {1, -1, 0, 0, -1, 1, -1, 1};


/*
 * Color a particular point, and all adjacent points.
 *
 * c is the current chunk
 * colors is the array of current point colors
 * counts is the array of current color counts
 * (y, x) are the co-ordinates
 * color is the color we are coloring
 * diagonal controls whether we can progress diagonally
 */
static void build_color_point(struct chunk *c, int *colors, int *counts, int y, int x,
    int color, bool diagonal)
{
    int h = c->height;
    int w = c->width;
    int size = h * w;
    struct queue *queue = q_new(size);
    int dslimit = (diagonal? 8: 4);
    int *added = mem_zalloc(size * sizeof(int));

    array_filler(added, 0, size);

    q_push_int(queue, yx_to_i(y, x, w));

    counts[color] = 0;

    while (q_len(queue) > 0)
    {
        int i, y2, x2;
        int n2 = q_pop_int(queue);

        i_to_yx(n2, w, &y2, &x2);

        if (ignore_point(c, colors, y2, x2)) continue;

        colors[n2] = color;
        counts[color]++;

        for (i = 0; i < dslimit; i++)
        {
            int y3 = y2 + yds[i];
            int x3 = x2 + xds[i];
            int n3 = yx_to_i(y3, x3, w);

            if (ignore_point(c, colors, y3, x3)) continue;
            if (added[n3]) continue;

            q_push_int(queue, n3);
            added[n3] = 1;
        }
    }

    q_free(queue);
    mem_free(added);
}


/*
 * Create a color for each "NESW contiguous" region of the dungeon.
 *
 * c is the current chunk
 * colors is the array of current point colors
 * counts is the array of current color counts
 * diagonal controls whether we can progress diagonally
 */
static void build_colors(struct chunk *c, int *colors, int *counts, bool diagonal)
{
    int y, x;
    int h = c->height;
    int w = c->width;
    int color = 1;

    for (y = 0; y < h; y++)
    {
        for (x = 0; x < w; x++)
        {
            if (ignore_point(c, colors, y, x)) continue;
            build_color_point(c, colors, counts, y, x, color, diagonal);
            color++;
        }
    }
}


/*
 * Find and delete all small (<9 square) open regions.
 *
 * c is the current chunk
 * colors is the array of current point colors
 * counts is the array of current color counts
 */
static void clear_small_regions(struct chunk *c, int *colors, int *counts)
{
    int i, y, x;
    int h = c->height;
    int w = c->width;
    int size = h * w;
    int *deleted = mem_zalloc(size * sizeof(int));

    array_filler(deleted, 0, size);

    for (i = 0; i < size; i++)
    {
        if (counts[i] < 9)
        {
            deleted[i] = 1;
            counts[i] = 0;
        }
    }

    for (y = 1; y < c->height - 1; y++)
    {
        for (x = 1; x < c->width - 1; x++)
        {
            i = yx_to_i(y, x, w);

            if (!deleted[colors[i]]) continue;

            colors[i] = 0;
            if (!square_isperm(c, y, x)) set_marked_granite(c, y, x, SQUARE_WALL_SOLID);
        }
    }

    mem_free(deleted);
}


/*
 * Return the number of colors which have active cells.
 *
 * counts is the array of current color counts
 * size is the total area
 */
static int count_colors(int *counts, int size)
{
    int i;
    int num = 0;

    for (i = 0; i < size; i++) if (counts[i] > 0) num++;
    return num;
}


/*
 * Return the first color which has one or more active cells.
 *
 * counts is the array of current color counts
 * size is the total area
 */
static int first_color(int *counts, int size)
{
    int i;

    for (i = 0; i < size; i++) if (counts[i] > 0) return i;
    return -1;
}


/*
 * Find all cells of 'fromcolor' and repaint them to 'tocolor'.
 *
 * colors is the array of current point colors
 * counts is the array of current color counts
 * from is the color to change
 * to is the color to change to
 * size is the total area
 */
static void fix_colors(int *colors, int *counts, int from, int to, int size)
{
    int i;

    for (i = 0; i < size; i++) if (colors[i] == from) colors[i] = to;
    counts[to] += counts[from];
    counts[from] = 0;
}


/*
 * Create a tunnel connecting a region to one of its nearest neighbors.
 *
 * c is the current chunk
 * colors is the array of current point colors
 * counts is the array of current color counts
 * color is the color of the region we want to connect
 * new_color is the color of the region we want to connect to (if used)
 */
static void join_region(struct chunk *c, int *colors, int *counts, int color, int new_color)
{
    int i;
    int h = c->height;
    int w = c->width;
    int size = h * w;

    /* Allocate a processing queue */
    struct queue *queue = q_new(size);

    /* Allocate an array to keep track of handled squares, and which square we reached them from */
    int *previous = mem_zalloc(size * sizeof(int));

    array_filler(previous, -1, size);

    /* Push all squares of the given color onto the queue */
    for (i = 0; i < size; i++)
    {
        if (colors[i] == color)
        {
            q_push_int(queue, i);
            previous[i] = i;
        }
    }

    /* Process all squares into the queue */
    while (q_len(queue) > 0)
    {
        /* Get the current square and its color */
        int n = q_pop_int(queue);
        int color2 = colors[n];

        /* If we're not looking for a specific color, any new one will do */
        if ((new_color == -1) && color2 && (color2 != color))
            new_color = color2;

        /* See if we've reached a square with a new color */
        if (color2 == new_color)
        {
            /* Step backward through the path, turning stone to tunnel */
            while (colors[n] != color)
            {
                int x, y;
                int xp, yp;

                i_to_yx(n, w, &y, &x);
                colors[n] = color;
                if (!square_isperm(c, y, x) && !square_isvault(c, y, x))
                    square_set_feat(c, y, x, FEAT_FLOOR);
                n = previous[n];

                /* Hack -- create broad corridors */
                i_to_yx(n, w, &yp, &xp);
                if (yp != y) x++;
                else y++;
                if (square_in_bounds_fully(c, y, x) && !square_isperm(c, y, x) && !square_isvault(c, y, x))
                    square_set_feat(c, y, x, FEAT_FLOOR);
            }

            /* Update the color mapping to combine the two colors */
            fix_colors(colors, counts, color2, color, size);

            /* We're done now */
            break;
        }

        /* If we haven't reached a new color, add all the unprocessed adjacent squares to our queue */
        for (i = 0; i < 4; i++)
        {
            int y, x, n2;

            i_to_yx(n, w, &y, &x);

            /* Move to the adjacent square */
            y += yds[i];
            x += xds[i];

            /* Make sure we stay inside the boundaries */
            if ((y < 0) || (y >= h)) continue;
            if ((x < 0) || (x >= w)) continue;

            /* If the cell hasn't already been processed, add it to the queue */
            n2 = yx_to_i(y, x, w);
            if (previous[n2] >= 0) continue;
            q_push_int(queue, n2);
            previous[n2] = n;
        }
    }

    /* Free the memory we've allocated */
    q_free(queue);
    mem_free(previous);
}


/*
 * Start connecting regions, stopping when the cave is entirely connected.
 *
 * c is the current chunk
 * colors is the array of current point colors
 * counts is the array of current color counts
 */
static void join_regions(struct chunk *c, int *colors, int *counts)
{
    int h = c->height;
    int w = c->width;
    int size = h * w;
    int num = count_colors(counts, size);

    /*
     * While we have multiple colors (i.e. disconnected regions), join one of
     * the regions to another one.
     */
    while (num > 1)
    {
        int color = first_color(counts, size);

        join_region(c, colors, counts, color, -1);
        num--;
    }
}


/*
 * Make sure that all the regions of the dungeon are connected.
 *
 * c is the current chunk
 *
 * This function colors each connected region of the dungeon, then uses that
 * information to join them into one connected region.
 */
void ensure_connectedness(struct chunk *c)
{
    int size = c->height * c->width;
    int *colors = mem_zalloc(size * sizeof(int));
    int *counts = mem_zalloc(size * sizeof(int));

    build_colors(c, colors, counts, true);
    join_regions(c, colors, counts);

    mem_free(colors);
    mem_free(counts);
}


#define MAX_CAVERN_TRIES 10


/*
 * The cavern generator's main function.
 *
 * p is the player
 * (h, w) the chunk's dimensions
 *
 * Returns a pointer to the generated chunk.
 */
static struct chunk *cavern_chunk(struct player *p, int h, int w)
{
    int i;
    int size = h * w;
    int limit = size / 13;
    int density = rand_range(25, 40);
    int times = rand_range(3, 6);
    int *colors = mem_zalloc(size * sizeof(int));
    int *counts = mem_zalloc(size * sizeof(int));
    int tries;
    struct chunk *c = cave_new(h, w);

    c->depth = p->depth;
    player_cave_new(p, h, w);

    /* Start trying to build caverns */
    for (tries = 0; tries < MAX_CAVERN_TRIES; tries++)
    {
        /* Build a random cavern and mutate it a number of times */
        init_cavern(c, density);
        for (i = 0; i < times; i++) mutate_cavern(c);

        /* If there are enough open squares then we're done */
        if (c->feat_count[FEAT_FLOOR] >= limit) break;
    }

    /* If we couldn't make a big enough cavern then fail */
    if (tries == MAX_CAVERN_TRIES)
    {
        mem_free(colors);
        mem_free(counts);
        cave_free(c);
        return NULL;
    }

    build_colors(c, colors, counts, false);
    clear_small_regions(c, colors, counts);
    join_regions(c, colors, counts);

    mem_free(colors);
    mem_free(counts);

    return c;
}


/*
 * Make a cavern level.
 *
 * p is the player
 */
struct chunk *cavern_gen(struct player *p)
{
    int i, k;
    int h = rand_range(z_info->dungeon_hgt / 2, (z_info->dungeon_hgt * 3) / 4);
    int w = rand_range(z_info->dungeon_wid / 2, (z_info->dungeon_wid * 3) / 4);
    struct chunk *c;

    /* If we're too shallow then don't do it */
    if (p->depth < 15) return NULL;

    /* Try to build the cavern, fail gracefully */
    c = cavern_chunk(p, h, w);
    if (!c) return NULL;

    /* Surround the level with perma-rock */
    draw_rectangle(c, 0, 0, h - 1, w - 1, FEAT_PERM, SQUARE_NONE);

    /* Place 1-3 down stairs near some walls */
    alloc_stairs(c, FEAT_MORE, rand_range(1, 3), 3);

    /* Place 1-2 up stairs near some walls */
    alloc_stairs(c, FEAT_LESS, rand_range(1, 2), 3);

    /* General some rubble, traps and monsters */
    k = MAX(MIN(c->depth / 3, 10), 2);

    /* Scale number of monsters items by cavern size */
    k = MAX((4 * k * (h * w)) / (z_info->dungeon_hgt * z_info->dungeon_wid), 6);

    /* Put some rubble in corridors */
    alloc_objects(p, c, SET_BOTH, TYP_RUBBLE, randint1(k), object_level(c->depth), 0);

    /* Place some traps in the dungeon */
    alloc_objects(p, c, SET_BOTH, TYP_TRAP, randint1(k), object_level(c->depth), 0);

    /* Determine the character location */
    new_player_spot(c, p);

    /* Put some monsters in the dungeon */
    for (i = randint1(8) + k; i > 0; i--)
        pick_and_place_distant_monster(p, c, 0, MON_SLEEP);

    /* Put some objects/gold in the dungeon */
    alloc_objects(p, c, SET_BOTH, TYP_OBJECT, Rand_normal(k, 2), object_level(c->depth) + 5,
        ORIGIN_CAVERN);
    alloc_objects(p, c, SET_BOTH, TYP_GOLD, Rand_normal(k / 2, 2), object_level(c->depth),
        ORIGIN_CAVERN);
    alloc_objects(p, c, SET_BOTH, TYP_GOOD, randint0(k / 4), object_level(c->depth),
        ORIGIN_CAVERN);

    /* Clear the flags for each cave grid */
    player_cave_clear(p, true);

    return c;
}


/* ------------------ TOWN ---------------- */


/*
 * Builds a feature at a given pseudo-location
 *
 * c is the current chunk
 * n is which feature it is
 * yy, xx the row and column of this feature in the feature layout
 *
 * Currently, there is a main street horizontally through the middle of town,
 * and all the shops face it (e.g. the shops on the north side face south).
 */
static void build_feature(struct chunk *c, int n, int yy, int xx)
{
    int y, x, dy, dx;
    int feat;

    /* Determine spacing based on town size */
    int y_space = z_info->dungeon_hgt / z_info->town_hgt;
    int x_space = z_info->dungeon_wid / z_info->town_wid;

    /* Find the "center" of the feature */
    int y0 = yy * y_space + y_space / 2;
    int x0 = xx * x_space + x_space / 2;

    /* Determine the feature boundaries */
    int y1 = y0 - randint1(3);
    int y2 = y0 + randint1(3);
    int x1 = x0 - randint1(5);
    int x2 = x0 + randint1(5);

    /* Hack -- make forest/tavern as large as possible */
    if ((n == 12) || (n == STORE_TAVERN))
    {
        y1 = y0 - 3;
        y2 = y0 + 3;
        x1 = x0 - 5;
        x2 = x0 + 5;
    }

    /* House (at least 2x2) */
    if (n == 13)
    {
        while (y2 - y1 == 2)
        {
            y1 = y0 - randint1((yy == 0)? 3: 2);
            y2 = y0 + randint1((yy == 1)? 3: 2);
        }
        while (x2 - x1 == 2)
        {
            x1 = x0 - randint1(5);
            x2 = x0 + randint1(5);
        }
    }

    /* Determine door location, based on which side of the street we're on */
    dy = (((yy % 2) == 0)? y2: y1);
    dx = rand_range(x1, x2);

    /* Build an invulnerable rectangular building */
    fill_rectangle(c, y1, x1, y2, x2, FEAT_PERM, SQUARE_NONE);

    /* Hack -- make tavern empty */
    if (n == STORE_TAVERN)
    {
        for (y = y1 + 1; y < y2; y++)
        {
            for (x = x1 + 1; x < x2; x++)
            {
                /* Create the tavern, make it PvP-safe */
                square_add_safe(c, y, x);

                /* Declare this to be a room */
                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
                sqinfo_on(c->squares[y][x].info, SQUARE_VAULT);
                sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
            }
        }

        /* Hack -- have everyone start in the tavern */
        c->level_down_y = (y1 + y2) / 2;
        c->level_down_x = (x1 + x2) / 2;
    }

    /* Pond */
    if (n == 10)
    {
        /* Create the pond */
        fill_rectangle(c, y1, x1, y2, x2, FEAT_WATER, SQUARE_NONE);

        /* Make the pond not so "square" */
        square_add_dirt(c, y1, x1);
        square_add_dirt(c, y1, x2);
        square_add_dirt(c, y2, x1);
        square_add_dirt(c, y2, x2);

        return;
    }

    /* Building with stairs */
    if (n == 11)
    {
        for (y = y1; y <= y2; y++)
        {
            for (x = x1; x <= x2; x++)
            {
                /* Create the area */
                if (magik(50))
                    square_add_grass(c, y, x);
                else
                    square_set_feat(c, y, x, FEAT_FLOOR);
            }
        }

        x = (x1 + x2) / 2;
        y = (y1 + y2) / 2;

        /* Create down stairs */
        square_set_feat(c, y, x, FEAT_MORE);

        /* Hack -- the players start on the stairs while coming up */
        c->level_up_y = c->level_rand_y = y;
        c->level_up_x = c->level_rand_x = x;

        return;
    }

    /* Forest */
    if (n == 12)
    {
        int xc, yc, max_dis;
        int size = (y2 - y1 + 1) * (x2 - x1 + 1);
        bool limit_trees = ((cfg_max_trees > 0) && (size > (cfg_max_trees / 4)));
        int max_chance = (limit_trees? (100 * (cfg_max_trees / 4)): (100 * size));

        /* Find the center of the forested area */
        xc = (x1 + x2) / 2;
        yc = (y1 + y2) / 2;

        /* Find the max distance from center */
        max_dis = distance(y2, x2, yc, xc);

        for (y = y1; y <= y2; y++)
        {
            for (x = x1; x <= x2; x++)
            {
                int chance;

                /* Put some grass */
                square_add_grass(c, y, x);

                /* Calculate chance of a tree */
                chance = 100 * (distance(y, x, yc, xc));
                chance /= max_dis;
                chance = 80 - chance;
                chance *= size;

                /* We want at most (cfg_max_trees / 4) trees */
                if (limit_trees && (chance > max_chance)) chance = max_chance;

                /* Put some trees */
                if (CHANCE(chance, 100 * size) &&
                    ((trees_in_town < cfg_max_trees) || (cfg_max_trees == -1)))
                {
                    square_add_tree(c, y, x);
                    trees_in_town++;
                }
            }
        }

        return;
    }

    /* House */
    if (n == 13)
    {
        int house, price;

        for (y = y1 + 1; y < y2; y++)
        {
            for (x = x1 + 1; x < x2; x++)
            {
                /* Fill with safe floor */
                square_add_safe(c, y, x);

                /* Make it "icky" */
                sqinfo_on(c->squares[y][x].info, SQUARE_VAULT);

                /* Make it glowing */
                sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
            }
        }

        /* Remember price */
        price = (x2 - x1 - 1) * (y2 - y1 - 1);
        price *= 20;
        price *= 80 + randint1(40);

        /* Hack -- only create houses that aren't already loaded from disk */
        house = pick_house(c->depth, dy, dx);
        if (house == -1)
        {
            house_type h_local;

            square_colorize_door(c, dy, dx, 0);

            /* Get an empty house slot */
            house = house_add(false);

            /* Setup house info */
            h_local.x_1 = x1 + 1;
            h_local.y_1 = y1 + 1;
            h_local.x_2 = x2 - 1;
            h_local.y_2 = y2 - 1;
            h_local.door_y = dy;
            h_local.door_x = dx;
            h_local.depth = c->depth;
            h_local.price = price;
            h_local.ownerid = 0;
            h_local.ownername[0] = '\0';
            h_local.color = 0;
            h_local.state = HOUSE_NORMAL;
            h_local.free = 0;

            /* Add a house to our houses list */
            house_set(house, &h_local);
        }
        else
        {
            /* Tag owned house door */
            square_colorize_door(c, dy, dx, house_get(house)->color);
        }

        return;
    }

    /* Clear previous contents, add a store door */
    for (feat = 0; feat < z_info->f_max; feat++)
    {
        if (feat_is_shop(feat) && (f_info[feat].shopnum == n + 1))
            square_set_feat(c, dy, dx, feat);
    }
}


/*
 * Build a road.
 */
static void place_street(struct chunk *c, int line, bool vert)
{
    int y, x, y1, y2, x1, x2;

    /* Vertical streets */
    if (vert)
    {
        x = line * z_info->dungeon_wid / z_info->town_wid;
        x1 = x - 2;
        x2 = x + 2;

        y1 = 5;
        y2 = c->height - 5;
    }
    else
    {
        y = line * z_info->dungeon_hgt / z_info->town_hgt;
        y1 = y - 2;
        y2 = y + 2;

        x1 = 5;
        x2 = c->width - 5;
    }

    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (c->squares[y][x].feat != FEAT_STREET) square_add_grass(c, y, x);
        }
    }

    if (vert)
    {
        x1++;
        x2--;
    }
    else
    {
        y1++;
        y2--;
    }

    fill_rectangle(c, y1, x1, y2, x2, FEAT_STREET, SQUARE_NONE);
}


/*
 * Generate the town for the first time, and place the player
 *
 * c is the current chunk
 */
static void town_gen_layout(struct chunk *c)
{
    int y, x, n, k;
    int *rooms;
    int n_rows = 2;
    int n_cols = (MAX_STORES - 1) / n_rows;
    int size = (c->height - 2) * (c->width - 2);
    bool limit_trees = ((cfg_max_trees > 0) && (size > (cfg_max_trees / 4)));
    int max_chance = (limit_trees? (100 * (cfg_max_trees / 4)): (100 * size));
    int chance;

    /* Determine spacing based on town size */
    int y0 = (z_info->town_hgt - n_rows) / 2;
    int x0 = (z_info->town_wid - n_cols) / 2;

    /* Create boundary */
    draw_rectangle(c, 0, 0, c->height - 1, c->width - 1, (cfg_town_wall? FEAT_PERM: FEAT_PERM_CLEAR),
        SQUARE_NONE);

    /* Create some floor */
    fill_rectangle(c, 1, 1, c->height - 2, c->width - 2, FEAT_FLOOR, SQUARE_NONE);

    trees_in_town = 0;

    /* Calculate chance of a tree */
    chance = 4 * size;

    /* We want at most (cfg_max_trees / 4) trees */
    if (limit_trees && (chance > max_chance)) chance = max_chance;

    /* Hack -- start with basic floors */
    for (y = 1; y < c->height - 1; y++)
    {
        for (x = 1; x < c->width - 1; x++)
        {
            /* Clear all features, set to "empty floor" */
            square_add_dirt(c, y, x);

            /* Generate some trees */
            if (CHANCE(chance, 100 * size) &&
                ((trees_in_town < cfg_max_trees) || (cfg_max_trees == -1)))
            {
                square_add_tree(c, y, x);
                trees_in_town++;
            }

            /* Generate grass patches */
            else if (magik(75)) square_add_grass(c, y, x);
        }
    }

    /* Place horizontal "streets" */
    for (y = 1; y <= z_info->town_hgt / 2; y = y + 2) place_street(c, y, false);
    for (y = z_info->town_hgt - 1; y > z_info->town_hgt / 2; y = y - 2) place_street(c, y, false);

    /* Place vertical "streets" */
    for (x = 1; x <= z_info->town_wid / 2; x = x + 2) place_street(c, x, true);
    for (x = z_info->town_wid - 1; x > z_info->town_wid / 2; x = x - 2) place_street(c, x, true);

    /* Prepare an Array of remaining features, and count them */
    rooms = mem_zalloc(z_info->town_wid * z_info->town_hgt * sizeof(int));
    for (n = 0; n < MAX_STORES - 1; n++)
        rooms[n] = n; /* MAX_STORES - 1 stores */
    for (n = MAX_STORES - 1; n < MAX_STORES + 5; n++)
        rooms[n] = 10; /* 6 ponds */
    for (n = MAX_STORES + 5; n < MAX_STORES + 8; n++)
        rooms[n] = 12; /* 3 forests */
    for (n = MAX_STORES + 8; n < z_info->town_wid * z_info->town_hgt - 1; n++)
        rooms[n] = 13; /* houses */
    rooms[n++] = 11; /* stairs */

    /* Place rows of stores */
    for (y = y0; y < y0 + n_rows; y++)
    {
        for (x = x0; x < x0 + n_cols; x++)
        {
            /* Pick a remaining store */
            k = randint0(n - z_info->town_wid * z_info->town_hgt + MAX_STORES - 1);

            /* Build that store at the proper location */
            build_feature(c, rooms[k], y, x);

            /* Shift the stores down, remove one store */
            n--;
            rooms[k] = rooms[n - z_info->town_wid * z_info->town_hgt + MAX_STORES - 1];
        }
    }

    /* Place rows of features */
    for (y = 0; y < z_info->town_hgt; y++)
    {
        for (x = 0; x < z_info->town_wid; x++)
        {
            /* Make sure we haven't already placed this one */
            if ((y >= y0) && (y < y0 + n_rows) && (x >= x0) && (x < x0 + n_cols)) continue;

            /* Pick a remaining feature */
            k = randint0(n) + MAX_STORES - 1;

            /* Build that feature at the proper location */
            build_feature(c, rooms[k], y, x);

            /* Shift the features down, remove one feature */
            n--;
            rooms[k] = rooms[n + MAX_STORES - 1];
        }
    }

    mem_free(rooms);
}


/*
 * Town logic flow for generation of new town.
 *
 * p is the player (unused -- should always be NULL)
 *
 * Returns a pointer to the generated chunk.
 *
 * We start with a fully wiped cave of normal floors. This function does NOT do
 * anything about the owners of the stores, nor the contents thereof. It only
 * handles the physical layout.
 *
 * Hack -- since boundary walls are a 'good thing' for many of the algorithms
 * used, the feature FEAT_PERM_CLEAR was created. It is used to create an
 * invisible boundary wall for town and wilderness levels, keeping the
 * algorithms happy, and the players fooled.
 */
struct chunk *town_gen(struct player *p)
{
    int i;
    int residents = (is_daytime()? z_info->town_monsters_day: z_info->town_monsters_night);
    struct chunk *c;

    /* Base town should be generated once -- when starting the server for the first time */
    my_assert(p == NULL);

    /* Make a new chunk */
    c = cave_new(z_info->dungeon_hgt, z_info->dungeon_wid);

    /* Build stuff */
    town_gen_layout(c);

    /* Apply illumination */
    cave_illuminate(NULL, c, is_daytime());

    /* Make some residents */
    for (i = 0; i < residents; i++)
        pick_and_place_distant_monster(NULL, c, 0, MON_SLEEP);

    return c;
}


/*
 * Builds a store at a given pseudo-location
 *
 * c is the current chunk
 * n is which shop it is
 * yy, xx the row and column of this store in the store layout
 */
static void build_store(struct chunk *c, int n, int yy, int xx)
{
    int feat, y, x;

    /* Hack -- make tavern as large as possible */
    int rad = ((n == STORE_TAVERN)? 2: 1);

    /* Determine door location */
    int dy = rand_range(yy - rad, yy + rad);
    int dx = (((dy == yy - rad) || (dy == yy + rad))? rand_range(xx - rad, xx + rad):
        (xx - rad + rad * 2 * randint0(2)));

    /* Build an invulnerable rectangular building */
    fill_rectangle(c, yy - rad, xx - rad, yy + rad, xx + rad, FEAT_PERM, SQUARE_NONE);

    /* Hack -- make tavern empty */
    if (n == STORE_TAVERN)
    {
        for (y = yy - 1; y <= yy + 1; y++)
        {
            for (x = xx - 1; x <= xx + 1; x++)
            {
                /* Create the tavern, make it PvP-safe */
                square_add_safe(c, y, x);

                /* Declare this to be a room */
                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
                sqinfo_on(c->squares[y][x].info, SQUARE_VAULT);
                sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
            }
        }
    }

    /* Clear previous contents, add a store door */
    for (feat = 0; feat < z_info->f_max; feat++)
    {
        if (feat_is_shop(feat) && (f_info[feat].shopnum == n + 1))
            square_set_feat(c, dy, dx, feat);
    }
}


/*
 * Locate an empty square for y1 <= y < y2, x1 <= x < x2.
 */
static bool find_empty_range(struct chunk *c, int *y, int y1, int y2, int *x, int x1, int x2)
{
    return square_find_in_range(c, y, y1, y2, x, x1, x2, square_isempty);
}


/*
 * Generate the town for the first time, and place the player
 *
 * p is the player
 * c is the current chunk
 */
static void new_town_gen_layout(struct player *p, struct chunk *c)
{
    int y, x, n, num_lava = 3 + randint0(3), num_rubble = 3 + randint0(3);

    /* Town dimensions (PWMAngband: make town twice as big as Angband) */
    int town_hgt = 44;
    int town_wid = 132;

    /* Town limits */
    int y1 = (c->height - town_hgt) / 2;
    int x1 = (c->width - town_wid) / 2;
    int y2 = (c->height + town_hgt) / 2;
    int x2 = (c->width + town_wid) / 2;

    /* Create walls */
    draw_rectangle(c, 0, 0, c->height - 1, c->width - 1, FEAT_PERM, SQUARE_NONE);

    /* Initialize to ROCK for build_streamer precondition */
    fill_rectangle(c, 1, 1, c->height - 2, c->width - 2, FEAT_GRANITE, SQUARE_WALL_SOLID);

    /* Make some lava streamers */
    for (n = 0; n < 3 + num_lava; n++) build_streamer(c, FEAT_LAVA_FLOW, 0);

    /* Make a town-sized starburst room. */
    generate_starburst_room(c, y1, x1, y2, x2, false, FEAT_FLOOR, false);

    /* Turn off room illumination flag */
    for (y = 1; y < c->height - 1; y++)
    {
        for (x = 1; x < c->width - 1; x++)
        {
            if (square_isfloor(c, y, x))
                sqinfo_off(c->squares[y][x].info, SQUARE_ROOM);
            else if (!square_isperm(c, y, x))
                square_set_feat(c, y, x, FEAT_PERM_STATIC);
        }
    }

    /* Place stores */
    for (n = STORE_GENERAL; n <= STORE_TAVERN; n++)
    {
        int tries = 1000;
        int xx, yy;

        /* Hack -- make tavern as large as possible */
        int rad = ((n == STORE_TAVERN)? 3: 2);

        /* Find an empty place */
        while (tries)
        {
            bool found_non_floor = false;

            find_empty_range(c, &y, y1, y2, &x, x1, x2);
            for (yy = y - rad; yy <= y + rad; yy++)
            {
                for (xx = x - rad; xx <= x + rad; xx++)
                {
                    if (!square_isfloor(c, yy, xx)) found_non_floor = true;
                }
            }
            if (!found_non_floor) break;
            tries--;
        }

        /* Build a store */
        my_assert(tries);
        build_store(c, n, y, x);
    }

    /* Place a few piles of rubble */
    for (n = 0; n < num_rubble; n++)
    {
        int tries = 1000;
        int xx, yy;

        /* Find an empty place */
        while (tries)
        {
            bool found_non_floor = false;

            find_empty_range(c, &y, y1, y2, &x, x1, x2);
            for (yy = y - 2; yy <= y + 2; yy++)
            {
                for (xx = x - 2; xx <= x + 2; xx++)
                {
                    if (!square_isfloor(c, yy, xx)) found_non_floor = true;
                }
            }
            if (!found_non_floor) break;
            tries--;
        }

        /* Place rubble at random */
        my_assert(tries);
        for (yy = y - 1; yy <= y + 1; yy++)
        {
            for (xx = x - 1; xx <= x + 1; xx++)
            {
                if (one_in_(1 + ABS(x - xx) + ABS(y - yy)))
                    square_set_feat(c, yy, xx, FEAT_PASS_RUBBLE);
            }
        }
    }

    /* Place the stairs in the north wall */
    x = rand_spread(c->width / 2, town_wid / 3);
    y = 2;
    while (square_isperm(c, y, x)) y++;
    y--;

    /* Clear previous contents, add down stairs */
    square_set_feat(c, y, x, FEAT_MORE);

    /* PWMAngband: it's a dungeon town, so set this to be the starting location for people going up */
    c->level_up_y = y;
    c->level_up_x = x;

    /* PWMAngband: it's a dungeon town, so place up stairs too (in the south wall) */
    x = rand_spread(c->width / 2, town_wid / 3);
    y = c->height - 2;
    while (square_isperm(c, y, x)) y--;
    y++;
    square_set_feat(c, y, x, FEAT_LESS);

    /* PWMAngband: it's a dungeon town, so set this to be the starting location for people going down */
    c->level_down_y = y;
    c->level_down_x = x;

    /* PWMAngband: it's a dungeon town, so determine the character location */
    new_player_spot(c, p);
}


/*
 * Town logic flow for generation of new town.
 *
 * p is the player
 *
 * Returns a pointer to the generated chunk.
 *
 * We start with a fully wiped cave of normal floors. This function does NOT do
 * anything about the owners of the stores, nor the contents thereof. It only
 * handles the physical layout.
 *
 * PWMAngband: the layout for Angband's new town is used to generate a static level at 750ft
 * for ironman servers (or when the "more towns" option is set)
 */
struct chunk *new_town_gen(struct player *p)
{
    /* Make a new chunk */
    struct chunk *c = cave_new(z_info->dungeon_hgt, z_info->dungeon_wid);

    c->depth = p->depth;
    player_cave_new(p, z_info->dungeon_hgt, z_info->dungeon_wid);

    /* Build stuff */
    new_town_gen_layout(p, c);

    /* Apply illumination */
    player_cave_clear(p, true);
    cave_illuminate(p, c, true);

    return c;
}
