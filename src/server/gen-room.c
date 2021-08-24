/*
 * File: gen-room.c
 * Purpose: Dungeon room generation
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
 * This file covers everything to do with generation of individual rooms in
 * the dungeon. It consists of room generating helper functions plus the
 * actual room builders (which are referred to in the room profiles in
 * generate.c).
 *
 * The room builders all take as arguments the chunk they are being generated
 * in, and the co-ordinates of the room centre in that chunk. Each room
 * builder is also able to find space for itself in the chunk using the
 * find_space() function; the chunk generating functions can ask it to do that
 * by passing too large centre co-ordinates.
 */


/*
 * Chooses a room template of a particular kind at random.
 *
 * typ template room type - currently unused
 *
 * Returns a pointer to the room template.
 */
static struct room_template *random_room_template(int typ)
{
    struct room_template *t = room_templates;
    struct room_template *r = NULL;
    int n = 1;

    do
    {
        if (t->typ == typ)
        {
            if (one_in_(n)) r = t;
            n++;
        }
        t = t->next;
    }
    while(t);

    return r;
}


/*
 * Chooses a vault of a particular kind at random.
 *
 * depth the current depth, for vault bounds checking
 * typ vault type
 *
 * Returns a pointer to the vault template.
 */
static struct vault *random_vault(int depth, const char *typ)
{
    struct vault *v = vaults;
    struct vault *r = NULL;
    int n = 1;

    do
    {
        if (streq(v->typ, typ) && (v->min_lev <= depth) && (v->max_lev >= depth))
        {
            if (one_in_(n)) r = v;
            n++;
        }
        v = v->next;
    }
    while (v);

    return r;
}


/*
 * Mark squares as being in a room, and optionally light them.
 *
 * c the current chunk
 * y1, x1, y2, x2 inclusive room boundaries
 * light whether or not to light the room
 */
static void generate_room(struct chunk *c, int y1, int x1, int y2, int x2, int light)
{
    int y, x;

    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
            if (light)
                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
        }
    }
}


/*
 * Mark a rectangle with a set of info flags
 *
 * c the current chunk
 * y1, x1, y2, x2 inclusive room boundaries
 * flag the SQUARE_* flag we are marking with
 */
void generate_mark(struct chunk *c, int y1, int x1, int y2, int x2, int flag)
{
    int y, x;

    for (y = y1; y <= y2; y++)
        for (x = x1; x <= x2; x++)
            sqinfo_on(c->squares[y][x].info, flag);
}


/*
 * Fill a rectangle with a feature.
 *
 * c the current chunk
 * y1, x1, y2, x2 inclusive room boundaries
 * feat the terrain feature
 * flag the SQUARE_* flag we are marking with
 */
void fill_rectangle(struct chunk *c, int y1, int x1, int y2, int x2, int feat, int flag)
{
    int y, x;

    for (y = y1; y <= y2; y++)
        for (x = x1; x <= x2; x++)
            square_set_feat(c, y, x, feat);
    if (flag) generate_mark(c, y1, x1, y2, x2, flag);
}


/*
 * Fill the edges of a rectangle with a feature.
 *
 * c the current chunk
 * y1, x1, y2, x2 inclusive room boundaries
 * feat the terrain feature
 * flag the SQUARE_* flag we are marking with
 */
void draw_rectangle(struct chunk *c, int y1, int x1, int y2, int x2, int feat, int flag)
{
    int y, x;

    for (y = y1; y <= y2; y++)
    {
        square_set_feat(c, y, x1, feat);
        square_set_feat(c, y, x2, feat);
    }
    if (flag)
    {
        generate_mark(c, y1, x1, y2, x1, flag);
        generate_mark(c, y1, x2, y2, x2, flag);
    }

    for (x = x1; x <= x2; x++)
    {
        square_set_feat(c, y1, x, feat);
        square_set_feat(c, y2, x, feat);
    }
    if (flag)
    {
        generate_mark(c, y1, x1, y1, x2, flag);
        generate_mark(c, y2, x1, y2, x2, flag);
    }
}


/*
 * Fill a horizontal range with the given feature/info.
 *
 * c the current chunk
 * y, x1, x2 inclusive range boundaries
 * feat the terrain feature
 * flag the SQUARE_* flag we are marking with
 * light lit or not
 */
static void fill_xrange(struct chunk *c, int y, int x1, int x2, int feat, int flag, bool light)
{
    int x;

    for (x = x1; x <= x2; x++)
    {
        square_set_feat(c, y, x, feat);
        sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
        if (flag) sqinfo_on(c->squares[y][x].info, flag);
        if (light)
            sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
    }
}


/*
 * Fill a vertical range with the given feature/info.
 *
 * c the current chunk
 * x, y1, y2 inclusive range boundaries
 * feat the terrain feature
 * flag the SQUARE_* flag we are marking with
 * light lit or not
 */
static void fill_yrange(struct chunk *c, int x, int y1, int y2, int feat, int flag, bool light)
{
    int y;

    for (y = y1; y <= y2; y++)
    {
        square_set_feat(c, y, x, feat);
        sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
        if (flag) sqinfo_on(c->squares[y][x].info, flag);
        if (light)
            sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
    }
}


#define ROUND(x) ((((x) - floor(x)) * 10 >= 5)? floor((x) + 1): floor(x))


/*
 * Fill a circle with the given feature/info.
 *
 * c the current chunk
 * y0, x0 the circle centre
 * radius the circle radius
 * border the width of the circle border
 * feat the terrain feature
 * flag the SQUARE_* flag we are marking with
 * light lit or not
 */
static void fill_circle(struct chunk *c, int y0, int x0, int radius, int border, int feat, int flag,
    bool light)
{
    int i, last = 0;
    int r2 = radius * radius;

    for (i = 0; i <= radius; i++)
    {
        double j = sqrt(r2 - (i * i));
        int k = ROUND(j);
        int b = 0;

        if (border && (last > k)) b++;

        fill_xrange(c, y0 - i, x0 - k - b, x0 + k + b, feat, flag, light);
        fill_xrange(c, y0 + i, x0 - k - b, x0 + k + b, feat, flag, light);
        fill_yrange(c, x0 - i, y0 - k - b, y0 + k + b, feat, flag, light);
        fill_yrange(c, x0 + i, y0 - k - b, y0 + k + b, feat, flag, light);
        last = k;
    }
}


/*
 * Fill the lines of a cross/plus with a feature.
 *
 * c the current chunk
 * y1, x1, y2, x2 inclusive room boundaries
 * feat the terrain feature
 * flag the SQUARE_* flag we are marking with
 *
 * When combined with draw_rectangle() this will generate a large rectangular
 * room which is split into four sub-rooms.
 */
static void generate_plus(struct chunk *c, int y1, int x1, int y2, int x2, int feat, int flag)
{
    int y, x;

    /* Find the center */
    int y0 = (y1 + y2) / 2;
    int x0 = (x1 + x2) / 2;

    my_assert(c);

    for (y = y1; y <= y2; y++) square_set_feat(c, y, x0, feat);
    if (flag) generate_mark(c, y1, x0, y2, x0, flag);
    for (x = x1; x <= x2; x++) square_set_feat(c, y0, x, feat);
    if (flag) generate_mark(c, y0, x1, y0, x2, flag);
}


/*
 * Generate helper -- open all sides of a rectangle with a feature
 *
 * c the current chunk
 * y1, x1, y2, x2 inclusive room boundaries
 * feat the terrain feature
 */
static void generate_open(struct chunk *c, int y1, int x1, int y2, int x2, int feat)
{
    int y0, x0;

    /* Center */
    y0 = (y1 + y2) / 2;
    x0 = (x1 + x2) / 2;

    /* Open all sides */
    square_set_feat(c, y1, x0, feat);
    square_set_feat(c, y0, x1, feat);
    square_set_feat(c, y2, x0, feat);
    square_set_feat(c, y0, x2, feat);
}


/*
 * Generate helper -- open one side of a rectangle with a feature
 *
 * c the current chunk
 * y1, x1, y2, x2 inclusive room boundaries
 * feat the terrain feature
 */
static void generate_hole(struct chunk *c, int y1, int x1, int y2, int x2, int feat)
{
    /* Find the center */
    int y0 = (y1 + y2) / 2;
    int x0 = (x1 + x2) / 2;

    my_assert(c);

    /* Open random side */
    switch (randint0(4))
    {
        case 0: square_set_feat(c, y1, x0, feat); break;
        case 1: square_set_feat(c, y0, x1, feat); break;
        case 2: square_set_feat(c, y2, x0, feat); break;
        case 3: square_set_feat(c, y0, x2, feat); break;
    }
}


/*
 * Place a square of granite with a flag
 *
 * c the current chunk
 * y, x the square co-ordinates
 * flag the SQUARE_* flag we are marking with
 */
void set_marked_granite(struct chunk *c, int y, int x, int flag)
{
    square_set_feat(c, y, x, FEAT_GRANITE);
    if (flag) generate_mark(c, y, x, y, x, flag);
}


/*
 * Make a starburst room.
 *
 * c the current chunk
 * y1, x1, y2, x2 boundaries which will contain the starburst
 * light lit or not
 * feat the terrain feature to make the starburst of
 * special_ok allow wacky cloverleaf rooms
 *
 * Starburst rooms are made in three steps:
 * 1: Choose a room size-dependant number of arcs.  Large rooms need to 
 *    look less granular and alter their shape more often, so they need 
 *    more arcs.
 * 2: For each of the arcs, calculate the portion of the full circle it 
 *    includes, and its maximum effect range (how far in that direction 
 *    we can change features in).  This depends on room size, shape, and 
 *    the maximum effect range of the previous arc.
 * 3: Use the table "get_angle_to_grid" to supply angles to each grid in 
 *    the room.  If the distance to that grid is not greater than the 
 *    maximum effect range that applies at that angle, change the feature 
 *    if appropriate (this depends on feature type).
 *
 * Usage notes:
 * - This function uses a table that cannot handle distances larger than 
 *   20, so it calculates a distance conversion factor for larger rooms.
 * - This function is not good at handling rooms much longer along one axis 
 *   than the other, so it divides such rooms up, and calls itself to handle
 *   each section.  
 * - It is safe to call this function on areas that might contain vaults or 
 *   pits, because "icky" and occupied grids are left untouched.
 *
 * - Mixing these rooms (using normal floor) with rectangular ones on a 
 *   regular basis produces a somewhat chaotic looking dungeon.  However, 
 *   this code does works well for lakes, etc.
 */
bool generate_starburst_room(struct chunk *c, int y1, int x1, int y2, int x2, bool light, int feat,
    bool special_ok)
{
    int y0, x0, y, x, ny, nx;
    int i, d;
    int dist, max_dist, dist_conv, dist_check;
    int height, width;
    int degree_first, center_of_arc, degree;

    /* Special variant room. Discovered by accident. */
    bool make_cloverleaf = false;

    /* Holds first degree of arc, maximum effect distance in arc. */
    int arc[45][2];

    /* Number (max 45) of arcs. */
    int arc_num;

    struct feature *f = &f_info[feat];

    /* Make certain the room does not cross the dungeon edge. */
    if ((!square_in_bounds(c, y1, x1)) || (!square_in_bounds(c, y2, x2)))
        return false;

    /* Robustness -- test sanity of input coordinates. */
    if ((y1 + 2 >= y2) || (x1 + 2 >= x2))
        return false;

    /* Get room height and width. */
    height = 1 + y2 - y1;
    width = 1 + x2 - x1;

    /* Handle long, narrow rooms by dividing them up. */
    if ((height > 5 * width / 2) || (width > 5 * height / 2))
    {
        int tmp_ay, tmp_ax, tmp_by, tmp_bx;

        /* Get bottom-left borders of the first room. */
        tmp_ay = y2;
        tmp_ax = x2;
        if (height > width)
            tmp_ay = y1 + 2 * height / 3;
        else
            tmp_ax = x1 + 2 * width / 3;

        /* Make the first room. */
        generate_starburst_room(c, y1, x1, tmp_ay, tmp_ax, light, feat, false);


        /* Get top_right borders of the second room. */
        tmp_by = y1;
        tmp_bx = x1;
        if (height > width)
            tmp_by = y1 + 1 * height / 3;
        else
            tmp_bx = x1 + 1 * width / 3;

        /* Make the second room. */
        generate_starburst_room(c, tmp_by, tmp_bx, y2, x2, light, feat, false);

        /*
         * If floor, extend a "corridor" between room centers, to ensure
         * that the rooms are connected together.
         */
        if (tf_has(f->flags, TF_FLOOR))
        {
            for (y = (y1 + tmp_ay) / 2; y <= (tmp_by + y2) / 2; y++)
                for (x = (x1 + tmp_ax) / 2; x <= (tmp_bx + x2) / 2; x++)
                    square_set_feat(c, y, x, feat);
        }

        /*
         * Otherwise fill any gap between two starbursts.
         */
        else
        {
            int tmp_cy1, tmp_cx1, tmp_cy2, tmp_cx2;

            if (height > width)
            {
                tmp_cy1 = y1 + (height - width) / 2;
                tmp_cx1 = x1;
                tmp_cy2 = tmp_cy1 - (height - width) / 2;
                tmp_cx2 = x2;
            }
            else
            {
                tmp_cy1 = y1;
                tmp_cx1 = x1 + (width - height) / 2;
                tmp_cy2 = y2;
                tmp_cx2 = tmp_cx1 + (width - height) / 2;

                tmp_cy1 = y1;
                tmp_cx1 = x1;
            }

            /* Make the third room. */
            generate_starburst_room(c, tmp_cy1, tmp_cx1, tmp_cy2, tmp_cx2, light, feat, false);
        }

        /* Return. */
        return true;
    }

    /* Get a shrinkage ratio for large rooms, as table is limited. */
    if ((width > 44) || (height > 44))
    {
        if (width > height)
            dist_conv = 10 * width / 44;
        else
            dist_conv = 10 * height / 44;
    }
    else
        dist_conv = 10;

    /* Make a cloverleaf room sometimes. */
    if ((special_ok) && (height > 10) && (randint0(20) == 0))
    {
        arc_num = 12;
        make_cloverleaf = true;
    }

    /* Usually, we make a normal starburst. */
    else
    {
        /* Ask for a reasonable number of arcs. */
        arc_num = 8 + (height * width / 80);
        arc_num = arc_num + 3 - randint0(7);
        if (arc_num < 8)
            arc_num = 8;
        if (arc_num > 45)
            arc_num = 45;
    }

    /* Get the center of the starburst. */
    y0 = y1 + height / 2;
    x0 = x1 + width / 2;

    /* Start out at zero degrees. */
    degree_first = 0;

    /* Determine the start degrees and expansion distance for each arc. */
    for (i = 0; i < arc_num; i++)
    {
        /* Get the first degree for this arc. */
        arc[i][0] = degree_first;

        /* Get a slightly randomized start degree for the next arc. */
        degree_first += (180 + randint0(arc_num)) / arc_num;
        if (degree_first < 180 * (i + 1) / arc_num)
            degree_first = 180 * (i + 1) / arc_num;
        if (degree_first > (180 + arc_num) * (i + 1) / arc_num)
            degree_first = (180 + arc_num) * (i + 1) / arc_num;

        /* Get the center of the arc. */
        center_of_arc = degree_first + arc[i][0];

        /* Calculate a reasonable distance to expand vertically. */
        if (((center_of_arc > 45) && (center_of_arc < 135)) ||
            ((center_of_arc > 225) && (center_of_arc < 315)))
        {
            arc[i][1] = height / 4 + randint0((height + 3) / 4);
        }

        /* Calculate a reasonable distance to expand horizontally. */
        else if (((center_of_arc < 45) || (center_of_arc > 315)) ||
            ((center_of_arc < 225) && (center_of_arc > 135)))
        {
            arc[i][1] = width / 4 + randint0((width + 3) / 4);
        }

        /* Handle arcs that count as neither vertical nor horizontal */
        else if (i != 0)
        {
            if (make_cloverleaf)
                arc[i][1] = 0;
            else
                arc[i][1] = arc[i - 1][1] + 3 - randint0(7);
        }

        /* Keep variability under control. */
        if ((!make_cloverleaf) && (i != 0) && (i != arc_num - 1))
        {
            /* Water edges must be quite smooth. */
            if (tf_has(f->flags, TF_SMOOTH))
            {
                if (arc[i][1] > arc[i - 1][1] + 2)
                    arc[i][1] = arc[i - 1][1] + 2;

                if (arc[i][1] > arc[i - 1][1] - 2)
                    arc[i][1] = arc[i - 1][1] - 2;
            }
            else
            {
                if (arc[i][1] > 3 * (arc[i - 1][1] + 1) / 2)
                    arc[i][1] = 3 * (arc[i - 1][1] + 1) / 2;

                if (arc[i][1] < 2 * (arc[i - 1][1] - 1) / 3)
                    arc[i][1] = 2 * (arc[i - 1][1] - 1) / 3;
            }
        }

        /* Neaten up final arc of circle by comparing it to the first. */
        if ((i == arc_num - 1) && (ABS(arc[i][1] - arc[0][1]) > 3))
        {
            if (arc[i][1] > arc[0][1])
                arc[i][1] -= randint0(arc[i][1] - arc[0][1]);
            else if (arc[i][1] < arc[0][1])
                arc[i][1] += randint0(arc[0][1] - arc[i][1]);
        }
    }

    /* Precalculate check distance. */
    dist_check = 21 * dist_conv / 10;

    /* Change grids between (and not including) the edges. */
    for (y = y1 + 1; y < y2; y++)
    {
        for (x = x1 + 1; x < x2; x++)
        {
            /* Do not touch vault grids. */
            if (square_isvault(c, y, x))
                continue;

            /* Do not touch occupied grids. */
            if (square_monster(c, y, x))
                continue;
            if (square_object(c, y, x))
                continue;

            /* Get distance to grid. */
            dist = distance(y0, x0, y, x);

            /* Reject grid if outside check distance. */
            if (dist >= dist_check)
                continue;

            /* Convert and reorient grid for table access. */
            ny = 20 + 10 * (y - y0) / dist_conv;
            nx = 20 + 10 * (x - x0) / dist_conv;

            /* Illegal table access is bad. */
            if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40))
                continue;

            /* Get angle to current grid. */
            degree = get_angle_to_grid[ny][nx];

            /* Scan arcs to find the one that applies here. */
            for (i = arc_num - 1; i >= 0; i--)
            {
                if (arc[i][0] <= degree)
                {
                    max_dist = arc[i][1];

                    /* Must be within effect range. */
                    if (max_dist >= dist)
                    {
                        /* If new feature is not passable, or floor, always place it. */
                        if ((tf_has(f->flags, TF_FLOOR)) || (!tf_has(f->flags, TF_PASSABLE)))
                        {
                            square_set_feat(c, y, x, feat);

                            if (tf_has(f->flags, TF_FLOOR))
                                sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
                            else
                                sqinfo_off(c->squares[y][x].info, SQUARE_ROOM);

                            if (light)
                                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
                            else
                                square_unglow(c, y, x);
                        }

                        /* If new feature is non-floor passable terrain, place it only over floor. */
                        else
                        {
                            /* Replace old feature entirely in some cases. */
                            if (tf_has(f->flags, TF_SMOOTH))
                            {
                                if (tf_has(f_info[c->squares[y][x].feat].flags, TF_FLOOR))
                                    square_set_feat(c, y, x, feat);
                            }

                            /* Make denser in the middle. */
                            else
                            {
                                if ((tf_has(f_info[c->squares[y][x].feat].flags, TF_FLOOR)) &&
                                    (randint1(max_dist + 5) >= dist + 5))
                                {
                                    square_set_feat(c, y, x, feat);
                                }
                            }

                            /* Light grid. */
                            if (light)
                                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
                        }
                    }

                    /* Arc found. End search */
                    break;
                }
            }
        }
    }

    /*
    * If we placed floors or dungeon granite, all dungeon granite next
    * to floors needs to become outer wall.
    */
    if ((tf_has(f->flags, TF_FLOOR)) || (feat == FEAT_GRANITE))
    {
        for (y = y1 + 1; y < y2; y++)
        {
            for (x = x1 + 1; x < x2; x++)
            {
                /* Floor grids only */
                if (tf_has(f_info[c->squares[y][x].feat].flags, TF_FLOOR))
                {
                    /* Look in all directions. */
                    for (d = 0; d < 8; d++)
                    {
                        /* Extract adjacent location */
                        int yy = y + ddy_ddd[d];
                        int xx = x + ddx_ddd[d];

                        /* Join to room */
                        sqinfo_on(c->squares[yy][xx].info, SQUARE_ROOM);

                        /* Illuminate if requested. */
                        if (light)
                        sqinfo_on(c->squares[yy][xx].info, SQUARE_GLOW);

                        /* Look for dungeon granite. */
                        if (c->squares[yy][xx].feat == FEAT_GRANITE)
                        {
                            /* Mark as outer wall. */
                            set_marked_granite(c, yy, xx, SQUARE_WALL_OUTER);
                        }
                    }
                }
            }
        }
    }

    /* Success */
    return true;
}


/*
 * Find a good spot for the next room.
 *
 * y, x centre of the room
 * height, width dimensions of the room
 *
 * Find and allocate a free space in the dungeon large enough to hold
 * the room calling this function.
 *
 * We allocate space in blocks.
 *
 * Be careful to include the edges of the room in height and width!
 *
 * Return true and values for the center of the room if all went well.
 * Otherwise, return false.
 */
static bool find_space(int *y, int *x, int height, int width)
{
    int i;
    int by, bx, by1, bx1, by2, bx2;
    bool filled;

    /* Find out how many blocks we need. */
    int blocks_high = 1 + ((height - 1) / dun->block_hgt);
    int blocks_wide = 1 + ((width - 1) / dun->block_wid);

    /* We'll allow twenty-five guesses. */
    for (i = 0; i < 25; i++)
    {
        filled = false;

        /* Pick a top left block at random */
        by1 = randint0(dun->row_blocks);
        bx1 = randint0(dun->col_blocks);

        /* Extract bottom right corner block */
        by2 = by1 + blocks_high - 1;
        bx2 = bx1 + blocks_wide - 1;

        /* Never run off the screen */
        if (by1 < 0 || by2 >= dun->row_blocks) continue;
        if (bx1 < 0 || bx2 >= dun->col_blocks) continue;

        /* Verify open space */
        for (by = by1; by <= by2; by++)
        {
            for (bx = bx1; bx <= bx2; bx++)
            {
                if (dun->room_map[by][bx])
                    filled = true;
            }
        }

        /* If space filled, try again. */
        if (filled) continue;

        /* Get the location of the room */
        *y = ((by1 + by2 + 1) * dun->block_hgt) / 2;
        *x = ((bx1 + bx2 + 1) * dun->block_wid) / 2;

        /* Save the room location */
        if (dun->cent_n < z_info->level_room_max)
        {
            dun->cent[dun->cent_n].y = *y;
            dun->cent[dun->cent_n].x = *x;
            dun->cent_n++;
        }

        /* Reserve some blocks */
        for (by = by1; by <= by2; by++)
            for (bx = bx1; bx <= bx2; bx++)
                dun->room_map[by][bx] = true;

        /* Success. */
        return true;
    }

    /* Failure. */
    return false;
}


/*
 * Build a circular room (interior radius 4-7).
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success
 */
bool build_circular(struct player *p, struct chunk *c, int y0, int x0)
{
    /* Pick a room size */
    int radius = 2 + randint1(2) + randint1(3);

    /* Occasional light */
    bool light = ((c->depth <= randint1(25))? true: false);

    /* Find and reserve lots of space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, 2 * radius + 10, 2 * radius + 10))
            return false;
    }

    /* Generate outer walls and inner floors */
    fill_circle(c, y0, x0, radius + 1, 1, FEAT_GRANITE, SQUARE_WALL_OUTER, light);
    fill_circle(c, y0, x0, radius, 0, FEAT_FLOOR, SQUARE_NONE, light);

    /* Especially large circular rooms will have a middle chamber */
    if ((radius - 4 > 0) && (randint0(4) < radius - 4))
    {
        /* Choose a random direction */
        int cd, rd;

        rand_dir(&rd, &cd);

        /* Draw a room with a secret door on a random side */
        draw_rectangle(c, y0 - 2, x0 - 2, y0 + 2, x0 + 2, FEAT_GRANITE, SQUARE_WALL_INNER);
        square_set_feat(c, y0 + cd * 2, x0 + rd * 2, FEAT_SECRET);

        /* Place a treasure in the vault */
        vault_objects(p, c, y0, x0, randint0(2));

        /* Create some monsters */
        vault_monsters(p, c, y0, x0, monster_level(c->depth) + 1, randint0(3));
    }

    return true;
}


/*
 * Builds a normal rectangular room.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success
 */
bool build_simple(struct player *p, struct chunk *c, int y0, int x0)
{
    int y, x, y1, x1, y2, x2;
    int light = false;

    /* Pick a room size */
    int height = 1 + randint1(4) + randint1(3);
    int width = 1 + randint1(11) + randint1(11);

    /* Find and reserve some space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, height + 2, width + 2))
            return false;
    }

    /* Pick a room size */
    y1 = y0 - height / 2;
    x1 = x0 - width / 2;
    y2 = y1 + height - 1;
    x2 = x1 + width - 1;

    /* Occasional light */
    if (c->depth <= randint1(25)) light = true;
    
    /* Generate new room */
    generate_room(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, light);

    /* Generate outer walls and inner floors */
    draw_rectangle(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_GRANITE, SQUARE_WALL_OUTER);
    fill_rectangle(c, y1, x1, y2, x2, FEAT_FLOOR, SQUARE_NONE);

    /* Sometimes make a pillar room */
    if (one_in_(20))
    {
        for (y = y1; y <= y2; y += 2)
            for (x = x1; x <= x2; x += 2)
                set_marked_granite(c, y, x, SQUARE_WALL_INNER);
    }

    /* Sometimes make a ragged-edge room */
    else if (one_in_(50))
    {
        for (y = y1 + 2; y <= y2 - 2; y += 2)
        {
            set_marked_granite(c, y, x1, SQUARE_WALL_INNER);
            set_marked_granite(c, y, x2, SQUARE_WALL_INNER);
        }
        for (x = x1 + 2; x <= x2 - 2; x += 2)
        {
            set_marked_granite(c, y1, x, SQUARE_WALL_INNER);
            set_marked_granite(c, y2, x, SQUARE_WALL_INNER);
        }
    }

    return true;
}


/*
 * Builds an overlapping rectangular room.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success
 */
bool build_overlap(struct player *p, struct chunk *c, int y0, int x0)
{
    int y1a, x1a, y2a, x2a;
    int y1b, x1b, y2b, x2b;
    int height, width;
    int light = false;

    /* Occasional light */
    if (c->depth <= randint1(25)) light = true;

    /* Determine extents of room (a) */
    y1a = randint1(4);
    x1a = randint1(11);
    y2a = randint1(3);
    x2a = randint1(10);

    /* Determine extents of room (b) */
    y1b = randint1(3);
    x1b = randint1(10);
    y2b = randint1(4);
    x2b = randint1(11);

    /* Calculate height and width */
    height = 2 * MAX(MAX(y1a, y2a), MAX(y1b, y2b)) + 1;
    width = 2 * MAX(MAX(x1a, x2a), MAX(x1b, x2b)) + 1;

    /* Find and reserve some space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, height + 2, width + 2))
            return false;
    }

    /* locate room (a) */
    y1a = y0 - y1a;
    x1a = x0 - x1a;
    y2a = y0 + y2a;
    x2a = x0 + x2a;

    /* locate room (b) */
    y1b = y0 - y1b;
    x1b = x0 - x1b;
    y2b = y0 + y2b;
    x2b = x0 + x2b;

    /* Generate new room (a) */
    generate_room(c, y1a - 1, x1a - 1, y2a + 1, x2a + 1, light);

    /* Generate new room (b) */
    generate_room(c, y1b - 1, x1b - 1, y2b + 1, x2b + 1, light);

    /* Generate outer walls (a) */
    draw_rectangle(c, y1a - 1, x1a - 1, y2a + 1, x2a + 1, FEAT_GRANITE, SQUARE_WALL_OUTER);

    /* Generate outer walls (b) */
    draw_rectangle(c, y1b - 1, x1b - 1, y2b + 1, x2b + 1, FEAT_GRANITE, SQUARE_WALL_OUTER);

    /* Generate inner floors (a) */
    fill_rectangle(c, y1a, x1a, y2a, x2a, FEAT_FLOOR, SQUARE_NONE);

    /* Generate inner floors (b) */
    fill_rectangle(c, y1b, x1b, y2b, x2b, FEAT_FLOOR, SQUARE_NONE);

    return true;
}


/*
 * Builds a cross-shaped room.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success
 *
 * Room "a" runs north/south, and Room "b" runs east/west
 * So a "central pillar" would run from x1a,y1b to x2a,y2b.
 *
 * Note that currently, the "center" is always 3x3, but I think that the code
 * below will work for 5x5 (and perhaps even for unsymetric values like 4x3 or
 * 5x3 or 3x4 or 3x5).
 */
bool build_crossed(struct player *p, struct chunk *c, int y0, int x0)
{
    int y, x;
    int height, width;
    int y1a, x1a, y2a, x2a;
    int y1b, x1b, y2b, x2b;
    int dy, dx, wy, wx;
    int light = false;

    /* Occasional light */
    if (c->depth <= randint1(25)) light = true;

    /* Pick inner dimension */
    wy = 1;
    wx = 1;

    /* Pick outer dimension */
    dy = rand_range(3, 4);
    dx = rand_range(3, 11);

    /* Determine extents of room (a) */
    y1a = dy;
    x1a = wx;
    y2a = dy;
    x2a = wx;

    /* Determine extents of room (b) */
    y1b = wy;
    x1b = dx;
    y2b = wy;
    x2b = dx;

    /* Calculate height and width */
    height = MAX(y1a + y2a + 1, y1b + y2b + 1);
    width = MAX(x1a + x2a + 1, x1b + x2b + 1);

    /* Find and reserve some space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, height + 2, width + 2))
            return false;
    }

    /* locate room (a) */
    y1a = y0 - dy;
    x1a = x0 - wx;
    y2a = y0 + dy;
    x2a = x0 + wx;

    /* locate room (b) */
    y1b = y0 - wy;
    x1b = x0 - dx;
    y2b = y0 + wy;
    x2b = x0 + dx;
    
    /* Generate new room (a) */
    generate_room(c, y1a - 1, x1a - 1, y2a + 1, x2a + 1, light);

    /* Generate new room (b) */
    generate_room(c, y1b - 1, x1b - 1, y2b + 1, x2b + 1, light);

    /* Generate outer walls (a) */
    draw_rectangle(c, y1a - 1, x1a - 1, y2a + 1, x2a + 1, FEAT_GRANITE, SQUARE_WALL_OUTER);

    /* Generate outer walls (b) */
    draw_rectangle(c, y1b - 1, x1b - 1, y2b + 1, x2b + 1, FEAT_GRANITE, SQUARE_WALL_OUTER);

    /* Generate inner floors (a) */
    fill_rectangle(c, y1a, x1a, y2a, x2a, FEAT_FLOOR, SQUARE_NONE);

    /* Generate inner floors (b) */
    fill_rectangle(c, y1b, x1b, y2b, x2b, FEAT_FLOOR, SQUARE_NONE);

    /* Special features */
    switch (randint1(4))
    {
        /* Nothing */
        case 1: break;

        /* Large solid middle pillar */
        case 2:
        {
            /* Generate a small inner solid pillar */
            fill_rectangle(c, y1b, x1a, y2b, x2a, FEAT_GRANITE, SQUARE_WALL_INNER);

            break;
        }

        /* Inner treasure vault */
        case 3:
        {
            /* Generate a small inner vault */
            draw_rectangle(c, y1b, x1a, y2b, x2a, FEAT_GRANITE, SQUARE_WALL_INNER);

            /* Open the inner vault with a secret door */
            generate_hole(c, y1b, x1a, y2b, x2a, FEAT_SECRET);

            /* Place a treasure in the vault */
            place_object(p, c, y0, x0, object_level(c->depth), false, false, ORIGIN_SPECIAL, 0);

            /* Let's guard the treasure well */
            vault_monsters(p, c, y0, x0, monster_level(c->depth) + 2, randint0(2) + 3);

            /* Traps naturally */
            vault_traps(c, y0, x0, 4, 4, randint0(3) + 2);

            break;
        }

        /* Something else */
        case 4:
        {
            /* Occasionally pinch the center shut */
            if (one_in_(3))
            {
                /* Pinch the east/west sides */
                for (y = y1b; y <= y2b; y++)
                {
                    if (y == y0) continue;
                    set_marked_granite(c, y, x1a - 1, SQUARE_WALL_INNER);
                    set_marked_granite(c, y, x2a + 1, SQUARE_WALL_INNER);
                }

                /* Pinch the north/south sides */
                for (x = x1a; x <= x2a; x++)
                {
                    if (x == x0) continue;
                    set_marked_granite(c, y1b - 1, x, SQUARE_WALL_INNER);
                    set_marked_granite(c, y2b + 1, x, SQUARE_WALL_INNER);
                }

                /* Open sides with secret doors */
                if (one_in_(3))
                    generate_open(c, y1b - 1, x1a - 1, y2b + 1, x2a + 1, FEAT_SECRET);
            }

            /* Occasionally put a "plus" in the center */
            else if (one_in_(3))
                generate_plus(c, y1b, x1a, y2b, x2a, FEAT_GRANITE, SQUARE_WALL_INNER);

            /* Occasionally put a "pillar" in the center */
            else if (one_in_(3))
                set_marked_granite(c, y0, x0, SQUARE_WALL_INNER);

            break;
        }
    }

    return true;
}


/*
 * Build a large room with an inner room.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success.
 *
 * Possible sub-types:
 *  1 - An inner room
 *  2 - An inner room with a small inner room
 *  3 - An inner room with a pillar or pillars
 *  4 - An inner room with a checkerboard
 *  5 - An inner room with four compartments
 */
bool build_large(struct player *p, struct chunk *c, int y0, int x0)
{
    int y, x, y1, x1, y2, x2;
    int height = 9;
    int width = 23;
    int light = false;
    int mlvl = monster_level(c->depth);
    int olvl = object_level(c->depth);

    /* Occasional light */
    if (c->depth <= randint1(25)) light = true;

    /* Find and reserve some space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, height + 2, width + 2))
            return false;
    }

    /* Large room */
    y1 = y0 - height / 2;
    y2 = y0 + height / 2;
    x1 = x0 - width / 2;
    x2 = x0 + width / 2;

    /* Generate new room */
    generate_room(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, light);

    /* Generate outer walls */
    draw_rectangle(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_GRANITE, SQUARE_WALL_OUTER);

    /* Generate inner floors */
    fill_rectangle(c, y1, x1, y2, x2, FEAT_FLOOR, SQUARE_NONE);

    /* The inner room */
    y1 = y1 + 2;
    y2 = y2 - 2;
    x1 = x1 + 2;
    x2 = x2 - 2;

    /* Generate inner walls */
    draw_rectangle(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_GRANITE, SQUARE_WALL_INNER);

    /* Inner room variations */
    switch (randint1(5))
    {
        /* An inner room */
        case 1:
        {
            /* Open the inner room with a secret door and place a monster */
            generate_hole(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_SECRET);
            vault_monsters(p, c, y0, x0, mlvl + 2, 1);

            break;
        }

        /* An inner room with a small inner room */
        case 2:
        {
            /* Open the inner room with a secret door */
            generate_hole(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_SECRET);

            /* Place another inner room */
            draw_rectangle(c, y0 - 1, x0 - 1, y0 + 1, x0 + 1, FEAT_GRANITE, SQUARE_WALL_INNER);

            /* Open the inner room with a locked door */
            generate_hole(c, y0 - 1, x0 - 1, y0 + 1, x0 + 1, FEAT_CLOSED);
            for (y = y0 - 1; y <= y0 + 1; y++)
            {
                for (x = x0 - 1; x <= x0 + 1; x++)
                {
                    if (square_iscloseddoor(c, y, x))
                        square_set_door_lock(c, y, x, randint1(7));
                }
            }

            /* Monsters to guard the treasure */
            vault_monsters(p, c, y0, x0, mlvl + 2, randint1(3) + 2);

            /* Object (80%) or Stairs (20%) */
            if (magik(80))
                place_object(p, c, y0, x0, olvl, false, false, ORIGIN_SPECIAL, 0);
            else
                place_random_stairs(c, y0, x0);

            /* Traps to protect the treasure */
            vault_traps(c, y0, x0, 4, 10, 2 + randint1(3));

            break;
        }

        /* An inner room with an inner pillar or pillars */
        case 3:
        {
            /* Open the inner room with a secret door */
            generate_hole(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_SECRET);

            /* Inner pillar */
            fill_rectangle(c, y0 - 1, x0 - 1, y0 + 1, x0 + 1, FEAT_GRANITE, SQUARE_WALL_INNER);

            /* Occasionally, two more Large Inner Pillars */
            if (one_in_(2))
            {
                if (one_in_(2))
                {
                    fill_rectangle(c, y0 - 1, x0 - 7, y0 + 1, x0 - 5, FEAT_GRANITE, SQUARE_WALL_INNER);
                    fill_rectangle(c, y0 - 1, x0 + 5, y0 + 1, x0 + 7, FEAT_GRANITE, SQUARE_WALL_INNER);
                }
                else
                {
                    fill_rectangle(c, y0 - 1, x0 - 6, y0 + 1, x0 - 4, FEAT_GRANITE, SQUARE_WALL_INNER);
                    fill_rectangle(c, y0 - 1, x0 + 4, y0 + 1, x0 + 6, FEAT_GRANITE, SQUARE_WALL_INNER);
                }
            }

            /* Occasionally, some Inner rooms */
            if (one_in_(3))
            {
                /* Inner rectangle */
                draw_rectangle(c, y0 - 1, x0 - 5, y0 + 1, x0 + 5, FEAT_GRANITE, SQUARE_WALL_INNER);

                /* Secret doors (random top/bottom) */
                place_secret_door(c, y0 - 3 + (randint1(2) * 2), x0 - 3);
                place_secret_door(c, y0 - 3 + (randint1(2) * 2), x0 + 3);

                /* Monsters */
                vault_monsters(p, c, y0, x0 - 2, mlvl + 2, randint1(2));
                vault_monsters(p, c, y0, x0 + 2, mlvl + 2, randint1(2));

                /* Objects */
                if (one_in_(3))
                    place_object(p, c, y0, x0 - 2, olvl, false, false, ORIGIN_SPECIAL, 0);
                if (one_in_(3))
                    place_object(p, c, y0, x0 + 2, olvl, false, false, ORIGIN_SPECIAL, 0);
            }

            break;
        }

        /* An inner room with a checkerboard */
        case 4:
        {
            /* Open the inner room with a secret door */
            generate_hole(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_SECRET);

            /* Checkerboard */
            for (y = y1; y <= y2; y++)
            {
                for (x = x1; x <= x2; x++)
                {
                    if ((x + y) & 0x01)
                        set_marked_granite(c, y, x, SQUARE_WALL_INNER);
                }
            }

            /* Monsters just love mazes. */
            vault_monsters(p, c, y0, x0 - 5, mlvl + 2, randint1(3));
            vault_monsters(p, c, y0, x0 + 5, mlvl + 2, randint1(3));

            /* Traps make them entertaining. */
            vault_traps(c, y0, x0 - 3, 2, 8, randint1(3));
            vault_traps(c, y0, x0 + 3, 2, 8, randint1(3));

            /* Mazes should have some treasure too. */
            vault_objects(p, c, y0, x0, 3);

            break;
        }

        /* Four small rooms. */
        case 5:
        {
            /* Inner "cross" */
            generate_plus(c, y1, x1, y2, x2, FEAT_GRANITE, SQUARE_WALL_INNER);

            /* Doors into the rooms */
            if (magik(50))
            {
                int i = randint1(10);

                place_secret_door(c, y1 - 1, x0 - i);
                place_secret_door(c, y1 - 1, x0 + i);
                place_secret_door(c, y2 + 1, x0 - i);
                place_secret_door(c, y2 + 1, x0 + i);
            }
            else
            {
                int i = randint1(3);

                place_secret_door(c, y0 + i, x1 - 1);
                place_secret_door(c, y0 - i, x1 - 1);
                place_secret_door(c, y0 + i, x2 + 1);
                place_secret_door(c, y0 - i, x2 + 1);
            }

            /* Treasure, centered at the center of the cross */
            vault_objects(p, c, y0, x0, 2 + randint1(2));

            /* Gotta have some monsters */
            vault_monsters(p, c, y0 + 1, x0 - 4, mlvl + 2, randint1(4));
            vault_monsters(p, c, y0 + 1, x0 + 4, mlvl + 2, randint1(4));
            vault_monsters(p, c, y0 - 1, x0 - 4, mlvl + 2, randint1(4));
            vault_monsters(p, c, y0 - 1, x0 + 4, mlvl + 2, randint1(4));

            break;
        }
    }

    return true;
}


/*
 * Hook for picking monsters appropriate to a nest/pit or region.
 *
 * race the race being tested for inclusion
 *
 * Returns if the race is acceptable.
 *
 * Requires dun->pit_type to be set.
 */
bool mon_pit_hook(struct monster_race *race)
{
    bool match_base = true;
    bool match_color = true;

    my_assert(dun->pit_type);
    my_assert(race);

    /* Decline unique monsters */
    if (rf_has(race->flags, RF_UNIQUE)) return false;

    /* Decline breeders */
    if (rf_has(race->flags, RF_MULTIPLY)) return false;

    /* Decline monsters that can kill other monsters */
    if (rf_has(race->flags, RF_KILL_BODY)) return false;

    /* Hack -- decline PWMANG_BASE dragons */
    if (rf_has(race->flags, RF_DRAGON) && rf_has(race->flags, RF_PWMANG_BASE)) return false;

    if (!rf_is_subset(race->flags, dun->pit_type->flags)) return false;
    if (rf_is_inter(race->flags, dun->pit_type->forbidden_flags)) return false;
    if (!rsf_is_subset(race->spell_flags, dun->pit_type->spell_flags)) return false;
    if (rsf_is_inter(race->spell_flags, dun->pit_type->forbidden_spell_flags)) return false;
    if (dun->pit_type->forbidden_monsters)
    {
        struct pit_forbidden_monster *monster;

        for (monster = dun->pit_type->forbidden_monsters; monster; monster = monster->next)
        {
            if (race == monster->race)
                return false;
        }
    }
    if (dun->pit_type->bases)
    {
        struct pit_monster_profile *bases;

        match_base = false;

        for (bases = dun->pit_type->bases; bases; bases = bases->next)
        {
            if (race->base == bases->base)
                match_base = true;
        }
    }

    if (dun->pit_type->colors)
    {
        struct pit_color_profile *colors;

        match_color = false;

        for (colors = dun->pit_type->colors; colors; colors = colors->next)
        {
            if (race->d_attr == colors->color)
                match_color = true;
        }
    }

    return (match_base && match_color);
}


/*
 * Pick a type of monster for pits (or other purposes), based on the level.
 *
 * We scan through all pit profiles, and for each one generate a random depth
 * using a normal distribution, with the mean given in pit.txt, and a
 * standard deviation of 10. Then we pick the profile that gave us a depth that
 * is closest to the player's actual depth.
 *
 * Sets dun->pit_type, which is required for mon_pit_hook.
 * depth is the pit profile depth to aim for in selection
 * type is 1 for pits, 2 for nests, 0 for any profile
 */
void set_pit_type(int depth, int type)
{
    int i;
    int pit_idx = 0;

    /* Hack -- set initial distance large */
    int pit_dist = 999;

    for (i = 0; i < z_info->pit_max; i++)
    {
        int offset, dist;
        struct pit_profile *pit = &pit_info[i];

        /* Skip empty pits or pits of the wrong room type */
        if (type && (!pit->name || (pit->room_type != type))) continue;

        offset = Rand_normal(pit->ave, 10);
        dist = ABS(offset - depth);

        if ((dist < pit_dist) && one_in_(pit->rarity))
        {
            /* This pit is the closest so far */
            pit_idx = i;
            pit_dist = dist;
        }
    }

    dun->pit_type = &pit_info[pit_idx];
}


/*
 * Build a monster nest
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success.
 *
 * A monster nest consists of a rectangular moat around a room containing
 * monsters of a given type.
 *
 * The monsters are chosen from a set of 64 randomly selected monster races,
 * to allow the nest creation to fail instead of having "holes".
 *
 * Note the use of the "get_mon_num_prep()" function to prepare the
 * "monster allocation table" in such a way as to optimize the selection
 * of "appropriate" non-unique monsters for the nest.
 *
 * The available monster nests are specified in gamedata/pit.txt.
 *
 * Note that get_mon_num() function can fail, in which case the nest will be
 * empty, and will not affect the level rating.
 *
 * Monster nests will never contain unique monsters.
 */
bool build_nest(struct player *p, struct chunk *c, int y0, int x0)
{
    int y, x, y1, x1, y2, x2;
    int i;
    int alloc_obj;
    struct monster_race *what[64];
    bool empty = false;
    int light = false;
    int size_vary = randint0(4);
    int height = 9;
    int width = 11 + 2 * size_vary;

    /* Find and reserve some space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, height + 2, width + 2))
            return false;
    }

    /* Large room */
    y1 = y0 - height / 2;
    y2 = y0 + height / 2;
    x1 = x0 - width / 2;
    x2 = x0 + width / 2;
    
    /* Generate new room */
    generate_room(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, light);

    /* Generate outer walls */
    draw_rectangle(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_GRANITE, SQUARE_WALL_OUTER);

    /* Generate inner floors */
    fill_rectangle(c, y1, x1, y2, x2, FEAT_FLOOR, SQUARE_NONE);

    /* Advance to the center room */
    y1 = y1 + 2;
    y2 = y2 - 2;
    x1 = x1 + 2;
    x2 = x2 - 2;

    /* PWMAngband -- generate pit floors */
    fill_rectangle(c, y1, x1, y2, x2, FEAT_FLOOR_PIT, SQUARE_NONE);

    /* Generate inner walls */
    /* PWMAngband -- make them permanent to prevent PASS_WALL/KILL_WALL monsters from escaping */
    draw_rectangle(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_PERM_FAKE, SQUARE_NONE);

    /* Open the inner room with a secret door */
    generate_hole(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_SECRET);

    /* PWMAngband -- make it "icky" and "NO_TELEPORT" to prevent teleportation */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            sqinfo_on(c->squares[y][x].info, SQUARE_VAULT);
            sqinfo_on(c->squares[y][x].info, SQUARE_NO_TELEPORT);
        }
    }

    /* Decide on the pit type */
    set_pit_type(c->depth, 2);

    /* Chance of objects on the floor */
    alloc_obj = dun->pit_type->obj_rarity;

    /* Prepare allocation table */
    get_mon_num_prep(mon_pit_hook);

    /* Pick some monster types */
    for (i = 0; i < 64; i++)
    {
        /* Get a (hard) monster type */
        what[i] = get_mon_num(c, monster_level(c->depth) + 10);

        /* Notice failure */
        if (!what[i]) empty = true;
    }

    /* Prepare allocation table */
    get_mon_num_prep(NULL);

    /* Oops */
    if (empty) return false;

    /* Increase the level rating */
    c->mon_rating += (size_vary + dun->pit_type->ave / 20);

    /* Place some monsters */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Figure out what monster is being used, and place that monster */
            struct monster_race *race = what[randint0(64)];

            place_new_monster(p, c, y, x, race, 0, ORIGIN_DROP_PIT);

            /* Occasionally place an item, making it good 1/3 of the time */
            if (randint0(100) < alloc_obj)
            {
                place_object(p, c, y, x, object_level(c->depth) + 10, one_in_(3), false,
                    ORIGIN_PIT, 0);
            }
        }
    }

    return true;
}


/*
 * Build a monster pit
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success.
 *
 * Monster pits are laid-out similarly to monster nests.
 *
 * The available monster pits are specified in gamedata/pit.txt.
 *
 * The inside room in a monster pit appears as shown below, where the
 * actual monsters in each location depend on the type of the pit
 *
 *   #############
 *   #11000000011#
 *   #01234543210#
 *   #01236763210#
 *   #01234543210#
 *   #11000000011#
 *   #############
 *
 * Note that the monsters in the pit are chosen by using get_mon_num() to
 * request 16 "appropriate" monsters, sorting them by level, and using the
 * "even" entries in this sorted list for the contents of the pit.
 *
 * Note the use of get_mon_num_prep() to prepare the monster allocation
 * table in such a way as to optimize the selection of appropriate non-unique
 * monsters for the pit.
 *
 * The get_mon_num() function can fail, in which case the pit will be empty,
 * and will not effect the level rating.
 *
 * Like monster nests, monster pits will never contain unique monsters.
 */
bool build_pit(struct player *p, struct chunk *c, int y0, int x0)
{
    struct monster_race *what[16];
    int i, j, y, x, y1, x1, y2, x2;
    bool empty = false;
    int light = false;
    int alloc_obj;
    int height = 9;
    int width = 15;

    /* Find and reserve some space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, height + 2, width + 2))
            return false;
    }

    /* Large room */
    y1 = y0 - height / 2;
    y2 = y0 + height / 2;
    x1 = x0 - width / 2;
    x2 = x0 + width / 2;

    /* Generate new room, outer walls and inner floor */
    generate_room(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, light);
    draw_rectangle(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_GRANITE, SQUARE_WALL_OUTER);
    fill_rectangle(c, y1, x1, y2, x2, FEAT_FLOOR, SQUARE_NONE);

    /* Advance to the center room */
    y1 = y1 + 2;
    y2 = y2 - 2;
    x1 = x1 + 2;
    x2 = x2 - 2;

    /* PWMAngband -- generate pit floors */
    fill_rectangle(c, y1, x1, y2, x2, FEAT_FLOOR_PIT, SQUARE_NONE);

    /* Generate inner walls, and open with a secret door */
    /* PWMAngband -- make them permanent to prevent PASS_WALL/KILL_WALL monsters from escaping */
    draw_rectangle(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_PERM_FAKE, SQUARE_NONE);
    generate_hole(c, y1 - 1, x1 - 1, y2 + 1, x2 + 1, FEAT_SECRET);

    /* PWMAngband -- make it "icky" and "NO_TELEPORT" to prevent teleportation */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            sqinfo_on(c->squares[y][x].info, SQUARE_VAULT);
            sqinfo_on(c->squares[y][x].info, SQUARE_NO_TELEPORT);
        }
    }

    /* Decide on the pit type */
    set_pit_type(c->depth, 1);

    /* Chance of objects on the floor */
    alloc_obj = dun->pit_type->obj_rarity;

    /* Prepare allocation table */
    get_mon_num_prep(mon_pit_hook);

    /* Pick some monster types */
    for (i = 0; i < 16; i++)
    {
        /* Get a (hard) monster type */
        what[i] = get_mon_num(c, monster_level(c->depth) + 10);

        /* Notice failure */
        if (!what[i]) empty = true;
    }

    /* Prepare allocation table */
    get_mon_num_prep(NULL);

    /* Oops */
    if (empty) return false;

    /* Sort the entries */
    for (i = 0; i < 16 - 1; i++)
    {
        for (j = 0; j < 16 - 1; j++)
        {
            int i1 = j;
            int i2 = j + 1;
            int p1 = what[i1]->level;
            int p2 = what[i2]->level;

            /* Bubble */
            if (p1 > p2)
            {
                struct monster_race *tmp = what[i1];

                what[i1] = what[i2];
                what[i2] = tmp;
            }
        }
    }

    /* Select every other entry */
    for (i = 0; i < 8; i++) what[i] = what[i * 2];

    /* Increase the level rating */
    c->mon_rating += (3 + dun->pit_type->ave / 20);

    /* Top and bottom rows (middle) */
    for (x = x0 - 3; x <= x0 + 3; x++)
    {
        place_new_monster(p, c, y0 - 2, x, what[0], 0, ORIGIN_DROP_PIT);
        place_new_monster(p, c, y0 + 2, x, what[0], 0, ORIGIN_DROP_PIT);
    }

    /* Corners */
    for (x = x0 - 5; x <= x0 - 4; x++)
    {
        place_new_monster(p, c, y0 - 2, x, what[1], 0, ORIGIN_DROP_PIT);
        place_new_monster(p, c, y0 + 2, x, what[1], 0, ORIGIN_DROP_PIT);
    }

    for (x = x0 + 4; x <= x0 + 5; x++)
    {
        place_new_monster(p, c, y0 - 2, x, what[1], 0, ORIGIN_DROP_PIT);
        place_new_monster(p, c, y0 + 2, x, what[1], 0, ORIGIN_DROP_PIT);
    }

    /* Middle columns */
    for (y = y0 - 1; y <= y0 + 1; y++)
    {
        place_new_monster(p, c, y, x0 - 5, what[0], 0, ORIGIN_DROP_PIT);
        place_new_monster(p, c, y, x0 + 5, what[0], 0, ORIGIN_DROP_PIT);

        place_new_monster(p, c, y, x0 - 4, what[1], 0, ORIGIN_DROP_PIT);
        place_new_monster(p, c, y, x0 + 4, what[1], 0, ORIGIN_DROP_PIT);

        place_new_monster(p, c, y, x0 - 3, what[2], 0, ORIGIN_DROP_PIT);
        place_new_monster(p, c, y, x0 + 3, what[2], 0, ORIGIN_DROP_PIT);

        place_new_monster(p, c, y, x0 - 2, what[3], 0, ORIGIN_DROP_PIT);
        place_new_monster(p, c, y, x0 + 2, what[3], 0, ORIGIN_DROP_PIT);
    }

    /* Corners around the middle monster */
    place_new_monster(p, c, y0 - 1, x0 - 1, what[4], 0, ORIGIN_DROP_PIT);
    place_new_monster(p, c, y0 - 1, x0 + 1, what[4], 0, ORIGIN_DROP_PIT);
    place_new_monster(p, c, y0 + 1, x0 - 1, what[4], 0, ORIGIN_DROP_PIT);
    place_new_monster(p, c, y0 + 1, x0 + 1, what[4], 0, ORIGIN_DROP_PIT);

    /* Above/Below the center monster */
    place_new_monster(p, c, y0 + 1, x0, what[5], 0, ORIGIN_DROP_PIT);
    place_new_monster(p, c, y0 - 1, x0, what[5], 0, ORIGIN_DROP_PIT);

    /* Next to the center monster */
    place_new_monster(p, c, y0, x0 + 1, what[6], 0, ORIGIN_DROP_PIT);
    place_new_monster(p, c, y0, x0 - 1, what[6], 0, ORIGIN_DROP_PIT);

    /* Center monster */
    place_new_monster(p, c, y0, x0, what[7], 0, ORIGIN_DROP_PIT);

    /* Place some objects */
    for (y = y0 - 2; y <= y0 + 2; y++)
    {
        for (x = x0 - 9; x <= x0 + 9; x++)
        {
            /* Occasionally place an item, making it good 1/3 of the time */
            if (randint0(100) < alloc_obj)
            {
                place_object(p, c, y, x, object_level(c->depth) + 10, one_in_(3), false,
                    ORIGIN_PIT, 0);
            }
        }
    }

    return true;
}


/*
 * Build a room template from its string representation.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 * ymax, xmax the room dimensions
 * doors the door position
 * data the room template text description
 * tval the object type for any included objects
 *
 * Returns success.
 */
static bool build_room_template(struct player *p, struct chunk *c, int y0, int x0, int ymax,
    int xmax, int doors, const char *data, int tval)
{
    int dx, dy, x, y, rnddoors, doorpos;
    const char *t;
    bool rndwalls, light;
    int olvl, mlvl;

    my_assert(c);

    olvl = object_level(c->depth);
    mlvl = monster_level(c->depth);

    /* Occasional light */
    light = ((c->depth <= randint1(25))? true: false);

    /*
     * Set the random door position here so it generates doors in all squares
     * marked with the same number
     */
    rnddoors = randint1(doors);

    /* Decide whether optional walls will be generated this time */
    rndwalls = (one_in_(2)? true: false);

    /* Find and reserve some space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, ymax + 2, xmax + 2))
            return false;
    }

    /* Place dungeon features and objects */
    for (t = data, dy = 0; (dy < ymax) && *t; dy++)
    {
        for (dx = 0; (dx < xmax) && *t; dx++, t++)
        {
            /* Extract the location */
            x = x0 - (xmax / 2) + dx;
            y = y0 - (ymax / 2) + dy;

            /* Skip non-grids */
            if (*t == ' ') continue;

            /* Lay down a floor */
            square_set_feat(c, y, x, FEAT_FLOOR);

            /* Debugging assertion */
            my_assert(square_isempty(c, y, x));

            /* Analyze the grid */
            switch (*t)
            {
                case '%': set_marked_granite(c, y, x, SQUARE_WALL_OUTER); break;
                case '#': set_marked_granite(c, y, x, SQUARE_WALL_INNER); break;
                case '+': place_secret_door(c, y, x); break;
                case 'x':
                {
                    /* If optional walls are generated, put a wall in this square */
                    if (rndwalls) set_marked_granite(c, y, x, SQUARE_WALL_INNER);
                    break;
                }
                case '(':
                {
                    /* If optional walls are generated, put a door in this square */
                    if (rndwalls) place_secret_door(c, y, x);
                    break;
                }
                case ')':
                {
                    /* If no optional walls are generated, put a door in this square */
                    if (!rndwalls)
                        place_secret_door(c, y, x);
                    else
                        set_marked_granite(c, y, x, SQUARE_WALL_INNER);
                    break;
                }
                case '8':
                {
                    /* Put something nice in this square: Object (80%) or Stairs (20%) */
                    if (magik(80))
                        place_object(p, c, y, x, olvl, false, false, ORIGIN_SPECIAL, 0);
                    else
                        place_random_stairs(c, y, x);

                    /* Some monsters to guard it */
                    vault_monsters(p, c, y, x, mlvl + 2, randint0(2) + 3);

                    /* And some traps too */
                    vault_traps(c, y, x, 4, 4, randint0(3) + 2);

                    break;
                }
                case '9':
                {
                    /* Create some interesting stuff nearby */

                    /* A few monsters */
                    vault_monsters(p, c, y - 3, x - 3, mlvl + randint0(2), randint1(2));
                    vault_monsters(p, c, y + 3, x + 3, mlvl + randint0(2), randint1(2));

                    /* And maybe a bit of treasure */
                    if (one_in_(2)) vault_objects(p, c, y - 2, x + 2, 1 + randint0(2));
                    if (one_in_(2)) vault_objects(p, c, y + 2, x - 2, 1 + randint0(2));

                    break;
                }
                case '*':
                {
                    /* Place an object of the template's specified tval */
                    place_object(p, c, y, x, olvl, false, false, ORIGIN_SPECIAL, tval);
                    break;
                }
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                {
                    /* Check if this is chosen random door position */
                    doorpos = (int)(*t - '0');

                    if (doorpos == rnddoors)
                        place_secret_door(c, y, x);
                    else
                        set_marked_granite(c, y, x, SQUARE_WALL_INNER);

                    break;
                }
            }

            /* Part of a room */
            sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
            if (light)
                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
        }
    }

    return true;
}


/*
 * Helper function for building room templates.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 * typ the room template type (currently unused)
 *
 * Returns success.
 */
static bool build_room_template_type(struct player *p, struct chunk *c, int y0, int x0, int typ)
{
    struct room_template *t_ptr = random_room_template(typ);

    if (t_ptr == NULL) return false;

    /* Build the room */
    if (!build_room_template(p, c, y0, x0, t_ptr->hgt, t_ptr->wid, t_ptr->dor, t_ptr->text,
        t_ptr->tval))
    {
        return false;
    }

    return true;
}


/*
 * Build a template room
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success.
 */
bool build_template(struct player *p, struct chunk *c, int y0, int x0)
{
    /* All room templates currently have type 1 */
    return build_room_template_type(p, c, y0, x0, 1);
}


/*
 * Build a vault from its string representation.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 * v pointer to the vault template
 *
 * Returns success.
 */
bool build_vault(struct player *p, struct chunk *c, int y0, int x0, struct vault *v)
{
    const char *data = v->text;
    int y1, x1, y2, x2;
    int x, y, races = 0;
    const char *t;
    char racial_symbol[30] = "";
    bool icky;
    int olvl;

    my_assert(c);

    /* Find and reserve some space in the dungeon. Get center of room. */
    if ((y0 >= c->height) || (x0 >= c->width))
    {
        if (!find_space(&y0, &x0, v->hgt + 2, v->wid + 2))
            return false;
    }

    /* Get the room corners */
    y1 = y0 - (v->hgt / 2);
    x1 = x0 - (v->wid / 2);
    y2 = y1 + v->hgt - 1;
    x2 = x1 + v->wid - 1;

    /* No random monsters in vaults. */
    generate_mark(c, y1, x1, y2, x2, SQUARE_MON_RESTRICT);

    olvl = object_level(c->depth);

    /* Hack -- don't generate if we go out of bounds or if there is already something there */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Be sure we are "in bounds" */
            if (!square_in_bounds_fully(c, y, x)) return false;

            /* No object */
            if (c->squares[y][x].obj) return false;

            /* Skip the DM */
            if ((c->squares[y][x].mon < 0) && is_dm_p(player_get(0 - c->squares[y][x].mon)))
                continue;

            /* No monster/player */
            if (c->squares[y][x].mon) return false;
        }
    }

    /* Place dungeon features and objects */
    for (t = data, y = y1; (y <= y2) && *t; y++)
    {
        for (x = x1; (x <= x2) && *t; x++, t++)
        {
            /* Skip non-grids */
            if (*t == ' ') continue;

            /* Lay down a floor */
            square_set_feat(c, y, x, FEAT_FLOOR);

            /* By default vault squares are marked icky */
            icky = true;

            /* Analyze the grid */
            switch (*t)
            {
                case '%':
                {
                    /*
                     * In this case, the square isn't really part of the
                     * vault, but rather is part of the "door step" to the
                     * vault. We don't mark it icky so that the tunneling
                     * code knows its allowed to remove this wall.
                     */
                    set_marked_granite(c, y, x, SQUARE_WALL_OUTER);
                    icky = false;
                    break;
                }

                /* Inner granite wall */
                case '#': set_marked_granite(c, y, x, SQUARE_WALL_INNER); break;

                /* Permanent wall */
                case '@':
                {
                    square_set_feat(c, y, x, FEAT_PERM);

                    /* PWMAngband: mark the wall as an "inner" wall */
                    sqinfo_on(c->squares[y][x].info, SQUARE_WALL_INNER);
                    break;
                }

                /* Gold seam */
                case '*':
                {
                    square_set_feat(c, y, x, (one_in_(2)? FEAT_MAGMA_K: FEAT_QUARTZ_K));
                    break;
                }

                /* Rubble */
                case ':': square_set_feat(c, y, x, FEAT_RUBBLE); break;

                /* Secret door */
                case '+': place_secret_door(c, y, x); break;

                /* Trap */
                case '^': square_add_trap(c, y, x); break;

                /* Treasure or a trap */
                case '&':
                {
                    if (magik(75))
                        place_object(p, c, y, x, olvl, false, false, ORIGIN_VAULT, 0);
                    else
                        square_add_trap(c, y, x);
                    break;
                }

                /* Stairs */
                case '<':
                {
                    if (cfg_limit_stairs < 2) square_set_feat(c, y, x, FEAT_LESS);
                    break;
                }
                case '>':
                {
                    /* No down stairs at bottom */
                    if (c->depth == z_info->max_depth - 1)
                    {
                        if (cfg_limit_stairs < 2) square_set_feat(c, y, x, FEAT_LESS);
                    }
                    else
                        square_set_feat(c, y, x, FEAT_MORE);
                    break;
                }
            }

            /* Part of a vault */
            sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
            if (icky) sqinfo_on(c->squares[y][x].info, SQUARE_VAULT);
        }
    }

    /* Place dungeon monsters and objects */
    for (t = data, y = y1; (y <= y2) && *t; y++)
    {
        for (x = x1; (x <= x2) && *t; x++, t++)
        {
            /* Skip non-grids */
            if (*t == ' ') continue;

            /* Most alphabetic characters signify monster races. */
            if (isalpha(*t) && (*t != 'x') && (*t != 'X'))
            {
                /* If the symbol is not yet stored, ... */
                if (!strchr(racial_symbol, *t))
                {
                    /* ... store it for later processing. */
                    if (races < 30) racial_symbol[races++] = *t;
                }
            }

            /* Otherwise, analyze the symbol */
            else switch (*t)
            {
                /* An ordinary monster, object (sometimes good), or trap. */
                case '1':
                {
                    if (one_in_(2))
                    {
                        pick_and_place_monster(p, c, y, x, monster_level(c->depth),
                            MON_SLEEP | MON_GROUP, ORIGIN_DROP_VAULT);
                    }
                    else if (one_in_(2))
                    {
                        place_object(p, c, y, x, olvl, (one_in_(8)? true: false), false,
                            ORIGIN_VAULT, 0);
                    }
                    else
                        square_add_trap(c, y, x);
                    break;
                }

                /* Slightly out of depth monster. */
                case '2':
                {
                    pick_and_place_monster(p, c, y, x, monster_level(c->depth) + 5,
                        MON_SLEEP | MON_GROUP, ORIGIN_DROP_VAULT);
                    break;
                }

                /* Slightly out of depth object. */
                case '3':
                {
                    place_object(p, c, y, x, olvl + 3, false, false, ORIGIN_VAULT, 0);
                    break;
                }

                /* Monster and/or object */
                case '4':
                {
                    if (one_in_(2))
                        pick_and_place_monster(p, c, y, x, monster_level(c->depth) + 3,
                            MON_SLEEP | MON_GROUP, ORIGIN_DROP_VAULT);
                    if (one_in_(2))
                        place_object(p, c, y, x, olvl + 7, false, false, ORIGIN_VAULT, 0);
                    break;
                }

                /* Out of depth object. */
                case '5':
                {
                    place_object(p, c, y, x, olvl + 7, false, false, ORIGIN_VAULT, 0);
                    break;
                }

                /* Out of depth monster. */
                case '6':
                {
                    pick_and_place_monster(p, c, y, x, monster_level(c->depth) + 11,
                        MON_SLEEP | MON_GROUP, ORIGIN_DROP_VAULT);
                    break;
                }

                /* Very out of depth object. */
                case '7':
                {
                    place_object(p, c, y, x, olvl + 15, false, false, ORIGIN_VAULT, 0);
                    break;
                }

                /* Very out of depth monster. */
                case '0':
                {
                    pick_and_place_monster(p, c, y, x, monster_level(c->depth) + 20,
                        MON_SLEEP | MON_GROUP, ORIGIN_DROP_VAULT);
                    break;
                }

                /* Meaner monster, plus treasure */
                case '9':
                {
                    pick_and_place_monster(p, c, y, x, monster_level(c->depth) + 9,
                        MON_SLEEP | MON_GROUP, ORIGIN_DROP_VAULT);
                    place_object(p, c, y, x, olvl + 7, true, false, ORIGIN_VAULT, 0);
                    break;
                }

                /* Nasty monster and treasure */
                case '8':
                {
                    pick_and_place_monster(p, c, y, x, monster_level(c->depth) + 40,
                        MON_SLEEP | MON_GROUP, ORIGIN_DROP_VAULT);
                    place_object(p, c, y, x, olvl + 20, true, true, ORIGIN_VAULT, 0);
                    break;
                }

                /* A chest. */
                case '~':
                {
                    place_object(p, c, y, x, olvl + 5, true, true, ORIGIN_VAULT, TV_CHEST);
                    break;
                }

                /* Treasure. */
                case '$':
                {
                    place_gold(p, c, y, x, olvl, ORIGIN_VAULT);
                    break;
                }

                /* Armour. */
                case ']':
                {
                    int temp = (one_in_(3)? randint0(9): randint0(8));

                    place_object(p, c, y, x, olvl + 3, true, false, ORIGIN_VAULT, TV_BOOTS + temp);
                    break;
                }

                /* Weapon (PWMAngband: allow diggers and mage staves). */
                case '|':
                {
                    int temp = randint0(6);

                    place_object(p, c, y, x, olvl + 3, true, false, ORIGIN_VAULT, TV_BOW + temp);
                    break;
                }

                /* Ring. */
                case '=':
                {
                    place_object(p, c, y, x, olvl + 3, one_in_(4), false, ORIGIN_VAULT, TV_RING);
                    break;
                }

                /* Amulet. */
                case '"':
                {
                    place_object(p, c, y, x, olvl + 3, one_in_(4), false, ORIGIN_VAULT, TV_AMULET);
                    break;
                }

                /* Potion. */
                case '!':
                {
                    place_object(p, c, y, x, olvl + 3, one_in_(4), false, ORIGIN_VAULT, TV_POTION);
                    break;
                }

                /* Scroll. */
                case '?':
                {
                    place_object(p, c, y, x, olvl + 3, one_in_(4), false, ORIGIN_VAULT, TV_SCROLL);
                    break;
                }

                /* Staff. */
                case '_':
                {
                    place_object(p, c, y, x, olvl + 3, one_in_(4), false, ORIGIN_VAULT, TV_STAFF);
                    break;
                }

                /* Wand or rod. */
                case '-':
                {
                    place_object(p, c, y, x, olvl + 3, one_in_(4), false, ORIGIN_VAULT,
                        (one_in_(2)? TV_WAND: TV_ROD));
                    break;
                }

                /* Food. */
                case ',':
                {
                    place_object(p, c, y, x, olvl + 3, one_in_(4), false, ORIGIN_VAULT, TV_FOOD);
                    break;
                }
            }
        }
    }

    /* Place specified monsters */
    /* get_vault_monsters(c, racial_symbol, v->typ, data, y1, y2, x1, x2); */

    /* Success */
    return true;
}


/*
 * Helper function for building vaults.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 * typ the vault type
 *
 * Returns success.
 */
static bool build_vault_type(struct player *p, struct chunk *c, int y0, int x0, const char *typ)
{
    struct vault *v = random_vault(c->depth, typ);

    if (v == NULL) return false;

    /* Hack -- medium vaults with a high rating have a rarity of (rating / 10) */
    if (streq(v->typ, "Medium vault") && !one_in_(v->rat / 10)) return false;

    /* Build the vault */
    if (!build_vault(p, c, y0, x0, v))
        return false;

    /* Boost the rating */
    c->mon_rating += v->rat;

    return true;
}


/*
 * Build a lesser vault.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success.
 */
bool build_lesser_vault(struct player *p, struct chunk *c, int y0, int x0)
{
    return build_vault_type(p, c, y0, x0, "Lesser vault");
}


/*
 *  Build a medium vault.
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success.
 */
bool build_medium_vault(struct player *p, struct chunk *c, int y0, int x0)
{
    return build_vault_type(p, c, y0, x0, "Medium vault");
}


/*
 * Build a greater vault
 *
 * p the player
 * c the chunk the room is being built in
 * y0, x0 co-ordinates of the centre; out of chunk bounds invoke find_space()
 *
 * Returns success.
 *
 * Since Greater Vaults are so large (4x6 blocks, in a 6x18 dungeon) there is
 * a 63% chance that a randomly chosen quadrant to start a GV on won't work.
 * To balance this, we give Greater Vaults an artificially high probability
 * of being attempted, and then in this function use a depth check to cancel
 * vault creation except at deep depths.
 *
 * The following code should make a greater vault with frequencies:
 *
 *  dlvl  freq
 *  100+  18.0%
 *  90-99 16.0 - 18.0%
 *  80-89 10.0 - 11.0%
 *  70-79  5.7 -  6.5%
 *  60-69  3.3 -  3.8%
 *  50-59  1.8 -  2.1%
 *  0-49   0.0 -  1.0%

 */
bool build_greater_vault(struct player *p, struct chunk *c, int y0, int x0)
{
    int i;
    int numerator = 2;
    int denominator = 3;

    /* Only try to build a GV as the first room. */
    if (dun->cent_n > 0) return false;

    /* Level 90+ has a 2/3 chance, level 80-89 has 4/9, ... */
    for (i = 90; i > c->depth; i -= 10)
    {
        numerator *= 2;
        denominator *= 3;
    }

    /* Attempt to pass the depth check and build a GV */
    if (randint0(denominator) >= numerator) return false;

    return build_vault_type(p, c, y0, x0, "Greater vault");
}


/*
 * Attempt to build a room of the given type at the given block
 *
 * p the player
 * c the chunk the room is being built in
 * by0, bx0 block co-ordinates of the top left block
 * profile the profile of the rooom we're trying to build
 * finds_own_space whether we are allowing the room to place itself
 *
 * Returns success.
 *
 * Note that this code assumes that profile height and width are the maximum
 * possible grid sizes, and then allocates a number of blocks that will always
 * contain them.
 *
 * Note that we restrict the number of pits/nests to reduce
 * the chance of overflowing the monster list during level creation.
 */
bool room_build(struct player *p, struct chunk *c, int by0, int bx0, struct room_profile profile,
    bool finds_own_space)
{
    /* Extract blocks */
    int by1 = by0;
    int bx1 = bx0;
    int by2 = by0 + profile.height / dun->block_hgt;
    int bx2 = bx0 + profile.width / dun->block_wid;

    int y, x;
    int by, bx;

    /* Enforce the room profile's minimum depth */
    if (c->depth < profile.level) return false;

    /* Only allow at most two pit/nests room per level */
    if ((dun->pit_num >= z_info->level_pit_max) && profile.pit) return false;

    /* Expand the number of blocks if we might overflow */
    if (profile.height % dun->block_hgt) by2++;
    if (profile.width % dun->block_wid) bx2++;

    /* Does the profile allocate space, or the room find it? */
    if (finds_own_space)
    {
        /* Try to build a room, pass silly place so room finds its own */
        if (!profile.builder(p, c, c->height, c->width)) return false;
    }
    else
    {
        /* Never run off the screen */
        if ((by1 < 0) || (by2 >= dun->row_blocks)) return false;
        if ((bx1 < 0) || (bx2 >= dun->col_blocks)) return false;

        /* Verify open space */
        for (by = by1; by <= by2; by++)
        {
            for (bx = bx1; bx <= bx2; bx++)
            {
                /* Previous rooms prevent new ones */
                if (dun->room_map[by][bx]) return false;
            }
        }

        /* Get the location of the room */
        y = ((by1 + by2 + 1) * dun->block_hgt) / 2;
        x = ((bx1 + bx2 + 1) * dun->block_wid) / 2;

        /* Try to build a room */
        if (!profile.builder(p, c, y, x)) return false;

        /* Save the room location */
        if (dun->cent_n < z_info->level_room_max)
        {
            dun->cent[dun->cent_n].y = y;
            dun->cent[dun->cent_n].x = x;
            dun->cent_n++;
        }

        /* Reserve some blocks */
        for (by = by1; by < by2; by++)
            for (bx = bx1; bx < bx2; bx++)
                dun->room_map[by][bx] = true;
    }

    /* Count pit/nests */
    if (profile.pit) dun->pit_num++;

    /* Success */
    return true;
}