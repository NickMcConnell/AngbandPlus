/*
 * File: gen-util.c
 * Purpose: Dungeon generation utilities
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2013 Erik Osheim, Nick McConnell
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
#include <math.h>


/*
 * This file contains various utility functions for dungeon generation - mostly
 * for finding appropriate grids for some purposes, or placing things.
 */


/*
 * Accept values for y and x (considered as the endpoints of lines) between
 * 0 and 40, and return an angle in degrees (divided by two).
 *
 * This table's input and output need some processing:
 *
 * Because this table gives degrees for a whole circle, up to radius 20, its
 * origin is at (x,y) = (20, 20).  Therefore, the input code needs to find
 * the origin grid (where the lines being compared come from), and then map
 * it to table grid 20,20.  Do not, however, actually try to compare the
 * angle of a line that begins and ends at the origin with any other line -
 * it is impossible mathematically, and the table will return the value "255".
 *
 * The output of this table also needs to be massaged, in order to avoid the
 * discontinuity at 0/180 degrees.  This can be done by:
 *   rotate = 90 - first value
 *   this rotates the first input to the 90 degree line)
 *   tmp = ABS(second value + rotate) % 180
 *   diff = ABS(90 - tmp) = the angular difference (divided by two) between
 *   the first and second values.
 *
 * Note that grids diagonal to the origin have unique angles.
 */
byte get_angle_to_grid[41][41] =
{
  {  68,  67,  66,  65,  64,  63,  62,  62,  60,  59,  58,  57,  56,  55,  53,  52,  51,  49,  48,  46,  45,  44,  42,  41,  39,  38,  37,  35,  34,  33,  32,  31,  30,  28,  28,  27,  26,  25,  24,  24,  23 },
  {  69,  68,  67,  66,  65,  64,  63,  62,  61,  60,  59,  58,  56,  55,  54,  52,  51,  49,  48,  47,  45,  43,  42,  41,  39,  38,  36,  35,  34,  32,  31,  30,  29,  28,  27,  26,  25,  24,  24,  23,  22 },
  {  69,  69,  68,  67,  66,  65,  64,  63,  62,  61,  60,  58,  57,  56,  54,  53,  51,  50,  48,  47,  45,  43,  42,  40,  39,  37,  36,  34,  33,  32,  30,  29,  28,  27,  26,  25,  24,  24,  23,  22,  21 },
  {  70,  69,  69,  68,  67,  66,  65,  64,  63,  61,  60,  59,  58,  56,  55,  53,  52,  50,  48,  47,  45,  43,  42,  40,  38,  37,  35,  34,  32,  31,  30,  29,  27,  26,  25,  24,  24,  23,  22,  21,  20 },
  {  71,  70,  69,  69,  68,  67,  66,  65,  63,  62,  61,  60,  58,  57,  55,  54,  52,  50,  49,  47,  45,  43,  41,  40,  38,  36,  35,  33,  32,  30,  29,  28,  27,  25,  24,  24,  23,  22,  21,  20,  19 },
  {  72,  71,  70,  69,  69,  68,  67,  65,  64,  63,  62,  60,  59,  58,  56,  54,  52,  51,  49,  47,  45,  43,  41,  39,  38,  36,  34,  32,  31,  30,  28,  27,  26,  25,  24,  23,  22,  21,  20,  19,  18 },
  {  73,  72,  71,  70,  69,  69,  68,  66,  65,  64,  63,  61,  60,  58,  57,  55,  53,  51,  49,  47,  45,  43,  41,  39,  37,  35,  33,  32,  30,  29,  27,  26,  25,  24,  23,  22,  21,  20,  19,  18,  17 },
  {  73,  73,  72,  71,  70,  70,  69,  68,  66,  65,  64,  62,  61,  59,  57,  56,  54,  51,  49,  47,  45,  43,  41,  39,  36,  34,  33,  31,  29,  28,  26,  25,  24,  23,  21,  20,  20,  19,  18,  17,  17 },
  {  75,  74,  73,  72,  72,  71,  70,  69,  68,  66,  65,  63,  62,  60,  58,  56,  54,  52,  50,  47,  45,  43,  40,  38,  36,  34,  32,  30,  28,  27,  25,  24,  23,  21,  20,  19,  18,  18,  17,  16,  15 },
  {  76,  75,  74,  74,  73,  72,  71,  70,  69,  68,  66,  65,  63,  61,  59,  57,  55,  53,  50,  48,  45,  42,  40,  37,  35,  33,  31,  29,  27,  25,  24,  23,  21,  20,  19,  18,  17,  16,  16,  15,  14 },
  {  77,  76,  75,  75,  74,  73,  72,  71,  70,  69,  68,  66,  64,  62,  60,  58,  56,  53,  51,  48,  45,  42,  39,  37,  34,  32,  30,  28,  26,  24,  23,  21,  20,  19,  18,  17,  16,  15,  15,  14,  13 },
  {  78,  77,  77,  76,  75,  75,  74,  73,  72,  70,  69,  68,  66,  64,  62,  60,  57,  54,  51,  48,  45,  42,  39,  36,  33,  30,  28,  26,  24,  23,  21,  20,  18,  17,  16,  15,  15,  14,  13,  13,  12 },
  {  79,  79,  78,  77,  77,  76,  75,  74,  73,  72,  71,  69,  68,  66,  63,  61,  58,  55,  52,  49,  45,  41,  38,  35,  32,  29,  27,  24,  23,  21,  19,  18,  17,  16,  15,  14,  13,  13,  12,  11,  11 },
  {  80,  80,  79,  79,  78,  77,  77,  76,  75,  74,  73,  71,  69,  68,  65,  63,  60,  57,  53,  49,  45,  41,  37,  33,  30,  27,  25,  23,  21,  19,  17,  16,  15,  14,  13,  13,  12,  11,  11,  10,  10 },
  {  82,  81,  81,  80,  80,  79,  78,  78,  77,  76,  75,  73,  72,  70,  68,  65,  62,  58,  54,  50,  45,  40,  36,  32,  28,  25,  23,  20,  18,  17,  15,  14,  13,  12,  12,  11,  10,  10,   9,   9,   8 },
  {  83,  83,  82,  82,  81,  81,  80,  79,  79,  78,  77,  75,  74,  72,  70,  68,  64,  60,  56,  51,  45,  39,  34,  30,  26,  23,  20,  18,  16,  15,  13,  12,  11,  11,  10,   9,   9,   8,   8,   7,   7 },
  {  84,  84,  84,  83,  83,  83,  82,  81,  81,  80,  79,  78,  77,  75,  73,  71,  68,  63,  58,  52,  45,  38,  32,  27,  23,  19,  17,  15,  13,  12,  11,  10,   9,   9,   8,   7,   7,   7,   6,   6,   6 },
  {  86,  86,  85,  85,  85,  84,  84,  84,  83,  82,  82,  81,  80,  78,  77,  75,  72,  68,  62,  54,  45,  36,  28,  23,  18,  15,  13,  12,  10,   9,   8,   8,   7,   6,   6,   6,   5,   5,   5,   4,   4 },
  {  87,  87,  87,  87,  86,  86,  86,  86,  85,  85,  84,  84,  83,  82,  81,  79,  77,  73,  68,  58,  45,  32,  23,  17,  13,  11,   9,   8,   7,   6,   6,   5,   5,   4,   4,   4,   4,   3,   3,   3,   3 },
  {  89,  88,  88,  88,  88,  88,  88,  88,  88,  87,  87,  87,  86,  86,  85,  84,  83,  81,  77,  68,  45,  23,  13,   9,   7,   6,   5,   4,   4,   3,   3,   3,   2,   2,   2,   2,   2,   2,   2,   2,   1 },
  {  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90, 255,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0 },
  {  91,  92,  92,  92,  92,  92,  92,  92,  92,  93,  93,  93,  94,  94,  95,  96,  97,  99, 103, 113, 135, 158, 167, 171, 173, 174, 175, 176, 176, 177, 177, 177, 178, 178, 178, 178, 178, 178, 178, 178, 179 },
  {  93,  93,  93,  93,  94,  94,  94,  94,  95,  95,  96,  96,  97,  98,  99, 101, 103, 107, 113, 122, 135, 148, 158, 163, 167, 169, 171, 172, 173, 174, 174, 175, 175, 176, 176, 176, 176, 177, 177, 177, 177 },
  {  94,  94,  95,  95,  95,  96,  96,  96,  97,  98,  98,  99, 100, 102, 103, 105, 108, 113, 118, 126, 135, 144, 152, 158, 162, 165, 167, 168, 170, 171, 172, 172, 173, 174, 174, 174, 175, 175, 175, 176, 176 },
  {  96,  96,  96,  97,  97,  97,  98,  99,  99, 100, 101, 102, 103, 105, 107, 109, 113, 117, 122, 128, 135, 142, 148, 153, 158, 161, 163, 165, 167, 168, 169, 170, 171, 171, 172, 173, 173, 173, 174, 174, 174 },
  {  97,  97,  98,  98,  99,  99, 100, 101, 101, 102, 103, 105, 106, 108, 110, 113, 116, 120, 124, 129, 135, 141, 146, 150, 154, 158, 160, 162, 164, 165, 167, 168, 169, 169, 170, 171, 171, 172, 172, 173, 173 },
  {  98,  99,  99, 100, 100, 101, 102, 102, 103, 104, 105, 107, 108, 110, 113, 115, 118, 122, 126, 130, 135, 140, 144, 148, 152, 155, 158, 160, 162, 163, 165, 166, 167, 168, 168, 169, 170, 170, 171, 171, 172 },
  { 100, 100, 101, 101, 102, 103, 103, 104, 105, 106, 107, 109, 111, 113, 115, 117, 120, 123, 127, 131, 135, 139, 143, 147, 150, 153, 155, 158, 159, 161, 163, 164, 165, 166, 167, 167, 168, 169, 169, 170, 170 },
  { 101, 101, 102, 103, 103, 104, 105, 106, 107, 108, 109, 111, 113, 114, 117, 119, 122, 125, 128, 131, 135, 139, 142, 145, 148, 151, 153, 156, 158, 159, 161, 162, 163, 164, 165, 166, 167, 167, 168, 169, 169 },
  { 102, 103, 103, 104, 105, 105, 106, 107, 108, 110, 111, 113, 114, 116, 118, 120, 123, 126, 129, 132, 135, 138, 141, 144, 147, 150, 152, 154, 156, 158, 159, 160, 162, 163, 164, 165, 165, 166, 167, 167, 168 },
  { 103, 104, 105, 105, 106, 107, 108, 109, 110, 111, 113, 114, 116, 118, 120, 122, 124, 127, 129, 132, 135, 138, 141, 143, 146, 148, 150, 152, 154, 156, 158, 159, 160, 161, 162, 163, 164, 165, 165, 166, 167 },
  { 104, 105, 106, 106, 107, 108, 109, 110, 111, 113, 114, 115, 117, 119, 121, 123, 125, 127, 130, 132, 135, 138, 140, 143, 145, 147, 149, 151, 153, 155, 156, 158, 159, 160, 161, 162, 163, 164, 164, 165, 166 },
  { 105, 106, 107, 108, 108, 109, 110, 111, 113, 114, 115, 117, 118, 120, 122, 124, 126, 128, 130, 133, 135, 137, 140, 142, 144, 146, 148, 150, 152, 153, 155, 156, 158, 159, 160, 161, 162, 162, 163, 164, 165 },
  { 107, 107, 108, 109, 110, 110, 111, 113, 114, 115, 116, 118, 119, 121, 123, 124, 126, 129, 131, 133, 135, 137, 139, 141, 144, 146, 147, 149, 151, 152, 154, 155, 156, 158, 159, 160, 160, 161, 162, 163, 163 },
  { 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 119, 120, 122, 123, 125, 127, 129, 131, 133, 135, 137, 139, 141, 143, 145, 147, 148, 150, 151, 153, 154, 155, 156, 158, 159, 159, 160, 161, 162, 163 },
  { 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 120, 121, 122, 124, 126, 128, 129, 131, 133, 135, 137, 139, 141, 142, 144, 146, 148, 149, 150, 152, 153, 154, 155, 157, 158, 159, 159, 160, 161, 162 },
  { 109, 110, 111, 112, 113, 114, 114, 115, 117, 118, 119, 120, 122, 123, 125, 126, 128, 130, 131, 133, 135, 137, 139, 140, 142, 144, 145, 147, 148, 150, 151, 152, 153, 155, 156, 157, 158, 159, 159, 160, 161 },
  { 110, 111, 112, 113, 114, 114, 115, 116, 117, 119, 120, 121, 122, 124, 125, 127, 128, 130, 132, 133, 135, 137, 138, 140, 142, 143, 145, 146, 148, 149, 150, 151, 153, 154, 155, 156, 157, 158, 159, 159, 160 },
  { 111, 112, 113, 114, 114, 115, 116, 117, 118, 119, 120, 122, 123, 124, 126, 127, 129, 130, 132, 133, 135, 137, 138, 140, 141, 143, 144, 146, 147, 148, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 159 },
  { 112, 113, 114, 114, 115, 116, 117, 118, 119, 120, 121, 122, 124, 125, 126, 128, 129, 131, 132, 133, 135, 137, 138, 139, 141, 142, 144, 145, 146, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159 },
  { 113, 114, 114, 115, 116, 117, 118, 118, 120, 121, 122, 123, 124, 125, 127, 128, 129, 131, 132, 134, 135, 136, 138, 139, 141, 142, 143, 145, 146, 147, 148, 149, 150, 152, 152, 153, 154, 155, 156, 157, 158 }
};


/*
 * Used to convert grid into an array index (i) in a chunk of width w.
 *
 * grid location
 * w area width
 *
 * Returns the grid index.
 */
int grid_to_i(struct loc *grid, int w)
{
    return grid->y * w + grid->x;
}


/*
 * Used to convert an array index (i) into grid in a chunk of width w.
 *
 * i grid index
 * w area width
 * grid location
 */
void i_to_grid(int i, int w, struct loc *grid)
{
    grid->y = i / w;
    grid->x = i % w;
}


/*
 * Shuffle an array using Knuth's shuffle.
 *
 * arr array
 * n number of shuffle moves
 */
void shuffle(int *arr, int n)
{
    int i, j, k;

    for (i = 0; i < n; i++)
    {
        j = randint0(n - i) + i;
        k = arr[j];
        arr[j] = arr[i];
        arr[i] = k;
    }
}


/*
 * Locate a square in a rectangle which satisfies the given predicate.
 *
 * c current chunk
 * grid found grid
 * top_left top left grid of rectangle
 * bottom_right bottom right grid of rectangle
 * pred square_predicate specifying what we're looking for
 *
 * Returns success
 */
bool cave_find_in_range(struct chunk *c, struct loc *grid, struct loc *top_left,
    struct loc *bottom_right, square_predicate pred)
{
    struct loc diff;
    int i, n;
    bool found = false;
    int *squares;

    loc_diff(&diff, bottom_right, top_left);
    n = diff.y * diff.x;

    /* Allocate the squares, and randomize their order */
    squares = mem_alloc(n * sizeof(int));

    for (i = 0; i < n; i++) squares[i] = i;

    /* Test each square in (random) order for openness */
    for (i = 0; i < n && !found; i++)
    {
        int j = randint0(n - i) + i;
        int k = squares[j];

        squares[j] = squares[i];
        squares[i] = k;

        grid->y = (k / diff.x) + top_left->y;
        grid->x = (k % diff.x) + top_left->x;
        if (pred(c, grid)) found = true;
    }

    mem_free(squares);

    /* Return whether we found an empty square or not. */
    return found;
}


/*
 * Locate a square in the dungeon which satisfies the given predicate.
 *
 * c current chunk
 * grid found grid
 * pred square_predicate specifying what we're looking for
 *
 * Returns success
 */
static bool cave_find(struct chunk *c, struct loc *grid, square_predicate pred)
{
    struct loc top_left, bottom_right;

    loc_init(&top_left, 0, 0);
    loc_init(&bottom_right, c->width, c->height);
    return cave_find_in_range(c, grid, &top_left, &bottom_right, pred);
}


/*
 * Locate an empty square for 0 <= y < ymax, 0 <= x < xmax.
 *
 * c current chunk
 * y found y co-ordinate
 * x found x co-ordinate
 *
 * Returns success
 */
bool find_empty(struct chunk *c, struct loc *grid)
{
    return cave_find(c, grid, square_isempty);
}


/*
 * Locate a grid within +/- yd, xd of a centre.
 *
 * c current chunk
 * grid found grid
 * centre starting grid
 * yd y-range
 * xd x-range
 *
 * Returns success
 */
bool find_nearby_grid(struct chunk *c, struct loc *grid, struct loc *centre, int yd, int xd)
{
    struct loc top_left, bottom_right;

    loc_init(&top_left, centre->x - xd, centre->y - yd);
    loc_init(&bottom_right, centre->x + xd + 1, centre->y + yd + 1);
    return cave_find_in_range(c, grid, &top_left, &bottom_right, square_in_bounds_fully);
}


/*
 * Given two points, pick a valid cardinal direction from one to the other.
 *
 * offset found offset direction from grid1 to grid2
 * grid1 starting grid
 * grid2 target grid
 */
void correct_dir(struct loc *offset, struct loc *grid1, struct loc *grid2)
{
    /* Extract horizontal and vertical directions */
    offset->x = CMP(grid2->x, grid1->x);
    offset->y = CMP(grid2->y, grid1->y);

    /* If we only have one direction to go, then we're done */
    if (!offset->x || !offset->y) return;

    /* If we need to go diagonally, then choose a random direction */
    if (magik(50))
        offset->y = 0;
    else
        offset->x = 0;
}


/*
 * Pick a random cardinal direction.
 *
 * offset direction offset
 */
void rand_dir(struct loc *offset)
{
    /* Pick a random direction and extract the dy/dx components */
    int i = randint0(4);

    loc_copy(offset, &ddgrid_ddd[i]);
}


/*
 * Locate a square that is suited for stair placement.
 *
 * c current chunk
 * grid found grid
 * walls number of adjacent walls
 *
 * Returns success
 */
static bool square_suits_stairs(struct chunk *c, struct loc *grid, int walls)
{
    int i, n = c->height * c->width;
    bool found = false;

    /* Allocate the squares, and randomize their order */
    int *squares = mem_alloc(n * sizeof(int));

    for (i = 0; i < n; i++) squares[i] = i;

    /* Test each square in (random) order for openness */
    for (i = 0; i < n && !found; i++)
    {
        int j = randint0(n - i) + i;
        int k = squares[j];

        squares[j] = squares[i];
        squares[i] = k;

        grid->y = (k / c->width);
        grid->x = (k % c->width);

        if (!square_isempty(c, grid)) continue;
        if (square_isvault(c, grid) || square_isno_stairs(c, grid)) continue;
        if (square_num_walls_adjacent(c, grid) == walls) found = true;
    }

    mem_free(squares);

    /* Return whether we found an empty square or not. */
    return found;
}


/*
 * Determine whether the given coordinate is a valid starting location.
 *
 * c current chunk
 * grid found grid
 *
 * Returns success
 */
static bool find_start(struct chunk *c, struct loc *grid)
{
    int walls = 3;

    /* Gradually reduce number of walls if having trouble */
    while (walls >= 0)
    {
        /* Find the best possible place */
        if (square_suits_stairs(c, grid, walls))
            return true;

        walls--;
    }

    quit("Failed to place player!");
    return false;
}


static bool square_suits_down_stairs(struct chunk *c, struct loc *grid)
{
    if (!square_isempty(c, grid)) return false;
    if (square_isvault(c, grid)) return false;
    return true;
}


/*
 * Add down stairs at a random location.
 *
 * c current chunk
 */
void add_down_stairs(struct chunk *c)
{
    struct loc grid;

    /* Try to find a good place to put the player */
    cave_find(c, &grid, square_suits_down_stairs);

    /* Place a staircase */
    square_set_downstairs(c, &grid, FEAT_MORE);

    /* Hack -- the players start on the stairs while recalling */
    square_set_join_rand(c, &grid);
}


/*
 * Place the player at a random starting location.
 *
 * c current chunk
 * p the player
 */
void new_player_spot(struct chunk *c, struct player *p)
{
    struct loc grid;

    /* Place the player */
    find_start(c, &grid);

    /* Save the new grid */
    square_set_join_rand(c, &grid);

    /* Disconnected stairs */
    if (cfg_limit_stairs)
    {
        /* Set this to be the starting location for people going down */
        find_start(c, &grid);
        square_set_join_down(c, &grid);

        /* Set this to be the starting location for people going up */
        find_start(c, &grid);
        square_set_join_up(c, &grid);
    }

    /* Hack -- stay in bounds (to avoid asserts during cave generation) */
    p->grid.y = MIN(MAX(p->grid.y, 1), c->height - 2);
    p->grid.x = MIN(MAX(p->grid.x, 1), c->width - 2);
}


/*
 * Place rubble at a given location.
 *
 * c current chunk
 * grid location
 */
static void place_rubble(struct chunk *c, struct loc *grid)
{
    int i, j;
    int feat = (one_in_(2)? FEAT_PASS_RUBBLE: FEAT_RUBBLE);

    /* Create rubble */
    square_set_feat(c, grid, feat);

    /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
    if (TURN_BASED) return;

    for (j = -1; j < 2; j++)
    {
        for (i = -1; i < 2; i++)
        {
            struct loc next;

            loc_init(&next, grid->x + i, grid->y + j);

            /* Skip corners */
            if (abs(i + j) != 1) continue;
            
            /* Check Bounds */
            if (!square_in_bounds_fully(c, &next)) continue;

            /* Totally useless AKA Require a certain number of adjacent walls */
            if (square_num_walls_adjacent(c, &next) < 2) continue;
            
            /* Require wall grid */
            if (square_isempty(c, &next)) continue;

            /* Require an empty grid on the opposite side */
            loc_init(&next, grid->x - i, grid->y - j);
            if (!square_isempty(c, &next)) continue;
            
            /* Place on the opposite side */
            square_set_feat(c, &next, feat);
            
            /* Done */
            return;     
        }
    }
}


/*
 * Place traps at (x, y).
 *
 * c current chunk
 * y, x co-ordinates
 */
static void place_traps(struct chunk *c, struct loc *grid)
{
    int i, j;

    /* Create trap */
    square_add_trap(c, grid);

    /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
    if (TURN_BASED) return;

    for (j = -1; j < 2; j++)
    {
        for (i = -1; i < 2; i++)
        {
            struct loc next;

            loc_init(&next, grid->x + i, grid->y + j);

            /* Skip corners */
            if (abs(i + j) != 1) continue;
            
            /* Check Bounds */
            if (!square_in_bounds_fully(c, &next)) continue;

            /* Totally useless AKA Require a certain number of adjacent walls */
            if (square_num_walls_adjacent(c, &next) < 2) continue;
            
            /* Require wall grid */
            if (square_isempty(c, &next)) continue;

            /* Require an empty grid on the opposite side */
            loc_init(&next, grid->x - i, grid->y - j);
            if (!square_isempty(c, &next)) continue;
            
            /* Place on the opposite side */
            square_add_trap(c, &next);
            
            /* Done */
            return;     
        }
    }
}


/*
 * Convert existing terrain type to fountain
 */
static void place_fountain(struct chunk *c, struct loc *grid)
{
    int feat;

    /* 25% chance of being dried out */
    if (magik(75)) feat = FEAT_FOUNTAIN;
    else feat = FEAT_FNT_DRIED;
    
    /* Create fountain */
    square_set_feat(c, grid, feat);
}


/*
 * Place stairs (of the requested type 'feat' if allowed) at a given location.
 *
 * c current chunk
 * grid location
 * feat requested feature
 *
 * All stairs from the surface go down. All stairs from bottom go up.
 */
static void place_stairs(struct chunk *c, struct loc *grid, int feat)
{
    struct wild_type *w_ptr = get_wt_info_at(&c->wpos.grid);

    if (c->wpos.depth == 0)
        square_set_feat(c, grid, FEAT_MORE);
    else if (c->wpos.depth == w_ptr->max_depth - 1)
        square_set_upstairs(c, grid);
    else
    {
        if (feat == FEAT_LESS)
            square_set_upstairs(c, grid);
        if (feat == FEAT_MORE)
            square_set_downstairs(c, grid, FEAT_MORE);
    }
}


/*
 * Place random stairs at (x, y).
 *
 * c current chunk
 * y, x co-ordinates
 */
void place_random_stairs(struct chunk *c, struct loc *grid)
{
    int feat = (magik(50)? FEAT_LESS: FEAT_MORE);

    if (square_canputitem(c, grid)) place_stairs(c, grid, feat);
}


/*
 * Place a random object at a given location.
 *
 * p the player
 * c current chunk
 * grid location
 * level generation depth
 * good is it a good object?
 * great is it a great object?
 * origin item origin
 * tval specified tval, if any
 */
void place_object(struct player *p, struct chunk *c, struct loc *grid, int level, bool good,
    bool great, byte origin, int tval)
{
    s32b rating = 0;
    struct object *new_obj;
    bool dummy = true;

    if (!square_in_bounds(c, grid)) return;
    if (!square_canputitem(c, grid)) return;

    /* Make an appropriate object */
    new_obj = make_object(p, c, level, good, great, false, &rating, tval);
    if (!new_obj) return;
    set_origin(new_obj, origin, c->wpos.depth, NULL);

    /* Give it to the floor */
    if (!floor_carry(p, c, grid, new_obj, &dummy))
    {
        object_delete(&new_obj);
        return;
    }

    if (new_obj->artifact) c->good_item = true;

    /* Avoid overflows */
    if (rating > 2500000) rating = 2500000;

    c->obj_rating += (rating / 100) * (rating / 100);
}


/*
 * Place a random amount of gold at a given location.
 *
 * p the player
 * c current chunk
 * grid location
 * level generation depth
 * origin item origin
 */
void place_gold(struct player *p, struct chunk *c, struct loc *grid, int level, byte origin)
{
    struct object *money = NULL;
    bool dummy = true;

    if (!square_in_bounds(c, grid)) return;
    if (!square_canputitem(c, grid)) return;

    /* Make some gold */
    money = make_gold(p, level, "any");
    set_origin(money, origin, c->wpos.depth, NULL);

    /* Give it to the floor */
    if (!floor_carry(p, c, grid, money, &dummy))
        object_delete(&money);
}


/*
 * Place a secret door at a given location.
 *
 * c current chunk
 * grid location
 */
void place_secret_door(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_SECRET);
}


/*
 * Place a closed door at a given location.
 *
 * c current chunk
 * grid location
 */
void place_closed_door(struct chunk *c, struct loc *grid)
{
    square_close_door(c, grid);
    if (one_in_(4))
        square_set_door_lock(c, grid, randint1(7));
}


/*
 * Place a random door at a given location.
 *
 * c current chunk
 * grid location
 *
 * The door generated could be closed, open, broken, or secret.
 */
void place_random_door(struct chunk *c, struct loc *grid)
{
    int tmp = randint0(100);
    struct worldpos dpos;
    struct location *dungeon;

    /* Get the dungeon */
    wpos_init(&dpos, &c->wpos.grid, 0);
    dungeon = get_dungeon(&dpos);

    /* Some dungeons don't have doors at all */
    if (dungeon && c->wpos.depth && df_has(dungeon->flags, DF_NO_DOORS)) return;

    /* Create open door */
    if (tmp < 30) square_open_door(c, grid);

    /* Create broken door */
    else if (tmp < 40) square_smash_door(c, grid);

    /* Create closed door */
    else place_closed_door(c, grid);
}


/*
 * Place some staircases near walls
 *
 * c current chunk
 * feat the stair terrain type
 * num number of staircases to place
 */
void alloc_stairs(struct chunk *c, int feat, int num)
{
    int i;
    struct loc grid;
    int walls = 3;

    /* Place "num" stairs */
    for (i = 0; i < num; i++)
    {
        /* Gradually reduce number of walls if having trouble */
        while (true)
        {
            /* Find the best possible place */
            if (square_suits_stairs(c, &grid, walls))
            {
                place_stairs(c, &grid, feat);
                break;
            }

            /* Require fewer walls */
            if (walls == 0) quit("Failed to place stairs!");
            walls--;
        }
    }
}


/*
 * Allocates 'num' random objects in the dungeon.
 *
 * p the player
 * c current chunk
 * set where the entity is placed (corridor, room or either)
 * typ what is placed (rubble, trap, gold, item)
 * num number to place
 * depth generation depth
 * origin item origin (if appropriate)
 *
 * See alloc_object() for more information.
 */
void alloc_objects(struct player *p, struct chunk *c, int set, int typ, int num, int depth,
    byte origin)
{
    int k, l = 0;

    for (k = 0; k < num; k++)
    {
         bool ok = alloc_object(p, c, set, typ, depth, origin);

         if (!ok) l++;
    }
}


/*
 * Allocates a single random object in the dungeon.
 *
 * p the player
 * c current chunk
 * set where the entity is placed (corridor, room or either)
 * typ what is placed (rubble, trap, gold, item)
 * depth generation depth
 * origin item origin (if appropriate)
 *
 * 'set' controls where the object is placed (corridor, room, either).
 * 'typ' controls the kind of object (rubble, trap, gold, item).
 */
bool alloc_object(struct player *p, struct chunk *c, int set, int typ, int depth,
    byte origin)
{
    int tries = 0;
    struct loc grid;

    /* Pick a "legal" spot */
    while (tries < 2000)
    {
        tries++;

        find_empty(c, &grid);

        /* If we are ok with a corridor and we're in one, we're done */
        if (set & SET_CORR && !square_isroom(c, &grid)) break;

        /* If we are ok with a room and we're in one, we're done */
        if (set & SET_ROOM && square_isroom(c, &grid)) break;
    }

    if (tries == 2000) return false;

    /* Place something */
    switch (typ)
    {
        case TYP_RUBBLE: place_rubble(c, &grid); break;
        case TYP_FOUNTAIN: place_fountain(c, &grid); break;
        case TYP_TRAP: place_traps(c, &grid); break;
        case TYP_GOLD:
        {
            place_gold(p, c, &grid, depth, origin);
            break;
        }
        case TYP_OBJECT:
        {
            place_object(p, c, &grid, depth, false, false, origin, 0);
            break;
        }
        case TYP_GOOD:
        {
            place_object(p, c, &grid, depth, true, false, origin, 0);
            break;
        }
        case TYP_GREAT:
        {
            place_object(p, c, &grid, depth, true, true, origin, 0);
            break;
        }
    }

    return true;
}


/*
 * Create up to 'num' objects near the given coordinates in a vault.
 *
 * p the player
 * c current chunk
 * grid location
 * num number of objects
 */
void vault_objects(struct player *p, struct chunk *c, struct loc *grid, int num)
{
    int i;

    /* Attempt to place 'num' objects */
    for (; num > 0; --num)
    {
        /* Try up to 11 spots looking for empty space */
        for (i = 0; i < 11; ++i)
        {
            struct loc nearby;

            /* Pick a random location */
            find_nearby_grid(c, &nearby, grid, 2, 3);

            /* Require "clean" floor space */
            if (!square_canputitem(c, &nearby)) continue;

            /* Place an item or gold */
            if (magik(75))
                place_object(p, c, &nearby, c->wpos.depth, false, false, ORIGIN_SPECIAL, 0);
            else
                place_gold(p, c, &nearby, c->wpos.depth, ORIGIN_SPECIAL);

            /* Placement accomplished */
            break;
        }
    }
}


/*
 * Place a trap near a location, with a given displacement.
 *
 * c current chunk
 * grid location to place the trap near
 * yd, xd how far afield to look for a place
 */
static void vault_trap_aux(struct chunk *c, struct loc *grid, int yd, int xd)
{
    int tries;

    /* Find a nearby empty grid and place a trap */
    for (tries = 0; tries <= 5; tries++)
    {
        struct loc nearby;

        find_nearby_grid(c, &nearby, grid, yd, xd);
        if (!square_isempty(c, &nearby)) continue;

        square_add_trap(c, &nearby);

        break;
    }
}


/*
 * Place 'num' traps near a location, with a given displacement.
 *
 * c current chunk
 * grid location to place the trap near
 * yd, xd how far afield to look for a place
 * num number of traps to place
 */
void vault_traps(struct chunk *c, struct loc *grid, int yd, int xd, int num)
{
    int i;

    for (i = 0; i < num; i++)
        vault_trap_aux(c, grid, yd, xd);
}


/*
 * Place 'num' sleeping monsters near the location.
 *
 * p the player
 * c current chunk
 * grid location to place the monsters near
 * depth generation depth
 * num number of monsters to place
 */
void vault_monsters(struct player *p, struct chunk *c, struct loc *grid, int depth, int num)
{
    int k, i;

    /* If the starting location is illegal, don't even start */
    if (!square_in_bounds(c, grid)) return;

    /* Try to summon "num" monsters "near" the given location */
    for (k = 0; k < num; k++)
    {
        /* Try nine locations */
        for (i = 0; i < 9; i++)
        {
            struct loc nearby;

            /* Pick a nearby location */
            if (!scatter(c, &nearby, grid, 1, true)) continue;

            /* Require "empty" floor grids */
            if (!square_isemptyfloor(c, &nearby)) continue;

            /* Place the monster (allow groups) */
            pick_and_place_monster(p, c, &nearby, depth, MON_ASLEEP | MON_GROUP, ORIGIN_DROP_SPECIAL);

            break;
        }
    }
}