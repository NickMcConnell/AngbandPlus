/*
 * File: wilderness.c
 * Purpose: Wilderness generation
 *
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
#include "../common/tvalsval.h"
#include "generate.h"
#include "monster/mon-make.h"
#include "monster/mon-util.h"
#include "wilderness.h"


/*
 * This function takes the players (x,y) level world coordinate and uses it to
 * calculate p_ptr->depth.  The levels are stored in a series of "rings"
 * radiating out from the town, as shown below.  This storage mechanism was
 * used because it does not place an initial restriction on the wilderness
 * dimensions.
 *
 * In order to gracefully introduce the wilderness to the preexisting mangband
 * functions, the indexes are negative and apply to the same cave structure
 * that holds the dungeon levels.
 *
 *         Indexes (-)             Ring #                         world_y
 *
 *           [05]                   [2]                            [ 2]
 *       [12][01][06]            [2][1][2]                         [ 1]
 *   [11][04][To][02][07]     [2][1][X][1][2]     world_x  [-2][-1][ 0][ 1][ 2]
 *       [10][03][08]            [2][1][2]                         [-1]
 *           [09]                   [2]                            [-2]
 *
 * A special function is required to init the wild_info structures because I have not
 * been able to devise a simple algorithm to go from an index to an x,y coordinate.
 * I derived an equation that would calculate the ring from the index, but it involved
 * a square root.
 */
int world_index(int world_x, int world_y)
{
    int ring, base, offset, idx;

    /* Calculate which "ring" the level is in */
    ring = abs(world_x) + abs(world_y);

    /* Hack -- the town is 0 */
    if (!ring) return 0;

    /* Calculate the base offset of this ring */
    base = 2 * ring * (ring - 1) + 1;

    /* Calculate the offset within this ring */
    if (world_x >= 0) offset = ring - world_y;
    else offset = (3 * ring) + world_y;

    idx = 0 - (base + offset);

    return idx;
}


/*
 * Returns the neighbor index, valid or invalid.
 */
static int neighbor_index(int depth, char dir)
{
    int cur_x, cur_y, neigh_idx = -1;

    cur_x = wild_info[depth].world_x;
    cur_y = wild_info[depth].world_y;

    switch (dir)
    {
        case DIR_NORTH: neigh_idx = world_index(cur_x, cur_y + 1); break;
        case DIR_EAST:  neigh_idx = world_index(cur_x + 1, cur_y); break;
        case DIR_SOUTH: neigh_idx = world_index(cur_x, cur_y - 1); break;
        case DIR_WEST:  neigh_idx = world_index(cur_x - 1, cur_y); break;
    }

    return neigh_idx;
}


/*
 * Initialize the wild_info structures. Uses a recursive fill algorithm.
 * Note that the flags for these structures are loaded from the server savefile.
 * Note that this has to be initially called with (0,0) to work properly.
 */
static void init_wild_info_aux(int x, int y)
{
    int depth = world_index(x, y), neigh_idx;

    /* If we are a valid index, initialize */
    if (depth >= 0 - MAX_WILD)
    {
        wild_info[depth].world_x = x;
        wild_info[depth].world_y = y;
        if (depth == 0) wild_info[depth].type = WILD_TOWN;
    }

    /* Initialize each of our uninitialized neighbors */

    /* North */
    neigh_idx = neighbor_index(depth, DIR_NORTH);
    if ((neigh_idx >= 0 - MAX_WILD) && !wild_radius(neigh_idx))
        init_wild_info_aux(x, y + 1);

    /* East */
    neigh_idx = neighbor_index(depth, DIR_EAST);
    if ((neigh_idx >= 0 - MAX_WILD) && !wild_radius(neigh_idx))
        init_wild_info_aux(x + 1, y);

    /* South */
    neigh_idx = neighbor_index(depth, DIR_SOUTH);
    if ((neigh_idx >= 0 - MAX_WILD) && !wild_radius(neigh_idx))
        init_wild_info_aux(x, y - 1);

    /* West */
    neigh_idx = neighbor_index(depth, DIR_WEST);
    if ((neigh_idx >= 0 - MAX_WILD) && !wild_radius(neigh_idx))
        init_wild_info_aux(x - 1, y);
}


/*
 * Initialize the wild_info structures. Uses a recursive fill algorithm.
 * Note that the flags for these structures are loaded from the server savefile.
 */
void init_wild_info()
{
    /* Set the wild_info to 0 by default */
    memset(&wild_info[0 - MAX_WILD], 0, sizeof(wilderness_type) * (MAX_WILD + 1));

    /* Initialize the wild_info structures */
    init_wild_info_aux(0, 0);
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_shore(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (rf_has(r_ptr->flags, RF_WILD_SHORE)) return TRUE;
    if (rf_has(r_ptr->flags, RF_WILD_ALL)) return TRUE;

    return FALSE;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_grass(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (rf_has(r_ptr->flags, RF_WILD_GRASS)) return TRUE;
    if (rf_has(r_ptr->flags, RF_WILD_ALL)) return TRUE;

    return FALSE;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_wood(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (rf_has(r_ptr->flags, RF_WILD_WOOD)) return TRUE;
    if (rf_has(r_ptr->flags, RF_WILD_ALL)) return TRUE;

    return FALSE;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_swamp(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (rf_has(r_ptr->flags, RF_WILD_SWAMP)) return TRUE;
    if (rf_has(r_ptr->flags, RF_WILD_ALL)) return TRUE;

    return FALSE;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_waste(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (rf_has(r_ptr->flags, RF_WILD_WASTE)) return TRUE;
    if (rf_has(r_ptr->flags, RF_WILD_ALL)) return TRUE;

    return FALSE;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_mountain(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (rf_has(r_ptr->flags, RF_WILD_MOUNTAIN)) return TRUE;
    if (rf_has(r_ptr->flags, RF_WILD_ALL)) return TRUE;

    return FALSE;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_volcano(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (rf_has(r_ptr->flags, RF_WILD_VOLCANO)) return TRUE;
    if (rf_has(r_ptr->flags, RF_WILD_ALL)) return TRUE;

    return FALSE;
}


/*
 * Place a monster in the wilderness
 */
void wild_add_monster(struct player *p, struct cave *c)
{
    int monst_x, monst_y, r_idx;
    int tries = 0;

    /* Reset the monster sorting function */
    get_mon_num_hook = NULL;
    switch (wild_info[c->depth].type)
    {
        case WILD_SHORE: get_mon_num_hook = wild_monst_aux_shore; break;
        case WILD_GRASS: get_mon_num_hook = wild_monst_aux_grass; break;
        case WILD_WOOD: get_mon_num_hook = wild_monst_aux_wood; break;
        case WILD_SWAMP: get_mon_num_hook = wild_monst_aux_swamp; break;
        case WILD_WASTE: get_mon_num_hook = wild_monst_aux_waste; break;
        case WILD_MOUNTAIN: get_mon_num_hook = wild_monst_aux_mountain; break;
        case WILD_VOLCANO: get_mon_num_hook = wild_monst_aux_volcano; break;
    }
    get_mon_num_prep();

    /* Find a legal, unoccupied space */
    while (tries < 50)
    {
        monst_x = randint0(DUNGEON_WID);
        monst_y = randint0(DUNGEON_HGT);

        if (cave_isempty(c, monst_y, monst_x)) break;
        tries++;
    }

    /* Get the monster */
    r_idx = get_mon_num(c->depth, monster_level(c->depth));

    /* Hack -- Restore the monster selection function */
    get_mon_num_hook = NULL;
    get_mon_num_prep();

    /* Handle failure */
    if (!r_idx) return;

    /* Place the monster */
    place_new_monster(p, c, monst_y, monst_x, r_idx, MON_GROUP, ORIGIN_DROP);
}


/*
 * Choose a clear building location, possibly specified by (xcen, ycen),
 * and "reserves" it so nothing else can choose any of its squares for building again
 */
static void reserve_building_plot(struct cave *c, int *x1, int *y1, int *x2, int *y2,
    int xlen, int ylen, int xcen, int ycen)
{
    int x, y, attempts = 0, plot_clear;

    while (attempts < 20)
    {
        /* If xcen, ycen have not been specified */
        if (!in_bounds_fully(ycen, xcen))
        {
            /* The upper left corner */
            *x1 = randint0(DUNGEON_WID - xlen - 4) + 2;
            *y1 = randint0(DUNGEON_HGT - ylen - 4) + 2;

            /* The lower right corner */
            *x2 = *x1 + xlen - 1;
            *y2 = *y1 + ylen - 1;
        }
        else
        {
            *x1 = xcen - xlen / 2;
            *y1 = ycen - ylen / 2;
            *x2 = *x1 + xlen - 1;
            *y2 = *y1 + ylen - 1;
        }

        /* Add a 'border' (reserve 1 tile more than needed) */
        --*x1; --*y1; ++*x2; ++*y2;

        /* Check acquired x1, y1, x2, y2 */
        if (!in_bounds_fully(*y1, *x1) || !in_bounds_fully(*y2, *x2))
        {
            *x1 = *y1 = *x2 = *y2 = -1;
            return;
        }

        plot_clear = 1;

        /* Check if its clear */
        for (y = *y1; y <= *y2; y++)
        {
            for (x = *x1; x <= *x2; x++)
            {
                switch (c->feat[y][x])
                {
                    /* Don't build on other buildings or farms */
                    case FEAT_LOOSE_DIRT:
                    case FEAT_CROP:
                    case FEAT_PERM_EXTRA:
                    case FEAT_PERM_ARENA:
                    case FEAT_LOGS:
                        plot_clear = 0;
                        break;
                }

                /* Any ickiness on the plot is NOT allowed */
                if (is_icky(c->depth, y, x)) plot_clear = 0;

                /* Spaces that have already been reserved are NOT allowed */
                if (c->info[y][x] & CAVE_TEMP) plot_clear = 0;
            }
        }

        /* Buildings and farms can partially, but not completely, be built on water. */
        if ((c->feat[*y1][*x1] == FEAT_WATER) && (c->feat[*y2][*x2] == FEAT_WATER))
            plot_clear = 0;

        /* If we have a clear plot, reserve it and return */
        if (plot_clear)
        {
            for (y = *y1; y <= *y2; y++)
            {
                for (x = *x1; x <= *x2; x++)
                {
                    c->info[y][x] |= CAVE_TEMP;
                }
            }
            ++*x1; ++*y1; --*x2; --*y2;
            return;
        }

        attempts++;
    }

    /* Plot allocation failed */
    *x1 = *y1 = *x2 = *y2 = -1;
}


/*
 * Adds a garden at a reasonable distance from a building.
 */
static void wild_add_garden(struct cave *c, int xmin, int ymin, int xmax, int ymax,
    int *x1, int *y1, int *x2, int *y2, int *type)
{
    int xlen, ylen, xcen, ycen, x, y, orientation;

    /* Choose a 'good' size for the garden */
    xlen = randint0(15) + 15;
    ylen = randint0(7) + 7;

    /* Choose a 'good' location for the garden */
    while (TRUE)
    {
        xcen = rand_range(xmin - xlen, xmax + xlen);
        ycen = rand_range(ymin - ylen, ymax + ylen);
        if (in_bounds_fully(ycen, xcen) &&
            ((xcen < xmin - xlen / 2) || (xcen > xmax + xlen / 2) ||
            (ycen < ymin - ylen / 2) || (ycen > ymax + ylen / 2))) break;
    }

    reserve_building_plot(c, x1, y1, x2, y2, xlen, ylen, xcen, ycen);

    /* If we failed to obtain a valid plot */
    if (*x1 < 0) return;

    /* Choose which type of garden it is */
    *type = randint0(7);

    /* Whether the crop rows are horizontal or vertical */
    orientation = randint0(2);

    /* Initially fill with a layer of dirt */
    for (y = *y1; y <= *y2; y++)
    {
        for (x = *x1; x <= *x2; x++)
        {
            cave_set_feat(c, y, x, FEAT_LOOSE_DIRT);
        }
    }

    /* Alternating rows of crops */
    for (y = *y1 + 1; y <= *y2 - 1; y++)
    {
        for (x = *x1 + 1; x <= *x2 - 1; x++)
        {
            /* Different orientations */
            if ((!orientation && (y % 2)) || (orientation && (x % 2)))
            {
                /* Set to crop */
                cave_set_feat(c, y, x, FEAT_CROP);
            }
        }
    }
}


/*
 * Adds crop to a given location.
 */
void wild_add_crop(int depth, int x, int y, int type)
{
    object_type food;

    /* Food choice */
    switch (type)
    {
        case WILD_CROP_POTATO:
            object_prep(&food, lookup_kind(TV_CROP, SV_FOOD_POTATO), 0, RANDOMISE);
            break;

        case WILD_CROP_CABBAGE:
            object_prep(&food, lookup_kind(TV_CROP, SV_FOOD_HEAD_OF_CABBAGE), 0,
                RANDOMISE);
            break;

        case WILD_CROP_CARROT:
            object_prep(&food, lookup_kind(TV_CROP, SV_FOOD_CARROT), 0, RANDOMISE);
            break;

        case WILD_CROP_BEET:
            object_prep(&food, lookup_kind(TV_CROP, SV_FOOD_BEET), 0, RANDOMISE);
            break;

        case WILD_CROP_MUSHROOM:
        {
            /* Hack -- mushrooms are rare */
            static int shroom_chance[][2] =
            {
                {SV_SHROOM_SECOND_SIGHT, 2},
                {SV_SHROOM_FAST_RECOVERY, 5},
                {SV_SHROOM_VIGOR, 1},
                {SV_SHROOM_CLEAR_MIND, 5},
                {SV_SHROOM_EMERGENCY, 5},
                {SV_SHROOM_TERROR, 5},
                {SV_SHROOM_STONE, 5},
                {SV_SHROOM_DEBILITY, 5},
                {SV_SHROOM_SPRINTING, 5},
                {SV_SHROOM_PURGING, 5},
                {SV_FOOD_SLIME_MOLD, 100}
            };
            int i;

            do {i = randint0(N_ELEMENTS(shroom_chance));}
            while (!magik(shroom_chance[i][1]));

            object_prep(&food, lookup_kind(TV_FOOD, shroom_chance[i][0]), 0,
                RANDOMISE);
            break;
        }

        case WILD_CROP_SQUASH:
            object_prep(&food, lookup_kind(TV_CROP, SV_FOOD_SQUASH), 0, RANDOMISE);
            break;

        case WILD_CROP_CORN:
            object_prep(&food, lookup_kind(TV_CROP, SV_FOOD_EAR_OF_CORN), 0,
                RANDOMISE);
            break;
    }

    /* Drop food */
    set_origin(&food, ORIGIN_FLOOR, depth, 0);
    drop_near(NULL, cave_get(depth), &food, 0, y, x, FALSE);
}


/*
 * Adds crops to a given garden.
 */
static void wild_add_crops(struct cave *c, int x1, int y1, int x2, int y2, int type)
{
    int x, y;

    /* Alternating rows of crops */
    for (y = y1 + 1; y <= y2 - 1; y++)
    {
        for (x = x1 + 1; x <= x2 - 1; x++)
        {
            /* Different orientations */
            if (c->feat[y][x] != FEAT_CROP) continue;

            /* Random chance of food */
            if (magik(60)) continue;

            wild_add_crop(c->depth, x, y, type);
        }
    }
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_invaders(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Is the monster humanoid? */
    if (is_humanoid(r_ptr)) return TRUE;

    /* Is the monster half-humanoid? */
    if (is_half_humanoid(r_ptr)) return TRUE;

    return FALSE;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_home_owner(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Is the monster humanoid? */
    return is_humanoid(r_ptr);
}


/*
 * Make a dwelling 'interesting'
 */
static void wild_furnish_dwelling(struct player *p, struct cave *c, int x1, int y1,
    int x2, int y2)
{
    wilderness_type *w_ptr = &wild_info[c->depth];
    bool at_home = FALSE, taken_over = FALSE;
    int num_food = 0, cash = 0, num_objects = 0;
    int x, y, trys, r_idx;
    object_type forge;
    int size = (x2 - x1) * (y2 - y1);
    int xmin = -1, ymin, xmax, ymax, type;
    u32b old_seed;
    object_kind *kind;

    /* Is the building deserted? */
    if (magik(25)) return;

    /* Possibly add a farm */
    if (magik(50))
        wild_add_garden(c, x1, y1, x2, y2, &xmin, &ymin, &xmax, &ymax, &type);

    /* Hack -- If we have created this level before, do not add anything more to it. */
    if (w_ptr->generated) return;

    /* Save the RNG */
    old_seed = Rand_value;

    /* Is someone to be found at this house? */
    if (magik(80)) at_home = TRUE;

    /* Taken over! */
    else if (magik(50)) taken_over = TRUE;

    /* Is there any cash inside? */
    if (magik(50)) cash = wild_radius(c->depth) + randint0(20);

    /* Are there objects to be found? */
    if (magik(50)) num_objects = randint0(randint0(size));

    /* Is there any food inside? */
    if (magik(50)) num_food = randint0(randint0(size));

    /* Add the cash */
    if (cash)
    {
        /* Try to place the cash */
        while (trys < 50)
        {
            x = rand_range(x1, x2);
            y = rand_range(y1, y2);

            if (cave_canputitem(c, y, x))
            {
                place_gold(p, c, y, x, cash, SV_GOLD_ANY, ORIGIN_FLOOR);
                break;
            }
            trys++;
        }
    }

    /* Add the objects */
    trys = 0;
    while (num_objects && (trys < 300))
    {
        x = rand_range(x1, x2);
        y = rand_range(y1, y2);

        if (cave_canputitem(c, y, x))
        {
            place_object(p, c, y, x, object_level(c->depth), FALSE, FALSE, ORIGIN_FLOOR, 0);
            num_objects--;
        }
        trys++;
    }

    /* Add the food */
    trys = 0;
    while (num_food && (trys < 100))
    {
        x = rand_range(x1, x2);
        y = rand_range(y1, y2);

        if (cave_canputitem(c, y, x))
        {
            if (magik(50))
                kind = lookup_kind(TV_FOOD, randint1(SV_FOOD_TROUT));
            else
                kind = lookup_kind(TV_CROP, randint1(SV_FOOD_EAR_OF_CORN));
            object_prep(&forge, kind, 0, RANDOMISE);

            set_origin(&forge, ORIGIN_FLOOR, c->depth, 0);

            drop_near(NULL, c, &forge, 0, y, x, FALSE);

            num_food--;
        }
        trys++;
    }

    /* Add the inhabitants */
    if (at_home)
    {
        /* Determine the home owner's species */
        get_mon_num_hook = wild_monst_aux_home_owner;
        get_mon_num_prep();

        /* Home owners can be tough */
        r_idx = get_mon_num(c->depth, monster_level(c->depth) + 10);

        /* Hack -- Restore the monster selection function */
        get_mon_num_hook = NULL;
        get_mon_num_prep();

        /* Handle failure */
        if (r_idx)
        {
            /* Get the owner's location */
            while (TRUE)
            {
                x = rand_range(x1 - 5, x2 + 5);
                y = rand_range(y1 - 5, y2 + 5);
                if (in_bounds_fully(y, x) &&
                    ((x < x1 - 1) || (x > x2 + 1) || (y < y1 - 1) || (y > y2 + 1)))
                {
                    break;
                }
            }

            /* Place the owner */
            place_new_monster(p, c, y, x, r_idx, 0, ORIGIN_DROP);
        }
    }

    /* Add the invaders */
    if (taken_over)
    {
        /* Determine the invaders species */
        get_mon_num_hook = wild_monst_aux_invaders;
        get_mon_num_prep();
        r_idx = get_mon_num(c->depth, monster_level(c->depth));

        /* Hack -- Restore the monster selection function */
        get_mon_num_hook = NULL;
        get_mon_num_prep();

        /* Handle failure */
        if (r_idx)
        {
            /* Add the monsters */
            for (y = y1; y <= y2; y++)
                for (x = x1; x <= x2; x++)
                    if (magik(50)) place_new_monster(p, c, y, x, r_idx, 0, ORIGIN_DROP);
        }
    }

    /* Restore the RNG */
    Rand_value = old_seed;

    /* No farm has been added */
    if (xmin < 0) return;

    /* Save the RNG (state should not be affected by farm generation) */
    old_seed = Rand_value;

    /* Add crops to the farm */
    wild_add_crops(c, xmin, ymin, xmax, ymax, type);

    /* Restore the RNG */
    Rand_value = old_seed;
}


/*
 * Adds a building to the wilderness. If the coordinate is not given,
 * find it randomly.
 */
static void wild_add_dwelling(struct player *p, struct cave *c, int x, int y)
{
    int h_x1, h_y1, h_x2, h_y2, p_x1, p_y1, p_x2, p_y2, plot_xlen, plot_ylen,
        house_xlen, house_ylen, door_x, door_y, drawbridge_x[3], drawbridge_y[3],
        tmp, type, area, price, num_door_attempts, i;
    char wall_feature, door_feature, has_moat = 0;
    wilderness_type *w_ptr = &wild_info[c->depth];
    bool rand_old = Rand_quick;
    byte floor_info = CAVE_ICKY;
    byte floor_feature = FEAT_FLOOR;

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Find the dimensions of the house */

    /* Chance of being a "large" house */
    if (one_in_(2))
    {
        house_xlen = randint0(10) + randint0(randint0(10)) + 9;
        house_ylen = randint0(5) + randint0(randint0(5)) + 6;
    }

    /* Chance of being a "small" house */
    else if (one_in_(2))
    {
        house_xlen = randint0(4) + 3;
        house_ylen = randint0(2) + 3;
    }

    /* A "normal" house */
    else
    {
        house_xlen = randint0(10) + 3;
        house_ylen = randint0(5) + 3;
    }

    /* Houses are at least 2x2 */
    if (house_xlen == 3) house_xlen++;
    if (house_ylen == 3) house_ylen++;

    area = (house_xlen - 2) * (house_ylen - 2);

    /* Find the dimensions of the "lawn" the house is built on */
    if (area < 30)
    {
        plot_xlen = house_xlen;
        plot_ylen = house_ylen;
    }
    else if (area < 60)
    {
        plot_xlen = house_xlen + (area / 15) * 2;
        plot_ylen = house_ylen + (area / 25) * 2;
    }
    else
    {
        plot_xlen = house_xlen + (area / 8) * 2;
        plot_ylen = house_ylen + (area / 14) * 2;
    }

    /* Hack -- sometimes large buildings get moats */
    if ((area >= 70) && one_in_(16)) has_moat = 1;
    if ((area >= 80) && one_in_(6)) has_moat = 1;
    if ((area >= 100) && one_in_(2)) has_moat = 1;
    if ((area >= 130) && CHANCE(3, 4)) has_moat = 1;
    if (has_moat) plot_xlen += 8;
    if (has_moat) plot_ylen += 8;

    /* Determine the plot's boundaries */
    reserve_building_plot(c, &p_x1, &p_y1, &p_x2, &p_y2, plot_xlen, plot_ylen, x, y);

    /* Determine the building's boundaries */
    h_x1 = p_x1 + ((plot_xlen - house_xlen) / 2);
    h_y1 = p_y1 + ((plot_ylen - house_ylen) / 2);
    h_x2 = p_x2 - ((plot_xlen - house_xlen) / 2);
    h_y2 = p_y2 - ((plot_ylen - house_ylen) / 2);

    /* Return if we didn't get a plot */
    if (p_x1 < 0)
    {
        Rand_quick = rand_old;
        return;
    }

    /* Initialise x and y, which may not be specified at this point */
    x = (h_x1 + h_x2) / 2;
    y = (h_y1 + h_y2) / 2;

    /* Create log cabins by default */
    type = WILD_LOG_CABIN;

    /* Add extra houses near the town */
    if (town_area(c->depth))
    {
        if (num_houses < MAX_HOUSES) type = WILD_TOWN_HOME;
    }

    /* Add arenas outside of town area */
    else if (!has_moat && (area >= 70) && magik(30))
    {
        if (num_arenas < MAX_ARENAS) type = WILD_ARENA;
    }

    /* Select features */
    switch (type)
    {
        case WILD_LOG_CABIN:
        {
            wall_feature = FEAT_LOGS;

            /* Doors are locked half of the time */
            if (magik(50)) door_feature = FEAT_DOOR_HEAD + randint0(7);
            else door_feature = FEAT_DOOR_HEAD;

            break;
        }

        case WILD_TOWN_HOME:
        {
            wall_feature = FEAT_PERM_EXTRA;
            door_feature = FEAT_HOME_HEAD;
            floor_feature = FEAT_FLOOR_SAFE;

            /* This is the dominant term for large houses */
            if (area > 40) price = (area - 40) * (area - 40) * (area - 40) * 3;
            else price = 0;

            /* This is the dominant term for medium houses */
            price += area * area * 33;

            /* This is the dominant term for small houses */
            price += area * (900 + randint0(200));

            /* Remember price */

            /* Hack -- Setup next possible house addition */
            houses[num_houses].price = price;
            houses[num_houses].x_1 = h_x1 + 1;
            houses[num_houses].y_1 = h_y1 + 1;
            houses[num_houses].x_2 = h_x2 - 1;
            houses[num_houses].y_2 = h_y2 - 1;
            houses[num_houses].depth = c->depth;

            break;
        }

        case WILD_ARENA:
        {
            wall_feature = FEAT_PERM_ARENA;
            door_feature = FEAT_PERM_ARENA;
            floor_info = CAVE_ROOM | CAVE_GLOW | CAVE_ICKY;

            arenas[num_arenas].x_1 = h_x1;
            arenas[num_arenas].y_1 = h_y1;
            arenas[num_arenas].x_2 = h_x2;
            arenas[num_arenas].y_2 = h_y2;
            arenas[num_arenas].depth = c->depth;

            break;
        }
    }

    /* Select the door location... */
    /* Done here so we can try to prevent it from being put on water. */
    num_door_attempts = 0;
    do
    {
        /* Pick a door direction (S,N,E,W) */
        tmp = randint0(4);

        /* Extract a "door location" */
        switch (tmp)
        {
            /* Bottom side */
            case DIR_SOUTH:
            {
                door_y = h_y2;
                door_x = rand_range(h_x1, h_x2);
                if (has_moat)
                {
                    drawbridge_y[0] = h_y2 + 1; drawbridge_y[1] = h_y2 + 2;
                    drawbridge_y[2] = h_y2 + 3;
                    drawbridge_x[0] = door_x; drawbridge_x[1] = door_x;
                    drawbridge_x[2] = door_x;
                }
                break;
            }

            /* Top side */
            case DIR_NORTH:
            {
                door_y = h_y1;
                door_x = rand_range(h_x1, h_x2);
                if (has_moat)
                {
                    drawbridge_y[0] = h_y1 - 1; drawbridge_y[1] = h_y1 - 2;
                    drawbridge_y[2] = h_y1 - 3;
                    drawbridge_x[0] = door_x; drawbridge_x[1] = door_x;
                    drawbridge_x[2] = door_x;
                }
                break;
            }

            /* Right side */
            case DIR_EAST:
            {
                door_y = rand_range(h_y1, h_y2);
                door_x = h_x2;
                if (has_moat)
                {
                    drawbridge_y[0] = door_y; drawbridge_y[1] = door_y;
                    drawbridge_y[2] = door_y;
                    drawbridge_x[0] = h_x2 + 1; drawbridge_x[1] = h_x2 + 2;
                    drawbridge_x[2] = h_x2 + 3;
                }
                break;
            }

            /* Left side */
            default:
            {
                door_y = rand_range(h_y1, h_y2);
                door_x = h_x1;
                if (has_moat)
                {
                    drawbridge_y[0] = door_y; drawbridge_y[1] = door_y;
                    drawbridge_y[2] = door_y;
                    drawbridge_x[0] = h_x1 - 1; drawbridge_x[1] = h_x1 - 2;
                    drawbridge_x[2] = h_x1 - 3;
                }
                break;
            }
        }

        /* Access the grid */
        num_door_attempts++;
    }
    while ((c->feat[door_y][door_x] == FEAT_WATER) && (num_door_attempts < 30));

    /* Build a rectangular building */
    for (y = h_y1; y <= h_y2; y++)
    {
        for (x = h_x1; x <= h_x2; x++)
        {
            /* Clear previous contents, add "basic" wall */
            cave_set_feat(c, y, x, wall_feature);
        }
    }

    /* Make it hollow */
    for (y = h_y1 + 1; y < h_y2; y++)
    {
        for (x = h_x1 + 1; x < h_x2; x++)
        {
            /* Fill with floor */
            cave_set_feat(c, y, x, floor_feature);

            /* Make it "icky" and/or glowing */
            c->info[y][x] |= floor_info;
        }
    }

    /* Add the door */
    cave_set_feat(c, door_y, door_x, door_feature);

    /* Build the moat */
    if (has_moat)
    {
        /* North / South */
        for (x = h_x1 - 2; x <= h_x2 + 2; x++)
        {
            cave_set_feat(c, h_y1 - 2, x, FEAT_WATER);
            c->info[h_y1 - 2][x] |= CAVE_ICKY;
            cave_set_feat(c, h_y1 - 3, x, FEAT_WATER);
            c->info[h_y1 - 3][x] |= CAVE_ICKY;
            cave_set_feat(c, h_y2 + 2, x, FEAT_WATER);
            c->info[h_y2 + 2][x] |= CAVE_ICKY;
            cave_set_feat(c, h_y2 + 3, x, FEAT_WATER);
            c->info[h_y2 + 3][x] |= CAVE_ICKY;
        }

        /* East / West */
        for (y = h_y1 - 2; y <= h_y2 + 2; y++)
        {
            cave_set_feat(c, y, h_x1 - 2, FEAT_WATER);
            c->info[y][h_x1 - 2] |= CAVE_ICKY;
            cave_set_feat(c, y, h_x1 - 3, FEAT_WATER);
            c->info[y][h_x1 - 3] |= CAVE_ICKY;
            cave_set_feat(c, y, h_x2 + 2, FEAT_WATER);
            c->info[y][h_x2 + 2] |= CAVE_ICKY;
            cave_set_feat(c, y, h_x2 + 3, FEAT_WATER);
            c->info[y][h_x2 + 3] |= CAVE_ICKY;
        }
        cave_set_feat(c, drawbridge_y[0], drawbridge_x[0], FEAT_DRAWBRIDGE);
        c->info[drawbridge_y[0]][drawbridge_x[0]] |= CAVE_ICKY;
        cave_set_feat(c, drawbridge_y[1], drawbridge_x[1], FEAT_DRAWBRIDGE);
        c->info[drawbridge_y[1]][drawbridge_x[1]] |= CAVE_ICKY;
        cave_set_feat(c, drawbridge_y[2], drawbridge_x[2], FEAT_DRAWBRIDGE);
        c->info[drawbridge_y[2]][drawbridge_x[2]] |= CAVE_ICKY;
    }

    /* Finish making the building */
    switch (type)
    {
        case WILD_LOG_CABIN:
        {
            /* Make the building interesting */
            wild_furnish_dwelling(p, c, h_x1 + 1, h_y1 + 1, h_x2 - 1, h_y2 - 1);

            break;
        }

        case WILD_TOWN_HOME:
        {
            /* Hack -- only add a house if it is not already in memory */
            i = pick_house(c->depth, door_y, door_x);
            if (i == -1)
            {
                cave_set_feat(c, door_y, door_x, door_feature);

                /* Store door location information */
                houses[num_houses].door_y = door_y;
                houses[num_houses].door_x = door_x;
                houses[num_houses].ownername[0] = '\0';
                houses[num_houses].ownerid = 0;
                houses[num_houses].color = 0;

                /* One more house */
                num_houses++;
            }
            else
            {
                /* Tag owned house door */
                cave_set_feat(c, door_y, door_x, door_feature + houses[i].color);
            }

            break;
        }

        case WILD_ARENA:
        {
            /* Hack -- only add arena if it is not already in memory */
            i = pick_arena(c->depth, door_y, door_x);
            if (i == -1) num_arenas++;

            break;
        }
    }

    /* Hack -- use the "complex" RNG */
    Rand_quick = rand_old;
}


/*
 * Auxiliary function to determine_wilderness_type, used for terminating
 * infinite loops of clones pointing at each other. See below. Originially
 * counted the length of the loop, but as virtually all loops turned out
 * to be 2 in length, it was revised to find the total depth of the loop.
 */
static int wild_clone_closed_loop_total(int cur_depth)
{
    int start_depth, total_depth, neigh_idx;
    total_depth = 0;

    /* Save our initial position */
    start_depth = cur_depth;

    /* Until we arrive back at our initial position */
    do
    {
        /* Seed the number generator */
        Rand_value = seed_town + cur_depth * 600;

        /*
         * Hack -- the second rand after the seed is used for the beginning of
         * the clone directions (see below function).  This rand sets things up.
         */
        Rand_div(100);

        /* Get a valid neighbor location */
        do
        {
            neigh_idx = neighbor_index(cur_depth, randint0(4));
        }
        while (!neigh_idx || (neigh_idx < 0 - MAX_WILD));

        /* Move to this new location */
        cur_depth = neigh_idx;

        /* Increase our loop total depth */
        total_depth += cur_depth;

    }
    while (cur_depth != start_depth);

    return total_depth;
}


/*
 * Figure out what kind of terrain a depth is.
 * This function assumes that wild_info's world_x and world_y values have been set.
 *
 * Hack -- Read this for an explanation of the wilderness generation. Each square
 * is seeded with a seed dependent on its depth, and this is used to find its terrain type.
 * If it is of type 'clone', then a random direction is picked, and it becomes the type
 * of terrain that its neighbor is, using recursion if necessary.  This was causing
 * problems with closed loops of clones, so I came up with a mega-hack solution :
 * if we notice we are in a closed loop, find the total depth of the loop by adding
 * all its components, and use this to seed the pseudorandom number generator and set
 * the loops terrain.
 *
 * Note that a lot of this craziness is performed to keep the wilderness' terrain
 * types independent of the order in which they are explored; they are completely defined
 * by the pseudorandom seed seed_town.
 */
int determine_wilderness_type(int depth)
{
    int neighbor_idx, closed_loop = 0 - 0xFFF;
    wilderness_type *w_ptr = &wild_info[depth];
    bool rand_old = Rand_quick;
    u32b old_seed = Rand_value;

    /* Paranoia */
    if (depth > 0) return WILD_UNDEFINED;

    /* Check if the town */
    if (!depth) return WILD_TOWN;

    /* Check if already defined */
    if ((w_ptr->type != WILD_UNDEFINED) && (w_ptr->type != WILD_CLONE))
        return w_ptr->type;

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Hack -- Induce consistant wilderness */
    Rand_value = seed_town + depth * 600;

    /* Check for infinite loops */
    if (w_ptr->type == WILD_CLONE)
    {
        /*
         * Mega-Hack -- we are in a closed loop of clones, find the length of the loop
         * and use this to seed the pseudorandom number generator.
         */
        closed_loop = wild_clone_closed_loop_total(depth);
        Rand_value = seed_town + closed_loop * 8973;
    }

    /* Hack -- Initialize the "simple" RNG */
    Rand_div(100);

    /*
     * Randomly determine the level type
     *
     * Hack -- if already a clone at this point, we are in a closed loop.  We
     * terminate the loop by picking a nonclone terrain type.  Yes, this prevents
     * "large" features from forming, but the resulting terrain is still rather
     * pleasing.
     */
    if (w_ptr->type != WILD_CLONE) w_ptr->type = WILD_CLONE;
    else
    {
        static int wild_chance[][2] =
        {
            {WILD_SHORE, 8},
            {WILD_GRASS, 50},
            {WILD_WOOD, 10},
            {WILD_SWAMP, 7},
            {WILD_WASTE, 15},
            {WILD_MOUNTAIN, 5},
            {WILD_VOLCANO, 5}
        };
        int i;

        do {i = randint0(N_ELEMENTS(wild_chance));}
        while (!magik(wild_chance[i][1]));

        w_ptr->type = wild_chance[i][0];
    }

    /* If a "clone", copy the terrain type from a neighbor, and recurse if necessary. */
    if (w_ptr->type == WILD_CLONE)
    {
        neighbor_idx = 0;

        /* Get a legal neighbor index */
        /* Illegal locations -- the town and off the edge */
        while (!neighbor_idx || (neighbor_idx < 0 - MAX_WILD))
        {
            /* Pick a random direction */
            neighbor_idx = neighbor_index(depth, randint0(4));
        }

        /* Recursively figure out our terrain type */
        w_ptr->type = determine_wilderness_type(neighbor_idx);

        /* Only WILD_GRASS next to town to preserve houses */
        if (town_area(depth)) w_ptr->type = WILD_GRASS;
    }

    /* Hack -- use the "complex" RNG */
    Rand_quick = rand_old;

    /* Hack -- don't touch number generation. */
    Rand_value = old_seed;

    return w_ptr->type;
}


typedef struct
{
    int type;
    int grass;
    int mud;
    int water;
    int tree;
    int eviltree;
    int mountain;
    int dirt;
    int lava;
} terrain_type;


/*
 * Determines terrain composition.
 * Separated from gen_wilderness_aux for bleed functions.
 */
static void init_terrain(terrain_type *t_ptr, int type)
{
    /* Initialize the terrain type */
    WIPE(t_ptr, terrain_type);
    t_ptr->type = type;

    switch (t_ptr->type)
    {
        /* Shore */
        case WILD_SHORE:
        {
            t_ptr->grass = 30;
            t_ptr->water = 39;
            t_ptr->tree = 1;
            t_ptr->dirt = 30;
            break;
        }

        /* Grass */
        case WILD_GRASS:
        {
            t_ptr->grass = 97;
            t_ptr->water = 1;
            t_ptr->tree = 1;
            t_ptr->dirt = 1;
            break;
        }

        /* Wood */
        case WILD_WOOD:
        {
            t_ptr->grass = 30;
            t_ptr->water = 1;
            t_ptr->tree = 39;
            t_ptr->dirt = 30;
            break;
        }

        /* Swamp */
        case WILD_SWAMP:
        {
            t_ptr->grass = 30;
            t_ptr->water = 20;
            t_ptr->tree = 20;
            t_ptr->dirt = 30;
            break;
        }

        /* Waste */
        case WILD_WASTE:
        {
            t_ptr->grass = 1;
            t_ptr->eviltree = 1;
            t_ptr->mountain = 1;
            t_ptr->dirt = 97;
            break;
        }

        /* Mountain */
        case WILD_MOUNTAIN:
        {
            t_ptr->grass = 30;
            t_ptr->eviltree = 1;
            t_ptr->mountain = 39;
            t_ptr->dirt = 30;
            break;
        }

        /* Volcano */
        case WILD_VOLCANO:
        {
            t_ptr->eviltree = 1;
            t_ptr->mountain = 15;
            t_ptr->dirt = 60;
            t_ptr->lava = 24;
            break;
        }

        /* Hack -- Mud pit */
        case WILD_MUDPIT:
        {
            t_ptr->grass = 10;
            t_ptr->mud = 60;
            t_ptr->water = 10;
            t_ptr->tree = 10;
            t_ptr->dirt = 10;
            break;
        }

        /* Hack -- Scorched forest */
        case WILD_SCORCHED:
        {
            t_ptr->grass = 30;
            t_ptr->eviltree = 39;
            t_ptr->mountain = 1;
            t_ptr->dirt = 30;
            break;
        }
    }
}


static char terrain_spot(const terrain_type *terrain)
{
    char feat = FEAT_NONE;
    int i;

    while (feat == FEAT_NONE)
    {
        i = randint0(8);

        switch (i)
        {
            case 0: if (magik(terrain->grass)) feat = FEAT_GRASS; break;
            case 1: if (magik(terrain->mud)) feat = FEAT_MUD; break;
            case 2: if (magik(terrain->water)) feat = FEAT_WATER; break;
            case 3: if (magik(terrain->tree)) feat = FEAT_TREE; break;
            case 4: if (magik(terrain->eviltree)) feat = FEAT_EVIL_TREE; break;
            case 5: if (magik(terrain->mountain)) feat = FEAT_MOUNTAIN; break;
            case 6: if (magik(terrain->dirt)) feat = FEAT_DIRT; break;
            case 7: if (magik(terrain->lava)) feat = FEAT_LAVA; break;
        }
    }

    return feat;
}


static void wild_add_grass_hotspot(int magnitude, int *type, bool *add_dwelling)
{
    *type = WILD_GRASS;

    /* Sometimes a dwelling */
    if (magnitude > 8)
    {
        if (magik(25)) *add_dwelling = TRUE;
    }
}


/*
 * Adds hotspots.
 * Done to make the levels a bit more interesting.
 *
 * Chopiness defines the randomness of the circular shape.
 */
static void wild_add_hotspot(struct player *p, struct cave *c)
{
    int x_cen, y_cen, max_mag, magnitude = 0, magsqr, chopiness, x, y;
    terrain_type hot_terrain;
    bool add_dwelling = FALSE;
    wilderness_type *w_ptr = &wild_info[c->depth];
    int type = wild_info[c->depth].type;

    /* Hack -- Minimum hotspot radius of 3 */
    while (magnitude < 3)
    {
        /* Determine the rough "coordinates" of the feature */
        x_cen = randint0(DUNGEON_WID - 11) + 5;
        y_cen = randint0(DUNGEON_HGT - 11) + 5;

        /*
         * Determine the maximum size of the feature, which is its distance to
         * its closest edge.
         */
        max_mag = y_cen;
        if (x_cen < max_mag) max_mag = x_cen;
        if ((DUNGEON_HGT - y_cen) < max_mag) max_mag = DUNGEON_HGT - y_cen;
        if ((DUNGEON_WID - x_cen) < max_mag) max_mag = DUNGEON_WID - x_cen;

        /*
         * Determine the magnitude of the feature.  the triple rand is done to
         * keep most features small, but have a rare large one.
         */
        magnitude = randint0(randint0(randint0(max_mag)));
    }

    /* Hack -- Take the square to avoid square roots */
    magsqr = magnitude * magnitude;

    /* The "roughness" of the hotspot */
    chopiness = 2 * magsqr / (randint0(5) + 1);

    /* Initialize the terrain type */
    switch (type)
    {
        /* Shore */
        case WILD_SHORE:
        {
            /* Land */
            wild_add_grass_hotspot(magnitude, &type, &add_dwelling);

            break;
        }

        /* Grass */
        case WILD_GRASS:
        {
            /* Sometimes a pond */
            if (magik(50)) type = WILD_SHORE;

            /* Otherwise a glade */
            else type = WILD_WOOD;

            break;
        }

        /* Wood */
        case WILD_WOOD:
        {
            /* Sometimes a pond */
            if (magik(50)) type = WILD_SHORE;

            /* Otherwise a clearing */
            else wild_add_grass_hotspot(magnitude, &type, &add_dwelling);

            break;
        }

        /* Swamp */
        case WILD_SWAMP:
        {
            /* Sometimes a pond */
            if (magik(50)) type = WILD_SHORE;

            /* Otherwise a mud pit */
            else type = WILD_MUDPIT;

            break;
        }

        /* Waste */
        case WILD_WASTE:
        {
            /* Sometimes a scorched forest */
            if (magik(50)) type = WILD_SCORCHED;

            /* Otherwise some hills */
            else type = WILD_MOUNTAIN;

            break;
        }

        /* Mountain */
        case WILD_MOUNTAIN:
        {
            /* Sometimes a scorched forest */
            if (magik(50)) type = WILD_SCORCHED;

            /* Otherwise some wasteland */
            else type = WILD_WASTE;

            break;
        }

        /* Volcano */
        case WILD_VOLCANO:
        {
            /* Sometimes some hills */
            if (magik(50)) type = WILD_MOUNTAIN;

            /* Otherwise some wasteland */
            else type = WILD_WASTE;

            break;
        }
    }

    /* Determine the terrain components */
    init_terrain(&hot_terrain, type);

    /* Create the hotspot */
    for (y = y_cen - magnitude; y <= y_cen + magnitude; y++)
    {
        for (x = x_cen - magnitude; x <= x_cen + magnitude; x++)
        {
            /* a^2 + b^2 = c^2... the rand makes the edge less defined */
            /* Hack -- multiply the y's by 4 to "squash" the shape */
            if (((x - x_cen) * (x - x_cen) + (y - y_cen) * (y - y_cen) * 4) <
                (magsqr + randint0(chopiness)))
                    cave_set_feat(c, y, x, terrain_spot(&hot_terrain));
        }
    }

    /* Add inhabitants */
    if (add_dwelling) wild_add_dwelling(p, c, x_cen, y_cen);
}


/* Helper function to wild_gen_bleedmap */
static void wild_gen_bleedmap_aux(int *bleedmap, int span, char dir)
{
    int c = 0, above, below, noise_mag, rand_noise, bleedmag;

    /* Make a pass of the bleedmap */
    while (c < DUNGEON_WID)
    {
        /* Check that its clear */
        if (bleedmap[c] == 0xFFFF)
        {
            /* If these are aligned right, they shouldn't overflow */
            if (bleedmap[c - span] != 0xFFFF) above = bleedmap[c - span];
            else above = 0;
            if (bleedmap[c + span] != 0xFFFF) below = bleedmap[c + span];
            else below = 0;

            noise_mag = (dir % 2)? 70: 25;

            /* Randomness proportional to span */
            rand_noise = ((randint0(noise_mag * 2) - noise_mag) * span) / 64;
            bleedmag = ((above + below) / 2) + rand_noise;

            /* Bounds checking */
            if (bleedmag < 0) bleedmag = 0;
            if (bleedmag > (DUNGEON_HGT - 1) / 2) bleedmag = (DUNGEON_HGT - 1) / 2;

            /* Set the bleed magnitude */
            bleedmap[c] = bleedmag;
        }

        c += span;
    }

    span /= 2;

    /* Do the next level of recursion */
    if (span) wild_gen_bleedmap_aux(bleedmap, span, dir);
}


/*
 * Using a simple fractal algorithm, generates the bleedmap used by the function below.
 * Hack -- for this algorithm to work nicely, an initial span of a power of 2 is required.
 */
static void wild_gen_bleedmap(int *bleedmap, char dir, int start, int end)
{
    int c = 0, bound;

    /* Initialize the bleedmap */
    for (c = 0; c <= 256; c++) bleedmap[c] = 0xFFFF;

    /* Initialize the "top" and "bottom" */
    if (start < 0) bleedmap[0] = randint0(((dir % 2)? 70: 25));
    else bleedmap[0] = start;
    if (end < 0) bleedmap[256] = randint0(((dir % 2)? 70: 25));
    else
    {
        bound = (dir % 2)? DUNGEON_HGT - 3: DUNGEON_WID - 3;
        for (c = bound; c <= 256; c++) bleedmap[c] = end;
    }

    /*
     * Hack -- if the start and end are zeroed, add something in the middle
     * to make exciting stuff happen.
     */
    if (!start && !end)
    {
        /* East or west */
        if (dir % 2) bleedmap[32] = randint0(40) + 15;

        /* North or south */
        else
        {
            bleedmap[64] = randint0(20) + 8;
            bleedmap[128] = randint0(20) + 8;
        }
    }

    /* Generate the bleedmap */
    wild_gen_bleedmap_aux(bleedmap, 128, dir);

    /* Hack -- no bleedmags less than 8 except near the edges */
    bound = (dir % 2)? DUNGEON_HGT - 1: DUNGEON_WID - 1;

    /* Beginning to middle */
    for (c = 0; c < 8; c++)
    {
        if (bleedmap[c] < c) bleedmap[c] = c;
    }

    /* Middle */
    for (c = 8; c < bound - 8; c++)
    {
        if (bleedmap[c] < 8) bleedmap[c] = randint0(3) + 8;
    }

    /* Middle to end */
    for (c = bound - 8; c < bound; c++)
    {
        if (bleedmap[c] < bound - c) bleedmap[c] = bound - c;
    }
}


/*
 * This function "bleeds" the terrain type of bleed_from to the side of c->depth
 * specified by dir.
 *
 * First, a bleedmap array is initialized using a simple fractal algorithm.
 * This map specifies the magnitude of the bleed at each point along the edge.
 * After this, the two structures bleed_begin and bleed_end are initialized.
 *
 * After this structure is initialized, for each point along the bleed edge,
 * up until the bleedmap[point] edge of the bleed, the terrain is set to
 * that of bleed_from.
 *
 * We should hack this to add interesting features near the bleed edge.
 * Such as ponds near shoreline to make it more interesting and
 * groves of trees near the edges of forest.
 */
static void wild_bleed_level(struct cave *c, int bleed_from, char dir, int start, int end)
{
    int x, y;
    int bleedmap[257], bleed_begin[DUNGEON_WID], bleed_end[DUNGEON_WID];
    terrain_type terrain;

    /* Sanity check */
    if (wild_info[bleed_from].type == wild_info[c->depth].type) return;

    /* Determine the terrain components */
    init_terrain(&terrain, wild_info[bleed_from].type);

    /* Generate the bleedmap */
    wild_gen_bleedmap(bleedmap, dir, start, end);

    /* Initialize the bleedruns */
    switch (dir)
    {
        case DIR_EAST:
        {
            for (y = 1; y < DUNGEON_HGT - 1; y++)
            {
                bleed_begin[y] = DUNGEON_WID - bleedmap[y];
                bleed_end[y] = DUNGEON_WID - 1;
            }
            break;
        }
        case DIR_WEST:
        {
            for (y = 1; y < DUNGEON_HGT - 1; y++)
            {
                bleed_begin[y] = 1;
                bleed_end[y] = bleedmap[y];
            }
            break;
        }
        case DIR_NORTH:
        {
            for (x = 1; x < DUNGEON_WID - 1; x++)
            {
                bleed_begin[x] = 1;
                bleed_end[x] = bleedmap[x];
            }
            break;
        }
        case DIR_SOUTH:
        {
            for (x = 1; x < DUNGEON_WID - 1; x++)
            {
                bleed_begin[x] = DUNGEON_HGT - bleedmap[x];
                bleed_end[x] = DUNGEON_HGT - 1;
            }
            break;
        }
    }

    if ((dir == DIR_EAST) || (dir == DIR_WEST))
    {
        for (y = 1; y < DUNGEON_HGT - 1; y++)
        {
            for (x = bleed_begin[y]; x < bleed_end[y]; x++)
            {
                cave_set_feat(c, y, x, terrain_spot(&terrain));
            }
        }
    }
    else
    {
        for (x = 1; x < DUNGEON_WID - 1; x++)
        {
            for (y = bleed_begin[x]; y < bleed_end[x]; y++)
            {
                cave_set_feat(c, y, x, terrain_spot(&terrain));
            }
        }
    }
}


/*
 * Determines whether or not to bleed from a given depth in a given direction.
 * Useful for initial determination, as well as shared bleed points.
 */
static bool should_we_bleed(int depth, char dir)
{
    int neigh_idx = 0, tmp;

    /* Get our neighbors index */
    neigh_idx = neighbor_index(depth, dir);

    /* Determine whether to bleed or not (if a valid location) */
    if ((neigh_idx >= 0 - MAX_WILD) && (neigh_idx < 0))
    {
        /* Make sure the level type is defined */
        wild_info[neigh_idx].type = determine_wilderness_type(neigh_idx);

        /* Check if our neighbor is of a different type */
        if (wild_info[depth].type != wild_info[neigh_idx].type)
        {
            /* Determine whether to bleed or not */
            Rand_value = seed_town + (depth + neigh_idx) * (93754);
            tmp = randint0(2);
            if (tmp && (depth < neigh_idx)) return TRUE;
            if (!tmp && (depth > neigh_idx)) return TRUE;
            return FALSE;
        }
        return FALSE;
    }
    return FALSE;
}


/*
 * To determine whether we bleed into our neighbor or whether our neighbor
 * bleeds into us, we seed the random number generator with our combined
 * depth.  If the resulting number is 0, we bleed into the greater (negative
 * wise) level.  Other wise we bleed into the lesser (negative wise) level.
 *
 * I added in shared points.... turning this function into something extremly
 * gross. This will be extremly annoying to get working. I wish I had a simpler
 * way of doing this.
 */
static void bleed_with_neighbors(struct cave *cv)
{
    int c, d, neigh_idx[4], tmp, side[2], start, end, opposite;
    wilderness_type *w_ptr = &wild_info[cv->depth];
    bool do_bleed[4], bleed_zero[4];
    int share_point[4][2];
    u32b old_seed = Rand_value;
    bool rand_old = Rand_quick;

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Get our neighbors indices */
    for (c = 0; c < 4; c++) neigh_idx[c] = neighbor_index(cv->depth, c);

    /* For each neighbor, determine whether to bleed or not */
    for (c = 0; c < 4; c++) do_bleed[c] = should_we_bleed(cv->depth, c);

    /* Calculate the bleed_zero values */
    for (c = 0; c < 4; c++)
    {
        tmp = c - 1;
        if (tmp < 0) tmp = 3;

        if ((neigh_idx[tmp] >= 0 - MAX_WILD) && (neigh_idx[tmp] < 0) &&
            (neigh_idx[c] >= 0 - MAX_WILD) && (neigh_idx[c] < 0))
        {
            if (wild_info[neigh_idx[tmp]].type == wild_info[neigh_idx[c]].type)
            {
                /* Calculate special case bleed zero values. */
                if (do_bleed[c])
                {
                    /* If get the opposite direction from tmp */
                    opposite = tmp - 2;
                    if (opposite < 0) opposite += 4;

                    /* If the other one is bleeding towards us */
                    if (should_we_bleed(neigh_idx[tmp], opposite))
                        bleed_zero[c] = TRUE;
                    else bleed_zero[c] = FALSE;

                }
                else if (do_bleed[tmp])
                {
                    /* Get the opposite direction from c */
                    opposite = c - 2;
                    if (opposite < 0) opposite += 4;

                    /* If the other one is bleeding towards us */
                    if (should_we_bleed(neigh_idx[c], opposite))
                        bleed_zero[c] = TRUE;
                    else bleed_zero[c] = FALSE;
                }

                else bleed_zero[c] = FALSE;
            }
            else bleed_zero[c] = TRUE;
        }
        else bleed_zero[c] = FALSE;
    }

    /* Calculate bleed shared points */
    for (c = 0; c < 4; c++)
    {
        side[0] = c - 1;
        if (side[0] < 0) side[0] = 3;
        side[1] = c + 1;
        if (side[1] > 3) side[1] = 0;

        /* If this direction is bleeding */
        if (do_bleed[c])
        {
            /* For the left and right sides */
            for (d = 0; d <= 1; d++)
            {
                /* If we have a valid neighbor */
                if ((neigh_idx[side[d]] < 0) && (neigh_idx[side[d]] >= 0 - MAX_WILD))
                {
                    /* If our neighbor is bleeding in a similar way */
                    if (should_we_bleed(neigh_idx[side[d]], c))
                    {
                        /* Are we a simmilar type of terrain */
                        if (wild_info[neigh_idx[side[d]]].type == w_ptr->type)
                        {
                            /* Share a point */
                            /* Seed the number generator */
                            Rand_value = seed_town +
                                (cv->depth + neigh_idx[side[d]]) * (89791);
                            share_point[c][d] = randint0(((c % 2)? 70: 25));
                        }
                        else share_point[c][d] = 0;
                    }
                    else share_point[c][d] = 0;
                }
                else share_point[c][d] = 0;
            }
        }
        else
        {
            share_point[c][0] = 0;
            share_point[c][1] = 0;
        }
    }

    /* Do the bleeds */
    for (c = 0; c < 4; c++)
    {
        tmp = c + 1;
        if (tmp > 3) tmp = 0;
        if (do_bleed[c])
        {
            if (!share_point[c][0] && !bleed_zero[c]) start = -1;
            else if (share_point[c][0]) start = share_point[c][0];
            else start = 0;

            if (!share_point[c][1] && !bleed_zero[tmp]) end = -1;
            else if (share_point[c][1]) end = share_point[c][1];
            else end = 0;

            if (c < 2)
                wild_bleed_level(cv, neigh_idx[c], c, start, end);
            else
                wild_bleed_level(cv, neigh_idx[c], c, end, start);
        }
    }

    /* Hack -- restore the random number generator */
    Rand_value = old_seed;
    Rand_quick = rand_old;
}


static void wilderness_gen_hack(struct player *p, struct cave *c)
{
    int y, x, x1, x2, y1, y2;
    terrain_type terrain;
    u32b tmp_seed = Rand_value;
    bool rand_old = Rand_quick;
    wilderness_type *w_ptr = &wild_info[c->depth];
    int dwelling = 0, radius = wild_radius(c->depth);

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Hack -- Induce consistant wilderness */
    Rand_value = seed_town + c->depth * 600;

    /* If not already set, determine the type of terrain */
    if (w_ptr->type == WILD_UNDEFINED)
        w_ptr->type = determine_wilderness_type(c->depth);

    /* Initialize the terrain */
    init_terrain(&terrain, w_ptr->type);

    /* Hack -- Set the monster level */
    w_ptr->monst_lev = radius;

    /* Tougher at night */
    if (!is_daytime() && !town_area(c->depth))
        w_ptr->monst_lev = (4 + w_ptr->monst_lev) * 3 / 2;

    /* Hack -- Start with basic floors */
    for (y = 1; y < DUNGEON_HGT - 1; y++)
    {
        for (x = 1; x < DUNGEON_WID - 1; x++)
        {
            cave_set_feat(c, y, x, terrain_spot(&terrain));
        }
    }

    /* To make the borders between wilderness levels more seamless, "bleed" the levels together */
    bleed_with_neighbors(c);

    /* Hack -- reseed, just to make sure everything stays consistent. */
    Rand_value = seed_town + c->depth * 287 + 490836;

    /* To make the level more interesting, add some "hotspots" */
    /* Only if not close to town to preserve houses */
    if (!town_area(c->depth))
    {
        for (y = 0; y < randint0(11); y++) wild_add_hotspot(p, c);
    }

    /* Add dwellings on grass */
    if (w_ptr->type == WILD_GRASS)
    {
        dwelling = 250;

        /* Hack -- If close to the town, make dwellings more likely */
        if (radius == 1) dwelling *= 40;
        if (radius == 2) dwelling *= 8;
        if (radius == 3) dwelling *= 2;
    }

    /*
     * Hack -- 50% of the time on a radius 1 level there will be a "park" which will make
     * the rest of the level more densely packed together
     */
    if ((radius == 1) && one_in_(2))
        reserve_building_plot(c, &x1, &y1, &x2, &y2, randint0(30) + 15, randint0(20) + 10, -1, -1);

    /* Add wilderness dwellings */
    while (dwelling > 0)
    {
        /* Hack -- The number of dwellings is proportional to their chance of existing */
        if (CHANCE(dwelling, 1000)) wild_add_dwelling(p, c, -1, -1);
        dwelling -= 50;
    }

    /* Clear the CAVE_TEMP flag */
    for (y = 0; y < DUNGEON_HGT; y++)
        for (x = 0; x < DUNGEON_WID; x++)
            c->info[y][x] &= ~(CAVE_TEMP);

    /* Hack -- Use the "complex" RNG */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;

    /* Place objects on this level */
    place_objects(c->depth);
}


/* Generates a wilderness level. */
bool wilderness_gen(struct cave *c, struct player *p)
{
    int i, y, x;
    wilderness_type *w_ptr;

    my_assert(c);

    c->height = DUNGEON_HGT;
    c->width = DUNGEON_WID;

    w_ptr = &wild_info[c->depth];

    /* Perma-walls -- North/South */
    for (x = 0; x < DUNGEON_WID; x++)
    {
        /* Clear previous contents, add "clear" perma-wall */
        cave_set_feat(c, 0, x, FEAT_PERM_CLEAR);
        cave_set_feat(c, DUNGEON_HGT - 1, x, FEAT_PERM_CLEAR);
    }

    /* Perma-walls -- West/East */
    for (y = 0; y < DUNGEON_HGT; y++)
    {
        /* Clear previous contents, add "clear" perma-wall */
        cave_set_feat(c, y, 0, FEAT_PERM_CLEAR);
        cave_set_feat(c, y, DUNGEON_WID - 1, FEAT_PERM_CLEAR);
    }

    /* Hack -- Build some wilderness (from memory) */
    wilderness_gen_hack(p, c);

    /* Apply illumination */
    cave_illuminate(p, c, (is_daytime()? TRUE: FALSE));

    /* Make some residents */
    for (i = 0; i < wild_radius(c->depth); i++) wild_add_monster(p, c);

    /*
     * Set if we have generated the level before, to determine
     * whether or not to respawn objects
     */
    w_ptr->generated = TRUE;

    return TRUE;
}


/* Add wilderness position in "03N, 12E" format to string "buf" */
void wild_cat_depth(int depth, char *buf, int len)
{
    int cur_x, cur_y;

    if (!depth)
    {
        my_strcat(buf, "Town", len);
        return;
    }

    if (depth > 0)
    {
        my_strcat(buf, "Dungeon", len);
        return;
    }

    cur_x = wild_info[depth].world_x;
    cur_y = wild_info[depth].world_y;

    my_strcat(buf, "[", len);
    if (cur_y)
        my_strcat(buf, format("%d%c", abs(cur_y), ((cur_y > 0)? 'N': 'S')), len);
    if (cur_y && cur_x) my_strcat(buf, ", ", len);
    if (cur_x)
        my_strcat(buf, format("%d%c", abs(cur_x), ((cur_x > 0)? 'E': 'W')), len);
    my_strcat(buf, "]", len);
}


bool wild_is_explored(struct player *p, int idx)
{
    /* Hack -- DM has knowledge of the full world */
    if (p->dm_flags & DM_SEE_LEVEL) return TRUE;

    return (p->wild_map[idx / 8] & (1 << (idx % 8)));
}


void wild_set_explored(struct player *p, int idx)
{
    p->wild_map[idx / 8] |= (1 << (idx % 8));
}
