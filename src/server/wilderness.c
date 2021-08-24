/*
 * File: wilderness.c
 * Purpose: Wilderness generation
 *
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


/* Hack -- consistent wilderness layout */
u32b seed_wild;


/* The information about arenas */
arena_type arenas[MAX_ARENAS];
u16b num_arenas = 0;


/*
 * The information about wilderness levels
 */
wilderness_type world_info[MAX_WILD + 1];
wilderness_type *wild_info = &(world_info[MAX_WILD]);


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
static bool wild_monst_aux_shore(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_SHORE)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_grass(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_GRASS)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_wood(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_WOOD)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_swamp(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_SWAMP)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_waste(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_WASTE)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_mountain(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_MOUNTAIN)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_volcano(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_VOLCANO)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Place a monster in the wilderness
 */
void wild_add_monster(struct player *p, struct chunk *c)
{
    int monst_x, monst_y;
    int tries = 50;
    struct monster_race *race;

    /* Prepare allocation table */
    switch (wild_info[c->depth].type)
    {
        case WILD_SHORE: get_mon_num_prep(wild_monst_aux_shore); break;
        case WILD_GRASS: get_mon_num_prep(wild_monst_aux_grass); break;
        case WILD_WOOD: get_mon_num_prep(wild_monst_aux_wood); break;
        case WILD_SWAMP: get_mon_num_prep(wild_monst_aux_swamp); break;
        case WILD_WASTE: get_mon_num_prep(wild_monst_aux_waste); break;
        case WILD_MOUNTAIN: get_mon_num_prep(wild_monst_aux_mountain); break;
        case WILD_VOLCANO: get_mon_num_prep(wild_monst_aux_volcano); break;
    }

    /* Find a legal, unoccupied space */
    while (true)
    {
        tries--;

        /* Handle failure */
        if (!tries) return;

        monst_x = randint0(c->width);
        monst_y = randint0(c->height);

        /* Hack -- don't place monster in an arena */
        if (pick_arena(c->depth, monst_y, monst_x) != -1) continue;

        if (square_isempty(c, monst_y, monst_x)) break;
    }

    /* Get the monster */
    race = get_mon_num(c, monster_level(c->depth));

    /* Prepare allocation table */
    get_mon_num_prep(NULL);

    /* Handle failure */
    if (!race) return;

    /* Place the monster */
    place_new_monster(p, c, monst_y, monst_x, race, MON_GROUP, ORIGIN_DROP);
}


static bool plot_clear(struct chunk *c, byte **plot, int *x1, int *y1, int *x2, int *y2)
{
    int x, y;

    /* Check if its clear */
    for (y = *y1; y <= *y2; y++)
    {
        for (x = *x1; x <= *x2; x++)
        {
            /* Don't build on other buildings or farms */
            if (square_isplot(c, y, x)) return false;

            /* Any ickiness on the plot is NOT allowed */
            if (square_isvault(c, y, x)) return false;

            /* Spaces that have already been reserved are NOT allowed */
            if (plot[y][x] == 1) return false;
        }
    }

    /* Buildings and farms can partially, but not completely, be built on water. */
    if (square_iswater(c, *y1, *x1) && square_iswater(c, *y2, *x2))
        return false;

    return true;
}


/*
 * Choose a clear building location, possibly specified by (xcen, ycen),
 * and "reserves" it so nothing else can choose any of its squares for building again
 */
static void reserve_building_plot(struct chunk *c, byte **plot, int *x1, int *y1, int *x2, int *y2,
    int xlen, int ylen, int xcen, int ycen)
{
    int x, y, attempts = 0;

    while (attempts < 20)
    {
        /* If xcen, ycen have not been specified */
        if (!square_in_bounds_fully(c, ycen, xcen))
        {
            /* The upper left corner */
            *x1 = randint0(c->width - xlen - 4) + 2;
            *y1 = randint0(c->height - ylen - 4) + 2;

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
        if (!square_in_bounds_fully(c, *y1, *x1) || !square_in_bounds_fully(c, *y2, *x2))
        {
            *x1 = *y1 = *x2 = *y2 = -1;
            return;
        }

        /* If we have a clear plot, reserve it and return */
        if (plot_clear(c, plot, x1, y1, x2, y2))
        {
            for (y = *y1; y <= *y2; y++)
                for (x = *x1; x <= *x2; x++)
                    plot[y][x] = 1;
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
static void wild_add_garden(struct chunk *c, byte **plot, int xmin, int ymin, int xmax, int ymax,
    int *x1, int *y1, int *x2, int *y2, int *type)
{
    int xlen, ylen, xcen, ycen, orientation;

    /* Choose a 'good' size for the garden */
    xlen = randint0(15) + 15;
    ylen = randint0(7) + 7;

    /* Choose a 'good' location for the garden */
    while (true)
    {
        xcen = rand_range(xmin - xlen, xmax + xlen);
        ycen = rand_range(ymin - ylen, ymax + ylen);
        if (square_in_bounds_fully(c, ycen, xcen) &&
            ((xcen < xmin - xlen / 2) || (xcen > xmax + xlen / 2) ||
            (ycen < ymin - ylen / 2) || (ycen > ymax + ylen / 2))) break;
    }

    reserve_building_plot(c, plot, x1, y1, x2, y2, xlen, ylen, xcen, ycen);

    /* If we failed to obtain a valid plot */
    if (*x1 < 0) return;

    /* Choose which type of garden it is */
    *type = randint0(7);

    /* Whether the crop rows are horizontal or vertical */
    orientation = randint0(2);

    /* Initially fill with a layer of dirt */
    fill_dirt(c, *y1, *x1, *y2, *x2);

    /* Alternating rows of crops */
    add_crop(c, *y1 + 1, *x1 + 1, *y2 - 1, *x2 - 1, orientation);
}


/*
 * Adds crop to a given location.
 */
void wild_add_crop(struct chunk *c, int x, int y, int type)
{
    struct object *food = object_new();

    /* Food choice */
    switch (type)
    {
        case WILD_CROP_POTATO:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Potato"), 0, RANDOMISE);
            break;

        case WILD_CROP_CABBAGE:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Head of Cabbage"), 0, RANDOMISE);
            break;

        case WILD_CROP_CARROT:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Carrot"), 0, RANDOMISE);
            break;

        case WILD_CROP_BEET:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Beet"), 0, RANDOMISE);
            break;

        case WILD_CROP_MUSHROOM:
        {
            /* Hack -- mushrooms are rare */
            static struct mushroom_crop
            {
                int tval;
                char *name;
                int chance;
            } shroom_chance[] =
            {
                {TV_MUSHROOM, "Second Sight", 2},
                {TV_MUSHROOM, "Fast Recovery", 5},
                {TV_MUSHROOM, "Vigor", 1},
                {TV_MUSHROOM, "Clear Mind", 5},
                {TV_MUSHROOM, "Emergency", 5},
                {TV_MUSHROOM, "Terror", 5},
                {TV_MUSHROOM, "Stoneskin", 5},
                {TV_MUSHROOM, "Debility", 5},
                {TV_MUSHROOM, "Sprinting", 5},
                {TV_MUSHROOM, "Purging", 5},
                {TV_FOOD, "Slime Mold", 100}
            };
            int i;

            do {i = randint0(N_ELEMENTS(shroom_chance));}
            while (!magik(shroom_chance[i].chance));

            object_prep(NULL, food,
                lookup_kind_by_name(shroom_chance[i].tval, shroom_chance[i].name), 0, RANDOMISE);
            break;
        }

        case WILD_CROP_SQUASH:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Squash"), 0, RANDOMISE);
            break;

        case WILD_CROP_CORN:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Ear of Corn"), 0, RANDOMISE);
            break;
    }

    /* Drop food */
    set_origin(food, ORIGIN_FLOOR, c->depth, 0);
    drop_near(NULL, c, food, 0, y, x, false, DROP_FADE);
}


/*
 * Adds crops to a given garden.
 */
static void wild_add_crops(struct chunk *c, int x1, int y1, int x2, int y2, int type)
{
    int x, y;

    /* Alternating rows of crops */
    for (y = y1 + 1; y <= y2 - 1; y++)
    {
        for (x = x1 + 1; x <= x2 - 1; x++)
        {
            /* Different orientations */
            if (!square_iscrop(c, y, x)) continue;

            /* Random chance of food */
            if (magik(60)) continue;

            wild_add_crop(c, x, y, type);
        }
    }
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_invaders(struct monster_race *race)
{
    /* Is the monster humanoid? */
    if (is_humanoid(race)) return true;

    /* Is the monster half-humanoid? */
    if (is_half_humanoid(race)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_home_owner(struct monster_race *race)
{
    /* Is the monster humanoid? */
    return is_humanoid(race);
}


/*
 * Make a dwelling 'interesting'
 */
static void wild_furnish_dwelling(struct player *p, struct chunk *c, byte **plot, int x1, int y1,
    int x2, int y2)
{
    wilderness_type *w_ptr = &wild_info[c->depth];
    bool at_home = false, taken_over = false;
    int num_food = 0, cash = 0, num_objects = 0;
    int x, y, trys;
    int size = (x2 - x1) * (y2 - y1);
    int xmin = -1, ymin, xmax, ymax, type;
    u32b old_seed;
    struct object_kind *kind;
    struct monster_race *race;

    /* Is the building deserted? */
    if (magik(25)) return;

    /* Possibly add a farm */
    if (magik(50))
        wild_add_garden(c, plot, x1, y1, x2, y2, &xmin, &ymin, &xmax, &ymax, &type);

    /* Hack -- if we have created this level before, do not add anything more to it. */
    if (w_ptr->generated != WILD_NONE) return;

    /* Mark level as furnished (objects + inhabitants) */
    w_ptr->generated = WILD_FURNISHED;

    /* Save the RNG */
    old_seed = Rand_value;

    /* Is someone to be found at this house? */
    if (magik(80)) at_home = true;

    /* Taken over! */
    else if (magik(50)) taken_over = true;

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

            if (square_canputitem(c, y, x))
            {
                place_gold(p, c, y, x, cash, ORIGIN_FLOOR);
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

        if (square_canputitem(c, y, x))
        {
            place_object(p, c, y, x, object_level(c->depth), false, false, ORIGIN_FLOOR, 0);
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

        if (square_canputitem(c, y, x))
        {
            struct object *food = object_new();

            if (magik(50))
                kind = lookup_kind(TV_FOOD, randint1(kb_info[TV_FOOD].num_svals));
            else
                kind = lookup_kind(TV_CROP, randint1(kb_info[TV_CROP].num_svals));
            object_prep(NULL, food, kind, 0, RANDOMISE);

            set_origin(food, ORIGIN_FLOOR, c->depth, 0);

            drop_near(NULL, c, food, 0, y, x, false, DROP_FADE);

            num_food--;
        }
        trys++;
    }

    /* Add the inhabitants */
    if (at_home)
    {
        /* Determine the home owner's species */
        get_mon_num_prep(wild_monst_aux_home_owner);

        /* Home owners can be tough */
        race = get_mon_num(c, monster_level(c->depth) + 10);

        /* Prepare allocation table */
        get_mon_num_prep(NULL);

        /* Handle failure */
        if (race)
        {
            /* Get the owner's location */
            while (true)
            {
                x = rand_range(x1 - 5, x2 + 5);
                y = rand_range(y1 - 5, y2 + 5);

                if (!square_in_bounds_fully(c, y, x)) continue;

                /* Hack -- don't place monster in an arena */
                if (pick_arena(c->depth, y, x) != -1) continue;

                if ((x < x1 - 1) || (x > x2 + 1) || (y < y1 - 1) || (y > y2 + 1))
                    break;
            }

            /* Place the owner */
            place_new_monster(p, c, y, x, race, 0, ORIGIN_DROP);
        }
    }

    /* Add the invaders */
    if (taken_over)
    {
        /* Determine the invaders species */
        get_mon_num_prep(wild_monst_aux_invaders);
        race = get_mon_num(c, monster_level(c->depth));

        /* Prepare allocation table */
        get_mon_num_prep(NULL);

        /* Handle failure */
        if (race)
        {
            /* Add the monsters */
            for (y = y1; y <= y2; y++)
            {
                for (x = x1; x <= x2; x++)
                {
                    if (magik(50)) continue;
                    place_new_monster(p, c, y, x, race, 0, ORIGIN_DROP);
                }
            }
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
static void wild_add_dwelling(struct player *p, struct chunk *c, byte **plot, int x, int y)
{
    int h_x1, h_y1, h_x2, h_y2, p_x1, p_y1, p_x2, p_y2, plot_xlen, plot_ylen,
        house_xlen, house_ylen, door_x, door_y, drawbridge_x[3], drawbridge_y[3],
        tmp, type, area, price, num_door_attempts, i;
    byte door_feature, has_moat = 0;
    int radius = wild_radius(c->depth);
    bool rand_old = Rand_quick;

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Find the dimensions of the house */

    /* PWMAngband: 50% "small" and 50% "medium" houses in immediate suburb */
    if (radius <= 1)
    {
        /* Chance of being a "small" house */
        if (one_in_(2))
        {
            house_xlen = randint0(4) + 3;
            house_ylen = randint0(2) + 3;
        }

        /* A "normal" house */
        else
        {
            house_xlen = randint0(10) + 6;
            house_ylen = randint0(5) + 4;
        }
    }

    /* PWMAngband: 50% "large" and 50% "medium" houses/buildings elsewhere */
    else
    {
        /* Chance of being a "large" house */
        if (one_in_(2))
        {
            house_xlen = randint0(10) + randint0(randint0(10)) + 9;
            house_ylen = randint0(5) + randint0(randint0(5)) + 6;
        }

        /* A "normal" house */
        else
        {
            house_xlen = randint0(10) + 6;
            house_ylen = randint0(5) + 4;
        }
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
    reserve_building_plot(c, plot, &p_x1, &p_y1, &p_x2, &p_y2, plot_xlen, plot_ylen, x, y);

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

    /* Initialize x and y, which may not be specified at this point */
    x = (h_x1 + h_x2) / 2;
    y = (h_y1 + h_y2) / 2;

    /* Create log cabins by default */
    type = WILD_LOG_CABIN;

    /* Add extra houses near the town */
    if (town_area(c->depth)) type = WILD_TOWN_HOME;

    /* Add arenas outside of town area */
    else if (!has_moat && (area >= 70) && magik(30))
    {
        if (num_arenas < MAX_ARENAS) type = WILD_ARENA;
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
    while (square_iswater(c, door_y, door_x) && (num_door_attempts < 30));

    /* Build a rectangular building and make it hollow */
    door_feature = add_building(c, h_y1, h_x1, h_y2, h_x2, type);

    /* Add the door */
    square_set_feat(c, door_y, door_x, door_feature);

    /* Build the moat */
    if (has_moat)
        add_moat(c, h_y1, h_x1, h_y2, h_x2, drawbridge_y, drawbridge_x);

    /* Finish making the building */
    switch (type)
    {
        case WILD_LOG_CABIN:
        {
            /* Make the building interesting */
            wild_furnish_dwelling(p, c, plot, h_x1 + 1, h_y1 + 1, h_x2 - 1, h_y2 - 1);

            break;
        }

        case WILD_TOWN_HOME:
        {
            /* This is the dominant term for large houses */
            if (area > 40) price = (area - 40) * (area - 40) * (area - 40) * 3;
            else price = 0;

            /* This is the dominant term for medium houses */
            price += area * area * 33;

            /* This is the dominant term for small houses */
            price += area * (900 + randint0(200));

            /* Hack -- only add a house if it is not already in memory */
            i = pick_house(c->depth, door_y, door_x);
            if (i == -1)
            {
                house_type h_local;

                square_set_feat(c, door_y, door_x, door_feature);

                /* Get an empty house slot */
                i = house_add(false);

                /* Setup house info */
                h_local.x_1 = h_x1 + 1;
                h_local.y_1 = h_y1 + 1;
                h_local.x_2 = h_x2 - 1;
                h_local.y_2 = h_y2 - 1;
                h_local.door_y = door_y;
                h_local.door_x = door_x;
                h_local.depth = c->depth;
                h_local.price = price;
                h_local.ownerid = 0;
                h_local.ownername[0] = '\0';
                h_local.color = 0;
                h_local.state = HOUSE_NORMAL;
                h_local.free = 0;

                /* Add a house to our houses list */
                house_set(i, &h_local);
            }
            else
            {
                /* Tag owned house door */
                square_set_feat(c, door_y, door_x, door_feature + house_get(i)->color);
            }

            break;
        }

        case WILD_ARENA:
        {
            /* Hack -- only add arena if it is not already in memory */
            i = pick_arena(c->depth, door_y, door_x);
            if (i == -1)
            {
                arenas[num_arenas].x_1 = h_x1;
                arenas[num_arenas].y_1 = h_y1;
                arenas[num_arenas].x_2 = h_x2;
                arenas[num_arenas].y_2 = h_y2;
                arenas[num_arenas].depth = c->depth;
                num_arenas++;
            }

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
        Rand_value = seed_wild + cur_depth * 600;

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
 * Hack -- read this for an explanation of the wilderness generation. Each square
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
 * by the pseudorandom seed seed_wild.
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

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Hack -- induce consistant wilderness */
    Rand_value = seed_wild + depth * 600;

    /* Check for infinite loops */
    if (w_ptr->type == WILD_CLONE)
    {
        /*
         * Hack -- we are in a closed loop of clones, find the length of the loop
         * and use this to seed the pseudorandom number generator.
         */
        closed_loop = wild_clone_closed_loop_total(depth);
        Rand_value = seed_wild + closed_loop * 8973;
    }

    /* Hack -- initialize the "simple" RNG */
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


/* Grass, mud, water, tree, evil tree, mountain, dirt, lava */
#define TERRAIN_TYPE_MAX    8


typedef struct
{
    int type;
    int chance[TERRAIN_TYPE_MAX];
} terrain_type;


static terrain_type terrain_types[12] =
{
    /* Type, % grass, % mud, % water, % tree, % evil tree, % mountain, % dirt, % lava */
    {WILD_UNDEFINED, {0, 0, 0, 0, 0, 0, 0, 0}},
    {WILD_SHORE, {30, 0, 39, 1, 0, 0, 30, 0}},
    {WILD_GRASS, {97, 0, 1, 1, 0, 0, 1, 0}},
    {WILD_WOOD, {30, 0, 1, 39, 0, 0, 30, 0}},
    {WILD_SWAMP, {30, 0, 20, 20, 0, 0, 30, 0}},
    {WILD_WASTE, {1, 0, 0, 0, 1, 1, 97, 0}},
    {WILD_MOUNTAIN, {30, 0, 0, 0, 1, 39, 30, 0}},
    {WILD_VOLCANO, {0, 0, 0, 0, 1, 15, 60, 24}},
    {WILD_CLONE, {0, 0, 0, 0, 0, 0, 0, 0}},
    {WILD_TOWN, {0, 0, 0, 0, 0, 0, 0, 0}},
    {WILD_MUDPIT, {10, 60, 10, 10, 0, 0, 10, 0}},
    {WILD_SCORCHED, {30, 0, 0, 0, 39, 1, 30, 0}}
};


/*
 * Determines terrain composition.
 * Separated from gen_wilderness_aux for bleed functions.
 */
static void init_terrain(terrain_type *t_ptr, int type)
{
    /* Initialize the terrain type */
    memcpy(t_ptr, &terrain_types[type], sizeof(terrain_type));
}


static byte terrain_spot(const terrain_type *terrain)
{
    int type;

    while (true)
    {
        type = randint0(TERRAIN_TYPE_MAX);
        if (magik(terrain->chance[type])) break;
    }

    return feat_terrain(type);
}


static void wild_add_grass_hotspot(int magnitude, int *type, bool *add_dwelling)
{
    *type = WILD_GRASS;

    /* Sometimes a dwelling */
    if (magnitude > 8)
    {
        if (magik(25)) *add_dwelling = true;
    }
}


/*
 * Adds hotspots.
 * Done to make the levels a bit more interesting.
 *
 * Chopiness defines the randomness of the circular shape.
 */
static void wild_add_hotspot(struct player *p, struct chunk *c, byte **plot)
{
    int x_cen, y_cen, max_mag, magnitude = 0, magsqr, chopiness, x, y;
    terrain_type hot_terrain;
    bool add_dwelling = false;
    int type = wild_info[c->depth].type;

    /* Hack -- minimum hotspot radius of 3 */
    while (magnitude < 3)
    {
        /* Determine the rough "coordinates" of the feature */
        x_cen = randint0(c->width - 11) + 5;
        y_cen = randint0(c->height - 11) + 5;

        /*
         * Determine the maximum size of the feature, which is its distance to
         * its closest edge.
         */
        max_mag = y_cen;
        if (x_cen < max_mag) max_mag = x_cen;
        if ((c->height - y_cen) < max_mag) max_mag = c->height - y_cen;
        if ((c->width - x_cen) < max_mag) max_mag = c->width - x_cen;

        /*
         * Determine the magnitude of the feature.  the triple rand is done to
         * keep most features small, but have a rare large one.
         */
        magnitude = randint0(randint0(randint0(max_mag)));
    }

    /* Hack -- take the square to avoid square roots */
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
            {
                square_set_feat(c, y, x, terrain_spot(&hot_terrain));
            }
        }
    }

    /* Add inhabitants */
    if (add_dwelling) wild_add_dwelling(p, c, plot, x_cen, y_cen);
}


/* Helper function to wild_gen_bleedmap */
static void wild_gen_bleedmap_aux(int *bleedmap, int span, char dir, int width, int height)
{
    int c = 0, above, below, noise_mag, rand_noise, bleedmag;

    /* Make a pass of the bleedmap */
    while (c < width)
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
            if (bleedmag > (height - 1) / 2) bleedmag = (height - 1) / 2;

            /* Set the bleed magnitude */
            bleedmap[c] = bleedmag;
        }

        c += span;
    }

    span /= 2;

    /* Do the next level of recursion */
    if (span) wild_gen_bleedmap_aux(bleedmap, span, dir, width, height);
}


/*
 * Using a simple fractal algorithm, generates the bleedmap used by the function below.
 * Hack -- for this algorithm to work nicely, an initial span of a power of 2 is required.
 */
static void wild_gen_bleedmap(int *bleedmap, char dir, int start, int end, int width, int height)
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
        bound = (dir % 2)? height - 3: width - 3;
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
    wild_gen_bleedmap_aux(bleedmap, 128, dir, width, height);

    /* Hack -- no bleedmags less than 8 except near the edges */
    bound = (dir % 2)? height - 1: width - 1;

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
static void wild_bleed_level(struct chunk *c, int bleed_from, char dir, int start, int end)
{
    int x, y;
    int bleedmap[257] = { 0 }, *bleed_begin, *bleed_end;
    terrain_type terrain;

    /* Sanity check */
    if (wild_info[bleed_from].type == wild_info[c->depth].type) return;

    /* Determine the terrain components */
    init_terrain(&terrain, wild_info[bleed_from].type);

    /* Generate the bleedmap */
    wild_gen_bleedmap(bleedmap, dir, start, end, c->width, c->height);

    bleed_begin = mem_zalloc(z_info->dungeon_wid * sizeof(int));
    bleed_end = mem_zalloc(z_info->dungeon_wid * sizeof(int));

    /* Initialize the bleedruns */
    switch (dir)
    {
        case DIR_EAST:
        {
            for (y = 1; y < c->height - 1; y++)
            {
                bleed_begin[y] = c->width - bleedmap[y];
                bleed_end[y] = c->width - 1;
            }
            break;
        }
        case DIR_WEST:
        {
            for (y = 1; y < c->height - 1; y++)
            {
                bleed_begin[y] = 1;
                bleed_end[y] = bleedmap[y];
            }
            break;
        }
        case DIR_NORTH:
        {
            for (x = 1; x < c->width - 1; x++)
            {
                bleed_begin[x] = 1;
                bleed_end[x] = bleedmap[x];
            }
            break;
        }
        case DIR_SOUTH:
        {
            for (x = 1; x < c->width - 1; x++)
            {
                bleed_begin[x] = c->height - bleedmap[x];
                bleed_end[x] = c->height - 1;
            }
            break;
        }
        default: break;
    }

    if ((dir == DIR_EAST) || (dir == DIR_WEST))
    {
        for (y = 1; y < c->height - 1; y++)
            for (x = bleed_begin[y]; x < bleed_end[y]; x++)
                square_set_feat(c, y, x, terrain_spot(&terrain));
    }
    else if ((dir == DIR_NORTH) || (dir == DIR_SOUTH))
    {
        for (x = 1; x < c->width - 1; x++)
            for (y = bleed_begin[x]; y < bleed_end[x]; y++)
                square_set_feat(c, y, x, terrain_spot(&terrain));
    }

    mem_free(bleed_begin);
    mem_free(bleed_end);
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
            Rand_value = seed_wild + (depth + neigh_idx) * (93754);
            tmp = randint0(2);
            if (tmp && (depth < neigh_idx)) return true;
            if (!tmp && (depth > neigh_idx)) return true;
            return false;
        }
        return false;
    }
    return false;
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
static void bleed_with_neighbors(struct chunk *cv)
{
    int c, d, neigh_idx[4], tmp, side[2], start, end, opposite;
    wilderness_type *w_ptr = &wild_info[cv->depth];
    bool do_bleed[4], bleed_zero[4];
    int share_point[4][2];
    u32b old_seed = Rand_value;
    bool rand_old = Rand_quick;

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

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
                        bleed_zero[c] = true;
                    else bleed_zero[c] = false;

                }
                else if (do_bleed[tmp])
                {
                    /* Get the opposite direction from c */
                    opposite = c - 2;
                    if (opposite < 0) opposite += 4;

                    /* If the other one is bleeding towards us */
                    if (should_we_bleed(neigh_idx[c], opposite))
                        bleed_zero[c] = true;
                    else bleed_zero[c] = false;
                }

                else bleed_zero[c] = false;
            }
            else bleed_zero[c] = true;
        }
        else bleed_zero[c] = false;
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
                            Rand_value = seed_wild +
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


static void wilderness_gen_hack(struct player *p, struct chunk *c)
{
    int y, x, x1, x2, y1, y2;
    terrain_type terrain;
    u32b tmp_seed = Rand_value;
    bool rand_old = Rand_quick;
    wilderness_type *w_ptr = &wild_info[c->depth];
    int dwelling = 0, radius = wild_radius(c->depth);
    byte **plot;

    plot = mem_zalloc(c->height * sizeof(byte*));
    for (y = 0; y < c->height; y++)
        plot[y] = mem_zalloc(c->width * sizeof(byte));

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Hack -- induce consistant wilderness */
    Rand_value = seed_wild + c->depth * 600;

    /* If not already set, determine the type of terrain */
    if (w_ptr->type == WILD_UNDEFINED)
        w_ptr->type = determine_wilderness_type(c->depth);

    /* Initialize the terrain */
    init_terrain(&terrain, w_ptr->type);

    /* Hack -- set the monster level */
    w_ptr->monst_lev = radius;

    /* Tougher at night */
    if (!is_daytime() && !town_area(c->depth))
        w_ptr->monst_lev = (4 + w_ptr->monst_lev) * 3 / 2;

    /* Hack -- start with basic floors */
    for (y = 1; y < c->height - 1; y++)
        for (x = 1; x < c->width - 1; x++)
            square_set_feat(c, y, x, terrain_spot(&terrain));

    /* To make the borders between wilderness levels more seamless, "bleed" the levels together */
    bleed_with_neighbors(c);

    /* Hack -- reseed, just to make sure everything stays consistent. */
    Rand_value = seed_wild + c->depth * 287 + 490836;

    /* To make the level more interesting, add some "hotspots" */
    /* Only if not close to town to preserve houses */
    if (!town_area(c->depth))
    {
        for (y = 0; y < randint0(11); y++) wild_add_hotspot(p, c, plot);
    }

    /* Add dwellings on grass */
    if (w_ptr->type == WILD_GRASS)
    {
        dwelling = 250;

        /* Hack -- if close to the town, make dwellings more likely */
        if (radius == 1) dwelling *= 80;
        if (radius == 2) dwelling *= 25;
        if (radius == 3) dwelling *= 8;
        if (radius == 4) dwelling *= 2;
    }

    /*
     * Hack -- 50% of the time on a radius 1 level there will be a "park" which will make
     * the rest of the level more densely packed together
     */
    if ((radius == 1) && one_in_(2))
    {
        reserve_building_plot(c, plot, &x1, &y1, &x2, &y2, randint0(30) + 15, randint0(20) + 10,
            -1, -1);
    }

    /* Add wilderness dwellings */
    while (dwelling > 0)
    {
        /* Hack -- the number of dwellings is proportional to their chance of existing */
        if (CHANCE(dwelling, 1000)) wild_add_dwelling(p, c, plot, -1, -1);
        dwelling -= 50;
    }

    /* Hack -- use the "complex" RNG */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;

    for (y = 0; y < c->height; y++)
        mem_free(plot[y]);
    mem_free(plot);
}


/* Generates a wilderness level. */
struct chunk *wilderness_gen(struct player *p)
{
    int i;
    wilderness_type *w_ptr;
    enum wild_gen state;
    struct chunk *c = cave_new(z_info->dungeon_hgt, z_info->dungeon_wid);

    c->depth = p->depth;
    player_cave_new(p, z_info->dungeon_hgt, z_info->dungeon_wid);

    w_ptr = &wild_info[c->depth];
    state = w_ptr->generated;

    /* Create boundary */
    draw_rectangle(c, 0, 0, c->height - 1, c->width - 1, FEAT_PERM_CLEAR, SQUARE_NONE);

    /* Hack -- build some wilderness (from memory) */
    wilderness_gen_hack(p, c);

    /* Apply illumination */
    player_cave_clear(p, true);
    cave_illuminate(p, c, is_daytime());

    /* Make some residents */
    for (i = 0; i < wild_radius(c->depth); i++) wild_add_monster(p, c);

    /* Mark levels without dwellings as generated */
    if (w_ptr->generated == WILD_NONE) w_ptr->generated = WILD_GENERATED;

    /* Mark regenerated levels with dwellings as deserted */
    if (state == WILD_FURNISHED) w_ptr->generated = WILD_DESERTED;

    return c;
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
    if (p->dm_flags & DM_SEE_LEVEL) return true;

    return ((p->wild_map[idx / 8] & (1 << (idx % 8)))? true: false);
}


void wild_set_explored(struct player *p, int idx)
{
    p->wild_map[idx / 8] |= (1 << (idx % 8));
}


void wild_deserted_message(struct player *p)
{
    /* Not on levels which contain (other) players */
    if (chunk_get_player_count(p->depth) > 1) return;

    /* Not in town or special levels */
    if (forbid_special(p->depth)) return;

    /* Not on levels which contain houses owned by players */
    if (level_has_owned_houses(p->depth)) return;

    /* Add message */
    if (wild_info[p->depth].generated == WILD_DESERTED)
        msg(p, "This seems to be a deserted area...");
}
