
/* File: gen_rooms.cpp */

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

// This file has the code for building most of the different room types.
// With the exception of starburst rooms and fractal areas.



/*
 * Room building routines.
 *
 * Six basic room types + 3 vault types:
 *   1 -- normal
 *   2 -- overlapping
 *   3 -- cross shaped
 *   4 -- large room with features
 *   5 -- monster nests
 *   6 -- monster pits
 *   7 -- simple vaults
 *   8 -- greater vaults
 *   9 -- quest vaults
 */


/*
 * Create up to "num" objects near the given coordinates
 * Only really called by some of the "vault" routines.
 */
static void vault_objects(int y, int x, int num)
{
    int i, j, k;

    /* Attempt to place 'num' objects */
    for (; num > 0; --num)
    {
        /* Try up to 11 spots looking for empty space */
        for (i = 0; i < 11; ++i)
        {
            /* Pick a random location */
            while (1)
            {
                j = rand_spread(y, 2);
                k = rand_spread(x, 3);
                if (!in_bounds(j, k)) continue;
                break;
            }

            /* Require "clean" floor space */
            if (!cave_clean_bold(j, k)) continue;

            /* Place an item */
            if (rand_int(100) < 75)
            {
                place_object(j, k, FALSE, FALSE, DROP_TYPE_UNTHEMED);
            }

            /* Place gold */
            else
            {
                place_gold(j, k);
            }

            /* Placement accomplished */
            break;
        }
    }
}


/*
 * Place a trap with a given displacement of point
 */
static void vault_trap_aux(int y, int x, int yd, int xd)
{
    int count, y1, x1;

    /* Place traps */
    for (count = 0; count <= 5; count++)
    {
        /* Get a location */
        while (1)
        {
            y1 = rand_spread(y, yd);
            x1 = rand_spread(x, xd);
            if (!in_bounds(y1, x1)) continue;
            break;
        }

        /* Require "trappable" floor grids. Ignore monsters */
        if (!cave_trappable_bold(y1, x1)) continue;

        /* Place the trap */
        place_trap(y1, x1, 0);

        /* Done */
        break;
    }
}


/*
 * Place some traps with a given displacement of given location
 */
static void vault_traps(int y, int x, int yd, int xd, int num)
{
    int i;

    for (i = 0; i < num; i++)
    {
        vault_trap_aux(y, x, yd, xd);
    }
}


/*
 * Place some sleeping monsters near the given location
 */
static void vault_monsters(int y1, int x1, int num)
{
    int k, i, y, x;
    int mon_level_old = monster_level;

    /* Temporary increase monster level */
    monster_level += 2;

    /* Try to summon "num" monsters "near" the given location */
    for (k = 0; k < num; k++)
    {
        /* Try nine locations */
        for (i = 0; i < 9; i++)
        {
            int d = 1;

            /* Pick a nearby location */
            scatter(&y, &x, y1, x1, d, 0);

            /* Require "empty" floor grids */
            if (!cave_empty_bold(y, x)) continue;

            /* Place the monster (allow groups) */
            (void)place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP));

            break;
        }
    }

    /* Restore monster level */
    monster_level = mon_level_old;
}

/*
 * Generate helper -- draw a rectangle with a feature
 */
static void generate_draw(int y1, int x1, int y2, int x2, u16b feat)
{
    int y, x;

    for (y = y1; y <= y2; y++)
    {
        cave_set_feat(y, x1, feat);
        cave_set_feat(y, x2, feat);
    }

    for (x = x1; x <= x2; x++)
    {
        cave_set_feat(y1, x, feat);
        cave_set_feat(y2, x, feat);
    }
}


/*
 * Generate helper -- split a rectangle with a feature
 */
static void generate_plus(int y1, int x1, int y2, int x2, u16b feat)
{
    int y, x;
    int y0, x0;

    /* Center */
    y0 = (y1 + y2) / 2;
    x0 = (x1 + x2) / 2;

    for (y = y1; y <= y2; y++)
    {
        cave_set_feat(y, x0, feat);
    }

    for (x = x1; x <= x2; x++)
    {
        cave_set_feat(y0, x, feat);
    }
}


/*
 * Generate helper -- open all sides of a rectangle with a feature
 */
static void generate_open(int y1, int x1, int y2, int x2, u16b feat)
{
    int y0, x0;

    /* Center */
    y0 = (y1 + y2) / 2;
    x0 = (x1 + x2) / 2;

    /* Open all sides */
    cave_set_feat(y1, x0, feat);
    cave_set_feat(y0, x1, feat);
    cave_set_feat(y2, x0, feat);
    cave_set_feat(y0, x2, feat);
}


/*
 * Generate helper -- create a new room with optional light
 */
static void generate_room(int y1, int x1, int y2, int x2, int light)
{
    int y, x;

    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            dungeon_info[y][x].cave_info |= (CAVE_ROOM);

            if (light) dungeon_info[y][x].cave_info |= (CAVE_GLOW);
        }
    }
    /*
     * Rooms can be placed over big lit lakes sometimes. We have
     * to explicitly remove the light in those cases. Note that outer
     * walls are ignored (you can see the room from the lake).
     */
    if (!light)
    {
        for (y = y1 + 1; y < y2; y++)
        {
            for (x = x1 + 1; x < x2; x++)
            {
                dungeon_info[y][x].cave_info &= ~(CAVE_GLOW);
            }
        }
    }
 }


/*
 * Generate helper -- fill a rectangle with a feature
 */
void generate_fill(int y1, int x1, int y2, int x2, u16b feat)
{
    int y, x;

    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            cave_set_feat(y, x, feat);
        }
    }
}

/*
 * Generate helper -- open one side of a rectangle with a feature
 */
static void generate_hole(int y1, int x1, int y2, int x2, u16b feat)
{
    int y0, x0;

    /* Center */
    y0 = (y1 + y2) / 2;
    x0 = (x1 + x2) / 2;

    /* Open random side */
    switch (rand_int(4))
    {
        case 0:
        {
            cave_set_feat(y1, x0, feat);
            break;
        }
        case 1:
        {
            cave_set_feat(y0, x1, feat);
            break;
        }
        case 2:
        {
            cave_set_feat(y2, x0, feat);
            break;
        }
        case 3:
        {
            cave_set_feat(y0, x2, feat);
            break;
        }
    }
}


/*
 * Type 1 -- normal rectangular rooms
 */
void build_type1(int y0, int x0)
{
    int y, x;

    int y1, x1, y2, x2;

    int light = FALSE;

    /* Occasional light */
    if (p_ptr->depth <= randint(25)) light = TRUE;

    /* Pick a room size */
    y1 = y0 - randint(4);
    x1 = x0 - randint(11);
    y2 = y0 + randint(3);
    x2 = x0 + randint(11);

    /* Generate new room */
    generate_room(y1-1, x1-1, y2+1, x2+1, light);

    /* Generate outer walls */
    generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_GRANITE_OUTER);

    /* Generate inner floors */
    generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

    /* Hack -- Occasional pillar room */
    if (one_in_(20))
    {
        for (y = y1; y <= y2; y += 2)
        {
            for (x = x1; x <= x2; x += 2)
            {
                cave_set_feat(y, x, FEAT_WALL_GRANITE_INNER);
            }
        }
    }

    /* Hack -- Occasional ragged-edge room */
    else if (one_in_(50))
    {
        for (y = y1 + 2; y <= y2 - 2; y += 2)
        {
            cave_set_feat(y, x1, FEAT_WALL_GRANITE_INNER);
            cave_set_feat(y, x2, FEAT_WALL_GRANITE_INNER);
        }

        for (x = x1 + 2; x <= x2 - 2; x += 2)
        {
            cave_set_feat(y1, x, FEAT_WALL_GRANITE_INNER);
            cave_set_feat(y2, x, FEAT_WALL_GRANITE_INNER);
        }
    }
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
void build_type2(int y0, int x0)
{
    int y1a, x1a, y2a, x2a;
    int y1b, x1b, y2b, x2b;

    int light = FALSE;


    /* Occasional light */
    if (p_ptr->depth <= randint(25)) light = TRUE;

    /* Determine extents of room (a) */
    y1a = y0 - randint(4);
    x1a = x0 - randint(11);
    y2a = y0 + randint(3);
    x2a = x0 + randint(10);

    /* Determine extents of room (b) */
    y1b = y0 - randint(3);
    x1b = x0 - randint(10);
    y2b = y0 + randint(4);
    x2b = x0 + randint(11);

    /* Generate new room (a) */
    generate_room(y1a-1, x1a-1, y2a+1, x2a+1, light);

    /* Generate new room (b) */
    generate_room(y1b-1, x1b-1, y2b+1, x2b+1, light);

    /* Generate outer walls (a) */
    generate_draw(y1a-1, x1a-1, y2a+1, x2a+1, FEAT_WALL_GRANITE_OUTER);

    /* Generate outer walls (b) */
    generate_draw(y1b-1, x1b-1, y2b+1, x2b+1, FEAT_WALL_GRANITE_OUTER);

    /* Generate inner floors (a) */
    generate_fill(y1a, x1a, y2a, x2a, FEAT_FLOOR);

    /* Generate inner floors (b) */
    generate_fill(y1b, x1b, y2b, x2b, FEAT_FLOOR);
}


/*
 * Type 3 -- Cross shaped rooms
 *
 * Room "a" runs north/south, and Room "b" runs east/east
 * So a "central pillar" would run from x1a,y1b to x2a,y2b.
 *
 * Note that currently, the "center" is always 3x3, but I think that
 * the code below will work for 5x5 (and perhaps even for unsymetric
 * values like 4x3 or 5x3 or 3x4 or 3x5).
 */
void build_type3(int y0, int x0)
{
    int y, x;

    int y1a, x1a, y2a, x2a;
    int y1b, x1b, y2b, x2b;

    int dy, dx, wy, wx;

    int light = FALSE;


    /* Occasional light */
    if (p_ptr->depth <= randint(25)) light = TRUE;

    /* Pick inner dimension */
    wy = 1;
    wx = 1;

    /* Pick outer dimension */
    dy = rand_range(3, 4);
    dx = rand_range(3, 11);

    /* Determine extents of room (a) */
    y1a = y0 - dy;
    x1a = x0 - wx;
    y2a = y0 + dy;
    x2a = x0 + wx;

    /* Determine extents of room (b) */
    y1b = y0 - wy;
    x1b = x0 - dx;
    y2b = y0 + wy;
    x2b = x0 + dx;

    /* Generate new room (a) */
    generate_room(y1a-1, x1a-1, y2a+1, x2a+1, light);

    /* Generate new room (b) */
    generate_room(y1b-1, x1b-1, y2b+1, x2b+1, light);

    /* Generate outer walls (a) */
    generate_draw(y1a-1, x1a-1, y2a+1, x2a+1, FEAT_WALL_GRANITE_OUTER);

    /* Generate outer walls (b) */
    generate_draw(y1b-1, x1b-1, y2b+1, x2b+1, FEAT_WALL_GRANITE_OUTER);

    /* Generate inner floors (a) */
    generate_fill(y1a, x1a, y2a, x2a, FEAT_FLOOR);

    /* Generate inner floors (b) */
    generate_fill(y1b, x1b, y2b, x2b, FEAT_FLOOR);

    /* Special features */
    switch (randint(4))
    {
        /* Nothing */
        case 1:
        {
            break;
        }

        /* Large solid middle pillar */
        case 2:
        {
            /* Generate a small inner solid pillar */
            generate_fill(y1b, x1a, y2b, x2a, FEAT_WALL_GRANITE_INNER);

            break;
        }

        /* Inner treasure vault */
        case 3:
        {
            /* Generate a small inner vault */
            generate_draw(y1b, x1a, y2b, x2a, FEAT_WALL_GRANITE_INNER);

            /* Open the inner vault with a secret door */
            generate_hole(y1b, x1a, y2b, x2a, get_secret_door_num());

            /* Place a treasure in the vault */
            place_object(y0, x0, FALSE, FALSE, DROP_TYPE_UNTHEMED);

            /* Let's guard the treasure well */
            vault_monsters(y0, x0, rand_int(2) + 3);

            /* Traps naturally */
            vault_traps(y0, x0, 4, 4, rand_int(3) + 2);

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
                    cave_set_feat(y, x1a - 1, FEAT_WALL_GRANITE_INNER);
                    cave_set_feat(y, x2a + 1, FEAT_WALL_GRANITE_INNER);
                }

                /* Pinch the north/south sides */
                for (x = x1a; x <= x2a; x++)
                {
                    if (x == x0) continue;
                    cave_set_feat(y1b - 1, x, FEAT_WALL_GRANITE_INNER);
                    cave_set_feat(y2b + 1, x, FEAT_WALL_GRANITE_INNER);
                }

                /* Open sides with secret doors */
                if (one_in_(3))
                {
                    generate_open(y1b-1, x1a-1, y2b+1, x2a+1, get_secret_door_num());
                }
            }

            /* Occasionally put a "plus" in the center */
            else if (one_in_(3))
            {
                generate_plus(y1b, x1a, y2b, x2a, FEAT_WALL_GRANITE_INNER);
            }

            /* Occasionally put a "pillar" in the center */
            else if (one_in_(3))
            {
                cave_set_feat(y0, x0, FEAT_WALL_GRANITE_INNER);
            }

            break;
        }
    }
}


/*
 * Type 4 -- Large room with an inner room
 *
 * Possible sub-types:
 *	1 - An inner room
 *	2 - An inner room with a small inner room
 *	3 - An inner room with a pillar or pillars
 *	4 - An inner room with a checkerboard
 *	5 - An inner room with four compartments
 */
void build_type4(int y0, int x0)
{
    int y, x, y1, x1, y2, x2;

    int light = FALSE;


    /* Occasional light */
    if (p_ptr->depth <= randint(25)) light = TRUE;

    /* Large room */
    y1 = y0 - 4;
    y2 = y0 + 4;
    x1 = x0 - 11;
    x2 = x0 + 11;

    /* Generate new room */
    generate_room(y1-1, x1-1, y2+1, x2+1, light);

    /* Generate outer walls */
    generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_GRANITE_OUTER);

    /* Generate inner floors */
    generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

    /* The inner room */
    y1 = y1 + 2;
    y2 = y2 - 2;
    x1 = x1 + 2;
    x2 = x2 - 2;

    /* Generate inner walls */
    generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_GRANITE_INNER);

    /* Inner room variations */
    switch (randint(5))
    {
        /* An inner room */
        case 1:
        {
            /* Open the inner room with a secret door */
            generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

            /* Place a monster in the room */
            vault_monsters(y0, x0, 1);

            break;
        }

        /* An inner room with a small inner room */
        case 2:
        {
            u16b feat;

            /* Open the inner room with a secret door */
            generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

            /* Place another inner room */
            generate_draw(y0-1, x0-1, y0+1, x0+1, FEAT_WALL_GRANITE_INNER);

            /*Get a locked door*/

            /*Set the hook */
            get_feat_num_hook = vault_locked_door;
            get_feat_num_prep();

            /* Click! */
            feat = get_feat_num(p_ptr->depth);

            /* Clear the hook */
            get_feat_num_hook = NULL;
            get_feat_num_prep();

            /* Open the inner room with a locked door */
            generate_hole(y0-1, x0-1, y0+1, x0+1, feat);

            /* Monsters to guard the treasure */
            vault_monsters(y0, x0, randint(3) + 2);

            /* Object (80%) */
            if (rand_int(100) < 80)
            {
                place_object(y0, x0, FALSE, FALSE, DROP_TYPE_UNTHEMED);
            }

            /* Stairs (20%) */
            else
            {
                place_random_stairs(y0, x0);
            }

            /* Traps to protect the treasure */
            vault_traps(y0, x0, 4, 10, 2 + randint(3));

            break;
        }

        /* An inner room with an inner pillar or pillars */
        case 3:
        {
            /* Open the inner room with a secret door */
            generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

            /* Inner pillar */
            generate_fill(y0-1, x0-1, y0+1, x0+1, FEAT_WALL_GRANITE_INNER);

            /* Occasionally, two more Large Inner Pillars */
            if (one_in_(2))
            {
                /* Three spaces */
                if (one_in_(2))
                {
                    /* Inner pillar */
                    generate_fill(y0-1, x0-7, y0+1, x0-5, FEAT_WALL_GRANITE_INNER);

                    /* Inner pillar */
                    generate_fill(y0-1, x0+5, y0+1, x0+7, FEAT_WALL_GRANITE_INNER);
                }

                /* Two spaces */
                else
                {
                    /* Inner pillar */
                    generate_fill(y0-1, x0-6, y0+1, x0-4, FEAT_WALL_GRANITE_INNER);

                    /* Inner pillar */
                    generate_fill(y0-1, x0+4, y0+1, x0+6, FEAT_WALL_GRANITE_INNER);
                }
            }

            /* Occasionally, some Inner rooms */
            if (one_in_(3))
            {
                /* Inner rectangle */
                generate_draw(y0-1, x0-5, y0+1, x0+5, FEAT_WALL_GRANITE_INNER);

                /* Secret doors (random top/bottom) */
                place_secret_door(y0 - 3 + (randint(2) * 2), x0 - 3);
                place_secret_door(y0 - 3 + (randint(2) * 2), x0 + 3);

                /* Monsters */
                vault_monsters(y0, x0 - 2, randint(2));
                vault_monsters(y0, x0 + 2, randint(2));

                /* Objects */
                if (one_in_(3)) place_object(y0, x0 - 2, FALSE, FALSE, DROP_TYPE_UNTHEMED);
                if (one_in_(3)) place_object(y0, x0 + 2, FALSE, FALSE, DROP_TYPE_UNTHEMED);
            }

            break;
        }

        /* An inner room with a checkerboard */
        case 4:
        {
            /* Open the inner room with a secret door */
            generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

            /* Checkerboard */
            for (y = y1; y <= y2; y++)
            {
                for (x = x1; x <= x2; x++)
                {
                    if ((x + y) & 0x01)
                    {
                        cave_set_feat(y, x, FEAT_WALL_GRANITE_INNER);
                    }
                }
            }

            /* Monsters just love mazes. */
            vault_monsters(y0, x0 - 5, randint(3));
            vault_monsters(y0, x0 + 5, randint(3));

            /* Traps make them entertaining. */
            vault_traps(y0, x0 - 3, 2, 8, randint(3));
            vault_traps(y0, x0 + 3, 2, 8, randint(3));

            /* Mazes should have some treasure too. */
            vault_objects(y0, x0, 3);

            break;
        }

        /* Four small rooms. */
        case 5:
        {
            /* Inner "cross" */
            generate_plus(y1, x1, y2, x2, FEAT_WALL_GRANITE_INNER);

            /* Doors into the rooms */
            if (one_in_(2))
            {
                int i = randint(10);
                place_secret_door(y1 - 1, x0 - i);
                place_secret_door(y1 - 1, x0 + i);
                place_secret_door(y2 + 1, x0 - i);
                place_secret_door(y2 + 1, x0 + i);
            }
            else
            {
                int i = randint(3);
                place_secret_door(y0 + i, x1 - 1);
                place_secret_door(y0 - i, x1 - 1);
                place_secret_door(y0 + i, x2 + 1);
                place_secret_door(y0 - i, x2 + 1);
            }

            /* Treasure, centered at the center of the cross */
            vault_objects(y0, x0, 2 + randint(2));

            /* Gotta have some monsters */
            vault_monsters(y0 + 1, x0 - 4, randint(4));
            vault_monsters(y0 + 1, x0 + 4, randint(4));
            vault_monsters(y0 - 1, x0 - 4, randint(4));
            vault_monsters(y0 - 1, x0 + 4, randint(4));

            break;
        }
    }
}


/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a monster nest or monster pit or
 * the given type.
 *
 * None of the pits/nests are allowed to include "unique" monsters,
 * or monsters which can "multiply".
 *
 * Some of the pits/nests are asked to avoid monsters which can blink
 * away or which are invisible.  This is probably a hack.
 *
 * The old method used monster "names", which was bad, but the new
 * method uses monster race characters, which is also bad.  XXX XXX XXX
 */


/*
 * Helper function for "monster nest (jelly)"
 */
static bool vault_aux_jelly(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "ijm,";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Require icky thing, jelly, mold, or mushroom */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster nest (kobolds, orcs, nagas and yeeks)"
 */
static bool vault_aux_kobold_yeek_orc_naga(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "oyn";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "k", "o", or "y" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster nest (humanoid)"
 */
static bool vault_aux_humanoids(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "ph";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "p" or "h" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster nest (young dragon)"
 */
static bool vault_aux_youngdragon(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "d";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "d" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster nest (animal)"
 */
static bool vault_aux_animal(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "Q";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /* Hack -- No "Q" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Require "animal" flag */
    if (!(r_ptr->flags3 & (RF3_ANIMAL))) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster nest (undead)"
 */
static bool vault_aux_undead(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Require Undead */
    if (!(r_ptr->flags3 & (RF3_UNDEAD))) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (orc)"
 */
static bool vault_aux_orc(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "o";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "o" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}

/*
 * Helper function for "monster pit (troll)"
 */
static bool vault_aux_troll(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "T";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "T" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}

/*
 * Helper function for "monster pit (orc, orge, troll, giant)"
 */
static bool vault_aux_orc_ogre_troll_giant(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "ToOP";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "T, o, O, or P" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (coins)"
 */
static bool vault_aux_coins(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "$";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "$" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (ogre)"
 */
static bool vault_aux_ogre(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "O";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "O" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (hounds)"
 */
static bool vault_aux_hounds(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "Z";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "Z" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (giant)"
 */
static bool vault_aux_giant(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "P";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "P" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Hack -- breath type for "vault_aux_dragon()"
 */
static u32b vault_aux_dragon_mask4;


/*
 * Helper function for "monster pit (dragon)"
 */
static bool vault_aux_dragon(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "Dd";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /* Hack -- Require "d" or "D" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require correct "breath attack" */
    if (r_ptr->flags4 != vault_aux_dragon_mask4) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (servants of the valar)"
 */
static bool vault_aux_valar_servant(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "A";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /* Hack -- Require "A" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (dragon)"
 */
static bool vault_aux_dragon_elem(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "Dd";
    u32b breath_mask;

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /* Hack -- Require "d" or "D" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

     /* Get the elemental flags of this race */
    breath_mask = (r_ptr->flags4 & RF4_ELEM_BREATH_MASK);

    /* Ignore non-elemental and chromatic dragons */
    if (!breath_mask || (breath_mask == RF4_ELEM_BREATH_MASK)) return (FALSE);

    /* Ignore dragons that breath something else (dracolichs, dracolisks, etc.) */
    if ((r_ptr->flags4 & RF4_BREATH_MASK) != breath_mask) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (ancient dragon)"
 */
static bool vault_aux_ancdragon(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "D";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /* Hack -- Require "D" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}

/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_all_demons(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "Uu";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /*check the demon flag*/
    if (!(r_ptr->flags3 & (RF3_DEMON))) return (FALSE);

    /* Hack -- Require "U" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (minor demon)"
 */
static bool vault_aux_minor_demon(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "u";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /*check the demon flag*/
    if (!(r_ptr->flags3 & (RF3_DEMON))) return (FALSE);

    /* Hack -- Require "U" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_major_demon(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = "U";

    /* Decline unique monsters if a nest or pit*/
    if ((!allow_uniques) &&
        (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /*no player ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (FALSE);

    /*check the demon flag*/
    if (!(r_ptr->flags3 & (RF3_DEMON))) return (FALSE);

    /* Hack -- Require "U" monsters */
    if (!filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*set the get_mon_num_hook functional pointer based on theme*/
void get_mon_hook(byte theme)
{
    if 		(theme == LEV_THEME_CREEPING_COIN)	get_mon_num_hook = vault_aux_coins;
    else if (theme == LEV_THEME_ORC)			get_mon_num_hook = vault_aux_orc;
    else if (theme == LEV_THEME_TROLL)			get_mon_num_hook = vault_aux_troll;
    else if (theme == LEV_THEME_OGRE)			get_mon_num_hook = vault_aux_ogre;
    else if (theme == LEV_THEME_HOUND)			get_mon_num_hook = vault_aux_hounds;
    else if (theme == LEV_THEME_GIANT)			get_mon_num_hook = vault_aux_giant;
    else if (theme == LEV_THEME_DRAGON_YOUNG)	get_mon_num_hook = vault_aux_youngdragon;
    else if (theme == LEV_THEME_DRAGON_ACID)
    {
        get_mon_num_hook = vault_aux_dragon;
        vault_aux_dragon_mask4 = RF4_BRTH_ACID;
    }
    else if (theme == LEV_THEME_DRAGON_FIRE)
    {
        get_mon_num_hook = vault_aux_dragon;
        vault_aux_dragon_mask4 = RF4_BRTH_FIRE;
    }
    else if (theme == LEV_THEME_DRAGON_ELEMENTAL)
    {
        get_mon_num_hook = vault_aux_dragon_elem;
    }
    else if (theme == LEV_THEME_DRAGON_ELEC)
    {
        get_mon_num_hook = vault_aux_dragon;
        vault_aux_dragon_mask4 = RF4_BRTH_ELEC;
    }
    else if (theme == LEV_THEME_DRAGON_COLD)
    {
        get_mon_num_hook = vault_aux_dragon;
        vault_aux_dragon_mask4 = RF4_BRTH_COLD;
    }
    else if (theme == LEV_THEME_DRAGON_POIS)
    {
        get_mon_num_hook = vault_aux_dragon;
        vault_aux_dragon_mask4 = RF4_BRTH_POIS;
    }
    else if (theme == LEV_THEME_DRAGON_CHROMATIC)
    {
        get_mon_num_hook = vault_aux_dragon;
        vault_aux_dragon_mask4 = RF4_ELEM_BREATH_MASK;
    }
    else if (theme == LEV_THEME_DRAGON_MISC)
    {
        get_mon_num_hook = vault_aux_dragon;
    }
    else if (theme == LEV_THEME_DRAGON_ELEMENTAL) get_mon_num_hook = vault_aux_dragon_elem;
    else if (theme == LEV_THEME_DRAGON_ANCIENT)	get_mon_num_hook = vault_aux_ancdragon;
    else if (theme == LEV_THEME_JELLY)			get_mon_num_hook = vault_aux_jelly;
    else if (theme == LEV_THEME_ORC_NAGA_YEEK_KOBOLD)	get_mon_num_hook = vault_aux_kobold_yeek_orc_naga;
    else if (theme == LEV_THEME_ANIMAL)			get_mon_num_hook = vault_aux_animal;
    else if (theme == LEV_THEME_HUMANOID)		get_mon_num_hook = vault_aux_humanoids;
    else if (theme == LEV_THEME_DEMON_MINOR)	get_mon_num_hook = vault_aux_minor_demon;
    else if (theme == LEV_THEME_DEMON_ALL)		get_mon_num_hook = vault_aux_all_demons;
    else if (theme == LEV_THEME_DEMON_MAJOR)	get_mon_num_hook = vault_aux_major_demon;
    else if (theme == LEV_THEME_CAVE_DWELLER)	get_mon_num_hook = vault_aux_orc_ogre_troll_giant;
    else if (theme == LEV_THEME_UNDEAD)			get_mon_num_hook = vault_aux_undead;
    else if (theme == LEV_THEME_VALAR_SERVANTS)	get_mon_num_hook = vault_aux_valar_servant;
}

/*return a theme for a monster nest*/
byte get_nest_theme(int nestlevel, bool quest_theme)
{
    int mindepth, whatnest;

    /*enforce minimum depth, to keep weak nests out of deep levels*/
    mindepth = nestlevel / 4;

    /*keep total to 100 or less*/
    if ((mindepth + nestlevel) > 100) nestlevel = 100 - mindepth;

    /* Hack -- Choose a nest type */
    whatnest = randint(nestlevel);

    /* Try for harder themes for quests */
    if (quest_theme)
    {
        int whatnest2 = randint(nestlevel);
        int whatnest3 = randint(nestlevel);
        if (whatnest2 > whatnest) whatnest = whatnest2;
        if (whatnest3 > whatnest) whatnest = whatnest3;

    }

    whatnest += mindepth;

    if ((whatnest <= 25)  && (nestlevel <= 35))
    {
        /*coins, jelly, or kobolds/yeeks/orcs*/
        if (one_in_(3))			return LEV_THEME_CREEPING_COIN;
        else if (one_in_(2))	return LEV_THEME_JELLY;
        else					return LEV_THEME_ORC_NAGA_YEEK_KOBOLD;
    }

    /*cave dwellers or young dragons*/
    else if (whatnest <= 50)
    {
        if (one_in_(2))			return LEV_THEME_CAVE_DWELLER;
        else 					return LEV_THEME_DRAGON_YOUNG;
    }

    /*Animals or humanoids*/
    else if (whatnest <=75)
    {
        if (one_in_(2))			return LEV_THEME_ANIMAL;
        else					return LEV_THEME_HUMANOID;
    }

    /*Monster nest (undead) */
    else if (whatnest <=95)
    {
        if (one_in_(2))			return LEV_THEME_VALAR_SERVANTS;
        else 					return LEV_THEME_UNDEAD;
    }

    /*Ancient Dragon Nest*/
    else						return LEV_THEME_DRAGON_ANCIENT;
}


/*
 * return a theme for a monster pit
 */
byte get_pit_theme(int pitlevel, bool quest_theme)
{
    int mindepth, whatpit;

    /*enforce minimum depth, to keep weak nests out of deep levels*/
    mindepth = pitlevel / 4;

    /*keep total to 100 or less, so results aren't skewed at deep depths*/
    if ((mindepth + pitlevel) > 100) pitlevel = 100 - mindepth;

    /* Hack -- Choose a nest type */
    whatpit = randint(pitlevel);

    /* Try for harder themes for quests */
    if (quest_theme)
    {
        int whatpit2 = randint(pitlevel);
        int whatpit3 = randint(pitlevel);
        if (whatpit2 > whatpit) whatpit = whatpit2;
        if (whatpit3 > whatpit) whatpit = whatpit3;
    }

    whatpit += mindepth;

    /* Orc pit */
    if ((whatpit <= 20) && (pitlevel <= 35))
    {
        if (one_in_(2))	return LEV_THEME_CREEPING_COIN;
        else 			return LEV_THEME_ORC;
    }

    /*troll or ogre*/
    else if ((whatpit <= 35)  && (pitlevel <= 45))
    {
        if (one_in_(3))			return LEV_THEME_CAVE_DWELLER;
        else if (one_in_(2))	return LEV_THEME_TROLL;
        else			return LEV_THEME_OGRE;
    }
    else if ((whatpit <= 50) && (p_ptr->depth <= 60))
    {
        /* Hound, youngdragon, or hydra pit */
        if (one_in_(2))			return LEV_THEME_HOUND;
        else					return LEV_THEME_DRAGON_YOUNG;
    }

    /* Giant pit */
    else if ((whatpit <= 60) && (p_ptr->depth <= 80))
    {
        if (one_in_(2))		return LEV_THEME_VALAR_SERVANTS;
        else 				return LEV_THEME_GIANT;
    }

    /* Dragon pit */
    else if (whatpit <= 80)
    {
        /* Pick dragon type */
        switch (rand_int(6))
        {
            /* Black */
            case 0: return LEV_THEME_DRAGON_ACID;
            /* Blue */
            case 1: return LEV_THEME_DRAGON_ELEC;
            /* Red */
            case 2: return LEV_THEME_DRAGON_FIRE;
            /* White */
            case 3: return LEV_THEME_DRAGON_COLD;
            /* Green */
            case 4: return LEV_THEME_DRAGON_POIS;
            /* Chromatic */
            default:return LEV_THEME_DRAGON_CHROMATIC;
        }
    }

    /* Either ancient Dragon pit, Major Demon pit, or Servant of the Valar Pit */
    else
    {
        /* Pick dragon type */
        switch (rand_int(3))
        {
            case 1: return LEV_THEME_DRAGON_ACID;
            case 2: return LEV_THEME_DRAGON_ANCIENT;
            default:return LEV_THEME_VALAR_SERVANTS;
        }
    }
}


/*
 * Type 5 -- Monster nests
 *
 * A monster nest is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type strewn about the room.
 *
 * The monsters are chosen from a set of 64 randomly selected monster
 * races, to allow the nest creation to fail instead of having "holes".
 *
 * Note the use of the "get_mon_num_prep()" function, and the special
 * "get_mon_num_hook()" restriction function, to prepare the "monster
 * allocation table" in such a way as to optimize the selection of
 * "appropriate" non-unique monsters for the nest.
 *
 * Note that the "get_mon_num()" function may (rarely) fail, in which
 * case the nest will be empty, and will not affect the level rating.
 *
 * Note that "monster nests" will never contain "unique" monsters.
 */
void build_type_nest(int y0, int x0)
{
    int y, x, y1, x1, y2, x2;

    int i, j, harder_nest_check;

    s16b what[64];

    byte room_theme;

    bool empty = FALSE;

    int light = FALSE;

    byte is_quest_level = FALSE;

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /*no uniques in nests/pits*/
    allow_uniques = FALSE;

    /* Large room */
    y1 = y0 - 4;
    y2 = y0 + 4;
    x1 = x0 - 11;
    x2 = x0 + 11;

    /*check if we need a quest*/
    if (quest_check(p_ptr->depth) == QUEST_NEST) is_quest_level = TRUE;

    /* Generate new room */
    generate_room(y1-1, x1-1, y2+1, x2+1, light);

    /* Generate outer walls */
    generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_GRANITE_OUTER);

    /* Generate inner floors */
    generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

    /* Advance to the center room */
    y1 = y1 + 2;
    y2 = y2 - 2;
    x1 = x1 + 2;
    x2 = x2 - 2;

    /* Generate inner walls */
    generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_GRANITE_INNER);

    /* Open the inner room with one or two secret doors */
    generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());
    generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

    /*select the theme, or get the quest level theme*/
    if (is_quest_level) room_theme = q_ptr->q_theme;
    else room_theme = get_nest_theme(p_ptr->depth, FALSE);

    /*get the mon_hook*/
    get_mon_hook(room_theme);

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Pick some monster types */
    for (i = 0; i < 64; i++)
    {
        /* Get a (hard) monster type */
        what[i] = get_mon_num(p_ptr->depth +
                              (is_quest_level ? PIT_NEST_QUEST_BOOST : NEST_LEVEL_BOOST), y1, x1, MPLACE_NO_GHOST);

        /* Notice failure */
        if (!what[i]) empty = TRUE;
    }

    /* Remove restriction */
    get_mon_num_hook = NULL;

    /* Prepare allocation table */
    get_mon_num_prep();

    /*allow uniques again*/
    allow_uniques = TRUE;

    /* Oops */
    if (empty) return;

    /* Describe */
    if (cheat_room)
    {
        /* Room type */
        message(QString("Monster nest (%1)") .arg(feeling_themed_level[room_theme]));
    }

    /* Increase the level rating */
    rating += 10;

    /* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
    if ((p_ptr->depth <= 40) &&
        (randint(p_ptr->depth * p_ptr->depth + 1) < 300))
    {
        good_item_flag = TRUE;
    }

    /* Sort the entries XXX XXX XXX */
    for (i = 0; i < 64 - 1; i++)
    {
        /* Sort the entries */
        for (j = 0; j < 64 - 1; j++)
        {
            int i1 = j;
            int i2 = j + 1;

            int p1 = r_info[what[i1]].level;
            int p2 = r_info[what[i2]].level;

            /* Bubble */
            if (p1 > p2)
            {
                int tmp = what[i1];
                what[i1] = what[i2];
                what[i2] = tmp;
            }
        }
    }

    /* occasionally make nest of hardest monsters,
     * if a random number of 100 is
     * less than the level of the hardest
     * monster minus the current level
     */
    harder_nest_check = r_info[what[63]].level - p_ptr->depth;

    /*Hack - make some pits harder if deeper*/
    if (randint(100) < harder_nest_check)
    {
        /* Use the top 8 entries */
        for (i = 0; i < 32; i++)
        {

            what[i] = what[i + 32];
        }
    }

    /* Place some monsters */
    for (y = y0 - 2; y <= y0 + 2; y++)
    {
        for (x = x0 - 9; x <= x0 + 9; x++)
        {
            int r_idx = what[rand_int(64)];

            /* Place that "random" monster (no groups) */
            (void)place_monster_aux(y, x, r_idx, 0L);
        }
    }

    /* No teleporting inside pits/nests*/
    for (y = y0 - 2; y <= y0 + 2; y++)
    {
        for (x = x0 - 9; x <= x0 + 9; x++)
        {
            /* Part of a vault */
            dungeon_info[y][x].cave_info |= (CAVE_ICKY);
        }
    }

    /*final preps if this is a quest level*/
    if (is_quest_level)
    {
        int counter = 19 * 5;
        int bonus_items = 5;

        q_ptr->q_num_killed = 0;
        q_ptr->q_max_num = 0;

        /* Square-by-square grid search for monsters */
        for (y = y0 - 2; y <= y0 + 2; y++)
        {
            for (x = x0 - 9; x <= x0 + 9; x++)
            {
                /*Is there a monster here?*/
                if (dungeon_info[y][x].monster_idx > 0)
                {
                    monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

                    /*mark it as a quest monster*/
                    m_ptr->mflag |= (MFLAG_QUEST);

                    /*increase the max_num counter*/
                    q_ptr->q_max_num ++;

                    /* Randomly give 5 monsters a bonus item to drop. */
                    /* Paranoia*/
                    if ((bonus_items) && (counter))
                    {
                        if ((randint0(counter)) < bonus_items)
                        {
                            m_ptr->mflag |= (MFLAG_BONUS_ITEM);
                            bonus_items--;
                        }
                        counter--;
                    }
                }
                /* Mark quests for mimics where an object is created rather than a monster */
                else if (dungeon_info[y][x].object_idx > 0)
                {
                    object_type *o_ptr = &o_list[dungeon_info[y][x].object_idx];

                    if (o_ptr->is_mimic())
                    {
                        o_ptr->ident |= (IDENT_QUEST);

                        /*increase the max_num counter*/
                        q_ptr->q_max_num ++;
                    }
                }
            }
        }
    }
}


/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type organized in the room.
 *
 * The inside room in a monster pit appears as shown in tables.c.
 *
 * Note that the monsters in the pit are now chosen by using "get_mon_num()"
 * to request 16 "appropriate" monsters, sorting them by level, and using
 * half of the entries in this sorted list for the contents of the pit.
 *
 * Hack -- all of the "dragons" in a "dragon" pit must be the same "color",
 * which is handled by requiring a specific "breath" attack for all of the
 * dragons.  This may include "multi-hued" breath.  Note that "wyrms" may
 * be present in many of the dragon pits, if they have the proper breath.
 *
 * Note the use of the "get_mon_num_prep()" function, and the special
 * "get_mon_num_hook()" restriction function, to prepare the "monster
 * allocation table" in such a way as to optimize the selection of
 * "appropriate" non-unique monsters for the pit.
 *
 * Note that the "get_mon_num()" function may (rarely) fail, in which case
 * the pit will be empty, and will not effect the level rating.
 *
 * Note that "monster pits" will never contain "unique" monsters.
 */
void build_type_pit(int y0, int x0)
{
    int what[16], harder_pit_check;

    int i, j, y, x, y1, x1, y2, x2, row, col;

    bool empty = FALSE;

    int light = FALSE;

    byte pit_theme;

    byte is_quest_level = FALSE;

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Pick one of the pit patterns */
    byte which_pit = randint0(MAX_PIT_PATTERNS);

    /*no uniques in nests/pits*/
    allow_uniques = FALSE;

    /*check if we need a quest*/
    if (quest_check(p_ptr->depth) == QUEST_PIT) is_quest_level = TRUE;

    /* Large room */
    y1 = y0 - 4;
    y2 = y0 + 4;
    x1 = x0 - 11;
    x2 = x0 + 11;

    /* Generate new room */
    generate_room(y1-1, x1-1, y2+1, x2+1, light);

    /* Generate outer walls */
    generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_GRANITE_OUTER);

    /* Generate inner floors */
    generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

    /* Advance to the center room */
    y1 = y1 + 2;
    y2 = y2 - 2;
    x1 = x1 + 2;
    x2 = x2 - 2;

    /* Generate inner walls */
    generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_GRANITE_INNER);

    /* Open the inner room with one or two secret doors */
    generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());
    generate_hole(y1-1, x1-1, y2+1, x2+1, get_secret_door_num());

    /* Choose a pit type */
    if(is_quest_level) pit_theme = q_ptr->q_theme;
    else pit_theme = get_pit_theme(p_ptr->depth, FALSE);

    /*get the monster hook*/
    get_mon_hook(pit_theme);

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Pick some monster types */
    for (i = 0; i < 16; i++)
    {
        /* Get a (hard) monster type */
        what[i] = get_mon_num(p_ptr->depth +
                              (is_quest_level ? PIT_NEST_QUEST_BOOST : PIT_LEVEL_BOOST), y1, x1, MPLACE_NO_GHOST);

        /* Notice failure */
        if (!what[i]) empty = TRUE;
    }

    /* Remove restriction */
    get_mon_num_hook = NULL;

    /* Prepare allocation table */
    get_mon_num_prep();

    /*allow uniques again*/
    allow_uniques = TRUE;

    /* Oops */
    if (empty) return;

    /* Sort the entries XXX XXX XXX */
    for (i = 0; i < 16 - 1; i++)
    {
        /* Sort the entries */
        for (j = 0; j < 16 - 1; j++)
        {
            int i1 = j;
            int i2 = j + 1;

            int p1 = r_info[what[i1]].level;
            int p2 = r_info[what[i2]].level;

            /* Bubble */
            if (p1 > p2)
            {
                int tmp = what[i1];
                what[i1] = what[i2];
                what[i2] = tmp;
            }
        }
    }

    /* occasionally make nest of hardest monsters,
     * if a random number of 100 is
     * less than the level of the hardest
     * monster minus the current level
     */
    harder_pit_check = r_info[what[15]].level - p_ptr->depth;

    /*Hack - make some pits harder if deeper*/
    if (randint(100) < harder_pit_check)
    {
        /* Use the top 8 entries */
        for (i = 0; i < 8; i++)
        {

            what[i] = what[i + 8];
        }
    }

    /* Use every other entry */
    else
    {
        for (i = 0; i < 8; i++)
        {
            /* Every other entry */
            what[i] = what[i * 2];
        }
    }

    /* Message */
    if (cheat_room)
    {
        /* Room type */
        message(QString("Monster pit (%1)") .arg(feeling_themed_level[pit_theme]));
    }

    /* Increase the level rating */
    rating += 10;

    /* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
    if ((p_ptr->depth <= 40) &&
        (randint(p_ptr->depth * p_ptr->depth + 1) < 300))
    {
        good_item_flag = TRUE;
    }

    /* Place the monsters in one of the patterns in tables.c */
    for (row = 0, y = y0 - 2; y <= y0 + 2; y++, row++)
    {
        for (col = 0, x = x0 - 9; x <= x0 + 9; x++, col++)
        {
            int r_idx = what[pit_room_maps[which_pit][row][col]];

            /* Place that "random" monster (no groups) */
            (void)place_monster_aux(y, x, r_idx, 0L);
        }
    }

    /* No teleporting inside pits/nests*/
    for (y = y0 - 2; y <= y0 + 2; y++)
    {
        for (x = x0 - 9; x <= x0 + 9; x++)
        {
            /* Mark it */
            dungeon_info[y][x].cave_info |= (CAVE_ICKY);
        }
    }

    /* Final preps if this is a quest level */
    if (is_quest_level)
    {
        int counter = 19 * 5;
        int bonus_items = 5;

        q_ptr->q_num_killed = 0;
        q_ptr->q_max_num = 0;

        /* Square-by-square grid search for monsters */
        for (y = y0 - 2; y <= y0 + 2; y++)
        {
            for (x = x0 - 9; x <= x0 + 9; x++)
            {
                /* Is there a monster here? */
                if (dungeon_info[y][x].monster_idx > 0)
                {
                    monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

                    /*mark it as a quest monster*/
                    m_ptr->mflag |= (MFLAG_QUEST);

                    /*increase the max_num counter*/
                    q_ptr->q_max_num ++;

                    /* Randomly give 5 monsters a bonus item to drop. */
                    /* Paranoia*/
                    if ((bonus_items) && (counter))
                    {
                        if ((randint0(counter)) < bonus_items)
                        {
                            m_ptr->mflag |= (MFLAG_BONUS_ITEM);
                            bonus_items--;
                        }
                        counter--;
                    }
                }
                /* Mark quests for mimics where an object is created rather than a monster */
                else if (dungeon_info[y][x].object_idx > 0)
                {
                    object_type *o_ptr = &o_list[dungeon_info[y][x].object_idx];

                    if (o_ptr->is_mimic())
                    {
                        o_ptr->ident |= (IDENT_QUEST);

                        /*increase the max_num counter*/
                        q_ptr->q_max_num ++;
                    }
                }
            }
        }
    }
}


/*
 * Determine if a monster is suitable for the vault
 */
static bool monster_vault_okay(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    QString filter = ".";

    /* No lurkers or trappers */
    if (filter.contains(r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Hack -- fill in "vault" rooms
 */
void build_vault(int y0, int x0, const vault_type *v_ptr)
{
    int dx, dy, x, y;
    int ax, ay;
    bool flip_v = FALSE;
    bool flip_h = FALSE;
    int ymax = v_ptr->hgt;
    int xmax = v_ptr->wid;
    QString data = v_ptr->vault_text;

    byte quest_artifact_spots = 0;

    int t = 0;
    int quest_type = quest_check(p_ptr->depth);
    bool quest_greater_vault = FALSE;
    bool quest_special_vault = FALSE;
    if (quest_type == QUEST_VAULT)
    {
        artifact_type *a_ptr = &a_info[QUEST_ART_SLOT];
        if (v_ptr->typ == 9) quest_special_vault = TRUE;

        /*Hack - allow the quest artifact to be created */
        a_ptr->a_cur_num = 0;
        a_ptr->a_max_num = 0;
    }
    else if (quest_type == QUEST_GREATER_VAULT)
    {
        if (v_ptr->typ == 8) quest_greater_vault = TRUE;
    }

    /* Flip the vault (sometimes) */
    if (one_in_(2)) flip_v = TRUE;
    if (one_in_(2)) flip_h = TRUE;

    // Avoid annoying compiler warning
    x = y = 0;

    /* Place dungeon features and objects */
    for (dy = 0; dy < ymax; dy++)
    {
        if (flip_v) ay = ymax - 1 - dy;
        else ay = dy;

        for (dx = 0; dx < xmax; dx++, t++)
        {
            QChar symbol = data[t];
            if (flip_h) ax = xmax - 1 - dx;
            else ax = dx;

            /* Extract the location */
            x = x0 - (xmax / 2) + ax;
            y = y0 - (ymax / 2) + ay;

            /* Hack -- skip "non-grids" */
            if (symbol == ' ') continue;

            /* Lay down a floor */
            cave_set_feat(y, x, FEAT_FLOOR);

            /* Part of a vault */
            dungeon_info[y][x].cave_info |= (CAVE_ROOM | CAVE_ICKY);

            /* Analyze the grid */
            /* Granite wall (outer) */
            if (symbol == '%')
            {
                /* For vault quests, make it a normal empty square */
                if (quest_greater_vault) dungeon_info[y][x].cave_info &= ~(CAVE_ROOM | CAVE_ICKY);

                /* Otherwise a wall */
                else cave_set_feat(y, x, FEAT_WALL_GRANITE_OUTER);
            }

            /* Granite wall (inner) */
            else if (symbol == '#')
            {
                cave_set_feat(y, x, FEAT_WALL_GRANITE_INNER);
            }

            /* Permanent wall (inner) */
            else if (symbol == 'X')
            {
                cave_set_feat(y, x, FEAT_WALL_PERM_INNER);
            }

            /* Treasure/trap */
            else if (symbol == '*')
            {
                if (rand_int(100) < 75)
                {
                    place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
                }
                else
                {
                    place_trap(y, x, 0);
                }
            }

            /* Secret doors */
            else if (symbol == '+')
            {
                place_locked_door(y, x);
            }

            /* Trap */
            else if (symbol == '^')
            {
                place_trap(y, x, 0);
            }
        }
    }

    /*Count the 'Q's for a quest vault*/
    for (t = 0; t < ymax * xmax; t++)
    {
        QChar symbol = data[t];

        /* Hack -- count the quest spots */
        if (symbol == 'Q') quest_artifact_spots++;
    }

    /*get the hook*/
    get_mon_num_hook = monster_vault_okay;

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Place dungeon monsters and objects */
    for (t = 0, dy = 0; dy < ymax; dy++)
    {
        if (flip_v) ay = ymax - 1 - dy;
        else ay = dy;

        for (dx = 0; dx < xmax; dx++, t++)
        {
            QChar symbol = data[t];

            if (flip_h) ax = xmax - 1 - dx;
            else ax = dx;

            /* Extract the grid */
            x = x0 - (xmax/2) + ax;
            y = y0 - (ymax/2) + ay;

            /* Hack -- skip "non-grids" */
            if (symbol == ' ') continue;

            /* Monster */
            else if (symbol == '&')
            {
                monster_level = p_ptr->depth + 4;
                place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
                monster_level = p_ptr->depth;
            }

            /* Meaner monster */
            else if (symbol == '@')
            {
                monster_level = p_ptr->depth + 8;
                place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
                monster_level = p_ptr->depth;
            }

            /* Meaner monster, plus treasure */
            else if (symbol == '9')
            {
                monster_level = p_ptr->depth + 7;
                place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
                monster_level = p_ptr->depth;
                object_level = p_ptr->depth + 7;
                place_object(y, x, TRUE, FALSE, DROP_TYPE_UNTHEMED);
                object_level = p_ptr->depth;
            }

            /* Nasty monster and treasure */
            else if (symbol == '8')
            {
                monster_level = p_ptr->depth + 20;
                place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
                monster_level = p_ptr->depth;
                object_level = p_ptr->depth + 15;
                place_object(y, x, TRUE, TRUE, DROP_TYPE_UNTHEMED);
                object_level = p_ptr->depth;
            }

            /* Nasty monster and a chest */
            else if (symbol == '~')
            {
                monster_level = p_ptr->depth + 20;
                place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
                monster_level = p_ptr->depth;
                object_level = p_ptr->depth + 15;
                place_object(y, x, FALSE, FALSE, DROP_TYPE_CHEST);
                object_level = p_ptr->depth;
            }

            /* Quest chest */
            else if (symbol == 'Q')
            {
                bool placed_quest_artifact = FALSE;
                monster_level = p_ptr->depth + 10;
                place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
                monster_level = p_ptr->depth;

                /*randomly pick from several quest artifacts spots to place the artifact*/
                if (!a_info[QUEST_ART_SLOT].a_cur_num)
                {
                    if (one_in_(quest_artifact_spots))
                    {
                        placed_quest_artifact = (place_quest_artifact(y, x));
                    }
                }
                if (!placed_quest_artifact)
                {
                    /*place a decent sized object*/
                    object_level = p_ptr->depth + 7;
                    place_object(y, x, TRUE, FALSE, DROP_TYPE_UNTHEMED);
                    object_level = p_ptr->depth;

                    /*This quest artifact spot is no longer an option*/
                    quest_artifact_spots --;
                }
            }

            /* Monster and/or object */
            else if (symbol == ',')
            {
                if (one_in_(2))
                {
                    monster_level = p_ptr->depth + 3;
                    place_monster(y, x, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
                    monster_level = p_ptr->depth;
                }
                if (one_in_(2))
                {
                    object_level = p_ptr->depth + 5;
                    place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
                    object_level = p_ptr->depth;
                }
            }
        }

        /*mark it as a quest monster*/
        if ((dungeon_info[y][x].monster_idx > 0) && (quest_greater_vault || quest_special_vault))
        {
            monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

            m_ptr->mflag |= (MFLAG_QUEST);
        }

        /*
         * Make the monsters carry the objects instead of just stand on them.
         */
        if ((dungeon_info[y][x].monster_idx > 0) && (dungeon_info[y][x].object_idx > 0))
        {

            /* Get the object */
            object_type *o_ptr = &o_list[dungeon_info[y][x].object_idx];

            /*but don't do gold*/
            if (o_ptr->tval != TV_GOLD)
            {
                /*Don't let the player see what the object it*/
                o_ptr->ident |= (IDENT_HIDE_CARRY);

                (void)monster_carry(dungeon_info[y][x].monster_idx, o_ptr);

                /*remove the item from the floor*/
                floor_item_increase(dungeon_info[y][x].object_idx, -1);
                floor_item_optimize(dungeon_info[y][x].object_idx);
            }
        }
    }

    /* Reset the allocation table */
    get_mon_num_hook = NULL;
    get_mon_num_prep();
}

/*
 * Type 7 -- simple vaults (see "vault.txt")
 */
void build_type_lesser_vault(int y0, int x0)
{
    vault_type *v_ptr;

    /* Pick a lesser vault */
    while (TRUE)
    {
        /* Get a random vault record */
        v_ptr = &v_info[rand_int(z_info->v_max)];

        /* Accept the first lesser vault */
        if (v_ptr->typ == 7) break;
    }

    /* Message */
    if (cheat_room)
    {
        message(QString("Lesser vault (%1)") .arg(v_ptr->vault_name));
    }

    /* Boost the rating */
    rating += v_ptr->rat;

    /* (Sometimes) Cause a special feeling */
    if ((p_ptr->depth <= 50) ||
        (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 1) < 400))
    {
        good_item_flag = TRUE;
    }

    /* Hack -- Build the vault */
    build_vault(y0, x0, v_ptr);
}


/*
 * Mark greater vault grids with the CAVE_G_VAULT flag.
 */
void mark_g_vault(int y0, int x0, int hgt, int wid)
{
    int y1, x1, y2, x2, y, x;

    /* Queue of reachable grids */
    QVector<coord> grid_queue;
    grid_queue.clear();

    /* Get the corners */
    y1 = y0 - hgt / 2;
    x1 = x0 - wid / 2;
    y2 = y1 + hgt - 1;
    x2 = x1 + wid - 1;

    /* Step 1 - Mark all vault grids with the new flag */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            dungeon_info[y][x].cave_info |= (CAVE_G_VAULT);
        }
    }

    /*
     * Step 2 - Most vaults have an inner room, which usually contains
     * the good stuff. Unmark all walls and floors outside this inner room
     */

    /* We need a perimeter of outer walls surrounding the vault */
    if (dungeon_info[y1][x1].feature_idx != FEAT_WALL_GRANITE_OUTER) return;


    /* Append the top-left corner of the vault to the queue */
    grid_queue.append(make_coords(y1, x1));

    /* Unmark that grid */
    dungeon_info[y1][x1].cave_info &= ~(CAVE_G_VAULT);

    /* Process all the reachable vault grids */
    while (grid_queue.size())
    {
        int d;

        /* Get the coordinates of the grid in the head of the queue */
        y = grid_queue.at(0).y;
        x = grid_queue.at(0).x;

        /* Remove that grid from the queue */
        grid_queue.removeFirst();

        /* Scan adjacent grids */
        for (d = 0; d < 8; d++)
        {
            /* Get coordinates */
            int yy = y + ddy_ddd[d];
            int xx = x + ddx_ddd[d];

            /* Ignore annoying locations */
            if (!in_bounds_fully(yy, xx)) continue;

            /*
             * Ignore already processed grids, or grids outside
             * the vault
             */
            if (!(dungeon_info[yy][xx].cave_info & (CAVE_G_VAULT))) continue;

            /* Unmark that grid, its type doesn't matter */
            dungeon_info[yy][xx].cave_info &= ~(CAVE_G_VAULT);

            /* But keep processing only certain grid types */
            if (cave_ff1_match(yy, xx, FF1_FLOOR) ||
                (dungeon_info[yy][xx].feature_idx == FEAT_WALL_GRANITE_OUTER))
            {
                /* Append that grid to the queue */
                grid_queue.append(make_coords(yy, xx));
            }
        }
    }
}


/*
 * Type 8 -- greater vaults (see "vault.txt")
 */
void build_type_greater_vault(int y0, int x0)
{
    vault_type *v_ptr;

    /* Pick a greater vault */
    while (TRUE)
    {
        /* Get a random vault record */
        v_ptr = &v_info[rand_int(z_info->v_max)];

        /* Accept the first greater vault */
        if (v_ptr->typ == 8) break;
    }

    /* Message */
    if (cheat_room)
    {
        message(QString("Greater vault (%1)") .arg(v_ptr->vault_name));
    }

    /* Boost the rating */
    rating += v_ptr->rat;

    /* (Sometimes) Cause a special feeling */
    if ((p_ptr->depth <= 50) ||
        (randint((p_ptr->depth-40) * (p_ptr->depth-40) + 1) < 400))
    {
        good_item_flag = TRUE;
    }

    /* Build the vault */
    build_vault(y0, x0, v_ptr);

    /* Mark vault grids with the CAVE_G_VAULT flag */
    mark_g_vault(y0, x0, v_ptr->hgt, v_ptr->wid);

    /* Remember the vault's name */
    g_vault_name = v_ptr->vault_name;
}


/*
 * Type 9 -- quest vaults (see "vault.txt")
 */
void build_type_special_vault(int y0, int x0)
{
    vault_type *v_ptr;

    /* Pick a quest vault */
    while (TRUE)
    {
        /* Get a random vault record */
        v_ptr = &v_info[rand_int(z_info->v_max)];

        /* Accept the first quest vault */
        if (v_ptr->typ == 9) break;
    }

    /* Message */
    if (cheat_room)
    {
        message(QString("Special vault (%1)") .arg(v_ptr->vault_name));
    }

    /* Boost the rating */
    rating += v_ptr->rat;

    /* Hack -- Build the vault */
    build_vault(y0, x0, v_ptr);
}

/*
 * Attempt to build a room of the given type at the given block
 *
 * Note that we restrict the number of "crowded" rooms to reduce
 * the chance of overflowing the monster list during level creation.
 */
bool room_build(int by0, int bx0, int typ)
{
    int y, x;
    int by, bx;
    int by1, bx1, by2, bx2;

    /* Restrict level */
    if (p_ptr->depth < room[typ].level) return (FALSE);

    /* Restrict "crowded" rooms */
    if (dun->crowded && ((typ == 5) || (typ == 6))) return (FALSE);

    /* Extract blocks */
    by1 = by0 + room[typ].dy1;
    bx1 = bx0 + room[typ].dx1;
    by2 = by0 + room[typ].dy2;
    bx2 = bx0 + room[typ].dx2;

    /* Never run off the screen */
    if ((by1 < 0) || (by2 >= dun->row_rooms)) return (FALSE);
    if ((bx1 < 0) || (bx2 >= dun->col_rooms)) return (FALSE);

    /* Verify open space */
    for (by = by1; by <= by2; by++)
    {
        for (bx = bx1; bx <= bx2; bx++)
        {
            if (dun->room_map[by][bx]) return (FALSE);
        }
    }

    /* It is *extremely* important that the following calculation */
    /* be *exactly* correct to prevent memory errors XXX XXX XXX */

    /* Get the location of the room */
    y = ((by1 + by2 + 1) * BLOCK_HGT) / 2;
    x = ((bx1 + bx2 + 1) * BLOCK_WID) / 2;

    /* Build a room */
    switch (typ)
    {
        /* Build an appropriate room */
        case 14:
        case 13:
        case 12:
        {
            byte fractal_type;

            switch (typ)
            {
                case 14: fractal_type = FRACTAL_TYPE_33x65; break;
                case 13: fractal_type = FRACTAL_TYPE_33x33; break;
                default: fractal_type = FRACTAL_TYPE_17x33; break;
            }

            if (!build_type_fractal(y, x, fractal_type)) return (FALSE);

            break;
        }
        case 11:build_type_starburst(y, x, TRUE); break;
        case 10:build_type_starburst(y, x, FALSE); break;
        case 9: build_type_special_vault(y, x); break;
        case 8: build_type_greater_vault(y, x); break;
        case 7: build_type_lesser_vault(y, x); break;
        case 6: build_type_pit(y, x); break;
        case 5: build_type_nest(y, x); break;
        case 4: build_type4(y, x); break;
        case 3: build_type3(y, x); break;
        case 2: build_type2(y, x); break;
        case 1: build_type1(y, x); break;

        /* Paranoia */
        default: return (FALSE);
    }

    /* Save the room location */
    if (dun->cent_n < CENT_MAX)
    {
        dun->cent[dun->cent_n].y = y;
        dun->cent[dun->cent_n].x = x;
        dun->cent_n++;
    }

    /* Reserve some blocks */
    for (by = by1; by <= by2; by++)
    {
        for (bx = bx1; bx <= bx2; bx++)
        {
            dun->room_map[by][bx] = TRUE;
        }
    }

    /* Count "crowded" rooms */
    if ((typ == 5) || (typ == 6)) dun->crowded = TRUE;

    /* Success */
    return (TRUE);
}
