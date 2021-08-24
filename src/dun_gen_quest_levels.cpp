

/* File: dun_gen_quest_levels.cpp */

/*
 * Copyright (c) 2012 Jeff Greene, Diego Gonzalez
 * Please see copyright.txt for complete copyright and licensing restrictions.
 */



// Code for generation of wilderness levels, themed levels, labyrnth levels, and arena levels.


#include "src/npp.h"
#include "src/dun_generate.h"


/*
 *
 * Build a wilderness level
 *
 */


/*
 * Place the irregular dungeon border of wilderness levels
 */
static void build_border(int y, int x, u16b feat)
{
    /* Ignore icky grids */
    if (dungeon_info[y][x].cave_info & (CAVE_ICKY)) return;

    /* Ignore grids with effects */
    if (dungeon_info[y][x].effect_idx) return;

    /* Ignore grids with objects */
    if (dungeon_info[y][x].object_idx) return;

    /* Ignore grids with monsters */
    if (dungeon_info[y][x].monster_idx) return;

    /* Place the border */
    cave_set_feat(y, x, feat);
}




/*
 * Lite all elemental features in the level (walls included), and their adjacent grids
 * If show_objects is TRUE we mark the objects placed on such grids
 */
static void light_elements(bool show_objects)
{
    int y, x;

    /* Look for interesting grids all over the dungeon */
    for (y = 1; y < p_ptr->cur_map_hgt - 1; y++)
    {
        for (x = 1; x < p_ptr->cur_map_wid - 1; x++)
        {
            int i;

            /* Must be an elemental feature */
            if (!cave_ff3_match(y, x, TERRAIN_MASK)) continue;

            /* Lite that grid and grids adjacent to it */
            /* We don't need to call in_bounds */
            for (i = 0; i < 9; i++)
            {
                int yy = y + ddy_ddd[i];
                int xx = x + ddx_ddd[i];

                /* Lite the grid */
                dungeon_info[yy][xx].cave_info |= (CAVE_GLOW | CAVE_MARK | CAVE_EXPLORED);

                /* Remember its objects if necessary */
                if (show_objects)
                {
                    /* Get the index of the first object */
                    s16b o_idx = dungeon_info[yy][xx].object_idx;

                    /* Mark all the objects of the pile */
                    while (o_idx)
                    {
                        /* Get the object */
                        object_type *o_ptr = &o_list[o_idx];

                        /* Mark the object */
                        o_ptr->mark_object();

                        /* Go to the next object */
                        o_idx = o_ptr->next_o_idx;
                    }
                }
            }
        }
    }
}

/*
 * Helper. Place an horizontal chain of ice mountains
 * row is the center of the chain
 */
static void build_ice_mountains(int row)
{
    int y, x;
    int y1, y2;
    /* Maximum offset */
    int max_offset = 10;
    /* Minimum width of the mountains */
    int pad = 2;

    /* Initial vertical offset */
    y1 = y2 = 4;

    /* Mountains are placed horizontally */
    for (x = 0; x < p_ptr->cur_map_wid; x++)
    {
        /* Randomize the top offset */
        y1 += 1 - rand_int(3);

        /* Check bounds */
        if (y1 < 0) y1 = 0;
        else if (y1 > max_offset) y1 = max_offset;

        /* Randomize the bottom offset */
        y2 += 1 - rand_int(3);

        /* Check bounds */
        if (y2 < 0) y2 = 0;
        else if (y2 > max_offset) y2 = max_offset;

        /* Place ice walls between top and bottom  */
        for (y = row - y1 - pad; y <= row + y2 + pad; y++)
        {
            /* Check sanity */
            if (in_bounds(y, x))
            {
                int k = rand_int(100);
                u16b feat;

                /* Flavor */
                if (k < 90) feat = FEAT_WALL_ICE;
                else feat = FEAT_WALL_ICE_CRACKED;

                /* Place the wall */
                cave_set_feat(y, x, feat);
            }
        }
    }
}


/*
 * Create the irregular borders of a wilderness levels.
 * Based on code created by Nick McConnel (FAangband)
 */
static void build_wilderness_borders(u16b feat)
{
    int hgt = p_ptr->cur_map_hgt;
    int wid = p_ptr->cur_map_wid;
    int y, x;
    int i;

    int start_offset = 4;
    int max_offset = 7;

    /* Top border */
    i = start_offset;

    for (x = 0; x < wid; x++)
    {
        /* Modify the offset by -1, +1 or 0 */
        i += (1 - rand_int(3));

        /* Check bounds */
        if (i > max_offset) i = max_offset;
        else if (i < 0) i = 0;

        /* Place border */
        for (y = 0; y < i; y++)
        {
            build_border(y, x, feat);
        }
    }

    /* Bottom border */
    i = start_offset;

    for (x = 0; x < wid; x++)
    {
        /* Modify the offset by -1, +1 or 0 */
        i += (1 - rand_int(3));

        /* Check bounds */
        if (i > max_offset) i = max_offset;
        else if (i < 0) i = 0;

        /* Place border */
        for (y = 0; y < i; y++)
        {
            build_border(hgt - 1 - y, x, feat);
        }
    }

    /* Left border */
    i = start_offset;

    for (y = 0; y < hgt; y++)
    {
        /* Modify the offset by -1, +1 or 0 */
        i += (1 - rand_int(3));

        /* Check bounds */
        if (i > max_offset) i = max_offset;
        else if (i < 0) i = 0;

        /* Place border */
        for (x = 0; x < i; x++)
        {
            build_border(y, x, feat);
        }
    }

    /* Right border */
    i = start_offset;

    for (y = 0; y < hgt; y++)
    {
        /* Modify the offset by -1, +1 or 0 */
        i += (1 - rand_int(3));

        /* Check bounds */
        if (i > max_offset) i = max_offset;
        else if (i < 0) i = 0;

        /* Place border */
        for (x = 0; x < i; x++)
        {
            build_border(y, wid - 1 - x, feat);
        }
    }
}


/*
 * Helper function for add_wilderness_quest_terrain.  Take a spot, add dangerous wall terrain, and surround
 * it with 3-4 dangerous floor terrains. *
 */
static void add_wilderness_quest_terrain_aux(int y, int x, u16b floor_terrain, u16b wall_terrain)
{
    int yy, xx;
    int y_places[8];
    int x_places[8];
    int num_places = 0;
    int num_floor = randint1(2);
    int i;

    /* Build a solid wall here */
    cave_set_feat(y, x, wall_terrain);

    /* Now find areas to add dangerous floor terrains */
    for (i = 0; i < 8; i++)
    {
        yy = y + ddy[i];
        xx = x + ddx[i];

        if (!in_bounds_fully(yy, xx)) continue;
        if (cave_ff1_match(yy, xx, FF1_PERMANENT))
        {
            /* Don't alter stairs or walls */
            if (cave_ff1_match(yy, xx, FF1_WALL | FF1_STAIRS))continue;
        }
        y_places[num_places] = yy;
        x_places[num_places] = xx;
        num_places++;
    }

    /* Boundry control */
    if (!num_places) return;

    /* Add up to num_floor of the dangerous floor terrains */
    for (i = 0; i < num_floor; i++)
    {
        int k = randint0(num_places);
        yy = y_places[k];
        xx = x_places[k];

        /* Paranoia */
        if (!in_bounds_fully(yy, xx)) continue;

        /* Place the floor terrain, and then eliminate this square from future choices */
        cave_set_feat(yy, xx, floor_terrain);
        num_places--;
        y_places[k] = y_places[num_places];
        x_places[k] = x_places[num_places];
    }
}


/*
 * Add several pockets of terrain that will eventually overrun the level
 */
static void add_wilderness_quest_terrain(u16b floor_terrain, u16b wall_terrain)
{
    int hgt = p_ptr->cur_map_hgt;
    int wid = p_ptr->cur_map_wid;
    int j;

    /*
     * Add some patches of wall and floor in each corner of the dungeon.
     */
    for (j = 0; j < 4; j++)
    {
        int i, x, y;
        int coord_count = 0;
        int y_start, y_end, x_start, x_end, slot;
        int y_places[900];
        int x_places[900];

        /*
         * Just to avoid repeating the code below,
         * we assign the x and y ranges here and then
         * loop through the code below 4 times
         */
        switch (j)
        {
            /* Top left */
            case 1: {y_start = 1; y_end = 31; x_start = 1; x_end = 31; break;}
            /* Bottom left */
            case 2: {y_start = (hgt - 31); y_end = hgt; x_start = 1; x_end = 31; break;}
            /* Top right */
            case 3: {y_start = 1; y_end = 31; x_start = (wid - 31); x_end = wid; break;}
            /* Bottom left */
            default: {y_start = (hgt - 31); y_end = hgt; x_start = (wid - 31); x_end = wid; break;}
        }

        for (y = y_start; y < y_end; y++)
        {
            for (x = x_start; x < x_end; x++)
            {
                /* Paranoia */
                if (!in_bounds_fully(y, x)) continue;

                /* Don't alter stairs or permanent walls */
                if (cave_ff1_match(y, x, FF1_PERMANENT))
                {
                    if (cave_ff1_match(y, x, FF1_WALL | FF1_STAIRS))continue;
                }

                /* store this square */
                y_places[coord_count] = y;
                x_places[coord_count] = x;
                coord_count++;
            }
        }

        for (i = 0; i < 8; i++)
        {
            /* Paranoia */
            if (!coord_count) break;

            /* Pick one of the set of coordinates */
            slot = randint0(coord_count);
            y = y_places[slot];
            x = x_places[slot];

            /* Paranoia */
            if (!in_bounds_fully(y, x)) continue;

            /* Place the floor terrain, and then eliminate this square from future choices */
            add_wilderness_quest_terrain_aux(y, x, floor_terrain, wall_terrain);
            coord_count--;
            y_places[slot] = y_places[coord_count];
            x_places[slot] = x_places[coord_count];
        }
    }
}




/*
 * Builds an ice level. Returns TRUE on success, FALSE on error
 */
static bool build_ice_level(void)
{
    int y, x;
    int i, j;
    int hgt, wid;
    int hgt2;

    /* Make it a smaller size */
    hgt = p_ptr->cur_map_hgt = MAX_DUNGEON_HGT;
    wid = p_ptr->cur_map_wid = MAX_DUNGEON_WID;

    /* Actual maximum number of rooms on this level */
    dun->row_rooms = p_ptr->cur_map_hgt / BLOCK_HGT;
    dun->col_rooms = p_ptr->cur_map_wid / BLOCK_WID;

    hgt2 = hgt / 2;

    /* Start with floors */
    for (y = 0; y < hgt; y++)
    {
        for (x = 0; x < wid; x++)
        {
            cave_set_feat(y, x, FEAT_FLOOR);

            dungeon_info[y][x].cave_info |= (CAVE_ROOM);
        }
    }

    /* Put lots of ice */
    for (i = 0; i < 15; i++)
    {
        build_formation(-1, -1, FEAT_FLOOR_ICE, FRACTAL_TYPE_33x65, 0, FEAT_NONE, 0);
    }

    /* Put some "mountains" */
    build_ice_mountains(hgt2);

    /* Add some granite formations */
    for (i = 0; i < 15; i++)
    {
        build_formation(-1, -1, FEAT_WALL_GRANITE_CRACKED, FRACTAL_TYPE_17x17, 0, FEAT_NONE, 0);
    }

    j = 20;

    /* Put some irregular ice walls to break los */
    for (i = 0; i < j; i++)
    {
        int tries;

        for (tries = 0; tries < 200; tries++)
        {
            /* Get random coordinates */
            y = randint(hgt - 1);
            x = randint(wid - 1);

            /* Location must be passable */
            if (cave_ff1_match(y, x, FF1_MOVE)) break;
        }

        build_formation(y, x, FEAT_WALL_ICE_CRACKED, FRACTAL_TYPE_17x17, 40, FEAT_NONE, 1);
    }

    j = 70 + rand_int(51);

    /* Put some pebbles */
    for (i = 0; i < j; i++)
    {
        int tries;

        for (tries = 0; tries < 200; tries++)
        {
            /* Get random coordinates */
            y = randint(hgt - 1);
            x = randint(wid - 1);

            /* Ignore ice */
            if (cave_ff3_match(y, x, ELEMENT_ICE)) continue;

            /* Location must be passable */
            if (cave_ff1_match(y, x, FF1_MOVE)) break;
        }

        cave_set_feat(y, x, FEAT_FLOOR_PEBBLES);
    }

    j = one_in_(4) ? (2 + rand_int(3)): 0;

    /* Add some water pools, sometimes */
    for (i = 0; i < j; i++)
    {
        int tries;

        for (tries = 0; tries < 200; tries++)
        {
            /* Get random coordinates */
            y = randint(hgt - 1);
            x = randint(wid - 1);

            /* Location must be passable */
            if (cave_ff1_match(y, x, FF1_MOVE)) break;
        }

        build_formation(y, x, FEAT_FLOOR_WATER, FRACTAL_TYPE_17x33, 10, FEAT_NONE, 0);
    }

    j = 4;

    /* Pierce the ice mountains making some tunnels */
    for (i = 0; i < j; i++)
    {
        int y2;
        int x2;
        int dist;

        /* Get start x coordinate */
        x = randint(wid - 1);

        do
        {
            /* Get final x coordinate */
            x2 = randint(wid - 1);

            /*
             * Calculate distance. The end must be somewhat
             * far away from the start
             */
            dist = ABS(x2 - x);
        } while ((dist < 20) || (dist > 40));

        /* Get start y coordinate */
        y = rand_range(1, hgt2 - 20);

        /* Get final y coordinate */
        y2 = rand_range(hgt2 + 20, hgt - 2);

        /* Sometimes we swap the y coordinates */
        if (one_in_(2))
        {
            int tmp = y;
            y = y2;
            y2 = tmp;
        }

        /* Make a tunnel */
        build_tunnel(y, x, y2, x2);
    }

    /* Place some fractal rooms more */
    for (i = 0; i < 5; i++)
    {
        /* Only part of the time */
        if (one_in_(2))
        {
            /* Pick a location */
            for (j = 0; j < 50; j++)
            {
                /* Get coordinates */
                y = rand_int(dun->row_rooms);
                x = rand_int(dun->col_rooms);

                /* Place the room */
                if (room_build(y, x, 13)) break;
            }
        }
    }

    return (TRUE);
}


/*
 * Build a full forest level
 * Returns TRUE on success
 */
static bool build_forest_level(void)
{
    int y, x;
    int i, j;
    int hgt, wid, wid2;

    /* Make it smaller size */
    hgt = p_ptr->cur_map_hgt = MAX_DUNGEON_HGT;
    wid = p_ptr->cur_map_wid = MAX_DUNGEON_WID;

     /* Actual maximum number of rooms on this level */
    dun->row_rooms = p_ptr->cur_map_hgt / BLOCK_HGT;
    dun->col_rooms = p_ptr->cur_map_wid / BLOCK_WID;

    /* Cache center point, height currently isn't needed. */
    wid2 = wid / 2;

    /* Initialize the dungoen with forest soil */
    for (y = 0; y < hgt; y++)
    {
        for (x = 0; x < wid; x++)
        {
            cave_set_feat(y, x, FEAT_FOREST_SOIL);

            dungeon_info[y][x].cave_info |= (CAVE_ROOM);
        }
    }

    /* Place some initial big earth formations */
    for (i = 0; i < 5; i++)
    {
        build_formation(-1, -1, FEAT_FLOOR_EARTH, FRACTAL_TYPE_33x65, 0, FEAT_NONE, 0);
    }

    /* Place one big earth wall formation most of the time */
    j = (one_in_(4) ? 0: 1);

    for (i = 0; i < j; i++)
    {
        build_formation(-1, -1, FEAT_WALL_EARTH, FRACTAL_TYPE_33x65, 0, FEAT_NONE, 0);
    }

    /* Place some irregular rooms of variable size */
    j = 3 + rand_int(20);

    for (i = 0; i < j; i++)
    {
        build_formation(-1, -1, FEAT_WALL_GRANITE, one_in_(3) ? FRACTAL_TYPE_9x9: FRACTAL_TYPE_17x17, 15,
            FEAT_WALL_GRANITE_CRACKED, 1);
    }

    /* Place a dense forest on the left side of the level */
    j = hgt * wid2;

    j = 10 * j / 100;

    for (i = 0; i < j; i++)
    {
        /* Pick random grid */
        y = randint(hgt - 1);
        x = randint(wid2 - 1);

        cave_set_feat(y, x, FEAT_TREE);
    }

    /* Place a clearer forest on the right side */
    j = hgt * wid2;

    j = 5 * j / 100;

    for (i = 0; i < j; i++)
    {
        /* Pick random grid */
        y = randint(hgt - 1);
        x = wid2 + randint(wid2 - 1);

        cave_set_feat(y, x, FEAT_TREE);
    }

    /* Scatter random features through the level (they pierce things) */
    j = hgt * wid;

    j = 5 * j / 100;

    for (i = 0; i < j; i++)
    {
        /* Pick random grid */
        y = randint(hgt - 1);
        x = randint(wid - 1);

        /* Place earth */
        cave_set_feat(y, x, FEAT_FLOOR_EARTH);

        /* Pick random grid */
        y = randint(hgt - 1);
        x = randint(wid - 1);

        /* Place forest soil or grass, sometimes dynamic */
        if (one_in_(4)) cave_set_feat(y, x, (i < (j / 5)) ? FEAT_GRASS_DYNAMIC: FEAT_GRASS);
        else cave_set_feat(y, x, (i < (j / 5)) ? FEAT_FOREST_SOIL_DYNAMIC: FEAT_FOREST_SOIL);
    }

    /* Place a few mud grids */
    j = one_in_(2) ? 30: 10;

    for (i = 0; i < j; i++)
    {
        /* Pick random grid */
        y = randint(hgt - 1);
        x = randint(wid - 1);

        cave_set_feat(y, x, FEAT_FLOOR_MUD);
    }

    /* Place a big lake most of the time */
    j = (one_in_(4) ? 0: 1);

    for (i = 0; i < j; i++)
    {
        build_formation(-1, -1, one_in_(2) ? FEAT_FLOOR_WATER : FEAT_FLOOR_MUD, FRACTAL_TYPE_33x65, 15, FEAT_NONE, 0);
    }

    /* Place some smaller earth wall formations */
    for (i = 0; i < 7; i++)
    {
        build_formation(-1, -1, FEAT_WALL_EARTH, FRACTAL_TYPE_17x17, 10, one_in_(2) ? FEAT_FLOOR_MUD: FEAT_NONE, 0);
    }

    /* Place some small vaults */
    for (i = 0; i < 5; i++)
    {
        /* They are somewhat rare */
        if (one_in_(10))
        {
            /* Pick a location */
            for (j = 0; j < 50; j++)
            {
                /* Get coordinates */
                y = rand_int(dun->row_rooms);
                x = rand_int(dun->col_rooms);

                /* Place the room */
                if (room_build(y, x, 7)) break;
            }
        }
    }

    /* Success */
    return (TRUE);
}


/*
 * Builds a pseudo-wilderness level on the dungeon
 * Returns TRUE on success, FALSE on error
 */
bool build_wilderness_level(void)
{
    int y, x, i;
    dun_data dun_body;
    bool done_ice = FALSE;
    bool is_quest_level = FALSE;
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Global data */
    dun = &dun_body;

    /* Clear it */
    memset(dun, 0, sizeof(dun_body));

    /* Set level type */
    set_dungeon_type(DUNGEON_TYPE_WILDERNESS);

    /* Reset terrain flags */
    level_flag = 0;

    /* Leave the player in the air for now */
    p_ptr->py = p_ptr->px = 0;

    /*check if we need a quest*/
    if (quest_check(p_ptr->depth) == QUEST_WILDERNESS)
    {
        is_quest_level = TRUE;
    }

    /* Try with a forest */
    if ((p_ptr->depth < 35) || one_in_(2))
    {
        if (!build_forest_level())
        {
            if (cheat_room)
            {
                message(QString("failed to build a forest level"));
            }

            return (FALSE);
        }
    }
    /* Or try with an ice level */
    else
    {
        if (!build_ice_level())
        {
            if (cheat_room)
            {
                message(QString("failed to build an ice level"));
            }

            return (FALSE);
        }

        done_ice = TRUE;
    }

    /* Irregular borders */
    build_wilderness_borders(FEAT_WALL_GRANITE);

    /* Mandatory dungeon borders */
    set_perm_boundry();

    /* Place 3 or 5 down stairs near some walls */
    if (!alloc_stairs(FEAT_STAIRS_DOWN, (3 + randint(2))))
    {
        if (cheat_room)
        {
            message(QString("failed to place down stairs"));
        }

        return (FALSE);
    }

    /* Place 1 or 3 up stairs near some walls */
    if (!alloc_stairs(FEAT_STAIRS_UP, (1 + randint(2))))
    {
        if (cheat_room)
        {
            message(QString("failed to place down stairs"));
        }

        return (FALSE);
    }

    /* Place some things */
    if (!place_traps_rubble_player())
    {
        if (cheat_room)
        {
            message(QString("failed to place traps, rubble and player"));
        }

        return FALSE;
    }

    /* We don't want to trap the player in the level */
    /* Build a tunnel to some far location */
    if (TRUE)
    {
        /* Get the larger dimension of the dungeon */
        int d = MAX(p_ptr->cur_map_hgt, p_ptr->cur_map_wid);
        int tries = 0;

        /* Get the distance to that far location */
        d = ((2 * d) / 5);

        /* Pick a location */
        while (TRUE)
        {
            /* Get coordinates */
            y = randint(p_ptr->cur_map_hgt - 1);
            x = randint(p_ptr->cur_map_wid - 1);

            /* Check distance */
            if (distance(y, x, p_ptr->py, p_ptr->px) >= d) break;

            /* Too many tries */
            if (++tries > 200) return (FALSE);
        }

        /* Build the tunnel */
        build_tunnel(p_ptr->py, p_ptr->px, y, x);
    }

    /* Additional features */
    build_misc_features();

    /* Start destruction of the level for quests */
    if (is_quest_level)
    {
        /* These terrain types are processed in dungeon.c during the quest level */
        if (done_ice) add_wilderness_quest_terrain(FEAT_FLOOR_WATER_BOILING, FEAT_WALL_CRACKED_OVER_BOILING_WATER);
        else add_wilderness_quest_terrain(FEAT_FLOOR_MUD_BOILING, FEAT_WALL_CRACKED_OVER_BOILING_MUD);
    }

    /*get the hook*/
    get_mon_num_hook = monster_wilderness_labrynth_okay;

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Place some things */
    if (!place_monsters_objects())
    {
        /* Reset the allocation table */
        get_mon_num_hook = NULL;
        get_mon_num_prep();

        if (cheat_room)
        {
            message(QString("failed to place monsters and objects"));
        }

        return FALSE;
    }

    /* Reset the allocation table */
    get_mon_num_hook = NULL;
    get_mon_num_prep();

    /* Special illumination for ice levels */
    if (done_ice && ((p_ptr->depth < 50) || one_in_(4))) light_elements(TRUE);

    /*final preps if this is a quest level*/
    if (is_quest_level)
    {
        u16b obj_count = WILDERNESS_COLLECT;
        u16b mon_count = 0;

        q_ptr->q_num_killed = 0;
        q_ptr->q_max_num = 0;

        /*
         * Go through every monster, and mark them as a questor,
         * then make them slightly faster, and light sleepers
         */
        /* Process the monsters */
        for (i = 1; i < mon_max; i++)
        {
            monster_type *m_ptr = &mon_list[i];

            /* Ignore non-existant monsters */
            if (!m_ptr->r_idx) continue;

            /*mark it as a quest monster*/
            m_ptr->mflag |= (MFLAG_QUEST);

            /* Count it */
            mon_count++;

            /* One in 25 generate a bonus item */
            if ((mon_max % 25) == 0) m_ptr->mflag |= (MFLAG_BONUS_ITEM);
        }

        /* Process the monsters */
        for (i = 1; i < mon_max; i++)
        {
            monster_type *m_ptr = &mon_list[i];

            /* Ignore non-existant monsters */
            if (!m_ptr->r_idx) continue;

            if (randint0(mon_count) < obj_count)
            {
                object_type *i_ptr;
                object_type object_type_body;

                /* Make a piece of parchment */
                int k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);
                i_ptr = &object_type_body;
                i_ptr->object_wipe();
                object_prep(i_ptr, k_idx);
                apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);

                /*Don't let the player see what the object it, and make it a quest item*/
                i_ptr->ident |= (IDENT_HIDE_CARRY | IDENT_QUEST);

                i_ptr->mark_known(TRUE);

                (void)monster_carry(i, i_ptr);

                /* One less object to drop */
                obj_count--;
            }

            /* One less monster to count */
            mon_count--;

            /* We are done */
            if (!obj_count) break;
        }

        /* Drop some additional parchments */
        alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_PARCHMENT, (WILDERNESS_COLLECT / 2));
    }

    /* Drop some chests to make the level worth exploring */
    alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_CHEST, damroll(3, 3));

    /* Let the player see the whole level */
    wiz_light();

    /* Try hard to place this level */
    rating += 25;

    /* Success */
    return (TRUE);
}



/*
 *
 * Build a themed level
 *
 */

/*
 * Select a monster type for a themed level
 */
byte get_level_theme(s16b orig_theme_num, bool quest_level)
{
    s16b mindepth, theme_depth;

    s16b theme_num = orig_theme_num;

    /*enforce minimum depth, to keep weak nests out of deep levels*/
    mindepth = theme_num / 4;

    /*keep total to 100 or less*/
    if ((mindepth + theme_num) > 100) theme_num = 100 - mindepth;

    /* Hack -- Choose a nest type */
    theme_depth = randint(theme_num) + mindepth;

    if ((theme_depth <= 20) && (orig_theme_num <= 28))
    {
        /*Coins, minor demons, Orcs, or a mixture of a couple monsters*/
        if (one_in_(4))			return (LEV_THEME_ORC_NAGA_YEEK_KOBOLD);
        else if (one_in_(3))	return (LEV_THEME_DEMON_MINOR);
        else if (one_in_(2))	return (LEV_THEME_CREEPING_COIN);
        else					return (LEV_THEME_ORC);
    }

    else if ((theme_depth <= 35)  && (orig_theme_num <= 40))
    {
        /*Trolls or Ogres, or a mixture of cave dwellers*/
        if (one_in_(3))			return (LEV_THEME_CAVE_DWELLER);
        else if (one_in_(2))	return (LEV_THEME_TROLL);
        else					return (LEV_THEME_OGRE);
    }

    else if ((theme_depth <= 50) && (orig_theme_num <= 60))
    {
        /* Hounds, demon, servants of valar, or young dragon*/
        if (one_in_(4))			return (LEV_THEME_VALAR_SERVANTS);
        if (one_in_(3))			return (LEV_THEME_DEMON_ALL);
        else if (one_in_(2))	return (LEV_THEME_HOUND);
        else					return (LEV_THEME_DRAGON_YOUNG);
    }

    /* Giant, animal, servants of valar, or humanoid pit */
    else if ((theme_depth <= 60) && (orig_theme_num <= 80))
    {
        if (one_in_(5))			return (LEV_THEME_VALAR_SERVANTS);
        else if (one_in_(4))	return (LEV_THEME_HUMANOID);
        else if (one_in_(3))	return (LEV_THEME_UNDEAD);
        else if (one_in_(2))	return (LEV_THEME_GIANT);
        else 					return (LEV_THEME_ANIMAL);
    }

    /* Dragon pit */
    else if (theme_depth <= 75)
    {
        /* Don't do specific elements for quest levels.  Too easy for those with the right immunity. */
        if (quest_level) return (LEV_THEME_DRAGON_ELEMENTAL);

        /* Pick dragon type */
        switch (rand_int(6))
        {
            /* Black */
            case 0:	return (LEV_THEME_DRAGON_ACID);
            /* Blue */
            case 1:	return (LEV_THEME_DRAGON_ELEC);
            /* Red */
            case 2:	return (LEV_THEME_DRAGON_FIRE);
            /* White */
            case 3:	return (LEV_THEME_DRAGON_COLD);
            /* Green */
            case 4:	return (LEV_THEME_DRAGON_POIS);
            /* Chromatic */
            default:return (LEV_THEME_DRAGON_CHROMATIC);
        }
    }

    else
    {
        if (one_in_(3))		return (LEV_THEME_VALAR_SERVANTS);

        /* Ancient Dragon pit */
        else if one_in_(2)	return (LEV_THEME_DRAGON_ANCIENT);

        /* > 90 - Demon pit */
        else			return (LEV_THEME_DEMON_MAJOR);
    }
}



/*
 * Build lakes and other terrain features for the given themed level
 */
static void build_themed_level_nature(byte theme)
{
    u16b i;

    /* Clear the level flag */
    level_flag = 0;

    /* No NPP terrains option turned on */
    if (birth_classic_dungeons) return;

    /* Find if the theme has some restrictions to generate terrain */
    for (i = 0; i < CUR_NUM_THEME_LEVEL_FLAGS; i++)
    {
        /* Found the theme? */
        if (theme == themed_level_flags[i].theme)
        {
            /* Apply the restriction */
            level_flag |= themed_level_flags[i].flags;

            /* Done */
            break;
        }
    }

    /* TODO: add lakes */
}


/*
 * Generate a themed dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
bool build_themed_level(void)
{
    int i;
    int r_idx;
    monster_race *r_ptr;
    int by, bx;
    byte is_quest_level = FALSE;
    long value;
    int total = 0;
    int max_depth;
    u32b max_diff, max_diff_unique;
    u32b highest_power = 0;
    int total_mon_placed = 0;

    int max_uniques = 3;

    alloc_entry *table = alloc_race_table;

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    byte level_theme;

    dun_data dun_body;

    u16b monster_number, potential_monsters;

    /* Global data */
    dun = &dun_body;

    /* Set the level type */
    set_dungeon_type(DUNGEON_TYPE_THEMED_LEVEL);

    /*check if we need a quest*/
    if (quest_check(p_ptr->depth) == QUEST_THEMED_LEVEL) is_quest_level = TRUE;

    /* Always a dungeon of 5 * 10 blocks*/
    p_ptr->cur_map_hgt = MAX_DUNGEON_HGT * 5 / 6;
    p_ptr->cur_map_wid = MAX_DUNGEON_WID * 5 / 9;

    /* Hack -- Start with basic granite */
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

    /* No rooms yet */
    dun->cent_n = 0;

    /* Select the monster theme, get the feeling */
    if (is_quest_level) level_theme = q_ptr->q_theme;
    else level_theme = get_level_theme(p_ptr->depth, FALSE);

    /* Insert the feeling now */
    feeling = level_theme + LEV_THEME_HEAD;

    /* Setup special features */
    build_themed_level_nature(level_theme);

    /* Build some rooms, note starburst rooms fail alot*/
    for (i = 0; i < DUN_ROOMS * 2; i++)
    {
        /* Pick a block for the room */
        by = rand_int(dun->row_rooms);
        bx = rand_int(dun->col_rooms);

        /*make most% of the rooms starburst rooms*/
        if (i < DUN_ROOMS)
        {
            /*make the first 33% of  attempts at an unusually large room*/
            bool large_room = ((dun->cent_n < 1) ? 11 : 10);

            if (!room_build(by, bx, large_room)) continue;
        }

        /* or else Attempt a trivial room */
        else if (room_build(by, bx, 1)) continue;
    }

    /*start over on all themed levels with less than 4 rooms due to inevitable crash*/
    if (dun->cent_n < 4)
    {
        if (cheat_room)
        {
            message(QString("not enough rooms"));
        }
        return (FALSE);
    }

    /*set the permanent walls*/
    set_perm_boundry();

    /*make the tunnels*/
    if (!scramble_and_connect_rooms_stairs())
    {
        if (cheat_room)
        {
            message(QString("unable to scramble and connect rooms"));
        }

        return (FALSE);
    }

    if (!place_traps_rubble_player()) return (FALSE);

    /*note we are going to have an extremely crowded dungeon*/
    /*there is no need for objects on the floor -JG*/

    /*get the hook*/
    get_mon_hook(level_theme);

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Monsters can be up to 7 levels out of depth, 10 for a quest */
    max_depth = p_ptr->depth + (is_quest_level ? THEMED_LEVEL_QUEST_BOOST : THEMED_LEVEL_NO_QUEST_BOOST);

    /*don't make it too easy if the player isn't diving very fast*/
    if (p_ptr->depth < p_ptr->max_lev)
    {
        max_depth += ((p_ptr->max_lev - p_ptr->depth) / 2);
    }

    /* Undead themed levels are just too hard */
    if (level_theme == LEV_THEME_UNDEAD) max_depth -= 2;

    /*boundry control*/
    if (max_depth > (MAX_DEPTH - 15)) max_depth = MAX_DEPTH - 15;

    /*get the average difficulty spanning 5 levels for monsters*/
    max_diff = max_diff_unique = 0;

    /*first get the total of the 5 levels*/
    for (i = 0; i < 5; i++)
    {
        /*put some boundry control on the highest level*/
        max_diff += mon_power_ave[max_depth + i][CREATURE_NON_UNIQUE];
        max_diff_unique += mon_power_ave[max_depth + i][CREATURE_UNIQUE];
    }

    /*now get the average*/
    max_diff /= 5;
    max_diff_unique /= 5;

    /* Quest levels are a little more crowded than non-quest levels*/
    if (is_quest_level) monster_number = QUEST_THEMED_LEVEL_NUM;
    else monster_number = 250;

    /* Undead themed levels are just too hard */
    if (level_theme == LEV_THEME_UNDEAD) monster_number /= 2;

    /* Reduce the number as monsters get more powerful*/
    monster_number -= ((p_ptr->depth / 3) + randint(p_ptr->depth / 3));

    /*boundry control*/
    if (monster_number > (z_info->m_max	- 25)) monster_number = z_info->m_max - 25;

    /*start the counter for potential monsters*/
    potential_monsters = 0;

    /*
     * Process the probabilities, starting from the back forward
     */
    for (i = alloc_race_size - 1; i >= 0; i--)
    {
        /* Default */
        table[i].prob3 = 0;

        /* Monster is not a part of this theme*/
        if (table[i].prob2 == 0) continue;

        /* Get the "r_idx" of the chosen monster */
        r_idx = table[i].index;

        /* Get the actual race */
        r_ptr = &r_info[r_idx];

        /*enforce a maximum depth*/
        if (r_ptr->level > max_depth) continue;

        /*no player ghosts*/
        if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

        /* Uniques only for unique quests*/
        if (r_ptr->flags1 & (RF1_UNIQUE))
        {
            /*get the right difficulty*/
            if (r_ptr->mon_power > max_diff_unique) continue;

            /* no dead ones*/
            if (r_ptr->cur_num >= r_ptr->max_num) continue;
        }
        /*other monsters based on difficulty*/
        else
        {
            /*get the right difficulty*/
            if (r_ptr->mon_power > max_diff) continue;
        }

        /*hack - no town monsters*/
        if (table[i].level <= 0) continue;

        /* Depth Monsters never appear in quests*/
        if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) continue;

        /* Accept the monster*/
        table[i].prob3 = table[i].prob2;

        /*limit the probability of weaker or stronger monsters*/
        if (!(r_ptr->flags1 & (RF1_UNIQUE)))
        {
            byte num;

            /*once we have enough monsters, start limiting the weaker ones*/
            if (potential_monsters > (monster_number * 3)) num = max_themed_monsters(r_ptr, max_diff);
            else num = r_ptr->max_num;

            /*reduce the probability*/
            if (num == MON_RARE_FREQ) table[i].prob3 /= 10;
            else if (num == MON_LESS_FREQ) table[i].prob3 /= 3;
            else if (!num) table[i].prob3 = 0;

            potential_monsters += num;
        }

        /*but always allow uniques*/
        else potential_monsters += 1;

        /* Total */
        total += table[i].prob3;

        /*record the most powerful monster*/
        if (r_ptr->mon_power > highest_power) highest_power = r_ptr->mon_power;
    }

    /*no eligible creatures - should never happen*/
    if (!total)
    {
        /* Remove restriction */
        get_mon_num_hook = NULL;

        /* Prepare allocation table */
        get_mon_num_prep();

        /* No monsters - no themed level */
        return (FALSE);
    }

    /*place the monsters in the dungeon*/
    while (total_mon_placed < monster_number)
    {
        int y, x;
        bool dont_use = FALSE;
        int tries = 0;

        /* Pick a monster */
        value = rand_int(total);

        /* Find the monster */
        for (i = 0; i < alloc_race_size; i++)
        {
            /* Found the entry */
            if (value < table[i].prob3) break;

            /* Decrement */
            value = value - table[i].prob3;
        }

        /* Get the "r_idx" of the chosen monster */
        r_idx = table[i].index;

        /* Get the actual race */
        r_ptr = &r_info[r_idx];

        /*don't attempt to re-place duplicate unique entries*/
        if (r_ptr->cur_num >= r_ptr->max_num) dont_use = TRUE;

        /*No more of this type of monster on the level, the monster type has been restricted*/
        if ((table[i].prob3 < table[i].prob2)  &&
            (r_ptr->cur_num >= max_themed_monsters(r_ptr, max_diff)))
        {
            dont_use = TRUE;
        }

        /*not a monster*/
        if (!r_idx) dont_use = TRUE;

        /* Limit the number of uniques per level */
        if ((r_ptr->flags1 & (RF1_UNIQUE)) && (max_uniques < 1)) dont_use = TRUE;

        if (dont_use)
        {
            /*Take out the weakest monster and repeat*/
            total -= table[i].prob3;
            table[i].prob3 = 0;

            /*we have maxed out all possible types of monsters*/
            if (total < 1) break;

            /*go back to the beginning*/
            continue;
        }

        /* Pick a location */
        while (TRUE)
        {
            int dist;

            /* Can't find a location for this monster */
            if (++tries >= 2500)
            {
                /* Remove restriction */
                get_mon_num_hook = NULL;

                /* Prepare allocation table */
                get_mon_num_prep();

                /* No themed level */
                return (FALSE);
            }

            y = rand_int(p_ptr->cur_map_hgt);
            x = rand_int(p_ptr->cur_map_wid);

            if (!cave_empty_bold(y,x)) continue;

            /*better distance*/
            dist = distance(p_ptr->py, p_ptr->px, y, x);

            /*hack - too close*/
            if (dist < 5) continue;

            /*give the player a chance - no monsters starting in LOS*/
            if (dist < MAX_RANGE)
            {
                if (los(y, x, p_ptr->py, p_ptr->px)) continue;
            }

            /*found a spot*/
            break;
        }

        /* Attempt to place the monster, allow sleeping, don't allow groups*/
        if (!place_monster_aux(y, x, r_idx, MPLACE_SLEEP)) continue;

        /* Count it */
        total_mon_placed++;

        if (r_ptr->flags1 & (RF1_UNIQUE)) max_uniques--;

        /* Mark 1 in 17 monsters for a bonus item */
        if (dungeon_info[y][x].monster_idx > 0)
        {
            monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

            if ((mon_cnt % 15) == 0) m_ptr->mflag |= (MFLAG_BONUS_ITEM);
        }
    }

    /*final preps if this is a quest level*/
    if (is_quest_level)
    {
        q_ptr->q_num_killed = 0;
        q_ptr->q_max_num = 0;

        /*
         * Go through every monster, and mark them as a questor,
         * then make them slightly faster, and light sleepers
         */
        /* Process the monsters */
        for (i = 1; i < mon_max; i++)
        {
            monster_type *m_ptr = &mon_list[i];
            monster_race *r_ptr;

            /* Ignore non-existant monsters */
            if (!m_ptr->r_idx) continue;

            r_ptr = &r_info[m_ptr->r_idx];

            /*mark it as a quest monster*/
            m_ptr->mflag |= (MFLAG_QUEST);

            if (!(r_ptr->flags1 & RF1_UNIQUE))
            {
                m_ptr->mflag &= ~(MFLAG_SLOWER);
                m_ptr->mflag |= (MFLAG_FASTER);
                calc_monster_speed(m_ptr->fy, m_ptr->fx);
            }

            /*increase the max_num counter*/
            q_ptr->q_max_num ++;

            /*Not many of them sleeping, others lightly sleeping*/
            if (one_in_(2)) m_ptr->m_timed[MON_TMD_SLEEP] = 0;
            else m_ptr->m_timed[MON_TMD_SLEEP] /= 2;
        }

        /* Process the mimic objects */
        for (i = 1; i < o_max; i++)
        {
            object_type *o_ptr = &o_list[i];

            /* Skip dead objects */
            if (!o_ptr->k_idx) continue;

            if (!o_ptr->is_mimic()) continue;

            /* Mark it as a questor */
            o_ptr->ident |= (IDENT_QUEST);

            /*increase the max_num counter*/
            q_ptr->q_max_num ++;
        }
    }

    /* Remove restriction */
    get_mon_num_hook = NULL;

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Try hard to place this level */
    rating += 25;

    return (TRUE);
}



/*
 *
 * Build a labyrnth level
 *
 */


/*
 * Determine if a monster is suitable for a labyrnth
 */
bool monster_wilderness_labrynth_okay(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* No breeders */
    if (r_ptr->flags2 & (RF2_MULTIPLY)) return (FALSE);

    /* creature must move */
    if (r_ptr->flags1 & (RF1_NEVER_MOVE)) return (FALSE);

    /* No mimics */
    if (r_ptr->flags1 & (RF1_CHAR_MIMIC)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/**
 * Used to convert (x, y) into an array index (i) in build_labrynth_level().
 */
static int lab_toi(int y, int x, int w)
{
    return y * w + x;
}


/**
 * Used to convert an array index (i) into (x, y) in labyrinth_gen().
 */
static void lab_toyx(int i, int w, int *y, int *x)
{
    *y = i / w;
    *x = i % w;
}


/**
 * Given an adjoining wall (a wall which separates two labyrinth cells)
 * set a and b to point to the cell indices which are separated. Used by
 * labyrinth_gen().
 */
static void lab_get_adjoin(int i, int w, int *a, int *b)
{
    int y, x;
    lab_toyx(i, w, &y, &x);
    if (x % 2 == 0)
    {
        *a = lab_toi(y - 1, x, w);
        *b = lab_toi(y + 1, x, w);
    }
    else
    {
        *a = lab_toi(y, x - 1, w);
        *b = lab_toi(y, x + 1, w);
    }
}



/**
 * Shuffle an array using Knuth's shuffle.
 */
static void shuffle(int *arr, int n)
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


/**
 * Return whether (x, y) is in a tunnel.
 *
 * For our purposes a tunnel is a horizontal or vertical path, not an
 * intersection. Thus, we want the squares on either side to walls in one
 * case (e.g. up/down) and open in the other case (e.g. left/right). We don't
 * want a square that represents an intersection point.
 *
 * The high-level idea is that these are squares which can't be avoided (by
 * walking diagonally around them).
 */
static bool lab_is_tunnel(int y, int x)
{
    bool west = cave_naked_bold(y, x - 1);
    bool east = cave_naked_bold(y, x + 1);
    bool north = cave_naked_bold(y - 1, x);
    bool south = cave_naked_bold(y + 1, x);

    return ((north == south) && (west == east) && (north != west));
}



/**
 * Build a labyrinth level.
 *
 * Note that if the function returns FALSE, a level wasn't generated.
 * Labyrinths use the dungeon level's number to determine whether to generate
 * themselves (which means certain level numbers are more likely to generate
 * labyrinths than others).  Taken from Angband 3.3.
 */
bool build_labyrinth_level(void)
{
    int i, j, k, y, x;
    bool is_quest_level = FALSE;
    int cave_squares_x[LABYRINTH_AREA];
    int cave_squares_y[LABYRINTH_AREA];
    int cave_squares_max = 0;

    /*
     * Size of the actual labyrinth part must be odd.
     * NOTE: these are not the actual dungeon size, but rather the size of the
     * area we're generating a labyrinth in (which doesn't count the enclosing
     * outer walls.
     */

    int hgt = LABYRINTH_HGT;
    int wid = LABYRINTH_WID;
    int area = hgt * wid;

    /* NOTE: 'sets' and 'walls' are too large... we only need to use about
     * 1/4 as much memory. However, in that case, the addressing math becomes
     * a lot more complicated, so let's just stick with this because it's
     * easier to read. */

    /* 'sets' tracks connectedness; if sets[i] == sets[j] then cells i and j
     * are connected to each other in the maze. */
    int sets[LABYRINTH_AREA];

    /* 'walls' is a list of wall coordinates which we will randomize */
    int walls[LABYRINTH_AREA];

    /* Most labyrinths are lit */
    bool lit = ((randint0(p_ptr->depth) < 25) || (one_in_(2)));

    /* Many labyrinths are known */
    bool known = (lit && (randint0(p_ptr->depth) < 25));

    /* Most labyrinths have soft (diggable) walls */
    bool soft = ((randint0(p_ptr->depth) < 35) || (!one_in_(3)));

    /* Check if we need a quest */
    if (quest_check(p_ptr->depth) == QUEST_LABYRINTH)
    {
        is_quest_level = TRUE;
        known = TRUE;

        /* Permanent walls for the quests */
        soft = FALSE;

        /* Quest levels are much smaller */
        wid = hgt = LABYRINTH_QUEST_DIMENSIONS;
        area = LABYRINTH_QUEST_AREA;
    }

    p_ptr->cur_map_hgt = hgt + 2;
    p_ptr->cur_map_wid = wid + 2;

    /* Set level type */
    set_dungeon_type(DUNGEON_TYPE_LABYRINTH);

    /* Reset terrain flags */
    level_flag = 0;

    /* Leave the player in the air for now */
    p_ptr->py = p_ptr->px = 0;

    /*
     * Build permanent walls.
     */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Create permanent wall */
            cave_set_feat(y, x, FEAT_WALL_PERM_SOLID);
        }
    }

    /*
     * Start with solid wall everywhere.
     */
    for (y = 1; y < p_ptr->cur_map_hgt - 1; y++)
    {
        for (x = 1; x < p_ptr->cur_map_wid - 1; x++)
        {
            /* Either soft or permanent walls */
            cave_set_feat(y, x, soft ? FEAT_WALL_GRANITE_SOLID : FEAT_WALL_PERM_SOLID);
        }
    }

    /* Initialize each wall. */
    for (i = 0; i < area; i++)
    {
        walls[i] = i;
        sets[i] = -1;
    }

    /* Cut out a grid of 1x1 rooms which we will call "cells" */
    for (y = 0; y < hgt; y += 2)
    {
        for (x = 0; x < wid; x += 2)
        {
            int k = lab_toi(y, x, wid);
            sets[k] = k;
            cave_set_feat(y + 1, x + 1, FEAT_FLOOR);
            dungeon_info[y + 1][x + 1].cave_info |= (CAVE_ROOM);
            if (lit) dungeon_info[y + 1][x + 1].cave_info |= (CAVE_GLOW);
        }
    }

    /* Shuffle the walls, using Knuth's shuffle. */
    shuffle(walls, area);

    /*
     * For each adjoining wall, look at the cells it divides. If they aren't
     * in the same set, remove the wall and join their sets.
     *
     * This is a randomized version of Kruskal's algorithm.
      */
    for (i = 0; i < area; i++)
    {
        int a, b, x, y;

        j = walls[i];

        /* If this cell isn't an adjoining wall, skip it */
        lab_toyx(j, wid, &y, &x);
        if ((x < 1 && y < 1) || (x > wid - 2 && y > hgt - 2)) continue;
        if (x % 2 == y % 2) continue;

        /* Figure out which cells are separated by this wall */
        lab_get_adjoin(j, wid, &a, &b);

        /* If the cells aren't connected, kill the wall and join the sets */
        if (sets[a] != sets[b])
        {
            int sa = sets[a];
            int sb = sets[b];
            cave_set_feat(y + 1, x + 1, FEAT_FLOOR);
            if (lit) dungeon_info[y + 1][x + 1].cave_info |= (CAVE_GLOW);

            for (k = 0; k < area; k++)
            {
                if (sets[k] == sb) sets[k] = sa;
            }
        }
    }

    /* Place 1 or 2 down stairs  */
    if (!alloc_stairs(FEAT_STAIRS_DOWN, (randint1(2))))
    {
        if (cheat_room)
        {
            message(QString("failed to place down stairs"));
        }

        return (FALSE);
    }

    /* Place 1 or 2 up stairs */
    if (!alloc_stairs(FEAT_STAIRS_UP, (randint1(2))))
    {
        if (cheat_room)
        {
            message(QString("failed to place down stairs"));
        }
        return (FALSE);
    }

    /* No doors, traps or rubble in quest levels */
    if (!is_quest_level)
    {
        /* Test each square in (random) order for openness */
        for (y = 1; y < p_ptr->cur_map_hgt - 1; y++)
        {
            for (x = 1; x < p_ptr->cur_map_wid - 1; x++)
            {
                if (cave_naked_bold(y, x))
                {
                    /* Not in the rooms */
                    if (!lab_is_tunnel(y, x)) continue;

                    cave_squares_y[cave_squares_max] = y;
                    cave_squares_x[cave_squares_max] = x;
                    cave_squares_max++;
                }
            }
        }

        /* Paranoia */
        if (!cave_squares_max)
        {
            if (cheat_room)
            {
                message(QString("failed to place doors"));
            }
            return (FALSE);
        }

        /* Generate a door for every 100 squares in the labyrinth */
        for (i = area / 100; i > 0; i--)
        {
            /* Paranoia */
            if (!cave_squares_max) break;

            x = randint0(cave_squares_max);

            /* Place same doors */
            place_random_door(cave_squares_y[x], cave_squares_x[x]);

            /* Replace the current with the top one */
            cave_squares_max--;
            cave_squares_y[x] = cave_squares_y[cave_squares_max];
            cave_squares_x[x] = cave_squares_x[cave_squares_max];
        }

        /* Place some traps and rubble */
        x = MAX(MIN(p_ptr->depth / 3, 10), 2);
        x = (3 * x * (hgt * wid)) / (MAX_DUNGEON_HGT * MAX_DUNGEON_WID);
        alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(x));
        alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(x));
    }

    /* Determine the character location, if it is needed */
    if (!new_player_spot_old())
    {
        if (cheat_room)
        {
            message(QString("failed to place player"));
        }

        return (FALSE);
    }

    /* Only put monsters on non-quest levels */
    if (!is_quest_level)
    {
        /*get the hook*/
        get_mon_num_hook = monster_wilderness_labrynth_okay;

        /* Prepare allocation table */
        get_mon_num_prep();

        /* Slightly out of depth */
        monster_level = p_ptr->depth + 2;

        /* Place some things */
        if (!place_monsters_objects())
        {
            if (cheat_room)
            {
                message(QString("failed to place monsters and objects"));
            }

            /* Reset the allocation table */
            get_mon_num_hook = NULL;
            get_mon_num_prep();
            monster_level = p_ptr->depth;

            return FALSE;
        }

        /* Reset the allocation table */
        get_mon_num_hook = NULL;
        get_mon_num_prep();
        monster_level = p_ptr->depth;
    }

    else q_info->turn_counter = (p_ptr->game_turn - 170);

    /* If we want the players to see the maze layout, do that now */
    if (known) wiz_light();

    /* Try hard to place this level */
    rating += 25;

    /* Success */
    return (TRUE);
}



/*
 *
 * Build an arena level
 *
 */


/*
 * Helper function for building and updating arena levels.
 * Also called from cave.c to have the walls gradually disappear
 * as the arena quest progresses.
 */
void update_arena_level(byte stage)
{
    byte y, x;

    /* No values higher than 9, see arena_level_map in table.c */
    if (stage >= ARENA_MAX_STAGES) return;

    /*
     * Start with add floor spaces where appropriate.
     * See arena_level_map in tables.c
     */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Ignore the outer walls locations */
            if (!in_bounds_fully(y, x)) continue;

            /* Look for an exact match to the stage */
            if (arena_level_map[y][x] != stage) continue;

            /* Expand the floor area */
            cave_set_feat(y, x, FEAT_FLOOR);

            /* Make it all one big room, and light it up */
            dungeon_info[y][x].cave_info |= (CAVE_ROOM | CAVE_GLOW);
            if (character_dungeon) light_spot(y, x);
        }
    }

    /*
     * On the permanent walls bordering the floor, make them
     * an outer permanent wall.
     */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            byte d;

            /* Ignore the outer walls locations */
            if (!in_bounds_fully(y, x)) continue;

            /* Really set the feature */
            if (dungeon_info[y][x].feature_idx != FEAT_WALL_PERM_SOLID) continue;

            /* Look in all directions to see if it borders a floor space */
            for (d = 0; d < 8; d++)
            {
                /* Extract adjacent location */
                byte yy = y + ddy_ddd[d];
                byte xx = x + ddx_ddd[d];

                if (!in_bounds_fully(yy, xx)) continue;

                /* Square (y, x) borders the floor */
                if (cave_ff1_match(yy, xx, FF1_MOVE))
                {
                    /* Set it to be an inner permanent wall */
                    cave_set_feat(y, x, FEAT_WALL_PERM_INNER);

                    /* Make it all one big room, and light it up */
                    dungeon_info[y][x].cave_info |= (CAVE_ROOM | CAVE_GLOW);
                    if (character_dungeon) light_spot(y, x);

                    /* Go to the next square */
                    break;
                }
            }
        }
    }
}

/*
 * Helper function for build_arena_level.  Because it is a simple,
 * empty 1x5 corridor, placing the player should be easy *
 */
static bool player_place_arena(void)
{
    u16b empty_squares_y[ARENA_LEVEL_AREA];
    u16b empty_squares_x[ARENA_LEVEL_AREA];
    byte empty_squares = 0;
    byte slot, y, x;

    /*
     * Start with add floor spaces where appropriate.
     * See where the new squares are
     */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* New, and open square */
            if (cave_naked_bold(y, x))
            {
                empty_squares_y[empty_squares] = y;
                empty_squares_x[empty_squares] = x;
                empty_squares++;
            }
        }
    }

    /* Paranoia - shouldn't happen */
    if (!empty_squares) return (FALSE);

    /* Pick a square at random */
    slot = randint0(empty_squares);

    /* Hack - excape stairs stairs */
    p_ptr->create_stair = FEAT_STAIRS_UP;

    return (player_place(empty_squares_y[slot], empty_squares_x[slot]));
}


/*
 * Build a small room to place the player in an arena-like quest.
 * This level should only be built for arena quests.
 * Returns TRUE on success, FALSE on error, but there should never be an error.
 * Monsters and objects are added later in cave.c about every 100 game turns.
 */
bool build_arena_level(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    byte y, x;

    /* Set level type */
    set_dungeon_type(DUNGEON_TYPE_ARENA);

    /* Reset terrain flags */
    level_flag = 0;

    /* Leave the player in the air for now */
    p_ptr->py = p_ptr->px = 0;

    /* Make it a single size, normal room */
    p_ptr->cur_map_hgt = ARENA_LEVEL_HGT;
    p_ptr->cur_map_wid = ARENA_LEVEL_WID;

    /*
     * Start with solid wall everywhere.
     * See arena_level_map in tables.c
     */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Create permanent wall */
            cave_set_feat(y, x, FEAT_WALL_PERM_SOLID);
        }
    }

    /* Build the initial arena */
    update_arena_level(0);

    /* Should never fail, since there is only a simple dungeon floor */
    if (!player_place_arena())
    {
        if (cheat_room)
        {
            message(QString("Failed to place player"));
        }

        return (FALSE);
    }

    /* Mark the start of the quest */
    q_ptr->turn_counter = p_ptr->game_turn;

    /* Always use this level */
    rating += 100;

    /* Success */
    return (TRUE);
}





/*
 *
 * Build a greater vault level
 *
 */

/*
 * Helper function for build_greater_vault_level.
 * Placing the player should be easy *
 */
static bool player_place_greater_vault_level(void)
{
    QVector<coord> empty_squares;
    int slot, y, x;

    /*
     * Start with add floor spaces where appropriate.
     * See where the new squares are
     */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            if (!in_bounds_fully(y, x)) continue;

            /* Not part of the vault */
            if (dungeon_info[y][x].cave_info & (CAVE_ICKY)) continue;

            /* We want to be next to a wall */
            if (!next_to_walls(y, x)) continue;

            /* New, and open square */
            if (cave_naked_bold(y, x))
            {
                empty_squares.append(make_coords(y, x));
            }
        }
    }

    /* Paranoia - shouldn't happen */
    if (empty_squares.size() < 3) return (FALSE);

    /* Pick a square at random */
    slot = randint0(empty_squares.size());

    /* Hack - escape stairs */
    p_ptr->create_stair = FEAT_STAIRS_UP;

    if (!player_place(empty_squares.at(slot).y, empty_squares.at(slot).x)) return (FALSE);

    /* Select a new location for down stairs */
    empty_squares.removeAt(slot);

    /* Pick a square at random */
    slot = randint0(empty_squares.size());

    /* Now place one up stair */
    cave_set_feat(empty_squares.at(slot).y, empty_squares.at(slot).x, FEAT_STAIRS_UP);

    /* Select a new location for down stairs */
    empty_squares.removeAt(slot);

    /* Pick a square at random */
    slot = randint0(empty_squares.size());

    /* Now place one down stair */
    cave_set_feat(empty_squares.at(slot).y, empty_squares.at(slot).x, FEAT_STAIRS_DOWN);

    return (TRUE);
}


/*
 * Build a small room to place the player in an arena-like quest.
 * This level should only be built for arena quests.
 * Returns TRUE on success, FALSE on error, but there should never be an error.
 * Monsters and objects are added later in cave.c about every 100 game turns.
 * Surround it with a border three walls thick of normal granite.
 */
bool build_greater_vault_level(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    bool is_quest_level = FALSE;
    vault_type *v_ptr;
    int hgt, wid;

    /*check if we need a quest*/
    if (quest_check(p_ptr->depth) == QUEST_GREATER_VAULT)
    {
        is_quest_level = TRUE;
    }

     /* Get the vault record */
    if (is_quest_level)
    {
        v_ptr = &v_info[q_ptr->q_theme];
    }
    /* For ordinary greater vault levels, select one at random */
    else while (TRUE)
    {
        u16b vault_choice = randint0(z_info->v_max);

        /* Get a random vault record */
        v_ptr = &v_info[vault_choice];

        /* Accept the first greater vault */
        if (v_ptr->typ == 8) break;
    }

    /* Set level type */
    set_dungeon_type(DUNGEON_TYPE_GREATER_VAULT);

    /* Reset terrain flags */
    level_flag = 0;

    /* Leave the player in the air for now */
    p_ptr->py = p_ptr->px = 0;

    /* Make it a single greater vault with  */
    hgt = p_ptr->cur_map_hgt = v_ptr->hgt + 6;
    wid = p_ptr->cur_map_wid = v_ptr->wid + 6;

    /* All floors to start, except the outer boundry */
    generate_fill(0, 0, hgt - 1, wid - 1, FEAT_WALL_GRANITE_OUTER);
    generate_fill(4, 4, hgt - 5, wid - 5, FEAT_FLOOR);
    set_perm_boundry();

    /* Message */
    if (cheat_room)
    {
        message(QString("Greater vault (%1)") .arg(v_ptr->vault_name));
    }

    /* Boost the rating */
    rating += v_ptr->rat;

    /* Build the vault */
    build_vault((hgt / 2), (wid / 2), v_ptr);

    /* Mark vault grids with the CAVE_G_VAULT flag */
    mark_g_vault((hgt / 2), (wid / 2), v_ptr->hgt, v_ptr->wid);

    /* Remember the vault's name */
    g_vault_name = v_ptr->vault_name;

    /* Should never fail, since there is only a simple dungeon floor */
    if (!player_place_greater_vault_level())
    {
        if (cheat_room)
        {
            message(QString("Failed to place player"));
        }

        return (FALSE);
    }

    /*final preps if this is a quest level*/
    if (is_quest_level)
    {
        s16b i;

        /*
         * Go through every monster, and mark them as a questor.
         */
        for (i = 1; i < mon_max; i++)
        {
            monster_type *m_ptr = &mon_list[i];

            /* Ignore non-existant monsters */
            if (!m_ptr->r_idx) continue;

            /*mark it as a quest monster*/
            m_ptr->mflag |= (MFLAG_QUEST);
        }
    }

    /* Mark the start of the quest */
    q_ptr->turn_counter = p_ptr->game_turn;

    /* Always place this level */
    rating += 100;

    /* Success */
    return (TRUE);
}
