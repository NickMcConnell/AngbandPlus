
/* File: dun_gen_features.cpp */

/*
 * Copyright (c) 2010 Jeff Greene, Diego Gonzalez
 * Please see copyright.txt for complete copyright and licensing restrictions.
 */

// Code for generating starburst rooms, fractal areas, and elemental terrains.

#include "src/npp.h"
#include "src/dun_generate.h"



/*
 * Return a number between 1 and 6 that describes the size of the dungeon
 */
static byte ponder_dungeon_size(void)
{
    /* Get real dungeon size in panels */
    int hgt = (p_ptr->cur_map_hgt / PANEL_HGT);
    int wid = (p_ptr->cur_map_wid / PANEL_WID);

    /* Paranoia. Check limits */
    if (hgt > 6) hgt = 6;
    if (hgt < 2) hgt = 2;

    if (wid > 6) wid = 6;
    if (wid < 2) wid = 2;

    /* Ponder size */
    return (dungeon_size_tab[hgt][wid]);
}


/*
 * Return TRUE if the given feature is suitable to make a lake or river
 * on the current level
 */
static bool cave_feat_lake(int f_idx)
{
    /* Get the feature */
    feature_type *f_ptr = &f_info[f_idx];

    /* If this is TRUE we ignore the feature if certain elemental flags already exist in the level */
    bool reject_elements = FALSE;

    /* Require lake or river */
    if (!_feat_ff2_match(f_ptr, FF2_LAKE | FF2_RIVER)) return (FALSE);

    /* Analyze the elemental flags */
    switch (_feat_ff3_match(f_ptr, TERRAIN_MASK))
    {
        /* "Boring" features */
        case 0:
        case ELEMENT_SAND:
        case ELEMENT_MUD:
        case ELEMENT_FOREST:
        {
            /* Don't mess with lava levels */
            if (level_flag & (LF1_LAVA)) return (FALSE);

            break;
        }

        case ELEMENT_LAVA:
        {
            /* True lava needs true lava or an empty dungeon */
            if (!(level_flag & (LF1_LAVA))) reject_elements = TRUE;

            break;
        }

        case ELEMENT_BMUD:
        case ELEMENT_BWATER:
        {
            /* These ones need lava, bmud, bwater or an empty dungeon */
            if (!(level_flag & (LF1_LAVA | LF1_BMUD | LF1_BWATER))) reject_elements = TRUE;

            break;
        }

        case ELEMENT_FIRE:
        case ELEMENT_OIL:
        {
            /* These two and true lava are compatible */
            if (!(level_flag & (LF1_OIL | LF1_FIRE | LF1_LAVA))) reject_elements = TRUE;

            break;
        }

        case ELEMENT_ACID:
        {
            /* Acid needs acid or an empty dungeon */
            if (!(level_flag & (LF1_ACID))) reject_elements = TRUE;

            break;
        }

        case ELEMENT_ICE:
        {
            /* Ice needs ice or an empty dungeon */
            if (!(level_flag & (LF1_ICE))) reject_elements = TRUE;

            break;
        }

        case ELEMENT_WATER:
        {
            /* Don't mess with lava levels */
            if (level_flag & (LF1_LAVA)) return (FALSE);

            /* Water needs water, boiling water or ice */
            /* Water is also compatible with acid (flavor) */
            if (!(level_flag & (LF1_ACID | LF1_BWATER | LF1_WATER | LF1_ICE)))
            {
                reject_elements = TRUE;
            }

            break;
        }
    }

    /* Test the presence of certaine flags in the level if necessary */
    if (reject_elements && (level_flag &
        (LF1_LAVA | LF1_FIRE | LF1_OIL | LF1_ACID | LF1_WATER | LF1_ICE | LF1_BMUD | LF1_BWATER)))
    {
        /* Failure */
        return (FALSE);
    }

    /* Success */
    return (TRUE);
}


/*
 * Note that the order we generate the dungeon is terrain features, then
 * rooms, then corridors, then streamers. This is important, because
 * (currently) we ensure that deep or hostile terrain is bridged by safe
 * terrain, and rooms (and vaults) alway have their respective walls intact.
 *
 * Note that rooms can be generated inside 'big' lakes, but not on regular
 * lakes. We take a risk here that 'big' lakes are less likely to have
 * areas rendered inaccessible by having a room block them.
 *
 * XXX XXX XXX Currently both types of lakes can have areas that are completely
 * blocked because of the 20% chance of filling a lake centre location with
 * a lake edge location. We should always guarantee that all areas are connected.
 *
 * XXX XXX These huge case statements should be cut down by using WALL, FLOOR,
 * etc. flags to take out the common cases and ensuring we never overwrite a
 * dun square with an edge square. But the resulting code might be less
 * efficient.
 */

/*
 * Places a terrain on another terrain
 */

static void build_terrain(int y, int x, int feat)
{
    int oldfeat, newfeat;
    int k;
    int effect_rock = 0;

    feature_type *f_ptr;
    feature_type *f2_ptr;

    /* Get the feature */
    oldfeat = dungeon_info[y][x].feature_idx;
    f_ptr = &f_info[oldfeat];

    /* Set the new feature */
    newfeat = oldfeat;
    f2_ptr = &f_info[feat];

    /* Paranoia */
    if (!oldfeat)
    {
        newfeat = feat;
    }
    /* Put the feature quickly if we are overriding boring walls */
    else if (_feat_ff1_match(f_ptr, FF1_WALL) &&
        !_feat_ff3_match(f_ptr, TERRAIN_MASK))
    {
        newfeat = feat;
    }
    /* Tunnel the old feature */
    else if (_feat_ff1_match(f2_ptr, FF1_FLOOR) &&
        _feat_ff1_match(f_ptr, FF1_CAN_TUNNEL))
    {
        newfeat = feat_state(oldfeat, FS_TUNNEL);
    }
    /*
     * EXPERIMENTAL. Leave some grids untouched when overlapping lakes.
     * Note that we check for a match of the LOS, PROJECT and MOVE flags to
     * support rivers and tree branches properly (this is a hack).
     */
    else if (_feat_ff3_match(f2_ptr, TERRAIN_MASK) &&
        _feat_ff3_match(f_ptr, TERRAIN_MASK) &&
        (_feat_ff1_match(f2_ptr, FF1_MOVE | FF1_LOS | FF1_PROJECT) ==
        _feat_ff1_match(f_ptr, FF1_MOVE | FF1_LOS | FF1_PROJECT)) &&
        one_in_(4))
    {
        newfeat = oldfeat;
    }
    /* Handle new lava */
    else if (_feat_ff3_match(f2_ptr, FF3_LAVA))
    {
        /* We are filling a hole in the dungeon */
        if (!_feat_ff3_match(f_ptr, FF3_LAVA))
        {
            /* Heat the water */
            if (_feat_ff3_match(f_ptr, FF3_WATER))
            {
                newfeat = FEAT_FLOOR_WATER_BOILING;
            }
            /* Melt the old feature */
            else
            {
                newfeat = FEAT_FLOOR_MUD_BOILING;
            }
        }
        /* Burn old features */
        else if (_feat_ff2_match(f_ptr, FF2_HURT_FIRE))
        {
            newfeat = feat_state(oldfeat, FS_HURT_FIRE);
        }
        /* Lava overrides all */
        else
        {
            newfeat = feat;
        }
    }
    /* Handle old lava */
    else if (_feat_ff3_match(f_ptr, FF3_LAVA))
    {
        /* Heat the water */
        if (_feat_ff3_match(f2_ptr, FF3_WATER))
        {
            newfeat = FEAT_FLOOR_WATER_BOILING;
        }
        /* Melt the new feature */
        else
        {
            newfeat = FEAT_FLOOR_MUD_BOILING;
        }
    }

    /* Handle new fire */
    else if (_feat_ff3_match(f2_ptr, FF3_FIRE))
    {
        /* Burn the old feature */
        if (_feat_ff2_match(f_ptr, FF2_HURT_FIRE))
        {
            newfeat = feat_state(oldfeat, FS_HURT_FIRE);
        }
        /* Some features resist fire */
        else if (!_feat_ff3_match(f_ptr, FF3_ICE | FF3_WATER))
        {
            newfeat = feat;
        }
    }

    /* Handle new ice */
    else if (_feat_ff3_match(f2_ptr, FF3_ICE) &&
        _feat_ff2_match(f_ptr, FF2_HURT_COLD))
    {
        newfeat = feat_state(oldfeat, FS_HURT_COLD);
    }
    /* Handle new water */
    else if (_feat_ff3_match(f2_ptr, FF3_WATER))
    {
        if (!_feat_ff3_match(f_ptr, FF3_WATER))
        {
            newfeat = feat;
        }
        /* Note that old water is unnafected to avoid "wave lakes" */
        else if ((_feat_ff3_match(f_ptr, TERRAIN_MASK) != FF3_WATER) &&
            _feat_ff2_match(f_ptr, FF2_HURT_WATER))
        {
            newfeat = feat_state(oldfeat, FS_HURT_WATER);
        }
    }
    /* All special cases were ignored. Put the feature */
    else
    {
        newfeat = feat;
    }

    /* Hack -- no change */
    if (newfeat == oldfeat) return;

    /* Get the chance to replace a feature */
    k = randint(100);

    /* Replace some features with something else (sometimes) */
    switch (newfeat)
    {
        case FEAT_WALL_LIMESTONE:
        {
            if (k <= 40) newfeat = FEAT_FLOOR;

            else if (k <= 60) newfeat = FEAT_FLOOR_WATER;

            break;
        }

        case FEAT_FLOOR_MUD:
        {
            if (k <= 10) newfeat = FEAT_FLOOR_EARTH;

            else if (k <= 23) newfeat = FEAT_FLOOR_MUD;

            else if (k <= 24) newfeat = FEAT_TREE;

            break;
        }

        case FEAT_FLOOR_MUD_BOILING:
        {
            if (k <= 10) newfeat = FEAT_FLOOR_WATER_BOILING;

            break;
        }

        case FEAT_FLOOR_WATER_BOILING:
        {
            if (k <= 10) newfeat = FEAT_MAGMA_VEIN;

            else if (k <= 13) newfeat = FEAT_WALL_WATER_BOILING_GEYSER;

            break;
        }

        case FEAT_FOREST_SOIL:
        {
            if (k <= 10) newfeat = FEAT_TREE;

            else if (k <= 20) newfeat = FEAT_FOREST_SOIL;

            else if (k <= 30) newfeat = FEAT_FOREST_SOIL_DYNAMIC;

            else if (k <= 40) newfeat = FEAT_GRASS;

            else if (k <= 45) newfeat = FEAT_GRASS_DYNAMIC;

            break;
        }

        case FEAT_FLOOR_ICE:
        {
            if (k <= 5) newfeat = FEAT_WALL_ICE_CRACKED;

            break;
        }

        case FEAT_WALL_ICE:
        {
            if (k <= 25) newfeat = FEAT_WALL_ICE_CRACKED;

            else if (k <= 50) newfeat = FEAT_FLOOR_ICE;

            break;
        }

        case FEAT_WALL_ICE_CRACKED:
        {
            if (k <= 90) newfeat = FEAT_WALL_ICE;

            break;
        }

        case FEAT_FLOOR_ACID:
        {
            if (k <= 5)
            {
                newfeat = FEAT_CRACKED_WALL_OVER_ACID;
            }

            break;
        }

        case FEAT_WALL_COAL:
        {
            if (k <= 5) newfeat = FEAT_WALL_COAL_BURNING;

            else if (k <= 50) newfeat = FEAT_BURNT_SPOT;

            else if (k <= 60) newfeat = FEAT_FIRE;

            break;
        }

        case FEAT_WALL_SHALE:
        {
            if (k <= 5) newfeat = FEAT_WALL_COAL;

            break;
        }


        case FEAT_FLOOR_SAND:
        {
            if (k <= 5) newfeat = FEAT_WALL_SANDSTONE;

            else if (k <= 10) newfeat = FEAT_QUARTZ_VEIN;

            else if (k <= 20)
            {
                newfeat = FEAT_FLOOR_ROCK;
                effect_rock = FEAT_LOOSE_ROCK;
            }

            else if (k <= 25) newfeat = FEAT_FLOOR_ROCK;

            break;
        }

        case FEAT_THICKET:
        {
            if (k <= 5) newfeat = FEAT_BUSH;

            else if (k <= 15) newfeat = FEAT_THORNS;

            else if (k <= 20) newfeat = FEAT_BRAMBLES;

            else if (k <= 21) newfeat = FEAT_TREE;

            else if (k <= 25) newfeat = FEAT_FLOOR_PEBBLES;

            else if (k <= 30)
            {
                if (level_flag & (ELEMENT_WATER)) newfeat = FEAT_FLOOR_WATER;
                else newfeat = FEAT_FLOOR_MUD;
            }

            else if (k <= 40) newfeat = FEAT_FLOOR_MUD;

            else if (k <= 70) newfeat = FEAT_FLOOR_EARTH;

            break;
        }

        case FEAT_FLOOR_LAVA:
        {
            if (k <= 10) newfeat = FEAT_WALL_OF_FIRE;

            else if (k <= 15) newfeat = FEAT_SCORCHED_WALL;

            break;
        }

        case FEAT_FLOOR_EARTH:
        {
            if (k <= 5) newfeat = FEAT_FLOOR_ROCK;

            else if (k <= 10) newfeat = FEAT_FLOOR_PEBBLES;

            break;
        }

        case FEAT_FIRE:
        {
            if (k <= 25) newfeat = FEAT_BURNT_SPOT;

            break;
        }

    }

    /* Set the new feature, if there is one */
    if (newfeat != oldfeat) cave_set_feat(y, x, newfeat);

    // Place an effect if called for
    if (effect_rock) set_effect_rocks(effect_rock, y, x);

}


/*
 * Returns TRUE if f_idx is a valid pool feature
 */
static bool cave_feat_pool(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Hack -- Ignore ice pools on non-ice levels */
    if (!(level_flag & LF1_ICE) && _feat_ff3_match(f_ptr, FF3_ICE))
    {
        return (FALSE);
    }

    /* Hack -- Ignore solid features */
    if (!_feat_ff1_match(f_ptr, FF1_MOVE))
    {
        return (FALSE);
    }

    /* All remaining lake features will be fine */
    return (cave_feat_lake(f_idx));
}



/*
 * Choose a terrain feature for the current level.
 * You can use a hook to ensure consistent terrain (lakes/pools).
 * This function handles themed levels as a special case. The "feeling"
 * global variable must be properly set to recognize the themed level. See
 * "build_themed_level".
 */
static u16b pick_proper_feature(bool (*feat_hook)(int f_idx))
{
    /* Default depth for the feature */
    int max_depth = p_ptr->depth;
    u16b feat;

    /* Special case - Themed levels with default features */
    /* Note that we have a small chance to ignore these features */
    if ((feeling >= LEV_THEME_HEAD) && (rand_int(100) < 75))
    {
        /* Get the theme */
        byte theme = feeling - LEV_THEME_HEAD;
        QVector<u16b> features;
        u16b i;

        features.clear();

        /* Find if we have to use default features for this level */
        for (i = 0; i < CUR_NUM_THEME_LEVEL_FEATURES; i++)
        {
            /* Ignore mismatching themes */
            if (theme != themed_level_features[i].theme) continue;

            /* Get the feature */
            feat = themed_level_features[i].feature;

            /* Ignore features who are too deep for the player */
            if (f_info[feat].f_level > p_ptr->depth + 25) continue;

            /* IMPORTANT - Check consistency with the level */
            if (feat_hook && !feat_hook(feat)) continue;

            /* Feature is OK */
            features.append(feat);
        }

        /* Pick a default feature, if any */
        if (features.size()) return (features[randint0(features.size())]);
    }

    /* Special case - Themed levels with random features */
    if (feeling >= LEV_THEME_HEAD)
    {
        /* Note that we have a boost to depth in themed levels */
        max_depth += 7;

        /* Quests have a bigger boost to depth */
        if (quest_check(p_ptr->depth) == QUEST_THEMED_LEVEL)
        {
            max_depth += 10;
        }

        /* Check bounds */
        max_depth = MIN(max_depth, MAX_DEPTH - 1);
    }

    /* Set the given hook, if any */
    get_feat_num_hook = feat_hook;

    get_feat_num_prep();

    /* Pick a feature */
    feat = get_feat_num(max_depth);

    /* Clear the hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    /* Return the feature */
    return (feat);
}


/*
 * Places a line in a fractal map given its start and end points and a certain
 * grid type. To be used in template initialization routines.
 * Note: This is a very basic drawing routine. It works fine with vertical,
 * horizontal and the diagonal lines of a square. It doesn't support other
 * oblique lines well.
 */
static void fractal_draw_line(fractal_map map, fractal_template *t_ptr,
        int y1, int x1, int y2, int x2, byte content)
{
    int dx, dy;

    /* Get the proper increments to reach the end point */
    dy = ((y1 < y2) ? 1: (y1 > y2) ? -1: 0);
    dx = ((x1 < x2) ? 1: (x1 > x2) ? -1: 0);

    /* Draw the line */
    while (TRUE)
    {
        /* Stop at the first illegal grid */
        if (!IN_FRACTAL(t_ptr, y1, x1)) break;

        /* Set the new content of the grid */
        map[y1][x1] = content;

        /* We reached the end point? */
        if ((y1 == y2) && (x1 == x2)) break;

        /* Advance one position */
        y1 += dy;
        x1 += dx;
    }
}


/*
 * Places walls in the perimeter of a fractal map
 */
static void fractal_draw_borders(fractal_map map, fractal_template *t_ptr)
{
    int last = t_ptr->size - 1;

    /* Left */
    fractal_draw_line(map, t_ptr, 0, 0, last, 0, FRACTAL_WALL);
    /* Important: Leave some space for tunnels */
    fractal_draw_line(map, t_ptr, 0, 1, last, 1, FRACTAL_WALL);

    /* Right */
    fractal_draw_line(map, t_ptr, 0, last, last, last, FRACTAL_WALL);
    /* Important: Leave some space for tunnels */
    fractal_draw_line(map, t_ptr, 0, last - 1, last, last - 1, FRACTAL_WALL);

    /* Top */
    fractal_draw_line(map, t_ptr, 0, 1, 0, last - 1, FRACTAL_WALL);

    /* Bottom */
    fractal_draw_line(map, t_ptr, last, 1, last, last - 1, FRACTAL_WALL);
}


/*
 * Some fractal templates
 */

/* 17x33 template */
void fractal1_init_func(fractal_map map, fractal_template *t_ptr)
{
    /* Borders */
    fractal_draw_borders(map, t_ptr);

    /*
     * Mega-hack -- place walls in the middle of the 33x33 map to generate
     * a 17x33 map
     */
    fractal_draw_line(map, t_ptr, 16, 1, 16, 32, FRACTAL_WALL);

    map[8][8] = (one_in_(15) ? FRACTAL_POOL_1: FRACTAL_FLOOR);
    map[8][16] = (one_in_(15) ? FRACTAL_POOL_2: FRACTAL_FLOOR);
    map[8][24] = (one_in_(15) ? FRACTAL_POOL_3: FRACTAL_FLOOR);
}


/* 33x65 template */
void fractal2_init_func(fractal_map map, fractal_template *t_ptr)
{
    int k;

    /* Borders */
    fractal_draw_borders(map, t_ptr);

    /*
     * Mega-hack -- place walls in the middle of the 65x65 map to generate
     * a 33x65 map
     */
    fractal_draw_line(map, t_ptr, 32, 1, 32, 64, FRACTAL_WALL);

    k = rand_int(100);
    /* 1 in 5 chance to make a pool and 1 in 5 to leave the map untouched */
    if (k < 80) map[8][16] = ((k < 20) ? FRACTAL_POOL_2: FRACTAL_FLOOR);

    k = rand_int(100);
    /* 1 in 4 chance to make a pool and 1 in 4 to leave the map untouched */
    if (k < 75) map[8][32] = ((k < 25) ? FRACTAL_POOL_3: FRACTAL_FLOOR);

    k = rand_int(100);
    /* 1 in 5 chance to make a pool and 1 in 5 to leave the map untouched */
    if (k < 80) map[8][48] = ((k < 20) ? FRACTAL_POOL_1: FRACTAL_FLOOR);

    map[16][16] = (one_in_(4) ? FRACTAL_POOL_1: FRACTAL_FLOOR);
    map[16][32] = (one_in_(3) ? FRACTAL_POOL_2: FRACTAL_FLOOR);
    map[16][48] = (one_in_(4) ? FRACTAL_POOL_3: FRACTAL_FLOOR);

    k = rand_int(100);
    /* 1 in 5 chance to make a pool and 1 in 5 to leave the map untouched */
    if (k < 80) map[24][16] = ((k < 20) ? FRACTAL_POOL_3: FRACTAL_FLOOR);

    k = rand_int(100);
    /* 1 in 4 chance to make a pool and 1 in 4 to leave the map untouched */
    if (k < 75) map[24][32] = ((k < 25) ? FRACTAL_POOL_1: FRACTAL_FLOOR);

    k = rand_int(100);
    /* 1 in 5 chance to make a pool and 1 in 5 to leave the map untouched */
    if (k < 80) map[24][48] = ((k < 20) ? FRACTAL_POOL_2: FRACTAL_FLOOR);
}


/* 9x9 template for pools */
void fractal3_init_func(fractal_map map, fractal_template *t_ptr)
{
    /*Unused*/
    (void)t_ptr;

    /* Walls in the corners */
    map[0][0] = FRACTAL_WALL;
    map[0][8] = FRACTAL_WALL;
    map[8][0] = FRACTAL_WALL;
    map[8][8] = FRACTAL_WALL;

    map[2][4] = FRACTAL_FLOOR;
    map[4][2] = FRACTAL_FLOOR;
    map[4][4] = FRACTAL_FLOOR;
    map[4][6] = FRACTAL_FLOOR;
    map[6][4] = FRACTAL_FLOOR;
}


/* 17x17 template for pools */
void fractal4_init_func(fractal_map map, fractal_template *t_ptr)
{
    /*Unused*/
    (void)t_ptr;

    /* Walls in the corners */
    map[0][0] = FRACTAL_WALL;
    map[0][16] = FRACTAL_WALL;
    map[16][0] = FRACTAL_WALL;
    map[16][16] = FRACTAL_WALL;

    map[4][8] = FRACTAL_FLOOR;
    map[8][4] = FRACTAL_FLOOR;
    map[8][8] = FRACTAL_FLOOR;
    map[8][12] = FRACTAL_FLOOR;
    map[12][8] = FRACTAL_FLOOR;
}


/* 33x33 template */
void fractal5_init_func(fractal_map map, fractal_template *t_ptr)
{
    bool flip_h = one_in_(2);

    /* Borders */
    fractal_draw_borders(map, t_ptr);

    if (one_in_(15)) map[8][flip_h ? 24: 8] = FRACTAL_FLOOR;

    map[16][16] = FRACTAL_FLOOR;

    if (one_in_(15)) map[24][flip_h ? 8: 24] = FRACTAL_FLOOR;
}


/*
 * Wipes the contents of a fractal map and applies the given template.
 */
static void fractal_map_reset(fractal_map map, fractal_template *t_ptr)
{
    int x, y;

    /* Fill the map with FRACTAL_NONE */
    for (y = 0; y < t_ptr->size; y++)
    {
        for (x = 0; x < t_ptr->size; x++)
        {
            map[y][x] = FRACTAL_NONE;
        }
    }

    /* Call the initialization function to place some floors */
    if (t_ptr->init_func)
    {
        t_ptr->init_func(map, t_ptr);
    }
}


/*
 * Returns a *reset* fractal map allocated in dynamic memory.
 * You must deallocate the map with FREE when it isn't used anymore.
 */
fractal_map fractal_map_create(fractal_template *t_ptr)
{
    /* The new map */
    fractal_map map;

    /* Allocate the map */
    map = C_ZNEW(t_ptr->size, fractal_map_wid);

    /* Reset the contents of the map */
    fractal_map_reset(map, t_ptr);

    /* Done */
    return (map);
}




/*
 * Completes a fractal map. The map must have been reset.
 */
void fractal_map_complete(fractal_map map, fractal_template *t_ptr)
{
    int x, y, x1, y1, x2, y2, cx, cy;
    /*
     * Set the initial size of the squares. At first, we have only
     * one big square.
     */
    int cur_size = t_ptr->size - 1;

    /*
     * Construct the map using a variable number of iterations.
     * Each iteration adds more details to the map.
     * This algorithm is originally recursive but we made it iterative
     * for efficiency.
     */
    do
    {
        /* Get the vertical coordinates of the first square */
        y1 = 0;
        y2 = cur_size;

        /*
         * Process the whole map. Notice the step used: (cur_size / 2)
         * is the middle point of the current "3x3" square.
         */
        for (y = 0; y < t_ptr->size; y += (cur_size / 2))
        {
            /* Change to the next "3x3" square, if needed */
            if (y > y2)
            {
                /*
                 * The end of the previous square becomes the
                 * beginning of the new square
                 */
                y1 = y2;

                /* Get the end of the new square */
                y2 += cur_size;
            }

            /* Get the horizontal coordinates of the first square */
            x1 = 0;
            x2 = cur_size;

            /* Notice the step */
            for (x = 0; x < t_ptr->size; x += (cur_size / 2))
            {
                /* Change to the next "3x3" square, if needed */
                if (x > x2)
                {
                    /*
                     * The end of the previous square
                     * becomes the beginning of the new
                     * square
                     */
                    x1 = x2;

                    /* Get the end of the new square */
                    x2 += cur_size;
                }

                /* IMPORTANT: ignore already processed grids */
                if (map[y][x] != FRACTAL_NONE) continue;

                /*
                 * Determine if the vertical coordinate of
                 * this grid should be fixed
                 */
                if ((y == y1) || (y == y2)) cy = y;
                /* Pick one *adjacent* corner randomly */
                else cy = ((rand_int(100) < 50) ? y1: y2);

                /*
                 * Determine if the horizontal coordinate of
                 * this grid should be fixed
                 */
                if ((x == x1) || (x == x2)) cx = x;
                /* Pick one *adjacent* corner randomly */
                else cx = ((rand_int(100) < 50) ? x1: x2);

                /* Copy the value of the chosed corner */
                map[y][x] = map[cy][cx];
            }
        }

    /* Decrease the size of the squares for the next iteration */
    cur_size /= 2;

    /* We stop when the squares can't be divided anymore */
    } while (cur_size > 1);
}


/*
 * Verify if all floor grids in a completed fractal map are connected.
 */
static int fractal_map_is_connected(fractal_map map, fractal_template *t_ptr)
{
    int x, y, i, connected = TRUE;
    fractal_map_wid *visited;
    /* Queue of visited grids */
    QVector<coord> grid_queue;
    grid_queue.clear();

    /* Allocate a "visited" matrix */
    visited = C_ZNEW(t_ptr->size, fractal_map_wid);

    /* Find a floor grid */
    for (y = 0; (y < t_ptr->size) && !grid_queue.size(); y++)
    {
        for (x = 0; (x < t_ptr->size) && !grid_queue.size(); x++)
        {
            /* Found one */
            if (map[y][x] >= FRACTAL_FLOOR)
            {
                /* Put it on the queue */
                grid_queue.append(make_coords(y, x));

                /* Mark as visited */
                visited[y][x] = TRUE;
            }
        }
    }

    /* Paranoia. No floor grid was found */
    if (!grid_queue.size())
    {
        /* Done */
        return (!connected);
    }

    /* Process all reachable floor grids */
    while (grid_queue.size())
    {
        /* Get the coordinates of the grid in the head of the queue */
        y = grid_queue.at(0).y;
        x = grid_queue.at(0).x;

        /* Remove that grid from the queue */
        grid_queue.removeFirst();

        /* Scan all adjacent grids */
        for (i = 0; i < 8; i++)
        {
            /* Get coordinates */
            int yy = y + ddy_ddd[i];
            int xx = x + ddx_ddd[i];

            /* Check bounds */
            if (!IN_FRACTAL(t_ptr, yy, xx)) continue;

            /* Ignore already processed grids */
            if (visited[yy][xx]) continue;

            /* Ignore walls */
            if (map[yy][xx] < FRACTAL_FLOOR) continue;

            /* Append the grid to the queue */
            grid_queue.append(make_coords(yy, xx));

            /* Mark as visited */
            visited[yy][xx] = TRUE;
        }
    }

    /* Find non-visited floor grids */
    for (y = 0; (y < t_ptr->size) && connected; y++)
    {
        for (x = 0; (x < t_ptr->size) && connected; x++)
        {
            /* Check the grid */
            if ((map[y][x] >= FRACTAL_FLOOR) && !visited[y][x])
            {
                /* Found a non-visited floor grid. Done */
                connected = FALSE;
            }
        }
    }

    /* Free resources */
    FREE_ARRAY(visited);

    /* Return answer */
    return connected;
}


/*
 * Places FRACTAL_EDGE walls in a completed fractal map. These grids were
 * created to be replaced by outer walls or other similar features.
 */
static void fractal_map_mark_edge(fractal_map map, fractal_template *t_ptr)
{
    int x, y, i;

    /* Process the whole map */
    for (y = 0; y < t_ptr->size; y++)
    {
        for (x = 0; x < t_ptr->size; x++)
        {
            /* Ignore wall grids */
            if (map[y][x] < FRACTAL_FLOOR) continue;

            /* Scan adjacent grids */
            for (i = 0; i < 8; i++)
            {
                /* Get coordinates */
                int yy = y + ddx_ddd[i];
                int xx = x + ddy_ddd[i];

                /* Check bounds */
                if (!IN_FRACTAL(t_ptr, yy, xx)) continue;

                /* Turn plain walls to edge walls */
                if (map[yy][xx] == FRACTAL_WALL)
                {
                    map[yy][xx] = FRACTAL_EDGE;
                }
            }
        }
    }
}




/*
 * Build a fractal formation in the current level using the given feature.
 * y0 and x0 are the center of the formation, if they are out of bounds another point
 * is picked randomly
 * fractal_type tells us the dimensions of the formation
 * Chance is the probability (in 100) to use feat2 for a grid.
 * If chance is 0 feat2 is ignored
 * If feat2 is FEAT_NONE the grid remains untouched
 * if mode is 0 the inner part of the fractal is used
 * if mode is 1 the edge of the fractal is used
 */
void build_formation(int y0, int x0, u16b feat, byte fractal_type, int chance, u16b feat2, byte mode)
{
    int y, x;
    int hgt, wid;
    int tries;

    fractal_template *t_ptr;
    fractal_map map;

    tries = 0;

    /* Pick a fractal template */
    while (TRUE)
    {
        /* Pick one template randomly */
        t_ptr = &fractal_repository[rand_int(SIZE_FRACTAL_REPOSITORY)];

        /* It is of the proper type */
        if (t_ptr->type == fractal_type) break;

        /* Failure */
        if (++tries > 100) return;
    }

    /* Initialize the map */
    map = fractal_map_create(t_ptr);

    /* Create fractal */
    fractal_map_complete(map, t_ptr);

    /* Mark edges if necessary */
    if (mode == 1) fractal_map_mark_edge(map, t_ptr);

    /* Get dimensiones */
    hgt = fractal_dim[fractal_type].hgt;
    wid = fractal_dim[fractal_type].wid;

    /* Pick a random center point if necessary */
    while (!in_bounds(y0, x0))
    {
        y0 = randint(p_ptr->cur_map_hgt - 1);
        x0 = randint(p_ptr->cur_map_wid - 1);
    }

    /* Get top-left corner */
    y0 -= hgt / 2;
    x0 -= wid / 2;

    /* Apply the fractal to the dungeon */
    for (y = 0; y < hgt; y++)
    {
        /* Get real row */
        int yy = y0 + y;

        for (x = 0; x < wid; x++)
        {
            /* Get real column */
            int xx = x0 + x;

            /* Ignore annyoing grids */
            if (!in_bounds(yy, xx)) continue;

            /* Verify if we have to modify the grid. It depends on mode */
            if (mode == 0)
            {
                if (map[y][x] < FRACTAL_FLOOR) continue;
            }
            else
            {
                if (map[y][x] != FRACTAL_EDGE) continue;
            }

            /* See if we have to replace feat with feat2 */
            if ((chance > 0) && (rand_int(100) < chance))
            {
                if (feat2 != FEAT_NONE) cave_set_feat(yy, xx, feat2);
            }
            /* Common case */
            else
            {
                cave_set_feat(yy ,xx, feat);
            }
        }
    }

    /* Free resources */
    FREE_ARRAY(map);
}


/*
 * Construct a fractal room given a fractal map and the room center's coordinates.
 */
static void fractal_map_to_room(fractal_map map, byte fractal_type, int y0, int x0)
{
    int x, y, y1, x1, wid, hgt;
    bool light = FALSE;
    int floor_type;
    /*
     * No pools for now. Note that we choose the features for the pool when
     * we need them. If we pick three random features right now we might
     * generate inconsistent levels.
     */
    u16b pool1 = FEAT_NONE;
    u16b pool2 = FEAT_NONE;
    u16b pool3 = FEAT_NONE;

    /* Get the dimensions of the fractal map */
    hgt = fractal_dim[fractal_type].hgt;
    wid = fractal_dim[fractal_type].wid;

    /* Get top-left coordinate */
    y1 = y0 - hgt / 2;
    x1 = x0 - wid / 2;

    /* Occasional light */
    if (p_ptr->depth <= randint(25)) light = TRUE;

    /* Use earth floor sometimes. EXPERIMENTAL */
    floor_type = rand_int(100);

    /* Apply the map to the dungeon */
    for (y = 0; y < hgt; y++)
    {
        for (x = 0; x < wid; x++)
        {
            byte grid_type = map[y][x];
            /* Translate to dungeon coordinates */
            int yy = y1 + y;
            int xx = x1 + x;

            /* Ignore annoying locations */
            if (!in_bounds_fully(yy, xx)) continue;

            /* Translate each grid type to dungeon features */
            if (grid_type >= FRACTAL_FLOOR)
            {
                u16b feat = FEAT_NONE;

                /* Pool grid */
                if (grid_type == FRACTAL_POOL_1)
                {
                    /* Pick a feature if necessary */
                    if (pool1 == FEAT_NONE)
                    {
                        pool1 = pick_proper_feature(cave_feat_pool);
                    }

                    /* Use the pool feature */
                    feat = pool1;
                }
                /* Pool grid */
                else if (grid_type == FRACTAL_POOL_2)
                {
                    /* Pick a feature if necessary */
                    if (pool2 == FEAT_NONE)
                    {
                        pool2 = pick_proper_feature(cave_feat_pool);
                    }

                    /* Use the pool feature */
                    feat = pool2;
                }
                /* Pool grid */
                else if (grid_type == FRACTAL_POOL_3)
                {
                    /* Pick a feature if necessary */
                    if (pool3 == FEAT_NONE)
                    {
                        pool3 = pick_proper_feature(cave_feat_pool);
                    }

                    /* Use the pool feature */
                    feat = pool3;
                }

                /* Place the selected pool feature */
                if (feat != FEAT_NONE)
                {
                    build_terrain(yy, xx, feat);
                }
                /* Or place a floor */
                else
                {
                    /* Use earth floor (15%) */
                    if (floor_type < 15)
                    {
                        feat = FEAT_FLOOR_EARTH;
                    }
                    /* Use scattered earth floor (5%) */
                    else if ((floor_type < 20) && one_in_(7))
                    {
                        feat = FEAT_FLOOR_EARTH;
                    }
                    /* Plain old floor (80%) */
                    else
                    {
                        feat = FEAT_FLOOR;
                    }

                    /* Place floors */
                    cave_set_feat(yy, xx, feat);
                }
            }
            else if (grid_type == FRACTAL_EDGE)
            {
                /* Place ice walls on ice levels */
                if (level_flag & LF1_ICE)
                {
                    build_terrain(yy, xx, FEAT_WALL_ICE);
                }
                /* Place usual walls on other levels */
                else
                {
                    cave_set_feat(yy, xx, FEAT_WALL_GRANITE);
                }
            }
            else
            {
                continue;
            }

            /* Mark the grid as a part of the room */
            dungeon_info[yy][xx].cave_info |= (CAVE_ROOM);

            /* Light the feature if needed */
            if (light)
            {
                dungeon_info[yy][xx].cave_info |= (CAVE_GLOW);
            }

            /* Or turn off the lights */
            else if (grid_type != FRACTAL_EDGE)
            {
                dungeon_info[yy][xx].cave_info &= ~(CAVE_GLOW);
            }
        }
    }
}


/*
 * Creates a fractal map given a template and copy part of it in the given map
 */
static void fractal_map_merge_another(fractal_map map, fractal_template *t_ptr)
{
    int y, x;

    fractal_map map2;

    /* Create the map */
    map2 = fractal_map_create(t_ptr);

    /* Complete it */
    fractal_map_complete(map2, t_ptr);

    /* Merge the maps */
    for (y = 0; y < t_ptr->size; y++)
    {
        for (x = 0; x < t_ptr->size; x++)
        {
            /* Sometimes we overwrite a grid in the original map */
            if ((map[y][x] != map2[y][x]) && one_in_(4)) map[y][x] = map2[y][x];
        }
    }

    /* Free resources */
    FREE_ARRAY(map2);
}


/*
 * Build a fractal room given its center. Returns TRUE on success.
 */
bool build_type_fractal(int y0, int x0, byte type)
{
    fractal_map map;
    fractal_template *t_ptr;
    int tries;
    bool do_merge = FALSE;

    /* Paranoia */
    if (type >= MAX_FRACTAL_TYPES) return (FALSE);

    /* Reset the loop counter */
    tries = 0;

    /* Get a fractal template */
    while (TRUE)
    {
        /* Get a template */
        int which = rand_int(SIZE_FRACTAL_REPOSITORY);

        t_ptr = &fractal_repository[which];

        /* Check if the type matches the wanted one */
        if (t_ptr->type == type) break;

        /* Avoid infinite loops */
        if (++tries >= 100) return (FALSE);
    }

    /* Create and reset the fractal map */
    map = fractal_map_create(t_ptr);


    /* Make medium fractal rooms more exotic sometimes */
    if ((type == FRACTAL_TYPE_33x33) && !one_in_(3)) do_merge = TRUE;

    /* Reset the loop counter */
    tries = 0;

    /* Construct the fractal map */
    while (TRUE)
    {
        /* Complete the map */
        fractal_map_complete(map, t_ptr);

        /* Put another room on top of this one if necessary */
        if (do_merge) fractal_map_merge_another(map, t_ptr);

        /* Accept only connected maps */
        if (fractal_map_is_connected(map, t_ptr)) break;

        /* Avoid infinite loops */
        if (++tries >= 100)
        {
            /* Free resources */
            FREE_ARRAY(map);

            /* Failure */
            return (FALSE);
        }

        /* Reset the map. Try again */
        fractal_map_reset(map, t_ptr);
    }

    /* Get edge information */
    fractal_map_mark_edge(map, t_ptr);

    /* Place the room */
    fractal_map_to_room(map, type, y0, x0);

    /* Free resources */
    FREE_ARRAY(map);

    /* Success */
    return (TRUE);
}


/*
 * Build a pool in a room given the center of the pool and a feature.
 * Outer and solid walls, and permanent features are unnafected.
 * Returns TRUE on success.
 */
static bool build_pool(int y0, int x0, int feat, bool do_big_pool)
{
    byte type;
    int wid, hgt;
    int x, y, x1, y1;
    fractal_map map;
    fractal_template *t_ptr;

    /* Paranoia */
    if (!feat) return (FALSE);

    /* Set some basic info */
    if (do_big_pool)
    {
        type = FRACTAL_TYPE_17x17;
    }
    else
    {
        type = FRACTAL_TYPE_9x9;
    }

    /* Get the dimensions of the fractal map */
    hgt = fractal_dim[type].hgt;
    wid = fractal_dim[type].wid;

    /* Get the top-left grid of the pool */
    y1 = y0 - hgt / 2;
    x1 = x0 - wid / 2;

    /* Choose a template for the pool */
    while (TRUE)
    {
        /* Pick a random template */
        int which = rand_int(SIZE_FRACTAL_REPOSITORY);

        t_ptr = &fractal_repository[which];

        /* Found the desired template type? */
        if (t_ptr->type == type) break;
    }

    /* Create and reset the fractal map */
    map = fractal_map_create(t_ptr);

    /* Complete the map */
    fractal_map_complete(map, t_ptr);

    /* Copy the map into the dungeon */
    for (y = 0; y < hgt; y++)
    {
        for (x = 0; x < wid; x++)
        {
            /* Translate map coordinates to dungeon coordinates */
            int yy = y1 + y;
            int xx = x1 + x;

            /* Ignore non-floors grid types in the map */
            if (map[y][x] != FRACTAL_FLOOR) continue;

            /* Ignore annoying locations */
            if (!in_bounds_fully(yy, xx)) continue;

            /* A pool must be inside a room */
            if (!(dungeon_info[yy][xx].cave_info & (CAVE_ROOM))) continue;

            /* Ignore anti-teleport grids */
            if (dungeon_info[yy][xx].cave_info & (CAVE_ICKY)) continue;

            /* Ignore some walls, and permanent features */
            if (cave_ff1_match(yy, xx, FF1_OUTER |
                FF1_SOLID | FF1_PERMANENT)) continue;

            /* Set the feature */
            build_terrain(yy, xx, feat);
        }
    }

    /* Free resources */
    FREE_ARRAY(map);

    /* Success */
    return (TRUE);
}


/*
 * Mark a starburst shape in the dungeon with the CAVE_TEMP flag, given the
 * coordinates of a section of the dungeon in "box" format. -LM-, -DG-
 *
 * Starburst are made in three steps:
 * 1: Choose a box size-dependant number of arcs.  Large starburts need to
 *    look less granular and alter their shape more often, so they need
 *    more arcs.
 * 2: For each of the arcs, calculate the portion of the full circle it
 *    includes, and its maximum effect range (how far in that direction
 *    we can change features in).  This depends on starburst size, shape, and
 *    the maximum effect range of the previous arc.
 * 3: Use the table "get_angle_to_grid" to supply angles to each grid in
 *    the room.  If the distance to that grid is not greater than the
 *    maximum effect range that applies at that angle, change the feature
 *    if appropriate (this depends on feature type).
 *
 * Usage notes:
 * - This function uses a table that cannot handle distances larger than
 *   20, so it calculates a distance conversion factor for larger starbursts.
 * - This function is not good at handling starbursts much longer along one axis
 *   than the other.
 * This function doesn't mark any grid in the perimeter of the given box.
 *
 */
static bool mark_starburst_shape(int y1, int x1, int y2, int x2, u32b flag)
{
    int y0, x0, y, x, ny, nx;
    int i;
    int size;
    int dist, max_dist, dist_conv, dist_check;
    int height, width, arc_dist;
    int degree_first, center_of_arc, degree;

    /* Special variant starburst.  Discovered by accident. */
    bool make_cloverleaf = FALSE;

    /* Holds first degree of arc, maximum effect distance in arc. */
    int arc[45][2];

    /* Number (max 45) of arcs. */
    int arc_num;

    /* Make certain the starburst does not cross the dungeon edge. */
    if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);

    /* Robustness -- test sanity of input coordinates. */
    if ((y1 + 2 >= y2) || (x1 + 2 >= x2)) return (FALSE);

    /* Get room height and width. */
    height = 1 + y2 - y1;
    width  = 1 + x2 - x1;

    /* Note the "size" */
    size = 2 + div_round(width + height, 22);

    /* Get a shrinkage ratio for large starbursts, as table is limited. */
    if ((width > 40) || (height > 40))
    {
        if (width > height) dist_conv = 1 + (10 * width  / 40);
        else                dist_conv = 1 + (10 * height / 40);
    }
    else dist_conv = 10;

    /* Make a cloverleaf starburst sometimes.  (discovered by accident) */
    if ((flag & (STAR_BURST_CLOVER)) && (height > 10) && (one_in_(20)))
    {
        arc_num = 12;
        make_cloverleaf = TRUE;
    }

    /* Usually, we make a normal starburst. */
    else
    {
        /* Ask for a reasonable number of arcs. */
        arc_num = 8 + (height * width / 80);
        arc_num = rand_spread(arc_num, 3);
        if (arc_num < 8) arc_num = 8;
        if (arc_num > 45) arc_num = 45;
    }

    /* Get the center of the starburst. */
    y0 = y1 + height / 2;
    x0 = x1 + width  / 2;

    /* Start out at zero degrees. */
    degree_first = 0;

    /* Determine the start degrees and expansion distance for each arc. */
    for (i = 0; i < arc_num; i++)
    {
        /* Get the first degree for this arc (using 180-degree circles). */
        arc[i][0] = degree_first;

        /* Get a slightly randomized start degree for the next arc. */
        degree_first += div_round(180, arc_num);

        /* Do not entirely leave the usual range */
        if (degree_first < 180 * (i+1) / arc_num)
        {
            degree_first = 180 * (i+1) / arc_num;
        }
        if (degree_first > (180 + arc_num) * (i+1) / arc_num)
        {
            degree_first = (180 + arc_num) * (i+1) / arc_num;
        }

        /* Get the center of the arc (convert from 180 to 360 circle). */
        center_of_arc = degree_first + arc[i][0];

        /* Get arc distance from the horizontal (0 and 180 degrees) */
        if      (center_of_arc <=  90) arc_dist = center_of_arc;
        else if (center_of_arc >= 270) arc_dist = ABS(center_of_arc - 360);
        else                           arc_dist = ABS(center_of_arc - 180);

        /* Special case -- Handle cloverleafs */
        if ((arc_dist == 45) && (make_cloverleaf)) dist = 0;

        /*
         * Usual case -- Calculate distance to expand outwards.  Pay more
         * attention to width near the horizontal, more attention to height
         * near the vertical.
         */
        else dist = ((height * arc_dist) + (width * (90 - arc_dist))) / 90;

        /* Randomize distance (should never be greater than radius) */
        arc[i][1] = rand_range(dist / 4, dist / 2);

        /* Keep variability under control (except in special cases). */
        if ((dist != 0) && (i != 0))
        {
            int diff = arc[i][1] - arc[i-1][1];

            if (ABS(diff) > size)
            {
                if (diff > 0)	arc[i][1] = arc[i-1][1] + size;
                else arc[i][1] = arc[i-1][1] - size;
            }
        }
    }

    /* Neaten up final arc of circle by comparing it to the first. */
    if (TRUE)
    {
        int diff = arc[arc_num - 1][1] - arc[0][1];

        if (ABS(diff) > size)
        {
            if (diff > 0)	arc[arc_num - 1][1] = arc[0][1] + size;
            else arc[arc_num - 1][1] = arc[0][1] - size;
        }
    }

    /* Precalculate check distance. */
    dist_check = 21 * dist_conv / 10;

    /* Change grids between (and not including) the edges. */
    for (y = y1 + 1; y < y2; y++)
    {
        for (x = x1 + 1; x < x2; x++)
        {
            /* Get distance to grid. */
            dist = distance(y0, x0, y, x);

            /* Look at the grid if within check distance. */
            if (dist < dist_check)
            {
                /* Convert and reorient grid for table access. */
                ny = 20 + 10 * (y - y0) / dist_conv;
                nx = 20 + 10 * (x - x0) / dist_conv;

                /* Illegal table access is bad. */
                if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40))  continue;

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
                            /* Mark the grid */
                            dungeon_info[y][x].cave_info |= (CAVE_TEMP);
                        }

                        /* Arc found.  End search */
                        break;
                    }
                }
            }
        }
    }

    return (TRUE);
}


/*
 * Make a starburst room. -LM-, -DG-
 *
 * Usage notes:
 * - This function is not good at handling rooms much longer along one axis
 *   than the other.
 * - It is safe to call this function on areas that might contain vaults or
 *   pits, because "icky" and occupied grids are left untouched.
 */
bool generate_starburst_room(int y1, int x1, int y2, int x2,
    u16b feat, u16b edge, u32b flag)
{
    int y, x, d;

    /* Mark the affected grids */
    if (!mark_starburst_shape(y1, x1, y2, x2, flag)) return (FALSE);

    /* Paranoia */
    if (edge == feat) edge = FEAT_NONE;

    /* Process marked grids */
    for (y = y1 + 1; y < y2; y++)
    {
        for (x = x1 + 1; x < x2; x++)
        {
            /* Marked grids only */
            if (!(dungeon_info[y][x].cave_info & (CAVE_TEMP))) continue;

            /* Do not touch "icky" grids. */
            if (dungeon_info[y][x].cave_info & (CAVE_ICKY)) continue;

            /* Do not touch occupied grids. */
            if (dungeon_info[y][x].monster_idx != 0) continue;
            if (dungeon_info[y][x].object_idx != 0) continue;

            /* Illuminate if requested */
            if (flag & (STAR_BURST_LIGHT))
            {
                dungeon_info[y][x].cave_info |= (CAVE_GLOW);
            }
            /* Or turn off the lights */
            else
            {
                dungeon_info[y][x].cave_info &= ~(CAVE_GLOW);
            }

            /* Floor overwrites the dungeon */
            if (flag & (STAR_BURST_RAW_FLOOR))
            {
                cave_set_feat(y, x, feat);
            }
            /* Floor is merged with the dungeon */
            else
            {
                build_terrain(y, x, feat);
            }

            /* Make part of a room if requested */
            if (flag & (STAR_BURST_ROOM))
            {
                dungeon_info[y][x].cave_info |= (CAVE_ROOM);
            }

            /* Special case. No edge feature */
            if (edge == FEAT_NONE)
            {
                /*
                 * We lite the outside grids anyway, to
                 * avoid lakes surrounded with blackness.
                 * We only do this if the lake is lit.
                 */
                if (!(flag & (STAR_BURST_LIGHT |
                    STAR_BURST_ROOM))) continue;

                /* Look in all directions. */
                for (d = 0; d < 8; d++)
                {
                    /* Extract adjacent location */
                    int yy = y + ddy_ddd[d];
                    int xx = x + ddx_ddd[d];

                    /* Ignore annoying locations */
                    if (!in_bounds_fully(yy, xx)) continue;

                    /* Already processed */
                    if (dungeon_info[yy][xx].cave_info & (CAVE_TEMP)) continue;

                    /* Lite the feature */
                    if (flag & (STAR_BURST_LIGHT))
                    {
                        dungeon_info[yy][xx].cave_info |= (CAVE_GLOW);
                    }

                    /* Make part of the room */
                    if (flag & (STAR_BURST_ROOM))
                    {
                        dungeon_info[yy][xx].cave_info |= (CAVE_ROOM);
                    }
                }

                /* Done */
                continue;
            }

            /* Common case. We have an edge feature */

            /* Look in all directions. */
            for (d = 0; d < 8; d++)
            {
                /* Extract adjacent location */
                int yy = y + ddy_ddd[d];
                int xx = x + ddx_ddd[d];

                /* Ignore annoying locations */
                if (!in_bounds_fully(yy, xx)) continue;

                /* Already processed */
                if (dungeon_info[yy][xx].cave_info & (CAVE_TEMP)) continue;

                /* Do not touch "icky" grids. */
                if (dungeon_info[yy][xx].cave_info & (CAVE_ICKY)) continue;

                /* Do not touch occupied grids. */
                if (dungeon_info[yy][xx].monster_idx != 0) continue;
                if (dungeon_info[yy][xx].object_idx != 0) continue;

                /* Illuminate if requested. */
                if (flag & (STAR_BURST_LIGHT))
                {
                    dungeon_info[yy][xx].cave_info |= (CAVE_GLOW);
                }

                /* Edge overwrites the dungeon */
                if (flag & (STAR_BURST_RAW_EDGE))
                {
                    cave_set_feat(yy, xx, edge);
                }
                /* Edge is merged with the dungeon */
                else
                {
                    build_terrain(yy, xx, edge);
                }

                /* Make part of a room if requested */
                if (flag & (STAR_BURST_ROOM))
                {
                    dungeon_info[yy][xx].cave_info |= (CAVE_ROOM);
                }
            }
        }
    }

    /* Clear the mark */
    for (y = y1 + 1; y < y2; y++)
    {
        for (x = x1 + 1; x < x2; x++)
        {
            dungeon_info[y][x].cave_info &= ~(CAVE_TEMP);
        }
    }

    /* Success */
    return (TRUE);
}


void build_type_starburst(int y0, int x0, bool giant_room)
{
    bool want_pools = (rand_int(150) < p_ptr->depth);
    /* Default floor and edge */
    u16b feat = FEAT_FLOOR;
    u16b edge = FEAT_WALL_GRANITE;
    /* Default flags, classic rooms */
    u32b flag = (STAR_BURST_ROOM | STAR_BURST_RAW_FLOOR |
        STAR_BURST_RAW_EDGE);

    int dy, dx;

    /*
     * Hack - get the size of the room, could be large or very large.
     */

    /* 66x44 */
    if (giant_room)
    {
        dy = 19;
        dx = 30;
    }
    /* 33x22 */
    else
    {
        dy = 9;
        dx = 14;
    }

    /* We have a second chance to build pools in themed levels */
    if (!want_pools && (feeling >= LEV_THEME_HEAD))
    {
        want_pools = (rand_int(90) < p_ptr->depth);
    }

    /* Occasional light */
    if (p_ptr->depth <= randint(25)) flag |= (STAR_BURST_LIGHT);

    /* Frozen edge on ice levels */
    if (level_flag & (LF1_ICE))
    {
        edge = FEAT_WALL_ICE;

        /* Make ice walls interesting */
        flag &= ~(STAR_BURST_RAW_EDGE);
    }

    /* Case 1. Plain starburst room */
    if (rand_int(100) < 75)
    {
        /* Allow cloverleaf rooms if pools are disabled */
        if (!want_pools) flag |= (STAR_BURST_CLOVER);

        generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
            feat, edge, flag);
    }
    /* Case 2. Add an inner room */
    else
    {
        /* Note no cloverleaf room */
        generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
            feat, edge, flag);

        /* Special case. Create a solid wall formation */
        if (one_in_(2))
        {
            /* Classic rooms */
            if (edge == FEAT_WALL_GRANITE)
            {
                feat = FEAT_WALL_GRANITE_INNER;
            }

            /* Ice wall formation */
            else
            {
                feat = edge;

                /* Make ice walls interesting */
                flag &= ~(STAR_BURST_RAW_FLOOR);
            }

            /* No edge */
            edge = FEAT_NONE;
        }

        /* Adjust the size of the inner room */
        if (feat_ff1_match(edge, FF1_WALL))
        {
            dy /= 4;
            dx /= 4;
        }
        else
        {
            dy /= 3;
            dx /= 3;
        }

        generate_starburst_room (y0 - dy, x0 - dx, y0 + dy, x0 + dx,
            feat, edge, flag);
    }

    /* Build pools */
    if (want_pools)
    {
        int i, n_pools, range;

        /* Randomize the number of pools */
        n_pools = randint(2);

        /* Adjust for giant rooms */
        if (giant_room) n_pools += 1;

        /* How far of room center? */
        range = giant_room ? 12: 5;

        /* Force the selection of a new feature */
        feat = FEAT_NONE;

        /* Place the pools */
        for (i = 0; i < n_pools; i++)
        {
            int tries;

            /* Pick a new feature */
            if (!feat || one_in_(4))
            {
                /* Choose a feature for the pool */
                feat = pick_proper_feature(cave_feat_pool);

                /* Got none */
                if (!feat) continue;
            }

            for (tries = 0; tries < 2500; tries++)
            {
                /* Get the center of the pool */
                int y = rand_spread(y0, range);
                int x = rand_spread(x0, range);

                /* Verify center */
                if (dungeon_info[y][x].feature_idx == FEAT_FLOOR)
                {
                    build_pool(y, x, feat, giant_room);

                    /* Done */
                    break;
                }
            }
        }
    }
}




/*
 * Build a fog formation on the dungeon
 */
static void build_fog(void)
{
    int x, y, x1, y1;
    int wid, hgt;
    int tries = 0;
    fractal_template *t_ptr;
    fractal_map map;
    /* Check room grids */
    bool want_room = (*dun_cap->can_place_fog_in_rooms)();
    bool locate_near_source = one_in_(2);
    byte type = (one_in_(5) ? FRACTAL_TYPE_33x33: FRACTAL_TYPE_17x17);

    /* Pick a location */
    while (TRUE)
    {
        bool is_near_source = FALSE;
        bool is_room;

        /* Too many failed attempts. Give up */
        if (++tries > 2500) return;

        /* Pick a random spot */
        y = rand_int(p_ptr->cur_map_hgt);
        x = rand_int(p_ptr->cur_map_wid);

        /* Must be passable */
        if (!cave_ff1_match(y, x, FF1_MOVE)) continue;

        /* Check room grids */
        is_room = ((dungeon_info[y][x].cave_info & (CAVE_ROOM)) != 0);

        /* Accept/reject room grids */
        if (want_room != is_room) continue;

        /* There are other restrictions to the center grid? */
        if (!locate_near_source) break;

        /* Yes. Place fog near water or ice grids */
        for (y1 = y - FOG_SRC_RAD; y1 <= y + FOG_SRC_RAD; y1++)
        {
            for (x1 = x - FOG_SRC_RAD; x1 <= x + FOG_SRC_RAD; x1++)
            {
                /* Ignore annoying locations */
                if (!in_bounds(y1, x1)) continue;

                /* Water/ice? */
                if (cave_ff3_match(y1, x1, FF3_WATER | FF3_ICE))
                {
                    /* Done */
                    is_near_source = TRUE;

                    break;
                }
            }

            /* Done? */
            if (is_near_source) break;
        }

        /* Done? */
        if (is_near_source) break;
    }

    /* Pick a fractal template */
    while (TRUE)
    {
        /* Pick any template index */
        int which = rand_int(SIZE_FRACTAL_REPOSITORY);

        /* Get the template */
        t_ptr = &fractal_repository[which];

        /* The size matches the desired type? */
        if (t_ptr->type == type) break;
    }

    /* Create and initialize a fractal map */
    map = fractal_map_create(t_ptr);

    /* Complete the fractal map */
    fractal_map_complete(map, t_ptr);

    /* Get the map size */
    hgt = fractal_dim[type].hgt;
    wid = fractal_dim[type].wid;

    /* Get the top-left corner */
    y1 = y - hgt / 2;
    x1 = x - wid / 2;

    /* Build the fog formation based on the fractal map */
    for (y = 0; y < hgt; y++)
    {
        for (x = 0; x < wid; x++)
        {
            /* Get dungeon coordinates */
            int yy = y1 + y;
            int xx = x1 + x;

            /* The map doesn't specifies a floor grid */
            if (map[y][x] < FRACTAL_FLOOR) continue;

            /* Ignore annoying locations */
            if (!in_bounds(yy, xx)) continue;

            /* Must be passable */
            if (!cave_ff1_match(yy, xx, FF1_MOVE)) continue;

            /* Forbid fog over open doors and stairs */
            if (cave_ff1_match(yy, xx, FF1_DOOR | FF1_STAIRS)) continue;

            /* Create the fog */
            set_effect_permanent_cloud(FEAT_EFFECT_FOG, yy, xx, 0, 0);
        }
    }

    /* Free resources */
    FREE_ARRAY(map);
}


/*
 * Pick a location for the center of a dungeon transformation (region, wall, etc.)
 * The location is stored in py and px.
 * flag must the LF1_* flag of the transformation. It can be 0.
 * marked_grids and num_marked_grids contain the array of grids marked with CAVE_TEMP.
 * These marked grids are the possible candidates for transformation centers.
 * Return TRUE on success, FALSE on failure.
 */
static bool pick_transform_center(QVector<coord> marked_grids, u32b flag, int *py, int *px)
{
    QVector<coord> matching_grids;
    matching_grids.clear();
    int i, j, k;
    int x = 0;
    int y = 0;
    bool found = FALSE;
    int rad = MAX_SIGHT * 2;

    /* First, find a random grid of the given element in the dungeon */
    if (flag && level_flag)
    {
        /* Scan the dungeon */
        for (y = 0; y < p_ptr->cur_map_hgt; y++)
        {
            for (x = 0; x < p_ptr->cur_map_wid; x++)
            {
                /* Get the feature */
                u16b feat = dungeon_info[y][x].feature_idx;

                /* It must be an elemental feature */
                if (!feat_ff3_match(feat, TERRAIN_MASK)) continue;

                /* It must match the given flag */
                if (get_level_flag(feat) != flag) continue;

                matching_grids.append(make_coords(y, x));
            }
        }

        /* Second. Pick a marked grid that is near to a valid elemental grid */
        if (matching_grids.size())
        {
            /* Try several times */
            for (i = 0; (i < 50) && !found; i++)
            {
                /* Pick a random elemental grid */
                k = rand_int(matching_grids.size());

                /* Try several times */
                for (j = 0; (j < 100) && !found; j++)
                {
                    /* Pick a random grid near the elemental grid */
                    y = rand_spread(matching_grids.at(k).y, rad);
                    x = rand_spread(matching_grids.at(k).x, rad);

                    /* Check bounds */
                    if (!in_bounds(y, x)) continue;

                    /* It must be marked */
                    if (dungeon_info[y][x].cave_info & (CAVE_TEMP)) found = TRUE;
                }
            }
        }

        /* Found? */
        if (found)
        {
            /* Return that location */
            *py = y;
            *px = x;
            return (TRUE);
        }
    }

    /* Paranoia */
    if (!marked_grids.size())
    {
        return (FALSE);
    }

    /* Default case. Just put it on some random location */
    for (i = 0; i < 100; i++)
    {
        /* Pick a random index */
        k = rand_int(marked_grids.size());

        /* Get coordinates */
        y = marked_grids.at(k).y;
        x = marked_grids.at(k).x;

        /* Found a marked grid? */
        if (dungeon_info[y][x].cave_info & (CAVE_TEMP))
        {
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
 * Pick and return a random pair of features
 * Return NULL if there isn't a suitable pair
 */
static int feature_selector_select(QVector<feature_selector_item_type> selections)
{
    u16b total_chance = 0;

    for (int i = 0; i < selections.size(); i++)
    {
        total_chance += selections.at(i).chance;
    }

    /* We must have something */
    if (total_chance)
    {
        /* Roll a random chance value */
        int chance = randint0(total_chance);

        int running_chance = 0;

        /* Traverse the array of pairs to see which one contains that value */
        for (int i = 0; i < selections.size(); i++)
        {
            running_chance += selections.at(i).chance;

            /* Found one? */
            if (running_chance > chance)
            {
                /* Return the pair */
                return (i);
            }
        }
    }

    return (-1);
}

/*
 * Transform walls and floors in the dungeon based on the given feature selector.
 * The transformation is fractal shaped and the central point of each fractal is
 * contained in the given array of grids. The grids selected from the array must
 * have assigned the CAVE_TEMP flag too (this enables us to control the location
 * of the transformed regions)
 */
static void transform_regions(QVector<coord> grids, QVector<feature_selector_item_type> selections)
{
    int max = 0, i;
    byte dun_size;
    bool done_big = FALSE;

    /* Paranoia */
    if (!selections.size()) return;

    /* Get dungeon size measure */
    dun_size = ponder_dungeon_size();

    /* Get a number of regions suitable for each size */
    if (dun_size == 1)
    {
        max = 3;
    }
    else if (dun_size == 2)
    {
        max = 5;
    }
    else if (dun_size == 3)
    {
        max = 6;
    }
    /* Medium and large dungeons */
    else
    {
        int k = rand_int(100);

        if (k < 10) max = 10;
        else if (k < 30) max = 9;
        else max = 8;
    }

    /* Message */
    if (cheat_room)
    {
        message(QString("transform_regions: changing %1 region%2.") .arg(max) .arg((max == 1) ? "": "s"));
    }

    /* Transform "max" regions */
    for (i = 0; i < max; i++)
    {
        u16b wall, floor;
        int wid, hgt;
        int y, x, y1, x1;
        fractal_template *t_ptr;
        fractal_map map;
        byte type;
        int tries = 0;
        u32b flags;

        /* Pick a wall feature and an optional floor feature */
        while (TRUE)
        {
            /* Select a feature pair */
            int item = feature_selector_select(selections);

            /* Got one */
            if (item > -1)
            {
                /* Get wall */
                wall = selections.at(item).wall;

                /* Get floor */
                floor = selections.at(item).floor;

                /* Get element flags */
                flags = selections.at(item).level_flag;

                /* Accept feature */
                break;
            }

            /* Can't get a valid pair. Done */
            if (++tries > 50) return;
        }

        /* Pick location */
        if (!pick_transform_center(grids, flags, &y, &x)) return;

        /* Default region size */
        type = FRACTAL_TYPE_33x33;

        /* Try to get a big region */
        if (!done_big && (dun_size >= 4) && one_in_(10))
        {
            type = FRACTAL_TYPE_33x65;

            /* Success */
            done_big = TRUE;
        }

        /* Pick a fractal template */
        while (TRUE)
        {
            /* Pick any template index */
            int which = rand_int(SIZE_FRACTAL_REPOSITORY);

            /* Get the template */
            t_ptr = &fractal_repository[which];

            /* The size matches the desired type? */
            if (t_ptr->type == type) break;
        }

        /* Create and initialize a fractal map */
        map = fractal_map_create(t_ptr);

        /* Complete the fractal map */
        fractal_map_complete(map, t_ptr);

        /* Get the map size */
        hgt = fractal_dim[type].hgt;
        wid = fractal_dim[type].wid;

        /* Get the top-left corner */
        y1 = y - hgt / 2;
        x1 = x - wid / 2;

        /* Transfor the dungeon */
        for (y = 0; y < hgt; y++)
        {
            for (x = 0; x < wid; x++)
            {
                /* Get dungeon coordinates */
                int yy = y1 + y;
                int xx = x1 + x;

                u16b feat;

                /* The map doesn't specifies a floor grid */
                if (map[y][x] < FRACTAL_FLOOR) continue;

                /* Ignore annoying locations */
                if (!in_bounds(yy, xx)) continue;

                /* Remove mark */
                dungeon_info[yy][xx].cave_info &= ~(CAVE_TEMP);

                /* Ignore forbidden locations */
                if (dungeon_info[yy][xx].cave_info & (CAVE_ICKY)) continue;

                /* Get the current feature */
                feat = dungeon_info[yy][xx].feature_idx;

                /* Certain features are forbidden */
                if (feat_ff1_match(feat, FF1_PERMANENT | FF1_DOOR | FF1_STAIRS | FF1_HAS_GOLD)) continue;

                /* Elemental features too */
                if (feat_ff3_match(feat, TERRAIN_MASK)) continue;

                /* Ignore features that contain  objects */
                if (dungeon_info[yy][xx].object_idx) continue;

                /* Replace walls */
                if (feat_ff1_match(feat, FF1_WALL))
                {
                    u16b new_wall = wall;

                    /* Flavor */
                    if ((wall == FEAT_CRACKED_WALL_OVER_LAVA) ||
                        (wall == FEAT_WALL_CRACKED_OVER_BOILING_MUD) ||
                        (wall == FEAT_WALL_CRACKED_OVER_BOILING_WATER) ||
                        (wall == FEAT_CRACKED_WALL_OVER_ACID))
                    {
                        int k = rand_int(100);

                        if (k < 7) new_wall = FEAT_SCORCHED_WALL;

                        else if (k < 10) new_wall = FEAT_WALL_GRANITE_CRACKED;
                    }
                    else if (wall == FEAT_WALL_SANDSTONE)
                    {
                        int k = rand_int(100);

                        if (k < 5) new_wall = FEAT_WALL_GRANITE_CRACKED;
                    }
                    else if (wall == FEAT_WALL_ICE)
                    {
                        int k = rand_int(100);

                        if (k < 10) new_wall = FEAT_WALL_ICE_CRACKED;
                    }
                    else if (wall == FEAT_WALL_ICE_CRACKED)
                    {
                        int k = rand_int(100);

                        if (k < 10) new_wall = FEAT_WALL_ICE;
                    }
                    else if (wall == FEAT_WALL_COAL)
                    {
                        int k = rand_int(100);

                        if (k < 10) new_wall = FEAT_SCORCHED_WALL;

                        else if (k < 17) new_wall = FEAT_WALL_COAL_BURNING;
                    }
                    else if (wall == FEAT_WALL_SHALE)
                    {
                        int k = rand_int(100);

                        if (k < 10) new_wall = FEAT_WALL_COAL;

                        else if (k < 17) new_wall = FEAT_QUARTZ_VEIN;
                    }
                    else if (wall == FEAT_WALL_VINES)
                    {
                        int k = rand_int(100);

                        if (k < 10) new_wall = FEAT_WALL_EARTH;
                    }

                    cave_set_feat(yy, xx, new_wall);
                }
                /* Replace floor if necessary */
                else if ((floor != FEAT_NONE) && feat_ff1_match(feat, FF1_MOVE))
                {
                    u16b new_floor = floor;

                    /* Flavor */
                    if (floor == FEAT_FLOOR_WET)
                    {
                        int k = rand_int(100);

                        if (k < 30) new_floor = FEAT_FLOOR_WATER;
                    }
                    else if (floor == FEAT_FOREST_SOIL)
                    {
                        int k = rand_int(100);

                        if (k < 20) new_floor = FEAT_FLOOR_EARTH;

                        else if (k < 21) new_floor = FEAT_TREE;

                        else if (k < 26) new_floor = FEAT_BUSH;

                        else if (k < 36) new_floor = FEAT_GRASS;
                    }
                    else if (floor == FEAT_FLOOR_SAND)
                    {
                        int k = rand_int(100);

                        if (k < 7) new_floor = FEAT_FLOOR_ROCK;
                    }
                    else if (floor == FEAT_FLOOR_MUD)
                    {
                        int k = rand_int(100);

                        if (k < 15) new_floor = FEAT_FLOOR_MUD;
                    }

                    cave_set_feat(yy, xx, new_floor);
                }
            }
        }

        /* Free resources */
        FREE_ARRAY(map);
    }
}


/*
 * Tranform walls in the dungeon based on the wall features contained in the
 * given feature selector
 * The location of the target grids are taken from the given array
 * The "rad" field of the pairs contained in the feature selector is used
 * to sometimes expand the size of the walls (flavor).
 */
static void transform_walls(QVector<coord> grids, QVector<feature_selector_item_type> selections)
{
    int max, y, x, i;

    int total_chance = 0;

    for (int i = 0; i < selections.size(); i++)
    {
        total_chance += selections.at(i).chance;
    }

    /* Get the number of grids to tranform */

    /* First we get a divisor */
    /* Just one feature */
    if (selections.size() == 1) max = 20;

    /* Rare features */
    else if (total_chance < 150) max = 15;

    /* Regular case */
    else max = 8 - selections.size();

    /* Must have a lower bound */
    if (max < 4) max = 4;

    /* Apply the divisor to the number of grids of the array */
    max = grids.size() / max;

    /* Paranoia */
    if (max < 1) return;

    /* Flavor */
    max += (rand_int(max) / 2);

    /* Transform "max" walls */
    for (i = 0; i < max; i++)
    {
        int yy, xx;
        int rad;
        bool is_effect = FALSE;

        /* Get a wall */
        int item_num = feature_selector_select(selections);

        // Paranoia
        if (item_num == -1) break;

        /* Got none */
        if (selections.at(item_num).wall == FEAT_NONE) continue;

        /* Find out if it is an effect */
        if (feat_ff2_match(selections.at(item_num).wall, FF2_EFFECT)) is_effect = TRUE;

        /* Pick a location */
        if (!pick_transform_center(grids, selections.at(item_num).level_flag, &y, &x)) return;

        /* Get the radius */
        rad = selections.at(item_num).rad;

        /* Flavor for radius equal to 1 */
        if ((selections.at(item_num).rad == 1) && one_in_(2)) rad = 0;

        /* Flavor for radius equal to 2 */
        if (selections.at(item_num).rad == 2)
        {
            int k = rand_int(100);
            if (k < 20) rad = 0;
            if (k < 60) rad = 1;
            else rad = 2;
        }

        /* Tranform the walls based on the calculated radius */
        for (yy = (y - rad); yy <= (y + rad); yy++)
        {
            for (xx = (x - rad); xx <= (x + rad); xx++)
            {
                /* Ignore annoying locations */
                if (!in_bounds(yy, xx)) continue;

                /* The grid  must be marked too */
                if (dungeon_info[yy][xx].cave_info & (CAVE_TEMP))
                {
                    /* Effects on walls */
                    if (is_effect)
                    {
                        set_effect_inscription(selections.at(item_num).wall, yy, xx, SOURCE_EFFECT, 0);
                    }
                    /* Normal walls */
                    else
                    {
                        cave_set_feat(yy, xx, selections.at(item_num).wall);
                    }

                    /* Clear the mark */
                    dungeon_info[yy][xx].cave_info &= ~(CAVE_TEMP);
                }
            }
        }
    }
}


/*
 * Try to add the LF1_* flags of num_rolls terrain features to level_flag.
 */
static void roll_level_flag(int num_rolls)
{
    u16b feat;
    int i;

    /* Try with num_rolls features */
    for (i = 0; i < num_rolls; i++)
    {
        /* Pick a lake feature */
        feat = pick_proper_feature(cave_feat_lake);

        /* Is it an elemental feature? */
        if (feat_ff3_match(feat, TERRAIN_MASK))
        {
            /* Get the element flag */
            u32b flag = get_level_flag(feat);

            /* Debug message */
            if (cheat_room && !(level_flag & flag))
            {

                QString name;


                name = describe_one_level_flag(flag);

                color_message(QString("Adding %1 to level_flag.") .arg(name), TERM_WHITE);
            }

            /* Extend level_flag */
            level_flag |= flag;
        }
    }
}


/*
 * Transforms walls and regions in the dungeon to add more flavor to the game
 */
static void transform_walls_regions(void)
{
    QVector<coord> grids;
    grids.clear();
    int y, x, i;
    u16b feat;
    bool enable_nature = FALSE;

    /* Feature selectors */
    QVector<feature_selector_item_type> wall_selections;
    QVector<feature_selector_item_type> region_selections;
    feature_selector_item_type item_body;
    feature_selector_item_type *item = &item_body;
    wall_selections.clear();
    region_selections.clear();

    /* Ignore wilderness dungeons */
    if (!(*dun_cap->can_be_transformed)()) return;

    /* Flavor */
    if (one_in_(20)) return;

    /* Add glowing walls */
    if ((p_ptr->depth < 50) || !one_in_(4))
    {
        item->level_flag = 0;
        item->wall = FEAT_WALL_ELVISH;
        item->floor = FEAT_NONE;
        item->chance = 100;
        item->rad = 1;

        wall_selections.append(item_body);
    }

    /* Add silent watchers */
    if ((p_ptr->depth < 10) ? FALSE: (p_ptr->depth >= 35) ? one_in_(10): one_in_(20))
    {
        item->level_flag = 0;
        item->wall = FEAT_SILENT_WATCHER;
        item->floor = FEAT_NONE;
        item->chance = 30;
        item->rad = 0;

        wall_selections.append(item_body);
    }

    /* Add inscription effect */
    item->level_flag = 0;
    item->wall = FEAT_WALL_INSCRIPTION;
    item->floor = FEAT_NONE;
    item->chance = 20;
    item->rad = 0;

    wall_selections.append(item_body);

    /* Add teleport walls */
    if ((p_ptr->depth < 5) ? FALSE: (p_ptr->depth >= 40) ? one_in_(2): one_in_(10))
    {
        item->level_flag = 0;
        item->wall = FEAT_ETHEREAL_WALL;
        item->floor = FEAT_NONE;
        item->chance = 30;
        item->rad = 0;

        wall_selections.append(item_body);
    }

    feat = 0;

    /* Count elemental features in the current level */
    for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
    {
        for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
        {
            if (cave_ff3_match(y, x, TERRAIN_MASK)) ++feat;
        }
    }

    /* We are allowed to use elemental features depending on element presence or dungeon depth */
    if ((feat > 15) || (rand_int(200) < p_ptr->depth))
    {
        /* Add more flavor */
        roll_level_flag(level_flag ? 3: 10);

        /* Allow elemental features */
        enable_nature = TRUE;
    }

    /* Traverse the array of elemental transformations */
    for (i = 0; enable_nature; i++)
    {
        /* Get the current elemental transformation */
        elemental_transformation_info *et_ptr = &elemental_transformations[i];

        /* The end of the array was reached */
        if (!et_ptr->level_flag) break;

        /* Allow only features compatible with the current level type */
        if (!(level_flag & et_ptr->level_flag)) continue;

        /* Check depth of features */
        if (TRUE)
        {
            int depth = -1;
            int damage = -1;

            /* Check walls */
            if (et_ptr->wall)
            {
                feature_type *f_ptr = &f_info[et_ptr->wall];

                depth = MAX(f_ptr->f_level, depth);

                damage = MAX(f_ptr->dam_non_native, damage);
            }

            /* Check floors */
            if (et_ptr->floor)
            {
                feature_type *f_ptr = &f_info[et_ptr->floor];

                depth = MAX(f_ptr->f_level, depth);

                damage = MAX(f_ptr->dam_non_native, damage);
            }

            /* Feature is OOD */
            if (depth > (p_ptr->depth + 20))
            {
                /* Feature is too dangerous */
                if (damage > (p_ptr->mhp / 2)) continue;

                /* Sometimes we allow this feature */
                if (depth > (p_ptr->depth + 40))
                {
                    if (!one_in_(7)) continue;
                }
                else
                {
                    if (!one_in_(4)) continue;
                }
            }
        }

        /* Create a feature selector item */
        item->level_flag = et_ptr->level_flag;
        item->wall = et_ptr->wall;
        item->floor = et_ptr->floor;
        item->chance = et_ptr->chance;
        item->rad = et_ptr->rad;

        /* Give it to the proper selector based on the transformation type */
        if (et_ptr->type == TRANSFORM_WALL)
        {
            wall_selections.append(item_body);
        }
        else if (et_ptr->type == TRANSFORM_REGION)
        {
            region_selections.append(item_body);
        }
    }

    /* We don't have a single feature pair */
    if (!wall_selections.size() && !region_selections.size()) return;

    /* Collect room walls locations for the transformations */
    for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
    {
        for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
        {
            bool ignore = TRUE;

            /* Must be room grids, and they don't have to be forbidden */
            if ((dungeon_info[y][x].cave_info & (CAVE_ROOM | CAVE_ICKY)) != (CAVE_ROOM)) continue;

            /* Get the current feature */
            feat = dungeon_info[y][x].feature_idx;

            /* Ignore non-walls */
            if (!feat_ff1_match(feat, FF1_WALL)) continue;

            /* Ignore certain wall types */
            if (feat_ff1_match(feat, FF1_PERMANENT | FF1_INNER | FF1_HAS_GOLD)) continue;

            /* Ignore elemental walls */
            if (feat_ff3_match(feat, TERRAIN_MASK)) continue;

            /* They must be outer walls */
            for (i = 0; i < 8; i++)
            {
                int yy = y + ddy_ddd[i];
                int xx = x + ddx_ddd[i];

                /* Ignore walls adjacent to doors and stairs */
                if (cave_ff1_match(yy, xx, FF1_DOOR | FF1_STAIRS))
                {
                    ignore = TRUE;
                    break;
                }

                /* We found a non-room grid. Remember that */
                /* Keep looking for doors or stairs */
                if (!(dungeon_info[yy][xx].cave_info & (CAVE_ROOM))) ignore = FALSE;
            }

            /* Ignore the wall if necessary */
            if (ignore) continue;

            /* Mark the wall */
            dungeon_info[y][x].cave_info |= (CAVE_TEMP);

            /*
             * Remember only some of the valid walls
             * This prevents an excesive concentration of transformed walls
             * in the lower part of the dungeon
             */
            if (one_in_(4))
            {
                /* Save the coordinates */
                grids.append(make_coords(y, x));
            }
        }
    }

    /* Apply the transformations */
    if (grids.size())
    {
        /* To walls */
        if (wall_selections.size()) transform_walls(grids, wall_selections);

        /* To regions */
        if (region_selections.size()) transform_regions(grids, region_selections);
    }

    /* Clear the marks */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            dungeon_info[y][x].cave_info &= ~(CAVE_TEMP);
        }
    }

    /* Debug message */
    if (cheat_room)
    {
        debug_all_level_flags(level_flag);
    }
}


/*
 * Build a lake using the given feature.
 * Returns TRUE on success.
 * The coordinates of its center are stored in y0 and x0.
 */
static bool build_lake(int feat, bool do_big_lake, int *y0, int *x0)
{
    int bx0, by0;
    int bx1, by1, bx2, by2;
    int wid, hgt;
    int tries = 0;
    int lake_idx;
    const room_data *ld;
    /* Starburst flags */
    u32b flag = 0;

    /*
     * Notice special cases: these are replaced with passable features
     * sometimes (build_terrain)
     */
    bool solid_lake = (feat_ff1_match(feat, FF1_WALL) &&
        (feat != FEAT_WALL_LIMESTONE) && (feat != FEAT_WALL_COAL));

    /* Solid lakes are made very large sometimes */
    if (solid_lake && one_in_(2)) do_big_lake = TRUE;

    /* Prevent secluded regions in the dungeon at shallow depths */
    if (!solid_lake && (p_ptr->depth <= 10)) do_big_lake = FALSE;

    /* Choose an initial size for the lake */
    if (do_big_lake)
    {
        if (one_in_(10)) lake_idx = LAKE_DATA_4x5;
        else if (one_in_(5)) lake_idx = LAKE_DATA_4x4;
        else lake_idx = LAKE_DATA_3x4;
    }
    else
    {
        /*
         * Lakes at shallow levels are smaller, and some at deeper
         * levels too
         */
        if ((p_ptr->depth >= 25) && one_in_(7))
        {
            lake_idx = LAKE_DATA_3x4;
        }
        else
        {
            lake_idx = LAKE_DATA_2x3;
        }
    }

    /* Adjust the size of the lake, if needed */
    while (TRUE)
    {
        /* Get block information for this kind of lake */
        ld = &lake_data[lake_idx];

        /* Get the size of the lake in blocks */
        hgt = ld->dy2 - ld->dy1 + 1;
        wid = ld->dx2 - ld->dx1 + 1;

        /* Can be placed in this dungeon? */
        if ((hgt <= dun->row_rooms) && (wid <= dun->col_rooms)) break;

        /* Try again with a smaller lake */
        --lake_idx;

        /* Level too small, give up */
        if (lake_idx < 0)
        {
            if (cheat_room)
            {
                color_message(QString("Can't place lakes in this dungeon!"), TERM_WHITE);
            }

            return (FALSE);
        }
    }

    /* Try to get a location for the lake */
    while (TRUE)
    {
        /* Too many tries. Reject lake */
        if (++tries >= 200)
        {
            if (cheat_room)
            {
                color_message(QString("Can't find a blocks for lakes in this dungeon!"), TERM_WHITE);
            }

            return (FALSE);
        }

        /* Get central block */
        by0 = rand_int(dun->row_rooms);
        bx0 = rand_int(dun->col_rooms);

        /* Get the blocks */
        by1 = by0 + ld->dy1;
        by2 = by0 + ld->dy2;
        bx1 = bx0 + ld->dx1;
        bx2 = bx0 + ld->dx2;

        /* Ignore blocks outside the dungeon */
        if ((by1 < 0) || (by2 >= dun->row_rooms)) continue;
        if ((bx1 < 0) || (bx2 >= dun->col_rooms)) continue;

        /* Found a suitable location */
        break;
    }

    /* Get total height and width of the available space */
    hgt *= BLOCK_HGT;
    wid *= BLOCK_WID;

    /* Get the center of the lake */
    *y0 = by1 * BLOCK_HGT + hgt / 2;
    *x0 = bx1 * BLOCK_WID + wid / 2;

    /* Store extra information for passable lakes */
    if (!solid_lake)
    {
        /* Forests are always lit. Others not so much */
        if (feat_ff3_match(feat, FF3_FOREST) ||
            (p_ptr->depth <= randint(25)))
        {
            flag |= (STAR_BURST_LIGHT);
        }

        /* Lakes are rooms now */
        flag |= (STAR_BURST_ROOM);

        /* Connect the lake with the dungeon */
        if (dun->cent_n < CENT_MAX)
        {
            dun->cent[dun->cent_n].y = *y0;
            dun->cent[dun->cent_n].x = *x0;
            dun->cent_n++;
        }

        /* We won't build rooms over small lakes */
        if (!do_big_lake)
        {
            int bx, by;

            for (by = by1; by <= by2; by++)
            {
                for (bx = bx1; bx <= bx2; bx++)
                {
                    /* Mark the blocks as used */
                    dun->room_map[by][bx] = TRUE;
                }
            }
        }
    }

    /*
     * Convenience. Get the distance from the center to the borders.
     * Note that we substract some space to place tunnels later and to
     * avoid dungeon permanent boundry
     */
    hgt = (hgt - 4) / 2;
    wid = (wid - 4) / 2;

    /* Place the lake */
    generate_starburst_room(*y0 - hgt, *x0 - wid, *y0 + hgt, *x0 + wid,
        feat, f_info[feat].f_edge, flag);

    /* Success */
    return (TRUE);
}


/*
 * Build a river given a feature and its starting location
 */
static void build_river(int feat, int y, int x)
{
    /*
     * This map contains the directions of the grids who must be converted
     * to edge, given a compass direction [0-3]
     */
    static byte edge_map[][3] =
    {
        {1, 2, 3},
        {7, 8, 9},
        {3, 6, 9},
        {1, 4, 7}
    };

    int i, dir, old_dir;
    int old_feat;
    int edge = f_info[feat].f_edge;
    /*
     * Notice special cases: they are replaced by passable features
     * sometimes (build_terrain)
     */
    bool solid_river = (feat_ff1_match(feat, FF1_WALL) &&
        (feat != FEAT_WALL_LIMESTONE) && (feat != FEAT_WALL_COAL));

    /* Choose a random compass direction */
    dir = old_dir = rand_int(4);

    /* Place river into dungeon */
    while (TRUE)
    {
        /* Stop at dungeon edge */
        if (!in_bounds_fully(y, x)) break;

        /* Get the previous content of the grid */
        old_feat = dungeon_info[y][x].feature_idx;

        /* Stop at permanent feature */
        if (feat_ff1_match(old_feat, FF1_PERMANENT)) break;

        /* Most rivers aren't pierced by rooms. */
        if (!solid_river)
        {
            /* Forbid rooms here */
            int by = y / BLOCK_HGT;
            int bx = x / BLOCK_WID;

            dun->room_map[by][bx] = TRUE;
        }

        /* Place a piece of the river, if needed */
        if (feat != old_feat) build_terrain(y, x, feat);

        /* Place river edge, if needed */
        if (edge != FEAT_NONE)
        {
            for (i = 0; i < 3; i++)
            {
                /* Use the map to modify only grids ahead */
                int yy = y + ddy[edge_map[dir][i]];
                int xx = x + ddx[edge_map[dir][i]];

                /* Ignore annoying locations */
                if (!in_bounds_fully(yy, xx)) continue;

                /* Get the previous content of the grid */
                old_feat = dungeon_info[yy][xx].feature_idx;

                /* Avoid permanent features */
                if (feat_ff1_match(old_feat, FF1_PERMANENT)) continue;

                /* IMPORTANT: Don't overwrite the river */
                if (old_feat == feat) continue;

                /* Place river edge, if needed */
                if (edge != old_feat) build_terrain(yy, xx, edge);
            }
        }

        /* Stagger the river */
        if (one_in_(2))
        {
            dir = rand_int(4);
        }
        /* Advance the streamer using the original direction */
        else
        {
            dir = old_dir;
        }

        /* Get the next coordinates */
        y += ddy_ddd[dir];
        x += ddx_ddd[dir];
    }
}


/*
 * Place lakes and rivers given a feature
 */
static bool build_feature(int feat, bool do_big_lake)
{
    /* No coordinates yet */
    int x0 = 0, y0 = 0;

    /* Build a lake? */
    if (feat_ff2_match(feat, FF2_LAKE) ||
        !feat_ff2_match(feat, FF2_RIVER))
    {
        /* Try to place the lake. Get its center */
        if (!build_lake(feat, do_big_lake, &y0, &x0)) return (FALSE);
    }

    /* Build a river */
    if (feat_ff2_match(feat, FF2_RIVER) && (ponder_dungeon_size() > 2))
    {
        /* Pick starting coordinates, if needed */
        if ((y0 + x0) == 0)
        {
            y0 = randint(p_ptr->cur_map_hgt - 2);
            x0 = randint(p_ptr->cur_map_wid - 2);
        }

        /* Generate the river */
        build_river(feat, y0, x0);
    }

    /* Success */
    return (TRUE);
}



/*
 * Build lakes and rivers for the dungeon
 */
void build_nature(void)
{
    int i;
    bool big;
    QString name;

    int feat, dev_feat = FEAT_NONE;
    int count = 0, max_features;

    /* Get the maximum number of features based on level size */
    byte dun_size = ponder_dungeon_size();

     /* Clear the level's restriction */
     level_flag = 0;

    /* No NPP terrains option turned on */
    if (birth_classic_dungeons) return;

    /* Debug message */
    if (cheat_room)
    {
        color_message(QString("Dungeon size: %1.") .arg((int)dun_size), TERM_WHITE);
    }

    if (dun_size == 1) max_features = (one_in_(4) ? 1: 0);

    else if (dun_size == 2) max_features = 1;

    else if (dun_size == 3) max_features = (one_in_(3) ? 2: 1);

    else max_features = DUN_MAX_LAKES;

    /* Check quests for specific element flags */
    for (i = 0; i < z_info->q_max; i++)
    {
        /* Get the quest */
        quest_type *q_ptr = &q_info[i];

        /* Active quest? */
        if ((q_ptr->base_level == p_ptr->depth) && !is_quest_complete(i)) continue;

        /* Monster quests */
        if ((quest_fixed(q_ptr)) || (quest_single_r_idx(q_ptr)))
        {
            /* Restrict feature generation */
            level_flag |= get_level_flag_from_race(&r_info[q_ptr->mon_idx]);
        }
        /* themed quests */
        else if (quest_themed(q_ptr))
        {
            u16b j;

            /* Find specific element flags for the theme */
            for (j = 0; j < CUR_NUM_THEME_LEVEL_FLAGS; j++)
            {
                /* Ignore other themes */
                if (themed_level_flags[j].theme != q_ptr->q_theme) continue;

                /* Restrict feature generation */
                level_flag |= themed_level_flags[j].flags;

                /* Done */
                break;
            }
        }
    }

    /* The chance to get special terrain varies with dungeon depth */
    if ((level_flag == 0) && (rand_int(300) >= p_ptr->depth)) return;

    /* Debug message */
    if (level_flag && cheat_room)
    {
        color_message(QString("Level flags added by quests."), TERM_WHITE);
        debug_all_level_flags(level_flag);
    }

    /* Allocate some lakes and rivers */
    for (count = 0; count < max_features; count++)
    {
        /* Very small levels always get a feature -DG- */
        if ((max_features > 1) && (rand_int(100) < 20))
        {
            continue;
        }

        /* Developer, are you testing features? */
        if (dev_feat)
        {
            feat = dev_feat;

            dev_feat = FEAT_NONE;
        }
        /* NPP releases only need this "else" block */
        else
        {
            /* Pick a feature */
            feat = pick_proper_feature(cave_feat_lake);
        }

        /* Got a valid feature? */
        if (feat)
        {
            /* Try a big lake */
            if ((dun_size <= 3) || (randint(150) < p_ptr->depth))
            {
                big = TRUE;
            }
            else
            {
                big = FALSE;
            }

            /* Report creation of lakes/rivers */
            if (cheat_room)
            {
                name = feature_desc(feat, FALSE, FALSE);

                if (f_info[feat].f_edge)
                {
                    QString edge = feature_desc(f_info[feat].f_edge, FALSE, FALSE);

                    message(QString("Building %1%2 surrounded by %3.") .arg(big ? "big ": "") .arg(name) .arg(edge));
                }
                else
                {
                    message(QString("Building %1%2.") .arg(big ? "big ": ""). arg(name));
                }
            }

            /* Build one lake/river. */
            build_feature(feat, big);
        }
    }

    /* Debug message */
    if (level_flag && cheat_room)
    {
        color_message(QString("Level flags added by lake generation."), TERM_WHITE);
        debug_all_level_flags(level_flag);
    }
}

/*
 *Helper function for max number of creatures on a themed level.
 This function is for non-uniques only.
 */
byte max_themed_monsters(const monster_race *r_ptr, u32b max_power)
{
    /*first off, handle uniques*/
    if (r_ptr->flags1 & RF1_UNIQUE) return (r_ptr->max_num);

    /*don't allow 99 of the out of depth monsters*/
    if (r_ptr->level > p_ptr->depth + 3)
    {
        int lev_ood = p_ptr->depth - r_ptr->level;

        /*Too strong*/
        if (r_ptr->mon_power > max_power) return (0);

        else if (lev_ood > 5) return (MON_RARE_FREQ);
        else return (MON_LESS_FREQ);
    }
    else if ((r_ptr->level < p_ptr->depth - 5) && (r_ptr->level < 75))
    {
        int lev_ood = p_ptr->depth - r_ptr->level;

        /*Too weak*/
        if (r_ptr->mon_power < max_power / 20) return (0);

        else if (r_ptr->mon_power < max_power / 10) return (MON_RARE_FREQ);
        else if (lev_ood > 5) return (MON_LESS_FREQ);
    }
    /*The rigth depth*/
    return (r_ptr->max_num);
}





/*
 * Place some miscellaneous features on dungeon.
 * Use this when dungeon generation is finished.
 */
void build_misc_features(void)
{
    /* Moria dungeons are simple */
    if (game_mode == GAME_NPPMORIA) return;

    /* Sometimes we place fog on water and ice levels */
    if ((level_flag & (LF1_WATER | LF1_ICE)) && !(level_flag & (LF1_LAVA | LF1_FIRE)) &&
        (p_ptr->depth >= 35) && one_in_(3))
    {
        int i, k = 2;

        /* One fog formation more if the levels is big enough */
        if (dun->cent_n >= 7) ++k;

        /* Sometimes two formations more */
        if ((dun->cent_n >= 10) && one_in_(2)) ++k;

        /* Sometimes three formations more */
        if ((dun->cent_n >= 12) && one_in_(3)) ++k;

        /* Build the fog formations */
        for (i = 0; i < k; i++)
        {
            /* Build */
            build_fog();
        }
    }

    /* More flavor! */
    if (!birth_classic_dungeons) transform_walls_regions();
}

