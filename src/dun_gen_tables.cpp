
/* File: dun_gen_tables.cpp */

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
//The tables for dungeon generation
// Also includes the global variables



/*
 * This table holds aditional flags for themed levels
 * These flags are used to forbid the generation of certain features BEFORE
 * placing any lakes or pools.
 * Example: you can assign LF1_LAVA to a red dragon level to avoid the
 * generation of ice, acid, oil, etc.
 * See "build_themed_level_nature"
 */
flags_themed_levels themed_level_flags[CUR_NUM_THEME_LEVEL_FLAGS] =
{
    {LEV_THEME_DEMON_MINOR,	LF1_FIRE},
    {LEV_THEME_DEMON_ALL,	LF1_FIRE},
    {LEV_THEME_DEMON_MAJOR,	LF1_LAVA | LF1_FIRE},
    {LEV_THEME_DRAGON_FIRE,	LF1_FIRE},
    {LEV_THEME_DRAGON_ACID,	LF1_ACID},
    {LEV_THEME_DRAGON_ELEC,	LF1_WATER},
    {LEV_THEME_DRAGON_COLD,	LF1_ICE},
    {LEV_THEME_UNDEAD,		LF1_ICE},
    /* Add entries for more themed levels if needed */
};

/*
 * Table of "magic" values used to ponder the size of the dungeon
 * 1 means "tiny": 2x2, 2x3, 2x4, 3x2, 3x3, 4x2
 * 2 means "small rectangle": 2x5, 2x6, 3x4, 4x3, 5x2, 6x2
 * 3 means "medium rectangle": 3x5, 3x6, 4x4, 5x3, 6x3
 * 4 means "medium": 4x5, 4x6, 5x4, 6x4
 * 5 means "large": 5x5, 5x6, 6x5
 * 6 means "largest": 6x6
 */
byte dungeon_size_tab[7][7] =
{
    /*	0	1	2	3	4	5	6	*/
    {	0,	0,	0,	0,	0,	0,	0	},	/* 0 */
    {	0,	0,	0,	0,	0,	0,	0	},	/* 1 */
    {	0,	0,	1,	1,	1,	2,	2	},	/* 2 */
    {	0,	0,	1,	1,	2,	3,	3	},	/* 3 */
    {	0,	0,	1,	2,	3,	4,	4	},	/* 4 */
    {	0,	0,	2,	3,	4,	5,	5	},	/* 5 */
    {	0,	0,	2,	3,	4,	5,	6	},	/* 6 */
};


/*
 * Array of room types (assumes 11x11 blocks)
 */
const room_data room[ROOM_MAX] =
{
    { 0, 0, 0, 0, 0 },		/* 0 = Nothing */
    { 0, 0, -1, 1, 1 },		/* 1 = Simple (33x11) */
    { 0, 0, -1, 1, 1 },		/* 2 = Overlapping (33x11) */
    { 0, 0, -1, 1, 3 },		/* 3 = Crossed (33x11) */
    { 0, 0, -1, 1, 3 },		/* 4 = Large (33x11) */
    { 0, 0, -1, 1, 5 },		/* 5 = Monster nest (33x11) */
    { 0, 0, -1, 1, 5 },		/* 6 = Monster pit (33x11) */
    { 0, 1, -1, 1, 5 },		/* 7 = Lesser vault (33x22) */
    { -1, 2, -2, 3, 10 },	/* 8 = Greater vault (66x44) */
    { 0, 1, -1, 1, 0 },		/* 9 = Quest vault (44x22) */
    { 0, 1, -1, 1, 0},		/* 10 = Starburst (33x22) */
    { -1, 2, -2, 3, 0},		/* 11 = Great Starburst (66x44) */

    /*
     * Hack - if the dimensions for starburst rooms or great starburst
     * rooms change, these lines in build_type_starburst need changing:
     if (giant_room)
     {
        dy = 19;
        dx = 30;
     }
     33x22
     else
     {
        dy = 9;
        dx = 14;
     }

     -JG
     */

    {0, 1, -1, 1, 0},		/* 12 = Small Fractal (33x22) */
    {-1, 1, -1, 1, 0},		/* 13 = Medium Fractal (33x33) */
    {-1, 1, -2, 3, 0}		/* 14 = Big Fractal (66x33) */
};

/*
 * This table holds the default features used in *almost* all lakes and pools
 * of a themed level.
 * See "pick_proper_feature"
 */
level_feature_themes themed_level_features[CUR_NUM_THEME_LEVEL_FEATURES] =
{
    {LEV_THEME_DEMON_MINOR,	FEAT_FIRE},
    {LEV_THEME_DEMON_ALL,	FEAT_FIRE},
    {LEV_THEME_DEMON_MAJOR,	FEAT_FLOOR_LAVA},
    {LEV_THEME_DEMON_MAJOR,	FEAT_FIRE},
    {LEV_THEME_DRAGON_FIRE,	FEAT_FIRE},
    {LEV_THEME_DRAGON_ACID,	FEAT_FLOOR_ACID},
    {LEV_THEME_DRAGON_ELEC,	FEAT_FLOOR_WATER},
    {LEV_THEME_DRAGON_COLD,	FEAT_FLOOR_ICE},
    {LEV_THEME_TROLL,		FEAT_FOREST_SOIL},
    {LEV_THEME_OGRE,		FEAT_FOREST_SOIL},
    {LEV_THEME_OGRE,		FEAT_THICKET},
    /* Add entries for more themed levels if needed */
};

/*
 * A list of the available fractal templates.
 */
fractal_template fractal_repository[SIZE_FRACTAL_REPOSITORY] =
{
    {FRACTAL_TYPE_17x33, 33, fractal1_init_func},
    {FRACTAL_TYPE_33x65, 65, fractal2_init_func},
    {FRACTAL_TYPE_9x9, 9, fractal3_init_func},
    {FRACTAL_TYPE_17x17, 17, fractal4_init_func},
    {FRACTAL_TYPE_33x33, 33, fractal5_init_func},
};

fractal_dim_struct fractal_dim[MAX_FRACTAL_TYPES] =
{
    {17, 33},		/* FRACTAL_TYPE_17x33 */
    {33, 65},		/* FRACTAL_TYPE_33x65 */
    {9, 9},			/* FRACTAL_TYPE_9x9 */
    {17, 17},		/* FRACTAL_TYPE_17x17 */
    {33, 33},		/* FRACTAL_TYPE_33x33 */
};

elemental_transformation_info elemental_transformations[CUR_NUM_ELEMENTAL_TRANSFORMATIONS] =
{
    {LF1_ICE, TRANSFORM_REGION, FEAT_WALL_ICE, FEAT_FLOOR_ICE, 200, 0},
    {LF1_ICE, TRANSFORM_REGION, FEAT_WALL_ICE_CRACKED, FEAT_FLOOR_ICE, 200, 0},

    {LF1_WATER, TRANSFORM_REGION, FEAT_WALL_LIMESTONE, FEAT_FLOOR_WET, 150, 0},

    {LF1_LAVA, TRANSFORM_REGION, FEAT_CRACKED_WALL_OVER_LAVA, FEAT_BURNT_SPOT, 200, 0},

    {LF1_SAND, TRANSFORM_REGION, FEAT_WALL_SANDSTONE, FEAT_FLOOR_SAND, 100, 0},

    {LF1_OIL, TRANSFORM_REGION, FEAT_WALL_COAL, FEAT_NONE, 200, 0},
    {LF1_OIL, TRANSFORM_REGION, FEAT_WALL_SHALE, FEAT_NONE, 100, 0},

    {LF1_FOREST, TRANSFORM_WALL, FEAT_PUTRID_FLOWER, FEAT_NONE, 10, 1},
    {LF1_FOREST, TRANSFORM_REGION, FEAT_WALL_VINES, FEAT_FOREST_SOIL, 150, 0},

    {LF1_MUD, TRANSFORM_REGION, FEAT_WALL_EARTH, FEAT_FLOOR_MUD, 100, 0},

    {LF1_BWATER, TRANSFORM_REGION, FEAT_WALL_CRACKED_OVER_BOILING_MUD, FEAT_NONE, 200, 0},

    {LF1_BMUD, TRANSFORM_REGION, FEAT_WALL_CRACKED_OVER_BOILING_WATER, FEAT_NONE, 200, 0},

    {LF1_FIRE, TRANSFORM_REGION, FEAT_SCORCHED_WALL, FEAT_BURNT_SPOT, 100, 0},

    {LF1_ACID, TRANSFORM_REGION, FEAT_CRACKED_WALL_OVER_ACID, FEAT_BURNT_SPOT, 100, 0},

    /* Marks the end of the list */
    {0,0,0,0,0,0}
};

/* Block information for lakes (sorted by size, smaller first) */
room_data lake_data[MAX_LAKE_DATA] =
{
    {0, 1, 0, 1, 1},		/* LAKE_DATA_2x2 */
    {0, 1, -1, 1, 1},		/* LAKE_DATA_2x3 */
    {-1, 1, -1, 1, 1},		/* LAKE_DATA_3x3 */
    {-1, 1, -1, 2, 1},		/* LAKE_DATA_3x4 */
    {-1, 2, -1, 2, 1},		/* LAKE_DATA_4x4 */
    {-1, 2, -2, 2, 1},		/* LAKE_DATA_4x5 */
};




// Global variables
dun_data *dun;
bool allow_uniques;
