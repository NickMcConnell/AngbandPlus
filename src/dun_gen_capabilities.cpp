
/* File: gen_capabilities.cpp */

/*
 * Copyright (c) 2010 Jeff Greene, Diego Gonzalez
 * Please see copyright.txt for complete copyright and licensing restrictions.
 */

#include "src/npp.h"
#include "src/dun_generate.h"

// This file handles the functional pointers that are used to know the current level
// limitations and charaacterists through the dun_cap structure.


/*
 * Allow escorts
 */
static bool can_place_escorts_true(s16b r_idx)
{
    (void)r_idx;
    return (TRUE);
}

/*
 * Allow escorts
 */
static bool can_place_escorts_false(s16b r_idx)
{
    (void)r_idx;
    return (FALSE);
}

/*
 * Player in rooms
 */
static bool can_place_player_in_rooms_false(void)
{
    return (FALSE);
}


/*
 * Player in rooms
 */
static bool can_place_player_in_rooms_true(void)
{
    return (TRUE);
}


/*
 * Valid location for stairs
 */
static bool can_place_stairs_default(int y, int x)
{
    return (cave_naked_bold(y, x) ? TRUE: FALSE);
}


/*
 * Adjust the number of stairs in a level
 */
static int adjust_stairs_number_default(int initial_amount)
{
    /* Smaller levels don't need that many stairs, but there are a minimum of 4 rooms */
    if (dun->cent_n > 0)
    {
        if (dun->cent_n <= 4) initial_amount = 1;
        else if (initial_amount > (dun->cent_n / 2)) initial_amount = dun->cent_n / 2;
    }

    return (initial_amount);
}


/*
 * Fog in rooms
 */
static bool can_place_fog_in_rooms_default(void)
{
    return (one_in_(3));
}


/*
 * Fog in rooms
 */
static bool can_place_fog_in_rooms_true(void)
{
    return (TRUE);
}


/*
 * Fog in rooms
 */
static bool can_place_fog_in_rooms_false(void)
{
    return (TRUE);
}


/*
 * Feature is interesting for the look command
 */
static bool can_target_feature_default(int f_idx)
{
    return (feat_ff1_match(f_idx, FF1_NOTICE) ? TRUE: FALSE);
}


/*
 * Dungeon can be transformed
 */
static bool can_be_transformed_true(void)
{
    return (TRUE);
}


/*
 * Dungeon can be transformed
 */
static bool can_be_transformed_false(void)
{
    return (FALSE);
}


/*
 * Non native monsters in elemental terrain
 */
static bool can_place_non_native_monsters_false(void)
{
    return (FALSE);
}


/*
 * Non native monsters in elemental terrain
 */
static bool can_place_non_native_monsters_true(void)
{
    return (TRUE);
}


/*
 * Allow repopulation of monsters on level
 */
static bool allow_level_repopulation_true(void)
{
    return (TRUE);
}


/*
 * Disallow repopulation of monsters on level
 */
static bool allow_level_repopulation_false(void)
{
    return (FALSE);
}


/*
 * Only allow summoners to be relocated from the current level
 */
static bool limited_level_summoning_true(void)
{
    return (TRUE);
}


/*
 * Summoned monsters can be appear from nowhere
 */
static bool limited_level_summoning_false(void)
{
    return (FALSE);
}


/*
 * Only allow summoners to be relocated from the current level
 */
static bool allow_monster_multiply_true(void)
{
    return (TRUE);
}


/*
 * Summoned monsters can be appear from nowhere
 */
static bool allow_monster_multiply_false(void)
{
    return (FALSE);
}


/* Allow breeders to slowly spread */
static bool allow_monster_multiply_quarter(void)
{
    if (one_in_(4)) return (TRUE);
    return (FALSE);
}


/*
 * Earthquakes and destruction are prevented.
 */
static bool prevent_destruction_true(void)
{
    return (TRUE);
}


/*
 * Earthquakes and destruction are allowed.
 */
static bool prevent_destruction_false(void)
{
    return (FALSE);
}


/*
 * Earthquakes and destruction are allowed, except in town.
 */
static bool prevent_destruction_default(void)
{
    if (!p_ptr->depth) return (TRUE);

    return (FALSE);
}


/*
 * Monsters in level
 */
static int get_monster_count_default(void)
{
    return (MIN_M_ALLOC_LEVEL);
}


/*
 * Objects in rooms
 */
static int get_object_count_default(void)
{
    return (Rand_normal(DUN_AMT_ROOM, 3));
}


/*
 * Objects in rooms
 */
static int get_object_count_zero(void)
{
    return (0);
}


/*
 * Gold in both rooms and corridors
 */
static int get_gold_count_default(void)
{
    return (Rand_normal(DUN_AMT_GOLD, 3));
}


/*
 * Gold in both rooms and corridors
 */
static int get_gold_count_zero(void)
{
    return (0);
}


/*
 * objects in both rooms and corridors
 */
static int get_extra_object_count_default(void)
{
    return (Rand_normal(DUN_AMT_ITEM, 3));
}


/*
 * Dungeon capabilities for classic levels
 */
static dungeon_capabilities_type dun_cap_body_default =
{
    can_place_escorts_true,
    can_place_player_in_rooms_false,
    can_place_stairs_default,
    adjust_stairs_number_default,
    can_place_fog_in_rooms_default,
    can_target_feature_default,
    can_be_transformed_true,
    can_place_non_native_monsters_false,
    allow_level_repopulation_true,
    limited_level_summoning_false,
    allow_monster_multiply_true,
    prevent_destruction_default,
    get_monster_count_default,
    get_object_count_default,
    get_gold_count_default,
    get_extra_object_count_default,
};

/*
 * Monsters in level
 */
static int get_monster_count_moria(void)
{
    int alloc_level = (p_ptr->depth / 3);

    /* Boundary Control */
    if (alloc_level < 2) alloc_level = 2;
    else if (alloc_level > 10) alloc_level = 10;

    alloc_level += MIN_M_ALLOC_LEVEL;

    return (alloc_level);
}


/*
 * Objects in rooms
 */
static int get_object_count_moria(void)
{
    return (Rand_normal(DUN_AMT_ROOM_MORIA, 3));
}


/*
 * Gold in both rooms and corridors
 */
static int get_gold_count_moria(void)
{
    return (Rand_normal(DUN_AMT_GOLD_MORIA, 3));
}

/*
 * objects in both rooms and corridors
 */
static int get_extra_object_count_moria(void)
{
    int alloc_level = (p_ptr->depth / 3);

    /* Boundary Control */
    if (alloc_level < 2) alloc_level = 2;
    else if (alloc_level > 10) alloc_level = 10;

    return (DUN_AMT_ITEM_MORIA + randint1(alloc_level));
}


/*
 * Dungeon capabilities for classic levels
 */
static dungeon_capabilities_type dun_cap_body_moria =
{
    can_place_escorts_false,
    can_place_player_in_rooms_false,
    can_place_stairs_default,
    adjust_stairs_number_default,
    can_place_fog_in_rooms_false,
    can_target_feature_default,
    can_be_transformed_false,
    can_place_non_native_monsters_false,
    allow_level_repopulation_true,
    limited_level_summoning_false,
    allow_monster_multiply_true,
    prevent_destruction_default,
    get_monster_count_moria,
    get_object_count_moria,
    get_gold_count_moria,
    get_extra_object_count_moria,
};



/*
 * Allow escorts
 */
static bool can_place_escorts_wild(s16b r_idx)
{
    u32b flags1 = r_info[r_idx].flags1;

    /* Uniques are allowed */
    if (flags1 & (RF1_UNIQUE)) return (TRUE);

    /* Monsters with escorts are allowed only part of the time */
    if (flags1 & (RF1_FRIEND | RF1_FRIENDS | RF1_ESCORT | RF1_ESCORTS)) return (one_in_(10));

    /* Default */
    return (FALSE);
}


/*
 * Valid location for stairs
 */
static bool can_place_stairs_wild(int y, int x)
{
    return (cave_plain_bold(y, x) ? TRUE: FALSE);
}


/*
 * Adjust the number of stairs in a level
 */
static int adjust_stairs_number_unchanged(int initial_amount)
{
    return (initial_amount);
}


/*
 * Feature is interesting for the look command
 */
static bool can_target_feature_wild(int f_idx)
{
    /* Only stairs and doors */
    return (feat_ff1_match(f_idx, FF1_STAIRS | FF1_DOOR) ? TRUE: FALSE);
}


/*
 * Monsters in level
 */
static int get_monster_count_wild(void)
{
    int count = 0;
    int y, x;
    u32b ff1 = (FF1_MOVE | FF1_PLACE);

    /* Count the grids that allow monsters in them */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Found one? Increment the count */
            if (cave_ff1_match(y, x, ff1) == ff1) ++count;
        }
    }

    /* Calculate the monster ratio */
    count = ((15 * count) / 1000);

    /* Paranoia */
    return (MAX(MIN_M_ALLOC_LEVEL, count));
}


/*
 * Objects in rooms
 */
static int get_object_count_wild(void)
{
    return (Rand_normal(90, 20));
}


/*
 * Gold in both rooms and corridors
 */
static int get_gold_count_wild(void)
{
    return (Rand_normal(30, 10));
}


/*
 * Objects in both rooms and corridors
 */
static int get_extra_object_count_zero(void)
{
    return (0);
}


/*
 * Dungeon capabilities for wilderness levels
 */
static dungeon_capabilities_type dun_cap_body_wild =
{
    can_place_escorts_wild,
    can_place_player_in_rooms_true,
    can_place_stairs_wild,
    adjust_stairs_number_unchanged,
    can_place_fog_in_rooms_true,
    can_target_feature_wild,
    can_be_transformed_false,
    can_place_non_native_monsters_true,
    allow_level_repopulation_false,
    limited_level_summoning_true,
    allow_monster_multiply_quarter,
    prevent_destruction_true,
    get_monster_count_wild,
    get_object_count_wild,
    get_gold_count_wild,
    get_extra_object_count_zero,
};


/*
 * Monsters in level
 */
static int get_monster_count_labyrinth(void)
{
    int count = 0;
    int y, x;
    u32b ff1 = (FF1_MOVE | FF1_PLACE);

    /* Count the grids that allow monsters in them */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Found one? Increment the count */
            if (cave_ff1_match(y, x, ff1) == ff1) ++count;
        }
    }

    /* Calculate the monster ratio */
    count = ((25 * count) / 1000);

    /* Paranoia */
    return (MAX(MIN_M_ALLOC_LEVEL, count));
}


/*
 * Objects in rooms
 */
static int get_object_count_labyrinth(void)
{
    return (Rand_normal(40, 10));
}


/*
 * Dungeon capabilities for labyrinth levels
 */
static dungeon_capabilities_type dun_cap_body_labyrinth =
{
    can_place_escorts_true,
    can_place_player_in_rooms_true,
    can_place_stairs_default,
    adjust_stairs_number_unchanged,
    can_place_fog_in_rooms_false,
    can_target_feature_default,
    can_be_transformed_false,
    can_place_non_native_monsters_true,
    allow_level_repopulation_false,
    limited_level_summoning_false,
    allow_monster_multiply_quarter,
    prevent_destruction_true,
    get_monster_count_labyrinth,
    get_object_count_labyrinth,
    get_gold_count_wild,
    get_extra_object_count_zero,
};


/*
 * Dungeon capabilities for arena levels
 */
static dungeon_capabilities_type dun_cap_body_arena =
{
    can_place_escorts_true,
    can_place_player_in_rooms_false,
    can_place_stairs_default,
    adjust_stairs_number_unchanged,
    can_place_fog_in_rooms_false,
    can_target_feature_default,
    can_be_transformed_false,
    can_place_non_native_monsters_true,
    allow_level_repopulation_false,
    limited_level_summoning_true,
    allow_monster_multiply_false,
    prevent_destruction_true,
    get_monster_count_labyrinth,
    get_object_count_zero,
    get_gold_count_zero,
    get_extra_object_count_zero,
};


/*
 * Dungeon capabilities for wilderness levels
 */
static dungeon_capabilities_type dun_cap_body_themed_level =
{
    can_place_escorts_true,
    can_place_player_in_rooms_false,
    can_place_stairs_default,
    adjust_stairs_number_default,
    can_place_fog_in_rooms_true,
    can_target_feature_default,
    can_be_transformed_true,
    can_place_non_native_monsters_true,
    allow_level_repopulation_false,
    limited_level_summoning_true,
    allow_monster_multiply_false,
    prevent_destruction_false,
    get_monster_count_labyrinth,
    get_object_count_zero,
    get_gold_count_zero,
    get_extra_object_count_zero,
};


/*
 * Dungeon capabilities for wilderness levels
 */
static dungeon_capabilities_type dun_cap_body_greater_vault =
{
    can_place_escorts_true,
    can_place_player_in_rooms_false,
    can_place_stairs_default,
    adjust_stairs_number_default,
    can_place_fog_in_rooms_false,
    can_target_feature_default,
    can_be_transformed_false,
    can_place_non_native_monsters_true,
    allow_level_repopulation_false,
    limited_level_summoning_false,
    allow_monster_multiply_quarter,
    prevent_destruction_true,
    get_monster_count_labyrinth,
    get_object_count_zero,
    get_gold_count_zero,
    get_extra_object_count_zero,
};


/*
 * Get the proper dungeon capabilities based on the given dungeon type
 */
void set_dungeon_type(u16b dungeon_type)
{
    /* Remember the type */
    p_ptr->dungeon_type = dungeon_type;

    /* Get the capabilities */
    switch (dungeon_type)
    {
        /* Special rules for wilderness levels */
        case DUNGEON_TYPE_WILDERNESS:
        {
            dun_cap = &dun_cap_body_wild;
            break;
        }
        /* Special rules for wilderness levels */
        case DUNGEON_TYPE_LABYRINTH:
        {
            dun_cap = &dun_cap_body_labyrinth;
            break;
        }
        case DUNGEON_TYPE_THEMED_LEVEL:
        {
            dun_cap = &dun_cap_body_themed_level;
            break;
        }
        case DUNGEON_TYPE_ARENA:
        {
            dun_cap = &dun_cap_body_arena;
            break;
        }
        case DUNGEON_TYPE_GREATER_VAULT:
        {
            dun_cap = &dun_cap_body_greater_vault;
            break;
        }
        /* Classic dungeons */
        default:
        {
            if (game_mode == GAME_NPPMORIA) dun_cap = &dun_cap_body_moria;

            else dun_cap = &dun_cap_body_default;
        }
    }
}
