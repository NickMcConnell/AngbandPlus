/* File: wild.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke,
 * Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Wilderness generation
 *
 * CTK: Added infinitely scrolling wilderness support (hackish).
 */

#include "angband.h"
#include "dun.h"
#include "rooms.h"
#include <assert.h>

bool plr_on_surface(void)
{
    return cave->type->id == D_SURFACE;
}

bool plr_in_town(void)
{
    return dun_world_town_id() != 0;
}

bool plr_in_dungeon(void)
{
    switch (cave->type->id)
    {
    case D_SURFACE:
    case D_WORLD:
    case D_QUEST: return FALSE;
    }
    return TRUE;
}

bool plr_can_recall(void)
{
    return TRUE;
}

