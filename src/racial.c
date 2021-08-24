/* File: racial.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Racial powers (and mutations) */

#include "angband.h"


bool can_do_cmd_cast(void)
{
    if (cave->type->id != D_SURFACE && (cave->flags & DF_NO_MAGIC))
    {
        msg_print("The dungeon absorbs all attempted magic!");
        msg_print(NULL);
        return FALSE;
    }
    else if (plr->anti_magic)
    {
        msg_print("An anti-magic shell disrupts your magic!");
        equip_learn_flag(OF_NO_MAGIC);
        return FALSE;
    }
    else if (plr_tim_find(T_BERSERK))
    {
        msg_format("You cannot think directly!");
        return FALSE;
    }
    else
        return TRUE;
}

void stop_mouth(void)
{
    if (music_current()) music_stop();
    if (hex_count()) hex_stop();
    if (bless_count()) bless_stop();
    warlock_stop_singing();
}


