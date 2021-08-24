/*
 * File: player_util.cpp
 * Purpose: Various file-related activities, poorly organised
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "src/npp.h"

/*
 * Obtain the "flags" for the player as if he was an item
 */
void player_flags(u32b *f1, u32b *f2, u32b *f3, u32b *fn)
{
    /* Clear */
    (*f1) = (*f2) = (*f3) = (*fn) = 0L;

    /* Add racial flags */
    (*f1) |= rp_ptr->pr_flags1;
    (*f2) |= rp_ptr->pr_flags2;
    (*f3) |= rp_ptr->pr_flags3;
    (*fn) |= rp_ptr->pr_native;
    (*fn) |= cp_ptr->c_native;

    if (cp_ptr->flags & CF_BRAVERY_30)
    {
        if (p_ptr->lev >= LEV_BRAVERY) (*f2) |= (TR2_RES_FEAR);
    }

    /* Brigand's poison resistance */
    if (cp_ptr->flags & CF_BRIGAND_COMBAT)
    {
        if (p_ptr->lev >= LEV_RES_POIS) (*f2) |= (TR2_RES_POIS);
    }
}

/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
s16b modify_stat_value(int value, int amount)
{
    int i;

    /* Reward */
    if (amount > 0)
    {
        /* Apply each point */
        for (i = 0; i < amount; i++)
        {
            /* One point at a time */
            if (value < 18) value++;

            /* Ten "points" at a time */
            else value += 10;
        }
    }

    /* Penalty */
    else if (amount < 0)
    {
        /* Apply each point */
        for (i = 0; i < (0 - amount); i++)
        {
            /* Ten points at a time */
            if (value >= 18+10) value -= 10;

            /* Hack -- prevent weirdness */
            else if (value > 18) value = 18;

            /* One point at a time */
            else if (value > 3) value--;
        }
    }

    /* Return new value */
    return (value);
}


/* Players with chaos or confusion resistance don't get confused*/
bool allow_player_confusion(void)
{
    if (p_ptr->state.resist_confu) return (FALSE);
    if (p_ptr->state.resist_chaos) return (FALSE);

    /*Don't have the right resists*/
    return (TRUE);
}
