/*
 * File: p-util.c
 * Purpose: Player utility functions
 *
 * Copyright (c) 2011 The Angband Developers. See COPYING.
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


#include "../s-angband.h"
#include "../../common/tvalsval.h"


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


/*
 * Apply confusion, if needed, to a direction
 *
 * Display a message and return TRUE if direction changes.
 */
bool player_confuse_dir(struct player *p, int *dp)
{
    int dir = *dp;

    /* Random direction */
    if (p->timed[TMD_CONFUSED] && ((dir == 5) || magik(75)))
        dir = ddd[randint0(8)];

    if (*dp != dir)
    {
        msg(p, "You are confused.");
        *dp = dir;
        return TRUE;
    }

    return FALSE;
}


/*
 * Is the player capable of casting a spell?
 */
bool player_can_cast(struct player *p)
{
    if (!p->clazz->spell_book) return FALSE;
    if (p->timed[TMD_BLIND] || no_light(p)) return FALSE;
    if (p->timed[TMD_CONFUSED]) return FALSE;
    return TRUE;
} 


/*
 * Same, with message.
 */
bool player_can_cast_msg(struct player *p)
{
    if (!p->clazz->spell_book)
    {
        msg(p, "You cannot pray or produce magics.");
        return FALSE;
    }

    if (p->timed[TMD_BLIND] || no_light(p))
    {
        msg(p, "You cannot see!");
        return FALSE;
    }

    if (p->timed[TMD_CONFUSED])
    {
        msg(p, "You are too confused!");
        return FALSE;
    }

    return TRUE;
}


static bool spell_okay_to_study(struct player *p, int spell_index)
{
    const magic_type *s_ptr = &p->clazz->spells.info[spell_index];

    /* Skip illegible spells */
    if (s_ptr->slevel >= 99) return FALSE;

    /* Analyze the spell */
    if (p->spell_flags[spell_index] & PY_SPELL_FORGOTTEN) return FALSE;
    if (!(p->spell_flags[spell_index] & PY_SPELL_LEARNED)) return (s_ptr->slevel <= p->lev);
    if (!(p->spell_flags[spell_index] & PY_SPELL_WORKED)) return FALSE;
    return (p->clazz->spell_book == TV_ELEM_BOOK);
}


/*
 * Does the player carry a book with a spell they can study?
 */
bool player_can_study_book(struct player *p)
{
    int i;
    struct spell *sp;

    /* Check if the player can cast spells */
    if (!player_can_cast(p)) return FALSE;

    /* Check if the player can learn new spells */
    if (!p->new_spells) return FALSE;

    /* Get the number of books in inventory */
    for (i = 0; i < INVEN_PACK; i++)
    {
        object_type *o_ptr = &p->inventory[i];

        if (o_ptr->tval != p->clazz->spell_book) continue;

        /* Extract spells */
        for (sp = o_ptr->kind->spells; sp; sp = sp->next)
        {
            /* Check if the player can study it */
            if (spell_okay_to_study(p, sp->spell_index))
            {
                /* There is a spell the player can study */
                return TRUE;
            }
        }
    }

    return FALSE;
}
