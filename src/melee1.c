/* File: melee1.c */

/* Purpose: Monster attacks */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

#include "angband.h"

#if 0
/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[] =
{
    "insults you",
    "insults your mother",
    "gives you the finger",
    "humiliates you",
    "defiles you",
    "dances around you",
    "makes obscene gestures",
    "moons you",
    "calls you a parasite",
    "calls you a cyborg"
};



/*
 * Hack -- possible "insult" messages
 */
static cptr desc_moan[] =
{
    "seems sad about something",
    "asks if you have seen his dogs",
    "tells you to get off his land",
    "mumbles something about mushrooms"

};
#endif

/* Player AC reduces the melee damage of HURT, SUPERHURT and SHATTER
 * attacks. Damage from other attack types is *not* reduced. */ 
int ac_melee_pct_aux(int ac, int max_reduce, int max_ac)
{
    int pct;
    if (ac > max_ac) ac = max_ac;
    if (ac < 0) ac = 0;
    pct = max_reduce*ac/max_ac;
    return 100 - pct;
}

int ac_melee_pct(int ac)
{
    return ac_melee_pct_aux(ac, 60, 180);
}

int reduce_melee_dam_p(int dam)
{
    int result = dam;
    switch (weaponmaster_get_toggle())
    {
    case TOGGLE_BULWARK:
        result -= (result + 2)/3;
        break;
    }

    if (result < 0) result = 0;
    return result;
}

