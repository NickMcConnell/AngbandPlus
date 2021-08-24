/*
 * File: z-spells.c
 * Purpose: Spell implementations and helpers
 *
 * Copyright (c) 2011-2016 Angband, MAngband and PWMAngband Developers
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


#include "angband.h"


static const char *gf_name_list[] =
{
    #define ELEM(a, b, c, d, e, f, g, h, col, pvp) #a,
    #include "list-elements.h"
    #undef ELEM
    #define PROJ_ENV(a, b, obv, col, desc, pvp) #a,
    #include "list-project-environs.h"
    #undef PROJ_ENV
    #define PROJ_MON(a, b, obv, col, desc, pvp) #a,
    #include "list-project-monsters.h"
    #undef PROJ_MON
    NULL
};


int gf_name_to_idx(const char *name)
{
    int i;

    for (i = 0; gf_name_list[i]; i++)
    {
        if (!my_stricmp(name, gf_name_list[i])) return i;
    }

    return -1;
}


const char *gf_idx_to_name(int type)
{
    my_assert(type >= 0);
    my_assert(type < GF_MAX);

    return gf_name_list[type];
}
