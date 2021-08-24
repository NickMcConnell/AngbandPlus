/*
 * File: z-spells.h
 * Purpose: Spell implementations and helpers
 */

#ifndef SPELLS_H
#define SPELLS_H

/*
 * Spell types used by project(), and related functions.
 */
enum
{
    #define ELEM(a, b, c, d) PROJ_##a,
    #include "list-elements.h"
    #undef ELEM
    #define PROJ(a) PROJ_##a,
    #include "list-projections.h"
    #undef PROJ
    PROJ_MAX
};

/*
 * Bolt motion (used in prefs.c, project.c)
 */
enum
{
    BOLT_NO_MOTION,
    BOLT_0,
    BOLT_45,
    BOLT_90,
    BOLT_135,
    BOLT_180,
    BOLT_225,
    BOLT_270,
    BOLT_315,
    BOLT_MAX
};

#endif
