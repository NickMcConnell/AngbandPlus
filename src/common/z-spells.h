/*
 * File: z-spells.h
 * Purpose: Spell implementations and helpers
 */

#ifndef SPELLS_H
#define SPELLS_H

#define ATT_SAVE        0x01
#define ATT_DAMAGE      0x02
#define ATT_NON_PHYS    0x04
#define ATT_RAW         0x08

/*
 * Spell types used by project(), and related functions.
 */
enum
{
    #define ELEM(a, b, c, d, e, f, g, h, col, pvp) GF_##a,
    #include "list-elements.h"
    #undef ELEM
    #define PROJ_ENV(a, b, obv, col, desc, pvp) GF_##a,
    #include "list-project-environs.h"
    #undef PROJ_ENV
    #define PROJ_MON(a, b, obv, col, desc, pvp) GF_##a,
    #include "list-project-monsters.h"
    #undef PROJ_MON
    GF_MAX
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

extern int gf_name_to_idx(const char *name);
extern const char *gf_idx_to_name(int type);

#endif
