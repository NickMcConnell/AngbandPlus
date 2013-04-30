/*
 * File: spells.h
 * Purpose: Spell implementations and helpers
 */

#ifndef SPELLS_H
#define SPELLS_H

/*
 * Spell types used by project(), and related functions.
 */
enum
{
    #define GF(a, b, c, d, e, f, g, h, i, j, k, l, m) GF_##a,
    #include "list-gf-types.h"
    #undef GF
    GF_MAX
};

/*
 * Structure for GF types and their resistances/immunities/vulnerabilities
 */
struct gf_type
{
    u16b name;          /* Numerical index (GF_#) */
    const char *desc;   /* Text description (if blind) */
    int resist;         /* Object flag for resistance */
    int num;            /* Numerator for resistance */
    random_value denom; /* Denominator for resistance */
    int opp;            /* Timed flag for temporary resistance ("opposition") */
    int immunity;       /* Object flag for total immunity */
    bool side_immune;   /* Whether immunity protects from ALL side effects */
    int vuln;           /* Object flag for vulnerability */
    int mon_res;        /* Monster flag for resistance */
    int mon_vuln;       /* Monster flag for vulnerability */
    int obj_hates;      /* Object flag for object vulnerability */
    int obj_imm;        /* Object flag for object immunity */
};

/*
 * Bolt motion (used in prefs.c, spells1.c)
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
