
/* File: spells1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Spell projection */

#include "angband.h"
#include <assert.h>

int project_length = 0;

/*
 * Ticked off monsters must have a powerful distance attack
 */
bool allow_ticked_off(monster_race *r_ptr)
{
    if (!r_ptr->spells) return FALSE;
    if ( r_ptr->spells->groups[MST_BREATH]
      || r_ptr->spells->groups[MST_BALL]
      || r_ptr->spells->groups[MST_BOLT]
      || r_ptr->spells->groups[MST_BEAM]
      || r_ptr->spells->groups[MST_SUMMON] )
    {
        return TRUE;
    }
    return FALSE;
}

/*
 * Get a legal "multi-hued" color for drawing "spells"
 */
static byte mh_attr(int max)
{
    switch (randint1(max))
    {
        case  1: return (TERM_RED);
        case  2: return (TERM_GREEN);
        case  3: return (TERM_BLUE);
        case  4: return (TERM_YELLOW);
        case  5: return (TERM_ORANGE);
        case  6: return (TERM_VIOLET);
        case  7: return (TERM_L_RED);
        case  8: return (TERM_L_GREEN);
        case  9: return (TERM_L_BLUE);
        case 10: return (TERM_UMBER);
        case 11: return (TERM_L_UMBER);
        case 12: return (TERM_SLATE);
        case 13: return (TERM_WHITE);
        case 14: return (TERM_L_WHITE);
        case 15: return (TERM_L_DARK);
    }

    return (TERM_WHITE);
}


/*
 * Return a color to use for the bolt/ball spells
 */
static byte spell_color(int type)
{
    /* Check if A.B.'s new graphics should be used (rr9) */
    if (streq(ANGBAND_GRAF, "new"))
    {
        /* Analyze */
        switch (type)
        {
            case GF_PSY_SPEAR:      return (0x06);
            case GF_MISSILE:        return (0x0F);
            case GF_ACID:           return (0x04);
            case GF_ELEC:           return (0x02);
            case GF_BLOOD:          return (0x00);
            case GF_FIRE:           return (0x00);
            case GF_COLD:           return (0x01);
            case GF_POIS:           return (0x03);
            case GF_HOLY_FIRE:      return (0x00);
            case GF_HELL_FIRE:      return (0x00);
            case GF_MANA:           return (0x0E);
              /* by henkma */
            case GF_SEEKER:         return (0x0E);
            case GF_SUPER_RAY:      return (0x0E);

            case GF_ARROW:          return (0x0F);
            case GF_STORM:
            case GF_WATER:
            case GF_WATER2: return (0x04);
            case GF_WEB:          return (0x04);
            case GF_NETHER:         return (0x07);
            case GF_CHAOS:          return (mh_attr(15));
            case GF_DISENCHANT:     return (0x05);
            case GF_NEXUS:          return (0x0C);
            case GF_CONFUSION:      return (mh_attr(4));
            case GF_SOUND:          return (0x09);
            case GF_SHARDS:         return (0x08);
            case GF_ROCK:         return (0x08);
            case GF_FORCE:          return (0x09);
            case GF_INERTIA:        return (0x09);
            case GF_GRAVITY:        return (0x09);
            case GF_AMNESIA:          return (0x09);
            case GF_TIME:           return (0x09);
            case GF_LIGHT_WEAK:      return (0x06);
            case GF_LIGHT:           return (0x06);
            case GF_DARK_WEAK:      return (0x07);
            case GF_DARK:           return (0x07);
            case GF_PLASMA:         return (0x0B);
            case GF_METEOR:         return (0x00);
            case GF_ICE:            return (0x01);
            case GF_ROCKET:         return (0x0F);
            case GF_DEATH_RAY:      return (0x07);
            case GF_NUKE:           return (mh_attr(2));
            case GF_DISINTEGRATE:   return (0x05);
            case GF_PSI:
            case GF_PSI_DRAIN:
            case GF_TELEKINESIS:
            case GF_DOMINATION:
            case GF_DRAIN_MANA:
            case GF_MIND_BLAST:
            case GF_BRAIN_SMASH:
                        return (0x09);
            case GF_CAUSE_1:
            case GF_CAUSE_2:
            case GF_CAUSE_3:
            case GF_CAUSE_4:        return (0x0E);
            case GF_HAND_DOOM:      return (0x07);
            case GF_CAPTURE  :      return (0x0E);
            case GF_IDENTIFY:       return (0x01);
            case GF_ATTACK:        return (0x0F);
        }
    }
    /* Normal tiles or ASCII */
    else
    {
        byte a;
        char c;

        /* Lookup the default colors for this type */
        cptr s = quark_str(gf_color[type]);

        /* Oops */
        if (!s) return (TERM_WHITE);

        /* Pick a random color */
        c = s[randint0(strlen(s))];

        /* Lookup this color */
        a = my_strchr(color_char, c) - color_char;

        /* Invalid color (note check for < 0 removed, gave a silly
         * warning because bytes are always >= 0 -- RG) */
        if (a > 15) return (TERM_WHITE);

        /* Use this color */
        return (a);
    }

    /* Standard "color" */
    return (TERM_WHITE);
}


/*
 * Find the attr/char pair to use for a spell effect
 *
 * It is moving (or has moved) from (x,y) to (nx,ny).
 *
 * If the distance is not "one", we (may) return "*".
 */
u16b bolt_pict(int y, int x, int ny, int nx, int typ)
{
    term_char_t tc = bolt_char(point_create(x, y), point_create(nx, ny), typ);
    return PICT(tc.a, tc.c);
}
term_char_t blast_char(int gf) /* '*' */
{
    point_t o = {0};
    return bolt_char(o, o, gf);
}
term_char_t bolt_char(point_t p, point_t np, int gf) /* -|/\* */
{
    point_t v = point_subtract(np, p); /* vector from p to np */
    int base;

    byte k;

    byte a;
    char c;

    /* No motion (*) */
    if (v.x == 0 && v.y == 0) base = 0x30;

    /* Vertical (|) */
    else if (v.x == 0) base = 0x40;

    /* Horizontal (-) */
    else if (v.y == 0) base = 0x50;

    /* Diagonal (/) (note y increases down the screen) */
    else if (v.x == -v.y) base = 0x60;

    /* Diagonal (\) */
    else if (v.x == v.y) base = 0x70;

    /* Weird (*) */
    else base = 0x30;

    /* Basic spell color */
    k = spell_color(gf);

    /* Obtain attr/char */
    a = misc_to_attr[base + k];
    c = misc_to_char[base + k];

    /* Create pict */
    return term_char_create(c, a);
}

int project_path(point_ptr points, int range, point_t p1, point_t p2, int flags)
{
    dun_path_ptr path = dun_path_alloc_aux(cave, p1, p2, flags, range);
    int          count = path->count;
    int          i;

    for (i = 0; i < path->count; i++)
        points[i] = path->points[i];

    dun_path_free(path);
    return count;
}

/* Mega-Hack -- monsters target */
point_t monster_target;



