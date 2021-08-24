/*
 * File: obj-inscrip.c
 * Purpose: Object inscription code
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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


#include "s-angband.h"


/*
 * Object inscription mapping: original keyset, roguelike keyset.
 *
 * Warning: should be synchronized with command mapping (client).
 *
 * Note: the roguelike mapping doesn't take into account the CTRL modifier, so multiple actions
 * can be prevented by a single inscription. For example, '^D' will prevent both destroying and
 * disarming.
 *
 * Note: preventing 'purchase a house' is mapped to 'h', even though the command is KTRL('E'). This
 * is for ease of use.
 *
 * Note: preventing 'auto-retaliate' is mapped to 'O', even though no command is associated to this
 * action.
 *
 * Note: preventing 'purchase an item' and 'sell an item' are mapped to 'p' and 's', which
 * correspond to the in-store commands.
 */
static char map_inscriptions[INSCRIPTION_MAX][3] =
{
    "ft", /* INSCRIPTION_FIRE */
    "vv", /* INSCRIPTION_THROW */
    "dd", /* INSCRIPTION_DROP */
    "kD", /* INSCRIPTION_DESTROY */
    "GG", /* INSCRIPTION_STUDY */
    "mm", /* INSCRIPTION_CAST */
    "EE", /* INSCRIPTION_EAT */
    "qq", /* INSCRIPTION_QUAFF */
    "rr", /* INSCRIPTION_READ */
    "uZ", /* INSCRIPTION_USE */
    "az", /* INSCRIPTION_AIM */
    "za", /* INSCRIPTION_ZAP */
    "AA", /* INSCRIPTION_ACTIVATE */
    "<<", /* INSCRIPTION_UP */
    ">>", /* INSCRIPTION_DOWN */
    "cc", /* INSCRIPTION_CLOSE */
    "TT", /* INSCRIPTION_TUNNEL */
    "DD", /* INSCRIPTION_DISARM */
    "++", /* INSCRIPTION_ALTER */
    ";;", /* INSCRIPTION_WALK */
    ".,", /* INSCRIPTION_RUN */
    "hh", /* INSCRIPTION_HOUSE */
    "JS", /* INSCRIPTION_STEAL */
    "__", /* INSCRIPTION_FOUNTAIN */
    "OO", /* INSCRIPTION_RETALIATE */
    "gg", /* INSCRIPTION_PICKUP */
    "tT", /* INSCRIPTION_TAKEOFF */
    "ww", /* INSCRIPTION_WIELD */
    "FF", /* INSCRIPTION_REFILL */
    "pp", /* INSCRIPTION_PURCHASE */
    "ss", /* INSCRIPTION_SELL */
    "{{", /* INSCRIPTION_INSCRIBE */
    "}}"  /* INSCRIPTION_UNINSCRIBE */
};


bool check_prevent_inscription(struct player *p, int what)
{
    int mode = (OPT(p, rogue_like_commands)? 1: 0);

    return p->prevents[map_inscriptions[what][mode]];
}


/*
 * Allow user to "prevent" certain choices
 * "!*" prevents everything
 */
static bool inscription_prevent(quark_t quark, char what, bool is_harmless)
{
    const char *str, *s;

    /* Get inscription */
    str = quark_str(quark);
    if (!str) return false;

    /* Check for a "prevention" inscription */
    str = strchr(str, '!');
    if (!str) return false;

    /* Allow user to "prevent" certain choices */
    for (s = str + 1; *s; s++)
    {
        /* Allow user to "prevent" all choices */
        if ((*s == '*') && !is_harmless) return true;

        /* Exact match */
        if (*s == what) return true;

        /* Stop at the first non-letter character */
        if (!isalpha(*s) && !strchr("{!}", *s)) return false;
    }

    /* Allow it */
    return false;
}


bool object_prevent_inscription(struct player *p, const struct object *obj, int what,
    bool is_harmless)
{
    int mode = (OPT(p, rogue_like_commands)? 1: 0);

    return inscription_prevent(obj->note, map_inscriptions[what][mode], is_harmless);
}


bool protected_p(struct player *p, const struct object *obj, int what, bool is_harmless)
{
    return (!is_dm_p(p) && obj->owner && (p->id != obj->owner) &&
        object_prevent_inscription(p, obj, what, is_harmless));
}
