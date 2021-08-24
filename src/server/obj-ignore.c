/*
 * File: obj-ignore.c
 * Purpose: Item ignoring
 *
 * Copyright (c) 2007 David T. Blackston, Iain McFall, DarkGod, Jeff Greene,
 * David Vestal, Pete Mack, Andi Sidwell.
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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
 * Ignore level markers.
 */
enum
{
    INSCRIP_BAD,        /* Item with bad combat bonuses */
    INSCRIP_AVERAGE,    /* Item with no interesting features */
    INSCRIP_GOOD,       /* Item with good combat bonuses */
    INSCRIP_EXCELLENT,  /* Ego-item */
    INSCRIP_SPECIAL     /* Artifact */
};


/*
 * Small helper function to see how an object trait compares to the one
 * in its base type.
 *
 * If the base type provides a positive bonus, we'll use that. Otherwise, we'll
 * use zero (players don't consider an item with a positive bonus to be bad
 * even if the base kind has a higher positive bonus).
 */
static int cmp_object_trait(int bonus, random_value base)
{
    int amt = randcalc(base, 0, MINIMISE);

    if (amt > 0) amt = 0;
    return CMP(bonus, amt);
}


/*
 * Small helper function to see if an item seems good, bad or average based on
 * to_h, to_d and to_a.
 *
 * The sign of the return value announces if the object is bad (negative),
 * good (positive) or average (zero).
 */
static int is_object_good(const struct object *obj)
{
    int good = 0;
    s16b to_h, to_d, to_a;

    object_to_h(obj, &to_h);
    object_to_d(obj, &to_d);
    object_to_a(obj, &to_a);

    good += 4 * cmp_object_trait(to_d, obj->kind->to_d);
    good += 2 * cmp_object_trait(to_h, obj->kind->to_h);
    good += 1 * cmp_object_trait(to_a, obj->kind->to_a);

    return good;
}


/*
 * Given an object, return a short identifier which gives some idea of what the item is.
 */
static int object_marker(const struct object *obj)
{
    int isgood = is_object_good(obj);

    if (obj->artifact) return INSCRIP_SPECIAL;

    /* Deal with jewelry specially. */
    if (tval_is_jewelry(obj))
    {
        if (!kind_is_good_other(obj->kind)) return INSCRIP_AVERAGE;
        return INSCRIP_GOOD;
    }

    /* Now just do bad, average, good, ego */
    if (obj->ego) return INSCRIP_EXCELLENT;
    if (isgood > 0) return INSCRIP_GOOD;
    if (isgood < 0) return INSCRIP_BAD;
    return INSCRIP_AVERAGE;
}


static const char *inscrip_text[] =
{
    "bad",
    "average",
    "good",
    "excellent",
    "special"
};


/*
 * Sense an object
 */
static void sense_object(struct player *p, struct object *obj)
{
    char o_name[NORMAL_WID];
    const char *text = NULL;
    int marker;
    bool cursed, worthless;

    /* Valid tval codes only */
    if (!tval_has_variable_power(obj)) return;

    /* It is known, no information needed */
    if (object_is_known(p, obj)) return;

    /* It has already been sensed, do not sense it again */
    if (object_was_sensed(obj)) return;

    /* Sense the object */
    object_notice_sensing(p, obj);
    cursed = (obj->curses && obj->known->curses);

    /* Hack -- worthless objects */
    worthless = worthless_p(obj);

    /* Get marker */
    marker = object_marker(obj);

    /* Stop everything */
    disturb(p, 0);

    if (cursed) text = "cursed";
    else if (worthless) text = "worthless";
    else text = inscrip_text[marker];

    /* Get an object description */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Message (inventory) */
    if (object_is_carried(p, obj))
    {
        msg(p, "You feel the %s (%c) in your pack %s %s...", o_name, gear_to_label(p, obj),
            VERB_AGREEMENT(obj->number, "is", "are"), text);
    }

    /* Message (floor) */
    else
    {
        msg(p, "You feel the %s on the ground %s %s...", o_name, VERB_AGREEMENT(obj->number, "is", "are"),
            text);
    }

    /* Set ignore flag as appropriate */
    p->upkeep->notice |= PN_IGNORE;

    /* Combine the pack (later) */
    p->upkeep->notice |= (PN_COMBINE);

    /* Update the gear */
    p->upkeep->update |= (PU_INVEN);

    /* Redraw */
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
}


/*
 * Assess an object
 */
void assess_object(struct player *p, struct object *obj)
{
    /* Transfer player object knowledge */
    player_know_object(p, obj);

    /* Mark artifact as found */
    set_artifact_info(p, obj, ARTS_FOUND);

    /* Automatically sense artifacts */
    obj->known->artifact = (struct artifact *)1;

    /* If the object has no ego, we know that */
    if (!obj->ego) obj->known->ego = (struct ego_item *)1;

    /* If the object has no pval or effect, we know that */
    if (!obj->pval) obj->known->pval = 1;
    if (!object_effect(obj)) obj->known->effect = (struct effect *)1;

    /* Automatically sense unflavored objects and artifacts */
    if (!obj->kind->flavor || obj->artifact)
    {
        obj->known->pval = 1;
        obj->known->effect = (struct effect *)1;
    }

    /* Auto-id */
    if (player_of_has(p, OF_KNOWLEDGE) && !object_is_known(p, obj))
        object_know_everything(p, obj);

    /* Sense the object */
    sense_object(p, obj);
}


/*
 * Determine the ignore level of an object.
 *
 * The main point is when the value is undetermined given current info,
 * return the maximum possible value.
 */
byte ignore_level_of(struct player *p, const struct object *obj)
{
    bool fullid = object_is_known(p, obj);

    /* Don't check marker for nonsensed things */
    if (object_was_sensed(obj) || fullid)
    {
        /* Get marker */
        int marker = object_marker(obj);

        /* Never ignore "special" items */
        if (marker == INSCRIP_SPECIAL) return IGNORE_MAX;

        /* Other worthless items */
        if ((obj->origin == ORIGIN_WORTHLESS) || (fullid && worthless_p(obj)))
            return IGNORE_BAD;

        /* Get result based on the marker */
        switch (marker)
        {
            case INSCRIP_BAD:
            {
                if (fullid) return IGNORE_BAD;
                return IGNORE_GOOD;
            }
            case INSCRIP_AVERAGE:
            {
                if (!kind_is_good_other(obj->kind)) return IGNORE_AVERAGE;
                return IGNORE_GOOD;
            }
            case INSCRIP_GOOD: return IGNORE_GOOD;
            case INSCRIP_EXCELLENT: return IGNORE_ALL;
        }

        /* Default to not ignoring */
        return IGNORE_MAX;
    }

    if (!obj->artifact && obj->known->artifact) return IGNORE_ALL;
    return IGNORE_MAX;
}


/*
 * Determines if an object is already ignored.
 */
bool object_is_ignored(struct player *p, const struct object *obj)
{
    byte type;

    /* Don't ignore artifacts */
    if (obj->artifact) return false;

    /* Don't ignore stuff inscribed not to be destroyed (!k) */
    if (object_prevent_inscription(p, obj, INSCRIPTION_DESTROY, false)) return false;

    /* Don't ignore protected items */
    if (obj->ignore_protect) return false;

    /* Do ignore individual objects that marked ignore */
    if (obj->known->notice & OBJ_NOTICE_IGNORE) return true;

    /* Do ignoring by kind */
    if (object_flavor_is_aware(p, obj) && p->kind_ignore[obj->kind->kidx]) return true;

    type = ignore_type_of(obj);

    /* Ignore ego items if known */
    if (obj->ego && obj->known->ego && p->ego_ignore_types[obj->ego->eidx][type])
        return true;

    if (type == ITYPE_MAX) return false;

    /* Ignore items known not to be artifacts */
    if (!obj->artifact && obj->known->artifact && (p->opts.ignore_lvl[type] == IGNORE_ALL))
        return true;

    /* Get result based on the feeling and the ignore_level */
    return (ignore_level_of(p, obj) <= p->opts.ignore_lvl[type]);
}


/*
 * Determines if an object is eligible for ignoring.
 */
bool ignore_item_ok(struct player *p, const struct object *obj)
{
    /* Player doesn't hide ignored items */
    if (p->unignoring) return false;

    return object_is_ignored(p, obj);
}


/*
 * Drop all ignored items.
 */
void ignore_drop(struct player *p)
{
    struct object *obj;

    /* Scan through the slots backwards */
    for (obj = gear_last_item(p); obj; obj = obj->prev)
    {
        /* Skip non-objects and unignoreable objects */
        if (!ignore_item_ok(p, obj)) continue;

        /* Check for !d (no drop) inscription */
        if (!object_prevent_inscription(p, obj, INSCRIPTION_DROP, false))
        {
            /* We're allowed to drop it. */
            do_cmd_drop(p, obj->oidx, obj->number);
            cmd_ignore_drop(p);
            break;
        }
    }
}


/*
 * Initialize the ignore package
 */
void init_ignore(void)
{
}


/*
 * Clean up the ignore package
 */
void cleanup_ignore(void)
{
}


struct init_module ignore_module =
{
    "ignore",
    init_ignore,
    cleanup_ignore
};


/*
 * Return an object kind autoinscription
 */
const char *get_autoinscription(struct player *p, struct object_kind *kind)
{
    if (!kind) return NULL;
    return quark_str(p->note_aware[kind->kidx]);
}


/*
 * Put an autoinscription on an object
 */
int apply_autoinscription(struct player *p, struct object *obj)
{
    char o_name[NORMAL_WID];
    const char *note = get_autoinscription(p, obj->kind);

    /* No note - don't inscribe */
    if (!note) return 0;

    /* Don't re-inscribe if it's already inscribed */
    if (obj->note) return 0;

    /* Don't inscribe unless the player is carrying it */
    if (!object_is_carried(p, obj)) return 0;

    /* Don't inscribe if ignored */
    if (ignore_item_ok(p, obj)) return 0;

    /* PWMAngband: don't inscribe if not aware */
    if (!p->obj_aware[obj->kind->kidx]) return 0;

    /* Get an object description */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

    if (note[0] != 0) obj->note = quark_add(note);
    else obj->note = 0;

    msg(p, "You autoinscribe %s.", o_name);

    return 1;
}


/*
 * Deregister an object kind autoinscription
 */
int remove_autoinscription(struct player *p, s16b kind)
{
    struct object_kind *k = &k_info[kind];

    if (!k) return 0;
    if (!p->note_aware[kind]) return 0;

    p->note_aware[kind] = 0;
    return 1;
}


/*
 * Register an object kind autoinscription
 */
int add_autoinscription(struct player *p, s16b kind, const char *inscription)
{
    struct object_kind *k = &k_info[kind];

    if (!k) return 0;
    if (!inscription || STRZERO(inscription)) return remove_autoinscription(p, kind);
    p->note_aware[kind] = quark_add(inscription);
    return 1;
}
