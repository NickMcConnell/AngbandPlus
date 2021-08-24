/*
 * File: obj-curse.c
 * Purpose: Functions to deal with object curses
 *
 * Copyright (c) 2016 Nick McConnell
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
 * Return the index of the curse with the given name
 */
int lookup_curse(const char *name)
{
    int i;

    for (i = 0; i < z_info->curse_max; i++)
    {
        struct curse *curse = &curses[i];

        if (curse->name && streq(name, curse->name)) return i;
    }

    return -1;
}


static void randomize_curse(struct curse_data *curse, struct curse *c)
{
    int i;

    if (c->obj->to_h)
        curse->to_h = SGN(c->obj->to_h) * randint1(ABS(c->obj->to_h));
    if (c->obj->to_d)
        curse->to_d = SGN(c->obj->to_d) * randint1(ABS(c->obj->to_d));
    if (c->obj->to_a)
        curse->to_a = SGN(c->obj->to_a) * randint1(ABS(c->obj->to_a));
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (c->obj->modifiers[i])
            curse->modifiers[i] = SGN(c->obj->modifiers[i]) * randint1(ABS(c->obj->modifiers[i]));
    }
}


/*
 * Copy all the curses from a template to an actual object.
 *
 * obj the object the curses are being attached to
 * source the curses being copied
 */
void copy_curses(struct object *obj, int *source)
{
    int i;

    if (!source) return;

    if (!obj->curses)
        obj->curses = mem_zalloc(z_info->curse_max * sizeof(struct curse_data));

    for (i = 0; i < z_info->curse_max; i++)
    {
        if (!source[i]) continue;
        obj->curses[i].power = source[i];

        /* Timeouts need to be set for new objects */
        obj->curses[i].timeout = randcalc(curses[i].obj->time, 0, RANDOMISE);

        /*
         * Because struct object doesn't allow random values, generic curse
         * objects represent these by fixed values to be randomised on
         * application to an actual object
         */
        randomize_curse(&obj->curses[i], &curses[i]);
    }
}


/*
 * Check whether two objects have the exact same curses
 *
 * obj1 the first object
 * obj2 the second object
 */
bool curses_are_equal(const struct object *obj1, const struct object *obj2)
{
    int i;

    if (!obj1->curses && !obj2->curses) return true;
    if (obj1->curses && !obj2->curses) return false;
    if (!obj1->curses && obj2->curses) return false;

    for (i = 0; i < z_info->curse_max; i++)
    {
        if (obj1->curses[i].power != obj2->curses[i].power) return false;
    }

    return true;
}


/*
 * Detect if a curse is in the conflict list of another curse
 */
static bool curses_conflict(int first, int second)
{
    struct curse *c = &curses[first];
    char buf[NORMAL_WID];

    /* First curse has no conflicts */
    if (!c->conflict) return false;

    /* Build the conflict string and search for it */
    strnfmt(buf, sizeof(buf), "|%s|", curses[second].name);
    if (strstr(c->conflict, buf)) return true;

    return false;
}


/*
 * Check an object for active curses, and remove the "curses" field if none is found
 */
static void check_object_curses(struct object *obj)
{
    int i;

    /* Look for a valid curse, return if one found */
    for (i = 0; i < z_info->curse_max; i++)
    {
        if (obj->curses[i].power) return;
    }

    /* Free the curse structure */
    mem_free(obj->curses);
    obj->curses = NULL;
}


static bool object_curse_conflicts(struct object *obj, int pick)
{
    struct curse *c = &curses[pick];

    /* Reject curses with effects foiled by an existing object property */
    if (c->obj->effect && (c->obj->effect->index == effect_lookup("TIMED_INC")))
    {
        int idx = c->obj->effect->params[0];
        struct timed_effect_data *status;

        my_assert(idx >= 0);
        my_assert(idx < TMD_MAX);

        status = &timed_effects[idx];
        if ((status->fail_code == TMD_FAIL_FLAG_OBJECT) && of_has(obj->flags, status->fail))
            return true;
        if ((status->fail_code == TMD_FAIL_FLAG_RESIST) && (obj->el_info[status->fail].res_level > 0))
            return true;
        if ((status->fail_code == TMD_FAIL_FLAG_VULN) && (obj->el_info[status->fail].res_level < 0))
            return true;
    }

    return false;
}


/*
 * Append a given curse with a given power to an object
 *
 * obj the object to curse
 * pick the curse to append
 * power the power of the new curse
 */
static bool append_object_curse_aux(struct object *obj, int pick, int power)
{
    struct curse *c = &curses[pick];
    int i;

    if (!obj->curses)
        obj->curses = mem_zalloc(z_info->curse_max * sizeof(struct curse_data));

    /* Reject conflicting curses */
    for (i = 0; i < z_info->curse_max; i++)
    {
        if (obj->curses[i].power && curses_conflict(i, pick))
        {
            check_object_curses(obj);
            return false;
        }
    }

    /* Reject curses with effects foiled by an existing object property */
    if (object_curse_conflicts(obj, pick))
    {
        check_object_curses(obj);
        return false;
    }

    /* Reject curses which explicitly conflict with an object property */
    if (of_is_inter(obj->flags, c->conflict_flags))
    {
        check_object_curses(obj);
        return false;
    }

    /* Check for existence */
    if (obj->curses[pick].power)
    {
        /* Same power or smaller, fail */
        if (power <= obj->curses[pick].power)
        {
            check_object_curses(obj);
            return false;
        }

        /* Greater power, increase and accept */
        obj->curses[pick].power = power;
        return true;
    }

    /* Add curse */
    obj->curses[pick].power = power;
    obj->curses[pick].timeout = randcalc(c->obj->time, 0, RANDOMISE);
    randomize_curse(&obj->curses[pick], c);
    return true;
}


int append_object_curse(struct object *obj, int lev, int tval)
{
    int max_curses = randint1(4);
    int power = randint1(9) + 10 * m_bonus(9, lev);
    int new_lev = lev;

    /* Blessed objects are immune */
    if (of_has(obj->flags, OF_BLESSED)) return lev;

    while (max_curses--)
    {
        int tries = 3;

        /* Try to curse it */
        while (tries--)
        {
            int pick = randint0(z_info->curse_max);

            if (curses[pick].poss && curses[pick].poss[tval])
            {
                if (append_object_curse_aux(obj, pick, power))
                    new_lev += randint1(1 + power / 10);
                break;
            }
        }
    }

    return new_lev;
}


/*
 * Check an artifact template for active curses, remove conflicting curses, and
 * remove the "curses" field if no curses remain
 */
static void check_artifact_curses(struct artifact *art)
{
    int i;

    /* Look for a valid curse, return if one found */
    for (i = 0; i < z_info->curse_max; i++)
    {
        if (art->curses[i]) return;
    }

    /* Free the curse structure */
    mem_free(art->curses);
    art->curses = NULL;
}


static bool artifact_curse_conflicts(struct artifact *art, int pick)
{
    struct curse *c = &curses[pick];

    /* Reject curses with effects foiled by an existing object property */
    if (c->obj->effect && c->obj->effect->index == effect_lookup("TIMED_INC"))
    {
        int idx = c->obj->effect->params[0];
        struct timed_effect_data *status;

        my_assert(idx >= 0);
        my_assert(idx < TMD_MAX);

        status = &timed_effects[idx];
        if ((status->fail_code == TMD_FAIL_FLAG_OBJECT) && of_has(art->flags, status->fail))
            return true;
        if ((status->fail_code == TMD_FAIL_FLAG_RESIST) && (art->el_info[status->fail].res_level > 0))
            return true;
        if ((status->fail_code == TMD_FAIL_FLAG_VULN) && (art->el_info[status->fail].res_level < 0))
            return true;
    }

    return false;
}


/*
 * Append a given curse with a given power to an artifact
 *
 * art the artifact to curse
 * pick the curse to append
 * power the power of the new curse
 */
static bool append_artifact_curse_aux(struct artifact *art, int pick, int power)
{
    struct curse *c = &curses[pick];
    int i;

    if (!art->curses)
        art->curses = mem_zalloc(z_info->curse_max * sizeof(int));

    /* Reject conflicting curses */
    for (i = 0; i < z_info->curse_max; i++)
    {
        if (art->curses[i] && curses_conflict(i, pick))
        {
            check_artifact_curses(art);
            return false;
        }
    }

    /* Reject curses with effects foiled by an existing artifact property */
    if (artifact_curse_conflicts(art, pick))
    {
        check_artifact_curses(art);
        return false;
    }

    /* Reject curses which explicitly conflict with an artifact property */
    if (of_is_inter(art->flags, c->conflict_flags))
    {
        check_artifact_curses(art);
        return false;
    }

    /* Check for existence */
    if (art->curses[pick])
    {
        /* Same power or smaller, fail */
        if (power <= art->curses[pick])
        {
            check_artifact_curses(art);
            return false;
        }

        /* Greater power, increase and accept */
        art->curses[pick] = power;
        return true;
    }

    /* Add curse */
    art->curses[pick] = power;
    return true;
}


int append_artifact_curse(struct artifact *art, int lev, int tval)
{
    int max_curses = randint1(4);
    int power = randint1(9) + 10 * m_bonus(9, lev);
    int new_lev = lev;

    /* Blessed artifacts are immune */
    if (of_has(art->flags, OF_BLESSED)) return lev;

    while (max_curses--)
    {
        int tries = 3;

        /* Try to curse it */
        while (tries--)
        {
            int pick = randint0(z_info->curse_max);

            if (curses[pick].poss && curses[pick].poss[tval])
            {
                if (append_artifact_curse_aux(art, pick, power))
                    new_lev += randint1(1 + power / 10);
                break;
            }
        }
    }

    return new_lev;
}


/*
 * Do a curse effect.
 *
 * i the index into the curses array
 */
bool do_curse_effect(struct player *p, int i)
{
    struct curse *curse = &curses[i];
    struct effect *effect = curse->obj->effect;
    bool ident = false;
    bool was_aware = player_knows_curse(p, i);
    int dir = randint1(8);
    struct source who_body;
    struct source *who = &who_body;

    if (dir > 4) dir++;
    if (effect->self_msg) msgt(p, MSG_GENERIC, effect->self_msg);
    source_player(who, get_player_index(get_connection(p->conn)), p);
    effect_do(effect, who, &ident, was_aware, dir, NULL, 0, 0, NULL);
    return !was_aware && ident;
}


bool append_curse(struct object *obj, struct object *source, int i)
{
    if (!obj->curses)
        obj->curses = mem_zalloc(z_info->curse_max * sizeof(struct curse_data));

    /* Check for existence */
    if (obj->curses[i].power)
    {
        /* Same power or smaller, fail */
        if (source->curses[i].power <= obj->curses[i].power)
        {
            check_object_curses(obj);
            return false;
        }

        /* Greater power, increase and accept */
        obj->curses[i].power = source->curses[i].power;
        return true;
    }

    /* Add curse */
    memcpy(&obj->curses[i], &source->curses[i], sizeof(struct curse_data));
    return true;
}
